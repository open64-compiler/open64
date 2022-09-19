/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


//-*-c++-*-
/* ====================================================================
 * ====================================================================
 *
 * Module: ir_reader.cxx
 *
 * Revision history:
 *  1-SEP-94 - Original Version
 *
 * Description:
 *
 * ====================================================================
 * ====================================================================
 */

/*************************************************************************
   IR Ascii/Binary Reader/Write

   Interface routines:
      IR_init
      IR_open
      IR_close
      IR_finish
      IR_get_func
      IR_output

 *************************************************************************/
#ifdef USE_PCH
#include "common_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#ifndef __MINGW32__
#include <sys/mman.h>
#endif /* __MINGW32__ */
#include <sys/stat.h>
#include <fcntl.h>
#include <stdarg.h>
#include <ctype.h>
#include <unistd.h>
#include <cmplrs/rcodes.h>

#ifndef USE_STANDARD_TYPES
#define USE_STANDARD_TYPES // to allow wn_tree_op.h to include ... vector.h
                           // which uses standard types.
#endif

#include "wn_tree_util.h"
#include "defs.h"
#include "errors.h"
#include "srcpos.h"
#include "opcode.h"
#include "stab.h"
#include "const.h"
#include "targ_const.h"
extern char * Targ_Print (const char *fmt, TCON c );
#include "targ_sim.h"
#include "strtab.h"
#include "irbdata.h"
#include "wn.h"		/* Whirl Node descriptor */
#include "wn_simp.h"    /* Whirl simplifier (can be stubs) */
#include "dwarf_DST.h"
#include "dwarf_DST_mem.h"
#include "ir_reader.h"
#include "tracing.h"
#include "config_opt.h"

#ifdef BACK_END
#include "opt_alias_mgr.h"
#endif /* BACK_END */

#include "wio.h"
#include "wintrinsic.h"
#include "wn_pragmas.h"
#include "wutil.h"
#if defined(TARG_NVISA)
#include "intrn_info.h"
#endif
#ifdef BACK_END
#include "intrn_info.h"
#include "region_util.h"
#include "dvector.h"
#endif /* BACK_END */

#if defined(BACK_END) || defined(IR_TOOLS)
/*for whirl ssa*/
#include "wssa_mgr.h"
#include "wssa_wn.h"
#include "pu_info.h"
#endif

#include <sstream> 
using namespace std; 

/* Extended Ascii-WHIRL */

enum OPR_EXTENDED {
    OPR_END_BLOCK	= OPERATOR_LAST+1,
    OPR_BODY		= OPERATOR_LAST+2,
    OPR_NOOP		= OPERATOR_LAST+3,
    OPR_THEN		= OPERATOR_LAST+4,
    OPR_ELSE		= OPERATOR_LAST+5,
    OPR_END_IF		= OPERATOR_LAST+6,
    OPR_INIT		= OPERATOR_LAST+7,
    OPR_COMP		= OPERATOR_LAST+8,
    OPR_INCR		= OPERATOR_LAST+9,
    OPR_END_COMPGOTO	= OPERATOR_LAST+10,
    OPR_END_XGOTO 	= OPERATOR_LAST+11,
    OPR_LOC		= OPERATOR_LAST+12,
    OPR_END_LINFO	= OPERATOR_LAST+13,
    OPR_END_SWITCH	= OPERATOR_LAST+14,
};

enum OPC_EXTENDED {
    OPC_END_BLOCK	= OPCODE_LAST+1,
    OPC_BODY		= OPCODE_LAST+2,
    OPC_NOOP		= OPCODE_LAST+3,
    OPC_THEN		= OPCODE_LAST+4,
    OPC_ELSE		= OPCODE_LAST+5,
    OPC_END_IF		= OPCODE_LAST+6,
    OPC_INIT		= OPCODE_LAST+7,
    OPC_COMP		= OPCODE_LAST+8,
    OPC_INCR		= OPCODE_LAST+9,
    OPC_END_COMPGOTO	= OPCODE_LAST+10,
    OPC_END_XGOTO 	= OPCODE_LAST+11,
    OPC_LOC		= OPCODE_LAST+12,
    OPC_END_LINFO	= OPCODE_LAST+13,
    OPC_END_SWITCH	= OPCODE_LAST+14,
    MAX_OPCODE		= OPCODE_LAST+15,
};

typedef struct {
  const char *pr_name;
  INT  n_kids;
  INT  flags;
} IR_OPCODE_TABLE;


IR_OPCODE_TABLE ir_opcode_table[1];         /* index by opcode */


#define HASH_LEN	2413	       /* prime number */
#define IR_MAX_ARGS     3              /* IR opcode has at most 3 arguments */

IR_OPCODE_TABLE *opcode_hash[HASH_LEN]; 

#define LINE_LEN  1024  /* maximum length of line */

typedef struct {
  OPCODE opcode;
  INT _operator;
  INT n_kids;
  INT flags;
  ST *st;
  TY_IDX ty;
  TY_IDX load_addr_ty;
  INT id;
  INT num_dim;
  INT num_entries;
  INT last_label;
  WN_OFFSET offset;
  INT16 offset1_of_2;
  INT16 offset2_of_2;
  WN_ESIZE  esize;
  INT16 cvtl_bits;
  INT64 const_val;
  INT32 label_number;
  UINT32 flags_val;
  INT cgoto;
  INT intrinsic;
  char *intrinsic_name;
  char *args[IR_MAX_ARGS+1];
} TOKEN;

static void ir_error(const char *s);
static INT ir_get_expr_list(void);
static WN * ir_get_expr(void);
static WN * ir_get_stmt(void);
static void ir_skip_token(void);
static char *ir_read_line(char *buf, INT size, FILE *ir_file);
static void ir_match_token(OPERATOR opr);
static void ir_expect_token(OPERATOR opc);
static TOKEN *ir_next_token(void);
static void ir_get_token(TOKEN *token);
static BOOL ir_insert_hash(const char *s, IR_OPCODE_TABLE *irt);
static INT ir_lookup(char *s);
static void ir_build_hashtable(void);
static void ir_put_wn(WN * wn, INT indent);
static void ir_put_expr(WN * wn, INT indent);
static void ir_put_marker(const char *str, INT indent);
static void ir_put_stmt(WN * wn, INT indent);
static void WN_TREE_put_stmt(WN *, INT); // fwd declaration

/* Should we dump the tree in prefix order or postfix order? */
/* The default is postfix */

static BOOL dump_parent_before_children = FALSE;

#ifdef BACK_END
/*
**  declaration is problematic, as the ALIAS_MANAGER is not exposed
**  to consumers of ir_reader.h
*/
extern void fdump_dep_tree(FILE *, const WN *, struct ALIAS_MANAGER *);

/*  Suppress warning if not resolved at link-time. */
/* CG_Dump_Region is defined in cg.so, only call if cg.so is loaded */
#if defined(__linux__) || defined(BUILD_OS_DARWIN) || !defined(SHARED_BUILD)
extern void (*CG_Dump_Region_p) (FILE*, WN*);
#define CG_Dump_Region (*CG_Dump_Region_p)
#else
#pragma weak CG_Dump_Region
#endif // __linux__

#endif /* BACK_END */

BOOL IR_dump_map_info = FALSE;
BOOL IR_dump_region = FALSE;
BOOL IR_DUMPDEP_info = FALSE;
BOOL IR_dump_line_numbers = TRUE;

WN_MAP IR_alias_map = WN_MAP_UNDEFINED;
const struct ALIAS_MANAGER *IR_alias_mgr = NULL;
WN_MAP IR_freq_map = WN_MAP_UNDEFINED;

#define OPCODE_has_alias_info(opc)	(OPCODE_is_load(opc) ||\
                                  	 OPCODE_is_store(opc) ||\
                                  	 OPCODE_operator(opc) == OPR_PARM)

#define IR_dump_alias_info(opc)		(IR_alias_map != WN_MAP_UNDEFINED && OPCODE_has_alias_info(opc))

typedef struct DUMPDEP
{
  WN			*node;
  INT32			id;
  struct DUMPDEP	*next;
} DUMPDEP, *DUMPDEPp;

#define	DUMPDEP_node(x)		(x)->node
#define	DUMPDEP_id(x)		(x)->id
#define	DUMPDEP_next(x)		((x)->next)


DUMPDEPp IR_DUMPDEP_head= NULL;
static INT32 AddToDUMPDEP(WN *node);


/*
**  Add node to list to be processed for a dep graph dump
**  List is processed in order
**  reset id if null. start at 1 (no good reason)
*/
static INT32 AddToDUMPDEP(WN *node)
{
  static DUMPDEPp	last;
  static INT32		id= 0;

  if (IR_DUMPDEP_head==NULL)
  {
    id=		1;
    last=	NULL;
  }

  {
    DUMPDEPp p;

    p = TYPE_L_ALLOC(DUMPDEP);

    DUMPDEP_node(p)=	node;
    DUMPDEP_id(p)=	id++;
    DUMPDEP_next(p)=	NULL;

    if (last)
    {
      DUMPDEP_next(last) = p;
      last=		p;
    }
    else
    {
      IR_DUMPDEP_head=	p;
      last=		p;
    }
    return DUMPDEP_id(p);
  }
}


/*========================================================================
   
    Initialization

 ========================================================================*/

#ifndef MONGOOSE_BE
static FILE *ir_file;
#endif
static FILE *ir_ofile;
static INT  ir_line;
#ifdef IR_TOOLS	
static char *line;
static char *errmsg;
#endif
BOOL follow_st;
static USRCPOS last_srcpos;
static BOOL is_initialized = FALSE;
static WN_MAP ir_put_map = WN_MAP_UNDEFINED;

extern void IR_reader_init(void)
{
  is_initialized = TRUE;
  MEM_POOL_Push(&MEM_local_pool);
#ifdef IR_TOOLS
  errmsg = (char *) MEM_POOL_Alloc(&MEM_local_pool, LINE_LEN);
  line   = (char *) MEM_POOL_Alloc(&MEM_local_pool, LINE_LEN);
//ir_build_hashtable();
#endif
  ir_ofile = stdout;
  follow_st = TRUE;
  USRCPOS_clear(last_srcpos);
}

/* If prefix is true, set the dump order to prefix order instead of postfix order */
/* return the old order. */
/* This function will soon be removed. R. Shapiro */
extern BOOL IR_set_dump_order(BOOL prefix)
{
   BOOL old_order;
   old_order = dump_parent_before_children;
   dump_parent_before_children = prefix;
   return (old_order);
}


#ifndef MONGOOSE_BE
extern void IR_reader_finish(void)
{
  MEM_POOL_Pop(&MEM_local_pool);
}


extern FILE *IR_open(char *filename)
{
   char prefix_check[6];
   ir_file = fopen(filename, "r");
   ir_line = 0;
   
   /* Check for prefix ordered file */
   (void) fread(prefix_check,6,1,ir_file);
   if (strncmp(prefix_check,"PREFIX",6)==0) {
      fclose(ir_file);
      fprintf(stderr,"File %s is prefix ordered and cannot be read.\n",filename);
   } else {
      /* seek back to the start of the file */
      rewind(ir_file);
   }
   return ir_file;
}

extern void IR_close(void)
{
  fclose(ir_file);
}


/*
 *  Write the IR into a file in ascii form.
 */
extern BOOL IR_open_output(char *filename)
{
  if (filename == NULL)
    ir_ofile = stdout;
  else
    if ((ir_ofile = fopen(filename, "w+")) == NULL) {
      ir_error("cannot open file for write");
      return FALSE;
    }
  return TRUE;
}


extern void IR_close_output(void)
{
  if (ir_ofile != NULL && ir_ofile != stdout)
    fclose(ir_ofile);
}
#endif /* MONGOOSE_BE */


/*========================================================================

   IR Error Handling
 
 ========================================================================*/

#define ir_chk_kids(m,n)   {if (m != n) ir_error("wrong number of kids"); }

static void ir_error(const char *s)
{
  fprintf(stderr, "Error parsing ascii IR at line %d: %s.\n", ir_line, s);
  exit(RC_INTERNAL_ERROR);
}


/*========================================================================

   Get source file path name from DST (taken from cgdwarf.c)
 
 ========================================================================*/

typedef struct {
  char *filename;
  INT incl_index;
  FILE *fileptr;
  INT max_line_printed;
} file_info;

static file_info *file_table = NULL;
static char **incl_table;
static INT cur_file_index = 0;
static BOOL file_table_generated = FALSE;

// separated the following functionality from IR_Dwarf_Gen_File_Table
// ir_print_filename allows us to print path names from DST onto the
// ir_ofile in the form of LOC entries.  This function can
// be called (with dump_filenames == TRUE) from multiple places.

static void ir_print_filename(BOOL dump_filenames)
{
/*
    * Optionally, (if dump_filenames == TRUE)
    * all path names present in the DST information may
    * be dumped to the ir_ofile in the form of LOC entries.
*/
  INT count;
  DST_IDX idx;
  DST_FILE_NAME *file;
  char *name;
  INT file_table_size;
  INT new_size;


  file_table_size = 0;
  file_table = NULL;
  count = 1;
  for (idx = DST_get_file_names (); 
       !DST_IS_NULL(idx); 
       idx = DST_FILE_NAME_next(file))
    {
      file = DST_FILE_IDX_TO_PTR (idx);
      if (DST_IS_NULL(DST_FILE_NAME_name(file))) {
        name = "NULLNAME";
      }
      else {
        name = DST_STR_IDX_TO_PTR (DST_FILE_NAME_name(file));
      }
      if (count >= file_table_size) {
        new_size = count + 10;
        if (file_table == NULL)
          file_table = (file_info *) malloc (new_size * sizeof (file_info));
        else 
          file_table = (file_info *) realloc (file_table, 
                                              new_size * sizeof (file_info));
        if (file_table == NULL) 
          fprintf(stderr, "IR_Dwarf_Gen_File_Table: Run out of memory\n");
        file_table_size = new_size;
      }
      file_table[count].filename = name;
      file_table[count].incl_index = DST_FILE_NAME_dir(file);
      file_table[count].fileptr = NULL;
      file_table[count].max_line_printed = 0;
      if (dump_filenames)
        fprintf (ir_ofile, " LOC 0 0 source files:\t%d\t\"%s/%s\"\n",
                 count,
                 incl_table[DST_FILE_NAME_dir(file)],
                 name);
      count++;
    }
  /* make sure all fileptr values are NULL */
  while (count < file_table_size) {
    file_table[count].fileptr = NULL;
    count++;
  }

} /* ir_print_filename */

extern void IR_Dwarf_Gen_File_Table (BOOL dump_filenames)
{
   /* Generate a table of the include directory names and a table
    * of the file names, based on the DST (Debugging Symbol Table).
    * The USRCPOS_filenum() is a positional index into the list of
    * file names as given by DST_get_file_names(), starting at 
    * index==1.  Similarly, the DST_FILE_NAME_dir(file_info) is a
    * positional index into the list of directory names as given by
    * DST_get_dir_names().
    * call ir_print_filename to optionally print all pathnames in the DST info
    */
  INT count;
  DST_IDX idx;
  DST_INCLUDE_DIR *incl;
  char *name;
  INT incl_table_size;
  INT new_size;

  if (file_table_generated && file_table != NULL) {
	/* only need to reset the fileptr and line_printed info */
  	for (count = 1; file_table[count].fileptr != NULL; count++) {
      		fclose (file_table[count].fileptr);
      		file_table[count].fileptr = NULL;
		file_table[count].max_line_printed = 0;
              }
	cur_file_index = 0;
        return;
        }

  incl_table_size = 0;
  incl_table = NULL;
  file_table = NULL;
  count = 1;
  for (idx = DST_get_include_dirs (); 
       !DST_IS_NULL(idx); 
       idx = DST_INCLUDE_DIR_next(incl))
    {
      incl = DST_DIR_IDX_TO_PTR (idx);
      name = DST_STR_IDX_TO_PTR (DST_INCLUDE_DIR_path(incl));
      if (count >= incl_table_size) {
        new_size = count + 10;
        if (incl_table == NULL)
          incl_table = (char **) malloc (new_size * sizeof (char *));
        else 
          incl_table = (char **) realloc (incl_table, new_size * sizeof (char *));
        if (incl_table == NULL) 
          fprintf(stderr, "IR_Dwarf_Gen_File_Table: Run out of memory\n");
        incl_table_size = new_size;
      }
      incl_table[count] = name;
      count++;
    }
#if defined(TARG_SL)
  /* Wenbo/2007-04-29: Because we use incl_table's 0th entry for current
     working dir. */
  if (incl_table == NULL)
    incl_table = (char **) malloc ((count + 2) * sizeof (char *));
  incl_table[0] = "./";
#endif
  
  ir_print_filename(dump_filenames); /* print the loc 0 0 heading */

  file_table_generated = TRUE;
} /* IR_Dwarf_Gen_File_Table */


extern void IR_Srcpos_Filename(SRCPOS srcpos,        /* in */
			       const char **fname,   /* out */
			       const char **dirname) /* out */
{
   /* Get two character strings denoting the file-name and directory
    * path-name for the file-number denoted by the srcpos.  fname
    * and dirname must both be non-NULL references to objects that
    * can denote the character strings returned by this subroutine.
    * For unknown file/directory components, *fname and/or *dirname 
    * will be set to NULL.
    */
   USRCPOS usrcpos;

   USRCPOS_srcpos(usrcpos) = srcpos;
   if (USRCPOS_filenum(usrcpos) == 0)
   {
      *fname = NULL;
      *dirname = NULL;
   }
   else
   {
      file_info *cur_file;

      if (!file_table_generated)
	 IR_Dwarf_Gen_File_Table(FALSE/*dump_filenames*/);

      cur_file = &file_table[USRCPOS_filenum(usrcpos)];
      *fname = cur_file->filename;
      *dirname = incl_table[cur_file->incl_index];
   }
} /* IR_Srcpos_Filename */


/*========================================================================

   Print source lines (taken from cgdwarf.c)
 
 ========================================================================*/
static void
print_source (SRCPOS srcpos)
{
  USRCPOS usrcpos;
  char srcfile[1024];
  char text[1024];
  file_info *cur_file;
  INT i;
  INT newmax;

  USRCPOS_srcpos(usrcpos) = srcpos;

  if (USRCPOS_filenum(usrcpos) == 0) { 
	/* ??? Shouldn't see this, but print it so know when it does occur */
	fprintf(ir_ofile, "LOC 0 %d\n", USRCPOS_linenum(usrcpos));
	return;
  }

  cur_file = &file_table[USRCPOS_filenum(usrcpos)];
  if (USRCPOS_filenum(usrcpos) != cur_file_index) {
    if (cur_file_index != 0) {
      /* close the previous file. */
      file_info *prev_file = &file_table[cur_file_index];
      fclose (prev_file->fileptr);
      prev_file->fileptr = NULL;
    }
    cur_file_index = USRCPOS_filenum(usrcpos);
    cur_file = &file_table[cur_file_index];
    /* open the new file. */
    sprintf (srcfile, "%s/%s", incl_table[cur_file->incl_index], 
				cur_file->filename);
    cur_file->fileptr = fopen (srcfile, "r");
    if (cur_file->fileptr == NULL) {
      cur_file_index = 0;	/* indicate invalid cur_file */
      fprintf (ir_ofile, " LOC %d %d\n", cur_file_index, 
	       USRCPOS_linenum(usrcpos));
      return;
    }
    cur_file->max_line_printed = 0;	/* reset so lines will be reprinted */
    newmax = USRCPOS_linenum(usrcpos) - 2;
  }
  else {
    newmax = USRCPOS_linenum(usrcpos) - 5;
  }
  if (cur_file->max_line_printed < newmax) {
    for (i = cur_file->max_line_printed; i < newmax; i++) {
      fgets (text, sizeof(text), cur_file->fileptr);
    }
    cur_file->max_line_printed = newmax;
  }
  if (cur_file->max_line_printed < USRCPOS_linenum(usrcpos)) {
    for (i = cur_file->max_line_printed; i < USRCPOS_linenum(usrcpos); i++) {
      if (fgets (text, sizeof(text), cur_file->fileptr) != NULL)
        fprintf (ir_ofile, " LOC %d %d %s", cur_file_index, i+1, text);
    }
    cur_file->max_line_printed = USRCPOS_linenum(usrcpos);
  }
  else if (cur_file->max_line_printed != USRCPOS_linenum(usrcpos)
	&& USRCPOS_linenum(usrcpos) != 0) 
  {
    /* 
     * could be a line before the last LOC;
     * too hard to print text, but at least print LOC
     */
    fprintf (ir_ofile, " LOC %d %d\n", cur_file_index, 
	USRCPOS_linenum(usrcpos));
  }
}

/*========================================================================

  Misc utilities

 ========================================================================*/
 

#ifdef IR_TOOLS	/* only included for tools which translate ascii->binary */

/*========================================================================

  Hash table routines for the lookup keywords.

    ir_insert_hash
    ir_lookup
    ir_build_hashtable

========================================================================*/


/*
 *  Find a new entry from the string in the operator table.
 */
static BOOL ir_insert_hash(const char *s, IR_OPCODE_TABLE *opcode_entry)
{
  const char *p;
  UINT sum = 0;
  INT i;

  for (p = s; *p != '\0'; p++)
    sum = (sum+sum+3377)^*p;	/* better than sum+= *p for our keyword set */
//if (sum < 0) sum = -sum;
  sum %= HASH_LEN;
  for (i = 0; i < HASH_LEN; i++) {
    if (opcode_hash[sum] == NULL) {
      opcode_hash[sum] = opcode_entry;
      return TRUE;
    }
    if (strcmp(opcode_hash[sum]->pr_name, s) == 0)
      return FALSE;
    sum = (sum + 1) % HASH_LEN;
  }
  return FALSE;
}


/*
 *  Lookup the string from the operator table.
 */
static INT ir_lookup(char *s)
{
  char *p;
  INT sum = 0;
  INT i;

  for (p = s; *p != '\0'; p++)
    sum = (sum+sum+3377)^*p;
  sum %= HASH_LEN;
  for (i = 0; i < HASH_LEN; i++) {
    if (opcode_hash[sum] == NULL)
      return -1;
    if (strcmp(opcode_hash[sum]->pr_name, s) == 0)
      return (opcode_hash[sum] - ir_opcode_table);
    sum = (sum + 1) % HASH_LEN;
  }
  return -1;
}


/*
 *  Enter opcode into the IR table.
 */
static void enter_opcode_table(const char *name, OPCODE opc, INT opr)
{
    ir_opcode_table[opc].pr_name = name;
    if (!ir_insert_hash(ir_opcode_table[opc].pr_name, &ir_opcode_table[opc])) {
      sprintf(errmsg, "cannot insert %s into hash table", OPCODE_name(opc));
      ir_error(errmsg);
    }
}


static inline
void enter_opcode_table (const char *name, OPC_EXTENDED opc, INT opr)
{
    enter_opcode_table (name, (OPCODE) opc, opr);
}

/*
 *  Build IR table 
 */
static void ir_build_hashtable(void)
{
  INT i;
  INT opr;
  
  /* Clear the tables */
  memset(ir_opcode_table, 0, sizeof(ir_opcode_table));
  memset(opcode_hash,     0, sizeof(opcode_hash));

  /* Enter the WHIRL opcodes. */
  for (i = OPCODE_FIRST; i <= OPCODE_LAST; i++) {
      OPCODE opc = (OPCODE) i;
    opr = OPCODE_operator(opc);
    enter_opcode_table(((char *)OPCODE_name(opc))+strlen("OPC_"), opc, opr);
  }

  /* Enter the extended opcodes. */
  enter_opcode_table("BODY", OPC_BODY, OPR_BODY);
  enter_opcode_table("NOOP", OPC_NOOP, OPR_NOOP);
  enter_opcode_table("END_BLOCK", OPC_END_BLOCK, OPR_END_BLOCK);
  enter_opcode_table("THEN", OPC_THEN, OPR_THEN);
  enter_opcode_table("ELSE", OPC_ELSE, OPR_ELSE);
  enter_opcode_table("END_IF", OPC_END_IF, OPR_END_IF);
  enter_opcode_table("INIT", OPC_INIT, OPR_INIT);
  enter_opcode_table("COMP", OPC_COMP, OPR_COMP);
  enter_opcode_table("INCR", OPC_INCR, OPR_INCR);
  enter_opcode_table("END_COMPGOTO", OPC_END_COMPGOTO, OPR_END_COMPGOTO);
  enter_opcode_table("END_XGOTO", OPC_END_XGOTO, OPR_END_XGOTO);
  enter_opcode_table("LOC", OPC_LOC, OPR_LOC);
  enter_opcode_table("END_LOOP_INFO", OPC_END_LINFO, OPR_END_LINFO);
  enter_opcode_table("END_SWITCH", OPC_END_SWITCH, OPR_END_SWITCH);
}

#endif /* IR_TOOLS */


/*========================================================================

  IR Ascii Writer
    ir_put_wn
    ir_put_expr
    ir_put_stmt
    IR_put_func

 ========================================================================*/

static void
ir_put_st (ST_IDX st_idx)
{
  char *name;
  char *p;
  
  if (st_idx == (ST_IDX) 0) {
    /* labels may have empty st */
    fprintf(ir_ofile, " <null-st>");

  } else if (!follow_st) {
    /* Do not follow ST *, so that it can dump useful information
       even when ST * is not valid */
    fprintf(ir_ofile, " <st %d>", (INT32) st_idx);

  } else {
    const ST* st = &St_Table[st_idx];
    if (ST_class(st) == CLASS_CONST) {
      name = Targ_Print(NULL, STC_val(st));
      /* new lines and spaces in constant strings 
       * will mess up the ascii reader,
       * so replace with underlines */
      for (p = name; *p != '\0'; p++)
	switch (*p) {
	case ' ':
	case '\t':
	case '\n':
	  *p = '_';
	}
    } else
      name = ST_name(st);
    fprintf (ir_ofile, " <%d,%d,%s>", ST_level (st), ST_index (st), name);
  }
}

#ifdef BACK_END
extern "C" UINT16 LNOGetVertex(WN *);
extern "C" BOOL LnoDependenceEdge(WN *, WN *, mUINT16 *, DIRECTION *, BOOL *, BOOL *);
#endif


/* 
 * little routine to print TY's and their attributes, since otherwise they aren't visible
 */
static void
ir_put_ty(TY_IDX ty)
{
   fprintf(ir_ofile, " T<%d,%s,%d", TY_id(ty),
	   TY_name(ty),TY_align(ty));
   
   if (TY_is_restrict(ty)) 
      fprintf(ir_ofile, ",R");
   
   if (TY_is_volatile(ty)) 
      fprintf(ir_ofile, ",V");
   
   if (TY_is_const(ty)) 
      fprintf(ir_ofile, ",C");
   
   fprintf(ir_ofile, ">");
}

#if defined(BACK_END) || defined(IR_TOOLS)
static void ir_put_phi_list(WN* wn, INT indent)
{
   const WSSA::WHIRL_SSA_MANAGER * wsm = PU_Info_ssa_ptr(Current_PU_Info);
   if (wsm->Stat() != WSSA::STAT_DUMP)
      return;

   if (! wsm->WN_has_phi(wn))
      return;

   ir_put_marker("PHI NODES",indent);
   for (WSSA::WHIRL_SSA_MANAGER::const_phi_iterator phi_iter = wsm->WN_phi_begin(wn);
        phi_iter != wsm->WN_phi_end(wn);
        ++phi_iter) {
      phi_iter->Print(ir_ofile, indent);
   }
} 

static void ir_put_chi_list(WN* wn, INT indent)
{
   const WSSA::WHIRL_SSA_MANAGER * wsm = PU_Info_ssa_ptr(Current_PU_Info);
   if (wsm->Stat() != WSSA::STAT_DUMP)
      return;

   if (! wsm->WN_has_chi(wn))
      return;

   ir_put_marker("CHI NODES",indent);
   for (WSSA::WHIRL_SSA_MANAGER::const_chi_iterator chi_iter = wsm->WN_chi_begin(wn);
        chi_iter != wsm->WN_chi_end(wn);
        ++chi_iter) {
      chi_iter->Print(ir_ofile, indent);
   }
}

static void ir_put_mu_list(WN* wn, INT indent)
{
   const WSSA::WHIRL_SSA_MANAGER * wsm = PU_Info_ssa_ptr(Current_PU_Info);
   if (wsm->Stat() != WSSA::STAT_DUMP)
       return;

   if (! wsm->WN_has_mu(wn))
      return;

   ir_put_marker("MU NODES",indent);
   for (WSSA::WHIRL_SSA_MANAGER::const_mu_iterator mu_iter = wsm->WN_mu_begin(wn);
        mu_iter != wsm->WN_mu_end(wn);
        ++mu_iter) {
      mu_iter->Print(ir_ofile, indent);
   }
}
#endif

/*
 *  Write an WN * in ascii form on an individual line.
 */ 
static void ir_put_wn(WN * wn, INT indent)
{
    OPCODE opcode;

    if (wn == NULL) {
    	/* null statement */
    	fprintf(ir_ofile, "### error: null WN pointer\n");
   	return;
    } else {
	if (IR_dump_wn_id) {
#ifdef WHIRL_USE_UNIQUE_ID_FOR_DEBUG
	    fprintf(ir_ofile, "[%6u] ", WN_id(wn));
#endif
	}
	if (IR_dump_wn_addr) {
	    fprintf(ir_ofile, "%8p: ", wn);
	}
	opcode = WN_opcode(wn);
    }
    if (opcode == 0) {
	fprintf(ir_ofile,"### error: WN opcode 0\n");
	return;
    }
    /*
     *  for dependency dumping, dump a handle to refer to later
     */
    if (IR_DUMPDEP_info) {
	INT32 handle=  0;
	if (OPCODE_has_alias_info(WN_opcode(wn)) && WN_map_id(wn) != -1) {
	    handle= AddToDUMPDEP(wn);
	}
	fprintf(ir_ofile, "[%5d]", handle);
    }

#if defined(BACK_END) || defined(IR_TOOLS)
    if (OPT_Enable_WHIRL_SSA && WSSA::WN_has_mu(wn)) {
        ir_put_mu_list(wn, indent);
    }
#endif

    if (indent > 0 && opcode == OPC_LABEL)
	fprintf(ir_ofile, "%*s", indent-1, "");
    else
	fprintf(ir_ofile, "%*s", indent, "");

    fprintf(ir_ofile, "%s", OPCODE_name(opcode) + strlen("OPC_"));
    if (OPCODE_has_offset(opcode)) {
	if (OPCODE_operator(opcode) == OPR_PRAGMA || 
	    OPCODE_operator(opcode) == OPR_XPRAGMA)
	    fprintf(ir_ofile, " %d %d", WN_pragma_flags(wn), WN_pragma(wn));
	else
	    fprintf(ir_ofile, " %d", WN_offset(wn));
    } else if (OPCODE_has_2offsets(opcode)) {
	fprintf(ir_ofile, " %d %d",
		WN_loop_trip_est(wn), WN_loop_depth(wn));
    }

    switch (OPCODE_operator(opcode)) {

    case OPR_INTRINSIC_OP:
    case OPR_ARRAYEXP:
#ifdef KEY
    case OPR_PURE_CALL_OP:
#endif
      fprintf(ir_ofile, " %d", WN_kid_count(wn));
      break;

    case OPR_REGION:
      fprintf(ir_ofile, " %d", WN_region_id(wn));
#ifdef BACK_END
      {
	RID *rid = REGION_get_rid(wn);
	if (rid != NULL)
	  fprintf(ir_ofile, " %d", RID_id(rid));
      }
#endif /* BACK_END */
      fprintf(ir_ofile, " (kind=%d)",WN_region_kind(wn));
      break;
#if defined(TARG_SL)
    case OPR_LDA:
    case OPR_ISTORE:
      fprintf(ir_ofile, "im:%d", WN_is_internal_mem_ofst(wn)); 
      break; 
#endif
    case OPR_LDBITS:
    case OPR_ILDBITS:
    case OPR_STBITS:
    case OPR_ISTBITS:
    case OPR_EXTRACT_BITS:
    case OPR_COMPOSE_BITS:
      fprintf(ir_ofile, " <bofst:%d bsize:%d>", WN_bit_offset(wn),
	      WN_bit_size(wn));
      break;

    case OPR_ASM_INPUT:
      fprintf(ir_ofile, " opnd:%d", WN_asm_opnd_num(wn));
      break;

    default:
      break;
    }

    if (OPCODE_has_inumber(opcode)) {
	switch (opcode) {
	case OPC_IO:
	    fprintf(ir_ofile, " <%d,%s,%s>", WN_intrinsic(wn),
		    IOSTATEMENT_name((IOSTATEMENT) WN_intrinsic(wn)),
		    get_iolibrary_name(WN_IO_Library(wn)));
	    break;
	case OPC_IO_ITEM:
	    fprintf(ir_ofile, " <%d,%s>", WN_intrinsic(wn),
		    IOITEM_name((IOITEM) WN_intrinsic(wn)));
	    break;
	default:		/* intrinsic */
	    Is_True(OPCODE_operator(opcode) == OPR_INTRINSIC_OP ||
		    OPCODE_operator(opcode) == OPR_INTRINSIC_CALL,
		    ("ir_put_wn, expected an intrinsic"));
#if defined(BACK_END) || defined(IR_TOOLS)
	    fprintf(ir_ofile, " <%d,%s>", WN_intrinsic(wn),
		    INTRINSIC_name((INTRINSIC) WN_intrinsic(wn)));
#endif
	    break;
	}
    }

    if (OPCODE_has_bits(opcode))
	fprintf(ir_ofile, " %d", WN_cvtl_bits(wn));
    if (OPCODE_has_label(opcode))
	fprintf(ir_ofile, " L%d", WN_label_number(wn));
    if (OPCODE_has_flags(opcode)) 
	fprintf(ir_ofile, " %d", WN_flag(wn));
    if (OPCODE_has_sym(opcode)) {
        ir_put_st (WN_st_idx(wn));
#if defined(BACK_END) || defined(IR_TOOLS)
        if (OPT_Enable_WHIRL_SSA) {
            WSSA::WHIRL_SSA_MANAGER * wsm = PU_Info_ssa_ptr(Current_PU_Info);
            Is_True( wsm != NULL, ("WHIRL SSA MANAGER is NULL") );
            if (wsm->Stat() == WSSA::STAT_DUMP) {
                if (wsm->WN_has_ver(wn)) {
                    WSSA::VER_IDX ver = wsm->Get_wn_ver(wn);
                    fprintf(ir_ofile, " ");
                    Print_ver(ir_ofile, ver);
                }
                else {
                    fprintf(ir_ofile, " INV_VER");
                }
            }
        }
#endif
    }

    if (OPCODE_has_1ty(opcode)) {
	if (WN_ty(wn) != (TY_IDX) 0) 
	   ir_put_ty(WN_ty(wn));
	else
	    if (opcode != OPC_IO_ITEM)
		fprintf(ir_ofile, " T<### ERROR: null ptr>");
    } else if (OPCODE_has_2ty(opcode)) {
	if (WN_ty(wn) != (TY_IDX) 0) 
	   ir_put_ty(WN_ty(wn));
	else
	    fprintf(ir_ofile, " T<### ERROR: null ptr>");
	if (WN_load_addr_ty(wn) != (TY_IDX) 0) 
	   ir_put_ty(WN_load_addr_ty(wn));
	else
	    fprintf(ir_ofile, " T<### ERROR: null ptr>");
    }

    if (OPCODE_has_ndim(opcode))
	fprintf(ir_ofile, " %d", WN_num_dim(wn));
    if (OPCODE_has_esize(opcode))
	fprintf(ir_ofile, " %" LL_FORMAT "d", WN_element_size(wn));

    if (OPCODE_has_num_entries(opcode))
	fprintf(ir_ofile, " %d", WN_num_entries(wn));
    if (OPCODE_has_last_label(opcode))
	fprintf(ir_ofile, " %d", WN_last_label(wn));

    if (OPCODE_has_value(opcode)) {
	fprintf(ir_ofile, " %" LL_FORMAT "d", WN_const_val(wn));
	/* Also print the hex value for INTCONSTs */
	if (OPCODE_operator(opcode) == OPR_INTCONST || opcode == OPC_PRAGMA) {
	    fprintf(ir_ofile, " (0x%" LL_FORMAT "x)", WN_const_val(wn));
	}
    }

    if (OPCODE_has_field_id(opcode) && WN_field_id(wn)) {
	fprintf(ir_ofile, " <field_id:%u>", WN_field_id(wn));
    }
    

    if (OPCODE_has_ereg_supp(opcode)) {
	INITO_IDX ino = WN_ereg_supp(wn);
	if (ino != 0)
	    fprintf (ir_ofile, " INITO<%d,%s>", INITO_IDX_index (ino), 
		     ST_name (INITO_st_idx (Inito_Table[ino])));
    }
    if (opcode == OPC_COMMENT) {
	fprintf(ir_ofile, " # %s", Index_To_Str(WN_offset(wn)));
    }

    if (follow_st && OPCODE_has_sym(opcode) && OPCODE_has_offset(opcode)
	&& WN_st_idx(wn) != (ST_IDX) 0 && (ST_class(WN_st(wn)) == CLASS_PREG)
        && opcode != OPC_PRAGMA)
	{
	    if (Preg_Is_Dedicated(WN_offset(wn))) {
		if (Preg_Offset_Is_Int(WN_offset(wn))) {
	    		fprintf(ir_ofile, " # $r%d", WN_offset(wn));
		}
		else if (Preg_Offset_Is_Float(WN_offset(wn))) {
	    		fprintf(ir_ofile, " # $f%d", 
				WN_offset(wn) - Float_Preg_Min_Offset);
		}
#ifdef TARG_X8664
		else if (Preg_Offset_Is_X87(WN_offset(wn))) {
	    		fprintf(ir_ofile, " # $st%d", 
				WN_offset(wn) - X87_Preg_Min_Offset);
		}
#endif
	    }
	    else { 
	    	/* reference to a non-dedicated preg */
		if ((WN_offset(wn) - Last_Dedicated_Preg_Offset) 
			< PREG_Table_Size (CURRENT_SYMTAB) )
	    	    fprintf(ir_ofile, " # %s", Preg_Name(WN_offset(wn)));
		else
		    fprintf(ir_ofile, " # <Invalid PREG Table index (%d)>",
			    WN_offset(wn));
	    }
	}

    if (opcode == OPC_XPRAGMA) {
	fprintf(ir_ofile, " # %s", WN_pragmas[WN_pragma(wn)].name);
    }

    if (OPCODE_operator(opcode) == OPR_ASM_INPUT) {
      fprintf(ir_ofile, " # \"%s\"", WN_asm_input_constraint(wn));
    }

    if (opcode == OPC_PRAGMA) {
	fprintf(ir_ofile, " # %s", WN_pragmas[WN_pragma(wn)].name);
	switch(WN_pragma(wn)) {
	case WN_PRAGMA_DISTRIBUTE:
	case WN_PRAGMA_REDISTRIBUTE:
	case WN_PRAGMA_DISTRIBUTE_RESHAPE:
	    fprintf(ir_ofile, ", %d",WN_pragma_index(wn));
	    switch(WN_pragma_distr_type(wn)) {
	    case DISTRIBUTE_STAR:
		fprintf(ir_ofile, ", *");
		break;
	    case DISTRIBUTE_BLOCK:
		fprintf(ir_ofile, ", BLOCK");
		break;
	    case DISTRIBUTE_CYCLIC_CONST:
		fprintf(ir_ofile, ", CYCLIC(%d)", WN_pragma_preg(wn));
		break;
	    case DISTRIBUTE_CYCLIC_EXPR:
		fprintf(ir_ofile, ", CYCLIC(expr)");
		break;
	    }
	    break;
	case WN_PRAGMA_ASM_CONSTRAINT:
	  fprintf(ir_ofile, ", \"%s\", opnd:%d preg:%d",
		  WN_pragma_asm_constraint(wn),
		  WN_pragma_asm_opnd_num(wn),
		  WN_pragma_asm_copyout_preg(wn));
	  break;
	default:
            if (WN_pragma_arg2(wn) != 0 )
		fprintf(ir_ofile, ", %d, %d", WN_pragma_arg1(wn), WN_pragma_arg2(wn));
            else
		if (WN_pragma_arg1(wn) != 0 )
		    fprintf(ir_ofile, ", %d", WN_pragma_arg1(wn));
	    break;
	} /* switch */
    }

    if (OPCODE_operator(opcode) == OPR_ASM_STMT) {
      fprintf(ir_ofile, " # \"%s\"", WN_asm_string(wn));
      if (WN_Asm_Volatile(wn))
      	fprintf(ir_ofile, " (volatile)");
      if (WN_Asm_Clobbers_Mem(wn))
      	fprintf(ir_ofile, " (memory)");
      if (WN_Asm_Clobbers_Cc(wn))
      	fprintf(ir_ofile, " (cc)");
    }

    if (OPCODE_is_call(opcode))
	fprintf(ir_ofile, " # flags 0x%x", WN_call_flag(wn));

    if (OPCODE_operator(opcode) == OPR_PARM) {
	INT flag =  WN_flag(wn);
	fprintf(ir_ofile, " # ");
	if (flag & WN_PARM_BY_REFERENCE) fprintf(ir_ofile, " by_reference ");
#if defined(TARG_SL)
	if (flag & WN_PARM_DEREFERENCE) fprintf(ir_ofile, " by_dereference ");
#endif
	if (flag & WN_PARM_BY_VALUE)     fprintf(ir_ofile, " by_value ");
	if (flag & WN_PARM_OUT)          fprintf(ir_ofile, " out ");
	if (flag & WN_PARM_DUMMY)        fprintf(ir_ofile, " dummy ");
	if (flag & WN_PARM_READ_ONLY)    fprintf(ir_ofile, " read_only ");
	if (flag & WN_PARM_PASSED_NOT_SAVED) fprintf(ir_ofile, "passed_not_saved ");
	if (flag & WN_PARM_NOT_EXPOSED_USE) fprintf(ir_ofile, " not_euse ");
	if (flag & WN_PARM_IS_KILLED) fprintf(ir_ofile, " killed ");
    }

    if (IR_dump_map_info) {
	fprintf(ir_ofile, " # <id %d:%d>", OPCODE_mapcat(opcode),
		WN_map_id(wn));
	if (ir_put_map && ( WN_map_id(wn) != -1 )) {
	    switch ( WN_MAP_Get_Kind( ir_put_map ) ) {
	    case WN_MAP_KIND_VOIDP:
		fprintf(ir_ofile, " <map %8p>", WN_MAP_Get( ir_put_map, wn ));
		break;
	    case WN_MAP_KIND_INT32:
		fprintf(ir_ofile, " <map %08x>", WN_MAP32_Get( ir_put_map, wn ));
		break;
	    case WN_MAP_KIND_INT64:
		fprintf(ir_ofile, " <map %08llx>", WN_MAP64_Get( ir_put_map, wn ));
		break;
	    }
	}
#ifdef BACK_END
	if (UINT16 vertex = LNOGetVertex(wn)) {
	    fprintf(ir_ofile, " <lno vertex %d>", vertex);
	}
#endif
    }

    if (IR_dump_line_numbers &&
        (OPCODE_is_scf(WN_opcode(wn)) || OPCODE_is_stmt(WN_opcode(wn)))) {

	USRCPOS srcpos;
	USRCPOS_srcpos(srcpos) = WN_Get_Linenum(wn);

	fprintf(ir_ofile, " {line: %d/%d}", USRCPOS_filenum(srcpos), USRCPOS_linenum(srcpos));
    }

#ifdef BACK_END
    if (IR_dump_alias_info(WN_opcode(wn))) {
      fprintf(ir_ofile, " [alias_id: %d%s]", WN_MAP32_Get(IR_alias_map, wn),
	      IR_alias_mgr && IR_alias_mgr->Safe_to_speculate(wn) ? ",fixed" : "");
    }
#endif

    if (IR_freq_map != WN_MAP_UNDEFINED && (OPCODE_is_scf(WN_opcode(wn)) ||
					    OPCODE_is_stmt(WN_opcode(wn)))) {
	USRCPOS srcpos;
	USRCPOS_srcpos(srcpos) = WN_Get_Linenum(wn);

	fprintf(ir_ofile, " {freq: %d, ln: %d, col: %d}",
		WN_MAP32_Get(IR_freq_map, wn),
		USRCPOS_linenum(srcpos),
		USRCPOS_column(srcpos));
    }
    if (Current_Map_Tab != NULL 
    	&& WN_MAP32_Get(WN_MAP_ALIAS_CLASS, wn) != 0) 
    {
      fprintf(ir_ofile, " {class %d}",
	      WN_MAP32_Get(WN_MAP_ALIAS_CLASS, wn));
    }

    if (Current_Map_Tab != NULL &&
        WN_MAP32_Get(WN_MAP_ALIAS_CGNODE, wn) != 0) 
    {
      if (OPERATOR_is_call(WN_operator(wn)))
        fprintf(ir_ofile, " {callsite %d}",
                WN_MAP32_Get(WN_MAP_ALIAS_CGNODE, wn));
      else
        fprintf(ir_ofile, " {cgnode %d}",
                WN_MAP32_Get(WN_MAP_ALIAS_CGNODE, wn));
    }

#ifdef BACK_END
    AliasAnalyzer *aa = AliasAnalyzer::aliasAnalyzer();
    if (Current_Map_Tab != NULL && aa != NULL && aa->getAliasTag(wn) != 0)
      fprintf(ir_ofile," {alias_tag %d}", aa->getAliasTag(wn));
#endif

    fprintf(ir_ofile, "\n");
#if defined(BACK_END) || defined(IR_TOOLS)
    if (OPT_Enable_WHIRL_SSA && WSSA::WN_has_chi(wn)) {
        ir_put_chi_list(wn, indent);
    }
#endif
}

/*
 *  Write an expression and its children in postfix order.
 *  If dump_parent_before_children, write it in prefix order instead.
 */
static void ir_put_expr(WN * wn, INT indent)
{
  INT i;
  WN * wn2;

  /* See if the parent op should be dumped before or after the children */
  if (dump_parent_before_children) {
     ir_put_wn(wn,indent);
  }
  for (i = 0; i < WN_kid_count(wn); i++) {
    wn2 = WN_kid(wn,i);
    if (wn2) {
      OPCODE op = WN_opcode(wn2);
      if ((OPCODE_FIRST <= op && op <= OPCODE_LAST) &&
	  (OPCODE_is_expression(op) || OPCODE_is_call(op)))
	      ir_put_expr(WN_kid(wn,i), indent+1);
      else
      if (    op == OPC_BLOCK
           && (    (WN_operator(wn) == OPR_RCOMMA && i == 1)
                || (WN_operator(wn) == OPR_COMMA && i == 0)))
        ir_put_stmt(wn2, indent+1);
      else
	fprintf(ir_ofile, "%*sopcode %d not an expression\n", indent+1, "", op);
    } else  
      fprintf(ir_ofile, "%*snull-expression\n", indent+1, "");
  }
  if (!dump_parent_before_children) {
     ir_put_wn(wn, indent);
  }
}


/*
 *  Write out a marker at the indentation level.
 */
static void ir_put_marker(const char *str, INT indent)
{
  if (IR_dump_wn_id) {
#ifdef WHIRL_USE_UNIQUE_ID_FOR_DEBUG
    indent += 9;
#endif
  }
  if (IR_dump_wn_addr) {
    indent += 10;
  }
  fprintf(ir_ofile, "%*s%s\n", indent, "", str);
}


/*
 *  Write a statement WN * and its children in prefix order.
 */
static void ir_put_stmt(WN * wn, INT indent)
{
  INT i;
  WN * wn2;
  USRCPOS srcpos;
  
  if (wn) {
    OPCODE opc = WN_opcode(wn);
    BOOL already_dumped_wn = FALSE;
    USRCPOS_srcpos(srcpos) = WN_Get_Linenum(wn);
    if (USRCPOS_srcpos(srcpos) != 0 && 
	USRCPOS_srcpos(srcpos) != USRCPOS_srcpos(last_srcpos)) {
      last_srcpos = srcpos;
#ifdef FRONT_END
      fprintf(ir_ofile, "%*sLOC %d %d\n", indent, "",
		USRCPOS_filenum(srcpos), USRCPOS_linenum(srcpos));
#else
      //print_source(USRCPOS_srcpos(srcpos));
#endif
    }

    if (OPCODE_is_scf(opc) || dump_parent_before_children) {
      ir_put_wn(wn, indent);
      already_dumped_wn = TRUE;
    }

    switch (opc) {

    case OPC_BLOCK:
      wn2 = WN_first(wn);
      while (wn2) {
	ir_put_stmt(wn2, indent);
	wn2 = WN_next(wn2);
      }
      ir_put_marker("END_BLOCK", indent);
      break;

    case OPC_REGION:
      ir_put_marker("REGION EXITS", indent);
      ir_put_stmt(WN_region_exits(wn), indent+1);

      ir_put_marker("REGION PRAGMAS", indent);
      ir_put_stmt(WN_region_pragmas(wn), indent+1);

      ir_put_marker("REGION BODY", indent);
      ir_put_stmt(WN_region_body(wn), indent+1);

      /* check to make sure cg.so is loaded first */
      /* IR_dump_region will be NULL if it is not */
#ifdef BACK_END
      if (IR_dump_region)
	CG_Dump_Region(ir_ofile, wn);
#endif /* BACK_END */
      { char str[20];
	sprintf(str,"END_REGION %d", WN_region_id(wn));
	ir_put_marker(str, indent);
      }
      break;
      
    case OPC_LABEL:
      ir_put_wn(wn, indent);
      if ( WN_label_loop_info(wn) != NULL ) {
	ir_put_stmt(WN_label_loop_info(wn), indent+1);
      }
#if defined(BACK_END) || defined(IR_TOOLS)
      if (OPT_Enable_WHIRL_SSA)
        ir_put_phi_list(wn, indent);
#endif
      already_dumped_wn = TRUE;
      break;

    case OPC_IF:
      ir_put_expr(WN_if_test(wn), indent+1);
      if (WN_then(wn)) {
	ir_put_marker("THEN", indent);
	ir_put_stmt(WN_then(wn), indent+1);
      }
      if (WN_else(wn)) {
	ir_put_marker("ELSE", indent);
	ir_put_stmt(WN_else(wn), indent+1);
      }
      ir_put_marker("END_IF", indent);
#if defined(BACK_END) || defined(IR_TOOLS)
      if (OPT_Enable_WHIRL_SSA)
        ir_put_phi_list(wn, indent);
#endif
      break;

    case OPC_DO_LOOP:
      ir_put_expr(WN_index(wn), indent+1);
      ir_put_marker("INIT",indent);
      ir_put_stmt(WN_start(wn), indent+1);
#if defined(BACK_END) || defined(IR_TOOLS)
      if (OPT_Enable_WHIRL_SSA) {
        ir_put_phi_list(wn, indent+1);
      }
#endif
      ir_put_marker("COMP", indent);
      ir_put_expr(WN_end(wn), indent+1);
      ir_put_marker("INCR", indent);
      ir_put_stmt(WN_step(wn), indent+1);
      /* optional loop_info */
      if ( WN_do_loop_info(wn) != NULL ) {
	ir_put_stmt(WN_do_loop_info(wn), indent);
      }
      ir_put_marker("BODY", indent);
      ir_put_stmt(WN_do_body(wn), indent+1);
      break;

    case OPC_WHILE_DO:
#if defined(BACK_END) || defined(IR_TOOLS)
      if (OPT_Enable_WHIRL_SSA) {
        ir_put_phi_list(wn, indent+1);
      }
#endif
      ir_put_marker("COMP",indent);
      ir_put_expr(WN_kid(wn, 0), indent+1);
      ir_put_marker("BODY", indent);
      ir_put_stmt(WN_kid(wn, 1), indent+1);
      break;

    case OPC_DO_WHILE:
#if defined(BACK_END) || defined(IR_TOOLS)
      if (OPT_Enable_WHIRL_SSA) {
        ir_put_phi_list(wn, indent+1);
      }
#endif
      ir_put_marker("BODY", indent);
      ir_put_stmt(WN_kid(wn, 1), indent+1);
      ir_put_marker("COMP",indent);
      ir_put_expr(WN_kid(wn, 0), indent+1);
      break;

    case OPC_LOOP_INFO:
      ir_put_wn(wn, indent);
      if ( WN_loop_induction(wn) != NULL ) {
	ir_put_expr(WN_loop_induction(wn), indent+1);
      }
      if ( WN_loop_trip(wn) != NULL ) {
	ir_put_expr(WN_loop_trip(wn), indent+1);
      }
      ir_put_marker("END_LOOP_INFO", indent);
      already_dumped_wn = TRUE;
      break;

    case OPC_COMPGOTO:
      ir_put_wn(wn, indent);
      ir_put_expr(WN_kid(wn,0), indent+1);
      ir_put_stmt(WN_kid(wn,1), indent+1);
      if (WN_kid_count(wn) > 2)
	ir_put_stmt(WN_kid(wn,2), indent+1);
      ir_put_marker("END_COMPGOTO", indent);
      already_dumped_wn = TRUE;
      break;

    case OPC_SWITCH:
      ir_put_wn(wn, indent);
      ir_put_expr(WN_kid(wn,0), indent+1);
      ir_put_stmt(WN_kid(wn,1), indent+1);
      if (WN_kid_count(wn) > 2)
	ir_put_stmt(WN_kid(wn,2), indent+1);
      ir_put_marker("END_SWITCH", indent);
#if defined(BACK_END) || defined(IR_TOOLS)
      if (OPT_Enable_WHIRL_SSA) {
        ir_put_phi_list(wn, indent+1);
      }
#endif
      already_dumped_wn = TRUE;
      break;

    case OPC_XGOTO:
      ir_put_wn(wn, indent);
      ir_put_expr(WN_kid(wn,0), indent+1);
      ir_put_stmt(WN_kid(wn,1), indent+1);
      ir_put_marker("END_XGOTO", indent);
      already_dumped_wn = TRUE;
      break;

    case OPC_WHERE:
      ir_put_expr(WN_kid(wn,0), indent+1);
      ir_put_marker("BODY", indent);
      ir_put_stmt(WN_kid(wn,1), indent+1);
      ir_put_stmt(WN_kid(wn,2), indent+1);
      break;

    case OPC_EXC_SCOPE_BEGIN:
      {
        INT i;
	for (i = 0; i < WN_kid_count(wn); i++)
	  ir_put_stmt(WN_kid(wn, i), indent+1);
	break;
      }
    case OPC_EXC_SCOPE_END:
     break;

    case OPC_ASM_STMT:
      ir_put_wn(wn, indent);
      already_dumped_wn = TRUE;
      ir_put_stmt(WN_kid(wn,0), indent+1);
      ir_put_stmt(WN_kid(wn,1), indent+1);
      ir_put_marker("ASM_INPUTS", indent);
      {
	INT i;
	for (i = 2; i < WN_kid_count(wn); i++) {
	  ir_put_expr(WN_kid(wn,i), indent+1);
	}
      }
      ir_put_marker("END_ASM_INPUTS", indent);
      break;

    default: 
      {
	INT last_is_expr = TRUE;
	OPCODE opc2;
	for (i = 0; i < WN_kid_count(wn); i++) {
	  wn2 = WN_kid(wn,i);
	  if (wn2) {
	    opc2 = WN_opcode(wn2);
	    if (opc2 == 0) {
	       fprintf(ir_ofile, "### error: WN opcode 0\n");
	    } else if (OPCODE_is_expression(opc2)) {
	      ir_put_expr(wn2, indent+1);
	      last_is_expr = 1;
	    }
	    else if (OPCODE_is_stmt(opc2) || OPCODE_is_scf(opc2)) {
	      if (last_is_expr) {
      		ir_put_marker("BODY", indent);
		ir_put_stmt(wn2, indent+1);
	      } else
		ir_put_stmt(WN_kid(wn,i), indent+1);
	      last_is_expr = 0;
	    } else {
	       fprintf(ir_ofile, "### error: unknown opcode type %d\n",opc2);
	    }
	  } else
	    ir_put_stmt(wn2, indent+1);
	}
      }
    }
    if (!already_dumped_wn)
      ir_put_wn(wn, indent);
  } else
    ir_put_wn(wn, indent);
}

#ifdef BACK_END

static void fdump_DUMPDEP(FILE *f, DUMPDEPp head, struct ALIAS_MANAGER *alias)
{
    DUMPDEPp	p,q;

    fprintf(f, "\nDUMP DEPENDENCY DUMP: == SAME_LOCATION, != NOT_ALIASED, ? POSSIBLY ALIASED\n");
    fprintf(f, "LNO dependency edges are <id: distance is_must direction>\n");

    for(p= head; p; p= DUMPDEP_next(p))
    {
	WN		*node= DUMPDEP_node(p);

	if (OPCODE_is_load(WN_opcode(node)))
	    continue;
	if (OPCODE_is_store(WN_opcode(node)))
	{
	    fprintf(f, "STORE[%d] ", DUMPDEP_id(p));
	}
	else if (WN_operator(node) == OPR_PARM)
	{
	    fprintf(f, "PARM[%d] ", DUMPDEP_id(p));
	}
	if (Valid_alias(alias, node))
	{
	    fprintf(f, "\t== {");
	    for(q= head; q; q= DUMPDEP_next(q))
	    {
		WN *wn =	DUMPDEP_node(q);
		if (Valid_alias(alias, wn) &&
		    SAME_LOCATION == Aliased(alias, node, wn))
		    fprintf(f, "%d,", DUMPDEP_id(q));
	    }
	    fprintf(f, "}\n\t\t!= {");

	    for(q= head; q; q= DUMPDEP_next(q))
	    {
		WN *wn =	DUMPDEP_node(q);
		if (Valid_alias(alias, wn) &&
		    NOT_ALIASED == Aliased(alias, node, wn))
		    fprintf(f, "%d,", DUMPDEP_id(q));
	    }

	    fprintf(f, "}\n\t\t? {");
	    for(q= head; q; q= DUMPDEP_next(q))
	    {
		WN *wn =	DUMPDEP_node(q);
		if (Valid_alias(alias, wn) &&
		    POSSIBLY_ALIASED == Aliased(alias, node, wn))
		    fprintf(f, "%d,", DUMPDEP_id(q));
	    }
	    fprintf(f, "}\n");
	}

	/*
	*  lno dependence information
	*/
	fprintf(f, "\t\t== LNO {");
	for(q= head; q; q= DUMPDEP_next(q))
	{
	    mUINT16		dist;
	    DIRECTION	dir;
	    BOOL		is_must, ok;


	    if (LnoDependenceEdge( node, DUMPDEP_node(q), &dist, &dir, &is_must, &ok))
	    {
		fprintf(f, "<%d: %d, %s",
			DUMPDEP_id(q), dist, is_must ? "MUST " : "");
		DIRECTION_Print(dir, f);
		fprintf(f, ">, ");
	    }
	}
	fprintf(f, "}\n");
    }
}
#endif /* BACK_END */

/*
 *  Write an WN * with OPC_FUNC_ENTRY.
 */
extern void IR_put_func(WN * wn, FILE *f)
{
  FILE *save;
  if (f) {
      save = ir_ofile;
      ir_ofile = f;
  }
  ir_put_stmt(wn, 0);
  if (f) {
      ir_ofile = save;
  }
}


/*========================================================================
 
      Dumping the WN structure.
  
 ========================================================================*/

/*
 *  Debugging:  dump an wn to stdout.
 */
extern void dump_wn(WN *wn)
{
  FILE *save;
  /* building hashtable is expensive, so may not initialize until dumping */
  if (!is_initialized) IR_reader_init();
  save = ir_ofile;
  ir_ofile = stdout; 
  IR_Dwarf_Gen_File_Table(TRUE/*dump_filenames*/); /* read source pathnames */
  ir_put_wn(wn, 0);
  ir_ofile = save;
}

extern void fdump_wn(FILE *fp, WN *wn)
{
  FILE *save;
  /* building hashtable is expensive, so may not initialize until dumping */
  if (!is_initialized) IR_reader_init();
  save = ir_ofile;
  ir_ofile = fp;
  IR_Dwarf_Gen_File_Table(TRUE/*dump_filenames*/); /* read source pathnames */
  ir_put_wn(wn, 0);
  ir_ofile = save;
}

extern void dump_tree(WN *wn)
{
   fdump_tree(stdout,wn);
}

#ifdef BACK_END
extern void dump_dep_tree(WN *wn, struct ALIAS_MANAGER *alias)
{
   fdump_dep_tree(stdout, wn, alias);
}
#endif /* BACK_END */

extern void dump_region_tree(WN *wn)
{
   BOOL	save = IR_dump_region;
   IR_dump_region = TRUE;
   fdump_tree(stdout, wn);
   IR_dump_region = save;
}

extern void dump_wn_no_st(WN *wn)
{
  BOOL save_follow_st;
  if (!is_initialized) IR_reader_init();
  save_follow_st = follow_st;
  follow_st = FALSE;
  dump_wn(wn);
  follow_st = save_follow_st;
}

extern void fdump_wn_no_st(FILE *fp, WN *wn)
{
  BOOL save_follow_st;
  FILE *save = ir_ofile;
  if (!is_initialized) IR_reader_init();
  save_follow_st = follow_st;
  ir_ofile = fp;
  follow_st = FALSE;
  IR_Dwarf_Gen_File_Table(TRUE/*dump_filenames*/); /* read source pathnames */
  ir_put_wn(wn, 0);
  follow_st = save_follow_st;
  ir_ofile = save;
}

extern void dump_tree_no_st(WN *wn)
{
  BOOL save_follow_st;
  if (!is_initialized) IR_reader_init();
  save_follow_st = follow_st;
  follow_st = FALSE;
  dump_tree(wn);
  follow_st = save_follow_st;
}

extern void fdump_tree(FILE *f, WN *wn)
{
  FILE *save;

  /* building hashtable is expensive, so may not initialize until dumping */
  if (!is_initialized) IR_reader_init();
  save = ir_ofile;
  ir_ofile = f;
  IR_Dwarf_Gen_File_Table(TRUE/*dump_filenames*/); /* read source pathnames */
  if (!wn) {
     fprintf(ir_ofile, "<null whirl tree>\n");
  } else if (OPCODE_is_stmt(WN_opcode(wn)) || OPCODE_is_scf(WN_opcode(wn)))
    ir_put_stmt(wn, 0);
  else if (OPCODE_is_expression(WN_opcode(wn)))
    ir_put_expr(wn, 0);
  else if (WN_opcode(wn) == OPC_FUNC_ENTRY)
    IR_put_func(wn, NULL);
  else
    fprintf(ir_ofile, "unknown opcode in (WN *) %p\n", wn);
  ir_ofile = save;
}


extern void fdump_tree_no_st( FILE *f, WN *wn )
{
  BOOL save_follow_st;
  if (!is_initialized) IR_reader_init();
  save_follow_st = follow_st;
  follow_st = FALSE;
  fdump_tree( f, wn );
  follow_st = save_follow_st;
}

extern void fdump_tree_with_alias( FILE *f, const WN *wn, WN_MAP map, const struct ALIAS_MANAGER *am)
{
  WN_MAP save = IR_alias_map;
  const struct ALIAS_MANAGER *save_am = am;
  IR_alias_map = map;
  IR_alias_mgr = am;
  fdump_tree( f, (WN *)wn );
  IR_alias_map = save;
  IR_alias_mgr = save_am;
}

extern void enable_tree_freq_display(void)
{
  IR_freq_map = WN_MAP_FEEDBACK;
}

extern void disable_tree_freq_display(void)
{
  IR_freq_map = WN_MAP_UNDEFINED;
}

extern void fdump_tree_with_freq( FILE *f, const WN *wn, WN_MAP map)
{
  WN_MAP save = IR_freq_map;
  IR_freq_map = map;
  fdump_tree( f, (WN *)wn );
  IR_freq_map = save;
}

extern void fdump_region_tree(FILE *f, WN *wn)
{
   BOOL	save = IR_dump_region;
   IR_dump_region = TRUE;
   fdump_tree(f, wn);
   IR_dump_region = save;
}
// ======================================================================
//   WN_TREE_put_stmt, WN_TREE_put_expr, WN_TREE_put_func
//   (akin to ir_put_stmt, ir_put_expr, IR_put_func in ir_reader.c)
// ======================================================================

// Expressions are printed in POSTFIX order unless dump_parent_before_children
// is set in which case they are printed in prefix order.
// WN_TREE_put_expr is akin to ir_put_expr

static void WN_TREE_put_expr(WN * wn, INT indent) {
  WN * wn2;
  WN * parent_wn;

  Is_True(OPCODE_is_expression(WN_opcode(wn)) || OPCODE_is_call(WN_opcode(wn)),
          ("WN_TREE_put_expr invoked with non-expression, opcode = %s",
           OPCODE_name(WN_opcode(wn))));

  // See if the parent op should be dumped before the children  (PRE_ORDER)
  if (dump_parent_before_children) {
    // create PRE_ORDER tree iterator; use stack of default size

    WN_TREE_ITER<PRE_ORDER> tree_iter(wn);

#ifndef __GNU_BUG_WORKAROUND
    while (tree_iter != LAST_PRE_ORDER_ITER) {
#else
    while (tree_iter != WN_TREE_ITER<PRE_ORDER, WN*>()) {
#endif
      wn2 = tree_iter.Wn();

      if (OPCODE_is_expression(WN_opcode(wn2)) ||
          OPCODE_is_call(WN_opcode(wn2)))
        ir_put_wn(wn2,indent+tree_iter.Depth());

      else if (WN_operator(wn2) == OPR_BLOCK) {
        // block under an expression had better be due to comma/rcomma
        parent_wn = tree_iter.Get_parent_wn();
        Is_True(parent_wn && 
                ((WN_operator(parent_wn) == OPR_COMMA) ||
                 (WN_operator(parent_wn) == OPR_RCOMMA)),
                ("block under expression not a part of comma/rcomma"));

        WN_TREE_put_stmt(wn2, indent + tree_iter.Depth()); 
        tree_iter.Unwind();// have already traversed down the tree 
                           //  skip going down this tree again
      }

      else
        fprintf(ir_ofile, "%*sopcode %d not an expression\n", 
                indent+1,"", WN_opcode(wn2));
      ++tree_iter;
    } // while
  } // if dump_parent_before_children
  else {
    // post order traversal of expressions falls back on ir_put_expr
    // this is so because expressions can contain statements (through comma)
    // and in post order, we need to stop going down at statement boundaries 
    // which is not currently possible with the generic post_order tree
    // iterator
    ir_put_expr(wn,indent);
  } 
}

// ======================================================================
// Statements are printed in PREFIX order or if dump_parent_before_children
// is set 
// WN_TREE_put_stmt is akin to ir_put_stmt (it is also recursive)
// ======================================================================

static void WN_TREE_put_stmt(WN * wn, INT indent)
{
  INT i;
  WN * wn2;
  USRCPOS srcpos;

  Is_True(wn != 0, ("WN_TREE_put_stmt called with null whirl tree"));

  OPCODE opc = WN_opcode(wn);
  Is_True(OPCODE_is_scf(opc) || OPCODE_is_stmt(opc),
          ("WN_TREE_put_stmt invoked on non statement: opcode = %s",
           OPCODE_name(opc)));

  if (wn) {
    BOOL already_dumped_wn = FALSE;
    USRCPOS_srcpos(srcpos) = WN_Get_Linenum(wn);
    if (USRCPOS_srcpos(srcpos) != 0 && 
	USRCPOS_srcpos(srcpos) != USRCPOS_srcpos(last_srcpos)) {
      last_srcpos = srcpos;
#ifdef FRONT_END
      fprintf(ir_ofile, "%*sLOC %d %d\n", indent, "",
		USRCPOS_filenum(srcpos), USRCPOS_linenum(srcpos));
#else
      print_source(USRCPOS_srcpos(srcpos));
#endif
    }

    if (OPCODE_is_scf(opc) || dump_parent_before_children) {
      ir_put_wn(wn, indent);
      already_dumped_wn = TRUE;
    }

    // create an iterator to go through the statements in a block;
    WN_TREE_ITER<PRE_ORDER> tree_iter(wn);

    switch (opc) {

    case OPC_BLOCK:
      if (WN_first(wn)) {
          ++tree_iter; // get to the first kid of the block
#ifndef __GNU_BUG_WORKAROUND
        while (tree_iter != LAST_PRE_ORDER_ITER) {
#else
        while (tree_iter != WN_TREE_ITER<PRE_ORDER, WN*>()) {
#endif
          wn2 = tree_iter.Wn();
          WN_TREE_put_stmt(wn2, indent);// traverse the tree under wn2
          tree_iter.Unwind(); // have already traversed the tree under wn2
                              // skip going down the tree again
        }
      }
      ir_put_marker("END_BLOCK", indent);
      break;

    case OPC_REGION:
      ir_put_marker("REGION EXITS", indent);
      WN_TREE_put_stmt(WN_region_exits(wn), indent+1);

      ir_put_marker("REGION PRAGMAS", indent);
      WN_TREE_put_stmt(WN_region_pragmas(wn), indent+1);

      ir_put_marker("REGION BODY", indent);
      WN_TREE_put_stmt(WN_region_body(wn), indent+1);

      /* check to make sure cg.so is loaded first */
      /* IR_dump_region will be NULL if it is not */
#ifdef BACK_END
      if (IR_dump_region)
	CG_Dump_Region(ir_ofile, wn);
#endif /* BACK_END */
      { char str[20];
	sprintf(str,"END_REGION %d", WN_region_id(wn));
	ir_put_marker(str, indent);
      }
      break;
      
    case OPC_LABEL:
      ir_put_wn(wn, indent);
      if ( WN_label_loop_info(wn) != NULL ) {
	WN_TREE_put_stmt(WN_label_loop_info(wn), indent+1);
      }
      already_dumped_wn = TRUE;
      break;

    case OPC_IF:
      WN_TREE_put_expr(WN_if_test(wn), indent+1);
      if (WN_then(wn)) {
	ir_put_marker("THEN", indent);
	WN_TREE_put_stmt(WN_then(wn), indent+1);
      }
      if (WN_else(wn)) {
	ir_put_marker("ELSE", indent);
	WN_TREE_put_stmt(WN_else(wn), indent+1);
      }
      ir_put_marker("END_IF", indent);
      break;

    case OPC_DO_LOOP:
      WN_TREE_put_expr(WN_index(wn), indent+1);
      ir_put_marker("INIT",indent);
      WN_TREE_put_stmt(WN_start(wn), indent+1);
      ir_put_marker("COMP", indent);
      WN_TREE_put_expr(WN_end(wn), indent+1);
      ir_put_marker("INCR", indent);
      WN_TREE_put_stmt(WN_step(wn), indent+1);
      /* optional loop_info */
      if ( WN_do_loop_info(wn) != NULL ) {
	WN_TREE_put_stmt(WN_do_loop_info(wn), indent);
      }
      ir_put_marker("BODY", indent);
      WN_TREE_put_stmt(WN_do_body(wn), indent+1);
      break;

    case OPC_LOOP_INFO:
      ir_put_wn(wn, indent);
      if ( WN_loop_induction(wn) != NULL ) {
	WN_TREE_put_expr(WN_loop_induction(wn), indent+1);
      }
      if ( WN_loop_trip(wn) != NULL ) {
	WN_TREE_put_expr(WN_loop_trip(wn), indent+1);
      }
      ir_put_marker("END_LOOP_INFO", indent);
      already_dumped_wn = TRUE;
      break;

    case OPC_COMPGOTO:
      ir_put_wn(wn, indent);
      WN_TREE_put_expr(WN_kid(wn,0), indent+1);
      WN_TREE_put_stmt(WN_kid(wn,1), indent+1);
      if (WN_kid_count(wn) > 2)
	WN_TREE_put_stmt(WN_kid(wn,2), indent+1);
      ir_put_marker("END_COMPGOTO", indent);
      already_dumped_wn = TRUE;
      break;

    case OPC_SWITCH:
      ir_put_wn(wn, indent);
      WN_TREE_put_expr(WN_kid(wn,0), indent+1);
      WN_TREE_put_stmt(WN_kid(wn,1), indent+1);
      if (WN_kid_count(wn) > 2)
	WN_TREE_put_stmt(WN_kid(wn,2), indent+1);
      ir_put_marker("END_SWITCH", indent);
      already_dumped_wn = TRUE;
      break;

    case OPC_XGOTO:
      ir_put_wn(wn, indent);
      WN_TREE_put_expr(WN_kid(wn,0), indent+1);
      WN_TREE_put_stmt(WN_kid(wn,1), indent+1);
      ir_put_marker("END_XGOTO", indent);
      already_dumped_wn = TRUE;
      break;

    case OPC_WHERE:
      WN_TREE_put_expr(WN_kid(wn,0), indent+1);
      ir_put_marker("BODY", indent);
      WN_TREE_put_stmt(WN_kid(wn,1), indent+1);
      WN_TREE_put_stmt(WN_kid(wn,2), indent+1);
      break;

    case OPC_EXC_SCOPE_BEGIN:
      {
        INT i;
	for (i = 0; i < WN_kid_count(wn); i++)
	  WN_TREE_put_stmt(WN_kid(wn, i), indent+1);
	break;
      }
    case OPC_EXC_SCOPE_END:
     break;
    default: 
      {
	INT last_is_expr = TRUE;
	OPCODE opc2;
	for (i = 0; i < WN_kid_count(wn); i++) {
	  wn2 = WN_kid(wn,i);
	  if (wn2) {
	    opc2 = WN_opcode(wn2);
	    if (opc2 == 0) {
	       fprintf(ir_ofile, "### error: WN opcode 0\n");
	    } else if (OPCODE_is_expression(opc2)) {
	      WN_TREE_put_expr(wn2, indent+1);
	      last_is_expr = 1;
	    }
	    else if (OPCODE_is_stmt(opc2) || OPCODE_is_scf(opc2)) {
	      if (last_is_expr) {
      		ir_put_marker("BODY", indent);
		WN_TREE_put_stmt(wn2, indent+1);
	      } else
		WN_TREE_put_stmt(WN_kid(wn,i), indent+1);
	      last_is_expr = 0;
	    } else {
	       fprintf(ir_ofile, "### error: unknown opcode type %d\n",opc2);
	    }
	  } else
	    WN_TREE_put_stmt(wn2, indent+1);
	}
      }
    }
    if (!already_dumped_wn)
      ir_put_wn(wn, indent);
  } else
    ir_put_wn(wn, indent);
}

extern void WN_TREE_put_func(WN * wn, FILE *f)
{
  FILE *save;
  if (f) {
      save = ir_ofile;
      ir_ofile = f;
  }
  WN_TREE_put_stmt(wn, 0);
  if (f) {
      ir_ofile = save;
  }
}

extern void WN_TREE_fdump_tree(FILE *f, WN *wn)
{
  FILE *save;

  /* building hashtable is expensive, so may not initialize until dumping */
  if (!is_initialized) IR_reader_init();
  save = ir_ofile;
  ir_ofile = f;
  IR_Dwarf_Gen_File_Table(TRUE/*dump_filenames*/); /* read source pathnames */
  ir_print_filename(TRUE/*dump_filenames*/); /* print LOC 0 0 header */
  if (!wn) {
     fprintf(ir_ofile, "<null whirl tree>\n");
  } else if (OPCODE_is_stmt(WN_opcode(wn)) || OPCODE_is_scf(WN_opcode(wn)))
    WN_TREE_put_stmt(wn, 0);
  else if (OPCODE_is_expression(WN_opcode(wn)))
    WN_TREE_put_expr(wn, 0);
  else if (WN_opcode(wn) == OPC_FUNC_ENTRY)
  WN_TREE_put_func(wn, NULL);
  else
    fprintf(ir_ofile, "unknown opcode in (WN *) %p\n", wn);
  ir_ofile = save;
} 

extern void WN_TREE_dump_tree(WN *wn)
{
   WN_TREE_fdump_tree(stdout,wn);
}


#ifdef BACK_END
extern void fdump_dep_tree( FILE *f, WN *wn, struct ALIAS_MANAGER *alias)
{
  if (alias)
  {
    BOOL   save= IR_dump_map_info;

    L_Save();
    IR_dump_map_info= TRUE;
    IR_DUMPDEP_info = TRUE;
    IR_DUMPDEP_head= NULL;;

    fdump_tree( f, (WN *)wn );
    fdump_DUMPDEP(f, IR_DUMPDEP_head, alias); 


    L_Free();
    IR_DUMPDEP_head= NULL;;
    IR_DUMPDEP_info = FALSE;
    IR_dump_map_info = save;
  }
  else
  {
    fprintf(f, "\talias manager not initialized\n");
  }
}
#endif /* BACK_END */

static void Check_for_IR_Dump0(INT phase, WN *pu, const char *phase_name, BOOL before)
{
    BOOL dump_ir;
    BOOL dump_symtab;
    dump_ir = Get_Trace ( TKIND_IR, phase );
    dump_symtab =  Get_Trace ( TKIND_SYMTAB, phase );
    if (dump_ir || dump_symtab) {
	fprintf(TFile,"\n\n========== Driver dump %s %s ==========\n",
		before == TRUE ? "before" : "after", phase_name);
	if (dump_ir) fdump_tree (TFile,pu);
	if (dump_symtab) {
	    Print_symtab (TFile, GLOBAL_SYMTAB);
	    Print_symtab (TFile, CURRENT_SYMTAB);
	}
    }
}

extern void Check_for_IR_Dump(INT phase, WN *pu, const char *phase_name)
{
    Check_for_IR_Dump0(phase, pu, phase_name, FALSE);
}

extern void Check_for_IR_Dump_Before_Phase(INT phase, WN *pu, const char *phase_name)
{
    Check_for_IR_Dump0(phase, pu, phase_name, TRUE);
}


// The following routines convert an ir data structure
// into a string.  We use helper routines to build the 
// string using stringstream.  Then the string is copied
// into a memory pool.  
//
//  ir_put_wn  -> help_image_wn(stringstream &ss, WN *, INT indent);
//  ir_put_expr -> help_image_expr(stringstream &ss, WN *wn, INT indent);
//  ir_put_stmt -> help_image_stmt(stringstream &ss, WN *wn, INT indent);
//  ir_put_WN_TREE_expr -> help_WN_TREE_image_expr(stringstream &ss, WN *wn, INT indent);
//  ir_put_WN_TREE_stmr -> help_WN_TREE_image_stmt(stringstream &ss, WN *wn, INT indent);
//  ir_put_wn  -> char *image_wn(MEM_POOL *p, WN *wn);


// foward declarations. 

static  void image_marker(stringstream &ss, const char *str, INT indent); 
char *image_st(MEM_POOL *pool, ST_IDX st_idx);
char *image_ty(MEM_POOL *pool, ST_IDX st_idx);
char *image_wn(MEM_POOL *pool, WN *wn);
char *image_stmt(MEM_POOL *pool, WN *wn);
char *image_WN_TREE_stmt(MEM_POOL *pool, WN *wn);
char *image_expr(MEM_POOL *pool, WN *wn);
char *image_WN_TREE_expr(MEM_POOL *pool, WN *wn);
char *image_lineno(MEM_POOL *pool, WN *wn);

void help_image_lineno(stringstream &ss, WN *wn);
void help_image_st (stringstream &ss, ST_IDX st_idx);
void help_image_ty(stringstream &ss, TY_IDX ty);
void help_image_expr(stringstream &ss, WN * wn, INT indent);
void help_image_wn(stringstream &ss, WN *wn, INT indent);
void help_image_stmt(stringstream &ss, WN * wn, INT indent);
void help_WN_TREE_image_stmt(stringstream &ss, WN *wn, int indent);
void help_WN_TREE_image_expr(stringstream &ss, WN * wn, INT indent);


/*
 *  Write out a marker at the indentation level.
 */
static 
void image_marker(stringstream &ss, const char *str, INT indent)
{
  int i;
  for (i = 0; i < indent; i++)
     ss << ' ';
  ss << str; 
}

char *
image_st(MEM_POOL *pool, ST_IDX st_idx)
{ 
  stringstream ss; 
  help_image_st(ss, st_idx);
  char *str  = (char *) MEM_POOL_Alloc(pool, strlen(ss.str().data()) + 1);
  
  // copy string into mempool.
  strcpy(str, ss.str().data()); 
  return str; 
}


char *
image_ty(MEM_POOL *pool, TY_IDX ty_idx)
{ 
  stringstream ss; 
  help_image_ty(ss, ty_idx);
  char *str  = (char *) MEM_POOL_Alloc(pool, strlen(ss.str().data()) + 1);
  
  // copy string into mempool.
  strcpy(str, ss.str().data()); 
  return str; 
}


void
help_image_st (stringstream &ss, ST_IDX st_idx)
{
  char *name;
  char *p;
  
  if (st_idx == (ST_IDX) 0) {
    /* labels may have empty st */
    ss << " <null-st>";

  } else if (!follow_st) {
    /* Do not follow ST *, so that it can dump useful information
       even when ST * is not valid */
    ss << " <st ";
    ss << (INT32) st_idx;
    ss << ">";

  } else {
    const ST* st = &St_Table[st_idx];
    if (ST_class(st) == CLASS_CONST) {
      name = Targ_Print(NULL, STC_val(st));
      /* new lines and spaces in constant strings 
       * will mess up the ascii reader,
       * so replace with underlines */
      for (p = name; *p != '\0'; p++)
	switch (*p) {
	case ' ':
	case '\t':
	case '\n':
	  *p = '_';
	}
    } else
    { 
      name = ST_name(st);
      ss << name; 
    }
  }
}

void
help_image_ty(stringstream &ss, TY_IDX ty)
{
   ss << " T<";
   ss << TY_id(ty);
   ss << ",";
   ss << TY_name(ty);
   ss << ",";
   ss << TY_align(ty);
   
   if (TY_is_restrict(ty)) 
      ss << ",R";
   
   if (TY_is_volatile(ty)) 
      ss << ",V";
   
   if (TY_is_const(ty)) 
      ss << ",C";
   
   ss << ">";
}

char *
image_lineno(MEM_POOL *pool, WN *wn)
{ 
  stringstream ss; 
  help_image_lineno(ss, wn);
  char *str  = (char *) MEM_POOL_Alloc(pool, strlen(ss.str().data()) + 1);
  Is_True(str != NULL, ("Allocation failed"));
  // copy string into mempool.
  strcpy(str, ss.str().data()); 
  return str; 
}

void
help_image_lineno(stringstream &ss, WN *wn)
{ 
  USRCPOS srcpos;
  USRCPOS_srcpos(srcpos) = WN_Get_Linenum(wn);
  if (USRCPOS_srcpos(srcpos) != 0)
  {
    ss << USRCPOS_linenum(srcpos);
  } 
} 

char *
image_wn(MEM_POOL *pool, WN *wn)
{ 
  stringstream ss; 
  help_image_wn(ss, wn, 0);
  char *str  = (char *) MEM_POOL_Alloc(pool, strlen(ss.str().data()) + 1);
  Is_True(str != NULL, ("Allocation failed"));
  // copy string into mempool.
  strcpy(str, ss.str().data()); 
  return str; 
}

char *
image_stmt(MEM_POOL *pool, WN *wn)
{ 
  stringstream ss; 
  help_image_stmt(ss, wn, 0);
  char *str  = (char *) MEM_POOL_Alloc(pool, strlen(ss.str().data()) + 1);
  Is_True(str != NULL, ("Allocation failed"));
  // copy string into mempool.
  strcpy(str, ss.str().data()); 
  return str; 
}





char *
image_WN_TREE_stmt(MEM_POOL *pool, WN *wn)
{ 
  stringstream ss; 
  help_WN_TREE_image_stmt(ss, wn, 0);
  char *str  = (char *) MEM_POOL_Alloc(pool, strlen(ss.str().data()) + 1);
  Is_True(str != NULL, ("Allocation failed"));
  // copy string into mempool.
  strcpy(str, ss.str().data()); 
  return str; 
}


char *
image_expr(MEM_POOL *pool, WN *wn)
{ 
  stringstream ss; 
  help_image_expr(ss, wn, 0);
  char *str  = (char *) MEM_POOL_Alloc(pool, strlen(ss.str().data()) + 1);
  Is_True(str != NULL, ("Allocation failed"));  
  // copy string into mempool.
  strcpy(str, ss.str().data()); 
  return str; 
}


char *
image_WN_TREE_expr(MEM_POOL *pool, WN *wn)
{ 
  stringstream ss; 
  help_WN_TREE_image_expr(ss, wn, 0);
  char *str  = (char *) MEM_POOL_Alloc(pool, strlen(ss.str().data()) + 1);
  Is_True(str != NULL, ("Allocation failed"));
  // copy string into mempool.
  strcpy(str, ss.str().data()); 
  return str; 
}


/*
 *  Write an expression and its children in postfix order.
 *  If dump_parent_before_children, write it in prefix order instead.
 */

void
help_image_expr(stringstream &ss, WN * wn, INT indent)
{
  INT i;
  WN * wn2;

  /* See if the parent op should be dumped before or after the children */
  if (dump_parent_before_children) {
     help_image_wn(ss,wn,indent);
  }
  for (i = 0; i < WN_kid_count(wn); i++) {
    wn2 = WN_kid(wn,i);
    if (wn2) {
      OPCODE op = WN_opcode(wn2);
      if ((OPCODE_FIRST <= op && op <= OPCODE_LAST) &&
	  (OPCODE_is_expression(op) || OPCODE_is_call(op)))
	      help_image_expr(ss, WN_kid(wn,i), indent+1);
      else
      if (    op == OPC_BLOCK
           && (    (WN_operator(wn) == OPR_RCOMMA && i == 1)
                || (WN_operator(wn) == OPR_COMMA && i == 0)))
        help_image_stmt(ss, wn2, indent+1);
      else
      { 
        int i; 
        for (i = 0; i < indent+1; i++) ss << ' ';
	ss << "opcode ";
        ss << op; 
        ss << " not an expression\n";
      } 
    } else  
    { 
        int i; 
        for (i = 0; i < indent+1; i++) ss << ' ';
        ss << "null-expression\n";
    } 
  }
  if (!dump_parent_before_children) {
     help_image_wn(ss, wn, indent);
  }
}


/*
 *  Write an WN * in ascii form on an individual line.
 */ 
void
help_image_wn(stringstream &ss, WN *wn, INT indent)
{ 
    OPCODE opcode;

    if (wn == NULL) {
    	/* null statement */
        ss << "### error: null WN pointer\n";
   	return;
    } else {
	if (IR_dump_wn_addr) {
	    char buff[32];
            sprintf(buff, "0x%8p: ", wn);
            ss << buff;
	}
	opcode = WN_opcode(wn);
    }
    if (opcode == 0) {
        ss << "### error: WN opcode 0\n";
	return;
    }

    if (indent > 0)
    { 
      image_marker(ss, "", indent);
    } 
    /*
     *  for dependency dumping, dump a handle to refer to later
     */
    if (IR_DUMPDEP_info) {
	INT32 handle=  0;
	if (OPCODE_has_alias_info(WN_opcode(wn)) && WN_map_id(wn) != -1) {
	    handle= AddToDUMPDEP(wn);
	}
	char buff[32];
        sprintf(buff, "[%5d]", handle);
        ss << buff;
    }
    if (indent > 0 && opcode == OPC_LABEL)
	image_marker(ss, "", indent-1);
    else
        image_marker(ss,"",indent);


    ss << OPCODE_name(opcode) + strlen("OPC_");

    if (OPCODE_has_offset(opcode)) {
	if (OPCODE_operator(opcode) == OPR_PRAGMA || 
	    OPCODE_operator(opcode) == OPR_XPRAGMA)
	{
	    ss << " ";
	    ss << WN_pragma_flags(wn);
            ss << " ";
            ss << WN_pragma(wn);
	}
	else
	{
	  ss << " ";
          ss << WN_offset(wn);
	}
    } else if (OPCODE_has_2offsets(opcode)) {
      ss << " ";
      ss << WN_loop_trip_est(wn);
      ss << " ";
      ss << WN_loop_depth(wn);
    }

    switch (OPCODE_operator(opcode)) {

    case OPR_INTRINSIC_OP:
    case OPR_ARRAYEXP:
#ifdef KEY
    case OPR_PURE_CALL_OP:
#endif
      {
	ss << " ";
        ss << WN_kid_count(wn);
        break;
      }

    case OPR_REGION:
	ss << " ";
        ss << WN_region_id(wn);

#ifdef BACK_END
      {
	RID *rid = REGION_get_rid(wn);
	if (rid != NULL)
        {
	  ss << " ";
          ss << RID_id(rid);
        }
      }
#endif /* BACK_END */
      ss << "(kind=";
      ss << WN_region_kind(wn);
      ss << ")";
      break;
#if defined(TARG_SL)
    case OPR_LDA:
    case OPR_ISTORE:
      ss << "im:";
      ss << WN_is_internal_mem_ofst(wn);
      break; 
#endif
    case OPR_LDBITS:
    case OPR_ILDBITS:
    case OPR_STBITS:
    case OPR_ISTBITS:
    case OPR_EXTRACT_BITS:
    case OPR_COMPOSE_BITS:
      ss << " <bofst:";
      ss << WN_bit_offset(wn);
      ss << " bsize:";
      ss << WN_bit_size(wn);
      ss << ">";
      break;

    case OPR_ASM_INPUT:
      ss << " opnd:";
      ss << WN_asm_opnd_num(wn);
      break;

    default:
      break;
    }

    if (OPCODE_has_inumber(opcode)) {
	switch (opcode) {
	case OPC_IO:
	  ss << " <";
          ss << WN_intrinsic(wn);
          ss << ",";
          ss << IOSTATEMENT_name((IOSTATEMENT) WN_intrinsic(wn));
          ss << ","; 
          ss <<     get_iolibrary_name(WN_IO_Library(wn)); 
          ss << ">";
          break;
	case OPC_IO_ITEM:
	  ss << " <";
          ss << WN_intrinsic(wn);
          ss << ",";
          ss << IOITEM_name((IOITEM) WN_intrinsic(wn));
          ss << ">";
          break;
	default:		/* intrinsic */
	    Is_True(OPCODE_operator(opcode) == OPR_INTRINSIC_OP ||
		    OPCODE_operator(opcode) == OPR_INTRINSIC_CALL,
		    ("ir_put_wn, expected an intrinsic"));
#if defined(BACK_END) || defined(IR_TOOLS) || defined(TARG_NVISA)
	    ss << " <";
            ss << WN_intrinsic(wn);
            ss << "," ;
            ss << INTRINSIC_name((INTRINSIC) WN_intrinsic(wn));
            ss << ">";
		
#endif
	    break;
	}
    }

    if (OPCODE_has_bits(opcode))
    {
      ss << " ";
      ss << WN_cvtl_bits(wn);
    }
    if (OPCODE_has_label(opcode))
    {
      ss << " L";
      ss << WN_label_number(wn);
    }
    if (OPCODE_has_flags(opcode)) 
    {
      ss << " ";
      ss << WN_flag(wn);
    }
    if (OPCODE_has_sym(opcode)) {
	help_image_st (ss,WN_st_idx(wn));
    }

    if (OPCODE_has_1ty(opcode)) {
	if (WN_ty(wn) != (TY_IDX) 0) 
	   help_image_ty(ss, WN_ty(wn));
	else
	    if (opcode != OPC_IO_ITEM)
	      ss << " T<### ERROR: null ptr>";
    } else if (OPCODE_has_2ty(opcode)) {
	if (WN_ty(wn) != (TY_IDX) 0) 
	   help_image_ty(ss,WN_ty(wn));
	else
	  ss << " T<### ERROR: null ptr>";
	if (WN_load_addr_ty(wn) != (TY_IDX) 0) 
	   help_image_ty(ss, WN_load_addr_ty(wn));
	else
	  ss << " T<### ERROR: null ptr>";
    }

    if (OPCODE_has_ndim(opcode))
    { 
      ss << " ";
      ss << WN_num_dim(wn);
    }
    if (OPCODE_has_esize(opcode))
    {
        ss << " ";
        ss << WN_element_size(wn);
    }
    if (OPCODE_has_num_entries(opcode))
    {
      ss << " ";
      ss << WN_num_entries(wn);
    }
    if (OPCODE_has_last_label(opcode))
    { 
      ss << " ";
      ss << WN_last_label(wn);
    }

    if (OPCODE_has_value(opcode)) {
      ss << " ";
      ss << WN_const_val(wn);
      /* Also print the hex value for INTCONSTs */
      if (OPCODE_operator(opcode) == OPR_INTCONST || opcode == OPC_PRAGMA) {
	ss << " (0x";
        ss << hex << WN_const_val(wn);
      }
    }

    if (OPCODE_has_field_id(opcode) && WN_field_id(wn)) {
      ss << " <field_id:";
      ss << WN_field_id(wn);
      ss << ">";
    }
    
    if (OPCODE_has_ereg_supp(opcode)) {
	INITO_IDX ino = WN_ereg_supp(wn);
	if (ino != 0)
	  {
	    ss << " INITO<";
            ss << INITO_IDX_index (ino); 
            ss << ",";
            ss << INITO_IDX_index (ino);
            ss << ">";
	  }
    }
    if (opcode == OPC_COMMENT) {
      ss << " # ";
      ss << Index_To_Str(WN_offset(wn));
    }

    if (follow_st && OPCODE_has_sym(opcode) && OPCODE_has_offset(opcode)
	&& WN_st_idx(wn) != (ST_IDX) 0 && (ST_class(WN_st(wn)) == CLASS_PREG)
        && opcode != OPC_PRAGMA)
	{
	    if (Preg_Is_Dedicated(WN_offset(wn))) {
		if (Preg_Offset_Is_Int(WN_offset(wn))) {
		  ss << " # $r";
                  ss << WN_offset(wn);
		}
		else if (Preg_Offset_Is_Float(WN_offset(wn))) {
		  ss << " # $f";
                  ss << WN_offset(wn) - Float_Preg_Min_Offset;
		}
#ifdef TARG_X8664
		else if (Preg_Offset_Is_X87(WN_offset(wn))) {
		  ss << " # $st";
                  ss << WN_offset(wn) - X87_Preg_Min_Offset;
		}
#endif
	    }
	    else { 
	    	/* reference to a non-dedicated preg */
		if ((WN_offset(wn) - Last_Dedicated_Preg_Offset) 
			< PREG_Table_Size (CURRENT_SYMTAB) )
	    	{ 
                  ss << " # ";
                  ss << Preg_Name(WN_offset(wn));
		}
		else
		{ 
		  ss << " # <Invalid PREG Table index (";
                  ss << WN_offset(wn);
                  ss << ")>";
		}
	    }
	}

    if (opcode == OPC_XPRAGMA) {
      ss << " # ";
      ss << WN_pragmas[WN_pragma(wn)].name;
    }

    if (OPCODE_operator(opcode) == OPR_ASM_INPUT) {
      ss << " # \"";
      ss << WN_asm_input_constraint(wn);
      ss << "\"";
    }

    if (opcode == OPC_PRAGMA) {
        ss << " # ";
        ss << WN_pragmas[WN_pragma(wn)].name;
	switch(WN_pragma(wn)) {
	case WN_PRAGMA_DISTRIBUTE:
	case WN_PRAGMA_REDISTRIBUTE:
	case WN_PRAGMA_DISTRIBUTE_RESHAPE:
	  ss << ", ";
          ss << WN_pragma_index(wn);
          switch(WN_pragma_distr_type(wn)) {
	    case DISTRIBUTE_STAR:
	      ss << ", *";
              break;
	    case DISTRIBUTE_BLOCK:
	      ss << ", BLOCK";
		break;
	    case DISTRIBUTE_CYCLIC_CONST:
	      ss << ", CYCLIC(";
              ss << WN_pragma_preg(wn);
              ss << ")";
		break;
	    case DISTRIBUTE_CYCLIC_EXPR:
	      ss << ", CYCLIC(expr)";
		break;
	    }
	    break;
	case WN_PRAGMA_ASM_CONSTRAINT:
	  ss << ", \"";
          ss <<   WN_pragma_asm_constraint(wn);
          ss << "\", opnd:";
          ss << 		  WN_pragma_asm_opnd_num(wn);
          ss << " preg:" ;
          ss <<   WN_pragma_asm_copyout_preg(wn);
	  break;
	default:
            if (WN_pragma_arg2(wn) != 0 ) 
	    { 
	      ss << ", ";
	      ss << WN_pragma_arg1(wn), WN_pragma_arg2(wn);
              ss << ", ";
              ss << WN_pragma_arg2(wn);
	    }
            else
		if (WN_pragma_arg1(wn) != 0 )
		  { 
		    ss << ", ";
                    ss << WN_pragma_arg1(wn);
		  }
	    break;
	} /* switch */
    }

    if (OPCODE_operator(opcode) == OPR_ASM_STMT) {
      ss << " # \"";
      ss << WN_asm_string(wn); 
      ss << "\"";
      if (WN_Asm_Volatile(wn))
      	ss << " (volatile)";
      if (WN_Asm_Clobbers_Mem(wn))
      	ss << " (memory)";
      if (WN_Asm_Clobbers_Cc(wn))
      	ss << " (cc)";
    }

    if (OPCODE_is_call(opcode))
    {
      ss << " # flags 0x";
      ss << hex << WN_call_flag(wn);
    }

    if (OPCODE_operator(opcode) == OPR_PARM) {
	INT flag =  WN_flag(wn);
	ss << " # ";
	if (flag & WN_PARM_BY_REFERENCE) 
	  ss << " by_reference ";
#if defined(TARG_SL)
	if (flag & WN_PARM_DEREFERENCE)  
          ss << " by_dereference ";
#endif
	if (flag & WN_PARM_BY_VALUE)     
	  ss << " by_value ";
	if (flag & WN_PARM_OUT)          
          ss << " out ";
	if (flag & WN_PARM_DUMMY)        
          ss << " dummy ";
	if (flag & WN_PARM_READ_ONLY)    
          ss << " read_only ";
	if (flag & WN_PARM_PASSED_NOT_SAVED) 
          ss << "passed_not_saved ";
	if (flag & WN_PARM_NOT_EXPOSED_USE) 
          ss << " not_euse ";
	if (flag & WN_PARM_IS_KILLED) 
          ss << " killed ";
    }

    if (IR_dump_map_info) {
      ss << " # <id ";
      ss << OPCODE_mapcat(opcode);
      ss << ":";
      ss << WN_map_id(wn);
      ss << ">";
		
	if (ir_put_map && ( WN_map_id(wn) != -1 )) {
	    switch ( WN_MAP_Get_Kind( ir_put_map ) ) {
	    case WN_MAP_KIND_VOIDP:
	      ss << " <map ";
              ss << hex << WN_MAP_Get( ir_put_map, wn );
              ss << ">";
		break;
	    case WN_MAP_KIND_INT32:
	      ss << " <map 0x";
              ss << hex << WN_MAP32_Get( ir_put_map, wn );
              ss << ">";
		break;
	    case WN_MAP_KIND_INT64:
	      ss <<  " <map 0x", 
 	      ss << WN_MAP64_Get( ir_put_map, wn );
              ss << ">";
	      break;
	    }
	}
#ifdef BACK_END
	if (UINT16 vertex = LNOGetVertex(wn)) {
	  ss << " <lno vertex ";
          ss << vertex;
          ss << ">";
	}
#endif
    }

    if (OPCODE_is_scf(WN_opcode(wn)) || OPCODE_is_stmt(WN_opcode(wn))) {

	USRCPOS srcpos;
	USRCPOS_srcpos(srcpos) = WN_Get_Linenum(wn);

	ss <<  " {line: ";
        ss << USRCPOS_filenum(srcpos); 
        ss <<  "/";
        ss << USRCPOS_linenum(srcpos); 
        ss << "}"; 
    }

#ifdef BACK_END
    if (IR_dump_alias_info(WN_opcode(wn))) {
      ss << " [alias_id: ";
      ss << WN_MAP32_Get(IR_alias_map, wn);
      ss << (IR_alias_mgr && IR_alias_mgr->Safe_to_speculate(wn) ? ",fixed" : "");
      ss << "]";
	      
    }
#endif

    if (IR_freq_map != WN_MAP_UNDEFINED && (OPCODE_is_scf(WN_opcode(wn)) ||
					    OPCODE_is_stmt(WN_opcode(wn)))) {
	USRCPOS srcpos;
	USRCPOS_srcpos(srcpos) = WN_Get_Linenum(wn);

	ss << " {freq: ";
        ss << WN_MAP32_Get(IR_freq_map, wn);
        ss << ", ln: ";
        ss << USRCPOS_linenum(srcpos);
        ss << ", col: ";
        ss << USRCPOS_column(srcpos);
        ss << "}";
		
    }
    if (Current_Map_Tab != NULL 
    	&& WN_MAP32_Get(WN_MAP_ALIAS_CLASS, wn) != 0) 
    {
      ss << " {class ";
      ss <<      WN_MAP32_Get(WN_MAP_ALIAS_CLASS, wn);
      ss << "}";
    }
    ss <<  "\n";
}


/*
 *  Write a statement WN * and its children in prefix order.
 */
void help_image_stmt(stringstream &ss, WN * wn, INT indent)
{
  INT i;
  WN * wn2;
  USRCPOS srcpos;

  if (wn) { 

    OPCODE opc = WN_opcode(wn);
    BOOL already_dumped_wn = FALSE;
  
    if (OPCODE_is_scf(opc) || dump_parent_before_children) {
      help_image_wn(ss, wn, indent);
      already_dumped_wn = TRUE;
    }

    switch (opc) {

    case OPC_BLOCK:
      wn2 = WN_first(wn);
      while (wn2) {
	help_image_stmt(ss,wn2, indent);
	wn2 = WN_next(wn2);
      }
      image_marker(ss, "END_BLOCK", indent);
      break;

    case OPC_REGION:
      image_marker(ss, "REGION EXITS", indent);
      help_image_stmt(ss, WN_region_exits(wn), indent+1);

      image_marker(ss, "REGION PRAGMAS", indent);
      help_image_stmt(ss, WN_region_pragmas(wn), indent+1);

      image_marker(ss, "REGION BODY", indent);
      help_image_stmt(ss, WN_region_body(wn), indent+1);

      /* check to make sure cg.so is loaded first */
      /* IR_dump_region will be NULL if it is not */
#ifdef BACK_END
      if (IR_dump_region)
	//CG_Dump_Region(ir_ofile, wn);
#endif /* BACK_END */
      { char str[20];
	sprintf(str,"END_REGION %d", WN_region_id(wn));
	image_marker(ss,str, indent);
      }
      break;
      
    case OPC_LABEL:
      help_image_wn(ss,wn, indent);
      if ( WN_label_loop_info(wn) != NULL ) {
	help_image_stmt(ss,WN_label_loop_info(wn), indent+1);
      }
      already_dumped_wn = TRUE;
      break;

    case OPC_IF:
      help_image_expr(ss,WN_if_test(wn), indent+1);
      if (WN_then(wn)) {
	image_marker(ss,"THEN", indent);
	help_image_stmt(ss, WN_then(wn), indent+1);
      }
      if (WN_else(wn)) {
	image_marker(ss,"ELSE", indent);
	help_image_stmt(ss,WN_else(wn), indent+1);
      }
      image_marker(ss,"END_IF", indent);
      break;

    case OPC_DO_LOOP:
      help_image_expr(ss,WN_index(wn), indent+1);
      image_marker(ss,"INIT",indent);
      help_image_stmt(ss,WN_start(wn), indent+1);
      image_marker(ss,"COMP", indent);
      help_image_expr(ss,WN_end(wn), indent+1);
      image_marker(ss,"INCR", indent);
      help_image_stmt(ss,WN_step(wn), indent+1);
      /* optional loop_info */
      if ( WN_do_loop_info(wn) != NULL ) {
	help_image_stmt(ss,WN_do_loop_info(wn), indent);
      }
      image_marker(ss,"BODY", indent);
      help_image_stmt(ss,WN_do_body(wn), indent+1);
      break;

    case OPC_LOOP_INFO:
      help_image_wn(ss,wn, indent);
      if ( WN_loop_induction(wn) != NULL ) {
	help_image_expr(ss,WN_loop_induction(wn), indent+1);
      }
      if ( WN_loop_trip(wn) != NULL ) {
	help_image_expr(ss,WN_loop_trip(wn), indent+1);
      }
      image_marker(ss,"END_LOOP_INFO", indent);
      already_dumped_wn = TRUE;
      break;

    case OPC_COMPGOTO:
      help_image_wn(ss,wn, indent);
      help_image_expr(ss,WN_kid(wn,0), indent+1);
      help_image_stmt(ss,WN_kid(wn,1), indent+1);
      if (WN_kid_count(wn) > 2)
	help_image_stmt(ss,WN_kid(wn,2), indent+1);
      image_marker(ss,"END_COMPGOTO", indent);
      already_dumped_wn = TRUE;
      break;

    case OPC_SWITCH:
      help_image_wn(ss,wn, indent);
      help_image_expr(ss,WN_kid(wn,0), indent+1);
      help_image_stmt(ss,WN_kid(wn,1), indent+1);
      if (WN_kid_count(wn) > 2)
	help_image_stmt(ss,WN_kid(wn,2), indent+1);
      image_marker(ss,"END_SWITCH", indent);
      already_dumped_wn = TRUE;
      break;

    case OPC_XGOTO:
      help_image_wn(ss,wn, indent);
      help_image_expr(ss,WN_kid(wn,0), indent+1);
      help_image_stmt(ss,WN_kid(wn,1), indent+1);
      image_marker(ss,"END_XGOTO", indent);
      already_dumped_wn = TRUE;
      break;

    case OPC_WHERE:
      help_image_expr(ss,WN_kid(wn,0), indent+1);
      image_marker(ss,"BODY", indent);
      help_image_stmt(ss,WN_kid(wn,1), indent+1);
      help_image_stmt(ss,WN_kid(wn,2), indent+1);
      break;

    case OPC_EXC_SCOPE_BEGIN:
      {
        INT i;
	for (i = 0; i < WN_kid_count(wn); i++)
	  help_image_stmt(ss,WN_kid(wn, i), indent+1);
	break;
      }
    case OPC_EXC_SCOPE_END:
     break;

    case OPC_ASM_STMT:
      help_image_wn(ss,wn, indent);
      already_dumped_wn = TRUE;
      help_image_stmt(ss,WN_kid(wn,0), indent+1);
      help_image_stmt(ss,WN_kid(wn,1), indent+1);
      image_marker(ss,"ASM_INPUTS", indent);
      {
	INT i;
	for (i = 2; i < WN_kid_count(wn); i++) {
	  help_image_expr(ss,WN_kid(wn,i), indent+1);
	}
      }
      image_marker(ss,"END_ASM_INPUTS", indent);
      break;

    default: 
      {
	INT last_is_expr = TRUE;
	OPCODE opc2;
	for (i = 0; i < WN_kid_count(wn); i++) {
	  wn2 = WN_kid(wn,i);
	  if (wn2) {
	    opc2 = WN_opcode(wn2);
	    if (opc2 == 0) {
	       fprintf(ir_ofile, "### error: WN opcode 0\n");
	    } else if (OPCODE_is_expression(opc2)) {
	      help_image_expr(ss,wn2, indent+1);
	      last_is_expr = 1;
	    }
	    else if (OPCODE_is_stmt(opc2) || OPCODE_is_scf(opc2)) {
	      if (last_is_expr) {
      		image_marker(ss,"BODY", indent);
		help_image_stmt(ss,wn2, indent+1);
	      } else
		help_image_stmt(ss,WN_kid(wn,i), indent+1);
	      last_is_expr = 0;
	    } else {
	       ss << "### error: unknown opcode type ";
               ss << opc2;
               ss << "\n";
	    }
	  } else
	    help_image_stmt(ss,wn2, indent+1);
	}
      }
    }
    if (!already_dumped_wn)
      help_image_wn(ss,wn, indent);
 } else
      help_image_wn(ss,wn, indent);
}


void
help_WN_TREE_image_stmt(stringstream& ss, WN *wn, int indent)
{
  INT i;
  WN * wn2;
  USRCPOS srcpos;

  Is_True(wn != 0, ("WN_TREE_image_stmt called with null whirl tree"));

  OPCODE opc = WN_opcode(wn);
  Is_True(OPCODE_is_scf(opc) || OPCODE_is_stmt(opc),
          ("WN_TREE_image_stmt invoked on non statement: opcode = %s", OPCODE_name(opc)));

  if (wn)
  { 
    BOOL already_dumped_wn = FALSE;
    if (OPCODE_is_scf(opc) || dump_parent_before_children) {
      help_image_wn(ss, wn, indent);
      already_dumped_wn = TRUE;
    }

    // create an iterator to go through the statements in a block;
    WN_TREE_ITER<PRE_ORDER> tree_iter(wn);

   switch (opc) {

      if (WN_first(wn)) {
          ++tree_iter; // get to the first kid of the block
#ifndef __GNU_BUG_WORKAROUND
        while (tree_iter != LAST_PRE_ORDER_ITER) {
#else
        while (tree_iter != WN_TREE_ITER<PRE_ORDER, WN*>()) {
#endif
          wn2 = tree_iter.Wn();
          help_WN_TREE_image_stmt(ss, wn2, indent);// traverse the tree under wn2
          tree_iter.Unwind(); // have already traversed the tree under wn2
                              // skip going down the tree again
        }
      }
      image_marker(ss,"END_BLOCK", indent);
      break;

    case OPC_REGION:
      image_marker(ss, "REGION EXITS", indent);
      help_WN_TREE_image_stmt(ss,WN_region_exits(wn), indent+1);

      image_marker(ss,"REGION PRAGMAS", indent);
      help_WN_TREE_image_stmt(ss, WN_region_pragmas(wn), indent+1);

      image_marker(ss, "REGION BODY", indent);
      help_WN_TREE_image_stmt(ss, WN_region_body(wn), indent+1);

      /* check to make sure cg.so is loaded first */
      /* IR_dump_region will be NULL if it is not */
#ifdef BACK_END
      if (IR_dump_region)
	CG_Dump_Region(ir_ofile, wn);
#endif /* BACK_END */
      { 
        char str[20];
	sprintf(str,"END_REGION %d", WN_region_id(wn));
        ss << "END_REGION ";
        ss << str; 
      }
      break;
      
    case OPC_LABEL:
      help_image_wn(ss, wn, indent);
      if ( WN_label_loop_info(wn) != NULL ) {
	help_WN_TREE_image_stmt(ss,WN_label_loop_info(wn), indent+1);
      }
      already_dumped_wn = TRUE;
      break;

    case OPC_IF:
      help_WN_TREE_image_expr(ss, WN_if_test(wn), indent+1);
      if (WN_then(wn)) {
	image_marker(ss, "THEN", indent);
	help_WN_TREE_image_stmt(ss,WN_then(wn), indent+1);
      }
      if (WN_else(wn)) {
	image_marker(ss,"ELSE", indent);
	help_WN_TREE_image_stmt(ss, WN_else(wn), indent+1);
      }
      image_marker(ss,"END_IF", indent);
      break;

    case OPC_DO_LOOP:
      help_WN_TREE_image_expr(ss,WN_index(wn), indent+1);
      image_marker(ss, "INIT",indent);
      help_WN_TREE_image_stmt(ss,WN_start(wn), indent+1);
      image_marker(ss,"COMP", indent);
      help_WN_TREE_image_expr(ss,WN_end(wn), indent+1);
      image_marker(ss,"INCR", indent);
      help_WN_TREE_image_stmt(ss,WN_step(wn), indent+1);
      /* optional loop_info */
      if ( WN_do_loop_info(wn) != NULL ) {
	help_WN_TREE_image_stmt(ss,WN_do_loop_info(wn), indent);
      }
      image_marker(ss,"BODY", indent);
      help_WN_TREE_image_stmt(ss,WN_do_body(wn), indent+1);
      break;

    case OPC_LOOP_INFO:
      help_image_wn(ss,wn, indent);
      if ( WN_loop_induction(wn) != NULL ) {
	help_WN_TREE_image_expr(ss,WN_loop_induction(wn), indent+1);
      }
      if ( WN_loop_trip(wn) != NULL ) {
	help_WN_TREE_image_expr(ss,WN_loop_trip(wn), indent+1);
      }
      image_marker(ss,"END_LOOP_INFO", indent);
      already_dumped_wn = TRUE;
      break;

    case OPC_COMPGOTO:
      help_image_wn(ss,wn, indent);
      help_WN_TREE_image_expr(ss,WN_kid(wn,0), indent+1);
      help_WN_TREE_image_stmt(ss,WN_kid(wn,1), indent+1);
      if (WN_kid_count(wn) > 2)
	help_WN_TREE_image_stmt(ss,WN_kid(wn,2), indent+1);
      image_marker(ss,"END_COMPGOTO", indent);
      already_dumped_wn = TRUE;
      break;

    case OPC_SWITCH:
      help_image_wn(ss,wn, indent);
      help_WN_TREE_image_expr(ss,WN_kid(wn,0), indent+1);
      help_WN_TREE_image_stmt(ss,WN_kid(wn,1), indent+1);
      if (WN_kid_count(wn) > 2)
	help_WN_TREE_image_stmt(ss,WN_kid(wn,2), indent+1);
      image_marker(ss,"END_SWITCH", indent);
      already_dumped_wn = TRUE;
      break;

    case OPC_XGOTO:
      help_image_wn(ss,wn, indent);
      help_WN_TREE_image_expr(ss,WN_kid(wn,0), indent+1);
      help_WN_TREE_image_stmt(ss,WN_kid(wn,1), indent+1);
      image_marker(ss,"END_XGOTO", indent);
      already_dumped_wn = TRUE;
      break;

    case OPC_WHERE:
      help_WN_TREE_image_expr(ss,WN_kid(wn,0), indent+1);
      image_marker(ss,"BODY", indent);
      help_WN_TREE_image_stmt(ss,WN_kid(wn,1), indent+1);
      help_WN_TREE_image_stmt(ss,WN_kid(wn,2), indent+1);
      break;

    case OPC_EXC_SCOPE_BEGIN:
      {
        INT i;
	for (i = 0; i < WN_kid_count(wn); i++)
	  help_WN_TREE_image_stmt(ss,WN_kid(wn, i), indent+1);
	break;
      }
    case OPC_EXC_SCOPE_END:
     break;
    default: 
      {
	INT last_is_expr = TRUE;
	OPCODE opc2;
	for (i = 0; i < WN_kid_count(wn); i++) {
	  wn2 = WN_kid(wn,i);
	  if (wn2) {
	    opc2 = WN_opcode(wn2);
	    if (opc2 == 0) {
	      ss << "### error: WN opcode 0\n";
	    } else if (OPCODE_is_expression(opc2)) {
	      help_WN_TREE_image_expr(ss,wn2, indent+1);
	      last_is_expr = 1;
	    }
	    else if (OPCODE_is_stmt(opc2) || OPCODE_is_scf(opc2)) {
	      if (last_is_expr) {
      		image_marker(ss,"BODY", indent);
		help_WN_TREE_image_stmt(ss,wn2, indent+1);
	      } else
		help_WN_TREE_image_stmt(ss,WN_kid(wn,i), indent+1);
	      last_is_expr = 0;
	    } else {
	      ss << "### error: unknown opcode type ";
              ss << opc2;
	    }
	  } else
	    help_WN_TREE_image_stmt(ss,wn2, indent+1);
	}
      }
   }
   if (!already_dumped_wn)
     help_image_wn(ss,wn, indent);
  }
}




void 
help_WN_TREE_image_expr(stringstream &ss, WN * wn, INT indent) {
  WN * wn2;
  WN * parent_wn;

  Is_True(OPCODE_is_expression(WN_opcode(wn)) || OPCODE_is_call(WN_opcode(wn)),
          ("WN_TREE_put_expr invoked with non-expression, opcode = %s",
           OPCODE_name(WN_opcode(wn))));

  // See if the parent op should be dumped before the children  (PRE_ORDER)
  if (dump_parent_before_children) {
    // create PRE_ORDER tree iterator; use stack of default size

    WN_TREE_ITER<PRE_ORDER> tree_iter(wn);

#ifndef __GNU_BUG_WORKAROUND
    while (tree_iter != LAST_PRE_ORDER_ITER) {
#else
    while (tree_iter != WN_TREE_ITER<PRE_ORDER, WN*>()) {
#endif
      wn2 = tree_iter.Wn();

      if (OPCODE_is_expression(WN_opcode(wn2)) ||
          OPCODE_is_call(WN_opcode(wn2)))
        help_image_wn(ss,wn2,indent+tree_iter.Depth());

      else if (WN_operator(wn2) == OPR_BLOCK) {
        // block under an expression had better be due to comma/rcomma
        parent_wn = tree_iter.Get_parent_wn();
        Is_True(parent_wn && 
                ((WN_operator(parent_wn) == OPR_COMMA) ||
                 (WN_operator(parent_wn) == OPR_RCOMMA)),
                ("block under expression not a part of comma/rcomma"));

        help_WN_TREE_image_stmt(ss,wn2, indent + tree_iter.Depth()); 
        tree_iter.Unwind();// have already traversed down the tree 
                           //  skip going down this tree again
      }

      else
        //fprintf(ir_ofile, "%*sopcode %d not an expression\n", 
        //        indent+1,"", WN_opcode(wn2));
      ++tree_iter;
    } // while
  } // if dump_parent_before_children
  else {
    // post order traversal of expressions falls back on ir_put_expr
    // this is so because expressions can contain statements (through comma)
    // and in post order, we need to stop going down at statement boundaries 
    // which is not currently possible with the generic post_order tree
    // iterator
    help_WN_TREE_image_expr(ss,wn,indent);
  } 
}




