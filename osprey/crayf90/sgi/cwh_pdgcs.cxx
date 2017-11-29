/*
 *  Copyright (C) 2006, 2007. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


/* ====================================================================
 * ====================================================================
 *
 * Module: cwh_pdgcs
 * $Revision: 1.9 $
 * $Date: 05/08/26 23:50:11-07:00 $
 * $Author: fchow@fluorspar.internal.keyresearch.com $
 * $Source: 
 *
 * Revision history:
 *  dd-mmm-95 - Original Version
 *
 * Description: contains administrative routines invoked from the Cray 
 *              FE at initialization and termination of the 
 *              compiler and around each program unit. PDGCS_* entry 
 *              points plus fei_* routines which deal with procedure
 *              initialization and cleanup.
 * 
 * ====================================================================
 * ====================================================================
 */
/*REFERENCED*/
static const char *source_file = __FILE__;

#ifdef _KEEP_RCS_ID
/*REFERENCED*/
static char *rcs_id = "$Source: crayf90/sgi/SCCS/s.cwh_pdgcs.cxx $ $Revision: 1.9 $";
#endif /* _KEEP_RCS_ID */


/* sgi includes */

#include "defs.h"
#include "config.h"
#include "config_opt.h"
#include "glob.h"
#include "stab.h"
#include "strtab.h"
#include "wn.h" 
#if defined(BUILD_OS_DARWIN)
#include "darwin_elf.h"
#else /* defined(BUILD_OS_DARWIN) */
#include "elf.h"
#endif /* defined(BUILD_OS_DARWIN) */
#include "pu_info.h"
#include <sys/types.h>
#include "ir_reader.h"
#include "ir_bwrite.h"
#include "file_util.h"
#include "tracing.h"

/* FE includes */

#include "i_cvrt.h"

/* conversion includes */

#include  "cwh_defines.h"
#include  "cwh_addr.h"
#include  "cwh_dst.h"
#include  "cwh_data.h"
#include  "cwh_mkdepend.h"
#include  "cwh_stmt.h"
#include  "cwh_preg.h"
#include  "cwh_stab.h"
#include  "cwh_auxst.h"
#include  "cwh_stk.h"
#include  "cwh_block.h"
/*#include  "cwh_stats.h" */
#include  "sgi_cmd_line.h"

/* OS includes */
#include <sys/stat.h>
#include <errno.h>  
#include <unistd.h>

INT32 cwh_assign_label_id;
static BOOL Generate_IR = TRUE;
static BOOL FE_Write_Binary = TRUE;
static PU_Info *PU_Tree_Root ; /* First PU (not internal) seen */
static PU_Info *PU_Current   ; /* Current PU    */
static PU_Info *PU_Parent    ; /* Parent of PU_current if current is internal */

#define	IRB_FILE_EXTENSION ".B"
#define DSTDUMP_FILE_EXTENSION ".fe.dst" 

static PU_Info * cwh_pdgcs_pu_mem(void)    ;
static void      update_rii_file ( void ) ;
static void      delete_rii_file ( void ) ;

/* Memory pool for WHIRL maps */
static MEM_POOL map_mem_pool_s;  /* Place to put auxilliary data structures */
static MEM_POOL *map_mem_pool = &map_mem_pool_s;

/*===============================================
 *
 * PDGCS_initialize
 *
 * Initialize data structures required for the 
 * translation at the start of compilation.
 * 
 * Configuration and initialization of memory,
 * error handling etc, has been done in 
 * sgi_cmd_line.
 *
 *===============================================
 */ 

/*ARGSUSED*/
extern void
PDGCS_initialize(LANG   language_code,
		 INT32  flags,
		 char   *cmplr_name,
		 char   *cmplr_rel,
		 char	*code_file_name,
		 char	*list_file_name,
		 INT32  trunc_bits,
		 INT32  debug_opts,
		 char   *src_path_name,
		 char   *cif_file_name,
		 char   *debug_file_name,
                 FILE   *debug_file, 
                 FILE   *cif_file,
		 char   *src_fname,
		 char   *cwd,
		 INT32  n_pes,
		 INT32  meta_test)

{

   Output_File *Irb_File1;

   Set_Error_Line ( ERROR_LINE_UNKNOWN );

   WHIRL_Keep_Cvt_On = TRUE;

   if ( Generate_IR ) {
   
      Initialize_Symbol_Tables (TRUE);
      cwh_auxst_alloc_container_table();
      cwh_auxst_register_table();

      if ( FE_Write_Binary ) {
	 if (!Irb_File_Name) {
	    Irb_File_Name = New_Extension(code_file_name,
					  IRB_FILE_EXTENSION);
	 }
	 Irb_File1 = Open_Output_Info ( Irb_File_Name ); 
#ifdef KEY /* Bug 12559 */
	 cwh_dst_init_file(src_fname) ;
#else /* KEY Bug 12559 */
	 cwh_dst_init_file(src_path_name) ;
#endif /* KEY Bug 12559 */
         cwh_stmt_init_file(test_flag(flags,PDGCS_INITIALIZE_MP));
	 fe_preg_init() ;

      }
   }

   if (Get_Trace (TKIND_IR,TP_IRB) || 
       Get_Trace (TKIND_SYMTAB,TP_IRB) ||
       Get_Trace (TP_IRB,TINFO_STATS)) {
      
      /* Use the Cray debug file */
      if (debug_file) {
	 Set_Trace_File_internal(debug_file);
      } else if (debug_file_name) {
	 Set_Trace_File_internal(init_debug_file());
      } else {
	 if (!Trc_File_Name) {
	    /* We need to make our own trace file */
	    Trc_File_Name = New_Extension(code_file_name,".t");
	 }
	 Set_Trace_File_internal(fopen(Trc_File_Name,"w"));
      }
      if (TFile == NULL) {
	 fprintf(stderr,"Error opening trace file %s, using stdout\n",Trc_File_Name); 
	 Set_Trace_File_internal(stdout);
      } 
   }

   if (Get_Trace(TP_IRB, 0x2000)) /* -tt11:0x2000 */
     DSTdump_File_Name = New_Extension(code_file_name, DSTDUMP_FILE_EXTENSION);
   
#ifndef KEY /* bug 11521: Disable common block splitting */
   /* We want to split COMMON with large into component blocks if
    * (a) -OPT:pad_common is set (unconditional), or
    * (b) -OPT:reorg_common is set and -FE:full_split is not OFF.
    * -FE:full_split=OFF is implicitly set by the driver if -IPA or
    * -INLINE is set.
    *
    * Common block splitting requires cooperation from the linker.
    * Currently we do this only for MIPS targets.
    */
#ifdef TARG_MIPS
   if ( ! FE_Full_Split_Set ) 
     FE_Full_Split = OPT_Reorg_Common || OPT_Pad_Common;
#endif
#endif /* bug 11421 */
   
   /* update/delete the .rii file */

    if ( rii_file_name ) {

      if ( enable_dsm_processing ) {
        if ( enable_dsm_recompile == FALSE )
          update_rii_file ();
      }

      else
        delete_rii_file ();
    }
   

   /* Initialize the WHIRL mapping mechanism */

   MEM_POOL_Initialize(map_mem_pool,"map_mem_pool",FALSE);  /* Init to 0 always */
   MEM_POOL_Push(map_mem_pool);
   Current_Map_Tab = WN_MAP_TAB_Create(map_mem_pool);

   Set_Error_Phase ( "Front End Parse/Semantic");
 }    

/*===============================================
 *
 * PDGCS_comp_unit
 *
 * Initialize data structures at the start of
 * a new compilation unit.
 * 
 *===============================================
 */ 
/*ARGSUSED*/
extern void
PDGCS_comp_unit(char          *comp_unit_name,
                INT32          module_node)
                

{
}


/*===============================================
 *
 * PDGCS_new_proc
 *
 * Initialize PU information at the start of 
 * each procedure.
 * 
 * If there is an internal procedure, allocate its
 * parent's PU_info to maintain the correct nesting,
 * and don't reallocate when the parent comes along.
 * PU_Tree_Root is the first level 1 PU we see.
 * PU_Parent is only set with an internal proc.
 *
 * A symtab is allocated in fei_proc_def.
 *
 *===============================================
 */ 
/*ARGSUSED*/
extern void 
PDGCS_new_proc(INT32  ir_count,
	       INTPTR func_st_idx,
	       INT32  alt_entry_count,
	       INT32  scalar_opt_level,
	       INT32  vector_opt_level,
	       INT32  task_opt_level,
	       INT32  opt_flags,
	       INT32  user_mobes,
	       INT32  user_sades,
	       INT32  lineno,
	       INT32  meta_opt_level)
{

  PU_Info *pu;
  PU_Info *pp;
  STB_pkt *p ;
  ST      *fn;


  p  = cast_to_STB(func_st_idx);
  fn = cast_to_ST(p->item); 

  cwh_stab_set_symtab(fn);


  if ( IN_NESTED_PU ) {

    /* internal, allocate parent's PU info too  */

    pu = cwh_pdgcs_pu_mem();

    if (PU_Parent == NULL) {

      pp = cwh_pdgcs_pu_mem();
      PU_Info_child(pp) = pu ;

      if (PU_Tree_Root == NULL)
	PU_Tree_Root = pp;
      else
	PU_Info_next(PU_Current) = pp ;

      PU_Parent = pp ;
      Set_PU_Info_flags(pp,PU_HAS_NESTED_PU);

    } else
	PU_Info_next(PU_Current) = pu ;

  } else if (PU_Tree_Root == NULL) {   

    /* 1st seen, external */

    pu = cwh_pdgcs_pu_mem();
    PU_Tree_Root = pu;
    PU_Parent = NULL;

  } else if (PU_Parent == NULL) {       

    /* another external */

    pu = cwh_pdgcs_pu_mem();
    PU_Info_next(PU_Current) = pu ;
  
  } else {                              

    /* parent of internal */

    pu = PU_Parent;
    PU_Parent = NULL;
  }

  PU_Info_proc_sym(pu) = ST_st_idx(fn) ;
  PU_Info_maptab(pu)   = Current_Map_Tab;
  PU_Current = pu ;

  /* Reset label # for computed goto/assign for each PU */

  cwh_assign_label_id=0;

  /* Create array information map, bounds checking needs array name */

  array_name_map = WN_MAP_Create(map_mem_pool);

  Set_Error_Phase ( "IR->WHIRL Conversion" );
}

/*===============================================
 *
 * fei_proc_body
 *
 * Initialize WN generation for procedure's body.
 *
 *===============================================
 */ 

extern void 
fei_proc_body( INT32 lineno )
{

  ST *st;

  st = &St_Table[PU_Info_proc_sym(PU_Current)];
  cwh_stab_set_tylist_for_entries(st);
  cwh_stab_emit_commons_and_equivalences(CURRENT_SYMTAB);
  cwh_stmt_init_pu(st,lineno);

}

/*===============================================
 *
 * PDGCS_do_proc
 *
 * Write out the IR for the current PU. Fill in
 * the PU info with the WN tree and symbol table.
 * Generate the DST information for this PU, but
 * don't write it out.
 *
 * If it was a main PU, then we exit with a call
 * to _END, called from the FE. The debuggger's
 * stack trace needs a return address to be spilled
 * correctly, so a return is added here.
 *
 * Free the space associated with WN Tree.
 *
 *===============================================
 */ 
extern void 
#ifdef KEY /* Bug 3507 */
PDGCS_do_proc(int is_module)
#else /* KEY Bug 3507 */
PDGCS_do_proc(void)
#endif /* KEY Bug 3507 */
{

  WN      *wn; 
  PU_Info *pu;
#ifndef KEY /* Bug 10177 */
  TYPE     tt;
#endif /* KEY Bug 10177 */
  ST *st;

  DST_IDX d  ;

  pu = PU_Current ;

  st = &St_Table[PU_Info_proc_sym(pu)];
  PU& p = Pu_Table[ST_pu(st)];
  if (PU_is_mainpu (p)) {
#ifdef KEY /* Bug 10177 */
     TYPE     tt;
     memset(&tt, 0, sizeof(tt));
#endif /* KEY Bug 10177 */
     fei_return(2,tt);  /* It's OK for this to be uninitialized */
  }

  if (cwh_stab_pu_has_globals) {
     Set_PU_Info_flags(pu,PU_HAS_GLOBALS);
  }

  cwh_stmt_postprocess_pu();
  wn = cwh_stmt_end_pu();
#ifdef KEY /* Bug 3507 */
  /* If this is a module, we already created a DW_TAG_module, not a
   * DW_TAG_subprogram, for it */
  if (is_module) {
    cwh_dst_module_vars(st);

    Set_PU_Info_tree_ptr(pu, wn); 
  }
  else {
    d  = cwh_dst_enter_pu(st);

    Set_PU_Info_tree_ptr(pu, wn); 
    Set_PU_Info_pu_dst(pu,d);
    Set_PU_Info_cu_dst(pu,d);
  }
#else /* KEY Bug 3507 */
  d  = cwh_dst_enter_pu(st);
  Set_PU_Info_pu_dst(pu,d);
  Set_PU_Info_cu_dst(pu,d);
#endif /* KEY Bug 3507 */

  Set_PU_Info_state(pu, WT_SYMTAB, Subsect_InMem);
  Set_PU_Info_state(pu, WT_TREE, Subsect_InMem);
  Set_PU_Info_state(pu, WT_PROC_SYM, Subsect_InMem);

  // Emit initializations
  cwh_data_emit_symbol_inits(CURRENT_SYMTAB);
                      
  if (Get_Trace (TKIND_IR,TP_IRB)) 
     fdump_tree(TFile,wn);

  if (Get_Trace (TKIND_SYMTAB,TP_IRB)) {

     fprintf(TFile,"PU info: %s (0x%08x)\n",
             ST_name(PU_Info_proc_sym(pu)), 
             PU_Info_flags(pu));

     Print_local_symtab (TFile, Scope_tab [CURRENT_SYMTAB]);
  }

  cwh_stk_verify_empty();
  Verify_SYMTAB (CURRENT_SYMTAB);
  Write_PU_Info (pu);


  WN_MAP_Delete(array_name_map);
  WN_Mem_Pop();

  Set_Error_Phase ( "IR->WHIRL Conversion" );
}

/*===============================================
 *
 * PDGCS_end_procs
 *
 * Clean up at the end of a procedure.
 *
 *===============================================
 */ 
/*ARGSUSED*/
extern void 
PDGCS_end_procs(INT32 *code_size,
                INT32 *data_size )
{
  cwh_stab_end_procs();
  Set_Error_Phase ( "Front End Parse/Semantic");
}

/*===============================================
 *
 * PDGCS_terminate
 *
 * Clean up at the end of compilation. Write
 * out the global symbol table, the DST info 
 * and the Makedepend information.
 *
 *===============================================
 */ 
/*ARGSUSED*/
extern void 
PDGCS_terminate ( void)
{

  // Emit common & module data that was avoiding duplication
  cwh_stab_emit_commons_and_equivalences(GLOBAL_SYMTAB);

  // Emit initializations
  cwh_data_emit_symbol_inits(GLOBAL_SYMTAB);

  if (Get_Trace (TKIND_SYMTAB,TP_IRB)) {
    Print_global_symtab ( TFile );
  }
  
  cwh_dst_write();

  // TODO_NEW_SYMTAB: figure out how to setup FE_gdar_filename
  Verify_SYMTAB (GLOBAL_SYMTAB);
  Write_Global_Info (PU_Tree_Root);
  Close_Output_Info ();
  
  cwh_write_makedepend();

  Set_Error_Phase ( "Front End Parse/Semantic");
}

/*===============================================
 *
 * cwh_pdgcs_pu_mem
 *
 * Initalize the memory pools for a PU. The WN
 * pool is deleted after the WN tree is written 
 * (PDGCS_do_proc), but the PU_info holds chains 
 * of PUs and persists until the end. 
 *
 *===============================================
 */ 
static PU_Info *
cwh_pdgcs_pu_mem(void)
{

  PU_Info *p;
 
  p = TYPE_MEM_POOL_ALLOC(PU_Info, FE_Mempool);
  PU_Info_init(p);

  WN_Mem_Push();

  return p ;

}
/*===============================================
 *
 * rii routines
 *
 * Handles the .rii files for dsm processing
 * 
 * routines lifted from common/fe/edwhirl.c
 *
 *===============================================
 */ 

static const char *
dirname ( char * const s )
{
  char * p;
  char * name;
  int    size;

  if ( s == NULL || *s == '\0' )
    return ".";

  else {

    p = s + strlen ( s );

    /* skip trailing '/' */

    while ( p != s && *p == '/' )
      --p;

    while ( p != s ) {

      if ( *--p == '/' ) {

        if ( p == s )
          return "/";

        while ( *p == '/' )
          --p;

        size = p - s + 1;
        name = (char *) malloc ( size + 1 );
        strncpy ( name, s, size );
	name [size] = '\0';
        return name;
      }
    }

    return ".";
  }
} /* dirname */

/*
 * Skip over leading lines upto and including the terminator (a line that
 * starts with ----). If the terminator is not found, rewind back to start.
 */
static void
skip_old_rii_controls ( FILE * f )
{
  int c;
  int terminator_found = 0;

  c = getc ( f );

  while ( c != EOF ) {

    if (    c == '-'
         && ( c = getc ( f ) ) == '-'
         && ( c = getc ( f ) ) == '-' 
         && ( c = getc ( f ) ) == '-' )
      terminator_found = 1;

    while ( c != '\n' && c != EOF )
      c = getc ( f );

    if ( terminator_found )
      break;

    if (c == '\n')
      c = getc ( f );
  }

  if ( c == EOF )
    rewind(f);
}


static void
delete_rii_file ( void )
{
  FILE * f_rii_file;

  f_rii_file = fopen ( rii_file_name, "r" );

  if ( f_rii_file ) {

    fclose ( f_rii_file );
    unlink ( rii_file_name );
  }
} /* delete_rii_file */


static void
update_rii_file ( void )
{
  FILE * f_rii_file;
  FILE * f_old_rii_file;
  FILE * f_cmd_file;
  const char * rii_dir_name;
  char * new_rii_file_name;
  int    ch;

  rii_dir_name = dirname ( rii_file_name );
  f_old_rii_file = fopen ( rii_file_name, "r" );

  if ( f_old_rii_file == NULL ) {

    if ( mkdir ( rii_dir_name, 0777 ) < 0 && errno != EEXIST ) {

      /* failed to create directory, and it does not already exists */

/*    str_catastrophe ( ec_could_not_create_ii_dir, rii_dir_name ); */
      fprintf ( stderr, "could not create rii directory %s\n", rii_dir_name );
      return;

    }

    else
      new_rii_file_name = rii_file_name;
  }

  else {

    skip_old_rii_controls ( f_old_rii_file );

    new_rii_file_name = (char *) malloc ( strlen ( rii_file_name ) + 5 );
    sprintf ( new_rii_file_name, "%s.NEW", rii_file_name );
  }

  if ( access ( rii_dir_name, W_OK ) < 0 ) {

    /* cannot write into instantiation info directory */

/*  str_catastrophe ( ec_no_write_permission_in_ii_dir, rii_dir_name ); */
    fprintf ( stderr, "no write permissions in rii directory %s\n",
                      rii_dir_name );
    return;
  }

  f_rii_file = fopen ( new_rii_file_name, "w" );

  if ( f_rii_file == NULL ) {

/*  str_catastrophe ( ec_no_write_permission_in_ii_dir, rii_dir_name ); */
    fprintf ( stderr, "unable to open rii file for writing %s\n", rii_file_name );
    return;
  }

  f_cmd_file = fopen ( FE_command_line, "r" );

  if ( f_cmd_file == NULL ) {

/*  str_catastrophe ( ec_no_write_permission_in_ii_dir, FE_command_line ); */
    fprintf ( stderr, "unable to open cmd file for reading %s\n",
                      FE_command_line );
    return;
  }

  fputs ( "CMDLINE=", f_rii_file );

  while ( ( ch = getc ( f_cmd_file ) ) != '\n' )
    putc ( ch, f_rii_file );

  fputs ( "\nPWD=", f_rii_file );

  while ( ( ch = getc ( f_cmd_file ) ) != '\n' )
    putc ( ch, f_rii_file );

  fclose ( f_cmd_file );

  fputs ( "\n----\n", f_rii_file );

  if ( f_old_rii_file ) {

    while ( ( ch = getc ( f_old_rii_file ) ) != EOF ) 
      putc ( ch, f_rii_file );

    /* Rename the new file to replace the existing .rii file */

    fclose ( f_old_rii_file );
    fclose ( f_rii_file );

    if ( rename ( new_rii_file_name, rii_file_name ) < 0 ) {

      fprintf (stderr, "error in renaming %s %s\n",
                         new_rii_file_name, rii_file_name );
    }

    free ( new_rii_file_name );
  }

  else
    fclose ( f_rii_file );
} /* update_rii_file */
