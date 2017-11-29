/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it would be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * Further, this software is distributed without any warranty that it is
 * free of the rightful claim of any third person regarding infringement 
 * or the like.  Any license provided herein, whether implied or 
 * otherwise, applies only to this software file.  Patent licenses, if 
 * any, provided herein do not apply to combinations of this program with 
 * other software, or any other product whatsoever.  
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write the Free Software Foundation, Inc., 59
 * Temple Place - Suite 330, Boston MA 02111-1307, USA.
 */

                                                                                                                                                     
/* ====================================================================
 * ====================================================================
 *
 * Module: cg_gcov.cxx
 * $Revision: 1.43 $
 * $Date: 05/12/05 08:59:03-08:00 $
 * $Author: bos@eng-24.pathscale.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.cg_gcov.cxx $
 *
 * Description:
 *
 * This file contains the routines for the generation of GCOV files .
 *
 *
 * ====================================================================
 * ====================================================================
 */

#include "bb.h"
#include "cg.h"
#include "cg_region.h"
#include "cg_flags.h"
#include "cgexp.h"
#include "cgexp_internals.h"
#include "cxx_memory.h"
#include "data_layout.h"
#include "defs.h"
#include "flags.h"
#include "gcov-io.h"
#include "glob.h"
#include "ir_reader.h"
#include "label_util.h"
#include "mempool.h"
#include "stblock.h"
#include "symtab.h"
#include "symtab_access.h"
#include "unistd.h"

#if defined(TARG_MIPS) && !defined(TARG_SL)
//static BOOL inline Is_Target_64bit (void) { return TRUE; }
static BOOL inline Is_Target_32bit (void) { return FALSE; }
#endif // TARG_MIPS

MEM_POOL name_pool, *name_pool_ptr = NULL;
#define MAX_COUNTER_SECTIONS       4
#define BB_TO_GCOV_INDEX(bb) BB_id(bb) 
#define MAXPATHLEN 100

struct function_list
{
  struct function_list *next;   /* next function */
  char *name;			/* function name */
  long cfg_checksum;            /* function checksum */
#ifdef GCC_303
  unsigned n_counter_sections;  /* number of counter sections */
  struct counter_section counter_sections[MAX_COUNTER_SECTIONS];
#else
  long count_edges;             /* number of intrumented edges in this function */
#endif
};

static struct function_list *functions_head = 0;
static struct function_list **functions_tail = &functions_head;                                                      

/* Name and file pointer of the output file for the basic block graph.  */
                                                                                                                                                             
static FILE *bbg_file;
static char* bbg_file_name;

#ifndef GCC_303
static FILE *bb_file;
#endif 

/* Name and file pointer of the input file for the arc count data.  */
                                                                                                                                                    
static FILE *da_file;
                                                                                                                                                    
/* Pointer of the output file for the basic block/line number map.  */

static int last_bb_file_num, last_bb_line_num;
static int local_n_edges = 0;
static int local_n_a_edges = 0;
static int local_n_basic_blocks = 0;
static int n_instr_edges = 0;
/* Compute checksum for the current function.  */

static int pu_flag = -1;
static int begin_id = -1, end_id = -1;
static long chksum = 0;

#  define DIR_SEPARATOR '/'
#  define IS_DIR_SEPARATOR(ch) ((ch) == DIR_SEPARATOR)
char *
lbasename (char *name)
{
  char *base;
  for (base = name; *name; name++)
    if (IS_DIR_SEPARATOR (*name))
      base = name + 1;
                                                                                                                                                             
  return base;
}
char *
getpwd ()
{
  static char *pwd = 0;
  if (!pwd)
    pwd = getcwd (CXX_NEW_ARRAY(char, MAXPATHLEN + 1, name_pool_ptr), MAXPATHLEN + 1);
  return pwd;
}
static ST * get_symbol(const char* sym_name)
{
  ST *st;
  int i;
  FOREACH_SYMBOL (GLOBAL_SYMTAB, st, i) {
    if (ST_class(st) == CLASS_VAR && strncmp(ST_name(st), sym_name, strlen(sym_name))==0){
      return st;
      break;
    }
  }
  return NULL;
}
// Bug 3806
void Gcov_BB_Prepend_Ops(BB *bb, OPS *ops)
{
#ifdef TARG_X8664
  if (OPS_first(ops) == NULL) return;
  OP *op = BB_first_op(bb);
  if (op && OP_opnds(op) > 0 && 
      TN_is_register(OP_opnd(op, 0)) && 
      TN_register(OP_opnd(op, 0)) == RAX)
    BB_Insert_Ops_After(bb, op, ops);
  else
    BB_Prepend_Ops(bb, ops);
#else
// Will see later
  return;
#endif
}

#ifdef GCC_303
void 
CG_Init_Couner_Infos(ST *counter_section)
{
  struct function_list *item = functions_head;
  ST *st = get_symbol("LPBX2");
  FmtAssert (st != NULL, ("Symbol LPBX2 should have generated before"));

  TY_IDX tyi;
  TY& ty = New_TY(tyi);
  TY_Init(ty, n_instr_edges*8, KIND_STRUCT, MTYPE_M,
          STR_IDX_ZERO);
  Set_TY_align(tyi, 8);
  ST* new_st = New_ST(GLOBAL_SYMTAB);
  ST_Init(new_st, Save_Str("LPBX2_TMP"),
          CLASS_VAR, SCLASS_PSTATIC, EXPORT_LOCAL, tyi);
  //Set_ST_is_initialized(new_st);
  Allocate_Object(new_st);
  INITO_IDX inito_lpbx2 = New_INITO(st);
  INITV_IDX initv_lpbx2;
  initv_lpbx2 = New_INITV();
  INITV_Init_Symoff(initv_lpbx2, new_st, 0);
  Append_INITV(initv_lpbx2, inito_lpbx2, INITV_IDX_ZERO);

 
  INITV_IDX prev_initv = INITV_IDX_ZERO;
  INITO_IDX inito_counter = New_INITO(counter_section);
  INITV_IDX inv_counter;
  for (INT i = 0; i < item->n_counter_sections; i++)
  {
    inv_counter = New_INITV();
    INITV_Init_Integer(inv_counter, MTYPE_I4, GCOV_TAG_ARC_COUNTS);
    prev_initv = Append_INITV(inv_counter, inito_counter, prev_initv);
    inito_counter = INITO_IDX_ZERO;
    inv_counter = New_INITV();
    INITV_Init_Integer(inv_counter, MTYPE_I4, n_instr_edges);
    prev_initv = Append_INITV(inv_counter, inito_counter, prev_initv);
    // LPBX2
    inv_counter = New_INITV();
    INITV_Init_Symoff(inv_counter, new_st, 0);
    prev_initv = Append_INITV(inv_counter, inito_counter, prev_initv);
  }
}
#endif
void
CG_Init_Func_Infos(ST *func_infos)
{
  struct function_list *item = functions_head;
  TY_IDX table;
  ST *listvar;
  INITO_IDX ino;
  INITV_IDX inv;
  INITO_IDX inito;
  INITV_IDX last_aggregate_initv = INITV_IDX_ZERO;
  inito = New_INITO(func_infos);
  TYPE_ID rtype;

  if (Is_Target_32bit())
    rtype = MTYPE_I4;
  else 
    rtype = MTYPE_I8;
  

  for (; item != 0; item = item->next){
#ifndef GCC_303
    /* checksum */
    inv = New_INITV();
    INITV_Init_Integer(inv, rtype, item->cfg_checksum);
    last_aggregate_initv = Append_INITV(inv, inito, last_aggregate_initv);
    inito = INITO_IDX_ZERO;
   /* arc_count */
    inv = New_INITV();
    INITV_Init_Integer(inv, MTYPE_I4, item->count_edges);
    last_aggregate_initv = Append_INITV(inv, inito, last_aggregate_initv);

    if (Is_Target_64bit()) {
      inv = New_INITV();
      INITV_Init_Pad(inv, 4);
      last_aggregate_initv = Append_INITV(inv, inito, last_aggregate_initv);
    }
#endif
    /* name */                                                        
    table = Make_Array_Type(MTYPE_U2, 1, strlen(item->name)+1);
    listvar = Gen_Read_Only_Symbol(table, item->name);
    Set_ST_is_initialized(listvar);    /* so goes in rdata section */
    ino = New_INITO(listvar);
    inv = New_INITV();
    INITV_Init_String(inv, item->name, strlen(item->name)+1);
    Append_INITV (inv, ino, INITV_IDX_ZERO);
    Allocate_Object(listvar);

    inv = New_INITV();
    INITV_Init_Symoff (inv, listvar, 0);
    last_aggregate_initv = Append_INITV (inv, inito, last_aggregate_initv);

    inito = INITO_IDX_ZERO;
#ifdef GCC_303
    /* checksum */
    inv = New_INITV();
    INITV_Init_Integer(inv, MTYPE_I4, item->cfg_checksum);
    last_aggregate_initv = Append_INITV(inv, inito, last_aggregate_initv);
    /* n_counter_sections */
    inv = New_INITV();
    INITV_Init_Integer(inv, MTYPE_I4, item->n_counter_sections);
    last_aggregate_initv = Append_INITV(inv, inito, last_aggregate_initv);

    /* counter_sections */ 
    UINT32 tag_size = 4;
    UINT32 n_counters_size = 4;
    UINT32 counter_section_size = tag_size + n_counters_size;

    TY_IDX tyi_counter;
    ST *counter_section;

    TY& ty_counter = New_TY(tyi_counter);
    TY_Init(ty_counter, counter_section_size*item->n_counter_sections, KIND_STRUCT, MTYPE_M, STR_IDX_ZERO);
    Set_TY_align(tyi_counter, 8);
    counter_section = New_ST(GLOBAL_SYMTAB);
    ST_Init(counter_section, STR_IDX_ZERO, CLASS_VAR, SCLASS_FSTATIC, EXPORT_LOCAL, tyi_counter);
    Set_ST_is_initialized(counter_section);
    Allocate_Object(counter_section);

    INITV_IDX prev_initv = INITV_IDX_ZERO;
    INITO_IDX inito_counter = New_INITO(counter_section);
    INITV_IDX inv_counter;
    for (INT i = 0; i < item->n_counter_sections; i++)
    {
      inv_counter = New_INITV();
      INITV_Init_Integer(inv_counter, MTYPE_I4, GCOV_TAG_ARC_COUNTS);
      prev_initv = Append_INITV(inv_counter, inito_counter, prev_initv);
      inito_counter = INITO_IDX_ZERO;
      inv_counter = New_INITV();
      INITV_Init_Integer(inv_counter, MTYPE_I4, item->counter_sections[0].n_counters);
      prev_initv = Append_INITV(inv_counter, inito_counter, prev_initv);
    }

    inv = New_INITV();
    INITV_Init_Symoff (inv, counter_section, 0);
    last_aggregate_initv = Append_INITV (inv, inito, last_aggregate_initv);
#endif
  } 
#ifndef GCC_303
    inv = New_INITV();
    INITV_Init_Pad(inv, Is_Target_32bit() ? 4 : 8 );
    last_aggregate_initv = Append_INITV(inv, inito, last_aggregate_initv);
    inito = INITO_IDX_ZERO;
    inv = New_INITV();
    INITV_Init_Integer(inv, MTYPE_I4, -1);
    last_aggregate_initv = Append_INITV(inv, inito, last_aggregate_initv);
    inv = New_INITV();
    INITV_Init_Pad(inv, Is_Target_32bit() ? 4 : 12 );
    last_aggregate_initv = Append_INITV(inv, inito, last_aggregate_initv);
#endif
}

void
CG_End_Final()
{
  int num_nodes = 0;
  struct function_list *item;
  for (item = functions_head; item != 0; item = item->next)
  {
    num_nodes++;
  }
  if (num_nodes == 0) return;
  ST *st = get_symbol("LPBX0");
  FmtAssert (st != NULL, ("Symbol LPBX0 should have generated before"));
  if ( ST_is_not_used(st) ){
    Clear_ST_is_not_used(st);
    Allocate_Object(st);
  }

#ifdef GCC_303
  /* Version ident */
  INITO_IDX aggregate_inito = New_INITO(st);
  INITV_IDX inv = New_INITV();
  INITV_Init_Integer(inv, MTYPE_U8, GCOV_VERSION);
  INITV_IDX last_aggregate_initv;
  last_aggregate_initv = Append_INITV (inv, aggregate_inito, INITV_IDX_ZERO);

  /* next -- NULL  */
  inv = New_INITV();
  INITV_Init_Integer(inv, MTYPE_U8, 0);
  last_aggregate_initv = Append_INITV(inv, INITO_IDX_ZERO, last_aggregate_initv);

  /* Address of filename.  */
  char *cwd, *da_filename;
  int da_filename_len;

  cwd = getpwd ();
  da_filename_len = strlen (Src_File_Name) + strlen (cwd) + 4 + 1;
  da_filename = CXX_NEW_ARRAY(char, da_filename_len, name_pool_ptr);
  strcpy (da_filename, cwd);
  strcat (da_filename, "/");
  strcat (da_filename, Src_File_Name);
  INT i;
  for (i = da_filename_len-1; i>0; i--)
    if (da_filename[i] == '.')
      break;
  FmtAssert (da_filename[i] == '.', ("Src_File_Name should associate with the prefix"));
  da_filename[i+1] = 'd';
  da_filename[i+3] = da_filename[i+2];
  da_filename[i+2] = 'a';
  //strcat (da_filename, ".da");

  TY_IDX table = Make_Array_Type(MTYPE_U2, 1, da_filename_len);
  ST *listvar = Gen_Read_Only_Symbol(table, "dafilename_table");
  Set_ST_is_initialized(listvar);    /* so goes in rdata section */
  INITO_IDX ino = New_INITO(listvar);
  INITV_IDX inv_local = New_INITV();
  INITV_Init_String(inv_local, da_filename, da_filename_len);
  Append_INITV (inv_local, ino, INITV_IDX_ZERO);
  Allocate_Object(listvar);
  inv = New_INITV();
  INITV_Init_Symoff (inv, listvar, 0);
  last_aggregate_initv = Append_INITV(inv, INITO_IDX_ZERO, last_aggregate_initv);

  /* Workspace */
  inv = New_INITV();
  INITV_Init_Integer(inv, MTYPE_U8, 0);
  last_aggregate_initv = Append_INITV(inv, INITO_IDX_ZERO, last_aggregate_initv);

  /* number of functions */
  inv = New_INITV();
  INITV_Init_Integer(inv, MTYPE_I4, num_nodes);
  last_aggregate_initv = Append_INITV(inv, INITO_IDX_ZERO, last_aggregate_initv);
  inv = New_INITV();
  INITV_Init_Pad(inv, 4);
  last_aggregate_initv = Append_INITV(inv, INITO_IDX_ZERO, last_aggregate_initv);

  /* function_info table  */
  UINT32 func_name_size = 8;
  UINT32 checksum_size = 4;
  UINT32 n_counter_sections_size = 4;
  UINT32 counter_section_size = 8;
  UINT32 function_info_size =  func_name_size + checksum_size + n_counter_sections_size + counter_section_size;
  TY_IDX tyi_function;
  TY& ty_function = New_TY(tyi_function);
  TY_Init(ty_function, function_info_size * num_nodes, KIND_STRUCT, MTYPE_M,
          STR_IDX_ZERO);
  Set_TY_align(tyi_function, 8);
  ST *func_infos = New_ST(GLOBAL_SYMTAB);
  ST_Init(func_infos, Save_Str("function_infos"), CLASS_VAR, SCLASS_FSTATIC, EXPORT_LOCAL, tyi_function);
  Set_ST_is_initialized(func_infos);
  CG_Init_Func_Infos(func_infos);
  Allocate_Object(func_infos);
  
  inv = New_INITV();
  INITV_Init_Symoff(inv, func_infos, 0);
  last_aggregate_initv = Append_INITV(inv, INITO_IDX_ZERO, last_aggregate_initv);

  /* n_counter_sections  */
  inv = New_INITV();
  INITV_Init_Integer(inv, MTYPE_I4, 1);
  last_aggregate_initv = Append_INITV(inv, INITO_IDX_ZERO, last_aggregate_initv);
  inv = New_INITV();
  INITV_Init_Pad(inv, 4);
  last_aggregate_initv = Append_INITV(inv, INITO_IDX_ZERO, last_aggregate_initv);

  /* counter sections */
  UINT32 tag_size = 4;
  UINT32 n_counters_size = 4;
  UINT32 counters_size = 8;
  counter_section_size = tag_size + n_counters_size + counters_size;

  TY_IDX tyi_counter;
  TY& ty_counter = New_TY(tyi_counter);
  TY_Init(ty_counter, counter_section_size * 3, KIND_STRUCT, MTYPE_M, STR_IDX_ZERO);
  Set_TY_align(tyi_counter, 8);
  ST *counter_section = New_ST(GLOBAL_SYMTAB);
  ST_Init(counter_section, Save_Str("counter_section"), CLASS_VAR, SCLASS_FSTATIC, EXPORT_LOCAL, tyi_counter);
  Set_ST_is_initialized(counter_section);
  Allocate_Object(counter_section);
  
  inv = New_INITV();
  INITV_Init_Symoff(inv, counter_section, 0);
  last_aggregate_initv = Append_INITV(inv, INITO_IDX_ZERO, last_aggregate_initv);
  CG_Init_Couner_Infos(counter_section);
#else
      /* Output the main header, of 7 words:
         0:  1 if this file is initialized, else 0.
         1:  address of file name (LPBX1).
         2:  address of table of counts (LPBX2).
         3:  number of counts in the table.
         4:  always 0, libgcc2 uses this as a pointer to next ``struct bb''
         The following are GNU extensions:
         5:  Number of bytes in this header.
         6:  address of table of function checksums (LPBX7).  */
  TYPE_ID rtype;
  INT32 ty_size;

  if (Is_Target_32bit()) {
    rtype = MTYPE_U4;
    ty_size = 4;
  }
  else {
    rtype = MTYPE_U8;
    ty_size = 8;
  }
  
  /* The zero word.  */
  INITO_IDX aggregate_inito = New_INITO(st);
  INITV_IDX inv = New_INITV();
  INITV_Init_Integer(inv, rtype, 0);
  INITV_IDX last_aggregate_initv;
  last_aggregate_initv = Append_INITV (inv, aggregate_inito, INITV_IDX_ZERO);

  /* Address of filename.  */
  char *cwd, *da_filename;
  int da_filename_len,i;

  cwd = getpwd ();
  da_filename_len = strlen (lbasename(Src_File_Name)) + strlen (cwd) + 4 + 1;
  da_filename = CXX_NEW_ARRAY(char, da_filename_len, name_pool_ptr);
  strcpy (da_filename, cwd);
  strcat (da_filename, "/");
  strcat (da_filename, lbasename(Src_File_Name));
  for (i = da_filename_len-1; i>0; i--)
    if (da_filename[i] == '.')
      break;
  FmtAssert (da_filename[i] == '.', ("Src_File_Name should associate with the prefix"));
  da_filename[i] = 0;
  strcat (da_filename, ".da");

  TY_IDX table = Make_Array_Type(MTYPE_U2, 1, da_filename_len);
  ST *listvar = Gen_Read_Only_Symbol(table, "dafilename_table");
  Set_ST_is_initialized(listvar);    /* so goes in rdata section */
  INITO_IDX ino = New_INITO(listvar);
  INITV_IDX inv_local = New_INITV();
  INITV_Init_String(inv_local, da_filename, da_filename_len);
  Append_INITV (inv_local, ino, INITV_IDX_ZERO);
  Allocate_Object(listvar);
  inv = New_INITV();
  INITV_Init_Symoff (inv, listvar, 0);
  last_aggregate_initv = Append_INITV(inv, INITO_IDX_ZERO, last_aggregate_initv);

  /* Table of counts.  */
  st = get_symbol("LPBX2");
  FmtAssert (st != NULL, ("Symbol LPBX2 should have generated before"));
  if ( ST_is_not_used(st) ){
    Clear_ST_is_not_used(st);
    Allocate_Object(st);
  }

  TY_IDX tyi;
  TY& ty = New_TY(tyi);
  TY_Init(ty, n_instr_edges*8, KIND_STRUCT, MTYPE_M,
          STR_IDX_ZERO);
  Set_TY_align(tyi, Is_Target_32bit() ? 4 : 32);
  ST* new_st = New_ST(GLOBAL_SYMTAB);
  ST_Init(new_st, Save_Str("LPBX2_TMP"),
          CLASS_VAR, SCLASS_PSTATIC, EXPORT_LOCAL, tyi);
  Allocate_Object(new_st);

  INITO_IDX inito_lpbx2 = New_INITO(st);
  INITV_IDX initv_lpbx2;
  initv_lpbx2 = New_INITV();
  INITV_Init_Symoff(initv_lpbx2, new_st, 0);
  Append_INITV(initv_lpbx2, inito_lpbx2, INITV_IDX_ZERO);

  inv = New_INITV();
  INITV_Init_Symoff(inv, new_st, 0);
  last_aggregate_initv = Append_INITV(inv, INITO_IDX_ZERO, last_aggregate_initv);

  /* Count of the # of instrumented arcs  */
  inv = New_INITV();
  INITV_Init_Integer(inv, rtype, n_instr_edges);
  last_aggregate_initv = Append_INITV(inv, INITO_IDX_ZERO, last_aggregate_initv
);

  /* Pointer to the next bb.  */
  inv = New_INITV();
  INITV_Init_Integer(inv, rtype, 0);
  last_aggregate_initv = Append_INITV(inv, INITO_IDX_ZERO, last_aggregate_initv);

 /* sizeof(struct bb) */
  inv = New_INITV();
  INITV_Init_Integer(inv, rtype, 7*ty_size);
  last_aggregate_initv = Append_INITV(inv, INITO_IDX_ZERO, last_aggregate_initv);
 
  /* struct bb_function [].  */
  UINT32 checksum_size = ty_size;
  UINT32 n_arcs_size = 4;
  UINT32 func_name_size = ty_size;
  UINT32 function_info_size =  checksum_size + n_arcs_size*(Is_Target_32bit()?1:2) + func_name_size;
  TY_IDX tyi_function;
  TY& ty_function = New_TY(tyi_function);
  TY_Init(ty_function, function_info_size * (num_nodes + 1), KIND_STRUCT, MTYPE_M,
          STR_IDX_ZERO);
  Set_TY_align(tyi_function, ty_size);
  ST *func_infos = New_ST(GLOBAL_SYMTAB);
  ST_Init(func_infos, Save_Str("function_infos"), CLASS_VAR, SCLASS_FSTATIC, EXPORT_LOCAL, tyi_function);
  Set_ST_is_initialized(func_infos);
  CG_Init_Func_Infos(func_infos);
  Allocate_Object(func_infos);
  
  inv = New_INITV();
  INITV_Init_Symoff(inv, func_infos, 0);
  last_aggregate_initv = Append_INITV(inv, INITO_IDX_ZERO, last_aggregate_initv);
#endif
}
void
CG_Compute_Checksum ()
{
#define CHSUM_HASH      500000003
#define CHSUM_SHIFT     2
  if (!Cur_PU_Name || strncmp(Cur_PU_Name, "_GLOBAL__GCOV_", strlen("_GLOBAL__GCOV_"))==0) 
     return;
  local_n_edges = 0;
  local_n_a_edges = 0;
  local_n_basic_blocks = 0;
  chksum = 0;
  BB* bb;
  for (bb = REGION_First_BB; bb; bb = BB_next(bb)) {
    BBLIST* bb_succs = BB_succs( bb );
    if (!bb_succs) {
      local_n_a_edges ++;
      local_n_basic_blocks++;
      chksum = ((chksum==0?1:chksum) << CHSUM_SHIFT) % CHSUM_HASH;
      continue;
    }
    BBLIST *succ;
    FOR_ALL_BBLIST_ITEMS(bb_succs, succ){
#ifdef GCC_303
      unsigned value = BB_TO_GCOV_INDEX (BBLIST_item(succ));
      unsigned ix;
      value ^= value << 16;
      value ^= value << 8;
      for (ix = 8; ix--; value <<= 1)
      {
        unsigned feedback;
        feedback = (value ^ chksum) & 0x80000000 ? 0x04c11db7 : 0;
        chksum <<= 1;
        chksum ^= feedback;
      }
#else
      chksum = ((chksum << CHSUM_SHIFT) + (BB_TO_GCOV_INDEX (BBLIST_item(succ)) + 1)) % CHSUM_HASH;
#endif
//      if (BB_Fall_Thru_Successor(bb) == BBLIST_item(succ))
      if (!BBLIST_on_tree(succ))
        local_n_edges ++;
      local_n_a_edges ++;
      chksum = (chksum << CHSUM_SHIFT) % CHSUM_HASH;
    }
    //if (bb_succs)
    local_n_basic_blocks++;
  }
  struct function_list *new_item = (struct function_list *)CXX_NEW_ARRAY(char, sizeof (struct function_list), name_pool_ptr);
                                                                                
  *functions_tail = new_item;
  functions_tail = &new_item->next;
                                                                                
  new_item->next = 0;
  new_item->name = CXX_NEW_ARRAY(char, strlen(Cur_PU_Name)+1, name_pool_ptr);
  strcpy (new_item->name, Cur_PU_Name); 
  new_item->cfg_checksum = chksum;
#ifdef GCC_303
  new_item->n_counter_sections = 1;
  new_item->counter_sections[0].n_counters = local_n_edges;
  new_item->counter_sections[0].tag = GCOV_TAG_ARC_COUNTS;
#else
  new_item->count_edges = local_n_edges+1;
  n_instr_edges += local_n_edges+1;
#endif
  //return chksum;
}

/* Union find algorithm implementation for the basic blocks using
   aux fields.  */
                                                                               
static BB*
find_group (BB* bb)
{
  BB* group = bb, *bb1;
                                                                               
  while (group != NULL && BB_aux(group) != group)
    group = BB_aux(group);
                                                                               
  /* Compress path.  */
  while (bb && group && BB_aux(bb) != group)
    {
      bb1 = BB_aux(bb);
      BB_aux(bb) = group;
      bb = bb1;
    }
  return group;
}
                                                                               
static void
union_groups (BB* bb1, BB* bb2)
{
  BB* bb1g = find_group (bb1);
  BB* bb2g = find_group (bb2);

  FmtAssert (bb1g != bb2g, ("Two group should be in one spanning tree"));
  if (bb1g)
    BB_aux(bb1g) = bb2g;
  else
    BB_aux(bb2g) = bb1g;
}
static BOOL EDGE_CRITICAL_P(BB* src, BB *dest)
{
  BBLIST* bb_succs = BB_succs( src );
  BBLIST* bb_preds = BB_preds( dest );
  return ( (BBlist_Len(bb_succs) > 1) &&  (BBlist_Len(bb_preds) > 1) );
}

/* This function searches all of the edges in the program flow graph, and puts
   as many bad edges as possible onto the spanning tree.  Bad edges include
   abnormals edges, which can't be instrumented at the moment.  Since it is
   possible for fake edges to form a cycle, we will have to develop some
   better way in the future.  Also put critical edges to the tree, since they
   are more expensive to instrument.  */
static void 
find_spanning_tree()
{
  BBLIST* bb_succs;
  BBLIST *succ;
  BB *bb;
  
  for (bb = REGION_First_BB; bb; bb = BB_next(bb)) 
  {
    if (BB_entry(bb) || BB_call(bb) || BB_exit(bb))
      BB_aux(bb) = NULL;
    else
      BB_aux(bb) = bb;
  }

  for (bb = REGION_First_BB; bb; bb = BB_next(bb)) 
  {
    bb_succs = BB_succs( bb );
    if (!bb_succs) continue;

    FOR_ALL_BBLIST_ITEMS(bb_succs, succ){
      if (EDGE_CRITICAL_P (bb, BBLIST_item(succ)) && find_group (bb) != find_group (BBLIST_item(succ))){
        Set_BBLIST_on_tree(succ);
        Set_BBLIST_on_tree(BB_Find_Pred(BBLIST_item(succ), bb));
	union_groups (bb, BBLIST_item(succ));
      }
    }
  }

  for (bb = REGION_First_BB; bb; bb = BB_next(bb)) 
  {
    bb_succs = BB_succs( bb );
    if (!bb_succs) continue;

    FOR_ALL_BBLIST_ITEMS(bb_succs, succ){
      if (find_group (bb) != find_group (BBLIST_item(succ))){
	Set_BBLIST_on_tree(succ);
        Set_BBLIST_on_tree(BB_Find_Pred(BBLIST_item(succ), bb));
	union_groups (bb, BBLIST_item(succ));
      }
    }
  }
}

#ifndef  GCC_303
static void
output_gcov_string (
     const char *string,
     long delimiter)
{
  size_t temp;
  /* Write a delimiter to indicate that a file name follows.  */
  __write_long (delimiter, bb_file, 4);
  /* Write the string.  */
  temp = strlen (string) + 1;
  fwrite (string, temp, 1, bb_file);
  /* Append a few zeros, to align the output to a 4 byte boundary.  */
  temp = temp & 0x3;
  if (temp)
    {
      char c[4];
      c[0] = c[1] = c[2] = c[3] = 0;
      fwrite (c, sizeof (char), 4 - temp, bb_file);
    }
  /* Store another delimiter in the .bb file, just to make it easy to find
     the end of the file name.  */
  __write_long (delimiter, bb_file, 4);
}
#endif


/* Perform file-level initialization for gcov processing.  */

void
CG_Init_Gcov ()
{
  char *base_name = lbasename(Src_File_Name);
  int len = strlen (base_name);
  int i;

  name_pool_ptr = &name_pool;
  MEM_POOL_Initialize (name_pool_ptr, "Names", FALSE);
  MEM_POOL_Push (name_pool_ptr);

  if (flag_test_coverage == FALSE)
    return;

  last_bb_file_num = -1;
  bbg_file_name = CXX_NEW_ARRAY(char, len + strlen (GCOV_GRAPH_SUFFIX) + 1, name_pool_ptr);
  strcpy (bbg_file_name, base_name);
  for (i = len + strlen (GCOV_GRAPH_SUFFIX); i>0; i--)
    if (bbg_file_name[i] == '.')
      break;
  FmtAssert (bbg_file_name[i] == '.', ("Src_File_Name should associate with the prefix"));
  bbg_file_name[i] = 0;
  strcat (bbg_file_name, GCOV_GRAPH_SUFFIX);
  bbg_file = fopen (bbg_file_name, "wb");
  if (!bbg_file)
    FmtAssert (bbg_file != 0, ("Gcov Error: can't open %s", bbg_file_name));
#ifdef GCC_303
  if (gcov_write_unsigned (bbg_file, GCOV_GRAPH_MAGIC)
     || gcov_write_unsigned (bbg_file, GCOV_VERSION))
  {
    fclose (bbg_file);
    FmtAssert (0, ("cannot write `%s'", bbg_file_name));
  }
#else
  char* data_file = CXX_NEW_ARRAY(char, len + 4, name_pool_ptr);
  strcpy (data_file, base_name);
  for (i = len + 3; i>0; i--)
    if (data_file[i] == '.')
      break;
  FmtAssert (data_file[i] == '.', ("Src_File_Name should associate with the prefix"));
  data_file[i] = 0;
  strcat (data_file, ".bb");
  if ((bb_file = fopen (data_file, "wb")) == 0)
    FmtAssert (bb_file != 0, ("Gcov Error: can't open %s", data_file));
#endif

}

/* Performs file-level cleanup after branch-prob processing
   is completed.  */
void
CG_End_Gcov ()
{
  if (name_pool_ptr != NULL) {
    MEM_POOL_Pop (name_pool_ptr);
    MEM_POOL_Delete (name_pool_ptr);
    name_pool_ptr = NULL;
  }
  if (flag_test_coverage == FALSE)
    return;
  fclose (bbg_file);
#ifndef GCC_303
  fclose (bb_file);
#endif
}

void
CG_Gcov_Generation ()
{
  long offset;
  const char *name = Cur_PU_Name;
  int i;
  const char *fname = NULL;
  const char *dname;
  int         cur_bb_file_num, cur_bb_line_num;
  BBLIST* bb_succs;
  BBLIST *succ;
  BB *bb;
  OP *op;

  if (!Cur_PU_Name || strncmp(Cur_PU_Name, "_GLOBAL__GCOV_", strlen("_GLOBAL__GCOV_"))==0) 
    return;
  find_spanning_tree();
  CG_Compute_Checksum();
  last_bb_line_num = -1;
  if (flag_test_coverage == FALSE)
    return;
#ifdef GCC_303
  /* Announce function */
  if (gcov_write_unsigned (bbg_file, GCOV_TAG_FUNCTION)
      || !(offset = gcov_reserve_length (bbg_file))
      || gcov_write_string (bbg_file, name,
                             strlen (name))
      || gcov_write_unsigned (bbg_file,
                            chksum)
      || gcov_write_length (bbg_file, offset))
    goto bbg_error;
  /* Basic block flags */
  if (gcov_write_unsigned (bbg_file, GCOV_TAG_BLOCKS)
      || !(offset = gcov_reserve_length (bbg_file)))
    goto bbg_error;
  for (i = 0; i != (unsigned) (local_n_basic_blocks); i++)
    if (gcov_write_unsigned (bbg_file, 0))
      goto bbg_error;
  if (gcov_write_length (bbg_file, offset))
    goto bbg_error;

  /* Arcs */
  for (bb = REGION_First_BB; bb; bb = BB_next(bb)) 
  {
    bb_succs = BB_succs( bb );
    if (!bb_succs) continue;
    if (gcov_write_unsigned (bbg_file, GCOV_TAG_ARCS)
        || !(offset = gcov_reserve_length (bbg_file))
        || gcov_write_unsigned (bbg_file, BB_TO_GCOV_INDEX (bb)))
      goto bbg_error;

    FOR_ALL_BBLIST_ITEMS(bb_succs, succ){
      unsigned flag_bits = 0;
      if (BB_Fall_Thru_Successor(bb) == BBLIST_item(succ))
	flag_bits |= GCOV_ARC_FALLTHROUGH;
      else
	flag_bits |= GCOV_ARC_ON_TREE;
      if (gcov_write_unsigned (bbg_file, BB_TO_GCOV_INDEX (BBLIST_item(succ)))
          || gcov_write_unsigned (bbg_file, flag_bits))
        goto bbg_error;
    }
    if (gcov_write_length (bbg_file, offset))
      goto bbg_error;
  }

/* Output line number information about each basic block for
   GCOV utility.  */


  for (bb = REGION_First_BB; bb; bb = BB_next(bb)) {
    offset = 0;
    for (op = BB_first_op(bb); op != NULL; op = OP_next(op)){
      cur_bb_file_num = SRCPOS_filenum(OP_srcpos(op));
      cur_bb_line_num = Srcpos_To_Line(OP_srcpos(op));

      /* If this is a new source file, then output the
         file's name to the .bb file.  */
      if (last_bb_file_num == -1 || cur_bb_file_num != last_bb_file_num){
        IR_Srcpos_Filename(OP_srcpos(op), &fname, &dname);
        if (fname == NULL)
	  continue;
        if (offset)
          ;
        else if (gcov_write_unsigned (bbg_file, GCOV_TAG_LINES)
            || !(offset = gcov_reserve_length (bbg_file))
            || gcov_write_unsigned (bbg_file,
                                    BB_TO_GCOV_INDEX (bb)))
          goto bbg_error;

	last_bb_file_num = cur_bb_file_num;
        last_bb_line_num = -1;
        if (gcov_write_unsigned (bbg_file, 0)
            || gcov_write_string (bbg_file, fname,
                                  strlen (fname)))
          goto bbg_error;
      }
      else if (last_bb_line_num == -1 || cur_bb_line_num != last_bb_line_num){
        if (offset)
          ;
        else if (gcov_write_unsigned (bbg_file, GCOV_TAG_LINES)
            || !(offset = gcov_reserve_length (bbg_file))
            || gcov_write_unsigned (bbg_file,
                                    BB_TO_GCOV_INDEX (bb)))
          goto bbg_error;
      }

      if (last_bb_line_num == -1 || cur_bb_line_num != last_bb_line_num){
	last_bb_line_num = cur_bb_line_num;
        if (gcov_write_unsigned (bbg_file, cur_bb_line_num))
          goto bbg_error;
      }
    }
    if (offset)
    {
      if (gcov_write_unsigned (bbg_file, 0)
          || gcov_write_string (bbg_file, NULL, 0)
          || gcov_write_length (bbg_file, offset))
      {
        bbg_error:;
          FmtAssert(0, ("error writing `%s'", bbg_file_name));
          fclose (bbg_file);
          bbg_file = NULL;
      }
    }
  }
#else
  /* Start of a function.  */
  output_gcov_string (name, (long) -2);

  /* Output line number information about each basic block for
     GCOV utility.  */
  for (BB* bb = REGION_First_BB; bb; bb = BB_next(bb)) {

    /* Output a zero to the .bb file to indicate that a new
       block list is starting.  */
    //BOOL first_time = TRUE;
    __write_long (0, bb_file, 4);
    for (OP* op = BB_first_op(bb); op != NULL; op = OP_next(op)){
      cur_bb_file_num = SRCPOS_filenum(OP_srcpos(op));
      cur_bb_line_num = Srcpos_To_Line(OP_srcpos(op));

      if (!cur_bb_line_num)
        continue;

      /* If this is a new source file, then output the
         file's name to the .bb file.  */
      if (last_bb_file_num == -1 || cur_bb_file_num != last_bb_file_num){
        IR_Srcpos_Filename(OP_srcpos(op), &fname, &dname);
        if (fname == NULL)
	  continue;
	last_bb_file_num = cur_bb_file_num;
        last_bb_line_num = -1;
        //if (first_time == TRUE){
        //  __write_long (0, bb_file, 4);
        //  first_time = FALSE;
        //}
        output_gcov_string (fname,
                            (long)-1);
      }

      if (last_bb_line_num == -1 || cur_bb_line_num != last_bb_line_num){
	last_bb_line_num = cur_bb_line_num;
        //if (first_time == TRUE){
        //  __write_long (0, bb_file, 4);
        //  first_time = FALSE;
        //}
        __write_long (cur_bb_line_num, bb_file, 4);
      }
    }
  }
  __write_long (0, bb_file, 4);

  /* Create a .bbg file from which gcov can reconstruct the basic block
     graph.  First output the number of basic blocks, and then for every
     edge output the source and target basic block numbers.
     NOTE: The format of this file must be compatible with gcov.  */
  int flag_bits;

  __write_gcov_string (name, strlen (name), bbg_file, -1);

  /* write checksum.  */
  __write_long (chksum, bbg_file, 4);

  /* The plus 2 stands for entry and exit block.  */
  __write_long (local_n_basic_blocks+2, bbg_file, 4);
  __write_long (local_n_a_edges+2, bbg_file, 4);

  __write_long (1, bbg_file, 4);
  __write_long (BB_TO_GCOV_INDEX (REGION_First_BB), bbg_file, 4);
  __write_long (0x4, bbg_file, 4);

  for (BB* bb = REGION_First_BB; bb; bb = BB_next(bb)) {

    BBLIST* bb_succs = BB_succs( bb );
    long count = BBlist_Len( bb_succs );
    if (count == 0){
      __write_long (1, bbg_file, 4);
      __write_long (0, bbg_file, 4);
      __write_long (0x1, bbg_file, 4);
      continue;
    }
    __write_long (count, bbg_file, 4);

    BBLIST *succ;
    FOR_ALL_BBLIST_ITEMS(bb_succs, succ){
      flag_bits = 0;
      if (BBLIST_on_tree(succ))
	flag_bits |= 0x1;
      else if (BB_Fall_Thru_Successor(bb) == BBLIST_item(succ))
	flag_bits |= 0x4;
      __write_long (BB_TO_GCOV_INDEX (BBLIST_item(succ)), bbg_file, 4);
      __write_long (flag_bits, bbg_file, 4);
    } 
  }
  /* Emit fake loopback edge for EXIT block to maintain compatibility with
     old gcov format.  */

  __write_long (1, bbg_file, 4);
  __write_long (0, bbg_file, 4);
  __write_long (0x1, bbg_file, 4);

  /* Emit a -1 to separate the list of all edges from the list of
     loop back edges that follows.  */
  __write_long (-1, bbg_file, 4);
#endif
}

static BOOL 
Opnd_Tn_In_BB( BB* bb, REGISTER reg, unsigned char type )
{
  OP* opcode = bb->ops.first;
  int i;
  for( ; opcode != NULL; opcode = OP_next( opcode ) )
  {
    for ( i = 0; i < OP_opnds( opcode ); i++ )
    {
      TN *tn = OP_opnd( opcode,i );
      if ( type == 0 && TN_register_class(tn) == ISA_REGISTER_CLASS_integer && TN_register(tn) == reg )
         return TRUE;
      else if ( type == 1 && TN_register_class(tn) == ISA_REGISTER_CLASS_float && TN_register(tn) == reg )
         return TRUE;
    }
  }
  return FALSE;
}

// Return the sum of the procedure return register in BB bb
static INT32 
Get_Return_Reg_Sum(BB* bb)
{
#ifdef TARG_X8664
  if ( Opnd_Tn_In_BB( bb, RDX, 0 ) )
    return 2;

  if ( Opnd_Tn_In_BB( bb, RAX, 0 ) )
    return 1;
#endif

  return 0;
}
                                                                         
// return the sum of the procedure return float register in BB bb
static INT32 
Get_Float_Return_Reg_Sum(BB* bb)
{
#ifdef TARG_X8664
  for ( int i = 1 ; i >= 0; i-- )
  {
    if ( Opnd_Tn_In_BB( bb,XMM0 + i, 1) )
      return i + 1;
  }
#endif
  return 0;
}
static BOOL
Is_BB_Empty (BB *bb)
{
  for (OP *op = BB_first_op(bb); op != NULL; op = OP_next(op)) {
    if (OP_Real_Ops(op) != 0) return FALSE;
  }
  return TRUE;
}

static void
BB_Mov_Ops(BB* dest_bb, BB *src_bb, REGISTER reg, unsigned char type)
{
  int i;
  for (OP* op = BB_first_op(src_bb); op != NULL; op = OP_next(op))
  {
    for ( i = 0; i < OP_opnds( op ); i++ )
    {
      TN *tn = OP_opnd( op,i );
      if ( ((type == 0 && TN_register_class(tn) == ISA_REGISTER_CLASS_integer) || (type == 1 && TN_register_class(tn) == ISA_REGISTER_CLASS_float)) && 
	   TN_register(tn) == reg )
      {
	BB_Remove_Op( src_bb, op);
	FmtAssert( !Is_BB_Empty(src_bb), ("BB can not be empty!"));
	BB_Prepend_Op (dest_bb, op);
	return;
      }
    }
  }
}

static void 
Move_Save_Regs_OP(BB *instr_bb, BB *bb, INT32 ret_reg_num, INT32 f_ret_reg_num)
{
#ifdef TARG_X8664
  FmtAssert (ret_reg_num == 0 || f_ret_reg_num == 0, ("cannot be both integer and floating point at the same time"));
  if (ret_reg_num){
    BB_Mov_Ops(instr_bb, bb, RAX, 0);
    ret_reg_num --;
    if (ret_reg_num){
      BB_Mov_Ops(instr_bb, bb, RDX, 0);
    }
  }
  else if (f_ret_reg_num){
    int i = 0;
    while ( i<f_ret_reg_num ){
      BB_Mov_Ops(instr_bb, bb,  XMM0+i++, 1);
    }
  }
#endif
}

static void
Process_Arc_Profile_Region_Options ( void )
{
  OPTION_LIST *ol;
                                                                                                                                                             
  for ( ol = Arc_Profile_Region; ol != NULL; ol = OLIST_next(ol) ) {
        if ( strcmp ( OLIST_opt(ol), "profile_proc" ) == 0 )
                if ( strcmp (Cur_PU_Name, OLIST_val(ol) ) ==0 )
                        pu_flag = 1; 
                else
                        pu_flag = 0;
        if ( strcmp ( OLIST_opt(ol), "profile_id1" ) == 0 )
                begin_id = atoi ( OLIST_val (ol) );
        if ( strcmp ( OLIST_opt(ol), "profile_id2" ) == 0 )
                end_id = atoi ( OLIST_val (ol) );
  }
}
static BOOL 
Indirect_Branch(BB *bb)
{
  OP *br_op = BB_branch_op(bb);
  if (br_op) {
    INT tfirst, tcount;
    CGTARG_Branch_Info(br_op, &tfirst, &tcount);
    if (tcount == 0) 
      return TRUE;
  }
  return FALSE;
}

static BOOL
BB_Is_Unique_Instr_Predecessor(BB *src, BB *dest)
{
  if (BB_Is_Unique_Predecessor(src, dest))
    return TRUE;

  int instr_count = 0;
  
  BBLIST*  edge;
  FOR_ALL_BB_PREDS(src, edge) {
    if (!BBLIST_on_tree(edge))
      instr_count ++;
  }
  if (instr_count > 1){
    int instr_count_for_indirect_branch = 0;
    FOR_ALL_BB_PREDS(src, edge) {
      BB* bb_pred =  BBLIST_item(edge);
      if (!BBLIST_on_tree(edge) && Indirect_Branch(bb_pred))
	instr_count_for_indirect_branch++;
    }
    FmtAssert(instr_count_for_indirect_branch <= 1, ("one indirect branch and one direct branch going to the same basic block need to be instrumented?"));
    //return FALSE;

    if (instr_count_for_indirect_branch == 1){
      if (Indirect_Branch(dest))
	return TRUE;
      else
	return FALSE;
    }
    else  
      return FALSE;
  }
  else
    return TRUE;
}

void 
CG_Instrument_Arcs()
{
  if (!Cur_PU_Name || strncmp(Cur_PU_Name, "_GLOBAL__GCOV_", strlen("_GLOBAL__GCOV_"))==0) return;

  ST *st = get_symbol("LPBX2");
  if (st == NULL){
    st = New_ST(GLOBAL_SYMTAB);
    char* func_name = TYPE_MEM_POOL_ALLOC_N(char, name_pool_ptr, strlen("LPBX2") + strlen(lbasename(Src_File_Name)) + 1);
    sprintf(func_name, "%s%s", "LPBX2", lbasename(Src_File_Name));
    for(INT i = 0; func_name[i]; i++){
        if( !(    func_name[i] == '_'
              || ('0' <= func_name[i] && func_name[i] <= '9')
              || ('A' <= func_name[i] && func_name[i] <= 'Z')
              || ('a' <= func_name[i] && func_name[i] <= 'z')) )
            func_name[i] = '_';
    }

    ST_Init(st, Save_Str(func_name),
            CLASS_VAR, SCLASS_PSTATIC, EXPORT_PREEMPTIBLE, MTYPE_To_TY(Pointer_type));
    Set_ST_is_initialized(st);
    Set_ST_is_not_used(st);
  }

  Process_Arc_Profile_Region_Options ();
  if (pu_flag == 0) return;

  static int count = 0;
  BBLIST  *bb_succs;
  BBLIST *succ;
  BB *bb_succ;
  BB *bb;
  OP *op;
  OPS new_ops;
  TN *ld_result_tn;
  TN *ld_2nd_result_tn;
  TN *const_tn;
  TN *result_tn;
  TYPE_ID rtype;

  if (Is_Target_32bit())
    rtype = MTYPE_U4;
  else
    rtype = MTYPE_U8;

  if ((begin_id == -1 && end_id == -1) || (begin_id <= count && count <= end_id)) {
    OPS_Init(&new_ops);
    ld_result_tn = Build_TN_Of_Mtype(rtype);
    Exp_Load (rtype, rtype, ld_result_tn, st, 0, &new_ops, 0);
    ld_2nd_result_tn = Build_TN_Of_Mtype(rtype);
#if defined(TARG_MIPS) || defined(TARG_X8664) || defined(TARG_PPC32)
    Expand_Load( OPCODE_make_op (OPR_LDID, rtype, rtype),ld_2nd_result_tn, ld_result_tn, Gen_Literal_TN(count*8,4), &new_ops);
#else
    Expand_Load( OPCODE_make_op (OPR_LDID, rtype, rtype),ld_2nd_result_tn, ld_result_tn, Gen_Literal_TN(count*8,4), (VARIANT)0, &new_ops);
#endif
    const_tn = Gen_Literal_TN(1,4);
    result_tn = Build_TN_Of_Mtype(rtype);
    Exp_OP2 (OPC_U4ADD, result_tn, ld_2nd_result_tn, const_tn, &new_ops);
#if defined(TARG_MIPS) || defined(TARG_PPC32)
    Expand_Store (OPCODE_desc(OPCODE_make_op(OPR_STID, MTYPE_V, rtype)),result_tn, ld_result_tn, Gen_Literal_TN(count*8,4), &new_ops);
#else
    Expand_Store (OPCODE_desc(OPCODE_make_op(OPR_STID, MTYPE_V, rtype)),result_tn, ld_result_tn, Gen_Literal_TN(count*8,4), (VARIANT)0, &new_ops);
#endif
    Gcov_BB_Prepend_Ops(REGION_First_BB,  &new_ops);
  }
  count++;

  BB *exit_bb = NULL;
  for (bb = REGION_First_BB; bb; bb = BB_next(bb)){
    if (BB_exit(bb))
      exit_bb = bb;
  }
  // Bug 456
  if (!exit_bb){
    exit_bb = REGION_First_BB;
    while (BB_next(exit_bb)) exit_bb = BB_next(exit_bb);
  }
  FmtAssert(exit_bb, ("exit bb should exist!"));

  for (bb = REGION_First_BB; bb; bb = BB_next(bb)) 
  {
    bb_succs = BB_succs( bb );
    if (!bb_succs) continue;

    BBLIST *succ;
    BBLIST *old_succ;
    for (succ = bb_succs; succ!= NULL; succ = old_succ)
    {
      old_succ = BBLIST_next(succ);
      bb_succ = BBLIST_item(succ);
      if (!BBLIST_on_tree(succ))
      {
        if (begin_id != -1 && end_id != -1 && (begin_id > count || count > end_id)){
          count ++;
          continue;
        }
	OPS_Init(&new_ops);
	ld_result_tn = Build_TN_Of_Mtype(rtype);
	Exp_Load (rtype, rtype, ld_result_tn, st, 0, &new_ops, 0);
	ld_2nd_result_tn = Build_TN_Of_Mtype(rtype);
#if defined(TARG_MIPS) || defined(TARG_X8664) || defined(TARG_PPC32)
	Expand_Load( OPCODE_make_op (OPR_LDID, rtype, rtype),ld_2nd_result_tn, ld_result_tn, Gen_Literal_TN(count*8,4), &new_ops);
#else
	Expand_Load( OPCODE_make_op (OPR_LDID, rtype, rtype),ld_2nd_result_tn, ld_result_tn, Gen_Literal_TN(count*8,4), (VARIANT)0, &new_ops);
#endif
	const_tn = Gen_Literal_TN(1,4);
	result_tn = Build_TN_Of_Mtype(rtype);
	Exp_OP2 (OPC_U4ADD, result_tn, ld_2nd_result_tn, const_tn, &new_ops);
#if defined(TARG_MIPS) || defined(TARG_SL) || defined(TARG_PPC32)
	Expand_Store (OPCODE_desc(OPCODE_make_op(OPR_STID, MTYPE_V, rtype)),result_tn, ld_result_tn, Gen_Literal_TN(count*8,4), &new_ops);
#else
	Expand_Store (OPCODE_desc(OPCODE_make_op(OPR_STID, MTYPE_V, rtype)),result_tn, ld_result_tn, Gen_Literal_TN(count*8,4), (VARIANT)0, &new_ops);
#endif
        if (BB_Is_Unique_Instr_Predecessor(bb_succ, bb))
	{
	  Gcov_BB_Prepend_Ops(bb_succ, &new_ops);
	  count ++;
	  continue;
	}

	BB* instr_bb;
	if (BB_Fall_Thru_Successor(bb) == bb_succ)
	  instr_bb = Gen_And_Insert_BB_After(bb);
	else{
	  instr_bb = Gen_And_Insert_BB_After(exit_bb);
	  LABEL_IDX new_label = Gen_Label_For_BB(instr_bb);
	  OP *branch_op = BB_branch_op (bb);
	  INT tn_num = OP_opnds( branch_op );
	  TN* tgt_tn;
	  for( int i = 0; i<tn_num; i++ )
	  {
	    tgt_tn = OP_opnd( branch_op, i );
	    if( TN_is_label( tgt_tn ) )
	    {
	      Set_TN_label( tgt_tn, new_label );
	      break;
	    }
	  }
	  // tgt_label is the branch target bb's label
	  LABEL_IDX tgt_label;
	  tgt_label = Gen_Label_For_BB( bb_succ );
#ifdef TARG_X8664
	  Build_OP( TOP_jmp, Gen_Label_TN(tgt_label, 0), &new_ops);
#elif defined(TARG_PPC32)
	  Build_OP( TOP_ba, Gen_Label_TN(tgt_label, 0), &new_ops);
#elif TARG_MIPS
	  // mips
	  Build_OP( TOP_j, Gen_Label_TN(tgt_label, 0), &new_ops);
#else
#ifndef TARG_LOONGSON
          // ia64
          Build_OP (TOP_br, Gen_Enum_TN(ECV_ph_few), Gen_Enum_TN(ECV_dh), Gen_Label_TN(tgt_label, 0), &new_ops);
#endif
#endif
	  FmtAssert(TN_is_label( tgt_tn ), ("should be branch target label"));
	}

	Gcov_BB_Prepend_Ops(instr_bb, &new_ops);
	if (BB_call( bb )){
	  int ret_reg_num = Get_Return_Reg_Sum( bb_succ );
	  int f_ret_reg_num = Get_Float_Return_Reg_Sum( bb_succ );
	  Move_Save_Regs_OP(instr_bb, bb_succ, ret_reg_num, f_ret_reg_num);
	}
	Unlink_Pred_Succ(bb, bb_succ);
	Link_Pred_Succ(bb, instr_bb);
	Set_BBLIST_on_tree( BBlist_Add_BB (&BB_succs(bb), instr_bb) );
        Set_BBLIST_on_tree( BBlist_Add_BB (&BB_preds(instr_bb), bb) );
	Link_Pred_Succ(instr_bb, bb_succ);
	Set_BBLIST_on_tree( BBlist_Add_BB (&BB_succs(instr_bb), bb_succ) );
	Set_BBLIST_on_tree( BBlist_Add_BB (&BB_preds(bb_succ), instr_bb) );
	count++;
      }
    }
  }
}

