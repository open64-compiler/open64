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


/* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */
// ====================================================================
// ====================================================================
//
// Module: ipa_preopt.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/ipa/main/analyze/ipa_preopt.cxx,v $
//
// Revision history:
//  7-15-98 - Original Version
//
// Description:
//   Infrastructure for selectively calling PREOPT from IPA
//
// ====================================================================
// ====================================================================

#include <stdint.h>
#include <dlfcn.h>                      // sgidladd, dlerror

#ifndef BACK_END
#define BACK_END                        // config.h needs it
#endif
#include "be_symtab.h"                  // BE_ST
#include "config.h"                     // Run_preopt, Run_ipl
#include "dso.h"                        // load_so
#include "erglob.h"                     // ErrMsg
#include "glob.h"                       // Show_Progress
#include "ir_reader.h"                  // fdump_tree
#include "ipa_cg.h"                     // IPA_NODE, IPA_EDGE, IPA_CALL_GRAPH
#include "ipa_option.h"                 // IPA_TRACE_* flags
#include "ipa_summary.h"                // IPA_get_*_file_array 
#include "ipa_section_annot.h"          // SECTION_FILE_ANNOT
#include "ipl_array_bread_write.h"      // ARRAY_SUMMARY_OUTPUT
#include "ipl_main.h"                   // Do_Par
#include "ipl_driver.h"                 // Ipl_Init_From_Ipa
#define IPA_SUMMARY                     // SUMMARIZE<IPL>
#include "ipl_summarize.h"              // SUMMARY
#undef IPA_SUMMARY
#include "ipo_const.h"                  // IPO_propagate_globals
#include "ipo_defs.h"                   // IPO_NODE_CONTEXT
#include "optimizer.h"                  // Pre_Optimizer
#include "region_main.h"                // REGION_Initialize
#include "ipa_main.h"
#include "ipa_nystrom_alias_analyzer.h"

// --- from wopt.so
#pragma weak Create_Du_Manager
#pragma weak Delete_Du_Manager
#pragma weak Pre_Optimizer

// --- from ipl.so
#pragma weak Array_Summary_Output
#pragma weak Do_Par
#pragma weak Ipl_Init_From_Ipa
#pragma weak Summary
#pragma weak Trace__20ARRAY_SUMMARY_OUTPUTGP8__file_s
#pragma weak Trace__31SUMMARIZE__pt__14_XC7PROGRAML10GP8__file_s


class ST_IDX_PAIR_TO_INT32_HASH_TABLE
{
private:
  struct HASH_ELEMENT 
  {
    ST_IDX _symbol_st_idx;
    ST_IDX _func_st_idx;
    INT32  _symbol_index;
    HASH_ELEMENT* _next;
  } *_table[0x40];

  MEM_POOL* _mem_pool;

  UINT32 hash(ST_IDX st_idx, ST_IDX func_st_idx) const 
  {
    return ((ST_IDX_index(st_idx) | ST_IDX_index(func_st_idx)) & 0xff) >> 2;
  }

public:
  ST_IDX_PAIR_TO_INT32_HASH_TABLE (MEM_POOL* mem_pool) :
    _mem_pool (mem_pool)
  {
    bzero(_table, sizeof(_table));
  }
 
  void Enter (ST_IDX symbol_st_idx, ST_IDX func_st_idx, INT32 symbol_index) 
  {
    HASH_ELEMENT* p = CXX_NEW (HASH_ELEMENT, _mem_pool);
    UINT32 hash_value = hash (symbol_st_idx, func_st_idx);
    p->_symbol_st_idx = symbol_st_idx;
    p->_func_st_idx = func_st_idx;
    p->_symbol_index = symbol_index;
    p->_next = _table[hash_value];
    _table[hash_value] = p;
  }

  INT32 Find (ST_IDX symbol_st_idx, ST_IDX func_st_idx) const 
  {
    UINT32 hash_value = hash (symbol_st_idx, func_st_idx);
    HASH_ELEMENT* p = _table[hash_value];
    while (p) {
      if (p->_symbol_st_idx == symbol_st_idx && 
          p->_func_st_idx == func_st_idx) {
        return p->_symbol_index;
      }
      p = p->_next;
    }
    return -1;
  }
    
}; 


struct AUX_FILE_INFO
{
  ST_IDX_PAIR_TO_INT32_HASH_TABLE* symbol_index_map;
  INT32 max_symbol_size;
};


static MEM_POOL IPA_preopt_pool;
static BOOL IPA_preopt_initialized = FALSE;
static AUX_FILE_INFO* Aux_file_info;


// --------------------------------------------------------------
// Build a hash-table to speed up the mapping of preopt-generated 
// symbol indices into the old file-based SYMMARY_SYMBOL array
// --------------------------------------------------------------
static void
IPA_build_symbol_index_map (const IPA_NODE* node)
{
  AUX_FILE_INFO& file_info = Aux_file_info[node->File_Index()];

  // do this only once per file, rather than for each node
  if (file_info.symbol_index_map == 0) {
    
    file_info.symbol_index_map = 
      CXX_NEW (ST_IDX_PAIR_TO_INT32_HASH_TABLE (&IPA_preopt_pool),
               &IPA_preopt_pool);
  
    INT32 num_symbols;
    SUMMARY_SYMBOL* symbols = 
      IPA_get_symbol_file_array(node->File_Header(), num_symbols);

    // has each symbol based on its st_idx and that of its enclosing
    // function (0 for globals)
    for (INT32 i = 0; i < num_symbols; ++i) {
      ST_IDX symbol_st_idx = symbols[i].St_idx();
      ST_IDX func_st_idx = (ST_IDX_level(symbol_st_idx) == GLOBAL_SYMTAB) ?
                           0 : symbols[i].Get_st_idx_func();
      file_info.symbol_index_map->Enter (symbol_st_idx, func_st_idx, i);
    }
  }
}
  

// --------------------------------------------------------------
// Add given symbol to the array of SUMMARY_SYMBOLS in the file
// that contains given node and adjust summary header information
// --------------------------------------------------------------
static INT32 
IPA_add_new_symbol (const IPA_NODE* node,
                    const SUMMARY_SYMBOL& symbol)
{
  AUX_FILE_INFO& file_info = Aux_file_info[node->File_Index()];
  IP_FILE_HDR& file_hdr = node->File_Header();
  SUMMARY_FILE_HEADER* summary_header = IP_FILE_HDR_file_header (file_hdr);

  INT32 num_syms;
  SUMMARY_SYMBOL* old_symbols = IPA_get_symbol_file_array (file_hdr, num_syms);
  SUMMARY_SYMBOL* new_symbols;
  INT32 num_bytes = num_syms * sizeof(SUMMARY_SYMBOL);
  
  // If max_symbol_size is 0, we are extending the array for the first time
  if (file_info.max_symbol_size == 0) {
    file_info.max_symbol_size = num_syms * 2;
    new_symbols = (SUMMARY_SYMBOL*) 
      MEM_POOL_Alloc (Malloc_Mem_Pool, num_bytes * 2);
    memcpy (new_symbols, old_symbols, num_bytes);
    Elf64_Word new_offset = summary_header->Get_symbol_offset() +
                            ((char*) new_symbols - (char*) old_symbols);
    summary_header->Set_symbol_offset (new_offset);
  }
  // Reallocating when the array is extended more than once
  else if (file_info.max_symbol_size <= num_syms) {
    file_info.max_symbol_size = num_syms * 2;
    new_symbols = (SUMMARY_SYMBOL*) 
      MEM_POOL_Realloc (Malloc_Mem_Pool, old_symbols, num_bytes, num_bytes*2);
    Elf64_Word new_offset = summary_header->Get_symbol_offset() +
                            ((char*) new_symbols - (char*) old_symbols);
    summary_header->Set_symbol_offset (new_offset);
  }
  else {
    new_symbols = old_symbols;
  }
  
  new_symbols[num_syms] = symbol;
  summary_header->Set_symbol_size(num_syms + 1);

  return num_syms;
}


// ---------------------------------------------------------------
// Given a new, preopt-generated SUMMARY_SYMBOL, find the index of
// the matching symbol in the old file-based SUMMARY_SYMBOL array
// ---------------------------------------------------------------
static INT32
IPA_map_symbol_index (const IPA_NODE* node,
                      const SUMMARY_SYMBOL& symbol)
{
  AUX_FILE_INFO& file_info = Aux_file_info[node->File_Index()];
  Is_True (file_info.symbol_index_map,
           ("Hash table for symbol to index mapping is not initialized"));
  
  ST_IDX symbol_st_idx = symbol.St_idx();
  ST_IDX func_st_idx = (ST_IDX_level(symbol_st_idx) == GLOBAL_SYMTAB) ?
                       0 : symbol.Get_st_idx_func();

  // look-up the symbol using its st_idx and that of the enclosing function
  // if not found, add it to the SUMMARY_SYMBOL array and enter in the table
  INT32 sym_idx = file_info.symbol_index_map->Find(symbol_st_idx, func_st_idx);
  if (sym_idx == -1) {
    sym_idx = IPA_add_new_symbol (node, symbol); 
    file_info.symbol_index_map->Enter (symbol_st_idx, func_st_idx, sym_idx);
  }
  
  return sym_idx;
}
  

// ----------------------------------------------------------
// - Update symbol indices in preopt-generated SUMMARY_FORMALs,
//   so that they point into file-based SUMMARY_SYMBOL array
// - Update formal indices in SUMMARY_SYMBOL file array so 
//   that they point to preopt-generated SUMMARY_FORMALs
// ----------------------------------------------------------
static void
IPA_update_formal_symbol_indices (const IPA_NODE* node,
                                  SUMMARY* new_summary)
{
  SUMMARY_FORMAL* old_formals = IPA_get_formal_array(node) + 
                                node->Summary_Proc()->Get_formal_index();

  SUMMARY_SYMBOL* new_symbols = new_summary->Get_symbol(0);
  SUMMARY_FORMAL* new_formals = new_summary->Get_formal(0);
  UINT num_formals = node->Num_Formals();

  for (UINT i = 0; i < num_formals; ++i) {
    INT32 new_sym_idx = new_formals[i].Get_symbol_index();
    if (new_sym_idx != -1) {
      INT32 old_sym_idx = IPA_map_symbol_index(node, new_symbols[new_sym_idx]);
      new_formals[i].Set_symbol_index (old_sym_idx);
      IPA_get_symbol_array(node)[old_sym_idx].Set_findex(i);
    }
  }
}


// ---------------------------------------------------------
// Update symbol indices in preopt-generated SUMMARY_ACTUALs
// so that they point into file-based SUMMARY_SYMBOL array
// ---------------------------------------------------------
static void
IPA_update_actual_symbol_indices (const IPA_NODE* node,
                                  SUMMARY* new_summary)
{
  SUMMARY_FORMAL* old_formals = IPA_get_formal_array(node) + 
                                node->Summary_Proc()->Get_formal_index();

  SUMMARY_SYMBOL* new_symbols = new_summary->Get_symbol(0);
  SUMMARY_ACTUAL* new_actuals = new_summary->Get_actual(0);
  INT32 num_new_actuals = new_summary->Get_actual_idx() + 1;
  
  for (INT32 i = 0; i < num_new_actuals; ++i) {
    INT32 new_sym_idx = new_actuals[i].Get_symbol_index();
    if (new_sym_idx != -1) {
      INT32 old_sym_idx = IPA_map_symbol_index(node, new_symbols[new_sym_idx]);
      new_actuals[i].Set_symbol_index (old_sym_idx);
    }
  }
}


// ---------------------------------------------------------
// update symbol indices in preopt-generated SUMMARY_GLOBALs
// so that they point into file-based SUMMARY_SYMBOL array
// ---------------------------------------------------------
static void
IPA_update_global_symbol_indices (const IPA_NODE* node,
                                  SUMMARY* new_summary)
{
  SUMMARY_SYMBOL* new_symbols = new_summary->Get_symbol(0);
  SUMMARY_GLOBAL* new_globals = new_summary->Get_global(0);
  INT32 num_new_globals = new_summary->Get_global_idx() + 1;

  for (INT32 i = 0; i < num_new_globals; ++i) {
    INT32 new_sym_idx = new_globals[i].Get_symbol_index();
    Is_True (new_sym_idx != -1, ("Invalid symbol index in SUMMARY_GLOBAL"));
    INT32 old_sym_idx = IPA_map_symbol_index(node, new_symbols[new_sym_idx]);
    new_globals[i].Set_symbol_index (old_sym_idx);
  }
}

    
// -----------------------------------------------------------
// Update symbol indices in preopt-generated SUMMARY_CALLSITEs
// so that they point into file-based SUMMARY_SYMBOL array
// -----------------------------------------------------------
static void
IPA_update_callsite_symbol_indices (const IPA_NODE* node,
                                    SUMMARY* new_summary)
{
  SUMMARY_SYMBOL* new_symbols = new_summary->Get_symbol(0);
  SUMMARY_CALLSITE* new_callsites = new_summary->Get_callsite(0);
  INT32 num_new_callsites = new_summary->Get_callsite_idx() + 1;
  
  for (INT32 i = 0; i < num_new_callsites; ++i) {
    if (!new_callsites[i].Is_func_ptr()) {
      INT32 new_sym_idx = new_callsites[i].Get_symbol_index();
      Is_True(new_sym_idx != -1, ("Invalid symbol index in SUMMARY_CALLSITE"));
      INT32 old_sym_idx = IPA_map_symbol_index(node, new_symbols[new_sym_idx]);
      new_callsites[i].Set_symbol_index (old_sym_idx);
    }
  }
}


// -------------------------------------------------------
// Update symbol indices in preopt-generated SUMMARY_STIDs
// so that they point into file-based SUMMARY_SYMBOL array
// -------------------------------------------------------
static void
IPA_update_stid_symbol_indices (const IPA_NODE* node,
                                SUMMARY* new_summary)
{
  SUMMARY_SYMBOL* new_symbols = new_summary->Get_symbol(0);
  SUMMARY_STID* new_stids = new_summary->Get_global_stid(0);
  INT32 num_new_stids = new_summary->Get_global_stid_idx() + 1;
  
  for (INT32 i = 0; i < num_new_stids; ++i) {
    INT32 new_sym_idx = new_stids[i].Get_symbol_index();
    Is_True (new_sym_idx != -1, ("Invalid symbol index in SUMMARY_STID"));
    INT32 old_sym_idx = IPA_map_symbol_index(node, new_symbols[new_sym_idx]);
    new_stids[i].Set_symbol_index (old_sym_idx);
  }
}


// --------------------------------------------------------
// Update symbol indices in preopt-generated SUMMARY_VALUEs
// so that they point into file-based SUMMARY_SYMBOL array
// --------------------------------------------------------
static void
IPA_update_value_symbol_indices (const IPA_NODE* node,
                                 SUMMARY* new_summary)
{
  SUMMARY_SYMBOL* new_symbols = new_summary->Get_symbol(0);
  SUMMARY_VALUE* new_values = new_summary->Get_value(0);
  INT32 num_new_values = new_summary->Get_value_idx() + 1;
  
  for (INT32 i = 0; i < num_new_values; ++i) {
    if (new_values[i].Is_global()) {
      INT32 new_sym_idx = new_values[i].Get_global_index();
      if (new_sym_idx != -1) {
        INT32 old_sym_idx=IPA_map_symbol_index(node,new_symbols[new_sym_idx]);
        new_values[i].Set_global_index (old_sym_idx);
      }
    }
    else if (new_values[i].Is_symbol()) {
      INT32 new_sym_idx = new_values[i].Get_symbol_index();
      Is_True (new_sym_idx != -1, ("Invalid symbol index in SUMMARY_VALUE"));
      INT32 old_sym_idx = IPA_map_symbol_index(node, new_symbols[new_sym_idx]);
      new_values[i].Set_symbol_index (old_sym_idx);
    }
  }
}


// -------------------------------------------------------
// Update symbol indices in preopt-generated SUMMARY_CHIs
// so that they point into file-based SUMMARY_SYMBOL array
// -------------------------------------------------------
static void
IPA_update_chi_symbol_indices (const IPA_NODE* node,
                               SUMMARY* new_summary)
{
  SUMMARY_SYMBOL* new_symbols = new_summary->Get_symbol(0);
  SUMMARY_CHI* new_chis = new_summary->Get_chi(0);
  INT32 num_new_chis = new_summary->Get_chi_idx() + 1;
  
  for (INT32 i = 0; i < num_new_chis; ++i) {
    INT32 new_sym_idx = new_chis[i].Get_symbol_index();
    Is_True (new_sym_idx != -1, ("Invalid symbol index in SUMMARY_CHI"));
    INT32 old_sym_idx = IPA_map_symbol_index(node, new_symbols[new_sym_idx]);
    new_chis[i].Set_symbol_index (old_sym_idx);
  }
}


// ----------------------------------------------------------
// Update symbol indices in preopt-generated REGION_ARRAYS
// so that they point into file-based SUMMARY_SYMBOL array
// ----------------------------------------------------------
static void
IPA_update_region_symbol_indices (const IPA_NODE* node,
                                  SUMMARY* new_summary,
                                  ARRAY_SUMMARY_OUTPUT* new_array_summary)
{
  SUMMARY_FORMAL* old_formals = IPA_get_formal_array(node) + 
                                node->Summary_Proc()->Get_formal_index();

  SUMMARY_SYMBOL* new_symbols = new_summary->Get_symbol(0);
  REGION_ARRAYS* new_regions = new_array_summary->Get_region_array(0);
  INT32 num_new_regions = new_array_summary->Get_region_count() + 1;
  
  for (INT32 i = 0; i < num_new_regions; ++i) {
    INT32 new_sym_idx = new_regions[i].Get_sym_id();
    Is_True (new_sym_idx != -1, ("Invalid symbol index in REGION_ARRAYS"));
    INT32 old_sym_idx = IPA_map_symbol_index (node, new_symbols[new_sym_idx]);
    new_regions[i].Set_sym_id (old_sym_idx);
  }
}


// -------------------------------------------------------
// Update symbol indices in preopt-generated SCALAR_INFOs
// so that they point into file-based SUMMARY_SYMBOL array
// -------------------------------------------------------
static void
IPA_update_scalar_symbol_indices (const IPA_NODE* node,
                                  SUMMARY* new_summary,
                                  ARRAY_SUMMARY_OUTPUT* new_array_summary)
{
  SUMMARY_SYMBOL* new_symbols = new_summary->Get_symbol(0);
  SCALAR_INFO* new_scalars = new_array_summary->Get_scalars(0);
  INT32 num_new_scalars = new_array_summary->Get_scalars_count() + 1;
  
  for (INT32 i = 0; i < num_new_scalars; ++i) {
    INT32 new_sym_idx = new_scalars[i].Get_id();
    Is_True (new_sym_idx != -1, ("Invalid symbol index in SCALAR_INFO"));
    INT32 old_sym_idx = IPA_map_symbol_index (node, new_symbols[new_sym_idx]);
    new_scalars[i].Set_id (old_sym_idx);
  }
}


// --------------------------------------------------------------
// For preopt-generated TERMs update IVAR and IVAR_GLOBAL indices 
// and their symbol indices so that point into file-based arrays
// --------------------------------------------------------------
static void
IPA_update_terms (const IPA_NODE* node,
                  ARRAY_SUMMARY_OUTPUT* new_array_summary,
                  SECTION_FILE_ANNOT* section_file_annot)
{
  IVAR* new_ivars = new_array_summary->Get_ivar(0);
  TERM* new_terms = new_array_summary->Get_term(0);
  INT32 num_new_terms = new_array_summary->Get_term_count() + 1;
  
  for (INT32 i = 0; i < num_new_terms; ++i) {
    if (new_terms[i].Get_type() == LTKIND_IV) {
      const IVAR& ivar = new_ivars[new_terms[i].Get_desc()];
      INT ivar_idx = section_file_annot->Find_ivar(node, ivar);
      if (ivar_idx == -1) {
        ivar_idx = section_file_annot->Add_ivar(node, ivar);
      }
      new_terms[i].Set_desc(ivar_idx);
    }
  }
}


// ------------------------------------------------------
// Change indices and counts in SUMMARY_PROCEDURE so that 
// they point into preopt-generated summary info arrays
// ------------------------------------------------------
static void
IPA_update_procedure (IPA_NODE* node,
                      SUMMARY* new_summary)
{
  // update the symbol index in the new SUMMARY_PROCEDURE
  SUMMARY_SYMBOL* new_symbols = new_summary->Get_symbol(0);
  SUMMARY_PROCEDURE* new_proc = new_summary->Get_procedure(0);
  INT32 new_sym_idx = new_proc->Get_symbol_index();
  Is_True (new_sym_idx != -1, ("Invalid symbol index in SUMMARY_PROCEDURE"));
  INT32 old_sym_idx = IPA_map_symbol_index(node, new_symbols[new_sym_idx]);
  new_proc->Set_symbol_index (old_sym_idx);

#ifdef KEY
  new_proc->Set_size (PU_WN_BB_Cnt, PU_WN_Stmt_Cnt, new_proc->Get_call_count());
#endif

  // copy everything from the new SUMMARY_PROCEDURE to the old one
  *(node->Summary_Proc()) = *new_proc;
}

  
// -------------------------------------------------------------------
// One-time initialization needs to do the following:
//   - dlopen wopt.so and ipl.so so
//   - initialize BE_SYMTAB resources
//   - initialize mempool for newly genereated summaries
//   - allocate and initialize auxiliary array (one elment per file)
// -------------------------------------------------------------------
static void
IPA_preopt_initialize ()
{
  load_so ("wopt.so", WOPT_Path, Show_Progress);
  load_so ("ipl.so", Ipl_Path, Show_Progress);

  MEM_POOL_Initialize (&IPA_preopt_pool, "IPA preopt pool", FALSE);
  MEM_POOL_Push (&IPA_preopt_pool);

  UINT32 bytes = IP_File_header.size() * sizeof(AUX_FILE_INFO);
  Aux_file_info = (AUX_FILE_INFO*) MEM_POOL_Alloc (&IPA_preopt_pool, bytes);
  bzero (Aux_file_info, bytes);
}


// -------------------------------------------------------------------
// One-time cleanup after preopt has been called on one or more nodes:
//   - free BE_SYMTAB resources
//   - eliminate functions that have become dead after preopt
// -------------------------------------------------------------------
void
IPA_Preopt_Finalize ()
{
  if (IPA_preopt_initialized) {

    // delete dead functions, but do not update modref counts
    // for global variables (651823)
    Eliminate_Dead_Func(FALSE);

    MEM_POOL_Pop (&IPA_preopt_pool);
    MEM_POOL_Delete (&IPA_preopt_pool);

    IPA_preopt_initialized = FALSE;
  }
}

#ifdef KEY
// Computes PU size after calling preopt
// Modifies: PU_WN_BB_Cnt, PU_WN_Stmt_Cnt, PU_WN_Call_Cnt
// Modeled after Count_tree_size()
static void
Compute_PU_Size (WN * wn)
{
  if (!wn) return;

  WN * wn2;

  switch (WN_operator(wn)) {

    case OPR_BLOCK:
      wn2 = WN_first(wn);
      while (wn2) {
        Compute_PU_Size(wn2);
        wn2 = WN_next(wn2);
      }
      break;

    case OPR_REGION:
      Compute_PU_Size(WN_region_body(wn));
      break;

    case OPR_IF:
      Compute_PU_Size(WN_if_test(wn));
      if (WN_then(wn))
        Compute_PU_Size(WN_then(wn));
      if (WN_else(wn))
        Compute_PU_Size(WN_else(wn));
      break;

    case OPR_DO_LOOP:
      Compute_PU_Size(WN_start(wn));
      Compute_PU_Size(WN_step(wn));
      Compute_PU_Size(WN_end(wn));
      Compute_PU_Size(WN_do_body(wn));
      break;

    case OPR_WHILE_DO:
    case OPR_DO_WHILE:
      Compute_PU_Size(WN_while_test(wn));
      Compute_PU_Size(WN_while_body(wn));
      break;

    case OPR_SWITCH:
    case OPR_COMPGOTO:
    case OPR_XGOTO:
      {
      WN *targ_blk = WN_kid1(wn);
      wn2 = WN_first(targ_blk);
      INT t = WN_num_entries(wn) - 1;
      for ( ; t >= 0; --t, wn2 = WN_next(wn2) )
          Compute_PU_Size(wn2);
      break;
      }

    default:
      {
      INT i;
      for (i = 0; i < WN_kid_count(wn); i++) {
          wn2 = WN_kid(wn,i);
          if (wn2)
              Compute_PU_Size(wn2);
        }
      }
    }
    Count_WN_Operator (WN_operator (wn), WN_rtype (wn) , PU_WN_BB_Cnt, PU_WN_Stmt_Cnt, PU_WN_Call_Cnt);
}
#endif // KEY

// -------------------------------------------------------------
// When constants are discovered in IPA, propagate them into PU,
// call preopt to clean things up, and rebuild array sections.
// -------------------------------------------------------------
void
IPA_Preoptimize (IPA_NODE* node)
{
  Is_True (!node->Is_Quasi_Clone(), ("Quasi-clones cannot be preoptimized!"));
  
  // We cannot call preopt on nested PUs because IPL requires
  // that their parent PUs be processed first (652328)
  if (node->Is_Nested_PU() || node->Summary_Proc()->Is_alt_entry()) {
    return;
  }
  
  // do one-time initialization if necessary
  if (!IPA_preopt_initialized) {
    IPA_preopt_initialize();
    IPA_preopt_initialized = TRUE;
  }
  
  IPA_NODE_CONTEXT context(node);

  // Generate constant assignment statements
  IPO_propagate_globals (node);		// globals
  IPA_Propagate_Constants (node, FALSE); // formals

  WN* wn = node->Whirl_Tree();

  if (Get_Trace(TP_IPA, IPA_TRACE_PREOPT_IPL)) {
    fprintf (TFile, "\n%s before preopt\n", node->Name());
    fdump_tree (TFile, wn);
  }

  REGION_Initialize (wn, PU_has_region(node->Get_PU()));

  MEM_POOL_Push (&MEM_local_pool);

  Set_Error_Phase ("IPA Global Optimizer");

  Ipl_Init_From_Ipa (Malloc_Mem_Pool);

  IPL_SUMMARY_PTRS* summary_ptrs = 
    CXX_NEW (IPL_SUMMARY_PTRS (Summary, Array_Summary_Output),
             Malloc_Mem_Pool);

  // run preopt and from within it call ipl summary phase
  Run_preopt = TRUE;
  Run_ipl = TRUE;
#ifndef KEY
  Do_Par = TRUE;
#endif

  BE_symtab_alloc_scope_level(CURRENT_SYMTAB);
  Scope_tab[CURRENT_SYMTAB].st_tab->
    Register(*Be_scope_tab[CURRENT_SYMTAB].be_st_tab);
  Scope_tab[CURRENT_SYMTAB].preg_tab->Register(Be_preg_tab);

  PU_adjust_addr_flags(Get_Current_PU_ST(), wn);

  DU_MANAGER* du_mgr = Create_Du_Manager (MEM_pu_nz_pool_ptr);

  // tell alias analyzer, current nystrom alias analyzer
  // is crated for IPA
  if (Alias_Nystrom_Analyzer) {
    Alias_Analyzer_in_IPA = TRUE;
    UINT16 fileIdx = (UINT16)(node->File_Index());
    UINT16 puIdx   = (UINT16)(node->Proc_Info_Index());
    Current_IPANode_File_PU_Idx = (fileIdx << 16) | puIdx;

    IPA_NystromAliasAnalyzer *ipan = IPA_NystromAliasAnalyzer::aliasAnalyzer();
    ConstraintGraph::IPANodeCG(ipan->cg(node->Node_Index()));

    IPA_NystromAliasAnalyzer::aliasAnalyzer()->mapWNToUniqCallSiteCGNodeId(node);
  }
  ALIAS_MANAGER* alias_mgr = Create_Alias_Manager (MEM_pu_nz_pool_ptr, wn);

  // call the preopt, which then calls Perform_Procedure_Summary_Phase
  WN* opt_wn = Pre_Optimizer (PREOPT_IPA1_PHASE, wn, du_mgr, alias_mgr);

  Delete_Du_Manager (du_mgr, MEM_pu_nz_pool_ptr);
  Delete_Alias_Manager (alias_mgr, MEM_pu_nz_pool_ptr);

  Scope_tab[CURRENT_SYMTAB].preg_tab->Un_register(Be_preg_tab);
  Be_preg_tab.Clear();
  Scope_tab[CURRENT_SYMTAB].st_tab->
    Un_register(*Be_scope_tab[CURRENT_SYMTAB].be_st_tab);
  Be_scope_tab[CURRENT_SYMTAB].be_st_tab->Clear();

  REGION_Finalize();

#ifdef KEY
  // Don't reset all PU stats
  PU_WN_BB_Cnt = PU_WN_Stmt_Cnt = PU_WN_Call_Cnt = 0;
  Compute_PU_Size (opt_wn);
#endif

  if (Get_Trace(TP_IPA, IPA_TRACE_PREOPT_IPL)) {
    fprintf (TFile, "\n%s after preopt\n", node->Name());
    fdump_tree (TFile, opt_wn);
    Summary->Trace (TFile);
    Array_Summary_Output->Trace (TFile);
  }

#if Is_True_On
  WN_verifier(opt_wn);
#endif

  if (Summary->Has_symbol_entry()) {
    IPA_build_symbol_index_map (node);
  }
  
  if (Summary->Has_formal_entry()) {
    IPA_update_formal_symbol_indices (node, Summary);
  }
  if (Summary->Has_actual_entry()) {
    IPA_update_actual_symbol_indices (node, Summary);
  }
  if (Summary->Has_global_entry()) {
    IPA_update_global_symbol_indices (node, Summary);
  }
  if (Summary->Has_callsite_entry()) {
    IPA_update_callsite_symbol_indices (node, Summary);
  }

#define _IPA_ITERATE_PREOPT_
#ifdef _IPA_ITERATE_PREOPT_
  if (Summary->Has_global_stid_entry()) {
    IPA_update_stid_symbol_indices (node, Summary);
  }
  if (Summary->Has_value_entry()) {
    IPA_update_value_symbol_indices (node, Summary);
  }
  if (Summary->Has_chi_entry()) {
    IPA_update_chi_symbol_indices (node, Summary);
  }
#endif // _IPA_ITERATE_PREOPT_

  SECTION_FILE_ANNOT* section_annot = 
    IP_FILE_HDR_section_annot(node->File_Header());
  if (section_annot && Array_Summary_Output->Get_term_count() != -1) {
    IPA_update_terms (node, Array_Summary_Output, section_annot);
  }
  if (Array_Summary_Output->Get_region_count() != -1) {
    IPA_update_region_symbol_indices (node, Summary, Array_Summary_Output);
  }
  if (Array_Summary_Output->Get_scalars_count() != -1) {
    IPA_update_scalar_symbol_indices (node, Summary, Array_Summary_Output);
  }

  IPA_update_procedure (node, Summary);
  
  // update the node info and its edges in the call graph
  IPA_Call_Graph->Update_Node_After_Preopt (node, 
                                            opt_wn,
                                            Summary->Has_callsite_entry() ?
                                            Summary->Get_callsite(0) : NULL,
                                            summary_ptrs);

  MEM_POOL_Pop (&MEM_local_pool);
}
