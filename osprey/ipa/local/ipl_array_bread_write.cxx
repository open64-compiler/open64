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

#include <stdint.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>                        // Elf64_Word
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>                        // Elf64_Word
#endif /* defined(BUILD_OS_DARWIN) */
#include <sys/types.h>                  // ir_bwrite.h needs it

#include "defs.h"
#include "stab.h"                       // ST
#include "wn.h"                         // ir_bwrite.h needs it
#include "pu_info.h"                    // ir_bwrite.h needs it
#include "ir_bwrite.h"                  // Output_File
#include "ir_bcom.h"                    // ir_b_save_buf
#include "ipl_summary.h"                // SUMMARY_* classes
#include "ipl_summarize.h"              // SUMMARY class
#include "ipl_array_bread_write.h"      // array summary classes
#include "loop_info.h"
 
ARRAY_SUMMARY_OUTPUT *Array_Summary_Output;

static INT ivar_offset = 0;
static INT regions_offset = 0;

// ----------------------------------------------------------
// Return TRUE if any of PROJECTED_REGIONs in the PU is messy 
// ----------------------------------------------------------
static BOOL 
PU_has_messy_regions(ARRAY_SUMMARY* summary)
{
  INT i, cfg_node_count;
  INT j, ra_count;
  INT k, pr_count;
  ARRAY_OF_REGION_ARRAYS* ara;
  PROJECTED_REGION_INFO_ARRAY* pra;
  
  INT* map = summary->Get_cd_map();

  // cfg nodes
  cfg_node_count = summary->Get_cfg_node_array_count();
  for (i = 0; i < cfg_node_count; ++i) {

    CFG_NODE_INFO* cfg_node = summary->Get_cfg_node_array(i);
    if (!cfg_node->Is_else())
      cfg_node = summary->Get_cfg_node_array(map[i]);

    // def arrays
    ara = cfg_node->Get_def_array();
    ra_count = ara ? ara->Lastidx()+1 : 0;
    for (j = 0; j < ra_count; ++j) {
      pra = (*ara)[j].Get_projected_region_array();
      pr_count = pra->Lastidx();
      for (k = 0; k <= pr_count; ++k) {
        if ((*pra)[k].Get_projected_region()->Is_messy_region()) 
          return TRUE;
      }
    }

    // use arrays;
    ara = cfg_node->Get_use_array();
    ra_count = ara ? ara->Lastidx()+1 : 0;
    for (j = 0; j < ra_count; ++j) {
      pra = (*ara)[j].Get_projected_region_array();
      pr_count = pra->Lastidx();
      for (k = 0; k <= pr_count; ++k) {
        if ((*pra)[k].Get_projected_region()->Is_messy_region()) 
          return TRUE;
      }
    }

    // param arrays;
    ara = cfg_node->Get_param_array();
    ra_count = ara ? ara->Lastidx()+1 : 0;
    for (j = 0; j < ra_count; ++j) {
      pra = (*ara)[j].Get_projected_region_array();
      pr_count = pra->Lastidx();
      for (k = 0; k <= pr_count; ++k) {
        if ((*pra)[k].Get_projected_region()->Is_messy_region()) 
          return TRUE;
      }
    }

    // formal arrays;
    ara = cfg_node->Get_formal_array();
    ra_count = ara ? ara->Lastidx()+1 : 0;
    for (j = 0; j < ra_count; ++j) {
      pra = (*ara)[j].Get_projected_region_array();
      pr_count = pra->Lastidx();
      for (k = 0; k <= pr_count; ++k) {
        if ((*pra)[k].Get_projected_region()->Is_messy_region())
          return TRUE;
      }
    }

    // perhaps we should also do process loop infos
  }
  
  return FALSE;
}

//---------------------------------------------------------------
// external interface to the array section summary information
//---------------------------------------------------------------
void
Init_write_asections(MEM_POOL* pool)
{
  Array_Summary_Output = CXX_NEW(ARRAY_SUMMARY_OUTPUT(pool), pool);
}

//---------------------------------------------------------------
// map the array sections per pu into the output file, so the
// memory can then all be freed
//---------------------------------------------------------------
void 
Map_asections(ARRAY_SUMMARY *summary, SUMMARY_PROCEDURE *p)
{
  INT start_idx = Array_Summary_Output->Get_cfg_node_count()+1;
  p->Set_array_section_index(start_idx);
  Array_Summary_Output->Map_summary_info(summary);
  INT end_idx  = Array_Summary_Output->Get_cfg_node_count()+1;
  p->Set_array_section_count(end_idx-start_idx);
  if (PU_has_messy_regions(summary)) 
    p->Set_has_messy_regions();
}

//---------------------------------------------------------------
// write the array info 
//---------------------------------------------------------------
void 
ARRAY_SUMMARY_OUTPUT::Write_summary(struct output_file *f2, 
                                    INT cur_sec_disp)
{
  INT size;
  
  Output_File *fl = (Output_File*) f2;
  // write out the TERM ARRAY
  if (Get_term_count() != -1) 
    {
      size = (Get_term_count() + 1) * sizeof(TERM);

      offset_term = (INT) ir_b_save_buf (Get_term (0),
                                         size, sizeof(INT64), 0, fl);
      offset_term -= cur_sec_disp;
    }


  // write out the PROJECTED_NODE array
  if (Get_projected_node_count() != -1) 
    {
      size = (Get_projected_node_count() + 1) * sizeof(PROJECTED_NODE);
      
      offset_projected_node = (INT) ir_b_save_buf (Get_projected_node (0),
                                                   size, sizeof(INT64), 0, fl);
      offset_projected_node -= cur_sec_disp;
    }

  // write out the PROJECTED_REGION array
  if (Get_projected_region_count() != -1) 
    {
      size = (Get_projected_region_count() + 1) * sizeof(PROJECTED_REGION);
      
      offset_projected_region = 
        (INT) ir_b_save_buf (Get_projected_region (0),
                             size, sizeof(INT64), 0, fl);
      offset_projected_region -= cur_sec_disp;
    }

  // write out the REGION_ARRAY
  if (Get_region_count() != -1) 
    {
      size = (Get_region_count() + 1) * sizeof(REGION_ARRAYS);
      
      offset_region = (INT) ir_b_save_buf (Get_region_array(0),
                                           size, sizeof(INT64), 0, fl);
      offset_region -= cur_sec_disp;
    }

  // write out the CFG_NODE_INFO array
  if (Get_cfg_node_count() != -1) 
    {
      size = (Get_cfg_node_count() + 1) * sizeof(CFG_NODE_INFO);
      
      offset_cfg_node =	(INT) ir_b_save_buf (Get_cfg_node (0),
                                             size, sizeof(INT64), 0, fl);
      offset_cfg_node -= cur_sec_disp;
    }

  // write out the IVAR array
  if (Get_ivar_count() != -1) 
    {
      size = (Get_ivar_count() + 1) * sizeof(IVAR);
      
      offset_ivar = (INT) ir_b_save_buf (Get_ivar (0),
                                         size, sizeof(INT64), 0, fl);
      offset_ivar -= cur_sec_disp;
    }

  // write out the LOOPINFO array
  if (Get_loopinfo_count() != -1) 
    {
      size = (Get_loopinfo_count() + 1) * sizeof(LOOPINFO);
      offset_loop_info = (INT) ir_b_save_buf (Get_loopinfo (0),
                                              size, sizeof(INT64), 0, fl);
      offset_loop_info -= cur_sec_disp;
    }

  // write out the scalars array
  if (Get_scalars_count() != -1) 
    {
      size = (Get_scalars_count() + 1) * sizeof(SCALAR_INFO);
      offset_scalars = (INT) ir_b_save_buf (Get_scalars (0),
                                            size, sizeof(INT64), 0, fl);
      offset_scalars -= cur_sec_disp;
    }
}

//---------------------------------------------------------------
// update the header with relevant fields
//---------------------------------------------------------------
void
ARRAY_SUMMARY_OUTPUT::Update_array_sect_header(SUMMARY_FILE_HEADER *header_addr)
{
  header_addr->Set_cfg_node_offset(offset_cfg_node);
  header_addr->Set_regions_array_offset(offset_region);
  header_addr->Set_projected_region_offset(offset_projected_region);
  header_addr->Set_projected_array_offset(offset_projected_node);
  header_addr->Set_term_array_offset(offset_term);
  header_addr->Set_ivar_offset(offset_ivar);
  header_addr->Set_loopinfo_offset(offset_loop_info);
  header_addr->Set_scalar_node_offset(offset_scalars);

  header_addr->Set_cfg_node_entry_size(sizeof(CFG_NODE_INFO));
  header_addr->Set_regions_array_entry_size(sizeof(REGION_ARRAYS));
  header_addr->Set_projected_region_entry_size(sizeof(PROJECTED_REGION));
  header_addr->Set_projected_array_entry_size(sizeof(PROJECTED_NODE));
  header_addr->Set_term_array_entry_size(sizeof(TERM));
  header_addr->Set_ivar_entry_size(sizeof(IVAR));
  header_addr->Set_loopinfo_entry_size(sizeof(LOOPINFO));
  header_addr->Set_scalar_node_entry_size(sizeof(SCALAR_INFO));

  header_addr->Set_cfg_node_size(Get_cfg_node_count()+1);
  header_addr->Set_regions_array_size(Get_region_count()+1);
  header_addr->Set_projected_region_size(Get_projected_region_count()+1);
  header_addr->Set_projected_array_size(Get_projected_node_count()+1);
  header_addr->Set_term_array_size(Get_term_count() + 1);
  header_addr->Set_ivar_size(Get_ivar_count() + 1);
  header_addr->Set_loopinfo_size(Get_loopinfo_count() + 1);
  header_addr->Set_scalar_node_size(Get_scalars_count() + 1);
}


//---------------------------------------------------------------
// update the actual array
//---------------------------------------------------------------
static void
update_actual_indices(ARRAY_SUMMARY *summary) 
{
  INT count = 0;
  INT_IDS* actual_scalar_info_map = summary->Get_actual_scalar_info_map();
  INT actual_count = summary->Get_actual_count();
  INT start_idx = summary->Get_actual_start_idx();
  INT end_idx = start_idx + actual_count;

  for (INT i= start_idx; i<end_idx; ++ i)
    {
      SUMMARY_ACTUAL* actual = Summary->Get_actual(i);
      if (actual_scalar_info_map[count].Get_id() > 0)
	{
	  INT cd_idx = actual_scalar_info_map[count].Get_cd_idx();
	  INT scalar_offset = actual_scalar_info_map[count].Get_id();
	  CFG_NODE_INFO *cfg_node = summary->Get_cfg_node_array(cd_idx);
	  INT cfg_offset = cfg_node->Get_scalar_index();
          if (actual->Get_pass_type() != PASS_ARRAY_SECTION)
	    actual->Set_index(scalar_offset+ cfg_offset-1);
	}
      ++count;
    }
}

//---------------------------------------------------------------
// update the formal array
//---------------------------------------------------------------
static void
update_formal_indices(ARRAY_SUMMARY *summary,
                      INT array_of_region_arrays)
{
  INT idx_formal = summary->Get_formal_start_idx();
  INT formal_count = summary->Get_formal_count();
  for (INT i = idx_formal; i < idx_formal + formal_count; i++) {
    SUMMARY_FORMAL* sf = Summary->Get_formal(i);
    if (sf->Get_region_index() >= 0)
      sf->Set_region_index(array_of_region_arrays + sf->Get_region_index());
  }
}

//---------------------------------------------------------------
// map the arrays in Array_Summary to the output structure
//---------------------------------------------------------------
void
ARRAY_SUMMARY_OUTPUT::Map_summary_info(ARRAY_SUMMARY *summary)
{
  INT idx, id;
  REGION_ARRAYS* rarray;
  INT count,  offset;
  SCALAR_INFO *item, *v;
  BOOL Do_mapping = FALSE;
  INT total_pass_count = 0;

  offset = 0;
  // Map_ivar_array(summary->Get_ivar_array());

  // cfg nodes
  INT cfg_node_count = summary->Get_cfg_node_array_count();
  INT lastidx = _cfg_nodes->Lastidx() + 1;
  INT* map = summary->Get_cd_map();
  BOOL found_scalar_actuals = FALSE; 
  INT local_regions_count = 0;
  INT i,j;


  for (i=0; i< cfg_node_count; ++i)
    {

      CFG_NODE_INFO* cfg_node = summary->Get_cfg_node_array(i);
      if (!cfg_node->Is_else())
	cfg_node = summary->Get_cfg_node_array(map[i]);
      // add this cfg_node
      idx = _cfg_nodes->Newidx();
      CFG_NODE_INFO*  cfg_node_out = &(*_cfg_nodes)[idx];
      cfg_node_out->Init_Out();

      if      (cfg_node->Is_if())      cfg_node_out->Set_type_if();
      else if (cfg_node->Is_do_loop()) cfg_node_out->Set_type_do_loop();
      else if (cfg_node->Is_entry())   cfg_node_out->Set_type_entry();
      else if (cfg_node->Is_else())    cfg_node_out->Set_type_else();

      if (cfg_node_out->Has_calls())   cfg_node_out->Set_has_calls();
      if (cfg_node_out->Is_executed()) cfg_node_out->Set_is_executed();

      cfg_node_out->Set_cd_index(cfg_node->Get_cd_index());

      // add the def arrays
      ARRAY_OF_REGION_ARRAYS* def_array = cfg_node->Get_def_array();
      if (def_array)
	count = def_array->Lastidx() + 1;
      else
	count = 0;
      for (j=0; j<count;++j)
	{
	  rarray = &(*def_array)[j];
	  id = Map_region_arrays(rarray);
	  if (j==0)
	    {
	      cfg_node_out->Set_def_count(count);
	      cfg_node_out->Set_def_index(id);
	    }
	}

      // add the use arrays;
      ARRAY_OF_REGION_ARRAYS* use_array = cfg_node->Get_use_array();
      if (use_array)
	count = use_array->Lastidx() + 1;
      else
 	count = 0;
      for (j=0; j<count;++j)
	{
	  rarray = &(*use_array)[j];
	  id = Map_region_arrays(rarray);
	  if (j == 0)
	    {
	      cfg_node_out->Set_use_count(count);
	      cfg_node_out->Set_use_index(id);
	    }
	}

      // add the param arrays;
      ARRAY_OF_REGION_ARRAYS* param_array =
	cfg_node->Get_param_array();

      if (param_array)
	count = param_array->Lastidx() + 1;
      else
	count = 0;

      INT id_base = -1;
      for (j=0; j<count;++j)
	{
	  rarray = &(*param_array)[j];
	  id = Map_region_arrays(rarray);
          if (id_base == -1)
            id_base = id;
	  if (j == 0)
	    {
	      cfg_node_out->Set_param_count(count);
	      cfg_node_out->Set_param_index(id);
	    }
	}

      // Add the base to PASS_ARRAY_SECTION actuals
      if (id_base != -1) { 
        INT k;
	DYN_ARRAY<SUMMARY_ACTUAL*> sa_actuals(summary->Get_local_pool());

        for (k = 0; k < count; k++) { 
          REGION_ARRAYS* ra = &(*param_array)[k];
	  PROJECTED_REGION_INFO_ARRAY* pria = ra->Get_projected_region_array();
	  PROJECTED_REGION_INFO* pri = &(*pria)[0];
          PROJECTED_REGION* pr = pri->Get_projected_region();
	  INT callsite_index = pr->Get_callsite_id();
	  SUMMARY_CALLSITE* sc = Summary->Get_callsite(callsite_index);
          INT idx_actual = sc->Get_actual_index();
          INT actual_count = sc->Get_param_count();
          for (j = idx_actual; j < idx_actual + actual_count; j++) {
            SUMMARY_ACTUAL* sa = Summary->Get_actual(j);
            if (sa->Get_pass_type() == PASS_ARRAY_SECTION
	        && sa->Get_index() == k) {
	      INT actual_index = sa_actuals.Newidx();
	      sa_actuals[actual_index] = sa;
	      break;
	    }
          }
	  FmtAssert(j < idx_actual + actual_count, 
	    ("Map_summary_info(): Expected to find SUMMARY_ACTUAL"));
	}

	for (k = 0; k < count; k++) {
	  SUMMARY_ACTUAL* sa = sa_actuals[k];
	  sa->Set_index(id_base + sa->Get_index()); 
        }
      }

      // add the formal arrays;
      ARRAY_OF_REGION_ARRAYS* formal_array =
        cfg_node->Get_formal_array();

      if (formal_array)
        count = formal_array->Lastidx() + 1;
      else
        count = 0;

      id_base = -1;
      for (j=0; j<count;++j)
        {
          rarray = &(*formal_array)[j];
          id = Map_region_arrays(rarray);
          if (id_base == -1)
            id_base = id;
          if (j == 0)
            {
              cfg_node_out->Set_formal_count(count);
              cfg_node_out->Set_formal_index(id);
            }
        }

      if (id_base != -1) {
        INT idx_formal = summary->Get_formal_start_idx();
        INT formal_count = summary->Get_formal_count();

        for (INT k = idx_formal; k < idx_formal + formal_count; k++) {
          SUMMARY_FORMAL* sf = Summary->Get_formal(k);
          if (sf->Get_region_index() >= 0)
            sf->Set_region_index(id_base + sf->Get_region_index());
        }
      }

      // add the loopinfo arrays;

      LOOPINFO *loop_info;
      if (cfg_node->Is_do_loop())
	{

	  loop_info = cfg_node->Get_loopinfo();
	  if (loop_info)
	    {
	      INT lindex = Map_loop_info(loop_info);
	      cfg_node_out->Set_loop_index(lindex);
	    }
	}
      
      if (cfg_node->Is_if())
	{
	  cfg_node_out->Set_else_index(cfg_node->Get_else_index() +
				       lastidx);
	  CFG_NODE_INFO *cfg_node_else = 
	    summary->Get_cfg_node_array(cfg_node->Get_else_index());
	  cfg_node_else->Set_if_index(i);
	}
      else if (cfg_node->Is_else())
	{
	  cfg_node_out->Set_if_index(cfg_node->Get_if_index() +
				     lastidx);
	}
	  
      // add the array reduc
      INT_ARRAY* scalar_array = cfg_node->Get_scalar_array();
      if (scalar_array)
	count = cfg_node->Get_scalar_array()->Lastidx() + 1;
      else
	count = 0;
      if (count > 0)
        found_scalar_actuals = TRUE;

      cfg_node_out->Set_scalar_count(count);

      offset = 0;
      for (j=0; j<count;++j)
	{
	  if (j==0)
	    offset = _scalar_items->Lastidx()+1;
	  item = &(*scalar_array)[j];
	  id = _scalar_items->Newidx();
	  v = &(*_scalar_items)[id];
	  v->Set_id(item->Get_id());
	  v->Set_type(item->Get_type());
	  v->Set_callsite_id(item->Get_callsite_id());
	  //v->Print(stderr);
	  // if this scalar info has the callsite id set then
	  // update the SUMMARY_ACTUAL node
	  if (item->Is_passed_ref() || item->Is_may_passed_ref())
	    {
	      Do_mapping = TRUE;
	    }
	}
      cfg_node_out->Set_scalar_index(offset);
      cfg_node->Set_scalar_index(offset);
    }
  // if  there are any scalar nodes then update the actual array
  // information
  if (found_scalar_actuals && Do_mapping)
    {
      update_actual_indices(summary);
    }
}

// --------------------------------------------
// Mark formal parameters that appear as linear 
// or non-linear terms in the access vector.
// --------------------------------------------
static void
Mark_used_formal_symbols(ACCESS_VECTOR* av)
{
  // For now, skip access vectors that are too messy
  if (av->Too_Messy) return;

  // Traverse symbols in linear terms
  if (av->Contains_Lin_Symb()) {
    INTSYMB_ITER iter(av->Lin_Symb);
    for (INTSYMB_NODE* node=iter.First(); !iter.Is_Empty(); node=iter.Next()) {
      ST* s = node->Symbol.St();
      Mark_formal_summary_symbol(s);
    }
  } 
  
  // Traverse symbols in non-linear terms
  if (av->Contains_Non_Lin_Symb()) {
    SUMPROD_ITER iter(av->Non_Lin_Symb);
    for (SUMPROD_NODE* node=iter.First(); !iter.Is_Empty(); node=iter.Next()) {
      SYMBOL_ITER sym_iter(node->Prod_List);
      for (SYMBOL_NODE* sym_node = sym_iter.First();
           !sym_iter.Is_Empty();
           sym_node = sym_iter.Next()) {
        ST* s = sym_node->Symbol.St();
        Mark_formal_summary_symbol(s);
      }
    }
  }
}

// --------------------------------------------
// Mark formal parameters that appear as linear 
// or non-linear terms in the access array.
// --------------------------------------------
static void
Mark_used_formal_symbols(ACCESS_ARRAY* aa)
{
  // Skip access arrays that are too messy
  if (!aa->Too_Messy) {
    for (UINT16 i = 0; i < aa->Num_Vec(); ++i) {
      Mark_used_formal_symbols(aa->Dim(i));
    }
  }
}

// --------------------------------------------
// Mark formal parameters that appear as linear 
// or non-linear terms in the loop info.
// --------------------------------------------
static void
Mark_used_formal_symbols(DO_LOOP_INFO_BASE* dli)
{
  Mark_used_formal_symbols(dli->Get_lb());
  Mark_used_formal_symbols(dli->Get_ub());
  Mark_used_formal_symbols(dli->Get_step());
}

//-------------------------------------------------------------
// map the loop information
//-------------------------------------------------------------
INT 
ARRAY_SUMMARY_OUTPUT::Map_loop_info(LOOPINFO *l)
{
  // Mark formals used as symbolic terms in loop bounds
  DO_LOOP_INFO_BASE* dli = IPL_Loopinfo_Map->Find(l);
  if (dli) {
    Mark_used_formal_symbols(dli);
  }

  INT idx = _loopinfo_nodes->Newidx();
  LOOPINFO* l_out = &(*_loopinfo_nodes)[idx];
  BZERO(l_out, sizeof(LOOPINFO));
  l_out->Set_nest_level(l->Get_nest_level());
  l_out->Set_flags(l->Get_flags());
  
  if (!l->Is_messy_ub()) {
    LINEX* upper = l->Get_upper_linex();
    for (INT j = 0; j <= upper->Num_terms(); ++j) {
      Map_term(upper->Get_term(j), upper->Get_term(j));
    }
    INT reuse_linex_idx = Search_for_terms(upper);
    if (reuse_linex_idx == 0) {
      for (INT j = 0; j <= upper->Num_terms(); ++j) {
        _terms->AddElement(*(upper->Get_term(j)));
      }
      INT first_term_idx = _terms->Lastidx() - upper->Num_terms();
      Insert_terms(upper->Get_term(0), first_term_idx, upper->Num_terms());
      l_out->Set_ub_term_index(first_term_idx);
    }
    else {
      l_out->Set_ub_term_index(reuse_linex_idx-1);
    }
    l_out->Set_ub_term_count(upper->Num_terms()+1);
  }

  if (!l->Is_messy_lb()) {
    LINEX* lower = l->Get_lower_linex();
    for (INT j = 0; j <= lower->Num_terms(); ++j) {
      Map_term(lower->Get_term(j), lower->Get_term(j));
    }
    INT reuse_linex_idx = Search_for_terms(lower);
    if (reuse_linex_idx == 0) {
      for (INT j = 0; j <= lower->Num_terms(); ++j) {
        _terms->AddElement(*(lower->Get_term(j)));
      }
      INT first_term_idx = _terms->Lastidx() - lower->Num_terms();
      Insert_terms(lower->Get_term(0), first_term_idx, lower->Num_terms());
      l_out->Set_lb_term_index(first_term_idx);
    }
    else {
      l_out->Set_lb_term_index(reuse_linex_idx-1);
    }
    l_out->Set_lb_term_count(lower->Num_terms()+1);
  }

  if (!l->Is_messy_step()) {
    LINEX* step = l->Get_step_linex();
    for (INT j = 0; j <= step->Num_terms(); ++j) {
      Map_term(step->Get_term(j), step->Get_term(j));
    }
    INT reuse_linex_idx = Search_for_terms(step);
    if (reuse_linex_idx == 0) {
      for (INT j = 0; j <= step->Num_terms(); ++j) {
        _terms->AddElement(*(step->Get_term(j)));
      }
      INT first_term_idx = _terms->Lastidx() - step->Num_terms();
      Insert_terms(step->Get_term(0), first_term_idx, step->Num_terms());
      l_out->Set_step_term_index(first_term_idx);
    }
    else {
      l_out->Set_step_term_index(reuse_linex_idx-1);
    }
    l_out->Set_step_term_count(step->Num_terms()+1);
  }
  
  return idx+1;
}


//-------------------------------------------------------------
// Map the region array to the summary output information
//-------------------------------------------------------------
INT
ARRAY_SUMMARY_OUTPUT::Map_region_arrays(REGION_ARRAYS* rarray)
{
  INT idx = _region_arrays->Newidx();
  REGION_ARRAYS* r = &(*_region_arrays)[idx];
  r->Copy_write(rarray);

  PROJECTED_REGION_INFO_ARRAY* proj_regions = 
    rarray->Get_projected_region_array();

  for (INT i = 0; i <= proj_regions->Lastidx(); ++i) {
    PROJECTED_REGION* proj_reg = (*proj_regions)[i].Get_projected_region();
    INT id = Map_projected_region(proj_reg);
    if (i == 0) {
      r->Set_idx(id);
      r->Set_count(proj_regions->Lastidx() + 1);
    }
  }
  
  return idx;
}

//--------------------------------------------------------------------
// Map the projected region 
//--------------------------------------------------------------------
INT
ARRAY_SUMMARY_OUTPUT::Map_projected_region(PROJECTED_REGION *proj_reg)
{
  // Mark formals used as symbolic terms in access arrays
  ACCESS_ARRAY* aa = IPL_Access_Array_Map->Find(proj_reg);
  if (aa) {
    Mark_used_formal_symbols(aa);
  }

  INT idx = _projected_regions->Newidx();
  PROJECTED_REGION* p = &(*_projected_regions)[idx];
  
  p->Copy_write(proj_reg);

  if (proj_reg->Get_projected_array()) {
    p->Set_id(Map_proj_array(proj_reg->Get_projected_array()));
  }
  else {
    p->Set_id(-1);
  }

  return idx;
}

//--------------------------------------------------------------------
// Map the projected array
//--------------------------------------------------------------------
INT
ARRAY_SUMMARY_OUTPUT::Map_proj_array(PROJECTED_ARRAY* proj_array)
{
  for (INT i = 0; i <= proj_array->Lastidx(); ++i) {

    PROJECTED_NODE* p = &(*_project_nodes)[_project_nodes->Newidx()];
    BZERO (p, sizeof(PROJECTED_NODE));

    PROJECTED_NODE* p_in = &(*proj_array)[i];
    p->Set_flags(p_in->Get_flags());

    LINEX* upper = p_in->Get_upper_linex();
    if (!p_in->Is_messy_ub() && upper->Num_terms() != -1) {
      for (INT j = 0; j <= upper->Num_terms(); ++j) {
        Map_term(upper->Get_term(j), upper->Get_term(j));
      }
      INT reuse_linex_idx = Search_for_terms(upper);
      if (reuse_linex_idx == 0) {
        for (INT j = 0; j <= upper->Num_terms(); ++j) {
          _terms->AddElement(*(upper->Get_term(j)));
        }
        INT first_term_idx = _terms->Lastidx() - upper->Num_terms();
        Insert_terms(upper->Get_term(0), first_term_idx, upper->Num_terms());
        p->Set_ub_term_index(first_term_idx);
      }
      else {
        p->Set_ub_term_index(reuse_linex_idx-1);
      }
      p->Set_ub_term_count(upper->Num_terms()+1);
    }

    LINEX* lower = p_in->Get_lower_linex();
    if (!p_in->Is_messy_lb() && lower->Num_terms() != -1) {
      for (INT j = 0; j <= lower->Num_terms(); ++j) {
        Map_term(lower->Get_term(j), lower->Get_term(j));
      }
      INT reuse_linex_idx = Search_for_terms(lower);
      if (reuse_linex_idx == 0) {
        for (INT j = 0; j <= lower->Num_terms(); ++j) {
          _terms->AddElement(*(lower->Get_term(j)));
        }
        INT first_term_idx = _terms->Lastidx() - lower->Num_terms();
        Insert_terms(lower->Get_term(0), first_term_idx, lower->Num_terms());
        p->Set_lb_term_index(first_term_idx);
      }
      else {
        p->Set_lb_term_index(reuse_linex_idx-1);
      }
      p->Set_lb_term_count(lower->Num_terms()+1);
    }

    LINEX* step = p_in->Get_step_linex();
    if (!p_in->Is_messy_step() && step->Num_terms() != -1) {
      for (INT j = 0; j <= step->Num_terms(); ++j) {
        Map_term(step->Get_term(j), step->Get_term(j));
      }
      INT reuse_linex_idx = Search_for_terms(step);
      if (reuse_linex_idx == 0) {
        for (INT j = 0; j <= step->Num_terms(); ++j) {
          _terms->AddElement(*(step->Get_term(j)));
        }
        INT first_term_idx = _terms->Lastidx() - step->Num_terms();
        Insert_terms(step->Get_term(0), first_term_idx, step->Num_terms());
        p->Set_step_term_index(first_term_idx);
      }
      else {
        p->Set_step_term_index(reuse_linex_idx-1);
      }
      p->Set_step_term_count(step->Num_terms()+1);
    }

    LINEX* segment_length = p_in->Get_segment_length_linex();
    if (segment_length) {
      // currently, segment length must be constant,
      // which implies that the only term is term(0)
      Is_True(segment_length->Is_const(), ("segment length must be constant"));

      INT reuse_linex_idx = Search_for_terms(segment_length);
      if (reuse_linex_idx == 0) {
        _terms->AddElement(*(segment_length->Get_term(0)));
        Insert_terms(segment_length->Get_term(0), _terms->Lastidx(), 0);
        p->Set_segment_length_term_index(_terms->Lastidx());
      }
      else {
        p->Set_segment_length_term_index(reuse_linex_idx-1);
      }
      p->Set_segment_length_term_count(1);
    }

    LINEX* segment_stride = p_in->Get_segment_stride_linex();
    if (segment_stride) {
      // currently, segment stride must be constant,
      // which implies that the only term is term(0)
      Is_True(segment_stride->Is_const(), ("segment stride must be constant"));

      INT reuse_linex_idx = Search_for_terms(segment_stride);
      if (reuse_linex_idx == 0) {
        _terms->AddElement(*(segment_stride->Get_term(0)));
        Insert_terms(segment_stride->Get_term(0), _terms->Lastidx(), 0);
        p->Set_segment_stride_term_index(_terms->Lastidx());
      }
      else {
        p->Set_segment_stride_term_index(reuse_linex_idx-1);
      }
      p->Set_segment_stride_term_count(1);
    }

  }

  return (proj_array->Lastidx() < 0 ? 
          -1 : 
          _project_nodes->Lastidx() - proj_array->Lastidx());
}

//--------------------------------------------------------------------
// make a simple copy of the term
//--------------------------------------------------------------------
void
ARRAY_SUMMARY_OUTPUT::Map_term(TERM* t_in, TERM* t_out)
{
  new (t_out) TERM(t_in);
}

//--------------------------------------------------------------------
// Map the ivar array
//--------------------------------------------------------------------
void
ARRAY_SUMMARY_OUTPUT::Map_ivar_array(IVAR_ARRAY *ivar_array)
{
  INT last_idx = ivar_array->Lastidx();
  for (INT i = 0; i <= last_idx; ++i) {
    _ivars->AddElement((*ivar_array)[i]);
  }
    
  if (last_idx >= 0) {
    ivar_offset = _ivars->Lastidx() - last_idx;
  }
}
				    
//----------------------------------------------------------------
// return the key, given the term, and num terms
//----------------------------------------------------------------
UINT64
ARRAY_SUMMARY_OUTPUT::Get_key(TERM *t, INT num_terms)
{
  UINT64 key = 0;

  key |= ((UINT64) num_terms << 56);
  key |= ((UINT64) t->Get_type() << 48);
  key |= ((UINT64) t->Get_desc() << 32);
  key |= ((UINT64) t->Get_coeff());
  
  return key;
}

//----------------------------------------------------------------
// mechanism to eliminate duplicate linex terms
// if the linex is not found then return FALSE
// majority of terms are of size at most 3. We will form groups
// of terms and for each such group hash based on the first term
//----------------------------------------------------------------
INT
ARRAY_SUMMARY_OUTPUT::Search_for_terms(LINEX *l)
{
  // get the first term, switch based on the type of the term 
  TERM* t = l->Get_term(0);
  UINT64 key = Get_key(t, l->Num_terms());
  INTEGER_ARRAY* int_array = Get_term_hash_table()->Find(key);

  if (int_array) {
    for (INT i = 0; i <= int_array->Lastidx(); ++i) {
      INT id = (*int_array)[i];
      if (Get_term(id)->Is_equal(t, l->Num_terms())) {
        // since terms start at 0, return id + 1
        return id + 1;
      }
    }
  }
  
  return 0;
}

//----------------------------------------------------------------
// mechanism to insert terms
//----------------------------------------------------------------
void
ARRAY_SUMMARY_OUTPUT::Insert_terms(TERM* t, INT idx, INT count)
{ 
  UINT64 key = Get_key(t, count);
  TERM_HASH_TABLE* hash_table = Get_term_hash_table();
  INTEGER_ARRAY* int_array = hash_table->Find(key);
  
  if (int_array) {
    for (INT i = 0; i <= int_array->Lastidx(); ++i) {
      if (t->Is_equal(Get_term((*int_array)[i]), count)) {
        return;
      }
    }
    // if we reach here then we need to insert this into the int array
    int_array->AddElement(idx);
  }
  else {
    // create the int array, enter this element into it,
    // and enter the int array into the hash table
    int_array = CXX_NEW(INTEGER_ARRAY(_m),_m);
    int_array->AddElement(idx);
    hash_table->Enter(key, int_array);
  }
}


//----------------------------------------------------------------
// Trace the array section summary information 
//----------------------------------------------------------------
void
ARRAY_SUMMARY_OUTPUT::Trace(FILE *f, const void *sbase)
{
  const char *section_base = (char *)sbase;
  Elf64_Word* offset = (Elf64_Word*)section_base;
  SUMMARY_FILE_HEADER *file_header = 
    (SUMMARY_FILE_HEADER*)(section_base + *offset);
  TERM* term_array;
  PROJECTED_NODE *projected_node_array;
  IVAR* ivar_array;
  REGION_ARRAYS* regions_array;
  PROJECTED_REGION* projected_region_array;
  CFG_NODE_INFO* cfg_node_array;
  LOOPINFO *loopinfo_array;

  // print the ivar array elements
  if (file_header->Get_ivar_size() != 0) {
    ivar_array = (IVAR *)
      (section_base + file_header->Get_ivar_offset());
    Print_ivar_array ( f, file_header->Get_ivar_size(), ivar_array);
  }

  // print the term array elements
  if (file_header->Get_term_array_size() != 0) {
    term_array = (TERM *)
      (section_base + file_header->Get_term_array_offset());
    Print_term_array ( f, file_header->Get_term_array_size(), term_array,
                       ivar_array);
  }

  // print the  cfg node info array
  if (file_header->Get_cfg_node_size() != 0) {
    cfg_node_array = (CFG_NODE_INFO *)
      (section_base + file_header->Get_cfg_node_offset());
    Print_cfg_node_array (f, file_header->Get_cfg_node_size(), cfg_node_array);
  }    
    
  // print the region array elements
  if (file_header->Get_regions_array_size() != 0) {
    regions_array = ( REGION_ARRAYS*)
      (section_base + file_header->Get_regions_array_offset());
    Print_regions_array ( f, file_header->Get_regions_array_size(), 
                          regions_array);
  }

  // print the projected region array elements
  if (file_header->Get_projected_region_size() != 0) {
    projected_region_array = ( PROJECTED_REGION*)
      (section_base + file_header->Get_projected_region_offset());
    Print_projected_region_array (f, file_header->Get_projected_region_size(), 
                                  projected_region_array);
  }

  // print the projected node array elements
  if (file_header->Get_projected_array_size() != 0) {
    projected_node_array = ( PROJECTED_NODE*)
      (section_base + file_header->Get_projected_array_offset());
    Print_projected_array ( f, file_header->Get_projected_array_size(), 
                            projected_node_array);

  }

  // print the term array elements
  if (file_header->Get_loopinfo_size() != 0) {
    loopinfo_array = (LOOPINFO *)
      (section_base + file_header->Get_loopinfo_offset());
    Print_loopinfo_array (f, file_header->Get_loopinfo_size(), loopinfo_array);
  }

  // print the scalar array elements
  if (file_header->Get_scalar_node_size() != 0) {
    SCALAR_INFO* scalar_array = (SCALAR_INFO*)
      (section_base + file_header->Get_scalar_offset());
    Print_scalar_array ( f, file_header->Get_scalar_node_size(), scalar_array);
  }
}

//----------------------------------------------------------------
// Trace the array section summary information 
//----------------------------------------------------------------
void
ARRAY_SUMMARY_OUTPUT::Trace(FILE *f)
{
  // print the cfg node info array
  if (Get_cfg_node_count() >= 0) 
    Print_cfg_node_array(f, Get_cfg_node_count()+1, Get_cfg_node(0));
    
  // print the region array elements
  if (Get_region_count() >= 0) 
    Print_regions_array(f, Get_region_count()+1, Get_region_array(0));

  // print the projected region array elements
  if (Get_projected_region_count() >= 0)
    Print_projected_region_array(f, 
                                 Get_projected_region_count()+1,
                                 Get_projected_region(0));

  // print the projected node array elements
  if (Get_projected_node_count() >= 0) 
    Print_projected_array(f, 
                          Get_projected_node_count()+1,
                          Get_projected_node(0));

  // print the loop info array elements
  if (Get_loopinfo_count() >= 0)
    Print_loopinfo_array(f, Get_loopinfo_count()+1, Get_loopinfo(0));

  // print the term array elements
  if (Get_term_count() >= 0)
    Print_term_array(f, 
                     Get_term_count()+1, 
                     Get_term(0),
                     Get_ivar(0));

  // print the ivar array elements
  if (Get_ivar_count() >= 0)
    Print_ivar_array(f, Get_ivar_count()+1, Get_ivar(0));

  // print the scalar array elements
  if (Get_scalars_count() >= 0)
    Print_scalar_array(f, Get_scalars_count()+1, Get_scalars(0));
}

//---------------------------------------------------------------
// print the loopinfo array
//---------------------------------------------------------------
void
ARRAY_SUMMARY_OUTPUT::Print_loopinfo_array(FILE *fp, INT size,
					   LOOPINFO *loop)
{
  fprintf ( fp, "%sStart loopinfo array\n%s", SBar, SBar );
  for ( INT i=0; i<size; ++i )
    {
      fprintf(fp, "LOOP[%d]: ", i);
      loop[i].Print_file ( fp);
    }
  fprintf ( fp, "%sEnd loopinfo array\n%s", SBar, SBar );
}


//---------------------------------------------------------------
// print the scalar array
//---------------------------------------------------------------
void
ARRAY_SUMMARY_OUTPUT::Print_scalar_array(FILE *fp, INT size, SCALAR_INFO *scalars)
{
  fprintf ( fp, "%sStart scalar array\n%s", SBar, SBar );

  for ( INT i=0; i<size; ++i )
    {
      SCALAR_INFO* scalar = &scalars[i];

      fprintf(fp, "SCALAR[%d]: symbol_id = %d : ", i,
	      scalars[i].Get_id());
      if (scalar->Is_may_kill())
	fprintf(fp, "may kill, ");
      if (scalar->Is_may_use())
	fprintf(fp, "may use, ");
      if (scalar->Is_may_reduc())
	fprintf(fp, "may reduc, ");
      if (scalar->Is_kill())
	fprintf(fp, "must kill, ");
      if (scalar->Is_use())
	fprintf(fp, "must use, ");
      if (scalar->Is_reduc())
	fprintf(fp, "must reduc, ");
      if (scalar->Is_passed_ref())
	fprintf(fp, "must passed ref, ");
      if (scalar->Is_may_passed_ref())
	fprintf(fp, "may passed ref, ");
      if (scalar->Is_euse())
	fprintf(fp, "must euse, " );
      if (scalar->Is_array_may_reduc())
	fprintf(fp, "may reduc, ");
      if (scalar->Is_array_reduc())
	fprintf(fp, "must array reduc, " );
      if (scalar->Is_array_may_reduc())
	fprintf(fp, "may array reduc, " );
      fprintf(fp, "callsite id = %d \n", scalars[i].Get_callsite_id());
    }

  fprintf ( fp, "%sEnd scalar array\n%s", SBar, SBar );
}

//---------------------------------------------------------------
// print the term array
//---------------------------------------------------------------
void
ARRAY_SUMMARY_OUTPUT::Print_term_array(FILE *fp, INT size, TERM *term,
				       IVAR*/*ivar*/)
{
  fprintf ( fp, "%sStart term array\n%s", SBar, SBar );
  for ( INT i=0; i<size; ++i )
    {
      fprintf(fp, "TERM[%d]: ", i);
      term[i].Print_file (fp);
    }
  fprintf ( fp, "%sEnd term array\n%s", SBar, SBar );
}

//---------------------------------------------------------------
// print the projected array
//---------------------------------------------------------------
void
ARRAY_SUMMARY_OUTPUT::Print_projected_array(FILE *fp, INT size, 
					    PROJECTED_NODE* node)
{
  fprintf ( fp, "%sStart projected node array\n%s", SBar, SBar );
  for ( INT i=0; i<size; ++i )
    {
      fprintf(fp, "PROJ_NODE[%d]: ", i);
      node[i].Print_file ( fp);
    }
  fprintf ( fp, "%sEnd projected node array\n%s", SBar, SBar );
}
//---------------------------------------------------------------
// print the projected region array
//---------------------------------------------------------------
void
ARRAY_SUMMARY_OUTPUT::Print_projected_region_array(FILE *fp, INT size,
						   PROJECTED_REGION *node)
{
  fprintf ( fp, "%sStart projected region array\n%s", SBar, SBar );
  for ( INT i=0; i<size; ++i )
    {
      fprintf(fp, "PROJ_REGION[%d]: ", i);
      node[i].Print_file ( fp);
    }
  fprintf ( fp, "%sEnd projected region array\n%s", SBar, SBar );
}

//---------------------------------------------------------------
// print the regions array
//---------------------------------------------------------------
void
ARRAY_SUMMARY_OUTPUT::Print_regions_array(FILE *fp, INT size,
					  REGION_ARRAYS* node)
{
  fprintf ( fp, "%sStart regions array\n%s", SBar, SBar );
  for ( INT i=0; i<size; ++i )
    {
      fprintf(fp, "REGION[%d]: ", i);
      node[i].Print_file ( fp);
    }
  fprintf ( fp, "%sEnd regions array\n%s", SBar, SBar );
}

//---------------------------------------------------------------
// print the cfg node array
//---------------------------------------------------------------
void
ARRAY_SUMMARY_OUTPUT::Print_cfg_node_array(FILE *fp, INT size,
					   CFG_NODE_INFO* node)
{
  fprintf ( fp, "%sStart cfg node array\n%s", SBar, SBar );
  for ( INT i=0; i<size; ++i )
    {
      fprintf(fp, "CFG_NODE[%d]: ", i);
      node[i].Print_file ( fp);
    }
  fprintf ( fp, "%sEnd cfg node array\n%s", SBar, SBar );
}

//---------------------------------------------------------------
// print the ivar array
//---------------------------------------------------------------
void
ARRAY_SUMMARY_OUTPUT::Print_ivar_array(FILE *fp, INT size, 
				      IVAR* node)
{
  fprintf ( fp, "%sStart ivar node array\n%s", SBar, SBar );
  for ( INT i=0; i<size; ++i )
    {
      fprintf(fp, "IVAR[%d]: ", i);
      node[i].Print ( fp);
    }
  fprintf ( fp, "%sEnd ivar node array\n%s", SBar, SBar );
}
