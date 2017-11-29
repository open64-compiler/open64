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


// ====================================================================
// ====================================================================
//
// Module: ipa_section_prop.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/ipa/main/analyze/ipa_section_prop.cxx,v $
//
// Revision history:
//  2-10-97 - Original Version
//
// Description:
//
// Array flow sensitive and insensitive section analysis
//
// ====================================================================
// ====================================================================

#include <stdint.h>
#include <alloca.h>

#include "be_symtab.h"          // ST_is_const_initialized_scalar
#include "ipa_cg.h"             // IPA_NODE, IPA_EDGE, IPA_CALL_GRAPH
#include "ipa_option.h"         // IPA_TRACE_* flags
#include "ipa_preopt.h"         // IPA_Preoptimize
#include "ipa_summary.h"        // IPA_get_*_file_array 
#include "ipa_section_annot.h"  // STATE, STATE_ARRAY
#include "ipa_section_main.h"   // Init_IPA_Print_Arrays
#include "ipa_section_prop.h"   // IPA_ARRAY_DF_FLOW
#include "ipaa.h"               // IPAA_NODE_INFO
#include "config_ipa.h"         // IPA_Enable_* flags
#include "ipa_lno_util.h"	// Utility routines
#include "ipa_cost.h"		// Execution cost analysis 

MEM_POOL IPA_array_prop_pool;
BOOL Trace_IPA_Sections = FALSE;

static BOOL IPA_array_prop_pool_initialized = FALSE;

// =====================================================
// Static functions for supporting the implementation of
// classes IPA_SCALAR_MUST_DF_FLOW and IPA_ARRAY_DF_FLOW
// =====================================================

//=========================================================================
// This code deals with SECTION MOD
// 1) map the compressed terms into individual linex nodes
// 2) update the terms to reflect the results of constant propagation
//    and scalar mod
// 3) union projected regions, to reflect the results of the above
//=========================================================================

//---------------------------------------------------------------------
// Use IPA's MOD information to determine if a symbol has
// been modified directly or indirectly
// a) For formals, obtain the position and determine the mod information
// b) For globals, get the name_idx and check if it has been modified
//---------------------------------------------------------------------
static BOOL
Is_formal_modified (const IPA_NODE* node, 
                    const SUMMARY_FORMAL& formal)
{
  if (IPA_Enable_Simple_Alias) {
    IPAA_NODE_INFO* ipaa = node->Mod_Ref_Info();
    Is_True(ipaa, ("NULL ipaa info in is_mod\n"));
    INT position = formal.Get_position();
    return (ipaa->Is_formal_dmod_elmt(position) || 
            ipaa->Is_formal_imod_elmt(position));
  }
  return TRUE;
}

static BOOL
Is_global_modified (const IPA_NODE* node, 
                    ST_IDX base_st_idx)
{
  if (IPA_Enable_Simple_Alias) {
    IPAA_NODE_INFO* ipaa = node->Mod_Ref_Info();
    Is_True(ipaa, ("NULL ipaa info in is_mod\n"));
    return (ipaa->Is_def_elmt(ST_IDX_index(base_st_idx)));
  }
  return TRUE;
}

//---------------------------------------------------------------
// If the summary value is an integer constant then return TRUE
// and val should contain the value of the constant
//---------------------------------------------------------------
static BOOL
Is_constant_val(SUMMARY_VALUE* summary_val, INT64* val)
{
  if (summary_val->Is_const_st()) {
    TCON_IDX tcon_idx = ST_tcon(ST_ptr(summary_val->Get_const_st_idx()));
    return Targ_Is_Integral(Tcon_Table[tcon_idx], val);
  }
  
  if (summary_val->Is_int_const()) {
    *val = summary_val->Get_int_const_value();
    return TRUE;
  }

  return FALSE;
}

//---------------------------------------------------------------
// Check if the global is a constant value for this procedure
//---------------------------------------------------------------
static BOOL 
Is_constant_global(const IPA_NODE* n, 
                   INT64* val,
                   ST_IDX base_st_idx, 
                   INT64 offset,
                   TYPE_ID mtype)
{
  // constant scalar variable discovered by CGI
  TCON tcon;
  //see the definition of ST_is_const_initialized_scalar
  if (ST_is_const_initialized_scalar(ST_ptr(base_st_idx), offset, tcon)) {
    return Targ_Is_Integral(tcon, val);
  }

  // constant value for (ST,offset) in this PU discovered by common cprop 
  GLOBAL_ANNOT* gannot = n->Global_Annot();
  if (gannot) {
    const GLOBAL_VALUE* gval = 
      gannot->Find(GLOBAL_ANNOT::Index(base_st_idx), offset, 0);
    if (gval) {
      SUMMARY_VALUE* summary_value = gval->Value();
      if (summary_value && 
          summary_value->Get_mtype() == mtype &&
          Is_constant_val(summary_value, val)) {
        return TRUE;
      }
    }
  }

  return FALSE;
}

//----------------------------------------------------------------
// Update the terms: try to promote non-constant terms to CONSTANT
//----------------------------------------------------------------
static void
Update_term_with_formal_constant (const IPA_NODE* node, 
                                  const SUMMARY_FORMAL& formal, 
                                  TERM* t)
{
  // Check if the symbol is constant-valued formal
  VALUE_DYN_ARRAY* cprop_annot = node->Cprop_Annot();
  if (cprop_annot && cprop_annot != (void*)-1) {
    SUMMARY_VALUE* annot_node = &(*cprop_annot)[formal.Get_position()];
    INT64 val;
    if (Is_constant_val(annot_node, &val)) {
      new (t) TERM(LTKIND_CONST, (COEFF)val*t->Get_coeff(), CONST_DESC, 0);
    }
  }
}

//----------------------------------------------------------------
// Update the terms: try to promote non-constant terms to CONSTANT
//----------------------------------------------------------------
static void
Update_term_with_global_constant (const IPA_NODE* node, 
                                  ST_IDX base_st_idx,
                                  INT64 offset,
                                  TYPE_ID mtype,
                                  TERM* t)
{
  // Check if it's a constant value from a common block
  INT64 val;
  if (Is_constant_global(node, &val, base_st_idx, offset, mtype)) {
    new (t) TERM(LTKIND_CONST, (COEFF)val*t->Get_coeff(), CONST_DESC, 0);
  }
}

//--------------------------------------------------------------------
// Update terms in the linex expressions with constant values if 
// possible and determine if any term has non-constant symbolic values
//--------------------------------------------------------------------
static LINEX*
update_linex_with_constant_mod(IPA_NODE* node, LINEX* linex, BOOL* modified)
{
  INT32 size;
  IVAR* ipa_ivar = IPA_get_ivar_array(node, size);
  SUMMARY_SYMBOL* ipa_symbol = IPA_get_symbol_array(node);
  SUMMARY_FORMAL* ipa_formal = IPA_get_formal_array(node);
  SUMMARY_PROCEDURE* ipa_proc = node->Summary_Proc();

  *modified = FALSE;

  for (INT i = 0; i <= linex->Num_terms(); ++i) {

    TERM* term = linex->Get_term(i);
    switch(term->Get_type()) {

      case LTKIND_CONST:
      case LTKIND_LINDEX:
      case LTKIND_SUBSCR:
        break;

      case LTKIND_IV: {
        const IVAR& ivar = ipa_ivar[term->Get_desc()];
        if (ivar.Is_Formal()) {
          INT32 idx = ipa_proc->Get_formal_index() + ivar.Formal_Position();
          const SUMMARY_FORMAL& formal = ipa_formal[idx];
          *modified |= Is_formal_modified(node, formal);
          if (ivar.Offset() == 0) {
            Update_term_with_formal_constant(node, formal, term);
          }
        }
        else {
          ST* st = ST_ptr(ivar.St_Idx());
          ST_IDX base_st = ST_base_idx(st);
          INT64 offset = ST_ofst(st) + ivar.Offset();
          TYPE_ID mtype = ivar.Mtype();
          Update_term_with_global_constant(node, base_st, offset, mtype, term);
          *modified |= Is_global_modified(node, base_st);
        }
        break;
      }

      default:
        Fail_FmtAssertion("update_linex_with_constant_mod: LTKIND_NONE");
        break;
    }
  }

  IPA_NODE_SECTION_INFO *section = node->Section_Annot();
  linex->Simplify();

  return linex;
}

//-------------------------------------------------------
// Update the loop information
//-------------------------------------------------------
static void 
update_loop_info(IPA_NODE* node, LOOPINFO* loop_info)
{
  BOOL is_mod;

  loop_info->Create_linex(IPA_get_term_array(node));

  LINEX* upper_linex = loop_info->Get_upper_linex();
  if (upper_linex && !loop_info->Is_messy_ub()) {
    LINEX* result = update_linex_with_constant_mod(node, upper_linex, &is_mod);
    result->Copy(upper_linex);
  }

  LINEX* lower_linex = loop_info->Get_lower_linex();
  if (lower_linex && !loop_info->Is_messy_lb()) {
    LINEX* result = update_linex_with_constant_mod(node, lower_linex, &is_mod);
    result->Copy(lower_linex);
  }

  LINEX* step_linex = loop_info->Get_step_linex();
  if (step_linex && !loop_info->Is_messy_step()) {
    LINEX* result = update_linex_with_constant_mod(node, step_linex, &is_mod);
    result->Copy(step_linex);
  }
}

//-------------------------------------------------------
// Update the projected region
//-------------------------------------------------------
static void
update_projected_region(IPA_NODE* node, PROJECTED_REGION* p)
{
  INT start_idx = p->Get_id();
  INT count = p->Get_num_dims();
  INT i;
  
  // Since clones share PROJECTED_NODEs from ummary files,
  // they need to be copied, rathern than changed in places
  PROJECTED_NODE* copy_pn = (PROJECTED_NODE*) 
    MEM_POOL_Alloc(&IPA_array_prop_pool, count * sizeof(PROJECTED_NODE));
  bcopy(IPA_get_projected_node_array(node) + start_idx, 
        copy_pn, 
        count * sizeof(PROJECTED_NODE));
  for (i = 0; i < count; i++) {
    copy_pn[i].Set_Mem_Pool(&IPA_array_prop_pool);
  }
  start_idx = 0;

  for (i = start_idx; i < start_idx + count; ++i) {
    
    BOOL has_mod;
    LINEX* l;
  
    // create linexes
    PROJECTED_NODE* pnode = &copy_pn[i];
    pnode->Create_linex(IPA_get_term_array(node));

    LINEX* upper_linex = pnode->Get_upper_linex();
    if (upper_linex) {
      l = update_linex_with_constant_mod(node, upper_linex, &has_mod);
      pnode->Set_upper_linex(l);
      if (has_mod) {
        p->Set_messy_region();
      }
    }

    LINEX* lower_linex = pnode->Get_lower_linex();
    if (lower_linex) {
      l = update_linex_with_constant_mod(node, lower_linex, &has_mod);
      pnode->Set_lower_linex(l);
      if (has_mod) {
        p->Set_messy_region();
      }
    }
    
    LINEX* step_linex = pnode->Get_step_linex();
    if (step_linex) {
      l = update_linex_with_constant_mod(node, step_linex, &has_mod);
      pnode->Set_step_linex(l);
      if (has_mod) {
        p->Set_messy_region();
      }
    }

    LINEX* segment_length_linex = pnode->Get_segment_length_linex();
    if (segment_length_linex) {
      l = update_linex_with_constant_mod(node, segment_length_linex, &has_mod);
      pnode->Set_segment_length_linex(l);
      if (has_mod) {
        p->Set_messy_region();
      }
    }

    LINEX* segment_stride_linex = pnode->Get_segment_stride_linex();
    if (segment_stride_linex) {
      l = update_linex_with_constant_mod(node, segment_stride_linex, &has_mod);
      pnode->Set_segment_stride_linex(l);
      if (has_mod) {
        p->Set_messy_region();
      }
    }
  }
  
  IPA_NODE_SECTION_INFO* info = node->Section_Annot();
  PROJECTED_ARRAY* parray = CXX_NEW(PROJECTED_ARRAY(info->Mem_Pool()),
                                    info->Mem_Pool());
  p->Set_projected_array(parray);
  for (i = start_idx; i < start_idx + count; ++i) {
    p->Set_projected_node(&copy_pn[i]);
  }
}

//----------------------------------------------------------------
// Update the region array
// Merge regions to compute DMOD after constant propagation and
// scalar mod information have been propagated
// NOTE: an easy extension to this is DUSE
// Need to set it to bottom if bad alias and or messy region
// A region is set to messy if it has non-constant symbolic terms
//-----------------------------------------------------------------
static void 
update_region(IPA_NODE* node, REGION_ARRAYS* r)
{
  INT32 start_idx = r->Get_idx();
  INT32 count = r->Get_count();
  INT i;

  // Because clones share PROJECTED_REGIONs from summary files
  // they need to be copied, rather than changed in place
  PROJECTED_REGION* copy_pr = (PROJECTED_REGION*)
    MEM_POOL_Alloc(&IPA_array_prop_pool, sizeof(PROJECTED_REGION) * count);
  bcopy(IPA_get_proj_region_array(node) + start_idx, 
        copy_pr, 
        count * sizeof(PROJECTED_REGION));
  for (i = 0; i < count; i++) {
    copy_pr[i].Set_Mem_Pool(&IPA_array_prop_pool);
  }
  start_idx = 0;

  IPA_NODE_SECTION_INFO* info = node->Section_Annot();
  SUMMARY_SYMBOL* sym = IPA_get_symbol_array(node) + r->Get_sym_id();  
  BOOL is_formal = FALSE;
  BOOL is_global = FALSE;
  INT position;

  if (sym->Is_formal()) {
    // get the position of the formal
    position = IPA_get_formal_array(node)[sym->Get_findex()].Get_position();
    is_formal = TRUE;
  }
  else if (ST_IDX_level(sym->St_idx()) == GLOBAL_SYMTAB) {
    is_global = TRUE;
  }

  for (i = start_idx; i < start_idx + count; ++i) {

    PROJECTED_REGION* pr;
    PROJECTED_REGION* p = &copy_pr[i - start_idx];
    if (!p->Is_messy_region()) {
      update_projected_region(node, p);
    }

    // if formal and a region exists
    if (is_formal) {
      if (r->Is_def()) {
        if (pr = info->Get_formal_mod_region(position))
          pr->May_Union(*p, Trace_IPA_Sections);
        else
          info->Set_formal_mod_region(position, p);
      }
      if (r->Is_use()) {
        if (pr = info->Get_formal_ref_region(position))
          pr->May_Union(*p, Trace_IPA_Sections);
        else
          info->Set_formal_ref_region(position, p);
      }
      if (r->Is_formal()) {
        if (pr = info->Get_formal_dcl_region(position))
          pr->May_Union(*p, Trace_IPA_Sections);
        else
          info->Set_formal_dcl_region(position, p);
      }
    }
    else if (is_global) {
      BOOL is_messy;
      if (r->Is_def() || r->Is_may_def()) {
        if (pr = info->Global_Array_Region(sym, &is_messy, p, TRUE))
          if (pr != p)
            pr->May_Union(*p, Trace_IPA_Sections);
      }
      if (r->Is_use() || r->Is_may_use()) {
        if (pr = info->Global_Array_Region(sym, &is_messy, p, FALSE))
          if (pr != p)
            pr->May_Union(*p, Trace_IPA_Sections);
      }
    }
  }
}


//-------------------------------------------------------
// Update the following, for each formal and common block 
// element, if the value is constant on entry and is NOT
// modified by the PU, then update the term element(s)
//-------------------------------------------------------
static void
update_mod_const_sections(IPA_NODE *node)
{
  // initialize global arrays used in ipa_section_print.cxx
  Init_IPA_Print_Arrays (node);

  CFG_NODE_INFO* ipa_cfg = IPA_get_cfg_node_array (node);
  REGION_ARRAYS* ipa_regions_array = IPA_get_region_array (node);
  LOOPINFO* ipa_loopinfo = IPA_get_loopinfo_array (node);
  SUMMARY_PROCEDURE* ipa_proc = node->Summary_Proc();

  if (ipa_proc->Has_incomplete_array_info())
    return; 

  // walk all the control flow nodes
  INT start_idx = ipa_proc->Get_array_section_index();
  INT end_idx = ipa_proc->Get_array_section_count() + start_idx;
  INT i;
  for (i = start_idx; i < end_idx; ++ i) {

    // mod region array
    CFG_NODE_INFO* cfg_info = &ipa_cfg[i];
    INT start_region_idx = cfg_info->Get_def_index();
    INT end_region_idx = cfg_info->Get_def_count() + start_region_idx;
    INT j;
    
    for (j = start_region_idx; j < end_region_idx; ++j) {
      update_region(node, &ipa_regions_array[j]);
    }

    // use region array
    start_region_idx = cfg_info->Get_use_index();
    end_region_idx = cfg_info->Get_use_count() + start_region_idx;
    for (j = start_region_idx; j < end_region_idx; ++j) {
      update_region(node, &ipa_regions_array[j]);
    }
      
    // passed region array
    start_region_idx = cfg_info->Get_param_index();
    end_region_idx = cfg_info->Get_param_count() + start_region_idx;
    for (j = start_region_idx; j < end_region_idx; ++j) {
      update_region(node, &ipa_regions_array[j]);
    }

    // formal region array
    start_region_idx = cfg_info->Get_formal_index();
    end_region_idx = cfg_info->Get_formal_count() + start_region_idx;
    for (j = start_region_idx; j < end_region_idx; ++j) {
      update_region(node, &ipa_regions_array[j]);
    }

    // loop info
    if (cfg_info->Is_do_loop()) {
      // Since clone share LOOPINFOs from summary files,
      // they need to be copied, rather than changed in place
      if (ipa_loopinfo != NULL) {
	LOOPINFO* l = &ipa_loopinfo[cfg_info->Get_loop_index()-1];
	LOOPINFO* copy_li = (LOOPINFO*)
	  MEM_POOL_Alloc(&IPA_array_prop_pool, sizeof(LOOPINFO));
	bcopy(l, copy_li, sizeof(LOOPINFO));
	copy_li->Set_Mem_Pool(&IPA_array_prop_pool);
	update_loop_info(node, copy_li);
      } 
    }
  }
}

//-------------------------------------------------------
// Walk the graph and initialize node information
//-------------------------------------------------------
static void
init_node_section_annot (IPA_NODE* node)
{
  if (node->Section_Annot() == NULL) {

    // If PU had messy regions and we've discovered some
    // constants, try to clean up PU and rebuild sections
    if (IPA_Enable_Preopt &&
        node->Has_Propagated_Const() &&
        (node->Summary_Proc()->Has_messy_regions() ||
         (IPA_Max_Node_Clones_Set && IPA_Max_Node_Clones != 0))) {
      IPA_Preoptimize(node);
    }

    SUMMARY_PROCEDURE* ipa_proc = node->Summary_Proc();

    IPA_NODE_SECTION_INFO* asection =
      CXX_NEW(IPA_NODE_SECTION_INFO(&IPA_array_prop_pool),
              &IPA_array_prop_pool);
    node->Set_Section_Annot(asection);

    INT count = node->Num_Formals();
    if (count > 0) {

      STATE_ARRAY* state = 
        CXX_NEW(STATE_ARRAY(&IPA_array_prop_pool), &IPA_array_prop_pool);
      state->Force_Alloc_array(count);
      state->Setidx(count-1);
      asection->Set_formals(state);  

      SUMMARY_FORMAL* formal = 
        IPA_get_formal_array(node) + ipa_proc->Get_formal_index();
      for (INT j = 0; j < count; ++j, ++formal) {
        (*state)[j].Init();
        if (TY_kind(formal->Get_ty()) != KIND_POINTER ||
            TY_kind(TY_pointed(formal->Get_ty())) != KIND_ARRAY) {
          (*state)[j].Set_is_scalar();
        }
      }
    }

    CFG_NODE_INFO* ipa_cfg = IPA_get_cfg_node_array (node);
    if (ipa_cfg) {
      if (ipa_proc->Get_array_section_count() != 0) {
        asection->Set_cfg_node(&ipa_cfg[ipa_proc->Get_array_section_index()]);
      }
    }

    update_mod_const_sections(node);

    Update_Execution_Cost(node, &IPA_array_prop_pool);
  }
} // init_node_section_annot 


// ==========================================
// Implementation for IPA_ARRAY_DF_FLOW class
// ==========================================

//----------------------------------------------------------------------
// Constructor
//----------------------------------------------------------------------
IPA_ARRAY_DF_FLOW::IPA_ARRAY_DF_FLOW(IPA_CALL_GRAPH*, 
                                     DF_DIRECTION ddf, 
                                     MEM_POOL *m)
  : IPA_DATA_FLOW(ddf,m)
{
  // the same pool is used by the scalar kill/euse analysis
  if (!IPA_array_prop_pool_initialized) {
    MEM_POOL_Initialize(&IPA_array_prop_pool, "ipa_array_prop_pool", 0);
    IPA_array_prop_pool_initialized = TRUE;
  }

  Trace_IPA_Sections = Get_Trace(TP_IPA, IPA_TRACE_SECTIONS);
}

//----------------------------------------------------------------------
// Interprocedural array sections, initialize each NODE in the
// call graph by mapping all the section data structures into pointers
// so that manipulating, modifying, deleting etc. is straightforward
//----------------------------------------------------------------------
void
IPA_ARRAY_DF_FLOW::InitializeNode(void* n)
{
  // set up independent variable arrays only once per file
  IPA_NODE* node = (IPA_NODE*) n;
  IP_FILE_HDR& file_hdr = node->File_Header();
  if (IP_FILE_HDR_section_annot(file_hdr) == NULL) {
    INT32 size;
    IVAR* ivar = IPA_get_ivar_array(node, size);
    SECTION_FILE_ANNOT* section_annot  = 
      CXX_NEW(SECTION_FILE_ANNOT(ivar, &IPA_array_prop_pool),
              &IPA_array_prop_pool);
    Set_IP_FILE_HDR_section_annot(file_hdr, section_annot);
  }

  init_node_section_annot(node);
}

//----------------------------------------------------------------------
// Meet operation, don't do anything yet
//----------------------------------------------------------------------
void*
IPA_ARRAY_DF_FLOW::Meet(void*, void*, INT*)
{
  return 0;
}

// -----------------------------------------------------------------
// After the array section propagation is finished, disable inlining 
// in cases where it would make the analysis less precise
// -----------------------------------------------------------------
void 
IPA_ARRAY_DF_FLOW::PostProcessIO(void* n)
{
  if (!n) {
    return;
  }
  IPA_NODE* node = (IPA_NODE*) n;

  // don't override user's explicit request to inline this function
  if (node->Has_Must_Inline_Attrib()) {
    return;
  }

  // we are already conservative: inlining will not make it worse
  if (node->Summary_Proc()->Has_incomplete_array_info()) {
    return;
  }

  IPA_NODE_SECTION_INFO* section = node->Section_Annot();
  IPAA_NODE_INFO* modref = node->Mod_Ref_Info();
  // no array section or scalar mod/ref info
  if (!section || !modref) {
    return;
  }

  // if scalar formals are both read and written, or if read/write
  // regions for array formals are messy, inlining will not make it worse
  UINT32 num_formals = node->Num_Formals();
  for (UINT32 pos = 0; pos < num_formals; ++pos) {
    STATE* formal = section->Get_formal(pos);
    if (formal->Is_scalar()) {
      if ((modref->Is_formal_dmod_elmt(pos) || 
           modref->Is_formal_imod_elmt(pos)) &&
          (modref->Is_formal_dref_elmt(pos) || 
           modref->Is_formal_iref_elmt(pos))) {
        return;
      }
    }
    else {
      PROJECTED_REGION* mod = formal->Get_projected_mod_region();
      PROJECTED_REGION* ref = formal->Get_projected_ref_region();
      if (mod && (mod->Is_messy_region() || (ref && ref->Is_messy_region()))) {
        return;
      }
    }
  }

  SUMMARY_SYMBOL* symbols = IPA_get_symbol_array(node);
  SUMMARY_ACTUAL* actuals = IPA_get_actual_array(node);
  
  // same for global arrays (should probably do scalars as well)
  ST_IDX st_idx;
  GLOBAL_ARRAY_LIST* ga_list;
  GLOBAL_ARRAY_TABLE* ga_table = section->Global_Array_Table();
  GLOBAL_ARRAY_TABLE_ITER ga_table_iter(ga_table);
  while (ga_table_iter.Step(&st_idx, &ga_list)) {
    if (ga_list->Is_messy()) {
      return;
    }
    GLOBAL_ARRAY_LIST_ITER ga_list_iter(ga_list);
    for (ga_list_iter.First(); !ga_list_iter.Is_Empty(); ga_list_iter.Next()) {
      GLOBAL_ARRAY_INFO* ga_info = ga_list_iter.Cur();
      PROJECTED_REGION* mod = ga_info->Get_projected_mod_region();
      PROJECTED_REGION* ref = ga_info->Get_projected_ref_region(); 
      if (mod && (mod->Is_messy_region() || (ref && ref->Is_messy_region()))) {
        return;
      }
    }
  }
  
  // if a callee has messy formals, and the corresponding actuals
  // are local to this node, then we should NOT inline, since it
  // will expose a dependence that wasn't there before inlining
  IPA_SUCC_ITER edge_iter(IPA_Call_Graph, node);
  for (edge_iter.First(); !edge_iter.Is_Empty(); edge_iter.Next()) {
    IPA_EDGE* e = edge_iter.Current_Edge();
    if (!e) {
      continue;
    }
    IPA_NODE* callee = IPA_Call_Graph->Callee(e);
    IPA_NODE_SECTION_INFO* section = callee->Section_Annot();
    IPAA_NODE_INFO* modref = node->Mod_Ref_Info();
    if (!section || !modref) {  
      node->Set_Noinline_Attrib();
      return;
    }

    INT32 actual_index = e->Summary_Callsite()->Get_actual_index();
    UINT32 num_formals = callee->Num_Formals();
    for (UINT32 pos = 0; pos < num_formals; ++pos) {
      STATE* formal = section->Get_formal(pos);
      if (formal->Is_scalar()) {
        if ((modref->Is_formal_dmod_elmt(pos) || 
             modref->Is_formal_imod_elmt(pos)) &&
            (modref->Is_formal_dref_elmt(pos) || 
             modref->Is_formal_iref_elmt(pos))) {
          INT32 symbol_index = actuals[actual_index+pos].Get_symbol_index();
          if (symbol_index != -1 &&
              ST_IDX_level(symbols[symbol_index].St_idx()) != GLOBAL_SYMTAB) {
            node->Set_Noinline_Attrib();
            return;
          }
        }
      }
      else {
        PROJECTED_REGION* mod = formal->Get_projected_mod_region();
        PROJECTED_REGION* ref = formal->Get_projected_ref_region();
        // TODO: nenad, 98/12/19
        // to be precise in the case when mod region is not messsy,
        // we should check that mod and ref regions are not disjoint
        if (mod && (mod->Is_messy_region() || ref)) {
          INT32 symbol_index = actuals[actual_index+pos].Get_symbol_index();
          if (symbol_index != -1 &&
              ST_IDX_level(symbols[symbol_index].St_idx()) != GLOBAL_SYMTAB) {
            node->Set_Noinline_Attrib();
            return;
          }
        }
      }
    }
  }
}

//----------------------------------------------------------------------
// Transfer operation, merge sections from callee to caller
// If change in annotation, set change to TRUE else set to FALSE
//----------------------------------------------------------------------
void*
IPA_ARRAY_DF_FLOW::Trans(void*, void*, void* vertex, INT* change)
{
  *change |= Merge_Section((IPA_NODE*)vertex);
  return 0;
}

//----------------------------------------------------------------------
// map and write information out for use by LNO
//----------------------------------------------------------------------
void
IPA_ARRAY_DF_FLOW::Print_entry(FILE* fp, void*, void* n)
{
  IPA_NODE* node = (IPA_NODE*)n;
  node->Print(fp);
  if (node->Section_Annot()) {
    node->Section_Annot()->Print(fp);
  }
}


// =============================================================
// Implementation of the class for propagating formal parameters
// that are used as symbolic terms in array sections (array 
// references and/or loop bounds). This information will be 
// used to decide when cloning may be profitable.
// =============================================================

// -----------
// Constructor
// -----------
IPA_FORMALS_IN_ARRAY_SECTION_DF::
IPA_FORMALS_IN_ARRAY_SECTION_DF(IPA_CALL_GRAPH*, 
                                DF_DIRECTION ddf, 
                                MEM_POOL* m)
  : IPA_DATA_FLOW(ddf,m)
{}

// -----------------------------
// All the work is done in Trans
// -----------------------------
void*
IPA_FORMALS_IN_ARRAY_SECTION_DF::Meet(void*, void*, INT*)
{
  return 0;
}


// These are made file statics so that we don't pass 
// 6 arguments into possibly deep recursive calls
// They are all from the summary of the caller, i.e., node in Trans
static SUMMARY_EXPR*   expressions;
static SUMMARY_VALUE*  values;
static SUMMARY_FORMAL* formals;

static BOOL
Value_has_only_formals(SUMMARY_VALUE* value,
                       mBOOL* formals_used);

// -------------------------------------------
// Check if an expression contains only formal 
// parameters (and optional constants).
// -------------------------------------------
static BOOL
Expr_has_only_formals(SUMMARY_EXPR* expr,
                      mBOOL* formals_used)
{
  if (expr->Is_expr_unknown()) {
    return FALSE;
  }
  if (expr->Has_const_operand()) {
    INT32 kid = expr->Get_kid();
    INT32 non_const_idx = expr->Get_node_index(kid);
    if (expr->Is_expr_value(kid)) {
      return Value_has_only_formals(values + non_const_idx, formals_used);
    }
    else if (expr->Is_expr_expr(kid)) {
      return Expr_has_only_formals(expressions + non_const_idx, formals_used);
    }
    else {
      return FALSE;
    }
  }
  else {
    INT32 kid0_idx = expr->Get_node_index(0);
    INT32 kid1_idx = expr->Get_node_index(1);
    if (((expr->Is_expr_value(0) &&
          Value_has_only_formals(values + kid0_idx, formals_used)) ||
         (expr->Is_expr_expr(0) &&
          Expr_has_only_formals(expressions + kid0_idx, formals_used))) &&
        ((expr->Is_expr_value(1) &&
          Value_has_only_formals(values + kid1_idx, formals_used)) ||
         (expr->Is_expr_expr(1) &&
          Expr_has_only_formals(expressions + kid1_idx, formals_used)))) {
      return TRUE;
    }
    else {
      return FALSE;
    }
  }
}

// ------------------------------------------------------------------------
// Check if a value ontains only formal parameters (and optional constants)
// ------------------------------------------------------------------------
static BOOL
Value_has_only_formals(SUMMARY_VALUE* value,
                       mBOOL* formals_used)
{
  if (value->Is_formal()) {
    formals_used[formals[value->Get_formal_index()].Get_position()] = TRUE;
    return TRUE;
  }
  else if (value->Is_expr()) {
    SUMMARY_EXPR* expr = expressions + value->Get_expr_index();
    return Expr_has_only_formals(expr, formals_used);
  }
  else if (value->Is_int_const() || value->Is_const_st()) {
    return TRUE;
  }
  else {
    return FALSE;
  }
}
    
//----------------------------------------------------------------------
// Transfer operation, merge sections from callee to caller
// If change in annotation, set change to TRUE.
//----------------------------------------------------------------------
void*
IPA_FORMALS_IN_ARRAY_SECTION_DF::Trans(void*, 
                                       void*, 
                                       void* vertex, 
                                       INT* change)
{
  IPA_NODE* node = (IPA_NODE*)vertex;
  UINT32 num_formals = node->Num_Formals();
  if (num_formals == 0) return 0;

  // BOOL vector for marking formals used in passing actual arguments
  mBOOL* formals_used = (mBOOL*) alloca(num_formals * sizeof(mBOOL));

  // Initialize static summary arrays
  expressions = IPA_get_expr_array (node);
  values      = IPA_get_value_array (node);
  formals     = IPA_get_formal_array (node);

  SUMMARY_FORMAL* caller_formals = formals 
                                 + node->Summary_Proc()->Get_formal_index();
  SUMMARY_ACTUAL* actuals = IPA_get_actual_array(node);
  SUMMARY_SYMBOL* symbols = IPA_get_symbol_array(node);
      
  // walk the calls for this procedure
  IPA_SUCC_ITER edge_iter(IPA_Call_Graph, node);
  for (edge_iter.First(); !edge_iter.Is_Empty(); edge_iter.Next()) {
    IPA_EDGE* e = edge_iter.Current_Edge();
    if (e) {
      SUMMARY_CALLSITE* callsite = e->Summary_Callsite();
      SUMMARY_ACTUAL* call_actuals = actuals + callsite->Get_actual_index();
      IPA_NODE* callee = IPA_Call_Graph->Callee(e);
      SUMMARY_PROCEDURE* callee_proc = callee->Summary_Proc();
      SUMMARY_SYMBOL* callee_symbols = IPA_get_symbol_array(callee);
      SUMMARY_FORMAL* callee_formals = IPA_get_formal_array(callee)
                                     + callee_proc->Get_formal_index();
      UINT32 count = callee->Num_Formals();
      if (count > callsite->Get_param_count()) {
        count = callsite->Get_param_count();
      }
      
      for (INT32 i = 0; i < count; ++i) {
        INT32 callee_formal_sym_idx = callee_formals[i].Get_symbol_index();
        if (callee_symbols[callee_formal_sym_idx].Used_in_array_section()) {
          // Map the actual argument to the caller's formals (if any)
          INT32 actual_value_idx = call_actuals[i].Get_value_index();
          if (actual_value_idx != -1) {
            bzero(formals_used, num_formals * sizeof(mBOOL));
            SUMMARY_VALUE* actual_value = values + actual_value_idx;
            if (Value_has_only_formals(actual_value, formals_used)) {
              // Set used_in_array_section bits based on formals_used
              for (INT32 j = 0; j < num_formals; ++j) {
                if (formals_used[j]) {
                  INT32 formal_sym_idx = caller_formals[j].Get_symbol_index();
                  if (!symbols[formal_sym_idx].Used_in_array_section()) {
                    symbols[formal_sym_idx].Set_used_in_array_section();
                    *change = TRUE;
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  return 0;
}

//-------------------------------------------------------
// Print a node's annotation for this data-flow problem
//-------------------------------------------------------
void
IPA_FORMALS_IN_ARRAY_SECTION_DF::Print_entry(FILE* fp, 
                                             void*, 
                                             void* vertex)
{
  IPA_NODE* node = (IPA_NODE*)vertex;
  UINT32 num_formals = node->Num_Formals();
  if (num_formals == 0) return;

  SUMMARY_SYMBOL* symbols = IPA_get_symbol_array(node);
  SUMMARY_FORMAL* formals = IPA_get_formal_array(node) 
                          + node->Summary_Proc()->Get_formal_index();

  fprintf(fp,"PU %s should be cloned", node->Name());
  BOOL should_clone = FALSE;

  for (INT32 i = 0; i < num_formals; ++i) {
    SUMMARY_SYMBOL* s = symbols + formals[i].Get_symbol_index();
    if (s->Used_in_array_section()) {
      fprintf(fp, "\n  if formal [%d] can be exposed as constant", i);
      should_clone = TRUE;
    }
  }

  if (!should_clone) {
    fprintf(fp, " - NOT!");
  }
  fprintf(fp, "\n");
}

