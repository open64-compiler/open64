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
//
// Exported functions:
//
//  extern void IPL_Access_Vector_To_Projected_Region(..)
//  extern void IPL_Finalize_Projected_Regions(..)
//
//--------------------------------------------------------------------------
#include <stdint.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <sys/elf_whirl.h>
#include <sys/types.h>

#include "defs.h"
#include "errors.h"
#include "tracing.h"
#include "wn.h"
#include "stab.h"                   /* for symtab    */
#include "const.h"                  /* for constab   */
#include "targ_const.h"             /* for tcon      */
#include "pu_info.h"
#include "irbdata.h"
#include "ir_bwrite.h"
#include "ir_bread.h"               /* for read routines routines */
#include "ir_bcom.h"
#include "loop_info.h"
#include "if_info.h"
#include "ipl_linex.h"
#include "ipa_section.h"
#include "cxx_hash.h"
#include "wn_util.h"
#include "lwn_util.h"
#include "ipl_summary.h"
#include "ipl_summarize.h"
#include "ipl_summarize_util.h"
#include "ipl_array_bread_write.h"
#include "ipl_tlog.h"
#include "ipl_main.h"
#include "ipl_lno_util.h"
#include "wb_ipl.h" 

ARRAY_SUMMARY Array_Summary;

static SUMMARY_PROCEDURE* Current_summary_procedure = NULL;
static CFG_NODE_INFO *Cfg_entry = NULL;
static INT Cfg_entry_idx = -1;

//---------------------------------------------------------------------
// process a scalar reduction node
// check to see how far out it should go
//---------------------------------------------------------------------
static void 
process_scalar_reduc_node(WN* stmt, ST* lhs_st)
{
  BOOL branch;
  INT loop_idx;
  INT stmt_idx;
  INT cd_idx;
  WN* loop = NULL;
  SUMMARY_CONTROL_DEPENDENCE* d = Get_controlling_stmt(stmt);

  while (d) {
    cd_idx = Get_cd_idx(d);
    if (d->Is_do_loop()) {
      loop = d->Get_wn();
      loop_idx = cd_idx;
      d = Get_controlling_stmt(loop); 
    }
    else {
      if (loop && d->Is_entry()) {
        cd_idx = loop_idx;
      }
      else if (d->Is_if_stmt()) {
        SUMMARY_STMT* summary_stmt = 
          Search_for_summary_stmt(loop ? loop : stmt, branch, stmt_idx);
        Is_True(summary_stmt,("process_scalar_reduc_node: NULL summary stmt"));
      }

      CFG_NODE_INFO* cfg_node = Array_Summary.Get_cfg_node_array(cd_idx);
      if (d->Is_if_stmt() && !branch) {
        cfg_node= Array_Summary.Get_cfg_node_array(cfg_node->Get_else_index());
      }
      cfg_node->Add_scalar_reduc(Summary->Get_symbol_index(lhs_st));
      return;
    }
  }

  // it is under some messy control flow
  Cfg_entry->Add_scalar_may_reduc(Summary->Get_symbol_index(lhs_st));
}

//---------------------------------------------------------------------
// process a scalar definition node
// check to see how far out it should go
//---------------------------------------------------------------------
static void 
process_scalar_def_node(WN* stmt, ST* lhs_st)
{
  BOOL branch;
  INT loop_idx;
  INT stmt_idx;
  INT cd_idx;
  CFG_NODE_INFO* cfg_node;
  WN* loop = NULL;
  INT id = Summary->Get_symbol_index(lhs_st);
  // PV 667258: If the user is writing a "constant", it must be under a 
  // conditional that he promises will never execute.  Therefor, we can 
  // ignore it. (rcox)
  if (id == -1)
    return; 
 
  SUMMARY_CONTROL_DEPENDENCE* d =  Get_controlling_stmt(stmt);

  while (d) {
    cd_idx = Get_cd_idx(d);
    if (d->Is_do_loop()) {
      loop = d->Get_wn();
      loop_idx = cd_idx;
      d = Get_controlling_stmt(loop); 
    }
    else { 
      // if the loop variable was set then add the definition
      // to the loop node
      if (loop && d->Is_entry()) {
        cd_idx = loop_idx;
        cfg_node = Array_Summary.Get_cfg_node_array(cd_idx);
      }
      else {
        if (d->Is_if_stmt()) {
          SUMMARY_STMT* summary_stmt = 
            Search_for_summary_stmt(stmt, branch, stmt_idx);
          Is_True(summary_stmt, ("process_scalar_def: NULL summary stmt"));
          cd_idx = Get_cd_idx(d);
          cfg_node = Array_Summary.Get_cfg_node_array(cd_idx);
          if (!branch) {
            cd_idx = cfg_node->Get_else_index();
            cfg_node = Array_Summary.Get_cfg_node_array(cd_idx);
          }
        }
        else {
          cd_idx = Get_cd_idx(d);
          cfg_node = Array_Summary.Get_cfg_node_array(cd_idx);
        }
      }
      

      // if it is the entry node add it to the symbols array
      if (cfg_node->Is_entry()) {
        Summary->Get_symbol(id)->Set_dkill();
      }

      // store it as part of the cfg node information
      cfg_node->Add_scalar_def(id);
      return;
    }
  }

  // is under a messy section, add it to the entry node
  Cfg_entry->Add_scalar_may_def(id);
}

//---------------------------------------------------------------------
// process a scalar definition node
// check to see how far out it should go
//---------------------------------------------------------------------
static void 
process_scalar_node(WN* node, IPA_SECTION_TYPE type)
{
  ST* s = WN_st(node);
  INT id = Summary->Get_symbol_index(s);

  // don't do anything if ST is constant
  if (id == -1) {
    return;
  }

  SUMMARY_STMT* summary_stmt;
  CFG_NODE_INFO* cfg_node;
  WN* loop = NULL;
  BOOL branch = FALSE;
  INT loop_idx;
  INT stmt_idx;

  INT cd_idx = -1;
  WN* stmt = IPL_get_stmt_scf(node);
  SUMMARY_CONTROL_DEPENDENCE* d = Get_controlling_stmt(stmt);

  while (d)
    {
      cd_idx = Get_cd_idx(d);
      if (d->Is_do_loop())
	{
	  loop = d->Get_wn();
	  loop_idx = cd_idx;
	  d = Get_controlling_stmt(loop); 
	}
      else
	{
	  // attach it at the loop header rather than the entry point
	  if ((loop) && (d->Is_entry()))
	    cd_idx = loop_idx;

	  // get the branch information
	  // if it is not part of some array then it is
	  // likely to be part of some control flow node
	  // and hence it belongs to the def set of that
	  // control flow node
	  cfg_node = Array_Summary.Get_cfg_node_array(cd_idx);

	  if (d->Is_if_stmt() && branch == FALSE)
	    {
	      cfg_node = 
		Array_Summary.Get_cfg_node_array(cfg_node->Get_else_index());
	    }
	  
	  switch( type )
	    {
	    case IPA_USE: {
	      // if there is a kill then don't bother with setting the
	      // euse bit
	      SUMMARY_SYMBOL *symbol = Summary->Get_symbol(id);
	      if (!symbol->Is_dkill())
		cfg_node->Add_scalar_use(id);
              }
	      break;
	      
	    case IPA_DEF:
	      cfg_node->Add_scalar_def(id);
	      break;
	      
	    case IPA_REDUC:
	      cfg_node->Add_scalar_reduc(id);
	      break;
	      
	    default:
	      Fail_FmtAssertion("unknown scalar type %d \n", id);
	      break;
	    }
	  return;
	}
    }
  
  if (loop)
    {
      d = Get_cd_by_idx(cd_idx);
      FmtAssert(d != NULL, (" Expecting a cd node \n"));

      if (d->Is_if_stmt())
	{
	  summary_stmt = Search_for_summary_stmt(loop, branch, stmt_idx);
	  FmtAssert(summary_stmt != NULL,("process_scalar_node: NULL summary stmt"));      
	}

      cfg_node = Array_Summary.Get_cfg_node_array(cd_idx);

      if (d->Is_if_stmt() && branch == FALSE)
	{
	  cfg_node = 
	    Array_Summary.Get_cfg_node_array(cfg_node->Get_else_index());
	}

      id = Summary->Get_symbol_index(s);

      switch( type )
	{
	case IPA_USE: {
	    SUMMARY_SYMBOL *symbol = Summary->Get_symbol(id);
	    if (!symbol->Is_dkill())
	      cfg_node->Add_scalar_use(id);
 	  } 
	  break;
	      
	case IPA_DEF:
	  cfg_node->Add_scalar_def(id);
	  break;
	      
	case IPA_REDUC:
	  cfg_node->Add_scalar_reduc(id);
	  break;

	default:
	  Fail_FmtAssertion("unknown scalar type %d \n", id);
	  break;
	  
	}
    }

  else
    {
      id = Summary->Get_symbol_index(s);
      switch( type )
	{
	case IPA_USE:
	  Cfg_entry->Add_scalar_may_use(id);
	  break;
	      
	case IPA_DEF:
	  Cfg_entry->Add_scalar_may_def(id);
	  break;
	      
	case IPA_REDUC:
	  Cfg_entry->Add_scalar_may_reduc(id);
	  break;

	default:
	  Fail_FmtAssertion("unknown scalar type %d \n", id);
	  break;
	  
	}
    }
}

//----------------------------------------------------------------
// return the id into the actual array
//----------------------------------------------------------------
static INT
get_actual_id(INT callsite_id, INT param_pos, INT start_idx)
{
  SUMMARY_CALLSITE* callsite = Summary->Get_callsite(callsite_id);
  INT actual_id = callsite->Get_actual_index() + param_pos - start_idx;

  if (Get_Trace(TP_IPL, TT_IPL_SECTION)) {
    fprintf(TFile, "callsite_id = %d , param_pos = %d , actual_id = %d\n", 
            callsite_id, param_pos, actual_id);
  }

  return actual_id;
}

//---------------------------------------------------------------------
// process an actual parameter
//---------------------------------------------------------------------
static void
process_actual_array_node(WN* wn, 
                          INT16 callsite_id,
			  INT16 actual_id)
{
  WN* array_base = WN_array_base(wn);
  while (array_base && (WN_operator(array_base) == OPR_ARRAY)) {
    array_base = WN_array_base(array_base);
  }
  FmtAssert(array_base != NULL,("NULL array base encountered\n"));
  if (!OPCODE_has_sym(WN_opcode(array_base))) {
    return;
  }
  ST* array_st = WN_st(array_base);
  // Currently, we cannot deal with formals from a parent PU (652403)
  if ((ST_sclass(array_st) == SCLASS_FORMAL 
	|| ST_sclass(array_st) == SCLASS_FORMAL_REF) 
	&& ST_level(array_st) != CURRENT_SYMTAB) {
    Current_summary_procedure->Set_has_incomplete_array_info();
    return;
  }
  // Skip local arrays
  if (ST_sclass(array_st) == SCLASS_AUTO) {
    return;
  }
  TY_IDX array_ty_idx = (ST_sclass(array_st) != SCLASS_FORMAL ?
                         ST_type(array_st) :
                         TY_pointed(ST_type(array_st)));
  // array within structs have ST of the struct plus offset
  // currently we can't deal with them (they'll be treated as scalars)
  if (TY_kind(array_ty_idx) != KIND_ARRAY) {
    return;
  }

  ACCESS_ARRAY* av = (ACCESS_ARRAY*) WN_MAP_Get(IPL_info_map, wn);
  FmtAssert(av != NULL, ("Null access vector encountered \n"));

  MEM_POOL* apool = Array_Summary.Get_array_pool();
  PROJECTED_REGION* proj_region = 
    CXX_NEW(PROJECTED_REGION(av, apool, NULL), apool);  
  
  CFG_NODE_INFO* cfg_node;

  // get the statement for this array expression
  WN* stmt = IPL_get_stmt_scf(wn);

  // get the control structure surrounding the statement
  SUMMARY_CONTROL_DEPENDENCE* d =  Get_controlling_stmt(stmt);

  INT cd_idx = d ? Get_cd_idx(d) : -1;

  if (cd_idx != -1) {
    cfg_node =  Array_Summary.Get_cfg_node_array(cd_idx);
    if (d->Is_if_stmt()) {
      BOOL branch;
      INT stmt_idx;
      SUMMARY_STMT* summary_stmt = 
        Search_for_summary_stmt (stmt, branch, stmt_idx);
      FmtAssert (summary_stmt != NULL,
                 ("process_actual_array_node: NULL summary stmt"));
      if (branch == FALSE) {
        cfg_node = 
          Array_Summary.Get_cfg_node_array(cfg_node->Get_else_index());
      }
    }
  }
  else {
    cfg_node = Cfg_entry;
  }

  // add this projected region to the entry node, It contains all the
  // call parameters
  INT element_size = TY_size(TY_etype(array_ty_idx));
  INT id =  Summary->Get_symbol_index(array_st);
  cfg_node->Add_array_param(proj_region, id, element_size, 
                            callsite_id, actual_id);
  INT actual_index = get_actual_id(callsite_id, actual_id, 0);
  SUMMARY_ACTUAL* actual = Summary->Get_actual(actual_index);
  actual->Set_symbol_index(id);
}

//----------------------------------------------------------------
// process an actual node, see if it is an array. If yes, then
// build the linex information
//----------------------------------------------------------------
static void
process_actual_node(WN* call_stmt, WN* parm_wn, INT callsite_id, INT param_pos)
{
  WN* actual = WN_kid0(parm_wn);

  if (WN_operator(actual) == OPR_ARRAY) {
    process_actual_array_node(actual, callsite_id, param_pos);
  }

#if _THIS_SEEMS_USELESS_
  // if actual is a formal parameter, record the passed bit
  if (OPERATOR_has_sym(WN_operator(actual))) {
    ST* st = WN_st(actual);
    if (ST_sclass(st) == SCLASS_FORMAL_REF || ST_sclass(st) == SCLASS_FORMAL) {
      INT id = Summary->Get_symbol_index(st);
      SUMMARY_CONTROL_DEPENDENCE* d = Get_controlling_stmt(call_stmt);

      if (d) {
        BOOL branch;
        if (d->Is_if_stmt()) {
          INT stmt_idx;
          SUMMARY_STMT* summary_stmt = 
            Search_for_summary_stmt(call_stmt, branch, stmt_idx);
          FmtAssert(summary_stmt, ("process_actual_node: NULL summary stmt"));
        }
        INT cd_idx = Get_cd_idx(d);
        // store it as part of the cfg node information
        CFG_NODE_INFO* cfg_node = Array_Summary.Get_cfg_node_array(cd_idx);
        if (d->Is_if_stmt() && branch == FALSE) {
          cd_idx = cfg_node->Get_else_index();
          cfg_node = Array_Summary.Get_cfg_node_array(cd_idx);
        }
        // in this case record passed by ref for the scalar
        // pass it the id into the actual array, which has
        // an index into the summary symbol array
        INT pid  = cfg_node->Add_scalar_ref_passed(id, callsite_id);
        INT actual_idx = get_actual_id(callsite_id, param_pos,
                                       Array_Summary.Get_actual_start_idx());
        // note the 1, to avoid zero values
        Array_Summary.Set_actual_scalar_info_map(pid+1, cd_idx, actual_idx);
      }
      else {
        // add it to the entry node as may scalar_ref_passed
        INT id = Summary->Get_symbol_index(WN_st(WN_kid0(actual)));
        INT pid = Cfg_entry->Add_scalar_ref_may_passed(id, callsite_id);
        INT actual_idx = get_actual_id(callsite_id, param_pos,
                                       Array_Summary.Get_actual_start_idx());
        Array_Summary.Set_actual_scalar_info_map(pid+1, Cfg_entry_idx, 
                                                 actual_idx);
      }
    }
  }
#endif // _THIS_SEEMS_USELESS_
}

// --------------------------------------------
// Fill in PROJECTED_NODE bounds conservatively
// --------------------------------------------
static void 
Min_Max_Fill_Proj_Node (PROJECTED_NODE* pn,
                        DYN_ARRAY<LOOPINFO*>* loops,
			INT bad_loop_count)
{
  BOOL substituted_lindex = FALSE;  

  pn->Fill_Out();
  
  LINEX* lx_lower = pn->Get_lower_linex();
  INT j;

  for (j = 0; j <= lx_lower->Num_terms(); j++) {
    TERM* tm = lx_lower->Get_term(j);
    if (tm->Get_type() == LTKIND_LINDEX) {
      INT loop_array_index = loops->Lastidx() - tm->Get_desc() 
	+ bad_loop_count; 

      if (loop_array_index < 0 || loop_array_index > loops->Lastidx()) {
	pn->Set_messy_lb();
	break; 
      } 

      LOOPINFO* l = (*loops)[loop_array_index];
      LINEX* lx_substitute = (tm->Get_coeff() < 0)
        ? l->Max_value() : l->Min_value();

      if (lx_substitute == NULL) {
        pn->Set_messy_lb();
        break; 
      } 

      lx_lower->Substitute_Lindex(tm->Get_desc(), lx_substitute);
      substituted_lindex = TRUE;
      j = -1; // Start over again
    }
  }

  LINEX* lx_upper = pn->Get_upper_linex();
  for (j = 0; j <= lx_upper->Num_terms(); j++) {
    TERM* tm = lx_upper->Get_term(j);
    if (tm->Get_type() == LTKIND_LINDEX) {
      INT loop_array_index = loops->Lastidx() - tm->Get_desc() 
	+ bad_loop_count; 
      if (loop_array_index < 0 || loop_array_index > loops->Lastidx()) {
	pn->Set_messy_ub();
	break; 
      } 
      LOOPINFO* l = (*loops)[loop_array_index];
      LINEX* lx_substitute = (tm->Get_coeff() < 0)
        ? l->Min_value() : l->Max_value();
      if (lx_substitute == NULL) {
        pn->Set_messy_ub();
        break; 
      } 
      lx_upper->Substitute_Lindex(tm->Get_desc(), lx_substitute);
      substituted_lindex = TRUE;
      j = -1; // Start over again
    }
  }  

  if (substituted_lindex) {
    LINEX* lx_step = pn->Get_step_linex();
    if (lx_step != NULL) { 
      lx_step->Free_terms();
      lx_step->Set_term(LTKIND_CONST, (INT32) 1, CONST_DESC, 0);
    } 
  } 
}


// -------------------------------------------------------------------
// Try to recognize two-strided section, e.g., those that are created
// when two- or three-dimensional arrays are linearized.
// Currently, the projected node must be of the form
//              [ A*I+B : A*I+C : D ]
// where A,B,C,D are constant (A != 0), I is the index variable of
// the outermost loop, and the bounds (L,U) of that loop are constant.
//
// When the condition 
//                      A*(U-L) < D 
// is satisified, we have the two-strided section
//      [ A*L+B : A*U+C : ((D % A == 0) ? A : 1) : A*(U-L)+1 : D ]
// -------------------------------------------------------------------
static BOOL
Proj_Node_Has_Two_Strides (PROJECTED_NODE* proj_node,
                           DYN_ARRAY<LOOPINFO*>* loops,
			   INT bad_loop_count)
{
  LOOPINFO* outer_loop = NULL;

  if (bad_loop_count != 0)
    return FALSE; 

  INT l_coeff = 0;
  INT l_offset = 0;
  INT i;
  LINEX* lx_lower = proj_node->Get_lower_linex();

  for (i = 0; i <= lx_lower->Num_terms(); ++i) {
    TERM* tm = lx_lower->Get_term(i);
    if (tm->Get_type() == LTKIND_LINDEX) {
      if (tm->Get_desc() != 0) { 
        // not the outermost loop
        return FALSE;
      }
      else {
        // accumulate A
        l_coeff += tm->Get_coeff();
      }
    }
    else if (tm->Get_type() == LTKIND_CONST) {
      // accumulate B
      l_offset += tm->Get_coeff();
    }
    else {
      // not a constant or loop-index variable
      return FALSE;
    }
  }

  if (l_coeff == 0) {
    // A must be non-zero
    return FALSE;
  }
  
  INT u_coeff = 0;
  INT u_offset = 0;
  LINEX* lx_upper = proj_node->Get_upper_linex();

  for (i = 0; i <= lx_upper->Num_terms(); ++i) {
    TERM* tm = lx_upper->Get_term(i);
    if (tm->Get_type() == LTKIND_LINDEX) {
      if (tm->Get_desc() != 0) { 
        // not the outermost loop
        return FALSE;
      }
      else {
        // accumulate A and remember outermost loop
        u_coeff += tm->Get_coeff();
        outer_loop = (*loops)[loops->Lastidx()];
      }
    }
    else if (tm->Get_type() == LTKIND_CONST) {
      // accumulate C
      u_offset += tm->Get_coeff();
    }
    else {
      // not a constant or loop-index variable
      return FALSE;
    }
  }

  if (l_coeff != u_coeff) {
    // coefficients of the loop-index variable must both be A
    return FALSE;
  }

  LINEX* lx_step = proj_node->Get_step_linex();
  if (lx_step == NULL || !lx_step->Is_const()) {
    // D must be constant
    return FALSE;
  }
  INT stride = lx_step->Get_constant_term();

  LINEX* outer_max = outer_loop->Max_value();
  if (outer_max == NULL || !outer_max->Is_const()) {
    // outermost loop bounds must be constant
    return FALSE;
  }
  INT outer_ub = outer_max->Get_constant_term();

  LINEX* outer_min = outer_loop->Min_value();
  if (!outer_min->Is_const()) {
    // outermost loop bounds must be constant
    return FALSE;
  }
  INT outer_lb = outer_min->Get_constant_term();

  // To be on the safe side, assert that:
  // A > 0, D > 0, U >= L
  if (l_coeff < 0 || stride < 0 || outer_ub < outer_lb) {
    return FALSE;
  }
  
  // Must take care of the signs of coefficients vs. Max-Min
  if (l_coeff * (outer_ub - outer_lb) < stride) {
    
    proj_node->Set_constant_two_strided_section(l_coeff*outer_lb + l_offset,
      l_coeff*outer_ub + u_offset, stride % l_coeff ? 1 : l_coeff,
      l_coeff*(outer_ub-outer_lb)+1, stride);
    return TRUE;
  }

  return FALSE;
}

// ----------------------------------------------
// Fill in PROJECTED_REGION bounds conservatively
// ----------------------------------------------
static void 
Min_Max_Fill_Region(PROJECTED_REGION* proj_region,
                    DYN_ARRAY<LOOPINFO*>* loops,
		    INT bad_loop_count)
{
  if (proj_region->Is_messy_region())
    return; 

  PROJECTED_ARRAY* pa = proj_region->Get_projected_array();
  if (pa != NULL) {
    BOOL is_projected_region = TRUE;

    for (INT i = 0; i <= pa->Lastidx(); i++) {
      PROJECTED_NODE* pn = &(*pa)[i];
      if (!Proj_Node_Has_Two_Strides(pn, loops, bad_loop_count)) {
        Min_Max_Fill_Proj_Node(pn, loops, bad_loop_count);
      }
      if (pn->Is_unprojected()) {
	is_projected_region = FALSE;
      }
    }

    if (is_projected_region) {
      proj_region->Reset_is_unprojected();
    }
    else {
      proj_region->Set_unprojected();
    }
  }
}


//---------------------------------------------------------------------
// process an array node
//---------------------------------------------------------------------
static void 
process_array_node(WN* wn, IPA_SECTION_TYPE type)
{
  WN* array_base = WN_array_base(wn);
  while (array_base && (WN_operator(array_base) == OPR_ARRAY)) {
    array_base = WN_array_base(array_base);
  }
  Is_True (array_base != NULL, ("process_array_node: NULL array base\n"));

  if (!OPERATOR_has_sym(WN_operator(array_base))) {
    return;
  }
  ST* array_st = WN_st(array_base);
  // Skip local arrays
  if (!array_st || ST_sclass(array_st) == SCLASS_AUTO) {
    return;
  }
  TY_IDX array_ty_idx = (ST_sclass(array_st) != SCLASS_FORMAL ?
                         ST_type(array_st) :
                         TY_pointed(ST_type(array_st)));
  // array within structs have ST of the struct plus offset
  // currently we can't deal with them (they'll be treated as scalars)
  if (TY_kind(array_ty_idx) != KIND_ARRAY) {
    return;
  }

  // currently, we cannot deal with formals from a parent PU (652403)
  if (ST_sclass(array_st) == SCLASS_FORMAL &&
      ST_level(array_st) != CURRENT_SYMTAB) {
    Current_summary_procedure->Set_has_incomplete_array_info();
    return;
  }

  ACCESS_ARRAY* av = (ACCESS_ARRAY*) WN_MAP_Get(IPL_info_map, wn);
  Is_True (av, ("process_array_node: NULL access vector"));

  MEM_POOL* apool = Array_Summary.Get_array_pool();
  DYN_ARRAY<LOOPINFO*> loops(apool);
  PROJECTED_REGION* proj_region = NULL;
  INT cd_idx = -1;

  // get the statement for this array expression
  WN* stmt = IPL_get_stmt_scf(wn);

  // get the control structure surrounding the statement
  SUMMARY_CONTROL_DEPENDENCE* d = Get_controlling_stmt(stmt);

  // Try to project out the dimensions of the array using loopinfo's
  while (d) {
    cd_idx = Get_cd_idx(d);
    if (d->Is_do_loop()) {
      LOOPINFO* l = Array_Summary.Get_cfg_node_array(cd_idx)->Get_loopinfo();
      loops.AddElement(l);
      if (!proj_region) {
        proj_region = CXX_NEW(PROJECTED_REGION(av, apool, l), apool);
        if (TY_AR_ndims(array_ty_idx) != av->Num_Vec()) {
          proj_region->Set_messy_region();
        }
      }
      proj_region->Project(l->Get_nest_level(), l);
    }
    d = Get_controlling_stmt(d->Get_wn());
  }

  // Use max and min values to fill in unprojected loop elements. 
  INT loop_count = 0; 
  for (WN* wnn = wn; wnn != NULL; wnn = LWN_Get_Parent(wnn))
    if (WN_operator(wnn) == OPR_DO_LOOP)
      loop_count++; 
  INT bad_loop_count = loop_count - (loops.Lastidx() + 1);
  if (proj_region) { 
    Min_Max_Fill_Region(proj_region, &loops, bad_loop_count);
    if (proj_region->Get_projected_kernel() &&
        proj_region->Get_projected_kernel()->Get_region()) {
      Min_Max_Fill_Region(proj_region->Get_projected_kernel()->Get_region(), 
                          &loops, bad_loop_count);
    }
  } 
  else {
    // when we reach here, check if the projected region has been
    // created. If it has then the array was inside a loop. Otherwise,
    // create a projected region and add it to the control structure
    proj_region = CXX_NEW(PROJECTED_REGION(av, apool, NULL), apool);
    if (TY_AR_ndims(array_ty_idx) != av->Num_Vec()) {
      proj_region->Set_messy_region();
    }
    if (!proj_region->Is_messy_region()) {
      proj_region->Fill_Out();
    }

    if (d) {
      d = Get_controlling_stmt(stmt);
      cd_idx = Get_cd_idx(d);
      if (d->Is_if_stmt()) {
        BOOL branch;
        INT stmt_idx;
        SUMMARY_STMT* summary_stmt = 
          Search_for_summary_stmt(stmt, branch, stmt_idx);
        Is_True (summary_stmt, ("process_array_node: NULL summary stmt"));
        if (branch == FALSE) {
          cd_idx = Array_Summary.Get_cfg_node_array(cd_idx)->Get_else_index();
        }
      }
    }
  }

  INT element_size = TY_size(TY_etype(array_ty_idx));
  INT id = Summary->Get_symbol_index(array_st);

  if (cd_idx != -1) {
    CFG_NODE_INFO* cfg_node = Array_Summary.Get_cfg_node_array(cd_idx);
    switch (type) {
    case IPA_DEF:
      cfg_node->Add_def_array(proj_region, element_size, id);
      break;
    case IPA_USE:
      cfg_node->Add_use_array(proj_region, element_size, id);
      break;
    case IPA_REDUC:
      cfg_node->Add_array_reduc(id);
      break;
    }
  }
  else {
    switch (type) {
    case IPA_DEF:
      Cfg_entry->Add_may_def_array(proj_region, element_size, id);
      break;
    case IPA_USE:
      Cfg_entry->Add_may_use_array(proj_region, element_size, id);
      break;
    case IPA_REDUC:
      Cfg_entry->Add_array_may_reduc(id);
      break;
    }
  }
}

//--------------------------------------------------------------------------
// process the stmt_scf node
//--------------------------------------------------------------------------
static void 
process_node(WN* wn, IPA_SECTION_TYPE type)
{
  WN* lhs, *rhs, *parent_wn;
  INT32 map;

  BOOL save_trace_sections = Trace_Sections;
  if (Get_Trace(TP_IPL, TT_IPL_VERBOSE))
    Trace_Sections = TRUE; 

  switch(WN_operator(wn))
    {
    case OPR_ISTORE:
    case OPR_MSTORE:
      lhs = WN_kid0(wn);
      rhs = WN_kid1(wn);
      process_node(lhs, IPA_USE);
      map = WN_MAP32_Get(IPL_reduc_map, wn);
      if (map > 0 && map < 5)
	process_node(rhs, IPA_REDUC);
      else
	process_node(rhs, IPA_DEF);
      break;
      
      // get the assignment statements
    case OPR_STID: {
	// check if it involves a reduction, if it does then set both
	// the rhs and the lhs as reduction
	rhs = WN_kid0(wn);
	ST* lhs_st = WN_st(wn);
	process_node(rhs, IPA_USE);
	map = WN_MAP32_Get(IPL_reduc_map, wn);
	if (map > 0 && map < 5)
	  process_scalar_reduc_node(wn, lhs_st);
	else
	  process_scalar_def_node(wn, lhs_st);
      } 
     break;

    case OPR_CALL:
    case OPR_ICALL: 
    case OPR_INTRINSIC_CALL: {
      // ignore fake calls
      if (WN_opcode(wn) == OPC_VCALL && WN_Fake_Call_EH_Region(wn, Parent_Map))
        break;
      // walk all the kids and update the actual parameter node
      // index into the callsite array is stored as a map
      INT call_index = IPL_get_callsite_id(wn);
      FmtAssert(call_index != -1, ("Unknown call encountered \n"));
      SUMMARY_CALLSITE* c = Summary->Get_callsite(0) + call_index;
      for (INT i = 0; i < c->Get_param_count(); ++i) {
        process_actual_node(wn, WN_actual(wn ,i), call_index, i);
      }
      break;
    }

    case OPR_ARRAY: {
	parent_wn = LWN_Get_Parent(wn);
	if (WN_operator(parent_wn) == OPR_ILOAD ||
            WN_operator(parent_wn) == OPR_MLOAD) {
	    // check if it is a reduction
	    map = WN_MAP32_Get(IPL_reduc_map, parent_wn);
	    if (map != RED_NONE)
	      process_array_node(wn, IPA_REDUC);
	    process_array_node(wn, IPA_USE);
	} else if (WN_operator(parent_wn) == OPR_ISTORE) {
	  process_array_node(wn, IPA_DEF);
	} else {
	  process_array_node(wn, type);
	}
	// Need to look for arrays nested inside arrays 
	for (INT i = 0; i < WN_num_dim(wn); i++) 
	  process_node(WN_array_index(wn, i), IPA_USE);
      } 
      break;

    case OPR_LDID:
      parent_wn = LWN_Get_Parent(wn);
      if ((WN_operator(parent_wn) == OPR_ARRAY) ||
  	 (WN_operator(parent_wn) == OPR_PARM))
	;
      else
	{
	  // check if is a reduction type
	  map = WN_MAP32_Get(IPL_reduc_map, wn);
	  if (map > 0 && map < 5)
	    process_scalar_node(wn, IPA_REDUC);
	  else if (type != IPA_DEF)
	    process_scalar_node(wn, IPA_USE);
	  else
	    process_scalar_node(wn, type);
	}
      break;

      // in the case of an iload, check if the parent node is an array
      // if so then don't bother to do anything with it
    case OPR_MLOAD: 
    case OPR_ILOAD: {
	parent_wn = LWN_Get_Parent(wn);
	if ((WN_operator(parent_wn) == OPR_ARRAY) ||
	   (WN_operator(parent_wn) == OPR_PARM))
	  ;
	else
	  for (INT i=0; i<WN_kid_count(wn); ++i)
	    process_node(WN_kid(wn, i), type);
      } 
      break;

    case OPR_IO: {
	 for (INT i=0; i<WN_kid_count(wn); ++i)
	   process_node(WN_kid(wn, i), type);
      }
      break;

    default: {
	if ((OPCODE_is_expression(WN_opcode(wn))))
	  {
	    for (INT i=0; i<WN_kid_count(wn); ++i)
	      process_node(WN_kid(wn, i), type);
	  }
      } 
      break;
    }
  Trace_Sections = save_trace_sections; 
}

//-----------------------------------------------------------------------
// NAME: Process_Array_Formals
// FUNCTION: For the subprogram 'wn_func', whose formals have indices in
//   the SUMMARY_FORMAL array from 'idx_formal_first' to 'idx_formal_first'
//   + 'formal_count' - 1, add entries in the REGION array corresponding
//   to those formals which are declared as arrays.  Allocate memory from
//   'mem_pool'.
// NOTE: These summaries are needed for reshaping analysis in IPA.
//-----------------------------------------------------------------------

static void 
Process_Array_Formals(WN* wn_func,
                      INT idx_formal_first,
                      INT formal_count,
                      MEM_POOL* mem_pool)
{
  for (INT i = 0; i < formal_count; i++) {
    WN* wn_formal = WN_kid(wn_func, i);
    ST* st_formal = WN_st(wn_formal);
    FmtAssert(st_formal != NULL, 
      ("Process_Array_Formals: Expecting non-NULL st_formal"));
    TY_IDX ty_idx_formal = ST_type(st_formal);
    if (TY_kind(ty_idx_formal) != KIND_POINTER)
      continue;
    TY_IDX ty_idx_pformal = TY_pointed(ty_idx_formal);
    if (TY_kind(ty_idx_pformal) != KIND_ARRAY)
      continue;
    INT element_size = TY_size(TY_etype(ty_idx_pformal));
    PROJECTED_REGION* pr = Projected_Region_From_St(wn_func, st_formal, 
      mem_pool, FALSE, NULL);
    INT idx_st_formal = Summary->Get_symbol_index(st_formal);
    Cfg_entry->Add_formal_array(pr, element_size, idx_st_formal, 
      idx_formal_first + i);
  }
}

//----------------------------------------------------------------
// walk the control structure, get all the do loops, build the
// loop information
//----------------------------------------------------------------
static void 
process_loops()
{
  MEM_POOL* apool = Array_Summary.Get_array_pool();
  SUMMARY_CONTROL_DEPENDENCE* cd;
  
  while (cd = Get_next_cd ()) {
    if (cd->Is_do_loop()) {

      // get LNO-like loop information
      DO_LOOP_INFO_BASE* dli = 
        (DO_LOOP_INFO_BASE*) WN_MAP_Get(IPL_info_map, cd->Get_wn());

      // map it into summary LOOPINFO structure
      INT cd_idx = Get_cd_idx(cd);
      LOOPINFO* l = CXX_NEW(LOOPINFO(apool, cd_idx), apool);
      l->Map_do_loop_info(dli);
      
      // store it as part of the cfg node information
      Array_Summary.Get_cfg_node_array(cd_idx)->Set_loopinfo(l);
    }
  }
}

//--------------------------------------------------------------------------
// record the tlog information
//--------------------------------------------------------------------------
static void
Record_tlog(TLOG_INFO *tlog)
{
  Ipl_record_tlog("LTKIND_CONST", 0, "Count %d", tlog->Get_cterm_count());
  Ipl_record_tlog("LTKIND_LINDEX", 0, "Count %d", tlog->Get_lterm_count());
  Ipl_record_tlog("LTKIND_IV_GLOBAL", 0,"Count %d",tlog->Get_iv_gterm_count());
  Ipl_record_tlog("LTKIND_IV", 0, "Count %d",tlog->Get_iv_term_count());
  Ipl_record_tlog("LTKIND_SUBSCR", 0, "Count %d",tlog->Get_sub_term_count());
}

//-----------------------------------------------------------------------
// NAME: Has_Threadprivate_Variable
// FUNCTION: Return TRUE if the tree rooted at 'wn_tree' has a THREAD_PRIVATE
//   variable.  Return FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Has_Threadprivate_Variable(WN* wn_tree)
{
  if (WN_operator(wn_tree) == OPR_BLOCK) { 
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      if (Has_Threadprivate_Variable(wn))
	return TRUE; 
  } else { 
    if (OPERATOR_has_sym(WN_operator(wn_tree)) && WN_st(wn_tree) != NULL
        && (ST_base(WN_st(wn_tree)) != WN_st(wn_tree)
        && ST_sclass(ST_base(WN_st(wn_tree))) == SCLASS_COMMON
        && ST_is_thread_private(ST_base(WN_st(wn_tree)))
        || ST_is_thread_private(WN_st(wn_tree)))) {
      return TRUE; 
    } 
    for (INT i = 0; i < WN_kid_count(wn_tree); i++) 
      if (Has_Threadprivate_Variable(WN_kid(wn_tree, i)))
	return TRUE; 
  } 
  return FALSE; 
} 

//--------------------------------------------------------------------------
// map access vectors to linex and loopinfo structures
//--------------------------------------------------------------------------
void 
IPL_Access_Vector_To_Projected_Region(WN* w, 
                                      SUMMARY_PROCEDURE* proc,
				      INT pu_first_formal_idx, 
				      INT pu_last_formal_idx,
                                      INT pu_first_actual_idx,
                                      INT pu_last_actual_idx,
				      INT pu_first_callsite_idx,
				      INT pu_last_callsite_idx)
{
  WN_ITER *wni;
  WN* wn;
  INT i;
  CFG_NODE_INFO *cfg_node_else = NULL;

  FmtAssert((w != NULL),("NULL node in IPL_Access_Vector_To_Proj_Region\n"));
  INT max_cd_size = Get_max_cd_idx();

  INT pu_formal_count = pu_last_formal_idx - pu_first_formal_idx + 1;
  INT pu_actual_count = pu_last_actual_idx - pu_first_actual_idx + 1;
  INT pu_callsite_count = pu_last_callsite_idx - pu_first_callsite_idx + 1;

  Array_Summary.Init(pu_formal_count, pu_first_formal_idx, 
    pu_actual_count, pu_first_actual_idx, 
    pu_callsite_count, pu_first_callsite_idx, 
    max_cd_size + 1);

  WB_IPL_Set_Array_Summary(&Array_Summary);

  // create an array of cfg_info nodes of the size of the control
  // dependence array
  CFG_NODE_INFO_ARRAY* cfg_nodes = Array_Summary.Get_cfg_node_array();

  // note, if there is an entry node, then the size will always
  // atleast be 1

  INT if_count = 0;

  for (i = 0; i <= max_cd_size; ++i) {
    SUMMARY_CONTROL_DEPENDENCE* cd = Get_cd_by_idx(i);
    if (cd->Is_if_stmt()) {
      ++if_count;
    }
  }

  if (max_cd_size == -1) {
    proc->Set_has_incomplete_array_info();
    return;
  } 

  // for each if we create and else node also
  cfg_nodes->Force_Alloc_array(max_cd_size+1+if_count);
  cfg_nodes->Setidx(max_cd_size+if_count);


  INT* map = Array_Summary.Get_cd_map();

  INT if_idx = 0;
  for (i=0; i<=max_cd_size; ++i)
    {
      SUMMARY_CONTROL_DEPENDENCE *d = Get_cd_by_idx(i);
      CFG_NODE_INFO *cfg_node = &(*cfg_nodes)[i];
      cfg_node->Init(Array_Summary.Get_array_pool());

      // mapping from the output cds to the input cds where 
      // real_idx is the output index and i is the input index
      INT real_idx = Get_cd_real_idx(d);

      // store real cd index in the cfg_node
      cfg_node->Set_cd_index(real_idx);

      // map is indexed relative to the beginning of PU
      map[real_idx - proc->Get_ctrl_dep_index()] = i;

      if (d->Is_if_stmt())
	{
	  // printf("cd[%d] = %s \n", i, "if node ");
	  cfg_node->Set_type_if();
	  ++if_idx;
	  cfg_node->Set_else_index(max_cd_size+if_idx);

	  // initialize the else control dependence
	  cfg_node_else = &(*cfg_nodes)[max_cd_size+if_idx];
	  cfg_node_else->Init(Array_Summary.Get_array_pool());
	  cfg_node_else->Set_type_else();
	  cfg_node_else->Set_if_index(i);
	}

      else if (d->Is_do_loop())
	{
	  cfg_node->Set_type_do_loop();
	}
      else
	{
	  cfg_node->Set_type_entry();
	  Cfg_entry = cfg_node;
	  Cfg_entry_idx = i;
	}


      if (cfg_node->Is_if())
	{
	  if (Get_cd_call_count(i, TRUE) > 0)
	    cfg_node->Set_has_calls();
	  if (Get_cd_call_count(i, FALSE) > 0)
	    cfg_node_else->Set_has_calls();
	}
      else if (Get_cd_call_count(i) > 0)
	cfg_node->Set_has_calls();
    }

  if (Cfg_entry == NULL) {
    proc->Set_has_incomplete_array_info();
    return;
  } 

  Current_summary_procedure = proc;
  
  // Initialize loop info and access array maps
  MEM_POOL* apool = Array_Summary.Get_array_pool();
  IPL_Loopinfo_Map = CXX_NEW(LOOPINFO_TO_DLI_MAP(64,apool),apool);
  IPL_Access_Array_Map = 
    CXX_NEW(PROJ_REGION_TO_ACCESS_ARRAY_MAP(128,apool),apool);
  
  // process loop structures, map the loop bounds to linexs
  process_loops();

  // check for threadprivate variables 
  if (Has_Threadprivate_Variable(w)) { 
    proc->Set_has_incomplete_array_info();
    return; 
  } 

  // walk the tree
  for (wni = WN_WALK_StmtIter(w); wni && WN_ITER_wn(wni) != 0;
       wni = WN_WALK_StmtNext(wni) ) {
    wn = WN_ITER_wn(wni);
    process_node(wn, IPA_UNKNOWN);
  }

  // Save dimensions of array formals for reshaping analysis
  Process_Array_Formals(w, pu_first_formal_idx, pu_formal_count,
    Array_Summary.Get_array_pool());

  Cfg_entry = NULL;
  Cfg_entry_idx = -1;
}

//------------------------------------------------------------------------
// finalize projected regions
//------------------------------------------------------------------------
void
IPL_Finalize_Projected_Regions(SUMMARY_PROCEDURE *p)
{
  INT term_offset = 0;

  if (Get_Trace(TP_PTRACE1, TP_PTRACE1_IPA)) {
    term_offset = Array_Summary_Output->Get_term_offset();
  }

  Map_asections(&Array_Summary, p);

  if (Get_Trace(TP_PTRACE1, TP_PTRACE1_IPA)) {
    Array_Summary.Record_tlogs(Array_Summary_Output->Get_term_array(),
                               term_offset+1);
    Record_tlog(Array_Summary.Get_tlog_info());
  }
  
  Array_Summary.Finalize();
}

