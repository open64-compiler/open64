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


// -*- C++ -*-
//                    ARA Liveness Analysis
//                    ---------------------

/* ======================================================================
 * ======================================================================
 *
 * Module: ara_live.cxx
 *
 * Description: Performing liveness analysis to determine if private arrays
 * need last value.
 *
 * Because we don't have CFG and,  determining if an array section is killed
 * is very expensive.  We will do a simple pass though the local variables
 * that are identified as private in all the loops and mark them as not
 * live if there is no exposed use of the array anywhere in the whole
 * program.
 * ======================================================================
 * ======================================================================
 */

#include <stdint.h>
#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include <sys/types.h>
#include <limits.h>
#include "pu_info.h"
#include "ara_loop.h"
#include "stab.h"
#include "stblock.h"
#include "lwn_util.h"
#include "opt_alias_interface.h"
#include "opt_du.h"
#include "config.h"
#include "snl_xbounds.h"
#include "debug.h"
#include "parmodel.h"
#include "parids.h"
#include "ara_utils.h" 

static void Add_Symbol_To_Use(WN* wn_sym,
			      ARA_LOOP_INFO* ali)
{
  ST* st_sym = WN_st(wn_sym);
  if (st_sym == NULL)
    return; 
  TY_IDX ty_idx_sym = ST_type(st_sym);
  if (TY_kind(ty_idx_sym) == KIND_POINTER)
    ty_idx_sym = TY_pointed(ty_idx_sym);
  if (TY_kind(ty_idx_sym) == KIND_ARRAY) {
    SYMBOL sym_array(wn_sym);
    REGION* rg_array = CXX_NEW(REGION(0, 0), &ARA_memory_pool);
    rg_array->_type = ARA_TOP;
    ARA_REF* ref_new = CXX_NEW(ARA_REF(&sym_array, rg_array, ali, TRUE),
      &ARA_memory_pool);
    ali->Add_Use(ref_new);
  } else if (WN_operator(wn_sym) == OPR_LDID || WN_operator(wn_sym) == 
      OPR_STID) { 
    ali->SCALAR_USE().Add_Scalar(wn_sym, 0);
  } else if (WN_operator(wn_sym) == OPR_LDA) { 
    SYMBOL sym(WN_st(wn_sym), 0, TY_mtype(ST_type(WN_st(wn_sym))));
    ali->SCALAR_USE().Add_Scalar(wn_sym, &sym, 0);
  }
} 

static void Add_Symbols_To_Uses_Traverse(WN* wn_tree,
					 ARA_LOOP_INFO* ali)
{
  if (OPCODE_has_sym(WN_opcode(wn_tree)))
    Add_Symbol_To_Use(wn_tree, ali);
  for (INT i = 0; i < WN_kid_count(wn_tree); i++)
    Add_Symbols_To_Uses_Traverse(WN_kid(wn_tree, i), ali);
}

static void Add_Symbols_To_Uses(WN* wn_node,
				ARA_LOOP_INFO* ali)
{
  Add_Symbols_To_Uses_Traverse(wn_node, ali);
}

//=============================================================================
//
// For bad loops, consider all the uses are exposed and nothing is 
// privatizable.
//
//=============================================================================
void
ARA_LOOP_INFO::Default_For_Bad_Loop()
{
  
  // Walk the children first
  for (INT i = 0; i < _children.Elements(); ++i)
    _children.Bottom_nth(i)->Walk_Loop();
  
  // Walk all the statements in the loop
  LWN_ITER * stmt_iter;
  if (WN_operator(_loop) == OPR_FUNC_ENTRY) 
    stmt_iter = LWN_WALK_StmtIter(WN_func_body(_loop));
  else
    stmt_iter = LWN_WALK_StmtIter(_loop);

  // Skip the OPC_DO_LOOP itself
  if (WN_operator(_loop) == OPR_DO_LOOP)
    stmt_iter = LWN_WALK_StmtNext(stmt_iter);

  // The LDID for the array name in OPR_ISTORE has to be skipped
  WN *skip_store_id = NULL;

  while (stmt_iter) {

    WN *stmt = stmt_iter->wn;
    stmt_iter = LWN_WALK_StmtNext(stmt_iter);
    OPERATOR opr = WN_operator(stmt);
    
    if (opr == OPR_DO_LOOP) {
      
      DO_LOOP_INFO* dli = Get_Do_Loop_Info(stmt);
      
      Is_True(dli, ("ARA_LOOP_INFO::Walk_Loop: No DO_LOOP_INFO for this loop"));
      ARA_LOOP_INFO* ali = dli->ARA_Info;
      Is_True(ali, ("ARA_LOOP_INFO::Walk_Loop: No ARA_LOOP_INFO for this loop"));

      for (INT i = 0; i < ali->_use.Elements(); ++i) {
	ARA_REF *cur_clone = CXX_NEW(ARA_REF(*(ali->_use.Bottom_nth(i))), &ARA_memory_pool);
	Add_Use(cur_clone);
      }

      // Handle scalar sets
      Merge_Scalar_List(&ali->_scalar_use, &_scalar_use);

      // skip the rest and continue onto next statement
      // this is not very efficient
      do 
	stmt_iter = LWN_WALK_StmtNext(stmt_iter);
      while (stmt_iter && Wn_Is_Inside(stmt_iter->wn,stmt));
	  
      continue;

    } else if (opr == OPR_IO) {

       Add_Symbols_To_Uses(stmt, this);

    } else if ((opr == OPR_ISTORE) && 
	       (WN_operator(WN_kid1(stmt)) == OPR_ARRAY)) {
      
      // Process the LHS for write
      WN* lfs = WN_kid1(stmt);

      skip_store_id = WN_kid0(lfs);

    } else if (opr == OPR_STID) { 

      _scalar_def.Add_Scalar(stmt, 0);
      _scalar_may_def.Add_Scalar(stmt, 0);
      for (WN* wn = stmt; wn != NULL; wn = LWN_Get_Parent(wn)) { 
	if (WN_operator(wn) == OPR_DO_LOOP 
	    && SYMBOL(WN_index(wn)) == SYMBOL(stmt)) { 
	  _scalar_def.Add_Scalar(stmt, 0);
	  _scalar_may_def.Add_Scalar(stmt, 0);
	} 
      } 

    } else if (OPCODE_is_scf(WN_opcode(stmt)))
      continue;

    // Process the reads (RHS)
    LWN_ITER* rhs = 
      LWN_WALK_TreeIter(stmt);

    while (rhs) {

      WN* wn = rhs->wn;
      rhs = LWN_WALK_TreeNext(rhs);
      if (wn == skip_store_id) {
	wn = rhs->wn;
	rhs = LWN_WALK_TreeNext(rhs);
	skip_store_id = NULL;
      }
	
      if ((WN_operator(wn) == OPR_ILOAD) &&
	  (WN_operator(WN_kid0(wn)) == OPR_ARRAY)) {
	ARA_REF *new_use = CXX_NEW(ARA_REF(WN_kid0(wn),WN_offset(wn),this),
				   &ARA_memory_pool);
	if (new_use->Has_Bad_Alias()) 
	  CXX_DELETE(new_use, &ARA_memory_pool);
	else
	  Add_Use(new_use);
	
	// Skip the LDID for OPR_ARRAY
	rhs = LWN_WALK_TreeNext(rhs);
	rhs = LWN_WALK_TreeNext(rhs);

      } else if (WN_operator(wn) == OPR_LDID) {

	if (Is_Covered(wn)) 
	  _scalar_pri.Add_Scalar(wn,0);
	else 
	  _scalar_use.Add_Scalar(wn,0);

      }

    } // RHS

  } // while (stmt_iter)
  
}

//=============================================================================
//
// Map the exposed use arrays to the hash table
//
//=============================================================================
void
ARA_LOOP_INFO::Create_Live_Use()
{

  Is_True(_live_use==NULL,("ARA_LOOP_INFO::Create_Live_Use(): It already exists"));

  if (_live_use) CXX_DELETE(_live_use,&ARA_memory_pool);

  // Walk the children
  INT i;
  for (i = 0; i < _children.Elements(); ++ i) {
    _children.Bottom_nth(i)->Create_Live_Use();
  }


  _live_use = CXX_NEW(S_HTABLE(_use.Elements()+1,&ARA_memory_pool),&ARA_memory_pool);
  for (i = 0; i < _use.Elements(); ++ i) {
    ST*  st = _use.Bottom_nth(i)->Array().St();
    _live_use->Enter(st,TRUE);
  }

}

//=============================================================================
//
// Walk the ARA_LOOP_INFOs surrounding current loop.  If a local private
// array has exposed uses in their Live_Use, conservatively requiring last
// value assignment for the array.
//
//=============================================================================
void
ARA_LOOP_INFO::Determine_Last_Value()
{

  INT i;
  for (i = 0; i < _children.Elements(); ++i) {
    _children.Bottom_nth(i)->Determine_Last_Value();
  }

  for (i = 0; i < _pri.Elements(); ++ i) {
    ARA_REF *ara = _pri.Bottom_nth(i);
    Is_True(ara->Need_Last_Value(),
            ("unexpected FALSE ara->Need_Last_Value()"));

    if (!ara->Has_Bad_Alias() &&
	ara->Need_Last_Value() &&
	ara->Is_Loop_Invariant()) {
      ST* st = ara->Array().St();
      if (ST_class(st) == CLASS_VAR && 
	  ST_sclass(st) == SCLASS_AUTO &&
	  ST_base_idx(st) == ST_st_idx(st) &&
	  ST_addr_not_saved(st) && 
	  ST_addr_not_passed(st) &&
	  TY_size(ST_type(st)) > 0 &&
	  !ST_is_initialized(st) &&
	  !ST_has_nested_ref(st) &&
	  !ST_is_reshaped(st) &&
	  TY_kind(ST_type(st)) == KIND_ARRAY) {
	for (ARA_LOOP_INFO *cur_p = _parent; cur_p; cur_p = cur_p->_parent) {
	  if (cur_p->_live_use->Find(st)) {
	    _has_last_value_array = TRUE;
	    goto next_pri;
	  }
	}
	
          // this is the only place where we decide we don't need the last
          // value of a privatized variable--see above assertion
	ara->Set_No_Last_Value();

next_pri:
	continue;

      } else
	_has_last_value_array = TRUE;
    }
  }

  for (i = 0; i < _def.Elements(); ++ i) {
    if (_def.Bottom_nth(i)->Is_Loop_Invariant() &&
	!_def.Bottom_nth(i)->Is_Whole_Array() &&
	!Overlap_Local_Array(_def.Bottom_nth(i)->Array()) &&
	!Overlap_Exposed_Array(_def.Bottom_nth(i)->Array())) {
      ST* st = _def.Bottom_nth(i)->Array().St();
#ifdef _NEW_SYMTAB
      if (ST_class(st) == CLASS_VAR && 
	  (ST_sclass(st) == SCLASS_AUTO) &&
	  ST_addr_not_saved(st) && 
	  ST_addr_not_passed(st) &&
	  (TY_size(ST_type(st)) > 0) &&
	  (!ST_is_initialized(st)) &&
	  !ST_has_nested_ref(st) &&
	  (!ST_is_reshaped(st)) &&
#else
      if (ST_symclass(st) == CLASS_VAR && 
	  (ST_sclass(st) == SCLASS_AUTO) &&
	  !ST_addr_taken_saved(st) && !ST_addr_taken_passed(st) &&
	  (ST_size(st)>0) &&
	  (!ST_is_initialized(st)) &&
	  !ST_has_nested_ref(st) &&
	  (!ST_is_reshaped(st)) &&
#endif
	  (TY_kind(ST_type(st)) == KIND_ARRAY)) {
	ARA_LOOP_INFO *cur_p = _parent;
	while (cur_p) {

	  if (cur_p->_live_use->Find(st)) {
	    _has_last_value_array = TRUE;
	    goto next_def;
	  }

	  cur_p = cur_p->_parent;
	}
	
next_def:
	continue;

      } else
	_has_last_value_array = TRUE;
    }
  }
  
  for (i = 0; i < _scalar_pri.Elements(); ++ i) {
    BOOL need_last_value = TRUE;
    SCALAR_NODE *s_pri = _scalar_pri.Bottom_nth(i);
    for (INT j = 0; j < _scalar_may_def.Elements(); ++j) 
      if (s_pri->_scalar == 
	  _scalar_may_def.Bottom_nth(j)->_scalar) {

	need_last_value = FALSE;
	SCALAR_NODE *sn = _scalar_may_def.Bottom_nth(j);

	for (INT k = 0; k < sn->Elements(); ++k) {
	  WN* def = sn->Bottom_nth(k)->Wn;
	  USE_LIST *use_list = Du_Mgr->Du_Get_Use(def);

	  if (use_list && use_list->Incomplete()) {
	    need_last_value = TRUE;
	    goto break_point;
	  }

	  USE_LIST_ITER iter_use(use_list);

	  for (DU_NODE* use_node = iter_use.First(); !iter_use.Is_Empty();
	       use_node = (DU_NODE *) iter_use.Next()) {
	    WN *use = use_node->Wn();
	    if (!Wn_Is_Inside(use,_loop)) {
	      need_last_value = TRUE;
	      goto break_point;
	    }
	  }

	}
      }
    
  break_point:
    _scalar_last_value.Push(need_last_value);

    if (need_last_value) {
      INT i;
      for (i = 0; i < _scalar_def.Elements(); ++i) {
	if (s_pri->_scalar == _scalar_def.Bottom_nth(i)->_scalar)
	  break;
      }
      if (i == _scalar_def.Elements()) { // is not always defined
	if (LNO_Prompl) { 
          INT j;
	  for (j = 0; j < _scalar_no_final.Elements(); j++) 
	    if (_scalar_no_final.Bottom_nth(j) == s_pri->_scalar)
	      break;
	  if (j ==  _scalar_no_final.Elements())
	    _scalar_no_final.Push(s_pri->_scalar); 
 	} 
	Set_To_Sequential();
      } 
    }
  }

  for (INT j = 0; j < _scalar_may_def.Elements(); ++j)  {
    SCALAR_NODE *sn = _scalar_may_def.Bottom_nth(j);
    if (!Overlap_Kill_Scalar(sn->_scalar)) { // may def but not must def
      BOOL need_last_value = FALSE;
      for (INT k = 0; k < sn->Elements(); ++k) {
	WN* def = sn->Bottom_nth(k)->Wn;

	if (red_manager && red_manager->Which_Reduction(def) != RED_NONE) {
	  continue;  // PV 495175
	}

	USE_LIST *use_list = Du_Mgr->Du_Get_Use(def);

	if (use_list && use_list->Incomplete()) {
	  need_last_value = TRUE;
	  goto break_point1;
	}

	USE_LIST_ITER iter_use(use_list);

	for (DU_NODE* use_node = iter_use.First(); !iter_use.Is_Empty();
	     use_node = (DU_NODE *) iter_use.Next()) {
	  WN *use = use_node->Wn();
	  if (!Wn_Is_Inside(use,_loop)) {
	    need_last_value = TRUE;
	    goto break_point1;
	  }
	}
      }
    
    break_point1:
      
      if (need_last_value) {
	if (LNO_Prompl) { 
          INT j;
	  for (j = 0; j < _scalar_no_final.Elements(); j++) 
	    if (_scalar_no_final.Bottom_nth(j) == sn->_scalar)
	      break;
	  if (j ==  _scalar_no_final.Elements())
	    _scalar_no_final.Push(sn->_scalar); 
	}
	Set_To_Sequential();
      }
    }
  }

}

//-----------------------------------------------------------------------
// NAME: Scalar_Defs 
// FUNCTION: Returns the stack of all STIDs with the symbol 'sym' in the 
//   loop 'wn_loop'.  
//-----------------------------------------------------------------------

static STACK<WN*>* Scalar_Defs(SYMBOL* sym,
                               WN* wn_loop)
{
  STACK<WN*>* st_def = CXX_NEW(STACK<WN*>(&ARA_memory_pool), &ARA_memory_pool);
  LWN_ITER* itr = LWN_WALK_TreeIter(wn_loop);
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn;
    if (WN_operator(wn) == OPR_STID
        && SYMBOL(wn) == *sym)
      st_def->Push(wn);
  }
  return st_def;
}

//-----------------------------------------------------------------------
// NAME: Array_Defs 
// FUNCTION: Returns the stack of all ISTOREs with the symbol 'sym' in the 
//   loop 'wn_loop'.  
//-----------------------------------------------------------------------

static STACK<WN*>* Array_Defs(SYMBOL* sym, 
                              WN* wn_loop)
{
  STACK<WN*>* st_def = CXX_NEW(STACK<WN*>(&ARA_memory_pool), &ARA_memory_pool);
  LWN_ITER* itr = LWN_WALK_TreeIter(wn_loop);
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn;
    if (WN_operator(wn) == OPR_ISTORE
        && WN_operator(WN_kid1(wn)) == OPR_ARRAY
        && SYMBOL(WN_kid0(WN_kid1(wn))) == *sym)
      st_def->Push(wn);
  }
  return st_def;
}

//-----------------------------------------------------------------------
// NAME: Scalar_Def_Covered_In_Loop 
// FUNCTION: Returns TRUE if the scalar definition 'wn_def' is covered 
//   by another scalar definition in the loop 'wn_loop'.  
//-----------------------------------------------------------------------

static BOOL Scalar_Def_Covered_In_Loop(WN* wn_def,
                                       WN* wn_loop) 
{ 
  WN* wn_first = LWN_Get_Parent(wn_def); 
  for (WN* wnn = wn_first; wnn != wn_loop; wnn = LWN_Get_Parent(wnn)) { 
    for (WN* wn = wnn; wn != NULL; wn = WN_prev(wn))    
      if (WN_operator(wn) == OPR_STID
          && SYMBOL(wn) == SYMBOL(wn_def))
       return TRUE; 
  } 
  return FALSE; 
} 

//-----------------------------------------------------------------------
// NAME: Always_Executes 
// FUNCTION: Returns TRUE if adding the 'dimlb'th lower bound and the 
//   'dimub'th upper bound to the existing bounds information 'ebounds' 
//   indicates that statements inside the loop with these bounds will 
//   always execute when 'wn_loop' executes.  Returns FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Always_Executes(SNL_BOUNDS_INFO* ebounds, 
			    WN* wn_loop, 
			    INT dimlb, 
			    INT dimub) 
{
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 
  INT loop_depth = Do_Depth(wn_loop); 
  ACCESS_VECTOR* avlb = dli->LB->Dim(dimlb);
  ACCESS_VECTOR* avub = dli->UB->Dim(dimub);
  if (avlb->Too_Messy || avub->Too_Messy)
    return FALSE; 
  ACCESS_VECTOR* nox = Difference_Inequality(avlb, avub, loop_depth,
    DIFFERENCE_EXEC_NEVER, &ARA_memory_pool);
  ebounds->Add_Access(nox, FALSE);
  BOOL is_consistent = ebounds->Bounds().Is_Consistent();
  return !is_consistent;
}

//-----------------------------------------------------------------------
// NAME: Peelable
// FUNCTION: Returns TRUE if the loop 'wn_outer' which contains 'wn_loop'
//   can easily be peeled 'peeled_iterations' number of iterations. 
//   Returns FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Peelable(WN* wn_outer, 
		     WN* wn_loop, 
		     INT peeled_iterations)
{ 
  if (peeled_iterations == 0) 
    return TRUE; 
  INT outer_depth = Do_Depth(wn_outer); 
  DO_LOOP_INFO* dli_outer = Get_Do_Loop_Info(wn_outer); 
  DO_LOOP_INFO* dli_loop = Get_Do_Loop_Info(wn_loop); 
  return dli_outer->UB->Num_Vec() == 1 
      && dli_loop->LB->Num_Vec() == 1 && dli_loop->UB->Num_Vec() == 1
      && dli_outer->UB->Dim(0)->Loop_Coeff(outer_depth) == 1;
} 

//-----------------------------------------------------------------------
// NAME: Convex_Peeling_Depth 
// FUNCTION: Returns either -1, 0, or a positive number.  Basically, we 
//   are trying to determine if 'wn_def' will always be executed in the 
//   last iteration of the loop 'wn_outer'.  If so, we return 0.  If we
//   return a positive number N, if we peel off N iterations of 'wn_outer',
//   then 'wn_def' will be executed in the last iteration of 'wn_outer'.
//   Otherwise, we return -1.
//-----------------------------------------------------------------------

static INT Convex_Peeling_Depth(WN* wn_def, 
                                WN* wn_outer)
{
  const INT Peel_Max = 2; 
  WN* wn_inner = Enclosing_Do_Loop(wn_def); 
  DOLOOP_STACK stack(&ARA_memory_pool);
  Build_Doloop_Stack(wn_inner, &stack);
  INT outer_depth = Do_Loop_Depth(wn_outer); 
  INT inner_depth = Do_Loop_Depth(wn_inner); 
  INT i;
  for (i = outer_depth + 1; i <= inner_depth; i++) 
    if (!SNL_Is_Invariant(&stack, outer_depth, i))
      break; 
  if (i == inner_depth + 1) 
    return 0;
  DO_LOOP_INFO** dli = CXX_NEW_ARRAY(DO_LOOP_INFO*, inner_depth + 1, 
    &ARA_memory_pool); 
  for (i = 0; i <= inner_depth; i++) 
    dli[i] = Get_Do_Loop_Info(stack.Bottom_nth(i)); 
  INT peel = 0; 
  INT k;
  for (k = 0; k <= Peel_Max; k++) { 
    SNL_BOUNDS_INFO ebounds_peel(&ARA_memory_pool);
    ebounds_peel.Outermost_Depth() = outer_depth;
    ebounds_peel.Collect_Outer_Info(wn_outer);
    INT i;
    for (i = 0; i < dli[outer_depth]->LB->Num_Vec(); i++) {
      ACCESS_VECTOR* lb = dli[outer_depth]->LB->Dim(i); 
      ACCESS_VECTOR first_iter(lb, &ARA_memory_pool);
      ebounds_peel.Add_Access(&first_iter, FALSE);
    } 
    for (i = 0; i < dli[outer_depth]->UB->Num_Vec(); i++) {
      ACCESS_VECTOR* ub = dli[outer_depth]->UB->Dim(i); 
      ACCESS_VECTOR last_iter(ub, &ARA_memory_pool);
      last_iter.Const_Offset -= k; 
      ebounds_peel.Add_Access(&last_iter, FALSE);
      last_iter.Negate_Me();
      ebounds_peel.Add_Access(&last_iter, FALSE);
    }
    BOOL pbreak = FALSE; 
    for (i = outer_depth + 1; !pbreak && i <= inner_depth; i++) { 
      WN* wn_loop = stack.Bottom_nth(i); 
      for (INT dimlb = 0; !pbreak && dimlb < dli[i]->LB->Num_Vec(); dimlb++) 
	for(INT dimub = 0; !pbreak && dimub < dli[i]->UB->Num_Vec(); dimub++)  
	  if (!Peelable(wn_outer, wn_loop, k) 
	      || !Always_Executes(&ebounds_peel, wn_loop, dimlb, dimub)) 
	    pbreak = TRUE; 
    } 
    if (!pbreak)
      return k; 
  }
  return -1; 
} 

//-----------------------------------------------------------------------
// NAME: Invariant_Loops 
// FUNCTION: Returns TRUE if there is no loop within 'wn_loop' whose trip
//   count depends on an enclosing loop within 'wn_loop' (inclusive).  
//-----------------------------------------------------------------------

static BOOL Invariant_Loops(WN* wn_loop)
{
  if (WN_opcode(wn_loop) != OPC_DO_LOOP)
    return TRUE; 
  INT outer_depth = Do_Depth(wn_loop); 
  LWN_ITER* itr = LWN_WALK_TreeIter(wn_loop); 
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) { 
    WN* wn = itr->wn; 
    if (WN_opcode(wn) == OPC_DO_LOOP) { 
      DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn); 
      if (dli->Is_Inner) { 
	DOLOOP_STACK stack(&ARA_memory_pool);
	Build_Doloop_Stack(wn, &stack); 
	INT inner_depth = Do_Loop_Depth(wn);
        INT i;
	for (i = 0; i <= inner_depth; i++) 
	  if (!Upper_Bound_Standardize(WN_end(stack.Bottom_nth(i)), TRUE))
	    return FALSE; 
	INT nloops = inner_depth - outer_depth + 1; 
	for (i = 2; i <= nloops; i++) {
	  INT d = inner_depth + 1 - i;
	  for (INT dd = d + 1; dd <= inner_depth; dd++)
	    if (!SNL_Is_Invariant(&stack, d, dd))
	      return FALSE;
	}
      }
    }
  }
  return TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: ARA_LOOP_INFO::Determine_Peel
// FUNCTION: Sets _peel_value to -1, 0, or a positive number.  Basically, we
//   are trying to determine if all of the privatizable scalars and arrays
//   within 'wn_outer' for which we need last values will always be executed 
//   in the last iteration of the loop 'wn_outer'.  If so, we return 0.  If 
//   we return a positive number N, if we peel off N iterations of 'wn_outer',
//   then all of these privatizable variables for which we need last values
//   will be executed in the last iteration of 'wn_outer'.  Otherwise, we 
//   set _peel_value to -1. 
//-----------------------------------------------------------------------

void ARA_LOOP_INFO::Determine_Peel()
{
  INT peel = 0;
  INT i;
  INT save_peel_value = _peel_value; 
  _peel_value = 0; 
  if (!Is_OK_Parallel()) { 
    _peel_value = save_peel_value; 
    peel = _peel_value; 
    goto check_kids; 
  } 
  if (Invariant_Loops(_loop)) { 
    peel = 0; 
    goto check_kids; 
  } 
  for (i = 0; i < _scalar_pri.Elements(); i++) {
    if (_scalar_last_value.Bottom_nth(i)) {
      SYMBOL sym = _scalar_pri.Bottom_nth(i)->_scalar;
      STACK<WN*>* stk_sdef = Scalar_Defs(&sym, _loop);
      for (INT j = 0; j < stk_sdef->Elements(); j++) {
        WN* wn = stk_sdef->Bottom_nth(j);
        if (Scalar_Def_Covered_In_Loop(wn, _loop))
          continue;
        INT local_peel = Convex_Peeling_Depth(wn, _loop);
        if (local_peel == -1) { 
	  peel = -1; 
	  if (LNO_Prompl) {  
	    SYMBOL* sym = CXX_NEW(SYMBOL(wn), &ARA_memory_pool); 
	    Scalar_Bad_Peel().Push(*sym); 
	    Ln_Scalar_Bad_Peel().Push(WN_Whirl_Linenum(wn)); 
	  } else 
            goto check_kids;
        } 
        if (local_peel == 0)
          continue;
        if (local_peel > peel)
          peel = local_peel;
      }
    }
  }
  for (i = 0; i < _pri.Elements(); i++) {
    ARA_REF* ara_ref = _pri.Bottom_nth(i);
    if (ara_ref->Is_Loop_Invariant() && !ara_ref->Is_Unknown_Size()
	&& ara_ref->Need_Last_Value()) {
      SYMBOL sym = ara_ref->Array();
      STACK<WN*>* stk_adef = Array_Defs(&sym, _loop);
      for (INT j = 0; j < stk_adef->Elements(); j++) {
        WN* wn = stk_adef->Bottom_nth(j);
        INT local_peel = Convex_Peeling_Depth(wn, _loop);
        if (local_peel == -1) {
	  peel = -1; 
	  if (LNO_Prompl) {
	    Dep_Bad_Peel().Push(SYMBOL(WN_Array_Symbol(wn))); 
	    Ln_Dep_Bad_Peel().Push(WN_Whirl_Linenum(wn));
	  } else 
            goto check_kids;
	} 
        if (local_peel == 0)
          continue;
        if (local_peel > peel)
          peel = local_peel;
      }
    }
  }

check_kids: 

  _peel_value = peel;

  // Adjust Suggested_Parallel for convexity considerations.  
  if (WN_opcode(_loop) == OPC_DO_LOOP) { 
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(_loop); 
    if (_peel_value >= 0 && !dli->Suggested_Parallel) { 
      WN* wn_first = LWN_Get_Parent(_loop); 
      for (WN* wn = wn_first; wn != NULL; wn = LWN_Get_Parent(wn)) {
	if (WN_opcode(wn) == OPC_DO_LOOP) { 
	  DO_LOOP_INFO* dli_loop = Get_Do_Loop_Info(wn); 
	  if (dli_loop->Suggested_Parallel) {  
	    if (dli_loop->ARA_Info != NULL 
		&& dli_loop->ARA_Info->_peel_value == -1) {
	      dli->Suggested_Parallel = TRUE; 
	      double work_estimate = 0;
	      INT nloops = SNL_Loop_Count(_loop);
	      double machine_cycles = SNL_Machine_Cost(_loop, nloops, 0, NULL,
		&work_estimate, TRUE);
	      dli->Work_Estimate = work_estimate;
	      if (Get_Trace(TP_LNOPT2, TT_LNO_PARALLEL_DEBUG))
	        fprintf(stdout, 
		  "Convex Problem: Parallelizing %s instead of %s\n", 
		  WB_Whirl_Symbol(_loop), WB_Whirl_Symbol(wn)); 
	    } 
	  }
	  break; 
	}
      }
    }
  } 
            
  for (i = 0; i < Children().Elements(); ++i) 
    Children().Bottom_nth(i)->Determine_Peel();
}
