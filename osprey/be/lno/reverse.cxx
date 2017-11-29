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


#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#ifdef _KEEP_RCS_ID
/*REFERENCED*/
static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/be/lno/SCCS/s.reverse.cxx $ $Revision: 1.5 $";
#endif /* _KEEP_RCS_ID */

#include <sys/types.h>

#include "lnoutils.h"
#include "lnopt_main.h"
#include "stab.h"
#include "targ_const.h"
#include "wn_simp.h"
#include "stdlib.h"
#include "lwn_util.h"
#include "strtab.h"
#include "config.h"
#include "optimizer.h"
#include "opt_du.h"
#include "name.h"
#include "wintrinsic.h"
#include "lno_bv.h"
#include "dep_graph.h"
#include "debug.h"
#include "cxx_memory.h"
#include "snl_utils.h"
#include "reverse.h"

static ARRAY_DIRECTED_GRAPH16* dg;
static DU_MANAGER* du;

//-----------------------------------------------------------------------
// NAME: RV_Easy_Bounds 
// FUNCTION: Returns TRUE if the bounds of loop 'wn_loop' are easy enough
//   to do a simple loop reversal, FALSE otherwise. 
// NOTE: Simple, in this case, means of the form: 
//   do i = exp1; i <= exp2; i = i + exp3  
//-----------------------------------------------------------------------

static BOOL RV_Easy_Bounds(WN* wn_loop) 
{
  WN* wn_start = WN_start(wn_loop); 
  if (WN_operator(wn_start) != OPR_STID)
    return FALSE; 
  if (SYMBOL(wn_start) != SYMBOL(WN_index(wn_loop)))
    return FALSE;  
  WN* wn_end = WN_end(wn_loop);
  OPERATOR opr = WN_operator(wn_end); 
  if (opr != OPR_GE && opr != OPR_GT && opr != OPR_LE && opr != OPR_LT)
    return FALSE;  
  WN* wn_index = SNL_UBvar(wn_end);  
  if (WN_operator(wn_index) != OPR_LDID) 
    return FALSE; 
  if (SYMBOL(wn_index) != SYMBOL(WN_index(wn_loop)))
    return FALSE;  
  WN* wn_step = WN_step(wn_loop); 
  if (WN_operator(wn_step) != OPR_STID)
    return FALSE; 
  if (SYMBOL(wn_step) != SYMBOL(WN_index(wn_loop)))
    return FALSE;  
  WN* wn_add = WN_kid0(wn_step); 
  if (WN_operator(wn_add) != OPR_ADD)
    return FALSE; 
  WN* wn_left = WN_kid0(wn_add); 
  if (WN_operator(wn_left) != OPR_LDID)
    return FALSE; 
  if (SYMBOL(wn_left) != SYMBOL(WN_index(wn_loop)))
    return FALSE;
  WN* wn_right = WN_kid1(wn_add); 
  if (WN_operator(wn_right) != OPR_INTCONST)
    return FALSE; 
  if (WN_const_val(wn_right) != 1) 
    return FALSE;  
  return TRUE; 
}

//-----------------------------------------------------------------------
// NAME: RV_Scalar_Node_Legal 
// FUNCTION: Returns TRUE if node 'wn' is a STID or LDID and does not 
//   inhibit reversal of loop 'wn_loop' .  Also returns TRUE if the node  
//   is not a STID or LDID.  Returns FALSE, otherwise.  
//-----------------------------------------------------------------------

static BOOL RV_Scalar_Node_Legal(WN* wn, 
			         WN* wn_loop) 
{
  if (WN_operator(wn) == OPR_STID) {
    if (dg->Get_Vertex(wn))
      return FALSE;
    for (WN* wn_tp = wn_loop; wn_tp != NULL; wn_tp = LWN_Get_Parent(wn_tp)) 
      if (WN_opcode(wn) == OPC_DO_LOOP 
	  && SYMBOL(wn) == SYMBOL(WN_index(wn_tp)))
        return TRUE; 
    USE_LIST* use_list = du->Du_Get_Use(wn); 
    if (use_list == NULL) 
      return TRUE; 
    if (use_list->Incomplete())
      return FALSE; 
    USE_LIST_ITER iter(use_list);
    DU_NODE* node = NULL;
    for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
      WN* wn_use = node->Wn();
      if (!Wn_Is_Inside(wn_use, wn_loop))
	return FALSE; 
    }
  } else if (WN_operator(wn) == OPR_LDID) {
    if (dg->Get_Vertex(wn))
      return FALSE; 
    for (WN* wn_tp = wn_loop; wn_tp != NULL; wn_tp = LWN_Get_Parent(wn_tp)) 
      if (WN_opcode(wn_tp) == OPC_DO_LOOP 
	  && SYMBOL(wn) == SYMBOL(WN_index(wn_tp)))
        return TRUE;
    DEF_LIST *def_list = du->Ud_Get_Def(wn);
    if (def_list == NULL) 
      return TRUE; 
    if (def_list->Incomplete()) {
      DEF_LIST_ITER iter(def_list); 
      DU_NODE* node = NULL;
      for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
        WN* wn_def = node->Wn();
        if (Wn_Is_Inside(wn_def, wn_loop))
          return FALSE;
      }
    } else {
      if (def_list->Loop_stmt() != NULL 
	  && (!Wn_Is_Inside(def_list->Loop_stmt(), wn_loop)
	  || def_list->Loop_stmt() == wn_loop))
	return FALSE;
    }
  }
  return TRUE;    
}   
  
//-----------------------------------------------------------------------
// NAME: RV_Scalar_Legal 
// FUNCTION: Returns TRUE if the expression rooted at 'wn' does not contain
//   scalars which inhibit loop reversal.  Returns FALSE, otherwise.  
//-----------------------------------------------------------------------

static BOOL RV_Scalar_Legal(WN* wn, 
		            WN* wn_loop)
{
  if (!RV_Scalar_Node_Legal(wn, wn_loop))
    return FALSE; 

  if (WN_opcode(wn) == OPC_BLOCK) {
    for (WN* wn_tp = WN_first(wn); wn_tp != NULL; wn_tp = WN_next(wn_tp))
      if (!RV_Scalar_Legal(wn_tp, wn_loop))
        return FALSE; 
  } else {
    for (INT i = 0; i < WN_kid_count(wn); i++) {
      if (!RV_Scalar_Legal(WN_kid(wn, i), wn_loop))
	return FALSE; 
    }
  }
  return TRUE; 
}

//-----------------------------------------------------------------------
// NAME: RV_Depv_Is_Reversable 
// FUNCTION: Returns TRUE if the 'array_index'th dependence vector in 
//   'depv_array' can have its component corresponding to loop 'wn_loop'
//   reversed without becoming lexicographically negative.  Returns 
//   FALSE otherwise.  
//-----------------------------------------------------------------------

static BOOL RV_Depv_Is_Reversable(DEPV_ARRAY* depv_array,
				  INT array_index,  
				  WN* wn_loop) 
{
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 
  INT dep_index = dli->Depth - depv_array->Num_Unused_Dim(); 
  DEPV* depv = depv_array->Depv(array_index);  
  for (INT i = 0; i < dep_index; i++) 
    if (DEP_Direction(DEPV_Dep(depv, i)) == DIR_POS)
      return TRUE; 
  DIRECTION dir = DEP_Direction(DEPV_Dep(depv, dep_index)); 
  if (dir == DIR_NEG || dir == DIR_EQ || dir == DIR_NEGEQ) 
    return TRUE;
  return FALSE; 
} 

//-----------------------------------------------------------------------
// NAME: RV_Array_Node_Legal 
// FUNCTION: Returns TRUE if no dependence incident on node 'wn' inhibits
//   the loop reversal of loop 'wn_loop'.  Returns FALSE, otherwise.  Use 
//   'edge_table' to keep track of which edges we have already checked.     
//-----------------------------------------------------------------------

static BOOL RV_Array_Node_Legal(WN* wn, 
			   WN* wn_loop,
			   HASH_TABLE<EINDEX16,INT>* edge_table) 
{
  OPERATOR opr = WN_operator(wn);
  if (opr == OPR_ILOAD || opr == OPR_ISTORE 
      || opr == OPR_LDID || opr == OPR_STID) {
    VINDEX16 v = dg->Get_Vertex(wn);
    if (v == 0)
      return (opr == OPR_LDID || opr == OPR_STID); 
    EINDEX16 e = 0;
    for (e = dg->Get_In_Edge(v); e != 0; e = dg->Get_Next_In_Edge(e)) {
      if (edge_table->Find(e))
	continue; 
      edge_table->Enter(e, 1); 
      DEPV_ARRAY* depv_array = dg->Depv_Array(e); 
      for (INT i = 0; i < depv_array->Num_Vec(); i++)
	if (!RV_Depv_Is_Reversable(depv_array, i, wn_loop))
	  return FALSE; 
    }
    for (e = dg->Get_In_Edge(v); e != 0; e = dg->Get_Next_In_Edge(e)) {
      if (edge_table->Find(e))
	continue; 
      edge_table->Enter(e, 1); 
      DEPV_ARRAY* depv_array = dg->Depv_Array(e); 
      for (INT i = 0; i < depv_array->Num_Vec(); i++)
	if (!RV_Depv_Is_Reversable(depv_array, i, wn_loop))
	  return FALSE; 
    }
  }
  return TRUE; 
}

//-----------------------------------------------------------------------
// NAME: RV_Array_Legal 
// FUNCTION: Returns TRUE if no dependence incident on a node in the tree
//   rooted at 'wn' inhibits the loop reversal of loop 'wn_loop'.  Returns
//   FALSE, otherwise.  Use 'edge_table' to keep track of which edges 
//   we have already checked.  
//----------------------------------------------------------------------
static BOOL RV_Array_Legal(WN* wn, 
	                   WN* wn_loop, 
		           HASH_TABLE<EINDEX16,INT>* edge_table)
{
  if (!RV_Array_Node_Legal(wn, wn_loop, edge_table))
    return FALSE; 

  if (WN_opcode(wn) == OPC_BLOCK) {
    for (WN* wn_tp = WN_first(wn); wn_tp != NULL; wn_tp = WN_next(wn_tp))
      if (!RV_Array_Legal(wn_tp, wn_loop, edge_table))
        return FALSE; 
  } else {
    for (INT i = 0; i < WN_kid_count(wn); i++) {
      if (!RV_Array_Legal(WN_kid(wn, i), wn_loop, edge_table))
	return FALSE; 
    }
  }
  return TRUE; 
}

//-----------------------------------------------------------------------
// NAME: RV_Is_Legal 
// FUNCTION: Returns TRUE if reversing loop 'wn_loop' is legal, FALSE 
//   otherwise. 
//-----------------------------------------------------------------------

extern BOOL RV_Is_Legal(WN* wn_loop)
{ 
  dg = Array_Dependence_Graph; 
  du = Du_Mgr; 
  if (!Do_Loop_Is_Good(wn_loop))
    return FALSE; 
  if (!RV_Easy_Bounds(wn_loop))  
    return FALSE;
  if (!RV_Scalar_Legal(wn_loop, wn_loop)) 
    return FALSE;
  INT hash_table_size = MIN(dg->Get_Edge_Count(), 512); 
  HASH_TABLE<EINDEX16,INT> edge_table(hash_table_size, &LNO_local_pool); 
  if (!RV_Array_Legal(wn_loop, wn_loop, &edge_table))
    return FALSE; 
  return TRUE; 
}

//-----------------------------------------------------------------------
// NAME: RV_Reverse_Index_Ldid 
// FUNCTION: Replace 'wn_index' with 'lower_bound + upper_bound - wn_index',
//   where 'lower_bound' and 'upper_bound' are the lower and upper bounds 
//   of the index variable of loop 'wn_loop'.   
//-----------------------------------------------------------------------

static void RV_Reverse_Index_Ldid(WN* wn_index, 
				  WN* wn_loop) 
{
  WN* wn_parent = LWN_Get_Parent(wn_index); 
  INT i;
  for (i = 0; i < WN_kid_count(wn_parent); i++) 
    if (WN_kid(wn_parent, i) == wn_index) 
      break;
  INT replace_index = i; 
  WN* wn_lower_bound = WN_kid0(WN_start(wn_loop)); 
  WN* wn_upper_bound = SNL_UBexp(WN_end(wn_loop));
  WN* wn_new_lb = LWN_Copy_Tree(wn_lower_bound); 
  WN* wn_new_ub = LWN_Copy_Tree(wn_upper_bound); 
  WN* wn_new_index = LWN_Copy_Tree(wn_index); 
  if (du != NULL) { 
    LWN_Copy_Def_Use(wn_lower_bound, wn_new_lb, du);  
    LWN_Copy_Def_Use(wn_upper_bound, wn_new_ub, du);  
    LWN_Copy_Def_Use(wn_index, wn_new_index, du);  
  }
  OPERATOR opr = WN_operator(WN_end(wn_loop));
  TYPE_ID tid = Do_Wtype(wn_loop); 
  if (opr == OPR_GT || opr == OPR_LT) {
    OPCODE op = OPCODE_make_op(OPR_INTCONST, tid, MTYPE_V); 
    WN* wn_minus_one = WN_CreateIntconst(op, (INT64) -1);
    OPCODE op_add = OPCODE_make_op(OPR_ADD, tid, MTYPE_V);
    wn_new_ub = LWN_CreateExp2(op_add, wn_new_ub, wn_minus_one); 
  } 
  OPCODE opc_add = OPCODE_make_op(OPR_ADD, tid, MTYPE_V);  
  WN* wn_sum = LWN_CreateExp2(opc_add, wn_new_lb, wn_new_ub);  
  OPCODE opc_sub = OPCODE_make_op(OPR_SUB, tid, MTYPE_V);  
  WN* wn_result = LWN_CreateExp2(opc_sub, wn_sum, wn_new_index);
  WN_kid(wn_parent, replace_index) = wn_result;  
  LWN_Set_Parent(wn_result, wn_parent);   
  LWN_Delete_Tree(wn_index);
} 

//-----------------------------------------------------------------------
// NAME: RV_Reverse_Indices 
// FUNCTION: Replace the references to the index variable of loop 'wn_loop' 
//  with their reversed value.  In other words, if 'i' is index variable 
//  of 'wn_loop', replace the references to 'i' in 'wn_tree' with: 
//  'lower_bound + upper_bound - i', where 'lower_bound' and 'upper_bound' 
//  are the lower and upper bounds of 'i' in 'wn_loop'.   
//-----------------------------------------------------------------------

static void RV_Reverse_Indices(WN* wn_tree, 
			       WN* wn_loop)
{
  if (WN_operator(wn_tree) == OPR_LDID
      && SYMBOL(wn_tree) == SYMBOL(WN_index(wn_loop))) {
    RV_Reverse_Index_Ldid(wn_tree, wn_loop); 
  } else if (WN_opcode(wn_tree) == OPC_BLOCK) {
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      RV_Reverse_Indices(wn, wn_loop); 
  } else { 
    for (INT i = 0; i < WN_kid_count(wn_tree); i++) 
      RV_Reverse_Indices(WN_kid(wn_tree, i), wn_loop); 
  }
}

//-----------------------------------------------------------------------
// NAME: RV_Reverse_Dependence 
// FUNCTION: Reverse the dependence component corresponding to the loop 
//   'wn_loop' in the 'array_index'th element of the 'depv_array'.  
//-----------------------------------------------------------------------

static void RV_Reverse_Dependence(DEPV_ARRAY* depv_array,
				  INT array_index,  
			          WN* wn_loop) 
{ 
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 
  INT dep_index = dli->Depth - depv_array->Num_Unused_Dim();
  DEPV* depv = depv_array->Depv(array_index); 
  DEPV_Dep(depv, dep_index) = DEP_Negate(DEPV_Dep(depv, dep_index));  
}

//-----------------------------------------------------------------------
// NAME: RV_Reverse_Node_Dependences 
// FUNCTION: Reverse all of the components of the dependence edges corres- 
//   ponding to 'wn_loop' which are incident on node 'wn', if those com-
//   ponents have not already been reversed.  Use the 'edge_table' to 
//   keep track of which edges we have already operated on. 
//-----------------------------------------------------------------------

static void RV_Reverse_Node_Dependences(WN* wn, 
				        WN* wn_loop, 
					HASH_TABLE<EINDEX16,INT>* edge_table)
{
  if (OPCODE_is_load(WN_opcode(wn)) || OPCODE_is_store(WN_opcode(wn))) {
    VINDEX16 v = dg->Get_Vertex(wn);
    if (v == 0) 
      return; 
    EINDEX16 e = 0;
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 
    INT loop_depth = dli->Depth;  
    for (e = dg->Get_In_Edge(v); e != 0; e = dg->Get_Next_In_Edge(e)) {
      if (edge_table->Find(e))
	continue; 
      edge_table->Enter(e, 1); 
      DEPV_ARRAY* depv_array = dg->Depv_Array(e); 
      for (INT i = 0; i < depv_array->Num_Vec(); i++)
	RV_Reverse_Dependence(depv_array, i, wn_loop); 
    }
    for (e = dg->Get_In_Edge(v); e != 0; e = dg->Get_Next_In_Edge(e)) {
      if (edge_table->Find(e))
	continue; 
      edge_table->Enter(e, 1); 
      DEPV_ARRAY* depv_array = dg->Depv_Array(e); 
      for (INT i = 0; i < depv_array->Num_Vec(); i++)
	RV_Reverse_Dependence(depv_array, i, wn_loop); 
    }
  }
}

//-----------------------------------------------------------------------
// NAME: RV_Reverse_Dependences 
// FUNCTION: Reverse all of the components in the tree rooted at 'wn'
//   corresponding to the loop 'wn_loop'.  Use the 'edge_table' to 
//   keep track of which edges we have already operated on.  
//-----------------------------------------------------------------------

static void RV_Reverse_Dependences(WN* wn, 
			           WN* wn_loop,
				   HASH_TABLE<EINDEX16,INT>* edge_table) 
{
  RV_Reverse_Node_Dependences(wn, wn_loop, edge_table);
  if (WN_opcode(wn) == OPC_BLOCK) {
    for (WN* wn_tp = WN_first(wn); wn_tp != NULL; wn_tp = WN_next(wn_tp))
      RV_Reverse_Dependences(wn_tp, wn_loop, edge_table);
  } else {
    for (INT i = 0; i < WN_kid_count(wn); i++) 
      RV_Reverse_Dependences(WN_kid(wn, i), wn_loop, edge_table);
  }
}  

//-----------------------------------------------------------------------
// NAME: RV_Simplify_Indicies 
// FUNCTION: Simplify the array indicies in the tree rooted at 'wn_tree'.  
//-----------------------------------------------------------------------

static void RV_Simplify_Indicies(WN* wn_tree) 
{
  if (WN_operator(wn_tree) == OPR_ARRAY) {
    LWN_Simplify_Tree(wn_tree); 
  } else if (WN_opcode(wn_tree) == OPC_BLOCK) {
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      RV_Simplify_Indicies(wn); 
  } else {
    for (INT i = 0; i < WN_kid_count(wn_tree); i++) 
      RV_Simplify_Indicies(WN_kid(wn_tree, i));  
  }
}

//-----------------------------------------------------------------------
// NAME: RV_Reverse_Loop  
// FUNCTION: Reverse the loop 'wn_loop'.  We assume that the loop has 
//   already been determined to be reversible.  
//-----------------------------------------------------------------------

extern void RV_Reverse_Loop(WN* wn_loop)
{
  if (LNO_Verbose) {
    fprintf(stdout, "Reversing Loop %s\n", WB_Whirl_Symbol(WN_index(wn_loop))); 
    fprintf(TFile, "Reversing Loop %s\n", WB_Whirl_Symbol(WN_index(wn_loop))); 
  }
  dg = Array_Dependence_Graph; 
  du = Du_Mgr;  
  RV_Reverse_Indices(WN_do_body(wn_loop), wn_loop);
  INT hash_table_size = MIN(dg->Get_Edge_Count(), 512);
  HASH_TABLE<EINDEX16,INT> edge_table(hash_table_size, &LNO_local_pool);
  RV_Reverse_Dependences(WN_do_body(wn_loop), wn_loop, &edge_table);   
  RV_Simplify_Indicies(WN_do_body(wn_loop));  
  DOLOOP_STACK loop_stack(&LNO_local_pool); 
  Build_Doloop_Stack(LWN_Get_Parent(wn_loop), &loop_stack); 
  LNO_Build_Access(wn_loop, &loop_stack, &LNO_default_pool); 
  DO_LOOP_INFO *dli = Get_Do_Loop_Info(wn_loop); 
  dli->Is_Backward = Do_Loop_Is_Backward(wn_loop); 
}

//-----------------------------------------------------------------------
// NAME: RV_Traverse
// FUNCTION: Reverse every loop in the tree rooted at 'wn_tree'.
// NOTE: This function is provided only for testing 'RV_Is_Legal()' 
//   and 'RV_Reverse_Loop()'.  
//-----------------------------------------------------------------------

static void RV_Traverse(WN* wn_tree) 
{
  if (WN_opcode(wn_tree) == OPC_DO_LOOP) {
    if (RV_Is_Legal(wn_tree))
      RV_Reverse_Loop(wn_tree);
    if (LNO_Verbose) {
      DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_tree);
      if (!dli->Is_Backward) {
	fprintf(stdout, "Loop 0x%p is indexed forward\n", wn_tree); 
	fprintf(TFile, "Loop 0x%p is indexed forward\n", wn_tree); 
      } else {
	fprintf(stdout, "Loop 0x%p is indexed backward\n", wn_tree); 
	fprintf(TFile, "Loop 0x%p is indexed backward\n", wn_tree); 
      }
    }
  }
  if (WN_opcode(wn_tree) == OPC_BLOCK) {
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      RV_Traverse(wn);
  } else {
    for (INT i = 0; i < WN_kid_count(wn_tree); i++) 
      RV_Traverse(WN_kid(wn_tree, i));
  }
}

//-----------------------------------------------------------------------
// NAME: Reverse_Loops  
// FUNCTION: Reverse every loop in the 'func_nd' for which loop reversal 
//   is legal. 
// NOTE: This function is provided only for testing 'RV_Is_Legal()' 
//   and 'RV_Reverse_Loop()'. 
//-----------------------------------------------------------------------

extern void Reverse_Loops(WN* func_nd) 
{
  if (!LNO_Blind_Loop_Reversal)
    return;
  if (LNO_Verbose) {
    fprintf(stdout, "Applying Loop Reversal\n");
    fprintf(TFile, "Applying Loop Reversal\n");
  }
  (void) RV_Traverse(func_nd);
  if (LNO_Verbose) {
    fprintf(stdout, "Loop Reversal Complete\n");
    fprintf(TFile, "Loop Reversal Complete\n");
  }
}

//-----------------------------------------------------------------------
// NAME: RV_Evaluate 
// FUNCTION: Returns the value of the expression rooted at 'wn_tree', 
//   if it is actually an integer expression.  Sets '*valid' to FALSE if 
//   'wn_tree' is not the root of an integer expression.  
//-----------------------------------------------------------------------

static INT RV_Evaluate(WN* wn_tree, BOOL* valid) 
{
  INT numeric1 = 0; 
  INT numeric2 = 0;
  BOOL valid1 = TRUE; 
  BOOL valid2 = TRUE; 
  switch (WN_operator(wn_tree)) {
  case OPR_INTCONST:  
    *valid = TRUE; 
    return WN_const_val(wn_tree); 
  case OPR_NEG:
    numeric1 = RV_Evaluate(WN_kid0(wn_tree), &valid1);  
    if (!valid1) { 
      *valid = FALSE; 
      return 0; 
    } else { 
      *valid = TRUE; 
      return numeric1; 
    } 
  case OPR_ADD: 
    numeric1 = RV_Evaluate(WN_kid0(wn_tree), &valid1); 
    numeric2 = RV_Evaluate(WN_kid1(wn_tree), &valid2); 
    if (!valid1 || !valid2) {
      *valid = FALSE; 
      return 0; 
    } else {
      *valid = TRUE; 
      return numeric1 + numeric2; 
    }
  case OPR_SUB: 
    numeric1 = RV_Evaluate(WN_kid0(wn_tree), &valid1); 
    numeric2 = RV_Evaluate(WN_kid1(wn_tree), &valid2); 
    if (!valid1 || !valid2) {
      *valid = FALSE; 
      return 0; 
    } else {
      *valid = TRUE; 
      return numeric1 - numeric2; 
    }
  case OPR_MPY: 
    numeric1 = RV_Evaluate(WN_kid0(wn_tree), &valid1); 
    numeric2 = RV_Evaluate(WN_kid1(wn_tree), &valid2); 
    if (!valid1 || !valid2) {
      *valid = FALSE; 
      return 0; 
    } else {
      *valid = TRUE; 
      return numeric1 * numeric2; 
    }
  case OPR_DIV: 
    numeric1 = RV_Evaluate(WN_kid0(wn_tree), &valid1); 
    numeric2 = RV_Evaluate(WN_kid1(wn_tree), &valid2); 
    if (!valid1 || !valid2) {
      *valid = FALSE; 
      return 0; 
    } else {
      *valid = TRUE; 
      return numeric1 / numeric2; 
    }
  default:
    *valid = FALSE; 
    return 0;  
  }
}

//-----------------------------------------------------------------------
// NAME: RV_Sign 
// FUNCTION: Returns 1 if the constant expression rooted at 'wn_tree' is 
//   provably positive or zero, -1 if it is provably negative, and 0 
//   otherwise.  
//-----------------------------------------------------------------------

static INT RV_Sign(WN* wn_tree)
{
  BOOL valid = FALSE; 
  INT numeric = RV_Evaluate(wn_tree, &valid); 
  if (!valid) 
    return 0; 
  return numeric < 0 ? -1 : 1; 
}

//-----------------------------------------------------------------------
// NAME: RV_Index_Sign
// FUNCTION: Returns -1 if the coefficient of 'wn_index' is provably 
//   negative, +1 if the coefficient of 'wn_index' is provably positive, 
//   and 0 otherwise.   
//-----------------------------------------------------------------------

static INT RV_Index_Sign(WN* wn_index) 
{
  INT sign = 1; 
  WN* wn = NULL; 
  WN* wn_last = wn_index;
  BOOL break_loop = FALSE; 
  for (wn = LWN_Get_Parent(wn_index); ; wn_last = wn, wn = LWN_Get_Parent(wn)) {
    if (break_loop || !OPCODE_is_expression(WN_opcode(wn))) 
      break;
    switch (WN_operator(wn)) {
    case OPR_NEG: 
      sign = -sign; 
      break;
    case OPR_ADD: 
    case OPR_CVT: 
      break;   
    case OPR_SUB: 
      sign = (wn_last == WN_kid1(wn)) ? -sign : sign;
      break; 
    case OPR_MPY: 
    case OPR_DIV: 
      {
        for (INT i = 0; i < WN_kid_count(wn); i++) {
	  if (WN_kid(wn, i) == wn_last) 
   	    continue; 
	  INT sibling_sign = RV_Sign(WN_kid(wn, i)); 
          if (sibling_sign == 0) 
	    return 0; 
   	    sign *= sibling_sign;  
        }
      }
      break;
    case OPR_ARRAY: 
    case OPR_PARM:
    case OPR_INTRINSIC_OP:
#ifdef KEY
    case OPR_PURE_CALL_OP:
#endif
      break_loop = TRUE; 
      break; 
    default:  
      return 0;
    }
  }  
  return (sign < 0 ? -1 : 1); 
}

//-----------------------------------------------------------------------
// NAME: RV_Find_Backward_Indices 
// FUNCTION: Returns TRUE if all of the indices of 'wn_loop' in 'wn_tree'
//  have provably negative coefficients, FALSE otherwise.  
//-----------------------------------------------------------------------

static BOOL RV_Tree_Has_All_Backward_Indices(WN* wn_tree, 
				             WN* wn_loop, 
					     BOOL* found_backward)
{
  if (WN_operator(wn_tree) == OPR_LDID
      && SYMBOL(wn_tree) == SYMBOL(WN_index(wn_loop))) {
    if (RV_Index_Sign(wn_tree) >= 0)
      return FALSE; 
    *found_backward = TRUE; 
  } else if (WN_opcode(wn_tree) == OPC_BLOCK) {
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      if (!RV_Tree_Has_All_Backward_Indices(wn, wn_loop, found_backward))
	return FALSE;
  } else {
    for (INT i = 0; i < WN_kid_count(wn_tree); i++)
      if (!RV_Tree_Has_All_Backward_Indices(WN_kid(wn_tree, i), wn_loop, 
	found_backward))
	return FALSE; 
  }
  return TRUE; 
} 


//-----------------------------------------------------------------------
// NAME: RV_Find_Written_Indices
// FUNCTION: Returns TRUE if all of the indices of 'wn_loop' in 'wn_tree'
//  have been written, FALSE otherwise.
//-----------------------------------------------------------------------

static BOOL RV_Tree_Has_All_Written_Indices(WN* wn_tree,
                                             WN* wn_loop)
{
  if (WN_operator(wn_tree) == OPR_STID
      && SYMBOL(wn_tree) == SYMBOL(WN_index(wn_loop))) {
    if (RV_Index_Sign(wn_tree) >= 0)
      return TRUE;
  } else if (WN_opcode(wn_tree) == OPC_BLOCK) {
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      if (RV_Tree_Has_All_Written_Indices(wn, wn_loop))
        return TRUE;
  } else {
    for (INT i = 0; i < WN_kid_count(wn_tree); i++)
      if (RV_Tree_Has_All_Written_Indices(WN_kid(wn_tree, i), wn_loop))
        return TRUE;
  }
  return FALSE;
}


//-----------------------------------------------------------------------
// NAME: Do_Loop_Is_Backward
// FUNCTION: Returns TRUE if all of the do loop indices in the body of
//   'wn_loop' have provably negative coefficients.  Otherwise, returns
//   FALSE.  
// NOTE: This is a simple algorithm that only looks at arithmetic expres-
//   sions.  It is inconclusive otherwise. 
//-----------------------------------------------------------------------

extern BOOL Do_Loop_Is_Backward(WN* wn_loop)
{
  BOOL found_backward = FALSE; 
  return RV_Tree_Has_All_Backward_Indices(WN_do_body(wn_loop), wn_loop, 
    &found_backward) && found_backward; 
}


//-----------------------------------------------------------------------
// NAME: Do_Loop_Is_Regular
// FUNCTION: Returns TRUE if all of the do loop indices in the body of
// 'wn_loop' has been written.  Otherwise, returns FALSE.
// NOTE: This is a simple algorithm that only looks at arithmetic expres-
// sions.  It is inconclusive otherwise.
//-----------------------------------------------------------------------

extern BOOL Do_Loop_Is_Regular(WN* wn_loop)
{
  return !RV_Tree_Has_All_Written_Indices(WN_do_body(wn_loop), wn_loop);				
}

