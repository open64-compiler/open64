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

#include <sys/types.h>

#include "lnopt_main.h"
#include "config.h"
#include "config_lno.h"
#include "strtab.h"
#include "stab.h"
#include "targ_const.h"

#include "lnoutils.h"
#include "wn_simp.h"
#include "stdlib.h"
#include "lwn_util.h"
#include "optimizer.h"
#include "opt_du.h"
#include "name.h"
#include "wintrinsic.h"
#include "lno_bv.h"
#include "dep_graph.h"
#include "debug.h"
#include "scalar_expand.h"
#include "cxx_memory.h"
#include "reduc.h"
#include "fiz_fuse.h"
#include "snl_utils.h"
#include "forward.h"
#include "minvariant.h"
#include "cond.h"
#include "move.h" 
#include "ipl_lno_util.h" 

#define MAX_NAME_SIZE 75 
#define HMB_ABS_CODE_EXPANSION 	1000 
#define HMB_REL_CODE_EXPANSION  3

static INT preg_counter = 0;

enum HMB_BOUND {HMB_LEFT, HMB_RIGHT, HMB_NONE}; 

//-----------------------------------------------------------------------
// NAME: HMB_Invariant_In_Loop
// FUNCTION: Returns TRUE if all of the elements in 'wn_exp' are 
//   invariant with respect to loop 'wn_loop', FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL HMB_Invariant_In_Loop(WN* wn_exp, 
				  WN* wn_loop) 
{
  DU_MANAGER* du = Du_Mgr;
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph;
  LWN_ITER* itr = LWN_WALK_TreeIter(wn_exp);
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn; 
    if (WN_operator(wn) == OPR_ILOAD) {
      VINDEX16 v = dg->Get_Vertex(wn);
      if (v == 0)
	continue; 
      EINDEX16 e = 0; 
      for (e = dg->Get_In_Edge(v); e != 0; e = dg->Get_Next_In_Edge(e)) {
	WN* wn_source = dg->Get_Wn(dg->Get_Source(e));
	if (Wn_Is_Inside(wn_source, wn_loop))
	  return FALSE;
      }
      for (e = dg->Get_Out_Edge(v); e != 0; e = dg->Get_Next_Out_Edge(e)) {
	WN* wn_sink = dg->Get_Wn(dg->Get_Sink(e));
	if (Wn_Is_Inside(wn_sink, wn_loop))
	  return FALSE;
      }
    } else if (WN_operator(wn) == OPR_LDID
	|| WN_operator(wn) == OPR_PARM) {
      DEF_LIST* def_list = du->Ud_Get_Def(wn);
      DEF_LIST_ITER iter(def_list);
      const DU_NODE* node = NULL;
      for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
	WN* wn_def = node->Wn(); 
	if (Wn_Is_Inside(wn_def, wn_loop))
	  return FALSE;
      }
    }
  }
  return TRUE;
}

//-----------------------------------------------------------------------
// NAME: HMB_Copy_Array_Deps 
// FUNCTION: Copy the array dependences for the node 'wn_orig' to those in 
//   'wn_copy', updating them as appropriate because their locations in the 
//   program unit tree are slightly different. The boolean 'inside_loop' 
//   indicates if the nodes in the expression 'wn_copy' are inside a loop.  
//-----------------------------------------------------------------------

static void HMB_Copy_Array_Deps(WN* wn_orig, 
			        WN* wn_copy, 
				BOOL inside_loop, 
				LS_IN_LOOP* loop_ls) 
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph;
  DOLOOP_STACK copy_stack(&LNO_local_pool);
  Build_Doloop_Stack(wn_copy, &copy_stack);
  LNO_Build_Access(WN_kid0(wn_copy), &copy_stack, &LNO_default_pool);
  if (!inside_loop) 
    return;   
  VINDEX16 v = dg->Get_Vertex(wn_orig);
  if (v == 0) 
    return; 
  STACK<WN*> wn_stack(&LNO_local_pool);
  EINDEX16 e = 0;
  INT node_position = loop_ls->In(wn_orig); 
  for (e = dg->Get_In_Edge(v); e != 0; e = dg->Get_Next_In_Edge(e)) {
    WN* wn_source = dg->Get_Wn(dg->Get_Source(e));
    INT i;
    for (i = 0; i < wn_stack.Elements(); i++) 
      if (wn_stack.Bottom_nth(i) == wn_source)
	break;
    if (i == wn_stack.Elements())
      wn_stack.Push(wn_source); 
  }
  for (e = dg->Get_Out_Edge(v); e != 0; e = dg->Get_Next_Out_Edge(e)) {
    WN* wn_sink = dg->Get_Wn(dg->Get_Sink(e));
    INT i;
    for (i = 0; i < wn_stack.Elements(); i++) 
      if (wn_stack.Bottom_nth(i) == wn_sink)
	break;
    if (i == wn_stack.Elements())
      wn_stack.Push(wn_sink); 
  }
  dg->Add_Vertex(wn_copy);
  DOLOOP_STACK other_stack(&LNO_local_pool); 
  INT i;
  for (i = 0; i < wn_stack.Elements(); i++) {
    WN* wn_other = wn_stack.Bottom_nth(i); 
    Build_Doloop_Stack(wn_other, &other_stack); 
    if (!dg->Add_Edge(wn_copy, &copy_stack, wn_other, &other_stack, 
      node_position < loop_ls->In(wn_other))) {
      LNO_Erase_Dg_From_Here_In(wn_other, dg);
    }
    other_stack.Clear(); 
  }
}

//-----------------------------------------------------------------------
// NAME: HMB_Copy_Array_Deps_Exp 
// FUNCTION: Copy the array dependences from the nodes in the expression
//   rooted at 'wn_orig' to those in 'wn_copy', updating them as appro-
//   priate because their locations in the program unit tree are slightly 
//   different. The boolean 'inside_loop' indicates if the nodes in the 
//   expression 'wn_copy' are inside a loop.  
//-----------------------------------------------------------------------

static void HMB_Copy_Array_Deps_Exp(WN* wn_orig, 
			            WN* wn_copy,
			            BOOL inside_loop,  
				    LS_IN_LOOP* loop_ls) 
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph;
  if (WN_operator(wn_orig) == OPR_ILOAD) 
    HMB_Copy_Array_Deps(wn_orig, wn_copy, inside_loop, loop_ls); 
  for (INT i = 0; i < WN_kid_count(wn_orig); i++) {
    WN* wn_orig_kid = WN_kid(wn_orig, i); 
    WN* wn_copy_kid = WN_kid(wn_copy, i); 
    HMB_Copy_Array_Deps_Exp(wn_orig_kid, wn_copy_kid, inside_loop, loop_ls); 
  }   
}

//-----------------------------------------------------------------------
// NAME: HMB_Replace_Messy_Bounds 
// FUNCTION: Replaces the expression 'wn_exp' with an LDID and defines
//   that LDID outside the 'wn_hoist_loop' with an STID. The LDID and STID
//   are both given the name 'name'.  
//-----------------------------------------------------------------------
static void HMB_Replace_Messy_Bounds(WN* wn_exp, 
			             WN* wn_hoist_loop,
				     HMB_BOUND bnd_type, 
				     WN* wn_guard, 
 			             LS_IN_LOOP* loop_ls, 
			             char name[], 
				     WN* wn_bound) 
{
  if (LNO_Verbose) {
     fprintf(stdout, " HMB: Replacing Messy Bound for Loop 0x%p\n",
       wn_hoist_loop);
     fprintf(TFile, " HMB: Replacing Messy Bound for Loop 0x%p\n",
       wn_hoist_loop);
  }
  DU_MANAGER* du = Du_Mgr;
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph;
  TYPE_ID type = WN_rtype(wn_exp);
#ifdef KEY //Bug 11381: retain the original type for messy loop bounds
           //TODO: should unfold CVT
  if(WN_opcode(wn_exp)==OPC_U4U8CVT || WN_opcode(wn_exp)==OPC_U4I8CVT){
      TYPE_ID dtype = WN_desc(WN_kid0(wn_exp));
    if(WN_operator(WN_kid0(wn_exp)) == OPR_ILOAD && dtype == MTYPE_I4)
      type = MTYPE_I4;
  }
#endif
  OPCODE preg_s_opcode = OPCODE_make_op(OPR_STID, MTYPE_V, type);
  OPCODE preg_l_opcode = OPCODE_make_op(OPR_LDID, 
    Promote_Type(OPCODE_desc(preg_s_opcode)), OPCODE_desc(preg_s_opcode));
  if (wn_bound != NULL) { 
    WN* wn_ldid = LWN_CreateLdid(preg_l_opcode, wn_bound);
    du->Add_Def_Use(wn_bound, wn_ldid);  
    WN* wn_parent = LWN_Get_Parent(wn_exp); 
    INT i;
    for (i = 0; i < WN_kid_count(wn_parent); i++) 
      if (WN_kid(wn_parent, i) == wn_exp)
	break; 
    LWN_Set_Parent(wn_ldid, wn_parent); 
    WN_kid(wn_parent, i) = wn_ldid; 
    LWN_Delete_Tree(wn_exp); 
  } else { 
    WN* wn_exp_copy = LWN_Copy_Tree(wn_exp);
    LWN_Copy_Def_Use(wn_exp, wn_exp_copy, du);
#ifdef _NEW_SYMTAB
    WN_OFFSET preg_num = Create_Preg(type, name);
#else
    WN_OFFSET preg_num = Create_Preg(type, name, NULL);
#endif
    ST* preg_st = MTYPE_To_PREG(type);
    WN* wn_stid = LWN_CreateStid(preg_s_opcode, preg_num, preg_st,
                                 Be_Type_Tbl(type), wn_exp_copy);
    INT inside_loop = Do_Loop_Depth(wn_hoist_loop) > 0; 
    WN* wn_loop = wn_hoist_loop;
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
    WN* wn_insert = (bnd_type != HMB_NONE && wn_guard != NULL) 
      ? wn_guard : wn_loop; 
    LWN_Insert_Block_Before(LWN_Get_Parent(wn_insert), wn_insert, wn_stid);
    HMB_Copy_Array_Deps_Exp(wn_exp, wn_exp_copy, inside_loop, loop_ls); 
    WN* wn_ldid_copy = LWN_CreateLdid(preg_l_opcode, wn_stid);
    WN* wn_ldid = Replace_Wnexp_With_Exp_Copy(wn_exp, wn_ldid_copy, du);
    du->Add_Def_Use(wn_stid, wn_ldid);
    if (bnd_type != HMB_NONE && wn_guard != NULL) {
      WN* wn_guard_exp = bnd_type == HMB_LEFT 
	? WN_kid0(WN_if_test(wn_guard)) : WN_kid1(WN_if_test(wn_guard)); 
      WN* wn_ldid = Replace_Wnexp_With_Exp_Copy(wn_guard_exp, wn_ldid_copy, du);
      du->Add_Def_Use(wn_stid, wn_ldid); 
    }
    LWN_Delete_Tree(wn_ldid_copy);
  } 
}

//-----------------------------------------------------------------------
// NAME: HMB_Has_Messy_Left_Bound 
// FUNCTION: Returns TRUE if WN_start(wn_loop) gives rise to a messy 
//   bound.  Returns FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL HMB_Has_Messy_Left_Bound(WN* wn_loop)
{
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 
  return (dli->Step->Const_Offset > 0) 
    ? Bound_Is_Too_Messy(dli->LB) : Bound_Is_Too_Messy(dli->UB); 
} 

//-----------------------------------------------------------------------
// NAME: HMB_Has_Messy_Right_Bound 
// FUNCTION: Returns TRUE if WN_end(wn_loop) gives rise to a messy 
//   bound.  Returns FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL HMB_Has_Messy_Right_Bound(WN* wn_loop)
{
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 
  return (dli->Step->Const_Offset > 0) 
    ? Bound_Is_Too_Messy(dli->UB) : Bound_Is_Too_Messy(dli->LB); 
} 

//-----------------------------------------------------------------------
// NAME: Bound_Exists 
// FUNCTION: Return the index of a bound identical to 'wn_bound' in 
//   'stk_bounds', if it exists, otherwise return NULL. 
//-----------------------------------------------------------------------

static INT Bound_Exists(STACK<WN*>* stk_bounds, 
			WN* wn_bound)
{
  for (INT i = 0; i < stk_bounds->Elements(); i++) 
    if (WN_Simp_Compare_Trees(WN_kid0(stk_bounds->Bottom_nth(i)), 
	wn_bound) == 0)
      return i; 
  return -1; 
} 

//-----------------------------------------------------------------------
// NAME: Unique_Definition
// FUNCTION: Return the unique definition of 'wn_ldid' according to DU 
//   information.  Assert if there is not a unique definition. 
//-----------------------------------------------------------------------

static WN* Unique_Definition(WN* wn_ldid)
{ 
  DU_MANAGER* du = Du_Mgr;
  DEF_LIST *def_list = du->Ud_Get_Def(wn_ldid); 
  DEF_LIST_ITER iter(def_list);	   
  INT count = 0;  
  WN* wn_stid = NULL; 
  const DU_NODE* node = NULL;
  for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
    wn_stid = node->Wn();
    count++; 
  } 
  FmtAssert(count == 1, ("Unique_Definition: Def not Unique")); 
  return wn_stid; 
} 

//-----------------------------------------------------------------------
// NAME: HMB_Replace_Messy_Bounds_Loop 
// FUNCTION: Replace all of the messy bounds of the loop 'wn_loop' with 
//   LDIDs and define those LDIDs outside the 'wn_hoist_loop' with STIDs.  
//-----------------------------------------------------------------------

static void HMB_Replace_Messy_Bounds_Loop(WN* wn_loop, 
					  WN* wn_hoist_loop, 
					  WN* wn_guard, 
					  STACK<WN*>* stk_bounds, 
					  LS_IN_LOOP* loop_ls) 
{
  FmtAssert(WN_opcode(wn_loop) == OPC_DO_LOOP, 
    ("HMB_Replace_Messy_Bounds_Loop: First arg must be do loop")); 
  FmtAssert(WN_opcode(wn_hoist_loop) == OPC_DO_LOOP, 
    ("HMB_Replace_Messy_Bounds_Loop: Second arg must be do loop")); 
  FmtAssert(Do_Loop_Depth(wn_loop) >= Do_Loop_Depth(wn_hoist_loop), 
    ("HMB_Replace_Messy_Bounds_Loop: Loop must be inside hoist loop")); 
  char buffer[MAX_NAME_SIZE];
  DOLOOP_STACK stack(&LNO_local_pool); 
  if (HMB_Has_Messy_Left_Bound(wn_loop)) {
    INT stk_number = Bound_Exists(stk_bounds, WN_kid0(WN_start(wn_loop))); 
    if (stk_number >= 0) {
      sprintf(buffer, "_ab%d", stk_number);
      HMB_Replace_Messy_Bounds(WN_kid0(WN_start(wn_loop)), wn_hoist_loop, 
	HMB_RIGHT, wn_guard, loop_ls, buffer, 
	stk_bounds->Bottom_nth(stk_number));
    } else {   
      sprintf(buffer, "_ab%d", preg_counter++);
      HMB_Replace_Messy_Bounds(WN_kid0(WN_start(wn_loop)), wn_hoist_loop, 
	HMB_RIGHT, wn_guard, loop_ls, buffer, NULL);
      stk_bounds->Push(Unique_Definition(WN_kid0(WN_start(wn_loop)))); 
    } 
    Build_Doloop_Stack(LWN_Get_Parent(wn_loop), &stack);
    LNO_Build_Do_Access(wn_loop, &stack); 
    stack.Clear(); 
  }
  if (HMB_Has_Messy_Right_Bound(wn_loop)) {
    INT stk_number = Bound_Exists(stk_bounds, UBexp(WN_end(wn_loop)));  
    if (stk_number >= 0) {
      sprintf(buffer, "_ab%d", stk_number);
      HMB_Replace_Messy_Bounds(UBexp(WN_end(wn_loop)), wn_hoist_loop, 
	HMB_LEFT, wn_guard, loop_ls, buffer, 
	stk_bounds->Bottom_nth(stk_number));
    } else {   
      sprintf(buffer, "_ab%d", preg_counter++);
      HMB_Replace_Messy_Bounds(UBexp(WN_end(wn_loop)), wn_hoist_loop, 
	HMB_LEFT, wn_guard, loop_ls, buffer, NULL);
      stk_bounds->Push(Unique_Definition(WN_kid1(WN_end(wn_loop)))); 
    } 
    Build_Doloop_Stack(LWN_Get_Parent(wn_loop), &stack);
    LNO_Build_Do_Access(wn_loop, &stack); 
    stack.Clear(); 
  }
}

//-----------------------------------------------------------------------
// NAME: Code_Expansion_Limit_Loop
// FUNCTION: For the innermost SNL of loops with outermost loop 'wn_outer' 
//   determine the outermost loop for which we can perform messy bounds 
//   hoisting without exceeding the allowed code expansion limit. 
//-----------------------------------------------------------------------

static WN* Code_Expansion_Limit_Loop(WN* wn_outer) 
				     
{
  INT nloops = SNL_Loop_Count(wn_outer); 
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops); 
  INT outer_depth = Do_Loop_Depth(wn_outer); 
  INT inner_depth = Do_Loop_Depth(wn_inner); 
  INT* node_counts = CXX_NEW_ARRAY(INT, inner_depth + 1, &LNO_local_pool);   
  WN* wn_loop = wn_outer; 
  INT i;
  for (i = outer_depth; i <= inner_depth; i++) {
    node_counts[i] = 1;
    node_counts[i] += Node_Count(WN_index(wn_loop)); 
    node_counts[i] += Node_Count(WN_start(wn_loop)); 
    node_counts[i] += Node_Count(WN_end(wn_loop)); 
    node_counts[i] += Node_Count(WN_step(wn_loop)); 
    node_counts[i]++; 
    WN* wn_first = WN_first(WN_do_body(wn_loop));
    for (WN* wn = wn_first; wn != NULL; wn = WN_next(wn)) {
      if (WN_opcode(wn) == OPC_DO_LOOP)
	continue; 
      node_counts[i] += Node_Count(wn); 
    } 
    wn_loop = Find_Next_Innermost_Do(wn_loop); 
  } 
  WN* wn_result = NULL; 
  wn_loop = wn_inner; 
  for (i = inner_depth; i >= outer_depth; i--) { 
    INT old_count = 0;
    INT new_count = 0;  
    INT factor = inner_depth - i + 1; 
    old_count += node_counts[i]; 
    new_count += factor * node_counts[i]; 
    FmtAssert(new_count >= old_count, 
      ("Code_Expansion_Limit_Loop: Code Expansion must be >= 1"));
    if (new_count > HMB_REL_CODE_EXPANSION * old_count
	&& new_count > HMB_ABS_CODE_EXPANSION)
      break;
    wn_result = wn_loop;   
    if (wn_result == wn_outer)
      break; 
    wn_loop = Enclosing_Do_Loop(LWN_Get_Parent(wn_loop)); 
  } 
  return wn_result; 
}

//-----------------------------------------------------------------------
// NAME: HMB_Maximum_Point 
// FUNCTION: Returns the loop within the singly nested loop nest 'wn_snl' 
//   which has the following properties: 
//     (1) All loops enclosed by this loop and the loop itself have their 
//         bounds in standard form, as determined by Upper_Bound_Standardize()
//     (2) All loops have constant step. (This is ensured by preopt, but
//         we are checking again, just to be sure.) 
//     (3) All array elements in the bounds of loops enclosed by this loop 
//         are invariant with respect to this loop. 
//   Returns NULL if none of the loops in the singly nested loop nest 
//   'wn_snl' meets both criteria above. 
//-----------------------------------------------------------------------

static WN* HMB_Maximum_Point(WN* wn_snl)
{
  WN* wn_last = NULL; 

  // Find innermost loop
  WN* wnn = NULL;  
  WN* wn = 0;
  for (wn = wn_snl; wn != NULL; wnn = wn, wn = Find_Next_Innermost_Do(wn));
  WN* wn_inner = wnn;

  // Ensure that no loop has gotos 
  for (wn = wn_inner; wn != LWN_Get_Parent(wn_snl); wn = LWN_Get_Parent(wn)) {
    if (WN_opcode(wn) != OPC_DO_LOOP)
      continue;
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn);
    if (dli->Has_Gotos)
      break;
    wn_last = wn;
  }
  if (wn_last == NULL)
    return NULL;

  // Ensure that Upper_Bound_Standardize() is TRUE for all loops 
  WN* wn_max = wn_last;
  wn_last = NULL;
  WN* wn_final = LWN_Get_Parent(wn_max);    
  for (wn = wn_inner; wn != wn_final; wn = LWN_Get_Parent(wn)) {
    if (WN_opcode(wn) != OPC_DO_LOOP) 
      continue; 
    if (!Upper_Bound_Standardize(WN_end(wn), TRUE))
      break;
    wn_last = wn; 
  } 
  if (wn_last == NULL) 
    return NULL;  
 
  // Ensure all loops have constant step 
  wn_max = wn_last; 
  wn_last = NULL; 
  wn_final = LWN_Get_Parent(wn_max); 
  for (wn = wn_inner; wn != wn_final; wn = LWN_Get_Parent(wn)) {
    if (WN_opcode(wn) != OPC_DO_LOOP)
      continue; 
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn); 
    if (!dli->Step->Is_Const())
      break;
    wn_last = wn; 
  }
  if (wn_last == NULL) 
    return NULL;  

  // Ensure that all arrays in bounds are nest invariant and that all 
  // bounds are promotable through non-invariant loops
  wn_max = wn_last;  
  wn_last = NULL; 
  DOLOOP_STACK stk_loop(&LNO_local_pool); 
  Build_Doloop_Stack(wn_inner, &stk_loop); 
  for (WN* wn_try = wn_inner; wn_try != LWN_Get_Parent(wn_max); 
      wn_try = LWN_Get_Parent(wn_try)) {
    if (WN_opcode(wn_try) != OPC_DO_LOOP) 
      continue;
    for (wn = wn_inner; wn != wn_try; wn = LWN_Get_Parent(wn)) {
      if (WN_opcode(wn) != OPC_DO_LOOP)
 	continue; 
      if (!HMB_Invariant_In_Loop(WN_kid0(WN_start(wn)), wn_try))
        break;
      if (!HMB_Invariant_In_Loop(UBexp(WN_end(wn)), wn_try))
        break;
    }
    if (wn != wn_try) 
      return wn_last; 
    wn_last = wn_try; 
  }

  return wn_last;
}

//-----------------------------------------------------------------------
// NAME: Tree_Has_Regions 
// FUNCTION: Returns TRUE if the tree rooted at 'wn_tree' has a REGION 
//   node, FALSE otherwise.  
//-----------------------------------------------------------------------

extern BOOL Tree_Has_Regions(WN* wn_tree)
{
  if (WN_opcode(wn_tree) == OPC_REGION)
    return TRUE; 
  if (WN_opcode(wn_tree) == OPC_BLOCK) {
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      if (Tree_Has_Regions(wn))
	return TRUE; 
  } else {
    for (INT i = 0; i < WN_kid_count(wn_tree); i++) 
      if (Tree_Has_Regions(WN_kid(wn_tree, i)))
	return TRUE;  
  } 
  return FALSE; 
}

//-----------------------------------------------------------------------
// NAME: Guard_Test_Redundant 
// FUNCTION: Returns TRUE if the guard test for the loop 'wn_loop' is 
//   provably redundant.  Returns FALSE if it is not or we can't tell.  
//-----------------------------------------------------------------------

static BOOL Guard_Test_Redundant(WN* wn_loop,
				 WN* wn_guards[], 
				 INT depth) 
{ 
  DU_MANAGER* du = Du_Mgr; 
  BOOL return_value = FALSE; 
  WN* wn_parent = LWN_Get_Parent(wn_loop); 
  COND_BOUNDS_INFO *info =
    CXX_NEW(COND_BOUNDS_INFO(&LNO_local_pool), &LNO_local_pool);
  info->Collect_Outer_Info(wn_parent);
  WN* wn_test = LWN_Copy_Tree(WN_end(wn_loop)); 
  LWN_Copy_Def_Use(WN_end(wn_loop), wn_test, du);
  Replace_Ldid_With_Exp_Copy(SYMBOL(WN_start(wn_loop)), wn_test, 
    WN_kid0(WN_start(wn_loop)), du); 
  WN* wn_if = LWN_CreateIf(wn_test, WN_CreateBlock(), WN_CreateBlock());
  LWN_Insert_Block_Before(LWN_Get_Parent(wn_loop), wn_loop, wn_if); 
  LWN_Extract_From_Block(wn_loop);
  LWN_Insert_Block_After(WN_then(wn_if), NULL, wn_loop); 
  IF_INFO *ii = CXX_NEW(IF_INFO(&LNO_default_pool, TRUE, 
    Tree_Has_Regions(wn_loop)), &LNO_default_pool);
  WN_MAP_Set(LNO_Info_Map, wn_if, (void *) ii);
  return_value = Redundant_Condition(info, wn_test, wn_if); 
  Forward_Substitute_Ldids(wn_test, du); 
  wn_guards[depth] = LWN_Copy_Tree(wn_test); 
  LWN_Copy_Def_Use(wn_test, wn_guards[depth], du); 
  LWN_Extract_From_Block(wn_loop); 
  LWN_Insert_Block_Before(LWN_Get_Parent(wn_if), wn_if, wn_loop);
  LWN_Extract_From_Block(wn_if); 
  LWN_Delete_Tree(wn_if); 
  if (return_value == FALSE) { 
    for (INT i = 0; i < depth; i++) { 
      if (wn_guards[i] != NULL 
	  &&  WN_Simp_Compare_Trees(wn_guards[i], wn_guards[depth]) == 0) {
	return_value = TRUE; 
	break; 
      }
    } 
  } 
  return return_value; 
} 

//-----------------------------------------------------------------------
// NAME: HMB_Add_Guard_Condition 
// FUNCTION: For the loop 'wn_loop' in the SNL 'wn_snl' for which we are
//   creating the guard test 'wn_if', compute a guard condition corres-
//   ponding to 'wn_loop' and .CAND. it with any existing conditions 
//   already present in 'WN_if_test(wn_if)'.  
//-----------------------------------------------------------------------

static void HMB_Add_Guard_Condition(WN* wn_loop,
				    WN* wn_snl, 
				    WN* wn_if,
				    LS_IN_LOOP* loop_ls) 
{ 
  DU_MANAGER* du = Du_Mgr; 
  OPCODE op_cand = OPCODE_make_op(OPR_CAND, Boolean_type, MTYPE_V);
  WN* wn_upper_bound = LWN_Copy_Tree(UBexp(WN_end(wn_loop)));
  LWN_Copy_Def_Use(UBexp(WN_end(wn_loop)), wn_upper_bound, du);
  WN* wn_lower_bound = LWN_Copy_Tree(WN_kid0(WN_start(wn_loop)));
  LWN_Copy_Def_Use(WN_kid0(WN_start(wn_loop)), wn_lower_bound, du);
  BOOL saved_fold_enable = WN_Simplifier_Enable(FALSE);
  OPCODE op_cond = OPCODE_make_op(OPR_GE, Boolean_type,
    WN_rtype(wn_upper_bound));
  WN* wn_cond = LWN_CreateExp2(op_cond, wn_upper_bound, wn_lower_bound);
  if (WN_if_test(wn_if) == NULL)
    WN_if_test(wn_if) = wn_cond;
  else
    WN_if_test(wn_if) = LWN_CreateExp2(op_cand, WN_if_test(wn_if), wn_cond);      LWN_Set_Parent(WN_if_test(wn_if), wn_if);
  BOOL inside_loop = Do_Loop_Depth(wn_snl) > 0;
  HMB_Copy_Array_Deps_Exp(UBexp(WN_end(wn_loop)), wn_upper_bound,
    inside_loop, loop_ls);
  HMB_Copy_Array_Deps_Exp(WN_kid0(WN_start(wn_loop)), wn_lower_bound,
    inside_loop, loop_ls);
  WN_Simplifier_Enable(saved_fold_enable);
}

//-----------------------------------------------------------------------
// NAME: HMB_Simple_Guard_Test 
// FUNCTION: Returns an IF node with a guard test for the loop 'wn_loop' 
//   in the SNL 'wn_snl'.  The guard test is inserted into the program 
//   tree around the loop 'wn_snl'.
//-----------------------------------------------------------------------

static WN* HMB_Simple_Guard_Test(WN* wn_loop, 
				 WN* wn_snl, 
				 LS_IN_LOOP* loop_ls)
{
  WN* wn_if = LWN_CreateIf(NULL, WN_CreateBlock(), WN_CreateBlock());
  WN_Set_Linenum(wn_if, WN_Get_Linenum(wn_snl));
  LWN_Insert_Block_After(LWN_Get_Parent(wn_snl), wn_snl, wn_if);
  LWN_Extract_From_Block(wn_snl);
  LWN_Insert_Block_After(WN_then(wn_if), NULL, wn_snl);
  HMB_Add_Guard_Condition(wn_loop, wn_snl, wn_if, loop_ls);
  IF_INFO *ii = CXX_NEW(IF_INFO(&LNO_default_pool, TRUE, 
    Tree_Has_Regions(wn_snl)), &LNO_default_pool);
  WN_MAP_Set(LNO_Info_Map, wn_if, (void *) ii);
  DOLOOP_STACK stack(&LNO_local_pool); 
  Build_Doloop_Stack(wn_if, &stack); 
  LNO_Build_If_Access(wn_if, &stack);
  return wn_if;  
}

//-----------------------------------------------------------------------
// NAME: HMB_Compound_Guard_Test 
// FUNCTION: Returns an IF node with a guard test for the SNL 'wn_snl'. 
//   The 'guard_mask' has a bit set for each loop in the SNL for which 
//   we must generate a guard.  The guard test is inserted into the program
//   tree around the loop 'wn_snl'. 
//-----------------------------------------------------------------------

static WN* HMB_Compound_Guard_Test(WN* wn_snl,
			           INT guard_mask,
			           LS_IN_LOOP* loop_ls) 
{
  if (guard_mask == 0)
    return NULL; 
  INT nloops = SNL_Loop_Count(wn_snl); 
  INT outer_depth = Do_Loop_Depth(wn_snl); 
  INT inner_depth = outer_depth + nloops - 1; 
  INT test_guard_mask = guard_mask; 
  WN* wn_if = LWN_CreateIf(NULL, WN_CreateBlock(), WN_CreateBlock());
  WN_Set_Linenum(wn_if, WN_Get_Linenum(wn_snl));
  LWN_Insert_Block_After(LWN_Get_Parent(wn_snl), wn_snl, wn_if); 
  LWN_Extract_From_Block(wn_snl);
  LWN_Insert_Block_After(WN_then(wn_if), NULL, wn_snl);  
  OPCODE op_cand = OPCODE_make_op(OPR_CAND, Boolean_type, MTYPE_V);
  WN* wn_loop = wn_snl; 
  for (INT i = outer_depth; i <= inner_depth; i++) { 
    if (guard_mask & (1 << i)) 
      HMB_Add_Guard_Condition(wn_loop, wn_snl, wn_if, loop_ls);
    wn_loop = Find_Next_Innermost_Do(wn_loop); 
  } 
  IF_INFO *ii = CXX_NEW(IF_INFO(&LNO_default_pool, TRUE, 
    Tree_Has_Regions(wn_snl)), &LNO_default_pool);
  WN_MAP_Set(LNO_Info_Map, wn_if, (void *) ii);
  DOLOOP_STACK stack(&LNO_local_pool); 
  Build_Doloop_Stack(wn_if, &stack); 
  LNO_Build_If_Access(wn_if, &stack);
  return wn_if; 
}

//-----------------------------------------------------------------------
// NAME: Has_Code_At_Depth
// FUNCTION: Returns TRUE if there is sandwiched code in the SNL 'wn_snl' 
//   at the given 'depth'.  Returns FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Has_Code_At_Depth(WN* wn_snl,
			      INT nloops, 
			      INT depth)
{
  INT outer_depth = Do_Loop_Depth(wn_snl); 
  INT inner_depth = outer_depth + nloops - 1; 
  FmtAssert(depth >= outer_depth && depth <= inner_depth, 
    ("Has_Code_At_Depth: Illegal depth")); 
  if (depth == inner_depth) 
    return TRUE; 
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_snl, depth - outer_depth + 2); 
  WN* wn = 0;
  for (wn = WN_prev(wn_inner); wn != NULL; wn = WN_prev(wn)) 
    if (!OPCODE_is_not_executable(WN_opcode(wn)))
      return TRUE; 
  for (wn = WN_next(wn_inner); wn != NULL; wn = WN_next(wn)) 
    if (!OPCODE_is_not_executable(WN_opcode(wn)))
      return TRUE; 
  return FALSE; 
} 

//-----------------------------------------------------------------------
// NAME: Contains_Index_Variable
// FUNCTION: Returns TRUE if 'wn_tree' contains an index variable, 
//   FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Contains_Index_Variable(WN* wn_tree)
{
  if (WN_operator(wn_tree) == OPR_LDID) {
    WN* wn = 0;
    for (wn = wn_tree; wn != NULL; wn = LWN_Get_Parent(wn))  
      if (WN_opcode(wn) == OPC_DO_LOOP 
	  && SYMBOL(WN_index(wn)) == SYMBOL(wn_tree))
	break; 
    if (wn != NULL)
      return TRUE; 
  } 
  for (INT i = 0; i < WN_kid_count(wn_tree); i++) 
    if (Contains_Index_Variable(WN_kid(wn_tree, i)))
      return TRUE; 
  return FALSE; 
} 

//-----------------------------------------------------------------------
// NAME: Contains_Vertex
// FUNCTION: Returns TRUE if the expression 'wn_exp' has a dependence 
//   graph vertex (or should).  Returns FALSE otherwise.  
//-----------------------------------------------------------------------

static BOOL Contains_Vertex(WN* wn_exp)
{ 
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  LWN_ITER* itr = LWN_WALK_TreeIter(wn_exp); 
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) 
    if (OPCODE_is_load(WN_opcode(itr->wn)) 
        && (WN_operator(itr->wn) == OPR_ILOAD 
        || dg->Get_Vertex(itr->wn)))
      return TRUE; 
  return FALSE; 
} 

//-----------------------------------------------------------------------
// NAME: Is_Messy_Expression
// FUNCTION: Returns TRUE if 'wn_tree' is a candidate expression for 
//   hoisting.  If 'need_safe' is TRUE, it must be statically safe for 
//   hoisting or it is not a candidate. 
//-----------------------------------------------------------------------

static BOOL Is_Messy_Expression(WN* wn_tree, 
				BOOL need_safe) 
{ 
  OPERATOR opr = WN_operator(wn_tree); 
  return WN_kid_count(wn_tree) > 0 && (opr != OPR_ADD && opr != OPR_SUB 
      && opr != OPR_MPY && opr != OPR_NEG) 
      && (!need_safe || Statically_Safe_Exp(wn_tree))
      && !Contains_Index_Variable(wn_tree) && !Contains_Vertex(wn_tree);
} 

//-----------------------------------------------------------------------
// NAME: HMB_Push_Messy_Expressions
// FUNCTION: Scan 'wn_tree' for expressions which are messy, do not contain 
//   index variables or dependence graph vertices, and push them on the 
//   stack 'stk_messy'.  If 'need_safe' only push those which are safe 
//   to speculate. 
//-----------------------------------------------------------------------

static void HMB_Push_Messy_Expressions(WN* wn_tree, 
				       BOOL need_safe, 
				       STACK<WN*>* stk_messy)
{
  DU_MANAGER* du = Du_Mgr; 
  INT i;
  for (i = 0; i < WN_kid_count(wn_tree); i++) 
    HMB_Push_Messy_Expressions(WN_kid(wn_tree, i), need_safe, stk_messy); 

  if (Is_Messy_Expression(wn_tree, need_safe)
      && Hoistable_Statement(wn_tree, du) < Loop_Depth(wn_tree)) {
    INT i;
    for (i = 0; i < stk_messy->Elements(); i++)
      if (stk_messy->Bottom_nth(i) == wn_tree)
        break;
    if (i == stk_messy->Elements())
      stk_messy->Push(wn_tree); 
  }
} 

//-----------------------------------------------------------------------
// NAME: HMB_Find_Messy_Subscripts
// FUNCTION: For the SNL 'wn_loop', find the messy expressions which are
//   candidates for hoisting in 'wn_loop' and push them on 'stk_messy'.
//   If 'need_safe', only include those which are safe to speculate. 
//-----------------------------------------------------------------------

static void HMB_Find_Messy_Subscripts(WN* wn_loop, 
				      BOOL need_safe, 
				      STACK<WN*>* stk_messy)
{
  DU_MANAGER* du = Du_Mgr; 
  STACK<WN*>* stk_subs = CXX_NEW(STACK<WN*>(&LNO_local_pool), &LNO_local_pool);
  LWN_ITER* itr = LWN_WALK_TreeIter(wn_loop);
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn_array = itr->wn;
    if (WN_operator(wn_array) == OPR_ARRAY) {
      ACCESS_ARRAY* aa = (ACCESS_ARRAY*) WN_MAP_Get(LNO_Info_Map, wn_array);
      FmtAssert(aa != NULL,
	("HMB_Find_Messy_Subscripts: Missing access array on OPR_ARRAY node")); 
      if (Bound_Is_Too_Messy(aa))
	HMB_Push_Messy_Expressions(wn_array, need_safe, stk_messy); 
    }
  }
}

//-----------------------------------------------------------------------
// NAME: HMB_Hoist_Messy_Subscripts
// FUNCTION: For the SNL 'wn_loop', hoist each of the expressions in the 
//   list 'stk_messy'. 
//-----------------------------------------------------------------------

static void HMB_Hoist_Messy_Subscripts(WN* wn_loop, 
				       STACK<WN*>* stk_messy, 
				       STACK<WN*>* stk_bounds, 
				       LS_IN_LOOP* loop_ls) 
{
  DU_MANAGER* du = Du_Mgr; 
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  char buffer[MAX_NAME_SIZE]; 
  DYN_ARRAY<WN*> new_uses(&LNO_local_pool); 
  DYN_ARRAY<WN*> parent_uses(&LNO_local_pool); 
  for (INT j = 0; j < stk_messy->Elements(); j++) {
    WN* wn_messy_kid = stk_messy->Bottom_nth(j); 
    WN* wn_messy = LWN_Get_Parent(wn_messy_kid); 
    // Don't copy any expression without first updating the dependences 
    // for any new expressions inside it. 
    for (INT k = new_uses.Elements() - 1; k >= 0; k--) { 
      WN* wn_use = new_uses[k];
      for (WN* wn = wn_use; wn != NULL; wn = LWN_Get_Parent(wn)) {
	if (wn == wn_messy) {
	  parent_uses.AddElement(wn_use);
	  for (INT m = k + 1; m < new_uses.Elements(); m++)
	    new_uses[m-1] = new_uses[m]; 
	  new_uses.Decidx(); 
	}
      }
    }
    if (parent_uses.Elements() > 0) {
      MIR_Update_Dependences(wn_loop, &parent_uses); 
      parent_uses.Resetidx();
    } 
    INT hoist_level = Hoistable_Statement(wn_messy_kid, du);
    WN* wn = 0;
    for (wn = wn_messy_kid; wn != NULL; wn = LWN_Get_Parent(wn))
      if (WN_opcode(wn) == OPC_DO_LOOP 
	  && Do_Loop_Depth(wn) == hoist_level + 1)
	break; 
    WN* wn_hoist_loop = wn; 
    FmtAssert(wn_hoist_loop != NULL, 
      ("HMB_Hoist_Messy_Subscripts: Could not find hoist loop")); 
    INT stk_number = Bound_Exists(stk_bounds, wn_messy_kid); 
    if (stk_number >= 0) { 
      sprintf(buffer, "_ab%d", stk_number);
      HMB_Replace_Messy_Bounds(wn_messy_kid, wn_hoist_loop, HMB_NONE, 
	NULL, loop_ls, buffer, stk_bounds->Bottom_nth(stk_number));
    } else { 
      sprintf(buffer, "_ab%d", preg_counter++);
      WN* wn_parent = LWN_Get_Parent(wn_messy_kid); 
      INT i;
      for (i = 0; i < WN_kid_count(wn_parent); i++) 
	if (WN_kid(wn_parent, i) == wn_messy_kid)
	  break; 
      INT kid = i; 
      HMB_Replace_Messy_Bounds(wn_messy_kid, wn_hoist_loop, HMB_NONE, 
	NULL, loop_ls, buffer, NULL); 
      stk_bounds->Push(Unique_Definition(WN_kid(wn_parent, kid)));
    } 
    for (wn = wn_messy; wn != NULL; wn = LWN_Get_Parent(wn)) 
      if (WN_operator(wn) == OPR_ARRAY)
        break;
    WN* wn_rebuild = wn; 
    DOLOOP_STACK do_stack(&LNO_local_pool);
    Build_Doloop_Stack(LWN_Get_Parent(wn_rebuild), &do_stack);
    LNO_Build_Access(wn_rebuild, &do_stack, &LNO_default_pool);
    INT i;
    for (i = 0; i < new_uses.Elements(); i++)
      if (new_uses[i] == wn_messy)
	break; 
    if (i == new_uses.Elements())
      new_uses.AddElement(wn_messy); 
  }
  MIR_Update_Dependences(wn_loop, &new_uses); 
}

//-----------------------------------------------------------------------
// NAME: HMB_Find_and_Hoist_Messy_Subscripts
// FUNCTION: For the SNL 'wn_loop' find and hoist messy expressions in 
//   array subscripts that are hoistable.  If 'need_safe', only do those
//   which are safe to speculate.  
//-----------------------------------------------------------------------

static void HMB_Find_and_Hoist_Messy_Subscripts(WN* wn_loop,
				                STACK<WN*>* stk_bounds, 
				                BOOL need_safe, 
				                LS_IN_LOOP* loop_ls) 
{ 
  STACK<WN*>* stk_messy = CXX_NEW(STACK<WN*>(&LNO_local_pool), 
    &LNO_local_pool);
  while (TRUE) { 
    stk_messy->Clear(); 
    HMB_Find_Messy_Subscripts(wn_loop, need_safe, stk_messy);
    if (stk_messy->Elements() == 0) 
      return;  
    HMB_Hoist_Messy_Subscripts(wn_loop, stk_messy, stk_bounds, loop_ls);
  }
}

//-----------------------------------------------------------------------
// NAME: HMB_Similar_Group
// FUNCTION: Remove a group of expressions containing divides each of 
//   which has the same value from 'stk_messy' and place them in 
//   'stk_group'. 
//-----------------------------------------------------------------------

static void HMB_Similar_Group(STACK<WN*>* stk_messy, 
			      STACK<WN*>* stk_group)
{
  STACK<WN*> local_stack(&LNO_local_pool); 
  WN* wn_pattern = stk_messy->Pop();  
  stk_group->Push(wn_pattern); 
  while (stk_messy->Elements() > 0) { 
    WN* wn_trial = stk_messy->Pop(); 
    if (WN_Simp_Compare_Trees(wn_pattern, wn_trial) == 0)
      stk_group->Push(wn_trial); 
    else 
      local_stack.Push(wn_trial); 
  } 
  while (local_stack.Elements() > 0)  
    stk_messy->Push(local_stack.Pop()); 
} 

//-----------------------------------------------------------------------
// NAME: HMB_Hoist_Expressions
// FUNCTION: Hoist all of the expressions on 'stk_messy'. 
//-----------------------------------------------------------------------

static void HMB_Hoist_Expressions(WN* wn_loop, 
				  STACK<WN*>* stk_messy)
{
  DU_MANAGER* du = Du_Mgr; 
  char buffer[MAX_NAME_SIZE];
  STACK<WN*> stk_group(&LNO_local_pool); 
  while (stk_messy->Elements() > 0) { 
    sprintf(buffer, "_mb%d", preg_counter++);
    HMB_Similar_Group(stk_messy, &stk_group); 
    WN* wn_pattern = stk_group.Bottom_nth(0); 
    TYPE_ID type = WN_rtype(wn_pattern); 
    OPCODE preg_l_opcode = OPCODE_make_op(OPR_LDID, Promote_Type(type), type);
    OPCODE preg_s_opcode = OPCODE_make_op(OPR_STID, MTYPE_V, type);
#ifdef _NEW_SYMTAB
    WN_OFFSET preg_num = Create_Preg(type, buffer);
#else
    WN_OFFSET preg_num = Create_Preg(type, buffer, NULL);
#endif
    ST* preg_st = MTYPE_To_PREG(type);
    WN* wn_hoist_place = Hoistable_Place(wn_pattern, du); 
    WN* wn_parent = LWN_Get_Parent(wn_pattern);
    WN* wn_stid = LWN_CreateStid(preg_s_opcode, preg_num, preg_st,
                                 Be_Type_Tbl(type), wn_pattern);
    WN* wn_ldid = LWN_CreateLdid(preg_l_opcode, wn_stid);
    INT j;
    for (j = 0; j < WN_kid_count(wn_parent); j++)
      if (WN_kid(wn_parent, j) == wn_pattern)
	break;
    FmtAssert(j < WN_kid_count(wn_parent), 
      ("Could not find kid for parent."));
    INT kid = j;
    WN_kid(wn_parent, kid) = wn_ldid;
    LWN_Set_Parent(wn_ldid, wn_parent);
    du->Add_Def_Use(wn_stid, wn_ldid);
    LWN_Insert_Block_Before(LWN_Get_Parent(wn_hoist_place), 
      wn_hoist_place, wn_stid); 
    INT i;
    for (i = 1; i < stk_group.Elements(); i++) { 
      WN* wn_div_exp = stk_group.Bottom_nth(i); 
      WN* wn_parent = LWN_Get_Parent(wn_div_exp); 
      WN* wn_ldid = LWN_CreateLdid(preg_l_opcode, wn_stid);
      INT j;
      for (j = 0; j < WN_kid_count(wn_parent); j++)
	if (WN_kid(wn_parent, j) == wn_div_exp)
	  break;
      FmtAssert(j < WN_kid_count(wn_parent), 
	("Could not find kid for parent."));
      INT kid = j;
      WN_kid(wn_parent, kid) = wn_ldid;
      LWN_Set_Parent(wn_ldid, wn_parent);
      du->Add_Def_Use(wn_stid, wn_ldid);
      LWN_Delete_Tree(wn_div_exp); 
    } 
  } 
  DOLOOP_STACK rebuild_stack(&LNO_local_pool);
  Build_Doloop_Stack(LWN_Get_Parent(wn_loop), &rebuild_stack);
  LNO_Build_Access(wn_loop, &rebuild_stack, &LNO_default_pool);
}

//-----------------------------------------------------------------------
// NAME: HMB_Compound_Guard_And_Hoist
// FUNCTION: Given the 'version_count' new versions of the code 
//   'wn_version[i]' and 'guard_mask[i]' which tells us for each version 
//   of the code which loops in 'wn_version[i]' must be guarded, modify 
//   each 'wn_version[i]' to generate guards for each of the indicated 
//   loops, then hoist the messy bounds of in each version out of the 
//   loop nests and hoist subscripted array references out of the loops 
//   when possible.  
//-----------------------------------------------------------------------

static void HMB_Compound_Guard_And_Hoist(WN* wn_version[], 
					 INT guard_mask[], 
					 INT version_count, 
					 STACK<WN*>* stk_bounds, 
      					 LS_IN_LOOP* loop_ls,
					 BOOL no_messy_hoist) 
{
  // Create guard tests for each of the versions 
  WN* wn_nest = NULL;
  WN* wn_last_nest = NULL;
  INT i;
  for (i = 0; i < version_count; i++) { 
    WN* wn_guard = HMB_Compound_Guard_Test(wn_version[i], guard_mask[i], 
      loop_ls); 
    wn_nest = wn_guard != NULL ? wn_guard : wn_version[i];   
    if (wn_last_nest != NULL) { 
      LWN_Extract_From_Block(wn_nest); 
      LWN_Insert_Block_After(WN_else(wn_last_nest), NULL, wn_nest); 
    }
    wn_last_nest = wn_nest; 
  } 

  // Replace the messy and array subscripts in each version that needs 
  // them replaced. 
  for (i = 0; i < version_count; i++) {
    INT stack_count = stk_bounds->Elements(); 
    INT version_nloops = SNL_Loop_Count(wn_version[i]); 
    for (INT j = 1; j <= version_nloops; j++) { 
      WN* wn_loop = SNL_Get_Inner_Snl_Loop(wn_version[i], j); 
      HMB_Replace_Messy_Bounds_Loop(wn_loop, wn_version[i], NULL, 
	stk_bounds, loop_ls); 
      if (!no_messy_hoist)
        MIR_Hoist_Messy_Subscripts(wn_version[i]); 
    } 
    HMB_Find_and_Hoist_Messy_Subscripts(wn_version[i], stk_bounds, 
      TRUE, loop_ls);  
    while (stk_bounds->Elements() > stack_count) 
      (void) stk_bounds->Pop(); 
  } 
}

//-----------------------------------------------------------------------
// NAME: HAB_Copy_Array_Deps
// FUNCTION: Copy the array dependences for the node 'wn_orig' to those in
//   'wn_copy', updating them as appropriate because their locations in the
//   program unit tree are slightly different. The boolean 'inside_loop'
//   indicates if the nodes in the expression 'wn_copy' are inside a loop.
//-----------------------------------------------------------------------

static void HAB_Copy_Array_Deps(WN* wn_orig,
                                WN* wn_copy,
                                BOOL inside_loop,
                                LS_IN_LOOP* loop_ls)
{
  if (!inside_loop)
    return;
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  DOLOOP_STACK copy_stack(&LNO_local_pool);
  Build_Doloop_Stack(wn_copy, &copy_stack);
  LNO_Build_Access(WN_kid0(wn_copy), &copy_stack, &LNO_default_pool);
  VINDEX16 v = dg->Get_Vertex(wn_orig);
  if (v == 0)
    return;
  STACK<WN*> wn_stack(&LNO_local_pool);
  EINDEX16 e = 0;
  INT node_position = loop_ls->In(wn_orig);
  for (e = dg->Get_In_Edge(v); e != 0; e = dg->Get_Next_In_Edge(e)) {
    WN* wn_source = dg->Get_Wn(dg->Get_Source(e));
    INT i;
    for (i = 0; i < wn_stack.Elements(); i++)
      if (wn_stack.Bottom_nth(i) == wn_source)
        break;
    if (i == wn_stack.Elements())
      wn_stack.Push(wn_source);
  }
  for (e = dg->Get_Out_Edge(v); e != 0; e = dg->Get_Next_Out_Edge(e)) {
    WN* wn_sink = dg->Get_Wn(dg->Get_Sink(e));
    INT i;
    for (i = 0; i < wn_stack.Elements(); i++)
      if (wn_stack.Bottom_nth(i) == wn_sink)
        break;
    if (i == wn_stack.Elements())
      wn_stack.Push(wn_sink);
  }
  dg->Add_Vertex(wn_copy);
  DOLOOP_STACK other_stack(&LNO_local_pool);
  for (INT i = 0; i < wn_stack.Elements(); i++) {
    WN* wn_other = wn_stack.Bottom_nth(i);
    Build_Doloop_Stack(wn_other, &other_stack);
    if (!dg->Add_Edge(wn_copy, &copy_stack, wn_other, &other_stack,
      node_position < loop_ls->In(wn_other))) {
      LNO_Erase_Dg_From_Here_In(wn_other, dg);
    }
    other_stack.Clear();
  }
}

//-----------------------------------------------------------------------
// NAME: HAB_Copy_Array_Deps_Exp
// FUNCTION: Copy the array dependences from the nodes in the expression
//   rooted at 'wn_orig' to those in 'wn_copy', updating them as appro-
//   priate because their locations in the program unit tree are slightly
//   different. The boolean 'inside_loop' indicates if the nodes in the
//   expression 'wn_copy' are inside a loop.
//-----------------------------------------------------------------------

static void HAB_Copy_Array_Deps_Exp(WN* wn_orig,
                                    WN* wn_copy,
                                    BOOL inside_loop,
                                    LS_IN_LOOP* loop_ls)
{
  if (WN_operator(wn_orig) == OPR_ILOAD)
    HAB_Copy_Array_Deps(wn_orig, wn_copy, inside_loop, loop_ls);
  for (INT i = 0; i < WN_kid_count(wn_orig); i++) {
    WN* wn_orig_kid = WN_kid(wn_orig, i);
    WN* wn_copy_kid = WN_kid(wn_copy, i);
    HAB_Copy_Array_Deps_Exp(wn_orig_kid, wn_copy_kid, inside_loop, loop_ls);
  }
}

//-----------------------------------------------------------------------
// NAME: HMB_Simple_Guard_And_Hoist
// FUNCTION: Given the new version of the code 'wn_version' and 'guard_mask'
//   which tells us which loops in 'wn_version' must be guarded, modify 
//   'wn_version' to generate guards for each of the indicated loops,
//   then hoist the messy bounds out of 'wn_version' and hoist subscripted 
//   array references out of 'wn_version' when possible.  
//-----------------------------------------------------------------------

static void HMB_Simple_Guard_And_Hoist(WN* wn_version, 
				       INT guard_mask, 
				       STACK<WN*>* stk_bounds, 
				       LS_IN_LOOP* loop_ls, 
				       BOOL no_messy_hoist) 
{
  INT nloops = SNL_Loop_Count(wn_version); 
  INT outer_depth = Do_Loop_Depth(wn_version); 
  INT inner_depth = outer_depth + nloops - 1;
  WN** wn_guard = CXX_NEW_ARRAY(WN*, inner_depth + 1, &LNO_local_pool);
  INT i;
  for (i = 0; i < inner_depth + 1; i++)
    wn_guard[i] = NULL;
  WN* wn = 0;
  for (wn = wn_version; wn != NULL; wn = Find_Next_Innermost_Do(wn)) {
    INT current_depth = Do_Loop_Depth(wn);
    if (guard_mask & (1 << current_depth)) { 
      WN* wn_guard_test = HMB_Simple_Guard_Test(wn, wn_version, loop_ls);
      wn_guard[current_depth] = wn_guard_test;
    } 
  }
  for (wn = wn_version; wn != NULL; wn = Find_Next_Innermost_Do(wn)) {
    INT current_depth = Do_Loop_Depth(wn);
    HMB_Replace_Messy_Bounds_Loop(wn, wn_version, wn_guard[current_depth],
      stk_bounds, loop_ls);
    if (wn_guard[current_depth] != NULL)
      LWN_Simplify_Tree(WN_if_test(wn_guard[current_depth]));
  }
  HMB_Find_and_Hoist_Messy_Subscripts(wn_version, stk_bounds, TRUE, loop_ls); 
  if (!no_messy_hoist)
    MIR_Hoist_Messy_Subscripts(wn_version); 
}

//-----------------------------------------------------------------------
// NAME: HMB_Has_Messy_Subscript
// FUNCTION: For the SNL 'wn_loop' which has candidate hoistable refer-
//   ences on 'stk_messy', set the values of the arrays 'can_hoist[]' and 
//   'lowest_depth[]'.  The 'can_hoist[depth]' is the number of references
//   which can be hoisted at loop depth 'depth'.  The 'lowest_depth[depth]'
//   is the lowest depth to which some hoistable array reference at loop
//   depth 'depth' can be hoisted.  If 'initialize', we initialize the 
//   'can_hoist[]' and 'lowest_depth[]' arrays. 
//-----------------------------------------------------------------------

static void HMB_Has_Messy_Subscript(WN* wn_loop, 
				    STACK<WN*>* stk_messy, 
				    INT can_hoist[], 
				    INT lowest_depth[], 
				    BOOL initialize) 
{
  DU_MANAGER* du = Du_Mgr; 
  STACK<WN*> local_stack(&LNO_local_pool);
  if (initialize) { 
    INT count = SNL_Loop_Count(wn_loop);
    WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_loop, count);
    INT inner_depth = Do_Loop_Depth(wn_inner);
    for (INT i = 0; i <= inner_depth; i++) {
      can_hoist[i] = 0;
      lowest_depth[i] = i;
    }
  } 
  for (INT i = 0; i < stk_messy->Elements(); i++) { 
    WN* wn = stk_messy->Bottom_nth(i); 
    INT depth = Loop_Depth(wn); 
    WN* wn_hoist = Hoistable_Place(wn, du); 
    INT hoist_depth = Loop_Depth(wn_hoist);  
    if (hoist_depth < depth) { 
      local_stack.Push(wn); 
      can_hoist[depth]++; 
      if (hoist_depth < lowest_depth[depth])
	lowest_depth[depth] = hoist_depth; 
    } 
  } 
  stk_messy->Clear(); 
  while (local_stack.Elements() > 0)
    stk_messy->Push(local_stack.Pop()); 
} 

//-----------------------------------------------------------------------
// NAME: HMB_Hoist_Messy_Bounds 
// FUNCTION: Hoist messy bounds for the perfectly nested loop 'wn_snl'
//   adding guard tests where appropriate.    
//-----------------------------------------------------------------------

static void HMB_Hoist_Messy_Bounds(WN* wn_snl,
				   STACK<WN*>* stk_bounds, 
				   LS_IN_LOOP* loop_ls)
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  REDUCTION_MANAGER* rm = red_manager; 
  // Skip out if there are no messy bounds to process.
  WN* wn_inner = NULL; 
  WN* wn = 0;
  for (wn = wn_snl; wn != NULL; wn = Find_Next_Innermost_Do(wn))
    wn_inner = wn;
  INT outer_depth = Do_Loop_Depth(wn_snl); 
  INT inner_depth = Do_Loop_Depth(wn_inner);  
  INT nloops = inner_depth - outer_depth + 1;

  // Get basic information needed for hoisting analysis.  
  INT* has_code = CXX_NEW_ARRAY(INT, inner_depth + 1, &LNO_local_pool);
  INT* has_messy_bound = CXX_NEW_ARRAY(INT, inner_depth + 1, &LNO_local_pool);
  INT* guard_test_true = CXX_NEW_ARRAY(INT, inner_depth + 1, &LNO_local_pool);
  INT* can_hoist = CXX_NEW_ARRAY(INT, inner_depth + 1, &LNO_local_pool);  
  INT* lowest_depth = CXX_NEW_ARRAY(INT, inner_depth + 1, &LNO_local_pool);  
  WN** wn_guards = CXX_NEW_ARRAY(WN*, inner_depth + 1, &LNO_local_pool);
  INT i;
  for (i = 0; i <= inner_depth; i++) {
    has_code[i] = 0; 
    has_messy_bound[i] = 0; 
    guard_test_true[i] = 0; 
    wn_guards[i] = NULL; 
  } 
  for (i = outer_depth; i <= inner_depth; i++) 
    has_code[i] = Has_Code_At_Depth(wn_snl, nloops, i); 
  for (i = outer_depth; i <= inner_depth; i++) { 
    WN* wn = SNL_Get_Inner_Snl_Loop(wn_snl, i - outer_depth + 1);
    if (HMB_Has_Messy_Left_Bound(wn) || HMB_Has_Messy_Right_Bound(wn))
      has_messy_bound[i] = TRUE; 
    guard_test_true[i] = Guard_Test_Redundant(wn, wn_guards, i); 
  } 
  for (i = outer_depth; i <= inner_depth; i++) 
    LWN_Delete_Tree(wn_guards[i]); 

  STACK<WN*> stk_safe_messy(&LNO_local_pool); 
  HMB_Find_Messy_Subscripts(wn_snl, TRUE, &stk_safe_messy); 
  STACK<WN*> stk_unsafe_messy(&LNO_local_pool); 
  MIR_Has_Messy_Subscript(wn_snl, can_hoist, lowest_depth, TRUE); 

  // Give up on messy subscript hoisting if we must hoist across 
  // non-invariant guards 
  BOOL no_messy_hoist = FALSE; 
  DOLOOP_STACK stk_loop(&LNO_local_pool);
  Build_Doloop_Stack(wn_inner, &stk_loop); 
  for (i = outer_depth; !no_messy_hoist && i <= inner_depth; i++) 
    if (can_hoist[i])  
      for (INT j = lowest_depth[i]; !no_messy_hoist && j < inner_depth; j++)  
	for (INT k = j + 1; !no_messy_hoist && k <= inner_depth; k++) 
	  if (!SNL_Is_Invariant(&stk_loop, j, k)) 
	    no_messy_hoist = TRUE; 
  if (no_messy_hoist)  
    for (i = outer_depth; i <= inner_depth; i++)
      can_hoist[i] = 0; 

  // Give up if there is nothing to do 
  for (i = outer_depth; i <= inner_depth; i++) 
    if (has_messy_bound[i] || can_hoist[i])
      break; 
  if (i > inner_depth && stk_safe_messy.Elements() == 0)
    return; 

  // Indicate that SNL is being transformed.
  if (LNO_Verbose) { 
    fprintf(stdout, "HMB: Transforming SNL %s at 0x%p\n", 
      WB_Whirl_Symbol(wn_snl), wn_snl);
    fprintf(TFile, "HMB: Transforming SNL %s at 0x%p\n", 
      WB_Whirl_Symbol(wn_snl), wn_snl);
  } 

  // Determine the guards and the loops which will be placed in each version. 
  INT* guard_mask = CXX_NEW_ARRAY(INT, inner_depth + 1, &LNO_local_pool);
  INT* last_loop = CXX_NEW_ARRAY(INT, inner_depth + 1, &LNO_local_pool);
  for (i = 0; i <= inner_depth; i++) {
    last_loop[i] = 0; 
    guard_mask[i] = 0; 
  }  
  INT inside_depth = inner_depth; 
  INT version_count;
  for (version_count = 0; inside_depth >= outer_depth; version_count++) {
    last_loop[version_count] = inside_depth; 
    for (INT i = inside_depth; i >= outer_depth; i--) { 
      if (i > outer_depth && has_messy_bound[i]) { 
        for (INT j = outer_depth; j < i; j++)
	  if (!guard_test_true[j])
	    guard_mask[version_count] |= 1 << j;   
      } 
      if (can_hoist[i]) {
	for (INT j = lowest_depth[i]; j <= i; j++) 
	  if (!guard_test_true[j])
            guard_mask[version_count] |= 1 << j;
      } 
    }
    if (guard_mask[version_count] == 0) {
      version_count++;
      break; 
    } 
    for (inside_depth--; inside_depth >= outer_depth 
	&& !has_code[inside_depth]; inside_depth--); 
  } 

  // Create version_count copies of the SNL 
  WN* wn_array[2];         
  wn_array[0] = wn_snl; 
  WN* wn_insert = wn_snl;
  WN** wn_version = CXX_NEW_ARRAY(WN*, version_count, &LNO_local_pool);  
  for (i = 0; i < version_count; i++)  
    wn_version[i] = NULL; 
  for (i = 0; i < version_count; i++) {
    WN_MAP version_map = WN_MAP_Create(&LNO_local_pool);
    wn_version[i] = LWN_Copy_Tree(wn_snl, TRUE, LNO_Info_Map, TRUE, 
      version_map);
    BOOL all_internal = WN_Rename_Duplicate_Labels(wn_snl, wn_version[i],
                          Current_Func_Node, &LNO_local_pool);
    Is_True(all_internal, ("external labels renamed"));
    wn_array[1] = wn_version[i]; 
    Unrolled_DU_Update(wn_array, 2, Do_Loop_Depth(wn_snl) - 1, TRUE, FALSE);
    dg->Versioned_Dependences_Update(wn_snl, wn_version[i], 
      outer_depth, version_map); 
    WN_MAP_Delete(version_map);
    if (rm != NULL)
      rm->Unroll_Update(wn_array, 2); 
    LWN_Insert_Block_After(LWN_Get_Parent(wn_insert), wn_insert, 
      wn_version[i]);
    if (i > 0) { 
      WN* wn_new_inner = SNL_Get_Inner_Snl_Loop(wn_version[i], 
	last_loop[i] - outer_depth + 1);    
      DO_LOOP_INFO* dli_new_inner = Get_Do_Loop_Info(wn_new_inner); 
      dli_new_inner->Is_Inner = TRUE; 
      WN* wn_tree = SNL_Get_Inner_Snl_Loop(wn_version[i], 
        last_loop[i] - outer_depth + 2); 
      LWN_Extract_From_Block(wn_tree); 
      LWN_Delete_Tree(wn_tree); 
    } 
    wn_insert = wn_version[i]; 
  }

  if (version_count > 1 || Get_Trace(TP_LNOPT2, TT_HMB_FORCE_VERSIONS)) 
    HMB_Compound_Guard_And_Hoist(wn_version, guard_mask, version_count, 
      stk_bounds, loop_ls, no_messy_hoist);
  else 
    HMB_Simple_Guard_And_Hoist(wn_version[0], guard_mask[0], stk_bounds, 
      loop_ls, no_messy_hoist); 

  // Get rid of the original version 
  LWN_Extract_From_Block(wn_snl); 
  LWN_Delete_Tree(wn_snl); 
}

//-----------------------------------------------------------------------
// NAME: Forward_Substitute_SNL_Bounds
// FUNCTION: Forward substitute the bounds of all loops in the SNL 'wn_loop'
//-----------------------------------------------------------------------

static void Forward_Substitute_SNL_Bounds(WN* wn_loop)
{ 
  DU_MANAGER* du = Du_Mgr; 
  INT nloops = SNL_Loop_Count(wn_loop); 
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_loop, nloops); 
  for (WN* wn = wn_inner; wn != NULL; wn = LWN_Get_Parent(wn)) { 
    if (WN_opcode(wn) != OPC_DO_LOOP) 
      continue; 
    Forward_Substitute_Ldids(WN_kid0(WN_start(wn)), du);
    Forward_Substitute_Ldids(WN_end(wn), du); 
    if (wn == wn_loop)
      break; 
  } 
} 

//-----------------------------------------------------------------------
// NAME: SNL_Finalize_Index_Variables
// FUNCTION: Finalize the index variables in the SNL 'wn_loop' which are 
//   live on exit. 
//-----------------------------------------------------------------------

static void SNL_Finalize_Index_Variables(WN* wn_loop) 
{ 
  for (WN* wn = wn_loop; wn != NULL; wn = Find_Next_Innermost_Do(wn))  
    if (Index_Variable_Live_At_Exit(wn))
      Finalize_Index_Variable(wn, TRUE, TRUE); 
} 

//-----------------------------------------------------------------------
// NAME: Has_Statically_Safe_Messy_Bounds 
// FUNCTION: Returns TRUE if there is at least one messy bound in 'wn_loop'
//   and all of the messy bounds in 'wn_loop' are statically safe for 
//   hoisting. 
//-----------------------------------------------------------------------

static BOOL Has_Statically_Safe_Messy_Bounds(WN* wn_loop)
{ 
  BOOL found_messy_bound = FALSE; 
  if (HMB_Has_Messy_Left_Bound(wn_loop)) {
    found_messy_bound = TRUE; 
    if (!Statically_Safe_Exp(WN_kid0(WN_start(wn_loop))))
      return FALSE; 
  } 
  if (HMB_Has_Messy_Right_Bound(wn_loop)) {
    found_messy_bound = TRUE; 
    if (!Statically_Safe_Exp(UBexp(WN_end(wn_loop))))
      return FALSE; 
  } 
  return found_messy_bound;
} 

//-----------------------------------------------------------------------
// NAME: HMB_Hoist_Easy_Messy_Bounds
// FUNCTION: For the innermost SNL 'wn_loop' whose nodes are described 
//   by 'ls_loop', hoist out the messy bounds of those loops for which 
//   the hoisting is always statically safe.  
//-----------------------------------------------------------------------

static void HMB_Hoist_Easy_Messy_Bounds(WN* wn_loop, 
					STACK<WN*>* stk_bounds, 
					LS_IN_LOOP* loop_ls) 
{ 
  DU_MANAGER* du = Du_Mgr; 
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  INT nloops = SNL_Loop_Count(wn_loop); 
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_loop, nloops); 
  WN* wn_final = LWN_Get_Parent(wn_loop); 
  for (WN* wn = wn_inner; wn != wn_final; wn = LWN_Get_Parent(wn)) 
    if (WN_opcode(wn) == OPC_DO_LOOP
        && Has_Statically_Safe_Messy_Bounds(wn))
      HMB_Replace_Messy_Bounds_Loop(wn, wn_loop, NULL, stk_bounds, 
	loop_ls);
} 

//-----------------------------------------------------------------------
// NAME: HMB_Hoist_Easy_Messy_Subscripts
// FUNCTION: Hoist the messy subscripts out of 'wn_loop' if they are all 
//   statically safe. 
//-----------------------------------------------------------------------

static void HMB_Hoist_Easy_Messy_Subscripts(WN* wn_loop) 
{
  INT messy_count = 0;  
  LWN_ITER* itr = LWN_WALK_TreeIter(wn_loop);
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn_array = itr->wn;  
    WN* wn_messy = Messy_Subscript(wn_array); 
    if (wn_messy != NULL) { 
      messy_count++; 
      if (!Statically_Safe_Exp(wn_messy))
	return; 
    } 
  } 
  if (messy_count > 0) 
    MIR_Hoist_Messy_Subscripts(wn_loop); 
}

//-----------------------------------------------------------------------
// NAME: SNL_Hoist_Messy_Bounds
// FUNCTION: Hoist the array elements in the bounds of the singly nested 
//   loop nest 'wn_snl', adding guard tests where needed.  
//-----------------------------------------------------------------------

static void SNL_Hoist_Messy_Bounds(WN* wn_snl)
{
  DU_MANAGER* du = Du_Mgr; 
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  REDUCTION_MANAGER* rm = red_manager; 
  
  if (!LNO_Hoist_Messy_Bounds) 
    return;

  WN* wn_outer = wn_snl;  
  Forward_Substitute_SNL_Bounds(wn_snl);  
  for (WN* wn = wn_outer; wn != NULL; wn = LWN_Get_Parent(wn))
    if (WN_opcode(wn) == OPC_DO_LOOP)
      wn_outer = wn; 
  WN* wn_max = HMB_Maximum_Point(wn_snl); 
  if (wn_max == NULL) 
    return; 

  STACK<WN*>* stk_bounds = CXX_NEW(STACK<WN*>(&LNO_local_pool), 
    &LNO_local_pool); 
  LS_IN_LOOP* loop_ls = CXX_NEW(LS_IN_LOOP(wn_outer, dg, &LNO_local_pool, 
    TRUE), &LNO_local_pool);
  HMB_Hoist_Easy_Messy_Bounds(wn_max, stk_bounds, loop_ls); 
  HMB_Hoist_Easy_Messy_Subscripts(wn_max); 
  wn_max = Code_Expansion_Limit_Loop(wn_max);  
  HMB_Replace_Messy_Bounds_Loop(wn_max, wn_max, NULL, stk_bounds, loop_ls); 
  SNL_Finalize_Index_Variables(wn_max);
  HMB_Hoist_Messy_Bounds(wn_max, stk_bounds, loop_ls); 
}

//-----------------------------------------------------------------------
// NAME: Hoist_Messy_Bounds
// FUNCTION: Hoist the array elements in loop bounds for all SNLs in the
//   tree 'func_nd', introducing appropriate guard tests where needed to
//   avoid speculation.
//-----------------------------------------------------------------------

extern void Hoist_Messy_Bounds(WN* func_nd)
{  
  if (!LNO_Hoist_Messy_Bounds) 
    return; 

  FIZ_FUSE_INFO* ffi=
    CXX_NEW(FIZ_FUSE_INFO(&LNO_local_pool), &LNO_local_pool);
  ffi->Build(func_nd);

  for (INT i = 0; i < ffi->Num_Snl(); i++) {
    if (ffi->Get_Depth(i) < 1 || ffi->Get_Type(i) != Inner)
      continue; 
    SNL_Hoist_Messy_Bounds(ffi->Get_Wn(i));
  }
}

