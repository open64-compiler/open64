/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
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
#include "move.h"
#include "snl_utils.h"
#include "access_vector.h"
#include "array_bounds.h"
#include "soe.h"
#include "cond.h"
#include "tlog.h"
#include "fiz_fuse.h"
#include "forward.h"
#include "small_trips.h"

#include "ir_reader.h"

//-----------------------------------------------------------------------
// NAME: Remove_Zero_Trip_Loop
// FUNCTION: Remove the zero trip loop 'wn_loop'.
//-----------------------------------------------------------------------

extern void Remove_Zero_Trip_Loop(WN* wn_loop)
{
  // Don't try this on doacross nests. 
  if (Is_Nested_Doacross(wn_loop)) { 
    DevWarn("Attempted removing one loop out of a nested doacross"); 
    return; 
  } 

  if (LNO_Verbose) {
    fprintf(stdout, "Removing Zero Trip Loop on line %d\n", 
      Srcpos_To_Line(WN_linenum(wn_loop)));
    fprintf(TFile, "Removing Zero Trip Loop on line %d\n", 
      Srcpos_To_Line(WN_linenum(wn_loop)));
  } 
  if (LNO_Tlog) 
    Generate_Tlog("LNO", "trip_count", Srcpos_To_Line(WN_linenum(wn_loop)),
      (char *) WB_Whirl_Symbol(wn_loop), "", "", "zero-trip");   
    FmtAssert(Iterations(wn_loop, &LNO_local_pool) == 0,
    ("Loop is not zero trip."));
  WN* wn_asg = WN_start(wn_loop);
  if (Index_Variable_Live_At_Exit(wn_loop)) {
    LWN_Insert_Block_Before(LWN_Get_Parent(wn_loop), wn_loop, 
      WN_start(wn_loop));
    WN_start(wn_loop) = NULL;
  }
  LWN_Extract_From_Block(wn_loop);
  LWN_Delete_Tree(wn_loop);
}

//-----------------------------------------------------------------------
// NAME: Remove_Unity_Trip_Loop_Loop_Stmt_Update 
// FUNCTION: Update the loop stmts inside 'wn_loop' by making those which 
//   indicate 'wn_loop' as their loop stmt indicate one loop further out.  
//-----------------------------------------------------------------------

static void Remove_Unity_Trip_Loop_Loop_Stmt_Update(WN* wn_loop, 
						    DU_MANAGER* du)
{
  LWN_ITER* itr = LWN_WALK_TreeIter(WN_do_body(wn_loop));
  WN* replace_with = LWN_Get_Parent(wn_loop);
  while (replace_with && WN_opcode(replace_with) != OPC_DO_LOOP)
    replace_with = LWN_Get_Parent(replace_with);
  for ( ; itr; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn;
    OPERATOR opr = WN_operator(wn);
    DEF_LIST *def_list = du->Ud_Get_Def(wn);
    if (def_list) {
      if (def_list->Loop_stmt() == wn_loop) {
        du->Ud_Get_Def(wn)->Set_loop_stmt(replace_with);
      }
    }
  }
}

//-----------------------------------------------------------------------
// NAME: Remove_Unity_Trip_Loop_Dep_Update 
// FUNCTION: Update the dependences for 'wn_loop', assuming that this 
//   loop is a unity trip loop which will be removed.  
//-----------------------------------------------------------------------

extern void Remove_Unity_Trip_Loop_Dep_Update(WN* wn_loop, 
					      ARRAY_DIRECTED_GRAPH16* dg,
					      BOOL will_not_remove_loop)
{
  for (LWN_ITER *iter = LWN_WALK_TreeIter(WN_do_body(wn_loop));
    iter; iter = LWN_WALK_TreeNext(iter)) {
    WN* wn = iter->wn;
    OPCODE op = WN_opcode(wn);
    if (!OPCODE_is_load(op) && !OPCODE_is_store(op) && !OPCODE_is_call(op))
      continue;
    VINDEX16 v = dg->Get_Vertex(wn);
    if (v == 0)
      continue;

    // first of all, if this vertex is for a node inside exactly one
    // loop (the loop we are removing), then delete the vertex.

    INT do_depth = Block_Loop_Depth(wn);
    FmtAssert(do_depth >= 0, ("this vertex must be inside a loop!"));
    if (do_depth == 0) {
      EINDEX16 e;
      EINDEX16 enext = 0;
      for (e = dg->Get_In_Edge(v); e; e = enext) {
        enext = dg->Get_Next_In_Edge(e);
        dg->Delete_Array_Edge(e);
      }
      for (e = dg->Get_Out_Edge(v); e; e = enext) {
        enext = dg->Get_Next_Out_Edge(e);
        dg->Delete_Array_Edge(e);
      }
      dg->Delete_Vertex(v);
      continue;
    }

    // Check each edge going out of a vertex which is inside the
    // winddown loop.  These are the ones which must be updated.
    EINDEX16 e_next = 0;
    for (EINDEX16 e = dg->Get_Out_Edge(v); e; e = e_next) {
      e_next = dg->Get_Next_Out_Edge(e);
      VINDEX16 vSink = dg->Get_Sink(e);
      WN* wnSink = dg->Get_Wn(vSink);
      if (!Wn_Is_Inside(wnSink, WN_do_body(wn_loop)))
        continue;
      DEPV_ARRAY* dva = dg->Depv_Array(e);
      INT wd_component = Do_Loop_Depth(wn_loop) - dva->Num_Unused_Dim();
      if (wd_component >= 0) {
        FmtAssert(wd_component < dva->Num_Dim(),
        	("Bad indexing into dependence vector."));
        // Only preserve dependences which have '=' winddown loop
        // component.  The others are extraneous and should be deleted.
        INT dva_new_num_vec = 0;
        for (INT i = 0; i < dva->Num_Vec(); i++) {
          DEPV* dv = dva->Depv(i);
          if (DEP_Direction(DEPV_Dep(dv, wd_component)) & DIR_EQ)
            dva_new_num_vec++;
        }
        // Update or remove the dependence vector.
        if (dva_new_num_vec > 0) {
          mUINT8 new_unused_dim=dva->Num_Unused_Dim();
          if (will_not_remove_loop)
            new_unused_dim++;
          DEPV_ARRAY* dva_new = Create_DEPV_ARRAY(dva_new_num_vec,
            dva->Num_Dim() - 1, new_unused_dim, dg->Pool());
          INT j = 0;
          for (INT i = 0; i < dva->Num_Vec(); i++) {
            DEPV* dv = dva->Depv(i);
            if (!(DEP_Direction(DEPV_Dep(dv, wd_component)) & DIR_EQ))
              continue;
            DEPV* dv_new = dva_new->Depv(j++);
            INT dnew = 0;
            for (INT dold = 0; dold < dva->Num_Dim(); dold++) {
              if (dold == wd_component)
                continue;
              FmtAssert(dnew >= 0 && dnew < dva_new->Num_Dim(),
                ("Bad indexing into dependence vector."));
              DEPV_Dep(dv_new, dnew++) = DEPV_Dep(dv, dold);
            }
          }
          Delete_DEPV_ARRAY(dva, dg->Pool());
          dg->Set_Depv_Array(e, dva_new);
        } else {
          dg->Delete_Array_Edge(e);
        }
      } else if (!will_not_remove_loop) {
	// we're deleting a bad loop, all we need to do is decrement the
	// number of bad loops
	dva->Set_Num_Unused_Dim(dva->Num_Unused_Dim()-1);
      }
    }
  }
}

//-----------------------------------------------------------------------
// NAME: Remove_Unity_Trip_Loop_Update_If_Accesses
// FUNCTION: Update the IF access "Contains_Do_Loops" condition for IFs
//   enclosing 'wn_parent' (inclusive), which is the parent of the unity 
//   trip loop that has just been removed. 
//-----------------------------------------------------------------------

static void Remove_Unity_Trip_Loop_Update_If_Accesses(WN* wn_parent) 
{
  for (WN* wn = wn_parent; wn != NULL; wn = LWN_Get_Parent(wn)) {
    if (WN_opcode(wn) == OPC_DO_LOOP)
      return; 
    if (WN_opcode(wn) == OPC_IF) {
      LWN_ITER* itr = LWN_WALK_TreeIter(wn);
      for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) 
	if (WN_opcode(itr->wn) == OPC_DO_LOOP)
	  break;
      BOOL has_do_loops = itr != NULL; 
      IF_INFO* ii = Get_If_Info(wn); 
      ii->Contains_Do_Loops = has_do_loops; 
    } 
  } 
}

//-----------------------------------------------------------------------
// NAME: Remove_Unity_Trip_Loop_Update_Is_Inner 
// FUNCTION: Mark the loop immediately enclosing 'wn_parent' (if there 
//   is one) as being Is_Inner if it now longer has an enclosed loops.  
//-----------------------------------------------------------------------

static void Remove_Unity_Trip_Loop_Update_Is_Inner(WN* wn_parent)
{
  WN* wn = 0;
  for (wn = wn_parent; wn != NULL; wn = LWN_Get_Parent(wn))
    if (WN_opcode(wn) == OPC_DO_LOOP)
      break;
  if (wn == NULL) 
    return; 
  WN* wn_loop = wn; 
  LWN_ITER* itr = LWN_WALK_TreeIter(WN_do_body(wn_loop));
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr))
    if (WN_opcode(itr->wn) == OPC_DO_LOOP)
      break;
  if (itr != NULL) 
    return; 
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 
  dli->Is_Inner = TRUE; 
}


//-----------------------------------------------------------------------
// NAME: Decrement_Loop_Depths
// FUNCTION: Decrement the loop depth of 'wn_loop' and all of the do
//   loops nested inside it.
//-----------------------------------------------------------------------

static void Decrement_Loop_Depths(WN* wn_loop)
{
  if (WN_opcode(wn_loop) == OPC_DO_LOOP) {
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
    dli->Depth--;
  }
  if (WN_opcode(wn_loop) == OPC_BLOCK) {
    for (WN* wn = WN_first(wn_loop); wn != NULL; wn = WN_next(wn))
      Decrement_Loop_Depths(wn);
  } else {
    for (INT i = 0; i < WN_kid_count(wn_loop); i++)
      Decrement_Loop_Depths(WN_kid(wn_loop, i));
  }
}

//-----------------------------------------------------------------------
// NAME: Remove_Unity_Trip_Loop
// FUNCTION: Remove the unity trip loop 'wdloop'.  If 'update_access',
//   update the access vectors, and 'update_do_depths' update the do
//   loop depths.  The firsst and last statements in the new code block 
//   are returned in 'wn_first' and 'wn_last'. 
//-----------------------------------------------------------------------

extern void Remove_Unity_Trip_Loop(WN* wn_loop,
                                   BOOL update_access,
				   WN** wn_first, 
				   WN** wn_last, 
				   ARRAY_DIRECTED_GRAPH16* dg, 
				   DU_MANAGER* du,
				   BOOL verify_trip_count)
{
  // Don't try this on doacross nests. 
  if (Is_Nested_Doacross(wn_loop)) {
    DevWarn("Attempted removing one loop out of a nested doacross"); 
    *wn_first = wn_loop; 
    *wn_last = wn_loop; 
    return; 
  } 

  // If live on exit, compute final value for index variable 
  if (Index_Variable_Live_At_Exit(wn_loop)) {
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
    FmtAssert(dli != NULL, ("Remove_Unity_Trip_Loop: No DO_LOOP_INFO"));
    if (dli->Has_Gotos) {
      WN* wn_copy = LWN_Copy_Tree(WN_start(wn_loop));
      LWN_Copy_Def_Use(WN_kid0(WN_start(wn_loop)), WN_kid0(wn_copy), du);
      const DU_NODE* node = NULL;
      USE_LIST *use_list = du->Du_Get_Use(WN_start(wn_loop));
      if (use_list != NULL) { 
	USE_LIST_ITER iter(use_list);
	for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
	  WN* wn_use = node->Wn();
	  du->Add_Def_Use(wn_copy, wn_use);
	}
	if (use_list->Incomplete()) {
	  USE_LIST* use_list_copy = du->Du_Get_Use(wn_copy);
	  use_list_copy->Set_Incomplete();
	}	
      } 
      LWN_Insert_Block_Before(LWN_Get_Parent(wn_loop), wn_loop, wn_copy);
      if (dg != NULL) { 
	if (!dg->Add_Deps_To_Copy_Block(WN_kid0(WN_start(wn_loop)), 
	    WN_kid0(wn_copy), FALSE))
	  LNO_Erase_Dg_From_Here_In(WN_start(wn_loop), dg);
      }
    }
    Finalize_Index_Variable_For_Remove_Unity_Trip_Loop(wn_loop, TRUE); 
  }

  if (LNO_Verbose) {
    fprintf(stdout, "Removing Unity Trip Loop on line %d\n", 
      Srcpos_To_Line(WN_linenum(wn_loop)));
    fprintf(TFile, "Removing Unity Trip Loop on line %d\n", 
      Srcpos_To_Line(WN_linenum(wn_loop)));
  } 
  if (LNO_Tlog) 
    Generate_Tlog("LNO", "trip_count", Srcpos_To_Line(WN_linenum(wn_loop)),
      (char *) WB_Whirl_Symbol(wn_loop), "", "", "unity-trip");   
  // If known, there better be 1 iteration
  // It's possible that the caller knows there is 1 even if Iterations
  // isn't smart enough to figure it out
  if (verify_trip_count) {
  	INT64 iter = Iterations(wn_loop, &LNO_local_pool);
  	FmtAssert((iter == 1) || (iter == -1),
    		("Loop not unity trip."));
  }

  // Replace the induction variable with the lower bound.
  WN* kid = NULL;
  WN* parent = LWN_Get_Parent(wn_loop);
  WN *wn_loop_block = WN_do_body(wn_loop);
  WN* wd_lower_bound = WN_kid0(WN_start(wn_loop));
  BOOL lb_is_const = FALSE;
  if (WN_operator(wd_lower_bound) != OPR_INTCONST) {
    Replace_Ldid_With_Exp_Copy(WN_index(wn_loop), WN_do_body(wn_loop),
    	wd_lower_bound, du);
  } else { // otherwise constant propogator will get them in a bit
    lb_is_const = TRUE;
  }
  // Update the loop stmts, dependences, and IF accesses. 
  Remove_Unity_Trip_Loop_Loop_Stmt_Update(wn_loop, du); 
  if (dg) Remove_Unity_Trip_Loop_Dep_Update(wn_loop, dg, FALSE); 
  WN* wn_parent = LWN_Get_Parent(wn_loop); 

  // Move the statements inside the loop outside 
  *wn_first = WN_first(wn_loop_block);
  *wn_last = WN_last(wn_loop_block);
  while ((kid = WN_last(wn_loop_block)) != NULL) {
    WN* exkid = LWN_Extract_From_Block(kid);
    LWN_Insert_Block_After(parent, wn_loop, exkid);
  }

  if (lb_is_const) { // move lower bound to just before the loop
    WN* lb = LWN_Copy_Tree(WN_start(wn_loop));
    WN* tmp = WN_start(wn_loop);
    WN_start(wn_loop) = lb;
    LWN_Set_Parent(WN_start(wn_loop),wn_loop);
    lb = tmp;
    LWN_Insert_Block_Before(LWN_Get_Parent(wn_loop),wn_loop,lb);
    *wn_first = lb;
    if (*wn_last == NULL) 
      *wn_last = lb; 
  }

  // Delete loop. 
  LWN_Delete_Tree(wn_loop);

  // Update access vectors. 
  if (update_access) {
    for (WN* wn = *wn_first; wn != NULL; wn = WN_next(wn)) {
      DOLOOP_STACK dostack(&LNO_local_pool);
      Build_Doloop_Stack(LWN_Get_Parent(wn), &dostack);
      LNO_Build_Access(wn, &dostack, &LNO_default_pool);
      if (wn == *wn_last)
        break;
    }
  }

  // Update loop depths. 
  for (WN* wn = *wn_first; wn != NULL; wn = WN_next(wn)) {
    Decrement_Loop_Depths(wn);
    if (wn == *wn_last)
      break;
  }

  // Update If Accesses and Is_Inner 
  Remove_Unity_Trip_Loop_Update_If_Accesses(wn_parent); 
  Remove_Unity_Trip_Loop_Update_Is_Inner(wn_parent); 

  WN *lb = *wn_first;
  if (lb && WN_operator(lb) == OPR_STID) {
    if (WN_operator(WN_kid0(lb)) == OPR_INTCONST) {
      Constant_Propogate(lb,WN_const_val(WN_kid0(lb)));
      USE_LIST *uses = Du_Mgr->Du_Get_Use(lb);
      if (uses && !uses->Incomplete() && uses->Is_Empty()) { //dead
	*wn_first = WN_next(*wn_first);
	if (*wn_first == NULL)
	  *wn_last = NULL; 
	else if ((*wn_last) && (WN_next(*wn_last) == *wn_first))
	  *wn_last = *wn_first;

	LWN_Delete_Tree(lb);
      }
    }
  }

}

//-----------------------------------------------------------------------
// NAME: Inner_LB_Is_Outer_Index_Variable 
// FUNCTION: Returns TRUE if the 'wn_outer_loop' and 'wn_inner_loop' have
//   the form: 
// 	do i = ...
//        do j = i + something, ...
//   Returns FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Inner_LB_Is_Outer_Index_Variable(WN* wn_inner_loop, 
					     WN* wn_outer_loop)
{
  DO_LOOP_INFO* dli_inner = Get_Do_Loop_Info(wn_inner_loop); 
  DO_LOOP_INFO* dli_outer = Get_Do_Loop_Info(wn_outer_loop); 
  ACCESS_ARRAY* aa_inner_lb = dli_inner->LB;
  if (Bound_Is_Too_Messy(aa_inner_lb))
    return FALSE; 
  if (aa_inner_lb->Num_Vec() > 1)
    return FALSE;
  ACCESS_VECTOR* av_inner_lb = aa_inner_lb->Dim(0); 
  for (INT i = 0; i < av_inner_lb->Nest_Depth(); i++) {
    if (i == dli_outer->Depth) {
      if (av_inner_lb->Loop_Coeff(i) != (mINT32) 1)
        return FALSE; 
    } else if (i == dli_inner->Depth) {
      if (av_inner_lb->Loop_Coeff(i) != (mINT32) -1)
	return FALSE; 
    } else if (av_inner_lb->Loop_Coeff(i) != 0) {
      return FALSE; 
    }
  }
  if (av_inner_lb->Contains_Lin_Symb() || av_inner_lb->Contains_Non_Lin_Symb())
    return FALSE; 
  if (av_inner_lb->Const_Offset != 0)
    return FALSE;  
  return TRUE; 
}

//-----------------------------------------------------------------------
// NAME: Inner_LB_Outer_IV_Offset 
// FUNCTION: Returns an ACCESS_VECTOR for "something" when the 'wn_outer_loop'
//   and 'wn_inner_loop' are in the form: 
//     do i = .... 
//       do j = i + something, ... 
//   Returns NULL otherwise. 
//-----------------------------------------------------------------------

static ACCESS_VECTOR* Inner_LB_Outer_IV_Offset(WN* wn_inner_loop,
                                               WN* wn_outer_loop)
{
  DO_LOOP_INFO* dli_inner = Get_Do_Loop_Info(wn_inner_loop); 
  DO_LOOP_INFO* dli_outer = Get_Do_Loop_Info(wn_outer_loop); 
  ACCESS_ARRAY* aa_inner_lb = dli_inner->LB;
  if (Bound_Is_Too_Messy(aa_inner_lb))
    return FALSE; 
  if (aa_inner_lb->Num_Vec() > 1)
    return FALSE;
  ACCESS_VECTOR* av_inner_lb = aa_inner_lb->Dim(0); 
  for (INT i = 0; i < av_inner_lb->Nest_Depth(); i++) {
    if (i == dli_outer->Depth) {
      if (av_inner_lb->Loop_Coeff(i) != (mINT32) 1)
        return FALSE; 
    } else if (i == dli_inner->Depth) {
      if (av_inner_lb->Loop_Coeff(i) != (mINT32) -1)
	return FALSE; 
    } else if (av_inner_lb->Loop_Coeff(i) != 0) {
      return FALSE; 
    }
  }
  ACCESS_VECTOR* av = CXX_NEW(ACCESS_VECTOR(av_inner_lb, &LNO_local_pool), 
    &LNO_local_pool); 
  av->Set_Loop_Coeff(dli_inner->Depth, 0); 
  return av; 
}

//-----------------------------------------------------------------------
// NAME: Loop_Has_Positive_Trip 
// FUNCTION: Returns TRUE if the loop 'wn_loop' has a provably positive trip
//   count, FALSE if it does not have a positive trip count or we can't tell
//   easily. 
//-----------------------------------------------------------------------

static BOOL Loop_Has_Positive_Trip(WN* wn_loop)
{
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
  if (dli->LB->Num_Vec() > 1) 
    return FALSE; 
  if (dli->UB->Num_Vec() > 1) 
    return FALSE; 
  ACCESS_VECTOR* av = Add(dli->LB->Dim(0), dli->UB->Dim(0), &LNO_local_pool);
  return av->Is_Const() && av->Const_Offset > 0; 
}  

//-----------------------------------------------------------------------
// NAME: Access_Vector_Condition_Provable
// FUNCTION: Returns TRUE we can prove the condition in 'av' within 'wn_loop'
//   FALSE if we can't prove it. 
// NOTE: The condition in the access vector represents a strict constraint
//  which must be achieved.  We look for two kinds of expressions: 
//    (1) 0 <= 0 
//    (2) index_variable + linear_exp + non_linear_exp <= constant_exp 
//  In the second case, we expect the index variable to have the value 
//  constant_exp - linear_exp - non_linear_exp the first time it reaches
//  the 'wn_loop' and some greater than or equal value for every subsequent
//  time it encounters this loop.  
//-----------------------------------------------------------------------

static BOOL Access_Vector_Condition_Provable(ACCESS_VECTOR* av, 
					     WN* wn_loop)
{
  // 0 <= 0 is the easiest condition to prove 
  if (av->Is_Const())
    return av->Const_Offset == 0; 

  // Look for a single index variable to prove something about.  
  INT loop_count = 0;
  INT loop_index = -1; 
  for (INT i = 0; i < av->Nest_Depth(); i++) {
    if (av->Loop_Coeff(i) != 0) {
      loop_count++; 
      loop_index = i;
    } 
  }
  if (loop_count > 1)
    return FALSE;

  // Make that index variable monotonically increasing with positive step.  
  WN* wn = 0;
  for (wn = wn_loop; wn != NULL; wn = LWN_Get_Parent(wn)) 
    if (WN_opcode(wn) == OPC_DO_LOOP && Do_Loop_Depth(wn) == loop_index)
      break;
  WN* wn_index_loop = wn;
  DO_LOOP_INFO* dli_index = Get_Do_Loop_Info(wn_index_loop); 
  if (!dli_index->Step->Is_Const() || dli_index->Step->Const_Offset < 1)
    return FALSE; 

  // If the lower bound of that index variable starts at the right value
  // we have proven what we want. 
  if (dli_index->LB->Num_Vec() > 1)
    return FALSE; 
  ACCESS_VECTOR *avs = Subtract(av, dli_index->LB->Dim(0), &LNO_local_pool);
  if (avs->Is_Const() && avs->Const_Offset == 0)
    return TRUE; 

  // Look for an IF statement enclosing the loop which may prove what we want.
  WN* wnn = NULL;
  INT if_count = 0;  
  WN* wn_if = NULL; 
  WN* wn_branch = NULL; 
  for (wn = wn_loop; wn != NULL; wnn = wn, wn = LWN_Get_Parent(wn)) {
    if (WN_opcode(wn) == OPC_IF) {
      if_count++;
      wn_if = wn; 
      wn_branch = wnn; 
    }
  }
  if (if_count != 1)
    return FALSE; 
  BOOL is_then_branch = WN_then(wn_if) == wn_branch; 
  IF_INFO* if_info = Get_If_Info(wn_if); 
  if (if_info->Condition->Num_Vec() > 1)
    return FALSE; 
  ACCESS_VECTOR av_if(if_info->Condition->Dim(0), &LNO_local_pool); 
  if (is_then_branch && !if_info->Condition_On_Then 
      || !is_then_branch && if_info->Condition_On_Then) {
    av_if.Mul(-1);
    av_if.Const_Offset = -av_if.Const_Offset; 
    av_if.Const_Offset -= 1; 
  }
  ACCESS_VECTOR *avf = Subtract(av, &av_if, &LNO_local_pool);
  if (avf->Is_Const() && avf->Const_Offset == 0)
    return TRUE; 

  return FALSE; 
} 
    
//-----------------------------------------------------------------------
// NAME: Outer_LB_GE_Inner_UB 
// FUNCTION: Returns TRUE if the 'wn_outer_loop' and the 'wn_inner_loop'
//   have the following form:
//     do i = s, ....
//       do j = ..., s  
//-----------------------------------------------------------------------

static BOOL Outer_LB_GE_Inner_UB(WN* wn_inner_loop, 
				 WN* wn_outer_loop)
{  
  DO_LOOP_INFO* dli_inner = Get_Do_Loop_Info(wn_inner_loop); 
  DO_LOOP_INFO* dli_outer = Get_Do_Loop_Info(wn_outer_loop); 
  ACCESS_ARRAY* aa_outer_lb = dli_outer->LB;
  if (Bound_Is_Too_Messy(aa_outer_lb))
    return FALSE; 
  if (aa_outer_lb->Num_Vec() > 1)
    return FALSE; 
  ACCESS_ARRAY* aa_inner_ub = dli_inner->UB;
  if (Bound_Is_Too_Messy(aa_inner_ub))
    return FALSE; 
  if (aa_inner_ub->Num_Vec() > 1)
    return FALSE;
  ACCESS_VECTOR av_outer_lb(aa_outer_lb->Dim(0), &LNO_local_pool); 
  ACCESS_VECTOR av_inner_ub(aa_inner_ub->Dim(0), &LNO_local_pool); 
  if (av_outer_lb.Loop_Coeff(dli_outer->Depth) != -1)
    return FALSE; 
  av_outer_lb.Set_Loop_Coeff(dli_outer->Depth, 0); 
  if (av_inner_ub.Loop_Coeff(dli_inner->Depth) != 1)
    return FALSE; 
  av_inner_ub.Set_Loop_Coeff(dli_inner->Depth, 0); 
  INT i;
  for (i = dli_outer->Depth; i < av_outer_lb.Nest_Depth(); i++) 
    if (av_outer_lb.Loop_Coeff(i) != 0)
      return FALSE; 
  for (i = dli_outer->Depth; i < av_inner_ub.Nest_Depth(); i++)
    if (av_outer_lb.Loop_Coeff(i) != 0)
      return FALSE;
  av_outer_lb.Set_Nest_Depth(dli_outer->Depth);
  av_inner_ub.Set_Nest_Depth(dli_outer->Depth);
  ACCESS_VECTOR* av = Add(&av_inner_ub, &av_outer_lb, &LNO_local_pool); 
  av->Mul(-1);
  av->Const_Offset = -av->Const_Offset; 
  if (Access_Vector_Condition_Provable(av, wn_outer_loop))
    return TRUE;  
  return FALSE; 
}

//-----------------------------------------------------------------------
// NAME: Perfect_Nested_Outer_Loop 
// FUNCTION: Returns the loop perfectly nested outside 'wn_inner_loop'
//  if such a loop exists and both loops are in standard form and have 
//  step sizes of 1.  Otherwise returns NULL.  
//-----------------------------------------------------------------------

static WN* Perfect_Nested_Outer_Loop(WN* wn_inner_loop)
{
  DO_LOOP_INFO* dli_inner = Get_Do_Loop_Info(wn_inner_loop);
  if (dli_inner->Depth == 0) 
    return NULL;
  if (WN_prev(wn_inner_loop) != NULL)
    return NULL; 
  WN* wn_outer_loop = LWN_Get_Parent(LWN_Get_Parent(wn_inner_loop));
  if (WN_opcode(wn_outer_loop) != OPC_DO_LOOP)
    return NULL; 
  DO_LOOP_INFO* dli_outer = Get_Do_Loop_Info(wn_outer_loop);
  if (!dli_inner->Step->Is_Const() || dli_inner->Step->Const_Offset != 1)
    return NULL; 
  if (!dli_outer->Step->Is_Const() || dli_outer->Step->Const_Offset != 1)
    return NULL; 
  if (!Upper_Bound_Standardize(WN_end(wn_inner_loop), TRUE))
    return NULL; 
  if (!Upper_Bound_Standardize(WN_end(wn_outer_loop), TRUE))
    return NULL; 
  return wn_outer_loop; 
}

//-----------------------------------------------------------------------
// NAME: Forward_Substitute_For_Access_Arrays 
// FUNCTION: Forward substitute LDIDs in the bounds of 'wn_inner_loop', its 
//   immediately enclosing outer loop and the tests of any enclosing IFs.
//-----------------------------------------------------------------------

static void Forward_Substitute_For_Access_Arrays(WN* wn_inner_loop, 
					         DU_MANAGER* du)
{
  WN* wn_outer_loop = LWN_Get_Parent(LWN_Get_Parent(wn_inner_loop));
  FmtAssert(wn_outer_loop != NULL, 
    ("Should be applied to a pair of coupled loops"));
  Forward_Substitute_Ldids(WN_kid0(WN_start(wn_outer_loop)), du); 
  Forward_Substitute_Ldids(UBexp(WN_end(wn_outer_loop)), du); 
  Forward_Substitute_Ldids(WN_kid0(WN_start(wn_inner_loop)), du); 
  Forward_Substitute_Ldids(UBexp(WN_end(wn_inner_loop)), du); 
  for (WN* wn = wn_inner_loop; wn != NULL; wn = LWN_Get_Parent(wn))
    if (WN_opcode(wn) == OPC_IF)
      Forward_Substitute_Ldids(WN_if_test(wn), du);
}

//-----------------------------------------------------------------------
// NAME: Unifiable_Loop 
// FUNCTION: Returns TRUE if 'wn_inner_loop' and its immediate enclosing 
//   loop can be "unified".  In this context, it means that the two loops
//   have the form: 
//     do i = s, x 
//        do j = i, s 
//   The loops are unified by replacing this with: 
//     do i = s, s 
//       do j = s, s 
//   We then expect Remove_Unity_Trip_Loop() to reduce both of these 
//   loops to a single iteration.  
//-----------------------------------------------------------------------

static BOOL Unifiable_Loop(WN* wn_inner_loop,
			   DU_MANAGER* du) 
{
  WN* wn_outer_loop = Perfect_Nested_Outer_Loop(wn_inner_loop);
  if (wn_outer_loop == NULL) 
    return FALSE; 
  Forward_Substitute_For_Access_Arrays(wn_inner_loop, du);
  if (!Inner_LB_Is_Outer_Index_Variable(wn_inner_loop, wn_outer_loop))
    return FALSE;
  if (!Loop_Has_Positive_Trip(wn_outer_loop))
    return FALSE;  
  if (Index_Variable_Live_At_Exit(wn_outer_loop))
    return FALSE; 
  if (!Outer_LB_GE_Inner_UB(wn_inner_loop, wn_outer_loop))
    return FALSE; 
  return TRUE; 
}

//-----------------------------------------------------------------------
// NAME: Unify_Loop 
// FUNCTION: Replace 'wn_inner_loop' and its immediately enclosing loop,
//   which must be of the form: 
//     do i = s, x 
//       do j = i, s 
//   with the loops: 
//     do i = s, s 
//      do j = s, s
//   loops to a single iteration.  
//-----------------------------------------------------------------------

static void Unify_Loop(WN* wn_inner_loop,
                       DU_MANAGER* du)
{
  WN* wn_outer_loop = LWN_Get_Parent(LWN_Get_Parent(wn_inner_loop));
  if (LNO_Verbose) {
    fprintf(stdout, "Unifying Coupled Loops on lines %d and %d\n", 
      Srcpos_To_Line(WN_linenum(wn_inner_loop)), Srcpos_To_Line(WN_linenum(wn_outer_loop)));
    fprintf(TFile, "Unifying Coupled Loops on lines %d and %d\n", 
      Srcpos_To_Line(WN_linenum(wn_inner_loop)), Srcpos_To_Line(WN_linenum(wn_outer_loop)));
  }
  if (LNO_Tlog) 
    Generate_Tlog("LNO", "trip_count", 
      Srcpos_To_Line(WN_linenum(wn_inner_loop)),
      (char *) WB_Whirl_Symbol(wn_inner_loop), "", "", "unify-coupled-loops"); 
  Replace_Wnexp_With_Exp_Copy(UBexp(WN_end(wn_outer_loop)), 
    WN_kid0(WN_start(wn_outer_loop)), du);  
  Replace_Wnexp_With_Exp_Copy(WN_kid0(WN_start(wn_inner_loop)), 
    UBexp(WN_end(wn_inner_loop)), du);  
  DOLOOP_STACK dostack(&LNO_local_pool);
  Build_Doloop_Stack(LWN_Get_Parent(wn_outer_loop), &dostack);
  LNO_Build_Access(wn_outer_loop, &dostack, &LNO_default_pool);
}

//-----------------------------------------------------------------------
// NAME: Trip_Reducible_Loop 
// FUNCTION: Returns a positive number if the loop immediately enclosing 
//   'wn_inner_loop' can have its trip count reduced.  The positive number
//   is the number of iterations the trip count can be reduced.  If 0 is 
//   returned, the trip count cannot be reduced.  
//-----------------------------------------------------------------------

static INT Trip_Reducible_Loop(WN* wn_inner_loop, 
			       DU_MANAGER* du) 
{
  WN* wn_outer_loop = Perfect_Nested_Outer_Loop(wn_inner_loop);
  if (wn_outer_loop == NULL) 
    return 0; 
  Forward_Substitute_For_Access_Arrays(wn_inner_loop, du);
  if (Index_Variable_Live_At_Exit(wn_outer_loop))
    return 0;
  ACCESS_VECTOR* av = Inner_LB_Outer_IV_Offset(wn_inner_loop, wn_outer_loop); 
  if (av == NULL)
    return 0;
  DO_LOOP_INFO* dli_inner = Get_Do_Loop_Info(wn_inner_loop);
  if (dli_inner->UB->Num_Vec() > 1)
    return 0; 
  ACCESS_VECTOR* av_offset = Add(av, dli_inner->UB->Dim(0), &LNO_local_pool);
  DO_LOOP_INFO* dli_outer = Get_Do_Loop_Info(wn_outer_loop);
  if (dli_outer->UB->Num_Vec() > 1)
    return 0; 
  av_offset->Set_Nest_Depth(dli_outer->Depth + 1);
  ACCESS_VECTOR* av_diff = Subtract(dli_outer->UB->Dim(0), av_offset,
    &LNO_local_pool);
  if (!av_diff->Is_Const() || av_diff->Const_Offset <= 0)
    return 0; 
  return av_diff->Const_Offset; 
}

//-----------------------------------------------------------------------
// NAME: Trip_Reduce_Loop 
// FUNCTION: Reduce the trip count of the loop 'wn_loop' by the amount 
//  'loop_count'. 
//-----------------------------------------------------------------------

static void Trip_Reduce_Loop(WN* wn_loop, 
			     INT loop_count,
			     DU_MANAGER* du)
{
  if (LNO_Verbose) {
    fprintf(stdout, "Trip Reducing Loop on line %d\n", 
      Srcpos_To_Line(WN_linenum(wn_loop)));
    fprintf(TFile, "Trip Reducing Loop on line %d\n", 
      Srcpos_To_Line(WN_linenum(wn_loop)));
  }
  if (LNO_Tlog) 
    Generate_Tlog("LNO", "trip_count", Srcpos_To_Line(WN_linenum(wn_loop)),
      (char *) WB_Whirl_Symbol(wn_loop), "", "", "trip-reduce-coupled-loops"); 
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
  FmtAssert(!Bound_Is_Too_Messy(dli->UB), ("Should be screened out"));
  FmtAssert(dli->UB->Num_Vec() == 1, ("Should be screened out")); 
  dli->UB->Dim(0)->Const_Offset -= loop_count; 
  FmtAssert(UBexp(WN_end(wn_loop)), ("Should already be standardized"));
  WN* wn_copy = LWN_Copy_Tree(UBexp(WN_end(wn_loop))); 
  LWN_Copy_Def_Use(UBexp(WN_end(wn_loop)), wn_copy, du);
  WN* wn_diff = LWN_Make_Icon(WN_rtype(wn_copy), -loop_count);
  TYPE_ID type_add = WN_rtype(wn_copy);
  OPCODE op_add = OPCODE_make_op(OPR_ADD, type_add, MTYPE_V);
  WN* wn_exp = LWN_CreateExp2(op_add, wn_copy, wn_diff);
  Replace_Wnexp_With_Exp_Copy(UBexp(WN_end(wn_loop)), wn_exp, du);
  LWN_Delete_Tree(wn_exp); 
}

//-----------------------------------------------------------------------
// NAME: Coupled_Loops_Traverse 
// FUNCTION: Traversal function for Optimize_Coupled_Loops().  
//-----------------------------------------------------------------------

static void Coupled_Loops_Traverse(WN* wn_tree, 
				   DU_MANAGER* du)
{
  if (WN_opcode(wn_tree) == OPC_DO_LOOP) {
    if (Unifiable_Loop(wn_tree, du))
      Unify_Loop(wn_tree, du);
    INT reduce_count = Trip_Reducible_Loop(wn_tree, du);
    if (reduce_count > 0)
      Trip_Reduce_Loop(LWN_Get_Parent(LWN_Get_Parent(wn_tree)), 
        reduce_count, du); 
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_tree);
    if (dli->Is_Inner) 
      return; 
  } 

  if (WN_opcode(wn_tree) == OPC_BLOCK) {
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn)) 
      Coupled_Loops_Traverse(wn, du);
  } else {
    for (INT i = 0; i < WN_kid_count(wn_tree); i++) 
      Coupled_Loops_Traverse(WN_kid(wn_tree, i), du);
  }
}

//-----------------------------------------------------------------------
// NAME: Optimize_Coupled_Loops 
// FUNCTION: Perform Unify_Loop() and Trip_Reduce_Loop() on applicable 
//   loop pairs, as described above. 
//-----------------------------------------------------------------------

extern void Optimize_Coupled_Loops(WN* wn_tree, 
		                   DU_MANAGER* du)
{
  if (!LNO_Coupled_Opts)
    return; 
  Coupled_Loops_Traverse(wn_tree, du);
}

//-----------------------------------------------------------------------
// NAME: Has_Scalar_Use_Inside_Loop 
// FUNCTION: Returns TRUE if 'wn_def' has a use within the loop 'wn_loop'
//   according to the DU_MANAGER 'du'.  Returns FALSE otherwise.  
//-----------------------------------------------------------------------

static BOOL Has_Scalar_Use_Inside_Loop(WN* wn_def, 
				       WN* wn_loop, 
				       DU_MANAGER* du)
{
  // Don't need to check array loads here.  Use dependence graph.
  if (WN_operator(wn_def) == OPR_ISTORE)
    return FALSE;
  USE_LIST *use_list = du->Du_Get_Use(wn_def);
  if (use_list == NULL)
    return FALSE; 
  if (use_list->Incomplete())
    return TRUE; 
  USE_LIST_ITER iter(use_list);
  const DU_NODE* node = NULL;
  for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
    WN* wn_use = node->Wn();
    if (Wn_Is_Inside(wn_use, wn_loop))
      return TRUE; 
  }
  return FALSE; 
}

//-----------------------------------------------------------------------
// NAME: Has_Array_Dep_Carried_Inside_Loop
// FUNCTION: Returns TRUE if 'wn_def' has a dependence to a use carried
//   by the loop 'wn_loop' or some loop inside it, according to the 
//   dependence graph 'dg'.  Returns FALSE otherwise.  
//-----------------------------------------------------------------------

static BOOL Has_Array_Dep_Carried_Inside_Loop(WN* wn_def, 
                                              WN* wn_loop,
				              ARRAY_DIRECTED_GRAPH16* dg)
{
  VINDEX16 v = dg->Get_Vertex(wn_def);
  if (v == 0) 
    return !WN_operator(wn_def) == OPR_STID; 
  EINDEX16 e = 0;
  for (e = dg->Get_In_Edge(v); e != 0; e = dg->Get_Next_In_Edge(e)) {
     WN* wn_source = dg->Get_Wn(dg->Get_Source(e));
     if (wn_source == wn_def)
       continue; 
     DEPV_ARRAY* dva = dg->Depv_Array(e); 
     INT dep_depth = dva->Loop_Carrying_Dependence(); 
     if (dep_depth >= Do_Loop_Depth(wn_loop))
       return TRUE; 
  }
  for (e = dg->Get_Out_Edge(v); e != 0; e = dg->Get_Next_Out_Edge(e)) {
     DEPV_ARRAY* dva = dg->Depv_Array(e); 
     WN* wn_sink = dg->Get_Wn(dg->Get_Sink(e));
     if (wn_sink == wn_def)
       continue; 
     INT dep_depth = dva->Loop_Carrying_Dependence(); 
     if (dep_depth >= Do_Loop_Depth(wn_loop))
       return TRUE; 
  }
  return FALSE; 
}

//-----------------------------------------------------------------------
// NAME: Has_Complex_Access_Array 
// FUNCTION: Returns TRUE if the access array of the definition 'wn_def' 
//   is too messy or not constant in all loops inside 'wn_loop'.  
//-----------------------------------------------------------------------

static BOOL Has_Complex_Access_Array(WN* wn_def,
				     WN* wn_loop) 
{
  if (WN_operator(wn_def) != OPR_ISTORE) 
    return FALSE; 
  WN* wn_array = WN_kid1(wn_def); 
  if (WN_operator(wn_array) != OPR_ARRAY)
    return TRUE; 
  ACCESS_ARRAY *aa = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map, wn_array);
  if (Bound_Is_Too_Messy(aa))
    return TRUE;  
  if (aa->Non_Const_Loops() > Do_Loop_Depth(wn_loop))
    return TRUE; 
  return FALSE; 
}

//-----------------------------------------------------------------------
// NAME: Mark_Indexed_References 
// FUNCTION: For the definition 'wn_def', set 'indexed[i]' to TRUE if 
//   'wn_def' indexes its enclosing loop of depth 'i'. 
//-----------------------------------------------------------------------

static void Mark_Indexed_References(WN* wn_def, 
				    INT indexed[])
{
  if (WN_operator(wn_def) != OPR_ISTORE)
    return; 
  WN* wn_array = WN_kid1(wn_def);  
  if (WN_operator(wn_array) != OPR_ARRAY)
    return; 
  ACCESS_ARRAY* aa = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map, wn_array);
  for (INT i = 0; i < aa->Num_Vec(); i++) {
    ACCESS_VECTOR* av = aa->Dim(i); 
    for (INT j = 0; j < av->Nest_Depth(); j++) 
      if (av->Loop_Coeff(j) != 0)
	indexed[j] = TRUE; 
  }
}
 
//-----------------------------------------------------------------------
// NAME: Access_Trip_Count 
// FUNCTION: Returns an ACCESS_VECTOR which represents the trip count of
//  loop 'wn_loop' which is nested within 'wn_outer_loop'.  Returns NULL
//  if the access vector representation is too complicated.  
//-----------------------------------------------------------------------

static ACCESS_VECTOR* Access_Trip_Count(WN *wn_outer_loop, 
				        WN* wn_loop)
{
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 
  if (dli->LB->Num_Vec() > 1 || Bound_Is_Too_Messy(dli->LB) 
      || dli->LB->Non_Const_Loops() > Do_Loop_Depth(wn_outer_loop))
    return NULL;  
  if (dli->UB->Num_Vec() > 1 || Bound_Is_Too_Messy(dli->UB) 
      || dli->UB->Non_Const_Loops() > Do_Loop_Depth(wn_outer_loop))
    return NULL;  
  ACCESS_VECTOR* av = Add(dli->UB->Dim(0), dli->LB->Dim(0), &LNO_local_pool);
  av->Mul(-1); 
  av->Const_Offset += 1;
  return av;
}

//-----------------------------------------------------------------------
// NAME: Discard_Possibly_Empty_Loops 
// FUNCTION: For the loop nest with outer loop 'wn_loop' and deepest loop 
//   of depth 'nest_depth', the value of 'indexed[i]' is FALSE if the loop 
//   inside this nest of depth 'i' can be finalized.  Set 'indexed[i]' to 
//   TRUE if some loop inside may have a zero trip count depending on  
//   the value of the finalized loop's index variable. 
//-----------------------------------------------------------------------

static void Discard_Possibly_Empty_Loops(WN* wn_loop, 
				         INT nest_depth,
					 BOOL indexed[]) 
{
  ACCESS_VECTOR** av_trip = CXX_NEW_ARRAY(ACCESS_VECTOR*, nest_depth + 1, 
    &LNO_local_pool);  
  INT i;
  for (i = 0; i <= nest_depth; i++) 
    av_trip[i] = NULL; 
  WN* wn = 0;
  for (wn = wn_loop; wn != NULL; wn = Find_Next_Innermost_Do(wn)) 
    if (!indexed[Do_Loop_Depth(wn)])
      break; 
  if (wn == NULL) 
    return; 
  WN* wn_first = wn; 
  INT first_index = Do_Loop_Depth(wn_first);
  for (wn = wn_first; wn != NULL; wn = Find_Next_Innermost_Do(wn))
    av_trip[Do_Loop_Depth(wn)] = Access_Trip_Count(wn_loop, wn);
  for (wn = wn_first; wn != NULL; wn = Find_Next_Innermost_Do(wn)) {
    i = Do_Loop_Depth(wn); 
    if (indexed[i]) 
      continue; 
    for (INT j = Do_Loop_Depth(wn) + 1; j <= nest_depth; j++) {
      if (av_trip[j] == NULL || av_trip[j]->Loop_Coeff(i) < 0) { 
	indexed[Do_Loop_Depth(wn)] = TRUE; 
	break; 
      }
    }
  }
}

//-----------------------------------------------------------------------
// NAME: Set_Indexed_Loop_Bounds
// FUNCTION: For the access array 'aa', which represents either the lower
//   or upper bound of a DO LOOP at depth 'loop_depth', set 'indexed[i]' 
//   if the i-th index variable is used to define this loop.
//-----------------------------------------------------------------------

static void Set_Indexed_Loop_Bounds(ACCESS_ARRAY* aa, 
				    INT loop_depth, 
				    BOOL indexed[],
				    BOOL is_upper_bound) 
{ 
  if (aa == NULL || aa->Too_Messy) {
    for (INT i = 0; i < loop_depth; i++) 
      indexed[i] = TRUE; 
    return; 
  } 
  INT i;
  for (i = 0; i < aa->Num_Vec(); i++) {
    ACCESS_VECTOR* av = aa->Dim(i);
    if (!av->Has_Loop_Coeff()) {
      for (INT j = 0; j < loop_depth; j++) 
	indexed[j] = TRUE; 
      return; 
    } 
    if (is_upper_bound) { 
      for (INT j = 0; j < loop_depth; j++) 
	if (av->Loop_Coeff(j) > 0) 
	  indexed[j] = TRUE; 
    } else { 
      for (INT j = 0; j < loop_depth; j++) 
	if (av->Loop_Coeff(j) != 0) 
	  indexed[j] = TRUE; 
    } 
  } 
  INT const_limit = aa->Non_Const_Loops();
  if (const_limit > loop_depth)
    const_limit = loop_depth;  
  for (i = 0; i < const_limit; i++)
    indexed[i] = TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: Finalizable_Loops 
// FUNCTION: Returns a DOLOOP_STACK of loops within 'wn_loop' which can 
//   be replaced by their final iteration.  
//-----------------------------------------------------------------------

extern DOLOOP_STACK* SNL_Finalizable_Loops(WN* wn_loop, 
				           ARRAY_DIRECTED_GRAPH16* dg, 
				           DU_MANAGER* du)
{
  DOLOOP_STACK* stack_final 
    = CXX_NEW(DOLOOP_STACK(&LNO_default_pool), &LNO_default_pool); 

  if (!LNO_Loop_Finalization || wn_loop == NULL) 
    return stack_final; 

  if (!Do_Loop_Is_Good(wn_loop) || Is_Nested_Doacross(wn_loop) ||
       Do_Loop_Has_Gotos(wn_loop)) 
    return SNL_Finalizable_Loops(Good_Do_Next_Innermost(wn_loop), dg, du); 

  INT nest_depth = 0; 
  WN* wn = 0;
  for (wn = wn_loop; wn != NULL; wn = Find_Next_Innermost_Do(wn)) {
    nest_depth = Do_Loop_Depth(wn); 
    WN* wn_first = WN_first(WN_do_body(wn)); 
    for (WN* wnn = wn_first; wnn != NULL; wnn = WN_next(wnn)) {
      if (WN_opcode(wnn) == OPC_DO_LOOP)
	continue; 
      if (OPCODE_is_not_executable(WN_opcode(wnn)))
	continue; 
      if (!OPCODE_is_store(WN_opcode(wnn)))
	return SNL_Finalizable_Loops(Find_Next_Innermost_Do(wn), dg, du);   
      if (Has_Scalar_Use_Inside_Loop(wnn, wn_loop, du))
	return SNL_Finalizable_Loops(Find_Next_Innermost_Do(wn_loop), dg, du); 
      if (Has_Array_Dep_Carried_Inside_Loop(wnn, wn_loop, dg))
	return SNL_Finalizable_Loops(Find_Next_Innermost_Do(wn_loop), dg, du);  
      if (Has_Complex_Access_Array(wnn, wn_loop))
	return SNL_Finalizable_Loops(Find_Next_Innermost_Do(wn_loop), dg, du);
    }
  }

  BOOL* indexed = CXX_NEW_ARRAY(BOOL, nest_depth + 1, &LNO_local_pool);
  INT i;
  for (i = 0; i <= nest_depth; i++)
   indexed[i] = FALSE; 
  for (wn = wn_loop; wn != NULL; wn = Find_Next_Innermost_Do(wn)) {
    WN* wn_first = WN_first(WN_do_body(wn));     
    for (WN* wnn = wn_first; wnn != NULL; wnn = WN_next(wnn)) 
      Mark_Indexed_References(wnn, indexed); 
  }
  for (wn = wn_loop; wn != NULL; wn = Find_Next_Innermost_Do(wn)) {
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn);
    Set_Indexed_Loop_Bounds(dli->LB, dli->Depth, indexed, FALSE); 
    Set_Indexed_Loop_Bounds(dli->UB, dli->Depth, indexed, TRUE); 
  } 
  Discard_Possibly_Empty_Loops(wn_loop, nest_depth, indexed); 

  WN* wn_candidate = wn_loop; 
  for (i = Do_Depth(wn_loop); i <= nest_depth; i++) {
    if (!indexed[i]) 
      stack_final->Push(wn_candidate);  
    wn_candidate = Find_Next_Innermost_Do(wn_candidate); 
  }
  return stack_final; 
}

//-----------------------------------------------------------------------
// NAME: First_Invariant_Depth
// FUNCTION: Returns the depth of the first loop over which the loop 
//   bound access array 'aa' for loop 'wn_loop' is invariant.
//-----------------------------------------------------------------------

static INT First_Invariant_Depth(WN* wn_loop, 
			         ACCESS_ARRAY* aa)
{
  if (Bound_Is_Too_Messy(aa))
    return Do_Loop_Depth(wn_loop); 
  INT depth = aa->Non_Const_Loops();
  for (INT i = 0; i < aa->Num_Vec(); i++) {
    ACCESS_VECTOR* av = aa->Dim(i); 
    for (INT j = 0; j < av->Nest_Depth() - 1; j++) 
      if (av->Loop_Coeff(j) != 0) 
	depth = j + 1; 
  }
  return depth; 
}

//-----------------------------------------------------------------------
// NAME: SNL_Finalize_Loops
// FUNCTION: Replace the loops within 'wn_outer_loop' listed in 'stack_final'
//   with their final value and an appropriate guard test.
//-----------------------------------------------------------------------

extern WN* SNL_Finalize_Loops(WN* wn_outer_loop, 
			      DOLOOP_STACK* stack_final, 
			      ARRAY_DIRECTED_GRAPH16* dg, 
			      DU_MANAGER* du)
{
  WN* wn_first = NULL; 
  WN* wn_last = NULL; 
  WN* wn_return = wn_outer_loop; 
  for (INT i = 0; i < stack_final->Elements(); i++) {
    WN* wn_loop = stack_final->Bottom_nth(i); 
    if (LNO_Verbose) {
      fprintf(stdout, "Finalizing Loop on line %d\n", 
        Srcpos_To_Line(WN_linenum(wn_loop)));
      fprintf(TFile, "Finalizing Loop on line %d\n", 
        Srcpos_To_Line(WN_linenum(wn_loop)));
    }
    if (LNO_Tlog) 
      Generate_Tlog("LNO", "trip_count", Srcpos_To_Line(WN_linenum(wn_loop)),
        (char *) WB_Whirl_Symbol(wn_loop), "", "", "finalize-loop");   
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 
    if (!Upper_Bound_Standardize(WN_end(wn_loop), TRUE))
      continue; 
    if (Bound_Is_Too_Messy(dli->LB) || Bound_Is_Too_Messy(dli->UB))
      continue; 
    if (!dli->Step->Is_Const() || dli->Step->Const_Offset != 1)
      continue; 
    if (COND_Do_Info(wn_loop, &LNO_local_pool) != COND_DO_AT_LEAST_ONCE) {
      WN* wn_guard = Guard_A_Do(wn_loop); 
      WN_Reset_If_Guard(wn_guard); 
    }
    wn_return = Find_Next_Innermost_Do(wn_loop);  
    Replace_Wnexp_With_Exp_Copy(WN_kid0(WN_start(wn_loop)), 
      UBexp(WN_end(wn_loop)), du); 
    CXX_DELETE(dli->LB, &LNO_default_pool); 
    dli->LB = CXX_NEW(ACCESS_ARRAY(dli->UB, &LNO_default_pool), 
      &LNO_default_pool); 
    for (INT j = 0; j < dli->LB->Num_Vec(); j++) 
      dli->LB->Dim(j)->Negate_Me(); 
    Remove_Unity_Trip_Loop(wn_loop, TRUE, &wn_first, &wn_last, dg, du);
  } 
  return wn_return; 
}  

//-----------------------------------------------------------------------
// NAME: Finalize_Loops
// FUNCTION: Scan the tree rooted at 'func_nd' for loops which can be 
//   replaced by their last iteration and replace them.  
//-----------------------------------------------------------------------

extern void Finalize_Loops(WN* func_nd)
{
  if (!LNO_Loop_Finalization)
    return; 

  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  DU_MANAGER* du = Du_Mgr; 

  FIZ_FUSE_INFO* ffi=
    CXX_NEW(FIZ_FUSE_INFO(&LNO_local_pool), &LNO_local_pool);
  ffi->Build(func_nd);
  
  for (INT i = 0; i < ffi->Num_Snl(); i++) {
    WN* wn = ffi->Get_Wn(i);
    if (ffi->Get_Type(i) != Inner)
      continue; 
    DOLOOP_STACK* final_stack = SNL_Finalizable_Loops(wn, dg, du); 
    SNL_Finalize_Loops(wn, final_stack, dg, du); 
    CXX_DELETE(final_stack, &LNO_default_pool); 
  } 
} 


static void 
Update_Def_List_Loop_Stmt(WN* old_loop, WN* new_loop)
{
  LWN_ITER* itr = LWN_WALK_TreeIter(WN_do_body(old_loop));
  for (; itr; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn;
    DEF_LIST *def_list = Du_Mgr->Ud_Get_Def(wn);
    if (def_list) {
      if (def_list->Loop_stmt() == old_loop) {
        def_list->Set_loop_stmt(new_loop);
      }
    }
  }
}

// ---------------------------------------------------------------
// In order to fully unroll a loop with small constant trip count,
// we perform unrolling by the trip count, which creates a loop
// with a single iteration; we then call Remove_Unity_Trip_Loop
// to get rid of the loop.
// ---------------------------------------------------------------
extern void
Unroll_Loop_By_Trip_Count(WN* outerloop, INT u)
{
  INT i;

  // ctor does not set Type correctly for loop index variable
  SYMBOL indexsym(WN_index(outerloop));
  indexsym.Type = Do_Wtype(outerloop);

  INT depth = Do_Loop_Depth(outerloop);

  // Step_Size sets the step of the outerloop to u
  INT64 ostep = Step_Size(outerloop);
  ostep = Step_Size(outerloop, u*ostep);

  // unroll loop body u times
  WN** unroll_body = CXX_NEW_ARRAY(WN*, u, &LNO_local_pool);
  // unroll_body[0] = outerloop;
  unroll_body[0] = outerloop;
  LWN_Scale_Frequency(WN_end(outerloop), 1.0/u);
  LWN_Scale_Frequency(WN_step(outerloop), 1.0/u);
  for (i = 1; i < u; i++) {
    unroll_body[i] = LWN_Copy_Tree(outerloop, TRUE, LNO_Info_Map);
    LWN_Scale_Frequency_Tree(unroll_body[i], 1.0/u);
  }
  
  Unrolled_DU_Update(unroll_body, u, depth, TRUE, TRUE);

  // add 'i' to loop index in unroll_body[i]
  for (i = 1; i < u; i++) {
    Add_To_Symbol(unroll_body[i], i*ostep, indexsym, TRUE);
  }

  for (i = 1; i < u; i++) {
    Update_Def_List_Loop_Stmt(unroll_body[i], outerloop);
    // the loops have DU information pointing uselessly to loops that are
    // about to go away.  Change that
    LWN_Update_Def_Use_Delete_Tree(WN_start(unroll_body[i]));
    LWN_Update_Def_Use_Delete_Tree(WN_end(unroll_body[i]));
    LWN_Update_Def_Use_Delete_Tree(WN_step(unroll_body[i]));
  }
  
  for (i = 1; i < u; i++) {
    LWN_Insert_Block_After(WN_do_body(outerloop), 
                           WN_last(WN_do_body(outerloop)),
                           WN_do_body(unroll_body[i]));
  }
}
