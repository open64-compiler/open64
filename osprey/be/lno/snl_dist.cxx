/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
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


// -*-C++-*-

/** $Revision: 1.5 $
*** $Date: 04/12/21 14:57:15-08:00 $
*** $Author: bos@eng-25.internal.keyresearch.com $
*** $Source: /home/bos/bk/kpro64-pending/be/lno/SCCS/s.snl_dist.cxx $
**/

#include <stdint.h>
#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#define snl_dist_CXX      "snl_dist.cxx"
const static char *rcs_id =   snl_dist_CXX "$Revision: 1.5 $";

#include <sys/types.h>
#include <alloca.h>
#include <ctype.h>
#include <limits.h>

#include "pu_info.h"
#include "snl.h"
#include "snl_dist.h"
#include "lwn_util.h"
#include "lnoutils.h"
#include "lnopt_main.h"
#include "opt_du.h"
#include "lego.h"
#include "lego_opts.h"
#include "permute.h"
#include "snl_dist.h"
#include "debug.h" 

//-----------------------------------------------------------------------
// NAME: SNL_GEN_Distribute
// FUNCTION: Distribute out the imperfect code in the SNL 'wn_outer' of
//   'nloops' loops, if permissible. The variables 'above_is_distributable'
//   and 'below_is_distributable' determine if the imperfect code above
//   and below the main nest can be distributed out. The region of code
//   created is bounded by 'wn_new_first' and 'wn_new_last'.  If 'split_
//   depth' == -1, a complete set of perfect nests are constructed, other-
//   wise, the first nest consists of the loops from Do_Loop_Depth(wn_
//   outer) up to 'split_depth' - 1, and the other nests are as before.
//-----------------------------------------------------------------------

extern void SNL_GEN_Distribute(WN* wn_outer,
			       INT split_depth, 
                               INT nloops,
                               BOOL above_is_distributable,
                               BOOL below_is_distributable,
                               WN** wn_new_first,
                               WN** wn_new_last)
{
  DU_MANAGER* du = Du_Mgr;
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph;
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops);
  INT first_in_stack = Do_Loop_Depth(wn_inner) - nloops + 1;
  DOLOOP_STACK stack(&LNO_local_pool);
  Build_Doloop_Stack(wn_inner, &stack);

  WN* newup = NULL;
  WN* newdown = NULL;
  INT start_depth = split_depth == -1 ? first_in_stack + 1 : split_depth;
  for (INT lp = start_depth; lp < first_in_stack + nloops; lp++) {
    WN* wn = stack.Bottom_nth(lp);
    if (above_is_distributable && WN_prev(wn)) {
      if (newup == NULL)
        newup = SNL_Distribute(&stack, lp, first_in_stack, TRUE);
      else
        SNL_Distribute(&stack, lp, first_in_stack, TRUE);
    }
    if (below_is_distributable && WN_next(wn)) {
      if (newdown == NULL)
        newdown = SNL_Distribute(&stack, lp, first_in_stack, FALSE);
      else
        SNL_Distribute(&stack, lp, first_in_stack, FALSE);
    }
  }
  *wn_new_first = newup;
  *wn_new_last = newdown;
}

//-----------------------------------------------------------------------
// NAME: Split_Depth
// FUNCTION: Returns the split depth for the SNL whose outermost loop is 
//   'wn_outer' and which has 'nloops' loops. 
//-----------------------------------------------------------------------

extern INT Split_Depth(WN* wn_outer, 
		       INT nloops) 
{
  INT outer_depth = Do_Loop_Depth(wn_outer); 
  INT split_depth = outer_depth + nloops;
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops);
  DOLOOP_STACK stack(&LNO_local_pool);
  Build_Doloop_Stack(wn_inner, &stack);
  for (INT i = stack.Elements() - 1; i >= outer_depth; i--) {
    WN* wn_split = stack.Bottom_nth(i);
    if (!SNL_Is_Distributable(wn_outer, wn_outer, wn_split, TRUE))
       break;
    if (!SNL_Is_Distributable(wn_outer, wn_outer, wn_split, FALSE))
       break;
    split_depth = Do_Loop_Depth(wn_split);\
  }
  if (split_depth == outer_depth)
    split_depth = -1;
  return split_depth; 
} 

//-----------------------------------------------------------------------
// NAME: SNL_INV_Distribute
// FUNCTION: Distribute out the imperfect code from the invariant SNL
//   'wn_outer' of 'nloops' loops.  The region of code created is bounded
//   by 'wn_new_first' and 'wn_new_last'.
//-----------------------------------------------------------------------

extern void SNL_INV_Distribute(WN* wn_outer,
			       INT split_depth, 
                               INT nloops,
                               WN** wn_new_first,
                               WN** wn_new_last)
{
  WN* newup = NULL;
  WN* newdown = NULL;
  DU_MANAGER* du = Du_Mgr;
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph;
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops);
  INT first_in_stack = Do_Loop_Depth(wn_inner) - nloops + 1;
  DOLOOP_STACK stack(&LNO_local_pool);
  Build_Doloop_Stack(wn_inner, &stack);

  INT start_depth = split_depth == -1 ? first_in_stack + 1 : split_depth;
  for (INT lp = start_depth; lp < first_in_stack + nloops; lp++) {
    WN* wn = stack.Bottom_nth(lp);
    if (WN_prev_executable(wn)) {
      if (newup == NULL)
        newup = SNL_Distribute(&stack, lp, first_in_stack, TRUE);
      else
        SNL_Distribute(&stack, lp, first_in_stack, TRUE);
    }
    if (WN_next_executable(wn)) {
      if (newdown == NULL)
        newdown = SNL_Distribute(&stack, lp, first_in_stack, FALSE);
      else
        SNL_Distribute(&stack, lp, first_in_stack, FALSE);
    }
  }
  *wn_new_first = newup;
  *wn_new_last = newdown;
}

//-----------------------------------------------------------------------
// NAME: Dep_Carried_Outside_Or_Zero
// FUNCTION: Returns TRUE if the dependences represented by the edge 'e'
//   is carried by some loop outside the loop of depth 'loopd' or is a 
//   loop independent dependence (i.e. all zeros).  Returns FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Dep_Carried_Outside_Or_Zero(INT loopd, 
					EINDEX16 e)
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph;

  DEPV_ARRAY* dv = dg->Depv_Array(e);
  for (INT d = 0; d < dv->Num_Vec(); d++) {
    BOOL ok = FALSE;
    DEPV* dd = dv->Depv(d);
    INT i;
    for (i = 0; !ok && (i < loopd - dv->Num_Unused_Dim()); i++) {
      if (i == dv->Num_Dim())
	return FALSE; 
      switch (DEP_Direction(DEPV_Dep(dd, i))) {
       case DIR_POS:
	ok = TRUE;
	break;
       case DIR_EQ:
       case DIR_POSEQ:
	break;
       default:
	Is_True(0, ("Impossible lexpos dependence"));
      }
    }
    if (ok == FALSE) {
      // if it's not carried outside, it must be zero inside.
      for ( ; i < dv->Num_Dim(); i++) {
	DIRECTION dir = DEP_Direction(DEPV_Dep(dd,i));
	if (dir != DIR_EQ)
	  return FALSE;
      }
    }
  }
  return TRUE;
}

//-----------------------------------------------------------------------
// NAME: In_Same_SNL_Section
// FUNCTION: Returns TRUE if 'wn1' and 'wn2' are in the same section of 
//   the SNL, returns FALSE otherwise.  By "same section", we mean that 
//   they are enclosed by the same loops and that they are either both 
//   inside the innermost loop, or they are both either above or below
//   the innermost loop in the SNL. 
//-----------------------------------------------------------------------

static BOOL In_Same_SNL_Section(WN* wn1, WN* wn2) 
{
  WN *wn;
  for (wn = wn1; wn != NULL; wn = LWN_Get_Parent(wn))
    if (WN_opcode(LWN_Get_Parent(wn)) == OPC_BLOCK) 
      break;
  WN* wn1_stat = wn;   
  for (wn = wn2; wn != NULL; wn = LWN_Get_Parent(wn))
    if (WN_opcode(LWN_Get_Parent(wn)) == OPC_BLOCK) 
      break;
  WN* wn2_stat = wn;   
  if (LWN_Get_Parent(wn1_stat) != LWN_Get_Parent(wn2_stat))
    return FALSE;
  WN* wn_first = WN_first(LWN_Get_Parent(wn1_stat)); 
  INT stat_count = 0; 
  for (wn = wn_first; wn != NULL; wn = WN_next(wn)) {
    if (wn == wn1_stat) 
      stat_count++; 
    if (wn == wn2_stat)
      stat_count++; 
    if (WN_opcode(wn) == OPC_DO_LOOP)
      return stat_count == 0 || stat_count == 2; 
  }
  FmtAssert(stat_count == 2, ("Must see both statements"));
  return TRUE; 
}

//-----------------------------------------------------------------------
// NAME: Wn_Is_Above
// FUNCTION: Return TRUE if the node 'wn_one' is above 'wn_two' in the 
//   whirl tree, return FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Wn_Is_Above(WN* wn_one, 
			WN* wn_two)
{
  if (wn_one == wn_two)
    return FALSE; 
  WN* wn_common = Common_Ancestor(wn_one, wn_two);
  WN* wn_sibling_one = NULL; 
  WN *wn;
  for (wn = wn_one; wn != wn_common; wn = LWN_Get_Parent(wn))
    wn_sibling_one = wn; 
  WN* wn_sibling_two = NULL; 
  for (wn = wn_two; wn != wn_common; wn = LWN_Get_Parent(wn))
    wn_sibling_two = wn; 
  if (WN_opcode(wn_common) == OPC_BLOCK) { 
    for (WN* wn = WN_next(wn_sibling_one); wn != NULL; wn = WN_next(wn)) 
      if (wn == wn_sibling_two)
        return TRUE; 
    return FALSE; 
  } else { 
    for (INT i = 0; i < WN_kid_count(wn_common); i++) {
      if (WN_kid(wn_common, i) == wn_sibling_one)
	return TRUE; 
      if (WN_kid(wn_common, i) == wn_sibling_two)
	return FALSE; 
    } 
    return FALSE; 
  } 
}

//-----------------------------------------------------------------------
// NAME: Wn_Is_Below
// FUNCTION: Return TRUE if the node 'wn_one' is below 'wn_two' in the 
//   whirl tree, return FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Wn_Is_Below(WN* wn_one, 
			WN* wn_two)
{
  if (wn_one == wn_two)
    return FALSE; 
  return !Wn_Is_Above(wn_one, wn_two);
} 

//-----------------------------------------------------------------------
// NAME: SNL_Is_Distributable_Tree
// FUNCTION: Check all of the nodes in the tree rooted at 'wn_tree' and
//   return TRUE if there is any node which cannot be distributed from the  
//   loop 'wn_inner'.  If 'above' is TRUE, we are attempting to distribute 
//   nodes ABOVE the loop 'wn_inner', if FALSE, we are attempting to dis-
//   tribute nodes BELOW the loop 'wn_inner'.  Dependences carried on loops 
//   outside 'wn_dist' are ignored.  Return FALSE if there is no distribution 
//   preventing node in 'wn_tree'. 
// NOTES: There are three basic sections of code we are concerned with:
//     (1) Inside the outer loop and ABOVE the inner loop
//     (2) Inside the inner loop
//     (3) Inside the outer loop and BELOW the inner loop
//   Arcs to (1) from (2) and (3) prevent distribution ABOVE
//   Arcs from (3) to (1) and (2) prevent distribution BELOW
//-----------------------------------------------------------------------

static BOOL SNL_Is_Distributable_Tree(WN* wn_tree,
				      WN* wn_dist, 
				      WN* wn_inner, 
				      BOOL above)
{
  INT dist_depth = Do_Loop_Depth(wn_dist); 
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  LWN_ITER* itr = LWN_WALK_TreeIter(wn_tree); 
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn; 

    // Must be load, store, or call
    if (!OPCODE_is_load(WN_opcode(wn)) && !OPCODE_is_store(WN_opcode(wn))
        && !OPCODE_is_call(WN_opcode(wn)))
      continue;

    // Must be a memory reference. 
    VINDEX16 v = dg->Get_Vertex(wn);
    if (v == 0) {
//Bug 10708: we should assume the worst case
      if (WN_operator(wn) == OPR_LDID 
	  || WN_operator(wn) == OPR_STID)
#ifdef KEY
//Bug 10915: For LDID, it is always fine to distribute (may require 
//           scalar expansion
//           For STID, there are two cases:
//           (1) above: fine through scalar expansion
//               do i ...
//                   x = ...
//                   do j ...
//                     = x ...
//                   enddo //j
//               enddo //i 
//
//            (2) below: distribution causes problem in bug 10708
//               do i ...
//                   do j ...
//                     = x ...
//                   enddo //j
//                   x = ...
//               enddo //i                  
          if(WN_operator(wn) == OPR_STID && !above)//case (2)            
            return FALSE;
          else
#endif
	 continue; // LDID and case (1)
      return FALSE;
   }

    // Check for distribution preventing dependences. 
    if (above) { 
      for (EINDEX16 e = dg->Get_In_Edge(v); e; e = dg->Get_Next_In_Edge(e)) { 
        VINDEX16 v2 = dg->Get_Source(e); 
	WN* wn2 = dg->Get_Wn(v2); 
	if ((Wn_Is_Inside(wn2, wn_inner) || Wn_Is_Below(wn2, wn_inner))
	    && !Dep_Carried_Outside_Or_Zero(dist_depth, e)
	    && !In_Same_SNL_Section(wn, wn2))  
	  return FALSE; 
      }
    } else { 
      for (EINDEX16 e = dg->Get_Out_Edge(v); e; e = dg->Get_Next_Out_Edge(e)) { 
        VINDEX16 v2 = dg->Get_Sink(e); 
	WN* wn2 = dg->Get_Wn(v2); 
	if ((Wn_Is_Inside(wn2, wn_inner) || Wn_Is_Above(wn2, wn_inner))
	    && !Dep_Carried_Outside_Or_Zero(dist_depth, e)
	    && !In_Same_SNL_Section(wn, wn2))  
	  return FALSE; 
      }
    }
  }
  return TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: SNL_Is_Distributable_Traverse
// FUNCTION: Traverse the loop 'wn_loop' and return TRUE if there is any 
//   node which cannot be distributed from the loop 'wn_inner'.
//   If 'above' is TRUE, we are attempting to distribute nodes ABOVE the 
//   loop 'wn_inner', if FALSE, we are attempting to distribute nodes 
//   BELOW the loop 'wn_inner'.  Dependences carried on loops outside 
//   'wn_dist' are ignored.  Return FALSE if there is no distribution 
//   preventing node in 'wn_loop'. 
//-----------------------------------------------------------------------

static BOOL SNL_Is_Distributable_Traverse(WN* wn_loop, 
					  WN* wn_dist, 
					  WN* wn_inner, 
					  BOOL above)
{
 FmtAssert(WN_opcode(wn_loop) == OPC_DO_LOOP, 
   ("SNL_Is_Distributable_Traverse: First arg must be do loop")); 
 if (wn_loop == wn_inner) 
   return TRUE; 
 WN* wn_first = WN_first(WN_do_body(wn_loop)); 
 if (above) { 
   WN *wn;
#ifdef KEY //bug 11671: we may not find a do loop, especially for APO
   for (wn = wn_first; wn && WN_opcode(wn) != OPC_DO_LOOP; wn = WN_next(wn))
#else
   for (wn = wn_first; WN_opcode(wn) != OPC_DO_LOOP; wn = WN_next(wn))
#endif
     if (!SNL_Is_Distributable_Tree(wn, wn_dist, wn_inner, above))
       return FALSE;
#ifdef KEY //bug 11671: even though wn_loop is not inner, but inner is hidden in region
   if(wn==NULL)
     return FALSE;
#endif 
   if (!SNL_Is_Distributable_Traverse(wn, wn_dist, wn_inner, TRUE))
     return FALSE; 
 } else { 
   WN *wn;
#ifdef KEY //bug 11671
  for (wn = wn_first; wn && WN_opcode(wn) != OPC_DO_LOOP; wn = WN_next(wn));
    if(wn==NULL)
      return FALSE;
#else
  for (wn = wn_first; wn && WN_opcode(wn) != OPC_DO_LOOP; wn = WN_next(wn));
#endif
   if (!SNL_Is_Distributable_Traverse(wn, wn_dist, wn_inner, FALSE))
     return FALSE; 
   for (wn = WN_next(wn); wn != NULL; wn = WN_next(wn))
     if (!SNL_Is_Distributable_Tree(wn, wn_dist, wn_inner, FALSE))
	return FALSE; 
 } 
 return TRUE;  
}

//-----------------------------------------------------------------------
// NAME: SNL_Is_Distributable
// FUNCTION: For the SNL 'wn_outer' returns TRUE if it is possible to 
//   distribute out the sandwiched code between 'wn_outer' and 'wn_inner'.
//   If 'above', then test if it is possible to distribute out the code
//   between 'wn_outer' and 'wn_inner' which is ABOVE 'wn_inner', other-
//   wise test if it is possible to distribute out the code between 
//   'wn_outer' and 'wn_inner' which is BELOW 'wn_inner'.  Dependences
//   carried on loops outside 'wn_dist' are ignored.
// NOTE: We usually take 'wn_dist' to be 'wn_outer'.  
//-----------------------------------------------------------------------

extern BOOL SNL_Is_Distributable(WN* wn_dist, 
				 WN* wn_outer, 
				 WN* wn_inner, 
				 BOOL above)
{
  return SNL_Is_Distributable_Traverse(wn_outer, wn_dist, wn_inner, above); 
}

//-----------------------------------------------------------------------
// NAME: Print_Distribution
// FUNCTION: Print an informative message which describes a distribution
//   which has been performed.
//-----------------------------------------------------------------------

static void Print_Distribution(FILE* file, 
			       DOLOOP_STACK* stack, 
			       INT outer, 
			       INT inner, 
			       BOOL above) 
{ 
  if (above) 
    fprintf(file, "Distributing Above ("); 
  else 
    fprintf(file, "Distributing Below ("); 

  INT i;
  for (i = outer; i <= inner; i++) {
    fprintf(file, "%s", WB_Whirl_Symbol(stack->Bottom_nth(i))); 
    if (i < inner) 
      fprintf(file, ","); 
  } 
  fprintf(file, ") at ("); 
  for (i = outer; i <= inner; i++) { 
    fprintf(file, "%d", Srcpos_To_Line(WN_linenum(stack->Bottom_nth(i)))); 
    if (i < inner) 
      fprintf(file, ","); 
  } 
  fprintf(file, ")\n"); 
} 

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
// Distribution Implementation
//
// SNL_Distribute() takes all the imperfect code above or below the inner loop
// and distributes loopd across it.  Thus we get
//
//	DO loopd
//	  S1
//	  DO x
//	    S2
//	    DO inner
//	      S3
//
// and we get 
//
//	DO loopd
//	  S1
//	  DO x
//	    S2
//	DO loopd
//	  DO x
//	    DO inner
//	      S3
//
// dependence graph updating goes as follows:
//
// All arcs from the outside world into here don't care.  All arcs from
// S3 to S3 or notS3 to notS3 remain the same as well.  All arcs from
// notS3 to S3 or S3 to notS3 shorten to their common nesting, and in fact
// may go away if their common nesting (ignoring bad loops) is zero.
//
// Note that if S2's body has a loop_stmt dependent on loop x, then we have
// to make it dependent on the new x.
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------

WN* SNL_Distribute(DOLOOP_STACK* stack, INT inner, INT loopd, BOOL above)
{
  ARRAY_DIRECTED_GRAPH16*	dg = Array_Dependence_Graph;
  DU_MANAGER*			du = Du_Mgr;

  INT imperfect = loopd;
  INT i;
  for (i = loopd+1; i <= inner; i++) {
    if (( above && WN_prev_executable(stack->Bottom_nth(i))) ||
        (!above && WN_next_executable(stack->Bottom_nth(i))))
      imperfect = i;
  }

  if (imperfect == loopd)
    return NULL;

  if (LNO_Verbose) {
    Print_Distribution(stdout, stack, imperfect, inner, above); 
    Print_Distribution(TFile, stack, imperfect, inner, above); 
  } 

  // build inside out

  WN* wn_inner = stack->Bottom_nth(inner);
  WN* newdo = NULL;

  WN*    newdos[SNL_MAX_LOOPS];
  SYMBOL newsyms[SNL_MAX_LOOPS];

  for (i = imperfect; i > loopd; i--) {
    WN*	loop = stack->Bottom_nth(i);
    WN*	loopp = LWN_Get_Parent(loop);
    WN* newblk = WN_CreateBlock();
    WN*	next = NULL;

    for (WN* wn = above ? WN_first(loopp) : WN_next(loop);
	 wn && wn != loop;
	 wn = next) {
      next = WN_next(wn);
      LWN_Extract_From_Block(loopp, wn);
      LWN_Insert_Block_Before(newblk, NULL, wn);
    }

    if (newdo) {
      if (above)
	LWN_Insert_Block_Before(newblk, NULL, newdo);
      else
	LWN_Insert_Block_After(newblk, NULL, newdo);
    }

    // Now create the do and a mapping with accurate data for this do.

    WN* olddo = stack->Bottom_nth(i-1);
    newdo = LWN_CreateDO(LWN_Copy_Tree(WN_index(olddo)),
			 LWN_Copy_Tree(WN_start(olddo)),
			 LWN_Copy_Tree(WN_end(olddo)),
			 LWN_Copy_Tree(WN_step(olddo)),
			 newblk);

    LWN_Copy_Linenumber(olddo, newdo);
    LWN_Copy_Def_Use(WN_kid0(WN_start(olddo)), WN_kid0(WN_start(newdo)), du);
    dg->Add_Deps_To_Copy_Block(WN_kid0(WN_start(olddo)), 
      WN_kid0(WN_start(newdo)), FALSE); 
    LWN_Copy_Def_Use(WN_end(olddo), WN_end(newdo), du);
    dg->Add_Deps_To_Copy_Block(WN_end(olddo), WN_end(newdo), FALSE); 
    LWN_Copy_Def_Use(WN_kid0(WN_step(olddo)), WN_kid0(WN_step(newdo)), du);
    dg->Add_Deps_To_Copy_Block(WN_kid0(WN_step(olddo)), 
      WN_kid0(WN_step(newdo)), FALSE); 

    newdos[i-loopd-1] = newdo;
    newsyms[i-loopd-1] = SYMBOL(WN_index(newdo));
    DO_LOOP_INFO* olddli = Get_Do_Loop_Info(olddo);
    ACCESS_ARRAY* newlb = CXX_NEW(ACCESS_ARRAY(olddli->LB, &LNO_default_pool),
				  &LNO_default_pool);
    ACCESS_ARRAY* newub = CXX_NEW(ACCESS_ARRAY(olddli->UB, &LNO_default_pool),
				  &LNO_default_pool);
    ACCESS_VECTOR* newstep = CXX_NEW(ACCESS_VECTOR(olddli->Step,
						  &LNO_default_pool),
				  &LNO_default_pool);

    DO_LOOP_INFO* dli = CXX_NEW(DO_LOOP_INFO(&LNO_default_pool,
      newlb, newub, newstep, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
      (i == imperfect)), &LNO_default_pool);
    if (olddli->Lego_Info != NULL) 
      dli->Lego_Info = CXX_NEW(LEGO_INFO(olddli->Lego_Info, LEGO_pool),
        LEGO_pool); 
    Set_Do_Loop_Info(newdo, dli);
    DO_LOOP_INFO* innerdli = Get_Do_Loop_Info(stack->Bottom_nth(i));
    if (innerdli->Est_Num_Iterations > 20 &&
	innerdli->Num_Iterations_Symbolic == FALSE) {
      // NOTE: if we have DO i imperfect DO j DO k, then we only look
      // at j to know if the loop is important.
      dli->Set_Generally_Unimportant();
    }
    FmtAssert(olddli->Depth == i-1,
              ("Strange depth %d vs %d", olddli->Depth, i-1));
  }

  // take the outermost do and put it on the list at the appropriate place

  WN* wn_outer = stack->Bottom_nth(loopd);
  if (above)
    LWN_Insert_Block_Before(LWN_Get_Parent(wn_outer), wn_outer, newdo);
  else
    LWN_Insert_Block_After(LWN_Get_Parent(wn_outer), wn_outer, newdo);

  // rebuild access vectors

  DOLOOP_STACK dostack(&SNL_local_pool);
  Build_Doloop_Stack(LWN_Get_Parent(newdo), &dostack);
  LNO_Build_Access(newdo, &dostack, &LNO_default_pool);

  // fix up new do annotations

  for (i = imperfect; i > loopd; i--) {
    WN* the_do = newdos[i-loopd-1];
    DOLOOP_STACK stk(&SNL_local_pool);
    Build_Doloop_Stack(LWN_Get_Parent(the_do), &stk);
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(the_do);
    DO_LOOP_INFO* olddli = Get_Do_Loop_Info(stack->Bottom_nth(i));
    dli->Set_Est_Num_Iterations(&stk);
    dli->Is_Ivdep = olddli->Is_Ivdep;
    dli->Depth = i - 1;
  }

  // Now iterate through all the memory references in the distributed
  // nest.  Any edges starting or ending there, but with the other vertex
  // having an ancestor that is wn_outer, need to have their dependences
  // shortened or even removed.

  for (LWN_ITER* iter = LWN_WALK_TreeIter(newdo);
       iter;
       iter = LWN_WALK_TreeNext(iter)) {

    WN*         wn = iter->wn;
    OPCODE      opc = WN_opcode(wn);
    OPERATOR    opr = OPCODE_operator(opc);

    // if it's an ldid of a distributed loop, then fix the du list
    // also fix the loop_stmt

    if (opr == OPR_LDID) {
      SYMBOL s(wn);
      INT i;
      for (i = 0; i < imperfect - loopd; i++) {
	if (newsyms[i] == s) {
	  LWN_Update_Def_Use_Delete_Tree(wn, du);
	  SNL_Add_Du_To_Index_Ldid(newdos[i], wn, du, TRUE);
	  break;
	}
      }
      if (du->Ud_Get_Def(wn) != NULL) { 
	WN* loop_stmt = du->Ud_Get_Def(wn)->Loop_stmt();
	for (i = 0; i < imperfect - loopd; i++) {
	  WN* loop = stack->Bottom_nth(i+loopd);
	  if (loop_stmt == loop) {
	    du->Ud_Get_Def(wn)->Set_loop_stmt(newdos[i]);
	    SNL_DEBUG2(3, "SNL_Distribute: loop_stmt(0x%p)->0x%p",
		       loop_stmt, newdos[i]);
	  }
	}
      } 
    }

    // if it's a load or a store, fix dependence info

    if (!OPCODE_is_load(opc) && !OPCODE_is_store(opc) && !OPCODE_is_call(opc))
      continue;

    VINDEX16 v = dg->Get_Vertex(wn);
    if (v == 0) {
      FmtAssert(opr == OPR_LDID || opr == OPR_STID,
                ("How can we distribute with missing dependence data?"));
      continue;
    }

    // first do the in edges, then the out edges
    for (INT do_out = 0; do_out <= 1; do_out++) {

      for (EINDEX16 e = do_out ? dg->Get_Out_Edge(v) : dg->Get_In_Edge(v);
	   e != 0;
	   e = do_out ? dg->Get_Next_Out_Edge(e) : dg->Get_Next_In_Edge(e)) {
	VINDEX16 v2 = do_out ? dg->Get_Sink(e) : dg->Get_Source(e);
	FmtAssert(v == (do_out ? dg->Get_Source(e) : dg->Get_Sink(e)),
		  ("Bad sink on in list"));

        WN *wn;
	for (wn = dg->Get_Wn(v2); wn; wn = LWN_Get_Parent(wn))
	  if (wn == wn_outer)
	    break;

	if (wn) {
	  // May need to shorten dependence vector.

	  DEPV_ARRAY* dv = dg->Depv_Array(e);
	  INT max_depvector_length = loopd - dv->Num_Unused_Dim();
	  INT new_num_vec = 0;

	  if (max_depvector_length <= 0) {
	    dg->Delete_Array_Edge(e);
	  }
	  else if (dv->Num_Dim() > max_depvector_length) {
	    DEPV_ARRAY* dvnew = Create_DEPV_ARRAY(dv->Num_Vec(),
						  max_depvector_length,
						  dv->Num_Unused_Dim(),
						  dg->Pool());
	    for (INT d = 0; d < dv->Num_Vec(); d++)
	      for (INT i = 0; i < max_depvector_length; i++)
		DEPV_Dep(dvnew->Depv(d), i) = DEPV_Dep(dv->Depv(d), i);
	    Delete_DEPV_ARRAY(dv, dg->Pool());
	    dg->Set_Depv_Array(e, dvnew);
	  }
	}
      }
    }
  }

  FmtAssert(newdo, ("SNL_Distribute: where's the loop??"));
  Renumber_Loops(newdo, newdo, dg);
  return newdo;
}

//-----------------------------------------------------------------------
// NAME: SNL_Permutation_Is_Distributable 
// FUNCTION: Returns TRUE if the SNL with outermost loop 'wn_outer' and 
//   'nloops' loops, can have its imperfect code distributed out so that 
//   'permutation' can be performed. 
// EXAMPLE: For the loop nest: 
// 	do i = 1, n
//	  S0 
//	  do j = 1, n 
//	    S1 
//	    do k = 1, n 
//	      S2 
//	      do l = 1, n 
//	 	S3 
//	      end do 
//	    end do 
//	  end do 
//	end do 
//  and the permutation (1 0 3 2), it is necessary to distribute out the 
//  imperfect code sections S0 and S2, but not S1. 
//-----------------------------------------------------------------------

extern BOOL SNL_Permutation_Is_Distributable(WN* wn_outer,
                                             INT permutation[],
                                             INT nloops)
{
  DOLOOP_STACK stack(&LNO_local_pool); 
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops);
  Build_Doloop_Stack(wn_inner, &stack); 
  INT outer_index = Do_Loop_Depth(wn_outer); 
  INT last = -1; 
  for (INT first = 0; first < nloops; first = last + 1) {
    last = Permutation_Last(first, permutation, nloops); 
    if (first == last) {
      WN* wn_loop = stack.Bottom_nth(outer_index + first);
      DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
      if (dli->Has_Exits)
	return FALSE; 
      continue; 
    } 
    WN* wn_local_outer = stack.Bottom_nth(outer_index + first);
    WN* wn_local_inner = stack.Bottom_nth(outer_index + last);
    for (INT i = first; i <= last; i++) {
      WN* wn_loop = stack.Bottom_nth(outer_index + i);
      DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
      if (dli->Has_Gotos || dli->Has_Gotos_This_Level || dli->Has_Exits)
	return FALSE; 
    } 
    INT local_nloops = Do_Loop_Depth(wn_local_inner) 
      - Do_Loop_Depth(wn_local_outer) + 1; 
    if (!SNL_Is_Distributable(wn_local_outer, local_nloops))
      return FALSE; 
  }
  return TRUE; 
}

//-----------------------------------------------------------------------
// NAME: SNL_Has_Sandwiched_Code
// FUNCTION: Returns TRUE if the SNL with outermost loop 'wn_outer' and 
//   innermost loop 'wn_inner' has sandwiched code, FALSE otherwise.  
//-----------------------------------------------------------------------

static BOOL SNL_Has_Sandwiched_Code(WN* wn_outer, 
				    WN* wn_inner)
{
  WN* wnn = NULL; 
  for (WN* wn = wn_inner; wn != wn_outer; wn = wnn) {
    if (WN_prev_executable(wn) != NULL || WN_next_executable(wn) != NULL)
       return TRUE; 
    wnn = LWN_Get_Parent(wn); 
    FmtAssert(WN_opcode(wnn) == OPC_BLOCK, ("Did not find block")); 
    wnn = LWN_Get_Parent(wnn); 
    if (WN_opcode(wnn) != OPC_DO_LOOP) 
      return TRUE; 
  }
  return FALSE; 
} 

//-----------------------------------------------------------------------
// NAME: SNL_Permutation_Needs_Distribution 
// FUNCTION: Returns TRUE if the SNL with outermost loop 'wn_outer' which 
//   has 'nloops' can have its loops permuted according to the 'permutation'
//   without first applying permutation.  Returns FALSE otherwise.
//-----------------------------------------------------------------------

extern BOOL SNL_Permutation_Needs_Distribution(WN* wn_outer,
                                               INT permutation[],
                                               INT nloops)
{
  DOLOOP_STACK stack(&LNO_local_pool); 
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops);
  Build_Doloop_Stack(wn_inner, &stack); 
  INT outer_index = Do_Loop_Depth(wn_outer); 
  INT last = -1; 
  for (INT first = 0; first < nloops; first = last + 1) {
    last = Permutation_Last(first, permutation, nloops); 
    if (first == last) 
      continue; 
    WN* wn_local_outer = stack.Bottom_nth(outer_index + first);
    WN* wn_local_inner = stack.Bottom_nth(outer_index + last);
    if (SNL_Has_Sandwiched_Code(wn_local_outer, wn_local_inner))
      return TRUE;
  }
  return FALSE; 
}

//-----------------------------------------------------------------------
// NAME: SNL_Distribute_For_Permutation  
// FUNCTION: Distribute out the required imperfect code sections for the 
//   SNL of 'nloops' loops with outermost loop 'wn_outer' so that the 
//   'permutation' can be performed. 
// EXAMPLE: In the example code for SNL_Permutation_Is_Distributable()
//   above, after applying the permutation (1 0 3 2), the code will look 
//   like: 
// 	do i = 1, n
//	  S0 
//	end do 
//	do j = 1, n 
//	  do i = 1, n 
//	    S1 
//	    do k = 1, n 
//	      S2 
//	    end do 
//	    do l = 1, n 
//	      do k = 1, n 
//	 	S3 
//	      end do 
//	    end do 
//	  end do 
//	end do 
//-----------------------------------------------------------------------

extern void SNL_Distribute_For_Permutation(WN* wn_outer,
					   WN* wn_inner, 
                                           INT permutation[],
                                           INT nloops, 
					   DOLOOP_STACK* new_stack)
{
  if (nloops == 0) 
    return; 
  DOLOOP_STACK stack(&LNO_local_pool); 
  Build_Doloop_Stack(wn_inner, &stack); 
  INT last = -1; 
  INT outer_index = Do_Loop_Depth(wn_outer); 
  for (INT first = 0; first < nloops; first = last + 1) {
    last = Permutation_Last(first, permutation, nloops); 
    INT first_depth = outer_index + first; 
    INT last_depth = outer_index + last; 
    WN* wn_new_above = SNL_Distribute(&stack, last_depth, first_depth, TRUE); 
    if (new_stack != NULL && wn_new_above != NULL 
        && stack.Bottom_nth(first_depth) != wn_new_above)
      new_stack->Push(wn_new_above); 
    WN* wn_new_below = SNL_Distribute(&stack, last_depth, first_depth, FALSE); 
    if (new_stack != NULL && wn_new_below != NULL 
	&& stack.Bottom_nth(first_depth) != wn_new_below)
      new_stack->Push(wn_new_below); 
  }
}

//-----------------------------------------------------------------------
// NAME: SNL_Distribute_By_Splitting
// FUNCTION: Distribute the SNL with outermost loop 'wn_outer' above and
//   below the loop at depth 'split_depth', and place any new SNLs created
//   on the 'stack'.
//-----------------------------------------------------------------------

extern void SNL_Distribute_By_Splitting(WN* wn_outer,
                                        WN* wn_inner,
                                        INT nloops,
                                        INT split_depth,
                                        DOLOOP_STACK* stack)
{
  if (wn_outer == NULL || nloops == 0)
    return;
  INT outer_depth = Do_Loop_Depth(wn_outer);
  if (split_depth == -1 || split_depth == outer_depth)
    return;
  DOLOOP_STACK local_stack(&LNO_local_pool);
  Build_Doloop_Stack(wn_inner, &local_stack);
  WN* wn_new = SNL_Distribute(&local_stack, split_depth, outer_depth, TRUE);
  if (wn_new != NULL)
    stack->Push(wn_new);
  wn_new = SNL_Distribute(&local_stack, split_depth, outer_depth, FALSE);
  if (wn_new != NULL)
    stack->Push(wn_new);
}

//-----------------------------------------------------------------------
// NAME: SNL_Is_Distributable 
// FUNCTION: Returns TRUE if the SNL with outer loop 'wn_outer' consisting  
//   of 'nloops' loops is distributable above and below.  Returns FALSE 
//   otherwise. 
//-----------------------------------------------------------------------

extern BOOL SNL_Is_Distributable(WN* wn_outer, 
			         INT nloops)
{
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops); 
  DOLOOP_STACK stack(&LNO_local_pool); 
  Build_Doloop_Stack(wn_inner, &stack); 
  INT depth_inner = Do_Loop_Depth(wn_outer) + nloops - 1; 
  for (INT i = 2; i <= nloops; i++) {
    INT d = depth_inner + 1 - i;
    WN* wn_dist = stack.Bottom_nth(d); 
    INT dd;
    for (dd = d + 1; dd <= depth_inner; dd++) {
      WN* wn_outer = stack.Bottom_nth(dd - 1);
      WN* wn_inner = stack.Bottom_nth(dd);
      if (WN_prev_executable(wn_inner) 
	  && !SNL_Is_Distributable(wn_dist, wn_outer, wn_inner, TRUE))
        return FALSE;
    }
    for (dd = d + 1; dd <= depth_inner; dd++) {
      WN* wn_outer = stack.Bottom_nth(dd - 1);
      WN* wn_inner = stack.Bottom_nth(dd);
      if (WN_next_executable(wn_inner) 
	  && !SNL_Is_Distributable(wn_dist, wn_outer, wn_inner, FALSE))
        return FALSE;
    }
  }
  return TRUE; 
}
