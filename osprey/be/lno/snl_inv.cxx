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

/**
*** This file contains some of the code to implement the invariant SNL 
*** transformations.  These are the transformations that operate on SNLs
*** whose loop bounds are invariant with respect to the entire loop nest 
*** AND whose imperfect parts are always distributable. 
***/

#include <stdint.h>
#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#define snl_CXX      "snl.cxx"
const static char *rcs_id =   snl_CXX "$Revision: 1.5 $";

#include <sys/types.h>
#include <alloca.h>
#include "snl.h"
#include "snl_xbounds.h"
#include "config_targ.h"
#include "lwn_util.h"
#include "lnoutils.h"
#include "cxx_graph.h"
#include "opt_du.h"
#include "opt_alias_interface.h"
#include "wintrinsic.h"
#include "scalar_expand.h"
#include "strtab.h"
#include "dvector.h"
#include "lnopt_main.h"
#include "fb_whirl.h"
#include "move.h"
#include "small_trips.h"
#include "sxlimit.h"
#include "ir_reader.h"
#include "sxlist.h"
#include "debug.h"
#include "permute.h"
#include "tile.h"
#include "cond.h"
#include "wind_down.h"
#include "ff_utils.h"
#include "fb_whirl.h"
#include "tlog.h"

#pragma weak New_Construct_Id

//-----------------------------------------------------------------------
// UTILITIES 
//-----------------------------------------------------------------------

//-----------------------------------------------------------------------
// NAME: Rebuild_Access_Arrays 
// FUNCTION: Rebuild the access arrays on the 'region' which has the 
//   given 'the_newest_outer_loop'.  If a permutation has been performed
//   the resulting loops are stored in 'permloop[]'. 
//-----------------------------------------------------------------------

static void Rebuild_Access_Arrays(SNL_REGION region, 
				  MEM_POOL* pool)
{
  for (WN* wn_reg = region.First; ; wn_reg = WN_next(wn_reg)) {
    DOLOOP_STACK dostack(pool);
    Build_Doloop_Stack(LWN_Get_Parent(wn_reg), &dostack);
    LNO_Build_Access(wn_reg, &dostack, &LNO_default_pool);
    if (wn_reg == region.Last)
      break;
  }
} 

//-----------------------------------------------------------------------
// ROUTINES FOR SCALAR EXPANSION AND DISTRIBUTION 
//-----------------------------------------------------------------------

//-----------------------------------------------------------------------
// NAME: SNL_INV_Distribution
// FUNCTION: Apply distribution to the SNL of 'nloops' loops with outermost 
//   loop 'wn_outer'.  Update the 'region', which contained 'wn_outer'. 
//   Also update 'the_newest_outer_loop'.  
//-----------------------------------------------------------------------

static void SNL_INV_Distribution(WN* wn_outer,
				 INT nloops,  
				 BOOL find_split_depth, 
				 SNL_REGION* region, 
				 WN** the_newest_outer_loop)
{ 
  WN* newup = NULL;
  WN* newdown = NULL;
  INT outer_depth = Do_Loop_Depth(wn_outer); 
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops);
  DOLOOP_STACK stack(&LNO_local_pool);
  Build_Doloop_Stack(wn_inner, &stack);
  INT split_depth = find_split_depth ? Split_Depth(wn_outer, nloops) : -1; 
  SNL_INV_Distribute(wn_outer, split_depth, nloops, &newup, &newdown); 
  if (newup) {
    if (region->First == wn_outer)
      region->First = newup;
    *the_newest_outer_loop = newup; 
  } 
  if (newdown)
    if (region->Last == wn_outer)
      region->Last = newdown;
} 

//-----------------------------------------------------------------------
// NAME: Perfect_Depth 
// FUNCTION: Given the loop 'wn_loop' which is at most 'nloops' deep, 
//   compute how deep it is as a perfectly nested loop, with its inner-
//   most loop being a true innermost loop.  Returns 0 if the loop nest 
//   is not perfectly nested or if the innermost perfectly nested loop 
//   is not an innermost loop.  
//-----------------------------------------------------------------------

static INT Perfect_Depth(WN* wn_loop, 
			 const INT nloops)
{
  FmtAssert(WN_opcode(wn_loop) == OPC_DO_LOOP, 
    ("Tried to find depth, but not a DO_LOOP")); 
  WN* wn = wn_loop; 
  WN* wn_last = wn_loop;  
  INT perfect_depth;
  for (perfect_depth = 1; perfect_depth < nloops; perfect_depth++) {
    for (wn = WN_first(WN_do_body(wn)); wn != NULL 
      && (WN_opcode(wn) == OPC_PRAGMA || WN_opcode(wn) == OPC_XPRAGMA); 
      wn = WN_next(wn));
    if (wn == NULL || WN_opcode(wn) != OPC_DO_LOOP) 
      return Do_Loop_Is_Inner(wn_last) ? perfect_depth : 0; 
    wn_last = wn; 
    WN *wn_next;
    for (wn_next = WN_next(wn); wn_next != NULL 
      && (WN_opcode(wn_next) == OPC_PRAGMA 
      || WN_opcode(wn_next) == OPC_XPRAGMA); wn_next = WN_next(wn_next));
    if (wn_next != NULL)
      return Do_Loop_Is_Inner(wn_last) ? perfect_depth : 0;   
  }
  return Do_Loop_Is_Inner(wn_last) ? perfect_depth : 0; 
}   

//-----------------------------------------------------------------------
// NAME: Reduced_Permutation 
// FUNCTION:  Given the 'permutation' of length 'nloops', compute the 
//   'reduced_permutation' of length 'perfect_depth', which consists 
//   only of those components from 0 to perfect_depth-1 in the same 
//   order as the original permutation. 
//-----------------------------------------------------------------------

static void Reduced_Permutation(const INT permutation[], 
				const INT nloops, 
				INT reduced_permutation[],
        			const INT perfect_depth)
{
  INT j = 0; 
  FmtAssert(perfect_depth <= nloops, 
    ("Reduced permutation is not really reducedi size.")); 
  for (INT i = 0; i < nloops; i++) 
    if (permutation[i] < perfect_depth) 
      reduced_permutation[j++] = permutation[i]; 
  FmtAssert(j == perfect_depth, ("Permutation is wrong length.")); 
  Is_True(Is_Permutation_Vector(reduced_permutation, perfect_depth), 
    ("Permutation is not really a permutation.")); 
}

//-----------------------------------------------------------------------
// NAME: Distribute_Traverse 
// FUNCTION: For the loop nest 'wn_dst' apply a reduced version of
//   the 'permutation' of length 'nloops' to each of the non-main dis- 
//   tributends in that region.  Return the new outermost loop in the 
//   nest. 
//-----------------------------------------------------------------------

static WN* Distribute_Traverse(WN* wn_dst, 
			       const INT permutation[], 
			       const INT nloops, 
			       ARRAY_DIRECTED_GRAPH16* dg, 
			       DU_MANAGER* du, 
			       REDUCTION_MANAGER* rm)
{
  INT reduced_permutation[SNL_MAX_LOOPS]; 
  INT perfect_depth = Perfect_Depth(wn_dst, nloops);
  WN* wn_outer = wn_dst; 
  if (perfect_depth > 1 && perfect_depth < nloops) {
    Reduced_Permutation(permutation, nloops, reduced_permutation,
      perfect_depth);
    if (!Identity_Permutation(reduced_permutation, perfect_depth)) {
      wn_outer = SNL_INV_Permute_Loops(wn_dst, reduced_permutation, 
        perfect_depth, TRUE);
    } 
  } else if (perfect_depth == 0) {
    WN* wn_next = NULL; 
    WN* wn_first = WN_first(WN_do_body(wn_dst));  
    for (WN* wn = wn_first; wn != NULL; wn = wn_next) { 
      wn_next = WN_next(wn); 
      if (WN_opcode(wn) == OPC_DO_LOOP)
	Distribute_Traverse(wn, permutation, nloops, dg, du, rm); 
    } 
  } 
  return wn_outer; 
}
 
//-----------------------------------------------------------------------
// NAME: SNL_INV_Permute_Distributends 
// FUNCTION: For the loop nests in the 'region' apply a reduced version of
//   the 'permutation' of length 'nloops' to each of the non-main dis- 
//   tributends in that region. 
//-----------------------------------------------------------------------

static void SNL_INV_Permute_Distributends(SNL_REGION* region, 
				          const INT permutation[], 
				          const INT nloops,
				          ARRAY_DIRECTED_GRAPH16* dg,
                                          DU_MANAGER* du,
				          REDUCTION_MANAGER* rm)
{
  WN* wn_dst_next = NULL; 
  INT reduced_permutation[SNL_MAX_LOOPS]; 
  for (WN* wn_dst = region->First; wn_dst != NULL; wn_dst = wn_dst_next) {
    WN* wn_outer = Distribute_Traverse(wn_dst, permutation, 
      nloops, dg, du, rm); 
    if (region->First == wn_dst) 
      region->First = wn_outer; 
    if (region->Last == wn_dst) 
      region->Last = wn_outer; 
    if (wn_dst == region->Last) 
      return;  
  }
}  

//-----------------------------------------------------------------------
// NAME: SNL_INV_Limited_Scalar_Expand_And_Distribute 
// FUNCTION: Perform scalar expansion and distribution with limited 
//   tile sizes on the SNL with outer loop 'wn_outer' of 'nloops'. 
//   Tile subscripts are created assuming that the main kernel will be 
//   put into a 'permutation' of length 'nloops'.  The 'plist' is a 
//   privatization list constructed using SX_INFO::Make_Sx_Info(). 
//   The 'region' and 'wn_new_outer' are updated as required by 
//   SNL_Invariant_Transforms(). The 't_ptr' is a pointer to an 
//   SNL_TILE_INFO which describes the cache tiling recommended for 
//   the SNL.  Scalar expansion tiles and cache tiles are overlapped 
//   if appropriate to reduce loop overhead. 
//-----------------------------------------------------------------------

static BOOL SNL_INV_Limited_Scalar_Expand_And_Distribute(WN* wn_outer,
						         SNL_TILE_INFO** t_ptr,
                                        	         INT permutation[],
                                        	         INT nloops,
						         SX_PLIST* plist,
						         SNL_REGION* region, 
						         WN** wn_new_outer)
{
  SNL_TILE_INFO* ti_se = NULL;
  SNL_TILE_INFO* ti_ct = NULL;
  SNL_TILE_INFO* t = *t_ptr;
  WN* wn_inner = SNL_Innermost_Do(wn_outer);
  WN* the_newest_outer_loop = wn_outer;
  INT first_in_stack = Do_Loop_Depth(wn_inner) - nloops + 1;
  SE_CT_New_Tile_Infos(wn_outer, plist, t, permutation, nloops, 
    &LNO_local_pool, &ti_se, &ti_ct, TRUE);
  if (ti_se == NULL || ti_se->Strips() == 0)
    return FALSE; 
  WN* wn_new = SNL_INV_Limited_SE_And_Dist(wn_outer, ti_se, permutation, 
    nloops, plist, TRUE);
  if (wn_new != NULL) {
    *wn_new_outer = wn_new; 
    if (wn_outer != *wn_new_outer) {
      if (region->First == wn_outer)
	region->First = *wn_new_outer;
      if (region->Last == wn_outer)
	region->Last = *wn_new_outer;
    }
  }
  return TRUE; 
}

//-----------------------------------------------------------------------
// ROUTINES FOR INTERCHANGE  
//-----------------------------------------------------------------------

//-----------------------------------------------------------------------
// NAME: SNL_INV_Local_Permute_Loops
// FUNCTION: Permute the 'nloops' loops which are the innermost loops in
//   the 'stack'.  The index of the first loop in the stack's permutation 
//   is 'first_in_stack'.  The 'permutation[]' of length 'nloops' specifies
//   the permutation.  The loop order after the permutation is returned 
//   in 'permloop'.  The 'region' is updated to reflect the change in 
//   loop order.  The 'loop_ls' must contain all of the array references
//   whose dependences should be updated to reflect the permutation.   
//   The 'est_register_usage' is also speciied because the permutation 
//   may change which loop is innermost. The 'current_depth' is the 
//   depth of the outermost loop in the permutation. 
//-----------------------------------------------------------------------

static WN* SNL_INV_Local_Permute_Loops(SX_PLIST* plist,  
			     DOLOOP_STACK* stack, 
			     INT first_in_stack, 
			     const INT nloops, 
			     const INT permutation[], 
 			     BOOL do_est_register_usage, 
			     EST_REGISTER_USAGE est_register_usage, 
			     WN* permloop[], 
			     BOOL print_lexneg, 
			     SNL_REGION* region, 
			     MEM_POOL* pool)		
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  WN* loop[SNL_MAX_LOOPS];
  INT i;
  for (i = 0; i < nloops; i++)
    loop[i] = stack->Bottom_nth(first_in_stack+i);

  if (!permutation) {
    for (i = 0; i < nloops; i++)
      permloop[i] = loop[i];
    if (do_est_register_usage) { 
      DO_LOOP_INFO* inner_dli = Get_Do_Loop_Info(permloop[nloops-1]);
      inner_dli->Est_Register_Usage = est_register_usage;
    }
    return permloop[0];
  }

  WN*           parents[SNL_MAX_LOOPS];
  WN*           prev[SNL_MAX_LOOPS];
  WN*           body[SNL_MAX_LOOPS];
  DO_LOOP_INFO* dli[SNL_MAX_LOOPS];
  BOOL          is_inner[SNL_MAX_LOOPS];
  INT           depth[SNL_MAX_LOOPS];
  BOOL          is_ivdep[SNL_MAX_LOOPS];

  FmtAssert(Is_Permutation_Vector(permutation, nloops),
            ("Bad permutation vector for SNL_INV_Local_Permute_Loops"));

  for (i = 0; i < nloops; i++) {
    if (permutation[i] != i)
      break;
  }

  INT outerxloop = i; // the first changed loop

  for (i = nloops-1; i >= outerxloop; i--) {
    if (permutation[i] != i)
      break;
  }

  INT innerxloop = i; // the last changed loop

  if (outerxloop >= innerxloop) {
    for (i = 0; i < nloops; i++)
      permloop[i] = loop[i];
    if (do_est_register_usage) { 
      DO_LOOP_INFO* inner_dli = Get_Do_Loop_Info(permloop[nloops-1]);
      inner_dli->Est_Register_Usage = est_register_usage;
    }
    return permloop[0];
  }

  BOOL permutation_transformation_invalid = FALSE;

  // put this before we rewrite the code, to get the good depth right.
  LS_IN_LOOP loop_ls(loop[outerxloop], dg, pool); 

  INT32 num_iter[SNL_MAX_LOOPS];
  for (i = 0; i < nloops; i++) {
    parents[i] = LWN_Get_Parent(loop[i]);
    prev[i] = WN_prev(loop[i]);
    body[i] = WN_do_body(loop[i]);
    WN_do_body(loop[i]) = NULL;
    LWN_Extract_From_Block(parents[i], loop[i]);
    dli[i] = Get_Do_Loop_Info(loop[i]);
    is_inner[i] = dli[i]->Is_Inner;
    depth[i] = dli[i]->Depth;
    is_ivdep[i] = dli[i]->Is_Ivdep;
    if (Cur_PU_Feedback) {
      num_iter[i] = (INT32) MAX(1.0,dli[i]->Est_Num_Iterations);
    }
  }
  
  // The multipliers for the original loop order
  float ratio[SNL_MAX_LOOPS];
  if (Cur_PU_Feedback) {
    ratio[0] = 1.0;
    for (i = 1; i < nloops; i++) {
      ratio[i] = ratio[i-1]*num_iter[i-1];
    }

    // The multipliers for the resulting loop order
    for (i = 0; i < nloops; i++) {
      float new_mul = 1.0;
      for (INT j = 0; j < nloops; j++) {
	if (permutation[j]<permutation[i])
	  new_mul = new_mul*num_iter[j];
      }
      ratio[i] = new_mul/ratio[i];
    }
  }

  for (i = 0; i < nloops; i++) {
    permloop[i] = loop[permutation[i]];
    LWN_Insert_Block_After(parents[i], prev[i], permloop[i]);
    WN_do_body(permloop[i]) = body[i];
    LWN_Set_Parent(body[i], permloop[i]);

    // the annotations get carried along, except for Is_Inner and Depth
    dli[permutation[i]]->Is_Inner = is_inner[i];
    dli[permutation[i]]->Depth = depth[i];
    stack->Bottom_nth(first_in_stack+i) = permloop[i];

    // Is_Ivdep annotation also gets carried along, but we have to reset
    // the annotation if a non-Is_Ivdep is now inside that used to be 
    // outside
    if (dli[permutation[i]]->Is_Ivdep) {
      for (INT ii = i+1; ii < nloops; ii++) {
	if (permutation[i] < permutation[ii] && !is_ivdep[ii]) {
	  dli[permutation[i]]->Is_Ivdep = FALSE;
	  break;
	}
      }
    }
  }

  if (Cur_PU_Feedback) {
    // Adjust the frequency count
    for (i = 0; i < nloops; i++) {
      LWN_Scale_Frequency(loop[i], ratio[i]);
      LWN_Scale_Frequency(WN_start(loop[i]), ratio[i]);
      LWN_Scale_Frequency(WN_step(loop[i]), ratio[i]);
    }
  }

  SNL_Change_Reduction_Loop_Stmts(plist, loop[outerxloop], 
    permloop[outerxloop]);

  if (region->First == loop[0])
    region->First = permloop[0];
  if (region->Last == loop[0])
    region->Last = permloop[0];

  // Update dependence graph.  This is easy: go through the memrefs
  // in the nest.
  // The only edges of adequate nloops will be from nodes in the loop
  // to nodes in the loop.  Thus we just traverse all the out arcs
  // at each vertex, and if the dependence is of adequate nloops,
  // swap elements.

  // If all loops have bad memory references, the depedences do not 
  // need to be updates. 
  for (i = 0; i < stack->Elements(); i++) {
    WN* wn_loop = stack->Bottom_nth(i); 
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 
    if (!dli->Has_Bad_Mem)
      break;
  }
  if (i == stack->Elements())
    return permloop[0];  

  SNL_Expand_Reduction_Deps(loop[outerxloop]);

  LS_IN_LOOP_ITER ali(&loop_ls);
  WN* awn;
  INT curgdepth = loop_ls.Good_Depth - outerxloop;
  while (awn = ali.Step()) {
    VINDEX16 v = dg->Get_Vertex(awn);
    if (v == 0)
      continue;

    INT alex = loop_ls.In(awn);
    EINDEX16 enext = 0;
    for (EINDEX16 e = dg->Get_Out_Edge(v); e; e = enext) {
      enext = dg->Get_Next_Out_Edge(e);
      INT components = dg->Depv_Array(e)->Num_Dim();
      if (components <= curgdepth + outerxloop)
	continue;

      FmtAssert(curgdepth + innerxloop + 1 <= components,
		("dependence vector too short ... imperfect interchange?"));

      // for each vector, permute entries.

      DEP olddep[LNO_MAX_DO_LOOP_DEPTH];
      for (INT i = 0; i < dg->Depv_Array(e)->Num_Vec(); i++) {
	DEPV* d = dg->Depv_Array(e)->Depv(i);
        INT j;
	for (j = outerxloop; j <= innerxloop; j++)
	  olddep[j] = DEPV_Dep(d, j+curgdepth);
	for (j = outerxloop; j <= innerxloop; j++)
	  DEPV_Dep(d, j+curgdepth) = olddep[permutation[j]];
      }

      WN* bwn = dg->Get_Wn(dg->Get_Sink(e));
      INT blex = loop_ls.In(bwn);

      if (SNL_Test_Reduction_Lexneg(e, awn, bwn, alex, blex)) {
	permutation_transformation_invalid = TRUE;
	SNL_DEBUG1(1, "permutation e=%d seemingly illegal\n", e);
      }
    }
  }

  if (do_est_register_usage) {
    DO_LOOP_INFO* inner_dli = Get_Do_Loop_Info(permloop[nloops-1]);
    inner_dli->Est_Register_Usage = est_register_usage;
  }

  if (print_lexneg && permutation_transformation_invalid)
    DevWarn("Permutation transformation invalid?!");

  Is_True(LWN_Check_Parentize(permloop[0]), ("Parentize fail 1"));
  return permloop[0];
}

//-----------------------------------------------------------------------
// NAME: SNL_INV_Permute_Loops 
// FUNCTION: Permute the loops in the nest whose outermost loop is 
//   'outer_loop' according the 'permutation' of length 'nloops'.  Return
//   the new outermost loop. If 'warn_lexneg' and a lexicographically 
//   dependence is produced, print a DevWarn.  Use an invariant permutation
//   technique. 
//-----------------------------------------------------------------------

extern WN* SNL_INV_Permute_Loops(WN* outer_loop,     
		                 INT permutation[], 
		                 INT nloops,
			         BOOL warn_lexneg)  
{
  if (nloops == 0 || Identity_Permutation(permutation, nloops))
    return outer_loop; 

  if (LNO_Verbose) { 
    Print_Interchange(stdout, outer_loop, permutation, nloops); 
    Print_Interchange(TFile, outer_loop, permutation, nloops); 
  } 

  if (LNO_Tlog)
    Print_Interchange(Get_Tlog_File(), outer_loop, permutation, nloops);
  
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  DU_MANAGER* du = Du_Mgr; 
  REDUCTION_MANAGER* rm = red_manager; 
  SNL_REGION region(outer_loop, outer_loop); 
  WN** permloop = CXX_NEW_ARRAY(WN*, nloops, &LNO_local_pool);
  WN* wn = outer_loop; 
  INT i;
  for (i = 1; i < nloops; i++) {
    WN* wn_temp = Find_Next_Innermost_Do(wn);  
    if (wn_temp == NULL) 
      break; 
    wn = wn_temp; 
  }
  WN* inner_loop = wn; 
  DO_LOOP_INFO* dli_outer = Get_Do_Loop_Info(outer_loop);  
  DO_LOOP_INFO* dli_inner = Get_Do_Loop_Info(inner_loop);  
  FmtAssert(dli_inner->Depth - dli_outer->Depth + 1 == nloops, 
    ("SNL_INV_Permute_Loops not passed an SNL."));    
  DOLOOP_STACK* stack = CXX_NEW(DOLOOP_STACK(&LNO_local_pool), 
    &LNO_local_pool); 
  Build_Doloop_Stack(inner_loop, stack); 
  INT first_in_stack = dli_inner->Depth - nloops + 1;
  i = 0; 
  for (i = nloops - 1; i >= 0; i--) 
    if (permutation[i] != i)
      break;
  WN* wn_sink = stack->Bottom_nth(first_in_stack + i);
  for (i = 0; i < nloops; i++) 
    if (permutation[i] != i)
      break;
  WN* wn_outer = stack->Bottom_nth(first_in_stack + i);
  WN* wn_sunk = Sink_Sandwiched_Code_In(wn_outer, wn_sink); 
  wn_outer = SNL_INV_Local_Permute_Loops(NULL, stack, first_in_stack, nloops, 
    permutation, TRUE, EST_REGISTER_USAGE(), permloop, warn_lexneg, 
    &region, &LNO_local_pool); 
  Hoist_Necessary_Code_Up(wn_sunk, du);
  Hoist_Statements(wn_outer, du);  
  Rebuild_Access_Arrays(region, &LNO_local_pool);
  CXX_DELETE_ARRAY(permloop, &LNO_local_pool);  
  return wn_outer; 
} 

//-----------------------------------------------------------------------
// ROUTINES FOR CACHE TILING 
//-----------------------------------------------------------------------

//-----------------------------------------------------------------------
// NAME: SNL_INV_Cache_Block
// FUNCTION: Cache block the innermost 'nloops' loops of the loop nest 
//   described by 'ni'.  Tiling is performed according to the tile_info 
//   't'.  A loop permutation may have been performed on the loops in 
//   'ni', and these loops are stored in the 'nloops' of 'permloop[]'. 
//   The 'loop_ls' gives a list of array references whose dependence 
//   information should be updated.  The 'est_register_usage' is used 
//   to update the information on the innermost loop.  The 'region' is
//   updated to reflect changes in the singly nested loop being transformed.
//-----------------------------------------------------------------------

extern WN* SNL_INV_Cache_Block(SNL_NEST_INFO* ni, 
                               const SNL_TILE_INFO* t,
                               WN* permloop[],
                               LS_IN_LOOP& loop_ls,  
                               SNL_REGION* region,
                               SNL_INV_CACHE_BLOCK_REASON reason, 
			       SYMBOL* outersym, 
                               MEM_POOL* pool)
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  DU_MANAGER* du = Du_Mgr; 
  REDUCTION_MANAGER* rm = red_manager; 
  BOOL cache_transformation_invalid = FALSE;
  INT strips = t ? t->Strips() : 0;
  WN* the_newest_outer_loop = NULL;

  for (INT s = 0; s < strips; s++) {
    WN* olddo = permloop[t->Iloop(s)];
    SYMBOL oldsym(WN_index(olddo));
    oldsym.Type = Do_Wtype(olddo);

    INT newstripsz = t->Stripsz(s);
    INT lvl = t->Striplevel(s);

    if (LNO_Tlog) 
      fprintf(Get_Tlog_File(), "##Blocking: %d \n", Srcpos_To_Line(WN_Get_Linenum(olddo)));
    
    // Decide whether the cache tiling should include wind-down.  It's
    // not necessary when the iteration count divides the trip
    // size.  Also, if we are not register tiling that loop, or if the
    // register tile trip size doesn't divide the cache tile trip size,
    // then just use a MIN rather than winding down, since nothing to gain
    // with the wind down.

    INT64 iters = Iterations(olddo, pool);
    if (iters == 0)
      DevWarn("No iterations in loop: quelle intrigue ...");
    BOOL need_wind_down_or_min = (iters < 0 || iters % newstripsz);
    const INT bufsz = 128;
    char      buf[bufsz];
    INT       bufcnt = 0;

    // create new tile variable
    switch (reason) {
     case SNL_INV_TILE_ONLY:
      bufcnt = sprintf(buf, "$tile%d", lvl);
      break;
     case SNL_INV_SE_ONLY:
      bufcnt = sprintf(buf, "$seonly%d", lvl);
      break;
     case SNL_INV_TILE_SE:
      bufcnt = sprintf(buf, "$setile%d", lvl);
      break;
     case SNL_INV_LEGO_TILE: 
      bufcnt = sprintf(buf, "$dsmtile%d", lvl);
      break;
     case SNL_INV_MP_TILE: 
      bufcnt = sprintf(buf, "$datile%d", lvl);
      break;
     case SNL_INV_DOACROSS_TILE: 
      bufcnt = sprintf(buf, "$doacross%d", lvl);
      break;
     default:
      Is_True(0, ("Impossible"));
    }
    oldsym.Name(buf+bufcnt, bufsz-bufcnt);
    TYPE_ID wtype = oldsym.Type;
    SYMBOL newsym = outersym == NULL ? Create_Preg_Symbol(buf, wtype)
      : *outersym;  

    // do the strip.  The LWN_Copy_Def_Use() here set def use right for
    // the L,U,S part of do istrip = L,U,S.

    WN* stripindex = WN_CreateIdname(newsym.WN_Offset(), newsym.St());

    WN* stripbegin = LWN_Copy_Tree(WN_start(olddo), TRUE, LNO_Info_Map);
    LWN_Copy_Def_Use(WN_kid0(WN_start(olddo)), WN_kid0(stripbegin), du);
    Replace_Symbol(stripbegin, oldsym, newsym, NULL, NULL);
    FmtAssert(WN_operator(stripbegin) == OPR_STID,
	      ("stripbegin is not an stid"));
    FmtAssert(SYMBOL(stripbegin) == newsym,
	      ("stripbegin's symbol is %s", SYMBOL(stripbegin).Name()));
    WN* newsym_alias_wn = stripbegin;

    WN* stripend = LWN_Copy_Tree(WN_end(olddo), TRUE, LNO_Info_Map);
    LWN_Copy_Def_Use(SNL_UBexp(WN_end(olddo)),
		     SNL_UBexp(stripend), du);
    Replace_Symbol(stripend, oldsym, newsym, NULL, NULL);

    WN* stripstep = LWN_Copy_Tree(WN_step(olddo), TRUE, LNO_Info_Map);
    LWN_Copy_Def_Use(WN_kid0(WN_step(olddo)), WN_kid0(stripstep), du);
    Replace_Symbol(stripstep, oldsym, newsym, NULL, NULL);
    Step_Size(stripstep, newstripsz);

    WN* loop0parent = LWN_Get_Parent(permloop[0]);
    WN* loop0prev = WN_prev(permloop[0]);
    LWN_Extract_From_Block(loop0parent, permloop[0]);
    WN* stripbody = WN_CreateBlock();
    LWN_Copy_Linenumber(WN_do_body(olddo), stripbody); 
    LWN_Insert_Block_Before(stripbody, NULL, permloop[0]);

    WN* stripdo = LWN_CreateDO(stripindex, stripbegin, stripend,
			       stripstep, stripbody);
    LWN_Copy_Linenumber(olddo, stripdo);
    if (Cur_PU_Feedback) {
      LWN_Copy_Frequency(stripstep, WN_step(olddo));
      LWN_Scale_Frequency(stripstep, 1.0/newstripsz);
      INT32 outer = WN_MAP32_Get(WN_MAP_FEEDBACK, permloop[0]);
      INT32 outer_trip = WN_MAP32_Get(WN_MAP_FEEDBACK, WN_step(permloop[0]));
      outer_trip = MAX(1,outer_trip/MAX(1,outer));
      LWN_Scale_Frequency(stripstep, 1.0/outer_trip);
      LWN_Copy_Frequency(stripdo, permloop[0]);
      LWN_Copy_Frequency_Tree(stripbegin, permloop[0]);

      INT32 count = WN_MAP32_Get(WN_MAP_FEEDBACK, stripstep);
      INT32 tripc = count/MAX(1,WN_MAP32_Get(WN_MAP_FEEDBACK, stripdo));
      LWN_Set_Frequency(permloop[0],count);
      LWN_Set_Frequency(WN_start(permloop[0]),count);
      LWN_Scale_Frequency(WN_step(permloop[0]), tripc);
      LWN_Scale_Frequency(olddo, tripc);
    }

    if (s == 0)
      the_newest_outer_loop = stripdo;

    LWN_Insert_Block_After(loop0parent, loop0prev, stripdo);

    // in do istrip = L,U,S, the du activity sets the DU for the istrip
    // index variable.
    du->Remove_Def_From_System(WN_start(stripdo));
    du->Remove_Def_From_System(WN_step(stripdo));
    SNL_Change_Du_To_Index_Ldid(stripdo, WN_start(stripdo), du, TRUE);
    SNL_Change_Du_To_Index_Ldid(stripdo, WN_end(stripdo), du, TRUE);
    SNL_Change_Du_To_Index_Ldid(stripdo, WN_step(stripdo), du, TRUE);

    // The stripped loop goes from i' to i'+B-1, but need min(i'+B-1,ub)
    // if not winding_down when need_wind_down_or_min.  The two i' ldids
    // need Du information set, thus the two SNL_Add_Du_to_Index_Ldid().

    TY_IDX ty = Be_Type_Tbl(wtype);

    LWN_Delete_Tree(WN_kid0(WN_start(olddo)));
    WN_kid0(WN_start(olddo)) = 
      LWN_CreateLdid(OPCODE_make_op(OPR_LDID, Promote_Type(wtype), wtype), 
        newsym_alias_wn);
    LWN_Set_Parent(WN_kid0(WN_start(olddo)), WN_start(olddo));
    SNL_Add_Du_To_Index_Ldid(stripdo, WN_kid0(WN_start(olddo)), du, TRUE);
    LWN_Copy_Frequency(WN_start(olddo), olddo);

    Upper_Bound_Standardize(WN_end(olddo));
    WN* oldubval = SNL_UBexp(WN_end(olddo));
    OPCODE opld = OPCODE_make_op(OPR_LDID, Promote_Type(wtype), wtype);
    OPCODE opadd = OPCODE_make_op(OPR_ADD, Promote_Type(wtype), MTYPE_V);
    WN* ldid = LWN_CreateLdid(opld, newsym_alias_wn);
    WN* newub = LWN_CreateExp2(opadd, ldid,
       LWN_Make_Icon(Promote_Type(wtype), newstripsz - 1));

    if (need_wind_down_or_min) {
      OPCODE opmin = OPCODE_make_op(OPR_MIN, Promote_Type(wtype), MTYPE_V);
      newub = LWN_CreateExp2(opmin, newub, oldubval);
    }
    else
      LWN_Delete_Tree(oldubval);

    LWN_Copy_Frequency(newub, WN_step(olddo));
    SNL_UBexp(WN_end(olddo)) = newub;
    LWN_Set_Parent(newub, WN_end(olddo));
    SNL_Add_Du_To_Index_Ldid(stripdo, ldid, du, TRUE);

    // annotate the new loop

    DO_LOOP_INFO* dli = CXX_NEW(DO_LOOP_INFO(&LNO_default_pool,
      NULL, NULL, NULL, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
      &LNO_default_pool);
    DO_LOOP_INFO* olddli = Get_Do_Loop_Info(olddo);
    Set_Do_Loop_Info(stripdo, dli);
    dli->Est_Num_Iterations = (olddli->Est_Num_Iterations +
			       newstripsz - 1)/ newstripsz;
    dli->Is_Ivdep = olddli->Is_Ivdep;
    dli->Is_Concurrent_Call = olddli->Is_Concurrent_Call;
    dli->Concurrent_Directive = olddli->Concurrent_Directive;
    dli->Num_Iterations_Symbolic = TRUE;
    olddli->Est_Num_Iterations = MIN(newstripsz, olddli->Est_Num_Iterations);
    olddli->Is_Inner_Tile = TRUE; 
    olddli->Tile_Size = newstripsz; 
    dli->Is_Outer_Tile = TRUE; 
    dli->Has_Calls = olddli->Has_Calls;
    dli->Has_Unsummarized_Calls = olddli->Has_Unsummarized_Calls;
    dli->Has_Gotos = olddli->Has_Gotos;
    dli->Has_Bad_Mem = olddli->Has_Bad_Mem;

    // Now the wind-down code has dependences, and the tiled code
    // has dependences assuming no tiling -- we need to fix that.
    // But first note that the tiling we have just performed does
    // not change any arcs anywhere except for of dependences from
    // vertices within the tile to vertices within the tile.  So
    // that makes our task reasonable.

    SNL_Expand_Reduction_Deps(the_newest_outer_loop);

    LS_IN_LOOP_ITER ali(&loop_ls);
    WN* awn;
    while (awn = ali.Step()) {

      VINDEX16 v = dg->Get_Vertex(awn);
      if (v == 0)
	continue;

      INT alex = loop_ls.In(awn);
      EINDEX16 enext = 0;
      for (EINDEX16 e = dg->Get_Out_Edge(v); e; e = enext) {
	enext = dg->Get_Next_Out_Edge(e);
	VINDEX16 vsink = dg->Get_Sink(e);
	FmtAssert(vsink, ("vsink cannot be zero"));
	if (!loop_ls.In(dg->Get_Wn(vsink)))
	  continue;

        WN* bwn = dg->Get_Wn(dg->Get_Sink(e));
        INT blex = loop_ls.In(bwn);
        if (SNL_Update_Strip_Dependence(loop_ls.Depth, s, t->Iloop(s),
                                        e, awn, bwn, alex, blex)) {
          cache_transformation_invalid = TRUE;
          SNL_DEBUG1(1, "invariant tiling e=%d seemingly illegal\n", e);
        }
      }
    }
  }

  if (cache_transformation_invalid)
    DevWarn("Cache blocking transformation invalid?!");

  if (the_newest_outer_loop) {
    Is_True(LWN_Check_Parentize(the_newest_outer_loop), ("Parentize fail 1"));
    if (region->First == permloop[0])
      region->First = the_newest_outer_loop;
    if (region->Last == permloop[0])
      region->Last = the_newest_outer_loop;

    if (ni != NULL) 
      SNL_Change_Reduction_Loop_Stmts(&(ni->Privatizability_Info().Plist), 
        permloop[0], the_newest_outer_loop);
  }
  else
    Is_True(LWN_Check_Parentize(permloop[0]), ("Parentize fail 1"));

  if (t)
    Renumber_Loops(region->First, region->Last, dg);
  return the_newest_outer_loop; 
} 

//-----------------------------------------------------------------------
// ROUTINES FOR REGISTER TILING 
//-----------------------------------------------------------------------

//-----------------------------------------------------------------------
// NAME: SNL_INV_Register_Tile 
// FUNCTION: Register tile the 'nloops' innermost loops of the singly nested
//   loop described in 'ni'.  The strip sizes are given in 'regstripsz'. 
//   If a permutation was previously applied to 'ni', the resulting 
//   permuted loops are given in 'permloop'.  The 'est_register_usage'
//   is passed as an estimate of the register usage.  The 'region' and 
//   'the_newest_outer_loop' represent the region over which SNL transfor-  
//   mations are being applied and the new outermost loop in this region. 
//-----------------------------------------------------------------------

static SNL_REGION SNL_INV_Register_Tile(INT nloops,
				  const INT* regstripsz,
				  WN* permloop[],
				  EST_REGISTER_USAGE est_register_usage, 
                                  INT depth_inner,
                                  SX_INFO* pinfo)
{
  SNL_REGION   region(permloop[0], permloop[0]);
  ARRAY_DIRECTED_GRAPH16*       dg = Array_Dependence_Graph;

  if (regstripsz == NULL) {
    if (!Valid_SNL_Region(region))
      DevWarn("SNL_INV_Register_Tile: Invalid SNL_REGION [0x%p,0x%p]",
	region.First, region.Last);
    return region;
  } 

  INT i;
  for (i = 0; i < nloops-1; i++) {
    if (regstripsz[i] <= 1) {
      Is_True(regstripsz[i] == 1, ("Unroll quantity %d", regstripsz[i]));
      continue;
    }

    HASH_TABLE<WN*,WN*>* ht;
    INT depth = depth_inner - nloops + 1 + i;
    SX_INFO* pinfo2;
    SNL_REGION region2 = SNL_Regtile_Loop(permloop[i], regstripsz[i],
                                          nloops-i, FALSE,
                                          est_register_usage,
                                          pinfo, depth, FALSE,
                                          &ht, &pinfo2);
    if (i == 0)
      region = region2;

    // somewhat redundant, but necessary for the further calls to
    // SNL_Regtile_Loop() inside this loop

    Renumber_Loops(region2.First, region2.Last, dg);

    // call this routine on wind-down loops, to get more register
    // unrolling, when conditions are favorable.

    if (LNO_Outer_Unroll_Deep && ht && pinfo2 && i < nloops-2) {
      WN* permloop2[SNL_MAX_LOOPS];
      for (INT ii = i+1; ii < nloops; ii++) {
        permloop2[ii] = ht->Find(permloop[ii]);
        FmtAssert(permloop2[ii], ("Bad mapping"));
      }

      SNL_REGION region2 = SNL_INV_Register_Tile(nloops-i-1,
          regstripsz+i+1, permloop2+i+1, est_register_usage,
          depth_inner, pinfo2);
      if (permloop2[i+1] == region.Last)
        region.Last = region2.Last;
    }

    CXX_DELETE(pinfo2, &SNL_local_pool);
    CXX_DELETE(ht, &SNL_local_pool);
  }

#ifdef Is_True_On
  {
    for (WN *w = region.First; w; w = (w == region.Last) ? NULL : WN_next(w))
      Is_True(LWN_Check_Parentize(w), ("Parentize fail"));
  }
#endif

  Renumber_Loops(region.First, region.Last, dg);

  // Remove loops that, after register tiling, go just once.
  // We could integrate this with SNL_Regtile_Loop, but that complicates
  // this routine ... for example, it messes up privatizability information.
  // Here, privatizability information is no longer needed, so we can mess
  // things up.

  BOOL found_unity = FALSE;
  for (i = 0; i < nloops-1; i++) {
    if (regstripsz[i] > 1 && Iterations(permloop[i], &SNL_local_pool) == 1) {
      found_unity = TRUE;
      WN* removing_loop = permloop[i];
      SNL_REGION region2 = SNL_Remove_Unity_Trip_Loop(removing_loop, FALSE); 
      if (region.First == removing_loop)
        region.First = region2.First;
      if (region.Last == removing_loop)
        region.Last = region2.Last;
    }
  }

  if (found_unity) {
#ifdef Is_True_On
    {
      for (WN *w = region.First; w; w = (w == region.Last) ? NULL : WN_next(w))
        Is_True(LWN_Check_Parentize(w), ("Parentize fail"));
    }
#endif
    Renumber_Loops(region.First, region.Last, dg);
  }

  if (!Valid_SNL_Region(region)) {
    DevWarn("SNL_INV_Register_Tile: Invalid SNL_REGION [0x%p,0x%p]",
      region.First, region.Last);
  } 
  return region;
}

//-----------------------------------------------------------------------
// NAME: SNL_INV_Transforms
// FUNCTION: Transform the SNL of 'nloops' loops by: 
//   (1) Doing scalar expansion and distribution if needed and 
//       'want_se_and_dist' is specified, 
//   (2) Applying the given 'permutation' (which may be NULL), 
//   (3) Applying outer invariant hoisting, if 'hoist_outer_invar' is TRUE
//   (4) Applying cache tiling according to 't' (which may be NULL)
//   (5) Applying register tiling, as specified in 'regstripsz[]'. 
// Relevant information about the nest is given in 'ni'.  The 'old_region' 
//   is the original region over which the transformation is applied,
//   while 'rg_kernel' is set to the kernel of the transformed loops.  
// We return the SNL_REGION represented by the transformed code. 
// NOTES: When bounds are invariant, specify all transformations at once 
//   and do them all.  Simpler, faster compile time, better runtime perfor-
//   mance, than the general case.  Distribution legal, so imperfects not 
//   a problem.  Strategy: do distribution as necessary, put windup-winddown
//   code for caches where useful (to enable efficient register tiling) and
//   necessary (uncooperative iteration count relative to cache size).
//   Permutation[0] = 1 means that the first loop becomes the zero-th.
//   Change the dostack when permuting.
//-----------------------------------------------------------------------

extern SNL_REGION SNL_INV_Transforms(WN* wn_outer, 
				     INT permutation[],
                                     SNL_NEST_INFO* ni,
				     INT nloops,
				     SNL_TILE_INFO* t,
				     INT regstripsz[],
                                     EST_REGISTER_USAGE est_register_usage,
				     BOOL want_se_and_dist, 
				     SNL_REGION* old_region,
				     BOOL hoist_outer_invar,
				     SNL_REGION* rg_kernel) 
{
  INT outer_depth = Do_Loop_Depth(wn_outer); 
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops); 
  DOLOOP_STACK* stack = CXX_NEW(DOLOOP_STACK(&LNO_local_pool), 
    &LNO_local_pool);  
  Build_Doloop_Stack(wn_inner, stack); 
  
  INT first_in_stack = Do_Loop_Depth(wn_inner) - nloops + 1;
  WN* loop0 = stack->Bottom_nth(first_in_stack); 
  
  SNL_REGION region(loop0, loop0);
  if (old_region != NULL) {
    region.First = old_region->First; 
    region.Last = old_region->Last; 
  }
  MEM_POOL_Push_Freeze(&SNL_local_pool);
  WN* the_newest_outer_loop = loop0; 

  {
    // Generic asserts. 

    FmtAssert(t == NULL || t->Rectangular(), ("Requires rectangular tiling"));
    // TODO: if t->Nloops() is smaller, just means tiling further inside,
    // which should be ok.
    FmtAssert(t == NULL || nloops == t->Nloops(), ("Bad nloops"));

    // Step 1: Distribute as necessary, if necessary.  For register blocking
    // only, we don't need to distribute.  Otherwise, go for it.  But before
    // distributing, we need to scalar expand.

    BOOL se_introduces_lcd = FALSE; 
    if (want_se_and_dist) {
      BOOL distribution_applied = FALSE;
      BOOL has_lcd = ni->Privatizability_Info().Lcd_Depth() != -1; 
      if (!has_lcd && !Get_Trace(TP_LNOPT, TT_LNO_BIG_SCALAR_TILES)) {
        distribution_applied = SNL_INV_Limited_Scalar_Expand_And_Distribute(
          loop0, &t, permutation, nloops, &ni->Privatizability_Info().Plist,
          &region, &the_newest_outer_loop);
      }
      if (distribution_applied == FALSE) {
        SNL_INV_Scalar_Expand(loop0, permutation, nloops, 
          &ni->Privatizability_Info().Plist, -1, NULL, FALSE, TRUE); 
	// So now distribution is legal and should be applied.
	SNL_INV_Distribution(loop0, nloops, has_lcd, &region, 
	  &the_newest_outer_loop);
      }
    }

    // Step 2: Apply permutation.  With invariant bounds, this is trivial:
    // just rearrange the loop nodes.  Don't have to change any DU
    // information, except possibly loop carried information.
    // Have to swap entries in dependences, though.  We do
    // update the DO_LOOP_INFO.

    WN* permloop[SNL_MAX_LOOPS];
    SX_PLIST* plist = &(ni->Privatizability_Info().Plist);  
    if (Cur_PU_Feedback)
      LNO_FB_Inv_Interchange(stack->Bottom_nth(first_in_stack),
        permutation, nloops);
    WN* outer_perm_loop = SNL_INV_Local_Permute_Loops(plist, stack, 
      first_in_stack, nloops, permutation, TRUE, est_register_usage, permloop, 
      TRUE, &region, &SNL_local_pool);
    if (outer_perm_loop != NULL && the_newest_outer_loop != NULL  
      && Do_Loop_Depth(outer_perm_loop) < Do_Loop_Depth(the_newest_outer_loop))
      the_newest_outer_loop = outer_perm_loop;
    if (rg_kernel->First == loop0 && rg_kernel->First != outer_perm_loop) 
      rg_kernel->First = outer_perm_loop; 
    if (rg_kernel->Last == loop0 && rg_kernel->Last != outer_perm_loop) 
      rg_kernel->Last = outer_perm_loop; 
     

    if (!se_introduces_lcd && permutation != NULL 
	&& !Identity_Permutation(permutation, nloops)) 
      SNL_INV_Permute_Distributends(&region, permutation, nloops, 
        Array_Dependence_Graph, Du_Mgr, red_manager);

    // Step 2.5: Find all array nodes and put them in a structure.
    LS_IN_LOOP loop_ls(permloop[0], Array_Dependence_Graph, &SNL_local_pool);

    // Step 3: Cache block.
    // Updating DU information is easy when there is no wind-down.

    WN* outer_cache_loop = SNL_INV_Cache_Block(ni, t, permloop, 
      loop_ls, &region, SNL_INV_TILE_ONLY, NULL, &SNL_local_pool);
    if (outer_cache_loop != NULL && the_newest_outer_loop != NULL
        && Do_Loop_Depth(outer_cache_loop) 
	< Do_Loop_Depth(the_newest_outer_loop)) {
      the_newest_outer_loop = outer_cache_loop; 
    }

    // Step 3.47: Hoist outer invariants
    // First, find the inner loop
    BOOL did_reg_tile_and_hoist=FALSE;
    WN *hoist_inner;
    if (hoist_outer_invar) {
      Rebuild_Access_Arrays(region,&SNL_local_pool);
      hoist_inner = stack->Bottom_nth(first_in_stack);
      INT depth = Do_Loop_Depth(hoist_inner);
      INT i;
      for (i=first_in_stack+1; i<stack->Elements(); i++) {
        WN *wn = stack->Bottom_nth(i);
        if (Do_Loop_Depth(wn) > depth) {
	  hoist_inner = wn;
	  depth = Do_Loop_Depth(wn);
        }
      }
      INT outer_reg_tile=nloops;
      BOOL done=FALSE;
      if (regstripsz) {
        for (i=0; i<nloops-1 && !done; i++) {
	  if (regstripsz[i] > 1) {
	    outer_reg_tile = i;
	    done = TRUE;
          }
        }
      }
      did_reg_tile_and_hoist = done;
      WN *outer = hoist_inner;
      for (i=0; i<nloops-1; i++) {
	outer = LWN_Get_Parent(LWN_Get_Parent(outer));  
      }
      WN *one_before_outer = WN_prev(outer);
      WN *one_after_outer = WN_next(outer);
      WN *parent = LWN_Get_Parent(outer);
      void Hoist_Outer_Invar(WN *hoist_inner, INT nloops, INT outer_reg_tile, 
        BOOL can_tile);
      Hoist_Outer_Invar(hoist_inner,nloops,outer_reg_tile,TRUE);
      if (region.First == outer) {
	if (one_before_outer) region.First = WN_next(one_before_outer);
	else region.First = WN_first(parent);
      }
      if (region.Last == outer) {
	if (one_after_outer) region.Last = WN_prev(one_after_outer);
	else region.Last = WN_last(parent);
      }
    }
 
    // step 4: register tiling.  Each loop that has been stripped is a perfect
    // multiple and we only need adjust the step and duplicate the body.
    // For unstripped loops, we also need wind-down code.
    // NOTE: In theory, the wind-down code could be regblocked via a recursive
    // call to this routine.  For now, let's not do that.  It only matters
    // on loops of nloops 3 or more.

    ni->Privatizability_Info().Update_Reduction_Loop_Stmts(wn_inner); 
    SNL_REGION region2 = SNL_INV_Register_Tile(nloops, regstripsz, permloop,
      est_register_usage, ni->Depth_Inner(), &ni->Privatizability_Info()); 
    if (rg_kernel->First == permloop[0] && region2.First != rg_kernel->First)
      rg_kernel->First = region2.First; 
    if (rg_kernel->Last == permloop[0] && region2.Last != rg_kernel->Last)
      rg_kernel->Last = region2.Last; 
    if (region.First == permloop[0])
      region.First = region2.First;
    if (region.Last == permloop[0])
      region.Last = region2.Last;

    if (did_reg_tile_and_hoist) {  // hoist the inner invariants
      void Hoist_Outer_Invar(WN *wn_inner, INT nloops, INT outer_reg_tile, 
        BOOL can_tile);
      Hoist_Outer_Invar(hoist_inner,1,1,TRUE);
    }

    // step 5: rebuild access arrays for all the entire region.  If we want
    // to rebuild for less, watch out for the sanity tests.  The Unity
    // stuff will the sanity tests.

    Rebuild_Access_Arrays(region, &SNL_local_pool);
#ifdef Is_True_On
    for (WN* w = region.First; w; w = w==region.Last ? NULL : WN_next(w))
      Is_True(LWN_Check_Parentize(w), ("Check Parentize fails"));
#endif
  }

  MEM_POOL_Pop_Unfreeze(&SNL_local_pool);
  if (!Valid_SNL_Region(region)) 
    DevWarn("SNL_Invariant_Transforms: Invalid SNL_REGION [0x%p,0x%p]",
      region.First, region.Last);
  return region;
}

    
