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
*** This file contains some of the code to implement the general SNL
*** transformations.  These are the transformations that operate on SNLs
*** which have one or more loop bounds which are not invariant with respect 
*** to the entire loop nest OR whch have an imperfect part which is not 
*** always distributable.
***/

#include <stdint.h>
#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#define snl_CXX      "snl.cxx"
const static char *rcs_id =   snl_CXX "$Revision: 1.9 $";

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
#include "ipa_lno_util.h"
#include "wn_simp.h"

//-----------------------------------------------------------------------
// ROUTINES FOR PEELING A SINGLE ITERATION OFF AN SNL 
//-----------------------------------------------------------------------

struct SNL_NEWINFO {
  VINDEX16	Vold;
  INT		Lex;
  SNL_NEWINFO(VINDEX16 v, INT l) : Vold(v), Lex(l) {}
  SNL_NEWINFO(INT l) : Vold(0), Lex(l) {FmtAssert(l == 0, ("Bad newinfo"));}
  SNL_NEWINFO() : Vold(0), Lex(0) {}
};

//-----------------------------------------------------------------------
// NAME: Build_New_To_Old
// FUNCTION: Build a mapping between the nodes which have dependence ver- 
//   tices in the tree rooted at 'orig' and the tree rooted at 'copy'. 
//   The mapping is stored in the hash table 'new2old'.  The last lexical 
//   position 'lex' is updated by this routine.  If it was not possible 
//   to construct the mapping, the function returns FALSE. 
// NOTE: The copy and the original match except when the copy is missing a
//   DO_LOOP at the outermost level.  The complicated iteration code below
//   detects this case and compensates for it.  
//-----------------------------------------------------------------------
 
static BOOL Build_New_To_Old(WN* orig,
			  WN* copy,
			  ARRAY_DIRECTED_GRAPH16* dg,
			  HASH_TABLE<VINDEX16, SNL_NEWINFO>* new2old,
			  INT& lex)
{
  FmtAssert(orig != NULL && copy != NULL, ("Should catch at higher level.")); 
  FmtAssert(WN_opcode(orig) != OPCODE_UNKNOWN &&
            WN_opcode(copy) != OPCODE_UNKNOWN, 
            ("Should catch at higher level.")); 
  if (OPCODE_is_load(WN_opcode(orig)) || OPCODE_is_store(WN_opcode(orig)) ||
      OPCODE_is_call(WN_opcode(orig))) {
    VINDEX16 origv = dg->Get_Vertex(orig);
    if (origv) {
      VINDEX16 newv = dg->Add_Vertex(copy);
      if (!newv)
	return 0;
      new2old->Enter(newv, SNL_NEWINFO(origv,++lex));
    }
  }

  if (WN_opcode(orig) == OPC_BLOCK) {
    WN *orig_kid = WN_first(orig);
    WN *copy_kid = WN_first(copy); 
    while (orig_kid != NULL && WN_opcode(orig_kid) == OPC_DO_LOOP
      && (copy == NULL || WN_opcode(orig_kid) != WN_opcode(copy))) 
      orig_kid = WN_next(orig_kid);   
    while (orig_kid != NULL) {
      if (!Build_New_To_Old(orig_kid, copy_kid, dg, new2old, lex))
	return 0;
      orig_kid = WN_next(orig_kid);
      copy_kid = WN_next(copy_kid);
      while (orig_kid != NULL && WN_opcode(orig_kid) == OPC_DO_LOOP
	&& (copy == NULL || WN_opcode(orig_kid) != WN_opcode(copy))) 
	orig_kid = WN_next(orig_kid);   
    }
  } else {
    for (INT kidno=0; kidno<WN_kid_count(orig); kidno++) {
      if (!Build_New_To_Old(WN_kid(orig,kidno), WN_kid(copy,kidno), dg,
			    new2old, lex))
	return 0;
    }
  }
  return 1;
}

//-----------------------------------------------------------------------
// NAME: SNL_Peel_Iteration
// FUNCTION: Peel an iteration off either the front or back of 'wn'.  If 
//   'first_iter' is TRUE, peel the first iteration and place it in front
//   of the loop, otherwise peel off the last iteration and place it after
//   the loop.  
//-----------------------------------------------------------------------

extern void SNL_Peel_Iteration(WN* wn,
			       BOOL first_iter)
{

// Peel last iteration of this loop.  The hard part is updating dependences
// efficiently.  Here's how I'll do this:  First, get a map of all the new
// vertices to all the old ones and of all the old ones to all the new ones.
// Note that some old ones are in the loop but don't have a corresponding new
// one if loops_go_zero is true.  In those cases, give the value -1.

// So now iterate through the new nodes.  For each arc entering or leaving
// the corresponding old node, if the other end new, then we ignore it --
// it's added in this phase and doesn't concern us.  Otherwise, we add arcs
// replacing the old node with the new one, and shortenting the dependence
// accordingly.  This is not very accurate, because it does not take into
// account last iteration information.  That doesn't matter in the current
// implementation, when the outermost loop only is peeled, but in other cases
// it could matter a lot.  In theory, instead of shortening all the
// dependences, we could recompute them.  That's probably cheap, since we
// already know all the arcs along which a dependence could occur, which
// shouldn't be that many.  Well, having written the above, I realize that's
// plausible, but to be general, I'm just going to recompute the dependences.
// Also, note, though, that the dependences within the nest may have changed
// since the bounds have tightened up.  Not touching them is conservative and
// almost always adequate, and I'm going to guess that it's too expensive and
// unprofitable to recompute them.

  ARRAY_DIRECTED_GRAPH16*	dg = Array_Dependence_Graph;
  DU_MANAGER*			du = Du_Mgr;
  DO_LOOP_INFO*			dli = Get_Do_Loop_Info(wn);

  // Set up a potential guard test 
  WN* wn_total_cond = LWN_Make_Icon(Boolean_type, 1);
  WN* wn_if = LWN_CreateIf(wn_total_cond, WN_CreateBlock(), WN_CreateBlock());
  if (first_iter) 
    LWN_Insert_Block_Before(LWN_Get_Parent(wn), wn, wn_if);
  else 
    LWN_Insert_Block_After(LWN_Get_Parent(wn), wn, wn_if);
  WN_Set_Linenum(wn_if, WN_Get_Linenum(wn));
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn, 2); 
  DO_LOOP_INFO* dli_inner = Get_Do_Loop_Info(wn_inner); 
  IF_INFO *ii =
    CXX_NEW(IF_INFO(&LNO_default_pool, FALSE, FALSE), &LNO_default_pool);
  WN_MAP_Set(LNO_Info_Map, wn_if, (void *) ii);
  DOLOOP_STACK *if_stack = CXX_NEW(DOLOOP_STACK(&LNO_default_pool),
    &LNO_default_pool);
  Build_Doloop_Stack(wn_if, if_stack);
  LNO_Build_If_Access(wn_if, if_stack);
  COND_BOUNDS_INFO *info =
    CXX_NEW(COND_BOUNDS_INFO(&LNO_local_pool), &LNO_local_pool);
  info->Collect_Outer_Info(LWN_Get_Parent(wn_if));
  WN* wn_test = LWN_Copy_Tree(WN_end(wn), TRUE, LNO_Info_Map);
  LWN_Copy_Def_Use(WN_end(wn), wn_test, Du_Mgr);
  Replace_Ldid_With_Exp_Copy(SYMBOL(WN_start(wn)), wn_test,
    WN_kid0(WN_start(wn)), Du_Mgr);
  if (Redundant_Condition(info, wn_test, wn_if)) {
    LWN_Delete_Tree(wn_if); 
    wn_if = NULL; 
  } else { 
    Replace_Wnexp_With_Exp_Copy(WN_if_test(wn_if), wn_test, du);  
  } 
  LWN_Delete_Tree(wn_test);

  WN* newblock = LWN_Copy_Tree(WN_do_body(wn), TRUE, LNO_Info_Map);
  LWN_Set_Frequency_Tree(newblock,1);
  LWN_Adjust_Frequency_Tree(WN_do_body(wn), -1);
  LWN_Adjust_Frequency(WN_step(wn), -1);

  WN* tmp_unrolls[2];
  tmp_unrolls[0] = WN_do_body(wn);
  tmp_unrolls[1] = newblock;

  if (red_manager != NULL) red_manager->Unroll_Update(tmp_unrolls, 2);
  // dli->Depth is the loop further out, so don't need to subtract 1
  Unrolled_DU_Update(tmp_unrolls, 2, dli->Depth, TRUE, FALSE);
  WN *w;
  for (w = WN_first(newblock); w; w = WN_next(w)) {
    if (WN_opcode(w) == OPC_DO_LOOP)
      LWN_Delete_Tree(LWN_Extract_From_Block(w));
  }

  // insert in newblock, but using value of upper/lower bound.  Also
  // adjust bound in wn.

  if (first_iter) {
    Replace_Ldid_With_Exp_Copy(WN_index(wn), newblock,
			       WN_kid0(WN_start(wn)), du);
    Increase_By(WN_kid0(WN_start(wn)), 1, WN_start(wn), 0);
    Is_True(dli->LB->Num_Vec() == 1, ("Bug in Peel_Iteration()"));
    dli->LB->Dim(0)->Const_Offset++;
  }
  else {
    Upper_Bound_Standardize(WN_end(wn));
    Replace_Ldid_With_Exp_Copy(WN_index(wn), newblock,
			       SNL_UBexp(WN_end(wn)), du);
    Increase_By(SNL_UBexp(WN_end(wn)), -1, WN_end(wn));
    Is_True(dli->UB->Num_Vec() == 1, ("Bug in Peel_Iteration()"));
    dli->UB->Dim(0)->Const_Offset--;
  }
  if (dli->Est_Num_Iterations > 0)
    dli->Est_Num_Iterations--;

  // Basic dependence information about peeled references ... use before
  // newblock is destroyed.  But first, if this is an outermost loop that
  // we are unpeeling, then there are no dependences to add.

  if (Get_Do_Loop_Info(wn)->Depth == 0) {
    WN* first = WN_first(newblock);
    WN* last = WN_last(newblock);
    if (wn_if != NULL) { 
      LWN_Insert_Block_After(WN_then(wn_if), NULL, newblock); 	
    } else { 
      if (first_iter)
	LWN_Insert_Block_Before(LWN_Get_Parent(wn), wn, newblock);
      else
	LWN_Insert_Block_After(LWN_Get_Parent(wn), wn, newblock);
    } 
    WN* wn_next = NULL; 
    for (WN* wn_temp = first; wn_temp != NULL; wn_temp = wn_next) {
      wn_next = WN_next(wn_temp);  
      Remove_Redundant_Stids(wn_temp, du); 
      if (wn_temp == last) 
	break;
    }
    return;
  }

  MEM_POOL_Push(&SNL_local_pool);

  BOOL disaster = FALSE;
  INT lex = 0;

  typedef HASH_TABLE<VINDEX16, SNL_NEWINFO> VSNL_HASH_TABLE;
  VSNL_HASH_TABLE 
    *new2old = CXX_NEW(VSNL_HASH_TABLE(43,&SNL_local_pool),&SNL_local_pool);
  HASH_TABLE_ITER<VINDEX16,SNL_NEWINFO>	iter(new2old);
  WN* first = WN_first(newblock);
  WN* last = WN_last(newblock);
  if (!Build_New_To_Old(WN_do_body(wn), newblock, dg, new2old, lex)) {
    disaster = TRUE;
    goto disaster;
  }

  {
    if (wn_if != NULL) { 
      LWN_Insert_Block_After(WN_then(wn_if), NULL, newblock); 	
    } else { 
      if (first_iter)
	LWN_Insert_Block_Before(LWN_Get_Parent(wn), wn, newblock);
      else
	LWN_Insert_Block_After(LWN_Get_Parent(wn), wn, newblock);
    } 

    for (w = first; w; w = WN_next(w)) {
      DOLOOP_STACK dostack(&LNO_local_pool);
      Build_Doloop_Stack(LWN_Get_Parent(wn), &dostack); 
      LNO_Build_Access(w, &dostack, &LNO_default_pool);
      if (w == last)
	break;
    }
    FmtAssert(w == last, ("Bug in Peel_iteration()"));

    // Dependence graph update


    INT count = 0;

    VINDEX16				vnew;
    SNL_NEWINFO				newinfo;
    while (iter.Step(&vnew, &newinfo)) {
      HASH_TABLE<WN*,INT> old_nodes(MIN(dg->Get_Edge_Count(), 512),
	&SNL_local_pool);
      INT lex = newinfo.Lex;
      VINDEX16 vold = newinfo.Vold;
      Is_True(vold > 0, ("Missing vold vertex"));
      WN* wnnew = dg->Get_Wn(vnew);
      DOLOOP_STACK stack(&SNL_local_pool);
      Build_Doloop_Stack(wnnew, &stack);

      // from new stuff to anywhere that preexisted

      EINDEX16 e;
      for (e = dg->Get_Out_Edge(vold); e; e = dg->Get_Next_Out_Edge(e)) {
	VINDEX16 vsink = dg->Get_Sink(e);
	Is_True(vsink, ("Sink has null vertex"));

	if (new2old->Find(vsink).Vold != 0)	// copy only old arcs
	  continue;

	WN* own = dg->Get_Wn(vsink);
	old_nodes.Enter(own, 1); 
	DOLOOP_STACK ostack(&SNL_local_pool);
	Build_Doloop_Stack(own, &ostack);

	count++;
	BOOL ok = dg->Add_Edge( wnnew, &stack, own, &ostack, first_iter);
	if (!ok) {
	  LNO_Erase_Dg_From_Here_In(wnnew, dg);
	  LNO_Erase_Dg_From_Here_In(own, dg);
	  disaster = TRUE;
	  goto disaster;
	}
      }

      // to new stuff from anywhere that preexisted

      for (e = dg->Get_In_Edge(vold); e; e = dg->Get_Next_In_Edge(e)) {
	VINDEX16 vsource = dg->Get_Source(e);
	Is_True(vsource, ("Source has null vertex"));

	if (new2old->Find(vsource).Vold != 0)	// copy only old arcs
	  continue;

	WN* own = dg->Get_Wn(vsource);
	if (old_nodes.Find(own) != 0) 
	  continue; 
	DOLOOP_STACK ostack(&SNL_local_pool);
	Build_Doloop_Stack(own, &ostack);

	count++;
	BOOL ok = dg->Add_Edge( wnnew, &stack, own, &ostack, first_iter);
	if (!ok) {
	  LNO_Erase_Dg_From_Here_In(wnnew, dg);
	  LNO_Erase_Dg_From_Here_In(own, dg);
	  disaster = TRUE;
	  goto disaster;
	}
      }
    }

    // now add arcs from new to new

    {
      HASH_TABLE_ITER<VINDEX16,SNL_NEWINFO> iter(new2old);
      VINDEX16				vnew;
      SNL_NEWINFO				newinfo;
      while (iter.Step(&vnew, &newinfo)) {
	INT lex = newinfo.Lex;
	WN* wnnew = dg->Get_Wn(vnew);
	if (OPCODE_is_load(WN_opcode(wnnew)))
	  continue;

	DOLOOP_STACK stack(&SNL_local_pool);
	Build_Doloop_Stack(wnnew, &stack);

	HASH_TABLE_ITER<VINDEX16,SNL_NEWINFO> iter2(new2old);
	VINDEX16				vnew2;
	SNL_NEWINFO			newinfo2;
	while (iter2.Step(&vnew2, &newinfo2)) {
	  INT lex2 = newinfo2.Lex;
	  WN* wnnew2 = dg->Get_Wn(vnew2);
	  BOOL is_load = OPCODE_is_load(WN_opcode(wnnew2));

	  if (!is_load && lex < lex2)		// redundant
	    continue;

	  count++;
	  BOOL ok = dg->Add_Edge( wnnew, &stack, wnnew2, &stack, lex < lex2);
	  if (!ok) {
	    LNO_Erase_Dg_From_Here_In(WN_kid1(wnnew), dg);
	    LNO_Erase_Dg_From_Here_In(WN_kid(wnnew2,is_load?0:1), dg);
	    disaster = TRUE;
	    goto disaster;
	  }
	}
      }
    }

    SNL_DEBUG1(1, "Peeling added %d edges\n", count);
  } 

 disaster:
  if (disaster) {
    SNL_DEBUG0(0, "Ran out of edges in peeling");
    HASH_TABLE_ITER<VINDEX16,SNL_NEWINFO> iter(new2old);
    VINDEX16		v;
    SNL_NEWINFO		dummy;
    while (iter.Step(&v, &dummy)) {
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
    }
  }

  WN* wn_next = NULL; 
#ifdef KEY
  // Bug 5489 - If the only statement inside the DO_LOOP 
  // is another DO_LOOP then, first = last = NULL because 
  // DO_LOOPs are already deleted out of new_block.
  for (WN* wn_temp = first; wn_temp; wn_temp = wn_next) {
#else
  for (WN* wn_temp = first; ; wn_temp = wn_next) {
#endif
    wn_next = WN_next(wn_temp);  
    Remove_Redundant_Stids(wn_temp, du); 
    if (wn_temp == last) 
      break;
  }
  MEM_POOL_Pop(&SNL_local_pool);
} 

static void Cleanup_For_Inner_Loop_Peeling(WN *body, WN *current_stmt, 
                                           WN *match_stmt, BOOL clear_match)
{
  // Now cleanup both the code and the dependence graph for
  // the removed if-test in the loop and the
  // match_stmt in newblock using the appropriate routines.
  if (match_stmt) {
    // In either case, always propagate the
    // contents of the then block back into the existing loop.
    WN *stmt = WN_first(WN_then(current_stmt));
    while (stmt) {
      WN *next = WN_next(stmt);
      LWN_Insert_Block_Before(body, current_stmt,
                              LWN_Extract_From_Block(stmt));
      stmt = next;
    }
    LWN_Delete_Tree(current_stmt);
    if (clear_match) {
      LWN_Delete_LNO_dep_graph(match_stmt);
      LWN_Delete_Tree(match_stmt);
    }
  }
}

//-----------------------------------------------------------------------
// NAME: SNL_Peel_Iteration_Inner
// FUNCTION: Peel an iteration off either the front or back of 'wn'.  If 
//   'first_iter' is TRUE, peel the first iteration and place it in front
//   of the loop, otherwise peel off the last iteration and place it after
//   the loop.  
//-----------------------------------------------------------------------

extern void SNL_Peel_Iteration_Inner(WN* wn,
                                     WN *stmt,
                                     BOOL exclusion,
			             BOOL first_iter)
{

// Peel last steration of this loop.  The hard part is updating dependences
// efficiently.  Here's how I'll do this:  First, get a map of all the new
// vertices to all the old ones and of all the old ones to all the new ones.
// Note that some old ones are in the loop but don't have a corresponding new
// one if loops_go_zero is true.  In those cases, give the value -1.

// So now iterate through the new nodes.  For each arc entering or leaving
// the corresponding old node, if the other end new, then we ignore it --
// it's added in this phase and doesn't concern us.  Otherwise, we add arcs
// replacing the old node with the new one, and shortenting the dependence
// accordingly.  This is not very accurate, because it does not take into
// account last iteration information.  That doesn't matter in the current
// implementation, when the innermost loop only is peeled, so we
// recompute the dependences to be safe.  That's probably cheap, since we
// already know all the arcs along which a dependence could occur, which
// shouldn't be that many.  

  ARRAY_DIRECTED_GRAPH16*	dg = Array_Dependence_Graph;
  DU_MANAGER*			du = Du_Mgr;
  DO_LOOP_INFO*			dli = Get_Do_Loop_Info(wn);

  // No guard test is needed, our criteria is that the loop is initialized
  // with a positive trip and that via the transformation test the stride is 1.
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn, 1); 
  DO_LOOP_INFO* dli_inner = Get_Do_Loop_Info(wn_inner); 

  WN *loop_body = WN_do_body(wn);
  WN* newblock = LWN_Copy_Tree(loop_body, TRUE, LNO_Info_Map);
  LWN_Set_Frequency_Tree(newblock,1);
  LWN_Adjust_Frequency_Tree(WN_do_body(wn), -1);
  LWN_Adjust_Frequency(WN_step(wn), -1);

  // Now walk newblock and find the match to our stmt, then remove it
  // as the neither the first or the last iteration will need the copy
  // as it was proven to be excluded.
  WN *cur, *match_stmt;
  match_stmt = NULL;
  int num_stmts = 0;
  for (cur = WN_first(newblock); cur; cur = WN_next(cur)) {
    if (WN_opcode(cur) == OPC_IF) {
      WN *stmt_if = WN_if_test(stmt);
      WN *cur_if = WN_if_test(cur);
      if ((match_stmt == NULL) && 
          (WN_Simp_Compare_Trees(stmt_if, cur_if) == 0)) {
        match_stmt = cur;
      }
    }
    num_stmts++;
  }

  // insert in newblock, but using value of upper/lower bound.  Also
  // adjust bound in wn.

  if (first_iter) {
    Replace_Ldid_With_Exp_Copy(WN_index(wn), newblock,
			       WN_kid0(WN_start(wn)), du);
    Increase_By(WN_kid0(WN_start(wn)), 1, WN_start(wn), 0);
    Is_True(dli->LB->Num_Vec() == 1, ("Bug in Peel_Iteration()"));
    dli->LB->Dim(0)->Const_Offset++;
  }
  else {
    Upper_Bound_Standardize(WN_end(wn));
    Replace_Ldid_With_Exp_Copy(WN_index(wn), newblock,
			       SNL_UBexp(WN_end(wn)), du);
    Increase_By(SNL_UBexp(WN_end(wn)), -1, WN_end(wn));
    Is_True(dli->UB->Num_Vec() == 1, ("Bug in Peel_Iteration()"));
    dli->UB->Dim(0)->Const_Offset--;
  }
  if (dli->Est_Num_Iterations > 0)
    dli->Est_Num_Iterations--;

  // check to see if there are other stmts in newblock before adding it
  if ((num_stmts == 1) && (match_stmt != NULL)) {
    Cleanup_For_Inner_Loop_Peeling(loop_body, stmt, match_stmt, FALSE);
    LWN_Delete_Tree(newblock);
    return;
  }

  WN* tmp_unrolls[2];
  tmp_unrolls[0] = WN_do_body(wn);
  tmp_unrolls[1] = newblock;

  if (red_manager != NULL) red_manager->Unroll_Update(tmp_unrolls, 2);
  // dli->Depth is the loop further out, so don't need to subtract 1
  Unrolled_DU_Update(tmp_unrolls, 2, dli->Depth, TRUE, FALSE);

  MEM_POOL_Push(&SNL_local_pool);

  BOOL disaster = FALSE;
  INT lex = 0;

  typedef HASH_TABLE<VINDEX16, SNL_NEWINFO> VSNL_HASH_TABLE;
  VSNL_HASH_TABLE 
    *new2old = CXX_NEW(VSNL_HASH_TABLE(43,&SNL_local_pool),&SNL_local_pool);
  HASH_TABLE_ITER<VINDEX16,SNL_NEWINFO>	iter(new2old);
  if (!Build_New_To_Old(WN_do_body(wn), newblock, dg, new2old, lex)) {
    disaster = TRUE;
    goto disaster;
  }

  {
    if (first_iter)
      LWN_Insert_Block_Before(LWN_Get_Parent(wn), wn, newblock);
    else
      LWN_Insert_Block_After(LWN_Get_Parent(wn), wn, newblock);

    // Dependence graph update

    INT count = 0;
    VINDEX16				vnew;
    SNL_NEWINFO				newinfo;
    while (iter.Step(&vnew, &newinfo)) {
      HASH_TABLE<WN*,INT> old_nodes(MIN(dg->Get_Edge_Count(), 512),
	&SNL_local_pool);
      INT lex = newinfo.Lex;
      VINDEX16 vold = newinfo.Vold;
      Is_True(vold > 0, ("Missing vold vertex"));
      WN* wnnew = dg->Get_Wn(vnew);
      DOLOOP_STACK stack(&SNL_local_pool);
      Build_Doloop_Stack(wnnew, &stack);

      // from new stuff to anywhere that preexisted

      EINDEX16 e;
      for (e = dg->Get_Out_Edge(vold); e; e = dg->Get_Next_Out_Edge(e)) {
	VINDEX16 vsink = dg->Get_Sink(e);
	Is_True(vsink, ("Sink has null vertex"));

	if (new2old->Find(vsink).Vold != 0)	// copy only old arcs
	  continue;

	WN* own = dg->Get_Wn(vsink);
	old_nodes.Enter(own, 1); 
	DOLOOP_STACK ostack(&SNL_local_pool);
	Build_Doloop_Stack(own, &ostack);

	count++;
	BOOL ok = dg->Add_Edge( wnnew, &stack, own, &ostack, first_iter);
	if (!ok) {
	  LNO_Erase_Dg_From_Here_In(wnnew, dg);
	  LNO_Erase_Dg_From_Here_In(own, dg);
	  disaster = TRUE;
	  goto disaster;
	}
      }

      // to new stuff from anywhere that preexisted

      for (e = dg->Get_In_Edge(vold); e; e = dg->Get_Next_In_Edge(e)) {
	VINDEX16 vsource = dg->Get_Source(e);
	Is_True(vsource, ("Source has null vertex"));

	if (new2old->Find(vsource).Vold != 0)	// copy only old arcs
	  continue;

	WN* own = dg->Get_Wn(vsource);
	if (old_nodes.Find(own) != 0) 
	  continue; 
	DOLOOP_STACK ostack(&SNL_local_pool);
	Build_Doloop_Stack(own, &ostack);

	count++;
	BOOL ok = dg->Add_Edge( wnnew, &stack, own, &ostack, first_iter);
	if (!ok) {
	  LNO_Erase_Dg_From_Here_In(wnnew, dg);
	  LNO_Erase_Dg_From_Here_In(own, dg);
	  disaster = TRUE;
	  goto disaster;
	}
      }
    }

    // now add arcs from new to new

    {
      HASH_TABLE_ITER<VINDEX16,SNL_NEWINFO> iter(new2old);
      VINDEX16				vnew;
      SNL_NEWINFO				newinfo;
      while (iter.Step(&vnew, &newinfo)) {
	INT lex = newinfo.Lex;
	WN* wnnew = dg->Get_Wn(vnew);
	if (OPCODE_is_load(WN_opcode(wnnew)))
	  continue;

	DOLOOP_STACK stack(&SNL_local_pool);
	Build_Doloop_Stack(wnnew, &stack);

	HASH_TABLE_ITER<VINDEX16,SNL_NEWINFO> iter2(new2old);
	VINDEX16				vnew2;
	SNL_NEWINFO			newinfo2;
	while (iter2.Step(&vnew2, &newinfo2)) {
	  INT lex2 = newinfo2.Lex;
	  WN* wnnew2 = dg->Get_Wn(vnew2);
	  BOOL is_load = OPCODE_is_load(WN_opcode(wnnew2));

	  if (!is_load && lex < lex2)		// redundant
	    continue;

	  count++;
	  BOOL ok = dg->Add_Edge( wnnew, &stack, wnnew2, &stack, lex < lex2);
	  if (!ok) {
	    LNO_Erase_Dg_From_Here_In(WN_kid1(wnnew), dg);
	    LNO_Erase_Dg_From_Here_In(WN_kid(wnnew2,is_load?0:1), dg);
	    disaster = TRUE;
	    goto disaster;
	  }
	}
      }
    }

    SNL_DEBUG1(1, "Peeling added %d edges\n", count);
  }

  Cleanup_For_Inner_Loop_Peeling(loop_body, stmt, match_stmt, TRUE); 

 disaster:
  if (disaster) {
    SNL_DEBUG0(0, "Ran out of edges in peeling");
    HASH_TABLE_ITER<VINDEX16,SNL_NEWINFO> iter(new2old);
    VINDEX16		v;
    SNL_NEWINFO		dummy;
    while (iter.Step(&v, &dummy)) {
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
    }
  }

  WN* wn_next = NULL; 
  WN* first = WN_first(newblock);
  WN* last = WN_last(newblock);
#ifdef KEY
  // Bug 5489 - If the only statement inside the DO_LOOP 
  // is another DO_LOOP then, first = last = NULL because 
  // DO_LOOPs are already deleted out of new_block.
  for (WN* wn_temp = first; wn_temp; wn_temp = wn_next) {
#else
  for (WN* wn_temp = first; ; wn_temp = wn_next) {
#endif
    wn_next = WN_next(wn_temp);  
    Remove_Redundant_Stids(wn_temp, du); 
    if (wn_temp == last) 
      break;
  }
  MEM_POOL_Pop(&SNL_local_pool);
} 

//-----------------------------------------------------------------------
// ROUTINES FOR PROTECTING A NEST WITH CONDITIONALS 
//-----------------------------------------------------------------------

//-----------------------------------------------------------------------
// NAME: Remove_Privatizable_DU_Copy_Arcs
// FUNCTION: For the 'nbodies' copies of the code 'bodies[]' (for which 
//   'bodies[0]' is the original), delete all cross-body DU arcs between  
//   nodes which are privatizable (as given by 'priv').  
//-----------------------------------------------------------------------

static void Remove_Privatizable_DU_Copy_Arcs(WN* bodies[], 
					     INT  nbodies,
                                             const SX_INFO* priv)
{
  if (priv->Plist.Is_Empty())
    return;

  // put <wn,body> in ldid/stid

  HASH_TABLE<WN*,INT>    ht(97, &SNL_local_pool);

  for (INT i = 1; i < nbodies; i++) {
    for (LWN_ITER *iter = LWN_WALK_TreeIter(bodies[i]);
      iter; iter = LWN_WALK_TreeNext(iter)) {
      WN* wn = iter->wn;
      OPERATOR opr = WN_operator(wn);
      if (opr == OPR_LDID || opr == OPR_STID)
        ht.Enter(wn, i);
    }
  }

  // go through original.  If a node is privatizable and there's an arc
  // to another body, remove arc.

  for (LWN_ITER *iter = LWN_WALK_TreeIter(bodies[0]);
    iter; iter = LWN_WALK_TreeNext(iter)) {
    WN* wn = iter->wn;
    OPERATOR opr = WN_operator(wn);
    if (opr == OPR_LDID && priv->Find(SYMBOL(wn))) {
      DEF_LIST* deflist = Du_Mgr->Ud_Get_Def(wn);
      DEF_LIST_ITER iter(deflist);
      for(const DU_NODE *n=iter.First(); !iter.Is_Empty(); n=iter.Next()) {
        WN *def = n->Wn();
        if (ht.Find(def))
          Du_Mgr->Delete_Def_Use(def, wn);
      }
    }
    else if (opr == OPR_STID && priv->Find(SYMBOL(wn))) {
      USE_LIST* uselist = Du_Mgr->Du_Get_Use(wn);
      USE_LIST_ITER iter(uselist);
      for(const DU_NODE *n=iter.First(); !iter.Is_Empty(); n=iter.Next()) {
        WN *use = n->Wn();
        if (ht.Find(use))
          Du_Mgr->Delete_Def_Use(wn, use);
      }
    }
  }
}

//-----------------------------------------------------------------------
// NAME: SNL_GEN_Protect_Nest_With_Conditionals
// FUNCTION: Using the information in 'ni', create two versions of the 
//   nest, and protect one with conditionals which force that version 
//   to execute only when each loop in the nest has at least one itera-
//   tion.  Set 'failed' if this operation failed.  Return an SNL_REGION
//   for the transformed code. 
//-----------------------------------------------------------------------

extern SNL_REGION SNL_GEN_Protect_Nest_With_Conditionals(const SNL_NEST_INFO* 
							   ni,
					                 BOOL* failed)
{
  ARRAY_DIRECTED_GRAPH16*	dg = Array_Dependence_Graph;
  *failed = FALSE; 
  INT r;
  const SYSTEM_OF_EQUATIONS* conds = &ni->Bi()->Conditionals();
  INT outerdepth = ni->Depth_Inner() - ni->Nloops_General() + 1;
  WN* outerloop = ni->Dostack().Bottom_nth(outerdepth);

  if (conds->Num_Le_Constraints() == 0 && conds->Num_Eq_Constraints() == 0)
    return SNL_REGION(outerloop, outerloop);

  WN* untransformable_nest = LWN_Copy_Tree(outerloop, TRUE, LNO_Info_Map);

  WN* tmp_unrolls[2];
  tmp_unrolls[0] = outerloop;
  tmp_unrolls[1] = untransformable_nest;
  if (red_manager) red_manager->Unroll_Update(tmp_unrolls, 2);
  Unrolled_DU_Update(tmp_unrolls, 2, outerdepth-1, TRUE, FALSE);

  // Note that for privatizable variables, Unrolled_DU_Update() is
  // too conservative.  Prune the arcs so that defs from one nest
  // do not link uses in the other.

  Remove_Privatizable_DU_Copy_Arcs(tmp_unrolls, 2,
                                   &ni->Privatizability_Info());

  if (!dg->Add_Deps_To_Copy_Block(outerloop, untransformable_nest)) {
    *failed = TRUE; 
    SNL_DEBUG0(0, "Add_Deps_To_Copy_Block failed");
    LNO_Erase_Dg_From_Here_In(untransformable_nest, dg); 
    LNO_Erase_Dg_From_Here_In(outerloop, dg); 
    LWN_Delete_Tree(untransformable_nest);
    return SNL_REGION(outerloop, outerloop);
  }

  // step 1: generate code for each eq row

  OPCODE andop = OPCODE_make_op(OPR_BAND, Boolean_type, MTYPE_V);

  WN* tree = NULL;
  for (r = 0; r < conds->Num_Eq_Constraints(); r++) {
    WN* expr = generate_tree_from_bounds_info_row(&conds->Aeq()(r,0),
						  conds->Beq()[r],
						  FALSE,
						  &ni->Bi()->Var_Info());
    tree = tree ? LWN_CreateExp2(andop, tree, expr) : expr;
  }
  
  for (r = 0; r < conds->Num_Le_Constraints(); r++) {
    WN* expr = generate_tree_from_bounds_info_row(&conds->Ale()(r,0),
						  conds->Ble()[r],
						  TRUE,
						  &ni->Bi()->Var_Info());
    tree = tree ? LWN_CreateExp2(andop, tree, expr) : expr;
  }

  Is_True(tree, ("SNL_Protect_Nest_With_Conditionals: bug"));

  // step 2: graft test in

  WN* parent = LWN_Get_Parent(outerloop);
  WN* prev = WN_prev(outerloop);

  LWN_Extract_From_Block(parent, outerloop);
  WN* wnif = LWN_CreateIf(tree, WN_CreateBlock(), WN_CreateBlock());
  LWN_Copy_Frequency(wnif, outerloop);
  LWN_Copy_Linenumber(outerloop, wnif);
  LWN_Insert_Block_After(WN_then(wnif), NULL, outerloop);
  LWN_Insert_Block_After(WN_else(wnif), NULL, untransformable_nest);
  LWN_Insert_Block_After(parent, prev, wnif);

  // annotate the if
  BOOL has_regions=Find_SCF_Inside(wnif,OPC_REGION)!=NULL;
  IF_INFO *ii = CXX_NEW(IF_INFO(&LNO_default_pool,TRUE,has_regions),
                        &LNO_default_pool);
  WN_MAP_Set(LNO_Info_Map,wnif,(void *)ii);
  DOLOOP_STACK* stack = CXX_NEW(DOLOOP_STACK(&LNO_local_pool),
				&LNO_local_pool);
  Build_Doloop_Stack(wnif, stack);
  LNO_Build_If_Access(wnif, stack);
  CXX_DELETE(stack, &LNO_local_pool);

  Renumber_Loops(WN_else(wnif), WN_else(wnif), dg);

  return SNL_REGION(wnif, wnif);
}
//-----------------------------------------------------------------------
// NAME: SNL_GEN_Distribution
// FUNCTION: Apply distribution to the SNL of 'nloops' loops with outermost
//   loop 'wn_outer', given that we wish to apply the unimodular trans-
//   formation 'unimodular' and cache tiling proscribed by 't'.  The 
//   scalar expanded variables are given by 'plist'.  The SNL is distri-
//   butable above if 'above_is_distributable' is TRUE; it is distributable
//   below if 'below_is_distributable' is TRUE.  The function returns the 
//   SNL_REGION of the transformed code. 
//-----------------------------------------------------------------------

extern SNL_REGION SNL_GEN_Distribution(WN* wn_outer, 
				       IMAT* unimodular,
				       SNL_TILE_INFO* ti,
				       INT nloops, 
				       BOOL find_split_depth, 
				       SX_PLIST* plist,
				       BOOL above_is_distributable, 
				       BOOL below_is_distributable) 
{
  // We do not scalar expand unless necessary.
  SNL_GEN_Scalar_Expand(wn_outer, unimodular, ti, nloops, plist,
    -1, NULL, FALSE, TRUE);

  // So now distribution is legal and should be applied.
  WN* wn_new_first = NULL; 
  WN* wn_new_last = NULL; 
  INT split_depth = find_split_depth ? Split_Depth(wn_outer, nloops) : -1;
  SNL_GEN_Distribute(wn_outer, split_depth, nloops, above_is_distributable,
    below_is_distributable, &wn_new_first, &wn_new_last);

  // Update regions. 
  SNL_REGION region(wn_outer, wn_outer);
  if (wn_new_first != NULL)
    region.First = wn_new_first;
  if (wn_new_last != NULL)
    region.Last = wn_new_last;
  if (!Valid_SNL_Region(region)) 
    DevWarn("SNL_General_Distribution: Invalid SNL_REGION [0x%p,0x%p]"
      , region.First, region.Last); 
  return region;
}

//-----------------------------------------------------------------------
// ROUTINES FOR INTERCHANGE AND CACHE TILING 
//-----------------------------------------------------------------------

//-----------------------------------------------------------------------
// NAME: Which_Loop_Bound
// FUNCTION: Returns the rightmost non-zero in the 'row' in the range 
//   ['from', 'from' + 'nloops' - 1], or -1 if there is none. 
//-----------------------------------------------------------------------

static INT Which_Loop_Bound(const mINT32* row, INT from, INT nloops)
{
  for (INT r = from + nloops -1; r >= from; r--)
    if (row[r])
      return r;
  return -1;
}

//-----------------------------------------------------------------------
// NAME: Row_Echelon
// FUNCTION: Convert the system of equations 's' to row echelon form. 
//   The 'n' equations starting with 'first' are actually the ones 
//   converted. (?)  The input is overwritten in the conversion. 
//-----------------------------------------------------------------------

static void Row_Echelon(SYSTEM_OF_EQUATIONS* s, 
			INT first, 
			INT n)
{
  INT   r;
  INT   i;
  BOOL	inconsistent;
  BOOL  ok;

  Is_True(n <= SNL_MAX_LOOPS,
	  ("loops nested too deeply: %d > %d", n, SNL_MAX_LOOPS));

  ok = s->Copy_To_Work();
  FmtAssert(ok, ("Work array for system of equations too small"));

  for (i = n+first-1; i >= first+1; i--) {
    ok = SYSTEM_OF_EQUATIONS::Project(i, &inconsistent);
    FmtAssert(ok, ("Projection failed!"));
    FmtAssert(!inconsistent, ("Projection can't be inconsistent!"));
    s->Add_Work_Le_Non_Simple_Redundant();
  }

  // In s we have all the equations, but need to sub in equals in the
  // work array

  ok = s->Copy_To_Work();
  FmtAssert(ok, ("Work array for system of equations too small"));
  ok = s->Sub_In_Equal(&inconsistent);	// do sub in last
  FmtAssert(ok, ("Sub_In_Equals failed"));
  FmtAssert(!inconsistent, ("Sub_In_Equal can't be inconsistent!"));
  s->Reset_To(0, 0, s->Num_Vars());
  s->Add_Work_Le();

  INT rows = s->Num_Le_Constraints();
  INT cols = s->Num_Vars();

  INT* which = CXX_NEW_ARRAY(INT, rows+1, &LNO_local_pool);
  BOOL* flag = CXX_NEW_ARRAY(BOOL, rows+1, &LNO_local_pool);

  for (r = 0; r < rows; r++)
    which[r] = Which_Loop_Bound(&s->Ale()(r,0), first, n);
  which[rows] = n + first;

  s->Sort_Le(which, FALSE);

  // Tighten up bounds.

  s->Take_Gcds();

  // now eliminate redundant rows, always leaving at least one for each
  // upper and lower bound at each depth.  Ancourt has an algorithm that
  // eliminates redundancies from the inside out, but I worry that it would
  // increase execution counts of the outermost loops by eliminating too many
  // constraints.  I'll do it differently.  I'll go from the outside in, adding
  // constraints that are not redundant given the previous loops added in.
  // At any given level, redundant equations can be added (e.g. i<100 then
  // i<99), so after added, make a pass through the equations in the other
  // direction (i<99, then i<100) to catch more redundant ones.

  SYSTEM_OF_EQUATIONS nsys(0, 0, cols, &LNO_local_pool);

  for (r = 0; which[r] == -1; r++)
    nsys.Add_Le(&s->Ale()(r,0), s->Ble()[r]);

  for (i = first; i < n+first; i++) {
    INT nle = nsys.Num_Le_Constraints();
    INT rle = r;

    // mark in flag which new equations are redundant.  Also, always be sure
    // there's at least one equation for each bound

    INT pbounds = 0;
    INT nbounds = 0;

    for (r = rle; which[r] == i; r++) {
      BOOL p = s->Ale()(r, i) > 0;
      if ((p && pbounds == 0) || (!p && nbounds == 0)) {
	flag[r] = TRUE;
	nsys.Add_Le(&s->Ale()(r,0), s->Ble()[r]);
      }
      else
	flag[r] = nsys.Add_Le_Non_Redundant(&s->Ale()(r,0), s->Ble()[r]);
      if (flag[r])
	p ? pbounds++ : nbounds++;
    }
    FmtAssert(pbounds > 0,
              ("Missing upper bounds expression r=%d rle=%d i=%d", r, rle, i));
    FmtAssert(nbounds > 0,
              ("Missing lower bounds expression r=%d rle=%d i=%d", r, rle, i));

    if (pbounds == 1 && nbounds == 1)
      continue;

    // now go other direction inserting non-redundant equations.  This
    // should catch more redundancies.

    nsys.Reset_To(nle, 0, cols);

    pbounds = 0;
    nbounds = 0;

    for (INT rr = r-1; rr >= rle; rr--) {
      if (flag[rr] == FALSE)
	continue;

      BOOL p = s->Ale()(rr, i) > 0;
      if ((p && pbounds == 0) || (!p && nbounds == 0))
	nsys.Add_Le(&s->Ale()(rr,0), s->Ble()[rr]);
      else
	flag[rr] = nsys.Add_Le_Non_Redundant(&s->Ale()(rr,0), s->Ble()[rr]);
      if (flag[rr])
	p ? pbounds++ : nbounds++;
      }
    FmtAssert(pbounds > 0 && nbounds > 0, ("Missing bounds expression"));
  }

  s->Reset_To(0, 0, s->Num_Vars());
  s->Add_Soe(&nsys);

  CXX_DELETE_ARRAY(which, &LNO_local_pool);
  CXX_DELETE_ARRAY(flag, &LNO_local_pool);
}

//-----------------------------------------------------------------------
// NAME: Rewrite_Bounds 
// FUNCTION: Use the 'td' to rewrite the loop bounds for the SNL whose 
//   loops are given in 'stack' and whose outermost loop is given by 
//   'stack->Bottom_nth(first_in_stack)'.  Put the transformed lower 
//   bound, upper bound, and steps in the arrays 'tlb', 'tub', and 'tstep'
//   which are indexed by the transformed loop depth.  The 'soe' is the 
//   systems of equations to use for the loop bounds.  The 'soe' is not 
//   modified.  The 'code' tells which transformation to apply: 
//     code: 0  unimodular transformation (ignore tile variables)
//     code: 1  tiling of loop body (tiling variables to be ignored)
//     code: 2  tiling (just compile outer loop tile boundaries)
//   For code 1, the body is also rewritten as necessary for tiling to be
//   legal with non-perfectly nested loops.
//-----------------------------------------------------------------------

static void Rewrite_Bounds(SNL_TRANS_INDEX_DATA* td,
			   const SYSTEM_OF_EQUATIONS* soe,
			   WN* tlb[],
			   WN* tub[],
			   WN* tstep[],
			   DOLOOP_STACK* stack,
			   INT first_in_stack,
			   INT code)
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph;

  // The system of equations contains the new loop bounds, one row
  // per bound expression.  td shows how to map the columns of soe into
  // the new indices: t_nloops, i_nloops and o_nloops handle it.

  INT   d;

  WN*	lb[SNL_MAX_LOOPS];
  WN*	ub[SNL_MAX_LOOPS];
  WN*	iloop[SNL_MAX_LOOPS];

  INT	tt_nloops = code == 2 ? td->t_nloops : 0;	// tile depth
  INT	ii_nloops = code == 2 ? 0 : td->i_nloops;	// body depth
  INT   x_nloops  = code == 2 ? tt_nloops : ii_nloops;

  // Zero the new loop bounds

  for (d = 0; d < x_nloops; d++) {
    lb[d] = NULL;
    ub[d] = NULL;
    iloop[d] = (code != 2) ? stack->Bottom_nth(first_in_stack+d) : NULL;
  }

  mINT32* trow = CXX_NEW_ARRAY(mINT32, soe->Num_Vars(), &LNO_local_pool);
  mINT64  tcon;

  // for each loop for which we are determining bounds ...

  for (d = 0; d < x_nloops; d++) {

    // fill in the loop expressions lb[d] and ub[d]

    SYMBOL symbol = (code == 2) ? td->tdata[d].symbol :
                                  td->idata[d].post_symbol;
    WN*    alias_wn = (code == 2) ? td->tdata[d].alias_wn :
                                    td->idata[d].post_alias_wn;

    // compute an expression for the loop bounds.  MAX/MIN if more than one

    for (INT r = 0 ; r < soe->Num_Le_Constraints(); r++) {
      INT from = code == 1 ? td->t_nloops : 0;
      INT l = Which_Loop_Bound(&soe->Ale()(r,0), from, x_nloops);
      if (l - from != d)	// slightly inefficient.
	continue;

      BOOL is_lb = soe->Ale()(r,l) < 0;
      for (INT i = 0; i < soe->Num_Vars(); i++)
	trow[i] = is_lb ? soe->Ale()(r,i) : -soe->Ale()(r,i);
      tcon = is_lb ? -soe->Ble()[r] : soe->Ble()[r];

      // Now the trow[l] coef is negative, the others we can
      // generate code from, and is_lower_bound tells us that information.
      // The next task is to reduce to lowest terms.

      INT coef = -trow[l];		// make it positive
      Is_True(coef > 0, ("Bug in Rewrite_Bounds()"));
      trow[l] = 0;			// no code for index

      if (coef > 1) {
	// Do a gcd and divide through.
	INT g1 = Gcd(trow, soe->Num_Vars());
	INT g = INT(Gcd(Gcd(INT64(g1), tcon),INT64(coef)));
	if (g > 1) {
	  for (INT c = 0; c < soe->Num_Vars(); c++)
	    trow[c] /= g;
	  tcon /= g;
	  coef /= g;
	}
      }
      INT part = UCTILE_I | UCTILE_O; 
      if (code != 0) 
        part |= UCTILE_T;  
      WN* wn = generate_tree_from_row(trow, td, tcon, symbol.Type, part); 
      FmtAssert(wn, ("Unable to generate tree from row!"));

      if (coef > 1) {
        WN* denominator = LWN_Make_Icon(symbol.Type, coef);
        if (is_lb)
          wn = LWN_CreateDivceil(symbol.Type, wn, denominator);
	else
          wn = LWN_CreateDivfloor(symbol.Type, wn, denominator);
      }

      WN** b = is_lb ? &lb[d] : &ub[d];
      if (*b == NULL)
	*b = wn;
      else {
	OPERATOR    opr = is_lb ? OPR_MAX : OPR_MIN;
	OPCODE	    opc = OPCODE_make_op(opr, symbol.Type, MTYPE_V);
	*b = LWN_CreateExp2(opc, *b, wn);
      }
    }

    FmtAssert(lb[d], ("Index %d has no lower bound <code=%d>", d, code));
    FmtAssert(ub[d], ("Index %d has no upper bound <code=%d>", d, code));
  }
  
  if (code != 2) {
    for (d = 0; d < x_nloops; d++) {
      SYMBOL symbol = td->idata[d].post_symbol;
      WN*    alias_wn = td->idata[d].post_alias_wn;
      WN* indx = WN_index(iloop[d]);
      if (SYMBOL(indx) == symbol)
        continue;

      Replace_Symbol(iloop[d], SYMBOL(indx), symbol, alias_wn, iloop[d]);
      WN_st_idx(indx) = ST_st_idx(symbol.St());
      WN_offset(indx) = symbol.WN_Offset();
    }
  }
  
  for (d = 0; d < x_nloops; d++) {
    SYMBOL symbol = (code == 2) ? td->tdata[d].symbol :
                                  td->idata[d].post_symbol;
    WN*    alias_wn = (code == 2) ? td->tdata[d].alias_wn :
                                    td->idata[d].post_alias_wn;

    // turn from expressions into statements.

    OPCODE	opadd = OPCODE_make_op(OPR_ADD, symbol.Type, MTYPE_V);
    OPCODE	opst = OPCODE_make_op(OPR_STID, MTYPE_V, symbol.Type);
    OPCODE	opld = OPCODE_make_op(OPR_LDID, symbol.Type, symbol.Type);
    OPCODE	ople = OPCODE_make_op(OPR_LE, Boolean_type, symbol.Type);
    TY_IDX	ty = Be_Type_Tbl(symbol.Type);

    if (code == 1) {
      // For tiling imperfect loops, the body needs to change in that the
      // imperfect parts need protection by ifs.  This is very special
      // case code, and is probably not worth combining with the
      // unimodular imperfect code, even though it is based on similar theory.
      // Most of all, it does belong here, because it needs the bounds before
      // and after tiling both!  The bounds before tiling are hanging off the
      // tree.  The bounds after tiling are in lb[] and ub[].

      if (d > 0 &&
          (WN_prev_executable(iloop[d]) || WN_next_executable(iloop[d]))) {

        // TODO: I believe two-sided imperfect tiling (imperfect code both
        // above and below the nest) is outlawed for
        // historical reasons only.  So this code didn't need to implement it
        // and doesn't.  If you fix this code to handle it, and fix snl_nest
        // to not give up when it sees it, you can extend functionality.
        // Very low priority.

	Is_True(!WN_prev(iloop[d]) || !WN_next(iloop[d]),
		("Cannot handle two sided tiling -- shouldn't happen here"));

	BOOL doing_above = WN_prev_executable(iloop[d]) != NULL;

	// Rewrite body for imperfectly nested portion.
        //
	//  DO i_(d-1) = L_(d-1), U_(d-1)
	//    S(...)
	//    DO i_d = L_d, U_d
	//      ...
        //
        // should execute only when if it had been written as
        //
	//   DO i_1 = ...
	//     DO i_n = ...
	//      if (i_d == L_d && ... && i_n == L_n) S(...)
	//       ...
        //
        // Tiling creates strips outside, so we get
        //
        //  DO t ...
	//   ...
	//    DO i_n = l'_n, u'_n
	//      if (i_d == L_d && ... && i_n == L_n) S(...)
	//       ...
	//      ...
        //
        // This condition can be moved out.  We know that i_k == L_k, and
        // we also need l'_n <= u'_n ... in that case S will execute.  For
        // example, to move the if (...) S(...) out one level, we'd need
        //   if (i_d == L_d && ... && i_(n-1) == L_(n-1) &&
        //       l'_n <= u'_n && l'_n == L_n) S(...)
        // That is, when we've pulled it out, i_n is l'_n (S only may execute
        // in the first iteration of i_n).  We may pull the S out as far as
        // just outside the i_d loop.  The if around it becomes elaborate.
        // The concept is that all loops inside must execute at least once,
        // given this (i_1, ..., i_(d-1)) indices, and that the next iteration
        // that will execute is (i_d, ..., i_n).  The following condition
        // produces that:
        //
        //    l_d = L_d(i_1, ..., i_(d-1))
        //    l'_d = L'_d(i_1, ..., i_(d-1))
        //    u'_d = U'_d(i_1, ..., i_(d-1))
        //    IF (l'_d == l_d && l'_d <= u'_d) THEN
        //      l_{d+1} = L_d(i_1, ..., i_(d-1); l_d)
        //      l'_{d+1} = L'_d(i_1, ..., i_(d-1); l_d)
        //      u'_{d+1} = U'_d(i_1, ..., i_(d-1); l_d)
        //      IF (l'_{d+1} == l_{d+1} && l'_{d+1} <= u'_{d+1}) THEN
        //        ...
        //        l_n = L_d(i_1, ..., i_(d-1); l_d, ... l_{n-1})
        //        l'_n = L'_d(i_1, ..., i_(d-1); l_d, ... l_{n-1})
        //        u'_n = U'_d(i_1, ..., i_(d-1); l_d, ... l_{n-1})
        //        IF (l'_n == l_n && l'_n <= u'_n) THEN
        //          S(...)
        //
        // For an imperfect below the loop rather than above, it's nearly
        // the same.  You just have to be sure the last iteration indeed
        // executes
        //

        WN**     lldid = CXX_NEW_ARRAY(WN*, x_nloops, &LNO_local_pool);

        // The imperfect code handling.

        WN*   s_block = (doing_above) ? 
          LWN_Create_Block_From_Stmts_Above(iloop[d]) :
          LWN_Create_Block_From_Stmts_Below(iloop[d]);

        WN*   current_body = WN_CreateBlock();

        // l_ii = L_ii(i_1, ... , i_{d-1}, l_d, l_{d+1}, ..., l_{ii-1})
        // l'_ii = L'_ii(i_1, ... , i_{d-1}, l_d, l_{d+1}, ..., l_{ii-1})
        // u'_ii = U'_ii(i_1, ... , i_{d-1}, l_d, l_{d+1}, ..., l_{ii-1})
	
        for (INT ii = d; ii < x_nloops; ii++) {
          char   name1[32];
          char   name2[32];
          char   name3[32];
	  
          sprintf(name1, "$imperf_%c%d_%d", doing_above ? 'L' : 'U', d, ii);
          sprintf(name2, "$imperf_Lnew%d_%d", d, ii);
          sprintf(name3, "$imperf_Unew%d_%d", d, ii);

          TYPE_ID type = Do_Wtype(iloop[ii]);

          OPCODE  opldid = OPCODE_make_op(OPR_LDID, type, type);
          OPCODE  opstid = OPCODE_make_op(OPR_STID, MTYPE_V, type);
          OPCODE  opeq = OPCODE_make_op(OPR_EQ, Boolean_type, type);
          OPCODE  ople = OPCODE_make_op(OPR_LE, Boolean_type, type);
          OPCODE  opand = OPCODE_make_op(OPR_LAND, Boolean_type, MTYPE_V);

          SYMBOL lii = Create_Preg_Symbol(name1, type);
          SYMBOL lpii = Create_Preg_Symbol(name2, type);
          SYMBOL upii = Create_Preg_Symbol(name3, type);

          WN* lstid = LWN_CreateStid(opstid, lii.WN_Offset(), lii.St(), ty,
                                     SNL_Copy_Exp((doing_above)?
                                                WN_kid0(WN_start(iloop[ii])):
                                                SNL_UBexp(WN_end(iloop[ii]))));

          WN* lpstid = LWN_CreateStid(opstid, lpii.WN_Offset(),
                                      lpii.St(), ty,
                                      SNL_Copy_Exp(lb[ii]));
          WN* upstid = LWN_CreateStid(opstid, upii.WN_Offset(),
                                      upii.St(), ty,
                                      SNL_Copy_Exp(ub[ii]));

          lldid[ii] = LWN_CreateLdid(opldid, lstid);
          WN* lupldid = doing_above ? LWN_CreateLdid(opldid, lpstid)
	    : LWN_CreateLdid(opldid, upstid);
          WN* lpldid = LWN_CreateLdid(opldid, lpstid);
          WN* upldid = LWN_CreateLdid(opldid, upstid);

	  if (doing_above)
            Du_Mgr->Add_Def_Use(lpstid, lupldid);
          else 
            Du_Mgr->Add_Def_Use(upstid, lupldid);
          Du_Mgr->Add_Def_Use(upstid, upldid);
          Du_Mgr->Add_Def_Use(lpstid, lpldid);
          Du_Mgr->Add_Def_Use(lstid, lldid[ii]);

          Du_Mgr->Ud_Get_Def(lupldid)->Set_loop_stmt(NULL);
          Du_Mgr->Ud_Get_Def(upldid)->Set_loop_stmt(NULL);
          Du_Mgr->Ud_Get_Def(lpldid)->Set_loop_stmt(NULL);
          Du_Mgr->Ud_Get_Def(lldid[ii])->Set_loop_stmt(NULL);

          // For each of these, replace i_n with l_n, etc.

          for (INT jj = d; jj < ii; jj++) {
            SYMBOL idx(WN_index(iloop[jj]));
            Replace_Ldid_With_Exp_Copy(idx, lstid, lldid[jj], Du_Mgr);
            Replace_Ldid_With_Exp_Copy(idx, lpstid, lldid[jj], Du_Mgr);
            Replace_Ldid_With_Exp_Copy(idx, upstid, lldid[jj], Du_Mgr);
          }

          LWN_Insert_Block_After(current_body, NULL, lstid);
          LWN_Insert_Block_After(current_body, lstid, lpstid);
          LWN_Insert_Block_After(current_body, lpstid, upstid);

          WN* wif_tst = LWN_CreateExp2(opand,
                                       LWN_CreateExp2(opeq, lupldid, lldid[ii]),
                                       LWN_CreateExp2(ople, lpldid, upldid));

          WN* wif = LWN_CreateIf(wif_tst, WN_CreateBlock(), WN_CreateBlock());
          LWN_Insert_Block_Before(current_body, NULL, wif);     // at end

	  // the current body contains references to index expressions,
	  // specifically indices from iloop.  Not all the DU chains
	  // are necessarily okay, so fix them up.
          Fix_Do_Du_Info(current_body, NULL, TRUE, iloop[ii], 0);

          if (ii == d) {
            WN* iloopd_prnt = LWN_Get_Parent(iloop[d]);
            if (doing_above)
              LWN_Insert_Block_Before(iloopd_prnt, iloop[d], current_body);
            else
              LWN_Insert_Block_After(iloopd_prnt, iloop[d], current_body);
          }

          // annotate the ifs
          BOOL has_regions=Find_SCF_Inside(wif,OPC_REGION)!=NULL;
          IF_INFO *ifi = CXX_NEW(IF_INFO(&LNO_default_pool, FALSE, has_regions),
                                 &LNO_default_pool);
          WN_MAP_Set(LNO_Info_Map, wif, (void *)ifi);
          DOLOOP_STACK* stack = CXX_NEW(DOLOOP_STACK(&LNO_local_pool),
                                        &LNO_local_pool);
          Build_Doloop_Stack(wif, stack);
          LNO_Build_If_Access(wif, stack);
          CXX_DELETE(stack, &LNO_local_pool);

          current_body = WN_then(wif);
        }

        LWN_Insert_Block_After(current_body, NULL, s_block);

        CXX_DELETE_ARRAY(lldid, &LNO_local_pool);
      }
    }

    // in the case of outer tile bounds, just put them in tlb, tub, tstep

    if (code == 2) {
      Is_True(tlb[d] == NULL, ("Surprising tile lb"));
      Is_True(tub[d] == NULL, ("Surprising tile ub"));
      Is_True(tstep[d] == NULL, ("Surprising tile step"));

      // Note that the lb, ub and step do not have DU or aliasing information
      // here for the new index variables, although they do for other
      // vairables.  They don't need them yet as they will be added later.

      lb[d] = LWN_CreateStid(opst, symbol.WN_Offset(), symbol.St(), 
						ty, lb[d]);

      // TODO OK: note that we fix up DO information later,
      // so the bad alias is ok

      ub[d] = LWN_CreateExp2(ople, WN_CreateLdid(opld, symbol.WN_Offset(),
						 symbol.St(), ty), ub[d]);

      tlb[d] = lb[d];
      tub[d] = ub[d];
      tstep[d] = LWN_CreateStid(opst, symbol.WN_Offset(), symbol.St(), ty,
				LWN_CreateExp2(opadd,
				       WN_CreateLdid(opld, symbol.WN_Offset(),
						     symbol.St(), ty),
				       LWN_Make_Icon(symbol.Type, 1)));

      LWN_Copy_Linenumber(stack->Bottom_nth(first_in_stack), tlb[d]);
      LWN_Copy_Linenumber(stack->Bottom_nth(first_in_stack), tub[d]);
      LWN_Copy_Linenumber(stack->Bottom_nth(first_in_stack), tstep[d]);
    }
    else {
      LWN_Delete_Tree(WN_kid0(WN_start(iloop[d])));
      WN_kid0(WN_start(iloop[d])) = lb[d];
      LWN_Set_Parent(WN_kid0(WN_start(iloop[d])), WN_start(iloop[d]));

      LWN_Delete_Tree(SNL_UBexp(WN_end(iloop[d])));
      SNL_UBexp(WN_end(iloop[d])) = ub[d];
      LWN_Set_Parent(SNL_UBexp(WN_end(iloop[d])), WN_end(iloop[d]));

      Fix_Do_Du_Info(iloop[d], td, FALSE, NULL, 1);

      Is_True(Step_Size(iloop[d]) == 1, ("Step wasn't unity"));
    }
  }
}

//-----------------------------------------------------------------------
// NAME: UT_Body_Exp
// FUNCTION: Apply the unimodular transformation (as expressed in td) to 
//   an expression 'wn' or any non-control-flow statement.  Return the depth
//   of the deepest substituted index, although no one uses it.  Maybe some-
//   one will care someday.
//-----------------------------------------------------------------------

static INT UT_Body_Exp(WN* wn, SNL_TRANS_INDEX_DATA* td)
{
  if (WN_operator(wn) == OPR_LDID) {
    for (INT d = 0; d < td->i_nloops; d++) {
      if (SYMBOL(wn) == td->idata[d].post_symbol) {
	FmtAssert(td->idata[d].newcode, ("Missing newcode"));
	(void) Replace_Wnexp_With_Exp_Copy(wn, td->idata[d].newcode, 
	  Du_Mgr); 
	return td->idata[d].max_used_depth;
      }
    }
    return -1;
  }
  else {
    INT rv = -1;

    for (INT kid = 0; kid < WN_kid_count(wn); kid++) {
      INT nrv = UT_Body_Exp(WN_kid(wn,kid), td);
      if (rv < nrv)
	rv = nrv;
    }
    return rv;
  }
}

//-----------------------------------------------------------------------
// NAME: UT_Body_Innermost
// FUNCTION: Apply unimodular transformation (as expressed in td) for the
//   innermost within 'body'.  The loop indices should be transformed.
//   Rewrite_Bounds() has already (somewhat inefficiently) realized the 
//   change of symbol name and rewritten bounds, so here we have to go 
//   through again and now change body to indicate a transformation. 
//   I.e. use post, not pre, symbols.
//-----------------------------------------------------------------------

static void UT_Body_Innermost(WN* body, 
			      SNL_TRANS_INDEX_DATA* td)
{
  for(WN* wn = WN_first(body); wn; wn = WN_next(wn)) {
    switch (WN_opcode(wn)) {
     case OPC_IF:
      UT_Body_Exp(WN_if_test(wn), td);
      UT_Body_Innermost(WN_then(wn), td);
      UT_Body_Innermost(WN_else(wn), td);
      break;
     case OPC_DO_LOOP:
      UT_Body_Exp(WN_start(wn), td);
      UT_Body_Exp(WN_end(wn), td);
      UT_Body_Exp(WN_step(wn), td);
      UT_Body_Innermost(WN_do_body(wn), td);
      break;
     case OPC_DO_WHILE:
     case OPC_WHILE_DO:
      UT_Body_Exp(WN_while_test(wn), td);
      UT_Body_Innermost(WN_while_body(wn), td);
      break;
#ifdef KEY //bug 11817: handle regions under loop body
     case OPC_REGION:
       UT_Body_Innermost(WN_region_pragmas(wn),td);
       UT_Body_Innermost(WN_region_exits(wn),td);
       UT_Body_Innermost(WN_region_body(wn),td);
       break;
#endif
     default:
      UT_Body_Exp(wn, td);
      break;
    }
  }
}

//-----------------------------------------------------------------------
// NAME: UT_Generate_Imperfect_If_Code
// FUNCTION: Move the imperfect code in the loop at depth 'd1' into the  
//   the loop with depth 'd2', adding an appropriate condtional.  If 
//   'above_do' is TRUE, the imperfect code is above the loop with depth 
//   'd2', otherwise it is below.  The SNLs loops are listed in 'stack'
//   with 'stack->Bottom_nth(first_in_stack) being the outermost loop.
//   The index data is summarized in 'td'. 
// EXAMPLE: Consider: 
//	do i0 = ..
//	   S0(i0)		<- d1 == 0; d2 == 1  
//	   do i1 = 
//	     ...
//   After transformation, at worst we need S0 in the inner loop and
//   if (i1 == L1 && ) protecting it.  
//-----------------------------------------------------------------------

static void UT_Generate_Imperfect_If_Code(DOLOOP_STACK*	stack,
					  INT first_in_stack,
					  INT d1,
					  INT d2,
					  BOOL above_do,
					  SNL_TRANS_INDEX_DATA* td)
{
  FmtAssert(0, ("TODO: imperfect interchange not implemented. "
                "(e.g. DU updating.)"));

  Is_True(d1 < d2, ("imperfect_if_code() doesn't have enough work"));

  WN* loop[SNL_MAX_LOOPS];

  for (INT i = 0; i < d2; i++)
    loop[i] = stack->Bottom_nth(first_in_stack+i);

  // insert conditionals from d1 to d2-1.  If above_do true, than insert
  // conditionals based upon lower bounds, otherwise upper bounds.
  // As usual, the steps must be unity to deal with the upper bounds
  // correctly.  Finally, the imperfect code actually has to be moved.

  // Step 1: extract evil imperfect code, putting it on block.

  WN* oldblock = LWN_Get_Parent(loop[d1]);
  WN* block = above_do ?
    LWN_Create_Block_From_Stmts_Above(loop[d1]) :
    LWN_Create_Block_From_Stmts_Below(loop[d1]);

  // Step 2: put conditionals from loops d1+1 through d2 around it,
  // the conditionals guaranteeing first or last execution of the loop.

  OPCODE opand = OPCODE_make_op(OPR_LAND, Boolean_type, MTYPE_V);
  WN* header = NULL;

  for (INT d = d1; d < d2; d++) {
    WN* bnd = above_do ? td->idata[d].lbtest : td->idata[d].ubtest;
    WN* dup = LWN_Copy_Tree(bnd, TRUE, LNO_Info_Map);
    header = header ? LWN_CreateExp2(opand, header, dup) : dup;
  }

  WN* wn_if = LWN_CreateIf(header, block, WN_CreateBlock());
  LWN_Copy_Linenumber(loop[d2-1], wn_if);

  // Step 3: put it in the correct new place

  LWN_Copy_Frequency_Tree(wn_if, WN_do_body(WN_first(loop[d2-1])));
  if (above_do)
    LWN_Insert_Block_After(WN_do_body(loop[d2-1]), NULL, wn_if);
  else
    LWN_Insert_Block_Before(WN_do_body(loop[d2-1]), NULL, wn_if);
  LWN_Set_Parent(wn_if, WN_do_body(loop[d2-1]));

  // annotate the if
  BOOL hasdo = !Get_Do_Loop_Info(loop[d2-1])->Is_Inner;
  BOOL has_regions = Find_SCF_Inside(wn_if,OPC_REGION)!=NULL;
  IF_INFO *ii = CXX_NEW(IF_INFO(&LNO_default_pool,hasdo,has_regions),
                                &LNO_default_pool);
  WN_MAP_Set(LNO_Info_Map,wn_if,(void *)ii);
  DOLOOP_STACK* stackx = CXX_NEW(DOLOOP_STACK(&LNO_local_pool),
				 &LNO_local_pool);
  Build_Doloop_Stack(wn_if, stackx);
  LNO_Build_If_Access(wn_if, stackx);
  CXX_DELETE(stackx, &LNO_local_pool);
}

//-----------------------------------------------------------------------
// NAME: UT_Body_Imperfect
// FUNCTION: For the code in 'body' of loop depth 'd', go from innermost 
//   out doing the substitutions and seeing if any imperfect code must move 
//   in (for non-perfectly nested interchange).  We are transforming the 
//   SNL whose loops are given by 'stack' and for which 'stack->Bottom_nth
//   (first_in_stack)'  is the SNLs outermost loop.  We are also applying 
//   the permutation 'moveto'.  Index data for the SNL is given in 'td'. 
//-----------------------------------------------------------------------

static void UT_Body_Imperfect(WN* body,
			      DOLOOP_STACK* stack,
			      INT first_in_stack,
			      INT d,
			      INT moveto[],
			      SNL_TRANS_INDEX_DATA* td)
{
  Is_True(WN_opcode(body) == OPC_BLOCK, ("trans_ut_body_imp gets non-block"));

  WN* do_seen = NULL;

  for (WN* wn = WN_first(body); wn; wn = WN_next(wn)) {
    switch (WN_opcode(wn)) {
     case OPC_IF:
      UT_Body_Exp(WN_if_test(wn), td);
      UT_Body_Imperfect(WN_then(wn), stack, first_in_stack, -1, moveto, td);
      UT_Body_Imperfect(WN_else(wn), stack, first_in_stack, -1, moveto, td);
      break;
     case OPC_DO_LOOP:
      Is_True(d >= 0, ("DO inside IF in SNL"));
      Is_True(do_seen == NULL, ("multiple DOs in SNL"));
      Is_True(stack->Bottom_nth(first_in_stack+d) == wn,
	      ("loop confusion in UT_Body_Imperfect"));

      do_seen = wn;
      if (d+1 < td->i_nloops)
	UT_Body_Imperfect(WN_do_body(wn), stack, first_in_stack,
			  d+1, moveto, td);
      break;
     case OPC_DO_WHILE:
     case OPC_WHILE_DO:
      FmtAssert(0, ("while loops can't exist within SNL"));
      break;
     default:
      UT_Body_Exp(wn, td);
      break;
    }
  }

  // test to see if the imperfect part before or after DO present a problem.

  Is_True(d >= 1 && moveto[d] <= SNL_MAX_LOOPS,
	  ("Problem with d=%d, moveto[d]=%d", d, moveto[d]));

  if (moveto[d] > d && do_seen && WN_prev(do_seen))
    UT_Generate_Imperfect_If_Code(stack, first_in_stack, d,
				  moveto[d], TRUE, td);
  if (moveto[d] > d && do_seen && WN_next(do_seen))
    UT_Generate_Imperfect_If_Code(stack, first_in_stack, d, moveto[d],
				  FALSE, td);
}

//-----------------------------------------------------------------------
// NAME: SNL_GEN_U_Ctiling
// FUNCTION: Perform unimodular transformation and/or cache tiling for 
//   the SNL whose outermost loop is 'wn_outer' and which consists of 
//   'nloops' loops.  The unimodular transformation 'u' is applied first 
//   followed by cache tiling as specified in 't'.  Information about 
//   the loop bounds is summarized in 'bi'.  Information about scalar 
//   expanded variables is summarized in 'plist'.  Estimated register 
//   usage for the loops is set to 'est_register_usage'.  If 'warn_lexneg' 
//   is TRUE, print a warning if the transformation generates lexicograph-
//   ically negative dependences.
//   It returns the SNL_REGION of the transformed code. 
// MICHAEL'S NOTE: Actually, might as well do ut followed by tiling.  They 
//   are included in the same routine in case one day I figure out how to 
//   squeeze more efficiency out of it.  For now, the problem is that tiling 
//   imperfectly nested loops requires knowing the bounds before tiling (i.e. 
//   after unimodular transformation), so that the bounds computation for 
//   unimodular transformation must occur separetely from tiling.
// ROBERT'S NOTE: Having these together really hurts our modularity. 
//   Hope I have time to separate these Siamese twins some day. 
//-----------------------------------------------------------------------

extern SNL_REGION SNL_GEN_U_Ctiling(WN* wn_outer, 
			            INT nloops, 
			            IMAT* u,
			            SNL_TILE_INFO* t,
			            SNL_BOUNDS_INFO* bi, 
			            SX_PLIST* plist,
                                    EST_REGISTER_USAGE est_register_usage,
			            BOOL warn_lexneg)
{
  ARRAY_DIRECTED_GRAPH16*	dg = Array_Dependence_Graph;
  DU_MANAGER*			du = Du_Mgr;

  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops);
  DOLOOP_STACK stack(&LNO_local_pool);
  Build_Doloop_Stack(wn_inner, &stack);
  INT first_in_stack = Do_Loop_Depth(wn_outer);

  Is_True(u != NULL || t != NULL, 
    ("SNL_GEN_U_Ctiling() with no transformation"));

  MEM_POOL_Push(&LNO_local_pool);

  MEM_POOL*	old_dflt_pool = IMAT::Set_Default_Pool(&LNO_local_pool);
  FmtAssert(nloops == (t != NULL ? t->KHT().Cols() : u->Rows()), 
    ("Old and new interfaces match")); 
  SNL_REGION	region(stack.Bottom_nth(first_in_stack),
		       stack.Bottom_nth(first_in_stack));
  // Now prepare for transformation

  const IMAT*	kht = t ? &t->KHT() : NULL;
  IMAT*		uinv = NULL;

  if (u) {
    uinv = CXX_NEW(IMAT(&LNO_local_pool), &LNO_local_pool);
    *uinv = u->Inv();
  }

  // data for unimodular transformation and/or cache tiling

  SNL_TRANS_INDEX_DATA *td = 
    CXX_NEW(SNL_TRANS_INDEX_DATA(u, uinv, kht, bi, &stack, first_in_stack,
  			  t, &LNO_local_pool), &LNO_local_pool);
			  
  // transform bounds and body: unimodular transformation

  if (u) {

    // Apply the interchange transformation. 
    const IMAT uinvold(*uinv, &LNO_local_pool);
    uinv->D_Add_Identity_Rows_and_Cols(bi->Bounds().Num_Vars() - nloops);
    bi->Bounds().Ale() *= *uinv;
    if (bi->Bounds().Num_Eq_Constraints() > 0)
      bi->Bounds().Aeq() *= *uinv;

    // Canonicalize and rewrite the bounds in the whirl. 
    Row_Echelon(&bi->Bounds(), 0, nloops);
    Rewrite_Bounds(td, &bi->Bounds(), NULL, NULL, NULL,
		   &stack, first_in_stack, 0);
    WN* inner = stack.Bottom_nth(first_in_stack+nloops-1);
    for (INT ii = 0; ii < nloops; ii++) {
      WN* loop = stack.Bottom_nth(first_in_stack+ii);
      Fix_Do_Du_Info(td->idata[ii].newcode, td, FALSE, inner, 0);
    }
    UT_Body_Innermost(WN_do_body(inner), td);

    // for imperfect code, see how far in we have to move ifs

    BOOL imperfect = FALSE;
    for (INT l = 1; l < nloops && imperfect == FALSE; l++) {
      WN* wn = stack.Bottom_nth(first_in_stack + l);
      if (WN_prev_executable(wn) || WN_next_executable(wn))
	imperfect = TRUE;
    }

    if (imperfect) {
      INT moveto[SNL_MAX_LOOPS];
      for (INT i = 1; i < nloops; i++) {
        INT j;
	for (j = 0; j < nloops; j++)
	  if ((*u)(i-1,j))
	    moveto[i] = j+1;
	for (j = 1; j < i; j++)
	  if (moveto[j] > moveto[i])
	    moveto[i] = moveto[j];
	Is_True(moveto[i] >= i, ("Bug with moveto (%d)", i));
      }

      UT_Body_Imperfect(WN_do_body(stack.Bottom_nth(first_in_stack)),
			&stack, first_in_stack, 1, moveto, td);
    }

    BOOL utransformation_invalid = FALSE;

    DOLOOP_STACK shortstack(&LNO_local_pool);
    Build_Doloop_Stack(LWN_Get_Parent(stack.Bottom_nth(first_in_stack)),
		       &shortstack);
    LNO_Build_Access(stack.Bottom_nth(first_in_stack),
		     &shortstack, &LNO_default_pool);

    // Update loop bound annotations

    for (INT i = first_in_stack; i < first_in_stack + nloops; i++) {
      WN*               loop = stack.Bottom_nth(i);
      DO_LOOP_INFO*     dli = Get_Do_Loop_Info(loop);
      dli->Set_Est_Num_Iterations(&stack);
      dli->Is_Ivdep = FALSE;                    // conservative
      dli->Is_Concurrent_Call = FALSE;
      dli->Concurrent_Directive = FALSE;
    }

    // TODO: probably ut_body should be doing this.

    Fix_Do_Du_Info(stack.Bottom_nth(first_in_stack),
                   NULL, TRUE, NULL, nloops);

    // update dependences.  for the imperfect case, references may have
    // moved all around, farther into loops, so in that case just
    // recompute dependences.  If things were not moved around, then
    // dependences can be updated by inspection.

    SNL_Expand_Reduction_Deps(stack.Bottom_nth(first_in_stack));

    // NOTE: DEPENDENCE UPDATE CODE BELOW IDENTICAL TO THE EQUIVALENT
    // BELOW FOR TILING, EXCEPT FOR THE ACTUAL PART THAT DOES THE
    // IN PLACE UPDATE

    // keep in hash table a list of all edges we want to transform.
    // 0: already transformed.  (Thus add_edge, which only adds new
    //    edges but doesn't add to this table, does the right thing.)
    // 1: untransformed.
    // We can delete right away, as long as we set to zero, since add
    // edge may reuse, but that's already transformed.

    SNL_Rebuild_Access_Arrays(stack.Bottom_nth(first_in_stack));
    LS_IN_LOOP          loop_ls(stack.Bottom_nth(first_in_stack), dg,
                                &LNO_local_pool);
    INT curdepth = loop_ls.Good_Depth;
    INT maxdepth = loop_ls.Good_Depth + nloops;

    // put edges in table that go from a vertex in loop_ls to a vertex
    // in loop_ls

    HASH_TABLE<EINDEX16,INT>
	edge_table(MIN(dg->Get_Edge_Count(),512),&LNO_local_pool);

    // only want to transform arcs from inside the loop to inside

    WN* awn;
    LS_IN_LOOP_ITER ali1(&loop_ls);
    while (awn = ali1.Step()) {
      Is_True(loop_ls.In(awn) > 0, ("Bug in ls_in_loop_iter"));
      DOLOOP_STACK astack(&LNO_local_pool);
      Build_Doloop_Stack(awn, &astack);
      VINDEX16 v = dg->Get_Vertex(awn);
      if (v == 0)
	continue;

      for (EINDEX16 e = dg->Get_Out_Edge(v); e; e = dg->Get_Next_Out_Edge(e)) {
        WN* bwn = dg->Get_Wn(dg->Get_Sink(e));
        if (bwn && loop_ls.In(bwn))
          edge_table.Enter(e, 1);
      }
    }

    LS_IN_LOOP_ITER ali(&loop_ls);
    while (awn = ali.Step()) {
      INT alex = loop_ls.In(awn);
      DOLOOP_STACK astack(&LNO_local_pool);
      Build_Doloop_Stack(awn, &astack);
      Is_True(alex > 0, ("Bug in ls_in_loop_iter"));
      VINDEX16 v = dg->Get_Vertex(awn);
      if (v == 0)
	continue;

      EINDEX16 enext = 0;
      for (EINDEX16 e = dg->Get_Out_Edge(v); e; e = enext) {
	enext = dg->Get_Next_Out_Edge(e);
	if (edge_table.Find(e) != 1)
	  continue;

	INT components = dg->Depv_Array(e)->Num_Dim();
        FmtAssert(components >= curdepth,
                  ("edge=%d components=%d curdepth=%d",
                   e, components, curdepth));

        WN* bwn = dg->Get_Wn(dg->Get_Sink(e));
        INT blex = loop_ls.In(bwn);

	if (components >= maxdepth) {
          // update in place in this important and easy special case:
          // the edges between vertices both inside the inner loop of the SNL

	  DEP olddep[SNL_MAX_LOOPS];
	  for (INT i = 0; i < dg->Depv_Array(e)->Num_Vec(); i++) {
	    DEPV* d = dg->Depv_Array(e)->Depv(i);
            INT j;
	    for (j = 0; j < components - curdepth; j++)
	      olddep[j] = DEPV_Dep(d, j+curdepth);
	    for (j = 0; j < components - curdepth; j++) {
	      if (j >= u->Rows()) 
		continue; 
	      SNL_DEP dd;
	      for (INT k = 0; k < nloops; k++)
		dd = dd + (*u)(j,k) * SNL_DEP(olddep[k]);
	      DEPV_Dep(d, j+curdepth) = dd.Dep();
	    }
	  }
          if (SNL_Test_Reduction_Lexneg(e, awn, bwn, alex, blex)) {
            utransformation_invalid = TRUE;
            SNL_DEBUG1(1, "general permutation edge=%d illegal?!\n", e);
          }
          edge_table.Remove(e);
	}
      }
    }

    // now do remaining (non-interior) nodes.  Since this adds edges,
    // cannot merge with above for loop!

    LS_IN_LOOP_ITER ali2(&loop_ls);
    while (awn = ali2.Step()) {
      INT alex = loop_ls.In(awn);
      DOLOOP_STACK astack(&LNO_local_pool);
      Build_Doloop_Stack(awn, &astack);
      Is_True(alex > 0, ("Bug in ls_in_loop_iter"));
      VINDEX16 v = dg->Get_Vertex(awn);
      if (v == 0)
	continue;

      EINDEX16 enext = 0;
      for (EINDEX16 e = dg->Get_Out_Edge(v); e; e = enext) {
	enext = dg->Get_Next_Out_Edge(e);
	if (edge_table.Find(e) != 1)
	  continue;

        // get rid of symetric edge
        EINDEX16 econj = dg->Get_Edge(dg->Get_Sink(e), dg->Get_Source(e));

        // recompute edge
        WN* bwn = dg->Get_Wn(dg->Get_Sink(e));
        INT blex = loop_ls.In(bwn);
        Is_True(blex > 0, ("Bug in ls_in_loop_iter"));
        if (econj && e != econj && edge_table.Find(econj)) {
          if (enext == econj)
            enext = dg->Get_Next_Out_Edge(e);
          edge_table.Remove(econj);
          dg->Delete_Array_Edge(econj);
        }
        edge_table.Remove(e);
        dg->Delete_Array_Edge(e);
        DOLOOP_STACK bstack(&LNO_local_pool);
        Build_Doloop_Stack(bwn, &bstack);
        BOOL ok = dg->Add_Edge(awn, &astack, bwn, &bstack, alex < blex);
        if (!ok) { 
          LNO_Erase_Dg_From_Here_In(awn, dg); 
          LNO_Erase_Dg_From_Here_In(bwn, dg); 
        }
      }
    } 
    if (warn_lexneg && utransformation_invalid)
      DevWarn("General permutation transformation invalid?!");
  }

  WN*		inner_loop = stack.Bottom_nth(first_in_stack+nloops-1);
  DO_LOOP_INFO* inner_dli = Get_Do_Loop_Info(inner_loop);
  inner_dli->Est_Register_Usage = est_register_usage;

  // transform bounds and imperfect body: tiling transformation

  if (t) {
    // Actually, non-rectangular tiling is implemented and thoroughly
    // tested, but the formulas implemented don't work for multi-dimensional
    // blocking.  I'm not even sure how to fix them.  The rectangular
    // formulas are identical to the non-rectangular when there is no
    // multi-dimensional blocking, but are correct for multi-dimensional.

    FmtAssert(t->Rectangular(), ("non-rectangular tiling unimplemented"));
    INT khtcols = t->KHT().Cols();
    INT lrows = t->L().Rows();
    INT lcols = t->L().Cols();
    INT bcols = bi->Bounds().Num_Vars();
    INT khtrows = t->KHT().Rows();
    INT blerows = bi->Bounds().Num_Le_Constraints();
    INT beqrows = bi->Bounds().Num_Eq_Constraints();
    INT i;
    INT j;

    i = 0; 

    // these will be placed in the tiled do loops.

    WN*		tlb[SNL_MAX_LOOPS];
    WN*		tub[SNL_MAX_LOOPS];
    WN*		tstep[SNL_MAX_LOOPS];

    for (i = 0; i < SNL_MAX_LOOPS; i++)
      tlb[i] = tub[i] = tstep[i] = NULL;

    // Step 1:
    //
    // Set up the system of equations s1:
    //
    //  [ 0    BUinv ][t] <= [b]		(note BUinv already in bi)
    //  [-SL   S     ][i] <= [s]
    //
    // where S = [ kHt]  and s = [k-1]  and B and b are in the bounds_info.
    //           [-kHt]          [ 0 ]
    //
    // Notice that because non-index/non-tile variables, B has extra columns.
    // That's ok.  Currently, it's implemented so that S, L etc don't have
    // extra columns (the trip sizes are compile-time constants), so we can
    // just add extra null colums to the lower right hand S to get our desired
    // system of equations.  If we decide to have trip sizes that depend upon
    // other variables (e.g. the number of processors), then we can add the
    // extra colums to S.  L will have that many more rows, so there will be
    // no problem there.  The only problem will be including the information
    // about which variables correspond to which colums, which is no big deal.
    // So the framework is there if we need it.
    //
    // Finally, note that when the lower bound is loop invariant, it's more
    // efficient if the blocks start there.  Suppose
    //
    //	o = (o_i), where o_i is the lower bound of loop i, if that's an integer
    //             constant, and zero otherwise.
    //
    // Then instead of S(i-Lt)<=s, the bottom row of the above equation, we
    // have S(i-o-Lt) <= s, which is S(i-Lt) <= s+So, where the left hand
    // side is integral.  Pretty cool.  Let's do it.

    // Begin by computing o.  First find whether it's easy to find the min.
    // Simple when single lower bound, and it's constant.  Our heuristic
    // is that loop counts start at 1.  (Ancourt's paper makes it zero.
    // Being more careful produces fewer yet more full tiles.)

    mINT32 o[SNL_MAX_LOOPS];	// lower bound

    for (INT dd = 0; dd < nloops; dd++)
      o[dd] = 1;		// unnecessary, since code below overwrites

    for (INT r = 0; r < blerows; r++) {
      INT l = Which_Loop_Bound(&bi->Bounds().Ale()(r,0), 0, nloops);

      if (l < 0 || bi->Bounds().Ale()(r,l) > 0)
	continue;

      // assume each other variable has a min of 1, roughly.
      // that way, j = i, 100 gets a lower bound of one.
      o[l] = -bi->Bounds().Ble()[r];
      for (INT ll = 0; ll < nloops; ll++) {
	if (l != ll)
	  o[l] += bi->Bounds().Ale()(r,ll);
      }
    }

    // now compute the system of equations.

    SYSTEM_OF_EQUATIONS s1(blerows + 2*khtrows, beqrows, lcols + bcols,
			   &LNO_local_pool);
    s1.Add_Le(blerows + 2*khtrows);
    s1.Add_Eq(beqrows);

    // equality and inequality constraints, coming only from the bounds.

    for (i = 0; i < beqrows; i++) {
      mINT32*		nreq = &s1.Aeq()(i,0);
      const mINT32*	req = &bi->Bounds().Aeq()(i,0);

      for (j = 0; j < lcols; j++)
	*nreq++ = 0;
      for (j = 0 ; j < bcols; j++)
	*nreq++ = *req++;
      s1.Beq()[i] = bi->Bounds().Beq()[i];
    }

    for (i = 0; i < blerows; i++) {
      mINT32*		nrle = &s1.Ale()(i,0);
      const mINT32*	rle = &bi->Bounds().Ale()(i,0);

      for (j = 0; j < lcols; j++)
	*nrle++ = 0;
      for (j = 0 ; j < bcols; j++)
	*nrle++ = *rle++;
      s1.Ble()[i] = bi->Bounds().Ble()[i];
    }

    if (!t->Rectangular()) {
      //  [-SL   S ]  where S = [ kHt]  and s = [k-1] + So
      //                        [-kHt]          [ 0 ]

      IMAT slmat(t->KHT() * t->L(), &LNO_local_pool);

      for (i = 0; i < khtrows; i++) {
        mINT32*		nr     = &s1.Ale()(i+blerows,0);
        mINT32*		nrneg  = &s1.Ale()(i+blerows+khtrows,0);
        const mINT32*	slx    = &slmat(i,0);
        const mINT32*	khtx   = &t->KHT()(i,0);

        for (j = 0; j < lcols; j++) {
          *nr++    = -*slx;
          *nrneg++ =  *slx++;
        }
        for (j = 0; j < khtcols; j++) {
          *nr++    =  *khtx;
          *nrneg++ = -*khtx++;
        }
        for (j = 0; j < bcols-khtcols; j++) {
          *nr++    = 0;
          *nrneg++ = 0;
        }
	INT32 dp = 0;
	if (LNO_Fancy_Tile) {  // TODO OK untested (but this code doesn't
			       // execute these days anyway)
          // This will try to make the first tile begin at the lower bound
          // rather than at zero.  Slight optimization, but very worth it.
          dp = Dot_Product(&t->KHT()(i,0), o, nloops);
	}
        s1.Ble()[i+blerows]         = t->K() - 1 + dp;
        s1.Ble()[i+blerows+khtrows] = 0 - dp;
      }
    }
    else {
      // to s1, just add in the equations
      //      i_l0 - B0 t_0 <= B0 -1
      //      i_l0 - B0 t_0 > 0
      // where l0 = iloop[0], and likewise for 1, 2, ... for all strips.
      
      // For this to work properly, an outer tile for loop i must
      // fully contain all inner tiles for loop i.  That is, B outer must
      // be an exact multiple of B inner, and likewise for o[] if we are
      // using those.  That is, if the inner tile is 10ti-1 <= i <= 10ti+8,
      // where the 10ti-1 has a -1 because i goes from -1; then if there
      // is a tii with stripsize 30, it should be 30tii-3 <= i <= 30tii+26
      // to exactly contain the ti.

      FmtAssert(khtrows == t->Strips(), ("Bug in general tiling"));

      for (INT s = 0; s < khtrows; s++) {
        INT row1 = blerows + s;
        INT row2 = blerows + s + khtrows;

        INT offset = 0;
	if (LNO_Fancy_Tile) {
	  // If a loop goes from 1 to N, then rather than use the equations
	  // Bt <= i < Bt+B, why not use Bt+1 <= i < Bt+B+1?  In fact, if
	  // i is not invariant, you can do better still, but it's more
	  // complicated, so we don't do that.  The only other trick is that
	  // if there's both a t and a t' for the same i, then make the
	  // equations be perfect multiples.  E.g.  B must divide B'
	  // (which it does) and also use B't'+B'/B <= i < B't + B'+B'/B.

          offset = o[t->Iloop(s)];
          for (INT ss = khtrows-1; ss > s; ss--) {
            if (t->Iloop(s) == t->Iloop(ss)) {
              FmtAssert(t->Stripsz(s) % t->Stripsz(ss) == 0,
                        ("multi-level blocking stripszs must be multiples"));
              FmtAssert(t->Stripsz(s) != t->Stripsz(ss),
                        ("multi-level blocking stripszs can't be equal"));
              offset *= t->Stripsz(s) / t->Stripsz(ss);
	      break;
            }
          }
	}
        s1.Zero_Row_Le(row1);
        s1.Ale()(row1, s) = -t->Stripsz(s);
        s1.Ale()(row1, t->Strips() + t->Iloop(s)) = 1;
        s1.Ble()[row1] = t->Stripsz(s) - 1 + offset;

        s1.Zero_Row_Le(row2);
        s1.Ale()(row2, s) = t->Stripsz(s);
        s1.Ale()(row2, t->Strips() + t->Iloop(s)) = -1;
        s1.Ble()[row2] = -offset;
      }
    }

    s1.Take_Gcds();

    // Step 2: The system of equations has been set up ... solve it!
    // Here we solve for the t variables by projecting away the i's.
    // s1x will be where we place the projected system.

    SYSTEM_OF_EQUATIONS s1x(0, 0, s1.Num_Vars(), &LNO_local_pool);

    BOOL inconsistent;
    BOOL ok;

    ok = s1.Copy_To_Work();
    FmtAssert(ok, ("Copy_To_Work() failed"));
    ok = s1.Sub_In_Equal(&inconsistent);
    FmtAssert(ok, ("Sub_In_Equal() failed"));
    FmtAssert(!inconsistent, ("inconsistent result from Sub_In_Equal()"));

    for (i = lcols; i < lcols + khtcols; i++) {
      ok = SYSTEM_OF_EQUATIONS::Project(i, &inconsistent);
      FmtAssert(ok, ("Projection failed!"));
      FmtAssert(!inconsistent, ("Projection can't be inconsistent!"));
    }

    s1x.Add_Work_Le();
    Row_Echelon(&s1x, 0, lcols);
    Rewrite_Bounds(td, &s1x, tlb, tub, tstep, &stack, first_in_stack, 2);

    // Step 3: create new DO nodes and change the bounds information


    WN* innerloop = stack.Bottom_nth(first_in_stack);
    WN* prev = WN_prev(innerloop);
    WN* block = LWN_Get_Parent(innerloop);
    LWN_Extract_From_Block(block, innerloop);

    WN* d[SNL_MAX_LOOPS];
    WN* outerdo = NULL;

    FmtAssert(!t->Rectangular() || t->Strips() == lcols, ("Huh?"));

    for (i = 0; i < lcols; i++) {
      WN* index = WN_CreateIdname(td->tdata[i].symbol.WN_Offset(),
				  td->tdata[i].symbol.St());
      d[i] = LWN_CreateDO(index, tlb[i], tub[i], tstep[i], WN_CreateBlock());
      if (i == 0)
	outerdo = d[i];
      LWN_Insert_Block_After(block, prev, d[i]);
      block = WN_do_body(d[i]);
      prev = NULL;
    }

    // Recall that in the L matrix, the number of rows, lrows, is 
    //   the number of loops we are transforming by tiling, while 
    //   the number of columns, lcols, is the number of strip loops 
    //   we are creating. 

    for (i = 0; i < lcols; i++) {
      for (INT j = 0; j <= i; j++) {
	SNL_Change_Du_To_Index_Ldid(d[j], tlb[i], du, TRUE);
	SNL_Change_Du_To_Index_Ldid(d[j], tub[i], du, TRUE);
	SNL_Change_Du_To_Index_Ldid(d[j], tstep[i], du, TRUE);
      }
      Fix_Do_Du_Info(d[i], td, FALSE, NULL, 1);
      td->tdata[i].alias_wn = Find_Use_In_Exp(WN_step(d[i]),
					      SYMBOL(WN_index(d[i])));
    }
    for (i = 0; i < lcols; i++) {
      if (td->tdata[i].alias_wn == NULL) {
        if (LNO_Verbose) {
          fprintf(TFile, "Disaster: step = ");
          Dump_WN(WN_step(d[i]), TFile, 2);
        }
	FmtAssert(td->tdata[i].alias_wn,
		  ("Symbol %s missing in step!",
                   SYMBOL(WN_index(d[i])).Name()));
      }

      DO_LOOP_INFO* dli = CXX_NEW(DO_LOOP_INFO(&LNO_default_pool,
        NULL, NULL, NULL, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
        &LNO_default_pool);
      Set_Do_Loop_Info(d[i], dli);
      Is_True(Get_Do_Loop_Info(d[i]) == dli, ("Big bug"));

      DO_LOOP_INFO* olddli;
      if (t->Rectangular()) {
        INT num = t->Stripsz(i);
        olddli= Get_Do_Loop_Info(stack.Bottom_nth(first_in_stack+t->Iloop(i)));

        dli->Est_Num_Iterations = (olddli->Est_Num_Iterations + num - 1) / num;
        olddli->Est_Num_Iterations = num;

        if (olddli->Est_Max_Iterations_Index != -1) {
          dli->Est_Max_Iterations_Index = 
            (olddli->Est_Max_Iterations_Index + num - 1) / num;
        }
        if (olddli->Est_Max_Iterations_Index == -1 ||
            olddli->Est_Max_Iterations_Index > num) {
          olddli->Est_Max_Iterations_Index = num;
        }
      }
      else {
        //TODO OK, but at some point get a better estimate, somewhat easy
        olddli = Get_Do_Loop_Info(innerloop);
        dli->Est_Num_Iterations = (olddli->Est_Num_Iterations + 19) / 20;
        olddli->Est_Num_Iterations = 20;
      }
      dli->Is_Ivdep = olddli->Is_Ivdep;
      dli->Is_Concurrent_Call = olddli->Is_Concurrent_Call;
      dli->Concurrent_Directive = olddli->Concurrent_Directive;
      dli->Num_Iterations_Symbolic = olddli->Num_Iterations_Symbolic;
      if (dli->Est_Num_Iterations < 1)
	dli->Est_Num_Iterations = 1;

      LWN_Copy_Linenumber(innerloop, d[i]);
    }

    if (outerdo) {
      region.First = outerdo;
      region.Last = outerdo;

      SNL_Change_Reduction_Loop_Stmts(plist, stack.Bottom_nth(first_in_stack), 
        outerdo);
    }

    // Step 4:
    // Here we solve for the i variables and update the bounds.  No need
    // to project anything away.

    s1.Add_Soe(&s1x);	// adding bounds outside helps remove redundant inside
    Row_Echelon(&s1, lcols, khtcols);
    Rewrite_Bounds(td, &s1, NULL, NULL, NULL, &stack, first_in_stack, 1);

    LWN_Insert_Block_After(block, prev, innerloop);

    // Step 5: update access vectors and dependences.  This code is similar
    // to that for unimodular updating, in that we can update anything
    // perfectly nested in place, and anything imperfectly nested we can simply
    // recompute.

    BOOL ttransformation_invalid = FALSE;

    DOLOOP_STACK shortstack(&LNO_local_pool);
    Build_Doloop_Stack(LWN_Get_Parent(outerdo), &shortstack);
    Renumber_Loops(outerdo, outerdo, dg);
    LNO_Build_Access(outerdo, &shortstack, &LNO_default_pool);

    // this isn't actually necessary when recomputing
    if (outerdo) {
      SNL_Expand_Reduction_Deps(outerdo);
      // this is necessary because of imperfectly nested loops
      Fix_Do_Du_Info(outerdo, td, TRUE, NULL, 0);
    }

    // NOTE: DEPENDENCE UPDATE CODE BELOW IDENTICAL TO THE EQUIVALENT
    // ABOVE FOR PERMUTATION, EXCEPT FOR THE ACTUAL PART THAT DOES THE
    // IN PLACE UPDATE

    // keep in hash table a list of all edges we want to transform.
    // 0: already transformed.  (Thus add_edge, which only adds new
    //    edges but doesn't add to this table, does the right thing.)
    // 1: untransformed.
    // We can delete right away, as long as we set to zero, since add
    // edge may reuse, but that's already transformed.

    SNL_Rebuild_Access_Arrays(stack.Bottom_nth(first_in_stack)); 
    LS_IN_LOOP loop_ls(stack.Bottom_nth(first_in_stack), dg, &LNO_local_pool);
    INT        curdepth = loop_ls.Good_Depth - lcols;
    INT        maxdepth = loop_ls.Good_Depth + nloops - lcols;

    // put edges in table that go from a vertex in loop_ls to a vertex
    // in loop_ls

    HASH_TABLE<EINDEX16,INT>
	edge_table(MIN(dg->Get_Edge_Count(),512),&LNO_local_pool);

    // only want to transform arcs from inside the loop to inside

    WN* awn;
    LS_IN_LOOP_ITER ali1(&loop_ls);
    while (awn = ali1.Step()) {
      Is_True(loop_ls.In(awn) > 0, ("Bug in ls_in_loop_iter"));
      DOLOOP_STACK astack(&LNO_local_pool);
      Build_Doloop_Stack(awn, &astack);
      VINDEX16 v = dg->Get_Vertex(awn);
      if (v == 0)
	continue;

      for (EINDEX16 e = dg->Get_Out_Edge(v); e; e = dg->Get_Next_Out_Edge(e)) {
        WN* bwn = dg->Get_Wn(dg->Get_Sink(e));
        if (bwn && loop_ls.In(bwn))
          edge_table.Enter(e, 1);
      }
    }

    // start with all the interior nodes

    LS_IN_LOOP_ITER ali(&loop_ls);
    while (awn = ali.Step()) {
      INT alex = loop_ls.In(awn);
      DOLOOP_STACK astack(&LNO_local_pool);
      Build_Doloop_Stack(awn, &astack);
      Is_True(alex > 0, ("Bug in ls_in_loop_iter"));
      VINDEX16 v = dg->Get_Vertex(awn);
      if (v == 0)
	continue;

      EINDEX16 enext = 0;
      for (EINDEX16 e = dg->Get_Out_Edge(v); e; e = enext) {
	enext = dg->Get_Next_Out_Edge(e);
	if (edge_table.Find(e) != 1)
	  continue;

	INT components = dg->Depv_Array(e)->Num_Dim();
        FmtAssert(components >= curdepth,
                  ("edge=%d components=%d curdepth=%d",
                   e, components, curdepth));

        WN* bwn = dg->Get_Wn(dg->Get_Sink(e));
        INT blex = loop_ls.In(bwn);

	if (t->Rectangular() && components >= maxdepth) {
          // update in place in this important and easy special case
          // the edges between vertices both inside the inner loop of the SNL

          for (INT s = 0; s < t->Strips(); s++) {
            // TODO: In some cases, this is still too slow.  For example,
            // three-deep double blocking, so this loops four times.
            // And it would be bad to block gmtry for
            // memory and a two-level cache.  Then again, anyone blocking
            // for that big a memory won't mind some compile time?
            // Still, probably not too hard to rewrite this routine to take
            // a range of s.

            if (SNL_Update_Strip_Dependence(loop_ls.Depth - lcols, s,
                                            t->Iloop(s),
                                            e, awn, bwn, alex, blex)) {
              ttransformation_invalid = TRUE;
              SNL_DEBUG1(1, "general tiling e=%d seemingly illegal?!\n", e);
            }
          }
          edge_table.Remove(e);
	}
      }
    }

    // now do remaining (non-interior) nodes.  Since this adds edges,
    // cannot merge with above for loop!

    LS_IN_LOOP_ITER ali2(&loop_ls);
    while (awn = ali2.Step()) {
      INT alex = loop_ls.In(awn);
      DOLOOP_STACK astack(&LNO_local_pool);
      Build_Doloop_Stack(awn, &astack);
      Is_True(alex > 0, ("Bug in ls_in_loop_iter"));
      VINDEX16 v = dg->Get_Vertex(awn);
      if (v == 0)
	continue;

      EINDEX16 enext = 0;
      for (EINDEX16 e = dg->Get_Out_Edge(v); e; e = enext) {
	enext = dg->Get_Next_Out_Edge(e);
	if (edge_table.Find(e) != 1)
	  continue;

        // get rid of symetric edge
        EINDEX16 econj = dg->Get_Edge(dg->Get_Sink(e), dg->Get_Source(e));

        // recompute edge
        WN* bwn = dg->Get_Wn(dg->Get_Sink(e));
        INT blex = loop_ls.In(bwn);
        Is_True(blex > 0, ("Bug in ls_in_loop_iter"));
        if (econj && e != econj && edge_table.Find(econj)) {
          if (enext == econj)
            enext = dg->Get_Next_Out_Edge(e);
          edge_table.Remove(econj);
          dg->Delete_Array_Edge(econj);
        }
        edge_table.Remove(e);
        dg->Delete_Array_Edge(e);

        DOLOOP_STACK bstack(&LNO_local_pool);
        Build_Doloop_Stack(bwn, &bstack);
        BOOL ok = dg->Add_Edge( awn, &astack, bwn, &bstack, alex < blex);
        if (!ok) { 
          LNO_Erase_Dg_From_Here_In(awn, dg); 
          LNO_Erase_Dg_From_Here_In(bwn, dg); 
        }
      }
    }
    if (warn_lexneg && ttransformation_invalid)
      DevWarn("General tiling transformation invalid?!");
  }

  if (Cur_PU_Feedback) {
    for (INT i = 0; i <= first_in_stack; ++i) {
      WN *cur_loop = stack.Bottom_nth(first_in_stack);
      WN *tile_loop = Outer_Tile(cur_loop, Du_Mgr);
      if (tile_loop) {
	if (WN_MAP32_Get(WN_MAP_FEEDBACK, tile_loop) == 0) {
	  INT32 count = WN_MAP32_Get(WN_MAP_FEEDBACK, cur_loop);
	  INT32 step  = WN_MAP32_Get(WN_MAP_FEEDBACK, WN_step(cur_loop));
	  step = MAX(step/20,1);
	  INT num_iter = MAX(1,step/count);
	  LWN_Set_Frequency(WN_start(cur_loop), MAX(count/20,1));
	  LWN_Set_Frequency(cur_loop, MAX(count/20,1));
	  
	  for (INT j = 0; j<stack.Elements(); j++) {
	    WN* inter_loop = stack.Bottom_nth(j);
	    if (inter_loop == tile_loop) {
	      LWN_Set_Frequency(tile_loop, count);
	      LWN_Set_Frequency(WN_start(tile_loop), count);
	      LWN_Set_Frequency(WN_step(tile_loop), count*num_iter);
	      break;
	    } else {
	      INT32 count1 = WN_MAP32_Get(WN_MAP_FEEDBACK, inter_loop);
	      LWN_Set_Frequency(WN_step(inter_loop), count*num_iter);
	      LWN_Set_Frequency(WN_start(inter_loop), count1*num_iter);
	      LWN_Set_Frequency(inter_loop, count1*num_iter);
	      count = count1;
	    }
	  }
	}
      }
    }
  }

  if (uinv)
    CXX_DELETE(uinv, &LNO_local_pool);
  IMAT::Set_Default_Pool(old_dflt_pool);
  CXX_DELETE(td, &LNO_local_pool);
  MEM_POOL_Pop(&LNO_local_pool);

  if (t)
    Renumber_Loops(region.First, region.Last, dg);
  if (!Valid_SNL_Region(region)) 
    DevWarn("SNL_GEN_U_Ctiling: Invalid SNL_REGION [0x%p,0x%p]",
      region.First, region.Last); 
  return region;
}

//-----------------------------------------------------------------------
// NAME: SNL_GEN_Permute_Loops 
// FUNCTION: Permute the loops in the nest whose outermost loop is 
//   'outer_loop' according the 'permutation' of length 'nloops'.  Return
//   the new outermost loop. If 'warn_lexneg' and a lexicographically 
//   dependence is produced, print a DevWarn.  Use a general permutation
//   technique. 
//-----------------------------------------------------------------------

extern WN* SNL_GEN_Permute_Loops(WN* wn_outer,
                                 INT permutation[],
                                 INT nloops, 
				 BOOL warn_lexneg)
{
  if (nloops == 0 || Identity_Permutation(permutation, nloops))
    return wn_outer; 

  if (LNO_Verbose) { 
    Print_Interchange(stdout, wn_outer, permutation, nloops); 
    Print_Interchange(TFile, wn_outer, permutation, nloops); 
  } 
  IMAT* unimodular = 
    CXX_NEW(IMAT(nloops, nloops, &LNO_local_pool), &LNO_local_pool);
  for (INT i = 0; i < nloops; i++)
    for (INT j = 0; j < nloops; j++)
      (*unimodular)(i,j) = j == permutation[i] ? 1 : 0;
  SNL_BOUNDS_INFO* bi = 
    CXX_NEW(SNL_BOUNDS_INFO(&LNO_local_pool), &LNO_local_pool); 
  DOLOOP_STACK stack(&LNO_local_pool); 
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops);
  Build_Doloop_Stack(wn_inner, &stack); 
  bi->Outermost_Depth() = Do_Loop_Depth(wn_outer);
  for (INT d = Do_Loop_Depth(wn_outer); d <= Do_Loop_Depth(wn_inner); d++)
    bi->Collect_Do_Info(stack.Bottom_nth(d));
  bi->Conditionals().Add_Vars(bi->Bounds().Num_Vars() -
    bi->Conditionals().Num_Vars());
  bi->Canonicize(nloops, &stack, Do_Loop_Depth(wn_outer));
  SNL_GEN_U_Ctiling(wn_outer, nloops, unimodular, NULL, bi, NULL, 
    EST_REGISTER_USAGE(), warn_lexneg); 
  return wn_outer; 
}

//-----------------------------------------------------------------------
// ROUTINES FOR REGISTER TILING 
//-----------------------------------------------------------------------

//-----------------------------------------------------------------------
// NAME: Twod_Setbound
// FUNCTION: Sets <bndst,offset> to be max(L(i), ... , L(i+u-1)), with a +1 
//   if imperfect above.  But optimizes away the max when easy.  Return the
//   stmt.  It's inserted in the list, so just return it for informational
//   purposes. 
// WARNING: I'm not sure of the exact meaning of all of these parameters,
//   so I'll attempt to fill them in later.
//-----------------------------------------------------------------------

static WN* Twod_Setbound(SNL_MONO m, 
			 SYMBOL symbol, 
			 WN* loop0, 
			 WN* loop1,
			 INT u, 
			 BOOL lb, 
			 BOOL imperfect)
{
  ARRAY_DIRECTED_GRAPH16*	dg = Array_Dependence_Graph;
  DU_MANAGER*			du = Du_Mgr;

  TYPE_ID	wtype = Do_Wtype(loop1);
  WN*		ploop1 = LWN_Get_Parent(loop1);
  OPCODE	ldop = OPCODE_make_op(OPR_LDID, wtype, wtype);
  OPCODE	stop = OPCODE_make_op(OPR_STID, MTYPE_V, wtype);

  WN*           alias_wn = NULL;
  WN*		bexpr;

  // set bexpr to be the old bound.  The new bound is i'.  The createldid
  // is fine because there is no alias analysis necessary.

  if (lb) {
    bexpr = WN_kid0(WN_start(loop1));
    alias_wn = WN_CreateLdid(ldop, symbol.WN_Offset(), symbol.St(),
                             Be_Type_Tbl(wtype));
    WN_kid0(WN_start(loop1)) = alias_wn;
    LWN_Set_Parent(WN_kid0(WN_start(loop1)), WN_start(loop1));
  }
  else {
    Upper_Bound_Standardize(WN_end(loop1));
    bexpr = SNL_UBexp(WN_end(loop1));
    alias_wn = WN_CreateLdid(ldop, symbol.WN_Offset(), symbol.St(),
                             Be_Type_Tbl(wtype));
    SNL_UBexp(WN_end(loop1)) = alias_wn;
    LWN_Set_Parent(SNL_UBexp(WN_end(loop1)), WN_end(loop1));
  }
  Create_alias(Alias_Mgr, alias_wn);

  // store expression before the loop

  WN* stmt = LWN_CreateStid(stop, alias_wn, bexpr);
  LWN_Copy_Linenumber(loop1, stmt);
  LWN_Copy_Frequency_Tree(stmt,loop1);
  LWN_Insert_Block_Before(ploop1, loop1, stmt);
  du->Add_Def_Use(stmt, alias_wn);
  du->Ud_Get_Def(alias_wn)->Set_loop_stmt(loop0);

  // now generate the code to compute the new lower bound correctly.
  // <bndst,offset> currently gets L(i), but we want it to get
  // max(L(i), ... ,L(i+u-1)), with a +1 if necessary, simplified.

  SYMBOL	sym0 = SYMBOL(WN_index(loop0));
  sym0.Type = Do_Wtype(loop0);

  if (m == SNL_MONO_OTHER) {
    OPCODE   opc = OPCODE_make_op(lb ? OPR_MAX : OPR_MIN, wtype, MTYPE_V);
    for (INT i = 1; i < u; i++) {
      WN* texpr = SNL_Copy_Exp(bexpr);
      Add_To_Symbol(texpr, i, sym0);
      bexpr = LWN_CreateExp2(opc, bexpr, texpr);
    }
    WN_kid0(stmt) = bexpr;
    LWN_Set_Parent(bexpr, stmt);
  }
  else if (m == SNL_MONO_INVARIANT ||
	   (m == SNL_MONO_DEC && lb) ||
	   (m == SNL_MONO_INC && !lb)) {
    // we want B(i), and we got B(i)
  }
  else {
    // we want B(i+u-1), but we got B(i)
    Add_To_Symbol(bexpr, u-1, sym0);
  }

  if (imperfect)
    Increase_By(stmt, (lb ? 1 : -1), NULL, -1);

  return stmt;
}

//-----------------------------------------------------------------------
// NAME: Set_Loop_Statements
// FUNCTION: Set each loop statement in 'newbody' with the value of 'wn_old'
//   to 'wn_new'. 
//-----------------------------------------------------------------------

static void Set_Loop_Statements(WN* newbody, 
				WN* wn_old,
				WN* wn_new)
{
  LWN_ITER* itr = LWN_WALK_TreeIter(newbody);
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn_ldid = itr->wn;
    if (!WN_operator(wn_ldid))
      continue;
    DEF_LIST* def_list = Du_Mgr->Ud_Get_Def(wn_ldid);
    if (def_list != NULL && def_list->Loop_stmt() == wn_old)
      def_list->Set_loop_stmt(wn_new);
  }
}

//-----------------------------------------------------------------------
// NAME: SNL_GEN_2D_Regtile
// FUNCTION: Perform a general 2D register tiling on the loop nest speci-
//   fied by 'ni' using the tile size 'tilesz' for the register tiled loop.
// NOTE: If any scalars need expanding, just don't register tile, and
//   print a warning.
//-----------------------------------------------------------------------

extern SNL_REGION SNL_GEN_2D_Regtile(SNL_NEST_INFO* ni, 
				     INT tilesz)
{
  const INT     bufsz = 64;
  char          buf[bufsz];
  INT           bufcnt;

  DOLOOP_STACK* stack = &ni->Dostack();
  INT           first_in_stack = ni->Depth_Inner() - 1;
  WN*           loop0 = stack->Bottom_nth(first_in_stack);
  WN*           loop1 = stack->Bottom_nth(first_in_stack+1);

  SNL_REGION	region(loop0,loop0);

  MEM_POOL_Push_Freeze(&SNL_local_pool);

  {
    ARRAY_DIRECTED_GRAPH16*	dg = Array_Dependence_Graph;
    DU_MANAGER*			du = Du_Mgr;

    EST_REGISTER_USAGE est_register_usage =
      Get_Do_Loop_Info(loop1)->Est_Register_Usage;

    Is_True(Step_Size(loop0) == 1, ("Unit step required: loop 0"));
    Is_True(Step_Size(loop1) == 1, ("Unit step required: loop 1"));

    ST*	        st0 = WN_st(WN_index(loop0));
    ST*         st1 = WN_st(WN_index(loop1));
    WN_OFFSET	offset0 = WN_offset(WN_index(loop0));
    WN_OFFSET	offset1 = WN_offset(WN_index(loop1));
    TYPE_ID	wtype0 = Do_Wtype(loop0);
    TYPE_ID	wtype1 = Do_Wtype(loop1);
    TY_IDX	ty0 = Be_Type_Tbl(wtype0);
    TY_IDX	ty1 = Be_Type_Tbl(wtype1);
    SYMBOL	symbol0(WN_index(loop0));
    WN*		alias_wn0 = WN_step(loop0);
    WN*		alias_wn1 = WN_step(loop1);

    symbol0.Type = wtype0;

    OPCODE	op_le0 = OPCODE_make_op(OPR_LE, Boolean_type, wtype0);
    OPCODE	op_le1 = OPCODE_make_op(OPR_LE, Boolean_type, wtype1);
    OPCODE	op_ldid0 = OPCODE_make_op(OPR_LDID, wtype0, wtype0);
    OPCODE	op_ldid1 = OPCODE_make_op(OPR_LDID, wtype1, wtype1);
    OPCODE	op_stid0 = OPCODE_make_op(OPR_STID, MTYPE_V, wtype0);
    OPCODE	op_stid1 = OPCODE_make_op(OPR_STID, MTYPE_V, wtype1);
    OPCODE	op_add0 = OPCODE_make_op(OPR_ADD, wtype0, MTYPE_V);
    OPCODE	op_add1 = OPCODE_make_op(OPR_ADD, wtype1, MTYPE_V);

    // make a copy of the entire loop, except replace the
    // bounds with DO i' = i, i+B-1, and replace all the i's inside by i'.

    WN* nest_copy = LWN_Copy_Tree(loop0, TRUE, LNO_Info_Map);
    WN* wn_holder[2];
    wn_holder[0] = loop0;
    wn_holder[1] = nest_copy;
    LWN_Scale_Frequency(nest_copy, 1.0/tilesz);
    LWN_Scale_Frequency(WN_start(nest_copy), 1.0/tilesz);
    LWN_Scale_Frequency(WN_index(nest_copy), 1.0/tilesz);
    if (red_manager) red_manager->Unroll_Update(wn_holder, 2);
    Unrolled_DU_Update(wn_holder, 2, Do_Loop_Depth(loop0)-1, TRUE, FALSE);
    if (!dg->Add_Deps_To_Copy_Block(loop0, nest_copy, FALSE)) {
      SNL_DEBUG0(0, "Add_Deps_To_Copy_Block() failed -- continuing");
      LWN_Update_Dg_Delete_Tree(nest_copy, dg);
      LNO_Erase_Dg_From_Here_In(nest_copy, dg); 
      LWN_Delete_Tree(nest_copy);
      MEM_POOL_Pop_Unfreeze(&SNL_local_pool);
      if (!Valid_SNL_Region(region))
        DevWarn("SNL_General_2D_Regtile: Invalid SNL_REGION [0x%p,0x%p]",
          region.First, region.Last);
      return region;
    }

    // Delete the S1 and S2 parts in the original code -- we will retrieve
    // them later from nest_copy if necessary

    BOOL	imperf_above = WN_prev(loop1) ? TRUE : FALSE;
    BOOL	imperf_below = WN_next(loop1) ? TRUE : FALSE;

    // Do the inner unrolling.

    region = SNL_Regtile_Loop(loop0, tilesz, 2, TRUE,
                              est_register_usage,
                              &ni->Privatizability_Info(),
                              ni->Depth_Inner() - 1, TRUE);

    // now that we've wound this loop down, we don't need S1 or S2 in the
    // original copy.  It's stored in the nest copy, and will be handled
    // appropriately there.

    if (imperf_above || imperf_below) {
      WN*  wnprnt = LWN_Get_Parent(loop1);
      WN*  wnnext  = NULL;
      for (WN* wn = WN_first(wnprnt); wn; wn = wnnext) {
        wnnext = WN_next(wn);
        if (wn != loop1) {
          LWN_Extract_From_Block(wnprnt, wn);
          LWN_Delete_Tree(wn);
        }
      }
    }

    // Now replace i by i' in the nest copy, except i'=i,i+B-1.
    // Replacing i by i' does not change the DU/UD information, but
    // changing the bounds does.

    bufcnt = sprintf(buf, "$r2d_");
    (SYMBOL(WN_index(loop0))).Name(buf+bufcnt, bufsz-bufcnt);
    Replace_Symbol(nest_copy, SYMBOL(st0,offset0,wtype0),
                   Create_Preg_Symbol(buf, wtype0), NULL, nest_copy);

    LWN_Delete_Tree(WN_kid0(WN_start(nest_copy)));
    WN* startld = LWN_CreateLdid(op_ldid0, alias_wn0);
    WN_kid0(WN_start(nest_copy)) = startld;
    LWN_Set_Parent(WN_kid0(WN_start(nest_copy)), WN_start(nest_copy));

    LWN_Delete_Tree(SNL_UBexp(WN_end(nest_copy)));
    WN* ubld = LWN_CreateLdid(op_ldid0, alias_wn0);
    SNL_UBexp(WN_end(nest_copy), NULL) = LWN_CreateExp2(op_add0, ubld,
                                              LWN_Make_Icon(wtype0, tilesz-1));
    LWN_Set_Parent(SNL_UBexp(WN_end(nest_copy), NULL), WN_end(nest_copy));

    // the du info gets screwed up.  Handle the two uses of i in the bounds,
    // and all the uses of i'.
    // TODO: why doesn't replace symbol handle the i' correctly?

    Fix_Do_Du_Info(WN_start(nest_copy), NULL, FALSE, loop0, 0);
    Fix_Do_Du_Info(WN_end(nest_copy), NULL, FALSE, loop0, 0);
    Fix_Do_Du_Info(nest_copy, NULL, TRUE, NULL, 0);  // get the i' variable

    // Make the loop0 code look like:
    //
    // DO i = Li, Ui, B
    //   L** = min(L(i), ... , L(i+B-1))
    //   L* = max(L(i), ... , L(i+B-1)) + (S1 exists ? 1 : 0)
    //   U* = min(U(i), ... , U(i+B-1)) - (S2 exists ? 1 : 0)
    //   U** = max(U(i), ... , U(i+B-1))
    //   if (L* <= U*)				//note: sometimes not necessary
    //     // large strip execution
    //   else
    //     // small strip execution
    // ENDDO
    //
    // Handling the L and U variables above is easy.  So is handling the
    // i's.  It's the rest of the variables that are tricky.  But once again
    // we use the "faking an unroll technique".

    SNL_MONO	lmono = Mono(WN_kid0(WN_start(loop1)), symbol0);
    SNL_MONO	umono = Mono(SNL_UBexp(WN_end(loop1)), symbol0);

    // Possible that L** < L*?  That U** > U*?

    BOOL		lextra = imperf_above || lmono != SNL_MONO_INVARIANT;
    BOOL		uextra = imperf_below || umono != SNL_MONO_INVARIANT;

    // begin by computing lstar and ustar, the new lower bounds

    SYMBOL lstar = Create_Preg_Symbol("$lstar", wtype0);
    SYMBOL ustar = Create_Preg_Symbol("$ustar", wtype0);

    WN* lstid = Twod_Setbound(lmono, lstar, loop0, loop1, tilesz,
                              TRUE, imperf_above);
    WN* ustid = Twod_Setbound(umono, ustar, loop0, loop1, tilesz,
                              FALSE, imperf_below);

    // so see if lstar <= ustar

    // TODO OK would be good to optimize away this if test, which we expect to
    // be unnecessary fairly often (though not always), but it's not that
    // easy.  Still, with some thought, it can be done in certain cases.
    // Setting up a system of equations to test whether we can prove L* <= U*
    // should be easy and fast.  But we won't do it for now.

    WN* luse = LWN_CreateLdid(op_ldid1, lstid);
    WN* uuse = LWN_CreateLdid(op_ldid1, ustid);
    du->Add_Def_Use(lstid, luse);
    du->Add_Def_Use(ustid, uuse);
    du->Ud_Get_Def(luse)->Set_loop_stmt(loop0);
    du->Ud_Get_Def(uuse)->Set_loop_stmt(loop0);

    WN* if1 = LWN_CreateIf(LWN_CreateExp2(op_le1, luse, uuse),
                           WN_CreateBlock(), WN_CreateBlock());
    WN* p1block = LWN_Get_Parent(loop1);
    LWN_Insert_Block_Before(p1block, loop1, if1);
    LWN_Copy_Linenumber(loop1, if1);
    LWN_Copy_Frequency_Tree(if1, loop1);
    LWN_Extract_From_Block(loop1);

    // annotate the if
    BOOL has_regions=Find_SCF_Inside(if1,OPC_REGION)!=NULL;
    IF_INFO *ii = CXX_NEW(IF_INFO(&LNO_default_pool,TRUE,has_regions),
                          &LNO_default_pool);
    WN_MAP_Set(LNO_Info_Map,if1,(void *)ii);
    DOLOOP_STACK* stackx = CXX_NEW(DOLOOP_STACK(&LNO_local_pool),
                                  &LNO_local_pool);
    Build_Doloop_Stack(if1, stackx);
    LNO_Build_If_Access(if1, stackx);
    CXX_DELETE(stackx, &LNO_local_pool);

    // The else part is easiest.  Generate a copy of the do loop, but with a
    // new outermost index that starts at i and goes to i+B-1.
    // and replace all i by that new index.
    // This is called "Small strip execution".

    LWN_Insert_Block_After(WN_else(if1), NULL, nest_copy);
    // the then part

    // change loop1 bounds to be from L* to U*.

    LWN_Delete_Tree(WN_kid0(WN_start(loop1)));
    WN_kid0(WN_start(loop1)) = LWN_CreateLdid(op_ldid1, lstid);
    LWN_Set_Parent(WN_kid0(WN_start(loop1)), WN_start(loop1));

    LWN_Delete_Tree(SNL_UBexp(WN_end(loop1)));
    SNL_UBexp(WN_end(loop1)) = LWN_CreateLdid(op_ldid1, ustid);
    LWN_Set_Parent(SNL_UBexp(WN_end(loop1)), WN_end(loop1));

    du->Add_Def_Use(lstid, WN_kid0(WN_start(loop1)));
    du->Add_Def_Use(ustid, SNL_UBexp(WN_end(loop1), NULL));

    // The unrolled loop goes in the then part

    LWN_Insert_Block_After(WN_then(if1), NULL, loop1);
    Fix_Do_Du_Info(loop0, NULL, TRUE, NULL, 0);

    // before and after the unrolled loop

    if (lextra) {

      // do i' = i, i+B-1, N
      //  S1
      //  do j = L(i'), L*-1
      //	 S(i',j)
      //  enddo
      // enddo

      WN* lpcopy = LWN_Copy_Tree(nest_copy, TRUE, LNO_Info_Map);
      wn_holder[0] = nest_copy;
      wn_holder[1] = lpcopy;
      if (red_manager) red_manager->Unroll_Update(wn_holder, 2);
      Unrolled_DU_Update(wn_holder, 2, Do_Loop_Depth(nest_copy)-1, TRUE, FALSE);
      if (!dg->Add_Deps_To_Copy_Block(nest_copy, lpcopy, FALSE)) {
        SNL_DEBUG0(0, "Add_Deps_To_Copy_Block() failed -- continuing");
        LNO_Erase_Dg_From_Here_In(lpcopy, dg); 
        LWN_Update_Dg_Delete_Tree(lpcopy, dg);
        Unmapped_Vertices_Here_Out(LWN_Get_Parent(nest_copy));
      }

      // delete any S2 part, and set wn to be the inner loop

      WN* wn_prev = NULL;
      WN *wn;
      for (wn = WN_last(WN_do_body(lpcopy)); wn; wn = wn_prev) {
        wn_prev = WN_prev(wn);
        if (WN_opcode(wn) == OPC_DO_LOOP)
          break;
        LWN_Extract_From_Block(WN_do_body(lpcopy), wn);
        LWN_Delete_Tree(wn);
      }
      Is_True(wn, ("where'd the DO go?"));

      // Set the inner loop upper bound to L* - 1.
      // Note that if lmono == SNL_MONO_INVARIANT then it must be the case that
      // the inner loop iterates exactly once, in which case we can optimize.

      if (lmono == SNL_MONO_INVARIANT) {
	WN* wn_encloser = Enclosing_Loop(LWN_Get_Parent(wn));
	FmtAssert(wn_encloser != NULL, ("Not a 2D regtile??"));
        WN* newbody = WN_do_body(wn);
        WN_do_body(wn) = WN_CreateBlock();	// empty block so deletes ok
	Set_Loop_Statements(newbody, wn, wn_encloser);
        LWN_Set_Parent(WN_do_body(wn), wn);
        Replace_Ldid_With_Exp_Copy(SYMBOL(st1,offset1,MTYPE_V),
                                   newbody, WN_kid0(WN_start(wn)), du);
        LWN_Insert_Block_Before(WN_do_body(lpcopy), wn, newbody);
        LWN_Delete_Tree(LWN_Extract_From_Block(WN_do_body(lpcopy), wn));
      }
      else {
        WN* luse = LWN_CreateLdid(op_ldid1, lstid);
        du->Add_Def_Use(lstid, luse);
        LWN_Delete_Tree(SNL_UBexp(WN_end(wn), NULL));
        SNL_UBexp(WN_end(wn)) = LWN_CreateExp2(op_add1, luse,
                                             LWN_Make_Icon(wtype1, -1));
        LWN_Set_Parent(SNL_UBexp(WN_end(wn), NULL), WN_end(wn));
        DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn);
        dli->Est_Num_Iterations = 2;	// something small
        dli->Is_Ivdep = FALSE;            // conservative
        dli->Is_Concurrent_Call = FALSE;            // conservative
        dli->Concurrent_Directive = FALSE;            // conservative
        dli->Num_Iterations_Symbolic = FALSE;
	dli->Num_Iterations_Profile = FALSE;
      }

      LWN_Insert_Block_After(WN_then(if1), NULL, lpcopy);
      // the du info gets screwed up.  Handle the two uses of i in the bounds,
      // and all the uses of i'.
      Fix_Do_Du_Info(WN_start(lpcopy), NULL, TRUE, loop0, 0);
      Fix_Do_Du_Info(WN_end(lpcopy), NULL, TRUE, loop0, 0);
      Fix_Do_Du_Info(lpcopy, NULL, TRUE, NULL, 0);
    }

    if (uextra) {

      // do i' = i, i+B-1, N
      //  do j = U*+1, U(i')
      //	 S(i',j)
      //  enddo
      //  S2
      // enddo

      WN* lpcopy = LWN_Copy_Tree(nest_copy, TRUE, LNO_Info_Map);
      wn_holder[0] = nest_copy;
      wn_holder[1] = lpcopy;
      if (red_manager) red_manager->Unroll_Update(wn_holder, 2);
      Unrolled_DU_Update(wn_holder, 2, Do_Loop_Depth(nest_copy)-1, TRUE, FALSE);
      if (!dg->Add_Deps_To_Copy_Block(nest_copy, lpcopy, FALSE)) {
        SNL_DEBUG0(0, "Add_Deps_To_Copy_Block() failed -- continuing");
        LNO_Erase_Dg_From_Here_In(lpcopy, dg);  
	Unmapped_Vertices_Here_Out(LWN_Get_Parent(nest_copy)); 
        LWN_Update_Dg_Delete_Tree(lpcopy, dg);
      }

      // delete any S1 part, and set wn to be the inner loop

      WN* wn_next = NULL;
      WN *wn;
      for (wn = WN_first(WN_do_body(lpcopy)); wn; wn = wn_next) {
        wn_next = WN_next(wn);
        if (WN_opcode(wn) == OPC_DO_LOOP)
          break;
        LWN_Extract_From_Block(WN_do_body(lpcopy), wn);
        LWN_Delete_Tree(wn);
      }
      Is_True(wn, ("where'd the DO go?"));

      // Set the inner loop lower bound to U* + 1.
      // Note that if umono == SNL_MONO_INVARIANT then it must be the case that
      // the inner loop iterates exactly once, in which case we can optimize.

      if (umono == SNL_MONO_INVARIANT) {
	WN* wn_encloser = Enclosing_Loop(LWN_Get_Parent(wn));
	FmtAssert(wn_encloser != NULL, ("Not a 2D regtile??"));
        WN* newbody = WN_do_body(wn);
        WN_do_body(wn) = NULL;
	Set_Loop_Statements(newbody, wn, wn_encloser);
        Replace_Ldid_With_Exp_Copy(SYMBOL(st1,offset1,MTYPE_V),
                                   newbody, SNL_UBexp(WN_end(wn)), du);
        LWN_Insert_Block_Before(WN_do_body(lpcopy), wn, newbody);
        LWN_Delete_Tree(LWN_Extract_From_Block(WN_do_body(lpcopy), wn));
      }
      else {
        WN* uuse = LWN_CreateLdid(op_ldid1, ustid);
        du->Add_Def_Use(ustid, uuse);
        LWN_Delete_Tree(WN_kid0(WN_start(wn)));
        WN_kid0(WN_start(wn)) = LWN_CreateExp2(op_add1, uuse,
                                  LWN_Make_Icon(wtype1, 1));
        LWN_Set_Parent(WN_kid0(WN_start(wn)), WN_start(wn));
        DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn);
        dli->Est_Num_Iterations = 2;	// something small
        dli->Is_Ivdep = FALSE;            // conservative
        dli->Is_Concurrent_Call = FALSE;            // conservative
        dli->Concurrent_Directive = FALSE;            // conservative
        dli->Num_Iterations_Symbolic = FALSE;
        dli->Num_Iterations_Profile = FALSE;
      }

      LWN_Insert_Block_Before(WN_then(if1), NULL, lpcopy);
      // the du info gets screwed up.  Handle the two uses of i in the bounds,
      // and all the uses of i'.
      Fix_Do_Du_Info(WN_start(lpcopy), NULL, TRUE, loop0, 0);
      Fix_Do_Du_Info(WN_end(lpcopy), NULL, TRUE, loop0, 0);
      Fix_Do_Du_Info(lpcopy, NULL, TRUE, NULL, 2);
    }

    // We've done everything except update the dependences and the actual
    // unrolling.

    // Update the dependences.
    // It's way too complicated to update all the dependences intelligently.
    // But by using Add_Deps_To_Copy_Block above, all the dependences should
    // be fine except inside the loop.  So we'll recompute all those only.
    // Note that we have no idea which array references have edges.  So
    // we'll just iterate through them all.  Since updating an edge may update
    // both directions, we just delete all possible relevant edges first and
    // recompute.  Note: reductions are not an issue, since recomputing.
    // Note: the dependences from loop1 to loop1 should already be correct
    // because SNL_Regtile_Loop took care of that.  So don't recompute those.

    SNL_Rebuild_Access_Arrays(loop0); 
    LS_IN_LOOP loop_ls0(loop0, dg, &SNL_local_pool);
    LS_IN_LOOP loop_ls1(loop1, dg, &SNL_local_pool);

    LS_IN_LOOP_ITER ali(&loop_ls0);
    WN* awn;
    while (awn = ali.Step()) {
      VINDEX16 v = dg->Get_Vertex(awn);
      if (v == 0)
        continue;
      EINDEX16 nexte = 0;
      BOOL     ain1 = loop_ls1.In(awn) > 0;
      for (EINDEX16 e = dg->Get_Out_Edge(v); e; e = nexte) {
        nexte = dg->Get_Next_Out_Edge(e);
        Is_True(v == dg->Get_Source(e), ("Huh?"));
        WN* wsink = dg->Get_Wn(dg->Get_Sink(e));
        if (loop_ls0.In(wsink) && !(ain1 && loop_ls1.In(wsink)))
          dg->Delete_Array_Edge(e);
      }
    }

    INT count = 0;

    BOOL failed = FALSE;
    Renumber_Loops(region.First, region.Last, dg);

    ali = &loop_ls0;
    while (awn = ali.Step()) {
      INT     alex = loop_ls0.In(awn);
      BOOL    ain1 = loop_ls1.In(awn) > 0;

      if (OPCODE_is_load(WN_opcode(awn)))
        continue;

      DOLOOP_STACK astack(&SNL_local_pool);
      Build_Doloop_Stack(awn, &astack);
      Is_True(alex > 0, ("Bug in ls_in_loop_iter"));

      VINDEX16 v = dg->Get_Vertex(awn);
      if (v == 0)
        continue;

      LS_IN_LOOP_ITER ali2(&loop_ls0);
      WN* bwn;
      while (bwn = ali2.Step()) {
        VINDEX16 v2 = dg->Get_Vertex(bwn);
        if (v2 == 0)
          continue;

        BOOL is_load = OPCODE_is_load(WN_opcode(bwn));
        INT blex = loop_ls0.In(bwn);
        if (blex == 0 || (!is_load && blex < alex))
          continue;	// don't want to do write-write pairs twice

        if (ain1 && loop_ls1.In(bwn))
          continue;     // don't recompute when both in loop1

        // recompute edge
        DOLOOP_STACK bstack(&SNL_local_pool);
        Build_Doloop_Stack(bwn, &bstack);
        count++;

#ifdef KEY
	// Bug 2946 - when creating scalar expansion tiles, we must take care 
	// not to make loop nests deeper than 15, which is the maximum allowed 
	// for dependence vectors (this is also noted in sxlist.cxx by nenad).
	// Reduction manager is initialzed in lnopt_main.cxx only if
	// ROUNDOFF_LEVEL >= ROUNDOFF_ASSOC (for Cse, zmult). Without reduction
	// manager, SX_INFO makes worst case assumption that scalars are not
	// scalar expandable. If the reduction manager is initialized, then 
	// outer loops are marked SE okay (privatizable scalars), and we end up 
	// with a 16-deep loop nest. This is the reason bug shows up only with
	// -OPT:Ofast (-OPT:ro=2).
	if (astack.Elements() >= 15 ||
	    bstack.Elements() >= 15) {
	  LNO_Erase_Dg_From_Here_In(awn, dg); 
	  LNO_Erase_Dg_From_Here_In(bwn, dg);
          failed = TRUE;
          goto out;
	}
#endif
        // TODO: don't use bounds info when recomputing dependences. This 
        // makes the run time tolerable.  Remove the optional last parameter
        // and watch it crawl.

        // like code elsewhere, but not using bounds.  Also, of course,
        // aopr is known to not be a store.
        BOOL ok = dg->Add_Edge( awn, &astack, bwn, &bstack, alex < blex,
                               FALSE);
        if (!ok) {
          LNO_Erase_Dg_From_Here_In(awn, dg); 
	  LNO_Erase_Dg_From_Here_In(bwn, dg);  
          failed = TRUE;
          goto out;
        }
      }
    }

   out:
    if (failed) {
      // play it safe: delete all the vertices for this loop
      LS_IN_LOOP_ITER ali(&loop_ls0);
      WN* awn;
      while (awn = ali.Step()) {
        VINDEX16 v = dg->Get_Vertex(awn);
        if (v) {
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
        }
      }
    }

    SNL_DEBUG1(2, "Updated %d pairs for 2D regblock\n", count);

    Renumber_Loops(region.First, region.Last, dg);

    // Update the access_vectors.

    for (WN* wn_reg = region.First; wn_reg; wn_reg = WN_next(wn_reg)) {
      DOLOOP_STACK shortstack(&SNL_local_pool);
      Build_Doloop_Stack(LWN_Get_Parent(wn_reg), &shortstack);
      LNO_Build_Access(wn_reg, &shortstack, &LNO_default_pool);
      Optimize_Coupled_Loops(wn_reg, Du_Mgr); 
      if (wn_reg == region.Last)
        break;
    }
  }

  MEM_POOL_Pop_Unfreeze(&SNL_local_pool);

  return region;
}

