/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

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


// -*-C++-*-

/**
*** This file contains some of the code to implement SNL transformations.
*** In addition, two other files snl_gen.cxx and snl_inv.cxx include code
*** used to implement specifically the general and invariant transforma-
*** tions. 
***/

#include <stdint.h>
#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#define snl_CXX      "snl.cxx"
const static char *rcs_id =   snl_CXX "$Revision: 1.8 $";

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
#include "fb_info.h"
#include "fb_whirl.h"

#pragma weak New_Construct_Id 

//-----------------------------------------------------------------------
// DEBUGGING STUFF
//-----------------------------------------------------------------------

char* ___SNL_Pcall = NULL;	// so dbx can look at any location
void* ___SNL_Pvall = NULL;	// so dbx can look at any location

//-----------------------------------------------------------------------
// REDUCTION UTILITIES
//-----------------------------------------------------------------------

//-----------------------------------------------------------------------
// NAME: SNL_Change_Reduction_Loop_Stmts
// FUNCTION: Find all the loop_stmts within the do loop 'from' that are part 
//   of a reduction.  Change those that point to 'from' and instead make them 
//   point to 'to'.  If 'plist' is NULL, search the 'from' loop to find the 
//   reductions.  Otherwise, assume that they are all mentioned on 'plist'. 
//-----------------------------------------------------------------------

extern void SNL_Change_Reduction_Loop_Stmts(SX_PLIST* plist, 
                                            WN* from,
                                            WN* to)
{
  DU_MANAGER* du = Du_Mgr; 
  SNL_DEBUG2(3, "SNL_Change_Reduction_Loop_Stmts(0x%p, 0x%p)", from, to);
  FmtAssert(WN_opcode(to) == OPC_DO_LOOP && WN_opcode(from) == OPC_DO_LOOP,
            ("Bad to opcode %d or %d", WN_opcode(to), WN_opcode(from)));

  if (plist != NULL) { 
    SX_PITER          ii(plist);
    INT cnt = 0;
    SX_PNODE *n;
    for (n = ii.First(); n; n = ii.Next()) {
      if (n->Has_Reduction())
	cnt++;
    }
    if (cnt == 0)
      return;               // by far the most likely outcome
   
    const SYMBOL**          syms = CXX_NEW_ARRAY(const SYMBOL*, cnt,
						 &MEM_local_pool);
    SX_PITER          iii(plist);
    INT                     c = 0;

    for (n = iii.First(); n; n = iii.Next()) {
      if (n->Has_Reduction())
	syms[c++] = &n->Symbol();
    }

    LWN_ITER* itr = LWN_WALK_TreeIter(WN_do_body(from));
    for ( ; itr; itr = LWN_WALK_TreeNext(itr)) {
      WN*      wn = itr->wn;
      OPERATOR opr = WN_operator(wn);
      if (opr == OPR_LDID) {
	SYMBOL symbol(wn);
	for (INT i = 0; i < cnt; i++) {
	  if (symbol == *syms[i]) { 
	    WN* loop_stmt = du->Ud_Get_Def(wn)->Loop_stmt();
	    if (loop_stmt == from) {
	      du->Ud_Get_Def(wn)->Set_loop_stmt(to);
	      SNL_DEBUG2(3, "SNL_Change_Reduction_Loop_Stmts: "
			 "loop_stmt(0x%p)->0x%p", wn, to);
	    }
	    break;
	  }
	}
      }
    } 
  } else {
    if (red_manager == NULL) 
      return; 
    LWN_ITER* itr = LWN_WALK_TreeIter(WN_do_body(from));
    for ( ; itr; itr = LWN_WALK_TreeNext(itr)) {
      WN* wn = itr->wn;
      OPERATOR opr = WN_operator(wn);
      if (opr == OPR_LDID && red_manager->Which_Reduction(wn) != RED_NONE) {
        WN* loop_stmt = du->Ud_Get_Def(wn)->Loop_stmt();
        if (loop_stmt == from) {
          du->Ud_Get_Def(wn)->Set_loop_stmt(to);
          SNL_DEBUG2(3, "SNL_Change_Reduction_Loop_Stmts: "
                     "loop_stmt(0x%p)->0x%p", wn, to);
        }
      }
    } 
  } 
}

//-----------------------------------------------------------------------
// NAME: SNL_Expand_Reduction_Deps
// FUNCTION: Find all reductions within the 'loop' and expand dependences 
//   so that lexneg component as well as lexpos component included.  Since 
//   dependences may be made lexneg, we must be sure that processing of 
//   these dependences will be analyzed.
//-----------------------------------------------------------------------

extern void SNL_Expand_Reduction_Deps(WN* loop)
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  REDUCTION_MANAGER* rm = red_manager; 
  if (rm == NULL)
    return;

  for (EINDEX16 e = dg->Get_Edge(); e; e = dg->Get_Next_Edge(e)) {
    VINDEX16 v1 = dg->Get_Source(e);
    VINDEX16 v2 = dg->Get_Sink(e);

    if (v1 == 0 || v2 == 0)
      continue;

    WN* wn1 = dg->Get_Wn(v1);
    WN* wn2 = dg->Get_Wn(v2);
    FmtAssert(wn1 && wn2, ("Missing v->wn mapping"));

    // make sure both are within the loop
    WN* p1 = wn1;
    while (p1 && p1 != loop)
      p1 = LWN_Get_Parent(p1);
    if (p1 == NULL)
      continue;

    WN* p2 = wn2;
    while (p2 && p2 != loop)
      p2 = LWN_Get_Parent(p2);
    if (p2 == NULL)
      continue;

    INT wr1 = -1;
    INT wr2 = -1;
    if ((wr1=rm->Which_Reduction(wn1)) == 0 ||
        (wr2=rm->Which_Reduction(wn2)) == 0 ||
        (wr1 != wr2))
      continue;

    // find the edge with the opposite source and sink.
    if (v1 == v2) {
      DEPV_ARRAY* array_e = dg->Depv_Array(e);
      FmtAssert(array_e, ("Edge %d (e) has no depv array!", e));

      DEPV_LIST dl1(array_e, &LNO_local_pool);
      DEPV_LIST dl2(array_e, &LNO_local_pool);

      DEPV_LIST* dl_new1 = Lex_Pos_Compose(&LNO_local_pool, &dl1, &dl2);
      Delete_DEPV_ARRAY(array_e, dg->Pool());

      array_e = Create_DEPV_ARRAY(dl_new1, dg->Pool());
      FmtAssert(array_e, ("Create_DEPV_ARRAY() returned NULL"));
      dg->Set_Depv_Array(e, array_e);
      CXX_DELETE(dl_new1, &LNO_local_pool);

      if (snl_debug >= 3) {
        fprintf(TFile, "changed reduction edge %d to: ", e);
        dg->Depv_Array(e)->Print(TFile);
      }
    }
    else if (v1 < v2) { // do this only once
      EINDEX16 econj = dg->Get_Edge(v2, v1);

      if (econj) {
        DEPV_ARRAY* array_e = dg->Depv_Array(e);
        DEPV_ARRAY* array_econj = dg->Depv_Array(econj);
        FmtAssert(array_e, ("Edge %d (e) has no depv array!", e));
        FmtAssert(array_econj, ("Edge %d (econj) has no depv array!", econj));
        DEPV_LIST dl1a(array_e, &LNO_local_pool);
        DEPV_LIST dl1b(array_e, &LNO_local_pool);
        DEPV_LIST dl2a(array_econj, &LNO_local_pool);
        DEPV_LIST dl2b(array_econj, &LNO_local_pool);

        DEPV_LIST* dl_new1 = Lex_Pos_Compose(&LNO_local_pool, &dl1a, &dl2a);
        DEPV_LIST* dl_new2 = Lex_Pos_Compose(&LNO_local_pool, &dl2b, &dl1b);
        Delete_DEPV_ARRAY(array_e, dg->Pool());
        Delete_DEPV_ARRAY(array_econj, dg->Pool());

        array_e = Create_DEPV_ARRAY(dl_new1, dg->Pool());
        array_econj = Create_DEPV_ARRAY(dl_new2, dg->Pool());
        FmtAssert(array_e, ("Create_DEPV_ARRAY() returned NULL"));
        FmtAssert(array_econj, ("Create_DEPV_ARRAY() returned NULL"));
        dg->Set_Depv_Array(e, array_e);
        dg->Set_Depv_Array(econj, array_econj);
        CXX_DELETE(dl_new1, &LNO_local_pool);
        CXX_DELETE(dl_new2, &LNO_local_pool);

        if (snl_debug >= 3) {
          fprintf(TFile, "changed reduction edge %d and %d to ", e, econj);
          dg->Depv_Array(e)->Print(TFile);
          fprintf(TFile, " and ");
          dg->Depv_Array(econj)->Print(TFile);
        }
      }
    }
  }
}

//-----------------------------------------------------------------------
// NAME: SNL_Test_Reduction_Lexneg 
// FUNCTION: Return TRUE if dependence on edge 'e' between nodes 'awn' and 
//   'bwn' is lexicographically negative.  The integers 'alex' and 'blex' 
//   are the lexicographic position of 'awn' and 'bwn'.  However, if the 
//   the edge 'e' represents a reduction, take the positive part of the 
//   dependence and return FALSE. 
//-----------------------------------------------------------------------

extern BOOL SNL_Test_Reduction_Lexneg(EINDEX16 e,
                                      WN* awn, 
				      WN* bwn, 
				      INT alex, 
				      INT blex)
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  REDUCTION_MANAGER* rm = red_manager;
  if (e == 0) {
    Is_True(0, ("Bad edge into SNL_Test_Reduction_Lexneg()"));
    return FALSE;
  }
  if (Is_Lexpos(dg->Depv_Array(e)))
    return FALSE;

  Is_True(awn == dg->Get_Wn(dg->Get_Source(e)),
          ("Bad awn 0x%p 0x%p", awn, dg->Get_Wn(dg->Get_Source(e))));
  Is_True(bwn == dg->Get_Wn(dg->Get_Sink(e)),
          ("Bad bwn 0x%p 0x%p", bwn, dg->Get_Wn(dg->Get_Sink(e))));

  if (rm != NULL &&
      rm->Which_Reduction(awn) && 
      rm->Which_Reduction(bwn) && 
      (rm->Which_Reduction(awn) ==
       rm->Which_Reduction(bwn))) {

    DEPV_ARRAY* array_e = dg->Depv_Array(e);
    FmtAssert(array_e, ("Edge %d has no depv array!", e));

    DEPV_LIST dl(array_e, &LNO_local_pool);
    DEPV_LIST pos(dl.Num_Dim(), dl.Num_Unused_Dim(), &LNO_local_pool);
    DEPV_LIST neg(dl.Num_Dim(), dl.Num_Unused_Dim(), &LNO_local_pool);
    dl.Lex_Pos_Decompose(&LNO_local_pool, &pos, &neg, alex < blex, alex > blex);
    DEPV_ARRAY* array = Create_DEPV_ARRAY(&pos, dg->Pool());
    if (array) {
      Delete_DEPV_ARRAY(array_e, dg->Pool());
      dg->Set_Depv_Array(e, array);
    }
    else
      dg->Delete_Array_Edge(e);

    if (snl_debug >= 3) {
      fprintf(TFile, 
        "SNL_Test_Reduction_Lexneg: made reduction edge %d into: ", e);
      if (array)
        array->Print(TFile);
      else
        fprintf(TFile, "<NULL>\n");
      fflush(TFile);
    }

    return FALSE;
  }
  else {
    if (snl_debug) {
      fprintf(TFile, "SNL_Test_Reduction_Lexneg: edge=%d lexneg:", e);
      dg->Depv_Array(e)->Print(TFile);
    }
    return TRUE;
  }
}

//-----------------------------------------------------------------------
// ROUTINES FOR USELESS LOOP REMOVAL
//-----------------------------------------------------------------------

//-----------------------------------------------------------------------
// NAME: SNL_Remove_Unity_Trip_Loop
// FUNCTION: Remove the unity trip loop 'wdloop'.  If 'update_access', 
//   update the access vectors. and 'update_do_depths' update the do 
//   loop depths.  Returns the SNL_REGION of the code replacing 'wdloop'.
//-----------------------------------------------------------------------

extern SNL_REGION SNL_Remove_Unity_Trip_Loop(WN* wdloop, 
					     BOOL update_access)
{
  WN* wn_first = NULL;
  WN* wn_last = NULL; 
  Remove_Unity_Trip_Loop(wdloop, update_access, &wn_first, &wn_last, 
   Array_Dependence_Graph, Du_Mgr); 
  SNL_REGION region;
  region.First = wn_first; 
  region.Last = wn_last; 
  if (!Valid_SNL_Region(region))
    DevWarn("SNL_Remove_Unity_Trip_Loop: Invalid SNL_REGION [0x%p,0x%p]",
      region.First, region.Last);
  return region; 
}

//-----------------------------------------------------------------------
// NAME: RUL_Region_Update
// FUNCTION: Update the 'region' given that Remove_Useless_Loops() 
//   returned 'local_region' and was called with 'wn'.  The nodes 
//   'wn_prev' and 'wn_next' were WN_prev(wn) and WN_next(wn) when 
//   Remove_Useless_Loops() was called. 
//-----------------------------------------------------------------------

static void RUL_Region_Update(SNL_REGION* region,
			      SNL_REGION* local_region,
			      WN* wn, 
			      WN* wn_prev,
			      WN* wn_next) 
{
  if (!Valid_SNL_Region(*local_region))
    DevWarn("RUL_Region_Update: Invalid Local SNL_REGION [0x%p,0x%p]",
      local_region->First, local_region->Last);
  if (local_region->First == NULL) { 
    FmtAssert(local_region->Last == NULL, 
      ("RUL_Region_Update: First NULL but not last")); 
    if (region->First == wn)
      region->First = wn_next; 
    if (region->Last == wn)
      region->Last = wn_prev; 
    if (!Valid_SNL_Region(*region))
      DevWarn("RUL_Region_Update: Invalid SNL_REGION [0x%p,0x%p]",
	region->First, region->Last);
    return; 
  } 
  if (region->First == wn && local_region->First != wn)
    region->First = local_region->First;
  if (region->Last == wn && local_region->Last != wn)
    region->Last = local_region->Last;
  if (!Valid_SNL_Region(*region))
    DevWarn("RUL_Region_Update: Invalid Input SNL_REGION [0x%p,0x%p]",
      region->First, region->Last);
}
					  

//-----------------------------------------------------------------------
// NAME: SNL_Remove_Useless_Loops
// FUNCTION: Remove the zero-trip loops and reduce the unity trip loops
//   nested within 'wn_tree'.  If 'update_access' update the access
//   vectors. Retuen the SNL_REGION of the code replacing 'wn_tree'. 
//-----------------------------------------------------------------------

extern SNL_REGION SNL_Remove_Useless_Loops(WN* wn_tree,
                                           BOOL update_access)
{
  SNL_REGION region(wn_tree, wn_tree);
  SNL_REGION local_region(wn_tree, wn_tree);
  if (WN_opcode(wn_tree) == OPC_DO_LOOP) {
    INT iteration_count = Iterations(wn_tree, &LNO_local_pool);
    if (iteration_count == 1) {
      local_region = SNL_Remove_Unity_Trip_Loop(wn_tree, update_access);
      if (region.First == wn_tree)
        region.First = local_region.First;
      if (region.Last == wn_tree)
        region.Last = local_region.Last;
      WN* wnn = NULL; 
      for (WN* wn = region.First; wn != NULL; wn = wnn) {
 	wnn = WN_next(wn);
	WN* old_region_last = region.Last;  	
        WN* wn_prev = WN_prev(wn);
        WN* wn_next = WN_next(wn); 
	local_region = SNL_Remove_Useless_Loops(wn, update_access); 
	RUL_Region_Update(&region, &local_region, wn, wn_prev, wn_next); 
	if (wn == old_region_last)
	  break; 
      }
      if (!Valid_SNL_Region(region))
        DevWarn("SNL_Remove_Useless_Loops: Invalid SNL_REGION [0x%p,0x%p]",
          region.First, region.Last);
      return region; 
    } 
    if (iteration_count == 0) {
      WN* wn_before = WN_prev(wn_tree);
      WN* wn_after = WN_next(wn_tree);
      Remove_Zero_Trip_Loop(wn_tree); 
      if (region.First == wn_tree && region.Last == wn_tree) {
	region.First = NULL; 
	region.Last = NULL; 
      } else if (region.First == wn_tree) {
        region.First = wn_after;
      } else if (region.Last == wn_tree) {
        region.Last = wn_before;
      }
      if (!Valid_SNL_Region(region))
        DevWarn("SNL_Remove_Useless_Loops: Returning SNL_REGION [0x%p,0x%p]",
          region.First, region.Last);
      return region;
    }
  }
  if (WN_opcode(wn_tree) == OPC_BLOCK) {
    WN* wnn = NULL; 
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = wnn) {
      wnn = WN_next(wn); 
      WN* wn_prev = WN_prev(wn); 
      WN* wn_next = WN_next(wn); 
      local_region = SNL_Remove_Useless_Loops(wn, update_access);
      RUL_Region_Update(&region, &local_region, wn, wn_prev, wn_next); 
    } 
  } else {
    for (INT i = 0; i < WN_kid_count(wn_tree); i++) {
      WN* wn = WN_kid(wn_tree, i);
      local_region = SNL_Remove_Useless_Loops(wn, update_access);
      RUL_Region_Update(&region, &local_region, wn, NULL, NULL); 
    }
  }
  if (!Valid_SNL_Region(region))
    DevWarn("SNL_Remove_Useless_Loops: Invalid SNL_REGION [0x%p,0x%p]",
      region.First, region.Last);
  return region;
}

//-----------------------------------------------------------------------
// NAME: Remove_Useless_Loops 
// FUNCTION: Remove useless unity and zero trip loops from the 'region'.
//-----------------------------------------------------------------------

extern void Remove_Useless_Loops(SNL_REGION* region)
{
  WN* wnn = NULL; 
  for (WN* wn = region->First; wn != NULL; wn = wnn) {
    wnn = WN_next(wn); 
    WN* old_region_last = region->Last; 
    WN* wn_prev = WN_prev(wn); 
    WN* wn_next = WN_next(wn); 
    SNL_REGION local_region = SNL_Remove_Useless_Loops(wn, TRUE);
    RUL_Region_Update(region, &local_region, wn, wn_prev, wn_next);
    if (wn == old_region_last)
      break;  
  }
}     

//-----------------------------------------------------------------------
// ROUTINES FOR INTERCHANGE
//-----------------------------------------------------------------------

//-----------------------------------------------------------------------
// NAME: Print_Interchange
// FUNCTION: Print an informative message which describes an interchange
//   which has been performed.  The message is printed to 'file', and 
//   indicates that the SNL with outermost loop 'outer_loop' has had the
//   'permutation' of length 'nloops' applied to it. 
//-----------------------------------------------------------------------

extern void Print_Interchange(FILE* file,
                              WN* outer_loop,
                              INT permutation[],
                              INT nloops)
{
  fprintf(file, "Interchange: (");
  INT i;
  for (i = 0; i < nloops; i++) {
    const char *name=WB_Whirl_Symbol(SNL_Get_Inner_Snl_Loop(outer_loop,i + 1));
    fprintf(file, "%s", name);
    if (i < nloops - 1)
      fprintf(file, ",");
  }
  fprintf(file, ") -> (");
  for (i = 0; i < nloops; i++) {
    const char *name = WB_Whirl_Symbol(SNL_Get_Inner_Snl_Loop(outer_loop,
      permutation[i] + 1));
    fprintf(file, "%s", name);
    if (i < nloops - 1)
      fprintf(file, ",");
  }
  fprintf(file, ") at ("); 
  for (i = 0; i < nloops; i++) {
    fprintf(file, "%d", Srcpos_To_Line(WN_linenum(SNL_Get_Inner_Snl_Loop(outer_loop, 
      i + 1))));
    if (i < nloops - 1)
      fprintf(file, ",");
  }
  fprintf(file, ")\n");
}

//-----------------------------------------------------------------------
// NAME: Scale_FB_Info_Loop
// FUNCTION: Multiply FB frequency of all fields of *filp by scale
//-----------------------------------------------------------------------

static void
Scale_FB_Info_Loop(FB_Info_Loop *filp, float scale)
{
  filp->freq_zero *= scale;
  filp->freq_positive *= scale;
  filp->freq_out *= scale;
  filp->freq_back *= scale;
  filp->freq_exit *= scale;
  filp->freq_iterate *= scale;
}

//-----------------------------------------------------------------------
// NAME: LNO_FB_Inv_Interchange
// FUNCTION: For the SNL with outermost loop 'wn_outer', update its
//   feedback information to reflect that the 'permutation' of length
//   'nloops' will be performed on it.
//-----------------------------------------------------------------------

extern void
LNO_FB_Inv_Interchange(WN* wn_outer, INT permutation[], INT nloops)
{
  Is_True(Cur_PU_Feedback, ("NULL Cur_PU_Feedback"));
  if (!permutation || nloops == 0 ||
      Identity_Permutation(permutation, nloops))
    return;

  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops);
  INT outer_depth = Do_Loop_Depth(wn_outer);
  DOLOOP_STACK stack(&LNO_local_pool);
  Build_Doloop_Stack(wn_inner, &stack);

#ifdef KEY
  // Return if any of the <wn_loop> has no loop feedback info.
  for( int i = 0; i < nloops; i++ ){
    const WN* wn_loop = stack.Bottom_nth(outer_depth + i);
    const FB_Info_Loop fb_info = Cur_PU_Feedback->Query_loop(wn_loop);
    if( fb_info.freq_positive.Uninitialized() )
      return;
  }
#endif

  INT i;
      // FB for each loop in the nest, from outermost to innermost
  FB_Info_Loop *old_fils = CXX_NEW_ARRAY(FB_Info_Loop, nloops,
                                         &LNO_local_pool);
  FB_Info_Loop *new_fils = CXX_NEW_ARRAY(FB_Info_Loop, nloops,
                                         &LNO_local_pool);
  for (i = 0; i < nloops; i++) {  // copy old FB values
    WN* wn_loop = stack.Bottom_nth(outer_depth + i);
    new_fils[i] = old_fils[i] = Cur_PU_Feedback->Query_loop(wn_loop);
  }

    // Update feedback for permuted loops. Because loop bounds are
    // invariant, we simply scale each loop's zero, positive, out, and back
    // FB values by the ratio (New / Old), where Old is the number of
    // invocations of the loop in the original nest, and New is the
    // invocations in the permuted nest. This should be a reasonable
    // approximation, but there are cases when it can be wrong. We would
    // need path-sensitive feedback data to get more precise values.
  for (i = 0; i < nloops; i++) {
    const INT idx = permutation[i];
    const FB_FREQ old_invokes = old_fils[idx].freq_zero +
                                old_fils[idx].freq_positive;
    FB_FREQ new_invokes;
    if (i > 0) {
        // inner loop invoked once per iteration of its outermore loop
      new_invokes = new_fils[permutation[i - 1]].freq_iterate;
    } else {
        // outermost loop invoked once per iteration of entire loop nest
      new_invokes = old_fils[0].freq_zero + old_fils[0].freq_positive;
    }

#ifdef KEY
    /* Handle situation when the inner loop is not called.
       IMO: lno does not update the feedback info consistently.
    */
    if( old_invokes.Zero() )
      Scale_FB_Info_Loop(&new_fils[idx], old_invokes.Value());
    else
#endif
      Scale_FB_Info_Loop(&new_fils[idx], (new_invokes / old_invokes).Value());
  }

  for (i = 0; i < nloops; i++) {  // set new FB values
    WN* wn_loop = stack.Bottom_nth(outer_depth + i);
    Cur_PU_Feedback->Annot_loop(wn_loop, new_fils[i]);
  }

  CXX_DELETE_ARRAY(old_fils, &LNO_local_pool);
  CXX_DELETE_ARRAY(new_fils, &LNO_local_pool);
} // LNO_FB_Inv_Interchange()

//-----------------------------------------------------------------------
// NAME: LNO_FB_MP_Tile
// FUNCTION: wn_orig_loop is a loop being processor-tiled by wn_tile_loop
//   (which has a tripcount of tile_loop_tripcount). Update feedback on
//   wn_orig_loop and wn_tile_loop to reflect tiling.
//-----------------------------------------------------------------------

extern void
LNO_FB_MP_Tile(WN* wn_tile_loop, INT tile_loop_tripcount, WN *wn_orig_loop)
{
  Is_True(Cur_PU_Feedback, ("NULL Cur_PU_Feedback"));
  Is_True(tile_loop_tripcount > 0, ("tile_loop_tripcount <= 0"));

  FB_Info_Loop orig_fil = Cur_PU_Feedback->Query_loop(wn_orig_loop),
               new_orig_fil = orig_fil, tile_fil = orig_fil;

    // update FB for wn_tile_loop
  tile_fil.freq_out = tile_fil.freq_positive;
  tile_fil.freq_iterate = tile_fil.freq_positive * tile_loop_tripcount;
  tile_fil.freq_back = tile_fil.freq_iterate - tile_fil.freq_positive;
  tile_fil.freq_exit = tile_fil.freq_out + tile_fil.freq_zero;

    // Update FB for wn_orig_loop:
    // Tiling preserves freq_iterate, which is an exact (measured) value.
    // freq_exit is dictated by wn_orig_loop being nested within wn_tile_loop.
  new_orig_fil.freq_exit = tile_fil.freq_iterate;
  if (new_orig_fil.freq_iterate.Value() >=
      9.0 * new_orig_fil.freq_exit.Value()) {

      // Normally, we expect positive to be much more than zero (say, 90%
      // versus 10%), and back to be much more than positive. So if iterate
      // is much larger than exit, we make guesses for back/positive/zero
      // that have these relative sizes.
    new_orig_fil.freq_zero = 0.1 * new_orig_fil.freq_exit;
          // freq_positive is 9 times as much as freq_zero
    new_orig_fil.freq_positive = new_orig_fil.freq_exit -
                                 new_orig_fil.freq_zero;
          // freq_back is at least 9 times as much as freq_positive
    new_orig_fil.freq_back = new_orig_fil.freq_iterate -
                             new_orig_fil.freq_positive;

  } else {

      // Otherwise, just make sure back/positive are in the ratio 90%/10%.
    new_orig_fil.freq_back = new_orig_fil.freq_iterate * 0.9;
    new_orig_fil.freq_positive = new_orig_fil.freq_iterate -
                                 new_orig_fil.freq_back;
    new_orig_fil.freq_zero = new_orig_fil.freq_exit -
                             new_orig_fil.freq_positive;
  }
    // freq_out is always identical to freq_positive
  new_orig_fil.freq_out = new_orig_fil.freq_positive;

  Cur_PU_Feedback->Annot_loop(wn_orig_loop, new_orig_fil);
  Cur_PU_Feedback->Annot_loop(wn_tile_loop, tile_fil);
} // LNO_FB_MP_Tile()


//-----------------------------------------------------------------------
// NAME: SNL_Permute_Loops
// FUNCTION: For the SNL with outermost loop 'wn_outer' and innermost loop 
//   'wn_inner' apply the 'permutation' of length 'nloops'.  If 'invariant',  
//   use an invariant algorithm, otherwise use a general algorithm.  If 
//   'warn_lexneg' is TRUE, issue a DevWarn for lexcographically negative 
//   dependences, otherwise recompute these bad dependences.
//-----------------------------------------------------------------------

extern WN* SNL_Permute_Loops(WN* wn_outer, 
			     WN* wn_inner, 
			     INT permutation[], 
			     INT nloops, 
			     BOOL invariant,
			     BOOL warn_lexneg)
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  INT outer_depth = Do_Loop_Depth(wn_outer);
  INT inner_depth = Do_Loop_Depth(wn_inner);
  FmtAssert(inner_depth - outer_depth + 1 == nloops,
    ("Inconsistent parameters to SNL_Permute_Loops"));
  DOLOOP_STACK stack(&LNO_local_pool); 
  Build_Doloop_Stack(wn_inner, &stack); 
  INT* spermutation = CXX_NEW_ARRAY(INT, nloops, &LNO_local_pool);
  WN* wn_new_outer = NULL; 
  INT last = -1; 
  for (INT first = 0; first < nloops; first = last + 1) { 
    last = Permutation_Last(first, permutation, nloops); 
    for (INT i = first; i <= last; i++) 
      spermutation[i - first] = permutation[i] - first; 
    INT snloops = last - first + 1; 
    WN* wn_souter = stack.Bottom_nth(outer_depth + first);  
    if (invariant) 
      wn_new_outer = SNL_INV_Permute_Loops(wn_souter, spermutation, snloops,
	warn_lexneg);
    else 
      wn_new_outer = SNL_GEN_Permute_Loops(wn_souter, spermutation, snloops,
	warn_lexneg); 
    if (!warn_lexneg)
      Repair_Bad_Dependences(wn_souter); 
    if (first == 0) 
      wn_new_outer = wn_new_outer; 
  }
  return wn_new_outer; 
}
			     
//-----------------------------------------------------------------------
// ROUTINES FOR TILING 
//-----------------------------------------------------------------------

//-----------------------------------------------------------------------
// NAME: SNL_Update_Strip_Dependence
// FUNCTION: Updates the dependence edge 'e' between nodes 'awn' (with lexi-
//   cographic position 'alex') and 'bwn' (with lexicographic position 'blex').
//   The depth of the deepest common loop is 'current_depth'.  The original 
//   index of the strip loop in the dependence vector is 's'.  In the per- 
//   mutation vector, 's' is moved to position 'i_for_s', i.e. t->Iloop(s) 
//   == 'i_for_s'.  The function returns TRUE if invalid. 
//-----------------------------------------------------------------------

extern BOOL SNL_Update_Strip_Dependence(INT current_depth,
                                        INT s,
                                        INT i_for_s,
                                        EINDEX16 e,
                                        WN* awn,
                                        WN* bwn,
                                        INT alex,
                                        INT blex)
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  
  // We have done a strip.  The dependence component of that strip
  // determines what the dependences will be for the corresponding
  // two loops that result after tiling.  Consider
  //
  //     for i=1,100 -> for i'=1,100,10 & for i = i',i'+9
  //
  // It is legal to basically keep the same dependence for i
  // and i', except there are some = that weren't there before.
  // Also note that if i' != I', then i != I.  Here's how the
  // dependence components are transformed.
  //
  //     (0)       goes to (0, 0),
  //     (+)       goes to (0, +), (+, +)
  //     (+=)      goes to (0, +=), (+, +)
  //     (-)       goes to (0, -), (-, -)
  //     (-=)      goes to (0, -=), (-, -)
  //     (+-)      goes to (*, *)
  //     (*)       goes to (*, *)
  //
  // This may be pessimistic.  When we know the blocking factor, and
  // when there are exact distances, then we can do better.  But this
  // is fine.

  // Ok, so edge from node in loop to node in loop.  Change the
  // dependence array according to the chart above.  The original
  // component was nloops+Iloops(s).  Now it's split tinto nloops+s
  // and nloops+strips+Iloop(s).  Ideally.  The only trouble is that
  // it's not really like that.  We are doing this strip by strip, and
  // we need the dependence graph correct (for wind-down code) between,
  // so it's slightly more complicated.  The original loop is no
  // longer nloops+Iloop(s), but rather nloops+s+Iloop(s), because we
  // have stripped s loops already.  That's also the inner resultant
  // loop.  The outer resultant loop is then inserted so that it is
  // the s loop.

  DEPV_ARRAY*  orig_dv = dg->Depv_Array(e);
  INT          ddepth = current_depth - orig_dv->Num_Unused_Dim();
  if (ddepth < 0) {
    DEPV_ARRAY* new_dv = Create_DEPV_ARRAY(orig_dv->Num_Vec(),
                                         orig_dv->Num_Dim(),
                                         orig_dv->Num_Unused_Dim() + 1,
                                         dg->Pool());
    for (INT ii = 0; ii < orig_dv->Num_Vec(); ii++) {
      DEPV* orig_depv = orig_dv->Depv(ii); 
      DEPV* new_depv = new_dv->Depv(ii); 
      for (INT jj = 0; jj < orig_dv->Num_Dim(); jj++) 
        DEPV_Dep(new_depv, jj) = DEPV_Dep(orig_depv, jj);
    } 
    dg->Set_Depv_Array(e, new_dv);
    Delete_DEPV_ARRAY(orig_dv, dg->Pool());
    if (SNL_Test_Reduction_Lexneg(e, awn, bwn, alex, blex))
      return TRUE;
    return FALSE;
  }

  INT nvec = 0;
  INT v;
  for (v = 0; v < orig_dv->Num_Vec(); v++) {
    DEPV* d = orig_dv->Depv(v);
    switch (DEP_Direction(DEPV_Dep(d, ddepth + i_for_s))) {
     case DIR_EQ:
     case DIR_STAR:
     case DIR_POSNEG:
      nvec++;
      break;
     default:
      nvec += 2;
      break;
    }
  }

  // Overly conservative for use in scalar expansion tiling
  // FmtAssert(orig_dv->Num_Dim() == ddepth + s + nloops,
  //	  ("Bad number of dimensions in dependence vector %d", e));

  // WARNING: We can only handle 255 (== 2^8-1) dependence vectors in 
  // a DEPV_ARRAY at this point!!

  BOOL conservative = nvec > UINT8_MAX;
  if (conservative)
    nvec = orig_dv->Num_Vec(); 
  DEPV_ARRAY* new_dv = Create_DEPV_ARRAY(nvec,
                                         orig_dv->Num_Dim()+1,
                                         orig_dv->Num_Unused_Dim(),
                                         dg->Pool());

  INT vcount = 0;

  //     (0)       goes to (0, ... , 0),
  //     (+/+=)    goes to (0, ... , +/+=), (+, ... , +)
  //     (-/-=)    goes to (0, ... , -/-=), (-, ... , -)
  //     (+-/*)    goes to (*, ... , *)

  for (v = 0; v < orig_dv->Num_Vec(); v++) {
    INT i;
    DEPV* d = orig_dv->Depv(v);
    DIRECTION dir = DEP_Direction(DEPV_Dep(d, ddepth + i_for_s));
    DEPV* dd1 = dir == DIR_STAR || dir == DIR_POSNEG ?
                                    NULL : new_dv->Depv(vcount++);
    DEPV* dd2 = conservative && dir != DIR_STAR && dir != DIR_POSNEG 
      || dir == DIR_EQ ? NULL : new_dv->Depv(vcount++);

    FmtAssert(dd1 != NULL || dd2 != NULL,
      ("SNL_Update_Strip_Dependence: Must produce at least one dep")); 

    // insert a 0 for the new strip, otherwise the same

    if (dd1) {
      for (i = 0; i < ddepth + s; i++)
        DEPV_Dep(dd1, i) = DEPV_Dep(d, i);
      if (conservative) {
	DIRECTION ndir = (dir == DIR_POS || dir == DIR_POSEQ) ? DIR_POSEQ 
		       : (dir == DIR_NEG || dir == DIR_NEGEQ) ? DIR_NEGEQ
		       : dir; 
        DEPV_Dep(dd1, i) = DEP_SetDirection(ndir); 
      } else {
        DEPV_Dep(dd1, i) = DEP_SetDistance(0);
      }
      for (; i < orig_dv->Num_Dim(); i++)
        DEPV_Dep(dd1, i+1) = DEPV_Dep(d, i);
    }

    // if (+/+=) -> (+, ..., +)
    // if (-/-=) -> (-, ..., -)
    // if (+-/*) -> (*, ..., *)
    // the old place

    if (dd2) {
      DIRECTION ndir = (dir == DIR_POS || dir == DIR_POSEQ) ? DIR_POS :
                       (dir == DIR_NEG || dir == DIR_NEGEQ) ? DIR_NEG :
                       DIR_STAR;
      DEP       ndep = DEP_SetDirection(ndir);
      for (i = 0; i < ddepth + s; i++)
        DEPV_Dep(dd2, i) = DEPV_Dep(d, i);
      DEPV_Dep(dd2, i) = ndep;
      for (; i < orig_dv->Num_Dim(); i++)
        DEPV_Dep(dd2, i+1) = DEPV_Dep(d, i);
      DEPV_Dep(dd2, ddepth + s + 1 + i_for_s) = ndep;
    }
  }
  Is_True(vcount == nvec, ("Bug in tile dependence stuff"));
  dg->Set_Depv_Array(e, new_dv);

  Delete_DEPV_ARRAY(orig_dv, dg->Pool());
  if (SNL_Test_Reduction_Lexneg(e, awn, bwn, alex, blex))
    return TRUE;
  return FALSE;
}

//-----------------------------------------------------------------------
// NAME: Tile_Loop  
// FUNCTION: Returns the outer tile after tiling loop 'wn_loop' with a 
//   the given 'tile_size'.  The 'tile_level' is the level of the tiling.
//   The 'reason' is the reason for tiling.  The 'outersym' is a symbol 
//   of the index variable for the new outer tile loop.  The 'dg', 'du',
//   and 'rm' are as usual.  The 'pool' is used for throwaway storage 
//   only. 
// NOTES: The 'tile_level' is normally 0, unless you are doing multilevel
//   cache tiling, in which case 0 indicates the L1 cache, 1 indicates 
//   the L2 cache, and so on.  The list of 'reason's is given in snl_
//   trans.cxx. 
//-----------------------------------------------------------------------

extern WN* Tile_Loop(WN* wn_loop, 
		     INT tile_size, 
		     INT tile_level, 
		     SNL_INV_CACHE_BLOCK_REASON reason, 
		     SYMBOL* outersym, 
                     MEM_POOL *pool)
{
  ARRAY_DIRECTED_GRAPH16 *dg = Array_Dependence_Graph; 
  DU_MANAGER* du = Du_Mgr; 
  REDUCTION_MANAGER* rm = red_manager; 
  INT iloop[1];
  INT stripsz[1];
  INT striplevel[1];
  WN* permloop[1];
  SNL_INV_CACHE_BLOCK_REASON reason_array[1]; 
  Upper_Bound_Standardize(WN_end(wn_loop), FALSE);
  iloop[0] = 0;
  stripsz[0] = tile_size; 
  striplevel[0] = tile_level;
  permloop[0] = wn_loop;
  reason_array[0] = reason; 
  SNL_TILE_INFO ti(1, 1, iloop, stripsz, striplevel, reason_array, pool);
  LS_IN_LOOP loop_ls(wn_loop, dg, pool);
  SNL_REGION region;
  region.First = wn_loop;
  region.Last = wn_loop;
  DOLOOP_STACK stack(&LNO_local_pool); 
  Build_Doloop_Stack(wn_loop, &stack);  
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
  if (Bound_Is_Too_Messy(dli->LB))
    Hoist_Lower_Bound(wn_loop, &stack, &LNO_default_pool); 
  if (Bound_Is_Too_Messy(dli->UB))
    Hoist_Upper_Bound(wn_loop, &stack, &LNO_default_pool); 
  WN* outer_tile = SNL_INV_Cache_Block(NULL, &ti, permloop, loop_ls, 
    &region, reason, outersym, pool);
  if (Cur_PU_Feedback) {
    INT32 orig_count = WN_MAP32_Get(WN_MAP_FEEDBACK, WN_start(wn_loop));
    if (orig_count > 0) {
      INT32 orig_test = WN_MAP32_Get(WN_MAP_FEEDBACK, WN_end(wn_loop));
      INT32 outer_count = orig_count;
      INT32 outer_test = MAX(orig_test/stripsz[0],1);
      LWN_Set_Frequency(outer_tile, outer_count);
      LWN_Set_Frequency(WN_start(outer_tile), outer_count);
      LWN_Set_Frequency(WN_step(outer_tile), outer_test-1);

      LWN_Set_Frequency(wn_loop, outer_test-1);
      LWN_Set_Frequency(WN_start(wn_loop), outer_test-1);
    }
  }

  DOLOOP_STACK dostack(pool);
  Build_Doloop_Stack(LWN_Get_Parent(outer_tile), &dostack);
  LNO_Build_Access(outer_tile, &dostack, &LNO_default_pool);
  return outer_tile;
}

//-----------------------------------------------------------------------
// ROUTINES FOR REGISTER TILING 
//-----------------------------------------------------------------------

//-----------------------------------------------------------------------
// NAME: SNL_Regtile_Loop
// FUNCTION: Register tile the 'outerloop' with an unrolling factor of 
//   'u'. 
// WARNING: I'm not sure of the exact meaning of all of these parameters,
//   so I'll attempt to fill them in later. 
//-----------------------------------------------------------------------

extern SNL_REGION SNL_Regtile_Loop(WN* outerloop,
				   INT u,
				   INT nloops,
				   BOOL unroll_just_inner,
                                   EST_REGISTER_USAGE est_register_usage,
                                   SX_INFO* pinfo,
                                   INT pinfo_depth,
				   BOOL no_further_unroll,
                                   HASH_TABLE<WN*,WN*>** loop_map_ptr,
                                   SX_INFO** wdpinfo_ptr)
{
  // SNL_Regtile_Loop() is passed a loop.  That loop is the outer loop being
  // unrolled.  u copies are made and passed to Dror's routine to update the
  // dependences.  Then they are grafted into one big loop.  E.g.
  // 	do i
  //	  S1
  //	  do j
  //	    S2
  // with u=2 means that we duplicate i's body and pass the original and body
  // to dror's routines.  Then we increment i in the copy and merge the bodies
  // back into the original i and j loops, adjusting the step of i (or is that
  // done elsewhere ... and I assume that the wind-down stuff is already taken
  // care of).  So we get
  // 	do i
  //	  S1; S1'
  //	  do j
  //	    S2; S2'
  // and we have only leftover copies of do j and empty bodies to delete.
  //
  // NOTE: SNL_Regtile_Loop() does not update the DO annotations properly.
  // That's ok, just call Renumber_Loops() after using it.
  // TODO OK: The privatizable info must be up-to-date.  Right now, if there is
  // no permutation or cache tiling occurs, then the pinfo isn't altered and
  // there is no problem; and if one of those occurs, then everything is
  // scalar expanded except the inner loop, so no problem.  But if some day
  // we scalar expand some variables (or none) and do a permutation, then the
  // pinfo must be updated.
  //
  // If wdpinfo_ptr is set, then wdpinfo is returned.  It's allocated from
  // LNO_local_pool and, if deleted, must be handles appropriately.  Likewise
  // for loop_map_ptr.
  // 
  // The "further_unrolling" only works for invariant nests, because with
  // general nests the body is mucked around with in a nasty way.  So pass
  // an indicator to that effect.

  ARRAY_DIRECTED_GRAPH16*	dg = Array_Dependence_Graph;
  DU_MANAGER*			du = Du_Mgr;

  ST*		st = WN_st(WN_index(outerloop));
  WN_OFFSET	offset = WN_offset(WN_index(outerloop));
  TYPE_ID	wtype = Do_Wtype(outerloop);
  SNL_REGION	region(outerloop, outerloop);
  INT           outerdepth = Do_Loop_Depth(outerloop);

  SYMBOL	indexsym(WN_index(outerloop));
  indexsym.Type = wtype;

  WN*                       wdloop = NULL;
  SX_INFO*                  wdpinfo = NULL;
  HASH_TABLE<WN*,WN*>*      loop_map = NULL;

  FmtAssert(u > 1, ("Register unrolling too little: %d\n", u));

  INT64 iters = Iterations(outerloop, &SNL_local_pool);
  BOOL do_winddown = (iters < 0 || iters%u);

  if (do_winddown) {
    // if the unrolled fit, so does this.  Otherwise, who knows.
    EST_REGISTER_USAGE ru;
    ru.Set_Fits(est_register_usage.Fits());
    wdloop = Wind_Down(outerloop, iters<0 ? (u+1)/2 : iters%u, FALSE, ru);
    region.Last = wdloop;

    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wdloop);
    dli->Est_Max_Iterations_Index = u;

    loop_map = Make_Loop_Mapping(outerloop, wdloop, &SNL_local_pool);

    // If the lower bound and upper bound are constant, replace the lower
    // bound of the wind down loop. 
    WN* lower_bound = WN_kid0(WN_start(outerloop)); 
    WN* upper_bound = SNL_UBexp(WN_end(outerloop)); 
    if (WN_operator(lower_bound) == OPR_INTCONST 
	&& WN_operator(upper_bound) == OPR_INTCONST) { 

      // Replace the lower bound of the wind down loop.
      INT64 lb = WN_const_val(lower_bound); 
      INT64 ub = WN_const_val(upper_bound); 
      INT64 wdlb = lb + iters/u * u; 
      
      LWN_Delete_Tree(WN_kid0(WN_start(wdloop)));
      WN* wd_lower_bound = LWN_Copy_Tree(lower_bound, TRUE, LNO_Info_Map); 
      WN_kid0(WN_start(wdloop)) = wd_lower_bound; 
      LWN_Copy_Frequency(wd_lower_bound, WN_start(wdloop));
      LWN_Set_Parent(wd_lower_bound, WN_start(wdloop));
      WN_const_val(wd_lower_bound) = wdlb; 
    }
    if ((wdloop && !no_further_unroll &&
         u >= LNO_Outer_Unroll_Min_For_Further_Unroll) ||
        wdpinfo_ptr) {
      wdpinfo = CXX_NEW(SX_INFO(*pinfo, outerloop,
                          loop_map, &SNL_local_pool), &SNL_local_pool);
    } 

    // make blocked loop have upper bound of u-1 less, step of u;
    // unnecessary in !do_winddown case
    Increase_By(SNL_UBexp(WN_end(outerloop)), -(u-1), WN_end(outerloop));
  }

  // adjust step

  INT64  ostep = Step_Size(outerloop, u);
  FmtAssert(ostep == 1, ("Non-unit step %lld for loop %s",
                         ostep, SYMBOL(WN_index(outerloop)).Name()));

  // Now unroll body (factor u) with index increased 1 further each time.

  WN** unroll_body = CXX_NEW_ARRAY(WN*, u, &SNL_local_pool);
  unroll_body[0] = outerloop;
  LWN_Scale_Frequency(WN_end(outerloop), 1.0/u);
  LWN_Scale_Frequency(WN_step(outerloop), 1.0/u);
  INT i;
  for (i = 1; i < u; i++) {
    unroll_body[i] = LWN_Copy_Tree(outerloop, TRUE, LNO_Info_Map);
    LWN_Scale_Frequency_Tree(unroll_body[i], 1.0/u);
  }
  
  if (!dg->Unrolled_Dependences_Update(unroll_body, u, Do_Depth(outerloop))) { 
    for (i = 0; i < u; i++) 
      LNO_Erase_Dg_From_Here_In(unroll_body[i], dg);
    Unmapped_Vertices_Here_Out(LWN_Get_Parent(outerloop)); 
  }  
  if (red_manager)
    red_manager->Unroll_Update(unroll_body, u);
  // the bodies are going inside, thus the wierd depth.
  Unrolled_DU_Update(unroll_body, u, outerdepth-1+nloops, FALSE, TRUE);

  // If there are privatizable scalars in this loop, or in loops farther
  // inside (including the innermost loop), make copies.  That is,
  // rename every privatizable scalar on in when duplicating the bodies.  

  INT privcnt = 0;
  SX_PITER ii(&pinfo->Plist);
  INT outer = Do_Depth(outerloop);
  for (SX_PNODE* n = ii.First(); !ii.Is_Empty(); n = ii.Next()) {
    switch (n->Transformable(outer)) {
     case SX_PNODE::SE_NOT_REQD:
      break;
     case SX_PNODE::SE_REQD:
      if (n->Expansion_Depth() >= pinfo_depth)
        privcnt++;
      break;
     case SX_PNODE::ILLEGAL:
      FmtAssert(0, ("Bug: can't expand scalar %s", n->Symbol().Name()));
      break;
     default:
      FmtAssert(0, ("Illegal value for SX_PNODE::STATUS"));
      break;
    }
  }

  if (privcnt > 0) {
    SYMBOL*     oldsyms = CXX_NEW_ARRAY(SYMBOL, privcnt, &SNL_local_pool);
    WN**        rloop   = CXX_NEW_ARRAY(WN*, privcnt, &SNL_local_pool);
    INT*        srqd    = CXX_NEW_ARRAY(INT, privcnt, &SNL_local_pool);
    INT*        nsrqd   = CXX_NEW_ARRAY(INT, privcnt, &SNL_local_pool);
    INT*        ed      = CXX_NEW_ARRAY(INT, privcnt, &SNL_local_pool);

    INT privcnt2 = 0;
    SX_PITER ii(&pinfo->Plist);
    SX_PNODE* nnext = NULL;
    for (SX_PNODE* n = ii.First(); n; n = nnext) {
      nnext = ii.Next();
      if (n->Transformable(outer) != SX_PNODE::SE_REQD)
        continue;

      if (n->Expansion_Depth() >= pinfo_depth) {
        oldsyms[privcnt2] = n->Symbol();
        rloop[privcnt2] = n->Reduction_Carried_By();
        srqd[privcnt2] = n->Outer_Se_Reqd();
        nsrqd[privcnt2] = n->Outer_Se_Not_Reqd();
        ed[privcnt2++] = n->Expansion_Depth();
	// don't remove pinfo!  AFter all, it's still there.
      }
    }
    FmtAssert(privcnt == privcnt2, ("Just checking .. easy to mess up"));

    SYMBOL*  newsyms = CXX_NEW_ARRAY(SYMBOL, (u-1)*privcnt, &SNL_local_pool);
    WN**     ancestors = CXX_NEW_ARRAY(WN*, privcnt, &SNL_local_pool);

    INT      newsymscnt = 0;
    for (INT i = 1; i < u; i++) {
      for (INT j = 0; j < privcnt; j++) {
        const INT  bufsz = 64;
        char       buf[bufsz];
        INT        bufcnt;

        ancestors[j] = unroll_body[i];  // same for all of them
        bufcnt = sprintf(buf, "$rse_");
	oldsyms[j].Name(buf+bufcnt, bufsz-bufcnt);
	SYMBOL newsym = Create_Preg_Symbol(buf, oldsyms[j].Type);
	newsyms[newsymscnt++] = newsym;
        pinfo->Enter(NULL, newsym, rloop[j], srqd[j], nsrqd[j], ed[j], 
	  FALSE, FALSE);
      }
      Replace_Symbols(unroll_body[i], oldsyms,
		      &newsyms[privcnt*(i-1)], privcnt, NULL, ancestors);
    }

    CXX_DELETE_ARRAY(oldsyms, &SNL_local_pool);
    CXX_DELETE_ARRAY(srqd, &SNL_local_pool);
    CXX_DELETE_ARRAY(nsrqd, &SNL_local_pool);
    CXX_DELETE_ARRAY(rloop, &SNL_local_pool);
    CXX_DELETE_ARRAY(ed, &SNL_local_pool);

    CXX_DELETE_ARRAY(newsyms, &SNL_local_pool);
    CXX_DELETE_ARRAY(ancestors, &SNL_local_pool);
  }

  for (i = 1; i < u; i++)
    Add_To_Symbol(unroll_body[i], i, indexsym, TRUE);

  // Grafting back in.  Find the next innermost body in each case
  // and throw them all back into the original nest.

  WN** loop = CXX_NEW_ARRAY(WN*, u, &SNL_local_pool);
  WN** nloop = CXX_NEW_ARRAY(WN*, u, &SNL_local_pool);

  for (i = 0; i < u; i++) {
    loop[i] = unroll_body[i];
    nloop[i] = Find_Next_Innermost_Do(loop[i]);
  }

  for (INT d = 0; d < nloops-1; d++) {

    // Find insertion point for statements below main loop body
    WN* wn_last = NULL;  
    for (WN* wn = WN_first(WN_do_body(loop[0])); wn != NULL; wn = WN_next(wn)) 
      wn_last = wn; 

    Is_True(nloop[0], ("no way"));
    if (!unroll_just_inner) {
      for (i = 1; i < u; i++) {
	if (WN_prev(nloop[i])) {
	  WN* above = LWN_Create_Block_From_Stmts_Above(nloop[i]);
	  LWN_Insert_Block_Before(LWN_Get_Parent(nloop[0]), nloop[0], above);
	}
      }
      for (i = u-1; i >= 1; i--) {
	if (WN_next(nloop[i])) {
	  WN* below = LWN_Create_Block_From_Stmts_Below(nloop[i]);
	  LWN_Insert_Block_After(LWN_Get_Parent(wn_last), wn_last, below);
	}
      }
    }

    for (i = 0; i < u; i++) {
      // the loops have DU information pointing uselessly to loops that are
      // about to go away.  Change that
      if (i != 0)
	SNL_Add_Du_To_Index_Ldid(loop[0], WN_do_body(loop[i]), du, TRUE);

      loop[i] = nloop[i];
      nloop[i] = Find_Next_Innermost_Do(loop[i]);
    }
  }

  for (i = 1; i < u; i++) {
    WN* bdy = WN_do_body(loop[i]);
    WN_do_body(loop[i]) = WN_CreateBlock();
    LWN_Insert_Block_Before(WN_do_body(loop[0]), NULL, bdy);
    LWN_Delete_Tree(unroll_body[i]);
  }

  // The following if is only true if we don't wish to generate
  //         do i = ... by 6
  //            ...
  //         do i = ... by 1 (wind-down)
  //            ...
  // but rather
  //         do i = ... by 6
  //            ...
  //         do i = ... by 2 (wind-down)
  //            ...
  //         do i = ... by 1 (wind-down)
  //            ...
  // When we unroll by a large unrolling factor, this can be rather
  // important, e.g. if we are unrolling by 11 and there are 20 iterations.
  // TODO OK: There are other ways to do it.  For example, we could pass
  // in u' = (u+1)/2.  Then we'd know that the do loop (not the unroll loop
  // but the main loop) always goes 0 or 1 time, and we could remove the
  // loop entirely, and even remove the if in many cases, effectively.  Since
  // this loop is outer, it's not crucial, but it's nice.  Even keeping this
  // framework, it would be nice to check the first loop to see if it goes
  // only once and get rid of it.  That would change the bounds info and
  // the transformation, though.  However, we could pass another parameter
  // to SNL_Regtile_Loop saying whether to do this or not, and make it
  // TRUE only in the call below, so that when we do
  //     DO i = 1, 123 (regblock by 10)
  // the winddown loop that handles unrolls of size 2, which only goes once,
  // can be removed.  But what we have now is perfectly adequate.

  if (wdloop && !no_further_unroll &&
      u >= LNO_Outer_Unroll_Min_For_Further_Unroll) {
    FmtAssert(wdpinfo, ("Bug"));
    SNL_REGION region2 = SNL_Regtile_Loop(wdloop, 2, nloops, unroll_just_inner,
                                          est_register_usage, wdpinfo,
                                          pinfo_depth, no_further_unroll);
    region.Last = region2.Last;
  }

  if (loop_map_ptr)
    *loop_map_ptr = loop_map;
  else if (loop_map)
    CXX_DELETE(loop_map, &SNL_local_pool);

  if (wdpinfo_ptr)
    *wdpinfo_ptr = wdpinfo;
  else if (wdpinfo)
    CXX_DELETE(wdpinfo, &SNL_local_pool);

  if (!Valid_SNL_Region(region))
    DevWarn("SNL_Regtile_Loop: Invalid SNL_REGION [0x%p,0x%p]",
      region.First, region.Last);
  return region;
}


