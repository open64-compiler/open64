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


#include "defs.h"
#include "config.h"
#include "tracing.h"
#include "timing.h"
#include "cgir.h"
#include "cg.h"
#include "cg_flags.h"
#include "ttype.h"
#include "bitset.h"
#include "bb_set.h"
#include "freq.h"
#include "whirl2ops.h"
#include "gra_live.h"
#include "bb.h"
#include "bb_map.h"
#include "tn_map.h"

#include "hb.h"
#include "hb_trace.h"

/////////////////////////////////////
static void
Check_Tail_Duplication(HB* hb, BB* bb, BB_MAP unduplicated,
		       HB_bb_list& need_duplication)
/////////////////////////////////////
//
//  Determine if the bb is a side entrance, and if
//  it has already been duplicated.  Add it to list,
//  and then add all successors reachable from it.
//
/////////////////////////////////////
{
  BBLIST* blp;

  if (bb == HB_Entry(hb)) {
    return;
  }

  if (BB_MAP32_Get(unduplicated, bb)) {
    return;
  }

  FOR_ALL_BB_PREDS(bb, blp) {
    BB* pred = BBLIST_item(blp);
    if (!HB_Contains_Block(hb, pred) || BB_MAP32_Get(unduplicated, pred)) {
      BB_MAP32_Set(unduplicated, bb, 1);
      if (HB_Trace(HB_TRACE_DUP)) {
	fprintf(HB_TFile, "<HB> BB%d is a side entrance.\n", BB_id(bb));
      }
      need_duplication.push_front(bb);
      BBLIST* bls;
      FOR_ALL_BB_SUCCS(bb, bls) {
	BB* succ = BBLIST_item(bls);
	//
	// Avoid loop back edge.
	//
	if (succ != HB_Entry(hb) && HB_Contains_Block(hb, succ)) {
	  Check_Tail_Duplication(hb, succ, unduplicated, need_duplication);
	}
      }
      break;
    }
  }
}


/////////////////////////////////////
static BOOL
Unduplicated_Preds(BB* bb, BB_MAP unduplicated)
/////////////////////////////////////
//
//  Determine if the bb is a side entrance, and if
//  it has already been duplicated.  
//
/////////////////////////////////////
{
  BBLIST* bl;
  FOR_ALL_BB_PREDS(bb, bl) {
    BB* pred = BBLIST_item(bl);
    if (BB_MAP32_Get(unduplicated, pred)) {
      return TRUE;
    }
  }
  return FALSE;
}

/////////////////////////////////////
static void
Fixup_Arcs(HB* hb, BB* old_bb, BB* new_bb, BB_MAP duplicate, BB** fall_thru,
	   HB_bb_list& duplicate_bbs)
/////////////////////////////////////
//
//  Fix up input arcs to the new block from blocks outside the hyperblock,
//  and add successor arcs.
//
/////////////////////////////////////
{
  BBLIST* bl;
  float new_freq = 0.0;

  //
  // Move predecessor arcs from outside of the hyperblock to new block.
  //
  for (bl = BB_preds(old_bb); bl != NULL;) {
    BB* pred = BBLIST_item(bl);
    bl = BBLIST_next(bl);

    //
    // Calculate block frequency.
    //
    BBLIST* blsucc = BB_Find_Succ(pred, old_bb);

    //
    // Now, it's either a block not selected for the hyperblock, or
    // a block that's already been duplicated, or it is an unduplicated
    // member of the hyperblock and we need do nothing.
    //
    BB* dup = (BB*) BB_MAP_Get(duplicate, pred);
    if (dup) {
      new_freq += BB_freq(pred) * BBLIST_prob(blsucc);

      //
      // Need to make the appropriate arcs from the duplicated block
      // to this one.
      //
#ifdef TARG_IA64
      Remove_Explicit_Branch(pred);
#endif
      if (BB_Fall_Thru_Successor(pred) == old_bb) {
	Link_Pred_Succ_with_Prob(dup, new_bb, BBLIST_prob(blsucc));
      } else if (BB_kind(dup) == BBKIND_LOGIF) {
	Target_Cond_Branch(dup, new_bb, BBLIST_prob(blsucc));
      } else {
	BB_Remove_Branch(dup);
	Add_Goto(dup, new_bb);
      }
    } else if (HB_Contains_Block(hb, pred)) {
      continue;
    } else {
      new_freq += BB_freq(pred) * BBLIST_prob(blsucc);
#ifdef TARG_IA64
      Remove_Explicit_Branch(pred);
#endif
      if (BB_Fall_Thru_Successor(pred) == old_bb) {
	Change_Succ(pred, old_bb, new_bb);
      } else {
	BB_Retarget_Branch(pred, old_bb, new_bb);
      }
    }
  }

  BB_freq(new_bb) = new_freq;

  //
  // Must check the original block's successors.  If they're outside
  // the hyperblock, we must add arcs here.
  //
  FOR_ALL_BB_SUCCS(old_bb, bl) {
    BB* succ = BBLIST_item(bl);
    if (!HB_Contains_Block(hb, succ) || succ == HB_Entry(hb)) {
#ifdef TARG_IA64
      Remove_Explicit_Branch(old_bb);
#endif
      if (BB_Fall_Thru_Successor(old_bb) == succ) {
	//
	// We insert a block here because it makes it easier if
	// this guy is a LOGIF, and both successors are outside
	// the hyperblock.  Cflow will clean up the unneeded block.
	// We add it to the duplicate bbs list so that it's liveness
	// will be calculated correctly later.
	//
	BB* new_succ = Gen_And_Insert_BB_After(new_bb);
	Link_Pred_Succ(new_bb, new_succ);
	Add_Goto(new_succ, succ);
	Link_Pred_Succ(new_succ, succ);
	GRA_LIVE_Compute_Liveness_For_BB(new_succ);
	duplicate_bbs.push_front(new_succ);
	*fall_thru = new_succ;
      } else {
	Link_Pred_Succ_with_Prob(new_bb, succ, BBLIST_prob(bl));
      }
    }	
  }
}

/////////////////////////////////////
static void
Rename_Locals(OP* op, hTN_MAP dup_tn_map)
/////////////////////////////////////
//
//  Local TN's must be renamed in duplicated block, otherwise
//  they'll look like globals.  Note that we assume we're processing
//  the ops in forward order.  If not, the way we map the new names
//  below won't work.
//
/////////////////////////////////////
{
  INT i = 0;
  TN* res;

  for (i = 0; i < OP_results(op); i++) {
    res = OP_result(op, i);
    if (TN_is_register(res) &&
	!(TN_is_dedicated(res) || TN_is_global_reg(res))) {
      TN* new_tn = Dup_TN(res);
      hTN_MAP_Set(dup_tn_map, res, new_tn);
      Set_OP_result(op, i, new_tn);
    }
  }

  i = 0;
  for (INT opndnum = 0; opndnum < OP_opnds(op); opndnum++) {
    res = OP_opnd(op, opndnum);
    if (TN_is_register(res) &&
	!(TN_is_dedicated(res) || TN_is_global_reg(res))) {
      res = (TN*) hTN_MAP_Get(dup_tn_map, res);
      Set_OP_opnd(op, i, res);
    }
    i++;
  }
}

#if !defined(TARG_IA64) && !defined(TARG_SL) && !defined(TARG_MIPS)
static
#endif
/////////////////////////////////////
BB*
Copy_BB_For_Tail_Duplication(HB* hb, BB* old_bb)
/////////////////////////////////////
//
//  Copy <old_bb> and all of its ops into a new
//  BB.
//
/////////////////////////////////////
{
  BB* new_bb = NULL;

  new_bb = Gen_BB_Like(old_bb);
  

  //
  // Copy the ops to the new block.
  //
  OP *op;
  MEM_POOL_Push(&MEM_local_pool);
  hTN_MAP dup_tn_map = hTN_MAP_Create(&MEM_local_pool);

  FOR_ALL_BB_OPs_FWD(old_bb, op) {
    OP *new_op = Dup_OP(op);
    Copy_WN_For_Memory_OP(new_op, op);
    BB_Append_Op(new_bb, new_op);
    Rename_Locals(new_op, dup_tn_map);
  }
  MEM_POOL_Pop(&MEM_local_pool);

  //
  // Take care of the annotations.
  //
  switch (BB_kind(old_bb)) {
  case BBKIND_CALL:
    BB_Copy_Annotations(new_bb, old_bb, ANNOT_CALLINFO);
    break;

  case BBKIND_TAIL_CALL:
    BB_Copy_Annotations(new_bb, old_bb, ANNOT_CALLINFO);
    //
    // NOTE FALLTHRU
    //
  case BBKIND_RETURN:
    {
      BB_Copy_Annotations(new_bb, old_bb, ANNOT_EXITINFO);
      OP *b_op;
      OP *suc_op;
      ANNOTATION *ant = ANNOT_Get(BB_annotations(new_bb), ANNOT_EXITINFO);
      EXITINFO *exit_info = ANNOT_exitinfo(ant);
      EXITINFO *new_info = TYPE_PU_ALLOC(EXITINFO);
      OP *sp_adj = EXITINFO_sp_adj(exit_info);
      *new_info = *exit_info;
      if (sp_adj) {
	for (suc_op = BB_last_op(old_bb), b_op = BB_last_op(new_bb);
	     suc_op != sp_adj;
	     suc_op = OP_prev(suc_op), b_op = OP_prev(b_op))
	  ;
	EXITINFO_sp_adj(new_info) = b_op;
      }
      ant->info = new_info;

      Set_BB_exit(new_bb);
      Exit_BB_Head = BB_LIST_Push(new_bb, Exit_BB_Head, &MEM_pu_pool);
      break;
    }
  }

  return new_bb;
}

/////////////////////////////////////
static void
Tail_Duplicate(HB* hb, BB* side_entrance, BB_MAP unduplicated,
	       BB_MAP duplicate, BB** last, HB_bb_list& duplicate_bbs)
/////////////////////////////////////
//
//  Duplicate and place it outside the hyperblock
//
/////////////////////////////////////
{
  BBLIST* bl;
  BB* dup =  Copy_BB_For_Tail_Duplication(hb, side_entrance);
  duplicate_bbs.push_front(dup);

  if (HB_Trace(HB_TRACE_DUP)) {
    fprintf(HB_TFile, "<HB> Tail duplicating BB:%d.  Duplicate BB%d\n",
	    BB_id(side_entrance), BB_id(dup));
  }

  //
  // Flag bb as having been duplicated.
  //
  BB_MAP32_Set(unduplicated, side_entrance, 0);
  BB_MAP_Set(duplicate, side_entrance, dup);

  //
  // Find new block's place.  If it's the fall thru successor of a
  // duplicated block or a block not included in the hyperblock, then
  // that's where it goes.  Otherwise, place it below the last block added.
  //
  FOR_ALL_BB_PREDS(side_entrance, bl) {
    BB* pred = BBLIST_item(bl);
#ifdef TARG_IA64
    Remove_Explicit_Branch(pred);
#endif
    if (side_entrance == BB_Fall_Thru_Successor(pred)) {
      BB* fall_dup = (BB*) BB_MAP_Get(duplicate, pred);
      if (fall_dup) {
	*last = fall_dup;
      } else if (!HB_Contains_Block(hb, pred)) {
	*last = pred;
      }
      break;
    }
  }

  //
  // Update the arcs and liveness.
  //
  BB* fall_thru = NULL;
  Fixup_Arcs(hb, side_entrance, dup, duplicate, &fall_thru, duplicate_bbs);

  //
  // Add block, and any block added to compensate for a fall thru out
  // of the hyperblock.
  //
  Insert_BB(dup, *last);
  if (fall_thru) {
    Insert_BB(fall_thru, dup);
    *last = fall_thru;
  } else {
    *last = dup;
  }
}

/////////////////////////////////////
BOOL
HB_Tail_Duplicate(HB* hb, BB_MAP duplicate,
		  HB_bb_list& duplicate_bbs,
		  BOOL post_tail_duplication)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  BB* bb;
  HB_bb_list need_duplication;
  BB_MAP unduplicated = BB_MAP32_Create();

  FOR_ALL_BB_SET_members(HB_Blocks(hb), bb) {
    Check_Tail_Duplication(hb, bb, unduplicated, need_duplication);
  }

  if (need_duplication.empty()) {
    BB_MAP_Delete(unduplicated);
    return TRUE;
  }
  if (post_tail_duplication) {
    //
    // We only tail duplicate the first time around.  We'll want to
    // trash the hyperblock if it needed it.
    //
    BB_MAP_Delete(unduplicated);
    return FALSE;
  }

  if (!HB_allow_tail_duplication) {
    BB_MAP_Delete(unduplicated);
    return FALSE;
  }    

  HB_did_tail_duplication = TRUE;

  //
  // Look or a place to start adding the duplicate blocks.
  //
  BB* last_duplicated;
  for (bb = HB_Entry(hb); bb && HB_Contains_Block(hb, bb);
       last_duplicated = bb, bb = BB_next(bb));
#ifdef TARG_IA64
  Remove_Explicit_Branch(bb);
#endif
  if (bb) {
    //
    // bb only NULL if it terminates in the last block in the PU.
    //
    for (; bb && BB_Fall_Thru_Successor(bb); bb = BB_next(bb));
#ifdef TARG_IA64
    Remove_Explicit_Branch(bb);
#endif
    last_duplicated = bb;
  }

  //
  // Fairly inefficient method of finding all of them, but it's easy
  // to code and there shouldn't be too many blocks in the list.  If
  // there are, we're doing something wrong.
  //
  while (!need_duplication.empty()) {
    HB_bb_list::iterator bbi, cur;
    for (bbi = need_duplication.begin(); bbi != need_duplication.end();) {
      cur = bbi++;
      if (!Unduplicated_Preds(*cur, unduplicated)) {
	Tail_Duplicate(hb, *cur, unduplicated, duplicate, &last_duplicated,
		       duplicate_bbs);
	need_duplication.erase(cur);
      }
    }
  }

  BB_MAP_Delete(unduplicated);

  //
  // TODO: implement heursitics to limit tail duplication!!!!!!
  //
  return TRUE;
}
