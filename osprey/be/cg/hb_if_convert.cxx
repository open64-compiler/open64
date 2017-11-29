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

#include <vector>
#include <queue>
#include "defs.h"
#include "config.h"
#include "tracing.h"
#include "timing.h"
#include "cgir.h"
#include "cg.h"
#include "cg_flags.h"
#include "ttype.h"
#include "bitset.h"
#include "bb.h"
#include "bb_set.h"
#include "freq.h"
#include "whirl2ops.h"
#include "dominate.h"
#include "cgtarget.h"
#include "gra_live.h"
#include "cgexp.h"
#include "gtn_universe.h"
#include "gtn_set.h"
#include "findloops.h"
#include "pqs_cg.h"

#ifdef TARG_IA64
#include "if_conv.h"
#endif

#include "hb.h"
#include "hb_trace.h"
#include "hb_id_candidates.h" // For use in Force_If_Convert
#ifdef KEY
#include "ti_res.h" // For TI_RES_Cycle_Count
#include "data_layout.h" // For Allocate_Temp_To_Memory
#endif

/////////////////////////////////////
static void 
dump_control_dependencies(const char * title, HB* hb, BB_MAP control_dependences,
			  BB_MAP true_edges)
{
  BB * bb;
  BB_SET* deps; 
  BB_SET* trues; 
  fprintf(HB_TFile,"Control dependence dump %s=============================\n",title);
  FOR_ALL_BB_SET_members(HB_Blocks(hb), bb) {
    fprintf(HB_TFile,"BB%d:  C=",BB_id(bb));
    deps  = (BB_SET*) BB_MAP_Get(control_dependences, bb);
    trues = (BB_SET*) BB_MAP_Get(true_edges, bb);
    if (deps) {
      BB_SET_Print(deps, HB_TFile);
    } else {
      fprintf(HB_TFile,"<none>");
    }
    fprintf(HB_TFile,"   T=");
    if (trues) {
      BB_SET_Print(trues, HB_TFile);
    } else {
      fprintf(HB_TFile,"<none>");
    }
    fprintf(HB_TFile,"\n");
  }
  fprintf(HB_TFile,"================================================================\n");
}


//////////////////////////////////////
//
//  Calculate all of the control dependences along with whether or
//  not they are dependent upon the true arc of the branch.
//
//  true_edges are those edges which are traversed when the branch instruction in
//  the BB is executed. 
//
/////////////////////////////////////
static void
Calculate_Control_Dependences(HB* hb, BB_MAP control_dependences,
			      BB_MAP true_edges)
{
  BB* bb;
  BBLIST* bl;
  BB_SET *hb_blocks;
  BB *hb_entry;

  BB_SET * bb_to_add = BB_SET_Create_Empty(PU_BB_Count+2, &MEM_local_pool);
  
  // Compute post dominators
  BB_SET_Calculate_Dominators(HB_Blocks(hb),FALSE,TRUE);
  
  //
  // Initialize control dependencies bit sets
  //
  FOR_ALL_BB_SET_members(HB_Blocks(hb), bb) {
    BB_SET* deps = BB_SET_Create_Empty(PU_BB_Count+2, &MEM_local_pool);
    BB_SET* trues = BB_SET_Create_Empty(PU_BB_Count+2, &MEM_local_pool);
    BB_MAP_Set(control_dependences, bb, deps);
    BB_MAP_Set(true_edges, bb, trues);
  }
  
  //
  // Find the control dependences. Also, in this algorithms it's
  // easy to set the true edges at the same time.
  //
  hb_blocks = HB_Blocks(hb);
  hb_entry =  HB_Entry(hb);
  FOR_ALL_BB_SET_members(hb_blocks, bb) {
    FOR_ALL_BB_SUCCS(bb,bl) {
      BB* bb_dep;
      BB* bb_succ = BBLIST_item(bl);
#ifdef TARG_IA64
      Remove_Explicit_Branch(bb);
#endif
      BOOL true_edge = (BB_Fall_Thru_Successor(bb) != bb_succ);
      //
      // bb_to_add is set to (pdom(bb_succ) - pdom(bb)) & HB_Blocks. 
      // In other words, everything which is in the hyperblock
      // which post-dominates bb_succ (which will include bb_succ) and
      // is not a post-dominator of bb. These are exactly the BB's which are
      // control dependent on bb. If the successor is not in the hyperblock
      // we are safe to assume that nothing else in the hyperblock post-dominates it,
      // so we can ignore it. Also, if the successor is the hyperblock entry, we
      // have a loop and hence we must not consider this arc in computing the control
      // dependence.
      //
      if (BB_SET_MemberP(hb_blocks,bb_succ) && (bb_succ != hb_entry)) {
	BB_SET_CopyD(bb_to_add,BB_pdom_set(bb_succ),NULL);  // better not reallocate
	bb_to_add = BB_SET_DifferenceD(bb_to_add,BB_pdom_set(bb));
	bb_to_add = BB_SET_IntersectionD(bb_to_add,HB_Blocks(hb));
	FOR_ALL_BB_SET_members(bb_to_add, bb_dep) {
	  BB_SET* deps  = (BB_SET*) BB_MAP_Get(control_dependences, bb_dep);
	  BB_SET* trues = (BB_SET*) BB_MAP_Get(true_edges, bb_dep);
	  BB_SET_Union1D(deps, bb, &MEM_local_pool);
	  if (true_edge) BB_SET_Union1D(trues, bb, &MEM_local_pool);
	}
      }
    }
  }
  
  if (HB_Trace(HB_TRACE_CDEP)) {
    dump_control_dependencies("",hb,control_dependences,true_edges);
  }
}



//================================================================
//
//                       Branch Removal
//
//================================================================

// Defines for block classification flags 
#define CASE_MASK 0xf  // 15 bits, but only seven cases
#define CASE_ALL_IN_HB  1  // All sucessors in hyperblock
#define CASE_CALL       2  // A call block, with its successor outside
#define CASE_UNCOND_BR  3  // Unconditional branch to outside HB
#define CASE_FALL_OUT   4  // no branch, fall through outside
#define CASE_IF_FALL_IN 5  // conditional branch to outside HB
#define CASE_IF_FALL_OUT 6 // conditional branch, fallthrough outside HB
#define CASE_IF_OUT      7 // both successors outside HB
#define CASE_CALL_IN     8 // call with successors in HB
#define NO_MERGE        0x100  // next block cannot be merged
#define NEEDS_FTG       0x200  // needs a fall-thru goto

#define Get_Case(x) ((x)&CASE_MASK)
#define No_Merge(x) (((x)&NO_MERGE) != 0)
#define Needs_FTG(x) (((x)&NEEDS_FTG) != 0)


//
// Struct for storing frequency related data during the merge phase
//
struct FREQ_DATA {
   float block_prob;  // Basic execution probability within the hyperblock
   float branch_prob;   // Probability of taking the branch in the block
   BB * fall_thru;       // Fall thru block
   BB * branch_bb;       // other block
   FREQ_DATA() {
      block_prob = 0.0;
      branch_prob = 0.0;
      fall_thru = NULL;
      branch_bb = NULL;
   }
};

// Helper routines for initializing the freq_map information

static void
Reset_Freq_Data_For_BB(BB *bb, BB_MAP freq_map)
{
   BBLIST *bl;
   FREQ_DATA *bb_freq_data = (FREQ_DATA *) BB_MAP_Get(freq_map,bb);
#ifdef TARG_IA64
   Remove_Explicit_Branch(bb);
#endif
   bb_freq_data->fall_thru = BB_Fall_Thru_Successor(bb);
   if (bb_freq_data->fall_thru) {
      bb_freq_data->branch_prob = 
	1.0 - BBLIST_prob(BB_Find_Succ(bb, bb_freq_data->fall_thru));
   } else {
      bb_freq_data->branch_prob = 1.0;
   }
   bb_freq_data->branch_bb = NULL;
   FOR_ALL_BB_SUCCS(bb,bl) {
      BB *succ = BBLIST_item(bl);
      if (succ != bb_freq_data->fall_thru) {
	 bb_freq_data->branch_bb = succ;
      }
   }
}
   

static 
FREQ_DATA * Get_Freq_Data_For_BB(BB *bb, BB_MAP freq_map)
{
   FREQ_DATA * bb_freq_data;
   bb_freq_data = (FREQ_DATA *) BB_MAP_Get(freq_map, bb);
   if (!bb_freq_data) {
      bb_freq_data = CXX_NEW(FREQ_DATA, &MEM_local_pool);
      BB_MAP_Set(freq_map,bb,bb_freq_data);
      Reset_Freq_Data_For_BB(bb,freq_map);
   }
   return bb_freq_data;
}

//
// Get the probability to assign to a branch
//
static float
Get_Branch_Prob(BB *source, BB *dest, BB_MAP freq_map)
{
   FREQ_DATA *fdata = Get_Freq_Data_For_BB(source, freq_map);
   if (dest == fdata->fall_thru) {
      return ((1.0 - fdata->branch_prob) * fdata->block_prob);
   } else {
      return (fdata->branch_prob * fdata->block_prob);
   }
}
      
//
// Get the probability of a block executing
//
static float
Get_Block_Prob(BB *source,BB_MAP freq_map)
{
   FREQ_DATA *fdata = Get_Freq_Data_For_BB(source, freq_map);
   return (fdata->block_prob);
}

static BB *
Get_Branch_Block(BB *source,BB_MAP freq_map)
{
   FREQ_DATA *fdata = Get_Freq_Data_For_BB(source, freq_map);
   return (fdata->branch_bb);
}


//
// Replace block bb_second with block bb_first in all HB candidates
//
static void
replace_block_walk(BB *bb_first, BB *bb_second, HB_CAND_TREE* c, BOOL walk_kids)
{
  HB *hb = HB_CAND_TREE_Candidate(c);
  BB_SET *blocks = HB_Blocks(hb);
  if (BB_SET_MemberP(blocks, bb_second)) {
    HB_Remove_Block(hb, bb_second);
    HB_Add_Block(hb, bb_first);
  }
  if (HB_Entry(hb) == bb_second) HB_Entry_Set(hb, bb_first);
  /////  if (HB_Exit(hb) == bb_second) HB_Exit_Set(hb, bb_second);
  // 
  // Walk the children
  //
  if (walk_kids) {
    std::list<HB_CAND_TREE*> kids = HB_CAND_TREE_Kids(c);
    std::list<HB_CAND_TREE*>::iterator k;
    for (k = kids.begin(); k != kids.end(); k++) {
      replace_block_walk(bb_first,bb_second,*k,walk_kids);
    }
  }
}

static void
Replace_Block(BB *bb_first, BB *bb_second, std::list<HB_CAND_TREE*>& candidate_regions)
{
  std::list<HB_CAND_TREE*>::iterator hbct;
  for (hbct = candidate_regions.begin(); hbct != candidate_regions.end(); 
       hbct++) {
    replace_block_walk(bb_first,bb_second,*hbct,TRUE);
  }
}



//
// Add block bb_to_add to every HB candidate containing the block bb in all HB candidates
//
static void
add_block_walk(BB *bb, BB *bb_to_add, HB_CAND_TREE* c, BOOL walk_kids)
{
  HB *hb = HB_CAND_TREE_Candidate(c);
  BB_SET *blocks = HB_Blocks(hb);
  if (BB_SET_MemberP(blocks, bb)) {
    HB_Add_Block(hb, bb_to_add);
  }
  // 
  // Walk the children
  //
  if (walk_kids) {
    std::list<HB_CAND_TREE*> kids = HB_CAND_TREE_Kids(c);
    std::list<HB_CAND_TREE*>::iterator k;
    for (k = kids.begin(); k != kids.end(); k++) {
      add_block_walk(bb,bb_to_add,*k,walk_kids);
    }
  }
}

static void
Add_Block(BB *bb, BB *bb_to_add, std::list<HB_CAND_TREE*>& candidate_regions)
{
  std::list<HB_CAND_TREE*>::iterator hbct;
  for (hbct = candidate_regions.begin(); hbct != candidate_regions.end(); 
       hbct++) {
    add_block_walk(bb,bb_to_add,*hbct,TRUE);
  }
}


#ifdef KEY
static BOOL merge_failed;
#endif
/////////////////////////////////////
static void
Merge_Blocks(HB*                  hb, 
	     BB*                  bb_first, 
	     BB*                  bb_second, 
	     BB_MAP               freq_map,
	     BOOL                 last_block,
	     std::list<HB_CAND_TREE*>& candidate_regions)
/////////////////////////////////////
//
//  Merge bb_first and bb_second, leaving bb_first as the block. Last_block
//  indicates that the block is the last in the hyperblock.  This is needed
//  to keep frequencies up-to-date correctly (although they are really kind
//  of bogus at this point)
//
/////////////////////////////////////
{
  OP* op;
  BB *succ;
  BBLIST *bl;

#if defined KEY && defined TARG_MIPS
  // If we already found out that we can not merge the two blocks return now
  if (merge_failed)
    return;
  // Look for feasibility of merge; we want to merge only those blocks 
  // that have same branch-to target
  OP *op_first = BB_branch_op(bb_first);
  OP *op_second = BB_branch_op(bb_second);
  if (op_first && op_second) {
    if ((OP_code(op_first) == OP_code(op_second)) && 
	(OP_code(op_first) == TOP_beq || 
	 OP_code(op_first) == TOP_bne)) {
      FmtAssert((TN_is_label(OP_opnd(op_first, 2)) &&
		  TN_is_label(OP_opnd(op_second, 2))), 
		 ("branch target should be labelled"));
      if (TN_label(OP_opnd(op_first, 2)) != 
	  TN_label(OP_opnd(op_second, 2))) {
	merge_failed = TRUE;
	return;
      }
    } else if ((OP_code(op_first) == OP_code(op_second)) && 
	       (OP_code(op_first) == TOP_bc1f || 
		OP_code(op_first) == TOP_bc1t)) {
      FmtAssert((TN_is_label(OP_opnd(op_first, 1)) &&
		  TN_is_label(OP_opnd(op_second, 1))), 
		 ("branch target should be labelled"));
      if (TN_label(OP_opnd(op_first, 1)) != 
	  TN_label(OP_opnd(op_second, 1))) {
	merge_failed = TRUE;
	return;
      }
    } else {
      //FmtAssert(FALSE, ("HANDLE THIS CASE"));
      merge_failed = TRUE;
      return;
    }
  }
#endif
  //
  // Move all of its ops to bb_first.
  //
  OP* op_next;
  OP* last = BB_last_op(bb_first);
  for (op = BB_first_op(bb_second); op; op = op_next) {
    op_next = OP_next(op);
    if (last) {
      BB_Move_Op(bb_first, last, bb_second, op, FALSE);
    } else {
      BB_Append_Op (bb_first, op);
    }
    last = op;
  }


  // Clean up the firsts pred-succ list
  while (bl = BB_succs(bb_first)) {
    succ = BBLIST_item(bl);
    Unlink_Pred_Succ(bb_first, succ);
  }

  //
  // Take bb_second out of the list.
  //
  Remove_BB(bb_second);

  //
  // If bb_second an exit, move the relevant info to the
  // merged block.
  //
  if (BB_exit(bb_second)) {
    BB_Transfer_Exitinfo(bb_second, bb_first);
    Exit_BB_Head = BB_LIST_Delete(bb_second, Exit_BB_Head);
    Exit_BB_Head = BB_LIST_Push(bb_first, Exit_BB_Head, &MEM_pu_pool);
  }

  //
  // Transfer call info if merged block will now contain a call.
  //
  if (BB_call(bb_second)) {
    BB_Copy_Annotations(bb_first, bb_second, ANNOT_CALLINFO);
  }

  //
  // Move bb_second's successor arcs to bb_first.
  //
  FmtAssert(BBlist_Len(BB_succs(bb_second))<=2,("Too many successors"));
  if (BBlist_Len(BB_succs(bb_second)) == 1) {
    bl = BB_succs(bb_second);
    float old_prob = BBLIST_prob(bl);
    succ = BBLIST_item(bl);
    Unlink_Pred_Succ(bb_second, succ);
    Link_Pred_Succ_with_Prob(bb_first, succ, old_prob);
  } else {
    while (bl = BB_succs(bb_second)) {
      succ = BBLIST_item(bl);
      Unlink_Pred_Succ(bb_second, succ);
      float prob = Get_Branch_Prob(bb_second,succ,freq_map);;
      if (succ != Get_Branch_Block(bb_second,freq_map)) {
	prob = 1.0 - Get_Branch_Prob(bb_second,succ,freq_map);
      }	
      Link_Pred_Succ_with_Prob(bb_first, succ, prob);
    }
  }
  //
  // Fix up hyperblock data structures.
  //
  if (HB_Exit(hb) == bb_second) {
    HB_Exit_Set(hb, bb_first);
  }

  Replace_Block(bb_first,bb_second,candidate_regions);
#if defined KEY && defined TARG_MIPS
  // We will end up having the branches from the 
  // 'then' block and the 'else' block merged. Search for 'beq' on
  // opposite predicate TNs and merge the two into a jump (unc)
  OP *op1, *op2, *op3;
  op2 = NULL;
  FOR_ALL_BB_OPs(bb_first,op1) {
    if (OP_code(op1) == TOP_beq || 
	OP_code(op1) == TOP_bne || 
	OP_code(op1) == TOP_bc1f || 
	OP_code(op1) == TOP_bc1t) {
      if (op2 == NULL)
	op2 = op1;
      else { // second branch instruction
	OPS new_ops;
	OPS_Init(&new_ops);
	Build_OP(TOP_j, OP_opnd(op2, 
				(OP_code(op2) == TOP_beq || 
				 OP_code(op2) == TOP_bne)? 2: 1), &new_ops);
	BB_Append_Ops(bb_first, &new_ops);
	OP_Change_To_Noop(op2);
	OP_Change_To_Noop(op1);
	break;
      }
    } 
  }
  printf("MERGE PASSED\n");
#endif
}

// want to AND together controlling predicate to existing qualifying predicate
// such that op is executed iff ptn1 and ptn2 are both true.
static void
AND_Predicate_To_OP (OP *op, TN *ptn1, TN *ptn2, 
	BB *bb_insert_point, OP *op_insert_point, BB_SET *hb_blocks)
{
	// may have case where repeatedly need to and same predicates;
	// to optimize that case, keep track of last one and reuse.
	typedef struct {
		BB *bb;
		TN *ptn1;
		TN *ptn2;
		TN *new_ptn;
	} last_and_pred_t;
	static last_and_pred_t last_and_pred = {NULL,NULL,NULL,NULL};
	BB *bb;
	BB *insert_bb;
	OP *insert_before_op = NULL;
	OP *o;
	OP *new_op;
	OP *def_ptn1_op = NULL;
	OP *def_ptn2_op = NULL;
	TN *new_ptn;
	if (ptn2 == ptn1) return;
	if (ptn2 == True_TN) {
		Set_OP_opnd(op, OP_PREDICATE_OPND, ptn1);
		return;
	}
	if (bb_insert_point == last_and_pred.bb 
		&& ptn1 == last_and_pred.ptn1
		&& ptn2 == last_and_pred.ptn2)
	{
		DevWarn("reusing last predicate and");
		new_ptn = last_and_pred.new_ptn;
		Set_OP_opnd(op, OP_PREDICATE_OPND, new_ptn);
		return;
	}
	else {
		last_and_pred.bb = NULL;
		last_and_pred.ptn1 = last_and_pred.ptn2
					= last_and_pred.new_ptn = NULL;
	}

	DevWarn("and controlling predicate TN%d to current predicate TN%d", TN_number(ptn1), TN_number(ptn2));
	// idea:  find op that defined ptn1,
	// check if that has p0 as qp,
	// then add op that does same but uses ptn2,
	// defines new ptn1, and then use ptn1 on op.
	// Can get into use before def problems if ptn1 and ptn2 are
	// defined in separate bb's, so check for that.
	// TN_Reaching_Value_At_Op doesn't work,
	// because hbf has unlinked some of the previous bbs
	// in the hb.  So instead do brute search in hb
	// and see if tn is only defined once.
	FOR_ALL_BB_SET_members(hb_blocks, bb) {
	    FOR_ALL_BB_OPs(bb,o) {
		if (OP_Defs_TN (o, ptn1)) {
			if (HB_Trace(HB_TRACE_CONVERT)) {
				fprintf(TFile, "def_ptn1_op: ");
				Print_OP_No_SrcLine(o);
				if (def_ptn1_op != NULL) Print_All_BBs();
			}
			FmtAssert(def_ptn1_op == NULL, 
				("and_pred mult defs; try recompiling with -CG:hb_exclude_pgtns=on"));
			def_ptn1_op = o;
		}
		if (OP_Defs_TN (o, ptn2)) {
			if (HB_Trace(HB_TRACE_CONVERT)) {
				fprintf(TFile, "def_ptn2_op: ");
				Print_OP_No_SrcLine(o);
				if (def_ptn2_op != NULL) Print_All_BBs();
			}
			FmtAssert(def_ptn2_op == NULL, 
				("and_pred mult defs; try recompiling with -CG:hb_exclude_pgtns=on"));
			def_ptn2_op = o;
		}
	    }
	}
	FmtAssert(def_ptn1_op != NULL, ("and_pred no def; try recompiling with -CG:hb_exclude_pgtns=on"));
	FmtAssert(def_ptn2_op != NULL, ("and_pred2 no def; try recompiling with -CG:hb_exclude_pgtns=on"));
	FmtAssert(OP_has_predicate(def_ptn1_op), ("and_pred no qp; try recompiling with -CG:hb_exclude_pgtns=on")); 
	FmtAssert(OP_has_predicate(def_ptn2_op), ("and_pred2 no qp; try recompiling with -CG:hb_exclude_pgtns=on")); 

        new_op = Dup_OP(def_ptn1_op);
	if (OP_opnd(new_op, OP_PREDICATE_OPND) != True_TN) {
		// sigh.  We have run into another problem case where
		// we need to and two predicates.  So recurse.
		// Eventually we should either find a clean def or assert.
		if (HB_Trace(HB_TRACE_CONVERT)) {
			fprintf(TFile, "bbs before recursive predicate AND:");
			Print_All_BBs();
		}
		AND_Predicate_To_OP (new_op, ptn2, 
			OP_opnd(new_op, OP_PREDICATE_OPND), 
			bb_insert_point, op_insert_point, hb_blocks);
	}
	else {
		Set_OP_opnd(new_op, OP_PREDICATE_OPND, ptn2);
	}
	new_ptn = Gen_Predicate_TN();
	Set_OP_opnd(op, OP_PREDICATE_OPND, new_ptn);
	if (OP_result(new_op,0) == ptn1) {
		Set_OP_result(new_op, 0, new_ptn);
		Set_OP_result(new_op, 1, True_TN);
	}
	else if (OP_result(new_op,1) == ptn1) {
		Set_OP_result(new_op, 0, True_TN);
		Set_OP_result(new_op, 1, new_ptn);
	}
	else
		FmtAssert(FALSE, ("where is ptn1 defined?"));

	if (OP_bb(def_ptn1_op) != OP_bb(def_ptn2_op)) {
		DevWarn("and_pred defs in different bb");
		if (HB_Trace(HB_TRACE_CONVERT)) {
			Print_All_BBs();
		}
		// can't put new ops in ptn1 block, cause ptn2 may be
		// defined after that.  So instead try to put just before
		// use op.
		insert_bb = bb_insert_point;
		insert_before_op = op_insert_point;
		// make sure tns are marked as global now
		Set_TN_is_global_reg (ptn1);
		Set_TN_is_global_reg (ptn2);
		INT i;
		for (i=0; i<OP_opnds(new_op); i++) {
			if (i == OP_PREDICATE_OPND) continue;
			if (! TN_is_register(OP_opnd(new_op,i))) continue;
			if (TN_is_dedicated(OP_opnd(new_op,i))) continue;
			Set_TN_is_global_reg (OP_opnd(new_op,i));
		}
	}
	else {
		// usually can put new op right after def_ptn1_op,
		// but is possible that ptn2 is defined after def_ptn1_op,
		// so instead add at end of bb.
		insert_bb = OP_bb(def_ptn1_op);
		insert_before_op = BB_xfer_op(insert_bb);
	}
	if (HB_Trace(HB_TRACE_CONVERT)) {
		fprintf(TFile, "before insert: ");
		Print_BB_No_Srclines(insert_bb);
	}
	if (insert_before_op != NULL) {
		BB_Insert_Op_Before (insert_bb, insert_before_op, new_op);
	} else {
		BB_Append_Op (insert_bb, new_op);
	}
	if (HB_Trace(HB_TRACE_CONVERT)) {
		fprintf(TFile, "after insert: ");
		Print_BB_No_Srclines(insert_bb);
	}
	last_and_pred.bb = bb_insert_point;
	last_and_pred.ptn1 = ptn1;
	last_and_pred.ptn2 = ptn2;
	last_and_pred.new_ptn = new_ptn;
	if (HB_Trace(HB_TRACE_CONVERT)) {
		fprintf(TFile, "new_op: ");
		Print_OP_No_SrcLine(new_op);
		Print_OP_No_SrcLine(op);
	}
}

#if defined KEY && defined TARG_MIPS
// ===================================================================
// The code is borrowed from cio_rwtran
//
// Generate_Black_Holes initializes the black hole memory locations.
//
// Safe_Offset examines the value of a literal offset operand of a memory
// OP which is being subtracted from the positive offset of a stack
// location.  Return TRUE if the result will not overflow the literal
// offset field of a memory operand.
//
// ====================================================================

static ST *ifcbh_node_int   = NULL;
static ST *ifcbh_node_float = NULL;
static ST *latest_pu       = NULL;

void
Generate_Black_Holes()
{
  // Only need to reinitialize for each new PU.
  if ( latest_pu == Get_Current_PU_ST() )
    return;

  latest_pu = Get_Current_PU_ST();

  ifcbh_node_int   = Gen_Temp_Symbol( Spill_Int_Type,   "ifc_blackhole_int" );
  ifcbh_node_float = Gen_Temp_Symbol( Spill_Float_Type, "ifc_blackhole_float" );
  Allocate_Temp_To_Memory( ifcbh_node_int );
  Allocate_Temp_To_Memory( ifcbh_node_float );
}

#define MAX_BLACK_HOLE_OFFSET 0x800

BOOL
Safe_Offset( INT64 offset, OP *op )
{
  return offset > - (0x8000 - MAX_BLACK_HOLE_OFFSET);
}

/////////////////////////////////////
// ====================================================================
//
// Predicate_Write
//
// Converts an uncondition memory store OP into a store conditional on
// a predicate tn. This is accomplished by inserting into ops a sequence 
// of OPs that manipulate the address of the memory store OP.
//
// ====================================================================


void
Predicate_Read_Write( OPS *ops, OP *op, TN *tn_predicate )
{
  Generate_Black_Holes();

  BOOL op_is_load = FALSE;
  if (OP_load(op))
    op_is_load = TRUE;

  // If the OP is an indexed memory OP, the offset is a symbol, or if
  // the literal TN to be computed below does not fit in the literal
  // operand of an add immediate OP, we combine the base and offset
  // into a new base, and (if necesssary) change the OP code on the
  // memory OP.

  UINT8 opnd_base   = OP_find_opnd_use( op, OU_base   );
  UINT8 opnd_offset = OP_find_opnd_use( op, OU_offset );
  TN *tn_offset = OP_opnd( op, opnd_offset );
  if ( ! TN_is_constant( tn_offset ) || ! TN_has_value( tn_offset ) ||
       Safe_Offset( TN_value( tn_offset ), op ) == FALSE ) {

    TN *tn_zero = Gen_Literal_TN( 0, 4 );
    TN *tn_new_base = Build_TN_Like( OP_opnd( op, opnd_base ) );

    Exp_OP2( Pointer_Size == 4 ? OPC_I4ADD : OPC_I8ADD,
	     tn_new_base, OP_opnd( op, opnd_base ), tn_offset, ops );

    // Update op

    TOP new_opcode = CGTARG_Equiv_Nonindex_Memory_Op( op );
    if ( new_opcode != TOP_UNDEFINED ) {
      OP_Change_Opcode( op, new_opcode );
    }
    Set_OP_opnd( op, opnd_base,   tn_new_base );
    Set_OP_opnd( op, opnd_offset, tn_zero );
    tn_offset = tn_zero;
  }

  // Generate OPS to compute the block hole address base.  Note that
  // the address base is offset by the negative of the offset of op.

  TN *tn_loadstoreval = 
    (op_is_load != TRUE)? OP_opnd( op, OP_find_opnd_use( op, OU_storeval ) ) 
    : OP_result( op, 0);
  ST *st_addr = 
    TN_is_float( tn_loadstoreval ) ? ifcbh_node_float : ifcbh_node_int;
  TN *tn_hole_base = Build_TN_Like( OP_opnd( op, opnd_base ) );
  Exp_Lda( Pointer_type, tn_hole_base, st_addr,
	   - TN_value( tn_offset ), OPERATOR_UNKNOWN, ops );

  // Generate a cond move, which uses the predicate TN to select 
  // the blackhole address base

  TN *tn_base = OP_opnd( op, opnd_base );
  TN *tn_safe_base  = Build_TN_Like( tn_base );
  Build_OP(TOP_or, tn_safe_base, tn_base, Zero_TN, ops);
  if (TN_register_class(tn_predicate) == ISA_REGISTER_CLASS_fcc)
    Build_OP(TOP_movt, tn_safe_base, tn_hole_base, tn_predicate, ops);
  else
    Build_OP(TOP_movn, tn_safe_base, tn_hole_base, tn_predicate, ops);
  Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);      
  Set_OP_opnd( op, opnd_base, tn_safe_base );
}
#endif
/////////////////////////////////////
void
Predicate_Block(BB* bb, TN *pred_tn, BB_SET *hb_blocks)
/////////////////////////////////////
//
//  Predicate <bb> with the predicate associated with bb_src
//  if a predicate has been assigned to it (the only blocks for
//  which this should not be true is the entry block and those
//  equivalent to it).
//
/////////////////////////////////////
{
  BOOL all_local;
  INT i;
  TN *result_tn;
  
  if (pred_tn && pred_tn != True_TN) {
    for (OP* op = BB_first_op(bb); op != NULL; op = OP_next(op)) {
#if defined KEY && defined TARG_MIPS
      switch(op->opr) {
      case TOP_j: 
	{
	  // Generate a conditional branch
	  OPS new_ops;
	  OPS_Init(&new_ops);
	  if (TN_register_class(pred_tn) == ISA_REGISTER_CLASS_fcc)
	    Build_OP(TOP_bc1f, pred_tn, OP_opnd(op, 0), &new_ops);
	  else
	    Build_OP(TOP_beq, pred_tn, Zero_TN, OP_opnd(op, 0), &new_ops);
	  BB_Insert_Ops_Before(bb, op, &new_ops);
	  OP_Change_To_Noop(op);
	  break;	
	}
      default:
	{
	  if (OP_load(op)) {
            OPS new_ops;
	    OPS_Init(&new_ops);
	    Predicate_Read_Write(&new_ops, op, pred_tn);
	    BB_Insert_Ops_Before(bb, op, &new_ops);
	    break;
	  } else if (OP_results(op) == 1) {
	    // For every operation, if the result is is not live out, 
	    // then, do not predicate it.
	    if (!GRA_LIVE_TN_Live_Outof_BB (OP_result(op, 0), bb))
	      break;
	    if (Eager_Level >= EAGER_MEMORY) {
	      // If the result of this operation is an operand to 
	      // a subsequent load in this block, then do not predicate
	      // this operation.
	      BOOL unremovable_op = FALSE;
	      for (OP *op_next = op->next; 
		   op_next != NULL && unremovable_op != TRUE;		     
		   op_next = op_next->next) {
		if (OP_load(op_next)) {
		  TN *tn = OP_result(op, 0);
		  for (INT i = 0; i < OP_opnds(op_next); i ++) {
		    if (OP_opnd(op_next, i) == tn) {
		      unremovable_op = TRUE;
		      break;
		    }
		  }
		}
	      }
	      if (unremovable_op)
		break;
	    }
	    // Re-direct the result and generate a conditional move
	    if (TN_register_class(OP_result(op, 0)) == 
		ISA_REGISTER_CLASS_hilo)
	      break;
	    TN *res_tn = Build_TN_Like(OP_result(op, 0));
	    OPS new_ops;
	    OPS_Init(&new_ops);
	    INT opnds = OP_opnds(op);
	    FmtAssert((opnds != 0 && opnds <= 3), ("Handle this case"));
	    if (OP_cond_def(op))
	      // Just so that it does not execute the ususal expansion below
	      opnds = 4; 
	    switch(opnds) {
	    case 1: 
	      Build_OP(OP_code(op), res_tn, OP_opnd(op, 0),
		       &new_ops);
	      break;
	    case 2: 
	      Build_OP(OP_code(op), res_tn, OP_opnd(op, 0), OP_opnd(op, 1), 
		       &new_ops);	  
	      break;
	    case 3: 
	      Build_OP(OP_code(op), res_tn, OP_opnd(op, 0), OP_opnd(op, 1), 
		       OP_opnd(op, 2), 
		       &new_ops);	  
	      break;
	    }
	    FmtAssert(!TN_is_fcc_register(res_tn), 
		      ("Handle this case"));
	    if (!OP_cond_def(op)) {
	      if (!TN_is_float(res_tn)) {
		if (TN_register_class(pred_tn) == ISA_REGISTER_CLASS_fcc)
		  Build_OP(TOP_movf, OP_result(op, 0), res_tn, 
			   pred_tn, &new_ops);
		else
		  Build_OP(TOP_movz, OP_result(op, 0), res_tn, 
			   pred_tn, &new_ops);
	      } else {
		if (TN_register_class(pred_tn) == ISA_REGISTER_CLASS_fcc)
		  Build_OP(TN_size(res_tn)==4?TOP_movf_s:TOP_movf_d, 
			   OP_result(op, 0), res_tn, pred_tn, &new_ops);
		else
		  Build_OP(TN_size(res_tn)==4?TOP_movz_s:TOP_movz_d, 
			   OP_result(op, 0), res_tn, pred_tn, &new_ops);
	      }
	    } else {	      
	      FmtAssert(TN_register_class(pred_tn) != ISA_REGISTER_CLASS_fcc, 
			("We do not handle float branches yet"));
	      FmtAssert(TN_register_class(OP_opnd(op, 2)) !=
			ISA_REGISTER_CLASS_fcc,
			("We do not handle float cond ops yet"));
	      TN *tmp_tn = Build_TN_Like(pred_tn);
	      if (OP_code(op) == TOP_movz || OP_code(op) == TOP_movz_d || 
		  OP_code(op) == TOP_movz_d) {
		Build_OP(TOP_or, tmp_tn, OP_opnd(op, 1), pred_tn, &new_ops);
		Build_OP(OP_code(op), OP_result(op, 0), OP_opnd(op, 0), 
			 tmp_tn, &new_ops);
	      } else { 
		// OP_code(op) == TOP_movn || OP_code(op) == TOP_movn_d || 
		// OP_code(op) == TOP_movn_s
		Build_OP(TOP_xor, tmp_tn, OP_opnd(op, 1), Zero_TN, &new_ops);
		Build_OP(TOP_sltiu, tmp_tn, tmp_tn, Gen_Literal_TN(1, 4), 
			 &new_ops);
		Build_OP(TOP_or, tmp_tn, tmp_tn, pred_tn, &new_ops);
		switch(OP_code(op)) {
		case TOP_movn:
		  Build_OP(TOP_movz, OP_result(op, 0), OP_opnd(op, 0), tmp_tn, 
			   &new_ops);		
		  break;
		case TOP_movn_d:
		  Build_OP(TOP_movz_d, OP_result(op, 0), OP_opnd(op, 0), 
			   tmp_tn,&new_ops);		
		  break;
		case TOP_movn_s:
		  Build_OP(TOP_movz_s, OP_result(op, 0), OP_opnd(op, 0), 
			   tmp_tn, &new_ops);		
		  break;
		default:
		  FmtAssert(FALSE, ("Handle this case"));
		}
	      } 
	    }
	    Set_OP_cond_def_kind(OPS_last(&new_ops), OP_ALWAYS_COND_DEF);      
	    BB_Insert_Ops_Before(bb, op, &new_ops);
	    OP_Change_To_Noop(op);
	    break;	    
	  } else if (OP_store(op)) {
	    OPS new_ops;
	    OPS_Init(&new_ops);	    
	    Predicate_Read_Write(&new_ops, op, pred_tn);
	    BB_Insert_Ops_Before(bb, op, &new_ops);
	    break;	    
	  }
	} 
      }
#endif
      if ( ! OP_has_predicate(op)) continue;

      if (TN_is_true_pred(OP_opnd(op, OP_PREDICATE_OPND))) {
	CGTARG_Predicate_OP(bb, op, pred_tn);
#ifdef TARG_IA64
	if (OP_icmp(op)
	|| OP_code(op) == TOP_tbit_nz
	|| OP_code(op) == TOP_tbit_z
	|| OP_code(op) == TOP_fcmp_eq
        || OP_code(op) == TOP_fcmp_ge
        || OP_code(op) == TOP_fcmp_gt
        || OP_code(op) == TOP_fcmp_le
        || OP_code(op) == TOP_fcmp_lt
        || OP_code(op) == TOP_fcmp_neq
        || OP_code(op) == TOP_fcmp_nge
        || OP_code(op) == TOP_fcmp_ngt
        || OP_code(op) == TOP_fcmp_nle
        || OP_code(op) == TOP_fcmp_nlt
        || OP_code(op) == TOP_fcmp_ord
        || OP_code(op) == TOP_fcmp_unord) 
        {
		// if had a default, non-unconditional compare,
		// then when change from true_pred to pred_tn,
		// should also switch to unc form of cmp.
		TOP top = CGTARG_Get_unc_Variant(OP_code(op));
		if (top != OP_code(op)) {
			DevWarn("predicate, so change opcode to %s", TOP_Name(top));
			OP_Change_Opcode(op, top);
		}
	} 

        
	if (Is_Para_Comp_May_Def(op)) 
	     continue;
#endif
	// Locally predicate-aware GRA_LIVE does a better, safer job of this
	//
	// We need to see if we should reset the cond_def flag.
	// We do this if all the results TN's are local registers.
	// We also don't want to eliminate cond_def on stores, since they are
	// always global in a sense.
	if (OP_store(op)) {
	  all_local = FALSE;
	} else {
	  all_local = TRUE;
	}
	for ( i = OP_results(op) - 1; i >= 0; --i ) {
	  result_tn = OP_result(op, i);
	  if (TN_is_register(result_tn) && !TN_is_const_reg(result_tn)) {
	    if (TN_is_global_reg(result_tn) || TN_is_dedicated(result_tn)) {
	      all_local = FALSE;
	      break;
	    }
	  }
	}
	
	if (all_local && OP_cond_def(op)) {
	  Set_OP_cond_def_kind(op,OP_ALWAYS_UNC_DEF);
	}
      } 
      else if ( ! HB_exclude_pgtns) {
	// If an op is already predicated, then we leave it alone
	// (we will have predicated the compare that sets its predicate).
	// However, if is predicated with a global tn (!exclude_pgtns),
	// then need to somehow combine the predicates.
	TN *ptn = OP_opnd(op, OP_PREDICATE_OPND);
	if (TN_is_global_reg(ptn) && ptn != pred_tn) {
DevWarn("bb %d: need to predicate existing predicate %d with %d", BB_id(bb), TN_number(ptn), TN_number(pred_tn));
		if (HB_Trace(HB_TRACE_CONVERT)) {
			fprintf(TFile, "predicate global pred in: ");
			Print_OP_No_SrcLine(op);
		}
		if ( PQSCG_is_disjoint(pred_tn, ptn)) {
			DevWarn("disjoint predicates so remove op");
			BB_Remove_Op(bb, op);
		}
		else if ( PQSCG_is_subset_of(ptn, pred_tn)) {
			// if ptn is subset of pred_tn then ignore 
		}
		else if ( PQSCG_is_subset_of(pred_tn, ptn)) {
			// if pred_tn is subset of ptn then switch predicates
			DevWarn("subset predicates so switch predicates");
			Set_OP_opnd(op, OP_PREDICATE_OPND, pred_tn);
		}
		else {
			AND_Predicate_To_OP (op, pred_tn, ptn, bb, op, hb_blocks);
		}
	}
      }
    } // for
  } // if
}

/////////////////////////////////////
//
// Make a goto from bb to fall_thru_exit, predicated with
// pred_tn. block_freq is the frequency of the new block.
// fall_thru_prog is the probability that the goto is executed
// i.e. the probability that the arc from bb->fall_through_block is executed.
// Goto_executes_prob is the probability that the fall through branch is taken
// 
static BB*
Make_Fall_Thru_Goto(BB*                  bb, 
		    BB*                  fall_thru_exit, 
		    TN *                 pred_tn,
		    float                block_freq,
		    float                fall_thru_prob,
		    float                goto_executes_prob,
		    std::list<HB_CAND_TREE*>& candidate_regions,
		    BOOL last_block,
		    BB_SET *hb_blocks)
{
  BB* fall_thru_goto = Gen_And_Insert_BB_After(bb);
  Unlink_Pred_Succ(bb, fall_thru_exit);
  Link_Pred_Succ_with_Prob(bb, fall_thru_goto,fall_thru_prob);
  BB_freq(fall_thru_goto) = block_freq;

  Add_Goto(fall_thru_goto, fall_thru_exit);
  if (pred_tn && pred_tn != True_TN) {
    Make_Branch_Conditional(fall_thru_goto);
    Predicate_Block(fall_thru_goto,pred_tn, hb_blocks);
    Unlink_Pred_Succ(fall_thru_goto, fall_thru_exit);
    Link_Pred_Succ_with_Prob(fall_thru_goto,fall_thru_exit,goto_executes_prob);
  }  

  // Add the block to the hyperblock trees
  Add_Block(bb,fall_thru_goto,candidate_regions);
  GRA_LIVE_Compute_Liveness_For_BB(fall_thru_goto);

  return fall_thru_goto;
}


static INT 
Classify_BB(BB *bb, HB *hb)
{
  INT result;
  BB *fall_thru;
  BB *other_succ;
  BOOL fall_thru_out;
  BOOL other_out;
  OP *br;
  BBLIST *bl;

  result = 0;
  // catch the call cases and exit cases
  if (BB_exit(bb) || BB_call(bb)) {
    result |= NO_MERGE;
  }
  
#ifdef TARG_IA64
  Remove_Explicit_Branch(bb);
#endif
  fall_thru = BB_Fall_Thru_Successor(bb);
  other_succ = NULL;

  FOR_ALL_BB_SUCCS(bb, bl) {
    other_succ = BBLIST_item(bl);
    if (other_succ != fall_thru) break;
  }
  // check for easy case, all succs in HB

  other_out = FALSE;
  fall_thru_out = FALSE;
  
  if (fall_thru && (!HB_Contains_Block(hb, fall_thru) ||
		    fall_thru == HB_Entry(hb))) {
    fall_thru_out = TRUE;
  }
  if (other_succ && (!HB_Contains_Block(hb, other_succ) ||
		     other_succ == HB_Entry(hb))) {
    other_out = TRUE;
  }
  if (!other_out && !fall_thru_out) {
     if (BB_exit(bb) || BB_call(bb)) {
	result |= CASE_CALL_IN;
     } else {
	result |= CASE_ALL_IN_HB;
     }
     return (result);
  }

  // At least one block is outside the HB. Identify the cases.
  result |= NO_MERGE;
  if (BB_exit(bb) || BB_call(bb)) {
    result |= CASE_CALL;
    result |= NEEDS_FTG;
    return result;
  }

  br = BB_branch_op(bb);

  if (!br) {
    // No branch, just fall_thru. If we are here, it must be to outside
    // the block
    result |= CASE_FALL_OUT;
    return result;
  }

  if (!OP_cond(br)) {
    // Unconditional branch
    result |= CASE_UNCOND_BR;
    return result;
  }

  // Conditional branch, better have two successors
  FmtAssert(fall_thru && other_succ,
	    ("Conditional branch block (BB%d) with only one successor",BB_id(bb)));
  FmtAssert(fall_thru_out || other_out,("Bad identification logic"));
  if (fall_thru_out && other_out) {
    result |= NEEDS_FTG;
    result |= CASE_IF_OUT;
  } else if (fall_thru_out && !other_out) {
    result |= CASE_IF_FALL_OUT;
  } else { //(!fall_thru_out && other_out)
    result |= CASE_IF_FALL_IN;
  }
  return result;
}


////////////////////////////////////////////////////////////////
//
//  This routine takes a hyperblock and produces a flow-ordering
//  and block classification for later if-conversion. It returns TRUE if
//  after if conversion, only one BB will be left.
//
//  block_order and block_class are the outputs.
//
////////////////////////////////////////////////////////////////

static BOOL
Order_And_Classify_Blocks(HB* hb, 
			  std::vector<BB *> &block_order,
			  std::vector<INT>  &block_class)
{
  std::queue<BB *>  blocks_to_do;
  BB * bb;
  BBLIST *bl;
  BB *succ;
  
  // Phase one: determine a block order, and classify all the blocks
  BB_MAP predecessor_count = BB_MAP32_Create();
  HB_Predecessor_Count(hb, predecessor_count);
  
  //
  // Push the entry block on the queue to start the process
  //
  blocks_to_do.push(HB_Entry(hb));
  
  // 
  // Go through all the blocks in a breadth-first style, and put all the 
  // blocks on in the block_order vector when their predecessors are all 
  // there.
  //
  INT pred_count;
  while (!blocks_to_do.empty()) {
    bb = blocks_to_do.front();
    blocks_to_do.pop();
    pred_count = BB_MAP32_Get(predecessor_count, bb);
    if (pred_count == 0) {
      //
      // Set the predecessor count to -1 so we don't try to reprocess the 
      // block. Add the block to the list since all its predecessors have
      // been processed.
      //
      BB_MAP32_Set(predecessor_count, bb, -1);
      block_order.push_back(bb);
      block_class.push_back(Classify_BB(bb,hb));
      // Decrement the predecessor count of all the successors, and push them
      FOR_ALL_BB_SUCCS(bb,bl) {
	succ = BBLIST_item(bl);
	// remember to omit a possible back-edge to the entry
	if (BB_SET_MemberP(HB_Blocks(hb),succ) && succ != HB_Entry(hb)) {
	  BB_MAP32_Set(predecessor_count, succ, BB_MAP32_Get(predecessor_count,succ)-1);
	  blocks_to_do.push(succ);
	}
      }
    } else if (pred_count > 0) {
      // push it back for later consideration
      blocks_to_do.push(bb);
    }
  }

  BB_MAP_Delete(predecessor_count);

  //
  // Decide if we can merge all blocks together
  // Look at all the blocks except the last one
  // since it doesn't matter what type it is.
  //
  INT i;
  for (i = 0; i < block_order.size()-1; i++) {
    if (No_Merge(block_class[i])) return (FALSE);
  }

  return (TRUE);
}

#undef CHECK_FREQENCIES
#ifdef CHECK_FREQENCIES
////////////////////////////////////////////////////////////////
//
//  Make sure frequency data is reasonable
//
static void
Check_Block_Frequencies(BB_SET* bbs, char *label) 
{
  INT idx;
  BB  *b;
  BBLIST *bl;
  float succ_total;
  float succ_prob;
  INT num_succs;

  FOR_ALL_BB_SET_members(bbs,b) {
    succ_total = 0;
    num_succs = 0;

    FOR_ALL_BB_SUCCS(b,bl) {
      ++num_succs;
      succ_prob = BBLIST_prob(bl);
      if (succ_prob < 0 || succ_prob > 1.0) {
	fprintf(HB_TFile,"%s: Bad succ prob %e on arc BB%d -> BB%d\n",label,
		succ_prob,BB_id(b),BB_id(BBLIST_item(bl)));
      }
      succ_total += succ_prob;
    }
    if (num_succs != 0 && (succ_total < 0.9999 || succ_total > 1.0001)) {
      fprintf(HB_TFile,"%s: Bad succ total %e for BB%d\n",label,
	      succ_total,BB_id(b));
    }
  }
}
#endif

////////////////////////////////////////////////////////////////
//
// Set up the frequency map for the blocks in the hyperblock.
//
static void
Compute_Block_Frequencies(std::vector<BB *> &block_order, BB_MAP freq_map)
{
  INT idx;
  FREQ_DATA *bb_freq_data;
  BB *bb;
  BB *succ;
  BBLIST *bl;
  
  for (idx = 0; idx < block_order.size(); idx++) {
    // Put the map on the BB, and initialize the frequency data
    bb = block_order[idx];
    bb_freq_data = Get_Freq_Data_For_BB(bb,freq_map);
    if (idx == 0) {
      // First block probability is one
      bb_freq_data->block_prob = 1.0;
    }
    //
    // Propagate information to the successor blocks
    // Each blocks probability is the sum of the probabilities along all the 
    // branches. Later, we will set the probability for all the branches 
    // which exit the block.
    //
    FOR_ALL_BB_SUCCS(bb,bl) {
      float succ_prob = BBLIST_prob(bl);
      succ = BBLIST_item(bl);
      if (succ != block_order[0]) {
	//
	// Add the probability of getting to this point along this arc to 
	// the BB freq data
	//
	FREQ_DATA * succ_freq_data = Get_Freq_Data_For_BB(succ,freq_map);
	succ_freq_data->block_prob += succ_prob*bb_freq_data->block_prob;
      }
    }
  }
}


#if defined KEY && defined TARG_MIPS
BOOL Okay_To_Predicate_BB (BB *bb, BB *prev_bb)
{
  if (BB_branch_op(bb) && BB_branch_op(prev_bb) &&
      !OP_cond(BB_branch_op(bb)) && OP_cond(BB_branch_op(prev_bb))) {
    if (OP_code(BB_branch_op(prev_bb)) == TOP_beq || 
	OP_code(BB_branch_op(prev_bb)) == TOP_bne) {
      if (TN_is_label(OP_opnd(BB_branch_op(bb), 0)) &&
	  TN_is_label(OP_opnd(BB_branch_op(prev_bb), 2))) {
	if (TN_label(OP_opnd(BB_branch_op(bb), 0)) == 
	    TN_label(OP_opnd(BB_branch_op(prev_bb), 2)))
	  return TRUE;
	else
	  return FALSE;
      } else {
	return FALSE;
      }
    } else {
      FmtAssert((OP_code(BB_branch_op(prev_bb)) == TOP_bc1f || 
		 OP_code(BB_branch_op(prev_bb)) == TOP_bc1t), 
		("Handle this case"));
      if (TN_is_label(OP_opnd(BB_branch_op(bb), 0)) &&
	  TN_is_label(OP_opnd(BB_branch_op(prev_bb), 1))) {
	if (TN_label(OP_opnd(BB_branch_op(bb), 0)) == 
	    TN_label(OP_opnd(BB_branch_op(prev_bb), 1)))
	  return TRUE;
	else
	  return FALSE;
      } else {
	return FALSE;
      }
    }
  } else if (BB_length(prev_bb) == 0) {
    return FALSE;
  } else {
    return TRUE;
  }
}
#endif
/////////////////////////////////////
//
//  Remove all branches that don't have targets
//  outside the hyperblock. Returns the last fall_through_goto block if it was created.
//
/////////////////////////////////////
static BB *
Remove_Branches(HB*                  hb, 
		BB_MAP               predicate_tns, 
		std::vector<BB *>         &block_order,
		std::vector<INT>          &block_class,
		std::list<HB_CAND_TREE*>& candidate_regions)
{
   BB * bb;
   BB * last_ft_block = NULL;    // keeps track of the last fall-though block inserted
   BB * prev_bb;
   INT idx;
   BB *fall_thru;   // Fallthru block for a BB
   BB *branch_bb;   // Block that a taken branch would go to for a BB
   BB *fall_thru_goto;
   BOOL last_block;
   BOOL prev_bb_unmergeable;
   BB_MAP freq_map;
   FREQ_DATA *bb_freq_data;
   float hb_continuation_prob;  // probablility that the HB will continue executing
   float prev_hb_continuation_prob;  // probablility that the HB will continue executing
   float block_freq;  // frequency of the current block

#ifdef CHECK_FREQENCIES
   Check_Block_Frequencies(HB_Blocks(hb),"Before");
#endif

   //
   // Compute the frequency information for the post-merge BB's
   //
   freq_map = BB_MAP_Create();
   Compute_Block_Frequencies(block_order,freq_map);
      
   // make sure pqs is available
   if (!PQSCG_pqs_valid()) {
	PQSCG_reinit(REGION_First_BB);
   }

   // Dump the trace information
   if (HB_Trace(HB_TRACE_CONVERT)) {
     INT i;
     fprintf(HB_TFile, "<HB> Converted block order:\n<HB>\t");
     for (i = 0; i < block_order.size(); i++) {
       fprintf(HB_TFile, "%d(%d[%5.3f]) ",BB_id(block_order[i]),Get_Case(block_class[i]),
	       Get_Block_Prob(block_order[i],freq_map));
       if (No_Merge(block_class[i])) {
	 fprintf(HB_TFile, "| ");
       }
       if (Needs_FTG(block_class[i])) {
	 fprintf(HB_TFile, "G | ");
       }
     }
     fprintf(HB_TFile, "\n");
   }
   
   //
   // Now start the task of merging. Insert whatever GOTO's and blocks we need 
   // to. The entry block is always block 0. 
   //
   fall_thru_goto = NULL;
   prev_bb = NULL;
   block_freq = BB_freq(block_order[0]);

   for (idx=0; idx < block_order.size(); idx++) {
     bb = block_order[idx];
     INT bclass = block_class[idx];
     last_block = (idx == block_order.size()-1);

     // Last_block is used to set conditions on the branch in the last BB
     // of the hyperblock. This one needs to be an unconditional branch.
     
     bb_freq_data = Get_Freq_Data_For_BB(bb,freq_map);
     fall_thru = bb_freq_data->fall_thru;
     branch_bb = bb_freq_data->branch_bb;
     
     switch (Get_Case(bclass)) {
	 
     case CASE_ALL_IN_HB:
       // Remove all the branches at the end of the block, if possible, then 
       // predicate it. 
       BB_Remove_Branch(bb);
       hb_continuation_prob = 1.0;  // Was: Get_Block_Prob(bb,freq_map);
       break;
	  
     case CASE_CALL_IN:
       // Unlink the fall through so the probablility is right later
       // Fall_thru will be null if it's an exit block. 
       if (fall_thru) {
	 Unlink_Pred_Succ(bb,fall_thru);
       }
       hb_continuation_prob = 1.0; // We always execute the next block
       break;
       
     case CASE_CALL:
       // Need to insert a fall-through goto
       // Probability of executing the fall_thru goto is 1.
       // Also, this may be an exit, in which case we do nothing
       // If fall_thru is NULL, we are an exit block.
       if (fall_thru) {
	 if (!last_block) {
	   fall_thru_goto = Make_Fall_Thru_Goto(bb,fall_thru,
						(TN*) BB_MAP_Get(predicate_tns,bb),
						block_freq,
						1.0,
						Get_Block_Prob(bb,freq_map),
						candidate_regions,FALSE,
						HB_Blocks(hb));
	 } else {
	   fall_thru_goto = Make_Fall_Thru_Goto(bb,fall_thru,
						NULL,
						block_freq,
						1.0,
						1.0,
						candidate_regions,FALSE,
						HB_Blocks(hb));
	 }
       }
       hb_continuation_prob = 1.0 - Get_Block_Prob(bb,freq_map);
       break;
	  
	  
     case CASE_UNCOND_BR:
       if (!last_block) {
	 // Just make the branch conditional
	 Make_Branch_Conditional(bb);
	 // Reset the probablility on the link
	 Unlink_Pred_Succ(bb,branch_bb);
	 Link_Pred_Succ_with_Prob(bb,branch_bb,Get_Block_Prob(bb,freq_map));
	 hb_continuation_prob = 1.0 - Get_Block_Prob(bb,freq_map);
       } else {
	 Unlink_Pred_Succ(bb,branch_bb);
	 Link_Pred_Succ_with_Prob(bb,branch_bb,1.0);
       }
	    
       break;
	  
     case CASE_FALL_OUT:
       // Add a branch to the block
       if (!last_block) {
	 Add_Goto(bb, fall_thru);
	 Make_Branch_Conditional(bb);
	 Unlink_Pred_Succ(bb,fall_thru);
	 Link_Pred_Succ_with_Prob(bb,fall_thru,Get_Block_Prob(bb,freq_map));
	 // Unlink to reset the branch frequencies
       } else {
	 Unlink_Pred_Succ(bb,fall_thru);
	 Add_Goto(bb, fall_thru);
       }
       hb_continuation_prob = 1.0 - Get_Block_Prob(bb,freq_map);
       break;
       
     case CASE_IF_FALL_OUT:
       // Invert the branch to make the fall through the one in the hyperblock
       // then predicate it.
       TN *inverse_pred,*current_pred;
       // This will either grab what's already there, or else create a new
       // false predicate if it isn't there
       Exp_True_False_Preds_For_Block(bb, current_pred, inverse_pred);
       // Remove the current branch
       BB_Remove_Branch(bb);
       Unlink_Pred_Succ(bb,branch_bb);
       // Add in a new branch to the old fall_thru
       fall_thru_goto = Make_Fall_Thru_Goto(bb,fall_thru,inverse_pred,
					    block_freq,
					    1.0,
					    Get_Branch_Prob(bb,fall_thru,freq_map),
					    candidate_regions,last_block,
					    HB_Blocks(hb));
       hb_continuation_prob = 1.0 - Get_Branch_Prob(bb,fall_thru,freq_map);
       break;
       
     case CASE_IF_FALL_IN:
       // Only need to predicate it
       // Reset the probablility on the other link
       Unlink_Pred_Succ(bb,branch_bb);
       Link_Pred_Succ_with_Prob(bb,branch_bb,Get_Branch_Prob(bb,branch_bb,freq_map));
       Unlink_Pred_Succ(bb,fall_thru);
       hb_continuation_prob = 1.0 - Get_Branch_Prob(bb,branch_bb,freq_map);
       break;
       
       
     case CASE_IF_OUT:
       // Need to insert a fall-through goto, then conditionalize it with the 
       // inverse conditional.
       //
       // Reset the links to the block outside
       Unlink_Pred_Succ(bb,branch_bb);
       Link_Pred_Succ_with_Prob(bb,branch_bb,Get_Branch_Prob(bb,branch_bb,freq_map));
       if (!last_block) {
	 TN *inverse_pred,*current_pred;
	 float ft_goto_prob,ft_prob;
	 ft_goto_prob = 1.0 - Get_Branch_Prob(bb,branch_bb,freq_map);
	 ft_prob = Get_Branch_Prob(bb,fall_thru,freq_map);
	 if (ft_goto_prob == 0.0) {
	   // We have a problem, the frequency data is bad, so we make something up
	   ft_prob = 0.5;
	 } else {
	   ft_prob = ft_prob / ft_goto_prob;
	 }
	   
	 // This will either grab what's already there, or else create a new
	 // false predicate if it isn't there
	 Exp_True_False_Preds_For_Block(bb, current_pred, inverse_pred);
	 fall_thru_goto = Make_Fall_Thru_Goto(bb,fall_thru,inverse_pred,
					      block_freq * ft_goto_prob,
					      ft_goto_prob,
					      ft_prob,
					      candidate_regions,last_block,
					      HB_Blocks(hb));
	 
	 hb_continuation_prob = 1.0 - ft_prob;
       } else {
	 float ft_goto_prob = 1.0 - Get_Branch_Prob(bb,branch_bb,freq_map);
	 // In the last block, we make the fall_thru_goto unconditional
	 fall_thru_goto = Make_Fall_Thru_Goto(bb,fall_thru,NULL,
					      block_freq * ft_goto_prob,
					      ft_goto_prob,
					      1.0,candidate_regions,last_block,
					      HB_Blocks(hb));
       }
       break;
       
     default:
       FmtAssert(0,("Unknown block classification"));
     }
           
#if defined KEY && defined TARG_MIPS
     merge_failed = FALSE;

     if (last_block) {
       if (!prev_bb_unmergeable) {	 
	 // see if we can merge in future then predicate it.
	 if (Okay_To_Predicate_BB(bb, prev_bb)) 
	   Predicate_Block(bb,(TN*) BB_MAP_Get(predicate_tns, bb), 
			   HB_Blocks(hb));
	 else
	   merge_failed = TRUE;
       }
     } else
       // This avoids unnecessarily creating a conditional branch
#endif
     // We always predicate the current block
     Predicate_Block(bb,(TN*) BB_MAP_Get(predicate_tns, bb), HB_Blocks(hb));
     
     // Update the frequency of the block
     BB_freq(bb) = block_freq;

     // See if we can merge with the previous block. If not, we need to
     // reposition ourselves to follow it.
     if (prev_bb) {
       if (prev_bb_unmergeable) {
	 Unlink_Pred_Succ(prev_bb, bb);
	 Move_BB(bb, prev_bb);
	 Link_Pred_Succ_with_Prob(prev_bb, bb, prev_hb_continuation_prob);
	 prev_bb = bb;
       } else {
	 Merge_Blocks(hb,prev_bb,bb,freq_map,last_block,candidate_regions);
#ifndef TARG_IA64
	 if (merge_failed) {
	   printf("MERGE FAILED\n\n\n");
	   Unlink_Pred_Succ(prev_bb, bb);
	   Move_BB(bb, prev_bb);
	   Link_Pred_Succ_with_Prob(prev_bb, bb, prev_hb_continuation_prob);
	   prev_bb = bb;
	 } else 
#endif
	 // Don't set prev_bb again, since we add to the current block
	 // Also, if we have a fall_thru_goto, we want to make sure it's in all hyperblocks 
	 // which contain the merged BB as well.
	 //
	 if (fall_thru_goto) {
	   Add_Block(prev_bb,fall_thru_goto,candidate_regions);
	 }
       }
     } else {
       prev_bb = bb;
     }
     
     if (fall_thru_goto) {
       // If we inserted a fall_thru_goto, we treat it as an unmergeable block
       // Since we either just merged or moved it successor, we need to move this into place
       Move_BB(fall_thru_goto,prev_bb);
       prev_bb = fall_thru_goto;
       prev_bb_unmergeable = TRUE;
       if (last_block) {
	 last_ft_block = fall_thru_goto;
       }
       fall_thru_goto = NULL;
     } else {
#ifndef TARG_IA64
       if (prev_bb == bb) 
	 // only if the prev_bb changed we need to reset 
	 // prev_bb_unmergeable
#endif
       prev_bb_unmergeable = No_Merge(bclass);
     }
     
     prev_hb_continuation_prob = hb_continuation_prob;
     block_freq *= hb_continuation_prob;
   } // End of loop over BB's
   
   //
   // Set Global TN's properly
   //
   GRA_LIVE_Detect_GTNs_In_Set(HB_Blocks(hb));

   // We are done. The old blocks have been removed from the hyperblock
   BB_MAP_Delete(freq_map);

#ifdef CHECK_FREQENCIES
   Check_Block_Frequencies(HB_Blocks(hb),"After");
#endif
   return last_ft_block;

}

//================================================================
//
//                Insertion of Predicates
//
//================================================================

/////////////////////////////////////
//
// equiv_class: The structure defining an control dependence equivalence class for the 
// control_dep_data : A structure containing the predicates which need to be set in a BB 
// for doing predication.
//
struct equiv_classes {
   BB_SET* control_dependences;
   BB_SET* true_edges;
   TN* pred_tn;
};

struct control_dep_data {
  TN* true_tn;
  TN* false_tn;
  std::vector <TN*> true_or_tns;
  std::vector <TN*> false_or_tns;

  control_dep_data() {
    true_tn = NULL;
    false_tn = NULL;
  }
};


///////////////////////////////////////////////////////////
//
// Insert all the instructions setting the compound predicates
// These will be a set of pairwise ORs sets. The TNs in the true_or_tns
// vector are set up so that they will be true when true_tn is true, and the 
// false_or_tns are set up so that they will be true when the false_tn is true.
//
static void
Insert_ORs_For_BB(BB *bb, control_dep_data *bb_cdep_data)
{
  OPS ops = OPS_EMPTY;
  INT i;
  INT num_to_set;
  TN *r1;
  TN *r2;
  TN *q;
  
  // Get the TRUE pairs
  num_to_set = bb_cdep_data->true_or_tns.size();
  q = bb_cdep_data->true_tn;
  for (i = 0; i < num_to_set; i += 2) {
    r1 = bb_cdep_data->true_or_tns[i];
    if (i+1 < num_to_set) {
      r2 = bb_cdep_data->true_or_tns[i+1];
    } else {
      r2 = True_TN;
    }
    Exp_Generic_Pred_Calc(r1,r2, COMPARE_TYPE_or, q, &ops);
  }
  
  // Now do the FALSE pairs
  num_to_set = bb_cdep_data->false_or_tns.size();
  q = bb_cdep_data->false_tn;
  for (i = 0; i < num_to_set; i += 2) {
    r1 = bb_cdep_data->false_or_tns[i];
    if (i+1 < num_to_set) {
      r2 = bb_cdep_data->false_or_tns[i+1];
    } else {
      r2 = True_TN;
    }
    Exp_Generic_Pred_Calc(r1,r2, COMPARE_TYPE_or, q, &ops);
  }
  
  // Insert these just before the ending branch
  OP* br_op = BB_branch_op(bb);
  BB_Insert_Ops(bb, br_op, &ops, TRUE);
}

///////////////////////////////////////////////////////////
//
// Setup the true_tn and false_tn for a BB. The true_tn is a TN such that
// it is true if the branch at the end of a BB is taken. If it false
// through the false_tn will be set true.
// 
// This routine works by trying to find the compare which generates the
// branch predicate for the block. Assuming it finds one, and it's of the
// right form (i.e. an unc form), it attempts to simply re-use the two TN's
// it generates. 
//
// Right now, if it doesn't find it, it asserts, but I don't think this is
// going to happen, given the current way we generate things.
//
// Well, the above remark is untrue. We can fail to find the compare
// if we are trying to set up the predicates for a block which has previously been if-converted.
// In this case, we need to set up the FALSE predicate in such a way that it's TRUE if the block is
// executed and the branch at the end is not taken.
//
static void
Setup_True_False_Predicates(BB *bb, control_dep_data *bb_cdep_data)
{
  Exp_True_False_Preds_For_Block(bb, bb_cdep_data->true_tn, bb_cdep_data->false_tn);
}


#ifndef TARG_IA64
//#ifdef KEY
static BOOL multiple_dependencies;
#endif
/////////////////////////////////////
void
Insert_Predicates(HB* hb, BB_MAP control_dependences, BB_MAP true_edges,
		  BB_MAP predicate_tns)
/////////////////////////////////////
//
//  Form equivalence classes of blocks that share precisely the same
//  control dependences (same blocks, same edges).  Assign a predicate
//  to each, and insert the calculation of that predicate in the blocks
//  that are the sources of the control dependences.
//
/////////////////////////////////////
{
  BB* bb;
  BB* bb_cd;
  std::vector <equiv_classes *> eclass;
  std::vector <equiv_classes *>::iterator ec_iter; 
  equiv_classes *ec;

  control_dep_data *bb_cdep_data;
  BB_SET* cds;
  BB_SET* trues;

  BB_MAP equiv_classes_map = BB_MAP_Create();
  BB_MAP control_dep_info = BB_MAP_Create();
  

  // First, compute the equivalence classes

  FOR_ALL_BB_SET_members(HB_Blocks(hb), bb) {
    //
    // Set up each equivalence class. We currently do a linear search, so
    // this section is N^2 in the number of equivalence classes, but
    // normally N will be small.  Note that the entry block has no need of
    // a predicate. But we will go through and generate an equivalence
    // class for it anyway.
    //
    ec = NULL;
    cds = (BB_SET*) BB_MAP_Get(control_dependences, bb);
    trues = (BB_SET*) BB_MAP_Get(true_edges, bb);
    
    //
    // Find the equivalence class.  If not member of existing one,
    // then create it and insert its predicate calculation.
    //
    for (ec_iter = eclass.begin(); ec_iter != eclass.end(); ec_iter++) {
      if (BB_SET_EqualP((*ec_iter)->control_dependences, cds) &&
	  BB_SET_EqualP((*ec_iter)->true_edges, trues)) {
	ec = *ec_iter;
      }
    }
    if (!ec) {
      ec = TYPE_MEM_POOL_ALLOC(equiv_classes, &MEM_local_pool);
      ec->control_dependences = cds;
#ifndef TARG_IA64 
      if (BB_SET_Size(cds) > 1) {
	// we do not deal with multiple dependencies
	multiple_dependencies = TRUE;
	return;
      }
#endif
      ec->true_edges = trues;
      eclass.push_back(ec);
    }
    BB_MAP_Set(equiv_classes_map,bb,ec);
    
    // Initialize the control dependency information for each BB.
    bb_cdep_data = CXX_NEW(control_dep_data, &MEM_local_pool);
    bb_cdep_data->true_tn = NULL;
    bb_cdep_data->false_tn = NULL;
    BB_MAP_Set(control_dep_info,bb,bb_cdep_data);
  }
  
#if defined KEY && defined TARG_MIPS
  // If more than one block end in a conditional branch,
  // then do not do If-conversion.
  BOOL countBranch = FALSE;
  OP *hb_br_op;
  FOR_ALL_BB_SET_members(HB_Blocks(hb), bb) {
    OP *br_op = BB_branch_op(bb);
    if (br_op && OP_cond(br_op)) {
      if (countBranch && !hammock_region) {
	multiple_dependencies = TRUE;
	return;
      }
      countBranch = TRUE;
      hb_br_op = br_op;
    }
  }
  // When the cond branch is flop, we do not handle any cond moves in the 
  // blocks. Also, we do not mixup int cond branch with any flop cond moves.
  // TODO:
  // we have to look up every hammock and make sure there are no conflicting
  // branches and cond moves in the same fashion.
  BOOL is_float_br = FALSE;
  if (hb_br_op &&
      (OP_code(hb_br_op) == TOP_bc1f || 
       OP_code(hb_br_op) == TOP_bc1t))
    is_float_br = TRUE; 
  INT bb_id = hb_br_op?hb_br_op->bb->id:-1;
  if (hb_br_op) {
    OP *op;
    FOR_ALL_BB_SET_members(HB_Blocks(hb), bb) {
      if (bb->id == bb_id)
	continue;      
      FOR_ALL_BB_OPs(bb, op) {
	if ((OP_cond_def(op) &&
	    is_float_br) || 
	    (!is_float_br &&
	     (OP_code(op) == TOP_movf || OP_code(op) == TOP_movt ||
	      OP_code(op) == TOP_movf_d || OP_code(op) == TOP_movf_s ||
	      OP_code(op) == TOP_movt_d || OP_code(op) == TOP_movt_s))) {
	  multiple_dependencies = TRUE;
	  return;
	}
      }
    }
  }     
#endif
  //
  // Next, set up the predicate data for the class, and insert initializations in then entry block
  //
  for (ec_iter = eclass.begin(); ec_iter != eclass.end(); ec_iter++) {
    ec = *ec_iter;
    cds = ec->control_dependences;
    trues = ec->true_edges;
    INT num_deps = BB_SET_Size(cds);
    if (num_deps == 0) {
      // No predicate necessary
      ec->pred_tn = NULL;
    } else if (num_deps == 1) {
      // Single dependence, set up the TRUE and FALSE tns for the block.
      bb_cd = BB_SET_Choose(cds);
      bb_cdep_data = (control_dep_data*) BB_MAP_Get(control_dep_info, bb_cd);
#ifndef TARG_IA64
      // More than one equivalence class may have the same bb_cd
      // So, choose already existing pred_tn instead of creating new ones.
      if (!bb_cdep_data->true_tn && !bb_cdep_data->false_tn)
#endif
      Setup_True_False_Predicates(bb_cd, bb_cdep_data);

      // Set the predicate for the equivalance class
      if (BB_SET_MemberP(trues, bb_cd)) {
	ec->pred_tn = bb_cdep_data->true_tn;
      } else {
	ec->pred_tn = bb_cdep_data->false_tn;
      }	
    } else {
      // Multiple dependencies. Insert an initialization in the entry block. 
      OPS ops = OPS_EMPTY;
      ec->pred_tn = Gen_Predicate_TN();
      Exp_Pred_Set(ec->pred_tn, True_TN, 0, &ops);
      OP *xfer_op;
      
      // Insert the predicate initialization code just before the branch
      // (if it exists) or append it to the Entry block.

      if (xfer_op = BB_xfer_op(HB_Entry(hb))) {
	BB_Insert_Ops_Before (HB_Entry(hb), xfer_op, &ops);
      } else {
	BB_Append_Ops (HB_Entry(hb), &ops);
      }

      // 
      // Insert the appropriate OR in the control-dependent block
      // 
      FOR_ALL_BB_SET_members(ec->control_dependences, bb_cd) {
	bb_cdep_data = (control_dep_data*) BB_MAP_Get(control_dep_info,bb_cd);
	Setup_True_False_Predicates(bb_cd,bb_cdep_data);
	// Add the pred_tn to the list of things to be or'ed in each block
	if (BB_SET_MemberP(trues, bb_cd)) {
	  bb_cdep_data->true_or_tns.push_back(ec->pred_tn);
	} else {
	  bb_cdep_data->false_or_tns.push_back(ec->pred_tn);
	}
      }
    }
  }
    
  //
  // At this point, we have predicate TN's for every equivalence class,
  // We have initialized the compound predicates, and we have set up the
  // True/False predicates for each BB.
  //
  // All that remains is to actually assign the predicate TN to each basic block, and insert the 
  // appropriate or's for the compound predicate TNs. We've jumped through all these hoops so we 
  // can try to insert them pairwise, since the OR form conveniently sets two at once.
  //
  
  FOR_ALL_BB_SET_members(HB_Blocks(hb), bb) {
    // Set the predicate TN for the BB
    ec = (equiv_classes *) BB_MAP_Get(equiv_classes_map,bb);
    BB_MAP_Set(predicate_tns, bb, ec->pred_tn);
    
    //
    // Insert the ORs for a BB
    // 
    bb_cdep_data = (control_dep_data*) BB_MAP_Get(control_dep_info,bb);
    Insert_ORs_For_BB(bb, bb_cdep_data);



    //========= Tracing information =========================================

    if (HB_Trace(HB_TRACE_CONVERT)) {
      if (ec->pred_tn) {
	fprintf(HB_TFile, "<HB> BB%d predicated with TN%d\n", BB_id(bb),
		TN_number(ec->pred_tn));
      } else {
	fprintf(HB_TFile, "<HB> BB%d has no predicate\n", BB_id(bb));
      }
      fprintf(HB_TFile, "<HB> Control dependences data:");
      INT ttn = bb_cdep_data->true_tn ? TN_number(bb_cdep_data->true_tn) : 0;
      INT ftn = bb_cdep_data->false_tn ? TN_number(bb_cdep_data->false_tn) : 0;
      fprintf(HB_TFile, "  True TN = TN%d     False TN = TN%d\n",ttn,ftn);
      INT i;
      if (bb_cdep_data->true_or_tns.size() > 0) {
	fprintf(HB_TFile, "<HB>   True  ORs:");
	for (i = 0; i < bb_cdep_data->true_or_tns.size(); i++) {
	  fprintf(HB_TFile, "TN%d ",TN_number(bb_cdep_data->true_or_tns[i]));
	}
	fprintf(HB_TFile, "\n");
      }
      if (bb_cdep_data->false_or_tns.size() > 0) {
	fprintf(HB_TFile,"<HB>   False ORs:");
	for (i = 0; i < bb_cdep_data->false_or_tns.size(); i++) {
	  fprintf(HB_TFile, "TN%d ",TN_number(bb_cdep_data->false_or_tns[i]));
	}
	fprintf(HB_TFile, "\n");
      }
      fprintf(HB_TFile,"\n");
    }
    
    CXX_DELETE(bb_cdep_data,&MEM_local_pool);
  }
  
  // Clean up
  
  BB_MAP_Delete(control_dep_info);
  BB_MAP_Delete(equiv_classes_map);
}
///////////////////////////////////////////////////////////////////////////////////
//
//      BOOL HB_Safe_For_If_Conversion(HB *hb)
//        Return TRUE if the hyperblock contains no side entrances. This
//        is used to screen out hyperblock candidates during the simple if-conversion
//        phase of the hyperblock formation. 
//
BOOL 
HB_Safe_For_If_Conversion(HB* hb)
{
  BB *bb;
  BB *bb_pred;
  BB_SET *hb_blocks;
  BBLIST* bl;

  hb_blocks = HB_Blocks(hb);
  
  FOR_ALL_BB_SET_members(hb_blocks, bb) {
    if (bb != HB_Entry(hb)) {
      FOR_ALL_BB_PREDS(bb,bl) {
	bb_pred = BBLIST_item(bl);
	if (!BB_SET_MemberP(hb_blocks,bb_pred)) {
	  // There is a side entrance, so it's not safe
	  return FALSE;
	}
      }
    }
  }
  
  return TRUE;
}


#if defined KEY && defined TARG_MIPS
// Compute the cost of If-Converting basic blocks in a hyper-block.
// The original cost is the static weighted average of latencies of ops before 
// predicating each (non-entry) basic block MINUS other savings. The cost after
// predication and If-Conversion is the sum of all latencies of ops in each 
// block (except the entry block) in the Hyper-Block.
float
HB_If_Convert_Cost_Diff(HB *hb)
{
  BB *bb;
  float initial_cost = 0, transformed_cost = 0;
  float freq;
  OP *op;
  INT sum_of_latencies;
  INT count_cond_moves = 0;
  INT op_latency;
  if (hammock_region)
    // TODO:
    // We need to compute costs of each hammock in the hammocked region
    // separately, and then add the cost diffs.
    // For now, always if-convert hammock regions.
    return 0.0;
  FOR_ALL_BB_SET_members(HB_Blocks(hb), bb) {  
    if (HB_Entry(hb) == bb) {
      // First block not predicated.
      continue;
    }
    freq = BB_freq(bb);
    sum_of_latencies = 0;
    FOR_ALL_BB_OPs(bb, op) {
      op_latency = TI_RES_Cycle_Count(OP_code(op));
      sum_of_latencies += (op_latency?op_latency:1); 
      if (OP_results(op) == 1) // needs a conditional move in transformed code
	count_cond_moves ++;	
    }
    initial_cost += sum_of_latencies*freq;
    transformed_cost += (sum_of_latencies + count_cond_moves);
    if (BB_branch_op(bb))
      transformed_cost --; // branches will be removed in transformed code.
  }
  // Count instructions needed to compute predicate TNs
  // To be more accurate, we need to consider integer and floating point 
  // branches separately
  if (BB_branch_op(HB_Entry(hb)) && 
      (OP_code(BB_branch_op(HB_Entry(hb))) == TOP_bc1f || 
       OP_code(BB_branch_op(HB_Entry(hb))) == TOP_bc1t))
    transformed_cost += 4 /* c_cond type FP instructions */; 
  else
    transformed_cost += 3 /* xor + sltiu + sltu */; 
  return transformed_cost - initial_cost;
}
#endif
/////////////////////////////////////
void
HB_If_Convert(HB* hb, std::list<HB_CAND_TREE*>& candidate_regions)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
#if defined KEY && defined TARG_MIPS
  // Compute cost-benefit and return if too costly
  if (HB_If_Convert_Cost_Diff(hb) > (float)HB_if_conversion_cut_off)
    return;
#endif
  vector<BB *>         block_order;
  vector<INT>          block_class;
  MEM_POOL_Push(&MEM_local_pool);
  BB_MAP true_edges = BB_MAP_Create();
  BB_MAP control_dependences = BB_MAP_Create();
  BB_MAP predicate_tns = BB_MAP_Create();  // The TN used to predicate a BB

  if (HB_Trace(HB_TRACE_CONVERT)) {
    HB_Trace_If_Convert_Blocks(hb);
  }
  
  Order_And_Classify_Blocks(hb,block_order,block_class);
  Calculate_Control_Dependences(hb, control_dependences, true_edges);
#ifndef TARG_IA64
  multiple_dependencies = FALSE;    
  Insert_Predicates(hb, control_dependences, true_edges, predicate_tns);
  if (!multiple_dependencies)
    Remove_Branches(hb, predicate_tns, block_order, block_class, candidate_regions);
  else
    // If hb_formation did not succeed then,no need of hb_sched phase.
    IGLS_Enable_HB_Scheduling = 0;
#else
  Insert_Predicates(hb, control_dependences, true_edges, predicate_tns);
  Remove_Branches(hb, predicate_tns, block_order, block_class, candidate_regions);
#endif

  BB_MAP_Delete(true_edges);
  BB_MAP_Delete(control_dependences);
  BB_MAP_Delete(predicate_tns);
  MEM_POOL_Pop(&MEM_local_pool);
}

static BOOL
Check_for_Cycles(HB* hb, BB* bb, BB_SET *visited)
  /////////////////////////////////////
  //
  //  Walk through all the paths in the bbset, and make sure there are no cycles in the graph
  //
  //  bb - current block we are visiting
  //  
  //  hb - hyperblock to check
  //
  //  visited - set indicating what blocks we've visited so far. If we hit ourself again,
  //  we have detected a loop, and we need to reject the hyperblock. This can occur in cases
  //  of nasty flow-of-control, where one can have a side-entry into a loop. In this case, 
  //  findloops does not see this as a loop. 
  //
  //  returns TRUE if the hyperblock is acceptable (i.e., no loops)
  //
  /////////////////////////////////////
{
  BB* succ;
  BBLIST* bl;

  if (BB_SET_MemberP(visited,bb)) {
    // Loop detected
    return FALSE;
  }
  
  // The set is large enough that we don't need to worry about reallocation
  visited = BB_SET_Union1D(visited, bb, &MEM_local_pool);

  FOR_ALL_BB_SUCCS(bb, bl) {
    succ = BBLIST_item(bl);
    //
    // We check to make sure that we always fall off the bottom, except for edges back to 
    // the entry block. 
    //
    if (succ != HB_Entry(hb) && HB_Contains_Block(hb, succ)) {
      if (!Check_for_Cycles(hb, succ, visited)) {
	return FALSE;
      }
    }
  }

  // "Unvisit" the block
  visited = BB_SET_Difference1D(visited,bb);
  return TRUE;
}


/////////////////////////////////////
BB *
Force_If_Convert(LOOP_DESCR *loop, BOOL allow_multi_bb)
/////////////////////////////////////
//
//  If-convert a loop body. If allow_multi_bb, it will always be converted if 
//  possible. Otherwise, it will be converted if the loop body results in a 
//  single BB. Return either the converted BB or NULL if the conversion is not
//  possible. 
//
/////////////////////////////////////
{
  std::vector<BB *>         block_order;
  std::vector<INT>          block_class;
  BOOL one_bb;
  BOOL ok_to_convert;
  BOOL all_blocks_ok;
  BB *single_bb=NULL;
  BB *fall_thru_block;
  BB *bb;
  BB *bb_entry;
  HB hbs;
  HB *hb=&hbs;
  std::list<HB_CAND_TREE*> candidate_regions;

#ifdef KEY
  // CG_LOOP_Optimize may call even though HB_formation is false
  if (!HB_formation)
    return NULL;
#endif
  MEM_POOL_Push(&MEM_local_pool);
  fall_thru_block = NULL;
  // Turn the BB_REGION into a "hyperblock"
  BB_SET * bbs = BB_SET_Create_Empty(PU_BB_Count+2, &MEM_local_pool);
  BB_SET * added_bbs;
  BB_SET * removed_bbs;
  bb_entry = LOOP_DESCR_loophead(loop);

  bbs = BB_SET_UnionD(bbs,LOOP_DESCR_bbset(loop), &MEM_local_pool);
  // 
  // possible quick exit if there is already only one block in the region
  //
  if (BB_SET_Size(bbs) == 1) {
    MEM_POOL_Pop(&MEM_local_pool);
    return bb_entry;
  }

#ifdef KEY
  // Set Speculation Level to highest for a loop
  // We can switch it back later.
  EAGER_LEVEL Eager_Level_Temp = Eager_Level;
  Eager_Level = EAGER_MEMORY;
#endif
  HB_Blocks_Set(hb, bbs);
  HB_Entry_Set(hb, bb_entry);
  
  ok_to_convert = HB_Safe_For_If_Conversion(hb);
  if (ok_to_convert) {
    BB_SET * visited = BB_SET_Create_Empty(PU_BB_Count+2, &MEM_local_pool);
    ok_to_convert = Check_for_Cycles(hb, bb_entry, visited);
  }

  if (ok_to_convert) {
    one_bb = Order_And_Classify_Blocks(hb,block_order,block_class);
  }
  
  all_blocks_ok = FALSE;
  if (ok_to_convert && (one_bb || allow_multi_bb)) {
    //
    // Make sure all blocks are acceptable
    //
    all_blocks_ok = TRUE;
    FOR_ALL_BB_SET_members(bbs, bb) {
      all_blocks_ok = all_blocks_ok && Check_BB_For_HB_Suitability(bb, bb_entry);
      if (!all_blocks_ok) break;
    }
  }
  all_blocks_ok = all_blocks_ok && Check_HB_For_PQS_Suitability(bbs, bb_entry);

  if (all_blocks_ok && CG_skip_local_hbf) {
	BOOL skip;
        skip = (BB_id(bb_entry) < CG_local_skip_before ||
                BB_id(bb_entry) > CG_local_skip_after ||
                BB_id(bb_entry) == CG_local_skip_equal);
        if (skip) DevWarn("skip hyperblock loop at BB %d", BB_id(bb_entry));
	else DevWarn("process hyperblock loop at BB %d", BB_id(bb_entry));
	if (skip) all_blocks_ok = FALSE;
  }

  // Do the if-conversion
  if (all_blocks_ok) {
    // Create a dummy candidate region
    HB_CAND_TREE* hct = HB_CAND_TREE_Alloc(&MEM_local_pool);
    HB_CAND_TREE_Candidate_Set(hct, hb);
    candidate_regions.push_front(hct);
    BB_MAP true_edges = BB_MAP_Create();
    BB_MAP control_dependences = BB_MAP_Create();
    BB_MAP predicate_tns = BB_MAP_Create();  // The TN used to predicate a BB
    
    if (HB_Trace(HB_TRACE_CONVERT)) {
      HB_Trace_If_Convert_Blocks(hb);
    }
    Calculate_Control_Dependences(hb, control_dependences, true_edges);
#ifndef TARG_IA64
    multiple_dependencies = FALSE;
    Insert_Predicates(hb, control_dependences, true_edges, predicate_tns);
    if (!multiple_dependencies)
      fall_thru_block = Remove_Branches(
	  hb, predicate_tns, block_order, block_class, candidate_regions);
    else {
      // If hb_formation did not succeed then,no need of hb_sched phase.
      IGLS_Enable_HB_Scheduling = 0;
      one_bb = FALSE;
    }
#else
    Insert_Predicates(hb, control_dependences, true_edges, predicate_tns);
    fall_thru_block = Remove_Branches(hb, predicate_tns, block_order, block_class, candidate_regions);
#endif

    BB_MAP_Delete(true_edges);
    BB_MAP_Delete(control_dependences);
    BB_MAP_Delete(predicate_tns);
    
    // Update the loop descriptor
    added_bbs = BB_SET_Difference(HB_Blocks(hb),LOOP_DESCR_bbset(loop),&MEM_local_pool);
    removed_bbs = BB_SET_Difference(LOOP_DESCR_bbset(loop),HB_Blocks(hb),&MEM_local_pool);

    // get rid of all the old blocks from the loop descriptors
    FOR_ALL_BB_SET_members(removed_bbs,bb) {
      LOOP_DESCR_Delete_BB(loop,bb);
    }
    
    // Add the fall_thru_block to the loop descriptors for all the enclosing loops
    if (fall_thru_block) {
      added_bbs = BB_SET_Difference1D(added_bbs,fall_thru_block);
      LOOP_DESCR *enclosing_loop = LOOP_DESCR_Next_Enclosing_Loop(loop);
      if (enclosing_loop) {
	LOOP_DESCR_Add_BB(enclosing_loop, fall_thru_block);
      }
    }
    // Add all but the fall through to the inner loop
    FOR_ALL_BB_SET_members(added_bbs,bb) {
      if (bb != fall_thru_block) {
	LOOP_DESCR_Add_BB(loop,bb);
      }
    }

    if (one_bb) {
      single_bb = HB_Entry(hb);
    }
    if (Get_Trace(TKIND_IR, TP_HBF, bb_entry)) {
	Trace_IR(TP_HBF, "Force_If_Convert", NULL);
    }
  }    

  MEM_POOL_Pop(&MEM_local_pool);
#ifdef KEY
  // Reset Speculation Level
  Eager_Level = Eager_Level_Temp;
#endif
  return single_bb;
}
