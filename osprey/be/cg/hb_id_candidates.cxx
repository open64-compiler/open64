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


#include <stack>
#include <queue>
#include "defs.h"
#include "config.h"
#include "mempool.h"
#include "tracing.h"
#include "timing.h"
#include "cgir.h"
#include "cg.h"
#include "cgtarget.h"
#include "cg_flags.h"
#include "ttype.h"
#include "bitset.h"
#include "bb.h"
#include "bb_set.h"
#include "freq.h"
#include "whirl2ops.h"
#include "dominate.h"
#include "findloops.h"
#include "cg_loop.h"
#include "gtn_universe.h"
#include "gtn_set.h"

#include "hb.h"
#include "hb_trace.h"
#include "hb_id_candidates.h"

static BB_SET* region_bbs;


/////////////////////////////////////
static void
verify_cand_tree(HB_CAND_TREE* c,FILE *f)
/////////////////////////////////////
//
//  Make sure a candidate tree is valid.
//
/////////////////////////////////////
{
  if (!HB_CAND_TREE_Kids(c).empty()) {
    std::list<HB_CAND_TREE*>::iterator k;
    for (k = HB_CAND_TREE_Kids(c).begin(); k != HB_CAND_TREE_Kids(c).end();
	 k++) {
      if (!BB_SET_ContainsP(HB_Blocks(HB_CAND_TREE_Candidate(c)),
			    HB_Blocks(HB_CAND_TREE_Candidate(*k)))) {
	fprintf(f, "\nBad Tree ");
	BB_SET_Print(HB_Blocks(HB_CAND_TREE_Candidate(c)),f);
	fprintf(f, " -> ");
	BB_SET_Print(HB_Blocks(HB_CAND_TREE_Candidate(*k)),f);
	fprintf(f, "\n");
      }	else {
	verify_cand_tree(*k,f);
      }
    }
  }
}

static void verify_ctree(std::list<HB_CAND_TREE*> cands)
{
  std::list<HB_CAND_TREE*>::iterator c;
  FILE *f=stderr;

  fprintf(f,"Verifying HB candidates...");
  for (c = cands.begin(); c != cands.end(); c++) {
    verify_cand_tree(*c,f);
  }
  fprintf(f,"done\n");
  fflush(f);
}

/////////////////////////////////////
//
//  Struct for using sort on vector of 
//  bb's.
//
/////////////////////////////////////
struct BB_Freq_Compare {
  BOOL operator () (BB* bb1, BB* bb2) {
    return BB_freq(bb1) > BB_freq(bb2);
  }
};

/////////////////////////////////////
BB*
Find_Immediate_Dominator(BB* bb)
/////////////////////////////////////
//
//  Find a predecessor that dominates the given block.  If no such
//  block exists, then return NULL (this, then, is sort of a join
//  point, and thus a good place to stop the growth of the hyperblock).
//  Blocks which cross loop boundaries are not considered, i.e all blocks 
//  should be within the same loop.
//
/////////////////////////////////////
{
  BBLIST* bl;
  BB* dom = NULL;
  FOR_ALL_BB_PREDS(bb, bl) {
    BB* pred = BBLIST_item(bl);
    // Also need the bb != pred test otherwise loops of one BB break things
    if (BB_SET_MemberP(BB_dom_set(bb), pred) && bb != pred &&
	LOOP_DESCR_Find_Loop(bb) == LOOP_DESCR_Find_Loop(pred)) {
      dom = pred;
      break;
    }
  }
  if (dom && BB_call(dom)) {
    //
    // Blocks broken at calls.  Want to find it's dominator.
    //
    return Find_Immediate_Dominator(dom);
  }
  return dom;
}


/////////////////////////////////////
BB*
Find_Immediate_Postdominator(BB* bb)
/////////////////////////////////////
//
//  Find a successor that post-dominates the given block.  If no such
//  block exists, then return NULL (this, then, is sort of a split
//  point, and thus a good place to stop the growth of the hyperblock).
//  Blocks which cross loop boundaries are not considered, i.e all blocks 
//  should be within the same loop.
//
//
/////////////////////////////////////
{
  BBLIST* bl;
  FOR_ALL_BB_SUCCS(bb, bl) {
    BB* succ = BBLIST_item(bl);
    // Also need the bb != pred test otherwise loops of one BB break things
    if (BB_SET_MemberP(BB_pdom_set(bb), succ) && bb != succ &&
	LOOP_DESCR_Find_Loop(bb) == LOOP_DESCR_Find_Loop(succ)) {
      return succ;
    }
  }
  return NULL;
}

/////////////////////////////////////
BOOL
Check_HB_For_PQS_Suitability(BB_SET* selected_hb, BB* bb_entry)
/////////////////////////////////////
//
//  Determine if the hyperblock has any characteristics that doesn't
//  allow PQS to work. HBF relies on PQS to determine accurate predicate
//  relations. Hyperblock is already garaunteed to be a single-entry
//  multiple-exit region, so need to check for live-ins to entry-points
//  other than the single entry. 
//
/////////////////////////////////////
{

  // PQS implementation relies on a unique TN number to handle a 
  // predicate value-def expression. This means, it cannot handle
  // scenarios, where a TN is defined more than once, is live-in coming to
  // the hyperblock, or is live-out. Local predicate TNs should work perfectly 
  // fine. We check for this constraint in the selected region here.

#ifdef TARG_IA64

  BB *bb;
  GTN_SET *pgtn_defs = GTN_SET_Create(GTN_UNIVERSE_size, &MEM_local_pool);
  GTN_SET *pgtn_uses = GTN_SET_Create(GTN_UNIVERSE_size, &MEM_local_pool);
#ifdef linux
  mINT16   *count_pgtns = (mINT16 *) alloca ((Last_TN + 1) * sizeof(mINT16));
  bzero(count_pgtns, (Last_TN+1) * sizeof(mINT16));
#else
  mINT16   count_pgtns[Last_TN + 1];  // predicate GTNs
  bzero(count_pgtns, sizeof(count_pgtns));
#endif /* linux */

  FOR_ALL_BB_SET_members (selected_hb, bb) {
    OP *op;
    FOR_ALL_BB_OPs(bb, op) {
      INT i;

      for (i = 0; i < OP_results(op); ++i) {
	TN *result_tn = OP_result(op, i);
	if (TN_is_global_reg(result_tn) && 
	    TN_register_class(result_tn) == ISA_REGISTER_CLASS_predicate) {

	  INT16 count = count_pgtns[TN_number(result_tn)];
	  count += 1;

	  // more than one definition, return FALSE, else add it to the 
	  // pgtn_defs GTN_SET.
	  if (count > 1) {
	    return FALSE;
	  } else {
	    pgtn_defs = GTN_SET_Union1D(pgtn_defs, result_tn, &MEM_local_pool);
	  } 


	  // Check to see if <result_tn> is live-into any of the side-exit
	  // blocks emanating from the hyperblock.

	  BBLIST *succ_list;
	  FOR_ALL_BB_SUCCS(bb, succ_list) {
	    BB *succ_bb = BBLIST_item(succ_list);
	    if (!BS_MemberP (selected_hb, BB_id(succ_bb)) &&
		GTN_SET_MemberP (BB_live_in(succ_bb), result_tn))
	      return FALSE;
	  }
	}
      }

      for (i = 0; i < OP_opnds(op); ++i) {
	TN *opnd_tn = OP_opnd(op, i);
	if (TN_is_constant(opnd_tn)) continue;

	if (TN_is_global_reg(opnd_tn) &&
	    TN_register_class(opnd_tn) == ISA_REGISTER_CLASS_predicate) {

	  pgtn_uses = GTN_SET_Union1D(pgtn_uses, opnd_tn, &MEM_local_pool);


	  // Check to see if <opnd_tn> is live-out from any of the side-exit
	  // entry blocks to the hyperblock.

	  BBLIST *pred_list;
	  FOR_ALL_BB_PREDS(bb, pred_list) {
	    BB *pred_bb = BBLIST_item(pred_list);
	    if (!BS_MemberP (selected_hb, BB_id(pred_bb)) &&
		GTN_SET_MemberP (BB_live_out(pred_bb), opnd_tn))
	      return FALSE;
	  }
	}
      }
    }
  }

  TN *tn;
  FOR_ALL_GTN_SET_members(pgtn_uses, tn) {
    if (!GTN_SET_MemberP(pgtn_defs, tn)) return FALSE;
  }

#endif

  return TRUE;
}

/////////////////////////////////////
BOOL
Check_BB_For_HB_Suitability(BB* bb, BB* bb_entry)
/////////////////////////////////////
//
//  Determine if the block has any characteristics that make it
//  undesirable or even illegal for if-conversion.
//
/////////////////////////////////////
{
#ifdef KEY
  if (BB_asm(bb)) // we do not want to If-convert BBs with asms.
    return FALSE;
#endif
  //
  // Want simple branches to deal with.
  //
  switch (BB_kind(bb)) {
  case BBKIND_GOTO:
  case BBKIND_LOGIF:
    break;
  case BBKIND_CALL:
    if (!CGTARG_Can_Predicate_Calls() || HB_exclude_calls) {
      return FALSE;
    }
    break;
  case BBKIND_RETURN:
    if (!CGTARG_Can_Predicate_Returns()) {
      return FALSE;
    }
    break;
  default:
    return FALSE;
  }

  //
  // No exception label blocks can be if-converted
  //
  if (BB_Has_Exc_Label(bb)) {
    return FALSE;
  }

  //
  // #792424
  // Blocks which have labels marked as address-taken cannot be if-converted.
  //
  if (BB_Has_Addr_Taken_Label(bb)) {
    return FALSE;
  }

  //
  // Don't cross region boundries
  //
  if (BB_rid(bb) != BB_rid(bb_entry)) {
    return FALSE;
  }

  // Make sure exit blocks post-dominate the entry block (or we might predicate them, and
  // that will confuse GRA
  if (BB_exit(bb) && !BB_SET_MemberP(BB_pdom_set(bb_entry),bb)) {
    return FALSE;
  }

  // 
  // Don't cross loop boundries 
  //
  if (LOOP_DESCR_Find_Loop(bb_entry) != LOOP_DESCR_Find_Loop(bb)) {
    return FALSE;
  }

#ifdef KEY
  if (bb != bb_entry) { // Entry BB will not be predicated and so it is okay
#endif
  //
  // Bail if we find an op that requires guards, i.e. it cannot execute
  // without side effects.  Note that this will have to become more 
  // sophisticated when we need to insert guard ops for these guys.
  //
  OP* op;
  FOR_ALL_BB_OPs_FWD(bb, op) {

    // #794169: Add a tighter constraints during HBF, if the <block> contains
    // OP which don't have qualifying predicate operands, then it may be
    // unsafe to employ predication. Exclude OP_xfer OPs as they
    // get eliminated as a by-product of doing if-conversion, so need to
    // check for those OPs.
    // TODO: Need to verify if any additional conditions need to be relaxed.

    if (!CGTARG_Check_OP_For_HB_Suitability(op)) return FALSE;

    if (OP_has_predicate(op) &&
	!TN_is_true_pred(OP_opnd(op, OP_PREDICATE_OPND))) {

      TN *pred_tn = OP_opnd(op, OP_PREDICATE_OPND);

      if (TN_is_global_reg(pred_tn)) {
      	if (HB_exclude_pgtns) {
	    if (HB_Trace(HB_TRACE_HAMMOCK))
		fprintf(HB_TFile, "not forming hyperblock cause BB %d has global predicate GTN%d\n", BB_id(bb), TN_number(pred_tn));
	    return FALSE;
	}
      }
      else {
	// local pred
	DEF_KIND kind;

	OP *def_op = TN_Reaching_Value_At_Op(pred_tn, op, &kind, TRUE);
	if (def_op && OP_has_predicate(def_op) &&
	    TN_is_true_pred(OP_opnd(def_op, OP_PREDICATE_OPND))) {

          //
          // It's unpredicated, now see if it's unconditional, or, if there
          // is an unconditional form that we can convert to (we should have
          // generated it that way, but we'll convert it here to be robust).
          //

          TOP uncond_op;
          if (!CGTARG_Unconditional_Compare(def_op, &uncond_op)) {
            if (uncond_op != TOP_UNDEFINED) {
              OP_Change_Opcode(def_op, uncond_op);
              Set_OP_cond_def_kind(def_op,OP_ALWAYS_UNC_DEF);
            } else {
              DevWarn("No unconditional form of compare available in BB%d.",BB_id(bb));
              return FALSE;
            }
	  }
	} /* if (def_op ...) */
      } /* if (!TN_is_global_reg ...) */
    } /* if (!TN_is_true_pred ...) */
  } /* FOR_ALL_.. */
#ifdef KEY
  }
#endif

  return TRUE;
}

/////////////////////////////////////
void
HB_Identify_Candidates_Init()
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{

  //
  // Both the dominator calculation and loop detection calls will
  // have to be moved/changed when the BB_REGION interface is added
  // for loop bodies.
  //
  Calculate_Dominators();
  (void) LOOP_DESCR_Detect_Loops(&MEM_HB_pool);
  region_bbs = BB_SET_Create_Empty(PU_BB_Count+2, &MEM_HB_pool);
}

/////////////////////////////////////
HB_CAND_TREE*
Make_New_Region(std::list<HB_CAND_TREE*>&       candidates, 
		BB*                        entry, 
		BB*                        exit,
		BB*                        fall_thru_exit, 
		BB_MAP*                    hct_entry_map, 
		BB_SET*                    blocks,
		MEM_POOL*                  hb_pool)
/////////////////////////////////////
//
//  Create a new candidate region and add it to the end of the list.
//  This structure will always be allocated to MEM_HB_pool.  The pool
//  from which the hyperblock structure should be allocated is passed
//  in.
//
/////////////////////////////////////
{
  HB_CAND_TREE* new_cand = HB_CAND_TREE_Alloc(&MEM_HB_pool);
  HB* new_hb = HB_Alloc(hb_pool);
  HB_CAND_TREE_Candidate_Set(new_cand, new_hb);
  HB_CAND_TREE_Set_Flag(new_cand, HCT_UNPROCESSED);
  HB_Entry_Set(new_hb, entry);
  if (hct_entry_map) {
    BB_MAP_Set(*hct_entry_map, entry, new_cand);
  }
  if (blocks) {
    //
    // Note that this does not set the bb->hb map.  We don't want
    // that as it affects block selection later.
    //
    HB_Add_BB_SET(new_hb, blocks, hb_pool);
    region_bbs = BB_SET_UnionD(region_bbs, blocks, &MEM_HB_pool);
  }

  HB_Exit_Set(new_hb, exit);
  HB_Fall_Thru_Exit_Set(new_hb, fall_thru_exit);
  if (fall_thru_exit) {
    //
    // Will have been added to blocks set, but this isn't
    // actually a member of the hyperblock (it just terminates
    // the paths in it).
    //
    HB_Remove_Block(new_hb, fall_thru_exit);
  }
  candidates.push_front(new_cand);

  return new_cand;
}

/////////////////////////////////////
void
Find_Interior_Blocks(std::list<HB_CAND_TREE*>& candidates)
/////////////////////////////////////
//
//  Find blocks that appear to be good starting places for finding
//  hammocks.  These will be the then or else blocks, and they'll
//  have one exit and won't dominate their successor.
//
/////////////////////////////////////
{
  BB* bb;
  BB* succ;

  if (HB_Trace(HB_TRACE_HAMMOCK)) {
    fprintf(HB_TFile,"Initial hammock blocks:");
  }

  for (bb = REGION_First_BB; bb; bb = BB_next(bb)) {
    succ = BB_Unique_Successor(bb);
    if (succ && !BB_SET_MemberP(BB_dom_set(succ), bb) && 
	Check_BB_For_HB_Suitability(bb, bb)) {
      HB_CAND_TREE* cand;
      cand = Make_New_Region(candidates, bb, bb, NULL, NULL, NULL,
			     &MEM_HB_pool);
      HB_CAND_TREE_Set_Flag(cand, HCT_SINGLE_BLOCK);
      HB_CAND_TREE_Set_Flag(cand, HCT_ERASE);
      if (HB_Trace(HB_TRACE_HAMMOCK)) {
	fprintf(HB_TFile," %d",BB_id(bb));
      }
    }
  }
  if (HB_Trace(HB_TRACE_HAMMOCK)) {
    fprintf(HB_TFile,"\n\n");
  }
}

/////////////////////////////////////
BOOL
Check_Region_Recur(BB*                    dom, 
		   BB*                    current, 
		   BB*                    pdom, 
		   BB_SET*                blocks,
		   std::list<HB_CAND_TREE*>&   kids,
		   HB_CAND_TREE*          hct_parent, 
		   BB_MAP                 hct_entry_map,
		   BOOL*                  bad_region)
/////////////////////////////////////
//
//  Recursively walk down successor lists looking for one not dominated
//  by dom.
//
/////////////////////////////////////
{
  if (*bad_region) return FALSE;

  if (BB_SET_MemberP(blocks, current)) {
      return TRUE;
  }

  //
  // Checks to see if <current> is not part of any internal loop cycle.
  //
  if (BB_loop_head_bb(current) != BB_loop_head_bb(dom) ||
      BB_loop_head_bb(current) != BB_loop_head_bb(pdom)) {
    *bad_region = TRUE;
    return FALSE;  
  }

  blocks = BB_SET_Union1D(blocks, current, &MEM_local_pool);

  if (!Check_BB_For_HB_Suitability(current, dom)) {
    *bad_region = TRUE;
    return FALSE;
  }
  if (current == pdom) {
    return TRUE;
  }
  if (!BB_SET_MemberP(BB_dom_set(current), dom)) {
    return FALSE;
  }
 
  BBLIST* bl;
  FOR_ALL_BB_SUCCS(current, bl) {
    BB* succ = BBLIST_item(bl);
    HB_CAND_TREE* hct = (HB_CAND_TREE*) BB_MAP_Get(hct_entry_map, succ);
    if (hct && hct_parent && HB_CAND_TREE_Parent(hct) == hct_parent) {
      //
      // Any subtree that is this node's immediate descendent will currently
      // have the same parent that it will be given.
      //
      kids.push_front(hct);
    }
    if (!Check_Region_Recur(dom, succ, pdom, blocks, kids, hct_parent,
			    hct_entry_map,
			    bad_region)) {
      return FALSE;
    }
  }
  return TRUE;
}


/////////////////////////////////////
BOOL
Check_Region(BB**                 orig_dom, 
	     BB**                 orig_pdom, 
	     BB_SET*              blocks,
	     std::list<HB_CAND_TREE*>& kids, 
	     HB_CAND_TREE*        hct_parent,
	     BB_MAP               hct_entry_map)
/////////////////////////////////////
//
//  Since cflow has been run, we will be unlikely to find a dominating
//  block that also dominates the post-dominating block.  As a result,
//  we cannot guarantee that all blocks reachable from the dominating
//  block are dominated by it.  So, we must check the region.  If we
//  find a block in the region that is not dominated by the dominating
//  block that we've found, we'll back off and look for a pair of blocks
//  such that the dominating block dominates the post-dominator.  This
//  will make for a larger hammock, but the control flow has to be a
//  little screwy in this area anyway (with well structured code, we
//  wouldn't be in this situation), so this is probably okay.
// 
/////////////////////////////////////
{
  BOOL retval;
  BOOL bad_region = FALSE;
  BOOL found_region = Check_Region_Recur(*orig_dom, *orig_dom, *orig_pdom,
					 blocks, kids, hct_parent,
					 hct_entry_map, &bad_region);

  if (HB_Trace(HB_TRACE_HAMMOCK)) {
    fprintf(HB_TFile,"<HB> Check region BB%d to BB%d = %s found %s\n",
	    BB_id(*orig_dom), BB_id(*orig_pdom),
	    found_region ? "" : "not",
	    bad_region ? "bad" : "");
  }

  if (!found_region && !bad_region) {
    BB* dom = *orig_dom;
    BB* pdom = *orig_pdom;
    do {
      if (!BB_SET_MemberP(BB_dom_set(pdom), dom)) {
	for (dom = Find_Immediate_Dominator(dom);
	     dom && !BB_SET_MemberP(BB_dom_set(pdom), dom);
#ifdef TARG_IA64
	     dom = Find_Immediate_Dominator(dom));
#else
	     dom = Find_Immediate_Dominator(pdom));
#endif
      }
      if (dom && !BB_SET_MemberP(BB_pdom_set(dom), pdom)) {
	for (pdom = Find_Immediate_Postdominator(pdom);
	     pdom && !BB_SET_MemberP(BB_pdom_set(dom), pdom);
	     pdom = Find_Immediate_Postdominator(pdom));
      }
    } while (dom && pdom && !BB_SET_MemberP(BB_dom_set(pdom), dom) &&
	     !BB_SET_MemberP(BB_pdom_set(dom), pdom));

    if (dom && pdom) {
      //
      // Add the blocks to "blocks" bitset.  
      //
      BB_SET_ClearD(blocks);
      kids.clear();
      found_region = Check_Region_Recur(dom, dom, pdom, blocks, kids, hct_parent,
			     hct_entry_map, &bad_region);
      if (HB_Trace(HB_TRACE_HAMMOCK)) {
	fprintf(HB_TFile,"<HB> Check region reprocess BB%d to BB%d = %s found %s\n",
		BB_id(dom), BB_id(pdom),
		found_region ? "" : "not",
		bad_region ? "bad" : "");
      }
      if (!found_region || bad_region) {
	dom = NULL;
	pdom = NULL;
      }
    } else {
      // Can't form a region, give up
      dom = NULL;
      pdom = NULL;
    }

    if (!dom || !pdom) {
      retval = FALSE;
    } else {
      retval = TRUE;
      *orig_dom = dom;
      *orig_pdom = pdom;
    }
  } else {
    retval = !bad_region;
  }

  //
  // Keep number of blocks below maximum.
  //
  return retval && BS_Size(blocks) <= HB_max_blocks;
}

// See if a cand tree is a subchild of a particular parent


static BOOL is_subtree_of(HB_CAND_TREE* parent, HB_CAND_TREE* child)
{
  if (child == parent) return TRUE;
  if (child == NULL || parent == NULL) return FALSE;
  return is_subtree_of(parent,HB_CAND_TREE_Parent(child));
}
  
/////////////////////////////////////
void
Insert_Parent(HB_CAND_TREE*        new_parent, 
	      HB_CAND_TREE*        child,
	      std::list<HB_CAND_TREE*>& candidates,
	      BB_MAP               hct_entry_map)
/////////////////////////////////////
//
//  Insert <new_parent> into tree as <child>'s parent.
//
/////////////////////////////////////
{
  HB_CAND_TREE* old_parent = HB_CAND_TREE_Parent(child);

  if (old_parent) {
    if (is_subtree_of(new_parent,old_parent)) {
      //
      // child already added
      //
      return;
    }
    if (HB_Trace(HB_TRACE_HAMMOCK)) {
      fprintf(HB_TFile,"<HB> Insert Parent (old):\n");
      HB_Trace_Print_Cand_Tree(old_parent,0);
      fprintf(HB_TFile,"---------------\n");
      HB_Trace_Print_Cand_Tree(new_parent,0);
      fprintf(HB_TFile,"---------------\n");
      HB_Trace_Print_Cand_Tree(child,0);
      fprintf(HB_TFile,"===============\n");
    }

    //    if (HB_CAND_TREE_Check_Flag(old_parent,HCT_ERASE)) {
    //      if (HB_Trace(HB_TRACE_HAMMOCK)) {
    // 	      fprintf(HB_TFile,"<HB> not reparenting\n");
    //      }
    //      return;
    //    }

    std::list<HB_CAND_TREE*>::iterator k;
    std::list<HB_CAND_TREE*>::iterator current;
    for (k = HB_CAND_TREE_Kids(old_parent).begin();
	 k != HB_CAND_TREE_Kids(old_parent).end();) {
      current = k++;
      if (*current == child) {
	HB_CAND_TREE_Kids(old_parent).erase(current);
	break;
      }
    }
    //
    // Only insert once ... we may get called multiple times
    // when several children found.
    //
    if (!HB_CAND_TREE_Parent(new_parent)) {
      HB_CAND_TREE_Kids(old_parent).push_front(new_parent);
      HB_CAND_TREE_Parent_Set(new_parent, old_parent);
      HB_CAND_TREE_Set_Flag(new_parent,HCT_ERASE);
    }
  } else {
    if (HB_Trace(HB_TRACE_HAMMOCK)) {
      fprintf(HB_TFile,"<HB> Insert Parent:\n");
      HB_Trace_Print_Cand_Tree(new_parent,0);
      fprintf(HB_TFile,"---------------\n");
      HB_Trace_Print_Cand_Tree(child,0);
      fprintf(HB_TFile,"===============\n");
    }
    //
    // Previously, was root of tree.  Need to remove it from the candidate
    // list.
    //
    std::list<HB_CAND_TREE*>::iterator c;
    std::list<HB_CAND_TREE*>::iterator cur;
    for (c = candidates.begin(); c != candidates.end();) {
      cur = c++;
      if (*cur == child) {
	HB_CAND_TREE_Set_Flag(*cur, HCT_ERASE);
	break;
      }
    }
  }
  HB_CAND_TREE_Parent_Set(child, new_parent);
  HB_CAND_TREE_Kids(new_parent).push_front(child);
}

/////////////////////////////////////
BOOL
Attempt_Merge(HB_CAND_TREE*        new_region, 
	      BB_MAP               hct_entry_map,
	      std::list<HB_CAND_TREE*>& candidates)
/////////////////////////////////////
//
//  Try to merge this region with a neighboring one.
//
/////////////////////////////////////
{
  HB_CAND_TREE* neighbor_region;
  BB* neighbor;
  BB* hb_exit;

  //
  // Can't do the merge if we don't dominate our exit.
  //
  HB* hb = HB_CAND_TREE_Candidate(new_region);
  if (HB_Fall_Thru_Exit(hb)) {
    return FALSE;
  }

  hb_exit = HB_Exit(hb);
  //
  //  See if our exit block is the entry to a hammock
  //  
  neighbor_region = (HB_CAND_TREE*) BB_MAP_Get(hct_entry_map, hb_exit);
  
  if (neighbor_region) {
    neighbor = hb_exit;
  } else {
    //
    //  See if the fall-through successor to our exit block is the entry to a hammock
    //
#ifdef TARG_IA64
    Remove_Explicit_Branch(hb_exit);
#endif
    neighbor = BB_Fall_Thru_Successor(hb_exit);
    if (!neighbor) return FALSE;
    // If we allow this in, we can get some horrendously bad hyperblocks (too many paths)
    //    if (BBlist_Len(BB_succs(hb_exit)) > 1) {
    //  return FALSE;
    //}
    neighbor_region = (HB_CAND_TREE*) BB_MAP_Get(hct_entry_map, neighbor);
  }

  if (!neighbor_region) {
    return FALSE;
  }

  // Check loops are the same
  if (BB_loop_head_bb(neighbor) != BB_loop_head_bb(hb_exit)) {
    return FALSE;
  }


  // 
  // If there is one, we can only merge if we won't exceed the maximum
  // block count, unless the neighbor is our exit, in which case we always
  // merge.
  //
  HB* hb_neighbor = HB_CAND_TREE_Candidate(neighbor_region);

  // Don't try to merge self with self
  if (hb_neighbor == hb) {
    return FALSE;
  }

  if ((hb_exit != neighbor) && (BS_Size(HB_Blocks(hb)) + BS_Size(HB_Blocks(hb_neighbor))) >
      HB_max_blocks) {
    return FALSE;
  }
  
  //
  // Make sure that there are no internal loop cycles between HBs. We
  // have already guaranteed that all BBs within the HB conform to this
  // restriction.
  //
  if (BB_loop_head_bb(HB_Entry(hb)) != BB_loop_head_bb(HB_Entry(hb_neighbor))){
    return FALSE;
  }
  
  //
  // Make a region that is a merge of the two.  It will become the parent
  // region.
  //
  if (HB_Trace(HB_TRACE_HAMMOCK)) {
    fprintf(HB_TFile,"<HB> Merging HBs ");
    BB_SET_Print(HB_Blocks(hb),HB_TFile);
    fprintf(HB_TFile," ");
    BB_SET_Print(HB_Blocks(hb_neighbor),HB_TFile);
    fprintf(HB_TFile,"\n");
  }

  HB_CAND_TREE* parent_region;
  if (HB_Fall_Thru_Exit(hb_neighbor)) {
    // It may be the case that the first hyperblock
    // dominates the fall_through of the second hyperblock. In this case, we 
    // want to get the fall-through exit of the second block in the merged region
    //
    if (BB_SET_MemberP(BB_dom_set(HB_Fall_Thru_Exit(hb_neighbor)),HB_Entry(hb))) {
      parent_region = Make_New_Region(candidates, HB_Entry(hb), HB_Fall_Thru_Exit(hb_neighbor),
				      NULL,
				      &hct_entry_map,
				      BB_SET_Union1D(BB_SET_Union(HB_Blocks(hb),
								  HB_Blocks(hb_neighbor),
								  &MEM_HB_pool),
						     HB_Fall_Thru_Exit(hb_neighbor),
						     &MEM_HB_pool),
				      &MEM_pu_pool);
    } else {
      parent_region = Make_New_Region(candidates, HB_Entry(hb), NULL,
				      HB_Fall_Thru_Exit(hb_neighbor),
				      &hct_entry_map,
				      BB_SET_Union(HB_Blocks(hb),
						   HB_Blocks(hb_neighbor),
						   &MEM_HB_pool),
				      &MEM_pu_pool);
    }
  } else {
    parent_region = Make_New_Region(candidates, HB_Entry(hb),
				    HB_Exit(hb_neighbor), NULL,
				    &hct_entry_map,
				    BB_SET_Union(HB_Blocks(hb),
						 HB_Blocks(hb_neighbor),
						 &MEM_HB_pool),
				    &MEM_pu_pool);
  }

  //
  // This will end up using <new_region>'s parent as the parent 
  // for the region representing the merge.  This may or may not
  // be correct.  I.e. depending upon the order that these things
  // are processed in, <new_region> may be pointing to a "grand parent"
  // while <neighbor_region> is actually pointing at the correct parent.
  // It doesn't matter, though, as we'll fix it when we try to grow
  // a region from <parent_region>.
  //
  Insert_Parent(parent_region, new_region, candidates, hct_entry_map);
  Insert_Parent(parent_region, neighbor_region, candidates, hct_entry_map);
  return (TRUE);
}

/////////////////////////////////////
static void
Add_Children(HB_CAND_TREE*            new_region, 
	     std::list<HB_CAND_TREE*>&     kids,
	     std::list<HB_CAND_TREE*>&     candidates, 
	     BB_MAP                   hct_entry_map)
/////////////////////////////////////
//
//  Make <new_region> parent of list of subtrees, <kids>
//
/////////////////////////////////////
{
  std::list<HB_CAND_TREE*>::iterator k;

  for (k = kids.begin(); k != kids.end(); k++) {
    Insert_Parent(new_region, *k, candidates, hct_entry_map);
  }
}

static void Clean_Up_Candidates(std::list<HB_CAND_TREE*>&   candidates) 
{
  std::list<HB_CAND_TREE*>::iterator cands;
  std::list<HB_CAND_TREE*>::iterator current;
  for (cands = candidates.begin(); cands != candidates.end();) {
    current = cands++;
    if (HB_CAND_TREE_Check_Flag(*current, HCT_ERASE)) {
      candidates.erase(current);
    }
  }
}


/////////////////////////////////////
void
Check_Parent(HB_CAND_TREE*          hct_parent, 
	     HB_CAND_TREE*          hct,
	     std::list<HB_CAND_TREE*>&   candidates, 
	     BB_MAP                 hct_entry_map)
/////////////////////////////////////
//
// If the dominator is already part of a candidate region,
// then this hammock has already been found.  We need to
// check to see if it's our parent, though.  If the parent
// tree was found first (via some other kid), then we will
// not yet have established the link.  Initial single "seed"
// blocks are not considered.
//
/////////////////////////////////////
{
  if (!HB_CAND_TREE_Parent(hct) &&
      !HB_CAND_TREE_Check_Flag(hct, HCT_SINGLE_BLOCK)) {
    Insert_Parent(hct_parent, hct, candidates, hct_entry_map);
  }
}

/////////////////////////////////////
void
HB_Identify_Hammock_Candidates(std::list<HB_CAND_TREE*>& candidates,
			       BB_MAP               hct_entry_map)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  std::list<HB_CAND_TREE*>::iterator cands;

  MEM_POOL_Push(&MEM_local_pool);

  //
  // Find likely starting points for hammocks.
  //
  Find_Interior_Blocks(candidates);

  BOOL done;
  do {
    done = TRUE;
    
    for (cands = candidates.begin(); cands != candidates.end(); cands++) {
      HB_CAND_TREE* hct = *cands;
      HB_CAND_TREE* hct_parent;

      //
      // We push unprocessed candidates on the front of the list, so
      // we stop searching after finding the first processed one.
      //
      if (!HB_CAND_TREE_Check_Flag(hct, HCT_UNPROCESSED)) {
	break;
      }

      HB_CAND_TREE_Reset_Flag(hct, HCT_UNPROCESSED);
      done = FALSE;

      HB* hb = HB_CAND_TREE_Candidate(hct);
      BB* entry = HB_Entry(hb);
      BB* dom;
      BB* pdom;
      
      dom = Find_Immediate_Dominator(entry);
      for (; dom && BB_SET_MemberP(BB_pdom_set(dom), entry);
	   dom = Find_Immediate_Dominator(dom));

      if (!dom) {
	//
	// No immediate dominator.  No hammock can be formed.
	//
	continue;
      }

      if (BB_kind(dom) != BBKIND_LOGIF) {
	//
	// If the dominator isn't a conditional, not interesting.
	//
	continue;
      }
    
      hct_parent = (HB_CAND_TREE*) BB_MAP_Get(hct_entry_map, dom);
      if (hct_parent) {
	Check_Parent(hct_parent, hct, candidates, hct_entry_map);
	continue;
      }      
      
      //
      // If it has a fall thru exit, i.e. it shares a termination
      // block with other hammocks, then that's it's immediate
      // post-dominator.
      //
      if (HB_Fall_Thru_Exit(hb)) {
	pdom = HB_Fall_Thru_Exit(hb);
      } else {
	pdom = Find_Immediate_Postdominator(HB_Exit(hb));
      }
      
      //
      // Find block that post-dominates the dominating block found above.
      //
      for (; pdom && !BB_SET_MemberP(BB_pdom_set(dom), pdom);) {
	pdom = Find_Immediate_Postdominator(pdom);
      }
      // If we have a cycle in the graph, it's actually possible for pdom 
      // to be equal to dom, which can cause bad things to happen becuase we 
      // try to form a single BB hyperblock, which can cause the parenting 
      // stuff to loop
      if (!pdom || pdom==dom) {
	continue;
      }
      
      BB_SET* blocks = BB_SET_Create_Empty(PU_BB_Count+2, &MEM_local_pool);
      std::list <HB_CAND_TREE*> kids;
      if (!Check_Region(&dom, &pdom, blocks, kids,
			HB_CAND_TREE_Parent(hct), hct_entry_map)) {
	continue;
      } else {
	hct_parent = (HB_CAND_TREE*) BB_MAP_Get(hct_entry_map, dom);
	if (hct_parent) {
	  Check_Parent(hct_parent, hct, candidates, hct_entry_map);
	  continue;
	}      
      }

      if ( ! Check_HB_For_PQS_Suitability(blocks, dom)) continue;

#ifdef KEY
      // We want cg_loop to handle all loops.
      BBLIST *succs;
      BB *succ;
      BOOL skip_this_hammock = FALSE;      
      BB *member;
      FOR_ALL_BB_SET_members (blocks, member) {
	FOR_ALL_BB_SUCCS(member, succs) {	
	  succ = BBLIST_item(succs);    
	  if (succ == dom || BB_SET_MemberP(BB_dom_set(dom), succ)) {
	    skip_this_hammock = TRUE;
	    break;
	  }
	}
	if (skip_this_hammock)
	  break;
      }
      if (skip_this_hammock)
	continue;
#endif
      if ( HB_skip_hammocks ) {
	// We have an unfortunate bug where if we form hyperblocks on
	// hammocks, and then later come back with force_if_convert
	// on a loop that contained the hammock, we end up with worse
	// code, compared to if we just force_if_convert the whole loop
	// at once.  In other words, having to re-hbf nested blocks
	// causes a performance problem.  For now, we just turn off
	// the simple hammock hbf, which results in good code for loops
	// but misses some non-loop hammock cases.  We need to redo this,
	// perhaps by knowing whether the hammock is in a loop.
	if (HB_Trace(HB_TRACE_ID)) fprintf(TFile, "stopping hbf on hammock at BB %d\n", BB_id(entry));
	continue;
      }

      //
      // Hammock found!  Must set exit/fall_thru_exit correctly based
      // upon whether the post dominating block is dominated by the
      // entry (i.e. the block that dominates all of the other blocks
      // in the region).
      //
      HB_CAND_TREE* new_region;
      
      if (pdom == dom) continue;
      if (BB_SET_MemberP(BB_dom_set(pdom), dom)) {
	new_region = Make_New_Region(candidates, dom, pdom, NULL,
				     &hct_entry_map, blocks, &MEM_pu_pool);
      } else {
	new_region = Make_New_Region(candidates, dom, NULL, pdom,
				     &hct_entry_map, blocks, &MEM_pu_pool);
      }
      
      if (!HB_CAND_TREE_Check_Flag(hct, HCT_SINGLE_BLOCK)) {
	//
	// The region from which this one has been created may have
	// been "adopted" by the region containing both it and the
	// new one.  That is, another contained region may have formed
	// the outermost one, and it would have found <cand> and made
	// it a child.  The outer one is now <new_region>'s parent.  Need
	// to remove hct from it's list.
	//
	Insert_Parent(new_region, hct, candidates, hct_entry_map);
	
	//
	// Add the kids found in Check_Region()
	//
	Add_Children(new_region, kids, candidates, hct_entry_map);
      }
      
    }
  } while (!done);


  Clean_Up_Candidates(candidates);

  //
  // Now try to merge top-level regions into still larger regions
  //
  BOOL blocks_merged;
  do {
    blocks_merged = FALSE;
    for (cands = candidates.begin(); cands != candidates.end(); ++cands ) {
      if (!HB_CAND_TREE_Check_Flag(*cands, HCT_ERASE)) {
	blocks_merged |= Attempt_Merge(*cands, hct_entry_map, candidates);
      }
    }
  } while (blocks_merged);

  //
  // Now erase all unwanted candidates from list ... too hard to do it
  // in the above loop.
  //
  Clean_Up_Candidates(candidates);

  MEM_POOL_Pop(&MEM_local_pool);
}

/////////////////////////////////////
void
Find_General_Region_Entry_Candidates(std::list<BB*>&  entry_candidates,
				     BB_MAP      hct_entry_map)
/////////////////////////////////////
//
//  Return sorted list of blocks that are potential entry points
//  for general regions.
//
/////////////////////////////////////
{
  BB* bb;

  if (HB_Trace(HB_TRACE_HAMMOCK)) {
    fprintf(HB_TFile,"Initial general blocks:");
  }
  
  for (bb = REGION_First_BB; bb; bb = BB_next(bb)) {
    //
    // On second pass, we'll see hyperblocks.  Don't include them.  Don't
    // start with the entry to another hb candidate either.
    //
    if (BB_MAP_Get(HB_bb_map, bb) || BB_MAP_Get(hct_entry_map, bb)) {
      continue;
    }

    //
    // Don't consider blocks that are already in a candidate region or those
    // whose priority is so low that the hyperblock would never have
    // high enough priority.  Loops are never included in general hyperblocks.
    //
    if (!BB_SET_MemberP(region_bbs, bb) && BB_freq(bb) > HB_minimum_priority
	&& Check_BB_For_HB_Suitability(bb, bb) && !LOOP_DESCR_Find_Loop(bb) 
	&& BB_kind(bb) == BBKIND_LOGIF) {
      entry_candidates.push_front(bb);
      if (HB_Trace(HB_TRACE_HAMMOCK)) {
	fprintf(HB_TFile," %d",BB_id(bb));
      }
    }
  }
  if (HB_Trace(HB_TRACE_HAMMOCK)) {
    fprintf(HB_TFile,"\n\n");
  }
  entry_candidates.sort(BB_Freq_Compare());
}

/////////////////////////////////////
static BB*
Process_Successors(BB*                                              bb,
		   std::priority_queue<BB*,vector<BB*>,BB_Freq_Compare>& bb_best,
		   std::list<BB*>&                                       bb_kids)
/////////////////////////////////////
//
//  Look at bb's successors.  Return the one with the highest probability.
//  If there is more than one, then put the other on a list to be processed
//  later based on flag control.
//
/////////////////////////////////////
{
  int succ_count = 0;
  float max_prob = -1.0;
  BB* max_succ = NULL;
  BB* min_succ = NULL;
  BBLIST* bl;
  

  FOR_ALL_BB_SUCCS(bb, bl) {
    BB* succ = BBLIST_item(bl);
    succ_count++;
    if (BBLIST_prob(bl) > max_prob) {
      if (max_succ) {
	min_succ = max_succ;
      }
      max_prob = BBLIST_prob(bl);
      max_succ = succ;
    } else {
      min_succ = succ;
    }
  }
  if (succ_count > 1) {
    //
    // Choose order in which side paths will be pursued.
    //
    if (HB_general_use_pq) {
      bb_best.push(min_succ);
    } else if (HB_general_from_top) {
      bb_kids.push_back(min_succ);
    } else {
      //
      // default is to grow side paths from the bottom
      // of the main path.
      //
      bb_kids.push_front(min_succ);
    }
  }
  return max_succ;
}

/////////////////////////////////////
static void
Form_General_Region(BB*                     bb, 
		    BB*                     bb_entry, 
		    BB_MAP                  hct_entry_map, 
		    BB_SET*                 blocks,
		    std::list<HB_CAND_TREE*>&    kids, 
		    BOOL                    allow_nesting)
/////////////////////////////////////
//
//  Attempt to form a general candidate region including a supplied
//  bb.  We will follow the highest frequency path until we are
//  stopped.  We will then explore side paths.  See the osprey
//  design web pages for hyperblock formation for more details.
//
/////////////////////////////////////
{
  std::priority_queue<BB*,vector<BB*>,BB_Freq_Compare> bb_best;

  std::list<BB*> bb_kids;
  
  //
  // On second pass, we'll see hyperblocks.  Don't include them.
  //
  if (BB_MAP_Get(HB_bb_map, bb)) {
    return;
  }

  //
  // We don't nest general regions ever.
  //
  if (BB_SET_MemberP(region_bbs, bb_entry)) {
    return;
  }

  //
  // Get out if we're going to attempt a nest and it's not 
  // allowed on this pass.
  //
  if (BB_SET_MemberP(region_bbs, bb) && !allow_nesting) {
    return;
  }

  if (BS_Size(blocks) >= HB_max_blocks) {
    return;
  }

  //
  // Follow the main path from the current block.
  //
  while (bb != NULL) {
    //
    // If we've already added it, then get out.
    //
    if (BB_SET_MemberP(blocks, bb)) {
      break;
    }

    //
    // Must be dominated by the entry for inclusion.
    //
    if (!BB_SET_MemberP(BB_dom_set(bb), bb_entry)) {
      break;
    } 

    //
    // If it's not a suitable for inclusion in the hyperblock, then
    // get out.
    //
    if (!Check_BB_For_HB_Suitability(bb, bb_entry)) {
      break;
    }

    //
    // If this is the entry to another candidate region, then we have to be
    // able to take the region in its entirety.  If we can, then do so,
    // else bug out.  We don't nest general regions within one another
    // at the moment because they don't have exits, and it's messy to
    // cruise through the blocks to find the list of successors to add
    // to the search list.  There's no obvious reason not to do it,
    // though.
    //
    HB_CAND_TREE* hct = (HB_CAND_TREE*) BB_MAP_Get(hct_entry_map, bb);
    if (hct && !HB_CAND_TREE_Check_Flag(hct, HCT_GENERAL)) {
      if (!allow_nesting) {
	break;
      }
      HB* hb = HB_CAND_TREE_Candidate(hct);
      if ((BS_Size(blocks) + BS_Size(HB_Blocks(hb))) > HB_max_blocks) {
	break;
      } 
      blocks = BB_SET_UnionD(blocks, HB_Blocks(hb),&MEM_local_pool);
      kids.push_front(hct);

      //
      // If it has a fall thru exit, then we want to use that as the next
      // block to consider.  If it has a regular exit, then we want to
      // look at it's successors.
      if (HB_Fall_Thru_Exit(hb)) {
	bb = HB_Fall_Thru_Exit(hb);
	continue;
      }
      bb = Process_Successors(HB_Exit(hb), bb_best, bb_kids);
      continue;
    }
  
    //
    // Don't want to wander into any loops, either.
    //
    if (LOOP_DESCR_Find_Loop(bb)) {
      break;
    }

    //
    // Okay, we've passed the test!
    //
    blocks = BB_SET_Union1D(blocks, bb, &MEM_local_pool);
  
    bb = Process_Successors(bb, bb_best, bb_kids);
    if (BS_Size(blocks) >= HB_max_blocks) {
      break;
    }
  }

  //
  // Okay, we've run out of space on the main path.  Start looking at
  // side paths.
  //
  BOOL done = FALSE;
  while (!done) {
    if (HB_general_use_pq) {
      if (bb_best.empty()) {
	done = TRUE;
      } else {
	Form_General_Region(bb_best.top(), bb_entry, hct_entry_map, blocks,
			    kids, allow_nesting);
	bb_best.pop();
      }
    } else {
      if (bb_kids.empty()) {
	done = TRUE;
      } else {
	Form_General_Region(bb_kids.front(), bb_entry, hct_entry_map, blocks,
			    kids, allow_nesting);
	bb_kids.pop_front();
      }
    }
  }
}

/////////////////////////////////////
static void
Add_General_Region_To_Tree(BB*                    bb_entry, 
			   BB_SET*                blocks,
			   std::list<HB_CAND_TREE*>&   kids,
			   BB_MAP                 hct_entry_map,
			   std::list<HB_CAND_TREE*>&   candidates)
/////////////////////////////////////
//
//  Create a new region for the set of blocks supplied, and add it to
//  the tree with <kids> as its subtrees.  Note that if any kids exist,
//  they should be roots of subtrees.
//
/////////////////////////////////////
{
  if (BS_Size(blocks) < HB_min_blocks) {
    return;
  }

  if (!Check_HB_For_PQS_Suitability(blocks, bb_entry)) return;

  HB_CAND_TREE* new_region = Make_New_Region(candidates, bb_entry, NULL, NULL,
					     &hct_entry_map, blocks,
					     &MEM_pu_pool);
  HB_CAND_TREE_Set_Flag(new_region, HCT_GENERAL);
  Add_Children(new_region, kids, candidates, hct_entry_map);
}

/////////////////////////////////////
void
HB_Identify_General_Candidates(std::list<HB_CAND_TREE*>&  candidates,
			       BB_MAP                hct_entry_map,
			       INT                   pass)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  std::list<BB*> entry_candidates;
  std::list<BB*>::iterator bbi;

  MEM_POOL_Push (&MEM_local_pool);

  BB_SET* blocks = BB_SET_Create_Empty(PU_BB_Count+2, &MEM_local_pool);
  BOOL allow_nesting = (pass == 1);

  //
  // Only have other regions to consider when on first pass.  After, they're
  // hyperblocks.
  //
  if (pass != 1) {
    BB_SET_ClearD(region_bbs);
  }

  Find_General_Region_Entry_Candidates(entry_candidates, hct_entry_map);
  for (bbi = entry_candidates.begin(); bbi != entry_candidates.end(); bbi++) {
    std::list<HB_CAND_TREE*> kids;
    BB_SET_ClearD(blocks);
    Form_General_Region(*bbi, *bbi, hct_entry_map, blocks, kids,
			allow_nesting);
    Add_General_Region_To_Tree(*bbi, blocks, kids, hct_entry_map, candidates);
  }

  MEM_POOL_Pop (&MEM_local_pool);
  Clean_Up_Candidates(candidates);
}
