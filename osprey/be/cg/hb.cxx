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


#include <list>
#include "defs.h"
#include "config.h"
#include "tracing.h"
#include "timing.h"
#include "cgir.h"
#include "cg.h"
#include "cgtarget.h"
#include "cg_flags.h"
#include "ttype.h"
#include "bitset.h"
#include "bb_set.h"
#include "freq.h"
#include "whirl2ops.h"
#include "dominate.h"
#include "findloops.h"
#include "gra_live.h"
#include "cxx_memory.h"

#include "hb.h"
#include "hb_trace.h"
#include "hb_id_candidates.h"
#include "hb_block_select.h"
#include "hb_if_convert.h"
#include "hb_tail_duplication.h"

#include "pqs_cg.h"

MEM_POOL MEM_HB_pool;
static MEM_POOL MEM_HB_loop_pool;
std::list<HB *> HB_list;
BB_MAP HB_bb_map;
float HB_minimum_priority;

BOOL HB_did_tail_duplication = FALSE;

/////////////////////////////////////
void
HB_Predecessor_Count(HB* hb, BB_MAP& predecessor_count)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  BB* bb;

  //
  // First, count predecessors of each node in the hyperblock.
  //
  FOR_ALL_BB_SET_members(HB_Blocks(hb), bb) {
    BBLIST* bl;
    FOR_ALL_BB_PREDS(bb, bl) {
      BB* pred = BBLIST_item(bl);
      if (HB_Contains_Block(hb, pred)) {
	INT count = BB_MAP32_Get(predecessor_count, bb);
	BB_MAP32_Set(predecessor_count, bb, ++count);
      }
    }
  }

  //
  // Entry has no predecessors within the hyperblock (latchback branches
  // "go out" of the hyperblock and come back in).  Rather than mess with
  // it in the above loop, just clobber its count.
  //
  BB_MAP32_Set(predecessor_count, HB_Entry(hb), 0);
}


HB* HB_Alloc(MEM_POOL* pool)
/////////////////////////////////////
//  Allocate a new hyperblock
/////////////////////////////////////
{
  HB* hb = CXX_NEW(HB, pool);
  HB_Blocks_Set(hb, BB_SET_Create_Empty(PU_BB_Count+2, pool));
  HB_Flags_Clear(hb);
  HB_Entry_Set(hb, NULL);
  return hb;
}

/////////////////////////////////////
void
HB_Add_BBs_And_Map(HB* hb, BB_SET* bbset)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  BB* bb;

  HB_Add_BB_SET(hb, bbset, &MEM_pu_pool);
  FOR_ALL_BB_SET_members(bbset, bb) {
    BB_MAP_Set(HB_bb_map, bb, (void *) hb);
  }
}

/////////////////////////////////////
void
HB_Copy_BBs_And_Map(HB* hb, BB_SET* bbset)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  BB* bb;

  HB_Blocks_Copy(hb, bbset);
  FOR_ALL_BB_SET_members(bbset, bb) {
    BB_MAP_Set(HB_bb_map, bb, (void *) hb);
  }
}


//////////////////////////////////////////////////////////////////
// Set up the BB_map for a given set of hyperblocks
//////////////////////////////////////////////////////////////////
static void
HB_Map_BBs(HB *hb)
{
  BB *bb;
  FOR_ALL_BB_SET_members(HB_Blocks(hb), bb) {
    if (bb) BB_MAP_Set(HB_bb_map, bb, (void *) hb);
  }
}


//////////////////////////////////////////////////////////////////
// Set up the HB_bb_map given a list of hyperblocks
//////////////////////////////////////////////////////////////////
extern void
Setup_HB_bb_map(void)
{
  std::list<HB*>::iterator hbi;
  BB *bb;

  for (bb = REGION_First_BB; bb; bb = BB_next(bb)) {
    BB_MAP_Set(HB_bb_map,bb,NULL);
  }

  for (hbi = HB_list.begin(); hbi != HB_list.end(); hbi++) {
    HB_Map_BBs(*hbi);
  }
}

////////////////////////////////////////////////////////////////
//
// Create the list of hyperblocks in the PU from what actually is on the maps
//
//


static void
HB_Form_HB_List(void)
{
  BB *bb;
  HB *hb;
  BB *bb_hb;
  LOOP_DESCR *hb_loop;

  // first, clear the hyperblock list
  HB_list.clear();

  // Now set up indicators so that we don't have hyperblocks crossing loop boundaries
  Free_Dominators_Memory();
  Calculate_Dominators();
  (void) LOOP_DESCR_Detect_Loops(&MEM_HB_pool);

  // Add each HB in which a BB appears
  for (bb = REGION_First_BB; bb; bb = BB_next(bb)) {
    hb = (HB *) BB_MAP_Get(HB_bb_map, bb);
    if (hb && !HB_Flags_Check(hb,HB_SEEN_FLAG)) {
      // Check the HB for different loop descriptors
      hb_loop = LOOP_DESCR_Find_Loop(HB_Entry(hb));
      FOR_ALL_BB_SET_members(HB_Blocks(hb),bb_hb) {
	if (hb_loop != LOOP_DESCR_Find_Loop(bb_hb)) {
	  HB_Remove_Block(hb,bb_hb);
	  BB_MAP_Set(HB_bb_map,bb_hb,NULL);
	}
      }
      // We may have deleted everything in this HB, or we may not have an entry block
      // anymore, which means we can't pass it along to the hyperblock scheduler, 
      // since we can no longer safely compute a blocks list.
      if (!BB_SET_EmptyP(HB_Blocks(hb)) && HB_Contains_Block(hb,HB_Entry(hb))) {
	HB_list.push_front(hb);
	HB_Flags_Set(hb,HB_SEEN_FLAG);
      }
    }
  }

  // Now for each HB, initialize its block list in the list form
  //
  std::list<HB *>::iterator hbi;
  std::list<BB *> *bl;
  for (hbi = HB_list.begin(); hbi != HB_list.end(); hbi++) {
    bl = HB_Blocks_List(*hbi);
    bl->clear();
    for (bb = HB_Entry(*hbi); bb != NULL && HB_Contains_Block(*hbi, bb);
	 bb = BB_next(bb)) {
      bl->push_back(bb);
    }
  }  
}
 
/////////////////////////////////////
//  Get the blocks list from the hyperblock adta structure. Do a sanity check as well.
/////////////////////////////////////
void Get_HB_Blocks_List(std::list<BB *> &blocks, HB* hb)
{
  blocks = hb->block_list;
#ifdef Is_True_On
  // Sanity check for repeated blocks
  // This is a really stupid, but and easy, way to check this
  std::list<BB *>::iterator i,j;
  for (i = blocks.begin(); i != blocks.end(); i++) {
    j = i;
    j++;
    for (; j != blocks.end(); j++) {
      if (*i == *j) {
	DevWarn(("Get_HB_Blocks_List: duplicate block found %d\n"),BB_id(*i));
      }
    }
  }
#endif
}

////////////////////////////////////////////////////////////////
//
// Remove the current set of blocks in a hyperblock from any hyperblocks
// which have previously been selected and mapped. This is so that if we nest a hyperblock
// within another, but don't select all the blocks, we get a non-overlapping set of hyperblocks.
//

static void
HB_Remove_BBs_From_Hyperblocks(BB_SET * orig_blocks, BB_SET *current_blocks)
{
  BB *bb;
  HB *hb;
  FOR_ALL_BB_SET_members(orig_blocks,bb) {
    hb = (HB *) BB_MAP_Get(HB_bb_map,bb);
    if (hb) {
      HB_Remove_Blocks(hb,current_blocks);
    }
  }
}

////////////////////////////////////////////////////////////////
//
// Remove blocks which may have been deleted by CFLOW from the active hyperblocks list
//
void
HB_Remove_Deleted_Blocks(void)
{
  BB_SET *live_bbs;
  BB *bb;
  MEM_POOL_Push(&MEM_local_pool);
  
  // Form list of live blocks
  live_bbs = BB_SET_Create_Empty(PU_BB_Count+2,&MEM_local_pool);
  for (bb = REGION_First_BB; bb; bb = BB_next(bb)) {
    live_bbs = BB_SET_Union1D(live_bbs,bb,&MEM_local_pool);
  }
  
  // Remove any blocks which aren't live from the hyperblocks lists
  std::list<HB*>::iterator hbi;
  for (hbi = HB_list.begin(); hbi != HB_list.end(); ) {
    HB_Blocks_Set(*hbi, BB_SET_IntersectionD(HB_Blocks(*hbi), live_bbs ));
    // Remove the hyperblock if it's empty
    if (BB_SET_EmptyP(HB_Blocks(*hbi))) {
      hbi = HB_list.erase(hbi);
    } else {
      hbi++;
    }
  }
  
  MEM_POOL_Pop(&MEM_local_pool);
}



////////////////////////////////////////////////////////////////
//
// Per PU initialization of the hyperblocks pass
//
void
HB_Init(void) 
{
  //
  // Erase list from previous PUs
  //
  HB_list.clear();
  HB_minimum_priority = atof(HB_min_priority);
  HB_bb_map = BB_MAP_Create();
  HB_Trace_Init();
#ifndef TARG_IA64
  // Turn off hyperblock formation if we can't predicate
  if (!CGTARG_Can_Predicate()) {
#else
  if (0) {
#endif
    HB_formation = 0;
  }
}


/////////////////////////////////////
static void
Initialize_Memory()
/////////////////////////////////////
//
//  Initialize memory pools.
//
/////////////////////////////////////
{
  static BOOL did_init = FALSE;

  if ( ! did_init ) {
    MEM_POOL_Initialize(&MEM_HB_pool,"HB pool",FALSE);
    MEM_POOL_Initialize(&MEM_HB_loop_pool,"HB loop pool",FALSE);
    did_init = TRUE;
  }
  MEM_POOL_Push(&MEM_HB_pool);
  MEM_POOL_Push(&MEM_HB_loop_pool);
  MEM_POOL_Push(&MEM_local_pool);
}

/////////////////////////////////////
static void
Finalize_Memory()
/////////////////////////////////////
//
//  Free memory pools.
//
/////////////////////////////////////
{
  Free_Dominators_Memory();
  MEM_POOL_Pop(&MEM_local_pool);
  MEM_POOL_Pop(&MEM_HB_pool);
  MEM_POOL_Pop(&MEM_HB_loop_pool);
}

////////////////////////////////////////////////////////////////
static void
Clear_Visited_Bits(std::list<HB_CAND_TREE*>& candidate_regions)
{
  std::list<HB_CAND_TREE*>::iterator cands;
  for (cands = candidate_regions.begin(); cands != candidate_regions.end();
       cands++) {
    HB_CAND_TREE_Reset_Flag(*cands, HCT_VISITED);
  }
}


/////////////////////////////////////
static void
Update_Tree(HB_CAND_TREE* cand)
/////////////////////////////////////
//
// <cand> has been fully if-converted.  Remove it's kids, if any, and
// fold their blocks into it.
//
/////////////////////////////////////
{
  std::list<HB_CAND_TREE*>::iterator kids;
  std::list<HB_CAND_TREE*>::iterator current_kid;

  HB_CAND_TREE_Set_Flag(cand, HCT_FULLY_CONVERTED);

  // 
  // Get rid of it's children.  They won't be stand alone hyperblocks
  // under any circumstances now.
  //
  for (kids = HB_CAND_TREE_Kids(cand).begin();
       kids != HB_CAND_TREE_Kids(cand).end();) {
    current_kid = kids++;
    HB* hb_cand = HB_CAND_TREE_Candidate(cand);
    HB* hb_kid = HB_CAND_TREE_Candidate(*current_kid);
    HB_Add_BBs_And_Map(hb_cand, HB_Blocks(hb_kid));
    HB_CAND_TREE_Kids(*kids).erase(current_kid);
  }
}

/////////////////////////////////////
static void
Convert_Tree_Leaves(HB_CAND_TREE* cand, BOOL* leaves_converted,
		    std::list<HB_CAND_TREE*>& candidate_regions)
/////////////////////////////////////
//
//  If <cand> is a leaf on a tree, try to fully if-convert it.
//  If it is not, try to if-convert it's children.  After it's
//  children are if-converted, it may become a leaf (i.e. all of
//  it's kids will be fully if-converted).  Note that we don't
//  convert single node trees here.
//
/////////////////////////////////////
{
  std::list<HB_CAND_TREE*>::iterator kids;

  if (HB_CAND_TREE_Check_Flag(cand, HCT_VISITED)) return;

  HB_CAND_TREE_Set_Flag(cand, HCT_VISITED);

  if (HB_Trace(HB_TRACE_CONVERT)) {
    fprintf(HB_TFile, "<HB> Converting tree leaves for Candidate: ");
    HB_Trace_Print_Cand_Tree(cand);
    fprintf(HB_TFile,"\n");
  }

  if (!HB_CAND_TREE_Kids(cand).empty()) {
    BOOL leaf = TRUE;
    for (kids = HB_CAND_TREE_Kids(cand).begin();
	 kids != HB_CAND_TREE_Kids(cand).end();
	 kids++) {
      Convert_Tree_Leaves(*kids, leaves_converted, candidate_regions);
      if (!HB_CAND_TREE_Check_Flag(*kids, HCT_FULLY_CONVERTED)) {
	leaf = FALSE;
      }
    }

    //
    // If all we're doing is simple, profitable if-conversion and we've
    // failed to if-convert all the children, then we need to add them
    // to the list of hyperblocks.
    //
    if (!leaf) {
      return;
    }
  }
    
  //
  // Note that since we will refuse to select the hyperblock unless all blocks
  // are selected, we don't need to deal with removing blocks from other
  // previously made hyperblocks (unlike the case below).
  //

  HB* hb = HB_CAND_TREE_Candidate(cand);
  if (HB_Safe_For_If_Conversion(hb) && HB_Block_Select(hb, TRUE)) {
    HB_bb_list no_tail_dups;
    HB_If_Convert(hb, candidate_regions);
    HB_Map_BBs(hb);
    Update_Tree(cand);
    *leaves_converted = TRUE;
  }
}

      
/////////////////////////////////////
void
Convert_Candidate_Leaves(std::list<HB_CAND_TREE*>& candidate_regions,
			 BOOL*                leaves_converted)
/////////////////////////////////////
//  
//  Fully if-convert the leaves of each of the candidate region trees so that
//  we will reduce the number of arcs that the path finding algorithm must
//  traverse.
//
/////////////////////////////////////
{
  std::list<HB_CAND_TREE*>::iterator hbct;

  for (hbct = candidate_regions.begin(); hbct != candidate_regions.end();
       hbct++) {
    Convert_Tree_Leaves(*hbct, leaves_converted, candidate_regions);
  }
}

#ifdef KEY
BOOL hammock_region;
#endif
/////////////////////////////////////
void
Form_Hyperblocks(HB_CAND_TREE*        cand, 
		 BB_MAP               duplicate,
		 BOOL                 post_tail_duplication,
		 std::list<HB_CAND_TREE*>& candidate_regions,
		 BB_SET *orig_blocks)
/////////////////////////////////////
//
//  Top down walk of a candidate region tree, forming hyperblocks on the
//  way down.  Will attempt to form a hyperblock if the subtree under
//  consideration has not been made into a hyperblock already.
//
/////////////////////////////////////
{
  HB* hb = HB_CAND_TREE_Candidate(cand);
  HB* orig_hb;
  BB* entry = HB_Entry(hb);

  if (HB_CAND_TREE_Check_Flag(cand, HCT_VISITED)) return;

  HB_CAND_TREE_Set_Flag(cand, HCT_VISITED);

  orig_hb = (HB *) BB_MAP_Get(HB_bb_map, entry); 
  if (!orig_hb || (!BB_SET_EqualP(HB_Blocks(hb),HB_Blocks(orig_hb)) &&
		   BB_SET_ContainsP(HB_Blocks(hb),HB_Blocks(orig_hb)))) {
    //
    // Form hyperblock for candidate region
    // If orig_hb is NULL, this is the first time we've tried to form a hyperblock 
    // containing this entry. If it's not NULL, we only want to try to convert HB's
    // which are a superset of the original block
    //
    if (HB_Trace(HB_TRACE_CONVERT)) {
      if (!orig_hb) {
	fprintf(HB_TFile, "<HB> Forming hyperblock for candidate: ");
      } else {
	fprintf(HB_TFile, "<HB> Forming merge hyperblock for candidate: ");
      }
      hb->Print();
      fprintf(HB_TFile,"\n");
    }

    orig_blocks = BB_SET_CopyD(orig_blocks,HB_Blocks(hb), &MEM_local_pool);
#ifdef KEY
    hammock_region = FALSE;
#endif
    if (HB_Block_Select(hb, FALSE)) {
      HB_bb_list duplicate_bbs;
      if (HB_Tail_Duplicate(hb, duplicate, duplicate_bbs,
			    post_tail_duplication)) {
	HB_If_Convert(hb, candidate_regions);
	HB_Remove_BBs_From_Hyperblocks(orig_blocks,HB_Blocks(hb));
	HB_Map_BBs(hb);
      } 
    }
  } 

  //
  // See if we left some of it's kids out.
  //
  std::list<HB_CAND_TREE*>::iterator kids;

  for (kids = HB_CAND_TREE_Kids(cand).begin();
       kids != HB_CAND_TREE_Kids(cand).end();
       kids++) {
    Form_Hyperblocks(*kids, duplicate, post_tail_duplication, candidate_regions,orig_blocks);
  }
}


/////////////////////////////////////
void
HB_Form_Hyperblocks(RID *rid, const BB_REGION* bb_region)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  std::list<HB_CAND_TREE*> candidate_regions; // List of all the candidates
  BB_SET *orig_blocks; // Working bitset temp

  if (!HB_formation || !(HB_simple_ifc || HB_complex_non_loop)) {
    return;
  }

  //
  // If we run complex-non-loop, let's not run simple_ifc unless we are forced to
  // by the user
  //
  if (HB_complex_non_loop && !HB_simple_ifc_set) {
    HB_simple_ifc = FALSE;
  }

  Start_Timer (T_HBF_CU);

  HB_did_tail_duplication = FALSE;

  //
  // Can't form hyperblocks without aliasing
  // information
  //
  if (!Alias_Manager && HB_require_alias) {
    return;
  }

  Set_Error_Phase ("Hyperblock Formation");

  Initialize_Memory();
  HB_Identify_Candidates_Init();

  if (HB_Trace(HB_TRACE_BEFORE)) {
    Trace_IR(TP_HBF, "Before Hyperblock Formation", NULL);
  }

  if (HB_Trace(HB_TRACE_DRAWFLOW1)) {
    draw_flow_graph();
  }

  //
  // Identify hammock candidate regions first, then find any generalized
  // control flow that would make a good hyperblock.
  //
  Setup_HB_bb_map();

  BB_MAP hct_entry_map = BB_MAP_Create();  
  
  HB_Identify_Hammock_Candidates(candidate_regions, hct_entry_map);

  if (HB_Trace(HB_TRACE_ID)) {
    HB_Trace_Candidates("Hammock Pass", candidate_regions);
  }
  
  if (HB_complex_non_loop) {
    HB_Identify_General_Candidates(candidate_regions, hct_entry_map, 1);
  }
  BB_MAP_Delete(hct_entry_map);

  if (HB_Trace(HB_TRACE_ID)) {
    HB_Trace_Candidates("First General Pass", candidate_regions);
  }

  //
  // Now, try to fully if-convert from the bottom of the candidate tree.
  //
  BOOL leaves_converted = FALSE;
  if (HB_simple_ifc) {
    Convert_Candidate_Leaves(candidate_regions, &leaves_converted);
    if (HB_Trace(HB_TRACE_DRAWFLOW2)) {
      draw_flow_graph();
    }
  }

  if (leaves_converted && Get_Trace(TKIND_IR, TP_HBF, REGION_First_BB)) {
    Trace_IR(TP_HBF, "After Initial If-conversion", NULL);
  }

  if (HB_complex_non_loop) {
    BB_MAP duplicate = BB_MAP_Create();
    //
    //  Form hyperblocks from the candidate regions.
    //
    std::list<HB_CAND_TREE*>::iterator cands;
    
    Clear_Visited_Bits(candidate_regions);
    orig_blocks = BB_SET_Create_Empty(PU_BB_Count+2,&MEM_local_pool);
    for (cands = candidate_regions.begin(); cands != candidate_regions.end();
	 cands++) {
      //
      // Skip candidates that were fully converted above.
      //
      if (!HB_CAND_TREE_Check_Flag(*cands, HCT_FULLY_CONVERTED)) {
	Form_Hyperblocks(*cands, duplicate, FALSE, candidate_regions,orig_blocks);
      }
    }

    //
    // Now, we want to find any opportunities for hyperblocks that may have
    // been created by tail duplication.  Create a new empty entry map, erase
    // candidate list, and form general regions again.
    //
    hct_entry_map = BB_MAP_Create();
    candidate_regions.clear();

    //
    // Recalculate dominators for tail duplicated blocks.
    //
    if (HB_did_tail_duplication) {
      Free_Dominators_Memory();
      Calculate_Dominators();
      HB_Identify_General_Candidates(candidate_regions, hct_entry_map, 2);
      if (HB_Trace(HB_TRACE_ID)) {
	HB_Trace_Candidates("Second General pass", candidate_regions);
      }
      for (cands = candidate_regions.begin(); cands != candidate_regions.end();
	   cands++) {
	Form_Hyperblocks(*cands, duplicate, TRUE, candidate_regions, orig_blocks);
      }
    } else if (HB_Trace(HB_TRACE_ID)) {
      fprintf(HB_TFile,
	      "<HB> No second pass.  No tail duplication performed.\n");
    }
    BB_MAP_Delete(hct_entry_map);
    BB_MAP_Delete(duplicate);
  }

  HB_Form_HB_List();
  if (HB_Trace(HB_TRACE_SELECT)) {
    HB_Trace_HB_List();
  }
    
  GRA_LIVE_Recalc_Liveness(rid);

  if (Get_Trace(TKIND_IR, TP_HBF, REGION_First_BB)) {
    Trace_IR(TP_HBF, "Hyperblock Formation", NULL);
  }
  if (HB_Trace(HB_TRACE_DRAWFLOW3)) {
      draw_flow_graph();
  }

  Finalize_Memory();
  Stop_Timer (T_HBF_CU);
}
