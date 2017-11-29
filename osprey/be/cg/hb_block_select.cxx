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


#include <math.h>

#include "defs.h"
#include "config.h"
#include "mempool.h"
#include "tracing.h"
#include "timing.h"
#include "cgir.h"
#include "cg.h"
#include "cg_flags.h"
#include "cgtarget.h"
#include "ttype.h"
#include "bitset.h"
#include "bb_set.h"
#include "freq.h"
#include "whirl2ops.h"
#include "dominate.h"
#include "findloops.h"
#include "cg_sched_est.h"
#include "opt_points_to.h"
#include "opt_alias_mgr.h"

#include "hb.h"
#include "hb_path.h"
#include "hb_trace.h"

static float call_hazard_multiplier;
static float memory_hazard_multiplier;
static float min_path_priority_ratio;
static float base_probability_contribution;
static float max_sched_growth;
static float max_sched_growth_sf;

#define HB_MAX_PATHS 256

// =======================================================================
// Verify_HB
//
// Recursively visit all the sucessors of blocks present in HB <hb_bbs>
// and make sure that there are no internal loop cycles present.
// <processed_bbs> is the set of BBs which have been processed so far,
// and <hb_bbs> is the container set of BBs in the hyperblock.
// The key is that successors are not visited until all the predecessors
// have been visited.  This won't happen if the list contains a loop
// because we will never get to the block that branches back.
// If the processed_bbs set is different from the hb_bbs set, it means
// that we could not visit all the blocks and the list contains a loop.
//
// =======================================================================
static void
Verify_HB(
  BB *bb,
  BB_SET **processed_bbs,
  BB_SET *hb_bbs)
{
  BBLIST *edge;
  BB *last_succ = NULL;
  FOR_ALL_BB_SUCCS(bb, edge) {
    BB *succ = BBLIST_item(edge);
    if (BB_SET_MemberP(hb_bbs, succ)) {
      last_succ = succ;
    }
  }
  if (last_succ == NULL) {
    *processed_bbs = BS_Union1D(*processed_bbs, BB_id(bb), &MEM_HB_pool);
    return;
  }

  FOR_ALL_BB_SUCCS(bb, edge) {
    BB *succ = BBLIST_item(edge);
    BOOL skip_succ = FALSE;
    if (BB_SET_MemberP(hb_bbs, succ)) {
      if (succ == last_succ) {
       // Set the "seen" flag for the original block so that the recursive
       // call will continue processing successors of this successor.
        *processed_bbs = BS_Union1D(*processed_bbs, BB_id(bb), &MEM_HB_pool);
      }

     // If we have not already seen all the predecessors of this block
     // (that are in the HB region), don't process it yet.
      BBLIST *pbl;
      FOR_ALL_BB_PREDS(succ, pbl) {
        BB *pred = BBLIST_item(pbl);
        if (BB_SET_MemberP(hb_bbs, pred) &&
            (!((bb == pred)) &&
             !BB_SET_MemberP(*processed_bbs, pred))) {
          skip_succ = TRUE;
          break;
        }
      }

      if (!skip_succ) Verify_HB(succ, processed_bbs, hb_bbs);
    }
  }

}

/////////////////////////////////////
static BOOL
Enumerate_Paths(HB* hb, BB* bb, float probability, std::list<HB_PATH*>& ret_list,
		BB *stopping_point, BB_SET *visited,
		MEM_POOL* pool)
  /////////////////////////////////////
  //
  //  Enumerate all of the paths in a hyperblock and return the
  //  list of them.  Note that this algorithm is highly sensitive
  //  to the number of nodes in the hyperblock.
  //
  //  stopping_point - enumerate the paths up to and including this BB. 
  //  If this is NULL, enumerate everything in the hyperblock
  //
  //  visited - set indicating what blocks we've visited so far. If we hit 
  //  ourself again,
  //  we have detected a loop, and we need to reject the hyperblock. This can 
  //  occur in cases
  //  of nasty flow-of-control, where one can have a side-entry into a loop. 
  //  In this case, 
  //  findloops does not see this as a loop. 
  //
  // returns TRUE if the hyperblock is acceptable (i.e., no loops)
  //
  /////////////////////////////////////
{
  BB* succ;
  INT exit_count = 0;
  INT num_succ = 0;
  BBLIST* bl;

  if (BB_SET_MemberP(visited, bb)) {
    // Loop detected
    return FALSE;
  }
  visited = BB_SET_Union1D(visited, bb, pool);

  if (bb != stopping_point) {
    FOR_ALL_BB_SUCCS(bb, bl) {
      succ = BBLIST_item(bl);
      //
      // Get the list of paths to exits for each successor and splice them 
      // together.  Don't traverse exit edges and loop backedges.  Include
      // the fall thru exit on all paths.
      //
      if (succ != HB_Entry(hb) && HB_Contains_Block(hb, succ)) {
	std::list<HB_PATH*> kids_paths;
	if (Enumerate_Paths(hb, succ, BBLIST_prob(bl), kids_paths, stopping_point, visited, pool)) {
	  ret_list.splice(ret_list.end(), kids_paths);
	  // Keep things from getting to complicated
	  if (ret_list.size() > HB_MAX_PATHS) {
	    return FALSE;
	  }
	} else {
	  // Quick exit, loop found
	  return FALSE;
	}
      } else {
	exit_count++;
      }
      num_succ++;
    }
  }

  // "Unvisit" the block
  visited = BB_SET_Difference1D(visited,bb);

  //
  // This block terminates a path, so must create a path structure.
  //
  if (exit_count > 0 || num_succ == 0) {
    HB_PATH* path = HB_PATH_Alloc(&MEM_local_pool);
    ret_list.push_front(path);
  }

  //
  // Add the current block to all of the paths that were returned
  // from its children and for a path that terminates here, then return
  // that list to its parent.
  //
  for(std::list<HB_PATH*>::iterator paths = ret_list.begin();
      paths != ret_list.end();
      paths++) {
    HB_PATH_Add_Block(*paths, bb, &MEM_local_pool);
    HB_PATH_Probability_Set(*paths,HB_PATH_Probability(*paths) * probability);
  }

  return TRUE;
}

/////////////////////////////////////
static void
Calculate_Path_Data(HB *hb, HB_PATH* path, BB_MAP bb_sched_est,
		    BB_MAP bb_mem_hazard, INT& max_sched_height,
		    INT* max_num_ops)
/////////////////////////////////////
//
//  Find hazards and various counts for the paths.  Keep track of 
//  useful maximums.  Returns FALSE if some path precludes hyperblock
//  formation.
//
/////////////////////////////////////
{
  BB* bb;
  INT num_ops = 0;
  INT num_cb_ops = 0;  // conditional branch ops
  INT num_ucb_ops = 0; // unconditional branch ops

  BB_SET * entry_pdom_set = BB_pdom_set(HB_Entry(hb));
  //
  // Find hazards on the path and calculate schedule height.  We always
  // use the hazard multiplier that will most greatly reduce the priority
  // of the path.
  //
  // Note: I think that we should not insert the information for blocks
  // which are control-equivalent to the entry block. Since the entry block
  // dominates everything, we will ignore blocks which are
  // control-equiavelent to the entry block, which means that they are on
  // the entry blocks post-dominator list.
  // 
  FOR_ALL_BB_SET_members(HB_PATH_Blocks(path), bb) {
    INT mem_hazard = BB_MAP32_Get(bb_mem_hazard, bb);

    if (!mem_hazard) {
      //
      // Walk the ops to find the memory hazards.  A 1 in the map
      // indicates the block has been checked, but there are no
      // hazards.  -1 indicates hazards.  0 indicates that the block
      // has not been checked.  If the alias manager is not present,
      // we are conservative in our assumptions about stores.
      //
      mem_hazard = 1;
      for (OP* op = BB_first_op(bb); op != NULL; op = OP_next(op)) {
	if (OP_store(op)) {
	  if (Alias_Manager) {
	    WN* wn = Get_WN_From_Memory_OP(op);
	    if (wn && Valid_alias(Alias_Manager, wn) &&
		Points_to(Alias_Manager, wn)->Base_kind() == BASE_IS_UNKNOWN) {
	      mem_hazard = -1;
	    }
	  } else {
	    mem_hazard = -1;
	  }
	}
	if (OP_cond(op)) num_cb_ops++;
	else if (OP_uncond(op)) num_ucb_ops++;
      }
      
      BB_MAP32_Set(bb_mem_hazard, bb, mem_hazard);
    }

    num_ops += BB_length(bb);
    
    if (!BB_SET_MemberP(entry_pdom_set,bb)) {
      if (BB_call(bb) &&
	  call_hazard_multiplier < HB_PATH_Hazard_Multiplier(path)) {
	HB_PATH_Hazard_Multiplier_Set(path, call_hazard_multiplier);
      }
      if ((mem_hazard < 0) &&
	  (memory_hazard_multiplier < HB_PATH_Hazard_Multiplier(path))) {
	HB_PATH_Hazard_Multiplier_Set(path, memory_hazard_multiplier);
      }
    }
  }

  CG_SCHED_EST* est = CG_SCHED_EST_Path_Create(HB_PATH_Blocks(path),
					       bb_sched_est,
					       &MEM_local_pool,
					       SCHED_EST_FOR_HB);
  HB_PATH_Sched_Est_Set(path, est);

  if (num_ops > *max_num_ops) {
    *max_num_ops = num_ops;
  }
  if (HB_PATH_Schedule_Height(path) > max_sched_height) {
    max_sched_height = HB_PATH_Schedule_Height(path);
  }

  HB_PATH_Num_Ops_Set(path, num_ops);
  HB_PATH_Num_CB_Ops_Set(path, num_cb_ops);
  HB_PATH_Num_UCB_Ops_Set(path, num_ucb_ops);

}

/////////////////////////////////////
static void
Calculate_Path_Priorities(std::list<HB_PATH*>& hb_paths, INT max_sched_height,
			  INT* max_num_ops, BOOL profitable_ifc)
/////////////////////////////////////
//
//  Calculate path priorities as per Mahlke's algorithm.
//
/////////////////////////////////////
{
  std::list<HB_PATH*>::iterator hbp;
  float max_priority = 0.0;
  INT max_priority_sched_height = 0;
  double branch_cost, factor;
  INT32 branchfixed, mispredict, branchtaken;

  CGTARG_Compute_Branch_Parameters(&mispredict, &branchfixed, &branchtaken, &factor);

  for (hbp = hb_paths.begin(); hbp != hb_paths.end(); hbp++) {
    HB_PATH* path = *hbp;
    float sched_ratio;
    float op_ratio;
    float priority;

    // Adds branch misprediction costs to sched_heights. Need more pruning.
    branch_cost = HB_PATH_Probability(path) * 
      ((HB_PATH_Num_CBranch_Ops(path) * mispredict)
       + ((HB_PATH_Num_CBranch_Ops(path) + HB_PATH_Num_UCBranch_Ops(path)) *
	  branchfixed));

    float cur_sched_height = (float) HB_PATH_Schedule_Height(path);
    cur_sched_height = cur_sched_height - branch_cost;
    if (cur_sched_height < 0) cur_sched_height = 0;

    sched_ratio = 1.0 -
      (cur_sched_height / (float) max_sched_height);
    op_ratio = 1.0 - ((float) HB_PATH_Num_Ops(path) / (float) *max_num_ops);
    priority = (HB_PATH_Probability(path) * HB_PATH_Hazard_Multiplier(path)) *
      (sched_ratio + op_ratio + base_probability_contribution);
    HB_PATH_Priority_Set(path, priority);
    if (priority > max_priority) {
      max_priority_sched_height = HB_PATH_Schedule_Height(path);
    }
  }
   
  //
  // If there is no feedback data and we're altering our heuristics
  // for static profiling data, then we'll try to adjust priorities
  // such that we decrease the priority of any path that has a schedule 
  // height different from that of the maximum priority path.  We also
  // do the same for the initial pass of clearly profitable full if-conversion.
  //
  if (max_priority > 0.0 &&
      ((HB_static_freq_heuristics && !CG_PU_Has_Feedback) || profitable_ifc)) {
    for (hbp = hb_paths.begin(); hbp != hb_paths.end(); hbp++) {
      HB_PATH* path = *hbp;
      if (HB_PATH_Schedule_Height(path) != max_priority_sched_height) {
	float sched_ratio = (float) HB_PATH_Schedule_Height(path) /
	  (float) max_priority_sched_height;
	float priority_multiplier = 1.0 - fabsf(1.0 - sched_ratio);
      
	float new_priority = HB_PATH_Priority(path) * priority_multiplier;
	HB_PATH_Priority_Set(path, new_priority);
      }
    }
  }

  //
  // Sort the paths in priority order.
  //
  hb_paths.sort(HB_PATH_Priority_Compare());
}

/////////////////////////////////////
static INT
Maximum_Sched_Height_Increase(HB_PATH* max_path, BOOL tolerate_increase)
/////////////////////////////////////
//
// Given the maximum priority path, calculate the largest
// increase in schedule height allowed relative to it.
//
/////////////////////////////////////
{
  INT sched_height = HB_PATH_Schedule_Height(max_path);
  if (tolerate_increase) {
    float f_sched_height = (float)sched_height;
    if (HB_static_freq_heuristics && !CG_PU_Has_Feedback) {
      return (INT)(f_sched_height * max_sched_growth_sf);
    }
    return (INT)(f_sched_height * max_sched_growth);
  }
  return sched_height;
}

/////////////////////////////////////
static BOOL
Path_Resources_Available(HB* hb, HB_PATH* path, HB_PATH* max_path,
			 CG_SCHED_EST* hb_est, BOOL tolerate_increase)
/////////////////////////////////////
//
//  Determine if the resources required by the path being
//  considered for inclusion in the hyperblock would cause
//  an unacceptable lengthening of the schedule.
//
/////////////////////////////////////
{
  CG_SCHED_EST* tmp_est;
  INT sum_cycles;

  MEM_POOL_Push(&MEM_local_pool);

  tmp_est = CG_SCHED_EST_Clone(hb_est, &MEM_local_pool);
  CG_SCHED_EST_Append_Scheds(tmp_est, HB_PATH_Sched_Est(path));
  sum_cycles = CG_SCHED_EST_Resource_Cycles(tmp_est);
  CG_SCHED_EST_Delete(tmp_est);

  MEM_POOL_Pop(&MEM_local_pool);			     

  if (sum_cycles > Maximum_Sched_Height_Increase(max_path,
						 tolerate_increase)) {
    return FALSE;
  }
  return TRUE;
}


/////////////////////////////////////
static BB_SET *
Select_Blocks(HB* hb, std::list<HB_PATH*>& hb_paths, BOOL profitable_ifc)
/////////////////////////////////////
//
//  Select paths for inclusing in the hyperblock via the 
//  algorithm outlined in the interface description for
//  this module and Scott Mahlke's thesis.
//
/////////////////////////////////////
{
  std::list<HB_PATH*>::iterator path;
  float last_priority;
  BB_MAP bb_sched_est = BB_MAP_Create();
  BB_MAP bb_mem_hazard = BB_MAP32_Create();
  INT max_sched_height = 0;
  INT max_num_ops = 0;
  HB_PATH* max_path;
  BB_SET* selected_blocks;

  CG_SCHED_EST* hb_est = CG_SCHED_EST_Create_Empty(&MEM_local_pool,
						   SCHED_EST_FOR_HB);
  //
  // We're only looking at resource counts 
  //
  CG_SCHED_EST_use_locs = FALSE;

  //
  // Find priorities, hazards, instruction counts, etc ... for each path.
  // Keep track of any maximum values needed for these items.
  //
  for (path = hb_paths.begin(); path != hb_paths.end(); path++) {
    Calculate_Path_Data(hb, *path, bb_sched_est, bb_mem_hazard, 
			max_sched_height, &max_num_ops);
  }
  
  //
  // Calculate priorities for paths.  Paths will be returned in sorted order
  // from lowest to highest priority.
  //
  Calculate_Path_Priorities(hb_paths, max_sched_height, &max_num_ops,
			    profitable_ifc);

  //
  // Initializations for selection loop.  The hyperblock initially
  // contains the maximum priority path.
  //
  path = hb_paths.begin();
  max_path = *path;
  selected_blocks = NULL;

  if (HB_PATH_Priority(max_path) < HB_minimum_priority) {
    if (HB_Trace(HB_TRACE_SELECT)) {
      fprintf(HB_TFile,
	      "<HB> Hyperblock discarded.  Max path priority too low:\n");
      HB_PATH_Trace(max_path);
    }
  } else if (HB_PATH_Num_Ops(max_path) > 2 * CG_maxinss) {
    if (HB_Trace(HB_TRACE_SELECT)) {
      fprintf(HB_TFile,
	      "<HB> Hyperblock discarded.  No. of instructions in hyperblock (%d) exceed 2 * CG_maxinss (%d) threshold :\n", HB_PATH_Num_Ops(max_path), 2 * CG_maxinss );
      HB_PATH_Trace(max_path);
    }
  }
  else {
    selected_blocks = BB_SET_Copy(HB_PATH_Blocks(max_path),&MEM_local_pool);
    last_priority = HB_PATH_Priority(max_path);
    if (HB_Trace(HB_TRACE_SELECT)) {
      HB_PATH_Trace(*path);
      fprintf(HB_TFile, "<HB>   Maximum priority path.\n\n");
    }

    BOOL tolerate_increase = TRUE;
    for (++path; path != hb_paths.end(); path++) {
      if (HB_Trace(HB_TRACE_SELECT)) {
	HB_PATH_Trace(*path);
      }

      //
      // If we've had a large drop in the relative priorities of the paths,
      // stop looking.
      //
      if (HB_PATH_Priority(*path) < (last_priority * min_path_priority_ratio)){
	if (HB_Trace(HB_TRACE_SELECT)) {
	  fprintf(HB_TFile,
		  "<HB>   Selection terminated.  Priority too low.\n\n");
	}
	tolerate_increase = FALSE;
      }

      //
      // Skip path if increase in schedule height is too great over that
      // of the maximum priority path.
      //
      if (HB_PATH_Schedule_Height(*path) >
	  Maximum_Sched_Height_Increase(max_path, tolerate_increase)) {
	if (HB_Trace(HB_TRACE_SELECT)) {
	  fprintf(HB_TFile,
		  "<HB>   Not Selected.  Schedule height too large.\n\n");
	}
	continue;
      }

      if (!Path_Resources_Available(hb, *path, max_path, hb_est,
				    tolerate_increase)) {
	if (HB_Trace(HB_TRACE_SELECT)) {
	  fprintf(HB_TFile,"<HB>   Not Selected.  Resources Unavailable\n\n");
	}
	continue;
      }

      //
      // Add it to the hyperblock.
      //
      selected_blocks = 
	BB_SET_UnionD(selected_blocks, HB_PATH_Blocks(*path), &MEM_local_pool);
      last_priority = HB_PATH_Priority(*path);
      CG_SCHED_EST_Append_Scheds(hb_est, HB_PATH_Sched_Est(*path));
      if (HB_Trace(HB_TRACE_SELECT)) {
	fprintf(HB_TFile,"<HB>   Selected");
	BB_SET_Print(selected_blocks,HB_TFile);
	fprintf(HB_TFile,"\n\n");
      }
    }

    //
    // For simple if-conversion, must have included all of the blocks
    // to continue.
    //
    if (profitable_ifc && 
	(!selected_blocks || !BB_SET_EqualP(HB_Blocks(hb), selected_blocks))) {
      if (HB_Trace(HB_TRACE_SELECT)) {
	fprintf(HB_TFile, "<HB> Hyperblock discarded.  All blocks in hammock not selected.\n");
      }
      selected_blocks = NULL;
    } else if (BS_Size(selected_blocks) < 2) {
      //
      // If no real if-conversion happened, remove this hyperblock from
      // the list.
      //
      if (HB_Trace(HB_TRACE_SELECT)) {
	fprintf(HB_TFile,
		"<HB> Hyperblock discarded.  Too few blocks included.\n");
      }
      selected_blocks = NULL;
    } else if (HB_Trace(HB_TRACE_SELECT)) {
      fprintf(HB_TFile, "<HB> Selected hyperblock schedule estimate: %d\n",
	      CG_SCHED_EST_Resource_Cycles(hb_est));
    }
  }

  // Clean up the messes
  for (path = hb_paths.begin(); path != hb_paths.end(); path++) {
    CG_SCHED_EST_Delete(HB_PATH_Sched_Est(*path));
  }

  BB *bb;
  FOR_ALL_BB_SET_members(HB_Blocks(hb),bb) {
    if (BB_MAP_Get(bb_sched_est,bb)) {
      CG_SCHED_EST_Delete((CG_SCHED_EST *) BB_MAP_Get(bb_sched_est,bb));
    }
  }

  CG_SCHED_EST_Delete(hb_est);
  BB_MAP_Delete(bb_sched_est);
  BB_MAP_Delete(bb_mem_hazard);

  return selected_blocks;
}
  
/////////////////////////////////////
BOOL
HB_Block_Select(HB* candidate, BOOL profitable_ifc)
/////////////////////////////////////
//
//  See interface description.
///
/////////////////////////////////////
{
  BOOL retval;
  vector<BB *> join_list;
  INT i;
  BB *bb_j;
  BB_SET *current_blocks;
  BB_SET *visited_blocks;
  BB_SET *selected_blocks;
  BBLIST *succs;
  std::list<HB_PATH*> hb_paths;
  BB_SET *hb_blocks = HB_Blocks(candidate);

  // Conditionally skip the selection under a HB triage option.
  if (CG_skip_local_hbf) {
	BOOL skip;
	i = BB_id(HB_Entry(candidate));
        skip = (i < CG_local_skip_before ||
                i > CG_local_skip_after ||
                i == CG_local_skip_equal);
        if (skip) DevWarn("skip hyperblock at BB %d", i);
        else DevWarn("process hyperblock at BB %d", i);
	if (skip) return FALSE;
  }

  //
  // Get values used to modify priorities and select paths.
  //
  min_path_priority_ratio = atof(HB_min_path_priority_ratio);
  call_hazard_multiplier = atof(HB_call_hazard_multiplier);
  call_hazard_multiplier = 1.;
  memory_hazard_multiplier = atof(HB_memory_hazard_multiplier);
  base_probability_contribution = atof(HB_base_probability_contribution);
  max_sched_growth = atof(HB_max_sched_growth);
  max_sched_growth_sf = max_sched_growth;

  //
  // For first pass of profitable full if-conversion, tighten the
  // constraints.
  //
  if (profitable_ifc) {
    memory_hazard_multiplier = 0.0;
    call_hazard_multiplier = 0.0;
    max_sched_growth /= 2.0;
    if (max_sched_growth < 1.0) {
      max_sched_growth = 1.0;
    }
    max_sched_growth_sf /= 2.0;
    if (max_sched_growth_sf < 1.0) {
      max_sched_growth_sf = 1.0;
    }
    min_path_priority_ratio *= 2.0;
  }

  //
  // Loop blocks are taken as is. 
  //
  if (HB_Flags_Check(candidate, HB_LOOP_FLAG)) {
    // 
    // Select all blocks
    //
    return TRUE;
  }

  BB *cur_bb;
  BB_SET *processed_bbs;
  BB_SET *hb_bbs = HB_Blocks(candidate);

  processed_bbs = BB_SET_Create_Empty(BS_Size(hb_bbs), &MEM_local_pool);

  // #784343: Check for BBs which are targets of any irregular loop-cycles.
  // HBs can't be formed with irregular cycles present.

  Verify_HB(HB_Entry(candidate), &processed_bbs, hb_bbs);
  if (!BS_EqualP(processed_bbs, hb_bbs)) {
    if (HB_Trace(HB_TRACE_SELECT)) {
        fprintf(HB_TFile, "<HB>   Not Selected.  Loop found.\n     Blocks in proposed HB: ");
        BS_Print(hb_bbs,TFile); fprintf(TFile,"\n     Blocks before loop:    ");
        BS_Print(processed_bbs,TFile); fprintf(TFile,"\n");
    }
    return FALSE;
  }

  // If a hyperblocks consists of a series of hammocks, we want to select the 
  // blocks from each hammock independently. We determine a list of "join 
  // points" which are control equivalent to the entry block of the 
  // hyperblock. We then sort them in flow order (by just walking the first 
  // successor of each block, since all paths cross them in order) and treat 
  // them as independent hyperblocks for purposes of block selection. 
  //
  // These join points are all the post-dominators of the entry in the 
  // hyperblock
  //

#ifdef KEY
  // Identify hammocks that have successive conditional branches between join 
  // points. We do not handle > 1 branch level and hence we should ignore such
  // such hyperblocks when we If-Convert.
  BB *bb_succ;
  BOOL no_hammock = FALSE;
#endif
  for (bb_j = HB_Entry(candidate); bb_j && HB_Contains_Block(candidate, bb_j); ) {
    if (BB_SET_MemberP(BB_pdom_set(HB_Entry(candidate)),bb_j)) {
      join_list.push_back(bb_j);
    }
#ifdef KEY
    if (!no_hammock) {
      FOR_ALL_BB_SUCCS(bb_j, succs) {
	bb_succ = BBLIST_item(succs);
	if (HB_Contains_Block(candidate, bb_succ) &&
	    BB_branch_op(bb_j) &&
	    BB_branch_op(bb_succ) &&
	    OP_cond(BB_branch_op(bb_j)) &&
	    OP_cond(BB_branch_op(bb_succ))) {
	  no_hammock = TRUE;
	  break;
	}   
      }
    }
#endif
    succs = BB_succs(bb_j);
    if (succs) {
      bb_j = BBLIST_item(succs);
      // Make sure we don't loop around to the top; we'll be here forever if we do
      if (bb_j == HB_Entry(candidate)) bb_j = NULL;
    } else {
      bb_j = NULL;
    }
  }
  //
  // Push a NULL on the end. Makes iteration easier.
  // If there is an exit, we will get to it in the usual course of events
  //
  if (!HB_Exit(candidate)) {
    join_list.push_back(NULL);
  }
  
  MEM_POOL_Push(&MEM_local_pool);
  current_blocks = BB_SET_Create_Empty(PU_BB_Count+2, &MEM_local_pool);
  visited_blocks = BB_SET_Create_Empty(PU_BB_Count+2, &MEM_local_pool);
  retval = TRUE;
  
#ifdef KEY
  if (join_list.size() > 1 && !no_hammock)
    hammock_region = TRUE;
#endif
  for (i=0; i<join_list.size()-1; i++) {
    hb_paths.clear();
    if (!Enumerate_Paths(candidate, 
			 join_list[i], 
			 1.0, 
			 hb_paths,
			 join_list[i+1],
			 visited_blocks,
			 &MEM_local_pool)) {

      retval = FALSE;
      if (HB_Trace(HB_TRACE_SELECT)) {
	fprintf(HB_TFile, "<HB> === Block rejected, loop found or block too complex\n");
      }
      break; // Stop looking
    }
    if (HB_Trace(HB_TRACE_SELECT)) {
#ifdef KEY /* Mac port */
      fprintf(HB_TFile, "<HB> examining %ld paths\n", (long) hb_paths.size());
#else /* KEY Mac port */
      fprintf(HB_TFile, "<HB> examining %d paths\n", hb_paths.size());
#endif /* KEY Mac port */
    }
    if (hb_paths.size() > HB_MAX_PATHS) {
      if (HB_Trace(HB_TRACE_SELECT)) {
	fprintf(HB_TFile, "<HB> === Block rejected, too complex\n");
      }
      retval = FALSE;
      break;
    }
    selected_blocks = Select_Blocks(candidate, hb_paths, profitable_ifc);
    if (selected_blocks && BB_SET_MemberP(selected_blocks,join_list[i])) {
      current_blocks = BB_SET_UnionD(current_blocks,selected_blocks,&MEM_local_pool);
    } else {
      // Didn't select the first block, so give up on the whole thing
      retval = FALSE;
      break;
    }
  }
  
  // If we did in fact select things, then we copy the current_blocks set 
  // into the  hyperblock.

  if (retval) {
    HB_Blocks_Copy(candidate,current_blocks);
    if (HB_Trace(HB_TRACE_SELECT)) {
      fprintf(HB_TFile, "<HB> === Block selection complete, using ");
      candidate->Print();
    }
  }
  
  MEM_POOL_Pop(&MEM_local_pool);

  return retval;

}

