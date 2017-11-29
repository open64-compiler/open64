/*
 * Copyright 2002, 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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

//  Live range splitting
/////////////////////////////////////
//
//  Description:
//
//      Implementation of live range splitting.  The algorithm is:
//
//          1. Identify the blocks that will be in the allocatable part of the
//             split.  We look for the highest priority LUNIT in the lrange
//             and then grow the set of allocatable blocks outward from it
//             always adding the highest priority neighbors first.  If we
//             cannot find an allocatable LRANGE containing at least two
//             LUNITs, the algorithm fails.
//
//          2. Create a new LRANGE, <alloc_lrange>.  It is the
//             allocatable part of the split.  The initial <split_lrange>
//             becomes <deferred_lrange> which is the part of the split not
//             guaranteed to be allocatable.  Notice that deferred_lrange
//             keeeps the identity.  Doing this prevents us from having to
//             change the LUNITs of any of the blocks, since we are going to
//             color the alloc_lrange immediately and don't have to have
//             accurate LUNITs for its blocks.  Also we expect that on average
//             the alloc_lrange will have fewer block than the deferred_lrange
//             and so we limit the amount of renaming this way.
//
//          3. Add spills and restores along the border of the two LRANGEs.
//             This is not as good as we could do -- we'd like to preform
//             exchanges in registers when possible, but it simplifies things
//             a great deal.  For one thing we don't have to violate the rule
//             that LRANGEs interfere if they are live in the same block (How
//             to do this with exchanges?  All the ways I can think of are
//             messy.)
//
//          4. Fix up the TH related information in the blocks in the
//             alloc_lrange (it has a new TN and so TN renaming is necessary)
//             and along the border (the value is now communicated in memory,
//             so the live_in and live_out TNs need to be fixed.  (Or do they?)
//
//          5. Recompute the inteference graph neighbors of the
//             deferred_lrange. (We are done with the alloc_lrange's
//             interference at this point.)
//
//          6. Figure out where to reinsert the deferred_lrange in the
//             coloring list and do so, adjusting the neighbors_left count's
//             of its neighbors as appropriate.
//
/////////////////////////////////////


//  $Revision: 1.19 $
//  $Date: 05/12/05 08:59:10-08:00 $
//  $Author: bos@eng-24.pathscale.com $
//  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_split.cxx $


#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_split.cxx $ $Revision: 1.19 $";
#endif

#ifdef USE_PCH
#include "cg_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include <math.h>
#include <float.h>      // FLT_MAX
#include <limits.h>

#include "defs.h"
#include "errors.h"
#include "mempool.h"
#include "bitset.h"
#include "priority_queue.h"
#include "tracing.h"
#include "register.h"
#include "cg.h"
#include "cgir.h"
#include "cg_region.h"
#include "cg_spill.h"
#include "gtn_universe.h"
#include "gtn_set.h"
#include "cg_flags.h"
#include "gra.h"
#include "gra_bb.h"
#include "gra_lunit.h"
#include "gra_lrange.h"
#include "gra_spill.h"
#include "gra_loop.h"
#include "gra_region.h"
#include "gra_trace.h"
#include "gra_interfere.h"
#ifdef TARG_SL //minor_reg_alloc
#include "gra_para_region.h"
#endif 
// Generate a priority queue type for GRA_BBs
TYPE_PRQ(GRA_BB,GBBPRQ)

#ifdef HAS_STACKED_REGISTERS
extern REGISTER_SET stacked_caller_used;// from register_targ.cxx
#endif
#ifdef TARG_IA64
extern BOOL fat_self_recursive;
#endif
BOOL GRA_split_lranges = TRUE;
INT GRA_non_split_tn_id = -1;
#ifdef KEY
BOOL GRA_pu_has_handler = FALSE;
#endif

static LRANGE*  split_lrange;           // The LRANGE to split
static LRANGE*  alloc_lrange;           // The part to allocate
static LRANGE*  deferred_lrange;        // The part to defer until later
                                        //   In fact, thi swill be the
                                        //   split_lrange itself, but I've
                                        //   tried to use "deferred_lrange"
                                        //   when talking about the LRANGE as
                                        //   it will be after the split and
                                        //   "splti_lrange" for references
                                        //   that must be to the original
                                        //   LRANGE.
static GRA_BB*  alloc_gbb_list_head;    // Blocks in the alloc half
static GRA_BB*  border_gbb_list_head;   // Blocks on the outside border
static GBBPRQ   gbbprq;                 // Used to identify alloc half
static MEM_POOL prq_pool;               // To hold the gbbprq
static INT32    lranges_to_pass_count;  // How may LRANGEs must the
                                        //   deferred_lrange be pusned into
                                        //   the coloring list?

static float tot_spill_cost;		// Total cost of spills for split

static float split_alloc_priority;
static float split_spill_cost;
static float split_restore_cost;

static INT split_lunit_count;

// Here are some things I found useful for debugging this module.
//

/////////////////////////////////////
static BOOL
Compare_Frequencies( GRA_BB* gbb0, GRA_BB* gbb1 )
/////////////////////////////////////
//
//  GRA_BB priority comparison function for use with priority queues.
//
/////////////////////////////////////
{
  return gbb0->Freq() > gbb1->Freq();
}

/////////////////////////////////////
static void
Initialize_Priority_Queue(void)
/////////////////////////////////////
//
//  Set up a GRA_BB priority queue, <gbbprq>, for use in finding the most
//  frequently executed neighboring blocks of those blocks already selected
//  for inclusion in the alloc_lrange.
//
/////////////////////////////////////
{
  static BOOL prq_initialized = FALSE;

  if ( ! prq_initialized ) {
    prq_initialized = TRUE;
    MEM_POOL_Initialize(&prq_pool,"GRA BB priority queue for splitting",
                                  FALSE);
  }
  MEM_POOL_Push(&prq_pool);
  GBBPRQ_Initialize(&gbbprq,Compare_Frequencies,NULL,NULL,&prq_pool,
                                                          PU_BB_Count+2,
                                                          200);
}

/////////////////////////////////////
static void
Finalize_Priority_Queue(void)
/////////////////////////////////////
//
//  Done with <gbbprq>.
//
/////////////////////////////////////
{
  MEM_POOL_Pop(&prq_pool);
}

/////////////////////////////////////
static REGISTER_SET
Regs_Used( TN* tn, GRA_BB* gbb, ISA_REGISTER_CLASS rc, LRANGE* lrange,
	   BOOL reclaim)
/////////////////////////////////////
//
//  Return the set of registers in <rc> used by <gbb>.  If <gbb> is not in the
//  complement region, it is a member of one of the preallocated regions and
//  we'll return the registers used in the whole preallocated region as we
//  must traverse the entire region in order to be able to be able to allocate
//  a register in any single block of it.
//
//  If <gbb> has a call, then we'll add the caller saves registers to the
//  used set if its TN is live out of the block.  If <gbb> is not in the
//  complement region, and its region contains a call, we'll also add the
//  caller saves registers.
//
#ifdef KEY
//  If GRA_optimize_boundary is TRUE:
//  Change the semantics of the function to return the registers in <rc> that
//  are unavailable to hold <tn> in <gbb>.  A register is unavailable only if
//  it is unavailable over the same portion of <gbb> that is required by <tn>.
//
//  If GRA_reclaim is TRUE:
//  Change the semantics of the function to return the registers in <rc> that
//  are unavailable to hold <tn> in <gbb>.  A register is unavailable if it is
//  not reclaimable, where reclaimable means allocated and not referenced.
#endif
/////////////////////////////////////
{
  REGISTER_SET used  = gbb->Registers_Used(rc);

#ifdef TARG_X8664
  // MMX OP clobbers all x87 registers.
  if (rc == ISA_REGISTER_CLASS_x87 && gbb->mmx_OP())
    return REGISTER_CLASS_allocatable(ISA_REGISTER_CLASS_x87);

  // x87 OP clobbers all MMX registers.
  if (rc == ISA_REGISTER_CLASS_mmx && gbb->x87_OP())
    return REGISTER_CLASS_allocatable(ISA_REGISTER_CLASS_mmx);
#endif

#ifdef KEY
   // In the context of this function, USED means not available.  For register
  // reclaiming, a register is not available if it is not reclaimable.
  if (reclaim) {
    REGISTER_SET reclaimable =
      REGISTER_SET_Difference(used, gbb->Registers_Referenced(rc));
    used = REGISTER_SET_Difference(REGISTER_CLASS_allocatable(rc), reclaimable);
  }

 if (GRA_optimize_boundary &&
      !reclaim) {
    REGISTER reg;
    if (! lrange->Contains_Internal_BB(gbb)) {
      // <gbb> is a boundary BB.  A register is available even if it is used
      // over some portion of the BB, as long as the used portion does not
      // interfere with the portion required by <tn>.
      for (reg = REGISTER_SET_Choose(used);
	   reg != REGISTER_UNDEFINED;
	   reg = REGISTER_SET_Choose_Next(used, reg)) {
	LRANGE_BOUNDARY_BB *boundary_bb = lrange->Get_Boundary_Bb(gbb->Bb());
	if (! boundary_bb->Interfere(gbb, rc, reg)) {
	  // Register is available.  Remove it from <used>.
	  used = REGISTER_SET_Difference1(used, reg);
	}
      }
    }
  }
#endif

  LUNIT*       lunit = gbb_mgr.Split_LUNIT(gbb);

  if ( lunit != NULL &&
       !reclaim) {
    REGISTER_SET allowed_prefs = lunit->Allowed_Preferences();

    used = REGISTER_SET_Difference(used,allowed_prefs);
  }

  //
  // Take caller saved registers out of available set if this is
  // a lrange spanning an infrequent call (we are trying to give
  // it a caller saved register and split it around it's call blocks).
  //
  if (BB_call(gbb->Bb())) {
    if (split_lrange->Spans_Infreq_Call() ||
	GTN_SET_MemberP(BB_live_out(gbb->Bb()),tn)) {
      used = REGISTER_SET_Union(used, REGISTER_CLASS_caller_saves(rc));
#ifdef HAS_STACKED_REGISTERS
#ifdef KEY
      // Figure out how register reclaiming interacts with stack regs.
      FmtAssert(!reclaim,
		("Regs_Used: reclaiming not yet supported for stack regs"));
#endif
      if (REGISTER_Has_Stacked_Registers(rc)) 
        used = REGISTER_SET_Union(used, stacked_caller_used);
#endif
      if (gbb->Setjmp()) 
        used = REGISTER_SET_Union(used, REGISTER_CLASS_callee_saves(rc));
    }
  }
  if (BB_mod_rotating_registers(gbb->Bb())) {
#ifdef KEY
    // Figure out how register reclaiming interacts with rotating regs.
    FmtAssert(!reclaim,
	      ("Regs_Used: reclaiming not yet supported for rotating regs"));
#endif
    used = REGISTER_SET_Union(used, REGISTER_CLASS_rotating(rc));
  }
  else if (BB_mod_pred_rotating_registers(gbb->Bb()) &&
	   Is_Predicate_REGISTER_CLASS(rc))
    used = REGISTER_SET_Union(used, REGISTER_CLASS_rotating(rc));

#ifdef TARG_SL   //minor_reg_alloc
/* exclude registers used in parallel region */ 
  if(BB_rid(gbb->Bb()) && RID_TYPE_minor(BB_rid(gbb->Bb()))) {
    RID* rid = BB_rid(gbb->Bb());
    GRA_PARA_REGION* region = gra_para_region_mgr.Get(rid);
    Is_True((region),  ("para region is NULL"));
    REGISTER_SET exclude_set = region->Registers_Exclude(rc);
    used = REGISTER_SET_Union(used,  exclude_set);
  }
#endif 

  return used;
}

/////////////////////////////////////
static BOOL
Max_Colorable_LUNIT( LUNIT** result, BOOL reclaim )
/////////////////////////////////////
//
//  Search for the most urgent of split_lrange's LUNITs for which there is an
//  allocatiable register.  If found return TRUE with the maximal LUNIT
//  returned by reference in <result>
//
/////////////////////////////////////
{
  LUNIT*              maxlunit;
  LRANGE_LUNIT_ITER   iter;
  BOOL                found       = FALSE;
  ISA_REGISTER_CLASS  rc          = split_lrange->Rc();
  REGISTER_SET        all_regs    = REGISTER_CLASS_allocatable(rc);
  TN*                 tn          = split_lrange->Tn();

  if ( split_lrange->Avoid_RA() )
    all_regs = REGISTER_SET_Difference1(all_regs,TN_register(RA_TN));

  for (iter.Init(split_lrange); ! iter.Done(); iter.Step()) {
    REGISTER_SET regs_used;
    LUNIT* lunit = iter.Current();

    // Establish map from blocks with LUNITs to their LUNITs:
    gbb_mgr.Split_LUNIT_Set(lunit->Gbb(), lunit);

    regs_used = Regs_Used(tn,lunit->Gbb(),rc,split_lrange, reclaim);

    // LUNITs can be present to represent spills/restores at live range
    // split borders.  We want to skip these as seeds of splitting because:
    //   1. We want to spill the live range if it doesn't contain a use or def
    //      so it won't use up a precious register for no reason.
    //   2. We don't want to keep splitting it for no good reason.  This can
    //      be a compile time disaster.
    if ( lunit->Has_Exposed_Use() || lunit->Has_Def() ) {
      if ( !REGISTER_SET_EmptyP(REGISTER_SET_Difference(all_regs,regs_used)) ) {
        if ( ! found || lunit->Priority() > maxlunit->Priority() ) {
          found = TRUE;
          maxlunit = lunit;
        }
      }
    }
  }

  *result = maxlunit;
  return found;
}


/////////////////////////////////////
static BOOL
Live_In( GRA_BB* gbb )
/////////////////////////////////////
//
//  Is split_lrange live into <gbb>?
//
/////////////////////////////////////
{
  return GTN_SET_Intersection_MemberP(BB_live_in(gbb->Bb()), 
				      BB_defreach_in(gbb->Bb()),
                                      split_lrange->Tn());
}

/////////////////////////////////////
static BOOL
Live_Out( GRA_BB* gbb )
/////////////////////////////////////
//
//  Is split_lrange live out of <gbb>?
//
/////////////////////////////////////
{
  return GTN_SET_Intersection_MemberP(BB_live_out(gbb->Bb()), 
				      BB_defreach_out(gbb->Bb()),
                                      split_lrange->Tn());
}

/////////////////////////////////////
static INT
Traverse_Neighbors( GRA_BB_FLOW_NEIGHBOR_ITER* iter, BOOL (*live_fn)(GRA_BB*) )
/////////////////////////////////////
//
//  Traverse the blocks in <iter>, queueing any unqueued ones of which
//  <live_in_fn> is true.  return the count of neighbors from the live
//  range that have yet to be added to the split.
//
/////////////////////////////////////
{
  
  INT not_added_count = 0; 
  for ( ; ! iter->Done(); iter->Step() ) {
    GRA_BB* nbb = iter->Current();
    if (live_fn(nbb)) {
      if ( !gbb_mgr.Split_In_Alloc_LRANGE(nbb) ) {
	not_added_count++;
      }
      if ( ! gbb_mgr.Split_Queued(nbb) ) {
	gbb_mgr.Split_Queued_Set(nbb);
	GBBPRQ_Insert(&gbbprq,nbb);
      }
    }
  }
  return not_added_count;
}

/////////////////////////////////////
static bool
Defines_Split_TN(GRA_BB* gbb)
/////////////////////////////////////
//
//  return true if block defines the tn
//
/////////////////////////////////////
{
  LUNIT *lunit = NULL;
  (void) split_lrange->Find_LUNIT_For_GBB(gbb, &lunit);
  return lunit && lunit->Has_Def();
}

/////////////////////////////////////
static float
Check_Interior_Predecessor_Spill_Cost(GRA_BB* gbb, float priority)
/////////////////////////////////////
//
//  check the predecessors of a block that is now on the interior
//  of the split.  they may have had to spill in order for this
//  block to be able to reload.  if there are no other successors
//  of any such predecessor that is still on the border, then we
//  remove the spill cost from the current priority.
//
/////////////////////////////////////
{
  GRA_BB_FLOW_NEIGHBOR_ITER flow_iter;

  for (flow_iter.Preds_Init(gbb); ! flow_iter.Done(); flow_iter.Step()) {
    GRA_BB* pred = flow_iter.Current();
    if (gbb_mgr.Split_In_Alloc_LRANGE(pred)) {
      INT border_count = pred->Split_Succ_Border_Count();
      LUNIT *p_lunit = NULL;

      //
      // no spilling issues with loops back to self.  can't deduct
      // spill cost if block already on border of another split
      //
      (void) split_lrange->Find_LUNIT_For_GBB(pred, &p_lunit);
      if (pred == gbb || (p_lunit && p_lunit->Spill_Below())) continue;
      
      if (--border_count == 0 && pred->Split_Exit_Count() == 0) {
	GRA_Trace_Split_Add_Priority(pred, TRUE);
	priority += (pred->Freq() * split_spill_cost);
      }
      pred->Split_Succ_Border_Count_Set(border_count);
    }
  }
  return priority;
}

#ifdef KEY
/////////////////////////////////////
static void
Calculate_Interim_Reclaim_Cost(GRA_BB *gbb, BOOL find_spills, float *priority)
/////////////////////////////////////
//
//  GBB is moving into the alloc region, causing some neighbors of GBB to
//  become (outside) border BBs.  Spills/restores have to be added to these
//  new border BBs for register reclaiming.  Determine their costs.
//
/////////////////////////////////////
{
  GRA_BB_FLOW_NEIGHBOR_ITER flow_iter1, flow_iter2;

  for (find_spills ? flow_iter1.Preds_Init(gbb) : flow_iter1.Succs_Init(gbb);
       ! flow_iter1.Done();
       flow_iter1.Step()) {
    GRA_BB* gbb1 = flow_iter1.Current();
    if (!gbb_mgr.Split_In_Alloc_LRANGE(gbb1)) {
      // Add spill/restore to GBB1 unless it is already at the (outside)
      // border, in which case it already has the spill/restore.

      // When finding restores (!find_spills), GBB1 is the succ of GBB, and we
      // are interested in putting a restore in GBB1.  The restore is needed
      // only if the donor lrange is live-in at GBB1.  However, at this point,
      // we don't have live-in info because the donor lrange has not been
      // chosen.  As a result, assume it's always live-in.

      BOOL add_spill_restore = TRUE;
      for (find_spills ?
	     flow_iter2.Succs_Init(gbb1) : flow_iter2.Preds_Init(gbb1);
	   ! flow_iter2.Done();
	   flow_iter2.Step()) {
	GRA_BB* gbb2 = flow_iter2.Current();
	if (gbb2 != gbb &&
	    gbb_mgr.Split_In_Alloc_LRANGE(gbb2)) {
	  // GBB2 is already at the outside border.
	  add_spill_restore = FALSE;
	  break;
	}
      }

      if (add_spill_restore) {
	float cost = (gbb1->Freq() *
		       (find_spills ? split_spill_cost : split_restore_cost));
	*priority -= cost;
	tot_spill_cost += cost;
	GRA_Trace_Split_Reclaim_Sub_Priority(gbb1, find_spills, cost);

	// If adding restore to GBB1, must also add spill to each pred of GBB1
	// that is not in the reclaimed region.
	if (!find_spills) {
	  for (flow_iter2.Preds_Init(gbb1);
	       ! flow_iter2.Done();
	       flow_iter2.Step()) {
	    GRA_BB* succ_pred = flow_iter2.Current();	// GBB's SUCC's pred
	    if (succ_pred != gbb &&
		!gbb_mgr.Split_In_Alloc_LRANGE(succ_pred)) {
	      LUNIT *lunit = NULL;
	      // Add spill if SUCC_PRED doesn't already have one.
	      (void) split_lrange->Find_LUNIT_For_GBB(succ_pred, &lunit);
	      if (!(lunit && lunit->Spill_Below())) {
		float cost = (succ_pred->Freq() * split_spill_cost);
		*priority -= cost;
		tot_spill_cost += cost;
	        GRA_Trace_Split_Reclaim_Sub_Priority(succ_pred, TRUE, // store
						     cost);
	      }
	    }
	  }
	}
      }
    }
  }
}

/////////////////////////////////////
static void
Calculate_Interim_Reclaim_Saving(GRA_BB *gbb, BOOL find_spills, float *priority)
/////////////////////////////////////
//
//  GBB is moving into the reclaimed region.  Find the spills/restores of the
//  donor lrange that are no longer needed.  Determine their saving.
//
/////////////////////////////////////
{
  GRA_BB_FLOW_NEIGHBOR_ITER flow_iter;

  // If GBB is moving into the end of the reclaimed region, delete the restore
  // at GBB.
  BOOL need_restore = FALSE;
  for (flow_iter.Preds_Init(gbb); ! flow_iter.Done(); flow_iter.Step()) {
    GRA_BB* pred = flow_iter.Current();
    if (!gbb_mgr.Split_In_Alloc_LRANGE(pred)) {
      need_restore = TRUE;
      break;
    }
  }
  if (!need_restore) {
    float cost = (gbb->Freq() * split_restore_cost);
    *priority += cost;
    tot_spill_cost -= cost;
    GRA_Trace_Split_Reclaim_Add_Priority(gbb, FALSE, cost);
  }

  // If GBB is moving into the beginning of the reclaimed region, delete the
  // spill at GBB.
  BOOL need_spill = FALSE;
  for (flow_iter.Succs_Init(gbb); ! flow_iter.Done(); flow_iter.Step()) {
    GRA_BB* succ = flow_iter.Current();
    if (!gbb_mgr.Split_In_Alloc_LRANGE(succ)) {
      need_spill = TRUE;
      break;
    }
  }
  if (!need_spill) {
    float cost = (gbb->Freq() * split_spill_cost);
    *priority += cost;
    tot_spill_cost -= cost;
    GRA_Trace_Split_Reclaim_Add_Priority(gbb, TRUE, cost);
  }
}
#endif	// KEY

/////////////////////////////////////
static void
Calculate_Interim_Split_Priority(GRA_BB* gbb, INT spills_needed,
				 INT restores_needed, BOOL reclaim)
/////////////////////////////////////
//
//  determine the priority of the split based on the addition
//  of this block.  we first determine what the contribution
//  of this block is to the cost of the split if it remains on
//  the border, then we adjust the current cost of the split to
//  reflect the effect of any block bordering the new block being
//  moved to the interior of the split.
//
/////////////////////////////////////
{
  float priority = 0.0;
  float freq = gbb->Freq();
  LUNIT *lunit = NULL;
  GRA_BB_FLOW_NEIGHBOR_ITER flow_iter;

  //
  // Get the lunit, if any.  Add to count.
  //
  (void) split_lrange->Find_LUNIT_For_GBB(gbb, &lunit);
  if (lunit) {
    split_lunit_count++;
  }
  gbb->Split_Lunit_Count_Set(split_lunit_count);

  //
  // what does this block contribute if it stays on the border?
  // note that it may already be on the border of another split,
  // and as such, may already have a spill/restore inserted that
  // we must account for.
  //
  if (spills_needed || gbb->Split_Succ_Border_Count() > 0 ||
      (lunit && lunit->Spill_Below())) {
    gbb->Split_Exit_Count_Set(spills_needed);
    if (!Defines_Split_TN(gbb)) {
      GRA_Trace_Split_Sub_Priority(gbb, TRUE);
      float cost = (freq * split_spill_cost);
      priority -= cost;
      tot_spill_cost += cost;
    }
  }

  //
  // restores_needed is based on live_out of predecessors.  may not
  // be live_in if we kill it and have no upwardly exposed use.
  //
  if ((restores_needed && Live_In(gbb)) ||
      (lunit && lunit->Restore_Above())) {
    gbb->Split_Entry_Count_Set(restores_needed);
    if (!(lunit && lunit->Has_Exposed_Use())) {
      float cost = (freq * split_restore_cost);
      priority -= cost;
      tot_spill_cost += cost;
      GRA_Trace_Split_Sub_Priority(gbb, FALSE);
    }
  } else if (lunit && lunit->Has_Exposed_Use()) {
    priority += (freq * split_restore_cost);
    GRA_Trace_Split_Add_Priority(gbb, FALSE);
  }

  //
  // recalculate priority of current split based on the inclusion of this
  // block removing another block from the border.
  //
  if (Live_In(gbb)) {
    for (flow_iter.Preds_Init(gbb); ! flow_iter.Done(); flow_iter.Step()) {
      GRA_BB* pred = flow_iter.Current();
      INT border_count = pred->Split_Succ_Border_Count();
      LUNIT *p_lunit = NULL;

      //
      // no spilling issues with loops back to self.  can't deduct
      // spill cost if block already on border of another split
      //
      (void) split_lrange->Find_LUNIT_For_GBB(pred, &p_lunit);
      if (pred == gbb || (p_lunit && p_lunit->Spill_Below())) continue;

      if (restores_needed) {
	pred->Split_Succ_Border_Count_Set(++border_count);
      }
      if (gbb_mgr.Split_In_Alloc_LRANGE(pred)) {
	//
	// remove the cost of the border spill for this block that's
	// now in the interior of the split live range if it no longer
	// feeds a border block needing a restore.  note that if
	// this block had a definition, then this has the effect of
	// adding the benefit of putting it in the split (now that its
	// interior, we don't need a spill as we would have had we not
	// given it a register in this block).
	//
	if (pred->Split_Exit_Count() == 1) {
	  if (border_count == 0) {
	    float cost = (pred->Freq() * split_spill_cost);
	    priority += cost;
	    tot_spill_cost -= cost;
	    GRA_Trace_Split_Add_Priority(pred, TRUE);	  
	  }
	  pred->Split_Exit_Count_Set(0);
	} else {
	  pred->Split_Exit_Count_Set(pred->Split_Exit_Count() - 1);
	}
      }
    }
  }

  
  if (Live_Out(gbb)) {
    for (flow_iter.Succs_Init(gbb); ! flow_iter.Done(); flow_iter.Step()) {
      GRA_BB* succ = flow_iter.Current();
      LUNIT *s_lunit = NULL;

      //
      // no spilling issues with loops back to self.  can't deduct
      // spill cost if block already on border of another split
      //
      (void) split_lrange->Find_LUNIT_For_GBB(succ, &s_lunit);
      if (succ == gbb || (s_lunit && s_lunit->Restore_Above())) continue;

      if (gbb_mgr.Split_In_Alloc_LRANGE(succ)) {
	//
	// remove the cost of the border restore for this block that's
	// now in the interior of the split live range.  if it had an
	// exposed use, this has the effect of adding the benefit of
	// allocating a register to it in this block (otherwise, we'd
	// have needed to reload it for the exposed use).
	//
	if (succ->Split_Entry_Count() == 1) {
	  succ->Split_Entry_Count_Set(0);
	  if (Live_In(succ)) {
	    float cost = (succ->Freq() * split_restore_cost);
	    priority += cost;
	    tot_spill_cost -= cost;
	    GRA_Trace_Split_Add_Priority(succ, FALSE);	  
	    priority = Check_Interior_Predecessor_Spill_Cost(succ, priority);
	  }
	} else {
	  succ->Split_Entry_Count_Set(succ->Split_Entry_Count()-1);
	}
      }
    }
  }

#ifdef KEY
  // Determine the cost of the reclaim due to the addition of GBB to the
  // reclaimed region.  Similar to the cost of the split, there are four
  // components to the cost of the reclaim:
  //   1) The cost of adding restores to BBs that are now (outside) border BBs.
  //   2) The cost of adding spills to BBs that are now (outside) border BBs.
  //   3) The benefit of removing restores from (outside) border BBs that are
  //      now inside the reclaimed region.
  //   4) The benefit of removing spills from (outside) border BBs that are
  //      now inside the reclaimed region.

  if (reclaim) {
    // Cost #1: restores due to outside BBs becoming (outside) border BBs.
    Calculate_Interim_Reclaim_Cost(gbb, FALSE, &priority);

    // Cost #2: spills due to outside BBs becoming (outside) border BBs.
    Calculate_Interim_Reclaim_Cost(gbb, TRUE, &priority);

    // Cost #3: restores deleted due to GBB moving from the (outside) border
    //          into the reclaimed region.
    // Cost #4: spills deleted due to GBB moving from the (outside) border
    //          into the reclaimed region.
    Calculate_Interim_Reclaim_Saving(gbb, FALSE, &priority);
  }
#endif

  // set new interim split priority
  split_alloc_priority += priority;

  gbb->Split_Priority_Set(split_alloc_priority);
  gbb->Split_Spill_Cost_Set(tot_spill_cost);
  GRA_Trace_Split(1,"Split%spriority after adding BB:%d is %f\n",
		  reclaim ? " (with reclaim) " : " ",
		  BB_id(gbb->Bb()), split_alloc_priority);
}

/////////////////////////////////////
static void
Add_To_Colorable_Neighborhood( GRA_BB* gbb, BOOL reclaim )
/////////////////////////////////////
//
//  Add <gbb> to alloc_first_gbb, the list of GRA_BB's in the colorable
//  neighborhood.  Visit its successors and predecessors, adding them to
//  gbbprq.
//
/////////////////////////////////////
{
  GRA_BB_FLOW_NEIGHBOR_ITER flow_iter;
  INT spills_needed;
  INT restores_needed;

  gbb_mgr.Split_In_Alloc_LRANGE_Set(gbb);
  alloc_gbb_list_head = alloc_gbb_list_head->Split_List_Push(gbb);
  GRA_Trace_Split(1,"BB:%d in alloc LRANGE",BB_id(gbb->Bb()));

  // Add neighbors to the queue if not already:
  flow_iter.Succs_Init(gbb);
  spills_needed = Traverse_Neighbors(&flow_iter,Live_In);
  flow_iter.Preds_Init(gbb);
  restores_needed = Traverse_Neighbors(&flow_iter,Live_Out);

  Calculate_Interim_Split_Priority(gbb, spills_needed, restores_needed,
				   reclaim);

}

/////////////////////////////////////
static BOOL
Avoid_Unit_Spill(GRA_BB* gbb, REGISTER_SET allowed_regs,
		 REGISTER_SET regs_used, REGISTER_SET *loop_allowed,
		 ISA_REGISTER_CLASS rc, GRA_LOOP *maxloop, BOOL reclaim)
/////////////////////////////////////
//
// if this block is a loop prolog or epilog block, or it is at the
// boundry of a previously allocated region (real not swp) make sure 
// entire unit (nest or region) that it is associated with can be added before
// adding it to the colorable neighborhood.  otherwise, spills
// will be placed within the loop or region.  we allow prolog blocks to be
// part of the live range as that will be the block that stores
// are placed in if the loop is not allowed.  so, we check loop
// heads for outer nests.
//
/////////////////////////////////////
{
  GRA_BB_FLOW_NEIGHBOR_ITER iter;
  GRA_LOOP *gloop;
  BOOL is_prolog = FALSE;
  BOOL region_entry = gbb->Is_Region_Entry_Block();
  BOOL region_boundry = region_entry || gbb->Region_Epilog();

  if (!GRA_loop_splitting && !region_boundry) {
    return FALSE;
  }

  gloop = gbb->Loop();
  
  //
  // no need to do further checks if still in loop nest containing maximum
  // priority block.  note that here we mean the nest direcly containing it,
  // not a sibling nest within the same outer nest or an innermore loop.
  // we must check these loops before adding them.  we want to push out as
  // far as possible whether or not we can get all the way out of the nest.
  // must check regions regardless, though.  
  //
  if (gloop != NULL && maxloop != NULL && gloop != maxloop &&
      !region_boundry &&
      gloop->Outermost() == maxloop->Outermost()) {
    GRA_LOOP *loop;
    for (loop = maxloop; loop != NULL && loop != gloop; loop = loop->Parent());
    if (loop != NULL) {
      return FALSE;
    }
  }

  //
  // if this is a region block, ignore the loop as we'll get into trouble
  // if the region cuts the loop in half and the part outside the region
  // causes the loop to be unavailable (then, we'd have a spill in the
  // region).
  //
  if ((gloop != NULL && gloop->Loop_Head() == gbb->Bb() &&
      !gbb->Is_Region_Block(FALSE)) || region_entry ) {
    // check for live in so that we don't restrict this epilog block
    // even though the loop itself won't be considered.
    
    if ( Live_In(gbb)) {
      is_prolog = TRUE;
      *loop_allowed = REGISTER_SET_Difference(allowed_regs, regs_used);
      if (region_entry) {
	GRA_REGION *region = gbb->Region();
	*loop_allowed = REGISTER_SET_Difference(*loop_allowed,
						region->Registers_Used(rc));
      } else {
	REGISTER_SET unavailable = gloop->Registers_Used(rc);
#ifdef KEY
	// Make the loop-reclaimable registers available.  A register is
	// loop-reclaimable if it is used in the loop but is not referenced
	// (used or defined) in the loop.
	if (reclaim) {
	  unavailable =
	    REGISTER_SET_Intersection(unavailable,
				      gloop->Registers_Referenced(rc));
	}
#endif
	*loop_allowed = REGISTER_SET_Difference(*loop_allowed, unavailable); 
      }
      if (REGISTER_SET_EmptyP(*loop_allowed)) {
	return(TRUE);
      }
    }
  }
   
  //
  // a block can be both prolog and epilog.  ignore loop if block is in
  // region (see above).
  //
  if ((gbb->Loop_Epilog() && !gbb->Is_Region_Block(FALSE)) ||
      gbb->Region_Epilog()) {
    if (!is_prolog) {
      *loop_allowed = REGISTER_SET_Difference(allowed_regs, regs_used);
    }
    for (iter.Preds_Init(gbb); ! iter.Done(); iter.Step()) {
      GRA_BB* pred = iter.Current();
      gloop = pred->Loop();

      if (gbb->Region_Epilog() && pred->Is_Region_Block(FALSE)) {
	GRA_REGION *region = pred->Region();
	*loop_allowed =
	  REGISTER_SET_Difference(*loop_allowed, region->Registers_Used(rc));
	if (REGISTER_SET_EmptyP(*loop_allowed)) {
	  return(TRUE);
	}
      } else {
	// check for live in so that we don't restrict this epilog block
	// even though the loop itself won't be considered.
	if (gloop != NULL &&  Live_Out(pred)) {
	  *loop_allowed = REGISTER_SET_Difference(*loop_allowed,
						  gloop->Registers_Used(rc));
	  if (REGISTER_SET_EmptyP(*loop_allowed)) {
	    return(TRUE);
	  }
	}
      }
    }
  }
  return(FALSE);
}

/////////////////////////////////////
static INT32
Identify_Max_Colorable_Neighborhood( LUNIT* lunit, BOOL reclaim )
/////////////////////////////////////
//
//  Starting with <lunit> find a maximal set of colorable connected blocks,
//  adding neighbor blocks in frequency order.  Returns a count of the blocks
//  in the set.
//
/////////////////////////////////////
{
  INT32               count           = 1;
  ISA_REGISTER_CLASS  rc              = split_lrange->Rc();
  REGISTER_SET        allowed_regs    = REGISTER_CLASS_allocatable(rc);
  TN*                 tn              = split_lrange->Tn();
  LRANGE_LIVE_GBB_ITER iter;

  //
  // initialize fields in gbb used to track interim split priorities
  //
  for (iter.Init(split_lrange); ! iter.Done(); iter.Step()) {
    GRA_BB* gbb = iter.Current();
    gbb->Split_Priority_Set(0.0);
    gbb->Split_Entry_Count_Set(0);
    gbb->Split_Exit_Count_Set(0);
    gbb->Split_Succ_Border_Count_Set(0);
    gbb->Split_Lunit_Count_Set(0);
  }

  //
  // Number of LUNITs in current split.
  //
  split_lunit_count = 0;

  if (    split_lrange->Type() != LRANGE_TYPE_LOCAL
       && TN_is_save_reg(split_lrange->Tn())
  ) {
    REGISTER sv_reg = TN_save_reg(split_lrange->Tn());
    REGISTER_SET singleton = REGISTER_SET_Union1(REGISTER_SET_EMPTY_SET,sv_reg);
    allowed_regs = REGISTER_SET_Intersection(allowed_regs,singleton);
  }
  else if ( split_lrange->Avoid_RA() ) {
    allowed_regs = REGISTER_SET_Difference1(allowed_regs,TN_register(RA_TN));
  }

  Initialize_Priority_Queue();
  border_gbb_list_head = NULL;
  alloc_gbb_list_head = NULL;
  split_alloc_priority = 0.0;
  tot_spill_cost = 0.0;
  gbb_mgr.Split_Queued_Set(lunit->Gbb());
  Add_To_Colorable_Neighborhood(lunit->Gbb(), reclaim);
  allowed_regs =
    REGISTER_SET_Difference(allowed_regs,Regs_Used(tn,lunit->Gbb(),rc,
						   split_lrange, reclaim));

  while ( GBBPRQ_Size(&gbbprq) > 0 ) {
    GRA_BB*         gbb       = GBBPRQ_Delete_Top(&gbbprq);
    REGISTER_SET    regs_used = Regs_Used(tn,gbb,rc, split_lrange, reclaim);
    REGISTER_SET    loop_allowed = REGISTER_SET_EMPTY_SET;

    if ( REGISTER_SET_EmptyP(REGISTER_SET_Difference(allowed_regs,regs_used)) ||
	Avoid_Unit_Spill(gbb, allowed_regs, regs_used, &loop_allowed, rc,
			 lunit->Gbb()->Loop(), reclaim) ) {
      border_gbb_list_head = border_gbb_list_head->Split_List_Push(gbb);
      GRA_Trace_Split(1,"BB:%d in deferred border",BB_id(gbb->Bb()));
    }
    else {
      //
      // if we're adding a prolog or epilog, Avoid_Unit_Spill will set
      // the allowed register set.
      //
      if (!REGISTER_SET_EmptyP(loop_allowed)) {
	allowed_regs = loop_allowed;
      } else {
	allowed_regs = REGISTER_SET_Difference(allowed_regs,regs_used);
      }
      Add_To_Colorable_Neighborhood(gbb, reclaim);
      ++count;
    }
  }
  Finalize_Priority_Queue();
  return count;
}

/////////////////////////////////////
static void
Divide_LRANGE(void)
/////////////////////////////////////
//
//  Create alloc_lrange.  Now deferred_lrange is split_lrange.  Divide up the
//  LUNITs in split_lrange between alloc_lrange and deferred_lrange.
//
/////////////////////////////////////
{
  LRANGE_LUNIT_ITER iter;

  deferred_lrange = split_lrange;
  alloc_lrange = lrange_mgr.Create_Duplicate(split_lrange);

  GRA_Trace_Split(0,"Divide_LRANGE TN%d split from TN%d",
	      TN_number(alloc_lrange->Tn()), TN_number(deferred_lrange->Tn()));

  // Go though the live units dividing them between the alloc and the
  // deferred LRANGE.

  iter.Init(split_lrange); 	// Must be before the clear
  split_lrange->First_Lunit_Reset(); // clear lunit list

  for ( ; ! iter.Done(); iter.Step()) {
    LUNIT* lunit = iter.Current();

    if ( gbb_mgr.Split_In_Alloc_LRANGE(lunit->Gbb()) ) {
      alloc_lrange->Add_LUNIT(lunit);
      GRA_Trace_Split(1,"BB%d with LUNIT in alloc_lrange",
                        BB_id(lunit->Gbb()->Bb()));
    } else {
      deferred_lrange->Add_LUNIT(lunit);
      GRA_Trace_Split(1,"BB%d with LUNIT in deferred_lrange",
                        BB_id(lunit->Gbb()->Bb()));
    }
  }
}

/////////////////////////////////////
static LUNIT*
Get_Possibly_Add_LUNIT( GRA_BB* gbb )
/////////////////////////////////////
//
//  If there is already a LUNIT for the split_lrange (either side) associated
//  with <gbb>, return it.  Else make one and return that.
//
/////////////////////////////////////
{
  LUNIT *lunit = gbb_mgr.Split_LUNIT(gbb);

  if ( lunit == NULL ) {
    if ( gbb_mgr.Split_In_Alloc_LRANGE(gbb) )
      lunit = LUNIT_Create(alloc_lrange,gbb);
    else
      lunit = LUNIT_Create(deferred_lrange,gbb);

    gbb_mgr.Split_LUNIT_Set(gbb, lunit);
    lunit->Split_Lunit_Set();
  }

  return lunit;
}

/////////////////////////////////////
static void
Split_Store( GRA_BB* gbb )
/////////////////////////////////////
//
//  Make a store from the appropriate LRANGE (alloc or deferred) at the bottom
//  of <gbb> it it is not there already.
//
/////////////////////////////////////
{
  // Could optimize this in that we won't need the store if we are sure
  // the home is up to date at the bottom of this block.  There are
  // various ways we could know this, in particular if there is no path
  // from a definition of the TN which is not a load from home to the
  // bottom of this block.  (This will be true of constants, of course.)

  if ( ! gbb_mgr.Split_Has_Store(gbb) ) {
    LUNIT* lunit = Get_Possibly_Add_LUNIT(gbb);

    gbb_mgr.Split_Has_Store_Set(gbb);
    GRA_Note_Save_Below(lunit);
  }
}

/////////////////////////////////////
static void
Split_Load( GRA_BB* gbb )
/////////////////////////////////////
//
//  Make a load into the appropriate LRANGE (alloc or deferred) at the top of
//  <gbb> if it is not already there.  Make sure all predecessors store the
//  appropriate LRANGE to home.  When adding split loads or stores, we may be
//  adding the first reference to the given <gbb> in which case we need to add
//  a LUNIT.
//
/////////////////////////////////////
{
  GRA_BB_FLOW_NEIGHBOR_ITER iter;

  if ( ! gbb_mgr.Split_Has_Load(gbb) ) {
    LUNIT *lunit   = Get_Possibly_Add_LUNIT(gbb);
    LRANGE *lrange = lunit->Lrange();

    gbb_mgr.Split_Has_Load_Set(gbb);

    for (iter.Preds_Init(gbb); ! iter.Done(); iter.Step()) {
      GRA_BB* pred_bb = iter.Current();
      if ( Live_Out(pred_bb) )
        Split_Store(pred_bb);
    }

    gbb->Remove_Live_In_LRANGE(split_lrange);

    GRA_Note_Restore_Above(lunit);
  }
}

/////////////////////////////////////
/*ARGUSED1*/
static void
Split_Load_GBB( GRA_BB* gbb, GRA_BB* neighbor )
/////////////////////////////////////
//
//  We've decided that <gbb> needs a reload of the alloc_lrange or
//  deferred_lrange (as the case may be).  This function exists to be passed
//  to Find_Border_Transitions.
//
/////////////////////////////////////
{
  Split_Load(gbb);
}

/////////////////////////////////////
/*ARGSUSED*/
static void
Split_Load_Neighbor( GRA_BB* gbb, GRA_BB* neighbor )
/////////////////////////////////////
//
//  We've decided that <neighbor> needs a reload of the alloc_lrange or
//  deferred_lrange (as the case may be).  This function exists to be passed
//  to Find_Border_Transitions.
//
/////////////////////////////////////
{
  Split_Load(neighbor);
}

/////////////////////////////////////
static void
Find_Border_Transitions( GRA_BB* gbb,
                         GRA_BB_FLOW_NEIGHBOR_ITER* iter,
                         void (gbb_neighbor_fn)(GRA_BB*, GRA_BB*)
)
/////////////////////////////////////
//
//  <iter> iterates over either the flow successors or predecessors of <gbb> a
//  block in the (outside) border of the alloc_lrange.  Find blocks on the
//  <iter> that are inside the border and invoke <gbb_neighbor_fn> on them.
//
/////////////////////////////////////
{
  for ( ; ! iter->Done(); iter->Step()) {
    GRA_BB* neighbor = iter->Current();

    if ( gbb_mgr.Split_In_Alloc_LRANGE(neighbor) ) {
      // Found one in alloc_lrange.
      gbb_neighbor_fn(gbb,neighbor);
    }
  }
}

/////////////////////////////////////
static void
Add_Spills_And_Restores(void)
/////////////////////////////////////
//
//  Walk the BBs on the (outside) border of the alloc_lrange, adding spills
//  and restores for the transition between the two LRANGEs.
//
/////////////////////////////////////
{
  GRA_BB_SPLIT_LIST_ITER    iter;
  GRA_BB_FLOW_NEIGHBOR_ITER flow_iter;

  for (iter.Init(border_gbb_list_head); ! iter.Done(); iter.Step()) {
    GRA_BB* gbb = iter.Current();

    //
    // Check predecessors for members of alloc_lrange.  If found make sure
    // that gbb loads from home and all predecessors store to home.
    //
    if (BB_live_in(gbb->Bb())) {
      flow_iter.Preds_Init(gbb);
      Find_Border_Transitions(gbb,&flow_iter,Split_Load_GBB);
    }

    //
    // Check successors for members of lrange0.  If found make sure gbb
    // stores to home and successor loads from home.  All the predecessors
    // of any such successor (including gbb) must also store.
    //
    flow_iter.Succs_Init(gbb);
    Find_Border_Transitions(gbb,&flow_iter,Split_Load_Neighbor);
  }
}

/////////////////////////////////////
#ifndef KEY
static
#endif
BOOL
Has_Live_In_Successor( GRA_BB* gbb, LRANGE* lrange )
/////////////////////////////////////
//
//  Is <lrange> Live_In to one of <gbb>'s flow successors?
//
/////////////////////////////////////
{
  GRA_BB_FLOW_NEIGHBOR_ITER iter;

  for (iter.Succs_Init(gbb); ! iter.Done(); iter.Step()) {
    GRA_BB* succ = iter.Current();

    if ( succ->Is_Live_In_LRANGE(lrange) )
      return TRUE;
  }

  return FALSE;
}

/////////////////////////////////////
static void
Fix_TN_Live_Out_Info( GRA_BB* gbb_list_head, LRANGE* old_lrange,
                                             LRANGE* new_lrange )
/////////////////////////////////////
//
//  Rename <old_lrange> to <new_lrange> in the Live_Outs of the blocks in the
//  Split_List headed by <gbb_list_head>.  Check to make sure that
//  <old_lrange> is still really live_out.
//
/////////////////////////////////////
{
  GRA_BB_SPLIT_LIST_ITER iter;

  for (iter.Init(gbb_list_head); ! iter.Done(); iter.Step()) {
    GRA_BB* gbb = iter.Current();

    if ( gbb->Is_Live_Out_LRANGE(old_lrange) ) {
      gbb->Remove_Live_Out_LRANGE(old_lrange);

      if (! gbb_mgr.Split_Has_Store(gbb) || 
	  Has_Live_In_Successor(gbb,old_lrange) ) {
        // Either:
        //    1. None of it's successors has had a load added, implying
        //       that none of its successors is on the opposite of the
        //       split, (this is a speed hack) or
        //    2. Even though, One of its successors has a predecessor in the
        //       opposite half of the split and we have checked and found that
        //       <gbb> still has a successor into which split_tn is live.
        //
        // We will add the new_tn the live_outs.
        //
        // Notice the assumption that the live_ins have not yet been renamed.

        gbb->Add_Live_Out_LRANGE(new_lrange);
      }
    }
  }
}

/////////////////////////////////////
static void
Fix_TN_Live_Info(void)
/////////////////////////////////////
//
//  Fix up the Live_In and Live_Out information associated with the BBs.  We
//  have already removed the LRANGE from the Live_In set of any block in which
//  we have decided we need to load it.  No we have to fix things to reflect
//  the following additional facts:
//
//      1. In all the blocks in the alloc_lrange, we have renamed the original
//         TN.
//      2. In all the blocks in the border of the deferred_lrange, the
//         deferred_lrange is only live_out if the block has a successor in
//         the deferred_lrange wih the lrange still live in.
//      3. Similarly for the alloc_lrange.
//
/////////////////////////////////////
{
  GRA_BB_SPLIT_LIST_ITER iter;

  TN *split_tn    = split_lrange->Tn();
  TN *alloc_tn    = alloc_lrange->Tn();
  TN *deferred_tn = deferred_lrange->Tn();

  Fix_TN_Live_Out_Info(alloc_gbb_list_head,split_lrange,alloc_lrange);
  Fix_TN_Live_Out_Info(border_gbb_list_head,split_lrange,deferred_lrange);

  for (iter.Init(alloc_gbb_list_head); ! iter.Done(); iter.Step()) {
    GRA_BB* gbb = iter.Current();

    if ( gbb->Is_Live_In_LRANGE(split_lrange) ) {
      gbb->Remove_Live_In_LRANGE(split_lrange);
      gbb->Add_Live_In_LRANGE(alloc_lrange);
    }

    GTN_SET_Difference1D(gbb->Needs_A_Register(),split_tn);
    gbb->Needs_A_Register_Set(GTN_SET_Union1D(gbb->Needs_A_Register(), alloc_tn,
					      GRA_pool));
  }
}

/////////////////////////////////////
static void
Rename_TN_References(void)
/////////////////////////////////////
//
//  Rename any TN references in the alloc_lrange.
//
/////////////////////////////////////
{
  LRANGE_LUNIT_ITER iter;
  TN* split_tn = split_lrange->Tn();
  TN* alloc_tn = alloc_lrange->Tn();

  for (iter.Init(alloc_lrange); ! iter.Done(); iter.Step()) {
    LUNIT* lunit = iter.Current();
    GRA_BB* gbb = lunit->Gbb();

    gbb->Rename_TN_References(split_tn,alloc_tn);
  }
}

/////////////////////////////////////
static void
Calculate_Live_BB_Sets(void)
/////////////////////////////////////
//
//  Make the right live_bb sets for both alloc_lrange and deferred_lrange
//  They both have to be correct because they will both have allowed registers
//  calculated later on.
//
/////////////////////////////////////
{
  GRA_BB_SPLIT_LIST_ITER iter;

  //  Notice the sneaky fact that deferred_lrange really is split_lrange and
  //  thus we can just remove any of the BBs in alloc_lrange and the result
  //  will be the deferred part.

  for (iter.Init(alloc_gbb_list_head); ! iter.Done(); iter.Step()) {
    GRA_BB* gbb = iter.Current();

    alloc_lrange->Add_Live_BB(gbb);
    deferred_lrange->Remove_Live_BB(gbb);
  }
}

/////////////////////////////////////
static void
Fix_Local_Interference(void)
/////////////////////////////////////
//
//  Replace split_lrange with alloc_lrange in the global interferences for
//  each block in its range.  These global interferences are uses as the
//  interference sets for all the local LRANGEs in the blocks.
//
/////////////////////////////////////
{
  GRA_BB_SPLIT_LIST_ITER iter;

  //  Notice the sneaky fact that deferred_lrange really is split_lrange and
  //  thus we can just remove any of the BBs in alloc_lrange and the result
  //  will be the deferred part.

  for (iter.Init(alloc_gbb_list_head); !iter.Done(); iter.Step()) {
    GRA_BB* gbb = iter.Current();
    gbb->Replace_Global_Interference(split_lrange,alloc_lrange);
  }
}

/////////////////////////////////////
static GTN_SET*
Calculate_Interfering_GTNs( LRANGE* lrange )
/////////////////////////////////////
//
//  Calculate the set of global TNs that interferes with <lrange>.
//  
/////////////////////////////////////
{
  GTN_SET* result = GTN_SET_Create_Empty(GTN_UNIVERSE_size,&MEM_local_nz_pool);

  LRANGE_LIVE_GBB_ITER iter;
  for (iter.Init(lrange); ! iter.Done(); iter.Step()) {
    GRA_BB *live_gbb = iter.Current();

    result = GTN_SET_UnionD(result,live_gbb->Needs_A_Register(),
                            &MEM_local_nz_pool);
  }

  return result;
}

/////////////////////////////////////
static BOOL
Interferes( LRANGE* lrange0, GTN_SET* interfering_gtns, LRANGE* lrange1 )
/////////////////////////////////////
//
//  Does <lrange0> interfere with <lrange1>?  If both LRANGEs are complement
//  LRANGEs, use the set <interfering_gtn>s to answer this question, as a
//  speed hack.
//  
/////////////////////////////////////
{
  if ( lrange1->Type() == LRANGE_TYPE_COMPLEMENT )
    return GTN_SET_MemberP(interfering_gtns, lrange1->Tn());
  else
    return lrange0->Interferes(lrange1);
}


/////////////////////////////////////
static void
Fix_Interference(void)
/////////////////////////////////////
//
//  Make deferred_lrange's and alloc_lrange's interference graph relations
//  be valid.  Any neighbors of <split_lrange> that are not neighbors of
//  <deferred_lrange> must have <split_lrange> (now <deferred_lrange>,
//  remember) removed from their neighbors set.
//
//  It is important to keep in mind the purpose of the interference sets at
//  this point.  Simplification has already taken place, and the interferences
//  sets are used only for preferencing -- in particular avoiding neighbor
//  preferences.  Because we are about to allocate a register to
//  <alloc_lrange> it is only important that it's interference information be
//  correct.  <alloc_lrange> does not have to be included in the neighbor set
//  of its neighbors, because we will color <alloc_lrange> before its
//  neighbors and thus will not need to avoid it preferences when coloring its
//  neighbors (we will avoid it's actual CHOICE.)
//
//  As a side effect, the LRANGE_1_set contains just the neighbors of
//  deferred_lrange that need to come before it in the coloring list and
//  lranges_to_pass_count is a count of these.  This will be used right away
//  in Add_Deferred_To_Coloring_List.  On exit the Neighbors_Left fields of
//  deferred_lrange and its neighbors reflect the situation if deferred_lrange
//  were added immediately after alloc_lrange.  Add_Deferred_To_Coloring_List
//  will change this as it figures out where to put deferred_lrange.
//
//  TODO:  it appears that we lose preferencing information for this portion
//  of the split.  that is, any lrange that is preferenced to this one but has
//  not yet been colored will not look at the allocated lrange when choosing
//  its color (it only has the original lrange in its list of preferences). 
//  this can be particularly damaging if the preferencing copy happens to be
//  in the allocated lrange.  we should update the preference lists here too.
//
/////////////////////////////////////
{
  LRANGE_NEIGHBOR_ITER iter;
  GTN_SET*           alloc_interfering_gtns;
  GTN_SET*           deferred_interfering_gtns;
  GRA_REGION*        cregion        = gra_region_mgr.Complement_Region();
  ISA_REGISTER_CLASS rc             = split_lrange->Rc();
  REGISTER_SET       neighbor_used  = REGISTER_SET_EMPTY_SET;

  MEM_POOL_Push(&MEM_local_nz_pool);

  Calculate_Live_BB_Sets();

  alloc_interfering_gtns = Calculate_Interfering_GTNs(alloc_lrange);
  deferred_interfering_gtns = Calculate_Interfering_GTNs(deferred_lrange);

  deferred_lrange->Calculate_Priority();
  lrange_mgr.Clear_One_Set();

  lranges_to_pass_count = 0;

  lrange_mgr.Begin_Complement_Interference(alloc_lrange);

  GRA_Trace_Split(1,"fix interference for TN%d with %d neighbors",TN_number(split_lrange->Tn()),
		  split_lrange->Neighbors_Left());

  for (iter.Init(split_lrange,gra_region_mgr.Complement_Region()); 
       !iter.Done(); iter.Step()) {
    LRANGE* neighbor = iter.Current();

    if ( ! Interferes(deferred_lrange,deferred_interfering_gtns,neighbor) ) {
      deferred_lrange->Remove_Neighbor(neighbor,cregion);
      neighbor->Remove_Neighbor(deferred_lrange,cregion);
    }
    else if ( neighbor->Allocated() ) {

      // The number of allocated neighbors should be equal to the
      // _Neighbors_Left field.  At this point, though we can do something a
      // little better.  We count the number of registers used by the
      // allocated neighbors which is a tighter upper bound.  Later on, we'll
      // want the _Neigibors_Left field to figure out if some future split
      // causes defereed_lrange to become a Briggs point.  Is so we may have
      // to push the split past the defereed_lrange in its coloring list.

      if ( REGISTER_SET_MemberP(neighbor_used, neighbor->Reg()) )
        deferred_lrange->Neighbors_Left_Decrement();
      else {
        neighbor_used = REGISTER_SET_Union1(neighbor_used, neighbor->Reg());
      }
    }
    else if ( neighbor->Spilled() ) {
      deferred_lrange->Neighbors_Left_Decrement();
    }
    else if ( Interferes(alloc_lrange,alloc_interfering_gtns,neighbor) ) {
      // This guy just had the number of interferences increased by 1 and
      // might becomre a Briggs point if we don't move deferred_lrange after
      // him in the coloring list.  Keep track of any such that have higher
      // priority than defereed_lrange.
      //
      // We could do a little better here at a price by keeping track of the
      // allowed (or forbidden) registers during coloring.  When two neighbors
      // of a LRANGE are colored with the same color, as can happen when the
      // neighbors don't interfere with each other, it's as if it had one
      // fewer neighbors from the standpoint of colorability.  If we did this
      // the _neighbors_left field might more accurately be called
      // _max_neighbor_colors and would be more accurate for this purpose at
      // hand, though still not a tight upper bound (but tighter than we are
      // using now.)

      neighbor->Neighbors_Left_Increment();

      if (neighbor->Neighbors_Left() + 1 >= neighbor->Candidate_Reg_Count()
           && neighbor->Priority() > deferred_lrange->Priority()
      ) {
        lrange_mgr.One_Set_Union1(neighbor);
        ++lranges_to_pass_count;
      }
    }

    if ( Interferes(alloc_lrange,alloc_interfering_gtns,neighbor) )
      lrange_mgr.Complement_Interference(neighbor);
  }

  lrange_mgr.End_Complement_Interference();

  Fix_Local_Interference();

  MEM_POOL_Pop(&MEM_local_nz_pool);
}

/////////////////////////////////////
static void
Add_Deferred_To_Coloring_List( LRANGE_CLIST_ITER* client_iter )
/////////////////////////////////////
//
//  Add the deferred part of the spit LRANGE back into the coloring tlist a
//  the appropriate point (see comment below.)
//
//  <client_iter> is the LRANGE_CLIST_ITER that the coloring engine was using
//  to iterate through the coloring list.  It must have had split_lrange as
//  it's _Current element.  We'll use to to find the appropriate point and
//  insert deferred_lrange there.
//
/////////////////////////////////////
{
  LRANGE_CLIST_ITER dup_iter;

#ifdef KEY
  for (dup_iter.Init_Following(client_iter);
       !dup_iter.Done();
       dup_iter.Step()) {
    LRANGE* lrange = dup_iter.Current();
    if (deferred_lrange->Priority() > lrange->Priority()) {
      dup_iter.Push(deferred_lrange);
      return;
    }
  }
  dup_iter.Push(deferred_lrange);
#else
  if ( lranges_to_pass_count == 0 ) {
    client_iter->Splice(deferred_lrange);
    return;
  }

  for (dup_iter.Init_Following(client_iter); ; dup_iter.Step()) {
    LRANGE* lrange = dup_iter.Current();

    if ( lrange->Interferes(deferred_lrange) ) {
      lrange->Neighbors_Left_Decrement();
      deferred_lrange->Neighbors_Left_Increment();

      if ( lrange_mgr.One_Set_MemberP(lrange) ) {
        if ( --lranges_to_pass_count == 0 ) {

          // At this point we have moved past all the neighbors of
          // deferred_lrange that:
          //
          //    1. are more urgent than deferred_lrange,
          //
          //    2. are Briggs points already or would be if deferred_lrange
          //       came first in the coloring list.
          //
          // So we have maintained the property that the Briggs points are in
          // order.  Now we *could* worry about the non-Briggs points and keep
          // moving 'deferred_lrange' down the priority list until we pass
          // it's last neighbor of higher priority.  Is this a good idea?
          // It's good if splitting subsequent splitting makes one of the guys
          // we would pass become a Briggs point.  It's bad if resplitting
          // deferred_lrange picks out a sub LRANGE that has higher priority
          // than the guys we would pass.  So I can't tell what to do.  We'll
          // come down on the side of doing the cheaper thing -- nothing.

	  dup_iter.Splice(deferred_lrange);
          return;
        }
      }
    }
  }
#endif
}

/////////////////////////////////////
static void
Fix_Call_Info( LRANGE* lrange )
/////////////////////////////////////
//
//  Check whether lrange still spans a call and set its _Spans_A_Call property
//  appropriately.  After the first split, we never will set the infrequen
//  call again.  The assumption being that we've already profitably split
//  around the infrequent call.
//
/////////////////////////////////////
{
  // looping over the GRA_BBs that have calls in a complement live range
  class LRANGE_LIVE_CALL_GBB_ITER {
    BB_SET *set;
    BB *current;
  public:
    LRANGE_LIVE_CALL_GBB_ITER(void) {}
    ~LRANGE_LIVE_CALL_GBB_ITER(void) {}
  
    void Init(LRANGE *lrange) {
      Is_True(lrange->Type() == LRANGE_TYPE_COMPLEMENT,
              ("Iterating over the live_gbbs of a non-complement LRANGE"));
      set = lrange->Live_BB_Set();
      current = BB_SET_Intersection_Choose(set, gbb_mgr.Blocks_With_Calls());
    }
    BOOL Done(void)		{ return current == BB_SET_CHOOSE_FAILURE; }
    GRA_BB *Current(void)		{ return gbb_mgr.Get(current); }
    void Step(void)		{ current = BB_SET_Intersection_Choose_Next(set,
				        gbb_mgr.Blocks_With_Calls(), current); }
  } iter;

  lrange->Spans_A_Call_Reset();
  lrange->Spans_Infreq_Call_Reset();

  for (iter.Init(lrange); ! iter.Done(); iter.Step()) {
    GRA_BB* gbb = iter.Current();

    if (    gbb != NULL
         && gbb->Region() == gra_region_mgr.Complement_Region()
         && GTN_SET_MemberP(BB_live_out(gbb->Bb()), lrange->Tn())
    ) {
      lrange->Spans_A_Call_Set();

      if (gbb->Setjmp())
        lrange->Spans_A_Setjmp_Set();
#ifdef TARG_X8664
      if (gbb->Savexmms())
        lrange->Spans_Savexmms_Set();
#endif
      return;
    }
  }
}

/////////////////////////////////////
static void
Fix_Rot_Reg_Clob_Info( LRANGE* lrange )
/////////////////////////////////////
//
// Check whether lrange still spans a BB that contains an instruction that
// clobbers rotating register
//
/////////////////////////////////////
{
  // looping over the GRA_BBs that clobbers rotating registers
  class LRANGE_ROT_REG_CLOB_BB_ITER {
  private:
    BB_SET *set;
    BB *current;
  public:
    LRANGE_ROT_REG_CLOB_BB_ITER(void) {}
   ~LRANGE_ROT_REG_CLOB_BB_ITER(void) {}

    void Init(LRANGE *lrange)	{ set = lrange->Live_BB_Set(); 
				  current = BB_SET_Intersection_Choose(set, 
					gbb_mgr.Blocks_With_Rot_Reg_Clob()); }
    BOOL Done(void)		{ return current == BB_SET_CHOOSE_FAILURE; }
    BB *Current(void)		{ return current; }
    void Step(void)		{ current = BB_SET_Intersection_Choose_Next(set,
					gbb_mgr.Blocks_With_Rot_Reg_Clob(),
					current); }
  } iter;

  lrange->Spans_Rot_Reg_Clob_Reset();

  for (iter.Init(lrange); !iter.Done(); iter.Step()) {
    BB *bb = iter.Current();
    if (BB_mod_rotating_registers(bb)) {
      lrange->Spans_Rot_Reg_Clob_Set();
      return;
    }
    else if (BB_mod_pred_rotating_registers(bb) &&
	     Is_Predicate_REGISTER_CLASS(lrange->Rc())) {
      lrange->Spans_Rot_Reg_Clob_Set();
      return;
    }
  }
}

#ifdef TARG_X8664
/////////////////////////////////////
static void
Fix_x86_Info( LRANGE* lrange )
/////////////////////////////////////
//
// Update x86-specific info about the lrange.
//
/////////////////////////////////////
{
  // Determine if lrange spans x87 OPs.
  lrange->Spans_x87_OP_Reset();
  if (BB_SET_Intersection_Choose(lrange->Live_BB_Set(),
				 gbb_mgr.Blocks_With_x87_OP())
      != BB_SET_CHOOSE_FAILURE) {
    lrange->Spans_x87_OP_Set();
  }

  // Determine if lrange spans MMX OPs.
  lrange->Spans_mmx_OP_Reset();
  if (BB_SET_Intersection_Choose(lrange->Live_BB_Set(),
				 gbb_mgr.Blocks_With_mmx_OP())
      != BB_SET_CHOOSE_FAILURE) {
    lrange->Spans_mmx_OP_Set();
  }
}
#endif

#ifdef KEY
/////////////////////////////////////
static void
Make_Boundary_BBs_At_Border(GRA_BB* border_gbb, GRA_BB* alloc_gbb)
/////////////////////////////////////
//
//  <border_gbb> and <alloc_gbb> are neighbors on opposite sides of the border.
//  Turn them into boundary BBs.  <alloc_gbb> is in the alloc_lrange.
//
/////////////////////////////////////
{
  // Turn <border_gbb> into a boundary BB.
  if (deferred_lrange->Contains_Internal_BB(border_gbb)) {
    deferred_lrange->Remove_Internal_BB(border_gbb);
    deferred_lrange->Add_Boundary_BB(border_gbb);
  }

  // Turn <alloc_gbb> into a boundary BB.
  if (alloc_lrange->Contains_Internal_BB(alloc_gbb)) {
    alloc_lrange->Remove_Internal_BB(alloc_gbb);
    alloc_lrange->Add_Boundary_BB(alloc_gbb);
  }
}

/////////////////////////////////////
static void
Fix_Boundary_BBs(void)
/////////////////////////////////////
//
//  Make the right internal_bb sets for both alloc_lrange and deferred_lrange.
//  Walk the BBs on the (outside) border of the alloc_lrange, turning BBs on
//  both sides of the border into boundary BBs.
//
/////////////////////////////////////
{
  GRA_BB_SPLIT_LIST_ITER    iter;
  GRA_BB_FLOW_NEIGHBOR_ITER flow_iter;

  // For the internal BBs and boundary BBs originally in the live range, move
  // them either to <alloc_lrange> or <deferred_lrange>.
  for (iter.Init(alloc_gbb_list_head); ! iter.Done(); iter.Step()) {
    GRA_BB* gbb = iter.Current();

    if (deferred_lrange->Contains_Internal_BB(gbb)) {
      // Notice the sneaky fact that deferred_lrange really is split_lrange and
      // thus we can just remove any of the internal BBs in alloc_lrange and
      // the result will be the internal BBs in the deferred part.
      alloc_lrange->Add_Internal_BB(gbb);
      deferred_lrange->Remove_Internal_BB(gbb);
    } else {
      // gbb is a boundary BB.  Remove it from split_lrange and put it in
      // alloc_lrange.
      LRANGE_BOUNDARY_BB* boundary_bb =
        deferred_lrange->Remove_Boundary_Bb(gbb->Bb());
      Is_True(boundary_bb != NULL, ("Fix_Boundary_BBs: boundary BB not found"));
      alloc_lrange->Boundary_BBs_Push(boundary_bb);
    }
  }

  // Add the boundary BBs created by splitting.
  for (iter.Init(border_gbb_list_head); ! iter.Done(); iter.Step()) {
    GRA_BB* gbb = iter.Current();

    //
    // Check predecessors for members of alloc_lrange.  If found turn the
    // BBs on both sides of the border into boundary BBs.
    //
    if (BB_live_in(gbb->Bb())) {
      flow_iter.Preds_Init(gbb);
      Find_Border_Transitions(gbb,&flow_iter,Make_Boundary_BBs_At_Border);
    }

    //
    // Check successors for members of lrange0.  If found turn the
    // BBs on both sides of the border into boundary BBs.
    //
    flow_iter.Succs_Init(gbb);
    Find_Border_Transitions(gbb,&flow_iter,Make_Boundary_BBs_At_Border);
  }

  // Update the boundary BBs to reflect the BBs' new live-in and live-out info.
  deferred_lrange->Update_Boundary_BBs();
  alloc_lrange->Update_Boundary_BBs();
}
#endif

/////////////////////////////////////
static void
Check_Local_Interferences( LRANGE* lrange )
/////////////////////////////////////
//
//  Check to make sure that all BBs in <lrange> have <lrange> in their live
//  global inteferences.
//
/////////////////////////////////////
{
  LRANGE_LIVE_GBB_ITER iter;

  for (iter.Init(lrange); ! iter.Done(); iter.Step()) {
    GRA_BB* gbb = iter.Current();
    INTERFERE_ITER liter;

    if ( ! gbb->Region_Is_Complement() ) continue;

    ISA_REGISTER_CLASS rc = lrange->Rc();
    for (liter.Init(gbb->Global_Lranges(rc), gbb->Region()->Subuniverse(rc));
         ! liter.Done(); liter.Step()) {
      LRANGE* global_lrange = liter.Current();

      if ( global_lrange == lrange )
        goto NEXT_BB;
    }

    DevWarn("LRANGE for TN%d not contained in member BB%d global "
            "interference set", TN_number(lrange->Tn()), BB_id(gbb->Bb()));
  NEXT_BB:;
  }
}

/////////////////////////////////////
static INT
Choose_Best_Split(INT count)
/////////////////////////////////////
//
//  see if we found a more profitable split along the way.  note that
//  we'll find the highest priority live range with the smallest number
//  of blocks (blocks are pushed on the list as they are added, so the
//  live range gets smaller the farther we go along the list to find our
//  maximum, and we'll take the last one we find).
//
/////////////////////////////////////
{
  INT ret_count = count;

  if (GRA_choose_best_split) {
    float max_priority = -FLT_MAX;
    INT max_lunit_count = 0;
    RID* max_rid = BB_rid(alloc_gbb_list_head->Bb());
    GRA_BB *max_gbb_list = alloc_gbb_list_head;
    GRA_BB_SPLIT_LIST_ITER split_iter;

    for (split_iter.Init(alloc_gbb_list_head); ! split_iter.Done(); 
	 split_iter.Step(), --count) {
      GRA_BB* gbb = split_iter.Current();
      float priority = gbb->Split_Priority();

      if (priority >= max_priority && count > 1 &&
	  BB_rid(gbb->Bb()) == max_rid) {
	//
	// don't remove lunit from live range if previous maximum is
	// the same as the current one, i.e. don't remove references
	// from the split just to shrink the size of the live range.
	// for save_tn live ranges, on the other hand, we want to
	// construct the largest live range we can for it that has
	// the highest priority.  Every live range that might want
	// its register has been processed, so there's no point in
	// minimizing the size of the live range.  The larger the
	// live range, the better the chance that Optimize_Placement()
	// can do something with it.
	//
	if (priority == max_priority &&
	    ((max_lunit_count > gbb->Split_Lunit_Count()) ||
	     TN_is_save_reg(split_lrange->Tn()))) {
	  continue;
	}

	max_lunit_count = gbb->Split_Lunit_Count();
	max_gbb_list = gbb;
	ret_count = count;
	max_priority = priority;
      }
    }

    //
    // If best split is unprofitable, no point in continuing.  We give
    // save tn's a little more leeway in the hopes that Optimize_Placement
    // will improve the shrink wrapping.  This is really a hack (see
    // Must_Split in gra_color.cxx), but it's the best we can do since
    // we can't really tell what the ultimate spill cost will be at this
    // point (we play it conservatively with OPT_Space on, though).
    //
    if (max_priority < 0.0 && (!TN_is_save_reg(split_lrange->Tn()) ||
	OPT_Space)) {
      return -1;
    }

    // 
    // have we found a better split than the largest one?  if so, remove
    // the blocks that are no longer part of the split from alloc_lrange.
    //
    if (max_gbb_list != alloc_gbb_list_head) {
      //
      // For live ranges spanning infrequent calls, we want to throw away
      // the split if the cost is more than the cost of saving and restoring
      // a callee saved register on entry and exit.
      //
      if (split_lrange->Spans_Infreq_Call() &&
	  max_gbb_list->Split_Spill_Cost() >
	  (CGSPILL_DEFAULT_STORE_COST + CGSPILL_DEFAULT_RESTORE_COST)) {
	return -1;
      }

      for (split_iter.Init(alloc_gbb_list_head); 
	   split_iter.Current() != max_gbb_list;
	   split_iter.Step()) {
	GRA_BB* gbb = split_iter.Current();
	gbb_mgr.Split_In_Alloc_LRANGE_Clear(gbb);
	GRA_Trace_Split_Removing_Block(gbb);
      }
      alloc_gbb_list_head = max_gbb_list;
      
      // recalculate the border list
      GRA_BB_FLOW_NEIGHBOR_ITER flow_iter;
      border_gbb_list_head = NULL;
      gbb_mgr.Clear_One_Set();

      for (split_iter.Init(alloc_gbb_list_head); ! split_iter.Done();
	   split_iter.Step()) {
	GRA_BB* gbb = split_iter.Current();

	for (flow_iter.Preds_Init(gbb); ! flow_iter.Done(); flow_iter.Step()) {
	  GRA_BB* pred = flow_iter.Current();
	  if (split_lrange->Contains_BB(pred) &&
	      !gbb_mgr.Split_In_Alloc_LRANGE(pred) &&
	      !gbb_mgr.One_Set_MemberP(pred)) {
	    gbb_mgr.One_Set_Union1(pred);
	    border_gbb_list_head =
	      border_gbb_list_head->Split_List_Push(pred);
	  }
	}

	for (flow_iter.Succs_Init(gbb); ! flow_iter.Done(); flow_iter.Step()) {
	  GRA_BB* succ = flow_iter.Current();
	  if (split_lrange->Contains_BB(succ) &&
	      !gbb_mgr.Split_In_Alloc_LRANGE(succ) &&
	      !gbb_mgr.One_Set_MemberP(succ)) {
	    gbb_mgr.One_Set_Union1(succ);
	    border_gbb_list_head =
	      border_gbb_list_head->Split_List_Push(succ);
	  }
	}
      }  
    }
  }
  return (ret_count);
}

/////////////////////////////////////
static void
Split_Consistency_Check()
/////////////////////////////////////
//
//  Do extra consistency checks after splitting.  Meant to be used only as an
//  option, so expense shouldn't matter.
//
/////////////////////////////////////
{
  Check_Local_Interferences(alloc_lrange);
  Check_Local_Interferences(deferred_lrange);
}

/////////////////////////////////////
static BOOL
Compare_Priorities(float p1, float p2)
/////////////////////////////////////
//
// compare floating point values with tolerance 
//
/////////////////////////////////////
{
  if (p1 == p2) {
    return TRUE;
  }
  float max = p1 = fabs(p1);
  p2 = fabs(p2);
  if (max == 0.0 || p2 > max) {
    max = p2;
  }
  return ((fabs(p1-p2)/max) < .01);
}

/////////////////////////////////////
static BOOL
LRANGE_Do_Split( LRANGE* lrange, LRANGE_CLIST_ITER* iter,
		LRANGE**           alloc_lrange_p,
		BOOL reclaim )
/////////////////////////////////////
//  See interface description for LRANGE_Split.
/////////////////////////////////////
{
  INT32  count;
  LUNIT* maxlunit;

  split_lrange = lrange;

  if ( ( split_lrange->Type() != LRANGE_TYPE_COMPLEMENT ) ||
       ( GRA_split_lranges == FALSE ) ||
       ( TN_number(split_lrange->Tn()) == GRA_non_split_tn_id ) ) {
    return FALSE;
  }

  if (lrange->Cannot_Split())
    return FALSE;

  if (lrange->Spans_A_Setjmp()) // TODO: do not give up splitting
    return FALSE;

#ifdef TARG_IA64
  if (fat_self_recursive) return FALSE;
  GRA_Trace_Color_LRANGE("Splitting",lrange);
#endif

#ifdef KEY // don't split the saved-TNs if PU has any handler entry point
  if (lrange->Tn_Is_Save_Reg() && GRA_pu_has_handler)
    return FALSE;

  if (PU_Has_Nonlocal_Goto_Target)
    return FALSE;
#endif

  GRA_Trace_Color_LRANGE(reclaim ? "Splitting (with reclaim)" : "Splitting",
			 lrange);

  gbb_mgr.Begin_Split();

  if ( ! Max_Colorable_LUNIT(&maxlunit, reclaim) ) {
    return FALSE;
  }

  split_lrange = lrange;

  // 
  // get spill/restore costs for calculating interim profitibility of split
  //
  CGSPILL_Cost_Estimate(split_lrange->Tn(), NULL, &split_spill_cost,
			&split_restore_cost, CGSPILL_GRA);

  GRA_Trace_Split_Priority_On("Interim method");
  count = Identify_Max_Colorable_Neighborhood(maxlunit, reclaim);
  GRA_Trace_Split_Priority_Off();
  if ( count <= 1 ) {
    return FALSE;
  }

  INT new_count = Choose_Best_Split(count);

  if (new_count < 1) {
    if (split_lrange->Spans_Infreq_Call()) {
      GRA_Trace_Split(1,"LRANGE split for infrequent call failed for TN%d\n",
		      TN_number(split_lrange->Tn()));
    }
    return FALSE;
  }

  GRA_Trace_Split(1,"BB%d is maximum frequency LUNIT in alloc_lrange",
		  BB_id(maxlunit->Gbb()->Bb()));

  Divide_LRANGE();
  iter->Replace_Current(alloc_lrange);

  Add_Spills_And_Restores();
  Fix_TN_Live_Info();
  Rename_TN_References();

#ifdef KEY
  if (GRA_optimize_boundary)
    Fix_Boundary_BBs();
#endif

  deferred_lrange->Calculate_Priority();
  Fix_Interference();

  if (GRA_Trace_Check_Splits()) {
    GRA_Trace_Split_Priority_On("LRANGE_Calculate_Priority");
    alloc_lrange->Calculate_Priority();
    GRA_Trace_Split_Priority_Off();
    if (!Compare_Priorities(alloc_lrange->Priority(),
			    alloc_gbb_list_head->Split_Priority())) {
      DevWarn("Mismatch in interim(%f) and final(%f) split priorities for TN%d.  Orig block count=%d, new =%d\n",
	      alloc_gbb_list_head->Split_Priority(),
	      alloc_lrange->Priority(),
	      TN_number(alloc_lrange->Tn()), count, new_count);
    }
  }
  alloc_lrange->Priority_Set(alloc_gbb_list_head->Split_Priority());

  Add_Deferred_To_Coloring_List(iter);
  Fix_Call_Info(deferred_lrange);
  Fix_Call_Info(alloc_lrange);
  Fix_Rot_Reg_Clob_Info(deferred_lrange);
  Fix_Rot_Reg_Clob_Info(alloc_lrange);
#ifdef TARG_X8664
  Fix_x86_Info(deferred_lrange);
  Fix_x86_Info(alloc_lrange);
#endif

  if ( Get_Trace(TP_GRA,0x10) )
    Split_Consistency_Check();

  *alloc_lrange_p = alloc_lrange;

  return TRUE;
}

/////////////////////////////////////
BOOL
LRANGE_Split(LRANGE* lrange, LRANGE_CLIST_ITER* iter,
	     LRANGE**           alloc_lrange_p,
	     BOOL reclaim)
/////////////////////////////////////
//  Wrapper for LRANGE_Do_Split to clean up after
//  failure.
/////////////////////////////////////
{
  MEM_POOL_Push(&MEM_local_nz_pool);

  BOOL ret_val = LRANGE_Do_Split(lrange, iter, alloc_lrange_p, reclaim);
  if (!ret_val) {
    *alloc_lrange_p = lrange;
    if (split_lrange->Spans_Infreq_Call()) {
      //
      // If we've failed to split for a live range that spans an infrequently
      // executed call, we will try again with a callee saved register.  Reset
      // flags accordingly.
      //
      split_lrange->Spans_Infreq_Call_Reset();
      split_lrange->Spans_A_Call_Set();
    }
  }

  MEM_POOL_Pop(&MEM_local_nz_pool);
  return ret_val;
}

#ifdef TARG_X8664
/////////////////////////////////////
static void
Identify_Mixed_x87_MMX_Neighborhood()
/////////////////////////////////////
//
//  Separate out the BBs in the live range that have a mixture of x87 and MMX
//  OPs, and those that don't.
//
/////////////////////////////////////
{
  LRANGE_LIVE_GBB_ITER iter;

  border_gbb_list_head = NULL;
  alloc_gbb_list_head = NULL;

  // Use the same data structure as for normal splitting, although by "border"
  // we mean all the BBs that don't have mixed x87 and MMX OPs.  These BBs can
  // be on the border and beyond.
  for (iter.Init(split_lrange); ! iter.Done(); iter.Step()) {
    GRA_BB *gbb = iter.Current();
    if (gbb->x87_OP() && gbb->mmx_OP()) {
      gbb_mgr.Split_In_Alloc_LRANGE_Set(gbb);
      alloc_gbb_list_head = alloc_gbb_list_head->Split_List_Push(gbb);
    } else {
      border_gbb_list_head = border_gbb_list_head->Split_List_Push(gbb);
    }
  }
}

/////////////////////////////////////
BOOL
LRANGE_Split_Mixed_x87_MMX (LRANGE* lrange, LRANGE_CLIST_ITER* iter,
			    LRANGE** alloc_lrange_p)
/////////////////////////////////////
//
//  LRANGE is a x87 or MMX live range.  Split out the part containing BBs that
//  have a mixture of x87 and MMX OPs.  Return it in <alloc_lrange_p>.
//
/////////////////////////////////////
{
  INT32  count;
  LUNIT* maxlunit;

  split_lrange = lrange;

  Is_True(split_lrange->Type() == LRANGE_TYPE_COMPLEMENT,
	  ("LRANGE_Split_And_Localize_x87_MMX: lrange not complement"));
  Is_True(lrange->Rc() == ISA_REGISTER_CLASS_x87 ||
	  lrange->Rc() == ISA_REGISTER_CLASS_mmx,
	  ("LRANGE_Split_And_Localize_x87_MMX: lrange not x87 or MMX"));

  if (lrange->Cannot_Split())
    return FALSE;

  MEM_POOL_Push(&MEM_local_nz_pool);

  GRA_Trace_Color_LRANGE(
    lrange->Rc() == ISA_REGISTER_CLASS_x87 ?
      "Splitting x87 lrange to avoid clobbering MMX registers" :
      "Splitting MMX lrange to avoid clobbering x87 registers",
    lrange);

  gbb_mgr.Begin_Split();

  // Establish map from blocks with LUNITs to their LUNITs.
  LRANGE_LUNIT_ITER lunit_iter;
  for (lunit_iter.Init(split_lrange); !lunit_iter.Done(); lunit_iter.Step()) {
    LUNIT *lunit = lunit_iter.Current();
    gbb_mgr.Split_LUNIT_Set(lunit->Gbb(), lunit);
  }

  split_lrange = lrange;

  Identify_Mixed_x87_MMX_Neighborhood();

  Divide_LRANGE();
  iter->Replace_Current(alloc_lrange);

  Add_Spills_And_Restores();
  Fix_TN_Live_Info();
  Rename_TN_References();

  if (GRA_optimize_boundary)
    Fix_Boundary_BBs();

  deferred_lrange->Calculate_Priority();
  Fix_Interference();

  Add_Deferred_To_Coloring_List(iter);
  Fix_Call_Info(deferred_lrange);
  Fix_Rot_Reg_Clob_Info(deferred_lrange);
  Fix_x86_Info(deferred_lrange);

  Is_True(!(deferred_lrange->Spans_x87_OP() && deferred_lrange->Spans_mmx_OP()),
	  ("LRANGE_Split_Mixed_x87_MMX: "
	   "lrange contains both x87 and MMX after splitting"));

  if ( Get_Trace(TP_GRA,0x10) )
    Split_Consistency_Check();

  *alloc_lrange_p = alloc_lrange;

  MEM_POOL_Pop(&MEM_local_nz_pool);

  return TRUE;
}
#endif

#ifdef KEY
/////////////////////////////////////
void
LRANGE_Split_Reclaimed_BBs (LRANGE *lrange, REGISTER reg)
/////////////////////////////////////
//
//  Reclaim <reg> over <lrange>'s BBs, for the purpose of allocating <reg> to
//  <lrange>.
//
/////////////////////////////////////
{
  LRANGE *from_lrange = NULL;		// Reclaim register from this lrange.
  LRANGE_LIVE_GBB_ITER gbb_iter;

  // Find the lrange that REG was originally allocated to.
  //
  // (Cannot use GRA_BB_LIVE_OUT_LRANGE_ITER to get GBB's live-out lranges,
  // which can then be searched to find the lrange holding REG.
  // GRA_BB_LIVE_OUT_LRANGES_ITER relies on BB_live_out which is not updated to
  // include the alloc'd part of a split lrange (alloc_lrange).)
  gbb_iter.Init(lrange);
  GRA_BB *gbb = gbb_iter.Current();
  from_lrange = gbb->Get_LRANGE_Owner(lrange->Rc(), reg);

  FmtAssert(from_lrange != NULL,
	    ("Do_Register_Reclaim: cannot find from_lrange"));

#ifdef Is_True_On
  Is_True(from_lrange->Allocated(),
	  ("Do_Register_Reclaim: the reclaimed lrange is not allocated"));
  BB_SET *reclaimed_BBs = BB_SET_Create_Empty(PU_BB_Count + 2,GRA_pool);

  // The BBs to be reclaimed should be a subset of the BBs contained in
  // FROM_LRANGE.
  for (gbb_iter.Init(lrange); !gbb_iter.Done(); gbb_iter.Step()) {
    GRA_BB *gbb = gbb_iter.Current();
    Is_True(from_lrange->Contains_BB(gbb),
	    ("LRANGE_Split_Reclaimed_BBs: BB not in from_lrange"));
    reclaimed_BBs = BB_SET_Union1D(reclaimed_BBs, gbb->Bb(), GRA_pool);
  }
  
  // FROM_LRANGE should not have any references in the reclaimed BBs.  Test
  // this by making sure FROM_LRANGE has no lunit for those BBs.
  LRANGE_LUNIT_ITER lunit_iter;
  for (lunit_iter.Init(from_lrange); !lunit_iter.Done(); lunit_iter.Step()) {
    LUNIT *lunit = lunit_iter.Current();
    Is_True(!BB_SET_MemberP(reclaimed_BBs, lunit->Gbb()->Bb()),
	 ("LRANGE_Split_Reclaimed_BBs: lrange has reference in reclaimed BBs"));
  }
#endif

  // Split out the reclaimed BBs from FROM_LRANGE.
  for (gbb_iter.Init(lrange); !gbb_iter.Done(); gbb_iter.Step()) {
    GRA_BB *gbb = gbb_iter.Current();
    GRA_BB_FLOW_NEIGHBOR_ITER flow_iter;

    gbb->Remove_Live_In_LRANGE(from_lrange);
    gbb->Remove_Live_Out_LRANGE(from_lrange);

    // Insert spills at the outside boundary of the reclaimed region.
    for (flow_iter.Preds_Init(gbb); !flow_iter.Done(); flow_iter.Step()) {
      GRA_BB *pred = flow_iter.Current();
      if (!lrange->Contains_BB(pred) &&
	  pred->Is_Live_Out_LRANGE(from_lrange)) {
	// PRED is not in the reclaimed region.  Insert spill at the end of
	// PRED.
	LUNIT *lunit;
	if (!from_lrange->Find_LUNIT_For_GBB(pred, &lunit)) {
	  lunit = LUNIT_Create(from_lrange, pred);
	  pred->Make_Register_Referenced(from_lrange->Rc(), from_lrange->Reg(),
					 from_lrange);
	}
	GRA_Note_Save_Below(lunit);
      }
      // FROM_LRANGE is no longer live out of PRED if PRED does not have any
      // successor that is outside of the reclaimed region.
      if (!Has_Live_In_Successor(pred, from_lrange))
	pred->Remove_Live_Out_LRANGE(from_lrange);
    }

    // Insert restores at the outside boundary of the reclaimed region.
    for (flow_iter.Succs_Init(gbb); !flow_iter.Done(); flow_iter.Step()) {
      GRA_BB *succ = flow_iter.Current();
      if (!lrange->Contains_BB(succ) &&
	  succ->Is_Live_In_LRANGE(from_lrange)) {
	// SUCC is not in the reclaimed region.  Insert restore at the
	// beginning of SUCC.
	LUNIT *lunit;
	if (!from_lrange->Find_LUNIT_For_GBB(succ, &lunit)) {
	  lunit = LUNIT_Create(from_lrange, succ);
	  succ->Make_Register_Referenced(from_lrange->Rc(), from_lrange->Reg(),
					 from_lrange);
	}
	GRA_Note_Restore_Above(lunit);

	// Must insert store at each of SUCC's live-out predecessors.
	GRA_BB_FLOW_NEIGHBOR_ITER iter;
	for (iter.Preds_Init(succ); !iter.Done(); iter.Step()) {
	  GRA_BB *pred = iter.Current();
	  if (pred->Is_Live_Out_LRANGE(from_lrange)) {
	    if (!from_lrange->Find_LUNIT_For_GBB(pred, &lunit)) {
	      lunit = LUNIT_Create(from_lrange, pred);
	      pred->Make_Register_Referenced(from_lrange->Rc(),
					     from_lrange->Reg(), from_lrange);
	    }
	    GRA_Note_Save_Below(lunit);
	  }
	}
      }
    }
  }
}
#endif
