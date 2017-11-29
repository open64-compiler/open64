/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
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


// =======================================================================
// =======================================================================
//
//  Module: igls.cxx
//  $Revision: 1.15 $
//  $Date: 05/12/05 08:59:08-08:00 $
//  $Author: bos@eng-24.pathscale.com $
//  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.igls.cxx $
//
//  Description:
//  ============
//
//  Integrated Global and Local Scheduling Framework (IGLS). The main driver 
//  controls the execution of the local scheduling phase (LOCS), the
//  hyperblock scheduling phase (HBS) and the global scheduling phase (GCM).
//
// =======================================================================
// =======================================================================

#include <stdint.h>
#include <alloca.h>
#include <math.h>
#include "defs.h"
#include "config.h"
#include "config_targ_opt.h"
#include "mempool.h"
#include "bb.h"
#include "bb_set.h"
#include "tracing.h"
#include "timing.h"
#include "cgir.h"
#include "glob.h"
#include "tn_map.h"
#include "cg.h"
#include "cg_flags.h"
#include "ercg.h"
#include "cgtarget.h"
#include "cg_vector.h"
#include "dominate.h"
#include "findloops.h"
#include "note.h"
#include "lra.h"
#include "gcm.h"
#include "ti_res.h"
#include "ti_res_res.h"
#include "ti_latency.h"
#include "ti_errors.h"
#include "cg_region.h"
#include "gtn_universe.h"
#include "gtn_set.h"
#include "cxx_memory.h"
#include "hb_sched.h"
#include "hb_hazards.h"
#include "targ_proc_properties.h"
#include "be_util.h"

#ifdef TARG_IA64
#include "bb.h"
#include "op.h"
#endif

#if defined (TARG_SL)
/* This is for binary search, to determine a PU should be skipped
 * by GCM
 */
static bool GCM_Should_Skip( int puid )
{
  if( puid == CG_GCM_skip_equal )
    return true;

  /* we don't set skip-before and skip-after, or we set skip-equal
   * but the puid is not equal to skip-equal
   */
  if( CG_GCM_skip_before < 0 && CG_GCM_skip_after < 0 )
    return false;

  /* we have only set skip-after */
  if( CG_GCM_skip_before < 0 ) {
    if( puid > CG_GCM_skip_after )
      return true;
  }

  /* we have only set skip-before */
  if( CG_GCM_skip_after < 0 ) {
    if( puid < CG_GCM_skip_before )
      return true;
  }

  /* we set both skip-after and skip-before.
   * 1. skip_before <= skip_after, there are no intersections, so there
   *    are two different sub-sections to check.
   * 2. skip_before > skip_after, there is a section, so only number in the
   *    intersection will be skipped.
   */
  if( CG_GCM_skip_before <= CG_GCM_skip_after ) {
    if( puid < CG_GCM_skip_before || puid > CG_GCM_skip_after )
      return true;
    else
      return false;
  } else {
    if( puid < CG_GCM_skip_before && puid > CG_GCM_skip_after )
      return true;
    else
      return false;
  }
}
#endif

#ifdef KEY
// Delete prefetches that are dropped by the scheduler.
static void
Delete_Unscheduled_Prefetches (BB *bb)
{
  OP *op, *next_op;
  int offset = -1;

  for (op = BB_first_op(bb); op != NULL; op = next_op) {
    next_op = OP_next(op);
    if (OP_prefetch_deleted(op)) {
      FmtAssert(OP_prefetch(op),
		("Delete_Unscheduled_Prefetches: OP not a prefetch"));
      BB_Remove_Op(bb, op);
    } else {
      // The new cycle 0 is at the first non-deleted instruction.
      if (offset == -1) {
	offset = OP_scycle(op);
      }

      // Adjust scycle to account for the deleted cycles.
      OP_scycle(op) = OP_scycle(op) - offset;
    }
  }
}

static void
Run_One_Sched (HB_Schedule *Sched, BOOL is_fwd, BB *bb, HBS_TYPE hbs_type,
	       const char *name, INT32 *best_cycles, const char **best_name)
{
  Sched->Init(bb, hbs_type, OP_scycle(BB_last_op(bb))+1, NULL, NULL);
  Sched->Schedule_BB(bb, NULL, is_fwd);
  if ((OP_scycle(BB_last_op(bb)) + 1) < *best_cycles) {
    *best_cycles = OP_scycle(BB_last_op(bb)) + 1;
    *best_name = name;
  }
}

// Run the BB scheduler.  Run it multiple times if necessary to pick the best
// schedule.
static void
Run_Sched (HB_Schedule *Sched, BB *bb, HBS_TYPE hbs_type, INT32 max_sched)
{
  INT32 best_cycles = INT32_MAX;
  const char *best_name = NULL;

  if (LOCS_Best) {

    // Run scheduler many times.

    HBS_TYPE base_hbs_type = hbs_type
			     & ~HBS_BALANCE_READY_TYPES
			     & ~HBS_BALANCE_UNSCHED_TYPES;
    
    Sched->Init(bb, hbs_type, max_sched, NULL, NULL);
    Sched->Schedule_BB(bb, NULL, 0);	// backward scheduling
    best_name = "backward";

    if (BB_length(bb) > 1) {
      best_cycles = OP_scycle(BB_last_op(bb)) + 1;

      // backward scheduling

      Run_One_Sched(Sched, 0, bb, base_hbs_type | HBS_BALANCE_READY_TYPES,
		    "backward balance_ready_types",
		    &best_cycles, &best_name);

      Run_One_Sched(Sched, 0, bb, base_hbs_type | HBS_BALANCE_UNSCHED_TYPES,
		    "backward balance_unsched_types",
		    &best_cycles, &best_name);

      Run_One_Sched(Sched, 0, bb,
		    base_hbs_type | HBS_BALANCE_UNSCHED_TYPES |
		      HBS_BALANCE_READY_TYPES,
		    "backward balance_unsched_types balance_ready_types",
		    &best_cycles, &best_name);

      // forward scheduling

      Run_One_Sched(Sched, 1, bb, base_hbs_type 
#if defined (KEY)
                    & ~HBS_DROP_UNSCHED_PREFETCHES
#endif
		    , "forward", &best_cycles, &best_name);
    }

  } else if (LOCS_Scheduling_Algorithm == 2) {

    // Do forward and backward scheduling.

    Sched->Init(bb, hbs_type, max_sched, NULL, NULL);
    Sched->Schedule_BB(bb, NULL, 0);	// backward scheduling
    best_name = "backward";

    if (BB_length(bb) > 1) {
      best_cycles = OP_scycle(BB_last_op(bb)) + 1;

      Run_One_Sched(Sched, 1, bb, hbs_type, "forward",
		    &best_cycles, &best_name);
    }

  // Run scheduler once.
  } else {
    Sched->Init(bb, hbs_type, max_sched, NULL, NULL);
    Sched->Schedule_BB(bb, NULL, LOCS_Scheduling_Algorithm);
  }

#ifdef KEY
  // Delete prefetches that didn't fit into idle issue slots.
  if (hbs_type & HBS_DROP_UNSCHED_PREFETCHES)
    Delete_Unscheduled_Prefetches(bb);
#endif

  if (Trace_HB &&
      best_name != NULL) {
    fprintf(TFile, "Best scheduling heuristic: %s\n", best_name);
  }
}
#endif

// ======================================================================
// IGLS_Schedule_Region 
//
// The main driver for invoking all the scheduling phases in CG. They mainly
// include HBS (for single-BBs and hyperblocks) and GCM. The data-speculation
// phase is also invoked here since it's tied very closely with the 
// scheduling phase. 
// The <before_regalloc> parameter indicates whether the scheduler is being 
// invoked before or after register allocation. The amount of work done by 
// the various phases depends on the optimization level. 
//
// -O0 : insert noops to remove hazards.
// -O1 : perform HBS scheduling for local BBs (ONLY) after register allocation.
//       fill branch delay slot nops (for MIPS).
// -O2 : perform hyperblock(s) scheduling before register allocation.
//       provide accurate register estimates for GRA.
//       invoke post-GRA global scheduling (post-GCM) phase
//       invoke THR phase to perform data-speculation (after register-
//       allocation).
// -O3 : perform hyperblock(s) scheduling before register allocation.
//	 provide accurate register estimates for GRA/GCM.
//	 invoke pre-GRA global scheduling (pre-GCM) phase.
//	 invoke post-GRA global scheduling (post-GCM) phase
//
// ======================================================================
void
IGLS_Schedule_Region (BOOL before_regalloc)
{
  HBS_TYPE hbs_type;
  /* for SL, we only use GCM_Schedule_Region to do global instruction 
   * scheduling, we don't need the other componnets here.
   */
#ifdef TARG_SL
  /* This is for binary search */
  int current_pu = Current_PU_Count();
  if( GCM_Should_Skip(current_pu) ){
    printf(" ... PU:%d is skipped by GCM\n", current_pu );
    return;
  }

  if( before_regalloc ){
    hbs_type = HBS_BEFORE_GRA | HBS_BEFORE_LRA | HBS_DEPTH_FIRST;
  } else {
    hbs_type = HBS_CRITICAL_PATH;
  }
  GCM_Schedule_Region( hbs_type );
  return ;
#endif

  BB *bb;
  BOOL should_we_local_schedule;  // controls local scheduling (single BBs).
  BOOL should_we_global_schedule; // controls HB scheduling and GCM.
  BOOL should_we_schedule;        // controls all scheduling (LOCS,  HBS, GCM)
  BOOL should_we_do_thr;          // controls the THR phase in CG.

  RID *rid;
  HB_Schedule *Sched = NULL;
  CG_THR      *thr = NULL;

  Set_Error_Phase ("Hyperlock Scheduler");
  Start_Timer (T_Sched_CU);
  Trace_HB = Get_Trace (TP_SCHED, 1);
  should_we_schedule = IGLS_Enable_All_Scheduling;
  should_we_do_thr = CG_enable_thr;
  L_Save();

  // 12581: In "main" entry, always keep spadjust at top, so that debugging
  // info and ctrl register setup occur in correct order.
  if (!strcmp(Cur_PU_Name, "MAIN__") || !strcmp(Cur_PU_Name, "main")) {
    Set_BB_scheduled(REGION_First_BB);
  }

  if (before_regalloc) {

    // schedule if (-O > O1) and
    // -CG:local_sched=on && -CG:pre_local_sched=on.
    should_we_local_schedule = (   CG_opt_level > 1
				   && LOCS_Enable_Scheduling
				   && LOCS_PRE_Enable_Scheduling);

    // global schedule if (-O > O2) and either of the following below are true.
    // -CG:hb_sched=on && -CG:pre_hb_sched=on (hyperblock scheduling).
    // -CG:gcm=on && -CG:pre_gcm=on for GCM.
    should_we_global_schedule = ( CG_opt_level > 2 &&
				  ((IGLS_Enable_HB_Scheduling &&
				    IGLS_Enable_PRE_HB_Scheduling) ||
				   (GCM_PRE_Enable_Scheduling &&
				    GCM_Enable_Scheduling)));

    hbs_type = HBS_BEFORE_GRA | HBS_BEFORE_LRA | HBS_DEPTH_FIRST;
#ifdef KEY
    if (LOCS_Balance_Ready_Types)
      hbs_type |= HBS_BALANCE_READY_TYPES;
    if (LOCS_Balance_Unsched_Types)
      hbs_type |= HBS_BALANCE_UNSCHED_TYPES;
#endif
    if (Trace_HB) {
      #pragma mips_frequency_hint NEVER
      fprintf (TFile, "***** HYPERBLOCK SCHEDULER (before GRA) *****\n");
    }
  }
  else {

    // schedule if (-O > O0) and
    // -CG:local_sched=on && -CG:post_local_sched=on.
    should_we_local_schedule = (   CG_opt_level > 0
				   && LOCS_Enable_Scheduling
				   && LOCS_POST_Enable_Scheduling);

    // global schedule if (-O > O1) and either of the following below are true.
    // -CG:hb_sched=on && -CG:post_hb_sched=on (hyperblock scheduling).
    // -CG:gcm=on && -CG:post_gcm=on for GCM.
    should_we_global_schedule = ( CG_opt_level > 1 &&
				  ((IGLS_Enable_HB_Scheduling &&
				   (IGLS_Enable_POST_HB_Scheduling ||
				    IGLS_Enable_PRE_HB_Scheduling)) ||
				   (GCM_Enable_Scheduling &&
				    GCM_POST_Enable_Scheduling)));
    hbs_type = HBS_CRITICAL_PATH;
    if (PROC_has_bundles()) hbs_type |= HBS_MINIMIZE_BUNDLES;

#ifdef KEY
    if (LOCS_Balance_Ready_Types)
      hbs_type |= HBS_BALANCE_READY_TYPES;
    if (LOCS_Balance_Unsched_Types)
      hbs_type |= HBS_BALANCE_UNSCHED_TYPES;
#endif

    // allow data-speculation if (-O > O1) and -OPT:space is turned off.
    should_we_do_thr = should_we_do_thr && (CG_opt_level > 1) && !OPT_Space;

    if (Trace_HB) {
      #pragma mips_frequency_hint NEVER
      fprintf (TFile, "***** HYPERBLOCK SCHEDULER (after GRA) *****\n");
    }
  }

  // Before register allocation:
  // - Do hyperblock scheduling first to get perfect schedules at each
  //   hyperblock level (register-sensitive). 
  // - Do GCM next to extract global parallelism. Some work needs to be
  //   done, so that it strictly enforces hyperblock boundaries.
  // - Do local scheduling for BBs which are not part of any hyperblocks.

  if (before_regalloc) {
    if (!should_we_schedule) return;

    // Do HB scheduling for all HBs generated (before register allocation).
    if (IGLS_Enable_HB_Scheduling && IGLS_Enable_PRE_HB_Scheduling &&
	should_we_global_schedule) {
      HB_Remove_Deleted_Blocks();
      std::list<HB*>::iterator hbi;
      FOR_ALL_BB_STLLIST_ITEMS_FWD(HB_list, hbi) {
	if (!Sched) {
	  Sched = CXX_NEW(HB_Schedule(), &MEM_local_pool);
	}

	// Check to see if not SWP'd.
	std::list<BB*> hb_blocks;
	Get_HB_Blocks_List(hb_blocks,*hbi);
	if (Can_Schedule_HB(hb_blocks)) {
	  Sched->Init(hb_blocks, hbs_type, NULL);
	  Sched->Schedule_HB(hb_blocks);
	}
      }
    }

    // Try GCM (before register allocation).
    if (GCM_Enable_Scheduling && should_we_global_schedule) {
	Stop_Timer (T_Sched_CU);

	GCM_Schedule_Region (hbs_type);

	Set_Error_Phase ("Hyperblock Scheduler (HBS)");
	Start_Timer (T_Sched_CU);
    }

    if (!should_we_local_schedule) return;

    // Do local scheduling for BBs which are not part of HBs. 
    // (before register allocation).
    for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
      
      if (    ( rid = BB_rid(bb) )
	      && ( RID_level(rid) >= RL_CGSCHED ) )
	continue;
      
      if (!BB_scheduled(bb)) {
	if (!Sched) {
	  Sched = CXX_NEW(HB_Schedule(), &MEM_local_pool);
	}
#ifdef KEY
	Run_Sched(Sched, bb, hbs_type, INT32_MAX);
#else
	Sched->Init(bb, hbs_type, INT32_MAX, NULL, NULL);
	Sched->Schedule_BB(bb, NULL, LOCS_Scheduling_Algorithm);
#endif
      }
    }
  }
  else {

    // After register allocation:
    // - Perform data-speculation first, since it will expose more 
    //   parallelism and scheduling opportunities at the block level.
    // - Do hyperblock scheduling next to get perfect schedules at each
    //   hyperblock level (parallelism-driven).
    // - Do GCM next to extract global parallelism. Some work needs to be
    //   done, so that it strictly enforces hyperblock boundaries.
    // - Do local scheduling for BBs which are not part of any hyperblocks.

    // Perform data-speculation first, since it will expose parallelism
    // and scheduling opportunities at the block level.
    // TODO: Invoke data-speculation phase before register allocation,
    // requires GRA spill support, and conditionally invoke the phase
    // after register allocation.

#ifdef KEY
    // Drop prefetches that can't be scheduled into an unused issue slot.
    if (LOCS_Reduce_Prefetch)
      hbs_type |= HBS_DROP_UNSCHED_PREFETCHES;
#endif

    if (should_we_do_thr) {
      Stop_Timer (T_Sched_CU);
      Set_Error_Phase ("Tree-Height Reduction (THR)");
      Start_Timer (T_THR_CU);

      for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
	if (    ( rid = BB_rid(bb) )
		&& ( RID_level(rid) >= RL_CGSCHED ) )
	  continue;

	// Perform data-speculation (if profitable).
	// Avoid processing SWP scheduled blocks, all other scheduled blocks
	// are still considered as candidates for THR.

	if (BB_scheduled(bb) && !BB_scheduled_hbs(bb)) continue;
	if (!thr) {
	  thr = CXX_NEW(CG_THR(), &MEM_local_pool);
	}
	thr->Init(bb, THR_DATA_SPECULATION_NO_RB, FALSE);
	thr->Perform_THR();
	
      } /* for (bb= REGION_First_BB).. */

      Stop_Timer (T_THR_CU);
      Check_for_Dump (TP_THR, NULL);
      Start_Timer (T_Sched_CU);

    } /* should_we_do_thr */

    // Do HB scheduling for all HBs generated (after register allocation).
    if (IGLS_Enable_HB_Scheduling && IGLS_Enable_POST_HB_Scheduling &&
	should_we_schedule && should_we_global_schedule) {

      HB_Remove_Deleted_Blocks();
      std::list<HB*>::iterator hbi;
      FOR_ALL_BB_STLLIST_ITEMS_FWD(HB_list, hbi) {
	if (!Sched) {
	  Sched = CXX_NEW(HB_Schedule(), &MEM_local_pool);
	}
	// Check to see if not SWP'd.
	std::list<BB*> hb_blocks;
	Get_HB_Blocks_List(hb_blocks,*hbi);
	if (Can_Schedule_HB(hb_blocks)) {
	  Sched->Init(hb_blocks, hbs_type, NULL);
	  Sched->Schedule_HB(hb_blocks);
	}
      }
    }

    // Try GCM for the region (after register allocation).
    if (GCM_Enable_Scheduling && should_we_schedule &&
	should_we_global_schedule) {
	Stop_Timer (T_Sched_CU);

 	GCM_Schedule_Region (hbs_type);

        Set_Error_Phase ("Hyperblock Scheduler (HBS)");
	Start_Timer (T_Sched_CU);
    }

    // Do local scheduling for BBs which are not part of HBs. 
    // (after register allocation).
    for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
      if (    ( rid = BB_rid(bb) )
	      && ( RID_level(rid) >= RL_CGSCHED ) )
	continue;

      BOOL skip_bb = BB_scheduled(bb) && !BB_scheduled_hbs(bb);

      if (should_we_do_thr && !skip_bb) Remove_Unnecessary_Check_Instrs(bb);

#ifdef KEY_1873
      /* The original code with Reschedule_BB is meanlingless. I think the original
	 author meant BB_scheduled(bb), not Reschedule_BB(bb).
      */
      const BOOL resched = FALSE;
#else
      BOOL resched = !skip_bb && Reschedule_BB(bb); /* FALSE; */      
#endif // KEY
      if (should_we_schedule && should_we_local_schedule &&
	  (!skip_bb || resched)) {
#ifdef TARG_IA64
        extern void Clean_Up (BB* bb);
        Clean_Up(bb); 
        Reset_BB_scheduled(bb);  
#endif  
	// TODO: try locs_type = LOCS_DEPTH_FIRST also.
	INT32 max_sched = (resched) ?  OP_scycle(BB_last_op(bb))+1 : INT32_MAX;
#ifdef KEY
	// Reschedule if new OPs were added since the last time the BB was
	// scheduled.  This includes spill OPs added by the register allocator.
	if (max_sched < INT32_MAX) {
	  OP *op;
	  FOR_ALL_BB_OPs_FWD(bb, op) {
	    if (OP_scycle(op) == 0) {
	      max_sched = INT32_MAX;
	      break;
	    }
	  }
	}
#endif
	if (LOCS_Enable_Scheduling) {
	  if (!Sched) {
	    Sched = CXX_NEW(HB_Schedule(), &MEM_local_pool);
	  }
#ifdef KEY
	  Run_Sched(Sched, bb, hbs_type, max_sched);
#else
	  Sched->Init(bb, hbs_type, max_sched, NULL, NULL);
	  Sched->Schedule_BB(bb, NULL, LOCS_Scheduling_Algorithm);
#endif
	}
      }
      Handle_All_Hazards (bb);
    } /* for (bb= REGION_First_BB).. */

#ifdef TARG_X8664
    if (Is_Target_Orochi() == FALSE)
    {
      extern void CG_Sched( MEM_POOL*, BOOL );
      CG_Sched( &MEM_local_pool, Get_Trace( TP_SCHED, 1 ) );
    }
#endif

    // Do branch optimizations here.
    if (should_we_schedule && should_we_local_schedule) {
      if (GCM_Enable_Scheduling) GCM_Fill_Branch_Delay_Slots ();
      if (Assembly) Add_Scheduling_Notes_For_Loops ();
    }
  }

  // need to explicitly delete Sched and thr
  // so that destructors are called.
  if (Sched) {
	CXX_DELETE(Sched, &MEM_local_pool);
  }
  if (thr) {
	CXX_DELETE(thr, &MEM_local_pool);
  }
  L_Free();

  Check_for_Dump (TP_SCHED, NULL);
  Stop_Timer (T_Sched_CU);
}


