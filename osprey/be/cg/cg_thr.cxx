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


// =======================================================================
// =======================================================================
//
//  Module: cg_thr.cxx
//  $Revision: 1.9 $
//  $Date: 05/12/05 08:59:04-08:00 $
//  $Author: bos@eng-24.pathscale.com $
//  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.cg_thr.cxx $
//
//  Description:
//  ============
//
//  CG Tree Height Reduction Routines.
//
// =======================================================================
// =======================================================================

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
#include "cg_thr.h"
#include "ercg.h"
#include "cgtarget.h"
#include "cxx_memory.h"
#include "cg_spill.h"
#include "targ_isa_enums.h"

// ======================================================================
// Declarations (macros, variables)
// ======================================================================

#define OP_visited_thr(o)                      OP_flag1(o)
#define Set_OP_visited_thr(o)                  Set_OP_flag1(o)
#define Reset_OP_visited_thr(o)                Reset_OP_flag1(o)

BOOL Trace_THR = FALSE;

// ======================================================================
// Remove_Unnecessary_Check_Instrs
//
// Remove all unnecessary check instructions prior inserted. The reason
// they become unnecessary is because the advanced load never moves past
// the store instruction.
//
// ======================================================================
void
Remove_Unnecessary_Check_Instrs(BB *bb)
{
  OP *op;

  FOR_ALL_BB_OPs_REV(bb, op) {

    // Check if this is a check load.
    if (OP_load(op) && CGTARG_Is_OP_Check_Load(op)) {
      OP *prev_op;
      BOOL store_present = FALSE;
      
      // Trace brackwards to find a matching advanced-load with no
      // intervening store(s) instructions present.
      
      for (prev_op = OP_prev(op); prev_op; prev_op = OP_prev(prev_op)) {
	
	if (OP_store(prev_op)) {store_present = TRUE; break; }

	if (OP_load(prev_op) &&
	    CGTARG_Is_OP_Advanced_Load(prev_op) && !store_present) {

	  CGTARG_Perform_THR_Code_Generation(prev_op, op, 
					THR_DATA_SPECULATION_NO_RB_CLEANUP);

	  Reset_BB_scheduled(bb);  // Need to reschedule the BB.
	} /* CGTARG_Is_OP_Advanced_Load */
      } /* for (..)  */
    } /* OP_load(op) && ... */
  } /* FOR_ALL_BB_OPs_REV */
}

// ======================================================================
// CG_THR::Check_THR_Profitability
//
// This routine determines profitability of each THR decision. 
// (1) It first creates a list of all arc of interest for the specific THR_TYPE
// (2) It then processes the arc_list of interest and checks profitabilty.
// (3) if profitable, it updates the _candidate_list, else resets the
//     ARC_is_dotted flag.
//
// ======================================================================
void
CG_THR::Check_THR_Profitability ()
{
  OP *op;
  std::list<ARC*> arc_list;
  ARC *arc;

  _candidate_list.clear();  // empty the list
  // Create a list of all arcs of interest.
  FOR_ALL_BB_OPs_FWD (_bb, op) {
    Reset_OP_visited_thr(op);
    if (_thr_type & THR_DATA_SPECULATION_NO_RB) {
      if (OP_store(op)) {

	ARC_LIST *arcs;
	for (arcs = OP_succs(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
	  arc = ARC_LIST_first(arcs);
	  OP *succ_op = ARC_succ(arc);

	  // If <arc> is store-load MEMIN dotted dependence, add it to
	  // the try_list. Exclude all <spill-load> ops.
	  if (ARC_is_dotted(arc) && OP_load(succ_op) && 
	      !OP_mem_fill_type(succ_op))
	    arc_list.push_back(arc);
	}
      }
    }
  }

  // Process the list of arcs of interest to check profitability.
  // if THR profitable, update the _candidate_list, otherwise reset the
  // ARC_is_dotted flag.

  std::list<ARC*>::iterator arc_iter;
  for (arc_iter = arc_list.begin(); arc_iter != arc_list.end(); arc_iter++) {
    if (_thr_type & THR_DATA_SPECULATION_NO_RB) {
      OP *pred_op = ARC_pred(*arc_iter);
      OP *succ_op = ARC_succ(*arc_iter);

      if (OP_Has_Restrictions(pred_op,succ_op,_thr_before_regalloc)) continue;

      OPSCH *pred_opsch = OP_opsch(pred_op, _thr_map);
      OPSCH *succ_opsch = OP_opsch(succ_op, _thr_map);

      INT old_pred_ltime = OPSCH_lstart (pred_opsch);
      INT old_succ_ltime = OPSCH_lstart (succ_opsch);

      INT new_succ_etime = old_pred_ltime + ARC_latency(*arc_iter);

      if (new_succ_etime > old_succ_ltime) {
	_candidate_list.push_back(*arc_iter);
      } else {
       Set_ARC_is_dotted(arc, FALSE);
      }
    }
  }
}

// ======================================================================
// CG_THR::Perform_THR_Code_Generation
//
// This routine performs all THR (and target-specific) code generation
// tasks once profitability has been determined.
//
// ======================================================================
void
CG_THR::Perform_THR_Code_Generation ()
{
  if (_thr_type & THR_DATA_SPECULATION_NO_RB) {
    std::list<ARC*>::iterator arc_iter;
    for (arc_iter = _candidate_list.begin(); 
	 arc_iter != _candidate_list.end(); arc_iter++) {

      OP *succ_op = ARC_succ(*arc_iter);
      if (OP_visited_thr(succ_op)) continue;

      CGTARG_Perform_THR_Code_Generation(succ_op, NULL, _thr_type);
      Set_OP_visited_thr(succ_op);
      _chk_instr_inserted = TRUE;
    }
  }
}

// ======================================================================
// CG_THR::OP_Has_Restrictions
//
// Check to see if any of the concerned OPs (pred_op, succ_op) have any
// restrictions in performing the necessary THR_TYPE action.
//
// ======================================================================
BOOL
CG_THR::OP_Has_Restrictions(OP *pred_op, OP *succ_op, BOOL before_regalloc)
{
  if (_thr_type & THR_DATA_SPECULATION_NO_RB) {
    if (!before_regalloc && OP_load(succ_op)) {
      INT succ_base_idx = TOP_Find_Operand_Use(OP_code(succ_op), OU_base);
      TN *succ_base_tn = OP_opnd(succ_op, succ_base_idx);
      TN *succ_result_tn = OP_result(succ_op, 0);

      if (TNs_Are_Equivalent(succ_result_tn, succ_base_tn)) return TRUE;
    }

#if !defined(TARG_MIPS) && !defined(TARG_X8664) && !defined(TARG_PPC32)
    //TODO: Need to add support for post-increment loads as well.
    if (OP_load(succ_op) && 
	TOP_Find_Operand_Use(OP_code(succ_op), OU_postincr) >= 0)
      return TRUE;
#endif
  }

  return FALSE;
}

// ======================================================================
// CG_THR
//
// CG Tree-Height Reduction Interface:
//
// ======================================================================
void
CG_THR::Perform_THR()
{

  //
  // Data Speculation Support.
  //
  // 1) Build the dependence graph.
  //
  // 2) Relax all store/load memory dependences which have been marked as
  //    non-definite. Calculate the Etime/Ltime for every OP in the graph after
  //   this relaxation (marked as dotted edges). This presents the best case
  //   scenario.
  //
  // 3) For every dotted edge, compute the Etime of the successor node with 
  //    the relaxed dependence now added back. If the resultant Etime is 
  //    greater than its Ltime, then adding the dependence slows down the 
  //    net graph; hence mark the dotted edge as relaxed dependence. 
  //    Otherwise, mark it as  unrelaxed.
  //
  // 4) Insert appropriate check instruction for every such dotted relaxed
  //     dependence. Convert all dotted unrelaxed edges as real edges.
  //
  // 5) Now, mark all register dependencies emanating out from the check
  //    instruction (i.e all prior forward uses of the preload) as dotted 
  //    edges. This is to allow flow dependent instructions of the preload 
  //    to bypass the check. Repeat the above steps to decide when to treat 
  //    these dotted edges as relaxed edges or real edges.
  //
  // 6) Insert appropriate recovery block code for every such dotted edge which
  //    gets relaxed.
  //

  if (_thr_type & THR_DATA_SPECULATION_NO_RB) {
    if (BB_length(_bb) > 1) {

      // Step (1) and Step (2). Treat all store-load dependences as dotted
      // edges. This presents the best case scenario.
      CG_DEP_Compute_Graph(
			   _bb,
			   INCLUDE_ASSIGNED_REG_DEPS,
			   NON_CYCLIC,
			   NO_MEMREAD_ARCS,
#ifndef TARG_MIPS
			   (Is_Target_Itanium()) ? NO_MEMIN_ARCS :
#endif
			                           INCLUDE_MEMIN_ARCS,
#ifndef TARG_MIPS
			   (Is_Target_Itanium()) ? INCLUDE_CONTROL_ARCS :
#endif
			                           NO_CONTROL_ARCS,
			   NULL);

      // Step (2). Compute the Etimes and Ltimes for the BB graph with the
      // dotted dependences.
      Compute_OPSCH (_bb, _thr_map, &_thr_pool);

      // Step (3). Check THR profitability.
      Check_THR_Profitability ();

      // Step (4). Perform THR (and target-specific) code generation tasks.
      Perform_THR_Code_Generation ();
     
      // Reset the BB_scheduled flag if THR performed. 
      if (_chk_instr_inserted) Reset_BB_scheduled(_bb); 

      CG_DEP_Delete_Graph (_bb);
    }
  }
}

CG_THR::CG_THR()
{

  // Initialize memory pool for use in the scheduling this bb.
  MEM_POOL_Initialize (&_thr_pool, "CG_THR_pool", FALSE);
  MEM_POOL_Initialize (&_thr_map_pool, "CG_THR_map_pool", FALSE);
  MEM_POOL_Push(&_thr_pool);
  MEM_POOL_Push (&_thr_map_pool);

  _thr_map = BB_MAP_Create ();
  Trace_THR = Get_Trace (TP_SCHED, 0);
}

void
CG_THR::Init(BB *bb, THR_TYPE thr_type, BOOL before_regalloc)
{
  _thr_type = thr_type;
  _bb = bb;
  BB_OP_MAP omap = BB_OP_MAP_Create(bb, &_thr_map_pool);
  BB_MAP_Set(_thr_map, bb, omap);
  _chk_instr_inserted = FALSE;
  _thr_before_regalloc = before_regalloc;
}

CG_THR::~CG_THR()
{
  BB_MAP_Delete(_thr_map);

  MEM_POOL_Pop (&_thr_pool);
  MEM_POOL_Pop (&_thr_map_pool);
  MEM_POOL_Delete (&_thr_pool);
  MEM_POOL_Delete (&_thr_map_pool);
}
