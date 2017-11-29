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


/* =======================================================================
 * =======================================================================
 *
 * Module: gra_live.h
 * $Revision: 1.6 $
 * $Date: 05/12/05 08:59:07-08:00 $
 * $Author: bos@eng-24.pathscale.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.gra_live.h $
 *
 * Revision history:
 *
 * 26-MAY-93 - Original Version
 *
 * Description:
 * ============
 *
 *  This module exports a functional interface through which global
 *  liveness and reaching definition information is initialized before
 *  scheduling and maintained after scheduling.
 *
 *  The product this analysis are four BB fields:
 *
 *      BB_defreach_in      - TN_SET whose members have a definition
 *                            that reaches the entry of the BB.
 *      BB_defreach_out     - TN_SET whose members have a definition
 *                            that reaches the exit of the BB.
 *      BB_live_in          - TN_SET whose members are live at the
 *                            entry to the BB.
 *      BB_live_out         - TN_SET whose members are live at the
 *                            exit to the BB.
 *
 *  Highest level interface
 *  =======================
 *
 *  In order to compute liveness for all the blocks in a PU or region, use the
 *  following functions:
 *
 *      void GRA_LIVE_Init_PU()
 *      void GRA_LIVE_Finish_PU()
 *      void GRA_LIVE_Finish_REGION()
 *
 *          Compute/release liveness for all the blocks in the PU or given
 *          <rid>.
 *
 *          It is required that the _Init_PU or _Init_REGION function be
 *          called before the package can be used at all.
 *
 *     void GRA_LIVE_Recalc_Liveness()
 *   
 *          When pre-GCM phase is enabled, this may result in unnecessary
 *          growth of GTN sets especially when some of them have been
 *          converted back to local TNs. This phase does a recalculation of
 *          global TN sets. 
 *
 *  Calculating and updating global local information:
 *  =====================================================
 *
 *	    extern void GRA_LIVE_Compute_Local_Info( BB *bb );
 *
 *  Calculating and updating global liveness information:
 *  =====================================================
 *
 *      After the local liveness information for each block has been
 *      calculated, we are ready to calculate global liveness
 *      information (which is really the only exported product of this
 *      package.)  This is most simply done via
 *
 *          void GRA_LIVE_Compute_GLobal_Live_Info( RID *rid )
 *
 *              (Re)compute the global live infomation for the 
 *              REGION described by rid, or the entire
 *              PU flow graph if rid == NULL.
 *
 *      Both GCM and SWP perform transformations on non-scheduled
 *      blocks that invalidate previously computed liveness info.
 *      We'd like to be able to the liveness information as cheaply as
 *      possible.  Our approach is to limit the number of blocks that
 *      need to be considered in the recomputation.
 *
 *      In the case of GCM, the transformations involve moving
 *      operations between block, possibly duplicating them.  In the
 *      case of SWP, the transformations are of two types:
 *
 *          1. Creating new unscheduled blocks and splicing them into
 *             the flow graph.
 *
 *          2. Adding instructions to the beginning or end of
 *             unscheduled blocks.
 *
 *      After these sort of transformations are done, we will want to
 *      recompute liveness informations for a region of the flow
 *      graph.  In the case of SWP, this region may also include
 *      some scheduled blocks as well, though they will always be
 *      contiguous for now.
 *
 *      (The following are tailored to the SWP, which is a more
 *      difficult problem, I think.  I haven't given the GCM as much
 *      thought.)
 *
 *      When new unscheduled blocks are added to the flow graph, their
 *      liveness information is presented to GRA through the
 *      GRA_BB_Init interface.  See above.
 *
 *      After transformations to an unscheduled block that may change
 *      its liveness information, it's liveness information must be
 *      re-presented to the GRA throught the GRA_BB_Init interface.
 *      See above.
 *
 *      When transformations have been made that may alter the global
 *      liveness information within a set of blocks, we'll need to
 *      recompute this.  We'd like to avoid recomputing all the
 *      liveness information for the entire program unit.  To do this,
 *      we define a region of the flow graph within which the changes
 *      are contained.  At the borders of this region, the liveness
 *      information is assumed not to have changed.  The region is
 *      defined by two sets of BBs:
 *
 *          1. The "entry-set" of the region which are the blocks
 *             through which the region may be entred from blocks
 *             outside the region.
 *
 *          2. The "exit-set" of the region which are the blocks
 *             through which the region may be exited to blocks
 *             outside the region.
 *
 *      Formally, a the entry-set and exit set define a region as follows:
 *
 *          The members of the entry-set are members of the region.
 *
 *          X is member of the region if x is a flow successor of y st
 *          y is a member of the region and y is not a member of the
 *          exit-set.
 *
 *      The live_out and defreach_out fields any predecessors of an
 *      entry block that are not in the region are assumed to be
 *      unaffected by the transformation.  Similarly, the live_in and
 *      defreach_in fields of any successors of exit blocks that are
 *      not in the region are assumed to be unaffected by the
 *      transformation.
 *
 *      void GRA_LIVE_Region_Start(void)
 *
 *          Start the definition of a transformed region of the
 *          flow-graph.
 *
 *      void GRA_LIVE_Region_Entry(
 *          BB *bb
 *      )
 *
 *          Tells GRA that the given BB, 'bb', is a member of the
 *          entry-set of region currently being defined.
 *
 *      void GRA_LIVE_Region_Exit(
 *          BB *bb
 *      )
 *
 *          Tells GRA that the givne BB, 'bb', is a member of the
 *          exit-set of the region currently being defined.
 *
 *      void GRA_LIVE_Region_Compute_Global_Live_Info(void)
 *
 *          Definition of a transformed region is now complete.
 *          (Re)compute its global liveness information assuming the
 *          information at the borders is correct.
 *
 *      void GRA_LIVE_Init_Loop(BB *pbb, BB *ebb, 
 *                              CG_LOOP_BACKPATCH *pbp, CG_LOOP_BACKPATCH *ebp)
 *      void GRA_LIVE_Fini_Loop()
 *
 *          Pass prolog/epilog backpatch information to GRA_LIVE.
 *          At most 1 loop is active at a time.
 *          The CG_LOOP_BACKPATCH mechanism needs a rewrite
 *          because it is inaccessible outside cg_loop.cxx.
 *
 *
 *  Updating global and live information in a BB_REGION
 *  ===================================================
 * 
 *   Simplfied interface for recompute GRA live info.  Fixed a latent bug of the
 *   older interface.  Consider a region composed of basic blocks A, B, C, D, E.
 *   The CFG is:  A->B,  B->C,E,  C->D.  The region we want to specify is {A,B,C}.
 *   The old interface will use A as the entry, and B,C as exits.  The dataflow
 *   on the edge B->C is not processed.
 *
 *      void BB_REGION_Recompute_Global_Live_Info(const BB_REGION& region, BOOL recompute_local_info)
 *         Recompute liveness for the region specified by BB_REGION. 
 *         Recompute local liveness if "recompute_local_info" is TRUE.
 *          
 *
 *  Low level (unstructured) liveness updating
 *  ==========================================
 *
 *  Sometimes it may be important to calculate liveness information is a
 *  less structured way than above.  For example, we may have the
 *  following flow graph:
 *
 *          b1 ...
 *             goto b3
 *
 *          b2 ...
 *             goto b3
 *
 *          b3
 *
 *  Suppose we are only interested in the live_in data for b1 and we know
 *  that the live_out data for b3 is correct (but do not know that the
 *  defreach data for b1, b2 are correct.  Using the region based
 *  approach, we'd have to calculate the live-info for b2 as well as b1
 *  and we'd have to calculate their defreach data as well.
 *
 *  For these rare situations, we provide:
 *
 *      void GRA_LIVE_Local_Live_Propagate2(
 *          BB *pred,
 *          BB *succ
 *      )
 *
 *      (Re)calculate the live_in information of 'succ' assuming that it's
 *      live_out information is correct.  Use this to update the live_out
 *      information of 'pred' and then (re)calculate its live_in
 *      information.
 *
 *
 *      void GRA_LIVE_Local_Live_Propagate(
 *          BB *bb
 *      )
 *
 *      (Re)calculate BB_live_in(bb), assuming that BB_live_out(bb) is
 *      correct.
 *
 *
 *  For an example of the use of thse see the function Create_Loop_Exits_P
 *  in [n]be/cg/swp_sch_info.c.  This function needs the live_in set for a
 *  loop tail block that it had generated (the SWP winddown code).  It
 *  needs this information in order to generate the best possible loop
 *  compensation code.  The problem is that the flow graph at this point
 *  is both complex and somewhat incomplete.  However, the direct path to
 *  the loop tail is simple and complete, so it just computes the local
 *  live info for the two blocks involved, uses
 *  GRA_LIVE_Local_Live_Propagate2 to propagate through the two block
 *  path.  Later on, all the blocks involved are included in a region that
 *  has GRA_LIVE_Region_Compute_Global_Live_Info applied to it, which is
 *  probably a good idea when dealing with siguations like this.
 *
 *  Ultra low level liveness updating
 *  =================================
 *
 *  For those times when you really can't live without getting in there and
 *  munging the sets directly, here are functions to do just that.  They are
 *  added for the exclusive use of GRA, which updates the data structures for
 *  its own purposes and is the final client for them.
 *
 *      void GRA_LIVE_Add_Live_In_GTN( BB* bb, TN* tn )
 *      void GRA_LIVE_Remove_Live_In_GTN( BB* bb, TN* tn )
 *      void GRA_LIVE_Add_Live_Out_GTN( BB* bb, TN* tn )
 *      void GRA_LIVE_Remove_Live_Out_GTN( BB* bb, TN* tn )
 *      void GRA_LIVE_Add_Defreach_In_GTN( BB* bb, TN* tn )
 *      void GRA_LIVE_Remove_Defreach_In_GTN( BB* bb, TN* tn )
 *      void GRA_LIVE_Add_Defreach_Out_GTN( BB* bb, TN* tn )
 *      void GRA_LIVE_Remove_Defreach_Out_GTN( BB* bb, TN* tn )
 *      void GRA_LIVE_Add_Live_Use_GTN( BB* bb, TN* tn )
 *      void GRA_LIVE_Remove_Live_Use_GTN( BB* bb, TN* tn )
 *
 *          Directly add/remove 'tn' from the named liveness set of 'bb'.
 *
 *	void GRA_LIVE_Merge_Blocks( BB *dst, BB *a, BB *b )
 *
 *	    Block <b> has been merged with block <a> resulting in
 *	    block <dst>. Adjust the live sets of the merged block.
 *
 *          
 *	void GRA_LIVE_Compute_Liveness_For_BB( BB *bb )
 *
 *         Generic utility routine which computes the liveness sets of <bb>.
 *
 *  Checking for liveness
 *  =====================
 *
 *  The GRA_LIVE sets can be used to check for liveness of a <tn>
 *  at either the entry or the exit of a <bb>. These interfaces 
 *  can be used only before register allocation. There is an assumption
 *  that dedicated TNs are not live across multiple basic blocks.
 *
 *	BOOL GRA_LIVE_TN_Live_Outof_BB (struct tn *tn, BB *bb);
 *	BOOL GRA_LIVE_TN_Live_Into_BB (struct tn *tn, BB *bb);
 *
 *  NOTE: For liveness information after register allocation use
 *        the corresponding REG_LIVE interfaces.
 *
 *          
 *  Debugging & tracing
 *  ===================
 *
 *      void GRA_LIVE_Print_Liveness( BB* bb )
 *          Print the liveness info the <bb>. 
 *
 * =======================================================================
 * =======================================================================
 */

#ifndef GRA_LIVE_INCLUDED
#define GRA_LIVE_INCLUDED

struct cg_loop_backpatch;

#include "region_util.h"
#include "bb.h"
#include "tn_set.h"

extern GTN_SET* force_live_gtns;  /* exported list */
extern void Force_Live_Add (TN *tn);  /* add a TN to the force list */
extern void Force_Live_Remove (TN *tn); /* remove a TN from the force list */

extern void GRA_LIVE_Compute_Local_Info( BB *bb );
extern void GRA_LIVE_Region_Start(void);
extern void GRA_LIVE_Region_Entry( BB *bb );
extern void GRA_LIVE_Region_Exit( BB *bb );
extern void GRA_LIVE_Region_Compute_Global_Live_Info(void);
extern void GRA_LIVE_Local_Live_Propagate2( BB *pred, BB *succ );
extern void GRA_LIVE_Local_Live_Propagate( BB *bb );

extern void GRA_LIVE_Add_Live_In_GTN( BB* bb, struct tn* tn );
extern void GRA_LIVE_Remove_Live_In_GTN( BB* bb, struct tn* tn );
extern void GRA_LIVE_Add_Live_Out_GTN( BB* bb, struct tn* tn );
extern void GRA_LIVE_Remove_Live_Out_GTN( BB* bb, struct tn* tn );
extern void GRA_LIVE_Add_Defreach_In_GTN( BB* bb, struct tn* tn );
extern void GRA_LIVE_Remove_Defreach_In_GTN( BB* bb, struct tn* tn );
extern void GRA_LIVE_Add_Defreach_Out_GTN( BB* bb, struct tn* tn );
extern void GRA_LIVE_Remove_Defreach_Out_GTN( BB* bb, struct tn* tn );
extern void GRA_LIVE_Add_Live_Use_GTN( BB* bb, struct tn* tn );
extern void GRA_LIVE_Remove_Live_Use_GTN( BB* bb, struct tn* tn );
extern void GRA_LIVE_Add_Live_Def_GTN( BB* bb, struct tn* tn );
extern void GRA_LIVE_Remove_Live_Def_GTN( BB* bb, struct tn* tn );

extern BOOL GRA_LIVE_TN_Live_Outof_BB (struct tn *tn, BB *bb);
extern BOOL GRA_LIVE_TN_Live_Into_BB (struct tn *tn, BB *bb);

extern void GRA_LIVE_Merge_Blocks( BB *dst, BB *a, BB *b );
extern void GRA_LIVE_Compute_Liveness_For_BB(BB *bb);

extern void GRA_LIVE_Init( RID *rid );
extern void GRA_LIVE_Recalc_Liveness( RID *rid);
extern void GRA_LIVE_Finish_PU(void);
extern void GRA_LIVE_Finish_REGION(void);

extern void GRA_LIVE_Print_Liveness( BB* bb );

#ifdef KEY
extern void Rename_TNs_For_BB (BB *bb, TN_SET *multiple_defined_set,
			       OP *rename_local_TN_op = NULL);
#else
extern void Rename_TNs_For_BB (BB *bb, TN_SET *multiple_defined_set);
#endif
extern void GRA_LIVE_Rename_TNs (void);
extern void BB_REGION_Recompute_Global_Live_Info(const BB_REGION& region, BOOL recompute_local_info);

extern void GRA_LIVE_Detect_GTNs_In_Set (BB_SET *bb_set);

extern void GRA_LIVE_Init_Loop(BB *pbb, BB *bbb, BB *ebb, 
			       struct cg_loop_backpatch *pbp, 
			       struct cg_loop_backpatch *ebp);
extern void GRA_LIVE_Fini_Loop();

#endif /* GRA_LIVE_INCLUDED */
