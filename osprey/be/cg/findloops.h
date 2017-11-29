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


/* ====================================================================
 * ====================================================================
 *
 * Module: findloops.h
 * $Revision: 1.2 $
 * $Date: 02/11/07 23:41:24-00:00 $
 * $Author: fchow@keyresearch.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.findloops.h $
 *
 * Description:
 *
 * Utilities for detecting loops.
 *
 * Exported Types:
 *
 *   LOOP_DESCR: A structure describing a loop. The various fields are:
 *
 *     next        - pointer to another LOOP_DESCR.
 *     bbset       - a set containing all the basic blocks in the loop.
 *     loophead    - the entry basic block for the loop.
 *     nestlevel   - nesting level of the loop (higher = more deeply nested).
 *     num_exits   - the number of exits in the loop.
 *     loopinfo    - loop info (if any) associated with this loop.
 *     flags	   - flags to indicate loop properties, eg. total loops 
 *		     present in this loop, number of innerloops, multibbloops, 
 *		     exitloops, has calls etc.
 *
 * Utilities:
 *
 *   void LOOP_DESCR_Init_For_PU(void)
 *     Initialization routine.  Must be called at the start of processing
 *     each program unit.
 *
 *   LOOP_DESCR *LOOP_DESCR_Detect_Loops(MEM_POOL *pool)
 *     Detect all the loops in the procedure (Dragon book algorithm).
 *     Return a list of loop descriptors (LOOP_DESCR). The list is 
 *     ordered with the innermost loops first. This routine also sets
 *     the BB_nest_level for each basic block. <pool> is used to
 *     allocate memory for the returned data structures.  Must call
 *     Calculate_Dominators() before invoking this function.
 *
 *   void LOOP_DESCR_Print_List(void)
 *     Print trace information about all the loops detected in the
 *     last call to LOOP_DESCR_Detect_Loops.
 *
 *   void LOOP_DESCR_Add_BB(LOOP_DESCR *loop, BB *bb)
 *     Requires: LOOP_DESCR_Find_Loop(bb) == NULL &&
 *		 <loop> is the most innermost loop containing <bb>
 *     Add <bb> to <loop>, and to any enclosing loops.
 *
 *   void LOOP_DESCR_Delete_BB(LOOP_DESCR *loop, BB *bb)
 *     Requires: LOOP_DESCR_Find_Loop(bb) == loop
 *     Delete <bb> from <loop>, and from any enclosing loops.
 *
 *   LOOP_DESCR *LOOP_DESCR_Find_Loop(BB *bb)
 *     Given a bb, figure out which loop it belongs to.
 *
 *   BB* LOOP_DESCR_Find_Unique_Tail(LOOP_DESCR *loop)
 *     If the <loop> has a unique tail BB, return it. Otherwise 
 *     return NULL.
 *
 *   LOOP_DESCR *LOOP_DESCR_Next_Enclosing_Loop(LOOP_DESCR *loop)
 *     Return the descriptor of the loop enclosing <loop>, or NULL
 *     if there is none.
 *
 *   LOOP_DESCR *LOOP_DESCR_Is_Exit_Edge(BB *bb, BB *succ)
 *     Check if the edge from bb to succ is an exit from a loop. If yes,
 *     return the outermost loop that the edge exits. If the edge is not
 *     a loop exit, return NULL.
 *
 *   void LOOP_DESCR_Retarget_Loop_Entrances(LOOP_DESCR *loop, BB *to)
 *     Change all branches from outside of <loop> to its head (except
 *     those <to>) to branch to <to> instead, updating preds/succs list
 *     frequency info.
 *
 *   BOOL LOOP_DESCR_Can_Retarget_Loop_Entrances(LOOP_DESCR *loop)
 *     Predict if LOOP_DESCR_Retarget_Loop_Entrances would succeed.
 *
 *   float LOOP_DESCR_Estimate_Cycles(LOOP_DESCR *loop)
 *     Estimates the number of cycles (weighted by probabilities in case
 *     the loop has multiple-bbs) present in the loop. The estimation
 *     process can be done by either using the sched_est interface 
 *     (DEFAULT for the moment), or by using the LOCS interface 
 *     (-CG:sched_est_use_locs=on).
 *
 *   BOOL LOOP_DESCR_Has_Side_Entrance(LOOP_DESCR *loop)
 *     Returns true if an interior block in the loop has a predecessor
 *     from outside the loop.
 * ====================================================================
 * ====================================================================
 */

#ifndef	FINDLOOPS_INCLUDED
#define	FINDLOOPS_INCLUDED

#include "bb_set.h"	/* to get definition of BB_SET */
#include "cg_vector.h"

struct LOOP_DESCR {
  LOOP_DESCR *next;
  BB_SET     *bbset;
  BB         *loophead;
  LOOPINFO   *loopinfo;
  INT         nestlevel;
  MEM_POOL   *mem_pool;
  INT         num_exits;
#ifdef TARG_SL
  INT         lc_index;  /* loop count index, it's useful
                         * only when Can_Zero_Delay(loop)
                         * is true
                         */
  VECTOR      children; /* children in the loop tree */
#endif
  mINT32      flags;
};

#define LOOP_DESCR_bbset(l)       ((l)->bbset)
#define LOOP_DESCR_loophead(l)    ((l)->loophead)
#define LOOP_DESCR_nestlevel(l)   ((l)->nestlevel)
#define LOOP_DESCR_num_exits(l)   ((l)->num_exits)
#define LOOP_DESCR_next(l)        ((l)->next)
#define LOOP_DESCR_loopinfo(l)    ((l)->loopinfo)
#define LOOP_DESCR_flags(l)    	  ((l)->flags)
#ifdef TARG_SL
#define LOOP_DESCR_lcidx(l)     ((l)->lc_index)
#endif
/* flags to annotate the properties of the loop. these comprise the flag
 * properties for LOOP_DESCR
 */
#define		LOOP_DESCR_Inner_Loop		0x001
#define		LOOP_DESCR_Multibb_Loop		0x002
#define		LOOP_DESCR_Exit_Loop		0x004
#define		LOOP_DESCR_Call_Loop		0x008

/* flag to annotate the Zero_Delay_Loop Optimizations */
#ifdef TARG_SL
#define         LOOP_DESCR_Can_Zero_Delay       0x010
#define         LOOP_DESCR_MVTC_Optimized       0x020
#endif 

/* high-level macros
 */

#ifdef TARG_SL
#define		Can_Zero_Delay(loop)	\
			(LOOP_DESCR_flags(loop) & LOOP_DESCR_Can_Zero_Delay)
#define		Set_Can_Zero_Delay(loop)	\
			(LOOP_DESCR_flags(loop) |= LOOP_DESCR_Can_Zero_Delay)
#define		Reset_Can_Zero_Delay(loop)	\
			(LOOP_DESCR_flags(loop) &= (~LOOP_DESCR_Can_Zero_Delay))
#define         MVTC_Optimized(loop) \
                        (LOOP_DESCR_flags(loop) & LOOP_DESCR_MVTC_Optimized)
#define         Set_MVTC_Optimized(loop) \
                        (LOOP_DESCR_flags(loop) |= LOOP_DESCR_MVTC_Optimized)
#endif

#define		Is_Inner_Loop(loop)	\
			(LOOP_DESCR_flags(loop) & LOOP_DESCR_Inner_Loop)
#define		Set_Inner_Loop(loop)	\
			(LOOP_DESCR_flags(loop) |= LOOP_DESCR_Inner_Loop)
#define		Reset_Inner_Loop(loop)	\
			(LOOP_DESCR_flags(loop) &= ~LOOP_DESCR_Inner_Loop)
#define		Is_Multibb_Loop(loop)	\
			(LOOP_DESCR_flags(loop) & LOOP_DESCR_Multibb_Loop)
#define		Set_Multibb_Loop(loop)	\
			(LOOP_DESCR_flags(loop) |= LOOP_DESCR_Multibb_Loop)
#define		Reset_Multibb_Loop(loop) \
			(LOOP_DESCR_flags(loop) &= ~LOOP_DESCR_Multibb_Loop)
#define		Is_Exit_Loop(loop)	\
			(LOOP_DESCR_flags(loop) & LOOP_DESCR_Exit_Loop)
#define		Set_Exit_Loop(loop)	\
			(LOOP_DESCR_flags(loop) |= LOOP_DESCR_Exit_Loop)
#define		Reset_Exit_Loop(loop) 	\
			(LOOP_DESCR_flags(loop) &= ~LOOP_DESCR_Exit_Loop)
#define		Is_Call_Loop(loop)	\
			(LOOP_DESCR_flags(loop) & LOOP_DESCR_Call_Loop)
#define		Set_Call_Loop(loop)	\
			(LOOP_DESCR_flags(loop) |= LOOP_DESCR_Call_Loop)
#define		Reset_Call_Loop(loop) 	\
			(LOOP_DESCR_flags(loop) &= ~LOOP_DESCR_Call_Loop)

#if defined(TARG_SL)
extern void LOOP_DESCR_Create_Loop_Tree( MEM_POOL* pool );
extern void LOOP_DESCR_Dump_Loop_Brief( LOOP_DESCR* loop );
extern void LOOP_DESCR_Dump_Loop( INT indent, LOOP_DESCR* cloop );
extern void LOOP_DESCR_Dump_Loop_Tree( void );
extern VECTOR loop_tree_roots;
#endif

extern void LOOP_DESCR_Init_For_PU(void);
extern LOOP_DESCR * LOOP_DESCR_Detect_Loops (MEM_POOL *pool);
extern void LOOP_DESCR_Print_List(void);
#pragma mips_frequency_hint NEVER LOOP_DESCR_Print_List
extern LOOP_DESCR *LOOP_DESCR_Is_Exit_Edge(BB *bb, BB *succ);
extern void LOOP_DESCR_Add_BB(LOOP_DESCR *loop, BB *bb);
extern void LOOP_DESCR_Delete_BB(LOOP_DESCR *loop, BB *bb);
extern BB* LOOP_DESCR_Find_Unique_Tail(LOOP_DESCR *loop);
extern LOOP_DESCR *LOOP_DESCR_Next_Enclosing_Loop(LOOP_DESCR *loop);
extern void LOOP_DESCR_Retarget_Loop_Entrances(LOOP_DESCR *loop, BB *to);
extern BOOL LOOP_DESCR_Can_Retarget_Loop_Entrances(LOOP_DESCR *loop);
extern float LOOP_DESCR_Estimate_Cycles(LOOP_DESCR *loop);
extern BOOL LOOP_DESCR_Has_Side_Entrance(LOOP_DESCR *loop);

extern BB_MAP LOOP_DESCR_map;
#define LOOP_DESCR_Find_Loop(bb) \
	((LOOP_DESCR *)BB_MAP_Get(LOOP_DESCR_map, bb))

#endif /* FINDLOOPS_INCLUDED */
