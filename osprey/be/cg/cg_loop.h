/*
 * Copyright (C) 2008 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

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
 *  Module: cg_loop.h
 *  $Revision: 1.6 $
 *  $Date: 05/12/05 08:59:03-08:00 $
 *  $Author: bos@eng-24.pathscale.com $
 *  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.cg_loop.h $
 *
 *  Revision comments:
 *
 *  3-May-1995 - Initial version
 *
 *  Description:
 *  ============
 *
 *  Interface to most CG loop analysis and transformation.  See also
 *  "cg_dep_graph.h" for support of cyclic dependence graphs and
 *  "findloops.h" for loop descriptor documentation.
 *
 *  At the start of loop processing for a PU or region, the following
 *  initialization function should be called:
 *    void CG_LOOP_Init()
 *
 *  The trip count of a loop is given by:
 *    TN *CG_LOOP_Trip_Count(LOOP_DESCR *loop), or
 *  which may generate prolog code if the trip count TN hasn't yet been
 *  generated, and which returns NULL if the trip count isn't computable
 *  before the loop body.
 *
 *  After initialization, if any new loop body OPs are created, the
 *  following routine must be called to initialize the CG_LOOP data
 *  structures for each new OP:
 *    void CG_LOOP_Init_Op(OP *op)
 *    void CG_LOOP_Init_Ops(OPS *ops)
 *
 *  To see whether an arbitrary OP has CG_LOOP info attached, use:
 *    BOOL Is_CG_LOOP_Op(OP *op)
 *
 *
 *  As CGPREP analyzes and transforms loop bodies, it annotates some
 *  uses of TNs with iteration distances, or "omegas", to specify the
 *  relative iteration producing the value to be used.  For example,
 *  TN100[2] means the value of TN100 produced two iterations before
 *  the current one.  An omega of MAX_OMEGA means the value comes from
 *  at *least* MAX_OMEGA iterations back.  An omega of zero means the
 *  value comes from the current iteration.  The omega associated with
 *  a particular TN use is accessed with:
 *    UINT8 OP_omega(OP *op, UINT8 opnd)
 *    void Set_OP_omega(OP *op, UINT8 opnd, UINT8 omega)
 *  Uses of dedicated TNs cannot have nonzero omegas.  Uses of loop-
 *  invariant TNs cannot have nonzero omegas.
 *
 *  Analogous to omegas for TNs, a use of a spill location can
 *  have an omega. The omega associated with a restore is accessed with:
 *    UINT8 OP_restore_omega(OP *op)
 *    void Set_OP_restore_omega(OP *op, UINT8 omega)
 *  Note that the restore omega is only valid for restore OPs.
 *
 *  The following routine is provided to check whether a TN is loop-
 *  invariant:
 *    BOOL CG_LOOP_TN_Is_Invariant(LOOP_DESCR *loop, TN *tn)
 *
 *
 *  Loop unrolling is performed by:
 *    BB *CG_LOOP_Unroll(LOOP_DESCR *loop)
 *  which returns a new unrolled body if the loop is unrolled.  It
 *  may also create a "remainder loop" in the prolog or epilog.
 *
 *  CGPREP (and the software pipeliner) may generate prolog and/or
 *  epilog code to be appended to the original loop head (prolog) or
 *  prepended to the original loop body (epilog).  The following
 *  global BB pointers are used to hold this code for the current
 *  loop body being processed:
 *    BB *CG_LOOP_prolog, *CG_LOOP_epilog;
 *  These are initially NULL.  In general, these may each be a list of
 *  BBs with arbitrarily complex control flow within.  The prolog
 *  always points to the last BB in the prolog (which must be
 *  dominated by the first BB in the prolog after any transformation).
 *  The epilog always points to the first BB in the epilog (which must
 *  be post-dominated by the last BB in the epilog).  The starting
 *  prolog and ending epilog BBs can be referenced by:
 *    BB *CG_LOOP_prolog_start, *CG_LOOP_epilog_end;
 *
 *  The following routines are provided to trace the prolog and epilog
 *  blocks:
 *    void CG_LOOP_Trace_Prolog(void)
 *    void CG_LOOP_Trace_Epilog(void)
 *
 *  To trace the whole loop, including the prolog and epilog,
 *  and preceded by a comment given with "printf"-style args, use:
 *    void CG_LOOP_Trace_Loop(LOOP_DESCR *loop, const char *fmt, ...)
 *
 *
 *  When a use of a TN with a non-zero omega is introduced into the
 *  loop body, we must "prime the pump" by specifying the initial
 *  values for TN[1..omega] so that the omega uses are defined during
 *  the first <omega> iterations.  Likewise, upon exit from the loop
 *  we may want to use some of the TNs defined in the loop (possibly
 *  from iterations prior to the last one) in the code that follows
 *  the loop.  We relate these initial values and live-out values for
 *  the body TNs to the TNs used/defined outside the body ("non body"
 *  TNs) with the CG_LOOP_BACKPATCH data structures.  These exist for
 *  the CG_LOOP_prolog and the CG_LOOP_epilog.  They are operated upon
 *  with:
 *
 *    CG_LOOP_BACKPATCH *CG_LOOP_Backpatch_Add(BB *bb, TN *non_body_tn,
 *					       TN *body_tn, UINT8 omega)
 *	Requires: bb == CG_LOOP_prolog || bb == CG_LOOP_epilog.
 *	Add a backpatch entry relating <non_body_tn> with <body_tn>[omega]
 *	in the prolog/epilog specified by <bb>.  Returns the new backpatch.
 *
 *    TN *CG_LOOP_Backpatch_Find_Body_TN(BB *bb, TN *tn, UINT8 *omptr)
 *	Requires: bb == CG_LOOP_prolog || bb == CG_LOOP_epilog.
 *	If <bp> contains an entry for non-body TN <tn>, return the body TN
 *      related	to it, and set <*omptr> to the related omega unless <omptr>
 *	is NULL.  Otherwise return NULL and don't touch <*omptr>.
 *
 *    TN *CG_LOOP_Backpatch_Find_Non_Body_TN(BB *bb, TN *tn, UINT8 om)
 *	Requires: bb == CG_LOOP_prolog || bb == CG_LOOP_epilog.
 *	If the backpatches for <bb> contains an entry for body TN <tn>
 *	with omega <om>, return the related non-body TN, otherwise return
 *	NULL.
 *
 *    CG_LOOP_BACKPATCH *CG_LOOP_Backpatch_First(BB *bb, TN *body_tn)
 *    CG_LOOP_BACKPATCH *CG_LOOP_Backpatch_Next(CG_LOOP_BACKPATCH *bp)
 *	Requires: bb == CG_LOOP_prolog || bb == CG_LOOP_epilog.
 *	Iterate over the backpatches for <bb>.  If <body_tn> is NULL,
 *	iterate over all backpatches, otherwise iterate only over the
 *	backpatches for <body_tn>.  The Next function steps to the next
 *	backpatch.  Both functions return NULL to indicate that iteration
 *	has completed.  The following rvalue accessors are provided:
 *	  TN *CG_LOOP_BACKPATCH_body_tn(CG_LOOP_BACKPATCH *bp)
 *	  UINT8 CG_LOOP_BACKPATCH_omega(CG_LOOP_BACKPATCH *bp)
 *	  TN *CG_LOOP_BACKPATCH_non_body_tn(CG_LOOP_BACKPATCH *bp)
 *	and the following mutators are provided:
 *	  void CG_LOOP_BACKPATCH_Set_body_tn(CG_LOOP_BACKPATCH *bp, TN *tn)
 *	  void CG_LOOP_BACKPATCH_Set_omega(CG_LOOP_BACKPATCH *bp, UINT8 omega)
 *	  void CG_LOOP_BACKPATCH_Set_non_body_tn(CG_LOOP_BACKPATCH *bp, TN *tn)
 *
 *    void CG_LOOP_Backpatch_Delete(BB *bb, CG_LOOP_BACKPATCH *bp)
 *	Requires: bb == CG_LOOP_prolog || bb == CG_LOOP_epilog
 *		  <bp> is a backpatch for <bb>
 *	Remove <bp> from <bb>'s backpatches.
 *
 *    void CG_LOOP_Backpatch_Replace_Body_TN(BB *bb, TN *tn, TN *newtn,
 *					     INT16 om_adj)
 *	Requires: bb == CG_LOOP_prolog || bb == CG_LOOP_epilog
 *	In the backpatches for <bb>, replace each entry with body
 *	TN <tn>[omega] with <newtn>[omega+om_adj].
 *
 *    void CG_LOOP_Backpatch_Replace_Non_Body_TN(BB *bb, TN *tn, TN *newtn)
 *	Requires: bb == CG_LOOP_prolog || bb == CG_LOOP_epilog
 *	In the backpatches for <bb>, replace each entry with non-body
 *	TN <tn> with <newtn>.
 *
 *    void CG_LOOP_Backpatch_Trace(BB *bb, CG_LOOP_BACKPATCH *bp)
 *      Print a representation of <bp> to the trace file.  If <bb>
 *      is CG_LOOP_prolog, the backpatch will be printed as:
 *	  TNbody[omega] <- TNnon_body
 *      If <bb> is CG_LOOP_epilog, it will be printed as:
 *  	  TNnon_body <- TNbody[omega]
 *      Otherwise (e.g., if <bb> is NULL) it will be printed as:
 *  	  TNbody[omega] TNnon_body
 *      If <bp> is NULL, we print all backpatches for the given <bb>.
 *      If both <bp> and <bb> are NULL, we print all backpatches
 *      (indicating which ones are for the prolog and which are for
 *      the epilog).
 *
 *
 *  CG_LOOP provides the per-OP containers for SCC (Strongly Connected
 *  Component) information.  See "cg_loop_scc.h" for per-SCC data
 *  structures and utility routines.  The following accessors are
 *  provided for the CG_LOOP SCC containers provided here:
 *
 *    CG_LOOP_SCC *OP_scc(OP *op)
 *    void Set_OP_scc(OP *op, CG_LOOP_SCC *scc)
 *	Get/set the SCC info for <op>.  See "cg_loop_scc.h" for the
 *	CG_LOOP_SCC description.  The Set accessor is intended for
 *	use only by the cg_loop_scc module.
 *
 *    INT16 OP_scc_index(OP *op)
 *    void Set_OP_scc_index(OP *op, INT16 idx)
 *	Get/set the index of <op> within OP_scc(op).  The Set accessor
 *	is intended for use only by the cg_loop_scc module.
 *
 *    ARC_LIST *OP_scc_ancestors(OP *op)
 *    ARC_LIST *OP_scc_descendents(OP *op)
 *	Return the list of SCC ancestor/descendent ARCs for <op>.
 *
 *    void CG_LOOP_Add_SCC_Arc(OP *ancestor, OP *descendent, UINT8 omega,
 *			       INT16 latency)
 *	Construct a new SCC arc from <ancestor> to <descendent> with the
 *	given <omega> and <latency> and attach it to <ancestor's> descendent
 *	list and <descendent's> ancestor list.
 *
 *    void CG_LOOP_Clear_SCCs(LOOP_DESCR *loop)
 *	Delete all SCC arcs for the OPs in <loop>, making them available
 *	for reuse by the dependence graph builder and SCC arc builder.
 *	Also set all OP_scc's to NULL.
 *
 *
 *  CG_LOOP provides a service for finding loop overhead OPs within a BB:
 *
 *    OP_LIST *CG_LOOP_Identify_Loop_Overhead(LOOP_DESCR *loop,
 *					      INT32 *loop_overhead_count,
 *					      MEM_POOL *pool);
 *	The function return value is a list of OPs that can be treated
 *	as overhead in loop body <loop>. The number of OPs on the list
 *	is returned through the reference parameter <loop_overhead_count>.
 *	<pool> identifies the pool that the list of OPs should be
 *	allocated from.
 *
 *  TODO: Describe loh accessors: OP_loh, OP_loh_mii, "set" accessors.
 *
 *
 *  Currently the software pipeliner is the only subphase following
 *  CGPREP that can make use of the loop notations.  The following
 *  routine converts a loop so that the loop notations are no longer
 *  needed.  This conversion may involve generating some copies in the
 *  loop body and some head/tail code so that the converted loop has
 *  the same semantics as the original loop plus the notations.  The
 *  BB's for the head/tail code are passed in explicitly since some
 *  clients (e.g., inner loop unrolling) may want to place this code
 *  somewhere other than the loop prolog:
 *    void CG_LOOP_Remove_Notations(LOOP_DESCR *loop, BB *head, BB *tail)
 *
 *
 *  Finally, the following function should be called to update the loop
 *  global liveness info:
 *
 *    void CG_LOOP_Recompute_Liveness(LOOP_DESCR *loop)
 *	Recompute global liveness info for <loop>.
 *
 *  The following function inserts prolog and epilog BB's around a loop:
 *
 *  BB* CG_LOOP_Gen_And_Prepend_To_Prolog(BB *loop_head, LOOP_DESCR *loop)
 *    Create and prepend a <new_bb> to loop prologue, retargets all 
 *    the outside loop entrances to this <new_bb>, updates the pred/succ 
 *    and live-ness info for this <new_bb> accordingly. Frequency
 *    and LOOP_DESCR updation is also done.     
 *    
 *  BB* CG_LOOP_Append_BB_To_Prolog(BB *loop_prolog, BB *loop_head)
 *    Create and append a <new_bb> to the <loop_prolog> bb (if necessary), 
 *    such that if it ends in a branch, it creates a fall-thru block and 
 *    puts the branch there. This is used as a placeholder  <bb> to keep 
 *    all the instructions which are moved out of the loop. This relies on
 *    CFLOW pass to later merge them.
 *
 *  BB* CG_LOOP_Append_BB_To_Prolog_MV(BB *loop_prolog, BB *loop_head)
 *    Create and append a <new_bb) to the <loop_prolog> bb (if necessary),
 (*    such that if it ends in a branch, it creates a fall-through block and
 *    puts a branch there, will added a conditional branch from the
 *    old prolog block to its branch target.  This is used as a 
 *    placeholder  <bb> to keep all the instructions which are moved 
 *    out of the loop. This relies on CFLOW pass to later merge them.
 *
 *  BB* CG_LOOP_Prepend_BB_To_Epilog_MV(BB *loop_epilog, BB *loop_head)
 *    Create and append a <new_bb> after a <loop_body> bb (if necessary), 
 *    such that it creates a goto from the <new_bb> to the formal
 *    epilog.  This is used as a placeholder  <bb> to keep 
 *    all the instructions which are moved out of the loop. This relies on
 *    CFLOW pass to later merge them.
 *  
 *  double CG_LOOP_Prefetch_Stride(OP *pref)
 *    Requires: OP_prefetch(pref)
 *    Returns the stride of prefetch OP <pref>.
 *  
 *  CG_LOOP_Coalesce_Backedges(LOOP_DESCR *loop)
 *    Coalesce all backedge branches in a loop into a single branch,
 *    i.e. create a block with an unconditional branch to the loop head
 *    and have all backedges branch to that.  This allows if-conversion
 *    of backedge branches.
 *
 * =======================================================================
 * ======================================================================= */

#ifndef CG_LOOP_INCLUDED
#define CG_LOOP_INCLUDED


#include "cg_dep_graph.h"
#include "cg_loop_scc.h"
#include "op_list.h"
#include "whirl2ops.h"
#include "findloops.h"

struct _CG_LOOP_INFO {
/* ---------------------------------------------------- */
/* All fields are private.				*/
/* This 'omega' field of this structure is variable-	*/
/* size according to the number of operands of the OP	*/
/* to which this structure is attached. However, its	*/
/* minimum size is large enough for all fixed size OPs.	*/
/* See CG_LOOP_Init_Op() for more info.			*/
/* ---------------------------------------------------- */
  CG_LOOP_SCC *scc;
  ARC_LIST *scc_ancestors;
  ARC_LIST *scc_descendents;
  mINT16 loh_mii;
  mINT16 scc_index;
  mUINT8 restore_omega;
  mBOOL loh;
  mUINT8 omega[OP_MAX_FIXED_OPNDS]; // THIS FIELD MUST BE LAST (see above)
};

/* Given an op, return the 'sizeof' the _CG_LOOP_INFO struct needed
 * for that OP. For the sake of simplicity, the result of the calculation 
 * is often slightly larger than necessary and not rounded up to
 * the alignment of the struct.
 */
inline UINT _CG_LOOP_info_sizeof(OP *op)
{
  UINT sizeof_info = sizeof(_CG_LOOP_INFO);
  if (OP_opnds(op) > OP_MAX_FIXED_OPNDS) {
    sizeof_info += sizeof(mUINT8) * (OP_opnds(op) - OP_MAX_FIXED_OPNDS);
  }
  return sizeof_info;
}

/* Internal OP_MAP to hold loop info.
 */
extern OP_MAP _CG_LOOP_info_map;

#define _CG_LOOP_info(op) ((_CG_LOOP_INFO *)OP_MAP_Get(_CG_LOOP_info_map, op))

/* Internal backpatch data structure representation.  When iterating
 * over backpatches, we need to know whether we're iterating over the
 * backpatches for a single body_tn or all body_tns.  We indicate this
 * with the least significant bit of the pointer result.  The internal
 * _CG_LOOP_BP macros below are used to access this.
 */
struct cg_loop_backpatch {
  TN *non_body_tn;
  TN *body_tn;
  struct cg_loop_backpatch *next;
  UINT8 omega;
};

#define _CG_LOOP_BP_iter_limited(bp) ((INTPTR)(bp) & 1)
#define _CG_LOOP_BP_limit_iter(bp) \
  ((struct cg_loop_backpatch *)((INTPTR)(bp) | 1))
#define _CG_LOOP_BP_actual_ptr(bp) \
  ((struct cg_loop_backpatch *)(((INTPTR)(bp)) & ~1))

/* Exported types.
 */
typedef struct cg_loop_backpatch CG_LOOP_BACKPATCH;


/* Exported macros.
 */

/* This could be as high as 255, but really large omegas make SWP
 * get very slow, so use 32 for now.
 */
#define MAX_OMEGA 32

#define Is_CG_LOOP_Op(op) \
  (_CG_LOOP_info_map && _CG_LOOP_info(op) ? TRUE : FALSE) 
#define OP_omega(op, opnd) (_CG_LOOP_info(op)->omega[opnd]+0)

inline void Set_OP_omega(OP *op, UINT8 opnd, UINT8 omega)
{
  _CG_LOOP_INFO *info = _CG_LOOP_info(op);
  Is_True(omega < 2 || !TN_is_dedicated(OP_opnd(op,opnd)),
	    ("Trying to put omega of %d on use of dedicated TN", omega));
  info->omega[opnd] = omega;
}

#define OP_restore_omega(op) (_CG_LOOP_info(op)->restore_omega+0)
#define Set_OP_restore_omega(op, omega) \
		(_CG_LOOP_info(op)->restore_omega = (omega))

#define OP_loh_mii(op) (_CG_LOOP_info(op)->loh_mii+0)
#define Set_OP_loh_mii(op, mii) (_CG_LOOP_info(op)->loh_mii = mii)

#define OP_loh(op) (_CG_LOOP_info(op)->loh+0)
#define Set_OP_loh(op) (_CG_LOOP_info(op)->loh = TRUE)
#define Reset_OP_loh(op) (_CG_LOOP_info(op)->loh = FALSE)

#define OP_scc(op) ((struct cg_loop_scc *)_CG_LOOP_info(op)->scc)
#define Set_OP_scc(op, s) (_CG_LOOP_info(op)->scc = s)

#define OP_scc_index(op) (_CG_LOOP_info(op)->scc_index+0)
#define Set_OP_scc_index(op, idx) (_CG_LOOP_info(op)->scc_index = idx)

#define OP_scc_ancestors(op) (_CG_LOOP_info(op)->scc_ancestors+0)
#define OP_scc_descendents(op) (_CG_LOOP_info(op)->scc_descendents+0)

#define CG_LOOP_Add_SCC_Arc(ancestor, descendent, omega, latency)	\
  CG_DEP_Add_SCC_Arc(ancestor, descendent, omega, latency,		\
		     &_CG_LOOP_info(descendent)->scc_ancestors,		\
		     &_CG_LOOP_info(ancestor)->scc_descendents)


#define CG_LOOP_BACKPATCH_non_body_tn(bp) \
  (_CG_LOOP_BP_actual_ptr(bp)->non_body_tn+0)
#define CG_LOOP_BACKPATCH_body_tn(bp) \
  (_CG_LOOP_BP_actual_ptr(bp)->body_tn+0)
#define CG_LOOP_BACKPATCH_omega(bp) \
  (_CG_LOOP_BP_actual_ptr(bp)->omega+0)

#define CG_LOOP_BACKPATCH_Set_non_body_tn(bp, newtn) \
  (_CG_LOOP_BP_actual_ptr(bp)->non_body_tn = (newtn))
#define CG_LOOP_BACKPATCH_Set_body_tn(bp, newtn) \
  (_CG_LOOP_BP_actual_ptr(bp)->body_tn = (newtn))
#define CG_LOOP_BACKPATCH_Set_omega(bp,om) \
  (_CG_LOOP_BP_actual_ptr(bp)->omega = (om))

extern BOOL CG_LOOP_TN_Is_Invariant(LOOP_DESCR *loop, TN *tn);

/* Exported global variables.
 */
extern BB *CG_LOOP_prolog;
extern BB *CG_LOOP_epilog;
extern BB *CG_LOOP_prolog_start;
extern BB *CG_LOOP_epilog_end;

extern UINT32 CG_LOOP_unroll_times_max;
extern UINT32 CG_LOOP_unrolled_size_max;
extern BOOL CG_LOOP_unroll_fully;
extern UINT32 CG_LOOP_unroll_level;
extern BOOL CG_LOOP_unroll_fb_required;
extern BOOL CG_LOOP_unroll_remainder_fully;
extern UINT32 CG_LOOP_unroll_min_trip;
extern BOOL CG_LOOP_unroll_analysis;
extern BOOL CG_LOOP_unroll_best_fit;
extern BOOL CG_LOOP_ooo_unroll_heuristics;
extern BOOL CG_LOOP_ooo_unroll_heuristics_set;
extern UINT32 CG_LOOP_reorder_buffer_size;
extern UINT32 CG_LOOP_cache_miss_threshold;
extern BOOL CG_LOOP_multi_bb_unroll_analysis;
extern UINT32 CG_LOOP_unroll_analysis_threshold;
extern BOOL CG_LOOP_unroll_multi_bb;
extern BOOL CG_LOOP_unroll_non_trip_countable;
extern BOOL CG_LOOP_create_loop_prologs;
extern BOOL CG_LOOP_create_loop_epilogs;
extern INT32 CG_LOOP_force_ifc;

extern BOOL CG_LOOP_optimize_lno_winddown_cache;
extern BOOL CG_LOOP_optimize_lno_winddown_reg;
extern BOOL CG_LOOP_optimize_non_innermost;
extern BOOL CG_LOOP_optimize_multi_targ;
extern BOOL CG_LOOP_optimize_non_trip_countable;

#ifdef KEY
extern INT32 CG_Enable_Loop_Opt_Limit;
#endif

/* Exported functions.
 */
extern void CG_LOOP_Init();
extern void CG_LOOP_Finish();

inline TN *CG_LOOP_Trip_Count(LOOP_DESCR *loop)
{
  LOOPINFO *info = LOOP_DESCR_loopinfo(loop);
  return info ? LOOPINFO_trip_count_tn(info) : NULL;
}

CG_LOOP_BACKPATCH *CG_LOOP_Backpatch_Add(BB *bb, TN *non_body_tn,
					 TN *body_tn, UINT8 omega);

TN *CG_LOOP_Backpatch_Find_Body_TN(BB *bb, TN *tn, UINT8 *omptr);
TN *CG_LOOP_Backpatch_Find_Non_Body_TN(BB *bb, TN *tn, UINT8 om);

CG_LOOP_BACKPATCH *CG_LOOP_Backpatch_First(BB *bb, TN *body_tn);
CG_LOOP_BACKPATCH *CG_LOOP_Backpatch_Next(CG_LOOP_BACKPATCH *bp);

void CG_LOOP_Backpatch_Delete(BB *bb, CG_LOOP_BACKPATCH *bp);

void CG_LOOP_Backpatch_Replace_Non_Body_TN(BB *bb, TN *tn, TN *newtn);
void CG_LOOP_Backpatch_Replace_Body_TN(BB *bb, TN *tn, TN *newtn,
				       INT16 om_adj);

void CG_LOOP_Backpatch_Trace(BB *bb, CG_LOOP_BACKPATCH *bp);

void CG_LOOP_Remove_Notations(LOOP_DESCR *loop, BB *head, BB *tail);

OP_LIST *CG_LOOP_Identify_Loop_Overhead(LOOP_DESCR *loop,
					INT32 *loop_overhead_count,
					MEM_POOL *pool);

void CG_LOOP_Clear_SCCs(LOOP_DESCR *loop);

void CG_LOOP_Init_Op(OP *op);
void CG_LOOP_Init_OPS(OPS *ops);

void CG_LOOP_Recompute_Liveness(LOOP_DESCR *loop);

BB *CG_LOOP_Unroll(LOOP_DESCR *loop, BOOL);
void CG_LOOP_Prune_Prefetches(BB *bb, INT32 offset, BOOL before_swp, BOOL swp_wind);

void CG_LOOP_Trace_Prolog(void);
void CG_LOOP_Trace_Epilog(void);
void CG_LOOP_Trace_Loop(LOOP_DESCR *loop, const char *fmt, ...);

void CG_LOOP_Remove_Prolog_OPs(BB *head);
void CG_LOOP_Remove_Epilog_OPs(BB *tail);
BB* CG_LOOP_Gen_And_Prepend_To_Prolog(BB *loop_head, LOOP_DESCR* loop);
BB* CG_LOOP_Append_BB_To_Prolog(BB *loop_prolog, BB *loop_head);

#if defined(TARG_X8664)
BB* CG_LOOP_Append_BB_To_Prolog_MV(BB *loop_prolog, BB *loop_head);
BB* CG_LOOP_Prepend_BB_To_Epilog_MV(BB *loop_epilog, BB *loop_head);
#endif

void CG_LOOP_Coalesce_Backedges(LOOP_DESCR *loop);

TN *CG_LOOP_unroll_names_get(TN *tn, UINT8 unrolling);
extern INT Branch_Target_Operand(OP *br_op);

inline UINT32 CG_LOOP_Prefetch_Stride(OP *pref)
{
  WN *pref_wn = Get_WN_From_Memory_OP(pref);
  UINT32 s1 = WN_pf_stride_1L(pref_wn);
  UINT32 s2 = WN_pf_stride_2L(pref_wn);
  Is_True(s1 == 0 || s2 == 0, ("prefetch has L1 and L2 strides"));
  return s1 + s2;
}


void unroll_do_loop(LOOP_DESCR *, UINT32);
void unroll_do_loop_fully(LOOP_DESCR *, UINT32);
void unroll_dowhile_loop(LOOP_DESCR *, UINT32);


//  A class to keep trace of all global states needed by loop optimizations.
// 
//  Currently the content of this class mirrows the global variables
//  needed by existing functions.
//
enum CG_LOOP_FLAGS {
  CG_LOOP_NONE       = 0x0,
  CG_LOOP_HAS_PROLOG = 0x1,
  CG_LOOP_HAS_EPILOG = 0x2,
  CG_LOOP_EPILOG_REACHABLE = 0x4,  // epilog reachable from loop
};

class CG_LOOP {

private:
  LOOP_DESCR *loop;
  BOOL        unroll_fully;
  INT32       unroll_factor;
  OP_MAP      op_map;
  BB         *unroll_remainder_bb;
  INT32       flags;
  BB         *prolog_start;
  BB         *prolog_end;
  BB         *epilog_start;
  BB         *epilog_end;
  BB         *trip_count_bb;  // The BB contains the trip count expr
#ifdef TARG_IA64
  INT32      acyclic_len; 
  INT32      acyclic_len_wo_dspec;
#endif

  void Attach_Prolog_And_Epilog(LOOP_DESCR *loop);

public:
  BOOL Has_prolog() const { return flags & CG_LOOP_HAS_PROLOG; }
  BOOL Has_epilog() const { return flags & CG_LOOP_HAS_EPILOG; }
  BOOL Has_prolog_epilog() const { return Has_prolog() && Has_epilog(); }
  void Set_has_prolog()   { flags |= CG_LOOP_HAS_PROLOG; }
  void Set_has_epilog()   { flags |= CG_LOOP_HAS_EPILOG; }
  BB  *Prolog_start() const { return prolog_start; }
  void Set_prolog_end(BB *new_prolog_end) { prolog_end = new_prolog_end; }
  BB  *Prolog_end() const { return prolog_end; }
  BB  *Epilog_start() const { return epilog_start; }
  BB  *Epilog_end() const { return epilog_end; }
  BB  *Trip_count_bb() const { return trip_count_bb; }
  BB  *Loop_header() const { return LOOP_DESCR_loophead(loop); }
  TN  *Trip_count_tn() const { return CG_LOOP_Trip_Count(loop); }

  LOOP_DESCR *Loop() const        { return loop; }
  OP_MAP Op_map() const           { return op_map; }
  BOOL Single_BB() const          { return BB_SET_Size(LOOP_DESCR_bbset(loop)) == 1; }
  BOOL Unroll_fully() const       { return unroll_fully; }
  void Set_unroll_fully()         { unroll_fully = TRUE; }
  INT32 Unroll_factor() const     { return unroll_factor; }
  void Set_unroll_factor(INT32 n) { unroll_factor = n; }
#ifdef TARG_IA64
  INT32 Acyclic_len (void) const  { return acyclic_len; }
  void Set_acyclic_len(INT32 len) { acyclic_len = len; }
  INT32 Acyclic_len_wo_dspec (void) const { return acyclic_len_wo_dspec; }
  void Set_acyclic_len_wo_dspec (INT32 len) { acyclic_len_wo_dspec = len; }
#endif

  void Recompute_Liveness();
  bool Determine_Unroll_Fully(BOOL count_multi_bb);
  void Determine_Best_Unit_Iteration_Interval(BOOL can_refit);
  void Determine_Unroll_Factor();
  void Determine_SWP_Unroll_Factor();
  void Build_CG_LOOP_Info(BOOL single_bb);
  void EBO_Before_Unrolling();
  void EBO_After_Unrolling();
  void Print(FILE *fp);
  void Verify();

  CG_LOOP(LOOP_DESCR *);
  ~CG_LOOP();
};

//  Data structure for fixup SWP loops
//
struct SWP_FIXUP {
  BB *prolog;
  BB *body;
  BB *epilog;
  INT control_loc;
  SWP_FIXUP(BB *bb1, BB *bb2, BB *bb3, INT cntrl_loc):
    prolog(bb1),body(bb2),epilog(bb3), control_loc(cntrl_loc) {}
};

typedef vector<SWP_FIXUP> SWP_FIXUP_VECTOR;


#include "tn_map.h"

// Create the mapping from a TN to its first definition 
// in the loop body.
//
struct CG_LOOP_DEF {
  TN_MAP tn_map;
  OP *Get(TN *tn); 
  BOOL Is_invariant(TN *tn);
  CG_LOOP_DEF(LOOP_DESCR *loop);
  CG_LOOP_DEF(BB *body);
  ~CG_LOOP_DEF();
};


// Construct a vector of OP*.  
//   and then uses the index to represent the OP.
//
struct OP_VECTOR {
  typedef int index_type;
  typedef vector<OP *>::iterator iterator;
  vector<OP *> op_vec;
  
  iterator begin() { return op_vec.begin(); }

  iterator end() { return op_vec.end(); }

  index_type size() const { return op_vec.size(); }

  OP* operator[](int i) const { return op_vec[i]; }

  OP* operator[](int i) { return op_vec[i]; }
  
  OP_VECTOR(BB *body) {
    OP *op;
    INT op_num = 0;
    FOR_ALL_BB_OPs(body, op) {
      op_vec.push_back(op);
      op_num++;
    }
  }
};


extern CG_LOOP *Current_CG_LOOP;

extern void Examine_Loop_Info(char *usage_str, BOOL after_presched);

#if defined(TARG_IA64) || defined(TARG_SL)  || defined(TARG_MIPS)
extern void Perform_Loop_Optimizations(void *rgn_loop_update=NULL);

extern BOOL CG_LOOP_Optimize(LOOP_DESCR *loop, SWP_FIXUP_VECTOR& fixup, 
                void **par_rgn, void *rgn_loop_update);
#else
extern void Perform_Loop_Optimizations();

extern BOOL CG_LOOP_Optimize(LOOP_DESCR *loop, vector<SWP_FIXUP>& fixup);
#endif

#if defined(TARG_X8664)
extern void CG_LOOP_Multiversion(LOOP_DESCR *loop, INT num_copies,
                                 MEM_POOL *pool);
#endif

#if defined(TARG_SL)
#define TRACE_ZDL_GEN     0x1
#define TRACE_ZDL_IR      0x2
#define TRACE_ZDL_SEQ_NO  0x4
#define TRACE_ZDL_ALL     0x7
extern void CG_LOOP_zero_delay_loop_gen();
#endif

extern BOOL Perform_SWP(CG_LOOP& cl, SWP_FIXUP_VECTOR& fixup, bool is_doloop);

extern void SWP_Fixup(SWP_FIXUP& fixup);

#endif /* CG_LOOP_INCLUDED */
