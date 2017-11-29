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
 *  Module: cg_dep_graph.h
 *  $Revision: 1.6 $
 *  $Date: 05/12/05 08:59:03-08:00 $
 *  $Author: bos@eng-24.pathscale.com $
 *  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.cg_dep_graph.h $
 *
 *  Revision comments:
 *
 *  4-Apr-1995 - Initial version
 *
 *  Description:
 *  ============
 *
 *  This module implements the data dependence graph within the code
 *  generator.  The dependence graph for a BB or BB_SET is a Directed
 *  Graph whose nodes are OPs and whose edges (arcs) indicate a
 *  dependence between the connected nodes.  CYCLIC graphs have cycles
 *  to represent loop-carried dependences.  Currently, only NON_CYCLIC
 *  graphs are built for BB_SETs.  BB_SETs must obey the same control-
 *  flow constraints as regions (single entry, containing all BBs on
 *  all control flow paths from the entry to all exits).  Dependence
 *  graphs are allocated in the PU memory pool.  Currently, at most one
 *  can exist at any one time.  
 *
 *  The main types exported by this module are CG_DEP_KINDs, ARCs
 *  and ARC_LISTs:
 *
 *   CG_DEP_KIND
 *	Dependency kinds for ARCs.
 *
 *   ARC_LIST
 *	A list of ARCs.  ARC_LISTs can be read (but not written) by:
 *
 *	  ARC *ARC_LIST_first(ARC_LIST *arcs)
 *	    Returns the first ARC in <arcs>.
 *	    (Equivalent to ARC_LIST_node in Ragnarok.)
 *
 *	  ARC_LIST *ARC_LIST_rest(ARC_LIST *arcs)
 *	    Returns list of the rest of the arcs (after the first)
 *	    in <arcs>.  (Equivalent to ARC_LIST_next in Ragnarok.)
 *
 *	  ARC_LIST *OP_preds(OP *op)
 *	  ARC_LIST *OP_succs(OP *op)
 *	    Return list of arcs coming into / out of <op>.
 *	    A NULL value indicates an empty list.
 *
 *	  ARC_LIST *CG_DEP_GTN_Use_Arcs(TN *tn)
 *	    Return list of arcs to local uses of global <tn> in
 *	    the currently computed dependence graph.  These arcs
 *	    all have kind CG_DEP_REGIN.  Neither ARC_latency nor ARC_pred
 *	    can be accessed for these arcs.
 *
 *      and searched by:
 *
 *	  ARC_LIST *ARC_LIST_Find(ARC_LIST *list,CG_DEP_KIND kind,INT16 opnd)
 *	    Searches <list> for the first arc such that ARC_kind(arc)
 *	    is <kind> and either ARC_has_opnd(arc) is FALSE, or
 *	    <opnd> is DONT_CARE, or ARC_opnd(arc) is <opnd>.  If
 *	    found, returns the ARC_LIST starting with <arc>.
 *	    Otherwise returns NULL.
 *
 *	  ARC *ARC_LIST_Find_First(ARC_LIST *list,CG_DEP_KIND kind,INT16 opnd)
 *	    Same as ARC_LIST_Find, but returns the first ARC from
 *	    <list> matching <kind> and <opnd>, or NULL if there's
 *	    no such arc.
 *
 *    ARC
 *	An edge indicating a dependence between two OPs.  ARC attributes
 *	can be read (but not written) with:
 *
 *	  CG_DEP_KIND ARC_kind(ARC *arc)
 *	    Describes the kind of dependence represented. 
 *	    Some examples are CG_DEP_REGIN and CG_DEP_MEMANTI.
 *
 *	  INT16 ARC_latency(ARC *arc)
 *	    Returns the latency between the OPs connected by <arc>.
 *
 *	  UINT8 ARC_omega(ARC *arc)
 *	    Returns the number of loop iterations back spanned by <arc>.
 *	    This is zero for all but cyclic dependences.  An omega of
 *	    MAX_OMEGA implies the actual omega is >= MAX_OMEGA.  (See
 *	    "cg_loop.h" for MAX_OMEGA.)
 *
 *	  UINT8 ARC_opnd(ARC *arc)
 *	    Available only for CG_DEP_REGIN and CG_DEP_REGANTI arcs.  Tells
 *	    which operand is involved in the dependence.  It is an error
 *	    to access this for non-REGIN/REGANTI arcs.
 *	  
 *	  BOOL ARC_has_opnd(ARC *arc)
 *	    Tells whether the ARC_opnd accessor is valid for this arc
 *	    (i.e., whether this is a REGIN or REGANTI arc).
 *	  
 *	  BOOL ARC_is_definite(ARC *arc)
 *	    Available only for memory dependence arcs (see ARC_is_mem,
 *	    below).  Tells whether the dependence in question is definite
 *	    (both addresses known to always be the same).  Always TRUE
 *	    for CG_DEP_SPILLIN arcs.
 *	  
 *	  BOOL ARC_is_dotted(ARC *arc)
 *	    Tells whether the dependence in question is dotted,
 *	    i.e. not always strictly observed.  
 *	  
 *	  BOOL ARC_is_mem(ARC *arc)
 *	    Tells whether the arc is a memory dependence arc (i.e., one
 *	    of CG_DEP_MEMIN/MEMOUT/MEMANTI/MEMVOL/MEMREAD/SPILLIN).
 *	  
 *	  BOOL ARC_is_reg(ARC *arc)
 *	    Tells whether the arc is a register dependence arc (i.e., one
 *	    of CG_DEP_REGIN/REGOUT/REGANTI).
 *	  
 *	  BOOL ARC_is_input(ARC *arc)
 *	    Tells whether the arc is an input (aka flow, true) dependence.
 *	  
 *	  BOOL ARC_is_output(ARC *arc)
 *	    Tells whether the arc is an output dependence.
 *	  
 *	  BOOL ARC_is_anti(ARC *arc)
 *	    Tells whether the arc is an anti-dependence.
 *	  
 *	  OP *ARC_pred(ARC *arc)
 *	    Returns the OP which is the source of the dependence in <arc>.
 *	    (This replaces ARC_tail in Ragnarok.)
 *
 *	  OP *ARC_succ(ARC *arc)
 *	    Returns the OP which is the destination of the dependence
 *	    in <arc>.  (This replaces ARC_head in Ragnarok.)
 *
 *  To compute (or update) the dependence graph for a BB, call:
 *
 *    void CG_DEP_Compute_Graph(BB *bb,
 *				BOOL include_assigned_registers,
 *				BOOL cyclic,
 *				BOOL include_memread_arcs,
 *                              BOOL include_memin_arcs,
 *				BOOL include_control_arcs,
 *                              TN_SET *need_anti_out_dep)
 *
 *    <include_assigned_registers> indicates
 *    whether dependences due solely to register assignments should be
 *    included in the graph.  (Note: dependences due to dedicated
 *    registers are always included).  <cyclic> indicates whether
 *    we're building a cyclic or non-cyclic graph.  The cyclic graph
 *    is intended for cyclic schedulers such as software pipeliners.
 *    It includes loop-carried dependences and does not include anti-
 *    dependences that can be removed by TN renaming (but does include
 *    anti-dependences due to dedicated register assignments since
 *    these can't be renamed).  <include_memread_arcs> tells the graph
 *    builder whether to include memory arcs between dependent loads.
 *    <include_memin_arcs> tells the graph builder whether to include
 *    memory input dependences, i.e a store followed by load(s) dependence.
 *    <include_control_arcs> tells the graph builder whether to include
 *    control arcs between the OP and the branch instruction.
 *
 *    For clarity, the following boolean symbolic constants are defined:
 *	INCLUDE_ASSIGNED_REG_DEPS = TRUE
 *	NO_ASSIGNED_REG_DEPS = FALSE
 *	CYCLIC = TRUE
 *	NON_CYCLIC = FALSE
 *	INCLUDE_MEMREAD_ARCS = TRUE
 *	NO_MEMREAD_ARCS = FALSE
 *      INCLUDE_MEMIN_ARCS = TRUE
 *      NO_MEMIN_ARCS = FALSE
 *	INCLUDE_CONTROL_ARCS = TRUE
 *	NO_CONTROL_ARCS = FALSE
 *
 *    need_anti_out_dep is meaning only when cyclic dep graph is built
 *
 *
 *    void CG_DEP_Compute_Region_Graph(list<BB*> bb_region,
 *				       BOOL include_assigned_registers,
 *				       BOOL include_memread_arcs,
 *				       BOOL include_control_arcs)
 *
 *    This routines computes the dependence graph for a bb_region containing
 *    a chain of sequential blocks (eg. hyperblocks). The essential 
 *    requirement of a bb_region is that it can only have a single entry point
 *    but can have multiple exits. The rest of the parameters are described in
 *    the previous section.
 *
 *  To prune the dependence graph for list of BBs, call:
 *
 *    void CG_DEP_Prune_Dependences(list<BB*> bblist,
 *                                  BOOL prune_predicate_arcs);
 *
 *    The above is used as a special-purpose routine to prune any dependences
 *    of interest. <prune_predicate_arcs> is used to prune predicate 
 *    dependence arcs for guarded OPs which are inherently safe to speculate.
 *
 *    For clarity, the following boolean symbolic constants are defined:
 *	PRUNE_PREDICATE_ARCS = TRUE
 *	NO_PRUNE_PREDICATE_ARCS = FALSE
 *
 *    The following control knobs for debugging are defined:
 *	BOOL CG_DEP_Ignore_LNO (-CG:ignore_lno)
 *	  Ignore LNO dependence info.
 *	BOOL CG_DEP_Ignore_WOPT (-CG:ignore_wopt)
 *	  Ignore WOPT alias info.
 *	BOOL CG_DEP_Addr_Analysis (-CG:addr_analysis)
 *	  Perform address analysis before resorting to WOPT and/or LNO
 *	  to determine memory aliasing relationships.
 *	BOOL CG_DEP_Verify_Mem_Deps (-CG:verify_mem_deps)
 *	  Cross-check memory dependence info (CG's analysis vs LNO/WOPT
 *	  analysis).  If there's a conflict, believe the CG info (since
 *	  its analysis is based on the current code, while the LNO/WOPT
 *	  info was based on the WHIRL IR), and emit a DevWarn showing
 *	  the conflict.
 *	INT32 CG_DEP_Mem_Arc_Pruning (-CG:prune_mem=n)
 *	  Specify the level of pruning of memory arcs.  Each level includes
 *	  all pruning enabled by lower levels:
 *	    level 0: No pruning
 *	    level 1: Pruning of non-cyclic graph
 *	    level 2: Pruning of 0-omega arcs in cyclic graph
 *	    level 3: Pruning of 1-omega arcs in cyclic graph
 *	    level 4: Maximum possible pruning
 *	BOOL CG_DEP_Add_Alloca_Arcs (-CG:add_alloca_arcs) (default TRUE)
 *	  Specify whether or not CG should add MISC arcs between
 *	  stack-ptr definitions and memory ops that might be accessing
 *	  the stack.  This is a temporary workaround for PV 707179.
 *
 *  The currently-computed dependence graph can be dumped to the trace
 *  file with:
 *    void CG_DEP_Trace_Graph(BB *bb)  // trace for a bb.
 *    void CG_DEP_Trace_HB_Graph(list<BB*> bblist)  // trace for a hyperblock.
 *  while the arcs for a single OP (and the OP itself) may be traced with:
 *    void CG_DEP_Trace_Op_Arcs(OP *op)
 *    void CG_DEP_Trace_Op_SCC_Arcs(OP *op) (for SCC arcs - see below)
 *
 *
 *  The identifier (BB_id) of the BB to which the graph applies is also
 *  printed in the trace file.  Individual ARCs can be dumped to the
 *  trace file with:
 *
 *    void CG_DEP_Trace_Arc(ARC *arc, BOOL is_succ, BOOL verbose)
 *	Print dependency <arc> to the trace file.  <is_succ> tells
 *	whether the arc is from a successor list (used only to print
 *	a 'p' or 's' before the arc) and <verbose> tells whether to
 *	print the end nodes also.  <is_succ> is ignored if <verbose>
 *	is true.
 *
 *  To determine whether the graph for BB is cyclic, use:
 *    BOOL CG_DEP_Graph_Is_Cyclic(BB *bb)
 *	Return TRUE iff there is a currently existing dep graph for <bb>
 *	that is CYCLIC.
 *
 *  The following functions are provided to determine the aliasing
 *  relationship between two memory OPs (from arbitrary BBs):
 *
 *    BOOL CG_DEP_Mem_Ops_Alias(OP *memop1, OP *memop2, BOOL *identical)
 *      Requires: OP_load(memop1) || OP_store(memop1)
 *		  OP_load(memop2) || OP_store(memop2)
 *      If <memop1> and <memop2> always access completely distinct memory
 *      locations, returns FALSE.  Otherwise returns TRUE, and if
 *	<identical> isn't NULL, indicates whether the references are
 *      to the exact same location and amount of data.
 *
 *    BOOL CG_DEP_Mem_Ops_Offsets_Overlap(OP *memop1, OP *memop2,
 *					  BOOL *identical);
 *      Requires: OP_load(memop1) || OP_store(memop1)
 *		  OP_load(memop2) || OP_store(memop2)
 *      Looks only at the offsets (ignoring base TN) of <memop1> and
 *      <memop2>, returning FALSE if the offsets might overlap (from
 *	the same base).  Otherwise returns TRUE, and if <identical>
 *	isn't NULL, indicates whether they refer to the same location
 *	and amount of data.
 *
 *  and this one to determine whether (and how) an arbitrary op might be
 *  aliased by a call:
 *
 *    BOOL CG_DEP_Call_Aliases(OP *call_op, OP *op, BOOL read, BOOL write);
 *      Requires: OP_call(call_op) && (read || write)
 *	Returns TRUE iff <op> is aliased by <call_op>.  Checks for read
 *	aliases iff <read>, and write aliases iff <write>.
 *
 *  The following latency-calculation function is exposed so that
 *  other components can calculate latencies between dependent OPs
 *  in different BBs.  (This is currently used by the local scheduler
 *  to schedule OPs whose results are live-out.)  It computes the
 *  latency from <pred> to <succ> of the given <kind> (typically
 *  CG_DEP_REGIN) using the given operand number <opnd> (used only for
 *  REGIN/REGANTI kinds).
 *
 *    INT16 CG_DEP_Latency(OP *pred, OP *succ, CG_DEP_KIND kind, UINT8 opnd)
 *
 *  To inquire about the latencies of operators without OPs, use:
 *
 *    INT16 CG_DEP_Oper_Latency(TOP pred_oper, TOP succ_oper,
 *				CG_DEP_KIND kind, UINT8 opnd)
 *
 *
 *  To delete the dependence graph for a BB or a HB (hyperblock), call:
 *
 *    void CG_DEP_Delete_Graph(void *item)
 *
 *  Software pipelining and Hyberblock scheduling require "prebranch" 
 *  (CG_DEP_PREBR) arcs in
 *  the cyclic graph.  To avoid the expense of maintaining these all the
 *  time, we provide: <comp_func> is a filter function to provide clients
 *  with ability to care only for dependences they care:
 *    void CG_DEP_Add_PREBR_Arcs(BB* bb, COMPARE_FUNCTION comp_func, 
 *                               BOOL ignore_latency);
 *
 *  Hyberblock scheduling also requires "postbranch" (CG_DEP_POSTBR) arcs
 *  between the predecessor branch node and the successor op node (in the
 *  context of single-entry multiple-exit BBs). 
 *    void CG_DEP_Add_POSTBR_Arcs(list<BB*>, COMPARE_FUNCTION comp_func, 
 *                                BOOL ignore_latency);
 *
 *  Special routines are provided so that the CG_LOOP module can add
 *  and delete SCC (Strongly Connected Component) arcs:
 *
 *    void CG_DEP_Add_SCC_Arc(OP *pred, OP *succ, UINT8 omega, INT16 latency,
 *			      ARC_LIST **scc_ancestor_list,
 *			      ARC_LIST **scc_descendent_list)
 *	Construct a new SCC arc from <pred> to <succ> with the given
 *	<omega> and <latency>, then attach it to <scc_ancestor_list>
 *	and <scc_descendent_list>.
 *
 *    void CG_DEP_Delete_SCC_Arcs(ARC_LIST *arcs)
 *	Free the arcs in <arcs> so they can be reused.
 *
 *    void CG_DEP_Set_SCC_ARC_omega(ARC *arc, UINT8 omega)
 *	Change the omega on SCC arc <arc> to <omega>.
 *
 *   void CG_DEP_Detach_Arc(ARC *arc)
 *     Remove <arc> from OP_preds(ARC_succ(arc)) (from_succ) and/or
 *     OP_succs(ARC_pred(arc)) (from_pred).  detach_arc does both.
 *
 *
 * Generic utilitiy routines which query predicate relations from any two
 * given OPs (represented as const void*)
 *
 *   BOOL OP_has_subset_predicate(const void *value1, const void *value2)
 *   Checks to see if the <value2> qualifying predicate is a subset 
 *   predicate of <value1> qualifying predicate.
 *
 *   BOOL OP_has_disjoint_predicate(const void *value1, const void *value2)
 *   Checks to see if the <value2> qualifying predicate and <value1>
 *   qualfying predicate are disjoint.
 *
 * =======================================================================
 * ======================================================================= */

#ifndef CG_DEP_GRAPH_INCLUDED
#define CG_DEP_GRAPH_INCLUDED

#ifdef _KEEP_RCS_ID
static char *cg_dep_graph_rcs_id = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.cg_dep_graph.h $ $Revision: 1.6 $";
#endif /* _KEEP_RCS_ID */

#include <list>
#include "op.h"
#include "op_map.h"
#include "op_list.h"
#include "tn.h"
#include "tn_set.h"
#include "cg_dep_graph_update.h"

/* Exported symbolic constants. */

#define INCLUDE_ASSIGNED_REG_DEPS TRUE
#define NO_ASSIGNED_REG_DEPS FALSE

#define CYCLIC TRUE
#define NON_CYCLIC FALSE

#define INCLUDE_MEMREAD_ARCS TRUE
#define NO_MEMREAD_ARCS FALSE

#define INCLUDE_MEMIN_ARCS TRUE
#define NO_MEMIN_ARCS FALSE

#define INCLUDE_CONTROL_ARCS TRUE
#define NO_CONTROL_ARCS FALSE

#define PRUNE_PREDICATE_ARCS TRUE
#define NO_PRUNE_PREDICATE_ARCS FALSE

#if defined(TARG_IA64)
#define delete_gtn_use_arcs() { TN_MAP_Delete(gtn_use_map); gtn_use_map = NULL; }
#endif

/* Exported types and accessors */

/* Define possible kinds for ARCs in the dependency graph:
 */
typedef enum cg_dep_kind {
  CG_DEP_REGIN,		/* Register flow (true dependency): the succ uses
			 * a TN defined by the pred. The pred must access
			 * its operand after the succ writes it. */
  CG_DEP_REGOUT,	/* Register output: the succ redefines a TN defined
			 * by the pred. The succ must write its operand
			 * after the pred writes it. */
  CG_DEP_REGANTI,	/* Register anti-dependency: the succ redefines
			 * a TN used by the pred. */
  CG_DEP_MEMIN,		/* Memory flow (true dependency): the succ may use
			 * a memory location defined by the pred. The
			 * succ must access its operand after the pred
			 * writes it. */
  CG_DEP_MEMOUT,	/* Memory output: the succ may redefine a memory
			 * location define by the pred. The succ must write
			 * its operand after the pred writes it. */
  CG_DEP_MEMANTI,	/* Memory anti-dependency: the succ may redefine
			 * a memory location used by the pred. The succ
			 * must write its operand after the pred uses it. */
  CG_DEP_MEMVOL,	/* Memory input (volatile data): the succ and pred
			 * both read the same memory location, and the
			 * reference order must be preserved. */
  CG_DEP_MEMREAD,	/* Memory definite read-read (for r/r elimination) */
  CG_DEP_SPILLIN,	/* Dependence between a spill and restore */
  CG_DEP_PREFIN,	/* prefetch memory flow */
  CG_DEP_PREFOUT,	/* prefetch memory output */
  CG_DEP_PREBR,		/* Pre-branch: the succ is a branch operation.
			 * The tail must be issued before the control
			 * transfer takes effect, perhaps in the shadow. */
  CG_DEP_POSTBR,	/* Post-branch: the pred is a branch operation.
			 * The head must be issued before the control
			 * transfer takes effect */
  CG_DEP_SCC,		/* Strongly connected component arcs */

#ifdef TARG_IA64
  CG_DEP_PRECHK,        /* Pre-chk: the succ is a check op  */
  CG_DEP_POSTCHK,       /* Post-chk: the pred is a check op */
  CG_DEP_CTLSPEC,       /* control speculation, a special dep */ 
			/* between a cmp and its guarded operations. */
#endif
  CG_DEP_MISC,		/* Everything else: the pred must be issued
			 * before the succ. */
  CG_DEP_NUM_KIND	/* Number (count) of defined CG_DEP_KINDs */
} CG_DEP_KIND;


/* In addition to the essential ARC attributes (pred, succ, latency,
 * kind, opnd), this implementation of ARCs keeps two "next" pointers:
 * one for the next predecessor and one for the next successor.  These
 * are used because each ARC is a member of one OP's predecessor list
 * and one OP's successor list, so this way we can share the essential
 * ARC info among the two lists.  This takes exactly the same amount
 * of memory as keeping a copy of the essential ARC info in both
 * lists, requires no external linkage, and makes memory management
 * easier than representing ARC_LISTs by dynamic vectors.  (Assuming
 * pointers are 32 bits, that is.  If we ever decide to use 64-bit
 * pointers, the dynamic vector approach will require less memory not
 * considering vector reuse efficiency.)  */

typedef struct arc {
  OP            *pred;		/* the predecessor */
  OP            *succ;		/* the successor */
  mINT16        latency;	/* latency in cycles from pred to succ */
  mUINT8        omega;		/* iteration distance for loop-carried deps */
  mUINT16       kind_def_opnd;	/* kind is LOW 8 bits, definite is next bit,
				 * dotted edge is the next bit, which tells
				 * if the edge is not always strict and opnd 
				 * is the HIGH 4 bits */
  struct arc    *next[2];	/* next ARC in pred/succ list, respectively */
} ARC;

/* These accessors are read-only.  The "+0" prevents use as lhs.  */

#define ARC_pred(arc)		(((arc)->pred)+0)
#define ARC_succ(arc)		(((arc)->succ)+0)
#define ARC_latency(arc)	((arc)->latency+0)
#define ARC_omega(arc)		((arc)->omega+0)
#define ARC_kind(arc)		((CG_DEP_KIND)((arc)->kind_def_opnd & 0xff))
#define ARC_is_definite(arc)	(((arc)->kind_def_opnd >> 8) & 1)
#define ARC_is_dotted(arc)	(((arc)->kind_def_opnd >> 9) & 1)
#define ARC_opnd(arc)		((arc)->kind_def_opnd >> 12)

#define Set_ARC_is_dotted(arc, val) {                           \
  ARC *_arc = arc;                                              \
  _arc->kind_def_opnd &= ~0x0200;                               \
  _arc->kind_def_opnd |= (val != 0) << 9;                       \
}

/* Because ARCs include next pred/succ fields, an ARC_LIST needs to
 * represent only the current first ARC in the list, plus a bit to
 * indicate whether to use the next[0] (pred) or next[1] (succ) field
 * when iterating.  (The ARC_LIST_rest iterator applies to both pred
 * and succ lists.  This allows us to walk ARC_LISTs the same
 * regardless of whether they come from succ or pred lists.)  Since
 * the interface uses ARC_LIST pointers, and we know the memory they
 * reference will be at least word-aligned, we'll use the least
 * significant bit of the pointer to indicate which next pointer to
 * use and strip away this bit before dereferencing.
 */
typedef ARC ARC_LIST;

#define ARC_LIST_first(list)		((ARC *)(((INTPTR)(list)) & ~1))

/* Make ARC_LIST_rest an inline function so <list> isn't
 * evaluated twice.
 */
inline ARC_LIST* ARC_LIST_rest(ARC_LIST *list)
{
  UINT8 which = (INTPTR)list & 1;
  ARC_LIST *result = ARC_LIST_first(list)->next[which];
  /* It's very common for loops to delete/detach arcs before using
   * ARC_LIST_rest, but this implementation does not allow that,
   * so check for this error.  See also detach_arc_from_succ/pred
   * and delete_arc.
   */
  DevAssert((INTPTR)result != ~0,
	    ("can't follow link from deleted/detached arc"));
  return result == NULL ? result : (ARC_LIST *)((INTPTR)result | which);
}

inline BOOL ARC_has_opnd(ARC *arc)
{
  CG_DEP_KIND kind = ARC_kind(arc);
  return kind == CG_DEP_REGIN || kind == CG_DEP_REGANTI;
}

inline BOOL ARC_is_mem(ARC *arc)
{
  CG_DEP_KIND kind = ARC_kind(arc);
  return kind == CG_DEP_MEMIN || kind == CG_DEP_MEMOUT || 
    kind == CG_DEP_MEMANTI || kind == CG_DEP_MEMVOL || 
    kind == CG_DEP_MEMREAD || kind == CG_DEP_SPILLIN;
}

inline BOOL ARC_is_reg(ARC *arc)
{
  CG_DEP_KIND kind = ARC_kind(arc);
  return kind == CG_DEP_REGIN || kind == CG_DEP_REGOUT || kind == CG_DEP_REGANTI;
}

inline BOOL ARC_is_input(ARC *arc)
{
  CG_DEP_KIND kind = ARC_kind(arc);
  return kind == CG_DEP_REGIN || kind == CG_DEP_MEMIN || 
    kind == CG_DEP_MEMVOL || kind == CG_DEP_SPILLIN;
}

inline BOOL ARC_is_output(ARC *arc)
{
  CG_DEP_KIND kind = ARC_kind(arc);
  return kind == CG_DEP_REGOUT || kind == CG_DEP_MEMOUT;
}

inline BOOL ARC_is_anti(ARC *arc)
{
  CG_DEP_KIND kind = ARC_kind(arc);
  return kind == CG_DEP_REGANTI || kind == CG_DEP_MEMANTI;
}

/* ---------------------------------------------------------------------
 * Some internal globals and types (not exported):
 *
 * _CG_DEP_OP_INFO is a struct holding the per-OP dependence info.
 *
 * _cg_dep_op_info maps each BB to a BB_OP_MAP holding _CG_DEP_OP_INFO
 * for each OP in the BB.  Usually we'll have _CG_DEP_OP_INFO for only
 * a single BB at a time, so this is much more efficient than having a
 * single (global) OP_MAP.  May want to experiment with other representation
 * if this is no longer true.
 */
 
extern BB_MAP _cg_dep_op_info;

typedef struct {
  ARC_LIST *preds;
  ARC_LIST *succs;
} _CG_DEP_OP_INFO;

inline _CG_DEP_OP_INFO* _CG_DEP_op_info(OP *op)
{
  BB_OP_MAP omap = (BB_OP_MAP) BB_MAP_Get(_cg_dep_op_info, OP_bb(op));
  _CG_DEP_OP_INFO *info = (_CG_DEP_OP_INFO *) BB_OP_MAP_Get(omap, op);
  return info;
}

inline _CG_DEP_OP_INFO* _CG_DEP_op_info_valid(OP *op)
{
  _CG_DEP_OP_INFO *info = _CG_DEP_op_info(op);
  DevAssert(info, ("OP has no CG_DEP info"));
  return info;
}

/* More exported declarations and macros  */

#ifdef DevAssert_On
#define OP_preds(op)		(_CG_DEP_op_info_valid(op)->preds+0)
#define OP_succs(op)		(_CG_DEP_op_info_valid(op)->succs+0)
#else
#define OP_preds(op)		(_CG_DEP_op_info(op)->preds+0)
#define OP_succs(op)		(_CG_DEP_op_info(op)->succs+0)
#endif

//  Data structure to manage memory allocation for CYCLIC DEP GRAPH
//
class CYCLIC_DEP_GRAPH {
private:
  BB *_body;
public:
  CYCLIC_DEP_GRAPH( BB *body, MEM_POOL *pool );
  ~CYCLIC_DEP_GRAPH();
};


#define DONT_CARE -1
ARC_LIST *ARC_LIST_Find(ARC_LIST *list, CG_DEP_KIND kind, INT16 opnd);

inline ARC *ARC_LIST_Find_First(ARC_LIST *list, CG_DEP_KIND kind, INT16 opnd)
{
  ARC_LIST *arcs = ARC_LIST_Find(list, kind, opnd);
  return arcs ? ARC_LIST_first(arcs) : NULL;
}

typedef BOOL (*COMPARE_FUNCTION)(const void*, const void*);

void CG_DEP_Compute_Graph(struct bb      *bb,
			  BOOL           assigned_regs,
			  BOOL           cyclic,
			  BOOL           memread_arcs,
			  BOOL           memin_arcs,
			  BOOL           control_arcs,
			  TN_SET         *need_anti_out_dep);

void CG_DEP_Compute_Region_Graph(std::list<BB*>   bb_region,
				 BOOL        assigned_regs,
				 BOOL        memread_arcs,
				 BOOL        control_arcs);

void CG_DEP_Prune_Dependence_Arcs(std::list<BB*>   bblist,
				  BOOL prune_predicate_arcs,
				  BOOL trace);

void CG_DEP_Trace_Graph(BB *bb);
#pragma mips_frequency_hint NEVER CG_DEP_Trace_Graph

void CG_DEP_Trace_HB_Graph(std::list<BB*> bblist);
#pragma mips_frequency_hint NEVER CG_DEP_Trace_HB_Graph

void CG_DEP_Trace_Arc(ARC *arc, BOOL is_succ, BOOL verbose);
#pragma mips_frequency_hint NEVER CG_DEP_Trace_Arc

void CG_DEP_Trace_Op_Arcs(OP *op);
#pragma mips_frequency_hint NEVER CG_DEP_Trace_Op_Arcs

void CG_DEP_Trace_Op_SCC_Arcs(OP *op);
#pragma mips_frequency_hint NEVER CG_DEP_Trace_Op_SCC_Arcs

void CG_DEP_Delete_Graph(void *item);

#ifdef TARG_IA64
INT16 CG_DEP_Oper_cycle(TOP oper, CG_DEP_KIND kind);  
#endif

INT16 CG_DEP_Latency(OP *pred, OP *succ, CG_DEP_KIND kind, UINT8 opnd);
INT16 CG_DEP_Oper_Latency(TOP              pred_oper, 
			  TOP              succ_oper,
			  CG_DEP_KIND      kind, 
			  UINT8            opnd);

void CG_DEP_Add_SCC_Arc(OP             *pred, 
			OP             *succ, 
			UINT8          omega, 
			INT16          latency,
			ARC_LIST       **scc_ancestor_list,
			ARC_LIST       **scc_descendent_list);

void CG_DEP_Delete_SCC_Arcs(ARC_LIST *arcs);

void CG_DEP_Set_SCC_ARC_omega(ARC *arc, UINT8 omega);

extern BOOL CG_DEP_Ignore_LNO;
extern BOOL CG_DEP_Ignore_WOPT;
extern BOOL CG_DEP_Addr_Analysis;
extern BOOL CG_DEP_Verify_Mem_Deps;
extern INT32 CG_DEP_Mem_Arc_Pruning;
extern BOOL CG_DEP_Add_Alloca_Arcs;
extern BOOL CG_DEP_Relax_Xfer_Dependence;
extern BOOL CG_DEP_Adjust_OOO_Latency;
extern BOOL CG_DEP_Prune_Dependence;

BOOL CG_DEP_Add_Same_Res_Arcs();
void CG_DEP_Remove_Same_Res_Arcs();

void CG_DEP_Add_Op_Same_Res_Arcs(OP *op);
void CG_DEP_Remove_Op_Same_Res_Arcs(OP *op);

void CG_DEP_Add_PREBR_Arcs(BB *bb, COMPARE_FUNCTION comp_func, BOOL ignore_latency);

BOOL CG_DEP_Graph_Is_Cyclic(BB *bb);

ARC_LIST *CG_DEP_GTN_Use_Arcs(TN *tn);

BOOL CG_DEP_Mem_Ops_Alias(OP *memop1, OP *memop2, BOOL *identical);

BOOL CG_DEP_Mem_Ops_Offsets_Overlap(OP *memop1, OP *memop2, BOOL *identical);

BOOL CG_DEP_Call_Aliases(OP *call_op, OP *op, BOOL read, BOOL write);

BOOL CG_DEP_Can_OP_Move_Across_Call(OP *cur_op, OP *call_op, BOOL forw, BOOL Ignore_TN_Dep);

extern BOOL OP_has_subset_predicate(const void *value1, const void *value2);
extern BOOL OP_has_disjoint_predicate(const OP *value1, const OP *value2);

#if defined(TARG_IA64)
inline BOOL TN_is_predicate (TN * tn) 
{
  return TN_is_register(tn) && TN_register_class(tn) == ISA_REGISTER_CLASS_predicate; 
}
#endif

extern void CG_DEP_Detach_Arc(ARC *arc);

#if defined(TARG_IA64) || defined(TARG_SL) || defined(TARG_MIPS)
extern ARC *new_arc(CG_DEP_KIND kind, OP *pred, OP *succ, UINT8 omega,
		    UINT8 opnd, BOOL is_definite);
#endif

#endif /* CG_DEP_GRAPH_INCLUDED */
