/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
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


/* ====================================================================
 *
 * Module: freq.cxx
 * $Revision: 1.9 $
 * $Date: 05/12/05 08:59:06-08:00 $
 * $Author: bos@eng-24.pathscale.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.freq.cxx $
 *
 * Description:
 *
 * Compute basic block and edge frequencies by estimating and/or
 * feedback. Based on: "Static Branch Frequency and Program Profile
 * Analysis" by Wu and Larus.
 *
 * ====================================================================
 */

#include <math.h>
#include <alloca.h>
#include <stack>

#include "defs.h"
#include "config.h"
#include "errors.h"
#include "mempool.h"
#include "tracing.h"
#include "glob.h"
#include "config_asm.h"
#include "bitset.h"
#include "bb.h"
#include "variants.h"
#include "bb_set.h"
#include "bb_map.h"
#include "dominate.h"
#include "region_util.h"
#include "cg_region.h"
#include "cg_flags.h"
#include "cflow.h"	/* for tracing flags */
#include "annotations.h"
#include "cg.h"		/* for CG_PU_Has_Feedback */
#include "whirl2ops.h"
#include "note.h"
#include "findloops.h"
#include "cgtarget.h"
#include "label_util.h"
#include "fb_whirl.h"
#include "DaVinci.h"
#include "freq.h"
#ifdef TARG_IA64
#include "cg_flags.h"
#include "ipfec_options.h"
#endif

/* ====================================================================
 * ====================================================================
 *
 * Global data
 *
 * ====================================================================
 * ====================================================================
 */

       BOOL    FREQ_freqs_computed; // True if freqs computed for region
static BB_SET *Never_BBs;	// Set of BBs having "never" freq pragmas
static BB_SET *Frequent_BBs;	// Set of BBs having "frequent" freq pragmas
static BB_SET *LMV_Precond_BBs;
static float   Frequent_Never_Ratio;
static BB_MAP  dfo_map;		// Depth-first order mapping
static BB    **dfo_vec;		// Vector of BBs ordered depth-first
static INT32   max_dfo_id;	// Max value in dfo_map
static float   EH_Freq;		// Freq that exc hndlrs are executed
#ifdef KEY
static float   Non_Local_Target_Freq;	// Freq that non-local targs are exec'd.
#endif
static LOOP_DESCR *loop_list;	// Loop descriptors for the PU


/* ====================================================================
 * ====================================================================
 *
 * Utility functions
 *
 * ====================================================================
 * ====================================================================
 */

/* ====================================================================
 *
 * Initialize_Depth_First_Info
 *
 * Initialize depth-first-map info for the PU.
 *
 * ====================================================================
 */
static void
Initialize_Depth_First_Info(MEM_POOL *pool)
{
  BB *bb;
  dfo_map = BB_Depth_First_Map(NULL, NULL);
  dfo_vec = TYPE_MEM_POOL_ALLOC_N(BB *, pool, PU_BB_Count);
  max_dfo_id = 0;
  for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    INT32 dfo_id = BB_MAP32_Get(dfo_map, bb);
    DevAssert(dfo_id >= 0 && dfo_id <= PU_BB_Count, ("bad <dfo_map> value"));
    if (dfo_id > 0) {
      max_dfo_id = MAX(dfo_id, max_dfo_id);
      dfo_vec[dfo_id - 1] = bb;
    }
  }
}


/* ====================================================================
 *
 * Finalize_Depth_First_Info
 *
 * Finish using depth-first-map info for the PU.
 *
 * ====================================================================
 */
inline void
Finalize_Depth_First_Info(void)
{
  BB_MAP_Delete(dfo_map);
}


/* ====================================================================
 * ====================================================================
 *
 * Control-flow graph creation and manipulation
 *
 * The CG control-flow graph data structures are not sufficient for
 * the algorithms here. Additional data needs to be kept with each
 * edge, and the fact that there is a node to describe the same edge
 * from the predecessor and successor vertex would complicate matters.
 *
 * To solve these problems, a parallel control-flow graph is created.
 *
 * ====================================================================
 * ====================================================================
 */

/* The following data structure describes an edge in the graph:
 */
typedef struct edge {
  struct edge *next_succ;	/* Next edge on successor chain */
  BB *succ;			/* The successor BB of this edge */
  BBLIST *slst;			/* The real CFG node for the succ edge */
  struct edge *next_pred;	/* Next edge on predecessor chain */
  BB *pred;			/* The predecessor BB of this edge */
  BBLIST *plst;			/* The real CFG node for this pred edge */
  double prob;			/* The probability this edge is taken (0..1) */
  double back_prob;		/* Back-edge probability, i.e. the probability
				 * that control passes from the head of
				 * the loop and through this edge.
				 */
  mUINT16 flags;		/* Edge flags */
} EDGE;

/* EDGE accessors:
 */
#define EDGE_next_succ(e)	(e->next_succ)
#define EDGE_succ(e)		(e->succ)
#define EDGE_slst(e)		(e->slst)
#define EDGE_next_pred(e)	(e->next_pred)
#define EDGE_pred(e)		(e->pred)
#define EDGE_plst(e)		(e->plst)
#define EDGE_prob(e)		(e->prob)
#define EDGE_back_prob(e)	(e->back_prob)
#define EDGE_flags(e)           (e->flags)

#define EF_PROB_FB_BASED        0x0001
#define EF_FB_PROPAGATED	0x0002
#define EF_PROB_HINT_BASED	0x0004    /* bug 6693 */

/* Indicate if this edge probability is based on feedback.
 */
#define EDGE_prob_fb_based(e)   (EDGE_flags(e) & EF_PROB_FB_BASED)
#define Set_EDGE_prob_fb_based(e)   (EDGE_flags(e) |= EF_PROB_FB_BASED)
#define Reset_EDGE_prob_fb_based(e) (EDGE_flags(e) &= ~EF_PROB_FB_BASED)

/* Indicate if freq info has been propagated down this path.
 */
#define EDGE_fb_propagated(e)	(EDGE_flags(e) & EF_FB_PROPAGATED)
#define Set_EDGE_fb_propagated(e)   (EDGE_flags(e) |= EF_FB_PROPAGATED)
#define Reset_EDGE_fb_propagated(e) (EDGE_flags(e) &= ~EF_FB_PROPAGATED)

/* Indicate if this edge probability is based on user hint, through
  pragma or builtins. */
#define EDGE_prob_hint_based(e)   (EDGE_flags(e) & EF_PROB_HINT_BASED)
#define Set_EDGE_prob_hint_based(e)   (EDGE_flags(e) |= EF_PROB_HINT_BASED)
#define Reset_EDGE_prob_hint_based(e) (EDGE_flags(e) &= ~EF_PROB_HINT_BASED)

/* Since we don't ever need to modify the CFG, we simply preallocate
 * a vector for chains of successor and predecessor edges, both indexed
 * by BB_id.
 */
static EDGE **succ_edges;
static EDGE **pred_edges;

/* Indicate when we're using the parallel edge structures:
 */
static BOOL using_EDGEs;

/* Accessors for successor and predecessor chains.
 */
#define BB_succ_edges(bb) (succ_edges[BB_id(bb)])
#define BB_pred_edges(bb) (pred_edges[BB_id(bb)])

/* The following iterate over the edges in the successor or predecessor
 * chains for a given BB.
 */
#define FOR_ALL_SUCC_EDGES(b, e) \
  for ((e) = BB_succ_edges(b); (e) != NULL; (e) = EDGE_next_succ(e))
#define FOR_ALL_PRED_EDGES(b, e) \
  for ((e) = BB_pred_edges(b); (e) != NULL; (e) = EDGE_next_pred(e))


/* ====================================================================
 *
 * Is_Loop_Back_Edge
 * 
 * Return a boolean value to indicate if an edge is a back edge to
 * a loop header.
 *
 * ====================================================================
 */
inline BOOL Is_Loop_Back_Edge(EDGE *e)
{
  return EDGE_succ(e) == BB_loop_head_bb(EDGE_pred(e));
}


/* ====================================================================
 *
 * BB_Find_Succ_Edge
 * 
 * Return the EDGE structure for the edge from <bb> to <succ>.
 * Return NULL if there is no such edge.
 *
 * ====================================================================
 */
static EDGE *
BB_Find_Succ_Edge(BB *bb, BB *succ)
{
  EDGE *e;
  FOR_ALL_SUCC_EDGES(bb, e) if (EDGE_succ(e) == succ) break;
  return e;
}


/* ====================================================================
 *
 * Initialize_Freq_Edges
 * 
 * Create the expanded, parallel CFG we'll use for frequency computations.
 *
 * ====================================================================
 */
static void
Initialize_Freq_Edges(void)
{
  BB *bb;

  /* Create the successor and predecessor chain vectors.
   */
  succ_edges = TYPE_MEM_POOL_ALLOC_N(EDGE *,
				     &MEM_local_pool,
				     PU_BB_Count + 2);
  pred_edges = TYPE_MEM_POOL_ALLOC_N(EDGE *,
				     &MEM_local_pool,
				     PU_BB_Count + 2);

  /* For each BB in the CFG, create one EDGE data structure
   * for each successor/predecessor edge pair.
   */
  for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    BBLIST *slst;
    BBLIST *plst;

    /* Create an EDGE for each successor and then find the matching
     * predecessor edge.
     */
    FOR_ALL_BB_SUCCS(bb, slst) {
      EDGE *edge = TYPE_MEM_POOL_ALLOC(EDGE, &MEM_local_nz_pool);
      BB *succ = BBLIST_item(slst);

      /* So we don't read uninitialized garbage if we encounter
       * a non-reducible CFG.
       */
      EDGE_prob(edge) = 0.0;

      EDGE_flags(edge) = 0; // freq_fb_based set by FREQ_Incorporate_Feedback
      EDGE_next_succ(edge) = BB_succ_edges(bb);
      EDGE_slst(edge) = slst;
      EDGE_succ(edge) = succ;
      BB_succ_edges(bb) = edge;
#ifdef KEY
      if (BBLIST_prob(slst) != 0.0 &&
          BBLIST_prob_hint_based(slst)) {
        EDGE_prob(edge) = BBLIST_prob(slst);
        Set_EDGE_prob_hint_based(edge);
      }
#endif

      FOR_ALL_BB_PREDS(succ, plst) {
        if (BBLIST_item(plst) == bb) break;
      }
      FmtAssert(plst,
	        ("couldn't find matching pred for succ: BB:%d -> BB:%d",
		 BB_id(bb), BB_id(succ)));

      EDGE_next_pred(edge) = BB_pred_edges(succ);
      EDGE_plst(edge) = plst;
      EDGE_pred(edge) = bb;
      BB_pred_edges(succ) = edge;
    }
  }

  using_EDGEs = TRUE;
}


/* ====================================================================
 *
 * Finalize_Freq_Edges
 * 
 * We're done with the parallel CFG. Copy the edge frequencies to the
 * real CFG.
 *
 * ====================================================================
 */
static void
Finalize_Freq_Edges(void)
{
  BB *bb;
  static const union { INT32 i; float f; } NaN_u = { 0x7fbfffff };
  const float NaN = NaN_u.f;

  for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    EDGE *edge;

    FOR_ALL_SUCC_EDGES(bb, edge) {
      BBLIST *slst = EDGE_slst(edge);
      BBLIST_prob(slst) = EDGE_prob(edge);
      if (EDGE_prob_fb_based(edge)) {
	Set_BBLIST_prob_fb_based(slst);
      } else {
	Reset_BBLIST_prob_fb_based(slst);
      }

      /* We aren't currently computing the pred edge probs, so
       * poison them to make sure people will notice if they use them.
       */
      BBLIST_prob(EDGE_plst(edge)) = NaN;
    }
  }

  using_EDGEs = FALSE;

  if (CG_warn_bad_freqs) {
    FREQ_Check_Consistency("Compute_BB_Frequencies");
  }
}


/* ====================================================================
 * ====================================================================
 *
 * Tracing
 *
 * ====================================================================
 * ====================================================================
 */

/* ====================================================================
 *
 * Trace_Frequencies
 * 
 * For each BB in the CFG the following is printed:
 *	- predecessor ids, frequencies and probabilities;
 *	- the BB's id and frequency;
 *	- successor ids, frequencies and probabilities.
 * Formatted as follows:
 *
 *	p	BB:1 <freq>(<probability>) ...
 *	BB:2 frequency = <freq>
 *	s	BB:3 <freq>(<probability>) ...
 *
 *
 * ====================================================================
 */
static void
Trace_Frequencies(void)
{
  BB *bb;
  enum {EDGES_PER_LINE = 3};

  for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    INT n;
    const char *s;
    EDGE *edge;

    /* Predecessors...
     */
    n = EDGES_PER_LINE;
    FOR_ALL_PRED_EDGES(bb, edge) {
      BB *pred = EDGE_pred(edge);
      s = ", ";
      if (n == EDGES_PER_LINE) {
	n = 0;
	s = "\np\t";
      }
      n++;
      fprintf(TFile, "%sBB:%d %#.2f(%#.2f)",
	      s, BB_id(pred), BB_freq(pred) * EDGE_prob(edge), EDGE_prob(edge));
    }

    /* The BB...
     */
    fprintf(TFile, "\nBB:%d freq = %#.2f%s",
	    BB_id(bb),
	    BB_freq(bb),
	    BB_freq_fb_based(bb) ? " (fb based)" : "");

    /* And the successors.
     */
    n = EDGES_PER_LINE;
    FOR_ALL_SUCC_EDGES(bb, edge) {
      BB *succ = EDGE_succ(edge);
      s = ", ";
      if (n == EDGES_PER_LINE) {
	n = 0;
	s = "\ns\t";
      }
      n++;
      fprintf(TFile, "%sBB:%d %#.2f(%#.2f)",
	      s, BB_id(succ), BB_freq(succ) * EDGE_prob(edge), EDGE_prob(edge));
    }

    fprintf(TFile, "\n");
  }
}


/* ====================================================================
 * ====================================================================
 *
 * Branch prediction heuristic functions
 *
 * ====================================================================
 * ====================================================================
 */

/* Define the probabilities of "taking the branch" for each heuristic.
 */
#define PROB_LBH 0.88		/* Loop Branch Heuristic */
#define PROB_LEH 0.80		/* Loop Exit Heuristic */
#define PROB_LHH 0.75		/* Loop Header Heuristic */
#define PROB_CH  0.78		/* Call Heuristic */
#define PROB_RH  0.72		/* Return Heuristic */
#define PROB_OH  0.84		/* Opcode Heuristic */
#define PROB_PH  0.60		/* Pointer Heuristic */
#define PROB_SH  0.55		/* Store Heuristic */
#ifdef KEY	// bug 8546
#define PROB_SBH 0.04		/* Sequential Branch Heuristic */
#endif

/* Declare the interface of a heuristic function. A heuristic function
 * analyzes the branch and returns a boolean value to indicate if the
 * heuristic applies. When it does, it also returns the probability
 * that successor-1 is taken.
 */
typedef BOOL (*heuristic_func)(
  BB *bb,			/* BB containing the branch */
  BB *s1,			/* Successor 1 */
  BB *s2,			/* Successor 2 */
  LOOP_DESCR *loops,		/* List of loops */
  double *s1_taken_prob);	/* Probability succ 1 is taken (returned) */

/* Forward decls for the heuristic functions (so we can put the vector first):
 */
static BOOL Call_Heuristic(BB *, BB *, BB *, LOOP_DESCR *, double *);
static BOOL Return_Heuristic(BB *, BB *, BB *, LOOP_DESCR *, double *);
static BOOL Loop_Header_Heuristic(BB *, BB *, BB *, LOOP_DESCR *, double *);
static BOOL Loop_Exit_Heuristic(BB *, BB *, BB *, LOOP_DESCR *, double *);
static BOOL Opcode_Heuristic(BB *, BB *, BB *, LOOP_DESCR *, double *);
static BOOL Pointer_Heuristic(BB *, BB *, BB *, LOOP_DESCR *, double *);
static BOOL Store_Heuristic(BB *, BB *, BB *, LOOP_DESCR *, double *);
#ifdef KEY
static BOOL Sequential_Branch_Heuristic(BB *, BB *, BB *, LOOP_DESCR *, double *);
#endif

/* A list of heuristic function pointers and their names.
 */
struct heuristic_info {
  heuristic_func f;
  char name[4];
};

static const struct heuristic_info heuristic[] = {
  { Call_Heuristic,		"CH"  },
  { Return_Heuristic,		"RH"  },
  { Loop_Header_Heuristic,	"LHH" },
  { Loop_Exit_Heuristic,	"LEH" },
  { Opcode_Heuristic,		"OH"  },
  { Pointer_Heuristic,		"PH"  },
  { Store_Heuristic,		"SH"  },
#ifdef KEY
  { Sequential_Branch_Heuristic, "SBH" },
#endif
};


/* ====================================================================
 *
 * BB_Has_Store
 * 
 * Return TRUE if 'bb' contains a store.
 *
 * ====================================================================
 */
static BOOL BB_Has_Store(BB *bb)
{
  OP *op;

  FOR_ALL_BB_OPs(bb, op) if (OP_store(op)) return TRUE;
  return FALSE;
}


/* ====================================================================
 *
 * Is_Store_BB
 * 
 * Return TRUE if 'succ' contains a store, or unconditionally passes
 * control to a block with a store, that it dominates, and the successor 
 * block does not postdominate the 'branchbb'.
 *
 * ====================================================================
 */
static BOOL
Is_Store_BB(BB *branchbb, BB *succ)
{
  BBLIST *blst;

  if (BS_MemberP(BB_pdom_set(branchbb), BB_id(succ))) return FALSE;

  /* Check if succ contains a store
   */
  if (BB_Has_Store(succ)) return TRUE;
  
  /* Check if succ is an unconditional branch to a basic block with a store
   */
  blst = BB_succs(succ);
  if (!BBlist_Has_One_Element(blst)) return FALSE;
  if (!BS_MemberP(BB_dom_set(BBLIST_item(blst)), BB_id(succ))) return FALSE;
  return BB_Has_Store(BBLIST_item(blst));
}


/* ====================================================================
 *
 * Store_Heuristic (SH)
 * 
 * Predict that a successor that contains a store and does not 
 * post-dominate will not be taken.
 *
 * ====================================================================
 */
static BOOL
Store_Heuristic(
  BB *bb,
  BB *s1,
  BB *s2,
  LOOP_DESCR * /* loops */,
  double *s1_taken_prob)
{
  BOOL s1_has_store = Is_Store_BB(bb, s1);
  BOOL s2_has_store = Is_Store_BB(bb, s2);

  if (s1_has_store == s2_has_store) {
    return FALSE;
  } else if (s1_has_store) {
    *s1_taken_prob = 1.0 - PROB_SH;
    return TRUE;
  } else { /* s2_has_store */
    *s1_taken_prob = PROB_SH;
    return TRUE;
  }
}


/* ====================================================================
 *
 * WN_Is_Pointer
 * 
 * Return a boolean that indicates if the passed whirl expression
 * results in a pointer.
 *
 * ====================================================================
 */
BOOL WN_Is_Pointer(WN *wn)
{
  switch (WN_operator(wn)) {
  case OPR_LDA:
  case OPR_ARRAY:

    /* Pointers, by definition
     */
    return TRUE;

  case OPR_LDID:
    /*FALLTHROUGH*/

  case OPR_ILOAD:
  case OPR_ILOADX:
    {
      TY_IDX ty = WN_ty(wn);
      if (TY_kind(ty) == KIND_POINTER) return TRUE;
    }
    break;
  }

  return FALSE;
}


/* ====================================================================
 *
 * Is_Pointer
 * 
 * Return a boolean that indicates if the passed TN is a pointer.
 *
 * ====================================================================
 */
static BOOL Is_Pointer(TN *tn, OP *use_op)
{
  OP *op;
  TN *use_tn = tn;

  /* Attempt to find the definition of 'tn' and if successful,
   * see if that definition determines whether 'tn' is a pointer or not.
   */
  for (;;) {
    OP *def_op;
    DEF_KIND kind;

    if (TN_is_register(use_tn) && TN_is_zero_reg(use_tn)) {

      /* Zero is not a pointer
       */
      return FALSE;
    } else if (TN_is_constant(use_tn)) {

      /* An integer constant is not a pointer.
       */
      if (TN_has_value(use_tn)) return FALSE;
    } else if (TN_is_rematerializable(use_tn)) {

      /* If it's rematerializable we know exactly what it is -- an
       * address or some kind of constant.
       */
      WN *home = TN_home(use_tn);
      OPERATOR opr = WN_operator(home);
      return opr == OPR_LDA;
    } else if (use_op && (def_op = TN_Reaching_Value_At_Op(use_tn, use_op, &kind, TRUE))) {
      if (   (OP_iadd(def_op) || OP_ior(def_op))
	  && TN_is_zero_reg(OP_opnd(def_op,0))
	  && TN_has_value(OP_opnd(def_op,1))
      ) {

	/* A load of an integer constant -- not a pointer.
	 */
	return FALSE;
      } else if (OP_copy(def_op)) {

	/* The 'tn' is a copy -- analyze the source of the copy.
	 */
	use_tn = OP_opnd(def_op, OP_COPY_OPND);
	use_op = def_op;
	continue;
      }
    }
    break;
  }

  /* We didn't learn anything interesting about the definition,
   * see if we have a use of the 'tn' as a base register in
   * a memory reference.
   */
  FOR_ALL_BB_OPs(OP_bb(use_op), op) {
    if (OP_load(op) && OP_opnd(op,0) == tn) return TRUE;
    if (OP_store(op) && OP_opnd(op,1) == tn) return TRUE;
  }

  return FALSE;
}


/* ====================================================================
 *
 * Pointer_Heuristic (PH)
 * 
 * Predict that a branch that branches on the result of a comparison
 * for equality of a pointer against NULL or of two pointers,
 * will not be taken.
 *
 * ====================================================================
 */
static BOOL
Pointer_Heuristic(
  BB *bb,
  BB *s1,
  BB * /* s2 */,
  LOOP_DESCR * /* loops */,
  double *s1_taken_prob)
{
  OPCODE br_opc;
  OPERATOR cond_oper;
  WN *cond_wn;
  TN *tn1;
  TN *tn2;
  OP *cmp;
  VARIANT variant;
  BOOL invert = s1 == BB_next(bb);
  OP *br_op = BB_branch_op(bb);
  WN *br_wn = BB_branch_wn(bb);

  /* Must have a conditional branch OP and a corresponding whirl node.
   */
  if (br_op == NULL || !OP_cond(br_op) || br_wn == NULL) return FALSE;

  /* Verify that the whirl is a conditional branch on EQ or NE.
   */
  br_opc = WN_opcode(br_wn);
  if (br_opc != OPC_FALSEBR && br_opc != OPC_TRUEBR) return FALSE;

  cond_wn = WN_kid0(br_wn);

#if defined(TARG_SL) || defined(TARG_PPC32)  
  if (cond_wn == 0)  return FALSE;
#endif

  cond_oper = WN_operator(cond_wn);
  if (cond_oper != OPR_NE && cond_oper != OPR_EQ) return FALSE;

  /* Account for the sense of the branch and for consistency, make sure the
   * br OP is also an EQ or NE test.
   */
  variant = CGTARG_Analyze_Compare(br_op, &tn1, &tn2, &cmp);
  switch (variant) {
  case V_BR_I4EQ:
  case V_BR_U4EQ:
  case V_BR_I8EQ:
  case V_BR_U8EQ:
    invert = !invert;
    break;

  case V_BR_I4NE:
  case V_BR_U4NE:
  case V_BR_I8NE:
  case V_BR_U8NE:
    break;

  default:
    return FALSE;
  }

  /* At least one of the operands must be a pointer.
   */
  if (   !Is_Pointer(tn1, br_op)
      && !Is_Pointer(tn2, br_op)
      && !WN_Is_Pointer(WN_kid0(cond_wn))
      && !WN_Is_Pointer(WN_kid1(cond_wn))
   ) return FALSE;

  /* We have a pointer comparision of some kind.
   */
  *s1_taken_prob = invert ? 1.0 - PROB_PH : PROB_PH;
  return TRUE;
}


/* ====================================================================
 *
 * Opcode_Heuristic (OH)
 * 
 * Predict that a branch that branches on the result of comparison of
 * an integer for less than zero, less than or equal to zero, or equal
 * to a constant, will not be taken.
 *
 * ====================================================================
 */
static BOOL
Opcode_Heuristic(
  BB *bb,
  BB *s1,
  BB * /* s2 */,
  LOOP_DESCR * /* loops */,
  double *s1_taken_prob)
{
  VARIANT variant;
  OP *cmp;
  TN *tn1, *tn2;
  INT64 val;
  BOOL isconst;
  BOOL invert = s1 == BB_next(bb);
  OP *br_op = BB_branch_op(bb);

  /* Must have a conditional branch.
   */
  if (br_op == NULL || !OP_cond(br_op)) return FALSE;

  /* Determine what type of branch we have.
   */
  variant = CGTARG_Analyze_Compare(br_op, &tn1, &tn2, &cmp);

  /* Determine if any of the operands are constant. Also check the
   * operands to make sure they are not pointers.
   */
  if (   tn2 == NULL
      || Is_Pointer(tn1, br_op)
      || Is_Pointer(tn2, br_op)) return FALSE;
  isconst = TN_Value_At_Op(tn2, br_op, &val);
  if (!isconst) {
    isconst = TN_Value_At_Op(tn1, br_op, &val);
    if (!isconst) return FALSE;
    variant = Invert_BR_Variant(variant);
  }

  /* If we have the original whirl, examine the comparison operands and
   * reject if any pointers are found. NOTE: You might be tempted to use
   * this whirl to determine the heuristic, but because of cflow and
   * code selection issues it is not possible to correctly determine
   * the sense of the branch.
   */
  WN *br_wn = BB_branch_wn(bb);
  if (br_wn) {
    WN *cond_wn = WN_kid0(br_wn);

#if defined(TARG_SL) || defined(TARG_PPC32)
    if (!cond_wn) return FALSE;
#endif    

    OPERATOR cond_oper = WN_operator(cond_wn);
    if (OPERATOR_is_compare(cond_oper)) {
      if (WN_Is_Pointer(WN_kid0(cond_wn)) || WN_Is_Pointer(WN_kid1(cond_wn))) {
	return FALSE;
      }
    }
  }

  /* Now check for specifics as determined by the branch variant.
   * Note: allow comparisions against 1 that can be converted into 
   * comparisions against 0. 
   */
  switch (variant) {
  case V_BR_I4LE:
  case V_BR_U4LE:
    if ((INT32)val != 0) return FALSE;
    invert = !invert;
    break;
  case V_BR_I8LE:
  case V_BR_U8LE:
    if (val != 0) return FALSE;
    invert = !invert;
    break;
  case V_BR_I4LT:
  case V_BR_U4LT:
    if ((UINT32)val > 1) return FALSE;
    invert = !invert;
    break;
  case V_BR_I8LT:
  case V_BR_U8LT:
    if ((UINT64)val > 1) return FALSE;
    invert = !invert;
    break;
  case V_BR_I4GT:
  case V_BR_U4GT:
    if ((INT32)val != 0) return FALSE;
    break;
  case V_BR_I8GT:
  case V_BR_U8GT:
    if (val != 0) return FALSE;
    break;
  case V_BR_I4GE:
  case V_BR_U4GE:
    if ((UINT32)val > 1) return FALSE;
    break;
  case V_BR_I8GE:
  case V_BR_U8GE:
    if ((UINT64)val > 1) return FALSE;
    break;
  case V_BR_I4EQ:
  case V_BR_U4EQ:
  case V_BR_I8EQ:
  case V_BR_U8EQ:
    invert = !invert;
    break;
  case V_BR_I4NE:
  case V_BR_U4NE:
  case V_BR_I8NE:
  case V_BR_U8NE:
    break;
  default:
    #pragma mips_frequency_hint NEVER
    DevWarn("Opcode_Heuristic: unexpected branch variant %s", 
	    BR_Variant_Name(variant));
    return FALSE;
  }

  /* We've satisfied the condition -- return the probability according
   * to the sense of the branch.
   */
  *s1_taken_prob = invert ? 1.0 - PROB_OH : PROB_OH;
  return TRUE;
}


/* ====================================================================
 *
 * LOOPINFO_Trip_Count
 * 
 * Given a pointer to a loopinfo structure, return the trip count
 * for the loop through the out parameter <tc>. A boolean status
 * is returned to indicated if the trip count could be determined
 * and was returned. Note that the trip count may be exact or an
 * estimate depending on the info available.
 *
 * ====================================================================
 */
static BOOL
LOOPINFO_Trip_Count(LOOPINFO* linfo, INT* tc)
{
  TN* trip_tn;
  WN* wn;

  if (!linfo) {
    return FALSE;
  } else if (    (trip_tn = LOOPINFO_trip_count_tn(linfo))
              && TN_is_constant(trip_tn)
  ) {
    *tc = TN_value(trip_tn);
    return TRUE;
  } else if (    (wn = LOOPINFO_wn(linfo))
            && WN_loop_trip_est(wn) > 0
  ) {
    *tc = WN_loop_trip_est(wn);
    return TRUE;
  } else
    return FALSE;
}


/* ====================================================================
 *
 * Loop_Exit_Heuristic (LEH)
 * 
 * Predict that a branch in a loop which has one successor outside the
 * loop will not exit the loop.
 *
 * ====================================================================
 */
static BOOL
Loop_Exit_Heuristic(
  BB *bb,
  BB *s1,
  BB *s2,
  LOOP_DESCR *loops,
  double *s1_taken_prob)
{
  for (; loops; loops = LOOP_DESCR_next(loops)) {
    BB_SET *loop_bbs = LOOP_DESCR_bbset(loops);

    if (BB_SET_MemberP(loop_bbs, bb)) {
      BOOL s1_in_loop = BB_SET_MemberP(loop_bbs, s1);
      BOOL s2_in_loop = BB_SET_MemberP(loop_bbs, s2);

      if (s1_in_loop == s2_in_loop) {
	continue;
      } else {
	double loop_prob = PROB_LEH; /* probability of staying in the loop */
	LOOPINFO *linfo = LOOP_DESCR_loopinfo(loops);
        INT tc;
        if (LOOPINFO_Trip_Count(linfo,&tc)) {
          double trip_count_est = tc;
          loop_prob = (trip_count_est - 1) / trip_count_est;
        }

	*s1_taken_prob = s1_in_loop ? loop_prob : 1.0 - loop_prob;

	return TRUE;
      }
    }
  }

  return FALSE;
}


/* ====================================================================
 *
 * Is_Loophead_BB
 * 
 * Returns TRUE if 'succ' does not postdominate 'branchbb' and is either a 
 * loop head or a loop preheader (i.e. passes control unconditionally to
 * a loop head which it dominates).
 *
 * ====================================================================
 */
static BOOL
Is_Loophead_BB(BB *branchbb, BB *succ)
{
  BBLIST *blst;

  if (BS_MemberP(BB_pdom_set(branchbb), BB_id(succ))) return FALSE;

  /* check if succ is a loop head. */
  if (BB_loophead(succ)) return TRUE;

  /* check if succ is a loop preheader. */
  blst = BB_succs(succ);
  if (!BBlist_Has_One_Element(blst)) return FALSE;
  if (!BS_MemberP(BB_dom_set(BBLIST_item(blst)), BB_id(succ))) return FALSE;
  return BB_loophead(BBLIST_item(blst));
}


/* ====================================================================
 *
 * Loop_Header_Heuristic (LHH)
 * 
 * Predict that a successor that is a loop header or a loop pre-header,
 * and does not dominate the loop, will be taken.
 *
 * ====================================================================
 */
static BOOL
Loop_Header_Heuristic(
  BB *bb,
  BB *s1,
  BB *s2,
  LOOP_DESCR * /* loops */,
  double *s1_taken_prob)
{
  BOOL s1_is_loophead = Is_Loophead_BB(bb, s1);
  BOOL s2_is_loophead = Is_Loophead_BB(bb, s2);

  if (s1_is_loophead == s2_is_loophead) {
    return FALSE;
  } else if (s1_is_loophead) {
    *s1_taken_prob = PROB_LHH;
    return TRUE;
  } else { /* s2_is_loophead */
    *s1_taken_prob = 1.0 - PROB_LHH;
    return TRUE;
  }
}


/* ====================================================================
 *
 * Is_Call_BB
 * 
 * Return TRUE if 'succ' contains a procedure call, or unconditionally 
 * passes control to a block with a call, that it dominates, and the 
 * successor block does not postdominate the 'branchbb'.
 *
 * ====================================================================
 */
static BOOL
Is_Call_BB(BB *branchbb, BB *succ)
{
  BBLIST *blst;

  if (BS_MemberP(BB_pdom_set(branchbb), BB_id(succ))) return FALSE;

  /* Check if succ contains a call
   */
  if (BB_call(succ)) return TRUE;
  
  /* Check if succ is a unconditional branch to a basic block with a call
   */
  blst = BB_succs(succ);
  if (!BBlist_Has_One_Element(blst)) return FALSE;
  if (!BS_MemberP(BB_dom_set(BBLIST_item(blst)), BB_id(succ))) return FALSE;
  return BB_call(BBLIST_item(blst));
}


/* ====================================================================
 *
 * Call_Heuristic (CH)
 * 
 * Predict that a successor that contains a call and does not 
 * post-dominate will not be taken.
 *
 * ====================================================================
 */
static BOOL
Call_Heuristic(
  BB *bb,
  BB *s1,
  BB *s2,
  LOOP_DESCR * /* loops */,
  double *s1_taken_prob)
{
  BOOL s1_is_call = Is_Call_BB(bb, s1);
  BOOL s2_is_call = Is_Call_BB(bb, s2);

  if (s1_is_call == s2_is_call) {
    return FALSE;
  } else if (s1_is_call) {
    *s1_taken_prob = 1.0 - PROB_CH;
    return TRUE;
  } else { /* s2_is_call */
    *s1_taken_prob = PROB_CH;
    return TRUE;
  }
}


/* ====================================================================
 *
 * Is_Return_BB
 * 
 * Return TRUE if the bb does not have any successors or is an unconditinal
 * branch to a basic block with no successors.
 *
 * ====================================================================
 */
static BOOL 
Is_Return_BB(BB *bb)
{
  BBLIST *succ;

  /* Get the successor of <bb>, skipping any intervening call BBs
   * (for the algorithms here, only branches terminate a BB)
   */
  for (; (succ = BB_succs(bb)) && BB_call(bb); bb = BBLIST_item(succ))
    ;

  /* See if successor returns
   */
  if (succ == NULL) return TRUE;

  /* See if successor goes to a return block (skipping intervening calls)
   */
  while (BB_call(BBLIST_item(succ)) && BB_succs(BBLIST_item(succ))) {
    succ = BB_succs(BBLIST_item(succ));
  }
  return BBLIST_next(succ) == NULL && BB_succs(BBLIST_item(succ)) == NULL;
}


/* ====================================================================
 *
 * Return_Heuristic (RH)
 * 
 * Predict that a successor that contains a return will not be taken.
 *
 * ====================================================================
 */
static BOOL
Return_Heuristic(
  BB * /* bb */,
  BB *s1,
  BB *s2,
  LOOP_DESCR * /* loops */,
  double *s1_taken_prob)
{
  BOOL s1_is_return = Is_Return_BB(s1);
  BOOL s2_is_return = Is_Return_BB(s2);

  if (s1_is_return == s2_is_return) {
    return FALSE;
  } else if (s1_is_return) {
    *s1_taken_prob = 1.0 - PROB_RH;
    return TRUE;
  } else { 
    /* s2_is_return */
    *s1_taken_prob = PROB_RH;
    return TRUE;
  }
}

#ifdef KEY
/* ====================================================================
 *
 *  Detect if BB is an IF that branches to a BB that ends in GOTO:
 *
 *     if (...) { ...; goto L;}
 *
 *  If so, return TRUE and save the "then" successor in GOTO_SUCC, save the
 *  other successor in NON_GOTO_SUCC, and save L's BB in GOTO_TARGET.
 * ====================================================================
 */
static BOOL
Is_Branch_With_Goto_Succ (BB *bb, BB **goto_succ, BB **non_goto_succ,
			  BB **goto_target)
{
  EDGE *edge;
  int n_succs = 0;
  BB *goto_bb, *non_goto_bb, *goto_target_bb;

  if (BB_kind(bb) != BBKIND_LOGIF)
    return FALSE;

  // Count the number of successors.
  FOR_ALL_SUCC_EDGES(bb, edge) {
    n_succs++;
  }
  if (n_succs != 2)
    return FALSE;

  // It's a 2-way branch.
  EDGE *edge1 = BB_succ_edges(bb);
  EDGE *edge2 = EDGE_next_succ(edge1);
  BB *succ1 = EDGE_succ(edge1);
  BB *succ2 = EDGE_succ(edge2);

  // Find the goto target BB.
  if (BB_kind(succ1) == BBKIND_GOTO) {
    *goto_succ = succ1;
    *non_goto_succ = succ2;
    *goto_target = BBLIST_item(BB_succs(succ1));
  } else if (BB_kind(succ2) == BBKIND_GOTO) {
    *goto_succ = succ2;
    *non_goto_succ = succ1;
    *goto_target = BBLIST_item(BB_succs(succ2));
  } else
    return FALSE;

  return TRUE;
}

/* ====================================================================
 *
 * Sequential_Branch_Heuristic (SBH)
 * 
 * Predict that this type of IF will fail:
 *     if (...) { ...; goto L;}
 *     if (...) { ...; goto L;}
 *     if (...) { ...; goto L;}
 *     ...
 *   L: 
 *
 * For bug 8546.
 * ====================================================================
 */
static BOOL
Sequential_Branch_Heuristic(
  BB *bb,
  BB *s1,
  BB *s2,
  LOOP_DESCR * /* loops */,
  double *s1_taken_prob)
{
  BB *goto_succ1, *non_goto_succ1, *goto_target1;
  BB *goto_succ2, *non_goto_succ2, *goto_target2;
  BB *goto_succ3, *non_goto_succ3, *goto_target3;

  // See if BB is an IF matching the pattern.
  if (!Is_Branch_With_Goto_Succ(bb, &goto_succ1, &non_goto_succ1,
				&goto_target1))
    return FALSE;

  // Check succ.
  if (Is_Branch_With_Goto_Succ(non_goto_succ1, &goto_succ2, &non_goto_succ2,
			       &goto_target2) &&
      goto_target1 == goto_target2) {

    // Succ ok.  Check succ's succ.
    if (Is_Branch_With_Goto_Succ(non_goto_succ2, &goto_succ3, &non_goto_succ3,
				 &goto_target3) &&
	goto_target1 == goto_target3) {
      *s1_taken_prob = (s1 == goto_succ1) ? PROB_SBH : 1 - PROB_SBH;
      return TRUE;
    }
  }

  // The succs are not IFs of that type.  Check the previous 2 preds.
  BB *pred1, *pred2;

  if (((pred1 = BB_Unique_Predecessor(bb)) == NULL) ||
      ((pred2 = BB_Unique_Predecessor(pred1)) == NULL))
    return FALSE;

  // Check pred.
  if (Is_Branch_With_Goto_Succ(pred1, &goto_succ2, &non_goto_succ2,
			       &goto_target2) &&
      goto_target1 == goto_target2) {

    // Pred ok.  Check pred's pred.
    if (Is_Branch_With_Goto_Succ(pred2, &goto_succ3, &non_goto_succ3,
				 &goto_target3) &&
	goto_target1 == goto_target3) {
      *s1_taken_prob = (s1 == goto_succ1) ? PROB_SBH : 1 - PROB_SBH;
      return TRUE;
    }
  }

  // BB doesn't precede or follow 2 IFs of that type.
  return FALSE;
}
#endif


/* ====================================================================
 * ====================================================================
 *
 * Edge probability determination
 *
 * ====================================================================
 * ====================================================================
 */

/* ====================================================================
 *
 * Trip_Loop_Exit_Prob
 * 
 * For loops with loopinfo and a trip count (constant or estimated), use it
 * to predict how often the branch is taken.
 *
 * ====================================================================
 */
static BOOL
Trip_Loop_Exit_Prob(
  BB *bb,
  BB *s1,
  BB *s2,
  LOOP_DESCR *loops,
  double *s1_taken_prob)
{
  for (; loops; loops = LOOP_DESCR_next(loops)) {
    BB_SET *loop_bbs = LOOP_DESCR_bbset(loops);

    if (BB_SET_MemberP(loop_bbs, bb)) {
      BOOL s1_in_loop = BB_SET_MemberP(loop_bbs, s1);
      BOOL s2_in_loop = BB_SET_MemberP(loop_bbs, s2);

      if (s1_in_loop == s2_in_loop) {
	continue;
      } else {
	double loop_prob; /* probability of staying in the loop */
	double trip_count;
        INT    tc;

        if (!LOOPINFO_Trip_Count(LOOP_DESCR_loopinfo(loops),&tc)) return FALSE;
        if (tc < 1) return FALSE;

        trip_count = (double)tc;

	loop_prob = trip_count == 0.0 ? 0.0 : (trip_count - 1) / trip_count;

	*s1_taken_prob = s1_in_loop ? loop_prob : 1.0 - loop_prob;

	return TRUE;
      }
    }
  }

  return FALSE;
}


/* ====================================================================
 *
 * Compute_BR_Prob_From_Feedback
 * 
 * One or more of <bb>'s successor edges has feedback. Attempt to use it to
 * determine edge probabilities. When one or more edges is missing
 * feedback, we guess about those edges.
 *
 * ====================================================================
 */
static BOOL
Compute_BR_Prob_From_Feedback(BB *bb)
{
  EDGE *sedge;
  double prob_sum = 0.0;
  INT fb_succs = 0;
  INT no_fb_succs = 0;

  /* Examine each of the successor edges of <bb>, counting the
   * number of feedback edges and their combine probabilities,
   * and the total number of succ edges.
   */
  FOR_ALL_SUCC_EDGES(bb, sedge) {
    if (EDGE_prob_fb_based(sedge)) {
      prob_sum += EDGE_prob(sedge);
      ++fb_succs;
    } else {
      ++no_fb_succs;
    }
  }
  Is_True(fb_succs > 0, 
	  ("Compute_BR_Prob_From_Feedback found no feedback succs for BB:%d",
	   BB_id(bb)));

  /* If one or more succs were missing feedback, then guess at the
   * values. The guess is computed very simply by evenly distributing
   * the remaining probability amoung the edges without feedback.
   * There's no sense in using the heuristics, since they only do anything
   * interesting with more than 2 succs, and in the 2 succ case, we
   * can guess exactly.
   */
  if (no_fb_succs != 0) {
    double prob = (1.0 - prob_sum) / (double)no_fb_succs;
    FOR_ALL_SUCC_EDGES(bb, sedge) {
      if (!EDGE_prob_fb_based(sedge)) EDGE_prob(sedge) = prob;
    }
  }

  return TRUE;
}


/* ====================================================================
 *
 * Find_Freq_Hint_Pragmas
 *
 * Scan the BBs in the region and locate any BBs that have a frequency
 * hint pragma. We look for two types: "frequent" and "never" and
 * for each kind we add the BB to a corresponding BB_SET.
 *
 * ====================================================================
 */
static void
Find_Freq_Hint_Pragmas(
  BB_SET **never,
  BB_SET **frequent,
  MEM_POOL *pool)
{
  BB *bb;

  if (never) *never = NULL;
  if (frequent) *frequent = NULL;
  for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    if (BB_has_pragma(bb)) {
      ANNOTATION *ant;

      for ( ant = ANNOT_First(BB_annotations(bb), ANNOT_PRAGMA);
            ant != NULL;
            ant = ANNOT_Next(ant, ANNOT_PRAGMA))
      {
	BB_SET **hint_bbs;
	WN *wn = ANNOT_pragma(ant);
	WN_PRAGMA_ID pragma = (WN_PRAGMA_ID) WN_pragma(wn);
	INT32 arg1 = WN_pragma_arg1(wn);

	if (pragma != WN_PRAGMA_MIPS_FREQUENCY_HINT) continue;

	switch (arg1) {
	case FREQUENCY_HINT_NEVER:
	  hint_bbs = never;
	  break;
	case FREQUENCY_HINT_FREQUENT:
	  hint_bbs = frequent;
	  break;
	default:
	  continue;
	}
	if (hint_bbs == NULL) continue;

	if (*hint_bbs == NULL) {
	  *hint_bbs = BB_SET_Create_Empty(PU_BB_Count + 2, pool);
	}
	BB_SET_Union1D(*hint_bbs, bb, NULL);
      }
    }
  }
}

void
Find_Freq_LMV_Predecessors(LOOP_DESCR *the_loops,
                           BB_SET **freq_bbs,
                           MEM_POOL *pool)
{
  INT i;
  FILE *tfile = TFile;

  i = 0;
  for (LOOP_DESCR *cloop = the_loops; cloop != NULL;
       cloop = LOOP_DESCR_next(cloop)) {
    LOOPINFO *loopinfo = LOOP_DESCR_loopinfo(cloop);
    if (loopinfo && LOOPINFO_multiversion(loopinfo)) {
      BOOL found_lmv_pattern = FALSE;
      std::stack<BB *> lmv_bbs;

      BB *loophead = LOOP_DESCR_loophead(cloop);

      // Find the header predecessor that is not inside the current
      // loop, i.e. the loop preheader
      BB *preheader = NULL;
      EDGE *edge;
      FOR_ALL_PRED_EDGES(loophead,edge) {
        BB *pred = EDGE_pred(edge);
        ANNOTATION *annot = ANNOT_Get(BB_annotations(pred),ANNOT_LOOPINFO);
        if (annot == NULL || ANNOT_loopinfo(annot) != loopinfo) {
          preheader = pred;
          break;
        }
      }
      if (!preheader)
        continue;
      lmv_bbs.push(preheader);

      // Follow the chain of single predecessors to find the
      // LMV pre-condition chain.  The first block with multiple
      // predecessors is the tail of the precondition chain, which
      // has the following structure:
      //
      //         X (pred1)
      //        / \
      //       /   Y (pred2)
      //       \  / \
      //        \/   Z : path to conservative LMV loop (offpath)
      //        M (merge)
      //
      int n_preds;
      BB *bb = preheader;
      do {
        BB *single_pred;
        n_preds = 0;
        FOR_ALL_PRED_EDGES(bb,edge) {
          single_pred = EDGE_pred(edge);
          n_preds++;
        }
        if (n_preds == 1) {
          bb = single_pred;
          lmv_bbs.push(bb);
        }
      } while (n_preds == 1);

      // At this point we have the first block having multiple predecessors.
      // Look for the pattern
      BOOL match_pattern = FALSE;
      BB *merge = bb;
      BB *off_path = NULL;
      BB *pred1 = NULL;
      BB *pred2 = NULL;
      do {
        n_preds = 0;
        match_pattern = FALSE;
        FOR_ALL_PRED_EDGES(merge,edge) {
          n_preds++;
        }
        if (n_preds == 2) {
          BB *pred[2];
          int i = 0;
          FOR_ALL_PRED_EDGES(merge,edge) {
            pred[i++] = EDGE_pred(edge);
          }
          if (BB_Find_Succ_Edge(pred[0],pred[1])) {
            pred1 = pred[0];
            pred2 = pred[1];
          }
          else if (BB_Find_Succ_Edge(pred[1],pred[0])) {
            pred1 = pred[1];
            pred2 = pred[0];
          }
          else
             break;

          BOOL off_path_match = FALSE;
          int n_succ = 0;
          FOR_ALL_SUCC_EDGES(pred2,edge) {
            n_succ++;
            if (EDGE_succ(edge) != merge) {
              if (!off_path) {
                off_path = EDGE_succ(edge);
                off_path_match = TRUE;
              }
              else if (off_path == EDGE_succ(edge))
                off_path_match = TRUE;
            }
          }
          if (n_succ != 2 || !off_path_match)
            break;

          lmv_bbs.push(pred2);
          lmv_bbs.push(merge);

          merge = pred1;
          match_pattern = TRUE;
          found_lmv_pattern = TRUE;
        }
      } while (match_pattern);

      /* If we managed to find the hammock pattern of the lmv precondition
       * chain, we now insert all discovered BBs into the lmv hint set
       */
      if (found_lmv_pattern) {
        if (*freq_bbs == NULL)
          *freq_bbs = BB_SET_Create_Empty(PU_BB_Count + 2, pool);
        while (!lmv_bbs.empty()) {
          BB *bb = lmv_bbs.top();
          lmv_bbs.pop();
          BB_SET_Union1D(*freq_bbs,bb,NULL);
        }
      }
    }
  }
}

/* ====================================================================
 *
 * Compute_BR_Prob_From_Hint
 *
 * Try to compute the edge probabilities for the given BB using
 * frequency hint pragmas. We do so by examining the successors
 * of the branch and for each one we determine if the successor
 * will [definitely] reach a BB that has a pragma. If no successors
 * reach a pragma BB, then we just give up.
 *
 * When one or more successors reach a pragma BB, we will compute
 * all the edge probabilites using this info.
 *
 * Rather than treat "never" and "frequent" as absolutes, we treat
 * them as a ratio: the ratio of probabilities of "frequent" to "never"
 * edges is <-CG:freq_frequent_never_ratio>:1 (default is 1000:1).
 *
 * In a 2-way branch, a "never" edge will have the probability 1/1001
 * and a "frequent" edge will have the probability 1000/1001 (using
 * the default ratio of 1000:1).
 *
 * N-way branches (where N is > 2) are slightly more complicated.
 * These may result in 3 different probabilities being assigned, one
 * each for "never" edges, "frequent" edges and "heuristic" edges.
 * The "heuristic" edges are those that do not reach a pragma.
 * Again, we assign the probabilities such that "frequent":"never"
 * is <-CG:freq_frequent_never_ratio>:1. We also obbey the additional
 * constraint that "frequent":"heuristic" == "heuristic":"never".
 *
 * Note: there is one bit of special casing when we have one or
 * more "heuristic" edges: if the other edges are either all "never"
 * edges or all "frequent" edges, we change the "heuristic" edges
 * to the opposite of the non-"heuristic" edges. This is done so
 * the results are what a user would expect. For example, a simple
 * 2-way branch with one of the successors being a "never" edge,
 * then the implication is that the other edge must be "frequent".
 * The special casing handles that implication.
 *
 * ====================================================================
 */
static BOOL
Compute_BR_Prob_From_Hint(BB *bb, INT n_succs)
{
  EDGE *sedge;
  enum prob_src {ps_heuristic, ps_never, ps_frequent} *prob_src;
  INT isucc;
  INT n_heuristic;
  INT n_never = 0;
  INT n_frequent = 0;

  /* Create an array to tag what will be the source of the probability
   * that we will assign to each edge.
   */
  prob_src = (enum prob_src *)alloca(sizeof(enum prob_src) * n_succs);

  /* Determine how to derive the probablity for each edge, keeping a count
   * of the various types.
   */
  isucc = 0;
  FOR_ALL_SUCC_EDGES(bb, sedge) {
    BB *succ = EDGE_succ(sedge);
    BB_SET *succ_pdom = (BB_SET *)BB_pdom_set(succ);
    BOOL frequent = Frequent_BBs && BB_SET_IntersectsP(Frequent_BBs, succ_pdom);
    BOOL never = Never_BBs && BB_SET_IntersectsP(Never_BBs, succ_pdom);

    prob_src[isucc] = ps_heuristic;

    if (never != frequent) {
      if (frequent) {
	prob_src[isucc] = ps_frequent;
	n_frequent++;
      } else {
	prob_src[isucc] = ps_never;
	n_never++;
      }
    } else if (never) {
      DevWarn("FREQ: BB:%d -> BB%d edge as conflicting freq pragmas -- ignored",
	      BB_id(bb), BB_id(succ));
    }

    isucc++;
  }
  n_heuristic = n_succs - n_never - n_frequent;

  /* Bail out if we found no hint pragmas.
   */
  if (n_heuristic == n_succs) return FALSE;

  /* If we have heuristic edges and only never or only frequent edges,
   * change the heuristic edges to the opposite of the other edges type.
   */
  if (n_heuristic && (n_never == 0 || n_frequent == 0)) {
    for (isucc = 0; isucc < n_succs; isucc++) {
      if (prob_src[isucc] == ps_heuristic) {
	prob_src[isucc] = n_never ? ps_frequent : ps_never;
      }
    }

    if (n_never == 0) {
      n_never = n_heuristic;
    } else {
      n_frequent = n_heuristic;
    }
    n_heuristic = 0;
  }

  /* Compute the probabilities of the different edge types and then
   * apply them to the successor edges. The probabilities are determined
   * by solving the following system of equations:
   *
   *	f
   *   --- = R
   *	n
   *
   *	f     h
   *   --- = ---
   *	h     n
   *
   *	an + bf + ch = 1
   *
   * where:
   *
   *	'n' is the probability of a "never" edge.
   *	'f' is the probability of a "frequent" edge.
   *	'h' is the probability of a "heuristic" edge.
   *	'R' is the ratio "frequent":"never".
   *	'a' is the number of "never" edges.
   *	'b' is the number of "frequent" edges.
   *	'c' is the number of "heuristic" edges.
   */
  {
    double a = n_never;
    double b = n_frequent;
    double c = n_heuristic;
    double R = Frequent_Never_Ratio;
    double sqrt_R = sqrt(R);
    double denom = a + (b * R) + (c * sqrt_R);
    double n = 1.0 / denom;
    double f = R / denom;
    double h = sqrt_R / denom;

    isucc = 0;
    FOR_ALL_SUCC_EDGES(bb, sedge) {
      switch (prob_src[isucc]) {
      case ps_heuristic:
	EDGE_prob(sedge) = h;
	break;
      case ps_never:
	EDGE_prob(sedge) = n;
	break;
      case ps_frequent:
	EDGE_prob(sedge) = f;
	break;
      }

      isucc++;
    }
  }

  return TRUE;
}


static BOOL
Compute_BR_Prob_From_LMV_Hint(BB *bb)
{
  EDGE *sedge;
  enum prob_src {ps_heuristic, ps_never, ps_frequent} *prob_src;
  INT isucc;
  INT n_heuristic;
  INT n_never = 0;
  INT n_frequent = 0;

  BB *lmv_succ = NULL;
  FOR_ALL_SUCC_EDGES(bb, sedge) {
    BB *succ = EDGE_succ(sedge);
    if (BB_SET_MemberP(LMV_Precond_BBs,succ)) {
      if (lmv_succ == NULL)
        lmv_succ = succ;
      else
        return FALSE;
    }
  }
  if (lmv_succ) {
    FOR_ALL_SUCC_EDGES(bb, sedge) {
        BB *succ = EDGE_succ(sedge);
        EDGE_prob(sedge) = (succ == lmv_succ)? 1.0 : 0.0;
    }
    return TRUE;
  }
  else
    return FALSE;
}


/* ====================================================================
 *
 * Compute_Branch_Probabilities
 *
 * Determine branch probabilities.
 * 
 * ====================================================================
 */
static void
Compute_Branch_Probabilities(void)
{
  INT i;
  LOOP_DESCR *loops = loop_list;

  for (i = max_dfo_id - 1; i >= 0; i--) {
    BB *succ;
    EDGE *edge;
    BB *bb = dfo_vec[i];
    INT n_succs = 0;
    INT n_back_succs = 0;
    INT n_fb_edges = 0;

    /* Count up number of succs, succs with feedback and the number 
     * of back edges.
     */
    FOR_ALL_SUCC_EDGES(bb, edge) {
      succ = EDGE_succ(edge);
      if (Is_Loop_Back_Edge(edge))  n_back_succs++;
      if (EDGE_prob_fb_based(edge)) n_fb_edges++;
      n_succs++;
    }

    if (CFLOW_Trace_Freq) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "Computing successor probabilities for BB:%d:\n"
		     "    n_succs: %d, n_back_succs: %d, n_fb_edges: %d\n",
		     BB_id(bb), n_succs, n_back_succs, n_fb_edges);
    }

    /* Determine the probabilities using the most accurate method available.
     */
    if (n_succs == 0) {

      /* No successors
       */
      continue;
    } else if (n_succs == 1) {

      /* One successor. It is not necessary to handle this specially --
       * it would get handled by the final else. However, since it
       * is common, and the probability can only be 1.0, we just
       * set it here and save a little bit of work.
       */
#if defined (TARG_SL)     
      if(!BB_freq_unbalanced(bb) || !CG_PU_Has_Feedback)
#endif	  	
      EDGE_prob(BB_succ_edges(bb)) = 1.0;
    } else if (   ((Frequent_BBs || Never_BBs)
	       && Compute_BR_Prob_From_Hint(bb, n_succs)) ||
	       (LMV_Precond_BBs && Compute_BR_Prob_From_LMV_Hint(bb))
    ) {
      if (CFLOW_Trace_Freq) {
	#pragma mips_frequency_hint NEVER

	/* Hint used
	 */
	fprintf(TFile, "\n    Heuristic ");
	FOR_ALL_SUCC_EDGES(bb, edge) {
	  succ = EDGE_succ(edge);
	  fprintf(TFile, " BB:%-3d -> BB:%-3d", BB_id(bb), BB_id(succ));
	}
	fprintf(TFile, "\n    %.*s\n"
		       "     hint ",
		       10 + 17 * n_succs, DBar);
	FOR_ALL_SUCC_EDGES(bb, edge) {
	  succ = EDGE_succ(edge);
	  fprintf(TFile, " %.13f ", EDGE_prob(edge));
	}
        fprintf(TFile, "\n");
      }
    } else if (n_fb_edges > 0 && Compute_BR_Prob_From_Feedback(bb)) {

      /* Feedback used
       */
      if (CFLOW_Trace_Freq) {
	#pragma mips_frequency_hint NEVER
	fprintf(TFile, "\n    Heuristic ");
	FOR_ALL_SUCC_EDGES(bb, edge) {
	  succ = EDGE_succ(edge);
	  fprintf(TFile, " BB:%-3d -> BB:%-3d", BB_id(bb), BB_id(succ));
	}
	fprintf(TFile, "\n    %.*s\n"
		       "     feedback ",
		       10 + 17 * n_succs, DBar);
	FOR_ALL_SUCC_EDGES(bb, edge) {
	  succ = EDGE_succ(edge);
	  fprintf(TFile, " %.13f%c ", EDGE_prob(edge), 
				      EDGE_prob_fb_based(edge) ? 'F' : 'H');
	}
        fprintf(TFile, "\n");
      }
    } else if (n_back_succs != 0 && n_back_succs < n_succs) {

      /* If some, but not all, edges are loop back edges, predict
       * the back edge as taken. We don't consider other heuristics
       * in determining the probability that the branch is taken
       * (I'm not sure why that is...)
       */
      LOOP_DESCR *loop;
      LOOPINFO *linfo;
      INT tc;
      double prob_taken = PROB_LBH;

      /* If we can determine we have a trip counted loop, use
       * that to determine the probability we'll loop back. Otherwise
       * use the imperical value.
       */
      loop = LOOP_DESCR_Find_Loop(bb);
      linfo = loop ? LOOP_DESCR_loopinfo(loop) : NULL;
      if (LOOPINFO_Trip_Count(linfo,&tc)) {
        double trip_count = (double)tc;
        if (trip_count > 0) {
          prob_taken = (trip_count - 1) / trip_count;
        } else if (trip_count == 0.0) {
          prob_taken = 0.0;
        }
      }

      /* Set the edge probabilities.
       */
      FOR_ALL_SUCC_EDGES(bb, edge) {
	if (Is_Loop_Back_Edge(edge)) {

	  /* Back edge successors
	   */
	  EDGE_prob(edge) = prob_taken / n_back_succs;
	} else {

	  /* Non-back edge successors
	   */
	  EDGE_prob(edge) = (1.0 - prob_taken) / (n_succs - n_back_succs);
	}
      }

       if (CFLOW_Trace_Freq) {
	#pragma mips_frequency_hint NEVER
	fprintf(TFile, "\n    Heuristic ");
	FOR_ALL_SUCC_EDGES(bb, edge) {
	  succ = EDGE_succ(edge);
	  fprintf(TFile, " BB:%-3d -> BB:%-3d", BB_id(bb), BB_id(succ));
	}
	fprintf(TFile, "\n    %.*s\n"
		       "       LBH    ",
		       10 + 17 * n_succs, DBar);
	FOR_ALL_SUCC_EDGES(bb, edge) {
	  succ = EDGE_succ(edge);
	  fprintf(TFile, " %.13f ", EDGE_prob(edge));
	}
        fprintf(TFile, "\n");
      }
   } else if (n_back_succs != 0 || n_succs != 2) {

      /* Only back edges, or not a 2-way branch
       */
      FOR_ALL_SUCC_EDGES(bb, edge) {
	EDGE_prob(edge) = 1.0 / n_succs;
      }
    }
#ifdef KEY
    else if (EDGE_prob_hint_based(BB_succ_edges(bb)) &&
             EDGE_prob_hint_based(EDGE_next_succ(BB_succ_edges(bb)))) {
      if (CFLOW_Trace_Freq) {
        #pragma mips_frequency_hint NEVER
        EDGE *edge1 = BB_succ_edges(bb);
        EDGE *edge2 = EDGE_next_succ(edge1);
        BB *succ1 = EDGE_succ(edge1);
        BB *succ2 = EDGE_succ(edge2);
 
        fprintf(TFile, "\n User builtin BB:%-3d -> BB:%-3d BB:%-3d -> BB:%-3d\n"
                      "    ============================================\n",
                      BB_id(bb), BB_id(succ1), BB_id(bb), BB_id(succ2));
        fprintf(TFile, "     Combined  %.13f  %.13f\n",
                      EDGE_prob(edge1), EDGE_prob(edge2));
      }
    }
#endif
    else {

      /* 2-way branch
       */
      INT i;
      double prob_succ1;
      double prob_succ2;
      EDGE *edge1 = BB_succ_edges(bb);
      EDGE *edge2 = EDGE_next_succ(edge1);
      BB *succ1 = EDGE_succ(edge1);
      BB *succ2 = EDGE_succ(edge2);

      if (CFLOW_Trace_Freq) {
	#pragma mips_frequency_hint NEVER
	fprintf(TFile, "\n    Heuristic  BB:%-3d -> BB:%-3d BB:%-3d -> BB:%-3d\n"
		       "    ============================================\n",
		       BB_id(bb), BB_id(succ1), BB_id(bb), BB_id(succ2));
      }

      if (Trip_Loop_Exit_Prob(bb, succ1, succ2, loops, &prob_succ1)) {
	prob_succ2 = 1.0 - prob_succ1;
      } else {
#ifdef TARG_IA64
	if (IPFEC_Enable_Random_Prob) {
	  double freq_succ1 = (double)(random() + 1);
	  double freq_succ2 = (double)(random() + 1);
	  prob_succ1 = freq_succ1 / (freq_succ1 + freq_succ2);
          prob_succ2 = freq_succ2 / (freq_succ1 + freq_succ2);
        } 
	else {
          prob_succ1 = 0.5;
          prob_succ2 = 0.5;
        }
#else
	prob_succ1 = 0.5;
	prob_succ2 = 0.5;

#endif

	/* Using "Dempster-Shafer" combine probabilities for each
	 * heuristic that applies to this branch.
	 */
	for (i = 0; i < sizeof(heuristic) / sizeof(*heuristic); i++) {
	  double prob_s1_taken;

	  if ((heuristic[i].f)(bb, succ1, succ2, loops, &prob_s1_taken)) {
	    double d = prob_succ1 * prob_s1_taken 
		     + prob_succ2 * (1.0 - prob_s1_taken);
	    prob_succ1 = (prob_succ1 * prob_s1_taken) / d;
	    prob_succ2 = (prob_succ2 * (1.0 - prob_s1_taken)) / d;

	    if (CFLOW_Trace_Freq) {
	      #pragma mips_frequency_hint NEVER
	      fprintf(TFile, "       %3s     %.13f  %.13f\n",
			     heuristic[i].name,
			     prob_s1_taken, 1.0 - prob_s1_taken);
	    }
	  }
	}
      }

      if (CFLOW_Trace_Freq) {
	#pragma mips_frequency_hint NEVER
	fprintf(TFile, "     Combined  %.13f  %.13f\n",
		       prob_succ1, prob_succ2);
      }
      EDGE_prob(edge1) = prob_succ1;
      EDGE_prob(edge2) = prob_succ2;
    }
  }
}


/* ====================================================================
 * ====================================================================
 *
 * Frequency determination
 *
 * ====================================================================
 * ====================================================================
 */

/* ====================================================================
 *
 * Propagate_Freq
 *
 * Propagate BB frequencies to successor blocks.
 * 
 * ====================================================================
 */
static void
Propagate_Freq(
  BB *bb,
  BB *head, 
  double head_freq, 
  BB_SET *to_visit,
  BB_SET *visited)
{
  EDGE *edge;

  if (!BB_SET_MemberP(to_visit, bb)) return;

  if (CFLOW_Trace_Freq) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "  Propagate_Freq(%d, %d, %g, ",
	    BB_id(bb), BB_id(head), head_freq);
    BB_SET_Print(to_visit, TFile);
    fprintf(TFile, ", ");
    BB_SET_Print(visited, TFile);
    fprintf(TFile, ")\n");
  }

  /* Find BB_freq(bb)
   */
  if (bb == head) {
    if (!BB_freq_fb_based(bb)) BB_freq(bb) = head_freq;
  } else {
    double cyclic_prob;
    double in_freq;

    /* Make sure all preds are either visited or loopback edges before
     * going on. The point here is to ensure that we don't use the
     * frequency for an edge until we've computed it. Each loopback
     * predecessor will make a contribution to the loop head's cyclic
     * probability, but not directly to its node frequency, and all
     * the other predecessors will contribute their (pred -> bb) edge
     * frequencies to the node frequency of bb (the current node).
     */
    FOR_ALL_PRED_EDGES(bb, edge) {
      BB *pred = EDGE_pred(edge);
      if (!BB_SET_MemberP(visited, pred) && !Is_Loop_Back_Edge(edge)) {
	/* Return to caller; we will visit this block and get its
	 * frequency right after we've visited all its
	 * predecessors.
	 */
#if Is_True_On
	if (CFLOW_Trace_Freq) {
	  #pragma mips_frequency_hint NEVER
	  fprintf(TFile, "    Non-loopback predecessor BB:%d of BB:%d not "
			 "visited; returning\n",
			 BB_id(pred), BB_id(bb));
	}
#endif
	return;
      }
#if Is_True_On
      else if (CFLOW_Trace_Freq) {
	#pragma mips_frequency_hint NEVER
	if (BB_SET_MemberP(visited, pred)) {
	  fprintf(TFile, "    Predecessor BB:%d of BB:%d visited; "
			 "continuing\n", BB_id(pred), BB_id(bb));
	} else {
	  fprintf(TFile, "    Unvisited predecessor BB:%d of BB:%d is "
			 "loopback; continuing\n", BB_id(pred), BB_id(bb));
	}
      }
#endif
    }

    /* Determine the sum of the incoming edge frequencies and compute
     * the cyclic probability if 'bb' is a loop head.  Note that the
     * cyclic probablity is only computed for a loop head that was
     * reached as a result of calling Propagate_Freq with an outer
     * loop or an entry point, otherwise the loop head would have
     * been handled above.
     */
    in_freq = 0.0;
    cyclic_prob = 0.0;

    FOR_ALL_PRED_EDGES(bb, edge) {
      if (Is_Loop_Back_Edge(edge)) {
	cyclic_prob += EDGE_back_prob(edge);
      } else {
	BB *pred = EDGE_pred(edge);
	in_freq += BB_freq(pred) * EDGE_prob(edge);
      }
    }

    /* Set the frequency of the block. If this is not a loophead,
     * i.e. the cyclic probability is 0, then this is simply the
     * sum of the incoming edge frequencies. Otherwise compute
     * the loophead frequency using the cyclic probability.
     */
    if (cyclic_prob != 0.0) {
      if (cyclic_prob < 1.0) {
	double one_minus_cp = 1.0 - cyclic_prob;
	const double big_float = 1e38;
	double min_prob = in_freq / big_float;

	/* If we had a non-reducible flow graph, or some other inconsistency,
	 * (1 - cp) may out of range or close enough to 0 to overflow
	 * a float after the divide. Wu and Larus used a simple epsilon
	 * scheme to keep it in range. However overflow is not strictly
	 * conditional on the cp, but also the incoming frequency. And setting
	 * the epsilon to too small a value causes reducible loops with
	 * large trip counts to be incorrecly estimated. Therefore we
	 * use a different mechanism to keep the cp in range: we effectively
	 * determine the maximum cp value (to produce an arbitrary big, but
	 * still in range frequency) for the given incoming frequency
	 * and then check against that.
	 */
	if (one_minus_cp <= min_prob) {
	  one_minus_cp = min_prob;
	}
	if (!BB_freq_fb_based(bb)) BB_freq(bb) = in_freq / one_minus_cp;
      } else {

	/* This is almost certainly the result of a non-reducible graph
	 * (we could tell by checking if any one of the back_probs were
	 * 1.0, which is what you get when you the back_prob is never
	 * set other than to its initial value. In the above logic we
	 * were trying to clamping legally high freqs. But here were
	 * are all but certain that is not the case, so set the
	 * the cyclic probabilty to something conservative.
	 */
	if (!BB_freq_fb_based(bb)) BB_freq(bb) = in_freq / (1.0 - PROB_LBH);
      }
    } else {
      if (!BB_freq_fb_based(bb)) BB_freq(bb) = in_freq;
    }

    if (CFLOW_Trace_Freq) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "    Setting frequency for BB:%d to %g\n", 
	      BB_id(bb), BB_freq(bb));
    }
  }

  /* Calculate the frequencies of bb's out edges
   */
  BB_SET_Union1D(visited, bb, NULL);
  BB_SET_Difference1D(to_visit, bb);

  FOR_ALL_SUCC_EDGES(bb, edge) {
    BB *succ = EDGE_succ(edge);
 
    /* Update Back_Edge_Prob(bb -> succ) so it can be used by
     * outer loops to the calculate cyclic probability of inner loops.
     * Note that in this context, BB_freq(bb) is the probability
     * that we get to 'bb' from the loop head. This is the case
     * because we call Propagate_Freq with the loop head with
     * a head_freq of 1.0, therefore the frequency of any block
     * in the loop is the probability that we get to that block
     * from the head.
     */
    if (succ == head) {
      EDGE_back_prob(edge) = EDGE_prob(edge) * BB_freq(bb);
      if (CFLOW_Trace_Freq) {
	#pragma mips_frequency_hint NEVER
	fprintf(TFile, "    Back probability set to %g for BB:%d -> BB:%d\n",
		       EDGE_back_prob(edge), BB_id(bb), BB_id(succ));
      }
    }
  }

  /* Propagate to successors
   */
  FOR_ALL_SUCC_EDGES(bb, edge) {
    if (!Is_Loop_Back_Edge(edge)) {
      BB *succ = EDGE_succ(edge);
      Propagate_Freq(succ, head, 1.0, to_visit, visited);
    }
  }
}


/* ====================================================================
 *
 * Set_Reachable
 *
 * Add all blocks reachable from <bb> in the set <reachable>.
 *
 * ====================================================================
 */
static void
Set_Reachable(BB *bb, BB_SET *reachable)
{
  BBLIST *blst;

  BB_SET_Union1D(reachable, bb, NULL);

  FOR_ALL_BB_SUCCS(bb, blst) {
    BB *succ = BBLIST_item(blst);
    if (!BB_SET_MemberP(reachable, succ)) Set_Reachable(succ, reachable);
  }
}


/* ====================================================================
 *
 * Compute_Frequencies
 *
 * Compute BB frequencies.
 * 
 * ====================================================================
 */
static void
Compute_Frequencies(void)
{
  BB *bb;
  LOOP_DESCR *loops = loop_list;
  INT loop_num = 0;
  BB_SET *visited = BB_SET_Create(PU_BB_Count + 2, &MEM_local_pool);
  BB_SET *to_visit = BB_SET_Create(PU_BB_Count + 2, &MEM_local_nz_pool);

  /* Initialize back probability for each edge.
   */
  for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    EDGE *edge;

    FOR_ALL_SUCC_EDGES(bb, edge) {
      EDGE_back_prob(edge) = EDGE_prob(edge);
    }
  }

  /* Compute the BB frequencies for loops from innermost to outermost.
   */
  while (loops) {
    BB *head = LOOP_DESCR_loophead(loops);
    BB_SET *loop_bbs = LOOP_DESCR_bbset(loops);

    if (CFLOW_Trace_Freq) {
      #pragma mips_frequency_hint NEVER
      ++loop_num;
      fprintf(TFile, "LOOP %d:\n", loop_num);
    }

    /* For each loop, limit the blocks we will visit to those contained
     * in the loop. We do this different than Wu & Larus since their
     * algorithm has a bug in that we can end up using the [uninitialized
     * garbage] frequency of a block outside the loop because it
     * was marked as visited. Robert Kennedy discovered this flaw and
     * made a fix in rev 3.349. However it had subtly different flaws.
     * For example we would visit blocks outside of the loop.
     * We therefore maintain two bit vectors: one for the blocks which
     * we should visit for this loop, and the other for the blocks
     * we have visited (and thus initialized their frequency).
     */
    BB_SET_CopyD(to_visit, loop_bbs, NULL);
    BB_SET_ClearD(visited);

    Propagate_Freq(head, head, 1.0, to_visit, visited);

    if (CFLOW_Trace_Freq && !BB_SET_EmptyP(to_visit)) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "  BBs unvisited in LOOP %d: ", loop_num);
      BB_SET_Print(to_visit, TFile);
      fprintf(TFile, "\n");
    }

    loops = LOOP_DESCR_next(loops);
  }

  /* Compute the BB frequencies for the entry points.
   */
  BB_SET_UniverseD(to_visit, PU_BB_Count + 2, NULL);
  BB_SET_ClearD(visited);

#ifdef KEY
  // Assign frequencies to non-local goto targets that don't have predecessor
  // BBs.
  if (PU_Has_Nonlocal_Goto_Target) {
    for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
      if (!BB_entry(bb) &&
	  BB_has_non_local_label(bb) &&
	  BB_preds(bb) == NULL) {
	Propagate_Freq(bb, bb, Non_Local_Target_Freq, to_visit, visited);
      }
    }
  }
#endif

  if (Compiling_Proper_REGION) {

    /* Region.
     */
    if (CFLOW_Trace_Freq) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "Region entry point:\n");
    }
    bb = CGRIN_entry(RID_Find_Cginfo(REGION_First_BB));
    Propagate_Freq(bb, bb, 1.0, to_visit, visited);
  } else if (BB_LIST_rest(Entry_BB_Head) == NULL) {

    /* PU with one entry point.
     */
    if (CFLOW_Trace_Freq) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "PU entry point:\n");
    }
    bb = BB_LIST_first(Entry_BB_Head);
    Propagate_Freq(bb, bb, 1.0, to_visit, visited);
  } else {

    /* PU with multiple entry points.
     */
    BB_LIST *ent;
    INT n_entries;
    BB_SET **reachable;
    BB **entries;
    INT i;

    /* Count up the number of entry points.
     */
    n_entries = 0;
    for (ent = Entry_BB_Head; ent != NULL; ent = BB_LIST_rest(ent)) {
      n_entries++;
    }

    /* Create a set of reachable BBs for each entry point.
     */
    reachable = (BB_SET **)alloca(sizeof(BB_SET *) * n_entries);
    entries = (BB **)alloca(sizeof(BB *) * n_entries);
    for (i = 0, ent = Entry_BB_Head; ent; i++, ent = BB_LIST_rest(ent)) {
      bb = BB_LIST_first(ent);
      entries[i] = bb;
      reachable[i] = BB_SET_Create_Empty(PU_BB_Count + 2, &MEM_local_pool);
      if (!BB_handler(bb)) Set_Reachable(bb, reachable[i]);
    }

    /* Finally we can propagate the frequencies for each entry point.
     * The sum of the entry frequencies for entry points which later
     * join is 1.0. (An entry may or may not share code with another entry).
     */
    for (i = 0; i < n_entries; i++) {
      double freq;
      INT j;

      bb = entries[i];

      /* Determine the frequency for the entry point -- 
       * all entry points are equally likely.
       */
      if (BB_handler(bb)) {
	freq = EH_Freq;
      } else {
	freq = 1.0;
	for (j = (i + 1) % n_entries; j != i; j = ++j % n_entries) {
	  if (BB_SET_IntersectsP(reachable[i], reachable[j])) {
	    freq += 1.0;
	  }
	}
	freq = 1.0 / freq;
      }

      if (CFLOW_Trace_Freq) {
	#pragma mips_frequency_hint NEVER
	fprintf(TFile, "PU entry point %d:\n", i);
      }
      Propagate_Freq(bb, bb, freq, to_visit, visited);
    }
  }

  if (CFLOW_Trace_Freq && !BB_SET_EmptyP(to_visit)) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "  BBs unvisited (includes dead BBs): ");
    BB_SET_Print(to_visit, TFile);
    fprintf(TFile, "\n");
  }
}


/* ====================================================================
 *
 * Normalize_BB_Frequencies
 *
 * When BB frequencies are the result of feedback, normalize them so
 * that the sum of the entry point frequencies is 1.0.
 *
 * ====================================================================
 */
static void
Normalize_BB_Frequencies(void)
{
  BB    *bb;
  float  entry_freq;

  if (Compiling_Proper_REGION) {
    bb = CGRIN_entry(RID_Find_Cginfo(REGION_First_BB));
    entry_freq = BB_freq_fb_based(bb) ? BB_freq(bb) : 0.0;
  } else if (BB_LIST_rest(Entry_BB_Head) == NULL) {
    bb = BB_LIST_first(Entry_BB_Head);
    entry_freq = BB_freq_fb_based(bb) ? BB_freq(bb) : 0.0;
  } else {

    /* Multiple entries for this PU. The sum of the entry frequencies
     * for entry points which later join is 1.0. (An entry may or may
     * not share code with another entry).
     */
    INT       i;
    mBOOL    *counted;
    BB_SET  **reachable;
    BB      **entries;
    BB_LIST  *ent;

    /* Count the entry points.
     */
    INT n_entries = 0;
    for (ent = Entry_BB_Head; ent; ent = BB_LIST_rest(ent)) ++n_entries;

    /* Create a set of reachable BBs for each entry point.
     */
    counted   = (mBOOL *)alloca(sizeof(mBOOL) * n_entries);
    reachable = (BB_SET **)alloca(sizeof(BB_SET *) * n_entries);
    entries   = (BB **)alloca(sizeof(BB *) * n_entries);

    /* Find the set of reachable BBs for each entry point.
     */
    for (i = 0, ent = Entry_BB_Head; ent; i++, ent = BB_LIST_rest(ent)) {
      bb = BB_LIST_first(ent);
      counted[i] = FALSE;
      entries[i] = bb;
      reachable[i] = BB_SET_Create_Empty(PU_BB_Count + 2, &MEM_local_pool);
      Set_Reachable(bb, reachable[i]);
    }

    for (i = 0; i < n_entries; i++) {
      INT j;

      bb = entries[i];
      if (!counted[i]) {
	entry_freq = BB_freq(bb);
	for (j = (i + 1) % n_entries; j != i; j = (j + 1) % n_entries) {
	  if (BB_SET_IntersectsP(reachable[i], reachable[j])) {
	    counted[j] = TRUE;
	    entry_freq += BB_freq(entries[j]);
	  }
	}
	if (entry_freq != 0.0) {
	  /* BB *bb gets recycled now */
	  FOR_ALL_BB_SET_members(reachable[i], bb) {
	    BB_freq(bb) = BB_freq(bb) / entry_freq;
	  }
	}
      }
    }
    return;
  }

  /* Handle the simple cases
   */
  if (entry_freq == 0.0) {
    #pragma mips_frequency_hint NEVER
    DevWarn("Can't normalize feedback freqs for PU \"%s\" -- entry freq is 0",
	    Cur_PU_Name);
    return;
  }

  for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    if (BB_freq_fb_based(bb)) BB_freq(bb) = BB_freq(bb) / entry_freq;
  }
}


/* ====================================================================
 * ====================================================================
 *
 * Exported interface
 *
 * ====================================================================
 * ====================================================================
 */


/* ====================================================================
 *
 * FREQ_Find_Never_BBs
 *
 * See interface description.
 *
 * ====================================================================
 */
BB_SET *FREQ_Find_Never_BBs(MEM_POOL *pool)
{
  BB_SET *pragma_bbs;
  BB_SET *never_bbs;
  BB *bb;
  BB *pragma_bb;

  Find_Freq_Hint_Pragmas(&pragma_bbs, NULL, pool);
  if (pragma_bbs == NULL) return NULL;

  Calculate_Dominators();

  never_bbs = BB_SET_Create_Empty(PU_BB_Count + 2, pool);

  do {
    for (pragma_bb = BB_SET_Choose(pragma_bbs);
	 pragma_bb != BB_SET_CHOOSE_FAILURE;
	 pragma_bb = BB_SET_Choose_Next(pragma_bbs, pragma_bb))
    {
      for (bb = REGION_First_BB; bb; bb = BB_next(bb)) {
	BOOL equiv_rev =    BB_SET_MemberP(BB_pdom_set(pragma_bb), bb)
			 && BB_SET_MemberP(BB_dom_set(bb), pragma_bb);
	BOOL equiv_fwd =    BB_SET_MemberP(BB_pdom_set(bb), pragma_bb)
		         && BB_SET_MemberP(BB_dom_set(pragma_bb), bb);
	if (equiv_rev || equiv_fwd) {
	  BB_SET_Union1D(never_bbs, bb, NULL);
	}
      }
    }

    BB_SET_ClearD(pragma_bbs);

    for (bb = REGION_First_BB; bb; bb = BB_next(bb)) {
      BBLIST *edge;

      if (BB_SET_MemberP(never_bbs, bb)) continue;

      edge = BB_preds(bb);
      if (edge) {
	for (;;) {
	  BB *pred = BBLIST_item(edge);
	  if (!BB_SET_MemberP(never_bbs, pred)) break;
	  edge = BBLIST_next(edge);
	  if (edge == NULL) {
	    BB_SET_Union1D(pragma_bbs, bb, NULL);
	    goto next_bb;
	  }
	}
      }

      edge = BB_succs(bb);
      if (edge) {
	for (;;) {
	  BB *succ = BBLIST_item(edge);
	  if (!BB_SET_MemberP(never_bbs, succ)) break;
	  edge = BBLIST_next(edge);
	  if (edge == NULL) {
	    BB_SET_Union1D(pragma_bbs, bb, NULL);
	    break;
	  }
	}
      }
    next_bb:
      ;
    }
  } while (!BB_SET_EmptyP(pragma_bbs));

  Free_Dominators_Memory();

  return never_bbs;
}


/* ====================================================================
 *
 * FREQ_Check_Consistency
 *
 * See interface description.
 *
 * ====================================================================
 */
static UINT FREQ_CHECK_MSG_MAX = 5;

static BOOL
Prob_sum_ok(BB* bb)
{
  if (!BB_succs(bb)) return TRUE;

  float prob = 0.0F;
  BBLIST* edge;
  FOR_ALL_BB_SUCCS(bb, edge) prob += BBLIST_prob(edge);

  return FREQ_Match(prob, 1.0F);
}

static BOOL
Freq_sum_ok(BB* bb)
{
  if (!BB_preds(bb)) return TRUE;

  BBLIST* edge;
  float in_freq = 0.0f;
  FOR_ALL_BB_PREDS(bb, edge) {
    BB *pred = BBLIST_item(edge);
    BBLIST *succ_edge = BB_Find_Succ(pred, bb);
    float prob = BBLIST_prob(succ_edge);
    float freq = BB_freq(pred);
    in_freq += freq * prob;
  }

  return FREQ_Match(in_freq, BB_freq(bb));
}

BOOL
FREQ_Check_Consistency(const char *caller)
{
  static INT msg1_cnt = 0;
  static INT msg2_cnt = 0;
  BOOL  is_ok = TRUE;
  BB   *bb;

  for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {

    if (!Prob_sum_ok(bb)) {
      is_ok = FALSE;
      ++msg1_cnt;
      if (msg1_cnt <= FREQ_CHECK_MSG_MAX) {
	DevWarn("%s: Succ probs of BB:%d do not sum to 1.0", caller,
		BB_id(bb)); 
      } else if (msg1_cnt == FREQ_CHECK_MSG_MAX+1) {
	DevWarn("%s: more than %d Succ probs sum msgs - disabling msg\n",
		caller, FREQ_CHECK_MSG_MAX);
      }
    }

    if (!Freq_sum_ok(bb)) {
      is_ok = FALSE;
      ++msg2_cnt;
      if (msg2_cnt <= FREQ_CHECK_MSG_MAX) {
	DevWarn("%s: BB:%d freq (%g) != the sum of its pred edge freq",
		caller, BB_id(bb), BB_freq(bb));
      } else if (msg2_cnt == FREQ_CHECK_MSG_MAX+1) {
	DevWarn("%s: more than %d sum of pred msgs - disabling msg\n",
		caller, FREQ_CHECK_MSG_MAX);
      }
    }
  }
  return is_ok;
}


/* ====================================================================
 *
 * FREQ_Print_BB_Note
 *
 * See interface description.
 *
 * ====================================================================
 */
void
FREQ_Print_BB_Note(
  BB *bb,
  FILE *file
)
{
  const char *prefix = file == Asm_File ? ASM_CMNT_LINE : "";
  BBLIST *bb_succs = BB_succs(bb);
  INT bb_id = BB_id(bb);

  if (!FREQ_freqs_computed && !CG_PU_Has_Feedback) return;

  fprintf(file, "%s<freq>\n", prefix);

  fprintf(file, "%s<freq> BB:%d freq = %#.5f (%s)\n",
	  prefix, bb_id, BB_freq(bb),
	  (BB_freq_fb_based(bb) ? "fb" : "heur"));

  /* Don't bother printing only one successor edge frequency; it's obvious
   * what it is and we don't need more clutter.
   */
  if (BBlist_Len(bb_succs) > 1) {
    BBLIST *succ;

    FOR_ALL_BBLIST_ITEMS(bb_succs, succ) {
      fprintf(file, "%s<freq> BB:%d => BB:%d prob = %#.3f\n",
	      prefix,
	      bb_id,
	      BB_id(BBLIST_item(succ)),
	      BBLIST_prob(succ));
    }
  }

  fprintf(file, "%s<freq>\n", prefix);
}


/* ====================================================================
 *
 * FREQ_Region_Initialize
 *
 * See interface description.
 *
 * ====================================================================
 */
void 
FREQ_Region_Initialize(void)
{
  static BOOL inited = FALSE;

  if (!inited) {
    Frequent_Never_Ratio = atof(FREQ_frequent_never_ratio);
    EH_Freq = atof(FREQ_eh_freq);
#ifdef KEY
    Non_Local_Target_Freq = atof(FREQ_non_local_targ_freq);
#endif
    inited = TRUE;
  }
  FREQ_freqs_computed = FALSE;
}

#if defined(TARG_SL)
static void
Initialize_Freq_BBs(void)
{
  for (BB* bb = REGION_First_BB; bb != NULL; bb = BB_next(bb))  {
    if(!BB_freq_fb_based(bb) && BB_freq(bb)!=0.0)
      BB_freq(bb) = 0.0;
  }
  return;
}

#endif

/* ====================================================================
 *
 * Initialize_Compute_BB_Frequencies
 *
 * Initialize the various data structures and utilities used
 * for frequency computation.
 *
 * ====================================================================
 */
static void
Initialize_Compute_BB_Frequencies(void)
{
  Calculate_Dominators();

  MEM_POOL_Push(&MEM_local_pool);
  MEM_POOL_Push(&MEM_local_nz_pool);

  Initialize_Depth_First_Info(&MEM_local_pool);

  Initialize_Freq_Edges();

#if defined(TARG_SL)
  //initialize bb freq, because the freq of some bb in black regions
  //  were computed by heuristic during the seperate compiling phase,
  //  these values should be rest to zero
  Initialize_Freq_BBs();
#endif

  Find_Freq_Hint_Pragmas(&Never_BBs, &Frequent_BBs, &MEM_local_pool);
  if (CFLOW_Trace_Freq) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "BBs with 'frequent' hint pragma: ");
    BB_SET_Print(Frequent_BBs, TFile);
    fprintf(TFile, "\n");

    fprintf(TFile, "BBs with 'never' hint pragma: ");
    BB_SET_Print(Never_BBs, TFile);
    fprintf(TFile, "\n");
  }

  loop_list = LOOP_DESCR_Detect_Loops(&MEM_local_pool);
  if (CFLOW_Trace_Freq) {
    #pragma mips_frequency_hint NEVER
    LOOP_DESCR_Print_List();
  }

  Find_Freq_LMV_Predecessors(loop_list,&LMV_Precond_BBs,&MEM_local_pool);
  if (CFLOW_Trace_Freq) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "BBs hinted as begin in LMV precondition chain: ");
    BB_SET_Print(LMV_Precond_BBs,TFile);
    fprintf(TFile,"\n");
  }
}


/* ====================================================================
 *
 * Finalize_Compute_BB_Frequencies
 *
 * Free up resources data structures and utilities used during
 * frequency computation.
 * 
 * ====================================================================
 */
static void
Finalize_Compute_BB_Frequencies(void)
{
  Finalize_Freq_Edges();

  Finalize_Depth_First_Info();

  MEM_POOL_Pop(&MEM_local_pool);
  MEM_POOL_Pop(&MEM_local_nz_pool);

  /* Make sure that we reset the bit sets allocated from the
   * pools that we have just popped.
   */
  Frequent_BBs = NULL;
  Never_BBs = NULL;
  LMV_Precond_BBs = NULL;

  Free_Dominators_Memory();

  FREQ_freqs_computed = TRUE;
}


/* ====================================================================
 *
 * FREQ_Compute_BB_Frequencies
 *
 * See interface description.
 *
 * ====================================================================
 */
void 
FREQ_Compute_BB_Frequencies(void)
{
  if (CFLOW_Trace_Freq) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "\n%sEnter Freq Estimate for %s\n%s\n",
            DBar, Cur_PU_Name, DBar);
  }

  if (!FREQ_enable) {
    BB *bb;

    /* If we are not computing the real frequency estimates, set BB_freq to
     * a non-zero value for each basic block.
     */
    for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
      if (!BB_freq_fb_based(bb)) BB_freq(bb) = 1.0;
    }

    if (CFLOW_Trace_Freq) {
      #pragma mips_frequency_hint NEVER
      fprintf (TFile, "Setting all BB frequencies to 1.0\n");
    }
  } else {
    Initialize_Compute_BB_Frequencies();

    Compute_Branch_Probabilities();
    Compute_Frequencies();

    Finalize_Compute_BB_Frequencies();

    if (CFLOW_Trace_Freq) {
      #pragma mips_frequency_hint NEVER
      BB *bb;
      Trace_Frequencies();

      fprintf(TFile, "\n%s%s\tIR after Frequency Estimates\n%s%s\n",
              DBar, DBar, DBar, DBar);
      for (bb = REGION_First_BB; bb; bb = BB_next(bb)) Print_BB(bb);
      fprintf(TFile, "%s%s\n", DBar, DBar);
    }

    if (FREQ_view_cfg) {
      #pragma mips_frequency_hint NEVER
      FREQ_View_CFG("After heuristic-based freq computation");
    }
  }

  if (CFLOW_Trace_Freq) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "\n%sLeave Freq Estimate for %s\n%s\n",
	    DBar, Cur_PU_Name, DBar);
  }
}


/* ====================================================================
 *
 * Node_Label
 *
 * Generate a label for a daVinci graph node.
 *
 * ====================================================================
 */
static const char *
Node_label(BB* bb)
{
  static char *buffer = NULL;
  static INT buf_size = 0;

  /* estimate the size of buffer needed:
   */
  const INT size =   25					// node id and freq
		   + BBlist_Len(BB_succs(bb)) * 25;	// edge prob

  if (buf_size == 0) {
    buffer = (char *)malloc(size);
    buf_size = size;
  } else if (size > buf_size) {
    while (size > buf_size) buf_size *= 2;
    buffer = (char *)realloc(buffer, buf_size);
  }

  char *p = buffer;
  p += sprintf(p, "%d: %g", BB_id(bb), BB_freq(bb));

  if (using_EDGEs) {
    EDGE *sedge;
    FOR_ALL_SUCC_EDGES(bb, sedge) {
      p += sprintf(p, "\\n-> %d: %g", BB_id(EDGE_succ(sedge)),
				      EDGE_prob(sedge));
    }
  } else {
    BBLIST *sedge;
    FOR_ALL_BB_SUCCS(bb, sedge) {
      p += sprintf(p, "\\n-> %d: %g", BB_id(BBLIST_item(sedge)),
				      BBLIST_prob(sedge));
    }
  }
  FmtAssert(p < buffer + size, ("Node_Label buffer size estimate too small"));

  return buffer;
}


/* ====================================================================
 *
 * FREQ_View_CFG
 *
 * See interface description.
 *
 * ====================================================================
 */
void
FREQ_View_CFG(const char *status)
{
  if (!DaVinci::enabled(TRUE)) return;

  CXX_MEM_POOL dv_pool("DaVinci", FALSE);

  DaVinci dv(dv_pool(), NULL);

  dv.Title(Cur_PU_Name);
  dv.Show_Status(status);

  /* Now we start drawing
   */
  NODE_TYPE nt_known, nt_uninit, nt_bad;
  EDGE_TYPE et_known, et_uninit;

  nt_uninit.Color("orange");
  nt_bad.Color("light sky blue");
  et_uninit.Color("red");

  dv.Graph_Begin();

  /* add all nodes
   */
  for (BB* bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    dv.Node_Begin(NODE_ID(bb), Node_label (bb),
		  (Prob_sum_ok(bb) && Freq_sum_ok(bb))
		   ? (BB_freq_fb_based(bb) ? nt_known : nt_uninit)
		   : nt_bad);
    if (using_EDGEs) {
      EDGE *sedge;
      FOR_ALL_SUCC_EDGES(bb, sedge) {
	dv.Out_Edge(EDGE_ID(NODE_ID(bb), NODE_ID(EDGE_succ(sedge))),
		    EDGE_prob_fb_based(sedge) ? et_known : et_uninit,
		    NODE_ID(EDGE_succ(sedge)));
      }
    } else {
      BBLIST *sedge;
      FOR_ALL_BB_SUCCS(bb, sedge) {
	dv.Out_Edge(EDGE_ID(NODE_ID(bb), NODE_ID(BBLIST_item(sedge))),
		    BBLIST_prob_fb_based(sedge) ? et_known : et_uninit,
		    NODE_ID(BBLIST_item(sedge)));
      }
    }
    dv.Node_End();
  }
  dv.Graph_End();

  dv.Event_Loop(NULL);
}


/* Auxillary data structures used by the freq propagate phase
 */

typedef enum {
  PS_UNKNOWN,
  PS_FEEDBACK,				// freq. comes from feedback info
  PS_COMPUTED				// freq. comes from heuristics
} PROP_STATE;

typedef struct {
  INT unprocessed_pred_count;		// number of incoming edges that
					// have not be processed
  PROP_STATE state;
} PROP_STATUS;

// the following two data structures are not the most efficient ones but
// that's probably ok for now.

// associate a PROP_STATUS with each BB
// typedef map<BB*, FREQ_PROP_STATUS> BB_PROP_STATUS;

inline void BB_PROP_STATUS_Init(
  PROP_STATUS *stsvec, 
  const BB *bb, 
  INT count, 
  PROP_STATE st)
{
  PROP_STATUS * const status = &stsvec[BB_id(bb)];
  status->unprocessed_pred_count = count;
  status->state = st;
}

inline PROP_STATUS *BB_PROP_STATUS(PROP_STATUS *stsvec, const BB *bb)
{
  return &stsvec[BB_id(bb)];
}

inline BOOL BB_PROP_STATUS_Valid(const PROP_STATUS *stsvec, const BB *bb)
{
  const PROP_STATUS * const status = &stsvec[BB_id(bb)];
  return    status->state != PS_UNKNOWN 
	 && status->unprocessed_pred_count == 0;
}

inline void BB_PROP_STATUS_Set_Valid(
  PROP_STATUS *stsvec, 
  BB *bb, 
  BOOL feedback_based)
{
  PROP_STATUS * const status = &stsvec[BB_id(bb)];
  status->unprocessed_pred_count = 0;
  status->state = feedback_based ? PS_FEEDBACK : PS_COMPUTED;
}


/* ====================================================================
 *
 * Propagate_Feedback
 *
 * Attempt to deduce feedback frequencies for blocks (and succ edges), where
 * possible, using feedback frequencies from successors and predecessors.
 * 
 * ====================================================================
 */
static void
Propagate_Feedback(PROP_STATUS * const bb_prop_status)
{
  INT i;

  /* Propagate feedback backwards -- Only look for blocks that have
   * one fall-through predecessor. Anything else gets rather
   * complicated, and this should be adequate as this would be the
   * only case where we didn't have a whirl branch attached to the
   * BB to obtain feedback from prior stages.
   */
  for (i = max_dfo_id - 1; i >= 0; i--) {
    BB *bb = dfo_vec[i];
    if (BB_PROP_STATUS_Valid(bb_prop_status, bb) && BB_Has_One_Pred(bb)) {
      BB *pred = BBLIST_item(BB_preds(bb));
      if (!BB_PROP_STATUS_Valid(bb_prop_status, pred) && BB_Has_One_Succ(pred)) {
	BB_freq(pred) = BB_freq(bb);
	EDGE *sedge = BB_succ_edges(pred);
	EDGE_prob(sedge) = 1.0;
	if (BB_freq_fb_based(bb)) {
	  Set_BB_freq_fb_based(pred);
	  Set_EDGE_prob_fb_based(sedge);
	}
	BB_PROP_STATUS_Set_Valid(bb_prop_status, pred, BB_freq_fb_based(bb));
      }
    }
  }

  /* Propagate feedback forwards -- If a block has feedback, then it's 
   * a given that its successor probabilities have been filled in. We 
   * can therefore use them to determine the frequency of any of its 
   * successor blocks.
   */
  BOOL changed;
  do {
    changed = FALSE;
    for (i = 0; i < max_dfo_id; i++) {
      BB *bb = dfo_vec[i];
      if (BB_PROP_STATUS_Valid(bb_prop_status, bb)) {
	EDGE *sedge;
	PROP_STATUS * const status = BB_PROP_STATUS(bb_prop_status, bb);
	float bb_freq = BB_freq(bb);
	FOR_ALL_SUCC_EDGES(bb, sedge) {
	  BB *succ = EDGE_succ(sedge);
	  if (   !BB_PROP_STATUS_Valid(bb_prop_status, succ)
	      && (bb_freq == 0.0 || EDGE_prob_fb_based(sedge)) 
	      && !EDGE_fb_propagated(sedge))
	  {
	    PROP_STATUS * const succ_status = BB_PROP_STATUS(bb_prop_status, succ);
	    float prob = EDGE_prob(sedge);
	    BB_freq(succ) += bb_freq * prob;
	    changed = TRUE;
	    
	    if (succ_status->state == PS_UNKNOWN) {
	      succ_status->state = status->state;
	    } else if (   status->state == PS_COMPUTED 
		       || !EDGE_prob_fb_based(sedge))
	    {
	      succ_status->state = PS_COMPUTED;
	    }

	    if (   --succ_status->unprocessed_pred_count == 0
	        && succ_status->state == PS_FEEDBACK) 
	    {
	      Set_BB_freq_fb_based(succ);
	    }

	    Set_EDGE_fb_propagated(sedge);
	  }
	}
      }
    }
  } while (changed);
}


/* ====================================================================
 *
 * FB_FREQ_Value
 *
 * return the frequency count.  Note that because of round off error,
 * sometimes a zero freq. count might end up as, say,  0.000001.  The
 * Zero() access function takes care of that and returns TRUE if
 * freq.Value() is within an epsilon of 0.0.
 *
 * ====================================================================
 */
static inline float
FB_FREQ_Value(FB_FREQ freq)
{
  return freq.Zero() ? 0.0 : freq.Value();
}


/* ====================================================================
 *
 * FREQ_Incorporate_Feedback
 *
 * See interface description.
 *
 * ====================================================================
 */
void
FREQ_Incorporate_Feedback(const WN* entry)
{
  BB *bb;

  /* Initialize all the data structures necessary to compute frequencies
   * using heuristics. We'll want some of these same data structures
   * for feedback propagation as well, so we do this first. A side
   * effect is that we MUST use the EDGE data structures rather than
   * BBLIST for filling in edge probabilities.
   */
  Initialize_Compute_BB_Frequencies();

  /* Feedback info is found on whirl branch nodes. We just happen
   * to attach whirl branch nodes to the relevant BBs, so scan the
   * BBs in the PU/region and process each one.
   *
   * The feedback info contains frequency data for each edge. We
   * use that info to compute the frequency of the BB and the probability
   * of each outgoing edge.
   */
  for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    WN *wn = BB_branch_wn(bb);
    if (wn != NULL) {
      switch (WN_operator(wn)) {
      case OPR_TRUEBR:
      case OPR_FALSEBR:
	{
	  FB_FREQ fb_take = Cur_PU_Feedback->Query(wn, FB_EDGE_BRANCH_TAKEN);
	  FB_FREQ fb_fall = Cur_PU_Feedback->Query(wn, FB_EDGE_BRANCH_NOT_TAKEN);
	  FB_FREQ fb_bb = fb_take + fb_fall;
	  if (fb_bb.Known()) {
	    LABEL_IDX label = WN_label_number(wn);
	    BB *take_bb = Get_Label_BB(label);
	    EDGE *take_edge = BB_Find_Succ_Edge(bb, take_bb);
	    EDGE *fall_edge = BB_Find_Succ_Edge(bb, BB_next(bb));
	    float freq_bb = FB_FREQ_Value(fb_bb);
	    BB_freq(bb) = freq_bb;
	    Set_BB_freq_fb_based(bb);
	    if (freq_bb != 0.0) {
	      float freq_take = fb_take.Value();
	      float freq_fall = fb_fall.Value();
	      if (take_edge) {
		EDGE_prob(take_edge) += freq_take / freq_bb;
		Set_EDGE_prob_fb_based(take_edge);
	      }
	      if (fall_edge) {
		EDGE_prob(fall_edge) += freq_fall / freq_bb;
		Set_EDGE_prob_fb_based(fall_edge);
	      }
	    }
	  }
	}
	break;
      case OPR_GOTO:
	{
	  FB_FREQ fb_goto = Cur_PU_Feedback->Query(wn, FB_EDGE_OUTGOING);
	  if (fb_goto.Known()) {
	    EDGE *edge = BB_succ_edges(bb);
	    BB_freq(bb) = fb_goto.Value();
	    Set_BB_freq_fb_based(bb);
	    if (edge) {
	      EDGE_prob(edge) = 1.0;
	      Set_EDGE_prob_fb_based(edge);
	    }
	  }
	}
	break;
      case OPR_XGOTO:
	{
	  INT i;
	  WN *goto_wn;
	  INT num_gotos = WN_num_entries(wn); 

	  FB_FREQ fb_bb = FB_FREQ_ZERO;
	  for (i = 0; i < num_gotos; ++i) {
	    FB_FREQ fb_case = Cur_PU_Feedback->Query(wn, FB_EDGE_SWITCH(i));
	    fb_bb += fb_case;
	  }

	  if (fb_bb.Known()) {
	    float freq_bb = FB_FREQ_Value(fb_bb);
	    BB_freq(bb) = freq_bb;
	    Set_BB_freq_fb_based(bb);
	    for (i = 0, goto_wn = WN_first(WN_kid1(wn));
		 i < num_gotos;
		 ++i, goto_wn = WN_next(goto_wn))
	    {
	      LABEL_IDX label = WN_label_number(goto_wn);
	      BB *targ_bb = Get_Label_BB(label);
	      EDGE *edge = BB_Find_Succ_Edge(bb, targ_bb);
	      if (edge) {
		if (freq_bb != 0.0) {
		  FB_FREQ fb_case = Cur_PU_Feedback->Query(wn,
							   FB_EDGE_SWITCH(i));
		  float freq_case = FB_FREQ_Value(fb_case);
		  EDGE_prob(edge) += freq_case / freq_bb;
		  Set_EDGE_prob_fb_based(edge);
		}
	      }
	    }
	  }
	}
	break;

#if defined (TARG_SL)	  
      case  OPR_SL2_FORK_MAJOR:
      case  OPR_SL2_FORK_MINOR:  
      	{
	   INT i;
         FB_FREQ fb0 = Cur_PU_Feedback->Query(wn, FB_EDGE_SWITCH(0)); 
         FB_FREQ fb1 = Cur_PU_Feedback->Query(wn, FB_EDGE_SWITCH(1));
         if(fb0.Known() && fb1.Known())  {
	      FmtAssert(fb0 == fb1, (" freq of forked two threads are not equal "));
	      BB_freq(bb)=FB_FREQ_Value(fb0); 
	      Set_BB_freq_fb_based(bb);
  	      
	      LABEL_IDX label = WN_label_number(wn);
	      BB *take_bb = Get_Label_BB(label);
	      EDGE *take_edge = BB_Find_Succ_Edge(bb, take_bb);
	      EDGE *fall_edge = BB_Find_Succ_Edge(bb, BB_next(bb));
	      EDGE_prob(take_edge) = 1.0;
	      Set_EDGE_prob_fb_based(take_edge);
	      EDGE_prob(fall_edge) = 1.0;
	      Set_EDGE_prob_fb_based(fall_edge);		  
	  }
         Set_BB_freq_unbalanced(bb);
      	}
	break;
#endif
      }
    }
    else if (BB_call(bb)) {
      ANNOTATION *ant = ANNOT_Get(BB_annotations(bb), ANNOT_CALLINFO);
      WN * call_wn = CALLINFO_call_wn(ANNOT_callinfo(ant));
      FB_FREQ fb_in = Cur_PU_Feedback->Query(call_wn, FB_EDGE_CALL_INCOMING);
      if (fb_in.Known()) {
	EDGE *edge = BB_succ_edges(bb);
	float freq_in = FB_FREQ_Value(fb_in);
	BB_freq(bb) = freq_in;
	Set_BB_freq_fb_based(bb);
	if (edge) {
	  FB_FREQ fb_out = Cur_PU_Feedback->Query(call_wn, 
						  FB_EDGE_CALL_OUTGOING);
	  if (fb_out.Known()) {
	    if (freq_in != 0.0) {
	      float freq_out = FB_FREQ_Value(fb_out);
	      EDGE_prob(edge) = freq_out / freq_in;
	      Set_EDGE_prob_fb_based(edge);
	    }
	  } else {
	    EDGE_prob(edge) = 1.0;
	  }
	}
      }
    } else if (BB_Has_One_Succ(bb)) {

      /* Straight line code, set the outgoing edge probability to 1.0
       */
      EDGE *edge = BB_succ_edges(bb);
#if defined(TARG_SL)
      if(!BB_freq_unbalanced(bb))
#endif
      if (edge) {
	EDGE_prob(edge) = 1.0;
	Set_EDGE_prob_fb_based(edge);
      }
    }
  }

  /* If the entry BB is not terminated by a branch, we may not have 
   * feedback inforamtion for it. Traverse all the entry blocks,
   * and get feedback information for them.
   */
  for (BB_LIST *elist = Entry_BB_Head; elist; elist = BB_LIST_rest(elist)) {
    BB *entry_bb = BB_LIST_first(elist);
    if (!BB_freq_fb_based(entry_bb)) {
      ANNOTATION *ant = ANNOT_Get(BB_annotations(entry_bb), ANNOT_ENTRYINFO);
      ENTRYINFO *ent = ANNOT_entryinfo(ant);
      WN *entry_wn = ENTRYINFO_entry_wn(ent);
      if (entry_wn != NULL) {
	FB_FREQ fb_entry = Cur_PU_Feedback->Query(entry_wn, FB_EDGE_OUTGOING);
        if (fb_entry.Known()) {
	  BB_freq(entry_bb) = FB_FREQ_Value(fb_entry);
	  Set_BB_freq_fb_based(entry_bb);
	}
      }
    }
  }

  /* aux. data structure used by Propagate_Feedback
   * we keep track of which node/edge frequency has been propagated
   * for each bb:
   *   unprocessed_pred_count > 0 specifies the number of pred. edges that
   * 				  have not been propagated into this bb
   *   unprocessed_pred_count == 0 specifies this bb's freq. is valid and
   * 				   can be propagated down its successors.
   *   state:  whether the freq. is from feedback (assumed accurate) or
   * 	       from heuristics.
   */
  PROP_STATUS *bb_prop_status = (PROP_STATUS *)alloca(  sizeof(PROP_STATUS)
						      * (PU_BB_Count + 1));
  for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    if (BB_freq_fb_based(bb)) {
      BB_PROP_STATUS_Init(bb_prop_status, bb, 0, PS_FEEDBACK);
    } else {
      BB_PROP_STATUS_Init(bb_prop_status, bb, BBlist_Len(BB_preds(bb)),
			  PS_UNKNOWN);
    }
  }

  /* We now have to fill in and normalize the frequency info.
   */

  /* There may have been problems in propagating the feedback all the
   * way through to CG. Therefore there may be some CFG nodes which
   * are missing feedback. In many cases the missing data can be
   * exactly derived.
   */
  Propagate_Feedback(bb_prop_status);
  if (FREQ_view_cfg) {
    #pragma mips_frequency_hint NEVER
    FREQ_View_CFG("Before feedback-based freq computation");
  }
  
  /* The frequency data we have created from the feedback is absolute.
   * CG conventions expect it to be normalized such that the entry
   * point (at least in a single entry PU) is 1.
   */
  Normalize_BB_Frequencies();

  /* Now, for any node or edge that is not covered by feedback, use default 
   * heuristics to compute the branch probabilities
   */
  Compute_Branch_Probabilities();
  Compute_Frequencies();
  if (FREQ_view_cfg) {
    #pragma mips_frequency_hint NEVER
    FREQ_View_CFG("After feedback-based freq computation");
  }

  Finalize_Compute_BB_Frequencies();
}


/* ====================================================================
 *
 * FREQ_Verify
 * 
 * For each BB in the CFG the following is printed:
 *	- the BB's id and frequency;
 *	- successor ids, frequencies and probabilities.
 * In addition, warning messages are printed if either check fails:
 *      - the outgoing probabilities should sum to 1.0
 *      - the incoming frequencies sum should match the block frequency
 *
 * ====================================================================
 */
BOOL
FREQ_Verify(const char *caller)
{
  float  *bb_total_freq;
  BOOL   all_ok = TRUE;
  BB     *bb, *succ;
  BBLIST *lst;
  float total_prob;
  const char *s;

  fprintf(TFile,"%s<freq> Freq_Verify after %s\n%s", DBar, caller, DBar);

  // Allocate an array for the total frequency counts, initialized
  // to zero.
  bb_total_freq = (float *) CXX_NEW_ARRAY( float, PU_BB_Count + 1,
					   &MEM_local_region_pool );

  // Sum the frequencies along the incoming edges for each BB
  for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    FOR_ALL_BB_SUCCS(bb, lst) {
      succ = BBLIST_item(lst);
      bb_total_freq[BB_id(succ)] += BB_freq(bb) * BBLIST_prob(lst);
    }
  }

  // Print the block and edge frequencies to the trace file
  for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {

    // Display the bb frequency
    fprintf(TFile, "BB:%d frequency = %#.4f", BB_id(bb), BB_freq(bb));

    // Display the successors and total probability of outgoing edges
    total_prob = 0.0;
    s = "\n\t";
    FOR_ALL_BB_SUCCS(bb, lst) {
      total_prob += BBLIST_prob(lst);
      succ = BBLIST_item(lst);
      fprintf(TFile, "%sBB:%d %#.4f(%#.4f)", s, BB_id(succ),
	      BB_freq(bb) * BBLIST_prob(lst), BBLIST_prob(lst));
      s = ", ";
    }
    fprintf(TFile, "\n");

    // Display a message if total outgoing probability is not 1.0
    if (! FREQ_Match(total_prob, 1.0) && BB_succs(bb)) {
#if defined(TARG_SL)
	if(!BB_freq_unbalanced(bb) && !CG_PU_Has_Feedback)
#endif
      fprintf(TFile, "FAIL total_prob == %#.4f != 1.0\n", total_prob);
      all_ok = FALSE;
    }

    // Display a message if total incoming freq doesn't match block freq
    if (! FREQ_Match(bb_total_freq[BB_id(bb)], BB_freq(bb))
	&& BB_preds(bb)) {
      fprintf(TFile, "FAIL total_freq == %#.4f != %f\n",
	      bb_total_freq[BB_id(bb)], BB_freq(bb));
      all_ok = FALSE;
    }
  }

  return all_ok;
}
