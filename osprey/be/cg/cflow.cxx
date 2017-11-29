/*
 * Copyright (C) 2008-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

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


/* ====================================================================
 * ====================================================================
 *
 * Module: cflow.cxx
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/cflow.cxx,v $
 *
 * Description:
 *
 * Control flow analysis and optimization functions.
 *
 * ====================================================================
 * ====================================================================
 */

#include <alloca.h>
#include <stdlib.h>
#include <math.h>
#include <float.h>
#include <limits.h>

#include "defs.h"
#include "config.h"
#include "config_opt.h"
#include "timing.h"
#include "glob.h"
#include "erglob.h"
#include "erbe.h"
#include "tracing.h"
#include "mempool.h"
#include "data_layout.h"
#include "strtab.h"
#include "config_asm.h"
#include "cgir.h"
#include "cg.h"
#include "cg_internal.h"
#include "cgtarget.h"
#include "bb_map.h"
#include "gra_live.h"
#include "gtn_universe.h"
#include "gtn_set.h"
#include "cgexp.h"
#include "variants.h"
#include "cg_flags.h"
#include "cg_region.h"
#include "irbdata.h"
#include "whirl2ops.h"
#include "freq.h"
#include "bb_set.h"
#include "cg_sched_est.h"
#include "note.h"
#include "ir_reader.h"
#include "tn_set.h"
#include "gtn_tn_set.h"
#include "label_util.h"
#include "eh_region.h"
#include "targ_proc_properties.h"
#include "hb_cflow.h"

#include "cflow.h"

#ifdef KEY
#include <float.h> // needed to pick up FLT_MAX at PathScale
#endif
#ifdef TARG_IA64
#include "region.h"
#include "region_bb_util.h"
#include "vt_region.h"
#include "ipfec_options.h"
#endif
#ifdef TARG_IA64
#include "speculation.h"
#endif

#define DEBUG_CFLOW Is_True_On

//#define CFLOW2_REGION_DEBUG

#if Is_True_On && defined(TARG_MIPS)
#define VERIFY_INDIRECT_JUMP_TARGET
#endif

static BOOL have_eh_regions;

/* Branch instruction costs:
 */
static float br_taken_cost;
static float br_fall_cost;
#ifdef KEY
static float br_static_cost;
#endif

/* Option control:
 */
static INT32 disabled_flags;	/* flag disabled via command line switches */
static INT32 current_flags;	/* flags for current invocation of cflow */

/* Cached copy of FREQ_Frequencies_Computed.
 */
static BOOL freqs_computed;

/* Keep a list of BBs we discard in hopes we might use them again.
 */
static BB *deleted_bbs;

/* Track if we remove an EH label so we can prune the range list.
 */
static BOOL eh_label_removed;

/* Tracing:
 */
#define TRACE_CFLOW	0x0001
#define TRACE_DETAIL	0x0002
#define TRACE_UNREACH	0x0004
#define TRACE_BRANCH	0x0008
#define TRACE_MERGE	0x0010
#define TRACE_REORDER	0x0020
#define TRACE_FREQ_ORDER 0x0040
#define TRACE_CLONE     0x0080
#define TRACE_FREQ	0x0100	/* used in freq.c */
#define TRACE_DOM	0x0200	/* used in dominate.c */
#ifdef TARG_IA64
#define TRACE_Empty_BB_Elim 0x0400/* used in cflow.cxx */
#endif


BOOL CFLOW_Trace;
BOOL CFLOW_Trace_Detail;
BOOL CFLOW_Trace_Unreach;
BOOL CFLOW_Trace_Branch;
BOOL CFLOW_Trace_Merge;
BOOL CFLOW_Trace_Reorder;
BOOL CFLOW_Trace_Freq_Order;
BOOL CFLOW_Trace_Clone;
BOOL CFLOW_Trace_Freq;
BOOL CFLOW_Trace_Dom;

#ifdef TARG_IA64
BOOL CFLOW_Trace_Empty_BB_Elim;
#endif

extern UINT32 CG_LOOP_unroll_level;

/* We need to keep some auxilary information for each BB for the various
 * optimizations we perform. The following BB_MAP provides the mechanism
 * to access the info.
 */
static BB_MAP bb_info_map;

/* This is the auxiliary info we keep for each BB.
 */
struct logif_info {
  TN *tn1;			/* branch condition tn 1 */
  TN *tn2;			/* branch condition tn 2 */
  OP *compare_op;		/* the OP that compares tn 1 and tn 2 */
  VARIANT variant;		/* branch variant */
  mBOOL b_likely;		/* branch was a branch-likely */
};

struct vargoto_info {
  ST *listvar;			/* label table symbol */
  INT *refcount;		/* jump table reference count */
};

struct succedge {
  INT64 offset;			/* offset (in bytes) in succ */
  BB *bb;			/* the successor BB */
  float prob;			/* probability this edge is taken */
};

#define BBINFO_NSUCCS (2)	/* Number of successor edges that are
				 * are guaranteed to be allocated
				 * for a BBINFO struct. This number
				 * is large enough for all kinds
				 * except VARGOTO and INDGOTO
				 */

typedef struct bbinfo {
  BBKIND kind;			/* BB kind */
  union {			/* kind specific info */
    struct logif_info l;
    struct vargoto_info v;
  } u;
  mUINT16 eh_rgn;		/* exc handling region number */
  mUINT16 pseudo_eh_rgn;
  mBOOL cold;			/* part of cold region */
  INT nsuccs;			/* number of successors */
  struct succedge succs[BBINFO_NSUCCS]; /* successor edges; we dynamically
				 * allocate this so it MUST BE LAST.
				 */
} BBINFO;

/* BB-info accessors:
 */
#define BB_BBINFO(bb) ((BBINFO *)BB_MAP_Get(bb_info_map, (bb)))

#define     BBINFO_kind(b)		((BBKIND)BB_BBINFO(b)->kind)
#define Set_BBINFO_kind(b,k)		(BB_BBINFO(b)->kind=(k))
#define     BBINFO_eh_rgn(b)		(BB_BBINFO(b)->eh_rgn+0)
#define Set_BBINFO_eh_rgn(b, e)		(BB_BBINFO(b)->eh_rgn=(e))
#define     BBINFO_pseudo_eh_rgn(b)     (BB_BBINFO(b)->pseudo_eh_rgn+0)
#define Set_BBINFO_pseudo_eh_rgn(b, e)  (BB_BBINFO(b)->pseudo_eh_rgn=(e))
#define     BBINFO_cold(b)		(BB_BBINFO(b)->cold+0)
#define Set_BBINFO_cold(b, e)		(BB_BBINFO(b)->cold=(e))
#define     BBINFO_nsuccs(b)		(BB_BBINFO(b)->nsuccs+0)
#define Set_BBINFO_nsuccs(b,n)		(BB_BBINFO(b)->nsuccs=(n))
#define     BBINFO_succ_bb(b, n)	(BB_BBINFO(b)->succs[n].bb+0)
#define Set_BBINFO_succ_bb(b, n, s)	(BB_BBINFO(b)->succs[n].bb=(s))
#define     BBINFO_succ_offset(b, n)	(BB_BBINFO(b)->succs[n].offset+0)
#define Set_BBINFO_succ_offset(b, n, o)	(BB_BBINFO(b)->succs[n].offset=(o))
#define     BBINFO_succ_prob(b, n)	(BB_BBINFO(b)->succs[n].prob+0)
#define Set_BBINFO_succ_prob(b, n, p)	(BB_BBINFO(b)->succs[n].prob=(p))
#define     BBINFO_variant(b)		((VARIANT)BB_BBINFO(b)->u.l.variant)
#define Set_BBINFO_variant(b, v)	(BB_BBINFO(b)->u.l.variant=(v))
#define     BBINFO_b_likely(b)		(BB_BBINFO(b)->u.l.b_likely+0)
#define Set_BBINFO_b_likely(b, v)	(BB_BBINFO(b)->u.l.b_likely=(v))
#define     BBINFO_condval1(b)		(BB_BBINFO(b)->u.l.tn1+0)
#define Set_BBINFO_condval1(b, tn)	(BB_BBINFO(b)->u.l.tn1=(tn))
#define     BBINFO_condval2(b)		(BB_BBINFO(b)->u.l.tn2+0)
#define Set_BBINFO_condval2(b, tn)	(BB_BBINFO(b)->u.l.tn2=(tn))
#define     BBINFO_compare_op(b)	(BB_BBINFO(b)->u.l.compare_op+0)
#define Set_BBINFO_compare_op(b, op)	(BB_BBINFO(b)->u.l.compare_op=(op))
#define     BBINFO_vargoto_listvar(b)	(BB_BBINFO(b)->u.v.listvar+0)
#define Set_BBINFO_vargoto_listvar(b, l) (BB_BBINFO(b)->u.v.listvar=(l))
#define     BBINFO_vargoto_refcount(b)	(BB_BBINFO(b)->u.v.refcount+0)
#define Set_BBINFO_vargoto_refcount(b, r) (BB_BBINFO(b)->u.v.refcount=(r))

/* Structure to associate a reference count with a listvar symbol.
 * A single linked-list is sufficient since there will be so few of
 * these.
 */
typedef struct listvar_count {
  struct listvar_count *next;
  ST *listvar;
  INT count;
} LISTVAR_COUNT;

/* The list of listvar refcounts:
 */
static LISTVAR_COUNT *listvar_counts;


/* ====================================================================
 * ====================================================================
 *
 * Debugging/tracing routines
 *
 * ====================================================================
 * ====================================================================
 */

void Print_BB_Info(BB *bb);
#pragma mips_frequency_hint NEVER Print_BB_Info
void Print_Cflow_Graph(const char *banner);
#pragma mips_frequency_hint NEVER Print_Cflow_Graph

#ifdef TARG_X8664
/* ====================================================================
 *
 * BB_savexmms_op
 *
 * Return the terminating savexmms OP in a given BB
 *
 * ====================================================================
 */
static inline OP* 
BB_savexmms_op( BB *bb )
{
  OP* op = BB_last_op(bb);
  return ( (op != NULL) && (OP_code(op) == TOP_savexmms) ) ? op : NULL;
}

static inline bool
BB_first_OP_computes_got (BB* bb)
{
  OP *first_op = BB_first_op(bb);
  return ((first_op != NULL) && OP_computes_got(first_op));
}

static inline bool
BB_last_OP_computes_got (BB* bb)
{
  OP *last_op = BB_last_op(bb);
  return ((last_op != NULL) && OP_computes_got(last_op));
}

static void
Extend_Truncate_Short_Cmp_Src(OP* compare_op, VARIANT br_variant, INT64 *v)
{
  BOOL is_sign;
  switch (br_variant) {
    case V_BR_I4EQ:
    case V_BR_I4NE:
    case V_BR_I4GE: 
    case V_BR_I4GT: 
    case V_BR_I4LE: 
    case V_BR_I4LT: 
    case V_BR_I8EQ:
    case V_BR_I8NE:
    case V_BR_I8GE: 
    case V_BR_I8GT: 
    case V_BR_I8LE: 
    case V_BR_I8LT:
      is_sign = TRUE;
      break;
    case V_BR_U4EQ:
    case V_BR_U4NE:
    case V_BR_U4GT:
    case V_BR_U4GE:
    case V_BR_U4LT:
    case V_BR_U4LE:
    case V_BR_U8EQ:
    case V_BR_U8NE:
    case V_BR_U8GT:
    case V_BR_U8GE:
    case V_BR_U8LT:
    case V_BR_U8LE:
      is_sign = FALSE;
      break;
    default:
      return;
  }

  // sign extend the constant value
  const TOP top = OP_code( compare_op );
  switch ( top ) {
    case TOP_test8:
    case TOP_testx8:
    case TOP_testxx8:
    case TOP_testxxx8:
    case TOP_testi8:
    case TOP_cmp8:
    case TOP_cmpx8:
    case TOP_cmpxx8:
    case TOP_cmpxxx8:
    case TOP_cmpi8:
    case TOP_cmpxi8:
    case TOP_cmpxxi8:
    case TOP_cmpxxxi8:
      if(is_sign)
        *v = ( (*v) << ( sizeof(INT64) * 8 - 8 ) ) >> ( sizeof(INT64) * 8 - 8 );
      else
        *v = *v & 0xff;
      return;
    case TOP_test16:
    case TOP_testx16:
    case TOP_testxx16:
    case TOP_testxxx16:
    case TOP_testi16:
    case TOP_cmp16:
    case TOP_cmpx16:
    case TOP_cmpxx16:
    case TOP_cmpxxx16:
    case TOP_cmpi16:
    case TOP_cmpxi16:
    case TOP_cmpxxi16:
    case TOP_cmpxxxi16:
      if(is_sign)
        *v = ( (*v) << ( sizeof(INT64) * 8 - 16 ) ) >> ( sizeof(INT64) * 8 - 16 );
      else
        *v = *v & 0xffff;
  }
}
#endif


/* ====================================================================
 *
 * Format_Succ
 *
 * Format a BB and offset for printing.
 * NOTE: the result is returned in a static buffer.
 *
 * ====================================================================
 */
static const char *
Format_Succ(BB *bb, INT isucc)
{
  static char buf[] = "BB:12345+9223372036854775807 (probability 12345678901234567890)";
  INT len;
  INT nsuccs = BBINFO_nsuccs(bb);

  if (nsuccs == 0) {
    strcpy(buf, "BB:-1");
  } else {
    INT64 offset = BBINFO_succ_offset(bb, isucc);
    len = sprintf(buf, "BB:%2d", BB_id(BBINFO_succ_bb(bb, isucc)));
    if (offset != 0) {
      len += sprintf(buf + len, "+%lld", offset);
    }
    if (freqs_computed) {
      sprintf(buf + len, " (probability %#.2f)", BBINFO_succ_prob(bb, isucc));
    }
  }

  return buf;
}


/* ====================================================================
 *
 * Print_BB_Info
 *
 * Print out the 'bbinfo' for a BB.
 *
 * ====================================================================
 */
void
Print_BB_Info(BB *bb)
{
  INT i;

  fprintf(TFile, "part of %s region\n", BBINFO_cold(bb) ? "cold" : "hot");
  fprintf(TFile, "rid = 0x%p\n", BB_rid(bb));
  fprintf(TFile, "eh_rgn = %d\n", BBINFO_eh_rgn(bb));

  fprintf(TFile, "kind = %s", BBKIND_Name(BBINFO_kind(bb)));
  switch (BBINFO_kind(bb)) {
  case BBKIND_GOTO:
    fprintf(TFile, "\nTarget:\t\t%s\n", Format_Succ(bb, 0));
    break;
  case BBKIND_LOGIF:
    fprintf(TFile, "%s\n", BBINFO_b_likely(bb) ? " (likely)" : "");
    fprintf(TFile, "Target 0:\t%s\n", Format_Succ(bb, 0));
    fprintf(TFile, "Target 1:\t%s\n", Format_Succ(bb, 1));
    fprintf(TFile, "Condition:\t");
    if (BBINFO_condval2(bb)) {
      Print_TN(BBINFO_condval1(bb), FALSE);
      fprintf(TFile, " %s ", BR_Variant_Name(BBINFO_variant(bb)));
      Print_TN(BBINFO_condval2(bb), FALSE);
    } else {
      fprintf(TFile, " %s ", BR_Variant_Name(BBINFO_variant(bb)));
      Print_TN(BBINFO_condval1(bb), FALSE);
    }
    fprintf(TFile, "\n");
    fprintf(TFile, "Compare OP:\t");
    Print_OP_No_SrcLine(BBINFO_compare_op(bb));
    break;
  case BBKIND_VARGOTO:
    fprintf(TFile, "\nListvar: 0x%p\n", BBINFO_vargoto_listvar(bb));
    fprintf(TFile, "RefCount: %d\n", *BBINFO_vargoto_refcount(bb));
    for (i = 0; i < BBINFO_nsuccs(bb); i++) {
      fprintf(TFile, "Target %d:\t%s\n", i, Format_Succ(bb, i));
    }
    break;
  case BBKIND_INDGOTO:
    fprintf(TFile, "\n");
    for (i = 0; i < BBINFO_nsuccs(bb); i++) {
      fprintf(TFile, "Target %d:\t%s\n", i, Format_Succ(bb, i));
    }
    break;
  case BBKIND_CALL:
    fprintf(TFile, "\nSuccessor:\t%s\n", Format_Succ(bb, 0));
    break;
#if defined(TARG_SL)
  case BBKIND_ZDL_BODY:
    fprintf( TFile, "\nSuccessor 0:\t%s\n", Format_Succ(bb, 0));
    fprintf( TFile, "Successor 1:\t%s\n", Format_Succ(bb, 1));
    break;
  case BBKIND_FORK:
    fprintf( TFile, "\nSuccessor 0:\t%s\n", Format_Succ(bb, 0));
    fprintf( TFile, "Successor 1:\t%s\n", Format_Succ(bb, 1));
    break;
#endif
  default:
    fprintf(TFile, "\n");
    break;
  }
}


/* ====================================================================
 *
 * Print_Cflow_Graph
 *
 * Print a control flow graph that includes 'bbinfo' for each BB.
 *
 * ====================================================================
 */
void
Print_Cflow_Graph(const char *banner)
{
  BB *bb;

  fprintf(TFile, "\n%s %s\n%s", DBar, banner, DBar);

  for (bb = REGION_First_BB; bb; bb = BB_next(bb)) {
    fprintf(TFile, "\n%sBB:%d\n%s"
		   "BB:%d (length:%d) -- flags = 0x%04x",
		   SBar, BB_id(bb), SBar,
		   BB_id(bb), BB_length(bb), BB_flag(bb));
    if (freqs_computed) fprintf(TFile, ", frequency = %g", BB_freq(bb));
    fprintf(TFile, "\n");
    if (BB_branch_wn(bb)) {
      fprintf(TFile, "Branch WN: ");
      fdump_tree(TFile, BB_branch_wn(bb));
    }
    Print_BB_Info(bb);
  }
}

/* ====================================================================
 * ====================================================================
 *
 * Utility routines
 *
 * ====================================================================
 * ====================================================================
 */


/* ====================================================================
 *
 * Move_LoopHead
 *
 * A loophead needs to be move (because it's being deleted).
 * If it is possible to determine the new loophead of the same loop,
 * move the loop info to that BB. Otherwise, remove the loop
 * body BB's identity.
 *
 * ====================================================================
 */
static void Move_LoopHead(BB *head)
{
  BB *bb;
  BB *new_head;
  INT cnt;

  /* Follow the successor of the original head to see if we
   * can find the new head block.
   */
  new_head = head;
  cnt = 0;
  while (new_head = BB_Unique_Successor(new_head)) {
    if (BB_loop_head_bb(new_head) != BB_loop_head_bb(head)) {

      /* Found a block outside the loop -- give up.
       */
      new_head = NULL;
      break;
    }

    if (!BB_unreachable(new_head)) {

      /* Success, we found the new loophead. Move the annotations to it.
       */
      BB_Copy_Annotations(new_head, head, ANNOT_LOOPINFO);
      BB_Copy_Annotations(new_head, head, ANNOT_PRAGMA);
      BB_Copy_Annotations(new_head, head, ANNOT_INLINE);
      break;
    }

    if (++cnt >= PU_BB_Count) {

      /* Prevent infinite loop in non-trivial cycles -- give up.
       */
      new_head = NULL;
      break;
    }
  }

  /* Modify the remaining loop BBs to point to the new loophead or
   * NULL if there no longer is one.
   */
  for (bb = REGION_First_BB; bb; bb = BB_next(bb)) {
    if (BB_loop_head_bb(bb) == head) Set_BB_loop_head_bb(bb, new_head);
  }
}


/* ====================================================================
 *
 * ListVar_RefCount
 *
 * Return a pointer to the reference count associated with a
 * listvar (jump table).
 *
 * ====================================================================
 */
static INT *ListVar_RefCount(ST *listvar)
{
  LISTVAR_COUNT *counts;

  if (listvar == NULL) return NULL;

  for (counts = listvar_counts; counts; counts = counts->next) {
    if (counts->listvar == listvar) break;
  }

  if (counts == NULL) {
    counts = TYPE_MEM_POOL_ALLOC(LISTVAR_COUNT, &MEM_local_nz_pool);
    counts->listvar = listvar;
    counts->count = 0;
    counts->next = listvar_counts;
    listvar_counts = counts;
  }

  return &counts->count;  
}


/* ====================================================================
 *
 * Cflow_Change_Succ
 *
 * Change the successor index <isucc> of <bb> from <old_succ>
 * to <new_succ>. This would be simple except for that the
 * case where <old_succ> has more than one succ that is the same
 * (because the CG succ list contains only one entry per succ
 * the probablities of the individual edges are lost).
 *
 * ====================================================================
 */
#if defined (TARG_IA64) 
static BOOL
#else
static void
#endif
Cflow_Change_Succ(BB *bb, INT isucc, BB *old_succ, BB *new_succ)
{
#if defined (TARG_IA64)
  if(IPFEC_Enable_Region_Formation && RGN_Formed) {
      if(Home_Region(bb)->Is_No_Further_Opt() ||
         Home_Region(old_succ)->Is_No_Further_Opt() ||
         Home_Region(new_succ)->Is_No_Further_Opt())
         //------------------------------------------------
         // Please add something here to make it sure that 
         // the bb,old_succ and new_succ pair will not be
         // detected again and again.Think about it.
         //------------------------------------------------
         return FALSE;
  }
#endif

  INT i;
  INT nsuccs = BBINFO_nsuccs(bb);
  float prob = BBINFO_succ_prob(bb, isucc);
  BOOL prob_acced = FALSE;  // whether the prob of old_edge has accumulated 
                            // to new_edge. should always be TRUE?

#if Is_True_On
  {
    BBINFO *old_bbinfo = BB_BBINFO(old_succ);
    BBINFO *new_bbinfo = BB_BBINFO(new_succ);
    if (new_bbinfo && old_bbinfo) {    
      Is_True(old_bbinfo->cold == new_bbinfo->cold,
	      ("changing succ of BB:%d from BB:%d to BB:%d crosses cold region",
	      BB_id(bb), BB_id(old_succ), BB_id(new_succ)));
    }
  }
#endif /* Is_True_On */

  BBLIST *old_edge = BB_Find_Succ(bb, old_succ);
  BBLIST *new_edge = BB_Find_Succ(bb, new_succ);

  FmtAssert( old_edge, ("Expect old_edge") );

  if ( new_edge
       && BBLIST_prob_fb_based(new_edge)
       && BBLIST_prob_fb_based(old_edge) ) {
    // bb->old; bb->new; so move old edge prob to new edge.
    BBLIST_prob(new_edge) += BBLIST_prob(old_edge);
    BBLIST_prob(old_edge)  = 0;
    prob_acced = TRUE;
  }
  Set_BBINFO_succ_bb(bb, isucc, new_succ);

  //more? is following consistency with new edge feedback?
  for (i = 0; i < nsuccs; ++i) {
    if (i != isucc && BBINFO_succ_bb(bb, i) == old_succ) {
      if ( freqs_computed ) {
	      float old_prob = BBLIST_prob(old_edge);
	      BBLIST_prob(old_edge) = prob > old_prob ? 0.0 : old_prob - prob;
      }
#if defined (TARG_IA64)
      if(IPFEC_Enable_Region_Formation && RGN_Formed) {
        RGN_Link_Pred_Succ_With_Prob(bb,new_succ,prob);
	      return TRUE;
	    } else {
        Link_Pred_Succ_with_Prob(bb, new_succ, prob);
        return TRUE;
      }
#else
      Link_Pred_Succ_with_Prob(bb, new_succ, prob);
      return;
#endif
    }
  }
#if defined (TARG_IA64)
  if (IPFEC_Enable_Region_Formation && RGN_Formed) {
      RGN_Unlink_Pred_Succ(bb,old_succ);
      RGN_Link_Pred_Succ_With_Prob(bb,new_succ,prob);
  } else {
      Unlink_Pred_Succ(bb, old_succ);
#if defined(KEY)
  Link_Pred_Succ_with_Prob(bb, new_succ, prob, FALSE, TRUE,
                           BBLIST_prob_hint_based(old_edge) != 0, !prob_acced);
#else
  Link_Pred_Succ_with_Prob(bb, new_succ, prob, FALSE, TRUE);
#endif  
  }

  return TRUE;
#else
  Unlink_Pred_Succ(bb, old_succ);
#if defined(KEY)
  Link_Pred_Succ_with_Prob(bb, new_succ, prob, FALSE, TRUE,
                           BBLIST_prob_hint_based(old_edge) != 0);
#else
  Link_Pred_Succ_with_Prob(bb, new_succ, prob, FALSE, TRUE);
#endif
#endif
}


/* ====================================================================
 *
 * Pragma_Affects_Cflow
 *
 * Determine if a BB's pragmas could affect cflow's decisions.
 *
 * ====================================================================
 */
inline BOOL
Pragma_Affects_Cflow(BB * /* bb */)
{
  /* Prior to this function being created, all pragmas were stripped
   * out in whirl2ops, so ignoring them here can't be bad. At least
   * until proven otherwise...
   */
  return FALSE;
}


/* ====================================================================
 *
 * Falls_Thru
 *
 * Return TRUE iff control flow from b may fall through to either the
 * given successor block if given, or the next block if not.
 *
 * ====================================================================
 */
static BOOL
Falls_Thru(BB *b, BB *succ)
{
  INT i;
  INT nsuccs;

  if (succ == NULL) {
    succ = BB_next(b);
    if (succ == NULL) return FALSE;
  }

 /* Successors of BBKIND_VARGOTO and BBKIND_INDGOTO are ignored,
  * because falling through to them will not avoid a branch.
  */
  if (BBINFO_kind(b) == BBKIND_VARGOTO || BBINFO_kind(b) == BBKIND_INDGOTO) {
    return FALSE;
  }

  nsuccs = BBINFO_nsuccs(b);
  for (i = 0; i < nsuccs; ++i) {
    if (BBINFO_succ_offset(b, i) == 0 && BBINFO_succ_bb(b, i) == succ) {
      return TRUE;
    }
  }
  return FALSE;
}


/* ====================================================================
 *
 * Alloc_BB_Like
 *
 * Allocate a BB by either recycling, or gen-ing a new one.
 *
 * ====================================================================
 */
static BB *
Alloc_BB_Like(BB *model)
{
  BB *new_bb = deleted_bbs;
  BB_NUM id;

  /* If no deleted BBs, return a new one.
   */
  if (new_bb == NULL) return Gen_BB_Like(model);

  /* Memory is a terrible thing to waste -- recycle a BB we discarded
   * earlier by zeroing everything except the id.
   */
  if (CFLOW_Trace) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "recycling BB:%d\n", BB_id(new_bb));
  }
  deleted_bbs = BB_next(new_bb);
  id = BB_id(new_bb);
  BZERO(new_bb, sizeof(*new_bb));
  new_bb->id = id;
  if (model) BB_rid(new_bb) = BB_rid(model);
  return new_bb;
}


/* ====================================================================
 *
 * Remove_Annotations
 *
 * Remove all annotations of type 'kind' from 'bb'.
 *
 * ====================================================================
 */
static void
Remove_Annotations(BB *bb, ANNOTATION_KIND kind)
{
  ANNOTATION *ant;
  ANNOTATION *next;

  for (ant = ANNOT_First(BB_annotations(bb), kind); ant; ant = next) {
    next = ANNOT_Next(ant, kind);
    BB_annotations(bb) = ANNOT_Unlink(BB_annotations(bb), ant);
  }
}


/* ====================================================================
 *
 * Delete_BB_Contents
 *
 * Delete the contents of a BB, i.e. remove all its OPs, etc, but leave
 * it connected to the BB chain (typically because we need its labels).
 *
 * ====================================================================
 */
static void
Delete_BB_Contents(BB *bp)
{
  BBLIST *edge;

  if (CFLOW_Trace_Unreach) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "removing contents of BB:%d\n", BB_id(bp));
  }

  /* Undo various annotations, etc.
   */
  if (BB_entry(bp)) {
    Entry_BB_Head = BB_LIST_Delete(bp, Entry_BB_Head);
    Reset_BB_entry(bp);
    Remove_Annotations(bp, ANNOT_ENTRYINFO);
  }

  if (BB_exit(bp)) {
    Exit_BB_Head = BB_LIST_Delete(bp, Exit_BB_Head);
    Reset_BB_exit(bp);
    Remove_Annotations(bp, ANNOT_EXITINFO);
  }

  if (BB_call(bp)) {
    Reset_BB_call(bp);
    Remove_Annotations(bp, ANNOT_CALLINFO);
  }

  if (BB_asm(bp)) {
    Reset_BB_asm(bp);
    Remove_Annotations(bp, ANNOT_ASMINFO);
  }

  if (BBINFO_kind(bp) == BBKIND_VARGOTO) {
    INT *refcount = BBINFO_vargoto_refcount(bp);

    /* Remove the jump table if this is the last use of it.
     */
    if (refcount != NULL && --(*refcount) == 0) {
      ST *listvar = BBINFO_vargoto_listvar(bp);
      Set_ST_is_not_used(listvar);
    }
    Remove_Annotations(bp, ANNOT_SWITCH);
  }

  BB_branch_wn(bp) = NULL;
  BB_freq(bp) = 0.0;

  if (!CG_localize_tns) {
    GTN_SET_ClearD(BB_live_in(bp));
    GTN_SET_ClearD(BB_live_out(bp));
    GTN_SET_ClearD(BB_defreach_in(bp));
    GTN_SET_ClearD(BB_defreach_out(bp));
    GTN_SET_ClearD(BB_live_use(bp));
    GTN_SET_ClearD(BB_live_def(bp));
  }

  /* Delete all OPs from BB.
   */
  BB_Remove_All(bp);
#ifdef TARG_IA64
  if (IPFEC_Enable_Region_Formation && RGN_Formed ) {
    /* If the region has been formed,must also delete the node from
     * region.
     */
    RGN_Unlink_BB_Edges(bp, Home_Region(bp)->Regional_Cfg());
  
  }else{ 
#endif
  /* Remove successor edges.
   */
  FOR_ALL_BB_SUCCS(bp, edge) {
    BB *succ = BBLIST_item(edge);
    BBlist_Delete_BB(&BB_preds(succ), bp);
  }
  BBlist_Free(&BB_succs(bp));

  /* Remove predecessor edges.
   */
  FOR_ALL_BB_PREDS(bp, edge) {
    BB *pred = BBLIST_item(edge);
    BBlist_Delete_BB(&BB_succs(pred), bp);
  }
  BBlist_Free(&BB_preds(bp));
#ifdef TARG_IA64
  }
#endif
  Set_BBINFO_kind(bp, BBKIND_GOTO);
  Set_BBINFO_nsuccs(bp, 0);
}


/* ====================================================================
 *
 * Delete_BB
 *
 * Delete a BB, i.e. remove all its OPs and remove it from the
 * list of BBs for the region/PU.
 *
 * ====================================================================
 */
static void
Delete_BB(BB *bp, BOOL trace)
{
  BBLIST *edge;

  if (trace) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "removing BB:%d\n", BB_id(bp));
  }

  /* If we're removing a loophead, make sure we maintain the correct
   * loop annotations.
   */
  if (BB_loophead(bp)) {
    Move_LoopHead(bp);
  }

  /* If this was an entry/exit BB, remove it from the list of entry/exit BBs.
   */
  if (BB_entry(bp)) Entry_BB_Head = BB_LIST_Delete(bp, Entry_BB_Head);
  if (BB_exit(bp)) Exit_BB_Head = BB_LIST_Delete(bp, Exit_BB_Head);

  /* If this was a VARGOTO BB, remove the jump table if this is the last
   * use of it.
   */
  if (BBINFO_kind(bp) == BBKIND_VARGOTO) {
    INT *refcount = BBINFO_vargoto_refcount(bp);
    if (refcount != NULL && --(*refcount) == 0) {
      ST *listvar = BBINFO_vargoto_listvar(bp);
      Set_ST_is_not_used(listvar);
    }
  }

  /* Delete all OPs from BB.
   */
  BB_Remove_All(bp);
#ifdef TARG_IA64
  if (IPFEC_Enable_Region_Formation && RGN_Formed ) {
	/* If the region has been formed,must also delete the node from
     * region.
     */
	RGN_Remove_BB_And_Edges(bp, Home_Region(bp)->Regional_Cfg());
  
  }else{ 
#endif
  /* Remove from BB chain.
   */
  Remove_BB(bp);
  
  /* Remove from hyperblocks */
  HB_CFLOW_Remove_Block(bp);

  /* Remove successor edges.
   */
  FOR_ALL_BB_SUCCS(bp, edge) {
    BB *succ = BBLIST_item(edge);
    BBlist_Delete_BB(&BB_preds(succ), bp);
  }
  BBlist_Free(&BB_succs(bp));

  /* Remove predecessor edges.
   */
  FOR_ALL_BB_PREDS(bp, edge) {
    BB *pred = BBLIST_item(edge);
    BBlist_Delete_BB(&BB_succs(pred), bp);
  }
  BBlist_Free(&BB_preds(bp));
#ifdef TARG_IA64
  }
#endif
  Set_BBINFO_kind(bp, BBKIND_UNKNOWN);
  Set_BBINFO_nsuccs(bp, 0);

  /* Save the BB hoping that we can recycle it.
   */
  BB_next(bp) = deleted_bbs;
  deleted_bbs = BB_next(bp);
}


#ifdef VERIFY_INDIRECT_JUMP_TARGET
/* ====================================================================
 *
 * Verify_Indirect_Jump_Target
 *
 * Attempt to verify the target of an indirect jump.
 *
 * ====================================================================
 */
static void
Verify_Indirect_Jump_Target(OP *br, BB *tgt, INT64 offset)
{
  INT i;
  OP *op;
  BB *bb = OP_bb(br);
  TN *tgt_tn = OP_opnd(br, 0);

  FmtAssert(TN_is_register(tgt_tn), ("branch target not a register"));

  FOR_ALL_BB_OPs_REV(bb, op) {
    for (i = OP_results(op) - 1; i >= 0; --i) {
      if (OP_result(op,i) == tgt_tn) goto got_result;
    }
  }
got_result:
  if (op && OP_iadd(op) && TN_is_label(OP_opnd(op, 1))) {
    LABEL_IDX lab;
    TN *tn = OP_opnd(op, 1);
    INT64 tn_offset = TN_offset(tn);
    lab = TN_label(tn);
    FmtAssert(Is_Label_For_BB(lab, tgt) && tn_offset == offset,
	      ("indirect jump add mistmatch"));

    tgt_tn = OP_opnd(op, 0);
    for (op = OP_prev(op); op; op = OP_prev(op)) {
      for (i = OP_results(op) - 1; i >= 0; --i) {
	if (OP_result(op,i) == tgt_tn) goto got_prev_result;
      }
    }
  got_prev_result:
    if (op && OP_load(op) && TN_is_label(OP_opnd(op, 1))) {
      tn = OP_opnd(op, 1);
      lab = TN_label(tn);
      tn_offset = TN_offset(tn);
      FmtAssert(Is_Label_For_BB(lab, tgt) && tn_offset == offset,
		("indirect jump load mistmatch"));
    }
  }
}
#endif /* VERIFY_INDIRECT_JUMP_TARGET */


/* ====================================================================
 *
 * Negate_Branch
 *
 * Negate the sense of a branch. The return value indicates if the
 * negation was successful.
 *
 * ====================================================================
 */
static BOOL
Negate_Branch(OP *br)
{
  TOP top = OP_code(br);
  TOP new_top = CGTARG_Invert(top);

  if (new_top != TOP_UNDEFINED) {
    OP_Change_Opcode(br, new_top);
    return TRUE;
  }

  if (OP_has_predicate(br)) {
    BB *br_bb = OP_bb(br);
    OP *cmp = BBINFO_compare_op(br_bb);
    if (cmp != NULL && OP_results(cmp) == 2) {
      if (   OP_has_predicate(cmp) 
	  && !TN_is_true_pred(OP_opnd(cmp,OP_PREDICATE_OPND))) return FALSE;

      BB *cmp_bb = OP_bb(cmp);
      TN *r0 = OP_result(cmp,0);
      TN *r1 = OP_result(cmp,1);
      TN *pred = OP_opnd(br,OP_PREDICATE_OPND);
      TN *neg_tn = r0 == pred ? r1 : r0;

      if (TN_is_true_pred(neg_tn)) {
	DevWarn("negative cmp result is a sink");
	return FALSE;
      }

      Set_OP_opnd(br, OP_PREDICATE_OPND, neg_tn);

      if (br_bb != cmp_bb && !CG_localize_tns) {
	GRA_LIVE_Compute_Local_Info(br_bb);
	GRA_LIVE_Region_Start();
	GRA_LIVE_Region_Entry(cmp_bb);
	GRA_LIVE_Region_Exit(br_bb);
	GRA_LIVE_Region_Compute_Global_Live_Info();
      }

      return TRUE;
    }
  }

  return FALSE;
}


/* ====================================================================
 *
 * Insert_Goto_BB
 *
 * <bb> has a fall through successor <targ_bb>, which is not the next BB.
 * Insert a new BB following <bb> and fill it in with an unconditional
 * branch to offset <targ_offset> in <targ_bb>. The flag <fill_delay_slots>
 * indicates if we should consider filling the delay slot if
 * applicable.
 *
 * ====================================================================
 */
static void Insert_Goto_BB(
  BB *bb,
  BB *targ_bb,
  INT64 targ_offset,
  BOOL fill_delay_slots
)
{
  LABEL_IDX lab;
  TN *lab_tn;
  BBLIST *sedge;
  BB *goto_bb;
  float goto_prob;
  BOOL goto_prob_fb;
  OPS ops = OPS_EMPTY;
  RID *rid = BB_rid(bb);
  BOOL region_is_scheduled = rid && RID_level(rid) >= RL_CGSCHED;

  /* Get the probability of the existing edge from bb to targ_bb.
   * Because of the context Insert_Goto_BB might be used in, this
   * isn't an obvious value, so it's better to find out what it should
   * be rather than guess wrong. And in addition, we can propogate
   * the feedback status of the edge.
   */
  sedge = BB_Find_Succ(bb, targ_bb);
  goto_prob = BBLIST_prob(sedge);
  goto_prob_fb = BBLIST_prob_fb_based(sedge);

  goto_bb = Alloc_BB_Like(bb);
#ifdef TARG_IA64
  if (IPFEC_Enable_Region_Formation && RGN_Formed ) 
      RGN_Gen_And_Insert_Node(goto_bb, bb, targ_bb);
#endif

  BB_freq(goto_bb) = BB_freq(bb) * goto_prob;
  
  Insert_BB(goto_bb, bb);
#ifdef TARG_IA64
  if (IPFEC_Enable_Region_Formation && RGN_Formed )
      RGN_Link_Pred_Succ_With_Prob(goto_bb,targ_bb, 1.0);
  else
      Link_Pred_Succ_with_Prob(goto_bb, targ_bb, 1.0);
#else
  Link_Pred_Succ_with_Prob(goto_bb, targ_bb, 1.0);
#endif
  if (BB_freq_fb_based(bb) && goto_prob_fb) {
    BBLIST *edge = BB_Find_Succ(goto_bb, targ_bb);
    Set_BB_freq_fb_based(goto_bb);
    Set_BBLIST_prob_fb_based(edge);
  }

  lab = Gen_Label_For_BB(targ_bb);
  lab_tn = Gen_Label_TN(lab, targ_offset);
  Exp_OP1(OPC_GOTO, NULL, lab_tn, &ops);

#ifdef TARG_SL
  // make up line info of GOTO instruction
  if (BB_last_op(bb) && (OP_srcpos(BB_last_op(bb)) != 0)) {
    OP_srcpos(OPS_last(&ops)) = OP_srcpos(BB_last_op(bb));
  }
#endif

  if (   PROC_has_branch_delay_slot()
      && (fill_delay_slots || region_is_scheduled))
  {
    Exp_Noop(&ops);
    Set_BB_scheduled(goto_bb);
  }
  BB_Append_Ops(goto_bb, &ops);
#ifdef TARG_IA64
  if (IPFEC_Enable_Region_Formation && RGN_Formed ){
      RGN_Unlink_Pred_Succ(bb, targ_bb);
      RGN_Link_Pred_Succ_With_Prob(bb, goto_bb, goto_prob);
  }else{
      Unlink_Pred_Succ(bb, targ_bb);
      Link_Pred_Succ_with_Prob(bb, goto_bb, goto_prob);
  }
#else
  Unlink_Pred_Succ(bb, targ_bb);
  Link_Pred_Succ_with_Prob(bb, goto_bb, goto_prob);
#endif
  if (goto_prob_fb) {
    BBLIST *edge = BB_Find_Succ(bb, goto_bb);
    Set_BBLIST_prob_fb_based(edge);
  }

  if (!CG_localize_tns) GRA_LIVE_Compute_Liveness_For_BB(goto_bb);
}


/* ====================================================================
 *
 * Finalize_BB
 *
 * Do post processing for a BB -- mostly manipulating branch instructions.
 *
 * ====================================================================
 */
static void
Finalize_BB(BB *bp)
{
  BOOL fill_delay_slots = (current_flags & CFLOW_FILL_DELAY_SLOTS) != 0;
#ifdef TARG_IA64
  // bug fix for OSP_105 & OSP_192
  // As to those BBs, whose unreachable_bb flag is set to TRUE, 
  // Except BBs which include EH related labels, which may not be deleted;
  // this means that these BBs holds a dangle speculative load or chk, 
  // which aslo needn't be deleted;
  // NO need to manipulate these situation
  //
  BOOL unreachable_bb = BB_unreachable(bp);
  if (unreachable_bb)
    return;
#endif  
  switch (BBINFO_kind(bp)) {
  case BBKIND_LOGIF:
#ifdef TARG_IA64
  case BBKIND_CHK: // bug fix for OSP_104, OSP_105, OSP_192
#endif
  {
      INT tfirst;
      INT tcount;
      LABEL_IDX lab;
      TN *lab_tn;
      BB *fall_through;
      BB *target;
      float fall_through_prob;
      float target_prob;
      INT64 fall_through_offset;
      INT64 target_offset;
      OP *br = BB_branch_op(bp);
#ifdef TARG_IA64
      if ((br == NULL) || OP_noop(br))// bug fix for OSP_104, OSP_105, OSP_192
      {
         br = BB_Last_chk_op(bp);
      }
#endif
      /* Get the fall through and the target BBs.
       */
      fall_through = BBINFO_succ_bb(bp, 1);
      fall_through_offset = BBINFO_succ_offset(bp, 1);
      fall_through_prob = BBINFO_succ_prob(bp, 1);
      target = BBINFO_succ_bb(bp, 0);
      target_offset = BBINFO_succ_offset(bp, 0);
      target_prob = BBINFO_succ_prob(bp, 0);
#if defined(TARG_SL)
      if ((Is_Target_Sl1_pcore() || Is_Target_Sl1_dsp()) && CG_branch_taken) {
         /* Since sl1 use a branch always taken policy, we may need to change layout here. 
            We negate branch for the three cases where  fall_through_prob > target_prob:
            1. fall_through is the start of the next BB
            2. target is the start of the next BB
            3. neither succ is the next BB
               Maybe we are too aggressive here.
          */
        if (fall_through_prob > target_prob) {
	        if (BBINFO_b_likely(bp)) {
      	    // ??
      	  } else if (Negate_Branch(br)) {
      	    target = fall_through;
      	    target_offset = fall_through_offset;
      	    target_prob = fall_through_prob;
      	    fall_through = BBINFO_succ_bb(bp, 0);
      	    fall_through_offset = BBINFO_succ_offset(bp, 0);
      	    fall_through_prob = BBINFO_succ_prob(bp, 0);
      	  }
      	}
      }
      else
#endif
      /* If the target is the start of the next BB or neither succ is
       * the next BB and the target probability is lower, negate the
       * branch condition so that we fall through to the next BB in
       * the first and minimize the dynamic branch cost in the second.
       */
      if (   (target == BB_next(bp) && target_offset == 0)
	  || (   (fall_through != BB_next(bp) || fall_through_offset != 0)
	      && (fall_through_prob > target_prob))
      ) {
	if (BBINFO_b_likely(bp)) {
	  /* If we invert the branch likely then we change when the
	   * the delay slot OP is executed, and that would be wrong.
	   * If this were a case where we were adding an unconditional
	   * branch regardless, we could move the b-likely delay slot 
	   * OP to the ucond br delay slot and put a nop in the b-likely
	   * slot. Probably not worth going through the trouble however.
	   */
	} else if (Negate_Branch(br)) {
	  target = fall_through;
	  target_offset = fall_through_offset;
	  target_prob = fall_through_prob;
	  fall_through = BBINFO_succ_bb(bp, 0);
	  fall_through_offset = BBINFO_succ_offset(bp, 0);
	  fall_through_prob = BBINFO_succ_prob(bp, 0);
	}
      }

      /* Handle the "taken" target -- set the successor and branch label
       * and the edge frequency.
       */
      Is_True(BB_Find_Succ(bp, target),
	      ("BB:%d preds/succs don't match BBINFO", BB_id(bp)));
      Is_True(   !freqs_computed
	      || (   BBLIST_prob(BB_Find_Succ(bp, target)) 
		  == (target != fall_through ? target_prob : 1.0)),
	      ("BB:%d preds/succs don't match BBINFO", BB_id(bp)));
      lab = Gen_Label_For_BB(target);
      lab_tn = Gen_Label_TN(lab, target_offset);
      CGTARG_Branch_Info(br, &tfirst, &tcount);
      FmtAssert(tcount == 1, ("unexpected number of branch targets"));
      Set_OP_opnd(br, tfirst, lab_tn);

      /* Handle the "fall through" target -- if the fall through BB is
       * not the next BB, we have to insert an unconditional branch to it.
       */
      Is_True(BB_Find_Succ(bp, fall_through),
	      ("BB:%d preds/succs don't match BBINFO", BB_id(bp)));
      Is_True(   !freqs_computed
	      || (   BBLIST_prob(BB_Find_Succ(bp, fall_through)) 
		  == (target != fall_through ? fall_through_prob : 1.0)),
	      ("BB:%d preds/succs don't match BBINFO", BB_id(bp)));
      Is_True(BB_succs_len(bp) == (1 + (target != fall_through)),
	      ("BB:%d preds/succs don't match BBINFO", BB_id(bp)));
      if (fall_through_offset != 0 || fall_through != BB_next(bp)) {
	Insert_Goto_BB(bp, fall_through, fall_through_offset, fill_delay_slots);
      }
    }
    break;

#if defined(TARG_SL)
  case BBKIND_ZDL_BODY:
  case BBKIND_FORK:
    {
      INT64 fall_through_offset;
      BB* fall_through;
      fall_through = BBINFO_succ_bb(bp, 1);
      fall_through_offset = BBINFO_succ_offset(bp, 1);
      if (fall_through_offset != 0 || fall_through != BB_next(bp)) {
	  Insert_Goto_BB(bp, fall_through, fall_through_offset, fill_delay_slots);
      }
    }
    break;
#endif

  case BBKIND_GOTO:
    if (BBINFO_nsuccs(bp)) {
      BB *succ_bb = BBINFO_succ_bb(bp, 0);
      INT64 offset = BBINFO_succ_offset(bp, 0);
      RID *rid = BB_rid(bp);
      BOOL region_is_scheduled = rid && RID_level(rid) >= RL_CGSCHED;

      Is_True(BB_Find_Succ(bp, succ_bb),
	      ("BB:%d preds/succs don't match BBINFO", BB_id(bp)));
      Is_True(   !freqs_computed
#if defined(TARG_SL)
		 || BB_freq_unbalanced(bp)
#endif
		 || (BBLIST_prob(BB_Find_Succ(bp, succ_bb)) == 1.0),
		 ("BB:%d preds/succs don't match BBINFO", BB_id(bp)));

      if (   offset == 0
	  && succ_bb == BB_next(bp) 
	  && BBINFO_cold(bp) == BBINFO_cold(succ_bb)
	 )
      {

	/* Fall through; remove terminating branch if there is one.
	 */
#ifdef TARG_X8664
        if (!region_is_scheduled) BB_Remove_Branch(bp);
#else
	if (!region_is_scheduled) {
		BB_Remove_Branch(bp);
		Reset_BB_scheduled(bp);
	}
#endif
      } else if (BB_asm(bp)) {

	/* An asm must remain the last OP in the BB, therefore
	 * if the successor is not the fall through we must insert
	 * a GOTO block.
	 */
	Insert_Goto_BB(bp, succ_bb, offset, fill_delay_slots);
      } else {

	/* Not a fall through.
	 */
	OP *br = BB_branch_op(bp);
	LABEL_IDX lab = Gen_Label_For_BB(succ_bb);
	TN *lab_tn = Gen_Label_TN(lab, offset);

	if (br != NULL) {
	  INT tfirst;
	  INT tcount;

	  /* Branch already exists, just adjust the target.
	   */
	  CGTARG_Branch_Info(br, &tfirst, &tcount);
	  if (tcount == 1) {
	    Set_OP_opnd(br, tfirst, lab_tn);
	  } else {
	    FmtAssert(tcount == 0, ("unexpected number of branch targets"));
#ifdef VERIFY_INDIRECT_JUMP_TARGET
	    Verify_Indirect_Jump_Target(br, succ_bb, offset);
#endif /* VERIFY_INDIRECT_JUMP_TARGET */
	  }
	} else {
	  OPS ops = OPS_EMPTY;

	  /* We must have had a fall through, so append a branch
	   * to the new target.
	   */
	  Exp_OP1(OPC_GOTO, NULL, lab_tn, &ops);
#ifdef TARG_SL
	  // make up line info of GOTO instruction
	  if (BB_last_op(bp) && (OP_srcpos(BB_last_op(bp)) != 0)) {
	    OP_srcpos(OPS_last(&ops)) = OP_srcpos(BB_last_op(bp));
	  } else if (succ_bb && BB_last_op(succ_bb) && (OP_srcpos(BB_last_op(succ_bb)) != 0)) {
	    OP_srcpos(OPS_last(&ops)) = OP_srcpos(BB_last_op(succ_bb));
	  }
#endif
	  if (   PROC_has_branch_delay_slot()
	      && (fill_delay_slots || region_is_scheduled)) {
	    Exp_Noop(&ops);
	  }
	  BB_Append_Ops(bp, &ops);
#ifdef TARG_IA64
	  Reset_BB_scheduled(bp);
#endif
	}
      }
    }
    break;

  case BBKIND_VARGOTO:
  case BBKIND_INDGOTO:
#if Is_True_On
    if (BBINFO_kind(bp) == BBKIND_VARGOTO) {
      ANNOTATION *ant = ANNOT_Get(BB_annotations(bp), ANNOT_SWITCH);
      ST *ant_listvar = ant ? ANNOT_switch(ant) : NULL;
      WN *br_wn = BB_branch_wn(bp);
      ST *bb_listvar = br_wn ? WN_st(br_wn) : NULL;
      ST *listvar = BBINFO_vargoto_listvar(bp);
      INT *refcount = BBINFO_vargoto_refcount(bp);
      Is_True(listvar == bb_listvar,
	      ("bad WN_st for BB:%d listvar; bb-listvar=0x%p, bbinfo-listvar=0x%p",
	      BB_id(bp), bb_listvar, listvar));
      Is_True(ant_listvar == listvar,
	      ("bad ANNOT_switch for BB:%d; bad=0x%p, good=0x%p",
	      BB_id(bp), ant_listvar, listvar));
      Is_True(listvar == NULL || *refcount > 0,
	      ("bad VARGOTO jump table RefCount: %d", *refcount));
    }
#endif /* Is_True_On */
    break;

  case BBKIND_CALL:
    if (BBINFO_nsuccs(bp)) {
      INT64 offset = BBINFO_succ_offset(bp, 0);
      BB *succ_bb = BBINFO_succ_bb(bp, 0);
      BB *next_bb = BB_next(bp);

      Is_True(   BBlist_Len(BB_succs(bp)) == 1
	      && BBLIST_item(BB_succs(bp)) == succ_bb,
	      ("BB:%d preds/succs don't match BBINFO", BB_id(bp)));
      Is_True(!freqs_computed || BBLIST_prob(BB_Find_Succ(bp, succ_bb)) == 1.0,
	      ("BB:%d preds/succs don't match BBINFO", BB_id(bp)));
      Is_True(offset == 0, ("CALL BB:%d offset non-zero", BB_id(bp)));

      /* The call block doesn't fall through to its succ so insert
       * a GOTO bb.
       */
      if (next_bb != succ_bb || offset != 0) {
	Insert_Goto_BB(bp, succ_bb, offset, fill_delay_slots);
      }
    }
    break;

  case BBKIND_RETURN:
  case BBKIND_REGION_EXIT:
  case BBKIND_TAIL_CALL:
    /* Nothing to do
     */
    break;

  default:
    #pragma mips_frequency_hint NEVER
    FmtAssert(FALSE, ("Finalize_BB: unhandled BB kind (%s)",
		     BBKIND_Name(BBINFO_kind(bp))));
    /*NOTREACHED*/
  }
}


#ifdef TARG_X8664

/* ====================================================================
 *
 * Br_Fuse_BB
 *
 * Do branch fuse processing for a BB -- mostly manipulating cmp instructions.
 *
 * ====================================================================
 */
static void
Br_Fuse_BB(BB *bp)
{
  if (BBINFO_kind(bp) == BBKIND_LOGIF) {
    OP *br = BB_branch_op(bp);
    OP *cmp = BBINFO_compare_op(bp);
    // now see if we pass the entrance criteria
    if ((cmp != NULL) && 
        (OP_load_exe(cmp) == false) &&
        (br != cmp) &&
        (OP_bb(cmp) == bp)) {
      ARC_LIST *arcs;
      OP *cmp_next = OP_next(cmp);

      // If we are already optimal, do nothing.
      if (cmp_next == br)
        return;

      // Now check for a modifier to the use regs of the cmp.
      CG_DEP_Compute_Graph ( bp,
                             INCLUDE_ASSIGNED_REG_DEPS,
                             NON_CYCLIC,
                             NO_MEMREAD_ARCS,
                             INCLUDE_MEMIN_ARCS,
                             NO_CONTROL_ARCS,
                             NULL);

      // Check for anti-deps on the cmp's src operands
      for (arcs = OP_succs(cmp);
           arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
        ARC *arc = ARC_LIST_first(arcs);
        OP *succ_op = ARC_succ(arc);
        if (succ_op == br) continue;

        // any other reader/writer represents a scheduling barrier
        if (OP_Precedes(succ_op, br)) {
          CG_DEP_Delete_Graph (bp);
          return;
        }
      }

      // Even if the cmp still winds up in the last
      // slot of the last dispatch group, and the fusion
      // does not happen, this is not a destructive activity.
      OP_scycle(cmp) = OP_scycle(br);
      OP_dgroup(cmp) = OP_dgroup(br);
      BB_Move_Op_Before(bp, br, bp, cmp);
      CG_DEP_Delete_Graph (bp);
    }
  }
}
#endif

/* ====================================================================
 *
 * Finalize_All_BBs
 *
 * Do post processing for all BBs in the region.
 *
 * ====================================================================
 */
static void
Finalize_All_BBs(void)
{
  BB *bp;
  BB *next;

  for (bp = REGION_First_BB; bp; bp = next) {

    /* Finalize_BB may insert a BB and not add BBINFO for it, so
     * grab the next BB so we don't try to finalize it.
     */
    next = BB_next(bp);

    Finalize_BB(bp);
  }

  /* If any EH labels were removed then prune the range list.
   */
  if (eh_label_removed) {
    EH_Prune_Range_List();
  }
}


#ifdef TARG_X8664
/* ====================================================================
 *
 * Br_Fuse_All_BBs
 *
 * Do branch fuse processing for all BBs in the region.
 *
 * ====================================================================
 */
static void
Br_Fuse_All_BBs(void)
{
  BB *bp;
  BB *next;

  for (bp = REGION_First_BB; bp; bp = next) {
    next = BB_next(bp);
    Br_Fuse_BB(bp);
  }
}
#endif


/* ====================================================================
 *
 * Normalize_Branch_Target
 *
 * Verify and normalize a branch target (a BB and offset pair).
 * The offset must be of an instruction within the BB or one that
 * it directly falls through to. The normalized target and offset
 * is the BB+offset of the instruction that is branched to.
 *
 * The normalized target is returned through the in/out parameters
 * <ptarget> and <poffset>.
 *
 * The return value indicates if branch target was valid as described
 * above (TRUE if it is; FALSE otherwise).
 *
 * ====================================================================
 */
static BOOL
Normalize_Branch_Target(BB **ptarget, INT64 *poffset)
{
  BB *succ;
  OP *op;
  INT64 op_offset;
  BB *target = *ptarget;
  INT64 offset = *poffset;

  /* Scan the OPs in the BB until we find the instruction that
   * is targeted.
   */
  for (op_offset = 0, op = BB_first_op(target);;) {
    if (op == NULL) {

      /* If we're out of OPs, the offset is past the end of the BB.
       * If we fall into the next BB, continue there. Otherwise,
       * give up.
       */
      succ = BB_Unique_Successor(target);
      if (succ != BB_next(target)) return FALSE;
      target = succ;
      offset -= op_offset;
      op_offset = 0;
      op = BB_first_op(target);
      continue;
    } else if (op_offset >= offset) {

      /* The current OP is the target (the start or middle if it's
       * a multi-instruction OP).  Our work is done here.
       */
      break;
    } else {

      /* Adjust the offset for the current OP and go check the next.
       */
      INT num_ops = OP_Real_Ops(op);
      op_offset += num_ops * 4;
      op = OP_next(op);
    }
  }

  /* Return the normalized target and offset.
   */
  *ptarget = target;
  *poffset = offset;
  return TRUE;
}


/* ====================================================================
 *
 * Initialize_BB_Info
 *
 * Gather various control flow information, for the BB's in the region,
 * that we'll use later.
 *
 * The return value indicates if we were successful in creating the BB info.
 *
 * ====================================================================
 */
static BOOL
Initialize_BB_Info(void)
{
  /* We maintain a stack of exception regions so that we can assign
   * a unique ID to each region. The stack is implemented with a linked
   * list of the following structures:
   */
  struct eh_ctx {
    INT32 rgn;			/* ID for this EH region */
    struct eh_ctx *prev;	/* Previous stack entry */
    struct eh_ctx *next;	/* Next stack entry */
  };

  struct eh_ctx eh_stack;	/* The bottom of the stack */
  struct eh_ctx *eh_tos;	/* The top of the stack -- eh_tos->next
				 * points to unused link list elements
				 */
  INT eh_rgn;			/* The current EH region ID */
  INT prev_eh_rgn = -2;
  INT pseudo_eh_rgn = -1;
  INT eh_id;			/* Highest EH region ID used */
  BB *bb;

  /* Init the exception region ID. We'll assign each exc region
   * a unique ID.
   */
  eh_id = 0;

  /* Create a stack that we'll use to save the current exc region ID
   * when we enter a new eh region.  The top-of-stack contains the
   * current eh region ID.
   */
  eh_tos = &eh_stack;
  eh_tos->rgn = 0;
  eh_tos->prev = NULL;
  eh_tos->next = NULL;

  bb_info_map = BB_MAP_Create();
  for (bb = REGION_First_BB; bb; bb = BB_next(bb)) {
    BBINFO *bbinfo;
    INT bbinfo_size;
    BBKIND bbkind = BB_kind(bb);

    /* Allocate and associate the bbinfo structure with this BB.
     */
    bbinfo_size = sizeof(BBINFO);
    if (bbkind == BBKIND_VARGOTO || bbkind == BBKIND_INDGOTO) {
      INT incr =  (BBlist_Len(BB_succs(bb)) - BBINFO_NSUCCS)
		* sizeof(struct succedge);
      if (incr > 0) bbinfo_size += incr;
    }
    bbinfo = (BBINFO *)MEM_POOL_Alloc(&MEM_local_nz_pool, bbinfo_size);
    BB_MAP_Set(bb_info_map, bb, bbinfo);

    /* If the block has labels, process any eh begin/end labels.
     */
    eh_rgn = eh_tos->rgn;
    if (BB_has_label(bb)) {
      ANNOTATION *ant;

      for (ant = ANNOT_First(BB_annotations(bb), ANNOT_LABEL);
	   ant != NULL;
	   ant = ANNOT_Next(ant, ANNOT_LABEL)
      ) {
	LABEL_IDX lab = ANNOT_label(ant);
	switch (LABEL_kind(Label_Table[lab])) {
	case LKIND_BEGIN_EH_RANGE:
	  {
	    struct eh_ctx *eh_next = eh_tos->next;

	    /* Start of a new eh region, save the old ID and setup the new.
	     * If there are available link entries, use the first one,
	     * otherwise allocate a new one.
	     */
	    if (eh_next == NULL) {
	      eh_next = (eh_ctx *)alloca(sizeof(struct eh_ctx));
	      eh_next->next = NULL;
	      eh_next->prev = eh_tos;
	      eh_tos->next = eh_next;
	    }
	    eh_tos = eh_next;
	    eh_rgn = ++eh_id;
	    eh_tos->rgn = eh_rgn;
	  }
	  break;
	case LKIND_END_EH_RANGE:

	  /* End of an eh region; restore the outer region's ID.
	   */
	  eh_tos = eh_tos->prev;
	  Is_True(eh_tos, ("exception stack underflow"));
	  break;
	}
      }
    }

    /* Set the exception region ID for this block.
     */
    bbinfo->eh_rgn = eh_rgn;
    if (eh_rgn != prev_eh_rgn) {
      prev_eh_rgn = eh_rgn;
      pseudo_eh_rgn++;
    }
    bbinfo->pseudo_eh_rgn = pseudo_eh_rgn;

    /* Determine if BB is in cold region or not. We cache this in
     * the BB-info rather than overload BB_local_flag1 since cflow
     * is a sub-phase, not a phase.
     */
    bbinfo->cold = BB_Is_Cold(bb);

    /* Set the BBKIND and fill in kind specific info.
     */
    bbinfo->kind = bbkind;

    switch (bbkind) {
    case BBKIND_TAIL_CALL:
    case BBKIND_REGION_EXIT:
    case BBKIND_RETURN:

      /* No successors or kind-specific info for these.
       */
      bbinfo->nsuccs = 0;
      continue;

    case BBKIND_CALL:
      bbinfo->nsuccs = BB_succs(bb) ? 1 : 0;
      if (BB_succs(bb)) {
	bbinfo->succs[0].bb = BBLIST_item(BB_succs(bb));
	bbinfo->succs[0].offset = 0;
	bbinfo->succs[0].prob = 1.0;
      }
      continue;

    case BBKIND_UNKNOWN:

      /* No idea what this is. Announce we won't do anything interesting
       * and give up.
       */
      DevWarn("cflow giving up because BB:%d's kind is UNKNOWN", BB_id(bb));
      return FALSE;

    case BBKIND_GOTO:
      bbinfo->nsuccs = BB_succs(bb) ? 1 : 0;
      if (BB_succs(bb)) {
	TN *lab_tn;
	OP *br = BB_branch_op(bb);
	BB *target = BBLIST_item(BB_succs(bb));

	bbinfo->succs[0].bb = target;
	bbinfo->succs[0].offset = 0;
	bbinfo->succs[0].prob = 1.0;

	if (br) {
	  INT tfirst;
	  INT tcount;
	  CGTARG_Branch_Info(br, &tfirst, &tcount);
	  FmtAssert(tcount == 1, ("unexpected number of branch targets"));
	  lab_tn = OP_opnd(br, tfirst);
	  if (TN_is_label(lab_tn)) {
	    INT64 offset = TN_offset(lab_tn);
	    if (offset != 0) {
	      BB *orig_target = target;
	      if (!Normalize_Branch_Target(&target, &offset)) {
		DevWarn("BB:%d branches past end of target BB:%d at line %d of %s",
			BB_id(bb), BB_id(target), __LINE__, __FILE__);
		return FALSE;
	      }
	      bbinfo->succs[0].offset = offset;

	      if (target != orig_target) {
		LABEL_IDX lab = Gen_Label_For_BB(target);
		TN *lab_tn = Gen_Label_TN(lab, offset);
		Set_OP_opnd(br, tfirst, lab_tn);
		Cflow_Change_Succ(bb, 0, orig_target, target);
	      }
	    }
	  }
	} else if (target != BB_next(bb)) {
	  DevWarn("BB:%d doesn't fall through, but branch inst not found at line %d of %s",
		  BB_id(bb), __LINE__, __FILE__);
	  return FALSE;
	}
      }
      continue;

    case BBKIND_LOGIF:
#ifdef TARG_IA64
    case BBKIND_CHK: // bug fix for OSP_104, OSP_105, OSP_192
#endif
#if defined(TARG_SL)
    case BBKIND_ZDL_BODY:
    case BBKIND_FORK:
#endif
      {
	INT tfirst;
	INT tcount;
	TN *lab_tn;
	BB *target;
	BBLIST *target_edge;
	BBLIST *fall_through_edge;
	OP *br = BB_branch_op(bb);
#ifdef TARG_IA64
	if ((br == NULL) || OP_noop(br))// bug fix for OSP_104, OSP_105, OSP_192
       {
          br = BB_Last_chk_op(bb);  
       }
#endif

	/* Get the targets. Note that target[0] is always the "true" target.
	 */
	target_edge = BB_succs(bb);
	fall_through_edge = BBLIST_next(target_edge);
	target = BBLIST_item(target_edge);
	if (fall_through_edge == NULL) {
	  fall_through_edge = target_edge;
	} else if (target == BB_next(bb)) {
	  target_edge = fall_through_edge;
	  fall_through_edge = BB_succs(bb);
	  target = BBLIST_item(target_edge);
	}

	bbinfo->nsuccs = 2;

	bbinfo->succs[0].bb = target;
	bbinfo->succs[0].offset = 0;
	bbinfo->succs[0].prob = BBLIST_prob(target_edge);

	bbinfo->succs[1].bb = BB_next(bb);
	bbinfo->succs[1].offset = 0;
	bbinfo->succs[1].prob = BBLIST_prob(fall_through_edge);

#if defined(TARG_SL)
	if(br == NULL)  continue;
#endif
	CGTARG_Branch_Info(br, &tfirst, &tcount);
	FmtAssert(tcount == 1, ("unexpected number of branch targets"));
	lab_tn = OP_opnd(br, tfirst);
	if (TN_is_label(lab_tn)) {
	  INT64 offset = TN_offset(lab_tn);
	  if (offset != 0) {
	    BB *orig_target = target;
	    if (!Normalize_Branch_Target(&target, &offset)) {
	      DevWarn("BB:%d branches past end of target BB:%d at line %d of %s",
		      BB_id(bb), BB_id(target), __LINE__, __FILE__);
	      return FALSE;
	    }
	    bbinfo->succs[0].offset = offset;

	    if (target != orig_target) {
	      LABEL_IDX lab = Gen_Label_For_BB(target);
	      TN *lab_tn = Gen_Label_TN(lab, offset);
	      Set_OP_opnd(br, tfirst, lab_tn);
	      Cflow_Change_Succ(bb, 0, orig_target, target);
	    }
	  }
	}

	/* Determine the branch condition and operands of the compare.
	 */
	{
	  TN *tn1 = NULL;
	  TN *tn2 = NULL;
	  OP *cmp = NULL;
	  VARIANT variant = CGTARG_Analyze_Compare(br, &tn1, &tn2, &cmp);

	  /* If the compare op has a predicate that isn't always true,
	   * then this effectively is part of the comparison expression
	   * and we don't have a way expressing that here so pretend
	   * as if we couldn't find the compare.
	   */
	  if (   cmp != NULL 
	      && OP_has_predicate(cmp) 
	      && !TN_is_true_pred(OP_opnd(cmp,OP_PREDICATE_OPND)))
	  {
	    cmp = NULL;
	  }

	  /* When we can't find the compare, analyze the branch instead.
	   * CGTARG_Analyze_Compare used to do this for us, but its
	   * interface isn't consistent for all archs, so make sure we
	   * get the behavior we count on.
	   */
	  if (cmp == NULL) {
	    variant = CGTARG_Analyze_Branch(br, &tn1, &tn2);
	    cmp = br;
	  }

	  bbinfo->u.l.variant = variant;
	  bbinfo->u.l.tn1 = tn1;
	  bbinfo->u.l.tn2 = tn2;
	  bbinfo->u.l.compare_op = cmp;
	  bbinfo->u.l.b_likely = OP_likely(br) != 0;
	}
	continue;
      }

    case BBKIND_VARGOTO:
    case BBKIND_INDGOTO:
      {
	BBLIST *succ;
	INT nsuccs;
        if (bbkind == BBKIND_VARGOTO) {
	  WN *br_wn = BB_branch_wn(bb);
	  ST *listvar = br_wn ? WN_st(br_wn) : NULL;
	  bbinfo->u.v.listvar = listvar;
	  bbinfo->u.v.refcount = ListVar_RefCount(listvar);
	  ++(*bbinfo->u.v.refcount);
	}

        for (nsuccs = 0, succ = BB_succs(bb); 
	     succ != NULL;
	     ++nsuccs, succ = BBLIST_next(succ)
	) {
	  bbinfo->succs[nsuccs].bb = BBLIST_item(succ);
	  bbinfo->succs[nsuccs].offset = 0;
	  bbinfo->succs[nsuccs].prob = BBLIST_prob(succ);
	}
	bbinfo->nsuccs = nsuccs;

#ifdef KEY
	/* bug#1047
	   An indirect goto could have no successor in the
	   current pu, like
	      int jump () { goto * (int (*) ()) 0xbabebec0; }
	 */
	if( nsuccs == 0 &&
	    bbkind == BBKIND_INDGOTO ){
	  DevWarn( "%s BB:%d has no successors", 
		   BBKIND_Name(bbkind), BB_id(bb) );
	} else
#endif

	  FmtAssert(nsuccs, ("%s BB:%d has no successors", 
			     BBKIND_Name(bbkind), BB_id(bb)));
	continue;
      }
    }

    DevWarn("Don't know how to generate BBINFO for BB:%d at line %d of %s",
	    BB_id(bb), __LINE__, __FILE__);
    return FALSE;
  }

  have_eh_regions = eh_id != 0;

  return TRUE;
}


/* ====================================================================
 *
 * Is_Empty_BB
 *
 * Return a boolean to indicate if <bb> is empty. A BB with only
 * a terminating branch (and an optional noop in the delay slot)
 * is considered empty.
 *
 * ====================================================================
 */
static BOOL
Is_Empty_BB(BB *bb)
{

  /* Consider "GRA spill" BBs non-empty, since they may end up
   * with GRA spills and we want to leave the empty blocks to
   * give GRA good places to spill things around loops.
   */
  if (BB_gra_spill(bb)) return FALSE;

  switch (BB_length(bb)) {
#ifdef TARG_IA64
  case 3:
  	if (BBINFO_kind(bb) == BBKIND_GOTO
	&& BBINFO_nsuccs(bb)
	&& BBINFO_cold(BBINFO_succ_bb(bb, 0)) != BBINFO_cold(bb)) return FALSE;
    if (BB_branch_op(bb) != NULL && OP_noop(BB_first_op(bb)) && OP_noop(BB_first_op(bb)->next)) return TRUE;
#endif
  case 2:
    if (!PROC_has_branch_delay_slot() || !OP_noop(BB_last_op(bb))) return FALSE;
    /*FALLTHROUGH*/
  case 1:
    if (BBINFO_kind(bb) == BBKIND_GOTO
	&& BBINFO_nsuccs(bb)
	&& BBINFO_cold(BBINFO_succ_bb(bb, 0)) != BBINFO_cold(bb)) return FALSE;
#ifdef TARG_SL
    return (BB_branch_op(bb) != NULL && OP_code(BB_branch_op(bb)) != TOP_auxbr);
#else
    return BB_branch_op(bb) != NULL;
#endif
  case 0:
    return TRUE;
  }
  return FALSE;
}

/* ====================================================================
 * ====================================================================
 *
 * Normalization
 *
 * ====================================================================
 * ====================================================================
 */

/* ====================================================================
 *
 * Normalize_Delay_Slots
 *
 * ====================================================================
 */
static BOOL
Normalize_Delay_Slots(void)
{
  BB *bb;
  BOOL changed = FALSE;

  for (bb = REGION_First_BB; bb; bb = BB_next(bb)) {
    OP *br_op = BB_branch_op(bb);
    if (br_op && OP_next(br_op) == NULL) {
      OP *op;
      INT delay_offset;
      BB *delay_bb;
      OP *delay_op;

      /* Find the offset of where the delay slot op will be moved to.
       */
      for (delay_offset = 0, op = BB_first_op(bb); op; op = OP_next(op)) {
	INT num_ops = OP_Real_Ops(op);
	delay_offset += num_ops * 4;
      }

      for (delay_bb = BB_next(bb);; delay_bb = BB_next(delay_bb)) {
	BBLIST *pred_edge;

	FmtAssert(delay_bb, ("couldn't find delay slot op BB for BB:%d", BB_id(bb)));
	FOR_ALL_BB_PREDS(delay_bb, pred_edge) {
	  INT nsuccs;
	  INT i;
	  BOOL found_succ;
	  BB *pred = BBLIST_item(pred_edge);
	  if (Falls_Thru(pred, delay_bb)) continue;

	  found_succ = FALSE;
	  nsuccs = BBINFO_nsuccs(pred);
	  for (i = 0; i < nsuccs; ++i) {
	    if (BBINFO_succ_bb(pred, i) == delay_bb) {
	      INT64 offset = BBINFO_succ_offset(pred, i);
	      if (offset == 0) {
		Cflow_Change_Succ(pred, i, delay_bb, bb);
		Set_BBINFO_succ_offset(pred, i, delay_offset);
DevWarn("moving BB:%d edge to delay slot op", BB_id(pred));
	      } else {
		Set_BBINFO_succ_offset(pred, i, offset - 4);
DevWarn("adjust BB:%d edge offset to delay BB", BB_id(pred));
	      }
	      found_succ = TRUE;
	    }
	  }
	  FmtAssert(found_succ, ("no adjustments made on BB:%d -> BB:%d edge",
				 BB_id(pred), BB_id(delay_bb)));
	  changed = TRUE;
	}

	FOR_ALL_BB_OPs_FWD(delay_bb, delay_op) {
	  INT num_ops = OP_Real_Ops(delay_op);
	  if (num_ops != 0) {
	    FmtAssert(num_ops == 1, ("BB:%d delay slot op is > 1 real ops",
				     BB_id(bb)));
	    goto found_delay_op;
	  }
	}
      }
    found_delay_op:
      DevWarn("BB:%d has delay slot op in BB:%d", BB_id(bb), BB_id(delay_bb));

      BB_Remove_Op(delay_bb, delay_op);
      BB_Append_Op(bb, delay_op);
    }
  }

  return changed;
}

/* ====================================================================
 * ====================================================================
 *
 * Redundant/empty branch removal routines
 *
 * ====================================================================
 * ====================================================================
 */


/* ====================================================================
 *
 * Collapse_Empty_Goto
 *
 * Given a BB pointer, follow any chain of empty GOTO BBs, returning
 * the first non-empty or non-GOTO BB in the chain (which may be the
 * BB passed).  Note that bp is passed just for tracing and should be the
 * predecessor of <targ>. <in_freq> is the frequency that <bp> goes
 * to <targ>.
 *
 * ====================================================================
 */
static BB *
Collapse_Empty_Goto(BB *bp, BB *targ, float in_freq)
{
  BB_NUM cnt;
  BB *new_targ;

  if (BBINFO_cold(bp) != BBINFO_cold(targ)) return targ;

#ifdef TARG_X8664
  if( BB_savexmms_op(bp) != NULL )
    return targ;
#endif

  new_targ = targ;
  cnt = 0;
  while (   BBINFO_kind(new_targ) == BBKIND_GOTO 
	 && Is_Empty_BB(new_targ)
	 && !BB_Has_Exc_Label(new_targ)
	 && BBINFO_nsuccs(new_targ)
	 && BBINFO_succ_bb(new_targ, 0) != new_targ
            && !ANNOT_Get(BB_annotations(new_targ), ANNOT_LOOPINFO)
            && !ANNOT_Get(BB_annotations(BBINFO_succ_bb(new_targ,0)), ANNOT_LOOPINFO)
  ) {
    BB *last_targ;
    if (freqs_computed || BB_freq_fb_based(new_targ)) {
      float new_freq = BB_freq(new_targ) - in_freq;
      if (new_freq < 0.0F) {
	if (  CG_warn_bad_freqs
	    && !BB_freq_fb_based(new_targ)
	    && !FREQ_Match(BB_freq(new_targ), in_freq)
	) {
	  DevWarn("cflow (Collapse_Empty_Goto) found inconsistent freqs:\n"
		  "\tmost likely, something before cflow is broken.\n"
		  "\tcflow will cope but please submit a pv");
	}
	new_freq = 0.0F;
      }
      BB_freq(new_targ) = new_freq;
    }
    last_targ = new_targ;
    new_targ = BBINFO_succ_bb(new_targ, 0);

    /* Since this is an empty GOTO, if it has any note annotations,
     * they will ultimately get lost when we delete the empty BB,
     * so transfer the notes to the target BB.
     */
    if (BB_has_note(last_targ)) {
      BB_Copy_Annotations(new_targ, last_targ, ANNOT_NOTE);
      Reset_BB_has_note(last_targ);
    }

    cnt++;
    if (cnt >= PU_BB_Count) break; /* prevent infinite loop in non-trivial
				      cycles */
  }

  if (CFLOW_Trace_Branch && new_targ != targ) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "replacing target BB:%d of %s BB:%d with BB:%d\n",
		   BB_id(targ),
		   BBKIND_Name(BBINFO_kind(bp)), BB_id(bp), 
		   BB_id(new_targ));
  }

  return new_targ;
}


/* ====================================================================
 *
 * Redundant_Logif
 *
 * 'pred' and 'succ' are both LOGIF BBs and 'pred' jumps directly to
 * the conditional branch of 'succ'. Determine if the test in
 * 'succ' is redundant, and return via 'negated' a flag that indicates
 * if the two comparisons are the negative of each other.
 *
 * TODO: The test for the second branch condition being equal to
 * the first is too strict. For example, if the first test is a 'bne'
 * against zero, and the second is a 'blez' then we can still perform
 * the optimization.
 *
 * ====================================================================
 */
static BOOL
Redundant_Logif(BB *pred, BB *succ, BOOL *pnegated)
{
  OP *pred_cmp, *succ_cmp;
  BOOL negated;
  VARIANT pred_variant = BBINFO_variant(pred);
  VARIANT succ_variant = BBINFO_variant(succ);

  /* The comparison variant must be the same or negated.
   */
  if (pred_variant == succ_variant) {
    negated = FALSE;
  } else if (Negate_BR_Variant(pred_variant) == succ_variant) {
    negated = TRUE;
  } else {
    return FALSE;
  }

  /* The operands of the comparison must be the same.
   */
  if (   BBINFO_condval1(pred) != BBINFO_condval1(succ)
      || BBINFO_condval2(pred) != BBINFO_condval2(succ)) return FALSE;

  /* The comparison OP must be the same OP, or the conditional branch.
   * If we allow arbitrary comparison OPs, then we have to carefully
   * analyze the definitions of its operands to be correct (see pv481181).
   */
  pred_cmp = BBINFO_compare_op(pred);
  succ_cmp = BBINFO_compare_op(succ);
  if (   pred_cmp != succ_cmp
      && (pred_cmp != BB_branch_op(pred) || succ_cmp != BB_branch_op(succ))
  ) return FALSE;

#ifdef TARG_NVISA
  // can have differing cmp, same variant (P_TRUE),
  // and both are branch_op cause couldn't find real source of compare.
  // Check for one being negate of the other.
  if (pred_cmp != succ_cmp) {
    DevWarn("differing cmp ops");
    if (OP_code(pred_cmp) == TOP_bra_p && OP_code(succ_cmp) == TOP_bra_np)
      negated = TRUE;
    else if (OP_code(pred_cmp) == TOP_bra_np && OP_code(succ_cmp) == TOP_bra_p)
      negated = TRUE;
  }
#endif

#ifdef KEY
  /* A BBINFO_condval could be re-defined by an op which is scheduled
     at the delay slot. */
  if( PROC_has_branch_delay_slot() ){
    OP* last_op = BB_last_op( pred );
    if( !OP_br( last_op ) ){
      for( int i = 0; i < OP_results( last_op ); i++ ){
	TN* result = OP_result( last_op, i );
	if( result == BBINFO_condval1(succ) ||
	    result == BBINFO_condval2(succ) ){
	  // Is_True( false, ("TN is re-defined more than once.\n") );// 11657
	  return FALSE;
	}
      }
    }
  }
#endif

  /* They're redundant!
   */
  *pnegated = negated;
  return TRUE;
}


/* ====================================================================
 *
 * Collapse_Same_Logif
 *
 *                (a c.op b)                      (a c.op b)
 *             F /       |                      F /       |
 *              /        |                       /        |
 *     (a c.op b)        |T     or     (a ~c.op b)        |T  
 *  F /          \T      |           T /          \F      |
 *   /            \      |            /            \      |
 *  lab2          BBx   BBy         lab2           BBx   BBy
 * 
 * is the same as the first branch (false target) to lab2, assuming the 
 * 2nd BB is only a branch condition. This happens for code like: 
 * if (a) { if (a && b)...
 *
 * 'bp' is the first LOGIF BB. 'targ' is the second LOGIF BB.
 * 'targ_idx' is the index of 'targ' in 'bp's target list.
 * 'edge_freq' is the frequency of the edge 'bp'->'targ'.
 *
 * ====================================================================
 */
static BB * 
Collapse_Same_Logif(BB *bp, BB *targ, INT targ_idx, float edge_freq)
{
  BOOL negated;
  BB *succ = Collapse_Empty_Goto(bp, targ, edge_freq);
  BB *new_succ = succ;

  Is_True(BBINFO_kind(bp) == BBKIND_LOGIF,
	  ("Collapse_Same_Logif called with %s BB:%d",
	   BBKIND_Name(BBINFO_kind(bp)), BB_id(bp)));

  /* The branch must directly target another conditional branch,
   * so make sure that is the only thing in the block (ignoring
   * the branch delay slot). The targetted branch must be
   * a conditional branch with the same arguments. And finally,
   * the first branch cannot have an OP in its delay slot that
   * defines one of the branch operands.
   */
  if (   bp != succ
      && Is_Empty_BB(succ) 
      && BBINFO_kind(succ) == BBKIND_LOGIF
      && Redundant_Logif(bp, succ, &negated))
  {
    if (PROC_has_branch_delay_slot()) {
      OP *br_op = BB_branch_op(bp);
      OP *last_op = BB_last_op(bp);
      if (   last_op != br_op
	  && OP_has_result(last_op)
	  && OP_Refs_TN(br_op, OP_result(last_op,0))) return succ;
    }

    new_succ = BBINFO_succ_bb(succ, targ_idx ^ negated);
  }

  if (new_succ != succ) {
    if (freqs_computed || BB_freq_fb_based(succ)) {

      /* Remove the contribution that was made to the second LOGIF
       * BB, since we no longer go there. Note that we don't need to 
       * add it to new_succ, since we reached it before and after the change.
       */
      float new_freq = BB_freq(succ) - edge_freq;
      if (new_freq < 0.0F) {
	if (   CG_warn_bad_freqs 
	    && !BB_freq_fb_based(succ)
	    && !FREQ_Match(BB_freq(succ), edge_freq)
	) {
	  DevWarn("cflow (Collapse_Same_Logif) found inconsistent freqs:\n"
		  "\tmost likely, something before cflow is broken.\n"
		  "\tcflow will cope but please submit a pv");
	}
	new_freq = 0.0F;
      }
      BB_freq(succ) = new_freq;
    }

    if (CFLOW_Trace_Branch) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "replacing target BB:%d of LOGIF BB:%d with BB:%d\n",
		    BB_id(succ),
		    BB_id(bp), 
		    BB_id(new_succ));
    }
  }

  return new_succ;
}


/* ====================================================================
 *
 * Convert_Indirect_Goto_To_Direct
 *
 * The operand is a VARGOTO/INDGOTO BB.  Determine if the block always 
 * branches to the same BB, and if so convert it to a GOTO BB.  
 * Return TRUE if the BB is converted.  
 *
 * ====================================================================
 */
static BOOL
Convert_Indirect_Goto_To_Direct ( BB *bp )
{
  INT i;
  BB *target;
  ST *listvar;
  BOOL vargoto = (BBINFO_kind(bp) == BBKIND_VARGOTO);
  INT nsuccs = BBINFO_nsuccs(bp);

  /* I'm not sure what this would mean, but we certainly can't handle it.
   */
  if (nsuccs == 0) return FALSE;

  /* See if all the targets are the same.
   */
  target = BBINFO_succ_bb(bp, 0);
  for (i = 1; i < nsuccs; ++i) {
    if (BBINFO_succ_bb(bp, i) != target) return FALSE;
  }

  /* If we perform the optimization, we have to remove the jump table since
   * if it is no longer used (otherwise it might reference undefined symbols).
   */
  if (vargoto) {
    listvar = BBINFO_vargoto_listvar(bp);
    if (listvar == NULL) {
      DevWarn("Convert_Indirect_Goto_To_Direct: unable to convert BB:%d, no listvar",
	      BB_id(bp));
      return FALSE;
    }
    if (--(*BBINFO_vargoto_refcount(bp)) == 0) Set_ST_is_not_used(listvar);
  }

  /* The preds/succs lists for <bp> should already be correct, but
   * verify it anyway.
   */
  BBLIST_prob(BB_Find_Succ(bp, target)) = 1.0;
  Is_True(   BBlist_Len(BB_succs(bp)) == 1
	  && BBLIST_item(BB_succs(bp)) == target
	  && BB_in_preds(target, bp),
	  ("BB:%d preds/succs don't match BBINFO", BB_id(bp)));

  /* Update the BBINFO.
   */
  Set_BBINFO_kind(bp, BBKIND_GOTO);
  Set_BBINFO_nsuccs(bp, 1);
  Set_BBINFO_succ_bb(bp, 0, target);
  Set_BBINFO_succ_offset(bp, 0, 0);
  Set_BBINFO_succ_prob(bp, 0, 1.0);

  /* Remove switch annotation.
   */
  if (vargoto) Remove_Annotations(bp, ANNOT_SWITCH);

  /* Must remove the old jump inst; no need to replace it with an 
   * unconditional one, Finalize_BB will handle it.
   */
  BB_Remove_Branch(bp);

  if (CFLOW_Trace_Branch) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "changing %s BB:%d to GOTO BB:%d\n",
		   vargoto ? "VARGOTO" : "INDGOTO",
		   BB_id(bp), BB_id(BBINFO_succ_bb(bp, 0)));
  }
  return TRUE;
}


/* ====================================================================
 *
 * Identical_Return_Blocks
 *
 * Given two BBs, return a boolean that indicates if they are both
 * return blocks and contain the same sequence of instructions.
 * NOTE: this could be alot more sophisticated. For example, we do not
 * account for differences in local TNs. Given that rather big limitation,
 * we will still detect return blocks that have not been modified since
 * their creation.
 *
 * ====================================================================
 */
static BOOL
Identical_Return_Blocks(BB *bb1, BB *bb2)
{
  OP *op1, *op2;
  if (BB_length(bb1) != BB_length(bb2)) return FALSE;
  if (BBINFO_kind(bb1) != BBKIND_RETURN) return FALSE;
  if (BBINFO_kind(bb2) != BBKIND_RETURN) return FALSE;

  for (op1 = BB_first_op(bb1), op2 = BB_first_op(bb2);
       op1;
       op1 = OP_next(op1), op2 = OP_next(op2)
  ) {
    INT i;
    if (OP_code(op1) != OP_code(op2)) return FALSE;
    if (OP_flags(op1) != OP_flags(op2)) return FALSE;
    for (i = 0; i < OP_results(op1); ++i) {
      if (OP_result(op1,i) != OP_result(op2,i)) return FALSE;
    }
    for (i = 0; i < OP_opnds(op1); ++i) {
      if (OP_opnd(op1,i) != OP_opnd(op2,i)) return FALSE;
    }
  }

  return TRUE;
}


/* ====================================================================
 *
 * Convert_If_To_Goto
 *
 * The operand is a LOGIF BB.  Determine whether it has a constant
 * condition, and if so convert it to a GOTO BB.  Return TRUE if the
 * BB is converted.  In addition, if the LOGIF always branches to the
 * same BB, or different but identical return blocks, convert it as well.
 *
 * ====================================================================
 */
static BOOL
Convert_If_To_Goto ( BB *bp )
{
  INT64 v1,v2;
  BOOL result;
  TN* tn1 = NULL;
  TN* tn2 = NULL;
  OP *compare_op = NULL;
  VARIANT br_variant = V_br_condition(BBINFO_variant(bp));
  BOOL false_br = V_false_br(BBINFO_variant(bp)) != 0;
#ifdef TARG_IA64
  OP *op_br=BB_xfer_op(bp);
  TN *tn = OP_opnd(op_br, 0);
  if (tn==True_TN)  {
  	return FALSE;
  }
#endif
  /* Give up immediately on branch variants we can't handle.
   */
  switch (br_variant) {
  case V_BR_NONE:
  case V_BR_CLOOP:
  case V_BR_CTOP:
  case V_BR_CEXIT:
  case V_BR_WTOP:
  case V_BR_WEXIT:
    return FALSE;
  }

  /* A non-branch-likely whose true and false targets are the same
   * or are different but identical return blocks can be made unconditional.
   */
  if (   !BBINFO_b_likely(bp)
      && BBINFO_succ_offset(bp, 0) == BBINFO_succ_offset(bp, 1)
      && (   BBINFO_succ_bb(bp, 0) == BBINFO_succ_bb(bp, 1)
	  || Identical_Return_Blocks(BBINFO_succ_bb(bp, 0), 
				     BBINFO_succ_bb(bp, 1)))
  ) {
    result = FALSE;
    goto convert_it;
  }
  
  /* Get the operands of the comparison and try to get their values.
   */
  
  tn1 = BBINFO_condval1(bp); 
  tn2 = BBINFO_condval2(bp); 
  compare_op = BBINFO_compare_op(bp);

#ifdef TARG_X8664
  /* load execute TOP access memory, so is not possible to be evaluated 
   * at compile time. */
  if (TOP_is_load_exe(OP_code(compare_op)))
    return FALSE;
#endif

#ifdef TARG_LOONGSON
  /* for loongson, there are some branch instructions with only one operand, such as:
       "mipsbgez",
       "mipsbgezal",
       "mipsbgtz",
       "mipsblez",
       "mipsbltz",
       "mipsbltzal",
       for the instructions, there is only one operand and the second implicit operand is 
       Zero_TN.
    */
  if (tn2 == NULL)  {     
  	tn2 = Zero_TN;
  }
#endif
  
  Is_True(tn1 != NULL, ("compare with no operands in BB:%d", BB_id(bp)));

  if (!TN_Value_At_Op(tn1, compare_op, &v1)) goto try_identities;
#ifdef TARG_X8664
  Extend_Truncate_Short_Cmp_Src(compare_op, br_variant, &v1);
#endif

  if (tn2 && !TN_Value_At_Op(tn2, compare_op, &v2)) goto try_identities;
#ifdef TARG_X8664
  Extend_Truncate_Short_Cmp_Src(compare_op, br_variant, &v2);
#endif

  /* Evaluate the condition.
   */
  switch (br_variant) {
  case V_BR_I4EQ:
  case V_BR_U4EQ: result = (INT32)v1 == (INT32)v2; break;
  case V_BR_I4NE:
  case V_BR_U4NE: result = (INT32)v1 != (INT32)v2; break;
  case V_BR_I4GE: result = (INT32)v1 >= (INT32)v2; break;
  case V_BR_I4GT: result = (INT32)v1 > (INT32)v2; break;
  case V_BR_I4LE: result = (INT32)v1 <= (INT32)v2; break;
  case V_BR_I4LT: result = (INT32)v1 < (INT32)v2; break;
  case V_BR_U4GT: result = (UINT32)v1 > (UINT32)v2; break;
  case V_BR_U4GE: result = (UINT32)v1 >= (UINT32)v2; break;
  case V_BR_U4LT: result = (UINT32)v1 < (UINT32)v2; break;
  case V_BR_U4LE: result = (UINT32)v1 <= (UINT32)v2; break;
  case V_BR_I8EQ:
  case V_BR_U8EQ: result = v1 == v2; break;
  case V_BR_I8NE:
  case V_BR_U8NE: result = v1 != v2; break;
  case V_BR_I8GE: result = v1 >= v2; break;
  case V_BR_I8GT: result = v1 > v2; break;
  case V_BR_I8LE: result = v1 <= v2; break;
  case V_BR_I8LT: result = v1 < v2; break;
  case V_BR_U8GT: result = (UINT64)v1 > (UINT64)v2; break;
  case V_BR_U8GE: result = (UINT64)v1 >= (UINT64)v2; break;
  case V_BR_U8LT: result = (UINT64)v1 < (UINT64)v2; break;
  case V_BR_U8LE: result = (UINT64)v1 <= (UINT64)v2; break;
  default:
    #pragma mips_frequency_hint NEVER
    DevWarn("Unhandled branch VARIANT (%lld) for BB:%d at line %d of %s",
	    (INT64)br_variant, BB_id(bp), __LINE__, __FILE__);
    return FALSE;
  }
 
/* Convert the BB:
 */
convert_it:
  {
    BOOL br_taken = result ^ false_br;
    INT itarg = br_taken ? 0 : 1;
    INT64 offset = BBINFO_succ_offset(bp, itarg);
    BB *targ = BBINFO_succ_bb(bp, itarg);
    BB *dead = BBINFO_succ_bb(bp, !itarg);
    OP *br = BB_branch_op(bp);

    /* Update block freqs to reflect that we can no longer get to
     * one of the targets via this branch. Note that for the case
     * where the true and false targets are the same block this is
     * unnecessary, but it doesn't hurt.
     */
    if (freqs_computed) {
      float freq = BB_freq(bp) * BBINFO_succ_prob(bp, !itarg);
      BB_freq(targ) += freq;
      BB_freq(dead) -= freq;
      if (BB_freq(dead) < 0.0F) BB_freq(dead) = 0.0F;
    }

    /* If this was a branch-likey, then the delay slot OP would have
     * only been executed when the branch was taken. Remove the delay
     * slot OP (if there is one) if we never take the branch.
     */
    if (!br_taken && BBINFO_b_likely(bp) && br != BB_last_op(bp)) {
      BB_Remove_Op(bp, BB_last_op(bp));
    }

    /* Must remove the old conditional branch inst; no need to replace
     * it with an unconditional one, Finalize_BB will handle it.
     */
#ifdef TARG_X8664
    if( compare_op != NULL ){
      FmtAssert( OP_bb( compare_op ) == bp,
		 ("compare op and branch op are located at different bbs") );
      // Delete the compare if its result isn't needed.
      OP *op;
      BOOL delete_compare = TRUE;
      for (op = OP_next(compare_op); ; op = OP_next(op)) {
	if (op == br)
	  break;
	else if (OP_reads_rflags(op)) {
	  delete_compare = FALSE;
	  break;
	}
      }
      if (delete_compare)
	BB_Remove_Op( bp, compare_op );
    }
#endif
    BB_Remove_Op(bp, br);

    /* Update succs/preds lists and verify we're in sync.
     */
#ifdef TARG_IA64
    if (targ != dead) {
	    if (IPFEC_Enable_Region_Formation && RGN_Formed) {
		    RGN_Unlink_Pred_Succ(bp,dead);
	    } else {
		    Unlink_Pred_Succ(bp, dead);
        } 
    }
#else
    if (targ != dead) Unlink_Pred_Succ(bp, dead);
#endif
    BBLIST_prob(BB_Find_Succ(bp, targ)) = 1.0;
    Is_True(   BBlist_Len(BB_succs(bp)) == 1
	    && BBLIST_item(BB_succs(bp)) == targ
	    && BB_in_preds(targ, bp),
	    ("BB:%d preds/succs don't match BBINFO", BB_id(bp)));

    /* Update the BBINFO:
     */
    Set_BBINFO_kind(bp, BBKIND_GOTO);
    Set_BBINFO_nsuccs(bp, 1);
    Set_BBINFO_succ_bb(bp, 0, targ);
    Set_BBINFO_succ_offset(bp, 0, offset);
    Set_BBINFO_succ_prob(bp, 0, 1.0);

    if ( CFLOW_Trace_Branch ) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "changing BB:%d from LOGIF to GOTO-BB:%d\n",
		      BB_id(bp), BB_id(targ));
    }
  }
  return TRUE;

try_identities:
  if ( tn1 == tn2 ) {
    switch (br_variant) {
    case V_BR_FGE:
    case V_BR_DGE:
    case V_BR_FLE:
    case V_BR_DLE:
    case V_BR_FEQ:
    case V_BR_DEQ: 
      if (Force_IEEE_Comparisons) break;
      /*FALLTHROUGH*/
    case V_BR_I4GE: 
    case V_BR_I4LE: 
    case V_BR_U4GE: 
    case V_BR_U4LE: 
    case V_BR_I4EQ: 
    case V_BR_U4EQ:
    case V_BR_I8GE: 
    case V_BR_I8LE: 
    case V_BR_U8GE: 
    case V_BR_U8LE: 
    case V_BR_I8EQ: 
    case V_BR_U8EQ:
      result = TRUE; goto convert_it;

    case V_BR_FLT:
    case V_BR_DLT:
    case V_BR_FGT:
    case V_BR_DGT:
    case V_BR_FNE:
    case V_BR_DNE:
      if (Force_IEEE_Comparisons) break;
      /*FALLTHROUGH*/
    case V_BR_I4LT: 
#ifdef KEY
    case V_BR_I4GT:
#endif
    case V_BR_U4GT: 
    case V_BR_U4LT: 
    case V_BR_I4NE: 
    case V_BR_U4NE:
    case V_BR_I8GT: 
    case V_BR_I8LT: 
    case V_BR_U8GT: 
    case V_BR_U8LT: 
    case V_BR_I8NE: 
    case V_BR_U8NE:
      result = FALSE; goto convert_it;

    case V_BR_FOR:
    case V_BR_FUO:
    case V_BR_DOR:
    case V_BR_DUO: break;

    default:
      #pragma mips_frequency_hint NEVER
      DevWarn("Unhandled branch VARIANT (%lld) for BB:%d at line %d of %s",
	      (INT64)br_variant, BB_id(bp), __LINE__, __FILE__);
#ifdef TARG_X8664
      FmtAssert( false, ("NYI") );
#endif
    }
  }
  return FALSE;
}


/* ====================================================================
 *
 * Branches_Around
 *
 * Detect a LOGIF where one succ is the next block after the last
 * fall-through block of the other succ:
 *
 *	BBi: LOGIF BBj, BBk
 *      BBj: GOTO (anywhere but BBk)
 *	BBk:
 *
 * where BBj==BB_next(BBi) and BBk==BB_next(BBj). BBj can be any
 * sequence of fall-through BBs ending in a block which does not
 * fall-through to BBk.
 *
 * ====================================================================
 */
static BOOL Branches_Around(BB *if_bb)
{
  BB *succ0;
  BB *succ1;
  INT64 offset0;
  INT64 offset1;
  BB *next;

  /* Clearly a loser if not a LOGIF block.
   */
  if (BBINFO_kind(if_bb) != BBKIND_LOGIF) return FALSE;

  /* Get the next and successor blocks.
   */
  next = BB_next(if_bb);
  succ0 = BBINFO_succ_bb(if_bb, 0);
  succ1 = BBINFO_succ_bb(if_bb, 1);
  offset0 = BBINFO_succ_offset(if_bb, 0);
  offset1 = BBINFO_succ_offset(if_bb, 1);

  /* Normalize so <succ0> is <next>.
   */
  if (succ0 != next) {
    BB *t;

    /* If neither succ is next in the chain then quit.
     */
    if (succ1 != next) return FALSE;

    t = succ0;
    succ0 = succ1;
    succ1 = t;

    offset1 = offset0;
  }

  /* We must branch to the start of succ1 in order to fall through.
   */
  if (offset1 != 0) return FALSE;

  /* Now find the first block that doesn't fall through.
   */
  do {
    succ0 = next;
    next = BB_next(succ0);
  } while (Falls_Thru(succ0, next));

  /* If the next block is the other successor, we have a winner.
   */
  return next == succ1;
}


/* ====================================================================
 *
 * Convert_Goto_To_If
 *
 * The operand is a GOTO BB.  Determine whether its target is a
 * LOGIF BB consisting of nothing but a conditional branch, and if so,
 * convert to a LOGIF BB. Return TRUE if the BB is converted.
 *
 * ====================================================================
 */
static BOOL
Convert_Goto_To_If ( BB *bp, mBOOL *used_branch_around )
{
  OP *compare_op;
  OP *old_cond_br;
  OP *new_cond_br;
  BB *targ_0;
  BB *targ_1;
  INT64 offset_0;
  INT64 offset_1;
  float prob_0;
  float prob_1;
  BB *targ;

  /* Give up if the GOTO doesn't goto the start of the target -- it's
   * too complicated.
   */
  if (BBINFO_nsuccs(bp) == 0 || BBINFO_succ_offset(bp, 0) != 0) return FALSE;

  /* Give up if the GOTO target is something other than a BB with only
   * a conditional branch. The branch cannot be a branch-likely.
   */
  targ = BBINFO_succ_bb(bp, 0);
  if (   BBINFO_kind(targ) != BBKIND_LOGIF
      || BB_length(targ) != 1
      || BBINFO_b_likely(targ)
  ) return FALSE;

  /* Give up if either block has been scheduled
   */
  if (BB_scheduled(bp) || BB_scheduled(targ)) return FALSE;

  /* Get the successors of the target LOGIF block.
   */
  targ_0 = BBINFO_succ_bb(targ, 0);
  targ_1 = BBINFO_succ_bb(targ, 1);
  offset_0 = BBINFO_succ_offset(targ, 0);
  offset_1 = BBINFO_succ_offset(targ, 1);
  prob_0 = BBINFO_succ_prob(targ, 0);
  prob_1 = BBINFO_succ_prob(targ, 1);

#ifdef TARG_IA64
  /* Give up if the GOTO's succ is a region's entry node and the goto does
   * not reside in the same region as its succ. Otherwise a SEME region may
   * be turned into a MEME region. 
   */
  if (IPFEC_Enable_Region_Formation && RGN_Formed) {
      REGIONAL_CFG_NODE *node = Regional_Cfg_Node(targ);
      if (node->Is_Entry()) return FALSE;
      if (node->Is_Loop_Tail()) {
        DevWarn("LOST ONE CHANCE OF CONVERT GOTO TO IF BECAUSE OF LOOP TAIL!");
        return FALSE;
      }  
  }
#endif

  /* At this point it is possible to do a correct conversion. The only
   * issue is if we want to avoid code expansion. Determine if we
   * have a reason to do it. This is coded as an if-then-else-if chain
   * mainly for readability.
   */
  if (current_flags & CFLOW_OPT_ALL_BR_TO_BCOND) {

    /* Because the user said so...
     */
  } else if (   (BB_next(bp) == targ_0 && offset_0 == 0)
	     || (BB_next(bp) == targ_1 && offset_1 == 0)
  ) {

    /* One of the succs of the LOGIF is the start of the block following 
     * the GOTO.
     */
  } else if (   (current_flags & (CFLOW_REORDER | CFLOW_FREQ_ORDER))
	     && !used_branch_around[BB_id(targ)]
	     && Branches_Around(targ))
  {

    /* The target branches around one of its succs. This means that
     * if we were to move the branched around succ here that the
     * LOGIF could still fall through, thus we don't add any branches.
     * This isn't foolproof, but we increase the odds by realizing
     * that we can only move the succ to one place. In the case of
     * multiple branches to a conditional branch, there is no
     * consideration given to which branch is best replaced.
     */
    used_branch_around[BB_id(targ)] = TRUE;
  } else {

    /* We weren't convinced this was profitiable.
     */
    return FALSE;
  }

  /* Remove the unconditional branch (if any) from the GOTO block.
   */
  BB_Remove_Branch(bp);

  /* Copy the conditional branch and append to the GOTO block.
   */
  old_cond_br = BB_branch_op(targ);
  new_cond_br = Dup_OP(old_cond_br);
  BB_Append_Op(bp, new_cond_br);

  /* See if we introduced a new live use into the block.
   */
  if (!CG_localize_tns) {
    INT i;
    for (i = 0; i < OP_opnds(new_cond_br); ++i) {
      TN *tn = OP_opnd(new_cond_br, i);
      if (TN_is_constant(tn)) continue;
      if (TN_is_const_reg(tn)) continue;
      if (!GTN_SET_MemberP(BB_live_def(bp), tn)) {
	if (!TN_is_global_reg(tn))
	  GTN_UNIVERSE_Add_TN(tn);
	// GRA_LIVE_Add_Live_Use_GTN requires 'tn' to be a GTN.
	GRA_LIVE_Add_Live_Use_GTN(bp, tn);
      }
    }
  }

  /* Set the new whirl branch node.
   */
  BB_branch_wn(bp) = BB_branch_wn(targ);

  /* Set the compare-op. The old compare-op may either be the
   * conditional branch, or an OP in some other BB. If the compare-op
   * was the conditional branch, use the new one, otherwise it remains.
   */
  compare_op = BBINFO_compare_op(targ);
  if (compare_op == old_cond_br) compare_op = new_cond_br;

  /* Update preds/succs lists.
   */
#ifdef TARG_IA64
  if (IPFEC_Enable_Region_Formation && RGN_Formed) {
    RGN_Unlink_Pred_Succ(bp,targ);
  } else {
    Unlink_Pred_Succ(bp, targ);
  }
#else
  Unlink_Pred_Succ(bp, targ);
#endif
  
  Is_True(BB_succs(bp) == NULL,
	  ("BB:%d preds/succs don't match BBINFO", BB_id(bp)));
#ifdef TARG_IA64
  if (IPFEC_Enable_Region_Formation && RGN_Formed) {
    RGN_Link_Pred_Succ_With_Prob(bp,targ_0,prob_0);
    RGN_Link_Pred_Succ_With_Prob(bp,targ_1,prob_1);
  } else {
    Link_Pred_Succ_with_Prob(bp, targ_0, prob_0, FALSE, TRUE);
    Link_Pred_Succ_with_Prob(bp, targ_1, prob_1, FALSE, TRUE);
  }
#else
  Link_Pred_Succ_with_Prob(bp, targ_0, prob_0, FALSE, TRUE);
  Link_Pred_Succ_with_Prob(bp, targ_1, prob_1, FALSE, TRUE);
#endif

  /* Now update the BBINFO to reflect that the GOTO block is now
   * a LOGIF block.
   */
  Set_BBINFO_succ_bb(bp, 0, targ_0);
  Set_BBINFO_succ_bb(bp, 1, targ_1);
  Set_BBINFO_succ_offset(bp, 0, offset_0);
  Set_BBINFO_succ_offset(bp, 1, offset_1);
  Set_BBINFO_succ_prob(bp, 0, prob_0);
  Set_BBINFO_succ_prob(bp, 1, prob_1);
  Set_BBINFO_condval1(bp, BBINFO_condval1(targ));
  Set_BBINFO_condval2(bp, BBINFO_condval2(targ));
  Set_BBINFO_compare_op(bp, compare_op);
  Set_BBINFO_variant(bp, BBINFO_variant(targ));
  Set_BBINFO_b_likely(bp, FALSE);
  Set_BBINFO_nsuccs(bp, 2);
  Set_BBINFO_kind(bp, BBKIND_LOGIF);

  /* Adjust frequencies, since we no longer GOTO the target LOGIF block,
   * but rather go directly to its targets.
   */
  if (freqs_computed || (BB_freq_fb_based(bp) && BB_freq_fb_based(targ))) {
    double targ_freq = BB_freq(targ);
    double bb_freq = BB_freq(bp);
    double new_targ_freq = targ_freq - bb_freq;

    if (new_targ_freq < 0.0F) {
      if (    CG_warn_bad_freqs 
	  && !BB_freq_fb_based(bp) 
	  && !BB_freq_fb_based(targ)
	  && !FREQ_Match(targ_freq, bb_freq)
      ) {
	DevWarn("cflow (Convert_Goto_To_If) found inconsistent freqs:\n"
		"\tmost likely, something before cflow is broken.\n"
		"\tcflow will cope but please submit a pv");
      }
      new_targ_freq = 0.0F;
    }
    BB_freq(targ) = new_targ_freq;
  }

  if ( CFLOW_Trace_Branch ) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "changing BB:%d from GOTO-BB:%d to LOGIF-(BB:%d, BB:%d)\n",
		   BB_id(bp),
		   BB_id(targ),
		   BB_id(targ_1),
		   BB_id(targ_0));
  }

  return TRUE;
}


/* ====================================================================
 *
 * Convert_Goto_To_Return
 *
 * The operand is a GOTO BB.  Determine whether its target is a
 * RETURN BB consisting of nothing but a return instruction, if so,
 * convert to a RETURN BB. Return TRUE if the BB is converted.
 *
 * ====================================================================
 */
static BOOL
Convert_Goto_To_Return ( BB *bp )
{
  OP *new_op;
  OP *br_op;
  OP *br_delay_op;
  OP *rtn_op;
  BB *targ;
  INT64 offset;

  /* Get the target BB and offset unless it's external in which case
   * just give up.
   */
  if ( BBINFO_nsuccs(bp) == 0 ) return FALSE;
  targ = BBINFO_succ_bb(bp, 0);
  offset = BBINFO_succ_offset(bp, 0);

  /* Verify that the target BB is a return block and the target instruction
   * is not before the BB!
   */
  if ( BBINFO_kind(targ) != BBKIND_RETURN || offset < 0 ) return FALSE;

  Is_True((offset % INST_BYTES) == 0,
	  ("instruction offset not a multiple of %d", INST_BYTES));

  /* Find the target and instruction and give up if it's not the return
   * Note that an exit block in a region, might not have the exit code
   * generated yet.
   */
  for (rtn_op = BB_first_op(targ);
       ;
       rtn_op = OP_next(rtn_op), offset -= INST_BYTES)
  {
    if ( rtn_op == NULL ) return FALSE;
    if ( offset == 0 ) break;
  }
  if ( !OP_br(rtn_op) ) return FALSE;

  /* If the return has a delay slot op, make sure it's a noop.
   */
  if ( PROC_has_branch_delay_slot() ) {
    OP *rtn_delay_op = OP_next(rtn_op);
    if ( rtn_delay_op && !OP_noop(rtn_delay_op) ) return FALSE;
  }

  /* Get the branch and delay slot OP from the GOTO BB and make sure
   * the delay slot OP doesn't write to $ra.
   */
  br_op = BB_branch_op(bp);
  br_delay_op = NULL;
  if (PROC_has_branch_delay_slot()) {
    BOOL fill_delay = (current_flags & CFLOW_FILL_DELAY_SLOTS) != 0;
    RID *rid = BB_rid(bp);
    BOOL region_is_scheduled = rid && RID_level(rid) >= RL_CGSCHED;
    if ( fill_delay || region_is_scheduled ) {
      br_delay_op = br_op ? OP_next(br_op) : BB_last_op(bp);
      
      // #681555: Don't put instructions which expand into more than 1
      // instruction in delay slot.
      if (!Is_Delay_Slot_Op(br_op, br_delay_op)) return FALSE;
      
      if (br_delay_op) {
	if (OP_Defs_Reg(br_delay_op, REGISTER_CLASS_ra, REGISTER_ra)) {
	  br_delay_op = NULL;
	} else if (fill_delay && br_delay_op == BB_last_op(bp)) {

	  /* Workaround for pv661427: delay slot filling may have
	   * caused the delay slot OP to be in the BB following a
	   * a branch. If we were to move that OP into the delay slot
	   * of the return, then effectively the branch would end up
	   * with a return in its delay slot. The fix here is to detect
	   * that condition and force the return's delay slot to be
	   * filled with a nop.
	   */
	  BB *prev = BB_prev(bp);
	  if (prev && Falls_Thru(prev, bp)) {
	    OP *last_op = BB_last_op(prev);
	    if (last_op && last_op == BB_branch_op(prev)) {
	      br_delay_op = NULL;
	    }
	  }
	}
      }
      if ( br_delay_op == NULL ) {
	OPS ops = OPS_EMPTY;
	Exp_Noop(&ops);
	BB_Append_Ops(bp, &ops);
	br_delay_op = BB_last_op(bp);
      }
    }
  }

  /* Everything looks OK -- do the transformation.
   */
  new_op = Dup_OP(rtn_op);
  if ( br_delay_op ) {
    BB_Insert_Op_Before(bp, br_delay_op, new_op);
  } else {
    BB_Append_Op(bp, new_op);
  }
  if ( br_op ) BB_Remove_Op(bp, br_op);

#ifdef TARG_IA64
  if (IPFEC_Enable_Region_Formation && RGN_Formed) {
    RGN_Unlink_Pred_Succ(bp,targ);
  } else {
    Unlink_Pred_Succ(bp, targ);
  }
#else
  Unlink_Pred_Succ(bp, targ);
#endif
  Is_True(BB_succs(bp) == NULL,
	  ("BB:%d preds/succs don't match BBINFO", BB_id(bp)));

  Set_BBINFO_nsuccs(bp, 0);
  Set_BBINFO_kind(bp, BBKIND_RETURN);

  if (BB_call(targ)) {
    FmtAssert(BB_Copy_Annotations(bp, targ, ANNOT_CALLINFO),
	      ("no CALLINFO annotations for BB:%d", BB_id(targ)));
  }

  if (BB_exit(targ)) {
    FmtAssert(BB_Copy_Annotations(bp, targ, ANNOT_EXITINFO),
	      ("no EXITINFO annotations for BB:%d", BB_id(targ)));

    ANNOTATION *ant = ANNOT_Get(BB_annotations(bp), ANNOT_EXITINFO);
    EXITINFO *exit_info = ANNOT_exitinfo(ant);
    OP *sp_adj = EXITINFO_sp_adj(exit_info);
    FmtAssert(sp_adj == NULL, ("bypassing sp-adjust in BB:%d", BB_id(bp)));

    Exit_BB_Head = BB_LIST_Push(bp, Exit_BB_Head, &MEM_pu_pool);
  }

  /* Adjust frequencies, since we no longer GOTO the target RETURN block,
   * but rather return directly.
   */
  if (freqs_computed || (BB_freq_fb_based(bp) && BB_freq_fb_based(targ))) {
    double targ_freq = BB_freq(targ);
    double bb_freq = BB_freq(bp);
    double new_targ_freq = targ_freq - bb_freq;

    if (new_targ_freq < 0.0F) {
      if (    CG_warn_bad_freqs 
	  && !BB_freq_fb_based(bp) 
	  && !BB_freq_fb_based(targ)
	  && !FREQ_Match(targ_freq, bb_freq)
      ) {
	DevWarn("cflow (Convert_Goto_To_Return) found inconsistent freqs:\n"
		"\tmost likely, something before cflow is broken.\n"
		"\tcflow will cope but please submit a pv");
      }
      new_targ_freq = 0.0F;
    }
    BB_freq(targ) = new_targ_freq;
  }

  if ( CFLOW_Trace_Branch ) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "changing BB:%d from GOTO-BB:%d+%lld to RETURN\n",
		   BB_id(bp),
		   BB_id(targ),
		   offset);
  }

  return TRUE;
}


/* ====================================================================
 *
 * Optimize_Branches
 *
 * 1) Get rid of branches to empty GOTO BBs by branching to their
 *    targets instead.
 * 2) Convert LOGIF BBs with constant conditions or only one target to 
 *    GOTO BBs.
 * 3) Optimize branches (GOTOs) to conditional branches (LOGIFs) by
 *    replacing the unconditional branch with the conditional branch.
 *
 * The return value indicates if we made any changes.
 *
 * ====================================================================
 */
static BOOL
Optimize_Branches(void)
{
  INT pass;
  mBOOL *used_branch_around;
  float edge_freq = 0.0;
  BOOL changed;

#ifdef TARG_IA64
  //-----------------------------------------------------------------
  // Indicate whether change succ success.This is because of region
  // formation.It is initialized to FALSE.
  //-----------------------------------------------------------------
  BOOL chan_succ = FALSE;
#endif
 
  used_branch_around = (mBOOL *)alloca((PU_BB_Count + 2) * sizeof(*used_branch_around));
  BZERO(used_branch_around, (PU_BB_Count + 2) * sizeof(*used_branch_around));
  pass = 0;
  do {
    BB *bp;
    changed = FALSE;
    pass++;
    for (bp = REGION_First_BB; bp; bp = BB_next(bp)) {

#ifdef TARG_IA64
        if (IPFEC_Enable_Region_Formation && RGN_Formed) {
            if(Home_Region(bp)->Is_No_Further_Opt())
                continue;
        }
#endif
   
      BB *old_tgt, *new_tgt;
      ST *st;
      INT i;
      RID *rid = BB_rid(bp);
#ifdef TARG_IA64
      BOOL flag=FALSE;
#endif
     /* Avoid modifying any block in a region that has already
      * been scheduled.
      */
     if (rid && RID_level(rid) >= RL_CGSCHED) continue;

      switch (BBINFO_kind(bp)) {
      case BBKIND_LOGIF:
        if (Convert_If_To_Goto(bp)) {
          old_tgt = BBINFO_succ_bb(bp, 0);
          new_tgt = Collapse_Empty_Goto(bp, old_tgt, BB_freq(bp));
          if (new_tgt != old_tgt) {
#ifdef TARG_IA64
            chan_succ = Cflow_Change_Succ(bp, 0, old_tgt, new_tgt);
	  }
          changed = TRUE;          
        } else {
          if (freqs_computed) {
            edge_freq = BBINFO_succ_prob(bp, 0) * BB_freq(bp);
          }
          old_tgt = BBINFO_succ_bb(bp, 0);
          if (IPFEC_Enable_Region_Formation && RGN_Formed) {
            REGIONAL_CFG_NODE *tgt_node=Regional_Cfg_Node(old_tgt); 
            if((Home_Region(old_tgt)->Region_Type()==LOOP) && 
               (tgt_node->Succ_Num() == 0)) {
              flag=TRUE;
            }
          }
          if (!flag){ 
            new_tgt = Collapse_Same_Logif(bp, old_tgt, 0, edge_freq);
            if (new_tgt != old_tgt) {
              chan_succ = Cflow_Change_Succ(bp, 0, old_tgt, new_tgt);
              if (chan_succ) {
                changed = TRUE;
              } else {
                BB_freq(old_tgt) = edge_freq;
              }
            }
          }

          flag=FALSE;  
          if (freqs_computed) {
            edge_freq = BBINFO_succ_prob(bp, 1) * BB_freq(bp);
          }
          old_tgt = BBINFO_succ_bb(bp, 1);
          if (IPFEC_Enable_Region_Formation && RGN_Formed) {
            REGIONAL_CFG_NODE *tgt_node=Regional_Cfg_Node(old_tgt); 
            if((Home_Region(old_tgt)->Region_Type()==LOOP) && 
               (tgt_node->Succ_Num() == 0)) {
              flag=TRUE;
            }
          }
          if (!flag){
            new_tgt = Collapse_Same_Logif(bp, old_tgt, 1, edge_freq);
            if (new_tgt != old_tgt) {
              chan_succ = Cflow_Change_Succ(bp, 1, old_tgt, new_tgt);
              if (chan_succ) {
                changed = TRUE;
              } else {
                BB_freq(old_tgt) = edge_freq;
              }
            }
          }
#else
	    Cflow_Change_Succ(bp, 0, old_tgt, new_tgt);
	  }
	  changed = TRUE;
        } else {
	  if (freqs_computed) {
	    edge_freq = BBINFO_succ_prob(bp, 0) * BB_freq(bp);
	  }
	  old_tgt = BBINFO_succ_bb(bp, 0);
	  new_tgt = Collapse_Same_Logif(bp, old_tgt, 0, edge_freq);
	  if (new_tgt != old_tgt) {
	    changed = TRUE;
	    Cflow_Change_Succ(bp, 0, old_tgt, new_tgt);
	  }

	  if (freqs_computed) {
	    edge_freq = BBINFO_succ_prob(bp, 1) * BB_freq(bp);
	  }
	  old_tgt = BBINFO_succ_bb(bp, 1);
	  new_tgt = Collapse_Same_Logif(bp, old_tgt, 1, edge_freq);
	  if (new_tgt != old_tgt) {
	    changed = TRUE;
	    Cflow_Change_Succ(bp, 1, old_tgt, new_tgt);
	  }
#endif // TARG_IA64
#ifdef KEY
	  /* If both false branch and true branch target to the
	     same bb, then it is not a LOGIF bb any more. (bug#3515)
	  */
	  if( BBINFO_succ_bb(bp, 0) == BBINFO_succ_bb(bp, 1) ){
	    if( !Convert_Indirect_Goto_To_Direct( bp ) ){
	      FmtAssert( FALSE, ("Optimize_Branches: fail to merge") );
	    }
	  }
#endif
	}
	break;
#if defined(TARG_SL)
      case BBKIND_ZDL_BODY:
        if (freqs_computed) {
          edge_freq = BBINFO_succ_prob(bp, 0) * BB_freq(bp);
        }
        old_tgt = BBINFO_succ_bb(bp, 0);
        new_tgt = Collapse_Empty_Goto(bp, old_tgt, edge_freq);
        if (new_tgt != old_tgt) {
          changed = TRUE;
          Cflow_Change_Succ(bp, 0, old_tgt, new_tgt);
        }
 
        if (freqs_computed) {
	  edge_freq = BBINFO_succ_prob(bp, 1) * BB_freq(bp);
	}
	old_tgt = BBINFO_succ_bb(bp, 1);
        new_tgt = Collapse_Empty_Goto(bp, old_tgt, edge_freq);
	if (new_tgt != old_tgt) {
	  changed = TRUE;
	  Cflow_Change_Succ(bp, 1, old_tgt, new_tgt);
	}
        break;
#endif

      case BBKIND_GOTO:
	if (BBINFO_nsuccs(bp) == 0) break;

	old_tgt = BBINFO_succ_bb(bp, 0);
	new_tgt = Collapse_Empty_Goto(bp, old_tgt, BB_freq(bp));
	if (new_tgt != old_tgt) {
#ifdef TARG_IA64
	  chan_succ = Cflow_Change_Succ(bp, 0, old_tgt, new_tgt);
	  if (chan_succ) changed = TRUE;
#else
	  changed = TRUE; 
	  Cflow_Change_Succ(bp, 0, old_tgt, new_tgt);
#endif
	}
        if (new_tgt != BB_next(bp)) {
          if (   Convert_Goto_To_If(bp, used_branch_around)
	      || Convert_Goto_To_Return(bp)
          ) changed = TRUE;
         
	} else if (   pass == 1 
		   && BB_branch_op(bp) 
		   && BBINFO_cold(bp) == BBINFO_cold(new_tgt))
	{

	  /* This block branches to the next BB. Say something has
	   * changed so that we guarantee that we will remove the
	   * branch (in Finalize_BB). Note that in this
	   * situation we have not modified the code, so setting
	   * 'changed' would result in an extra scan over the BBs.
	   */
	  ++pass;

	  if (CFLOW_Trace_Branch) {
	    #pragma mips_frequency_hint NEVER
	    fprintf(TFile, "removing branch to next BB from BB:%d\n", BB_id(bp));
	  }
        }
	break;
      case BBKIND_VARGOTO:
      case BBKIND_INDGOTO:
	if (Convert_Indirect_Goto_To_Direct(bp)) {
	  old_tgt = BBINFO_succ_bb(bp, 0);
	  new_tgt = Collapse_Empty_Goto(bp, old_tgt, BB_freq(bp));

	      
	  if (new_tgt != old_tgt) {
#ifdef TARG_IA64
	    chan_succ = Cflow_Change_Succ(bp, 0, old_tgt, new_tgt);
#else
	    Cflow_Change_Succ(bp, 0, old_tgt, new_tgt);
#endif
	  }
	  
	  changed = TRUE;
	  
	  break;
	}

        if (BBINFO_kind(bp) == BBKIND_VARGOTO) {
	  st = BBINFO_vargoto_listvar(bp);
	  if (st == NULL) break;
	  for (i = 0; i < BBINFO_nsuccs(bp); i++) {
	    if (freqs_computed) {
	      edge_freq = BBINFO_succ_prob(bp, i) * BB_freq(bp);
	    }
	    old_tgt = BBINFO_succ_bb(bp, i);
	    new_tgt = Collapse_Empty_Goto (bp, old_tgt, edge_freq);

	    if (new_tgt == old_tgt) continue;

	    if (!Change_Switchtable_Entries(st, old_tgt, new_tgt)) continue;
#ifdef TARG_IA64
      chan_succ = Cflow_Change_Succ(bp, i, old_tgt, new_tgt);
      if (chan_succ) changed = TRUE;
#else
      changed = TRUE;  
      Cflow_Change_Succ(bp, i, old_tgt, new_tgt);
#endif
	  }
	}
	break;
      case BBKIND_CALL:
	if (   (current_flags & CFLOW_FREQ_ORDER)
	    && freqs_computed
	    && BBINFO_nsuccs(bp)
	) {
	  old_tgt = BBINFO_succ_bb(bp, 0);
	  new_tgt = Collapse_Empty_Goto(bp, old_tgt, BB_freq(bp));

	  if (new_tgt != old_tgt) {
#ifdef TARG_IA64
	    chan_succ = Cflow_Change_Succ(bp, 0, old_tgt, new_tgt);
	    if (chan_succ) changed = TRUE;
#else
	    changed = TRUE;      
	    Cflow_Change_Succ(bp, 0, old_tgt, new_tgt);
#endif
	  }
	}
	break;
      }
    }
  } while (changed);

  return pass > 1;
}

/* ====================================================================
 * ====================================================================
 *
 * Unreachable block routines
 *
 * ====================================================================
 * ====================================================================
 */


/* ====================================================================
 *
 * Delete_Unreachable_Blocks
 *
 * Locate all unreachable BBs in the flowgraph, and remove them.
 * The return value indicates if we made any changes.
 *
 * Once again, exceptions complicate things -- care must be taken
 * with the handler and eh_range labels so that the nesting and
 * pairing of the labels is maintained.
 *
 * ====================================================================
 */
static BOOL
Delete_Unreachable_Blocks(void)
{
  struct estack_elm {			/* eh stack elements */
    BOOL unreachable;
    struct estack_elm *next;
    struct estack_elm *prev;
  };

  BB *bp;
  BB *next;
  struct estack_elm estack;		/* Base of the eh stack, used only
					 * for its pointers, not stack data.
					 */
  struct estack_elm *etos = &estack;	/* The top of stack. etos->next
					 * points to free stack elements.
					 */
  BOOL deleted = FALSE;

  /* Mark the unreachable blocks.
   */
  BB_Mark_Unreachable_Blocks();

  estack.next = NULL;
  estack.prev = NULL;

  /* Now make a pass over the BBs and get rid of any that weren't reachable.
   */
  for (bp = REGION_First_BB; bp; bp = next) {
    BOOL has_eh_lab = FALSE;
    BOOL unreachable_bb = BB_unreachable(bp);
    next = BB_next(bp);
   

    /* Special handling is necessary for exception labels. A BB with
     * the 'unreachable' flag set indicates that the code in that
     * BB is never reached/executed. However, in some cases, if exception
     * labels are present, they must be retained. In these cases we
     * can delete the contents of the BB but keep the labels.
     *
     * The eh_range labels mark a section of code that has a handler
     * for an exception if one occurs. eh_range regions are reached by
     * normal control flow and as a result, BB_Mark_Unreachable_Blocks
     * marks the BBs in the region accordingly. Therefore, an unreachable
     * begin_eh_range label may be removed. However, the unreachable
     * attribute of a BB containing an end_eh_range label DOES NOT tell us
     * that we can remove that label. To know whether or not to keep
     * an end_eh_range label, we need to know if its corresponding begin
     * label was reachable. To accomplish this, we maintain a stack of
     * begin_eh_range labels. Each time we encounter a begin label,
     * we push a flag on the stack that indicates if the BB was unreachable.
     * For each end label, we pop the stack, giving us the unreachable
     * flag for the corresponding begin label.
     */
    if (have_eh_regions && BB_has_label(bp)) {
      ANNOTATION *ant;
      ANNOTATION *next_ant;

      ant = ANNOT_First(BB_annotations(bp), ANNOT_LABEL);

#ifdef KEY
      Is_True(ant != NULL, ("Delete_Unreachable_Blocks: BB has no label"));
#endif

      do {
	LABEL_IDX lab = ANNOT_label(ant);

	next_ant = ANNOT_Next(ant, ANNOT_LABEL);

	switch (LABEL_kind(Label_Table[lab])) {
	case LKIND_BEGIN_EH_RANGE:
	  {
	    struct estack_elm *enext = etos->next;
	    if (enext == NULL) {
	      enext = (struct estack_elm *)alloca(sizeof(struct estack_elm));
	      enext->next = NULL;
	      enext->prev = etos;
	      etos->next = enext;
	    }
	    etos = enext;
	    etos->unreachable = unreachable_bb;

	    if (unreachable_bb) {
	      if (CFLOW_Trace_Unreach) {
	        #pragma mips_frequency_hint NEVER
	        fprintf(TFile, "Removing begin_eh_range label %s from BB:%d\n",
			       LABEL_name(lab), BB_id(bp));
	      }
	      BB_annotations(bp) = ANNOT_Unlink(BB_annotations(bp), ant);
	      if (ANNOT_First(BB_annotations(bp), ANNOT_LABEL) == NULL) //bug fix for OSP_358
	        Reset_BB_has_label(bp);//no label annotate, reset has_lable flag
	      Set_Label_BB(lab, NULL);
	      eh_label_removed = TRUE;
	    } else {
	      has_eh_lab = TRUE;
	    }
	  }
	  break;
	case LKIND_END_EH_RANGE:
	  {
	    BOOL unreachable_range = etos->unreachable;
	    etos = etos->prev;
	    FmtAssert(etos, ("estack underflow with label %s of BB:%d",
			     LABEL_name(lab), BB_id(bp)));
	    if (unreachable_range) {
	      if (CFLOW_Trace_Unreach) {
	        #pragma mips_frequency_hint NEVER
	        fprintf(TFile, "Removing end_eh_range label %s from BB:%d\n",
			       LABEL_name(lab), BB_id(bp));
	      }
	      BB_annotations(bp) = ANNOT_Unlink(BB_annotations(bp), ant);
	      if (ANNOT_First(BB_annotations(bp), ANNOT_LABEL) == NULL) //bug fix for OSP_358
	        Reset_BB_has_label(bp);//no label annotate, reset has_lable flag
	      Set_Label_BB(lab, NULL);
	      eh_label_removed = TRUE;
	    } else {
	      has_eh_lab = TRUE;
	    }
	  }
	  break;
	}
      } while (ant = next_ant);
    }

    /* Keep scanning if we must keep this BB.
     */
    if (!unreachable_bb) continue;
#ifdef TARG_IA64
    if (IPFEC_Enable_Speculation) {
        if (BB_Hold_Disjoint_Speculative_Code(bp)) 
            continue;
	
	Delete_Recovery_Info_For_BB(bp);
    }
#endif
    /* Delete the contents or the whole BB, depending on whether or
     * not we need to keep some labels or not.
     */
    if (   has_eh_lab 
	|| BB_Has_Addr_Taken_Label(bp)
	|| BB_Has_Outer_Block_Label(bp))
    {
      Delete_BB_Contents(bp);
    } else {
      Delete_BB(bp, CFLOW_Trace_Unreach);
    }

    deleted = TRUE;
  }

  FmtAssert(etos == &estack, ("estack not empty"));

  return deleted;
}

/* ====================================================================
 * ====================================================================
 *
 * Block merging routines
 *
 * ====================================================================
 * ====================================================================
 */


/* ====================================================================
 *
 * Merge_With_Pred
 *
 * Given an empty GOTO BB b and its predecessor pred, merge pred into b.
 *
 * ====================================================================
 */
static BOOL
Merge_With_Pred ( BB *b, BB *pred )
{
  BB *b_targ;
  RID *b_rid;
  UINT32 i;
  BB *merged_pred;

#ifdef TARG_IA64
  if (IPFEC_Enable_Region_Formation && RGN_Formed ) {
      if(Regional_Cfg_Node(pred)->Is_Entry())
          return FALSE;
      if(Home_Region(b)->Is_No_Further_Opt() ||
         Home_Region(pred)->Is_No_Further_Opt())
         return FALSE;
  }
#endif

  /* Only handle the cases that won't get handled by Merge_With_Succ
   * or by some other means.
   */
  switch (BBINFO_kind(pred)) {
  case BBKIND_GOTO:
  case BBKIND_RETURN:
  case BBKIND_TAIL_CALL:
  case BBKIND_REGION_EXIT:
  case BBKIND_LOGIF:
  case BBKIND_VARGOTO:
  case BBKIND_INDGOTO:
#ifdef TARG_IA64
  case BBKIND_CHK:// bug fix for OSP_104, OSP_105, OSP_192
#endif
#if defined(TARG_SL) 
  case BBKIND_ZDL_BODY:
  case BBKIND_FORK:
#endif
    if (CFLOW_Trace_Merge) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "Merge_With_Pred rejecting merge of BB:%d into BB:%d (unsupported kind: %s)\n",
	      BB_id(b), BB_id(pred), BBKIND_Name(BBINFO_kind(pred)));
    }
    return FALSE;

  default:
    #pragma mips_frequency_hint NEVER
    FmtAssert(FALSE, ("Merge_With_Pred: unknown BB kind (%s)", 
		      BBKIND_Name(BBINFO_kind(pred))));
    /*NOTREACHED*/

  case BBKIND_CALL:
    if (BBINFO_succ_bb(pred, 0) != BB_next(pred)) {
      if (CFLOW_Trace_Merge) {
	#pragma mips_frequency_hint NEVER
	fprintf(TFile, "Merge_With_Pred rejecting merge of BB:%d into BB:%d (call succ not next BB)\n",
		BB_id(b), BB_id(pred));
      }
      return FALSE;
    }

    /* We handle these.
     */
    break;
  }

  /* <b> must fall through to its successor.
   */
  b_targ = BBINFO_succ_bb(b, 0);
  if (b_targ != BB_next(b)) {
    if (CFLOW_Trace_Merge) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "Merge_With_Pred rejecting merge of BB:%d into BB:%d (GOTO not a fall through)\n",
		     BB_id(b), BB_id(pred));
    }
    return FALSE;
  }

  /* Reject if <b> has an exception label.
   */
  if (BB_Has_Exc_Label(b)) {
    if (CFLOW_Trace_Merge) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "Merge_With_Pred rejecting merge of BB:%d into BB:%d (sucessor has exception label)\n",
		     BB_id(b), BB_id(pred));
    }
    return FALSE;
  }

  /* Reject if <b> has an address-taken label.
   */
  if (BB_Has_Addr_Taken_Label(b)) {
    if (CFLOW_Trace_Merge) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "Merge_With_Pred rejecting merge of BB:%d into BB:%d (sucessor has address-taken label)\n",
		     BB_id(b), BB_id(pred));
    }
    return FALSE;
  }

  /* Reject if <b> has an outer_block label.
   */
  if (BB_Has_Outer_Block_Label(b)) {
    if (CFLOW_Trace_Merge) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "Merge_With_Pred rejecting merge of BB:%d into BB:%d (sucessor is target of outer block goto)\n",
		     BB_id(b), BB_id(pred));
    }
    return FALSE;
  }

  /* Reject if <b> has a pragma.
   */
  if (BB_has_pragma(b) && Pragma_Affects_Cflow(b)) {
    if (CFLOW_Trace_Merge) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "Merge_With_Pred rejecting merge of BB:%d into BB:%d (pragma found)\n",
		     BB_id(b), BB_id(pred));
    }
    return FALSE;
  }

  /* Reject if crossing region boundaries.
   */
  b_rid = BB_rid(b);
  if (b_rid != BB_rid(pred)) {
    if (CFLOW_Trace_Merge) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "Merge_With_Pred rejecting merge of BB:%d into BB:%d (different regions)\n",
		     BB_id(b), BB_id(pred));
    }
    return FALSE;
  }

  /* GRA cannot handle a single block being the entry and exit block
   * of a region. Reject a merge that will create that case.
   */
  merged_pred = BB_Unique_Predecessor(pred);
  if (   merged_pred
      && b_targ != NULL
      && BB_rid(merged_pred) != b_rid
      && BB_rid(b_targ) != b_rid
  ) {
    if (CFLOW_Trace_Merge) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "Merge_With_Pred rejecting merge of BB:%d into BB:%d (would be a region entry and exit block)\n",
		     BB_id(b), BB_id(pred));
    }
    return FALSE;
  }

  /* Reject if either block modifies the rotating registers.
   * TODO: allow this merge after GRA has run.
   */
  if (   BB_mod_pred_rotating_registers(b)
      || BB_mod_pred_rotating_registers(pred)
      || BB_mod_rotating_registers(b)
      || BB_mod_rotating_registers(pred))
  {
    if (CFLOW_Trace_Merge) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "Merge_With_Pred rejecting merge of BB:%d into BB:%d (one of the blocks modifies the rotating registers)\n",
		     BB_id(b), BB_id(pred));
    }
    return FALSE;
  }

  //
  // Hyperblock checks
  //
  if (!HB_CFLOW_Can_Merge_BBs(b,pred)) {
    return FALSE;
  }

  /* Copy love notes to the GOTO target.
   */
  BB_Copy_Annotations(b_targ, b, ANNOT_NOTE);

  /* Copy pragmas to the pred.
   */
  BB_Copy_Annotations(pred, b, ANNOT_PRAGMA);
  BB_Copy_Annotations(pred, b, ANNOT_INLINE);

  /* Update BB successor info if necessary.
   */
#ifdef TARG_IA64
  if (IPFEC_Enable_Region_Formation && RGN_Formed ) {
    RGN_Unlink_Pred_Succ(pred,b);
  } else {
    Unlink_Pred_Succ(pred, b);
  }
#else
  Unlink_Pred_Succ(pred, b);
#endif
  for (i = 0; i < BBINFO_nsuccs(pred); ++i) {
    if (BBINFO_succ_bb(pred, i) == b) {

#if defined(TARG_SL)
  if(BB_zdl_prolog(b))
    Set_BB_zdl_prolog(pred);
#endif

#ifdef TARG_IA64
	  if (IPFEC_Enable_Region_Formation && RGN_Formed) {
        RGN_Link_Pred_Succ_With_Prob(pred, b_targ, BBINFO_succ_prob(pred, i));
      } else {
        Link_Pred_Succ_with_Prob(pred, b_targ, BBINFO_succ_prob(pred, i));
      }
#else
      Link_Pred_Succ_with_Prob(pred, b_targ, BBINFO_succ_prob(pred, i));
#endif


      Set_BBINFO_succ_bb(pred, i, b_targ);
    }
  }

  if (CFLOW_Trace_Merge) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "Merge_With_Pred merged BB:%d into BB:%d\n", BB_id(b), BB_id(pred));
  }

  /* We succeeded in merging <b> into <pred>. We can delete <b>.
   */
  HB_CFLOW_Replace_Block(b,pred);
  Delete_BB(b, CFLOW_Trace_Merge);

  return TRUE;
}


/* ====================================================================
 *
 * Can_Append_Succ
 *
 * If possible, append BB <suc> to BB <b>. The returned boolean
 * indicates if <suc> was appended. If <delete_suc> is TRUE,
 * <suc> is deleted after the append, otherwise it is kept.
 *
 * ====================================================================
 */
static BOOL
Can_Append_Succ(
  BB *b, 
  BB *suc, 
  BB *merged_pred, 
  BOOL delete_suc,
  BOOL trace)
{
  BB *merged_succ;
  RID *b_rid = BB_rid(b);
  const char *oper_name = delete_suc ? "merge" : "append";
  INT nsuccs = BBINFO_nsuccs(suc);

  /* Reject if we don't know how to deal with <suc>'s kind.
   */
  switch (BBINFO_kind(suc)) {
  case BBKIND_CALL:

    /* If <b> transfered control to a BB that was not the next BB, then
     * we have no way of indicating that after the merge (because a call
     * block always falls through). We either need to insert a new GOTO
     * block or modify the BBINFO for calls to allow an arbitrary successor
     * for the fall through like LOGIF does.
     */
    if (nsuccs && suc != BB_next(b)) {
      if (trace) {
 	#pragma mips_frequency_hint NEVER
	fprintf(TFile, "rejecting %s of BB:%d into BB:%d"
		       " (BBKIND_CALL pred must be fall through)\n",
		       oper_name, BB_id(suc), BB_id(b));
      }
      return FALSE;
    }
    /*FALLTHROUGH*/
  case BBKIND_GOTO:
  case BBKIND_LOGIF:
  case BBKIND_RETURN:
  case BBKIND_TAIL_CALL:
  case BBKIND_REGION_EXIT:
  case BBKIND_VARGOTO:
  case BBKIND_INDGOTO:

    /* We handle these.
     */
    break;
#if defined(TARG_SL) 
  case BBKIND_ZDL_BODY:
    {
      if (trace) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile, "rejecting %s of BB:%d into BB:%d"
		       " (BBKIND_ZDL must not be appended)\n",
		       oper_name, BB_id(suc), BB_id(b));
      }
      return FALSE;
    }
  case BBKIND_FORK:
    {
      if (trace) {
 	  #pragma mips_frequency_hint NEVER
	  fprintf(TFile, "rejecting %s of BB:%d into BB:%d"
		       " (BBKIND_FORL can not be appended currently)\n",
		       oper_name, BB_id(suc), BB_id(b));
      }
      return FALSE;
    }
#endif
  default:
    if (trace) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "rejecting %s of BB:%d into BB:%d (unsupported kind: %s)",
	      oper_name, BB_id(suc), BB_id(b), BBKIND_Name(BBINFO_kind(suc)));
    }
    return FALSE;
  }

#if defined(TARG_SL)
  /* if <suc> is loop-prolog-bb, and <suc>'s last op is TOP_loop, 
   * then <b> must fall through to <suc>;
   * otherwise, after we merge <suc> into <b>, the new loop-prolog-bb is <b>,
   * and <b> does not fall through to loop-head-bb, so, there will be a TOP_jp after TOP_loop.
   */
  if(BB_next(b) != suc && BB_last_op(suc) && OP_code(BB_last_op(suc)) == TOP_loop) {
    #pragma mips_frequency_hint NEVER
    if (trace) {
      fprintf(TFile, "rejecting %s of BB:%d into BB:%d"
         " (loop-head-BB:%d can NOT be %s into BB:%d which does NOT fall through to it)\n",
         oper_name, BB_id(suc), BB_id(b), BB_id(suc), oper_name, BB_id(b));
    }
    return FALSE;
  }

  if(BBINFO_kind(b) == BBKIND_ZDL_BODY) {
    if (trace) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "rejecting %s of BB:%d into BB:%d"
		       " (BBKIND_ZDL must not append other bb)\n",
		       oper_name, BB_id(suc), BB_id(b));
      }
    return FALSE;
  }

  if(BBINFO_kind(b) == BBKIND_FORK) {
    if(trace) {
      #pragma mips_frequency_hint NEVER
	fprintf(TFile, "rejecting %s of BB:%d into BB:%d"
		       " (BBKIND_FORL can not append other bb currently)\n",
		       oper_name, BB_id(suc), BB_id(b));
    }
    return FALSE;
  }
#endif

  /* Reject if BB has an asm.
   */
  if (BB_asm(b)) {
    if (trace) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "rejecting %s of BB:%d into BB:%d"
		     " (predecessor has an asm)\n",
		     oper_name, BB_id(suc), BB_id(b));
    }
    return FALSE;
  }

#ifdef TARG_X8664
  // merging would delete the label needed by this instr
  if( BB_savexmms_op(b) != NULL ){
    return FALSE;
  }

  if (BB_last_OP_computes_got(b) ||		// bug 14452
      BB_first_OP_computes_got(suc)) {
    return FALSE;
  }
#endif

  /* Reject if suc is an entry point.
   */
  if (BB_entry(suc)) {
    if (trace) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "rejecting %s of BB:%d into BB:%d"
		     " (successor is an entry point)\n",
		     oper_name, BB_id(suc), BB_id(b));
    }
    return FALSE;
  }

  /* Reject if suc has an exception label.
   */
  if (BB_Has_Exc_Label(suc)) {
    if (trace) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "rejecting %s of BB:%d into BB:%d"
		     " (successor has exception label)\n",
		     oper_name, BB_id(suc), BB_id(b));
    }
    return FALSE;
  }

  /* Reject if suc has an address-taken label.
   */
  if (BB_Has_Addr_Taken_Label(suc)) {
    if (trace) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "rejecting %s of BB:%d into BB:%d"
		     " (successor has address-taken label)\n",
		     oper_name, BB_id(suc), BB_id(b));
    }
    return FALSE;
  }

  /* Reject if suc has an outer-block label.
   */
  if (BB_Has_Outer_Block_Label(suc)) {
    if (trace) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "rejecting %s of BB:%d into BB:%d"
		     " (successor is target of outer block goto)\n",
		     oper_name, BB_id(suc), BB_id(b));
    }
    return FALSE;
  }

  /* Reject if either BB has a pragma.
   */
  if (  (BB_has_pragma(b) && Pragma_Affects_Cflow(b)) 
      || (BB_has_pragma(suc) && Pragma_Affects_Cflow(suc))
  ) {
    if (trace) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "rejecting %s of BB:%d into BB:%d (pragmas found)\n",
		     oper_name, BB_id(suc), BB_id(b));
    }
    return FALSE;
  }

  /* Reject if crossing region boundaries.
   */
  if (b_rid != BB_rid(suc)) {
    if (trace) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "rejecting %s of BB:%d into BB:%d (different regions)\n",
		     oper_name, BB_id(suc), BB_id(b));
    }
    return FALSE;
  }

  /* Reject if removing the succ would cause the RID of the region's
   * first BB to change.
   */
  if (delete_suc && suc == REGION_First_BB) {
    BB *suc_next = BB_next(suc);
    if (suc_next && BB_rid(suc) != BB_rid(suc_next)) {
      if (trace) {
	#pragma mips_frequency_hint NEVER
	fprintf(TFile, "rejecting %s of BB:%d into BB:%d (would change RID for REGION_First_BB)\n",
		     oper_name, BB_id(suc), BB_id(b));
      }
    return FALSE;
    }
  }

  /* Reject if the region has been scheduled.
   */
  if (b_rid && RID_level(b_rid) >= RL_CGSCHED) {
    if (trace) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "rejecting %s of BB:%d into BB:%d (scheduled region)\n",
		     oper_name, BB_id(suc), BB_id(b));
    }
    return FALSE;
  }

  /* Reject if the region has been register allocated.
   */
  if (b_rid && RID_has_reg_alloc(b_rid)) {
    if (trace) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "rejecting %s of BB:%d into BB:%d (reg alloced region)\n",
		     oper_name, BB_id(suc), BB_id(b));
    }
    return FALSE;
  }

  /* Reject if merged BB will be too large.
   */
  if (BB_length(b) + BB_length(suc) >= Split_BB_Length) {
    if (trace) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "rejecting %s of BB:%d into BB:%d"
		     " (combined size too large)\n",
		     oper_name, BB_id(suc), BB_id(b));
    }
    return FALSE;
  }

  /* Reject if either BB is from an unrolled loop and the other BB
   * isn't in the same unrolled loop. This can occur with multi-BB
   * unrolling, or fully unrolled single-BB loops. We don't want to
   * merge these BBs since mixing loop memory refs seems likely
   * to violate dependence info assumptions from LNO/WOPT.
   */
  if (BB_unrollings(b) || BB_unrollings(suc)) {
    if (   !BB_unrollings(b)
	|| !BB_unrollings(suc)
	|| BB_loop_head_bb(b) != BB_loop_head_bb(suc)
    ) {
      if (trace) {
	#pragma mips_frequency_hint NEVER
	fprintf(TFile, "rejecting %s of BB:%d into BB:%d "
	        "(not in same unrolled loop)\n",
	        oper_name, BB_id(suc), BB_id(b));
      }
      return FALSE;
    }
  }

  /* GRA cannot handle a single block being the entry and exit block
   * of a region. Reject a merge that will create that case.
   */
  merged_succ = (nsuccs == 1) ? BBINFO_succ_bb(suc, 0) : NULL;
  if (   merged_pred != NULL
      && merged_succ != NULL
      && BB_rid(merged_pred) != b_rid
      && BB_rid(merged_succ) != b_rid)
  {
    if (trace) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "rejecting %s of BB:%d into BB:%d (would be a region entry and exit block)\n",
		     oper_name, BB_id(suc), BB_id(b));
    }
    return FALSE;
  }

  /* Reject if either block modifies the rotating registers.
   * TODO: allow this merge after GRA has run.
   */
  if (   BB_mod_pred_rotating_registers(b)
      || BB_mod_pred_rotating_registers(suc)
      || BB_mod_rotating_registers(b)
      || BB_mod_rotating_registers(suc))
  {
    if (CFLOW_Trace_Merge) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "rejecting %s of BB:%d into BB:%d (one of the blocks modifies the rotating registers)\n",
		     oper_name, BB_id(suc), BB_id(b));
    }
    return FALSE;
  }

  //
  // Various hyperblock checks
  //
  if (!HB_CFLOW_Can_Merge_BBs(b,suc)) {
    return FALSE;
  }

  return TRUE;
}




/* ====================================================================
 *
 * Append_Succ
 *
 * Append BB <suc> to BB <b>. If <delete_suc> is TRUE,
 * <suc> is deleted after the append, otherwise it is kept.
 *
 * ====================================================================
 */
static void
Append_Succ(
  BB *b, 
  BB *suc, 
  BOOL in_cgprep, 
  BOOL delete_suc)
{
  INT num;
  INT i;
  RID *b_rid = BB_rid(b);
  INT nsuccs = BBINFO_nsuccs(suc);
#ifdef KEY
  OP *first_new_op = NULL;
#endif

  /* If the block we're merging into ended in a branch, remove it.
   */
  BB_Remove_Branch(b);

  /* Move the OPs from suc into b
   */
  if (delete_suc) {
    BB_Append_All(b, suc);
    HB_CFLOW_Replace_Block(suc,b);
  } else {
    OP *op;
    FOR_ALL_BB_OPs_FWD(suc, op) {
      OP *new_op = Dup_OP(op);
      if (OP_memory(op)) Copy_WN_For_Memory_OP(new_op, op);
      BB_Append_Op(b, new_op);
#ifdef KEY
      if (first_new_op == NULL)
	first_new_op = new_op;
#endif

      // After an OP is duplicated, its TN becomes GTN.
      if (!CG_localize_tns) {
	for (INT i = 0; i < OP_results(new_op); ++i) {
	  TN *tn = OP_result(new_op, i);
	  if (TN_is_constant(tn)) continue;
	  if (TN_is_const_reg(tn)) continue;
	  if (!TN_is_global_reg(tn))
	    GTN_UNIVERSE_Add_TN(tn);
	}
      }
    }
  }
  if (!CG_localize_tns) {
    GRA_LIVE_Merge_Blocks(b, b, suc);
    Rename_TNs_For_BB(b, NULL);  // why only two argument, but three parameter.
  }
#ifndef TARG_IA64
  else {
    // Rename duplicated local TNs.  Otherwise, those (non-GTN) TNs would
    // appear in <suc> as well as in the merged BB, causing LRA's
    // Consistency_Check to complain.  Bug 4327.
    Rename_TNs_For_BB(b, NULL, first_new_op);
  }
#endif

  /* Merge the ending of suc into b
   */
  Set_BBINFO_kind(b, BBINFO_kind(suc));
  Set_BBINFO_nsuccs(b, nsuccs);
  switch (BBINFO_kind(suc)) {
    case BBKIND_TAIL_CALL:
    case BBKIND_CALL:
    case BBKIND_RETURN:
      if (BB_call(suc)) {
	FmtAssert(BB_Copy_Annotations(b, suc, ANNOT_CALLINFO),
		  ("no CALLINFO annotations for BB:%d", BB_id(suc)));
      }
      if (BB_exit(suc)) {
	FmtAssert(BB_Copy_Annotations(b, suc, ANNOT_EXITINFO),
		 ("no EXITINFO annotations for BB:%d", BB_id(suc)));

	if (!delete_suc) {
	  OP *b_op;
	  OP *suc_op;
	  ANNOTATION *ant = ANNOT_Get(BB_annotations(b), ANNOT_EXITINFO);
	  EXITINFO *exit_info = ANNOT_exitinfo(ant);
	  EXITINFO *new_info = TYPE_PU_ALLOC(EXITINFO);
	  OP *sp_adj = EXITINFO_sp_adj(exit_info);
          *new_info = *exit_info;
	  if (sp_adj) {
	    for (suc_op = BB_last_op(suc), b_op = BB_last_op(b);
	         suc_op != sp_adj;
	         suc_op = OP_prev(suc_op), b_op = OP_prev(b_op))
	      ;
	    EXITINFO_sp_adj(new_info) = b_op;
	  }
	  ant->info = new_info;
	}

	/* The merged BB (b) will now be an exit BB, add it to the list of
	 * exit BBs (Delete_BB will remove the old exit BB (suc) from the
	 * list.
	 */
	Exit_BB_Head = BB_LIST_Push(b, Exit_BB_Head, &MEM_pu_pool);
      }
      break;
    case BBKIND_GOTO:
      Is_True(delete_suc, ("keeping succ not handled"));
      if (BB_asm(suc)) {
	FmtAssert(BB_Copy_Annotations(b, suc, ANNOT_ASMINFO),
		  ("no ASMINFO annotations for BB:%d", BB_id(suc)));
      }
      break;
    case BBKIND_REGION_EXIT:
      /* nothing to do */
      Is_True(delete_suc, ("keeping succ not handled"));
      break;
    case BBKIND_LOGIF:
      Set_BBINFO_condval1(b, BBINFO_condval1(suc));
      Set_BBINFO_condval2(b, BBINFO_condval2(suc));
      Set_BBINFO_compare_op(b, BBINFO_compare_op(suc));
      if (!delete_suc) {
	OP *cmp_op = BBINFO_compare_op(suc);
	if (OP_bb(cmp_op) == suc) {
	  OP *b_op, *suc_op;
	  for (suc_op = BB_last_op(suc), b_op = BB_last_op(b);
	       suc_op != cmp_op;
	       suc_op = OP_prev(suc_op), b_op = OP_prev(b_op))
	    ;
	  Set_BBINFO_compare_op(b, b_op);
	}
      }
      Set_BBINFO_variant(b, BBINFO_variant(suc));
      Set_BBINFO_b_likely(b, BBINFO_b_likely(suc));
      break;
    case BBKIND_VARGOTO:
      {
	INT incr;
	ST *listvar;
	INT *refcount;

	incr = (nsuccs - BBINFO_NSUCCS) * sizeof(struct succedge);
	if (incr > 0) {
	  INT bbinfo_size = sizeof(BBINFO) + incr;
	  BBINFO *new_bbinfo = (BBINFO *) MEM_POOL_Alloc(&MEM_local_nz_pool, bbinfo_size);
	  *new_bbinfo = *BB_BBINFO(b);
	  BB_MAP_Set(bb_info_map, b, new_bbinfo);
	}

	refcount = BBINFO_vargoto_refcount(suc);
	listvar = BBINFO_vargoto_listvar(suc);
	if (listvar) {
	  FmtAssert(BB_Copy_Annotations(b, suc, ANNOT_SWITCH),
		   ("no switch annotation for BB:%d", BB_id(suc)));

	  /* We always share the jump table when we clone a VARGOTO
	   * block. The assumption is that any changes we'd make
	   * to the jump table would be valid for all uses.
	   * But we do have to track how many times we're using
	   * the table so we know when we can get rid of it.
	   */
	  ++(*refcount);
	}
	Set_BBINFO_vargoto_listvar(b, listvar);
	Set_BBINFO_vargoto_refcount(b, refcount);
      }
      break;
    case BBKIND_INDGOTO:
      {
	INT incr = (nsuccs - BBINFO_NSUCCS) * sizeof(struct succedge);
	if (incr > 0) {
	  INT bbinfo_size = sizeof(BBINFO) + incr;
	  BBINFO *new_bbinfo = (BBINFO *) MEM_POOL_Alloc(&MEM_local_nz_pool, bbinfo_size);
	  *new_bbinfo = *BB_BBINFO(b);
	  BB_MAP_Set(bb_info_map, b, new_bbinfo);
	}
      }
      break;
    default:
      #pragma mips_frequency_hint NEVER
      FmtAssert(FALSE, ("Append_Succ"));
      /*NOTREACHED*/
  }

  // Decrease the successor's block frequency
  if (!delete_suc) {
    BBLIST *suc_lst = BBlist_Find_BB(BB_succs(b), suc);
    Is_True(suc_lst, ("Append_Succ: suc not successor of bb"));
    float freq_edge = BBLIST_prob(suc_lst) * BB_freq(b);
    BB_freq(suc) -= freq_edge;
  }

#ifdef TARG_IA64
  if (IPFEC_Enable_Region_Formation && RGN_Formed) {
    RGN_Unlink_Pred_Succ(b,suc);
  } else {
    Unlink_Pred_Succ(b, suc);
  }
#else
  Unlink_Pred_Succ(b, suc);
#endif
  for (i = 0; i < nsuccs; ++i) {
    BB *succ_i = BBINFO_succ_bb(suc, i);
    float prob_i = BBINFO_succ_prob(suc, i);

#ifdef TARG_IA64
	if (IPFEC_Enable_Region_Formation && RGN_Formed)	{
	  RGN_Link_Pred_Succ_With_Prob(b,succ_i,prob_i);
	} else {
      Link_Pred_Succ_with_Prob(b, succ_i, prob_i);
    }
#else
	Link_Pred_Succ_with_Prob(b, succ_i, prob_i);
#endif

    Set_BBINFO_succ_bb(b, i, succ_i);
    Set_BBINFO_succ_prob(b, i, prob_i);
    Set_BBINFO_succ_offset(b, i, BBINFO_succ_offset(suc, i));
  }

  BB_branch_wn(b) = BB_branch_wn(suc);

  BB_Copy_Annotations(b, suc, ANNOT_NOTE);
  BB_Copy_Annotations(b, suc, ANNOT_PRAGMA);
  BB_Copy_Annotations(b, suc, ANNOT_INLINE);

  if ((num = BB_REGION_Exit(suc, b_rid)) != NO_REGION_EXIT) {

    /* is exit 'num', so make b be that exit now 
     */
    CGRIN_exit_i(RID_cginfo(b_rid), num) = b;
  }
  if (BB_REGION_Entry(suc, b_rid)) {

    /* is region entry, so make b be that entry now 
     */
    CGRIN_entry(RID_cginfo(b_rid)) = b;
  }
}


/* ====================================================================
 *
 * Merge_With_Succ
 *
 * Given a BB b and its successor suc, merge suc into b.
 *
 * ====================================================================
 */
static BOOL
Merge_With_Succ(BB *b, BB *suc, BB *merged_pred, BOOL in_cgprep)
{

#ifdef TARG_IA64
  if (IPFEC_Enable_Region_Formation && RGN_Formed ) {
      if(Regional_Cfg_Node(suc)->Is_Entry())
          return FALSE;
      if(Home_Region(b)->Is_No_Further_Opt() ||
         Home_Region(suc)->Is_No_Further_Opt())
         return FALSE;
  }
#endif
  /* Try to append <suc> to <b>.
   */
  if (!Can_Append_Succ(b, suc, merged_pred, TRUE, CFLOW_Trace_Merge)) {
    return FALSE;
  }

#if defined(TARG_SL)
  if(BB_zdl_prolog(suc))
    Set_BB_zdl_prolog(b);
#endif

  Append_Succ(b, suc, in_cgprep, TRUE);
  if (CFLOW_Trace_Merge) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "Merge_With_Succ merged BB:%d into BB:%d\n", BB_id(suc), BB_id(b));
  }

  /* We succeeded in merging <suc> into <b>. We can delete <suc>.
   */
  Delete_BB(suc, CFLOW_Trace_Merge);

  return TRUE;
}


/* ====================================================================
 *
 * Merge_Blocks
 *
 * Attempt to merge BBs -- eliminate unconditional GOTOs by merging the
 * target into its predecessor.
 *
 * The return value indicates if we made any changes.
 *
 * ====================================================================
 */
static BOOL
Merge_Blocks ( BOOL in_cgprep )
{
  BB *next_b;
  BB *b;
  BB *suc;
  BOOL merged = FALSE;

  /* Now merge BBs...
   */
  for (b = REGION_First_BB; b; b = next_b) {
    next_b = BB_next(b);

    /* Loop as long as we can merge successors of <b> into <b>.
     */
    for (;;) {
      BB *pred;

      /* If <b> doesn't GOTO the start of a known block, can't merge.
       */
      if (   BBINFO_kind(b) != BBKIND_GOTO 
	  || BBINFO_nsuccs(b) == 0
	  || BBINFO_succ_offset(b, 0) != 0) break;

      /* If the successor is <b> then can't merge it
       */
      suc = BBINFO_succ_bb(b, 0);
      if (suc == b) break;

#ifdef TARG_IA64
      /* We don't need not to merge profile added BB.
       * This modification originated from a bug caused by 
       * split such a merged. The result is incorrect 
       * live information and error register allocation.
       */
      if (BB_profile_added(b)) break;
#endif      
      pred = BB_Unique_Predecessor(b);

      /* If <suc> has a unique predecessor (it would have to be <b>),
       * we might be able to merge with it.
       */
      if (BB_Unique_Predecessor(suc)) {

#ifdef TARG_X8664
	// merging would delete the label needed by this instr
	if( BB_savexmms_op(b) != NULL ){
	  break;
	}

	if (BB_first_OP_computes_got(b) ||
	    BB_last_OP_computes_got(b)) {	// bug 14452
	  break;
	}
#endif

	/* We have a candidate to merge with its successor.
	 * Merge them if we can and know how.
	 */
	       if (Merge_With_Succ(b, suc, pred, in_cgprep)) {
#ifdef TARG_IA64
	           Reset_BB_scheduled(b);
#endif	
         	   merged = TRUE;
	           next_b = BB_next(b);
               continue;
	       }
     }

      /* We failed to merge <b> with it successor. If it's an
       * empty BB, we might be able to merge it with its predecessor.
       */
      if (pred && Is_Empty_BB(b)) {
	      if (Merge_With_Pred(b, pred)) {
#ifdef TARG_IA64
		      Reset_BB_scheduled(b);
#endif
			  merged = TRUE;
	      }
     }

      break;
    }
  }

  return merged;
}

/* ====================================================================
 * ====================================================================
 *
 * Block reordering routines
 *
 * ====================================================================
 * ====================================================================
 */


/* ====================================================================
 *
 * Print_Fall_Thrus
 *
 * Calculate and print the proportion of fall-through BBs in the active
 * list.
 *
 * ====================================================================
 */
static void
Print_Fall_Thrus(const char *msg)
{
  BB *bb;
  INT count, ft_count;

  /* Calculate the counts:
   */
  count = ft_count = 0;
  for (bb = REGION_First_BB; bb; bb = BB_next(bb)) {
    ++count;
    if (Falls_Thru(bb, NULL)) ++ft_count;
  }

  /* Report the information:
   */
  fprintf(TFile, "Fall-through BBs: %d of %d %s\n",
		  ft_count, count, msg);
}


/* ====================================================================
 *
 * Reorder_Blocks
 *
 * Search for cases where no immediate control flow successor BBj of
 * BBi (GOTO or LOGIF) is its BB_next successor, but some such BBj is
 * not the control flow successor of its BB_prev predecessor BBk, i.e.:
 *
 *	BBi ->cf BBj	but not	BBi ->nx BBj
 * and
 *	BBk ->nx BBj	but not BBk ->cf BBj
 *
 * For any such cases found, move BBj so it is the BB_next successor
 * of BBi, so that (one of) BBi's target(s) is a fall-through.
 *
 * ====================================================================
 */
static BOOL
Reorder_Blocks(void)
{
  INT cnt;
  BOOL more;
  BB *BBi;
  BOOL reordered;
  BB *succs[2];
  INT nsuccs;
  INT i;

  cnt = MIN(PU_BB_Count, 25);
  reordered = FALSE;
  do {
    more = FALSE;
    for (BBi = REGION_First_BB; BBi; BBi = BB_next(BBi)) {

      /* If <BBi> already falls through, leave it alone.
       */
      if (Falls_Thru(BBi, NULL)) continue;

      /* Get the successors for candidate blocks.
       */
      nsuccs = BBINFO_nsuccs(BBi);
      switch (BBINFO_kind(BBi)) {
      case BBKIND_GOTO:
	if (nsuccs == 0) continue;
	succs[0] = BBINFO_succ_bb(BBi, 0);
	break;

      case BBKIND_LOGIF:
	succs[0] = BBINFO_succ_bb(BBi, 0);
	succs[1] = BBINFO_succ_bb(BBi, 1);
	break;

      default:

	/* No chance of reordering these.
	 */
	continue;
      }

      /* Move one of the successors if profitable.
       */
      i = 0;
      do {
	BB *BBk;
	BB *last_BBj;
	BB *BBj = succs[i];

	/* Self-branches are boring:
	 */
	if (BBj == BBi) continue;

	/* If the current predecessor of <BBj> falls through to it,
	 * don't move it:
	 */
	BBk = BB_prev(BBj);
	if (BBk && Falls_Thru(BBk, BBj)) {

	  /* Special case a branch-around: if we have one of
	   * these, moving BBj will still leave BBk with a
	   * fall through succ.
	   */
	  if (!Branches_Around(BBk)) continue;
	}

	/* If we can move BBj, then we can also move its fall
	 * through succs. Find the last block to move (and
	 * while we're at it, make sure all the blocks are in
	 * the same region):
	 */
	last_BBj = BBj;
	for (;;) {
	  if (BB_rid(last_BBj) != BB_rid(BBi)) goto next_succ;
	  if (BBINFO_eh_rgn(last_BBj) != BBINFO_eh_rgn(BBi)) goto next_succ;
	  if (!Falls_Thru(last_BBj, NULL)) break;
	  last_BBj = BB_next(last_BBj);
	}

	/* If the last block to be moved is also the block we're
	 * targetting the move to, then we have a cycle. There is
	 * nothing that can be done here...
	 */
	if (last_BBj == BBi) continue;

	/* Looks good -- move it and its fall through succs:
	 */
	more = TRUE;
	if (CFLOW_Trace_Reorder) {
	  #pragma mips_frequency_hint NEVER
	  BB *bp = BBj;
	  char sep = ':';
	  fprintf(TFile, "Moved BB");
	  do {
	    fprintf(TFile, "%c%d", sep, BB_id(bp));
	    bp = BB_next(bp);
	    sep = ',';
	  } while (bp != BB_next(last_BBj));
	  if (BBk) fprintf(TFile, " from after BB:%d", BB_id(BBk));
	  fprintf(TFile, " to after BB:%d\n", BB_id(BBi));
	}

	/* Remove blocks to be moved from the chain.
	 */
	Chain_BBs(BB_prev(BBj), BB_next(last_BBj));

	/* Insert the blocks into the chain after BBi.
	 */
	Chain_BBs(last_BBj, BB_next(BBi));
	Chain_BBs(BBi, BBj);

	/* We can get into a vicious circle by moving blocks back
	 * and forth and as a result, we might not exit the pass 
	 * over the BBs. Rather than add an extra circuit breaker,
	 * we just skip over the moved BBs. If there were reordering
	 * opportunities in the moved blocks we'll see them on the
	 * next pass.
	 */
	BBi = last_BBj;

	break;

      next_succ:
	;
      } while (++i != nsuccs);
    }
    reordered |= more;
  } while (more && --cnt > 0);

  if (CFLOW_Trace_Reorder && cnt == 0) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "Circuit breaker tripped (%d) -- giving up\n",
	    MIN(PU_BB_Count, 25));
  }

  return reordered;
}

/* ====================================================================
 * ====================================================================
 *
 * Frequency-based Block reordering routines
 *
 * The algorithms here were derived from information in the paper:
 * "Profile Guided Code Positioning", by Karl Pettis and Robert C. Hansen, 
 * ACM SIGPLAN '90.
 *
 * ====================================================================
 * ====================================================================
 */


static double heuristic_tolerance = 0.40;
static double feedback_tolerance = 0.10;
static double cold_threshold;
static BB_SET *never_bbs;

/* The following data structure holds information about successor
 * edges in the CFG.
 */
typedef struct edge {
  BB *pred;		/* predecessor node */
  BB *succ;		/* successor node */
  struct edge *preds;	/* list of predecessor edges of 'succ' */
  double freq;		/* frequency this edge is taken */
  double weight;	/* weight assigned to this edge */
} EDGE;


/* ====================================================================
 *
 * Count_Succ_Edges
 *
 * Return the count of successor edges in the CFG. Used for allocating
 * data structures, so it is not exact. It does however guarantee to
 * return a value >= the actual number of succ edges.
 *
 * ====================================================================
 */
static INT
Count_Succ_Edges(void)
{
  BB *bb;
  INT count = 0;

  for (bb = REGION_First_BB; bb; bb = BB_next(bb)) {
    count += BBINFO_nsuccs(bb);
  }

  return count;
}


/* ====================================================================
 *
 * Compare_Edges
 *
 * qsort comparison function for sorting successor edges. The sort
 * key is the weight assigned to the edge. We sort in descending
 * order, i.e. larger weights come first.
 *
 * ====================================================================
 */
static INT
Compare_Edges(const void *p1, const void *p2)
{
  /* qsort claims to sort things in ascending order, but in reality
   * it is sorting based on how this comparison function classifies
   * the relationship. Define some constants that make that clear.
   */
  enum {
    sort_1_before_2 = -1, 
    sort_1_after_2  = 1, 
    sort_1_same_2   = 0 
  };
  EDGE *e1 = (EDGE *)p1;
  EDGE *e2 = (EDGE *)p2;
  double weight1 = e1->weight;
  double weight2 = e2->weight;

  if (weight1 > weight2) {
    return sort_1_before_2;
  } else if (weight1 < weight2) {
    return sort_1_after_2;
  } else {
    return sort_1_same_2;
  }
}


/* ====================================================================
 *
 * New_Edge
 *
 * Allocate and initialize a new successor edge. Edges are allocated
 * from an array. 'e' points to the next free element. The return
 * points to the next free element after allocating the new edge.
 *
 * ====================================================================
 */
inline EDGE *
New_Edge(
  EDGE *e,
  BB *pred,
  BB *succ,
  double prob,
  double weighted_prob,
  EDGE **bb_preds)
{
  if (   never_bbs 
      && (BB_SET_MemberP(never_bbs, pred) != BB_SET_MemberP(never_bbs, succ)))
  {

    /* If this edge is between BBs with differing freq hint pragmas,
     * then suppress the edge. This has the effect of mixing differing
     * pragma'd BBs into the same chain.
     */
    if (CFLOW_Trace_Freq_Order) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "  Suppressing BB:%d -> BB:%d edge -- freq hint pragmas differ\n",
		     BB_id(pred), BB_id(succ));
    }
  } else if (pred != succ) {
    const double pred_freq = BB_freq(pred);
    e->pred = pred;
    e->succ = succ;
    e->freq = pred_freq * prob;
    e->weight = pred_freq * weighted_prob;
    e->preds = bb_preds[BB_id(succ)];
    bb_preds[BB_id(succ)] = e;
    ++e;
  }
  return e;
}


/* ====================================================================
 *
 * Init_Edges
 *
 * Create a sorted (by weight) list of successor edges.
 *
 * The published algorithm uses the edge frequency for the weighting.
 * However this causes undesirable results in cases like 'if's where
 * where the probs are similar. For example, an 'if' with succs having
 * probs of 0.49 and 0.51 will result in the lower probability path
 * being moved out-of-band. Even worse is what happens to an if-then-else
 * with similar probs.
 *
 * The algorithm forms BB chains based on the weights of the edges. In 
 * cases where we prefer a specific chaining to occur, we stack the deck.
 * We do this in 2 cases: A conditional branch with 2 "equally likely"
 * succs, and a BB with N "equally likely" preds.
 *
 * We define "equally likely" to be "equal +/- a specified tolerance".
 * This definition allows us to trust feedback- more than heuristic-generated
 * frequencies and also allows user control.
 *
 * When we detect one of the two cases where we want to adjust the
 * weighting we bias the weightings so that the fall-through edge
 * has the highest weight. This guarantees that the biased edge
 * will be picked before other edges into (out of) the BB. Note however
 * that this does not force the biased edge to be chained (this is
 * a good thing). There might be a more important edge out of the
 * other side of one the involved BBs that forms a chain first.
 *
 * This weighting algorithm can be improved further -- consider
 * the following shortcomings: 
 *
 * 1) The fall-through edge is assumed to be the best edge
 *    to leave as the fall-through. This is OK in the 50/50 case, but
 *    for the others, the higher probability edge should be made the
 *    fall through (this is easy to do but would break the next shortcut).
 *
 * 2) We probably don't want to adjust the weight of all "equally likely"
 *    pred edges as we do. Ideally we'd only want to adjust those
 *    that are the join of some earlier 'if-then-else'. To do so we'd
 *    need to be able to analyze the structure of arbitrary 'if'
 *    constructs.
 *
 * ====================================================================
 */
static INT
Init_Edges(EDGE *edges)
{
  BB *bb;
  INT n_edges;
  EDGE *e;
  EDGE **bb_preds;

  if (CFLOW_Trace_Freq_Order) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "\nInit_Edges:\n");
  }

  /* We use 'bb_preds' to find the head of the list of predecessor edges
   * for given BB.
   */
  bb_preds = (EDGE **)alloca((PU_BB_Count + 2) * sizeof(EDGE *));
  BZERO(bb_preds, (PU_BB_Count + 2) * sizeof(EDGE *));

  /* Visit all the BBs and create and initialize the edges.
   * On this pass the weighting only accounts for things we can
   * determine from successor edges.
   */
  e = edges;
  for (bb = REGION_First_BB; bb; bb = BB_next(bb)) {
    BB *succ;

    switch (BBINFO_kind(bb)) {
    case BBKIND_LOGIF:
#if defined(TARG_SL)
    case BBKIND_ZDL_BODY:
    case BBKIND_FORK:
#endif
      {
        double prob0 = BBINFO_succ_prob(bb, 0);
	double prob1 = BBINFO_succ_prob(bb, 1);
	double weight0 = prob0;
	double weight1 = prob1;
	BB *succ0 = BBINFO_succ_bb(bb, 0);
	BB *succ1 = BBINFO_succ_bb(bb, 1);

	/* Only consider modifying the weighting if we will
	 * create two edges from this BB.
	 */
	if (succ0 != bb && succ1 != bb) {
	  BOOL have_feedback =    BB_freq_fb_based(succ0)
			       || BB_freq_fb_based(succ1)
			       || BB_freq_fb_based(bb);
	  double tolerance = have_feedback ? feedback_tolerance
					   : heuristic_tolerance;
	  double delta = 0.50 * tolerance;
          if (prob0 >= 0.50 - delta && prob0 <= 0.50 + delta) {
	    if (BB_next(bb) == succ0) {
	      weight0 = 0.51;
	      weight1 = 0.49;
	    } else if (BB_next(bb) == succ1) {
	      weight0 = 0.49;
	      weight1 = 0.51;
	    }
	  }
	}

	e = New_Edge(e, bb, succ0, prob0, weight0, bb_preds);
	e = New_Edge(e, bb, succ1, prob1, weight1, bb_preds);
      }
      break;
    case BBKIND_GOTO:
    case BBKIND_CALL:
      if (BBINFO_nsuccs(bb) != 0) {
	succ = BBINFO_succ_bb(bb, 0);
	e = New_Edge(e, bb, succ, 1.0, 1.0, bb_preds);
      }
      break;
    case BBKIND_VARGOTO:
    case BBKIND_INDGOTO:
      /* VARGOTO/INDGOTO can never fall through to its successor */
    case BBKIND_RETURN:
    case BBKIND_TAIL_CALL:
    case BBKIND_REGION_EXIT:
      /* No successors... */
      break;
    default:
      #pragma mips_frequency_hint NEVER
      FmtAssert(FALSE, ("unhandled BBKIND"));
      /*NOTREACHED*/
    }
  }
  n_edges = e - edges;

  /* For each BB, determine if we should modify the weighting
   * of the edges entering that BB.
   */
  for (bb = REGION_First_BB; bb; bb = BB_next(bb)) {
    INT n;
    double freq;
    double avg_prob;
    double delta;
    double tolerance;
    EDGE *biased;
    BOOL have_feedback;
    EDGE *preds = bb_preds[BB_id(bb)];

    /* Skip BBs with only 0 or 1 predecessors.
     */
    if (preds == NULL || preds->preds == NULL) continue;

    /* Determine the number of pred edges, the average probability
     * of a pred edge reaching bb, and the sum of the edge freqs,
     * and if any of the BBs have feedback.
     */
    have_feedback = BB_freq_fb_based(bb) != 0;
    freq = 0.0;
    for (n = 0, e = preds; e; ++n, e = e->preds) {
      have_feedback = have_feedback || BB_freq_fb_based(e->pred);
      freq += e->freq;
    }

    /* Determine the average probablity of a pred edge reaching bb,
     * the tolerance of the average probability that we will use
     * to consider the preds equally likely, and the corresponding
     * delta above and below the average probability.
     */
    avg_prob = 1.0 / (double)n;
    tolerance = have_feedback ? feedback_tolerance
			      : heuristic_tolerance;
    delta = avg_prob * tolerance;

    /* Make sure all the pred edges are within tolerance of equally
     * likely, and make note of the edge we want to bias.
     */
    biased = NULL;
    for (e = preds; e; e = e->preds) {
      if (BB_next(e->pred) == e->succ) biased = e;
      if (freq != 0.0) {
	double prob = e->freq / freq;
	if (prob < avg_prob - delta) goto next_bb;
	if (prob > (100.0 - avg_prob) + delta) goto next_bb;
	if (prob > avg_prob + delta && prob < (100.0 - avg_prob) - delta) goto next_bb;
      }
    }
    if (biased == NULL) continue;

    /* Adjust the weights so that the biased edge will sort
     * before the others.
     */
    for (e = preds; e; e = e->preds) {
      double prob;
      double factor;

      if (e == biased) {
	factor = 1.0 + 0.01;
      } else {
	factor = 1.0 - (0.01 / (double)(n - 1));
      }
      prob = avg_prob * factor;
      e->weight = freq * prob;
    }

  next_bb:
    ;
  }

  /* Sort the edges by their weights.
   */
  qsort(edges, n_edges, sizeof(EDGE), Compare_Edges);

  if (CFLOW_Trace_Freq_Order) {
    #pragma mips_frequency_hint NEVER
    INT i;
    for (i = 0; i < n_edges; ++i) {
      fprintf(TFile, "  BB:%d -> BB:%d, weight=%g, freq=%g\n", 
	      BB_id(edges[i].pred), BB_id(edges[i].succ), 
	      edges[i].weight, edges[i].freq);
    }
  }

  return n_edges;
}


/* The following data structure is used to generate chains of BBs
 * that fall-through to each other.
 */
typedef struct bbchain {
  double weight;	/* we use this for ordering the chains */
  BB *head;		/* first block of the chain */
  BB *tail;		/* last block of the chain */
  struct bbchain *next;	/* next chain */
  struct bbchain *prev;	/* previous chain */
  BOOL never;		/* blocks in chain have NEVER freq hint */
} BBCHAIN;

#ifdef TARG_SL
static BB*
Get_Zdl_Loop_Tail(BB * prolog)
{
  FmtAssert(OP_code(BB_last_op(prolog))==TOP_loop, ("Get_Zdl_Loop_Tail::wrong zdl prolog"));
  FmtAssert(BB_in_succs(prolog, BB_next(prolog)), ("Get_Zdl_Loop_Tail::wrong zdl header"));
  BB *body = BB_next(prolog);
  BB *tail = BB_Other_Predecessor(body, prolog);
  FmtAssert(tail!=NULL, ("Get_Zdl_Loop_Tail::cannot find zdl tail"));
  return tail;
}
#endif


/* ====================================================================
 *
 * Init_Chains
 *
 * Form the initial chains and the establish the mapping from BB to
 * chain.
 *
 * The initial chains consist of single BBs except when we encounter
 * regions that we must not reorder (a REGION region, a SWP region or 
 * structured exception handling region). When we encounter a BB that 
 * starts one such region, we grow its initial chain to include all the 
 * BBs in the region.
 *
 * ====================================================================
 */
static BB_MAP
Init_Chains(BBCHAIN *chains)
{
  BB *bb;
  BB *next_bb;
  RID *first_rid = BB_rid(REGION_First_BB);
  BB_MAP chain_map = BB_MAP_Create();
  BBCHAIN *prev = NULL;

  /* Make initial chains for all the BBs in the region.
   */
  for (bb = REGION_First_BB; bb; bb = next_bb) {
    BB *tail = bb;
    BB_MAP_Set(chain_map, bb, chains);

#ifdef TARG_SL
    /* We do not intend to reorder bb inside a zero delay loop
     */
    {
      OP *op = BB_last_op(bb);
      if (op!=NULL && OP_code(op) == TOP_loop) {
        BB *zdl_tail=Get_Zdl_Loop_Tail(bb);
        BB *loop_bb = bb;
        while (loop_bb!=zdl_tail) {
          loop_bb = BB_next(loop_bb);
          BB_MAP_Set(chain_map, loop_bb, chains);
        }
        tail = zdl_tail;
      }
    }
#endif

    /* We have to be careful about reordering in the face of regions
     * (REGION and exception regions). The two kind of regions have
     * slightly different rules, but we handle both here.
     *
     * For REGION regions, we want to respect nesting. When we find
     * a BB that is not part of the outermost region, we create a chain
     * to include the BB such that the head and tail BBs of the chain
     * are in the outermost region.
     *
     * Exception regions are similar except that the created chain need
     * only consist of the BBs in the same or nested exception region.
     */
    next_bb = BB_next(tail);
    if (Optimize_exception_ranges != 2 &&
        !(Optimize_exception_ranges == 1 && PU_cxx_lang(Get_Current_PU()))) {
      // if the next BB is of different REGION, put them into a chain
      while (next_bb &&
             ((BB_rid(next_bb) != first_rid) ||
             (BBINFO_eh_rgn(tail) && BBINFO_eh_rgn(next_bb)))
      ) {
        do {
          tail = next_bb;
          BB_MAP_Set(chain_map, tail, chains);
          next_bb = BB_next(tail);
        } while (next_bb && BB_rid(tail) != first_rid);
      }
    } else if (!PU_simple_eh(Get_Current_PU())) {
        while (TRUE) {
          if (next_bb &&
              (BB_rid(next_bb) != BB_rid(tail) ||
               BBINFO_pseudo_eh_rgn(next_bb) != BBINFO_pseudo_eh_rgn(tail))) {
          tail = next_bb;
          BB_MAP_Set(chain_map, next_bb, chains);
          next_bb = BB_next(tail);
          } else break;
        }
    }

    /* Isolate the new chain.
     */
    BB_prev(bb) = NULL;
    BB_next(tail) = NULL;
	
    chains->head = bb;
    chains->tail = tail;
    chains->next = chains + 1;
    chains->prev = prev;
    chains->weight = 0.0;
    chains->never = never_bbs && BB_SET_MemberP(never_bbs, bb);

    prev = chains;
    ++chains;

    if (CFLOW_Trace_Freq_Order && bb != tail) {
      #pragma mips_frequency_hint NEVER
      BB *bbc;
      fprintf(TFile, "  multi-BB chain created:");
      bbc = bb; 
      do {
	fprintf(TFile, "%c%d", bbc == bb ? ' ' : '-', BB_id(bbc));
      } while (bbc = BB_next(bbc));
      fprintf(TFile, "\n");
    }
  }
  prev->next = NULL;

  return chain_map;
}


/* ====================================================================
 *
 * BB_Chain
 *
 * Given a BB, return the chain that contains it.
 *
 * ====================================================================
 */
inline BBCHAIN *
BB_Chain(BB_MAP chain_map, BB *bb)
{
  return (BBCHAIN *)BB_MAP_Get(chain_map, bb);
}


/* ====================================================================
 *
 * Remove_Chain
 *
 * Remove the specified chain from a doubly-linked list of chains.
 *
 * ====================================================================
 */
inline BBCHAIN *
Remove_Chain(BBCHAIN *chains, BBCHAIN *chain)
{
  BBCHAIN *prev = chain->prev;
  BBCHAIN *next = chain->next;
  if (prev) prev->next = next;
  if (next) next->prev = prev;
  if (chains == chain) chains = next;

  return chains;
}


/* ====================================================================
 *
 * Weight_Succ_Chains
 *
 * Given the specified chain, adjust the weights of its succcessor
 * chains by the edge frequency to those chains.
 *
 * ====================================================================
 */
static void
Weight_Succ_Chains(BB_MAP chain_map, BBCHAIN *chain)
{
  BB *bb;
  mINT16 last_rgn = BBINFO_pseudo_eh_rgn(chain->tail);

  /* The successors of 'chain' are the successors of the BBs in 'chain'.
   */
  bb = chain->head;
  do {
    INT i;
    INT nsuccs = BBINFO_nsuccs(bb);
    float bb_freq = BB_freq(bb);

    /* For each successor of BB, adjust it's weight by the freq of the
     * edge from BB to the succ.
     */
    for (i = 0; i < nsuccs; ++i) {
      BB *succ = BBINFO_succ_bb(bb, i);
      BBCHAIN *succ_chain = BB_Chain(chain_map, succ);
      succ_chain->weight += bb_freq * BBINFO_succ_prob(bb, i);
      // keep loops with flow that are unrolled together
      if (CG_LOOP_unroll_level == 2) {
        if (BB_unrolled_fully(bb))
          succ_chain->weight = 1.0;
      }
    }
  } while (bb = BB_next(bb));
}


/* ====================================================================
 *
 * Emit_Cold_Threshold_Note
 *
 * Emit a love note that marks the start of the cold region.
 *
 * ====================================================================
 */
static void
Emit_Cold_Threshold_Note(
  NOTE_ACTION action,
  NOTE_INFO  *,
  FILE       *file
)
{
  switch (action) {
  case NOTE_PRINT_TO_ANL_FILE:
    break;
  case NOTE_PRINT_TO_FILE:
    {
      const char *prefix = file == Asm_File ? ASM_CMNT_LINE : "";
      fprintf(file, "%s<cflow> Start of cold region\n", prefix);
    }
    break;
  case NOTE_PRINT_HANDLER_NAME_TO_FILE:
    fprintf(file,"Emit_Cold_Threshold_Note");
    break;
  default:
    #pragma mips_frequency_hint NEVER
    FmtAssert(FALSE,("Didn't recognize action"));
    /*NOTREACHED*/
  }
}


/* ====================================================================
 *
 * Create_Cold_Region
 *
 * Create a cold region starting with the chain <cold>.
 *
 * The cold region is identified by put the BBs in the cold region
 * into a transparent region of type RID_TYPE_cold. All BBs in the
 * cold region well either have this RID type or will have an ancestor
 * which does.
 *
 * ====================================================================
 */
static RID *
Create_Cold_Region(BBCHAIN *cold)
{
  BBCHAIN *ch;
  RID *r;
  RID *parent = BB_rid(REGION_First_BB);

  /* Create the REGION for the cold region.
   */
  r = RID_Create(New_Region_Id(), 0, NULL);
  RID_level(r) = RL_CG;
  RID_type(r) = RID_TYPE_cold;
  RID_bounds_exist(r) = REGION_BOUND_UNKNOWN;
  RID_has_return(r) = REGION_NO_RETURN;
  RID_num_exits(r) = 0;
  RID_is_glue_code(r) = FALSE;        
  RID_parent(r) = parent;
  RID_cginfo(r) = NULL; /* ?? this should have a value */


  if ( parent ) RID_Add_kid(r, parent);

  /* Modify the REGION tree to include the new region and modify
   * the RID of any BBs in the cold region that are at the same
   * level as REGION_First_BB.
   */
  ch = cold;
  do {
    BB *bb;
    for (bb = ch->head; bb; bb = bb->next) {
      RID *bb_rid = BB_rid(bb);
      if (bb_rid == parent) {
	BB_rid(bb) = r;
      } else if (bb_rid && RID_parent(bb_rid) == parent) {
	RID_parent(bb_rid) = r;
	RID_Add_kid(bb_rid, r);
      }
      Set_BBINFO_cold(bb, TRUE);
    }
  } while (ch = ch->next);

  return r;
}


/* ====================================================================
 *
 * Generate_Hot_Cold_Jump
 *
 * Generate a jump between the hot and the cold regions (in either
 * direction). The source of the jump is <bb>. The target of the
 * jump is the <isuc>th successor of <bb>. <jump_map> is used to
 * cache the created "jump BB" for each target of a jump between
 * hot and cold regions. <jumps> is a chain of jump BBs that are
 * created by Generate_Hot_Cold_Jump (the jump BBs are not placed
 * in the PU/regions BB chain).
 *
 * ====================================================================
 */
static void
Generate_Hot_Cold_Jump(
  BB *bb, 
  INT isuc, 
  BB_MAP jump_map, 
  BB **jumps,
  RID *cold_rid)
{
  BB *jump_bb;
  BB *suc = BBINFO_succ_bb(bb, isuc);
  INT64 offset = BBINFO_succ_offset(bb, isuc);
  OPS ops = OPS_EMPTY;

  /* If <bb> is already a GOTO block then we can just replace
   * the branch with the jump generated above.
   */
  if (BBINFO_kind(bb) == BBKIND_GOTO) {
    OP *br = BB_branch_op(bb);
    OP *delay_slot = (PROC_has_branch_delay_slot() && br) ? OP_next(br) : NULL;
    if (br) BB_Remove_Op(bb, br);

    Exp_Local_Jump(suc, offset, &ops);
    if (delay_slot) {
      BB_Insert_Ops_Before(bb, delay_slot, &ops);
    } else {
      BB_Append_Ops(bb, &ops);
    }
    if (BB_branch_wn(bb) == NULL) {
      LABEL_IDX lab = Gen_Label_For_BB(suc);
      LABEL_IDX lnum = lab;
      WN *wn = WN_CreateGoto(lab, lnum);
      BB_branch_wn(bb) = wn;
    }

    /* Update for liveness info.
     */
    if (!CG_localize_tns) {
      GRA_LIVE_Compute_Liveness_For_BB(bb);
    }

DevWarn("replaced branch with jump in BB:%d", BB_id(bb));
    return;
  }

  /* We can't append the jump to <bb>, so use a "jump" BB and target
   * it instead.
   */
  Unlink_Pred_Succ(bb, suc);

  /* Get the jump BB, creating it if necessary.
   */
  jump_bb = (BB *)BB_MAP_Get(jump_map, suc);
  if (jump_bb == NULL) {
    WN *wn;
    BBINFO *bbinfo;
    LABEL_IDX lab = Gen_Label_For_BB(suc);
    LABEL_IDX lnum = lab;

    jump_bb = Alloc_BB_Like(NULL);
    BB_rid(jump_bb) = BBINFO_cold(bb) ? cold_rid : BB_rid(REGION_First_BB);
    BB_freq(jump_bb) = 0.0;
    wn = WN_CreateGoto(lab, lnum);
    BB_branch_wn(jump_bb) = wn;

    bbinfo = (BBINFO *)MEM_POOL_Alloc(&MEM_local_nz_pool, sizeof(BBINFO));
    bbinfo->kind = BBKIND_GOTO;
    bbinfo->eh_rgn = 0;
    bbinfo->cold = BBINFO_cold(bb);
    bbinfo->nsuccs = 1;
    bbinfo->succs[0].bb = suc;
    bbinfo->succs[0].offset = offset;
    bbinfo->succs[0].prob = 1.0;
    BB_MAP_Set(bb_info_map, jump_bb, bbinfo);

    Link_Pred_Succ_with_Prob(jump_bb, suc, 1.0);

    Exp_Local_Jump(suc, offset, &ops);
    BB_Append_Ops(jump_bb, &ops);

    BB_MAP_Set(jump_map, suc, (void *)jump_bb);

    Chain_BBs(jump_bb, *jumps);
    *jumps = jump_bb;
DevWarn("created jump BB:%d", BB_id(jump_bb));
  }

  /* Re-target <bb> to the jump BB.
   */
  Set_BBINFO_succ_bb(bb, isuc, jump_bb);
  Link_Pred_Succ_with_Prob(bb, jump_bb, BBINFO_succ_prob(bb, isuc));
  BB_freq(jump_bb) += BB_freq(bb) * BBINFO_succ_prob(bb, isuc);

  /* Compute live regs for the jump BB.
   */
  if (!CG_localize_tns && BB_bbregs(jump_bb) == NULL) {
    GRA_LIVE_Compute_Liveness_For_BB(jump_bb);
  }
DevWarn("redirected BB:%d to jump BB:%d", BB_id(bb), BB_id(jump_bb));
}


/* ====================================================================
 *
 * Patch_Hot_Cold_Jumps
 *
 * Scan the hot and cold regions and locate any control flow edges
 * between the two regions. For each edge, insert a jump between the
 * two.
 *
 * ====================================================================
 */
static void
Patch_Hot_Cold_Jumps(
  BBCHAIN *chains, 
  BB_MAP chain_map, 
  BBCHAIN *cold, 
  RID *cold_rid)
{
  BBCHAIN *ch;
  BB_MAP jump_map;
  BB *jumps;

  FmtAssert(chains != cold, ("empty hot region!"));

  /* Generate jumps between the hot and cold regions.
   */
  jump_map = BB_MAP_Create();

  jumps = NULL;
  ch = chains;
  do {
    BB *bb;
    for (bb = ch->head; bb; bb = bb->next) {
      INT i;

      if (   BBINFO_kind(bb) == BBKIND_VARGOTO 
	  || BBINFO_kind(bb) == BBKIND_INDGOTO) continue;

      for (i = 0; i < BBINFO_nsuccs(bb); ++i) {
	BB *suc = BBINFO_succ_bb(bb,i);
	if (BBINFO_cold(bb) != BBINFO_cold(suc)) {
	  Generate_Hot_Cold_Jump(bb, i, jump_map, &jumps, cold_rid);
	}
      }
    }
  } while (ch = ch->next);

  BB_MAP_Delete(jump_map);

  /* Now <jumps> contains all the jump BBs we created. Find appropriate
   * places for them in the BB chain: The jump BBs are always placed
   * at the end of a chain. The candidate chains are the chains containing
   * the preds of the jump BB. If one or more of the preds is the tail
   * of a chain, the those are the only candidates. The candidate containing
   * the highest frequency edge to the jump is then chosen. The result
   * of this is that if we first prefer to minimize code expansion.
   */
  while (jumps) {
    BBLIST *pedge;
    BOOL best_pred_is_tail;
    BB *best_pred = NULL;
    float best_freq = -FLT_MAX;
    BB *bb = jumps;
    jumps = BB_next(jumps);

    best_pred_is_tail = FALSE;
    FOR_ALL_BB_PREDS(bb, pedge) {
      BOOL pred_is_tail;
      BB *pred = BBLIST_item(pedge);
      BBLIST *sedge = BB_Find_Succ(pred, bb);
      float freq = BB_freq(pred) * BBLIST_prob(sedge);
      ch = BB_Chain(chain_map, pred);
      pred_is_tail = pred == ch->tail;

      if (   (pred_is_tail == best_pred_is_tail && freq > best_freq)
	  || (pred_is_tail && !best_pred_is_tail))
      {
	best_freq = freq;
	best_pred = pred;
	best_pred_is_tail = pred_is_tail;
      }
    }

    ch = BB_Chain(chain_map, best_pred);
    Chain_BBs(bb, NULL);
    Chain_BBs(ch->tail, bb);
    ch->tail = bb;
  }
}


/* ====================================================================
 *
 * Validate_Cold_Region
 *
 * We've formed a tenative cold region. Verify that there isn't some
 * constraint that isn't being violated and if so adjust the start of
 * the cold region to correct the problems.
 *
 * ====================================================================
 */
static BBCHAIN *
Validate_Cold_Region(BBCHAIN *chains, BBCHAIN *cold_region)
{
  mBB_NUM *bb_ord;
  BB_NUM n;
  BBCHAIN *ch;

#define BB_ORD(b) (bb_ord[BB_id(b)])

  /* We'll need to know which side of the cold region boundary
   * a given BB is on. Since we will be moving the boundary
   * a hot/cold flag would have to be recomputed. So instead
   * we give each BB an ordinal number. Then we can compare
   * the given BBs ordinal number with the cold region start's.
   *
   * Allocate the BB-ordinal map and assign the numbers.
   */
  bb_ord = (mBB_NUM *)alloca((PU_BB_Count + 1) * sizeof(mBB_NUM));
  n = 0;
  ch = chains;
  do {
    BB *bb;
    for (bb = ch->head; bb; bb = BB_next(bb)) bb_ord[BB_id(bb)] = n++;
  } while (ch = ch->next);

  /* Now scan the cold region, chain by chain, looking for problems.
   * When we encounter a problem, set the cold region to the start
   * of the next region and re-start the scan from the new boundary.
   */
  ch = cold_region;
  do {
    BB *bb;
    BB_NUM cold_ord = BB_ORD(cold_region->head);

#ifdef TARG_SL
    BB *ch_head = ch->head;
    if (ch_head != NULL) {
      BBLIST *edge;
      FOR_ALL_BB_PREDS(ch_head, edge) {
        BB *pred = BBLIST_item(edge);
        if (OP_fork(BB_last_op(pred))) {
          BBCHAIN *new_cold = ch->next;
          if (CFLOW_Trace_Freq_Order) {
            #pragma mips_frequency_hint NEVER
            fprintf(TFile, "  fork target caused cold region start"
                           " to move from BB:%d to BB:%d\n",
                           BB_id(cold_region->head),
                           new_cold ? BB_id(new_cold->head) : -1);
          }
          cold_region = new_cold;
          goto next_chain;
        }
      }
    }
#endif

    for (bb = ch->head; bb; bb = BB_next(bb)) {
      BBLIST *edge;

      /* Yet another kludge for exceptions: we can't move an exception
       * region or handler into the cold region, the EH tables don't
       * correctly handle it. 
       */
      if (BBINFO_eh_rgn(bb) || BB_handler(bb)) {
	BBCHAIN *new_cold = ch->next;
	if (CFLOW_Trace_Freq_Order) {
	  #pragma mips_frequency_hint NEVER
	  fprintf(TFile, "  EH region or handler caused cold region start"
			 " to move from BB:%d to BB:%d\n",
			 BB_id(cold_region->head), 
			 new_cold ? BB_id(new_cold->head) : -1);
	}
	cold_region = new_cold;
	goto next_chain;
      }

#ifdef TARG_SL
      if (OP_fork(BB_last_op(bb))) {
        BBCHAIN *new_cold = ch->next;
        if (CFLOW_Trace_Freq_Order) {
          #pragma mips_frequency_hint NEVER
          fprintf(TFile, "  fork instruction caused cold region start"
                         " to move from BB:%d to BB:%d\n",
                         BB_id(cold_region->head),
                         new_cold ? BB_id(new_cold->head) : -1);
        }
        cold_region = new_cold;
        goto next_chain;
      }

#endif

      /* Check to make sure that an edge between the hot and cold
       * region is also not a transition between regions. If we
       * allowed this then after the jump was inserted, GRA would
       * not be able to find the glue copies.
       */
      FOR_ALL_BB_SUCCS(bb, edge) {
	BB *suc = BBLIST_item(edge);
	if (   BB_rid(bb) != BB_rid(suc)
	    && (BB_ORD(bb) < cold_ord) != (BB_ORD(suc) < cold_ord))
	{
	  BBCHAIN *new_cold = ch->next;
	  if (CFLOW_Trace_Freq_Order) {
	    #pragma mips_frequency_hint NEVER
	    fprintf(TFile, "  glue block caused cold region start"
			   " to move from BB:%d to BB:%d\n",
			   BB_id(cold_region->head), 
			   new_cold ? BB_id(new_cold->head) : -1);
	  }
	  cold_region = new_cold;
	  goto next_chain;
	}
      }
      FOR_ALL_BB_PREDS(bb, edge) {
	BB *pred = BBLIST_item(edge);
	if (   BB_rid(bb) != BB_rid(pred)
	    && (BB_ORD(bb) < cold_ord) != (BB_ORD(pred) < cold_ord))
	{
	  BBCHAIN *new_cold = ch->next;
	  if (CFLOW_Trace_Freq_Order) {
	    #pragma mips_frequency_hint NEVER
	    fprintf(TFile, "  glue block caused cold region start"
			   " to move from BB:%d to BB:%d\n",
			   BB_id(cold_region->head), 
			   new_cold ? BB_id(new_cold->head) : -1);
	  }
	  cold_region = new_cold;
	  goto next_chain;
	}
      }
    }
  next_chain:
    ;
  } while (ch = ch->next);

  return cold_region;
}


/* ====================================================================
 *
 * Order_Chains
 *
 * Order the chains topologically, guided by a frequency weighting.
 *
 * The first chain will be the chain containing the entry point. Subsequent
 * chains are selected according to their weight -- chains with
 * a heavier weight are chosen first. The weight of a unordered chain 
 * is the sum of the edge frequencies from the ordered chains to that
 * chain. Once a chain is ordered, its weight is irrelevant.
 *
 * This ordering has the effect of somewhat clustering related
 * chains, but more certainly, it causes frequently executed
 * chains to end up near the end of the region.
 *
 * Note that we only consider the edges from the ordered chains to the
 * unordered chains when computing the weight. Ideally we should consider
 * the edges from the unordered chains to the ordered chains as well.
 *
 * ====================================================================
 */
static BBCHAIN *
Order_Chains(BBCHAIN *unordered, BB_MAP chain_map)
{
  BBCHAIN *chain;
  BBCHAIN *ordered;
  BBCHAIN *last_ordered;
  BB *root;

  /* Get the root BB.
   */
  if (Compiling_Proper_REGION) {
    root = CGRIN_entry(RID_Find_Cginfo(REGION_First_BB));
  } else if (BB_LIST_rest(Entry_BB_Head) == NULL) {
    root = BB_LIST_first(Entry_BB_Head);
  } else {
    /****** TODO:  "main" entry seems to need to come first
     */
    root = REGION_First_BB;
  }

  /* Find the chain containing the root BB. Remove the chain
   * from the unordered list and create the ordered list with
   * it as the head.
   */
  chain = BB_Chain(chain_map, root);
  unordered = Remove_Chain(unordered, chain);
  chain->prev = NULL;
  chain->next = NULL;
  ordered = chain;
  last_ordered = chain;

  /* Continue as long as there are unordered chains. The weights
   * of the unordered chains are accumulated as we move chains
   * to the ordered list.
   */
  while (unordered) {
    BBCHAIN *ch;
    double max_weight;
    BOOL eh_succ;
    BOOL eh_continue;
    mINT32 last_eh;

    last_eh = BBINFO_pseudo_eh_rgn(last_ordered->tail);

    /* Adjust the weights for the edges from the last ordered chain
     * to the unordered chains.
     */
    Weight_Succ_Chains(chain_map, last_ordered);

    /* Select the unordered chain with the heaviest weight, preferring
     * normal BBs over those with a NEVER freq hint.
     */
    max_weight = unordered->weight;
    eh_succ = (BBINFO_pseudo_eh_rgn(unordered->head) == last_eh);
    eh_continue = (BBINFO_pseudo_eh_rgn(unordered->tail) == last_eh);

    chain = unordered;
    for (ch = unordered->next; ch; ch = ch->next) {
      BOOL my_succ = (BBINFO_pseudo_eh_rgn(ch->head) == last_eh);
      BOOL my_continue = (BBINFO_pseudo_eh_rgn(ch->tail) == last_eh);
      if (eh_succ && !my_succ)
        continue;
      if (eh_continue && !my_continue) 
        continue;
      if ((my_succ && !eh_succ) || (my_continue && !eh_continue))
      {
        eh_succ = my_succ;
        eh_continue = my_continue;
        chain = ch;
        max_weight = ch->weight;
      } else if ((ch->never == chain->never && ch->weight > max_weight) ||
                 (!ch->never && chain->never))
      {
	chain = ch;
	max_weight = ch->weight;
      }
    }

    /* If the input freqs/probs are bogus (usually NaNs) we might
     * not find a candidate. Strongly complain and then just take 
     * the first thing on the list. 
     */
    if (max_weight < 0.0) {
      if (CG_warn_bad_freqs) {
	#pragma mips_frequency_hint NEVER
	DevWarn("cflow (Order_Chains) found inconsistent freqs:\n"
		"\tmost likely, something before cflow is broken.\n"
		"\tcflow will cope but please submit a pv");
      }
      chain = unordered;
    }

    /* Move the selected chain from the unordered list to the tail
     * of the ordered list.
     */
    unordered = Remove_Chain(unordered, chain);
    chain->next = NULL;
    chain->prev = last_ordered;
    last_ordered->next = chain;
    last_ordered = chain;
  }

  /* The chains have been ordered. Now if a cold region threshold
   * has been defined, locate the appropriate chain and create
   * the cold region.
   */
#if defined(TARG_SL)
  if (CFLOW_cold_threshold &&  (current_flags & CFLOW_COLD_REGION)) {
#else
  if (CFLOW_cold_threshold) {
#endif
    BBCHAIN *ch = last_ordered;

    /* The cold region boundary is placed between chains to avoid
     * breaking a fall-through path. The boundary occurs at the
     * first chain which is "never" executed, or all its BBs
     * have a frequency <= -CG:cflow_cold_threshold.
     */
    do {
      BB *bb;

      if (ch->never) break;

      for (bb = ch->head; bb; bb = BB_next(bb)) {
	if (BB_freq(bb) > cold_threshold) {
	  ch = ch->next;
	  goto found_hot;
	}
      }
    } while (ch = ch->prev);

  found_hot:
    ;

    /* If we have some cold BBs -- create the cold region.
     */
    if (ch) {

      /* Make sure we have a valid cold region, adjusting the
       * start if necessary, and if there is still a cold region,
       * do the formal cold region creation.
       */
      ch = Validate_Cold_Region(ordered, ch);
      if (ch) {
	RID *cold_rid;
	BB *cold_bb = ch->head;
	NOTE_Add_To_BB(cold_bb, Emit_Cold_Threshold_Note, (NOTE_INFO *)NULL);
	cold_rid = Create_Cold_Region(ch);
	Patch_Hot_Cold_Jumps(ordered, chain_map, ch, cold_rid);
      }
    }
  }

  return ordered;
}


static void Print_Chain_BBs(BBCHAIN *chain);
/* ====================================================================
 *
 * Optimize_Cyclic_Chain
 *
 * Given a cyclic chain (a chain where the chain-head is a succ of the
 * chain-tail), we can arbitrarily rotate the chain so that any BB
 * is the head. Because the freq ordering algorithm works only at the
 * BB edge level, we can end up creating cycles that are non-optimal,
 * or possibly just rotated for no good reason. Therefore, when a cycle
 * is formed, we analyze the chain and rotate it get the lowest
 * branch costs.
 *
 * ====================================================================
 */
static void
Optimize_Cyclic_Chain(BBCHAIN *chain, BB_MAP chain_map)
{
  float best_gain;
  BB *bb;
  BB *best_tail = chain->tail; // only necessary if all BBs in EH regions
  RID *outer_rid = BB_rid(chain->head);

  /* Examine each BB in the chain, and determine the gain we would
   * see if it were the tail of the chain. The BB with the largest
   * gain is tracked and eventually becomes the tail of the chain.
   *
   * NOTE: We loop backwards over the chain so that in cases where
   * the order of the chain does not affect performance, we choose
   * the current tail and don't perform a gratuitous rotation.
   */
  best_gain = -FLT_MAX;
  for (bb = chain->tail; bb; bb = BB_prev(bb)) {
    float tail_gain;
    BB *next;

    /* Get the next BB in the cycle.
     */
    next = bb == chain->tail ? chain->head : BB_next(bb);

    /* Can't place the tail in the middle of an EH region --
     * the EH region must be kept contiguous and in the same order.
     */
    if ((Optimize_exception_ranges != 2 &&
         !(PU_cxx_lang(Get_Current_PU())) && Optimize_exception_ranges == 1)
        && BBINFO_eh_rgn(bb) && BBINFO_eh_rgn(next)) continue;

    if (BBINFO_pseudo_eh_rgn(bb) != BBINFO_pseudo_eh_rgn(next) ||
        BBINFO_pseudo_eh_rgn(bb) != BBINFO_pseudo_eh_rgn(chain->tail) ||
        BBINFO_pseudo_eh_rgn(bb) != BBINFO_pseudo_eh_rgn(chain->head)) continue;

    /* Can't place the tail such that the new head and tail BBs
     * would not be in the same region as originally.
     */
    if (BB_rid(bb) != outer_rid || BB_rid(next) != outer_rid) continue;

    /* Compute the gain in performance we would see by making this
     * BB be the tail of the chain. We model dynamic branch costs
     * to compute the performance gain.
     */
    switch (BBINFO_kind(bb)) {
    case BBKIND_LOGIF:
      {
	float p_fall;
	float p_taken;
	BB *targ;
	BBCHAIN *targ_chain;
	BOOL no_fall;

	/* Find the [taken] target of the branch and the probability
	 * that we take it.
	 */
        targ = BBINFO_succ_bb(bb, 0);
	p_taken = BBINFO_succ_prob(bb, 0);
	if (targ == next) {
	  p_taken = 1.0 - p_taken;
	  targ = BBINFO_succ_bb(bb, 1);
	}

	/* The cost of this branch being the tail depends on whether
	 * or not the fall through BB is placed after this chain.
	 * In general, we can't predict the placement of the fall
	 * through BB, but there are a couple of cases where we can
	 * be sure. Therefore, we are optimistic that the branch
	 * can fall through to the next BB, except if the target is
	 * already placed in a chain and a way that would make it 
	 * impossible to be next.
	 */
	targ_chain = BB_Chain(chain_map, targ);
	no_fall = targ_chain == chain || targ != targ_chain->head;

	/* Find the probabilites of the taken and fall through targets.
	 * If fall through is not the next bb then model the placement
	 * of the unconditional branch done by Finalize_BB.
	 */
	if (no_fall && p_taken < 0.5) p_taken = 1.0 - p_taken;
	p_fall = 1.0 - p_taken;

	/* Finally, compute the gain of the BB being the tail,
	 * including the extra unconditional branch when necessary.
	 */
	tail_gain =   (p_fall - p_taken) * br_fall_cost
		    + (p_taken - p_fall) * br_taken_cost;
	if (no_fall) tail_gain -= p_taken * br_taken_cost;
	tail_gain *= BB_freq(bb);
      }
      break;
    case BBKIND_GOTO:
    case BBKIND_CALL:

      /* Making these the tail requires an unconditional branch;
       * otherwise it can just fall through to the next BB.
       */
      tail_gain = 0.0 - (br_taken_cost * BB_freq(bb));
      break;
    default:

      /* These cost the same regardless of where they are in the chain.
       */
      tail_gain = 0.0;
      break;
    }

    /* If we found a bigger gain, or the same gain but this is the
     * loophead of the outermost cycle, then this is best candidate so far.
     */
    if (   (tail_gain > best_gain)
	|| (   tail_gain == best_gain
	    && BB_loop_head_bb(next) == next
	    && BB_loop_head_bb(bb) == next)
    ) {
      best_tail = bb;
      best_gain = tail_gain;
    }
  }

  /* If we have found a better tail, rotate the chain.
   */
  if (best_tail != chain->tail) {
    BB *best_head = BB_next(best_tail);
    BB *orig_head = chain->head;
    BB *orig_tail = chain->tail;
    if (CFLOW_Trace_Freq_Order) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile,
	      "  BB:%d would be a better head of cyclic chain than BB:%d\n",
	      BB_id(best_head), BB_id(orig_head));
      Print_Chain_BBs(chain);
    }
    Chain_BBs(NULL, best_head);
    Chain_BBs(best_tail, NULL);
    Chain_BBs(orig_tail, orig_head);
    chain->head = best_head;
    chain->tail = best_tail;
  }
}


/* ====================================================================
 *
 * Grow_Chains
 *
 * Attempt to grow chains along frequently executed paths.
 *
 * ====================================================================
 */
static BBCHAIN *
Grow_Chains(BBCHAIN *chains, EDGE *edges, INT n_edges, BB_MAP chain_map)
{
  INT i;
#ifdef KEY
  INT j;
#endif

  /* Visit the succ edges from heaviest weight to lightest.
   */
  for (i = 0; i < n_edges; ++i) {
    EDGE *e = edges + i;
    BB *pred = e->pred;
    BB *succ = e->succ;
    BBCHAIN *pchain = BB_Chain(chain_map, pred);
    BBCHAIN *schain = BB_Chain(chain_map, succ);

#ifdef KEY
    if (CFLOW_Enable_Freq_Order_On_Heuristics){
      if (e->freq == 0) 
        continue;
      if  (BB_id(pred) > BB_id(succ)) {
        for (j = i+1; j < n_edges; j++) {
          if (edges[j].pred == pred && edges[j].succ != succ && edges[j].freq > 0)
            break;
        }
        if (j != n_edges)
          continue;
      }
    }
#endif

    // keep loops with flow that are unrolled together
    BOOL can_combine = TRUE;
    if (CG_LOOP_unroll_level == 2) {
      can_combine = (BB_unrolled_fully(succ) == FALSE);
    }

    /* If this edge connects the tail of one chain to the head of
     * another, then combine the chains.
     */
    if ((can_combine) &&
        BBINFO_pseudo_eh_rgn(succ) == BBINFO_pseudo_eh_rgn(pred) &&
        (BBINFO_pseudo_eh_rgn(succ) == BBINFO_pseudo_eh_rgn(schain->tail) ||
         BBINFO_pseudo_eh_rgn(pred) == BBINFO_pseudo_eh_rgn(pchain->head)) &&
        (pchain != schain && pchain->tail == pred && schain->head == succ)) {
      INT j;
      INT nsuccs;
      BB *bb;
      BB *head = pchain->head;
      BB *tail = schain->tail;

      /* We arbitrarily add the succ chain's BBs to the pred chain.
       */
      Chain_BBs(pred, succ);
      pchain->tail = tail;

      /* Update the BB-to-chain mapping for the BBs we're moving.
       */
      for (bb = schain->head; ; bb = BB_next(bb)) {
	BB_MAP_Set(chain_map, bb, pchain);
	if (bb == tail) break;
      }

      /* Remove the succ chain.
       */
      chains = Remove_Chain(chains, schain);

      /* If we've just created a cycle in the chain BBs, then see if
       * the chain order can be optimized.
       */
      nsuccs = BBINFO_nsuccs(tail);
      for (j = 0; j < nsuccs; j++) {
	if (   BBINFO_succ_bb(tail, j) == head
	    && BBINFO_succ_offset(tail, j) == 0
	) {
	  Optimize_Cyclic_Chain(pchain, chain_map);
	  break;
	}
      }
    }
  }

  return chains;
}


/* ====================================================================
 *
 * Combine_Chains
 *
 * Combine the BB chains into one and make it the region's BB chain.
 *
 * ====================================================================
 */
static void
Combine_Chains(BBCHAIN *chains)
{
  BBCHAIN *chain;
  BB *tail = NULL;
  BB *head = chains->head;
  for (chain = chains; chain; chain = chain->next) {
    Chain_BBs(tail, chain->head);
    tail = chain->tail;
  }
  Chain_BBs(tail, NULL);

  if (Compiling_Proper_REGION) {
    RID *head_rid = BB_rid(head);
    CGRIN *cgrin = RID_cginfo(head_rid);
    FmtAssert(/* head_rid == BB_rid(tail) && */ 
	      head_rid == BB_rid(REGION_First_BB),
	      ("Combine_Chains: illegal region formed"));
	      /* NOTE: the tail check was too restrictive, for Olimit
	       * regions they are just laid out end-to-end.
	       */
    CGRIN_first_bb(cgrin) = head;
    CGRIN_last_bb(cgrin) = tail;
  }
  REGION_First_BB = head;
}


/* ====================================================================
 *
 * Print_Chain_BBs
 *
 * Print the BBs in <chain> to the trace file.
 *
 * ====================================================================
 */
static void
Print_Chain_BBs(BBCHAIN *chain)
{
  BB *bb;
  for (bb = chain->head; ; ) {
    fprintf(TFile, "%d", BB_id(bb));
    bb = BB_next(bb);
    if (!bb) break;
    fprintf(TFile, "-");
  }
  fprintf(TFile, "\n");
}


/* ====================================================================
 *
 * Print_Chains
 *
 * Print the BBs for each chain in the list specified by <chains>.
 *
 * ====================================================================
 */
static void
Print_Chains(BBCHAIN *chains)
{
  BBCHAIN *chain;
  INT i;
  BBCHAIN *cold = NULL;

  for (i = 0, chain = chains; chain; chain = chain->next, ++i) {
    if (cold == NULL && BBINFO_cold(chain->head)) {
      fprintf(TFile, "  Start of cold region\n");
    }
    fprintf(TFile, "  [%d]: weight=%g, ", i, chain->weight);
    Print_Chain_BBs(chain);
  }
}


/* ====================================================================
 *
 * Dynamic_Branch_Cost
 *
 * Return the cost of dynamically executing branches in the given BB chain
 * (roughly equivalent to cycles). Also return, by an out-parameter,
 * the number of static fall-throughs (pass NULL if it shouldn't be
 * returned).
 *
 * ====================================================================
 */
static double
Dynamic_Branch_Cost(
  BB *first_bb,
  INT *p_n_stat_fall)
{
  BB *bb;
  BB *bb_next;
  double n_taken = 0.0;
  double n_fall_thru = 0.0;
  INT n_stat_fall = 0;

  for (bb = first_bb; bb; bb = bb_next) {
    double freq;
    BB *succ;

    bb_next = BB_next(bb);

    switch (BBINFO_kind(bb)) {
    case BBKIND_LOGIF:
      succ = BBINFO_succ_bb(bb, 0);
      freq = BB_freq(bb) * BBINFO_succ_prob(bb, 0);
      if (succ == bb_next) {
	n_fall_thru += freq;
	++n_stat_fall;
      } else {
	n_taken += freq;
      }
      succ = BBINFO_succ_bb(bb, 1);
      freq = BB_freq(bb) * BBINFO_succ_prob(bb, 1);
      if (succ == bb_next) {
	n_fall_thru += freq;
	++n_stat_fall;
      } else {
	n_taken += freq;
      }
      break;
    case BBKIND_GOTO:
    case BBKIND_CALL:
      succ = BBINFO_succ_bb(bb, 0);
      if (succ == bb_next) {
	++n_stat_fall;
      } else {
	n_taken += BB_freq(bb);
      }
      break;
    case BBKIND_VARGOTO:
    case BBKIND_INDGOTO:
      /* We don't know the cost of an indirect jump, but it doesn't
       * matter since the cost is constant regardless of how things
       * are ordered.
       */
      break;
    case BBKIND_RETURN:
    case BBKIND_TAIL_CALL:
    case BBKIND_REGION_EXIT:
#if defined(TARG_SL)
    case BBKIND_ZDL_BODY:
    case BBKIND_FORK:
#endif
      break;
    default:
      #pragma mips_frequency_hint NEVER
      FmtAssert(FALSE, ("unhandled BBKIND"));
      /*NOTREACHED*/
    }
  }

  if (p_n_stat_fall) *p_n_stat_fall = n_stat_fall;
  return n_taken * br_taken_cost + n_fall_thru * br_fall_cost;
}


/* ====================================================================
 *
 * Freq_Order_Blocks
 *
 * Order the blocks in a region based on frequency information.
 *
 * ====================================================================
 */
static BOOL
Freq_Order_Blocks(void)
{
  double cost0, cost1;
  INT stat_fall0, stat_fall1;
  INT n_succs;
  EDGE *edges;
  INT n_edges;
  BBCHAIN *chains;
  BB_MAP chain_map;


  /* Find the BBs that hint pragmas indicate are never executed.
   */
  never_bbs = FREQ_Find_Never_BBs(&MEM_local_pool);
  if (CFLOW_Trace_Freq_Order) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "\nBBs 'never' executed as infered by hint pragmas: ");
    BB_SET_Print(never_bbs, TFile);
    fprintf(TFile, "\n");
  }

  /* Snapshot the branch statistics before we start.
   */
  if (CFLOW_Trace_Freq_Order) {
    #pragma mips_frequency_hint NEVER
    cost0 = Dynamic_Branch_Cost(REGION_First_BB, &stat_fall0);
  }

#if defined(TARG_SL)
  //delete the region info in the last pass cg
  if ( !Compiling_Proper_REGION ) {
    for (BB* bb = REGION_First_BB; bb; bb=BB_next(bb)) {
      if(BBINFO_eh_rgn(bb) || BB_handler(bb)) continue;
      BB_rid(bb) = BB_rid(REGION_First_BB);
    }
  }
#endif
  /* Get the max number of succ edges and allocate that many edge structs.
   */
  n_succs = Count_Succ_Edges();
  edges = (EDGE *)alloca(sizeof(EDGE) * n_succs);

  /* Initialize the edges structs.
   */
  n_edges = Init_Edges(edges);

  /* Allocate memory for the chains (at most we'll have one BB per chain).
   * Then create the initial chains.
   */
  if (CFLOW_Trace_Freq_Order) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "\nInit_Chains:\n");
  }
  chains = (BBCHAIN *)alloca(sizeof(BBCHAIN) * (PU_BB_Count + 2));
  chain_map = Init_Chains(chains);

  /* Grow the chains.
   */
  if (CFLOW_Trace_Freq_Order) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "\nGrow_Chains:\n");
  }
  chains = Grow_Chains(chains, edges, n_edges, chain_map);

  if (CFLOW_Trace_Freq_Order) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "\nMerged chains:\n");
    Print_Chains(chains);
  }

  /* Order the chains.
   */
  chains = Order_Chains(chains, chain_map);

  if (CFLOW_Trace_Freq_Order) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "\nOrdered chains:\n");
    Print_Chains(chains);
  }

  /* Combine the chains into a single chain for the region.
   */
  Combine_Chains(chains);

  /* Check branch stats after ordering.
   */
  if (CFLOW_Trace_Freq_Order) {
    #pragma mips_frequency_hint NEVER
    cost1 = Dynamic_Branch_Cost(REGION_First_BB, &stat_fall1);
    fprintf(TFile, "\nBranches %s: cost=%g, stat-fall=%d\n",
		   "before ordering", cost0, stat_fall0);
    fprintf(TFile, "Branches %s: cost=%g, stat-fall=%d\n",
		   "after ordering", cost1, stat_fall1);
    if (cost1 > cost0) {
      DevWarn("Dynamic branch cost increased: before=%g, after=%g",
	      cost0, cost1);
    }
    if (stat_fall0 > stat_fall1) {
      DevWarn("Static branch fall-thrus decreased: before=%d, after=%d",
	      stat_fall0, stat_fall1);
    }
  }

  return TRUE;
}

/* ====================================================================
 * ====================================================================
 *
 * Block cloning routines
 *
 * ====================================================================
 * ====================================================================
 */


/* A description of a cloning candidate.
 */
typedef struct clone_cand {
  struct clone_cand *next;	// next on list of candidates
  BB *pred;			// the pred (GOTO) of the two BBs
#if DEBUG_CFLOW
  float gain;			// freq-weighted gain in cycles of this clone
  float cost;			// instruction increase of this clone
#endif /* DEBUG_CFLOW */
  float metric;			// low-order relative benefit of this clone
  INT metric_class;		// high-order relative benefit of this clone
} CLONE_CAND;


/* Use to track the callee saved regs we estimate the PU will need.
 * Each register class is tracked seperately.
 */
static INT callee_saves[ISA_REGISTER_CLASS_MAX + 1];


/* ====================================================================
 *
 * Estimate_Callee_Saves
 *
 * Perform a crude estimate of how many callee saved registers
 * the current PU will use.
 *
 * The algorithm used here is a rough implementation of what was
 * suggested by Tony:
 *
 *   There are two things that will impact the number of restores in 
 *   return blocks: 1) the actual number of callee saved registers used, 
 *   and 2) shrink wrapping.  you could make some kind of guess at 1 with 
 *   an algorithm similar to what is described above (with the addition 
 *   of frequency weighting and liveness across calls), but there's
 *   not much you can do to account for 2.  here's a wild ass guess at an
 *   algorithm:
 *
 *	1) estimate how many tn's will be allocated to callee saved
 *	   registers because they're live across calls based on
 *	   liveness in call blocks.
 *
 *	2) estimate how many tn's will be allocated to callee saved
 *	   registers because of register pressure in high frequency
 *	   blocks based on liveness in those blocks.
 *
 *	3) take the maximum of these two, and divide by a constant
 *	   to account for shrink wrapping (probably something less
 *	   than 2).
 *
 * ====================================================================
 */
static void
Estimate_Callee_Saves(void)
{
  BB *bb;
  ISA_REGISTER_CLASS rc;

  /* If localizing, then GRA won't be run to improve the situation,
   * so there's no reason to estimate anything -- what we have now is
   * what we get.
   */
  if (CG_localize_tns) return;

  MEM_POOL_Push(&MEM_local_pool);

  BZERO(callee_saves, sizeof(callee_saves));
  for (bb = REGION_First_BB; bb; bb = BB_next(bb)) {
    INT tn_count[ISA_REGISTER_CLASS_MAX + 1];
    TN *tn;
    GTN_SET *live_in;
    GTN_SET *live_out;
    GTN_SET *need_reg;
    BOOL is_call = (BBINFO_kind(bb) == BBKIND_CALL) && BBINFO_nsuccs(bb);

    /* Get the set of GTNs that will need a register in the block.
     */
    live_in = GTN_SET_Intersection(BB_live_in(bb),
				   BB_defreach_in(bb),
				   &MEM_local_pool);
    live_out = GTN_SET_Intersection(BB_live_out(bb),
				    BB_defreach_out(bb),
				    &MEM_local_pool);
    need_reg = GTN_SET_Union(live_in, live_out, &MEM_local_pool);

    /* Count up the number of GTNs that need a register for each
     * register class.
     */
    BZERO(tn_count, sizeof(tn_count));
    for (tn = GTN_SET_Choose(need_reg);
	 tn && tn != GTN_SET_CHOOSE_FAILURE; // gra_live::live_init has brought in some virtual tn that may not there
	 tn = GTN_SET_Choose_Next(need_reg, tn))
    {
      if (tn && !TN_is_save_reg(tn)) {
	rc = TN_register_class(tn);
	++tn_count[rc];
      }
    }

    /* For each register class compute the count of global regs needed
     * and save the maximum of all BBs.
     */
    FOR_ALL_ISA_REGISTER_CLASS(rc) {
      INT count;

      /* Start out assuming we need one global reg for each TN that
       * must be in a reg.
       */
      count = tn_count[rc];

      /* If this is not a call, then we can use the caller saves
       * for so, bias the amount of regs needed by the number of
       * caller saves for this class.
       */
      if (!is_call) {
	INT caller = REGISTER_SET_Size(REGISTER_CLASS_caller_saves(rc));
	count -= caller;
      }

      /* Finally, account for shrink wrapping by dividing by 1.5 and
       * rounding up.
       */
      count = (count * 2 + 2) / 3;

      if (count > callee_saves[rc]) callee_saves[rc] = count;
    }
  }

  MEM_POOL_Pop(&MEM_local_pool);

  if (CFLOW_Trace_Clone) {
    ISA_REGISTER_CLASS rc;
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "\nEstimate_Callee_Saves:\n");
    FOR_ALL_ISA_REGISTER_CLASS(rc) {
      INT count = callee_saves[rc];
      fprintf(TFile, "  %s callee saves: %d\n", REGISTER_CLASS_name(rc), count);
    }
  }
}


/* ====================================================================
 *
 * Estimate_BB_Length
 *
 * Estimate the length (in instructions) of <bb>.
 * If <bb> is an entry or exit BB, attempt to improve the estimate
 * by recognizing that some callee save copies may not be necessary.
 *
 * ====================================================================
 */
static INT32
Estimate_BB_Length(BB *bb)
{
  INT32 cost = BB_length(bb);

  if (!CG_localize_tns && (BB_exit(bb) || BB_entry(bb))) {
    INT callees_needed[ISA_REGISTER_CLASS_MAX + 1];
    OP *op;
    BCOPY(callee_saves, callees_needed, sizeof(callee_saves));
    FOR_ALL_BB_OPs(bb, op) {
      ISA_REGISTER_CLASS rc;
      TN *tn;

      if (!OP_copy(op)) continue;
      if (   !TN_is_save_reg(tn = OP_result(op,0)) 
	  && !TN_is_save_reg(tn = OP_opnd(op,OP_COPY_OPND))) continue;

      rc = TN_register_class(tn);
      if (callees_needed[rc]) {
	--callees_needed[rc];
      } else {
	--cost;
      }
    }
  }

  return cost;
}


/* ====================================================================
 *
 * Create_Sched_Est
 *
 * Create a schedule estimate for <bb> using <pool> for dynamic memory.
 * If <bb> is an entry or exit BB, attempt to improve the estimate
 * by recognizing that some callee save copies may not be necessary.
 *
 * ====================================================================
 */
static CG_SCHED_EST *
Create_Sched_Est(BB *bb, MEM_POOL *pool)
{
  CG_SCHED_EST *se;
  se = CG_SCHED_EST_Create(bb, pool, SCHED_EST_FOR_CLONING);

  if (!CG_localize_tns && (BB_entry(bb) || BB_exit(bb))) {
    INT callees_needed[ISA_REGISTER_CLASS_MAX + 1];
    OP *op;
    BCOPY(callee_saves, callees_needed, sizeof(callee_saves));
    FOR_ALL_BB_OPs(bb, op) {
      ISA_REGISTER_CLASS rc;
      TN *tn;

      if (!OP_copy(op)) continue;
      if (   !TN_is_save_reg(tn = OP_result(op,0)) 
	  && !TN_is_save_reg(tn = OP_opnd(op,OP_COPY_OPND))) continue;

      rc = TN_register_class(tn);
      if (callees_needed[rc]) {
	--callees_needed[rc];
      } else {
	CG_SCHED_EST_Ignore_Op(se, op);
      }
    }
  }

  return se;
}


/* ====================================================================
 *
 * Cloned_Gain
 *
 * Compute the freq-weighted gain, in cycles, of cloning <suc> and
 * appending it to <pred>.
 *
 * ====================================================================
 */
static float
Cloned_Gain(BB *pred, BB *suc)
{
  float uncombined_cycles;
  float combined_cycles;
  float gain;
  CG_SCHED_EST *se_a;
  CG_SCHED_EST *se_b;

  /* Fix up the branch OP to reflect reality so that we get
   * correct schedule estimates.
   */
  Finalize_BB(pred);

  /* Compute the schedule estimate before the proposed cloning.
   */
  MEM_POOL_Push(&MEM_local_nz_pool);
  se_a = Create_Sched_Est(pred, &MEM_local_nz_pool);
  se_b = Create_Sched_Est(suc, &MEM_local_nz_pool);
  uncombined_cycles =   CG_SCHED_EST_Cycles(se_a)
		      + CG_SCHED_EST_Cycles(se_b);

  /* If pred ended in a branch, we can remove it from the cost of the
   * combined BB.
   */
  if (BB_next(pred) != suc) {
    OP *br = BB_branch_op(pred);
    uncombined_cycles += CGTARG_Branch_Taken_Penalty();
    CG_SCHED_EST_Ignore_Op(se_a, br);
  }
  Is_True(uncombined_cycles != 0, ("uncombined_cycles == 0"));

  /* Compute the schedule estimate after the proposed cloning.
   */
  CG_SCHED_EST_Append_Scheds(se_a, se_b);
  combined_cycles = CG_SCHED_EST_Cycles(se_a);
  CG_SCHED_EST_Delete(se_a);
  CG_SCHED_EST_Delete(se_b);
  MEM_POOL_Pop(&MEM_local_nz_pool);

  /* Compute the freq-weighted gain in cycles.
   *
   * Note that BB_freq(pred) is the same as the edge freq pred->suc
   * since pred is a GOTO (with prob == 1.0).
   */
  gain = (uncombined_cycles - combined_cycles) * BB_freq(pred);

  return gain;
}


/* ====================================================================
 *
 * Compare_Clone_Cands
 *
 * qsort comparison function for sorting clone candidates. The sort
 * key is the metric assigned to the candidate. We sort in descending
 * order, i.e. larger metrics come first.
 *
 * We perform a two level sort. We first consider the metric_class. If
 * they are different, the metric_class defines the ordering. Otherwise,
 * the metric defines the ordering.
 *
 * ====================================================================
 */
static INT
Compare_Clone_Cands(const void *p1, const void *p2)
{

  /* qsort claims to sort things in ascending order, but in reality
   * it is sorting based on how this comparison function classifies
   * the relationship. Define some constants that make that clear.
   */
  enum {
    sort_1_before_2 = -1, 
    sort_1_after_2  = 1, 
    sort_1_same_2   = 0 
  };
  CLONE_CAND *c1 = *(CLONE_CAND **)p1;
  CLONE_CAND *c2 = *(CLONE_CAND **)p2;

  INT metric_class1 = c1->metric_class;
  INT metric_class2 = c2->metric_class;
  if (metric_class1 > metric_class2) {
    return sort_1_before_2;
  } else if (metric_class1 < metric_class2) {
    return sort_1_after_2;
  }

  float metric1 = c1->metric;
  float metric2 = c2->metric;
  if (metric1 > metric2) {
    return sort_1_before_2;
  } else if (metric1 < metric2) {
    return sort_1_after_2;
  } else {
    return sort_1_same_2;
  }
}


/* ====================================================================
 *
 * Sort_Clone_Cands
 *
 * Sort the clone candidates according to _metric and _metric_class.
 * This results in a list of candidates with what we believe to be
 * the most profitable candidates first on the list.
 *
 * ====================================================================
 */
static CLONE_CAND *
Sort_Clone_Cands(CLONE_CAND *cands, INT n_cand)
{
  CLONE_CAND **candvec;
  INT i;
  CLONE_CAND *cand;

  if (n_cand <= 1) return cands;

  /* Build an array of pointers to the candidates.
   */
  candvec = (CLONE_CAND **)alloca(sizeof(CLONE_CAND *) * n_cand);
  for (cand = cands, i = 0; i < n_cand; cand = cand->next, ++i) {
    candvec[i] = cand;
  }

  /* Sort the array of pointers
   */
  qsort(candvec, n_cand, sizeof(CLONE_CAND *), Compare_Clone_Cands);

  /* Reconstruct the list of candidates, now in sorted order.
   */
  for (i = 0; i < n_cand - 1; ++i) candvec[i]->next = candvec[i + 1];
  candvec[n_cand - 1]->next = NULL;

  return candvec[0];
}


/* ====================================================================
 *
 * Clone_Blocks
 *
 * Clone BBs where profitable to improve scheduling.
 *
 * The return value indicates if we made any changes.
 *
 * ====================================================================
 */
static BOOL
Clone_Blocks ( BOOL in_cgprep )
{
  INT i;
  BOOL changed;
  BB *bp;
  INT npred;
  INT allowance;
  CLONE_CAND *cand;
  CLONE_CAND *cands = NULL;
  INT n_cand = 0;
  INT total_inst_incr = 0;
  INT pu_size = 0;

  /* Estimate the number of callee saves used by the PU so
   * we can estimate how many of the save_tn copies in entry and
   * exit blocks will remain.
   */
  Estimate_Callee_Saves();

  /* First we make a pass over the BBs to find all the candidates for
   * cloning. For each candidate we compute a metric that identifies
   * its relative profitability.
   */
  for (bp = REGION_First_BB; bp; bp = BB_next(bp)) {

#ifdef TARG_IA64
      if(IPFEC_Enable_Region_Formation && RGN_Formed){
         if(Home_Region(bp)->Is_No_Further_Opt())
             continue;
      }
#endif
    INT n_local_cand;
    BBLIST *edge;
    INT32 bb_length_est = Estimate_BB_Length(bp);

    /* Keep a running total of the instruction count of the BBs.
     */
    pu_size += bb_length_est;

    /* Skip this as a candidate if we don't like its BBKIND.
     * This is actually a profitability test, we are capable
     * of cloning all suc BBKINDs.
     */
    switch (BBINFO_kind(bp)) {
    case BBKIND_RETURN:
    case BBKIND_VARGOTO:
    case BBKIND_INDGOTO:
    case BBKIND_TAIL_CALL:
      break;
    default:
      continue;
    }

    /* Loop over each of bp's preds, and if it's a GOTO, see if
     * it is a contender.
     */
    npred = 0;
    n_local_cand = 0;
    FOR_ALL_BB_PREDS(bp, edge) {
      BB *pred = BBLIST_item(edge);
#ifdef TARG_IA64
      if(IPFEC_Enable_Region_Formation && RGN_Formed){
         if(Home_Region(pred)->Is_No_Further_Opt())
             continue;
      }
#endif
      ++npred;
      if (BB_length(pred) && BBINFO_kind(pred) == BBKIND_GOTO) {

	/* If we move a part of a loop outside the loop it might
	 * cause the trip count TN for the loop to be wrong.
	 */
	if (BB_loop_head_bb(pred) != BB_loop_head_bb(bp)) continue;

	if (!Can_Append_Succ(pred, bp, NULL, FALSE, CFLOW_Trace_Clone)) {
	  continue;
	}

	/* Add another candidate to the list.
	 */
	cand = (CLONE_CAND *)alloca(sizeof(CLONE_CAND));
	cand->pred = pred;
	cand->next = cands;
	cands = cand;
	++n_local_cand;
      }
    }

    /* If bp had one or more candidates, compute their gain, cost and
     * ultimately their profitability metric.
     */
    if (n_local_cand != 0) {
      float rebate;

      /* If all of bp's preds are candidates and we clone them all,
       * then we will be able to remove bp. That would lower the cost
       * of the cloning. This is an NP complete problem since we can't
       * know the cost until we perform the clones, but the cost affects
       * the cloning decisions. Therefore we just make a token
       * acknowledgement of the possible savings by reducing the cost
       * of each candidate by the average amount saved by removing bp
       */
      rebate =   (n_local_cand == npred) 
	       ? ((float)bb_length_est / (float)n_local_cand) : 0.0;

      /* Compute the profitability components of each candidate.
       */
      cand = cands;
      i = n_local_cand;
      do {
	INT metric_class;
	BB *pred = cand->pred;
        BOOL pred_had_branch = (BB_next(pred) != bp);
	float cost = bb_length_est - pred_had_branch - rebate;
	float gain = Cloned_Gain(pred, bp);

#if DEBUG_CFLOW
	cand->cost = cost + rebate;
	cand->gain = gain;
#endif /* DEBUG_CFLOW */

	/* The metric class is the first level profitability metric,
	 * with higher classes being more profitable, and is derived
	 * from the gain.
	 */
	metric_class = 2;
	if (gain == 0.0 && cost != 0.0) {
	  float freq = BB_freq(pred);
	  if (freq != 0.0) {
	    gain = freq;
	    metric_class = 1;
	  } else {
	    gain = 1.0;
	    metric_class = 0;
	  }
	}

	cand->metric_class = metric_class;
	cand->metric = gain / cost;
      } while (cand = cand->next, --i);

      n_cand += n_local_cand;
    }
  }

  /* Sort the candidates based on the metrics.
   */
  cands = Sort_Clone_Cands(cands, n_cand);
  if (CFLOW_Trace_Clone) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "\nSorted candidates:\n");
    for (cand = cands, i = 0; i < n_cand; cand = cand->next, ++i) {
      BB *pred = cand->pred;
      BB *suc = BBINFO_succ_bb(pred, 0);
      fprintf(TFile, "  [%d]: BB:%d->BB:%d"
#if DEBUG_CFLOW
		     ", gain=%g, cost=%g"
#endif /* DEBUG_CFLOW */
		     ", class=%d, metric=%g\n",
		     i, BB_id(pred), BB_id(suc),
#if DEBUG_CFLOW
		     cand->gain, cand->cost, 
#endif /* DEBUG_CFLOW */
		     cand->metric_class, cand->metric);
    }
  }

  /* Now perform the actual cloning. We do so by cloning the most
   * profitable candidates until we exceed a certain increase in
   * the number of instructions.
   */
  allowance = (pu_size * CFLOW_clone_incr + 99) / 100;
  if (allowance < CFLOW_clone_min_incr) allowance = CFLOW_clone_min_incr;
  if (allowance > CFLOW_clone_max_incr) allowance = CFLOW_clone_max_incr;

  if (CFLOW_Trace_Clone) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "\ninstruction allowance=%d (%d%% of PU size %d is %d)\n\n",
		    allowance, CFLOW_clone_incr, pu_size,
		    (pu_size * CFLOW_clone_incr + 99) / 100);
  }

  changed = FALSE;
  for (cand = cands, i = 0; i < n_cand; cand = cand->next, ++i) {
    INT cost;
    BB *pred = cand->pred;
    BB *suc = BBINFO_succ_bb(pred, 0);

#ifdef TARG_IA64
    if(RGN_Formed && Home_Region(pred) != Home_Region(suc) &&
       BB_succs_len(suc) > 2)
       cost = CFLOW_clone_max_incr;
    else {
      cost = BB_Has_One_Pred(suc) ? 0 : Estimate_BB_Length(suc);
      cost -= (BB_next(pred) != suc);
    }
#else
    cost = BB_Has_One_Pred(suc) ? 0 : Estimate_BB_Length(suc);
    cost -= (BB_next(pred) != suc);
#endif

    if (cost <= allowance) {
      Append_Succ(pred, suc, in_cgprep, BB_Has_One_Pred(suc));
      changed = TRUE;
      if (CFLOW_Trace_Clone) {
	#pragma mips_frequency_hint NEVER
	fprintf(TFile, "appended BB:%d to BB:%d\n", BB_id(suc), BB_id(pred));
      }

      /* If we've now cloned the block into all its predecessors,
       * we can remove it.
       */
      if (!BB_preds(suc)) {
	Delete_BB(suc, CFLOW_Trace_Clone);
      }

      allowance -= cost;
      total_inst_incr += cost;
    }
  }

  if (changed && CFLOW_Trace_Clone) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "\ncloning added %d instruction%s (%3.2f%% of PU size %d)\n",
		   total_inst_incr,
		   total_inst_incr == 1 ? "" : "s",
		   (total_inst_incr * 100.0F) / pu_size,
		   pu_size);
  }

  return changed;
}

#ifdef TARG_IA64
static void
Adjust_Branch_Hint(void)
{
    BB *bp;
    BB *next;
    for (bp = REGION_First_BB; bp; bp = next) {
        TN *enum_tn;
        OP *br_op;
        float fall_through_prob,target_prob;
        BB *fall_through;
        BB *target;
        next = BB_next(bp);
        br_op = BB_xfer_op(bp);
        if (br_op != NULL) {
            switch (OP_code(br_op)) {
            case TOP_br_cond:
            case TOP_br_r_cond:  
                fall_through = BBINFO_succ_bb(bp, 1);
                fall_through_prob = BBINFO_succ_prob(bp, 1);
                target = BBINFO_succ_bb(bp, 0);
                target_prob = BBINFO_succ_prob(bp, 0);
                if (fall_through_prob > target_prob) {
                    enum_tn = Gen_Enum_TN(ECV_bwh_dpnt);
                } else {
                    enum_tn = Gen_Enum_TN(ECV_bwh_dptk);
                }
                Set_OP_opnd(br_op, 1, enum_tn);
                break;
            default:
                break;
            }
        }
    }        
}

#endif

/* ====================================================================
 * ====================================================================
 *
 * Exported interfaces.
 *
 * ====================================================================
 * ====================================================================
 */


/* ====================================================================
 *
 * CFLOW_Optimize
 *
 * See interface description.
 *
 * ====================================================================
 */
void
CFLOW_Optimize(INT32 flags, const char *phase_name)
{
  BOOL change;
  const char *prev_phase;
  BOOL flow_change = FALSE;

  if (!CFLOW_Enable) return;

  /* Setup error phase.
   */
  if (phase_name == NULL || *phase_name == '\0') phase_name = "CFLOW";
  prev_phase = Get_Error_Phase();
  Set_Error_Phase(phase_name);
  Start_Timer(T_CFLOW_CU);

  /* Get flag settings for this invocation.
   */
  current_flags = flags & ~disabled_flags;
#if defined(TARG_SL)
  if(((current_flags & CFLOW_COLD_REGION) && CFLOW_cold_threshold))
    current_flags |= CFLOW_FREQ_ORDER;
#endif

  if (CFLOW_Trace) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "\n%s"
		   " %s -- before optimization\n"
		   "(PU \"%s\")\n"
		   "%s",
		   DBar, phase_name, Cur_PU_Name, DBar);

    fprintf(TFile, "\nFlags for current invocation: 0x%08x\n", current_flags);
    fprintf(TFile, "  unreachable block removal:\t%s\n",
	    (current_flags & CFLOW_UNREACHABLE) ? "enabled" : "disabled");
    fprintf(TFile, "  branch optimization:\t\t%s\n",
	    (current_flags & CFLOW_BRANCH) ? "enabled" : "disabled");
    fprintf(TFile, "  block merging:\t\t%s\n",
	    (current_flags & CFLOW_MERGE) ? "enabled" : "disabled");
    fprintf(TFile, "  block reordering:\t\t%s\n",
	    (current_flags & CFLOW_REORDER) ? "enabled" : "disabled");
    fprintf(TFile, "  freq-based block reordering:\t%s\n",
	    (current_flags & CFLOW_FREQ_ORDER) ? "enabled" : "disabled");
    fprintf(TFile, "  block cloning:\t\t%s\n",
	    (current_flags & CFLOW_CLONE) ? "enabled" : "disabled");
    fprintf(TFile, "  optimize all br to bcond:\t%s\n",
	    (current_flags & CFLOW_OPT_ALL_BR_TO_BCOND) ? "enabled" : "disabled");
    fprintf(TFile, "  fill delay slots:\t\t%s\n",
	    (current_flags & CFLOW_FILL_DELAY_SLOTS) ? "enabled" : "disabled");
#if defined (TARG_SL)
    fprintf(TFile, "  cold region:\t\t%s\n",
	    (current_flags & CFLOW_COLD_REGION) ? "enabled" : "disabled");
#endif

    fprintf(TFile, "\n");
    Print_All_BBs();
  }

  /* Skip it all if nothing to do.
   */
  if (current_flags == 0) goto nothing;

  /* No need to make this function call a million times.
   */
  freqs_computed = FREQ_Frequencies_Computed();

  /* Misc inits.
   */
  listvar_counts = NULL;
  deleted_bbs = NULL;
  eh_label_removed = FALSE;

  /* Start a new local memory allocation space
   */
  MEM_POOL_Push(&MEM_local_nz_pool);

  /* Create BBINFO structures for all the BBs.
   */
  if (!Initialize_BB_Info()) goto done;

  // Reset the mapping between BBs and hyperblocks.
  Setup_HB_bb_map();


  if (CFLOW_Trace_Detail) {
    #pragma mips_frequency_hint NEVER
    Print_Cflow_Graph("CFLOW_Optimize flow graph -- before optimization");
  }

  if (current_flags & CFLOW_BRANCH) {
    if (CFLOW_Trace_Branch) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "\n%s CFLOW_Optimize: optimizing branches\n%s",
		     DBar, DBar);
    }
    change = Optimize_Branches();
    if (CFLOW_Trace_Branch) {
      #pragma mips_frequency_hint NEVER
      if (change) {
	Print_Cflow_Graph("CFLOW_Optimize flow graph -- after optimizing branches");
      } else {
	fprintf(TFile, "No changes.\n");
      }
    }
    flow_change |= change;
  }

  if (current_flags & CFLOW_UNREACHABLE) {
    if (CFLOW_Trace_Unreach) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "\n%s CFLOW_Optimize: delete unreachable blocks\n%s",
		     DBar, DBar);
    }
    change = Delete_Unreachable_Blocks(); 
    if (CFLOW_Trace_Unreach) {
      #pragma mips_frequency_hint NEVER
      if (change) {
	Print_Cflow_Graph("CFLOW_Optimize flow graph -- after deleting unreachable blocks");
      } else {
	fprintf(TFile, "No changes.\n");
      }
    }
    flow_change |= change;
  }

  if (current_flags & CFLOW_REORDER) {
    if (CFLOW_Trace_Reorder) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "\n%s CFLOW_Optimize: reorder blocks\n%s",
		     DBar, DBar);
      Print_Fall_Thrus("before Reorder_Blocks");
    }
    change = Reorder_Blocks();

    if (CFLOW_Trace_Reorder) {
      #pragma mips_frequency_hint NEVER
      if (change) {
	Print_Fall_Thrus("after Reorder_Blocks");
	Print_Cflow_Graph("CFLOW_Optimize flow graph -- after reordering blocks");
      } else {
	fprintf(TFile, "No changes.\n");
      }
    }
    flow_change |= change;
  }

  if ((current_flags & CFLOW_FREQ_ORDER) && freqs_computed) {
    if (CFLOW_Trace_Freq_Order) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "\n%s CFLOW_Optimize: frequency-guided reordering of blocks\n%s",
		     DBar, DBar);
    }
    change = Freq_Order_Blocks();

    if (CFLOW_Trace_Freq_Order) {
      #pragma mips_frequency_hint NEVER
      if (change) {
	Print_Cflow_Graph("CFLOW_Optimize flow graph -- after frequency-guided reordering blocks");
      } else {
	fprintf(TFile, "No changes.\n");
      }
    }
    flow_change |= change;
  }

  if (current_flags & CFLOW_MERGE) {
    if (CFLOW_Trace_Merge) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "\n%s CFLOW_Optimize: merge blocks\n%s",
		     DBar, DBar);
    }
    change = Merge_Blocks(current_flags & CFLOW_IN_CGPREP);
  
    if (CFLOW_Trace_Merge) {
      #pragma mips_frequency_hint NEVER
      if (change) {
	Print_Cflow_Graph("CFLOW_Optimize flow graph -- after merging blocks");
      } else {
	fprintf(TFile, "No changes.\n");
      }
    }
    flow_change |= change;
  }

  if (freqs_computed && (current_flags & CFLOW_CLONE)) {
    if (CFLOW_Trace_Clone) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "\n%s CFLOW_Optimize: clone blocks\n%s",
		     DBar, DBar);
    }
    change = Clone_Blocks(current_flags & CFLOW_IN_CGPREP);
    
    if (CFLOW_Trace_Clone) {
      #pragma mips_frequency_hint NEVER
      if (change) {
	Print_Cflow_Graph("CFLOW_Optimize flow graph -- after cloning blocks");
      } else {
	fprintf(TFile, "No changes.\n");
      }
    }
    flow_change |= change;
  }

#ifdef TARG_X8664
  if (current_flags & CFLOW_BR_FUSE) {
    Br_Fuse_All_BBs();
  }
#endif

  /* If we made any flow changes, re-create the preds and succs lists.
   */
  if (flow_change) Finalize_All_BBs();

#ifdef TARG_IA64  
  if (strcmp(phase_name,"CFLOW (third pass)") == 0) {
      Adjust_Branch_Hint();
  }
#endif
  if (CFLOW_Trace) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "\n%s"
		   " %s -- after optimization\n"
		   "(PU \"%s\")\n"
		   "%s\n",
		   DBar, phase_name, Cur_PU_Name, DBar);
    if (flow_change) {
      Print_All_BBs();
    } else {
      fprintf(TFile, "No changes.\n");
    }
  }

done:
  BB_MAP_Delete(bb_info_map);
  MEM_POOL_Pop(&MEM_local_nz_pool);

nothing:
  Check_for_Dump(TP_FLOWOPT, NULL);
  Stop_Timer(T_CFLOW_CU);
  Set_Error_Phase(prev_phase);
}


/* ====================================================================
 *
 * CFLOW_Initialize
 *
 * See interface description.
 *
 * ====================================================================
 */
void
CFLOW_Initialize(void)
{
  INT32 flags;
  INT32 idummy;
  INT32 fixed, taken;
  double ddummy;

  CGTARG_Compute_Branch_Parameters(&idummy, &fixed, &taken, &ddummy);
  br_taken_cost = fixed + taken;
  br_fall_cost = fixed;

#ifdef TARG_MIPS
  /* If we are optimizing for space, or if we are using estimated
     profiles, then we make the static cost for branches high so that
     we avoid adding unconditional branches. When optimizing for
     space, we also set the 'heuristic_tolerance' so that frequency
     information is ignored during layout (since this results in fewer
     unconditional jumps). */

  if (OPT_Space || !CG_PU_Has_Feedback)
  {
    br_static_cost = 1e+07;
    if (OPT_Space)
      heuristic_tolerance = 1.0;
  }
  else
  {
    br_static_cost = 100;
  }
#else
  br_static_cost = 0;
#endif

  if (Get_Trace(TP_FLOWOPT, 0xffffffff)) {
    #pragma mips_frequency_hint NEVER
    CFLOW_Trace	= Get_Trace(TP_FLOWOPT, TRACE_CFLOW);
    CFLOW_Trace_Detail = Get_Trace(TP_FLOWOPT, TRACE_DETAIL);
    CFLOW_Trace_Reorder = Get_Trace(TP_FLOWOPT, TRACE_REORDER);
    CFLOW_Trace_Branch = Get_Trace(TP_FLOWOPT, TRACE_BRANCH);
    CFLOW_Trace_Unreach = Get_Trace(TP_FLOWOPT, TRACE_UNREACH);
    CFLOW_Trace_Merge = Get_Trace(TP_FLOWOPT, TRACE_MERGE);
    CFLOW_Trace_Clone = Get_Trace(TP_FLOWOPT, TRACE_CLONE);
    CFLOW_Trace_Freq_Order = Get_Trace(TP_FLOWOPT, TRACE_FREQ_ORDER);
    CFLOW_Trace_Freq = Get_Trace(TP_FLOWOPT, TRACE_FREQ);
    CFLOW_Trace_Dom = Get_Trace(TP_FLOWOPT, TRACE_DOM);
#ifdef TARG_IA64
    CFLOW_Trace_Empty_BB_Elim = Get_Trace(TP_FLOWOPT, TRACE_Empty_BB_Elim);
#endif

    CFLOW_Trace_Branch |= CFLOW_Trace_Detail;
    CFLOW_Trace_Unreach |= CFLOW_Trace_Detail;
    CFLOW_Trace_Merge |= CFLOW_Trace_Detail;
    CFLOW_Trace_Reorder |= CFLOW_Trace_Detail;
    CFLOW_Trace_Clone |= CFLOW_Trace_Detail;
    CFLOW_Trace_Freq_Order |= CFLOW_Trace_Detail;
#ifdef TARG_IA64
    CFLOW_Trace_Empty_BB_Elim |= CFLOW_Trace_Detail;
#endif
  }

  if (CFLOW_heuristic_tolerance && CFLOW_heuristic_tolerance[0] != '\0') {
    double d = atof(CFLOW_heuristic_tolerance);
    if (d >= 0.0 && d <= 1.0) {
      heuristic_tolerance = d;
    } else {
      DevWarn("cflow heuristic tolerance (%s) must be between 0 and 1",
	      CFLOW_heuristic_tolerance);
    }
  }

  if (CFLOW_feedback_tolerance && CFLOW_feedback_tolerance[0] != '\0') {
    double d = atof(CFLOW_feedback_tolerance);
    if (d >= 0.0 && d <= 1.0) {
      feedback_tolerance = d;
    } else {
      DevWarn("cflow feedback tolerance (%s) must be between 0 and 1",
	      CFLOW_feedback_tolerance);
    }
  }

  if (CFLOW_cold_threshold && CFLOW_cold_threshold[0] != '\0') {
    double d = atof(CFLOW_cold_threshold);
    if (d >= 0.0) {
      cold_threshold = d;
    } else {
      DevWarn("cflow cold region threshold (%s) must be non-negative",
	      CFLOW_cold_threshold);
    }
  } else {
    cold_threshold = Gen_PIC_Shared ? 0.005 : 0.01;
  }

  flags = 0;
  if (!CFLOW_Enable_Unreachable) flags |= CFLOW_UNREACHABLE;
  if (!CFLOW_Enable_Branch) flags |= CFLOW_BRANCH;
  if (!CFLOW_Enable_Merge) flags |= CFLOW_MERGE;
  if (!CFLOW_Enable_Reorder) flags |= CFLOW_REORDER;
  if (!CFLOW_Enable_Clone) flags |= CFLOW_CLONE;
  if (!CFLOW_Enable_Freq_Order) flags |= CFLOW_FREQ_ORDER;
  if (!CFLOW_opt_all_br_to_bcond) flags |= CFLOW_OPT_ALL_BR_TO_BCOND;
  disabled_flags = flags;
}

#ifdef TARG_IA64
void 
CFLOW_Process(void)
{BB *bp;
 for (bp = REGION_First_BB; bp; bp = BB_next(bp)) {
        
        if (IPFEC_Enable_Region_Formation && RGN_Formed) {
            if(Home_Region(bp)->Is_No_Further_Opt())
                continue;
        }
        if (BB_length(bp)==0){
        	BBLIST *list;
        	if (BB_preds_len(bp)==1){
        		list=BB_preds(bp);
                BB *pred=BBLIST_item(list);
                BBLIST *lists; 
                //FOR_ALL_BB_SUCCS(bp,lists) {
                for (lists = BB_succs(bp); lists != NULL;) {
                    BB *succ = BBLIST_item(lists);
                    lists = BBLIST_next(lists);
                    BBLIST *blsucc = BB_Find_Succ(bp, succ);
                    float prob=BBLIST_prob(blsucc);
                    RGN_Link_Pred_Succ_With_Prob(pred,succ,prob);
                    RGN_Unlink_Pred_Succ(bp,succ);
                }
                RGN_Unlink_Pred_Succ(pred,bp);
        	}else if (BB_succs_len(bp)==1){
                list=BB_succs(bp);
                BB *succ=BBLIST_item(list);
                BBLIST *lists; 
                //FOR_ALL_BB_SUCCS(bp,lists) {
                for (lists = BB_preds(bp); lists != NULL;) {
                    BB *pred = BBLIST_item(lists);
                    lists = BBLIST_next(lists);
                    BBLIST *blsucc = BB_Find_Succ(pred,bp);
                    float prob=BBLIST_prob(blsucc);
                    RGN_Link_Pred_Succ_With_Prob(pred,succ,prob);
                    RGN_Unlink_Pred_Succ(pred,bp);
                }
                RGN_Unlink_Pred_Succ(bp,succ);
           }
 	}
}
 for (bp = REGION_First_BB; bp; bp = BB_next(bp)) {
        
        if (IPFEC_Enable_Region_Formation && RGN_Formed) {
            if(Home_Region(bp)->Is_No_Further_Opt())
                continue;
        }
        if (BB_length(bp)==1 && BB_xfer_op(bp)){
           	BBLIST *list;
            OP *op=BB_xfer_op(bp);
            if (OP_opnd(op,0)==True_TN && !(BB_has_label(bp))){
        	    if (BB_succs_len(bp)>1) Is_True(bp,("br BB has 2 succ"));
                if (BB_succs_len(bp)==1){ 
                    list=BB_succs(bp);
                    BB *succ=BBLIST_item(list);
                    BBLIST *lists; 
                   
                    for (lists = BB_preds(bp); lists != NULL;) {
                        BB *pred = BBLIST_item(lists);
                        lists = BBLIST_next(lists);
                        BBLIST *blsucc = BB_Find_Succ(pred,bp);
                        float prob=BBLIST_prob(blsucc);
                        RGN_Link_Pred_Succ_With_Prob(pred,succ,prob);
                        RGN_Unlink_Pred_Succ(pred,bp);
                    }
                    RGN_Unlink_Pred_Succ(bp,succ);
                }
            }
        }
 	}
}


/* ====================================================================
 *
 * Initialize_BB_Info_For_Delete
 *
 * Only process the info for those BB who are LOGIF and GOTO
 * This function is to prepare the information for delete empty BB
 * before emitting the file 
 * 
 *
 * ====================================================================
 */
static BOOL
Initialize_BB_Info_For_Delete(void)
{
  BB *bb;

  bb_info_map = BB_MAP_Create();
  for (bb = REGION_First_BB; bb; bb = BB_next(bb)) {
    BBINFO *bbinfo;
    INT bbinfo_size;
    BBKIND bbkind = BB_kind(bb);

    /* consider the chk bb to be the LOGIF bb */
    if (bbkind == BBKIND_UNKNOWN) {
       if (BB_Last_chk_op(bb)) {
          bbkind = BBKIND_LOGIF;
       }
    }
    /* process the bbinfo structure with this BB.
     */
    bbinfo_size = sizeof(BBINFO);
    bbinfo = (BBINFO *)MEM_POOL_Alloc(&MEM_local_nz_pool, bbinfo_size);
    BB_MAP_Set(bb_info_map, bb, bbinfo);

    bbinfo->kind = bbkind;

    switch (bbkind) {
    case BBKIND_GOTO:
      bbinfo->nsuccs = BB_succs(bb) ? 1 : 0;
      if (BB_succs(bb)) {
	TN *lab_tn;
	BB *target = BBLIST_item(BB_succs(bb));

	bbinfo->succs[0].bb = target;
	bbinfo->succs[0].offset = 0;
	bbinfo->succs[0].prob = 1.0;
      }
      continue;

    case BBKIND_LOGIF:
    case BBKIND_CHK:// bug fix for OSP_104, OSP_105, OSP_192
      {
	INT tfirst;
	INT tcount;
	TN *lab_tn;
	BB *target;
	BBLIST *target_edge;
	BBLIST *fall_through_edge;
	OP *br = BB_branch_op(bb);
        if ((br == NULL)|| (OP_noop(br)) )
        {
           br = BB_Last_chk_op(bb);  
        }

	/* Get the targets. Note that target[0] is always the "true" target.
	 */
	target_edge = BB_succs(bb);
	fall_through_edge = BBLIST_next(target_edge);
	target = BBLIST_item(target_edge);
	if (fall_through_edge == NULL) {
	  fall_through_edge = target_edge;
	} else if (target == BB_next(bb)) {
	  target_edge = fall_through_edge;
	  fall_through_edge = BB_succs(bb);
	  target = BBLIST_item(target_edge);
	}

	bbinfo->nsuccs = 2;

	bbinfo->succs[0].bb = target;
	bbinfo->succs[0].offset = 0;
	bbinfo->succs[0].prob = BBLIST_prob(target_edge);

	bbinfo->succs[1].bb = BB_next(bb);
	bbinfo->succs[1].offset = 0;
	bbinfo->succs[1].prob = BBLIST_prob(fall_through_edge);

	CGTARG_Branch_Info(br, &tfirst, &tcount);
	Is_True(tcount == 1, ("unexpected number of branch targets in BB:%d",
                BB_id(bb)));
	lab_tn = OP_opnd(br, tfirst);
	continue;
      }

    }

  }

  return TRUE;
}
/* ====================================================================
 *
 * CFLOW_Delete_Empty_BB
 * 
 *
 * Search all the BBs , if a empty bb is founded, the previous bbs of it 
 * will be handled . If previous bb is LOGIF and GOTO, the corresponding
 * annotation which includes the label of empty bb will be copied to
 * the succ bb. 
 * 
 *
 * ====================================================================
 */       	
void
CFLOW_Delete_Empty_BB(void)
{
  BB *bp ,*tgt_succ;
  BB *left_tgt, *right_tgt;
  INT tgt_num;
  LABEL_IDX tgt_label;
  float prob;
  BB *next_bb;
  MEM_POOL_Push(&MEM_local_nz_pool);
  if (!Initialize_BB_Info_For_Delete()) {
      BB_MAP_Delete(bb_info_map);
      MEM_POOL_Pop(&MEM_local_nz_pool);
      return;
  }
  for (bp = REGION_First_BB ; bp!=NULL ; bp= next_bb) {
      next_bb =BB_next(bp) ;
      if ( BBINFO_kind(bp) == BBKIND_GOTO && !BB_length(bp) ) {
         BBLIST *prev_bbs, *next_bbs;
         BB *prev_bb;
         BOOL can_do ;
         LABEL_IDX old_label, tgt_lable;

	 // BB with EH Range labels can not be removed, even
	 // though its length is 0 and it has no succ,
	 // coz these labels are required by LSDA construction.
	 // bug fix for OSP_350
         if (BB_Has_Exc_Label(bp)
		|| BB_Has_Addr_Taken_Label(bp)
		|| BB_Has_Outer_Block_Label(bp))
	   		continue;

	 // Be caution to the empty GOTO BB bp, if BB_next(bp) == NULL;
	 // In this situation, we can remove it only when none of it's
	 // predecessors is GOTO or LOGIF.
	 // bug fix for OSP_208
	 //
	 if (BBINFO_nsuccs(bp) == 0 && next_bb == NULL) {
	   BOOL is_removeable = TRUE;
	   for ( prev_bbs = BB_preds(bp); prev_bbs != NULL; prev_bbs = next_bbs) {
	     next_bbs = BBLIST_next(prev_bbs);
	     BB *prev_bb = BBLIST_item(prev_bbs);
	     switch BBINFO_kind(prev_bb) {
	     	case BBKIND_GOTO :
	     	case BBKIND_LOGIF:
			is_removeable = FALSE;
			break;

		default:
			continue;
	     } 
	   }
	   if (is_removeable) {
	     Delete_BB(bp, CFLOW_Trace_Empty_BB_Elim);
	   }
	   continue;
	 }

	 Is_True (BBINFO_nsuccs(bp), ("GOTO BB: %d has no succ bb", BB_id(bp)));
	 Is_True (BBINFO_succ_bb(bp, 0) != bp, ("GOTO BB: %d is a loop~!", BB_id(bp)));
	 
	 old_label = Gen_Label_For_BB(bp);
         tgt_succ = BBINFO_succ_bb(bp, 0);
         /* if the tgt bb has no label , 
          * the previous bb must fall though to it. 
          */

         tgt_label = Gen_Label_For_BB(tgt_succ);
         if (CFLOW_Trace_Empty_BB_Elim) {
            fprintf(TFile, "The label %s of empty BB: %d will be copied "
			    "to BB labeled with %s\n", LABEL_name(old_label),
                            BB_id(bp), LABEL_name(tgt_label));
         }
         /* fetch all the previous bb of bp */

         for ( prev_bbs = BB_preds(bp); prev_bbs != NULL; prev_bbs = next_bbs ) {
             can_do = TRUE;
             next_bbs = BBLIST_next(prev_bbs);
             BB *prev_bb = BBLIST_item(prev_bbs) ;

             switch BBINFO_kind(prev_bb) {

             case BBKIND_GOTO :
                  tgt_num = 0;
                  break;

             case BBKIND_LOGIF: 
                  if ((left_tgt = BBINFO_succ_bb(prev_bb, 0)) == bp) {
                     tgt_num = 0;
                  }
                  if ((right_tgt = BBINFO_succ_bb(prev_bb, 1)) == bp) {
                     tgt_num = 1;
                  }
                  break;
                  

             default: 
                      /*  do not handle BB of other kinds  */
                      can_do = FALSE;
                      continue;

             } 

             /* fetch the label of target bb, if needed , 
              * set the target of previous bb to new label
              */
             if (can_do) {

                /* handle the prob and freq
                 * the prob of tgt bb should be the prob from prev bb to bp
                 * the freq of tgt bb should keep intact
                 */
             
                prob = BBINFO_succ_prob(prev_bb,tgt_num);
                Unlink_Pred_Succ(prev_bb, bp);
                Link_Pred_Succ_with_Prob(prev_bb, tgt_succ, prob, FALSE, TRUE);
             } 
         }
         /* if bp has annotation , cp all to the tgt_succ 
          * if bp has no annotation, delete it directly
          */
         BB_Copy_All_Annotations (tgt_succ, bp);
         Delete_BB(bp, CFLOW_Trace_Empty_BB_Elim);
      }
  } 
  BB_MAP_Delete(bb_info_map);
  MEM_POOL_Pop(&MEM_local_nz_pool);
}
#endif

#if defined(KEY) && (defined(TARG_MIPS) && !defined(TARG_SL))


// Fix for bugs 8748 and 11720:  Build a long jump using the jr instruction.
// Save and restore own temp register.
static void
Build_Long_Goto(BB *targ_bb, OPS *ops)
{
  // Build this sequence:
  //   sp = sp - 8      ; grow stack segment before storing r2
  //   store r2,0(sp)   ; arbitrarily pick r2 to be tmp_reg
  //   r2 = target_label
  //   sp = sp + 8
  //   jr r2
  //   r2 = load -8(sp) ; restore r2

  TN *tmp_reg = Build_Dedicated_TN(ISA_REGISTER_CLASS_integer, 3, 8);
  LABEL_IDX label_idx = Gen_Label_For_BB(targ_bb);

  // 11720: Create st that matches the label name that we can then use with
  // relocations. (Code modified from LDA_LABEL in be/cg/whirl2ops.cxx)
  ST *st = New_ST(CURRENT_SYMTAB);
  ST_Init(st, Save_Str(LABEL_name(label_idx)), CLASS_NAME,
          SCLASS_UNKNOWN, EXPORT_LOCAL, MTYPE_To_TY(Pointer_Mtype));

  Build_OP(TOP_daddiu, SP_TN, SP_TN, Gen_Literal_TN(-8, 0), ops);
  Build_OP(TOP_sd, tmp_reg, SP_TN, Gen_Literal_TN(0, 0), ops);

  if (MTYPE_byte_size(Pointer_Mtype) == 8) {    // 64-bit address
    // Cannot use TN_RELOC_HIGH16/TN_RELOC_LOW16 because that will produce a
    // 32-bit address only.  Bug 12662.
    TN *target_tn = Gen_Symbol_TN(st, 0, TN_RELOC_GOT_DISP);
    Build_OP(TOP_ld, tmp_reg, GP_TN, target_tn, ops);
  } else {                                      // 32-bit address
    TN *target_hi_tn = Gen_Symbol_TN(st, 0, TN_RELOC_HIGH16);
    TN *target_lo_tn = Gen_Symbol_TN(st, 0, TN_RELOC_LOW16);
    Build_OP(TOP_lui, tmp_reg, target_hi_tn, ops);
    Build_OP(TOP_addiu, tmp_reg, tmp_reg, target_lo_tn, ops);
  }

  Build_OP(TOP_daddiu, SP_TN, SP_TN, Gen_Literal_TN(8, 0), ops);
  Build_OP(TOP_jr, tmp_reg, ops);
  Build_OP(TOP_ld, tmp_reg, SP_TN, Gen_Literal_TN(-8, 0), ops);
}

// Estimate the branch distance for each branch OP.  If the distance is too
// large to fit in the branch instruction's displacement field, replace the
// branch with a jump.
void
CFLOW_Fixup_Long_Branches()
{
  BB *bb;
  UINT32 *bb_position, ops_count;
  BB_NUM old_PU_BB_Count = PU_BB_Count;

  // Estimate the beginning position of each BB relative to the beginning of
  // the PU.
  int size = (PU_BB_Count + 1) * sizeof(UINT32);
  bb_position = (UINT32 *) alloca(size);
  memset(bb_position, 0, size);
  ops_count = 0;
  for (bb = REGION_First_BB; bb; bb = BB_next(bb)) {
    Is_True(BB_id(bb) <= PU_BB_Count, ("CFLOW_Fixup_Long_Branches: bad BB id"));
    bb_position[BB_id(bb)] = ops_count;
    ops_count += BB_length(bb);
  }

  // Replace long branches with jumps.
  for (bb = REGION_First_BB; bb; bb = BB_next(bb)) {
    // Skip BBs that we just added.
    if (BB_id(bb) > old_PU_BB_Count)
      continue;

    // GOTO bbs related to C++ exception handling often do not have
    // successors.
    if (!BB_succs(bb))
      continue;

    // Check for jumps too because the GNU assembler changes the j instruction
    // to a branch.
    BBKIND bb_kind = BB_kind(bb);
    if (bb_kind == BBKIND_GOTO ||
        bb_kind == BBKIND_LOGIF) {
      // Get the (approx) position of the branch OP.
      UINT32 branch_position =
        bb_position[BB_id(BB_next(bb) ? BB_next(bb) : bb)];

      // Get the target BB's position.
      BB *succ0 = BBLIST_item(BB_succs(bb));
      BB *succ1 = NULL;
      BB *targ_bb = NULL;
      BB *old_fall_thru_bb = NULL;

      // Skip if the branch target BB is the same as the fall thru BB.
      if (bb_kind == BBKIND_GOTO) {
        targ_bb = succ0;
      } else {  // BBKIND_LOGIF
        if (BBLIST_next(BB_succs(bb)) == NULL)
          continue;
        succ1 = BBLIST_item(BBLIST_next(BB_succs(bb)));
        if (BB_next(bb) == succ0) {
          targ_bb = succ1;
          old_fall_thru_bb = succ0;
        } else {
          targ_bb = succ0;
          old_fall_thru_bb = succ1;
        }
        // Cflow should have already made the fall thru BB be the next BB.
        Is_True(old_fall_thru_bb == BB_next(bb),
                ("CFLOW_Fixup_Long_Branches: fall thru BB is not the next BB"));
      }

      UINT32 targ_position = bb_position[BB_id(targ_bb)];

      // Estimate displacement in bytes.  Inflate estimate for safety.
      INT64 disp = (INT64) targ_position - (INT64) branch_position;
      disp *= 4;
      // 14212: Hardware workaround will inflate code.
#if defined(TARG_SL) || !defined(TARG_MIPS)
      if (CG_hw_round > 0 || CG_hw_stall > 0) {
	disp = (INT64) (disp * 1.2);
      } else {
	disp = (INT64) (disp * 1.1);
      }
#endif

      if (!CGTARG_Can_Fit_Displacement_In_Branch_Instruction(disp)) {
        if (bb_kind == BBKIND_GOTO) {
          OPS ops = OPS_EMPTY;
          OP *branch_op = BB_branch_op(bb);
          OP *next_op = OP_next(branch_op);

          if (next_op &&
              OP_code(next_op) == TOP_nop) {
            BB_Remove_Op(bb, next_op);  // Delete NOP in branch delay slot.
          }
          BB_Remove_Op(bb, branch_op);  // Delete old jump OP.
          Build_Long_Goto(targ_bb, &ops);
          BB_Append_Ops(bb, &ops);

          if (!CG_localize_tns) {
            GRA_LIVE_Compute_Liveness_For_BB(bb);
          }
        } else {        // BBKIND_LOGIF
          // Create <goto_bb> to hold the jump to <targ_bb>.  Insert <goto_bb>
          // after <bb>.  (Based on Insert_Goto_BB.)
          OPS ops = OPS_EMPTY;
          BBLIST *sedge = BB_Find_Succ(bb, targ_bb);
          float goto_prob = BBLIST_prob(sedge);
          BOOL goto_prob_fb = BBLIST_prob_fb_based(sedge);
          RID *rid = BB_rid(bb);
          BOOL region_is_scheduled = rid && RID_level(rid) >= RL_CGSCHED;
          BOOL fill_delay_slots = (current_flags & CFLOW_FILL_DELAY_SLOTS) != 0;

          BB *goto_bb = Alloc_BB_Like(bb);
          BB_freq(goto_bb) = BB_freq(bb) * goto_prob;
          Insert_BB(goto_bb, bb);
          Build_Long_Goto(targ_bb, &ops);

          if (BB_freq_fb_based(bb) && goto_prob_fb) {
            BBLIST *edge = BB_Find_Succ(goto_bb, targ_bb);
            Set_BB_freq_fb_based(goto_bb);
            Set_BBLIST_prob_fb_based(edge);
          }

          if (PROC_has_branch_delay_slot()
              && (fill_delay_slots || region_is_scheduled)) {
            Set_BB_scheduled(goto_bb);
          }
          BB_Append_Ops(goto_bb, &ops);

          Unlink_Pred_Succ(bb, targ_bb);
          Link_Pred_Succ_with_Prob(bb, goto_bb, goto_prob);
          if (goto_prob_fb) {
            BBLIST *edge = BB_Find_Succ(bb, goto_bb);
            Set_BBLIST_prob_fb_based(edge);
          }

          INT tfirst;
          INT tcount;
          LABEL_IDX lab;
          TN *lab_tn;
          OP *branch_op = BB_branch_op(bb);
          CGTARG_Branch_Info(branch_op, &tfirst, &tcount);
          lab = Gen_Label_For_BB(old_fall_thru_bb);
          lab_tn = Gen_Label_TN(lab, 0);
          Set_OP_opnd(branch_op, tfirst, lab_tn);       // Change branch target.
          Negate_Logif_BB(bb);                  // Negate sense of branch.

          if (PROC_has_branch_delay_slot()) {

            // If <bb> ends in branch likely, move the delay slot OP
            // to <goto_bb>
            OP *delay_op = OP_next(branch_op);
            if (delay_op != NULL && !OP_noop(delay_op) && OP_likely(branch_op)) {
              BB_Move_Op_To_Start(goto_bb, bb, delay_op);
              delay_op = NULL;
            }

            // If <bb> branch delay slot is empty (because we just moved out
            // the delay slot OP, or because GCM deliberately deleted the NOP
            // -- see Fill_From_Successor in gcm.cxx), then insert NOP.
            // (Fixes bug 11776)
            if (delay_op == NULL && (fill_delay_slots || region_is_scheduled)) {
              OPS ops = OPS_EMPTY;
              Exp_Noop(&ops);
              Set_BB_scheduled(bb);
              BB_Append_Ops(bb, &ops);
            }

          }

          if (!CG_localize_tns) {
            GRA_LIVE_Compute_Liveness_For_BB(goto_bb);
            GRA_LIVE_Compute_Liveness_For_BB(bb);
          }
        }
      }
    }
  }
}


#endif  // KEY and TARG_MIPS

