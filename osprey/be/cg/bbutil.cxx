/*
 * Copyright (C) 2008-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
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
 * Module: bbutil.c
 * $Revision: 1.15 $
 * $Date: 05/12/05 08:59:02-08:00 $
 * $Author: bos@eng-24.pathscale.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.bbutil.cxx $
 *
 * Revision history:
 *  22-Sept-89 - Original Version
 *  01-Feb-91 - Copied for TP/Muse
 *  12-Jun-91 - Removed OP_specific insertion routines to oputil.c
 *
 * Description:
 *
 * Routines for manipulating the BB data structure.
 *
 * ====================================================================
 * ====================================================================
 */

#include <stdint.h>
#include <alloca.h>
#include <stdio.h>
#include <iterator>
#include "defs.h"
#include "symtab.h"
#include "config.h"
#include "tracing.h"
#include "erglob.h"
#include "const.h"
#include "glob.h"
#include "config_asm.h"
#include "wn.h"
#include "ir_reader.h"

#include "targ_proc_properties.h"
#include "strtab.h"
#include "tn.h"
#include "bb.h"
#include "gra_live.h"
#include "op.h"
#include "cg_region.h"
#include "tn_set.h"
#include "gtn_universe.h"
#include "gtn_tn_set.h"
#include "note.h"
#include "cgtarget.h"
#include "cgexp.h"
#include "irbdata.h"
#include "whirl2ops.h"
#include "xstats.h"
#include "data_layout.h"
#include "freq.h"
#include "cg_loop.h"
#include "label_util.h"
#include "lra.h"
#include "bb_set.h"       // BB_SET_* routines 
#include "DaVinci.h"
#include "vcg.h"
#include <sstream>
#include <fstream>
#include <cstdlib>
#include "cg.h"
#ifdef TARG_IA64
#include "ipfec_options.h"
#include "region_bb_util.h"
#include "region.h"
#include <vector>
#include "if_conv.h"
#endif

/* Allocate basic blocks for the duration of the PU. */
#define BB_Alloc()  TYPE_PU_ALLOC(BB)
#define BB_Alloc_N(n) TYPE_PU_ALLOC_N(BB, n)
#define BB_MEM_pool_ptr MEM_pu_pool_ptr
#define BB_MEM_pool MEM_pu_pool

mBB_NUM *bb_dfo_id;	/* Vector of depth-first ids, indexed by BB_id(bb) */
BB     **bb_dfo_vec;	/* Vector of BBs, indexed by depth-first id. */
INT      min_dfo_id;	/* Lowest depth-first id assigned. */

BB_LIST	*Entry_BB_Head;	  /* List of entry points */
BB_LIST	*Exit_BB_Head;	  /* List of entry points */
BB	*REGION_First_BB; /* First BB in the current region (or PU) */
BB_NUM	 PU_BB_Count;     /* Number of BBs in the current PU,
			   * PU, indexed 1..PU_BB_Count in BB_id.
			   */

#define BB_VEC_INCR 32	/* Amount to grow BB_Vec by */
static BB_NUM BB_Vec_Count; /* Number of entries allocated in BB_Vec */
BB **BB_Vec;		/* Mapping from bb idx to BB for each PU */

static BB_MAP bb_st_map;	/* map BBs to their STs */
static INT32 bb_st_count = 0;	/* number of BB STs created
				 * (per compilation, not just PU)
				 */

/* ====================================================================
 *
 * BB_PU_Initialize
 *
 * Initialize for a new PU
 *
 * ====================================================================
 */
void
BB_PU_Initialize(void)
{
  BB_Vec_Count = 0;
  PU_BB_Count = 0;
  REGION_First_BB = NULL;
  Entry_BB_Head = NULL;
  Exit_BB_Head = NULL;
  bb_st_map = 0;
}

/* ====================================================================
 *
 * BB_REGION_Initialize
 *
 * Initialize for a new REGION
 *
 * ====================================================================
 */
void
BB_REGION_Initialize(void)
{
  REGION_First_BB = NULL;
}

/* ====================================================================
 *
 * Gen_BB
 *
 * Create a new BB and assign an ID to it.
 *
 * ====================================================================
 */

BB *
Gen_BB (void)
{
  BB *bp;

  bp = BB_Alloc();
  if (bp == NULL) ErrMsg(EC_No_Mem, "Gen_BB");

  if (PU_BB_Count >= BB_NUM_MAX) ErrMsg(EC_Out_Of, "BB numbers", "Gen_BB");
  bp->id = ++PU_BB_Count;
  PU_BB_Cnt++;

  /* Make sure the BB vector is big enough to hold the entry for the new ID
   */
  if (PU_BB_Count + 1 > BB_Vec_Count) {
    if (BB_Vec_Count == 0) {
      BB_Vec = TYPE_PU_ALLOC_N(BB *, PU_BB_Count + 1 + BB_VEC_INCR);
      BB_Vec_Count = PU_BB_Count + 1 + BB_VEC_INCR;
    } else {
      BB_NUM size = BB_Vec_Count * 2;
      while (size <= PU_BB_Count)
	size *= 2;
      BB_Vec = TYPE_MEM_POOL_REALLOC_N(BB *,
				       BB_MEM_pool_ptr,
				       BB_Vec,
				       BB_Vec_Count,
				       size);
      BB_Vec_Count = size;
    }
  }

  BB_Vec[PU_BB_Count] = bp;

  return bp;
}


/* ====================================================================
 *
 * Gen_BB_N
 *
 * Create a vector of 'n' new BBs and assign an ID to each.
 *
 * ====================================================================
 */

BB *
Gen_BB_N (INT n)
{
  BB *bp;
  INT i;

  bp = BB_Alloc_N(n);
  if (bp == NULL) ErrMsg(EC_No_Mem, "Gen_BB_N");

  if (PU_BB_Count + n > BB_NUM_MAX) ErrMsg(EC_Out_Of, "BB numbers", "Gen_BB_N");

  /* Make sure the BB vector is big enough to hold the entries for the new IDs
   */
  if (PU_BB_Count + 1 + n > BB_Vec_Count) {
    if (BB_Vec_Count == 0) {
      BB_Vec = TYPE_PU_ALLOC_N(BB *, PU_BB_Count + 1 + n + BB_VEC_INCR);
      BB_Vec_Count = PU_BB_Count + 1 + n + BB_VEC_INCR;
    } else {
      BB_NUM size = BB_Vec_Count * 2;
      while (size <= PU_BB_Count + n)
	size *= 2;
      BB_Vec = TYPE_MEM_POOL_REALLOC_N(BB *,
				       BB_MEM_pool_ptr,
				       BB_Vec,
				       BB_Vec_Count,
				       size);
      BB_Vec_Count = size;
    }
  }

  /* Fill in the BB vector entries and set the IDs in the new BBs
   */
  for (i = 0; i < n; i++) {
    PU_BB_Count++;
    BB_Vec[PU_BB_Count] = bp + i;
    bp[i].id = PU_BB_Count;
  }

  return bp;
}

BB *
Gen_BB_Like (BB *model)
{
  BB *bb = Gen_BB();
  if (model == NULL) model = REGION_First_BB;
  if (model) BB_rid(bb) = BB_rid(model);
  return bb;
}

/* ====================================================================
 *
 * Gen_And_Append_BB
 *
 * Allocate a new BB and append it as the next basic block after prev_bb.
 *
 * ====================================================================
 */

BB *
Gen_And_Append_BB (BB *prev_bb) 
{
#ifdef KEY
  /* bug#448
     The rid info from <prev_bb> should be propagated.
   */
  BB *bb = Gen_BB_Like(prev_bb);
#else
  BB *bb = Gen_BB();
#endif

  if (prev_bb != NULL) {
    Is_True (BB_next(prev_bb) == NULL, 
	    ("Gen_And_Append_BB: prev_bb not last one"));
    BB_next(prev_bb) = bb;
    BB_prev(bb) = prev_bb;
  }
  return bb;
}

BB *Gen_And_Insert_BB_After(BB *point)
/* -----------------------------------------------------------------------
 * See "bb.h" for specification.
 * -----------------------------------------------------------------------
 */
{
  BB *bb = Gen_BB_Like(point);
  BB_prev(bb) = point;
  BB_next(bb) = BB_next(point);
  if ( BB_next(point) != NULL )     /* Else last in region and/or PU */
    BB_prev(BB_next(point)) = bb;
  BB_next(point) = bb;

  return bb;
}

BB* Gen_And_Insert_BB_Before(BB *point)
/* -----------------------------------------------------------------------
 * See "bb.h" for specification.
 * -----------------------------------------------------------------------
 */
{
  BB *bb = Gen_BB_Like(point);

  BB_next(bb) = point;
  BB_prev(bb) = BB_prev(point);
  if (BB_prev(point))		    /* Else first in region and/or PU */
    BB_next(BB_prev(point)) = bb;
  BB_prev(point) = bb;

  return bb;
}

/* =======================================================================
 *
 *  Append_Region_BBs
 *
 *  See interface description.
 *
 * =======================================================================
 */
BB *
Append_Region_BBs(BB *prev, RID *rid)
{
  BB *first_bb;
  CGRIN *cgrin = RID_cginfo( rid );
  first_bb = CGRIN_first_bb( cgrin );

  if ( first_bb == NULL )
    return prev;

  if ( prev ) {
    BB_next( prev ) = first_bb;
    BB_prev( first_bb ) = prev;
  }
  else {
    /*
     * starting a new REGION
     */
    REGION_First_BB = first_bb;
  }
  BB *bb;
  for (bb = first_bb; BB_next(bb) != NULL; bb = BB_next(bb))
	;
  /* BB_next(bb) == NULL */
  if (bb != CGRIN_last_bb(cgrin)) {
	DevWarn("CGRIN_last_bb of rid %d is not really the last one?", RID_id(rid));
  }
  return bb;	/* last_bb */
}

/* ====================================================================
 *
 * Free_BB_Memory
 *
 * At the end of compiling a PU, free the memory associated with its
 * BBs.
 *
 * At present, this means the predecessor/successor lists and the
 * dominator/post-dominator bit vectors.  Everything else is allocated
 * with BB_Alloc and should be automatically freed at the appropriate
 * time (by PU_Free).
 *
 * ====================================================================
 */

void
Free_BB_Memory ( void )
{
  BB *bb;

  for ( bb = REGION_First_BB; bb; bb = BB_next(bb) ) {
    BBlist_Free ( &BB_preds(bb) );
    BBlist_Free ( &BB_succs(bb) );
  }
  REGION_First_BB = NULL;

}



/* ====================================================================
 *
 * Remove_BB / Insert_BB / Move_BB
 *
 * Remove a BB from the BB list, insert one at a given place in the
 * list, or move it from one place to another in the list.
 *
 * ====================================================================
 */

void
Remove_BB ( BB *bb )
{
  BB *p = BB_prev(bb);
  BB *n = BB_next(bb);
  RID *rid = BB_rid(bb);
  if (rid != NULL && RID_cginfo(rid) != NULL) {
	/* update cgrin pointers */
	CGRIN *cgrin = RID_cginfo( rid );
	if (cgrin && CGRIN_first_bb(cgrin) == bb)
		CGRIN_first_bb(cgrin) = n;
	if (cgrin && CGRIN_last_bb(cgrin) == bb)
		CGRIN_last_bb(cgrin) = p;
  }

  if ( p != NULL ) {
    BB_next(p) = n;
  } else {
    REGION_First_BB = n;
  }

  if ( n != NULL )
    BB_prev(n) = p;

  BB_prev(bb) = BB_next(bb) = NULL;
}

/* ================================================================= */

void
Insert_BB ( BB *bb, BB *after )
{
  BB *nx = (after!=NULL) ? BB_next(after) : REGION_First_BB;

  BB_next(bb) = nx;
  BB_prev(bb) = after;
  if ( after != NULL ) {
    BB_next(after) = bb;
  } else {
    REGION_First_BB = bb;
  }
  if ( nx != NULL ) {
    BB_prev(nx) = bb;
  }
  if (after != NULL && BB_rid(after) != NULL && BB_rid(bb) == BB_rid(after)) {
	/* update cgrin pointers if this is new tail bb for rid */
        CGRIN *cgrin = RID_cginfo(BB_rid(after));
	if (cgrin && CGRIN_last_bb(cgrin) == after)
		CGRIN_last_bb(cgrin) = bb;
  }
}

/* ================================================================= */

void
Move_BB ( BB *bb, BB *after )
{
  Remove_BB ( bb );
  Insert_BB ( bb, after );
}

/* =======================================================================
 *
 *  Chain_BBs
 *
 *  See interface description.
 *
 * =======================================================================
 */
void
Chain_BBs(
  BB *bb1,
  BB *bb2
)
{
  if (bb1) Verify_BB(bb1);
  if (bb2) Verify_BB(bb2);
  if (bb1) BB_next(bb1) = bb2;
  if (bb2) BB_prev(bb2) = bb1;
  if (bb1 && bb2 && BB_rid(bb1) != NULL && BB_rid(bb1) == BB_rid(bb2)) {
	/* update cgrin pointers if this is new tail bb for rid */
	BB *bb;
        CGRIN *cgrin = RID_cginfo(BB_rid(bb1));
	if (cgrin && CGRIN_first_bb(cgrin) == bb2) {
		for (bb = bb1; BB_prev(bb); bb = BB_prev(bb))
			;	/* find beginning */
		CGRIN_first_bb(cgrin) = bb;
		if (REGION_First_BB == bb2)
			REGION_First_BB = bb;
	}
	if (cgrin && CGRIN_last_bb(cgrin) == bb1) {
		for (bb = bb2; BB_next(bb); bb = BB_next(bb))
			;	/* find end */
		CGRIN_last_bb(cgrin) = bb;
	}
  }
}


/* =======================================================================
 *
 *  Target_Simple_Fall_Through_BB
 *
 *  See interface description.
 *
 * =======================================================================
 */
void
Target_Simple_Fall_Through_BB(
  BB *bb,
  BB *target_bb
)
{
  OP *br;

  FmtAssert(BB_succs(bb) == NULL, ("Unexpected nonempty succ list."));

  BB_next(bb) = target_bb;
  BB_prev(target_bb) = bb;

#ifdef TARG_IA64   
  if(IPFEC_Enable_Region_Formation && RGN_Formed) {
    BB *bb_wo_node = NULL;
    REGIONAL_CFG_NODE *src_node   = Regional_Cfg_Node(bb);
    REGIONAL_CFG_NODE *target_node = Regional_Cfg_Node(target_bb);
    Is_True(((src_node != NULL)||(target_node != NULL)),("Two node NULL error in Target Simple Fall Through BB"));
    if (target_node == NULL) {
      REGION *rgn = Home_Region(bb);
      REGIONAL_CFG *regional_cfg = rgn->Regional_Cfg();
      RGN_Gen_And_Insert_Node(target_bb,NULL,NULL,regional_cfg);
    } else if (src_node == NULL) {
      REGION *rgn = Home_Region(target_bb);
      REGIONAL_CFG *regional_cfg = rgn->Regional_Cfg();
      RGN_Gen_And_Insert_Node(bb,NULL,NULL,regional_cfg);
    }
    RGN_Link_Pred_Succ_With_Prob (bb, target_bb, 1.0F);
  } else {
    Link_Pred_Succ_with_Prob (bb, target_bb, 1.0F);
  }  
#else
  Link_Pred_Succ_with_Prob (bb, target_bb, 1.0F);
#endif

  br = BB_Remove_Branch(bb);
  FmtAssert(!(br && OP_cond(br)), ("Unexpected conditional branch."));
}

/* =======================================================================
 *
 *  Target_Cond_Branch
 *
 *  See interface description.
 *
 * =======================================================================
 */
void
Target_Cond_Branch(
  BB *bb,
  BB *br_targ_bb,
  float prob		   
)
{
  INT32 opnd, opnd_count;
  OP *branch_op;
  LABEL_IDX targ_label    = Gen_Label_For_BB(br_targ_bb);
  TN       *targ_label_tn = Gen_Label_TN(targ_label,0);

  branch_op = BB_branch_op(bb);
  FmtAssert(branch_op && OP_cond(branch_op),
	    ("BB doesn't end in a conditional branch."));

  Link_Pred_Succ_with_Prob (bb, br_targ_bb, prob);

  CGTARG_Branch_Info(branch_op,&opnd,&opnd_count);
  FmtAssert(opnd_count == 1,("Branch with multiple bbs"));
  Set_OP_opnd(branch_op,opnd, targ_label_tn);
}

/* =======================================================================
 *
 *  Target_Logif_BB
 *
 *  See interface description.
 *
 * =======================================================================
 */
void
Target_Logif_BB(
  BB *bb,
  BB *br_targ_bb,
  float br_targ_prob,
  BB *fall_through_bb
)
{
  INT32 opnd, opnd_count;
  OP *branch_op;
  LABEL_IDX targ_label    = Gen_Label_For_BB(br_targ_bb);
  TN       *targ_label_tn = Gen_Label_TN(targ_label,0);

  branch_op = BB_branch_op(bb);
  FmtAssert(branch_op && OP_cond(branch_op),
	    ("BB doesn't end in a conditional branch."));

  Chain_BBs(bb,fall_through_bb);

  Link_Pred_Succ_with_Prob (bb, br_targ_bb, br_targ_prob);
  Link_Pred_Succ_with_Prob (bb, fall_through_bb, 1.0F - br_targ_prob);

  CGTARG_Branch_Info(branch_op,&opnd,&opnd_count);
  FmtAssert(opnd_count == 1,("Branch with multiple bbs"));
  Set_OP_opnd(branch_op,opnd, targ_label_tn);
}


/* =======================================================================
 *
 *  Negate_Logif_BB
 *
 *  See interface description.
 *
 * =======================================================================
 */
void
Negate_Logif_BB(
  BB *bb
)
{
  OP  *op      = BB_branch_op(bb);
  TOP  old_top = OP_code(op);
  TOP  new_top = CGTARG_Invert(old_top);

  Is_True(new_top != TOP_UNDEFINED, 
	  ("unable to negate branch: %s", TOP_Name(old_top)));
  OP_Change_Opcode(op, new_top);
}


/* =======================================================================
 *
 *  Add_Goto_Op
 *
 *  See interface description.
 *
 * =======================================================================
 */
void
Add_Goto_Op
(
  BB *bb,
  BB *target_bb
)
{
  TN *target_label_tn = Gen_Label_TN(Gen_Label_For_BB(target_bb),0);

  OPS  ops = OPS_EMPTY;
  Exp_OP1(OPC_GOTO,NULL,target_label_tn,&ops);

  /* set srcpos of goto:  first try to use srcpos of bb;
   * if that doesn't work try the prev-bb. */
  SRCPOS srcpos = 0;
  if (BB_last_op(bb) != NULL) {
	srcpos = OP_srcpos(BB_last_op(bb));
  }
  if (srcpos == 0 && BB_prev(bb) != NULL && BB_last_op(BB_prev(bb)) != NULL) {
	srcpos = OP_srcpos(BB_last_op(BB_prev(bb)));
  }

  OP *op;
  FOR_ALL_OPS_OPs(&ops, op) {
      OP_srcpos(op) = srcpos;
  }
  BB_Append_Ops(bb,&ops);
}


/* =======================================================================
 *
 *  Add_Goto
 *
 *  See interface description.
 *
 * =======================================================================
 */
void
Add_Goto
(
  BB *bb,
  BB *target_bb
)
{
  Add_Goto_Op(bb, target_bb);
  Link_Pred_Succ_with_Prob(bb, target_bb, 1.0F);
}

/* =======================================================================
 *
 *  Create_Dummy_BB
 *
 *  See interface description.
 *
 * =======================================================================
 */
BB*
Create_Dummy_BB(
  BB *dest_bb
)
{
  BB *dummy_bb;

  dummy_bb = Gen_BB();
  BB_rid(dummy_bb) = BB_rid(dest_bb);

  Target_Simple_Fall_Through_BB(dummy_bb,dest_bb);

  return dummy_bb;
}


/* ====================================================================
 *
 * BBKIND_Name
 *
 * Return a string containing the name of the BB kind.
 *
 * ====================================================================
 */

/* Define an array of BBKIND names for printing purposes: */
static const char * const BBKIND_names[] = {
  /* 0 */	"--UNKNOWN--",
  /* 1 */	"GOTO",
  /* 2 */	"LOGIF",
  /* 3 */	"VARGOTO",
  /* 4 */	"INDGOTO",
  /* 5 */	"RETURN",
  /* 6 */	"CALL",
  /* 7 */	"REGION_EXIT",
  /* 8 */	"TAIL_CALL",
#ifdef TARG_IA64
  /* 9 */	"CHK",
#elif defined(TARG_SL)
  /* 9 */       "BBKIND_ZDL_BODY", 
  /* 10 */      "BBKIND_FORK",
#endif
};
/* WARNING: the order of this array must match #defines in bb.h. */

const char *
BBKIND_Name ( BBKIND k )
{
  static char numbuf[11];

  if ( ! VALID_BBKIND ( k ) ) {
    DevWarn ("BBKIND_Name called with unknown BBKIND");
    sprintf ( numbuf, "%u", k );
    return numbuf;
  }

  return BBKIND_names[k];
}


/* ====================================================================
 *
 * BB_kind
 *
 * See interface description.
 *
 * ====================================================================
 */
BBKIND
BB_kind(BB *bb)
{
  OP *br;
  INT nsuccs;
  INT tcount;
  INT tfirst;

  /* An exit block is a return or a tail call.
   */
  if (BB_exit(bb)) return BB_call(bb) ? BBKIND_TAIL_CALL : BBKIND_RETURN;

  /* A call block is a (drum roll please...) call.
   */
  if (BB_call(bb)) return BBKIND_CALL;

#if defined(TARG_SL)
  if ( BB_zdl_body(bb)) return BBKIND_ZDL_BODY;
#endif

#ifdef TARG_IA64
  /* A chk bb end with a chk op
   */
  if (BB_Last_chk_op(bb)) return BBKIND_CHK;// bug fix for OSP_104, OSP_105, OSP_192
#endif

  /* Get the branch OP and the number of successors.
   */
  br = BB_branch_op(bb);
  nsuccs = BBlist_Len(BB_succs(bb));

  /* If no terminating branch, make sure we have an implicit goto.
   */
  if (br == NULL) {

    /* A block with one explicit successor just falls through to the
     * next BB. A block with no successors, just falls off the end of
     * the PU -- I guess it goes nowhere.
     */
    if (nsuccs <= 1) return BBKIND_GOTO;

    /* No idea what this could be...
     */
    DevWarn("BB_kind: Unable to determine BB_kind for BB:%d", BB_id(bb));
    return BBKIND_UNKNOWN;
  }

#if defined(TARG_SL) && defined(TARG_SL2)
  if (OP_fork(br)) {
    FmtAssert(nsuccs == 2, ("BB_kind: FORK BB has %d successors", nsuccs));
    return BBKIND_FORK;
  }
#endif

  /* Get some info about the terminating branch.
   */
  CGTARG_Branch_Info(br, &tfirst, &tcount);

  /* Handle conditional branches.
   */
  if (OP_cond(br) && !OP_ijump(br)) {
    if (tcount != 1) {
      DevWarn("BB_kind: Conditional branch in BB:%d with %d labels",
	      BB_id(bb), tcount);
      return BBKIND_UNKNOWN;
    }

    switch (nsuccs) {
    case 1:

      /* Our one-and-only successor better be the next BB.
       */
      if (BBLIST_item(BB_succs(bb)) == BB_next(bb)) break;

      DevWarn("BB_kind: LOGIF BB:%d doesn't have a fall through, nsuccs=%d",
	      BB_id(bb),nsuccs);
      return BBKIND_UNKNOWN;

    case 2:

      /* Make sure one of the successors is the next BB.
       */
      if (BBLIST_item(BB_succs(bb)) == BB_next(bb) ||
	  BBLIST_item(BBLIST_next(BB_succs(bb))) == BB_next(bb)) break;

      DevWarn("BB_kind: LOGIF BB:%d doesn't have a fall through, nsuccs=%d",
	      BB_id(bb),nsuccs);
      return BBKIND_UNKNOWN;

    default:
      DevWarn("BB_kind: LOGIF BB:%d with %d successors", BB_id(bb), nsuccs);
      return BBKIND_UNKNOWN;
    }

    return BBKIND_LOGIF;
  }

  /* If there were no label operands on the branch, then we must
   * be performing an indirect jump. Unless we are positive it is
   * a GOTO, assume we have some sort of case statement.
   */
  if (tcount == 0) {
    if (nsuccs == 1) {
      WN *wn = BB_branch_wn(bb);
      if (wn && WN_opcode(wn) == OPC_GOTO) return BBKIND_GOTO;
    }

    ANNOTATION *ant = ANNOT_Get(BB_annotations(bb), ANNOT_SWITCH);
    return ant != NULL ? BBKIND_VARGOTO : BBKIND_INDGOTO;
  }

  /* Anything else has got to be a simple unconditional branch to label.
   */
  if (tcount != 1) {
    DevWarn("BB_kind: Branch with %d labels in BB:%d", tcount, BB_id(bb));
    return BBKIND_UNKNOWN;
  }

  /* Special case branches to external labels.
   */
  if (nsuccs == 0) {
    WN *wn = BB_branch_wn(bb);
    if (wn && WN_opcode(wn) == OPC_REGION_EXIT) return BBKIND_REGION_EXIT;
  }

  if (nsuccs != 1) {
    DevWarn("BB_kind: GOTO BB:%d has %d successors", BB_id(bb), nsuccs);
    return BBKIND_UNKNOWN;
  }

  return BBKIND_GOTO;
}

/* ====================================================================
 *
 * Print_LOOPINFO
 *
 * Print out information associated with LOOPINFO annotations.
 *
 * ====================================================================
 */
void
Print_LOOPINFO(LOOPINFO *info)
{
  WN *loop_info = LOOPINFO_wn(info);
  fprintf(TFile, "  Head of loop body line %d\n", LOOPINFO_line(info));
  fprintf(TFile, "    ivar = ");
  fdump_tree(TFile, WN_loop_induction(loop_info));
  fprintf(TFile, "    trip count = ");
  fdump_tree(TFile, WN_loop_trip(loop_info));
  if (WN_loop_trip_est(loop_info))
    fprintf(TFile, "    trip count estimate = %u\n",
		   WN_loop_trip_est(loop_info));
  fprintf(TFile, "    nesting depth = %u\n", WN_loop_depth(loop_info));
  fprintf(TFile, "    flags = ");
  if (WN_Loop_Innermost(loop_info)) fprintf(TFile, "INNERMOST ");
  if (WN_Loop_Winddown_Reg(loop_info)) fprintf(TFile, "WINNDOWN_REG ");
  if (WN_Loop_Winddown_Cache(loop_info)) fprintf(TFile, "WINNDOWN_CACHE ");
  if (WN_Loop_Unimportant_Misc(loop_info)) fprintf(TFile, "UNIMPORTANT_MISC ");
  if (WN_Loop_Nz_Trip(loop_info)) fprintf(TFile, "NZ_TRIP ");
  if (WN_Loop_Symb_Trip(loop_info)) fprintf(TFile, "SYMB_TRIP ");
  if (WN_Loop_Up_Trip(loop_info)) fprintf(TFile, "UP_TRIP ");
  if (LOOPINFO_multiversion(info)) fprintf(TFile, "LMV");
  if (LOOPINFO_vectorized(info)) fprintf(TFile, "VEC");
  if (LOOPINFO_align_peeled(info)) fprintf(TFile, "ALIGN_PEELED");
  fprintf(TFile, "\n");
  if (LOOPINFO_trip_count_tn(info)) {
    fprintf(TFile, "    trip count TN = ");
    Print_TN(LOOPINFO_trip_count_tn(info),FALSE);
    fprintf(TFile, "\n");
  }
}

/* ====================================================================
 *
 * Print_BB_Header
 *
 * Print the header information associated with a BB to the trace file.
 *
 * ====================================================================
 */
void
Print_BB_Header ( BB *bp, BOOL flow_info_only, BOOL print_tn_info )
{
  BBLIST *bl;
  INT16 i;
  ANNOTATION *annot = ANNOT_Get(BB_annotations(bp), ANNOT_LOOPINFO);
  BOOL freqs = FREQ_Frequencies_Computed();

  fprintf ( TFile,
	    "BB:%d (length:%d) -- "
	    "flags = 0x%04x",
	    BB_id(bp), BB_length(bp), BB_flag(bp) );
  if (freqs || BB_freq_fb_based(bp))
    fprintf(TFile, ", frequency = %g (%s)", BB_freq(bp),
	    BB_freq_fb_based(bp) ? "feedback" : "heuristic");
  fprintf ( TFile, "\n");
  
  if (BB_unreachable(bp)) fprintf ( TFile, "  Unreachable block\n");
  if (BB_entry(bp))	fprintf ( TFile, "  Entry block\n" );
  if (BB_handler(bp))	fprintf ( TFile, "  Handler block\n" );
  if (BB_asm(bp)) 	fprintf ( TFile, "  Asm block\n" );
  if (BB_exit(bp)) {
    if (BB_call(bp))	fprintf ( TFile, "  Tail call block\n" );
    else		fprintf ( TFile, "  Exit block\n" );
  } else if (BB_call(bp)) fprintf ( TFile, "  Call block\n" );
#ifdef TARG_IA64
  if (BB_chk_split(bp)) 	fprintf ( TFile, "  Check split block\n" );
  if (BB_chk_split_head(bp)) 	fprintf ( TFile, "  Check split head block\n" );
  //bug fix for OSP_212
  if (BB_chk_split_tail(bp))    fprintf ( TFile, "  Check split tail block\n" );
  if (BB_recovery(bp)) 	fprintf ( TFile, "  Recovery block\n" );
  if (BB_scheduled(bp)) 	fprintf ( TFile, "  Scheduled BB\n" );
#endif

  if (BB_rid(bp)) {
    INT exits;
    RID *rid = BB_rid(bp);
    CGRIN *cgrin = RID_cginfo(rid);
    if (cgrin) {
      if (bp == CGRIN_entry(cgrin)) {
	fprintf ( TFile, "  Region entry block\n" );
      }
      exits = RID_num_exits(rid);
      for (i = 0; i < exits; ++i) {
	if (bp == CGRIN_exit_i(cgrin, i)) {
	  fprintf ( TFile, "  Region exit block %d\n", i );
	}
      }
    }
  }

  if (annot)
    Print_LOOPINFO(ANNOT_loopinfo(annot));

  if (BB_loop_head_bb(bp)) {
    if (BB_loophead(bp)) {
      if (!annot) {
	fprintf(TFile, "  Head of loop body line %d\n", BB_Loop_Lineno(bp));
      }
    } else {
      BB *head = BB_loop_head_bb(bp);
      fprintf(TFile,
	      "  Part of loop body starting at line %d with head BB:%d\n",
	      BB_Loop_Lineno(head), BB_id(head));
    }
  }

  if (BB_unrollings(bp) > 1)
    fprintf(TFile, "  Unrolled %d times%s\n", BB_unrollings(bp),
	    BB_unrolled_fully(bp) ? " (fully)" : "");

  if ( BB_rid(bp) )
    RID_Fprint( TFile, BB_rid(bp) );

  if ( BB_entry(bp) ) {
    ANNOTATION *ant = ANNOT_Get (BB_annotations(bp), ANNOT_ENTRYINFO);
    ENTRYINFO *ent = ANNOT_entryinfo (ant);
    OP *sp_adj = BB_entry_sp_adj_op(bp);
    Is_True ((sp_adj == ENTRYINFO_sp_adj(ent)),("bad sp_adj"));

    fprintf ( TFile, "Entrypoint: %s\t Starting Line %d\n",
	      ST_name(ENTRYINFO_name(ent)),
	      Srcpos_To_Line(ENTRYINFO_srcpos(ent)));

    if ( ! flow_info_only && sp_adj) {
      OP *op;
      BOOL found_sp_adj = FALSE;
      fprintf ( TFile, "SP entry adj: " );
      Print_OP_No_SrcLine (sp_adj);
      FOR_ALL_BB_OPs_FWD(bp,op)
	if (op == sp_adj)
	{
	  found_sp_adj = TRUE;
	  break;
	}
      if (found_sp_adj == FALSE)
	fprintf ( TFile, "******** ERROR ******** sp adjust not found in entry block\n");
    }
  }

  if ( BB_exit(bp) ) {
    ANNOTATION *ant = ANNOT_Get (BB_annotations(bp), ANNOT_EXITINFO);
    EXITINFO *exit = ANNOT_exitinfo (ant);
    OP *sp_adj = BB_exit_sp_adj_op(bp);
    Is_True ((sp_adj == EXITINFO_sp_adj(exit)),("bad sp_adj"));

    if ( ! flow_info_only && sp_adj) {
      OP *op;
      BOOL found_sp_adj = FALSE;
      fprintf ( TFile, "SP exit adj: " );
      Print_OP_No_SrcLine (sp_adj);

      FOR_ALL_BB_OPs_FWD(bp,op)
	if (op == sp_adj)
	{
	  found_sp_adj = TRUE;
	  break;
	}
      if (found_sp_adj == FALSE)
	fprintf ( TFile, "******** ERROR ******** sp adjust not found in exit block\n");
    }
  }

  /* Print predecessor/successor information: */
  fprintf ( TFile, "In linear order, BB_prev == BB:%2d; BB_next == BB:%2d\n",
	    (BB_prev(bp) ? BB_id(BB_prev(bp)) : -1),
	    (BB_next(bp) ? BB_id(BB_next(bp)) : -1) );

  fprintf ( TFile, "Predecessors:\t" );
  i = 0;
  FOR_ALL_BB_PREDS (bp, bl) {
    fprintf ( TFile, "%sBB:%2d ",
	      ( (i == 0) ? "" : (i%5 == 0) ? ",\n\t\t" : ", " ),
	      BB_id(BBLIST_item(bl)));
    ++i;
  }
  if ( i == 0 ) fprintf ( TFile, "none" );
  fprintf ( TFile, "\nSuccessors%s:\t", freqs ? " (w/probs)" : "" );
  i = 0;
  FOR_ALL_BB_SUCCS (bp, bl) {
    fprintf ( TFile, "%sBB:%2d",
	      ( (i == 0) ? "" : (i%5 == 0) ? ",\n\t\t" : ", " ),
	      BB_id(BBLIST_item(bl)));
    if (freqs) fprintf(TFile, " (%g)", BBLIST_prob(bl));
    ++i;
  }
  if ( i == 0 ) fprintf ( TFile, "none" );
  fprintf ( TFile, "\n" );

  if (BB_has_label(bp)) {
    ANNOTATION *ant;
    fprintf(TFile, "Labeled with ");
    for (ant = ANNOT_First(BB_annotations(bp), ANNOT_LABEL);
	 ant != NULL;
	 ant = ANNOT_Next(ant, ANNOT_LABEL))
    {
      INT eh_labs = 0;
      LABEL_IDX label = ANNOT_label(ant);
      fprintf (TFile,"%s ", LABEL_name(label));
      FmtAssert((Get_Label_BB(label) == bp),
		("Inconsistent ST for BB:%2d label", BB_id(bp)));
      switch (LABEL_kind(Label_Table[label])) {
      case LKIND_BEGIN_EH_RANGE:
	fprintf (TFile,"%cbegin_eh_range", eh_labs++ ? ' ' : '(');
	break;
      case LKIND_END_EH_RANGE:
	fprintf (TFile,"%cend_eh_range", eh_labs++ ? ' ' : '(');
	break;
      }
      if (eh_labs)
	fprintf (TFile,") ");
    }
    fprintf(TFile, "\n");
  }

  if ( flow_info_only ) return;


  if ( print_tn_info )
    GRA_LIVE_Print_Liveness(bp);
}

/* ====================================================================
 *
 * Print_BB_Pragmas
 *
 * Print the pragmas associated with a block to TFile.
 *
 * ====================================================================
 */

void
Print_BB_Pragmas( BB *bp )
{
  if ( BB_has_pragma(bp) ) {
    ANNOTATION *ant;
    BOOL first = TRUE;
    fprintf(TFile, "Pragmas:\t");
    for ( ant = ANNOT_First (BB_annotations(bp), ANNOT_PRAGMA);
          ant != NULL;
          ant = ANNOT_Next (ant, ANNOT_PRAGMA))
    {
      WN *wn = ANNOT_pragma(ant);
      WN_PRAGMA_ID pragma = (WN_PRAGMA_ID) WN_pragma(wn);
      INT32 arg1 = WN_pragma_arg1(wn);
      INT32 arg2 = WN_pragma_arg2(wn);
      if (!first) fprintf(TFile, "\t\t");
      if ((UINT32)pragma >= (UINT32)MAX_WN_PRAGMA) {
	fprintf(TFile, "%d", pragma);
      } else {
	fprintf(TFile, "%s", WN_pragmas[WN_pragma(wn)].name);
      }
      switch (pragma) {
      case WN_PRAGMA_MIPS_FREQUENCY_HINT:
	switch (arg1) {
	case FREQUENCY_HINT_NEVER:
	  fprintf(TFile, " NEVER\n");
	  break;
	case FREQUENCY_HINT_INIT:
	  fprintf(TFile, " INIT\n");
	  break;
	case FREQUENCY_HINT_FREQUENT:
	  fprintf(TFile, " FREQUENT\n");
	  break;
	default:
	  fprintf(TFile, " %d\n", arg1);
	  break;
	}
	break;

      default:
	fprintf(TFile, " arg1 = %d, arg2 = %d\n", arg1, arg2);
	break;
      }
      first = FALSE;
    }
  }
}

/* ====================================================================
 *
 * Trace_BB / Print_BB / Print_All_BBs
 *
 * Print a BB (sequence of BBs) to the trace file.
 *
 * ====================================================================
 */

void Trace_BB ( BB *bp, char *msg )
{
  fprintf ( TFile, "%sBB:%d  %s\n%s", DBar, BB_id(bp), msg, DBar );
  Print_BB_Header ( bp, FALSE, TRUE );
  Print_BB_Pragmas ( bp );
  fprintf ( TFile, "\n" );
  NOTE_BB_Act(bp, NOTE_PRINT_TO_FILE, TFile);
  FREQ_Print_BB_Note(bp, TFile);
  if (BB_first_op(bp))	Print_OPs (BB_first_op(bp));
  fprintf ( TFile, "%s\n", DBar );
} 

/* ================================================================= */

/* ====================================================================
 *
 * Get_Procedure_Name
 *
 *   Helper rotuines to get the procedure name - separate from the rest
 *   of the BB text.
 *
 * ====================================================================
 */
char *
Get_Procedure_Name ( BB *bp )
{
  ANNOTATION *ant = ANNOT_Get (BB_annotations(bp), ANNOT_ENTRYINFO);
  ENTRYINFO *ent = ANNOT_entryinfo (ant);
  return ST_name(ENTRYINFO_name(ent));
}

char *
Get_Procedure_Name ( void )
{
  for (BB* bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    if ( BB_entry(bb) ) {
      return Get_Procedure_Name(bb);
    }
  }
  return NULL;
}

void Print_BB ( BB *bp )
{
  BBLIST *bl;
  if ( BB_entry(bp) ) {
    fprintf ( TFile, "\n\t.proc  %s#\n", Get_Procedure_Name(bp) );
  }
  fprintf ( TFile, "%s", SBar);
  fprintf ( TFile, "// Block: %d", BB_id(bp) );
  fprintf ( TFile, " Pred:" );
  FOR_ALL_BB_PREDS (bp, bl) {
    fprintf ( TFile, " %d", BB_id(BBLIST_item(bl)));
  }
  fprintf ( TFile, " Succ:" );
  FOR_ALL_BB_SUCCS (bp, bl) {
    fprintf ( TFile, " %d", BB_id(BBLIST_item(bl)));
  }
  fprintf ( TFile, "\n" );
  fprintf ( TFile, "%s", SBar );
  Print_BB_Header ( bp, FALSE, TRUE );
  Print_BB_Pragmas ( bp );
  fprintf ( TFile, "\n" );
  NOTE_BB_Act(bp, NOTE_PRINT_TO_FILE, TFile);
  FREQ_Print_BB_Note(bp, TFile);
  if (BB_first_op(bp))	Print_OPs (BB_first_op(bp));
#if defined(VENDOR_OSP)
  if (BB_exit(bp) && !BB_call(bp)) { fprintf ( TFile, "\n\t.endp\n" ); }
#endif
} 

/* ================================================================= */

void Print_BB_No_Srclines ( BB *bp )
{
  fprintf ( TFile, "%sBB:%d \n%s", SBar, BB_id(bp), SBar );
  Print_BB_Header ( bp, FALSE, TRUE );
  Print_BB_Pragmas ( bp );
  fprintf ( TFile, "\n" );
  NOTE_BB_Act(bp, NOTE_PRINT_TO_FILE, TFile);
  FREQ_Print_BB_Note(bp, TFile);
  if (BB_first_op(bp))	Print_OPs_No_SrcLines(BB_first_op(bp));
} 

// Debugging routine
void dump_bb (BB *bb)
{
   FILE *f;
   f = TFile;
   Set_Trace_File_internal(stdout);
   Print_BB_No_Srclines(bb);
   Set_Trace_File_internal(f);
}

void dump_bbs (BB *bb)
{
   FILE *f;
   f = TFile;
   Set_Trace_File_internal(stdout);
   for (;bb;bb=bb->next)Print_BB_No_Srclines(bb);
   Set_Trace_File_internal(f);
}

/* ================================================================= */

void Print_BB_by_id ( mBB_NUM id) 
{
  BB *bp;

  for (bp = REGION_First_BB; bp; bp = BB_next(bp)) {
    if (bp->id == id){
      Print_BB ( bp );
      fprintf ( TFile,"\n" );
      break;
    }
  }
}
/* ================================================================= */

void Print_All_BBs ( void ) 
{
  BB *bp;

  for (bp = REGION_First_BB; bp; bp = BB_next(bp)) {
    Print_BB ( bp );
    fprintf ( TFile,"\n" );
  }
}
/* ================================================================= */

void Print_All_BB_Headers ( void )
{
  BB *bp;

  for (bp = REGION_First_BB; bp; bp = BB_next(bp)) {
      fprintf ( TFile, "%sBB:%d \n%s", SBar, BB_id(bp), SBar );
      Print_BB_Header ( bp, FALSE, FALSE );
      fprintf ( TFile,"\n" );
  }
}

/* ====================================================================
 *
 * Print_Entry_Chain
 *
 * Print the list of entry BBs to the current PU.
 *
 * ====================================================================
 */

void
Print_Entry_Chain ( char *banner )
{
  BB_LIST *bbl;

  fprintf ( TFile, "\n%s Entry chain -- %s\n%s\n",
	    DBar, banner, DBar );

  for (bbl = Entry_BB_Head; bbl; bbl = BB_LIST_rest(bbl)) {
    Print_BB_Header ( BB_LIST_first(bbl), TRUE, FALSE );
    fprintf ( TFile,"\n" );
  }
}


/* ====================================================================
 *
 * Print_Flow_Graph
 *
 * Print the flow graph information from the BB headers of the current
 * PU.
 *
 * ====================================================================
 */

void
Print_Flow_Graph ( char *banner, BOOL verbose )
{
  BB *bb;

  fprintf ( TFile, "\n%s Flow graph -- %s\n%s\n",
	    DBar, banner, DBar );

  for ( bb = REGION_First_BB; bb; bb = BB_next(bb)) {
    Print_BB_Header ( bb, !verbose, verbose );
    fprintf ( TFile,"\n" );
  }
}

/* ====================================================================
 *
 * BB_branch_op
 *
 * Return the terminating branch OP in a given BB
 *
 * ====================================================================
 */

OP *
BB_branch_op( BB *bb )
{
  OP *op = BB_last_op(bb);

  /* Test the last two OPs looking for the terminating branch (it may not 
   * be the last OP if we have put something in the delay slot).
   */
  if (op) {
    if (OP_br(op)) return op;

    if (PROC_has_branch_delay_slot()) {
      op = OP_prev(op);
      if (op && OP_br(op)) return op;
    }
  }

  return NULL;
}


#ifdef TARG_IA64
/* ====================================================================
 *
 * Last_Non_Nop_op
 *
 * Return the last non nop OP in a given BB   
 *
 * ====================================================================
 */

OP *
Last_Non_Nop_op( BB *bb )
{
  OP *op;
  for (op =BB_last_op(bb); op; op = OP_prev(op)) {
      if (!(OP_noop(op))) return op;
  }
  return NULL;
 
}

OP *
BB_Last_chk_op( BB *bb )
{
  
  OP *op = Last_Non_Nop_op(bb);

  if (!op || !OP_chk(op)){
      return NULL;
  } else {
      return op;
  }
  return NULL;
}
#endif

/* ====================================================================
 *
 * BB_xfer_op
 *
 * Return the terminating xfer OP in a given BB
 *
 * ====================================================================
 */

OP*
BB_xfer_op( BB *bb )
{
  OP *op = BB_last_op(bb);

  /* Test the last two OPs looking for the terminating xfer OP (it may not
   * be the last OP if we have put something in the delay slot).
   */
  if (op) {
    if (OP_xfer(op)) return op;

    if (PROC_has_branch_delay_slot()) {
      op = OP_prev(op);
      if (op && OP_xfer(op)) return op;
    }
  }

  return NULL;
}

/* ====================================================================
 *
 * BB_call_op
 *
 * Return the call OP in a given BB
 *
 * ====================================================================
 */

OP*
BB_call_op( BB *bb )
{
  if(!BB_call(bb)) return NULL;
  OP *op = BB_last_op(bb);

  /* Test the last two OPs looking for the terminating xfer OP (it may not
   * be the last OP if we have put something in the delay slot).
   */
  if (op) {
    if (OP_call(op)) return op;

    if (PROC_has_branch_delay_slot()) {
      op = OP_prev(op);
      if (op && OP_call(op)) return op;
    }
  }

  return NULL;
}

/* ====================================================================
 *
 * BB_copy_xfer_op
 *
 * Return the last copy or xfer OP in a given BB
 *
 * ====================================================================
 */

OP*
BB_copy_xfer_op( BB *bb )
{
  OP *op, *prev_op;

  op = BB_last_op(bb);

#define OP_copy_xfer(o) (OP_xfer(o) || OP_copy(o) || OP_glue(o))

  /* Test the last two OPs looking for the terminating xfer OP (it may not
   * be the last OP if we have put something in the delay slot).
   */
  if (op) {

    // check for any real ops which sometime get placed in the delay slots.
    BOOL real_last_op = !OP_copy_xfer(op);

    prev_op = OP_prev(op);
    if (OP_copy_xfer(op)) {
      if (prev_op == NULL || !OP_copy_xfer(prev_op))
	return op;
    }

    if (prev_op) {
      op = OP_prev(prev_op);
      if (OP_copy_xfer(prev_op)) {
	if (op == NULL || !OP_copy_xfer(op) ||

            // Check for any real ops which sometime get placed in the delay
            // slots. If yes, avoid proceeding further.
            (OP_xfer(prev_op) && real_last_op))
	  return prev_op;
      } else return NULL;
      
      // iterate until you find the last op which isn't copy/glue op
      for (;op != NULL; op = prev_op) {
	prev_op = OP_prev(op);
	if (prev_op == NULL ||
	    (!OP_copy(prev_op) && !OP_glue(prev_op)))
	  return op;
      }
    }
  }

  return NULL;
}

UINT16 BB_New_Op_Map_Idx(BB *bb)
{
  mUINT16 result = bb->next_op_map_idx++;
      
  /* Ran out of idxs.  We could reuse idxs by keeping track of used ones
   * in a bitvector (per BB), then invalidating old map entries (probably
   * quickest to do via a generation count per idx).
   */
  FmtAssert((result+1) != 0, ("ran out of OP_MAP idx's"));
  return result;
}


/* Add an annotation of the given type to the BB */
void
BB_Add_Annotation (BB *bb, ANNOTATION_KIND kind, void *info)
{
  switch (kind) {
  case ANNOT_LABEL:
    Set_BB_has_label(bb); break;
  case ANNOT_PRAGMA:
    Set_BB_has_pragma(bb); break;
  case ANNOT_ENTRYINFO:
    Set_BB_entry(bb); break;
  case ANNOT_EXITINFO:
    Set_BB_exit(bb); break;
  case ANNOT_NOTE:
    Set_BB_has_note(bb); break;
  case ANNOT_LOOPINFO:
    Set_BB_loop_head_bb(bb, bb);
    break;
  case ANNOT_CALLINFO:
    Set_BB_call(bb); break;
  case ANNOT_ASMINFO:
    Set_BB_asm(bb); break;
  case ANNOT_SWITCH:
    break;
  case ANNOT_ROTATING_KERNEL:
    Set_BB_rotating_kernel(bb);
    break;
  case ANNOT_INLINE:
    break;
  default:
    FmtAssert(FALSE, ("unexpected annotation kind: %d", kind));
    /*NOTREACHED*/
  }
  BB_annotations(bb) = ANNOT_Add(BB_annotations(bb), kind, info,
				 BB_MEM_pool_ptr);
}


/* Copy annotations of a given type from one BB to another */
INT BB_Copy_Annotations(BB *to_bb, BB *from_bb, ANNOTATION_KIND kind)
{
  ANNOTATION *ant;
  INT count = 0;

  for (ant = ANNOT_First(BB_annotations(from_bb), kind);
       ant;
       ant = ANNOT_Next(ant, kind))
  {
    BB_Add_Annotation(to_bb, kind, ANNOT_info(ant));
    if( kind == ANNOT_LABEL ){
        Set_Label_BB( ANNOT_label(ant), to_bb );
    }
    ++count;
  }

  return count;
}

/* Copy all annotations from <from_bb> to <to_bb>, and returns 
 * the number of annotations copied 
 */
INT BB_Copy_All_Annotations (BB *to_bb, BB *from_bb) {
   INT32 cnt = 0;
   ANNOTATION *ant = BB_annotations(from_bb);
   while (ant) {
      cnt ++; 
      BB_Add_Annotation(to_bb, ANNOT_kind(ant), ANNOT_info(ant));
      if(ANNOT_kind(ant) == ANNOT_LABEL) {
        Set_Label_BB( ANNOT_label(ant), to_bb );
      }
      ant = ANNOT_next(ant); 
   }
   return cnt;
}

OP *
BB_entry_sp_adj_op (BB *bb)
{
  ANNOTATION *ant = ANNOT_Get (BB_annotations(bb), ANNOT_ENTRYINFO);
  return (ant != NULL) ? ENTRYINFO_sp_adj(ANNOT_entryinfo(ant)) : NULL;
}

OP *
BB_exit_sp_adj_op (BB *bb)
{
  ANNOTATION *ant = ANNOT_Get (BB_annotations(bb), ANNOT_EXITINFO);
  return (ant != NULL) ? EXITINFO_sp_adj(ANNOT_exitinfo(ant)) : NULL;
}

void Set_BB_entry_sp_adj_op (BB *bb, OP *op)
{
  ANNOTATION *ant = ANNOT_Get (BB_annotations(bb), ANNOT_ENTRYINFO);

  FmtAssert(ant, ("expected entry annotation"));

  ENTRYINFO_sp_adj(ANNOT_entryinfo(ant)) = op;
}

void Set_BB_exit_sp_adj_op (BB *bb, OP *op)
{
  ANNOTATION *ant = ANNOT_Get (BB_annotations(bb), ANNOT_EXITINFO);

  FmtAssert(ant, ("expected entry annotation"));

  EXITINFO_sp_adj(ANNOT_exitinfo(ant)) = op;
}


BOOL Is_Label_For_BB(LABEL_IDX label, BB *bb)
/* -----------------------------------------------------------------------
 * See "bb.h" for interface specification.
 * -----------------------------------------------------------------------
 */
{
  ANNOTATION *ant = ANNOT_First(BB_annotations(bb), ANNOT_LABEL);

  if (BB_has_label(bb) && ant == NULL) {
    DevWarn("BB_has_label(BB:%d) set, but found no ANNOT_LABEL", BB_id(bb));
    Reset_BB_has_label(bb);
  } else if (ant && !BB_has_label(bb)) {
    DevWarn("BB:%d has label(s), but BB_has_label not set", BB_id(bb));
    Set_BB_has_label(bb);
  }
  
  while (ant) {
    if (label == ANNOT_label(ant)) return TRUE;
    ant = ANNOT_Next(ant, ANNOT_LABEL);
  }
  return FALSE;
}



/* ====================================================================
 *
 * Gen_Label_For_BB
 *
 * Create a label entry in the symbol table for the given BB, attach it
 * to the BB, and return it.  If there already is one, don't create
 * another.
 *
 * ====================================================================
 */

LABEL_IDX
Gen_Label_For_BB(BB *bb)
{
  char *buf;
  LABEL_IDX lab;
  LABEL *label;
#define  EXTRA_NAME_LEN  32

  FmtAssert (BB_id(bb) != 0, ("Gen_Label_For_BB: BB_id not set for BB"));

  /* Check for an existing label: */
  if (BB_has_label(bb)) {
    ANNOTATION *ant = ANNOT_First(BB_annotations(bb), ANNOT_LABEL);
    if (ant == NULL) {
      DevWarn("BB_has_label(BB:%d) set, but found no ANNOT_LABEL", BB_id(bb));
    } else {
      /* Return first label */
      return ANNOT_label(ant);
    }
  }

  /* Name this label: */
  buf = (char *)alloca(strlen(Cur_PU_Name) + EXTRA_NAME_LEN);
  sprintf(buf, BB_Label_Format, BB_id(bb), Cur_PU_Name);

  label = &New_LABEL(CURRENT_SYMTAB, lab);
  LABEL_Init (*label, Save_Str(buf), LKIND_DEFAULT);

  Set_Label_BB (lab, bb);
  BB_Add_Annotation (bb, ANNOT_LABEL, (void *)(INTPTR)lab);
  return lab;

#undef EXTRA_NAME_LEN
}


/* =======================================================================
 *
 *  BB_Other_Successor
 *
 *  See interface description.
 *
 * =======================================================================
 */
BB *
BB_Other_Successor(
  BB *bb,
  BB *succ)
{
  BBLIST *bblst;
  BB *succ2 = NULL;

  FOR_ALL_BB_SUCCS(bb, bblst) {
    BB *sbb = BBLIST_item(bblst);
    if (sbb == succ) continue;
    FmtAssert(succ2 == NULL, ("multiple other successors"));
    succ2 = sbb;
  }

  FmtAssert(succ2 != NULL, ("no other successor"));

  return succ2;
}

/* =======================================================================
 *
 *  BB_Other_Predecessor
 *
 *  See interface description.
 *
 * =======================================================================
 */
BB *
BB_Other_Predecessor(
  BB *bb,
  BB *pred)
{
  BBLIST *bblst;
  BB *pred2 = NULL;

  FOR_ALL_BB_PREDS(bb, bblst) {
    BB *pbb = BBLIST_item(bblst);
    if (pbb == pred) continue;
    FmtAssert(pred2 == NULL, ("multiple other predecessors"));
    pred2 = pbb;
  }

  FmtAssert(pred2 != NULL, ("no other predecessor"));

  return pred2;
}

/* =======================================================================
 *
 *  BB_Retarget_Branch
 *
 * Requires: <bb> is in BB_preds(from)
 *
 * If <bb> branches to <from>, make it branch to <to> instead, updating
 * pred/succ lists and frequency info and returning TRUE.  Otherwise
 * return FALSE.
 *
 * Note: Since this deletes <bb> from BB_preds(from), be careful when
 *	 calling this from a loop over the BB_preds of <from>.  Need to
 *	 grab the BBLIST_next pointer *before* calling this routine.
 *	 So don't call this from inside FOR_ALL_BB_PREDS(from, x) or
 *	 FOR_ALL_BBLIST_ITEMS(BB_preds(from), x).
 *
 * =======================================================================
 */
BOOL
BB_Retarget_Branch(BB *bb, BB *from, BB *to)
{
  OP *br_op = BB_branch_op(bb);

  if (br_op) {
    INT opnd;
    INT opnd_count;
    CGTARG_Branch_Info(br_op, &opnd, &opnd_count);
    if (opnd_count > 0) {

      /* Direct branch
       */
      TN *br_targ = OP_opnd(br_op, opnd);
      Is_True(opnd_count == 1, ("Branch with multiple bbs"));
      if (Is_Label_For_BB(TN_label(br_targ), from)) {
        LABEL_IDX label = Gen_Label_For_BB(to);
        Set_OP_opnd(br_op, opnd, Gen_Label_TN(label,0));
        Change_Succ(bb, from, to);
        return TRUE;
      }
    } else if (ANNOT_Get(BB_annotations(bb), ANNOT_SWITCH)) {

      /* Indirect branch (switch statement).
       */
      WN *br_wn = BB_branch_wn(bb);
      Is_True(br_wn,
                ("indirect branch ending BB:%d has no associated WHIRL node",
                 BB_id(bb)));
      ST * const listvar = WN_st(br_wn);
      Change_Switchtable_Entries(listvar, from, to);
      Change_Succ(bb, from, to);
      return TRUE;
    } else {

      /* Indirect branch (indirect goto, i.e AGOTO). 
       *
       * In theory this could be handled by modifying all places the label 
       * for the <from> block is used, but in practice this is very difficult 
       * and probably not worth it.
       */
    }
  }
  return FALSE;
}

/* =======================================================================
 *
 *  BB_Can_Retarget_Branch
 *
 *  Indicate if the branch ending <bb> can be retargeted from <from>.
 *
 * =======================================================================
 */
BOOL
BB_Can_Retarget_Branch(BB *bb, BB *from)
{
  OP *br_op = BB_branch_op(bb);

  if (br_op) {
    INT opnd;
    INT opnd_count;
    CGTARG_Branch_Info(br_op, &opnd, &opnd_count);
    if (opnd_count > 0) {

      /* Direct branch
       */
      TN *br_targ = OP_opnd(br_op, opnd);
      Is_True(opnd_count == 1, ("Branch with multiple bbs"));
      return Is_Label_For_BB(TN_label(br_targ), from);
    } else if (ANNOT_Get(BB_annotations(bb), ANNOT_SWITCH)) {

      /* Indirect branch (switch statement).
       */
      WN *br_wn = BB_branch_wn(bb);
      Is_True(br_wn,
                ("indirect branch ending BB:%d has no associated WHIRL node",
                 BB_id(bb)));
      ST * const listvar = WN_st(br_wn);
      return Find_INITO_For_Symbol(listvar) != (INITO_IDX)0;
    } else {

      /* Indirect branch (indirect goto, i.e AGOTO). 
       *
       * In theory this could be handled by modifying all places the label 
       * for the <from> block is used, but in practice this is very difficult 
       * and probably not worth it.
       */
    }
  }
  return FALSE;
}

/* =======================================================================
 *
 *  BB_Unique_Successor
 *
 *  If bb has a unique successor, return it.  Otherwise return NULL.
 *
 * =======================================================================
 */
BB *BB_Unique_Successor( BB *bb )
{
  BBLIST *slist = BB_succs(bb);
  BB *succ = NULL;

  if (BBlist_Has_One_Element(slist) && BBLIST_item(slist) != bb) {
    succ = BBLIST_item(slist);
  }
  return succ;
}

void
Remove_Explicit_Branch (BB *bb)
/* -----------------------------------------------------------------------
 * Remove useless explicit branch to BB_next(bb) 
 * -----------------------------------------------------------------------
 */
{
  if ( bb == NULL) return;
  BB *next = BB_next(bb);

  if (next && BB_Find_Succ(bb, next)) {
    /* Make sure it's not a branch target (direct or indirect). */
    OP *br_op = BB_branch_op(bb);
    if (br_op) {
      INT tfirst, tcount;
      CGTARG_Branch_Info(br_op, &tfirst, &tcount);
      if (tcount != 0) {
        TN *dest = OP_opnd(br_op, tfirst);
        DevAssert(tcount == 1, ("%d branch targets, expected 1", tcount));
        DevAssert(TN_is_label(dest), ("expected label"));
        if (Is_Label_For_BB(TN_label(dest), next)) {
          /* Remove useless explicit branch to <next> */
          BB_Remove_Op(bb, br_op);
        } else {
          DevAssert(OP_cond(br_op), ("BB_succs(BB:%d) wrongly contains BB:%d",
                                     BB_id(bb), BB_id(next)));
        }
      }
    }
  }
}

/* =======================================================================
 *
 *  BB_Fall_Thru_Successor
 *
 *  Returns the fall through control flow successor of <bb>, or NULL if there
 *  is none.
 *
 * =======================================================================
 */
BB *BB_Fall_Thru_Successor( BB *bb )
{
  BBLIST *list = BBlist_Fall_Thru_Succ(bb);
  return list ? BBLIST_item(list) : NULL;
} 

/* =======================================================================
 *
 *  BB_Fall_Thru_Predecessor
 *
 *  If bb has a unique successor, return it.  Otherwise return NULL.
 *
 * =======================================================================
 */
BB *BB_Fall_Thru_Predecessor( BB *bb )
{
  BBLIST *list = BBlist_Fall_Thru_Pred(bb);
  return list ? BBLIST_item(list) : NULL;
} 


/* =======================================================================
 *
 *  BB_Unique_Successor_Not_In_Set
 *
 *  If bb has exactly one successor not in the set described by map.
 *  Otherwise return NULL.
 *
 * =======================================================================
 */
BB *BB_Unique_Successor_Not_In_Set( BB *bb, BB_MAP map )
{
  BBLIST *slist;
  BB *succ;
  BB *ans = NULL;
  BOOL found = FALSE;

  for ( slist = BB_succs( bb ); slist; slist = BBLIST_next( slist ) ) {
    succ = BBLIST_item( slist );
    if ( BB_MAP32_Get( map, succ) )
      continue;
    if ( found )
      return NULL;
    found = TRUE;
    ans = succ;
  }
  return ans;
}

/* =======================================================================
 *
 *  BB_Unique_Predecessor
 *
 *  If bb has a unique predecessor, return it.  Otherwise return NULL.
 *
 * =======================================================================
 */
BB *BB_Unique_Predecessor( BB *bb )
{
  BBLIST *plist = BB_preds( bb );
  BB *pred = NULL;

  if (BBlist_Has_One_Element(plist) && BBLIST_item(plist) != bb) {
    pred = BBLIST_item(plist);
  }
  return pred;
}

/* =======================================================================
 *
 *  BB_Unique_Source
 *
 *  If bb has a unique source, return it. A unique source is an unique
 *  BB which controls the execution of this <bb> even though there may
 *  exist multiple predecessors. If doesn;t exist, return NULL.
 *
 * =======================================================================
 */
BB *BB_Unique_Source( BB *bb )
{
  BBLIST *bl;
  BB *src = NULL;
  INT num_src = 0;

  FOR_ALL_BB_PREDS(bb, bl) {
    BB *pbb = BBLIST_item(bl);
    if (BB_branch_op(pbb)) {
      src = pbb;
      num_src += 1;
    }
  }
  return (num_src == 1) ? src : NULL;
}


/* reset the BB_unreachable flag for <bb> and all its successors. */
static void
Mark_Reachable (BB *bb)
{
  BBLIST *blst;
  
  Reset_BB_unreachable (bb);

  FOR_ALL_BB_SUCCS (bb, blst) {
    BB *succ = BBLIST_item(blst);
    if (BB_unreachable(succ)) {
      Mark_Reachable (succ);
    }
  }
}


/* see interface in bb.h */
void
BB_Mark_Unreachable_Blocks (void)
{
  BB *bb;

  /* In this first pass, set all BBs as unreachable. */
  for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    Set_BB_unreachable (bb);
  }
  
  /* Reset the BB_unreachable flag for all basic blocks that can be
   * reached from any of the entry points or exception labels.
   */
  if (Compiling_Proper_REGION) {
    Mark_Reachable (CGRIN_entry(RID_Find_Cginfo(REGION_First_BB)));
  }
  for (bb = REGION_First_BB; bb; bb = BB_next(bb)) {

    /* If we've already noted to keep this BB then just continue
     * with the next BB -- calling Mark_Reachable would be redundant.
     */
    if (!BB_unreachable(bb)) continue;

#ifdef KEY
    if (BB_has_non_local_label(bb)) {
      Mark_Reachable(bb);
      continue;
    }
#endif

    if (!BB_entry(bb)) continue;

    ANNOTATION *ant = ANNOT_Get (BB_annotations(bb), ANNOT_ENTRYINFO);
    ENTRYINFO *ent = ANNOT_entryinfo(ant);
    ST *entry_sym = ENTRYINFO_name(ent);
    if (ST_is_not_used(entry_sym)) continue;

    Mark_Reachable(bb);
  }
}


static INT32 map_depth_first(BB_MAP map, BB_SET *region, BB *bb, INT32 max_id)
/* -----------------------------------------------------------------------
 * Workhorse for BB_Depth_First_Map.
 * -----------------------------------------------------------------------
 */
{
  BBLIST *succs;

  Is_True(BB_MAP32_Get(map, bb) == 0, ("BB_Depth_First_Map visited BB:%d twice", BB_id(bb)));

  BB_MAP32_Set(map, bb, ++max_id);

  /* Recursively visit (once) all the successors of this bb in the region.
   */
  FOR_ALL_BB_SUCCS(bb, succs) {
    BB *succ = BBLIST_item(succs);
    if ((region == NULL || BB_SET_MemberP(region, succ)) &&
	BB_MAP32_Get(map, succ) == 0)
      max_id = map_depth_first(map, region, succ, max_id);
  }

  return max_id;
}


BB_MAP BB_Depth_First_Map(BB_SET *region, BB *entry)
/* -----------------------------------------------------------------------
 * See "bb.h" for interface specification.
 * -----------------------------------------------------------------------
 */
{
  BB_MAP dfo_map = BB_MAP32_Create();

  Is_True(region == NULL || entry, ("<entry> not specified"));
  Is_True(region == NULL || BB_SET_MemberP(region, entry),
	    ("<entry> not in <region>"));

  if (region) {
    map_depth_first(dfo_map, region, entry, 0);
  } else if (Compiling_Proper_REGION) {
    map_depth_first(dfo_map, region,
		    CGRIN_entry(RID_Find_Cginfo(REGION_First_BB)), 0);
  } else {
    BB_LIST *entries;
    INT32 max_id = 0;
    for (entries = Entry_BB_Head; entries; entries = BB_LIST_rest(entries)){
      // when compile with -fno-execptions, the entry block of EH handler possibly 
      // belongs to two separate closure sets of successor relation of Entry_BB_Head and
      // BB_LIST_rest(Entry_BB_Head). So needn't do map_depth_first twice for the same BB.
      FmtAssert ((region == NULL || BB_SET_MemberP(region, BB_LIST_first(entries))),
		 ("BB_Depth_First_Map visited BB:%d twice", BB_id(BB_LIST_first(entries)))); 
#ifdef KEY
      /* bug#1458
	 Don't visit an unrecognizable region twice.
       */
      if( BB_MAP32_Get( dfo_map, BB_LIST_first(entries) ) != 0 ){
	FmtAssert( region == NULL,
		   ("BB_Depth_First_Map visited a region twice") );
	continue;
      }
#endif
      max_id = map_depth_first(dfo_map, region, BB_LIST_first(entries),
			       max_id);
    }
#ifdef KEY
    // Visit predecessor-less BBs that are non-local goto targets.  They behave
    // like entry points.
    if (PU_Has_Nonlocal_Goto_Target) {
      BB *bb;
      for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
	if (!BB_entry(bb) &&
	    BB_has_non_local_label(bb) &&
	    BB_preds(bb) == NULL) {
	  // Don't visit an unrecognizable region twice.  (Just do what above
	  // does.)
	  if (BB_MAP32_Get(dfo_map, bb) != 0) {
	    FmtAssert(region == NULL,
		      ("BB_Depth_First_Map visited a region twice"));
	    continue;
	  }
	  max_id = map_depth_first(dfo_map, region, bb, max_id);
	}
      }
    }
#endif
  }
  return dfo_map;
}


static BOOL all_bbs_mapped(BBLIST *bbs, BB_MAP map)
/* -----------------------------------------------------------------------
 * Return TRUE iff every BB in <bbs> has a nonzero entry in <map>.
 * -----------------------------------------------------------------------
 */
{
  while (bbs) {
    if (BB_MAP32_Get(map, BBLIST_item(bbs)) == 0)
      return FALSE;
    bbs = BBLIST_next(bbs);
  }
  return TRUE;
}


static INT32 map_topologically(BB_MAP map, BB_SET *region, BB *bb,
			       INT32 max_id)
/* -----------------------------------------------------------------------
 * Workhorse for BB_Topological_Map.
 * -----------------------------------------------------------------------
 */
{
  BBLIST *succs;
  BB *fall_thru = NULL;

  BB_MAP32_Set(map, bb, ++max_id);

  /* If the next BB in the chain is a successor, try to visit 
   * it first.  Various clients prefer this.
   */
  if (BB_next(bb) && BB_Find_Succ(bb, BB_next(bb)))
    fall_thru = BB_next(bb);
  if (fall_thru && BB_MAP32_Get(map, fall_thru) == 0 &&
      (region == NULL || BB_SET_MemberP(region, fall_thru)) &&
      all_bbs_mapped(BB_preds(fall_thru), map))
    /* <bb> and <fall_thru> already topologically ordered. */
    max_id = map_topologically(map, region, fall_thru, max_id);

  /* Try to find successors of <bb> that can be next in topo order.
   */
  FOR_ALL_BB_SUCCS(bb, succs) {
    BB *succ = BBLIST_item(succs);
    if (succ != fall_thru && BB_MAP32_Get(map, succ) == 0 &&
	(region == NULL || BB_SET_MemberP(region, succ)) &&
	all_bbs_mapped(BB_preds(succ), map))
      /* <succ> can be next in topological ordering. */
      max_id = map_topologically(map, region, succ, max_id);
  }

  return max_id;
}


BB_MAP BB_Topological_Map(BB_SET *region, BB *entry)
/* -----------------------------------------------------------------------
 * See "bb.h" for interface specification.
 * -----------------------------------------------------------------------
 */
{
  BB_MAP top_map = BB_MAP32_Create();

  Is_True(region == NULL || entry, ("<entry> not specified"));
  Is_True(region == NULL || BB_SET_MemberP(region, entry),
	    ("<entry> not in <region>"));

  if (region) {
    map_topologically(top_map, region, entry, 0);
  } else if (Compiling_Proper_REGION) {
    map_topologically(top_map, region,
		      CGRIN_entry(RID_Find_Cginfo(REGION_First_BB)), 0);
  } else {
    BB_LIST *entries;
    INT32 max_id = 0;
    for (entries = Entry_BB_Head; entries; entries = BB_LIST_rest(entries))
      max_id = map_topologically(top_map, region, BB_LIST_first(entries),
				 max_id);
  }
  return top_map;
}


/* ====================================================================
 *
 * Change_Switchtable_Entries
 *
 * Change the target basic block of one or more of the cases of a switch
 * table. This routine changes only the entries in the switch table itself.
 * Return a boolean value to indicate if we were successful.
 *
 * ====================================================================
 */
BOOL
Change_Switchtable_Entries(
    ST   *listvar,	/* ST for the switch table */
    BB   *old_target,	/* the old target basic block */
    BB   *new_target)	/* the new target basic block */
{
  BOOL      changed;
  INITV_IDX initv;
  INITO_IDX inito = Find_INITO_For_Symbol(listvar);
  LABEL_IDX lab;

  if (inito == (INITO_IDX) 0) {
    DevWarn("No INITO for listvar at line %d of %s", __LINE__, __FILE__);
    return FALSE;
  }

  /* Make the change.
   */
  changed = FALSE;
  FOREACH_INITV(INITO_val(inito), initv) {

    if (Is_Label_For_BB(INITV_lab(initv), old_target)) {
      if (!changed) {

	/* Get existing label for new target or generate a new one
	 */
	lab = Gen_Label_For_BB(new_target);

	// labels don't get allocated in _NEW_SYMTAB.

	changed = TRUE;
      }
      Set_INITV_lab(initv, lab);
    }
  }

  /* Warn if we didn't find anything to change.
   */
  if (!changed) {
    DevWarn("Switch table doesn't contain BB:%d as a target at line %d of %s", 
	    BB_id(old_target), __LINE__, __FILE__);
  }

  return changed;
}



/* ======================================================================
 * BB_Move_Delay_Slot_Op (BB *bb)
 *
 * Check if the <bb> is terminated by an OP in the delay slot of an
 * 'xfer_op'. If yes, move the delay slot OP above the xfer_op. If
 * the delay slot OP defines one of the operands of the xfer_op, copy
 * that value into a temp before the delay slot OP and rename the 
 * operand in the xfer_op.
 * ======================================================================*/
BOOL
BB_Move_Delay_Slot_Op (BB *bb)
{
  INT i;

  FmtAssert(PROC_has_branch_delay_slot(), ("no delay slot to move"));

  OP *delay_op = BB_last_op(bb);
  OP *xfer_op = NULL;

  if (delay_op != NULL) {
    xfer_op = OP_prev(delay_op);
  }
  if (xfer_op == NULL || !OP_xfer(xfer_op)) return FALSE;

  /* Move the delay slot OP above the branch */
  BB_Move_Op_Before(bb, xfer_op, bb, delay_op);

  /* if the delay slot OP defined an operand of the xfer OP, we'll have to
   * use a temp for the reference(s) in the xfer_op.
   */
  for (i = OP_results(delay_op) - 1; i >= 0; --i) {
    TN *result_tn = OP_result(delay_op,i);
    if (OP_Refs_TN (xfer_op, result_tn)) {
      OPS copy_ops = OPS_EMPTY;
      TN* tmp_tn = Dup_TN (result_tn);
      INT i;

      Exp_COPY (tmp_tn, result_tn, &copy_ops);
      BB_Insert_Ops_Before (bb, delay_op, &copy_ops);
      for (i = 0; i < OP_opnds(xfer_op); i++) {
        if (OP_opnd(xfer_op,i) == result_tn) {
	  Set_OP_opnd (xfer_op, i, tmp_tn);
	}
      }
    }
  }
  return TRUE;
}


/* =======================================================================
 *
 *  BB_Loop_Srcpos
 *
 *  See interface description.
 *
 * =======================================================================
 */
SRCPOS
BB_Loop_Srcpos(BB *bb)
{
  OP *op;
  SRCPOS srcpos;
  ANNOTATION *annot;
  LOOPINFO *loopinfo;

  /* Get the srcpos from the loopinfo if it's there.
   */
  annot = ANNOT_Get(BB_annotations(bb), ANNOT_LOOPINFO);
  loopinfo = NULL;
  srcpos = 0;
  if (annot) {
    loopinfo = ANNOT_loopinfo(annot);
    srcpos = LOOPINFO_srcpos(loopinfo);
  }

  /* If we didn't have a srcpos, scan the first BB to
   * find an OP with a srcpos.
   */
  if (srcpos == 0) {
    FOR_ALL_BB_OPs_FWD(bb, op) {
      srcpos = OP_srcpos(op);
      if (srcpos != 0) {

	/* Found one! Modify the loopinfo so we don't have to repeat
	 * this exercise.
	 */
	if (loopinfo) LOOPINFO_srcpos(loopinfo) = srcpos;
	break;
      }
    }
  }

  return srcpos;
}


/* =======================================================================
 *
 *  BB_Transfer_Entryinfo
 *
 *  Transfer the entryinfo annotation from <from> to <to>.
 *
 * =======================================================================
 */
extern void
BB_Transfer_Entryinfo(BB* from, BB* to)
{
  ANNOTATION* annot;

  Set_BB_entry(to);
  Reset_BB_entry(from);
  if (BB_handler(from)) {
    Set_BB_handler(to);
#ifdef TARG_IA64
    Reset_BB_handler(to);
#else
    Reset_BB_handler(from);
#endif
  }
  annot = ANNOT_Get(BB_annotations(from),ANNOT_ENTRYINFO);
  Is_True(annot != NULL,("No entryinfo annotation for BB%d",BB_id(from)));
  BB_Add_Annotation(to,ANNOT_ENTRYINFO,ANNOT_entryinfo(annot));
  BB_annotations(from) = ANNOT_Unlink(BB_annotations(from),annot);
}

/* =======================================================================
 *
 *  BB_Transfer_Callinfo
 *
 *  Transfer the callinfo annotation from <from> to <to>.
 *
 * =======================================================================
 */
extern void
BB_Transfer_Callinfo(BB* from, BB* to)
{
  ANNOTATION* annot;
  Reset_BB_call(from);
  annot = ANNOT_Get(BB_annotations(from),ANNOT_CALLINFO);
  Is_True(annot != NULL,("No callinfo annotation for BB%d",BB_id(from)));
  BB_Add_Annotation(to,ANNOT_CALLINFO,ANNOT_callinfo(annot));
  BB_annotations(from) = ANNOT_Unlink(BB_annotations(from),annot);
}


/* =======================================================================
 *
 *  BB_Transfer_Asminfo
 *
 *  Transfer the asminfo annotation from <from> to <to>.
 *
 * =======================================================================
 */
extern void
BB_Transfer_Asminfo (BB *from, BB *to)
{
  ANNOTATION* annot;
  Reset_BB_asm(from);
  annot = ANNOT_Get(BB_annotations(from),ANNOT_ASMINFO);
  Is_True(annot != NULL,("No asminfo annotation for BB%d",BB_id(from)));
  BB_Add_Annotation(to,ANNOT_ASMINFO,ANNOT_asminfo(annot));
  BB_annotations(from) = ANNOT_Unlink(BB_annotations(from),annot);
}


/* =======================================================================
 *
 *  BB_Transfer_Exitinfo
 *
 *  Transfer the exitinfo annotation from <from> to <to>.
 *
 * =======================================================================
 */
extern void
BB_Transfer_Exitinfo(BB* from, BB* to)
{
  ANNOTATION* annot;

  Set_BB_exit(to);
  Reset_BB_exit(from);
  annot = ANNOT_Get(BB_annotations(from),ANNOT_EXITINFO);
  Is_True(annot != NULL,("No exitinfo annotation for BB%d",BB_id(from)));
  BB_Add_Annotation(to,ANNOT_EXITINFO,ANNOT_exitinfo(annot));
  BB_annotations(from) = ANNOT_Unlink(BB_annotations(from),annot);

  /* If the exit is a tail call, transfer the call info as well.
   */
  if (BB_call(from)) {
    BB_Transfer_Callinfo(from, to);
  }
}


void Change_Succ(BB *pred, BB *old_succ, BB *new_succ)
/* -----------------------------------------------------------------------
 * See "bb.h" for interface description.
 * -----------------------------------------------------------------------
 */
{
  float adjust;
  float old_succ_freq = BB_freq(old_succ);
  BBLIST *succs = BB_Find_Succ(pred, old_succ);
  FmtAssert(succs, ("<old_succ> BB:%d not in BB_succs(BB:%d)",
		    BB_id(old_succ), BB_id(pred)));

  BBLIST_item(succs) = new_succ;
  if (FREQ_Frequencies_Computed()) {
    adjust = BB_freq(pred) * BBLIST_prob(succs);
#if !defined(TARG_SL) && !defined(KEY)   // embedded systems uses int for feedback
    if ((BB_freq(pred) == 0) && (BBLIST_prob(succs) >= 0) && 
	!(adjust == adjust)) /* the result of NaN compare will always be false */
	adjust = 0;
#endif
  } else if (BB_freq_fb_based(pred)) {
    /* Guess that P(succ) is same for all succs */
    adjust = BB_freq(pred) / BBlist_Len(BB_succs(pred));
  } else {
    adjust = 0.0F;
  }

  FmtAssert(adjust >= 0.0F, ("negative freq or probability found"));

  if (old_succ_freq < adjust) {
    if (CG_warn_bad_freqs && !FREQ_Match(old_succ_freq, adjust)) {
      DevWarn("Change_Succ: freq adjustment generated bad results");
    }
    adjust = old_succ_freq;
  }

  BB_freq(new_succ) += adjust;
  BB_freq(old_succ) -= adjust;

  BBlist_Delete_BB(&BB_preds(old_succ), pred);
  BBlist_Add_BB(&BB_preds(new_succ), pred);
}



void Change_Succ_Prob(BB *pred, BB *succ, float prob)
/* -----------------------------------------------------------------------
 * See "bb.h" for interface description.
 * -----------------------------------------------------------------------
 */
{
  BBLIST *succs = BB_Find_Succ(pred, succ);
  float old_prob;
  float factor;
  FmtAssert(succs, ("<succ> BB:%d not in BB_succs(BB:%d)",
		    BB_id(succ), BB_id(pred)));
  old_prob = BBLIST_prob(succs);
  BBLIST_prob(succs) = prob;

  factor = 1.0F + prob - old_prob;
  if (factor < 0.0F) {
    if (CG_warn_bad_freqs && !FREQ_Match(prob, old_prob)) {
      DevWarn("Change_Succ_Prob: freq adjustment generated bad results");
    }
    factor = 0.0F;
  }
  BB_freq(succ) *= factor;
}



BOOL BB_Add_Ancestors(BB_SET **set, BB *bb, BB *start_bb, MEM_POOL *pool)
/* -----------------------------------------------------------------------
 * See "bb.h" for interface description.
 * -----------------------------------------------------------------------
 */
{
  BOOL visited_twice = FALSE;
  BBLIST *blst;

  *set = BB_SET_Union1D(*set, bb, pool);
  FOR_ALL_BB_PREDS(bb, blst) {
    BB *pred = BBLIST_item(blst);
    visited_twice |= (pred == start_bb);
    if (!BB_SET_MemberP(*set, pred))
      visited_twice |= BB_Add_Ancestors(set, pred, start_bb, pool);
  }
  return visited_twice;
}


BOOL BB_Has_Exc_Label(BB *bb)
/* -----------------------------------------------------------------------
 * See "bb.h" for interface description.
 * -----------------------------------------------------------------------
 */
{
  if (BB_has_label(bb)) {
    ANNOTATION *ant;

    for (ant = ANNOT_First(BB_annotations(bb), ANNOT_LABEL);
         ant != NULL;
         ant = ANNOT_Next(ant, ANNOT_LABEL)
    ) {
      LABEL_IDX lab = ANNOT_label(ant);
      switch (LABEL_kind(Label_Table[lab])) {
      case LKIND_BEGIN_EH_RANGE:
      case LKIND_END_EH_RANGE:
	return TRUE;
      }
    }
  }

  return FALSE;
}


/* =======================================================================
 *
 *  BB_Has_Addr_Taken_Label
 *
 *  See interface description.
 *
 * =======================================================================
 */
BOOL BB_Has_Addr_Taken_Label(BB *bb)
{
  if (BB_has_label(bb)) {
    ANNOTATION *ant;

    for (ant = ANNOT_First(BB_annotations(bb), ANNOT_LABEL);
         ant != NULL;
         ant = ANNOT_Next(ant, ANNOT_LABEL)
    ) {
      LABEL_IDX lab = ANNOT_label(ant);
      if (LABEL_addr_saved(lab)) return TRUE;
    }
  }
  return FALSE;
}


/* =======================================================================
 *
 *  BB_Has_Outer_Block_Label
 *
 *  See interface description.
 *
 * =======================================================================
 */
BOOL BB_Has_Outer_Block_Label(BB *bb)
{
  if (BB_has_label(bb)) {
    ANNOTATION *ant;

    for (ant = ANNOT_First(BB_annotations(bb), ANNOT_LABEL);
         ant != NULL;
         ant = ANNOT_Next(ant, ANNOT_LABEL)
    ) {
      LABEL_IDX lab = ANNOT_label(ant);
      if (LABEL_target_of_goto_outer_block(lab)) return TRUE;
    }
  }
  return FALSE;
}


/* =======================================================================
 *
 *  BB_Is_Cold
 *
 *  See interface description.
 *
 * =======================================================================
 */
BOOL BB_Is_Cold(BB *bb)
{
  RID *r;

  /* We are part of a cold region if <bb>'s containing region is "cold"
   * or one of it's ancestors is.
   */
  for (r = BB_rid(bb); r; r = RID_parent(r)) {
    if (RID_type(r) == RID_TYPE_cold) return TRUE;
  }
  return FALSE;
}


/* =======================================================================
 *
 *  Gen_ST_For_BB
 *
 *  See interface description.
 *
 * =======================================================================
 */
ST *Gen_ST_For_BB(BB *bb)
{
  char buf[10+1];
  ST *st;
  TY_IDX ty;

  if (bb_st_map == NULL) {
    bb_st_map = BB_MAP_Create();
  } else {
    st = (ST *)BB_MAP_Get(bb_st_map, bb);
    if (st) return st;
  }

  st = New_ST (GLOBAL_SYMTAB);
  sprintf(buf, "%d", bb_st_count++);
  ty = Make_Function_Type(MTYPE_To_TY(MTYPE_V));
  ST_Init (st, Save_Str2(".S.", buf), CLASS_FUNC, SCLASS_TEXT, EXPORT_LOCAL, ty);
  Allocate_Object(st);

  BB_MAP_Set(bb_st_map, bb, (void *)st);

  return st;
}


/* =======================================================================
 *
 *  BB_st
 *
 *  See interface description.
 *
 * =======================================================================
 */
ST *BB_st(BB *bb)
{
  if (bb_st_map) return (ST *)BB_MAP_Get(bb_st_map, bb);
  return NULL;
}


#ifdef KEY
static BOOL OP_defs_argument( OP* op )
{
  for( int resnum = 0; resnum < OP_results(op); resnum++ ){
    TN* tn = OP_result( op, resnum );
    if( TN_is_dedicated( tn ) &&
	REGISTER_SET_MemberP( REGISTER_CLASS_function_argument(TN_register_class(tn)), TN_register(tn) ) )
      return TRUE;
  }

  return FALSE;
}
#endif


/* =======================================================================
 *
 *  Split_BB
 *
 *  Split large blocks at points of minimal register pressure to reduce
 *  the creation of unnecessary globals.
 *
 * =======================================================================
 */
static void Split_BB(BB *bb)
{
  OP* op;
  INT i;
  const INT len = BB_length(bb);
  const INT high  = (INT)(Split_BB_Length * 1.25);
  const INT low = (len <= high) ? (Split_BB_Length / 2) : (INT)(Split_BB_Length * .75);
  mINT8 fatpoint[ISA_REGISTER_CLASS_MAX+1];
  INT* regs_in_use = (INT *)alloca(sizeof(INT) * (len+1));
  INT* splits = (INT *)alloca(sizeof(INT) * ((len / low)+1));
  OP** op_vec = (OP **)alloca(sizeof(OP*) * (len+1));
					       
  MEM_POOL_Push(&MEM_local_pool);
  LRA_Estimate_Fat_Points(bb, fatpoint, regs_in_use, &MEM_local_pool);
  MEM_POOL_Pop(&MEM_local_pool);

  //
  // Put op's in a vector to speed up the splitting loop.
  // Skip 0th element.
  //
  op_vec[0] = NULL;
  i = 1;
  FOR_ALL_BB_OPs_FWD(bb, op) op_vec[i++] = op;

  //
  // Find all of the splits in the block.
  //
  INT min_remainder = ((high - Split_BB_Length)/2); /* The last block must be longer than this. */
  INT min_in_use = INT_MAX;
  INT high_idx = high > len ? len : high;
  INT split_idx = 0;
  INT min_idx = 0;
  for (i = low;;) {
    if (i == high_idx) {
      if (i == len) {
       // We've reached the end of the block that we want to split
       // without finding a smaller minimum.  If there is just a
       // small amount left after the point where we would split
       // the block, combine it with the previous block.
        if ((len - min_idx) < min_remainder ) {
          if (split_idx == 0) {
           // There is no advantage to splitting the block.
            return;
          }
          break;
        }
      }
#ifdef TARG_X8664
      /* The life of rflags does not across BBs.
       */
      while( min_idx <= len && OP_reads_rflags(op_vec[min_idx]) ){
	min_idx++;
      }
#endif

#ifdef KEY
      /* Fix bug#1820
	 While splitting a big bb, don't violate the assumption that
	 a definition of an argument register should be in the call block.
       */
      if( BB_call( bb ) ){
	while( min_idx > 1 &&
	       OP_defs_argument( op_vec[min_idx-1] ) ){
	  min_idx--;
	}
      }
#endif
      splits[split_idx++] = min_idx;
      if (len - min_idx <= Split_BB_Length) break;
      i = min_idx + low;
      min_in_use = INT_MAX;
      high_idx = min_idx + high;
      if (high_idx > len) high_idx = len;
    } else {
      i++;
    }
    if (regs_in_use[i] < min_in_use) {
      min_in_use = regs_in_use[i];
      min_idx = i;
      if (regs_in_use[i] < 0) {
        DevWarn("Negative registers-in-use count for OP.");
      }
    }
  }
  FmtAssert(split_idx > 0, ("failed to split BB:%d", BB_id(bb)));

  //
  // Set last "split" to one past end of the block.  Simplifies
  // op movement loop.
  //
  splits[split_idx] = len + 1;
      
  //
  // Move branches to last split.
  //
  BB* last_bb = Gen_And_Insert_BB_After(bb);
  if (BB_exit(bb)) {
    BB_Transfer_Exitinfo(bb, last_bb);
    Exit_BB_Head = BB_LIST_Delete(bb, Exit_BB_Head);
    Exit_BB_Head = BB_LIST_Push(last_bb, Exit_BB_Head, &MEM_pu_pool);
  }
  if (BB_call(bb)) {
    BB_Transfer_Callinfo(bb, last_bb);
  }
  if (BB_asm(bb)) {
    BB_Transfer_Asminfo (bb, last_bb);
  }
	
  BBLIST* nxt;
  BBLIST* succ;
  for (succ = BB_succs(bb); succ; succ = nxt) {
    BB* bb_succ = BBLIST_item(succ);
    nxt = BBLIST_next(succ);
    Unlink_Pred_Succ(bb, bb_succ);
    Link_Pred_Succ(last_bb, bb_succ);
  }

  //
  // Do the splits
  //
  INT split_id_max = split_idx - 1;
  BB* bb_prev = last_bb;
  for (i = split_id_max; i >= 0; i--) {
    BB* new_bb;

    if (i == split_id_max) {
      new_bb = last_bb;
    } else {
      new_bb = Gen_And_Insert_BB_Before(bb_prev);
      bb_prev = new_bb;
    }
	
    INT op_idx;
    for (op_idx = splits[i]; op_idx < splits[i+1]; op_idx++) {
      op = op_vec[op_idx];
#ifdef Is_True_On
      if( !BB_call( new_bb ) &&
	  BB_call( last_bb ) ){
	if( OP_defs_argument( op ) ){
	  DevWarn( "Split_BBs: argument and call are not defined at the same BB" );
	}
      }
#endif
      BB_Move_Op_To_End(new_bb, bb, op);
    }
  }

  //
  // Now, make all the splits fall through to one another.
  //
  bb_prev = bb;
  BB *bb_tmp, *bb_stop = BB_next(last_bb);
  for (bb_tmp = BB_next(bb); bb_tmp != bb_stop; bb_tmp = BB_next(bb_tmp))
  {
    Target_Simple_Fall_Through_BB(bb_prev, bb_tmp);
    bb_prev = bb_tmp;
  }

  // Bug 13664: Propagate BB_has_globals flag to new blocks for localize
  // Faster: Insert "if (BB_has_globals(bb)) Set_BB_has_globals(new_bb);"
  // in code above.
  if (BB_has_globals(bb)) {
    Reset_BB_has_globals(bb);
    for (bb_tmp = bb; bb_tmp != bb_stop; bb_tmp = BB_next(bb_tmp)) {
      BOOL has_dedicated = FALSE;
      FOR_ALL_BB_OPs_FWD(bb_tmp, op) {
	for (i = 0; i < OP_opnds(op); ++i) {
	  TN *tn = OP_opnd(op, i);
	  if (TN_is_dedicated(tn)) { has_dedicated = TRUE; break; }
	}
	if (has_dedicated) break;
	for (i = 0; i < OP_results(op); ++i) {
	  TN *tn = OP_result(op, i);
	  if (TN_is_dedicated(tn)) { has_dedicated = TRUE; break; }
	}
	if (has_dedicated) break;
      }	
      if (has_dedicated) Set_BB_has_globals(bb_tmp);
    }
  }
}


/* =======================================================================
 *
 *  Split_BBs
 *
 *  See interface description.
 *
 * =======================================================================
 */
void
Split_BBs(void)
{
  BB* bb;

  if (!Enable_BB_Splitting) return;

  for ( bb = REGION_First_BB; bb; bb = BB_next(bb) )	{
    if (BB_rid(bb) && (RID_level(BB_rid(bb)) >= RL_CGSCHED))
	// don't change bb's which have already been through CG
	continue;

    if (BB_length(bb) > Split_BB_Length) 
	Split_BB(bb);
  }
}


#ifdef TARG_IA64
/* =========================================================================
* Find_BB_Parents
*
* Return all bb's parents bb including itself.
* ==========================================================================
*/
BB_SET*
Find_BB_Parents(BB* bb)
{
  BB_SET* result = BB_SET_Create_Empty(PU_BB_Count+2, &BB_MEM_pool);
  BB_CONTAINER_ALLOC bb_mem(&BB_MEM_pool);
  BB_CONTAINER bb_vector(bb_mem);
  BB_CONTAINER::iterator iter;
  bb_vector.push_back(bb);
  BOOL bb_visited[PU_BB_Count+2];
  for(INT i = 0; i < PU_BB_Count+2; i++) bb_visited[i] = FALSE;
  bb_visited[bb->id] = TRUE;
  while(!bb_vector.empty()) {
    BBLIST* pre_bbs = NULL;
    iter = bb_vector.begin();
  	result = BB_SET_Union1D(result, *iter, &BB_MEM_pool);
    for(pre_bbs = BB_preds(*iter),iter = bb_vector.erase(iter); pre_bbs != NULL; pre_bbs = BBLIST_next(pre_bbs))
    {
      if(!BBLIST_item(pre_bbs) || bb_visited[BBLIST_item(pre_bbs)->id]) continue;
      bb_vector.push_back(BBLIST_item(pre_bbs));
      bb_visited[BBLIST_item(pre_bbs)->id] = TRUE;
    }
  }
  return result;
}
#endif

// A temp BB_SET variable reserved for the BB_REGION routines because
// creating and destroying BB_SET is an expensive operation.
// The BB_SET is associated with MEM_pu_pool!
//
struct BB_REGION_SET {
  BB_SET  *bbs;
  MEM_POOL pool;

  BOOL operator()(BB *bb) { 
    return BB_SET_MemberP(bbs, bb); 
  }

  void Set(BB *bb) { 
    bbs = BB_SET_Union1D(bbs, bb, &pool);
  }

  void Reset(BB *bb) {
    bbs = BB_SET_Difference1D(bbs, bb);
  }

  void Clear() {
    bbs = BB_SET_ClearD(bbs);
  }

  template <class BB_VECTOR> 
  void Set(const BB_VECTOR& v) { 
    for (INT i = 0; i < v.size(); i++)
      Set(v[i]);
  }

  template <class BB_VECTOR> 
  void Reset(const BB_VECTOR& v) { 
    for (INT i = 0; i < v.size(); i++)
      Reset(v[i]);
  }

  BOOL Is_empty() const {
    return BB_SET_EmptyP(bbs);
  }

  void Init() {
    if (bbs == NULL) {
      MEM_POOL_Initialize(&pool, "BB_REGION_SET pool", TRUE);
      MEM_POOL_Push(&pool);
      bbs = BB_SET_Create_Empty(PU_BB_Count+2, &pool);
    }
  }

  BB_REGION_SET():bbs(NULL) {}
};

static BB_REGION_SET region_exits;
static BB_REGION_SET region_temp;

/*========================================================================
 *
 *  BB_REGION_to_Vector
 *
 *    Append the BBs in the BB_REGION to the container.
 *
 *========================================================================
 */
void BB_REGION_to_Vector(vector<BB*>& bv, const BB_REGION& r)
{
  BB_REGION_SET region_temp;

  region_temp.Init();

  // Initialize region_exit for faster checking of exit blocks
  region_temp.Set(r.exits);

  // Depth-first traversal of the CFG to collect all BBs in the region.
  // Recursively put bbs reachable from the entries blocks without 
  // passing through an exit block into the bitset.  Process each BB
  // at most once.
  vector<BB*> stack(r.entries.begin(), r.entries.end());
  while (!stack.empty()) {
    BB *bb = stack.back();
    stack.pop_back();
    region_temp.Set(bb);
    bv.push_back(bb);  // insert into the Container
    BBLIST *succs;
    FOR_ALL_BB_SUCCS(bb, succs) {
      BB *succ = BBLIST_item(succs);
      if (!region_temp(succ))
	stack.push_back(succ);
    }
  }

  // Reset region_exit.  region_exit should always be the empty set;
  region_temp.Clear();
}
  
/*========================================================================
 *
 *  BB_REGION_to_BB_SET
 *
 *    Convert a BB_REGION representation into a BB_SET representation.
 *    The usage convention follows BB_SET_UnionD() where the pool is the
 *    MEM_POOL of bbs.
 *
 *========================================================================
 */
BB_SET *BB_REGION_to_BB_SET(BB_SET *bbs, const BB_REGION& r, MEM_POOL *pool)
{
  // Clear BB_SET first.
  BB_SET_ClearD(bbs);

  region_exits.Init();

  // Initialize region_exit for faster checking of exit blocks
  region_exits.Set(r.exits);

  // Depth-first traversal of the CFG to collect all BBs in the region.
  // Recursively put bbs reachable from the entries blocks without 
  // passing through an exit block into the bitset.  Process each BB
  // at most once.
  vector<BB*> stack(r.entries.begin(), r.entries.end());
  while (!stack.empty()) {
    BB *bb = stack.back();
    stack.pop_back();
    bbs = BB_SET_Union1D(bbs, bb, pool);
    BBLIST *succs;
    FOR_ALL_BB_SUCCS(bb, succs) {
      BB *succ = BBLIST_item(succs);
      if (!BB_SET_MemberP(bbs, succ) && !region_exits(bb))
	stack.push_back(succ);
    }
  }

  // Reset region_exit.  region_exit should always be the empty set;
  region_exits.Clear();

  return bbs;
}


/*========================================================================
 *
 *  BB_REGION::BB_REGION(BB_SET *included, MEM_POOL *pool)
 *
 *   Construction a BB_REGION from a BB_SET
 *
 *========================================================================
 */
BB_REGION::BB_REGION(BB_SET *included, MEM_POOL *pool)
  :data_allocator(pool),
   entries(0, (BB*) NULL, data_allocator),
   exits(0, (BB*)NULL, data_allocator) ,
   has_omega(false)
{
  region_exits.Init();

  BB *bb;
  FOR_ALL_BB_SET_members(included, bb) {
    BBLIST *succs;
    // If a successor of an "included" basic block is not included,
    // then it is in the exit boundary set.  Each exit block appears
    // once in the exits vector (even though it might have multiple
    // predecessors included.
    FOR_ALL_BB_SUCCS(bb, succs) {
      BB *succ = BBLIST_item(succs);
      if (!BB_SET_MemberP(included, succ) && 
	  !region_exits(succ)) {
	exits.push_back(succ);
	region_exits.Set(succ);
      }
    }
    region_exits.Reset(exits);

    // A "included" block that has at least one predecessor not included
    // is an entry block.
    BBLIST *preds;
    if (BB_preds(bb)) {
      FOR_ALL_BB_PREDS(bb, preds) {
	BB *pred = BBLIST_item(preds);
	if (!BB_SET_MemberP(included, pred)) {
	  entries.push_back(bb);
	  break;
	}
      }
    } else {
      //
      // No predecessors, so must be entry (prolog block).
      //
      entries.push_back(bb);
    }      
  }
}


/*========================================================================
 *
 *  BB_REGION::Verify()
 *
 *    Verify the invariant properties of BB_REGION.
 *
 *========================================================================
 */
void BB_REGION::Verify() const
{
  // Initialize static data structure 
  region_exits.Init();

  // entries must contain at least one basic block
  //
  FmtAssert(entries.size() >= 1, ("BB_REGION should have at least 1 entry."));

  // The temp variables used by BB_REGION routine must be left as the empty set.
  //
  FmtAssert(region_exits.Is_empty(), ("BB_REGION::Verify: temp variables in unknown state."));

  region_exits.Set(exits);

  // No basic blocks are in both entry and exit sets
  for (INT i = 0; i < entries.size(); i++) {
    FmtAssert(!region_exits(entries[i]),
	      ("BB_REGION: BB%d in both entry ad exit.", BB_id(entries[i])));
  }

  // Verify that all OPs has CG_LOOP_INFO
  if (Has_omega()) {
    
    vector<BB*> stack(entries.begin(), entries.end());
    while (!stack.empty()) {
      BB *bb = stack.back();
      stack.pop_back();
      region_exits.Set(bb);
      OP *op;
      FOR_ALL_BB_OPs(bb, op) {
	FmtAssert(_CG_LOOP_info(op),
		  ("BB_REGION: OP has no CG_LOOP_Info."));
	FmtAssert(OP_bb(op) == bb,
		  ("BB_REGION: OP_bb(op) != bb in BB%d\n", BB_id(bb)));

	for (INT opnd = 0; opnd < OP_opnds(op); opnd++) {
	  TN *tn = OP_opnd(op,opnd);
	  if (!TN_is_register(tn)) {
	    FmtAssert(OP_omega(op, opnd) == 0,
		      ("non-register TN must have zero omega."));
	  }
	}
      }
      BBLIST *succs;
      FOR_ALL_BB_SUCCS(bb, succs) {
	BB *succ = BBLIST_item(succs);
	if (!region_exits(bb))
	  stack.push_back(succ);
      }
    }
    region_exits.Clear();
  } else 
    region_exits.Reset(exits);

  Is_True(region_exits.Is_empty(), ("BB_REGION::Verify: bb_set is not empty."));
}

void BB_REGION::Print(void) const 
{
  INT i;
  FILE *f = stdout;
  fprintf(f, "Has omega = %d",has_omega);
  fprintf(f,"Entries: ");
  for (i = 0; i < entries.size(); i++) {
    fprintf(f,"%d ",BB_id(entries[i]));
  }
  fprintf(f,"\nExits: ");
  for (i = 0; i < exits.size(); i++) {
    fprintf(f,"%d ",BB_id(exits[i]));
  }
  fprintf(f,"\n");
}

#ifdef Is_True_On
void Verify_BB(BB *bb)
{
  INT id = BB_id(bb);
  FmtAssert(id >= 1 && id <= PU_BB_Count, ("Verify_BB: invalid id."));
  FmtAssert(BBvec(id) == bb, ("Verify_BB: inconsistency BB_Vec."));
}
#endif


void
draw_flow_graph(void)
{
  if (! DaVinci::enabled (TRUE)) return;

  char nlabel[10];

  MEM_POOL dv_pool;
  dv_pool.magic_num = 0;		// force it to be unintialized
  MEM_POOL_Constructor pool (&dv_pool, "DaVinci", FALSE);

  DaVinci dv (&dv_pool, NULL);

  dv.Title (Cur_PU_Name);

  // Now we start drawing
  NODE_TYPE nt_plain, nt_entry, nt_exit, nt_multi,nt, nt_call;
  EDGE_TYPE et_known;

  nt_entry.Color ("palegreen");
  nt_exit.Color ("pink");
  nt_call.Boarder(NB_DOUBLE);
  nt_multi.Color ("lightgoldenrod");
  nt_multi.Shape(NS_CIRCLE);

  dv.Graph_Begin ();

  // add all nodes
  for (BB* bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    sprintf(nlabel,"%d",BB_id(bb));
    nt = nt_plain;
    if (BBlist_Len(BB_preds(bb)) > 1 || BBlist_Len(BB_succs(bb)) > 1) nt = nt_multi;
    if (BB_entry(bb)) nt = nt_entry;
    if (BB_exit(bb))  nt = nt_exit;
    if (BB_call(bb))  nt = nt_call;
    dv.Node_Begin (NODE_ID (bb), nlabel, nt);
    BBLIST* sedge;
    FOR_ALL_BB_SUCCS(bb, sedge) {
      dv.Out_Edge (EDGE_ID (NODE_ID (bb), NODE_ID (BBLIST_item(sedge))),
		   et_known,
		   NODE_ID (BBLIST_item(sedge)));
    }
    dv.Node_End ();
  }
  dv.Graph_End ();

  dv.Event_Loop (NULL);
}


void 
verify_flow_graph(void) 
{
  MEM_POOL_Push(&MEM_local_pool);
  
  BB_SET *visited = BB_SET_Create_Empty(PU_BB_Count+2,&MEM_local_pool);

  BB *bb;
  BB *pred, *succ;
  BBLIST *bl,*bl1;
  BOOL found;

  for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    //
    // Check that the block is on the successor list of all its predecessors
    // 
    FOR_ALL_BB_PREDS(bb,bl) {
      pred = BBLIST_item(bl);
      found = FALSE;
      FOR_ALL_BB_SUCCS(pred,bl1) {
	succ = BBLIST_item(bl1);
	if (succ == bb) {
	  found = TRUE;
	  break;
	}
      }
      if (!found) {
	fprintf(TFile,"verify_flow_graph: BB%d missing from pred BB%d succ list\n",
		BB_id(bb),BB_id(pred));
      }
    }	  
	
    //
    // Check that the block is on the predecessor list of all its successors
    // 
    FOR_ALL_BB_SUCCS(bb,bl) {
      succ = BBLIST_item(bl);
      found = FALSE;
      FOR_ALL_BB_PREDS(succ,bl1) {
	pred = BBLIST_item(bl1);
	if (pred == bb) {
	  found = TRUE;
	  break;
	}
      }
      if (!found) {
	fprintf(TFile,"verify_flow_graph: BB%d missing from succ BB%d pred list\n",
		BB_id(bb), BB_id(succ));
      }
    }	  
  }
  MEM_POOL_Pop(&MEM_local_pool);
}

/*========================================================================
 *
 * VCG support for CG
 *
 *========================================================================
 */

/*========================================================================
 *
 *  Get_BB_Labels
 *
 *    Excerpt labels for the BB and append them to the stringstream.
 *
 *========================================================================
 */
void
Get_BB_Labels(BB* bb, std::stringstream &ss)
{
  if (BB_has_label(bb)) {
    ANNOTATION *ant;
    for (ant = ANNOT_First(BB_annotations(bb), ANNOT_LABEL);
	 ant != NULL;
	 ant = ANNOT_Next(ant, ANNOT_LABEL)) {
      LABEL_IDX label = ANNOT_label(ant);
      ss << LABEL_name(label);
    }
    ss << "\n";
  }
}

/*========================================================================
 *
 *  get_vcg_node
 *
 *    Create a VCG node corresponding to a BB.  This routine reuses the
 *    support to print a BB.  Basically we:
 *    - save off TFile
 *    - open a temp file
 *    - point TFile to the temp file
 *    - call the appropriate "print" command
 *    - restore TFile
 *    - close the temp file
 *    - read the temp file into a string
 *    - use that string as the VCG node info
 *    While this process is a bit convoluted:
 *    - we reuse the existing infrastructure to print BBs
 *    - VCG dumps look like regular ASCII dumps
 *    - we aovid duplicating code
 *    Unfortunately, the existing BB print routines were not written in a
 *    more modular way - i.e. the print statments are in the lowest level
 *    routines.  This was the best way to reuse the code without overhauling
 *    it.  While opening/clsong files can be innefficient, at the worst, it
 *    only imapcts the performance of an internal tracing option.
 *
 *========================================================================
 */
VCGNode*
get_vcg_node(MEM_POOL* mpool, VCGGraph& vcg, BB* bb)
{
  std::stringstream title_ss;
  title_ss << "BB:";
  title_ss << (INT32) BB_id(bb);
  char* title = (char *) MEM_POOL_Alloc(mpool, title_ss.str().size()+1);
  strcpy(title, title_ss.str().c_str());
  VCGNode* bb_node =CXX_NEW(VCGNode(title, title), mpool);

  FILE *save_f, *temp_f;
  char ch;

  save_f = TFile;
  temp_f = fopen("/tmp/vcg", "w");
  Set_Trace_File_internal(temp_f);
  if (BB_first_op(bb))	Print_OPs (BB_first_op(bb));
  Set_Trace_File_internal(save_f);
  fclose(temp_f);
  std::ifstream info1_from ("/tmp/vcg");
  std::stringstream info1_to;
  Get_BB_Labels(bb, info1_to);
  while (info1_from.get(ch)) {
    if (ch != '"') {
      info1_to.put(ch);
    }
  }
  char* info1_info = (char *) MEM_POOL_Alloc(mpool, info1_to.str().size()+1);
  strcpy(info1_info, info1_to.str().c_str());
  bb_node->info(1,info1_info);

  save_f = TFile;
  temp_f = fopen("/tmp/vcg", "w");
  Set_Trace_File_internal(temp_f);
  if (BB_first_op(bb))	Print_OPs_No_SrcLines (BB_first_op(bb));
  Set_Trace_File_internal(save_f);
  fclose(temp_f);
  std::ifstream info2_from ("/tmp/vcg");
  std::stringstream info2_to;
  Get_BB_Labels(bb, info2_to);
  while (info2_from.get(ch)) {
    if (ch != '"') {
      // Skip '"', since this deliniates the VCG string.
      info2_to.put(ch);
    }
  }
  char* info2_info = (char *) MEM_POOL_Alloc(mpool, info2_to.str().size()+1);
  strcpy(info2_info, info2_to.str().c_str());
  bb_node->info(2,info2_info);
  system("rm -f /tmp/vcg");

  vcg.addNode(*bb_node);
  return bb_node;
}

/*========================================================================
 *
 *  draw_vcg_flow_graph
 *
 *    Create a VCG graph of the CG VCG
 *
 *========================================================================
 */
void
draw_vcg_flow_graph(const char* fname)
{
  MEM_POOL vcg_pool;
  MEM_POOL_Initialize(&vcg_pool, "CFG VCG pool", FALSE);
  VCGGraph vcg("CFG VCG dump");
  int max_id = 0;
  for (BB* bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    if (BB_id(bb) > max_id) max_id = BB_id(bb);
  }
  VCGNode** vcg_nodes =
    (VCGNode **) MEM_POOL_Alloc(&vcg_pool, sizeof(VCGNode*) * (max_id + 1));

  for (BB* bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    VCGNode* vcg_node = get_vcg_node(&vcg_pool, vcg, bb);
    vcg_nodes[BB_id(bb)] = vcg_node;
  }
  for (BB* bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    BBLIST *bl;
    BB *succ;
    FOR_ALL_BB_SUCCS(bb,bl) {
      succ = BBLIST_item(bl);
      VCGEdge* edge =
	CXX_NEW(VCGEdge(vcg_nodes[BB_id(bb)]->title(),
			vcg_nodes[BB_id(succ)]->title()),
		&vcg_pool);
      edge->lineStyle(Continuous);
      vcg.addEdge(*edge);
    }
  }
  
  vcg.infoName(1, "Insts with source");
  vcg.infoName(2, "Insts without source");
  vcg.emit(fname);
  MEM_POOL_Delete(&vcg_pool);
}
