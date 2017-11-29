/*
 *  Copyright 2005-2008 NVIDIA Corporation.  All rights reserved.
 *
 *  This program is free software; you can redistribute it and/or modify it
 *  under the terms of version 2 of the GNU General Public License as
 *  published by the Free Software Foundation.
 *
 *  This program is distributed in the hope that it would be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
 *
 *  Further, this software is distributed without any warranty that it is
 *  free of the rightful claim of any third person regarding infringement 
 *  or the like.  Any license provided herein, whether implied or 
 *  otherwise, applies only to this software file.  Patent licenses, if 
 *  any, provided herein do not apply to combinations of this program with 
 *  other software, or any other product whatsoever.  
 *
 *  You should have received a copy of the GNU General Public License along
 *  with this program; if not, write the Free Software Foundation, Inc., 59
 *  Temple Place - Suite 330, Boston MA 02111-1307, USA.
 */
// When multiple definitions of a tn,
// try to create unique independent definitions,
// so each use has a single def.
// This will help future optimizations find the def easier.

#include "defs.h"
#include "tracing.h"
#include "errors.h"
#include "wn.h"
#include "mempool.h"
#include "bb.h"
#include "op.h"
#include "tn.h"
#include "cg.h"
#include "cgtarget.h"
#include "dominate.h"

static BOOL tracing = FALSE;
#define Trace(msg)	if (tracing) fprintf(TFile, msg "\n");

static MEM_POOL def_info_pool;
static TN_MAP tn_def_bbs; // list of bbs that define tn
static TN_MAP tn_use_bbs; // list of bbs that use tn

// If 2 defs have same initial value and same pred block,
// move defs to common pred block.
// Ideally wopt should handle this, but it leaves some cases.
// This may be a win if one path has a block with just the def in it,
// so can simplify control flow.  Could try to do this in cflow, 
// but we already have infrastructure here,
// and may help to create one-def property.
// Assumes tn_def_bbs already computed, will reset if merged.
static BOOL
Defs_Can_Be_Merged (TN *tn)
{
#define MAXDEFBBS 16
  BB *bb;
  BB *pred_bb;
  BB *dbbs[MAXDEFBBS]; // array of defining bbs
  BB_SET *bbdef_set = (BB_SET*) TN_MAP_Get (tn_def_bbs, tn);
  INT i = 0;
  INT n; // bbs are 0..n
  INT j;
  if (BB_SET_Size(bbdef_set) > MAXDEFBBS) {
    if (tracing) fprintf(TFile, "give up on tn%d cause %d def bbs\n", 
      TN_number(tn), BB_SET_Size(bbdef_set));
    return FALSE;	// ignore if too many defs
  }
 
  FOR_ALL_BB_SET_members (bbdef_set, bb) {
    dbbs[i] = bb;
    ++i;
  }
  n = i;
  for (i = 0; i < n; ++i) {
    pred_bb = BB_Unique_Predecessor(dbbs[i]);
    if (pred_bb == NULL) continue;
    // this optimization only works when exactly 2 succs of pred_bb,
    // each with same def.
    if (BBlist_Len(BB_succs(pred_bb)) != 2)
	continue;
    for (j = i+1; j < n; ++j) {
      if (BB_Unique_Predecessor(dbbs[j]) == pred_bb) {
	// same predecessor, now see if same value
	OP *opi, *opj; // def ops in bbs i and j
	BOOL same = TRUE;
	// find def ops in bbs
	FOR_ALL_BB_OPs_FWD (dbbs[i], opi) {
	  if (OP_Refs_TN(opi, tn))
	    same = FALSE;	// use before def, so can't move up
	  if (OP_Defs_TN(opi, tn))
	    break;
	}
	FOR_ALL_BB_OPs_FWD (dbbs[j], opj) {
	  if (OP_Refs_TN(opj, tn))
	    same = FALSE;	// use before def, so can't move up
	  if (OP_Defs_TN(opj, tn))
	    break;
	}
	FmtAssert(opi && opj, ("didn't find def in bb?"));
	if (OP_code(opi) != OP_code(opj))
	  break;
	for (INT k = 0; k < OP_opnds(opi); ++k) {
	  if (OP_opnd(opi,k) != OP_opnd(opj,k))
	    same = FALSE;
	  // for now, to avoid issues of checking dependencies when move ops,
	  // will only allow constant opnds, i.e. mov r,N.
	  if (TN_is_register(OP_opnd(opi,k)))
	    same = FALSE;
	}
	if (same) {
	  // same values so move to common predecessor
	  if (tracing) 
	    fprintf(TFile, 
	      "move def of tn %d from bbs %d and %d to pred bb %d\n", 
	      TN_number(tn), BB_id(dbbs[i]), BB_id(dbbs[j]), BB_id(pred_bb) );
	  BB_Move_Op_Before (pred_bb, BB_xfer_op(pred_bb), dbbs[i], opi);
	  BB_Remove_Op (dbbs[j], opj);
	  bbdef_set = BB_SET_Union1D(bbdef_set, pred_bb, &def_info_pool);
	  // make sure not multiple defs in bb before removing
	  FOR_ALL_BB_OPs_FWD (dbbs[i], opi) {
	    if (OP_Defs_TN(opi, tn))
	      break;
	  }
	  if (opi == NULL)
	    bbdef_set = BB_SET_Difference1D(bbdef_set, dbbs[i]);
	  FOR_ALL_BB_OPs_FWD (dbbs[j], opj) {
	    if (OP_Defs_TN(opj, tn))
	      break;
	  }
	  if (opj == NULL)
	    bbdef_set = BB_SET_Difference1D(bbdef_set, dbbs[j]);
	  TN_MAP_Set (tn_def_bbs, tn, bbdef_set);
	  return TRUE;
	}
      }
    }
  }
  return FALSE;
}

#define DEF_NOT_BEFORE_USE 0
#define DEF_BEFORE_USE 1
#define DEF_MAYBE_BEFORE_USE 2
// check if is def before use of tn in given bb.
// if multiple uses, make sure all have same def
// (e.g. if have use/def/use then def is "maybe" before use).
// return 0 for no, 1 for yes, 2 for maybe 
static INT
Def_Before_Use (TN *tn, BB *bb)
{
  OP *op;
  BOOL def = FALSE;
  INT status = -1;
  FOR_ALL_BB_OPs (bb, op) {
    if (OP_Refs_TN(op, tn)) {
      if (status == -1)
        status = DEF_NOT_BEFORE_USE;
      else if (def && status == DEF_NOT_BEFORE_USE)
        status = DEF_MAYBE_BEFORE_USE;
    }
    if (OP_Defs_TN(op, tn)) {
      if (status == -1)
        status = DEF_BEFORE_USE;
      def = TRUE;
    }
  }
  FmtAssert(status != -1, ("unexpected def_before_use"));
  return status;
}

// Add all blocks reachable via predecessor from <bb> in the set <reachable>.
static void
Set_Pred_Reachable(BB *bb, BB_SET *reachable)
{
  BBLIST *blst;
  FOR_ALL_BB_PREDS(bb, blst) {
    BB *pred = BBLIST_item(blst);
    if (!BB_SET_MemberP(reachable, pred)) {
      BB_SET_Union1D(reachable, pred, NULL);
      Set_Pred_Reachable(pred, reachable);
    }
  }
}

// return def bb of given use bb of tn; return NULL if multiple defs
static BB*
Def_BB_For_Use (TN *tn, BB *use_bb)
{
  BB_SET *bbdef_set = (BB_SET*) TN_MAP_Get (tn_def_bbs, tn);
  BB *def_bb = NULL;
  BB *bb;
  BB_SET *reachable = BB_SET_Create_Empty(PU_BB_Count + 2, &def_info_pool);

  if (bbdef_set == NULL)
    return NULL; // no defs?

  Set_Pred_Reachable (use_bb, reachable);

  FOR_ALL_BB_SET_members (bbdef_set, bb) {
    if (bb == use_bb) {
      // def and use in same bb, check if def is before use
      INT status = Def_Before_Use (tn, bb);
      if (status == DEF_BEFORE_USE) {
        return bb;
      } 
      else if (status == DEF_MAYBE_BEFORE_USE) {
        // multiple uses in bb with mixed def, so give up.
        return NULL;
      }
      else if (BB_SET_MemberP(reachable, bb)) {
        // loop to self, so must be multiple defs before and after use
        return NULL;
      }
    } 
    else if (BB_SET_MemberP(reachable, bb)) {
      // potential def for use
      if (def_bb != NULL) {
        if (BB_SET_MemberP(BB_dom_set(def_bb), bb) 
      //    && BB_SET_MemberP(BB_pdom_set(bb), def_bb) 
          && BB_SET_MemberP(BB_dom_set(use_bb), def_bb))
        {
          // bb must reach use through def_bb, so def_bb is def for use
          if (tracing) fprintf(TFile,
              "bb %d is later def than bb %d for use bb %d\n", 
              BB_id(def_bb), BB_id(bb), BB_id(use_bb));
        }
        else if (BB_SET_MemberP(BB_dom_set(bb), def_bb)
       //   && BB_SET_MemberP(BB_pdom_set(def_bb), bb) 
          && BB_SET_MemberP(BB_dom_set(use_bb), bb))
        {
          // def_bb must reach use through bb, so bb is def for use
          if (tracing) fprintf(TFile,
              "bb %d is later def than bb %d for use bb %d\n", 
              BB_id(bb), BB_id(def_bb), BB_id(use_bb));
          def_bb = bb;
        }
        else { 
          return NULL;	// multiple defs
        }
      }
      else { 
        def_bb = bb;
      }
    }
  }
  return def_bb;
}

// check that each use has a single independent def
static BOOL
Each_Use_Has_Single_Def (TN *tn)
{
  BB *def_bb;
  BB *use_bb;
  BB_SET *bbuse_set = (BB_SET*) TN_MAP_Get (tn_use_bbs, tn);
  if (bbuse_set == NULL)
    return TRUE; // no uses?

  FOR_ALL_BB_SET_members (bbuse_set, use_bb) {
    if ( ! Def_BB_For_Use (tn, use_bb))
      return FALSE;
  }
  return TRUE;
}

// try to create unique def for tns,
// catching case where multiple independent defs.
void
Create_Unique_Defs_For_TNs (void)
{
  // If multiple bbs, if bbs do not dominate each other,
  // then independent and can replace with one_def tn
  // (need to replace uses too, matching them to dominating version)
  BB *bb;
  OP *op;
  TN *tn;
  BB_SET *bbdef_set;
  BB_SET *bbuse_set;
  INT i, j;
  INT opt_count = 0; // use this to narrow down problems

  tracing = Get_Trace(TP_EBO, 0x400);
  MEM_POOL_Initialize (&def_info_pool, "def_info_pool", FALSE);
  MEM_POOL_Push(&def_info_pool);

  tn_def_bbs = TN_MAP_Create();
  tn_use_bbs = TN_MAP_Create();

  // Create list of bbs that tn is defined in;
  // also create list of bbs that tn is used in
  // to speed later check of uses having a dominator bb.
  for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    FOR_ALL_BB_OPs (bb, op) {
      // uses
      for (i = 0; i < OP_opnds(op); i++) {
	tn = OP_opnd(op,i);
	if (!TN_is_register(tn)) 
		continue;
	bbuse_set = (BB_SET*) TN_MAP_Get (tn_use_bbs, tn);
	if (bbuse_set == NULL) {
		bbuse_set = BB_SET_Create_Empty (PU_BB_Count+2, &def_info_pool);
		bbuse_set = BB_SET_Union1D (bbuse_set, bb, &def_info_pool);
	} 
	else {
		bbuse_set = BB_SET_Union1D (bbuse_set, bb, &def_info_pool);
	}
	TN_MAP_Set (tn_use_bbs, tn, bbuse_set);
      }
      // defs
      for (i = 0; i < OP_results(op); i++) {
	tn = OP_result(op,i);
	if (!TN_is_register(tn)) 
		continue;
	bbdef_set = (BB_SET*) TN_MAP_Get (tn_def_bbs, tn);
	if (bbdef_set == NULL) {
		bbdef_set = BB_SET_Create_Empty (PU_BB_Count+2, &def_info_pool);
		bbdef_set = BB_SET_Union1D (bbdef_set, bb, &def_info_pool);
	}
	else if (BB_SET_MemberP(bbdef_set, bb)) {
		// multiply defined in same bb
		// add entry bb so will dominate and never rename.
		bbdef_set = BB_SET_Union1D (bbdef_set, bb, &def_info_pool);
		bbdef_set = BB_SET_Union1D (bbdef_set, REGION_First_BB, &def_info_pool);
	} 
	else {
		bbdef_set = BB_SET_Union1D (bbdef_set, bb, &def_info_pool);
	}
	TN_MAP_Set (tn_def_bbs, tn, bbdef_set);
      }
    }
  }

  TN_NUM tnum;
  BB *dbbs[4]; // array of defining bbs
  TN *dtns[4]; // array of defining tns
  // now see if any of these multiple bbs do not dominate each other.
  for (tnum = First_REGION_TN; tnum <= Last_TN; ++tnum) {
    tn = TNvec(tnum);
    bbdef_set = (BB_SET*) TN_MAP_Get (tn_def_bbs, tn);
    if (bbdef_set == NULL)
	continue;	// no def
    if (BB_SET_Size(bbdef_set) == 1) { // only 1 def
	Set_TN_has_one_def(tn);
	continue;	
    }

    if (Defs_Can_Be_Merged (tn)) {
	// merged multiple defs, so recompute def set
        bbdef_set = (BB_SET*) TN_MAP_Get (tn_def_bbs, tn);
        if (BB_SET_Size(bbdef_set) == 1)
	  continue;	// only 1 def
    }

    if (BB_SET_MemberP(bbdef_set, REGION_First_BB))
	continue;	// dominates everything
    if (BB_SET_Size(bbdef_set) > 4)
	continue;	// for now, ignore if lots of defs

    if (Each_Use_Has_Single_Def(tn)) {
    	++opt_count;
         //if (opt_count < 3 || opt_count > 3) continue;
        if (tracing) 
            fprintf(TFile, "%d: TN%d defs are independent\n", 
                          opt_count, TN_number(tn));
    }
    else
        continue;	// conflicting defs
      
    if (tracing) { Print_TN (tn, TRUE); fprintf(TFile, "\n"); }

    // create new tn for each def
    for (i = 0; i < 4; ++i) {
	dbbs[i] = NULL;
	dtns[i] = NULL;
    }
    i = 0;
    FOR_ALL_BB_SET_members (bbdef_set, bb) {
	dbbs[i] = bb;
	dtns[i] = Dup_TN(tn);
	Set_TN_has_one_def(dtns[i]);
	if (tracing) fprintf(TFile, "def in bb %d is tn%d\n", BB_id(dbbs[i]), TN_number(dtns[i]));
	++i;
    }
    bbuse_set = (BB_SET*) TN_MAP_Get (tn_use_bbs, tn);

    // now iterate thru ops again and replace each def and use of tn
    // with new tns, based on dominating def
    for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
      TN *def_tn = NULL;
      TN *use_tn = NULL;
      if (BB_SET_MemberP(bbdef_set, bb)) {
        for (i = 0; i < 4; ++i) {
          if (dbbs[i] == bb) {
	    def_tn = dtns[i];
            continue;
          }
	}
      }
      if (bbuse_set != NULL && BB_SET_MemberP(bbuse_set, bb)) {
        BB *defbb = Def_BB_For_Use (tn, bb);
        FmtAssert(defbb, ("defbb null?"));
        for (i = 0; i < 4; ++i) {
          if (dbbs[i] == defbb)
	    use_tn = dtns[i];
	}
      }
      if (def_tn || use_tn) {
       FOR_ALL_BB_OPs (bb, op) {
        for (i = 0; i < OP_results(op); i++) {
	  if (tn == OP_result(op,i)) {
            FmtAssert(def_tn, ("no def_tn?"));
	    Set_OP_result(op,i, def_tn);
	    if (TN_from_shared_load(tn)) {
	      Set_TN_from_shared_load(def_tn);
	    }
	  }
	}
        for (i = 0; i < OP_opnds(op); i++) {
	  if (tn == OP_opnd(op,i)) {
            FmtAssert(use_tn, ("no use_tn?"));
	    Set_OP_opnd(op,i, use_tn);
	  }
	}
       }
      }
    }
  }

  TN_MAP_Delete (tn_use_bbs);
  TN_MAP_Delete (tn_def_bbs);
  MEM_POOL_Pop(&def_info_pool);
  MEM_POOL_Delete (&def_info_pool);
}

