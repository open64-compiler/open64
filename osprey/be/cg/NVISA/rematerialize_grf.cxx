/*
 *  Copyright 2005-2008 NVIDIA Corporation.  All rights reserved.
 */
/*
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
#include "defs.h"
#include "tracing.h"
#include "errors.h"
#include "wn.h"
#include "bb.h"
#include "bb_set.h"
#include "op.h"
#include "tn.h"
#include "cg.h"
#include "cgtarget.h"
#include "whirl2ops.h"
#include "opt_alias_interface.h"
#include "opt_alias_mgr.h"
#include "findloops.h"

static BOOL tracing = FALSE;
#define Trace(msg)	if (tracing) fprintf(TFile, msg "\n");

static BOOL
OP_is_shared_memory (OP *op)
{
  if (OP_store(op) || OP_load(op))
    return (TN_enum(OP_opnd(op,1)) == ECV_space_shared);
  else
    return FALSE;
}

// Check if safe to rematerialize the load,
// by checking each op between first and last_op.
// Assumes that first_op and last_op are in same bb.
static BOOL
Safe_To_Rematerialize_Load_In_BB (OP *load_op, OP *first_op, OP *last_op)
{
  OP *op = first_op;
  Is_True(op == NULL || OP_bb(first_op) == OP_bb(last_op), ("ops not in same bb?"));
  while (op != NULL) {
	if (OP_store(op) && OP_is_shared_memory(op)
			 && OP_is_shared_memory(load_op)) 
	{
		// check if store aliases with load
  		WN *load_wn;
		WN *store_wn;
  		ALIAS_RESULT result;
		Trace("check alias of load with store");
      		if (Alias_Manager == NULL) return FALSE;
		load_wn = Get_WN_From_Memory_OP(load_op);
		store_wn = Get_WN_From_Memory_OP(op);
		if (load_wn == NULL || store_wn == NULL)
			return FALSE;	// no info so conservative
          	result = Aliased(Alias_Manager, load_wn, store_wn);
		if (result != NOT_ALIASED)
			return FALSE;
	}
        else if (CGTARG_Is_OP_Barrier(op)) { // e.g. syncthreads
		Trace("barrier in bb stops remat");
		return FALSE;
	}
	if (op == last_op) break;
	op = OP_next(op);
  }
  return TRUE;
}

// Check if safe to rematerialize the load,
// by checking each bb between load_bb and use_bb
static BOOL
Safe_To_Rematerialize_Load (BB *load_bb, OP *load_op, BB *use_bb, OP *use_op)
{
  Is_True(load_bb != use_bb, ("bbs are same?"));
  if ( ! Safe_To_Rematerialize_Load_In_BB (load_op, 
                                           BB_first_op(use_bb), use_op))
	return FALSE;

  BOOL found = FALSE;
  BB *bb = use_bb;
  if (BB_Has_One_Pred(use_bb)) {
      bb = BB_Unique_Predecessor(use_bb);
  }
  // else if loop will search whole loop

  while (bb != NULL && !found) {
    if (bb == load_bb) {
      found = TRUE;
      // search from load to end
      return Safe_To_Rematerialize_Load_In_BB (load_op, 
                                         OP_next(load_op), BB_last_op(load_bb));
    }
    else {
      // search whole bb
      if ( ! Safe_To_Rematerialize_Load_In_BB (load_op, 
                                         BB_first_op(bb), BB_last_op(bb)))
        return FALSE;
    }

    if (BB_Has_One_Pred(bb)) {
      bb = BB_Unique_Predecessor(bb);
    }
    else {
      // multiple preds, check for loop
      BB_SET *loopbbs = NULL;
      LOOP_DESCR *loopd = LOOP_DESCR_Find_Loop(bb);
      if (loopd) {
        loopbbs = LOOP_DESCR_bbset(loopd);
        if (loopbbs) {
          // if loop, check all bbs in loop
          // then check bb that is not in loop (pre-loop-header)
          BB *bbl;
          FOR_ALL_BB_SET_members(loopbbs, bbl) {
            if (bbl == load_bb) {
              found = TRUE;
            }
            // search whole bb
            if ( ! Safe_To_Rematerialize_Load_In_BB (load_op, 
                                         BB_first_op(bbl), BB_last_op(bbl)))
	      return FALSE;
          }
          if (found) break;
          // get loop header, then bb above loop
          bbl = LOOP_DESCR_loophead(loopd);
          BBLIST *edge;
          bb = NULL;
          FOR_ALL_BB_PREDS(bbl, edge) {
            if ( ! BB_SET_MemberP(loopbbs, BBLIST_item(edge)))
              bb = BBLIST_item(edge);
          }
        }
      }
      else {
        // not a loop
	BB *pred1 = NULL;
	BB *pred2 = NULL;
        BBLIST *edge;
        FOR_ALL_BB_PREDS(bb, edge) {
          if (pred1 == NULL) pred1 = BBLIST_item(edge);
          else if (pred2 == NULL) pred2 = BBLIST_item(edge);
	  else return FALSE; // will we ever see more than 2 preds?
        }
        FmtAssert(pred2, ("pred2 not set?"));
	// check for simple if-then:  pred(pred2) == pred1
        if (BB_Unique_Predecessor(pred2) == pred1) {
          // search whole intermediate bb
          if ( ! Safe_To_Rematerialize_Load_In_BB (load_op, 
                                         BB_first_op(pred2), BB_last_op(pred2)))
	    return FALSE;
          bb = pred1;
        }
        // if-then-else:  pred(pred1) == pred(pred2)
        else if (BB_Has_One_Pred(pred1)
          && (BB_Unique_Predecessor(pred1) == BB_Unique_Predecessor(pred2))) 
        {
          // search whole intermediate bbs
          if ( ! Safe_To_Rematerialize_Load_In_BB (load_op, 
                                         BB_first_op(pred1), BB_last_op(pred1)))
	    return FALSE;
          if ( ! Safe_To_Rematerialize_Load_In_BB (load_op, 
                                         BB_first_op(pred2), BB_last_op(pred2)))
	    return FALSE;
          bb = BB_Unique_Predecessor(pred1);
        }
        else {
          // not simple so give up
          bb = NULL;
        }
      }
    }
  }
  return found;
}

void
Rematerialize_GRF (void)
{
  BB *bb;
  OP *op;
  TN *tn;
  INT i;

  tracing = Get_Trace(TP_EBO, 0x100);

  for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
  if (tracing) fprintf(TFile, "rematerialize_grf: bb %d\n", BB_id(bb));
    FOR_ALL_BB_OPs (bb, op) {
      for (i = 0; i < OP_opnds(op); i++) {
	tn = OP_opnd(op,i);
        if (TN_from_shared_load(tn)) {
	  OP *load_op = NULL;
	  INT j;
	  // search for unique reaching def
	  load_op = Find_Reaching_Def (tn, op);
	  if (load_op == NULL) 
		continue;	// no unique def	
	  if (OP_bb(load_op) == bb) 
		continue; 	// leave alone if already in same bb
	  // make sure the reaching def was the shared load
	  // (might be multiple defs)
	  if (!OP_load(load_op))
		continue;
	  if (!OP_is_shared_memory(load_op))
		continue;

	  BOOL one_def = TRUE;
	  TN *base_tn;
      	  for (j = 0; j < OP_opnds(load_op); j++) {
		base_tn = OP_opnd(load_op,j);
		if (TN_is_register(base_tn)) {
		  // check that base is not redefined before use
		  if (TN_has_one_def(base_tn))
			continue;
		  else {
			OP *base1_op = Find_Reaching_Def (base_tn, load_op);
			OP *base2_op = Find_Reaching_Def (base_tn, op);
			if (base1_op != NULL && base1_op == base2_op)
				continue;	// def doesn't change
			else {
				Trace("base defs don't match");
				one_def = FALSE;
			}
		  }
		}
	  }
	  if (one_def) {
	    if (Safe_To_Rematerialize_Load (OP_bb(load_op), load_op, bb, op)) {
		Trace("safe to insert shared load at use");
		OP *new_op = Dup_OP(load_op);
		BB_Insert_Op_Before (bb, op, new_op);
	    }
	  }
	}
      }
    }
  }
}
