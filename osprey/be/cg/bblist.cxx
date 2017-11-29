/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
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



#include "defs.h"
#include "erglob.h"
#include "mempool.h"
#include "bb.h"
#include "cgtarget.h"
#include "freq.h"
#include "cg.h"

/* Since sizeof(BBLIST) is 12, 670*12 = 8040, which is just under 8K. */ 
#define BBLISTS_PER_BLOCK 670

#define BBLIST_BLOCK_SIZE (BBLISTS_PER_BLOCK * sizeof(BBLIST))

static BBLIST *BBlist_Free_List = NULL;

static INT num_bblist_buffers = 0;


/* Get a BBLIST item from the free list. If the free list is empty, 
 * allocate a new block and return the first item in the block.
 */
static BBLIST *
bblist_alloc (void)
{
  BBLIST *tmp_list;
  BBLIST *cur_item;
  INT i;

  tmp_list = BBlist_Free_List;
  if (tmp_list == NULL) {
    /* TODO: we could use Pu_Alloc instead of malloc here. If we do, we
       need to make sure to set BBlist_Free_List to NULL whenever we
       do a Pu_Free.
    */
    tmp_list = (BBLIST *) malloc (BBLIST_BLOCK_SIZE);
    num_bblist_buffers++;
    if (tmp_list == NULL) {
      ErrMsg (EC_No_Mem, "bblist_alloc");
    }
    cur_item = tmp_list;
    for (i = 0; i < BBLISTS_PER_BLOCK - 1; i++) {
      BBLIST_next(cur_item) = cur_item + 1;
      cur_item++;
    }
    BBLIST_next(cur_item) = NULL;
  }
  BBlist_Free_List = BBLIST_next(tmp_list);
  BBLIST_prob(tmp_list)  = 0.0;
  BBLIST_next(tmp_list)  = NULL;
  BBLIST_flags(tmp_list) = 0;
  return tmp_list;
}


#if USE_DEBUG_VERSION
static void
bblist_free (BBLIST *lst)
{
  BBLIST *p;

  FOR_ALL_BBLIST_ITEMS (BBlist_Free_List, p) {
    if (p == lst) printf ("*** ERROR in bblist_free\n");
  }
  BBLIST_next(lst) = BBlist_Free_List;
  BBlist_Free_List = lst;
}
#else
#define bblist_free(lst)  \
    ((BBLIST_next(lst) = BBlist_Free_List), (BBlist_Free_List = lst))
#endif


/* Free 'lst', and put back all its elements to the free list. */
void
BBlist_Free (BBLIST **lst )
{
  BBLIST *tmp1, *tmp2;

  for (tmp1 = *lst; tmp1 != NULL; tmp1 = tmp2) {
    tmp2 = BBLIST_next(tmp1);
    bblist_free (tmp1);
  }
  *lst = NULL;
}


/* returns a count of the number of elements in the list. */
INT
BBlist_Len (BBLIST *lst)
{
  INT count = 0;
  BBLIST *p;

  FOR_ALL_BBLIST_ITEMS (lst, p) 
    count++;
  return count;
}


/* Add the bb to the end of the lst. If bb was already in the lst, don't 
   add it twice.
*/
BBLIST *
BBlist_Add_BB (BBLIST **lst, BB *bb)
{
  BBLIST *p, *last;

  p = *lst;
  /* check if the lst is empty, If yes put the bb into the list and return. */
  if (p == NULL) {
    p = bblist_alloc ();
    BBLIST_item(p) = bb;
    *lst = p;
    return p;
  }

  /* check if the bb is already in the lst. */
  last = NULL;
  for (;p != NULL; p = BBLIST_next(p)) {
    if (BBLIST_item(p) == bb) return p;
    last = p;
  }

  /* Add the bb to the end of the lst. */
  p = bblist_alloc ();
  BBLIST_item(p) = bb;
  BBLIST_next(last) = p;
  return p;
}

/* Add the bb to the end of the lst with edge probability <prob>.
 * If bb was already in the lst, just increment edge probability.
 * Edge probabilities not updated unless FREQ_Frequencies_Computed().
 */

BBLIST *
BBlist_Add_BB_with_Prob (BBLIST **lst, BB *bb, float prob,
			 BOOL via_feedback, BOOL set_prob
#ifdef KEY
                         ,BOOL via_hint
#endif
			 ,BOOL incr_prob)
{
  BBLIST *p, *last;

  p = *lst;
  /* check if the lst is empty, If yes put the bb into the list and return. */
  if (p == NULL) {
    p = bblist_alloc ();
    BBLIST_item(p) = bb;
    if (via_feedback || CG_PU_Has_Feedback) {
      BBLIST_prob(p) = prob;
      Set_BBLIST_prob_fb_based(p);
    } if (set_prob || FREQ_Frequencies_Computed()) {
      BBLIST_prob(p) = prob;
#if defined(KEY)
      if(via_hint)
        Set_BBLIST_prob_hint_based(p);
#endif
      Reset_BBLIST_prob_fb_based(p);
    }
    *lst = p;
    return p;
  }

  /* check if the bb is already in the lst. */
  last = NULL;
  for (;p != NULL; p = BBLIST_next(p)) {
    if (BBLIST_item(p) == bb) {
      if (FREQ_Frequencies_Computed() && incr_prob) {
        BBLIST_prob(p) += prob;
        if (BBLIST_prob(p) >= 1.0f)
          BBLIST_prob(p) = 1.0f;
      }
      return p;
    }
    last = p;
  }

  /* Add the bb to the end of the lst. */
  p = bblist_alloc ();
  BBLIST_item(p) = bb;
  BBLIST_next(last) = p;
  if (via_feedback || CG_PU_Has_Feedback) {
    BBLIST_prob(p) = prob;
    Set_BBLIST_prob_fb_based(p);
  } if (
#if defined(KEY) 
      set_prob ||
#endif
      FREQ_Frequencies_Computed()) {
    BBLIST_prob(p) = prob;
#if defined(KEY)
      if(via_hint)
        Set_BBLIST_prob_hint_based(p);
#endif
    Reset_BBLIST_prob_fb_based(p);
  }
  return p;
}

static const union { INT32 i; float f; } NaN_u = { 0x7fbfffff };
static const float NaN = NaN_u.f;

void
Link_Pred_Succ (BB *pred, BB *succ)
{
  Verify_BB(pred);
  Verify_BB(succ);

  BBLIST *pedge;
  BBlist_Add_BB (&BB_succs(pred), succ);
  pedge = BBlist_Add_BB (&BB_preds(succ), pred);

  /* Poison probability of pred edge since it is unused.
   */
  BBLIST_prob(pedge) = NaN;
}

void
Link_Pred_Succ_with_Prob (BB *pred, BB *succ, float prob,
                          BOOL via_feedback, BOOL set_prob
#ifdef KEY
                          , BOOL via_hint
#endif
                          , BOOL incr_prob
                         )
{
  Verify_BB(pred);
  Verify_BB(succ);

  BBLIST *pedge;
  BBlist_Add_BB_with_Prob (&BB_succs(pred), succ, prob,
                           via_feedback, set_prob
#ifdef KEY
                           , via_hint
#endif
                           , incr_prob
                          );
  pedge = BBlist_Add_BB (&BB_preds(succ), pred);

  /* Poison probability of pred edge since it is unused.
   */
  BBLIST_prob(pedge) = NaN;
}


/* Delete bb from lst. */
void
BBlist_Delete_BB (BBLIST **lst, BB *bb)
{
  BBLIST *p, *last;

  last = NULL;
  for (p = *lst; p != NULL; p = BBLIST_next(p)) { 
    if (BBLIST_item(p) == bb) {
      if (last == NULL) {
	*lst = BBLIST_next(p);
      }
      else {
	BBLIST_next(last) = BBLIST_next(p);
      }
      bblist_free (p);
      break;
    }
    last = p;
  }
}

void
Unlink_Pred_Succ (BB *pred, BB *succ)
{
  BBlist_Delete_BB (&BB_succs(pred), succ);
  BBlist_Delete_BB (&BB_preds(succ), pred);
}


BBLIST *
BBlist_Find_BB (BBLIST *lst, BB *bb)
/* -----------------------------------------------------------------------
 * Returns the BBLIST node in <lst> whose BBLIST_item is <bb>, or NULL
 * if there is none.
 * -----------------------------------------------------------------------
 */
{
  BBLIST *p;
  FOR_ALL_BBLIST_ITEMS(lst, p)
    if (BBLIST_item(p) == bb) break;
  return p;
}

BBLIST *
BBlist_Fall_Thru_Succ (BB *bb)
/* -----------------------------------------------------------------------
 * Returns a pointer to the BBLIST <node> in BB_preds(bb) such that
 * BBLIST_item(node) is the fall through control flow successor of
 * <bb>, or NULL if there is none.
 * -----------------------------------------------------------------------
 */
{
  BB *next = BB_next(bb);
  BBLIST *node = NULL;

  if (next && (node = BB_Find_Succ(bb, next))) {
    /* Make sure it's not a branch target (direct or indirect). */
    OP *br_op = BB_branch_op(bb);
    if (br_op) {
      INT tfirst, tcount;
      CGTARG_Branch_Info(br_op, &tfirst, &tcount);
      if (tcount == 0) {
	/* Indirect jump - no fall-through succ */
	node = NULL;
      } else {
	TN *dest = OP_opnd(br_op, tfirst);
	DevAssert(tcount == 1, ("%d branch targets, expected 1", tcount));
	DevAssert(TN_is_label(dest), ("expected label"));
	if (Is_Label_For_BB(TN_label(dest), next)) {
	  /* Remove useless explicit branch to <next> */
	  BB_Remove_Op(bb, br_op);
	} else {
#if defined(TARG_SL)
#ifndef  fork_joint
         if(!OP_fork(br_op))
#endif 
#endif 
	  DevAssert(OP_cond(br_op), ("BB_succs(BB:%d) wrongly contains BB:%d",
				     BB_id(bb), BB_id(next)));
	}
      }
    }
  }
  return node;
}

BBLIST *
BBlist_Fall_Thru_Pred (BB *bb)
/* -----------------------------------------------------------------------
 * Returns a pointer to the BBLIST <node> in BB_preds(bb) such that
 * BBLIST_item(node) is the fall through control flow predecessor of
 * <bb>, or NULL if there is none.
 * -----------------------------------------------------------------------
 */
{
  BB *prev = BB_prev(bb);
  BBLIST *node = NULL;

  if (prev && (node = BB_Find_Pred(bb, prev))) {
    /* Make sure <bb> is not a branch target of <prev> (direct or indirect). */
    OP *br_op = BB_branch_op(prev);
    if (br_op) {
      INT tfirst, tcount;
      CGTARG_Branch_Info(br_op, &tfirst, &tcount);
      if (tcount == 0) {
	/* Indirect jump - no fall-through pred */
	node = NULL;
      } else {
	TN *dest = OP_opnd(br_op, tfirst);
	DevAssert(tcount == 1, ("%d branch targets, expected 1", tcount));
	DevAssert(TN_is_label(dest), ("expected label"));
	if (Is_Label_For_BB(TN_label(dest), bb)) {
	  /* Remove useless explicit branch to <bb> */
	  BB_Remove_Op(prev, br_op);
	} else {
	  DevAssert(OP_cond(br_op), ("BB_preds(BB:%d) wrongly contains BB:%d",
				     BB_id(bb), BB_id(prev)));
	}
      }
    }
  }
  return node;
}


