/*
 * Copyright (C) 2008 Advanced Micro Devices, Inc.  All Rights Reserved.
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
 * Module: dominate.c
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/dominate.cxx,v $
 *
 * Description:
 * 
 * Support for generation and maintenance of basic block (BB) 
 * dominator/post-dominator information.
 *
 * ====================================================================
 * ====================================================================
 */

#include "defs.h"
#include "config.h"
#include "erglob.h"
#include "erbe.h"
#include "tracing.h"
#include "mempool.h"
#include "bb.h"
#include "bitset.h"
#include "cflow.h"	/* for tracing flags */
#include "timing.h"
#include "cg_region.h"

#include "dominate.h"

DOMINATORS bb_dom_map;

static MEM_POOL dom_pool;
static BOOL pool_inited;

/* We need to represent two imaginary BBs: the predecessor of all entry
 * point BBs, and the the successor of all exit BBs. Define the IDs
 * which are guaranteed not to conflict with any real BBs.
 */
#define BB_ID_PRE_ENTRY		0
#define BB_ID_POST_EXIT		(1+PU_BB_Count)

#ifdef IMMEDIATE
/* Ragnarok also computed the immediate dominator and post dominator
 * for each BB. We haven't had a use for it so far, so these accessors
 * haven't been implemented yet.
 *
 * NOTE: calculating the immediate [post-]dominator info seems to have
 * a pretty big compile-time implication.
 */
#error immediate dominator accessors need to be implemented
#define Set_BB_dominator(bb,dom) 
#define Set_BB_post_dom(bb,pdom) 
#define     BB_dominator(bb)	 (0)
#define     BB_post_dom(bb)	 (0)
#endif /* IMMEDIATE */

/* ====================================================================
 *
 * Set_BB_dom_set
 *
 * Sets the dominator set of block <bb> to BS* <bs>. Extended to 
 * dynamically regrow the dom_sets to twice the original size, if
 * required.
 *
 * ====================================================================
 */
void
Set_BB_dom_set(BB *bb, BS *bs) {

   if (BB_id(bb) < bb_dom_map.size)
     bb_dom_map.dom_set[BB_id(bb)] = bs;
   else {
     BS_ELT old_length = bb_dom_map.size;
     INT32 new_size = old_length * 2;

     // Resize the dom_set to twice the original size.     
     bb_dom_map.dom_set = TYPE_MEM_POOL_REALLOC_N(BS*, &dom_pool,
						  bb_dom_map.dom_set,
						  old_length,
						  new_size);

     // Need to resize the pdom_set as well since they are symmetric.
     bb_dom_map.pdom_set = TYPE_MEM_POOL_REALLOC_N(BS*, &dom_pool,
						   bb_dom_map.pdom_set,
						   old_length,
						   new_size);
     bb_dom_map.size = new_size;
     bb_dom_map.dom_set[BB_id(bb)] = bs;
   }
}

/* ====================================================================
 *
 * Set_BB_pdom_set
 *
 * Sets the post-dominator set of block <bb> to BS* <bs>. Extended to 
 * dynamically regrow the pdom_sets to twice the original size, if
 * required.
 *
 * ====================================================================
 */
void
Set_BB_pdom_set(BB *bb, BS *bs) {

   if (BB_id(bb) < bb_dom_map.size)
     bb_dom_map.pdom_set[BB_id(bb)] = bs;
   else {
     INT old_length = bb_dom_map.size;
     INT new_size = old_length * 2;

     // Resize the pdom_set to twice the original size.
     bb_dom_map.pdom_set = TYPE_MEM_POOL_REALLOC_N(BS*, &dom_pool,
						   bb_dom_map.pdom_set,
						   old_length,
						   new_size);

     // Need to resize the dom_set as well since they are symmetric.
     bb_dom_map.dom_set = TYPE_MEM_POOL_REALLOC_N(BS*, &dom_pool,
						  bb_dom_map.dom_set,
						  old_length,
						  new_size);

     bb_dom_map.size = new_size;
     bb_dom_map.pdom_set[BB_id(bb)] = bs;
   }
}

/* ====================================================================
 *
 * Print_BB_Dominators
 *
 * Print out the domaintor and post-dominator sets for a BB.
 *
 * ====================================================================
 */
void
Print_BB_Dominators(BB* bb)
{
  fprintf(TFile, "%sBB:%d\n%s", SBar, BB_id(bb), SBar);
#ifdef IMMEDIATE
  fprintf(TFile, "Immediate dominator (BB_dominator): BB:%d\n",
	  BB_dominator(bb));
  fprintf(TFile, "Immediate post-dominator (BB_post_dom): BB:%d\n",
	  BB_post_dom(bb));
#endif /* IMMEDIATE */
  fprintf(TFile, "Dominators (BB_dom_set):  ");
  BS_Print(BB_dom_set(bb), TFile);
  fprintf(TFile, "\nPost-dominators (BB_pdom_set): ");
  BS_Print(BB_pdom_set(bb), TFile);
  fprintf(TFile, "\n\n");
}


/* ====================================================================
 *
 * Print_Dominators
 *
 * Print out the domaintor and post-dominator sets for each BB.
 *
 * ====================================================================
 */
void
Print_Dominators(void)
{
  BB *bb;

  fprintf(TFile, "%sBegin dominator/post-dominator dump\n%s\n", DBar, DBar);
  for ( bb = REGION_First_BB; bb; bb = BB_next(bb) ) {
    Print_BB_Dominators(bb);
  }
  fprintf(TFile,"%sEnd dominator/post-dominator dump\n%s\n", DBar, DBar);
}

/* ====================================================================
 *
 * Vector_Print_Dominators
 *
 * Print out the domaintor and post-dominator sets for each BB in
 * a vector.
 *
 * ====================================================================
 */
void 
Vector_Print_Dominators(const std::vector<BB*>& region_bbs)
{
  INT i;
  fprintf(TFile, "Dominator dump for BB_region\n");
  for (i=0; i < region_bbs.size(); i++) {
    Print_BB_Dominators(region_bbs[i]);
  }
}


/* ====================================================================
 *
 * Init_BB_Dom_Info
 *
 * Initialize dominator/post-dominator bit vectors.  Any existing ones
 * are freed, and new ones are allocated with length 2+PU_BB_Count.
 * They are initialized to all TRUE, since the dominator algorithm
 * starts with TRUE and uses an intersection iteration to converge to
 * the true dominator (post-dominator) set.
 *
 * NOTE:  BBs are numbered from 1, so their indices in the bit vectors
 * start from 1.  We use BB_ID_PRE_ENTRY (0) to represent a non-existent 
 * BB which is the immediate predecessor of all entries, and 
 * BB_ID_POST_ENTRY (1+PU_BB_Count) to represent a non-existent BB which 
 * is the immediate successor of all exits. This results in all BBs having 
 * an immediate dominator and, except for BBs which can't reach an exit, 
 * an immediate post-dominator.
 *
 * ====================================================================
 */
static void
Init_BB_Dom_Info(BB *bb, BS *dom_init)
{
  RID *rid = BB_rid(REGION_First_BB);
  CGRIN *cgrin = rid ? RID_Find_Cginfo(REGION_First_BB) : NULL;

#ifdef IMMEDIATE
  Set_BB_dominator(bb, NULL);
#endif /* IMMEDIATE */

  /* Initialize the dominators of entry BBs to themselves and the
   * dummy BB_ID_PRE_ENTRY, which is a predecessor of all entries.  
   * Initialize the dominator sets of other BBs to everything.
   */
  if (BB_entry(bb) || cgrin && bb == CGRIN_entry(cgrin)) {
    Set_BB_dom_set(bb, BS_Create_Empty(2+PU_BB_Count, &dom_pool));
    BS_Union1D(BB_dom_set(bb), BB_ID_PRE_ENTRY, NULL);
    BS_Union1D(BB_dom_set(bb), BB_id(bb), NULL);
  } else {
    Set_BB_dom_set(bb, BS_Copy(dom_init, &dom_pool));
  }

#ifdef IMMEDIATE
  Set_BB_post_dom(bb, NULL);
#endif /* IMMEDIATE */
  Set_BB_pdom_set(bb, BS_Create_Empty(2+PU_BB_Count, &dom_pool));

  /* Initialize the post-dominators of all BBs to themselves.
   * In addition, initialize exit BBs to also contain the dummy
   * post-exit BB_ID_POST_EXIT.
   */
  BS_Union1D(BB_pdom_set(bb), BB_id(bb), NULL);
  if (BB_exit(bb) || rid && BB_REGION_Exit(bb, rid) != NO_REGION_EXIT) {
    BS_Union1D(BB_pdom_set(bb), BB_ID_POST_EXIT, NULL);
  }
}

BOOL
Are_Dominators_Calculated(void)
{
  return pool_inited;
}

/* ====================================================================
 *
 * Calculate_Dominators
 *
 * Calculate the dominator/post-dominator information.
 *
 * See algorithm on page 670 of Aho/Sethi/Ullman 1986 (The Dragon
 * Reincarnate).
 *
 * NOTE:  The performance of this routine will degrade if the BB list
 * is not topologically sorted.
 *
 * ====================================================================
 */
void
Calculate_Dominators(void)
{
  BB *bb;
  BOOL changed;
  BS *isect;
  BS *dom_init;
#define check isect	/* Share this bitvector */
#define match dom_init	/* Share this bitvector */
  BB *last_bb = NULL;

  Start_Timer(T_CalcDom_CU);
  CFLOW_Trace_Dom = Get_Trace(TP_FLOWOPT, 0x200);

  /* Prepare to allow allocations from our private mem pool.
   */
  if (!pool_inited) {
    MEM_POOL_Initialize(&dom_pool, 
			"private_memory_for_Calculate_Dominators",
			FALSE);
    pool_inited = TRUE;
  }

  MEM_POOL_Push_Freeze(&dom_pool);
  MEM_POOL_Push(&MEM_local_nz_pool);

  /* Allocate a bit vector for temporary usage (mostly the
   * results of intersections). Later we reuse it as 'check'.
   */
  isect = BS_Create(2+PU_BB_Count, &MEM_local_nz_pool);

  /* Allocate and initialize a bit vector which contains the initial
   * vector for BB dominators. The initial vector contains all
   * the reachable BBs, including the dummy pre-entry, but excludes
   * the dummy post-exit. Later we recycle this bit vector for 'match'.
   */
  dom_init = BS_Create_Empty(2+PU_BB_Count, &MEM_local_nz_pool);
  for ( bb = REGION_First_BB; bb; bb = BB_next(bb) ) {
    BS_Union1D(dom_init, BB_id(bb), NULL);
  }
  BS_Union1D(dom_init, BB_ID_PRE_ENTRY, NULL);

  /* Allocate and initialize bit vectors for BBs:
   */
  bb_dom_map.size = PU_BB_Count + 1;
  bb_dom_map.dom_set = TYPE_MEM_POOL_ALLOC_N(BS *, &dom_pool, bb_dom_map.size);
  bb_dom_map.pdom_set = TYPE_MEM_POOL_ALLOC_N(BS *, &dom_pool, bb_dom_map.size);
  for ( bb = REGION_First_BB; bb; bb = BB_next(bb) ) {
    Init_BB_Dom_Info(bb, dom_init);
    last_bb = bb;
  }

  /* Dominators -- the dominators of entry BBs are themselves and the
   * dummy BB_ID_PRE_ENTRY which is a predecessor of all entries.  The 
   * dominator sets of other BBs are initialized to everything (above, in
   * Init_BB_Dom_Info) and then an iterative algorithm trims them down
   * to the correct set:
   */
  do {
    changed = FALSE;
    bb = REGION_First_BB;
    do {
      BBLIST *list;
      BS *bb_dom_set;
      BS *src;

      list = BB_preds(bb);
      if ( list == NULL ) continue;

      bb_dom_set = BB_dom_set(bb);
      src = bb_dom_set;
      do {
	BB *bbp = BBLIST_item(list);
	BS_IntersectionR(isect, src, BB_dom_set(bbp));
	src = isect;
      } while (list = BBLIST_next(list));
      BS_Union1D(isect, BB_id(bb), NULL);
      if ( ! BS_EqualP(isect, bb_dom_set) ) {
	BS_CopyD(bb_dom_set, isect, NULL);
	changed = TRUE;
      }
    } while (bb = BB_next(bb));
  } while (changed);

#ifdef IMMEDIATE
  /* The immediate dominator of BBi is the BB with the same set of
   * dominators except for BBi itself:
   */
  for ( bb = REGION_First_BB; bb; bb = BB_next(bb) ) {
    INT i;

    /* match is the BS (set of dominators) to be matched: */
    BS_CopyD(match, BB_dom_set(bb), NULL);	/* bb's dom set ... */
    BS_Difference1D(match, BB_id(bb));		/* ... except bb. */

    /* match is also the set of dominators to check: */
    BS_CopyD(check, match, NULL);		/* Check all bb's dominators ... */
    BS_Difference1D(check, BB_ID_PRE_ENTRY);	/* ... except dummy pre-entry */

    /* Now go check them.  We start with the last (highest numbered)
     * dominator, because that will be the immediate dominator if we
     * are dealing with a topologically-sorted BB list:
     */
    Set_BB_dominator(bb, NULL);
    for ( i = BS_Choose(check);
	  i != BS_CHOOSE_FAILURE;
	  i = BS_Choose_Next(check, i))
    {
      if ( BBvec(i) && BS_EqualP(match, BB_dom_set(BBvec(i))) ) {
	Set_BB_dominator(bb, BBvec(i));
	break;
      }
    }
    /* Note that if the above loop does not find a match, the immediate
     * dominator should be the dummy pre-entry BB, which is indicated
     * by a NULL, the initialized value of the BB_dominator field.
     */
  }
#endif /* IMMEDIATE */

  /* Post-dominators -- the post-dominators of exit BBs are themselves
   * and the dummy BB_ID_POST_EXIT which is a successor of all exits.
   * The post-dominator sets of other BBs are harder to initialize than
   * the dominator sets, since we may have BBs which can't reach an
   * exit.  Therefore, we initialize (in Init_BB_Dom_Info) exit BBs
   * as described, other BBs to themselves, and iterate so that the 
   * initial sets are all BBs reachable from the BB.  Then an iterative 
   * algorithm trims them down to the correct set.
   *
   * Iterate the non-exit sets to include all reachable BBs:
   */
  do {
    changed = FALSE;
    bb = last_bb;
    do {
      BBLIST *list;
      BS *bb_pdom_set;
      BS *src;

      list = BB_succs(bb);
      if ( list == NULL ) continue;

      bb_pdom_set = BB_pdom_set(bb);
      src = bb_pdom_set;
      do {
	BB *bbp = BBLIST_item(list);
	BS_UnionR(isect, src, BB_pdom_set(bbp), NULL);
	src = isect;
      } while (list = BBLIST_next(list));
      if ( ! BS_EqualP(isect, bb_pdom_set) ) {
	BS_CopyD(bb_pdom_set, isect, NULL);
	changed = TRUE;
      }
    } while (bb != REGION_First_BB && (bb = BB_prev(bb)));
  } while (changed);

  /* Finally, use the above initial values to calculate the post-
   * dominator sets:
   */
  do {
    changed = FALSE;
    bb = last_bb;
    do {
      BBLIST *list;
      BS *bb_pdom_set;
      BS *src;

      list = BB_succs(bb);
      if ( list == NULL ) continue;

      bb_pdom_set = BB_pdom_set(bb);
      src = bb_pdom_set;
      do {
	BB *bbp = BBLIST_item(list);
	BS_IntersectionR(isect, src, BB_pdom_set(bbp));
	src = isect;
      } while (list = BBLIST_next(list));
      BS_Union1D(isect, BB_id(bb), NULL);
      if ( ! BS_EqualP(isect, bb_pdom_set) ) {
	BS_CopyD(bb_pdom_set, isect, NULL);
	changed = TRUE;
      }
    } while (bb != REGION_First_BB && (bb = BB_prev(bb)));
  } while (changed);

#ifdef IMMEDIATE
  /* The immediate post-dominator of BBi is the BB with the same set
   * of post-dominators except for BBi itself:
   */
  for ( bb = REGION_First_BB; bb; bb = BB_next(bb) ) {
    INT i;

    /* match is the BS (set of post-dominators) to be matched: */
    BS_CopyD(match, BB_pdom_set(bb), NULL);	/* bb's pdom set ... */
    BS_Difference1D(match, BB_id(bb));		/* ... except bb. */

    /* match is also the set of post-dominators to check: */
    BS_CopyD(check, match, NULL);		/* Check all bb's post-dominators ... */
    BS_Difference1D(check, BB_ID_PRE_ENTRY);	/* ... except dummy pre-entry */

    /* Now go check them.  We start with the last (lowest numbered)
     * post-dominator, because that will be the immediate post-dominator
     * if we are dealing with a topologically-sorted BB list:
     */
    Set_BB_post_dom(bb, NULL);
    for ( i = BS_Choose(check);
	  i != BS_CHOOSE_FAILURE;
	  i = BS_Choose_Next(check, i) )
    {
      if ( i != BB_ID_POST_EXIT ) {
	/* We don't have a set to check for the dummy post-exit "BB",
	 * but we will represent that by a NULL post-dominator pointer
	 * and can just ignore it here.
	 */
	if ( BBvec(i) && BS_EqualP(match, BB_pdom_set(BBvec(i))) ) {
	  Set_BB_post_dom(bb, BBvec(i));
	  break;
	}
      }
    }
    /* Note that if the above loop does not find a match, the immediate
     * post-dominator should be the dummy post-exit BB, or none for a
     * BB which branches to itself, either of which we indicate
     * by a NULL, the initialized value of the BB_post_dom field.
     * The two cases can be distinguished by whether BB_ID_POST_EXIT is
     * in the post-dominator bit vector; if we develop a need to
     * distinguish these two cases more easily, we'll do something
     * about it.
     */
  }
#endif /* IMMEDIATE */

  if ( CFLOW_Trace_Dom ) Print_Dominators();

  MEM_POOL_Pop ( &MEM_local_nz_pool );

  //
  // Remove the non-existant bb's from the bitsets.
  //
  for (bb = REGION_First_BB; bb; bb = BB_next(bb)) {
    BS_Difference1D(BB_dom_set(bb), 0);
    BS_Difference1D(BB_pdom_set(bb), 1+PU_BB_Count);
  }

  Stop_Timer(T_CalcDom_CU);
}

/* ====================================================================
 *
 *  BB_REGION_Calculate_Dominators
 *
 *  See interface description
 *
 * ====================================================================
 */
void
BB_REGION_Calculate_Dominators(const BB_REGION& region)
{
  CFLOW_Trace_Dom = Get_Trace(TP_FLOWOPT, 0x200);
  MEM_POOL_Push(&MEM_local_pool);
  BB_SET* bs_tmp = BB_SET_Universe(PU_BB_Count+2, &MEM_local_pool);
  std::vector<BB*> region_bbs;
  //  BB_MAP preds = BB_MAP_Create();
  BOOL changed;

  BB_REGION_to_Vector(region_bbs, region);

  //
  // Initialize dom and pdom sets for all bb's in the region to
  // be everything.  PU entry and exit blocks are initialized to
  // themselves.
  //
  INT i;
  BB* bb;
  for (i = 0; i < region_bbs.size(); i++) {
    bb = region_bbs[i];
    //
    // Sigh.  We have to use Set* routines even though many of
    // the blocks may already have dominator information.  This
    // is because the stupid BB_Has_Dominator_Info routine is
    // insufficient to tell whether a bitset has actually been
    // allocated (as opposed to the entry in the dominator table
    // having been allocated, which isn't all that useful).  Should
    // fix this.
    //
    if (BB_preds(bb)) {
      Set_BB_dom_set(bb, BS_Copy(bs_tmp, &dom_pool));
    } else {
      Set_BB_dom_set(bb, BS_Create_Empty(2+PU_BB_Count, &dom_pool));
      BS_Union1D(BB_dom_set(bb), BB_id(bb), NULL);      
    }
    if (BB_succs(bb)) {
	Set_BB_pdom_set(bb, BS_Copy(bs_tmp, &dom_pool));
    } else {
      Set_BB_pdom_set(bb, BS_Create_Empty(2+PU_BB_Count, &dom_pool));
      BS_Union1D(BB_pdom_set(bb), BB_id(bb), NULL);      
    }
  }

  //
  // Find the dominators.  We'll propogate the correct information in
  // from the borders into the region.
  //
  (void) BS_ClearD(bs_tmp);
  do {
    changed = FALSE;
    i = 0;
    do {
      BBLIST *list;
      BS *bb_dom_set;
      BS *src;
      bb = region_bbs[i++];

      list = BB_preds(bb);
      if ( list == NULL ) continue;

      bb_dom_set = BB_dom_set(bb);
      src = bb_dom_set;
      do {
	BB *bbp = BBLIST_item(list);
	BS_IntersectionR(bs_tmp, src, BB_dom_set(bbp));
	src = bs_tmp;
      } while (list = BBLIST_next(list));
      BS_Union1D(bs_tmp, BB_id(bb), NULL);
      if ( ! BS_EqualP(bs_tmp, bb_dom_set) ) {
	BS_CopyD(bb_dom_set, bs_tmp, NULL);
	changed = TRUE;
      }
    } while (i < region_bbs.size());
  } while (changed);

  //
  // Same story as above for post-dominators.
  //
  do {
    changed = FALSE;
    i = region_bbs.size() - 1;
    do {
      BBLIST *list;
      BS *bb_pdom_set;
      BS *src;
      bb = region_bbs[i--];

      list = BB_succs(bb);
      if ( list == NULL ) continue;

      bb_pdom_set = BB_pdom_set(bb);
      src = bb_pdom_set;
      do {
	BB *bbp = BBLIST_item(list);
	BS_IntersectionR(bs_tmp, src, BB_pdom_set(bbp));
	src = bs_tmp;
      } while (list = BBLIST_next(list));
      BS_Union1D(bs_tmp, BB_id(bb), NULL);
      if ( ! BS_EqualP(bs_tmp, bb_pdom_set) ) {
	BS_CopyD(bb_pdom_set, bs_tmp, NULL);
	changed = TRUE;
      }
    } while (i > 0);
  } while (changed);

  if ( CFLOW_Trace_Dom ) {
    Vector_Print_Dominators(region_bbs);
  }

  MEM_POOL_Pop(&MEM_local_pool);
}



/* ====================================================================
 *
 *  BB_SET_Calculate_Dominators
 *
 *  See interface description
 *
 * ====================================================================
 */
void
BB_SET_Calculate_Dominators(BB_SET *bbset, BOOL compute_dom, BOOL compute_pdom)
{
  CFLOW_Trace_Dom = Get_Trace(TP_FLOWOPT, 0x200);
  MEM_POOL_Push(&MEM_local_pool);
  BB_SET* bs_tmp = BB_SET_Universe(PU_BB_Count+2, &MEM_local_pool);
  std::vector<BB*> set_bbs;
  BOOL changed;

  //
  // Initialize dom and pdom sets for all bb's in the region to
  // be themselve only.
  // Also note that we need to look at sucessors a predecessors outside the current set, 
  // Because otherwise we may falsely compute dominators and or post dominators because we miss
  // control edges.
  //
  INT i;
  BB* bb;
  FOR_ALL_BB_SET_members(bbset, bb) {
    set_bbs.push_back(bb);
    //
    if (compute_dom) {
      Set_BB_dom_set(bb, BS_Create_Empty(2+PU_BB_Count, &dom_pool));
      BS_Union1D(BB_dom_set(bb), BB_id(bb), NULL);      
    }
    if (compute_pdom) {
      Set_BB_pdom_set(bb, BS_Create_Empty(2+PU_BB_Count, &dom_pool));
      BS_Union1D(BB_pdom_set(bb), BB_id(bb), NULL);      
    }
  }
  
  //
  // Find the dominators.
  //
  if (compute_dom) {
    do {
      changed = FALSE;
      for (i = 0; i < set_bbs.size(); ++i) {
	BOOL first_time;
	BBLIST *list;
	BS *bb_dom_set;
	bb = set_bbs[i];
	
	list = BB_preds(bb);
	if ( list == NULL ) continue;
	
	bb_dom_set = BB_dom_set(bb);
	first_time = TRUE;
	BS_ClearD(bs_tmp);
	do {
	  BB *bbp = BBLIST_item(list);
	  if (BB_SET_MemberP(bbset,bbp)) {
	    if (first_time) {
	      BS_CopyD(bs_tmp, BB_dom_set(bbp), NULL);
	      first_time = FALSE;
	    } else {
	      BS_IntersectionR(bs_tmp, bs_tmp, BB_dom_set(bbp));
	    }
	  } else {
	    // There is a predecessor not in the set, therefore, 
	    // we need to treat it as if it's empty
	    first_time = FALSE;
	    BS_ClearD(bs_tmp);
	  }
	} while (list = BBLIST_next(list));
	BS_UnionD(bs_tmp, bb_dom_set, NULL);
	if ( ! BS_EqualP(bs_tmp, bb_dom_set) ) {
	  BS_CopyD(bb_dom_set, bs_tmp, NULL);
	  changed = TRUE;
	}
      }
    } while (changed);
  }
  //
  // Same story as above for post-dominators.
  //
  if (compute_pdom) {
    do {
      changed = FALSE;
      for (i = 0; i < set_bbs.size(); ++i) {
	BOOL first_time;
	BBLIST *list;
	BS *bb_pdom_set;
	bb = set_bbs[i];
      
	list = BB_succs(bb);
	if ( list == NULL ) continue;
      
	bb_pdom_set = BB_pdom_set(bb);
	first_time = TRUE;
	BS_ClearD(bs_tmp);
	do {
	  BB *bbp = BBLIST_item(list);
	  if (BB_SET_MemberP(bbset,bbp)) {
	    if (first_time) {
	      BS_CopyD(bs_tmp, BB_pdom_set(bbp), NULL);
	      first_time = FALSE;
	    } else {
	      BS_IntersectionR(bs_tmp, bs_tmp, BB_pdom_set(bbp));
	    }
	  } else {
	    // There is a successor not in the set, therefore, 
	    // we need to treat it as if it's empty
	    first_time = FALSE;
	    BS_ClearD(bs_tmp);
	  }
	} while (list = BBLIST_next(list));
	BS_UnionD(bs_tmp, bb_pdom_set, NULL);
	if ( ! BS_EqualP(bs_tmp, bb_pdom_set) ) {
	  BS_CopyD(bb_pdom_set, bs_tmp, NULL);
	  changed = TRUE;
	}
      }
    } while (changed);
  }
  
  if ( CFLOW_Trace_Dom ) {
    Vector_Print_Dominators(set_bbs);
  }

  MEM_POOL_Pop(&MEM_local_pool);
}

/* ====================================================================
 *
 *  Free_Dominators_Memory
 *
 *  See interface description
 *
 * ====================================================================
 */
void
Free_Dominators_Memory(void)
{
  /* Clean up our mess.
   */
  MEM_POOL_Pop_Unfreeze(&dom_pool);
  MEM_POOL_Delete(&dom_pool);
  bb_dom_map.size = 0;
  pool_inited = FALSE;
}
