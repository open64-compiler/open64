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


#include <stdint.h>
#include <sys/types.h>
#include <stdio.h>
#include <assert.h>
#include <math.h>
#include "lnopt_main.h"
#include "access_main.h"
#include "access_vector.h"
#include "dep.h"
#include "cxx_memory.h"
#include "soe.h"
#include "dnf.h"
#include "cxx_queue.h"
#include "lwn_util.h"
#include "shackle_ifs.h"
#include "opt_du.h"
#include "lego_util.h"
#include "fiz_fuse.h"
#include "errors.h"
#include "whirl2src.h"
#include "scalar_expand.h"
#include "shackle.h"

static MEM_POOL     *shackle_mem_pool;
extern QUEUE<WN *>  *Extract_Stmts_With_Chain_Id (QUEUE<WN *> *,
						 INT32);
extern BOOL          Ref_Is_Significant (QUEUE<ACCESS_ARRAY *> *,
					ACCESS_ARRAY *);
extern SHACKLE_INFO *Shackle_Info_For_Symbol (QUEUE<SHACKLE_INFO*> *,
					      const    ST*);
extern ST           *Identical_Array_Refbase (WN *, WN *);
extern QUEUE<WN *>  *gather_stmts_in_func(WN *);
extern WN_MAP        shackle_shackle_map;
extern TY_IDX        Shackle_Is_Array_Type (TY_IDX);
extern INT64         Shackle_Base_Type_Size (TY_IDX array_type);

static INT32 
Shackle_Mem_Wn_Depth(WN *main_snl, INT32 current)
{
  if (OPC_BLOCK != WN_opcode (main_snl) &&
      0 == WN_kid_count (main_snl))
    return current;
  else if (OPC_DO_LOOP == WN_opcode (main_snl))
    return Shackle_Mem_Wn_Depth (WN_do_body (main_snl), 
				 current+1);
  else {
    INT32 depth = 0, ndepth;
    FOR_CHILDREN (main_snl, child, ignCount) {
      ndepth = Shackle_Mem_Wn_Depth (child, current);
      if (ndepth > depth)
	depth = ndepth;
    }
    END_CHILDREN;
    return depth;
  }
}

static void
Gather_References_At_Deepest_Depth(WN           *main_snl,
				   INT32         current_depth,
				   INT32         max_depth,
				   QUEUE<WN *>  *ref_queue)
{
  if ((current_depth == max_depth) &&
      OPR_ARRAY == WN_operator (main_snl)) {
    ACCESS_ARRAY *ar_main = (ACCESS_ARRAY *)
      WN_MAP_Get (LNO_Info_Map, main_snl);
    QUEUE_ITER<WN *>   iter(ref_queue);
    WN                *array_step;
    QUEUE<ACCESS_ARRAY*> *test_q = 
      CXX_NEW (QUEUE<ACCESS_ARRAY *> (shackle_mem_pool),
	       shackle_mem_pool);
    BOOL can_add_ref = TRUE;
    while (iter.Step (&array_step)) {
      FmtAssert (OPR_ARRAY == WN_operator (array_step),
		 ("Ref queue contains Non arrays!"));
      ACCESS_ARRAY *test_ar = (ACCESS_ARRAY *)
	WN_MAP_Get (LNO_Info_Map, array_step);
      test_q->Add_Tail_Q (test_ar);
      // If the ref is the same as a ref that already exists,
      // then we should not add this to the queue...
      if (!Ref_Is_Significant (test_q, ar_main)) {
	can_add_ref = FALSE;
	break;
      }
      (void *) test_q->Get_Tail_Q();
      FmtAssert (test_q->Queue_Isempty(),
		 ("Insertion followed by deletion - nonempty!"));
    }
    if (can_add_ref)
      ref_queue->Add_Tail_Q (main_snl);
    return;
  }
  if (OPC_DO_LOOP == WN_opcode (main_snl)) 
    Gather_References_At_Deepest_Depth (WN_do_body (main_snl),
					current_depth+1,
					max_depth,
					ref_queue);
  else {
    FOR_CHILDREN (main_snl, child, ignCount) {
      Gather_References_At_Deepest_Depth (child, current_depth,
					  max_depth, ref_queue);
    }
    END_CHILDREN;
  }
}

void
Shackle_Mem_Initialize(MEM_POOL *pool)
{
  shackle_mem_pool = pool;
}

static QUEUE<WN *> *
Gather_Deepest_References(WN *main_snl)
{
  INT32 depth = Shackle_Mem_Wn_Depth (main_snl, 0);
  QUEUE<WN *> *refq = 
    CXX_NEW (QUEUE<WN *> (shackle_mem_pool), shackle_mem_pool);
  Gather_References_At_Deepest_Depth (main_snl, 0,
				      depth, refq);
  return refq;
}

static BOOL
Volume_Is_Symbolic(SHACKLE_INFO *sh)
{
  INT32 i;

  for (i = 0; i < sh->Ndim(); i++)
    if (!sh->Is_Const_Lower (i) || !sh->Is_Const_Upper (i))
      return TRUE;
  return FALSE;
}

static mINT64
Integral_Volume(SHACKLE_INFO *sh)
{
  mINT64 vol = 1;
  INT32  i;

  for (i = 0; i < sh->Ndim(); i++) {
    FmtAssert(sh->Is_Const_Lower (i) || sh->Is_Const_Upper (i),
	      ("Must have const lower and upper bounds"));
    vol = vol * (sh->Const_Upper(i) - sh->Const_Lower(i));
  }
  return vol;
}

static BOOL
Cache_Is_Normal ()
{
  INT64 current = 0;
  INT32 i;

  for (i = Mhd.First(); i != -1; i = Mhd.Next(i)) {
    MHD_LEVEL *mhdp = &Mhd.L[i];
    if (mhdp->Valid()) {
      if (mhdp->Effective_Size < current)
	return FALSE;
      else
	current = mhdp->Effective_Size;
    }
  }
  FmtAssert (0 != current,
	     ("Must have had some non-zero effective sized cache"));
  return TRUE;
}

static INT32
Last_Level_Of_Cache_Smaller(SHACKLE_INFO *sh)
{
  BOOL is_symbolic = Volume_Is_Symbolic (sh);
  if (is_symbolic)
    return -1;
  BOOL cache_is_normal = Cache_Is_Normal();
  if (!cache_is_normal)
    return -1;
  INT64              volume = Integral_Volume (sh);
  INT32              i, prev = -1;
  MHD_LEVEL         *mhdp;

  for (i = Mhd.First(); i != -1; i = Mhd.Next(i)) {
    mhdp = &Mhd.L[i];
    if (mhdp->Valid()) {
      if (mhdp->Effective_Size > volume)
	return prev;
      else
	prev = i;
    }
  }
  FmtAssert (-1 == prev || mhdp->Effective_Size <= volume,
	     ("Must be empty or too small outermost cache"));
  return prev;
}

static INT64
Nth_Integral_Root (INT64 val, INT64 root)
{
  FmtAssert (root > 0,
	     ("Cannot take a non-positive root of an integer"));
  FmtAssert (val >= 0,
	     ("Cannot take a root of a non-positive integer"));
  double dval = (double) val;
  double dlog = log (dval);
  double droot = exp (dlog / ((double) root));
  FmtAssert (droot >= 0,
	     ("Desired root cannot be negative"));
  return ((INT64) (0.5 + droot));
}

static void
Set_Shackle_Size_Info (SHACKLE_INFO *sh,
		       INT64         allowed_size)
{
  // First, find out if there are any unshackled dimensions
  // with symbolic shackle_sizes

  BOOL  unfortunate_case = FALSE;
  INT32 i;

  for (i = 0; i < sh->Ndim(); i++)
    if (!sh->Is_Dim_Shackled (i) &&
	(!sh->Is_Const_Lower(i) || !sh->Is_Const_Upper (i))) {
      unfortunate_case = TRUE;
      break;
    }
  if (!unfortunate_case) {
    TY_IDX btype = Shackle_Is_Array_Type (ST_type (sh->Symbol()));
    INT64 vol = Shackle_Base_Type_Size(btype);
    FmtAssert (0 != vol,
	       ("Couldn't have been shackling a non-array!"));
    for (i = 0; i < sh->Ndim(); i++) 
      if (!sh->Is_Dim_Shackled (i)) {
	FmtAssert (sh->Is_Const_Lower(i) && sh->Is_Const_Upper (i),
		   ("Not an unfortunate case!"));
	vol = vol * (sh->Const_Upper(i) - sh->Const_Lower(i));
      }
    INT64 common_size = Nth_Integral_Root (allowed_size/vol,
					   sh->Ndim_Shackled());
    if (common_size > SHACKLED_DIM_MIN_SIZE) 
      for (i = 0; i < sh->Ndim(); i++) {
	if (sh->Is_Dim_Shackled (i))
	  sh->Set_Shackle_Dim_Size (i, (INT32) common_size);
      }
    else if (sh->Ndim_Shackled() <= MAX_SHACKLE_DIM_OF_MIN_SIZE)
      for (i = 0; i < sh->Ndim(); i++) {
	if (sh->Is_Dim_Shackled (i)) 
	  sh->Set_Shackle_Dim_Size (i, SHACKLED_DIM_MIN_SIZE);
      }
    else 
      for (i = 0; i < sh->Ndim(); i++) {
	sh->Set_Dim_Shackled (i, FALSE);
	sh->Set_Shackle_Dim_Size (i, 0);
      }
    return;
  }
  else {
    for (i = 0; i < sh->Ndim(); i++) {
      if (sh->Is_Dim_Shackled (i))
	sh->Set_Shackle_Dim_Size (i, (INT32) 
				  SHACKLED_DIM_MIN_SIZE);
    }
    return;
  }
}

static void
Set_Shackle_Size_Info (SHACKLE_INFO *sh,
			QUEUE<WN *> *refq)
{
  if (0 == sh->Ndim_Shackled())
    return;
  INT32 level_of_cache = Last_Level_Of_Cache_Smaller (sh);
  if (-1 == level_of_cache) {
    for (INT32 i = 0; i < sh->Ndim(); i++) {
      sh->Set_Dim_Shackled (i, FALSE);
      sh->Set_Shackle_Dim_Size (i, (INT32) 0);
    }
    return;
  }
  INT64 cache_size = Mhd.L[level_of_cache].Effective_Size;
  INT64 allowed_size = cache_size / refq->Queue_Length();
  Set_Shackle_Size_Info (sh, allowed_size);
}

BOOL
Appropriate_Shackle_Size_Set(WN *main_snl, 
			     QUEUE<SHACKLE_INFO *> *shq)
{
  QUEUE<WN *> *refq = Gather_Deepest_References (main_snl);
  INT32 count = refq->Queue_Length();
  QUEUE<WN *> *stmt_list = gather_stmts_in_func (main_snl);
  QUEUE<WN *> *unchained = 
    Extract_Stmts_With_Chain_Id (stmt_list, 0);
  if (unchained->Queue_Isempty())
    return TRUE;
  WN *stmt = unchained->Queue_First()->Qnode_Item();
  QUEUE<WN *> *shackle = (QUEUE<WN *> *)
    WN_MAP_Get (shackle_shackle_map, stmt);
  QUEUE_ITER<WN *>  iter(shackle);
  WN               *shackle_ref;

  while (iter.Step (&shackle_ref)) {
    const ST *st = Identical_Array_Refbase (shackle_ref, shackle_ref);
    SHACKLE_INFO *sh = Shackle_Info_For_Symbol (shq, st);
    if (NULL == sh)
      return FALSE;
    Set_Shackle_Size_Info (sh, refq);
  }
  return TRUE;
}
