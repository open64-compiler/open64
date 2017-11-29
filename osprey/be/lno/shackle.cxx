/*
 * Copyright (C) 2008-2009 Advanced Micro Devices, Inc.  All Rights Reserved.
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


#include <stdint.h>
#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include <sys/types.h>
#include <stdio.h>
#include <assert.h>
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
#include "debug.h" 
#ifdef KEY
#include "wn_simp.h"            // for WN_Simp_Compare_Trees
#endif

#define SHACKLE_CHAIN_VISITED               1
#define SHACKLE_CHAIN_NOT_VISITED           0

#define Shackle_Chain_Visited_Set(wn) \
  WN_MAP32_Set (shackle_chain_map, (wn), SHACKLE_CHAIN_VISITED)
#define Shackle_Chain_Not_Visited_Set(wn) \
  WN_MAP32_Set (shackle_chain_map, (wn), SHACKLE_CHAIN_NOT_VISITED)
#define Shackle_Chain_Visited(wn) \
  (SHACKLE_CHAIN_VISITED == WN_MAP32_Get (shackle_chain_map, (wn)))
#define Shackle_Chain_Not_Visited(wn) \
  (SHACKLE_CHAIN_NOT_VISITED==WN_MAP32_Get(shackle_chain_map,(wn)))



FILE                   *shackle_debug_file = stdout;
extern ST              *Get_ST_Base(WN *);
static QUEUE<WN *>    **shackle_chain_info;
extern MEM_POOL         LNO_default_pool;

MEM_POOL         shackle_default_pool;
MEM_POOL         shackle_map_pool;
WN_MAP           shackle_ref_map;
WN_MAP           shackle_chain_map;
WN_MAP           shackle_chain_id_map;
static INT32     shackle_chain_id_map_cnt;
WN_MAP           shackle_shackle_map;
static INT32     shackle_debug_level;
extern WN       *Sh_LWN_CreateDivceil  (TYPE_ID, WN *, WN *);
extern WN       *Sh_LWN_CreateDivfloor (TYPE_ID, WN *, WN *);
extern BOOL      Is_Parent_Of (WN *, WN*);
extern WN       *Simplify_If_Conditional (WN *);
static BOOL      _xis_legal_shackle(QUEUE<WN *>          *stmts,
				    QUEUE<SHACKLE_INFO*> *shq);
extern BOOL      Appropriate_Shackle_Size_Set (WN *,
					       QUEUE<SHACKLE_INFO*>*);
extern void      Shackle_Mem_Initialize(MEM_POOL *);
QUEUE<WN *> *    gather_stmts_in_func(WN *func_nd);
ST*              Identical_Array_Refbase (WN *array1, WN *array2);
SHACKLE_INFO *   Shackle_Info_For_Symbol(QUEUE<SHACKLE_INFO *> *,
					 const ST              *);
static void      Dump_Shackle_Map_Info (WN *func_nd, 
					QUEUE<WN *> *s);



static QUEUE<SHACKLE_INFO *> *
Shackle_Info_For_Shackled_Arrays(WN *main_snl,
				 QUEUE<SHACKLE_INFO *> *shq)
{
  QUEUE<SHACKLE_INFO *> *result = 
    CXX_NEW (QUEUE<SHACKLE_INFO *> (&shackle_default_pool),
	     &shackle_default_pool);
  QUEUE<WN *>     *stmts = gather_stmts_in_func (main_snl);
  QUEUE_ITER<WN *> iter(stmts);
  WN              *step;

  while (iter.Step (&step)) {
    QUEUE<WN *> *shackle = (QUEUE<WN *> *)
      WN_MAP_Get (shackle_shackle_map, step);
    FmtAssert (NULL != shackle && !shackle->Queue_Isempty(),
	       ("Some shackled ref must exist!"));
    QUEUE_ITER<WN *>     iter2(shackle);
    WN                  *step2;
    while (iter2.Step (&step2)) {
      const ST *st = Identical_Array_Refbase (step2, step2);
      FmtAssert (NULL != st, ("Can't have a NULL st shackle!"));
      SHACKLE_INFO *sh = Shackle_Info_For_Symbol (shq, st);
      FmtAssert (NULL != sh, ("Shackle info must exist for st!"));
      result->Index (sh, TRUE);
    }
  }
  return result;
}

static BOOL
Ref_Contains_Reuse_For_Loop (WN *array_ref, INT32 index,
			     SHACKLE_INFO *sh)
{
  if (NULL == array_ref)
    return FALSE;
  FmtAssert (OPR_ARRAY == WN_operator (array_ref),
	     ("Call Ref_Contains_Reuse_For_Loop with OPR_ARRAY!"));
  const ST *st = Identical_Array_Refbase (array_ref, array_ref);
  if (st != sh->Symbol())
    return FALSE;
  ACCESS_ARRAY *ar = 
    (ACCESS_ARRAY *) WN_MAP_Get (LNO_Info_Map, array_ref);
  if (ar->Too_Messy)
    return FALSE;
  INT32 i;

  for (i = 0; i < ar->Num_Vec(); i++) {
    ACCESS_VECTOR *av = ar->Dim (i);
    if (av->Too_Messy)
      return FALSE;
    FmtAssert (0 <= index && index < av->Nest_Depth(),
	       ("Invalid index for this array reference"));
    // Reuse not *along* this loop
    if (0 != av->Loop_Coeff (index))
      return FALSE;
  }
  return TRUE;
}

static BOOL
Reuse_Exists_For_Loop_For_Array (WN *main_snl, INT32 index,
				 SHACKLE_INFO *sh)
{
  if (OPC_DO_LOOP == WN_opcode (main_snl))
    return Reuse_Exists_For_Loop_For_Array (WN_do_body (main_snl),
					    index, sh);
  else if (OPR_ARRAY == WN_operator (main_snl))
    return Ref_Contains_Reuse_For_Loop (main_snl, index, sh);
  else {
    FOR_CHILDREN (main_snl, child, ignCount) {
      BOOL result = 
	Reuse_Exists_For_Loop_For_Array (child, index, sh);
      if (TRUE == result)
	return TRUE;
    }
    END_CHILDREN;
    return FALSE;
  }
}

static BOOL
Reuse_Exists_In_Loop (WN *main_snl, QUEUE<SHACKLE_INFO *> *shq)
{
  FmtAssert (OPC_DO_LOOP == WN_opcode (main_snl),
	     ("Reuse_Exists_In_Loop must be called w/ do_loop"));
  INT32                       depth = Do_Loop_Depth (main_snl);
  QUEUE<SHACKLE_INFO *>      *shackled = 
    Shackle_Info_For_Shackled_Arrays (main_snl, shq);
  QUEUE_ITER<SHACKLE_INFO *>  iter(shackled);
  SHACKLE_INFO               *sh;

  while (iter.Step (&sh)) {
    if (Reuse_Exists_For_Loop_For_Array (main_snl, depth, sh))
      return TRUE;
  }
  return FALSE;
}

static void
Shackle_Postprocess_Do_Loop_Bounds(WN *wn)
{
  if (OPC_DO_LOOP == WN_opcode (wn)) {
    DO_LOOP_INFO *dli = Get_Do_Loop_Info (wn);
    DOLOOP_STACK do_stack (&shackle_default_pool);
    Build_Doloop_Stack (wn, &do_stack);
    dli->Set_Est_Num_Iterations (&do_stack);
  }
  FOR_CHILDREN (wn, child, ignCount) {
    Shackle_Postprocess_Do_Loop_Bounds (child);
  }
  END_CHILDREN;
}


static WN *
Find_Wn_Storing_ST_IDX(ST_IDX st, WN *wn)
{
  WN *result;

  switch (WN_operator (wn)) {
  case OPR_STID:
    if (st == WN_st_idx (wn)) 
      return wn;
    else
      return NULL;
  default:
    FOR_CHILDREN (wn, child, ignCount) {
      result = Find_Wn_Storing_ST_IDX (st, child);
      if (NULL != result)
	return result;
    }
    END_CHILDREN;
    return NULL;
  }
}

INT64
Shackle_Base_Type_Size (TY_IDX array_type)
{
  if (array_type == TY_IDX_ZERO)
    return 0;
  else if (KIND_ARRAY != TY_kind (array_type))
    return 0;
  else {
    TY_IDX step = array_type;
    while (1) {
      if (KIND_SCALAR == TY_kind (step) ||
	  KIND_STRUCT == TY_kind (step)) 
	return TY_size (step);
      else if (KIND_ARRAY == TY_kind (step))
	step = TY_etype (step);
      else
	return 0;
    }
  } 
}

TY_IDX 
Shackle_Is_Array_Type(TY_IDX ty)
{
  if (ty == TY_IDX_ZERO)
    return TY_IDX_ZERO;
  if (KIND_ARRAY == TY_kind (ty)) 
    return ((Shackle_Base_Type_Size (ty) == 0) ? TY_IDX_ZERO : ty);
  else if (KIND_POINTER == TY_kind (ty)) {
    if (KIND_ARRAY == TY_kind (TY_pointed (ty)))
      return ((Shackle_Base_Type_Size (TY_pointed (ty)) == 0) ? 
              TY_IDX_ZERO : TY_pointed (ty));
    else
      return TY_IDX_ZERO;
  }
  else
    return TY_IDX_ZERO;
}

static WN *
Find_Do_Loop(WN *root) 
{
  QUEUE_WKLIST_ITER<WN *> wklist (root, &shackle_default_pool);
  WN                     *wk_item;

  while (wklist.Step (&wk_item)) {
    switch (WN_opcode (wk_item)) {
    case OPC_DO_LOOP:
      return wk_item;
    default:
      FOR_CHILDREN (wk_item, child, ignCount) {
	wklist.Wklist_Queue()->Add_Tail_Q (child);
      }
      END_CHILDREN;
    }
  }
  return NULL;
}

// Finds the "largest" type that needs to be defined to be able
// to handle the shackle loops
static TYPE_ID
Find_Highest_Type_Of_Loop(WN *func_nd)
{
  WN *step = func_nd;
  TYPE_ID retval, current;
  BOOL type_found = FALSE;

  QUEUE_WKLIST_ITER<WN *> wklist (func_nd, &shackle_default_pool);
  WN                     *wk_item;

  while (wklist.Step (&wk_item)) {
    switch (WN_opcode (wk_item)) {
    case OPC_DO_LOOP:
      current = Promote_Type (Do_Wtype (wk_item));
      if (FALSE == type_found) 
	retval = current;
      else if (current != retval) {
	if (((retval == MTYPE_U4) && (current == MTYPE_U8)) ||
	    ((MTYPE_U8 == retval) && (MTYPE_U4 == current))) 
	  retval = MTYPE_U8;
	else if (((retval == MTYPE_I4) && (current == MTYPE_I8)) ||
		 ((MTYPE_I8 == retval) && (MTYPE_I4 == current))) 
	  retval = MTYPE_I8;
	else
	  return MTYPE_V;
      }
      wklist.Wklist_Queue()->Add_Tail_Q (WN_do_body (wk_item));
      break;
    default:
      FOR_CHILDREN (wk_item, child, ignCount) {
	wklist.Wklist_Queue()->Add_Tail_Q (child);
      }
      END_CHILDREN;
    }
  }
  return retval;
}

static WN *
Create_Loop_Symbol (const ST *st, INT32 i, 
		    TYPE_ID type)
{
  char buf[128];

  sprintf(buf, "S%s%d", ST_name (st), i);
  SYMBOL newsym = Create_Preg_Symbol (buf, type);
  return WN_CreateIdname (newsym.WN_Offset(), newsym.St());
}

SHACKLE_INFO::SHACKLE_INFO(const ST *st, 
			   WN       *func_nd,
			   MEM_POOL *pool,
			   TYPE_ID   type,
			   BOOL      inquire_info)
{
#ifndef _NEW_SYMTAB
  // Does st correspond to an array type? 
  TY_IDX ty = Shackle_Is_Array_Type (ST_type (st));
  INT32 i;
  const INT32 _default_shackle_size = 0;

  _pool = pool;
  _next_loop_count = 0;
  _st_is_reshaped = FALSE;
  if (ty != TY_IDX_ZERO) {
    _st = st;
    _ndim = ARI_ndims (TY_arinfo (ty));
    _is_shackled = CXX_NEW_ARRAY (BOOL, _ndim, pool);
    _shackle_sizes = CXX_NEW_ARRAY (INT32, _ndim, pool);
    _loop_stmts = CXX_NEW_ARRAY (WN *, _ndim, pool);
    _array_bounds = ARI_bnds (TY_arinfo (ty));
    _type_to_give = type;
    // Initialize conditions:
    for (i = 0; i < _ndim; i++) {
      if (inquire_info) {
	INT32 size;
	scanf("%d", &size);
	if (size > 0) {
	  _is_shackled[i] = TRUE;
	  _shackle_sizes[i] = size;
	  _loop_stmts[i] = NULL;
	} else {
	  _is_shackled[i] = FALSE;
	  _shackle_sizes[i] = 0;
	  _loop_stmts[i] = NULL;
	}
      } else { // default conditions..

	// Fix the comment in the next few lines - for the moment
	// we'll always shackle even if we don't have constant
	// symbolic upper and lower bounds
	/*
	  This weird NULL != was introduced bec. if I have a 
	  declaration like int y(*), the size of y is not known
	  and the expression appears as a NULL expr upper bnd 
	  if ((ARB_const_lbnd (_array_bounds[i]) ||
	  (NULL != ARB_lbnd_tree (_array_bounds[i]))) &&
	  (ARB_const_ubnd (_array_bounds[i]) ||
	  (NULL != ARB_ubnd_tree (_array_bounds[i]))))
	  */
	_is_shackled[i] = TRUE;
	_shackle_sizes[i] = _default_shackle_size;
	_loop_stmts[i] = NULL;
      }
    }

  }
  else {
    _st = NULL;
    _ndim = 0;
    _is_shackled = NULL;
    _shackle_sizes = NULL;
    _array_bounds = NULL;
    _loop_stmts = NULL;
    _type_to_give = MTYPE_V;
  }
#else
  TY_IDX ty = Shackle_Is_Array_Type (ST_type (st));
  INT32 i;
  const INT32 _default_shackle_size = 0;

  _pool = pool;
  _next_loop_count = 0;
  _st_is_reshaped = FALSE;
  if (ty != TY_IDX_ZERO) {
    _st = st;
    _ndim = TY_AR_ndims (ty);
    _is_shackled = CXX_NEW_ARRAY (BOOL, _ndim, pool);
    _shackle_sizes = CXX_NEW_ARRAY (INT32, _ndim, pool);
    _loop_stmts = CXX_NEW_ARRAY (WN *, _ndim, pool);
    _cached_lb_expr_st = CXX_NEW_ARRAY (WN*, _ndim, pool);
    _cached_ub_expr_st = CXX_NEW_ARRAY (WN*, _ndim, pool);
    _array_bounds = TY_arb (ty);
    _type_to_give = type;
    // Initialize conditions:
    for (i = 0; i < _ndim; i++) {
      if (inquire_info) {
	INT32 size;
	scanf("%d", &size);
	if (size > 0) {
	  _is_shackled[i] = TRUE;
	  _shackle_sizes[i] = size;
	  _loop_stmts[i] = NULL;
	} else {
	  _is_shackled[i] = FALSE;
	  _shackle_sizes[i] = 0;
	  _loop_stmts[i] = NULL;
	}
      } else { // Default conditions 
	_is_shackled[i] = TRUE;
	_shackle_sizes[i] = _default_shackle_size;
	_loop_stmts[i] = NULL;
      }
      TYPE_ID desc_type;
      if (!TY_AR_const_lbnd (ty, i)) {
	WN *st_lb = 
	  Find_Wn_Storing_ST_IDX (TY_AR_lbnd_var (ty, i), func_nd);
	_cached_lb_expr_st[i] = st_lb;
	if (NULL == st_lb)
	  _is_shackled[i] = FALSE;
      }
      else {
	_cached_lb_expr_st[i] = NULL;
      }
      if (!TY_AR_const_ubnd (ty, i)) {
	ST_IDX st_ub_idx = TY_AR_ubnd_var (ty, i);
	WN *st_ub = Find_Wn_Storing_ST_IDX (st_ub_idx, func_nd);
	_cached_ub_expr_st[i] = st_ub;
	if (NULL == st_ub)
	  _is_shackled[i] = FALSE;
      }
      else {
	_cached_ub_expr_st[i] = NULL;
      }
    }
  } else {
    _st = NULL;
    _ndim = 0;
    _is_shackled = NULL;
    _shackle_sizes = NULL;
    _array_bounds = ARB_HANDLE();
    _loop_stmts = NULL;
    _type_to_give = MTYPE_V;
  }
#endif
}

INT32
SHACKLE_INFO::Ndim_Shackled()
{
  INT32 i, count = 0;
  for (i = 0; i < _ndim; i++) {
    if (_is_shackled[i]) 
      count++;
  }
  return count;
}
WN*
SHACKLE_INFO::Loop()
{
  return Create_Loop_Symbol (_st, ++_next_loop_count, _type_to_give);
}
  
QUEUE<WN *> *
gather_stmts_in_func(WN *func_nd)
{
  QUEUE<WN *> *result = CXX_NEW (QUEUE<WN *>(&shackle_default_pool),
				 &shackle_default_pool);
  QUEUE_WKLIST_ITER<WN *> wklist(func_nd, &shackle_default_pool);
  WN *                    wk_item;
  
  while (wklist.Step(&wk_item)) {
    switch (WN_opcode (wk_item)) {
    case OPC_DO_LOOP:
      /* If we have a do loop, just add its body, and
	 nothing else */
      wklist.Wklist_Queue()->Add_Tail_Q (WN_do_body (wk_item));
      break;
    default:
      if (OPCODE_is_stmt (WN_opcode (wk_item))) {
	if ((WN_opcode (wk_item) != OPC_RETURN) &&
	    (WN_opcode (wk_item) != OPC_PRAGMA) &&
	    (WN_opcode (wk_item) != OPC_XPRAGMA)) {
	  // printf("%R: %s\n", OPCODE_name (WN_opcode (wk_item)));
	  result->Add_Tail_Q (wk_item);
	}
      }
      else {
	FOR_CHILDREN (wk_item, child, ignCount) {
	  assert (child != (WN *) NULL);
	  wklist.Wklist_Queue()->Add_Tail_Q (child);
	}
	END_CHILDREN;
      }
    }
  }
  return result;
}

static BOOL
_xfunc_has_stmts2prevent_shackle(QUEUE<WN *> *stmts)
{
  /* We'll look at the set of statements we have collected
     for this function (which have excluded pragmas, and 
     return statements. If we have only store statements,
     this is a function that has no unshackle-able statements */
  QUEUE_ITER<WN *> iter(stmts);
  WN *             stmt;

  while (iter.Step(&stmt)) {
    if (!OPCODE_is_store (WN_opcode (stmt)))
      return TRUE;
  }
  return FALSE;
}

#ifdef KEY
// We just want to return a ST that is meaningful
static ST* Array_Base_St (WN* node)
{
  if (OPCODE_is_load(WN_opcode(node)) && WN_has_sym(node)) return WN_st(node);

  // Recurse
  ST* sym;
  for (INT kid = 0; kid < WN_kid_count(node); kid ++)
    if (sym = Array_Base_St(WN_kid(node, kid)))
      return sym;

  return NULL;
}
#endif

ST*
Identical_Array_Refbase (WN *array1, WN *array2)
{
  ST *base1, *base2;
  WN *t1, *t2;

  WN *p1 = LWN_Get_Parent (array1);
  WN *p2 = LWN_Get_Parent (array2);

  // if either parent is an LDID or STID, we've probably taken
  // the address of an array ref. No point in further analysis..
  if (OPR_LDID == WN_operator (p1) || OPR_STID == WN_operator (p1) ||
      OPR_LDID == WN_operator (p2) || OPR_STID == WN_operator (p2))
    return NULL;

  /* Look at the type of the base address */
  t1 = WN_array_base (array1);
  t2 = WN_array_base (array2);

  if ((OPCODE_is_load (WN_opcode (p1)) || 
       OPCODE_is_store (WN_opcode (p1))) &&
      (OPCODE_is_load (WN_opcode (p2)) || 
       OPCODE_is_store (WN_opcode (p2)))) {
    if ((DEP_CONTINUE == DEPV_COMPUTE::Base_Test (p1, NULL, 
						 p2, NULL))
	&& (WN_operator(t1) != OPR_ILOAD)) {
      // assert (WN_array_base (array1) == WN_array_base (array2));
#ifdef KEY // Bug 2484
      if (WN_Simp_Compare_Trees(t1, t2) != 0)
	return NULL;
      else if (WN_kid_count(t1) != 0)
	return Array_Base_St(t1);
#endif
      if (WN_st (t1) == WN_st (t2))
	return WN_st (t1);
      else
	return NULL;
    }
  }
  assert (WN_operator (array1) == OPR_ARRAY);
  assert (WN_operator (array2) == OPR_ARRAY);
  /* If the two opcodes are not equal, then probably they are
     not the same reference */
  if (WN_operator (t1) != 
      WN_operator (t2))
    return NULL;
  if (OPR_LDID == WN_operator (t1)) {
    if ((WN_st (t1) == WN_st (t2)) &&
	(WN_offset (t1) == WN_offset (t2)))
      return WN_st (t1);
  }
  base1 = Get_ST_Base (array1);
  base2 = Get_ST_Base (array2);
  /* If this was not trapped by LDID, no hope */
  if ((NULL == base1) || (NULL == base2)) {
    return NULL;
  }
  else if (base1 == base2) 
    return base1;
  else
    return NULL;
}

static BOOL
_xis_simple_shackle_case (QUEUE<WN *> *stmts)
{
  WN              *array, *first_array;
  INT32            count = 0;
  ACCESS_ARRAY    *ar;
  QUEUE_ITER<WN *> iter (stmts);
  WN              *stmt;

  /* A shackling is potentially simple if the same array appears 
     on the left hand side of every assignment statement. While
     this appears to be very restricted, it is amazing how often
     this does work */
  while (iter.Step(&stmt)) {
    assert (OPCODE_is_store (WN_opcode (stmt)));
    assert (WN_operator (stmt) != OPR_ISTOREX);
    // If this is a scalar, then not a simple case of shackling
    if (OPR_STID == WN_operator (stmt)) {
      return FALSE;
    }
    /* Now, for the remaining store opcodes types: In
       all these cases, we are basically storing some
       value to an address computed by Kid 1 plus other
       stuff. So, kid 1 is the center of attention */
    array = WN_kid (stmt, 1);
    /* We have scalars, it is certainly not a simple case */
    if (WN_operator(array) != OPR_ARRAY)
      return FALSE;
    if (0 == count) {
      first_array = array;
    }
    /* Obtain the array access function for the lhs ref */
    ar = (ACCESS_ARRAY *)WN_MAP_Get (LNO_Info_Map, array);
    if (shackle_debug_level > 0) 
      ar->Print (stdout, FALSE);
    /* If not affine, can't shackle */
    if (Bound_Is_Too_Messy (ar))
      return FALSE;
    /* Is it the same array on the lhs in all statements ? */
    /* If two things belong to the same equiv class */
    if (NULL == Identical_Array_Refbase (array, first_array))
      return FALSE;
    count++;
  }
  return TRUE;
}

static void
Form_Statement_Refs(QUEUE<WN *> *stmts)
{
  /* For every statement, we take all the affine references
     and stuff them as a vector of references in a map_array
     that we create for that purpose */
  INT32                     count, i;
  QUEUE<WN *>              *tmp;
  QUEUE_ITER<WN *>          iter(stmts);
  WN                       *stmt, *wk_item;
  QUEUE_WKLIST_ITER<WN *>  *wklist;
 
  while (iter.Step(&stmt)) {
    tmp = CXX_NEW (QUEUE<WN *> (&shackle_default_pool),
		   &shackle_default_pool);
    wklist = CXX_NEW (QUEUE_WKLIST_ITER<WN *> (stmt, 
					       &shackle_default_pool),
		      &shackle_default_pool);
    while (wklist->Step(&wk_item)) {
      if (WN_operator (wk_item) == OPR_ARRAY) 
	tmp->Add_Tail_Q (wk_item);
      else {
	FOR_CHILDREN (wk_item, child, ignCount) {
	  assert (child != (WN *) NULL);
	  wklist->Wklist_Queue()->Add_Tail_Q (child);
	}
	END_CHILDREN;
      }
    }
    /* At this stage, we have all the top level array references
       in this statement lined up. Go thru them one by one and
       compute a vector, and store that in shackle_ref_map */
    if (shackle_debug_level > 0) 
      printf("Number of refs in Stmt: %d\n", tmp->Queue_Length());
    WN_MAP_Set (shackle_ref_map, stmt, (void *) tmp);
  }
}

static BOOL
_xis_avect_linear_comb_amat_queue(QUEUE<ACCESS_ARRAY*> *s,
				  ACCESS_VECTOR *q)
{
  /* What we want to do is this: s is a queue of access
     matrices. Each access matrix has a bunch of access 
     vectors as its rows. Given a bunch of these access 
     vectors from a bunch of access matrices, we want to
     determine if a query access vector q is a linear
     combination of these access vectors. We'll do this
     by formulating a system of equations to be passed to
     the soe solver. The system will pretend that q is
     in fact a linear combination, and will try to find
     a solution */
  mUINT16                   count;
  INT32                      i, j, k;
  INT32                      nvars;
  mINT32                    *eqrow;
  SYSTEM_OF_EQUATIONS       *soe;
  ACCESS_VECTOR             *v;
  QUEUE_ITER<ACCESS_ARRAY*> *iter;
  ACCESS_ARRAY              *m;

  nvars = 0;
  iter = CXX_NEW (QUEUE_ITER<ACCESS_ARRAY *>(s),
		  &shackle_default_pool);
  while (iter->Step(&m)) {
    nvars += m->Num_Vec();
  }
  soe = CXX_NEW (SYSTEM_OF_EQUATIONS (0, 0, nvars,
				      &shackle_default_pool),
		 &shackle_default_pool);
  eqrow = CXX_NEW_ARRAY (mINT32, nvars, &shackle_default_pool);
  for (i = 0; i < q->Nest_Depth(); i++) {
    count = 0;
    iter = CXX_NEW (QUEUE_ITER<ACCESS_ARRAY *>(s),
		    &shackle_default_pool);
    while (iter->Step(&m)) {
      for (j = 0; j < m->Num_Vec(); j++) {
	eqrow[count] = m->Dim(j)->Loop_Coeff(i);
	count++;
      }
    }
    soe->Add_Eq (eqrow, (INT64) q->Loop_Coeff (i));
  }
  /* If the system has a solution, then q is indeed a linear
     combination, otherwise q is not. We simply return the
     satisfiablility of the system of linear equations */
  return soe->Is_Consistent();
}

/* A reference is significant w.r.t a queue of other references iff 
   at least one row of its access matrix is not a linear combination 
   of all the rows of the access matrices of the other references */
BOOL
Ref_Is_Significant(QUEUE<ACCESS_ARRAY *> *s,
			 ACCESS_ARRAY *q)
{
  INT32 i;

  /* If we find even one row which is not a linear combination
     of the access matrices, we have a significant reference */
  for (i = 0; i < q->Num_Vec(); i++) {
    if (!_xis_avect_linear_comb_amat_queue (s, q->Dim(i)))
      return TRUE;
  }
  return FALSE;
}

static void
test_significance(QUEUE<WN *> *s)
{

  QUEUE<ACCESS_ARRAY*> *tst;
  QUEUE<WN *>          *refQ;
  ACCESS_ARRAY         *a1, *a2;
  WN                   *t1, *t2, *stmt;
  BOOL                  b;
  QUEUE_ITER<WN*>       iter_s (s), *iter_ref;
  /* S is a queue of statements. For each statement, we wish
     to check if the significant computation works or not */
  while (iter_s.Step(&stmt)) {
    refQ = (QUEUE<WN *> *) WN_MAP_Get (shackle_ref_map, stmt);
    tst = CXX_NEW (QUEUE<ACCESS_ARRAY *> (&shackle_default_pool),
		   &shackle_default_pool);
    t1 = refQ->Queue_First()->Qnode_Item();
    a1 = (ACCESS_ARRAY *) WN_MAP_Get (LNO_Info_Map, t1);
    tst->Add_Tail_Q (a1);
    a1->Print (stdout, FALSE);
    iter_ref = CXX_NEW (QUEUE_ITER<WN*> (refQ), &shackle_default_pool);
    while (iter_ref->Step(&t2)) {
      a2 = (ACCESS_ARRAY *) WN_MAP_Get (LNO_Info_Map, t2);
      fprintf(stdout, "\t");
      b = Ref_Is_Significant (tst, a2);
      if (FALSE == b) 
	fprintf(stdout, "Not significant: ");
      else
	fprintf(stdout, "Significant: ");
      a2->Print (stdout, FALSE);
    }
  }
}

static void
_xcreate_simple_basic_shackle(QUEUE<WN *> *s)
{
  /* At this stage, we have verified that the same array ref occurs
     on the left of *every* assignment statement. So, we'll init 
     our basic shackle mechanism. We'll allocate the shackle_
     shackle_map for every statement */
  INT32             i, count;
  QUEUE<WN *>      *ref_list, *shackle_list;
  WN               *ref;
  QUEUE_ITER<WN*>   iter(s), *iter2;
  WN               *stmt;

  // we use a slightly different strategy in the case when there
  // is only one statement being shackled..
  if (shackle_debug_level > 0)
    printf("%d Statements in Func\n", s->Queue_Length());
  if (1 == s->Queue_Length()) {
    iter.Step(&stmt);
    UINT32 max_dim;
    assert (OPCODE_is_store (WN_opcode (stmt)));
    assert (OPCODE_is_stmt (WN_opcode (stmt)));
    ref_list = (QUEUE<WN *> *) WN_MAP_Get (shackle_ref_map, stmt);
    assert (NULL != ref_list);
    iter2 = CXX_NEW (QUEUE_ITER<WN *> (ref_list), 
		     &shackle_default_pool);
    max_dim = 0;
    while (iter2->Step (&ref)) {
      ACCESS_ARRAY *ar = 
	(ACCESS_ARRAY *) WN_MAP_Get (LNO_Info_Map, ref);
      if ((!ar->Too_Messy) && (ar->Num_Vec() > max_dim))
	max_dim = ar->Num_Vec();
    }
    // There should be at least one array!
    assert (0 != max_dim);
    iter2 = CXX_NEW (QUEUE_ITER<WN *> (ref_list), 
		     &shackle_default_pool);
    while (iter2->Step(&ref)) {
      ACCESS_ARRAY *ar = 
	(ACCESS_ARRAY *) WN_MAP_Get (LNO_Info_Map, ref);
      if ((!ar->Too_Messy) && (ar->Num_Vec() == max_dim)) {
	shackle_list = CXX_NEW (QUEUE<WN*> (&shackle_default_pool),
				&shackle_default_pool);
	shackle_list->Add_Tail_Q (ref);
	WN_MAP_Set (shackle_shackle_map, stmt, (void *) shackle_list);
	return;
      }
    }
  } 
  else {
    while (iter.Step(&stmt)) {
      assert (OPCODE_is_store (WN_opcode (stmt)));
      assert (OPCODE_is_stmt (WN_opcode (stmt)));
      ref_list = (QUEUE<WN *> *) WN_MAP_Get (shackle_ref_map, stmt);
      assert (ref_list != NULL);
      shackle_list = CXX_NEW (QUEUE<WN*> (&shackle_default_pool),
			      &shackle_default_pool);
      ref = ref_list->Queue_First()->Qnode_Item();
      shackle_list->Add_Tail_Q (ref);
      WN_MAP_Set (shackle_shackle_map, stmt, (void *) shackle_list);
    }
  }
}
static BOOL
Is_Ref_Significant_In_Stmt(WN *query_ref, WN *stmt)
{
  QUEUE_ITER<WN *>         *shackle_iter;
  WN                       *shackle;
  QUEUE<WN *>              *shackleQ;
  QUEUE<ACCESS_ARRAY *>    *arq;
  ACCESS_ARRAY             *ar;

  shackleQ = (QUEUE<WN *> *) WN_MAP_Get (shackle_shackle_map,
					 stmt);
  shackle_iter = CXX_NEW (QUEUE_ITER<WN *> (shackleQ), 
			  &shackle_default_pool);
  
  arq = CXX_NEW (QUEUE<ACCESS_ARRAY*>(&shackle_default_pool),
		 &shackle_default_pool);
  while (shackle_iter->Step(&shackle)) {
    ar = (ACCESS_ARRAY *) WN_MAP_Get (LNO_Info_Map, shackle);
    if (!ar->Too_Messy) 
      arq->Add_Tail_Q (ar);
  }
  ar = (ACCESS_ARRAY *) WN_MAP_Get (LNO_Info_Map, query_ref);
  if (!ar->Too_Messy) 
    return Ref_Is_Significant (arq, ar);
  else
    return FALSE;
}

static WN*
Has_Stmt_Significant_Ref(WN *stmt)
{
  WN *ref;
  QUEUE<WN *> *refQ;
  QUEUE_ITER<WN *> *ref_iter;

  refQ = (QUEUE<WN *> *) WN_MAP_Get (shackle_ref_map, stmt);
  ref_iter = CXX_NEW (QUEUE_ITER<WN *> (refQ), 
		      &shackle_default_pool);
  while (ref_iter->Step (&ref)) {
    if (Is_Ref_Significant_In_Stmt (ref, stmt))
      return ref;
  }
  return NULL;
}

static void
Augment_Simple_Basic_Shackle (QUEUE<WN *>          *s,
			      QUEUE<SHACKLE_INFO*> *sinfo)
{
  QUEUE_ITER<WN *>     *iter, *ref_iter, *shackle_iter;
  WN                   *step, *step2, *ref, *ref2;
  QUEUE<WN *>          *refQ, *shackleQ, *refQ2, *shackleQ2;
  BOOL                  can_be_augmented = FALSE;

  // Dump_Shackle_Map_Info (func_nd, s);
  iter = CXX_NEW (QUEUE_ITER<WN *> (s), &shackle_default_pool);
  while (iter->Step (&step)) {
    if (Has_Stmt_Significant_Ref (step)) {
      can_be_augmented = TRUE;
      break;
    }
  }
  if (!can_be_augmented)
    return;
  refQ = (QUEUE<WN*> *) WN_MAP_Get (shackle_ref_map, step);
  ref_iter = CXX_NEW (QUEUE_ITER<WN *> (refQ),
		      &shackle_default_pool);
  while (ref_iter->Step (&ref)) {
    if (Is_Ref_Significant_In_Stmt (ref, step)) {
      shackleQ = (QUEUE<WN *> *) WN_MAP_Get (shackle_shackle_map,
					     step);
      shackleQ->Add_Tail_Q (ref);
      WN_MAP_Set (shackle_shackle_map, step, 
		  (void *) shackleQ);
      iter = CXX_NEW (QUEUE_ITER<WN *> (s), &shackle_default_pool);
      while (iter->Step (&step2)) {
	if (step != step2) {
	  shackleQ2 = 
	    (QUEUE<WN *> *) WN_MAP_Get (shackle_shackle_map,
					step2);
	  ref2 = Has_Stmt_Significant_Ref (step2);
	  if (NULL == ref2) {
	    ref2 = shackleQ2->Queue_First()->Qnode_Item();
	  }
	  assert (ref2 != NULL);
	  shackleQ2->Add_Tail_Q (ref2);
	  WN_MAP_Set (shackle_shackle_map, step2,
		      (void *) shackleQ2);
	}
      }
      // Dump_Shackle_Map_Info (func_nd, s);
      if (_xis_legal_shackle (s, sinfo))
	return;
      iter = CXX_NEW (QUEUE_ITER<WN *> (s), &shackle_default_pool);
      // If this was not a legal shackle, just delete
      // the latest level of composition..
      while (iter->Step (&step)) {
	shackleQ2 = (QUEUE<WN *> *) WN_MAP_Get (shackle_shackle_map,
						step);
	shackleQ2->Get_Tail_Q();
      }
    }
  }    
}
#ifndef _NEW_SYMTAB
static QUEUE<SHACKLE_INFO*> *
_xcreate_shackle_map_for_arrays_in_func(WN *main_snl,
					WN *func_nd)
{
  QUEUE<SHACKLE_INFO *> *si;
  ST                    *st;
  TY_IDX                 ty;
  SHACKLE_INFO          *sh;
  TYPE_ID                type;
  SYMTAB_IDX             symtab = Current_Symtab;

  si = CXX_NEW (QUEUE<SHACKLE_INFO *> (&shackle_default_pool),
		&shackle_default_pool);
  /* We go through each of the symbols in the symtab. If it is
     an array (i.e. declared in the usual manner), or a pointer
     to an array type - (as would happen when we are passing 
     ararys as formal parameters), then, we create a shackle
     info structure for this node, and add it to the return */
  type = Find_Highest_Type_Of_Loop (main_snl);
  for (st = SYMTAB_symbols (symtab); NULL != st; 
       st = ST_next (st)) {
    ty = Shackle_Is_Array_Type (ST_type (st));
    if (NULL != ty) {
      sh = CXX_NEW (SHACKLE_INFO (st, func_nd, 
				  &shackle_default_pool,
				  type, FALSE),
		    &shackle_default_pool);
      si->Add_Tail_Q (sh);
    }
  }
  return si;
}
#else
static QUEUE<SHACKLE_INFO*> *
_xcreate_shackle_map_for_arrays_in_func(WN *main_snl,
					WN *func_nd)
{
  QUEUE<SHACKLE_INFO*> *si;
  TY_IDX                ty;
  TYPE_ID               type;
  ST                   *st;
  SHACKLE_INFO         *sh;
  ST_TAB                st_table;
  ST_ITER               st_iter;
  UINT32                count;
  INT                   i;

  si = CXX_NEW (QUEUE<SHACKLE_INFO *> (&shackle_default_pool),
		&shackle_default_pool);
  type = Find_Highest_Type_Of_Loop (main_snl);

  FOREACH_SYMBOL (CURRENT_SYMTAB, st, i) {
    if (CLASS_UNK == ST_sym_class (st) ||
	CLASS_BLOCK == ST_sym_class (st) ||
	CLASS_COUNT == ST_sym_class (st)) 
      continue;
    ty = Shackle_Is_Array_Type (ST_type (st));
    if (ty != TY_IDX_ZERO) {
      if (shackle_debug_level > 0) 
	fprintf(stdout, "Symbol: %s\n", ST_name (st));
      sh = CXX_NEW (SHACKLE_INFO (st, func_nd,
				  &shackle_default_pool,
				  type, FALSE),
		    &shackle_default_pool);
      si->Add_Tail_Q (sh);
    }
  }
  FOREACH_SYMBOL (GLOBAL_SYMTAB, st, i) {
    if (CLASS_UNK == ST_sym_class (st) ||
	CLASS_BLOCK == ST_sym_class (st) ||
	CLASS_COUNT == ST_sym_class (st)) 
      continue;
    ty = Shackle_Is_Array_Type (ST_type (st));
    if (ty != TY_IDX_ZERO) {
      if (shackle_debug_level > 0) 
	fprintf(stdout, "Symbol: %s\n", ST_name (st));
      sh = CXX_NEW (SHACKLE_INFO (st, func_nd,
				  &shackle_default_pool,
				  type, FALSE),
		    &shackle_default_pool);
      si->Add_Tail_Q (sh);
    }
  }
  return si;
}
#endif

SHACKLE_INFO *
Shackle_Info_For_Symbol(QUEUE<SHACKLE_INFO *> *shq,
		       const ST              *st)
{
  QUEUE_ITER<SHACKLE_INFO*>  iter(shq);
  SHACKLE_INFO              *sh;

  while (iter.Step(&sh)) {
    if (sh->Symbol() == st)
      return sh;
  }
  return NULL;
}

static void
Extend_Dep_Vectors(WN *top_level, INT32 extend_count)
{
  // Every depvector in the system has to be extended by count
  // additional loops (if the src and dest are contained within
  // top level
  assert (0 != extend_count);
  extern ARRAY_DIRECTED_GRAPH16 *Array_Dependence_Graph;
  EINDEX16 e;
  ARRAY_DIRECTED_GRAPH16 *dg = Array_Dependence_Graph;
  VINDEX16 src, dst;

  e = dg->Get_Edge();
  while (e) {
    // Only if the edge is completely in current SNL..
    src = dg->Get_Source(e);
    dst = dg->Get_Sink(e);
    if (Is_Parent_Of (top_level, dg->Get_Wn (src)) &&
	Is_Parent_Of (top_level, dg->Get_Wn (dst))) {
      DEPV_ARRAY *d = dg->Depv_Array (e);
      INT32 num_vec = d->Num_Vec();
      INT32 num_dim = d->Num_Dim();
      INT32 num_unused_dim = d->Num_Unused_Dim();
      DEPV_ARRAY *newd = 
	Create_DEPV_ARRAY (num_vec, num_dim + extend_count,
			   num_unused_dim,
			   dg->Pool());
      for (INT32 j = 0; j < d->Num_Vec(); j++) {
	DEPV *newv = newd->Depv (j);
	DEPV *oldv = d->Depv (j);
        INT32 i;
	for (i = 0; i < extend_count; i++)
	  DEPV_Dep (newv, i) = DEP_SetDistance (0);
	for (i = 0; i < d->Num_Dim(); i++)
	  DEPV_Dep (newv, i + extend_count) = DEPV_Dep (oldv, i);
      }
      Delete_DEPV_ARRAY (d, dg->Pool());
      dg->Set_Depv_Array (e, newd);
    }
    e = dg->Get_Next_Edge (e);
  }
}    
  
static WN**
Create_Simple_Shackle_Loops(WN                    *main_body,
			    QUEUE<WN *>           *stmts,
			    QUEUE<SHACKLE_INFO*>  *shackle_info,
			    INT32                  shackle_depth)
{
  QUEUE<WN *>   *shackle_list;
  WN            *stmt = stmts->Queue_First()->Qnode_Item();
  SHACKLE_INFO  *sh;
  WN            *top_level;
  OPCODE         opc;
  WN            *index_var, *start_exp, *end_exp, *do_step;
  WN            *end_num_exp, *end_denom_exp;
  TYPE_ID        index_type, rtype;
  WN            *fake_unroll[2];
  WN            *current_do, *prev_do = NULL;
  WN            *if_to_insert, *prev_if_to_insert;
  WN            *another_do;
  DEF_LIST      *dl;
  INT32          added_loop_count = 0;
  WN            *outermost_do;
  INT64          linenum = WN_Get_Linenum (main_body);

  another_do = Find_Do_Loop (main_body);
  assert (NULL != another_do);
  TY_IDX            high_level_type = WN_ty (WN_start (another_do));
  WN            *dummy_do_load_op = NULL;

  WN            *parent_block = LWN_Get_Parent (main_body);
  shackle_list = (QUEUE<WN *> *) WN_MAP_Get (shackle_shackle_map,
					     stmt);
  top_level = main_body;
  if (NULL == top_level)
    return NULL;
  QUEUE_ITER<WN *>   iter (shackle_list);
  WN                *step;
  WN               **ret_val = CXX_NEW_ARRAY (WN *, shackle_depth,
					      &shackle_default_pool);

  while (iter.Step(&step)) {
    sh = Shackle_Info_For_Symbol (shackle_info, 
				 Identical_Array_Refbase (step,
							    step));
    index_type = sh->Loop_Type();
    OPCODE load_opc = 
      OPCODE_make_op (OPR_LDID, Promote_Type (index_type),
		      index_type);
    OPCODE store_opc = 
      OPCODE_make_op (OPR_STID, MTYPE_V, Promote_Type (index_type));
    for (INT32 i = 0; i < sh->Ndim(); i++) {
      if (sh->Is_Dim_Shackled (i)) {
	WN *do_index = sh->Loop ();
	dummy_do_load_op =     
	  WN_CreateLdid (load_opc, WN_offset (do_index),
			 WN_st (do_index), high_level_type);

	// We'll create shackle loops with unit step..
	WN *step_size = WN_CreateIntconst 
	  (OPCODE_make_op (OPR_INTCONST, 
			   Promote_Type (index_type), MTYPE_V),
	   (INT64) 1);
	start_exp = WN_CreateIntconst
	  (OPCODE_make_op (OPR_INTCONST,
			   Promote_Type (index_type), MTYPE_V),
	   (INT64) 0);
	if (sh->Is_Const_Upper (i)) 
	  end_num_exp = WN_CreateIntconst 
	    (OPCODE_make_op (OPR_INTCONST, 
			     Promote_Type (index_type), MTYPE_V),
	     (INT64) sh->Const_Upper (i));
	else {
	  end_num_exp = sh->Expr_Upper (i);
	}
	WN *dup_lbnd;
	if (sh->Is_Const_Lower (i)) 
	  dup_lbnd = WN_CreateIntconst 
	    (OPCODE_make_op (OPR_INTCONST,
			     Promote_Type (index_type), MTYPE_V),
	     (INT64) sh->Const_Lower(i));
	else {
	  dup_lbnd = sh->Expr_Lower (i);
	}
	end_num_exp = LWN_CreateExp2 
	  (OPCODE_make_op (OPR_SUB, Promote_Type (index_type), 
			   MTYPE_V),
	   end_num_exp, dup_lbnd);
	FmtAssert (sh->Shackle_Dim_Size (i) > 0,
		   ("Size of shackle for this dimension is negative?"));
	end_denom_exp = WN_CreateIntconst 
	  (OPCODE_make_op (OPR_INTCONST,
			   Promote_Type (index_type), MTYPE_V),
	   (INT64) sh->Shackle_Dim_Size (i));
	end_exp = Sh_LWN_CreateDivfloor (Promote_Type (index_type),
					 end_num_exp,
					 end_denom_exp);
	// Now, take care of each of the pieces
	// First, the lower bound:
	WN *do_lbnd = 
	  LWN_CreateStid (store_opc, dummy_do_load_op, start_exp);
	// Nest, the step
	WN *step_kid_rhs = LWN_CreateLdid (load_opc, dummy_do_load_op);
	Du_Mgr->Add_Def_Use (do_lbnd, step_kid_rhs);
	WN *step_kid = 
	  LWN_CreateExp2 
	  (OPCODE_make_op (OPR_ADD, Promote_Type (index_type),
			   MTYPE_V),
	   step_size,
	   step_kid_rhs);
	do_step = LWN_CreateStid 
	  (store_opc, dummy_do_load_op, step_kid);
	Du_Mgr->Add_Def_Use (do_step, step_kid_rhs);
	// Next, upper bound
	rtype = OPCODE_rtype (WN_opcode (WN_end (another_do)));
	WN *do_ubnd_rhs = LWN_CreateLdid (load_opc, dummy_do_load_op);
	Du_Mgr->Add_Def_Use (do_step, do_ubnd_rhs);
	Du_Mgr->Add_Def_Use (do_lbnd, do_ubnd_rhs);
	WN *do_ubnd = LWN_CreateExp2 
	  (OPCODE_make_op (OPR_LE, rtype, 
			   Promote_Type (index_type)),
	   do_ubnd_rhs,
	   end_exp);
	current_do = 
	  LWN_CreateDO (do_index, do_lbnd, do_ubnd, do_step, 
			WN_CreateBlock());
	ret_val[added_loop_count] = current_do;
	added_loop_count++;
	dl = Du_Mgr->Ud_Get_Def (step_kid_rhs);
	dl->Set_loop_stmt (current_do);
	dl = Du_Mgr->Ud_Get_Def (do_ubnd_rhs);
	dl->Set_loop_stmt (current_do);
	DO_LOOP_INFO *dli = CXX_NEW (DO_LOOP_INFO (&LNO_default_pool,
						   NULL, NULL, NULL,
						   FALSE, FALSE, FALSE, FALSE, FALSE,
						   FALSE, FALSE, FALSE),
				     &LNO_default_pool);
	Set_Do_Loop_Info (current_do, dli);
	sh->Set_Loop_Stmt (i, current_do);
	WN_Set_Linenum (current_do, linenum);

	WN *fake_unroll[2];
	WN *if_cond_lhs = LWN_Copy_Tree (start_exp);
	fake_unroll[0] = start_exp;
	fake_unroll[1] = if_cond_lhs;
	Unrolled_DU_Update (fake_unroll, 2, 0);
	WN *if_cond_rhs = LWN_Copy_Tree (end_num_exp);
	fake_unroll[0] = end_num_exp;
	fake_unroll[1] = if_cond_rhs;
	Unrolled_DU_Update (fake_unroll, 2, 0);
	WN *if_cond_to_insert = LWN_CreateExp2 
	  (OPCODE_make_op (OPR_LE, rtype, Promote_Type (index_type)),
	   if_cond_lhs, if_cond_rhs);
	if_cond_to_insert = 
	  Simplify_If_Conditional (if_cond_to_insert);
	WN *if_to_insert = LWN_CreateIf (if_cond_to_insert,
					 WN_CreateBlock(),
					 WN_CreateBlock());
	IF_INFO *ii = 
	  CXX_NEW (IF_INFO (&LNO_default_pool, TRUE, FALSE),
		   &LNO_default_pool);
	WN_MAP_Set (LNO_Info_Map, if_to_insert, (void *)ii);
	LWN_Insert_Block_After (WN_do_body (current_do), NULL,
				if_to_insert);
	if (prev_do) {
	  WN *move_stmt;
	  FmtAssert (prev_if_to_insert ==
		     WN_first (WN_do_body (prev_do)),
		     ("Mismatch in previous inserted pair"));
	  FOR_CHILDREN (WN_then (prev_if_to_insert), 
			child, ignCount) {
	    move_stmt = LWN_Extract_From_Block (child);
	    LWN_Insert_Block_Before(WN_then (if_to_insert),
				    NULL, move_stmt);
	  }
	  END_CHILDREN;
	  LWN_Insert_Block_Before (WN_then (prev_if_to_insert),
				   NULL, current_do);
	}
	else {
	  outermost_do = current_do;
	  WN *move_stmt;
	  Replace_WN (top_level, current_do);
	  LWN_Insert_Block_Before (WN_then (if_to_insert),
				   NULL, top_level);
	}
	prev_do = current_do;
	prev_if_to_insert = if_to_insert;
	LWN_Delete_Tree (dummy_do_load_op);
      } 
    }
  }
  if (NULL != prev_do) {
    Reset_Do_Loop_Depths (outermost_do, 0);
    LWN_Parentize (parent_block);
    LNO_Build_Access (parent_block, &LNO_default_pool);
    Extend_Dep_Vectors(outermost_do, added_loop_count);
  }
  assert (added_loop_count == shackle_depth);
  return ret_val;
}

static void
Create_Shackle_If_Per_Stmt (WN                   *stmt, 
			    QUEUE<SHACKLE_INFO*> *shackle_info,
			    WN **shackle_loop_info,
			    INT32 shackling_depth)
{
  QUEUE<WN*>         *shackle_list = 
    (QUEUE<WN *> *) WN_MAP_Get (shackle_shackle_map, stmt);
  INT32               stmt_depth = 
    Num_Common_Loops (stmt, stmt);
  WN                 *step;
  QUEUE_ITER<WN *>    iter (shackle_list);
  DOLOOP_STACK        do_stack(&shackle_default_pool);
  INT32 added_loop_count = 0;

  Build_Doloop_Stack (stmt, &do_stack);
  while (iter.Step (&step)) {
    SHACKLE_INFO *sh = 
      Shackle_Info_For_Symbol (shackle_info, 
			      Identical_Array_Refbase (step, step));
    assert (sh->Ndim() == (WN_kid_count (step) >> 1));
    for (INT32 i = 0; i < sh->Ndim(); i++) {
      if (sh->Is_Dim_Shackled (i)) {
	WN *shackle_do = shackle_loop_info[added_loop_count];
	added_loop_count++;
	TYPE_ID index_type = sh->Loop_Type ();
	OPCODE load_opc = 
	  OPCODE_make_op (OPR_LDID, Promote_Type (index_type), 
			  index_type);
	WN *shackle_loop_var[2];
	WN *data_centric_ref[2];
	WN *fake_unroll[2];
        INT32 k;
	for (k = 0; k < 2; k++) 
	  shackle_loop_var[k] = LWN_CreateLdid (load_opc, 
						WN_start (shackle_do));
	for (k = 0; k < 2; k++) 
	  data_centric_ref[k] = 
	    LWN_Copy_Tree (WN_kid (step, sh->Ndim() + i + 1));
	fake_unroll[0] = WN_kid (step, sh->Ndim() + i + 1);
	for (k = 0; k < 2; k++) {
	  fake_unroll[1] = data_centric_ref[k];
	  Unrolled_DU_Update (fake_unroll, 2, stmt_depth);
	}
	// Add def-use info for shackle loop var
	for (k = 0; k < 2; k++) {
	  Du_Mgr->Add_Def_Use (WN_start (shackle_do), shackle_loop_var[k]);
	  Du_Mgr->Add_Def_Use (WN_step (shackle_do), shackle_loop_var[k]);
	  DEF_LIST *dl = Du_Mgr->Ud_Get_Def (shackle_loop_var[k]);
	  dl->Set_loop_stmt (shackle_do);
	}
	{
	  // add 1 to shackle_loop_var[1]
	  WN *add_one = WN_CreateIntconst
	    (OPCODE_make_op (OPR_INTCONST,
			     Promote_Type (index_type), MTYPE_V),
	     (INT64) 1);
	  shackle_loop_var[1] = WN_CreateExp2
	    (OPCODE_make_op (OPR_ADD, Promote_Type (index_type),
			     MTYPE_V),
	     add_one, shackle_loop_var[1]);
	}
	// Multiply both loop vars by shackle_dim_size
	for (k = 0; k < 2; k++) {
	  WN *shackle_mul = WN_CreateIntconst 
	    (OPCODE_make_op (OPR_INTCONST, 
			     Promote_Type (index_type), MTYPE_V),
	     (INT64) sh->Shackle_Dim_Size (i));
	  shackle_loop_var[k] = WN_CreateExp2
	    (OPCODE_make_op (OPR_MPY, 
			     Promote_Type (index_type), MTYPE_V),
	     shackle_loop_var[k], shackle_mul);
	}
	// We have to insert 2 if statements..
	TYPE_ID rtype = OPCODE_rtype (WN_opcode (WN_end (shackle_do)));
	WN *if_cond0 = LWN_CreateExp2 
	  (OPCODE_make_op (OPR_LE, rtype, Promote_Type (index_type)),
	   shackle_loop_var[0], data_centric_ref[0]);
	WN *if_stmt0 = LWN_CreateIf (if_cond0, 
				     WN_CreateBlock(), 
				     WN_CreateBlock());
	Replace_WN (stmt, if_stmt0);
	LWN_Insert_Block_After (WN_then (if_stmt0), NULL, stmt);
	WN *if_cond1 = LWN_CreateExp2
	  (OPCODE_make_op (OPR_GT, rtype, Promote_Type (index_type)),
	   shackle_loop_var[1], data_centric_ref[1]);
	WN *if_stmt1 = LWN_CreateIf (if_cond1, 
				     WN_CreateBlock(),
				     WN_CreateBlock());
	Replace_WN (if_stmt0, if_stmt1);
	LWN_Insert_Block_After (WN_then (if_stmt1), NULL, if_stmt0);
	IF_INFO *ii = 
	  CXX_NEW (IF_INFO (&LNO_default_pool, FALSE, FALSE), 
		   &LNO_default_pool);
	WN_MAP_Set (LNO_Info_Map, if_stmt0, (void *) ii);
	LNO_Build_If_Access (if_stmt0, &do_stack);
	ii = 
	  CXX_NEW (IF_INFO (&LNO_default_pool, FALSE, FALSE), 
		   &LNO_default_pool);
	WN_MAP_Set (LNO_Info_Map, if_stmt1, (void *) ii);
	LNO_Build_If_Access (if_stmt1, &do_stack);
      }
    }
  }
  assert (added_loop_count == shackling_depth);
}

static void
Create_Shackle_If_All_Stmts(WN *func_nd,
			    QUEUE<WN *> *stmts,
			    QUEUE<SHACKLE_INFO *> *shackle_info,
			    WN **shackle_loop_info,
			    INT32 shackle_depth)
{
  QUEUE_ITER<WN *>  iter(stmts);
  WN               *step;

  while (iter.Step (&step)) 
    Create_Shackle_If_Per_Stmt (step, shackle_info, 
				shackle_loop_info,
				shackle_depth);
  LWN_Parentize (func_nd);
}

static INT32
shackling_depth(QUEUE<WN *> *shackle,
		QUEUE<SHACKLE_INFO *> *shq)
{
  // Given a composite shackle, find the total number of naive loops
  // we need to run over it.
  INT32                      i, count = 0;
  QUEUE_ITER<WN *>           iter(shackle);
  WN                        *wn;
  SHACKLE_INFO              *sh;
  ST                        *st;

  while (iter.Step (&wn)) {
    st = Identical_Array_Refbase (wn, wn);
    assert (NULL != st);
    sh = Shackle_Info_For_Symbol (shq, st);
    FmtAssert (NULL != sh, ("Shackling info cannot be NULL"));
    count += sh->Ndim_Shackled();
  }
  return count;
}

static INT32
Common_Shackling_Depth(QUEUE<WN *>           *stmts,
		       QUEUE<SHACKLE_INFO *> *shq)
{
  INT32             count, retval;
  QUEUE_ITER<WN *>  iter(stmts);
  WN               *step;

  count = 0;
  while (iter.Step(&step)) {
    QUEUE<WN *> *shackle = 
      (QUEUE<WN *> *) WN_MAP_Get (shackle_shackle_map, step);
    retval = shackling_depth (shackle, shq);
    if (0 == count)
      count = retval;
    assert (count == retval);
  }
  return count;
}

static BOOL
_xdependence_is_preserved(WN                   *stmt1, 
			  WN                   *stmt2,
			  WN                   *ref1,  
			  WN                   *ref2,
			  BOOL                  stmt1_before_stmt2,
			  QUEUE<SHACKLE_INFO*> *shq)
{
  /* Given ref1 and ref2 from statements 1 and 2 respectively, 
     we wish to determine if the dependence expressed between
     those two references is indeed preserved by the present 
     proposed shackling. shackle_shackle_map is used to store
     the present data-centric references for stmts 1,2. We need to 
     combine all this information into one big disjunct that we need to 
     test. The assumption is that in the initial code, there was a 
     dependence from ref2 (stmt2) to ref1 (stmt1). So, ref2 at least 
     ought to be a write, ref1 can be either a read or a write. So, 
     we proceed as follows: Encode the condition that stmt1 instance
     was executed before stmt2. This condition is different based
     on whether stmt1 is before or after stmt2 in the program tree.
     Next, assume that it is violated by the shackling order. This
     evaluates to another disjunct. Both these disjuncts must
     be simultaneously valid, along with other trivialities like
     they write to the same location for the shackle to violate
     this particular dependence */
  WN                        *wn1, *wn2, *stmt;
  ST                        *st;
  TY_IDX                    ty;
  QUEUE<WN *>               *shackle1, *shackle2;
  ACCESS_ARRAY              *ar1, *ar2, *lb, *ub;
  ACCESS_VECTOR             *v1, *v2;
  INT32                      shackle_depth, i, j, k;
  INT32                      nvars, depth1, depth2, count;
  INT32                      common_depth;
  mINT32                    *row;
  SYSTEM_OF_EQUATIONS      **soe_lex_init, **soe_lex_final;
  SYSTEM_OF_EQUATIONS       *soe_same_loc, *soe_data_shackle;
  SYSTEM_OF_EQUATIONS       *soe_loop_bnds1, *soe_loop_bnds2;
  QUEUE_ITER<SHACKLE_INFO*> *iter;
  QUEUE_ITER<WN*>           *wn_iter1, *wn_iter2;
  SHACKLE_INFO              *sh;
  INT32                      stmt_rhs_offset;
  LINEAR_CLAUSE             *l1, *l2, *l3, *l4, *l5, *l6;
  LINEAR_CLAUSE             *l11, *l12, *l13;
  DO_LOOP_INFO              *dli;
  BOOL                       return_val;
  
  st = Identical_Array_Refbase (ref1, ref2);
  if (NULL == st) {
    // Certainly not identical arrays, so dependences are
    // automatically preserved 
    return TRUE;
  }
  MEM_POOL_Push (&shackle_default_pool);
  assert (NULL != st);
  ty = Shackle_Is_Array_Type (ST_type (st));
  if (ty == TY_IDX_ZERO) {
    MEM_POOL_Pop (&shackle_default_pool);
    // Can't really say anything, probably a pointer - be conservative
    return FALSE;
  } 
  assert (ty != TY_IDX_ZERO);
  shackle1 = (QUEUE<WN *> *) WN_MAP_Get (shackle_shackle_map,
					 stmt1);
  shackle2 = (QUEUE<WN *> *) WN_MAP_Get (shackle_shackle_map,
					 stmt2);
  ar1 = (ACCESS_ARRAY *) WN_MAP_Get (LNO_Info_Map, ref1);
  ar2 = (ACCESS_ARRAY *) WN_MAP_Get (LNO_Info_Map, ref2);
  assert (shackling_depth (shackle1, shq) == 
	  shackling_depth (shackle2, shq));
  shackle_depth = shackling_depth (shackle1, shq);
  depth1 = Num_Common_Loops (stmt1, stmt1);
  depth2 = Num_Common_Loops (stmt2, stmt2);
  nvars = depth1 + depth2 + 2 * shackle_depth;
  row = CXX_NEW_ARRAY (mINT32, nvars, &shackle_default_pool);
  for (i = 0; i < nvars; i++) 
    row[i] = 0;
  // First, form the constraints for the accessed data to be the same
  // One or both of the references got delinearized differently
  if (ar1->Num_Vec() != ar2->Num_Vec()) {
    MEM_POOL_Pop (&shackle_default_pool);
    return FALSE;
  } 
  FmtAssert (ar1->Num_Vec() == ar2->Num_Vec(), 
	     ("Number of dimensions in references must be same!"));
  soe_same_loc = 
    CXX_NEW (SYSTEM_OF_EQUATIONS (0, 0, nvars, &shackle_default_pool),
	     &shackle_default_pool);
  for (i = 0; i < ar1->Num_Vec(); i++) {
    for (j = 0; j < depth1 + depth2; j++) 
      row[j] = 0;
    v1 = ar1->Dim(i);
    v2 = ar2->Dim(i);
    assert (v1->Nest_Depth() == depth1);
    assert (v2->Nest_Depth() == depth2);
    for (j = 0; j < depth1; j++) {
      row[j] = v1->Loop_Coeff (j);
    }
    for (j = 0; j < depth2; j++) {
      row[j+depth1] = - v2->Loop_Coeff (j);
    }
    soe_same_loc->Add_Eq (row, v2->Const_Offset - v1->Const_Offset);
  }
  // Next, form constraints for shackling constraints.
  for (i = 0; i < nvars; i++)
    row[i] = 0;
  
  assert (shackle1->Queue_Length() == shackle2->Queue_Length());
  wn_iter1 = CXX_NEW (QUEUE_ITER<WN *> (shackle1), 
		      &shackle_default_pool);
  soe_data_shackle = 
    CXX_NEW (SYSTEM_OF_EQUATIONS (0, 0, nvars, &shackle_default_pool),
	     &shackle_default_pool);
  // Count points to the index of the outermost shackle loop
  count = depth1 + depth2;
  while (wn_iter1->Step(&wn1)) {
    // wn_iter1->Step(&wn1);
    // A given shackle has as many dimensions as the array being 
    // shackled.
    sh = Shackle_Info_For_Symbol (shq, 
				 Identical_Array_Refbase (wn1,
							    wn1));
    assert (NULL != sh);
    ar1 = (ACCESS_ARRAY *) WN_MAP_Get (LNO_Info_Map, wn1);
    // At this moment, we don't know how to handle delinearized
    // references, so we just become conservative.
    if (ar1->Num_Vec() != sh->Ndim()) {
      MEM_POOL_Pop (&shackle_default_pool);
      return FALSE;
    } 
    assert (ar1->Num_Vec() == sh->Ndim());
    for (i = 0; i < sh->Ndim(); i++) {
      if (sh->Is_Dim_Shackled(i)) {
	v1 = ar1->Dim(i);
	// assert (v1->Nest_Depth() == depth1);
	for (j = 0; j < depth1; j++) {
	  row[j] =  - v1->Loop_Coeff (j);
	}
	row[count] = sh->Shackle_Dim_Size (i);
	soe_data_shackle->Add_Le (row, v1->Const_Offset + 
				  sh->Shackle_Dim_Size (i));
	for (j = 0; j < depth1; j++) {
	  row[j] = - row[j];
	}
	row[count] = - row[count];
	soe_data_shackle->Add_Le (row, -1 - v1->Const_Offset);
	row[count] = 0;
	count++;
      }
    }
  }
  for (i = 0; i < depth1; i++) 
    row[i] = 0;
  assert ((count + shackle_depth) == nvars);
  wn_iter2 = CXX_NEW (QUEUE_ITER<WN *> (shackle2),
		      &shackle_default_pool);
  while (wn_iter2->Step (&wn2)) {
    // wn_iter2->Step (&wn2);
    // A given shackle has as many dimensions as the 
    // array being blocked
    sh = Shackle_Info_For_Symbol (shq, 
				 Identical_Array_Refbase (wn2,
							    wn2));
    assert (NULL != sh);
    ar2 = (ACCESS_ARRAY *) WN_MAP_Get (LNO_Info_Map, wn2);
    // At this moment, we don't know how to handle delinearized
    // references, so we just become conservative.
    if (ar2->Num_Vec() != sh->Ndim()) {
      MEM_POOL_Pop (&shackle_default_pool);
      return FALSE;
    } 
    assert (ar2->Num_Vec() == sh->Ndim());
    for (i = 0; i < sh->Ndim(); i++) {
      if (sh->Is_Dim_Shackled(i)) {
	v2 = ar2->Dim (i);
	// assert (v2->Nest_Depth() == depth2);
	for (j = 0; j < depth2; j++) {
	  row[j+depth1] = - v2->Loop_Coeff (j);
	}
	row[count] = sh->Shackle_Dim_Size (i);
	soe_data_shackle->Add_Le (row, v2->Const_Offset + 
				  sh->Shackle_Dim_Size (i));
	for (j = 0; j < depth2; j++) {
	  row[j+depth1] = - row[j+depth1];
	}
	row[count] = - row[count];
	soe_data_shackle->Add_Le (row, -1 - v2->Const_Offset);
	row[count] = 0;
	count++;
      }
    }
  }
  assert (count == nvars);
  if (shackle_debug_level > 0) {
    printf("First statement nested in %d loops\n", depth1);
    printf("Secnd statement nested in %d loops\n", depth2);
    printf("Shackle depth is %d\n", shackle_depth);
    printf("Total number of variables is %d\n", nvars);
    printf("Conditions expressing smae location:");
    soe_same_loc->Print(stdout);
    printf("Conditions expressing shackling:");
    soe_data_shackle->Print(stdout);
  }
  /* Next, put in the conditions about the read iteration being 
     after the written iteration in the initial loop */
  stmt_rhs_offset = (stmt1_before_stmt2) ? -1 : 0;
  common_depth = Num_Common_Loops (stmt1, stmt2);
  for (i = 0; i < nvars; i++) 
    row[i] = 0;
  soe_lex_init = CXX_NEW_ARRAY (SYSTEM_OF_EQUATIONS *, 
				common_depth, 
				&shackle_default_pool);
  for (i = 0; i < common_depth; i++) {
    soe_lex_init[i] = 
      CXX_NEW (SYSTEM_OF_EQUATIONS (0, 0, nvars, 
				    &shackle_default_pool),
	       &shackle_default_pool);
  }
  for (i = 0; i < common_depth; i++) {
    for (j = 0; j < i; j++) {
      row[j] = 1;
      row[j+depth1] = -1;
      soe_lex_init[i]->Add_Eq (row, 0);
      row[j] = 0;
      row[j+depth1] = 0;
    }
    row[i] = -1;
    row[i+depth1] = 1;
    soe_lex_init[i]->Add_Le (row, stmt_rhs_offset);
    row[i] = 0;
    row[i+depth1] = 0;
  }
  l1 = CXX_NEW (LINEAR_CLAUSE (soe_lex_init, common_depth,
			       &shackle_default_pool),
		&shackle_default_pool);
  if (shackle_debug_level > 0) {
    fprintf(stdout, "We'll now dump the initial lex conds\n");
    l1->Print(stdout);
  }
  soe_lex_final = CXX_NEW_ARRAY (SYSTEM_OF_EQUATIONS *,
				 shackle_depth,
				 &shackle_default_pool);
  for (i = 0; i < shackle_depth; i++) {
    soe_lex_final[i] = 
      CXX_NEW (SYSTEM_OF_EQUATIONS (0, 0, nvars,
				    &shackle_default_pool),
	       &shackle_default_pool);
  }
  /* Next, formulat that the write iteration is executed in a 
     block strictly after the read iterations. This is easy */
  for (i = 0; i < shackle_depth; i++) {
    for (j = 0; j < i; j++) {
      row[j+depth1+depth2] = 1;
      row[j+depth1+depth2+shackle_depth] = -1;
      soe_lex_final[i]->Add_Eq (row, 0);
      row[j+depth1+depth2] = 0;
      row[j+depth1+depth2+shackle_depth] = 0;
    }
    row[i+depth1+depth2] = 1;
    row[i+depth1+depth2+shackle_depth] = -1;
    soe_lex_final[i]->Add_Le (row, -1);
    row[i+depth1+depth2] = 0;
    row[i+depth1+depth2+shackle_depth] = 0;
  }
  l2 = CXX_NEW (LINEAR_CLAUSE (soe_lex_final, shackle_depth,
			       &shackle_default_pool),
		&shackle_default_pool);
  if (shackle_debug_level > 0) {
    fprintf(stdout, "We'll now dump the Final lex conds\n");
    l2->Print(stdout);
  }
  l3 = CXX_NEW (LINEAR_CLAUSE (soe_same_loc, &shackle_default_pool),
		&shackle_default_pool);
  l4 = CXX_NEW (LINEAR_CLAUSE (soe_data_shackle, 
			       &shackle_default_pool),
		&shackle_default_pool);
  // Next, add loop bounds for stmt1
  soe_loop_bnds1 = 
    CXX_NEW (SYSTEM_OF_EQUATIONS (0, 0, nvars, &shackle_default_pool),
	     &shackle_default_pool);
  count = depth1;
  stmt = stmt1;
  while (stmt != NULL) {
    // If we have a do loop 
    if (OPC_DO_LOOP == WN_opcode (stmt)) {
      dli = Get_Do_Loop_Info (stmt);
      assert (NULL != dli);
      lb = dli->LB;
      for (i = 0; i < lb->Num_Vec(); i++) {
	v1 = lb->Dim(i);
	// If v1 has linear or nonlinear symbols in its bounds,
	// we are screwed. We can't really add this into constraints
	if ((!v1->Contains_Lin_Symb()) &&
	    (!v1->Contains_Non_Lin_Symb())) {
	  for (j = 0; j < count; j++) {
	    row[j] = v1->Loop_Coeff (j);
	  }
	  soe_loop_bnds1->Add_Le (row, v1->Const_Offset);
	}
      }
      count--;
      row[count] = 0;
    }
    stmt = LWN_Get_Parent (stmt);
  }
  assert (0 == count);
  count = depth1;
  stmt = stmt1;
  while (stmt != NULL) {
    // If we have a do loop 
    if (OPC_DO_LOOP == WN_opcode (stmt)) {
      dli = Get_Do_Loop_Info (stmt);
      assert (NULL != dli);
      ub = dli->UB;
      for (i = 0; i < ub->Num_Vec(); i++) {
	v1 = ub->Dim(i);
	// If v1 has linear or nonlinear symbols in its bounds,
	// we are screwed. We can't really add this into constraints
	if ((!v1->Contains_Lin_Symb()) &&
	    (!v1->Contains_Non_Lin_Symb())) {
	  for (j = 0; j < count; j++) {
	    row[j] = v1->Loop_Coeff (j);
	  }
	  soe_loop_bnds1->Add_Le (row, v1->Const_Offset);
	}
      }
      count--;
      row[count] = 0;
    }
    stmt = LWN_Get_Parent (stmt);
  }
  assert (0 == count);
  // Next, add bounds for statement 2
  soe_loop_bnds2 = 
    CXX_NEW (SYSTEM_OF_EQUATIONS (0, 0, nvars, &shackle_default_pool),
	     &shackle_default_pool);
  count = depth2;
  stmt = stmt2;
  while (stmt != NULL) {
    // If we have a do loop 
    if (OPC_DO_LOOP == WN_opcode (stmt)) {
      dli = Get_Do_Loop_Info (stmt);
      assert (NULL != dli);
      lb = dli->LB;
      for (i = 0; i < lb->Num_Vec(); i++) {
	v2 = lb->Dim(i);
	// If v2 has linear or nonlinear symbols in its bounds,
	// we are screwed. We can't really add this into constraints
	if ((!v2->Contains_Lin_Symb()) &&
	    (!v2->Contains_Non_Lin_Symb())) {
	  for (j = 0; j < count; j++) {
	    row[j+depth1] = v2->Loop_Coeff (j);
	  }
	  soe_loop_bnds2->Add_Le (row, v2->Const_Offset);
	}
      }
      count--;
      row[count+depth1] = 0;
    }
    stmt = LWN_Get_Parent (stmt);
  }
  assert (0 == count);
  count = depth2;
  stmt = stmt2;
  while (stmt != NULL) {
    // If we have a do loop 
    if (OPC_DO_LOOP == WN_opcode (stmt)) {
      dli = Get_Do_Loop_Info (stmt);
      assert (NULL != dli);
      ub = dli->UB;
      for (i = 0; i < ub->Num_Vec(); i++) {
	v2 = ub->Dim(i);
	// If v2 has linear or nonlinear symbols in its bounds,
	// we are screwed. We can't really add this into constraints
	if ((!v2->Contains_Lin_Symb()) &&
	    (!v2->Contains_Non_Lin_Symb())) {
	  for (j = 0; j < count; j++) {
	    row[j+depth1] = v2->Loop_Coeff (j);
	  }
	  soe_loop_bnds2->Add_Le (row, v2->Const_Offset);
	}
      }
      count--;
      row[count+depth1] = 0;
    }
    stmt = LWN_Get_Parent (stmt);
  }
  assert (0 == count);
  if (shackle_debug_level > 0) {
    printf("Printing bounds for statement 1\n");
    soe_loop_bnds1->Print(stdout);
    printf("Print bounds for statement 2 \n");
    soe_loop_bnds2->Print(stdout);
  }
  l5 = CXX_NEW (LINEAR_CLAUSE (soe_loop_bnds1, 
			       &shackle_default_pool),
		&shackle_default_pool);
  l6 = CXX_NEW (LINEAR_CLAUSE (soe_loop_bnds2,
			       &shackle_default_pool),
		&shackle_default_pool);
  l11 = combine_clauses (combine_clauses (l3, l4),
			combine_clauses (l5, l6));
  l12 = combine_clauses (l1, l2);
  l13 = combine_clauses (l11, l12);
  if (shackle_debug_level > 1) {
    fprintf(stdout, "Here is the big final mess\n");
    l13->Print (stdout);
  }
  return_val = !l13->Is_Consistent();
  MEM_POOL_Pop (&shackle_default_pool);
  return return_val;
}

static BOOL
_xis_legal_shackle(QUEUE<WN *>          *stmts,
		   QUEUE<SHACKLE_INFO*> *shq)
{
  SYMBOL           *newsyms;
  const INT32       bufsz = 128;
  char              buf[bufsz];
  INT32             bufcnt, i;
  INT32             length = stmts->Queue_Length();
  QUEUE_ITER<WN *>  iter1(stmts);
  QUEUE_ITER<WN *> *iter2;
  QUEUE_ITER<WN *> *iter_ref;
  WN               *step1, *step2, *ref, *ref2;
  BOOL              read_stmt_prior, result;
  QUEUE<WN *>      *reflist1, *reflist2;
  
  while (iter1.Step(&step1)) {
    reflist1 = (QUEUE<WN *> *) WN_MAP_Get (shackle_ref_map,
					   step1);
    iter_ref = CXX_NEW (QUEUE_ITER<WN *> (reflist1),
			&shackle_default_pool);
    while (iter_ref->Step(&ref)) {
      iter2 = CXX_NEW (QUEUE_ITER<WN *> (stmts),
		       &shackle_default_pool);
      read_stmt_prior = FALSE;
      while (iter2->Step(&step2)) {
	if (step2 == step1) 
	  read_stmt_prior = TRUE;
	reflist2 = (QUEUE<WN *> *) WN_MAP_Get (shackle_ref_map,
					       step2);
	if (reflist2->Queue_Isempty())
	  continue;
	ref2 = reflist2->Queue_First()->Qnode_Item();
	result = _xdependence_is_preserved (step1, step2, ref, ref2,
					    read_stmt_prior, shq);
	if ((result) && (shackle_debug_level > 1))
	  fprintf (stdout, "Dependence is preserved\n");
	else if (shackle_debug_level > 1)
	  fprintf (stdout, "Dependence is violated- arrgh\n");
	if (FALSE == result)
	  return FALSE;
      }
    }
  }
  return TRUE;
}

static BOOL
Contained_In_Loop(WN *wn)
{
  WN *step = LWN_Get_Parent (wn);
  while (step) {
    if (OPC_DO_LOOP == WN_opcode (step)) 
      return TRUE;
    step = LWN_Get_Parent (step);
  }
  return FALSE;
}

static void
_xtest_dependence_is_preserved(QUEUE<WN *>          *stmts,
			       QUEUE<SHACKLE_INFO*> *shq)
{
  WN  *stmt1, *stmt2;
  WN  *ref1,  *ref2;
  QUEUE<WN *> *shackle_list1, *shackle_list2;
  BOOL result;

  stmt1 = stmts->Queue_First()->Qnode_Item();
  stmt2 = stmts->Queue_First()->Qnode_Next()->Qnode_Item();

  shackle_list1 = (QUEUE<WN *> *) 
    WN_MAP_Get (shackle_shackle_map, stmt1);
  shackle_list2 = (QUEUE<WN *> *)
    WN_MAP_Get (shackle_shackle_map, stmt2);
  ref1 = shackle_list1->Queue_First()->Qnode_Item();
  ref2 = shackle_list2->Queue_First()->Qnode_Item();
  result = 
    _xdependence_is_preserved (stmt1, stmt2, ref1, ref2, TRUE, shq);
  if (result) 
    fprintf(stdout, "Dependence is preserved\n");
  else
    fprintf(stdout, "Dependence is violated\n");
}

static BOOL
Stmt_With_Only_Scalars(WN *stmt)
{
  switch (WN_operator (stmt)) {
  case OPR_LDID:
  case OPR_STID:
    return TRUE;
  case OPR_ARRAY:
  case OPR_ILOAD:
  case OPR_MLOAD:
  case OPR_ISTORE:
  case OPR_MSTORE:
    return FALSE;
  default:
    FOR_CHILDREN (stmt, child, ignCount) {
      if (FALSE == Stmt_With_Only_Scalars (child))
	return FALSE;
    }
    END_CHILDREN;
    return TRUE;
  }
}

static WN *
Ldid_Comes_From_Loop(WN *wn)
{
  FmtAssert (OPR_LDID == WN_operator (wn),
	     ("Ldid_Comes_From_Loop called with non Ldid"));
  DU_MANAGER *du = Du_Mgr;
  DEF_LIST *def_list = du->Ud_Get_Def (wn);
  WN *parent;
  DEF_LIST_ITER iter(def_list);
  const DU_NODE *node = NULL;

  for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
    WN *def = node->Wn();
    parent = LWN_Get_Parent (def);
    // Can't possibly come from a loop
    if ((NULL == parent) || (OPC_DO_LOOP != WN_opcode (parent)))
      return NULL;
  }
  return parent;
}

static WN *
Enclosing_If_Or_Store (WN *wn)
{
  if (NULL == wn)
    return NULL;
  else if (OPC_IF == WN_opcode (wn) ||
	   OPCODE_is_store (WN_opcode (wn))) 
    return wn;
  else
    return Enclosing_If_Or_Store (LWN_Get_Parent (wn));
}

static WN *
Enclosing_Store (WN *wn)
{
  if (NULL == wn)
    return NULL;
  if (OPCODE_is_store (WN_opcode (wn)))
    return wn;
  else
    return Enclosing_Store (LWN_Get_Parent (wn));
}

static BOOL
Stid_Comes_From_Loop (WN *wn)
{
  FmtAssert (OPR_STID == WN_operator (wn),
	     ("Stid_Comes_From_Loop called with non Stid"));
  WN *parent = LWN_Get_Parent (wn);
  if (NULL == parent || (OPC_DO_LOOP != WN_opcode (parent)))
    return FALSE;
  return TRUE;
}
// Find a scalar that has not yet been chained. Take care to
// ensure that it is not inside array expressions.

static WN *
Find_Unchained_Scalar(WN *root)
{
  if (OPR_LDID == WN_operator (root)) {
    if (Shackle_Chain_Not_Visited(root)) {
      WN *frm_loop = Ldid_Comes_From_Loop (root);
      if (NULL == frm_loop) {
	//Shackle_Chain_Visited_Set (root);
	return root;
      } else {
	// Shackle_Chain_Visited_Set (root);
	return NULL;
      }
    } else
      return NULL;
  } 
  else if (OPR_ARRAY == WN_operator (root) ||
	     OPR_ILOAD == WN_operator (root) ||
	     OPR_MLOAD == WN_operator (root))
    return NULL;
  else if (OPR_STID == WN_operator (root)) {
    if (Shackle_Chain_Not_Visited (root)) {
      BOOL frm_loop = Stid_Comes_From_Loop (root);
      if (FALSE == frm_loop) {
	// Shackle_Chain_Visited_Set (root);
	return root;
      } else {
	// Shackle_Chain_Visited_Set (root);
	return NULL;
      }
    } else
      return NULL;
  }
  FOR_CHILDREN (root, child, ignCount) {
    WN *result = Find_Unchained_Scalar (child);
    if (NULL != result)
      return result;
  }
  END_CHILDREN;
  return NULL;
}

static WN *
Find_Unchained_If_With_Scalar_Cond(WN *root)
{
  if (OPC_IF == WN_opcode (root)) 
    if (Shackle_Chain_Not_Visited(root)) {
      WN *contained_scalars = 
	Find_Unchained_Scalar (WN_if_test (root));
      if (NULL != contained_scalars) {
	// Shackle_Chain_Visited_Set (root);
	return root;
      } else {
	// Shackle_Chain_Visited_Set (root);
      }
    }
  FOR_CHILDREN (root, child, ignCount) {
    WN *result = Find_Unchained_If_With_Scalar_Cond (child);
    if (NULL != result)
      return result;
  }
  END_CHILDREN;
  return NULL;
}

static WN *
Find_Unchained_Store_With_Scalar(WN *root)
{
  if (OPCODE_is_store (WN_opcode (root))) {
    if (Shackle_Chain_Not_Visited (root)) {
      WN *contained_scalars = Find_Unchained_Scalar (root);
      if (NULL != contained_scalars)
	return root;
    }
  }
  else {
    FOR_CHILDREN (root, child, ignCount) {
      WN *result = Find_Unchained_Store_With_Scalar (child);
      if (NULL != result)
	return result;
    }
    END_CHILDREN;
  }
  return NULL;
}

static void
Initialize_Shackle_Chain_Id_Map (QUEUE<WN *> *s)
{
  QUEUE_ITER<WN *>  iter (s);
  WN               *stmt;

  while (iter.Step (&stmt)) {
    WN_MAP32_Set (shackle_chain_id_map, stmt, 
		  shackle_chain_id_map_cnt);
  }
}

static void
Create_Chains_Of_Scalars(WN *top_level)
{
  FmtAssert (OPC_DO_LOOP == WN_opcode (top_level),
	     ("Non do loop being shackled"));
  WN *block = WN_do_body (top_level);
  FmtAssert (OPC_BLOCK == WN_opcode (block),
	     ("Not a block as the statement list in a do?"));
  QUEUE<WN *> *work_q = 
    CXX_NEW (QUEUE<WN *> (&shackle_default_pool), 
	     &shackle_default_pool);
  WN *stmt = Find_Unchained_If_With_Scalar_Cond (top_level);
  if (NULL == stmt)
    stmt = Find_Unchained_Store_With_Scalar (top_level);
  while (NULL != stmt) {
    shackle_chain_id_map_cnt++;
    WN *scalar;
    scalar = Find_Unchained_Scalar (stmt);
    while (NULL != scalar) {
      Shackle_Chain_Visited_Set (scalar);
      work_q->Add_Tail_Q (scalar);
      scalar = Find_Unchained_Scalar (stmt);
    }
    while (!work_q->Queue_Isempty()) {
      scalar = work_q->Get_Q();
      FmtAssert (NULL != scalar,
		 ("Null scalar in Unchained if"));
      FmtAssert (OPR_LDID == WN_operator (scalar) ||
		 OPR_STID == WN_operator (scalar),
		 ("Scalar not an Stid or Ldid"));
      if (OPR_LDID == WN_operator (scalar)) {
	DU_MANAGER *du = Du_Mgr;
	DEF_LIST *def_list = du->Ud_Get_Def (scalar);
	DEF_LIST_ITER iter(def_list);
	const DU_NODE *node = NULL;
	for (node = iter.First(); !iter.Is_Empty(); 
	     node = iter.Next()) {
	  WN *def = node->Wn();
	  WN *parent = Enclosing_If_Or_Store (def);
	  if (NULL != parent)
	    WN_MAP32_Set (shackle_chain_id_map, parent,
			  shackle_chain_id_map_cnt);
	  if (OPR_STID == WN_operator (def)) 
	    work_q->Add_Tail_Q (def);
	}
      }
      else if (OPR_STID == WN_operator (scalar)) {
	Shackle_Chain_Visited_Set (scalar);
	DU_MANAGER *du = Du_Mgr;
	USE_LIST *use_list = du->Du_Get_Use (scalar);
	USE_LIST_ITER iter (use_list);
	const DU_NODE *node = NULL;
	for (node = iter.First(); !iter.Is_Empty();
	     node = iter.Next()) {
	  WN *use = node->Wn();
	  WN *parent = Enclosing_If_Or_Store (use);
	  if (NULL != parent)
	    WN_MAP32_Set (shackle_chain_id_map, parent,
			  shackle_chain_id_map_cnt);
	  if (NULL != parent) {
	    WN *nscalar = Find_Unchained_Scalar (parent);
	    while (NULL != nscalar) {
	      Shackle_Chain_Visited_Set (nscalar);
	      work_q->Add_Tail_Q (nscalar);
	      nscalar = Find_Unchained_Scalar (parent);
	    }
	  }
	}
      }
    }
    stmt = Find_Unchained_If_With_Scalar_Cond (top_level);
    if (NULL == stmt)
      stmt = Find_Unchained_Store_With_Scalar (top_level);
  }
}

// Is an index expression shackleable? - it is if it is not an
// array expression or non-loop scalars

static BOOL
Is_Index_Expr_Shackleable(WN *wn)
{
  if (OPR_ARRAY == WN_operator (wn))
    return FALSE;
  else if (OPR_LDID == WN_operator (wn)) 
    return (NULL != Ldid_Comes_From_Loop (wn));
  else {
    FOR_CHILDREN (wn, child, ignCount) {
      BOOL ret_val = Is_Index_Expr_Shackleable (child);
      if (FALSE == ret_val)
	return FALSE;
    }
    END_CHILDREN;
    return TRUE;
  }
}

static BOOL
Set_Dim_Shackleable(SHACKLE_INFO *sh, INT32 dim, WN *wn)
{
  BOOL ret_val;

  if (OPR_ARRAY == WN_operator (wn)) {
    ST *st = Identical_Array_Refbase (wn, wn);
    if (st == sh->Symbol()) {
      INT32 ndim = (WN_kid_count (wn) >> 1);
      if (dim >= ndim) { // there must have been reshaping..
	sh->Set_Reshaped (TRUE);
	return FALSE;
      }
      ret_val = 
	Is_Index_Expr_Shackleable (WN_kid (wn, ndim + 1 + dim));
      if (FALSE == ret_val) {
	sh->Set_Dim_Shackled (dim, FALSE);
	return FALSE;
      } else
	return TRUE;
    } else
      return TRUE;
  } 
  else {
    FOR_CHILDREN (wn, child, ignCount) {
      ret_val = Set_Dim_Shackleable (sh, dim, child);
      if (FALSE == ret_val)
	return FALSE;
    }
    END_CHILDREN;
    return TRUE;
  }
}

static void
Set_Shackleable(SHACKLE_INFO *sh, WN *main_snl)
{
  INT32 ndim = sh->Ndim();
  for (INT32 i = 0; i < ndim; i++) 
    Set_Dim_Shackleable (sh, i, main_snl);
  if (sh->Is_Reshaped()) {
    for (INT32 j = 0; j < ndim; j++)
      sh->Set_Dim_Shackled (j, FALSE);
  }
}

static void
Set_Shackleable(QUEUE<SHACKLE_INFO *> *shq, WN *main_snl)
{
  QUEUE_ITER<SHACKLE_INFO *>  iter (shq);
  SHACKLE_INFO               *sh;

  while (iter.Step (&sh)) 
    Set_Shackleable (sh, main_snl);
}

static void
Dump_Shackle_Info (SHACKLE_INFO *sh)
{
  extern void dump_st(ST *);

  fprintf(stdout, "Shackle_info for ");
  dump_st ((ST *) sh->Symbol());
  fprintf(stdout, "[");
  for (INT32 i = 0; i < sh->Ndim(); i++) {
    if (sh->Is_Dim_Shackled (i)) 
      fprintf(stdout, "{Sh %3d}", sh->Shackle_Dim_Size (i));
    else
      fprintf(stdout, "{No sh}");
  }
  fprintf(stdout, "]");
  fprintf(stdout, "\n");
}

QUEUE<WN *> *
Extract_Stmts_With_Chain_Id(QUEUE<WN *> *stmts,
			    INT32 chain_id)
{
  QUEUE<WN *> *result = CXX_NEW (QUEUE<WN *> (&shackle_default_pool),
				 &shackle_default_pool);
  QUEUE_ITER<WN *> iter (stmts);
  WN              *step;

  while (iter.Step (&step)) {
    INT32 id = WN_MAP32_Get (shackle_chain_id_map, step);
    if (id == chain_id)
      result->Add_Tail_Q (step);
  }
  return result;
}

static WN *
Enclosing_Do_Loop_Of_Chain(QUEUE<WN *> *stmts)
{
  if (stmts->Queue_Isempty())
    return NULL;
  WN *head = stmts->Queue_First()->Qnode_Item();
  INT32 chain_id = WN_MAP32_Get (shackle_chain_id_map, head);
  QUEUE_ITER<WN *> iter (stmts);
  WN *step;
  INT32 id;
  WN *do_loop = Enclosing_Do_Loop (head);
  if (NULL == do_loop)
    return NULL;

  while (iter.Step (&step)) {
    id = WN_MAP32_Get (shackle_chain_id_map, step);
    FmtAssert (id == chain_id,
	       ("Statements in the same chain have different ids!"));
    do_loop = LNO_Common_Loop (step, do_loop);
    if (NULL == do_loop)
      return NULL;
  }
  return do_loop;
}

static BOOL
Index_Derived_From_Parents_Of (WN *index_exp, WN *do_loop)
{
  if (OPR_LDID == WN_operator (index_exp)) {
    WN *dup_loop = Ldid_Comes_From_Loop (index_exp);
    FmtAssert (NULL != dup_loop,
	       ("Index expression must come from loop"));
    return Is_Parent_Of (dup_loop, do_loop);
  } 
  else if (OPR_ARRAY == WN_operator (index_exp)) {
    FmtAssert (FALSE, ("Index exp cannot contain an array!"));
    return FALSE;
  }
  else {
    FOR_CHILDREN (index_exp, child, ignCount) {
      BOOL ret_val = Index_Derived_From_Parents_Of (child, do_loop);
      if (FALSE == ret_val)
	return FALSE;
    }
    END_CHILDREN;
    return TRUE;
  }
}

// Given a chain of a single Id, find a list of shackleable references
// on this chain.

static void
Shackleable_Refs_From_Stmt_In_Chain(QUEUE<SHACKLE_INFO*> *shq,
				    WN                   *stmt,
				    QUEUE<WN *>          *result,
				    WN                   *do_loop)
{
  if (OPR_ARRAY == WN_operator (stmt)) {
    const ST *st = Identical_Array_Refbase (stmt, stmt);
    SHACKLE_INFO *sh = Shackle_Info_For_Symbol (shq, st);
    if (NULL == sh)
      return;
    INT32 ndim = (WN_kid_count (stmt) >> 1);
    BOOL ret_val = TRUE;
    for (INT32 i = 0; i < sh->Ndim(); i++) {
      if (sh->Is_Dim_Shackled (i)) {
	ret_val = ret_val &&
	  Index_Derived_From_Parents_Of (WN_kid (stmt, ndim+1+i),
					 do_loop);
	if (FALSE == ret_val)
	  return;
      }
    }
    FmtAssert (TRUE == ret_val, 
	       ("Must be a shackleable references"));
    result->Add_Tail_Q (stmt);
    return;
  }
  else {
    FOR_CHILDREN (stmt, child, ignCount) {
      Shackleable_Refs_From_Stmt_In_Chain (shq, child, 
					   result, do_loop);
    }
    END_CHILDREN;
  }
}

static QUEUE<WN *> *
Shackleable_Refs_From_Chain (QUEUE<SHACKLE_INFO *> *shq,
			     QUEUE<WN *>           *stmtq)
{
  if (stmtq->Queue_Isempty())
    return NULL;
  WN *head = stmtq->Queue_First()->Qnode_Item();
  INT32 id, chain_id = WN_MAP32_Get (shackle_chain_id_map, head);
  QUEUE<WN *> *result = 
    CXX_NEW (QUEUE<WN *> (&shackle_default_pool),
	     &shackle_default_pool);
  WN *do_loop = Enclosing_Do_Loop_Of_Chain (stmtq);
  QUEUE_ITER<WN *> iter (stmtq);
  WN *step;

  while (iter.Step (&step)) {
    id = WN_MAP32_Get (shackle_chain_id_map, step);
    FmtAssert (id == chain_id,
	       ("Statments in same chain with differented ids!"));
    Shackleable_Refs_From_Stmt_In_Chain (shq, step, result, do_loop);
  }
  return result;
}


static void
Dump_Shackle_Info(QUEUE<SHACKLE_INFO *> *shq)
{
  QUEUE_ITER<SHACKLE_INFO *> iter(shq);
  SHACKLE_INFO              *sh;

  while (iter.Step (&sh)) 
    Dump_Shackle_Info (sh);
}

static void
Dump_Shackle_Chain_Id_Map_Info(WN *func_nd, QUEUE<WN *> *s)
{
  QUEUE_ITER<WN *> iter (s);
  WN              *stmt;

  Whirl2F_Init(func_nd);
  while (iter.Step (&stmt)) {
    Whirl2F_Emit(stdout, stmt); 
    fprintf (stdout, ": %4d", 
	     WN_MAP32_Get (shackle_chain_id_map, stmt));
    fprintf(stdout, "\n");
  }
}

static void
Dump_Wn_Queue (WN *func_nd, QUEUE<WN *> *s)
{
  if (s == NULL)
    return;
  QUEUE_ITER<WN *> iter (s);
  WN              *step;

  Whirl2F_Init (func_nd);
  fprintf(stdout, "{{");
  while (iter.Step (&step)) {
    fprintf(stdout, "[[");
    Whirl2F_Emit (stdout, 
		  ((OPR_ARRAY == WN_operator (step)) ? 
		   LWN_Get_Parent (step) : step));
    fprintf(stdout, "]]");
  }
  fprintf(stdout, "}}");
}

static void
Dump_Shackle_Map_Info (WN *func_nd, QUEUE<WN *> *s)
{
  QUEUE_ITER<WN *>      iter(s);
  WN                   *step;

  Whirl2F_Init (func_nd);
  while (iter.Step(&step)) {
    QUEUE<WN *> *shackle = 
      (QUEUE<WN *> *) WN_MAP_Get (shackle_shackle_map, step);
    Whirl2F_Emit (stdout, step);
    fprintf(stdout, "-----------------");
    Dump_Wn_Queue (func_nd, shackle);
    fprintf(stdout, "\n");
  }
}

static void
Shackleable_Refs_From_All_Chains(QUEUE<SHACKLE_INFO *> *shq,
				 QUEUE<WN *>           *stmtq)
{
  shackle_chain_info = 
    CXX_NEW_ARRAY (QUEUE<WN *> *, 
		   shackle_chain_id_map_cnt + 1,
		   &shackle_default_pool);
  for (INT32 i = 1; i <= shackle_chain_id_map_cnt; i++) {
    QUEUE<WN *> *stmtq_chosen = 
      Extract_Stmts_With_Chain_Id (stmtq, i);
    shackle_chain_info[i] = 
      Shackleable_Refs_From_Chain (shq, stmtq_chosen);
    // Dump_Wn_Queue (func_nd, shackle_chain_info[i]);
  }
}

static WN*
Exists_Same_Array_In_Queue (WN *array, QUEUE<WN *> *array_q)
{
  if (NULL == array_q)
    return NULL;

  QUEUE_ITER<WN *> iter(array_q);
  WN              *step;

  while (iter.Step (&step)) {
    if (NULL != Identical_Array_Refbase (step, array))
      return step;
  }
  return NULL;
}

static BOOL
Simple_Chain_Shackle_Case(QUEUE<WN *>           *stmtq)
{
  QUEUE<WN *> *unchained = Extract_Stmts_With_Chain_Id (stmtq, 0);
  if (unchained->Queue_Isempty())
    return FALSE;
  BOOL simple_chain_case = _xis_simple_shackle_case (unchained);
  if (!simple_chain_case)
    return FALSE;
  WN *array_ref = WN_kid1 (unchained->Queue_First()->Qnode_Item());
  WN **chain_ref_list = 
    CXX_NEW_ARRAY (WN *, shackle_chain_id_map_cnt+1, 
		   &shackle_default_pool);
  for (INT32 i = 1; i <= shackle_chain_id_map_cnt; i++) {
    chain_ref_list[i] = 
      Exists_Same_Array_In_Queue (array_ref, shackle_chain_info[i]);
    if (NULL == chain_ref_list[i])
      return FALSE;
  }
  QUEUE_ITER <WN *>    iter(stmtq);
  WN                  *step;

  while (iter.Step (&step)) {
    INT32 chain_id = WN_MAP32_Get (shackle_chain_id_map, step);
    QUEUE<WN *> *shackle_q = 
      CXX_NEW (QUEUE<WN *> (&shackle_default_pool),
	       &shackle_default_pool);
    FmtAssert (0 <= chain_id && chain_id <= shackle_chain_id_map_cnt,    
	       ("Invalid range for the id of the chain"));
    if (0 == chain_id) 
      shackle_q->Add_Tail_Q (WN_kid1 (step));
    else
      shackle_q->Add_Tail_Q (chain_ref_list[chain_id]);
    WN_MAP_Set (shackle_shackle_map, step, (void *) shackle_q);
  }
  return TRUE;
}

static BOOL
Stored2Array_Assigned_Singly(WN *wn)
{
  if (OPR_ISTORE != WN_operator (wn)) 
    return FALSE;
  WN *kid1 = WN_kid1 (wn);
  if (OPR_ARRAY != WN_operator (kid1))
    return FALSE;
  ARRAY_DIRECTED_GRAPH16 *dg = Array_Dependence_Graph;
  if (NULL == dg)
    return FALSE;
  VINDEX16 v = dg->Get_Vertex (wn);
  EINDEX16 e;
  if (0 == v)
    return FALSE;
  // If there are no in and out edges, certainly a private copy
  if (!dg->Get_In_Edge (v) && !dg->Get_Out_Edge (v))
    return TRUE;
  // If self input dependences, certainly not a private copy.
  if (dg->Get_In_Edge (v)) {
    for (e = dg->Get_In_Edge (v); e != 0; e = dg->Get_Next_In_Edge(e))
      if (dg->Get_Source (e) == v)
	return FALSE;
  }
  // If self output depedences, certainly not a private copy
  if (dg->Get_Out_Edge (v)) {
    for (e = dg->Get_Out_Edge (v); e != 0; 
	 e = dg->Get_Next_Out_Edge (e))
      if (dg->Get_Sink (e) == v)
	return FALSE;
  }
  return TRUE;
}

static WN *
Ref_To_Same_Array(WN *array, QUEUE<WN *> *refq)
{
  QUEUE_ITER<WN *>  iter (refq);
  WN               *step;

  while (iter.Step (&step)) {
    if (Identical_Array_Refbase (step, array))
      return step;
  }
  return NULL;
}

static BOOL
Scalar_Queue_Contains_Scalar(QUEUE<WN *> *lst,
			     WN          *scalar)
{
  FmtAssert (OPR_LDID == WN_operator (scalar) ||
	     OPR_STID == WN_operator (scalar),
	     ("Scalar_Queue_Contains_Scalar called w/ non scalar"));
  QUEUE_ITER<WN *>       iter(lst);
  WN                    *step;

  while (iter.Step (&step)) {
    FmtAssert (OPR_LDID == WN_operator (step) ||
	       OPR_STID == WN_operator (step),
	       ("Scalar queue contains non scalars!"));
    // If the scalar is already there in another form
    if (WN_st (step) == WN_st (scalar) &&
	WN_offset (step) == WN_offset (scalar)) 
      return TRUE;
  }
  return FALSE;
}


static BOOL
Is_Nonloop_Scalar (WN *wn)
{
  FmtAssert (OPR_LDID == WN_operator (wn) ||
	     OPR_STID == WN_operator (wn),
	     ("Is_Nonloop_Scalar called with non LD/STid"));
  if (OPR_STID == WN_operator (wn)) 
    return !Stid_Comes_From_Loop (wn);
  else {
    if (Ldid_Comes_From_Loop (wn))
      return FALSE;
    else if (NULL != LWN_Get_Parent (wn) && 
	     OPR_ARRAY == WN_operator (LWN_Get_Parent (wn)) &&
	     wn == WN_kid0 (LWN_Get_Parent (wn)))
      return FALSE;
    else
      return TRUE;
  }
}

static void
Gather_Nonloop_Scalars_In_Wn(QUEUE<WN *> *lst,
			     WN          *wn)
{
  if (NULL == wn)
    return;
  else if (OPR_LDID == WN_operator (wn) || 
	   OPR_STID == WN_operator (wn)) {
    if (!Scalar_Queue_Contains_Scalar (lst, wn) &&
	Is_Nonloop_Scalar (wn))
      lst->Add_Tail_Q (wn);
    return;
  }
  else {
    FOR_CHILDREN (wn, child, ignCount) {
      Gather_Nonloop_Scalars_In_Wn (lst, child);
    }
    END_CHILDREN;
  }
}


static BOOL
Shackle_Scalars_Privatizable (WN *loop, QUEUE<WN *> *scq)
{
  QUEUE_ITER<WN *>     iter1(scq);
  WN                  *step1;
  DU_MANAGER          *du_mgr = Du_Mgr;

  while (iter1.Step (&step1)) 
    if (SE_EASY != Scalar_Expandable (step1, loop, du_mgr))
      return FALSE;
  return TRUE;
}

static void
Shackle_Scalars_Do_Privatize(WN *loop, QUEUE<WN *> *scq)
{
  QUEUE<WN *>     *old_stmts = gather_stmts_in_func (loop);
  QUEUE_ITER<WN *> old_iter (old_stmts);
  WN              *old_step;
  INT32            count = old_stmts->Queue_Length();
  QUEUE<WN *>    **shackle = CXX_NEW_ARRAY (QUEUE<WN *> *,
					    count, 
					    &shackle_default_pool);
  count = 0;
  while (old_iter.Step (&old_step)) {
    shackle[count] = (QUEUE<WN *> *)
      WN_MAP_Get (shackle_shackle_map, old_step);
    count++;
  }
  FmtAssert (count == old_stmts->Queue_Length(),
	     ("Queue length cannot change!"));

  QUEUE_ITER<WN *>     iter2(scq);
  WN                  *step2;

  while (iter2.Step (&step2)) {
    FmtAssert (OPR_LDID == WN_operator (step2) ||
	       OPR_STID == WN_operator (step2),
	       ("Scalar queue contains non scalars!"));
    INT32 dummy[1] = {0};
    SYMBOL sym(step2);
    Scalar_Expand (loop, loop, step2, sym, &loop, dummy,
		   1, TRUE, FALSE, FALSE, NULL);
  }
  QUEUE<WN *>     *new_stmts = gather_stmts_in_func (loop);
  QUEUE_ITER<WN *> new_iter (new_stmts);
  WN              *new_step;
  count = 0;
  while (new_iter.Step (&new_step)) {
    WN_MAP_Set (shackle_shackle_map, new_step, 
		(void *) shackle[count]);
    count++;
  }
  FmtAssert (count == new_stmts->Queue_Length(),
	     ("New statements Introduced!"));
  return;
}

static QUEUE<WN *> *
Create_Alternate_Simple_Chain_Shackle(QUEUE<WN *> *sq)
{
  QUEUE_ITER<WN *>          iter (sq);
  WN                       *step;
  QUEUE <WN *>             *result;

  result = CXX_NEW (QUEUE<WN *>(&shackle_default_pool),
		    &shackle_default_pool);

  while (iter.Step (&step)) {
    QUEUE<WN *> *old = (QUEUE<WN *> *)
      WN_MAP_Get (shackle_shackle_map, step);
    INT32 chain_id = WN_MAP32_Get (shackle_chain_id_map, step);
    QUEUE<WN *> *ref_array = (QUEUE<WN *> *)
      WN_MAP_Get (shackle_ref_map, step);
    if (0 != chain_id && 
	NULL != ref_array && 
	!ref_array->Queue_Isempty()) {
      WN *old_array = old->Queue_First()->Qnode_Item();
      WN *alternate = Ref_To_Same_Array (old_array, ref_array);
      if (NULL != alternate) {
	QUEUE<WN *> *newref = 
	  CXX_NEW (QUEUE<WN *> (&shackle_default_pool),
		   &shackle_default_pool);
	newref->Add_Tail_Q (alternate);
	WN_MAP_Set (shackle_shackle_map, step, (void *) newref);
	Gather_Nonloop_Scalars_In_Wn (result, alternate);
      }
    }
  }
  return result;
}

static BOOL
SNL_Contains_Ge2_Do_Loops(WN *main_snl)
{
  if (OPC_DO_LOOP != WN_opcode (main_snl))
    return FALSE;
  INT32 do_children_count = 0;
  FOR_CHILDREN (WN_do_body (main_snl), child, ignCount) {
    if (OPC_DO_LOOP == WN_opcode (child))
      do_children_count++;
  }
  END_CHILDREN;
  return (do_children_count >= 2);
}

#ifdef KEY
// 
static BOOL _xfunc_has_vectorizable_loop (WN* func_nd)
{
  QUEUE_WKLIST_ITER<WN *> wklist(func_nd, &shackle_default_pool);
  WN *                    wk_item;
  
  while (wklist.Step(&wk_item)) {
    if (OPC_DO_LOOP == WN_opcode (wk_item)) {
      DO_LOOP_INFO* dli = Get_Do_Loop_Info(wk_item);
      if (dli->Vectorizable)
	return TRUE;
      wklist.Wklist_Queue()->Add_Tail_Q (WN_do_body (wk_item));
    } else {
      FOR_CHILDREN (wk_item, child, ignCount) {
	assert (child != (WN *) NULL);
	wklist.Wklist_Queue()->Add_Tail_Q (child);
      }
      END_CHILDREN;
    }
  }
  return FALSE;
}
#endif
static BOOL
Per_SNL_Shackle_Phase(WN *main_snl, 
		      WN *func_nd)
{
  /* We are not doing anything right now */
  QUEUE<WN *>            *s;
  QUEUE<SHACKLE_INFO*>   *shackle_info;
  BOOL                    shackle_legal;
  INT32                   shackling_depth;
  WN                    **shackle_loops_info;

  if (OPC_DO_LOOP != WN_opcode (main_snl))
    return FALSE;
  if (!SNL_Contains_Ge2_Do_Loops (main_snl))
    return FALSE;
  DO_LOOP_INFO *dli = Get_Do_Loop_Info (main_snl);

  // Under these conditions, we'll not shackle this loop
  if (dli->Cannot_Block || dli->Auto_Parallelized || 
      dli->Is_Outer_Lego_Tile || dli->Is_Inner_Lego_Tile ||
      dli->Is_Processor_Tile || dli->Is_Doacross ||
      dli->Suggested_Parallel) 
    return FALSE;

  s = gather_stmts_in_func (main_snl);
  shackle_chain_id_map_cnt = 0;
  if (shackle_debug_level > 0) 
    printf("The number of statements is %d\n", s->Queue_Length());
#ifdef KEY
  /* Bug 4063
   * Under -LNO:simd_avoid_fusion, loops which would normally be fused
   * before Shackle phase get shackled here. There is assumption here
   * that fusion will avoid such loops for shackle. We should not 
   * shackle loops that have vectorizable statements, in line with
   * the author's expectation. ( Shackle output appears to be sane but
   * there is unnecessary overhead introduced which is probably not what 
   * the author had wanted. It is possible that the original code was 
   * never tested in the absence of fusion for cases like bug 4063. )
   */
  if ( LNO_Run_Simd > 0 && 
       LNO_Simd_Avoid_Fusion /* In line with avoid_fusion */ ) {
    if (_xfunc_has_vectorizable_loop (main_snl)) {
      if (shackle_debug_level > 0) 
	printf("Vectorizable loop(s) prevent function from being shackled.\n");
      return FALSE;
    }
  }       
#endif
  /* If the function has complicated statements, it can't be
     shackled. Complicated == calls, I/O etc */
  if (TRUE == _xfunc_has_stmts2prevent_shackle (s)) {
    if (shackle_debug_level > 0) 
      printf("Bad statements prevent function from being shackled\n");
    return FALSE;
  }
  if (TRUE == _xis_simple_shackle_case (s)) {
    if (shackle_debug_level > 0) 
      printf("Simple case of shackling\n");
    Initialize_Shackle_Chain_Id_Map (s);
    Form_Statement_Refs(s);
    // test_significance (s);
    _xcreate_simple_basic_shackle(s);
    shackle_info = 
      _xcreate_shackle_map_for_arrays_in_func(main_snl, func_nd);
    Set_Shackleable (shackle_info, main_snl);
    // Dump_Shackle_Info (shackle_info);
    if (shackle_debug_level > 0) 
      printf("Number of arrays to be shackled: %d\n",
	     shackle_info->Queue_Length());
    // Did we find any arrays to shackle?
    if (0 == shackle_info->Queue_Length()) 
      return FALSE;
    if (!Appropriate_Shackle_Size_Set (main_snl, shackle_info))
      return FALSE;
    shackle_legal = _xis_legal_shackle (s, shackle_info);
    if (!shackle_legal)
      return FALSE;
    if (Get_Trace(TP_LNOPT2, TT_SHACKLE_ONLY))
      Augment_Simple_Basic_Shackle (s, shackle_info);
    // Dump_Shackle_Map_Info (func_nd, s);
    if (!Reuse_Exists_In_Loop (main_snl, shackle_info))
      return FALSE;
    shackling_depth = Common_Shackling_Depth (s, shackle_info);
    if (0 == shackling_depth)
      return FALSE;
    shackle_loops_info = Create_Simple_Shackle_Loops (main_snl, s, 
						      shackle_info, 
						      shackling_depth);
    Create_Shackle_If_All_Stmts (main_snl, s, shackle_info,
				 shackle_loops_info,
				 shackling_depth);
    return TRUE;
  }
  else {
    if (shackle_debug_level > 0) 
      printf("Not a simple case of shackling\n");
    Initialize_Shackle_Chain_Id_Map (s);
    Create_Chains_Of_Scalars(main_snl);
    // Dump_Shackle_Chain_Id_Map_Info (func_nd, s);
    shackle_info = 
      _xcreate_shackle_map_for_arrays_in_func(main_snl, func_nd);
    if (0 == shackle_info->Queue_Length()) 
      return FALSE;
    Set_Shackleable (shackle_info, main_snl);
    // Dump_Shackle_Info (shackle_info);
    Shackleable_Refs_From_All_Chains (shackle_info, s);
    if (Simple_Chain_Shackle_Case(s)) {
      // Dump_Shackle_Map_Info (func_nd, s);
      Form_Statement_Refs(s);
      if (!Appropriate_Shackle_Size_Set (main_snl, shackle_info))
	return FALSE;
      shackle_legal = _xis_legal_shackle (s, shackle_info);
      if (!shackle_legal) {
	QUEUE<WN *> *privatize_alternate = 
	  Create_Alternate_Simple_Chain_Shackle(s);
	BOOL alternate_shackle = 
	  Shackle_Scalars_Privatizable (main_snl, privatize_alternate);
	if (!alternate_shackle)
	  return FALSE;
	// Dump_Shackle_Map_Info (func_nd, s);
	if (!Appropriate_Shackle_Size_Set (main_snl, shackle_info))
	  return FALSE;
	shackle_legal = _xis_legal_shackle (s, shackle_info);
	if (!shackle_legal) {
	  if (shackle_debug_level > 0)
	    fprintf(stdout, "Illegal shackling\n");
	  return FALSE;
	}
	else {
	  if (shackle_debug_level > 0)
	    fprintf(stdout, "Alternate legal shackling\n");
	  Shackle_Scalars_Do_Privatize (main_snl, privatize_alternate);
	  s = gather_stmts_in_func (main_snl);
	}
      } else {
	if (shackle_debug_level > 0)
	  fprintf(stdout, "Legal shackling");
      }
      if (!Reuse_Exists_In_Loop (main_snl, shackle_info))
	return FALSE;
      shackling_depth = Common_Shackling_Depth (s, shackle_info);
      if (0 == shackling_depth)
	return FALSE;
      shackle_loops_info = 
	Create_Simple_Shackle_Loops (main_snl, s, shackle_info,
				     shackling_depth);
      Create_Shackle_If_All_Stmts (main_snl, s, shackle_info,
				   shackle_loops_info,
				   shackling_depth);
      return TRUE;
    }
    else
      return FALSE;
  }
}

void
SHACKLE_Phase (WN *func_nd)
{
  BOOL                    func_shackled = FALSE;

  // If shackling is not enabled.
  if (!LNO_Shackle && !Get_Trace(TP_LNOPT2, TT_SHACKLE_ONLY)) 
    return;
  if (Get_Trace(TP_LNOPT2, TT_TILE_ONLY))
    return; 

  if (Get_Trace(TP_LNOPT2, TT_SHACKLE_DEBUG))
    shackle_debug_level = 1;
  else
    shackle_debug_level = 0;
  
  if (shackle_debug_level > 0) {
    printf("Shackling started\n");
  }
  
  MEM_POOL_Initialize (&shackle_default_pool, "shackle_default_pool", FALSE);
  MEM_POOL_Initialize (&shackle_map_pool, "shackle_map_pool", FALSE);
  MEM_POOL_Push (&shackle_default_pool);
  Shackle_Mem_Initialize (&shackle_default_pool);
  shackle_ref_map = WN_MAP_Create (&shackle_map_pool);
  shackle_shackle_map = WN_MAP_Create (&shackle_map_pool);
  shackle_chain_map = WN_MAP32_Create (&shackle_map_pool);
  shackle_chain_id_map = WN_MAP32_Create (&shackle_map_pool);
  FIZ_FUSE_INFO *ffi = 
    CXX_NEW (FIZ_FUSE_INFO (&shackle_default_pool), 
	     &shackle_default_pool);
  ffi->Build(func_nd);
  for (INT32 i = 0; i < ffi->Num_Snl(); i++) {
    if (ffi->Get_Type(i) != Invalid) {
      WN *loop = ffi->Get_Wn (i);
      // For inner outer SNLs, we shackle them only once..
      if (Contained_In_Loop (loop))
	continue;
      if (Per_SNL_Shackle_Phase (ffi->Get_Wn (i), func_nd))
	func_shackled = TRUE;
    }
  }
  if (func_shackled) {
    shackle_if_init(&shackle_default_pool);
    analyze_stmts_in_func_for_if (func_nd);
    LWN_Parentize (func_nd);
    Convert_Do_Loops_Conditionals (func_nd);
    shackle_if_finalize();
    LWN_Parentize (func_nd);
    Shackle_Postprocess_Do_Loop_Bounds(func_nd);
  }
  WN_MAP_Delete (shackle_ref_map);
  WN_MAP_Delete (shackle_shackle_map);
  WN_MAP_Delete (shackle_chain_map);
  WN_MAP_Delete (shackle_chain_id_map);
  MEM_POOL_Pop (&shackle_default_pool);
  MEM_POOL_Delete (&shackle_default_pool);
  MEM_POOL_Delete (&shackle_map_pool);
}
