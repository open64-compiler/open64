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


#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include <sys/types.h>
#include <assert.h>
#include <stdio.h>
#include "wn.h"
#include "cxx_memory.h"
#include "cxx_queue.h"
#include "if_info.h"
#include "lnopt_main.h"
#include "lwn_util.h"
#include "lnoutils.h"
#include "access_vector.h"
#include "lego_util.h"
#include "soe.h"
#include "wn_simp.h"
#include "opt_du.h"
#include "array_bounds.h"
#include "access_main.h"
#include "shackle.h"
#include "shackle_ifs.h"

static INT32     shackle_if_debug_level;
extern MEM_POOL  LNO_default_pool;
extern MEM_POOL  LNO_local_pool;
static MEM_POOL* shackle_if_pool;
extern MEM_POOL  shackle_map_pool;
static WN_MAP    shackle_if_copy_map1;
static WN_MAP    shackle_if_if_map;
static BOOL      hoist_mins_from_loop_bounds = TRUE;
static INT32     shackle_debug_exec_count;
static BOOL      Has_Sibling_In_Block(WN *);
static WN*       Shackle_Unseen_Highest_Enclosing_If(WN *stmt);
static void      Maybe_Handle_Sink_Promotion_Case (WN *,
						   WN *,
						   INT32, 
						   ACCESS_VECTOR *);

#define SHACKLE_IF_HAS_BEEN_SEEN 1
#define SHACKLE_IF_YET_UNSEEN    0

#define Shackle_If_Seen(wn) \
  (WN_MAP32_Get (shackle_if_if_map, (wn)) == SHACKLE_IF_HAS_BEEN_SEEN)
#define Shackle_If_Unseen(wn) \
  (WN_MAP32_Get (shackle_if_if_map, (wn)) == SHACKLE_IF_YET_UNSEEN)
#define Shackle_If_Seen_Set(wn) \
  (WN_MAP32_Set (shackle_if_if_map, (wn), SHACKLE_IF_HAS_BEEN_SEEN))
#define Shackle_If_Unseen_Set(wn) \
  (WN_MAP32_Set (shackle_if_if_map, (wn), SHACKLE_IF_YET_UNSEEN))


static void
Recursively_Add_Bound_Lin_Symbols(QUEUE<ST *> *lin_sym_q, 
				  WN *do_loop)
{
  if (!do_loop)
    return;
  if (OPC_DO_LOOP == WN_opcode (do_loop)) {
    DO_LOOP_INFO *dli = Get_Do_Loop_Info (do_loop);
    ACCESS_ARRAY *lb, *ub;
    lb = dli->LB;
    ub = dli->UB;
    ACCESS_VECTOR *ar;
    INT32 index;

    INT32 i;
    for (i = 0; i < lb->Num_Vec(); i++) {
      ar = lb->Dim(i);
      if (NULL != ar->Lin_Symb) {
	INTSYMB_CONST_ITER iter(ar->Lin_Symb);
	for (const INTSYMB_NODE *node = iter.First();
	     !iter.Is_Empty();
	     node = iter.Next()) {
	  if (0 == node->Coeff) 
	    DevWarn("Access vector has zero coeff. linear symbol");
	  else {
	    index = lin_sym_q->Index (node->Symbol.St());
	    if (-1 == index) 
	      lin_sym_q->Add_Tail_Q (node->Symbol.St());
	  }
	}
      }
    }
    for (i = 0; i < ub->Num_Vec(); i++) {
      ar = ub->Dim(i);
      if (NULL != ar->Lin_Symb) {
	INTSYMB_CONST_ITER iter(ar->Lin_Symb);
	for (const INTSYMB_NODE *node = iter.First();
	     !iter.Is_Empty();
	     node = iter.Next()) {
	  if (0 == node->Coeff) 
	    DevWarn("Access vector has zero coeff. linear symbol");
	  else {
	    index = lin_sym_q->Index (node->Symbol.St());
	    if (-1 == index) 
	      lin_sym_q->Add_Tail_Q (node->Symbol.St());
	  }
	}
      }
    }
  }
  Recursively_Add_Bound_Lin_Symbols(lin_sym_q, 
				    LWN_Get_Parent (do_loop));
}

static void
Recursively_Add_Parent_If_Lin_Symbols (QUEUE<ST *> *lin_sym_q,
				       WN          *wn)
{
  if (!wn)
    return;
  if (OPC_IF == WN_opcode (wn)) {
    IF_INFO *ii = (IF_INFO *) WN_MAP_Get (LNO_Info_Map, wn);
    ACCESS_ARRAY *ar = ii->Condition;
    ACCESS_VECTOR *v;
    for (INT32 i = 0; i < ar->Num_Vec(); i++) {
      v = ar->Dim (i);
      if (NULL != v->Lin_Symb) {
	INTSYMB_CONST_ITER iter (v->Lin_Symb);
	for (const INTSYMB_NODE *node = iter.First();
	     !iter.Is_Empty(); node = iter.Next()) {
	  INT32 index = lin_sym_q->Index (node->Symbol.St());
	  if (-1 == index)
	    lin_sym_q->Add_Tail_Q (node->Symbol.St());
	}
      }
    }
  }
  Recursively_Add_Parent_If_Lin_Symbols (lin_sym_q,
					 LWN_Get_Parent (wn));
}

static void
Add_Parent_If_Constraints(WN                  *wn,
			  SYSTEM_OF_EQUATIONS *soe,
			  INT32                size_loop,
			  INT32                size_sym,
			  QUEUE<ST *>         *lin_sym_q)
{
  INT32 size = size_loop + size_sym, index, i, j;
  if (!wn)
    return;
  if (OPC_IF == WN_opcode (wn)) {
    IF_INFO *ii = (IF_INFO *) WN_MAP_Get (LNO_Info_Map, wn);
    ACCESS_ARRAY *ar = ii->Condition;
    mINT32 *row = CXX_NEW_ARRAY (mINT32, size, shackle_if_pool);
    for (i = 0; i < ar->Num_Vec(); i++) {
      ACCESS_VECTOR *v = ar->Dim (i);
      if (NULL == v->Non_Lin_Symb) {
	for (j = 0; j < size_loop; j++)
	  row[j] = v->Loop_Coeff (j);
	for (j = size_loop; j < size; j++)
	  row[j] = 0;
	if (NULL != v->Lin_Symb) {
	  INTSYMB_CONST_ITER iter (v->Lin_Symb);
	  for (const INTSYMB_NODE *node = iter.First();
	       !iter.Is_Empty(); node = iter.Next()) {
	    INT32 index = lin_sym_q->Index (node->Symbol.St());
	    FmtAssert (0 <= index && index < size, 
		       ("Invalid value for index"));
	    row[size_loop+index] = node->Coeff;
	  }
	}
	soe->Add_Le (row, v->Const_Offset);
      }
    }
  }
  Add_Parent_If_Constraints (LWN_Get_Parent (wn), soe,
			     size_loop, size_sym, lin_sym_q);
}

static void
Add_Parent_Loop_Constraints(WN                  *wn,
			    SYSTEM_OF_EQUATIONS *soe,
			    INT32                size_loop,
			    INT32                size_sym,
			    QUEUE<ST*>          *lin_sym_q)
{
  INT32 size = size_loop + size_sym, index;

  if (!wn)
    return;
  if (OPC_DO_LOOP == WN_opcode (wn)) {
    DO_LOOP_INFO *dli = Get_Do_Loop_Info (wn);
    ACCESS_ARRAY *lb, *ub;
    ACCESS_VECTOR *ar;
    lb = dli->LB;
    ub = dli->UB;
    INT32 i, j;
    mINT32 *row = CXX_NEW_ARRAY (mINT32, size, shackle_if_pool);
    for (i = 0; i < size; i++) 
      row[i] = 0;
    for (j = 0; j < lb->Num_Vec(); j++) {
      ar = lb->Dim(j);
      for (i = 0; i < ar->Nest_Depth(); i++) 
	row[i] = ar->Loop_Coeff (i);
      if (NULL != ar->Lin_Symb) {
	INTSYMB_CONST_ITER iter(ar->Lin_Symb);
	for (const INTSYMB_NODE *node = iter.First();
	     !iter.Is_Empty();
	     node = iter.Next()) {
	  if (0 == node->Coeff) 
	    DevWarn("Access vector has zero coeff linear symbol");
	  else {
	    index = lin_sym_q->Index (node->Symbol.St());
	    assert ((0 <= index) && (index < size_sym));
	    row[size_loop+index] = node->Coeff;
	  }
	}
      }
      soe->Add_Le (row, ar->Const_Offset);
    }
    for (j = 0; j < ub->Num_Vec(); j++) {
      ar = ub->Dim(j);
      for (i = 0; i < ar->Nest_Depth(); i++) 
	row[i] = ar->Loop_Coeff (i);
      if (NULL != ar->Lin_Symb) {
	INTSYMB_CONST_ITER iter(ar->Lin_Symb);
	for (const INTSYMB_NODE *node = iter.First();
	     !iter.Is_Empty();
	     node = iter.Next()) {
	  if (0 == node->Coeff) 
	    DevWarn("Access vector has zero coeff. linear symbol");
	  else {
	    index = lin_sym_q->Index (node->Symbol.St());
	    assert ((0 <= index) && (index < size_sym));
	    row[size_loop+index] = node->Coeff;
	  }
	}
      }
      soe->Add_Le (row, ar->Const_Offset);
    }
  }
  Add_Parent_Loop_Constraints (LWN_Get_Parent (wn), 
			       soe, size_loop, size_sym,
			       lin_sym_q);
}

static void
Recursively_Add_Array_Lin_Symbols(QUEUE<ST *> *lin_sym_q,
				  WN *wn)
{
  if (OPR_ARRAY == WN_operator (wn)) {
    ACCESS_ARRAY *ar = (ACCESS_ARRAY *) WN_MAP_Get (LNO_Info_Map, wn);
    ACCESS_VECTOR *av;
    INT32 index;

    for (INT32 i = 0; i < ar->Num_Vec(); i++) {
      av = ar->Dim (i);
      if (NULL != av->Lin_Symb) {
	INTSYMB_CONST_ITER iter (av->Lin_Symb);
	for (const INTSYMB_NODE *node = iter.First();
	     !iter.Is_Empty(); node = iter.Next()) {
	  index = lin_sym_q->Index (node->Symbol.St());
	  if (-1 == index)
	    lin_sym_q->Add_Tail_Q (node->Symbol.St());
	}
      }
    }
  }
  else {
    FOR_CHILDREN (wn, child, ignCount) {
      Recursively_Add_Array_Lin_Symbols (lin_sym_q, child);
    }
    END_CHILDREN;
  }
}

static void
Simplify_Cond_With_Div_Floor(WN *inp)
{
  LWN_Parentize (inp);
  OPERATOR opr = WN_operator (inp);
  if ((OPR_GT != opr) && (OPR_LT != opr) && (OPR_LE != opr) &&
      (OPR_GE != opr))
    return;
  WN *kid0 = WN_kid0 (inp);
  WN *kid1 = WN_kid1 (inp);
  INT64 const_val;
  WN *d1, *d2;
  WN *kid00, *kid01;
  TYPE_ID rtype, index_type;
  if (OPR_INTCONST != WN_operator (kid1))
    return;
  if (OPR_INTRINSIC_OP != WN_operator (kid0))
    return;
  INTRINSIC intrinsic;
  intrinsic = (INTRINSIC) WN_intrinsic (kid0);
  switch (intrinsic) {
  case INTRN_I4DIVFLOOR:
  case INTRN_I8DIVFLOOR:
  case INTRN_U4DIVFLOOR:
  case INTRN_U8DIVFLOOR:
    kid00 = WN_kid0 (kid0);
    kid01 = WN_kid1 (kid0);
    assert (OPR_PARM == WN_operator (kid00));
    assert (OPR_PARM == WN_operator (kid01));
    kid00 = WN_kid0 (kid00);
    kid01 = WN_kid0 (kid01);
    if (OPR_INTCONST != WN_operator (kid01))
      return;
    if (WN_const_val (kid1) < 0)
      return;
    const_val = WN_const_val (kid1) * WN_const_val (kid01);
    if (OPR_GT == opr)
      const_val += WN_const_val (kid01);
    if (OPR_LE == opr)
      const_val += WN_const_val (kid01) - 1;
    WN_const_val (kid1) = const_val;
    rtype = WN_rtype (kid00);
    d1 = WN_CreateIntconst (OPCODE_make_op (OPR_INTCONST,
					    rtype, MTYPE_V),
			    (INT64) 0);
    Replace_WN (kid00, d1);
    Replace_WN (kid0, kid00);
    LWN_Delete_Tree (kid0);
    return;
  default:
    return;
  }
}
    
static void
Simplify_Coeff_Access_Vector (ACCESS_VECTOR *ar)
{
  if (ar->Const_Offset != 0)
    return;
  if (NULL != ar->Lin_Symb)
    return;
  if (NULL != ar->Non_Lin_Symb)
    return;
  INT32 i, val, current = 0;
  for (i = 0; i < ar->Nest_Depth(); i++) {
    val = ar->Loop_Coeff (i);
    if (val < 0)
      val = -val;
    if (0 == current)
      current = val;
    else if ((val > 0) && (current != val))
      return;
  }
  for (i = 0; i < ar->Nest_Depth(); i++) {
    val = ar->Loop_Coeff (i) / current;
    ar->Set_Loop_Coeff (i, val);
  }
}

static BOOL
Is_Exp_Divided_By_Const(WN *wn, INT64 const_val)
{
  BOOL is_lhs = FALSE, is_rhs = FALSE;

  OPERATOR opr = WN_operator (wn);
  switch (opr) {
  case OPR_NEG:
    return Is_Exp_Divided_By_Const (WN_kid0 (wn), const_val);
  case OPR_ADD:
    is_lhs = Is_Exp_Divided_By_Const (WN_kid0 (wn), const_val);
    is_rhs = Is_Exp_Divided_By_Const (WN_kid1 (wn), const_val);
    return (is_lhs && is_rhs);
  case OPR_MPY:
    is_lhs = Is_Exp_Divided_By_Const (WN_kid0 (wn), const_val);
    is_rhs = Is_Exp_Divided_By_Const (WN_kid1 (wn), const_val);
    return (is_lhs || is_rhs);
  case OPR_INTCONST:
    return (((WN_const_val(wn) / const_val) * const_val) == 
	    WN_const_val (wn));
  default:
    return FALSE;
  }
}

static BOOL
Divide_Exp_By_Const (WN *wn, INT64 const_val)
{
  BOOL is_lhs = FALSE, is_rhs = FALSE;
  /* It must be the case that this expression has been verified
     to have been divisible by this const already */
  OPERATOR opr = WN_operator (wn);
  switch (opr) {
  case OPR_NEG:
    assert (TRUE == Divide_Exp_By_Const (WN_kid0 (wn), const_val));
    return TRUE;
  case OPR_ADD:
    is_lhs = Divide_Exp_By_Const (WN_kid0 (wn), const_val);
    assert (is_lhs);
    is_rhs = Divide_Exp_By_Const (WN_kid1 (wn), const_val);
    assert (is_rhs);
    return TRUE;
  case OPR_MPY:
    is_lhs = Divide_Exp_By_Const (WN_kid0 (wn), const_val);
    if (!is_lhs) {
      is_rhs = Divide_Exp_By_Const (WN_kid1 (wn), const_val);
      assert (is_rhs);
      return TRUE;
    }
  case OPR_INTCONST:
    if (((WN_const_val(wn) / const_val) * const_val) == 
      WN_const_val (wn)) {
      WN_const_val (wn) = WN_const_val (wn) / const_val;
      return TRUE;
    } else {
      return FALSE;
    }
  default:
    return FALSE;
  }
}

static WN*
Remove_Consts_From_Conditionals(WN *wn)
{
  // Simplify an expression of the form ax+b .op. ay+c to x .op. y
  // when 0 <= b, c < a
  OPERATOR opr = WN_operator (wn);
  if ((OPR_GT != opr) && (OPR_LT != opr) && (OPR_GE != opr) &&
      (OPR_LE != opr)) 
    return wn;
  assert ((OPR_GT == opr) || (OPR_LT == opr) || (OPR_LE == opr) 
	  || (OPR_GE == opr));
  INT64 lhs_a = -1, rhs_a = -1;
  INT64 lhs_b = -1, rhs_c = -1;
  WN *lhs = WN_kid0 (wn);
  WN *rhs = WN_kid1 (wn);
  WN *rlhs, *llhs, *rllhs, *rrhs, *rlrhs, *lrhs;

  if (OPR_ADD == WN_operator (lhs)) {
    rlhs = WN_kid1 (lhs);
    llhs = WN_kid0 (lhs);
    if (OPR_INTCONST == WN_operator (rlhs))
      lhs_b = WN_const_val (rlhs);
    if (OPR_MPY == WN_operator (llhs)) {
      rllhs = WN_kid1 (llhs);
      if (OPR_INTCONST == WN_operator (rllhs))
	lhs_a = WN_const_val (rllhs);
    }
  } else if (OPR_MPY == WN_operator (lhs)) {
    rlhs = WN_kid1 (lhs);
    if (OPR_INTCONST == WN_operator (rlhs)) {
      lhs_a = WN_const_val (rlhs);
      lhs_b = 0;
    }
  }
  if (OPR_ADD == WN_operator (rhs)) {
    rrhs = WN_kid1 (rhs);
    lrhs = WN_kid0 (rhs);
    if (OPR_INTCONST == WN_operator (rrhs))
      rhs_c = WN_const_val (rrhs);
    if (OPR_MPY == WN_operator (lrhs)) {
      rlrhs = WN_kid1 (lrhs);
      if (OPR_INTCONST == WN_operator (rlrhs))
	rhs_a = WN_const_val (rlrhs);
    }
  } else if (OPR_MPY == WN_operator (rhs)) {
    rrhs = WN_kid1 (rhs);
    if (OPR_INTCONST == WN_operator (rrhs)) {
      rhs_a = WN_const_val (rrhs);
      rhs_c = 0;
    }
  }
  if ((-1 != lhs_a) && (-1 != rhs_a) && (lhs_a == rhs_a) &&
      (0 <= lhs_b) && (lhs_b < lhs_a) && (0 <= rhs_c) &&
      (rhs_c < rhs_a)) {
    if (0 == lhs_b) 
      WN_const_val (rlhs) = 1;
    else {
      WN_const_val (rlhs) = 0;
      WN_const_val (rllhs) = 1;
    }
    if (0 == rhs_c)
      WN_const_val (rrhs) = 1;
    else {
      WN_const_val (rrhs) = 0;
      WN_const_val (rlrhs) = 1;
    }
    return WN_Simplify_Tree (wn);
  } else
    return wn;
}

static WN *
Remove_Floor_From_One_Sided_Cond(WN *wn)
{
  LWN_Parentize (wn);
  OPERATOR opr = WN_operator (wn);
  if (OPR_LT != opr && OPR_GT != opr && OPR_LE != opr && OPR_GE != opr)
    return wn;
  WN *kid0 = WN_kid0 (wn);
  WN *kid1 = WN_kid1 (wn);
  mBOOL kid0_is_floor = FALSE, kid1_is_floor = FALSE;
  if (OPR_INTRINSIC_OP == WN_operator (kid0) &&
      (INTRN_I4DIVFLOOR == WN_intrinsic (kid0) ||
       INTRN_I8DIVFLOOR == WN_intrinsic (kid0) ||
       INTRN_U4DIVFLOOR == WN_intrinsic (kid0) ||
       INTRN_U8DIVFLOOR == WN_intrinsic (kid0)))
    kid0_is_floor = TRUE;
  if (OPR_INTRINSIC_OP == WN_operator (kid1) &&
      (INTRN_I4DIVFLOOR == WN_intrinsic (kid1) ||
       INTRN_I8DIVFLOOR == WN_intrinsic (kid1) ||
       INTRN_U4DIVFLOOR == WN_intrinsic (kid1) ||
       INTRN_U8DIVFLOOR == WN_intrinsic (kid1)))
    kid1_is_floor = TRUE;
  if (kid0_is_floor && kid1_is_floor)
    return wn;
  if (!kid0_is_floor && !kid1_is_floor)
    return wn;
  OPERATOR rev_opr;
  WN *result;
  TYPE_ID rtype = WN_rtype (wn);
  TYPE_ID index_type = Promote_Type (WN_desc (wn));
  if (kid0_is_floor) {
    if (OPR_LT == opr)
      rev_opr = OPR_GT;
    else if (OPR_LE == opr)
      rev_opr = OPR_GE;
    else if (OPR_GE == opr)
      rev_opr = OPR_LE;
    else if (OPR_GT == opr)
      rev_opr = OPR_LT;
    WN *dummy1 = WN_CreateComment ("dummy1");
    WN *dummy2 = WN_CreateComment ("dummy2");
    Replace_WN (kid0, dummy1);
    Replace_WN (kid1, dummy2);
    result = WN_CreateExp2 (OPCODE_make_op (rev_opr,
					    rtype, 
					    index_type),
			    kid1, kid0);
    LWN_Parentize (result);
    if (NULL != LWN_Get_Parent (wn))
      Replace_WN (wn, result);
    LWN_Delete_Tree (wn);
  }
  else
    result = wn;
  // lhs of result must be a non-floor and the rhs must be a floor..
  opr = WN_operator (result);
  WN *pm1;
  OPERATOR new_opr;
  kid0 = WN_kid0 (result);
  kid1 = WN_kid1 (result);
  if (OPR_LT == opr || OPR_GT == opr) {
    if (OPR_LT == opr) {
      pm1 = WN_CreateIntconst (OPCODE_make_op (OPR_INTCONST,
					       index_type,
					       MTYPE_V),
			       (INT64) 1);
      new_opr = OPR_LE;
    } else if (OPR_GT == opr) {
      pm1 = WN_CreateIntconst (OPCODE_make_op (OPR_INTCONST,
					       index_type,
					       MTYPE_V),
			       (INT64) -1);
      new_opr = OPR_GE;
    }
    WN *dummy1 = WN_CreateComment ("dummy1");
    WN *dummy2 = WN_CreateComment ("dummy2");
    Replace_WN (kid0, dummy1);
    Replace_WN (kid1, dummy2);
    WN *add_wn = WN_CreateExp2 (OPCODE_make_op (OPR_ADD,
						index_type,
						MTYPE_V),
				pm1, kid0);
    WN *replace_wn = WN_CreateExp2 (OPCODE_make_op (new_opr,
						    rtype,
						    index_type),
				    add_wn, kid1);
    if (NULL != LWN_Get_Parent (result))
      Replace_WN (result, replace_wn);
    LWN_Delete_Tree (result);
    result = replace_wn;
    LWN_Parentize (result);
    kid0 = WN_kid0 (result);
    kid1 = WN_kid1 (result);
    opr = WN_operator (result);
  }
  FmtAssert (opr == OPR_LE || opr == OPR_GE,
	     ("Invalid op After normalization!"));
  WN *kid10 = WN_kid0 (kid1);
  WN *kid11 = WN_kid1 (kid1);
  FmtAssert (OPR_PARM == WN_operator (kid10),
	     ("Child of intrinsic must be an OPR_PARM"));
  FmtAssert (OPR_PARM == WN_operator (kid11),
	     ("Child of intrinsic must be an OPR_PARM"));
  kid10 = WN_kid0 (kid10);
  kid11 = WN_kid0 (kid11);
  if (OPR_INTCONST != WN_operator (kid11))
    return result;
  INT64 const_val = WN_const_val (kid11);
  if (const_val <= 0)
    return result;
  WN *dummy1 = WN_CreateComment ("dummy1");
  WN *dummy2 = WN_CreateComment ("dummy2");
  Replace_WN (kid0, dummy1);
  WN *const_val_n = 
    WN_CreateIntconst (OPCODE_make_op (OPR_INTCONST, 
				       index_type, MTYPE_V),
		       (INT64) const_val);
  WN *kid0_repl = 
    WN_CreateExp2 (OPCODE_make_op (OPR_MPY, index_type, MTYPE_V),
		   kid0, const_val_n);
  if (OPR_GE == opr) {
    WN *const_val_nm1 = 
      WN_CreateIntconst (OPCODE_make_op (OPR_INTCONST,
					 index_type, MTYPE_V),
			 (INT64) const_val - 1);
    kid0_repl = 
      WN_CreateExp2 (OPCODE_make_op (OPR_ADD, index_type, MTYPE_V),
		     kid0_repl, const_val_nm1);
  }
  Replace_WN (dummy1, kid0_repl);
  LWN_Delete_Tree (dummy1);
  Replace_WN (kid10, dummy2);
  Replace_WN (kid1, kid10);
  LWN_Delete_Tree (kid1);
  return result;
}

static WN *
Convert_Le_With_Floor_2Lt(WN *wn)
{
  LWN_Parentize (wn);
  OPERATOR opr = WN_operator (wn);
  if (OPR_LE != opr)
    return wn;
  WN *kid0 = WN_kid0 (wn);
  WN *kid1 = WN_kid1 (wn);
  if (OPR_INTRINSIC_OP != WN_operator (kid0) ||
      OPR_INTRINSIC_OP != WN_operator (kid1))
    return wn;
  INTRINSIC intrinsic1, intrinsic2;
  intrinsic1 = (INTRINSIC) WN_intrinsic (kid0);
  intrinsic2 = (INTRINSIC) WN_intrinsic (kid1);
  if (intrinsic1 != intrinsic2)
    return wn;
  if ((intrinsic1 != INTRN_I4DIVFLOOR) &&
      (intrinsic1 != INTRN_I8DIVFLOOR) &&
      (intrinsic1 != INTRN_U4DIVFLOOR) &&
      (intrinsic1 != INTRN_U8DIVFLOOR))
    return wn;
  WN *kid00 = WN_kid0 (kid0);
  WN *kid01 = WN_kid1 (kid0);
  WN *kid10 = WN_kid0 (kid1);
  WN *kid11 = WN_kid1 (kid1);
  FmtAssert (OPR_PARM == WN_operator (kid00),
	     ("Child of intrinsic must be an OPR_PARM"));
  FmtAssert (OPR_PARM == WN_operator (kid01),
	     ("Child of intrinsic must be an OPR_PARM"));
  FmtAssert (OPR_PARM == WN_operator (kid10),
	     ("Child of intrinsic must be an OPR_PARM"));
  FmtAssert (OPR_PARM == WN_operator (kid11),
	     ("Child of intrinsic must be an OPR_PARM"));
  kid01 = WN_kid0 (kid01);
  kid11 = WN_kid0 (kid11);
  if (OPR_INTCONST != WN_operator (kid01) ||
      OPR_INTCONST != WN_operator (kid11))
    return wn;
  if ((WN_const_val (kid01) != WN_const_val (kid11)) ||
      (WN_const_val (kid01) <= 0))
    return wn;
  WN *dummy1 = WN_CreateComment ("dummy1");
  TYPE_ID rtype = WN_rtype (kid01), index_type;
  INT64 const_val = WN_const_val (kid01);
  WN *wn_const_val = 
    WN_CreateIntconst (OPCODE_make_op (OPR_INTCONST, rtype,
				       MTYPE_V),
		       const_val);
  kid10 = WN_kid0 (kid10);
  Replace_WN (kid10, dummy1);
  WN *add_expr = 
    WN_CreateExp2 (OPCODE_make_op (OPR_ADD, rtype, MTYPE_V),
		   kid10, wn_const_val);
  Replace_WN (dummy1, add_expr);
  LWN_Delete_Tree (dummy1);
  rtype = WN_rtype (wn);
  index_type = WN_desc (wn);
  WN_set_opcode (wn, OPCODE_make_op (OPR_LT,
				     rtype, index_type));
  return wn;
}
static WN *
Simplify_Cond_With_Floor (WN *wn)
{
  LWN_Parentize (wn);
  OPERATOR opr = WN_operator (wn);
  if (OPR_LE == opr)
    wn = Convert_Le_With_Floor_2Lt(wn);
  opr = WN_operator (wn);
  if ((OPR_LT != opr) && (OPR_GT != opr))
    return wn;
  WN *kid0 = WN_kid0 (wn);
  WN *kid1 = WN_kid1 (wn);
  if (OPR_INTRINSIC_OP != WN_operator (kid0))
    return wn;
  if (OPR_INTRINSIC_OP != WN_operator (kid1))
    return wn;
  INTRINSIC intrinsic1, intrinsic2;
  intrinsic1 = (INTRINSIC) WN_intrinsic (kid0);
  intrinsic2 = (INTRINSIC) WN_intrinsic (kid1);
  if (intrinsic1 != intrinsic2)
    return wn;
  if ((intrinsic1 != INTRN_I4DIVFLOOR) &&
      (intrinsic1 != INTRN_I8DIVFLOOR) &&
      (intrinsic1 != INTRN_U4DIVFLOOR) &&
      (intrinsic1 != INTRN_U8DIVFLOOR))
    return wn;
  WN *kid00 = WN_kid0 (kid0);
  WN *kid01 = WN_kid1 (kid0);
  WN *kid10 = WN_kid0 (kid1);
  WN *kid11 = WN_kid1 (kid1);
  FmtAssert (OPR_PARM == WN_operator (kid00),
	     ("Child of intrinsic must be an OPR_PARM"));
  FmtAssert (OPR_PARM == WN_operator (kid01),
	     ("Child of intrinsic must be an OPR_PARM"));
  FmtAssert (OPR_PARM == WN_operator (kid10),
	     ("Child of intrinsic must be an OPR_PARM"));
  FmtAssert (OPR_PARM == WN_operator (kid11),
	     ("Child of intrinsic must be an OPR_PARM"));
  kid01 = WN_kid0 (kid01);
  kid11 = WN_kid0 (kid11);
  if (OPR_INTCONST != WN_operator (kid01) ||
      OPR_INTCONST != WN_operator (kid11))
    return wn;
  if ((WN_const_val (kid01) != WN_const_val (kid11)) ||
      (WN_const_val (kid01) <= 0))
    return wn;
  WN *dummy1 = WN_CreateComment ("dummy1");
  WN *dummy2 = WN_CreateComment ("dummy2");
  kid00 = WN_kid0 (kid00);
  kid10 = WN_kid0 (kid10);
  Replace_WN (kid00, dummy1);
  Replace_WN (kid10, dummy2);
  Replace_WN (kid0, kid00);
  Replace_WN (kid1, kid10);
  LWN_Delete_Tree (kid0);
  LWN_Delete_Tree (kid1);
  return WN_Simplify_Tree (wn);
}

static WN*
Toggle_Eq_To_Remove_One(WN *wn)
{
  OPERATOR opr = WN_operator (wn);
  if ((OPR_LT != opr) && (OPR_LE != opr) && (OPR_GE != opr) &&
      (OPR_GT != opr)) 
    return wn;
  WN *kid0 = WN_kid0 (wn);
  WN *kid1 = WN_kid1 (wn);
  WN *const_node;
  INT64 const_val;
  if (OPR_ADD != WN_operator (kid0) ||
      OPR_ADD != WN_operator (kid1))
    return wn;
  mBOOL llhs_is1, rlhs_is1, lrhs_is1, rrhs_is1;

  WN      *kid10      = WN_kid1 (kid0);
  WN      *kid00      = WN_kid0 (kid0);
  WN      *kid11      = WN_kid1 (kid1);
  WN      *kid01      = WN_kid0 (kid1);
  TYPE_ID  rtype      = WN_rtype (wn);
  TYPE_ID  index_type = WN_desc (wn);

  if (OPR_INTCONST == WN_operator (kid00)) {
    if (OPR_INTCONST == WN_operator (kid01)) {
      WN_const_val (kid01) -= WN_const_val (kid00);
      WN_const_val (kid00) = 0;
    } 
    else if (OPR_INTCONST == WN_operator (kid11)) {
      WN_const_val (kid11) -= WN_const_val (kid00);
      WN_const_val (kid00) = 0;
    }
  }
  else if (OPR_INTCONST == WN_operator (kid10)) {
    if (OPR_INTCONST == WN_operator (kid01)) {
      WN_const_val (kid01) -= WN_const_val (kid10);
      WN_const_val (kid10) = 0;
    }
    else if (OPR_INTCONST == WN_operator (kid11)) {
      WN_const_val (kid11) -= WN_const_val (kid10);
      WN_const_val (kid10) = 0;
    }
  }
  llhs_is1 = ((OPR_INTCONST == WN_operator (kid00)) 
	      && (1 == WN_const_val (kid00)));
  rlhs_is1 = ((OPR_INTCONST == WN_operator (kid10))
	      && (1 == WN_const_val (kid10)));
  lrhs_is1 = ((OPR_INTCONST == WN_operator (kid01))
	      && (1 == WN_const_val (kid01)));
  rrhs_is1 = ((OPR_INTCONST == WN_operator (kid11))
	      && (1 == WN_const_val (kid11)));
  if ((llhs_is1 || rlhs_is1) && (OPR_LE == opr)) {
    WN_set_opcode (wn, 
		   OPCODE_make_op (OPR_LT, 
				   rtype, index_type));
    if (llhs_is1)
      WN_const_val (kid00) = 0;
    else if (rlhs_is1)
      WN_const_val (kid10) = 0;
    return WN_Simplify_Tree (wn);
  } else if ((lrhs_is1 || rrhs_is1) && (OPR_GE == opr)) {
    WN_set_opcode (wn, 
		   OPCODE_make_op (OPR_GT, 
				   rtype, index_type));
    if (lrhs_is1)
      WN_const_val (kid01) = 0;
    else if (rrhs_is1)
      WN_const_val (kid11) = 0;
    return WN_Simplify_Tree (wn);
  } else
    return WN_Simplify_Tree (wn);
}

WN*
Simplify_If_Conditional(WN *wn)
{
  wn = Simplify_Cond_With_Floor (wn);
  BOOL disable_simp = WN_Simplifier_Enable (FALSE);
  wn = Remove_Floor_From_One_Sided_Cond (wn);
  WN_Simplifier_Enable (disable_simp);
  wn = WN_Simplify_Tree (wn);
  wn = Toggle_Eq_To_Remove_One (wn);
  wn = Remove_Consts_From_Conditionals (wn);
  OPERATOR opr = WN_operator (wn);
  if ((OPR_LT != opr) && (OPR_LE != opr) && (OPR_GE != opr) &&
      (OPR_GT != opr))
    return wn;
  WN *kid0 = WN_kid0 (wn);
  WN *kid1 = WN_kid1 (wn);
  WN *const_node;
  INT64 const_val = 0;
  WN *other;
  // at least one of them is an multiply.
  if (OPR_MPY == WN_operator (kid0)) {
    if (OPR_INTCONST == 
	WN_operator (WN_kid0 (kid0))) {
      const_val = WN_const_val (WN_kid0 (kid0));
      other = kid1;
    }
    else if (OPR_INTCONST == 
	     WN_operator (WN_kid1 (kid0))) {
      const_val = WN_const_val (WN_kid1 (kid0));
      other = kid1;
    }
  } else if (OPR_MPY == WN_operator (kid1)) {
    if (OPR_INTCONST == 
	WN_operator (WN_kid0 (kid1))) {
      const_val = WN_const_val (WN_kid0 (kid1));
      other = kid0;
    }
    else if (OPR_INTCONST == 
	    WN_operator (WN_kid1 (kid1))) {
      const_val = WN_const_val (WN_kid1 (kid1));
      other = kid0;
    }
  }
  if (0 == const_val) 
    return wn;
  if (!Is_Exp_Divided_By_Const (other, const_val))
    return wn;
  Divide_Exp_By_Const (kid0, const_val);
  Divide_Exp_By_Const (kid1, const_val);
  return wn;
}

WN *
Sh_LWN_CreateDivceil (TYPE_ID type,
		      WN *lhs,
		      WN *rhs)
{
  if (OPR_INTCONST != 
      WN_operator (rhs)) 
    return LWN_CreateDivceil (type, lhs, rhs);
  INT64 const_val = WN_const_val (rhs);
  if (OPR_ADD == WN_operator (lhs)) {
    WN *llhs, *rlhs, *rllhs;
    
    llhs = WN_kid0 (lhs);
    rlhs = WN_kid1 (lhs);
    if (OPR_INTCONST != WN_operator (rlhs)) 
      return LWN_CreateDivceil (type, lhs, rhs);
    INT64 const_val2 = WN_const_val (rlhs);
    if (OPR_MPY != WN_operator (llhs))
      return LWN_CreateDivceil (type, lhs, rhs);
    rllhs = WN_kid1 (llhs);
    if (OPR_INTCONST != WN_operator (rllhs))
      return LWN_CreateDivceil (type, lhs, rhs);
    INT64 const_val3 = WN_const_val (rllhs);
    if (0 != const_val3 % const_val)
      return LWN_CreateDivceil (type, lhs, rhs);
    WN_const_val (rllhs) = const_val3 / const_val;
    if (const_val2 > 0) 
      WN_const_val (rlhs) = const_val2 / const_val + 1;
    else 
      WN_const_val (rlhs) = - (-const_val2 / const_val);
    LWN_Delete_Tree (rhs);
    return WN_Simplify_Tree (lhs);
  }
  if ((OPR_MPY != WN_operator (lhs)) || 
      (const_val <= 0))
    return LWN_CreateDivceil (type, lhs, rhs);
  WN *l_lhs = WN_kid0 (lhs);
  WN *r_lhs = WN_kid1 (lhs);
  if ((OPR_INTCONST != WN_operator (l_lhs)) &&
      (OPR_INTCONST != WN_operator (r_lhs)))
    return LWN_CreateDivceil (type, lhs, rhs);
  if ((OPR_INTCONST == WN_operator (l_lhs)) &&
      (OPR_INTCONST == WN_operator (r_lhs))) {
    INT64 result = 
      1 + (((WN_const_val (l_lhs)*WN_const_val (r_lhs))-1)/const_val);
    LWN_Delete_Tree (lhs);
    LWN_Delete_Tree (rhs);
    return 
      WN_CreateIntconst (OPCODE_make_op (OPR_INTCONST, 
					 type, MTYPE_V),
			 (INT64) result);
  }
  else if (OPR_INTCONST == WN_operator (r_lhs)) {
    BOOL divides = 
      (0 == (WN_const_val (r_lhs) % const_val));
    if (divides) {
      LWN_Delete_Tree (rhs);
      WN_const_val (r_lhs) = WN_const_val (r_lhs) / const_val;
      return WN_Simplify_Tree (lhs);
    }
    else 
      return LWN_CreateDivceil (type, lhs, rhs);
  } else {
    BOOL divides = (0 == (WN_const_val (l_lhs) % const_val));
    if (divides) {
      LWN_Delete_Tree (rhs);
      WN_const_val (l_lhs) = WN_const_val (l_lhs) / const_val;
      return WN_Simplify_Tree (lhs);
    }
    else
      return LWN_CreateDivceil (type, lhs, rhs);
  }
}

INT64
Int_DivFloor (INT64 num, INT64 denom)
{
  if ((num > 0) && (denom > 0))
    return (num/denom);
  else if (num == 0)
    return 0;
  else if ((num < 0) && (denom > 0)) 
    return -1 - ((-num - 1) / denom);
  else {
    FmtAssert (denom > 0, ("Denominator must be positive"));
    return -1;
  }
}
       
WN *
Sh_LWN_CreateDivfloor (TYPE_ID type,
		      WN *lhs,
		      WN *rhs)
{
  if (OPR_INTCONST != 
      WN_operator (rhs)) 
    return LWN_CreateDivfloor (type, lhs, rhs);
  INT64 const_val = WN_const_val (rhs);
  if ((OPR_MPY != WN_operator (lhs)) || 
      (const_val <= 0))
    return LWN_CreateDivfloor (type, lhs, rhs);
  WN *l_lhs = WN_kid0 (lhs);
  WN *r_lhs = WN_kid1 (lhs);
  if ((OPR_INTCONST != WN_operator (l_lhs)) &&
      (OPR_INTCONST != WN_operator (r_lhs)))
    return LWN_CreateDivfloor (type, lhs, rhs);
  if ((OPR_INTCONST == WN_operator (l_lhs)) &&
      (OPR_INTCONST == WN_operator (r_lhs))) {
    INT64 result = 
      (WN_const_val (l_lhs) * WN_const_val (r_lhs)) / const_val;
    LWN_Delete_Tree (lhs);
    LWN_Delete_Tree (rhs);
    return 
      WN_CreateIntconst (OPCODE_make_op (OPR_INTCONST, 
					 type, MTYPE_V),
			 (INT64) result);
  }
  else if (OPR_INTCONST == WN_operator (r_lhs)) {
    BOOL divides = 
      (0 == (WN_const_val (r_lhs) % const_val));
    if (divides) {
      LWN_Delete_Tree (rhs);
      WN_const_val (r_lhs) = WN_const_val (r_lhs) / const_val;
      return WN_Simplify_Tree (lhs);
    }
    else 
      return LWN_CreateDivfloor (type, lhs, rhs);
  } else {
    BOOL divides = (0 == (WN_const_val (l_lhs) % const_val));
    if (divides) {
      LWN_Delete_Tree (rhs);
      WN_const_val (l_lhs) = WN_const_val (l_lhs) / const_val;
      return WN_Simplify_Tree (lhs);
    }
    else
      return LWN_CreateDivfloor (type, lhs, rhs);
  }
}
      
       
// Given loop index constraints lb and ub, find out if 
// cond is trivially true. i.e. lb /\ ub => cond
static BOOL
is_vector_trivial(ACCESS_ARRAY  *lb,
		  ACCESS_ARRAY  *ub,
		  ACCESS_VECTOR *cond,
		  INT32          size_loop,
		  INT32          size_sym,
		  QUEUE<ST *>   *lin_sym_q,
		  WN            *do_loop)
{
  INT32 size = size_loop + size_sym, index;

  mINT32 *row = CXX_NEW_ARRAY (mINT32, size, shackle_if_pool);
  ACCESS_VECTOR *dup = 
    CXX_NEW (ACCESS_VECTOR (cond, shackle_if_pool),
	     shackle_if_pool);
  INT32 i, j;
  SYSTEM_OF_EQUATIONS *soe;
  ACCESS_VECTOR *v;

  dup->Negate_Me();
  dup->Const_Offset--;
  
  soe = CXX_NEW (SYSTEM_OF_EQUATIONS (0, 0, size, shackle_if_pool),
		 shackle_if_pool);
  for (i = 0; i < lb->Num_Vec(); i++) {
    v = lb->Dim(i);
    assert (!v->Too_Messy);
    for (j = 0; j < size; j++) 
      row[j] = v->Loop_Coeff (j);
    if (NULL != v->Lin_Symb) {
      INTSYMB_CONST_ITER iter(v->Lin_Symb);
      for (const INTSYMB_NODE *node = iter.First();
	   !iter.Is_Empty();
	   node = iter.Next()) {
	if (0 == node->Coeff) 
	  DevWarn("Access vector has zero coeff linear symbol");
	else {
	  index = lin_sym_q->Index (node->Symbol.St());
	  assert ((0 <= index) && (index < size_sym));
	  row[size_loop+index] = node->Coeff;
	}
      }
    }
    soe->Add_Le (row, (INT64) v->Const_Offset);
  }
  for (i = 0; i < ub->Num_Vec(); i++) {
    v = ub->Dim(i);
    assert (!v->Too_Messy);
    for (j = 0; j < size; j++) 
      row[j] = v->Loop_Coeff (j);
    if (NULL != v->Lin_Symb) {
      INTSYMB_CONST_ITER iter(v->Lin_Symb);
      for (const INTSYMB_NODE *node = iter.First();
	   !iter.Is_Empty();
	   node = iter.Next()) {
	if (0 == node->Coeff)
	  DevWarn("Access vector has zero coeff linear symbol");
	else {
	  index = lin_sym_q->Index (node->Symbol.St());
	  assert ((0 <= index) && (index < size_sym));
	  row[size_loop+index] = node->Coeff;
	}
      }
    }
    soe->Add_Le (row, (INT64) v->Const_Offset);
  }
  for (j = 0; j < size; j++)
    row[j] = dup->Loop_Coeff (j);
  if (NULL != dup->Lin_Symb) {
    INTSYMB_CONST_ITER iter(dup->Lin_Symb);
    for (const INTSYMB_NODE *node = iter.First();
	 !iter.Is_Empty();
	 node = iter.Next()) {
      if (0 == node->Coeff)
	DevWarn("Access vector has zero coeff linear symbol");
      else {
	index = lin_sym_q->Index (node->Symbol.St());
	assert ((0 <= index) && (index < size_sym));
	row[size_loop+index] = node->Coeff;
      }
    }
  }
  soe->Add_Le (row, dup->Const_Offset);
  Add_Parent_If_Constraints (do_loop, soe, size_loop,
			     size_sym, lin_sym_q);
  Add_Parent_Loop_Constraints (do_loop, soe, size_loop, 
			       size_sym, lin_sym_q);
  if (shackle_if_debug_level > 1) 
    soe->Print (stdout);
  return !soe->Is_Consistent();
}


// Given loop index constraints lb and ub, find out if 
// cond is inconsistent . i.e. lb /\ ub /\ cond = false
static BOOL
is_vector_inconsistent(ACCESS_ARRAY  *lb,
		       ACCESS_ARRAY  *ub,
		       ACCESS_VECTOR *cond,
		       INT32          size_loop,
		       INT32          size_sym,
		       QUEUE<ST*>    *lin_sym_q,
		       WN            *do_loop)
{
  INT32 size = size_loop + size_sym, index;

  mINT32 *row = CXX_NEW_ARRAY (mINT32, size, shackle_if_pool);
  ACCESS_VECTOR *dup = 
    CXX_NEW (ACCESS_VECTOR (cond, shackle_if_pool),
	     shackle_if_pool);
  INT32 i, j;
  SYSTEM_OF_EQUATIONS *soe;
  ACCESS_VECTOR *v;

  soe = CXX_NEW (SYSTEM_OF_EQUATIONS (0, 0, size, shackle_if_pool),
		 shackle_if_pool);
  for (i = 0; i < lb->Num_Vec(); i++) {
    v = lb->Dim(i);
    FmtAssert (!v->Too_Messy, ("How could it be?"));
    for (j = 0; j < size; j++) 
      row[j] = v->Loop_Coeff (j);
    if (NULL != v->Lin_Symb) {
      INTSYMB_CONST_ITER iter(v->Lin_Symb);
      for (const INTSYMB_NODE *node = iter.First();
	   !iter.Is_Empty();
	   node = iter.Next()) {
	if (0 == node->Coeff)
	  DevWarn("Access vector has zero coeff linear symbol");
	else {
	  index = lin_sym_q->Index (node->Symbol.St());
	  assert ((0 <= index) && (index < size_sym));
	  row[size_loop+index] = node->Coeff;
	}
      }
    }
    soe->Add_Le (row, (INT64) v->Const_Offset);
  }
  for (i = 0; i < ub->Num_Vec(); i++) {
    v = ub->Dim(i);
    assert (!v->Too_Messy);
    for (j = 0; j < size; j++) 
      row[j] = v->Loop_Coeff (j);
    if (NULL != v->Lin_Symb) {
      INTSYMB_CONST_ITER iter(v->Lin_Symb);
      for (const INTSYMB_NODE *node = iter.First();
	   !iter.Is_Empty();
	   node = iter.Next()) {
	if (0 == node->Coeff) 
	  DevWarn("Access vector has zero coeff linear symbol");
	else {
	  index = lin_sym_q->Index (node->Symbol.St());
	  assert ((0 <= index) && (index < size_sym));
	  row[size_loop+index] = node->Coeff;
	}
      }
    }
    soe->Add_Le (row, (INT64) v->Const_Offset);
  }
  for (j = 0; j < size; j++)
    row[j] = dup->Loop_Coeff (j);
  if (NULL != dup->Lin_Symb) {
    INTSYMB_CONST_ITER iter(dup->Lin_Symb);
    for (const INTSYMB_NODE *node = iter.First();
	 !iter.Is_Empty();
	 node = iter.Next()) {
      if (0 == node->Coeff)
	DevWarn("Access vector has zero coeff linear symbol");
      else {
	index = lin_sym_q->Index (node->Symbol.St());
	FmtAssert (0 <= index && index < size_sym, 
		   ("Incorrect value for index"));
	row[size_loop+index] = node->Coeff;
      }
    }
  }
  soe->Add_Le (row, dup->Const_Offset);
  Add_Parent_If_Constraints (do_loop, soe, size_loop,
			     size_sym, lin_sym_q);
  Add_Parent_Loop_Constraints (do_loop, soe, size_loop, 
			       size_sym, lin_sym_q);
  if (shackle_if_debug_level > 1) 
    soe->Print (stdout);
  return !soe->Is_Consistent();
}

static BOOL
Interferes_With_Symbolic_Bound(ACCESS_VECTOR *if_test,
			       INT32          pos,
			       ACCESS_ARRAY  *lb,
			       ACCESS_ARRAY  *ub)
{
  INT32          i;
  ACCESS_VECTOR *tmp;

  assert (0 != if_test->Loop_Coeff (pos));
  if (if_test->Loop_Coeff (pos) < 0) { // a lower bound
    for (i = 0; i < lb->Num_Vec(); i++) {
      tmp = lb->Dim(i);
      if (NULL != tmp->Lin_Symb)
	return TRUE;
    }
    return FALSE;
  } else {
    for (i = 0; i < ub->Num_Vec(); i++) {
      tmp = ub->Dim(i);
      if (NULL != tmp->Lin_Symb)
	return TRUE;
    }
    return FALSE;
  }
}

static void
copy_access_array_from_src2dst(ACCESS_ARRAY *dst,
			       ACCESS_ARRAY *src,
			       INT32         depth)
{
  INT32             i, j;
  ACCESS_VECTOR    *v1, *v2;

  assert (dst->Num_Vec() <= src->Num_Vec());
  for (i = 0; i < dst->Num_Vec(); i++ ) {
    v1 = dst->Dim(i);
    v2 = src->Dim(i);
    assert (depth <= v1->Nest_Depth());
    assert (depth <= v2->Nest_Depth());
    for (j = 0; j < depth; j++) {
      v1->Set_Loop_Coeff (j, v2->Loop_Coeff (j));
    }
    v1->Const_Offset = v2->Const_Offset;
  }
  return;
}

static INT64
determine_if_sinkable_in_do(WN *if_stmt,
			    WN *do_stmt)
{
  IF_INFO *if_info = Get_If_Info (if_stmt);
  assert (NULL != if_info);
  ACCESS_ARRAY *ar = if_info->Condition;
  ACCESS_VECTOR *v1, *step;
  WN *wn1, *wn2, *kid, *add0, *add1;
  ST *do_index;
  DO_LOOP_INFO *dli;

  // If the Condition has more than one conjunct, we don't sink
  if (1 != ar->Num_Vec())
    return 0;
  v1 = ar->Dim(0);
  // if v1 is too messy, then quit
  if (v1->Too_Messy)
    return 0;
  // If v1 contains non linear symbols, just quit
  if (v1->Contains_Non_Lin_Symb())
    return 0;
  // We have only affines and constants.
  // Next, we check if the do loop has a positive constant step 
  dli = Get_Do_Loop_Info (do_stmt);
  step = dli->Step;
  if (step->Is_Const()) {
    if (step->Const_Offset > 0)
      return step->Const_Offset;
    else 
      return 0;
  }
  else
    return 0;
}

static WN *
Enclosing_Ith_Do_Loop (WN *stmt, INT32 level)
{
  WN    *loop;
  INT32  i;

  loop = stmt;
  for (i = 0; i < level; i++) {
    // For some reason, enclosing_do_loop returns the current
    // node as the enclosing do loop if the node itself is a 
    // do loop. So, we have to have the hack of taking the 
    // parent before getting the enclosing loop
    loop = LWN_Get_Parent (loop);
    loop = Enclosing_Do_Loop (loop);
    if (NULL == loop)
      return NULL;
  }
  return loop;
}

/* Takes an if condition in a certain form and returns an
   expression which is equivalent to the first in the sense that 
   result <= 0 is equivalent to the first condition */
static WN*
canonicalize_if_condition(WN *cond, INT32 depth)
{
  WN *fake_unroll[2];
  if (!OPCODE_is_compare (WN_opcode (cond)))
    return NULL;
  Simplify_Cond_With_Div_Floor (cond);

  OPERATOR  opr = WN_operator (cond);
  TYPE_ID   index_type;
  WN       *wnm1, *tmp1, *tmp2, *unchng, *tmp3;

  if ((OPR_GE != opr) && (OPR_GT != opr) && (OPR_LE != opr) &&
      (OPR_LT != opr))
    return NULL;
  if ((OPR_LE == opr) || (OPR_LT == opr)) {
    unchng = LWN_Copy_Tree (WN_kid0(cond));
    fake_unroll[0] = WN_kid0 (cond);
    fake_unroll[1] = unchng;
    Unrolled_DU_Update (fake_unroll, 2, depth);

    tmp1 = LWN_Copy_Tree (WN_kid1(cond));
    fake_unroll[0] = WN_kid1(cond);
    fake_unroll[1] = tmp1;
    Unrolled_DU_Update (fake_unroll, 2, depth);
  }
  else {
    assert ((OPR_GT == opr) || (OPR_GE == opr));

    unchng = LWN_Copy_Tree (WN_kid1 (cond));
    fake_unroll[0] = WN_kid1 (cond);
    fake_unroll[1] = unchng;
    Unrolled_DU_Update (fake_unroll, 2, depth);

    tmp1 = LWN_Copy_Tree (WN_kid0 (cond));
    fake_unroll[0] = WN_kid0(cond);
    fake_unroll[1] = tmp1;
    Unrolled_DU_Update (fake_unroll, 2, depth);
  }
  index_type = WN_rtype (tmp1);
  wnm1 = WN_CreateIntconst 
    (OPCODE_make_op (OPR_INTCONST, Promote_Type (index_type),
		     MTYPE_V),
     (INT64) 1);
  if ((OPR_GT == opr) || (OPR_LT == opr)) {
    tmp2 = WN_CreateExp2
      (OPCODE_make_op (OPR_ADD, Promote_Type (index_type),
		       MTYPE_V),
       unchng, wnm1);
    // LWN_Set_Parent (unchng, tmp2);
    // LWN_Set_Parent (wnm1, tmp2);
  }
  else
    tmp2 = unchng;
  index_type = WN_rtype (tmp1);
  tmp3 = WN_CreateExp1
    (OPCODE_make_op (OPR_NEG, Promote_Type (index_type),
		     MTYPE_V),
     tmp1);
  // LWN_Set_Parent (tmp1, tmp3);
  tmp1 = WN_CreateExp2 
    (OPCODE_make_op (OPR_ADD, Promote_Type (index_type),
		     MTYPE_V),
     tmp3, tmp2);
  // LWN_Set_Parent (tmp3, tmp1);
  // LWN_Set_Parent (tmp2, tmp1);
  return tmp1;
}

INT32
Return_Floor_Given_Ceil(INT32 inp)
{
  switch (inp) {
  case INTRN_I4DIVCEIL:
    return INTRN_I4DIVFLOOR;
  case INTRN_U4DIVCEIL:
    return INTRN_U4DIVFLOOR;
  case INTRN_I8DIVCEIL:
    return INTRN_I8DIVFLOOR;
  case INTRN_U8DIVCEIL:
    return INTRN_U8DIVFLOOR;
  default:
    FmtAssert (0, ("Must pass an INTRN Div/Floor as argument"));
    return -1;
  }
}

// We add 1 to the input expression and return it 
static WN*
return_upper_boundplus1(WN *upper_bound, INT32 depth)
{
  WN        *wnp1, *tmp;
  WN        *fake_unroll[2];
  WN        *dup;
  TYPE_ID    index_type;

  if (OPR_INTRINSIC_OP == 
      WN_operator (upper_bound)) {
    INT32 intr = WN_intrinsic(upper_bound);
    if ((intr == INTRN_I4DIVFLOOR) || (intr == INTRN_I8DIVFLOOR)||
	(intr == INTRN_U4DIVFLOOR) || (intr == INTRN_U8DIVFLOOR)) {
      WN *const_kid = WN_kid0 (WN_kid1 (upper_bound));
      if ((OPR_INTCONST == WN_operator(const_kid)) &&
	  (WN_const_val (const_kid) > 0)) {
	WN *replace = WN_kid0 (WN_kid0 (upper_bound));
	dup = LWN_Copy_Tree (replace);
	fake_unroll[0] = replace;
	fake_unroll[1] = dup;
	Unrolled_DU_Update (fake_unroll, 2, depth);
	index_type = WN_rtype (upper_bound);
	WN *incr = WN_CreateIntconst 
	  (OPCODE_make_op (OPR_INTCONST, Promote_Type (index_type),
			   MTYPE_V),
	   (INT64) WN_const_val (const_kid));
	WN *replace_by = WN_CreateExp2
	  (OPCODE_make_op (OPR_ADD, Promote_Type (index_type),
			   MTYPE_V),
	   dup, incr);
	Replace_WN (replace, replace_by);
	LWN_Delete_Tree (replace);
	return upper_bound;
      }
    } else if ((intr == INTRN_I4DIVCEIL)  || (intr == INTRN_I8DIVCEIL) ||
	       (intr == INTRN_U4DIVCEIL)  || (intr == INTRN_U8DIVCEIL)) {
      WN *const_kid = WN_kid0 (WN_kid1 (upper_bound));
      if ((OPR_INTCONST == WN_operator (const_kid)) &&
	  (WN_const_val (const_kid) > 0)) {
	WN *old = WN_kid0 (WN_kid0 (upper_bound));
	dup = LWN_Copy_Tree (old);
	fake_unroll[0] = old;
	fake_unroll[1] = dup;
	Unrolled_DU_Update (fake_unroll, 2, depth);
	index_type = WN_rtype (upper_bound);
	WN *incr = WN_CreateIntconst 
	  (OPCODE_make_op (OPR_INTCONST, Promote_Type (index_type),
			   MTYPE_V),
	   (INT64) (2*WN_const_val(const_kid) - 1));
	WN *new_num = WN_CreateExp2
	  (OPCODE_make_op (OPR_ADD, Promote_Type (index_type),
			   MTYPE_V),
	   dup, incr);
	WN *new_denom = WN_CreateIntconst
	  (OPCODE_make_op (OPR_INTCONST, Promote_Type (index_type),
			   MTYPE_V),
	   (INT64) WN_const_val (const_kid));
	LWN_Delete_Tree (upper_bound);
	return Sh_LWN_CreateDivfloor (Promote_Type (index_type),
				      new_num, new_denom);
      }
    }
  }
  dup = upper_bound;
  index_type = WN_rtype (upper_bound);
  wnp1 = WN_CreateIntconst 
    (OPCODE_make_op (OPR_INTCONST, Promote_Type (index_type),
		     MTYPE_V),
     (INT64) 1);
  tmp = WN_CreateExp2 
    (OPCODE_make_op (OPR_ADD, Promote_Type (index_type),
		     MTYPE_V),
     dup, wnp1);
  // LWN_Set_Parent (dup, tmp);
  // LWN_Set_Parent (wnp1, tmp);
  return WN_Simplify_Tree (tmp);
}
     
/* Takes a canonicalized if condition and returns a lower bound */
static WN*
return_upper_bound(WN    *canonical_inp,
		   SYMBOL symbol_to_remove,
		   INT32  loop_coeff,
		   BOOL   is_coeff_pos)
{
  TYPE_ID                index_type;

  // because of a bug in replace_ldid_with_exp_copy, we short-ckt
  // the following piece of code.
  if (OPR_LDID == WN_operator (canonical_inp)) {
    if (symbol_to_remove == SYMBOL (canonical_inp)) {
      index_type = WN_rtype (canonical_inp);
      LWN_Delete_Tree (canonical_inp);
      return WN_CreateIntconst
	(OPCODE_make_op (OPR_INTCONST, Promote_Type (index_type),
			 MTYPE_V),
	 (INT64) 0);
    }
  }
  /* First construct a tree removing from it all occuruences
     of symbol_to_remove */
  WN                    *wn, *wnm1, *zero;
  WN                    *dup, *return_val;
  WN                    *fake_unroll[2];

  LWN_Parentize (canonical_inp);
  dup = canonical_inp;
  index_type = WN_rtype (dup);
  
  zero = WN_CreateIntconst
    (OPCODE_make_op (OPR_INTCONST, Promote_Type (index_type),
		     MTYPE_V),
     (INT64) 0);
  Replace_Ldid_With_Exp_Copy (symbol_to_remove, 
			      dup,
			      zero);
  dup = WN_Simplify_Tree (dup);
  index_type = WN_rtype (dup);
  // If the loop_coeff is 1, then, do division is necessary
  if ((1 == loop_coeff) || (-1 == loop_coeff)) {
    if (!is_coeff_pos) {
      wnm1 = WN_CreateIntconst
	(OPCODE_make_op (OPR_INTCONST, Promote_Type (index_type),
			 MTYPE_V),
	 (INT64) -1);
      return_val =  WN_CreateExp2
	(OPCODE_make_op (OPR_ADD, Promote_Type (index_type),
			 MTYPE_V),
	 dup, wnm1);
      // LWN_Set_Parent (dup, return_val);
      // LWN_Set_Parent (wnm1, return_val);
      return WN_Simplify_Tree (return_val);
    }
    else {
      return_val = WN_CreateExp1
	(OPCODE_make_op (OPR_NEG, Promote_Type (index_type),
			 MTYPE_V),
	 dup);
      // LWN_Set_Parent (dup, return_val);
      return WN_Simplify_Tree (return_val);
    }
  }
  else { // division required. ugh!
    // We do a divfloor operation and return-the result
    UINT32 divisor = (loop_coeff > 0) ? loop_coeff : -loop_coeff;
    assert (divisor > 0);
    WN *div_result = WN_CreateIntconst 
      (OPCODE_make_op (OPR_INTCONST, Promote_Type (index_type),
		       MTYPE_V),
       divisor);
    WN *tmp_result;
    if (!is_coeff_pos) {
      wnm1 = WN_CreateIntconst 
	(OPCODE_make_op (OPR_INTCONST, Promote_Type (index_type),
			 MTYPE_V),
	 (INT64) -1);
      tmp_result = WN_CreateExp2
	(OPCODE_make_op (OPR_ADD, Promote_Type (index_type),
			 MTYPE_V),
	 dup, wnm1);
      return Sh_LWN_CreateDivfloor (Promote_Type (index_type),
				    tmp_result, div_result);
      /*
	tmp_result = Sh_LWN_CreateDivceil (Promote_Type (index_type),
	dup, div_result);
	return 
	WN_Simplify_Tree (WN_CreateExp2 
	(OPCODE_make_op (OPR_ADD, Promote_Type (index_type),
	MTYPE_V),
	tmp_result, wnm1));
	*/
    }
    else {
      tmp_result = WN_CreateExp1 
	(OPCODE_make_op (OPR_NEG, Promote_Type (index_type),
			 MTYPE_V),
	 WN_Simplify_Tree (dup));
      tmp_result = WN_Simplify_Tree (tmp_result);
      return 
	WN_Simplify_Tree (Sh_LWN_CreateDivfloor (Promote_Type (index_type),
						 tmp_result, div_result));
    }
  }
}

BOOL
Is_Parent_Of(WN *parent, WN *kid)
{
  WN *step = kid;
  while (NULL != step) {
    if (step == parent)
      return TRUE;
    else
      step = LWN_Get_Parent (step);
  }
  return FALSE;
}

// This function removes all empty if statements, and 
// empty do statements which do not have a usage of the
// loop index variable outside the do loop and which
// have an empty body
static WN *
Largest_Empty_Subtree(WN *wn)
{
  WN *wn_then, *wn_else, *wn_body, *wn_kid;
  WN *wn_index, *last_child_saved, *delete_stmt;
  USE_LIST *use_list;
  USE_LIST_ITER *iter1, *iter2;

  switch (WN_opcode (wn)) {
  case OPC_IF:
    wn_then = Largest_Empty_Subtree (WN_then (wn));
    wn_else = Largest_Empty_Subtree (WN_else (wn));
    if ((WN_then (wn) == wn_then) &&
	(WN_else (wn) == wn_else)) 
      return wn;
    else if (WN_then (wn) == wn_then)
      return wn_else;
    else 
      return wn_then;
  case OPC_DO_LOOP:
    {
      wn_body = Largest_Empty_Subtree (WN_do_body (wn));
      if (wn_body != WN_do_body (wn)) 
	return wn_body;
      wn_index = WN_start (wn);
      assert (OPR_STID == WN_operator (wn_index));
      use_list = Du_Mgr->Du_Get_Use (wn_index);
      iter1 = CXX_NEW (USE_LIST_ITER (use_list), shackle_if_pool);
      for (const DU_NODE *node1 = iter1->First();
	   !iter1->Is_Empty(); node1 = iter1->Next()) {
	WN *use = node1->Wn();
	// If there is a use outside the body, can't delete
	if (!Is_Parent_Of (wn, use)) 
	  return wn_body;
      }
      wn_index = WN_step (wn);
      assert (OPR_STID == WN_operator (wn_index));
      use_list = Du_Mgr->Du_Get_Use (wn_index);
      iter2 = CXX_NEW (USE_LIST_ITER (use_list), shackle_if_pool);
      for (const DU_NODE *node2 = iter2->First();
	   !iter2->Is_Empty(); node2 = iter2->Next()) {
	WN *use = node2->Wn();
	// If there is a use outside the body, can't delete
	if (!Is_Parent_Of (wn, use)) 
	  return wn_body;
      }
      // Must be the case that the body is empty, and there are
      // no uses from the loop body to outside
      return wn;
    }
  case OPC_BLOCK:
    if (WN_block_empty (wn)) 
      return wn;
    FOR_CHILDREN (wn, child, ignCount) {
      wn_kid = Largest_Empty_Subtree (child);
      // If there is actually something to delete
      if (wn_kid) {
	delete_stmt = LWN_Extract_From_Block (wn_kid);
	LWN_Delete_Tree (delete_stmt);
      }
    }
    END_CHILDREN;
    if (WN_block_empty (wn))
      return wn;
    else
      return NULL;
  case OPC_FUNC_ENTRY:
    wn_kid = Largest_Empty_Subtree (WN_func_body (wn));
    if (wn_kid) {
      delete_stmt = LWN_Extract_From_Block (wn_kid);
      LWN_Delete_Tree (delete_stmt);
    }
    return NULL;
  default:
    return NULL;
  }
}
static void
Handle_Sink_Inconsistent_Case (WN *if_stmt)
{
  // We know that for this do loop, we are guarenteed that 
  // the if_stmt is always false..

  WN *delete_stmt = LWN_Extract_From_Block (if_stmt);
  LWN_Delete_Tree (delete_stmt);
  return;
}

static void
Handle_Sink_Redundant_Case (WN *if_stmt)
{
  // The if statement is redundant in this case, so we need to 
  // delete it..
  WN *delete_stmt;

  FOR_CHILDREN (WN_then (if_stmt), child, ignCount) {
    delete_stmt = LWN_Extract_From_Block (child);
    LWN_Insert_Block_Before (NULL, if_stmt, delete_stmt);
  }
  END_CHILDREN;
  delete_stmt = LWN_Extract_From_Block (if_stmt);
  LWN_Delete_Tree (delete_stmt);
  return;
}

static void
Handle_Sink_General_Case(WN *if_stmt, WN *do_loop,
			 INT32 loop_depth, ACCESS_VECTOR *cond)
{
  assert (cond->Loop_Coeff (loop_depth) != 0);
  WN *dup_loop = LWN_Copy_Tree (do_loop, TRUE, LNO_Info_Map,
				TRUE, shackle_if_copy_map1, TRUE);
  LWN_Insert_Block_After (NULL, do_loop, dup_loop);
  BOOL true_branch_first = (cond->Loop_Coeff (loop_depth) > 0);
  ARRAY_DIRECTED_GRAPH16 *dg = Array_Dependence_Graph;

  dg->Versioned_Dependences_Update (do_loop, dup_loop,
				    loop_depth,
				    shackle_if_copy_map1);
  WN *fake_unroll[2];
  fake_unroll[0] = do_loop;
  fake_unroll[1] = dup_loop;
  Unrolled_DU_Update (fake_unroll, 2, loop_depth - 1);
  WN *dup_if_stmt = (WN *) WN_MAP_Get (shackle_if_copy_map1, if_stmt);
  // Only remaining things are to update loop bounds
  // The upper bound of the original loop and the lower bounds
  // of the new loop need to be updated.
  WN *canonical_cond, *ubnd, *lbnd;

  canonical_cond = canonicalize_if_condition (WN_if_test (dup_if_stmt),
					      loop_depth);
  ubnd = return_upper_bound (canonical_cond,
			     SYMBOL (WN_index (dup_loop)),
			     cond->Loop_Coeff (loop_depth),
			     true_branch_first);
  lbnd = return_upper_boundplus1 (ubnd, loop_depth);
  TYPE_ID index_type = WN_rtype (lbnd);
  assert (OPR_STID == 
	  WN_operator (WN_start (dup_loop)));
  assert(index_type == WN_desc (WN_start (dup_loop)));
  WN *delete_node = WN_kid0 (WN_start (dup_loop));
  Replace_WN (delete_node, lbnd);
  LWN_Delete_Tree (delete_node);
  // Next, upper bound..
  canonical_cond = canonicalize_if_condition (WN_if_test (if_stmt),
					      loop_depth);
  ubnd = return_upper_bound (canonical_cond,
			     SYMBOL (WN_index (do_loop)),
			     cond->Loop_Coeff (loop_depth),
			     true_branch_first);
  index_type = WN_desc (WN_start (do_loop));
  assert (Promote_Type (index_type) == 
	  Promote_Type (WN_rtype (ubnd)));
  OPCODE load_opc = 
    OPCODE_make_op (OPR_LDID, Promote_Type (index_type),
		    index_type);
  WN *end_lhs = LWN_CreateLdid (load_opc, WN_start (do_loop));
  Du_Mgr->Add_Def_Use (WN_start (do_loop), end_lhs);
  Du_Mgr->Add_Def_Use (WN_step (do_loop), end_lhs);
  Du_Mgr->Ud_Get_Def(end_lhs)->Set_loop_stmt(do_loop);
  TYPE_ID rtype = WN_rtype (WN_end (do_loop));
  WN_set_opcode (WN_end (do_loop),
		 OPCODE_make_op (OPR_LE, rtype,
				 Promote_Type (index_type)));
  delete_node = WN_kid0 (WN_end (do_loop));
  Replace_WN (delete_node, end_lhs);
  LWN_Delete_Tree (delete_node);
  delete_node = WN_kid1 (WN_end (do_loop));
  Replace_WN (delete_node, ubnd);
  LWN_Delete_Tree (delete_node);
  // Now, attend to the statements under the conditionals..
  WN *true_branch, *false_branch;

  true_branch = (true_branch_first) ? if_stmt : dup_if_stmt;
  false_branch = (true_branch_first) ? dup_if_stmt : if_stmt;
  // False branch - easier one..
  WN *delete_stmt = LWN_Extract_From_Block (false_branch);
  LWN_Delete_Tree (delete_stmt);
  // Next, the true branch - just remove the conditional
  FOR_CHILDREN (WN_then (true_branch), child, ignCount) {
    delete_stmt = LWN_Extract_From_Block (child);
    LWN_Insert_Block_Before (NULL, true_branch, child);
  }
  END_CHILDREN;
  delete_stmt = LWN_Extract_From_Block (true_branch);
  LWN_Delete_Tree (delete_stmt);
}

static BOOL
Is_Provably_In_Bounds(ACCESS_ARRAY       *LB_ARRAY,
		      ACCESS_ARRAY       *UB_ARRAY,
		      ACCESS_VECTOR      *v3,
		      INT32               size_loop,
		      INT32               size_sym,
		      QUEUE<ST *>        *lin_sym_q,
		      INT32               pos,
		      WN                 *do_loop, 
		      enum SHACKLE_BOUND  which_shackle)
{
  UINT32 i;
  INT32  size = size_loop + size_sym, index;
  INT32 lb_coeff, v3_coeff, ub_coeff;
  ACCESS_VECTOR *lb_v3, *ub_v3;
  BOOL retval;
  SYSTEM_OF_EQUATIONS *soe;
  BOOL lb_sat, ub_sat;
  ACCESS_VECTOR *LB = LB_ARRAY->Dim(0);
  ACCESS_VECTOR *UB = UB_ARRAY->Dim(0);

  lb_coeff = LB->Loop_Coeff (pos);
  v3_coeff = v3->Loop_Coeff (pos);
  ub_coeff = UB->Loop_Coeff (pos);
  assert (v3_coeff != 0);
  lb_v3 = CXX_NEW (ACCESS_VECTOR (v3, shackle_if_pool),
		     shackle_if_pool);
  ub_v3 = CXX_NEW (ACCESS_VECTOR (v3, shackle_if_pool),
		     shackle_if_pool);
  if (v3_coeff < 0) { // this was a lower bound..
    ub_v3->Negate_Me();
    ub_v3->Const_Offset--;
    v3_coeff = - v3_coeff;
  }
  else { // this was an upper bound..
    lb_v3->Negate_Me();
    lb_v3->Const_Offset--;
  }
  lb_coeff = -lb_coeff;
  assert (ub_coeff > 0);
  assert (lb_coeff > 0);
  assert (v3_coeff > 0);
  mINT32 *row = CXX_NEW_ARRAY (mINT32, size, shackle_if_pool);
  ACCESS_VECTOR *dup = 
    CXX_NEW (ACCESS_VECTOR (size_loop, shackle_if_pool), 
	     shackle_if_pool);
  dup->Too_Messy = FALSE;
  {
    // test if this can be proved to be larger than lower bound
    soe = CXX_NEW (SYSTEM_OF_EQUATIONS (0, 0, size, shackle_if_pool),
		   shackle_if_pool);
    for (i = 0; i < size; i++) 
      row[i] = 0;
    for (i = 0; i < size_loop; i++) {
      row[i] = v3_coeff * LB->Loop_Coeff (i) - 
	lb_coeff * lb_v3->Loop_Coeff (i);
    }
    if (NULL != LB->Lin_Symb) {
      INTSYMB_CONST_ITER iter (LB->Lin_Symb);
      for (const INTSYMB_NODE *node = iter.First();
	   !iter.Is_Empty(); node = iter.Next()) {
	index = lin_sym_q->Index (node->Symbol.St());
	FmtAssert (0 <= index && index < size_sym, 
		   ("Incorrect value for index"));
	row[size_loop+index] += v3_coeff * node->Coeff;
      }
    }
    if (NULL != lb_v3->Lin_Symb) {
      INTSYMB_CONST_ITER iter (lb_v3->Lin_Symb);
      for (const INTSYMB_NODE *node = iter.First();
	   !iter.Is_Empty(); node = iter.Next()) {
	index = lin_sym_q->Index (node->Symbol.St());
	FmtAssert (0 <= index && index < size_sym, 
		   ("Incorrect value for index"));
	row[size_loop+index] -= lb_coeff * node->Coeff;
      }
    }
    assert (0 == row[pos]);
    INT64 lb_const_offset = v3_coeff * lb_coeff - lb_coeff + 
      v3_coeff * LB->Const_Offset - lb_coeff * lb_v3->Const_Offset;
    // Simplify_Coeff_Access_Vector (dup);
    for (i = 0; i < size; i++) 
      row[i] = -row[i];
    lb_const_offset = - lb_const_offset;
    lb_const_offset--;
    Add_Parent_If_Constraints (do_loop, soe, size_loop,
			       size_sym, lin_sym_q);
    Add_Parent_Loop_Constraints (do_loop, soe, size_loop,
				 size_sym, lin_sym_q);
    soe->Add_Le (row, lb_const_offset);
    lb_sat = !soe->Is_Consistent();
  } 
  {
    // test if this can be proved to be smaller than upper bound
    soe = CXX_NEW (SYSTEM_OF_EQUATIONS (0, 0, size, shackle_if_pool),
		   shackle_if_pool);
    for (i = 0; i < size; i++)
      row[i] = 0;
    for (i = 0; i < size_loop; i++) {
      row[i] = v3_coeff * UB->Loop_Coeff (i) - 
	ub_coeff * ub_v3->Loop_Coeff (i);
    }
    if (NULL != UB->Lin_Symb) {
      INTSYMB_CONST_ITER iter(UB->Lin_Symb);
      for (const INTSYMB_NODE *node = iter.First();
	   !iter.Is_Empty(); node = iter.Next()) {
	index = lin_sym_q->Index (node->Symbol.St());
	FmtAssert (0 <= index && index < size_sym, 
		   ("Incorrect value for index"));
	row[size_loop+index] += v3_coeff * node->Coeff;
      }
    }
    if (NULL != ub_v3->Lin_Symb) {
      INTSYMB_CONST_ITER iter (ub_v3->Lin_Symb);
      for (const INTSYMB_NODE *node = iter.First();
	   !iter.Is_Empty(); node = iter.Next()) {
	index = lin_sym_q->Index (node->Symbol.St());
	FmtAssert (0 <= index && index < size_sym,
		   ("Incorrect value for index"));
	row[size_loop+index] -= ub_coeff * node->Coeff;
      }
    }
    assert (0 == row[pos]);
    INT64 ub_const_offset = - ub_coeff + v3_coeff * ub_coeff +
      v3_coeff * UB->Const_Offset - ub_coeff * ub_v3->Const_Offset;
    // Simplify_Coeff_Access_Vector (dup);
    for (i = 0; i < size; i++)
      row[i] = -row[i];
    ub_const_offset = -ub_const_offset;
    ub_const_offset--;
    Add_Parent_If_Constraints (do_loop, soe, size_loop,
			       size_sym, lin_sym_q);
    Add_Parent_Loop_Constraints (do_loop, soe, size_loop,
				 size_sym, lin_sym_q);
    soe->Add_Le (row, ub_const_offset);
    ub_sat = !soe->Is_Consistent();
  }
  switch (which_shackle) {
  case SHACKLE_LOWER:
    return lb_sat;
  case SHACKLE_UPPER:
    return ub_sat;
  case SHACKLE_BOTH:
    return (lb_sat && ub_sat);
  }
  return FALSE;
}

static void
Handle_Sink_Symbolic_Non_Promotion_Case (WN            *if_stmt,
					 WN            *do_loop,
					 INT32          loop_depth,
					 ACCESS_VECTOR *cond)
{
  assert (cond->Loop_Coeff (loop_depth) != 0);
  WN *canonical_cond, *ubnd, *lbnd;
  canonical_cond = 
    canonicalize_if_condition (WN_if_test (if_stmt),
			       loop_depth);
  ubnd = return_upper_bound (canonical_cond,
			     SYMBOL (WN_index (do_loop)),
			     cond->Loop_Coeff (loop_depth),
			     (cond->Loop_Coeff (loop_depth) > 0));
  lbnd = NULL;
  if (cond->Loop_Coeff (loop_depth) < 0) { // Lower bound
    lbnd = return_upper_boundplus1 (ubnd, loop_depth);
    ubnd = NULL;
  }
  if (cond->Loop_Coeff (loop_depth) > 0) { // Upper bound
    WN *do_end = WN_end (do_loop);
    OPERATOR opr = WN_operator (do_end);
    assert ((OPR_LE == opr) || (OPR_LT == opr) 
      || (OPR_GE == opr) || (OPR_GT == opr));
    Upper_Bound_Standardize (WN_end (do_loop), FALSE);
    WN *do_end_rhs = WN_kid1 (WN_end (do_loop));
    TYPE_ID index_type = WN_rtype (do_end_rhs);
    WN *dummy_one = 
      WN_CreateIntconst (OPCODE_make_op (OPR_INTCONST, 
					 Promote_Type (index_type),
					 MTYPE_V),
			 (INT64) 1);
    Replace_WN (do_end_rhs, dummy_one);
    WN *new_do_end_rhs = 
      WN_CreateExp2 (OPCODE_make_op (OPR_MIN, 
				     Promote_Type (index_type),
				     MTYPE_V),
		     ubnd, do_end_rhs);
    Replace_WN (dummy_one, new_do_end_rhs);
    LWN_Delete_Tree (dummy_one);
  }
  else { // Lower bound
    assert (cond->Loop_Coeff (loop_depth) < 0);
    WN *do_begin = WN_start (do_loop);
    OPERATOR opr = WN_operator (do_begin);
    assert (OPR_STID == opr);
    WN *do_begin_rhs = WN_kid0 (do_begin);
    TYPE_ID index_type = WN_desc (do_begin_rhs);
    WN *dummy_one = 
      WN_CreateIntconst (OPCODE_make_op (OPR_INTCONST,
					 Promote_Type (index_type),
					 MTYPE_V),
			 (INT64) 1);
    Replace_WN (do_begin_rhs, dummy_one);
    WN *new_do_begin_rhs = 
      WN_CreateExp2 (OPCODE_make_op (OPR_MAX,
				     Promote_Type (index_type),
				     MTYPE_V),
		     lbnd, do_begin_rhs);
    Replace_WN (dummy_one, new_do_begin_rhs);
    LWN_Delete_Tree (dummy_one);
  }
  // get rid of the if_stmt
  WN *delete_stmt;
  FOR_CHILDREN (WN_then (if_stmt), child, ignCount) {
    delete_stmt = LWN_Extract_From_Block (child);
    LWN_Insert_Block_Before (NULL, if_stmt, delete_stmt);
  }
  END_CHILDREN;
  delete_stmt = LWN_Extract_From_Block (if_stmt);
  LWN_Delete_Tree (delete_stmt);
  return;
}

static BOOL
Soe_Implies_Access_Vector(SYSTEM_OF_EQUATIONS *soe,
			  ACCESS_VECTOR       *v,
			  UINT32               size)
{
  INT32 i;
  if (shackle_if_debug_level > 0) {
    fprintf(stdout, "Before system\n");
    if (shackle_if_debug_level > 1) 
      soe->Print (stdout);
    v->Print (stdout, FALSE);
    fprintf(stdout, "Analysis started\n");
  }
  ACCESS_VECTOR *dup = 
    CXX_NEW (ACCESS_VECTOR (v, shackle_if_pool),
	     shackle_if_pool);
  dup->Negate_Me();
  dup->Const_Offset--;
  SYSTEM_OF_EQUATIONS *dup_soe = 
    CXX_NEW (SYSTEM_OF_EQUATIONS (0, 0, size, shackle_if_pool),
	     shackle_if_pool);
  dup_soe->Add_Soe (soe);
  mINT32 *row = CXX_NEW_ARRAY (mINT32, size, shackle_if_pool);
  for (i = 0; i < size; i++) {
    row[i] = dup->Loop_Coeff (i);
  }
  dup_soe->Add_Le (row, dup->Const_Offset);
  if (shackle_if_debug_level > 1) 
    dup_soe->Print (stdout);
  return !dup_soe->Is_Consistent();
}

static BOOL
V3geLB_Implies_V3geUB(ACCESS_VECTOR *LB,
		      ACCESS_VECTOR *UB,
		      ACCESS_VECTOR *v3,
		      UINT32         size,
		      UINT32         pos)
{
  UINT32 i;
  INT32 lb_coeff, ub_coeff, ub_v3;
  
  lb_coeff = LB->Loop_Coeff (pos);
  ub_coeff = UB->Loop_Coeff (pos);
  ub_v3 = v3->Loop_Coeff (pos);
  assert (lb_coeff < 0);
  assert (ub_coeff > 0);
  assert (ub_v3 > 0);
  // we are interested in the abs value of the lb..
  lb_coeff = -lb_coeff;
  mINT32 *row = CXX_NEW_ARRAY (mINT32, size, shackle_if_pool);
  for (i = 0; i < size; i++) {
    row[i] = ub_v3 * LB->Loop_Coeff (i) + lb_coeff * v3->Loop_Coeff (i);
  }
  assert (0 == row[pos]);
  SYSTEM_OF_EQUATIONS *soe = 
    CXX_NEW (SYSTEM_OF_EQUATIONS (0, 0, size, shackle_if_pool),
	     shackle_if_pool);
  soe->Add_Le (row, ub_v3 * LB->Const_Offset + 
	       lb_coeff * v3->Const_Offset);
  ACCESS_VECTOR *dup = 
    CXX_NEW (ACCESS_VECTOR (size, shackle_if_pool), shackle_if_pool);
  for (i = 0; i < size; i++) {
    dup->Set_Loop_Coeff (i, ub_coeff * v3->Loop_Coeff (i) - 
			 ub_v3 * UB->Loop_Coeff (i));
  }
  dup->Const_Offset = ub_coeff * v3->Const_Offset - 
    ub_v3 * UB->Const_Offset;
  dup->Too_Messy = FALSE;
  assert (0 == dup->Loop_Coeff (pos));
  return Soe_Implies_Access_Vector (soe, dup, size);
}

static BOOL
V3leUB_Implies_V3leLB(ACCESS_VECTOR *LB,
		      ACCESS_VECTOR *UB,
		      ACCESS_VECTOR *v3,
		      UINT32         size,
		      UINT32         pos)
{
  UINT32 i;
  INT32 lb_coeff, ub_coeff, lb_v3;
  lb_coeff = LB->Loop_Coeff (pos);
  ub_coeff = UB->Loop_Coeff (pos);
  lb_v3 = v3->Loop_Coeff (pos);
  assert (lb_coeff < 0);
  assert (ub_coeff > 0);
  assert (lb_v3 < 0);
  // As before, we are interested in the abs values of the lbs
  lb_coeff = -lb_coeff;
  lb_v3 = -lb_v3;
  mINT32 *row = CXX_NEW_ARRAY (mINT32, size, shackle_if_pool);
  for (i = 0; i < size; i++) {
    row[i] = lb_v3 * UB->Loop_Coeff (i) + ub_coeff * v3->Loop_Coeff (i);
  }
  assert (0 == row[pos]);
  SYSTEM_OF_EQUATIONS *soe = 
    CXX_NEW (SYSTEM_OF_EQUATIONS (0, 0, size, shackle_if_pool),
	     shackle_if_pool);
  soe->Add_Le (row, lb_v3 * UB->Const_Offset + 
	       ub_coeff * v3->Const_Offset);
  ACCESS_VECTOR *dup = 
    CXX_NEW (ACCESS_VECTOR (size, shackle_if_pool), shackle_if_pool);
  for (i = 0; i < size; i++) {
    dup->Set_Loop_Coeff (i, lb_coeff * v3->Loop_Coeff (i) - 
			 lb_v3 * LB->Loop_Coeff (i));
  }
  dup->Const_Offset = lb_coeff * v3->Const_Offset - 
    lb_v3 * LB->Const_Offset;
  dup->Too_Messy = FALSE;
  assert (0 == dup->Loop_Coeff (pos));
  return Soe_Implies_Access_Vector (soe, dup, size);
}

static BOOL
is_promotion_case (ACCESS_ARRAY *LB, ACCESS_ARRAY *UB,
		   ACCESS_VECTOR *cond, INT32 depth) 
{
  assert (1 == LB->Num_Vec());
  assert (1 == UB->Num_Vec());
  
  assert (0 != cond->Loop_Coeff (depth));
  if (cond->Loop_Coeff (depth) > 0) 
    return V3geLB_Implies_V3geUB (LB->Dim(0), UB->Dim(0),
				  cond, depth+1, depth);
  else
    return V3leUB_Implies_V3leLB (LB->Dim(0), UB->Dim(0),
				  cond, depth+1, depth);
}

static void
Handle_Sink_Promotion_Case (WN *if_stmt, WN *do_loop,
			    INT32 loop_depth, 
			    ACCESS_VECTOR *cond) 
{
  /* Promotion is in two parts. Say, we have a condition that 
     says that k <lb,ub> and a new conditional comes in of the 
     form k <= ub2. We need to first insert a conditional around 
     the whole thing saying that ub2 >= lb.. Similarly, in the 
     other case that we have a condition of the form k >= lb2,
     then we need to insert a conditional that says lb2 <= ub */
  WN *fake_unroll[2];
  WN *dup_loop = LWN_Copy_Tree (do_loop, TRUE, LNO_Info_Map,
				TRUE, shackle_if_copy_map1,
				TRUE);
  LWN_Insert_Block_After (NULL, do_loop, dup_loop);
  ARRAY_DIRECTED_GRAPH16 *dg = Array_Dependence_Graph;
  dg->Versioned_Dependences_Update (do_loop, dup_loop,
				    loop_depth, 
				    shackle_if_copy_map1);
  fake_unroll[0] = do_loop;
  fake_unroll[1] = dup_loop;
  Unrolled_DU_Update (fake_unroll, 2, loop_depth - 1);
  WN *dup_if_stmt = (WN *) WN_MAP_Get (shackle_if_copy_map1, 
				       if_stmt);
  {
    WN *main_if, *main_cond1, *main_cond2;
    WN *cond1 = canonicalize_if_condition (WN_if_test (if_stmt),
					   loop_depth);
    main_cond1 = 
      return_upper_bound (cond1,
			  SYMBOL (WN_index (do_loop)),
			  cond->Loop_Coeff (loop_depth),
			  (cond->Loop_Coeff (loop_depth) > 0));
    if (cond->Loop_Coeff (loop_depth) < 0) 
      main_cond1 = return_upper_boundplus1 (main_cond1,
					    loop_depth);

    // If we are getting a lower bound, we ought to compare with
    // the upper bound of the loop
    if (cond->Loop_Coeff (loop_depth) < 0) {
      Upper_Bound_Standardize (WN_end (do_loop), FALSE);
      main_cond2 = LWN_Copy_Tree (WN_kid1 (WN_end (do_loop)));

      fake_unroll[0] = WN_kid1 (WN_end (do_loop));
      fake_unroll[1] = main_cond2;
      Unrolled_DU_Update (fake_unroll, 2, loop_depth);
    } else {
      assert (OPR_STID == 
	      WN_operator (WN_start (do_loop)));
      main_cond2 = LWN_Copy_Tree (WN_kid0 (WN_start (do_loop)));
      fake_unroll[0] = WN_kid0 (WN_start (do_loop));
      fake_unroll[1] = main_cond2;
      Unrolled_DU_Update (fake_unroll, 2, loop_depth);
    }
    assert (WN_rtype (main_cond1) == 
	    WN_rtype (main_cond2));
    TYPE_ID index_type = WN_rtype (main_cond1);
    TYPE_ID rtype = WN_rtype (WN_if_test (if_stmt));
    WN *main_cond_to_insert;
    if (cond->Loop_Coeff (loop_depth) > 0) {
      main_cond_to_insert = WN_CreateExp2 
	(OPCODE_make_op (OPR_GE, rtype, Promote_Type (index_type)),
	 main_cond1, main_cond2);
    } else {
      main_cond_to_insert = WN_CreateExp2
	(OPCODE_make_op (OPR_LE, rtype, Promote_Type (index_type)),
	 main_cond1, main_cond2);
    }
    main_cond_to_insert = 
      Simplify_If_Conditional (main_cond_to_insert);
    Simplify_Cond_With_Div_Floor (main_cond_to_insert);
    main_if = LWN_CreateIf (main_cond_to_insert,
			    WN_CreateBlock(),
			    WN_CreateBlock());
    Replace_WN (do_loop, main_if);
    LWN_Insert_Block_After (WN_then (main_if), NULL, do_loop);
    IF_INFO *ii = 
      CXX_NEW (IF_INFO (&LNO_default_pool, TRUE,
			Tree_Has_Regions (main_if)),
	       &LNO_default_pool);
    WN_MAP_Set (LNO_Info_Map, main_if, (void *) ii);
    DOLOOP_STACK do_stack (shackle_if_pool);
    Build_Doloop_Stack (main_if, &do_stack);
    LNO_Build_If_Access (main_if, &do_stack);
  }
  {
    WN *main_if, *main_cond1, *main_cond2;
    WN *cond1 = canonicalize_if_condition (WN_if_test (dup_if_stmt),
					   loop_depth);
    main_cond1 = 
      return_upper_bound (cond1, 
			  SYMBOL (WN_index (dup_loop)),
			  cond->Loop_Coeff (loop_depth),
			  (cond->Loop_Coeff (loop_depth) > 0));
    if (cond->Loop_Coeff (loop_depth) < 0)
      main_cond1 = return_upper_boundplus1 (main_cond1, 
					    loop_depth);
    // If we are getting a lower bound, we ought to compare with
    // the upper bound of the loop
    if (cond->Loop_Coeff (loop_depth) < 0) {
      Upper_Bound_Standardize (WN_end (dup_loop), FALSE);
      main_cond2 = LWN_Copy_Tree (WN_kid1 (WN_end (dup_loop)));

      fake_unroll[0] = WN_kid1 (WN_end (dup_loop));
      fake_unroll[1] = main_cond2;
      Unrolled_DU_Update (fake_unroll, 2, loop_depth);
    } else {
      FmtAssert (OPR_STID == WN_operator (WN_start (dup_loop)),
		 ("Do loop with Non STID start!"));
      main_cond2 = LWN_Copy_Tree (WN_kid0 (WN_start (dup_loop)));
      fake_unroll[0] = WN_kid0 (WN_start (dup_loop));
      fake_unroll[1] = main_cond2;
      Unrolled_DU_Update (fake_unroll, 2, loop_depth);
    }
    FmtAssert (WN_rtype (main_cond1) == WN_rtype (main_cond2),
	       ("Two halves of a cond with different rtypes!"));
    TYPE_ID index_type = WN_rtype (main_cond1);
    TYPE_ID rtype = WN_rtype (WN_if_test (dup_if_stmt));
    WN *main_cond_to_insert;
    if (cond->Loop_Coeff (loop_depth) > 0) {
      main_cond_to_insert = WN_CreateExp2 
	(OPCODE_make_op (OPR_LT, rtype, Promote_Type (index_type)),
	 main_cond1, main_cond2);
    } else {
      main_cond_to_insert = WN_CreateExp2
	(OPCODE_make_op (OPR_GT, rtype, Promote_Type (index_type)),
	 main_cond1, main_cond2);
    }
    main_cond_to_insert = 
      Simplify_If_Conditional (main_cond_to_insert);
    Simplify_Cond_With_Div_Floor (main_cond_to_insert);
    main_if = LWN_CreateIf (main_cond_to_insert,
			    WN_CreateBlock(),
			    WN_CreateBlock());
    Replace_WN (dup_loop, main_if);
    LWN_Insert_Block_After (WN_then (main_if), NULL, dup_loop);
    IF_INFO *ii = 
      CXX_NEW (IF_INFO (&LNO_default_pool, TRUE,
			Tree_Has_Regions (main_if)),
	       &LNO_default_pool);
    WN_MAP_Set (LNO_Info_Map, main_if, (void *) ii);
    DOLOOP_STACK do_stack (shackle_if_pool);
    Build_Doloop_Stack (main_if, &do_stack);
    LNO_Build_If_Access (main_if, &do_stack);
  }
  for (INT32 i = 0; i < 2; i++) {
  }
}

static BOOL
Sink_If2do(WN *if_stmt, ACCESS_ARRAY *ar)
{
  if (1 != ar->Num_Vec()) 
    return FALSE;

  assert (1 == ar->Num_Vec());
  assert (!ar->Too_Messy);
  INT32 if_depth = Num_Common_Loops (if_stmt, if_stmt);
  // Ensure that there are at least as many entries as
  // the depth of this if statement.
  assert (ar->Dim(0)->Nest_Depth() >= if_depth);
  ACCESS_VECTOR *vector_to_sink = ar->Dim(0);
  assert (!vector_to_sink->Too_Messy);
  // Find the last non zero in this vector..
  INT32 i;
  for (i = if_depth - 1; i >= 0; i--) 
    if (0 != vector_to_sink->Loop_Coeff (i))
      break;
  // This is the do loop we'll sink our condition into..
  INT32 posn_of_do_loop = i;
  if (posn_of_do_loop < 0) {
    if (NULL == vector_to_sink->Lin_Symb) {
      // it must be the case that we are dealing with a loop
      // independent condition which is .TRUE. or .FALSE.
      if (vector_to_sink->Const_Offset < 0) // must be .FALSE.
	Handle_Sink_Inconsistent_Case (if_stmt);
      else // Must be .TRUE.
	Handle_Sink_Redundant_Case (if_stmt);
      return TRUE;
    } else {
      // We have a loop indenpendent condition that we want to
      // hoist outside of the enclosing do loop. If the if
      // statement has no siblings, then it is safe to hoist
      // it out, else the if statement must be left alone.
      if (FALSE == Has_Sibling_In_Block (if_stmt)) {
	WN *delete_stmt;
	WN *do_loop = Enclosing_Ith_Do_Loop (if_stmt, if_depth);
	FOR_CHILDREN (WN_then (if_stmt), child, ignCount) {
	  delete_stmt = LWN_Extract_From_Block (child);
	  LWN_Insert_Block_Before (NULL, if_stmt, delete_stmt);
	}
	END_CHILDREN;
	delete_stmt = LWN_Extract_From_Block (if_stmt);
	Replace_WN (do_loop, delete_stmt);
	LWN_Insert_Block_After (WN_then (delete_stmt), NULL, do_loop);
	return TRUE;
      } else {
	Shackle_If_Seen_Set (if_stmt);
	return FALSE;
      }
    }
  }
  WN *do_loop = Enclosing_Ith_Do_Loop (if_stmt, 
				       if_depth - posn_of_do_loop);
  assert (NULL != do_loop);
  INT32 loop_depth = Num_Common_Loops (do_loop, do_loop) - 1;
  assert (loop_depth == posn_of_do_loop);
  // Handle simple cases first
  DO_LOOP_INFO *dli = Get_Do_Loop_Info (do_loop);
  QUEUE<ST *> *lin_sym_q = 
    CXX_NEW (QUEUE<ST *> (shackle_if_pool), shackle_if_pool);
  Recursively_Add_Bound_Lin_Symbols(lin_sym_q, if_stmt);
  Recursively_Add_Array_Lin_Symbols(lin_sym_q, if_stmt);
  Recursively_Add_Parent_If_Lin_Symbols (lin_sym_q, if_stmt);
  // Is the condition just false by default?
  if (shackle_if_debug_level > 0)
    fprintf(stdout, "Attempt: %d\n", shackle_debug_exec_count++);
  if (is_vector_inconsistent (dli->LB, dli->UB, vector_to_sink,
			      loop_depth + 1, 
			      lin_sym_q->Queue_Length(),
			      lin_sym_q,
			      do_loop)) {
    Handle_Sink_Inconsistent_Case (if_stmt);
    return TRUE;
  }
  else if (is_vector_trivial (dli->LB, dli->UB, vector_to_sink,
			      loop_depth + 1, 
			      lin_sym_q->Queue_Length(),
			      lin_sym_q,
			      do_loop)) {
    Handle_Sink_Redundant_Case (if_stmt);
    return TRUE;
  }
  else if (Is_Provably_In_Bounds (dli->LB, dli->UB, vector_to_sink,
				  loop_depth+1, 
				  lin_sym_q->Queue_Length(),
				  lin_sym_q,
				  loop_depth, do_loop, 
				  SHACKLE_BOTH)) {
    Handle_Sink_General_Case (if_stmt, do_loop, loop_depth, 
			      ar->Dim(0));
    return TRUE;
  }
  /*
    else if (Interferes_With_Symbolic_Bound(ar->Dim(0),
    loop_depth, 
    dli->LB,
    dli->UB)) {
    Handle_Sink_Symbolic_Non_Promotion_Case(if_stmt, do_loop,
    loop_depth, ar->Dim(0));
    Convert_Do_Loops_Conditionals (func_nd);
    return TRUE;
    }
    */
  else if (1 || is_promotion_case (dli->LB, dli->UB, vector_to_sink,
				   loop_depth)) {
    Maybe_Handle_Sink_Promotion_Case (if_stmt, do_loop, 
				      loop_depth, 
				      ar->Dim(0));
    return TRUE;
  }
}

static void
Remove_Redundant_And_Inconsistent_If(WN *wn)
{
  if (OPC_IF == WN_opcode (wn)) {
    IF_INFO *ii = (IF_INFO *) WN_MAP_Get (LNO_Info_Map, wn);
    ACCESS_ARRAY *ar = ii->Condition;
    if (!ar->Too_Messy && (1 == ar->Num_Vec())) {
      INT32 if_depth = Num_Common_Loops (wn, wn);
      assert (ar->Dim(0)->Nest_Depth() >= if_depth);
      ACCESS_VECTOR *vector_to_sink = ar->Dim (0);
      if (!vector_to_sink->Too_Messy) {
        INT32 i;
	for (i = if_depth - 1; i >= 0; i--) 
	  if (0 != vector_to_sink->Loop_Coeff (i))
	    break;
	INT32 posn_of_do_loop = i;
	if (posn_of_do_loop >= 0) {
	  WN *do_loop = 
	    Enclosing_Ith_Do_Loop (wn, if_depth - posn_of_do_loop);
	  FmtAssert (NULL != do_loop, ("Impossible for a 0 do loop"));
	  INT32 loop_depth = Num_Common_Loops (do_loop, do_loop) - 1;
	  FmtAssert (loop_depth == posn_of_do_loop,
		     ("Loop depth and posn of do loop must be same"));
	  DO_LOOP_INFO *dli = Get_Do_Loop_Info (do_loop);
	  QUEUE<ST *> *lin_sym_q = 
	    CXX_NEW (QUEUE<ST *> (shackle_if_pool), shackle_if_pool);
	  Recursively_Add_Bound_Lin_Symbols (lin_sym_q, wn);
	  Recursively_Add_Array_Lin_Symbols (lin_sym_q, wn);
	  Recursively_Add_Parent_If_Lin_Symbols (lin_sym_q, wn);
	  if (is_vector_inconsistent (dli->LB, dli->UB, vector_to_sink,
				      loop_depth+1,
				      lin_sym_q->Queue_Length(),
				      lin_sym_q, do_loop)) {
	    Handle_Sink_Inconsistent_Case (wn);
	    return;
	  }
	  else if (is_vector_trivial (dli->LB, dli->UB, vector_to_sink,
				      loop_depth + 1,
				      lin_sym_q->Queue_Length(),
				      lin_sym_q, do_loop)) {
	    FOR_CHILDREN (wn, child, ignCount) {
	      Remove_Redundant_And_Inconsistent_If (child);
	    }
	    END_CHILDREN;
	    Handle_Sink_Redundant_Case (wn);
	    return;
	  }
	}
      }
    }
  }
  FOR_CHILDREN (wn, child, ignCount) {
    Remove_Redundant_And_Inconsistent_If (child);
  }
  END_CHILDREN;
}

static WN *
Find_Topmost_Unseen_If_Containing_Node(WN *wn)
{
  WN *step = wn, *prev = NULL;
  while (step) {
    if ((OPC_IF == WN_opcode (step)) && Shackle_If_Unseen (step))
      prev = step;
    step = LWN_Get_Parent(step);
  }
  if (prev) 
    assert (OPC_IF == WN_opcode (prev));
  return prev;
}

// FInd an IF statement surrounding a do statement surrounding inp
// if such an if exists, then return it, else return null..

static WN *
Find_Unseen_If_Outside_Do(WN *inp)
{
  WN *step = inp;

  while (step) {
    if (OPC_DO_LOOP == WN_opcode (step))
      break;
    step = LWN_Get_Parent (step);
  }
  // If no enclosing do loops, return NULL
  if (!step) 
    return NULL;
  assert (OPC_DO_LOOP == WN_opcode (step));
  while (step) {
    if ((OPC_IF == WN_opcode (step)) && Shackle_If_Unseen (step))
      break;
    step = LWN_Get_Parent (step);
  }
  // If no enclosing if of the enclosing do, return NULL
  if (!step)
    return NULL;
  assert (OPC_IF == WN_opcode (step));
  return step;
}

static WN *
Find_Enclosing_Unseen_If (WN *inp)
{
  WN *step = LWN_Get_Parent (inp);
  while (step) {
    if ((OPC_IF == WN_opcode (step)) && Shackle_If_Unseen (step))
      return step;
    step = LWN_Get_Parent (step);
  }
  return NULL;
}

static WN *
Find_Unseen_If_Outside_Do(QUEUE<WN *> *inp)
{
  WN               *if_stmt;
  QUEUE_ITER<WN *>  iter(inp);
  WN               *step;

  while (iter.Step(&step)) {
    if_stmt = Find_Unseen_If_Outside_Do (step);
    if (NULL != if_stmt) 
      return step;
  }
  return NULL;
}

static WN *
Find_Stmt_With_Enclosing_If(QUEUE<WN *> *inp)
{
  WN               *step;
  QUEUE_ITER<WN *>  iter(inp);
  WN               *if_stmt;

  while (iter.Step (&step)) {
    if_stmt = Find_Enclosing_Unseen_If (step);
    if (NULL != if_stmt)
      return step;
  }
  return NULL;
}

static BOOL
_xanalyze_stmt_for_conds(WN *stmt)
{
  WN               *step, *dup, *enclosing_do;
  IF_INFO          *if_info;
  ACCESS_ARRAY     *ar;
  INT64             do_step;
  BOOL              retval;

  step = Shackle_Unseen_Highest_Enclosing_If (stmt);
  if (NULL == step) {
    return FALSE;
  }
  assert (OPC_IF == WN_opcode (step));
  enclosing_do = Enclosing_Do_Loop (step);
  if (NULL == enclosing_do) {
    Shackle_If_Seen_Set (step);
    return FALSE;
  }
  do_step = determine_if_sinkable_in_do (step, enclosing_do);
  if (shackle_if_debug_level > 1) {
    fprintf(stdout, "The step of the do loop is %d\n", 
	    (INT32) do_step);
  }
  if_info = Get_If_Info (step);
  ar = if_info->Condition;
  if (shackle_if_debug_level > 1) {
    fprintf(stdout, "Analysing if condition\n");
    ar->Print (stdout, TRUE);
  }
  if (0 != do_step) {
    MEM_POOL_Push (shackle_if_pool);
    retval = Sink_If2do (step, ar);
    MEM_POOL_Pop (shackle_if_pool);
    return retval;
  } else {
    Shackle_If_Seen_Set (step);
    // printf("Should'nt really happen\n");
  }
  // Nothing has changed..
  return FALSE;
}

static void
Initialize_Ifs_Unseen(WN *func_nd)
{
  switch (WN_opcode (func_nd)) {
  case OPC_IF:
    Shackle_If_Unseen_Set (func_nd);
    break;
  }
  FOR_CHILDREN (func_nd, child, ignCount) {
    Initialize_Ifs_Unseen (child);
  }
  END_CHILDREN;
}

BOOL
analyze_stmts_in_func_for_if (WN *func_nd)
{
  QUEUE_ITER<WN *>      *iter;
  WN                    *stmt;
  IF_INFO               *if_info;
  ACCESS_ARRAY          *ar;
  WN                    *dup;
  BOOL                   recomp;
  QUEUE<WN *>           *new_stmts;
  extern QUEUE<WN *>    *gather_stmts_in_func(WN *);
  WN                    *empty, *delete_stmt;

  Initialize_Ifs_Unseen (func_nd);
  new_stmts = gather_stmts_in_func (func_nd);
  while (1) {
    stmt = Find_Unseen_If_Outside_Do (new_stmts);
    if (NULL == stmt)
      stmt = Find_Stmt_With_Enclosing_If (new_stmts);
    if (NULL == stmt)
      return FALSE;
    recomp = _xanalyze_stmt_for_conds (stmt);
    if (recomp) {
      LWN_Parentize (func_nd);
      LNO_Build_Access (func_nd, &LNO_default_pool);
      Remove_Redundant_And_Inconsistent_If (func_nd);
      empty = Largest_Empty_Subtree (func_nd);
      if (empty) {
	delete_stmt = LWN_Extract_From_Block (empty);
	LWN_Delete_Tree (delete_stmt);
      }
      new_stmts = gather_stmts_in_func (func_nd);
    }
  }
}

// Find a subtree of root, not contained in mask, which 
// is an expression containing loop index variables

static WN *
Another_Expression_Comes_From_Loop(WN *root, WN *mask)
{
  if (root == mask)
    return NULL;
  DU_MANAGER *du = Du_Mgr;
  if (OPR_LDID == WN_operator (root)) {
    DEF_LIST *def_list = du->Ud_Get_Def (root);
    DEF_LIST_ITER iter (def_list);
    const DU_NODE *node = NULL;
    for (node = iter.First(); 
	 !iter.Is_Empty();
	 node = iter.Next()) {
      WN *def = node->Wn();
      if ((NULL != LWN_Get_Parent (def)) &&
	  (OPC_DO_LOOP == WN_opcode (LWN_Get_Parent (def))))
	return root;
    }
    return NULL;
  } else {
    FOR_CHILDREN (root, child, ignCount) {
      WN *result = 
	Another_Expression_Comes_From_Loop (child, mask);
      if (NULL != result)
	return result;
    }
    END_CHILDREN;
    return NULL;
  }
}
static WN*
Find_Do_Loop_With_Min(WN *root, WN **expr1, WN **expr2)
{
  if (OPC_DO_LOOP == WN_opcode (root)) {
    if (!Do_Loop_Is_Unsigned (root))
      if (FALSE == Upper_Bound_Standardize (WN_end (root), TRUE)) {
	*expr1 = NULL;
	*expr2 = NULL;
	return NULL;
      }
    WN *wn1 = 
      Another_Expression_Comes_From_Loop (WN_kid1 (WN_end (root)),
					  NULL);
    if (NULL == wn1) {
      *expr1 = NULL;
      *expr2 = NULL;
      return NULL;
    }
    WN *parent, *other, *wn2 = NULL, *same;
    parent = LWN_Get_Parent (wn1);
    same = wn1;

    while (NULL != parent) {
      if (OPR_MIN == WN_operator (parent)) {
	if (same == WN_kid0 (parent))
	  other = WN_kid1 (parent);
	else {
	  assert (WN_kid1 (parent) == same);
	  other = WN_kid0 (parent);
	}
	wn2 = Another_Expression_Comes_From_Loop (other, NULL);
	if (NULL != wn2) {
	  while (OPR_MIN != 
		 WN_operator (LWN_Get_Parent (wn1)))
	    wn1 = LWN_Get_Parent (wn1);
	  while (OPR_MIN !=
		 WN_operator (LWN_Get_Parent (wn2)))
	    wn2 = LWN_Get_Parent (wn2);
	  *expr1 = wn1;
	  *expr2 = wn2;
	  return root;
	}
      }
      same = parent;
      parent = LWN_Get_Parent (parent);
    }
    *expr1 = *expr2 = NULL;
    return NULL;
  }
  *expr1 = *expr2 = NULL;
  return NULL;
}

static WN *
Recursively_Find_Do_Loop_With_Min(WN *root, WN **expr1, WN **expr2)
{
  WN *result = NULL;

  if (OPC_DO_LOOP == WN_opcode (root)) {
    result = Find_Do_Loop_With_Min (root, expr1, expr2);
    if (NULL != result)
      return result;
  }
  FOR_CHILDREN (root, child, ignCount) {
    result = Recursively_Find_Do_Loop_With_Min (child, expr1, expr2);
    if (NULL != result)
      return result;
  }
  END_CHILDREN;
  *expr1 = *expr2 = NULL;
  return NULL;
}

static WN *
Sibling(WN *wn)
{
  WN *parent = LWN_Get_Parent (wn);
  if (wn == WN_kid0 (parent))
    return WN_kid1 (parent);
  else
    return WN_kid0 (parent);
}

static BOOL
Has_Sibling_In_Block(WN *wn)
{
  WN *parent = LWN_Get_Parent (wn);
  FmtAssert (OPC_BLOCK == WN_opcode (parent), 
	     ("Parent must be a block"));
  FOR_CHILDREN (parent, child, ignCount) {
    if (child != wn)
      return TRUE;
  }
  END_CHILDREN;
  return FALSE;
}

void 
Convert_Do_Loops_Conditionals(WN *func_nd)
{
  WN                    *do_loop, *wn1, *wn2;
  extern QUEUE<WN *>    *gather_stmts_in_func(WN *);
  WN                    *fake_unroll[2];

  do_loop = Recursively_Find_Do_Loop_With_Min (func_nd, &wn1, &wn2);
  if (NULL == do_loop)
    return;
  assert (NULL != do_loop);
  assert ((NULL != wn1) && (NULL != wn2) && (wn1 != wn2));
  INT32 loop_depth = Num_Common_Loops (do_loop, do_loop) - 1;
  // Add a pair of if conditionals around the do_loop ensuring
  // that the two upper bounds selected are both larger than
  // the lower bound
  {
    WN *do_start = WN_start (do_loop);
    assert (OPR_STID == WN_operator (do_start));
    WN *do_start_kid = WN_kid0 (do_start);
    WN *if1_wn1 = LWN_Copy_Tree (wn1);
    fake_unroll[0] = wn1;
    fake_unroll[1] = if1_wn1;
    Unrolled_DU_Update (fake_unroll, 2, loop_depth-1);
    WN *if1_wn2 = LWN_Copy_Tree (do_start_kid);
    fake_unroll[0] = do_start_kid;
    fake_unroll[1] = if1_wn2;
    Unrolled_DU_Update (fake_unroll, 2, loop_depth-1);
    TYPE_ID rtype = WN_rtype (WN_end (do_loop));
    TYPE_ID index_type = WN_desc (do_start);
    WN *if1_cond = WN_CreateExp2 
      (OPCODE_make_op (OPR_GE, rtype, Promote_Type (index_type)),
       if1_wn1, if1_wn2);
    if1_cond = Simplify_If_Conditional (if1_cond);
    WN *if1 = LWN_CreateIf (if1_cond,
			    WN_CreateBlock(),
			    WN_CreateBlock());
    Replace_WN (do_loop, if1);
    LWN_Insert_Block_After (WN_then (if1), NULL, do_loop);
    IF_INFO *ii = 
      CXX_NEW (IF_INFO (&LNO_default_pool, TRUE,
			Tree_Has_Regions (if1)),
	       &LNO_default_pool);
    WN_MAP_Set (LNO_Info_Map, if1, (void *) ii);
    DOLOOP_STACK do_stack (shackle_if_pool);
    Build_Doloop_Stack (if1, &do_stack);
    LNO_Build_If_Access (if1, &do_stack);
    // The second if statement
    WN *if2_wn1 = LWN_Copy_Tree (wn2);
    fake_unroll[0] = wn2;
    fake_unroll[1] = if2_wn1;
    Unrolled_DU_Update (fake_unroll, 2, loop_depth-1);
    WN *if2_wn2 = LWN_Copy_Tree (do_start_kid);
    fake_unroll[0] = do_start_kid;
    fake_unroll[1] = if2_wn2;
    Unrolled_DU_Update (fake_unroll, 2, loop_depth-1);
    WN *if2_cond = WN_CreateExp2
      (OPCODE_make_op (OPR_GE, rtype, Promote_Type (index_type)),
       if2_wn1, if2_wn2);
    if2_cond = Simplify_If_Conditional (if2_cond);
    WN *if2 = LWN_CreateIf (if2_cond, WN_CreateBlock(), 
			    WN_CreateBlock());
    Replace_WN (if1, if2);
    LWN_Insert_Block_After (WN_then (if2), NULL, if1);
    ii = CXX_NEW (IF_INFO (&LNO_default_pool, TRUE,
			   Tree_Has_Regions (if2)),
		  &LNO_default_pool);
    WN_MAP_Set (LNO_Info_Map, if2, (void *) ii);
    Build_Doloop_Stack (if2, &do_stack);
    LNO_Build_If_Access (if2, &do_stack);
  }

  WN *dup_loop = LWN_Copy_Tree (do_loop, TRUE, LNO_Info_Map,
				TRUE, shackle_if_copy_map1, TRUE);
  // Around each of the do loops, we need to add a conditional
  // around the first, we add if (wn1 .lt. wn2 and around the 
  // second one, we add dwn1 >= dwn2. In the first case, we get
  // rid of wn2 from the min and in the second case, we get rid
  // of dwn1 from the min
  {
    LWN_Insert_Block_After (NULL, do_loop, dup_loop);
    ARRAY_DIRECTED_GRAPH16 *dg = Array_Dependence_Graph;
    dg->Versioned_Dependences_Update (do_loop, dup_loop, 
				      loop_depth,
				      shackle_if_copy_map1);
    fake_unroll[0] = do_loop;
    fake_unroll[1] = dup_loop;
    Unrolled_DU_Update (fake_unroll, 2, loop_depth-1);
    
    // Add an if_stmt around the first loop.
    TYPE_ID rtype = WN_rtype (WN_end (do_loop));
    assert (WN_rtype (wn1) ==
	    WN_rtype (wn2));
    TYPE_ID index_type = WN_rtype (wn1);
    WN *if_wn1 = LWN_Copy_Tree (wn1);
    WN *if_wn2 = LWN_Copy_Tree (wn2);
    fake_unroll[0] = wn1;
    fake_unroll[1] = if_wn1;
    Unrolled_DU_Update (fake_unroll, 2, loop_depth-1);
    fake_unroll[0] = wn2;
    fake_unroll[1] = if_wn2;
    Unrolled_DU_Update (fake_unroll, 2, loop_depth-1);
    WN *cond_to_insert = WN_CreateExp2
      (OPCODE_make_op (OPR_GT, rtype, Promote_Type (index_type)),
       if_wn1, if_wn2);
    cond_to_insert = Simplify_If_Conditional (cond_to_insert);
    WN *if_to_insert = LWN_CreateIf (cond_to_insert,
				     WN_CreateBlock(),
				     WN_CreateBlock());
    Replace_WN (do_loop, if_to_insert);
    LWN_Insert_Block_After (WN_then (if_to_insert), NULL,
			    do_loop);
    IF_INFO *ii = 
      CXX_NEW (IF_INFO (&LNO_default_pool, TRUE,
			Tree_Has_Regions (if_to_insert)),
	       &LNO_default_pool);
    WN_MAP_Set (LNO_Info_Map, if_to_insert, (void *) ii);
    DOLOOP_STACK do_stack (shackle_if_pool);
    Build_Doloop_Stack (if_to_insert, &do_stack);
    LNO_Build_If_Access (if_to_insert, &do_stack);
    // Add if_stmt around second loop
    WN *dup_wn1 = (WN *) WN_MAP_Get (shackle_if_copy_map1, wn1);
    WN *dup_wn2 = (WN *) WN_MAP_Get (shackle_if_copy_map1, wn2);
    WN *dup_if_wn1 = LWN_Copy_Tree (dup_wn1);
    WN *dup_if_wn2 = LWN_Copy_Tree (dup_wn2);
    fake_unroll[0] = dup_wn1;
    fake_unroll[1] = dup_if_wn1;
    Unrolled_DU_Update (fake_unroll, 2, loop_depth - 1);
    fake_unroll[0] = dup_wn2;
    fake_unroll[1] = dup_if_wn2;
    Unrolled_DU_Update (fake_unroll, 2, loop_depth - 1);
    WN *dup_cond_to_insert = WN_CreateExp2 
      (OPCODE_make_op (OPR_LE, rtype, Promote_Type (index_type)),
       dup_if_wn1, dup_if_wn2);
    dup_cond_to_insert = Simplify_If_Conditional (dup_cond_to_insert);
    WN *dup_if_to_insert = LWN_CreateIf (dup_cond_to_insert,
					 WN_CreateBlock(),
					 WN_CreateBlock());
    Replace_WN (dup_loop, dup_if_to_insert);
    LWN_Insert_Block_After (WN_then (dup_if_to_insert), NULL,
			    dup_loop);
    ii = CXX_NEW (IF_INFO (&LNO_default_pool, TRUE, 
			   Tree_Has_Regions (dup_if_to_insert)),
		  &LNO_default_pool);
    WN_MAP_Set (LNO_Info_Map, dup_if_to_insert, (void *) ii);
    Build_Doloop_Stack (dup_if_to_insert, &do_stack);
    LNO_Build_If_Access (dup_if_to_insert, &do_stack);
    // Get rid of wn1 and dup_wn2 in the respective do loops
    WN *dummy_one = WN_CreateIntconst 
      (OPCODE_make_op (OPR_INTCONST, Promote_Type (index_type),
		       MTYPE_V),
       (INT64) 1);
    WN *dummy_two = WN_CreateIntconst
      (OPCODE_make_op (OPR_INTCONST, Promote_Type (index_type),
		       MTYPE_V),
       (INT64) 2);
    WN *sib_wn1 = Sibling (wn1);
    if (sib_wn1 == wn2) {
      WN *parent = LWN_Get_Parent (wn1);
      FmtAssert (OPR_MIN == WN_operator (parent),
		 ("Parent must be an OPR_MIN"));
      assert (LWN_Get_Parent (wn2) == parent);
      Replace_WN (wn2, dummy_one);
      Replace_WN (parent, wn2);
      LWN_Delete_Tree (parent);
      LWN_Delete_Tree (dummy_two);
    }
    else if (OPR_MIN == WN_operator (sib_wn1)) {
      WN *sib_wn1_kid0 = WN_kid0 (sib_wn1);
      WN *sib_wn1_kid1 = WN_kid1 (sib_wn1);
      Replace_WN (sib_wn1_kid0, dummy_one);
      Replace_WN (sib_wn1_kid1, dummy_two);
      Replace_WN (wn1, sib_wn1_kid0);
      Replace_WN (sib_wn1, sib_wn1_kid1);
      LWN_Delete_Tree (wn1);
      LWN_Delete_Tree (sib_wn1);
    } else {
      WN *grand_parent = LWN_Get_Parent (LWN_Get_Parent (wn1));
      FmtAssert (NULL != grand_parent, ("Grand parent cannot be 0"));
      FmtAssert (OPR_MIN == 
		 WN_operator (grand_parent),
		 ("Grand parent must have OPR_MIN as operator"));
      Replace_WN (sib_wn1, dummy_one);
      Replace_WN (LWN_Get_Parent (wn1), sib_wn1);
      LWN_Delete_Tree (dummy_two);
      LWN_Delete_Tree (LWN_Get_Parent (wn1));
    }
    dummy_one = WN_CreateIntconst 
      (OPCODE_make_op (OPR_INTCONST, Promote_Type (index_type),
		       MTYPE_V),
       (INT64) 1);
    dummy_two = WN_CreateIntconst
      (OPCODE_make_op (OPR_INTCONST, Promote_Type (index_type),
		       MTYPE_V),
       (INT64) 2);
    WN *sib_dup_wn2 = Sibling (dup_wn2);
    if (sib_dup_wn2 == dup_wn1) {
      WN *parent = LWN_Get_Parent (dup_wn1);
      FmtAssert (OPR_MIN == WN_operator (parent),
		 ("Parent must be an OPR_MIN"));
      FmtAssert (LWN_Get_Parent (dup_wn2) == parent,
		 ("Parents of siblings must be identical"));
      Replace_WN (dup_wn1, dummy_one);
      Replace_WN (parent, dup_wn1);
      LWN_Delete_Tree (parent);
      LWN_Delete_Tree (dummy_two);
    }
    else if (OPR_MIN == WN_operator (sib_dup_wn2)) {
      WN *sib_dup_wn2_kid0 = WN_kid0 (sib_dup_wn2);
      WN *sib_dup_wn2_kid1 = WN_kid1 (sib_dup_wn2);
      Replace_WN (sib_dup_wn2_kid0, dummy_one);
      Replace_WN (sib_dup_wn2_kid1, dummy_two);
      Replace_WN (dup_wn2, sib_dup_wn2_kid0);
      Replace_WN (sib_dup_wn2, sib_dup_wn2_kid1);
      LWN_Delete_Tree (dup_wn2);
      LWN_Delete_Tree (sib_dup_wn2);
    } else {
      WN *grand_parent = LWN_Get_Parent (LWN_Get_Parent (dup_wn2));
      FmtAssert (NULL != grand_parent, ("Grand parent cannot be 0"));
      FmtAssert (OPR_MIN == 
		 WN_operator (grand_parent),
		 ("Grand parent must have OPR_MIN as operator"));
      Replace_WN (sib_dup_wn2, dummy_one);
      Replace_WN (LWN_Get_Parent (dup_wn2), sib_dup_wn2);
      LWN_Delete_Tree (LWN_Get_Parent (dup_wn2));
      LWN_Delete_Tree (dummy_two);
    }
    QUEUE<WN *> *new_stmts = gather_stmts_in_func (func_nd);
    analyze_stmts_in_func_for_if (func_nd);
    Convert_Do_Loops_Conditionals (func_nd);
  }
}

static void
Invert_Conditional (WN *wn)
{
  OPERATOR opr = WN_operator (wn);
  FmtAssert (OPR_LE == opr || OPR_LT == opr || OPR_GE == opr ||
	     OPR_GT == opr || OPR_INTCONST == opr,
	     ("Unforseen operator in conditional!"));
  TYPE_ID index_type = WN_desc (wn);
  TYPE_ID rtype = WN_rtype (wn);
  if (OPR_INTCONST == opr) {
    if (0 == WN_const_val (wn))
      WN_const_val (wn) = (INT64) 1;
    else
      WN_const_val (wn) = (INT64) 0;
  }
  else {
    WN *wn1 = WN_kid0 (wn);
    WN *wn2 = WN_kid1 (wn);
    OPERATOR new_opr;
    if (OPR_LT == opr)
      new_opr = OPR_GE;
    else if (OPR_LE == opr)
      new_opr = OPR_GT;
    else if (OPR_GT == opr)
      new_opr = OPR_LE;
    else if (OPR_GE == opr)
      new_opr = OPR_LT;
    WN *dummy1 = WN_CreateComment ("dummy1");
    WN *dummy2 = WN_CreateComment ("dummy2");
    Replace_WN (wn1, dummy1);
    Replace_WN (wn2, dummy2);
    WN *new_cond = WN_CreateExp2 (OPCODE_make_op (new_opr,
						  rtype,
						  index_type),
				  wn1, wn2);
    Replace_WN (wn, new_cond);
    LWN_Delete_Tree (wn);
  }
}

static void
Maybe_Handle_Sink_Promotion_Case (WN *if_stmt, 
				  WN *do_loop,
				  INT32 loop_depth, 
				  ACCESS_VECTOR *cond)
{
  WN *new_if;
  /* Promotion is in two parts. Say, we have a condition that 
     says that k <lb,ub> and a new conditional comes in of the 
     form k <= ub2. We need to first insert a conditional around 
     the whole thing saying that ub2 >= lb.. Similarly, in the 
     other case that we have a condition of the form k >= lb2,
     then we need to insert a conditional that says lb2 <= ub */
  {
    WN *main_if, *main_cond1, *main_cond2, *fake_unroll[2];
    WN *cond1 = canonicalize_if_condition (WN_if_test (if_stmt),
					   loop_depth);
    main_cond1 = 
      return_upper_bound (cond1,
			  SYMBOL (WN_index (do_loop)),
			  cond->Loop_Coeff (loop_depth),
			  (cond->Loop_Coeff (loop_depth) > 0));
    if (cond->Loop_Coeff (loop_depth) < 0) 
      main_cond1 = return_upper_boundplus1 (main_cond1,
					    loop_depth);

    // If we are getting a lower bound, we ought to compare with
    // the upper bound of the loop
    if (cond->Loop_Coeff (loop_depth) < 0) {
      Upper_Bound_Standardize (WN_end (do_loop), FALSE);
      main_cond2 = LWN_Copy_Tree (WN_kid1 (WN_end (do_loop)));

      fake_unroll[0] = WN_kid1 (WN_end (do_loop));
      fake_unroll[1] = main_cond2;
      Unrolled_DU_Update (fake_unroll, 2, loop_depth);
    } else {
      assert (OPR_STID == 
	      WN_operator (WN_start (do_loop)));
      main_cond2 = LWN_Copy_Tree (WN_kid0 (WN_start (do_loop)));
      fake_unroll[0] = WN_kid0 (WN_start (do_loop));
      fake_unroll[1] = main_cond2;
      Unrolled_DU_Update (fake_unroll, 2, loop_depth);
    }
#ifdef KEY
    // Bug 1183
    // It is okay to have a U4INTCONST in Fortran as long as the constant is 
    // within the range. Should not assert here incase there is a compare 
    // between I4 and U4 (when the U4 constant is within the range).
    // We will assume the U4INTCONST are within the I4INTCONST range and 
    // let them have their normal course of life and death thru the compilation
    // process.
    assert ((WN_rtype(main_cond1) == WN_rtype(main_cond2)) ||
	    (MTYPE_is_integral(WN_rtype(main_cond1)) && 
	     MTYPE_is_integral(WN_rtype(main_cond2)) &&
	     MTYPE_size_reg(WN_rtype(main_cond1)) == 
			    MTYPE_size_reg(WN_rtype(main_cond2)) &&
	     ((WN_operator(main_cond1) == OPR_INTCONST &&
	       !MTYPE_is_signed(WN_rtype(main_cond1)) && 
	       MTYPE_is_signed(WN_rtype(main_cond2))) ||
	      (WN_operator(main_cond2) == OPR_INTCONST &&
	       !MTYPE_is_signed(WN_rtype(main_cond2)) && 
	       MTYPE_is_signed(WN_rtype(main_cond1))))));
#else
    assert (WN_rtype (main_cond1) == 
	    WN_rtype (main_cond2));
#endif
    TYPE_ID index_type = WN_rtype (main_cond1);
    TYPE_ID rtype = WN_rtype (WN_if_test (if_stmt));
    WN *main_cond_to_insert;
    if (cond->Loop_Coeff (loop_depth) > 0) {
      main_cond_to_insert = WN_CreateExp2 
	(OPCODE_make_op (OPR_GE, rtype, Promote_Type (index_type)),
	 main_cond1, main_cond2);
    } else {
      main_cond_to_insert = WN_CreateExp2
	(OPCODE_make_op (OPR_LE, rtype, Promote_Type (index_type)),
	 main_cond1, main_cond2);
    }
    main_cond_to_insert = 
      Simplify_If_Conditional (main_cond_to_insert);
    Simplify_Cond_With_Div_Floor (main_cond_to_insert);
    main_if = LWN_CreateIf (main_cond_to_insert,
			    WN_CreateBlock(),
			    WN_CreateBlock());
    new_if = main_if;
    Replace_WN (do_loop, main_if);
    LWN_Insert_Block_After (WN_then (main_if), NULL, do_loop);
    IF_INFO *ii = 
      CXX_NEW (IF_INFO (&LNO_default_pool, TRUE,
			Tree_Has_Regions (main_if)),
	       &LNO_default_pool);
    WN_MAP_Set (LNO_Info_Map, main_if, (void *) ii);
    DOLOOP_STACK do_stack (shackle_if_pool);
    Build_Doloop_Stack (main_if, &do_stack);
    LNO_Build_If_Access (main_if, &do_stack);
  }
  // Rest of it works as follows: We can't resolve the conditional
  // at the current level, so, we create two conditionals at the
  // parent level - each enclosing the current loop body
  WN *if_to_insert, *delete_stmt, *ubnd, *lbnd, *delete_node;
  TYPE_ID index_type;
  TYPE_ID rtype = WN_rtype (WN_if_test (if_stmt));

  assert (cond->Loop_Coeff (loop_depth) != 0);
  WN *dup_loop = LWN_Copy_Tree (do_loop, TRUE, LNO_Info_Map,
				TRUE, shackle_if_copy_map1, TRUE);
  LWN_Insert_Block_After (NULL, do_loop, dup_loop);
  ARRAY_DIRECTED_GRAPH16 *dg = Array_Dependence_Graph;
  dg->Versioned_Dependences_Update (do_loop, dup_loop, loop_depth,
				    shackle_if_copy_map1);
  WN *fake_unroll[2];
  fake_unroll[0] = do_loop;
  fake_unroll[1] = dup_loop;
  Unrolled_DU_Update (fake_unroll, 2, loop_depth - 1);
  WN *dup_if_stmt = (WN *) WN_MAP_Get (shackle_if_copy_map1, 
				       if_stmt);
  /* The condition being promoted is some affine expression. There
     are 3 quantities involved. lb and ub are the current loop bound
     b_c is either a lower bound or an upper bound depending on the
     form of the conditional. If the conditional imposes a lower
     bound, then we create two conditionals of the form: b_c < lb
     and b_c >= lb. In the former case, the loop remains unchanged.
     In the latter case, the lower bound of the loop is replaced by
     b_c. Similarly, if b_c imposes an upper bound, we create two 
     conditionals of the form: b_c > ub and b_c <= ub. Again, in the
     first case, the loop remains the same. In the second case, the
     upper bound of the loop actually gets modified. */
  {
    WN *cond1 = canonicalize_if_condition (WN_if_test (if_stmt),
					   loop_depth);
    WN *up_cond1 = 
      return_upper_bound (cond1,
			  SYMBOL (WN_index (do_loop)),
			  cond->Loop_Coeff (loop_depth),
			  (cond->Loop_Coeff (loop_depth) > 0));
    if (cond->Loop_Coeff (loop_depth) < 0) {
      up_cond1 = return_upper_boundplus1 (up_cond1,
					  loop_depth);
    }
    WN *up_cond2;
    if (cond->Loop_Coeff (loop_depth) > 0) {
      WN *cond2 = canonicalize_if_condition (WN_end (do_loop),
					     loop_depth);
      up_cond2 = return_upper_bound (cond2,
				     SYMBOL (WN_index (do_loop)),
				     cond->Loop_Coeff (loop_depth),
				     TRUE);
    } else {
      assert (OPR_STID == 
	      WN_operator (WN_start (do_loop)));
      up_cond2 = LWN_Copy_Tree (WN_kid0 (WN_start (do_loop)));
      fake_unroll[0] = WN_kid0 (WN_start (do_loop));
      fake_unroll[1] = up_cond2;
      Unrolled_DU_Update (fake_unroll, 2, loop_depth);
    }
#ifdef KEY
    // Bug 1183
    // It is okay to have a U4INTCONST in Fortran as long as the constant is 
    // within the range. Should not assert here incase there is a compare 
    // between I4 and U4 (when the U4 constant is within the range).
    // We will assume the U4INTCONST are within the I4INTCONST range and 
    // let them have their normal course of life and death thru the compilation
    // process.
    assert ((WN_rtype(up_cond1) == WN_rtype(up_cond2)) ||
	    (MTYPE_is_integral(WN_rtype(up_cond1)) && 
	     MTYPE_is_integral(WN_rtype(up_cond2)) &&
	     MTYPE_size_reg(WN_rtype(up_cond1)) == 
			    MTYPE_size_reg(WN_rtype(up_cond2)) &&
	     ((WN_operator(up_cond1) == OPR_INTCONST &&
	       !MTYPE_is_signed(WN_rtype(up_cond1)) && 
	       MTYPE_is_signed(WN_rtype(up_cond2))) ||
	      (WN_operator(up_cond2) == OPR_INTCONST &&
	       !MTYPE_is_signed(WN_rtype(up_cond2)) && 
	       MTYPE_is_signed(WN_rtype(up_cond1))))));
#else
    assert (WN_rtype (up_cond1) == 
	    WN_rtype (up_cond2));
#endif
    TYPE_ID index_type = WN_rtype (up_cond1);
    WN *cond_to_insert;
    if (cond->Loop_Coeff (loop_depth) > 0) {
      cond_to_insert = WN_CreateExp2 
	(OPCODE_make_op (OPR_GT, rtype, Promote_Type (index_type)),
	 up_cond1, up_cond2);
    } else {
      cond_to_insert = WN_CreateExp2 
	(OPCODE_make_op (OPR_LT, rtype, Promote_Type (index_type)),
	 up_cond1, up_cond2);
    }
    cond_to_insert = Simplify_If_Conditional (cond_to_insert);
    Simplify_Cond_With_Div_Floor (cond_to_insert);
    if_to_insert = LWN_CreateIf (cond_to_insert,
				 WN_CreateBlock(),
				 WN_CreateBlock());
    Replace_WN (do_loop, if_to_insert);
    LWN_Insert_Block_After (WN_then (if_to_insert), NULL, 
			    do_loop);
  }
  IF_INFO *ii = 
    CXX_NEW (IF_INFO (&LNO_default_pool, TRUE, 
		      Tree_Has_Regions (if_to_insert)),
	     &LNO_default_pool);
  WN_MAP_Set (LNO_Info_Map, if_to_insert, (void *) ii);
  DOLOOP_STACK do_stack (shackle_if_pool);
  Build_Doloop_Stack (if_to_insert, &do_stack);
  LNO_Build_If_Access (if_to_insert, &do_stack);

  /* The second conditional. This is more tricky. Not only have
     we to get the conditional right, we also need to update the
     appropriate loop bound to mirror the conditional */
  {
    WN *cond1 = canonicalize_if_condition (WN_if_test (dup_if_stmt),
					   loop_depth);
    WN *up_cond1 = 
      return_upper_bound (cond1, 
			  SYMBOL (WN_index (dup_loop)),
			  cond->Loop_Coeff (loop_depth),
			  (cond->Loop_Coeff (loop_depth) > 0));
    if (cond->Loop_Coeff (loop_depth) < 0) 
      up_cond1 = return_upper_boundplus1 (up_cond1,
					  loop_depth);
    WN *up_cond2;
    if (cond->Loop_Coeff (loop_depth) > 0) {
      WN *cond2 = canonicalize_if_condition (WN_end (dup_loop),
					     loop_depth);
      up_cond2 = return_upper_bound (cond2,
				     SYMBOL (WN_index (dup_loop)),
				     cond->Loop_Coeff (loop_depth),
				     TRUE);
    } else {
      assert (OPR_STID == 
	      WN_operator (WN_start (dup_loop)));
      up_cond2 = LWN_Copy_Tree (WN_kid0 (WN_start (dup_loop)));
      fake_unroll[0] = WN_kid0 (WN_start (dup_loop));
      fake_unroll[1] = up_cond2;
      Unrolled_DU_Update (fake_unroll, 2, loop_depth);
    }
#ifdef KEY
    // Bug 1183
    // It is okay to have a U4INTCONST in Fortran as long as the constant is 
    // within the range. Should not assert here incase there is a compare 
    // between I4 and U4 (when the U4 constant is within the range).
    // We will assume the U4INTCONST are within the I4INTCONST range and 
    // let them have their normal course of life and death thru the compilation
    // process.
    assert ((WN_rtype(up_cond1) == WN_rtype(up_cond2)) ||
	    (MTYPE_is_integral(WN_rtype(up_cond1)) && 
	     MTYPE_is_integral(WN_rtype(up_cond2)) &&
	     MTYPE_size_reg(WN_rtype(up_cond1)) == 
			    MTYPE_size_reg(WN_rtype(up_cond2)) &&
	     ((WN_operator(up_cond1) == OPR_INTCONST &&
	       !MTYPE_is_signed(WN_rtype(up_cond1)) && 
	       MTYPE_is_signed(WN_rtype(up_cond2))) ||
	      (WN_operator(up_cond2) == OPR_INTCONST &&
	       !MTYPE_is_signed(WN_rtype(up_cond2)) && 
	       MTYPE_is_signed(WN_rtype(up_cond1))))));
#else
    assert (WN_rtype (up_cond1) == 
	    WN_rtype (up_cond2));
#endif
    TYPE_ID index_type = WN_rtype (up_cond1);
    WN *cond_to_insert;
    if (cond->Loop_Coeff (loop_depth) > 0) 
      cond_to_insert = WN_CreateExp2 
	(OPCODE_make_op (OPR_GT, rtype, Promote_Type (index_type)),
	 up_cond1, up_cond2);
    else 
      cond_to_insert = WN_CreateExp2
	(OPCODE_make_op (OPR_LT, rtype, Promote_Type (index_type)),
	 up_cond1, up_cond2);
    cond_to_insert = Simplify_If_Conditional (cond_to_insert);
    Simplify_Cond_With_Div_Floor (cond_to_insert);
    if_to_insert = LWN_CreateIf (cond_to_insert,
				 WN_CreateBlock(),
				 WN_CreateBlock());
    Invert_Conditional (cond_to_insert);
    Replace_WN (dup_loop, if_to_insert);
    LWN_Insert_Block_After (WN_then (if_to_insert), NULL,
			    dup_loop);
  }
  ii = CXX_NEW (IF_INFO (&LNO_default_pool, TRUE,
			 Tree_Has_Regions (if_to_insert)),
		&LNO_default_pool);
  WN_MAP_Set (LNO_Info_Map, if_to_insert, (void *) ii);
  Build_Doloop_Stack (if_to_insert, &do_stack);
  LNO_Build_If_Access (if_to_insert, &do_stack);
  // The last step..
  {
    WN *dummy1, *dummy2, *dummy3;
    TYPE_ID index_type, rtype;
    WN *dup_if = LWN_Copy_Tree (new_if, TRUE, LNO_Info_Map,
				TRUE, shackle_if_copy_map1);
    LWN_Insert_Block_After (NULL, new_if, dup_if);
    ARRAY_DIRECTED_GRAPH16 *dg = Array_Dependence_Graph;
    dg->Versioned_Dependences_Update (new_if, dup_if, 
				      loop_depth, 
				      shackle_if_copy_map1);
    WN *fake_unroll[2];
    fake_unroll[0] = new_if;
    fake_unroll[1] = dup_if;
    Unrolled_DU_Update (fake_unroll, 2, loop_depth - 1);
    // Next, we need to invert the sense of the conditional..
    WN *cond = WN_if_test (dup_if);
    Invert_Conditional (cond);
    ii = CXX_NEW (IF_INFO (&LNO_default_pool, TRUE,
			   Tree_Has_Regions (dup_if)),
		  &LNO_default_pool);
    WN_MAP_Set (LNO_Info_Map, dup_if, (void *) ii);
    Build_Doloop_Stack (dup_if, &do_stack);
    LNO_Build_If_Access (dup_if, &do_stack);
  }
  for (INT32 i = 0; i < 2; i++) {
  }
}

static QUEUE<WN *> *
Shackleable_Ifs_Surrounding_Stmt(WN *wn)
{
  QUEUE<WN *> *result = 
    CXX_NEW (QUEUE<WN *> (shackle_if_pool), shackle_if_pool);
  WN *step = wn;
  BOOL this_if_bad;

  while (NULL != step) {
    this_if_bad = FALSE;
    if (OPC_IF == WN_opcode (step) && Shackle_If_Unseen (step)) {
      IF_INFO *ii = (IF_INFO *) WN_MAP_Get (LNO_Info_Map, step);
      ACCESS_ARRAY *ar = ii->Condition;
      if (ar->Too_Messy)
	this_if_bad = TRUE;
      else 
	for (INT32 i = 0; i < ar->Num_Vec(); i++)
	  if (ar->Dim (i)->Too_Messy) {
	    this_if_bad = TRUE;
	    break;
	  }
      if (!this_if_bad)
	result->Add_Tail_Q (step);
      else
	Shackle_If_Seen_Set (step);
    }
    step = LWN_Get_Parent (step);
  }
  return result;
}

static INT32 
Shackle_Do_Depth_For_If(WN *if_stmt)
{
  FmtAssert (OPC_IF == WN_opcode (if_stmt),
	     ("Shackle_Do_Depth_For_If called with non if!"));
  INT32 i, j;
  IF_INFO *ii = (IF_INFO *) WN_MAP_Get (LNO_Info_Map, if_stmt);
  ACCESS_ARRAY *ar = ii->Condition;
  if (ar->Too_Messy)
    return -1;
  for (j = 0; j < ar->Num_Vec(); j++) 
    if (ar->Dim(j)->Too_Messy)
      return -1;
  INT32 max_depth = ar->Dim(0)->Nest_Depth();
  for (i = max_depth; i >= 0; i--) {
    for (j = 0; j < ar->Num_Vec(); j++) 
      if (ar->Dim(j)->Loop_Coeff (i) != 0)
	return i;
  }
  // Must be a constant conditional. Return 0 to pretend as a 
  // highest priority conditional
  return 0;
}


static WN*
Shackle_Unseen_Highest_Enclosing_If(WN *stmt)
{
  QUEUE<WN *> *enclosing_ifs = 
    Shackleable_Ifs_Surrounding_Stmt (stmt);
  if (enclosing_ifs->Queue_Isempty())
    return NULL;
  QUEUE_ITER<WN *> iter(enclosing_ifs);
  WN              *step;
  BOOL             found_if = FALSE;
  INT32            depth, min;
  WN              *result_if = NULL;

  while (iter.Step (&step)) {
    if (Shackle_If_Unseen (step)) {
      depth = Shackle_Do_Depth_For_If (step);
      if (-1 != depth && !found_if) {
	found_if = TRUE;
	result_if = step;
	min = depth;
      }
      else if (-1 != depth && depth <= min) {
	min = depth;
	result_if = step;
      }
    }
  }
  return result_if;
}



void
shackle_if_init(MEM_POOL *pool)
{
  shackle_if_pool = pool;
  MEM_POOL_Push (shackle_if_pool);
  shackle_if_copy_map1 = WN_MAP_Create (&shackle_map_pool);
  shackle_if_if_map = WN_MAP32_Create (&shackle_map_pool);
  if (Get_Trace(TP_LNOPT2, TT_SHACKLE_DEBUG))
    shackle_if_debug_level = 1;
  else
    shackle_if_debug_level = 0;
}

void
shackle_if_finalize()
{
  WN_MAP_Delete (shackle_if_if_map);
  WN_MAP_Delete (shackle_if_copy_map1);
  MEM_POOL_Pop (shackle_if_pool);
}

