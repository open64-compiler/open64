/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
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


#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include <sys/types.h>

#include "lnopt_main.h"
#include "config.h"
#include "config_lno.h"
#include "strtab.h"
#include "stab.h"
#include "targ_const.h"

#include "lnoutils.h"
#include "wn_simp.h"
#include "stdlib.h"
#include "lwn_util.h"
#include "optimizer.h"
#include "opt_du.h"
#include "name.h"
#include "forward.h"
#include "ir_reader.h" /* fdump_tree */
#include "glob.h"      /* Cur_PU_Name */

static DU_MANAGER* du = NULL; 

static UINT32 IFMM_Count;
static UINT32 IFMM_CleanUp_Count;

//-----------------------------------------------------------------------
// NAME: Matching_Addresses
// FUNCTION: Returns TRUE if 'wn_array_one' and 'wn_array_two' point
//   to the same location. 
//-----------------------------------------------------------------------

static BOOL Matching_Addresses(WN* wn_array_one,
                               WN* wn_array_two)
{
  WN* wn_base_one = WN_array_base(wn_array_one); 
  WN* wn_base_two = WN_array_base(wn_array_two); 
  ST* st_base_one(Get_ST_Base(wn_base_one)); 
  ST* st_base_two(Get_ST_Base(wn_base_two)); 
  BOOL same_base = (st_base_one == NULL || st_base_two == NULL)
    ? st_base_one == st_base_two 
    : ST_base(st_base_one) == ST_base(st_base_two)
    && ST_ofst(st_base_one) == ST_ofst(st_base_two);
  if (!same_base) 
    return FALSE; 
  ACCESS_ARRAY* aa_one = (ACCESS_ARRAY*) WN_MAP_Get(LNO_Info_Map, 
    wn_array_one);
  ACCESS_ARRAY* aa_two = (ACCESS_ARRAY*) WN_MAP_Get(LNO_Info_Map, 
    wn_array_two);
  if (!(*aa_one == *aa_two))
    return FALSE; 
  return TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: Matching_Store_Load
// FUNCTION: Returns TRUE if 'wn_store' and 'wn_load' reference
//   to the same location. 
//-----------------------------------------------------------------------

static BOOL Matching_Store_Load(WN* wn_store,
			        WN* wn_load)
{
  OPERATOR opr_store = WN_operator(wn_store);
  OPERATOR opr_load = WN_operator(wn_load);
  if (opr_store == OPR_STID && opr_load == OPR_LDID)    
    return SYMBOL(wn_store) == SYMBOL(wn_load); 
  if (opr_store == OPR_ISTORE && opr_load == OPR_ILOAD) 
  {
    WN* wn_array_one = WN_kid1(wn_store); 
    WN* wn_array_two = WN_kid0(wn_load); 

    if (WN_Simp_Compare_Trees(wn_array_one, wn_array_two) == 0)
       return TRUE;

    if (WN_operator(wn_array_one) != OPR_ARRAY)
      return FALSE; 

    if (WN_operator(wn_array_two) != OPR_ARRAY)
      return FALSE; 

    return Matching_Addresses(wn_array_one,
                              wn_array_two);
  }
  return FALSE; 
}

//-----------------------------------------------------------------------
// NAME: Matching_Stores
// FUNCTION: Returns TRUE if 'wn_store_one' and 'wn_store_two' are stores
//   to the same location. 
//-----------------------------------------------------------------------

static BOOL Matching_Stores(WN* wn_store_one, 
			    WN* wn_store_two)
{
  OPCODE op_one = WN_opcode(wn_store_one); 
  OPCODE op_two = WN_opcode(wn_store_two); 
  if (op_one != op_two)
    return FALSE; 
  OPERATOR opr = WN_operator(wn_store_one);
  if (opr == OPR_STID)    
    return SYMBOL(wn_store_one) == SYMBOL(wn_store_two); 
  if (opr == OPR_ISTORE) {

    WN* wn_array_one = WN_kid1(wn_store_one); 
    WN* wn_array_two = WN_kid1(wn_store_two); 

    if (WN_Simp_Compare_Trees(wn_array_one, wn_array_two) == 0)
       return TRUE;

    if (WN_operator(wn_array_one) != OPR_ARRAY)
      return FALSE; 

    if (WN_operator(wn_array_two) != OPR_ARRAY)
      return FALSE; 

    return Matching_Addresses(wn_array_one,
                              wn_array_two);
  }
  return FALSE; 
}

// NAME: Store_Expr
// FUNCTION: Returns the expression being stored into 'wn_store'. 
//-----------------------------------------------------------------------

static WN* Store_Expr(WN* wn_store)
{
  switch (WN_operator(wn_store)) {
  case OPR_STID: 
  case OPR_ISTORE:
    return WN_kid0(wn_store);
  default: 
    FmtAssert(TRUE, ("Store_Expr(): Don't understand this store type")); 
    return NULL; 
  } 
} 

//-----------------------------------------------------------------------
// NAME: Matching_Exprs
// FUNCTION: Returns TRUE if 'wn_one' and 'wn_two' are equivalent ex- 
//   pressions. 
//-----------------------------------------------------------------------

static BOOL Matching_Exprs(WN* wn_one, 
			   WN* wn_two)
{
  BOOL ret = WN_Simp_Compare_Trees(wn_one, wn_two) == 0;

  if (ret) return TRUE;

  if (WN_operator(wn_one) == OPR_INTCONST &&
      WN_operator(wn_two) == OPR_INTCONST )
    ret = WN_const_val(wn_one) == WN_const_val(wn_two);

  return ret;
}

//-----------------------------------------------------------------------
// NAME: CleanUP_IntConsant_Off_One
// FUNCTION: Returns TRUE if we can change the INTCONST in wn_test, 
//   by +1 or -1, to march the wn_test. TURE means wn_test is
//   changed. FALSE means wn_test is the same.
//-----------------------------------------------------------------------

BOOL CleanUp_IntContant_Off_One(WN* wn_expr,
                                WN* wn_test)
{
  if (WN_operator(wn_expr) != OPR_INTCONST)
    return FALSE;

  if (IFMM_CleanUp_Count >= LNO_IfMinMax_Fix_Cond_Limit)
    return FALSE;

  OPERATOR opr = WN_operator(wn_test);
  Is_True(opr == OPR_LT || opr == OPR_LE || opr == OPR_GT || opr == OPR_GE,
          ("Unexpected OPR"));

  INT64 expr_val = WN_const_val(wn_expr);
  WN* wn_left = WN_kid0(wn_test);
  WN* wn_right = WN_kid1(wn_test);

  BOOL return_value = FALSE;

  INT64 test_expr_val;
  if (WN_operator(wn_left) == OPR_INTCONST)
  {
    test_expr_val = WN_const_val(wn_left);

    if (test_expr_val > expr_val)
    {
      if (test_expr_val - expr_val == 1)
      {
        if (opr == OPR_LT || opr == OPR_GE)
        {

          LWN_Delete_Tree(wn_left);
          WN* new_val = LWN_Copy_Tree(wn_expr);
          WN_kid0(wn_test) = new_val;
          LWN_Set_Parent(new_val, wn_test);
          WN_set_operator(wn_test, (opr == OPR_LT ? OPR_LE : OPR_GT));
          ++IFMM_CleanUp_Count;
          return_value = TRUE;
        }
      }
    }
    else
    {
      if (expr_val - test_expr_val == 1)
      {
        if (opr == OPR_LE || opr == OPR_GT)
        {
          LWN_Delete_Tree(wn_left);
          WN* new_val = LWN_Copy_Tree(wn_expr);
          WN_kid0(wn_test) = new_val;
          LWN_Set_Parent(new_val, wn_test);
          WN_set_operator(wn_test, (opr == OPR_LE ? OPR_LT : OPR_GE));
          ++IFMM_CleanUp_Count;
          return_value = TRUE;
        }
      }
    }
  }
  else if (WN_operator(wn_right) == OPR_INTCONST)
  {
    test_expr_val = WN_const_val(wn_right);

    if (test_expr_val > expr_val)
    {
      if (test_expr_val - expr_val == 1)
      {
        if (opr == OPR_LT || opr == OPR_GE)
        {
          LWN_Delete_Tree(wn_right);
          WN* new_val = LWN_Copy_Tree(wn_expr);
          WN_kid1(wn_test) = new_val;
          LWN_Set_Parent(new_val, wn_test);
          WN_set_operator(wn_test, (opr == OPR_LT ? OPR_LE : OPR_GT));
          ++IFMM_CleanUp_Count;
          return_value = TRUE;
        }
      }
    }
    else
    {
      if (expr_val - test_expr_val == 1)
      {
        if (opr == OPR_LE || opr == OPR_GT)
        {
          LWN_Delete_Tree(wn_right);
          WN* new_val = LWN_Copy_Tree(wn_expr);
          WN_kid1(wn_test) = new_val;
          LWN_Set_Parent(new_val, wn_test);
          WN_set_operator(wn_test, (opr == OPR_LE ? OPR_LT : OPR_GE));
          ++IFMM_CleanUp_Count;
          return_value = TRUE;
        }
      }
    }
  }

  if (return_value && LNO_IfMinMax_Trace >=2)
  {
    fprintf(stderr, "[IFMM]: Cond (%d / %d) test cond is modified to:\n",
                        IFMM_CleanUp_Count-1, LNO_IfMinMax_Fix_Cond_Limit);
    fdump_tree(stderr,wn_test);
  }

  return return_value;
}

static WN* 
Find_Prev_Single_Def(WN* def)
{
  USE_LIST *use_list = du->Du_Get_Use(def);

  USE_LIST_ITER u_iter(use_list);
  if (u_iter.Is_Empty())
    return 0;

  // I think checking one use is enough ...
  DU_NODE* dnode = u_iter.First();
  WN *use = dnode->Wn();

  DEF_LIST *def_list = du->Ud_Get_Def(use);
  if (def_list == NULL || def_list->Incomplete())
    return 0;

  DEF_LIST_ITER d_iter(def_list);

  INT def_count = 0;
  BOOL meet_this_def_ = FALSE;
  WN* the_other_def = 0;
  for (dnode = d_iter.First(); !d_iter.Is_Empty();
        dnode = d_iter.Next())
  {
     WN* def1 = dnode->Wn();
     if (def1 == def) 
       meet_this_def_ = TRUE;
     else
       the_other_def = def1;

     ++def_count;
  }

  if (def_count != 2 || !meet_this_def_)
    return 0;

  return the_other_def;
}

static void IFMM_From_Select(WN* wn_tree)
{
  OPERATOR tree_opr = WN_operator(wn_tree);
  Is_True(tree_opr == OPR_STID || tree_opr == OPR_ISTORE, ("error")); 
  WN *rhs = Store_Expr(wn_tree);
  Is_True( WN_operator(rhs) == OPR_SELECT, ("error")); 

  LWN_Simplify_Tree(WN_kid0(rhs));
  WN* wn_test = WN_kid0(rhs);
  OPERATOR opr = WN_operator(wn_test);
  if (!(opr == OPR_LT || opr == OPR_LE || opr == OPR_GT || opr == OPR_GE))
    return;
  
  WN* wn_expr_left = WN_kid0(wn_test);
  WN* wn_expr_right = WN_kid1(wn_test);

#if defined (TARG_SL) && defined (EMULATE_FLOAT_POINT)
  /* Don't generate F8/F4 MIN/MAX */
  if (MTYPE_is_float(WN_rtype(wn_expr_left)) || MTYPE_is_float(WN_rtype(wn_expr_right)))
    return;
#endif


  WN* wn_expr_then = WN_kid1(rhs);
  WN* wn_expr_else = WN_kid(rhs,2);

  BOOL mm_max;
  if (Matching_Exprs(wn_expr_left, wn_expr_then) &&
      Matching_Exprs(wn_expr_right, wn_expr_else))
  {
    mm_max = opr == OPR_GT || opr == OPR_GE; 
  } else 
  if (Matching_Exprs(wn_expr_left, wn_expr_else) &&
      Matching_Exprs(wn_expr_right, wn_expr_then))
  {
    mm_max = opr == OPR_LT || opr == OPR_LE; 
  }
  else return;

  if (LNO_IfMinMax_Trace > 1)
  {
    fprintf(stderr, "[IFMM] (%d / %d) DumpBefore:\n", IFMM_Count, LNO_IfMinMax_Limit);
    fdump_tree(stderr,wn_tree);
  }

  TYPE_ID type_cmp = Max_Wtype(WN_rtype(wn_expr_then), 
                               WN_rtype(wn_expr_else));

  OPCODE op = OPCODE_make_op(mm_max ? OPR_MAX : OPR_MIN, type_cmp, MTYPE_V); 

  WN* wn_cmp = LWN_CreateExp2(op, wn_expr_then, wn_expr_else); 
  
  WN_kid0(wn_tree) = wn_cmp;
  LWN_Set_Parent(wn_cmp, wn_tree);
  
  LWN_Delete_DU(wn_test);
  LWN_Delete_Tree(wn_test);

  ++IFMM_Count;
  if (LNO_IfMinMax_Trace > 1)
  {
    fprintf(stderr, "[IFMM] DumpAfter:\n");
    fdump_tree(stderr, wn_tree);
  }
}

//-----------------------------------------------------------------------
// NAME: IFMM_Convertible
// FUNCTION: Returns TRUE if 'wn_tree' is an OPR_IF which can be converted
//   into an OPR_MIN or OPR_MAX.  If so, '*if_mm_max' is set to TRUE if it
//   can be converted to an OPR_MAX , to FALSE if it can be converted
//   to an OPR_MIN.  If no conversion is possible, returns FALSE.  
//-----------------------------------------------------------------------


static BOOL IFMM_Convertible(WN* wn_tree, 
			     BOOL* if_mm_max) 
{
  switch (WN_operator(wn_tree)) {
  case OPR_IF:
     break;
  case OPR_STID: 
  case OPR_ISTORE:
    {
      WN *rhs = Store_Expr(wn_tree);
      if (WN_operator(rhs) == OPR_SELECT)
        IFMM_From_Select(wn_tree);
    }
    return FALSE;
  default: 
    return FALSE;
  }

  if (IFMM_Count >= LNO_IfMinMax_Limit) return false;

  if (LNO_IfMinMax_Trace == 3)
  {
    fprintf(stderr, "[IFMM] (%d / %d) DumpBefore:\n", IFMM_Count, LNO_IfMinMax_Limit);
    fdump_tree(stderr,wn_tree);
  }

  LWN_Simplify_Tree(WN_if_test(wn_tree));
  WN* wn_test = WN_if_test(wn_tree);
  OPERATOR opr = WN_operator(wn_test);
  if (!(opr == OPR_LT || opr == OPR_LE || opr == OPR_GT || opr == OPR_GE))
    return FALSE;

  if (WN_first(WN_then(wn_tree)) == NULL)
    return FALSE;  
  if (WN_next(WN_first(WN_then(wn_tree))) != NULL)
    return FALSE;  
  LWN_Simplify_Tree(WN_first(WN_then(wn_tree)));

  WN* wn_st_then = WN_first(WN_then(wn_tree));  
  WN* wn_expr_then = Store_Expr(wn_st_then);

  if (!wn_expr_then) return FALSE;

  if (LNO_IfMinMax_Trace == 2)
  {
    fprintf(stderr, "[IFMM] (%d / %d) DumpBefore:\n", IFMM_Count, LNO_IfMinMax_Limit);
    fdump_tree(stderr,wn_tree);
  }

  BOOL expr_else_expect_left; // record what is expected.

  WN* wn_expr_left = WN_kid0(wn_test);
  WN* wn_expr_right = WN_kid1(wn_test);

#if defined (TARG_SL) && defined (EMULATE_FLOAT_POINT)
  /* Don't generate F8/F4 MIN/MAX */
  if (MTYPE_is_float(WN_rtype(wn_expr_left)) || MTYPE_is_float(WN_rtype(wn_expr_right)))
    return FALSE;
#endif

  BOOL mm_max;
 
  // we use this bool to make sure we only clean up once
  // for intConstant off by one. Cleanup twice means to
  // branches are both constant, which should not happen.
  BOOL had_cleanup = false;

  if (CleanUp_IntContant_Off_One(wn_expr_then, wn_test))
  {
    // wn_test changed; reload
    wn_expr_left = WN_kid0(wn_test);
    wn_expr_right = WN_kid1(wn_test);
    had_cleanup = true;
  }

  if (Matching_Exprs(wn_expr_left, wn_expr_then)) 
  { 
    mm_max = opr == OPR_GT || opr == OPR_GE; 
    expr_else_expect_left = FALSE;
  }else if (Matching_Exprs(wn_expr_right, wn_expr_then)) 
  {
    mm_max = opr == OPR_LT || opr == OPR_LE; 
    expr_else_expect_left = TRUE; 
  }else
    return FALSE;

  BOOL return_value = FALSE;

  if (WN_first(WN_else(wn_tree)) != NULL)
  {
    if (WN_next(WN_first(WN_else(wn_tree))) != NULL)
      return FALSE; 

    LWN_Simplify_Tree(WN_first(WN_else(wn_tree)));
    WN* wn_st_else = WN_first(WN_else(wn_tree));  

    if (!Matching_Stores(wn_st_then, wn_st_else))
      return FALSE;

    WN* wn_expr_else = Store_Expr(wn_st_else);
    if (!wn_expr_else) 
      return FALSE;

    if (!had_cleanup)
    {
      if (CleanUp_IntContant_Off_One(wn_expr_else, wn_test))
      {
        wn_expr_left = WN_kid0(wn_test);
        wn_expr_right = WN_kid1(wn_test);
      }
    }

    WN* wn_expr_else_expect = expr_else_expect_left ? 
                       wn_expr_left : wn_expr_right;

    if (Matching_Exprs(wn_expr_else,wn_expr_else_expect)) 
      return_value = TRUE;
  }
  else 
  {
    if (Matching_Store_Load(wn_st_then,wn_expr_left) || 
        Matching_Store_Load(wn_st_then,wn_expr_right)) 
    {
      // this should not happen, but we check it anyway...
      if (Matching_Store_Load(wn_st_then,wn_expr_then)) 
        return FALSE;
      return_value = TRUE;
    }
    else
    {
      //we check the previous statement that defines the same variable. 
      WN* wn_prev_stmt = Find_Prev_Single_Def(wn_st_then);
      if (!wn_prev_stmt) 
      {
        wn_prev_stmt = WN_prev(wn_tree);
        if (!wn_prev_stmt) 
          return FALSE; 
      }

      if (!Matching_Stores(wn_prev_stmt, wn_st_then))
        return FALSE;

      WN* wn_expr_prev = Store_Expr(wn_prev_stmt);
      if (!wn_expr_prev) 
        return FALSE;

      if (!had_cleanup)
      {
        if (CleanUp_IntContant_Off_One(wn_expr_prev, wn_test))
        {
          wn_expr_left = WN_kid0(wn_test);
          wn_expr_right = WN_kid1(wn_test);
        }
      }

      WN* wn_expr_else_expect = expr_else_expect_left ? 
                       wn_expr_left : wn_expr_right;

      if (Matching_Exprs(wn_expr_prev,wn_expr_else_expect)) 
        return_value = TRUE;
    }
  }

  *if_mm_max = mm_max;

  if (return_value) ++IFMM_Count;
    
  return return_value;
}  

//-----------------------------------------------------------------------
// NAME: IFMM_Convert
// FUNCTION: Converts the tree rooted at 'wn_if' to expression beginning 
//   with OPR_MIN if 'ifmm_max' is FALSE or an expression beginning with
//   with OPR_MAX if 'ifmm_max' is TRUE. 
//-----------------------------------------------------------------------

static WN* IFMM_Convert(WN* wn_if, 
			BOOL ifmm_max)
{
  if (LNO_IfMinMax_Trace)
    fprintf(stderr, "[IFMM]: Performed transformation for the IF at file %d@%s (pu=%s)\n",
            Srcpos_To_Line(WN_linenum(wn_if)),
            Irb_File_Name, Cur_PU_Name);

  WN* wn_left = WN_kid0(WN_if_test(wn_if));
  WN* wn_right = WN_kid1(WN_if_test(wn_if));
  WN* wn_result = WN_first(WN_then(wn_if));
  WN* wn_result_expr = Store_Expr(wn_result); 
  INT i;
  for (i = 0; i < WN_kid_count(wn_result_expr); i++) 
    if (WN_kid(LWN_Get_Parent(wn_result_expr), i) == wn_result_expr)
      break;
  INT kid_count = i; 
  TYPE_ID type_cmp = WN_desc(WN_if_test(wn_if));
  OPCODE op = OPCODE_make_op(ifmm_max ? OPR_MAX : OPR_MIN, type_cmp, MTYPE_V); 
  WN* wn_cmp = LWN_CreateExp2(op, wn_left, wn_right); 
  WN_kid0(WN_if_test(wn_if)) = NULL; 
  WN_kid1(WN_if_test(wn_if)) = NULL; 
  LWN_Set_Parent(wn_cmp, wn_result);
  WN_kid(wn_result, kid_count) = wn_cmp; 

  LWN_Extract_From_Block(wn_result); 
  LWN_Insert_Block_Before(LWN_Get_Parent(wn_if), wn_if, wn_result); 

  WN* else_part = WN_first(WN_else(wn_if));
  if (else_part)
    LWN_Delete_DU(else_part);

  LWN_Extract_From_Block(wn_if);
  LWN_Delete_Tree(wn_if); 

  LWN_Delete_DU(wn_result_expr);
  LWN_Delete_Tree(wn_result_expr);

  if (LNO_IfMinMax_Trace>1)
  {
    fprintf(stderr, "[IFMM] DumpAfter:\n");
    fdump_tree(stderr, wn_result);
  }

  return wn_result; 
} 

//-----------------------------------------------------------------------
// NAME: Is_Loop_Lower_Bound
// FUNCTION: Returns TRUE if 'wn_use' is a lower bound of a OPC_DO_LOOP, 
//   (i.e. the WN_kid0(WN_start(wn_loop)) of some loop wn_loop).  Returns
//   FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Is_Loop_Lower_Bound(WN* wn_use)
{ 
  if (WN_operator(wn_use) != OPR_LDID)
    return FALSE; 
  WN* wn_start = LWN_Get_Parent(wn_use); 
  if (wn_start == NULL) 
    return FALSE; 
  WN* wn_loop = LWN_Get_Parent(wn_start);
  if (wn_loop == NULL) 
    return FALSE; 
  if (WN_opcode(wn_loop) != OPC_DO_LOOP)
    return FALSE; 
  if (wn_start != WN_start(wn_loop))
    return FALSE; 
  if (wn_use != WN_kid0(wn_start))
    return FALSE; 
  return TRUE;
} 

//-----------------------------------------------------------------------
// NAME: Is_Loop_Upper_Bound
// FUNCTION: Returns TRUE if 'wn_use' is the upper bound of an OPC_DO_LOOP,
//   (i.e. the UBexp(WN_end(wn_loop)) of some loop wn_loop).  Returns 
//   FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Is_Loop_Upper_Bound(WN* wn_use)
{
  if (WN_operator(wn_use) != OPR_LDID)
    return FALSE;
  WN* wn_end = LWN_Get_Parent(wn_use);
  if (wn_end == NULL) 
    return FALSE; 
  WN* wn_loop = LWN_Get_Parent(wn_end); 
  if (wn_loop == NULL) 
    return FALSE; 
  if (WN_opcode(wn_loop) != OPC_DO_LOOP)
    return FALSE; 
  if (wn_end != WN_end(wn_loop))
    return FALSE; 
  if (wn_use != UBexp(WN_end(wn_loop)))
    return FALSE; 
  return TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: IFMM_Sink
// FUNCTION: Attempt to sink the expression rooted at 'wn_max_store' 
//   into any loop bounds for which it is a defintion. 
//-----------------------------------------------------------------------

static void IFMM_Sink(WN* wn_max_store)
{ 
  if (WN_operator(wn_max_store) != OPR_STID)
    return; 
  USE_LIST *use_list = du->Du_Get_Use(wn_max_store);
  if (use_list == NULL) 
    return;
  USE_LIST_ITER iter(use_list);
  const DU_NODE* node = NULL;
  const DU_NODE* nnode = NULL;
  for (node = iter.First(); !iter.Is_Empty(); node = nnode) {
    WN* wn_use = node->Wn();
    nnode = iter.Next();
    if (Is_Loop_Lower_Bound(wn_use)) {
      WN* wn_loop = LWN_Get_Parent(LWN_Get_Parent(wn_use)); 
      Forward_Substitute_Ldids(wn_use, du);
      DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 
      if (Bound_Is_Too_Messy(dli->LB))
	Hoist_Bounds_One_Level(wn_loop); 
    } else if (Is_Loop_Upper_Bound(wn_use)) { 
      WN* wn_loop = LWN_Get_Parent(LWN_Get_Parent(wn_use)); 
      Forward_Substitute_Ldids(wn_use, du);
      DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 
      if (Bound_Is_Too_Messy(dli->UB))
	Hoist_Bounds_One_Level(wn_loop); 
    } 
  }
} 

//-----------------------------------------------------------------------
// NAME: If_MinMax_Traverse
// FUNCTION: Traverse the 'wn_tree', attempting to convert IF statements
//  into MIN and MAX statements and sinking them into the loop bounds. 
//-----------------------------------------------------------------------

static void If_MinMax_Traverse(WN* wn_tree)
{
  BOOL ifmm_max = FALSE; 
  if (IFMM_Convertible(wn_tree, &ifmm_max)) {
    WN* wn_max_store = IFMM_Convert(wn_tree, ifmm_max);
    IFMM_Sink(wn_max_store);
    return; 
  } 

  if (WN_opcode(wn_tree) == OPC_BLOCK) { 
    WN* wnn = NULL; 
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = wnn) {
      wnn = WN_next(wn); 
      If_MinMax_Traverse(wn); 
    } 
  } else { 
    for (INT i = 0; i < WN_kid_count(wn_tree); i++) 
      If_MinMax_Traverse(WN_kid(wn_tree, i)); 
  } 
} 

//-----------------------------------------------------------------------
// NAME: If_MinMax
// FUNCTION: Convert expressions of the form: 
//   if (expr1 .relop. expr2) then 
//     result = expr1 
//   else 
//     result = expr2 
//   end if  
// to expressions of the form: 
//   result = min(expr1, expr2) 
// or
//   result = max(expr1, expr2) 
// where .relop. is one of .LT., .GT., .LE., and .GE.  
// Attempt sinking converted expressions into loop bounds where 
//   appropriate. 
//-----------------------------------------------------------------------

extern void If_MinMax(WN* func_nd)
{
  if (!LNO_IfMinMax)
    return; 
  if (LNO_Verbose) { 
    fprintf(stdout, "Attempting to convert IFs to MAXs and MINs\n"); 
    fprintf(TFile, "Attempting to convert IFs to MAXs and MINs\n"); 
  } 
  du = Du_Mgr;
  If_MinMax_Traverse(func_nd); 
  if (LNO_Verbose) { 
    fprintf(stdout, "Finished converting IFs to MAXs and MINs\n"); 
    fprintf(TFile, "Finished converting IFs to MAXs and MINs\n"); 
  } 
}
