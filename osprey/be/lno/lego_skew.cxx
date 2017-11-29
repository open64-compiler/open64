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


#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#ifdef _KEEP_RCS_ID
/*REFERENCED*/
static char *rcs_id = "$Source$ $Revision$";
#endif /* _KEEP_RCS_ID */

#include <sys/types.h>
#include <ctype.h>
#include <limits.h>
#include <alloca.h>

#include "pu_info.h"
#include "lnoutils.h"
#include "lnopt_main.h"
#include "stab.h"
#include "targ_const.h"
#include "wn_simp.h"
#include "stdlib.h"
#include "lwn_util.h"
#include "strtab.h"
#include "config.h"
#include "optimizer.h"
#include "opt_du.h"
#include "name.h"
#include "wintrinsic.h"
#include "lno_bv.h"
#include "dep_graph.h"
#include "debug.h"
#include "cxx_memory.h"
#include "lego_pragma.h"
#include "lego_util.h"
#include "lego_skew.h"
#include "tlog.h"

//-----------------------------------------------------------------------
// NAME: Lego_Reshaped_Array 
// FUNCTION: Returns TRUE if the OPR_ARRAY node 'wn_array' is a lego 
//   reshaped array, FALSE otherwise.  
//-----------------------------------------------------------------------

static BOOL Lego_Reshaped_Array(WN* wn_array)
{
  if (WN_operator(wn_array) != OPR_ARRAY)
    return FALSE;
  ST* st_array;
  WN *array_base = WN_array_base(wn_array);

  st_array = (WN_has_sym(array_base) ? WN_st(array_base) : NULL);
  DISTR_ARRAY* dact = Lookup_DACT(st_array);
  if (dact == NULL || !dact->Dinfo()->IsReshaped())
    return FALSE;
  return TRUE; 
}

//-----------------------------------------------------------------------
// NAME: Lego_Skew_Canonical
// FUNCTION: Returns TRUE if the access vector is canonical with respect
//   to the loop 'wn_loop'.  Right now, this means that it has the form 
//   'c1*i+x-c2' where "i" is the index variable of 'wn_loop', "x" is some  
//  linear combination of loop invariant variables, and "c1" and "c2" are 
//  literal constants. Also, "c1" must be either 1 or -1.  
//-----------------------------------------------------------------------

static BOOL Lego_Skew_Canonical(WN* wn_loop, 
			        ACCESS_VECTOR* av)
{
  if (av->Too_Messy)
    return FALSE; 
  if (av->Contains_Non_Lin_Symb())
    return FALSE;
  if (!av->Has_Loop_Coeff())
    return FALSE; 
  INT loop_depth = Do_Depth(wn_loop);  
  INT loop_coeff = av->Loop_Coeff(loop_depth);
  if (loop_coeff != 1 && loop_coeff != -1)
    return FALSE; 
  for (INT i = 0; i < av->Nest_Depth(); i++)
    if (i != loop_depth && av->Loop_Coeff(i) != 0)
      return FALSE;
  if (!av->Contains_Lin_Symb())
    return FALSE;
  return TRUE;    
}

//-----------------------------------------------------------------------
// NAME: Lego_Skew_Equivalent 
// FUNCTION: Returns TRUE if the canonical access vectors 'av1' and 'av2'
//   are equivalent.  Right now, this means that their linear portions are
//   equivalent and the coefficients corresponding to 'wn_loop' are equal. 
//-----------------------------------------------------------------------

static BOOL Lego_Skew_Equivalent(WN* wn_loop, 
				 ACCESS_VECTOR* av1, 
				 ACCESS_VECTOR* av2)
{
  if (av1->Loop_Coeff(Do_Depth(wn_loop)) != av2->Loop_Coeff(Do_Depth(wn_loop)))
    return FALSE;
  INTSYMB_LIST* isl = NULL; 
  isl = Subtract(av1->Lin_Symb, av2->Lin_Symb, &LNO_local_pool);
  if (isl != NULL) 
    return FALSE;
  return TRUE; 
}

//-----------------------------------------------------------------------
// NAME: Lego_Update_Skew_Count 
// FUNCTION: For the given array 'wn_array' with respect to the loop 
//   'wn_loop', enter all canonical skew accesses into separate buckets 
//   of 'st_skew'. 
//-----------------------------------------------------------------------

static void Lego_Update_Skew_Count(WN* wn_loop, 
				   WN* wn_array, 
				   STACK<LEGO_SKEW*>* st_skew)
{
  DISTR_ARRAY* dact = Lookup_DACT(WN_has_sym(WN_array_base(wn_array)) ?
                                  WN_st(WN_array_base(wn_array)) :
                                  NULL);
  ACCESS_ARRAY *aa = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map, wn_array);
  if (aa == NULL || aa->Too_Messy) 
    return; 
  for (INT i = 0; i < aa->Num_Vec(); i++) { 
    ACCESS_VECTOR* av = aa->Dim(i);
    if (Lego_Skew_Canonical(wn_loop, av)) {
      INT j;
      for (j = 0; j < st_skew->Elements(); j++) {
	LEGO_SKEW* lsk = st_skew->Bottom_nth(j);
	ST* st_old = (WN_has_sym(WN_array_base(lsk->Array())) ?
                  WN_st(WN_array_base(lsk->Array())) :
                  NULL);
	DISTR_ARRAY* dact_old = Lookup_DACT(st_old); 
	if (dact->DACT_Equiv(dact_old, i, lsk->Dim()) 
	  && Lego_Skew_Equivalent(wn_loop, av, lsk->Av())) {
	  lsk->Increment();
	  break;
	} 
      }
      if (j == st_skew->Elements()) {
	LEGO_SKEW* lsk_new = CXX_NEW(LEGO_SKEW(av, wn_array, i, 1), 
	  &LNO_default_pool);
	st_skew->Push(lsk_new);  
      }
    }
  }
}

//-----------------------------------------------------------------------
// NAME: Lego_Skew_Offset 
// FUNCTION: Returns a WN* to an expression tree corresponding to the 
//   LEGO_SKEW 'lsk'. 
//-----------------------------------------------------------------------

static WN* Lego_Skew_Offset(LEGO_SKEW* lsk,
			    BOOL negative_stride, 
		            DU_MANAGER* du)
{
  WN* wn_skew = NULL; 
  INTSYMB_CONST_ITER iter(lsk->Av()->Lin_Symb);
  const INTSYMB_NODE *first = iter.First();
  for (const INTSYMB_NODE *node=first; !iter.Is_Empty(); node = iter.Next()) {
    WN* wn_variable = Find_Node(node->Symbol, lsk->Array());
    WN* wn_constant = LWN_Make_Icon(WN_rtype(wn_variable), node->Coeff);
    WN* wn_varcopy = LWN_Copy_Tree(wn_variable);
    LWN_Copy_Def_Use(wn_variable, wn_varcopy, du); 
    OPCODE op_mpy = OPCODE_make_op(OPR_MPY, WN_rtype(wn_variable), MTYPE_V);
    WN* wn_prod = LWN_CreateExp2(op_mpy, wn_constant, wn_varcopy);
    if (wn_skew == NULL) {
      wn_skew = wn_prod; 
    } else {
      TYPE_ID type_add = Max_Wtype(WN_rtype(wn_skew), WN_rtype(wn_prod));
      OPCODE op_add = OPCODE_make_op(OPR_ADD, type_add, MTYPE_V);
      wn_skew = LWN_CreateExp2(op_add, wn_skew, wn_prod);  
    }
  }
  if (negative_stride) {
    WN* wn_minusone = LWN_Make_Icon(WN_rtype(wn_skew), -1);
    OPCODE op_mpy = OPCODE_make_op(OPR_MPY, WN_rtype(wn_skew), MTYPE_V); 
    wn_skew = LWN_CreateExp2(op_mpy, wn_minusone, wn_skew);  
  }
  return wn_skew; 
}

//-----------------------------------------------------------------------
// NAME: Lego_Loop_Want_Skew
// FUNCTION: Returns TRUE if we would like to skew the indices of the
//   loop 'wn_loop'. 
// NOTE: On exit, the value of the skew factor is copied back into 
//   'wn_lego_skew_addr'. 
//-----------------------------------------------------------------------

static BOOL Lego_Loop_Want_Skew(WN* wn_loop, 
				WN** wn_lego_skew_addr, 
			        DU_MANAGER* du)
{
  *wn_lego_skew_addr = NULL; 
  WN* wn_step = Loop_Step(wn_loop);
  if (WN_operator(wn_step) != OPR_INTCONST)
    return FALSE;
  if (WN_const_val(wn_step) != 1)
    return FALSE;
  if (!Upper_Bound_Standardize(WN_end(wn_loop), TRUE))
    return FALSE;
  STACK<LEGO_SKEW*> st_skew(&LNO_local_pool);
  LWN_ITER* itr = LWN_WALK_TreeIter(WN_do_body(wn_loop));
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn;
    if (Lego_Reshaped_Array(wn))
      Lego_Update_Skew_Count(wn_loop, wn, &st_skew); 
  }
  if (st_skew.Elements() == 0) 
    return FALSE; 
  LEGO_SKEW* lsk_best = st_skew.Bottom_nth(0);  
  for (INT i = 1; i < st_skew.Elements(); i++) {
    LEGO_SKEW* lsk = st_skew.Bottom_nth(i); 
    if (lsk->Count() > lsk_best->Count())
      lsk_best = lsk; 
  }
  BOOL negative_stride = lsk_best->Av()->Loop_Coeff(Do_Depth(wn_loop)) < 0;
  *wn_lego_skew_addr = Lego_Skew_Offset(lsk_best, negative_stride, du); 
  return TRUE; 
}   

//-----------------------------------------------------------------------
// NAME: Lego_Parent_Array 
// FUNCTION: Returns the closest enclosing OPR_ARRAY node which is a 
//   parent of 'wn_node'.  Returns NULL if there is none.  
//-----------------------------------------------------------------------

static WN* Lego_Parent_Array(WN* wn_node) 
{
  for (WN* wn = wn_node; wn != NULL; wn = LWN_Get_Parent(wn))
    if (WN_operator(wn) == OPR_ARRAY)
      return wn;
  return NULL; 
}

//-----------------------------------------------------------------------
// NAME: Lego_Skew_Index
// FUNCTION: Returns an expression 'index - offset' where "index"
//   is the index variable of 'wn_loop' and "offset" is the expression 
//   in 'wn_lego_skew_offset'.
//-----------------------------------------------------------------------

static WN* Lego_Skew_Index(WN* wn_loop,
                           WN* wn_lego_skew_offset,
                           DU_MANAGER* du)
{
  TYPE_ID type = Promote_Type(Do_Wtype(wn_loop));
  WN* wn_index = UBvar(WN_end(wn_loop));
  WN* wn_index_copy = LWN_Copy_Tree(wn_index);
  LWN_Copy_Def_Use(wn_index, wn_index_copy, du);
  WN* wn_ldid = LWN_Copy_Tree(wn_lego_skew_offset);
  LWN_Copy_Def_Use(wn_lego_skew_offset, wn_ldid, du);
  OPCODE op = OPCODE_make_op(OPR_SUB, type, MTYPE_V);
  WN* wn_skew = LWN_CreateExp2(op, wn_index_copy, wn_ldid);
  return wn_skew;
}

//-----------------------------------------------------------------------
// NAME: Lego_Contains_Non_Index_Ldid 
// FUNCTION: Returns TRUE if the expression 'wn_index' contains an LDID
//   of a symbol other than the index variable of 'wn_loop', FALSE 
//   otherwise. 
//-----------------------------------------------------------------------

static BOOL Lego_Contains_Non_Index_Ldid(WN* wn_loop, 
				         WN* wn_index)
{
  LWN_ITER* itr = LWN_WALK_TreeIter(wn_index);
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn;
    if (WN_operator(wn) == OPR_LDID 
        && SYMBOL(wn) != SYMBOL(WN_index(wn_loop)))
      return TRUE; 
  }
  return FALSE; 
} 

//-----------------------------------------------------------------------
// NAME: Lego_Index_From_Access_Vector 
// FUNCTION: Returns a WN* to an expression equivalent to the access 
//   vector 'av'.  The expressions value should be equivalent to that 
//   in 'wn_index'. 
//-----------------------------------------------------------------------

static WN* Lego_Index_From_Access_Vector(ACCESS_VECTOR* av, 
				         WN* wn_index, 
					 DU_MANAGER* du) 
{
  WN* wn_new_index = NULL; 
  for (INT i = 0; i < av->Nest_Depth(); i++) {
    if (av->Loop_Coeff(i) == 0) 
      continue; 
    WN* wn = 0;
    for (wn = wn_index; wn != NULL; wn = LWN_Get_Parent(wn))
      if (WN_opcode(wn) == OPC_DO_LOOP && Do_Depth(wn) == i)
	break; 
    FmtAssert(wn != NULL, ("Could not find do loop with given depth")); 
    OPCODE op_ldid = Matching_Load_Opcode(WN_opcode(WN_start(wn)));  
    WN* wn_ldid = LWN_CreateLdid(op_ldid, WN_start(wn));
    du->Add_Def_Use(WN_start(wn), wn_ldid); 
    du->Add_Def_Use(WN_step(wn), wn_ldid); 
    du->Ud_Get_Def(wn_ldid)->Set_loop_stmt(wn); 
    WN* wn_constant = LWN_Make_Icon(WN_rtype(wn_ldid), av->Loop_Coeff(i)); 
    OPCODE op_mpy = OPCODE_make_op(OPR_MPY, WN_rtype(wn_ldid), MTYPE_V);
    WN* wn_prod = LWN_CreateExp2(op_mpy, wn_constant, wn_ldid); 
    if (wn_new_index == NULL) {
      wn_new_index = wn_prod; 
    } else {
      TYPE_ID type_add = Max_Wtype(WN_rtype(wn_ldid), WN_rtype(wn_new_index));
      OPCODE op_add = OPCODE_make_op(OPR_ADD, type_add, MTYPE_V);
      wn_new_index = LWN_CreateExp2(op_add, wn_new_index, wn_prod);
    }
  }
  if (av->Const_Offset != 0) {
    if (wn_new_index == 0) {
      wn_new_index = LWN_Make_Icon(av->Const_Offset, WN_rtype(wn_index));
    } else {
      WN* wn_offset = LWN_Make_Icon(WN_rtype(wn_new_index), av->Const_Offset);
      OPCODE op_add = OPCODE_make_op(OPR_ADD, WN_rtype(wn_new_index), MTYPE_V);
      wn_new_index = LWN_CreateExp2(op_add, wn_new_index, wn_offset);
    }
  }
  return wn_new_index; 
}

//-----------------------------------------------------------------------
// NAME: Lego_Simplify 
// FUNCTION: Simplify the subscripts of the array expression 'wn_loop' 
//   which contain the index variable of loop 'wn_loop' if their access
//   vectors contain only index variables and constants and if the 
//   subscripts also contain other symbols. 
//-----------------------------------------------------------------------

static void Lego_Simplify(WN* wn_loop, 
			  WN* wn_array, 
			  DU_MANAGER* du)
{
  ACCESS_ARRAY *aa = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map, wn_array);
  if (aa == NULL || aa->Too_Messy)
    return;
  INT loop_depth = Do_Depth(wn_loop);  
  INT i;
  for (i = 0; i < aa->Num_Vec(); i++) 
    if (aa->Dim(i)->Delinearized_Symbol != NULL) 
      return; 
  for (i = 0; i < aa->Num_Vec(); i++) {
    ACCESS_VECTOR* av = aa->Dim(i);
    if (av->Too_Messy)
      continue; 
    INT loop_coeff = av->Loop_Coeff(loop_depth);
    if (av->Loop_Coeff(loop_depth)== 0 || av->Contains_Non_Lin_Symb())
      continue;
    if (av->Contains_Lin_Symb())
      continue; 
    WN* wn_index = WN_array_index(wn_array, i); 
    if (!Lego_Contains_Non_Index_Ldid(wn_loop, wn_index))
      continue; 
    WN* wn_new_index = Lego_Index_From_Access_Vector(av, wn_index, du);
    Replace_Wnexp_With_Exp_Copy(wn_index, wn_new_index, du); 
    LWN_Delete_Tree(wn_new_index);
  }  
}

//-----------------------------------------------------------------------
// NAME: Lego_Simplify_Loop
// FUNCTION: Simplify the subscripts of array elements in 'wn_loop' using
//   access vectors. 
//-----------------------------------------------------------------------

static void Lego_Simplify_Loop(WN* wn_loop,
			       DU_MANAGER* du)
{
  LWN_ITER* itr = LWN_WALK_TreeIter(WN_do_body(wn_loop));
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn;
    if (WN_operator(wn) == OPR_ARRAY)
      Lego_Simplify(wn_loop, wn, du);
  } 
}

//-----------------------------------------------------------------------
// NAME: Lego_Skew_Loop 
// FUNCTION: Skew the loop 'wn_loop' by subtracting 'wn_lego_skew_index' 
//   from each instance of the index variable of 'wn_loop' and adding the 
//   same value to the loop bounds.  
//-----------------------------------------------------------------------

static void Lego_Skew_Loop(WN* wn_loop,
			   WN* wn_lego_skew_offset,   
			   DU_MANAGER* du)
{
  if (LNO_Verbose) { 
    fprintf(stdout, "Lego skewing loop %s on line %d\n", 
      WB_Whirl_Symbol(wn_loop), Srcpos_To_Line(WN_linenum(wn_loop))); 
    fprintf(TFile, "Lego skewing loop %s on line %d\n", 
      WB_Whirl_Symbol(wn_loop), Srcpos_To_Line(WN_linenum(wn_loop))); 
    if (LNO_Tlog)
      Generate_Tlog("LNO", "lego_skewing", Srcpos_To_Line(WN_linenum(wn_loop)),
        (char *) WB_Whirl_Symbol(wn_loop), "", "", "");
  }
  if (Index_Variable_Live_At_Exit(wn_loop))
    Finalize_Index_Variable(wn_loop, TRUE, TRUE); 
  TYPE_ID type = Promote_Type(Do_Wtype(wn_loop));
  OPCODE op_skew = OPCODE_make_op(OPR_ADD, type, MTYPE_V);
  LWN_ITER* itr = LWN_WALK_TreeIter(WN_do_body(wn_loop));
  STACK<WN*> index_stack(&LNO_local_pool); 
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn;
    if (WN_operator(wn) == OPR_LDID
	&& SYMBOL(wn) == SYMBOL(WN_index(wn_loop)))
      index_stack.Push(wn);
  }
  INT i;
  for (i = 0; i < index_stack.Elements(); i++) {
    WN* wn = index_stack.Bottom_nth(i);
    WN* wn_array = Lego_Parent_Array(wn);
    WN* wn_new_index = Lego_Skew_Index(wn_loop, wn_lego_skew_offset, du); 
    Replace_Wnexp_With_Exp_Copy(wn, wn_new_index, du); 
    if (wn_array != NULL) 
      LWN_Simplify_Tree(wn_array); 
    LWN_Delete_Tree(wn_new_index); 
  } 
  WN* wn_start = WN_kid0(WN_start(wn_loop)); 
  WN* wn_stop = UBexp(WN_end(wn_loop));
  for (i = 0; i < WN_kid_count(WN_end(wn_loop)); i++) 
    if (WN_kid(WN_end(wn_loop), i) == wn_stop)
      break;
  INT stop_index = i; 
  WN* wn_skew = LWN_Copy_Tree(wn_lego_skew_offset); 
  LWN_Copy_Def_Use(wn_lego_skew_offset, wn_skew, du); 
  WN* wn_new_start = LWN_CreateExp2(op_skew, wn_start, wn_skew);
  wn_skew = LWN_Copy_Tree(wn_lego_skew_offset); 
  LWN_Copy_Def_Use(wn_lego_skew_offset, wn_skew, du); 
  WN* wn_new_stop = LWN_CreateExp2(op_skew, wn_stop, wn_skew);
  WN_kid0(WN_start(wn_loop)) = wn_new_start; 
  LWN_Set_Parent(wn_new_start, WN_start(wn_loop));
  WN_kid(WN_end(wn_loop), stop_index) = wn_new_stop; 
  LWN_Set_Parent(wn_new_stop, WN_end(wn_loop));
  LWN_Delete_Tree(wn_lego_skew_offset);  
  DOLOOP_STACK shortstack(&LNO_local_pool);
  Build_Doloop_Stack(LWN_Get_Parent(wn_loop), &shortstack);
  LNO_Build_Access(wn_loop, &shortstack, &LNO_default_pool, 0, TRUE);
  Lego_Simplify_Loop(wn_loop, du); 
}

//-----------------------------------------------------------------------
// NAME: Lego_Skew_Traverse 
// FUNCTION: Traverse 'wn_tree' preforming lego skewing on loops for 
//   which is it appropriate. 
//-----------------------------------------------------------------------

static void Lego_Skew_Traverse(WN* wn_tree, 
			      DU_MANAGER* du) 
{
  if (WN_opcode(wn_tree) == OPC_DO_LOOP) {
    WN* wn_lego_skew_offset = NULL;  
    if (Lego_Loop_Want_Skew(wn_tree, &wn_lego_skew_offset, du))
      Lego_Skew_Loop(wn_tree, wn_lego_skew_offset, du); 
  }
  if (WN_opcode(wn_tree) == OPC_BLOCK) {
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      Lego_Skew_Traverse(wn, du); 
  } else {
    for (INT i = 0; i < WN_kid_count(wn_tree); i++) 
      Lego_Skew_Traverse(WN_kid(wn_tree, i), du);  
  }
} 

//-----------------------------------------------------------------------
// NAME: Lego_Skew_Indices 
// FUNCTION: Apply lego skewing to all appropriate loops in the tree 
//   'wn_tree'. 
//-----------------------------------------------------------------------

extern void Lego_Skew_Indices(WN* wn_tree)
{
  DU_MANAGER* du = Du_Mgr; 
  Lego_Skew_Traverse(wn_tree, du); 
}

