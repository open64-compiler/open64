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


//-*-c++-*-
//                     Access Main
//                     --------------
//
// Description:
//
//     
//    High level routines to build access vectors
//      
//
//

/* ====================================================================
 * ====================================================================
 *
 * Module: access_main.cxx
 * $Revision: 1.7 $
 * $Date: 04/12/21 14:57:11-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/be/lno/SCCS/s.access_main.cxx $
 *
 * Revision history:
 *  11-6-96 - Original Version
 *
 * Description: High level routines used to build access vectors
 *              
 *
 * ====================================================================
 * ====================================================================
 */

#include <stdint.h>
#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include <sys/types.h>
#include <alloca.h>

#include "call_info.h"
#include "defs.h"
#include "config_cache.h"
#include "access_main.h"
#include "access_vector.h"
#include "lnopt_main.h"
#include "stab.h"
#include "lwn_util.h"
#include "opt_du.h"
#include "lnoutils.h"
#include "soe.h"
#include "cond.h"
#include "fb_whirl.h"
#include "move.h"

static INT preg_counter = 0; 
extern BOOL Promote_Messy_Bound(WN* wn_loop, 
			        WN* wn_bound,
				char name[],
				DU_MANAGER* du);
#define MAX_NAME_SIZE 66
extern BOOL Hoist_Lower_Bound(WN* wn_loop,
			      DOLOOP_STACK* stack,
			      MEM_POOL* pool) 
{
  char buffer[MAX_NAME_SIZE]; 
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 
  WN* wn_load = UBvar(WN_end(wn_loop)); 
  WN* wn_start = WN_kid0(WN_start(wn_loop));
  if (wn_load == NULL) 
    return FALSE;  
  if (!dli->Step->Is_Const())
    return FALSE;
  sprintf(buffer, "_lb%d", preg_counter++);
  BOOL promoted = Promote_Messy_Bound(wn_loop, wn_start, buffer, Du_Mgr);
  FmtAssert(promoted, ("Could not promote lower bound.")); 
  CXX_DELETE(dli->LB, dli->LB->Pool());
  INT num_bounds = Num_Lower_Bounds(wn_loop, dli->Step); 
  dli->LB =
    CXX_NEW(ACCESS_ARRAY(num_bounds,stack->Elements(),pool),pool);
  dli->LB->Set_LB(WN_kid0(WN_start(wn_loop)), stack, dli->Step->Const_Offset);
  return TRUE; 
}

extern BOOL Hoist_Upper_Bound(WN* wn_loop,
			      DOLOOP_STACK* stack, 
			      MEM_POOL* pool) 
{
  char buffer[MAX_NAME_SIZE]; 
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 
  WN* wn_load = UBvar(WN_end(wn_loop)); 
  WN* wn_end = UBexp(WN_end(wn_loop));
  if (wn_load == NULL || wn_end == NULL)
    return FALSE; 
  if (!dli->Step->Is_Const())
    return FALSE;
  sprintf(buffer, "_ub%d", preg_counter++);
  BOOL promoted = Promote_Messy_Bound(wn_loop, wn_end, buffer, Du_Mgr);
  FmtAssert(promoted, ("Could not promote upper bound.")); 
  CXX_DELETE(dli->UB, dli->UB->Pool());
  INT num_bounds = Num_Upper_Bounds(wn_loop);
  dli->UB = CXX_NEW(ACCESS_ARRAY(num_bounds,stack->Elements(),pool),pool);
  dli->UB->Set_UB(WN_end(wn_loop), stack);
  return TRUE; 
}

static BOOL Expr_Has_Vertex(WN* wn_tree)
{ 
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  if (dg->Get_Vertex(wn_tree))
    return TRUE; 
  for (INT i = 0; i < WN_kid_count(wn_tree); i++) 
    if (Expr_Has_Vertex(WN_kid(wn_tree, i)))
      return TRUE;
  return FALSE; 
} 

extern void Hoist_Bounds_One_Level(WN* wn_tree)
{
  DOLOOP_STACK shortstack(&LNO_local_pool); 
  if (WN_operator(wn_tree) == OPR_DO_LOOP) {
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_tree); 
    if (Bound_Is_Too_Messy(dli->LB) 
        || Expr_Has_Vertex(WN_kid0(WN_start(wn_tree)))) {
      Build_Doloop_Stack(wn_tree, &shortstack);
      Hoist_Lower_Bound(wn_tree, &shortstack, &LNO_default_pool);
      shortstack.Clear();
    }
    WN* wn_var = UBvar(WN_end(wn_tree)); 
    if (wn_var != NULL && (Bound_Is_Too_Messy(dli->UB)
	|| Expr_Has_Vertex(UBexp(WN_end(wn_tree))))
        && WN_operator(wn_var) == OPR_LDID
        && SYMBOL(wn_var) == SYMBOL(WN_index(wn_tree))) {
      Build_Doloop_Stack(wn_tree, &shortstack);
      Hoist_Upper_Bound(wn_tree, &shortstack, &LNO_default_pool); 
      shortstack.Clear();
    }
    Hoist_Bounds_One_Level(WN_do_body(wn_tree));
    return; 
  }

  if (WN_opcode(wn_tree) == OPC_BLOCK) {
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      Hoist_Bounds_One_Level(wn); 
  } else {
    for (INT i = 0; i < WN_kid_count(wn_tree); i++)
      Hoist_Bounds_One_Level(WN_kid(wn_tree, i));
  }
}


//-----------------------------------------------------------------------
// NAME: Hoist_Iload_Ldid_Upper_Bound_One_Level
// FUNCTION: Hoist the ILOAD-LDID upper bound (from lego-tiling)
//		one level out
//-----------------------------------------------------------------------

extern void Hoist_Iload_Ldid_Upper_Bound_One_Level(
	WN* loop,
	BOOL negative_stride) {
  char name[MAX_NAME_SIZE];
  DOLOOP_STACK stack(&LNO_local_pool);
  if (WN_operator(loop) == OPR_DO_LOOP) {
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(loop);
    if (!dli->Step->Is_Const())
      return;
    WN* wn_var = UBvar(WN_end(loop));
    WN* wn_exp=NULL;
    WN* wn_bound = negative_stride ? WN_kid0(WN_start(loop))
        : UBexp(WN_end(loop));
    LWN_ITER* itr = LWN_WALK_TreeIter(wn_bound);
    for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
      wn_exp = itr->wn;
      OPERATOR opr = WN_operator(wn_exp);
      if (opr == OPR_MPY || opr == OPR_LDID || opr == OPR_ILOAD)
	break;
    }
    if (wn_var == NULL || wn_exp == NULL)
      return;
    if (WN_operator(wn_exp)==OPR_ILOAD
	&& WN_operator(WN_kid0(wn_exp))==OPR_LDID
        && WN_operator(wn_var) == OPR_LDID
        && SYMBOL(wn_var) == SYMBOL(WN_index(loop))) {

      WN* dart_ldid=WN_kid0(wn_exp);
      WN* dart_stid=Du_Mgr->Ud_Get_Def(dart_ldid)->Head()->Wn();
  
      Build_Doloop_Stack(loop, &stack);
  
      WN* wn_parent = LWN_Get_Parent(wn_exp);
      INT i;
      for (i = 0; i < WN_kid_count(wn_parent); i++)
        if (wn_exp == WN_kid(wn_parent, i))
          break;
      FmtAssert(i<WN_kid_count(wn_parent), ("Could not find kid for parent."));
      INT kid = i;
      TYPE_ID type = WN_desc(WN_start(loop));
      OPCODE preg_s_opcode = OPCODE_make_op(OPR_STID, MTYPE_V, type);
      sprintf(name, "_ub%d", preg_counter++);
      WN_OFFSET preg_num = Create_Preg(type, name);
      ST* preg_st = MTYPE_To_PREG(type);
      WN* wn_stid = LWN_CreateStid(preg_s_opcode, preg_num, preg_st,
                                   Be_Type_Tbl(type), wn_exp);
      LWN_Insert_Block_After(LWN_Get_Parent(dart_stid), dart_stid, wn_stid);
      WN* wn_ldid = LWN_CreateLdid(WN_opcode(UBvar(WN_end(loop))), wn_stid);
      WN_kid(wn_parent, kid) = wn_ldid;
      LWN_Set_Parent(wn_ldid, wn_parent);
      Du_Mgr->Add_Def_Use(wn_stid, wn_ldid);
      INT hoist_level = Hoistable_Statement(wn_stid, Du_Mgr);
      if (hoist_level < Loop_Depth(wn_stid))
        Hoist_Statement(wn_stid, hoist_level);
      CXX_DELETE(dli->UB, dli->UB->Pool());
      INT num_bounds = Num_Upper_Bounds(loop);
      dli->UB = CXX_NEW(ACCESS_ARRAY(num_bounds,stack.Elements(),
	&LNO_default_pool),&LNO_default_pool);
      dli->UB->Set_UB(WN_end(loop), &stack);
      stack.Clear();
    }
  }
}

// Walk the program, build all the access vectors and attach them
// to the code using the mapping mechanism
extern void LNO_Build_Access(WN *func_nd, 
			     MEM_POOL *pool, 
			     BOOL Hoist_Bounds)
{
  MEM_POOL_Push(&LNO_local_pool);
  DOLOOP_STACK *stack = CXX_NEW(DOLOOP_STACK(&LNO_local_pool),&LNO_local_pool); 
  INDX_RANGE_STACK *irs = 
		CXX_NEW(INDX_RANGE_STACK(&LNO_local_pool),&LNO_local_pool); 
  LNO_Build_Access(func_nd,stack,pool,irs,Hoist_Bounds);
  CXX_DELETE(stack,&LNO_local_pool);
  CXX_DELETE(irs,&LNO_local_pool);
  MEM_POOL_Pop(&LNO_local_pool);
}

// Walk the program, starting at wn, given that stack describes all the
// outer do loops

extern void LNO_Build_Access(WN *wn, DOLOOP_STACK *stack, MEM_POOL *pool,
	INDX_RANGE_STACK *irs, BOOL Hoist_Bounds)
{
  WN *kid;

  Is_True(wn,("Null wn in LNO_Build_Access"));

  if (OPCODE_is_leaf(WN_opcode(wn))) return;

  if (WN_opcode(wn) == OPC_BLOCK) {
    kid = WN_first (wn);
    while (kid) {
      LNO_Build_Access(kid,stack,pool,irs,Hoist_Bounds);
      kid = WN_next(kid);
    }
    return;
  } 

  if (WN_opcode(wn) == OPC_DO_LOOP) {
    LNO_Build_Do_Access(wn,stack,Hoist_Bounds);
    stack->Push(wn);
    if (irs) irs->Push(INDX_RANGE());
  } else if (WN_operator(wn) == OPR_ARRAY) {
    LNO_Build_Access_Array(wn,stack,pool,irs);
  } else if (WN_opcode(wn) == OPC_IF) {
    LNO_Build_If_Access(wn,stack);
  }

  for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
    kid = WN_kid(wn,kidno);
    LNO_Build_Access(kid,stack,pool,irs,Hoist_Bounds);
  }

  if (WN_opcode(wn) == OPC_DO_LOOP) {
    if (irs) {
      INDX_RANGE *ir = &irs->Top_nth(0);
      if (ir->Valid) {
        DO_LOOP_INFO *dli = (DO_LOOP_INFO *) WN_MAP_Get(LNO_Info_Map,wn);
	ACCESS_VECTOR *step = dli->Step;
        INT64 new_max = -1;
	if (step->Is_Const() && (abs(step->Const_Offset) != 1)) {
          new_max = ir->Maxsize()/abs(step->Const_Offset);
        } else {
          new_max = ir->Maxsize();
        }
        if (new_max != -1 &&
            (dli->Est_Max_Iterations_Index == -1 ||
             dli->Est_Max_Iterations_Index > new_max)) {
          dli->Est_Max_Iterations_Index = new_max;
        }
	if (dli->Est_Max_Iterations_Index >= 0 &&
	    dli->Est_Max_Iterations_Index < dli->Est_Num_Iterations) {
	  dli->Est_Num_Iterations = dli->Est_Max_Iterations_Index;
	  dli->Num_Iterations_Symbolic = FALSE;
        }
      }
      irs->Pop();
    }
    stack->Pop();
  }
}

static void LNO_Update_Indx_Range(INDX_RANGE_STACK *irs,
					ACCESS_ARRAY *array, WN *wn)
{
  if (!array->Too_Messy && (WN_element_size(wn) > 0)) {
    INT num_dim = WN_num_dim(wn);
    for (INT i = 0; i < num_dim; i++) {
      WN*   dim_wn = WN_array_dim(wn,i);
      if (WN_operator(dim_wn) != OPR_INTCONST)
        continue;
      INT64 sz = WN_const_val(dim_wn);
      if (((sz == 1) || (sz == 0)) && i == 0)
        continue;       // FORTRAN evil, size is meaningless
      ACCESS_VECTOR *av = array->Dim(i);
      if (av->Too_Messy) continue;
      INT coeff = -1;
      BOOL seen_two_coeff=FALSE;
      for (INT j=0; j<av->Nest_Depth() && !seen_two_coeff; j++) {
        if (av->Loop_Coeff(j)) {
	  if (coeff != -1) {
	    seen_two_coeff = TRUE;
          } else {
	    coeff = j;
          }
	}
      }
      // use this access vector to update index range number coeff
      if ((coeff != -1) && !seen_two_coeff) {
	if (av->Contains_Non_Lin_Symb() || av->Contains_Lin_Symb()) {
	  if (av->Non_Const_Loops() <= coeff) {
	    irs->Bottom_nth(coeff).Union(0,0,av->Loop_Coeff(coeff),sz);
	  }
	} else {
	    irs->Bottom_nth(coeff).Union(av->Const_Offset,TRUE,
						av->Loop_Coeff(coeff),sz);
	}
      }
    }
  }
}

extern void LNO_Build_Access_Array(WN *wn, DOLOOP_STACK *stack, MEM_POOL *pool,
				INDX_RANGE_STACK *irs)
{
  ACCESS_ARRAY *array = CXX_NEW(ACCESS_ARRAY(WN_num_dim(wn),stack->Elements(),
				pool),pool);
  array->Set_Array(wn,stack);
  WN_MAP_Set(LNO_Info_Map,wn,(void *)array);

  if (irs) {
    LNO_Update_Indx_Range(irs,array,wn);
  }
}


// Build the access arrays for the bounds of the do loop
extern void LNO_Build_Do_Access(WN *wn, 
				DOLOOP_STACK *stack, 
				BOOL Hoist_Bounds)
{

  DO_LOOP_INFO *dli = (DO_LOOP_INFO *) WN_MAP_Get(LNO_Info_Map,wn);
  FmtAssert(dli,("Unmapped DO loop in LNO_Build_Do_Access"));

  MEM_POOL *pool=dli->Pool();
  {

    // first the step
    ACCESS_VECTOR *step=CXX_NEW(ACCESS_VECTOR(stack->Elements(),pool),pool);

    stack->Push(wn);

    dli->Step = step;
    dli->LB = NULL;
    dli->UB = NULL;
    WN *s = WN_step(wn);
    if (WN_operator(s) != OPR_STID) {
      step->Too_Messy = TRUE;
      goto return_point;
    }
    s = WN_kid(s,0);
    INT8 sign;
    if (WN_operator(s) == OPR_ADD) {
      sign = 1;
    } else if (WN_operator(s) == OPR_SUB) {
      sign = -1;
    } else {
      step->Too_Messy = TRUE;
      goto return_point;
    }
  
    // one of s's children is the loop variable, the other is the step
    // we don't know which
    BOOL found_step = FALSE;
    SYMBOL doloop(WN_index(wn));
    if (WN_operator(WN_kid(s,0)) == OPR_LDID) {
      WN*kid = WN_kid(s,0);
      SYMBOL symbol(kid);
      Is_True(symbol.St(),("Null symbol in LNO_Build_Do_Access"));
      if (symbol == doloop) {
  	  step->Set(WN_kid(s,1),stack,sign,0);
	  found_step = TRUE;
      }
    }
  
    if (!found_step && WN_operator(WN_kid(s,1)) == OPR_LDID) {
      SYMBOL symbol(WN_kid(s,1));
      if (symbol == doloop) {
  	  step->Set(WN_kid(s,0),stack,sign,0);
	  found_step = TRUE;
      }
    }
    if (!found_step) {
      step->Too_Messy = TRUE;
      goto return_point;
    }
    if (!step->Is_Const()) {  // can't get a bound if don't know sign of step
      goto return_point;
    }

    // Next the lower bound
    // First, how many bounds
  
    WN *l = WN_start(wn);
    Is_True(WN_operator(l) == OPR_STID,
      ("LNO_Build_Do_Access::LB of do loop is not a stid"));
  
    INT num_bounds = Num_Lower_Bounds(wn, step); 
  
    // Why doesn't this work here? (RJC) 
    // Upper_Bound_Standardize(WN_end(wn), TRUE);
  
    dli->LB =
      CXX_NEW(ACCESS_ARRAY(num_bounds,stack->Elements(),pool),pool);
    // now actually do the building
    dli->LB->Set_LB(WN_kid(l,0),stack,step->Const_Offset);
    if (Hoist_Bounds && Bound_Is_Too_Messy(dli->LB))
      Hoist_Lower_Bound(wn, stack, pool);      
  
    // Now the upper bound
    // First, how many bounds
      
    WN *u = WN_end(wn);
    Is_True(OPCODE_is_compare(WN_opcode(u)),
      ("LNO_Build_Do_Access::UB of do loop is not a compare"));
    num_bounds = Num_Upper_Bounds(wn);
  
    // now actually do the building
    dli->UB= CXX_NEW(ACCESS_ARRAY(num_bounds,stack->Elements(),pool),pool);
    dli->UB->Set_UB(u,stack);
    WN* wn_var = UBvar(WN_end(wn));
    if (Hoist_Bounds && wn_var != NULL && Bound_Is_Too_Messy(dli->UB)
      && WN_operator(wn_var) == OPR_LDID
      && SYMBOL(wn_var) == SYMBOL(WN_index(wn))) 
      Hoist_Upper_Bound(wn, stack, pool); 
  
    // Swap the lower bound and upper bound if the step is negative
    if (step->Is_Const() && (step->Const_Offset < 0)) {
      ACCESS_ARRAY *ub = dli->UB;
      dli->UB = dli->LB;
      dli->LB = ub;
    }
  
    // check for the weird degenerate case for (i =1; i<N; i--)
    // these should never happen, but they can bother SNL
    if (step->Is_Const()) {
      if ((step->Const_Offset > 0)) {
        for (INT i=0; i<dli->UB->Num_Vec(); i++) {
	  ACCESS_VECTOR *ub = dli->UB->Dim(i);
	  if (ub->Loop_Coeff(dli->Depth) < 0) {
	    dli->LB->Too_Messy = TRUE;
	    dli->UB->Too_Messy = TRUE;
	    step->Too_Messy = TRUE;
	    break;
	  }
        }
      } else {
        for (INT i=0; i<dli->LB->Num_Vec(); i++) {
	  ACCESS_VECTOR *lb = dli->LB->Dim(i);
	  if (lb->Loop_Coeff(dli->Depth) > 0) {
	    dli->LB->Too_Messy = TRUE;
	    dli->UB->Too_Messy = TRUE;
	    step->Too_Messy = TRUE;
	    break;
	  }
        }
      }
    }
  }
return_point:

  stack->Pop();

  if (!dli->LB) {
    ACCESS_ARRAY *lb=
    	CXX_NEW(ACCESS_ARRAY(1,stack->Elements()+1,pool),pool);
    dli->LB = lb;
    lb->Too_Messy = TRUE;
  }
  if (!dli->UB) {
    ACCESS_ARRAY *ub=
    	CXX_NEW(ACCESS_ARRAY(1,stack->Elements()+1,pool),pool);
    dli->UB = ub;
    ub->Too_Messy = TRUE;
  }

  if (dli->Est_Num_Iterations == -1) {
    dli->Set_Est_Num_Iterations(stack);
    if (Cur_PU_Feedback && dli->Num_Iterations_Symbolic) {
#ifndef KEY
      INT32 freq_loop_header = WN_MAP32_Get(WN_MAP_FEEDBACK,WN_start(wn));
      INT32 freq_loop_body = WN_MAP32_Get(WN_MAP_FEEDBACK,WN_step(wn));
      if (LNO_Verbose) {
	fprintf(stdout, "Header executed  %d\n", freq_loop_header);
	fprintf(stdout, "Body executed  %d\n", freq_loop_body);
      }
      
      if (freq_loop_header > 0) {
	dli->Est_Num_Iterations = (INT64) (freq_loop_body/freq_loop_header);
	dli->Num_Iterations_Symbolic = FALSE;
	dli->Num_Iterations_Profile=TRUE;
	if (LNO_Verbose)
	  fprintf(stdout, "Iteration counts from profile %lld\n", dli->Est_Num_Iterations);
      }
#else
      const FB_Info_Loop fb_info = Cur_PU_Feedback->Query_loop(wn);

      if (!LNO_Ignore_Feedback && fb_info.freq_iterate._value > 0) {
	// This is still an estimate because we are averaging over as many 
	// times the loop was executed.
	dli->Est_Num_Iterations = (INT64) ( ( fb_info.freq_back._value +
					      fb_info.freq_out._value ) / 
					    fb_info.freq_positive._value );
	dli->Num_Iterations_Symbolic = FALSE;
	dli->Num_Iterations_Profile=TRUE;
	if (LNO_Verbose)
	  fprintf(stdout, "Iteration counts from profile %lld\n", 
		  dli->Est_Num_Iterations);
      }
#endif
    }
  }
}

// Build the access array for an IF statement
extern void LNO_Build_If_Access(WN *wn, DOLOOP_STACK *stack)
{
  Is_True(WN_opcode(wn) == OPC_IF,("Non-if in LNO_Build_If_Access"));
  IF_INFO *info = (IF_INFO *) WN_MAP_Get(LNO_Info_Map,wn);
  Is_True(info,("Unmapped IF loop in LNO_Build_IF_Access"));

  MEM_POOL *pool=info->Pool();

  WN *compare = WN_if_test(wn);
  BOOL top_level_not;

  if (WN_operator(compare) == OPR_LNOT) {
    top_level_not = TRUE;
    compare = WN_kid0(compare);
  } else {
    top_level_not = FALSE;
  }

  if (WN_operator(compare) == OPR_LIOR
      || WN_operator(compare) == OPR_CIOR) {
    INT num_lors = Num_Liors(compare);
    info->Condition = 
      CXX_NEW(ACCESS_ARRAY(num_lors+1,stack->Elements(),pool),pool);
    info->Condition_On_Then = top_level_not;
    info->Condition->Set_IF(compare,stack,TRUE,FALSE,0);
  } else {
    INT num_lands = Num_Lands(compare);
    info->Condition = 
      CXX_NEW(ACCESS_ARRAY(num_lands+1,stack->Elements(),pool),pool);
    info->Condition_On_Then = !top_level_not;
    info->Condition->Set_IF(compare,stack,FALSE,TRUE,0);
  } 

  if (Cur_PU_Feedback && info->Freq_True<0 && info->Freq_False<0) {
    INT32 freq_header = WN_MAP32_Get(WN_MAP_FEEDBACK, wn);
    if (freq_header > 0) {
      INT32 freq_true = WN_MAP32_Get(WN_MAP_FEEDBACK, WN_then(wn));
      info->Freq_True = (float) freq_true / (float) freq_header;

      if (LNO_Verbose) 
	fprintf(stdout, "True branch frequency %f\n", info->Freq_True);

      if (!WN_else_is_empty(wn)) {
	INT32 freq_false = WN_MAP32_Get(WN_MAP_FEEDBACK, WN_else(wn));
	info->Freq_False = (float) freq_false / (float) freq_header;
	if (LNO_Verbose) 
	  fprintf(stdout, "False branch frequency %f\n", info->Freq_False);
      }
    }
  }
}

// Print access information for one whirl node.

extern void LNO_Print_One_Access(FILE *fp, WN* wn) 
{ 
  if (WN_opcode(wn) == OPC_DO_LOOP) {
    DO_LOOP_INFO *dli = (DO_LOOP_INFO *) WN_MAP_Get(LNO_Info_Map,wn);
    fprintf(fp,"The do loop info is \n"); 
    if (dli != NULL) { 
      dli->Print(fp);
    } else { 
      fprintf(fp, "Null DO_LOOP_INFO\n");
    } 
  } else if (WN_opcode(wn) == OPC_REGION) { 
    REGION_INFO* rgi = (REGION_INFO *) WN_MAP_Get(LNO_Info_Map, wn); 
    if (rgi != NULL) {
       fprintf(fp,"The region info is \n"); 
       rgi->Print(fp);
    } else { 
       fprintf(fp, "Null REGION_INFO\n");
    } 
  } else if (WN_opcode(wn) == OPC_IF) {
    IF_INFO *info = (IF_INFO *) WN_MAP_Get(LNO_Info_Map,wn);
    if (info != NULL) { 
      fprintf(fp,"The if info is \n"); info->Print(fp);
      if (WN_Is_If_MpVersion(wn))
	fprintf(fp, "WN_IF_IS_MPVERSION\n"); 
    } else { 
      fprintf(fp, "Null IF_INFO\n");
    } 
  } else if (WN_operator(wn) == OPR_CALL) {
    CALL_INFO *info = (CALL_INFO *) WN_MAP_Get(LNO_Info_Map,wn);
    if (info) {
      if (info != NULL) { 
	fprintf(fp,"The call info is \n");
	info->Print(fp);
      } else { 
        fprintf(fp, "Null CALL_INFO\n");
      } 
    }
  } else if (WN_operator(wn) == OPR_ARRAY) {
    ACCESS_ARRAY *array = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map,wn);
    if (array != NULL) {
      fprintf(fp,"The access array is \n"); array->Print(fp);
    } else { 
      fprintf(fp,"Null ACCESS_ARRAY\n");
    } 
  }
} 

// Print all the access vectors in a routine
extern void LNO_Print_Access(FILE *fp, WN *wn)
{
  WN *kid;

  if (!wn) return;

  if (OPCODE_is_leaf(WN_opcode(wn))) return;

  if (WN_opcode(wn) == OPC_BLOCK) {
    kid = WN_first (wn);
    while (kid) {
      LNO_Print_Access(fp,kid);
      kid = WN_next(kid);
    }
    return;
  } 

  LNO_Print_One_Access(fp, wn); 

  for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
    kid = WN_kid(wn,kidno);
    LNO_Print_Access(fp,kid);
  }

}

//-----------------------------------------------------------------------
// NAME: Exp_Node_Varies_In_Loop
// FUNCTION:  Returns TRUE if the node 'wn_node' varies in the loop
//  'wn_loop', returns FALSE otherwise. 
//-----------------------------------------------------------------------

extern BOOL Exp_Node_Varies_In_Loop(WN* wn_node, 
				    WN* wn_loop)
{ 
  DU_MANAGER* du = Du_Mgr; 
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 

  if (WN_operator(wn_node) == OPR_LDID) {
    LWN_ITER* itr = LWN_WALK_TreeIter(WN_do_body(wn_loop));
    for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
      WN* wn = itr->wn;
      if (OPCODE_is_store(WN_opcode(wn))
          && Aliased(Alias_Mgr, wn_node, wn) != NOT_ALIASED)
        return TRUE;
      if (OPCODE_is_call(WN_opcode(wn))
          && Aliased_with_region(Alias_Mgr, wn_node, wn, WRITE) != NOT_ALIASED)
        return TRUE;
    }
  }
  if (WN_operator(wn_node) == OPR_ILOAD) {
    VINDEX16 v = dg->Get_Vertex(wn_node);
    if (v == 0)
      return TRUE;
    if (Enclosing_Loop(wn_node) == NULL)
      return TRUE;
    EINDEX16 e = 0;
    for (e = dg->Get_Out_Edge(v); e != 0; e = dg->Get_Next_Out_Edge(e)) {
      WN* wn_def = dg->Get_Wn(dg->Get_Sink(e));
      if (Wn_Is_Inside(wn_def, wn_loop))
        return TRUE;
    }
  }
  return FALSE; 
} 

//-----------------------------------------------------------------------
// NAME: Exp_Varies_In_Loop
// FUNCTION:  Returns TRUE if the expression 'wn_exp' varies in the loop
//  'wn_loop', returns FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Exp_Varies_In_Loop(WN* wn_exp, 
			       WN* wn_loop)
{ 
  if (Exp_Node_Varies_In_Loop(wn_exp, wn_loop))
    return TRUE; 
  
  for (INT i = 0; i < WN_kid_count(wn_exp); i++)  
    if (Exp_Varies_In_Loop(WN_kid(wn_exp, i), wn_loop))
      return TRUE; 
  return FALSE; 
} 

//-----------------------------------------------------------------------
// NAME: Hoist_Varying_Lower_Bounds_Traverse
// FUNCTION: For each loop lower bounds in the tree rooted at 'wn_tree' 
//   which is assigned in its loop, put that lower bound into a preg 
//   and hoist it out of the loop. 
//-----------------------------------------------------------------------

static void Hoist_Varying_Lower_Bounds_Traverse(WN* wn_tree)
{ 
  DOLOOP_STACK shortstack(&LNO_local_pool);
  if (WN_opcode(wn_tree) == OPC_DO_LOOP 
      && Exp_Varies_In_Loop(WN_kid0(WN_start(wn_tree)), wn_tree)) {
    Build_Doloop_Stack(wn_tree, &shortstack); 
    Hoist_Lower_Bound(wn_tree, &shortstack, &LNO_default_pool); 
  } 

  if (WN_opcode(wn_tree) == OPC_BLOCK) { 
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      Hoist_Varying_Lower_Bounds_Traverse(wn); 
  } else { 
    for (INT i = 0; i < WN_kid_count(wn_tree); i++) 
      Hoist_Varying_Lower_Bounds_Traverse(WN_kid(wn_tree, i)); 
  } 
}

//-----------------------------------------------------------------------
// NAME: Hoist_Varying_Lower_Bounds
// FUNCTION: Put any loop lower bound which is assigned in its loop into 
//   a preg and hoist it out of the loop.
//-----------------------------------------------------------------------

extern void Hoist_Varying_Lower_Bounds(WN* func_nd)
{
  Hoist_Varying_Lower_Bounds_Traverse(func_nd); 
}
