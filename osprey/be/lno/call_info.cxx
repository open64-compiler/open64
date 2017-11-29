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


// -*-C++-*-
/* ====================================================================
 * ====================================================================
 *
 * Module: call_info.cxx
 * $Revision: 
 * $Date: 
 * $Author:
 * $Source:
 *
 * Revision history:
 *  20-Nov-96 - Original Version 
 *
 * Description:
 *
 * Basic call information required to analyze loops
 *
 * ====================================================================
 * ====================================================================
 */

#include <stdint.h>
#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include "call_info.h"
#include "lwn_util.h"
#include "targ_const.h"
#include "opt_du.h"
#include "debug.h"
#include "tlog.h"
#include "ipa_lno_read.h"

extern MEM_POOL ARA_memory_pool;

void CALL_INFO::Print(FILE *fp)
{
  fprintf(fp, "CALL %s FROM %s AT 0x%p. ", WB_Whirl_Symbol(_wn_call), 
    ST_name(WN_st(Current_Func_Node)), _wn_call);
  if (_needs_evaluation)
    fprintf(fp, "Needs evaluation. ");
  if (_needs_evaluation) { 
    if (_is_evaluated)
      fprintf(fp, "Evaluated. ");
    else 
      fprintf(fp, "Unevaluated. ");
  }
  fprintf(fp, "\n");
  _ara_call->CI_Print(fp);
  if (_value != NULL) {
    for (INT i = 0; i <= _value->Lastidx(); i++) 
      (*_value)[i].WB_Print(fp, i);
  } else { 
    fprintf(fp, "NULL SUMMARY_VALUEs\n");
  } 
  if (_expr != NULL) { 
    for (INT i = 0; i <= _expr->Lastidx(); i++) 
      (*_expr)[i].WB_Print(fp, i);
  } else { 
    fprintf(fp, "NULL SUMMARY_EXPRs\n");
  } 
}

void CALL_INFO::Tlog_Print()
{
  char bf[MAX_TLOG_CHARS]; 
  INT new_ccount = 0;
  new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, "CALL ");
  new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, 
    (char*) WB_Whirl_Symbol(_wn_call));
  new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, " FROM ");
  new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, 
    ST_name(WN_st(Current_Func_Node)));
  new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, ". ");
  Generate_Tlog("LNO", "Call_Info", (SRCPOS) 0, "", "", "", bf);
  new_ccount = 0;
  if (_needs_evaluation)
    new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, 
      "Needs evaluation. ");
  if (_needs_evaluation) {
    if (_is_evaluated)
      new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, "Evaluated. ");
    else
      new_ccount = snprintfs(bf, new_ccount, MAX_TLOG_CHARS, "Unevaluated. ");
  }
  if (new_ccount > 0)
    Generate_Tlog("LNO", "Call_Info", (SRCPOS) 0, "", "", "", bf);
  _ara_call->Tlog_CI_Print();
  // Need to generate execution cost (value,expr) pairs at some point. 
}


// Given a WN*, loop depth and loop stack, create an ACCESS_VECTOR

static ACCESS_VECTOR *Wn_To_Access_Vector(
	WN* wn, INT16 loop_depth, DOLOOP_STACK *loop_stack)
{
    ST* st=WN_st(wn);
    ACCESS_VECTOR* av=
	CXX_NEW(ACCESS_VECTOR((INT64)loop_depth,&ARA_memory_pool),
	        &ARA_memory_pool);
    av->Too_Messy=FALSE;
    if (ST_class(st)==CLASS_CONST) {
        av->Const_Offset=Targ_To_Host(STC_val(st));
    } else {
        SYMBOL sym(wn);
        av->Add_Symbol(1,sym,loop_stack,wn);
    }
    return av;
}

// Create an Access Array which has all 0s for enclosing loop coefficients

static ACCESS_ARRAY *Create_Dummy_Access_Array(INT16 dim, INT16 loop_depth)
{
    ACCESS_ARRAY* kernel=
	CXX_NEW(ACCESS_ARRAY(dim,loop_depth,&ARA_memory_pool),
		&ARA_memory_pool);
    for (INT i=0; i<dim; i++)
      for (INT j=0; j<loop_depth; j++) {
	kernel->Dim(i)->Set_Loop_Coeff(j,0);
	kernel->Dim(i)->Too_Messy=FALSE;
    }
    kernel->Too_Messy=FALSE;
    return kernel;
}

// Given lower bound, upper bound and stride WNs, construct an AXLE_NODE

static void Prepare_Axle(
	WN* lb_wn, WN* ub_wn, WN* stride_wn,
	INT16 loop_depth, DOLOOP_STACK* loop_stack,
	REGION* array_reg, INT16 axle_id)
{
      ST* stride_st=WN_st(stride_wn);

      ACCESS_VECTOR* lbav=Wn_To_Access_Vector(lb_wn,loop_depth,loop_stack);
      if (lbav->Has_Loop_Coeff()) {
	BOOL* is_independent=array_reg->_kernel->Get_Independent_Loops();
        for (INT k=0; k<loop_depth; k++)
	  if (lbav->Loop_Coeff(k))
	    is_independent[k]=FALSE;
      }
      CON_PAIR* lbcp=CXX_NEW(CON_PAIR(lbav),&ARA_memory_pool);

      ACCESS_VECTOR* ubav=Wn_To_Access_Vector(ub_wn,loop_depth,loop_stack);
      if (ubav->Has_Loop_Coeff()) {
	BOOL* is_independent=array_reg->_kernel->Get_Independent_Loops();
        for (INT k=0; k<loop_depth; k++)
	  if (ubav->Loop_Coeff(k))
	    is_independent[k]=FALSE;
      }
      CON_PAIR* ubcp=CXX_NEW(CON_PAIR(ubav),&ARA_memory_pool);

      INT stride=Targ_To_Host(STC_val(stride_st));

      if (*lbav == *ubav) {
        array_reg->_axle[axle_id].Set_Axle(lbcp,0,stride,array_reg->Num_Dim());
      } else {
        array_reg->_axle[axle_id].Set_Axle(lbcp,ubcp,
				stride,array_reg->Num_Dim());
      }
}

// process each call and looks at the parameters to construct ARA_LOOP_INFO
// each array region can be specified by a set (>=5) of consecutive parameters:
// array name -- e.g. a
// access kind -- e.g. 'r' or 'w'
// for i in num_of_dim_of array a
//   WN* lower bound of i-th dim
//   WN* upper bound of i-th dim
//   int stride of i-th dim

static void Scan_Parameters(WN* call_stmt, ARA_LOOP_INFO* ali)
{
  INT16 loop_depth=Do_Loop_Depth(Enclosing_Do_Loop(call_stmt))+1;
  DOLOOP_STACK *loop_stack=CXX_NEW(DOLOOP_STACK(&ARA_memory_pool),
                                 &ARA_memory_pool);
  Build_Doloop_Stack(call_stmt, loop_stack);
  INT16 kidno=0;
  while (kidno+5<WN_kid_count(call_stmt)) {
    WN* array_wn=WN_kid0(WN_kid(call_stmt,kidno++));
    SYMBOL array_sym(array_wn);
    ST* array_st=WN_st(array_wn);
    TY_IDX array_ty=ST_type(array_st);
    if (TY_kind(array_ty)==KIND_POINTER)
      array_ty=TY_pointed(array_ty);
    INT dim=TY_AR_ndims(array_ty);
    WN* read_or_write=WN_kid0(WN_kid(call_stmt,kidno++));
    ST* rw_st=WN_st(read_or_write);
    BOOL is_read=((*Targ_String_Address(STC_val(rw_st))=='r'));

    REGION* array_reg=CXX_NEW(REGION(0,dim),&ARA_memory_pool);
    array_reg->_axle=CXX_NEW_ARRAY(AXLE_NODE,dim,&ARA_memory_pool);
    array_reg->_type=ARA_NORMAL;
    array_reg->_coupled=FALSE;
    array_reg->_wn_list.Push(call_stmt);
    ACCESS_ARRAY* kernel=Create_Dummy_Access_Array(dim,loop_depth);
    array_reg->_kernel=CXX_NEW(KERNEL_IMAGE(kernel), &ARA_memory_pool);

    for (INT16 i=0; i<dim; i++) {
        WN* lb=WN_kid0(WN_kid(call_stmt,kidno++));
        WN* ub=WN_kid0(WN_kid(call_stmt,kidno++));
        WN* stride=WN_kid0(WN_kid(call_stmt,kidno++));
	Prepare_Axle(lb,ub,stride, loop_depth, loop_stack, array_reg, i);
    }
    REGION* new_region=CXX_NEW(REGION(*array_reg),&ARA_memory_pool);
    array_reg->_kernel->Set_Region(new_region);

    ARA_REF* new_ref=CXX_NEW(ARA_REF(&array_sym, array_reg, ali, TRUE),
				&ARA_memory_pool);
    if (is_read)
      ali->Add_Use(new_ref);
    else
      ali->Add_Def(new_ref);

  }
}

// process calls to subroutines with special prefix "lno_test"
// and generate CALL_INFO

void Process_Call(WN* call_stmt) 
{
  char* name=ST_name(WN_st(call_stmt));
  if (strncmp(name,"lno_test",8))
    return;
  ARA_LOOP_INFO* ali=CXX_NEW(ARA_LOOP_INFO(call_stmt, NULL, TRUE), 
    &ARA_memory_pool);
  Scan_Parameters(call_stmt,ali);
  ali->Print(stdout);
  CALL_INFO* call_info=CXX_NEW(CALL_INFO(ali, call_stmt, FALSE, 
    &ARA_memory_pool), &ARA_memory_pool);
  Set_Call_Info(call_stmt, call_info);
}

extern void Call_Info_Walk(WN* root) 
{
  WN* call_stmt=root;
  while (call_stmt=LWN_Get_Next_Stmt_Node(call_stmt)) {
    //if (OPCODE_is_call(WN_opcode(call_stmt)))
    if (WN_operator(call_stmt)==OPR_CALL)
      Process_Call(call_stmt);
  }

}

//-----------------------------------------------------------------------
// NAME: IPA_LNO_Evaluate_Scalar_Formals
// FUNCTION: Evaluate the 'formal_number'th of the 'wn_call' node on the
//   stack 'st_scalar'.
//-----------------------------------------------------------------------

static void IPA_LNO_Evaluate_Scalar_Formals(WN* wn_call,
                                            INT formal_number,
                                            SCALAR_STACK* st_scalar)
{
  WN* wn_symbol = NULL;
  WN* wn_parm = WN_kid(wn_call, formal_number);
  FmtAssert(WN_operator(wn_parm) == OPR_PARM,
    ("IPA_LNO_Evaluate_Scalar_Formals: Expecting PARM node"));
  WN* wn_lda = WN_kid0(wn_parm);
  if (WN_operator(wn_lda) == OPR_INTCONST)
    return; 
  for (INT i = 0; i < st_scalar->Elements(); i++) {
    SCALAR_NODE* sn = st_scalar->Bottom_nth(i);
    DYN_ARRAY<WN*> wn_list(&LNO_local_pool);
    DYN_ARRAY<INT> int_list(&LNO_local_pool);
    INT64 const_value = 0;
    if (sn->_scalar.Is_Formal() &&
        sn->_scalar.Formal_Number() == formal_number) {
      if (WN_operator(wn_lda) == OPR_LDA || WN_operator(wn_lda) == OPR_LDID) {
        SYMBOL sym_lda(WN_st(wn_lda), WN_offset(wn_lda), sn->_scalar.Type);
        WN* wn_single = Single_Definition_Temp(wn_lda);
	if (wn_single != NULL && Scalar_Expr(wn_single))
	  Add_Scalars_In_Expr(wn_single, st_scalar);
	st_scalar->Add_Scalar(wn_lda, &sym_lda, 0);
      } else if (Scalar_Expr(wn_lda) && Linear_Expr(wn_lda, &wn_list,
          &int_list, &const_value)) {
        Add_Scalars_In_Expr(wn_lda, st_scalar);
      } else { 
        FmtAssert(FALSE,
        ("IPA_LNO_Read_Formal: Expecting LDA, LDID, INTCONST, or S-LIN Exp")); 
      }
    }
    st_scalar->Clear_Formal(formal_number);
  }
}

//-----------------------------------------------------------------------
// NAME: IPA_LNO_Find_Formal_Value
// FUNCTION: Return the node which defines the 'formal_number'th parameter
//   of 'wn_call' (assuming that it is a scalar formal).  Return NULL if
//   it is not a scalar formal, or if it is difficult to find.
//-----------------------------------------------------------------------

static WN* IPA_LNO_Find_Formal_Value(WN* wn_call,
                                     INT formal_number)
{
  WN* wn_use = WN_kid(wn_call, formal_number);
  FmtAssert(WN_operator(wn_use) == OPR_PARM,
    ("IPA_LNO_Find_Formal_Value: Expecting PARM node"));
  if (WN_Parm_By_Reference(wn_use)) {
    LWN_ITER* itr = LWN_WALK_TreeIter(wn_use);
    DEF_LIST *def_list = Du_Mgr->Ud_Get_Def(wn_use);
    if (def_list == NULL)
      return NULL;
    DEF_LIST_ITER iter(def_list);
    const DU_NODE* node = NULL;
    WN* wn_def = NULL;
    for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
      if (wn_def != NULL)
        return NULL;
      wn_def = node->Wn();
    }
    if (wn_def == NULL)
      return NULL;
    wn_use = WN_kid0(wn_def);
  } else {
    wn_use = WN_kid0(wn_use);
  }
  return wn_use;
}

//-----------------------------------------------------------------------
// NAME: IPA_LNO_Evaluate_Array_Formals
// FUNCTION: Replace all references to the 'formal_number' in 'st' with the
//   access vector equivalent of 'wn_use'.  The 'stack' is the stack of loops
//   enclosing 'wn_use'.
//-----------------------------------------------------------------------

static void IPA_LNO_Evaluate_Array_Formals(ARA_REF_ST* st,
                                           WN* wn_call,
                                           INT formal_number)
{
  WN* wn_use = NULL;
  DOLOOP_STACK* stack = NULL;
  for (INT i = 0; i < st->Elements(); i++) {
    ARA_REF* ar = st->Bottom_nth(i);
    REGION_UN* rgun = &ar->Image();
    if (rgun == NULL)
      continue;
    REGION_ITER iter(rgun);
    for (REGION* rg = iter.First(); !iter.Is_Empty(); iter.Next()) {
      if (rg->_type == ARA_TOP || rg->_type == ARA_BOTTOM
          || rg->_type == ARA_TOO_MESSY)
        continue;
      for (INT i = 0; i < rg->_dim; i++) {
        AXLE_NODE* ax = &rg->_axle[i];
        CON_PAIR* cp_lo = ax->lo;
        // DevWarn("rcox: Ignoring WN_OFFSET and TYPE_ID for array formals");
        if (cp_lo != NULL) {
          ACCESS_VECTOR* av_lo = cp_lo->_ac_v;
          if (av_lo != NULL) {
            if (wn_use == NULL) {
              wn_use = IPA_LNO_Find_Formal_Value(wn_call, formal_number);
              if (wn_use != NULL) {
                stack = CXX_NEW(DOLOOP_STACK(&LNO_local_pool),&LNO_local_pool);
                Build_Doloop_Stack(LWN_Get_Parent(wn_use), stack);
                LNO_Build_Access(wn_use, stack, &LNO_default_pool);
              }
            }
            av_lo->Substitute(formal_number, wn_use, stack, 
	      LNO_Allow_Nonlinear);
          }
        }
        CON_PAIR* cp_up = ax->up;
        if (cp_up != NULL) {
          ACCESS_VECTOR* av_up = cp_up->_ac_v;
          if (av_up != NULL) {
            if (wn_use == NULL) {
              wn_use = IPA_LNO_Find_Formal_Value(wn_call, formal_number);
              if (wn_use != NULL) {
                stack = CXX_NEW(DOLOOP_STACK(&LNO_local_pool), &LNO_local_pool);                Build_Doloop_Stack(LWN_Get_Parent(wn_use), stack);
                LNO_Build_Access(wn_use, stack, &LNO_default_pool);
              }
            }
            av_up->Substitute(formal_number, wn_use, stack, 
	      LNO_Allow_Nonlinear);
          }
        }
      }
    }
  }
}

//-----------------------------------------------------------------------
// NAME: IPA_LNO_Evaluate_Formal_Symbols
// FUNCTION: Evaluate the formal symbols in the scalar stacks and access
//   vectors of 'wn_call'.
//-----------------------------------------------------------------------

static void IPA_LNO_Evaluate_Formal_Symbols(WN* wn_call)
{
  if (!Has_Call_Info(wn_call))
    return;
  CALL_INFO* ci = Get_Call_Info(wn_call);
  ARA_LOOP_INFO* ali = ci->Call_Ara_Info();
  for (INT i = 0; i < WN_kid_count(wn_call); i++) {
    SCALAR_STACK* st_sdef = &ali->SCALAR_MAY_DEF();
    IPA_LNO_Evaluate_Scalar_Formals(wn_call, i, st_sdef);
    SCALAR_STACK* st_suse = &ali->SCALAR_USE();
    IPA_LNO_Evaluate_Scalar_Formals(wn_call, i, st_suse);
    ARA_REF_ST* st_def = &ali->MAY_DEF();
    IPA_LNO_Evaluate_Array_Formals(st_def, wn_call, i);
    ARA_REF_ST* st_use = &ali->USE();
    IPA_LNO_Evaluate_Array_Formals(st_use, wn_call, i);
  }
  if (Get_Trace(TP_LNOPT2, TT_CALL_INFO)) {
    fprintf(stdout, "==========\n");
    fprintf(stdout, "EVALUATING %s AT 0x%p\n", WB_Whirl_Symbol(wn_call),
      wn_call);
    CALL_INFO* call_info = (CALL_INFO *) WN_MAP_Get(LNO_Info_Map, wn_call);
    call_info->Print(stdout);
  }
}

//-----------------------------------------------------------------------
// NAME: CALL_INFO::Evaluate
// FUNCTION: Evaluate the CALL_INFO's formal parameters.
//-----------------------------------------------------------------------

void CALL_INFO::Evaluate()
{
  if (!_needs_evaluation)
    return; 
  if (_is_evaluated) {
    DevWarn("CALL_INFO::Evaluate: Already evaluated");
    return;
  } 
  FmtAssert(_wn_call != NULL, 
    ("CALL_INFO::Evaluate: Must be assigned to particular call"));
  // OUCH!! BOGUS ... UNTIL WE GET A TRUE CONSTRUCTOR
  _ara_call_save = CXX_NEW(ARA_LOOP_INFO(_ara_call), _pool);
  ARA_LOOP_INFO* ali = Call_Ara_Info();
  for (INT i = 0; i < WN_kid_count(_wn_call); i++) {
    SCALAR_STACK* st_sdef = &ali->SCALAR_MAY_DEF();
    IPA_LNO_Evaluate_Scalar_Formals(_wn_call, i, st_sdef);
    SCALAR_STACK* st_suse = &ali->SCALAR_USE();
    IPA_LNO_Evaluate_Scalar_Formals(_wn_call, i, st_suse);
    ARA_REF_ST* st_def = &ali->MAY_DEF();
    IPA_LNO_Evaluate_Array_Formals(st_def, _wn_call, i);
    ARA_REF_ST* st_use = &ali->USE();
    IPA_LNO_Evaluate_Array_Formals(st_use, _wn_call, i);
  }
  _is_evaluated = TRUE;
}

//-----------------------------------------------------------------------
// NAME: CALL_INFO::Unevaluate
// FUNCTION: Evaluate the CALL_INFO's formal parameters.
//-----------------------------------------------------------------------

void CALL_INFO::Unevaluate()
{
  if (!_needs_evaluation)
    return;
  if (!_is_evaluated) {
    DevWarn("CALL_INFO::Unevaluate: Already unevaluated"); 
    return;
  }
  CXX_DELETE(_ara_call, _pool); 
  _ara_call = _ara_call_save; 
  _ara_call_save = NULL; 
  _is_evaluated = FALSE; 
} 

//-----------------------------------------------------------------------
// NAME: CALL_INFO::CALL_INFO
// FUNCTION: Constructor for CALL_INFO.
//-----------------------------------------------------------------------

CALL_INFO::CALL_INFO(CALL_INFO* ci) 
{
  FmtAssert(ci != NULL, 
    ("CALL_INFO constructor: Called with NULL pointer"));
  _is_evaluated = ci->_is_evaluated;
  _needs_evaluation = ci->_needs_evaluation;
  _wn_call = ci->_wn_call;
  _pool = ci->_pool;
  FmtAssert(_pool == &ARA_memory_pool, 
    ("CALL_INFO::CALL_INFO: can only allocate from ARA_memory_pool"));
  if (_ara_call != NULL)
    _ara_call = CXX_NEW(ARA_LOOP_INFO(ci->_ara_call), _pool);
  if (_ara_call_save != NULL)
    _ara_call_save = CXX_NEW(ARA_LOOP_INFO(ci->_ara_call_save), _pool);
  _value = CXX_NEW(DYN_ARRAY<SUMMARY_VALUE>(_pool), _pool);
  INT i;
  for (i = 0; i <= ci->_value->Lastidx(); i++)
    _value->AddElement((*(ci->_value))[i]);
  _expr = CXX_NEW(DYN_ARRAY<SUMMARY_EXPR>(_pool), _pool);
  for (i = 0; i <= ci->_expr->Lastidx(); i++)
    _expr->AddElement((*(ci->_expr))[i]);
} 

//-----------------------------------------------------------------------
// NAME: CALL_INFO::Has_Formal_Parameter
// FUNCTION: Returns TRUE if CALL_INFO has at least one SYMBOL with a 
//   formal parameter (which will therefore require evaluation).
// NOTE: This is not a search of all possible symbols.  I'm assuming that 
//   formal parameters are only going to appear in the "may_def" and "use"
//   in the CALL_INFO. 
//-----------------------------------------------------------------------

BOOL CALL_INFO::Has_Formal_Parameter()
{ 
  ARA_LOOP_INFO* ali = Call_Ara_Info();
  if (ali == NULL)
    return FALSE; 
  SCALAR_STACK* st_scalar = &ali->SCALAR_MAY_DEF();
  INT i;
  for (i = 0; i < st_scalar->Elements(); i++) {
    SCALAR_NODE* sn = st_scalar->Bottom_nth(i);
    if (sn->_scalar.Is_Formal())
       return TRUE; 
  } 
  st_scalar = &ali->SCALAR_USE();
  for (i = 0; i < st_scalar->Elements(); i++) {
    SCALAR_NODE* sn = st_scalar->Bottom_nth(i);
    if (sn->_scalar.Is_Formal())
       return TRUE; 
  } 
  for (i = 0; i < ali->MAY_DEF().Elements(); i++) {
    ARA_REF* ara_ref = ali->MAY_DEF().Bottom_nth(i);
    if (ara_ref->Has_Formal_Parameter())
      return TRUE; 
  } 
  for (i = 0; i < ali->USE().Elements(); i++) {
    ARA_REF* ara_ref = ali->USE().Bottom_nth(i);
    if (ara_ref->Has_Formal_Parameter())
      return TRUE; 
  } 
  return FALSE; 
} 
