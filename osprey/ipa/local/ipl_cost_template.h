/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <sys/elf_whirl.h>
#include <sys/types.h>
#include "defs.h" 
#include "mtypes.h"
#include "access_vector.h" 
#include "ipl_lno_util.h"
#include "ipl_summary.h" 
#include "ipl_summarize.h" 
#include "ipa_cost_util.h" 

// These two functions have been moved here to lessen recompilation time.

//-----------------------------------------------------------------------
// NAME: IPL_LNO_Make_Icon
// FUNCTION: Create an integer constant of type 'wtype' and value 'i'. 
//-----------------------------------------------------------------------

static WN* IPL_LNO_Make_Icon(TYPE_ID wtype, INT64 i)
{
  OPCODE intconst_opc = OPCODE_make_op(OPR_INTCONST, wtype, MTYPE_V);
  return WN_CreateIntconst(intconst_opc, i);
}

//-----------------------------------------------------------------------
// NAME: IPL_LNO_Do_Wtype
// FUNCTION: Return the TYPE_ID of the DO loop 'wn'. 
//-----------------------------------------------------------------------

static TYPE_ID IPL_LNO_Do_Wtype(WN* wn)
{
  Is_True(WN_opcode(wn) == OPC_DO_LOOP,
    ("IPL_LNO_Do_Wtype: requires do parameter"));
  Is_True(WN_start(wn) && WN_operator(WN_start(wn)) == OPR_STID,
    ("IPL_LNO_Do_Wtype: bad do start, op=%d", WN_opcode(WN_start(wn))));
  return WN_desc(WN_start(wn));
}

//-----------------------------------------------------------------------
// NAME: Step_Size
// FUNCTION: Step_Size sets the step to a new integer.  It also returns the
//   old value.  To not change it, pass a step of 0.  Returns 0 if
//   non-integral.  Pass either the loop or the step itself.
//-----------------------------------------------------------------------

static INT64 Step_Size(WN* loop,
                       INT64 newstep)
{
  WN* step;

  if (WN_opcode(loop) == OPC_DO_LOOP) {
    step = WN_step(loop);
    WN* index = WN_index(loop);

    if (WN_st(step) != WN_st(index) || WN_offset(step) != WN_offset(index)) {
      DevWarn("Index %s/%d but assignment to %s/%d in step",
              ST_name(WN_st(step)), WN_offset(step),
              ST_name(WN_st(index)), WN_offset(index));
      FmtAssert(newstep == 0, ("Bug in Step_Size"));
      return 0;
    }
  }
  else {
    step = loop;
  }

  if (WN_operator(step) != OPR_STID) {
    DevWarn("Step expression operator not STID: %s",
            OPERATOR_name(WN_operator(step)));
    FmtAssert(newstep == 0, ("Bug in Step_Size"));
    return 0;
  }

  WN* kid = WN_kid0(step);

  OPERATOR opr = WN_operator(kid);
  if (opr != OPR_ADD && opr != OPR_SUB) {
    FmtAssert(newstep == 0,
              ("Require ADD or SUB for step, but saw `%s'",
               OPERATOR_name(opr)));
    return 0;
  }

  BOOL neg = (opr == OPR_SUB);

  WN* ldkid = WN_kid0(kid);
  WN* constkid = WN_kid1(kid);
  INT constkidno = 1;

  if (WN_operator(ldkid) != OPR_LDID) {
    if (!neg) {
      WN* tmp = ldkid;
      ldkid = constkid;
      constkid = tmp;
      constkidno = 0;
    }
    if (WN_operator(ldkid) != OPR_LDID) {
      FmtAssert(newstep == 0, ("Saw the add, but not of the right thing"));
      return 0;
    }
  }

  if (WN_operator(constkid) != OPR_INTCONST) {
    if (newstep != 0) {
      LWN_Delete_Tree(constkid);
      WN_kid(kid,constkidno) = IPL_LNO_Make_Icon(IPL_LNO_Do_Wtype(loop),
                                             neg?-newstep:newstep);
      LWN_Set_Parent(WN_kid(kid,constkidno), kid);
    }
    return 0;
  }
  else {
    INT64 rval = WN_const_val(constkid);
    if (newstep != 0)
      WN_const_val(constkid) = neg ? -newstep : newstep;
    return neg ? -rval : rval;
  }
}

//-----------------------------------------------------------------------
// NAME: Step_Size
// FUNCTION: Returns the step size of 'loop'.  Returns 0 if a non-integral
//   value of step size is found.
//-----------------------------------------------------------------------

static INT64 Step_Size(WN* loop)
{
  return Step_Size((WN*)loop, 0);
}

//-----------------------------------------------------------------------
// NAME: IPL_GEN_Value
// FUNCTION: Try to generate a SUMMARY_VALUE for the expression 'wn_value', 
//   and add it to the pair ('sv','sx').  If this is successful, return the
//   index of the added expression, otherwise return -1. 
//-----------------------------------------------------------------------

template <PROGRAM program>
INT SUMMARIZE<program>::IPL_GEN_Value(WN* wn_value, 
			              DYN_ARRAY<SUMMARY_VALUE>* sv,
                                      DYN_ARRAY<SUMMARY_EXPR>* sx)
{
  SUMMARY_DESC result; 
  INT old_value_idx = _value.Lastidx(); 
  Classify_const_value(result, wn_value);
  switch (result.Get_type()) { 
  case VALUE_EXPR: {
    INT sx_index = IPL_EX_Expr(result.Get_wn(), sv, sx);
    return sx_index;
  }  
  case VALUE_FORMAL: 
  case VALUE_GLOBAL: 
    break;
  case VALUE_UNKNOWN: 
  case VALUE_INT_CONST: 
  case VALUE_TWO_CONSTS: 
  case VALUE_CONST: 
  case VALUE_SYMBOL: 
  case VALUE_PHI: 
  case VALUE_CHI: 
  case VALUE_CALLSITE: 
  case VALUE_NOT_CONST: 
    return -1; 
  }  
  INT value_index = Process_jump_function(&result);
  if (value_index == -1)
    return -1; 
  INT sv_index = sv->Newidx();
  SUMMARY_VALUE* svv = &(*sv)[sv_index];  
  SUMMARY_VALUE* svv_new = Get_value(value_index); 
  INT new_value_idx = _value.Lastidx();
  BCOPY(svv_new, svv, sizeof(SUMMARY_VALUE));
  if (new_value_idx > old_value_idx)  
    _value.Decidx();
  INT sx_index = sx->Newidx();
  SUMMARY_EXPR* sxx = &(*sx)[sx_index];
  sxx->Clear_is_trip_count();
  sxx->Set_has_const_operand();
  sxx->Set_const_value((INT64) 0);
  OPERATOR opr = OPR_ADD; 
  TYPE_ID type = MTYPE_I4; 
  OPCODE opcode = OPCODE_make_op(opr, type, MTYPE_V);
  sxx->Set_opcode(opcode); 
  sxx->Set_expr_value(0);
  sxx->Set_node_index(0, sv_index); 
  sxx->Set_mtype(type);
  return sx_index; 
} 

//-----------------------------------------------------------------------
// NAME: IPL_GEN_Expr
// FUNCTION: Generate a new SUMMARY_EXPR for 'sx', with OPERATOR 'opr', 
//   which combines the expression at indices 'exp_one' and 'exp_two'. 
//-----------------------------------------------------------------------

template <PROGRAM program>
INT SUMMARIZE<program>::IPL_GEN_Expr(OPERATOR opr, 
		                     INT exp_one, 
		        	     INT exp_two, 
		        	     DYN_ARRAY<SUMMARY_EXPR>* sx)
{ 
  INT index = sx->Newidx();
  SUMMARY_EXPR* sxx = &(*sx)[index];
  sxx->Clear_is_trip_count();
  sxx->Clear_has_const_operand();
  TYPE_ID type = MTYPE_I4; 
  OPCODE opcode = OPCODE_make_op(opr, type, MTYPE_V);
  sxx->Set_opcode(opcode);
  sxx->Set_expr_expr(0);
  sxx->Set_expr_expr(1);
  sxx->Set_node_index(0, exp_one);
  sxx->Set_node_index(1, exp_two);
  sxx->Set_mtype(type);
  return index; 
} 

//-----------------------------------------------------------------------
// NAME: IPL_GEN_Const
// FUNCTION: Generate a "constant" with the guven 'value' and place it 
//   in the pair ('sv','sx').  
// NOTE: This type of constant is generated as a SUMMARY_EXPR ('value' 
//   + 0).  This is done so that we can simplify the final result more
//   easily and uniformly. 
//-----------------------------------------------------------------------

template <PROGRAM program>
INT SUMMARIZE<program>::IPL_GEN_Const(INT value, 
			 	      DYN_ARRAY<SUMMARY_VALUE>* sv,
			 	      DYN_ARRAY<SUMMARY_EXPR>* sx)  
{ 
  TYPE_ID type = MTYPE_I4; 
  INT sv_index = IPL_EX_New_Constant(sv, value);
  INT sx_index = sx->Newidx();
  SUMMARY_EXPR* sxx = &(*sx)[sx_index];
  sxx->Clear_is_trip_count();
  sxx->Set_has_const_operand();
  sxx->Set_const_value((INT64) 0);
  OPERATOR opr = OPR_ADD; 
  OPCODE opcode = OPCODE_make_op(opr, MTYPE_I4, MTYPE_V);
  sxx->Set_opcode(opcode); 
  sxx->Set_expr_value(0);
  sxx->Set_node_index(0, sv_index); 
  sxx->Set_mtype(type);
  return sx_index; 
} 

//-----------------------------------------------------------------------
// NAME: Easy_Trip_Count
// FUNCTION: Return TRUE if the trip count of loop 'wn_loop' is an "easy"
//   trip count, i.e. we can easily represent it using the SUMMARY_VALUEs
//   and SUMMARY_EXPRs.  If we return TRUE, 'wn_addr_ub' will point to 
//   the upper bound of the loop, 'wn_addr_lb' will point to the lower 
//   bound of the loop, and 'addr_intconst' will point to the integer 
//   stride. 
//-----------------------------------------------------------------------

template <PROGRAM program>
BOOL SUMMARIZE<program>::Easy_Trip_Count(WN* wn_loop,
			                 WN** wn_addr_ub,
			                 WN** wn_addr_lb,
			                 INT* addr_intconst)
{ 
  INT64 step_size = Step_Size(wn_loop);
  if (step_size != 1 && step_size != -1)
    return FALSE; 
  WN* wn_end = WN_end(wn_loop);
  if (WN_operator(wn_end) != OPR_LT && WN_operator(wn_end) != OPR_GT 
      && WN_operator(wn_end) != OPR_LE && WN_operator(wn_end) != OPR_GE)
    return FALSE; 
  if (step_size == 1) { 
    if (WN_operator(wn_end) == OPR_LT || WN_operator(wn_end) == OPR_LE) { 
      WN* wn_index = WN_kid0(wn_end); 
      if (WN_operator(wn_index) != OPR_LDID)
        return FALSE; 
      if (SYMBOL(WN_index(wn_loop)) != SYMBOL(wn_index))
        return FALSE; 
      *wn_addr_lb = WN_kid0(WN_start(wn_loop));
      *wn_addr_ub = WN_kid1(WN_end(wn_loop));
      *addr_intconst = WN_operator(wn_end) == OPR_LE ? 1 : 0; 
    } else { 
      WN* wn_index = WN_kid1(wn_end); 
      if (WN_operator(wn_index) != OPR_LDID)
        return FALSE; 
      if (SYMBOL(WN_index(wn_loop)) != SYMBOL(wn_index))
        return FALSE; 
      *wn_addr_ub = WN_kid0(WN_start(wn_loop));
      *wn_addr_lb = WN_kid0(WN_end(wn_loop));
      *addr_intconst = WN_operator(wn_end) == OPR_GE ? 1 : 0; 
    } 
  } else { 
    if (WN_operator(wn_end) == OPR_LT || WN_operator(wn_end) == OPR_LE) { 
      WN* wn_index = WN_kid0(wn_end); 
      if (WN_operator(wn_index) != OPR_LDID)
        return FALSE; 
      if (SYMBOL(WN_index(wn_loop)) != SYMBOL(wn_index))
        return FALSE; 
      *wn_addr_ub = WN_kid0(WN_start(wn_loop));
      *wn_addr_lb = WN_kid1(WN_end(wn_loop));
      *addr_intconst = WN_operator(wn_end) == OPR_LE ? 1 : 0; 
    } else { 
      WN* wn_index = WN_kid1(wn_end); 
      if (WN_operator(wn_index) != OPR_LDID)
        return FALSE; 
      if (SYMBOL(WN_index(wn_loop)) != SYMBOL(wn_index))
        return FALSE; 
      *wn_addr_lb = WN_kid0(WN_start(wn_loop));
      *wn_addr_ub = WN_kid0(WN_end(wn_loop));
      *addr_intconst = WN_operator(wn_end) == OPR_GE ? 1 : 0; 
    } 
  } 
  return TRUE;  
}  

//-----------------------------------------------------------------------
// NAME: IPL_EX_Expr
// FUNCTION: Develop an intra-procedural cost estimate for the expression
//   'wn_expr', and place it in the pair ('sv',sx').
//-----------------------------------------------------------------------
  

template <PROGRAM program>
INT SUMMARIZE<program>::IPL_EX_Expr(WN* wn_expr, 
		                    DYN_ARRAY<SUMMARY_VALUE>* sv,
                                    DYN_ARRAY<SUMMARY_EXPR>* sx)
{
  if (IPL_EXS_Too_Complicated(sv, sx, 1))
    return -1; 
  switch (WN_operator(wn_expr)) { 
  case OPR_ADD:  
  case OPR_SUB:  
  case OPR_MPY: 
  case OPR_DIV: {
    INT expr_one = IPL_EX_Expr(WN_kid0(wn_expr), sv, sx);
    if (expr_one == -1)
      return -1;
    INT expr_two = IPL_EX_Expr(WN_kid1(wn_expr), sv, sx);
    if (expr_two == -1)
      return -1;
    INT expr_result = IPL_GEN_Expr(WN_operator(wn_expr), expr_one, 
      expr_two, sx);
    return expr_result; 
  } 
  case OPR_NEG: {  
    INT expr_one = IPL_GEN_Const(0, sv, sx);
    INT expr_two = IPL_EX_Expr(WN_kid0(wn_expr), sv, sx);
    if (expr_two == -1)
      return -1; 
    INT expr_result = IPL_GEN_Expr(OPR_SUB, expr_one, expr_two, sx);
    return expr_result; 
  } 
  case OPR_LDID: {
    INT expr_value = IPL_GEN_Value(wn_expr, sv, sx);
    return expr_value; 
  } 
  case OPR_INTCONST: { 
    INT const_value = WN_const_val(wn_expr);
    INT expr_value = IPL_GEN_Const(const_value, sv, sx);
    return expr_value; 
  } 
  default:
    return -1;   
  } 
} 

//-----------------------------------------------------------------------
// NAME: IPL_EX_Trip_Count
// FUNCTION: Develop an intra-procedural cost estimate for the trip count
//   of loop 'wn_loop' and place it in the pair ('sv',sx'). If 
//   'constant_estimate' is TRUE, produce only a constant estimate.
//-----------------------------------------------------------------------

template <PROGRAM program>
INT SUMMARIZE<program>::IPL_EX_Trip_Count(WN* wn_loop, 
		                          DYN_ARRAY<SUMMARY_VALUE>* sv,
			                  DYN_ARRAY<SUMMARY_EXPR>* sx,
					  BOOL constant_estimate)  
{
  if (IPL_EXS_Too_Complicated(sv, sx, 1))
    return -1; 
  WN* wn_ub = NULL; 
  WN* wn_lb = NULL; 
  INT int_const = -1; 
  if (constant_estimate 
      || !Easy_Trip_Count(wn_loop, &wn_ub, &wn_lb, &int_const))
    return IPL_GEN_Const(DEFAULT_TRIP_COUNT, sv, sx);
  INT expr_end = IPL_EX_Expr(wn_ub, sv, sx);
  if (expr_end == -1)
    return IPL_GEN_Const(DEFAULT_TRIP_COUNT, sv, sx);
  INT expr_start = IPL_EX_Expr(wn_lb, sv, sx);
  if (expr_start == -1)
    return IPL_GEN_Const(DEFAULT_TRIP_COUNT, sv, sx);
  INT expr_sum = IPL_GEN_Expr(OPR_SUB, expr_end, expr_start, sx);
  if (int_const == 0) {
    SUMMARY_EXPR* sxx = &(*sx)[expr_sum];
    sxx->Set_is_trip_count();
    return expr_sum; 
  } 
  INT expr_one = IPL_GEN_Const(1, sv, sx);
  INT expr_result = IPL_GEN_Expr(OPR_ADD, expr_sum, expr_one, sx);
  SUMMARY_EXPR* sxx = &(*sx)[expr_result];
  sxx->Set_is_trip_count();
  return expr_result; 
}   

//-----------------------------------------------------------------------
// NAME: IPL_EX_Call
// FUNCTION: Develop an intra-procedural cost estimate for the CALL 
//   'wn_call', and and place it in the pair ('sv',sx').
//-----------------------------------------------------------------------

template <PROGRAM program>
INT SUMMARIZE<program>::IPL_EX_Call(WN* wn_call, 
		                    DYN_ARRAY<SUMMARY_VALUE>* sv,
                                    DYN_ARRAY<SUMMARY_EXPR>* sx)
{ 
  ST_IDX st_idx = ST_st_idx(WN_st(wn_call));
  INT i;

  for (i = Summary->Get_callsite_idx(); i >= 0; i--) { 
    SUMMARY_CALLSITE* sc = Summary->Get_callsite(i);
    if (sc->Get_map_id() == WN_map_id(wn_call))
      break; 
  } 

  FmtAssert(i >= 0, ("IPL_EX_Call: Expected to find map-id for call")); 
  INT sv_index = sv->Newidx();
  SUMMARY_VALUE* svv = &(*sv)[sv_index];  
  svv->Set_callsite();
  svv->Set_callsite_index(i);
  svv->Set_mtype(WN_rtype(wn_call));
  svv->Clear_is_addr_of();
  svv->Clear_is_trip_count();
  INT sx_index = sx->Newidx();
  SUMMARY_EXPR* sxx = &(*sx)[sx_index];
  sxx->Clear_is_trip_count();
  sxx->Set_has_const_operand();
  sxx->Set_const_value((INT64) 0);
  OPERATOR opr = OPR_ADD; 
  TYPE_ID type = MTYPE_I4; 
  OPCODE opcode = OPCODE_make_op(opr, type, MTYPE_V);
  sxx->Set_opcode(opcode); 
  sxx->Set_expr_value(0);
  sxx->Set_node_index(0, sv_index); 
  sxx->Set_mtype(type);
  return sx_index; 
} 

//-----------------------------------------------------------------------
// NAME: IPL_EX_Statement
// FUNCTION: Develop an intra-procedural cost estimate for the code in
//   the statement 'wn_statement', and place it in the pair ('sv',sx'). If
//   'constant_estimate' is TRUE, produce only a constant estimate.
//-----------------------------------------------------------------------

template <PROGRAM program>
INT SUMMARIZE<program>::IPL_EX_Statement(WN* wn_statement, 
			                 DYN_ARRAY<SUMMARY_VALUE>* sv,
			                 DYN_ARRAY<SUMMARY_EXPR>* sx,
					 BOOL constant_estimate)
{ 
  if (IPL_EXS_Too_Complicated(sv, sx, 1))
    return -1; 
  switch (WN_operator(wn_statement)) { 
  case OPR_DO_LOOP: { 
    INT exp_trip = IPL_EX_Trip_Count(wn_statement, sv, sx, constant_estimate);
    if (exp_trip == -1)
      return -1; 
    INT exp_body = IPL_EX_Block(WN_do_body(wn_statement), sv, sx,
      constant_estimate);
    if (exp_body == -1)
      return -1; 
    INT exp_result = IPL_GEN_Expr(OPR_MPY, exp_trip, exp_body, sx);
    return exp_result; 
  } 
  case OPR_DO_WHILE: 
  case OPR_WHILE_DO: 
    return IPL_EX_Block(WN_while_body(wn_statement), sv, sx,
      constant_estimate);
  case OPR_IF: { 
    INT if_nodes = Node_Count(WN_if_test(wn_statement));
    INT exp_if = IPL_GEN_Const(if_nodes, sv, sx);
    INT exp_then = IPL_EX_Block(WN_then(wn_statement), sv, sx,
      constant_estimate);
    if (exp_then == -1)
      return -1;
    INT exp_sum_one = IPL_GEN_Expr(OPR_ADD, exp_if, exp_then, sx);
    INT exp_else = IPL_EX_Block(WN_else(wn_statement), sv, sx,
      constant_estimate);
    if (exp_else == -1)
      return -1; 
    INT exp_sum_two = IPL_GEN_Expr(OPR_ADD, exp_sum_one, exp_else, sx);
    return exp_sum_two; 
  } 
  case OPR_REGION: 
    return IPL_EX_Block(WN_region_body(wn_statement), sv, sx,
      constant_estimate); 
  case OPR_CALL: 
    return IPL_EX_Call(wn_statement, sv, sx);
  default: { 
    INT default_nodes = Node_Count(wn_statement);
    INT exp_const = IPL_GEN_Const(default_nodes, sv, sx);
    return exp_const; 
  } 
  }
} 

//-----------------------------------------------------------------------
// NAME: IPL_EX_Block
// FUNCTION: Develop an intra-procedural cost estimate for the code in 
//   the BLOCK 'wn_block', and place it in the pair ('sv',sx'). If 
//   'constant_estimate' is TRUE, produce only a constant estimate.
//-----------------------------------------------------------------------

template <PROGRAM program>
INT SUMMARIZE<program>::IPL_EX_Block(WN* wn_block,
			             DYN_ARRAY<SUMMARY_VALUE>* sv,
			             DYN_ARRAY<SUMMARY_EXPR>* sx,
				     BOOL constant_estimate)
{
  if (IPL_EXS_Too_Complicated(sv, sx, 1))
    return -1; 
  WN* wn_first = WN_first(wn_block);
  if (wn_first == NULL)
    return IPL_GEN_Const(0, sv, sx);
  INT exp_one = IPL_EX_Statement(wn_first, sv, sx, constant_estimate);
  if (exp_one == -1)
    return -1; 
  WN* wn_next = WN_next(wn_first);
  if (wn_next == NULL)  
     return exp_one;
  INT exp_old = exp_one; 
  for (WN* wn = wn_next; wn != NULL; wn = WN_next(wn)) {
    INT exp_new = IPL_EX_Statement(wn, sv, sx, constant_estimate);
    if (exp_new == -1)
      return -1; 
    INT exp_result = IPL_GEN_Expr(OPR_ADD, exp_old, exp_new, sx);
    exp_old = exp_result; 
  } 
  return exp_old; 
} 

//-----------------------------------------------------------------------
// NAME: IPL_Execution_Cost
// FUNCTION: Develop an intra-procedural cost estimate for the procedure 
//   'wn_func' whose SUMMARY_PROCEDURE is 'sp', and place it in the program
//   summary.  If 'constant_estimate' is TRUE, produce only a constant 
//   valued estimate, otherwise use expressions. 
//-----------------------------------------------------------------------

template <PROGRAM program>
void SUMMARIZE<program>::IPL_Execution_Cost(WN* wn_func,
					    SUMMARY_PROCEDURE* sp, 
			                    MEM_POOL* mem_pool,
					    BOOL constant_estimate) 
{
  INT i; 
  DYN_ARRAY<SUMMARY_VALUE>* sv
    = CXX_NEW(DYN_ARRAY<SUMMARY_VALUE>(mem_pool), mem_pool);
  DYN_ARRAY<SUMMARY_EXPR>* sx
    = CXX_NEW(DYN_ARRAY<SUMMARY_EXPR>(mem_pool), mem_pool);
  INT expr_index = IPL_EX_Block(WN_func_body(wn_func), sv, sx, 
    constant_estimate);
  if (expr_index == -1)  
    IPL_EXS_Chop_Down_Estimate(sv, sx);
  if (Get_Trace(TP_IPL, TT_IPL_EXCOST)) { 
    fprintf(stdout, "BEFORE SIMPLIFICATION: \n");
    Print_Exprs(stdout, sv, sx);
  } 
  IPL_EX_Simplify(sv, sx);
  if (Get_Trace(TP_IPL, TT_IPL_EXCOST)) { 
    fprintf(stdout, "AFTER SIMPLIFICATION: \n");
    Print_Exprs(stdout, sv, sx);
  } 
  INT value_offset = _value.Lastidx() + 1;
  INT expr_offset = _expr.Lastidx() + 1;
  IPL_EX_Add_Expr_Offsets(sx, value_offset, expr_offset);
  for (i = 0; i <= sv->Lastidx(); i++)  
    _value.AddElement((*sv)[i]);
  for (i = 0; i <= sx->Lastidx(); i++)
    _expr.AddElement((*sx)[i]);
  INT ex_value_index = value_offset;
  INT ex_value_count = sv->Lastidx() + 1;
  INT ex_expr_index = expr_offset;
  INT ex_expr_count = sx->Lastidx() + 1;
  sp->Set_ex_value_index(ex_value_index);
  sp->Set_ex_value_count(ex_value_count);
  sp->Set_ex_expr_index(ex_expr_index);
  sp->Set_ex_expr_count(ex_expr_count);
}
 
