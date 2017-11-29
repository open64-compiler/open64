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
#include <limits.h>
#include <stdio.h>
#include "pu_info.h"
#include "defs.h"
#include "wn.h"
#include "strtab.h"
#include "targ_sim.h"
#include "ipa_section.h" 
#include "ipa_lno_summary.h"
#include "ipa_lno_info.h"
#include "ara_loop.h"
#include "call_info.h"
#include "lnopt_main.h"
#include "lwn_util.h"
#include "opt_du.h"
#include "ipa_lno_reshape.h"
#include "debug.h"
#include "reshape.h"
#include "ipl_lno_util.h"
#include "be_util.h"
#include "ipl_summary.h" 
#include "ipa_cost_util.h" 
#include "ipa_lno_file.h"
#include "ipa_lno_read.h"

//-----------------------------------------------------------------------
// NAME: LNO_IPA_Read_Sections
// FUNCTION: If the TT_IPA_LNO_READ trace flag is set, print a summary of
//   of the contents of the IPALNO file to stdout.
//-----------------------------------------------------------------------

static void IPA_LNO_Read_Sections(IPA_LNO_READ_FILE* IPA_LNO_File)
{
  if (Get_Trace(TP_LNOPT2, TT_IPA_LNO_READ))  
    fprintf(stdout, "\n+++ BEGIN READING IPA LNO FILE +++\n");

  // IVAR section
  IVAR* ivar_array
    = (IVAR*) IPA_LNO_File->Section_Address(IPA_IVAR);
  INT ivar_size = IPA_LNO_File->Section_Size(IPA_IVAR);
  INT ivar_count = ivar_size / sizeof(IVAR);
  if (Get_Trace(TP_LNOPT2, TT_IPA_LNO_READ)) { 
    fprintf(stdout, "Reading %d ivars\n", ivar_count);
    for (INT i = 0; i < ivar_count; i++)
      ivar_array[i].IPA_LNO_Print_File(stdout, i);
  } 

  // PROCEDURE section
  IPA_LNO_SUMMARY_PROCEDURE* procedure_array
    = (IPA_LNO_SUMMARY_PROCEDURE*) IPA_LNO_File->Section_Address(IPA_PROCEDURE);  INT procedure_size = IPA_LNO_File->Section_Size(IPA_PROCEDURE);
  INT procedure_count = procedure_size / sizeof(IPA_LNO_SUMMARY_PROCEDURE);
  if (Get_Trace(TP_LNOPT2, TT_IPA_LNO_READ)) { 
    fprintf(stdout, "Reading %d procedures\n", procedure_count);
    for (INT i = 0; i < procedure_count; i++)
      procedure_array[i].Print(stdout, i);
  } 

  // FORMAL section
  IPA_LNO_SUMMARY_FORMAL* formal_array
    = (IPA_LNO_SUMMARY_FORMAL*) IPA_LNO_File->Section_Address(IPA_FORMAL);
  INT formal_size = IPA_LNO_File->Section_Size(IPA_FORMAL);
  INT formal_count = formal_size / sizeof(IPA_LNO_SUMMARY_FORMAL);
  if (Get_Trace(TP_LNOPT2, TT_IPA_LNO_READ)) { 
    fprintf(stdout, "Reading %d formals\n", formal_count);
    for (INT i = 0; i < formal_count; i++)
      formal_array[i].Print(stdout, i);
  } 

  // GLOBAL section
  IPA_LNO_SUMMARY_GLOBAL* global_array
    = (IPA_LNO_SUMMARY_GLOBAL*) IPA_LNO_File->Section_Address(IPA_GLOBAL);
  INT global_size = IPA_LNO_File->Section_Size(IPA_GLOBAL);
  INT global_count = global_size / sizeof(IPA_LNO_SUMMARY_GLOBAL);
  if (Get_Trace(TP_LNOPT2, TT_IPA_LNO_READ)) { 
    fprintf(stdout, "Reading %d globals\n", global_count);
    for (INT i = 0; i < global_count; i++)
      global_array[i].Print(stdout, i);
  } 

  // PROJECTED REGION section
  PROJECTED_REGION* projected_region_array
    = (PROJECTED_REGION*) IPA_LNO_File->
    Section_Address(IPA_PROJECTED_REGION);
  INT pr_size = IPA_LNO_File->Section_Size(IPA_PROJECTED_REGION);
  INT pr_count = pr_size / sizeof(PROJECTED_REGION);
  if (Get_Trace(TP_LNOPT2, TT_IPA_LNO_READ)) { 
    fprintf(stdout, "Reading %d projected regions\n", pr_count);
    for (INT i = 0; i < pr_count; i++)
      projected_region_array[i].IPA_LNO_Print_File(stdout, i);
  } 

  // PROJECTED (NODE) ARRAY section
  PROJECTED_NODE* projected_node_array
    = (PROJECTED_NODE*) IPA_LNO_File->Section_Address(IPA_PROJECTED_ARRAY);
  INT pn_size = IPA_LNO_File->Section_Size(IPA_PROJECTED_ARRAY);
  INT pn_count = pn_size / sizeof(PROJECTED_NODE);
  if (Get_Trace(TP_LNOPT2, TT_IPA_LNO_READ)) { 
    fprintf(stdout, "Reading %d projected nodes\n", pn_count);
    for (INT i = 0; i < pn_count; i++)
      projected_node_array[i].IPA_LNO_Print_File(stdout, i);
  } 

  // TERM ARRAY section
  TERM* term_array = (TERM*) IPA_LNO_File->Section_Address(IPA_TERM_ARRAY);
  INT term_size = IPA_LNO_File->Section_Size(IPA_TERM_ARRAY);
  INT term_count = term_size / sizeof(TERM);
  if (Get_Trace(TP_LNOPT2, TT_IPA_LNO_READ)) { 
    fprintf(stdout, "Reading %d terms\n", term_count);
    for (INT i = 0; i < term_count; i++)
      term_array[i].IPA_LNO_Print_File(stdout, i);
  } 

  // VALUE section
  SUMMARY_VALUE* value_array 
    = (SUMMARY_VALUE*) IPA_LNO_File->Section_Address(IPA_VALUE);
  INT value_size = IPA_LNO_File->Section_Size(IPA_VALUE);
  INT value_count = value_size / sizeof(SUMMARY_VALUE);
  if (Get_Trace(TP_LNOPT2, TT_IPA_LNO_READ)) { 
    fprintf(stdout, "Reading %d values\n", value_count);
    for (INT i = 0; i < value_count; i++)
      value_array[i].WB_Print(stdout, i);
  } 

  // EXPR section
  SUMMARY_EXPR* expr_array 
    = (SUMMARY_EXPR*) IPA_LNO_File->Section_Address(IPA_EXPR);
  INT expr_size = IPA_LNO_File->Section_Size(IPA_EXPR);
  INT expr_count = expr_size / sizeof(SUMMARY_EXPR);
  if (Get_Trace(TP_LNOPT2, TT_IPA_LNO_READ)) { 
    fprintf(stdout, "Reading %d exprs\n", expr_count);
    for (INT i = 0; i < expr_count; i++)
      expr_array[i].WB_Print(stdout, i);
  } 

  if (Get_Trace(TP_LNOPT2, TT_IPA_LNO_READ))  
    fprintf(stdout, "+++ END READING IPA LNO FILE +++\n\n");
}

//-----------------------------------------------------------------------
// NAME: Find_Av_Loop_Index
// FUNCTION: Return the depth of the loop enclosing 'wn_call' which has 
//   'sym_formal' as its index variable, if there is such a loop.  Other-
//   wise, return -1. 
//-----------------------------------------------------------------------

static INT Find_Av_Loop_Index(WN* wn_call, 
			      SYMBOL* sym_formal)
{
  WN* wn = NULL;
  for (wn = wn_call; wn != NULL; wn = LWN_Get_Parent(wn))
    if (WN_opcode(wn) == OPC_DO_LOOP && SYMBOL(WN_index(wn)) == *sym_formal)
      break; 
  if (wn == NULL)
    return -1; 
  else 
    return Do_Loop_Depth(wn);
} 

//-----------------------------------------------------------------------
// NAME: Has_Optimizable_Node_Traverse
// FUNCTION: Traverse the 'wn_tree' and return TRUE if there is a node 
//   which might be changed by some optimization algorithm in LNO.  Re-
//   turn FALSE otherwise.  In the case that we return TRUE, a formal 
//   number placeholder will be used in the access vector representation
//   of 'wn_tree' in the CALL_INFO.  
//-----------------------------------------------------------------------

static BOOL Has_Optimizable_Node_Traverse(WN* wn_tree)
{
  INT64 val = 0;
  if (WN_operator(wn_tree) == OPR_LDA)
    return FALSE; 
  if (OPCODE_has_sym(WN_opcode(wn_tree)) && !Wn_Is_Intconst(wn_tree, &val))
    return TRUE; 
  if (WN_operator(wn_tree) == OPR_ARRAY) {
    for (INT i = 0; i < WN_num_dim(wn_tree); i++) 
      if (Has_Optimizable_Node_Traverse(WN_array_index(wn_tree, i)))
	return TRUE; 
  } else if (WN_opcode(wn_tree) == OPC_BLOCK) { 
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      if (Has_Optimizable_Node_Traverse(wn))
        return TRUE;
  } else { 
    for (INT i = 0; i < WN_kid_count(wn_tree); i++)
      if (Has_Optimizable_Node_Traverse(WN_kid(wn_tree, i)))
	return TRUE; 
  } 
  return FALSE; 
} 

//-----------------------------------------------------------------------
// NAME: Has_Optimizable_Node
// FUNCTION: Return TRUE if the 'formal_number'th argument of 'wn_call'
//   contains a node which may be altered by some optimization algorithm
//   in LNO.  Return FALSE otherwise.
//-----------------------------------------------------------------------

static BOOL Has_Optimizable_Node(WN* wn_call,
				 INT formal_number)
{
  if (!(formal_number >= 0 && formal_number < WN_kid_count(wn_call)))
    fprintf(stdout, "formal_number = %d\n", formal_number);
  FmtAssert(formal_number >= 0 && formal_number < WN_kid_count(wn_call), 
    ("Has_Optimizable_Node: formal_number out of range"));
  WN* wn_tree = WN_kid(wn_call, formal_number);
  return Has_Optimizable_Node_Traverse(wn_tree);
} 

//-----------------------------------------------------------------------
// NAME: Add_Access_Vector_Entry
// FUNCTION: Given the access vector 'av' associated with the call 'wn_call',
//   add 'coeff' * 'sym' to that access vector. 
//-----------------------------------------------------------------------

static void Add_Access_Vector_Entry(WN* wn_call, 
				    SYMBOL* sym, 
				    INT coeff, 
				    ACCESS_VECTOR* av,
				    MEM_POOL* mem_pool)
{
  INT loop_index = Find_Av_Loop_Index(wn_call, sym);
  if (loop_index == -1) {
    if (av->Lin_Symb == NULL)
      av->Lin_Symb = CXX_NEW(INTSYMB_LIST, mem_pool);
    INTSYMB_NODE* isn_formal = CXX_NEW(INTSYMB_NODE(sym, coeff), 
      mem_pool);
    av->Lin_Symb->Append(CXX_NEW(INTSYMB_NODE(isn_formal), mem_pool));
  } else {
    INT value = av->Loop_Coeff(loop_index) + 1;
    av->Set_Loop_Coeff(loop_index, value);
  }
}

//-----------------------------------------------------------------------
// NAME: Single_Definition_Temp
// FUNCTION: For the LDA or LDID call argument 'wn_argument', return the
//  WN* of the unique STID which assigns its value, if there is one.  
//  Otherwise, return NULL. 
//-----------------------------------------------------------------------

extern WN* Single_Definition_Temp(WN* wn_argument) 
{ 
  OPERATOR opr_argument = WN_operator(wn_argument);
  FmtAssert(opr_argument == OPR_LDA || opr_argument == OPR_LDID, 
    ("Single_Definition_Temp: Expecting LDA or LDID"));
  WN* wn_use = opr_argument == OPR_LDA ? LWN_Get_Parent(wn_argument)
    : wn_argument; 
  DEF_LIST *def_list = Du_Mgr->Ud_Get_Def(wn_use);
  if (def_list == NULL || def_list->Incomplete())
    return NULL; 
  const DU_NODE* node = NULL;
  WN* wn_def = NULL; 
  DEF_LIST_ITER iter(def_list);
  for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
    if (wn_def == NULL)
      wn_def = node->Wn();
    else 
      return NULL;
  }
  if (WN_operator(wn_def) != OPR_STID)
    return NULL; 
  return WN_kid0(wn_def); 
} 

//-----------------------------------------------------------------------
// NAME: Scalar_Expr
// FUNCTION: Return TRUE if all of the nodes with symbols in the tree 
//   rooted at 'wn_expr' have symbols which are of KIND_SCALAR.  Return
//   FALSE otherwise. 
//-----------------------------------------------------------------------

extern BOOL Scalar_Expr(WN* wn_expr)
{
  if (OPCODE_has_sym(WN_opcode(wn_expr))) { 
    ST* st_expr = WN_st(wn_expr);
    if (st_expr == NULL)
      return FALSE; 
    if (TY_kind(ST_type(st_expr)) != KIND_SCALAR)
      return FALSE; 
    switch (WN_operator(wn_expr)) {
    case OPR_LDID: 
    case OPR_STID: 
    case OPR_LDA:
    case OPR_CALL:
    case OPR_CONST: 
      break;
    default: 
      return FALSE; 
    } 
  }
  for (INT i = 0; i < WN_kid_count(wn_expr); i++) 
    if (!Scalar_Expr(WN_kid(wn_expr, i)))
      return FALSE; 
  return TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: Linear_Expr_With_Mode
// FUNCTION: Return TRUE if 'wn_expr' can be evaluated as an integer 
//   coefficient linear combination of LDIDs.  Return FALSE otherwise.  
//   If we return TRUE, the value of 'coefficient' * 'wn_expr' is given as 
//   the sum of the products of the corresponding entries in 'wn_list' and 
//   'int_list' plus 'const_value'.  
//-----------------------------------------------------------------------

static BOOL Linear_Expr_With_Mode(WN* wn_expr,
			          INT coefficient, 
                                  DYN_ARRAY<WN*>* wn_list,
                                  DYN_ARRAY<INT>* int_list,
                                  INT64* const_value) 
{
  switch (WN_operator(wn_expr)) { 
  case OPR_INTCONST: 
    *const_value += coefficient * WN_const_val(wn_expr);
    break; 
  case OPR_LDID: 
    wn_list->AddElement(wn_expr);
    int_list->AddElement(coefficient);
    break; 
  case OPR_ADD: {
    BOOL ok1 = Linear_Expr_With_Mode(WN_kid0(wn_expr), coefficient, wn_list,
      int_list, const_value);
    if (!ok1)
      return FALSE; 
    BOOL ok2 = Linear_Expr_With_Mode(WN_kid1(wn_expr), coefficient, wn_list,
      int_list, const_value);
    if (!ok2)
      return FALSE; 
    } 
    break; 
  case OPR_SUB: {
    BOOL ok1 = Linear_Expr_With_Mode(WN_kid0(wn_expr), coefficient, wn_list,
      int_list, const_value);
    if (!ok1)
      return FALSE; 
    BOOL ok2 = Linear_Expr_With_Mode(WN_kid1(wn_expr), -coefficient, wn_list,
      int_list, const_value);
    if (!ok2)
      return FALSE; 
    } 
    break; 
  case OPR_MPY: {
    WN* wn_constant = WN_kid0(wn_expr);
    WN* wn_non_constant = WN_kid1(wn_expr);
    if (WN_operator(wn_constant) != OPR_INTCONST) {
      wn_constant = WN_kid1(wn_expr);
      wn_non_constant = WN_kid0(wn_expr);
      if (WN_operator(wn_constant) != OPR_INTCONST)
	return FALSE; 
    } 
    INT64 constant_value = WN_const_val(wn_constant);
    BOOL ok = Linear_Expr_With_Mode(wn_non_constant, 
      constant_value * coefficient, wn_list, int_list, const_value);
    if (!ok)
      return FALSE; 
    } 
    break; 
  case OPR_NEG: {
    BOOL ok = Linear_Expr_With_Mode(WN_kid0(wn_expr), -coefficient, 
      wn_list, int_list, const_value);
    if (!ok)
      return FALSE; 
    } 
    break; 
  default: 
    return FALSE; 
  } 
  return TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: Linear_Expr
// FUNCTION: Return TRUE if 'wn_expr' can be evaluated as an integer 
//   coefficient linear combination of LDIDs.  Return FALSE otherwise.  
//   If we return TRUE, the value of 'wn_expr' is given as the sum of the 
//   products of the corresponding entries in 'wn_list' and 'int_list' plus 
//   'const_value'.  
//-----------------------------------------------------------------------

extern BOOL Linear_Expr(WN* wn_expr,
			DYN_ARRAY<WN*>* wn_list,
			DYN_ARRAY<INT>* int_list,
			INT64* const_value)
{
  return Linear_Expr_With_Mode(wn_expr, 1, wn_list, int_list, 
    const_value);
} 

//-----------------------------------------------------------------------
// NAME: Add_Scalars_In_Expr
// FUNCTION: For each node with a symbol in the tree rooted at 'wn_expr',   
//   add an entry to the SCALAR_USE list in the 'ali' to indicate that 
//   the node is a scalar use.  
// NOTE: We assume that all of the nodes in 'wn_expr' represent scalars
//   rather than arrays. 
//-----------------------------------------------------------------------

extern void Add_Scalars_In_Expr(WN* wn_expr,
				SCALAR_STACK* st_scalar)
{
  if (OPCODE_has_sym(WN_opcode(wn_expr))) {
    if (WN_operator(wn_expr) == OPR_LDA) {
      SYMBOL sym_lda(WN_st(wn_expr), (WN_OFFSET) 0, 
	TY_mtype(ST_type(WN_st(wn_expr))));
      st_scalar->Add_Scalar(wn_expr, &sym_lda, 0);
    } else if (WN_operator(wn_expr) != OPR_CONST) {
      st_scalar->Add_Scalar(wn_expr, 0);
    } 
  } 
  for (INT i = 0; i < WN_kid_count(wn_expr); i++)
    Add_Scalars_In_Expr(WN_kid(wn_expr, i), st_scalar);
} 

//-----------------------------------------------------------------------
// NAME: Add_Access_Terms_From_Lists
// FUNCTION: Let 'tm' be a TERM whose value is a multiple of IVAR 'ivar'.
//   Also assume that the 'ivar' evaluates to the linear expression 
//   represented by 'wn_list', 'int_list', and 'const_value' (as described
//   in "Linear_Expr()" above).  Evaluate 'tm->Coeff()' * 'ivar' and accum-
//   ulate the result in 'av', which is an access vector for 'wn_call'.     
//-----------------------------------------------------------------------

static void Add_Access_Terms_From_Lists(WN* wn_call, 
					DYN_ARRAY<WN*>* wn_list,
					DYN_ARRAY<INT>* int_list, 
					INT64 const_value, 
					TERM* tm,
					IVAR* ivar,
					ACCESS_VECTOR* av,
					MEM_POOL* mem_pool)
{
  for (INT j = 0; j <= wn_list->Lastidx(); j++) {
    WN* wn_sym = (*wn_list)[j];
    INT coeff = tm->Get_coeff() * (*int_list)[j];
    SYMBOL sym_formal(WN_st(wn_sym), WN_offset(wn_sym)
      + ivar->Offset(), ivar->Mtype());
    Add_Access_Vector_Entry(wn_call, &sym_formal, coeff,
      av, mem_pool);
  }
  if (const_value != 0)
    av->Const_Offset += tm->Get_coeff() * const_value;
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_Read_Terms
// FUNCTION: Return an ACCESS_VECTOR representation for the call 'wn_call'
//   of the 'idx_count' terms starting at 'idx_terms' in 'IPA_LNO_File'. 
//   Assume that 'wn_call' is enclosed by 'loop_depth' + 1 DO loops. 
//   Also, create a 'coeff' array of integers, if necessary, to represent
//   coupled subscripts (if they appear in this list of terms'.  Otherwise,
//   make 'coeff' NULL. 
//-----------------------------------------------------------------------

static ACCESS_VECTOR* IPA_LNO_Read_Terms(IPA_LNO_READ_FILE* IPA_LNO_File,
					 WN* wn_call, 
			                 INT idx_terms,
			                 INT idx_count, 
			                 INT loop_depth,
					 INT** coeff,
					 MEM_POOL* mem_pool)
{
  ACCESS_VECTOR* av = CXX_NEW(ACCESS_VECTOR(loop_depth, mem_pool), mem_pool);
  INT value = 0;
  av->Const_Offset = 0; 
  av->Too_Messy = FALSE; 
  *coeff = NULL; 
  for (INT i = idx_terms; i < idx_terms + idx_count; i++) {
    TERM* tm = IPA_LNO_File->Term(i);
    switch (tm->Get_type()) {
    case LTKIND_CONST: 
      av->Const_Offset += tm->Get_coeff();
      break;
    case LTKIND_LINDEX: 
      value = av->Loop_Coeff(tm->Get_desc()) + tm->Get_coeff();
      av->Set_Loop_Coeff(tm->Get_desc(), value);
      break;
    case LTKIND_IV: {
      IVAR* ivar = IPA_LNO_File->Ivar(tm->Get_desc());
      WN_OFFSET offset  = ivar->Offset();
      TYPE_ID mtype = ivar->Mtype();
      if (ivar->Is_Formal()) { 
	INT formal_number = ivar->Formal_Position();
        if (Has_Optimizable_Node(wn_call, formal_number)) { 
          SYMBOL sym_formal(formal_number, offset, mtype);
	  if (av->Lin_Symb == NULL)
	    av->Lin_Symb = CXX_NEW(INTSYMB_LIST, mem_pool);
	  INTSYMB_NODE* isn_formal = 
            CXX_NEW(INTSYMB_NODE(&sym_formal, tm->Get_coeff()), mem_pool);
	  av->Lin_Symb->Append(isn_formal); 
        } else { 
	  WN* wn_parm = WN_kid(wn_call, formal_number);
	  FmtAssert(WN_operator(wn_parm) == OPR_PARM, 
	    ("IPA_LNO_Read_Terms: Only handling ref parameters now"));
	  WN* wn_lda = WN_kid0(wn_parm);
          OPERATOR opr_lda = WN_operator(wn_lda);
	  if (opr_lda == OPR_LDA || opr_lda == OPR_LDID) {
	    WN* wn_single = Single_Definition_Temp(wn_lda);
            DYN_ARRAY<WN*> wn_list(&LNO_local_pool);
            DYN_ARRAY<INT> int_list(&LNO_local_pool);
            INT64 const_value = 0;
            if (wn_single != NULL && Scalar_Expr(wn_single)
                && Linear_Expr(wn_single, &wn_list, &int_list, &const_value)) {
	      Add_Access_Terms_From_Lists(wn_call, &wn_list, &int_list, 
		const_value, tm, ivar, av, mem_pool);
	    } else { 
	      SYMBOL sym_formal(WN_st(wn_lda), WN_offset(wn_lda) 
		+ ivar->Offset(), ivar->Mtype());
	      Add_Access_Vector_Entry(wn_call, &sym_formal, tm->Get_coeff(), 
		av, mem_pool);
	    } 
          } else if (opr_lda == OPR_INTCONST) { 
	    INT64 const_value = WN_const_val(wn_lda);
            av->Const_Offset += tm->Get_coeff() * const_value;
          } else { 
            DYN_ARRAY<WN*> wn_list(&LNO_local_pool);
            DYN_ARRAY<INT> int_list(&LNO_local_pool);
            INT64 const_value = 0;
	    FmtAssert(Scalar_Expr(wn_lda) && Linear_Expr(wn_lda, 
	      &wn_list, &int_list, &const_value),
	      ("IPA_LNO_Read_Terms: Non-linear expression"));
	    Add_Access_Terms_From_Lists(wn_call, &wn_list, &int_list, 
	      const_value, tm, ivar, av, mem_pool);
          } 
        } 
      } else { 
        SYMBOL symbol(ST_ptr(ivar->St_Idx()), offset, mtype); 
        Add_Access_Vector_Entry(wn_call, &symbol, tm->Get_coeff(), av, 
	  mem_pool);
      }
      break; 
    } 
    case LTKIND_SUBSCR:
      if (*coeff == NULL)
        *coeff = CXX_NEW_ARRAY(INT, loop_depth + 1, mem_pool);
      (*coeff)[tm->Get_desc()] = tm->Get_coeff();
      break;
    }
  }  
  if (!av->Has_Loop_Coeff()) 
    av->Set_Loop_Coeff(0, 0);
  return av; 
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_Update_Independent_Loops
// FUNCTION: Use the ACCESS_VECTOR 'av' to set indicate which loops are 
//   independent in 'rg_array'.  Assume that the call for which 'rg_array' 
//   is part of its representation is at the given 'loop_depth'. 
//-----------------------------------------------------------------------

static void IPA_LNO_Update_Independent_Loops(ACCESS_VECTOR* av,
					     REGION* rg_array, 
				             INT loop_depth)
{ 
  if (av->Has_Loop_Coeff()) {
    BOOL* is_independent = rg_array->_kernel->Get_Independent_Loops();
    for (INT k = 0; k < loop_depth; k++)
      if (av->Loop_Coeff(k))
	is_independent[k] = FALSE;
  }
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_Read_Stride
// FUNCTION: Given that the stride of LINEX is represented by the 
//   'idx_step_count' terms starting at 'idx_step' in the TERM array 
//   of 'IPA_LNO_File', return the value of that stride. 
// NOTE: We are assuming very simple constant strides at the moment. 
//   If this is not the case, this function will assert. 
//-----------------------------------------------------------------------

static INT IPA_LNO_Read_Stride(IPA_LNO_READ_FILE* IPA_LNO_File,
			       INT idx_step, 
			       INT idx_step_count)
{ 
  if (idx_step == -1 || idx_step_count == 0)
    return 1;
  INT return_value = 1;
  FmtAssert(idx_step_count == 1, 
    ("IPA_LNO_Read_Stride: Can handle only constant stride sections"));
  TERM* tm = IPA_LNO_File->Term(idx_step);
  FmtAssert(tm->Get_type() == LTKIND_CONST,
    ("IPA_LNO_Read_Stride: Can handle only constant stride sections"));
  FmtAssert(tm->Get_desc() == CONST_DESC,
    ("IPA_LNO_Read_Stride: Can handle only constant stride sections"));
  return_value = tm->Get_coeff(); 
  return return_value; 
}

//-----------------------------------------------------------------------
// NAME: IPA_LNO_Read_Projected_Node
// FUNCTION: Add an AXLE to the REGION 'rg_array' corresponding to the 
//   projected node at index 'idx_pn' in the PROJECTED_NODE array of 
//   'IPA_LNO_File', and representing the 'dim_number'th dimension of 
//   that AXLE. This 'rg_array' is being built for 'wn_call' which
//   is at the given 'loop_depth'.  
//-----------------------------------------------------------------------

static void IPA_LNO_Read_Projected_Node(IPA_LNO_READ_FILE* IPA_LNO_File,
					WN* wn_call,  
					INT idx_pn,
					INT loop_depth,
					REGION* rg_array, 
					INT dim_number)
{ 
  if (idx_pn == -1)
    return; 

  PROJECTED_NODE* pn = IPA_LNO_File->Projected_Node(idx_pn); 

  // Handle lower bound
  INT* coeff_lb = NULL;
  INT idx_lb = pn->Get_lb_term_index();
  INT count_lb = pn->Get_lb_term_count();
  ACCESS_VECTOR* av_lb = IPA_LNO_Read_Terms(IPA_LNO_File, wn_call, idx_lb, 
    count_lb, loop_depth, &coeff_lb, &ARA_memory_pool); 
  IPA_LNO_Update_Independent_Loops(av_lb, rg_array, loop_depth);
  CON_PAIR* cp_lb = CXX_NEW(CON_PAIR(av_lb, coeff_lb, loop_depth + 1, 
    &LNO_default_pool), &LNO_default_pool);

  // Handle upper bound
  INT* coeff_ub = NULL;  
  ACCESS_VECTOR* av_ub = NULL;
  INT idx_ub = pn->Get_ub_term_index();
  INT count_ub = pn->Get_ub_term_count();
  if (count_ub == 0) { 
    av_ub = av_lb; 
    coeff_ub = coeff_lb; 
  } else { 
    av_ub = count_ub == 0 ? av_lb : IPA_LNO_Read_Terms(IPA_LNO_File, 
      wn_call, idx_ub, count_ub, loop_depth, &coeff_ub,
      &ARA_memory_pool); 
  }
  IPA_LNO_Update_Independent_Loops(av_ub, rg_array, loop_depth);
  CON_PAIR* cp_ub = CXX_NEW(CON_PAIR(av_ub, coeff_ub, loop_depth + 1,
    &LNO_default_pool), &LNO_default_pool);
   
  // Handle step 
  INT idx_step = pn->Get_step_term_index();
  INT count_step = pn->Get_step_term_count();
  INT stride = IPA_LNO_Read_Stride(IPA_LNO_File, idx_step, count_step);

  // Add the AXLE to the REGION 
  rg_array->_axle[dim_number].Set_Axle(cp_lb, cp_ub, stride, 
    rg_array->Num_Dim());
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_Create_Dummy_Access_Array
// FUNCTION: Returni an empty access array with 'dim' dimensions at 
//   the given 'loop_depth'. 
//-----------------------------------------------------------------------

static ACCESS_ARRAY* IPA_LNO_Create_Dummy_Access_Array(INT dim, 
						       INT loop_depth)
{
  ACCESS_ARRAY* kernel = CXX_NEW(ACCESS_ARRAY(dim, loop_depth, 
    &LNO_default_pool), &LNO_default_pool);
  for (INT i = 0; i < dim; i++) {
    for (INT j = 0; j < loop_depth; j++) {
      kernel->Dim(i)->Set_Loop_Coeff(j, 0);
      kernel->Dim(i)->Too_Messy = FALSE;
    }
  }
  kernel->Too_Messy = FALSE;
  return kernel;
}

//-----------------------------------------------------------------------
// NAME: IPA_LNO_Read_Projected_Region
// FUNCTION: Add entries to the ARA_LOOP_INFO 'ali' which correspond to 
//   the 'idx_pr'th entry in the PROJECTED_REGION array in 'IPA_LNO_File'.
//   The projected region gives the dimensions of the array with symbol
//   
//-----------------------------------------------------------------------

static void IPA_LNO_Read_Projected_Region(IPA_LNO_READ_FILE* IPA_LNO_File,
					  WN* wn_call, 
				          INT idx_pr,
					  SYMBOL* sym_array,
					  ARA_LOOP_INFO* ali,
					  BOOL is_modified)
{
  if (idx_pr == -1)
    return; 
  WN* wn_enclosing_loop = Enclosing_Do_Loop(wn_call);
  INT loop_depth = wn_enclosing_loop == NULL ? 1  
    : Do_Loop_Depth(wn_enclosing_loop) + 1;
  DOLOOP_STACK* st_call = CXX_NEW(DOLOOP_STACK(&LNO_default_pool),
    &LNO_default_pool);
  Build_Doloop_Stack(wn_call, st_call);
  PROJECTED_REGION* pr = IPA_LNO_File->Projected_Region(idx_pr);  
  INT idx_pa_base = pr->Get_id();
  INT pa_count = pr->Get_num_dims();
  REGION* rg_array = CXX_NEW(REGION(0, pa_count), &LNO_default_pool);
  rg_array->_axle = CXX_NEW_ARRAY(AXLE_NODE, pa_count, &LNO_default_pool);  
  rg_array->_type = ARA_NORMAL;
  rg_array->_coupled = FALSE; 
  rg_array->_wn_list.Push(wn_call);
  ACCESS_ARRAY* kernel = IPA_LNO_Create_Dummy_Access_Array(pa_count, 
    loop_depth);
  rg_array->_kernel = CXX_NEW(KERNEL_IMAGE(kernel), &ARA_memory_pool);
  if (pr->Is_messy_region()) { 
    rg_array->_type = ARA_TOO_MESSY;
    ACCESS_ARRAY* aa_kernel = rg_array->_kernel->Get_Kernel();
    aa_kernel->Too_Messy = TRUE; 
  } else { 
    for (INT i = 0; i < pa_count; i++) { 
      INT idx_pn = idx_pa_base + i;  
      IPA_LNO_Read_Projected_Node(IPA_LNO_File, wn_call, idx_pn, loop_depth, 
	rg_array, i); 
    } 
  } 
  REGION* rg_new = CXX_NEW(REGION(*rg_array), &LNO_default_pool);
  rg_array->_kernel->Set_Region(rg_new);
  ARA_REF* ref_new = CXX_NEW(ARA_REF(sym_array, rg_array, ali, TRUE),
    &LNO_default_pool);
  if (is_modified)
    ali->Add_May_Def(ref_new);
  else
    ali->Add_Use(ref_new);
}

//-----------------------------------------------------------------------
// NAME: IPA_LNO_Procedure_Index
// FUNCTION: Returns the index into the PROCEDURE array of 'IPA_LNO_File'
//   that corresponds to the procedure being called in 'wn_call'.  
//   Returns -1 if no procedure index in 'IPA_LNO_File' corresponds to 
//   'wn_call'.
//-----------------------------------------------------------------------

extern INT IPA_LNO_Procedure_Index(WN* wn_call, 
				   IPA_LNO_READ_FILE* IPA_LNO_File)
{ 
  ST_IDX st_idx_procedure = WN_st_idx(wn_call);
  for (INT i = 0; i < IPA_LNO_File->Procedure_Count(); i++) {
    if (IPA_LNO_File->Procedure(i)->St_Idx() == st_idx_procedure) {
      return i; 
    }
  } 
  return -1;
}    

//-----------------------------------------------------------------------
// NAME: Walk_Linex
// FUNCTION: Return an ACCESS_VECTOR which corresponds the LINEX 'lx' 
//   (which contains terms written in terms of the entries in 
//   'IPA_LNO_File') applied to the CALL 'wn_call' at the given 'loop_depth'.
//   Also set the 'coeff' of coupled subscripts, if there are any, otherwise
//   just set 'coeff' to NULL.  
//-----------------------------------------------------------------------

static ACCESS_VECTOR* Walk_Linex(IPA_LNO_READ_FILE* IPA_LNO_File, 
		                 LINEX* lx, 
		                 WN* wn_call, 
		                 INT loop_depth,
		                 INT** coeff, 
		                 MEM_POOL* mem_pool)
{ 
  ACCESS_VECTOR* av = CXX_NEW(ACCESS_VECTOR(loop_depth, mem_pool), mem_pool);
  INT value = 0;
  av->Const_Offset = 0;
  av->Too_Messy = FALSE;
  *coeff = NULL;
  for (INT i = 0; i <= lx->Num_terms(); i++) { 
    TERM* tm = lx->Get_term(i);
    switch (tm->Get_type()) {
    case LTKIND_CONST:
      av->Const_Offset += tm->Get_coeff();
      break;
    case LTKIND_LINDEX:
      value = av->Loop_Coeff(tm->Get_desc()) + tm->Get_coeff();
      av->Set_Loop_Coeff(tm->Get_desc(), value);
      break;
    case LTKIND_IV: {
      IVAR* ivar = IPA_LNO_File->Ivar(tm->Get_desc());
      WN_OFFSET offset  = ivar->Offset();
      TYPE_ID mtype = ivar->Mtype();
      if (ivar->Is_Formal()) { 
        INT formal_number = ivar->Formal_Position();
	WN* wn_sym = WN_kid(Current_Func_Node, formal_number);
        SYMBOL sym_formal(WN_st(wn_sym), offset, mtype);
	Add_Access_Vector_Entry(wn_call, &sym_formal, tm->Get_coeff(), 
	  av, mem_pool);
      } else {
        SYMBOL symbol(ST_ptr(ivar->St_Idx()), offset, mtype); 
        Add_Access_Vector_Entry(wn_call, &symbol, tm->Get_coeff(), av, 
	  mem_pool);
      } 
      break;
    }
    case LTKIND_SUBSCR:
      if (*coeff == NULL)
        *coeff = CXX_NEW_ARRAY(INT, loop_depth + 1, mem_pool);
      (*coeff)[tm->Get_desc()] = tm->Get_coeff();
      break;
    }
  }
  if (!av->Has_Loop_Coeff() && loop_depth > 0)
    av->Set_Loop_Coeff(0, 0);
  return av;
} 

//-----------------------------------------------------------------------
// NAME: Walk_Projected_Node
// FUNCTION: Set the 'dim_number' dimension of the 'rg_array' to be equi-
//   valent to the PROJECTED_NODE 'pn_caller'. Entries in 'pn_caller' may
//   use entries in 'IPA_LNO_File'.  The 'rg_array' is being created for  
//   the CALL node 'wn_call' which appears at the given 'loop_depth'. 
//-----------------------------------------------------------------------

static void Walk_Projected_Node(IPA_LNO_READ_FILE* IPA_LNO_File,
			        PROJECTED_NODE* pn_caller, 
				WN* wn_call, 
				INT loop_depth,
				REGION* rg_array, 
				INT dim_number)
{
  // Handle lower bound
  INT* coeff_lb = NULL;
  LINEX* lb = pn_caller->Get_lower_linex();
  ACCESS_VECTOR* av_lb = Walk_Linex(IPA_LNO_File, lb, wn_call, loop_depth, 
    &coeff_lb, &ARA_memory_pool);
  IPA_LNO_Update_Independent_Loops(av_lb, rg_array, loop_depth);
  CON_PAIR* cp_lb = CXX_NEW(CON_PAIR(av_lb, coeff_lb, loop_depth + 1,
    &LNO_default_pool), &LNO_default_pool);

  // Handle upper bound
  INT* coeff_ub = NULL;
  LINEX* ub = pn_caller->Get_upper_linex();
  ACCESS_VECTOR* av_ub = NULL;
  if (ub == NULL) {
    av_ub = av_lb;
    coeff_ub = coeff_lb;
  } else {
    av_ub = Walk_Linex(IPA_LNO_File, ub, wn_call, loop_depth, &coeff_ub, 
      &ARA_memory_pool); 
  }
  IPA_LNO_Update_Independent_Loops(av_ub, rg_array, loop_depth);
  CON_PAIR* cp_ub = CXX_NEW(CON_PAIR(av_ub, coeff_ub, loop_depth + 1,
    &LNO_default_pool), &LNO_default_pool);

  // Handle step
  INT stride = 1;

  // Add the AXLE to the REGION
  rg_array->_axle[dim_number].Set_Axle(cp_lb, cp_ub, stride,
    rg_array->Num_Dim());
} 

//-----------------------------------------------------------------------
// NAME: Walk_Projected_Region
// FUNCTION: Add an entry to the ARA_LOOP_INFO 'ali' of the CALL node 
//   'wn_call', for an array 'sym_array' with the array section structure 
//   specified by the PROJECTED_REGION 'pr_caller'.  Terms in the 
//   'pr_caller' may refer to entries in 'IPA_LNO_File'.  If 'is_modified'
//   is TRUE, add an entry for this array in the 'ali's DEF list, other-
//   wise, add it to its USE list.
//-----------------------------------------------------------------------

static void Walk_Projected_Region(IPA_LNO_READ_FILE* IPA_LNO_File,
				  PROJECTED_REGION* pr_caller, 
				  WN* wn_call, 
				  SYMBOL* sym_array, 
				  ARA_LOOP_INFO* ali, 
				  BOOL is_modified)
{
  WN* wn_enclosing_loop = Enclosing_Do_Loop(wn_call);
  INT loop_depth = wn_enclosing_loop == NULL ? 0
    : Do_Loop_Depth(wn_enclosing_loop) + 1;
  DOLOOP_STACK* st_call = CXX_NEW(DOLOOP_STACK(&LNO_default_pool),
    &LNO_default_pool);
  Build_Doloop_Stack(wn_call, st_call);
  INT pa_count = pr_caller->Get_num_dims();
  REGION* rg_array = CXX_NEW(REGION(0, pa_count), &LNO_default_pool);
  rg_array->_axle = CXX_NEW_ARRAY(AXLE_NODE, pa_count, &LNO_default_pool);
  rg_array->_type = ARA_NORMAL;
  rg_array->_coupled = FALSE;
  rg_array->_wn_list.Push(wn_call);
  ACCESS_ARRAY* kernel = IPA_LNO_Create_Dummy_Access_Array(pa_count,
    loop_depth);
  rg_array->_kernel = CXX_NEW(KERNEL_IMAGE(kernel), &ARA_memory_pool);
  if (pr_caller->Is_messy_region()) {
    rg_array->_type = ARA_TOO_MESSY;
  } else { 
    for (INT i = 0; i < pa_count; i++) {
      PROJECTED_NODE* pn_caller = pr_caller->Get_projected_node(i);
      Walk_Projected_Node(IPA_LNO_File, pn_caller, wn_call, loop_depth, 
	rg_array, i);
    }
  }
  REGION* rg_new = CXX_NEW(REGION(*rg_array), &LNO_default_pool);
  rg_array->_kernel->Set_Region(rg_new);
  ARA_REF* ref_new = CXX_NEW(ARA_REF(sym_array, rg_array, ali, TRUE),
    &LNO_default_pool);
  if (is_modified)
    ali->Add_May_Def(ref_new);
  else
    ali->Add_Use(ref_new);
} 

//-----------------------------------------------------------------------
// NAME: Attempt_Reshape
// FUNCTION: Attempt to reshape the MOD or REF information the 'formal_
//   number'th argument of 'wn_call'.  The MOD or REF information is 
//   stored on the 'IPA_LNO_File' in the 'idx_pr'th PROJECTED_REGION. 
//   The declaration of the array is given by 'pr_caller_shape' in the 
//   caller and 'pr_callee_shape' in  the callee.  The form of the array 
//   section passed at the argument, if any, is given by 'pr_callsite_region'.
//   If 'ok_shapes' is TRUE, the caller and callee shapes conform (match). 
//   The MOD/REF information is for the array 'sym_array' in the caller. 
//   If 'is_mod' is TRUE, we are attempting to reshape the MOD information,
//   otherwise we are attempting to reshape the REF information.  After
//   reshaping, we add the resulting PROJECTED_REGION to the 'ali'.  We
//   return TRUE if the reshaping was accomplished successfully, FALSE 
//   otherwise. 
//-----------------------------------------------------------------------

static BOOL Attempt_Reshape(IPA_LNO_READ_FILE* IPA_LNO_File,
			    PROJECTED_REGION* pr_caller_shape,
			    PROJECTED_REGION* pr_callee_shape, 
			    PROJECTED_REGION* pr_callsite_region, 
			    INT idx_pr,
			    BOOL ok_shapes,
			    SYMBOL* sym_array, 
			    WN* wn_call,
			    INT formal_number, 
			    ARA_LOOP_INFO* ali,
			    BOOL is_mod)
{
  PROJECTED_REGION* pr_callee_region = NULL; 
  pr_callee_region = Projected_Region(IPA_LNO_File, idx_pr, wn_call);
  const char* callee_name = ST_name(WN_st(wn_call));
  const char* mod_ref_string = is_mod ? "MOD" : "REF";  
  if (pr_callee_region == NULL 
      || pr_callee_region->Is_messy_region()) {
    if (Get_Trace(TP_LNOPT2, TT_CALL_INFO)) {
      fprintf(stdout, "TRY RESHAPE %s: %s ARG %d: ", 
	mod_ref_string, callee_name, formal_number);
      fprintf(stdout, "Could not form projected region\n");
    } 
    return FALSE; 
  } 
  BOOL tried_reshape = FALSE; 
  RESHAPE reshape(pr_caller_shape, pr_callee_shape,
    pr_callee_region, pr_callsite_region, &ARA_memory_pool, FALSE);
  PROJECTED_REGION* pr_callee = NULL;
  if (ok_shapes) {
    pr_callee = pr_callee_region;
    reshape.Set_callee_proj_reshaped_region(pr_callee_region);
  } else {
    tried_reshape = TRUE;
    pr_callee = reshape.Reshape_Callee_To_Caller(FALSE);
  }
  if (pr_callee == NULL || pr_callee->Is_messy_region()) { 
    if (Get_Trace(TP_LNOPT2, TT_CALL_INFO)) {
      fprintf(stdout, "TRY RESHAPE %s: %s ARG %d: ", 
        mod_ref_string, callee_name, formal_number);
      fprintf(stdout, "Could not Perform_Reshape()\n");
    }
    return FALSE; 
  }
  PROJECTED_REGION* pr_caller = 
    Map_Projected_Region(IPA_LNO_File, pr_callee, wn_call);
  if (pr_caller == NULL || pr_caller->Is_messy_region()) {
    if (Get_Trace(TP_LNOPT2, TT_CALL_INFO)) {
      fprintf(stdout, "TRY RESHAPE %s: %s ARG %d: ",
        mod_ref_string, callee_name, formal_number);
      fprintf(stdout, "Could not Map_Projected_Region()\n");
    }
    return FALSE;
  }
  if (pr_callsite_region != NULL) { 
    tried_reshape = TRUE; 
    BOOL ok = reshape.Reshapeable_Passed_Section(FALSE);
    if (!ok) {
      if (Get_Trace(TP_LNOPT2, TT_CALL_INFO)) {
	fprintf(stdout, "TRY RESHAPE %s: %s ARG %d: ", 
	  mod_ref_string, callee_name, formal_number);
        fprintf(stdout, "!Actual_Passed_Reshapable()\n");
      } 
      return FALSE; 
    } 
    reshape.Reshape_Passed_Section(pr_caller, FALSE);
  }
  Walk_Projected_Region(IPA_LNO_File, pr_caller, wn_call, 
    sym_array, ali, is_mod);
  if (tried_reshape && Get_Trace(TP_LNOPT2, TT_CALL_INFO)) 
    fprintf(stdout, "RESHAPE SUCCESSFUL %s: %s ARG %d\n",
      mod_ref_string, callee_name, formal_number);
  if (Get_Trace(TP_LNOPT2, TT_IPA_LNO_READ))  
    fprintf(stdout, "  Formal #%d has array %s section\n", 
      formal_number, mod_ref_string);
  return TRUE; 
}

//-----------------------------------------------------------------------
// NAME: Formal_Machine_Type
// FUNCTION: Returns the machine type of the 'formal_number' (starting 
//   with 0) formal parameter of 'wn_call', if the information exists 
//   in 'IPA_LNO_File'.  Otherwise, it returns MTYPE_UNKNOWN. 
//-----------------------------------------------------------------------

extern TYPE_ID Formal_Machine_Type(WN* wn_call,
				   INT formal_number,
				   IPA_LNO_READ_FILE* IPA_LNO_File)
{
  INT idx_procedure = IPA_LNO_Procedure_Index(wn_call, IPA_LNO_File);
  if (idx_procedure == -1)
    return MTYPE_UNKNOWN; 
  IPA_LNO_SUMMARY_PROCEDURE* sp = IPA_LNO_File->Procedure(idx_procedure);
  if (sp->Has_Incomplete_Array_Info())
    return MTYPE_UNKNOWN;
  INT idx_formal_base = sp->Formal_Index();
  INT idx_formal = idx_formal_base + formal_number;
  IPA_LNO_SUMMARY_FORMAL* sf = IPA_LNO_File->Formal(idx_formal);
  return sf->Machine_Type();
}    

//-----------------------------------------------------------------------
// NAME: IPA_LNO_Read_Formal
// FUNCTION: Create entries in the ARA_LOOP_INFO 'ali' for CALL 'wn_call'
//   for DEF and/or USE of the 'idx_formal'th entry of the FORMAL array 
//   in 'IPA_LNO_File', which is the 'formal_number'th parameter of 
//   'wn_call' (starting with 0). 
//-----------------------------------------------------------------------

static BOOL IPA_LNO_Read_Formal(IPA_LNO_READ_FILE* IPA_LNO_File,
				WN* wn_call, 
				INT formal_number, 
				INT idx_formal,
				ARA_LOOP_INFO* ali)
{ 
  IPA_LNO_SUMMARY_FORMAL* sf = IPA_LNO_File->Formal(idx_formal);
  char* callee_name = ST_name(WN_st(wn_call));
  if (formal_number < 0 || formal_number >= WN_kid_count(wn_call)) { 
    if (Get_Trace(TP_LNOPT2, TT_CALL_INFO)) 
      fprintf(stdout, 
	"SHAPE: Formal/Actual counts do not match for call 0x%p\n",
	wn_call);
    return FALSE; 
  } 
  WN* wn_parm = WN_kid(wn_call, formal_number);
  FmtAssert(WN_operator(wn_parm) == OPR_PARM, 
    ("IPA_LNO_Read_Formal: Expecting PARM node"));
  WN* wn_argument = WN_kid0(wn_parm);
  if (WN_operator(wn_argument) == OPR_INTCONST) 
    return TRUE;
  if (OPCODE_has_sym(WN_opcode(wn_argument)) 
      && ST_class(WN_st(wn_argument)) == CLASS_CONST)
    return TRUE; 
  if (!sf->Is_May_Kill() && !sf->Is_Use())
    return TRUE;
  DYN_ARRAY<WN*> wn_list(&LNO_local_pool);
  DYN_ARRAY<INT> int_list(&LNO_local_pool);
  INT64 const_value = 0;
  OPERATOR opr = WN_operator(wn_argument);
  if (sf->Is_Scalar()) { 
    if (opr == OPR_LDA || opr == OPR_LDID) { 
      FmtAssert(OPCODE_has_sym(WN_opcode(wn_argument)), 
	("IPA_LNO_Read_Formal: Expecting a simple argument")); 
      ST* st_lda = WN_st(wn_argument);
      if (ST_class(st_lda) == CLASS_VAR) { 
	if (Has_Optimizable_Node(wn_call, formal_number)) { 
          SYMBOL sym_formal(formal_number, 0, sf->Machine_Type());
	  if (sf->Is_May_Kill())
	    ali->SCALAR_MAY_DEF().Add_Scalar(wn_call, &sym_formal, 0);
	  if (sf->Is_Use())
	    ali->SCALAR_USE().Add_Scalar(wn_call, &sym_formal, 0);
	} else { 
          SYMBOL sym_lda(WN_st(wn_argument), WN_offset(wn_argument), 
	    sf->Machine_Type());
	  if (sf->Is_May_Kill())  
	    ali->SCALAR_MAY_DEF().Add_Scalar(wn_argument, &sym_lda, 0);
	  if (sf->Is_Use()) { 
	    WN* wn_single = Single_Definition_Temp(wn_argument);
	    if (wn_single != NULL && Scalar_Expr(wn_single))  
	      Add_Scalars_In_Expr(wn_single, &ali->SCALAR_USE());
	    ali->SCALAR_USE().Add_Scalar(wn_argument, &sym_lda, 0);
	  }
	} 
      } else { 
	DevWarn("IPA_LNO_Read_Formal: Expecting a VAR or CONST");
	return FALSE; 
      }
    } else if (opr == OPR_ARRAY) { 
      WN* wn_symbol = WN_array_base(wn_argument);
      if (!OPCODE_has_sym(WN_opcode(wn_symbol))) { 
        return FALSE; 
      } 
      SYMBOL sym_array(wn_symbol);
      ACCESS_ARRAY* aa
          = (ACCESS_ARRAY*) WN_MAP_Get(LNO_Info_Map, wn_argument);
      PROJECTED_REGION* pr_element = Projected_Region_From_Access_Array(aa, 
        &ARA_memory_pool, IPA_LNO_File);
      if (pr_element == NULL)
	return FALSE; 
      pr_element->Fill_Out();
      if (sf->Is_May_Kill())  
	Walk_Projected_Region(IPA_LNO_File, pr_element, wn_call, &sym_array, 
	  ali, TRUE);
      if (sf->Is_Use())  
	Walk_Projected_Region(IPA_LNO_File, pr_element, wn_call, &sym_array, 
	  ali, FALSE);
    } else if (Scalar_Expr(wn_argument) && Linear_Expr(wn_argument, &wn_list,
        &int_list, &const_value)) { 
      Add_Scalars_In_Expr(wn_argument, &ali->SCALAR_USE());
    } else { 
      FmtAssert(FALSE, 
	("IPA_LNO_Read_Formal: Expecting LDA, LDID, ARRAY, or S-LIN Exp"));
    }  
  } else { 
    if (opr == OPR_LDID || opr == OPR_LDA || opr == OPR_ARRAY) { 
      IPA_LNO_SUMMARY_FORMAL* sf = IPA_LNO_File->Formal(idx_formal);
      INT idx_decl = sf->Decl_Array_Section_Index();
      PROJECTED_REGION* pr_formal = Projected_Region(IPA_LNO_File, idx_decl,
	wn_call);
      if (pr_formal == NULL || pr_formal->Is_messy_region()) { 
	char* callee_name = ST_name(WN_st(wn_call));
	if (Get_Trace(TP_LNOPT2, TT_CALL_INFO)) {
	  fprintf(stdout, 
	    "SHAPE: %s ARG %d: Could not form callee decl projected region\n",
	    callee_name, formal_number);
        } 
        return FALSE; 
      } 
      WN* wn_symbol = (opr == OPR_ARRAY) 
	? WN_array_base(wn_argument) : wn_argument;
      SYMBOL sym_array(wn_symbol);
      PROJECTED_REGION* pr_callsite_region = NULL; 
      if (opr == OPR_ARRAY) { 
	ACCESS_ARRAY* aa 
	  = (ACCESS_ARRAY*) WN_MAP_Get(LNO_Info_Map, wn_argument);
	pr_callsite_region 
	  = Projected_Region_From_Access_Array(aa, &ARA_memory_pool,
	      IPA_LNO_File); 
      } 
      if (pr_callsite_region != NULL && pr_callsite_region->Is_messy_region()) {
	if (Get_Trace(TP_LNOPT2, TT_CALL_INFO)) {
	  fprintf(stdout, 
	    "SHAPE: %s ARG %d: Could not form callsite projected region\n",
	    callee_name, formal_number);
        } 
	return FALSE; 
      }
      PROJECTED_REGION* pr_caller_shape = NULL; 
      BOOL ok_shapes = Array_Shapes_Match_At_Formal(IPA_LNO_File, wn_call,
	formal_number, pr_formal);
      pr_caller_shape = Projected_Region_From_St(Current_Func_Node, 
        sym_array.St(), &ARA_memory_pool, TRUE, IPA_LNO_File);
      PROJECTED_REGION* pr_callee_shape = pr_formal;  
      INT idx_mod = sf->Mod_Array_Section_Index();
      if (idx_mod != -1) { 
	BOOL ok_mod = Attempt_Reshape(IPA_LNO_File, pr_caller_shape, 
	  pr_callee_shape, pr_callsite_region, idx_mod, ok_shapes, &sym_array,
	  wn_call, formal_number, ali, TRUE);
	if (!ok_mod) { 
          if (Get_Trace(TP_LNOPT2, TT_CALL_INFO)) 
            fprintf(stdout, "SHAPE: %s ARG %d: Reshape of MOD failed\n",
	      callee_name, formal_number);
	  return FALSE; 
	} 
      }
      INT idx_ref = sf->Ref_Array_Section_Index();
      if (idx_ref != -1) { 
	BOOL ok_ref = Attempt_Reshape(IPA_LNO_File, pr_caller_shape, 
	  pr_callee_shape, pr_callsite_region, idx_ref, ok_shapes, &sym_array,
	  wn_call, formal_number, ali, FALSE);
        if (!ok_ref) { 
          if (Get_Trace(TP_LNOPT2, TT_CALL_INFO)) 
            fprintf(stdout, "SHAPE: %s ARG %d: Reshape of REF failed\n",
	      callee_name, formal_number);
	  return FALSE;
	}  
      }
    } else { 
      DevWarn("IPA_LNO_Read_Formal: Expecting an LDID, LDA, or ARRAY");
      return FALSE; 
    } 
  } 
  return TRUE; 
}

//-----------------------------------------------------------------------
// NAME: IPA_LNO_Read_Global
// FUNCTION: Create entries in the ARA_LOOP_INFO 'ali' for CALL 'wn_call'
//   for DEF and/or USE of the 'idx_global'th entry of the GLOBAL array 
//   in 'IPA_LNO_File'. 
//-----------------------------------------------------------------------

static void IPA_LNO_Read_Global(IPA_LNO_READ_FILE* IPA_LNO_File,
                                WN* wn_call,
                                INT idx_global,
                                ARA_LOOP_INFO* ali)
{
  if (idx_global == -1)
    return; 

  IPA_LNO_SUMMARY_GLOBAL* sc = IPA_LNO_File->Global(idx_global);
  ST* st = ST_ptr(sc->St_Idx());
  SYMBOL sym_global(st, (WN_OFFSET) 0, TY_mtype(ST_type(st)));
  if (sc->Is_Scalar()) { 
    if (sc->Is_May_Kill()) { 
      ali->SCALAR_MAY_DEF().Add_Scalar(wn_call, &sym_global, 0); 
    } 
    if (sc->Is_Use()) {
      ali->SCALAR_USE().Add_Scalar(wn_call, &sym_global, 0); 
    } 
  } 
  else { 
    INT idx_mod = sc->Mod_Array_Section_Index();
    IPA_LNO_Read_Projected_Region(IPA_LNO_File, wn_call, idx_mod, 
      &sym_global, ali, TRUE);
    if (Get_Trace(TP_LNOPT2, TT_IPA_LNO_READ)) { 
      if (idx_mod != -1)
	fprintf(stdout, "  Common %s has array mod section\n", 
	  sym_global.Name());
    } 
    INT idx_ref = sc->Ref_Array_Section_Index();
    IPA_LNO_Read_Projected_Region(IPA_LNO_File, wn_call, idx_ref, 
      &sym_global, ali, FALSE);
    if (Get_Trace(TP_LNOPT2, TT_IPA_LNO_READ)) { 
      if (idx_ref != -1)
	fprintf(stdout, "  Common %s has array ref section\n", 
	  sym_global.Name());
    }
  } 
} 

//-----------------------------------------------------------------------
// NAME: Loop_Statement
// FUNCTION: Given that 'wn_def' is a definition of 'wn_use', return the
//   loop statement with respect to that definition.  
// NOTE: This is the outermost loop which carries this definition to the
//   use.  The answer many be conservative, in that a more outer loop 
//   may be returned than is strictly necessary. 
//-----------------------------------------------------------------------

static WN* Loop_Statement(WN* wn_def,
			  WN* wn_use)
{
  INT ld_def = Loop_Depth(wn_def);
  INT ld_use = Loop_Depth(wn_use);
  if (ld_def < ld_use)
    return NULL; 
  WN* wn_common_loop = LNO_Common_Loop(wn_def, wn_use);
  if (wn_common_loop == NULL)
     return NULL; 
  STACK<WN*> stk_enclose(&LNO_local_pool);
  for (WN* wn = wn_common_loop; wn != NULL; wn = LWN_Get_Parent(wn))  
    stk_enclose.Push(wn);
  WN* wn_loop = NULL; 
  for (INT i = 0; i < stk_enclose.Elements(); i++) { 
    WN* wn_stmt = stk_enclose.Top_nth(i);
    if (WN_operator(wn_stmt) == OPR_DO_LOOP) { 
      if (wn_loop == NULL)
	wn_loop = wn_stmt; 
    } else if (WN_operator(wn_stmt) == OPR_BLOCK) { 
      WN* wn_start = WN_first(wn_stmt); 
      WN* wn_stop = i == stk_enclose.Elements() - 1 
        ? wn_def : stk_enclose.Top_nth(i+1);
      for (WN* wn = wn_start; wn != NULL; wn = WN_next(wn)) { 
	if (OPCODE_has_sym(WN_opcode(wn_def))
	    && WN_operator(wn) == OPR_STID && SYMBOL(wn) == SYMBOL(wn_def))
	  wn_loop = NULL;
        if (wn == wn_stop)
	  break; 
      } 
    } 
  } 
  return wn_loop; 
} 

//-----------------------------------------------------------------------
// NAME: Update_Loop_Stmt
// FUNCTION: Update the loop stmt for 'wn_use'. 
// NOTE: The loop stmt is the outermost loop which carries a definition 
//   for 'wn_use'. 
//-----------------------------------------------------------------------

extern void Update_Loop_Stmt(WN* wn_use)
{ 
  WN* wn_loop = NULL; 
  DEF_LIST *def_list = Du_Mgr->Ud_Get_Def(wn_use);
  if (def_list == NULL) 
    return; 
  const DU_NODE* node = NULL;
  DEF_LIST_ITER iter(def_list);
  for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
    WN* wn_def = node->Wn();
    WN* wn_loop_stat = Loop_Statement(wn_def, wn_use); 
    if (wn_loop_stat != NULL && (wn_loop == NULL 
	|| Do_Loop_Depth(wn_loop_stat) < Do_Loop_Depth(wn_loop)))
      wn_loop = wn_loop_stat;
  } 
  def_list->Set_loop_stmt(wn_loop);
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_Clip_Call_Def_Arcs
// FUNCTION: If the call node 'wn_call' has a call info, use the informa-
//   tion in the call info to remove extraneous DEF/USE arcs from the call
//   to other nodes.
//-----------------------------------------------------------------------

static void IPA_LNO_Clip_Call_Def_Arcs(WN* wn_call)
{
  if (!Has_Call_Info(wn_call))
    return;
  DEF_LIST* def_list = Du_Mgr->Ud_Get_Def(wn_call);
  if (def_list == NULL)
    return;
  if (def_list->Incomplete()) {
    if (Get_Trace(TP_LNOPT2, TT_CALL_INFO)) {
      fprintf(stdout, 
	"CLIP: Cannot clip DEF arcs for call %s: Incomplete list\n",
        WB_Whirl_Symbol(wn_call));
    } 
    return;
  }
  CALL_INFO* ci_call = Get_Call_Info(wn_call);
  ci_call->Evaluate();
  ARA_LOOP_INFO* ali_call = ci_call->Call_Ara_Info();
  DEF_LIST_ITER iter(def_list);
  const DU_NODE* node = NULL;
  const DU_NODE* nnode = NULL;
  for (node = iter.First(); !iter.Is_Empty(); node = nnode) {
    nnode = iter.Next();
    WN* wn_def = node->Wn();
    USE_LIST* use_list = Du_Mgr->Du_Get_Use(wn_def);
    if (use_list == NULL)
      continue;
    if (use_list->Incomplete()) {
      if (Get_Trace(TP_LNOPT2, TT_CALL_INFO)) {
        fprintf(stdout, 
	  "CLIP: Cannot clip DEF arcs for use %s: Incomplete list\n",
          WB_Whirl_Symbol(wn_def));
      } 
      continue;
    }
    if (WN_operator(wn_def) == OPR_STID) {
      SYMBOL sym_stid(wn_def);
      INT i;
      for (i = 0; i < ali_call->SCALAR_USE().Elements(); i++) {
        SYMBOL sym = ali_call->SCALAR_USE().Bottom_nth(i)->_scalar;
        if (sym == sym_stid)
          break;
      }
      if (i == ali_call->SCALAR_USE().Elements()) {
        if (Get_Trace(TP_LNOPT2, TT_CALL_INFO))
	  fprintf(stdout, "DU CLIP: From %s to %s\n", 
	    WB_Whirl_Symbol(wn_def), WB_Whirl_Symbol(wn_call));
        Du_Mgr->Delete_Def_Use(wn_def, wn_call);
        Update_Loop_Stmt(wn_call);
      } 
    } else if (WN_operator(wn_def) == OPR_CALL) {
      if (Has_Call_Info(wn_def)) {
        CALL_INFO* ci_def = Get_Call_Info(wn_def);
        ARA_LOOP_INFO* ali_def = ci_def->Call_Ara_Info();
        BOOL fm = FALSE;
        for (INT j = 0; !fm && j < ali_def->SCALAR_MAY_DEF().Elements(); j++) {
          SYMBOL sym_def = ali_def->SCALAR_MAY_DEF().Bottom_nth(j)->_scalar;
          for (INT i = 0; !fm && i < ali_call->SCALAR_USE().Elements(); i++) {
            SYMBOL sym_use = ali_call->SCALAR_USE().Bottom_nth(i)->_scalar;
            if (sym_def == sym_use) {
              fm = TRUE;
              break;
            }
          }
        }
        if (!fm) {
	  if (Get_Trace(TP_LNOPT2, TT_CALL_INFO))
	    fprintf(stdout, "DU CLIP: From %s to %s\n", 
	      WB_Whirl_Symbol(wn_def), WB_Whirl_Symbol(wn_call));
          Du_Mgr->Delete_Def_Use(wn_def, wn_call);
          Update_Loop_Stmt(wn_call);
	}
      }
    }
  }
  ci_call->Unevaluate();
}

//-----------------------------------------------------------------------
// NAME: IPA_LNO_Clip_Call_Use_Arcs
// FUNCTION: If the call node 'wn_call' has a call info, use the informa-
//   tion in the call info to remove extraneous DEF/USE arcs from other
//   nodes to the call.
//-----------------------------------------------------------------------

static void IPA_LNO_Clip_Call_Use_Arcs(WN* wn_call)
{
  if (!Has_Call_Info(wn_call))
    return;
  USE_LIST* use_list = Du_Mgr->Du_Get_Use(wn_call);
  if (use_list == NULL)
    return;
  if (use_list->Incomplete()) {
    if (Get_Trace(TP_LNOPT2, TT_CALL_INFO)) {
      fprintf(stdout, 
	"CLIP: Cannot clip USE arcs for call %s: Incomplete list\n",
        WB_Whirl_Symbol(wn_call));
    } 
    return;
  }
  CALL_INFO* ci_call = Get_Call_Info(wn_call);
  ci_call->Evaluate();
  ARA_LOOP_INFO* ali_call = ci_call->Call_Ara_Info();
  USE_LIST_ITER iter(use_list);
  const DU_NODE* node = NULL;
  const DU_NODE* nnode = NULL;
  for (node = iter.First(); !iter.Is_Empty(); node = nnode) {
    nnode = iter.Next();
    WN* wn_use = node->Wn();
    if (OPERATOR_has_sym(WN_operator(wn_use)) &&
        ST_class(WN_st(wn_use)) == CLASS_PREG &&
        Preg_Is_Dedicated(WN_offset(wn_use))) {
      continue;
    }
    DEF_LIST* def_list = Du_Mgr->Ud_Get_Def(wn_use);
    if (def_list == NULL)
      continue;
    if (def_list->Incomplete()) {
      if (Get_Trace(TP_LNOPT2, TT_CALL_INFO)) { 
        fprintf(stdout, 
	  "CLIP: Cannot clip USE arcs for use %s: Incomplete list\n",
          WB_Whirl_Symbol(wn_use));
      } 
      continue;
    }
    if (WN_operator(wn_use) == OPR_LDID) {
      SYMBOL sym_ldid(wn_use);
      INT i;
      for (i = 0; i < ali_call->SCALAR_MAY_DEF().Elements(); i++) {
        SYMBOL sym = ali_call->SCALAR_MAY_DEF().Bottom_nth(i)->_scalar;
        if (sym == sym_ldid)
          break;
      }
      if (i == ali_call->SCALAR_MAY_DEF().Elements()) {
        if (Get_Trace(TP_LNOPT2, TT_CALL_INFO))
	  fprintf(stdout, "DU CLIP: From %s to %s\n", 
	    WB_Whirl_Symbol(wn_call), WB_Whirl_Symbol(wn_use));
        Du_Mgr->Delete_Def_Use(wn_call, wn_use);
        Update_Loop_Stmt(wn_use);
      }
    } else if (WN_operator(wn_use) == OPR_CALL) {
      if (Has_Call_Info(wn_use)) {
        CALL_INFO* ci_use = Get_Call_Info(wn_use);
        ARA_LOOP_INFO* ali_use = ci_use->Call_Ara_Info();
        BOOL fm = FALSE;
        for (INT j = 0; !fm && j < ali_use->SCALAR_USE().Elements(); j++) {
          SYMBOL sym_use = ali_use->SCALAR_USE().Bottom_nth(j)->_scalar;
          for (INT i = 0; !fm && i < ali_call->SCALAR_MAY_DEF().Elements();
              i++) {
            SYMBOL sym_def = ali_call->SCALAR_MAY_DEF().Bottom_nth(i)->_scalar;
            if (sym_def == sym_use) {
              fm = TRUE;
              break;
            }
          }
        }
        if (!fm) {
	  if (Get_Trace(TP_LNOPT2, TT_CALL_INFO))
	    fprintf(stdout, "DU CLIP: From %s to %s\n", 
	      WB_Whirl_Symbol(wn_call), WB_Whirl_Symbol(wn_use));
          Du_Mgr->Delete_Def_Use(wn_call, wn_use);
          Update_Loop_Stmt(wn_use);
	}
      }
    } else if (WN_operator(wn_use) == OPR_PARM
	&& WN_operator(WN_kid0(wn_use)) == OPR_LDA) { 
      WN* wn_lda = WN_kid0(wn_use);
      SYMBOL sym_lda(wn_lda);
      INT i;
      for (i = 0; i < ali_call->SCALAR_MAY_DEF().Elements(); i++) {
        SYMBOL sym = ali_call->SCALAR_MAY_DEF().Bottom_nth(i)->_scalar;
        if (sym == sym_lda)
          break;
      }
      if (i == ali_call->SCALAR_MAY_DEF().Elements()) {
        if (Get_Trace(TP_LNOPT2, TT_CALL_INFO))
	  fprintf(stdout, "DU CLIP: From %s to %s\n", 
	    WB_Whirl_Symbol(wn_call), WB_Whirl_Symbol(wn_lda));
        Du_Mgr->Delete_Def_Use(wn_call, wn_use);
        Update_Loop_Stmt(wn_use);
      }
    } 
  }
  ci_call->Unevaluate();
}

//-----------------------------------------------------------------------
// NAME: IPA_LNO_Clip_Call_DU_Arcs
// FUNCTION: If the call node 'wn_call' has a call info, use that informa-
//   tion to remove extraneous DEF/USE arcs to and from the call.
//-----------------------------------------------------------------------

static void IPA_LNO_Clip_Call_DU_Arcs(WN* wn_call)
{
  IPA_LNO_Clip_Call_Def_Arcs(wn_call);
  IPA_LNO_Clip_Call_Use_Arcs(wn_call);
}

//-----------------------------------------------------------------------
// NAME: Is_Kind_Array
// FUNCTION: Return TRUE if the 'st_idx' is of type KIND_ARRAY or of type
//   KIND_ARRAY*. 
//-----------------------------------------------------------------------

static BOOL Is_Kind_Array(const ST* st)
{
  TY_IDX ty_idx = ST_type(st);
  if (TY_kind(ty_idx) == KIND_POINTER)
    ty_idx = TY_pointed(ty_idx);
  return TY_kind(ty_idx) == KIND_ARRAY;
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_Annotate_Scalar_Call
// FUNCTION: Annotate the call 'wn_call' with information about scalars.
//-----------------------------------------------------------------------

static BOOL IPA_LNO_Annotate_Scalar_Call(IPA_LNO_READ_FILE* IPA_LNO_File,
				         WN* wn_call,
					 STACK<ST_IDX>* st_pstatic)
{ 
  char* callee_name = ST_name(WN_st(wn_call));
  INT idx_procedure = IPA_LNO_Procedure_Index(wn_call, IPA_LNO_File);  
  if (idx_procedure == -1) {
    if (Get_Trace(TP_LNOPT2, TT_CALL_INFO))
      fprintf(stdout, 
	"SCALAR: %s: Could not find procedure index\n", callee_name);
    return FALSE;
  } 
  if (Get_Trace(TP_LNOPT2, TT_IPA_LNO_READ))  
    fprintf(stdout, "Annotating call site %s\n", ST_name(WN_st(wn_call)));
  // Give up if we do not have adequate information about formals
  IPA_LNO_SUMMARY_PROCEDURE* sp = IPA_LNO_File->Procedure(idx_procedure);
  if (sp->Has_Incomplete_Array_Info()) {
    if (Get_Trace(TP_LNOPT2, TT_CALL_INFO))
      fprintf(stdout, 
	"SCALAR: %s: Has incomplete array info\n", callee_name);
    return FALSE; 
  } 
  INT idx_formal_base = sp->Formal_Index(); 
  INT i;
  for (i = 0; i < sp->Formal_Count(); i++) {
    INT idx_formal = idx_formal_base + i;
    IPA_LNO_SUMMARY_FORMAL* sf = IPA_LNO_File->Formal(idx_formal);
    if (sf->Is_Unknown()) { 
      if (Get_Trace(TP_LNOPT2, TT_CALL_INFO))  
	fprintf(stdout, 
          "SCALAR: %s ARG %d: Formal has unknown type\n", callee_name, i);
      return FALSE;
    } 
  } 
  INT idx_global_base = sp->Global_Index();
  ARA_LOOP_INFO* ali = CXX_NEW(ARA_LOOP_INFO(wn_call, NULL, TRUE), 
    &ARA_memory_pool);
  for (i = 0; i < sp->Formal_Count(); i++) {
    INT idx_formal = idx_formal_base + i;
    if (Shape_Mismatch_At_Formal(IPA_LNO_File, wn_call, i, idx_formal)) { 
      if (Get_Trace(TP_LNOPT2, TT_CALL_INFO))  
	fprintf(stdout, 
          "SCALAR: %s ARG %d: Formal has shape mismatch\n", callee_name, i);
      return FALSE;
    }
  }
  for (i = 0; i < sp->Global_Count(); i++) {
  INT idx_global_base = sp->Global_Index();
    INT idx_global = idx_global_base + i;
    if (Shape_Mismatch_At_Common(IPA_LNO_File, idx_global)) {
      if (Get_Trace(TP_LNOPT2, TT_CALL_INFO)) 
	fprintf(stdout, 
	  "SCALAR: %s GLOBAL[%i]: Global has shape mismatch\n", callee_name, i);
      return FALSE;
    } 
  }
  for (i = 0; i < sp->Formal_Count(); i++) {
    INT idx_formal = idx_formal_base + i;
    IPA_LNO_SUMMARY_FORMAL* sf = IPA_LNO_File->Formal(idx_formal);
    if (sf->Is_Scalar()) {
      BOOL ok = IPA_LNO_Read_Formal(IPA_LNO_File, wn_call, i, idx_formal, ali);
      if (!ok) {
        if (Get_Trace(TP_LNOPT2, TT_CALL_INFO)) 
	  fprintf(stdout, 
	    "SCALAR: %s FORMAL[%i]: Can\'t read formal\n", callee_name, i);
        return FALSE;
      } 
    } 
  } 
  for (i = 0; i < sp->Global_Count(); i++) { 
    INT idx_global = idx_global_base + i;
    IPA_LNO_SUMMARY_GLOBAL* sc = IPA_LNO_File->Global(idx_global);
    if (sc->Is_Scalar())  
      IPA_LNO_Read_Global(IPA_LNO_File, wn_call, idx_global, ali);
  } 
  for (i = 0; i < st_pstatic->Elements(); i++) {
    ST* st = ST_ptr(st_pstatic->Bottom_nth(i)); 
    if (!Is_Kind_Array(st)) { 
      TYPE_ID tid = TY_kind(ST_type(st)) == KIND_POINTER ? 
        TY_mtype(TY_pointed(ST_type(st))) : TY_mtype(ST_type(st));
      SYMBOL sym_global(st, (WN_OFFSET) 0, tid);
      if (!ST_is_const_var(st))
        ali->SCALAR_MAY_DEF().Add_Scalar(wn_call, &sym_global, 0);
      ali->SCALAR_USE().Add_Scalar(wn_call, &sym_global, 0);
    } 
  } 
  CALL_INFO* call_info = CXX_NEW(CALL_INFO(ali, wn_call, TRUE, 
    &ARA_memory_pool), &ARA_memory_pool);
  Set_Call_Info(wn_call, call_info);
  if (!call_info->Has_Formal_Parameter())
    call_info->Reset_Needs_Evaluation();
  return TRUE; 
}

//-----------------------------------------------------------------------
// NAME: Treat_As_Pure
// FUNCTION: Return TRUE if 'wn_call' can be treated as pure.  Return 
//   FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Treat_As_Pure(WN* wn_call)
{
  if (LNO_Pure_Level == 0)
    return FALSE; 
  if (LNO_Pure_Level == 1)
    return WN_Call_Pure(wn_call);
  if (LNO_Pure_Level == 2)
    return WN_Call_Pure(wn_call) || WN_Call_No_Side_Effect(wn_call);
  return FALSE; 
} 

//-----------------------------------------------------------------------
// NAME: NSE_Annotate_Scalar_Call
// FUNCTION: Annotate the call 'wn_call' with information about scalars, 
//   given that it has no side effects. 
//-----------------------------------------------------------------------

static BOOL NSE_Annotate_Scalar_Call(WN* wn_call)
{
  if (!Treat_As_Pure(wn_call))
    return FALSE; 
  ARA_LOOP_INFO* ali = CXX_NEW(ARA_LOOP_INFO(wn_call, NULL, TRUE),
    &ARA_memory_pool);
  for (INT i = 0; i < WN_kid_count(wn_call); i++) { 
    WN* wn_parm = WN_kid(wn_call, i);
    if (WN_operator(wn_parm) != OPR_PARM)
      return FALSE; 
    WN* wn_arg = WN_kid0(wn_parm);
    switch (WN_operator(wn_arg)) { 
    case OPR_LDID: 
    case OPR_LDA: {  
      ST* st = WN_st(wn_arg);
      if (!Is_Kind_Array(st)) { 
	SYMBOL sym_scalar(st, (WN_OFFSET) 0, TY_mtype(ST_type(st)));
	WN* wn_single = Single_Definition_Temp(wn_arg); 
	if (wn_single != NULL && Scalar_Expr(wn_single))
	  Add_Scalars_In_Expr(wn_single, &ali->SCALAR_USE());
	ali->SCALAR_USE().Add_Scalar(wn_call, &sym_scalar, 0);
      } 
      } 
      break; 
    case OPR_ILOAD: 
      // This is the array case, handle it in NSE_Annotate_Array_Call()
      break; 
    default: 
      if (Scalar_Expr(wn_arg))
	Add_Scalars_In_Expr(wn_arg, &ali->SCALAR_USE());
      else 
        return FALSE; 
    } 
  } 
  CALL_INFO* call_info = CXX_NEW(CALL_INFO(ali, wn_call, FALSE, 
    &LNO_default_pool), &LNO_default_pool);
  Set_Call_Info(wn_call, call_info);
  if (!call_info->Has_Formal_Parameter())
    call_info->Reset_Needs_Evaluation();
  return TRUE; 
}  

//-----------------------------------------------------------------------
// NAME: IPA_LNO_Annotate_Execution_Cost
// FUNCTION: Annotate the call 'wn_call' with information about arrays.
//-----------------------------------------------------------------------

static void IPA_LNO_Annotate_Execution_Cost(IPA_LNO_READ_FILE* IPA_LNO_File,
			                    WN* wn_call, 
					    MEM_POOL* mem_pool)
{ 
  CALL_INFO* call_info = (CALL_INFO *) WN_MAP_Get(LNO_Info_Map, wn_call);
  DYN_ARRAY<SUMMARY_VALUE>* sv
    = CXX_NEW(DYN_ARRAY<SUMMARY_VALUE>(mem_pool), mem_pool);
  DYN_ARRAY<SUMMARY_EXPR>* sx
    = CXX_NEW(DYN_ARRAY<SUMMARY_EXPR>(mem_pool), mem_pool);
  call_info->Set_Value(sv);
  call_info->Set_Expr(sx);
  INT idx_procedure = IPA_LNO_Procedure_Index(wn_call, IPA_LNO_File);
  IPA_LNO_SUMMARY_PROCEDURE* sp = IPA_LNO_File->Procedure(idx_procedure);
  INT value_index = sp->Value_Index();
  INT value_count = sp->Value_Count();
  INT formal_index = sp->Formal_Index();
  INT global_index = sp->Global_Index();
  INT i;
  for (i = 0; i < value_count; i++) { 
    SUMMARY_VALUE* svv = IPA_LNO_File->Value(value_index + i);
    sv->AddElement(*svv);
  } 
  INT expr_index = sp->Expr_Index();
  INT expr_count = sp->Expr_Count(); 
  for (i = 0; i < expr_count; i++) { 
    SUMMARY_EXPR* sxx = IPA_LNO_File->Expr(expr_index + i);
    sx->AddElement(*sxx);
  } 
  IPL_EX_Add_Value_Offsets(sv, -formal_index, -global_index);
  IPL_EX_Add_Expr_Offsets(sx, -value_index, -expr_index);
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_Annotate_Array_Call
// FUNCTION: Annotate the call 'wn_call' with information about arrays.
//-----------------------------------------------------------------------

static BOOL IPA_LNO_Annotate_Array_Call(IPA_LNO_READ_FILE* IPA_LNO_File,
				        WN* wn_call,
					STACK<ST_IDX>* st_pstatic)
{
  CALL_INFO* call_info = (CALL_INFO *) WN_MAP_Get(LNO_Info_Map, wn_call);
  if (call_info == NULL) 
    return FALSE;
  ARA_LOOP_INFO* ali = call_info->Call_Ara_Info();
  INT idx_procedure = IPA_LNO_Procedure_Index(wn_call, IPA_LNO_File);  
  IPA_LNO_SUMMARY_PROCEDURE* sp = IPA_LNO_File->Procedure(idx_procedure);
  INT idx_formal_base = sp->Formal_Index(); 
  INT i;
  for (i = 0; i < sp->Formal_Count(); i++) {
    INT idx_formal = idx_formal_base + i;
    IPA_LNO_SUMMARY_FORMAL* sf = IPA_LNO_File->Formal(idx_formal);
    if (!sf->Is_Scalar()) {
      BOOL ok = IPA_LNO_Read_Formal(IPA_LNO_File, wn_call, i, idx_formal, ali);
      if (!ok) {
	ali->Remove_Array_Info();
        return FALSE;
      } 
    } 
  } 
  INT idx_global_base = sp->Global_Index();
  for (i = 0; i < sp->Global_Count(); i++) { 
    INT idx_global = idx_global_base + i;
    IPA_LNO_SUMMARY_GLOBAL* sc = IPA_LNO_File->Global(idx_global);
    if (!sc->Is_Scalar())  
      IPA_LNO_Read_Global(IPA_LNO_File, wn_call, idx_global, ali);
  }
  for (i = 0; i < st_pstatic->Elements(); i++) {
    ST* st = ST_ptr(st_pstatic->Bottom_nth(i)); 
    if (Is_Kind_Array(st)) {
      TYPE_ID tid_base = TY_kind(ST_type(st)) == KIND_POINTER ?
        TY_pointed(ST_type(st)) : ST_type(st);
      SYMBOL sym_array(st, (WN_OFFSET) 0, TY_mtype(tid_base));
      PROJECTED_REGION* pr_static_ref = CXX_NEW(PROJECTED_REGION(MESSY_REGION,
	0, TY_AR_ndims(tid_base), &ARA_memory_pool), &ARA_memory_pool);
      Walk_Projected_Region(IPA_LNO_File, pr_static_ref, wn_call, &sym_array, 
	ali, FALSE);
      if (!ST_is_const_var(st)) { 
	PROJECTED_REGION* pr_static_mod = CXX_NEW(PROJECTED_REGION(MESSY_REGION,
	  0, TY_AR_ndims(ST_type(st)), &ARA_memory_pool), &ARA_memory_pool);
	Walk_Projected_Region(IPA_LNO_File, pr_static_mod, wn_call, &sym_array, 
	  ali, TRUE);
      } 
    } 
  } 
  IPA_LNO_Annotate_Execution_Cost(IPA_LNO_File, wn_call, &ARA_memory_pool); 
  if (Get_Trace(TP_LNOPT2, TT_CALL_INFO)) {
    CALL_INFO* call_info = (CALL_INFO *) WN_MAP_Get(LNO_Info_Map, wn_call);
    call_info->Print(stdout);
  }
  if (LNO_Tlog || Get_Trace(TP_PTRACE1, TP_PTRACE1_CALLINFO)) {
    CALL_INFO* call_info = (CALL_INFO *) WN_MAP_Get(LNO_Info_Map, wn_call);
    call_info->Tlog_Print();
  }
  return TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: NSE_Annotate_Array_Call
// FUNCTION: Annotate the call 'wn_call' with information about arrays,
//   given that it has no side effects.
//-----------------------------------------------------------------------

static BOOL NSE_Annotate_Array_Call(WN* wn_call)
{ 
  if (!Treat_As_Pure(wn_call))
    return FALSE; 
  CALL_INFO* call_info = (CALL_INFO *) WN_MAP_Get(LNO_Info_Map, wn_call);
  if (call_info == NULL)
    return FALSE; 
  ARA_LOOP_INFO* ali = call_info->Call_Ara_Info();
  for (INT i = 0; i < WN_kid_count(wn_call); i++) { 
    WN* wn_parm = WN_kid(wn_call, i);
    FmtAssert(WN_operator(wn_parm) == OPR_PARM, 
      ("NSE_Annotate_Array_Call: Expecting PARM node"));
    WN* wn_arg = WN_kid0(wn_parm);
    switch (WN_operator(wn_arg)) { 
    case OPR_LDID: 
    case OPR_LDA: { 
      ST* st = WN_st(wn_arg);
      if (Is_Kind_Array(st)) {  
        TYPE_ID tid_base = TY_kind(ST_type(st)) == KIND_POINTER ?
           TY_pointed(ST_type(st)) : ST_type(st);
	SYMBOL sym_array(st, (WN_OFFSET) 0, TY_mtype(tid_base));
	PROJECTED_REGION* pr_array = CXX_NEW(PROJECTED_REGION(MESSY_REGION,
	  0, TY_AR_ndims(tid_base), &ARA_memory_pool), &ARA_memory_pool);
	Walk_Projected_Region(NULL, pr_array, wn_call, &sym_array, ali, FALSE);
      } 
      } 
      break;
    case OPR_ILOAD: { 
      WN* wn_array = WN_kid0(wn_arg);
      if (WN_operator(wn_array) != OPR_ARRAY) { 
	return FALSE; 
      }  
      WN* wn_sym = WN_array_base(wn_array); 
      if (!OPCODE_has_sym(WN_opcode(wn_sym))) { 
        return FALSE; 
      }  
      ST* st = WN_st(wn_sym);
      SYMBOL sym_array(st, (WN_OFFSET) 0, TY_mtype(ST_type(st)));
      ACCESS_ARRAY* aa = (ACCESS_ARRAY*) WN_MAP_Get(LNO_Info_Map, wn_array);
      PROJECTED_REGION* pr_array = Projected_Region_From_Access_Array(aa, 
	&ARA_memory_pool, NULL);
      if (pr_array == NULL) { 
	return FALSE; 
      } 
      pr_array->Fill_Out();
      Walk_Projected_Region(NULL, pr_array, wn_call, &sym_array, ali, FALSE);
      } 
      break;  
    } 
  } 
  if (Get_Trace(TP_LNOPT2, TT_CALL_INFO)) {
    CALL_INFO* call_info = (CALL_INFO *) WN_MAP_Get(LNO_Info_Map, wn_call);
    call_info->Print(stdout);
  }
  if (LNO_Tlog || Get_Trace(TP_PTRACE1, TP_PTRACE1_CALLINFO)) {
    CALL_INFO* call_info = (CALL_INFO *) WN_MAP_Get(LNO_Info_Map, wn_call);
    call_info->Tlog_Print();
  }
  return TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: LNO_Annotate_Scalar_Call
// FUNCTION: Annotate the call 'wn_call' with information about scalars.
//-----------------------------------------------------------------------

static BOOL LNO_Annotate_Scalar_Call(IPA_LNO_READ_FILE* IPA_LNO_File,
                                     WN* wn_call,
                                     STACK<ST_IDX>* st_pstatic)
{
  if (IPA_LNO_File == NULL)  
    return NSE_Annotate_Scalar_Call(wn_call);
  BOOL ok1 = IPA_LNO_Annotate_Scalar_Call(IPA_LNO_File, wn_call, st_pstatic);
  if (ok1) 
    return TRUE; 
  BOOL ok2 = NSE_Annotate_Scalar_Call(wn_call);
  if (ok2) 
    return TRUE; 
  return FALSE; 
} 

//-----------------------------------------------------------------------
// NAME: LNO_Map_Calls_Scalar_Traverse
// FUNCTION: Traverse the tree rooted at 'wn_tree', annotating the call
//   nodes with information about scalars in the file 'IPA_LNO_File'.
//-----------------------------------------------------------------------

static void LNO_Map_Calls_Scalar_Traverse(WN* wn_tree,
				          IPA_LNO_READ_FILE* IPA_LNO_File,
					  STACK<ST_IDX>* st_pstatic)
{
  if (WN_operator(wn_tree) == OPR_CALL) 
    LNO_Annotate_Scalar_Call(IPA_LNO_File, wn_tree, st_pstatic);

  if (WN_opcode(wn_tree) == OPC_BLOCK) { 
    for (WN * wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      LNO_Map_Calls_Scalar_Traverse(wn, IPA_LNO_File, st_pstatic); 
  } else {
    for (INT i = 0; i < WN_kid_count(wn_tree); i++) 
      LNO_Map_Calls_Scalar_Traverse(WN_kid(wn_tree, i), IPA_LNO_File,
	st_pstatic);
  }
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_Map_Calls_Clip_DU_Traverse
// FUNCTION: Traverse the tree rooted at 'wn_tree', clipping DU arcs which 
//   are made extraneous due to CALL_INFO information on calls. 
//-----------------------------------------------------------------------

static void IPA_LNO_Map_Calls_Clip_DU_Traverse(WN* wn_tree,
				               IPA_LNO_READ_FILE* IPA_LNO_File)
{
  if (WN_operator(wn_tree) == OPR_CALL) 
    IPA_LNO_Clip_Call_DU_Arcs(wn_tree);

  if (WN_opcode(wn_tree) == OPC_BLOCK) { 
    for (WN * wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      IPA_LNO_Map_Calls_Clip_DU_Traverse(wn, IPA_LNO_File); 
  } else {
    for (INT i = 0; i < WN_kid_count(wn_tree); i++) 
      IPA_LNO_Map_Calls_Clip_DU_Traverse(WN_kid(wn_tree, i), IPA_LNO_File);
  }
} 

//-----------------------------------------------------------------------
// NAME: LNO_Annotate_Array_Call
// FUNCTION: Annotate the call 'wn_call' with information about arrays.
//-----------------------------------------------------------------------

static void LNO_Annotate_Array_Call(IPA_LNO_READ_FILE* IPA_LNO_File,
				    WN* wn_call,
				    STACK<ST_IDX>* st_pstatic)
{
  CALL_INFO* call_info = (CALL_INFO *) WN_MAP_Get(LNO_Info_Map, wn_call);
  if (call_info == NULL)
    return; 
  if (call_info->Has_IPA_Summaries()) { 
    BOOL ok1 = IPA_LNO_Annotate_Array_Call(IPA_LNO_File, wn_call, st_pstatic);
    if (!ok1) { 
       BOOL ok2 = NSE_Annotate_Array_Call(wn_call);
       if (!ok2)  
         Set_Call_Info(wn_call, NULL); 
    } 
  } else { 
    BOOL ok3 = NSE_Annotate_Array_Call(wn_call);
    if (!ok3) 
      Set_Call_Info(wn_call, NULL); 
  } 
} 

//-----------------------------------------------------------------------
// NAME: LNO_Map_Calls_Arrays_Traverse
// FUNCTION: Traverse the tree rooted at 'wn_tree', annotating the call
//   nodes with information about arrays in the file 'IPA_LNO_File'.
//-----------------------------------------------------------------------

static void LNO_Map_Calls_Array_Traverse(WN* wn_tree,
				         IPA_LNO_READ_FILE* IPA_LNO_File,
					 STACK<ST_IDX>* st_pstatic)
{
  if (WN_operator(wn_tree) == OPR_CALL) 
    LNO_Annotate_Array_Call(IPA_LNO_File, wn_tree, st_pstatic);

  if (WN_opcode(wn_tree) == OPC_BLOCK) { 
    for (WN * wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      LNO_Map_Calls_Array_Traverse(wn, IPA_LNO_File, st_pstatic); 
  } else {
    for (INT i = 0; i < WN_kid_count(wn_tree); i++) 
      LNO_Map_Calls_Array_Traverse(WN_kid(wn_tree, i), IPA_LNO_File,
	st_pstatic);
  }
} 

//-----------------------------------------------------------------------
// NAME: IPA_Reassign_Unsummarized_Calls
// FUNCTION: Reassign the 'Has_Unsummarized_Calls' but in the DO_LOOP_INFO
//   of loop 'wn_loop' if all of the calls in that loop have been summar-
//   ized (have CALL_INFOs created for them).
//-----------------------------------------------------------------------

static void IPA_LNO_Reassign_Unsummarized_Calls(WN* wn_loop)
{
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
  dli->Has_Unsummarized_Calls = FALSE;
  dli->Has_Unsummarized_Call_Cost = FALSE;
  LWN_ITER* itr = LWN_WALK_TreeIter(wn_loop);
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn;
    if (OPCODE_is_call(WN_opcode(wn))) { 
      if (!Has_Call_Info(wn))
	dli->Has_Unsummarized_Calls = TRUE;
      if (!Has_Execution_Cost(wn))
	dli->Has_Unsummarized_Call_Cost = TRUE; 
      if (dli->Has_Unsummarized_Calls && dli->Has_Unsummarized_Call_Cost)
	return; 
    }
  }
}

//-----------------------------------------------------------------------
// NAME: IPA_Reassign_Unsummarized_Calls_Traverse
// FUNCTION: Traverse the tree of nodes rooted at 'wn_tree', reassigning
//   the 'Has_Unsummarized_Calls' bit in the DO_LOOP_INFO of each loop
//   for which all of its calls have been summarized.
//-----------------------------------------------------------------------

static void IPA_LNO_Reassign_Unsummarized_Calls_Traverse(WN* wn_tree)
{
  if (WN_opcode(wn_tree) == OPC_DO_LOOP)
    IPA_LNO_Reassign_Unsummarized_Calls(wn_tree);

  if (WN_opcode(wn_tree) == OPC_BLOCK) {
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      IPA_LNO_Reassign_Unsummarized_Calls_Traverse(wn);
  } else {
    for (INT i = 0; i < WN_kid_count(wn_tree); i++)
      IPA_LNO_Reassign_Unsummarized_Calls_Traverse(WN_kid(wn_tree, i));
  }
}

//-----------------------------------------------------------------------
// NAME: IPA_LNO_Make_Pstatic_List
// FUNCTION: Add an entry to 'st_pstatic' for each SCLASS_PSTATIC symbol
//   in the CURRENT_SYMTAB.
//-----------------------------------------------------------------------

static void IPA_LNO_Make_Pstatic_List(STACK<ST_IDX>* st_pstatic)
{ 
  INT i; 
  ST* st; 
  FOREACH_SYMBOL(CURRENT_SYMTAB, st, i) 
    if (ST_sclass(st) == SCLASS_PSTATIC)  
      st_pstatic->Push(ST_st_idx(st));
} 

//-----------------------------------------------------------------------
// NAME: IPA_LNO_Map_Calls
// FUNCTION: Create CALL_INFOs for each of the calls in 'func_nd', if
//   possible, and use the information in each of these CALL_INFOs to
//   remove extraneuous DU arcs to and from the calls.  Information to
//   create the calls' summary information is given in 'IPA_LNO_File'.
//-----------------------------------------------------------------------

extern void IPA_LNO_Map_Calls(WN* func_nd, 
			      IPA_LNO_READ_FILE* IPA_LNO_File)
{ 
  STACK<ST_IDX> st_pstatic(&LNO_local_pool);
  if (IPA_LNO_File != NULL)
    IPA_LNO_Read_Sections(IPA_LNO_File);
  if (Get_Trace(TP_LNOPT2, TT_CALL_INFO))
    fprintf(stdout, "=== BEGIN CALL INFO TRACE: %s ===\n", 
      WB_Whirl_Symbol(func_nd));
  IPA_LNO_Make_Pstatic_List(&st_pstatic);
  LNO_Map_Calls_Scalar_Traverse(func_nd, IPA_LNO_File, &st_pstatic);
  IPA_LNO_Map_Calls_Clip_DU_Traverse(func_nd, IPA_LNO_File);
  LNO_Map_Calls_Array_Traverse(func_nd, IPA_LNO_File, &st_pstatic);
  if (Get_Trace(TP_LNOPT2, TT_CALL_INFO))
    fprintf(stdout, "==== END CALL INFO TRACE: %s ====\n", 
      WB_Whirl_Symbol(func_nd));
  IPA_LNO_Reassign_Unsummarized_Calls_Traverse(func_nd);
}

