/*
 * Copyright (C) 2007, 2008. PathScale, LLC. All Rights Reserved.
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
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#include <sys/elf_whirl.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <sys/types.h>
#include "defs.h"
#include "mtypes.h"
#include "access_vector.h"
#include "ipl_lno_util.h"
#include "ipl_summary.h"
#include <alloca.h>
#include "ipa_cost_util.h"
#include "call_info.h" 
#include "be_util.h" 
#include "lwn_util.h" 
#include "ipa_lno_summary.h" 
#include "ipa_lno_read.h" 
#include "opt_du.h" 
#include "const.h"

//-----------------------------------------------------------------------
// NAME: Cast_Float_Operands
// FUNCTION: Given that 'wn_addr_one' and 'wn_addr_two' are the addresses
//   of two nodes whose result type is a float type, cast them so that
//   they are the same float type, and return their common float type. 
//-----------------------------------------------------------------------

extern TYPE_ID Cast_Float_Operands(WN** wn_addr_one, 
				     WN** wn_addr_two) 
{ 
  WN* wn_one = *wn_addr_one; 
  WN* wn_two = *wn_addr_two; 
  TYPE_ID mtype_one = OPCODE_rtype(WN_opcode(wn_one));
  TYPE_ID mtype_two = OPCODE_rtype(WN_opcode(wn_two));
  TYPE_ID mtype = MTYPE_UNKNOWN; 
  if (mtype_one < mtype_two) {
    OPCODE op = OPCODE_make_op(OPR_CVT, mtype_two, mtype_one);
    wn_one = LWN_CreateExp1(op, wn_one);
    mtype = mtype_two;
  } else if (mtype_two < mtype_one) {
    OPCODE op = OPCODE_make_op(OPR_CVT, mtype_one, mtype_two);
    wn_two = LWN_CreateExp1(op, wn_two);
    mtype = mtype_one;
  } else { 
    mtype = mtype_one; 
  } 
  wn_addr_one = &wn_one; 
  wn_addr_two = &wn_two; 
  return mtype; 
} 

//-----------------------------------------------------------------------
// NAME: Simple_Cost_Value
// FUNCTION: Return the "simple" cost value of the 'sv_index'th SUMMARY_
//   VALUE in the DYN_ARRAY 'sv'. 
//-----------------------------------------------------------------------

static INT64 Simple_Cost_Value(DYN_ARRAY<SUMMARY_VALUE>* sv,
			       INT sv_index)
{
  SUMMARY_VALUE* svv = &(*sv)[sv_index];
  if (svv->Is_trip_count())
    return DEFAULT_TRIP_COUNT; 
  if (svv->Is_int_const())
    return svv->Get_int_const_value(); 
  INT64 value = -1;
  if (svv->Is_const_st()) 
    if (St_Idx_Is_Intconst(svv->Get_const_st_idx(), &value))
      return value;
  FmtAssert(FALSE, 
    ("Simple_Cost_Value: Non-const VALUE not part of trip count"));
  return -1;
} 

//-----------------------------------------------------------------------
// NAME: Simple_Cost_Traverse
// FUNCTION: Return the "simple" cost value of the 'sx_index'th SUMMARY_
//   EXPR in the DYN_ARRAY pair ('sv','sx'). 
//-----------------------------------------------------------------------

static INT64 Simple_Cost_Traverse(DYN_ARRAY<SUMMARY_VALUE>* sv,
			          DYN_ARRAY<SUMMARY_EXPR>* sx, 
			          INT sx_index)
{
  INT64 value_left = -1;
  INT64 value_right = -1;
  INT64 value_result = -1; 
  SUMMARY_EXPR* sxx = &(*sx)[sx_index];
  if (sxx->Is_trip_count()) 
    return DEFAULT_TRIP_COUNT;
  if (sxx->Has_const_operand()) { 
    if (sxx->Is_expr_value(0)) { 
      INT sv_index = sxx->Get_node_index(0);
      value_left = Simple_Cost_Value(sv, sv_index);
    } else if (sxx->Is_expr_expr(0)) { 
      INT sx_index = sxx->Get_node_index(0);
      value_left = Simple_Cost_Traverse(sv, sx, sx_index);
    } 
    value_right = sxx->Get_const_value();
  } else { 
    if (sxx->Is_expr_value(0)) { 
      INT sv_index = sxx->Get_node_index(0);
      value_left = Simple_Cost_Value(sv, sv_index);
    } else if (sxx->Is_expr_expr(0)) { 
      INT sx_index = sxx->Get_node_index(0);
      value_left = Simple_Cost_Traverse(sv, sx, sx_index);
    } 
    if (sxx->Is_expr_value(0)) { 
      INT sv_index = sxx->Get_node_index(0);
      value_right = Simple_Cost_Value(sv, sv_index);
    } else if (sxx->Is_expr_expr(0)) { 
      INT sx_index = sxx->Get_node_index(0);
      value_right = Simple_Cost_Traverse(sv, sx, sx_index);
    } 
  }
  switch (OPCODE_operator(sxx->Get_opcode())) {
  case OPR_ADD:
    value_result = value_left + value_right;
    break;
  case OPR_SUB:
    value_result = value_left - value_right;
    break;
  case OPR_MPY:
    value_result = value_left * value_right;
    break;
  case OPR_DIV: 
    value_result = value_left / value_right;
    break;
  default:
    FmtAssert(FALSE,
      ("Simple_Cost_Traverse: Unexpected operator in SUMMARY_EXPR"));
  }
  return value_result; 
} 

//-----------------------------------------------------------------------
// NAME: Simple_Execution_Cost
// FUNCTION: Return the "simple" cost of executing the call 'wn_call'. 
//-----------------------------------------------------------------------

extern INT64 Simple_Execution_Cost(WN* wn_call) 
{ 
  CALL_INFO* call_info = (CALL_INFO *) WN_MAP_Get(LNO_Info_Map, wn_call);
  DYN_ARRAY<SUMMARY_VALUE>* sv = call_info->Value();
  DYN_ARRAY<SUMMARY_EXPR>* sx = call_info->Expr();
  INT sx_index = sx->Lastidx();
  return Simple_Cost_Traverse(sv, sx, sx_index);
} 

//-----------------------------------------------------------------------
// NAME: Can_Evaluate_Cost
// FUNCTION: Return TRUE if we can develop an "advanced" formula for the 
//   cost of call 'wn_call', using the information on 'IPA_LNO_File'.  
//   Return FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Can_Evaluate_Cost(WN* wn_call)
{ 
  CALL_INFO* call_info = (CALL_INFO *) WN_MAP_Get(LNO_Info_Map, wn_call);
  DYN_ARRAY<SUMMARY_VALUE>* sv = call_info->Value();
  for (INT i = 0; i <= sv->Lastidx(); i++) { 
    SUMMARY_VALUE* svv = &(*sv)[i];
    if (svv->Is_formal()) { 
      INT formal_position = svv->Get_formal_index();
      if (!(formal_position >= 0 && formal_position < WN_kid_count(wn_call)))
	return FALSE; 
      WN* wn_parm = WN_kid(wn_call, formal_position);
      if (WN_operator(wn_parm) != OPR_PARM)
	return FALSE; 
      WN* wn_arg = WN_kid0(wn_parm);
      if (!OPCODE_has_sym(WN_opcode(wn_arg)))
	return FALSE; 
    } 
  } 
  return TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: Make_Def_List_For_Global
// FUNCTION: Create a DEF_LIST for the LDID 'wn_ldid', which is a global
//   variable which is referenced in the CALL 'wn_call'. 
//-----------------------------------------------------------------------

static void Make_Def_List_For_Global(WN* wn_call, 
				     WN* wn_ldid)
{ 
  DEF_LIST* def_list = Du_Mgr->Ud_Get_Def(wn_call);
  if (def_list == NULL)
    return; 
  DEF_LIST_ITER iter(def_list);
  const DU_NODE* node = NULL;
  for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
    WN* wn_def = node->Wn();
    if (Aliased(Alias_Mgr, wn_def, wn_ldid) == NOT_ALIASED)
      continue; 
    BOOL is_preg_def = OPCODE_has_sym(WN_opcode(wn_def)) 
	&& ST_class(WN_st(wn_def)) == CLASS_PREG;
    BOOL is_preg_ldid = OPCODE_has_sym(WN_opcode(wn_ldid))
	&& ST_class(WN_st(wn_ldid)) == CLASS_PREG;
    if (is_preg_def && !is_preg_ldid || !is_preg_def && is_preg_ldid)
      continue; 
    if (is_preg_def && is_preg_ldid && WN_offset(wn_def) != WN_offset(wn_ldid))
      continue; 
    Du_Mgr->Add_Def_Use(wn_def, wn_ldid);
  } 
  DEF_LIST* dl_new = Du_Mgr->Ud_Get_Def(wn_ldid);
  if (dl_new != NULL && def_list->Incomplete())
    dl_new->Set_Incomplete();
  Update_Loop_Stmt(wn_ldid);
} 

//-----------------------------------------------------------------------
// NAME: Find_And_Make_Alias
// FUNCTION: Find a node which is a definition of CALL 'wn_call' with the 
//   same SYMBOL as 'wn_ldid' and copy the alias info from that node to 
//   'wn_ldid'.  If there is no such definition, then do nothing. 
//-----------------------------------------------------------------------

static void Find_And_Make_Alias(WN* wn_call, 
				WN* wn_ldid)
{ 
  DEF_LIST* def_list = Du_Mgr->Ud_Get_Def(wn_call);
  if (def_list == NULL)
    return; 
  DEF_LIST_ITER iter(def_list);
  const DU_NODE* node = NULL;
  WN* wn_good_def = NULL;
  for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
    WN* wn_def = node->Wn();
    if (OPCODE_has_sym(WN_opcode(wn_def))
	&& SYMBOL(wn_def) == SYMBOL(wn_ldid)) { 
      wn_good_def = wn_def; 
      break;
    } 
  } 
  if (wn_good_def == NULL)
    return; 
  Copy_alias_info(Alias_Mgr, wn_good_def, wn_ldid);
} 

//-----------------------------------------------------------------------
// NAME: Execution_Cost_Value
// FUNCTION: Return WN expression corresponding to the 'sv_index'th entry 
//   of 'sv' evaluated at the CALL 'wn_call'.
// NOTE: Use floating point values because the arithmetic is faster than 
//   integer arithmetic. 
//-----------------------------------------------------------------------

static WN* Execution_Cost_Value(WN* wn_call,
                                DYN_ARRAY<SUMMARY_VALUE>* sv, 
                                INT sv_index)
{ 
  SUMMARY_VALUE* svv = &(*sv)[sv_index]; 
  if (svv->Is_int_const()) { 
    INT64 const_value = svv->Get_int_const_value();
    float float_value = (float) const_value; 
    return Make_Const(Host_To_Targ_Float(MTYPE_F8, float_value));
  } else if (svv->Is_const_st()) { 
    ST_IDX st_idx = svv->Get_const_st_idx();
    TYPE_ID mtype = TY_mtype(ST_type(st_idx));
    if (MTYPE_is_integral(mtype)) { 
      ST* st = &St_Table[st_idx];
      INT64 const_value = Targ_To_Host(STC_val(st));
      float float_value = (float) const_value;
      return Make_Const(Host_To_Targ_Float(MTYPE_F8, float_value));
    } 
    WN* wn_ldid = WN_CreateLdid(OPR_LDID, Promote_Type(mtype), mtype, 0, 
      st_idx, ST_type(st_idx));
    Find_And_Make_Alias(wn_call, wn_ldid);
    Du_Mgr->Add_Def_Use(Current_Func_Node, wn_ldid);
    OPCODE op_float = OPCODE_make_op(OPR_CVT, MTYPE_F8, WN_rtype(wn_ldid));
    WN* wn_fldid = LWN_CreateExp1(op_float, wn_ldid);
    return wn_fldid;  
  } else if (svv->Is_formal()) {  
    INT formal_position = svv->Get_formal_index();
    FmtAssert(formal_position >= 0 && formal_position < WN_kid_count(wn_call),
      ("Execution_Cost_Value: Unexpected formal position"));
    WN* wn_parm = WN_kid(wn_call, formal_position);  
    FmtAssert(WN_operator(wn_parm) == OPR_PARM, 
      ("Execution_Cost_Value: Expecting PARM node under CALL"));
    WN* wn_arg = WN_kid0(wn_parm);
    FmtAssert(OPCODE_has_sym(WN_opcode(wn_arg)), 
      ("Execution_Cost_Value: Expecting single symbol argument"));
    ST_IDX st_idx = WN_st_idx(wn_arg);
    ST* st = &St_Table[st_idx];
    TYPE_ID mtype = TY_mtype(ST_type(st_idx));
    if (ST_class(st) == CLASS_CONST && MTYPE_is_integral(mtype)) { 
      INT64 const_value = Targ_To_Host(STC_val(st));
      float float_value = (float) const_value;
      return Make_Const(Host_To_Targ_Float(MTYPE_F8, float_value));
    } 
    WN* wn_ldid = WN_CreateLdid(OPR_LDID, Promote_Type(mtype), mtype, 0, 
      st_idx, ST_type(st_idx));
    Create_alias(Alias_Mgr, wn_ldid);
    LWN_Copy_Def_Use_Node(wn_parm, wn_ldid, Du_Mgr);
    DEF_LIST* def_list = Du_Mgr->Ud_Get_Def(wn_ldid);
    if (def_list == NULL)
      Du_Mgr->Add_Def_Use(Current_Func_Node, wn_ldid);
    OPCODE op_float = OPCODE_make_op(OPR_CVT, MTYPE_F8, WN_rtype(wn_ldid));
    WN* wn_fldid = LWN_CreateExp1(op_float, wn_ldid);
    return wn_fldid;  
  } else if (svv->Is_global()) { 
    ST_IDX st_idx = svv->Get_global_st_idx();
    ST* st = &St_Table[st_idx];
    TYPE_ID mtype = TY_mtype(ST_type(st_idx));
    if (ST_class(st) == CLASS_CONST && MTYPE_is_integral(mtype)) {
      INT64 const_value = Targ_To_Host(STC_val(st));
      float float_value = (float) const_value;
      return Make_Const(Host_To_Targ_Float(MTYPE_F8, float_value));
    }
    WN* wn_ldid = WN_CreateLdid(OPR_LDID, Promote_Type(mtype), mtype, 0, 
      st_idx, ST_type(st_idx));
    Create_alias(Alias_Mgr, wn_ldid);
    Make_Def_List_For_Global(wn_call, wn_ldid);
    OPCODE op_float = OPCODE_make_op(OPR_CVT, MTYPE_F8, WN_rtype(wn_ldid));
    WN* wn_fldid = LWN_CreateExp1(op_float, wn_ldid);
    return wn_fldid;  
  } else { 
    FmtAssert(FALSE, ("Execution_Cost_Value: Unexpected SUMMARY_VALUE type"));
    return NULL; 
  } 
} 

//-----------------------------------------------------------------------
// NAME: Execution_Cost_Expr
// FUNCTION: Return WN expression corresponding to the 'sx_index'th entry 
//   of 'sx' evaluated at the CALL 'wn_call' using the information in 
//   'IPA_LNO_File'. (Here 'sv' is the corresponding DYN_ARRAY<SUMMARY_VALUE>
//   in the pair ('sv','sx').)
//-----------------------------------------------------------------------

static WN* Execution_Cost_Expr(IPA_LNO_READ_FILE* IPA_LNO_File,
			       WN* wn_call,
			       DYN_ARRAY<SUMMARY_EXPR>* sx, 
			       DYN_ARRAY<SUMMARY_VALUE>* sv, 
			       INT sx_index)
{
  SUMMARY_EXPR* sxx = &(*sx)[sx_index]; 
  WN* wn_one = NULL; 
  WN* wn_two = NULL; 
  TYPE_ID mtype = MTYPE_UNKNOWN; 
  if (sxx->Has_const_operand()) { 
    WN* wn_non_const = NULL; 
    if (sxx->Is_expr_value(0)) { 
      INT value_index = sxx->Get_node_index(0);
      wn_one = Execution_Cost_Value(wn_call, sv, value_index); 
    } else if (sxx->Is_expr_expr(0)) { 
      INT expr_index = sxx->Get_node_index(0); 
      wn_one = Execution_Cost_Expr(IPA_LNO_File, wn_call, sx, sv, expr_index);
    } 
    FmtAssert(wn_one != NULL, ("Execution_Cost_Expr: NULL expression"));
    INT64 const_value = sxx->Get_const_value();
    float float_value = (float) const_value; 
    wn_two = Make_Const(Host_To_Targ_Float(MTYPE_F8, float_value));
  } else { 
    if (sxx->Is_expr_value(0)) { 
      INT value_index = sxx->Get_node_index(0);
      wn_one = Execution_Cost_Value(wn_call, sv, value_index); 
    } else if (sxx->Is_expr_expr(0)) { 
      INT expr_index = sxx->Get_node_index(0); 
      wn_one = Execution_Cost_Expr(IPA_LNO_File, wn_call, sx, sv, expr_index);
    } 
    FmtAssert(wn_one != NULL, ("Execution_Cost_Expr: NULL expression"));
    if (sxx->Is_expr_value(1)) { 
      INT value_index = sxx->Get_node_index(1);
      wn_two = Execution_Cost_Value(wn_call, sv, value_index); 
    } else if (sxx->Is_expr_expr(1)) { 
      INT expr_index = sxx->Get_node_index(1); 
      wn_two = Execution_Cost_Expr(IPA_LNO_File, wn_call, sx, sv, expr_index);
    } 
    FmtAssert(wn_two != NULL, ("Execution_Cost_Expr: NULL expression"));
  } 
  mtype = Cast_Float_Operands(&wn_one, &wn_two);
  OPERATOR opr = OPCODE_operator(sxx->Get_opcode());
  OPCODE opc = OPCODE_make_op(opr, mtype, MTYPE_V); 
  return LWN_CreateExp2(opc, wn_one, wn_two);
} 

//-----------------------------------------------------------------------
// NAME: Execution_Cost
// FUNCTION: Return the cost of executing 'wn_call' using the information
//   on 'IPA_LNO_File'.  If a constant cost is returned, make it a node 
//   with machine type 'wtype'. 
//-----------------------------------------------------------------------

extern WN* Execution_Cost(IPA_LNO_READ_FILE* IPA_LNO_File, 
			  WN* wn_call,
			  TYPE_ID wtype)
{ 
  if (!Can_Evaluate_Cost(wn_call)) { 
    INT64 const_value = Simple_Execution_Cost(wn_call);
    float float_value = (float) const_value;
    return Make_Const(Host_To_Targ_Float(MTYPE_F8, float_value));
  } 
  CALL_INFO* call_info = (CALL_INFO *) WN_MAP_Get(LNO_Info_Map, wn_call);
  DYN_ARRAY<SUMMARY_VALUE>* sv = call_info->Value();
  DYN_ARRAY<SUMMARY_EXPR>* sx = call_info->Expr();
  if (sx->Lastidx() == -1)
    return Execution_Cost_Value(wn_call, sv, sv->Lastidx());
  WN* wn_expr = Execution_Cost_Expr(IPA_LNO_File, wn_call, sx, sv, 
    sx->Lastidx());
  INT node_count = Node_Count(wn_expr, MAX_CALL_EXPR_COUNT, FALSE);
  if (node_count <= MAX_CALL_EXPR_COUNT)
    return wn_expr; 
  LWN_Delete_Tree(wn_expr);
  INT64 const_value = Simple_Execution_Cost(wn_call);
  float float_value = (float) const_value;
  return Make_Const(Host_To_Targ_Float(MTYPE_F8, float_value));
} 
