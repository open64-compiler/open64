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


#include <stdint.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include "ipl_summary.h"
#include "ipa_cg.h" 
#include "ipa_section_main.h"
#include "ipa_section_annot.h"
#include "ipa_cost_util.h"
#include "be_util.h"
#include "ipa_option.h" 
#include "ipaa.h" 

// Forward declaration.

static INT IPA_EX_Copy_Expr_Tree(IPA_NODE* ipan_caller, 
				 SUMMARY_VALUE* sv_old,
                                 SUMMARY_EXPR* sx_old,
                                 DYN_ARRAY<SUMMARY_VALUE>* sv_new,
                                 DYN_ARRAY<SUMMARY_EXPR>* sx_new,
                                 INT sx_old_index);

//-----------------------------------------------------------------------
// NAME: Convert_Global_To_St_Idx
// FUNCTION: Convert the SUMMARY_VALUE 'svv' from "Is_global" type to 
//   "Is_global_st_idx" type. 
// NOTE: We must do this before passing information to LNO, because 
//   LNO is aware of global variables only through the global symbol table. 
//-----------------------------------------------------------------------

static void Convert_Global_To_St_Idx(IPA_NODE* ipan, 
				     SUMMARY_VALUE* svv)
{
  FmtAssert(svv->Is_global(), 
    ("Convert_Global_To_St_Idx: Expecting a global value"));
  FmtAssert(!svv->Is_global_st_idx(),
    ("Convert_Global_To_St_Idx: Expecting global in symbol index format"));
  INT symbol_index = svv->Get_global_index();
  SUMMARY_SYMBOL* symbol_array = IPA_get_symbol_array(ipan);
  SUMMARY_SYMBOL* ss = &symbol_array[symbol_index];
  ST_IDX st_idx = ss->St_idx();
  svv->Set_global_index(-1);
  svv->Set_is_global_st_idx();
  svv->Set_global_st_idx(st_idx);
} 

//-----------------------------------------------------------------------
// NAME: Update_Execution_Cost
// FUNCTION: Annotate 'ipan' with the execution cost information that 
//   was gathered during the IPL phase. 
//-----------------------------------------------------------------------

extern void Update_Execution_Cost(IPA_NODE* ipan,
                                  MEM_POOL* mem_pool)
{
  DYN_ARRAY<SUMMARY_VALUE>* sv
    = CXX_NEW(DYN_ARRAY<SUMMARY_VALUE>(mem_pool), mem_pool);
  DYN_ARRAY<SUMMARY_EXPR>* sx
    = CXX_NEW(DYN_ARRAY<SUMMARY_EXPR>(mem_pool), mem_pool);
  IPA_NODE_SECTION_INFO* si = ipan->Section_Annot();
  si->Set_value(sv);
  si->Set_expr(sx);
  SUMMARY_PROCEDURE* sp = ipan->Summary_Proc();
  INT value_index = sp->Get_ex_value_index();
  INT value_count = sp->Get_ex_value_count();
  INT expr_index = sp->Get_ex_expr_index();
  INT expr_count = sp->Get_ex_expr_count();
  INT formal_index = sp->Get_formal_index();
  INT global_index = sp->Get_global_index();
  SUMMARY_VALUE* value_array = IPA_get_value_array(ipan);
  INT i;

  for (i = value_index; i < value_index + value_count; i++) {
    SUMMARY_VALUE* svv = value_array + i;
    sv->AddElement(*svv);
  }
  SUMMARY_EXPR* expr_array = IPA_get_expr_array(ipan);
  for (i = expr_index; i < expr_index + expr_count; i++) {
    SUMMARY_EXPR* sxx = expr_array + i;
    sx->AddElement(*sxx);
  }
  IPL_EX_Add_Value_Offsets(sv, -formal_index, -global_index);
  IPL_EX_Add_Expr_Offsets(sx, -value_index, -expr_index);
  // Put ST_IDX values into the SUMMARY_GLOBALs
  for (i = 0; i <= sv->Lastidx(); i++) {
    SUMMARY_VALUE* svv = &(*sv)[i];
    if (svv->Is_global())
      Convert_Global_To_St_Idx(ipan, svv);
  }
}

//-----------------------------------------------------------------------
// NAME: Execution_Count_Evaluated
// FUNCTION: Return TRUE if the execution cost for 'ipan' is fully eval-
//   uated, i.e. does not contain any explicit references to callsites.
//   Return FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Execution_Count_Evaluated(IPA_NODE* ipan)
{ 
  IPA_NODE_SECTION_INFO* si = ipan->Section_Annot();
  DYN_ARRAY<SUMMARY_VALUE>* sv = si->Get_value();
  for (INT i = 0; i <= sv->Lastidx(); i++) { 
    SUMMARY_VALUE* svv = &(*sv)[i];
    if (svv->Is_callsite())
      return FALSE; 
  } 
  return TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: IPA_EX_Valid_Opcode
// FUNCTION: Return TRUE if 'opcode' is a valid OPCODE for an execution 
//   cost expression.  Return FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL IPA_EX_Valid_Opcode(OPCODE opcode)
{
  switch (opcode) { 
  case OPC_I4ADD:
  case OPC_I4MPY: 
  case OPC_I4NEG: 
  case OPC_I4SUB: 
  case OPC_I4DIV: 
    return TRUE; 
  default: 
    return FALSE; 
  } 
} 

//-----------------------------------------------------------------------
// NAME: Is_Non_Exported
// FUNCTION:  Return TRUE if the 'st_idx' refers to a variable of export 
//   class EXPORT_LOCAL or EXPORT_LOCAL_INTERNAL.  Return FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Is_Exported(ST_IDX st_idx)
{
  ST_EXPORT st_export = ST_export(St_Table[st_idx]);
  return st_export != EXPORT_LOCAL && st_export != EXPORT_LOCAL_INTERNAL;
}

//-----------------------------------------------------------------------
// NAME: IPA_EX_Valid_Value
// FUNCTION: Return TRUE if the 'value_index'th VALUE in the cost for 
//   'ipan_caller' is a valid value.  Return FALSE otherwise. 
// NOTE: All values are valid except globals which are defined at some  
//   place in the 'ipan_caller' and those which do not have appropriate 
//   exported base information. 
//-----------------------------------------------------------------------

static BOOL IPA_EX_Valid_Value(IPA_NODE* ipan_caller, 
			       INT value_index)
{
  SUMMARY_VALUE* value_array = IPA_get_value_array(ipan_caller);
  SUMMARY_VALUE* svv = &value_array[value_index];
  if (svv->Is_global()) {
    FmtAssert(!svv->Is_global_st_idx(),
      ("IPA_EX_Valid_Value: Expecting global in symbol index format"));
    IPAA_NODE_INFO* ipaa = ipan_caller->Mod_Ref_Info();
    SUMMARY_VALUE svv_test; 
    bcopy(svv, &svv_test, sizeof(SUMMARY_VALUE));
    Convert_Global_To_St_Idx(ipan_caller, &svv_test);
    ST_IDX st_idx = svv_test.Get_global_st_idx(); 
    ST_IDX st_idx_test = 0; 
    if (ST_is_split_common(St_Table[st_idx])) { 
      st_idx_test = ST_full_idx(St_Table[st_idx]);
    } else { 
      for (st_idx_test = st_idx; 
	ST_base_idx(St_Table[st_idx_test]) != st_idx_test
	  && !Is_Exported(st_idx_test); 
	st_idx_test = ST_base_idx(St_Table[st_idx_test]));
      if (!Is_Exported(st_idx_test))
	return FALSE; 
    } 
    UINT32 mod_ref_key = ST_IDX_index(st_idx_test);
    if (ipaa->Is_def_elmt(mod_ref_key))
      return FALSE; 
    return TRUE; 
  } 
  return svv->Is_int_const() || svv->Is_const_st() || svv->Is_formal();
} 

//-----------------------------------------------------------------------
// NAME: IPA_EX_Valid_Expr
// FUNCTION: Return TRUE if the 'expr_index'th EXPR in the cost for 
//   'ipan' is valid.  Otherwise return FALSE. 
//-----------------------------------------------------------------------

static BOOL IPA_EX_Valid_Expr(IPA_NODE* ipan, 
			      INT expr_index)
{
  SUMMARY_EXPR* expr_array = IPA_get_expr_array(ipan);
  SUMMARY_EXPR* sxx = &expr_array[expr_index];
  if (!IPA_EX_Valid_Opcode(sxx->Get_opcode()))
    return FALSE; 
  if (sxx->Has_const_operand()) { 
    if (sxx->Is_expr_value(0))
      return IPA_EX_Valid_Value(ipan, sxx->Get_node_index(0));
    else if (sxx->Is_expr_expr(0))
      return IPA_EX_Valid_Expr(ipan, sxx->Get_node_index(0)); 
    return FALSE; 
  } else { 
    if (sxx->Is_expr_value(0)) { 
      if (!IPA_EX_Valid_Value(ipan, sxx->Get_node_index(0)))
        return FALSE; 
    } else if (sxx->Is_expr_expr(0)) { 
      if (!IPA_EX_Valid_Expr(ipan, sxx->Get_node_index(0)))
        return FALSE; 
    } else { 
      return FALSE; 
    } 
    if (sxx->Is_expr_value(1)) { 
      if (!IPA_EX_Valid_Value(ipan, sxx->Get_node_index(1)))
        return FALSE; 
    } else if (sxx->Is_expr_expr(1)) { 
      if (!IPA_EX_Valid_Expr(ipan, sxx->Get_node_index(1)))
        return FALSE; 
    } else { 
      return FALSE; 
    } 
  } 
  return TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: IPA_EX_Value_Expr_Tree
// FUNCTION: Return TRUE if the VALUE 'svv' represents a valid expression.
//   Return FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL IPA_EX_Value_Expr_Tree(IPA_NODE* ipan, 
				   SUMMARY_VALUE* svv)
{ 
  if (!svv->Is_expr())
    return FALSE; 
  return IPA_EX_Valid_Expr(ipan, svv->Get_expr_index());
} 

//-----------------------------------------------------------------------
// NAME: IPA_EX_Copy_Value
// FUNCTION: Copy the VALUE in the 'sv_old_index'th entry of 'sv_old' to 
//   'sv_new'.  Use 'ipan_caller' to convert any global to a global st_idx. 
//   Return the index of the new VALUE in 'sv_new'. 
//-----------------------------------------------------------------------

static INT IPA_EX_Copy_Value(IPA_NODE* ipan_caller, 
			     SUMMARY_VALUE* sv_old,
                             DYN_ARRAY<SUMMARY_VALUE>* sv_new,
                             INT sv_old_index)
{
  INT sv_new_index = sv_new->Newidx();
  SUMMARY_VALUE* svv_old = &sv_old[sv_old_index];
  SUMMARY_VALUE* svv_new = &(*sv_new)[sv_new_index];
  bcopy(svv_old, svv_new, sizeof(SUMMARY_VALUE));
  if (svv_new->Is_formal()) { 
    SUMMARY_PROCEDURE* sp_caller = ipan_caller->Summary_Proc();
    INT base_caller_formal = sp_caller->Get_formal_index();
    INT new_formal_index = svv_new->Get_formal_index() - base_caller_formal; 
    svv_new->Set_formal_index(new_formal_index);
  } else if (svv_new->Is_global()) {
    Convert_Global_To_St_Idx(ipan_caller, svv_new);
  } 
  return sv_new_index;
}

//-----------------------------------------------------------------------
// NAME: IPA_EX_Copy_Expr
// FUNCTION: Copy the EXPR in the 'sx_old_index' entry of 'sx_old' to 
//   'sx_new'.  Return the index of the new EXPR in 'sv_new'. 
//-----------------------------------------------------------------------

static INT IPA_EX_Copy_Expr(SUMMARY_EXPR* sx_old,
                            DYN_ARRAY<SUMMARY_EXPR>* sx_new,
                            INT sx_old_index)
{
  INT sx_new_index = sx_new->Newidx();
  SUMMARY_EXPR* sxx_old = &sx_old[sx_old_index];
  SUMMARY_EXPR* sxx_new = &(*sx_new)[sx_new_index];
  bcopy(sxx_old, sxx_new, sizeof(SUMMARY_EXPR));
  return sx_new_index;
}

//-----------------------------------------------------------------------
// NAME: IPA_EX_Set_Expr_Index
// FUNCTION: Copy the VALUE or EXPR of the 'kid'th part of 'sxx_old' 
//   from ('sv_old','sx_old') to ('sv_new','sx_new').  Use 'ipan_caller' 
//   to convert globals to global st_idx's when necessary.  Return the 
//   index of added entry. 
//-----------------------------------------------------------------------

static INT IPA_EX_Set_Expr_Index(IPA_NODE* ipan_caller, 
				 SUMMARY_VALUE* sv_old,
                                 SUMMARY_EXPR* sx_old,
                                 DYN_ARRAY<SUMMARY_VALUE>* sv_new,
                                 DYN_ARRAY<SUMMARY_EXPR>* sx_new,
                                 SUMMARY_EXPR* sxx_old,
                                 INT kid)
{
  if (sxx_old->Is_expr_value(kid)) {
    INT sv_old_index = sxx_old->Get_node_index(kid);
    INT sv_new_index = IPA_EX_Copy_Value(ipan_caller, sv_old, sv_new, 
      sv_old_index);
    return sv_new_index; 
  } else if (sxx_old->Is_expr_expr(kid)) {
    INT sxx_old_index = sxx_old->Get_node_index(kid);
    INT sxx_new_index = IPA_EX_Copy_Expr_Tree(ipan_caller, sv_old, sx_old, 
      sv_new, sx_new, sxx_old_index);
    return sxx_new_index; 
  } else {
    FmtAssert(FALSE,
      ("IPA_EX_Set_Expr_Index: Not handling this case yet"));
    return -1; 
  }
} 

//-----------------------------------------------------------------------
// NAME: IPA_EX_Copy_Expr_Tree
// FUNCTION: Copy the 'sx_old_index'th EXPR from ('sv_old','sx_old') to 
//   ('sv_new','sx_new').  Use 'ipan_caller' to convert globals to global 
//   st_idx's when necessary.  Return the index of added entry. 
//-----------------------------------------------------------------------

static INT IPA_EX_Copy_Expr_Tree(IPA_NODE* ipan_caller, 
				 SUMMARY_VALUE* sv_old,
                                 SUMMARY_EXPR* sx_old,
                                 DYN_ARRAY<SUMMARY_VALUE>* sv_new,
                                 DYN_ARRAY<SUMMARY_EXPR>* sx_new,
                                 INT sx_old_index)
{
  SUMMARY_EXPR* sxx_old = &sx_old[sx_old_index];
  INT sx_expr_one = -1;
  INT sx_expr_two = -1;
  INT sx_new_index = -1;   
  if (sxx_old->Has_const_operand()) {
    sx_expr_one = IPA_EX_Set_Expr_Index(ipan_caller, sv_old, sx_old, 
      sv_new, sx_new, sxx_old, 0);
    sx_new_index = IPA_EX_Copy_Expr(sx_old, sx_new, sx_old_index);
    SUMMARY_EXPR* sxx_new = &(*sx_new)[sx_new_index];
    sxx_new->Set_node_index(0, sx_expr_one);
  } else {
    sx_expr_one = IPA_EX_Set_Expr_Index(ipan_caller, sv_old, sx_old, 
      sv_new, sx_new, sxx_old, 0);
    sx_expr_two = IPA_EX_Set_Expr_Index(ipan_caller, sv_old, sx_old, 
      sv_new, sx_new, sxx_old, 1);
    sx_new_index = IPA_EX_Copy_Expr(sx_old, sx_new, sx_old_index);
    SUMMARY_EXPR* sxx_new = &(*sx_new)[sx_new_index];
    sxx_new->Set_node_index(0, sx_expr_one);
    sxx_new->Set_node_index(1, sx_expr_two);
  }
  return sx_new_index;
}

//-----------------------------------------------------------------------
// NAME: IPA_EX_Can_Evaluate_At_Callsite
// FUNCTION: Return TRUE if the CALLSITE 'cs' of 'ipan_callee' can be 
//   evalauted for the 'ipan_caller'.  The VALUEs for 'ipan_callee' are 
//   given in 'sv_callee'. 
//-----------------------------------------------------------------------

static BOOL IPA_EX_Can_Evaluate_At_Callsite(IPA_NODE* ipan_caller,
					    IPA_NODE* ipan_callee,
					    DYN_ARRAY<SUMMARY_VALUE>* 
					      sv_callee,
					    SUMMARY_CALLSITE* cs)
{
  if (ipan_caller->Has_Recursive_In_Edge())
    return FALSE; 
  SUMMARY_PROCEDURE* sp_callee = ipan_callee->Summary_Proc();
  for (INT i = 0; i <= sv_callee->Lastidx(); i++) {
    SUMMARY_VALUE* svv = &(*sv_callee)[i];
    if (svv->Is_formal()) {
      INT formal_index = svv->Get_formal_index();
      FmtAssert(formal_index >= 0, 
	("IPA_EX_Can_Evaluate_At_Callsite: Expecting nonnegative formal idx"));
      INT actual_index = cs->Get_actual_index() + formal_index;
      FmtAssert(actual_index >= 0, 
        ("IPA_EX_Can_Evaluate_At_Callsite: Expecting nonnegative actual idx"));
      SUMMARY_ACTUAL* sa = IPA_get_actual_array(ipan_caller) + actual_index;
      FmtAssert(sa != NULL, 
	("IPA_EX_Can_Evaluate_At_Callsite: NULL SUMMARY_ACTUAL")); 
      if (sa->Get_value_index() == -1)
        return FALSE; 
      SUMMARY_VALUE* sv = IPA_get_value_array(ipan_caller) 
        + sa->Get_value_index();
      if (!sv->Is_int_const() && !sv->Is_const_st() && !sv->Is_formal()
	  && !sv->Is_expr())
        return FALSE; 
      if (sv->Is_expr() && !IPA_EX_Value_Expr_Tree(ipan_caller, sv))
        return FALSE; 
    }
  }
  return TRUE; 
}

//-----------------------------------------------------------------------
// NAME: IPA_EX_Evaluate_At_Callsite
// FUNCTION: Evaluate 'ipan_caller' at the CALLSITE 'cs' (which is a call 
//   to 'ipan_callee').  The cost at this callsite is given by the pair 
//   ('sv_callee','sx_callee').
//-----------------------------------------------------------------------

static void IPA_EX_Evaluate_At_Callsite(IPA_NODE* ipan_caller, 
					IPA_NODE* ipan_callee,
					DYN_ARRAY<SUMMARY_VALUE>* sv_callee,
					DYN_ARRAY<SUMMARY_EXPR>* sx_callee,
					SUMMARY_CALLSITE* cs)
{ 
  SUMMARY_PROCEDURE* sp_callee = ipan_callee->Summary_Proc();
  INT original_value_count = sv_callee->Lastidx() + 1;
  for (INT i = 0; i < original_value_count; i++) { 
    SUMMARY_VALUE* svv = &(*sv_callee)[i];
    if (svv->Is_int_const() || svv->Is_two_consts() || svv->Is_const_st())
      continue; 
    if (svv->Is_formal()) { 
      INT formal_index = svv->Get_formal_index();
      INT actual_index = cs->Get_actual_index() + formal_index;
      SUMMARY_ACTUAL* sa = IPA_get_actual_array(ipan_caller) + actual_index;
      SUMMARY_VALUE* sv = sa->Get_value_index() == -1 ? NULL
	: IPA_get_value_array(ipan_caller) + sa->Get_value_index();
      FmtAssert(sv != NULL, 
	("IPA_EX_Evaluate_At_Callsite: Expecting SUMMARY_VALUE index"));
      if (sv->Is_int_const()) {
	svv->Set_int_const();
	svv->Set_int_const_value(sv->Get_int_const_value());	
      } else if (sv->Is_const_st()) { 
	INT64 int_value = -1;
	if (St_Idx_Is_Intconst(sv->Get_const_st_idx(), &int_value)) {
	  svv->Set_int_const();
	  svv->Set_int_const_value(int_value);
	} else { 
	  svv->Set_const_st();
	  svv->Set_const_st_idx(sv->Get_const_st_idx());
	} 
      } else if (sv->Is_formal()) { 
        SUMMARY_PROCEDURE* sp_caller = ipan_caller->Summary_Proc();
        INT base_caller_formal = sp_caller->Get_formal_index();
        INT new_formal_index = sv->Get_formal_index() - base_caller_formal; 
	svv->Set_formal_index(new_formal_index);
      } else if (sv->Is_expr()) { 
        SUMMARY_VALUE* sv_from = IPA_get_value_array(ipan_caller);
        SUMMARY_EXPR* sx_from = IPA_get_expr_array(ipan_caller);
	INT expr_index = IPA_EX_Copy_Expr_Tree(ipan_caller, sv_from, sx_from, 
          sv_callee, sx_callee, sv->Get_expr_index());
        SUMMARY_EXPR* sxx = &(*sx_callee)[expr_index]; 
        sxx->Set_is_trip_count();
      } else if (sv->Is_global()) {
        Fail_FmtAssertion("VALUE_GLOBALs not supported in execution costs");
      } else { 
	FmtAssert(FALSE,
	  ("IPA_EX_Evaluate_At_Callsite: Not expecting other VALUE types"));
      } 
    } else if (!svv->Is_global()) { 
      FmtAssert(FALSE, 
	("IPA_EX_Evaluate_At_Callsite: Not expecting other VALUE types"));
    } 
  } 
} 

//-----------------------------------------------------------------------
// NAME: IPL_EX_Replace_Value_With_Expr
// FUNCTION: In the 'sx', replace each reference to the 'value_index'th
//   VALUE with a reference to the 'expr_index'th EXPR. 
//-----------------------------------------------------------------------

static void IPL_EX_Replace_Value_With_Expr(DYN_ARRAY<SUMMARY_EXPR>* sx,
					   INT value_index, 
					   INT expr_index)
{ 
  for (INT i = 0; i <= sx->Lastidx(); i++) { 
    SUMMARY_EXPR* sxx = &(*sx)[i];
    if (sxx->Is_expr_value(0) && sxx->Get_node_index(0) == value_index) { 
      sxx->Set_expr_expr(0);
      sxx->Set_node_index(0, expr_index);
    } 
    if (sxx->Is_expr_value(1) && sxx->Get_node_index(1) == value_index) { 
      sxx->Set_expr_expr(1);
      sxx->Set_node_index(1, expr_index);
    } 
  } 
} 

//-----------------------------------------------------------------------
// NAME: IPL_EX_Substitute
// FUNCTION: Substitute the value of the ('sv_callee','sx_callee') pair
//   into the 'caller_value_index'th index of ('sv_caller'), obtaining
//   a new ('sv_caller','sx_caller') pair for the caller.  The substitute
//   value index must refer to a callsite. 
//-----------------------------------------------------------------------

static void IPL_EX_Substitute(DYN_ARRAY<SUMMARY_VALUE>* sv_caller, 
			      DYN_ARRAY<SUMMARY_EXPR>* sx_caller, 
			      DYN_ARRAY<SUMMARY_VALUE>* sv_callee, 
			      DYN_ARRAY<SUMMARY_EXPR>* sx_callee, 
			      INT caller_value_index, 
			      MEM_POOL* mem_pool)
{ 
  if (sv_callee->Lastidx() == -1 && sx_callee->Lastidx() == -1) { 
    SUMMARY_VALUE* svv = &(*sv_caller)[caller_value_index];
    svv->Set_int_const();
    svv->Set_int_const_value(DEFAULT_CALL_COST);
    svv->Set_mtype(MTYPE_I4);
    svv->Clear_is_addr_of();
    svv->Clear_is_trip_count();
  } else {
    INT i;

    DYN_ARRAY<SUMMARY_VALUE> sv_result(mem_pool);
    DYN_ARRAY<SUMMARY_EXPR> sx_result(mem_pool);

    for (i = 0; i <= sv_callee->Lastidx(); i++) { 
      SUMMARY_VALUE* svv = &(*sv_callee)[i];
      sv_result.AddElement(*svv);
    } 

    for (i = 0; i <= sx_callee->Lastidx(); i++) { 
      SUMMARY_EXPR* sxx = &(*sx_callee)[i];
      sx_result.AddElement(*sxx);
    } 

    INT value_offset = sv_callee->Lastidx() + 1;
    INT expr_offset = sx_callee->Lastidx() + 1;
    IPL_EX_Add_Expr_Offsets(sx_caller, value_offset, expr_offset);

    for (i = 0; i <= sv_caller->Lastidx(); i++) { 
      SUMMARY_VALUE* svv = &(*sv_caller)[i];
      sv_result.AddElement(*svv);
    } 

    for (i = 0; i <= sx_caller->Lastidx(); i++) { 
      SUMMARY_EXPR* sxx = &(*sx_caller)[i];
      sx_result.AddElement(*sxx);
    } 

    INT callsite_expr_index = sx_callee->Lastidx();
    INT new_caller_value_index = caller_value_index + value_offset; 

    IPL_EX_Replace_Value_With_Expr(&sx_result, new_caller_value_index, 
      callsite_expr_index); 
    IPL_EX_Eliminate_Value(&sv_result, &sx_result, new_caller_value_index);
    sv_caller->Resetidx(); 
    sx_caller->Resetidx(); 

    for (i = 0; i <= sv_result.Lastidx(); i++) 
      sv_caller->AddElement(sv_result[i]);

    for (i = 0; i <= sx_result.Lastidx(); i++) 
      sx_caller->AddElement(sx_result[i]);
  } 
} 

//-----------------------------------------------------------------------
// NAME: IPL_EX_Remove_Calls
// FUNCTION: For the execution cost patr ('sv','sx'), replace all refer-
//   ences to calls with constant values of cost DEFAULT_CALL_COST. 
// NOTE: This is done as a backup when evaluating a callsite cannot 
//   be done (or cannot be done conveniently).
//-----------------------------------------------------------------------

static void IPL_EX_Remove_Calls(DYN_ARRAY<SUMMARY_VALUE>* sv,
				DYN_ARRAY<SUMMARY_EXPR>* sx)
{
  for (INT i = sv->Lastidx(); i >= 0; i--) { 
    SUMMARY_VALUE* svv = &(*sv)[i];
    if (svv->Is_callsite()) {
      INT64 constant_value = DEFAULT_CALL_COST; 
      INT sv_index = IPL_EX_New_Constant(sv, constant_value);
      INT sx_index = IPL_EX_New_Value_Expr(sx, sv_index);
      IPL_EX_Replace_Value_With_Expr(sx, i, sx_index);
    } 
  }
} 

//-----------------------------------------------------------------------
// NAME: Merge_Execution_Cost
// FUNCTION: Merge the execution cost of each of the descendants of 
//    'ipan_caller' into 'ipan_caller's execution cost estimate. 
//-----------------------------------------------------------------------

extern BOOL Merge_Execution_Cost(IPA_NODE* ipan_caller,
				 MEM_POOL* mem_pool)
{ 
  if (Execution_Count_Evaluated(ipan_caller))
    return FALSE; 
  BOOL change = FALSE; 
  IPA_NODE_SECTION_INFO* si_caller = ipan_caller->Section_Annot();
  DYN_ARRAY<SUMMARY_VALUE>* sv_caller = si_caller->Get_value();
  DYN_ARRAY<SUMMARY_EXPR>* sx_caller = si_caller->Get_expr();
  if (Get_Trace(TP_IPA, IPA_TRACE_EXCOST)) {
    fprintf(stdout, "BEFORE SUBSTITUTION: \n");
    Print_Exprs(stdout, sv_caller, sx_caller);
  }
  IPA_SUCC_ITER edge_iter(IPA_Call_Graph, ipan_caller);
  for (edge_iter.First(); !edge_iter.Is_Empty(); edge_iter.Next()) {
    IPA_EDGE* ipa_edge = edge_iter.Current_Edge();
    if (ipa_edge == NULL)
      continue; 
    IPA_NODE* ipan_callee = IPA_Call_Graph->Callee(ipa_edge);
    IPA_NODE_SECTION_INFO* si_callee = ipan_callee->Section_Annot();
    SUMMARY_CALLSITE* sc = ipa_edge->Summary_Callsite();
    if (!Execution_Count_Evaluated(ipan_callee))
      continue; 
    // Find the callsite to be substituted, if any.
    INT callsite_index = sc - IPA_get_callsite_array(ipan_caller);
    INT i;

    for (i = 0; i <= sv_caller->Lastidx(); i++) {
      SUMMARY_VALUE* svv = &(*sv_caller)[i];
      if (svv->Is_callsite() 
	  && svv->Get_callsite_index() == callsite_index) 
	break; 
    } 
    if (i > sv_caller->Lastidx())
      continue; 
    change = TRUE; 
    INT callsite_value_index = i;
    DYN_ARRAY<SUMMARY_VALUE>* sv_callee = si_callee->Get_value();
    DYN_ARRAY<SUMMARY_EXPR>* sx_callee = si_callee->Get_expr();
    // Make copies of the callee 
    DYN_ARRAY<SUMMARY_VALUE>* sv_callee_copy 
      = CXX_NEW(DYN_ARRAY<SUMMARY_VALUE>(mem_pool), mem_pool);
    for (i = 0; i <= sv_callee->Lastidx(); i++) {
      SUMMARY_VALUE* svv_callee = &(*sv_callee)[i];
      sv_callee_copy->AddElement(*svv_callee);
    } 
    DYN_ARRAY<SUMMARY_EXPR>* sx_callee_copy 
      = CXX_NEW(DYN_ARRAY<SUMMARY_EXPR>(mem_pool), mem_pool);
    for (i = 0; i <= sx_callee->Lastidx(); i++) {
      SUMMARY_EXPR* sxx_callee = &(*sx_callee)[i];
      sx_callee_copy->AddElement(*sxx_callee);
    } 
    if (IPA_EX_Can_Evaluate_At_Callsite(ipan_caller, ipan_callee,
	sv_callee_copy, sc)) { 
      // Evaluate the callee at the callsite 
      IPA_EX_Evaluate_At_Callsite(ipan_caller, ipan_callee, 
	sv_callee_copy, sx_callee_copy, sc);
    } else { 
      // Substitute constants for trip counts 
      IPL_EX_Collapse_Trip_Counts(sv_callee_copy, sx_callee_copy);
    } 
    IPL_EX_Substitute(sv_caller, sx_caller, sv_callee_copy, 
      sx_callee_copy, callsite_value_index, mem_pool);
  } 
  IPL_EX_Remove_Calls(sv_caller, sx_caller);
  IPL_EX_Simplify(sv_caller, sx_caller);
  if (Get_Trace(TP_IPA, IPA_TRACE_EXCOST)) {
    fprintf(stdout, "AFTER SUBSTITUTION: \n");
    Print_Exprs(stdout, sv_caller, sx_caller);
  }
  return change; 
} 
