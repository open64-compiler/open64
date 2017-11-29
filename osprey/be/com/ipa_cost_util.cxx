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
#include <sys/elf_whirl.h>
#include <sys/types.h>
#include "defs.h"
#include "mtypes.h"
#include "access_vector.h"
#include "ipl_lno_util.h"
#include "ipl_summary.h"
#include <alloca.h>
#include "ipa_cost_util.h"
#include "be_util.h" 

#ifdef IPA_SUMMARY
#include "ipl_summary.h"
#else 
#include "ipa_trace.h"
#endif

// Forward declaration.

static INT IPL_EX_Copy_Expr_Tree(DYN_ARRAY<SUMMARY_VALUE>* sv,
                                 DYN_ARRAY<SUMMARY_EXPR>* sx,
                                 INT sx_old_index);

//-----------------------------------------------------------------------
// NAME: IPL_EX_New_Constant
// FUNCTION: Add a new integer constant of value 'constant_value' to 'sv'.
//   Return the index of the newly created VALUE. 
//-----------------------------------------------------------------------

extern INT IPL_EX_New_Constant(DYN_ARRAY<SUMMARY_VALUE>* sv, 
			       INT64 constant_value) 
{ 
  INT sv_index = sv->Newidx();
  SUMMARY_VALUE* svv = &(*sv)[sv_index];
  svv->Set_int_const();
  svv->Set_int_const_value(constant_value);
  svv->Set_mtype(MTYPE_I4);
  svv->Clear_is_addr_of();
  svv->Clear_is_trip_count();
  return sv_index; 
} 

//-----------------------------------------------------------------------
// NAME: IPL_EX_New_Value_Expr
// FUNCTION: Create a new EXPR in 'sx' which has the value of 'sv_index' 
//   in the corresponding DYN_ARRAY<SUMMARY_VALUE>.  Return the index of
//   the newly created EXPR. 
//-----------------------------------------------------------------------

extern INT IPL_EX_New_Value_Expr(DYN_ARRAY<SUMMARY_EXPR>* sx,
                                 INT sv_index)
{
  INT index = sx->Newidx();
  SUMMARY_EXPR* sxx = &(*sx)[index];
  sxx->Clear_is_trip_count();
  sxx->Set_has_const_operand();
  sxx->Set_const_value((INT64) 0);
  TYPE_ID type = MTYPE_I4;
  OPCODE opcode = OPCODE_make_op(OPR_ADD, type, MTYPE_V);
  sxx->Set_opcode(opcode);
  sxx->Set_expr_value(0);
  sxx->Set_node_index(0, sv_index);
  sxx->Set_mtype(type);
  return index;
}

//-----------------------------------------------------------------------
// NAME: IPL_EX_New_Expr_Expr
// FUNCTION: Create a new EXPR which applies the operator 'opr' to the 
//   EXPRs at indices 'sx_index_one' and 'sx_index_two'.  Return the 
//   index of the newly created EXPR.  
//-----------------------------------------------------------------------

extern INT IPL_EX_New_Expr_Expr(DYN_ARRAY<SUMMARY_EXPR>* sx,
                                OPERATOR opr,
                                INT sx_index_one,
                                INT sx_index_two)
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
  sxx->Set_node_index(0, sx_index_one);
  sxx->Set_node_index(1, sx_index_two);
  sxx->Set_mtype(type);
  return index;
}

//-----------------------------------------------------------------------
// NAME: IPL_EX_Copy_Value
// FUNCTION: Create a new VALUE for 'sv' which is a copy of the 
//   'sv_old_index'th VALUE in 'sv'.  Return the index of the newly created 
//   VALUE. 
//-----------------------------------------------------------------------

static INT IPL_EX_Copy_Value(DYN_ARRAY<SUMMARY_VALUE>* sv,
                             INT sv_old_index)
{
  INT sv_new_index = sv->Newidx();
  SUMMARY_VALUE* svv_old = &(*sv)[sv_old_index];
  SUMMARY_VALUE* svv_new = &(*sv)[sv_new_index];
  BCOPY(svv_old, svv_new, sizeof(SUMMARY_VALUE));
  return sv_new_index;
}

//-----------------------------------------------------------------------
// NAME: IPL_EX_Copy_Expr
// FUNCTION: Create a new EXPR for 'sx' which is a copy of the 
//   'sx_old_index'th EXPR in 'sx'.  Return the index of the newly created 
//   EXPR. 
//-----------------------------------------------------------------------

static INT IPL_EX_Copy_Expr(DYN_ARRAY<SUMMARY_EXPR>* sx,
                            INT sx_old_index)
{
  INT sx_new_index = sx->Newidx();
  SUMMARY_EXPR* sxx_old = &(*sx)[sx_old_index];
  SUMMARY_EXPR* sxx_new = &(*sx)[sx_new_index];
  BCOPY(sxx_old, sxx_new, sizeof(SUMMARY_EXPR));
  return sx_new_index;
}

//-----------------------------------------------------------------------
// NAME: IPL_EX_Set_Expr_Index
// FUNCTION: Create either a new VALUE or a new EXPR as appropriate which 
//   is a copy of the 'kid'th part of the 'sx_old_index'th EXPR in the 
//   ('sv','sx') pair.  Return the index of the newly added VALUE or EXPR. 
//-----------------------------------------------------------------------

static INT IPL_EX_Set_Expr_Index(DYN_ARRAY<SUMMARY_VALUE>* sv,
                                 DYN_ARRAY<SUMMARY_EXPR>* sx,
				 INT sx_old_index, 
                                 INT kid)
{
  SUMMARY_EXPR* sxx_old = &(*sx)[sx_old_index];
  if (sxx_old->Is_expr_value(kid)) {
    INT sv_old_index = sxx_old->Get_node_index(kid);
    INT sv_new_index = IPL_EX_Copy_Value(sv, sv_old_index);
    return sv_new_index; 
  } else if (sxx_old->Is_expr_expr(kid)) {
    INT sxx_old_index = sxx_old->Get_node_index(kid);
    INT sxx_new_index = IPL_EX_Copy_Expr_Tree(sv, sx, sxx_old_index);
    return sxx_new_index; 
  } else {
    FmtAssert(FALSE,
      ("IPL_EX_Set_Expr_Index: Not handling this case yet"));
    return -1; 
  }
}

//-----------------------------------------------------------------------
// NAME: IPL_EX_Copy_Expr_Tree
// FUNCTION: Copy the 'sx_old_index'th EXPR in the ('sv','sx') pair.  
//   Return the index of the newly created EXPR in 'sx'. 
//-----------------------------------------------------------------------

static INT IPL_EX_Copy_Expr_Tree(DYN_ARRAY<SUMMARY_VALUE>* sv,
                                 DYN_ARRAY<SUMMARY_EXPR>* sx,
                                 INT sx_old_index)
{
  SUMMARY_EXPR* sxx_old = &(*sx)[sx_old_index];
  INT sx_expr_one = -1;
  INT sx_expr_two = -1;
  INT sx_new_index = -1;
  if (sxx_old->Has_const_operand()) {
    sx_expr_one = IPL_EX_Set_Expr_Index(sv, sx, sx_old_index, 0);
    sx_new_index = IPL_EX_Copy_Expr(sx, sx_old_index);
    SUMMARY_EXPR* sxx_new = &(*sx)[sx_new_index];
    sxx_new->Set_node_index(0, sx_expr_one);
  } else {
    sx_expr_one = IPL_EX_Set_Expr_Index(sv, sx, sx_old_index, 0);
    sx_expr_two = IPL_EX_Set_Expr_Index(sv, sx, sx_old_index, 1);
    sx_new_index = IPL_EX_Copy_Expr(sx, sx_old_index);
    SUMMARY_EXPR* sxx_new = &(*sx)[sx_new_index];
    sxx_new->Set_node_index(0, sx_expr_one);
    sxx_new->Set_node_index(1, sx_expr_two);
  }
  return sx_new_index;
}

//-----------------------------------------------------------------------
// NAME: SUMMARY_EXPR::Equal 
// FUNCTION: Return TRUE if the SUMMARY_VALUE 'this' is equal to the 
//   SUMMARY_VALUE 'sv'.  Return FALSE otherwise. 
//-----------------------------------------------------------------------

BOOL SUMMARY_VALUE::Equal(SUMMARY_VALUE* sv)
{ 
  if (Get_mtype() != sv->Get_mtype())
    return FALSE; 
  if (Target_mtype() != sv->Target_mtype())
    return FALSE; 
  if (Get_const_type() != sv->Get_const_type())
    return FALSE; 
  if (Is_int_const()) { 
    if (Get_int_const_value() != sv->Get_int_const_value())
      return FALSE; 
  } else if (Is_two_consts()) { 
    if (Get_first_of_two_values() != sv->Get_first_of_two_values())
      return FALSE; 
    if (Get_second_of_two_values() != sv->Get_second_of_two_values())
      return FALSE; 
  } else if (Is_const_st()) { 
    if (Get_const_st_idx() != sv->Get_const_st_idx())
      return FALSE; 
    if (Get_tcon_idx() != sv->Get_tcon_idx())
      return FALSE; 
  } else if (Is_formal()) { 
    if (Get_formal_index() != sv->Get_formal_index())
      return FALSE; 
  } else if (Is_global()) { 
    if (Is_global_st_idx()) { 
      if (Get_global_st_idx() != sv->Get_global_st_idx())
          return FALSE;
    } else { 
      if (Get_global_index() != sv->Get_global_index())
	return FALSE; 
      if (Get_global_index() == -1) { 
	if (Get_global_st_idx() != sv->Get_global_st_idx())
	  return FALSE; 
      } 
    } 
  } else if (Is_symbol()) { 
    if (Get_symbol_index() != sv->Get_symbol_index())
      return FALSE; 
  } else if (Is_expr()) { 
    if (Get_expr_index() != sv->Get_expr_index())
      return FALSE; 
  } else if (Is_phi()) { 
    if (Get_phi_index() != sv->Get_phi_index())
      return FALSE; 
  } else if (Is_chi()) { 
    if (Get_chi_index() != sv->Get_chi_index())
      return FALSE; 
  } else if (Is_callsite()) { 
    if (Get_callsite_index() != sv->Get_callsite_index())
      return FALSE; 
  } 
  if (Is_remove_param() != sv->Is_remove_param())
    return FALSE; 
  if (Is_convertible_to_global() != sv->Is_convertible_to_global())
    return FALSE; 
  return TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: SUMMARY_EXPR::Equal_Node
// FUNCTION: Return TRUE if the 'kid'th part of the SUMMARY_EXPR 'this' 
//   is equal to the 'kid'th part of SUMMARY_EXPR 'sx'.  Return FALSE 
//   otherwise. 
//-----------------------------------------------------------------------

BOOL SUMMARY_EXPR::Equal_Node(INT kid, 
			      SUMMARY_EXPR* sx)
{
  if (Is_expr_value(kid) != sx->Is_expr_value(kid))
    return FALSE; 
  if (Is_expr_phi(kid) != sx->Is_expr_phi(kid))
    return FALSE; 
  if (Is_expr_expr(kid) != sx->Is_expr_expr(kid))
    return FALSE; 
  if (Is_expr_chi(kid) != sx->Is_expr_chi(kid))
    return FALSE; 
  if (Get_node_index(kid) != sx->Get_node_index(kid))
    return FALSE; 
  return TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: SUMMARY_EXPR::Equal 
// FUNCTION: Return TRUE if the SUMMARY_EXPR 'this' is equal to the 
//   SUMMARY_EXPR 'sx'.  Return FALSE otherwise. 
//-----------------------------------------------------------------------

BOOL SUMMARY_EXPR::Equal(SUMMARY_EXPR* sx)
{ 
  if (Has_const_operand() != sx->Has_const_operand())
    return FALSE; 
  if (Is_trip_count() != sx->Is_trip_count())
    return FALSE;
  if (Get_kid() != sx->Get_kid())
    return FALSE; 
  if (Is_expr_unknown() != sx->Is_expr_unknown())
    return FALSE; 
  if (Get_mtype() != sx->Get_mtype())
    return FALSE; 
  if (Get_opcode() != sx->Get_opcode())
    return FALSE; 
  if (Has_const_operand()) { 
    if (!Equal_Node(0, sx))
      return FALSE; 
    if (Get_const_value() != sx->Get_const_value())
      return FALSE; 
  } else { 
    if (!Equal_Node(0, sx))
      return FALSE; 
    if (!Equal_Node(1, sx))
      return FALSE; 
  } 
  return TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: Substitute_Expr
// FUNCTION: Substitute the EXPR index 'expr_new_index' for the EXPR index
//   'expr_old_index' in 'sx'.
//-----------------------------------------------------------------------

static void Substitute_Expr(DYN_ARRAY<SUMMARY_EXPR>* sx, 
			    INT expr_old_index,  
			    INT expr_new_index)
{ 
  SUMMARY_EXPR* sxx_old = &(*sx)[expr_old_index];
  SUMMARY_EXPR* sxx_new = &(*sx)[expr_new_index];
  if (sxx_old->Is_trip_count())
    sxx_new->Set_is_trip_count();
  for (INT i = 0; i <= sx->Lastidx(); i++) { 
    SUMMARY_EXPR* sxx = &(*sx)[i];
    if (sxx->Has_const_operand()) { 
      if (sxx->Is_expr_expr(0) && sxx->Get_node_index(0) == expr_old_index)
	sxx->Set_node_index(0, expr_new_index);
    } else { 
      if (sxx->Is_expr_expr(0) && sxx->Get_node_index(0) == expr_old_index)
	sxx->Set_node_index(0, expr_new_index);
      if (sxx->Is_expr_expr(1) && sxx->Get_node_index(1) == expr_old_index)
	sxx->Set_node_index(1, expr_new_index);
    } 
  } 
} 

//-----------------------------------------------------------------------
// NAME: Eliminate_Expr
// FUNCTION: Eliminate all occurrences of 'expr_index' in 'sx'. 
// NOTE: Shrink the 'sx' appropriately. 
//-----------------------------------------------------------------------

static void Eliminate_Expr(DYN_ARRAY<SUMMARY_EXPR>* sx, 
			   INT expr_index)
{ 
  INT i;
  
  for (i = 0; i <= sx->Lastidx(); i++) {
    SUMMARY_EXPR* sxx = &(*sx)[i];
    if (sxx->Has_const_operand()) { 
      if (sxx->Is_expr_expr(0) && sxx->Get_node_index(0) > expr_index)
	sxx->Set_node_index(0, sxx->Get_node_index(0) - 1);
    } else { 
      if (sxx->Is_expr_expr(0) && sxx->Get_node_index(0) > expr_index)
        sxx->Set_node_index(0, sxx->Get_node_index(0) - 1);
      if (sxx->Is_expr_expr(1) && sxx->Get_node_index(1) > expr_index)
        sxx->Set_node_index(1, sxx->Get_node_index(1) - 1);
    } 
  } 

  for (i = expr_index + 1; i <= sx->Lastidx(); i++) { 
    SUMMARY_EXPR* sxx_old = &(*sx)[i]; 
    SUMMARY_EXPR* sxx_new = &(*sx)[i-1]; 
    BCOPY(sxx_old, sxx_new, sizeof(SUMMARY_EXPR));
  } 

  sx->Decidx();
} 

//-----------------------------------------------------------------------
// NAME: Substitute_Expr_Value
// FUNCTION: Substitute all occurrences of EXPR index 'expr_old_index' 
//   with VALUE index 'value_new_index' in the pair ('sv','sx'). 
//-----------------------------------------------------------------------

static void Substitute_Expr_Value(DYN_ARRAY<SUMMARY_VALUE>* sv,
				  DYN_ARRAY<SUMMARY_EXPR>* sx, 
			          INT expr_old_index,  
			          INT value_new_index)
{ 
  SUMMARY_EXPR* sxx = &(*sx)[expr_old_index];
  SUMMARY_VALUE* svv = &(*sv)[value_new_index];
  if (sxx->Is_trip_count())
    svv->Set_is_trip_count();
  for (INT i = 0; i <= sx->Lastidx(); i++) { 
    SUMMARY_EXPR* sxx = &(*sx)[i];
    if (sxx->Has_const_operand()) { 
      if (sxx->Is_expr_expr(0) && sxx->Get_node_index(0) == expr_old_index) {
        sxx->Set_expr_value(0);
	sxx->Set_node_index(0, value_new_index);
      }
    } else { 
      if (sxx->Is_expr_expr(0) && sxx->Get_node_index(0) == expr_old_index) {
        sxx->Set_expr_value(0);
	sxx->Set_node_index(0, value_new_index);
      }
      if (sxx->Is_expr_expr(1) && sxx->Get_node_index(1) == expr_old_index) {
        sxx->Set_expr_value(1);
	sxx->Set_node_index(1, value_new_index);
      }
    } 
  } 
} 

//-----------------------------------------------------------------------
// NAME: Substitute_Value
// FUNCTION: Substitute all occurrences of VALUE index 'old_value_index' 
//   with VALUE index 'new_value_index' in the pair ('sv','sx').
//-----------------------------------------------------------------------

static void Substitute_Value(DYN_ARRAY<SUMMARY_VALUE>* sv,
			     DYN_ARRAY<SUMMARY_EXPR>* sx,
			     INT old_value_index,
			     INT new_value_index)
{ 
  SUMMARY_VALUE* svv_old = &(*sv)[old_value_index];
  SUMMARY_VALUE* svv_new = &(*sv)[new_value_index];
  if (svv_old->Is_trip_count())
    svv_new->Set_is_trip_count();
  for (INT i = 0; i <= sx->Lastidx(); i++) { 
    SUMMARY_EXPR* sxx = &(*sx)[i];
    if (sxx->Is_expr_value(0) && sxx->Get_node_index(0) == old_value_index)
      sxx->Set_node_index(0, new_value_index);
    if (sxx->Is_expr_value(1) && sxx->Get_node_index(1) == old_value_index)
      sxx->Set_node_index(1, new_value_index);
  } 
} 

//-----------------------------------------------------------------------
// NAME: IPL_EX_Eliminate_Value
// FUNCTION: Eliminate all occurrences of the VALUE index 'value_index'
//   for the pair ('sv','sx'), shrinking the size of 'sv' and 'sx' 
//   appropriately. 
//-----------------------------------------------------------------------

extern void IPL_EX_Eliminate_Value(DYN_ARRAY<SUMMARY_VALUE>* sv,
                                   DYN_ARRAY<SUMMARY_EXPR>* sx,
                                   INT value_index)
{
  INT i;
  
  for (i = value_index + 1; i <= sv->Lastidx(); i++) {
    SUMMARY_VALUE* svv_old = &(*sv)[i];
    SUMMARY_VALUE* svv_new = &(*sv)[i-1];
    BCOPY(svv_old, svv_new, sizeof(SUMMARY_VALUE));
  }

  sv->Decidx();
  for (i = 0; i <= sx->Lastidx(); i++) {
    SUMMARY_EXPR* sxx = &(*sx)[i];
    if (sxx->Has_const_operand()) {
      if (sxx->Is_expr_value(0))
        if (sxx->Get_node_index(0) > value_index)
          sxx->Set_node_index(0, sxx->Get_node_index(0) - 1);
    } else {
      if (sxx->Is_expr_value(0))
        if (sxx->Get_node_index(0) > value_index)
          sxx->Set_node_index(0, sxx->Get_node_index(0) - 1);
      if (sxx->Is_expr_value(1))
        if (sxx->Get_node_index(1) > value_index)
          sxx->Set_node_index(1, sxx->Get_node_index(1) - 1);
    }
  }
}

//-----------------------------------------------------------------------
// NAME: IPL_EXS_Sort_Exprs
// FUNCTION: Sort the EXPRs in 'sx' so that each EXPR refers only to 
//   EXPRs whose indices are lower valued than itself. 
//-----------------------------------------------------------------------

static void IPL_EXS_Sort_Exprs(DYN_ARRAY<SUMMARY_VALUE>* sv,
			       DYN_ARRAY<SUMMARY_EXPR>* sx) 
{
#ifdef IPA_SUMMARY
  if (Get_Trace(TP_IPL, TT_IPL_SIMPLIFY)) {
#else 
  if (Get_Trace(TP_IPA, IPA_TRACE_SIMPLIFY)) { 
#endif
    fprintf(stdout, "BEFORE SORTING EXPRESSIONS: \n");
    Print_Exprs(stdout, sv, sx);
  } 

  INT index_count = sx->Lastidx() + 1; 
  INT* new_index = (INT*) alloca(index_count * sizeof(INT));
  INT i;

  for (i = 0; i <= sx->Lastidx(); i++)
    new_index[i] = -1; 

  INT new_value = 0; 
  INT start_index = -1;

  for (i = 0; i <= sx->Lastidx(); i++) {
    SUMMARY_EXPR* sxx = &(*sx)[i];
    if (sxx->Has_const_operand()) {
      if (!sxx->Is_expr_expr(0))
	new_index[i] = new_value++;
    } else { 
      if (!sxx->Is_expr_expr(0) && !sxx->Is_expr_expr(1))
	new_index[i] = new_value++; 
    }  
    if (start_index == -1 && new_index[i] == -1)
      start_index = i;
  }

  while (new_value < index_count) {  
    INT my_start_index = start_index;
    start_index = -1;
    for (i = my_start_index; i <= sx->Lastidx(); i++) {
      if (new_index[i] == -1) {
        SUMMARY_EXPR* sxx = &(*sx)[i];
	if (sxx->Has_const_operand()) { 
	  if (sxx->Is_expr_expr(0) 
	      && new_index[sxx->Get_node_index(0)] != -1) {
	    sxx->Set_node_index(0, new_index[sxx->Get_node_index(0)]);
	    new_index[i] = new_value++; 
	  }
	} else { 
          if ((!sxx->Is_expr_expr(0)
	      || new_index[sxx->Get_node_index(0)] != -1)
	      && (!sxx->Is_expr_expr(1)
	      || new_index[sxx->Get_node_index(1)] != -1)) {
	    if (sxx->Is_expr_expr(0))
	      sxx->Set_node_index(0, new_index[sxx->Get_node_index(0)]);
	    if (sxx->Is_expr_expr(1))
	      sxx->Set_node_index(1, new_index[sxx->Get_node_index(1)]);
	    new_index[i] = new_value++;
	  }
	}
	if (start_index == -1 && new_index[i] == -1)
	  start_index = i;
      }
    }
  } 

  SUMMARY_EXPR* new_exprs 
    = (SUMMARY_EXPR*) alloca(index_count * sizeof(SUMMARY_EXPR));

  for (i = 0; i <= sx->Lastidx(); i++) {
    SUMMARY_EXPR* sxx = &(*sx)[i];
    BCOPY(sxx, &new_exprs[new_index[i]], sizeof(SUMMARY_EXPR));
  }

  sx->Resetidx();
  for (i = 0; i < index_count; i++)
    sx->AddElement(new_exprs[i]);
} 

//-----------------------------------------------------------------------
// NAME: IPL_EXS_Too_Complicated
// FUNCTION: Return TRUE if the number of VALUEs or the number of EXPRs 
//   in the ('sv','sx') pair are greater than 'multiplier' times as many
//   as the (MAX_VALUE_COUNT, MAX_EXPR_COUNT).
//-----------------------------------------------------------------------

extern BOOL IPL_EXS_Too_Complicated(DYN_ARRAY<SUMMARY_VALUE>* sv,
				    DYN_ARRAY<SUMMARY_EXPR>* sx,
				    INT multiplier)
{
  return sv->Lastidx() + 1 > multiplier * MAX_VALUE_COUNT 
    || sx->Lastidx() + 1 > multiplier * MAX_EXPR_COUNT;
} 

//-----------------------------------------------------------------------
// NAME: IPL_EXS_Chop_Down_Estimate
// FUNCTION: Replace the ('sv','sx') pair with a single constant estimate
//   which is the square of DEFAULT_TRIP_COUNT.
//-----------------------------------------------------------------------

extern INT IPL_EXS_Chop_Down_Estimate(DYN_ARRAY<SUMMARY_VALUE>* sv,
				      DYN_ARRAY<SUMMARY_EXPR>* sx)
{
  sv->Resetidx();
  sx->Resetidx();
  INT64 constant_value = DEFAULT_TRIP_COUNT * DEFAULT_TRIP_COUNT; 
  INT sv_index = IPL_EX_New_Constant(sv, constant_value);
  INT sx_index = IPL_EX_New_Value_Expr(sx, sv_index);
  return sx_index; 
} 

//-----------------------------------------------------------------------
// NAME: Find_Useless_Exprs_Traverse
// FUNCTION: Search the pair ('sv','sx') starting at EXPR index 'expr_index'
//   At the end of the seach, 'sv_used[i]' is TRUE if the i-th VALUE is 
//   used in the 'expr_index'th EXPR and 'sx_used[i]' is TRUE if the i-th
//   VALUE is used in the 'expr_index'th EXPR.  Otherwise, these bits are
//   set to FALSE. 
//-----------------------------------------------------------------------

static void Find_Useless_Exprs_Traverse(INT expr_index,
					DYN_ARRAY<SUMMARY_VALUE>* sv,
			                DYN_ARRAY<SUMMARY_EXPR>* sx,
			                BOOL* sv_used, 
			                BOOL* sx_used)
{ 
  SUMMARY_EXPR* sxx = &(*sx)[expr_index]; 
  if (sxx->Has_const_operand()) { 
    if (sxx->Is_expr_value(0)) { 
      sv_used[sxx->Get_node_index(0)] = TRUE; 
    } else if (sxx->Is_expr_expr(0)) { 
      sx_used[sxx->Get_node_index(0)] = TRUE; 
      Find_Useless_Exprs_Traverse(sxx->Get_node_index(0), 
	sv, sx, sv_used, sx_used);
    } 
  } else { 
    if (sxx->Is_expr_value(0)) {
      sv_used[sxx->Get_node_index(0)] = TRUE; 
    } else if (sxx->Is_expr_expr(0)) { 
      sx_used[sxx->Get_node_index(0)] = TRUE; 
      Find_Useless_Exprs_Traverse(sxx->Get_node_index(0), 
	sv, sx, sv_used, sx_used);
    } 
    if (sxx->Is_expr_value(1)) { 
      sv_used[sxx->Get_node_index(1)] = TRUE; 
    } else if (sxx->Is_expr_expr(1)) { 
      sx_used[sxx->Get_node_index(1)] = TRUE; 
      Find_Useless_Exprs_Traverse(sxx->Get_node_index(1), 
	sv, sx, sv_used, sx_used);
    } 
  } 
} 
    
//-----------------------------------------------------------------------
// NAME: Find_Useless_Exprs
// FUNCTION: Search the pair ('sv','sx') for useless expressions. Set 
//   'sv_used[i]' to TRUE if the i-th VALUE in 'sv' is used.  Set     
//   'sx_used[i]' to TRUE if the i-th EXPR in 'sx' is used.  Otherwise, 
//   set these bits to FALSE.  
// NOTE: The cost value respresented by the pair ('sv','sx') is repre-
//   sented by the highest index entry in 'sx'.
//-----------------------------------------------------------------------

static void Find_Useless_Exprs(DYN_ARRAY<SUMMARY_VALUE>* sv,
			       DYN_ARRAY<SUMMARY_EXPR>* sx,
			       BOOL* sv_used, 
			       BOOL* sx_used)
{
  INT expr_index = sx->Lastidx();
  sx_used[expr_index] = TRUE; 
  Find_Useless_Exprs_Traverse(expr_index, sv, sx, sv_used, sx_used);
}

//-----------------------------------------------------------------------
// NAME: IPL_EXS_Useless
// FUNCTION: Remove all of the useless expressions and values in the 
//   ('sv','sx') pair. 
//-----------------------------------------------------------------------

static void IPL_EXS_Useless(DYN_ARRAY<SUMMARY_VALUE>* sv,
                            DYN_ARRAY<SUMMARY_EXPR>* sx)
{
#ifdef IPA_SUMMARY
  if (Get_Trace(TP_IPL, TT_IPL_SIMPLIFY)) {
#else 
  if (Get_Trace(TP_IPA, IPA_TRACE_SIMPLIFY)) { 
#endif
    fprintf(stdout, "BEFORE USELESS EXPR ELIMINATION: \n");
    Print_Exprs(stdout, sv, sx);
  }
  BOOL* sv_used = (BOOL*) alloca((sv->Lastidx() + 1) * sizeof(BOOL));
  BOOL* sx_used = (BOOL*) alloca((sx->Lastidx() + 1) * sizeof(BOOL));
  INT i;

  for (i = 0; i <= sv->Lastidx(); i++)
    sv_used[i] = FALSE;

  for (i = 0; i <= sx->Lastidx(); i++)
    sx_used[i] = FALSE;

  Find_Useless_Exprs(sv, sx, sv_used, sx_used);

  for (i = sx->Lastidx(); i >= 0; i--)
    if (!sx_used[i])
      Eliminate_Expr(sx, i);

  for (i = sv->Lastidx(); i >= 0; i--)
    if (!sv_used[i])
      IPL_EX_Eliminate_Value(sv, sx, i);
}

//-----------------------------------------------------------------------
// NAME: IPL_EX_Value_Evaluate
// FUNCTION: If the 'sv_index'th VALUE in 'sv' evaluates to a constant, 
//   return the value of that constant.  Otherwise, set 'valid' to FALSE
//   and return -1.  
//-----------------------------------------------------------------------

static INT64 IPL_EX_Value_Evaluate(DYN_ARRAY<SUMMARY_VALUE>* sv,
			           INT sv_index,
				   BOOL* valid)
{
  SUMMARY_VALUE* svv = &(*sv)[sv_index]; 
  if (svv->Is_int_const()) 
    return svv->Get_int_const_value();
  if (svv->Is_const_st()) {
    INT64 value = -1; 
    BOOL ok = St_Idx_Is_Intconst(svv->Get_const_st_idx(), &value);
    FmtAssert(ok, ("IPL_EX_Value_Evaluate: Expected INT int_const"));
    return value; 
  } 
  *valid = FALSE; 
  return -1; 
} 

//-----------------------------------------------------------------------
// NAME: IPL_EX_Expr_Evaluate
// FUNCTION: If the 'sx_index'th EXPR in the pair ('sv','sx') evaluates 
//   to a constant, return the value of that constant.  Otherwise, set 
//   'valid' to FALSE and return -1. 
//-----------------------------------------------------------------------

static INT64 IPL_EX_Expr_Evaluate(DYN_ARRAY<SUMMARY_VALUE>* sv,
			          DYN_ARRAY<SUMMARY_EXPR>* sx,
			          INT sx_index,
				  BOOL* valid)
{
  SUMMARY_EXPR* sxx = &(*sx)[sx_index];
  INT64 value_one = -1; 
  INT64 value_two = -1; 
  if (sxx->Has_const_operand()) { 
    if (sxx->Is_expr_value(0)) {
      INT sv_index = sxx->Get_node_index(0);
      value_one = IPL_EX_Value_Evaluate(sv, sv_index, valid);
      value_two = sxx->Get_const_value(); 
    } else if (sxx->Is_expr_expr(0)) { 
      INT sx_index = sxx->Get_node_index(0);
      value_one = IPL_EX_Expr_Evaluate(sv, sx, sx_index, valid);
      value_two = sxx->Get_const_value();    
    } else { 
      *valid = FALSE;    
      return -1; 
    } 
  } else { 
    if (sxx->Is_expr_value(0)) {
      INT sv_index = sxx->Get_node_index(0);
      value_one = IPL_EX_Value_Evaluate(sv, sv_index, valid);
    } else if (sxx->Is_expr_expr(0)) { 
      INT sx_index = sxx->Get_node_index(0);
      value_one = IPL_EX_Expr_Evaluate(sv, sx, sx_index, valid);
    } else { 
      *valid = FALSE;    
      return -1; 
    } 
    if (sxx->Is_expr_value(1)) {
      INT sv_index = sxx->Get_node_index(1);
      value_two= IPL_EX_Value_Evaluate(sv, sv_index, valid);
    } else if (sxx->Is_expr_expr(1)) { 
      INT sx_index = sxx->Get_node_index(1);
      value_two = IPL_EX_Expr_Evaluate(sv, sx, sx_index, valid);
    } else { 
      *valid = FALSE;    
      return -1; 
    } 
  } 
  switch (OPCODE_operator(sxx->Get_opcode())) { 
  case OPR_ADD: 
    return value_one + value_two;
  case OPR_SUB: 
    return value_one - value_two;
  case OPR_MPY: 
    return value_one * value_two; 
  case OPR_DIV: 
    return value_one / value_two; 
  default: 
    *valid = FALSE; 
    return -1; 
  } 
}  

//-----------------------------------------------------------------------
// NAME: IPL_EX_Reassociate
// FUNCTION: Simplify the pair ('sv','sx') by reassociating expressions 
//   of the form (sx + value1) + value2 to get sx + (value1 + value2). 
//   Also change (sx - value1) + value2 to sx + (value2 - value1), and 
//   apply other similar used reassociations. 
// NOTE: This is particularly useful because many trip count expressions 
//   initially look like (N - 1) + 1. 
//-----------------------------------------------------------------------

static void IPL_EXS_Reassociate(DYN_ARRAY<SUMMARY_VALUE>* sv,
                                DYN_ARRAY<SUMMARY_EXPR>* sx)
{
#ifdef IPA_SUMMARY
  if (Get_Trace(TP_IPL, TT_IPL_SIMPLIFY)) {
#else 
  if (Get_Trace(TP_IPA, IPA_TRACE_SIMPLIFY)) { 
#endif
    fprintf(stdout, "BEFORE REASSOCIATION: \n");
    Print_Exprs(stdout, sv, sx);
  }
  for (INT i = 0; i <= sx->Lastidx(); i++) { 
    INT expr_index_one = i; 
    SUMMARY_EXPR* sxx_one = &(*sx)[expr_index_one];
    if (sxx_one->Has_const_operand() || !sxx_one->Is_expr_expr(0)
        || !sxx_one->Is_expr_expr(1))
      continue; 
    if (OPCODE_operator(sxx_one->Get_opcode()) != OPR_ADD
        && OPCODE_operator(sxx_one->Get_opcode()) != OPR_SUB)
      continue; 
    BOOL is_cst_left = TRUE; 
    BOOL is_cst_right = TRUE; 
    INT sx_left = sxx_one->Get_node_index(0);
    INT sx_right = sxx_one->Get_node_index(1);
    INT64 cst_left = IPL_EX_Expr_Evaluate(sv, sx, sx_left, &is_cst_left);
    INT64 cst_right = IPL_EX_Expr_Evaluate(sv, sx, sx_right, &is_cst_right);
    if (!is_cst_left && !is_cst_right || is_cst_left && is_cst_right)
      continue; 
    sxx_one = &(*sx)[expr_index_one];
    INT64 constant_value = 0; 
    SUMMARY_EXPR* sxx_two = NULL; 
    INT sxx_two_index = -1; 
    if (is_cst_left) {
      constant_value += OPCODE_operator(sxx_one->Get_opcode()) == OPR_ADD
        ? cst_left : -cst_left; 
      sxx_two_index = sx_right; 
      sxx_two = &(*sx)[sx_right];
    } else { 
      constant_value += OPCODE_operator(sxx_one->Get_opcode()) == OPR_ADD
        ? cst_right : -cst_right; 
      sxx_two_index = sx_left; 
      sxx_two = &(*sx)[sx_left]; 
    } 
    if (sxx_two->Has_const_operand() || !sxx_two->Is_expr_expr(0)
        || !sxx_two->Is_expr_expr(1))
      continue;
    if (OPCODE_operator(sxx_two->Get_opcode()) != OPR_ADD
        && OPCODE_operator(sxx_two->Get_opcode()) != OPR_SUB)
      continue; 
    is_cst_left = TRUE;
    is_cst_right = TRUE;
    sx_left = sxx_two->Get_node_index(0);
    sx_right = sxx_two->Get_node_index(1);
    cst_left = IPL_EX_Expr_Evaluate(sv, sx, sx_left, &is_cst_left);
    cst_right = IPL_EX_Expr_Evaluate(sv, sx, sx_right, &is_cst_right);
    if (!is_cst_left && !is_cst_right || is_cst_left && is_cst_right)
      continue; 
    sxx_two = &(*sx)[sxx_two_index];
    if (OPCODE_operator(sxx_two->Get_opcode()) == OPR_SUB && !is_cst_right)
      continue; 
    INT sx_expr = -1; 
    if (is_cst_left) {
      constant_value += OPCODE_operator(sxx_two->Get_opcode()) == OPR_ADD
        ? cst_left : -cst_left; 
      sx_expr = sx_right; 
    } else { 
      constant_value += OPCODE_operator(sxx_two->Get_opcode()) == OPR_ADD
        ? cst_right : -cst_right; 
      sx_expr = sx_left; 
    } 
    INT sx_expr_new = IPL_EX_Copy_Expr_Tree(sv, sx, sx_expr);
    INT sv_index = IPL_EX_New_Constant(sv, constant_value);
    INT sx_index = IPL_EX_New_Value_Expr(sx, sv_index);
    sxx_one = &(*sx)[expr_index_one];
    sxx_one->Set_node_index(0, sx_expr_new);
    sxx_one->Set_node_index(1, sx_index);
    if (IPL_EXS_Too_Complicated(sv, sx, 2)) { 
      IPL_EXS_Sort_Exprs(sv, sx);
      IPL_EXS_Useless(sv, sx);
      if (IPL_EXS_Too_Complicated(sv, sx, 1)) {
	IPL_EXS_Chop_Down_Estimate(sv, sx);
	return;
      }
    }
  } 
  IPL_EXS_Sort_Exprs(sv, sx);
  IPL_EXS_Useless(sv, sx);
}

//-----------------------------------------------------------------------
// NAME: IPL_EXS_Outer_Fold
// FUNCTION: Simplify the pair ('sv','sx') by replacing EXPRs which 
//   evaluate to a constant VALUE with a canonical expr (VALUE + 0).
//-----------------------------------------------------------------------

static void IPL_EXS_Outer_Fold(DYN_ARRAY<SUMMARY_VALUE>* sv,
                               DYN_ARRAY<SUMMARY_EXPR>* sx)
{
#ifdef IPA_SUMMARY
  if (Get_Trace(TP_IPL, TT_IPL_SIMPLIFY)) {
#else 
  if (Get_Trace(TP_IPA, IPA_TRACE_SIMPLIFY)) { 
#endif
    fprintf(stdout, "BEFORE OUTER FOLDING: \n");
    Print_Exprs(stdout, sv, sx);
  }
  for (INT i = sx->Lastidx(); i >= 0; i--) {
    BOOL valid = TRUE; 
    INT64 const_result = -1; 
    SUMMARY_EXPR* sxx = &(*sx)[i];
    if ((*sx)[i].Has_const_operand()) {
      if ((*sx)[i].Is_expr_value(0)) { 
	INT sv_index = (*sx)[i].Get_node_index(0);
        INT64 const_one = IPL_EX_Value_Evaluate(sv, sv_index, &valid);
        const_result = const_one + (*sx)[i].Get_const_value();
      } else if ((*sx)[i].Is_expr_expr(0)) { 
	INT sx_index = (*sx)[i].Get_node_index(0);
        INT64 const_one = IPL_EX_Expr_Evaluate(sv, sx, sx_index, &valid);
        const_result = const_one + (*sx)[i].Get_const_value();
      } 
    } else { 
      INT64 const_one = -1; 
      INT64 const_two = -1; 
      if ((*sx)[i].Is_expr_value(0)) { 
        INT sv_index = (*sx)[i].Get_node_index(0);
        const_one = IPL_EX_Value_Evaluate(sv, sv_index, &valid);
      } else if ((*sx)[i].Is_expr_expr(0)) { 
	INT sx_index = (*sx)[i].Get_node_index(0);
	const_one = IPL_EX_Expr_Evaluate(sv, sx, sx_index, &valid);
      } else { 
	valid = FALSE; 
      } 
      if (valid) { 
	if ((*sx)[i].Is_expr_value(1)) { 
	  INT sv_index = (*sx)[i].Get_node_index(1);
	  const_two = IPL_EX_Value_Evaluate(sv, sv_index, &valid);
	} else if ((*sx)[i].Is_expr_expr(1)) { 
	  INT sx_index = (*sx)[i].Get_node_index(1);
	  const_two = IPL_EX_Expr_Evaluate(sv, sx, sx_index, &valid);
	} else { 
	  valid = FALSE; 
	} 
	if (valid) { 
	  switch (OPCODE_operator((*sx)[i].Get_opcode())) {
	  case OPR_ADD:
	    const_result = const_one + const_two;
	    break;
	  case OPR_SUB:
	    const_result = const_one - const_two;
	    break;
	  case OPR_MPY:
	    const_result = const_one * const_two;
	    break;
	  case OPR_DIV:
	    const_result = const_one / const_two;
	    break;
	  default:
	    FmtAssert(FALSE,
	      ("IPL_EXS_Outer_Fold: Unexpected operator in SUMMARY_EXPR"));
	  }
       }
     }
   } 
   if (valid) {
      INT sv_index = IPL_EX_New_Constant(sv, const_result);
      (*sx)[i].Clear_is_trip_count();
      (*sx)[i].Set_has_const_operand();
      (*sx)[i].Set_const_value((INT64) 0);
      TYPE_ID type = MTYPE_I4;
      OPCODE opcode = OPCODE_make_op(OPR_ADD, type, MTYPE_V);
      (*sx)[i].Set_opcode(opcode);
      (*sx)[i].Set_expr_value(0);
      (*sx)[i].Set_node_index(0, sv_index);
      (*sx)[i].Set_mtype(type);
    } 
  } 
  IPL_EXS_Sort_Exprs(sv, sx);
  IPL_EXS_Useless(sv, sx);
}

//-----------------------------------------------------------------------
// NAME: IPL_EXS_Eliminate_Duplicate_Values
// FUNCTION: Simplify 'sv' by replacing VALUEs which are equal to one 
//   another with a single canonical representative, and eliminating the 
//   non-canonical versions.
//-----------------------------------------------------------------------

static void IPL_EXS_Eliminate_Duplicate_Values(DYN_ARRAY<SUMMARY_VALUE>* sv,
				       DYN_ARRAY<SUMMARY_EXPR>* sx)
{
#ifdef IPA_SUMMARY
  if (Get_Trace(TP_IPL, TT_IPL_SIMPLIFY)) {
#else 
  if (Get_Trace(TP_IPA, IPA_TRACE_SIMPLIFY)) { 
#endif
    fprintf(stdout, "BEFORE ELIMINATING DUPLICATE VALUES: \n");
    Print_Exprs(stdout, sv, sx);
  }
  for (INT i = 0; i <= sv->Lastidx(); i++) { 
    SUMMARY_VALUE* sx_one = &(*sv)[i];
    for (INT j = i + 1; j <= sv->Lastidx(); j++) { 
      SUMMARY_VALUE* sx_two = &(*sv)[j]; 
      if (sx_one->Equal(sx_two)) { 
	Substitute_Value(sv, sx, j, i);
	IPL_EX_Eliminate_Value(sv, sx, j--);
      }  
    } 
  } 
} 

//-----------------------------------------------------------------------
// NAME: IPL_EXS_Eliminate_Duplicate_Exprs
// FUNCTION: Simplify 'sx' by replacing EXPRs which are equal to 
//   one another with a single canonical representative, and eliminating 
//   the non-canonical versions.
//-----------------------------------------------------------------------

static void IPL_EXS_Eliminate_Duplicate_Exprs(DYN_ARRAY<SUMMARY_VALUE>* sv,
					      DYN_ARRAY<SUMMARY_EXPR>* sx)
{
#ifdef IPA_SUMMARY
  if (Get_Trace(TP_IPL, TT_IPL_SIMPLIFY)) {
#else 
  if (Get_Trace(TP_IPA, IPA_TRACE_SIMPLIFY)) { 
#endif
    fprintf(stdout, "BEFORE ELIMINATING DUPLICATE EXPRS: \n");
    Print_Exprs(stdout, sv, sx);
  }
  for (INT i = 0; i <= sx->Lastidx(); i++) { 
    SUMMARY_EXPR* sx_one = &(*sx)[i];
    for (INT j = i + 1; j <= sx->Lastidx(); j++) { 
      SUMMARY_EXPR* sx_two = &(*sx)[j]; 
      if (sx_one->Equal(sx_two)) { 
	Substitute_Expr(sx, j, i);
	Eliminate_Expr(sx, j--);
      }  
    } 
  } 
} 

//-----------------------------------------------------------------------
// NAME: IPL_EX_Inner_Fold
// FUNCTION: Simplify the ('sv','sx') pair by folding the values of 
//   expressions that have constant value into the constant field of 
//   expressions. 
//-----------------------------------------------------------------------

static void IPL_EXS_Inner_Fold(DYN_ARRAY<SUMMARY_VALUE>* sv,
                               DYN_ARRAY<SUMMARY_EXPR>* sx)
{
#ifdef IPA_SUMMARY
  if (Get_Trace(TP_IPL, TT_IPL_SIMPLIFY)) {
#else 
  if (Get_Trace(TP_IPA, IPA_TRACE_SIMPLIFY)) { 
#endif
    fprintf(stdout, "BEFORE INNER FOLDING: \n");
    Print_Exprs(stdout, sv, sx);
  }
  for (INT i = sx->Lastidx(); i >= 0; i--) { 
    if ((*sx)[i].Has_const_operand())
      continue; 
    BOOL valid = TRUE; 
    INT64 const_value = -1; 
    if ((*sx)[i].Is_expr_value(0)) {
      INT sv_index = (*sx)[i].Get_node_index(0);
      const_value = IPL_EX_Value_Evaluate(sv, sv_index, &valid);
    } else if ((*sx)[i].Is_expr_expr(0)) { 
      INT sx_index = (*sx)[i].Get_node_index(0);
      const_value = IPL_EX_Expr_Evaluate(sv, sx, sx_index, &valid);
    } else { 
      valid = FALSE; 
    } 
    if (valid) { 
      INT index = (*sx)[i].Get_node_index(1);
      BOOL expr_value = (*sx)[i].Is_expr_value(1);
      (*sx)[i].Set_has_const_operand();
      if (expr_value)
	(*sx)[i].Set_expr_value(0);
      else 
	(*sx)[i].Set_expr_expr(0);
      (*sx)[i].Set_node_index(0, index);
      (*sx)[i].Set_const_value(const_value);
      continue; 
    }
    valid = TRUE; 
    const_value = -1; 
    if ((*sx)[i].Is_expr_value(1)) {
      INT sv_index = (*sx)[i].Get_node_index(1);
      const_value = IPL_EX_Value_Evaluate(sv, sv_index, &valid);
    } else if ((*sx)[i].Is_expr_expr(1)) { 
      INT sx_index = (*sx)[i].Get_node_index(1);
      const_value = IPL_EX_Expr_Evaluate(sv, sx, sx_index, &valid);
    } else { 
      valid = FALSE;
    } 
    if (valid) { 
      INT index = (*sx)[i].Get_node_index(0);
      BOOL expr_value = (*sx)[i].Is_expr_value(0);
      (*sx)[i].Set_has_const_operand();
      if (expr_value) 
	(*sx)[i].Set_expr_value(0);
      else 
	(*sx)[i].Set_expr_expr(0);
      (*sx)[i].Set_node_index(0, index);
      (*sx)[i].Set_const_value(const_value);
      continue; 
    }
  }
  IPL_EXS_Sort_Exprs(sv, sx);
  IPL_EXS_Useless(sv, sx);
}

//-----------------------------------------------------------------------
// NAME: IPL_EXS_Eliminate_Expr_Identities
// FUNCTION: Simplify the pair ('sv','sx') by applying the identities: 
//   sxx + 0 == sxx, 0 + sxx == sxx, sxx - 0 == sxx, sxx * 1 == sxx, 
//   1 * sxx == sxx, sxx / 1 == sxx. 
//-----------------------------------------------------------------------

static void IPL_EXS_Eliminate_Expr_Identities(DYN_ARRAY<SUMMARY_VALUE>* sv,
					      DYN_ARRAY<SUMMARY_EXPR>* sx)
{
#ifdef IPA_SUMMARY
  if (Get_Trace(TP_IPL, TT_IPL_SIMPLIFY)) {
#else 
  if (Get_Trace(TP_IPA, IPA_TRACE_SIMPLIFY)) { 
#endif
    fprintf(stdout, "BEFORE ELIMINATING EXPR IDENTITIES: \n");
    Print_Exprs(stdout, sv, sx);
  }
  for (INT i = 0; i <= sx->Lastidx(); i++) { 
    SUMMARY_EXPR* sxx = &(*sx)[i];
    if (!sxx->Has_const_operand() 
        || !sxx->Is_expr_expr(0) && !sxx->Is_expr_value(0))
      continue; 
    BOOL can_eliminate = FALSE; 
    switch (OPCODE_operator(sxx->Get_opcode())) {
    case OPR_ADD: 
      if (sxx->Get_const_value() == 0)
	can_eliminate = TRUE; 
      break; 
    case OPR_SUB: 
      if (sxx->Get_const_value() == 0)
	can_eliminate = TRUE; 
      break; 
    case OPR_MPY: 
      if (sxx->Get_const_value() == 1)
	can_eliminate = TRUE; 
      break; 
    case OPR_DIV: 
      if (sxx->Get_const_value() == 1)
	can_eliminate = TRUE; 
      break; 
    } 
    if (can_eliminate) {
      if (sxx->Is_expr_expr(0)) 
        Substitute_Expr(sx, i, sxx->Get_node_index(0));
      else if (sxx->Is_expr_value(0))
        Substitute_Expr_Value(sv, sx, i, sxx->Get_node_index(0));
    }
  } 
  IPL_EXS_Useless(sv, sx);
} 
 
//-----------------------------------------------------------------------
// NAME: IPL_EX_Collapse_Trip_Counts
// FUNCTION: Replace each reference to a trip count in the ('sv','sx') 
//   with a DEFAULT_TRIP_COUNT constant value. 
// NOTE: This is generally done when the expressions get too complicated 
//   or can't otherwise be evaluated. 
//-----------------------------------------------------------------------

extern void IPL_EX_Collapse_Trip_Counts(DYN_ARRAY<SUMMARY_VALUE>* sv,
					DYN_ARRAY<SUMMARY_EXPR>* sx)
{
  INT i;
  
  for (i = 0; i <= sv->Lastidx(); i++) { 
    SUMMARY_VALUE* svv = &(*sv)[i];
    if (svv->Is_trip_count()) { 
      svv->Set_int_const();
      svv->Set_int_const_value(DEFAULT_TRIP_COUNT);
      svv->Set_mtype(MTYPE_I4);
      svv->Clear_is_addr_of();
      svv->Clear_is_trip_count();
    } 
  } 

  for (i = 0; i <= sx->Lastidx(); i++) { 
    SUMMARY_EXPR* sxx = &(*sx)[i]; 
    if (sxx->Is_trip_count()) { 
      INT sv_index = sv->Newidx();
      SUMMARY_VALUE* svv = &(*sv)[sv_index];
      svv->Set_int_const();
      svv->Set_int_const_value(DEFAULT_TRIP_COUNT);
      svv->Set_mtype(MTYPE_I4);
      svv->Clear_is_addr_of();
      svv->Clear_is_trip_count();
      sxx->Clear_is_trip_count();
      sxx->Set_has_const_operand();
      sxx->Set_const_value((INT64) 0);
      OPCODE opcode = OPCODE_make_op(OPR_ADD, MTYPE_I4, MTYPE_V);
      sxx->Set_opcode(opcode);
      sxx->Set_expr_value(0);
      sxx->Set_node_index(0, sv_index);
      sxx->Set_mtype(MTYPE_I4);
    } 
  } 

  IPL_EXS_Useless(sv, sx);
} 

//-----------------------------------------------------------------------
// NAME: IPL_EX_Simplify
// FUNCTION: Apply a number of simple optimizations to reduce the size 
//   of the pair ('sv','sx'). 
//-----------------------------------------------------------------------

extern void IPL_EX_Simplify(DYN_ARRAY<SUMMARY_VALUE>* sv,
                            DYN_ARRAY<SUMMARY_EXPR>* sx)
{
#ifdef IPA_SUMMARY
  if (Get_Trace(TP_IPL, TT_IPL_SIMPLIFY)) {
#else 
  if (Get_Trace(TP_IPA, IPA_TRACE_SIMPLIFY)) { 
#endif
    fprintf(stdout, "BEFORE SIMPLIFICATION: \n");
    Print_Exprs(stdout, sv, sx);
  }
  if (IPL_EXS_Too_Complicated(sv, sx, 1))
    IPL_EXS_Chop_Down_Estimate(sv, sx);
  // Sort out of order expressions
  IPL_EXS_Sort_Exprs(sv, sx);
  // Mark and eliminate useless values and expressions
  IPL_EXS_Useless(sv, sx);
#ifdef Is_True_On
  Check_Exprs(sv, sx, stdout);
#endif
  // Reassociate constants, grouping them together
  IPL_EXS_Reassociate(sv, sx);
  // Fold together "const" OPR "const"
  IPL_EXS_Outer_Fold(sv, sx);
  // Eliminate duplicate SUMMARY_VALUEs
  IPL_EXS_Eliminate_Duplicate_Values(sv, sx);
  // Eliminate duplicate SUMMARY_EXPRs
  IPL_EXS_Eliminate_Duplicate_Exprs(sv, sx);
  // Fold constants into expressions
  IPL_EXS_Inner_Fold(sv, sx);
  // Eliminate useless identity exprs 
  IPL_EXS_Eliminate_Expr_Identities(sv, sx);
#ifdef Is_True_On
  Check_Exprs(sv, sx, stdout);
#endif
}

//-----------------------------------------------------------------------
// NAME: IPL_EX_Add_Value_Offsets
// FUNCTION: Adjust the indices in 'sv' by adding 'formal_offset' to each 
//   FORMAL index and 'global_offset' to each GLOBAL index. 
//-----------------------------------------------------------------------

extern void IPL_EX_Add_Value_Offsets(DYN_ARRAY<SUMMARY_VALUE>* sv,
                                     INT formal_offset,
                                     INT global_offset)
{
  for (INT i = 0; i <= sv->Lastidx(); i++) {
    SUMMARY_VALUE* svv = &(*sv)[i];
    if (svv->Is_formal()) 
      svv->Set_formal_index(formal_offset + svv->Get_formal_index());
    else if (svv->Is_global())
      svv->Set_global_index(global_offset + svv->Get_global_index());
  } 
}

//-----------------------------------------------------------------------
// NAME: IPL_EX_Add_Expr_Offsets
// FUNCTION: Adjust the indices in 'sx' by adding 'value_offset' to each 
//   VALUE index and 'expr_offset' to each EXPR index. 
//-----------------------------------------------------------------------

extern void IPL_EX_Add_Expr_Offsets(DYN_ARRAY<SUMMARY_EXPR>* sx,
                                    INT value_offset,
                                    INT expr_offset)
{
  for (INT i = 0; i <= sx->Lastidx(); i++) {
    SUMMARY_EXPR* sxx = &(*sx)[i];
    if (sxx->Has_const_operand()) {
      if (sxx->Is_expr_expr(0)) {
        sxx->Set_node_index(0, sxx->Get_node_index(0) + expr_offset);
      } else if (sxx->Is_expr_value(0)) {
        sxx->Set_node_index(0, sxx->Get_node_index(0) + value_offset);
      }
    } else {
      if (sxx->Is_expr_expr(0)) {
        sxx->Set_node_index(0, sxx->Get_node_index(0) + expr_offset);
      } else if (sxx->Is_expr_value(0)) {
        sxx->Set_node_index(0, sxx->Get_node_index(0) + value_offset);
      }
      if (sxx->Is_expr_expr(1)) {
        sxx->Set_node_index(1, sxx->Get_node_index(1) + expr_offset);
      } else if (sxx->Is_expr_value(1)) {
        sxx->Set_node_index(1, sxx->Get_node_index(1) + value_offset);
      }
    }
  }
}

//-----------------------------------------------------------------------
// NAME: Print_Exprs
// FUNCTION: Print a representation of the pair ('sv','sx') to the file 
//   'fp'.  
//-----------------------------------------------------------------------

extern void Print_Exprs(FILE* fp, 
			DYN_ARRAY<SUMMARY_VALUE>* sv,
                        DYN_ARRAY<SUMMARY_EXPR>* sx)
{
  INT i;
  
  for (i = 0; i <= sv->Lastidx(); i++) {
    SUMMARY_VALUE* svv = &(*sv)[i];
    svv->WB_Print(fp, i);
  }

  for (i = 0; i <= sx->Lastidx(); i++) {
    SUMMARY_EXPR* sxx = &(*sx)[i];
    sxx->WB_Print(fp, i);
  }
}

//-----------------------------------------------------------------------
// NAME: Check_Trip_Counts_Traverse
// FUNCTION: Check the EXPR at index 'expr_index' in the pair ('sv','sx') 
//   by setting 'sv_used[i]' to TRUE if it is used in the expression 
//   and setting 'sx_used[i]' to TRUE if it is used in the expression. 
//   Set these bits to FALSE otherwise.  
//-----------------------------------------------------------------------

static void Check_Trip_Counts_Traverse(DYN_ARRAY<SUMMARY_VALUE>* sv,
				       DYN_ARRAY<SUMMARY_EXPR>* sx,
				       BOOL sv_used[],
				       BOOL sx_used[],
				       INT expr_index)
{
  sx_used[expr_index] = TRUE;   
  SUMMARY_EXPR* sxx = &(*sx)[expr_index];
  if (sxx->Has_const_operand()) { 
    if (sxx->Is_expr_value(0)) {
      INT node_index = sxx->Get_node_index(0);
      sv_used[node_index] = TRUE; 
    } else if (sxx->Is_expr_expr(0)) { 
      INT node_index = sxx->Get_node_index(0);
      sx_used[node_index] = TRUE; 
      Check_Trip_Counts_Traverse(sv, sx, sv_used, sx_used, node_index);
    } 
  } else { 
    if (sxx->Is_expr_value(0)) {
      INT node_index = sxx->Get_node_index(0);
      sv_used[node_index] = TRUE; 
    } else if (sxx->Is_expr_expr(0)) { 
      INT node_index = sxx->Get_node_index(0);
      sx_used[node_index] = TRUE; 
      Check_Trip_Counts_Traverse(sv, sx, sv_used, sx_used, node_index);
    } 
    if (sxx->Is_expr_value(1)) {
      INT node_index = sxx->Get_node_index(1);
      sv_used[node_index] = TRUE; 
    } else if (sxx->Is_expr_expr(1)) { 
      INT node_index = sxx->Get_node_index(1);
      sx_used[node_index] = TRUE; 
      Check_Trip_Counts_Traverse(sv, sx, sv_used, sx_used, node_index);
    } 
  } 
}  

//-----------------------------------------------------------------------
// NAME: Check_Trip_Counts
// FUNCTION: Check the pair ('sv','sx') for trip count errors.  Print 
//   errors to the file 'fp'.  Return the number of errors found. 
// NOTE: An error here is any VALUE which is not either (1) constant, 
//   (2) representing a callsite, or (3) part of a trip count.  
//-----------------------------------------------------------------------

static INT Check_Trip_Counts(DYN_ARRAY<SUMMARY_VALUE>* sv,
                             DYN_ARRAY<SUMMARY_EXPR>* sx,
                             FILE* fp)
{ 
  BOOL* sv_used = (BOOL*) alloca((sv->Lastidx() + 1) * sizeof(BOOL));
  BOOL* sx_used = (BOOL*) alloca((sx->Lastidx() + 1) * sizeof(BOOL));
  INT i;
  
  for (i = 0; i <= sv->Lastidx() + 1; i++)
    sv_used[i] = FALSE;

  for (i = 0; i <= sx->Lastidx() + 1; i++)
    sx_used[i] = FALSE;

  for (i = 0; i <= sv->Lastidx(); i++) { 
    SUMMARY_VALUE* svv = &(*sv)[i];
    if (svv->Is_trip_count()) 
      sv_used[i] = TRUE; 
  } 

  for (i = 0; i <= sx->Lastidx(); i++) { 
    SUMMARY_EXPR* sxx = &(*sx)[i];
    if (sxx->Is_trip_count()) { 
      Check_Trip_Counts_Traverse(sv, sx, sv_used, sx_used, i); 
    } 
  } 

  INT error_count = 0; 
  for (i = 0; i <= sv->Lastidx(); i++) {
    SUMMARY_VALUE* svv = &(*sv)[i]; 
    if (!svv->Is_int_const() && !svv->Is_const_st() && !svv->Is_callsite()
	 && !sv_used[i]) {
      fprintf(fp, "VALUE[%d]: Not part of trip count\n", i);
      error_count++; 
    } 
  }
  return error_count; 
} 

//-----------------------------------------------------------------------
// NAME: Check_Exprs
// FUNCTION: Check the integrity of the pair ('sv','sx').  Print error 
//   messages to the file 'fp'.  Return the number of errors found. 
// NOTE: The errors we are looking for are VALUE or EXPR indices which 
//   are out of range and trip count errors as defined in Check_Trip_Counts().
//-----------------------------------------------------------------------

extern INT Check_Exprs(DYN_ARRAY<SUMMARY_VALUE>* sv,
                       DYN_ARRAY<SUMMARY_EXPR>* sx,
		       FILE* fp)
{ 
  INT error_count = 0; 
  INT sv_upper = sv->Lastidx(); 
  INT sx_upper = sx->Lastidx(); 
  for (INT i = 0; i <= sx->Lastidx(); i++) {
    SUMMARY_EXPR* sxx = &(*sx)[i];
    if (sxx->Is_expr_value(0)) 
      if (sxx->Get_node_index(0) < 0 || sxx->Get_node_index(0) > sv_upper) {
	fprintf(fp, "EXPR[%d]: EXPR[%d] INVALID \n", 
	  i, sxx->Get_node_index(0));
	error_count++;
      } 
    if (sxx->Is_expr_value(1)) 
      if (sxx->Get_node_index(1) < 0 || sxx->Get_node_index(1) > sv_upper) {
	fprintf(fp, "EXPR[%d]: EXPR[%d] INVALID \n", 
	  i, sxx->Get_node_index(1));
	error_count++;
      }
    if (sxx->Is_expr_expr(0)) 
      if (sxx->Get_node_index(0) < 0 || sxx->Get_node_index(0) > sx_upper) {
	fprintf(fp, "EXPR[%d]: VALUE[%d] INVALID \n", 
	  i, sxx->Get_node_index(0));
	error_count++;
      }
    if (sxx->Is_expr_expr(1)) 
      if (sxx->Get_node_index(1) < 0 || sxx->Get_node_index(1) > sx_upper) {
	fprintf(fp, "EXPR[%d]: VALUE[%d] INVALID \n", 
	  i, sxx->Get_node_index(1));
	error_count++;
      }
  }
  error_count += Check_Trip_Counts(sv, sx, fp);
  return error_count; 
} 

