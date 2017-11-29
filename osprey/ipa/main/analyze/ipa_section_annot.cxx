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

#include "defs.h"
#include "mempool.h"            // MEM_POOL
#include "config_ipa.h"	        // IPA_Enable_* flags
#include "ipa_cg.h"             // IPA_NODE, IPA_Call_Graph
#include "ipaa.h"		// IPAA_NODE_INFO 
#include "ipa_section_annot.h"  // array section classes
#include "ipa_section_main.h"   // Init_IPA_Print_Arrays
#include "ipa_section_prop.h"   // Trace_IPA_Sections
#include "ipa_reshape.h"        // RESHAPE
#include "reshape.h"            // RESHAPE
#include "ipa_cost.h" 		// Execution cost 

// ==========================================================
// ==========================================================
// INTERPROCEDURAL ARRAY SECTION ANALYSIS FOR PARALLELIZATION
// ==========================================================
// ==========================================================
		  
//=====================================================================
// return the position of the formal parameter from its summary symbol
//=====================================================================
static inline INT32
Formal_position(const SUMMARY_FORMAL* formal_array, 
                const SUMMARY_SYMBOL* formal_symbol)
{
  Is_True(formal_symbol->Is_formal(), ("Expected a formal symbol"));
  return formal_array[formal_symbol->Get_findex()].Get_position();
}

// ------------------------------------
// Add an IVAR term to the caller linex
// ------------------------------------
static void
Add_ivar_to_caller_linex(const IPA_NODE* caller,
                         COEFF coeff,
                         const IVAR* ivar,
                         LINEX* linex)
{
  IP_FILE_HDR& file_hdr = caller->File_Header();
  SECTION_FILE_ANNOT* caller_file_annot = IP_FILE_HDR_section_annot(file_hdr);
  INT32 idx = caller_file_annot->Find_ivar(caller, *ivar);
  if (idx == -1) {
    idx = caller_file_annot->Add_ivar(caller, *ivar);
  }
  linex->Set_term(LTKIND_IV, coeff, idx, 0);
}

// forward declaration because of recursion  
static void
Add_value_to_caller_linex(const IPA_NODE* caller,
                          COEFF coeff,
                          const SUMMARY_VALUE* value,
                          LINEX* linex);

// --------------------------------------
// Add a SUMMARY_EXPR to the caller linex
// --------------------------------------
static void
Add_expr_to_caller_linex(const IPA_NODE* caller,
                         COEFF coeff,
                         const SUMMARY_EXPR* expr,
                         LINEX* linex)
{
  OPERATOR opr = OPCODE_operator(expr->Get_opcode());
  
  if (expr->Has_const_operand()) {
    // constant +|-|* value
    if (expr->Is_expr_value(0)) {
      Is_True(opr == OPR_ADD || opr == OPR_SUB || opr == OPR_MPY,
              ("Add_expr_to_caller_linex: expected +, -, or *"));
      const SUMMARY_VALUE* value = 
        IPA_get_value_array(caller) + expr->Get_node_index(expr->Get_kid());
      if (opr == OPR_ADD || opr == OPR_SUB) {
        linex->Set_term(LTKIND_CONST, 
                        (COEFF) expr->Get_const_value() * coeff,
                        CONST_DESC, 0);
      }
      if (opr == OPR_SUB) {
        coeff = -coeff;
      }
      else if (opr == OPR_MPY) {
        coeff *= expr->Get_const_value();
      }
      Add_value_to_caller_linex(caller, coeff, value, linex);
    }
    // constant +|- expression
    else {
      Is_True(expr->Is_expr_expr(0) && (opr == OPR_ADD || opr == OPR_SUB),
              ("Add_expr_to_caller_linex: expected const +|- expr"));
      const SUMMARY_EXPR* kid_expr = 
        IPA_get_expr_array(caller) + expr->Get_node_index(expr->Get_kid());
      linex->Set_term(LTKIND_CONST, 
                      (COEFF) expr->Get_const_value() * coeff,
                      CONST_DESC, 0);
      if (opr == OPR_SUB) {
        coeff = -coeff;
      }
      Add_expr_to_caller_linex(caller, coeff, kid_expr, linex);
    }
  }
  else {
    Is_True(opr == OPR_ADD || opr == OPR_SUB,
            ("Add_expr_to_caller_linex: expected + or -"));
    // expression +|- value
    for (INT i = 0; i < 2; ++i) {
      if (expr->Is_expr_value(i)) {
        const SUMMARY_VALUE* value = 
          IPA_get_value_array(caller) + expr->Get_node_index(i);
        Add_value_to_caller_linex(caller, coeff, value, linex);
      }
      else if (expr->Is_expr_expr(i)) {
        const SUMMARY_EXPR* kid_expr = 
          IPA_get_expr_array(caller) + expr->Get_node_index(i);
        Add_expr_to_caller_linex(caller, coeff, kid_expr, linex);
      }
      else {
        Is_True(0, 
                ("Add_expr_to_caller_linex: kid %d must be value or expr", i));
      }
      if (opr == OPR_SUB) {
        coeff = -coeff;
      }
    }
  }
}

// ---------------------------------------
// Add a SUMMARY_VALUE to the caller linex
// ---------------------------------------
static void
Add_value_to_caller_linex(const IPA_NODE* caller,
                          COEFF coeff,
                          const SUMMARY_VALUE* value,
                          LINEX* linex)
{
  IPA_CONST_TYPE value_kind = value->Get_const_type();
  
  if (value_kind == VALUE_INT_CONST) {
    linex->Set_term(LTKIND_CONST, 
                    (COEFF) value->Get_int_const_value() * coeff,
                    CONST_DESC, 0);
  }
  else if (value_kind == VALUE_CONST) {
    TCON_IDX tcon_idx = ST_tcon(ST_ptr(value->Get_const_st_idx()));
    linex->Set_term(LTKIND_CONST, 
                    (COEFF) Targ_To_Host(Tcon_Table[tcon_idx]) * coeff, 
                    CONST_DESC, 0);
  }
  else if (value_kind == VALUE_FORMAL) {
    const SUMMARY_FORMAL& formal = 
      IPA_get_formal_array(caller)[value->Get_formal_index()];
    const SUMMARY_SYMBOL& symbol = 
      IPA_get_symbol_array(caller)[formal.Get_symbol_index()];

    IVAR ivar(formal.Get_position(), 0, symbol.Get_btype());
    Add_ivar_to_caller_linex(caller, coeff, &ivar, linex);
  }
  else if (value_kind == VALUE_GLOBAL) {
    const SUMMARY_SYMBOL& symbol = 
      IPA_get_symbol_array(caller)[value->Get_global_index()];
    IVAR ivar(ST_ptr(symbol.St_idx()), 0, symbol.Get_btype());
    Add_ivar_to_caller_linex(caller, coeff, &ivar, linex);
  }
  else {
    Is_True(value_kind == VALUE_EXPR,
            ("Add_value_to_caller_linex: expected a linexable value"));
    SUMMARY_EXPR* expr = IPA_get_expr_array(caller) + value->Get_expr_index();
    Add_expr_to_caller_linex(caller, coeff, expr, linex);
  }
}
  
// ---------------------------------------------------------------
// update caller linex with the actual argument passed at callsite
// ---------------------------------------------------------------
static void
Add_actual_to_caller_linex(const IPA_NODE* caller,
                           COEFF coeff,
                           const SUMMARY_ACTUAL* actual,
                           LINEX* linex)
{
  if (actual->Get_symbol_index() != -1) {

    IPAA_NODE_INFO* modref_info = caller->Mod_Ref_Info();
    const SUMMARY_SYMBOL& symbol = 
      IPA_get_symbol_array(caller)[actual->Get_symbol_index()];
  
    if (symbol.Is_formal()) {
      // pass-through formal
      UINT32 position = Formal_position(IPA_get_formal_array(caller), &symbol);
      if (!modref_info->Is_formal_dmod_elmt(position) &&
          !modref_info->Is_formal_imod_elmt(position)) {
        IVAR ivar(position, 0, symbol.Get_btype());
        Add_ivar_to_caller_linex(caller, coeff, &ivar, linex);
        return;
      }
    }
    else if (ST_IDX_level(symbol.St_idx()) == GLOBAL_SYMTAB) {
      // global variable
      ST* st = ST_ptr(symbol.St_idx());
      while (ST_base(st) != st) {
        st = ST_base(st);
      }
      if (!modref_info->Is_def_elmt(ST_index(st))) {
        IVAR ivar(st, 0, symbol.Get_btype());
        Add_ivar_to_caller_linex(caller, coeff, &ivar, linex);
        return;
      }
    }
  }
  
  Is_True(actual->Get_value_index() != -1, 
          ("Add_actual_to_caller_linex: expected a linexable argument"));
  
  const SUMMARY_VALUE* value = 
    IPA_get_value_array(caller) + actual->Get_value_index();
  Add_value_to_caller_linex(caller, coeff, value, linex);
}

//================================================
// Map callee term and add it to the caller linex 
//================================================
static void
Map_term_to_caller(const IPA_NODE* caller, 
                   const IPA_NODE* callee,
                   const SUMMARY_CALLSITE* call,
                   TERM* term,
                   LINEX* linex)
{
  LTKIND term_kind = term->Get_type();

  if (term_kind == LTKIND_CONST) {
    linex->Set_term(term);
  }
  else if (term_kind == LTKIND_IV) {
    INT32 size;
    const IVAR* ivar = IPA_get_ivar_array(callee, size) + term->Get_desc();
    if (ivar->Is_Formal()) {
      const SUMMARY_ACTUAL* actual = IPA_get_actual_array(caller) 
                                   + call->Get_actual_index() 
                                   + ivar->Formal_Position();

      Add_actual_to_caller_linex(caller, term->Get_coeff(), actual, linex);
    }
    else {
      // ivar is global
      Add_ivar_to_caller_linex(caller, term->Get_coeff(), ivar, linex);
    }
  } 
      
  Is_True(1, ("Map_term_to_caller: Unexpected term kind\n"));
}
 
//---------------------------------------------------------------------------
//      Map the linex terms to the caller variables
//---------------------------------------------------------------------------
static void
Map_linex_to_caller(const IPA_NODE* caller,
                    const IPA_NODE* callee,
                    const SUMMARY_CALLSITE *callsite,
                    LINEX* callee_linex,
                    LINEX* caller_linex)
{     
  for (INT j = 0; j <= callee_linex->Num_terms(); ++j) {
    Map_term_to_caller(caller, callee, callsite, 
                       callee_linex->Get_term(j), caller_linex);
  }
}

// ------------------------------------------------------------
// Map a PROJECTED_NODE from the callee's to the caller's space
// ------------------------------------------------------------
static void
Map_projected_node_to_caller(const IPA_NODE* caller,
                             const IPA_NODE* callee,
                             const SUMMARY_CALLSITE* callsite,
                             PROJECTED_NODE* caller_pnode,
                             PROJECTED_NODE* callee_pnode)
{
  MEM_POOL* pool = caller_pnode->Mem_Pool();
  
  caller_pnode->Set_flags(callee_pnode->Get_flags());
  
  // map the linex's upper bound
  if (!callee_pnode->Is_messy_ub()) {
    Map_linex_to_caller(caller, callee, callsite, 
                        callee_pnode->Get_upper_linex(),
                        caller_pnode->Get_upper_linex());
    caller_pnode->Get_upper_linex()->Simplify();
  }

  // map lower bound
  if (!callee_pnode->Is_messy_lb()) {
    Map_linex_to_caller(caller, callee, callsite, 
                        callee_pnode->Get_lower_linex(),
                        caller_pnode->Get_lower_linex());
    caller_pnode->Get_lower_linex()->Simplify();
  }

  // map step
  if (!callee_pnode->Is_messy_step()) {
    Map_linex_to_caller(caller, callee, callsite, 
                        callee_pnode->Get_step_linex(),
                        caller_pnode->Get_step_linex());
    caller_pnode->Get_step_linex()->Simplify();
  }
  
  // segment length and stride are always constant: make a straight copy
  if (callee_pnode->Get_segment_length_linex()) {
    LINEX* segment_length = CXX_NEW(LINEX(pool), pool);
    callee_pnode->Get_segment_length_linex()->Copy(segment_length);
    caller_pnode->Set_segment_length_linex(segment_length);
  } 

  if (callee_pnode->Get_segment_stride_linex()) {
    LINEX* segment_stride = CXX_NEW(LINEX(pool), pool);
    callee_pnode->Get_segment_stride_linex()->Copy(segment_stride);
    caller_pnode->Set_segment_stride_linex(segment_stride);
  } 
}
  
// forward declaration because of recursion
static BOOL 
Is_caller_value_linexable(const IPA_NODE* caller, 
                          const SUMMARY_VALUE* value);

// ----------------------------------------------------
// Check if an expression can be represented as a LINEX 
// of constants, globals, and caller formals
// ----------------------------------------------------
static BOOL 
Is_caller_expr_linexable(const IPA_NODE* caller, 
                         const SUMMARY_EXPR* expr)
{
  if (expr->Is_expr_unknown()) {
    return FALSE;
  }

  OPERATOR opr = OPCODE_operator(expr->Get_opcode());

  if (expr->Has_const_operand()) {
    // constant +|-|* value
    if (expr->Is_expr_value(0) &&
        (opr == OPR_ADD || opr == OPR_SUB || opr == OPR_MPY)) {
      const SUMMARY_VALUE* value = 
        IPA_get_value_array(caller) + expr->Get_node_index(expr->Get_kid());
      return Is_caller_value_linexable(caller, value);
    }
    // constant +|- expression
    else if (expr->Is_expr_expr(0) && (opr == OPR_ADD || opr == OPR_SUB)) {
      const SUMMARY_EXPR* kid_expr = 
        IPA_get_expr_array(caller) + expr->Get_node_index(expr->Get_kid());
      return Is_caller_expr_linexable(caller, kid_expr);
    }
  }
  else if (opr == OPR_ADD || opr == OPR_SUB) {
    // expression +|- value
    for (INT i = 0; i < 2; ++i) {
      if (expr->Is_expr_value(i)) {
        const SUMMARY_VALUE* value = 
          IPA_get_value_array(caller) + expr->Get_node_index(i);
        if (!Is_caller_value_linexable(caller, value)) {
          return FALSE;
        }
      }
      else if (expr->Is_expr_expr(i)) {
        const SUMMARY_EXPR* kid_expr = 
          IPA_get_expr_array(caller) + expr->Get_node_index(i);
        if (!Is_caller_expr_linexable(caller, kid_expr)) {
          return FALSE;
        }
      }
      else {
        return FALSE;
      }
    }
    return TRUE;
  }
  
  return FALSE;
}

// -------------------------------------------------
// Check if a value can be represented as a LINEX of
// constants, globals, and caller formals
// -------------------------------------------------
static BOOL 
Is_caller_value_linexable(const IPA_NODE* caller, 
                          const SUMMARY_VALUE* value)
{
  switch (value->Get_const_type()) {

    case VALUE_INT_CONST:
      return TRUE;

    case VALUE_CONST: {
      INT64 int_val;
      TCON_IDX tcon_idx = ST_tcon(ST_ptr(value->Get_const_st_idx()));
      return Targ_Is_Integral(Tcon_Table[tcon_idx], &int_val);
    }

    case VALUE_FORMAL: {
      IPAA_NODE_INFO* caller_modref = caller->Mod_Ref_Info();      
      INT32 caller_position = 
        IPA_get_formal_array(caller)[value->Get_formal_index()].Get_position();
      return (!caller_modref->Is_formal_dmod_elmt(caller_position) &&
              !caller_modref->Is_formal_imod_elmt(caller_position));
    }
    
    case VALUE_GLOBAL: {
      INT32 symbol_index = value->Get_global_index();
      if (symbol_index != -1) {
        IPAA_NODE_INFO* caller_modref = caller->Mod_Ref_Info();      
        ST* st = ST_ptr(IPA_get_symbol_array(caller)[symbol_index].St_idx());
        while (ST_base(st) != st) {
          st = ST_base(st);
        }
        return !caller_modref->Is_def_elmt(ST_index(st));
      }
    }
    
    case VALUE_EXPR: {
      SUMMARY_EXPR* expr = IPA_get_expr_array(caller)+value->Get_expr_index();
      return Is_caller_expr_linexable(caller, expr);
    }
  }
  
  return FALSE;
}

// ----------------------------------------------------------------
// Check if a callee's formal can be mapped into the caller's space
// The formal can only be mapped if the actual argument is:
// 1. constant value 
// 2. pass-through formal (not modified in the caller)
// 3. global variable (not modified in the caller)
// -----------------------------------------------------------------
static BOOL
Is_callee_formal_mappable_to_caller(const IPA_NODE* caller, 
                                    const SUMMARY_CALLSITE* call,
                                    UINT32 position)
{
  if (position >= call->Get_param_count()) {
    return FALSE;
  }
  
  const SUMMARY_ACTUAL& actual = 
    IPA_get_actual_array(caller)[call->Get_actual_index() + position];

  if (actual.Get_symbol_index() != -1) {     
    IPAA_NODE_INFO* caller_modref = caller->Mod_Ref_Info();
    const SUMMARY_SYMBOL& caller_symbol = 
      IPA_get_symbol_array(caller)[actual.Get_symbol_index()];
    // pass-through formal
    if (caller_symbol.Is_formal()) {
      INT32 caller_position = 
        Formal_position(IPA_get_formal_array(caller), &caller_symbol);
      return (!caller_modref->Is_formal_dmod_elmt(caller_position) &&
              !caller_modref->Is_formal_imod_elmt(caller_position));
    }
    // global variable
    else if (ST_IDX_level(caller_symbol.St_idx()) == GLOBAL_SYMTAB) {
      ST* st = ST_ptr(caller_symbol.St_idx());
      while (ST_base(st) != st) {
        st = ST_base(st);
      }
      return !caller_modref->Is_def_elmt(ST_index(st));
    }
  }

  if (actual.Get_value_index() != -1) {     
    const SUMMARY_VALUE* value = 
      IPA_get_value_array(caller) + actual.Get_value_index();
    if (Is_caller_value_linexable(caller, value)) {
      return TRUE;
    }
  }

  return FALSE;
}

//==========================================================================
// Check if the callee term can be mapped into caller's variables
//==========================================================================
static BOOL
Is_term_mappable_to_caller(const IPA_NODE* caller, 
                           const IPA_NODE* callee,
                           const SUMMARY_CALLSITE* call,
                           const TERM* t)
{
  switch (t->Get_type()) {

    case LTKIND_CONST:
      return TRUE;

    case LTKIND_IV: 
      if (IPA_Enable_Simple_Alias) {
        INT32 size;
        const IVAR& ivar = IPA_get_ivar_array(callee, size)[t->Get_desc()];
        if (ivar.Is_Formal()) {
          UINT32 position = ivar.Formal_Position();
          return (ivar.Offset() == 0 &&
                  Is_callee_formal_mappable_to_caller(caller, call, position));
        }
        else {
          // must be a global variable
          Is_True(ST_IDX_level(ivar.St_Idx()) == GLOBAL_SYMTAB,
                  ("Map_term_to_caller: expected a global ST"));
          UINT32 modref_key = ST_IDX_index(ST_base_idx(ST_ptr(ivar.St_Idx())));
          return !caller->Mod_Ref_Info()->Is_def_elmt(modref_key);
        }
      }
      return FALSE;

    default:
      return FALSE;
  }
}

//---------------------------------------------------------------------------
//      Map the linex terms to the caller variables, if we are unable
//      to do the mapping then, return FALSE
//---------------------------------------------------------------------------
static BOOL
Is_linex_mappable_to_caller(const IPA_NODE* caller, 
                            const IPA_NODE* callee,
                            const SUMMARY_CALLSITE* callsite,
                            LINEX* l)
{     
  for (INT j = 0; j <= l->Num_terms(); ++j) {
    if (!Is_term_mappable_to_caller(caller, callee, callsite, l->Get_term(j))){
      return FALSE;
    }
  }
  return TRUE; 
}

//---------------------------------------------------------------------------
// Return TRUE if we can map callee region in terms of caller's variables
//---------------------------------------------------------------------------
static BOOL
Is_region_mappable_to_caller(const IPA_NODE* caller,
                             const IPA_NODE* callee,
                             const SUMMARY_CALLSITE *callsite,
                             PROJECTED_REGION *callee_region)
{
  if (callee_region->Is_messy_region()) {
    return FALSE;
  }
  
  for (INT i = 0; i < callee_region->Get_num_dims(); ++i) {
    
    PROJECTED_NODE* p1 = callee_region->Get_projected_node(i);
    Is_True(p1, ("Is_projected_region_mappable_to_caller: p1 is NULL\n"));

    // map upper bound
    if (!p1->Is_messy_ub()) {
      if (LINEX* ub = p1->Get_upper_linex()) {
        if (!Is_linex_mappable_to_caller(caller, callee, callsite, ub)) {
          return FALSE;
        }
      }
    }

    // map lower bound
    if (!p1->Is_messy_lb()) {
      if (LINEX *lb = p1->Get_lower_linex()) {
        if (!Is_linex_mappable_to_caller(caller, callee, callsite, lb)) {
          return FALSE;
        }
      }
    }

    // map step
    if (!p1->Is_messy_step()) {
      if (LINEX *step = p1->Get_step_linex()) {
        if (!Is_linex_mappable_to_caller(caller, callee, callsite, step)) {
          return FALSE;
        }
      }
    }
  }

  return TRUE;
}

//---------------------------------------------------------------------------
// Map callee annotation in terms of caller variables
// If we are able to map all the callee variables in terms of 
// actuals or globals in ther caller, then perform the mapping;
// otherwise set the new caller region to messy and return
//---------------------------------------------------------------------------
extern void
Map_callee_region_to_caller(const IPA_NODE* caller,
                            const IPA_NODE* callee,
                            const SUMMARY_CALLSITE* callsite,
                            PROJECTED_REGION* caller_region, 
                            PROJECTED_REGION* callee_region) 
{
  if (Is_region_mappable_to_caller(caller, callee, callsite, callee_region)) {

    Is_True(callee_region->Get_num_dims() == caller_region->Get_num_dims(), 
            ("Dim size mismatch in Map_callee_region_to_caller\n"));
      
    for (INT i = 0; i < callee_region->Get_num_dims(); ++i) {
      Map_projected_node_to_caller(caller, callee, callsite,
                                   caller_region->Get_projected_node(i),
                                   callee_region->Get_projected_node(i));
    }
  }
  else {
    caller_region->Set_messy_region();
  }
}

//-----------------------------------------------------------------------
// FUNCTION: Return TRUE if at the 'callsite' in the node 'caller_node',
//   the 'caller_shape' and the 'callee_shape' have the same shape.  
//   Return FALSE otherwise.  Use memory from 'mem_pool'.
//-----------------------------------------------------------------------
static BOOL
Same_Shape (const IPA_NODE* caller_node,
            const IPA_NODE* callee_node,
            const SUMMARY_CALLSITE* callsite, 
            PROJECTED_REGION* caller_shape, 
            PROJECTED_REGION* callee_shape,
            TYPE_ID caller_mtype, 
            TYPE_ID callee_mtype, 
            MEM_POOL* mem_pool)
{
  if (!caller_shape && !callee_shape) {
    return TRUE;
  }
 
  if (!caller_shape || !callee_shape || !callsite ||
      caller_shape->Get_num_dims() != callee_shape->Get_num_dims() ||
      caller_mtype != callee_mtype) {
    return FALSE;
  } 

  PROJECTED_REGION callee_shape_in_caller(callee_shape->Get_type(),
                                          callee_shape->Get_depth(),
                                          callee_shape->Get_num_dims(),
                                          mem_pool);
  
  Map_callee_region_to_caller(caller_node, callee_node, callsite,
                              &callee_shape_in_caller, callee_shape);

  return caller_shape->Equivalent(&callee_shape_in_caller);
} 

//-----------------------------------------------------------------------
// Return the projected region of an array section passed in
//-----------------------------------------------------------------------
static PROJECTED_REGION*
Get_actual_passed_region(const IPA_NODE* caller, 
                         const SUMMARY_ACTUAL& actual)
{
  INT ra_idx = actual.Get_index();
  Is_True(ra_idx != -1, ("Expecting a valid region array index\n"));

  INT pr_idx = IPA_get_region_array(caller)[ra_idx].Get_idx();
  Is_True(pr_idx != -1, ("Expecting a valid projected region index\n"));

  return IPA_get_proj_region_array(caller) + pr_idx;
}

//---------------------------------------------------------------------------
// Union section annotations
// 1) if caller and callee  regions exist, union the annotations and
// check for change from old annotation
// 2) if only caller region exists, return no change, since no union 
// operation is performed
// 3) if only callee region exists, copy the region and return change
// BOOL is_mod: TRUE if unioning mod regions else use regions
//---------------------------------------------------------------------------
static BOOL
Union_sections(const IPA_NODE* caller,
               const IPA_NODE* callee, 
	       const SUMMARY_CALLSITE* callsite,
               STATE* caller_annot,
	       STATE* callee_annot, 
               BOOL is_mod, 
	       PROJECTED_REGION* caller_shape = NULL,
	       PROJECTED_REGION* callee_shape = NULL,
	       TYPE_ID caller_mtype = MTYPE_UNKNOWN, 
	       TYPE_ID callee_mtype = MTYPE_UNKNOWN,  
	       PROJECTED_REGION* callsite_region = NULL)
{
  PROJECTED_REGION* caller_region;
  PROJECTED_REGION* callee_region;

  if (is_mod) {
    caller_region = caller_annot->Get_projected_mod_region();
    callee_region = callee_annot->Get_projected_mod_region();
  }
  else {
    caller_region = caller_annot->Get_projected_ref_region();
    callee_region = callee_annot->Get_projected_ref_region();
  }

  // check if callee region is empty, or caller region is already messsy
  if (!callee_region || (caller_region && caller_region->Is_messy_region())) {
    return FALSE;
  }
  
  MEM_POOL* mem_pool = caller->Section_Annot()->Mem_Pool();
  BOOL created_caller_region = FALSE;

  // initialize caller_region, if necessary
  if (!caller_region) {
    created_caller_region = TRUE;
    caller_region = CXX_NEW(PROJECTED_REGION(callee_region->Get_type(),
                                             callee_region->Get_depth(),
                                             callee_region->Get_num_dims(),
                                             mem_pool), mem_pool);
    if (is_mod) {
      caller_annot->Set_projected_mod_region(caller_region);
    }
    else {
      caller_annot->Set_projected_ref_region(caller_region);
    }
  }

  // if callee region is messy, caller region should be too
  if (callee_region->Is_messy_region()) {
    caller_region->Set_messy_region();
    return TRUE;
  }

  BOOL reshape_trace = Get_Trace(TP_IPA, IPA_TRACE_RESHAPE);

  RESHAPE reshape(caller_shape, callee_shape, callee_region, 
                  callsite_region, mem_pool, reshape_trace);

  BOOL same_shape = Same_Shape(caller, callee, callsite, 
                               caller_shape, callee_shape, 
                               caller_mtype, callee_mtype, mem_pool);

  // caller and callee see the same shape, but an array section is passed
  if (same_shape && callsite_region) {
    reshape.Set_callee_proj_reshaped_region(callee_region);
    if (!reshape.Reshapeable_Passed_Section(reshape_trace)) {
      caller_region->Set_messy_region();
      return TRUE;
    }
  }

  // diferent shapes, so we need to reshape callee region
  if (!same_shape && caller_shape) {
    callee_region = reshape.Reshape_Callee_To_Caller(reshape_trace);
    if (callsite_region && !callee_region->Is_messy_region()) {
      if (!reshape.Reshapeable_Passed_Section(reshape_trace)) {
        callee_region->Set_messy_region();
      }
    }
  }

  if (callee_region->Is_messy_region()) {
    caller_region->Set_messy_region();
    return TRUE;
  }

  PROJECTED_REGION* new_caller_region = 
    (created_caller_region && 
     caller_region->Get_num_dims() == callee_region->Get_num_dims()) ?
    caller_region :
    CXX_NEW(PROJECTED_REGION(callee_region->Get_type(),
                             callee_region->Get_depth(), 
                             callee_region->Get_num_dims(), 
                             mem_pool), mem_pool);

  // map (possibly reshaped) callee region into caller's space
  Map_callee_region_to_caller(caller, callee, callsite, 
                              new_caller_region, callee_region);

  // factor the effects of the array section passed at callsite
  if (callsite_region) {
    reshape.Reshape_Passed_Section(new_caller_region, reshape_trace); 
  }

  BOOL change = TRUE;
  if (!created_caller_region) {
    change = caller_region->May_Union(*new_caller_region, Trace_IPA_Sections);
  } 
  else {
    if (is_mod) {
      caller_annot->Set_projected_mod_region(new_caller_region);
    }
    else {
      caller_annot->Set_projected_ref_region(new_caller_region);
    }
  }
  
  return change;
}

//-----------------------------------------------------------------------
// For the caller with given 'section', set the 'caller_annot' to MESSY. 
// Clone an annot from 'callee_annot' if 'caller_annot' is NULL.
//-----------------------------------------------------------------------

static 
BOOL Set_Caller_Annot_Messy(STATE* caller_annot,
			    STATE* callee_annot,
			    BOOL make_messy_region, 
                            MEM_POOL* pool)
{
  BOOL change = FALSE;

  PROJECTED_REGION* pr_callee_mod 
    = callee_annot != NULL ? callee_annot->Get_projected_mod_region() : NULL;
  if (pr_callee_mod || make_messy_region) {
    PROJECTED_REGION* pr_caller_mod= caller_annot->Get_projected_mod_region();
    if (!pr_caller_mod) {
      if (pr_callee_mod) { 
	pr_caller_mod = CXX_NEW(PROJECTED_REGION(MESSY_REGION,
          pr_callee_mod->Get_depth(), pr_callee_mod->Get_num_dims(), pool), 
	  pool);
      } else { 
	pr_caller_mod = CXX_NEW(PROJECTED_REGION(MESSY_REGION, 0, 0, pool), 
	  pool); 
      } 
      caller_annot->Set_projected_mod_region(pr_caller_mod);
      change = TRUE;
    } else if (!pr_caller_mod->Is_messy_region()) {
      pr_caller_mod->Set_messy_region();
      change = TRUE;
    }
  } 
 
  PROJECTED_REGION* pr_callee_ref 
    = callee_annot != NULL ? callee_annot->Get_projected_ref_region() : NULL;
  if (pr_callee_ref || make_messy_region) { 
    PROJECTED_REGION* pr_caller_ref= caller_annot->Get_projected_ref_region();
    if (!pr_caller_ref) {
      if (pr_callee_ref) { 
        pr_caller_ref = CXX_NEW(PROJECTED_REGION(MESSY_REGION,
          pr_callee_ref->Get_depth(), pr_callee_ref->Get_num_dims(), pool), 
	  pool);
      } else { 
	pr_caller_ref = CXX_NEW(PROJECTED_REGION(MESSY_REGION, 0, 0, pool), 
	  pool);
      } 
      caller_annot->Set_projected_ref_region(pr_caller_ref);
      change = TRUE;
    } else if (!pr_caller_ref->Is_messy_region()) {
      pr_caller_ref->Set_messy_region();
      change = TRUE;
    }
  } 

  return change;
}

//-----------------------------------------------------------------------
// NAME: Set_Caller_Actual_Messy
// FUNCTION: Set the array section for the 'i'-th argument of the 'caller'
//  to MESSY, because the 'callee' has been passed a reshaped array section.
//  information about the actuals of the caller at that callsite is found
//  in the array 'actuals'.
//-----------------------------------------------------------------------
static BOOL 
Set_Caller_Actual_Messy(const IPA_NODE* caller,
                        IPA_NODE_SECTION_INFO* caller_info,
                        IPA_NODE_SECTION_INFO* callee_info,
                        const SUMMARY_FORMAL* caller_formals,
                        const SUMMARY_SYMBOL* caller_symbols,
			const SUMMARY_ACTUAL* actuals,
                        INT i)
{
  INT symbol_index = actuals[i].Get_symbol_index();
  if (symbol_index == -1) {
    // DevWarn(("Could not find symbol index"));
    return FALSE;
  }

  STATE* callee_formal = NULL; 
  BOOL make_messy_region = FALSE; 
  if (i >= 0 && i < callee_info->Get_formal_count()) {
    callee_formal = callee_info->Get_formal(i);
  } else { 
    make_messy_region = TRUE; 
  } 

  const SUMMARY_SYMBOL* symbol = caller_symbols + symbol_index;

  if (symbol->Is_formal() &&
      ST_IDX_level(symbol->St_idx()) == caller->Lexical_Level()) {
    INT formal_pos = Formal_position(caller_formals, symbol);
    return Set_Caller_Annot_Messy(caller_info->Get_formal(formal_pos),
                                  callee_formal, make_messy_region, 
                                  caller_info->Mem_Pool());
  }

  else if (ST_IDX_level(symbol->St_idx()) == GLOBAL_SYMTAB &&
           ST_class(symbol->St_idx()) == CLASS_VAR &&
           TY_kind(ST_type(symbol->St_idx())) == KIND_ARRAY) {

    // Give up completely if common block elements are equivalenced
    if (ST_is_equivalenced(ST_ptr(symbol->St_idx()))) {
      return caller_info->Set_Global_Array_List_To_Messy(symbol);      
    }
    
    GLOBAL_ARRAY_INFO* gai = caller_info->Find_Global_Array_Info(symbol);
    if (!gai) { 
      gai = caller_info->Add_Global_Array_Info(symbol);
    }
    return Set_Caller_Annot_Messy(gai->Get_state(),
                                  callee_formal, make_messy_region, 
                                  caller_info->Mem_Pool());
  }
  
  return FALSE;
}

//---------------------------------------------------------------------------
//       IPA_NODE* caller : caller node
//       IPA_NODE* callee : callee node
//       Walk the global sections of the callee 
//       and merge them in with the caller
//---------------------------------------------------------------------------
static BOOL
Merge_global_sections(IPA_NODE *caller, 
                      IPA_NODE *callee,
		      SUMMARY_CALLSITE *call)
{
  BOOL change = FALSE; 
  IPA_NODE_SECTION_INFO* caller_info = caller->Section_Annot();
  IPA_NODE_SECTION_INFO* callee_info = callee->Section_Annot();

  GLOBAL_ARRAY_TABLE* caller_tbl = caller_info->Global_Array_Table();
  GLOBAL_ARRAY_TABLE* callee_tbl = callee_info->Global_Array_Table();

  MEM_POOL* pool = caller_info->Mem_Pool();
  
  // walk all commons in the callee and merge them with those in the caller
  ST_IDX st;
  GLOBAL_ARRAY_LIST* callee_list;
  GLOBAL_ARRAY_TABLE_ITER callee_tbl_iter(callee_tbl);
  while (callee_tbl_iter.Step (&st, &callee_list)) {
    GLOBAL_ARRAY_LIST* caller_list = caller_tbl->Find(st);
    // if the list hasn't been created, do it now
    if (!caller_list) {
      caller_list = CXX_NEW(GLOBAL_ARRAY_LIST(st), pool);
      caller_tbl->Enter(st, caller_list);	   
    }
    change |= caller_list->Merge(caller, callee, call, callee_list, pool);
  }
  return change; 
}

//---------------------------------------------------------------------------
// Build the projected region that represents the shape of the caller. 
// If no region exists return NULL
//---------------------------------------------------------------------------
static PROJECTED_REGION*
Global_shape_region(SUMMARY_SYMBOL* symbol, MEM_POOL* pool)
{
  TY_IDX ty_idx = ST_type(symbol->St_idx());
  INT num_dims = TY_AR_ndims(ty_idx);
  
  PROJECTED_REGION* p = 
    CXX_NEW(PROJECTED_REGION(NON_MESSY_REGION, 0, num_dims, pool), pool);
  
  // fill in projected nodes
  for (INT i = 0; i < num_dims; ++i) {
    PROJECTED_NODE* node = p->Get_projected_node (i);
    node->Set_constant_linexs (TY_AR_ubnd_val(ty_idx,i) -
                               TY_AR_lbnd_val(ty_idx,i),
                               0,
                               1, 
                               0, 
                               0);
  }
  return p;
}

//---------------------------------------------------------------------------
// For each call do
//   For each actual parameter do
//    if (array && formal) {
//     perform reshape analysis
//     merge with formal section 
//    }       
//    if (array && global) {
//     if (section exists) {
//      perform reshape analysis
//      merge with global section
//      }
//     else
//      mark global as bottom
//    }
//   For each global do
//    if (section exists in caller) {
//      if consistent shape and no aliasing
//       merge with global section
//      else
//       mark global as bottom
//    else
//      create new section
// Return TRUE is a change in a section is noted
//---------------------------------------------------------------------------
BOOL
Merge_Section(IPA_NODE* caller)
{
  if (caller->Summary_Proc()->Has_incomplete_array_info()) {
    return FALSE;
  }

  // be conservative if there are indirect or opaque calls
  if (!caller->Icall_List().empty() || !caller->Ocall_List().empty()) {
    caller->Summary_Proc()->Set_has_incomplete_array_info();
    return TRUE; 
  }
  
  BOOL change = FALSE;
  IPA_NODE_SECTION_INFO* caller_info = caller->Section_Annot();
  SUMMARY_FORMAL* caller_formals = IPA_get_formal_array(caller);
  SUMMARY_SYMBOL* caller_symbols = IPA_get_symbol_array(caller);

  change = Merge_Execution_Cost(caller, &IPA_array_prop_pool);

  // walk the calls for this procedure
  IPA_SUCC_ITER edge_iter(IPA_Call_Graph, caller);
  for (edge_iter.First(); !edge_iter.Is_Empty(); edge_iter.Next()) {

    IPA_EDGE* e = edge_iter.Current_Edge();
    if (!e) {
      continue;
    }

    IPA_NODE* callee = IPA_Call_Graph->Callee(e);
    IPA_NODE_SECTION_INFO* callee_info = callee->Section_Annot();
    SUMMARY_FORMAL* callee_formals = IPA_get_formal_array (callee);
    Init_IPA_Print_Arrays(callee);

    // Propagate INCOMPLETE_ARRAY_INFO bit from callees to the caller
    if (callee->Summary_Proc()->Has_incomplete_array_info()) {
      caller->Summary_Proc()->Set_has_incomplete_array_info();
      return TRUE; 
    }

    SUMMARY_CALLSITE* call = e->Summary_Callsite();
    SUMMARY_ACTUAL* actuals = IPA_get_actual_array(caller) 
                              + call->Get_actual_index();

    // Go conservative if caller actual and formal counts do not match 
    INT formal_count = callee->Summary_Proc()->Get_formal_count();
    INT actual_count = e->Num_Actuals();
    if (formal_count != actual_count) {
      for (INT i = 0; i < actual_count; i++)
	change = Set_Caller_Actual_Messy(caller, 
                                         caller_info, callee_info, 
                                         caller_formals, caller_symbols, 
                                         actuals, i);
      continue; 
    } 

    for (INT i = 0; i < actual_count; ++i) {
      if ((!IPA_Enable_Reshape &&
          Try_Reshape_Callee_Formal(caller,callee,call,i,&IPA_array_prop_pool))
          ||
          (IPA_Enable_Reshape && 
           Mismatched_Types(caller, callee, call, i, &IPA_array_prop_pool))) {
        change = Set_Caller_Actual_Messy(caller, 
                                         caller_info, callee_info, 
                                         caller_formals, caller_symbols,
                                         actuals, i);
        continue; 
      } 

      INT actual_symbol_index = actuals[i].Get_symbol_index();
      // this is the case where an entire array is passed. 
      if (actual_symbol_index != -1) { 
        
        STATE*            caller_annot;
        PROJECTED_REGION* caller_shape;
        STATE*            callee_annot = callee_info->Get_formal(i);
        PROJECTED_REGION* callee_shape = callee_annot->
                                         Get_projected_dcl_region();
        PROJECTED_REGION* passed_region = NULL;

        INT32 callee_f_idx = callee->Summary_Proc()->Get_formal_index() + i;
        TYPE_ID callee_mtype = callee_formals[callee_f_idx].Get_machine_type();

        SUMMARY_SYMBOL* symbol = caller_symbols + actual_symbol_index;

        if (symbol->Is_formal() &&
            ST_IDX_level(symbol->St_idx()) == caller->Lexical_Level()) {
          INT caller_mtype = 
            caller_formals[symbol->Get_findex()].Get_machine_type();
          INT formal_pos = Formal_position(caller_formals, symbol);
          
          caller_annot = caller_info->Get_formal(formal_pos);
          caller_shape = caller_annot->Get_projected_dcl_region();
          
          if (actuals[i].Get_pass_type() == PASS_ARRAY_SECTION) {
            passed_region = Get_actual_passed_region(caller, actuals[i]);
            passed_region = Projected_Region_To_Memory(caller, 
                                                       passed_region,
                                                       &IPA_array_prop_pool);
          }

          change |= Union_sections(caller, callee, call, 
                                   caller_annot, callee_annot, TRUE,
                                   caller_shape, callee_shape, 
                                   caller_mtype, callee_mtype, passed_region);
            
          change |= Union_sections(caller, callee, call, 
                                   caller_annot, callee_annot, FALSE,
                                   caller_shape, callee_shape,
                                   caller_mtype, callee_mtype, passed_region);
        }

        else if (ST_IDX_level(symbol->St_idx()) == GLOBAL_SYMTAB) {

	  if (ST_class(symbol->St_idx()) == CLASS_FUNC 
	      || ST_class(symbol->St_idx()) == CLASS_BLOCK) { 
            change = Set_Caller_Actual_Messy(caller, 
                                             caller_info, callee_info, 
                                             caller_formals, caller_symbols, 
                                             actuals, i);
            continue; 
          } 
          
          TY_IDX ty_idx = ST_type(symbol->St_idx());
          if (TY_kind(ty_idx) != KIND_ARRAY) {
            // no need to do anything for common scalars
            continue;
          }
          
          BOOL is_messy = FALSE;
          caller_info->Global_Array_Region(symbol, &is_messy, NULL, TRUE);
          caller_info->Global_Array_Region(symbol, &is_messy, NULL, FALSE);
          if (is_messy) {
            continue;
          }
          
          if (actuals[i].Get_pass_type() == PASS_ARRAY_SECTION) {
            passed_region = Get_actual_passed_region(caller, actuals[i]);
            passed_region = Projected_Region_To_Memory(caller, 
                                                       passed_region,
                                                       &IPA_array_prop_pool);
          }

          caller_annot = caller_info->Find_Global_Array_Sections(symbol);
          caller_shape = Global_shape_region(symbol, &IPA_array_prop_pool);

          INT caller_mtype = TY_mtype(TY_etype(ty_idx));
		
          change |= Union_sections(caller, callee, call,
                                   caller_annot, callee_annot, TRUE, 
                                   caller_shape, callee_shape,
                                   caller_mtype, callee_mtype, passed_region);
		      
          change |= Union_sections(caller, callee, call, 
                                   caller_annot, callee_annot, FALSE, 
                                   caller_shape, callee_shape,
                                   caller_mtype, callee_mtype, passed_region);
        }
      }
    }

    // merge global sections in the callee with those in the caller
    change |= Merge_global_sections(caller, callee, call);
  }

  return change;
}

// ==================================
// GLOBAL_ARRAY_LIST member functions
// ==================================

//---------------------------------------------------------------------------
//      Find_Global_Array_Info(SUMMARY_COMMON_SHAPE *table)
//      Return the common shape element in the common block list
//      If it is not found then return NULL
//---------------------------------------------------------------------------
GLOBAL_ARRAY_INFO*
GLOBAL_ARRAY_LIST::Find_Global_Array_Info(ST_IDX st_idx)
{
  Is_True(ST_IDX_level(st_idx) == GLOBAL_SYMTAB, 
          ("Find_Global_Array_Info: Symbol is NOT global!\n"));

  GLOBAL_ARRAY_LIST_ITER  iter(this);
  for (iter.First(); !iter.Is_Empty(); iter.Next()) {

    GLOBAL_ARRAY_INFO* current_section = iter.Cur();
    ST_IDX current_st_idx = current_section->St_Idx();

    if (st_idx == current_st_idx ||
        (ST_type(st_idx) == ST_type(current_st_idx) &&
         ST_ofst(ST_ptr(st_idx)) == ST_ofst(ST_ptr(current_st_idx)))) {
      return current_section;
    }
  }

  return NULL;
}

//---------------------------------------------------------------------------
// Merge callee's sections for the common block with the caller's
//---------------------------------------------------------------------------
BOOL
GLOBAL_ARRAY_LIST::Merge(const IPA_NODE* caller, 
                         const IPA_NODE* callee,
                         const SUMMARY_CALLSITE* call,  
                         GLOBAL_ARRAY_LIST* callee_list,
                         MEM_POOL* m)
{
  if (callee_list->Is_messy()) {
    if (Is_messy()) { 
      return FALSE; // if caller's list is messy, no change
    }
    Set_is_messy(); // set caller's list to messy
    return TRUE;
  }

  BOOL change = FALSE; 

  // walk all common elements and merge mod/ref regions
  GLOBAL_ARRAY_LIST_ITER iter(callee_list);
  for (iter.First(); !iter.Is_Empty(); iter.Next()) {

    GLOBAL_ARRAY_INFO* callee_info = iter.Cur();
    ST_IDX st_idx = callee_info->St_Idx();
    GLOBAL_ARRAY_INFO* caller_info = Find_Global_Array_Info(st_idx);
    if (!caller_info) {
      caller_info = Append(st_idx, m);
    }

    STATE* caller_state = caller_info->Get_state();
    STATE* callee_state = callee_info->Get_state();
    
    // NOTE: dont's use if with || here, because short-circuiting may skip 
    // the second call to Union_sections
    change |= Union_sections(caller, callee, call, 
                             caller_state, callee_state, TRUE);
    change |= Union_sections(caller, callee, call, 
                             caller_state, callee_state, FALSE);
  }

  return change; 
}
     
//------------------------------------------------------------------------
//        Print the GLOBAL_ARRAY_LIST
//------------------------------------------------------------------------
void 
GLOBAL_ARRAY_LIST::Print(FILE* fp)
{
  GLOBAL_ARRAY_LIST_ITER iter(this);
  for (iter.First(); !iter.Is_Empty(); iter.Next()) {
    iter.Cur()->Print(fp);
  }
}


// ======================================
// IPA_NODE_SECTION_INFO member functions
// ======================================

//------------------------------------------------------------------------
// For the SUMMARY_SYMBOL 's' that represents a global array, return
// MOD (is_mod == TRUE) or REF (is_mod == FALSE) projected region.
// Set 'is_messy' to TRUE if the entire common block messy
// If a new GLOBAL_ARRAY_INFO is created, set its MOD/REF to 'region'
//------------------------------------------------------------------------
PROJECTED_REGION*
IPA_NODE_SECTION_INFO::Global_Array_Region(const SUMMARY_SYMBOL* s,
                                           BOOL* is_messy,
                                           PROJECTED_REGION* region,
                                           BOOL is_mod)
{
  ST_IDX st_idx = s->St_idx();
  
  GLOBAL_ARRAY_LIST* list = Find_Global_Array_List(s);
  if (!list) {
    list = Add_Global_Array_List(s);
    if (ST_is_equivalenced(ST_ptr(s->St_idx()))) {
      list->Set_is_messy();
    }
  }
  
  if (!list->Is_messy()) {
    *is_messy = FALSE;
    GLOBAL_ARRAY_INFO* global_array_info = Find_Global_Array_Info(s);
    if (!global_array_info) {
      global_array_info = Add_Global_Array_Info(s);
    }
    if (is_mod) {
      if (region && !global_array_info->Get_projected_mod_region()) {
        global_array_info->Set_projected_mod_region(region);
      }
      return global_array_info->Get_projected_mod_region();
    }
    else {
      if (region && !global_array_info->Get_projected_ref_region()) {
        global_array_info->Set_projected_ref_region(region);
      }
      return global_array_info->Get_projected_ref_region();
    }
  }
  else {
    *is_messy = TRUE;
    return 0;
  }
}

//--------------------------------------------------------
// SUMMARY_SYMBOL* s: summary symbol for the global array
//--------------------------------------------------------
BOOL
IPA_NODE_SECTION_INFO::Set_Global_Array_List_To_Messy(const SUMMARY_SYMBOL* s)
{ 
  GLOBAL_ARRAY_LIST* list = Find_Global_Array_List(s);
  if (!list) {
    list = Add_Global_Array_List(s);
  }
  if (list->Is_messy()) {
    return FALSE;
  }
  list->Set_is_messy();
  return TRUE;
}

//-------------------------------------------------------
// print the section annotation
//-------------------------------------------------------
void
IPA_NODE_SECTION_INFO::Print(FILE *fp)
{
  fprintf(fp,"---------start printing section information-------\n");
  if (STATE_ARRAY* state = Get_formals()) {
    for (INT i = 0; i < state->Elements(); ++i) {
      fprintf(fp, "formal %d : ", i);
      (*state)[i].Print(fp);
    }
  }
  fprintf(fp,"------end printing section information-------\n");
}

//-------------------------------------------------------
//      Print the global annotation
//-------------------------------------------------------
void 
IPA_NODE_SECTION_INFO::Print_Global_Sections (FILE* fp)
{
  GLOBAL_ARRAY_TABLE_ITER tbl_iter(Global_Array_Table());

  GLOBAL_ARRAY_LIST* list;
  ST_IDX st_idx;
  
  while (tbl_iter.Step(&st_idx, &list)) {
    fprintf(fp, "Common block = %s\n", ST_name(st_idx));
    GLOBAL_ARRAY_LIST_ITER iter(list);
    for (iter.First(); !iter.Is_Empty(); iter.Next())	{
      iter.Cur()->Print(fp);
    }
  }
}

//-------------------------------------------------------
// get the euse count for the formals in this PU
//-------------------------------------------------------
INT 
IPA_NODE_SECTION_INFO::Get_formal_euse_count()
{
  INT count = 0;
  if (STATE_ARRAY* formals_state = Get_formals()) {
    for (INT i = 0; i < formals_state->Elements(); ++i)	{
      if ((*formals_state)[i].Is_euse()) {
        ++count;
      }
    }
  }
  return count;
}

//-------------------------------------------------------
// get the kill count for the formals in this PU
//-------------------------------------------------------
INT 
IPA_NODE_SECTION_INFO::Get_formal_kill_count()
{
  INT count = 0;
  if (STATE_ARRAY* formals_state = Get_formals()) {
    for (INT i = 0; i < formals_state->Elements(); ++i)	{
      if ((*formals_state)[i].Is_must_kill()) {
        ++count;
      }
    }
  }
  return count;
}


// ==================================
// GLOBAL_ARRAY_INFO member functions
// ==================================

//-------------------------------------------
// Print mod/ref sections for a global array
//-------------------------------------------
void
GLOBAL_ARRAY_INFO::Print(FILE* fp)
{
  const ST& st = St_Table[St_Idx()];
  st.Print(fp, FALSE);
  Ty_Table[ST_type(st)].Print(fp);

  if (PROJECTED_REGION* mod = Get_projected_mod_region()) {
    fprintf(fp, "mod region\n");
    mod->Print(fp);
  }

  if (PROJECTED_REGION* ref = Get_projected_ref_region()) {
    fprintf(fp, "ref region\n");
    ref->Print(fp);
  }
}


// ===================================
// SECTION_FILE_ANNOT member functions
// ===================================

//------------------------------------------------------------------------
//      Find an ivar entry
//------------------------------------------------------------------------
INT32
SECTION_FILE_ANNOT::Find_ivar(const IPA_NODE* node, const IVAR& ivar)
{
  INT32 ivar_size;
  const IVAR* ivar_array = IPA_get_ivar_array(node, ivar_size);
  for (INT32 i = 0; i < ivar_size; i++) {
    if (ivar_array[i] == ivar) {
      return i;
    }
  } 
  return -1; 
}     

//------------------------------------------------------------------------
//      Add a local ivar entry
//------------------------------------------------------------------------
INT32
SECTION_FILE_ANNOT::Add_ivar(const IPA_NODE* node, const IVAR& ivar)
{
  if (!_iv_grow) {
    // copy all IVARs from the summary file into _iv_grow
    _iv_grow = CXX_NEW(IVAR_ARRAY(Mem_Pool()), Mem_Pool());
    INT32 ivar_size;
    IPA_get_ivar_file_array(node->File_Header(), ivar_size);
    for (INT32 i = 0; i < ivar_size; ++i) {
      _iv_grow->AddElement(_iv_base[i]);
    }
  }

  _iv_grow->AddElement(ivar);
  return _iv_grow->Lastidx();
}

//-------------------------------------------------------
//  Print the state information
//-------------------------------------------------------
void
STATE::Print(FILE *fp)
{
  if (Is_must_kill()) fprintf(fp, " must kill, ");
  if (Is_may_kill())  fprintf(fp, " may kill, ");
  if (Is_euse())      fprintf(fp, " euse, ");
  if (Is_use())       fprintf(fp, " use, ");
  if (Is_must_reduc())fprintf(fp, " must reduc, ");
  if (Is_may_reduc()) fprintf(fp, " may reduc, ");
  if (Is_scalar())    fprintf(fp, "is scalar, ");

  if (PROJECTED_REGION* mod = Get_projected_mod_region()) mod->Print_file(fp);
  fprintf(fp, "\n");
  if (PROJECTED_REGION* ref = Get_projected_ref_region()) ref->Print_file(fp);
  fprintf(fp, "\n");
  if (PROJECTED_REGION* dcl = Get_projected_dcl_region()) dcl->Print_file(fp);
  fprintf(fp, "\n");
}
