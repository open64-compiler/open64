/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2006, 2007. QLogic Corporation. All Rights Reserved.
 */

//-*-c++-*-

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_prop.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_prop.cxx,v $
//
// Revision history:
//  20-DEC-94 - Original Version
//
// ====================================================================
//
// Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it
// is free of the rightful claim of any third person regarding
// infringement  or the like.  Any license provided herein, whether
// implied or otherwise, applies only to this software file.  Patent
// licenses, if any, provided herein do not apply to combinations of
// this program with other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write the Free Software Foundation,
// Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, USA.
//
// Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
// Mountain View, CA 94043, or:
//
// http://www.sgi.com
//
// For further information regarding this notice, see:
//
// http://oss.sgi.com/projects/GenInfo/NoticeExplan
//
// ====================================================================
//
// Description:
//
// Global copy propagation
//
// ====================================================================
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#include "defs.h"
#include "errors.h"
#include "erglob.h"
#include "glob.h"	// for Cur_PU_Name
#include "mempool.h"
#include "tracing.h"	/* for TFile */
#include "stab.h"
#include "irbdata.h"
#include "cxx_memory.h"
#include "be_symtab.h"

#include "opt_defs.h"
#include "opt_config.h"
#include "opt_cfg.h"
#include "opt_ssa.h"
#include "opt_main.h"
#include "bb_node_set.h"
#include "opt_util.h"
#include "opt_fold.h"
#include "opt_mu_chi.h"
#include "opt_sym.h"
#include "opt_alias_rule.h"
#include "opt_prop.h"
#include "opt_cvtl_rule.h"

// ====================================================================
// Contains_only_constants - see if the expr contains only constants
// ====================================================================
BOOL
CODEREP::Contains_only_constants() const
{
  switch (Kind()) {
  case CK_LDA:
  case CK_CONST:
  case CK_RCONST: 
    return TRUE;
  case CK_VAR:
  case CK_IVAR:
    return FALSE;
  case CK_OP: {
      for  (INT32 i = 0; i < Kid_count(); i++) {
	if (! Opnd(i)->Contains_only_constants())
	  return FALSE;
      }
      return TRUE;
    }
  }
  return FALSE;
}

// ====================================================================
// Contains_only_the_var -  the expr is known to contain non-constants;
// see if the expr contains only variables of the given aux_id; 
// return FALSE if containing other variable, TRUE if containing only
// the variable of the given AUX_ID. ISOP_CONTAIN_VISITED flag is used to
// avoid exponential behavior in this function in the case of huge tree.
// The caller must call Reset_isop_visited afterwards because
// another caller may call it with a different aux_id. 
// ====================================================================
static BOOL
Contains_only_the_var(AUX_ID id, CODEREP *cr)
{

  switch (cr->Kind()) {
  case CK_LDA:
  case CK_CONST:
  case CK_RCONST: 
    return TRUE;
  case CK_VAR:
    if (cr->Aux_id() == id)
      return TRUE;
    else return FALSE;
  case CK_IVAR:
    return FALSE;
  case CK_OP: {
      if (cr->Is_isop_flag_set(ISOP_CONTAIN_VISITED))
	return TRUE;
      for  (INT32 i = 0; i < cr->Kid_count(); i++) {
        if (! Contains_only_the_var(id, cr->Opnd(i))) {
	  for (INT32 j = 0; j < i; j++)
	    cr->Opnd(j)->Reset_isop_visited(ISOP_CONTAIN_VISITED);
	  return FALSE;
	}
      }
      cr->Set_isop_flag(ISOP_CONTAIN_VISITED);
      return TRUE;
    }
  }
  return FALSE;
}

// ====================================================================
// Is_function_of_itself - see if the assignment is of the form x2 = f(x1)
// where f(x1) contains x1 as the only variable
// ====================================================================
BOOL
COPYPROP::Is_function_of_itself(STMTREP *stmt, OPT_STAB *sym)
{
  CHI_NODE *cnode;
  CHI_LIST_ITER chi_iter;
  CODEREP *lhs = stmt->Lhs(); 
  CODEREP *rhs = stmt->Rhs(); 

  if (lhs->Is_var_volatile())
    return FALSE;
  // check if it is a physical register
  ST *s = sym->St(lhs->Aux_id());
  if ((ST_class(s) == CLASS_PREG && Preg_Is_Dedicated(lhs->Offset())))
    return FALSE;

  if (rhs->Contains_only_constants())
    return FALSE;
  if (! Contains_only_the_var(lhs->Aux_id(), rhs))
    return FALSE;

  rhs->Reset_isop_visited(ISOP_CONTAIN_VISITED);

  INT32 height = 0;
  INT32 weight = 0;
  if (!Propagatable(rhs, FALSE, 0, FALSE, FALSE, &height, &weight, FALSE ,NULL))
    return FALSE;

  if (! rhs->Non_leaf()) {	// identity assignment
    // make sure there is no chi whose lhs is zero version
    FOR_ALL_NODE(cnode, chi_iter, Init(stmt->Chi_list())) {
      if (! cnode->Live()) continue;
      if (cnode->RESULT()->Is_flag_set(CF_IS_ZERO_VERSION))
	return FALSE;
    }
    return TRUE;
    }

  // not an identify assignment:
  if (! WOPT_Enable_Itself_Prop)
    return FALSE;

  // if not identity assignment, need to check DONT_PROP flag;
  // otherwise, the assignment may not be deleted by DCE
  if (lhs->Is_flag_set((CR_FLAG) CF_DONT_PROP))
    return FALSE;
  // if not identity assignment, make sure there is no chi other than itself;
  // otherwise, the assignment may not be deleted by DCE
  FOR_ALL_NODE(cnode, chi_iter, Init(stmt->Chi_list())) {
    if (! cnode->Live()) continue;
//  if (cnode->Aux_id() != lhs->Aux_id()) 
    return FALSE;
  }
  return TRUE;
}

// ====================================================================
//  Invertible_occurrences - var must be a CK_VAR node; if expr contains
//  operations that has no 100% accurate inverse fucntion, return -1;
//  if expr has any occurrence of variable other than var and is not current
//  version, return -1;
//  otherwise, return the number of occurrences of var in expr.
// ====================================================================
INT32
COPYPROP::Invertible_occurrences(CODEREP *var, CODEREP *cr)
{
  switch (cr->Kind()) {
  case CK_LDA:
    return -1;
  case CK_CONST:
    return 0;
  case CK_RCONST: 
    return -1;
  case CK_VAR:
    if (cr == var)
      return 1;

    // check if it is volatile
    if (cr->Is_var_volatile())
      return -1;

    // check if it is a physical register
    {
      ST *s = Opt_stab()->St(cr->Aux_id());
      if ((ST_class(s) == CLASS_PREG && Preg_Is_Dedicated(cr->Offset())))
        return -1;
    }

    if (Opt_stab()->NULL_coderep(cr->Aux_id()) ||
        cr == Opt_stab()->Top_coderep(cr->Aux_id()))
      return 0;
    return -1;
  case CK_IVAR:
    return -1;
  case CK_OP:
    if (! MTYPE_IS_INTEGER(cr->Dtyp()))
      return -1;
    if (cr->Opr() == OPR_NEG)
      return Invertible_occurrences(var, cr->Opnd(0));
#ifndef TARG_IA64 // IA-64 has post-incr/decr, so suppress inverse function
    if (cr->Opr() == OPR_ADD || cr->Opr() == OPR_SUB) {
      INT32 res0, res1;

      res0 = Invertible_occurrences(var, cr->Opnd(0));
      if (res0 == -1)
	return -1;
      res1 = Invertible_occurrences(var, cr->Opnd(1));
      if (res1 == -1 || res0 + res1 > 1)
	return -1;
      return res0 + res1;
    }
#endif
  }
  return -1;
}

// ====================================================================
//  Is_function_of_cur - var must be a CK_VAR node; if it can be expressed
//  as a function of its current version, cur_var, then return TRUE.
// ====================================================================
BOOL
COPYPROP::Is_function_of_cur(CODEREP *var, CODEREP *cur_var)
{
  if (cur_var == NULL) return FALSE;
  if (cur_var->Is_flag_set((CR_FLAG) (CF_DEF_BY_PHI | CF_DEF_BY_CHI)))
    return FALSE;
  STMTREP *dstmt = cur_var->Defstmt();
  if (dstmt == NULL) return FALSE;
#ifdef KEY // bug 11440: don't reverse the effect of i=i propagation
  if (dstmt->Is_identity_assignment_removable() && WOPT_Enable_DCE)
    return FALSE;
#endif
  return Invertible_occurrences(var, dstmt->Rhs()) == 1;
}

// ====================================================================
//  CODEREP::Propagatable - if the expr x can be propagated to the current
//  point in the program; if chk_inverse if TRUE, is live range overlap, try
//  to see if the previous version can be expressed in terms of the current
//  version to allow the expr to be propagated; propagating_var is used only if
//  chk_inverse is TRUE; it gives the propagating variable, and 
//  Is_function_of_cur is not called if it is the propagating variable.
//
//  If inside_cse is true, will expression containing iload whose mu is
//  the default vsym is not propagatable because it may mess up cse; if
//  in icopy phase, inside_cse is always true because cse is still forming,
//  so don't know. The parameter height returns to the caller the height
//  of the tree; if not propagatable, height does not need to be set.
//
//  If in_array is TRUE, it means that "x" is a subexpression of one
//  of the index expressions.  We may keep some expressions from being
//  propagatable into these indexes.
// 
//  If no_complex_preg is TRUE, x must be directly underneath a PARM.  In this
//  case, x should not be propagated to a complex preg, since a complex variable
//  directly under a PARM is not lowered before mainopt, and this can cause 
//  lowered STIDs to the pregs for the real and imag parts to be wrongly 
//  deleted by DCE (bug 10577).
//  
// ====================================================================
PROPAGATABILITY
COPYPROP::Propagatable(CODEREP *x, BOOL chk_inverse,
		       AUX_ID propagating_var, BOOL icopy_phase, 
		       BOOL inside_cse, INT32 *height, INT32 *weight,
		       BOOL in_array, BB_NODE *curbb)
{
  PROPAGATABILITY prop, prop0;
  INT32 height0 = 0;
  INT32 weight0 = 0;
  switch (x->Kind()) {
  case CK_LDA: 
  case CK_CONST: 
  case CK_RCONST:
    *height = 1;
    *weight = 1;
    return PROPAGATABLE;
  case CK_VAR:
    // check if it is volatile
    if (x->Is_var_volatile())
      return NOT_PROPAGATABLE;

    // check if it is a physical register
    {
      ST *s = Opt_stab()->St(x->Aux_id());
      if ((ST_class(s) == CLASS_PREG && Preg_Is_Dedicated(x->Offset())))
        return NOT_PROPAGATABLE;
    }

    *height = 1;
    *weight = 1;
    if (! Opt_stab()->NULL_coderep(x->Aux_id()) && 
        x != Opt_stab()->Top_coderep(x->Aux_id()))
      if (! chk_inverse || x->Aux_id() == propagating_var ||
	  MTYPE_size_min(x->Dsctyp()) < 32)
        return NOT_PROPAGATABLE;
      else if (Is_function_of_cur(x, Opt_stab()->Top_coderep(x->Aux_id())))
        return PROP_WITH_INVERSE;
      else return NOT_PROPAGATABLE;

    return PROPAGATABLE;

  case CK_IVAR: 
    {
      if (! WOPT_Enable_Iload_Prop) return NOT_PROPAGATABLE;
      if (! WOPT_Enable_Prop_CSE &&
	  inside_cse && 
          x->Ivar_occ() && 
	  Opt_stab()->Default_vsym() != 0 &&
	  (x->Ivar_occ()->Aux_id() == Opt_stab()->Default_vsym() ||
	   Opt_stab()->Rule()->Aliased_Memop(
	     x->Ivar_occ()->Points_to(), 
	   Opt_stab()->Points_to(Opt_stab()->Default_vsym()))))
        return NOT_PROPAGATABLE;

      // if not checking version, never prop ivars because cannot fix any 
      // resulting overlapped live ranges
      if (x->Is_ivar_volatile()) 
        return NOT_PROPAGATABLE; 

      CODEREP *ivar_vsym = x->Get_ivar_vsym();
      if (ivar_vsym != NULL && ivar_vsym->Is_var_volatile())
        return NOT_PROPAGATABLE;

      CODEREP *base = x->Ilod_base();
      prop = Propagatable(base, chk_inverse, propagating_var, 
                          icopy_phase, inside_cse, height, weight, in_array, curbb);
      // if base is PROP_WITH_INVERSE, make not propagatable because the new
      // ILOAD cannot be CSE'ed
      if (prop == NOT_PROPAGATABLE || prop == PROP_WITH_INVERSE)
        return NOT_PROPAGATABLE;
      if (x->Opr() == OPR_MLOAD)
        return NOT_PROPAGATABLE;
      if (x->Opr() == OPR_ILOADX) {
        prop0 = Propagatable(x->Index(), chk_inverse, propagating_var, 
                             icopy_phase, inside_cse, &height0, &weight0, in_array, curbb);
        prop = MIN(prop, prop0);
        *height = MAX(*height, height0);
	*weight += weight0;
        if (prop == NOT_PROPAGATABLE) return NOT_PROPAGATABLE;
      }
    
      MU_NODE *mnode = x->Ivar_mu_node();
      if (mnode) {
        CODEREP *opnd = mnode->OPND();
        if (opnd) {
          if (! Opt_stab()->NULL_coderep(opnd->Aux_id()) &&
              opnd != Opt_stab()->Top_coderep(opnd->Aux_id()))
            return NOT_PROPAGATABLE;
        }
      }
      (*height)++;
      (*weight)++;
      return prop;
    }
  case CK_OP: {
    if (OPERATOR_is_volatile(x->Opr()))
      return NOT_PROPAGATABLE;

#ifdef TARG_SL
    // temporary hack
    if (x->Opr() == OPR_INTRINSIC_OP && x->Is_C3_Intrinsic())
      return NOT_PROPAGATABLE;

    if (MTYPE_is_float(x->Dtyp()) || MTYPE_is_float(x->Dsctyp()))
      return NOT_PROPAGATABLE;
#endif
    // intrinsic op may by lowered into a call, so propagating it past the
    // def of a return preg is wrong
    if (Past_ret_reg_def() && (x->Opr() == OPR_INTRINSIC_OP
#ifdef KEY
        || x->Opr() == OPR_PURE_CALL_OP
#endif
       ))
      return NOT_PROPAGATABLE;

#if !defined(TARG_IA64)
    // These code are not applied on TARG_IA64 since it will cause compilation time out
    // in RVI2 phase during building 403.gcc.
    // Too aggressive copy propagation may increase the compilation time in later phases
    if (icopy_phase) {
      if (x->Is_isop_flag_set(ISOP_ICOPY_VISITED)) {
	*height = 1;	// don't really know the height, so return 1
	*weight = 1;
        return (PROPAGATABILITY) (0x3 & x->Propagatability()); // to prevent sign extension
      }
      else {
	x->Set_isop_flag(ISOP_ICOPY_VISITED);
        Add_visited_node(x);
      }
    }
    else {
      if (x->Is_isop_flag_set(ISOP_COPY_VISITED)) {
	*height = 1;	// don't really know the height, so return 1
	*weight = 1;
        return (PROPAGATABILITY) (0x3 & x->Propagatability()); // to prevent sign extension
      }
      else {
	x->Set_isop_flag(ISOP_COPY_VISITED);
        Add_visited_node(x);
      }
    }
#else
    if (icopy_phase) {
      if (!x->Is_isop_flag_set(ISOP_ICOPY_VISITED)) {
        x->Set_isop_flag(ISOP_ICOPY_VISITED);
        Add_visited_node(x);
      }
    }
    else {
      if (!x->Is_isop_flag_set(ISOP_COPY_VISITED)) {
        x->Set_isop_flag(ISOP_COPY_VISITED);
        Add_visited_node(x);
      }
    }
#endif

    // determine if there are ops that we are not allowed to propagate 
    // into an array subscript
    // index expression
    if ( in_array && ! WOPT_Enable_Copy_Prop_Ops_Into_Array ) {
      const OPERATOR x_opr = x->Opr();
      // list out all operators that are not allowed to be propagated
      // into an array subscript
      if ( x_opr == OPR_ARRAY || x_opr == OPR_SELECT ||
           x_opr == OPR_MIN || x_opr == OPR_MAX || x_opr == OPR_MINMAX ) 
      {
	x->Set_propagatability(NOT_PROPAGATABLE);
	return NOT_PROPAGATABLE;
      }
    }

#if defined(TARG_IA32) || defined(TARG_X8664)
    // do not allow SELECT to be propagated
    const OPERATOR x_opr = x->Opr();
    if (x_opr == OPR_SELECT ||
        x_opr == OPR_MIN || x_opr == OPR_MAX || x_opr == OPR_MINMAX) 
    { 
      x->Set_propagatability(NOT_PROPAGATABLE);
      return NOT_PROPAGATABLE;
    }
#endif

    prop = PROPAGATABLE;
    *height = 0;
    *weight = 0;
    for  (INT32 i = 0; i < x->Kid_count(); i++) {
      prop0 = Propagatable(x->Opnd(i), chk_inverse, propagating_var, 
			   icopy_phase, inside_cse, &height0, &weight0,
			   in_array, curbb);
      prop = MIN(prop, prop0);
      *height = MAX(*height, height0);
      *weight += weight0;
      if (prop == NOT_PROPAGATABLE) {
	x->Set_propagatability(NOT_PROPAGATABLE);
	return NOT_PROPAGATABLE;
      }
    }
    if (! Op_can_be_propagated(x->Op(), Htable()->Phase())) {
      x->Set_propagatability(NOT_PROPAGATABLE);
      return NOT_PROPAGATABLE;
    }

    if (*height >= WOPT_Enable_Prop_Limit || 
	*weight >= WOPT_Enable_Prop_Weight_Limit) { // exceeds prop limit
      x->Set_propagatability(NOT_PROPAGATABLE);
      return NOT_PROPAGATABLE;
    }
      
    x->Set_propagatability(prop);
    (*height)++;
    (*weight)++;
    return prop;
    }
  }
  return NOT_PROPAGATABLE;
}

// ====================================================================
//  CODEMAP::Rehash - rehash and re-insert expression in hash table;
//  only apply to non-leaves; this routine is modeled after CODEMAP::Add_expr
// ====================================================================
CODEREP *
CODEMAP::Rehash(CODEREP *cr, BOOL canonicalize)
{
  Is_True(cr->Coderep_id() == 0 || cr->Is_flag_set(CF_OWNED_BY_TEMP),
	  ("CODEMAP:: Rehash a CR with a 0 coderep ID."));
  switch (cr->Kind()) {
  case CK_VAR:	
    FmtAssert(FALSE, ("cannot call rehash with CK_VAR."));
  case CK_LDA:	   return Hash_Lda(cr);
  case CK_CONST:   return Hash_Const(cr);
  case CK_RCONST:  return Hash_Rconst(cr);
  case CK_IVAR:    return Hash_Ivar(cr);
  case CK_OP:
    CODEREP *retv = Hash_Op(cr, canonicalize);
    FmtAssert(retv->Opr() != OPR_ILOADX,
	      ("CODEMAP::Rehash: should not see ILOADX in Copy_propagate"));
    return retv;
  }
  FmtAssert(FALSE,("CODEMAP::Rehash, unknown kind"));
  return NULL; // to satisfy compiler
}

/* CVTL-RELATED start (correctness) */
// ====================================================================
// CODEREP::Convert_type - "this", a memory variable, is being substituted by
// "expr"; it potentially can generate 2 CVT/CVTL's.  The first is to simulate 
// the truncation effect of the assignment statement based on which we perform 
// propagation.  The second is to simulate the sign-extension effect when we 
// load the variable being substituted.  If "this" is a bit-field, a third
// CVTL can be generated.
// ====================================================================
CODEREP *
CODEREP::Convert_type(CODEMAP *htable, CODEREP *expr, BOOL icopy_phase)
{
  CODEREP *cr = Alloc_stack_cr(0);
  FOLD     ftmp;
  OPCODE   opc;
  INT      cvt_kind;
  MTYPE    rhs_type = expr->Dtyp();
  MTYPE    dsc_type = Dsctyp();

#ifdef KEY // bug 2668: screen out obvious case where no conversion is needed
  if (expr->Kind() == CK_VAR && Kind() == CK_VAR && expr->Aux_id() == Aux_id()
      && /* bug 10220 */ ! Is_flag_set(CF_IS_ZERO_VERSION)
      && /* bug 11616 */ MTYPE_signed(expr->Dtyp()) == MTYPE_signed(Dtyp()))
    return expr;
#endif
  if ( MTYPE_is_integral(rhs_type) && MTYPE_is_integral(dsc_type) ) {
    cvt_kind = NOT_AT_ALL;
    if (dsc_type == MTYPE_BS)
      ;
    else if ( WOPT_Enable_Min_Type && 
	( expr->Kind() == CK_VAR ||
	 ( expr->Kind() == CK_IVAR && expr->Ivar_has_e_num() ) )  &&
        MTYPE_size_min(dsc_type) >= MTYPE_size_min(expr->Dsctyp()) &&
	Is_sign_extd() == expr->Is_sign_extd() )
      ;
    else {
      if ( MTYPE_size_min(dsc_type) < MTYPE_size_min(rhs_type) ) { 
        // truncation
        cvt_kind = Need_type_conversion(rhs_type, dsc_type, &opc);
        if ( WOPT_Enable_Min_Type && 
             expr->Kind() == CK_VAR &&
	     No_truncation_by_value_size(dsc_type, Is_sign_extd(), expr, htable->Sym(), !icopy_phase) )
          cvt_kind = NOT_AT_ALL;
      }
    }
  } else { // conversion between int, float, and complex
    cvt_kind = Need_type_conversion(rhs_type, dsc_type, &opc);
  }

  switch (cvt_kind) {
  case NOT_AT_ALL:
    break;
  case NEED_CVT:
    if ((opc == OPC_U4U8CVT || opc == OPC_U4I8CVT) && Dtyp() == MTYPE_U8) {
      // Generate (U8CVTL 32) high word zero extended value and return     
      opc = OPC_U8CVTL;
      cr->Init_expr(opc, expr);
      cr->Set_offset(MTYPE_size_min(dsc_type));
      expr = ftmp.Fold_Expr(cr);
      if (!expr) {
	expr = htable->Rehash(cr);
	expr->Set_isop_flag(ISOP_FOLD_EXPR_VISITED);
      }
    } else {
      cr->Init_expr(opc, expr);
      expr = ftmp.Fold_Expr(cr);
      if (!expr) {
	expr = htable->Rehash(cr);
	expr->Set_isop_flag(ISOP_FOLD_EXPR_VISITED);
      }
    }
    break;
  case NEED_CVTL:
    cr->Init_expr(opc, expr);
    cr->Set_offset(MTYPE_size_min(dsc_type));
    expr = ftmp.Fold_Expr(cr);
    if (!expr) {
      expr = htable->Rehash(cr);
      expr->Set_isop_flag(ISOP_FOLD_EXPR_VISITED);
    }
    break;
  }

  // sign extension at the load
  // expr->Dtyp() == Dsctyp() at this point
  if (Dsctyp() != MTYPE_BS &&
      MTYPE_size_min(Dsctyp()) != MTYPE_size_min(Dtyp()) &&
      MTYPE_size_min(Dsctyp()) >= MTYPE_size_min(MTYPE_I4)) {
    INT cvt_kind = Need_type_conversion(Dsctyp(), Dtyp(), &opc);
    switch (cvt_kind) {
    case NOT_AT_ALL:
      break;
    case NEED_CVT:
      cr->Init_expr(opc, expr);
      expr = ftmp.Fold_Expr(cr);
      if (!expr) {
	expr = htable->Rehash(cr);
	expr->Set_isop_flag(ISOP_FOLD_EXPR_VISITED);
      }
      break;
    case NEED_CVTL:
      Is_True(FALSE, ("CODEREP::Convert_type: should not reach here"));
      break;
    }
  }

  INT varsize;				// in bits
  if (Dsctyp() == MTYPE_BS) {
    if (Kind() == CK_VAR) {
      if (expr->Kind() == CK_VAR && Aux_id() == expr->Aux_id() &&
          (MTYPE_signed(Dtyp()) == MTYPE_signed(expr->Dtyp()) ||
	   expr->Is_flag_set(CF_INCOMPLETE_USES)))
	return expr;  // it's an identity assignment propagation (bug 11732)
      AUX_STAB_ENTRY *aux = htable->Sym()->Aux_stab_entry(Aux_id());
      varsize = aux->Bit_size();
    }
    else { // Kind() == CK_IVAR
      UINT cur_field_id = 0;
      UINT64 field_offset = 0;
      FLD_HANDLE fld = FLD_And_Offset_From_Field_Id(Ilod_ty(), I_field_id(),
					    cur_field_id, field_offset);
      varsize = FLD_bsize(fld);
    }
  }
  else if (Kind() == CK_VAR && Bit_field_valid()) {
    if (expr->Kind() == CK_VAR && Aux_id() == expr->Aux_id() &&
        (MTYPE_signed(Dtyp()) == MTYPE_signed(expr->Dtyp()) ||
	 expr->Is_flag_set(CF_INCOMPLETE_USES)))
      return expr;  // it's an identity assignment propagation (bug 11732)
    varsize = Bit_size();
  }
  else if (Kind() == CK_IVAR && Opr() == OPR_ILDBITS)
    varsize = I_bit_size();
  else return expr;
  // generate an additional CVTL to simulate the bits truncation
  cr->Init_expr(OPCODE_make_op(OPR_CVTL, Dtyp(), MTYPE_V), expr);
  cr->Set_offset(varsize);
  expr = ftmp.Fold_Expr(cr);
  if (!expr) {
    expr = htable->Rehash(cr);
    expr->Set_isop_flag(ISOP_FOLD_EXPR_VISITED);
  }
  return expr;
}
/* CVTL-RELATED finish */

// ====================================================================
// Prop_identity_assignment - cr is a CK_VAR node being referenced; check if
// it is defined at an identity assignment; if so, return the rhs version of
// the identity assignment; else return NULL.
//
// This is required to eliminate redundant assignments of the form I=I.
// The form is generated in the IVR phase to faciliate the replacement of I
// if the trip count can be determined.  IVR relies on COPYPROP and DCE to
// remove the redundant assignment if the trip count is not determined.
// ====================================================================
//
//  DONT CHANGE THIS FUNCTION ---
//
//    Any extra conditions must be in Is_identity_assignment_removeable()
//    so that COPYPROP and DCE is working on the same set of 
//    "identity assignments".
//
static CODEREP *
Prop_identity_assignment(CODEREP *cr)
{
  STMTREP *dstmt;
  if (cr->Is_flag_set(CF_DEF_BY_PHI)) 
    return NULL;
  if (cr->Is_flag_set(CF_DEF_BY_CHI)) {
    dstmt = cr->Defstmt();
    if (dstmt != NULL && 
	dstmt->Is_identity_assignment_removable() &&
	WOPT_Enable_DCE)
      return cr->Defchi()->OPND();
  } else {
    if ((dstmt = cr->Defstmt()) != NULL && 
	dstmt->Is_identity_assignment_removable() &&
	WOPT_Enable_DCE)
      return dstmt->Rhs();
  }
  return NULL;  
}

// ====================================================================
// Form_inverse - x is an expression tree containing v; form and return the
// inverse of this expression based on the current version of the variable
// by decending the tree rooted at x; forming_x is the tree being constructed;
// construction is completed when it reaches the v node inside x; x contains 
// one and only one occurrence of v.
// ====================================================================
CODEREP *
COPYPROP::Form_inverse(CODEREP *v, CODEREP *x, CODEREP *forming_x)
{
  switch (x->Kind()) {
  case CK_CONST:
  case CK_RCONST:
  case CK_LDA:
  case CK_IVAR:
    Is_True(FALSE, ("Form_inverse: illegal node."));
  case CK_VAR:
    if (x == v)
      return forming_x;
    return x;
  case CK_OP: {
    CODEREP *cr = Alloc_stack_cr(x->Extra_ptrs_used());
    CODEREP *res;
    FOLD ftmp;

    Is_True(MTYPE_IS_INTEGER(x->Dtyp()), ("Form_inverse: illegal node."));
    if (x->Opr() == OPR_NEG) { 
      // ..i2..  = - ( ..i1.. ) becomes - ( ..i2.. ) = ( ..i1.. )
      cr->Copy(*x);
      cr->Set_usecnt(0);
      forming_x = forming_x->Fixup_type(cr->Dtyp(), Htable());
      cr->Set_opnd(0, forming_x);
      res = ftmp.Fold_Expr(cr);
      if (res == NULL) { // either not folded or Fold_Expr has not rehashed
	res = Htable()->Rehash(cr);
        res->Set_isop_flag(ISOP_FOLD_EXPR_VISITED);
      }
      return Form_inverse(v, x->Opnd(0), res);
    }
    if (x->Opr() == OPR_ADD) {
      cr->Copy(*x);
      cr->Set_usecnt(0);
      forming_x = forming_x->Fixup_type(cr->Dtyp(), Htable());
      cr->Set_opnd(0, forming_x);
      cr->Set_opr(OPR_SUB);
      if (Invertible_occurrences(v, x->Opnd(0)) == 0) {
	// ( ..i2.. ) = y + ( ..i1.. ) becomes  ( ..i2.. ) - y = ( ..i1.. )
        cr->Set_opnd(1, x->Opnd(0));
        res = ftmp.Fold_Expr(cr);
        if (res == NULL) { // either not folded or Fold_Expr has not rehashed
	  res = Htable()->Rehash(cr);
	  res->Set_isop_flag(ISOP_FOLD_EXPR_VISITED);
	}
	return Form_inverse(v, x->Opnd(1), res);
      }
      else {
	// ( ..i2.. ) = ( ..i1.. ) + y  becomes  ( ..i2.. ) - y = ( ..i1.. )
        cr->Set_opnd(1, x->Opnd(1));
        res = ftmp.Fold_Expr(cr);
        if (res == NULL) { // either not folded or Fold_Expr has not rehashed
	  res = Htable()->Rehash(cr);
	  res->Set_isop_flag(ISOP_FOLD_EXPR_VISITED);
	}
	return Form_inverse(v, x->Opnd(0), res);
      }
    }
    if (x->Opr() == OPR_SUB) {
      cr->Copy(*x);
      cr->Set_usecnt(0);
      forming_x = forming_x->Fixup_type(cr->Dtyp(), Htable());
      if (Invertible_occurrences(v, x->Opnd(0)) == 0) {
	// ( ..i2.. ) = y - ( ..i1.. ) becomes y - ( ..i2.. ) = ( ..i1.. )
	cr->Set_opr(x->Opr());
        cr->Set_opnd(0, x->Opnd(0));
        cr->Set_opnd(1, forming_x);
        res = ftmp.Fold_Expr(cr);
        if (res == NULL) { // either not folded or Fold_Expr has not rehashed
	  res = Htable()->Rehash(cr);
	  res->Set_isop_flag(ISOP_FOLD_EXPR_VISITED);
	}
	return Form_inverse(v, x->Opnd(1), res);
      }
      else {
	// ( ..i2.. ) = ( ..i1.. ) - y  becomes  ( ..i2.. ) + y = ( ..i1.. )
        cr->Set_opr(OPR_ADD);
        cr->Set_opnd(0, forming_x);
        cr->Set_opnd(1, x->Opnd(1));
        res = ftmp.Fold_Expr(cr);
        if (res == NULL) { // either not folded or Fold_Expr has not rehashed
	  res = Htable()->Rehash(cr);
	  res->Set_isop_flag(ISOP_FOLD_EXPR_VISITED);
	}
	return Form_inverse(v, x->Opnd(0), res);
      }
    }
    Is_True(FALSE, ("Inverse_of: illegal CK_OP node."));
  }
  }
  return NULL;
}

// ====================================================================
// Rehash_inverted_expr - Descend the expression tree; at the var node,
// replace it by its inverse function based on the current version of the
// variable; on the way back up, rehash each internal node and return the
// pointer to the root of the re-hashed subtree; if the tree is unchanged,
// return NULL.. 
// ====================================================================
CODEREP *
COPYPROP::Rehash_inverted_expr(CODEREP *x, BOOL icopy_phase)
{
  CODEREP *cr = Alloc_stack_cr(x->Extra_ptrs_used());
  BOOL need_rehash;
  CODEREP *res;
  FOLD ftmp;

  switch (x->Kind()) {
  case CK_CONST:
  case CK_LDA:
  case CK_RCONST: 
    return NULL;
  case CK_VAR:  // replace it by the inverse of the expression that defines
		// the current version
    if (!Opt_stab()->NULL_coderep(x->Aux_id()) && 
	x != Opt_stab()->Top_coderep(x->Aux_id())) {
      CODEREP *v = Opt_stab()->Top_coderep(x->Aux_id());
      CODEREP *expr = v->Defstmt()->Rhs();
      expr = Form_inverse(x, expr, v);
      expr = v->Convert_type(Htable(), expr, icopy_phase);
      return expr;
    } else return NULL;
  case CK_IVAR: {
    cr->Copy(*x);
    cr->Set_usecnt(0);
    res = Rehash_inverted_expr(x->Ilod_base(), icopy_phase);
    if (res == NULL)
      return NULL;
    INT64 ofst = cr->Offset();
    CODEREP *canon_res = OPERATOR_is_load(x->Opr())
      && Use_Load_Store_Offset && x->Istr_base() == NULL ?  
      // disable offset canon if both ilod_base and istr_base are non-NULL.
      Htable()->Canon_base(res, &ofst) : Htable()->Canon_rhs(res);
    if (canon_res) {
      res = canon_res;
      cr->Set_offset(ofst);
    }
    cr->Set_ilod_base(res);
    cr->Set_istr_base(NULL);
    return Htable()->Rehash(cr);
    }
  case CK_OP: {
    cr->Copy(*x);
    cr->Set_usecnt(0);
    need_rehash = FALSE;

    OPERATOR opr = cr->Opr();
    if (opr != OPR_ADD && opr != OPR_SUB && opr != OPR_NEG && opr != OPR_MPY) {
      for (INT32 i = 0; i < x->Kid_count(); i++) {
	res = Rehash_inverted_expr(x->Opnd(i), icopy_phase);
	if (res) {
	  need_rehash = TRUE;
	  CODEREP *canon_res = Htable()->Canon_rhs(res);
	  if (canon_res)
	    cr->Set_opnd(i, canon_res);
	  else
	    cr->Set_opnd(i, res);
	}
	else cr->Set_opnd(i, x->Opnd(i));
      }
    } else {
      for (INT32 i = 0; i < x->Kid_count(); i++) {
	res = Rehash_inverted_expr(x->Opnd(i), icopy_phase);
	if (res) {
	  need_rehash = TRUE;
	  cr->Set_opnd(i, res);
	}
	else cr->Set_opnd(i, x->Opnd(i));
      }
    }
    if (! need_rehash) 
      return NULL;
    res = ftmp.Fold_Expr(cr);
    if (res == NULL) { // either not folded or Fold_Expr has not rehashed
      res = Htable()->Rehash(cr);
      res->Set_isop_flag(ISOP_FOLD_EXPR_VISITED);
    }
    return res;
    }
  }
  return NULL;
}

// ====================================================================
//  COPYPROP::Propagated_to_loop_branch - check if the propagation cause
//  something from outside the loop to be propagated to a branch in the loop;
//  if so, return the bb where the eval should be inserted.
// ====================================================================
BB_NODE *
COPYPROP::Propagated_to_loop_branch(BB_NODE *srcbb, BB_NODE *destbb)
{
  BB_LOOP *loop;
  if (destbb->Loopdepth() == 0)	// not inside any loop
    return NULL;
  if (srcbb->Loopdepth() >= destbb->Loopdepth()) // srcbb outside the loop
    return NULL;
  loop = destbb->Innermost();
  if (Htable()->Phase() == MAINOPT_PHASE) {
    // get the outermost loop that encloses destbb but not srcbb 
    while (loop->Parent() != NULL && loop->Parent() != srcbb->Innermost()) 
      loop = loop->Parent();
  }
  // for preopt, condition is weaker by only checking the innermost loop

  if (loop->Body() == NULL)   // Not a DO, WHILE_DO, or DO_WHILE loop
    return NULL;              // TO DO: Change opt_cfg:Allocate_loop instead
  if (destbb->Postdominates(loop->Body()))
    return NULL;
  // for preopt, the bb containing the while condition is not loop branch
  if (Htable()->Phase() != MAINOPT_PHASE && destbb == loop->End())
    return NULL;
  return loop->Dohead();
}


// Replace CK_VAR or CK_IVAR with their constant initialized value.
// x is  CK_VAR st  or
//       CK_IVAR CK_LDA st
// and var_aux_id is corr opt_stab entry for the st.
//
CODEREP *
COPYPROP::Prop_const_init_scalar(CODEREP *x, AUX_ID var_aux_id)
{
#ifdef Is_True_On
  if (x->Kind() == CK_VAR) {
    Is_True(var_aux_id == x->Aux_id(),
	    ("COPYPROP::Prop_const_init_scalar: wrong aux_id."));
  } else if (x->Kind() == CK_IVAR) {
    Is_True(x->Offset() == 0, 
	    ("COPYPROP::Prop_const_init_scalar: wrong aux_id."));
  } else {
    Is_True(FALSE, ("COPYPROP::Prop_const_init_scalar: wrong tree."));
  }
#endif

  AUX_STAB_ENTRY *psym = Opt_stab()->Aux_stab_entry(var_aux_id);

  // is this variable a constant initialized scalar?
  BOOL const_initialized = 
    psym->Is_flag_const_init() &&
#if defined(TARG_NVISA)
    !psym->Is_volatile();
#else
    !psym->Is_volatile() &&
    psym->St_ofst() == 0;  // a limitation for matching INITV or TCON
                           // should make it smarter to look into block of INITV.
#endif

  if (const_initialized) {
    TCON init_tcon;
    ST *st = psym->St();
    if (ST_is_const_initialized_scalar(st, psym->St_ofst(), init_tcon))
      {
	// if the ST's initialized value can be represented with a TCON
	// first convert the init value to the type of the variable
	if ( x->Dsctyp() != TCON_ty(init_tcon) ) {
	  // init_tcon = Targ_Conv( x->Dsctyp(), init_tcon );
	  // Do not use Targ_Conv.  Consider when the INITO is a
	  // structure type and x is a member of the structure.
	  Warn_todo("Prop_const_init_scalar: should copy the bits instead of targ_conv.");
	  return NULL;
	}
	// then convert it to the type of the result
	if ( Get_Trace(TP_GLOBOPT, PROP_DUMP_FLAG)) {
	  fprintf(TFile, "Prop_const_init_scalar:  replacing LDID/ILOAD-LDA aux %d with TCON\n",
		  var_aux_id);
	}
	if ( x->Dtyp() != TCON_ty(init_tcon) )
	  init_tcon = Targ_Conv(x->Dtyp(), init_tcon);
	TCON_IDX tcon_idx = Enter_tcon(init_tcon);
	return Htable()->Add_tcon(tcon_idx);
      }
    else {
      if (INITV_IDX initv = ST_is_const_and_has_initv(st)) {
	// if the ST's initialized value is a INITV (e.g. SYMOFF)
	if (MTYPE_size_min(x->Dsctyp()) == MTYPE_size_min(Pointer_type) &&
	    MTYPE_size_min(x->Dtyp()) == MTYPE_size_min(Pointer_type) &&
	    INITV_kind(Initv_Table[initv]) == INITVKIND_SYMOFF &&
	    ST_class(&St_Table[INITV_st(Initv_Table[initv])]) == CLASS_VAR) {
	  AUX_ID aux_id =
	    Opt_stab()->Find_sym_with_st_and_ofst(&St_Table[INITV_st(Initv_Table[initv])],
						  INITV_ofst(Initv_Table[initv]));
	  Is_True(aux_id != 0, ("COPY_PROP::const_init_scalar: can't find opt_stab entry."));
	  if (Get_Trace(TP_GLOBOPT, PROP_DUMP_FLAG)) {
	    fprintf(TFile, "Prop_const_init_scalar:  replacing LDID/ILOAD-LDA aux %d with LDA %d\n", 
		    var_aux_id,  aux_id);
	  }
	  AUX_STAB_ENTRY *psym2 = Opt_stab()->Aux_stab_entry(aux_id);
	  CODEREP  *cr = Alloc_stack_cr(0);
	  cr->Init_lda(Pointer_type,
		       aux_id,
		       psym2->St_ofst(),     // redundant info
		       ST_type(psym2->St()), // redundant info
		       psym2->St());         // redundant info
	  return Htable()->Rehash(cr, FALSE);
	}
      }
    }
  }
  return NULL;
}

// ====================================================================
//  COPYPROP::Prop_var - return the new root of the tree if copy 
//  propagation results in a new tree
// ====================================================================
CODEREP *
COPYPROP::Prop_var(CODEREP *x, BB_NODE *curbb, BOOL icopy_phase, 
		   BOOL inside_cse, BOOL in_array,
		   BOOL no_complex_preg)
{
  STMTREP *stmt;
  CODEREP *expr;
  MTYPE    expr_ty;

  // is this variable a constant initialized scalar?

  if (x->Is_var_volatile()) return NULL;

  // disable copy prop for SPRE and EPRE/LPRE temps
  if (Opt_stab()->Aux_stab_entry(x->Aux_id())->SPRE_temp() ||
      Opt_stab()->Aux_stab_entry(x->Aux_id())->LPRE_VNFRE_temp() ||  
      Opt_stab()->Aux_stab_entry(x->Aux_id())->EPRE_temp())
    return NULL;

  CODEREP *retv = Prop_const_init_scalar(x, x->Aux_id());
  if (retv) {
    if (icopy_phase)
      Htable()->Inc_inputprops();
    else 
      Htable()->Inc_mainprops();
    return retv;
  }

  if (x->Is_flag_set((CR_FLAG)(CF_DEF_BY_PHI|CF_DEF_BY_CHI)) )
    return NULL;

#ifdef KEY // bug 6220: do not propagate anything to the fields of a dope vector
  if (Htable()->Phase() != PREOPT_IPA0_PHASE && 
      Htable()->Phase() != PREOPT_IPA1_PHASE &&
      ! WOPT_Enable_Prop_Dope) {
    ST *st = Opt_stab()->St(x->Aux_id());
    if (ST_class(st) == CLASS_VAR && 
	TY_kind(ST_type(st)) == KIND_STRUCT &&
	strncmp(TY_name(ST_type(st)), ".dope.", 6) == 0)
      return NULL;
  }
#endif

  stmt = x->Defstmt();
  if (! stmt) return NULL;

  expr = stmt->Rhs();

#ifdef TARG_IA64
  // disable copy prop for float-pt pregs (except for leaf nodes) in
  // mainopt because they may be created by LNO for cross-iteration CSEs,
  // whose assignments wopt will not be able to delete
  if (Htable()->Phase() == MAINOPT_PHASE &&
      MTYPE_is_float(x->Dtyp()) &&
      ST_class(Opt_stab()->St(x->Aux_id())) == CLASS_PREG &&
      expr->Is_flag_set((CR_FLAG) CF_DONT_PROP) &&
      (expr->Non_leaf() || expr->Kind() == CK_VAR))
    return NULL;
#endif

  // do not propagate if DONT_PROP flag is set and it's non_leaf node.
  // Currently, input copy propagation is sensitive to BB traversal order,
  // because the DONT_PROP flag is set during coderep construction phase at 
  // each mu encountered.  This can be fixed by uncommenting out the
  // "icopy_phase ||", in which case, when icopy is true, it's like assuming
  // everything is DONT_PROP, with the consequence of less copy propagation.
  if ((/*icopy_phase ||*/ x->Is_flag_set((CR_FLAG) CF_DONT_PROP)) &&
      (expr->Non_leaf() ||
       (expr->Kind() == CK_VAR && expr->Aux_id() != x->Aux_id())))
    return NULL;

#ifdef KEY // bug 5131
  if (x->Mp_shared())
    return NULL;
#endif

#ifdef KEY // bug 1596: do not propagate an ARRAY node to the base field of
	   // 		a dope vector
  if (expr->Kind() == CK_OP && expr->Opr() == OPR_ARRAY) {
    ST *st = Opt_stab()->St(x->Aux_id());
    if (ST_class(st) == CLASS_VAR && 
        TY_kind(ST_type(st)) == KIND_STRUCT &&
        x->Offset() == 0 &&
        strncmp(TY_name(ST_type(st)), ".dope.", 6) == 0)
      return NULL;
  }
#endif

  // Fix 658173: In IPL's preopt, don't propagate through
  // PT_TO_UNIQUE_MEM objects. The reason is that IPL's preopt doesn't
  // write the restricted map, and IPA doesn't make any attempt to
  // preserve it through symbol table merging. Therefore we have no
  // mechanism that would allow us to violate PT_TO_UNIQUE_MEM
  // semantics in IPL's preopt.
  if (Htable()->Phase() == PREOPT_IPA0_PHASE &&
      ST_pt_to_unique_mem(Opt_stab()->Aux_stab_entry(x->Aux_id())->St())) {
    return NULL;
  }

  CODEREP *curversion = NULL;
  if (!Opt_stab()->NULL_coderep(x->Aux_id()))
    curversion = Opt_stab()->Top_coderep(x->Aux_id());

  // if WOPT_Enable_Small_Br_Target is true, do not propagate into a BB that
  // does not post-dominate the assignment statement
  if (WOPT_Enable_Small_Br_Target && expr->Non_leaf() && x == curversion &&
      ! curbb->Postdominates(stmt->Bb()))
    return NULL;

  // do not propagate in preopt if propagation is into branch in a loop
  // and it is not introduced by IVR
  if (expr->Non_leaf() && !stmt->Ivr_introduced() && Htable()->Phase() != MAINOPT_PHASE && 
      Htable()->Phase() != PREOPT_LNO_PHASE &&
      Propagated_to_loop_branch(stmt->Bb(), curbb) && x == curversion)
    return NULL;

  if (Htable()->Phase() != MAINOPT_PHASE &&
      ((curbb->MP_region() || stmt->Bb()->MP_region()) &&
       curbb->Rid_id() != stmt->Bb()->Rid_id() &&
       (!WOPT_Enable_MP_Const_Prop || (expr->Kind() != CK_CONST &&
                                       expr->Kind() != CK_RCONST))) &&
      x == curversion)
    return NULL;

#ifdef KEY // bug 10577
  if (no_complex_preg && expr->Kind() == CK_VAR && 
      MTYPE_is_complex(expr->Dtyp()) &&
      ST_class(Opt_stab()->Aux_stab_entry(expr->Aux_id())->St()) == CLASS_PREG)
    return NULL;
#endif

  BOOL prop;
  INT32 height, weight;
  if (x != curversion) {
    // it is not the current version (due to Is_function_of_itself), so 
    // must propagate
    prop = PROPAGATABLE;
  }
  else {
    prop = Propagatable(expr, WOPT_Enable_Prop_Aggressive, 
			x->Aux_id(), icopy_phase, inside_cse, &height,
			&weight, in_array, curbb);
    if ( WOPT_Enable_LNO_Copy_Propagate && expr->Non_leaf()) {
      MTYPE dtyp = expr->Dtyp();
      if (MTYPE_is_float(dtyp) || MTYPE_is_complex(dtyp) ||
	  OPERATOR_is_scalar_iload (expr->Opr()))
        return NULL;
      MTYPE dsctyp = expr->Dsctyp();
      if (dsctyp != dtyp && 
	  (MTYPE_is_float(dsctyp)||MTYPE_is_complex(dsctyp)))
        return NULL;
    }
    if (prop == NOT_PROPAGATABLE)
      return NULL;
  }

  x->DecUsecnt();
  if (icopy_phase)
    Htable()->Inc_inputprops();
  else Htable()->Inc_mainprops();
  if (prop != PROP_WITH_INVERSE) {
    expr->IncUsecnt();
  }
  else {	// curcronly must be true
    expr = Rehash_inverted_expr(expr, icopy_phase);
  }
  expr = x->Convert_type(Htable(), expr, icopy_phase);
  BB_NODE *insertbb;
  if (expr->Non_leaf() && Htable()->Phase() == MAINOPT_PHASE &&
      prop != PROP_WITH_INVERSE &&
      (insertbb = Propagated_to_loop_branch(stmt->Bb(), curbb)) &&
      stmt->Bb()->Dominates_strictly(insertbb)) {
    // insert an eval at insertbb to promote loop-invariant motion by PRE
    STMTREP *eval_stmt = CXX_NEW(STMTREP(OPC_EVAL), Htable()->Mem_pool());
    expr->IncUsecnt();
    eval_stmt->Set_rhs(expr);
    insertbb->Append_stmtrep(eval_stmt);
  }
  return expr;
}

// ====================================================================
//  CODEREP::Prop_ivar - return the new root of the tree if copy 
//  propagation results in a new tree
// ====================================================================
CODEREP *
COPYPROP::Prop_ivar(CODEREP *x, BB_NODE *curbb, BOOL icopy_phase, 
		    BOOL inside_cse, BOOL in_array,
		    BOOL no_complex_preg)
{

  if (! WOPT_Enable_Prop_Ivar) return NULL;
#ifdef KEY // bug 5804
  if (Htable()->Phase() != MAINOPT_PHASE && PU_has_mp(Get_Current_PU())) 
    return NULL;
#endif
  if (x->Is_ivar_volatile()) return NULL;

  STMTREP *stmt = x->Ivar_defstmt();
  if (stmt == NULL) return NULL;

  CODEREP *expr = stmt->Rhs();
//  MTYPE    expr_ty = stmt->Lhs()->Dsctyp();

  // if WOPT_Enable_Small_Br_Target is true, do not propagate into a BB that
  // does not post-dominate the assignment statement
  if (WOPT_Enable_Small_Br_Target && expr->Non_leaf() && 
      ! curbb->Postdominates(stmt->Bb()))
    return NULL;

  // do not propagate in preopt if propagation is into branch in a loop
  if (expr->Non_leaf() && Htable()->Phase() != MAINOPT_PHASE &&
      Propagated_to_loop_branch(stmt->Bb(), curbb)) 
    return NULL;

#ifdef KEY // bug 10577
  if (no_complex_preg && expr->Kind() == CK_VAR && 
      MTYPE_is_complex(expr->Dtyp()) &&
      ST_class(Opt_stab()->Aux_stab_entry(expr->Aux_id())->St()) == CLASS_PREG)
    return NULL;
#endif

  PROPAGATABILITY prop;
  INT32 height, weight;
  prop = Propagatable(expr, FALSE, 0, 
		      icopy_phase, inside_cse, &height, &weight, in_array, curbb);
  if ( WOPT_Enable_LNO_Copy_Propagate && expr->Non_leaf()) {
    MTYPE dtyp = expr->Dtyp();
    if (MTYPE_is_float(dtyp) || MTYPE_is_complex(dtyp) ||
	OPERATOR_is_scalar_iload (expr->Opr()))
      return NULL;
    MTYPE dsctyp = expr->Dsctyp();
    if (dsctyp != dtyp && 
        (MTYPE_is_float(dsctyp)||MTYPE_is_complex(dsctyp)))
      return NULL;
  }
  if (prop == NOT_PROPAGATABLE)
    return NULL;

  x->DecUsecnt();
  if (icopy_phase)
    Htable()->Inc_inputprops();
  else Htable()->Inc_mainprops();
  expr->IncUsecnt();
  expr = x->Convert_type(Htable(), expr, icopy_phase);
  BB_NODE *insertbb;
  if (expr->Non_leaf() && Htable()->Phase() == MAINOPT_PHASE &&
      (insertbb = Propagated_to_loop_branch(stmt->Bb(), curbb)) &&
      stmt->Bb()->Dominates_strictly(insertbb)) {
    // insert an eval at insertbb to promote loop-invariant motion by PRE
    STMTREP *eval_stmt = CXX_NEW(STMTREP(OPC_EVAL), Htable()->Mem_pool());
    expr->IncUsecnt();
    eval_stmt->Set_rhs(expr);
    insertbb->Append_stmtrep(eval_stmt);
  }
  return expr;
}


// ====================================================================
//  COPYPROP::Get_node_rehashed_to - The C_P_REHASHED flag of node x is
//  set; search for it in _rehashed_vec and return the node rehashed to
// ====================================================================
CODEREP *
COPYPROP::Get_node_rehashed_to(CODEREP *x)
{
  INT32 i;
  for (i = 0; i <= _rehashed_vec.Lastidx(); i++) {
    if (_rehashed_vec[i] == x)
      return _rehashed_to_vec[i];
  }
  FmtAssert(FALSE, ("COPYPROP::Get_node_rehashed_to: cannot find rehashed node"));
  return NULL;
}


// ====================================================================
//  COPYPROP::Copy_propagate_cr - return the new root of the tree if copy 
//  propagation results in a new tree
// ====================================================================
CODEREP *
COPYPROP::Copy_propagate_cr(CODEREP *x, BB_NODE *curbb, 
			    BOOL inside_cse, BOOL in_array,
			    BOOL no_complex_preg)
{
  CODEREP *expr;
  CODEREP *cr = Alloc_stack_cr(x->Extra_ptrs_used());
  BOOL need_rehash;
  FOLD ftmp;

  switch (x->Kind()) {
  case CK_LDA: 
  case CK_CONST: 
  case CK_RCONST:
    return NULL;
  case CK_VAR: 
    {
      inside_cse = inside_cse || x->Usecnt() > 1;
      CODEREP *id_cr = Prop_identity_assignment(x);
     if (id_cr) {
#ifdef KEY // bug 3009
      if (id_cr->Is_flag_set(CF_IS_ZERO_VERSION)) {
	Htable()->Fix_zero_version(x->Defchi(), x->Defstmt());
	id_cr = x->Defchi()->OPND();
      }
#endif
      if (id_cr->Dsctyp() == MTYPE_UNKNOWN ||
         id_cr->Is_flag_set(CF_MADEUP_TYPE)) {
         id_cr->Set_dtyp(x->Dtyp());
         id_cr->Set_dsctyp(x->Dsctyp());
         id_cr->Set_lod_ty(x->Lod_ty());
         id_cr->Set_field_id(x->Field_id());
         if (x->Bit_field_valid()) {
           id_cr->Set_bit_field_valid();
         }
         id_cr->Set_sign_extension_flag();
         id_cr->Reset_flag(CF_MADEUP_TYPE);
      }
#ifdef KEY // bug 3009
      if (Prop_identity_assignment(id_cr)) // bug 3091
        return Copy_propagate_cr(id_cr, curbb, inside_cse, in_array); // recurse
#endif
      id_cr = x->Convert_type(Htable(), id_cr, FALSE);
      return id_cr;
     }
     return Prop_var(x, curbb, FALSE, inside_cse, in_array, no_complex_preg);
    }
  case CK_IVAR: {
    MU_NODE *mnode = x->Ivar_mu_node();
    if (mnode) {
      CODEREP *m_cr = Prop_identity_assignment(mnode->OPND());
      if (m_cr)
	mnode->Set_OPND(m_cr);
    }

    CODEREP *expr2;
    expr = Prop_ivar(x, curbb, FALSE, inside_cse, in_array, no_complex_preg);
    if (expr) {
      x->DecUsecnt_rec();
      return expr;
    }
    inside_cse = inside_cse || x->Usecnt() > 1;
    expr = Copy_propagate_cr(x->Ilod_base(), curbb, inside_cse, in_array
#ifdef KEY // bug 10577
    			     , TRUE
#endif
    			    );
    {
      CODEREP *ilod_base = (expr != NULL) ? expr : x->Ilod_base();
      if (ilod_base->Kind() == CK_LDA && x->Offset() == 0) {
	CODEREP *retv = Prop_const_init_scalar(x, ilod_base->Lda_aux_id());
	if (retv)
	  return retv;
      }
    }
    INT64 ofst = x->Offset();
    if (expr) {
      CODEREP *canon_expr = OPERATOR_is_load(x->Opr())
	&& Use_Load_Store_Offset && x->Istr_base() == NULL ? 
	// disable offset canon if both ilod_base and istr_base are non-NULL.
	Htable()->Canon_base(expr,&ofst) : Htable()->Canon_rhs(expr);
      if (canon_expr) expr = canon_expr;
    }
    if (x->Opr() == OPR_MLOAD)
      expr2 = Copy_propagate_cr(x->Mload_size(), curbb, 
				inside_cse, in_array);
    else if (x->Opr() == OPR_ILOADX)
      expr2 = Copy_propagate_cr(x->Index(), curbb, inside_cse, in_array);
    else
      expr2 = NULL;
    if (expr || expr2) { // need rehash
      cr->Copy(*x);   // not creating new cr, just copy
      if (expr) {
	cr->Set_offset(ofst); // after canonicalization
        cr->Set_ilod_base(expr);
      }
      cr->Set_istr_base(NULL);
      cr->Set_usecnt(0);
      if (expr2)
	cr->Set_mload_size(expr2);
      cr->Set_ivar_occ(x->Ivar_occ());
      x->DecUsecnt();
      if (x->Opr() == OPR_MLOAD || x->Opr() == OPR_ILOADX) {
	expr = Htable()->Rehash(cr);
      } else {
	expr = ftmp.Fold_Expr(cr);     
	if (expr == NULL) { // either not folded or Fold_Expr has not rehashed
	  expr = Htable()->Rehash(cr);
	  if (expr->Kind()==CK_OP) expr->Set_isop_flag(ISOP_FOLD_EXPR_VISITED);
	}
      }
      return expr;
    } 
    // make sure Ilod_base() and Istr_base() are the same if the iload is not
    // to be rehashed
    if (x->Istr_base() != NULL && x->Istr_base() != x->Ilod_base()) {
      // undo the copy propagation of Istr_base()
      x->Istr_base()->DecUsecnt_rec();
      x->Ilod_base()->IncUsecnt();
      x->Set_istr_base(x->Ilod_base());
    }
    return NULL;
    }
  case CK_OP:
    if (x->Is_flag_set(CF_C_P_PROCESSED)) {
      if (x->Is_flag_set(CF_C_P_REHASHED)) {
	x->DecUsecnt();
	expr = Get_node_rehashed_to(x);
	expr->IncUsecnt();
	return expr;
      } else 
	return NULL;
    }
    need_rehash = FALSE;
    cr->Copy(*x);    // not creating new cr, just copy
    cr->Set_usecnt(0);
    inside_cse = inside_cse || x->Usecnt() > 1;

    const OPERATOR opr = x->Opr();

    if (opr != OPR_ADD && opr != OPR_SUB && opr != OPR_NEG && opr != OPR_MPY) {
      for  (INT32 i = 0; i < x->Kid_count(); i++) {

	// can do this in this loop because this is the only place
	// where opr can be array
	if ( opr == OPR_ARRAY ) {
	  // only concerned about the index expressions, not the base
	  if ( i > 0 )
	    in_array = TRUE;
	}
#if defined(KEY) && !defined(TARG_NVISA)
        if (opr != OPR_ASM_INPUT ||
            (x->Opnd(i)->Kind() != CK_VAR && x->Opnd(i)->Kind() != CK_IVAR) )
	  expr = Copy_propagate_cr(x->Opnd(i), curbb, inside_cse, in_array);
        else {
	  // OSP_384
	  if(opr == OPR_ASM_INPUT) {
            // open64.net bug787. Try propagate for VAR first, if the propagated expr is the same
            // kind to the original one,i.e, also a VAR. we allow this propagation. otherwise, disable it.
            if ( x->Opnd(i)->Kind() == CK_VAR ) {
              CODEREP *possible_prop = Copy_propagate_cr(x->Opnd(i), curbb, inside_cse, in_array);
              if (possible_prop && possible_prop->Kind() == x->Opnd(i)->Kind()) 
                expr = possible_prop;
              // open64.net bug963. If the Asm_input constraint is "i", i.e, required immediate
              // and the possible propagation is also a constant, we do this propagation on demand.
              else if (possible_prop &&
                       (possible_prop->Kind() == CK_CONST ||
                        possible_prop->Kind() == CK_RCONST) &&
                       !strncmp(ST_name(&St_Table[x->Asm_constraint()]),"i",1)) {
                expr = possible_prop;
              }
              else {
                x->Opnd(i)->Set_flag(CF_DONT_PROP);
                expr = NULL;
              }
            }
            else {
              x->Opnd(i)->Set_flag(CF_DONT_PROP);
              expr = NULL;
            }
          }
	}
#else
	// for NVISA, the usage of asm is an array of const val,
	// then was passing arr[3] and was seeing the array node
	// rather than the const val under the asm_input
	expr = Copy_propagate_cr(x->Opnd(i), curbb, inside_cse, in_array);
#endif
	if (expr) {
	  need_rehash = TRUE;
	  CODEREP *expr2 = Htable()->Canon_rhs(expr);
	  if (expr2) 
	    cr->Set_opnd(i, expr2);
	  else
	    cr->Set_opnd(i, expr);
	}
	else cr->Set_opnd(i, x->Opnd(i));
      }
    } else {
      for  (INT32 i = 0; i < x->Kid_count(); i++) {
	expr = Copy_propagate_cr(x->Opnd(i), curbb, inside_cse, in_array);
	if (expr) {
	  need_rehash = TRUE;
	  cr->Set_opnd(i, expr);
	}
	else cr->Set_opnd(i, x->Opnd(i));
      }
    }

    // equality-simplification when this pattern appears:
    //	p = <ptr_expr>
    //  *q = *<ptr_expr)
    //  replace (*q != *p) by FALSE
    //  replace (*q == *p) by TRUE
    // needs to turn on iload_prop to get the maximum effect
    if ((x->Opr() == OPR_EQ || x->Opr() == OPR_NE) &&
	x->Opnd(0)->Kind() == CK_IVAR &&
	x->Opnd(1)->Kind() == CK_IVAR &&
	x->Opnd(0)->Opr() == OPR_ILOAD &&
	x->Opnd(1)->Opr() == OPR_ILOAD) {
      CODEREP *lside = x->Opnd(0);
      CODEREP *rside = x->Opnd(1);
      STMTREP *ldef = lside->Ivar_defstmt();
      STMTREP *rdef = rside->Ivar_defstmt();
      CODEREP *istoreval, *stidval;
      if (ldef != NULL && rdef == NULL) {
	if (rside->Ilod_base()->Kind() == CK_VAR &&
	    rside->Ilod_base()->Defstmt() != NULL) {
          istoreval = ldef->Rhs();
	  stidval = rside->Ilod_base()->Defstmt()->Rhs();
	  if (istoreval->Kind() == CK_IVAR &&
	      istoreval->Opr() == OPR_ILOAD &&
	      istoreval->Ilod_base() == stidval) { // successful
	    // adjust usecnt
	    cr->Init_const(x->Dtyp(), x->Opr() == OPR_EQ);
	    return Htable()->Rehash(cr);
	  }
	}
      }
      else if (ldef == NULL && rdef != NULL) {
	if (lside->Ilod_base()->Kind() == CK_VAR &&
	    lside->Ilod_base()->Defstmt() != NULL) {
	  istoreval = rdef->Rhs();
	  stidval = lside->Ilod_base()->Defstmt()->Rhs();
	  if (istoreval->Kind() == CK_IVAR &&
	      istoreval->Opr() == OPR_ILOAD &&
	      istoreval->Ilod_base() == stidval) { // successful
	    // adjust usecnt
	    cr->Init_const(x->Dtyp(), x->Opr() == OPR_EQ);
	    return Htable()->Rehash(cr);
	  }
	}
      }
    }

    BOOL canonicalized = FALSE;

    // Canonicalize comparison for LFTR2 in MAINOPT phase only.
    if (Htable()->Phase() == MAINOPT_PHASE && OPERATOR_is_compare(cr->Opr())) {
      BOOL modified;
      canonicalized = Htable()->Canonicalize_compare(cr, curbb, &modified);
      if (modified)      // Canonicalize_compare will overwrite cr!
	need_rehash = TRUE;
    }

    x->Set_flag(CF_C_P_PROCESSED);
    if (need_rehash) {
      x->DecUsecnt(); 
      if (! canonicalized || 
	  (OPERATOR_is_compare(cr->Opr()) && 
	   (cr->Opnd(0)->Kind() != CK_VAR) || cr->Opnd(1)->Kind() == CK_CONST))
        expr = ftmp.Fold_Expr(cr);
      else expr = NULL; // Fold_Expr may un-do what Canonicalize_compare did
      if (expr == NULL) { // either not folded or Fold_Expr has not rehashed
	expr = Htable()->Rehash(cr, !canonicalized);
        if (expr->Kind()==CK_OP) expr->Set_isop_flag(ISOP_FOLD_EXPR_VISITED);
	expr->Set_flag(CF_C_P_PROCESSED);
      }
      x->Set_flag(CF_C_P_REHASHED);
      // to record rehashed entry for use if visited again
      Add_rehashed_node(x, expr);
      return expr;
    }
    else return NULL;
  }
  return NULL;
}

// ====================================================================
//  Determine if we should propagate into this statement
// ====================================================================

inline BOOL
Propagate_into_stmt( const STMTREP *stmt )
{
  // cannot propagate into "black-box" statements
  if ( stmt->Black_box() ) {
    return FALSE;
  }

  return TRUE;
}

// ====================================================================
//  COPYPROP::Copy_propagate_stmt
// ====================================================================
void
COPYPROP::Copy_propagate_stmt(STMTREP *stmt, BB_NODE *bb)
{
  CODEREP *x;

  if ( ! Propagate_into_stmt( stmt ) )
    return;

  if (stmt->Op() == OPC_XPRAGMA &&
      WN_pragma(stmt->Orig_wn()) == WN_PRAGMA_COPYIN_BOUND)
    return;

  if (stmt->Rhs()) {
    x = Copy_propagate_cr(stmt->Rhs(), bb, FALSE, FALSE/*in_array*/);
    if (x) {
      CODEREP *y = Htable()->Canon_rhs(x);
      if (y)
	stmt->Set_rhs(y);
      else
	stmt->Set_rhs(x);
    }
  }
  switch (stmt->Opr()) { 
  case OPR_ISTORE:
  case OPR_ISTBITS:
    x = Copy_propagate_cr(stmt->Lhs()->Istr_base(), bb, FALSE,
    			  FALSE/*in_array*/);
    if (x) {
      INT64 ofst = stmt->Lhs()->Offset();
      CODEREP *y = stmt->Lhs()->Ilod_base() == NULL && Use_Load_Store_Offset ?
	// disable offset canon if both ilod_base and istr_base are non-NULL.
	Htable()->Canon_base(x, &ofst) : Htable()->Canon_rhs(x);
      if (y) {
	stmt->Lhs()->Set_istr_base(y);
	stmt->Lhs()->Set_offset(ofst);
      } else
	stmt->Lhs()->Set_istr_base(x);
    }
    break;
  case OPR_MSTORE:
    {
      x = Copy_propagate_cr(stmt->Lhs()->Istr_base(), bb, FALSE,
                            FALSE/*in_array*/ );
      if (x) {
        INT64 ofst = stmt->Lhs()->Offset();
        CODEREP *y = stmt->Lhs()->Ilod_base() == NULL
	  && Use_Load_Store_Offset ?
          // disable offset canon if both ilod_base and istr_base are non-NULL.
          Htable()->Canon_base(x, &ofst) : Htable()->Canon_rhs(x);
        if (y) {
          stmt->Lhs()->Set_istr_base(y);
          stmt->Lhs()->Set_offset(ofst);
        } else
          stmt->Lhs()->Set_istr_base(x);
      }
      CODEREP *num_bytes = (CODEREP *)stmt->Lhs()->Mstore_size();
      x = Copy_propagate_cr(num_bytes, bb, FALSE, FALSE/*in_array*/);
      if (x) stmt->Lhs()->Set_mstore_size(x);
    }
    break;
  default: ;
  }
}

// ====================================================================
// COPYPROP::Var_has_as_value_on_the_other_path -
//   Check if the variable var, which is from one side of a 2-way phi, has
//   as value on the other side of the phi the value given by "value".
//   "value" must be a leaf node, and if it is a variable, must be of different
//   aux_id from v; it also must NOT have a phi at the current bb.
// ====================================================================
BOOL
COPYPROP::Var_has_as_value_on_the_other_path(CODEREP *var, CODEREP *value,
					     BOOL var_on_left, BB_NODE *bb)
{
  INT32 height;
  ST *s;
  PHI_NODE *phi;
  PHI_LIST_ITER phi_iter;
  if (var->Is_var_volatile())
    return FALSE;
  s = Opt_stab()->St(var->Aux_id());
  if ((ST_class(s) == CLASS_PREG && Preg_Is_Dedicated(var->Offset())))
    return FALSE;
  // check if there is a phi in current BB
  FOR_ALL_ELEM (phi, phi_iter, Init(bb->Phi_list())) 
    if (phi->Aux_id() == var->Aux_id()) 
      break;
  if (phi == NULL || ! phi->Live())
    return FALSE;

  CODEREP *opnd, *other_opnd;
  if ( var_on_left ) {
    opnd = phi->OPND(1);
    other_opnd = phi->OPND(0);
  }
  else {
    opnd = phi->OPND(0);
    other_opnd = phi->OPND(1);
  }
  if ( opnd->Is_flag_set(CF_IS_ZERO_VERSION) )
    return FALSE;
  if (opnd->Is_flag_set(CF_DEF_BY_CHI))
    return FALSE;
  // assumed that the phi versions match because we may be creating
  // an overlapping live-range otherwise
  if ( other_opnd != var )
    return FALSE;

  STMTREP *defstmt = opnd->Get_defstmt();
  if (defstmt == NULL || ! OPERATOR_is_scalar_store (defstmt->Opr()))
    return FALSE;
  if (defstmt->Rhs() != value) 
    return FALSE;
  return TRUE;
}

// ====================================================================
// COPYPROP::Propagatable_thru_phis -
//   Check if the two given rhs expressions from two incoming edges at a
//   phi are syntactically congruent and propagatable to after the phis;
//   if not syntactically congruent, check if that is still possible by
//   using the expression from one of the two sides, in which case
//   ppref will return which side should be used; ppref must be set
//   whenever it returns TRUE. phi_simp_var is the variable whose phi
//   we are trying to simplify.
// ====================================================================
BOOL
COPYPROP::Propagatable_thru_phis(CODEREP *lexp, CODEREP *rexp, 
				 BB_NODE *bb,
				 CODEREP *phi_simp_var,
                                 PROP_THRU_PHI_PREFERENCE *ppref)
{
  INT32 height, weight;
  if (lexp == rexp) {
    if (Propagatable(lexp, FALSE, 0, FALSE, FALSE, 
		     &height, &weight, FALSE/*in_array*/, NULL) == PROPAGATABLE)
    {
      *ppref = EITHER_SIDE;
      return TRUE;
    }
    else return FALSE;
  }
  if (lexp->Kind() != rexp->Kind()) {
    if (lexp->Kind() == CK_VAR) {
      if (lexp->Aux_id() == phi_simp_var->Aux_id())
        return FALSE;
      if (Var_has_as_value_on_the_other_path(lexp, rexp, TRUE, bb)) {
	*ppref = LEFT_SIDE;
	return TRUE;
      }
      return FALSE;
    }
    if (rexp->Kind() == CK_VAR) {
      if (rexp->Aux_id() == phi_simp_var->Aux_id())
        return FALSE;
      if (Var_has_as_value_on_the_other_path(rexp, lexp, FALSE, bb)) {
	*ppref = RIGHT_SIDE;
	return TRUE;
      }
      return FALSE;
    }
    return FALSE;
  }
  switch (lexp->Kind()) {	// kinds are the same
  case CK_LDA:
  case CK_CONST:
  case CK_RCONST:
  case CK_IVAR:
    return FALSE;
  case CK_VAR:
    if (lexp->Is_var_volatile() || rexp->Is_var_volatile())
      return FALSE;
    if (lexp->Aux_id() == phi_simp_var->Aux_id())
      return FALSE;
    if (rexp->Aux_id() == phi_simp_var->Aux_id())
      return FALSE;
    if (lexp->Aux_id() == rexp->Aux_id()) {
      PHI_NODE *phi;
      PHI_LIST_ITER phi_iter;
      ST *s = Opt_stab()->St(lexp->Aux_id());
      if ((ST_class(s) == CLASS_PREG && Preg_Is_Dedicated(lexp->Offset())))
        return FALSE;
      // there may or may not be a phi
      FOR_ALL_ELEM (phi, phi_iter, Init(bb->Phi_list())) {
        if (phi->Aux_id() == lexp->Aux_id()) 
	  break;
      }
      if (phi == NULL || ! phi->Live())
	return FALSE;
      if (phi->OPND(0) == lexp && phi->OPND(1) == rexp) {
	*ppref = EITHER_SIDE;
	return TRUE;
      }
      return FALSE;
    }
    else {
      if (Var_has_as_value_on_the_other_path(lexp, rexp, TRUE, bb)) {
	*ppref = LEFT_SIDE;
	return TRUE;
      }
      if (Var_has_as_value_on_the_other_path(rexp, lexp, FALSE, bb)) {
	*ppref = RIGHT_SIDE;
	return TRUE;
      }
      return FALSE;
    }
  case CK_OP: {
    if (OPERATOR_is_volatile(lexp->Opr()))
      return NOT_PROPAGATABLE;
    if (lexp->Op() != rexp->Op() &&
	OPCODE_commutative_op(lexp->Op()) != rexp->Op() ||
	! Op_can_be_propagated(lexp->Op(), Htable()->Phase()))
      return FALSE;
    if (lexp->Opr() == OPR_ARRAY && rexp->Opr() == OPR_ARRAY &&
	lexp->Elm_siz() != rexp->Elm_siz()) return FALSE;
#ifdef KEY // bug 1795: need to verify boffset and bsize are identical
    if (lexp->Opr() == OPR_EXTRACT_BITS &&
	(lexp->Op_bit_offset() != rexp->Op_bit_offset() ||
	 lexp->Op_bit_size() != rexp->Op_bit_size()))
        return FALSE;
    if (lexp->Opr() == OPR_CVTL && lexp->Offset() != rexp->Offset())
	return FALSE;
    if (lexp->Opr() == OPR_COMPOSE_BITS &&
        (lexp->Op_bit_offset() != rexp->Op_bit_offset() ||
         lexp->Op_bit_size() != rexp->Op_bit_size()))
        return FALSE;
#endif
    if (lexp->Opr() == OPR_INTRINSIC_OP && rexp->Opr() == OPR_INTRINSIC_OP
        && lexp->Intrinsic() != rexp->Intrinsic())  // fix 804479
      return FALSE;
#ifdef KEY
    if (lexp->Opr() == OPR_PURE_CALL_OP && rexp->Opr() == OPR_PURE_CALL_OP
        && lexp->Call_op_aux_id() != rexp->Call_op_aux_id())
      return FALSE;
    if (lexp->Kid_count() != rexp->Kid_count())
      return FALSE;
#endif
    PROP_THRU_PHI_PREFERENCE pref0;
    BOOL can_prop = TRUE;
    *ppref = EITHER_SIDE;
    for (INT32 i = 0; i < lexp->Kid_count(); i++) {
      if (! Propagatable_thru_phis(lexp->Opnd(i), rexp->Opnd(i), bb, phi_simp_var, &pref0)) {
	can_prop = FALSE;
	break;
      }
      if (*ppref != pref0) {
	if (*ppref != EITHER_SIDE && pref0 != EITHER_SIDE) {
	  can_prop = FALSE;
	  break;
	}
	*ppref = MAX(*ppref, pref0);
      }
    }
    if (can_prop && lexp->Op() == rexp->Op())
      return TRUE;
    if (OPCODE_commutative_op(lexp->Op()) != rexp->Op())
      return FALSE;
    // must have only 2 kids; check further with swapped kids
    if (! Propagatable_thru_phis(lexp->Opnd(0), rexp->Opnd(1), bb, phi_simp_var, &pref0)) 
      return FALSE;
    *ppref = pref0;
    if (! Propagatable_thru_phis(lexp->Opnd(1), rexp->Opnd(0), bb, phi_simp_var, &pref0)) 
      return FALSE;
    if (*ppref != pref0) {
      if (*ppref != EITHER_SIDE && pref0 != EITHER_SIDE) 
	return FALSE;
      *ppref = MAX(*ppref, pref0);
    }
    return TRUE;
  }
  }
  return UNDEFINED; // to satisfy compiler
}

// ====================================================================
// COPYPROP::Rehash_thru_phis - rehash and re-insert expression in hash table;
// for CK_VAR nodes, if a phi is present in bb, use lhs of phi.
// ====================================================================
CODEREP *
COPYPROP::Rehash_thru_phis(CODEREP *x, BB_NODE *bb)
{
  switch (x->Kind()) {
  case CK_LDA:     
  case CK_CONST:  
  case CK_RCONST:
  case CK_IVAR:    
    x->IncUsecnt();
    return x;
  case CK_VAR: {
    PHI_NODE *phi;
    PHI_LIST_ITER phi_iter;
    // check if there is a phi in bb
    FOR_ALL_ELEM (phi, phi_iter, Init(bb->Phi_list())) {
      if (phi->Aux_id() == x->Aux_id()) 
        break;
    }
    if (phi == NULL) {
      x->IncUsecnt();
      return x;
    }
    // Fix 626386:  the phi might define a zero versions.
    if (phi->RESULT()->Is_flag_set(CF_IS_ZERO_VERSION)) {
      CODEREP *retval = Htable()->Add_def(x->Aux_id(),
					  -1,   /* dummy version number */
					  NULL, /* defstmt */
					  x->Dtyp(),
					  x->Dsctyp(),
					  x->Offset(),
					  x->Lod_ty(),
					  x->Field_id(),
					  TRUE);
      if (x->Is_flag_set(CF_MADEUP_TYPE))
	retval->Set_flag(CF_MADEUP_TYPE);
      retval->Set_defphi(phi);
      retval->Set_flag(CF_DEF_BY_PHI);
      retval->Set_flag(CF_INCOMPLETE_USES);  // because it might still have other zero ver use
      phi->Set_result(retval);
    }

    CODEREP *retv = 
      phi->RESULT()->Var_type_conversion(Htable(), x->Dtyp(), x->Dsctyp(),
					 x->Lod_ty(), x->Field_id()); 
    retv->IncUsecnt();
    return retv;
  }
  case CK_OP: {
    CODEREP *cr = Alloc_stack_cr(x->Extra_ptrs_used());
    cr->Copy(*x); 
    cr->Set_usecnt(0);
    for (INT32 i = 0; i < x->Kid_count(); i++) 
      cr->Set_opnd(i, Rehash_thru_phis(x->Opnd(i), bb));
    CODEREP *retv = Htable()->Hash_Op(cr);
    return retv;
  }
  }
  return NULL; // to satisfy compiler
}

CODEREP*
COPYPROP::Strictly_identical_phi_opnd(PHI_NODE *phi, BB_NODE *bb)
{
  CODEREP *popnd = NULL;
  BB_NODE *pred; BB_LIST_ITER bb_iter;
  INT32 height, weight;

  FOR_ALL_ELEM (pred, bb_iter, Init(bb->Pred())) {
    CODEREP *tmp = phi->OPND(bb_iter.Idx());
    STMTREP *defstmt = tmp->Get_defstmt();
    if (defstmt == NULL || ! OPERATOR_is_scalar_store (defstmt->Opr()) ||
        tmp->Is_flag_set(CF_DEF_BY_CHI)) {
      popnd = NULL; break;
    }
    if (popnd == NULL && 
        Propagatable(defstmt->Rhs(), FALSE, 0, FALSE, FALSE,
		     &height, &weight, FALSE/*in_array*/, NULL) == PROPAGATABLE)
      popnd = defstmt->Rhs();
    if (popnd != defstmt->Rhs()) { popnd = NULL; break; }
  }
  return popnd;
}

// ====================================================================
// COPYPROP::Identical_phi_opnd -
//   Test if the phi operands are all the same, return it if its TRUE,
//   return NULL otherwise
// ====================================================================
CODEREP *
COPYPROP::Identical_phi_opnd(PHI_NODE *phi, BB_NODE *bb)
{
  CODEREP *res = phi->RESULT();
  INT32 height, weight;

  if (!WOPT_Enable_Aggressive_Phi_Simp || 
      bb->Pred()->Len() > 2 ||
      bb->Pred()->Len() == 1) {
    return Strictly_identical_phi_opnd(phi, bb);
  }

  // for 2 operands phi's, do not require identical codereps for the two rhs
  CODEREP *lopnd = phi->OPND(0);
  CODEREP *ropnd = phi->OPND(1);
  if (lopnd->Is_flag_set(CF_DEF_BY_CHI) || ropnd->Is_flag_set(CF_DEF_BY_CHI))
    return NULL;
  STMTREP *ldefstmt = lopnd->Get_defstmt();
  STMTREP *rdefstmt = ropnd->Get_defstmt();
  if (ldefstmt == NULL || rdefstmt == NULL)
    return NULL;
#ifdef KEY 
  // bug 7228: if there is chi, DCE wont be able to delete the dead stores
  if (ldefstmt->Chi_list() || rdefstmt->Chi_list())
    return NULL;
#endif
  if (! OPERATOR_is_scalar_store (ldefstmt->Opr()) ||
      ! OPERATOR_is_scalar_store (rdefstmt->Opr()))
    return NULL;
  if (ldefstmt->Rhs() == rdefstmt->Rhs()) {
    if (Propagatable(ldefstmt->Rhs(), FALSE, 0, FALSE, FALSE,
		     &height, &weight, FALSE/*in_array*/, NULL) == PROPAGATABLE)
      return ldefstmt->Rhs();	// trivial case (equivalent to above) 
    else return NULL;
  }
  PROP_THRU_PHI_PREFERENCE pref;
  if (Propagatable_thru_phis(ldefstmt->Rhs(), rdefstmt->Rhs(), bb, res, &pref)) 
    return Rehash_thru_phis((pref != RIGHT_SIDE) ? ldefstmt->Rhs() : rdefstmt->Rhs(), bb);
  else return NULL;
}

void
COPYPROP::Unvisit_nodes(void)
{
  INT32 i;
  for (i = 0; i <= _visited_vec.Lastidx(); i++) {
    _visited_vec[i]->Reset_isop_flag((ISOP_FLAG)(ISOP_COPY_VISITED | ISOP_ICOPY_VISITED));
  }
  _visited_vec.Resetidx();	// reset the entire vector 

  for (i = 0; i <= _rehashed_vec.Lastidx(); i++) {
    _rehashed_vec[i]->Reset_flag((CR_FLAG)(CF_C_P_PROCESSED | CF_C_P_REHASHED));
  }
  _rehashed_vec.Resetidx();	// reset the entire vector 
  _rehashed_to_vec.Resetidx();	// reset the entire vector 
}


//  Convert all zero version chi opnd into real versions
//  because the assignment is going to be deleted.
//
void
COPYPROP::Fix_identity_assignment(STMTREP *stmt)
{
  CHI_LIST_ITER  chi_iter;
  CHI_NODE      *cnode;
  FOR_ALL_NODE(cnode, chi_iter, Init(stmt->Chi_list())) {
    if (cnode->Live()) {
      if (cnode->OPND()->Is_flag_set(CF_IS_ZERO_VERSION))
	Htable()->Fix_zero_version(cnode, stmt);
      if (cnode->RESULT()->Is_flag_set(CF_IS_ZERO_VERSION)) {
	// if the definition is a LDID, recursively mark incomplete uses
	CODEREP *def = cnode->OPND();
	while (!def->Is_flag_set((CR_FLAG)(CF_DEF_BY_PHI|CF_DEF_BY_CHI)) &&
	       def->Defstmt() != NULL &&
	       def->Defstmt()->Is_identity_assignment_removable()) {
	  def = def->Defstmt()->Rhs();
	}
	def->Set_flag(CF_INCOMPLETE_USES);
      }
    }
  }
}

// ====================================================================
//  Copy_propagate - do according to pre-order traversal of the
//  dominator tree so that I don't have to call copy_propagate
//  for the substituted expression tree; the order of processing variables 
//  ensures that all variables' defs are seen before any use.
// ====================================================================

void
COPYPROP::Copy_propagate(BB_NODE *bb)
{
  CODEREP *cr;
  PHI_NODE *phi; 
  PHI_LIST_ITER phi_iter;

  //  Iterate through each phi-node 
  FOR_ALL_ELEM (phi, phi_iter, Init(bb->Phi_list())) {
    if (phi->Live()) 
      Opt_stab()->Push_coderep(phi->Aux_id(), phi->RESULT());
    else Opt_stab()->Push_coderep(phi->Aux_id(), NULL);
  }

  Unvisit_nodes();

  Is_True(! Past_ret_reg_def(), 
          ("COPYPROP::Copy_propagate: _past_ret_reg_def flag set incorrectly"));

  // Fix 377535:  a DO-loop cannot be raised back if phi-simp
  // inserted a stmt into the DOEND block.
  if (Htable()->Phase() == MAINOPT_PHASE || bb->Kind() != BB_DOEND) {

    // Iterate through each phi again to see if their operands have same values;
    // if so, replace by a real assignment
    FOR_ALL_ELEM (phi, phi_iter, Init(bb->Phi_list())) {
      if (phi->Live()) {
	cr = phi->RESULT();
	if (! WOPT_Enable_Phi_Simp) continue;
	if (cr->Is_flag_set(CF_IS_ZERO_VERSION)) continue;
	if (!Opt_stab()->Aux_stab_entry(cr->Aux_id())->Is_real_var()) continue;
	CODEREP *popnd = Identical_phi_opnd(phi, bb);
	if (popnd) { // all operands are the same 
	  cr->Reset_flag(CF_DEF_BY_PHI);
	  // Fix 461984 (don't change the dtyp and dsctyp)
	  if (cr->Dsctyp() == MTYPE_UNKNOWN ||
	      cr->Is_flag_set(CF_MADEUP_TYPE)) {
	    cr->Set_dtyp(phi->OPND(0)->Dtyp());
	    cr->Set_dsctyp(phi->OPND(0)->Dsctyp());
	    cr->Set_lod_ty(phi->OPND(0)->Lod_ty());
	    cr->Set_field_id(phi->OPND(0)->Field_id());
#ifdef KEY // bug 7228
	    cr->Set_offset(phi->OPND(0)->Offset());
#endif
	    if (phi->OPND(0)->Bit_field_valid())
	      cr->Set_bit_field_valid();
	    cr->Set_sign_extension_flag();
	    cr->Reset_flag(CF_MADEUP_TYPE);
	  }
	  STMTREP *copy_stmt = popnd->Create_cpstmt(cr, Htable()->Mem_pool());
	  bb->Prepend_stmtrep(copy_stmt);
	  phi->Reset_live(); 
	}
      }
    }
  }

  // Fix 654206:  DO-loop raising relies on copy propagation to
  // copy propagate all assignments in the DOEND block and DCE
  // to delete this assignment.  The emitter expects a DOEND containing
  // of assignment.  In this bug, the expression was too big to be 
  // copy propagated and breaks the emitter.
  INT32 saved_prop_limit = WOPT_Enable_Prop_Limit;
  if (bb->Kind() == BB_DOEND) 
#ifdef KEY // bug 6097, bug 11784, bug 13003
    WOPT_Enable_Prop_Limit = WOPT_Enable_Doend_Prop_Limit;
#else
    WOPT_Enable_Prop_Limit = 9999;
#endif

  // iterate through each statement in this bb
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {
    INT32 linenum = Srcpos_To_Line(stmt->Linenum());	// for debugging
    Copy_propagate_stmt(stmt, bb);
    if (OPERATOR_is_scalar_store (stmt->Opr())) {
      if (stmt->Is_identity_assignment_removable() && WOPT_Enable_DCE) 
	Fix_identity_assignment(stmt);  // repair zero version chi operands
      CODEREP *lhs = stmt->Lhs();
      if (Is_function_of_itself(stmt, Opt_stab()) &&
	  Opt_stab()->Stack_elements(lhs->Aux_id()) != 0)
	// push the last version to be the current version because I'm sure
	// I can substitute all current version with the last version
	Opt_stab()->Push_coderep(lhs->Aux_id(),
			       Opt_stab()->Top_coderep(lhs->Aux_id()));
      else Opt_stab()->Push_coderep(lhs->Aux_id(), lhs);
      }

    // propagate any identity assignment into mu and chi operands
    if (stmt->Has_mu()) {
      MU_LIST_ITER mu_iter;
      MU_NODE *mnode;
      FOR_ALL_NODE( mnode, mu_iter, Init(stmt->Mu_list())) {
	cr = Prop_identity_assignment(mnode->OPND());
	if (cr)
	  mnode->Set_OPND(cr);
      } // forall mnode
    } // if stmt->Has_mu
    if (stmt->Has_chi()) {
      CHI_LIST_ITER chi_iter;
      CHI_NODE *cnode;
      CHI_LIST *chi_list = stmt->Chi_list();
      FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
	if (cnode->Live()) {
          Opt_stab()->Push_coderep(cnode->Aux_id(), cnode->RESULT());
	  cr = Prop_identity_assignment(cnode->OPND());
	  if (cr)
	    cnode->Set_OPND(cr);
	}
	else Opt_stab()->Push_coderep(cnode->Aux_id(), NULL);
      }
    }

    if (OPERATOR_is_scalar_store (stmt->Opr()) &&
	Opt_stab()->Aux_stab_entry(stmt->Lhs()->Aux_id())->Is_dedicated_preg())
      Set_past_ret_reg_def();
    else if (stmt->Opr() == OPR_RETURN || 
	     stmt->Opr() == OPR_RETURN_VAL ||
	     stmt->Opr() == OPR_REGION
#ifdef KEY
  	     || stmt->Opr() == OPR_GOTO_OUTER_BLOCK
#endif
	    )
      Reset_past_ret_reg_def();

    if (stmt->Has_chi() || OPERATOR_is_store(stmt->Opr()))
      Unvisit_nodes();
  }

  WOPT_Enable_Prop_Limit = saved_prop_limit;

  // Propagate identity assignment into phi opnds of succ bbs; this is done here
  // so as not to violate the assumption that when copy propagation processes a
  // use, the assignment statement that defines it has already been processed;
  // otherwise, there is ordering problem (which eventually causes
  // overlapping live range) when copy propagation changes an assignment into
  // an indentity assignment.
  BB_NODE *succ; BB_LIST_ITER bb_iter;
  FOR_ALL_ELEM (succ, bb_iter, Init(bb->Succ())) {
    INT32 pos = succ->Pred()->Pos(bb);
    FOR_ALL_ELEM (phi, phi_iter, Init(succ->Phi_list())) {
      if (phi->Live()) {
        cr = Prop_identity_assignment(phi->OPND(pos));
	if (cr) {
	  phi->Set_opnd(pos, cr);
	  cr->Set_flag(CF_DONT_PROP);
	}
      }
    }
  }

  // do copy propagation for its dominated nodes
  BB_NODE *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM(dom_bb, dom_bb_iter, Init(bb->Dom_bbs()))
    Copy_propagate(dom_bb);

  // iterate through each statement in this bb
  FOR_ALL_NODE_REVERSE(stmt, stmt_iter, Init()) {
    if (stmt->Has_chi()) {
      CHI_LIST_ITER chi_iter;
      CHI_NODE *cnode;
      CHI_LIST *chi_list = stmt->Chi_list();
      FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
        Opt_stab()->Pop_coderep(cnode->Aux_id());
      }
    } 
    if (OPERATOR_is_scalar_store (stmt->Opr()))
      Opt_stab()->Pop_coderep(stmt->Lhs()->Aux_id());
  }

  FOR_ALL_ELEM (phi, phi_iter, Init(bb->Phi_list())) {
    Opt_stab()->Pop_coderep(phi->Aux_id());
  }
}

// ====================================================================
//  COMP_UNIT::Do_copy_propagate - invoke copy propagation for this PU
// ====================================================================
void
COMP_UNIT::Do_copy_propagate()
{
  MEM_POOL cp_pool;
  OPT_POOL_Initialize(&cp_pool, "copy prop pool", FALSE, PROP_DUMP_FLAG);
  OPT_POOL_Push(&cp_pool, PROP_DUMP_FLAG);
  
  Opt_stab()->New_coderep(&cp_pool);
  _opt_stab->Clear_coderep();

  {
    COPYPROP copyprop(Htable(), Opt_stab(), Cfg(), &cp_pool);
    copyprop.Copy_propagate(Cfg()->Entry_bb());
    // Fix 630423:  reset all ISOP_*COPY_VISITED 
    copyprop.Unvisit_nodes();
  }

#ifdef Is_True_On
  _opt_stab->Check_stack();
#endif
  OPT_POOL_Pop(&cp_pool, PROP_DUMP_FLAG);
  OPT_POOL_Delete(&cp_pool, PROP_DUMP_FLAG);


  // Move IVR-generated assignment for secondary IV into the subsequent block
  // to ensure the DO-loop can be promoted back.
  if (Phase() == PREOPT_PHASE || 
      Phase() == PREOPT_LNO_PHASE ||
      Phase() == PREOPT_IPA0_PHASE ||
      Phase() == PREOPT_IPA1_PHASE) {
    CFG_ITER cfg_iter(Cfg());
    BB_NODE *bb;
    FOR_ALL_NODE (bb, cfg_iter, Init()) {
      if (bb->Kind() == BB_DOEND &&
	  bb->Innermost()->Test_at_entry()) {
	if (bb->Last_stmtrep() != NULL &&
	    (bb->Last_stmtrep()->Opr() == OPR_TRUEBR ||
	     bb->Last_stmtrep()->Opr() == OPR_FALSEBR)) {
	  BB_NODE *insertbb = bb->Next();
	  if (insertbb->Kind() == BB_REGIONSTART)
	    insertbb = bb; // don't allow insert into region start (446809)
	  STMTREP *stmt, *prevstmt;
	  CODEREP *cmp = bb->Last_stmtrep()->Rhs();
	  for (stmt = bb->Last_stmtrep(), prevstmt = NULL;
	       stmt != NULL;
	       stmt = prevstmt ) {
	    prevstmt = stmt->Prev();
	    // test if the stmt is ivr introduced assignment.
	    if (!stmt->Ivr_introduced()	||
		! OPERATOR_is_scalar_store (stmt->Opr()))
	      continue;
	    // if the ivr_generated expr is still used
	    // by the comparision, cannot move it to the loopbody.
	    if (cmp->Contains(stmt->Lhs()))
	      continue;
	    bb->Remove_stmtrep(stmt);
	    stmt->Set_bb(insertbb);
	    insertbb->Prepend_stmtrep(stmt);
	  }
	}
      }
    }
  }

  if ( Get_Trace(TP_GLOBOPT, PROP_DUMP_FLAG)) {
    fprintf( TFile, "%sAfter COMP_UNIT::Do_copy_propagate\n%s",
	     DBar, DBar );
    Cfg()->Print(TFile);
  }

  Opt_tlog( "MAINPROP", 0, "%d copy propagations",
	    Htable()->Num_mainprops() );
}

