/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it would be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * Further, this software is distributed without any warranty that it is
 * free of the rightful claim of any third person regarding infringement 
 * or the like.  Any license provided herein, whether implied or 
 * otherwise, applies only to this software file.  Patent licenses, if 
 * any, provided herein do not apply to combinations of this program with 
 * other software, or any other product whatsoever.  
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write the Free Software Foundation, Inc., 59
 * Temple Place - Suite 330, Boston MA 02111-1307, USA.
 */

#ifdef USE_PCH
#include "be_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop

#include "defs.h"
#include "config.h"
#include "errors.h"
#include "erglob.h"
#include "tracing.h"
#include "glob.h"
#include "timing.h"
#include "wn.h"
#include "wn_util.h"
#include "wn_lower.h"

static WN * RETYPE_EXPR_expr(WN *tree, BOOL can_be_32bit, BOOL addr_expr);

// taken from wn_mp.cxx
static const char * const dope_str_prefix = ".dope." ;
static const INT dope_str_prefix_len = 6;

static BOOL
ST_Has_Dope_Vector(ST *st) {
  if (ST_class(st) != CLASS_VAR)
    return FALSE;

  if ( TY_is_f90_pointer(ST_type(st)) )
    return TRUE;

  TY_IDX ty = ST_type(st);
  while (TY_kind(ty) == KIND_POINTER)
    ty = TY_pointed(ty);

  if (TY_kind(ty) == KIND_STRUCT &&
      strncmp(TY_name(ty), dope_str_prefix, dope_str_prefix_len) == 0)
    return TRUE;

  return FALSE;
}

/* ====================================================================
 * walk down the tree and change the types of appropriate nodes to 32-bit
 * ==================================================================== */
static WN *Change_types_to_32bit(WN *tree)
{
  OPERATOR opr = WN_operator(tree);
  TYPE_ID rtype = WN_rtype(tree);
  TYPE_ID desc = WN_desc(tree);

  Is_True(MTYPE_is_integral(rtype),("unexpected mtype"));

  switch (opr) {
  // leaves
  case OPR_LDID: 
  case OPR_LDBITS: 
  case OPR_INTCONST: 
    WN_set_rtype(tree, Mtype_TransferSize(MTYPE_U4, rtype));
    if (MTYPE_byte_size(desc) == 8) {
      WN_set_desc(tree, Mtype_TransferSize(MTYPE_U4, desc));
      WN_set_ty(tree, MTYPE_To_TY(WN_desc(tree)));
    }
    break;

  case OPR_LDA: 
  case OPR_LDA_LABEL: 
    break;

  // unary
  case OPR_ILOAD: 
  case OPR_ILDBITS: 
    WN_set_rtype(tree, Mtype_TransferSize(MTYPE_U4, rtype));
    break;

  case OPR_EXTRACT_BITS: 
  case OPR_LNOT: 
    WN_set_rtype(tree, Mtype_TransferSize(MTYPE_U4, rtype));
    break;

  case OPR_SQRT: case OPR_RSQRT: case OPR_RECIP:
  case OPR_REALPART: case OPR_IMAGPART:
  case OPR_HIGHPART: case OPR_LOWPART:
  case OPR_ALLOCA:
  case OPR_PARM:
  case OPR_RND: case OPR_TRUNC: case OPR_CEIL: case OPR_FLOOR:
  case OPR_BNOT:
  case OPR_TAS: 
#ifdef TARG_X8664
  case OPR_ATOMIC_RSQRT:
#endif
    break;

  case OPR_PAREN:
  case OPR_NEG:
  case OPR_ABS:
  case OPR_MINPART: case OPR_MAXPART:
    WN_set_rtype(tree, Mtype_TransferSize(MTYPE_U4, rtype));
    WN_kid0(tree) = Change_types_to_32bit(WN_kid0(tree));
    break;

  case OPR_CVTL:
    WN_set_rtype(tree, Mtype_TransferSize(MTYPE_U4, rtype));
    WN_set_desc(tree, WN_rtype(tree));
    if (WN_cvtl_bits(tree) > 32) 
      WN_kid0(tree) = Change_types_to_32bit(WN_kid0(tree));
    break;

  case OPR_CVT:
    if (MTYPE_byte_size(rtype) <= 4)
      return tree;
    else if (MTYPE_byte_size(desc) == 8) 
      return Change_types_to_32bit(WN_kid0(tree)); // delete the CVT
    else return WN_kid0(tree); // delete the CVT
    break;

  // binary
  case OPR_MPY:
  case OPR_DIV: 
  case OPR_MOD: case OPR_REM:
  case OPR_DIVREM: 
    Is_True(MTYPE_byte_size(rtype) <= 4,("unexpected mtype size"));
    break;

  case OPR_ADD: case OPR_SUB:
  case OPR_MAX: case OPR_MIN: 
  case OPR_MINMAX:
    if (MTYPE_byte_size(rtype) == 8) {
      WN_set_rtype(tree, Mtype_TransferSize(MTYPE_U4, rtype));
      WN_kid0(tree) = Change_types_to_32bit(WN_kid0(tree));
      WN_kid1(tree) = Change_types_to_32bit(WN_kid1(tree));
    }
    break;

  case OPR_BAND: case OPR_BIOR: case OPR_BNOR: case OPR_BXOR:
  case OPR_ASHR: case OPR_LSHR:
  case OPR_SHL: 
  case OPR_COMPOSE_BITS:
    Is_True(MTYPE_byte_size(rtype) <= 4,("unexpected mtype size"));
    break;

  case OPR_EQ: case OPR_NE: 
  case OPR_GE: case OPR_GT: case OPR_LE: case OPR_LT: 
  case OPR_LAND: case OPR_LIOR:
    WN_set_rtype(tree, Mtype_TransferSize(MTYPE_U4, rtype));
    break;

  // ternary
  case OPR_SELECT:
    if (MTYPE_byte_size(rtype) == 8) {
      WN_set_rtype(tree, Mtype_TransferSize(MTYPE_U4, rtype));
      WN_kid0(tree) = Change_types_to_32bit(WN_kid0(tree));
      WN_kid1(tree) = Change_types_to_32bit(WN_kid1(tree));
    }
    break;

  case OPR_INTRINSIC_OP: 
    Is_True(MTYPE_byte_size(rtype) <= 4,("unexpected mtype size"));
    break;

  default: Is_True(FALSE,("unexpected operator"));
  }
  return tree;
}

/* ====================================================================
 * check if there is only 32-bit operands for the given operation; it assumes
 * RETYPE_EXPR_expr has already been called for the given tree.
 * ==================================================================== */
BOOL Only_32bit_opnds(WN *tree)
{
  OPERATOR opr = WN_operator(tree);
  TYPE_ID rtype = WN_rtype(tree);
  TYPE_ID desc = WN_desc(tree);

  if (!MTYPE_is_integral(rtype))
    return FALSE;

  switch (opr) {
  // leaves
  case OPR_LDID: 
    return MTYPE_byte_size(desc) <= 4 ||
           rtype == MTYPE_I8 && ST_Has_Dope_Vector(WN_st(tree));

  case OPR_LDBITS: return WN_bit_size(tree) <= 32;

  case OPR_INTCONST: return WN_const_val(tree) >= (INT32)0x80000000 &&
  			    WN_const_val(tree) <= 0x7fffffff;

  case OPR_CONST: 
  case OPR_LDA: 
  case OPR_LDA_LABEL: return FALSE;

  // unary
  case OPR_ILOAD: return MTYPE_byte_size(desc) <= 4;

  case OPR_ILDBITS: 
  case OPR_EXTRACT_BITS: return WN_bit_size(tree) <= 32;

  case OPR_LNOT: return TRUE;

  case OPR_SQRT: case OPR_RSQRT: case OPR_RECIP:
  case OPR_REALPART: case OPR_IMAGPART:
  case OPR_HIGHPART: case OPR_LOWPART:
  case OPR_ALLOCA:
  case OPR_PARM:
  case OPR_RND: case OPR_TRUNC: case OPR_CEIL: case OPR_FLOOR:
  case OPR_BNOT:
  case OPR_TAS: 
#ifdef TARG_X8664
  case OPR_ATOMIC_RSQRT:
#endif
    return MTYPE_byte_size(rtype) <= 4;

  case OPR_PAREN:
  case OPR_NEG:
  case OPR_ABS:
  case OPR_MINPART: case OPR_MAXPART:
    return Only_32bit_opnds(WN_kid0(tree)); 

  case OPR_CVTL: return WN_cvtl_bits(tree) <= 32 || 
  			Only_32bit_opnds(WN_kid0(tree));

  case OPR_CVT: return MTYPE_byte_size(rtype) <= 4 ||
		       MTYPE_is_integral(desc) && 
  		       (MTYPE_byte_size(desc) <= 4 || 
		        Only_32bit_opnds(WN_kid0(tree)));

  // binary
  case OPR_MLOAD: return FALSE;

  case OPR_MPY:
  case OPR_DIV: 
  case OPR_MOD: case OPR_REM:
  case OPR_DIVREM: return MTYPE_byte_size(rtype) <= 4;

  case OPR_ADD: case OPR_SUB:
  case OPR_MAX: case OPR_MIN: 
  case OPR_MINMAX:
    return MTYPE_byte_size(rtype) <= 4 ||
           Only_32bit_opnds(WN_kid0(tree)) && Only_32bit_opnds(WN_kid1(tree));

  case OPR_BAND: case OPR_BIOR: case OPR_BNOR: case OPR_BXOR:
  case OPR_ASHR: case OPR_LSHR:
  case OPR_SHL: 
  case OPR_COMPOSE_BITS:
    return MTYPE_byte_size(rtype) <= 4;

  case OPR_EQ: case OPR_NE: 
  case OPR_GE: case OPR_GT: case OPR_LE: case OPR_LT: 
  case OPR_LAND: case OPR_LIOR:
    return TRUE;

  // ternary
  case OPR_SELECT:
    return MTYPE_byte_size(rtype) <= 4 ||
           Only_32bit_opnds(WN_kid1(tree)) && Only_32bit_opnds(WN_kid2(tree));

  // n-ary
  case OPR_INTRINSIC_OP: 
    return MTYPE_byte_size(rtype) <= 4;

  default: Is_True(FALSE,("unexpected operator"));
  }

  return FALSE;
}

/* ====================================================================
 * Only 32-bit or less of the value computed by the tree is needed, so
 * shrink the type of the computation to 32-bit if it will not affect the
 * correctness, which in turn depends on the size of the operands.
 * ==================================================================== */
static WN *
Shrink_to_32bit(WN *tree)
{
  OPERATOR opr = WN_operator(tree);
  TYPE_ID rtype = WN_rtype(tree);
  TYPE_ID desc = WN_desc(tree);
  TYPE_ID orig_kid0_rtype, orig_kid1_rtype;

  if (MTYPE_byte_size(rtype) == 8) {
    switch (opr) {
    case OPR_MPY:
    case OPR_DIV: 
    case OPR_MOD: case OPR_REM:
    case OPR_DIVREM:
      orig_kid0_rtype = WN_rtype(WN_kid0(tree));
      orig_kid1_rtype = WN_rtype(WN_kid1(tree));
      WN_kid0(tree) = RETYPE_EXPR_expr(WN_kid0(tree), TRUE, FALSE);
      WN_kid1(tree) = RETYPE_EXPR_expr(WN_kid1(tree), TRUE, FALSE);
      if (Only_32bit_opnds(WN_kid0(tree)) && Only_32bit_opnds(WN_kid1(tree)) &&
	  (opr != OPR_MPY || (WN_operator(WN_kid0(tree)) != OPR_INTCONST &&
	  		      WN_operator(WN_kid1(tree)) != OPR_INTCONST))) {
	// not doing for mult with const so as not to disable lea optimization
	WN_kid0(tree) = Change_types_to_32bit(WN_kid0(tree));
	WN_kid1(tree) = Change_types_to_32bit(WN_kid1(tree));
	WN_set_rtype(tree, MTYPE_I4);
      }
      else {
	TYPE_ID kid0_rtype = WN_rtype(WN_kid(tree, 0));
	if (orig_kid0_rtype != kid0_rtype &&
	    MTYPE_byte_size(WN_rtype(WN_kid(tree, 0))) < 8) { // insert CVT
	  WN_kid(tree, 0) = WN_Cvt(kid0_rtype, 
				   Mtype_TransferSize(MTYPE_I8, kid0_rtype), 
				   WN_kid(tree, 0));
	}
	TYPE_ID kid1_rtype = WN_rtype(WN_kid(tree, 1));
	if (orig_kid1_rtype != kid1_rtype &&
	    MTYPE_byte_size(WN_rtype(WN_kid(tree, 1))) < 8) { // insert CVT
	  WN_kid(tree, 1) = WN_Cvt(kid1_rtype, 
				   Mtype_TransferSize(MTYPE_I8, kid1_rtype), 
				   WN_kid(tree, 1));
	}
      }
      return tree;
  
    default: ;
    }
  }
  return RETYPE_EXPR_expr(tree, TRUE, FALSE);
}

/* ====================================================================
 * Traverse the given expr wn and find 64-bit expressions that can be retyped to
 * 32-bit without affecting the results; can_be_32bit gives whether the current
 * expr can be converted to 32-bit if all the operands are 32-bit; addr_expr 
 * gives whether we are in an expression that performs address computation;
 * if addr_expr is true, any 64-bit integer mul/div/mod/rem can be converted 
 * to 32-bit if all the operands are 32-bit.
 * ==================================================================== */
static WN *
RETYPE_EXPR_expr(WN *tree, BOOL can_be_32bit, BOOL addr_expr)
{
  INT i;
  OPERATOR opr = WN_operator(tree);
  TYPE_ID rtype = WN_rtype(tree);
  TYPE_ID desc = WN_desc(tree);

  switch (opr) {
  // leaves
  case OPR_LDID: 
  case OPR_LDBITS:
  case OPR_INTCONST:
  case OPR_CONST:
  case OPR_LDA:
  case OPR_LDA_LABEL:
    return tree;

  // unary
  case OPR_ILOAD:
  case OPR_ILDBITS:
    WN_kid0(tree) = RETYPE_EXPR_expr(WN_kid0(tree), FALSE, TRUE);
    return tree;

  case OPR_SQRT: case OPR_RSQRT: case OPR_RECIP:
  case OPR_PAREN:
  case OPR_REALPART: case OPR_IMAGPART:
  case OPR_HIGHPART: case OPR_LOWPART:
  case OPR_ALLOCA:
  case OPR_LNOT:
  case OPR_EXTRACT_BITS:
  case OPR_BNOT:
  case OPR_PARM:
  case OPR_TAS:
  case OPR_RND: case OPR_TRUNC: case OPR_CEIL: case OPR_FLOOR:
#ifdef TARG_X8664
  case OPR_REPLICATE:
  case OPR_REDUCE_ADD: case OPR_REDUCE_MPY: 
  case OPR_REDUCE_MAX: case OPR_REDUCE_MIN:
  case OPR_SHUFFLE:
  case OPR_ATOMIC_RSQRT:
#endif // TARG_X8664
    WN_kid0(tree) = RETYPE_EXPR_expr(WN_kid0(tree), FALSE, FALSE);
    return tree;

  case OPR_NEG:
  case OPR_ABS:
  case OPR_MINPART: case OPR_MAXPART:
    WN_kid0(tree) = RETYPE_EXPR_expr(WN_kid0(tree), can_be_32bit, addr_expr);
    return tree;

  case OPR_CVTL:
    if (WN_cvtl_bits(tree) > 32)
      WN_kid0(tree) = RETYPE_EXPR_expr(WN_kid0(tree), FALSE, FALSE);
    else WN_kid0(tree) = RETYPE_EXPR_expr(WN_kid0(tree), TRUE, FALSE);
    return tree;

  case OPR_CVT:
    if (! MTYPE_is_integral(rtype) || 
        ! MTYPE_is_integral(desc) || 
	MTYPE_byte_size(rtype) == 8)
      WN_kid0(tree) = RETYPE_EXPR_expr(WN_kid0(tree), FALSE, FALSE);
    else {
      WN_kid0(tree) = RETYPE_EXPR_expr(WN_kid0(tree), TRUE, FALSE);
      if (can_be_32bit && MTYPE_byte_size(WN_rtype(WN_kid0(tree))) <= 8)
	return WN_kid0(tree); // can skip the CVT
    }
    return tree;

  // binary
  case OPR_MLOAD:
    WN_kid0(tree) = RETYPE_EXPR_expr(WN_kid0(tree), FALSE, TRUE);
    WN_kid1(tree) = RETYPE_EXPR_expr(WN_kid1(tree), FALSE, FALSE);
    return tree;

  case OPR_MPY:
  case OPR_DIV: 
  case OPR_MOD: case OPR_REM:
  case OPR_DIVREM:
    if (MTYPE_byte_size(rtype) == 8 && (can_be_32bit || addr_expr)) {
      tree = Shrink_to_32bit(tree);
      if (MTYPE_byte_size(WN_rtype(tree)) < 8 && !can_be_32bit) { // insert CVT
	return WN_Cvt(WN_rtype(tree), rtype, tree);
      } 
      return tree;
    }
    // fall thru

  case OPR_ADD: case OPR_SUB:
  case OPR_MAX: case OPR_MIN: 
  case OPR_MINMAX:
    WN_kid0(tree) = RETYPE_EXPR_expr(WN_kid0(tree), can_be_32bit, addr_expr);
    if (MTYPE_byte_size(rtype) == 8 &&
        MTYPE_byte_size(WN_rtype(WN_kid0(tree))) != 8)
      WN_kid0(tree) = WN_Cvt(WN_rtype(WN_kid0(tree)), rtype, WN_kid0(tree));
    WN_kid1(tree) = RETYPE_EXPR_expr(WN_kid1(tree), can_be_32bit, addr_expr);
    if (MTYPE_byte_size(rtype) == 8 &&
        MTYPE_byte_size(WN_rtype(WN_kid1(tree))) != 8)
      WN_kid1(tree) = WN_Cvt(WN_rtype(WN_kid1(tree)), rtype, WN_kid1(tree));
    return tree;

  case OPR_BAND: case OPR_BIOR: case OPR_BNOR: case OPR_BXOR:
  case OPR_ASHR: case OPR_LSHR:
  case OPR_SHL: 
  case OPR_EQ: case OPR_NE: 
  case OPR_GE: case OPR_GT: case OPR_LE: case OPR_LT:
  case OPR_LAND: case OPR_LIOR:
  case OPR_COMPLEX:
    WN_kid0(tree) = RETYPE_EXPR_expr(WN_kid0(tree), FALSE, FALSE);
    WN_kid1(tree) = RETYPE_EXPR_expr(WN_kid1(tree), FALSE, FALSE);
    return tree;

  case OPR_COMPOSE_BITS:
    if (WN_bit_size(tree) <= 32)
      WN_kid0(tree) = RETYPE_EXPR_expr(WN_kid0(tree), TRUE, FALSE);
    else WN_kid0(tree) = RETYPE_EXPR_expr(WN_kid0(tree), FALSE, FALSE);
    WN_kid1(tree) = RETYPE_EXPR_expr(WN_kid1(tree), FALSE, FALSE);
    return tree;

  // ternary
  case OPR_SELECT:
    WN_kid0(tree) = RETYPE_EXPR_expr(WN_kid0(tree), FALSE, FALSE);
    WN_kid1(tree) = RETYPE_EXPR_expr(WN_kid1(tree), can_be_32bit, addr_expr);
    WN_kid2(tree) = RETYPE_EXPR_expr(WN_kid2(tree), can_be_32bit, addr_expr);
    return tree;

  // n-ary
  case OPR_INTRINSIC_OP: 
    for (i = 0; i < WN_kid_count(tree); i++)  // kids must be PARMs
      RETYPE_EXPR_expr(WN_kid(tree,i), FALSE, FALSE); 
    return tree;

  default: Is_True(FALSE,("unexpected operator"));
  }

  return NULL;
}

/* ====================================================================
 * In the given stmt wn, find 64-bit expressions that can be retyped to
 * 32-bit without affecting the results and call RETYPE_EXPR_expr to perform 
 * the retyping
 * ==================================================================== */
static void
RETYPE_EXPR_stmt(WN *tree)
{
  OPERATOR opr = WN_operator(tree);
  INT i;

  switch (opr) {

  // operators with no expression as kid
  case OPR_GOTO:
  case OPR_GOTO_OUTER_BLOCK:
  case OPR_RETURN:
  case OPR_COMMENT:
  case OPR_TRAP	:
  case OPR_FORWARD_BARRIER:
  case OPR_BACKWARD_BARRIER:
  case OPR_ALTENTRY:
  case OPR_PRAGMA:
  case OPR_LABEL:
  case OPR_REGION:
  case OPR_REGION_EXIT:
    return;

  case OPR_DEALLOCA:	
  case OPR_PREFETCH:
  case OPR_AGOTO:
    WN_kid0(tree) = RETYPE_EXPR_expr(WN_kid0(tree), FALSE, TRUE);
    return;

  case OPR_EVAL: 
  case OPR_TRUEBR:
  case OPR_FALSEBR:
  case OPR_ASSERT:
  case OPR_RETURN_VAL:
  case OPR_COMPGOTO:
  case OPR_XGOTO:
  case OPR_XPRAGMA:
    WN_kid0(tree) = RETYPE_EXPR_expr(WN_kid0(tree), FALSE, FALSE);
    return;

  case OPR_ISTORE:
    WN_kid1(tree) = RETYPE_EXPR_expr(WN_kid1(tree), FALSE, TRUE);
  // fall through

  case OPR_STID:
    if (MTYPE_byte_size(WN_desc(tree)) != 8)
      WN_kid0(tree) = RETYPE_EXPR_expr(WN_kid0(tree), TRUE, FALSE);
    else WN_kid0(tree) = RETYPE_EXPR_expr(WN_kid0(tree), FALSE, FALSE);
    return;

  case OPR_ISTBITS:
    WN_kid1(tree) = RETYPE_EXPR_expr(WN_kid1(tree), FALSE, TRUE);
  // fall through

  case OPR_STBITS:
    if (WN_bit_size(tree) <= 32)
      WN_kid0(tree) = RETYPE_EXPR_expr(WN_kid0(tree), TRUE, FALSE);
    else WN_kid0(tree) = RETYPE_EXPR_expr(WN_kid0(tree), FALSE, FALSE);
    return;

  case OPR_CALL:          
  case OPR_ICALL:
  case OPR_INTRINSIC_CALL:
    for (i = 0; i < WN_kid_count(tree); i++) 
      WN_kid(tree,i) = RETYPE_EXPR_expr(WN_kid(tree,i), FALSE, FALSE);
    return;

  case OPR_PICCALL:
    for (i = 0; i < (WN_kid_count(tree)-1); i++) 
      WN_kid(tree,i) = RETYPE_EXPR_expr(WN_kid(tree,i), FALSE, FALSE);
    return;

  case OPR_ASM_STMT:
    for (i = 2; i < WN_kid_count(tree); i++) {
      WN_kid0(WN_kid(tree,i)) = RETYPE_EXPR_expr(WN_kid0(WN_kid(tree,i)), FALSE, FALSE);
    }
    return;

  case OPR_MSTORE:
    WN_kid0(tree) = RETYPE_EXPR_expr(WN_kid0(tree), FALSE, FALSE);
    WN_kid1(tree) = RETYPE_EXPR_expr(WN_kid1(tree), FALSE, TRUE);
    WN_kid2(tree) = RETYPE_EXPR_expr(WN_kid2(tree), FALSE, FALSE);
    return;

  case OPR_LOOP_INFO:
    if (WN_kid1(tree) != NULL) {
      WN_kid1(tree) = RETYPE_EXPR_expr(WN_kid1(tree), FALSE, FALSE);
    }
    return;

  // structured control flow statements

  case OPR_BLOCK: {
    WN *stmt;
    for (stmt = WN_first(tree); stmt; stmt = WN_next(stmt)) {
      Is_True(OPERATOR_is_stmt(WN_operator(stmt)) || OPERATOR_is_scf(WN_operator(stmt)),
	      ("statement operator expected"));
      RETYPE_EXPR_stmt(stmt);
    }
    return;
    }

  case OPR_DO_LOOP:
    RETYPE_EXPR_stmt(WN_kid(tree, 1)); // the initialization statement
    WN_kid2(tree) = RETYPE_EXPR_expr(WN_kid2(tree), FALSE, FALSE);
    RETYPE_EXPR_stmt(WN_kid(tree, 3)); // the increment statement
    RETYPE_EXPR_stmt(WN_kid(tree, 4)); // the block
    if (WN_kid_count(tree) == 6)
      RETYPE_EXPR_stmt(WN_kid(tree, 5)); // LOOP_INFO
    return;

  case OPR_DO_WHILE:
  case OPR_WHILE_DO:
    WN_kid0(tree) = RETYPE_EXPR_expr(WN_kid0(tree), FALSE, FALSE);
    RETYPE_EXPR_stmt(WN_kid(tree, 1));
    return;

  case OPR_IF:
    WN_kid0(tree) = RETYPE_EXPR_expr(WN_kid0(tree), FALSE, FALSE);
    RETYPE_EXPR_stmt(WN_kid(tree, 1));
    RETYPE_EXPR_stmt(WN_kid(tree, 2));
    return;

  default:	
    Is_True(FALSE,("unexpected operator"));
  }

  return;  
}

/* ====================================================================
 * Top level routine for finding 64-bit expressions that can be retyped to
 * 32-bit without affecting the results
 * ==================================================================== */
void
WN_retype_expr(WN *tree)
{
  Start_Timer(T_Lower_CU);
  Set_Error_Phase("WN_retype_expr");

  if (WN_operator(tree) == OPR_FUNC_ENTRY) 
    RETYPE_EXPR_stmt(WN_func_body(tree));
  else if (WN_operator(tree) == OPR_REGION) 
    RETYPE_EXPR_stmt(WN_region_body(tree));
  else if (OPERATOR_is_stmt(WN_operator(tree)) || OPERATOR_is_scf(WN_operator(tree)))
    RETYPE_EXPR_stmt(tree);
  else Is_True(FALSE, ("unexpected WHIRL operator"));

  Stop_Timer(T_Lower_CU);

  WN_Lower_Checkdump("After wn_retype_expr", tree, 0);   

  WN_verifier(tree);

  return;
}
