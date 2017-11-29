/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

//-*-c++-*-

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_fold.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_fold.cxx,v $
//
// Revision history:
//  31-MAY-95 dahl - Original Version
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
// Perform folding operations that produce integer constants.
// Calls the simplifier to do the actual work. The routines here
// are to interface to the simplifier and make it work with CRs
// instead of WNs.
//
// ====================================================================
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#ifdef _KEEP_RCS_ID
#define opt_fold_CXX "opt_fold.cxx"
static char *rcs_id = opt_fold_CXX"$Revision: 1.9 $";
#endif /* _KEEP_RCS_ID */

#include <stdint.h>
#include "defs.h"
#include "errors.h"
#include "erglob.h"
#include "tracing.h"  	      	// for TFile
#include "config_opt.h"		// for Div_Split_Allowed, Recip_Allowed

#include "opt_wn.h"		// for WN_operator
#include "opcode_core.h"	// for opcodes
#include "wn_map.h"		// for WN maps
#include "opt_sym.h" 	      	// for symbol table
#include "targ_const.h"	      	// for Targ_Is_Zero
#include "stab.h"		// for MTYPE_To_TY
#include "const.h"    	      	// for Gen_Const_Sym / New_Const_Sym
#include "opt_util.h" 	      	// for Warn_todo
#include "opt_config.h"         // for WOPT_Enable_Fold2const
#include "opt_fold.h"		// FOLD class, includes opt_htable.h, config.h
#include "opt_htable.h"		// hash functions, ISOP flags

// forward definitions
// the only routines in the simplifier that create nodes are:
// CR_SimpCreateExp1, CR_SimpCreateExp2, CR_CreateIntconst, CR_CreateFPconst
static CODEREP *CR_Create(OPCODE, INT, CODEREP *k0, CODEREP *k1 = NULL,
			  CODEREP *k2 = NULL);
static CODEREP *CR_SimpCreateExp1(OPCODE, CODEREP *);
static CODEREP *CR_SimpCreateExp2(OPCODE, CODEREP *, CODEREP *);
static OPCODE   CR_opcode(CODEREP *);
static OPERATOR CR_operator(CODEREP *);
static ST      *CR_st(CODEREP *);
static TY_IDX   CR_ty(CODEREP *);
static void     CR_set_kid0(CODEREP *, CODEREP *);
static CODEREP *CR_kid0(CODEREP *);
static CODEREP *CR_kid(CODEREP *, INT);
static INT32	CR_kid_count(CODEREP *);
static CODEREP *Combine_bits_any_nzero_test(CODEREP *);
static CODEREP *Combine_bits_all_zero_test(CODEREP *);

//============================================================================
// usage:
//	ptr = Fold_Expr(cr,htable);
// returns:
//	NULL	no rehash necessary, Fold_Expr did nothing
//	CONST	rehash is necessary, Fold_Expr folded to a constant
//	ptr	no rehash necessary, folded into a variable or expression
// globals used:
//	Force_IEEE_Comparisons: TRUE safe folding, FALSE aggressive folding
//    	      	      	      	applies to floats only
//	WOPT_Enable_Fold2const:	TRUE do Fold_Expr, FALSE do nothing
//
//	Note: if WOPT_Enable_Fold2const is TRUE bu WOPT_Enable_CRSIMP is
//		FALSE, nothing will be simplified.
//	WOPT_Enable_CRSIMP controls the CR simplifier.
//	WOPT_Enable_Fold2const controls the constant folder which needs
//		to have WOPT_Enable_CRSIMP on to do anything. (The folder
//		during copy propagation cannot be on while the CR simplifier
//		is off.)
// 	WOPT_Enable_CRSIMP WOPT_Enable_Fold2const action
//      ------------------------------------------------
//		0		0		  both off
//		0		1		  both off, folder needs CRSIMP
//		1		0		  CR simplifier
//		1		1		  CR simplifier and folder
//============================================================================

// The simplifier needs to know floating point values. To get these we
// need the htable pointer to find the symbol table. wn_simp_code.h
// is not written to pass the htable pointer down to the appropriate
// places so we use this static variable. It's a hack.
static CODEMAP *fold_htable;

// We need to turn tracing on/off in the simplifier without making it
// a member of the FOLD class.
static BOOL     fold_trace;

static CODEREP * CR_CreateFPconst(TCON tc);

void
Initialize_CR_simp(CODEMAP *htable)
{
  fold_htable = htable;
}

// entry point for single level constant folder
CODEREP *
FOLD::Fold_Expr(CODEREP *cr)
{
  if (!WOPT_Enable_Fold2const)	// do nothing if OFF
    return NOHASH;

#ifdef KEY
  if (cr->Kind() == CK_IVAR)
    return CR_Simplify_Iload(cr);
#endif

  if (cr->Kind() != CK_OP)
    return NOHASH;

  return CR_Simplify_Expr(cr);
}

// entry point for multi-level constant folder (call only once at root of expr)
CODEREP *
FOLD::Fold_Tree(CODEREP *cr)
{
  if (!WOPT_Enable_Fold2const)	// do nothing if OFF
    return NOHASH;

  if (cr->Kind() != CK_OP)
    return NOHASH;

  return CR_Simplify_Tree(cr);
}

//============================================================================
// default constructor sets debug flag
FOLD::FOLD(void)
{
  fold_trace = Get_Trace(TP_GLOBOPT, FOLD_DUMP_FLAG);
}

//============================================================================
// for debugging, print a string and the current sub-tree
void
FOLD::Print(const char *str, const CODEREP *cr)
{
  if (fold_trace)
  { fprintf(TFile,"Fold_Expr: %s:\n",str);
    if (cr)
      cr->Print(0,TFile);
    else
      fprintf(TFile,"NULL\n");
    fflush(TFile);
  }
}

//============================================================================
//============================================================================
// the following static routines are based on wn_simp.c in common/com

// given an opcode, create a CR for it.
static CODEREP *
CR_Create(OPCODE op, INT nkids, CODEREP *k0, CODEREP *k1, CODEREP *k2)
{
  CODEREP *cr = Alloc_stack_cr(nkids+IVAR_EXTRA_NODE_CNT);
  CODEREP *retv = NULL;
  const OPERATOR oper = OPCODE_operator(op);

  if (OPCODE_is_leaf(op)) {
    if (oper == OPR_LDID) {
      if (fold_trace)
	fprintf(TFile,"CR_Create, LDID\n");
    } else if (oper == OPR_LDBITS) {
      if (fold_trace)
	fprintf(TFile,"CR_Create, LDBITS\n");
    } else if (oper == OPR_LDA) {
      if (fold_trace)
	fprintf(TFile,"CR_Create, LDA\n");
    } else if (oper == OPR_INTCONST) {
      if (fold_trace)
	fprintf(TFile,"CR_Create, INT const\n");
    } else if (oper == OPR_CONST) {
      if (fold_trace)
	fprintf(TFile,"CR_Create, FP const\n");
    } else
      FmtAssert(FALSE,("CR_Create, unknown leaf opcode %s",OPCODE_name(op)));
  } else if (oper == OPR_ILOAD) {
      if (fold_trace)
	fprintf(TFile,"CR_Create, ILOAD\n");
  } else if (oper == OPR_ILDBITS) {
       if (fold_trace)
	fprintf(TFile,"CR_Create, ILDBITS\n");
  } else if (oper == OPR_MLOAD) {
      if (fold_trace)
	fprintf(TFile,"CR_Create, MLOAD\n");
  } else {
      Is_True(nkids == 1 || nkids == 2 || nkids == 3,
	      ("CR_Create, can't handle nkids=%d",nkids));
      if (fold_trace)
	fprintf(TFile,"CR_Create, %s, nkids=%d\n",OPCODE_name(op),nkids);
      cr->Init_op(op,nkids);
      cr->Set_opnd(0,k0);
      if (nkids >= 2)
	cr->Set_opnd(1,k1);
      if (nkids == 3)
	cr->Set_opnd(2,k2);
      retv = fold_htable->Hash_Op(cr);
  }
  Is_True(retv,("CR_Create, needs work"));
  return retv;
}

static void
show_tree(OPCODE opc, CODEREP *k0, CODEREP *k1, CODEREP *r)
{
  if (fold_trace) {
    fprintf(TFile,"\nBefore:\n");
    k0->Print(0,TFile);
    if (OPCODE_operator(opc) != OPR_CVTL) {
      if (k1)
	k1->Print(0,TFile);
      fprintf(TFile,"%s\n",OPCODE_name(opc));
    } else
      fprintf(TFile,"%s %ld\n",OPCODE_name(opc),(INTPS) k1);
    fprintf(TFile,"=====\nAfter:\n");
    r->Print(0,TFile);
    fprintf(TFile,"-----------------------------------------------------\n");
  }
}

// return value:
//   	valid hashed CR if a simplification was done
//	NOHASH if nothing was done
CODEREP *
FOLD::CR_Simplify_Tree(CODEREP *cr)
{
  // based on wn_simp.c, WN_Simplify_Tree()
  OPCODE   op = SIMPNODE_opcode(cr);
  OPERATOR opr= OPCODE_operator(op);
  CODEREP *k0, *k1, *k2, *r = NULL, *result;
  INT numkids = SIMPNODE_kid_count(cr), i;
  BOOL found = FALSE;

  if (!WOPT_Enable_CRSIMP || cr->Is_isop_flag_set(ISOP_FOLD_TREE_VISITED))
    return NOHASH;

  if (opr == OPR_INTRINSIC_OP
#ifdef KEY
      || opr == OPR_PURE_CALL_OP
#endif
     ) {
    for (i=0; i<numkids; i++) {
      k0 = CR_Simplify_Tree(cr->Opnd(i));
      if (k0 == NOHASH)
	k0 = cr->Opnd(i);
      else if (k0 != cr->Opnd(i))
	found = TRUE;
      cr->Set_opnd(i,k0);
    }
#ifdef KEY
    if (opr == OPR_PURE_CALL_OP)
      result = NULL;
    else
#endif
    result = SIMPNODE_SimplifyIntrinsic(op, SIMPNODE_intrinsic(cr), numkids,
					cr->Opnd_ptr());
    if (result) {
      CODEREP *tmp = Alloc_stack_cr(numkids+IVAR_EXTRA_NODE_CNT);
      tmp->Copy(*result);
      result = fold_htable->Rehash(tmp);
    } else if (found) {
      cr->Set_coderep_id(0);
      result = fold_htable->Rehash(cr);
    }
    else
      result = NOHASH;
  } else if (numkids == 0) {		// zero children
    result = NOHASH;
  } else if (numkids == 1) {		// unary operators and IVAR
    CODEREP *kid = SIMPNODE_kid0(cr);	// could be CK_OP or CK_IVAR
    k0 = CR_Simplify_Tree(kid);
    if (k0 == NOHASH)
      k0 = kid;
    else if (k0 != kid)
      found = TRUE;
    if ( opr != OPR_CVTL )
      r = SIMPNODE_SimplifyExp1(op, k0);
    else
      r = SIMPNODE_SimplifyCvtl(op, /*WN_cvtl_bits(t)*/cr->Offset(), k0);
    if (r) {
      SIMPNODE_DELETE(cr);
      result = r;			// no need for rehash, already done
    } else {
      if (found) {
        CODEREP *tmp = Alloc_stack_cr(numkids+IVAR_EXTRA_NODE_CNT);
	tmp->Copy(*cr);
	CR_set_kid0(tmp,k0);
	result = fold_htable->Rehash(tmp);
      } else
	result = NOHASH;
    }
  } else if (numkids == 2) {		// binary operators
    k0 = CR_Simplify_Tree(cr->Opnd(0));
    if (k0 == NULL)
      k0 = cr->Opnd(0);
    else if (k0 != cr->Opnd(0))
      found = TRUE;

    k1 = CR_Simplify_Tree(cr->Opnd(1));
    if (k1 == NULL)
      k1 = cr->Opnd(1);
    else if (k1 != cr->Opnd(1))
      found = TRUE;

    r = SIMPNODE_SimplifyExp2(op, k0, k1);
    if (r) {
      SIMPNODE_DELETE(cr);
      result = r;			// no need for rehash, already done
    } else {
      if (found) {
        CODEREP *tmp = Alloc_stack_cr(numkids+IVAR_EXTRA_NODE_CNT);
	tmp->Copy(*cr);
	tmp->Set_opnd(0,k0);
        tmp->Set_opnd(1,k1);
	result = fold_htable->Rehash(tmp);
      } else
	result = NOHASH;
    }
  } else if (numkids == 3) {		// binary operators
    k0 = CR_Simplify_Tree(cr->Opnd(0));
    if (k0 == NULL)
      k0 = cr->Opnd(0);
    else if (k0 != cr->Opnd(0))
      found = TRUE;

    k1 = CR_Simplify_Tree(cr->Opnd(1));
    if (k1 == NULL)
      k1 = cr->Opnd(1);
    else if (k1 != cr->Opnd(1))
      found = TRUE;

    k2 = CR_Simplify_Tree(cr->Opnd(2));
    if (k2 == NULL)
      k2 = cr->Opnd(2);
    else if (k2 != cr->Opnd(2))
      found = TRUE;

    r = SIMPNODE_SimplifyExp3(op, k0, k1, k2);
    if (r) {
      SIMPNODE_DELETE(cr);
      result = r;			// no need for rehash, already done
    } else {
      if (found) {
        CODEREP *tmp = Alloc_stack_cr(numkids+IVAR_EXTRA_NODE_CNT);
	tmp->Copy(*cr);
	tmp->Set_opnd(0,k0);
        tmp->Set_opnd(1,k1);
        tmp->Set_opnd(2,k2);
	result = fold_htable->Rehash(tmp);
      } else
	result = NOHASH;
    }
 } else {				// arrays, calls, etc
    for (i=0; i<numkids; i++) {
      k0 = CR_Simplify_Tree(cr->Opnd(i));
      if (k0 == NULL)
	k0 = cr->Opnd(i);
      else if (k0 != cr->Opnd(i))
	found = TRUE;
      cr->Set_opnd(i,k0);
    }
    if (found) {
      cr->Set_coderep_id(0);
      result = fold_htable->Rehash(cr);
    } else 
      result =NOHASH;
  }
  return result;
}

// return value:
//   	valid hashed CR if a simplification was done
//	NOHASH if nothing was done
//      This differs from CR_Simplify_Tree in that we don't walk the children
//      because we assume they have already been simplified
//
CODEREP *
FOLD::CR_Simplify_Expr(CODEREP *cr)
{
   // based on wn_simp.c, WN_Simplify_Tree()
   OPCODE   op = SIMPNODE_opcode(cr);
   OPERATOR opr = OPCODE_operator(op);
   CODEREP *k0, *k1, *k2, *r = NULL, *result;
   INT numkids = SIMPNODE_kid_count(cr);
   BOOL found = FALSE;
   
   if (!WOPT_Enable_CRSIMP || cr->Is_isop_flag_set(ISOP_FOLD_EXPR_VISITED))
     return NOHASH;
   
   if (opr == OPR_INTRINSIC_OP) {

      result = SIMPNODE_SimplifyIntrinsic(op, SIMPNODE_intrinsic(cr), numkids,
					  cr->Opnd_ptr());
      if (result) {
	CODEREP *tmp = Alloc_stack_cr(numkids+IVAR_EXTRA_NODE_CNT);
	tmp->Copy(*result);
	result = fold_htable->Rehash(tmp);
      } else
	result = NOHASH;

#ifdef KEY
   } else if (opr == OPR_PURE_CALL_OP) {
      result = NOHASH;
#endif
   } else if (numkids == 1) {	// unary operators and IVAR

      CODEREP *k0 = SIMPNODE_kid0(cr); // could be CK_OP or CK_IVAR
      found = check_convert(cr, &k0, 0);
#if defined(TARG_X8664) || defined(TARG_LOONGSON)
      // Bug 5935 - disable simplification because if it is a SQRT(RECIP)
      // when the type is F8 because there is no x86 instruction for translating
      // F8RSQRT.
      if ( op == OPC_F8SQRT && SIMPNODE_opcode(k0) == OPC_F8RECIP )
	return NOHASH;
#endif

      // bug fix for OSP_130
      // disabe simplification because SIMPNODE_SimplifyCvtl will not
      // set the bit_offset and bit_size of EXTRACT_BITS and COMPOSE_BITS,
      // which is related to lower_level bit_fields operation
      // TODO: pass the corresponding bit_offset and bit_size
      //
      if ((opr == OPR_EXTRACT_BITS || opr == OPR_COMPOSE_BITS) &&
		      SIMPNODE_operator(k0) == OPR_SELECT) {
	return NOHASH;
      }

      if ( opr != OPR_CVTL )
	r = SIMPNODE_SimplifyExp1(op, k0);
      else
	r = SIMPNODE_SimplifyCvtl(op, /*WN_cvtl_bits(t)*/cr->Offset(), k0);
      if (r) {
	 SIMPNODE_DELETE(cr);
	 result = r;			// no need for rehash, already done
      } else {
	if (found) {
	  CODEREP *tmp = Alloc_stack_cr(numkids+IVAR_EXTRA_NODE_CNT);
	  tmp->Copy(*cr);
	  tmp->Set_opnd(0,k0);
	  result = fold_htable->Rehash(tmp);
	} else
	  result = NOHASH;
      }

   } else if (numkids == 2) {	// binary operators

      k0 = cr->Opnd(0);
      found = check_convert(cr, &k0, 0);
      k1 = cr->Opnd(1);
      found |= check_convert(cr, &k1, 1);
      r = SIMPNODE_SimplifyExp2(op, k0, k1);
      if (r) {
	 if (SIMPNODE_rtype(r) == MTYPE_F8 && 
   	   SIMPNODE_rtype(cr) == MTYPE_C8)
	 {
	   CODEREP *zero;
	   zero = CR_CreateFPconst(Host_To_Targ_Float(MTYPE_F8, 0.0)); 
	   r = CR_Create(OPC_C8PAIR, 2, r, zero, NULL);
	 }
	 SIMPNODE_DELETE(cr);
	 result = r;			// no need for rehash, already done
      } else {
	if (found) {
	  CODEREP *tmp = Alloc_stack_cr(numkids+IVAR_EXTRA_NODE_CNT);
	  tmp->Copy(*cr);
	  tmp->Set_opnd(0,k0);
	  tmp->Set_opnd(1,k1);
	  result = fold_htable->Rehash(tmp);
	} else
#ifndef KEY
	  result = NOHASH;
#else // bug 8581
	  result = Combine_bits_any_nzero_test(cr);
	  if (result == NOHASH)
	    result = Combine_bits_all_zero_test(cr);
#endif
      }

   } else if (numkids == 3) {	// trinary operators
      k0 = cr->Opnd(0);
      found = check_convert(cr, &k0, 0);
      k1 = cr->Opnd(1);
      found |= check_convert(cr, &k1, 1);
      k2 = cr->Opnd(2);
      found |= check_convert(cr, &k2, 2);
      r = SIMPNODE_SimplifyExp3(op, k0, k1, k2);
      if (r) {
	 SIMPNODE_DELETE(cr);
	 result = r;			// no need for rehash, already done
      } else {
	if (found) {
	  CODEREP *tmp = Alloc_stack_cr(numkids+IVAR_EXTRA_NODE_CNT);
	  tmp->Copy(*cr);
	  tmp->Set_opnd(0,k0);
	  tmp->Set_opnd(1,k1);
	  tmp->Set_opnd(2,k2);
	  result = fold_htable->Rehash(tmp);
	} else
	  result = NOHASH;
      }
   } else {			// arrays, calls, zero children, etc
      result = NOHASH;
   }
   return result;
}

#ifdef KEY
static CODEREP *CR_CreateIntconst(OPCODE opc, INT64 val);

// return value:
//   	valid hashed CR if ILOAD can be folded to a constant
//	NOHASH if nothing was done
//
CODEREP *
FOLD::CR_Simplify_Iload(CODEREP *cr)
{
  if (cr->Ilod_base()->Kind() != CK_LDA)
    return NOHASH;
  if (MTYPE_byte_size(cr->Dsctyp()) != 1)
    return NOHASH;
  CODEREP *x = cr->Ilod_base();
  ST *s = CR_st(x);
  if (ST_class(s) == CLASS_CONST && TCON_ty(ST_tcon_val(s)) == MTYPE_STR) {
    TCON c = ST_tcon_val(s);
    char *p = Index_to_char_array(TCON_str_idx(c));
    p += x->Offset() + cr->Offset();
    return CR_CreateIntconst(OPCODE_make_op(OPR_INTCONST, cr->Dtyp(), MTYPE_V), 
    			     *p);
  }
  return NOHASH;
}

// combine non-zero bit tests as follows:
//	x & c1 || x & c2  -->  (x & (c1 | c2)) != 0
static
CODEREP *Combine_bits_any_nzero_test(CODEREP *cr)
{
  if (cr->Kind() != CK_OP)
    return NOHASH;
  if (cr->Opr() != OPR_CIOR && cr->Opr() != OPR_LIOR)
    return NOHASH;
  CODEREP *l = cr->Opnd(0);
  if (l->Kind() == CK_OP && l->Opr() == OPR_NE) {
    if (l->Opnd(1)->Kind() == CK_CONST && l->Opnd(1)->Const_val() == 0)
      l = l->Opnd(0);
    else if (l->Opnd(0)->Kind() == CK_CONST && l->Opnd(0)->Const_val() == 0)
      l = l->Opnd(1);
    else return NOHASH;
  }
  CODEREP *r = cr->Opnd(1);
  if (r->Kind() == CK_OP && r->Opr() == OPR_NE) {
    if (r->Opnd(1)->Kind() == CK_CONST && r->Opnd(1)->Const_val() == 0)
      r = r->Opnd(0);
    else if (r->Opnd(0)->Kind() == CK_CONST && r->Opnd(0)->Const_val() == 0)
      r = r->Opnd(1);
    else return NOHASH;
  }
  CODEREP *lvar, *rvar;
  INT64 lbitmask, rbitmask;
  if (l->Kind() == CK_OP && l->Opr() == OPR_BAND) {
    if (l->Opnd(1)->Kind() == CK_CONST) {
      lbitmask = l->Opnd(1)->Const_val();
      lvar = l->Opnd(0);
    }
    else if (l->Opnd(0)->Kind() == CK_CONST) {
      lbitmask = l->Opnd(0)->Const_val();
      lvar = l->Opnd(1);
    }
    else return NOHASH;
  }
  else {
    lvar = l;
    lbitmask = -1;
  }
  if (r->Kind() == CK_OP && r->Opr() == OPR_BAND) {
    if (r->Opnd(1)->Kind() == CK_CONST) {
      rbitmask = r->Opnd(1)->Const_val();
      rvar = r->Opnd(0);
    }
    else if (r->Opnd(0)->Kind() == CK_CONST) {
      rbitmask = r->Opnd(0)->Const_val();
      rvar = r->Opnd(1);
    }
    else return NOHASH;
  }
  else {
    rvar = r;
    rbitmask = -1;
  }
  if (lvar != rvar)
    return NOHASH;
  if (lvar->Kind() != CK_VAR && lvar->Kind() != CK_IVAR)
    return NOHASH;
  // succeeded in matching pattern
  CODEREP *newcr;
  INT nbits = MTYPE_bit_size(lvar->Dsctyp()); 
  INT64 maxbitmask;
  if (nbits >= 64)
    maxbitmask = UINT64_MAX;
  else maxbitmask = ((UINT64)1 << nbits) - 1;
  INT64 newbitmask = (lbitmask | rbitmask) & maxbitmask;
  if (newbitmask == maxbitmask)
    newcr = lvar;
  else newcr = CR_Create(OPCODE_make_op(OPR_BAND, lvar->Dtyp(), MTYPE_V), 2, 
    			 lvar,
			 CR_CreateIntconst(
			    OPCODE_make_op(OPR_INTCONST, lvar->Dtyp(), MTYPE_V),
			    newbitmask));
  SIMPNODE_DELETE(cr);
  return CR_Create(OPCODE_make_op(OPR_NE, MTYPE_I4, lvar->Dtyp()), 2, newcr,
  				  CR_CreateIntconst(
				    OPCODE_make_op(OPR_INTCONST, lvar->Dtyp(), MTYPE_V),
				    0));
}

// combine zero bit tests as follows:
//	(x & c1) == 0 && (x & c2) == 0  -->  x & (c1 | c2) == 0
static
CODEREP *Combine_bits_all_zero_test(CODEREP *cr)
{
  if (cr->Kind() != CK_OP)
    return NOHASH;
  if (cr->Opr() != OPR_CAND && cr->Opr() != OPR_LAND)
    return NOHASH;
  CODEREP *l = cr->Opnd(0);
  if (l->Kind() == CK_OP && l->Opr() == OPR_EQ) {
    if (l->Opnd(1)->Kind() == CK_CONST && l->Opnd(1)->Const_val() == 0)
      l = l->Opnd(0);
    else if (l->Opnd(0)->Kind() == CK_CONST && l->Opnd(0)->Const_val() == 0)
      l = l->Opnd(1);
    else return NOHASH;
  }
  else return NOHASH;
  CODEREP *r = cr->Opnd(1);
  if (r->Kind() == CK_OP && r->Opr() == OPR_EQ) {
    if (r->Opnd(1)->Kind() == CK_CONST && r->Opnd(1)->Const_val() == 0)
      r = r->Opnd(0);
    else if (r->Opnd(0)->Kind() == CK_CONST && r->Opnd(0)->Const_val() == 0)
      r = r->Opnd(1);
    else return NOHASH;
  }
  else return NOHASH;
  CODEREP *lvar, *rvar;
  INT64 lbitmask, rbitmask;
  if (l->Kind() == CK_OP && l->Opr() == OPR_BAND) {
    if (l->Opnd(1)->Kind() == CK_CONST) {
      lbitmask = l->Opnd(1)->Const_val();
      lvar = l->Opnd(0);
    }
    else if (l->Opnd(0)->Kind() == CK_CONST) {
      lbitmask = l->Opnd(0)->Const_val();
      lvar = l->Opnd(1);
    }
    else return NOHASH;
  }
  else {
    lvar = l;
    lbitmask = -1;
  }
  if (r->Kind() == CK_OP && r->Opr() == OPR_BAND) {
    if (r->Opnd(1)->Kind() == CK_CONST) {
      rbitmask = r->Opnd(1)->Const_val();
      rvar = r->Opnd(0);
    }
    else if (r->Opnd(0)->Kind() == CK_CONST) {
      rbitmask = r->Opnd(0)->Const_val();
      rvar = r->Opnd(1);
    }
    else return NOHASH;
  }
  else {
    rvar = r;
    rbitmask = -1;
  }
  if (lvar != rvar)
    return NOHASH;
  if (lvar->Kind() != CK_VAR && lvar->Kind() != CK_IVAR)
    return NOHASH;
  // succeeded in matching pattern
  CODEREP *newcr;
  INT nbits = MTYPE_bit_size(lvar->Dsctyp()); 
  INT64 maxbitmask;
  if (nbits >= 64)
    maxbitmask = UINT64_MAX;
  else maxbitmask = ((UINT64)1 << nbits) - 1;
  INT64 newbitmask = (lbitmask | rbitmask) & maxbitmask;
  if (newbitmask == maxbitmask)
    newcr = lvar;
  else newcr = CR_Create(OPCODE_make_op(OPR_BAND, lvar->Dtyp(), MTYPE_V), 2, 
    			 lvar,
			 CR_CreateIntconst(
			    OPCODE_make_op(OPR_INTCONST, lvar->Dtyp(), MTYPE_V),
			    newbitmask));
  SIMPNODE_DELETE(cr);
  return CR_Create(OPCODE_make_op(OPR_EQ, MTYPE_I4, lvar->Dtyp()), 2, newcr,
  				  CR_CreateIntconst(
				    OPCODE_make_op(OPR_INTCONST, lvar->Dtyp(), MTYPE_V),
				    0));
}
#endif

BOOL
FOLD::check_convert(CODEREP *cr, CODEREP **k, INT kid)
{
  if ((*k)->Kind() == CK_OP) {
    OPERATOR opr = OPCODE_operator(SIMPNODE_opcode(*k));
    if (opr == OPR_CVTL || opr == OPR_CVT) { // look past CVT one level
      BOOL found = FALSE;
      *k = CR_Simplify_Expr(*k);
      if (*k == NULL)
	*k = cr->Opnd(kid);
      else if (*k != cr->Opnd(kid))
	found = TRUE;
      cr->Set_opnd(kid, *k);
      return found;
    }
  }
  return FALSE;
}

static CODEREP *
CR_SimpCreateExp1(OPCODE opc, CODEREP *k0)
{
   CODEREP *cr;
   cr = SIMPNODE_SimplifyExp1(opc, k0);
   if (cr == NULL)
     cr = CR_Create(opc,1,k0);
   return cr;
}

static CODEREP *
CR_SimpCreateExp2(OPCODE opc, CODEREP *k0, CODEREP *k1)
{
   CODEREP *cr;
   cr = SIMPNODE_SimplifyExp2(opc, k0, k1);
   if (cr == NULL)
     cr = CR_Create(opc,2,k0,k1);
   return cr;
}

static CODEREP *
CR_SimpCreateExp3(OPCODE opc, CODEREP *k0, CODEREP *k1, CODEREP *k2)
{
   CODEREP *cr;
   cr = SIMPNODE_SimplifyExp3(opc, k0, k1, k2);
   if (cr == NULL)
     cr = CR_Create(opc,3,k0,k1,k2);
   return cr;
}

static CODEREP *
CR_SimpCreateCvtl(OPCODE op, INT16 bits, CODEREP *k0)
{
   CODEREP *cr = Alloc_stack_cr(0);
   CODEREP *retv = NULL;
   const OPERATOR oper = OPCODE_operator(op);
   if (fold_trace)
     fprintf(TFile,"CR_CreateCvtl, %s, cvtl_bits=%d\n",OPCODE_name(op),bits);
   cr->Init_op(op,1);
   cr->Set_opnd(0,k0);
   cr->Set_offset((mINT32) bits);
   retv = fold_htable->Hash_Op(cr);
   
   Is_True(retv,("CR_CreateCvtl, needs work"));
   return retv;
}

static CODEREP *
CR_SimpCreateExtract(OPCODE op, INT16 boffset, INT16 bsize, CODEREP *k0)
{
   CODEREP *cr = Alloc_stack_cr(0);
   CODEREP *retv = NULL;
   const OPERATOR oper = OPCODE_operator(op);
   if (fold_trace)
     fprintf(TFile,"CR_CreateExtract, %s, boffset=%d, bsize=%d\n",OPCODE_name(op),
	     boffset,bsize);
   cr->Init_op(op,1);
   cr->Set_opnd(0,k0);
   cr->Set_op_bit_offset(boffset);
   cr->Set_op_bit_size(bsize);
   retv = fold_htable->Hash_Op(cr);
   
   return retv;
}

static CODEREP *
CR_SimpCreateDeposit(OPCODE op, INT16 boffset, INT16 bsize, CODEREP *k0, CODEREP *k1)
{
   CODEREP *cr = Alloc_stack_cr(0);
   CODEREP *retv = NULL;
   const OPERATOR oper = OPCODE_operator(op);
   if (fold_trace)
     fprintf(TFile,"CR_CreateDeposit, %s, boffset=%d, bsize=%d\n",OPCODE_name(op),
	     boffset,bsize);
   cr->Init_op(op,2);
   cr->Set_opnd(0,k0);
   cr->Set_opnd(1,k1);
   cr->Set_op_bit_offset(boffset);
   cr->Set_op_bit_size(bsize);
   retv = fold_htable->Hash_Op(cr);
   
   return retv;
}

static CODEREP *
CR_CreateIntconst(OPCODE opc, INT64 val)
{
   CODEREP *cr = Alloc_stack_cr(0);
   if (opc == OPC_U4INTCONST || opc == OPC_I4INTCONST) {
      /* make sure that 32-bit value is sign-extended */
      UINT32 uval = val;
      INT32 sval = uval;
      val = (INT64) sval;
   }
   cr->Init_const(OPCODE_rtype(opc),val);
   return fold_htable->Hash_Const(cr);
}

static CODEREP *
CR_CreateFPconst(TCON tc)
{
   CODEREP *cr = Alloc_stack_cr(0);
   ST *st = New_Const_Sym(Enter_tcon(tc),Be_Type_Tbl(TCON_ty(tc)));
   cr->Init_rconst(TCON_ty(tc),st);
   return fold_htable->Hash_Rconst(cr);
}

#ifdef TARG_X8664
static CODEREP *
CR_CreateSIMDconst(TCON tc)
{
   CODEREP *cr = Alloc_stack_cr(0);
   ST *st = New_Const_Sym(Enter_tcon(tc),Be_Type_Tbl(TCON_ty(tc)));
   cr->Init_rconst(TCON_ty(tc),st);
   return fold_htable->Hash_Rconst(cr);
}
#endif

// given a CR, regenerate the opcode
static OPCODE
CR_opcode(CODEREP *cr)
{
   switch (cr->Kind()) {
    case CK_OP:
    case CK_IVAR:
      return cr->Op();
      // for the rest of these cases need to regenerate opcode
    case CK_LDA:
      // cr->Dsctyp() is meaningless for LDAs
      return cr->Is_flag_set(CF_LDA_LABEL) ?
		 OPCODE_make_op(OPR_LDA_LABEL, cr->Dtyp(), MTYPE_V) :
		 OPCODE_make_op(OPR_LDA, cr->Dtyp(), MTYPE_V);
    case CK_CONST:
      // cr->Dsctyp() is meaningless for INT constants
      return OPCODE_make_op(OPR_INTCONST, cr->Dtyp(), MTYPE_V);
    case CK_RCONST:
      // cr->Dsctyp() is meaningless for FP constants
      return OPCODE_make_op(OPR_CONST, cr->Dtyp(), MTYPE_V);
    case CK_VAR:
      return OPCODE_make_op(cr->Bit_field_valid() ? OPR_LDBITS : OPR_LDID,
			    cr->Dtyp(), cr->Dsctyp());
    default:
      FmtAssert(FALSE,("CRSIMP, CR_opcode, unknown kind, 0x%x",cr->Kind()));
      return OPCODE_UNKNOWN;		// to satisfy compiler
   }
}

// given a CR, generate the operator
static OPERATOR
CR_operator(CODEREP *cr)
{
   switch (cr->Kind()) {
    case CK_OP:
    case CK_IVAR:
      return cr->Opr();
    case CK_LDA:
      return cr->Is_flag_set(CF_LDA_LABEL) ? OPR_LDA_LABEL : OPR_LDA;
    case CK_CONST:
      return OPR_INTCONST;
    case CK_RCONST:
      return OPR_CONST;
    case CK_VAR:
      return cr->Bit_field_valid() ? OPR_LDBITS : OPR_LDID;
    default:
      FmtAssert(FALSE,("CRSIMP, CR_operator, unknown kind, 0x%x",cr->Kind()));
      return OPERATOR_UNKNOWN;		// to satisfy compiler
   }
}

TY_IDX
CODEREP::lod_addr_ty()
{
  switch (Kind()) {
  case CK_IVAR:
    return CR_ty(Ilod_base());
  default:
    return TY_IDX_ZERO;
  }
}

TY_IDX
CODEREP::object_ty()
{
  switch (Kind()) {
    case CK_VAR:
      if (Field_id() != 0) {
          UINT cur_field_id = 0;
          TY_IDX ty_idx = Lod_ty();
          UINT field_id = Field_id();
          FLD_HANDLE fld = FLD_get_to_field (ty_idx, field_id, cur_field_id);
          Is_True (!fld.Is_Null(), ("Invalid field id %d for type 0x%x",
                                     field_id, ty_idx));
          return FLD_type(fld);
      } else {
        return Lod_ty();
      }
    case CK_IVAR:
      if (I_field_id() != 0) {
          UINT cur_field_id = 0;
          TY_IDX ty_idx = Ilod_ty();
          UINT field_id = I_field_id();
          FLD_HANDLE fld = FLD_get_to_field (ty_idx, field_id, cur_field_id);
          Is_True (!fld.Is_Null(), ("Invalid field id %d for type 0x%x",
                                     field_id, ty_idx));
          return FLD_type(fld);
      } else {
        return Ilod_ty();
      }
      
    default:
      return TY_IDX_ZERO;
  }
}

static TY_IDX
CR_ty(CODEREP *cr)
{
   switch (cr->Kind()) {
    case CK_VAR:  // u2.isvar.isvarty.ty
      return cr->Lod_ty();
    case CK_LDA:  // u2.islda.ty
      return cr->Lda_ty();
    case CK_IVAR: // u2.isivar.base[2]
      return cr->Ilod_ty();
    case CK_OP:   // convert result type to TY, pv400094
      return MTYPE_To_TY(cr->Dsctyp());
    default:
      return TY_IDX_ZERO;
   }
}

static ST *
CR_st(CODEREP *cr)
{
   switch (cr->Kind()) {
    case CK_VAR:
      return fold_htable->Sym()->St(cr->Aux_id());
    case CK_LDA:
      return cr->Lda_base_st();
    case CK_RCONST:
      return cr->Const_id();
    case CK_OP:
#ifdef KEY
      if (cr->Opr() == OPR_PURE_CALL_OP)
        return &St_Table[cr->Call_op_aux_id()];
#endif
    case CK_CONST:
    case CK_IVAR:
    default:
      FmtAssert(FALSE,("CRSIMP, CR_st, no symbol table entry for kind 0x%x",
		       cr->Kind()));
      return NULL;
   }
}

// return kid0
// In Whirl kid0 for a ILOAD is the address calc so we translate that here.
static void CR_set_kid0(CODEREP *cr, CODEREP *k0)
{
   switch (cr->Kind()) {
    case CK_OP:			// common case
      cr->Set_opnd(0,k0);
      break;
    case CK_IVAR:			// OPC_PARM
      if (cr->Ilod_base())
	cr->Set_ilod_base(k0);
      if (cr->Istr_base())		// no else: both Istr and Ilod base can be set
	cr->Set_istr_base(k0);
      // else both are NULL (F90 missing parameter)
      break;
    default:
      FmtAssert(0,("FOLD::CR_set_kid0, unknown kind"));
      break;
   }
}

// return kid0
// In Whirl kid0 for a ILOAD is the address calc so we translate that here.
static CODEREP *CR_kid0(CODEREP *cr)
{
   switch (cr->Kind()) {
    case CK_OP:
      return cr->Opnd(0);
    case CK_IVAR:
      return (cr->Ilod_base() != NULL) ? cr->Ilod_base() : cr->Istr_base();
    default:
      return NULL;
   }
}

// return kid i
// In Whirl kid0 for a ILOAD is the address calc so we translate that here.
static CODEREP *CR_kid(CODEREP *cr, INT i)
{
   switch (cr->Kind()) {
    case CK_OP:
      return cr->Opnd(i);
    case CK_IVAR:
      FmtAssert(i == 0, ("CR_kid, only kid0 defined for indirect reference"));
      return (cr->Ilod_base() != NULL) ? cr->Ilod_base() : cr->Istr_base();
    default:
      return NULL;
   }
}

// return number of kids
static INT32 CR_kid_count(CODEREP *cr)
{
   switch (cr->Kind()) {
    case CK_OP:
      return cr->Kid_count();
    case CK_IVAR:			// check if the base really exists (F90 PARM kid is optional)
      return (cr->Ilod_base() != NULL || cr->Istr_base() != NULL) ? 1 : 0;
    default:
      return 0;
   }
}

// Utility procedure which does a comparison on two symbols
// sister routine to wn_simp.c:WN_Compare_Symbols
INT32 CR_Compare_Symbols(CODEREP *t1, CODEREP *t2)
{
   // this check should really be in
   // wn_simp_code.h:SIMPNODE_Simp_Compare_Trees, case LDID
   // but having it here makes for a cleaner interface
   if (t1->Kind() == CK_VAR && t2->Kind() == CK_VAR && t1 == t2)
     return 0;

   ST *s1 = SIMPNODE_st(t1);
   ST *s2 = SIMPNODE_st(t2);
   if (s1 != s2) {
     ST_IDX idx1 = ST_st_idx(s1);
     ST_IDX idx2 = ST_st_idx(s2);
     UINT s1id = ST_IDX_level(idx1);
     UINT s2id = ST_IDX_level(idx2);
     if (s1id < s2id) return -1;
     if (s1id > s2id) return 1;
     ST_IDX s1index = ST_IDX_index(idx1);
     ST_IDX s2index = ST_IDX_index(idx2);
     if (s1index < s2index) return -1;
     if (s1index > s2index) return 1;
   }

   // Fix 572985:  CR_compare_symbols() must return non-zero for
   //  difference coderep of same symbols.
   //
   if (t1->Kind() == CK_VAR && t2->Kind() == CK_VAR) {
     if (t1->Coderep_id() < t2->Coderep_id()) 
       return -1;
     else if (t1->Coderep_id() > t2->Coderep_id()) 
       return 1;
   }
   return 0;
}

// Utility procedure which does a comparison on two trees
// sister routine to :SIMPNODE_Simp_Compare_Trees
INT32 CR_Compare_Trees(CODEREP *t1, CODEREP *t2)
{
  // for now, do it only if both tree are terminal
  if (t1->Non_leaf() || t2->Non_leaf()) return 0;
  if (t1->Kind() == t2->Kind() &&
      t1->Kind() == CK_VAR) {
    if (SIMPNODE_load_offset(t1) < SIMPNODE_load_offset(t2)) return(-1);
    if (SIMPNODE_load_offset(t1) > SIMPNODE_load_offset(t2)) return(1);
    return SIMPNODE_Compare_Symbols(t1,t2);
  }
  else if (inCODEKIND(t1->Kind(), CK_VAR | CK_LDA | CK_RCONST) &&
           inCODEKIND(t2->Kind(), CK_VAR | CK_LDA | CK_RCONST)) {
    return SIMPNODE_Compare_Symbols(t1,t2);
  }
  return 0;
}

/************  The code is here *******************/

#include "wn_simp_code.h"

/**************************************************/

// This has to be declared after wn_simp_code.h because trace_rules
// and trace_trees are declared there.
static void CR_Simplify_Initialize( void )
{
   trace_rules = trace_trees = fold_trace;
   SIMPNODE_simp_initialized = TRUE;
}
