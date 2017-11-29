/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

//-*-c++-*-

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
//
// Module: opt_canon.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_canon.cxx,v $
//
// Revision history:
//  3-MAY-95 dahl - Original Version
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
//	1) Canonicalize and simplify arithmetic expression
//
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#ifdef _KEEP_RCS_ID
#define opt_canon_CXX	"opt_canon.cxx"
static char *rcs_id = 	opt_canon_CXX"$Revision: 1.8 $";
#endif /* _KEEP_RCS_ID */

#include "defs.h"
#include "erglob.h"
#include "opcode.h"
#include "errors.h"
#include "tracing.h"		// for TFile
#include "mtypes.h"
#include "config_targ.h"
#include "cxx_memory.h"
#include "wn_util.h"

#include "opt_config.h"
#include "opt_wn.h"
#include "opt_util.h"
#include "opt_cfg.h"
#include "opt_sym.h"
#include "opt_htable.h"
#include "opt_fold.h"
#include "opt_ssa.h"
#include "opt_main.h"

CODEREP*
CANON_CR::Convert2cr(WN *wn, CODEMAP *htable, BOOL foldit) const
{
  const OPERATOR opr = WN_operator(wn);
  CODEREP *cr;
  MTYPE typ = WN_rtype(wn);

  if (typ == MTYPE_V) 
    typ = WN_desc(wn);
#ifdef TARG_X8664
  else if (WN_opcode(wn) == OPC_U8U4CVT ||
           WN_opcode(wn) == OPC_U8U4LDID || // bug 13382
	   WN_opcode(wn) == OPC_U8U4ILOAD)
    typ = MTYPE_U4; // U8U4CVT is deleted in Canon_cvt; otherwise will 
    		       // generate U8ADD instead of U4ADD which is wrong
#endif
  cr = Convert2cr(typ,opr,WN_opcode(wn),htable,foldit);
  return cr;
}

CODEREP*
CANON_CR::Convert2cr(MTYPE typ, OPERATOR opr, OPCODE opc, CODEMAP *htable, BOOL foldit) const
{
  CODEREP *cr;
   
  if (Tree() && Scale() != 0) {
    cr = htable->Add_bin_node_and_fold
      (OPCODE_make_op(OPR_ADD, typ, MTYPE_V),
#if defined(TARG_SL) || defined(TARG_NVISA)
       Tree(), htable->Add_const(typ, Scale()));
#else
       Tree(), htable->Add_const(MTYPE_I8, Scale()));
#endif

    return cr;
  }

  if (Tree())
    return Tree();

  // return a CK_CONST node
  // PathScale forced a I8 type here,
  // but we want smaller type when don't have native I8,
  // and this is what original code had.
#if defined(TARG_SL) || defined(TARG_NVISA)
  cr =  htable->Add_const(typ, Scale());
#else
  cr =  htable->Add_const(MTYPE_I8, Scale());
#endif

  return cr;
}

void
CANON_CR::Trim_to_16bits(WN *wn, CODEMAP *htable)
{
  const OPCODE op = WN_opcode(wn);
  MTYPE typ;
  INT64 multiple32K;

  if (Scale() >= (- 0x8000) && Scale() <= 0x7fff)
    return;

  typ = OPCODE_desc(op);
  if (typ == MTYPE_V) 
    typ = OPCODE_rtype(op);
  if (MTYPE_size_min(typ) < 32)
    typ = OPCODE_rtype(op);

  if (Tree() == NULL) {
    // iload of a constant cannot be canonicalized. Very likely
    // doing some memory mapped I/O.
    multiple32K = ((Scale() + 0x8000) >> 16) << 16;
    Set_tree(htable->Add_const(typ, multiple32K));
    Set_scale(Scale()-multiple32K);
    return;
  }
  
  // make multiple32K one of the values: -96K, -32K, 32K, 96K, 160K, etc
  multiple32K = ((Scale() + 0x8000) >> 16) << 16;
  Set_scale(Scale() - multiple32K);

  Set_tree(htable->Add_bin_node(OPCODE_make_op(OPR_ADD, typ, MTYPE_V),
				Tree(), htable->Add_const(typ, multiple32K)));
}

void
CANON_CR::Print(FILE *fp) const
{
  if (Tree()) {
    fprintf(fp,"Tree:\n");
    Tree()->Print(0,fp);
  } else
    fprintf(fp,"Tree: NULL\n");
  fprintf(fp,"scale=%lld\n",Scale());
}




//  Canonicalization after the copy propagation phase.
//
//   The canonical form is  _sign * _nonconst + constval
//   It can canonicalize expressions with +,-,* operators.
//   
//   It should be called for all the kids of iload/istore/operators
//   not equal to +,-,*, and the rhs of all statements,
//   after copy propagation changed them.
//
class CANON_EXPR {
private:
  CODEREP  *_expr;     // original expression tree

  //  the canonicalized representation:
  //     expr ==  _sign * _nonconst + constval
  //
  INT32    _sign;       // either +1 or -1
  CODEREP  *_nonconst;  // the nonconst part of the expression
  INT64    _constval;  // constant

  void Set_expr(CODEREP *cr)     { _expr = cr; }
  void Set_nonconst(CODEREP *cr) { _nonconst = cr; }
  void Set_constval(INT64 value) { _constval = value; }
  void Set_sign(INT32 sign)      { _sign = sign; }

public:
  CANON_EXPR(void)          { _expr = NULL; _nonconst = NULL; _constval = 0; _sign = 1; }
  CANON_EXPR(CODEREP *cr)   { _expr = cr;   _nonconst = NULL; _constval = 0; _sign = 1; }
  ~CANON_EXPR(void)         { }

  CODEREP *Expr(void) const      { return _expr; }
  CODEREP *Nonconst(void) const  { return _nonconst; }
  INT64    Constval(void) const  { return _constval; }
  INT32    Sign(void) const      { Is_True( _sign == 1 || _sign == -1, ("invalid value."));
                                   return _sign; }

  BOOL Canonicalized(void) const { return (_expr != _nonconst) || (_sign != 1) || _constval != 0; }
  BOOL Is_const(void) const      { return (_nonconst == NULL && _sign == 1); }
  BOOL Trivial(void) const;
  BOOL Type_safe(CODEREP *) const;

  void Canon_expr(CODEREP *cr, CODEMAP *htable);
};



// Returns TRUE if the canonicalization is a trivial canonicalization,
// i.e., the CODEREP constructed from the canonical form is the same as
// the original one.
BOOL
CANON_EXPR::Trivial(void) const
{
  if (Expr()->Kind() == CK_CONST)
    return TRUE;
  if (Expr()->Kind() == CK_OP) {
    OPERATOR opr = OPCODE_operator(Expr()->Op());
    if (opr == OPR_ADD) {
      if (Expr()->Opnd(0) == Nonconst() && Sign() > 0 &&
	  Expr()->Opnd(1)->Kind() == CK_CONST &&
	  Expr()->Opnd(1)->Const_val() == Constval())
	return TRUE;
      if (Expr()->Opnd(1) == Nonconst() && Sign() > 0 &&
	  Expr()->Opnd(0)->Kind() == CK_CONST && 
	  Expr()->Opnd(0)->Const_val() == Constval())
	return TRUE;
    } else if (opr == OPR_SUB) {
      if (Expr()->Opnd(0) == Nonconst() && Sign() > 0 &&
	  Expr()->Opnd(1)->Kind() == CK_CONST &&
	  Expr()->Opnd(1)->Const_val() == - Constval())
	return TRUE;
      if (Expr()->Opnd(1) == Nonconst() && Sign() < 0 &&
	  Expr()->Opnd(0)->Kind() == CK_CONST &&
	  Expr()->Opnd(0)->Const_val() == Constval())
	return TRUE;
    } else if (opr == OPR_NEG) {
      if (Expr()->Opnd(0) == Nonconst() && Sign() < 0 &&
	  Constval() == 0)
	return TRUE;
    }
  }
  return FALSE;
}


//  Return TRUE if there will be no need to insert CVT
//  after canonicalization
BOOL
CANON_EXPR::Type_safe(CODEREP *parent) const
{
  if (Nonconst() != NULL && 
      Nonconst()->Kind() == CK_VAR &&
      Expr()->Kind() == CK_OP) {
    MTYPE type1 = OPCODE_rtype(parent->Op());
    MTYPE type2 = OPCODE_rtype(Expr()->Op());
    if (MTYPE_size_min(type1) == MTYPE_size_min(type2))
      return TRUE;
    else
      return FALSE;
  } else 
    return TRUE;
}



//  Convert a linear expression into canonical form.
//   
//    Linear expression is of the from:
//        expr -->  expr + expr
//        expr -->  expr - expr
//        expr -->  expr * expr
//        expr -->  - expr
//        expr -->  CK_CONST
//        expr -->  terminal
//        terminal -->  CK_RCONST | CK_LDA | CK_VAR | CK_IVAR
//
void 
CANON_EXPR::Canon_expr(CODEREP *cr, CODEMAP *htable)
{
  Set_expr(cr);
  switch (cr->Kind()) {
  case CK_RCONST:
  case CK_LDA:
  case CK_VAR:
  case CK_IVAR:
    // no need to canonicalize
    goto no_canon;

  case CK_CONST:
    // the type of the constant is maintained in the opcode.
    Set_nonconst(NULL);
    Set_constval(cr->Const_val());
    Set_sign(1);
    return;

  case CK_OP:
    {
      if (cr->Is_isop_flag_set(ISOP_CANON_VISITED)) // save compile time
	goto no_canon;

      OPCODE opc = cr->Op();
      OPERATOR opr = OPCODE_operator(opc);
      CANON_EXPR opnd0, opnd1;
      switch (opr) {

      case OPR_NEG:
	opnd0.Canon_expr(cr->Opnd(0), htable);
	if (!opnd0.Type_safe(cr))
	  goto no_canon;
	Set_constval( -opnd0.Constval());
	Set_nonconst( opnd0.Nonconst() );
	Set_sign( opnd0.Sign() * -1 );
	return;

      case OPR_CVT:
	if (opc == OPC_U8I4CVT) {
	  opnd0.Canon_expr(cr->Opnd(0), htable);
	  if (!opnd0.Canonicalized())
	    goto no_canon;
	  if (opnd0.Nonconst() == NULL) {
	    Set_nonconst( NULL );
	    Set_constval( opnd0.Constval() );
	    Set_sign( opnd0.Sign() );
	  }
	  else {
	    Set_nonconst( htable->Add_unary_node(opc, opnd0.Nonconst()));
	    Set_constval ( opnd0.Constval());
	    Set_sign( opnd0.Sign ());
	  }
	  return;
	}
	goto no_canon;

      case OPR_ADD:
	opnd0.Canon_expr(cr->Opnd(0), htable);
	opnd1.Canon_expr(cr->Opnd(1), htable);
	if (!opnd0.Canonicalized() && !opnd1.Canonicalized()) 
	  goto no_canon;
	if (!opnd0.Type_safe(cr) || !opnd1.Type_safe(cr))
	  goto no_canon;

	Set_constval( opnd0.Constval() + opnd1.Constval() );
	if (opnd0.Nonconst() == NULL) {
	  Set_sign( opnd1.Sign() );
	  Set_nonconst( opnd1.Nonconst() );
	} else if (opnd1.Nonconst() == NULL) {
	  Set_sign( opnd0.Sign() );
	  Set_nonconst( opnd0.Nonconst() );
	} else {
	  //  a + b    ==>   1 * (a + b)
          //  a + -b   ==>   1 * (a - b)
          // -a + b    ==>  -1 * (a - b)
	  // -a + -b   ==>  -1 * (a + b)
	  Set_sign( opnd0.Sign() );
	  if (opnd0.Sign() > 0) {
	    if (opnd1.Sign() > 0) 
	      Set_nonconst( htable->Add_bin_node(opc, opnd0.Nonconst(), opnd1.Nonconst()));
	    else {
	      OPCODE subop = OPCODE_make_op(OPR_SUB, OPCODE_rtype(opc), MTYPE_V);
	      Set_nonconst( htable->Add_bin_node(subop, opnd0.Nonconst(), opnd1.Nonconst()));
	    }
	  } else {
	    if (opnd1.Sign() > 0) {
	      OPCODE subop = OPCODE_make_op(OPR_SUB, OPCODE_rtype(opc), MTYPE_V);
	      Set_nonconst( htable->Add_bin_node(subop, opnd0.Nonconst(), opnd1.Nonconst()));
	    } else 
	      Set_nonconst( htable->Add_bin_node(opc, opnd0.Nonconst(), opnd1.Nonconst()));
	  }
	}
	return;

      case OPR_SUB:
	opnd0.Canon_expr(cr->Opnd(0), htable);
	opnd1.Canon_expr(cr->Opnd(1), htable);
	if (!opnd0.Canonicalized() && !opnd1.Canonicalized()) 
	  goto no_canon;
	if (!opnd0.Type_safe(cr) || !opnd1.Type_safe(cr))
	  goto no_canon;

	Set_constval( opnd0.Constval() - opnd1.Constval() );
	if (opnd0.Nonconst() == NULL) {
	  Set_sign( opnd1.Sign() * -1);
	  Set_nonconst( opnd1.Nonconst() );
	} else if (opnd1.Nonconst() == NULL) {
	  Set_sign( opnd0.Sign() );
	  Set_nonconst( opnd0.Nonconst() );
	} else {
	  Set_sign( opnd0.Sign() );
	  //  a - b 	==>  1 * (a - b)
          //  a - -b    ==>  1 * (a + b)
          // -a - b     ==> -1 * (a + b)
 	  // -a - -b    ==> -1 * (a - b)
	  if (opnd0.Sign() > 0) {
	    if (opnd1.Sign() > 0) 
	      Set_nonconst( htable->Add_bin_node(opc, opnd0.Nonconst(), opnd1.Nonconst()));
	    else {
	      OPCODE addop = OPCODE_make_op(OPR_ADD, OPCODE_rtype(opc), MTYPE_V);
	      Set_nonconst( htable->Add_bin_node(addop, opnd0.Nonconst(), opnd1.Nonconst()));
	    }
	  } else {
	    if (opnd1.Sign() > 0) {
	      OPCODE addop = OPCODE_make_op(OPR_ADD, OPCODE_rtype(opc), MTYPE_V);
	      Set_nonconst( htable->Add_bin_node(addop, opnd0.Nonconst(), opnd1.Nonconst()));
	    } else
	      Set_nonconst( htable->Add_bin_node(opc, opnd0.Nonconst(), opnd1.Nonconst()));
	  }
	}
	return;

      case OPR_MPY:
	opnd0.Canon_expr(cr->Opnd(0), htable);
	opnd1.Canon_expr(cr->Opnd(1), htable);
	if (!opnd0.Canonicalized() && !opnd1.Canonicalized()) 
	  goto no_canon;
	if (!opnd0.Type_safe(cr) || !opnd1.Type_safe(cr))
	  goto no_canon;

	{
	  CANON_EXPR *c = NULL;
	  CANON_EXPR *x = NULL;
	  if (opnd0.Is_const() && opnd1.Canonicalized()) {
	    c = &opnd0;
	    x = &opnd1;
	  } else if (opnd1.Is_const() && opnd0.Canonicalized()) {
	    c = &opnd1;
	    x = &opnd0;
	  } else
	    goto no_canon;

	  if (x->Nonconst() == NULL) {
	    // both x and c are constants
	    Set_sign(1);
	    Set_constval( c->Constval() * x->Constval());
	    Set_nonconst(NULL);
	  } else {
	    // c is a constant coderep  
	    // x is a canonicalized expr (x == sign * expr + c1)
	    //
	    // c * (sign * expr + c1) ==> (sign * (c * expr)) + (c * c1)     if (c > 0)
	    //                        ==> (-sign * (-c * expr)) + (c * c1)   if (c < 0)
	    //                        ==> 0                                  if (c == 0)
	    //
	    if (c->Constval() == 0) {
	      Set_constval(0);
	      Set_nonconst(NULL);
	      Set_sign(1);
	    } else if (c->Constval() > 0) {
	      CODEREP *cr = htable->Add_const(OPCODE_rtype(opc), c->Constval());
	      Set_nonconst( htable->Add_bin_node( opc, x->Nonconst(), cr));
	      Set_constval( c->Constval() * x->Constval());
	      Set_sign(x->Sign());
	    } else {
	      CODEREP *cr = htable->Add_const(OPCODE_rtype(opc), - c->Constval());
	      Set_nonconst( htable->Add_bin_node( opc, x->Nonconst(), cr));
	      Set_constval( c->Constval() * x->Constval());
	      Set_sign(x->Sign() * -1);
	    }
	  }
	}
	return;

      default:
	goto no_canon;
      } // switch (opr)
    } // CK_OP
    
  default:
    Is_True(FALSE, ("unknown CK_KIND."));
  } // switch (cr->Kind())
  
no_canon:
  if (cr->Kind() == CK_OP)
    cr->Set_isop_flag(ISOP_CANON_VISITED);
  Set_nonconst(cr);
  Set_sign(1);
  Set_constval(0);
  return;
}


// Return non-null CODEREP 
//
CODEREP *
CODEMAP::Canon_base(CODEREP *base, INT64 *ofst)
{
  if (WOPT_Enable_Canon_Expr) {
    CANON_EXPR expr;
    expr.Canon_expr(base, this);
    if (expr.Canonicalized() && expr.Constval() != 0) {
      INT64 constval = *ofst + expr.Constval();
      CODEREP *newbase = expr.Nonconst();
      if (expr.Nonconst() == NULL) 
	newbase = Add_const(Pointer_type, 0);
      if (expr.Sign() < 0) {
	if (newbase->Kind() == CK_OP && OPCODE_operator(newbase->Op()) == OPR_SUB)
	  newbase = Add_bin_node(newbase->Op(), newbase->Opnd(1), newbase->Opnd(0));
	else {
	  OPCODE opc = OPCODE_make_op(OPR_NEG, Pointer_type, MTYPE_V);
	  newbase = Add_unary_node(opc, newbase);
	}
      }
      // Trim to 16 bit for wopt
      if (_phase == MAINOPT_PHASE && (constval < -0x8000 || constval > 0x7fff)
	  || (((constval >> 31) + 1) >> 1) != 0 /* not fits in 32 bits */ ) {
	// make multiple32K one of the values: -96K, -32K, 32K, 96K, 160K, etc
	INT64 multiple32K = ((constval + 0x8000) >> 16) << 16;
	if (multiple32K > 0) {
	  OPCODE opc = OPCODE_make_op(OPR_ADD, Pointer_type, MTYPE_V);
	  newbase = Add_bin_node(opc, newbase, Add_const(Pointer_type, multiple32K));
	} else {
	  OPCODE opc = OPCODE_make_op(OPR_SUB, Pointer_type, MTYPE_V);
	  newbase = Add_bin_node(opc, newbase, Add_const(Pointer_type, -multiple32K));
	}
	constval =  constval - multiple32K;
      }
      if (Get_Trace(TP_GLOBOPT, PROP_DUMP_FLAG)) {
	fprintf(TFile, "Before Canon_base:\n");
	base->Print(0,TFile);
	fprintf(TFile, "After Canon_base:\n");
	newbase->Print(0,TFile);
	fprintf(TFile, "\n");
      }
      *ofst = constval;  // update offset
      return newbase;
    }
  }
  return NULL;
}


CODEREP *
CODEMAP::Canon_rhs(CODEREP *rhs)
{
  if (WOPT_Enable_Canon_Expr) {
    CANON_EXPR expr;
    expr.Canon_expr(rhs, this);
    if (expr.Canonicalized() && !expr.Trivial()) {
      CODEREP *newrhs = expr.Nonconst();
      MTYPE rtype = rhs->Kind() == CK_OP ? OPCODE_rtype(rhs->Op()) : rhs->Dtyp();
      if (expr.Nonconst() == NULL) 
	newrhs = Add_const(rtype, expr.Constval());
      else {
	if (expr.Sign() < 0) {
	  if (newrhs->Kind() == CK_OP && OPCODE_operator(newrhs->Op()) == OPR_SUB) 
	    newrhs = Add_bin_node(newrhs->Op(), newrhs->Opnd(1), newrhs->Opnd(0));
	  else {
	    OPCODE opc = OPCODE_make_op(OPR_NEG, rtype, MTYPE_V);
	    newrhs = Add_unary_node(opc, newrhs);
	  }
	} 
	if (expr.Constval() != 0) {
	  if (expr.Constval() > 0) {
	    OPCODE opc = OPCODE_make_op(OPR_ADD, rtype, MTYPE_V);
	    newrhs = Add_bin_node(opc, newrhs, Add_const(rtype, expr.Constval()));
	  } else {
	    OPCODE opc = OPCODE_make_op(OPR_SUB, rtype, MTYPE_V);
	    newrhs = Add_bin_node(opc, newrhs, Add_const(rtype, -expr.Constval()));
	  }
	}
      }
      if (Get_Trace(TP_GLOBOPT, PROP_DUMP_FLAG)) {
	fprintf(TFile, "Before Canon_rhs:\n");
	rhs->Print(0,TFile);
	fprintf(TFile, "After Canon_rhs:\n");
	newrhs->Print(0,TFile);
	fprintf(TFile, "\n");
      }
      return newrhs;
    } 
  }
  return NULL;
}


// Canonicalize comparison into the form into
//  1)   iv compare_op invar
//  2)   iv * invar compare_op invar
//  3)   invar * iv compare_op invar
//  4)   invar compare_op iv
//  5)   invar compare_op iv * invar
//  6)   invar compare_op invar * iv
//
// It is nice to have just form 1 and 2.  However, the simplifier is
// free to rearrange the comparion into forms 3-6.
//
//  If the final form is 2, 3, 5 or 6.  Undo the canonicalization
//  because strength reduction may introduce extra variable.
//
CODEREP *
CODEMAP::Separate_iv_invar(CODEREP *cr, BB_NODE *curbb)
{
  if (WOPT_Enable_Compare_Simp && 
      OPCODE_is_compare(cr->Op()) &&
      MTYPE_is_integral(OPCODE_desc(cr->Op())) &&
      (OPCODE_operator(cr->Op()) == OPR_NE ||
       OPCODE_operator(cr->Op()) == OPR_EQ)) {
    CODEREP *iv = NULL;
    CODEREP *invar = NULL;
    BOOL swapped = FALSE;
    if (curbb->Innermost()->Invariant_cr(cr->Opnd(1))) {
      iv = cr->Opnd(0);
      invar = cr->Opnd(1);
    } else if (curbb->Innermost()->Invariant_cr(cr->Opnd(0))) {
      iv = cr->Opnd(1);
      invar = cr->Opnd(0);
      swapped = TRUE;
    }
    if (invar != NULL && !curbb->Innermost()->Invariant_cr(iv)) {
      BOOL cont = TRUE;
      while (iv->Kind() == CK_OP && cont) {
	switch (OPCODE_operator(iv->Op())) {
	case OPR_NEG:
	  invar = Add_unary_node(iv->Op(), invar);
	  iv = iv->Opnd(0);
	  swapped = !swapped;  // toggle
	  break;
	case OPR_ADD:
	  {
	    OPCODE subop = OPCODE_make_op(OPR_SUB, OPCODE_rtype(iv->Op()), MTYPE_V);
	    if (curbb->Innermost()->Invariant_cr(iv->Opnd(0))) {
#ifdef KEY // bug 4959 wraparound will cause problem
	      if (OPCODE_rtype(subop) == MTYPE_U4 &&
		  invar->Kind() == CK_CONST && iv->Opnd(0)->Kind() ==CK_CONST &&
		  invar->Const_val() < iv->Opnd(0)->Const_val()) {
	        cont = FALSE;
		break;
	      }
#endif
	      invar = Add_bin_node_and_fold(subop, invar, iv->Opnd(0));
	      iv = iv->Opnd(1);
	    } else {
#ifdef KEY // bug 4959 wraparound will cause problem
	      if (OPCODE_rtype(subop) == MTYPE_U4 &&
		  invar->Kind() == CK_CONST && iv->Opnd(1)->Kind() ==CK_CONST &&
		  invar->Const_val() < iv->Opnd(1)->Const_val()) {
	        cont = FALSE;
		break;
	      }
#endif
	      invar = Add_bin_node_and_fold(subop, invar, iv->Opnd(1));
	      iv = iv->Opnd(0);
	    }
	  }
	  break;
	case OPR_SUB:
	  if (curbb->Innermost()->Invariant_cr(iv->Opnd(0))) {
	    invar = Add_bin_node_and_fold(iv->Op(), iv->Opnd(0), invar);
	    iv = iv->Opnd(1);
	    swapped = !swapped; // toggle
	  } else {
	    OPCODE addop = OPCODE_make_op(OPR_ADD, OPCODE_rtype(iv->Op()), MTYPE_V);
	    invar = Add_bin_node_and_fold(addop, invar, iv->Opnd(1));
	    iv = iv->Opnd(0);
	  }
	  break;
	default:
	  cont = FALSE;
	  break;
	}
      }
      if (iv->Kind() != CK_VAR)
	return NULL;

      // Do not apply canon_cmp to any var other primary IV.
      CODEREP *primary_iv = curbb->Innermost()->Iv();
      if (primary_iv == NULL ||
	  primary_iv->Kind() != CK_VAR ||
	  iv->Aux_id() != primary_iv->Aux_id())
	return NULL;

      if (!swapped) {
	if (cr->Opnd(0) != iv) {
	  cr->Set_opnd(0, iv);
	  cr->Set_opnd(1, invar);
	  return cr;
	}
      } else {
	if (cr->Opnd(1) != iv) {
	  cr->Set_opnd(0, invar);
	  cr->Set_opnd(1, iv);
	  return cr;
	}
      }
    }
  }
  return NULL;
}

// This routine perform canonicalization on comparison expressions 'cr',
// and returns TRUE if this expression is canonicalized.
BOOL
CODEMAP::Canonicalize_compare(CODEREP *cr, BB_NODE *curbb, BOOL *modified)
{
  OPCODE op = cr->Op();
  if (!OPCODE_is_compare(op)) return FALSE;

  // Separate invariant and variant to either side of the operator
  *modified = Separate_iv_invar(cr, curbb) != NULL;
    
  // Swap the kids of the compare opcode
  //   if curbb inside loop,
  //      the LHS is a loop variant and RHS is a loop invariant
  //   otherwise
  //      move constant to the RHS
  const BB_LOOP *loop = Cfg()->Find_innermost_loop_contains( curbb );
  CODEREP *lhs = cr->Get_opnd(0), *rhs = cr->Get_opnd(1);
  BOOL swapped = FALSE;
  if (loop) {	// the comparison is inside a loop
    if (!loop->Invariant_cr(lhs)) return TRUE; // canonicalized already
    else if (loop->Invariant_cr(rhs)) return FALSE; // nothing can be
                                                     // done
    else {
      cr->Set_opnd(0, rhs);
      cr->Set_opnd(1, lhs);
      swapped = TRUE;
      *modified = TRUE;
    }
  }
  else {
    if (lhs->Kind() == CK_VAR) {
      if (rhs->Kind() == CK_VAR)
        return FALSE; // cannot not do better, leave it as default
      else
        return TRUE;  // make sure the CK_VAR stay on the left
    }
    else {
      // might need swapping
      if (rhs->Kind() == CK_VAR){
        cr->Set_opnd(0, rhs);
        cr->Set_opnd(1, lhs);
        swapped = TRUE;
        *modified = TRUE;
      }
      else
        return FALSE;
    }
  }

  if (swapped)
    switch (OPCODE_operator(op)) {
    case OPR_LT:
      cr->Set_opr(OPR_GT);
      break;
    case OPR_LE:
      cr->Set_opr(OPR_GE);
      break;
    case OPR_GT:
      cr->Set_opr(OPR_LT);
      break;
    case OPR_GE:
      cr->Set_opr(OPR_LE);
      break;
    }
  return TRUE;
}
