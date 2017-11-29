//-*-c++-*-

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_u64_lower.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_u64_lower.cxx,v $
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
// Lowering module for unsigned 64-bit ISA working in coderep
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
#include "tracing.h"	/* for TFile */

#include "opt_defs.h"
#include "opt_cfg.h"
#include "opt_ssa.h"
#include "opt_main.h"
#include "opt_sym.h"
#include "opt_htable.h"

#define U64_LOWER_alloc_stack(x) Alloc_stack_cr(x->Extra_ptrs_used())

#include "u64_lower_template.h"

static CODEMAP *htable;

// ====================================================================
// specialized version of support routines used in the U64_LOWER template
// ==================================================================== 

inline void
U64_LOWER_delete(CODEREP *cr) {}

INT
U64_LOWER_kid_count(const CODEREP *cr) { 
  if (cr->Kind() == CK_OP)
    return cr->Kid_count(); 
  if (cr->Kind() != CK_IVAR)
    return 0;
  if (cr->Ilod_base() == NULL)
    return 0;
  return cr->Opr() == OPR_MLOAD ? 2 : 1;
}

CODEREP *
U64_LOWER_kid(const CODEREP *cr, INT i) { 
  if (cr->Kind() == CK_OP)
    return cr->Opnd(i); 
  if (cr->Kind() != CK_IVAR)
    return NULL;
  return i == 0 ? cr->Ilod_base() : cr->Mload_size();
}

void
U64_LOWER_set_kid(CODEREP *cr, INT i, CODEREP *x) { 
  if (cr->Kind() == CK_OP)
    cr->Set_opnd(i, x); 
  else if (cr->Kind() == CK_IVAR) {
    if (i == 0)
      cr->Set_ilod_base(x);
    else cr->Set_mload_size(x);
  }
}

inline CODEREP *
U64_LOWER_kid0(const CODEREP *cr) { return U64_LOWER_kid(cr, 0); }

inline void
U64_LOWER_set_kid0(CODEREP *cr, CODEREP *x) { U64_LOWER_set_kid(cr, 0, x); }

inline CODEREP *
U64_LOWER_kid1(const CODEREP *cr) { return U64_LOWER_kid(cr, 1); }

inline void
U64_LOWER_set_kid1(CODEREP *cr, CODEREP *x) { U64_LOWER_set_kid(cr, 1, x); }

inline CODEREP *
U64_LOWER_kid2(const CODEREP *cr) { return U64_LOWER_kid(cr, 2); }

inline void
U64_LOWER_set_kid2(CODEREP *cr, CODEREP *x) { U64_LOWER_set_kid(cr, 2, x); }

OPERATOR
U64_LOWER_operator(const CODEREP *cr) { 
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
    }
    return OPERATOR_UNKNOWN;
}

inline TYPE_ID
U64_LOWER_rtype(const CODEREP *cr) { return cr->Dtyp(); }

inline void
U64_LOWER_set_rtype(CODEREP *cr, TYPE_ID t) { 
  if (! inCODEKIND(cr->Kind(), CK_CONST | CK_VAR | CK_IVAR))
    cr->Set_dtyp_strictly(t); 
}

inline TYPE_ID
U64_LOWER_desc(const CODEREP *cr) { return cr->Dsctyp(); }

inline void
U64_LOWER_set_desc(CODEREP *cr, TYPE_ID t) { 
  if (! inCODEKIND(cr->Kind(), CK_CONST | CK_VAR | CK_IVAR))
    cr->Set_dsctyp(t); 
}

inline INT
U64_LOWER_cvtl_bits(const CODEREP *cr) { return cr->Offset(); }

inline void
U64_LOWER_set_cvtl_bits(CODEREP *cr, INT i) { cr->Set_offset(i); }

inline INT16
U64_LOWER_bit_size(const CODEREP *cr) 
{ 
  if (cr->Kind() == CK_OP) 
    return cr->Op_bit_size(); 
  else if (cr->Kind() == CK_IVAR) 
    return cr->I_bit_size(); 
  else return cr->Bit_size(); 
}

inline INT16
U64_LOWER_bit_offset(const CODEREP *cr) 
{ 
  if (cr->Kind() == CK_OP) 
    return cr->Op_bit_offset(); 
  else if (cr->Kind() == CK_IVAR) 
    return cr->I_bit_offset(); 
  else return cr->Bit_offset(); 
}

inline INT64
U64_LOWER_const_val(const CODEREP *cr) { return cr->Const_val(); }

inline void
U64_LOWER_set_const_val(CODEREP *cr, INT64 i) { cr->Set_const_val(i); }

inline ST_CLASS
U64_LOWER_class(const CODEREP *cr) { 
  return ST_class(htable->Sym()->Aux_stab_entry(cr->Aux_id())->St()); }

inline void
U64_LOWER_copy_node(CODEREP *newcr, CODEREP *oldcr) { newcr->Copy(*oldcr); }

CODEREP *
U64_LOWER_form_node(CODEREP *cr, CODEREP *old_cr) { 
  // if any operand in old_cr has been changed, cannot re-use old_cr
  // (cannot just call Match because does not need to check mu/chi
  BOOL changed = FALSE;
  switch (old_cr->Kind()) {
  case CK_CONST: case CK_RCONST: case CK_LDA: case CK_VAR:
    return old_cr;
  case CK_IVAR:
    if (! OPERATOR_is_scalar_iload(old_cr->Opr()) && old_cr->Opr() != OPR_PARM)
      break;
    if (cr->Ilod_base() != old_cr->Ilod_base())
      changed = TRUE;
    if (cr->Opr() == OPR_MLOAD)
      if (cr->Mload_size() != old_cr->Mload_size())
        changed = TRUE;
    break;
  case CK_OP:
    if ((cr->Dtyp() != old_cr->Dtyp()) || (cr->Dsctyp() != old_cr->Dsctyp())) {
        changed = TRUE;
	break;
    }
    for (INT i = 0; i < old_cr->Kid_count(); i++)
      if (cr->Opnd(i) != old_cr->Opnd(i)) {
        changed = TRUE;
	break;
      }
    break;
  }
  if (changed) {
    old_cr->DecUsecnt();
    return htable->Rehash(cr); 
  }
  return old_cr;
}

CODEREP *
U64_LOWER_create_cvtl(TYPE_ID res, CODEREP *x, INT cvtl_len) {
  CODEREP *stack_cr = Alloc_stack_cr(0); 
  stack_cr->Init_expr(OPCODE_make_op(OPR_CVTL, res, MTYPE_V), x);
  stack_cr->Set_offset(cvtl_len);
  return htable->Rehash(stack_cr);
}

CODEREP *
U64_LOWER_create_ne_0(TYPE_ID res, TYPE_ID desc, CODEREP *x) {
  CODEREP *stack_cr = Alloc_stack_cr(1); 
  stack_cr->Init_expr(OPCODE_make_op(OPR_NE, res, desc), x);
  stack_cr->Set_opnd(1, htable->Add_const(desc, 0));
  return htable->Rehash(stack_cr);
}

/* ====================================================================
 * Insert a CVTL that has been delayed. hob_to_do gives the target signedness.
 * The CVTL is on Kid kidno of tree with size cvtl_bits.
 * hob_state provides hob info so the CVTL can be omitted if necessary.
 * Resulting hob info is returned in hob_state.
 * ==================================================================== */
void 
U64_LOWER_insert_cvtl_for_kid(CODEREP *x, HIGH_ORDER_BITS hob_to_do, INT kidno,
			      INT cvtl_bits, HIGH_ORDER_BITS &hob_state)
{
  if (cvtl_bits == 64)
    return;
  if (cvtl_bits == 0)
    return;
  if (hob_to_do == HOB_none)
    return;
  if (hob_to_do == hob_state)
    return;

  CODEREP *cr = U64_LOWER_create_cvtl(
			    hob_to_do == HOB_sign_xtd ? MTYPE_I8 : MTYPE_U8,
			    U64_LOWER_kid(x, kidno), cvtl_bits);
  U64_LOWER_set_kid(x, kidno, cr);
  hob_state = hob_to_do;
}

void
U64_LOWER_insert_cvtl_for_kid(STMTREP *s, HIGH_ORDER_BITS hob_to_do, INT kidno,
                              INT cvtl_bits, HIGH_ORDER_BITS &hob_state)
{
  if (cvtl_bits == 64)
    return;
  if (cvtl_bits == 0)
    return;
  if (hob_to_do == HOB_none)
    return;
  if (hob_to_do == hob_state)
    return;

  CODEREP *opnd;
  if (kidno == 0)
    opnd = s->Rhs();
  else if (kidno == 1)
    opnd = s->Lhs()->Istr_base();
  else opnd = s->Lhs()->Mstore_size();

  CODEREP *cr = U64_LOWER_create_cvtl(
			    hob_to_do == HOB_sign_xtd ? MTYPE_I8 : MTYPE_U8,
			    opnd, cvtl_bits);
  if (kidno == 0)
    s->Set_rhs(cr);
  else if (kidno == 1)
    s->Lhs()->Set_istr_base(cr);
  else s->Lhs()->Set_mstore_size(cr);
  hob_state = hob_to_do;
}

/* ====================================================================
 * End of specialized support routines 
 * ==================================================================== */


/* ====================================================================
 * lower the given statement to zero-extended 64-bit target ISA.
 * ==================================================================== */
void
U64_lower_stmtrep(STMTREP *s, BOOL leave_CVTL_at_leaf)
{
  INT maxsize;
  HIGH_ORDER_BITS hob_state, hob_to_do;
  OPERATOR opr = s->Opr();
  TYPE_ID desc = s->Desc();
  TYPE_ID res = s->Rtype();
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
  case OPR_BLOCK:
  case OPR_OPT_CHI:
    return;

  case OPR_DEALLOCA:	// only kid 0 contains node relevant to U64 lowering
    s->Rhs()->Set_opnd(0, U64_LOWER_expr(s->Rhs()->Opnd(0), maxsize, hob_state,
					 hob_to_do, leave_CVTL_at_leaf));
    return;

  case OPR_PREFETCH:
    s->Rhs()->Set_ilod_base(U64_LOWER_expr(s->Rhs()->Ilod_base(), maxsize, 
				hob_state, hob_to_do, leave_CVTL_at_leaf));
    return;

  case OPR_AGOTO:
    // don't need to do anything
    s->Set_rhs(U64_LOWER_expr(s->Rhs(), maxsize, 
				    hob_state, hob_to_do, leave_CVTL_at_leaf));
    return;

  case OPR_EVAL: 
    s->Set_rhs(U64_LOWER_expr(s->Rhs(), maxsize, 
				    hob_state, hob_to_do, leave_CVTL_at_leaf));
    // need to set high-order bits because may become kid of LOOP_INFO
    U64_LOWER_insert_cvtl_for_kid(s, hob_to_do, 0, maxsize, hob_state);
    return;

  case OPR_TRUEBR:
  case OPR_FALSEBR:
  case OPR_ASSERT:
    s->Set_rhs(U64_LOWER_expr(s->Rhs(), maxsize, 
				    hob_state, hob_to_do, leave_CVTL_at_leaf));
    if (maxsize != 0 && maxsize != 64) { // generate comparison with 0
      if (hob_state == HOB_none)  // enlarge Kid 0
	U64_LOWER_insert_cvtl_for_kid(s, HOB_zero_xtd, 0, maxsize, hob_state);
    }
    return;

  case OPR_ISTORE:
    s->Lhs()->Set_istr_base(U64_LOWER_expr(s->Lhs()->Istr_base(), maxsize, 
				    hob_state, hob_to_do, leave_CVTL_at_leaf));
  // fall through

  case OPR_STID:
    s->Set_rhs(U64_LOWER_expr(s->Rhs(), maxsize, 
				    hob_state, hob_to_do, leave_CVTL_at_leaf));
    if (MTYPE_bit_size(desc) > maxsize) 
      U64_LOWER_insert_cvtl_for_kid(s, hob_to_do, 0, maxsize, hob_state);
    return;

  case OPR_ISTBITS:
    s->Lhs()->Set_istr_base(U64_LOWER_expr(s->Lhs()->Istr_base(), maxsize, 
				    hob_state, hob_to_do, leave_CVTL_at_leaf));
    s->Set_rhs(U64_LOWER_expr(s->Rhs(), maxsize, 
				    hob_state, hob_to_do, leave_CVTL_at_leaf));
    if (s->Lhs()->I_bit_size() > maxsize) 
      U64_LOWER_insert_cvtl_for_kid(s, hob_to_do, 0, maxsize, hob_state);
    return;

  case OPR_STBITS:
    s->Set_rhs(U64_LOWER_expr(s->Rhs(), maxsize, 
				    hob_state, hob_to_do, leave_CVTL_at_leaf));
    if (s->Lhs()->Bit_size() > maxsize) 
      U64_LOWER_insert_cvtl_for_kid(s, hob_to_do, 0, maxsize, hob_state);
    return;

  case OPR_MSTORE:
    s->Lhs()->Set_mstore_size(U64_LOWER_expr(s->Lhs()->Mstore_size(), maxsize, 
				    hob_state, hob_to_do, leave_CVTL_at_leaf));
    U64_LOWER_insert_cvtl_for_kid(s, hob_to_do, 2, maxsize, hob_state);
    s->Lhs()->Set_istr_base(U64_LOWER_expr(s->Lhs()->Istr_base(), maxsize, 
				    hob_state, hob_to_do, leave_CVTL_at_leaf));
    s->Set_rhs(U64_LOWER_expr(s->Rhs(), maxsize, 
				    hob_state, hob_to_do, leave_CVTL_at_leaf));
    return;

  case OPR_RETURN_VAL:
    s->Set_rhs(U64_LOWER_expr(s->Rhs(), maxsize, 
				    hob_state, hob_to_do, leave_CVTL_at_leaf));
    if (MTYPE_bit_size(res) > maxsize)
      U64_LOWER_insert_cvtl_for_kid(s, hob_to_do, 0, maxsize, hob_state);
    return;

  case OPR_CALL:          
  case OPR_ICALL:
  case OPR_INTRINSIC_CALL:
    for (i = 0; i < s->Rhs()->Kid_count(); i++)
      s->Rhs()->Set_opnd(i, U64_LOWER_expr(s->Rhs()->Opnd(i), maxsize, 
				   hob_state, hob_to_do, leave_CVTL_at_leaf));
    return;

  case OPR_ASM_STMT:
    for (i = 2; i < s->Rhs()->Kid_count(); i++)
      s->Rhs()->Opnd(i)->Set_opnd(0, U64_LOWER_expr(s->Rhs()->Opnd(i)->Opnd(0),
			    maxsize, hob_state, hob_to_do, leave_CVTL_at_leaf));
    return;

  case OPR_COMPGOTO:
  case OPR_XGOTO:
    s->Set_rhs(U64_LOWER_expr(s->Rhs(), maxsize, 
				    hob_state, hob_to_do, leave_CVTL_at_leaf));
    U64_LOWER_insert_cvtl_for_kid(s, HOB_zero_xtd, 0, maxsize, hob_state);
    return;

  case OPR_XPRAGMA:
    s->Set_rhs(U64_LOWER_expr(s->Rhs(), maxsize, 
				   hob_state, hob_to_do, leave_CVTL_at_leaf));
    U64_LOWER_insert_cvtl_for_kid(s, hob_to_do, 0, maxsize, hob_state);
    return;

  default:	
    Is_True(FALSE,("unexpected operator"));
  }

  return;  
}

// ====================================================================
// Top level routine for lowering to zero-extended 64-bit target ISA
// for coderep.  The dtyp/dsctyp for CK_VAR, CK_IVAR and CK_CONST nodes
// are changed only in a post-pass.
// ==================================================================== 
void
COMP_UNIT::U64_lower_cr(BOOL leave_CVTL_at_leaf)
{
  if ( Get_Trace(TP_GLOBOPT, OPT_LOWER_FLAG)) {
    fprintf( TFile, "%sBefore COMP_UNIT::U64_lower_cr\n%s",
             DBar, DBar );
    Cfg()->Print(TFile);
  }

  htable = Htable();  // initialize the file-scope static variable

  CFG_ITER cfg_iter(Cfg());
  BB_NODE *bb;
  FOR_ALL_NODE(bb, cfg_iter, Init()) {
    STMTREP_ITER stmt_iter(bb->Stmtlist());
    STMTREP *stmt;
    FOR_ALL_NODE(stmt, stmt_iter, Init()) 
      U64_lower_stmtrep(stmt, leave_CVTL_at_leaf);
  }

  // fix the dtyp/dsctyp for CK_VAR
  AUX_ID i;
  AUX_STAB_ITER aux_stab_iter(Opt_stab());
  CODEREP *cr;
  CODEREP_ITER cr_iter;
  FOR_ALL_NODE(i, aux_stab_iter, Init()) {
    AUX_STAB_ENTRY *aux = Opt_stab()->Aux_stab_entry(i);
    if (! aux->Is_real_var()) 
      continue;
    if (! (aux->Mclass() & MTYPE_CLASS_INTEGER))
      continue;
    if (ST_class(aux->St()) == CLASS_PREG) {
      FOR_ALL_NODE(cr, cr_iter, Init(aux->Cr_list())) {
	if (cr->Dtyp() != MTYPE_B)
          cr->Set_dtyp(Mtype_TransferSize(MTYPE_A8, cr->Dtyp()));
      }
    }
    else {
      FOR_ALL_NODE(cr, cr_iter, Init(aux->Cr_list())) {
	Is_True(cr->Dtyp() != MTYPE_B,
	    ("COMP_UNIT::U64_lower_cr: non-preg var cannot have have MTYPE_B"));
        cr->Reset_sign_extd();
        if (MTYPE_signed(cr->Dsctyp())) 
	  cr->Set_dsctyp(Mtype_TransferSign(MTYPE_U8, cr->Dsctyp()));
        cr->Set_dtyp(MTYPE_U8);
      }
    }
  }

  // fix the dtyp/dsctyp for CK_IVAR and CK_CONST nodes
  CODEREP *bucket;
  CODEMAP_ITER codemap_iter;
  FOR_ALL_ELEM(bucket, codemap_iter, Init(Htable())) {
    FOR_ALL_NODE(cr, cr_iter, Init(bucket)) {
      if (cr->Kind() == CK_CONST)
        cr->Set_dtyp_strictly(Mtype_TransferSize(MTYPE_A8, cr->Dtyp()));
      else if (cr->Kind() == CK_IVAR && cr->Opr() == OPR_ILOAD) {
	Is_True(cr->Dtyp() != MTYPE_B,
	  ("COMP_UNIT::U64_lower_cr: non-preg ivar cannot have have MTYPE_B"));
	if (! MTYPE_is_integral(cr->Dtyp()))
	  continue;
        cr->Reset_sign_extd();
        if (MTYPE_signed(cr->Dsctyp())) 
	  cr->Set_dsctyp(Mtype_TransferSign(MTYPE_U8, cr->Dsctyp()));
        cr->Set_dtyp(MTYPE_U8);
      }
    }
  }

  if ( Get_Trace(TP_GLOBOPT, OPT_LOWER_FLAG)) {
    fprintf( TFile, "%sAfter COMP_UNIT::U64_lower_cr\n%s",
             DBar, DBar );
    Cfg()->Print(TFile);
  }
}
