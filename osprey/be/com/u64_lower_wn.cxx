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
#include "stab.h"
#include "wn.h"
#include "wn_util.h"
#include "ir_reader.h"
#include "wn_lower.h"

#define U64_LOWER_alloc_stack(x) x;

#include "u64_lower_template.h"


/* ====================================================================
 * specialized version of support routines used in the U64_LOWER template 
 * ==================================================================== */

inline void
U64_LOWER_delete(WN *wn) { WN_Delete(wn); }

inline INT
U64_LOWER_kid_count(const WN *wn) { return WN_kid_count(wn); }

inline WN *
U64_LOWER_kid(const WN *wn, INT i) { return WN_kid(wn, i); }

inline void
U64_LOWER_set_kid(WN *wn, INT i, WN *k) { WN_kid(wn, i) = k; }

inline WN *
U64_LOWER_kid0(const WN *wn) { return WN_kid0(wn); }

inline void
U64_LOWER_set_kid0(WN *wn, WN *k) { WN_kid0(wn) = k; }

inline WN *
U64_LOWER_kid1(const WN *wn) { return WN_kid1(wn); }

inline void
U64_LOWER_set_kid1(WN *wn, WN *k) { WN_kid1(wn) = k; }

inline WN *
U64_LOWER_kid2(const WN *wn) { return WN_kid2(wn); }

inline void
U64_LOWER_set_kid2(WN *wn, WN *k) { WN_kid2(wn) = k; }

inline OPERATOR
U64_LOWER_operator(const WN *wn) { return WN_operator(wn); }

inline TYPE_ID
U64_LOWER_rtype(const WN *wn) { return WN_rtype(wn); }

inline void
U64_LOWER_set_rtype(WN *wn, TYPE_ID t) { WN_set_rtype(wn, t); }

inline TYPE_ID
U64_LOWER_desc(const WN *wn) { return WN_desc(wn); }

inline void
U64_LOWER_set_desc(WN *wn, TYPE_ID t) { WN_set_desc(wn, t); }

inline INT
U64_LOWER_cvtl_bits(const WN *wn) { return WN_cvtl_bits(wn); }

inline void
U64_LOWER_set_cvtl_bits(WN *wn, INT i) { WN_cvtl_bits(wn) = i; }

inline INT
U64_LOWER_bit_size(const WN *wn) { return WN_bit_size(wn); }

inline INT
U64_LOWER_bit_offset(const WN *wn) { return WN_bit_offset(wn); }

inline INT64
U64_LOWER_const_val(const WN *wn) { return WN_const_val(wn); }

inline void
U64_LOWER_set_const_val(WN *wn, INT64 i) { WN_const_val(wn) = i; }

inline ST_CLASS
U64_LOWER_class(const WN *wn) { return WN_class(wn); }

inline void
U64_LOWER_copy_node(WN *newwn, WN *oldwn) {}

inline WN *
U64_LOWER_alloc_stack_copy(WN *tree) { return tree; } // because WN not shared

inline WN *
U64_LOWER_form_node(WN *new_nd, WN *old_nd) { return new_nd; }

WN *
U64_LOWER_create_cvtl(TYPE_ID res, WN *kid, INT cvtl_len) {
  WN *wn = WN_Create(OPR_CVTL, res, MTYPE_V, 1);
  WN_kid0(wn) = kid;
  WN_cvtl_bits(wn) = cvtl_len;
  return wn;
}

WN *
U64_LOWER_create_ne_0(TYPE_ID res, TYPE_ID desc, WN *kid) {
  WN *wn = WN_Create(OPR_NE, res, desc, 2);
  WN_kid0(wn) = kid;
  WN_kid1(wn) = WN_Intconst(MTYPE_U8, 0);
  return wn;
}

/* ====================================================================
 * Insert a CVTL that has been delayed. hob_to_do gives the target signedness.
 * The CVTL is on Kid kidno of tree with size cvtl_bits.  
 * hob_state provides hob info so the CVTL can be omitted if necessary.  
 * Resulting hob info is returned in hob_state.
 * ==================================================================== */
void 
U64_LOWER_insert_cvtl_for_kid(WN *tree, HIGH_ORDER_BITS hob_to_do, INT kidno, 
		    INT cvtl_bits, HIGH_ORDER_BITS &hob_state)
{
  WN *wn;

  if (cvtl_bits == 64)
    return;
  if (cvtl_bits == 0)
    return;
  if (hob_to_do == HOB_none)
    return;
  if (hob_to_do == hob_state)
    return;
  if (hob_to_do == HOB_sign_xtd) { // sign-extend
    wn = U64_LOWER_create_cvtl(MTYPE_I8, WN_kid(tree, kidno), cvtl_bits); 
  }
  else { // zero-extend
    wn = U64_LOWER_create_cvtl(MTYPE_U8, WN_kid(tree, kidno), cvtl_bits); 
  }
  WN_kid(tree, kidno) = wn;
  hob_state = hob_to_do;
}

/* ====================================================================
 * End of specialized support routines
 * ==================================================================== */


/* ====================================================================
 * lower the given statement to zero-extended 64-bit target ISA.
 * ==================================================================== */
static void
U64_LOWER_stmt_wn(WN *tree, BOOL leave_CVTL_at_leaf)
{
  INT maxsize;
  HIGH_ORDER_BITS hob_state, hob_to_do;
  OPERATOR opr = WN_operator(tree);
  TYPE_ID desc = WN_desc(tree);
  TYPE_ID res = WN_rtype(tree);
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
  case OPR_REGION_EXIT:
    return;

  case OPR_REGION:
#ifdef TARG_IA64
    U64_LOWER_stmt_wn(WN_region_body(tree), leave_CVTL_at_leaf);
#endif
    return;

  case OPR_DEALLOCA:	// only kid 0 contains no node relevant to U64 lowering
  case OPR_PREFETCH:
  case OPR_EVAL: 
  case OPR_AGOTO:
    // don't need to do anything
    WN_kid0(tree) = U64_LOWER_expr(WN_kid0(tree), maxsize, hob_state, hob_to_do,
				   leave_CVTL_at_leaf);
    return;

  case OPR_TRUEBR:
  case OPR_FALSEBR:
  case OPR_ASSERT:
    WN_kid0(tree) = U64_LOWER_expr(WN_kid0(tree), maxsize, hob_state, hob_to_do,
				   leave_CVTL_at_leaf);
    if (maxsize != 0 && maxsize != 64) { // generate comparison with 0
      if (hob_state == HOB_none)  // enlarge Kid 0
	U64_LOWER_insert_cvtl_for_kid(tree, HOB_zero_xtd, 0, maxsize, hob_state);
    }
    return;

  case OPR_ISTORE:
    WN_kid1(tree) = U64_LOWER_expr(WN_kid1(tree), maxsize, hob_state, hob_to_do,
				   leave_CVTL_at_leaf);
  // fall through

  case OPR_STID:
    WN_kid0(tree) = U64_LOWER_expr(WN_kid0(tree), maxsize, hob_state, hob_to_do,
				   leave_CVTL_at_leaf);
    Is_True(desc != MTYPE_B, ("illegal mtype B"));
    if (MTYPE_bit_size(desc) > maxsize) 
      U64_LOWER_insert_cvtl_for_kid(tree, hob_to_do, 0, maxsize, hob_state);
    return;

  case OPR_ISTBITS:
    WN_kid1(tree) = U64_LOWER_expr(WN_kid1(tree), maxsize, hob_state, hob_to_do,
				   leave_CVTL_at_leaf);
  // fall through

  case OPR_STBITS:
    WN_kid0(tree) = U64_LOWER_expr(WN_kid0(tree), maxsize, hob_state, hob_to_do,
				   leave_CVTL_at_leaf);
    if (WN_bit_size(tree) > maxsize) 
      U64_LOWER_insert_cvtl_for_kid(tree, hob_to_do, 0, maxsize, hob_state);
    return;

  case OPR_RETURN_VAL:
    WN_kid0(tree) = U64_LOWER_expr(WN_kid0(tree), maxsize, hob_state, hob_to_do,
				   leave_CVTL_at_leaf);
    if (MTYPE_bit_size(res) > maxsize)
      U64_LOWER_insert_cvtl_for_kid(tree, hob_to_do, 0, maxsize, hob_state);
    return;

  case OPR_CALL:          
  case OPR_ICALL:
  case OPR_INTRINSIC_CALL:
    for (i = 0; i < WN_kid_count(tree); i++) 
      WN_kid(tree,i) = U64_LOWER_expr(WN_kid(tree,i), maxsize, hob_state,
				      hob_to_do, leave_CVTL_at_leaf);
    return;

  case OPR_PICCALL:
    for (i = 0; i < (WN_kid_count(tree)-1); i++) 
      WN_kid(tree,i) = U64_LOWER_expr(WN_kid(tree,i), maxsize, hob_state,
				      hob_to_do, leave_CVTL_at_leaf);
    return;

  case OPR_ASM_STMT:
    for (i = 2; i < WN_kid_count(tree); i++) {
      WN_kid0(WN_kid(tree,i)) =
	U64_LOWER_expr(WN_kid0(WN_kid(tree,i)), maxsize, hob_state,
		       hob_to_do, leave_CVTL_at_leaf);
    }
    return;

  case OPR_COMPGOTO:
  case OPR_XGOTO:
    WN_kid0(tree) = U64_LOWER_expr(WN_kid0(tree), maxsize, hob_state, hob_to_do,
				   leave_CVTL_at_leaf);
    U64_LOWER_insert_cvtl_for_kid(tree, HOB_zero_xtd, 0, maxsize, hob_state);
    return;

  case OPR_MSTORE:
    WN_kid0(tree) = U64_LOWER_expr(WN_kid0(tree), maxsize, hob_state, hob_to_do,
				   leave_CVTL_at_leaf);
    WN_kid1(tree) = U64_LOWER_expr(WN_kid1(tree), maxsize, hob_state, hob_to_do,
				   leave_CVTL_at_leaf);
    WN_kid2(tree) = U64_LOWER_expr(WN_kid2(tree), maxsize, hob_state, hob_to_do,
				   leave_CVTL_at_leaf);
    U64_LOWER_insert_cvtl_for_kid(tree, hob_to_do, 2, maxsize, hob_state);
    return;

  case OPR_XPRAGMA:
    WN_kid0(tree) = U64_LOWER_expr(WN_kid0(tree), maxsize, hob_state, hob_to_do,
				   leave_CVTL_at_leaf);
    U64_LOWER_insert_cvtl_for_kid(tree, hob_to_do, 0, maxsize, hob_state);
    return;

  case OPR_LOOP_INFO:
    if (WN_kid1(tree) != NULL) {
      WN_kid1(tree) = U64_LOWER_expr(WN_kid1(tree), maxsize, hob_state, 
				     hob_to_do, leave_CVTL_at_leaf);
      U64_LOWER_insert_cvtl_for_kid(tree, HOB_zero_xtd, 1, maxsize, hob_state);
    }
    return;

  // structured control flow statements

  case OPR_BLOCK: {
    WN *stmt;
    for (stmt = WN_first(tree); stmt; stmt = WN_next(stmt)) {
      Is_True(OPERATOR_is_stmt(WN_operator(stmt)) || OPERATOR_is_scf(WN_operator(stmt)),
	      ("statement operator expected"));
      U64_LOWER_stmt_wn(stmt, leave_CVTL_at_leaf);
    }
    return;
    }

  case OPR_DO_LOOP:
    U64_LOWER_stmt_wn(WN_kid(tree, 1), leave_CVTL_at_leaf); // the initialization statement
    WN_kid2(tree) = U64_LOWER_expr(WN_kid2(tree), maxsize, hob_state, hob_to_do,
				   leave_CVTL_at_leaf);
    U64_LOWER_stmt_wn(WN_kid(tree, 3), leave_CVTL_at_leaf); // the increment statement
    U64_LOWER_stmt_wn(WN_kid(tree, 4), leave_CVTL_at_leaf); // the block
    if (WN_kid(tree, 5) != NULL)
      U64_LOWER_stmt_wn(WN_kid(tree, 5), leave_CVTL_at_leaf); // LOOP_INFO
    return;

  case OPR_DO_WHILE:
  case OPR_WHILE_DO:
    WN_kid0(tree) = U64_LOWER_expr(WN_kid0(tree), maxsize, hob_state, hob_to_do,
				   leave_CVTL_at_leaf);
    U64_LOWER_stmt_wn(WN_kid(tree, 1), leave_CVTL_at_leaf);
    return;

  case OPR_IF:
    WN_kid0(tree) = U64_LOWER_expr(WN_kid0(tree), maxsize, hob_state, hob_to_do,
				   leave_CVTL_at_leaf);
    U64_LOWER_stmt_wn(WN_kid(tree, 1), leave_CVTL_at_leaf);
    U64_LOWER_stmt_wn(WN_kid(tree, 2), leave_CVTL_at_leaf);
    return;

  default:	
    Is_True(FALSE,("unexpected operator"));
  }

  return;  
}

/* ====================================================================
 * Top level routine for lowering to zero-extended 64-bit target ISA.
 * M WHIRL assumed.
 * ==================================================================== */
void
U64_lower_wn(WN *tree, BOOL leave_CVTL_at_leaf)
{
  Start_Timer(T_Lower_CU);
  Set_Error_Phase("U64 Lowering");

  if (WN_operator(tree) == OPR_FUNC_ENTRY) 
    U64_LOWER_stmt_wn(WN_func_body(tree), leave_CVTL_at_leaf);
  else if (WN_operator(tree) == OPR_REGION) 
    U64_LOWER_stmt_wn(WN_region_body(tree), leave_CVTL_at_leaf);
  else if (OPERATOR_is_stmt(WN_operator(tree)) || OPERATOR_is_scf(WN_operator(tree)))
    U64_LOWER_stmt_wn(tree, leave_CVTL_at_leaf);
  else Is_True(FALSE, ("unexpected WHIRL operator"));

  Stop_Timer(T_Lower_CU);

  WN_Lower_Checkdump("After U64 lowering", tree, 0);   

  WN_verifier(tree);

  return;
}
