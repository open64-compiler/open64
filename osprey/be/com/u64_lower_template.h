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


//-*-c++-*-

#ifndef u64_lower_template_INCLUDED
#define u64_lower_template_INCLUDED "u64_lower_template.h"



enum HIGH_ORDER_BITS {
  HOB_none,
  HOB_zero_xtd,
  HOB_sign_xtd,
};


/* ====================================================================
 * Lowers integer operations to exclusively use 64-bit operations and
 * unsigned loads.  Size of integer (in bits) currently representing the 
 * result is returned in maxsize.  Floating-point and boolean values are assumed
 * stored in special registers, so the optimization does not apply to them.
 * Thus, maxsize is 0 if it is a floating-point or boolean value.  
 * hob_state returns info about the bits that are outside the part 
 * representing the result. hob_to_do returns info as to whether any delayed 
 * operation to the high-order bits is needed in order to represent the value 
 * correctly in the 64-bit register.  hob_state and hob_to_do is unused 
 * if maxsize is 0 or 64.
 * ==================================================================== */
template <class NODE>
NODE *
U64_LOWER_expr(NODE *tree, INT &maxsize, 
	       HIGH_ORDER_BITS &hob_state, HIGH_ORDER_BITS &hob_to_do, 
	       BOOL leave_CVTL_at_leaf)
{
  INT maxsize0, maxsize1, new_maxsize;
  HIGH_ORDER_BITS hob_state0, hob_state1;
  HIGH_ORDER_BITS hob_to_do0, hob_to_do1;
  INT i;
  NODE *nd;
  NODE *new_nd = U64_LOWER_alloc_stack(tree); U64_LOWER_copy_node(new_nd, tree);
  OPERATOR opr = U64_LOWER_operator(tree);
  TYPE_ID res = U64_LOWER_rtype(tree);
  TYPE_ID desc = U64_LOWER_desc(tree);
  if (desc == MTYPE_V || desc == MTYPE_UNKNOWN) desc = res; 

  if (opr == OPR_INTRINSIC_OP
#ifdef KEY
      || opr == OPR_PURE_CALL_OP
#endif
     ) {
    for (i = 0; i < U64_LOWER_kid_count(tree); i++) { // kids must be PARMs
      U64_LOWER_set_kid(new_nd, i, U64_LOWER_expr(U64_LOWER_kid(tree,i), 
			maxsize0, hob_state0, hob_to_do0, leave_CVTL_at_leaf));
    }
    if (MTYPE_is_integral(res) && res != MTYPE_B) { 
      maxsize = MTYPE_bit_size(res);
      hob_state = HOB_none;
      hob_to_do = MTYPE_signed(res) ? HOB_sign_xtd : HOB_zero_xtd;
    }
    else maxsize = 0;
    return U64_LOWER_form_node(new_nd, tree);
  }
  if (opr == OPR_SELECT) {
    U64_LOWER_set_kid(new_nd, 0, U64_LOWER_expr(U64_LOWER_kid(tree,0), maxsize0,
				hob_state0, hob_to_do0, leave_CVTL_at_leaf));
    if (maxsize0 != 0 && maxsize0 != 64) { // generate comparison with 0
      if (hob_state0 == HOB_none)  
	U64_LOWER_insert_cvtl_for_kid(new_nd, HOB_zero_xtd, 0, maxsize0, hob_state0);
    }
    U64_LOWER_set_kid1(new_nd, U64_LOWER_expr(U64_LOWER_kid1(tree), maxsize1, 
				  hob_state1, hob_to_do1, leave_CVTL_at_leaf));
    U64_LOWER_set_kid2(new_nd, U64_LOWER_expr(U64_LOWER_kid2(tree), maxsize, 
				    hob_state, hob_to_do, leave_CVTL_at_leaf));
    if (! MTYPE_is_integral(res) || res == MTYPE_B)
      return U64_LOWER_form_node(new_nd, tree);
    U64_LOWER_set_rtype(new_nd, Mtype_TransferSize(MTYPE_A8, res));
    if (maxsize == maxsize1 && hob_state == hob_state1 &&
	hob_to_do == hob_to_do1)
      return U64_LOWER_form_node(new_nd, tree);
    U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do1, 1, maxsize1, hob_state1);
    U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do, 2, maxsize, hob_state);
    maxsize = 64;
    return U64_LOWER_form_node(new_nd, tree);
  }
  // only leaves, unary, binary or ternary expression operators are left
  if (U64_LOWER_kid_count(tree) > 0) {
    U64_LOWER_set_kid0(new_nd, U64_LOWER_expr(U64_LOWER_kid0(tree), maxsize, 
				    hob_state, hob_to_do, leave_CVTL_at_leaf));
    if (U64_LOWER_kid_count(tree) > 1) {
      U64_LOWER_set_kid1(new_nd, U64_LOWER_expr(U64_LOWER_kid1(tree), maxsize1, 
				  hob_state1, hob_to_do1, leave_CVTL_at_leaf));
      if (U64_LOWER_kid_count(tree) > 2) { 
	Is_True(! MTYPE_is_integral(desc), ("unhandled ternary operator"));
        U64_LOWER_set_kid2(new_nd, U64_LOWER_expr(U64_LOWER_kid2(tree), maxsize1,
				hob_state1, hob_to_do1, leave_CVTL_at_leaf));
	maxsize = 0;
	return U64_LOWER_form_node(new_nd, tree);   // must be floating-point 
						// ops, so nothing can be done
      }
    }
  }

  // only leaves, unary or binary expression operators are left
  switch (opr) {

  // loads

  case OPR_LDID:
    if (! MTYPE_is_integral(res) || res == MTYPE_B) {
      maxsize = 0;
      return tree;
    }
    if (U64_LOWER_class(tree) == CLASS_PREG) {
      maxsize = MTYPE_bit_size(res);
      hob_state = HOB_none;
      hob_to_do = MTYPE_signed(res) ? HOB_sign_xtd : HOB_zero_xtd;
      U64_LOWER_set_rtype(tree, Mtype_TransferSize(MTYPE_A8, res));
      return tree;
    }
    maxsize = MTYPE_bit_size(desc);
    if (maxsize == 64)
      return tree;
    hob_state = HOB_zero_xtd; // all loads are zero extended
    hob_to_do = MTYPE_signed(res) ? HOB_sign_xtd : HOB_zero_xtd;
    if (MTYPE_signed(desc)) { // change to unsigned
      U64_LOWER_set_desc(tree, Mtype_TransferSign(MTYPE_U8, desc));
    }
    // force 8-byte loads
    U64_LOWER_set_rtype(tree, Mtype_TransferSize(MTYPE_A8, U64_LOWER_desc(tree)));

    if (! leave_CVTL_at_leaf)
      return tree;
    else if (maxsize < 64 && hob_to_do == HOB_sign_xtd) {
      hob_state = HOB_sign_xtd;
      return U64_LOWER_create_cvtl(MTYPE_I8, tree, maxsize);
    }
    else return tree;

  case OPR_ILOAD:
    if (! MTYPE_is_integral(res) || res == MTYPE_B) {
      maxsize = 0;
      return U64_LOWER_form_node(new_nd, tree);
    }
    maxsize = MTYPE_bit_size(desc);
    if (maxsize == 64)
      return U64_LOWER_form_node(new_nd, tree);
    hob_state = HOB_zero_xtd; // all loads are zero extended
    hob_to_do = MTYPE_signed(res) ? HOB_sign_xtd : HOB_zero_xtd;
    if (MTYPE_signed(desc)) { // change to unsigned
      U64_LOWER_set_desc(new_nd, Mtype_TransferSign(MTYPE_U8, desc));
    }
    // force 8-byte loads
    U64_LOWER_set_rtype(new_nd, Mtype_TransferSize(MTYPE_A8, U64_LOWER_desc(new_nd)));

    if (! leave_CVTL_at_leaf)
      return U64_LOWER_form_node(new_nd, tree);
    else if (maxsize < 64 && hob_to_do == HOB_sign_xtd) {
      hob_state = HOB_sign_xtd;
      return U64_LOWER_create_cvtl(MTYPE_I8, 
				   U64_LOWER_form_node(new_nd, tree), 
				   maxsize);
    }
    else return U64_LOWER_form_node(new_nd, tree);

  case OPR_LDBITS:
    maxsize = U64_LOWER_bit_size(tree); // maxsize cannot be 64 or 0
    hob_state = MTYPE_signed(res) ? HOB_sign_xtd : HOB_zero_xtd;
    hob_to_do = HOB_none;
    return tree;

  case OPR_ILDBITS:
    maxsize = U64_LOWER_bit_size(tree); // maxsize cannot be 64 or 0
    hob_state = MTYPE_signed(res) ? HOB_sign_xtd : HOB_zero_xtd;
    hob_to_do = HOB_none;
    return U64_LOWER_form_node(new_nd, tree);

  case OPR_MLOAD:
    Is_True(maxsize1 != 0, ("illegal type for kid 1 of MLOAD node"));
    U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do, 1, maxsize1, hob_state1);
    maxsize = 0; // because result type is M
    return U64_LOWER_form_node(new_nd, tree);

  // other leaves

  case OPR_INTCONST:
    maxsize = MTYPE_bit_size(res);
    if (maxsize == 32) { 
      if (MTYPE_signed(res)) { // change to I8
        U64_LOWER_set_const_val(tree, (U64_LOWER_const_val(tree) << 32) >> 32);
        hob_state = HOB_sign_xtd;
      }
      else { // change to U8
        U64_LOWER_set_const_val(tree, (UINT64) (U64_LOWER_const_val(tree) << 32) >> 32);
        hob_state = HOB_zero_xtd;
      }
      U64_LOWER_set_rtype(tree, Mtype_TransferSize(MTYPE_A8, res));
      hob_to_do = hob_state;
    }
    return tree;

  case OPR_CONST:
    if (! MTYPE_is_integral(res)) {
      maxsize = 0;
      return tree;
    }
    maxsize = MTYPE_bit_size(res);
    hob_state = HOB_none; // because don't know how it is generated
    hob_to_do = MTYPE_signed(res) ? HOB_sign_xtd : HOB_zero_xtd;
    return tree;

  case OPR_LDA: case OPR_LDA_LABEL:
    maxsize = MTYPE_bit_size(res);
    hob_state = HOB_zero_xtd; // assume all addresses are 0-extended in register
    hob_to_do = HOB_none;
    return tree;

  // unary ops

  case OPR_SQRT: case OPR_RSQRT: case OPR_RECIP:
  case OPR_PAREN:
  case OPR_REALPART: case OPR_IMAGPART:
  case OPR_HIGHPART: case OPR_LOWPART:
    return U64_LOWER_form_node(new_nd, tree);

  case OPR_LNOT:	
    if (MTYPE_bit_size(res) > maxsize) 
      U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do, 0, maxsize, hob_state);
    maxsize = 1;
    hob_state = HOB_zero_xtd;
    hob_to_do = HOB_none;
    return U64_LOWER_form_node(new_nd, tree);

  case OPR_ALLOCA:
    U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do, 0, maxsize, hob_state);
    maxsize = MTYPE_bit_size(res);
    hob_state = HOB_zero_xtd; // all addresses are 0-extended in register
    hob_to_do = HOB_none;
    return U64_LOWER_form_node(new_nd, tree);

  case OPR_NEG:
    if (MTYPE_is_integral(res)) {
      if (maxsize < MTYPE_bit_size(res)) { // enlarge Kid 0
        U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do, 0, maxsize, hob_state);
      }
      U64_LOWER_set_rtype(new_nd, Mtype_TransferSize(MTYPE_A8, res));
    }
    return U64_LOWER_form_node(new_nd, tree);

  case OPR_EXTRACT_BITS:
    U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do, 0, maxsize, hob_state);
    // if bit offset is 0, can do same optimization as ORP_CVTL; omit for now
    U64_LOWER_set_rtype(new_nd, Mtype_TransferSize(MTYPE_A8, res));
    // bug fix for OSP_336
    /* >    U4EXTRACT_BITS o:16 s:15 
       >   F8I4CVT
       Here, compiler will insert a cvtl between the node and its kid.
       >     U8EXTRACT_BITS o:16 s:15
       >    I8CVTL 15 
       >   F8I8CVT
       I think res is not MTYPE_BS, so maxsize should be MTYPE_bit_size(res).
     */
    if (res == MTYPE_BS) {
      maxsize = U64_LOWER_bit_size(tree); // maxsize cannot be 64 or 0
    }
    else {
      maxsize = MTYPE_bit_size(res);// when result type is not bits
    }
    hob_state = MTYPE_signed(res) ? HOB_sign_xtd : HOB_zero_xtd;
    hob_to_do = HOB_none;
    return U64_LOWER_form_node(new_nd, tree);

  case OPR_BNOT: 
    if (maxsize < MTYPE_bit_size(res))
      U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do, 0, maxsize, hob_state);
    if (MTYPE_bit_size(res) == 64) {
      hob_to_do = HOB_none;
      maxsize = 64;
    }
    else {
      U64_LOWER_set_rtype(new_nd, Mtype_TransferSize(MTYPE_A8, res));
      hob_to_do = MTYPE_signed(res) ? HOB_sign_xtd : HOB_zero_xtd;
      maxsize = MTYPE_bit_size(res);
      hob_state = HOB_none;  // because after the bitwise not operation
    }
    return U64_LOWER_form_node(new_nd, tree);

  case OPR_PARM:
  case OPR_TAS:
    if (MTYPE_bit_size(res) > maxsize) 
      U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do, 0, maxsize, hob_state);
    maxsize = MTYPE_is_integral(res) ? 64 : 0;
    return U64_LOWER_form_node(new_nd, tree);

  case OPR_ABS:
    if (maxsize != 0 && maxsize != 64) {
      U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do, 0, maxsize, hob_state);
      hob_state = HOB_zero_xtd;
      hob_to_do = HOB_zero_xtd;
      // leave maxsize's value unchanged
    }
    if (MTYPE_is_integral(res)) 
      U64_LOWER_set_rtype(new_nd, Mtype_TransferSize(MTYPE_A8, res));
    return U64_LOWER_form_node(new_nd, tree);

  case OPR_CVTL:
    if (U64_LOWER_cvtl_bits(tree) > maxsize) {
      if (MTYPE_signed(res) && 
	  (hob_to_do == HOB_sign_xtd ||
	   hob_to_do == HOB_none && hob_state == HOB_sign_xtd) ||
	  (hob_to_do == HOB_zero_xtd ||
	   hob_to_do == HOB_none && hob_state == HOB_zero_xtd) ) {
	// CVTL can be deleted
      }
      else {
        U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do, 0, maxsize, hob_state);
	maxsize = U64_LOWER_cvtl_bits(tree);
        hob_state = MTYPE_signed(res) ? HOB_sign_xtd : HOB_zero_xtd;
        hob_to_do = HOB_none;
	return U64_LOWER_form_node(new_nd, tree);
      }
    }
    else {
      maxsize = U64_LOWER_cvtl_bits(tree);
      hob_state = HOB_none;
      hob_to_do = MTYPE_signed(res) ? HOB_sign_xtd : HOB_zero_xtd;
    }
    // CVTL can be omitted because delayed
    nd = U64_LOWER_kid0(new_nd);
    U64_LOWER_delete(tree);
    return nd;

  case OPR_CVT:
    if (! MTYPE_is_integral(res)) { 
      hob_to_do = MTYPE_is_signed(desc) ? HOB_sign_xtd : HOB_zero_xtd;
      U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do, 0, maxsize, hob_state);
      maxsize = 0; 
      if (MTYPE_is_integral(desc))
        U64_LOWER_set_desc(new_nd, Mtype_TransferSize(MTYPE_A8, desc));
      return U64_LOWER_form_node(new_nd, tree);
    }
    if (! MTYPE_is_integral(desc) || desc == MTYPE_B) { 
      // res must be integral here
      U64_LOWER_set_rtype(new_nd, Mtype_TransferSize(MTYPE_A8, res));
      maxsize = 64;
      return U64_LOWER_form_node(new_nd, tree);
    }
    if (MTYPE_bit_size(res) < maxsize) { // truncation
      maxsize = MTYPE_bit_size(res);
      hob_state = HOB_none;
      hob_to_do = MTYPE_signed(res) ? HOB_sign_xtd : HOB_zero_xtd;
    }
    else if (MTYPE_bit_size(desc) == maxsize) {
      if (MTYPE_bit_size(res) == MTYPE_bit_size(desc))
        hob_to_do = MTYPE_signed(res) ? HOB_sign_xtd : HOB_zero_xtd;
      else hob_to_do = MTYPE_signed(desc) ? HOB_sign_xtd : HOB_zero_xtd;
    }
    // this CVT can be omitted
    nd = U64_LOWER_kid0(new_nd);
    U64_LOWER_delete(tree);
    return nd;

  case OPR_MINPART: case OPR_MAXPART:
    if (! MTYPE_is_integral(res) || res == MTYPE_B)
      return U64_LOWER_form_node(new_nd, tree);
    // fall thru
  case OPR_RND: case OPR_TRUNC: case OPR_CEIL:
    U64_LOWER_set_rtype(new_nd, Mtype_TransferSize(MTYPE_A8, res));
    maxsize = 64;

    // OSP, do not change rtype of FLOOR into A8
    // fall thru 
  case OPR_FLOOR:
    return U64_LOWER_form_node(new_nd, tree);

  // binary ops

  case OPR_ADD: case OPR_SUB: 
    if (MTYPE_is_integral(res) && res != MTYPE_B) { 
      if (maxsize < MTYPE_bit_size(res)) { // enlarge Kid 0
        U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do, 0, maxsize, hob_state);
      }
      if (maxsize1 < MTYPE_bit_size(res)) { // enlarge Kid 1
        U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do1, 1, maxsize1, hob_state1);
      }
      U64_LOWER_set_rtype(new_nd, Mtype_TransferSize(MTYPE_A8, res));

      // If res of kid 0 and kid 1 are both MTYPE_B, we do not generate 
      //   CVTL for res for the highest bit of the result will be treates as sign
      //   after CVTL is expand to extr instruction
      if( maxsize != 0 || maxsize1 != 0 ) 
	  maxsize = MIN(MAX(maxsize, maxsize1) + 1, 64);
      // if both operands's high-order bits were already in designated state and
      // the operation's result cannot overflow the bit size, just return
      if (hob_state != HOB_none && hob_state == hob_state1 &&
          (hob_to_do == HOB_none || hob_to_do == hob_state) &&
	  (hob_to_do1 == HOB_none || hob_to_do1 == hob_state1) &&
	  maxsize <= MTYPE_bit_size(res))
	return U64_LOWER_form_node(new_nd, tree);
      if (maxsize > MTYPE_bit_size(res))
	maxsize = MTYPE_bit_size(res); // can overflow, so need to truncate
      hob_state = HOB_none;
      hob_to_do = MTYPE_signed(res) ? HOB_sign_xtd : HOB_zero_xtd;
    }
    return U64_LOWER_form_node(new_nd, tree);

  case OPR_MPY: // coming in must be either 64-bit or 32-bit
    if (MTYPE_is_integral(res) && res != MTYPE_B) { 
      // in our implementation of int multiply, the high-order bits of the 
      // operands are not looked at and the high-order bits of the result can
      // contain garbage
      if (maxsize < maxsize1) { // enlarge Kid 0
        U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do, 0, maxsize, hob_state);
	maxsize = maxsize1;
      }
      if (maxsize > maxsize1) { // enlarge Kid 1
        U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do1, 1, maxsize1, hob_state1);
      }

      if (MTYPE_bit_size(res) == 64 && maxsize > 16) { // use 64-bit MPY
	if (maxsize < 64) {
          U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do, 0, maxsize, hob_state);
          U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do1, 1, maxsize1, hob_state1);
	}
        U64_LOWER_set_rtype(new_nd, Mtype_TransferSize(MTYPE_A8, res));
	maxsize = 64;
      }
#ifdef TARG_IA64
      //in ia64 use 32-bit MPY instead of 16-bit and 8-bit MPY
      else { 
	if (maxsize < 32) {
          U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do, 0, maxsize, hob_state);
          U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do1, 1, maxsize1, hob_state1);
	}
        U64_LOWER_set_rtype(new_nd, Mtype_TransferSize(MTYPE_A4, res));
	maxsize = 32;
      }
#else
      else if (maxsize > 8) { // use 32-bit MPY
	if (maxsize < 32) {
          U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do, 0, maxsize, hob_state);
          U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do1, 1, maxsize1, hob_state1);
	}
        U64_LOWER_set_rtype(new_nd, Mtype_TransferSize(MTYPE_A4, res));
	maxsize = 32;
      }
      else if (maxsize > 4) { // use 16-bit MPY
	if (maxsize < 16) {
          U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do, 0, maxsize, hob_state);
          U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do1, 1, maxsize1, hob_state1);
	}
        U64_LOWER_set_rtype(new_nd, Mtype_TransferSize(MTYPE_I2, res));
	maxsize = 16;
      }
      else { // use 8-bit MPY
	if (maxsize < 8) {
          U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do, 0, maxsize, hob_state);
          U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do1, 1, maxsize1, hob_state1);
	}
        U64_LOWER_set_rtype(new_nd, Mtype_TransferSize(MTYPE_I1, res));
	maxsize = 8;
      }
#endif  //TARG_IA64

      if (MTYPE_bit_size(res) == 32 && maxsize > 32) 
	maxsize = 32; // for [IU]4MPY, extra truncation at 32-bit boundary
      hob_state = HOB_none;
      hob_to_do = MTYPE_signed(res) ? HOB_sign_xtd : HOB_zero_xtd;
    }
    return U64_LOWER_form_node(new_nd, tree);

  case OPR_DIV: 
  case OPR_MOD: case OPR_REM:
  case OPR_DIVREM:
    // in our implementation of int divide, the high-order bits of the 
    // operands ARE looked at and the high-order bits of the result does NOT
    // contain garbage; will leave operation as original size (4 or 8 bytes) 
    // since smaller-size operation translates to shorter code sequence
    if (MTYPE_is_integral(res) && res != MTYPE_B) { 
      if (opr == OPR_DIV || opr == OPR_DIVREM)
	new_maxsize = maxsize;
      else new_maxsize = maxsize1;
      // [IU]4DIV assumes high-order 32-bits do not contain garbage
      U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do, 0, maxsize, hob_state);
      U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do1, 1, maxsize1, hob_state1);
      // following needed because operand of 32-bit div may be of different sign
      // e.g. for -2 under U4DIV, need to zero-out the high-order 32 bits
      if (res == MTYPE_I4) {
        U64_LOWER_insert_cvtl_for_kid(new_nd, HOB_sign_xtd, 0, 32, hob_state);
        U64_LOWER_insert_cvtl_for_kid(new_nd, HOB_sign_xtd, 1, 32, hob_state1);
      }
      else if (res == MTYPE_U4) {
	U64_LOWER_insert_cvtl_for_kid(new_nd, HOB_zero_xtd, 0, 32, hob_state);
        U64_LOWER_insert_cvtl_for_kid(new_nd, HOB_zero_xtd, 1, 32, hob_state1);
      }

      maxsize = new_maxsize;
      // high-order 32-bits of result guaranteed clean
      hob_state = MTYPE_signed(res) ? HOB_sign_xtd : HOB_zero_xtd;
      hob_to_do = HOB_none;
    }
    return U64_LOWER_form_node(new_nd, tree);

  case OPR_BAND: case OPR_BIOR: case OPR_BNOR: case OPR_BXOR:
    // the sign in these operators are somewhat arbitrary, so need to be
    // less aggressive (751054)
    if (desc != MTYPE_B) { 
      U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do, 0, maxsize, hob_state);
      U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do1, 1, maxsize1, hob_state1);
      maxsize = MAX(maxsize, maxsize1);
      U64_LOWER_set_rtype(new_nd, Mtype_TransferSize(MTYPE_A8, res));
      hob_state = HOB_none;
      hob_to_do = HOB_none;
    }
    return U64_LOWER_form_node(new_nd, tree);

  case OPR_MAX: case OPR_MIN: 
    if (MTYPE_is_integral(desc) && desc != MTYPE_B) { 
      // desc size is either 64 or 32; make both operands at least that size
      if (maxsize < MTYPE_bit_size(desc)) // enlarge Kid 0
	U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do, 0, maxsize, hob_state);
      if (maxsize1 < MTYPE_bit_size(desc)) // enlarge Kid 1
	U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do1, 1, maxsize1, hob_state1);
      maxsize = MAX(maxsize, maxsize1);
      if (MTYPE_bit_size(desc) == 64) {
	hob_to_do = HOB_none;
	hob_state = MTYPE_signed(desc) ? HOB_sign_xtd : HOB_zero_xtd;
      }
      else {
	if (hob_to_do == hob_state && hob_to_do1 == hob_state1 ||
	    hob_to_do == HOB_none && hob_to_do1 == HOB_none) {
	  hob_to_do = HOB_none;
	  hob_state = MTYPE_signed(desc) ? HOB_sign_xtd : HOB_zero_xtd;
	} 
	else {
	  hob_state = HOB_none;
	  hob_to_do = MTYPE_signed(desc) ? HOB_sign_xtd : HOB_zero_xtd;
        }
      }
    }
    return U64_LOWER_form_node(new_nd, tree);

  case OPR_MINMAX:
    if (MTYPE_is_integral(desc) && desc != MTYPE_B) { 
      U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do, 0, maxsize, hob_state);
      U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do1, 1, maxsize1, hob_state1);
      maxsize = MAX(maxsize, maxsize1);
      U64_LOWER_set_rtype(new_nd, Mtype_TransferSize(MTYPE_A8, res));
      hob_state = MTYPE_signed(desc) ? HOB_sign_xtd : HOB_zero_xtd;
      hob_to_do = HOB_none;
    }
    for (int i = 0; i < U64_LOWER_kid_count(new_nd); i++) { 
      if (U64_LOWER_operator(U64_LOWER_kid(new_nd, i)) == OPR_CVTL) {
        TYPE_ID new_res_ty; 
        if (U64_LOWER_desc(new_nd) == MTYPE_V)
          new_res_ty = Mtype_TransferSign(U64_LOWER_rtype(new_nd), 
          	              U64_LOWER_rtype(U64_LOWER_kid(new_nd, i))); 
        else 
          new_res_ty = Mtype_TransferSign(U64_LOWER_desc(new_nd), 
                                U64_LOWER_rtype(U64_LOWER_kid(new_nd, i))); 
        U64_LOWER_set_rtype(U64_LOWER_kid(new_nd, i), new_res_ty); 
      } 
    }
    return U64_LOWER_form_node(new_nd, tree);

  case OPR_EQ: case OPR_NE: 
  case OPR_GE: case OPR_GT: case OPR_LE: case OPR_LT:
    if (MTYPE_is_integral(desc) && desc != MTYPE_B) { 
      // desc size is either 64 or 32; make both operands at least that size
      if (maxsize < MTYPE_bit_size(desc)) // enlarge Kid 0
	U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do, 0, maxsize, hob_state);
      if (maxsize1 < MTYPE_bit_size(desc)) // enlarge Kid 1
	U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do1, 1, maxsize1, hob_state1);
    }
    maxsize = 0;	// since result is type B
    return U64_LOWER_form_node(new_nd, tree);

  case OPR_ASHR: case OPR_LSHR:
    if (MTYPE_bit_size(res) == 64) {
      U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do, 0, maxsize, hob_state);
      U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do1, 1, maxsize1, hob_state1);
      // maxsize is maxsize of left operand
      hob_state = (opr == OPR_ASHR) ? HOB_sign_xtd : HOB_zero_xtd;
      hob_to_do = HOB_none;
      return U64_LOWER_form_node(new_nd, tree);
    }
    // MTYPE_bit_size(res) == 32
    if (U64_LOWER_operator(U64_LOWER_kid1(tree)) == OPR_INTCONST) {
      // I4SHR will translate to extr, U4SHR to extr.u
      if (maxsize < 32) { // CVTL for kid 0
        U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do, 0, maxsize, hob_state);
      }
    }
    else { // variable shift amount
      if (maxsize < 32) { // CVTL for kid 0
        U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do, 0, maxsize, hob_state);
      }

      if (maxsize > 32) { // truncate kid via a CVTL 32
	nd = U64_LOWER_create_cvtl((opr == OPR_ASHR) ? MTYPE_I8 : MTYPE_U8,
				   U64_LOWER_kid0(tree), 32);
        U64_LOWER_set_kid0(new_nd, nd);
      }
      else {
	if (opr == OPR_ASHR && hob_state != HOB_sign_xtd)
	  U64_LOWER_insert_cvtl_for_kid(new_nd, HOB_sign_xtd, 0, 32, hob_state);
	else if (opr == OPR_LSHR && hob_state != HOB_zero_xtd)
	  U64_LOWER_insert_cvtl_for_kid(new_nd, HOB_zero_xtd, 0, 32, hob_state);
      }

      U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do1, 1, maxsize1, hob_state1); // kid 1
      U64_LOWER_set_rtype(new_nd, Mtype_TransferSize(MTYPE_A8, res));
    }
    maxsize = 32;
    hob_state = (opr == OPR_ASHR) ? HOB_sign_xtd : HOB_zero_xtd;
    hob_to_do = HOB_none;
    return U64_LOWER_form_node(new_nd, tree);

  case OPR_SHL: 
    if (MTYPE_bit_size(res) == 64) {
      U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do, 0, maxsize, hob_state);
      U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do1, 1, maxsize1, hob_state1);
      maxsize = 64;
      return U64_LOWER_form_node(new_nd, tree);
    }
    // MTYPE_bit_size(res) == 32
    if (maxsize < 32) { // CVTL for kid 0
      U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do, 0, maxsize, hob_state);
    }
    maxsize = 32;
    hob_state = HOB_none; 
    hob_to_do = MTYPE_signed(res) ? HOB_sign_xtd : HOB_zero_xtd;
    if (U64_LOWER_operator(U64_LOWER_kid1(tree)) == OPR_INTCONST) {
      // I4SHL and U4SHL will translate to dep.z
      return U64_LOWER_form_node(new_nd, tree);
    }
    else { // variable shift amount
      U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do1, 1, maxsize1, hob_state1); // kid 1
      U64_LOWER_set_rtype(new_nd, Mtype_TransferSize(MTYPE_A8, res));
      return U64_LOWER_form_node(new_nd, tree);
    }

  case OPR_COMPOSE_BITS:
    if (maxsize <= (U64_LOWER_bit_size(tree) + U64_LOWER_bit_offset(tree)))
      U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do, 0, maxsize, hob_state);
    if (maxsize1 < U64_LOWER_bit_size(tree))
      U64_LOWER_insert_cvtl_for_kid(new_nd, hob_to_do1, 1, maxsize1, hob_state1);
    U64_LOWER_set_rtype(new_nd, Mtype_TransferSize(MTYPE_A8, res));
    return U64_LOWER_form_node(new_nd, tree);

  case OPR_LAND: case OPR_LIOR:
  case OPR_COMPLEX:
    return U64_LOWER_form_node(new_nd, tree);

  default:	
    Is_True(FALSE,("unexpected operator"));
  }
  return NULL;
}

#endif
