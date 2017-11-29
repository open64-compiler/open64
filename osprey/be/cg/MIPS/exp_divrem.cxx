/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
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


/* CGEXP routines for expanding divide and rem */

#include <stdint.h>
#include <signal.h>
#include "defs.h"
#include "errors.h"
#include "erglob.h"
#include "ercg.h"
#include "tracing.h"
#include "config.h"
#include "config_targ_opt.h"
#include "config_debug.h"
#include "mtypes.h"
#include "tn.h"
#include "cg_flags.h"
#include "op.h"
#include "cgexp.h"
#include "cgexp_internals.h"

#define RESET_COND_DEF_LAST(ops) Set_OP_cond_def_kind(OPS_last(ops),OP_ALWAYS_UNC_DEF)

/*****************************************************************************
 *
 * Floating-point division external interface
 *
 *****************************************************************************/

void
Expand_Float_Divide(TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  FmtAssert(FALSE, ("Not Yet Implemented"));
}
    
void
Expand_Float_Recip(TN *result, TN *src, TYPE_ID mtype, OPS *ops)
{
  FmtAssert(FALSE, ("Not Yet Implemented"));
}

/*****************************************************************************
 *
 * Integer division internal support routines
 *
 *****************************************************************************/

/* return TRUE if the val is a power of 2 */
#define IS_POWER_OF_2(val)	((val != 0) && ((val & (val-1)) == 0))

BOOL Is_Power_Of_2(INT64 val, TYPE_ID mtype)
{
  if (MTYPE_is_signed(mtype) && val < 0) val = -val;

  if (mtype == MTYPE_U4) val &= 0xffffffffull;

  return IS_POWER_OF_2(val);
}


static INT
Get_Power_Of_2 (INT64 val, TYPE_ID mtype)
{
  INT i;
  INT64 pow2mask;

  if (MTYPE_is_signed(mtype) && val < 0) val = -val;

  if (mtype == MTYPE_U4) val &= 0xffffffffull;

  pow2mask = 1;
  for ( i = 0; i < MTYPE_size_reg(mtype); ++i ) {
    if (val == pow2mask) return i;
    pow2mask <<= 1;
  }

  FmtAssert(FALSE, ("Get_Power_Of_2 unexpected value"));
  /* NOTREACHED */
}


/* Expand the sequence for division by a power of two. It is the
 * caller's job to verify that divisor is a non-zero power of two.
 */
static void
Expand_Power_Of_2_Divide (TN *result, TN *numer, INT64 dvsr, TYPE_ID mtype, OPS *ops)
{
  INT n = Get_Power_Of_2(dvsr, mtype);
  TN *tmp_result;

  if (MTYPE_is_unsigned(mtype)) {
    Expand_Shift(result, numer, Gen_Literal_TN(n, 4), mtype, shift_lright, ops);
  } 
  else {

/*For MIPS3*/
    TN *t1 = Build_TN_Of_Mtype(mtype);
    TN *t2 = dvsr < 0 ? Build_TN_Of_Mtype(mtype) : result;
    INT64 absdvsr = dvsr < 0 ? -dvsr : dvsr;
    BOOL is_double = MTYPE_is_size_double(mtype);
	
    TN* mask_tn = Build_TN_Of_Mtype (mtype);
    Expand_Shift(mask_tn, numer, Gen_Literal_TN(MTYPE_size_reg(mtype)-1, 4), mtype, shift_aright, ops);

    TN* addend = Build_TN_Of_Mtype (mtype);
    TN* absdvsr_tn = Build_TN_Of_Mtype (mtype);
    Expand_Immediate(absdvsr_tn, Gen_Literal_TN(absdvsr-1, 8), TRUE, ops);
    Build_OP (TOP_and, addend, mask_tn, absdvsr_tn, ops);
    TN* temp = Build_TN_Like(result);
    Expand_Add (temp, numer, addend, mtype, ops);

    Expand_Shift(t2, temp, Gen_Literal_TN(n, 4), mtype, shift_aright, ops);
	

    if (dvsr < 0) Expand_Neg(result, t2, mtype, ops);

/*The following code is for MIPS4, to be merged with MIPS3*/
  }
}

/******************************************************************************
 *
 *   Function Name: determine_pseudo_inverse
 *
 *   Author: Bill Homer
 *
 *   Input Parameters: b              constant divisor
 *                     maxbits_in_a   size of dividend
 *
 *   Returns:          pseudo inverse
 *                     pn             smallest n, such that 2^n >= b
 *
 *   Description:
 *   Given an unsigned integer, calculate a "pseudo-inverse"
 *   (which is the return value) and the associated shift width
 *   (which is returned via the third parameter).
 *
 *   Usage:
 *    Let BPUL be the number of bits in an unsigned long.
 *    When b is a compile time constant, optimize an unsigned
 *    long integer division on T90,IEEE
 *                                    q = a/b
 *    by replacing it with:
 *
 *    Case 1) b == 2^n                q = b>>n
 *            Not done here.
 *
 *    Case 2) b >= 2^(BPUL-1)         q = (a >= b)
 *
 *    Case 3) a, b < 2^(BPUL/2)
 *            Not used; might be a good way to handle 32 bit ints.
 *            At compile time:        d = (~0UL)/b
 *            At run time:            q = int_mult_upper(d,a)
 *
 *    Case 4) Not used - general case; longer code than 5) & 6)
 *
 *    Case 5) a < 2^(BPUL-1)
 *            Used for 32 and 64 bit signed ints.
 *            At compile time:        d = determine_pseudo_inverse(b,BPUL-1,&n)
 *            At run time:            q = int_mult_upper(d,a) >> (n-1)
 *
 *    Case 6) default
 *            Used for unsigned 32 and 64 bit ints.
 *            At compile time:        d = determine_pseudo_inverse(b,BPUL,&n)
 *            At run time:            p = int_mult_upper(d,a)
 *                                    q = (p + ((a-p)>>1)) >> (n-1)
 *
 ******************************************************************************/

static UINT64 determine_pseudo_inverse (
  UINT64      b,
  INT64       maxbits_a,
  INT64      *pn)
{
  INT64  i, n;
  UINT64 m, q, b1;

  /*  Calculate the smallest n such that 2^n >= b,
   *  and the corresponding m = 2^n - 1
   *  (which satisfies m >= b - 1).
   */
  b1 = b - 1;
  n = 1;
  m = 1;
  while(m < b1) {
    n++;
    m = (m<<1) | 1;
  }
  *pn = n;

  /*  Calculate the "pseudo-inverse" of b, which is
   *  the ceiling of 2^(n+maxbits_a) divided by b, or
   *     d = 1 + (2^(n+maxbits_a) - 1) / b
   *  Because 2^n >=  b, d >= 2^maxbits_a, and
   *  because 2^n < 2*b, d <  2^(maxbits_a+1).
   *  Therefore d occupies (maxbits_a+1) bits,
   *  and its top bit is 1.
   *  Return value is:
   *     d         if maxbits_a  < BPUL (bits per unsigned long)
   *     d-2^BPUL  if maxbits_a == BPUL (i.e., all but top bit)
   */
  for(q=i=0; i<=maxbits_a; i++) {
    q <<= 1;
    if(m >= b) {
      m -= b;
      q |= 1;
    }
    m = (m<<1) | 1;
  }
  return 1+q;
}


/* Expand the sequence for division by a constant. It is the caller's
 * job to verify that the divisor is non-zero.
 */
static BOOL
Expand_Integer_Divide_By_Constant(TN *result, TN *numer_tn, INT64 denom_val,
				  TYPE_ID mtype, OPS *ops)
{
  UINT64 b;				// b = |denom_val|
  UINT64 d;      			// division scaling factor
  INT64  precision_required;
  INT64  n;
  BOOL is_odd;
  TN *d_tn;
  TN *mult_tn;
  TN *shift_tn;
  TOP opc;
  TN *p1;
  BOOL is_double = MTYPE_is_size_double(mtype);
  BOOL is_signed = MTYPE_is_signed(mtype);

  /* Handle the trivial ones:
   */
  if (denom_val == 1) {
    Exp_COPY(result, numer_tn, ops);
    return TRUE;
  } else if (is_signed && denom_val == -1) {
    Expand_Neg(result, numer_tn, mtype, ops);
    return TRUE;
  }

  /* Look for simple shift optimizations:
   */
  if (Is_Power_Of_2( denom_val, mtype)) {
    Expand_Power_Of_2_Divide(result, numer_tn, denom_val, mtype, ops);
    return TRUE;
  }

  if (!CGEXP_cvrt_int_div_to_mult) return FALSE;

  if (is_signed) {

    b = denom_val<0 ? -denom_val : denom_val;       // b = |denom_val|
    is_odd = (b&1);

    d = determine_pseudo_inverse (b, is_double ? 63 : 31, &n);

    if (n > (is_double ? 63 : 31)) {
      /* OOPS! The shift count can't be bigger than the word size! */
      return FALSE;
    }

    d_tn = Build_TN_Like (result);
    Expand_Immediate (d_tn, Gen_Literal_TN (d, 8), is_signed, ops);

    /* Generate the absolute value of the numerator:
     */
    TN *neg_tn = Build_TN_Of_Mtype (mtype);
    p1 = Build_TN_Like (result);
    if (is_double)
      Build_OP (TOP_dsubu, neg_tn, Zero_TN, numer_tn, ops);
    else
      Build_OP (TOP_subu, neg_tn, Zero_TN, numer_tn, ops);      
    if (is_double) 
      Build_OP (TOP_dsrl32, p1, numer_tn, Gen_Literal_TN(31, 4), ops);
    else
      Build_OP (TOP_srl, p1, numer_tn, Gen_Literal_TN(31, 4), ops);
    Build_OP (TOP_movz, neg_tn, numer_tn, p1, ops);
    Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);

    /* Generate a multiply upper:
     */
    mult_tn = Build_TN_Of_Mtype (mtype);
    if (denom_val > 0) {
      Expand_High_Multiply (mult_tn, neg_tn, d_tn, 
			    is_double?MTYPE_U8:MTYPE_U4, ops);
      
      /* Generate and attach the shift:
       */
      if (n > 0) {
	shift_tn = Build_TN_Of_Mtype (mtype);
	if (is_double) 
	  Build_OP (((n - 1)>31)?TOP_dsra32:TOP_dsra, 
		    shift_tn, mult_tn, Gen_Literal_TN ((n-1)%32, 4), ops);
	else
	  Build_OP (TOP_sra, 
		    shift_tn, mult_tn, Gen_Literal_TN ((n-1)%32, 4), ops);
      } else {
	shift_tn = mult_tn;
      }
      
      /* Select positive or negated result:
       */
      if (is_double)
	Build_OP (TOP_dsubu, result, Zero_TN, shift_tn, ops);
      else
	Build_OP (TOP_subu, result, Zero_TN, shift_tn, ops);
      Build_OP (TOP_movz, result, shift_tn, p1, ops);
      Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
    } else {
      Expand_High_Multiply (result, neg_tn, d_tn, 
			    is_double?MTYPE_U8:MTYPE_U4, ops);
      
      /* Generate and attach the shift:
       */
      if (n > 0) {
	shift_tn = Build_TN_Of_Mtype (mtype);
	if (is_double) 
	  Build_OP (((n - 1)>31)?TOP_dsra32:TOP_dsra, 
		    result, result, Gen_Literal_TN ((n-1)%32, 4), ops);
	else
	  Build_OP (TOP_sra, 
		    result, result, Gen_Literal_TN ((n-1)%32, 4), ops);
      } 
      
      /* Select positive or negated result:
       */
      if (is_double)
	Build_OP (TOP_dsubu, mult_tn, Zero_TN, result, ops);
      else
	Build_OP (TOP_subu, mult_tn, Zero_TN, result, ops);
      Build_OP (TOP_movz, result, mult_tn, p1, ops);
      Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);      
    }
  } /* end Signed */

  else { /* Unsigned */

    b = denom_val;
    is_odd = (b&1);

    /* Full precision calculation is required.
     */
    if (is_odd) {
      precision_required = is_double?64:32;
    } else {
      
      /* Pre-shift the numerator and denominator so that
	 one less bit is required in the calculation. Then
	 we can avoid the subtract-shift-add after the 
	 multiply operation. */
      b >>= 1;
      precision_required = is_double?63:31;
	
      /* Pre-shift to simplify later calculations.
       */
      TN *tmp1_tn = Build_TN_Of_Mtype (mtype);
      Build_OP (is_double?TOP_dsrl:TOP_srl, tmp1_tn, numer_tn, Gen_Literal_TN (1, 4), ops);
      numer_tn = tmp1_tn;
    }

    d = determine_pseudo_inverse (b, precision_required, &n);

    if (n > precision_required) {
      /* OOPS! The shift count can't be bigger than the word size! */
      return FALSE;
    }

    d_tn = Build_TN_Like (result);

    Expand_Immediate (d_tn, Gen_Literal_TN (d, 8), is_signed, ops);

    /* Generate a multiply upper:
     */
    mult_tn = Build_TN_Of_Mtype (mtype);
    
    Expand_High_Multiply (mult_tn, numer_tn, d_tn, 
			  is_double?MTYPE_U8:MTYPE_U4, ops);

    if (precision_required == 64 || precision_required == 32) {

      /* Odd divisors need full precision and, hence, extra instructions.
       */
      TN *tmp1_tn = Build_TN_Of_Mtype (mtype);
      TN *tmp2_tn = Build_TN_Of_Mtype (mtype);
      TN *tmp3_tn = Build_TN_Of_Mtype (mtype);
      if (is_double) {
	Build_OP (TOP_dsubu, tmp1_tn, numer_tn, mult_tn, ops);
	Build_OP (TOP_dsrl, tmp2_tn, tmp1_tn, Gen_Literal_TN (1, 4), ops);
	Build_OP (TOP_daddu, tmp3_tn, mult_tn, tmp2_tn, ops);
      }
      else {
	Build_OP (TOP_subu, tmp1_tn, numer_tn, mult_tn, ops);
	Build_OP (TOP_srl, tmp2_tn, tmp1_tn, Gen_Literal_TN (1, 4), ops);
	Build_OP (TOP_addu, tmp3_tn, mult_tn, tmp2_tn, ops);
      }
      mult_tn = tmp3_tn;
    }

    /* Generate and attach the shift:
     */
    if (is_double)
      Build_OP (((n-1)>31)?TOP_dsrl32:TOP_dsrl, 
		result, mult_tn, Gen_Literal_TN ((n-1)%32, 4), ops);    
    else
      Build_OP (TOP_srl, result, mult_tn, Gen_Literal_TN ((n-1)%32, 4), ops);    
  } /* end Unsigned */

  return TRUE;
}


/* Expand the sequence for remainder of a power of two. It is the
 * caller's job to verify that divisor is a non-zero power of two.
 *
 * Expand rem(x, [+-]2^n) as follows:
 *
 *	Using the identities
 *		rem(x,y) =  rem( x,-y)
 *		rem(x,y) = -rem(-x, y)
 *
 *	unsigned
 *	f=	x & MASK(n)
 *
 *	signed
 *	f=	x & MASK(n)		x>=0
 *	f=	-(-x & MASK(n))		x<0
 */
static void 
Expand_Power_Of_2_Rem (TN *result, TN *src1, INT64 src2_val, TYPE_ID mtype, OPS *ops)
{
  BOOL is_double = MTYPE_is_size_double(mtype);
  INT n = Get_Power_Of_2(src2_val, mtype);
  INT64 nMask = (1LL << n) - 1;
  TN *con = Gen_Literal_TN(nMask, is_double ? 8 : 4);

  if (MTYPE_is_signed(mtype)) {
    TN *tmp1, *tmp2;
    TN *p1;
    TOP opc;

#if defined(TARG_SL)
    // tmp1 = src1 >= 0 ? src1 : -src1;
    tmp1 = Build_TN_Of_Mtype(mtype);
    Build_OP(TOP_mc_abs, tmp1, src1, ops);
    Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_UNC_DEF);
#else
    /* Test sign of src1
     */
    p1 = Build_TN_Of_Mtype(MTYPE_I4);
    Build_OP(TOP_slt, p1, src1, Zero_TN, ops);

    /* Get absolute value of src1
     */
    tmp1 = Build_TN_Of_Mtype(mtype);
    if (is_double) 
      Build_OP(TOP_dsubu, tmp1, Zero_TN, src1, ops);
    else
      Build_OP(TOP_subu, tmp1, Zero_TN, src1, ops);
    Build_OP(TOP_movz, tmp1, src1, p1, ops);
    Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
#endif

    /* Perform the AND
     */
    tmp2 = Build_TN_Of_Mtype(mtype);
    Expand_Binary_And(tmp2, tmp1, con, mtype, ops);

    /* Negate the result if src1 was negative
     */
#if defined(TARG_SL)
    // result = (src1 >= 0) ? tmp2 : -tmp2;
    int unsignedflag = (mtype == MTYPE_U4) ? 1 : 0;
    Is_True((mtype == MTYPE_U4 || mtype==MTYPE_I4) ,
            (" Expand_Power_Of_2_Rem:: mtype must be MTYPE_I4 or MTYPE_U4" )); 
    Build_MC_OP(TOP_mc_zn_ge, result, src1, tmp2, unsignedflag, ops, OP_ALWAYS_UNC_DEF);
#else
    if (is_double) 
      Build_OP(TOP_dsubu, result, Zero_TN, tmp2, ops);
    else
      Build_OP(TOP_subu, result, Zero_TN, tmp2, ops);
    Build_OP(TOP_movz, result, tmp2, p1, ops);
    Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
#endif
  } else {
    Expand_Binary_And(result, src1, con, mtype, ops);
  }
}


/* Expand the sequence for mod of a power of two. It is the
 * caller's job to verify that divisor is a non-zero power of two.
 *
 * Expand mod(x, [+-]2^n) as follows:
 *
 *	Using the identity
 *		mod(x,y) = -mod(-x, y)
 *	
 *	f=	x & MASK(n)		(2^n > 0) || unsigned
 *
 *	t=	-(-x & MASK(n))		(2^n < 0)
 *
 *	Special case for n=31 and n=63 return zero
 */
static void 
Expand_Power_Of_2_Mod (TN *result, TN *src1, INT64 src2_val, TYPE_ID mtype, OPS *ops)
{
  BOOL is_double = MTYPE_is_size_double(mtype);
  INT64 absval = src2_val < 0 ? -src2_val : src2_val;
  INT	n      = Get_Power_Of_2(absval, mtype);
  INT64	nMask  = (1LL << n) - 1;
  TN	*con   = Gen_Literal_TN(nMask, is_double ? 8 : 4);

  if (MTYPE_is_signed(mtype) && src2_val < 0) {
    TN *tmp1, *tmp2;

    tmp1 = Build_TN_Of_Mtype(mtype);
    Expand_Neg(tmp1, src1, mtype, ops);

    tmp2 = Build_TN_Of_Mtype(mtype);
    Expand_Binary_And(tmp2, tmp1, con, mtype, ops);

    Expand_Neg(result, tmp2, mtype, ops);
  } else {
    Expand_Binary_And(result, src1, con, mtype, ops);
  }
}

/*****************************************************************************
 *
 * Integer division external interfaces
 *
 *****************************************************************************/

TN *
Expand_Divide (TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  /* Look for simple shift optimizations and multiply_hi optimizations:
   */
  INT64 src2_val;
  BOOL const_src2 = TN_Value_At_Op (src2, NULL, &src2_val);
  if (const_src2) {
    if (src2_val == 0) // Division by Zero!
      FmtAssert (FALSE, ("Division by zero detected.\n"));
    if (Expand_Integer_Divide_By_Constant(result, src1, src2_val, mtype, ops)) {
      return NULL; // no hilo
    }
  }

  if (MTYPE_signed(mtype) && !MTYPE_is_size_double(mtype)) {
    // R1000 (octane)
    //src2 could be a constant and we did not succeed 
    //Expand_Integer_Divide_By_Constant
    TN *tmp = Build_TN_Of_Mtype (mtype);
    UINT success = 1;
    if (!CGEXP_cvrt_int_div_to_mult && const_src2 && TN_is_constant(src2)) {
      success = 0;
      tmp = Expand_Immediate_Into_Register (src2, mtype, ops);
    }
    // To minimize diff with Octane cc
    TN *t1 = Build_TN_Of_Mtype(MTYPE_F8);
    TN *t2 = Build_TN_Of_Mtype(MTYPE_F8);
    TN *t3 = Build_TN_Of_Mtype(MTYPE_F8);
    TN *t4 = Build_TN_Of_Mtype(MTYPE_F8);
    TN *t5 = Build_TN_Of_Mtype(MTYPE_F8);
    TN *t6 = Build_TN_Of_Mtype(MTYPE_F8);
    Build_OP(TOP_mtc1, t1, src1, ops);
    Build_OP(TOP_mtc1, t2, (success == 1)? src2: tmp, ops);
    Build_OP(TOP_cvt_d_w, t3, t1, ops);
    Build_OP(TOP_cvt_d_w, t4, t2, ops);
    Build_OP(TOP_div_d, t5, t3, t4, ops);
    Build_OP(TOP_trunc_w_d, t6, t5, ops);
    Build_OP(TOP_mfc1, result, t6, ops);
    Build_OP(TOP_teq, (success == 1)? src2: tmp, 
	     Zero_TN, Gen_Literal_TN(7, 4), ops);
    return NULL;
  }

  TOP top;
  FmtAssert(!TN_is_constant(src1),("Expand_Divide: unexpected constant operand"));
  if (! MTYPE_is_size_double(mtype))
    top = MTYPE_signed(mtype) ? TOP_div : TOP_divu;
  else top = MTYPE_signed(mtype) ? TOP_ddiv : TOP_ddivu;
  if (TN_is_constant(src2))
    src2 = Expand_Immediate_Into_Register(src2, MTYPE_is_size_double(mtype), ops);
  Build_OP(top, Hilo_TN(), src1, src2, ops);
#if !defined(TARG_SL)
  Build_OP(TOP_teq, src2, Zero_TN, Gen_Literal_TN(7, 4), ops);
#else
  DevWarn("Handle Division by zero");
#endif
  Build_OP(TOP_mflo, result, Hilo_TN(), ops);
}


void
Expand_Rem (TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  TOP top;
  BOOL is_double = MTYPE_is_size_double(mtype);

  /* Try to optimize when constant divisor.
   */
  INT64 src2_val;
  BOOL const_src2 = TN_Value_At_Op (src2, NULL, &src2_val);
  if (const_src2) {

    /* Handle powers of 2 specially.
     */
    if (Is_Power_Of_2(src2_val, mtype)) {
      Expand_Power_Of_2_Rem(result, src1, src2_val, mtype, ops);
      return;
    }

    if (CGEXP_cvrt_int_div_to_mult) {
      TN *div_tn = Build_TN_Like (result);

      if (Expand_Integer_Divide_By_Constant(div_tn, src1, src2_val, mtype, ops)) {
	TN *mult_tn;

	/* Generate a multiply:
	 */
	mult_tn = Build_TN_Like (result);
	Expand_Multiply(mult_tn, div_tn, src2, mtype, ops);

	/* Subtract the result of the multiply from the original value.
	 */
	if (is_double)
	  Build_OP(TOP_dsubu, result, src1, mult_tn, ops);
	else
	  Build_OP(TOP_subu, result, src1, mult_tn, ops);
	return;
      }
    }
  }

  FmtAssert(!TN_is_constant(src1),("Expand_Rem: unexpected constant operand"));
  if (! MTYPE_is_size_double(mtype))
    top = MTYPE_signed(mtype) ? TOP_div : TOP_divu;
  else top = MTYPE_signed(mtype) ? TOP_ddiv : TOP_ddivu;
  if (TN_is_constant(src2))
    src2 = Expand_Immediate_Into_Register(src2, MTYPE_is_size_double(mtype), ops);
  Build_OP(top, Hilo_TN(), src1, src2, ops);
#if !defined(TARG_SL)
  Build_OP(TOP_teq, src2, Zero_TN, Gen_Literal_TN(7, 4), ops);
#else
  DevWarn("Handle Division by zero");
#endif
  Build_OP(TOP_mfhi, result, Hilo_TN(), ops);
}


/*	Expand mod(x,y) as follows:
 *		t1=	rem(x,y)
 *		t2=	xor(t1,y)
 *		t3,t4=	cmp.lt(t2,0)
 *	  if t3 r=	t1+y
 *	  if t4 r=	t1
 */
void 
Expand_Mod (TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  TN *tmp1;
  TN *tmp2;
  TN *tmp3;
  TN *p1;
  TOP opc;
  BOOL is_double = MTYPE_is_size_double(mtype);

  /* Handle mod by power of 2 specially
   */
  INT64 src2_val;
  BOOL const_src2 = TN_Value_At_Op (src2, NULL, &src2_val);
  if (const_src2 && Is_Power_Of_2(src2_val, mtype)) {
    Expand_Power_Of_2_Mod (result, src1, src2_val, mtype, ops);
    return;
  }

  if (const_src2)
    if (src2_val == 0) // Division by Zero!
      FmtAssert (FALSE, ("Division by zero detected.\n"));

  /* Calculate remainder 
   */
  tmp1 = Build_TN_Like(result);
  Expand_Rem(tmp1, src1, src2, mtype, ops);

  /* Are signs different? 
   */
  tmp2 = Build_TN_Like(result);
  Build_OP(TOP_xor, tmp2, tmp1, src2, ops);

  p1 = Gen_Typed_Register_TN(MTYPE_I4, 4);
  Build_OP(TOP_slt, p1, tmp2, Zero_TN, ops);

  /* result = divisor + remainder if p1
   * result = remainder if ~p1
   */
  tmp3 = Build_TN_Like(result);
  Build_OP(TOP_movn, tmp3, src2, p1, ops);
  Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
  Build_OP(TOP_movz, tmp3, Zero_TN, p1, ops);
  Set_OP_cond_def_kind(OPS_last(ops), OP_ALWAYS_COND_DEF);
  if (is_double)
    Build_OP(TOP_daddu, result, tmp1, tmp3, ops);
  else
    Build_OP(TOP_addu, result, tmp1, tmp3, ops);
}


void 
Expand_DivRem(TN *result, TN *result2, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  BOOL is_double = MTYPE_is_size_double(mtype);

  /* Usually we expect whirl operators to be folded where possible.
   * But divrem is an odd beast in that the result is a special
   * "handle" rather than a value. There is no way to produce constants.
   * Therefore in some odd instances we can get constant operands,
   * so fold them here, avoiding nasty trapping issues.
   */
  INT64 src1_val;
  BOOL const_src1 = TN_Value_At_Op (src1, NULL, &src1_val);
  INT64 src2_val;
  BOOL const_src2 = TN_Value_At_Op (src2, NULL, &src2_val);

  if (const_src2)
    if (src2_val == 0) // Division by Zero!
      FmtAssert (FALSE, ("Division by zero detected.\n"));
  if (const_src1 && const_src2) {
    if (MTYPE_is_signed(mtype)) {
      const INT64 minintval = is_double ? INT64_MIN : INT32_MIN;
      if (src1_val == minintval && src2_val == -1)  
	FmtAssert (FALSE, ("Division overflow detected.\n"));
    }
    INT64 quot_val, rem_val;
    switch (mtype) {
    case MTYPE_I8:
      quot_val = (INT64)src1_val / (INT64)src2_val;
      rem_val = (INT64)src1_val % (INT64)src2_val;
      break;
    case MTYPE_U8:
      quot_val = (UINT64)src1_val / (UINT64)src2_val;
      rem_val = (UINT64)src1_val % (UINT64)src2_val;
      break;
    case MTYPE_U4:
      quot_val = (UINT32)src1_val / (UINT32)src2_val;
      rem_val = (UINT32)src1_val % (UINT32)src2_val;
      break;
    case MTYPE_I4:
      quot_val = (INT32)src1_val / (INT32)src2_val;
      rem_val = (INT32)src1_val % (INT32)src2_val;
      break;
    }
    BOOL is_signed = MTYPE_is_signed(mtype);
    INT tn_size = MTYPE_is_size_double(mtype) ? 8 : 4;
    Exp_Immediate(result, Gen_Literal_TN(quot_val, tn_size), is_signed, ops);
    Exp_Immediate(result2, Gen_Literal_TN(rem_val, tn_size), is_signed, ops);
    return;
  }

  /* Look for simple shift optimizations and multiply_hi optimizations:
   */
  if (const_src2) {
    if (Expand_Integer_Divide_By_Constant(result, src1, src2_val, mtype, ops)) {

      // Now get the rem part. Since the constant value is probably small, 
      // we are unlikely to generate a multiply here (i.e., we'll probably
      // generate shifts)
      //
      if (!MTYPE_is_signed(mtype) && Is_Power_Of_2(src2_val, mtype)) {
        Expand_Power_Of_2_Rem(result2, src1, src2_val, mtype, ops);
      } else {
	TN *t1 = Build_TN_Like(result);
	Expand_Multiply(t1, result, src2, mtype, ops);
	if (is_double)
	  Build_OP(TOP_dsubu, result2, src1, t1, ops);
	else
	  Build_OP(TOP_subu, result2, src1, t1, ops);
      }
      return;
    }
  }

  TOP top;  	 
  if (! MTYPE_is_size_double(mtype))
    top = MTYPE_signed(mtype) ? TOP_div : TOP_divu;
  else top = MTYPE_signed(mtype) ? TOP_ddiv : TOP_ddivu;
  if (TN_is_constant(src2))
    src2 = Expand_Immediate_Into_Register(src2, MTYPE_is_size_double(mtype), ops);
  Build_OP(top, Hilo_TN(), src1, src2, ops);
#if !defined(TARG_SL)
  Build_OP(TOP_teq, src2, Zero_TN, Gen_Literal_TN(7, 4), ops);
#else
  DevWarn("Handle Division by zero");
#endif
  Build_OP(TOP_mflo, result, Hilo_TN(), ops);
  Build_OP(TOP_mfhi, result2, Hilo_TN(), ops);
}
