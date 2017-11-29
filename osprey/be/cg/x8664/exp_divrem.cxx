/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2007. QLogic Corporation. All Rights Reserved.
 */

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


/* CGEXP routines for expanding divide and rem */

#include <stdint.h>
#include <signal.h>
#include "pathscale_defs.h"
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
#include "whirl2ops.h"

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

static BOOL Is_Power_Of_2(INT64 val, TYPE_ID mtype)
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
Expand_Power_Of_2_Divide (TN *result, TN *numer, INT64 dvsr, 
			  TYPE_ID mtype, OPS *ops)
{
  INT n = Get_Power_Of_2(dvsr, mtype);

  if (MTYPE_is_unsigned(mtype)) {
    Expand_Shift(result, numer, Gen_Literal_TN(n, 4), mtype, 
		 shift_lright, ops);
  } else {
    TN *t1 = Build_TN_Of_Mtype(mtype);
    INT64 absdvsr = dvsr < 0 ? -dvsr : dvsr;
    BOOL is_double = MTYPE_is_size_double(mtype);
    TN *t2 = Build_TN_Of_Mtype(mtype);
    TN *t3 = Build_TN_Of_Mtype(mtype);
    TN *t4 = Build_TN_Of_Mtype(mtype);
    TN *t5 = (dvsr < 0) ? Build_TN_Of_Mtype(mtype) : result;
    Expand_Shift(t1, numer, Gen_Literal_TN(is_double?63:31, 4), mtype, 
		    shift_aright, ops);
    Expand_Immediate (t2, Gen_Literal_TN (absdvsr - 1, is_double?8:4), 
		    FALSE, ops);
    Expand_Binary_And( t3, t1, t2, mtype, ops );
    Expand_Add( t4, t3, numer, mtype, ops );
    Expand_Shift(t5, t4, Gen_Literal_TN(n, 4), mtype, 
		 shift_aright, ops);
    if (dvsr < 0) Expand_Neg(result, t5, mtype, ops);
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
  BOOL m_overflow = FALSE;
  for(q=i=0; i<=maxbits_a; i++) {
    q <<= 1;
    if(m_overflow) {
      // because ((m>>1) | 0x8000000000000000ULL) >= m
      // if m>=b in this iteration, then in last iteration m also >= b. 
      // This can't happen
      Is_True(m < b, ("m bigger than b and m is overflow in last iteration\n"));
      m -= b;
      q |= 1;
      m_overflow = FALSE;
    }
    else if(m >= b) {
      m -= b;
      q |= 1;
    }
    // After subtraction, m must be smaller than b. And m's 64 bit MSB must be zero.
    // if m's 64bit MSB is 1, then subtraction not happen in this iteration.
    // it means b>m, then b's 64 bit MSB is also 1.
    // Mark m overflow and in next iteration, actually m is bigger than b.
    // Need do subtraction in next itration.
    if (m & 0x8000000000000000ULL) {
      Is_True(b & 0x8000000000000000ULL, ("b's 64th bit must be 1\n"));
      m_overflow = TRUE;
    }
    m = (m<<1) | 1;
  }
  return 1+q;
}


static UINT log2_uint (UINT i)
{
  UINT t = 0;
  i = i >> 1;
  while(i) {
    i = i >> 1;
    t ++;
  }
  return t;  
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

  if (!is_double && CG_idivbyconst_opt) {
    if (is_signed) {
      UINT64 d = labs((UINT64)denom_val);
      UINT l = log2_uint((UINT)d);
      INT64 e = denom_val;
      UINT s, a;
      UINT64 m;
      UINT64 m_low, m_high, j, k;
      UINT shift_amt = 31;

      j = (((UINT64)(0x80000000)) % ((UINT64)(d)));
      k = (((UINT64)(1)) << (32 + l)) / ((UINT64)(0X80000000 - j));
      m_low = (((UINT64)(1)) << (32 + l)) / d;
      m_high = ((((UINT64)(1)) << (32 + l)) + k) / d;
      
      while (((m_low >> 1) < (m_high >> 1)) && (l > 0)) {
	m_low = m_low >> 1;
	m_high = m_high >> 1;
	l = l - 1;
      }
      m = ((UINT)(m_high));
      s = l;
      a = (m_high >> 31) ? 1: 0;

      if (is_double) {
	m |= (m -1)<<32;
	shift_amt = 63;
      }

      if (a) {
	TN* tmp1_tn = Build_TN_Of_Mtype(MTYPE_I8);
	if (!is_double)
	  Expand_Convert_Length(tmp1_tn, numer_tn, Gen_Literal_TN(32, 4), 
				MTYPE_I8, TRUE, ops);
	else
	  Expand_Copy( tmp1_tn, numer_tn, mtype, ops);
	mult_tn = Build_TN_Of_Mtype(MTYPE_I8);
	d_tn = Build_TN_Of_Mtype(MTYPE_I8);
	Expand_Immediate (d_tn, 
			  Gen_Literal_TN (!is_double ? (INT)m : m, 8), 
			  is_signed, ops);
	if (!is_double) {
	  Expand_Multiply(mult_tn, tmp1_tn, d_tn, MTYPE_I8, ops);
	  shift_tn = Build_TN_Of_Mtype(MTYPE_I8);
	  Expand_Shift(shift_tn, mult_tn, Gen_Literal_TN(32, 4), 
		       MTYPE_I8, shift_lright, ops);
	} else {
	  Expand_High_Multiply(mult_tn, tmp1_tn, d_tn, MTYPE_I8, ops);
	  shift_tn = mult_tn;
        }
	TN* tmp2_tn = Build_TN_Of_Mtype(mtype);
	Expand_Add(tmp2_tn, shift_tn, numer_tn, mtype, ops);
	TN* tmp3_tn = Build_TN_Of_Mtype(mtype);
	Expand_Shift(tmp3_tn, numer_tn, Gen_Literal_TN(shift_amt, 4), 
		mtype, shift_aright, ops);
	TN* tmp4_tn = Build_TN_Like(result);
	Expand_Shift(tmp4_tn, tmp2_tn, Gen_Literal_TN(s, 4),
		mtype, shift_aright, ops);
	Expand_Sub(result, tmp4_tn, tmp3_tn, mtype, ops);
	// Bug 1447 
	if (e > 0)
	  Expand_Sub(result, tmp4_tn, tmp3_tn, mtype, ops);
	else
	  Expand_Sub(result, tmp3_tn, tmp4_tn, mtype, ops);
         
      } else if( Is_Target_32bit() ){
	TN* eax_tn = Build_TN_Of_Mtype(mtype);
	Expand_Immediate( eax_tn, Gen_Literal_TN(m,4), is_signed, ops );

	TN* edx_tn = Build_TN_Like( result );
	Build_OP( TOP_imulx32, eax_tn, edx_tn, eax_tn, numer_tn, ops );

	Expand_Copy( eax_tn, numer_tn, mtype, ops );

	if( s ){
	  Expand_Shift( edx_tn, edx_tn, Gen_Literal_TN(s, 4), 
			mtype, shift_aright, ops);
	}

	Expand_Shift( eax_tn, eax_tn, Gen_Literal_TN(shift_amt, 4), 
		      mtype, shift_lright, ops);

	Expand_Add( edx_tn, edx_tn, eax_tn, mtype, ops );

	if( e < 0 ){
	  Expand_Neg( edx_tn, edx_tn, mtype, ops );
	}

	Expand_Copy( result, edx_tn, mtype, ops );

      } else {	
	TN* tmp1_tn = Build_TN_Of_Mtype(MTYPE_I8);
	if (!is_double)
	  Expand_Convert_Length(tmp1_tn, numer_tn, Gen_Literal_TN(32, 4), 
			      MTYPE_I8, TRUE, ops);
	else
	  Expand_Copy( tmp1_tn, numer_tn, mtype, ops);
	TN* tmp2_tn = Build_TN_Of_Mtype(mtype);
	Expand_Shift(tmp2_tn, numer_tn, Gen_Literal_TN(shift_amt, 4), 
		     mtype, shift_aright, ops);
	mult_tn = Build_TN_Of_Mtype(MTYPE_I8);
	d_tn = Build_TN_Of_Mtype(MTYPE_I8);
	Expand_Immediate (d_tn, Gen_Literal_TN (m, 8), is_signed, ops);
	if (!is_double) {
	  Expand_Multiply(mult_tn, tmp1_tn, d_tn, MTYPE_I8, ops);
	  shift_tn = Build_TN_Of_Mtype(MTYPE_I8);
	  Expand_Shift(shift_tn, mult_tn, Gen_Literal_TN(32, 4), 
		       MTYPE_I8, shift_lright, ops);
	} else {
	  Expand_High_Multiply(mult_tn, tmp1_tn, d_tn, MTYPE_I8, ops);
	  shift_tn = mult_tn;
	}
	TN* tmp3_tn = Build_TN_Like(result);
	if (s) 
	  Expand_Shift(tmp3_tn, shift_tn, Gen_Literal_TN(s, 4), 
		       mtype, shift_aright, ops);
	else
	  Expand_Copy(tmp3_tn, shift_tn, mtype, ops);

	if( e < 0 ) 
	  // Bug 1214
	  Expand_Sub(result, tmp2_tn, tmp3_tn, mtype, ops);
	else
	  Expand_Sub(result, tmp3_tn, tmp2_tn, mtype, ops);
      }
      return TRUE;
    } else if ((UINT)denom_val < 0x80000000UL) { // !is_signed and special case
      UINT l, n, t, d, m, s, a, r;
      UINT64 m_low, m_high, j, k;

      n = 0;
      t = d = (UINT)denom_val;

      while (!(t & 1)) {
	t >>= 1;
	n++;
      }
      
      l = log2_uint(t) + 1;
      j = (((UINT64)(0xffffffff)) % ((UINT64)(t)));
      k = (((UINT64)(1)) << (32 + l)) / ((UINT64)(0xffffffff - j));
      m_low = (((UINT64)(1)) << (32 + l)) / t;
      m_high = ((((UINT64)(1)) << (32 + l)) + k) / t;
      while (((m_low >> 1) < (m_high >> 1)) && (l > 0)) {
	m_low = m_low >> 1;
	m_high = m_high >> 1;
	l = l - 1;
      }
      if ((m_high >> 32) == 0) {
	m = ((UINT)(m_high));
	s = l;
	a = 0;
      }
      else {
	s = log2_uint(t);
	m_low = (((UINT64)(1)) << (32 + s)) / ((UINT64)(t));
	r = ((UINT)((((UINT64)(1)) << (32 + s)) % ((UINT64)(t))));
	m = (r < ((t >> 1) + 1)) ? ((UINT)(m_low)) : ((UINT)(m_low)) + 1;
	a = 1;
      }

      s += n;

      if (a) {
	TN* tmp1_tn = Build_TN_Of_Mtype(MTYPE_I8);
	TN* tmp2_tn = Build_TN_Like(result);
	TN* tmp3_tn = Build_TN_Like(result);
	TN* tmp4_tn = Build_TN_Like(result);
	m <<= 1;
	m ++;
	d_tn = Build_TN_Of_Mtype(MTYPE_I8);
	Expand_Immediate(d_tn, Gen_Literal_TN (m, 8), is_signed, ops);
	mult_tn = Build_TN_Of_Mtype(MTYPE_I8);
	Expand_Multiply(mult_tn, numer_tn, d_tn, MTYPE_I8, ops);	
	Expand_Shift(tmp1_tn, mult_tn, Gen_Literal_TN(32, 4), 
		     MTYPE_I8, shift_lright, ops);	
	Expand_Sub(tmp2_tn, numer_tn, tmp1_tn, mtype, ops);
	Expand_Shift(tmp3_tn, tmp2_tn, Gen_Literal_TN(1, 4), 
		     mtype, shift_lright, ops);		
	Expand_Add(tmp4_tn, tmp3_tn, tmp1_tn, mtype, ops);
	Expand_Shift(result, tmp4_tn, Gen_Literal_TN(s, 4), 
		     mtype, shift_lright, ops);		
      } else {
	TN* tmp1_tn = Build_TN_Of_Mtype(MTYPE_I8);
	d_tn = Build_TN_Of_Mtype(MTYPE_I8);
	Expand_Immediate (d_tn, Gen_Literal_TN (m, 8), is_signed, ops);
	mult_tn = Build_TN_Of_Mtype(MTYPE_I8);
	Expand_Multiply(mult_tn, numer_tn, d_tn, MTYPE_I8, ops);	
	Expand_Shift(tmp1_tn, mult_tn, Gen_Literal_TN(32, 4), 
		     MTYPE_I8, shift_lright, ops);
	Expand_Shift(result, tmp1_tn, Gen_Literal_TN(s, 4), 
		     mtype, shift_lright, ops);
      }
      return TRUE;
    } else {
      // Bug 1214
      TN* tmp1_tn = Build_TN_Of_Mtype(MTYPE_I4);
      TN* tmp2_tn = Build_TN_Of_Mtype(MTYPE_I4);
      TN* tmp3_tn = Build_TN_Of_Mtype(MTYPE_I4);
      TN* rflags = Rflags_TN();
      Expand_Immediate (tmp1_tn, Gen_Literal_TN ((INT)denom_val, 4), 
			is_signed, ops);
      Build_OP(TOP_cmp32, rflags, numer_tn, tmp1_tn, ops);
      Expand_Immediate(tmp2_tn, Gen_Literal_TN (0, 4), is_signed, ops);
      Expand_Immediate(tmp3_tn, Gen_Literal_TN(-1, 4), is_signed, ops);
      Build_OP(TOP_sbb32, result, tmp2_tn, tmp3_tn, ops);
      return TRUE;
    }
  }

  if (is_signed) {

    b = denom_val<0 ? -denom_val : denom_val;       // b = |denom_val|
    is_odd = (b&1);

    d = determine_pseudo_inverse (b, is_double?63:31, &n);

    if (n > (is_double ? 63 : 31)) {
      /* OOPS! The shift count can't be bigger than the word size! */
      return FALSE;
    }

    d_tn = Build_TN_Like (result);
    Expand_Immediate (d_tn, Gen_Literal_TN (d, 8), is_signed, ops);

    /* Generate the absolute value of the numerator:
     */
    TN *neg_tn = Build_TN_Of_Mtype (mtype);
    p1 = Rflags_TN();
    Expand_Neg (neg_tn, numer_tn, mtype, ops);
    Expand_Cmov (TOP_cmovs, neg_tn, numer_tn, p1, ops);

    /* Generate a multiply upper:
     */
    mult_tn = Build_TN_Of_Mtype (mtype);
    Expand_High_Multiply (mult_tn, neg_tn, d_tn, 
			  is_double?MTYPE_U8:MTYPE_U4, ops);
    
    /* Generate and attach the shift:
     */
    if (n > 0) {
      shift_tn = Build_TN_Of_Mtype (mtype);
      Expand_Shift(shift_tn, mult_tn, Gen_Literal_TN (n-1, 4), 
		   mtype, shift_aright, ops);
    } else {
      shift_tn = mult_tn;
    }
    
    /* Select positive or negated result:
     */
    if (shift_tn == result) {
      TN *tmp_result = Build_TN_Like(shift_tn);
      Expand_Copy( tmp_result, shift_tn, mtype, ops);
      shift_tn = tmp_result;
    } else if (result == numer_tn) {
      TN *tmp_numer = Build_TN_Like(numer_tn);
      Expand_Copy( tmp_numer, numer_tn, mtype, ops);
      numer_tn = tmp_numer;
    }
    Expand_Neg(result, shift_tn, mtype, ops);    
    Build_OP(is_double?TOP_test64:TOP_test32, p1, 
	     numer_tn, numer_tn, ops);
    if (denom_val > 0)
      Expand_Cmov (TOP_cmovge, result, shift_tn, p1, ops);
    else
      Expand_Cmov (TOP_cmovl, result, shift_tn, p1, ops);
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
      Expand_Shift(tmp1_tn, numer_tn, Gen_Literal_TN (1, 4), 
		   mtype, shift_lright, ops);
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
      Expand_Sub (tmp1_tn, numer_tn, mult_tn, mtype, ops);
      Expand_Shift (tmp2_tn, tmp1_tn, Gen_Literal_TN (1, 4), 
		    mtype, shift_lright, ops);
      Expand_Add (tmp3_tn, mult_tn, tmp2_tn, mtype, ops);
      mult_tn = tmp3_tn;
    }

    /* Generate and attach the shift:
     */
    Expand_Shift (result, mult_tn, Gen_Literal_TN (n-1, 4), 
		  mtype, shift_lright, ops);
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

static void Expand_Fast_Power_Of_2_Rem( TN* result, TN* src1, INT64 src2_val,
					TYPE_ID mtype, OPS* ops )
{
  FmtAssert( !TN_is_dedicated(result), ("NYI") );

  const BOOL is_64bit = MTYPE_is_size_double(mtype);
  BB* bb_entry = Cur_BB;
  BB* bb_then  = Gen_And_Append_BB( bb_entry );  // for case <src1> < 0
  BB* bb_else  = Gen_And_Append_BB( bb_then );   // for case <src1> >= 0
  const LABEL_IDX bb_else_label = Gen_Label_For_BB( bb_else );

  BB* bb_exit  = Gen_And_Append_BB( bb_else );
  const LABEL_IDX bb_exit_label = Gen_Label_For_BB( bb_exit );

  BB_branch_wn(bb_then) = WN_Create(OPC_GOTO,0);
  WN_label_number(BB_branch_wn(bb_then)) = bb_exit_label;

  BB_branch_wn(bb_entry) = WN_Create(OPC_TRUEBR,1);
  WN_kid0(BB_branch_wn(bb_entry)) = NULL;
  WN_label_number(BB_branch_wn(bb_entry)) = bb_else_label;

  // Build bb_entry
  {
    Exp_OP3v( OPC_TRUEBR,
	      NULL,
	      Gen_Label_TN( bb_else_label, 0 ),
	      src1,
	      Gen_Literal_TN(0,4),
	      V_BR_I4GE,
	      ops );

    if( &New_OPs != ops )
      OPS_Append_Ops( &New_OPs, ops );
    Process_New_OPs();
    BB_Append_Ops( bb_entry, &New_OPs );
    OPS_Init( &New_OPs );
    OPS_Init( ops );
  }

  // Build bb_then here.
  {
    OPS* bb_then_ops = &New_OPs;

    TN *t1 = Build_TN_Of_Mtype(mtype);
    INT64 absdvsr = src2_val < 0 ? -src2_val : src2_val;
    BOOL is_double = MTYPE_is_size_double(mtype);
    TN *t2 = Build_TN_Of_Mtype(mtype);
    TN *t3 = Build_TN_Of_Mtype(mtype);
    TN *t4 = Build_TN_Of_Mtype(mtype);
    TN *t5 = Build_TN_Of_Mtype(mtype);
    Expand_Shift(t1, src1, Gen_Literal_TN(is_double?63:31, 4), mtype, 
		 shift_aright, bb_then_ops);
    Expand_Immediate (t2, Gen_Literal_TN (absdvsr - 1, is_double?8:4), 
		      FALSE, bb_then_ops);
    Expand_Binary_And( t3, t1, t2, mtype, bb_then_ops );
    Expand_Add( t4, t3, src1, mtype, bb_then_ops );
    Expand_Binary_And( t5, t4, t2, mtype, bb_then_ops );
    Expand_Sub( result, t5, t3, mtype, bb_then_ops );

    Build_OP( TOP_jmp, Gen_Label_TN( bb_exit_label, 0 ), bb_then_ops );

    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops( bb_then, bb_then_ops );
    OPS_Init( bb_then_ops );
  }

  // Build bb_else here.
  {
    OPS* bb_else_ops = &New_OPs;

    FmtAssert( ISA_LC_Value_In_Class( src2_val, LC_simm32 ), ("NYI") );
    Expand_Binary_And( result, src1, Gen_Literal_TN(src2_val-1,4),
		       mtype, bb_else_ops );    

    total_bb_insts = 0;
    Last_Processed_OP = NULL;
    Process_New_OPs();
    BB_Append_Ops( bb_else, bb_else_ops );
    OPS_Init( bb_else_ops );
  }

  Cur_BB = bb_exit;
}


static void 
Expand_Power_Of_2_Rem (TN *result, TN *src1, INT64 src2_val, TYPE_ID mtype, OPS *ops)
{
  BOOL is_double = MTYPE_is_size_double(mtype);
  INT n = Get_Power_Of_2(src2_val, mtype);
  INT64 nMask = (1LL << n) - 1;
  TN *con = Gen_Literal_TN(nMask, is_double ? 8 : 4);

  if (MTYPE_is_signed(mtype)) {
    /* Avoid generating multi-BBs under -m32, which does not have too many
       registers.
    */
    if (src2_val > 0 && CG_divrem_opt) {
      Expand_Fast_Power_Of_2_Rem( result, src1, src2_val, mtype, ops );
    } else if( CG_use_setcc || Is_Target_32bit() || src2_val < 0 ){
      TN *t1 = Build_TN_Of_Mtype(mtype);
      INT64 absdvsr = src2_val < 0 ? -src2_val : src2_val;
      BOOL is_double = MTYPE_is_size_double(mtype);
      TN *t2 = Build_TN_Of_Mtype(mtype);
      TN *t3 = Build_TN_Of_Mtype(mtype);
      TN *t4 = Build_TN_Of_Mtype(mtype);
      TN *t5 = Build_TN_Of_Mtype(mtype);
      Expand_Shift(t1, src1, Gen_Literal_TN(is_double?63:31, 4), mtype, 
		   shift_aright, ops);
      Expand_Immediate (t2, Gen_Literal_TN (absdvsr - 1, is_double?8:4), 
			FALSE, ops);
      Expand_Binary_And( t3, t1, t2, mtype, ops );
      Expand_Add( t4, t3, src1, mtype, ops );
      Expand_Binary_And( t5, t4, t2, mtype, ops );
      Expand_Sub( result, t5, t3, mtype, ops );

    } else {
      Expand_Fast_Power_Of_2_Rem( result, src1, src2_val, mtype, ops );
    }
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

// Extend DIVIDEND_LO to DIVIDEND_HI/DIVIDEND_LO in preparation for division.
// The result occupies rdx/rax.
static void
Extend_Dividend (TN **dividend_lo, TN **dividend_hi, TYPE_ID mtype,
		 BOOL is_double, OPS *ops)
{
  *dividend_hi = Build_TN_Like(*dividend_lo);
  if (MTYPE_is_signed(mtype)){
    // Sign extend rAX to rDX with cltd/cqto.
    //
    // Must create new TN instead of reusing the same DIVIDEND_LO TN as src and
    // result.  This is because DIVIDEND_LO may be read-only in WHIRL.  In
    // general, CG cannot redefine TNs representing read-only WHIRL symbols,
    // because other parts of CG assume these TNs are read-only.  For example,
    // GRA assumes these TNs don't require spilling.  Bug 12283.
    TN *new_dividend_lo = Build_TN_Like(*dividend_lo);
    Build_OP(is_double ? TOP_cqto : TOP_cltd, new_dividend_lo, *dividend_hi,
	     *dividend_lo, ops);
    *dividend_lo = new_dividend_lo;
  } else {
    Build_OP(TOP_ldc32, *dividend_hi, Gen_Literal_TN(0, 4), ops);
  }
}

TN *
Expand_Divide (TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  /* Look for simple shift optimizations and multiply_hi optimizations:
   */
  INT64 src2_val;
  BOOL const_src2 = TN_Value_At_Op (src2, NULL, &src2_val);
  BOOL is_double = MTYPE_is_size_double(mtype);
  if (const_src2) {
    if (src2_val == 0){
      /* Instead of triggering an assertion, give a warning mesg could be good
	 enought. Since this piece of code might never be executed, especially
	 when the code is compiled at -O0 level.
	 Does opteron provide any break instruction like ia64 does?
      */
      DevWarn("Division by zero detected.\n");

    } else if (Expand_Integer_Divide_By_Constant(result, src1, src2_val, mtype, ops)){
      return NULL; // no hilo
    }
  }

  TOP top;
  FmtAssert(!TN_is_constant(src1),
	    ("Expand_Divide: unexpected constant operand"));
  if (TN_is_constant(src2))
    src2 = Expand_Immediate_Into_Register(src2, is_double, ops);
  // The following code is modeled after gcc treatment of signed vs. 
  // unsigned divide
  switch (mtype) {
  case MTYPE_I4: top = TOP_idiv32; break;
  case MTYPE_I8: top = TOP_idiv64; break;
  case MTYPE_U4: top = TOP_div32; break;
  case MTYPE_U8: top = TOP_div64; break;
  default: FmtAssert (FALSE, ("Handle this")); break;
  }
  TN *result1 = Build_TN_Like(result);
  TN *src3;
  Extend_Dividend(&src1, &src3, mtype, is_double, ops);
  FmtAssert( !OP_NEED_PAIR(mtype), ("NYI") );
  Build_OP(top, result, result1, src1, src3, src2, ops);
}


void
Expand_Rem (TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  TOP top;
  BOOL is_double = MTYPE_is_size_double(mtype);

  if( src1 == src2 ){
    Build_OP( is_double ? TOP_ldc64 : TOP_ldc32,
	      result, Gen_Literal_TN(0, 4), ops );
    return;
  }

  /* Try to optimize when constant divisor.
   */
  INT64 src2_val;
  BOOL const_src2 = TN_Value_At_Op (src2, NULL, &src2_val);
  if (const_src2) {
    if (src2_val == 0) // Division by Zero!
      DevWarn("Division by zero detected.\n");

    /* Handle powers of 2 specially.
     */
    if (Is_Power_Of_2(src2_val, mtype)) {
      Expand_Power_Of_2_Rem(result, src1, src2_val, mtype, ops);
      return;
    }

    /* Bug:147
       Don't expand integer divide by constant, if the value of this
       constant is 0.
    */
    if (CGEXP_cvrt_int_div_to_mult && (src2_val != 0)) {
	    
      TN *div_tn = Build_TN_Like (result);

      if (Expand_Integer_Divide_By_Constant(div_tn, src1, src2_val, 
			      mtype, ops)) {
	TN *mult_tn;

	/* Generate a multiply:
	 */
	mult_tn = Build_TN_Like (result);
	Expand_Multiply(mult_tn, div_tn, src2, mtype, ops);

	/* Subtract the result of the multiply from the original value.
	 */
	Build_OP(is_double?TOP_sub64:TOP_sub32, result, src1, mult_tn, ops);
	return;
      }
    }
  }

  FmtAssert(!TN_is_constant(src1),
	    ("Expand_Divide: unexpected constant operand"));
  if (TN_is_constant(src2))
    src2 = Expand_Immediate_Into_Register(src2, is_double, ops);
  switch (mtype) {
  case MTYPE_I4: top = TOP_idiv32; break;
  case MTYPE_I8: top = TOP_idiv64; break;
  case MTYPE_U4: top = TOP_div32; break;
  case MTYPE_U8: top = TOP_div64; break;
  default: FmtAssert (FALSE, ("Handle this")); break;
  }
  TN *result1 = Build_TN_Like(result);
  TN *src3;
  Extend_Dividend(&src1, &src3, mtype, is_double, ops);
  Build_OP(top, result1, result, src1, src3, src2, ops);
}


/* Expand mod. */
void 
Expand_Mod (TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
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

  // Calculate remainder.
  TN *remainder = result;
  Expand_Rem(remainder, src1, src2, mtype, ops);

  // p1 = (dividend and divisor have different signs) ? 1 : 0
  // divisor_or_zero = p1 ? divisor : 0
  // remainder_plus_divisor = remainder + divisor_or_zero
  // result = (remainder == 0) ? remainder : remainder + divisor_or_zero

  // Generate p1.
  TN *p1 = Rflags_TN();
  TN *src1_xor_src2 = Build_TN_Like(result);
  Build_OP(is_double ? TOP_xor64 : TOP_xor32, src1_xor_src2, src1, src2, ops);
  Build_OP(is_double ? TOP_test64 : TOP_test32, p1,
	   src1_xor_src2, src1_xor_src2, ops);

  // Generate divisor_or_zero.
  TN *divisor_or_zero = Build_TN_Like(result);
  Build_OP(is_double ? TOP_ldc64 : TOP_ldc32, 
	   divisor_or_zero, Gen_Literal_TN(0, is_double ? 8 : 4), ops);
  Expand_Cmov(TOP_cmovl, divisor_or_zero, src2, p1, ops);

  // Generate remainder_plus_divisor (actually remainder + divisor_or_zero).
  TN *remainder_plus_divisor = Build_TN_Like(result);
  Build_OP(is_double ? TOP_add64 : TOP_add32,
	   remainder_plus_divisor, remainder, divisor_or_zero, ops);

  // Generate result by adjusting the remainder.
  TN *p2 = Rflags_TN();
  Build_OP(TOP_test32, p2, remainder, remainder, ops);	// remainder zero?
  Expand_Cmov(TOP_cmovne, remainder, remainder_plus_divisor, p2, ops);

  // Since we defined remainder as the same TN as the result, remainder is now
  // the result.
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

  if (const_src2){
    if (src2_val == 0){ // Division by Zero!
      DevWarn( "Division by zero detected at compile time.\n" );
      const_src2 = FALSE;
    }
  }

  if (const_src1 && const_src2) {
    if (MTYPE_is_signed(mtype)) {
      const INT64 minintval = is_double ? INT64_MIN : INT32_MIN;
      if (src1_val == minintval && src2_val == -1) {
        //
        // INT_MIN/-1 => INT_MIN * 1/-1 => INT_MIN * -1 
        //  => -INT_MIN => ~INT_MIN + 1 => INT_MIN 
        // 
        // INT_MIN % -1 => 0

        BOOL is_signed = true;
        INT tn_size = is_double ? 8 : 4;
        Exp_Immediate(result, Gen_Literal_TN(minintval, tn_size), is_signed, ops);
        Exp_Immediate(result2, Gen_Literal_TN(0, tn_size), is_signed, ops);
        return;
      }
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
	Expand_Sub( result2, src1, t1, mtype, ops );
      }
      return;
    }
  }

  FmtAssert( !OP_NEED_PAIR(mtype), ("Expand_DivRem: DIVREM should not be 64-bit") );

  TOP top;
  FmtAssert(!TN_is_constant(src1),
	    ("Expand_Divide: unexpected constant operand"));
  if (TN_is_constant(src2))
    src2 = Expand_Immediate_Into_Register(src2, is_double, ops);
  // The following code is modeled after gcc treatment of signed vs. 
  // unsigned divide
  switch (mtype) {
  case MTYPE_I4: top = TOP_idiv32; break;
  case MTYPE_I8: top = TOP_idiv64; break;
  case MTYPE_U4: top = TOP_div32; break;
  case MTYPE_U8: top = TOP_div64; break;
  default: FmtAssert (FALSE, ("Handle this")); break;
  }
  TN *src3;
  Extend_Dividend(&src1, &src3, mtype, is_double, ops);
  Build_OP(top, result, result2, src1, src3, src2, ops);
}
