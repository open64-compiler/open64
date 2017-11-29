/*
 * Copyright (C) 2006-2009 Tsinghua University.  All Rights Reserved.
 */

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
  return FALSE;
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
  INT64 src2_val;
  BOOL const_src2 = TN_Value_At_Op (src2, NULL, &src2_val);
  if ((const_src2) && (src2_val == 0)){
  	const_src2 == FALSE;
      //FmtAssert (FALSE, ("Division by zero detected.\n"));
  }


  if(const_src2 && Is_Power_Of_2(src2_val,mtype))
  {
        Expand_Power_Of_2_Mod(result,src1,src2_val,mtype,ops);
        return;
  }

  if(TN_is_constant(src1))
    src1 = Expand_Immediate_Into_Register(NULL, src1, mtype, ops);
  if (TN_is_constant(src2))
    src2 = Expand_Immediate_Into_Register(NULL, src2, mtype, ops);

   TOP div = TOP_divw;
  if (MTYPE_is_unsigned(mtype)) div = TOP_divwu;
  TN* tn1 = Build_TN_Like(result);
  TN* tn2 = Build_TN_Like(result);
  TN* tn3 = Build_TN_Like(result);
  TN* tn4 = Build_TN_Like(result);
  TN* tn5 = Build_TN_Like(result);
  TN* tn6 = Build_TN_Like(result);
  
  Build_OP(div, tn1, src1, src2, ops);
  Build_OP(TOP_mullw, tn2, tn1, src2, ops);
  Build_OP(TOP_subf, tn3, tn2, src1, ops);

  // add 0 or src2 to rem result
  Build_OP(TOP_xor, tn4, tn3, src2, ops);
  Build_OP(TOP_srawi, tn5, tn4, Gen_Literal_TN(31, 4), ops);
  Build_OP(TOP_and, tn5, src2, ops);
  Build_OP(TOP_add, result, tn3, tn5, ops);
}

/*****************************************************************************
 *
 * Integer division external interfaces
 *
 *****************************************************************************/

TN *
Expand_Divide (TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  TN* new_src1 = src1;
  TN* new_src2 = src2;
  if (TN_is_constant(src1))
    new_src1 = Expand_Immediate_Into_Register(NULL, src1, mtype, ops);
  /* Look for simple shift optimizations and multiply_hi optimizations:
   */
  INT64 src2_val;
  BOOL const_src2 = TN_Value_At_Op (src2, NULL, &src2_val);
  if (const_src2) {
    if (src2_val == 0)  {// Division by Zero!
      new_src2 = Expand_Immediate_Into_Register(NULL, src2, mtype, ops);
      //FmtAssert (FALSE, ("Division by zero detected.\n"));
    }
    else
    if (Expand_Integer_Divide_By_Constant(result, src1, src2_val, mtype, ops)) {
      return result; // no hilo
    } else if (TN_is_constant(src2)) {
      new_src2 = Expand_Immediate_Into_Register(NULL, src2, mtype, ops);
    }
  }
  
  TOP div = TOP_divw;
  if (MTYPE_is_unsigned(mtype)) div = TOP_divwu;
  Build_OP(div, result, new_src1, new_src2, ops);
  return result;
}


void
Expand_Rem (TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  INT64 src2_val;
  BOOL const_src2 = TN_Value_At_Op (src2, NULL, &src2_val);
  if ((const_src2) && (src2_val == 0)) {
    const_src2 = FALSE;
      //FmtAssert (FALSE, ("Division by zero detected.\n"));
  }
  if(const_src2 && Is_Power_Of_2(src2_val, mtype))
  {
    Expand_Power_Of_2_Rem(result,src1,src2_val,mtype,ops);
    return;
  }

  TN * tn1 = Build_TN_Like(result);
  TN * tn2 = Build_TN_Like(result);
  BOOL succ = FALSE;
  if ((const_src2)) {
    succ = Expand_Integer_Divide_By_Constant(tn1, src1,src2_val, mtype, ops);
  }

  if (!succ) {
    if (TN_is_constant(src1))
      src1 = Expand_Immediate_Into_Register(NULL, src1, mtype, ops);
    if (TN_is_constant(src2))
      src2 = Expand_Immediate_Into_Register(NULL, src2, mtype, ops);

    TOP div = TOP_divw;
    if (MTYPE_is_unsigned(mtype)) div = TOP_divwu;
    Build_OP(div, tn1, src1, src2, ops);
  }
  
  Build_OP(TOP_mullw, tn2, tn1, src2, ops);
  Build_OP(TOP_subf, result, tn2, src1, ops);
}


void 
Expand_DivRem(TN *result, TN *result2, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  INT64 src2_val;
  BOOL const_src2 = TN_Value_At_Op (src2, NULL, &src2_val);
  if ((const_src2) && (src2_val == 0)) {
  	const_src2 == FALSE;
      //FmtAssert (FALSE, ("Division by zero detected.\n"));
  }
  TN* result_new = result;
  TN* result2_new = result2;
  if (result_new == src1 || result_new == src2) {
    result_new = Build_TN_Like(result);
  }
  if (result2_new == src1 || result2_new == src2) {
    result2_new = Build_TN_Like(result2);
  }
  BOOL succ = FALSE;
  if (const_src2) {
    succ = Expand_Integer_Divide_By_Constant(result_new, src1, src2_val, mtype, ops);
  }
  if (!succ) {
    if (TN_is_constant(src1))
      src1 = Expand_Immediate_Into_Register(NULL, src1, mtype, ops);
    if (TN_is_constant(src2))
      src2 = Expand_Immediate_Into_Register(NULL, src2, mtype, ops);

    TOP div = TOP_divw;
    if (MTYPE_is_unsigned(mtype)) div = TOP_divwu;
    Build_OP(div, result_new, src1, src2, ops);
  }
  if(const_src2 && Is_Power_Of_2(src2_val, mtype))
  {
    Expand_Power_Of_2_Rem(result2_new, src1,src2_val,mtype,ops);
  } else {
    TN * tn = Build_TN_Like(result2);
    Build_OP(TOP_mullw, tn, result_new, src2, ops);
    Build_OP(TOP_subf, result2_new, tn, src1, ops);
  }
  if (result_new != result) {
    Expand_Copy(result, result_new, mtype, ops);
  }
  if (result2_new != result2) {
    Expand_Copy(result2, result2_new, mtype, ops);
  }
}
