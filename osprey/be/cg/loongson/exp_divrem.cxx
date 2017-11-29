/*

  Copyright (C) 2000 Silicon Graphics, Inc.  All Rights Reserved.

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
#include "topcode.h"
#include "tn.h"
#include "cg_flags.h"
#include "targ_isa_lits.h"
#include "op.h"
#include "whirl2ops.h"
#include "cg_spill.h"
#include "cgexp.h"
#include "cgexp_internals.h"

/* Define the exponent parameters for the various float types.
 */
enum { E32min = -126,   E32max = 127,   E32bias = 127   }; // single
enum { E64min = -1022,  E64max = 1023,  E64bias = 1023  }; // double
enum { E80min = -16382, E80max = 16383, E80bias = 16383 }; // long double
enum { E82min = -65534, E82max = 65535, E82bias = 65535 }; // register-format

/*****************************************************************************
 *
 * Integer division internal support routines
 *
 *****************************************************************************/

/* Return values for Check_Divide:
 */
typedef enum
{
    DIVCHK_RUNTIME,	// unknown except at run-time
    DIVCHK_BYZERO,	// unconditional div-by-zero
    DIVCHK_OVERFLOW	// unconditional overflow
} DIVCHK_STATUS;

/* Check a divide for undefined operations. The operands are examined
 * to determine if it is known at compile-time that a fault will
 * occur or if it cannot be known until run-time. The return value
 * indicates the situation. In addition, if divide checking is
 * enabled, code is generated to cause an exception.
 */
static DIVCHK_STATUS
Check_Divide(TN *numerator, TN *divisor, TYPE_ID mtype, OPS *ops)
{
    // TODO: don't want to generate checks while using simulator, so reset
    // div-by-zero checking which is on by default.

    INT64	divisor_val;
    BOOL const_divisor = TN_Value_At_Op(divisor, NULL, &divisor_val);
    BOOL is_double = MTYPE_is_size_double(mtype);

    /* Check for divide-by-zero.
     */
    if (const_divisor && divisor_val == 0)
    {
        return DIVCHK_BYZERO;
    }

    /* Check for overflow.
     */
    if (MTYPE_is_signed(mtype))
    {
        INT64 numer_val;
        BOOL const_numer = TN_Value_At_Op(numerator, NULL, &numer_val);
        const INT64 minint_val = is_double ? INT64_MIN : INT32_MIN;
        const INT min_tn_size = is_double ? 8 : 4;
        if (const_divisor && const_numer)
        {
            if (numer_val == minint_val && divisor_val == -1)
            {
                return DIVCHK_OVERFLOW;
            }
        }
    }

    return DIVCHK_RUNTIME;
}


/* return TRUE if the val is a power of 2 */
#define IS_POWER_OF_2(val)	((val != 0) && ((val & (val-1)) == 0))

static BOOL Is_Power_Of_2(INT64 val, TYPE_ID mtype)
{
    if (MTYPE_is_signed(mtype) && val < 0) val = -val;

    if (mtype == MTYPE_U4) val &= 0xffffffffull;

    return IS_POWER_OF_2(val);
}


static INT
Get_Power_Of_2(INT64 val, TYPE_ID mtype)
{
    INT i;
    INT64 pow2mask;

    if (MTYPE_is_signed(mtype) && val < 0) val = -val;

    if (mtype == MTYPE_U4) val &= 0xffffffffull;

    pow2mask = 1;
    for (i = 0; i < MTYPE_size_reg(mtype); ++i)
    {
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
Expand_Power_Of_2_Divide(TN *result, TN *number, INT64 dvsr, TYPE_ID mtype, OPS *ops)
{
    INT  power_of_dvsr = Get_Power_Of_2(dvsr, mtype);
    if (MTYPE_is_unsigned(mtype))
    {
        Expand_Shift(result, number, Gen_Literal_TN(power_of_dvsr, 4), mtype, shift_lright, ops);
    }
    else
    {
        TN *tmp_res = dvsr < 0 ? Build_TN_Like(result) : result;
        INT64 absdvsr = dvsr < 0 ? -dvsr : dvsr;
        BOOL is_double = MTYPE_is_size_double(mtype);
        if (absdvsr == 2)
        {

            /* Optimize for abs(divisor) == 2:
             *	srl		tmp1, numerator, 31;
             *	addu	tmp2, numerator, tmp1;
             *	sra		result, tmp2, 1;
             */
            TN* tmp1 = Build_TN_Like(result);
            Expand_Shift(tmp1, number, Gen_Literal_TN(MTYPE_size_reg(mtype) - 1, 4), mtype, shift_lright, ops);

            TN* tmp2 = Build_TN_Like(result);
            Expand_Add(tmp2, number,  tmp1, mtype, ops);

            Expand_Shift(tmp_res, tmp2, Gen_Literal_TN(1, 4), mtype, shift_aright, ops);

        }
        else
        {

            /* General case:
             *	 if ( numerator < 0)
             *		addu	tmp, numerator, abs(divisor) -1;
             * 	else
             *		move	tmp, numerator;
             *	sra	result, tmp, power;
             */

            /* Generate the sign of the numerator */
            TN* mask_tn = Build_TN_Of_Mtype(mtype);
            Expand_Shift(mask_tn, number, Gen_Literal_TN(MTYPE_size_reg(mtype) - 1, 4), mtype, shift_aright, ops);

            TN* addend = Build_TN_Of_Mtype(mtype);
            if (ISA_LC_Value_In_Class(absdvsr - 1, LC_k16))
                Build_OP(TOP_andi, addend, True_TN, mask_tn, Gen_Literal_TN(absdvsr - 1, 2), ops);
            else
            {
                TN* absdvsr_tn = Build_TN_Of_Mtype(mtype);
                Expand_Immediate(absdvsr_tn, Gen_Literal_TN(absdvsr - 1, 8), TRUE, ops);
                Build_OP(TOP_and, addend, True_TN, mask_tn, absdvsr_tn, ops);
            }
            TN* temp = Build_TN_Like(result);
            Expand_Add(temp, number, addend, mtype, ops);

            Expand_Shift(tmp_res, temp, Gen_Literal_TN(power_of_dvsr, 4), mtype, shift_aright, ops);
        }
        if (dvsr < 0) Expand_Neg(result, tmp_res, mtype, ops);
    }
}

/* Expand the sequence for division and/or remainder by a variable with
 * MIPS Instruciton set.   --George Her 11.6,2003
 */
static void
Expand_MIPS_NonConst_DivRem(TN *quot, TN *rem, TN *dividend, TN *divisor,
                            TYPE_ID mtype, OPS *ops)
{

    Is_True((quot != NULL || rem != NULL), (" Quotient and Remainder could not all be  NULL!!"));

    if (TN_has_value(divisor))
    {
        divisor = Expand_Immediate_Into_Register(divisor, ops);
    }

    if (FALSE)
    {
        if MTYPE_is_unsigned(mtype)
        {
            if (quot != NULL)
                Build_OP(TOP_divulo, quot, HI_TN, LO_TN, True_TN, Zero_TN, dividend, divisor, ops);
            if (rem != NULL)
                Build_OP(TOP_divuhi, rem, HI_TN, LO_TN, True_TN, Zero_TN, dividend, divisor, ops);

        }
        else
        {
            if (quot != NULL)
                Build_OP(TOP_divlo, quot, HI_TN, LO_TN, True_TN, Zero_TN, dividend, divisor, ops);
            if (rem != NULL)
                Build_OP(TOP_divhi, rem, HI_TN, LO_TN, True_TN, Zero_TN, dividend, divisor, ops);
        }
    }
    else
    {
        if MTYPE_is_unsigned(mtype)
        {
            MTYPE_is_size_double(mtype) ?
            Build_OP(TOP_ddivu, HI_TN, LO_TN, True_TN, Zero_TN, dividend, divisor, ops)
            : Build_OP(TOP_divu, HI_TN, LO_TN, True_TN, Zero_TN, dividend, divisor, ops);
            Build_OP(TOP_teq, True_TN, divisor, Zero_TN, ops);
            if (quot != NULL)  Build_OP(TOP_mflo, quot, True_TN, LO_TN, ops);
            if (rem != NULL)   Build_OP(TOP_mfhi, rem, True_TN, HI_TN, ops);

        }
        else
        {
            MTYPE_is_size_double(mtype) ?
            Build_OP(TOP_ddiv, HI_TN, LO_TN, True_TN, Zero_TN, dividend, divisor, ops)
            : Build_OP(TOP_div, HI_TN, LO_TN, True_TN, Zero_TN, dividend, divisor, ops);
            Build_OP(TOP_teq, True_TN, divisor, Zero_TN, ops);
            if (quot != NULL)  Build_OP(TOP_mflo, quot, True_TN, LO_TN, ops);
            if (rem != NULL)   Build_OP(TOP_mfhi, rem, True_TN, HI_TN, ops);
        }
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

static UINT64 determine_pseudo_inverse(
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
    while (m < b1)
    {
        n++;
        m = (m << 1) | 1;
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
    for (q = i = 0; i <= maxbits_a; i++)
    {
        q <<= 1;
        if (m >= b)
        {
            m -= b;
            q |= 1;
        }
        m = (m << 1) | 1;
    }
    return 1 + q;
}


/* Expand the sequence for division by a constant. It is the caller's
 * job to verify that the divisor is non-zero.
 */
static BOOL
Expand_Integer_Divide_By_Constant(TN *result, TN *numer_tn, INT64 denom_val,
                                  TYPE_ID mtype, OPS *ops)
{
    UINT64 abs_of_denom;		// b = |denom_val|
    UINT64 d;
    INT64  precision_required;
    INT64  shift_width;
    BOOL is_odd;
    TN *d_tn;
    TN *abs_tn;
    TN *mult_tn;
    TN *shift_tn;
    TOP opc;
    TN *p1, *p2;
    BOOL is_double = MTYPE_is_size_double(mtype);
    BOOL is_signed = MTYPE_is_signed(mtype);

    /* Handle the trivial ones:
     */
    if (denom_val == 1)
    {
        Exp_COPY(result, numer_tn, ops);
        return TRUE;
    }
    else if (is_signed && denom_val == -1)
    {
        Expand_Neg(result, numer_tn, mtype, ops);
        return TRUE;
    }

    /* Look for simple shift optimizations:
     */
    if (Is_Power_Of_2(denom_val, mtype))
    {
        Expand_Power_Of_2_Divide(result, numer_tn, denom_val, mtype, ops);
        return TRUE;
    }

    if (!CGEXP_cvrt_int_div_to_mult) return FALSE;

    if (is_signed)
    {

        abs_of_denom = denom_val < 0 ? -denom_val : denom_val;
        is_odd = (abs_of_denom & 1);

        d = determine_pseudo_inverse(abs_of_denom, is_double ? 63 : 31, &shift_width);

        if (shift_width > (is_double ? 63 : 31))
        {
            /* OOPS! The shift count can't be bigger than the word size! */
            return FALSE;
        }

        d_tn = Build_TN_Like(result);
        Expand_Immediate(d_tn,
                         Gen_Literal_TN(d, is_double ? 8 : 4),
                         is_signed, ops);

        /* Generate the absolute value of the numerator:
         * abs_of_numer= numerator < 0 ? -numerator : numerator;
         */

        /* Generate the sign of the numerator */
        TN* condition_tn = Build_TN_Of_Mtype(mtype);
        TN* sign_tn = 	Build_TN_Of_Mtype(mtype);
        Expand_Shift(sign_tn, numer_tn, Gen_Literal_TN(is_double ? 63 : 31, 4), mtype, shift_lright, ops);
        Expand_Shift(condition_tn, numer_tn, Gen_Literal_TN(is_double ? 63 : 31, 4), mtype, shift_aright, ops);

        /*Get absolute value of numerator*/
        TN* abs_of_numer = Build_TN_Of_Mtype(mtype);
        Expand_Abs(abs_of_numer, numer_tn, mtype, ops);


        /* Generate a multiply upper:
         */
        mult_tn = Build_TN_Of_Mtype(mtype);
        Expand_High_Multiply(mult_tn, abs_of_numer, d_tn, is_double ? MTYPE_U8 : MTYPE_U4, ops);

        /* Generate and attach the shift:
         */
        if (shift_width > 0)
        {
            shift_tn = Build_TN_Of_Mtype(mtype);
            Expand_Shift(shift_tn, mult_tn, Gen_Literal_TN(shift_width - 1, 4), mtype, shift_aright, ops);
        }
        else
        {
            shift_tn = mult_tn;
        }

        /* Select positive or negated result:
         *	if ( divisor < 0 )
         *		result = numerator < 0 ? shift_tn : -shift_tn;
         *	else
         *		result = numerator < 0 ? -shift_tn : shift_tn;
         */
        TN* neg_result = Build_TN_Of_Mtype(mtype);
        Expand_Sub(neg_result, Zero_TN, shift_tn, mtype, ops);

        TN* tmpr = Build_TN_Of_Mtype(mtype);
        if (denom_val < 0)
        {
            Expand_Binary_Xor(tmpr, neg_result, condition_tn, mtype, ops);
            Expand_Add(result, tmpr, sign_tn, mtype, ops);
        }
        else
        {
            Expand_Binary_Xor(tmpr, shift_tn, condition_tn, mtype, ops);
            Expand_Add(result, tmpr, sign_tn, mtype, ops);
        }

    } /* end Signed */

    else   /* Unsigned */
    {

        abs_of_denom = denom_val;
        is_odd = (abs_of_denom & 1);

        /* Full precision calculation is required.
         */
        if (is_odd)
        {
            precision_required = is_double ? 64 : 32 ;
        }
        else
        {

            /* Pre-shift the numerator and denominator so that
               one less bit is required in the calculation. Then
               we can avoid the subtract-shift-add after the
               multiply operation. */
            abs_of_denom >>= 1;
            precision_required =  is_double ? 63 : 31;

            /* Pre-shift to simplify later calculations.
             */
            TN *tmp1_tn = Build_TN_Of_Mtype(mtype);
            Expand_Shift(tmp1_tn, numer_tn, Gen_Literal_TN(1, 4), mtype, shift_lright, ops);
            numer_tn = tmp1_tn;
        }

        d = determine_pseudo_inverse(abs_of_denom, precision_required, &shift_width);
        /* George Her add the following line to avoid U4 d out of range.12.9,2003*/
        if (is_odd && !is_double)
            d = (INT32) d;

        if (shift_width > precision_required)
        {
            /* OOPS! The shift count can't be bigger than the word size! */
            return FALSE;
        }

        d_tn = Build_TN_Like(result);
        Expand_Immediate(d_tn, Gen_Literal_TN(d,  is_double ? 8 : 4), is_signed, ops);

        /* Generate a multiply upper:
         */
        mult_tn = Build_TN_Of_Mtype(mtype);
        Expand_High_Multiply(mult_tn, numer_tn, d_tn, is_double ? MTYPE_U8 : MTYPE_U4, ops);

        if (is_odd)
        {
            /* Odd divisors need full precision and, hence, extra instructions.
             */
            TN *tmp1_tn = Build_TN_Of_Mtype(mtype);
            TN *tmp2_tn = Build_TN_Of_Mtype(mtype);
            TN *tmp3_tn = Build_TN_Of_Mtype(mtype);
            Expand_Sub(tmp1_tn, numer_tn, mult_tn, mtype, ops);
            Expand_Shift(tmp2_tn, tmp1_tn, Gen_Literal_TN(1, 4), mtype, shift_lright, ops);
            Expand_Add(tmp3_tn, mult_tn, tmp2_tn, mtype, ops);
            mult_tn = tmp3_tn;
        }

        /* Generate and attach the shift:
         */
        Expand_Shift(result, mult_tn, Gen_Literal_TN(shift_width - 1, 4), mtype, shift_lright, ops);

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
Expand_Power_Of_2_Rem(TN *result, TN *src1, INT64 src2_val, TYPE_ID mtype, OPS *ops)
{
    BOOL is_double = MTYPE_is_size_double(mtype);
    INT n = Get_Power_Of_2(src2_val, mtype);
    INT64 nMask = (1LL << n) - 1;
    TN *con = Gen_Literal_TN(nMask, is_double ? 8 : 4);

    if (MTYPE_is_signed(mtype))
    {

        TN* abs_tn = Build_TN_Of_Mtype(mtype);

        /* Get sign of src1
         */
        TN* condition_tn = Build_TN_Of_Mtype(mtype);
        TN* sign_tn = 	Build_TN_Of_Mtype(mtype);
        Expand_Shift(sign_tn, src1, Gen_Literal_TN(is_double ? 63 : 31, 4), mtype, shift_lright, ops);
        Expand_Shift(condition_tn, src1, Gen_Literal_TN(is_double ? 63 : 31, 4), mtype, shift_aright, ops);

        /* Get absolute value of src1
         */
        Expand_Abs(abs_tn, src1, mtype, ops);

        /* Perform the AND
         */
        TN* abs_result = Build_TN_Of_Mtype(mtype);
        Expand_Binary_And(abs_result, abs_tn, con, mtype, ops);

        /* Negate the result if src1 was negative
         */
        TN* tmp_result = Build_TN_Of_Mtype(mtype);
        Expand_Binary_Xor(tmp_result, abs_result, condition_tn, mtype, ops);
        Expand_Add(result, tmp_result, sign_tn, mtype, ops);

    }
    else
    {
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
Expand_Power_Of_2_Mod(TN *result, TN *src1, INT64 src2_val, TYPE_ID mtype, OPS *ops)
{
    BOOL is_double = MTYPE_is_size_double(mtype);
    INT64 absval = src2_val < 0 ? -src2_val : src2_val;
    INT	n      = Get_Power_Of_2(absval, mtype);
    INT64	nMask  = (1LL << n) - 1;
    TN	*con   = Gen_Literal_TN(nMask, is_double ? 8 : 4);

    if (MTYPE_is_signed(mtype) && src2_val < 0)
    {
        TN *tmp1, *tmp2;

        tmp1 = Build_TN_Of_Mtype(mtype);
        Expand_Neg(tmp1, src1, mtype, ops);

        tmp2 = Build_TN_Of_Mtype(mtype);
        Expand_Binary_And(tmp2, tmp1, con, mtype, ops);

        Expand_Neg(result, tmp2, mtype, ops);
    }
    else
    {
        Expand_Binary_And(result, src1, con, mtype, ops);
    }
}

/*****************************************************************************
 *
 * Integer division external interfaces
 *
 *****************************************************************************/
extern inline BOOL Arg_or_Retval(TN *tn);
TN *
Expand_Divide(TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
    /* Check for undefined operations we can detect at compile-time
     * and when enabled, generate run-time checks.
     */
    switch (Check_Divide(src1, src2, mtype, ops))
    {
    case DIVCHK_BYZERO:
    case DIVCHK_OVERFLOW:
        DevWarn("Divide by zero or overflow!\n");
        Build_OP(TOP_break, True_TN, Gen_Literal_TN(7, 4), ops);
        return NULL;
    }

    /* Look for simple shift optimizations and multiply_hi optimizations:
     */
    INT64 src2_val;
    BOOL const_src2 = TN_Value_At_Op(src2, NULL, &src2_val);
    if (const_src2)
    {
        if (Expand_Integer_Divide_By_Constant(result, src1, src2_val, mtype, ops))
        {
            return NULL; // no hilo
        }
    }


    if (CGEXP_use_Loongson2e_MultDivMod)
    {
        if (TN_has_value(src2))
        {
            src2 = Expand_Immediate_Into_Register(src2, ops);
        }

        if (MTYPE_is_unsigned(mtype))
        {

            if (MTYPE_is_size_double(mtype))
            {
                Build_OP(TOP_ddivu_g, result, True_TN, src1, src2, ops);
            }
            else
            {
                Build_OP(TOP_divu_g, result, True_TN, src1, src2, ops);
            }
        }
        else
        {
            if (MTYPE_is_size_double(mtype))
            {
                Build_OP(TOP_ddiv_g, result, True_TN, src1, src2, ops);
            }
            else
            {
                Build_OP(TOP_div_g, result, True_TN, src1, src2, ops);
            }
        }
    }
    else
    {
        Expand_MIPS_NonConst_DivRem(result, NULL, src1, src2, mtype, ops);
    }

    // To avoid deleting it!
    if (Arg_or_Retval(result))
        Set_OP_volatile(OPS_last(ops));

    return NULL;  // hilo
}


void
Expand_Rem(TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops, OPCODE opcode)
{
    /* Check for undefined operations we can detect at compile-time
     * and when enabled, generate run-time checks.
     */
    switch (Check_Divide(src1, src2, mtype, ops))
    {
    case DIVCHK_BYZERO:
    case DIVCHK_OVERFLOW:
        DevWarn("divide by zero or overflow");
        Build_OP(TOP_break, True_TN, Gen_Literal_TN(7, 4), ops);
        return;
    }

    /* Try to optimize when constant divisor.
     */
    INT64 src2_val;
    BOOL const_src2 = TN_Value_At_Op(src2, NULL, &src2_val);
    if (const_src2)
    {

        /* Handle powers of 2 specially.
         */
        if (Is_Power_Of_2(src2_val, mtype))
        {
            Expand_Power_Of_2_Rem(result, src1, src2_val, mtype, ops);
            return;
        }

        if (CGEXP_cvrt_int_div_to_mult)
        {
            TN *div_tn = Build_TN_Like(result);

            if (Expand_Integer_Divide_By_Constant(div_tn, src1, src2_val, mtype, ops))
            {
                TN *mult_tn;

                /* Generate a multiply:
                 */
                mult_tn = Build_TN_Like(result);
                Expand_Multiply(mult_tn, div_tn, src2, mtype, ops, opcode);

                /* Subtract the result of the multiply from the original value.
                 */
                Build_OP(TOP_subu, result, True_TN, src1, mult_tn, ops);
                return;
            }
        }
    }

    if (CGEXP_use_Loongson2e_MultDivMod)	// new ops supported  on L2e
    {
        if (TN_has_value(src2))
        {
            src2 = Expand_Immediate_Into_Register(src2, ops);
        }

        if (MTYPE_is_unsigned(mtype))
        {
            if (MTYPE_is_size_double(mtype))
                Build_OP(TOP_dmodu_g, result, True_TN, src1, src2, ops);
            else
                Build_OP(TOP_modu_g, result, True_TN, src1, src2, ops);
        }
        else
        {
            if (MTYPE_is_size_double(mtype))
                Build_OP(TOP_dmod_g, result, True_TN, src1, src2, ops);
            else
                Build_OP(TOP_mod_g, result, True_TN, src1, src2, ops);
        }
        Build_OP(TOP_teq, True_TN, src2, Zero_TN, ops);
    }
    else
    {
        Expand_MIPS_NonConst_DivRem(NULL, result, src1, src2, mtype, ops);
    }

}


/*	Expand mod(x,y) as follows:
 *		t1=	rem(x,y)
 *		t2=	xor(t1,y)
 *	       if (t2 >= 0)
 *              t3 = t2
 *           else
 *             t3 = t2 + y
 */
void
Expand_Mod(TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops, OPCODE opcode)
{
    switch (Check_Divide(src1, src2, mtype, ops))
    {
    case DIVCHK_BYZERO:
    case DIVCHK_OVERFLOW:
        DevWarn("divide by zero or overflow");
        Build_OP(TOP_break, True_TN, Gen_Literal_TN(7, 4), ops);
        return;
    }

    TN *tmp1, *tmp2, *tmp3;
    tmp1 = Build_TN_Like(result);
    tmp2 = Build_TN_Like(result);
    tmp3 = Build_TN_Like(result);
    // Rem
    Expand_Rem(tmp1, src1, src2, mtype, ops, opcode);

    // xor
    Expand_Binary_Xor(tmp2, src2, tmp1, mtype, ops);

    LABEL_IDX lab_idx_lt_zero, lab_idx_exit;
    LABEL* lb_lt_zero = &New_LABEL(CURRENT_SYMTAB, lab_idx_lt_zero);
    LABEL* lb_exit = &New_LABEL(CURRENT_SYMTAB, lab_idx_exit);
    TN* tn_lt_zero = Gen_Label_TN(lab_idx_lt_zero, 0);
    TN* tn_exit = Gen_Label_TN(lab_idx_exit, 0);

    Build_OP(TOP_bltz, True_TN, tmp2, tn_lt_zero, ops);
    // >= 0
    Begin_New_Basic_Block();
    Expand_Copy(tmp3, tmp1, mtype, ops);
    Build_OP(TOP_j, True_TN, tn_exit, ops);

    // <0
    Begin_New_Basic_Block();
    Start_New_Label(lab_idx_lt_zero, lb_lt_zero, "mod_lt_zero");
    Expand_Add(tmp3, src2, tmp1, mtype, ops);

    // copy to result
    Begin_New_Basic_Block();
    Start_New_Label(lab_idx_exit, lb_exit, "mod_exit");
    Expand_Copy(result, tmp3, mtype, ops);
}


void
Expand_DivRem(TN *result, TN *result2, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops, OPCODE opcode)
{
    /* Check for undefined operations we can detect at compile-time
     * and when enabled, generate run-time checks.
     */
    switch (Check_Divide(src1, src2, mtype, ops))
    {
    case DIVCHK_BYZERO:
    case DIVCHK_OVERFLOW:
        DevWarn("Divide by zero or overflow!\n");
        Build_OP(TOP_break, True_TN, Gen_Literal_TN(7, 4), ops);
        return;
    }

    /* Usually we expect whirl operators to be folded where possible.
     * But divrem is an odd beast in that the result is a special
     * "handle" rather than a value. There is no way to produce constants.
     * Therefore in some odd instances we can get constant operands,
     * so fold them here, avoiding nasty trapping issues.
     */
    INT64 src1_val;
    BOOL const_src1 = TN_Value_At_Op(src1, NULL, &src1_val);
    INT64 src2_val;
    BOOL const_src2 = TN_Value_At_Op(src2, NULL, &src2_val);
    if (const_src1 && const_src2)
    {
        INT64 quot_val, rem_val;
        switch (mtype)
        {
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
        Expand_Immediate(result, Gen_Literal_TN(quot_val, tn_size), is_signed, ops);
        Expand_Immediate(result2, Gen_Literal_TN(rem_val, tn_size), is_signed, ops);
        return;
    }

    /* Look for simple shift optimizations and multiply_hi optimizations:
     */
    if (const_src2)
    {
        if (Expand_Integer_Divide_By_Constant(result, src1, src2_val, mtype, ops))
        {

            // Now get the rem part. Since the constant value is probably small,
            // we are unlikely to generate a multiply here (i.e., we'll probably
            // generate shifts)
            //
            if (!MTYPE_is_signed(mtype) && Is_Power_Of_2(src2_val, mtype))
            {
                Expand_Power_Of_2_Rem(result2, src1, src2_val, mtype, ops);
            }
            else
            {
                TN *t1 = Build_TN_Like(result);
                Expand_Multiply(t1, result, src2, mtype, ops, opcode);
                Build_OP(TOP_sub, result2, True_TN, src1, t1, ops);
            }
            return;
        }
    }
    Expand_MIPS_NonConst_DivRem(result, result2, src1, src2, mtype, ops);
}
