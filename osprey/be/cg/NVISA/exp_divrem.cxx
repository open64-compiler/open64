/*
 * Copyright 2005-2008 NVIDIA Corporation.  All rights reserved.
 */

/*
 * Copyright 2003, 2004, 2005 PathScale, Inc.  All Rights Reserved.
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
    Expand_Mtype_Immediate (t2, Gen_Literal_TN (absdvsr - 1, is_double?8:4), 
		mtype, ops);
    Expand_Binary_And( t3, t1, t2, mtype, ops );
    Expand_Add( t4, t3, numer, mtype, ops );
    Expand_Shift(t5, t4, Gen_Literal_TN(n, 4), mtype, 
		 shift_aright, ops);
    if (dvsr < 0) Expand_Neg(result, t5, mtype, ops);
  }
}

/* gen_pseudo_inverse_32bit
 * 
 * Parameters : ld    - The 32 bit unsigned denominator
 *              addp  - pointer to predicate which indicates additional
 *                      instructions are needed for precision.
 *              shift - pointer where the amount to shift is returned
 * Return     : The magic number m which satisfies floor(n/b) =floor(m*n/2^(n+l))   
 *
 * Usage      : Given unsigned (n / b)
 *              d = magic number(b)
 *              m = multiply high (n * d)
 *              if (addp)
 *                 (((m - n) >> 1) + m) >> shift-1
 *              else
 *                 m >> shift
 *             
 * See Division by Invariant Integers using Multiplication by Granlund and Montgomery
 * for an explanation of the algorithm.
 *
 * Note: This generator is being used in place of the IA64 version which is better
 * suited for 64 bit math.
 */

UINT64 gen_pseudo_inverse_32bit (UINT64 ld, BOOL *addp, INT64 *shift)
{
    int  p;
    UINT32 nc, delta, q1, r1, q2, r2, d;

    d = (unsigned) ld;
    
    *addp = false;

    nc = -1 - (-d) %d;    
    p = 31;
    q1 = 1U << 31;
    r1 = q1;
    q2 = ~q1;
    r2 = q2;
    
    q1 /= nc;
    r1 -= q1 *nc;
    q2 /= d;
    r2 -= q2 * d;
    do
    {
        p++;
        if (r1 >= (nc - r1))
        {
            q1 = 2*q1 +1;
            r1 = 2*r1 - nc;
        }
        else
        {
            q1 = 2*q1;
            r1 = 2*r1;
        }
        if (r2 + 1 >= d - r2)
        {
            if (q2 >= ~(1U << 31))
                *addp = true;
            q2 = 2*q2+1;
            r2 = 2*r2+1-d;
        }
        else
        {
            if (q2 >= (1U<< 31))
                *addp = true;
            q2 = 2*q2;
            r2 = 2*r2+1;
        }
        delta = d - 1 - r2;
    } while (p < 64 && (q1 < delta | (q1 == delta && r1 == 0)));
    *shift = p  - 32;
    return q2 + 1;
}

UINT log2 (UINT i)
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
  BOOL addp;
  BOOL is_odd;
  TN *abs_tn;
  TN *tmp_tn;
  TN *d_tn;
  TN *mult_tn;
  TN *shift_tn;
  TOP opc;
  TN *p1;
  BOOL is_double = MTYPE_is_size_double(mtype);
  BOOL is_signed = MTYPE_is_signed(mtype);

  FmtAssert(!is_double, ("recip constant gen only supports 32 bits currently"));
  
  /* Handle the trivial ones */
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

  /* Look for simple shift optimizations: */
   if (Is_Power_Of_2( denom_val, mtype))
  {
      Expand_Power_Of_2_Divide(result, numer_tn, denom_val, mtype, ops);
      return TRUE;
  }
 
  if (!CGEXP_cvrt_int_div_to_mult) return FALSE;
  
  if (is_signed)
  {
      b = denom_val<0 ? -denom_val : denom_val;       // b = |denom_val|
      is_odd = (b&1);

      d = gen_pseudo_inverse_32bit (b, &addp, &n);
      
      if (n > (is_double ? 63 : 31))
          return FALSE;          /* OOPS! The shift count can't be bigger than the word size! */

      d_tn = Build_TN_Of_Mtype (mtype);
      Expand_Mtype_Immediate (d_tn, Gen_Literal_TN (d , (is_double ? 8 : 4)), mtype, ops);

      /* Set predicate on sign */
      TN *p1;
      TOP opc = (TOP) (TOP_setp_lt_s8 + Mtype_Index(mtype));
      TN *zero_tn = Expand_Mtype_Immediate_Into_Register (
                     Gen_Literal_TN (0, (is_double ? 8 : 4)),
                     is_double ? MTYPE_I8 : MTYPE_I4,
                     ops);
      
      p1 = Build_RCLASS_TN (ISA_REGISTER_CLASS_predicate);
      Build_OP (opc, p1, numer_tn, zero_tn, ops);
      
      /* Generate the absolute value of the numerator: */
      abs_tn = Build_TN_Of_Mtype (mtype);
      Build_OP ((is_double ? TOP_abs_s64 : TOP_abs_s32), abs_tn, numer_tn, ops);      

      /* Generate a multiply upper: */
      mult_tn = Build_TN_Of_Mtype (is_double ? MTYPE_U8 : MTYPE_U4);
      Expand_High_Multiply (mult_tn, abs_tn, d_tn, (is_double ? MTYPE_U8 : MTYPE_U4), ops);

      shift_tn = Build_TN_Of_Mtype (mtype);
      if (addp)
      {
          /* Odd divisors need full precision and, hence, extra instructions. */
          TN *tmp1_tn = Build_TN_Of_Mtype (mtype);
          TN *tmp2_tn = Build_TN_Of_Mtype (mtype);
          TN *tmp3_tn = Build_TN_Of_Mtype (mtype);
          
          Build_OP ((is_double ? TOP_sub_u64 : TOP_sub_u32), tmp1_tn, abs_tn, mult_tn, ops);
          Expand_Shift (tmp2_tn, tmp1_tn, Gen_Literal_TN (1, 4), mtype, shift_lright, ops);
          Build_OP ((is_double ? TOP_add_u64 : TOP_add_u32), tmp3_tn, tmp2_tn, mult_tn, ops);
          Expand_Shift (shift_tn, tmp3_tn, Gen_Literal_TN(n-1, 4), mtype, shift_aright, ops);
      }
      else
      {
          Expand_Shift (shift_tn, mult_tn, Gen_Literal_TN(n, 4), mtype, shift_aright, ops);  
      }
 
      /* Select positive or negated result: */
      if (denom_val < 0)
      {
          Build_OP ((is_double ? TOP_sub_s64_np : TOP_sub_s32_np),
                    shift_tn, zero_tn, shift_tn, p1, ops);
      }
      else
      {
          Build_OP ((is_double ? TOP_sub_s64_p : TOP_sub_s32_p),
                    shift_tn, zero_tn, shift_tn, p1, ops);
      }

      Build_OP ((is_double ? TOP_mov_s64 : TOP_mov_s32), result, shift_tn, ops);      
      /* End Signed */
  } 
  else 
  {
      /* Unsigned */ 
      b = denom_val;
      is_odd = (b&1);

      d = gen_pseudo_inverse_32bit (b, &addp, &n);

      if (n > (is_double ? 63 : 31))
          return FALSE;          /* OOPS! The shift count can't be bigger than the word size! */

      d_tn = Build_TN_Of_Mtype (mtype);
      Expand_Mtype_Immediate (d_tn, Gen_Literal_TN (d, (is_double ? 8 : 4)), mtype, ops);

      /* Generate a multiply upper: */
      mult_tn = Build_TN_Of_Mtype (is_double ? MTYPE_U8 : MTYPE_U4);
      Expand_High_Multiply (mult_tn, numer_tn, d_tn, (is_double ? MTYPE_U8 : MTYPE_U4), ops);

      if (addp)
      {
          /* Odd divisors need full precision and, hence, extra instructions. */
          TN *tmp1_tn = Build_TN_Of_Mtype (mtype);
          TN *tmp2_tn = Build_TN_Of_Mtype (mtype);
          TN *tmp3_tn = Build_TN_Of_Mtype (mtype);
          
          Build_OP ((is_double ? TOP_sub_u64 : TOP_sub_u32), tmp1_tn, numer_tn, mult_tn, ops);
          Expand_Shift (tmp2_tn, tmp1_tn, Gen_Literal_TN (1, 4), mtype, shift_lright, ops);
          Build_OP ((is_double ? TOP_add_u64 : TOP_add_u32), tmp3_tn, tmp2_tn, mult_tn, ops);
          Expand_Shift (result, tmp3_tn, Gen_Literal_TN (n-1, 4), mtype, shift_lright, ops);
      }
      else
      {          
          /* Generate and attach the shift: */
          Expand_Shift (result, mult_tn, Gen_Literal_TN (n, 4), mtype, shift_lright, ops);
      }
      
  } /* end Unsigned */
  return TRUE;
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
  TOP opc = (TOP) (TOP_div_s8 + Mtype_Index(mtype));
  INT64 val;
  BOOL is_double = MTYPE_is_size_double(mtype);

  FmtAssert(!TN_is_constant(src1), ("NYI"));
  
  /* Look for simple shift optimizations and multiply_hi optimizations: */
  INT64 src2_val;
  BOOL const_src2 = TN_Value_At_Op (src2, NULL, &src2_val);

  /* Disable for 64bit. Currently emulated and inserts too many instructions
   * If and when 64bit math it supported in hardware re-enable. Also see
   * MOD below
   */
  if ((const_src2) && (!is_double))
  {
      if (Expand_Integer_Divide_By_Constant(result, src1, src2_val, mtype, ops))
          return NULL; 
  }

  if (TN_Can_Use_Constant_Value (src2, mtype, &val)) {
      // Should we convert to shifts, or let OCG do that?
      // For now, we'll defer to OCG.
      opc = (TOP) (TOP_div_s8_lit + Mtype_Index(mtype));
      src2 = Gen_Literal_TN_Of_Mtype (val, mtype);
  }
  else if (TN_is_constant(src2)) {
      // expand add a,const into mov t,const; op a,t;
      src2 = Expand_Mtype_Immediate_Into_Register (src2, mtype, ops);
  }
  Build_OP (opc, result, src1, src2, ops);
}


void
Expand_Rem (TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  // same?
    Expand_Mod(result, src1, src2, mtype, ops);
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
  TOP opc = (TOP) (TOP_rem_s8 + Mtype_Index(mtype));
  INT64 val;
  BOOL is_double = MTYPE_is_size_double(mtype);
  TN *mult_tn;
  TN *div_tn;
  
  FmtAssert(!TN_is_constant(src1), ("NYI"));

  INT64 src2_val;
  BOOL const_src2 = TN_Value_At_Op (src2, NULL, &src2_val);

  /* Attempt to calculate remainder via :
   * q = n/d     ( use int divide by constant )
   * r = n - q*d
   * 
   * Note : Currently optimization disabled for 64bit. See above
   */
  if ((const_src2) && (!is_double))
  {
      div_tn = Build_TN_Of_Mtype (mtype);
      if (Expand_Integer_Divide_By_Constant(div_tn, src1, src2_val, mtype, ops))
      {      
          /* It's possible the divide above might abort so skip
           * to default behaivor in outer scope if false*/
          mult_tn = Build_TN_Of_Mtype (mtype);
          Expand_Multiply (mult_tn, div_tn, src2, mtype, ops);
      
          Build_OP ((TOP) (TOP_sub_s8 + Mtype_Index(mtype)), result, src1, mult_tn, ops);          
          return;
      }
  }

  if (TN_Can_Use_Constant_Value (src2, mtype, &val)) {
      // Should we convert to shifts, or let OCG do that?
      // For now, we'll defer to OCG.
      opc = (TOP) (TOP_rem_s8_lit + Mtype_Index(mtype));
      src2 = Gen_Literal_TN_Of_Mtype (val, mtype);
  }
  else if (TN_is_constant(src2)) {
      // expand add a,const into mov t,const; op a,t;
      src2 = Expand_Mtype_Immediate_Into_Register (src2, mtype, ops);
  }
  
  Build_OP (opc, result, src1, src2, ops);
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

  FmtAssert(FALSE, ("Not Yet Implemented"));
}
