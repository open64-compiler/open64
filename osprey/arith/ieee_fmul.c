/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
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


/*
 *	Portable IEEE 754 floating-point multiplication evaluator
 */

#include "arith.internal.h"



int
ar_ifmul32 (AR_IEEE_32 *x,
			const AR_IEEE_32 *a,
			const AR_IEEE_32 *b,
			int roundmode)
{

	int i, res = AR_STAT_OK;
	unsigned long x_lbits, z_lbits, rbits, carry;
	signed int x_expo;
	AR_IEEE_32 x2, y, y2, z;

	/* If either a or b is a NaN, it's the result. */
	if (IS_IEEE32_NaN(a)) {
		*x = *a;
		return res | AR_STAT_UNDEFINED;
	}
	if (IS_IEEE32_NaN(b)) {
		*x = *b;
		return res | AR_STAT_UNDEFINED;
	}

	/* Test for infinities */
	if (b->expo > AR_IEEE32_MAX_EXPO)
		if (a->expo == 0 && !IS_IEEE32_NZ_COEFF(a)){
			/* zero * inf = quiet NaN */
			QNaNIEEE32 (x);
			return res | AR_STAT_UNDEFINED;
		} else {
			/* anything * infinity = infinity */
			if (i = a->sign ^ b->sign)
				res |= AR_STAT_NEGATIVE;
			*x = *b;
			x->sign = i;
			return res | AR_STAT_OVERFLOW;
		}
	if (a->expo > AR_IEEE32_MAX_EXPO)
		if (b->expo == 0 && !IS_IEEE32_NZ_COEFF(b)){
			/* infinity * zero = quiet NaN */
			QNaNIEEE32 (x);
			return res | AR_STAT_UNDEFINED;
		} else {
			/* infinity * anything = infinity */
			if (i = a->sign ^ b->sign)
				res |= AR_STAT_NEGATIVE;
			*x = *a;
			x->sign = i;
			return res | AR_STAT_OVERFLOW;
		}

	/* Test for denorms (they have zero exponents) to determine the
	 * values of the implicit normalization bits; make them explicit.
	 */
	if (ar_state_register.ar_denorms_trap &&
		((!a->expo && IS_IEEE32_NZ_COEFF(a)) ||
		 (!b->expo && IS_IEEE32_NZ_COEFF(b)))) {
		/* operand is a denorm and denorms cause a trap */
		x->expo = AR_IEEE32_MAX_EXPO + 1;
		return res | AR_STAT_UNDEFINED;
	}
	ZEROIEEE32 (y);
	y.coeff1 = !!a->expo;
	y2 = *a;
	z = *b;
	z_lbits = !!b->expo;
	x_expo = a->expo + b->expo + !a->expo + !b->expo - AR_IEEE32_EXPO_BIAS;
	if (x_expo <= 0)
		x_expo--;
	i = a->sign ^ b->sign;

	/* Sum the pyramid */
	if (z.coeff1 & 1) {
		x2 = *a;
		*x = y;
	}
	else {
		ZEROIEEE32 (*x);
		ZEROIEEE32 (x2);
	}
	x->sign = i;
	x_lbits = z_lbits & y.coeff1;
	SHLEFTIEEE32_2 (y, y2);
	SHRIGHTIEEE32 (z);
	z.coeff0 |= z_lbits << (AR_IEEE32_C0_BITS - 1);
	for (i = 1; i < AR_IEEE32_COEFF_BITS + 1; i++) {

		if (z.coeff1 & 1) {
			carry = 0;
			ADDIEEE32 (x2, carry, x2, y2);
			ADDIEEE32 (*x, carry, *x, y);
			x_lbits += carry;
		}

		SHLEFTIEEE32_2 (y, y2);
		SHRIGHTIEEE32 (z);
	}

	/* Extract rounding bits */
	rbits = x2.coeff0 >> (AR_IEEE32_C0_BITS - AR_IEEE32_ROUND_BITS);
	if (x2.coeff0 & MASKR (AR_IEEE32_C0_BITS - AR_IEEE32_ROUND_BITS) |
		x2.coeff1)
		rbits |= 1; /* sticky bit */

	/* Normalize and round */
	return ar_i32norm (x_expo, x_lbits, rbits, x, roundmode);
}



int
ar_ifmul64 (AR_IEEE_64 *x,
			const AR_IEEE_64 *a,
			const AR_IEEE_64 *b,
			int roundmode)
{

	int i, res = AR_STAT_OK;
	unsigned long x_lbits, z_lbits, rbits, carry;
	signed int x_expo;
	AR_IEEE_64 x2, y, y2, z;

	/* If either a or b is a NaN, it's the result. */
	if (IS_IEEE64_NaN(a)) {
		*x = *a;
		return res | AR_STAT_UNDEFINED;
	}
	if (IS_IEEE64_NaN(b)) {
		*x = *b;
		return res | AR_STAT_UNDEFINED;
	}

	/* Test for infinities */
	if (b->expo > AR_IEEE64_MAX_EXPO)
		if (a->expo == 0 && !IS_IEEE64_NZ_COEFF(a)) {
			/* zero * inf = quiet NaN */
			QNaNIEEE64 (x);
			return res | AR_STAT_UNDEFINED;
		} else {
			/* anything * infinity = infinity */
			if (i = a->sign ^ b->sign)
				res |= AR_STAT_NEGATIVE;
			*x = *b;
			x->sign = i;
			return res | AR_STAT_OVERFLOW;
		}
	if (a->expo > AR_IEEE64_MAX_EXPO)
		if (b->expo == 0 && !IS_IEEE64_NZ_COEFF(b)) {
			/* infinity * zero = quiet NaN */
			QNaNIEEE64 (x);
			return res | AR_STAT_UNDEFINED;
		} else {
			/* infinity * anything = infinity */
			if (i = a->sign ^ b->sign)
				res |= AR_STAT_NEGATIVE;
			*x = *a;
			x->sign = i;
			return res | AR_STAT_OVERFLOW;
		}

	/* Test for denorms (they have zero exponents) to determine the
	 * values of the implicit normalization bits; make them explicit.
	 */
	if (ar_state_register.ar_denorms_trap &&
		((!a->expo && IS_IEEE64_NZ_COEFF(a)) ||
		 (!b->expo && IS_IEEE64_NZ_COEFF(b)))) {
		/* operand is a denorm and denorms cause a trap */
		x->expo = AR_IEEE64_MAX_EXPO + 1;
		return res | AR_STAT_UNDEFINED;
	}
	ZEROIEEE64 (y);
	y.coeff3 = !!a->expo;
	y2 = *a;
	z = *b;
	z_lbits = !!b->expo;
	x_expo = a->expo + b->expo + !a->expo + !b->expo - AR_IEEE64_EXPO_BIAS;
	if (x_expo <= 0)
		x_expo--;
	i = a->sign ^ b->sign;

	/* Sum the pyramid */
	if (z.coeff3 & 1) {
		x2 = *a;
		*x = y;
	}
	else {
		ZEROIEEE64 (*x);
		ZEROIEEE64 (x2);
	}
	x->sign = i;
	x_lbits = z_lbits & y.coeff3;
	SHLEFTIEEE64_2 (y, y2);
	SHRIGHTIEEE64 (z);
	z.coeff0 |= z_lbits << (AR_IEEE64_C0_BITS - 1);
	for (i = 1; i < AR_IEEE64_COEFF_BITS + 1; i++) {

		if (z.coeff3 & 1) {
			carry = 0;
			ADDIEEE64 (x2, carry, x2, y2);
			ADDIEEE64 (*x, carry, *x, y);
			x_lbits += carry;
		}

		SHLEFTIEEE64_2 (y, y2);
		SHRIGHTIEEE64 (z);
	}

	/* Extract rounding bits */
	rbits = x2.coeff0 >> (AR_IEEE64_C0_BITS - AR_IEEE64_ROUND_BITS);
	if (x2.coeff0 & MASKR (AR_IEEE64_C0_BITS - AR_IEEE64_ROUND_BITS) |
		x2.coeff1 | x2.coeff2 | x2.coeff3)
		rbits |= 1; /* sticky bit */

	/* Normalize and round */
	return ar_i64norm (x_expo, x_lbits, rbits, x, roundmode);
}

#ifdef __mips

int
ar_ifmul128(AR_IEEE_128 *x,
			const AR_IEEE_128 *a,
			const AR_IEEE_128 *b,
			int roundmode)
{
   AR_TYPE ty = AR_Float_IEEE_NR_128;
   *(long double *)x = *(long double *)a * *(long double *)b;
   return AR_status((const AR_DATA *) x, &ty);
}

#else

int
ar_ifmul128(AR_IEEE_128 *x,
			const AR_IEEE_128 *a,
			const AR_IEEE_128 *b,
			int roundmode)
{

	int i, res = AR_STAT_OK;
	unsigned long x_lbits, z_lbits, rbits, carry;
	signed int x_expo;
	AR_IEEE_128 x2, y, y2, z;

	/*
	 * Use native arithmetic for MIPS.
	 */
	if (HOST_IS_MIPS) {
		AR_TYPE ty = AR_Float_IEEE_NR_128;

		*(long double *)x = *(long double *)a * *(long double *)b;
		return AR_status((AR_DATA *) x, &ty);
	}

	/* If either a or b is a NaN, it's the result. */
	if (IS_IEEE128_NaN(a)) {
		*x = *a;
		return res | AR_STAT_UNDEFINED;
	}
	if (IS_IEEE128_NaN(b)) {
		*x = *b;
		return res | AR_STAT_UNDEFINED;
	}

	/* Test for infinities */
	if (b->expo > AR_IEEE128_MAX_EXPO)
		if (a->expo == 0 && !IS_IEEE128_NZ_COEFF(a)) {
			/* zero * inf = quiet NaN */
			QNaNIEEE128 (x);
			return res | AR_STAT_UNDEFINED;
		} else {
			/* anything * infinity = infinity */
			if (i = a->sign ^ b->sign)
				res |= AR_STAT_NEGATIVE;
			*x = *b;
			x->sign = i;
			return res | AR_STAT_OVERFLOW;
		}
	if (a->expo > AR_IEEE128_MAX_EXPO)
		if (b->expo == 0 && !IS_IEEE128_NZ_COEFF(b)) {
			/* infinity * zero = quietNaN */
			QNaNIEEE128 (x);
			return res | AR_STAT_UNDEFINED;
		} else {
			/* infinity * anything = infinity */
			if (i = a->sign ^ b->sign)
				res |= AR_STAT_NEGATIVE;
			*x = *a;
			x->sign = i;
			return res | AR_STAT_OVERFLOW;
		}

	/* Test for denorms (they have zero exponents) to determine the
	 * values of the implicit normalization bits; make them explicit.
	 */
	if (ar_state_register.ar_denorms_trap &&
		((!a->expo && IS_IEEE128_NZ_COEFF(a)) ||
		 (!b->expo && IS_IEEE128_NZ_COEFF(b)))) {
		/* operand is a denorm and denorms cause a trap */
		x->expo = AR_IEEE128_MAX_EXPO + 1;
		return res | AR_STAT_UNDEFINED;
	}
	ZEROIEEE128 (y);
	y.coeff6 = !!a->expo;
	y2 = *a;
	z = *b;
	z_lbits = !!b->expo;
	x_expo = a->expo + b->expo + !a->expo + !b->expo - AR_IEEE128_EXPO_BIAS;
	if (x_expo <= 0)
		x_expo--;
	i = a->sign ^ b->sign;

	/* Sum the pyramid */
	if (z.coeff6 & 1) {
		x2 = *a;
		*x = y;
	}
	else {
		ZEROIEEE128 (*x);
		ZEROIEEE128 (x2);
	}
	x->sign = i;
	x_lbits = z_lbits & y.coeff6;
	SHLEFTIEEE128_2 (y, y2);
	SHRIGHTIEEE128 (z);
	z.coeff0 |= z_lbits << (AR_IEEE128_C0_BITS - 1);
	for (i = 1; i < AR_IEEE128_COEFF_BITS + 1; i++) {

		if (z.coeff6 & 1) {
			carry = 0;
			ADDIEEE128 (x2, carry, x2, y2);
			ADDIEEE128 (*x, carry, *x, y);
			x_lbits += carry;
		}

		SHLEFTIEEE128_2 (y, y2);
		SHRIGHTIEEE128 (z);
	}

	/* Extract rounding bits */
	rbits = x2.coeff0 >> (AR_IEEE128_C0_BITS - AR_IEEE128_ROUND_BITS);
	if (x2.coeff0 & MASKR (AR_IEEE128_C0_BITS - AR_IEEE128_ROUND_BITS) |
		x2.coeff1 | x2.coeff2 | x2.coeff3 | x2.coeff3 | x2.coeff5 | x2.coeff6)
		rbits |= 1; /* sticky bit */

	/* Normalize and round */
	return ar_i128norm (x_expo, x_lbits, rbits, x, roundmode);
}
#endif


static char USMID [] = "\n%Z%%M%	%I%	%G% %U%\n";
static char rcsid [] = "$Id: ieee_fmul.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $";
