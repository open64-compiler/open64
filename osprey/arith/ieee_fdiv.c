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
 *	Portable IEEE 754 floating-point division evaluator
 */

#include "arith.internal.h"


int
ar_ifdiv32 (AR_IEEE_32 *x,
			const AR_IEEE_32 *a,
			const AR_IEEE_32 *b,
			int roundmode) {

	int i, s;
	int res = AR_STAT_OK;
	unsigned long x_lbits, y_lbits, z_lbits, rbits, carry;
	signed int x_expo;
	AR_IEEE_32 y, y2, z, z2;

	/* If either a or b is a NaN, it's the result. */
	if (IS_IEEE32_NaN(a)) {
		*x = *a;
		return res | AR_STAT_UNDEFINED;
	}
	if (IS_IEEE32_NaN(b)) {
		*x = *b;
		return res | AR_STAT_UNDEFINED;
	}

	s = a->sign ^ b->sign;
	/* Test for infinities and zeros */
	if (b->expo > AR_IEEE32_MAX_EXPO)
		if (a->expo > AR_IEEE32_MAX_EXPO) {
			/* infinity / infinity = quiet NaN */
			QNaNIEEE32 (x);
			return res | AR_STAT_UNDEFINED;
		} else {
			/* anything / infinity = zero */
			if (s)
				res |= AR_STAT_NEGATIVE;
			ZEROIEEE32 (*x);
			x->sign = s;
			return res | AR_STAT_UNDERFLOW | AR_STAT_ZERO;
		}
	if (a->expo > AR_IEEE32_MAX_EXPO) {
		/* infinity / anything = infinity */
		if (s)
			res |= AR_STAT_NEGATIVE;
		*x = *a;
		x->sign = s;
		return res | AR_STAT_OVERFLOW;
	}
	if (b->expo == 0 && !IS_IEEE32_NZ_COEFF(b)) {
		if (a->expo == 0 && !IS_IEEE32_NZ_COEFF(a)) {
			/* zero/zero = quiet NaN */
			QNaNIEEE32 (x);
			return res | AR_STAT_UNDEFINED;
		} else {
			/* anything / zero = infinity */
			if (s)
				res |= AR_STAT_NEGATIVE;
			ZEROIEEE32 (*x);
			x->sign = s;
			x->expo = AR_IEEE32_MAX_EXPO + 1;
			return res | AR_STAT_OVERFLOW;
		}
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

	y = *a;
	y_lbits = !!a->expo;
	ZEROIEEE32 (y2);
	z = *b;
	z_lbits = !!b->expo;
	ZEROIEEE32 (z2);
	x_lbits = 0;
	x_expo = a->expo - b->expo + !a->expo - !b->expo + AR_IEEE32_EXPO_BIAS;
	ZEROIEEE32 (*x);
	x->sign = s;
	rbits = 0;

	/* Handle division by a denormalized value */
	if (!b->expo) {
		while (!z_lbits) {
			z_lbits = z.coeff0 >> (AR_IEEE32_C0_BITS - 1);
			SHLEFTIEEE32 (z);
			x_expo++;
		}
	}

	if (x_expo <= 0)
		x_expo--;

	/* Divide by repeated subtraction */
	for (i = 0; i < AR_IEEE32_COEFF_BITS + AR_IEEE32_ROUND_BITS + 1; i++) {

		x_lbits = x->coeff0 >> (AR_IEEE32_C0_BITS - 1);
		SHLEFTIEEE32 (*x);
		x->coeff1 |= rbits >> (AR_IEEE32_ROUND_BITS - 1);
		rbits = (rbits << 1) & MASKR (AR_IEEE32_ROUND_BITS);

		/* If scaled denominator <= numerator, subtract */
		if (z_lbits < y_lbits ||
			z_lbits == y_lbits &&
			(z.coeff0 < y.coeff0 ||
			 z.coeff0 == y.coeff0 &&
			 (z.coeff1 < y.coeff1 ||
			  z.coeff1 == y.coeff1 &&
			  (z2.coeff0 < y2.coeff0 ||
			   z2.coeff0 == y2.coeff0 &&
			   z2.coeff1 <= y2.coeff1)))) {

			y2.coeff1 = carry = y2.coeff1 + 1 +
					(z2.coeff1 ^
					 MASKR (AR_IEEE32_C1_BITS));
			carry >>= AR_IEEE32_C1_BITS;
			y2.coeff0 = carry += y2.coeff0 +
					(z2.coeff0 ^
					 MASKR (AR_IEEE32_C0_BITS));
			carry >>= AR_IEEE32_C0_BITS;
			y.coeff1 = carry += y.coeff1 +
					(z.coeff1 ^
					 MASKR (AR_IEEE32_C1_BITS));
			carry >>= AR_IEEE32_C1_BITS;
			y.coeff0 = carry += y.coeff0 +
					(z.coeff0 ^
					 MASKR (AR_IEEE32_C0_BITS));
			carry >>= AR_IEEE32_C0_BITS;
			y_lbits = (y_lbits + carry + (z_lbits ^ 1)) & 1;

			rbits |= 1;
		}

		SHRIGHTIEEE32_2 (z, z2);
		z.coeff0 |= z_lbits << (AR_IEEE32_C0_BITS - 1);
		z_lbits = 0;
	}

	/* Sticky rounding bit */
	if (IS_IEEE32_NZ_COEFF(&y2))
		rbits |= 1;

	/* Normalize, round, and return */
	return ar_i32norm (x_expo, x_lbits, rbits, x, roundmode);
}


int
ar_ifdiv64 (AR_IEEE_64 *x,
			const AR_IEEE_64 *a,
			const AR_IEEE_64 *b,
			int roundmode) {

	int i, s;
	int res = AR_STAT_OK;
	unsigned long x_lbits, y_lbits, z_lbits, rbits, carry;
	signed int x_expo;
	AR_IEEE_64 y, y2, z, z2;

	/* If either a or b is a NaN, it's the result. */
	if (IS_IEEE64_NaN(a)) {
		*x = *a;
		return res | AR_STAT_UNDEFINED;
	}
	if (IS_IEEE64_NaN(b)) {
		*x = *b;
		return res | AR_STAT_UNDEFINED;
	}

	s = a->sign ^ b->sign;
	/* Test for infinities and zeros */
	if (b->expo > AR_IEEE64_MAX_EXPO)
		if (a->expo > AR_IEEE64_MAX_EXPO) {
			/* infinity / infinity = quiet NaN */
			QNaNIEEE64 (x);
			return res | AR_STAT_UNDEFINED;
		} else {
			/* anything / infinity = zero */
			if (s)
				res |= AR_STAT_NEGATIVE;
			ZEROIEEE64 (*x);
			x->sign = s;
			return res | AR_STAT_UNDERFLOW | AR_STAT_ZERO;
		}
	if (a->expo > AR_IEEE64_MAX_EXPO) {
		/* infinity / anything = infinity */
		if (s)
			res |= AR_STAT_NEGATIVE;
		*x = *a;
		x->sign = s;
		return res | AR_STAT_OVERFLOW;
	}
	if (b->expo == 0 && !IS_IEEE64_NZ_COEFF(b)) {
		if (a->expo == 0 && !IS_IEEE64_NZ_COEFF(a)) {
			/* zero/zero = quiet NaN */
			QNaNIEEE64 (x);
			return res | AR_STAT_UNDEFINED;
		} else {
			/* anything / zero = infinity */
			if (s)
				res |= AR_STAT_NEGATIVE;
			ZEROIEEE64 (*x);
			x->sign = s;
			x->expo = AR_IEEE64_MAX_EXPO + 1;
			return res | AR_STAT_OVERFLOW;
		}
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

	y = *a;
	y_lbits = !!a->expo;
	ZEROIEEE64 (y2);
	z = *b;
	z_lbits = !!b->expo;
	ZEROIEEE64 (z2);
	x_lbits = 0;
	x_expo = a->expo - b->expo + !a->expo - !b->expo + AR_IEEE64_EXPO_BIAS;
	ZEROIEEE64 (*x);
	x->sign = s;
	rbits = 0;

	/* Handle division by a denormalized value */
	if (!b->expo) {
		while (!z_lbits) {
			z_lbits = z.coeff0 >> (AR_IEEE64_C0_BITS - 1);
			SHLEFTIEEE64 (z);
			x_expo++;
		}
	}

	if (x_expo <= 0)
		x_expo--;

	/* Divide by repeated subtraction */
	for (i = 0; i < AR_IEEE64_COEFF_BITS + AR_IEEE64_ROUND_BITS + 1; i++) {

		x_lbits = x->coeff0 >> (AR_IEEE64_C0_BITS - 1);
		SHLEFTIEEE64 (*x);
		x->coeff3 |= rbits >> (AR_IEEE64_ROUND_BITS - 1);
		rbits = (rbits << 1) & MASKR (AR_IEEE64_ROUND_BITS);

		/* If scaled denominator <= numerator, subtract */
		if (z_lbits < y_lbits ||
			z_lbits == y_lbits &&
			(z.coeff0 < y.coeff0 ||
			 z.coeff0 == y.coeff0 &&
			 (z.coeff1 < y.coeff1 ||
			  z.coeff1 == y.coeff1 &&
			  (z.coeff2 < y.coeff2 ||
			   z.coeff2 == y.coeff2 &&
			   (z.coeff3 < y.coeff3 ||
			z.coeff3 == y.coeff3 &&
			(z2.coeff0 < y2.coeff0 ||
			 z2.coeff0 == y2.coeff0 &&
			 (z2.coeff1 < y2.coeff1 ||
			  z2.coeff1 == y2.coeff1 &&
			  (z2.coeff2 < y2.coeff2 ||
			   z2.coeff2 == y2.coeff2 &&
			   z2.coeff3 <= y2.coeff3)))))))) {

			y2.coeff3 = carry = y2.coeff3 + 1 +
				(z2.coeff3 ^
				 MASKR (AR_IEEE64_C3_BITS));
			carry >>= AR_IEEE64_C3_BITS;
			y2.coeff2 = carry += y2.coeff2 +
					(z2.coeff2 ^
					 MASKR (AR_IEEE64_C2_BITS));
			carry >>= AR_IEEE64_C2_BITS;
			y2.coeff1 = carry += y2.coeff1 +
					(z2.coeff1 ^
					 MASKR (AR_IEEE64_C1_BITS));
			carry >>= AR_IEEE64_C1_BITS;
			y2.coeff0 = carry += y2.coeff0 +
					(z2.coeff0 ^
					 MASKR (AR_IEEE64_C0_BITS));
			carry >>= AR_IEEE64_C0_BITS;
			y.coeff3 = carry += y.coeff3 +
					(z.coeff3 ^
					 MASKR (AR_IEEE64_C3_BITS));
			carry >>= AR_IEEE64_C3_BITS;
			y.coeff2 = carry += y.coeff2 +
					(z.coeff2 ^
					 MASKR (AR_IEEE64_C2_BITS));
			carry >>= AR_IEEE64_C2_BITS;
			y.coeff1 = carry += y.coeff1 +
					(z.coeff1 ^
					 MASKR (AR_IEEE64_C1_BITS));
			carry >>= AR_IEEE64_C1_BITS;
			y.coeff0 = carry += y.coeff0 +
					(z.coeff0 ^
					 MASKR (AR_IEEE64_C0_BITS));
			carry >>= AR_IEEE64_C0_BITS;
			y_lbits = (y_lbits + carry + (z_lbits ^ 1)) & 1;

			rbits |= 1;
		}

		SHRIGHTIEEE64_2 (z, z2);
		z.coeff0 |= z_lbits << (AR_IEEE64_C0_BITS - 1);
		z_lbits = 0;
	}

	/* Sticky rounding bit */
	if (IS_IEEE64_NZ_COEFF(&y2))
		rbits |= 1;

	/* Normalize, round, and return */
	return ar_i64norm (x_expo, x_lbits, rbits, x, roundmode);
}


int
ar_ifdiv128(AR_IEEE_128 *x,
		   const AR_IEEE_128 *a,
		   const AR_IEEE_128 *b,
		   int roundmode) {

	int i, s;
	int res = AR_STAT_OK;
	unsigned long x_lbits, y_lbits, z_lbits, rbits, carry;
	signed int x_expo;
	AR_IEEE_128 y, y2, z, z2;

	/*
	 * Use native arithmetic for MIPS.
	 */
	if (HOST_IS_MIPS) {
		AR_TYPE ty = AR_Float_IEEE_NR_128;

		*(long double *)x = *(long double *)a / *(long double *)b;
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

	s = a->sign ^ b->sign;
	/* Test for infinities and zeros */
	if (b->expo > AR_IEEE128_MAX_EXPO)
		if (a->expo > AR_IEEE128_MAX_EXPO) {
			/* infinity / infinity = quiet NaN */
			QNaNIEEE128 (x);
			return res | AR_STAT_UNDEFINED;
		} else {
			/* anything / infinity = zero */
			if (s)
				res |= AR_STAT_NEGATIVE;
			ZEROIEEE128 (*x);
			x->sign = s;
			return res | AR_STAT_UNDERFLOW | AR_STAT_ZERO;
		}
	if (a->expo > AR_IEEE128_MAX_EXPO) {
		/* infinity / anything = infinity */
		if (s)
			res |= AR_STAT_NEGATIVE;
		*x = *a;
		x->sign = s;
		return res | AR_STAT_OVERFLOW;
	}
	if (b->expo == 0 && !IS_IEEE128_NZ_COEFF(b)) {
		if (a->expo == 0 && !IS_IEEE128_NZ_COEFF(a)) {
			/* zero/zero = quiet NaN */
			QNaNIEEE128 (x);
			return res | AR_STAT_UNDEFINED;
		} else {
			/* anything / zero = infinity */
			if (s)
				res |= AR_STAT_NEGATIVE;
			ZEROIEEE128 (*x);
			x->sign = s;
			x->expo = AR_IEEE128_MAX_EXPO + 1;
			return res | AR_STAT_OVERFLOW;
		}
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

	y = *a;
	y_lbits = !!a->expo;
	ZEROIEEE128 (y2);
	z = *b;
	z_lbits = !!b->expo;
	ZEROIEEE128 (z2);
	x_lbits = 0;
	x_expo = a->expo - b->expo + !a->expo - !b->expo + AR_IEEE128_EXPO_BIAS;
	ZEROIEEE128 (*x);
	x->sign = s;
	rbits = 0;

	/* Handle division by a denormalized value */
	if (!b->expo) {
		while (!z_lbits) {
			z_lbits = z.coeff0 >> (AR_IEEE128_C0_BITS - 1);
			SHLEFTIEEE128 (z);
			x_expo++;
		}
	}

	if (x_expo <= 0)
		x_expo--;

	/* Divide by repeated subtraction */
	for (i = 0; i < AR_IEEE128_COEFF_BITS + AR_IEEE128_ROUND_BITS + 1; i++) {

		x_lbits = x->coeff0 >> (AR_IEEE128_C0_BITS - 1);
		SHLEFTIEEE128 (*x);
		x->coeff6 |= rbits >> (AR_IEEE128_ROUND_BITS - 1);
		rbits = (rbits << 1) & MASKR (AR_IEEE128_ROUND_BITS);

		/* If scaled denominator <= numerator, subtract */
		if (z_lbits < y_lbits ||
			z_lbits == y_lbits &&
			(z.coeff0 < y.coeff0 ||
			 z.coeff0 == y.coeff0 &&
			 (z.coeff1 < y.coeff1 ||
			  z.coeff1 == y.coeff1 &&
			  (z.coeff2 < y.coeff2 ||
			   z.coeff2 == y.coeff2 &&
			   (z.coeff3 < y.coeff3 ||
				z.coeff3 == y.coeff3 &&
				(z.coeff4 < y.coeff4 ||
				 z.coeff4 == y.coeff4 &&
				 (z.coeff5 < y.coeff5 ||
				  z.coeff5 == y.coeff5 &&
				  (z.coeff6 < y.coeff6 ||
				   z.coeff6 == y.coeff6 &&
				   (z2.coeff0 < y2.coeff0 || 
					z2.coeff0 == y2.coeff0 &&
					(z2.coeff1 < y2.coeff1 ||
					 z2.coeff1 == y2.coeff1 &&
					 (z2.coeff2 < y2.coeff2 ||
					  z2.coeff2 == y2.coeff2 &&
					  (z2.coeff3 < y2.coeff3 ||
					   z2.coeff3 == y2.coeff3 &&
					   (z2.coeff4 < y2.coeff4 ||
						z2.coeff4 == y2.coeff4 &&
						(z2.coeff5 < y2.coeff5 ||
						 z2.coeff5 == y2.coeff5 &&
						 z2.coeff6 <= y2.coeff6)))))))))))))) {

			y2.coeff6 = carry  = y2.coeff6 + 1 +
					(z2.coeff6 ^
					 MASKR (AR_IEEE128_C6_BITS));
			carry >>= AR_IEEE128_C6_BITS;
			y2.coeff5 = carry += y2.coeff5 +
					(z2.coeff5 ^
					 MASKR (AR_IEEE128_C5_BITS));
			carry >>= AR_IEEE128_C5_BITS;
			y2.coeff4 = carry += y2.coeff4 +
					(z2.coeff4 ^
					 MASKR (AR_IEEE128_C4_BITS));
			carry >>= AR_IEEE128_C4_BITS;
			y2.coeff3 = carry += y2.coeff3 +
					(z2.coeff3 ^
					 MASKR (AR_IEEE128_C3_BITS));
			carry >>= AR_IEEE128_C3_BITS;
			y2.coeff2 = carry += y2.coeff2 +
					(z2.coeff2 ^
					 MASKR (AR_IEEE128_C2_BITS));
			carry >>= AR_IEEE128_C2_BITS;
			y2.coeff1 = carry += y2.coeff1 +
					(z2.coeff1 ^
					 MASKR (AR_IEEE128_C1_BITS));
			carry >>= AR_IEEE128_C1_BITS;
			y2.coeff0 = carry += y2.coeff0 +
					(z2.coeff0 ^
					 MASKR (AR_IEEE128_C0_BITS));
			carry >>= AR_IEEE128_C0_BITS;
			y.coeff6 = carry += y.coeff6 +
					(z.coeff6 ^
					 MASKR (AR_IEEE128_C6_BITS));
			carry >>= AR_IEEE128_C6_BITS;
			y.coeff5 = carry += y.coeff5 +
					(z.coeff5 ^
					 MASKR (AR_IEEE128_C5_BITS));
			carry >>= AR_IEEE128_C5_BITS;
			y.coeff4 = carry += y.coeff4 +
					(z.coeff4 ^
					 MASKR (AR_IEEE128_C4_BITS));
			carry >>= AR_IEEE128_C3_BITS;
			y.coeff3 = carry += y.coeff3 +
					(z.coeff3 ^
					 MASKR (AR_IEEE128_C3_BITS));
			carry >>= AR_IEEE128_C3_BITS;
			y.coeff2 = carry += y.coeff2 +
					(z.coeff2 ^
					 MASKR (AR_IEEE128_C2_BITS));
			carry >>= AR_IEEE128_C2_BITS;
			y.coeff1 = carry += y.coeff1 +
					(z.coeff1 ^
					 MASKR (AR_IEEE128_C1_BITS));
			carry >>= AR_IEEE128_C1_BITS;
			y.coeff0 = carry += y.coeff0 +
					(z.coeff0 ^
					 MASKR (AR_IEEE128_C0_BITS));
			carry >>= AR_IEEE128_C0_BITS;
			y_lbits = (y_lbits + carry + (z_lbits ^ 1)) & 1;

			rbits |= 1;
		}

		SHRIGHTIEEE128_2 (z, z2);
		z.coeff0 |= z_lbits << (AR_IEEE128_C0_BITS - 1);
		z_lbits = 0;
	}

	/* Sticky rounding bit */
	if (IS_IEEE128_NZ_COEFF(&y2))
		rbits |= 1;

	/* Normalize, round, and return */
	return ar_i128norm (x_expo, x_lbits, rbits, x, roundmode);
}


static char USMID [] = "\n%Z%%M%	%I%	%G% %U%\n";
static char rcsid [] = "$Id: ieee_fdiv.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $";
