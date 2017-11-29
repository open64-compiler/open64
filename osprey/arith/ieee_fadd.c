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
 *	Portable IEEE 754 floating-point addition and subtraction
 */

#include "arith.internal.h"


int
ar_ifadd32 (AR_IEEE_32 *x,
			const AR_IEEE_32 *a,
			const AR_IEEE_32 *b,
			int roundmode)
{

	int res = AR_STAT_OK;
	unsigned int lbits, y_lbits, rbits;
	unsigned int shift, diffsign, carry, x_expo;
	AR_IEEE_32 y;

	/* Ensure that the first argument has the largest exponent,
	 * for simplicity.
	 */
	if (b->expo > a->expo)
		y = *a, *x = *b;
	else
		y = *b, *x = *a;
	x_expo = x->expo;

	/* If either x or y is a NaN, it's the result. */
	if (IS_IEEE32_NaN(x)) {
		return res | AR_STAT_UNDEFINED;
	}
	if (IS_IEEE32_NaN(&y)) {
		*x = y;
		return res | AR_STAT_UNDEFINED;
	}

	/* Test for infinities */
	if (x_expo > AR_IEEE32_MAX_EXPO)
		if (y.expo > AR_IEEE32_MAX_EXPO)
			if (x->sign == y.sign) {
				/* infinities of same sign - return infinity */
				if (x->sign)
					res |= AR_STAT_NEGATIVE;
				return res | AR_STAT_OVERFLOW;
			} else {
				/* infinities of different signs - quiet NaN */
				QNaNIEEE32 (x);
				return res | AR_STAT_UNDEFINED;
			}
		else {
			/* infinity + finite quantity - return infinity */
			if (x->sign)
				res |= AR_STAT_NEGATIVE;
			return res | AR_STAT_OVERFLOW;
		}
	else if (y.expo > AR_IEEE32_MAX_EXPO) {
		*x = y;
		if (x->sign)
			res |= AR_STAT_NEGATIVE;
		return res | AR_STAT_OVERFLOW;
	}

	/* Test for denorms (they have zero exponents) to determine the
	 * values of the implicit normalization bits.
	 */
	if (ar_state_register.ar_denorms_trap &&
		((!x_expo && IS_IEEE32_NZ_COEFF(x)) ||
		 (!y.expo && IS_IEEE32_NZ_COEFF(&y)))) {
		/* operand is a denorm and denorms cause a trap */
		x->expo = AR_IEEE32_MAX_EXPO + 1;
		return res | AR_STAT_UNDEFINED;
	}
	lbits = !!x_expo;
	y_lbits = !!y.expo;

	/* Shift the coefficient of the second argument down (right). We
	 * do this in parts so as not to overflow registers.
	 */
	rbits = 0;
	shift = (x_expo - lbits) - (y.expo - y_lbits);
	if (shift > AR_IEEE32_COEFF_BITS + AR_IEEE32_ROUND_BITS) {
		rbits = !!(y.coeff0 | y.coeff1);
		y.sign = y.expo = y_lbits = 0;
		y.coeff0 = y.coeff1 = 0;
	} else
		for (; shift; shift--) {
			/* Sticky bit shifting */
			rbits = (rbits & 1) | (rbits >> 1) |
				((y.coeff1 & 1) << (AR_IEEE32_ROUND_BITS-1));
			SHRIGHTIEEE32 (y);
			y.coeff0 |= y_lbits << (AR_IEEE32_C0_BITS - 1);
			y_lbits = 0;
		}

	/* If signs differ, complement the first argument; this is equivalent
	 * to negating and then subtracting one, in a two's complement sense.
	 */
	if (diffsign = (x->sign ^ y.sign)) {
		lbits ^= MASKR (2);
		NOTIEEE32 (*x);
	}

	/* Compute sum of coefficients */
	if (diffsign) {
		rbits += MASKR (AR_IEEE32_ROUND_BITS);
		carry = rbits >> AR_IEEE32_ROUND_BITS;
		rbits &= MASKR (AR_IEEE32_ROUND_BITS);
	} else
		carry = 0;
	ADDIEEE32 (*x, carry, *x, y);
	lbits = (carry + lbits + y_lbits) & MASKR (2);

	/* Opposite signs */
	if (diffsign)
		if (lbits & 2) {
			/* Complement */
			lbits ^= MASKR (2);
			NOTIEEE32 (*x);
			rbits ^= MASKR (AR_IEEE32_ROUND_BITS);
		} else {
			/* End-around carry */
			x->sign ^= 1;
			carry = 1 + rbits;
			rbits = carry & MASKR (AR_IEEE32_ROUND_BITS);
			carry >>= AR_IEEE32_ROUND_BITS;
			INCIEEE32 (*x, carry);
			lbits = (carry + lbits) & MASKR (2);
		}

	/* Shift rightward if carry, or if undenormalized */
	if (lbits & 2 || lbits && !x_expo) {
		rbits = (rbits >> 1) | rbits & 1 |
			((x->coeff1 & 1) << (AR_IEEE32_ROUND_BITS - 1));
		SHRIGHTIEEE32 (*x);
		x->coeff0 |= lbits << (AR_IEEE32_C0_BITS - 1);
		lbits >>= 1;
		if (x_expo)
			x_expo++;
		else
			x_expo = 2;
	}

	/* Check for a zero result, as a special case */
	if (!(lbits | x->coeff0 | x->coeff1 | rbits)) {
		x->sign &= !diffsign;	/* -0 + -0 == -0 */
		x->expo = 0;
		if (x->sign)
			res |= AR_STAT_NEGATIVE;
		return res | AR_STAT_ZERO;
	}

	return ar_i32norm (x_expo, lbits, rbits, x, roundmode);
}


int
ar_ifsub32 (AR_IEEE_32 *x,
			const AR_IEEE_32 *a,
			const AR_IEEE_32 *b,
			int roundmode)
{

	AR_IEEE_32 nb;

	/* If either a or b is a NaN, it's the result. */
	if (IS_IEEE32_NaN(a)) {
		*x = *a;
		return AR_STAT_UNDEFINED;
	}
	if (IS_IEEE32_NaN(b)) {
		*x = *b;
		return AR_STAT_UNDEFINED;
	}

	nb = *b;
	nb.sign ^= 1;
	return ar_ifadd32 (x, a, &nb, roundmode);
}


int
ar_ifadd64 (AR_IEEE_64 *x,
			const AR_IEEE_64 *a,
			const AR_IEEE_64 *b,
			int roundmode)
{

	int res = AR_STAT_OK;
	unsigned int lbits, y_lbits, rbits;
	unsigned int shift, diffsign, carry, x_expo;
	AR_IEEE_64 y;

	/* Ensure that the first argument has the largest exponent,
	 * for simplicity.
	 */
	if (b->expo > a->expo)
		y = *a, *x = *b;
	else
		y = *b, *x = *a;
	x_expo = x->expo;

	/* If either x or y is a NaN, it's the result. */
	if (IS_IEEE64_NaN(x)) {
		return res | AR_STAT_UNDEFINED;
	}
	if (IS_IEEE64_NaN(&y)) {
		*x = y;
		return res | AR_STAT_UNDEFINED;
	}

	/* Test for infinities */
	if (x_expo > AR_IEEE64_MAX_EXPO)
		if (y.expo > AR_IEEE64_MAX_EXPO)
			if (x->sign == y.sign) {
				/* infinities of same sign - return infinity */
				if (x->sign)
					res |= AR_STAT_NEGATIVE;
				return res | AR_STAT_OVERFLOW;
			} else {
				/* infinities of different signs - quiet NaN */
				QNaNIEEE64 (x);
				return res | AR_STAT_UNDEFINED;
			}
		else {
			/* infinity + finite quantity - return infinity */
			if (x->sign)
				res |= AR_STAT_NEGATIVE;
			return res | AR_STAT_OVERFLOW;
		}
	else if (y.expo > AR_IEEE64_MAX_EXPO) {
		*x = y;
		if (x->sign)
			res |= AR_STAT_NEGATIVE;
		return res | AR_STAT_OVERFLOW;
	}

	/* Test for denorms (they have zero exponents) to determine the
	 * values of the implicit normalization bits.
	 */
	if (ar_state_register.ar_denorms_trap &&
		((!x_expo && IS_IEEE64_NZ_COEFF(x)) ||
		 (!y.expo && IS_IEEE64_NZ_COEFF(&y)))) {
		/* operand is a denorm and denorms cause a trap */
		x->expo = AR_IEEE64_MAX_EXPO + 1;
		return res | AR_STAT_UNDEFINED;
	}
	lbits = !!x_expo;
	y_lbits = !!y.expo;

	/* Shift the coefficient of the second argument down (right). We
	 * do this in parts so as not to overflow registers.
	 */
	rbits = 0;
	shift = (x_expo - lbits) - (y.expo - y_lbits);
	if (shift > AR_IEEE64_COEFF_BITS + AR_IEEE64_ROUND_BITS) {
		rbits = !!(y.coeff0 | y.coeff1 | y.coeff2 | y.coeff3);
		y.sign = y.expo = y_lbits = 0;
		y.coeff0 = y.coeff1 = y.coeff2 = y.coeff3 = 0;
	} else
		for (; shift; shift--) {
			/* Sticky bit shifting */
			rbits = (rbits & 1) | (rbits >> 1) |
				((y.coeff3 & 1) << (AR_IEEE64_ROUND_BITS-1));
			SHRIGHTIEEE64 (y);
			y.coeff0 |= y_lbits << (AR_IEEE64_C0_BITS - 1);
			y_lbits = 0;
		}

	/* If signs differ, complement the first argument; this is equivalent
	 * to negating and then subtracting one, in a two's complement sense.
	 */
	if (diffsign = (x->sign ^ y.sign)) {
		lbits ^= MASKR (2);
		NOTIEEE64 (*x);
	}

	/* Compute sum of coefficients */
	if (diffsign) {
		rbits += MASKR (AR_IEEE64_ROUND_BITS);
		carry = rbits >> AR_IEEE64_ROUND_BITS;
		rbits &= MASKR (AR_IEEE64_ROUND_BITS);
	} else
		carry = 0;
	ADDIEEE64 (*x, carry, *x, y);
	lbits = (carry + lbits + y_lbits) & MASKR (2);

	/* Opposite signs */
	if (diffsign)
		if (lbits & 2) {
			/* Complement */
			lbits ^= MASKR (2);
			NOTIEEE64 (*x);
			rbits ^= MASKR (AR_IEEE64_ROUND_BITS);
		} else {
			/* End-around carry */
			x->sign ^= 1;
			carry = 1 + rbits;
			rbits = carry & MASKR (AR_IEEE64_ROUND_BITS);
			carry >>= AR_IEEE64_ROUND_BITS;
			INCIEEE64 (*x, carry);
			lbits = (carry + lbits) & MASKR (2);
		}

	/* Shift rightward if carry, or if undenormalized */
	if (lbits & 2 || lbits && !x_expo) {
		rbits = (rbits >> 1) | rbits & 1 |
			((x->coeff3 & 1) << (AR_IEEE64_ROUND_BITS - 1));
		SHRIGHTIEEE64 (*x);
		x->coeff0 |= lbits << (AR_IEEE64_C0_BITS - 1);
		lbits >>= 1;
		if (x_expo)
			x_expo++;
		else
			x_expo = 2;
	}

	/* Check for a zero result, as a special case */
	if (!(lbits | x->coeff0|x->coeff1|x->coeff2|x->coeff3 | rbits)) {
		x->sign &= !diffsign;	/* -0 + -0 == -0 */
		x->expo = 0;
		if (x->sign)
			res |= AR_STAT_NEGATIVE;
		return res | AR_STAT_ZERO;
	}

	return ar_i64norm (x_expo, lbits, rbits, x, roundmode);
}


int
ar_ifsub64 (AR_IEEE_64 *x,
			const AR_IEEE_64 *a,
			const AR_IEEE_64 *b,
			int roundmode)
{

	AR_IEEE_64 nb;

	/* If either a or b is a NaN, it's the result. */
	if (IS_IEEE64_NaN(a)) {
		*x = *a;
		return AR_STAT_UNDEFINED;
	}
	if (IS_IEEE64_NaN(b)) {
		*x = *b;
		return AR_STAT_UNDEFINED;
	}

	nb = *b;
	nb.sign ^= 1;
	return ar_ifadd64 (x, a, &nb, roundmode);
}

#ifdef __mips
/* Use native addition for MIPS */
int
ar_ifadd128(AR_IEEE_128 *x,
			const AR_IEEE_128 *a,
			const AR_IEEE_128 *b,
			int roundmode)
{
   AR_TYPE ty = AR_Float_IEEE_NR_128;
   *(long double *)x = *(long double *)a + *(long double *)b;
   return AR_status((const AR_DATA *) x, &ty);
}

int
ar_ifsub128( AR_IEEE_128 *x,
			const  AR_IEEE_128 *a,
			const  AR_IEEE_128 *b,
			int roundmode)
{
   AR_TYPE ty = AR_Float_IEEE_NR_128;
   *(long double *)x = *(long double *)a - *(long double *)b;
   return AR_status((const AR_DATA *) x, &ty);
}

#else

int
ar_ifadd128(AR_IEEE_128 *x,
			const AR_IEEE_128 *a,
			const AR_IEEE_128 *b,
			int roundmode)
{

	int res = AR_STAT_OK;
	unsigned int lbits, y_lbits, rbits;
	unsigned int shift, diffsign, carry, x_expo;
	AR_IEEE_128 y;

	/*
	 * Use native arithmetic for MIPS.
	 */
	if (HOST_IS_MIPS) {
		AR_TYPE ty = AR_Float_IEEE_NR_128;

		*(long double *)x = *(long double *)a + *(long double *)b;
		return AR_status((AR_DATA *) x, &ty);
	}

	/* Ensure that the first argument has the largest exponent,
	 * for simplicity.
	 */
	if (b->expo > a->expo)
		y = *a, *x = *b;
	else
		y = *b, *x = *a;
	x_expo = x->expo;

	/* If either x or y is a NaN, it's the result. */
	if (IS_IEEE128_NaN(x)) {
		return res | AR_STAT_UNDEFINED;
	}
	if (IS_IEEE128_NaN(&y)) {
		*x = y;
		return res | AR_STAT_UNDEFINED;
	}

	/* Test for infinities */
	if (x_expo > AR_IEEE128_MAX_EXPO)
		if (y.expo > AR_IEEE128_MAX_EXPO)
			if (x->sign == y.sign) {
				/* infinities of same sign - return infinity */
				if (x->sign)
					res |= AR_STAT_NEGATIVE;
				return res | AR_STAT_OVERFLOW;
			} else {
				/* infinities of different signs - quiet NaN */
				QNaNIEEE128 (x);
				return res | AR_STAT_UNDEFINED;
			}
		else {
			/* infinity + finite quantity - return infinity */
			if (x->sign)
				res |= AR_STAT_NEGATIVE;
			return res | AR_STAT_OVERFLOW;
		}
	else if (y.expo > AR_IEEE128_MAX_EXPO) {
		*x = y;
		if (x->sign)
			res |= AR_STAT_NEGATIVE;
		return res | AR_STAT_OVERFLOW;
	}

	/* Test for denorms (they have zero exponents) to determine the
	 * values of the implicit normalization bits.
	 */
	if (ar_state_register.ar_denorms_trap &&
		((!x_expo && IS_IEEE128_NZ_COEFF(x)) ||
		 (!y.expo && IS_IEEE128_NZ_COEFF(&y)))) {
		/* operand is a denorm and denorms cause a trap */
		x->expo = AR_IEEE128_MAX_EXPO + 1;
		return res | AR_STAT_UNDEFINED;
	}
	lbits = !!x_expo;
	y_lbits = !!y.expo;

	/* Shift the coefficient of the second argument down (right). We
	 * do this in parts so as not to overflow registers.
	 */
	rbits = 0;
	shift = (x_expo - lbits) - (y.expo - y_lbits);
	if (shift > AR_IEEE128_COEFF_BITS + AR_IEEE128_ROUND_BITS) {
		rbits = !!(y.coeff0 | y.coeff1 | y.coeff2 |
				   y.coeff3 | y.coeff4 | y.coeff5 | y.coeff6);
		y.sign = y.expo = y_lbits = 0;
		y.coeff0 = y.coeff1 = y.coeff2 =
		y.coeff3 = y.coeff4 = y.coeff5 = y.coeff6 = 0;
	} else
		for (; shift; shift--) {
			/* Sticky bit shifting */
			rbits = (rbits & 1) | (rbits >> 1) |
				((y.coeff6 & 1) << (AR_IEEE128_ROUND_BITS-1));
			SHRIGHTIEEE128 (y);
			y.coeff0 |= y_lbits << (AR_IEEE128_C0_BITS - 1);
			y_lbits = 0;
		}

	/* If signs differ, complement the first argument; this is equivalent
	 * to negating and then subtracting one, in a two's complement sense.
	 */
	if (diffsign = (x->sign ^ y.sign)) {
		lbits ^= MASKR (2);
		NOTIEEE128 (*x);
	}

	/* Compute sum of coefficients */
	if (diffsign) {
		rbits += MASKR (AR_IEEE128_ROUND_BITS);
		carry = rbits >> AR_IEEE128_ROUND_BITS;
		rbits &= MASKR (AR_IEEE128_ROUND_BITS);
	} else
		carry = 0;
	ADDIEEE128 (*x, carry, *x, y);
	lbits = (carry + lbits + y_lbits) & MASKR (2);

	/* Opposite signs */
	if (diffsign)
		if (lbits & 2) {
			/* Complement */
			lbits ^= MASKR (2);
			NOTIEEE128 (*x);
			rbits ^= MASKR (AR_IEEE128_ROUND_BITS);
		} else {
			/* End-around carry */
			x->sign ^= 1;
			carry = 1 + rbits;
			rbits = carry & MASKR (AR_IEEE128_ROUND_BITS);
			carry >>= AR_IEEE128_ROUND_BITS;
			INCIEEE128 (*x, carry);
			lbits = (carry + lbits) & MASKR (2);
		}

	/* Shift rightward if carry, or if undenormalized */
	if (lbits & 2 || lbits && !x_expo) {
		rbits = (rbits >> 1) | rbits & 1 |
			((x->coeff6 & 1) << (AR_IEEE128_ROUND_BITS - 1));
		SHRIGHTIEEE128 (*x);
		x->coeff0 |= lbits << (AR_IEEE128_C0_BITS - 1);
		lbits >>= 1;
		if (x_expo)
			x_expo++;
		else
			x_expo = 2;
	}

	/* Check for a zero result, as a special case */
	if (!(lbits | rbits | x->coeff0 | x->coeff1 | x->coeff2 |
						  x->coeff3 | x->coeff4 | x->coeff5 | x->coeff6)) {
		x->sign &= !diffsign;	/* -0 + -0 == -0 */
		x->expo = 0;
		if (x->sign)
			res |= AR_STAT_NEGATIVE;
		return res | AR_STAT_ZERO;
	}

	return ar_i128norm (x_expo, lbits, rbits, x, roundmode);
}


int
ar_ifsub128(AR_IEEE_128 *x,
			const AR_IEEE_128 *a,
			const AR_IEEE_128 *b,
			int roundmode)
{

	AR_IEEE_128 nb;

	/* If either a or b is a NaN, it's the result. */
	if (IS_IEEE128_NaN(a)) {
		*x = *a;
		return AR_STAT_UNDEFINED;
	}
	if (IS_IEEE128_NaN(b)) {
		*x = *b;
		return AR_STAT_UNDEFINED;
	}

	nb = *b;
	nb.sign ^= 1;
	return ar_ifadd128 (x, a, &nb, roundmode);
}
#endif /* __mips */


static char USMID [] = "\n%Z%%M%	%I%	%G% %U%\n";
static char rcsid [] = "$Id: ieee_fadd.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $";
