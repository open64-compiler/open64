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
 *	Portable Cray floating-point addition/subtraction emulation
 */

#include "arith.internal.h"


int
ar_cfadd64 (AR_CRAY_64 *x,
	    const AR_CRAY_64 *a,
	    const AR_CRAY_64 *b) {

	int res = AR_STAT_OK, a_expo = a->expo, b_expo = b->expo;
	unsigned int carry, shift, inexact;
	AR_CRAY_64 y;

	/* Ensure that the first argument has the largest exponent */
	if (b_expo > a_expo)
		y = *a, *x = *b;
	else
		y = *b, *x = *a;

	/* Test for underflow */
	if (x->expo < AR_CRAY_MIN_EXPO) {
		ZEROCRAY64 (*x);
		return AR_STAT_UNDERFLOW | AR_STAT_ZERO;
	}

	/* Shift the coefficient of the second argument down (right). We
	 * do this in parts so as not to overflow registers.
	 */
	inexact = 0;
	shift = x->expo - y.expo;
	if (shift > AR_CRAY64_COEFF_BITS)
		ZEROCRAY64 (y);
	else {
		for (; shift; shift--) {
			inexact |= y.coeff2 & 1;
			SHRIGHTCRAY64 (y);
			y.expo++;
		}
	}

	/* If signs differ, complement the first argument; this is equivalent
	 * to negating and then subtracting one, in a two's complement sense.
	 */
	if (x->sign != y.sign)
		NOTCRAY64 (*x);

	/* Compute sum of coefficients */
	carry = 0;
	ADDCRAY64 (*x, carry, *x, y);

	/* Check if the sign changed, and add 1 if so; otherwise, undo
	 * the complement.
	 */
	if (x->sign != y.sign)
		if (carry) {
			x->sign ^= 1;
			carry = 1;
			INCCRAY64 (*x, carry);
			carry = 0;
		} else
			NOTCRAY64 (*x);

	if (carry) {
		SHRIGHTCRAY64 (*x);
		x->coeff0 |= 1 << (AR_CRAY_C0_BITS - 1);
		x->expo++;
	}

	/* Check for a zero result, as a special case */
	if (!(x->coeff0 | x->coeff1 | x->coeff2)) {
		x->sign = x->expo = 0;
		return AR_STAT_ZERO;
	}

	/* Shift the result coefficient left until normalized */
	while (!(x->coeff0 >> (AR_CRAY_C0_BITS - 1))) {
		x->expo--;
		SHLEFTCRAY64 (*x);
	}

	/* Test for out-of-range result or operand */
	if (x->expo > AR_CRAY_MAX_EXPO ||
	    a_expo > AR_CRAY_MAX_EXPO ||
	    b_expo > AR_CRAY_MAX_EXPO) {
		x->sign = 0;
		x->expo = AR_CRAY_MAX_EXPO + 1;
		res |= AR_STAT_OVERFLOW;
	}

	if (inexact)
		res |= AR_STAT_INEXACT;
	if (x->sign)
		res |= AR_STAT_NEGATIVE;

	return res;
}


int
ar_cfsub64 (AR_CRAY_64 *x,
	    const AR_CRAY_64 *a,
	    const AR_CRAY_64 *b) {

	AR_CRAY_64 nb = *b;

	nb.sign ^= 1;
	return ar_cfadd64 (x, a, &nb);
}


int
ar_cfadd128 (AR_CRAY_128 *x,
	     const AR_CRAY_128 *a,
	     const AR_CRAY_128 *b) {

	int res = AR_STAT_OK, x_expo;
	long carry, shift, inexact;
	AR_CRAY_128 y;

	x->zero = 0;

	/* Ensure that the first argument has the largest absolute value
	 * (assuming normalized inputs)
	 */
	if (b->expo > a->expo ||
	    b->expo == a->expo &&
	    (b->coeff0 > a->coeff0 ||
	     b->coeff0 == a->coeff0 &&
	     (b->coeff1 > a->coeff1 ||
	      b->coeff1 == a->coeff1 &&
	      (b->coeff2 > a->coeff2 ||
	       b->coeff2 == a->coeff2 &&
	       (b->coeff3 > a->coeff3 ||
	        b->coeff3 == a->coeff3 &&
	        (b->coeff4 > a->coeff4 ||
	         b->coeff4 == a->coeff4 &&
	         b->coeff5 > a->coeff5))))))
		y = *a, *x = *b;
	else
		y = *b, *x = *a;

	if ((x_expo = x->expo) > AR_CRAY_MAX_EXPO) {
		ZEROCRAY128 (*x);
		x->expo = AR_CRAY_MAX_EXPO + 1;
		x->coeff0 = 1 << (AR_CRAY_C0_BITS - 1);
		return AR_STAT_OVERFLOW;
	}

	/* Shift the coefficient of the second argument down (right) */
	inexact = 0;
	shift = x_expo - y.expo;
	if (shift > AR_CRAY128_COEFF_BITS)
		ZEROCRAY128 (y);
	else
		for (; shift; shift--) {
			inexact |= y.coeff5 & 1;
			SHRIGHTCRAY128 (y);
			y.expo++;
		}

	/* If signs differ, negate the second argument in two's complement. */
	if (x->sign != y.sign) {
		NOTCRAY128 (y);
		carry = 1;
	} else
		carry = 0;

	/* Compute sum of coefficients */
	ADDCRAY128 (*x, carry, *x, y);

	/* Handle a carry out of the upper bit */
	if (x->sign != y.sign)
		carry--;

	if (carry < 0) {

		/* Bug reproduction from a negative shift count resulting
		 * from an unnormalized argument in a subtraction.
		 */
		x_expo += 16;
		x->coeff0 = x->coeff1 = x->coeff2 =
			x->coeff3 = x->coeff4 = x->coeff5 = 0;

	} else if (carry > 0) {

		/* Handle carry out of coefficient */
		if (carry > 0) {
			SHRIGHTCRAY128 (*x);
			x->coeff0 |= 1 << (AR_CRAY_C0_BITS - 1);
			x_expo++;
		}

	} else if (x->coeff0 | x->coeff1 | x->coeff2 |
		   x->coeff3 | x->coeff4 | x->coeff5) {

		/* Shift the nonzero result coefficient left until normalized */
		while (!(x->coeff0 >> (AR_CRAY_C0_BITS - 1))) {
			x_expo--;
			SHLEFTCRAY128 (*x);
		}

	} else if (x_expo >= AR_CRAY_MIN_EXPO && x_expo <= AR_CRAY_MAX_EXPO) {

		/* Zero result */
		x->sign = 0;
		x_expo = 0;
		res |= AR_STAT_ZERO;
	}

	/* Test for out-of-range result or operand, as well as an
	 * underflowed result.
	 */
	if (x_expo > AR_CRAY_MAX_EXPO) {
		ZEROCRAY128 (*x);
		x->expo = AR_CRAY_MAX_EXPO + 1;
		x->coeff0 = 1 << (AR_CRAY_C0_BITS - 1);
		res |= AR_STAT_OVERFLOW;
	} else if (x_expo < AR_CRAY_MIN_EXPO) {
		ZEROCRAY128 (*x);
		res |= AR_STAT_UNDERFLOW | AR_STAT_ZERO;
	} else
		x->expo = x_expo;

	if (inexact)
		res |= AR_STAT_INEXACT;
	if (x->sign)
		res |= AR_STAT_NEGATIVE;

	return res;
}


int
ar_cfsub128 (AR_CRAY_128 *x,
	     const AR_CRAY_128 *a,
	     const AR_CRAY_128 *b) {

	AR_CRAY_128 nb = *b;

	nb.sign ^= 1;
	return ar_cfadd128 (x, a, &nb);
}


static char USMID [] = "\n%Z%%M%	%I%	%G% %U%\n";
static char rcsid [] = "$Id: cray_fadd.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $";
