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
 *	Portable Cray floating-point multiplication evaluator
 *
 *	Sources: hardware manuals, online diagnostics, Dick Nelson's
 *		 emulation code, and the source for the double-precision
 *		 library routines
 */

#include "arith.internal.h"


int
ar_cfmul64 (AR_CRAY_64 *x,
	    const AR_CRAY_64 *a,
	    const AR_CRAY_64 *b,
            int roundmode) {

	int i, res = AR_STAT_OK;
	long x_expo, a_expo = a->expo, b_expo = b->expo, test_expo;
	unsigned int x_lbits, y_rbits, zcoeff, carry, x_rbits = 0;
	AR_CRAY_64 y, z;

	x->sign = a->sign ^ b->sign;
	y = *a;
	z = *b;

	if (!a_expo && !b_expo)
		x->sign = x_expo = 0;
	else {
		x_expo = a_expo + b_expo - AR_CRAY_EXPO_BIAS;
		if (a_expo < AR_CRAY_MIN_EXPO - 1 ||
		    b_expo < AR_CRAY_MIN_EXPO - 1 ||
		    x_expo < AR_CRAY_MIN_EXPO - 1) {
			ZEROCRAY64 (*x);
			return AR_STAT_UNDERFLOW | AR_STAT_ZERO;
		}
	}

	switch (roundmode) {
	case AR_ROUNDED:		/* CRAY-1 rounded multiply */
		x_lbits = 0;
		x->coeff0 = x->coeff1 = x->coeff2 = 0;
		x_rbits = 0151;
		break;
	case AR_UNROUNDED:		/* CRAY-1 truncation compensation */
		x_lbits = 0;
		x->coeff0 = x->coeff1 = x->coeff2 = 0;
		x_rbits = 0011;
		break;
	case AR_RECIPROCAL_ITERATION:		/* CRAY-1 recip iter */
		x_lbits = 1;
		x->coeff0 = ~0;
		x->coeff1 = ~0;
		x->coeff2 = (0011 - 0320) >> 7;
		x_rbits = (0011 - 0320) & MASKR (7);
		break;
	}

	/* Compute and sum the pyramid */
#if AR_CRAY_C0_BITS*3 == AR_CRAY64_COEFF_BITS
	y_rbits = y.coeff2<<7;
	i = AR_CRAY_C0_BITS;
	zcoeff = z.coeff0;
	while(zcoeff) {
		if(zcoeff & 0x8000) {
			x_rbits += (y_rbits & 0177);
			carry = x_rbits >> 7;
			x_rbits &= 0177;
			ADDCRAY64 (*x, carry, *x, y);
			x_lbits += carry;
		}
		SHRIGHTCRAY64 (y);
		y_rbits >>= 1;
		zcoeff = (zcoeff & 0x7fff) << 1;
		i--;
	}
	y.coeff2 = (y.coeff2>>i) | (y.coeff1<<(AR_CRAY_C0_BITS-i));
	y.coeff1 = (y.coeff1>>i) | (y.coeff0<<(AR_CRAY_C0_BITS-i));
	y.coeff0 = 0;
	y_rbits = (y_rbits>>i) | (y.coeff2<<7);
	i = AR_CRAY_C1_BITS;
	zcoeff = z.coeff1;
	while(zcoeff) {
		if(zcoeff & 0x8000) {
			x_rbits += (y_rbits & 0177);
			carry = x_rbits >> 7;
			x_rbits &= 0177;
			ADDCRAY64 (*x, carry, *x, y);
			x_lbits += carry;
		}
		SHRIGHTCRAY64 (y);
		y_rbits >>= 1;
		zcoeff = (zcoeff & 0x7fff) << 1;
		i--;
	}
	y.coeff2 = (y.coeff2>>i) | (y.coeff1<<(AR_CRAY_C1_BITS-i));
	y.coeff1 = 0;
	y_rbits = (y_rbits>>i) | (y.coeff2<<7);
	zcoeff = z.coeff2;
	while(zcoeff) {
		if(zcoeff & 0x8000) {
			x_rbits += (y_rbits & 0177);
			carry = x_rbits >> 7;
			x_rbits &= 0177;
			ADDCRAY64 (*x, carry, *x, y);
			x_lbits += carry;
		}
		y.coeff2 >>= 1;
		y_rbits >>= 1;
		zcoeff = (zcoeff & 0x7fff) << 1;
	}
#else
	y_rbits = 0;
	for (i = 0; i < AR_CRAY64_COEFF_BITS; i++) {
		/* Add scaled operand to sum, with truncation */
		if (z.coeff0 >> (AR_CRAY_C0_BITS - 1)) {
			x_rbits += y_rbits;
			carry = x_rbits >> 7;
			x_rbits &= MASKR (7);
			ADDCRAY64 (*x, carry, *x, y);
			x_lbits += carry;
		}
		y_rbits = (y_rbits >> 1) | ((y.coeff2 & 1) << 6);
		SHRIGHTCRAY64 (y);
		SHLEFTCRAY64 (z);
	}
#endif

	if (roundmode == AR_RECIPROCAL_ITERATION) {
		x_lbits ^= 1;
		NOTCRAY64 (*x);
	}

	/* Normalize right if necessary */
	test_expo = x_expo;
	if (x_lbits & 1 || !(a_expo | b_expo)) {
		SHRIGHTCRAY64 (*x);
		x->coeff0 |= x_lbits << (AR_CRAY_C0_BITS - 1);
		if (a_expo | b_expo)
			x_expo++;
	}

	/* Check for overflow with unadjusted result exponent */
	if (test_expo >= AR_CRAY_MAX_EXPO ||
	    a_expo > AR_CRAY_MAX_EXPO ||
	    b_expo > AR_CRAY_MAX_EXPO) {
		x_expo = AR_CRAY_MAX_EXPO + 1;
		res |= AR_STAT_OVERFLOW;
	}

	x->expo = x_expo;

	if (!(x->sign | x->expo | x->coeff0 | x->coeff1 | x->coeff2))
		res |= AR_STAT_ZERO;
	if (x->sign)
		res |= AR_STAT_NEGATIVE;

	return res;
}


int
ar_cfmul128 (AR_CRAY_128 *x,
	     const AR_CRAY_128 *a,
	     const AR_CRAY_128 *b,
	     int roundmode) {

	int i, res = AR_STAT_OK, rnd, inexact, a_expo = a->expo, b_expo = b->expo;
	long x_expo, test_expo;
	unsigned int x_lbits = 0, carry;
	AR_CRAY_128 x2, y, y2, z;
	AR_CRAY_64 spa, spb;

	/* Use the sign and exponent together in a 16-bit package */
	test_expo = x_expo = a_expo + b_expo - AR_CRAY_EXPO_BIAS;
	x_expo += (a->sign << AR_CRAY_EXPO_BITS) +
		  (b->sign << AR_CRAY_EXPO_BITS);

	/* Special-case zero arguments or zero SP products */
	if (!(a->coeff0 | a->coeff1 | a->coeff2 |
	      a->coeff3 | a->coeff4 | a->coeff5) ||
	    !(b->coeff0 | b->coeff1 | b->coeff2 |
	      b->coeff3 | b->coeff4 | b->coeff5)) {
		ZEROCRAY128 (*x);
		return AR_STAT_ZERO;
	}

	y = *a;
	z = *b;
	ZEROCRAY128 (x2);
	ZEROCRAY128 (y2);
	ZEROCRAY128 (*x);

	/* Compute and sum the pyramid */
	for (i = 0; i < AR_CRAY128_COEFF_BITS; i++) {

		/* Add scaled operand to sum, with truncation */
		if (z.coeff0 & (1 << (AR_CRAY_C0_BITS - 1))) {
			carry = 0;
			ADDCRAY128 (x2, carry, x2, y2);
			ADDCRAY128 (*x, carry, *x, y);
			x_lbits += carry;
		}

		SHRIGHTCRAY128 (y2);
		y2.coeff0 |= y.coeff5 << (AR_CRAY_C0_BITS - 1);
		SHRIGHTCRAY128 (y);
		SHLEFTCRAY128 (z);
	}

	/* Rounding */
	inexact = !!(x2.coeff0 | x2.coeff1 | x2.coeff2 | x2.coeff3 |
			x2.coeff4 | x2.coeff5);
	carry = rnd = x2.coeff0 >> (AR_CRAY_C0_BITS - 1);
	INCCRAY128 (*x, carry);
	x_lbits += carry;

	/* Normalize right if necessary */
	if (x_lbits) {
		rnd ^= 1;
		INCCRAY128 (*x, rnd);
		SHRIGHTCRAY128 (*x);
		x->coeff0 |= 1 << (AR_CRAY_C0_BITS - 1);
		x_expo++;
	}

	/* Check for overflow with unadjusted result exponent */
	if (test_expo >= AR_CRAY_MAX_EXPO ||
	    a_expo > AR_CRAY_MAX_EXPO && !!b_expo ||
	    b_expo > AR_CRAY_MAX_EXPO && !!a_expo)
		res |= AR_STAT_OVERFLOW;

	/* Check for underflow */
	if (!(((x_expo + AR_CRAY_MIN_EXPO) >> (AR_CRAY_EXPO_BITS - 1)) & 1)) {
		ZEROCRAY128 (*x);
		x_expo = 0;
		res |= AR_STAT_UNDERFLOW;
	}

	x->expo = x_expo;
	x->sign = x_expo >> AR_CRAY_EXPO_BITS;

	if (!(x->sign | x->expo | x->coeff0 | x->coeff1 | x->coeff2 |
              x->coeff3 | x->coeff4 | x->coeff5))
		res |= AR_STAT_ZERO;
	if (x->sign)
		res |= AR_STAT_NEGATIVE;
	if (inexact)
		res |= AR_STAT_INEXACT;
	return res;
}


static char USMID [] = "\n%Z%%M%	%I%	%G% %U%\n";
static char rcsid [] = "$Id: cray_fmul.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $";
