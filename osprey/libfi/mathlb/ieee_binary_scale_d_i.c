/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License 
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU Lesser General Public 
  License along with this program; if not, write the Free Software 
  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
  USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


#pragma ident "@(#) libfi/mathlb/ieee_binary_scale_d_i.c	92.1	07/09/99 11:00:36"

#include <fortran.h>
#include <fmath.h>
#include "inline.h"
#include "leadz.h"

extern _f_real16 _IEEE_BINARY_SCALE_D_I8(_f_real16 orig_number,
   _f_int8 orig_scale_factor);

static _f_real8 _raisovfl(_f_real8 x);
static _f_real8 _raisunfl(_f_real8 y,_f_real8 x);

#ifndef IEEE_128_MAX_EXPONENT
#define IEEE_128_MAX_EXPONENT (LL_CONST(32767))
#endif

static _f_real8 _raisovfl(_f_real8 x)
{
	return (x * x);
}

static _f_real8 _raisunfl(_f_real8 y,_f_real8 x)
{
	return (y / x);
}

/* SCALE - return X * b**i
 ***/

#ifndef __mips
#pragma _CRI duplicate _IEEE_BINARY_SCALE_D_I8 as _IEEE_BINARY_SCALE_D
#endif
_f_real16
_IEEE_BINARY_SCALE_D_I8(_f_real16 orig_number, _f_int8 orig_scale_factor)
{
	union ldble_float {
		_f_real16		whole;
		unsigned long long	ui[2];
	} f, result;
	unsigned long long	sign_bit;
	unsigned long long	tmp_orig;
	long long		exponent;

	union ldble_float orig_mantissa, mantissa;
	int	lz, lzm, ileadzcnt, shift, loopn, round;
	int	scale_factor;
	int	expon;
	int	tmp =	0;
	static int word_size =	64;

	/* if x is a NaN, return a Nan */
	if (isnan128(orig_number))
		return orig_number;
	f.whole =	orig_number;

	/* extract sign bit. */
	sign_bit =	IEEE_128_64_SIGN_BIT & f.ui[0];

	/* extract exponent. */
	exponent =      f.ui[0] & IEEE_128_64_EXPO;

	/* extract mantissa. */
	orig_mantissa.whole = f.whole;
	orig_mantissa.ui[0] &= IEEE_128_64_MANT1;

	/* if x is + or -infinity, return same infinity */
	f.ui[0] &= ~IEEE_128_64_SIGN_BIT;
	if ((f.ui[0] == IEEE_128_64_EXPO) && (f.ui[1] == 0))
		return orig_number;

	/* if x is zero, return zero. */
	if (orig_number == 0)
		return orig_number;

	scale_factor =	orig_scale_factor;

	/* check for unnormalized number */
	if (exponent == LL_CONST(0x0)) {

		/* Unnormmal.  Get leading zeros in original mantissa */
		lz =	0;
		for (loopn = 0; loopn < 2; loopn++) {
			ileadzcnt = _leadz8(orig_mantissa.ui[loopn]);
			lz += ileadzcnt;
			if (ileadzcnt < word_size)
				break;
		}

		if (scale_factor > 0) {

			/* Scale number by a positive power of 2.  The
			 * result may need to be normalized.  Get
			 * leading zeros in mantissa = lzm.
			 */
			lzm = lz - IEEE_128_EXPO_BITS - 1;

			/* Any leading zeros in mantissa? */
			if (lzm > 0) {

				/* check if number of leading zeros in
				 * mantissa allows scaling by shifting.
				 */
				if (scale_factor <
					lzm || scale_factor == lzm) {

					/* Enough lead zeros to shift.
					 * Exponent is unaffected.
					 */
					shift = scale_factor;
					expon = 0;
				} else {

					/* Scale by shifting what we can
					 * and adjust exponent for rest.
					 * The result is a normalized
					 * number.
					 */
					shift = lzm + 1;
					expon = scale_factor - lzm;
				}
			/* No leading zeros in mantissa. */
			} else {

				/* Shift by 1 to normalize mantissa. Do
				 * rest of scaling through exponent.
				 */
				shift =	1;
				expon = scale_factor;
			}
			/* position the exponent. */
			exponent = expon <<
				(IEEE_128_MANT_BITS - word_size);

			/* scale the mantissa. */
			tmp = word_size - shift;
			if (tmp <= 0) {

				/* load 64-bit zero as last 64 bits
				 * of the mantissa when shift .GE 64.
				 */
				mantissa.ui[1] = LL_CONST(0x0);

				/* load bits from word 2 of original
				 * mantissa as word one of mantissa.
				 */
				mantissa.ui[0] = orig_mantissa.ui[1] <<
					(-tmp);
			} else {
				/* shift is .LT. 64 */
				mantissa.ui[1] = orig_mantissa.ui[1] <<
					shift;

				/* Shift word one of original mantissa
				 * and OR shifted bits from word 2 of
				 * original mantissa into word one.
				 */
				mantissa.ui[0] = (orig_mantissa.ui[0] <<
					shift) || (orig_mantissa.ui[1] >>
					tmp);
			}

		/* Scale_factor LE 0. */
		} else {

			/* scale mantissa. */
			tmp = word_size + scale_factor;
			if (-scale_factor >= word_size) {

				/* -scale factor .GE. 64, so first word
				 * of mantissa is zero and second word
				 * is shifted part of word two of
				 * original mantissa.
				 */
				mantissa.ui[0] =  LL_CONST(0x0);
				mantissa.ui[1] = (orig_mantissa.ui[0] >>
					(-tmp));
				if (tmp == 0) {
					tmp_orig = orig_mantissa.ui[1];
					round = word_size - 1;
				} else {
					tmp_orig = orig_mantissa.ui[0];
					round = (-tmp) - 1;
				}
			} else if (-scale_factor == 0) {
				mantissa.ui[0] = orig_mantissa.ui[0];
				mantissa.ui[1] = orig_mantissa.ui[1];
			} else {
				/* scale word one of mantissa. */
				mantissa.ui[0] = orig_mantissa.ui[0] >>
					(-scale_factor);

				/* scale word two of mantissa and OR in
				 * contents from word one.
				 */
				mantissa.ui[1] = (orig_mantissa.ui[1] >>
					(-scale_factor)) ||
					(orig_mantissa.ui[0] << tmp);
				tmp_orig = orig_mantissa.ui[1];
				round = tmp - 1;
			}

			/* This is bit of magic that does some rounding.
			 * Get last bit to be shifted off to right.
			 * If it's 1, round mantissa up by adding 1.
			 */
			if (((-scale_factor) != 0) &&
				  (tmp_orig & (0x1 << round) != 0)) {
				if (mantissa.ui[1] &
					 (0x1 << (word_size - 1)) != 0) {
						round = 1;
				} else {
					round = 0;
				}
				mantissa.ui[1] = mantissa.ui[1] + 1;
				if ((mantissa.ui[1] &
					 (0x1 << (word_size - 1)) != 0) &&
					  (round == 1)) {
						mantissa.ui[0] =
						  mantissa.ui[0] + 1;
				}
			}
		}

		/* mask out any bits that may have shifted to the left of
		 * the mantissa area.
		 */
		mantissa.ui[0] &= IEEE_128_64_MANT1;

		/* OR the new mantissa, the new exponent, and the original
		 * sign bit together to create the result.
		 */
		result.whole = mantissa.whole;
		result.ui[0] = mantissa.ui[0] | exponent | sign_bit;

	} else {

		_f_real8	result8 = 0;
		_f_real8	ovfl =	HUGE_REAL8_F90;
		_f_real8	unfl =	TINY_REAL8_F90;

		/* Number is Normal.  Make an exponent an integer. */
		exponent >>=	(IEEE_128_MANT_BITS - word_size);

		/* Add in the scale factor. */
		exponent += scale_factor;

		if (exponent >= IEEE_128_MAX_EXPONENT) {

			/* raise overflow exception. */
			result8 = _raisovfl(ovfl);

			/* overflowed exponent. Return signed infinity. */
			result.whole = 0.0;
			result.ui[0] = IEEE_128_64_EXPO | sign_bit;

		} else if (exponent <= LL_CONST(0x0)) {

			/* raise underflow exception. */
			result8 = _raisunfl(unfl,ovfl);

			/* Scaled exponent is negative or zero.
			 * result.ui = (orig_mantissa.ui | (0x1 <<
			 *	IEEE_64_MANT_BITS)) >> (-exponent + 1);
			 * becomes zero on sparc.  Make 2 statements.
			 * Return denormal number. */
			mantissa.ui[0] = LL_CONST(0x1);
			mantissa.ui[1] = orig_mantissa.ui[1];
			mantissa.ui[0] = (((mantissa.ui[0] <<
				(IEEE_128_MANT_BITS - word_size)) |
				orig_mantissa.ui[0]));
			if (-exponent > LL_CONST(0x40)) {
				result.ui[0] = LL_CONST(0x0);
				result.ui[1] = mantissa.ui[0] >>
					((-exponent + 1) - word_size);
			} else {
				result.ui[0] = mantissa.ui[0] >>
					(-exponent + 1);
				result.ui[1] = (mantissa.ui[1] >>
					(-exponent + 1)) ||
					(mantissa.ui[0] <<
					(word_size - (-exponent + 1)));
			}

		} else {

			/* Scaled exponent is positive and within range.
			 * Position into exponent.
			 */
			exponent <<= (IEEE_128_MANT_BITS - word_size);

			/* OR new mantissa, new exponent, and original 
			 * sign bit together to create result.
			 */
			result.whole = orig_mantissa.whole;
			result.ui[0] = exponent |
				orig_mantissa.ui[0] | sign_bit;
		}
	}
	return result.whole;
}
