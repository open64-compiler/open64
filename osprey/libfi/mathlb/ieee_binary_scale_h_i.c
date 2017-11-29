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


#pragma ident "@(#) libfi/mathlb/ieee_binary_scale_h_i.c	92.1	07/09/99 11:00:36"

#include <fortran.h>
#include <fmath.h>
#include "inline.h"
#include "leadz.h"

extern _f_real4 _IEEE_BINARY_SCALE_H_I8(_f_real4 orig_number,
   _f_int8 orig_scale_factor);

static _f_real4 _raisovfl(_f_real4 x);
static _f_real4 _raisunfl(_f_real4 y,_f_real4 x);

#ifndef IEEE_32_MAX_EXPONENT
#define IEEE_32_MAX_EXPONENT 255
#endif

static _f_real4 _raisovfl(_f_real4 x)
{
	return (x * x);
}

static _f_real4 _raisunfl(_f_real4 y,_f_real4 x)
{
	return (y / x);
}

/* SCALE - return X * b**i
 ***/

_f_real4
_IEEE_BINARY_SCALE_H_I8(_f_real4 orig_number, _f_int8 orig_scale_factor)
{
	int	lz, lzm, shift;
	REGISTER_4	number, scale_factor, expon, exponent,
			orig_mantissa, mantissa, sign_bit;

	/* if x is a NaN, return a Nan */
	if (isnan32(orig_number))
		return orig_number;
	number.f =	orig_number;

	/* extract sign bit. */
	sign_bit.ui =	IEEE_32_SIGN_BIT & number.ui;

	/* extract exponent. */
	exponent.ui =	IEEE_32_EXPONENT & number.ui;

	/* extract mantissa. */
	orig_mantissa.ui = IEEE_32_MANTISSA & number.ui;

	/* if x is + or -infinity, return same infinity */
	number.ui &= ~IEEE_32_SIGN_BIT;
	if (number.ui == IEEE_32_INFINITY)
		return orig_number;

	/* if x is zero, return zero. */
	if (orig_number == 0)
		return orig_number;

	scale_factor.i =	orig_scale_factor;

	/* check for unnormalized number */
	if (exponent.ui == 0) {

		/* Denormmal.  Get leading zeros in original mantissa */
		lz =	_leadz4(orig_mantissa.ui);

		if (scale_factor.i > 0) {

			/* Scale number by a positive power of 2.  The
			 * result may need to be normalized.  Get
			 * leading zeros in mantissa = lzm.
			 */
			lzm = lz - IEEE_32_EXPO_BITS - 1;

			/* Any leading zeros in mantissa? */
			if (lzm > 0) {

				/* check if number of leading zeros in
				 * mantissa allows scaling by shifting.
				 */
				if (scale_factor.i <= lzm) {

					/* Enough lead zeros to shift.
					 * Exponent is unaffected.
					 */
					shift = scale_factor.i;
					expon.i = 0;
				} else {

					/* Scale by shifting what we can
					 * and adjust exponent for rest.
					 * The result is a normalized
					 * number.
					 */
					shift = lzm + 1;
					expon.i = scale_factor.i - lzm;
				}
			/* No leading zeros in mantissa. */
			} else {

				/* Shift by 1 to normalize mantissa. Do
				 * rest of scaling through exponent.
				 */
				shift =	1;
				expon.i = scale_factor.i;
			}
			/* position the exponent. */
			exponent.ui = expon.ui << IEEE_32_MANT_BITS;

			/* scale the mantissa. */
			mantissa.ui = orig_mantissa.ui << shift;

		/* Scale_factor LE 0. */
		} else {

			/* scale mantissa. */
			mantissa.ui = orig_mantissa.ui >> (-scale_factor.i);
			if ((-scale_factor.ui != 0) &&
			    (orig_mantissa.ui & (0x1 <<
			    (-scale_factor.i - 1)))) {
					mantissa.ui++;
			}

		}

		/* mask out any bits that may have shifted to the left of
		 * the mantissa area.
		 */
		mantissa.ui &= IEEE_32_MANTISSA;

		/* OR the new mantissa, the new exponent, and the original
		 * sign bit together to create the result.
		 */
		number.ui = mantissa.ui | exponent.ui | sign_bit.ui;

	} else {

		_f_real4	result4 = 0;
		_f_real4	ovfl =	HUGE_REAL4_F90;
		_f_real4	unfl =	TINY_REAL4_F90;

		/* Number is Normal.  Make an exponent an integer. */
		exponent.ui >>= IEEE_32_MANT_BITS;

		/* Add in scale factor. */
		exponent.i += scale_factor.i;

		if (exponent.i >= IEEE_32_MAX_EXPONENT) {

			/* raise overflow exception. */
			result4 = _raisovfl(ovfl);

			/* overflowed exponent. Return signed infinity. */
			number.ui = IEEE_32_INFINITY | sign_bit.ui;

		} else if (exponent.i <= 0) {

			/* raise underflow exception. */
			result4 = _raisunfl(unfl,ovfl);

			/* Scaled exponent is negative or zero.
			 * result.ui = (orig_mantissa.ui | (0x1 <<
			 *	IEEE_64_MANT_BITS)) >> (-exponent + 1);
			 * becomes zero on sparc.  Make 2 statements.
			 * Return denormal number. */
			number.ui = (orig_mantissa.ui | (0x1 <<
			   IEEE_32_MANT_BITS)) >> (-exponent.i + 1);

		} else {

			/* Scaled exponent is positive and within range.
			 * Position into exponent.
			 */
			exponent.ui <<= IEEE_32_MANT_BITS;

			/* OR new mantissa, new exponent, and original 
			 * sign bit together to create result.
			 */
			number.ui = exponent.ui |
				orig_mantissa.ui | sign_bit.ui;
		}
	}
	return number.f;
}
