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


#pragma ident "@(#) libfi/mathlb/scale.c	92.1	07/09/99 11:00:36"


#include <fortran.h>
#include "inline.h"
#include "leadz.h"

#define IEEE_32_MAX_EXPONENT 255
#define IEEE_64_MAX_EXPONENT (LL_CONST(2047))
#define IEEE_128_MAX_EXPONENT (LL_CONST(32767))

/* SCALE - return X * b**i
 ***/

#ifdef _F_REAL4
extern _f_real4 _SCALE_4_I8(_f_real4 orig_number, _f_int8 orig_scale_factor);
extern _f_real4 _SCALE_4(_f_real4 orig_number, _f_int orig_scale_factor);
extern _f_real4 _SCALE_4_I4(_f_real4 orig_number, _f_int4 orig_scale_factor);

_f_real4
_SCALE_4_I4(_f_real4 orig_number, _f_int4 orig_scale_factor)
{
	_f_int	def_orig_scale;
	def_orig_scale =	orig_scale_factor;
	return(_SCALE_4(orig_number, def_orig_scale));
}

_f_real4
_SCALE_4_I8(_f_real4 orig_number, _f_int8 orig_scale_factor)
{
	int	def_orig_scale;
	def_orig_scale =	orig_scale_factor;
	return(_SCALE_4(orig_number, def_orig_scale));
}

#ifdef DO_INLINE
inline static		/* for the inline version of this function. */
#endif
_f_real4
_SCALE_4(_f_real4 orig_number, _f_int orig_scale_factor)
{
	int		lz, lzm, shift;
	REGISTER_4	number, scale_factor, expon, exponent,
			orig_mantissa, mantissa, sign_bit;

	/* eliminate the easy cases first. */
	if (orig_scale_factor == 0)
		return orig_number;
	if (orig_number == 0)
		return 0.0;
	number.f = orig_number;
	scale_factor.i = orig_scale_factor;

	/* extract exponent. */
	exponent.ui = IEEE_32_EXPONENT & number.ui;

	/* extract mantissa. */
	orig_mantissa.ui = IEEE_32_MANTISSA & number.ui;

	/* extract sign bit. */
	sign_bit.ui = IEEE_32_SIGN_BIT & number.ui;

	if (exponent.ui == 0) {

		/* number is Denormal. */
		lz = _leadz4(orig_mantissa.ui); /* get leading zeros in
						    * original mantissa. */
		if (scale_factor.i > 0) {

			/* Scale number by a positive power of 2.  We may
			 * create a number that will need to be normalized.
			 * Get leading zeros in mantissa = lzm.
			 */
			lzm = lz - IEEE_32_EXPO_BITS - 1;
			/* if number of leading zeros in mantissa GT zero. */
			if (lzm > 0) {

				/* leading zeros in mantissa. */
				if (scale_factor.i <= lzm) {

					/* Enough leading zeros in mantissa
					 * to allow scaling by shifting.
					 * Exponent is unaffected.
					 */
					shift = scale_factor.i;
					expon.i = 0;
				} else {

					/* Scale by shifting what we can and
					 * by adjusting exponent for the rest.
					 * This creates a normalized number.
					 */
					shift = lzm + 1;
					expon.i = scale_factor.i - lzm;
				}
			/* no leading zeros in mantissa. */
			} else {

				/* Shift by 1 to normalize mantissa.  The
				 * rest of the scaling will be done
				 * through the exponent.
				 */
				shift = 1;
				expon.i = scale_factor.i;
			}
			/* position the exponent. */
			exponent.ui = expon.ui << IEEE_32_MANT_BITS;
			/* scale the mantissa. */
			mantissa.ui = orig_mantissa.ui << shift;

		/* Scale_factor.i LE 0. */
		} else {

			/* scale mantissa. */
			mantissa.ui = orig_mantissa.ui >> (-scale_factor.i);

			/* This is a bit of magic that does rounding. Get the
			 * last bit that will be shifted off to the right, if
			 * it's a 1 round the mantissa up by adding 1.
			 */
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

		/* number is Normal.  Make exponent an integer. */
		exponent.ui >>= IEEE_32_MANT_BITS;
		exponent.i += scale_factor.i;  /* add in the scale factor. */
		if (exponent.i >= IEEE_32_MAX_EXPONENT) {

			/* overflowed exponent. Use infinity. */
			number.ui = IEEE_32_INFINITY | sign_bit.ui;
		} else if (exponent.i <= 0) {

			/* Scaled exponent is negative or zero. */
			number.ui = (orig_mantissa.ui | (0x1 <<
				IEEE_32_MANT_BITS)) >> (-exponent.i + 1);
		} else {

			/* Scaled exponent is positive and within range.
			 * Position into exponent.
			 */
			exponent.ui <<= IEEE_32_MANT_BITS;

			/* or the new mantissa, the new exponent, and the
			 * original sign bit together to create the result.
			 */
			number.ui =
				exponent.ui | orig_mantissa.ui | sign_bit.ui;
		}
	}
	return number.f;
}
#endif

extern _f_real8 _SCALE_8_I8(_f_real8 orig_number, _f_int8 orig_scale_factor);
extern _f_real8 _SCALE_8(_f_real8 orig_number, _f_int orig_scale_factor);
extern _f_real8 _SCALE_8_I4(_f_real8 orig_number, _f_int4 orig_scale_factor);
extern _f_real8 _SCALE(_f_real8 orig_number, _f_int orig_scale_factor);

_f_real8
_SCALE_8_I4(_f_real8 orig_number, _f_int4 orig_scale_factor)
{
	_f_int	def_orig_scale;
	def_orig_scale =	orig_scale_factor;
	return(_SCALE(orig_number, def_orig_scale));
}

_f_real8
_SCALE_8_I8(_f_real8 orig_number, _f_int8 orig_scale_factor)
{
	int	def_orig_scale;
	def_orig_scale =	orig_scale_factor;
	return(_SCALE(orig_number, def_orig_scale));
}

_f_real8
_SCALE_8(_f_real8 orig_number, _f_int orig_scale_factor)
{
	_f_real8 _SCALE(_f_real8 orig_number, _f_int orig_scale_factor);
	return (_SCALE(orig_number, orig_scale_factor));
}

#ifdef DO_INLINE
inline static		/* for the inline version of this function. */
#endif
_f_real8
_SCALE(_f_real8 orig_number, _f_int orig_scale_factor)
{
	int	lz, lzm, shift;
	REGISTER_8	number, scale_factor, expon, exponent,
			orig_mantissa, mantissa, sign_bit;

	/* eliminate the easy cases first. */
	if (orig_scale_factor == 0)
		return orig_number;
	if (orig_number == 0)
		return 0.0;
	number.f = orig_number;
	scale_factor.i = orig_scale_factor;

	/* extract exponent. */
	exponent.ui = IEEE_64_EXPONENT & number.ui;

	/* extract mantissa. */
	orig_mantissa.ui = IEEE_64_MANTISSA & number.ui;

	/* extract sign bit. */
	sign_bit.ui = IEEE_64_SIGN_BIT & number.ui;

	if (exponent.ui == LL_CONST(0)) {

		/* number is Subnormal.
		 * Get leading zeros in original mantissa. 
		 */
		lz = _leadz8(orig_mantissa.ui);
		if (scale_factor.i > 0) {

			/* Scale number by a positive power of 2.  We may
			 * create a number that will need to be normalized.
			 * Get leading zeros in mantissa = lzm.
			 */
			lzm = lz - IEEE_64_EXPO_BITS - 1;
			/* if number of leading zero in mantissa GT zero. */
			if (lzm > 0) {

				/* leading zeros in mantissa. */
				if (scale_factor.i <
					lzm || scale_factor.i == lzm) {
					/* Enough leading zeros in mantissa
					 * to allow scaling by shifting.
					 * Exponent is unaffected.
					 */
					shift = scale_factor.i;
					expon.i = 0;
				} else {

					/* scale by shifting what we can and
					 * by adjusting exponent for rest.
					 * This creates a normalized number.
					 */
					shift = lzm + 1;
					expon.i = scale_factor.i - lzm;
				}
			/* No leading zeros in mantissa. */
			} else {

				/* Shift by 1 to normalize mantissa.  The
				 * rest of the scaling will be done
				 * through the exponent.
				 */
				shift = 1;
				expon.i = scale_factor.i;
			}
			/* position the exponent. */
			exponent.ui = expon.ui << IEEE_64_MANT_BITS;

			/* scale the mantissa. */
			mantissa.ui = orig_mantissa.ui << shift;

		/* Scale_factor.i LE 0. */
		} else {

			/* scale mantissa. */
			mantissa.ui = orig_mantissa.ui >> (-scale_factor.i);

			/* This is bit of magic that does some rounding. Get
			 * last bit that will be shifted off to right, if
			 * it's a 1 round the mantissa up by adding 1.
			 */
			if (((-scale_factor.ui) != 0) &&
				(orig_mantissa.ui & (0x1 <<
					(-scale_factor.i - 1)))) {
						mantissa.ui++;
			}
		}

		/* mask out any bits that may have shifted to the left of
		 * the mantissa area.
		 */
		mantissa.ui &= IEEE_64_MANTISSA;

		/* OR the new mantissa, the new exponent, and the original
		 * sign bit together to create the result.
		 */
		number.ui = mantissa.ui | exponent.ui | sign_bit.ui;
	} else {

		/* Number is Normal.
		 * Make an exponent an integer. 
		 */
		exponent.ui >>= IEEE_64_MANT_BITS;

		/* Add in the scale factor. */
		exponent.i += scale_factor.i;
		if (exponent.i >= IEEE_64_MAX_EXPONENT) {

			/* overflowed exponent. Use infinity. */
			number.ui = IEEE_64_INFINITY | sign_bit.ui;
		} else if (exponent.i <= 0) {

			/* Scaled exponent is negative or zero. */
			/* number.ui = (orig_mantissa.ui | (0x1 <<
			 *	IEEE_64_MANT_BITS)) >> (-exponent.i + 1);
			 * becomes zero on sparc.  Make 2 statements.
			 */
			number.ui = LL_CONST(0x1);
			number.ui = (((number.ui << IEEE_64_MANT_BITS) |
				orig_mantissa.ui) >> (-exponent.i + 1));
		} else {

			/* scaled exponent is positive and within range.
			 * Position into exponent.
			 */
		exponent.ui <<= IEEE_64_MANT_BITS;

			/* or the new mantissa, the new exponent, and the
			 * original sign bit together to create the result.
			 */
			number.ui =
				exponent.ui | orig_mantissa.ui | sign_bit.ui;
		}
	}
	return number.f;
}

#if	!defined(__mips)
#if _F_REAL16 == 1
extern _f_real16 _SCALE_16_I8(_f_real16 orig_number, _f_int8 orig_scale_factor);
extern _f_real16 _SCALE_16(_f_real16 orig_number, _f_int orig_scale_factor);
extern _f_real16 _SCALE_16_I4(_f_real16 orig_number, _f_int4 orig_scale_factor);

_f_real16
_SCALE_16_I4(_f_real16 orig_number, _f_int4 orig_scale_factor)
{
	_f_int	def_orig_scale;
	def_orig_scale =	orig_scale_factor;
	return(_SCALE_16(orig_number, def_orig_scale));
}

_f_real16
_SCALE_16_I8(_f_real16 orig_number, _f_int8 orig_scale_factor)
{
	int	def_orig_scale;
	def_orig_scale =	orig_scale_factor;
	return(_SCALE_16(orig_number, def_orig_scale));
}

_f_real16
_SCALE_16(_f_real16 orig_number, _f_int orig_scale_factor)
{
#if defined(_WORD32)
	union ldble_float {
		_f_real16		whole;
		unsigned long long	ui[1];
	} f, result;
	unsigned long long	sign_bit;
	unsigned long long	tmp_orig;
	long long		exponent;
#else
	union ldble_float {
		_f_real16		whole;
		unsigned long 		ui[1];
	} f, result;
	unsigned long 		sign_bit;
	unsigned long		tmp_orig;
	long 			exponent;
#endif

	union ldble_float orig_mantissa, mantissa;
	int	lz, lzm, ileadzcnt, shift, loopn, round;
	int	scale_factor, expon;
	int	tmp =	0;
	static int word_size =	64;

	/* eliminate the easy cases first. */
	if (orig_scale_factor == 0)
		return orig_number;
	if (orig_number == 0)
		return 0.0;

	f.whole =	orig_number;
	scale_factor =	orig_scale_factor;

	/* extract sign bit. */
	sign_bit =	IEEE_128_64_SIGN_BIT & f.ui[0];

	/* extract exponent. */
	exponent =      f.ui[0] & IEEE_128_64_EXPO;

	/* extract mantissa. */
	orig_mantissa.whole = f.whole;
	orig_mantissa.ui[0] &= IEEE_128_64_MANT1;

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

		/* Number is Normal.  Make an exponent an integer. */
		exponent >>=	(IEEE_128_MANT_BITS - word_size);

		/* Add in the scale factor. */
		exponent += scale_factor;
		if (exponent >= IEEE_128_MAX_EXPONENT) {

			/* overflowed exponent. Use infinity. */
			result.whole = 0.0;
			result.ui[0] = IEEE_128_64_EXPO | sign_bit;
		} else if (exponent <= LL_CONST(0x0)) {

			/* Scaled exponent is negative or zero.
			 * result.ui = (orig_mantissa.ui | (0x1 <<
			 *	IEEE_64_MANT_BITS)) >> (-exponent + 1);
			 * becomes zero on sparc.  Make 2 statements.
			 */
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
#endif
#endif
