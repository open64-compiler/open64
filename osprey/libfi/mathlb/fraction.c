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


#pragma ident "@(#) libfi/mathlb/fraction.c	92.1	07/09/99 11:00:36"
#include <fortran.h>
#include <fp.h>
#ifdef  __mips
#include <math.h>
#endif
#include "inline.h"
#include "leadz.h"

/***  Algorithm for f90 FRACTION
 * FRACTION - return the fractional part of the 32-bit or 64-bit model
 * 		representation of the argument value.
 **/

#ifdef DO_INLINE
inline static	/* for the inline version of this function. */
#endif
#ifdef _F_REAL4
_f_real4
_FRACTION_4(_f_real4 x)
{
	int             lz;
	REGISTER_4      s1, sign_bit, mantissa, exponent;
	if (x == 0.0)
		return 0.0;
	/* if x is either infinity or a NaN, return a Nan */
	if (x == IEEE_32_INFINITY)
		return	_HALF_NaN;
	if (isnan32(x))
		return	x;
	s1.f = x;

	/* get sign bit */
	sign_bit.ui = IEEE_32_SIGN_BIT & s1.ui;

	/* get mantissa */
	mantissa.ui = IEEE_32_MANTISSA & s1.ui;

	/* get exponent */
	exponent.ui = IEEE_32_EXPONENT & s1.ui;
	if (exponent.ui == 0x0) {
		/* number is subnormal.  normalize mantissa. */

		/* 1. Get leading zeros of mantissa. */
		lz = _leadz4(mantissa.ui) - IEEE_32_EXPO_BITS;

		/* 2. Normalize by shifting out all leading zeros and
		 *    first 1 bit.
		 */
		mantissa.ui = (mantissa.ui << lz) & IEEE_32_MANTISSA;
	}

	/* position the exponent bias less the implicit bit */
	exponent.ui = (IEEE_32_EXPO_BIAS - 1) << IEEE_32_MANT_BITS;

	/* extract fraction. */
	s1.ui = exponent.ui | mantissa.ui | sign_bit.ui;
	return s1.f;
}
#endif	/* _F_REAL4 */

/* provide this entry point only until the compiler changes to _FRACTION. */
_f_real8
_FRACTION_8(_f_real8 x)
{
	_f_real8 _FRACTION(_f_real8 x);
	return (_FRACTION(x) );
}

#ifdef DO_INLINE
inline static		/* for the inline version of this function. */
#endif
_f_real8
_FRACTION(_f_real8 x)
{
	int             lz;
	REGISTER_8      s1, sign_bit, mantissa, exponent;
	if (x == 0.0)
		return 0.0;
	/* if x is either infinity or a NaN, return a Nan */
	if (x == IEEE_64_INFINITY)
		return	_SGL_NaN;
	if (isnan64(x))
		return	x;
	s1.f = x;

	/* get sign bit. */
	sign_bit.ui = IEEE_64_SIGN_BIT & s1.ui;

	/* get mantissa. */
	mantissa.ui = IEEE_64_MANTISSA & s1.ui;

	/* get exponent. */
	exponent.ui = IEEE_64_EXPONENT & s1.ui;
	if (exponent.ui == LL_CONST(0x0)) {

		/* 1. number is subnormal.  normalize mantissa.
		 * Get leading zeros of mantissa.
		 */
		lz = _leadz8(mantissa.ui) - IEEE_64_EXPO_BITS;

		/* 2. normalize by shifting out all leading zeros
		 *    and first 1 bit.
		 */
		mantissa.ui = (mantissa.ui << lz) & IEEE_64_MANTISSA;
	}

	/* position exponent bias less the implicit bit. */
	exponent.ui = IEEE_64_EXPO_BIAS - 1;
	exponent.ui = exponent.ui << IEEE_64_MANT_BITS;

	/* extract fraction. */
	s1.ui = exponent.ui | mantissa.ui | sign_bit.ui;
	return s1.f;
}

#ifndef	__mips
#if _F_REAL16 == 1
#ifdef DO_INLINE
inline static           /* for the inline version of this function. */
#endif

#define MIN(a,b)        ((a) < (b) ? (a) : (b))

/***  Algorithm for f90 FRACTION
 * FRACTION - return the fractional part of the 128-bit model
 * 		representation of the argument value.
 **/

_f_real16
_FRACTION_16(_f_real16 x)
{
	int	lz, ileadzcnt, loopn;
#if defined(_WORD32)
	union ldble_float {
		_f_real16		whole;
		unsigned long long	ui[1];
	} f, result, mantissa;
	unsigned long long	sign_bit, exponent;
#else
	union ldble_float {
		_f_real16		whole;
		unsigned long           ui[1];
	} f, result;
	unsigned long		sign_bit, exponent;
#endif

	static int word_size =	64;

        if (x == 0.0)
                return 0.0;
	f.whole =	x;
	/* if x is either infinity or a NaN, return a Nan */
	if ((f.ui[0] == IEEE_128_64_EXPO) && (f.ui[1] == 0))
			return	_DBL_NaN;
	if (isnan128(x))
		return	x;

        /* get sign bit. */
        sign_bit =	IEEE_128_64_SIGN_BIT & f.ui[0];

	/* Get the absolute value of x by ANDing the upper half
	 * with the NOT of 0x8000000000000000 (the sign bit mask).
	 */
	f.ui[0] &= ~IEEE_128_64_SIGN_BIT;

        /* get exponent. */
	exponent =	f.ui[0] & IEEE_128_64_EXPO;

        /* get mantissa and zero out exponent portion */
	f.ui[0] &= IEEE_128_64_MANT1;

	result.whole =	f.whole;
	if (exponent == LL_CONST(0x0)) {

		/* 1. Number is subnormal.  Normalize mantissa and
		 * get leading zeros in mantissa
		 */
		lz =	0;
		for (loopn = 0; loopn < 2; loopn++) {
			ileadzcnt = _leadz8(f.ui[loopn]);
			lz += ileadzcnt;
			if (ileadzcnt < word_size)
				break;
		}
		lz = lz - IEEE_128_EXPO_BITS;

		/* 2. Normalize by shifting out all leading zeros
		 *    and first 1 bit.
		 * Determine number of mantissa bits in first 64 bits
		 * of the mantissa plus the implicit bit.
		 */
		ileadzcnt =	word_size - IEEE_128_EXPO_BITS;

		if (lz >= ileadzcnt) {

			/* The first 48 bits of the mantissa are zero. */
			result.ui[0] = (f.ui[1] << (lz - ileadzcnt)) >>
				IEEE_128_EXPO_BITS;
			result.ui[1] = f.ui[1] << (MIN(word_size,lz));
		} else {
			result.ui[0] = (f.ui[0] << lz) || 
					(f.ui[1] >> (word_size - lz));
			result.ui[1] = f.ui[1] << lz;
		}

		/* remove the implicit bit */
		result.ui[0] &= IEEE_128_64_MANT1;
	}

	/* position exponent bias less the implicit bit. */
	exponent = (IEEE_128_EXPO_BIAS - 1);
	exponent <<= (IEEE_128_MANT_BITS - word_size);

	/* extract fraction. */
	result.ui[0] = exponent | result.ui[0] | sign_bit;
	return result.whole;
}

#endif	/* _F_REAL16 == 1 */
#else	/* NOT mips */
/*  This routine returns the fraction part of its real argument.  */
_f_real16
_FRACTION_16(_f_real16 a)
{
	_f_real16	result, aa;
	_f_int4		 ia;
	if (a == 0.0L)
		result	= 0.0L;
	else
		result = _get_frac_and_exp( a, &ia );
	return result;
}
#endif	/* NOT mips */
