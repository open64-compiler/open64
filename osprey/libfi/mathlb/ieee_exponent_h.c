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


#pragma ident "@(#) libfi/mathlb/ieee_exponent_h.c	92.1	07/09/99 11:00:36"
#include <fortran.h>
#include <fp.h>
#include "inline.h"
#include "leadz.h"

#ifdef _F_REAL4
/* IEEE_EXPONENT(X,Y_R4) returns real(kind=4)result: */
extern _f_real4 _IEEE_EXPONENT_H_H(_f_real4 x);
extern _f_real4 _IEEE_EXPONENT_H_R(_f_real8 x);
#if _F_REAL16 == 1
extern _f_real4 _IEEE_EXPONENT_H_D(_f_real16 x);
#endif

static _f_real4 _raisdivz(_f_real4 y,_f_real4 x);

static _f_real4 _raisdivz(_f_real4 y,_f_real4 x)
{
	return (y/x);
}

/* _IEEE_EXPONENT_H_H - IEEE EXPONENT returns the exponent part of the
 *                      32-bit argument in 32-bit real.
 */
#ifndef __mips
#pragma _CRI duplicate _IEEE_EXPONENT_H_H as _IEEE_LOGB_H_H
#endif
_f_real4
_IEEE_EXPONENT_H_H(_f_real4 x)
{
	int		i;
	REGISTER_4	s1, s2;

	/* if x is a NaN, return a Nan */
	if (isnan32(x))
		return x;
	s1.f = x;

	/* clear sign bit. */
	s1.ui &= ~IEEE_32_SIGN_BIT;

	/* if x is + or -infinity, return a +infinity */
	if (s1.i == IEEE_32_INFINITY)
		return s1.f;

	/* if x is 0.0, return a -infinity and
	 * raise divide by zero exception.
	 */
	if (x == (_f_real4) 0.0e0) {
		_f_real4 dvzr = 1.0;
		_f_real4 result4;
		result4 = _raisdivz(dvzr,x);
		s1.ui = IEEE_32_INFINITY | IEEE_32_SIGN_BIT;
		return s1.f;
	}

	/* shift exponent bits to right. */
	s1.i >>= IEEE_32_MANT_BITS;
	if (s1.ui == 0) {

		/* x is a subnormal number (implicit leading bit is zero
		 * and the exponent is zero).  calculate the exponent
		 * based on normalized x.
		 *
		 * get mantissa
		 */
		s2.f = x;
		s2.ui = IEEE_32_MANTISSA & s2.ui;

		/* get leading zeros in mantissa part. */
		i = _leadz4(s2.ui) - IEEE_32_EXPO_BITS;
		/* calculate exponent. */
		s1.i -= (IEEE_32_EXPO_BIAS + i);
	} else {
		/* subtract exponent bias. */
		s1.i -= IEEE_32_EXPO_BIAS;
	}
	return (_f_real4) s1.i;
}


/* _IEEE_EXPONENT_H_R - IEEE EXPONENT returns the exponent part of the
 *                      64-bit argument in 32-bit real.
 */
#ifndef __mips
#pragma _CRI duplicate _IEEE_EXPONENT_H_R as _IEEE_LOGB_H_R
#endif
_f_real4
_IEEE_EXPONENT_H_R(_f_real8 x)
{
	int		i;
	REGISTER_8	s1, s2;
	REGISTER_4	s3;

	/* if x is a NaN, return a Nan */
	if (isnan64(x))
		return _HALF_NaN;
	s1.f = x;

	/* clear sign bit. */
	s1.ui &= ~IEEE_64_SIGN_BIT;

	/* if x is + or -infinity, return a +infinity */
	if (s1.i == IEEE_64_INFINITY) {
		s3.ui = IEEE_32_INFINITY;
		return s3.f;
	}

	/* if x is 0.0, return a -infinity and
	 * raise divide by zero exception.
	 */
	if (x == (_f_real8) 0.0e0) {
		_f_real4 dvzr = 1.0;
		_f_real4 result4;
		result4 = _raisdivz(dvzr,x);
		s3.ui = IEEE_32_INFINITY | IEEE_32_SIGN_BIT;
		return s3.f;
	}

	/* shift exponent bits to right. */
	s1.ui >>= IEEE_64_MANT_BITS;
	if (s1.ui == 0) {

		/* x is a subnormal number (implicit leading bit is zero
		 * and the exponent is zero).  Calculate the exponent
		 * based on normalized x.
	 	 *
		 * get mantissa
		 */
		s2.f = x;
		s2.ui = IEEE_64_MANTISSA & s2.ui;

		/* get leading zeros in mantissa part. */
		i = _leadz8(s2.ui) - IEEE_64_EXPO_BITS;
		/* calculate exponent. */
		s1.i -= (IEEE_64_EXPO_BIAS + i);
	} else {
		/* subtract exponent bias. */
		s1.i -= IEEE_64_EXPO_BIAS;
	}
	return (_f_real4) s1.i;
}


#if _F_REAL16 == 1
/* _IEEE_EXPONENT_H_D - IEEE EXPONENT returns the exponent part of the
 *                       128-bit argument in 32-bit integer.
 */
#ifndef __mips
#pragma _CRI duplicate _IEEE_EXPONENT_H_D as _IEEE_LOGB_H_D
#endif
_f_real4
_IEEE_EXPONENT_H_D(_f_real16 x)
{
	int		i, ileadzcnt, loopn;
	REGISTER_4	s3;
	union ldble_float {
		_f_real16		whole;
		unsigned long long	ui[2];
		long long		si[2];
	} f, result;

	static int word_size =	(sizeof(_f_real8) << 3);

	/* if x is a NaN, return a Nan */
	if (isnan128(x))
		return _HALF_NaN;
	f.whole =	x;

	/* Get the absolute value of x by ANDing the upper half
	 * with the NOT of 0x8000000000000000 (the sign bit mask).
	 */
	f.ui[0] &= ~IEEE_128_64_SIGN_BIT;

	/* if x is + or -infinity, return a +infinity */
	if ((f.ui[0] == IEEE_128_64_EXPO) && (f.ui[1] == 0)) {
		s3.ui = IEEE_32_INFINITY;
		return s3.f;
	}

	/* if x is 0.0, return a -infinity and
	 * raise divide by zero exception.
	 */
	if (x == (_f_real16) 0.0e0) {
		_f_real4 dvzr = 1.0;
		_f_real4 result4;
		result4 = _raisdivz(dvzr,x);
		s3.ui = IEEE_32_INFINITY | IEEE_32_SIGN_BIT;
		return s3.f;
	}

	/* Separate the exponent from the 128-bit float value and
	 * right justify it.
	 */
	result.ui[0] = f.ui[0] >> (IEEE_128_MANT_BITS - word_size);
	if (result.ui[0] == 0) {

		/* x is a subnormal number (implicit leading bit is zero
		 * and the exponent is zero).  Calculate the exponent
		 * based on normalized x.
		 *
		 * get mantissa
		 */
		f.ui[0] &= IEEE_128_64_MANT1;
		i = 0;

		/* get leading zeros in mantissa part */
		for (loopn = 0; loopn < 2; loopn++) {
			ileadzcnt = _leadz8(f.ui[loopn]);
			i += ileadzcnt;
			if (ileadzcnt < word_size)
				break;
		}
		i = i - IEEE_128_EXPO_BITS;

		/* calculate exponent. */
		result.si[0] -= (IEEE_128_EXPO_BIAS + i);
	} else {
		/* subtract exponent bias. */
		result.si[0] -= IEEE_128_EXPO_BIAS;
	}
	return (_f_real4) result.si[0];
}

#endif  /* FREAL16 */

#endif  /* FREAL4 */
