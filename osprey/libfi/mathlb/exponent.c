/*
 * Copyright 2004, 2005 PathScale, Inc.  All Rights Reserved.
 */

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


#pragma ident "@(#) libfi/mathlb/exponent.c	92.1	07/09/99 11:00:36"

#include <fortran.h>
#include "inline.h"
#include "leadz.h"

/* Remove this entry point when the SPARC f90 compiler calls EXPONENT for
 * for an f_real8 argument.
 */
_f_int
_EXPONENT_8(_f_real8 x)
{
	_f_int _EXPONENT(_f_real8 x);
	return(_EXPONENT(x));
}


/* _EXPONENT - F90 EXPONENT returns the exponent part of the 64-bit
 *                          argument in default integer.
 */
_f_int
_EXPONENT(_f_real8 x)
{
	int             i;
	REGISTER_8      s1, s2;
	if (x == 0.0e0)
		return 0;
	s1.f = x;
	s1.ui &= ~IEEE_64_SIGN_BIT; /* clear sign bit. */
	s1.ui >>= IEEE_64_MANT_BITS; /* shift exponent bits to right. */
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
		i = _leadz8(s2.ui) - IEEE_64_EXPO_BITS - 1;
		/* calculate exponent. */
# ifdef KEY
// Bug 2365
		s1.i -= (IEEE_64_EXPO_BIAS - 1 + i);
# else
		s1.i -= (IEEE_64_EXPO_BIAS + i);
# endif
	} else {
		/* subtract exponent bias and implicit bit. */
		s1.i -= (IEEE_64_EXPO_BIAS - 1);
	}
	return s1.i;
}

#ifdef _F_INT4
/* _EXPONENT_4 - F90 EXPONENT returns the exponent part of the 32-bit
 *                          argument in default integer.
 */
_f_int
_EXPONENT_4(_f_real4 x)
{
	int             i;
	REGISTER_4      s1, s2;
	if (x == (float) 0.0e0)
		return 0;
	s1.f = x;

	/* clear sign bit. */
	s1.ui &= ~IEEE_32_SIGN_BIT;

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
		i = _leadz4(s2.ui) - IEEE_32_EXPO_BITS - 1;
		/* calculate exponent. */
#ifdef KEY /* Bug 2365 */
		s1.i -= (IEEE_32_EXPO_BIAS - 1 + i);
#else
		s1.i -= (IEEE_32_EXPO_BIAS + i);
#endif /* KEY */
	} else {
		/* subtract exponent bias and implicit bit. */
		s1.i -= (IEEE_32_EXPO_BIAS - 1);
	}
	return s1.i;
}
#endif

#if _F_REAL16 == 1
/* _EXPONENT_16 - F90 EXPONENT returns the exponent part of the 128-bit
 *                          argument in default integer.
 */
#if !defined(__mips)
_f_int
_EXPONENT_16(_f_real16 x)
{
	int		i, ileadzcnt, loopn;
#if defined(_WORD32)
	union ldble_float {
		_f_real16		whole;
		unsigned long long	ui[1];
		long long		si[2];
	} f, result;
#else
	union ldble_float {
		_f_real16		whole;
		unsigned long		ui[1];
		long 			si[2];
	} f, result;
#endif

	static int word_size =	64;
	if (x == 0.0e0)
		return 0;
	f.whole =	x;

	/* Get the absolute value of x by ANDing the upper half
	 * with the NOT of 0x8000000000000000 (the sign bit mask).
	 */
	f.ui[0] &= ~IEEE_128_64_SIGN_BIT;

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
		i = i - IEEE_128_EXPO_BITS - 1;

		/* calculate exponent. */
#ifdef KEY /* Bug 3395 */
		result.si[0] -= (IEEE_128_EXPO_BIAS - 1 + i);
#else
		result.si[0] -= (IEEE_128_EXPO_BIAS + i);
#endif /* KEY */
	} else {
		/* subtract exponent bias and implicit bit. */
		result.si[0] -= (IEEE_128_EXPO_BIAS - 1);
	}
	return result.si[0];
}
#else
_f_int
_EXPONENT_16(_f_real16 x)
{
	int		i;
	REGISTER_16	s1, s2;
	if (x == 0.0e0)
		return 0;
	s1.f = x;
	s1.ui[0] &= ~IEEE_64_SIGN_BIT; /* clear sign bit. */
	s1.ui[0] >>= IEEE_64_MANT_BITS; /* shift exponent bits to right */
	if (s1.ui[0] == 0) {

		/* x is a subnormal number (implicit leading bit is zero
		 * and the exponent is zero).  Calculate the exponent
		 * based on normalized x.
		 *
		 * get mantissa
		 */
		s2.f = x;
		s2.ui[0] = IEEE_64_MANTISSA & s2.ui[0];

		/* get leading zeros in mantissa part. */
		i = _leadz8(s2.ui[0]) - IEEE_64_EXPO_BITS - 1;
		/* calculate exponent. */
#ifdef KEY /* Bug 3395 */
		s1.i[0] -= (IEEE_64_EXPO_BIAS - 1 + i);
#else
		s1.i[0] -= (IEEE_64_EXPO_BIAS + i);
#endif /* KEY */
	} else {
		/* subtract exponent bias and implicit bit. */
		s1.i[0] -= (IEEE_64_EXPO_BIAS - 1);
	}
	return s1.i[0];
}
#endif		/* end not __mips */
#endif		/* end _F_REAL16 = 1 */
