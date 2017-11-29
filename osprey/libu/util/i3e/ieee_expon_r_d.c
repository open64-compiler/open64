/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
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


#pragma ident "@(#) libu/util/i3e/ieee_expon_r_d.c	92.1	07/07/99 14:37:09"
#include <fenv.h>
#include <fp.h>
#include "i3eintrin.h"

#ifndef _LD64
extern double _IEEE_EXPONENT_R_D(long double x);
/* _IEEE_EXPONENT_R_D - IEEE EXPONENT returns the exponent part of the
 *                       128-bit argument in 64-bit double result.
 *
 * IEEE_EXPONENT(X,Y_R8) returns real(kind=8)result
 */
#pragma _CRI duplicate _IEEE_EXPONENT_R_D as _IEEE_LOGB_R_D
double
_IEEE_EXPONENT_R_D(long double x)
{
	/* Union defined to work with IEEE 128 bit floating point. */
	union _ieee_ldouble {
		long double	dword;
		long long	lword[2];
		struct {
		   unsigned int sign		: 1;
		   unsigned int exponent	: IEEE_128_EXPO_BITS;
		   unsigned int mantissa_up	: IEEE_128_MANT_BITS_UP;
		   unsigned int mantissa_low	: IEEE_128_MANT_BITS_LOW;
		} lparts;
	};
	union _ieee_double {
		double  dwrd;
		long long    lwrd;
		struct {
#if __BYTE_ORDER == __LITTLE_ENDIAN
			unsigned int mantissa	: IEEE_64_MANT_BITS;
			unsigned int expon	: IEEE_64_EXPO_BITS;
			unsigned int sgn	: 1;
#else
			unsigned int sgn	: 1;
			unsigned int expon	: IEEE_64_EXPO_BITS;
			unsigned int mantissa	: IEEE_64_MANT_BITS;
#endif
		} parts;
	};
	switch (_fpclassifyl(x)) {
		case FP_NAN:
			{
			union _ieee_double x_val;
			x_val.lwrd		= _SGL_NaN;
			return(x_val.dwrd);
			}
		case FP_INFINITE:
			{
 			union _ieee_double x_val;
			x_val.lwrd		 = IEEE_64_INFINITY;
			return(x_val.dwrd);
			}
		case FP_NORMAL:
			{
			union _ieee_ldouble x_val;
			x_val.dword		 = x;
			return(x_val.lparts.exponent - IEEE_128_EXPO_BIAS);
			}
		case FP_SUBNORMAL:
			{
			union _ieee_ldouble x_val;
			int y;
			x_val.dword = x;

			/* _leadz returns number of zeros before first 1
			 * in mantissa. Add IEEE_128_EXPO_BITS to exclude
			 * exponent bits, but count sign bit since
			 * implicit bit needs to be counted.
			 */
			y	= _leadz(x_val.lparts.mantissa_up);

			if (y == 64) /* entire upper mantissa is zero */
				y += _leadz(x_val.lparts.mantissa_low);
			return(-IEEE_128_EXPO_BIAS - y +
				IEEE_128_EXPO_BITS);
			}
		case FP_ZERO:
			{
			int j;
			union _ieee_double x_val;

			/* raise divide-by-zero exception */
			j		= FE_DIVBYZERO;
			feraiseexcept(j);

			/* return negative infinity */
			x_val.lwrd	= IEEE_64_INFINITY;
			x_val.parts.sgn	= 1;
			return(x_val.dwrd);
			}
	}
}
#endif  /* not LD64 */
