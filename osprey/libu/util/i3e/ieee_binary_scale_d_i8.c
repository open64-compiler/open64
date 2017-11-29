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


#pragma ident "@(#) libu/util/i3e/ieee_binary_scale_d_i8.c	92.1	07/07/99 14:37:09"

#ifndef _LD64
#include <fenv.h>
#include <fp.h>

extern int _leadz(int);
extern long double _IEEE_BINARY_SCALE_D_I8(long double x, int n);

/* BINARY_SCALE - computes x * 2**n efficiently by not computing 2**n
 *	INPUT:   128 bit real x
 *	         64 bit integer n
 *	OUTPUT:  128 bit real result
 */

#pragma _CRI duplicate _IEEE_BINARY_SCALE_D_I8 as _IEEE_BINARY_SCALE_D
#pragma _CRI duplicate _IEEE_BINARY_SCALE_D_I8 as _IEEE_BINARY_SCALE_16
long double
_IEEE_BINARY_SCALE_D_I8(long double x, int n)
{  
	/* Union defined to work with IEEE 128 bit floating point. */
	union _ieee_ldouble {
		long double	dword;
		long		lword[2];
		struct {
			unsigned int sign	  : 1;
			unsigned int exponent	  : IEEE_128_EXPO_BITS;
			unsigned int mantissa_up  : IEEE_128_MANT_BITS_UP;
			unsigned int mantissa_low : IEEE_128_MANT_BITS_LOW;
		} parts;
	};
	int fp_class	= _fpclassifyl(x);

	if (fp_class==FP_NORMAL) {
		union _ieee_ldouble x_val;
		long exponent;
		x_val.dword	= x;

		exponent	= x_val.parts.exponent + n;

		if (exponent <= 0) {
			int	j;
			unsigned long	mantissa_bits;
			unsigned long	mantissa_up = x_val.parts.mantissa_up;
			unsigned long	mantissa_low = x_val.parts.mantissa_low;
			int	shift;

			/* need to emit underflow exception */
			j	= FE_UNDERFLOW;
			feraiseexcept(j);

			/* add implicit bit to mantissa */
			mantissa_up |=  IEEE_128_IMPLICIT_BIT;

			/* shift mantissa over by exponent remaining */
			shift	= -exponent + 1;
			if (shift <= 64) {
				mantissa_bits	= mantissa_up << (64 - shift);
				mantissa_up >>= shift;
				mantissa_low	= (mantissa_low >> shift) |
						mantissa_bits;
			} else {
				mantissa_low	= mantissa_up >> (shift - 64);
				mantissa_up	= 0;
			}

			/* return denormal number */
			x_val.parts.exponent		= 0;
			x_val.parts.mantissa_up		= mantissa_up;
			x_val.parts.mantissa_low	= mantissa_low;
		} else if ((exponent>>IEEE_128_EXPO_BITS) != 0) {
			int	j;
			unsigned int k	= x_val.parts.sign;

			/* need to emit overflow exception */
			j	= FE_OVERFLOW;
			feraiseexcept(j);

			/* return infinity */
			x_val.lword[0]		= INFINITY_128_UP;
			x_val.lword[1]		= INFINITY_128_LOW;
			x_val.parts.sign	= k;
		} else {
			x_val.parts.exponent	= exponent;
		}
		return(x_val.dword);
	} else if (fp_class==FP_SUBNORMAL) {
		union _ieee_ldouble	x_val;
		unsigned long	mantissa_bits;
		unsigned long	mantissa_up;
		unsigned long	mantissa_low;

		x_val.dword	= x;

		mantissa_up	= x_val.parts.mantissa_up;
		mantissa_low	= x_val.parts.mantissa_low;

		if (n <= 0) {
			int	saven;
			saven	= -n;
			if (saven <= 64) {
				mantissa_bits	= mantissa_up << (64 - saven);
				mantissa_up >>= saven;
				mantissa_low	= mantissa_bits |
						(mantissa_low >> saven);
			} else {
				mantissa_low	= mantissa_up >> (saven - 64);
				mantissa_up	= 0;
			}
			x_val.parts.mantissa_up		= mantissa_up;
			x_val.parts.mantissa_low	= mantissa_low;
		} else {
			long	mantissa_bits_to_fill = _leadz(mantissa_up);
			if (mantissa_bits_to_fill == 64) {
				mantissa_bits_to_fill += _leadz(mantissa_low);
			}
			if (mantissa_bits_to_fill >= n) {
				unsigned long	mantissa_bits;

				if (n <= 64) {
					mantissa_bits	= mantissa_low >>
							(64 - n);
					mantissa_low <<= n;
					mantissa_up	= mantissa_bits |
						(mantissa_up << n);
				} else {
					mantissa_up	= mantissa_low <<
						(n - 64);
					mantissa_low	= 0;
				}

			} else {

				/* Return a normal number.
				 * Shift out implicit bit.
				 */
				int	shift;

				shift	= mantissa_bits_to_fill +1;
				if (shift <= 64) {
					mantissa_bits = mantissa_low <<
							(64 - shift);
					mantissa_low <<= shift;
					mantissa_up	= mantissa_bits |
							(mantissa_up << shift);
				} else {
					mantissa_up	= mantissa_low <<
							(shift-64);
					mantissa_low	= 0;
				}
				x_val.parts.exponent	= n -
						mantissa_bits_to_fill + 1;
			}
			x_val.parts.mantissa_low	= mantissa_low;
			x_val.parts.mantissa_up		= mantissa_up;
		}

		return(x_val.dword);
	} else { /* NAN, INFINITE, or ZERO */
		return(x);
	}
}

#endif  /* not LD64 */
