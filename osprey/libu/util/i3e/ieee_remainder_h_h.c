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


#pragma ident "@(#) libu/util/i3e/ieee_remainder_h_h.c	92.1	07/07/99 14:37:09"

#include <fenv.h>
#include <fp.h>
#include "i3eintrin.h"

extern float _IEEE_REMAINDER_H_H(float x, float y);

#define IEEE_32_MANT_BITS_H1    11
#define IEEE_32_MANT_BITS_H2    12

/*
 * _IEEE_REMAINDER_H_H(X,Y) - calculate remainder of two real(4)
 *                            arguments.
 *
 *              The standard definition for real is:
 *                 If y = 0, remainder(x,y) = NaN and raise the INVALID
 *                           exception.
 *                 else if x = INFINITY, remainder(x,y) = NaN and raise
 *                           the INVALID exception.
 *                 else remainder(x,y) = x - (REAL(NINT(x/y))) * y
 *
 *              The algorithm for real is:
 *                x - (REAL(NINT(x/y))) * y.
 *
 * Use the following algorithm for x/y rounded quantity:
 *   1.  x/y
 *   2. if(fraction(x/y) = .5 exactly, round to next EVEN number.
 *           0.5 = 0.0, 1.5 + 2.0, 10.5 = 10, etc.
 * Use the following algorithm for the multiply:
 *   ((((x - tdivU *yU) - tdivU * yL) -tdivL * yU) - tdivL * yL)
 * Th calculation is done in double (64-bit) real precision so the
 * upper and lower split is not required.
 */

#pragma _CRI duplicate _IEEE_REMAINDER_H_H as _IEEE_REMAINDER_4_4
#pragma _CRI duplicate _IEEE_REMAINDER_H_H as _IEEE_REMAINDER_4
#pragma _CRI duplicate _IEEE_REMAINDER_H_H as _IEEE_REMAINDER_H
#pragma _CRI duplicate _IEEE_REMAINDER_H_H as _REMAINDER_4
#pragma _CRI duplicate _IEEE_REMAINDER_H_H as _REMAINDER_H
float
_IEEE_REMAINDER_H_H(float argx, float argy)
{
	union _ieee_float {
		float		fpword;
		unsigned int	usword;
		int		int32;
		struct {
#if __BYTE_ORDER == __LITTLE_ENDIAN
			unsigned int mantissa	: IEEE_32_MANT_BITS;
			unsigned int exponent	: IEEE_32_EXPO_BITS;
			unsigned int sign	: 1;
#else
			unsigned int sign	: 1;
			unsigned int exponent	: IEEE_32_EXPO_BITS;
			unsigned int mantissa	: IEEE_32_MANT_BITS;
#endif
		} parts;
	};
	union _ieee_float x_val, y_val, nearint, tdiv, evenchk, tmp, res;
	unsigned int	even_x = 0X00000001;
	int xfpclas	= _fpclassifyf(argx);
	int yfpclas	= _fpclassifyf(argy);
	x_val.fpword	= argx;
	y_val.fpword	= argy;

	if ((xfpclas == FP_INFINITE) || yfpclas == FP_ZERO) {
		union _ieee_float x_val;
		int	j;

		x_val.fpword	= _HALF_NaN;

		/* need to emit invalid exception */
		j	= FE_INVALID;
		feraiseexcept(j);
		return(x_val.fpword);
	}
	tdiv.fpword	= argx / argy;
	tmp.usword	= tdiv.usword & (~IEEE_32_SIGN_BIT);

	/* check for 2**23 or greater = already integer */
	if (tmp.fpword < 8388608) {

		/* calculate fraction */
                evenchk.fpword =
                        tdiv.fpword - (float)((int)tdiv.fpword);

                if (tdiv.fpword < 0.0) {
                        nearint.int32 = (int) (tdiv.fpword - 0.5);
                        if ((evenchk.fpword == -0.5) &&
                           ((nearint.usword & even_x) != 0))
                                nearint.int32 += 1;
                } else {
                        nearint.int32 = (int) (tdiv.fpword + 0.5);
                        if ((evenchk.fpword == 0.5) &&
                           ((nearint.usword & even_x) != 0))
                                nearint.int32 -= 1;
                }
                tdiv.fpword = (float) nearint.int32;
        }

	/* algorithm for ieee in 64-bits for x - (x/y)*y. */
	res.fpword	= (float) ((double) x_val.fpword -
		((double) tdiv.fpword * (double) y_val.fpword));
	if (res.fpword == 0.0)
		res.usword= res.usword | (x_val.usword & IEEE_32_SIGN_BIT);
	return(res.fpword);
}
