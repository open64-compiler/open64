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


#pragma ident "@(#) libfi/mathlb/ieee_remainder_h.c	92.1	07/09/99 11:00:36"

#include <fortran.h>
#include <fp.h>
#include "inline.h"

#define IEEE_32_MANT_BITS_H1	11
#define IEEE_32_MANT_BITS_H2	12

extern _f_real4 _IEEE_REMAINDER_H_H(_f_real4 x, _f_real4 y);

static _f_real4 _raisinvld(_f_real4 x, _f_real4 y);

static _f_real4 _raisinvld(_f_real4 x, _f_real4 y)
{
        return (x/y);
}

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
 */

_f_real4
_IEEE_REMAINDER_H_H(_f_real4 argx, _f_real4 argy)
{
	/* Union defined to work with IEEE 64-bit floating point. */
	union _ieee_fdouble {
		_f_real4	fpword;
		unsigned int	lword;
		int		int32;
	};

	unsigned int	sign_x = 0X80000000;
	unsigned int	even_x = 0X00000001;
	union _ieee_fdouble x_val, y_val, nearint, tdiv, evenchk, tmp, res;
	x_val.fpword	= argx;
	y_val.fpword	= argy;

	/* check input values: x for infinity and y for zero. */
	if (((x_val.lword & ~IEEE_32_SIGN_BIT) == IEEE_32_INFINITY) ||
	   ((!(isnan32(y_val.fpword))) && (y_val.fpword == 0.0))) {
		union _ieee_fdouble x_val;
		_f_real4	result4;
		_f_real4	arg4 = 0.0;

		x_val.lword	= _HALF_NaN;

		/* need to emit invalid exception */
		result4		= _raisinvld(arg4,arg4);
		return(x_val.fpword);
	}
	tdiv.fpword	= argx / argy;
	tmp.lword	= tdiv.lword & (~sign_x);

	/* check for 2**23 or greater = already integer */
	if (tmp.fpword < 8388608) {

		/* calculate fraction */
		evenchk.fpword =
			tdiv.fpword - (_f_real4)((_f_int4)tdiv.fpword);

		if (tdiv.fpword < 0.0) {
			nearint.int32 = (_f_int4) (tdiv.fpword - 0.5);
			if ((evenchk.fpword == -0.5) &&
			   ((nearint.lword & even_x) != 0))
				nearint.int32 += 1;
		} else {
			nearint.int32 = (_f_int4) (tdiv.fpword + 0.5);
			if ((evenchk.fpword == 0.5) &&
			   ((nearint.lword & even_x) != 0))
				nearint.int32 -= 1;
		}
		tdiv.fpword = (_f_real4) nearint.int32;
	}

	/* algorithm for ieee in 64-bits for x - (x/y)*y. */
	res.fpword  = (_f_real4) ((_f_real8) x_val.fpword -
		((_f_real8) tdiv.fpword * (_f_real8) y_val.fpword));
	if (res.fpword == 0.0)
		res.lword= res.lword | (x_val.lword & IEEE_32_SIGN_BIT);
	return(res.fpword);
}
