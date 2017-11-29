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


#pragma ident "@(#) libu/util/i3e/ieee_remainder_r_h.c	92.1	07/07/99 14:37:09"

#include <fenv.h>
#include <fp.h>

extern double _IEEE_REMAINDER_R_H(double x, float y);

/*
 * _IEEE_REMAINDER_R_H(X,Y) - calculate remainder of one real(8)
 *                            and one real(4) argument.
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
 *
 */

#pragma _CRI duplicate _IEEE_REMAINDER_R_H as _IEEE_REMAINDER_8_4
double
_IEEE_REMAINDER_R_H(double argx, float argy)
{
	union _ieee_fdouble {
		float		dword;
		short		lword;
	};
	double  __remainder_r(double x, double y);
	double y_val;
	int xfpclas	= _fpclassify(argx);
	int yfpclas	= _fpclassifyf(argy);
	if ((xfpclas == FP_NAN) || yfpclas == FP_ZERO) {
		union _ieee_fdouble x_val;
		int	j;

		x_val.dword	= _HALF_NaN;

		/* need to emit invalid exception */
		j	= FE_INVALID;
		feraiseexcept(j);
		return(x_val.dword);
	}
	y_val	= (double) argy;
	return(__remainder_r(argx, y_val));
}
