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


#pragma ident "@(#) libfi/mathlb/dint.c	92.1	07/09/99 11:00:36"

#include <fortran.h>

extern _f_real16 _DINT_(_f_real16 *x);
extern _f_real16 _DINT(_f_real16 x);

/*
 * DINT:  Truncate real(kind=16) to a whole number
 *        - pass by value
 */
_f_real16
_DINT(_f_real16 x)
{
	union ldble_float {
		struct {
			unsigned	upperl;
			unsigned	upperr;
			unsigned	lowerl;
			unsigned	lowerr;
		} parts;
		_f_real16	whole;
	} f, result;

	static union ldble_float four_112 = {0x406F0000, 0x00000000,
			0x00000000, 0x00000000};
	unsigned	abs_x;
	unsigned	sign_x = 0x80000000;
	_f_real16	tmp;

	f.whole =	x;

	/* Get the absolute value of x by ANDing the upper half
	 * with the NOT of 0x80000000 (the sign bit mask).
	 */
	abs_x = f.parts.upperl & (~sign_x);

	/* See what the fraction entails.  If greater than 2**112,
	 * return x since value .GE. 2**(112) is an integer.  The
	 * size of the mantissa is 112 and the value of x must be
	 * (f_real16) x >= 5070602400912917605986812821504 to be
	 * an integer.
	 */
	if (abs_x >= four_112.parts.upperl)
		return (x);

	/* Otherwise, clear the fraction by adding 2**112, then
	 * subtracting 2**112.  The hardware will round the result,
	 * thus clearing it.
	 */
	result.parts.upperl = abs_x;
	result.parts.upperr = f.parts.upperr;
	result.parts.lowerl = f.parts.lowerl;
	result.parts.lowerr = f.parts.lowerr;
	tmp = result.whole + four_112.whole;
	tmp -= four_112.whole;

	/* If this is greater than the absolute value of x, subtract 1.0;
	 * otherwise it's already truncated.
	 */
	if (tmp > result.whole)
		tmp -= (_f_real16) 1.0;
	result.whole = tmp;

	/* Now reset the sign bit and we're done. */
	result.parts.upperl = (f.parts.upperl & sign_x) |
		 result.parts.upperl;
	return (result.whole);
}

/*
 * DINT:  Truncate real(kind=16) to a whole number
 *        - pass by address
 */

_f_real16
_DINT_(_f_real16 *x)
{
	return (_DINT(*x));
}
