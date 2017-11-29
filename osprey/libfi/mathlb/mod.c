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


#pragma ident "@(#) libfi/mathlb/mod.c	92.1	07/09/99 11:00:36"


#include <fortran.h>
#include <math.h>
#include "inline.h"

#define IEEE_64_MANT_BITS_H1    26
#define IEEE_64_MANT_BITS_H2    27

extern _f_real8 _AMOD_(_f_real8 *x,  _f_real8 *y);

/*
 * AMOD: real(kind=8) - pass by address
 *    Remainder function - use X - INT(X/Y) * Y if Y > zero.
 *                         else...
 *	Split the real(8) up to get a more accurate answer.
 *
 *	_f_real8 _AINT_(_f_real8 *x);
 *	_f_real8 a = *x;
 *	_f_real8 b = *y;
 *	_f_real8 c = a/b;
 *	return (a - b * ((_f_real8) _AINT_(&c)));
 *
 */
_f_real8
_AMOD_( _f_real8 *x,
	_f_real8 *y)
{
	/* Union defined to work with IEEE 64-bit floating point. */
	union _ieee_double {
		struct {
#if __BYTE_ORDER == __LITTLE_ENDIAN
                    unsigned int mantissa_lo : IEEE_64_MANT_BITS_H2;
                    unsigned int mantissa_up : IEEE_64_MANT_BITS_H1;
                    unsigned int exponent    : IEEE_64_EXPO_BITS;
                    unsigned int sign        : 1;
#else
                    unsigned int sign        : 1;
                    unsigned int exponent    : IEEE_64_EXPO_BITS;
                    unsigned int mantissa_up : IEEE_64_MANT_BITS_H1;
                    unsigned int mantissa_lo : IEEE_64_MANT_BITS_H2;
#endif
		} parts1;
		_f_real8		dword;
		unsigned int		lword[2];
		unsigned long long	llword;
		long long		int64;
	};
#if __BYTE_ORDER == __LITTLE_ENDIAN
        const int lword_hi = 1;
        const int lword_lo = 0;
#else
        const int lword_hi = 0;
        const int lword_lo = 1;
#endif
	_f_real8 _AINT_(_f_real8 *x);
	union _ieee_double a, b, c, two_52, div_52;
	union _ieee_double c_up, c_lo, b_up, b_lo, res;
	_f_real8 scaleb, scalec, divc;

	two_52.lword[lword_hi] = 0x43300000;   /* 2**52        */
	two_52.lword[lword_lo] = 0x00000000;
	div_52.lword[lword_hi] = 0x3CB00000;   /* 2**-52       */
	div_52.lword[lword_lo] = 0x00000000;
	a.dword = *x;
	b.dword = *y;
	divc = a.dword/b.dword;
	c.dword = (_f_real8) _AINT_(&divc);
	c_up.dword = c.dword;
	b_up.dword = b.dword;
	c_up.parts1.mantissa_lo = 0x0;
	b_up.parts1.mantissa_lo = 0x0;
	scaleb = 1.0;
	scalec = 1.0;

	/* If b_up exponent < 27, scale up to prevent underflow. */
	if ((int) b.parts1.exponent < 27) {
		b_lo.dword = (b.dword * two_52.dword) -
			(b_up.dword * two_52.dword);
		scaleb = div_52.dword;
	} else
		b_lo.dword = b.dword - b_up.dword;

	/* If c_up exponent < 27, scale up to prevent underflow. */
	if ((int) c.parts1.exponent < 27) {
		c_lo.dword = (c.dword * two_52.dword) -
			(c.dword * two_52.dword);
		scalec = div_52.dword;
	} else
		c_lo.dword = c.dword - c_up.dword;

	/* algorithm for ieee for a - b*c. */
	res.dword = ((((a.dword - (b_up.dword * c_up.dword)) -
		(scalec * (b_up.dword * c_lo.dword))) -
		(scaleb * (b_lo.dword * c_up.dword))) -
		(scaleb * scalec * (b_lo.dword * c_lo.dword)));

	if (res.dword == 0.0)
		res.parts1.sign = a.parts1.sign;

	return(res.dword);
}
