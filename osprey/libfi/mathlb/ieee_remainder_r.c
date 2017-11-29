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


#pragma ident "@(#) libfi/mathlb/ieee_remainder_r.c	92.1	07/09/99 11:00:36"

#include <fortran.h>
#include <fp.h>
#include "inline.h"

#define IEEE_64_MANT_BITS_H1	26
#define IEEE_64_MANT_BITS_H2	27

extern _f_real8 _IEEE_REMAINDER_H_R(_f_real4 x, _f_real8 y);
extern _f_real8 _IEEE_REMAINDER_R_H(_f_real8 x, _f_real4 y);
extern _f_real8 _IEEE_REMAINDER_R_R(_f_real8 x, _f_real8 y);

static _f_real8 _raisinvld(_f_real8 x, _f_real8 y);

static _f_real8 _raisinvld(_f_real8 x, _f_real8 y)
{
        return (x/y);
}

/*
 * _IEEE_REMAINDER_R_R(X,Y) - calculate remainder of two real(8)
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

_f_real8
_IEEE_REMAINDER_R_R(_f_real8 argx, _f_real8 argy)
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

	_f_real8	scalet, scaley;
	unsigned int	sign_x = 0X80000000;
	unsigned int	even_x = 0X00000001;
	union _ieee_double x_val, y_val, tdiv, two_52, evenchk, div_52;
	union _ieee_double nearint, y_up, y_lo, tdiv_up, tdiv_lo, res;
	x_val.dword	= argx;
	y_val.dword	= argy;
	two_52.lword[lword_hi] = 0x43300000;	/* 2**52	*/
	two_52.lword[lword_lo] = 0x00000000;
	div_52.lword[lword_hi] = 0x3CB00000;	/* 2**-52	*/
	div_52.lword[lword_lo] = 0x00000000;

	/* check input values: x for infinity and y for zero. */
	if (((x_val.llword & ~IEEE_64_SIGN_BIT) == IEEE_64_INFINITY) ||
	   ((!(isnan64(y_val.dword))) && (y_val.dword == 0.0))) {
		union _ieee_double x_val;
		_f_real8	result8;
		_f_real8	arg8 = 0.0;

		x_val.llword	= _SGL_NaN;

		/* need to emit invalid exception */
		result8		= _raisinvld(arg8, arg8);
		return(x_val.dword);
	}
	tdiv.dword	= argx / argy;

	/* check for 2**52 or greater = already integer */
	if ((int) (tdiv.lword[lword_hi] & (~sign_x)) < (int) two_52.lword[lword_hi]) {

		/* calculate fraction */
		evenchk.dword =
			tdiv.dword - (_f_real8)((_f_int8)tdiv.dword);

		if (tdiv.dword < 0.0) {
			nearint.int64 = (_f_int8) (tdiv.dword - 0.5);
			if ((evenchk.dword == -0.5) &&
			    ((nearint.lword[lword_lo] & even_x) != 0))
				nearint.int64 += 1;
		} else {
			nearint.int64 = (_f_int8) (tdiv.dword + 0.5);
			if ((evenchk.dword == 0.5) &&
			   ((nearint.lword[lword_lo] & even_x) != 0))
				nearint.int64 -= 1;
		}
		tdiv.dword = (_f_real8) nearint.int64;
	}

	/* Calculate upper and lower for y and tdiv. */
	y_up.dword = y_val.dword;
	tdiv_up.dword = tdiv.dword;
	y_up.parts1.mantissa_lo = 0x0;
	tdiv_up.parts1.mantissa_lo = 0x0;
	scalet = 1.0;
	scaley = 1.0;

	/* If tdiv_up exponent < 27, scale up to prevent underflow. */
	if ((int) tdiv.parts1.exponent < 27) {
		tdiv_lo.dword = (tdiv.dword * two_52.dword) -
			(tdiv_up.dword * two_52.dword);
		scalet = div_52.dword;
	} else
		tdiv_lo.dword = tdiv.dword - tdiv_up.dword;

	/* If y_up exponent < 27, scale up to prevent underflow. */
	if ((int) y_val.parts1.exponent < 27) {
		y_lo.dword = (y_val.dword * two_52.dword) -
			(y_val.dword * two_52.dword);
		scaley = div_52.dword;
	} else
		y_lo.dword = y_val.dword - y_up.dword;

	/* algorithm for ieee for x - (x/y)*y. */
	res.dword = ((((x_val.dword - (tdiv_up.dword * y_up.dword)) -
			(scaley * (tdiv_up.dword * y_lo.dword))) -
			(scalet * (tdiv_lo.dword * y_up.dword))) -
		(scalet * scaley * (tdiv_lo.dword * y_lo.dword)));
	if (res.dword == 0.0)
		res.parts1.sign = x_val.parts1.sign;
	return(res.dword);
}

_f_real8
_IEEE_REMAINDER_R_H(_f_real8 argx, _f_real4 argy)
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

	/* Union defined to work with IEEE 32-bit floating point. */
	union _ieee_fdouble {
		_f_real4	fpword;
		unsigned int	l4word;
		int		int32;
	};

	_f_real8	scalet, scaley;
	unsigned int	sign_x = 0X80000000;
	unsigned int	even_x = 0X00000001;
	union _ieee_double x_val, y_val, tdiv, two_52, evenchk, div_52;
	union _ieee_double nearint, y_up, y_lo, tdiv_up, tdiv_lo, res;
	union _ieee_fdouble y4_val;
	x_val.dword	= argx;
	y4_val.fpword	= argy;
	two_52.lword[lword_hi] = 0x43300000;	/* 2**52	*/
	two_52.lword[lword_lo] = 0x00000000;
	div_52.lword[lword_hi] = 0x3CB00000;	/* 2**-52	*/
	div_52.lword[lword_lo] = 0x00000000;

	/* check input values: x for infinity and y for zero. */
	if (((x_val.llword & ~IEEE_64_SIGN_BIT) == IEEE_64_INFINITY) ||
	   ((!(isnan32(y4_val.fpword))) && (y4_val.fpword == 0.0))) {
		union _ieee_double x_val;
		_f_real8	result8;
		_f_real8	arg8 = 0.0;

		x_val.llword	= _SGL_NaN;

		/* need to emit invalid exception */
		result8		= _raisinvld(arg8, arg8);
		return(x_val.dword);
	}
	y_val.dword	= y4_val.fpword;
	tdiv.dword	= argx / y_val.dword;

	/* check for 2**52 or greater = already integer */
	if ((tdiv.lword[lword_hi] & (~sign_x)) < two_52.lword[lword_hi]) {

		/* calculate fraction */
		evenchk.dword =
			tdiv.dword - (_f_real8)((_f_int8)tdiv.dword);

		if (tdiv.dword < 0.0) {
			nearint.int64 = (_f_int8) (tdiv.dword - 0.5);
			if ((evenchk.dword == -0.5) &&
			    ((nearint.lword[lword_lo] & even_x) != 0))
				nearint.int64 += 1;
		} else {
			nearint.int64 = (_f_int8) (tdiv.dword + 0.5);
			if ((evenchk.dword == 0.5) &&
			   ((nearint.lword[lword_lo] & even_x) != 0))
				nearint.int64 -= 1;
		}
		tdiv.dword = (_f_real8) nearint.int64;
	}

	/* Calculate upper and lower for y and tdiv. */
	y_up.dword = y_val.dword;
	tdiv_up.dword = tdiv.dword;
	y_up.parts1.mantissa_lo = 0x0;
	tdiv_up.parts1.mantissa_lo = 0x0;
	scalet = 1.0;
	scaley = 1.0;

	/* If tdiv_up exponent < 27, scale up to prevent underflow. */
	if ((int) tdiv.parts1.exponent < 27) {
		tdiv_lo.dword = (tdiv.dword * two_52.dword) -
			(tdiv_up.dword * two_52.dword);
		scalet = div_52.dword;
	} else
		tdiv_lo.dword = tdiv.dword - tdiv_up.dword;

	/* If y_up exponent < 27, scale up to prevent underflow. */
	if ((int) y_val.parts1.exponent < 27) {
		y_lo.dword = (y_val.dword * two_52.dword) -
			(y_val.dword * two_52.dword);
		scaley = div_52.dword;
	} else
		y_lo.dword = y_val.dword - y_up.dword;

	/* algorithm for ieee for x - (x/y)*y. */
	res.dword = ((((x_val.dword - (tdiv_up.dword * y_up.dword)) -
			(scaley * (tdiv_up.dword * y_lo.dword))) -
			(scalet * (tdiv_lo.dword * y_up.dword))) -
		(scalet * scaley * (tdiv_lo.dword * y_lo.dword)));
	if (res.dword == 0.0)
		res.parts1.sign = x_val.parts1.sign;
	return(res.dword);
}

_f_real8
_IEEE_REMAINDER_H_R(_f_real4 argx, _f_real8 argy)
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

	/* Union defined to work with IEEE 32-bit floating point. */
	union _ieee_fdouble {
		_f_real4	fpword;
		unsigned int	l4word;
		int		int32;
	};

	_f_real8	scalet, scaley;
	unsigned int	sign_x = 0X80000000;
	unsigned int	even_x = 0X00000001;
	union _ieee_double x_val, y_val, tdiv, two_52, evenchk, div_52;
	union _ieee_double nearint, y_up, y_lo, tdiv_up, tdiv_lo, res;
	union _ieee_fdouble x4_val;
	x4_val.fpword	= argx;
	y_val.dword	= argy;
	two_52.lword[lword_hi] = 0x43300000;	/* 2**52	*/
	two_52.lword[lword_lo] = 0x00000000;
	div_52.lword[lword_hi] = 0x3CB00000;	/* 2**-52	*/
	div_52.lword[lword_lo] = 0x00000000;

	/* check input values: x for infinity and y for zero. */
	if (((x4_val.l4word & ~IEEE_32_SIGN_BIT) == IEEE_32_INFINITY) ||
	   ((!(isnan64(y_val.dword))) && (y_val.dword == 0.0))) {
		union _ieee_double x_val;
		_f_real8	result8;
		_f_real8	arg8 = 0.0;

		x_val.llword	= _SGL_NaN;

		/* need to emit invalid exception */
		result8		= _raisinvld(arg8, arg8);
		return(x_val.dword);
	}
	x_val.dword	= x4_val.fpword;
	tdiv.dword	= x_val.dword / argy;

	/* check for 2**52 or greater = already integer */
	if ((tdiv.lword[lword_hi] & (~sign_x)) < two_52.lword[lword_hi]) {

		/* calculate fraction */
		evenchk.dword =
			tdiv.dword - (_f_real8)((_f_int8)tdiv.dword);

		if (tdiv.dword < 0.0) {
			nearint.int64 = (_f_int8) (tdiv.dword - 0.5);
			if ((evenchk.dword == -0.5) &&
			    ((nearint.lword[lword_lo] & even_x) != 0))
				nearint.int64 += 1;
		} else {
			nearint.int64 = (_f_int8) (tdiv.dword + 0.5);
			if ((evenchk.dword == 0.5) &&
			   ((nearint.lword[lword_lo] & even_x) != 0))
				nearint.int64 -= 1;
		}
		tdiv.dword = (_f_real8) nearint.int64;
	}

	/* Calculate upper and lower for y and tdiv. */
	y_up.dword = y_val.dword;
	tdiv_up.dword = tdiv.dword;
	y_up.parts1.mantissa_lo = 0x0;
	tdiv_up.parts1.mantissa_lo = 0x0;
	scalet = 1.0;
	scaley = 1.0;

	/* If tdiv_up exponent < 27, scale up to prevent underflow. */
	if ((int) tdiv.parts1.exponent < 27) {
		tdiv_lo.dword = (tdiv.dword * two_52.dword) -
			(tdiv_up.dword * two_52.dword);
		scalet = div_52.dword;
	} else
		tdiv_lo.dword = tdiv.dword - tdiv_up.dword;

	/* If y_up exponent < 27, scale up to prevent underflow. */
	if ((int) y_val.parts1.exponent < 27) {
		y_lo.dword = (y_val.dword * two_52.dword) -
			(y_val.dword * two_52.dword);
		scaley = div_52.dword;
	} else
		y_lo.dword = y_val.dword - y_up.dword;

	/* algorithm for ieee for x - (x/y)*y. */
	res.dword = ((((x_val.dword - (tdiv_up.dword * y_up.dword)) -
			(scaley * (tdiv_up.dword * y_lo.dword))) -
			(scalet * (tdiv_lo.dword * y_up.dword))) -
		(scalet * scaley * (tdiv_lo.dword * y_lo.dword)));
	if (res.dword == 0.0)
		res.parts1.sign = x_val.parts1.sign;
	return(res.dword);
}
