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


#pragma ident "@(#) libfi/mathlb/ieee_exponent_i.c	92.1	07/09/99 11:00:36"
#include <fmath.h>
#include <fortran.h>
#ifdef	__mips
#include <fp_class.h>
#include <math.h>
#endif
#include "inline.h"
#include "leadz.h"

/* IEEE_EXPONENT(X,Y_I8) returns integer(kind=8) result: */
#ifdef	_CRAYMPP
extern _f_int8 _IEEE_EXPONENT_I8_H(_f_real8 x);
#else	/* _CRAYMPP */
extern _f_int8 _IEEE_EXPONENT_I8_H(_f_real4 x);
#endif	/* _CRAYMPP */
extern _f_int8 _IEEE_EXPONENT_I8_R(_f_real8 x);
#if _F_REAL16 == 1
extern _f_int8 _IEEE_EXPONENT_I8_D(_f_real16 x);
#endif

#ifndef	__mips
#ifdef	_F_REAL4
/* _IEEE_EXPONENT_I8_H - IEEE EXPONENT returns the exponent part of the
 *                      32-bit argument in 64-bit integer.
 */
#ifdef	_CRAYMPP
static int __ieee_real4_i8(_f_real4 x);

_f_int8
_IEEE_EXPONENT_I8_H(_f_real8 x)
{
	int		i;
	REGISTER_8	s1;

	/* if x is a NaN, return HUGE */
	if (isnan64(x))
		return(HUGE_INT8_F90);
	s1.f	= x;

	/* clear sign bit. */
	s1.ui &= ~IEEE_64_SIGN_BIT;

	/* if x is + or -infinity, return HUGE */
	if (s1.ui == IEEE_64_INFINITY)
		return(HUGE_INT8_F90);

	/* if x is zero, return -HUGE */
	if (x == 0.0e0)
		return(-HUGE_INT8_F90);
	i	= __ieee_real4i(x);
	return ((_f_int8) i);
}
#else	/* _CRAYMPP */
_f_int8
_IEEE_EXPONENT_I8_H(_f_real4 x)
{
	int		i;
	REGISTER_4	s1, s2;

	/* if x is a NaN, return HUGE */
	if (isnan32(x))
		return(HUGE_INT8_F90);
	s1.f	= x;

	/* clear sign bit. */
	s1.ui &= ~IEEE_32_SIGN_BIT;

	/* if x is + or -infinity, return HUGE */
	if (s1.ui == IEEE_32_INFINITY)
		return(HUGE_INT8_F90);

	/* if x is zero, return -HUGE */
	if (x == (float) 0.0e0)
		return(-HUGE_INT8_F90);

	/* shift exponent bits to right. */
	s1.i >>= IEEE_32_MANT_BITS;
	if (s1.ui == 0) {

		/* x is a subnormal number (implicit leading bit is zero
		 * and the exponent is zero).  calculate the exponent
		 * based on normalized x.
		 *
		 * get mantissa
		 */
		s2.f	= x;
		s2.ui	= IEEE_32_MANTISSA & s2.ui;

		/* get leading zeros in mantissa part. */
		i	= _leadz4(s2.ui) - IEEE_32_EXPO_BITS;

		/* calculate exponent. */
		s1.i -= (IEEE_32_EXPO_BIAS + i);
	} else {
		/* subtract exponent bias and implicit bit. */
		s1.i -= (IEEE_32_EXPO_BIAS);
	}
	return((_f_int8) s1.i);
}
#endif	/* _CRAYMPP */
#endif	/* _F_REAL4 */


/* _IEEE_EXPONENT_I8_R - IEEE EXPONENT returns the exponent part of the
 *                      64-bit argument in 64-bit integer.
 */
_f_int8
_IEEE_EXPONENT_I8_R(_f_real8 x)
{
	int		i;
	REGISTER_8	s1, s2;

	/* if x is a NaN, return HUGE */
	if (isnan64(x))
		return(HUGE_INT8_F90);
	s1.f	= x;

	/* clear sign bit */
	s1.ui &= ~IEEE_64_SIGN_BIT;

	/* if x is + or -infinity, return HUGE */
	if (s1.ui == IEEE_64_INFINITY)
		return(HUGE_INT8_F90);

	/* if x is zero, return -HUGE */
	if (x == 0.0e0)
		return(-HUGE_INT8_F90);

	/* shift exponent bits to right. */
	s1.ui >>= IEEE_64_MANT_BITS;
	if (s1.ui == 0) {

		/* x is a subnormal number (implicit leading bit is zero
		 * and the exponent is zero).  Calculate the exponent
		 * based on normalized x.
	 	 *
		 * get mantissa
		 */
		s2.f	= x;
		s2.ui	= IEEE_64_MANTISSA & s2.ui;

		/* get leading zeros in mantissa part. */
		i	= _leadz8(s2.ui) - IEEE_64_EXPO_BITS;
		/* calculate exponent. */
		s1.i -= (IEEE_64_EXPO_BIAS + i);
	} else {
		/* subtract exponent bias. */
		s1.i -= (IEEE_64_EXPO_BIAS);
	}
	return(s1.i);
}


#if _F_REAL16 == 1
/* _IEEE_EXPONENT_I8_D - IEEE EXPONENT returns the exponent part of the
 *                       128-bit argument in 64-bit integer.
 */
_f_int8
_IEEE_EXPONENT_I8_D(_f_real16 x)
{
	int		i, ileadzcnt, loopn;
#if defined(_WORD32)
	union ldble_float {
		_f_real16		whole;
		unsigned long long	ui[2];
		long long		si[2];
	} f, result;
#else	/* _WORD32 */
	union ldble_float {
		_f_real16		whole;
		unsigned long		ui[2];
		long			si[2];
	} f, result;
#endif	/* _WORD32 */

	static int word_size =	64;

	/* if x is a NaN, return HUGE */
	if (isnan128(x))
		return(HUGE_INT8_F90);

	f.whole	= x;

	/* Get the absolute value of x by ANDing the upper half
	 * with the NOT of 0x8000000000000000 (the sign bit mask).
	 */
	f.ui[0] &= ~IEEE_128_64_SIGN_BIT;

	/* if x is + or -infinity, return HUGE */
	if ((f.ui[0] == IEEE_128_64_EXPO) && (f.ui[1] == 0))
		return(HUGE_INT8_F90);

	/* if x is zero, return -HUGE */
	if (x == 0.0e0)
		return(-HUGE_INT8_F90);

	/* Separate the exponent from the 128-bit float value and
	 * right justify it.
	 */
	result.ui[0]	= f.ui[0] >> (IEEE_128_MANT_BITS - word_size);
	if (result.ui[0] == 0) {

		/* x is a subnormal number (implicit leading bit is zero
		 * and the exponent is zero).  Calculate the exponent
		 * based on normalized x.
		 *
		 * get mantissa
		 */
		f.ui[0] &= IEEE_128_64_MANT1;
		i	= 0;

		/* get leading zeros in mantissa part */
		for (loopn = 0; loopn < 2; loopn++) {
			ileadzcnt	= _leadz8(f.ui[loopn]);
			i += ileadzcnt;
			if (ileadzcnt < word_size)
				break;
		}
		i	= i - IEEE_128_EXPO_BITS;

		/* calculate exponent. */
		result.si[0] -= (IEEE_128_EXPO_BIAS + i);
	} else {
		/* subtract exponent bias. */
		result.si[0] -= (IEEE_128_EXPO_BIAS);
	}
	return((_f_int8) result.si[0]);
}
#endif	/* _FREAL16 equal 1 */
#else	/* NOT __mips */
/* _IEEE_EXPONENT_I8_H - IEEE EXPONENT returns the exponent part of the
 *                      32-bit argument in 64-bit integer.
 */
_f_int8
_IEEE_EXPONENT_I8_H(_f_real4 x)
{
	union _ieee_fdouble {
		_f_real4	dword;
		_f_int4		lword;
		struct {
#if  __BYTE_ORDER == __LITTLE_ENDIAN
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
	_f_int8	iresult	= 0;

	switch(fp_class_f(x)) {
		case FP_SNAN:
		case FP_QNAN:
		case FP_POS_INF:
		case FP_NEG_INF:
			{
			/* return positive huge for NaN or infinity. */
			return(HUGE_INT8_F90);
			}
		case FP_POS_NORM:
		case FP_NEG_NORM:
			{
			union _ieee_fdouble x_val;
			x_val.dword	= x;
			return((_f_int8)(x_val.parts.exponent -
				IEEE_32_EXPO_BIAS));
			}
		case FP_POS_DENORM:
		case FP_NEG_DENORM:
			{
			union _ieee_fdouble x_val;
			x_val.dword	= x;

			/* _leadz returns number of zeros before first 1
			 * in mantissa.  Add 8 to exclude exponent bits,
			 * but count sign bit since implicit bit needs to
			 * be counted.
			 */
			return((_f_int8)(-IEEE_32_EXPO_BIAS -
				(_leadz4(x_val.parts.mantissa) +
				IEEE_32_EXPO_BITS)));
			}
		case FP_POS_ZERO:
		case FP_NEG_ZERO:
			{
			/* return negative huge for zero. */
			return(-HUGE_INT8_F90);
			}
		}
	return(iresult);
}

/* _IEEE_EXPONENT_I8_R - IEEE EXPONENT returns the exponent part of the
 *                      64-bit argument in 64-bit integer.
 */
_f_int8
_IEEE_EXPONENT_I8_R(_f_real8 x)
{
	union _ieee_double {
		_f_real8		dword;
		_f_int8			lword;
		unsigned long long	ull;
		struct {
#if  __BYTE_ORDER == __LITTLE_ENDIAN
			unsigned int mantissa2	: IEEE_64_MANT_BTS2;
			unsigned int mantissa1	: IEEE_64_MANT_BTS1;
			unsigned int exponent	: IEEE_64_EXPO_BITS;
			unsigned int sign	: 1;
#else
			unsigned int sign	: 1;
			unsigned int exponent	: IEEE_64_EXPO_BITS;
			unsigned int mantissa1	: IEEE_64_MANT_BTS1;
			unsigned int mantissa2	: IEEE_64_MANT_BTS2;
#endif
		} parts;
	};
	_f_int8	iresult	= 0;

	switch(fp_class_d(x)) {
		case FP_SNAN:
		case FP_QNAN:
		case FP_POS_INF:
		case FP_NEG_INF:
			{
			/* return positive huge for NaN or infinity. */
			return(HUGE_INT8_F90);
			}
		case FP_POS_NORM:
		case FP_NEG_NORM:
			{
			union _ieee_double x_val;
			x_val.dword	= x;
			return((_f_int8)(x_val.parts.exponent -
				IEEE_64_EXPO_BIAS));
			}
		case FP_POS_DENORM:
		case FP_NEG_DENORM:
			{
			union _ieee_double x_val;
			x_val.dword	= x;
			x_val.ull	= IEEE_64_MANTISSA & x_val.ull;

			/* _leadz returns number of zeros before first 1
			 * in mantissa.  Add 8 to exclude exponent bits,
			 * but count sign bit since implicit bit needs to
			 * be counted.
			 */
			return((_f_int8)(-IEEE_64_EXPO_BIAS -
				(_leadz8(x_val.ull) +
				IEEE_64_EXPO_BITS)));
			}
		case FP_POS_ZERO:
		case FP_NEG_ZERO:
			{
			/* return negative huge for zero. */
			return(-HUGE_INT8_F90);
			}
		}
	return(iresult);
}

/* _IEEE_EXPONENT_I8_D - IEEE EXPONENT returns the exponent part of the
 *                      128-bit argument in 64-bit integer.
 */
_f_int8
_IEEE_EXPONENT_I8_D(_f_real16 x)
{
	union _ieee_ldouble {
		_f_real16	ldword;
		_f_real8	dbword[2];
	};
#if __BYTE_ORDER == __LITTLE_ENDIAN
        const int dbword_hi = 1;
        const int dbword_lo = 0;
#else
        const int dbword_hi = 0;
        const int dbword_lo = 1;
#endif
	union _ieee_double {
		_f_real8		dword;
		_f_int8			lword;
		unsigned long long	ull;
		struct {
#if  __BYTE_ORDER == __LITTLE_ENDIAN
			unsigned int mantissa2	: IEEE_64_MANT_BTS2;
			unsigned int mantissa1	: IEEE_64_MANT_BTS1;
			unsigned int exponent	: IEEE_64_EXPO_BITS;
			unsigned int sign	: 1;
#else
			unsigned int sign	: 1;
			unsigned int exponent	: IEEE_64_EXPO_BITS;
			unsigned int mantissa1	: IEEE_64_MANT_BTS1;
			unsigned int mantissa2	: IEEE_64_MANT_BTS2;
#endif
		} parts;
	};
	_f_int8	iresult	= 0;

	switch(fp_class_l(x)) {
		case FP_SNAN:
		case FP_QNAN:
		case FP_POS_INF:
		case FP_NEG_INF:
			{
			/* return positive huge for NaN or infinity. */
			return(HUGE_INT8_F90);
			}
		case FP_POS_NORM:
		case FP_NEG_NORM:
			{
#pragma weak	logbl
			return((_f_int8) logbl(x));
			}
		case FP_POS_DENORM:
		case FP_NEG_DENORM:
			{
			/* return exponent from first 64-bit double. */
			union _ieee_ldouble x_val;
			x_val.ldword	= x;
			switch(fp_class_d(x_val.dbword[dbword_hi])) {
				case FP_POS_NORM:
				case FP_NEG_NORM:
					{
					union _ieee_double db_x;
					db_x.dword	= x_val.dbword[dbword_hi];
					return((_f_int8)(db_x.parts.exponent -
					  IEEE_64_EXPO_BIAS));
					}
				case FP_POS_DENORM:
				case FP_NEG_DENORM:
					{
					union _ieee_double db_x;
					db_x.dword	= x_val.dbword[dbword_hi];
					db_x.ull	=
					  IEEE_64_MANTISSA & db_x.ull;
					return((_f_int8)(-IEEE_64_EXPO_BIAS -
					  (_leadz8(db_x.ull) +
					  IEEE_64_EXPO_BITS)));
					}
				}
			}
		case FP_POS_ZERO:
		case FP_NEG_ZERO:
			{
			/* return negative huge for zero. */
			return(-HUGE_INT8_F90);
			}
		}
	return(iresult);
}
#endif	/* NOT __mips */
