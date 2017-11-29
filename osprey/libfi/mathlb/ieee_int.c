/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


#pragma ident "@(#) libfi/mathlb/ieee_int.c	92.1	07/09/99 11:00:36"

#include <fmath.h>
#include <fortran.h>
#include <fp.h>
#include <liberrno.h>
#include "inline.h"
#include <math.h>
#include <fenv.h>

#ifdef __mips
_f_int8 _d_to_int(_f_real16 argx, _f_int8  huge);
#endif

extern _f_real16 _IEEE_INT_D_D(_f_real16 x);
extern _f_real4 _IEEE_INT_D_H(_f_real16 x);
extern _f_real8 _IEEE_INT_D_R(_f_real16 x);
extern _f_real16 _IEEE_INT_H_D(_f_real4 x);
extern _f_real16 _IEEE_INT_R_D(_f_real8 x);

extern _f_int4 _IEEE_INT_D_I4(_f_real16 x);
extern _f_int8 _IEEE_INT_D_I8(_f_real16 x);

#ifndef	__mips
#define CALC_DINT()					\
	if (x_val.ldword < 0.0) {			\
		tmp.ldword = x_val.ldword - two_112.ldword;	\
		tmp.ldword += two_112.ldword;			\
	} else {					\
		tmp.ldword = x_val.ldword + two_112.ldword;	\
		tmp.ldword -= two_112.ldword;			\
	}

#define CALC_DNINT()					\
	evenchk.ldword = x_val.ldword -			\
	   (_f_real16)((_f_int8) x_val.ldword);		\
	if (x_val.ldword < 0.0) {			\
		nearint.lsword[1] = (_f_int8) (x_val.ldword - 0.5); \
		if ((evenchk.ldword == -0.5) &&		\
		    ((nearint.lword[3] & even_x) != 0))	\
			nearint.lsword[1] += 1;		\
	} else {					\
		nearint.lsword[1] = (_f_int8) (x_val.ldword + 0.5); \
		if ((evenchk.ldword == 0.5) &&		\
		    ((nearint.lword[3] & even_x) != 0))	\
			nearint.lsword[1] -= 1;		\
	}

#else
extern _f_int2 _IEEE_INT_D_I2(_f_real16 x);
extern _f_int1 _IEEE_INT_D_I1(_f_real16 x);
#endif

#define TWO_106 81129638414606681695789005144064.
#define TWO_53 9007199254740992.
#define TWO_52 4503599627370496.
#define TWO_24 16777216.
#define TWO_23 8388608.

static _f_real8 _raisinvld(_f_real8 x, _f_real8 y);

static _f_real8 _raisinvld(_f_real8 x, _f_real8 y)
{
        return (x/y);
}

/*
 * _IEEE_INT(X[,Y]) - Find the nearest integer of a floating point
 *                    number.  The value returned depends on the
 *                    presence or absence of the second argument.
 *                    The routine rounds the result according to the
 *                    IEEE_ROUNDING mode bits.
 *
 *	INPUT:	one real argument
 *
 *	OUTPUT:	Rounded real result of same type as second
 *		argument when second argument is real.
 * 		-	RETURN signed zero if X is 0.0 or denormal
 * 		-	RETURN quiet NaN if X is NaN
 * 		-	RETURN signed inf if X is signed inf
 *			Raise Inexact if NaN, Inf, overflow.
 *	or	Rounded integer result of same type as second
 *		argument when second argument is integer.
 *			Raise Inexact if NaN, Inf, overflow.
 *	or	Rounded integer result of default type integer 
 *		when second argument is absent.
 *			Raise Inexact if NaN, Inf, overflow.
 *
 */

_f_int4
_IEEE_INT_D_I4(_f_real16 argx)
#ifndef	__mips
{
#ifdef KEY /* Bug 6236 */
	/* The non-MIPS code seems very broken on the X86, where the back
	 * end gives us a "long double" argx which is currently in 80 bit
	 * little endian format */
	return (_f_int4) rintl(argx);
#else /* KEY Bug 6236 */
	/* Union defined to work with IEEE 128-bit floating point. */
	union _ieee_ldouble {
		struct {
                        unsigned int sign	: 1;
			unsigned int exponent	: IEEE_128_EXPO_BITS;
			unsigned int mantissa_up1 : IEEE_128_MANT_BTS_UP1;
			unsigned int mantissa_up2 : IEEE_128_MANT_BTS_UP2;
			unsigned int mantissa_lo1 : IEEE_128_MANT_BTS_LO1;
			unsigned int mantissa_lo2 : IEEE_128_MANT_BTS_LO2;
		} parts1;
		_f_real16		ldword;
		unsigned long long	llword[2];
		unsigned int		lword[4];
		long long		lsword[2];
	};
	static union _ieee_ldouble two_112 =
	   {0, 0x406F, 0x0000, 0x00000000, 0x00000000, 0x00000000};
	static union _ieee_ldouble div_112 =
	   {0, 0x3F8F, 0x0000, 0x00000000, 0x00000000, 0x00000000};
	unsigned int	sign_x = 0X80000000;
	unsigned int	even_x = 0X00000001;
	unsigned int	two_32 = 0X401F0000;
	_f_int4		result = 0;
	int	rounding;
	union _ieee_ldouble	x_val, evenchk, nearint, tmp;
	x_val.ldword	= argx;
	/* check x for NaN, infinity, denormal, or zero. */
	if (!(isnormal128(argx))) {

		/* check for kinds of denormal or zero. */
		if (isnan128(argx) ||
		   ((((x_val.llword[0] & ~IEEE_128_64_SIGN_BIT) ==
		    IEEE_128_64_EXPO) && x_val.llword[1] == 0))) {
			_f_real8	result8;
			_f_real8	arg8 = 0.0;

			/* Invoke invalid operation for Nan or Infinity.
			 * No error if invalid operation exception is not
			 * trapped. Return signed HUGE (largest integer)
			 * for signed infinity and zero for NaN.
			 * _lerror (_LELVL_ABORT, FEIEINTE);
			 */
			if (!isnan128(argx)) {
				result = (x_val.parts1.sign == 0) ?
					HUGE_INT4_F90 :	-HUGE_INT4_F90;
			}
			result8	= _raisinvld(arg8, arg8);
			return(result);
		} else if (argx == 0.0) {
			/* x is zero. */
			return(result);
		}
	} else if ((int) two_32 <= (int) (x_val.lword[0] & (~sign_x))) {
		_f_real8	result8;
		_f_real8	arg8 = 0.0;

		/* Invoke invalid operation for too large a float value.
		 * No error if invalid operation exception is not
		 * trapped. Return signed HUGE.(largest integer)
		 * _lerror (_LELVL_ABORT, FEIEINTL);
		 */
		result = (x_val.parts1.sign == 0) ?
			HUGE_INT4_F90 :	-HUGE_INT4_F90;
		result8	= _raisinvld(arg8, arg8);
		return(result);
	}

	rounding	= fegetround();
	if (rounding == FE_TONEAREST) {
		/* round toward nearest */
		CALC_DNINT();
		result = (_f_int4) nearint.lsword[1];
	} else {
		/*
		 * RP_RMZ = round toward zero=truncate.
		 * FE_UPWARD = round toward plus infinity=round up.
		 * FE_DOWNWARD = round toward minus infinity=round down.
		 */
		CALC_DINT();
		result = (_f_int4) tmp.ldword;
	}
	return(result);
#endif /* KEY Bug 6236 */
}
#else
{
	return((_f_int4) _d_to_int(argx, (_f_int8) HUGE_INT4_F90));
}
#endif

_f_int8
_IEEE_INT_D_I8(_f_real16 argx)
#ifndef	__mips
{
	/* Union defined to work with IEEE 128-bit floating point. */
	union _ieee_ldouble {
		struct {
                        unsigned int sign	: 1;
			unsigned int exponent	: IEEE_128_EXPO_BITS;
			unsigned int mantissa_up1 : IEEE_128_MANT_BTS_UP1;
			unsigned int mantissa_up2 : IEEE_128_MANT_BTS_UP2;
			unsigned int mantissa_lo1 : IEEE_128_MANT_BTS_LO1;
			unsigned int mantissa_lo2 : IEEE_128_MANT_BTS_LO2;
		} parts1;
		_f_real16		ldword;
		unsigned long long	llword[2];
		unsigned int		lword[4];
		long long		lsword[2];
	};

	static union _ieee_ldouble two_112 =
	   {0, 0x406F, 0x0000, 0x00000000, 0x00000000, 0x00000000};
	static union _ieee_ldouble div_112 =
	   {0, 0x3F8F, 0x0000, 0x00000000, 0x00000000, 0x00000000};
	unsigned int	sign_x = 0X80000000;
	unsigned int	even_x = 0X00000001;
	unsigned int	two_64 = 0X403F0000;
	_f_int8		result = 0;
	int	rounding;
	union _ieee_ldouble	x_val, evenchk, nearint, tmp;

	x_val.ldword	= argx;

	/* check x for infinity, NaN, zero, and denormal. */
	if (!isnormal128(x_val.ldword)) {

		/* check for kinds of denormal or zero. */
		if ((((x_val.llword[0] & ~IEEE_128_64_SIGN_BIT) ==
		    IEEE_128_64_EXPO) && x_val.llword[1] == 0) ||
		    isnan128(x_val.ldword)) {
			_f_real8	result8;
			_f_real8	arg8 = 0.0;

			/* Invoke invalid operation for Nan or Infinity.
			 * No error if invalid operation exception is not
			 * trapped. Return signed HUGE (largest integer)
			 * for signed infinity and zero for NaN.
			 * _lerror (_LELVL_ABORT, FEIEINTE);
			 */
			if (!isnan128(argx)) {
				result = (x_val.parts1.sign == 0) ?
					HUGE_INT8_F90 :	-HUGE_INT8_F90;
			}
			result8	= _raisinvld(arg8, arg8);
			return(result);

		} else if (x_val.ldword == 0.0) {
			/* x is zero. */
			return(result);
		}
	} else if ((int) two_64 <= (int) (x_val.lword[0] & (~sign_x))) {
		/* put out error of integer too large. */
		_f_real8	result8;
		_f_real8	arg8 = 0.0;

		/* Invoke invalid operation for too large a float value.
		 * No error if invalid operation exception is not
		 * trapped. Return signed HUGE.(largest integer)
		 * _lerror (_LELVL_ABORT, FEIEINTL);
		 */
		result = (x_val.parts1.sign == 0) ?
			HUGE_INT8_F90 :	-HUGE_INT8_F90;
		result8	= _raisinvld(arg8, arg8);
		return(result);
	}

	rounding	= fegetround();
	if (rounding == FE_TONEAREST) {
		/* round toward nearest */
		CALC_DNINT();
		result = (_f_int8) nearint.lsword[1];
	} else {
		/*
		 * RP_RMZ = round toward zero=truncate.
		 * FE_UPWARD = round toward plus infinity=round up.
		 * FE_DOWNWARD = round toward minus infinity=round down.
		 */
		CALC_DINT();
		result = (_f_int8) tmp.ldword;
	}
	return(result);
}
#else
{
	return((_f_int8) _d_to_int(argx, (_f_int8) HUGE_INT8_F90));
}
#endif

#ifdef	__mips
_f_int2
_IEEE_INT_D_I2(_f_real16 argx)
{
	return((_f_int2) _d_to_int(argx, (_f_int8) HUGE_INT2_F90));
}

_f_int1
_IEEE_INT_D_I1(_f_real16 argx)
{
	return((_f_int1) _d_to_int(argx, (_f_int8) HUGE_INT1_F90));
}
#endif

_f_real16
_IEEE_INT_D_D(_f_real16 argx)
#ifndef	__mips
{
	/* Union defined to work with IEEE 128-bit floating point. */
	union _ieee_ldouble {
		struct {
                        unsigned int sign	: 1;
			unsigned int exponent	: IEEE_128_EXPO_BITS;
			unsigned int mantissa_up1 : IEEE_128_MANT_BTS_UP1;
			unsigned int mantissa_up2 : IEEE_128_MANT_BTS_UP2;
			unsigned int mantissa_lo1 : IEEE_128_MANT_BTS_LO1;
			unsigned int mantissa_lo2 : IEEE_128_MANT_BTS_LO2;
		} parts1;
		_f_real16		ldword;
		unsigned long long	llword[2];
		unsigned int		lword[4];
		long long		lsword[2];
	};

	static union _ieee_ldouble two_112 =
	   {0, 0x406F, 0x0000, 0x00000000, 0x00000000, 0x00000000};
	static union _ieee_ldouble div_112 =
	   {0, 0x3F8F, 0x0000, 0x00000000, 0x00000000, 0x00000000};
	unsigned int	sign_x = 0X80000000;
	unsigned int	even_x = 0X00000001;
	int	rounding;
	union _ieee_ldouble x_val, evenchk, nearint, tmp, abs, result;
	x_val.ldword	= argx;
	result.ldword	= 0.0;
	abs.ldword	= argx;
	abs.lword[0] &= ~sign_x;

	/* check x for infinity, NaN, zero, and denormal. */
	if (!isnormal128(x_val.ldword)) {

		if ((isnan128(x_val.ldword)) ||
		   (((x_val.llword[0] & ~IEEE_128_64_SIGN_BIT) ==
		    IEEE_128_64_EXPO) && x_val.llword[1] == 0)) {
			/* x is NaN or Infinity. */
			return(x_val.ldword);
		} else if (x_val.ldword == 0.0) {
			/* x is zero. */
			return(x_val.ldword);
		} else {
			/* x is denormal. */
			if (x_val.parts1.sign != 0)
				result.parts1.sign = 1;
			return(result.ldword);
		}
	} else if (two_112.ldword <= abs.ldword) {
		/* value is already an integer. */
		return(x_val.ldword);
	}

	rounding	= fegetround();
	if (rounding == FE_TONEAREST) {
		/* round toward nearest */
		CALC_DNINT();
		result.ldword = (_f_real16) nearint.lsword[1];
	} else {
		/*
		 * RP_RMZ = round toward zero=truncate
		 * FE_UPWARD = round toward plus infinity=round up
		 * FE_DOWNWARD = round toward minus infinity=round down
		 */
		CALC_DINT();
		result.ldword = tmp.ldword;
	}
	return(result.ldword);
}
#else	/* NOT __mips */
{
	/* Union defined to work with MIPS 128-bit floating point. */
	union _ieee_ldouble {
		_f_real16		ldword;
		_f_real8		dword[2];
		unsigned long long	llword[2];
		unsigned int		lword[4];
		long long		lsword[2];
	};
	unsigned int	sign_x = 0X80000000;
	int	rounding;
	union _ieee_ldouble x_val, tmp, abs, result;
	_f_real16	da;
	_f_real16 _add_q(_f_real16 x, _f_real16 y);
	x_val.ldword	= argx;
	result.ldword	= 0.0;
	abs.ldword	= argx;
	abs.lword[0] &= ~sign_x;

	/* get rounding mode. */
	rounding	= fegetround();

	/* check x for infinity, NaN, zero, and denormal. */
	if (!isnormal128(x_val.ldword)) {

		/* return input value if NaN, Infinity or zero. */
		if (isnan64(x_val.dword[0]) ||
		     isnan64(x_val.dword[1]) ||
		     abs.llword[0] == IEEE_64_INFINITY ||
		     abs.llword[1] == IEEE_64_INFINITY)
			/* raise possible exception on NaN */
			return (_f_real16) (x_val.dword[0]+x_val.dword[1]);
		else if (x_val.ldword == 0.0)
			return(x_val.ldword);
		else if ((x_val.dword[0] != 0.0) && (x_val.dword[1] == 0.0)) 
			return(x_val.ldword);
		else {
			/* x is denormal.
			 *
			 * On MIPS, if either word denormal
			 *   if round down mode FE_DOWNWARD
			 *     if first word negative,
			 *        return -1.0d0.
			 *   else if round up mode FE_UPWARD
			 *     if first word negative,
			 *        return -1.0d0.
			 *   else Return -0.0
			 */
			if (rounding == FE_TONEAREST) {
				if (sign_x & x_val.lword[0] != 0)
					return -1.0L;
			} else if (rounding == FE_UPWARD) {
				if (sign_x & x_val.lword[0] != 0)
					return -1.0L;
			}
			return -result.ldword;
		}
	} else if (TWO_106  <= abs.ldword) {
		/* value is already an integral, return it.
		 * This assumes value was rounded to 107 bits.
		 */
		return(x_val.ldword);
	}

	/* MIPS HW/SW 128 bit may not respond to mode.
	 * Assume round to nearest works when set.
	 * RP_RMN = round toward nearest
	 * RP_RMZ = round toward zero=truncate
	 * FE_UPWARD = round toward plus infinity=round up
	 * FE_DOWNWARD = round toward minus infinity=round down
	 * Note: abs(value) >0.0, due to prior test
	 */
	if (rounding == FE_TONEAREST) {
		/* round abs(x) toward nearest, then sign it.
		 * MIPS 128-bit is accurate to 107 bits.
		 *
		 * Under current SW rules, x+2**106 has no fraction,
		 * as abs(x) < 2**106 and 128-bit rounds to 107
		 * bits.  Rounding to 107 bits is required for
		 * x+2**106 to be rounded correctly in all cases.
		 * This rule may change in the future.
		 *
		 * truncl truncates to a whole number.
		 */
#pragma noinline
		da =	_add_q(abs.ldword,TWO_106);
		da =	da - TWO_106;
		if( x_val.ldword < 0.0 )
			da = -da;
		return da;
	} else if (rounding == FE_TOWARDZERO) {
		return truncl(x_val.ldword);
	} else if( truncl(abs.ldword) == abs.ldword ) {
		return x_val.ldword;
	} else if (rounding == FE_DOWNWARD) {
		if( x_val.ldword > 0.0 )
			return truncl(x_val.ldword);
		if( x_val.ldword < 0.0 )
			return truncl(x_val.ldword)-1.0L;
	} else {		/* if (rounding == FE_UPWARD) */
		if( x_val.ldword < 0.0 )
			return truncl(x_val.ldword);
		if( x_val.ldword > 0.0 )
			return truncl(x_val.ldword)+1.0L;
	}
	return(result.ldword);
}
#endif

_f_real4
_IEEE_INT_D_H(_f_real16 argx)
#ifndef	__mips
{
	/* Union defined to work with IEEE 128-bit floating point. */
	union _ieee_ldouble {
		struct {
                        unsigned int sign	: 1;
			unsigned int exponent	: IEEE_128_EXPO_BITS;
			unsigned int mantissa_up1 : IEEE_128_MANT_BTS_UP1;
			unsigned int mantissa_up2 : IEEE_128_MANT_BTS_UP2;
			unsigned int mantissa_lo1 : IEEE_128_MANT_BTS_LO1;
			unsigned int mantissa_lo2 : IEEE_128_MANT_BTS_LO2;
		} parts1;
		_f_real16		ldword;
		unsigned long long	llword[2];
		unsigned int		lword[4];
		long long		lsword[2];
	};
	/* Union defined to work with IEEE 32-bit floating point. */
	union _ieee_fdouble {
		_f_real4	fpword;
		unsigned int	l4word;
	};

	static union _ieee_ldouble two_112 =
	   {0, 0x406F, 0x0000, 0x00000000, 0x00000000, 0x00000000};
	static union _ieee_ldouble div_112 =
	   {0, 0x3F8F, 0x0000, 0x00000000, 0x00000000, 0x00000000};
	static union _ieee_ldouble two_32 =
	   {0, 0x401F, 0x0000, 0x00000000, 0x00000000, 0x00000000};
	unsigned int	sign_x = 0X80000000;
	unsigned int	even_x = 0X00000001;
	int	rounding;
	union _ieee_ldouble x_val, evenchk, nearint, tmp, abs;
	union _ieee_fdouble result;
	x_val.ldword	= argx;
	result.fpword	= 0.0;
	abs.ldword	= argx;
	abs.lword[0] &= ~sign_x;

	/* check x for infinity, NaN, and zero. */
	if (!isnormal128(x_val.ldword)) {
		if (isnan128(x_val.ldword)) {
			result.fpword	= _HALF_NaN;
			return(result.fpword);
		} else if ((((x_val.llword[0] & ~IEEE_128_64_SIGN_BIT) ==
	 		   IEEE_128_64_EXPO) && x_val.llword[1] == 0)) {
			result.l4word	= IEEE_32_INFINITY;
			return(result.fpword);
		} else if (x_val.ldword == 0.0) {
			result.fpword	= (_f_real4) x_val.ldword;
			return(result.fpword);
		}
	} else if (two_112.ldword <= abs.ldword) {
		/* value is already an integer. */
		result.fpword	= (_f_real4) x_val.ldword;
		return(result.fpword);
	}

	rounding	= fegetround();
	if (rounding == FE_TONEAREST) {
		/* round toward nearest */
		CALC_DNINT();
		result.fpword = (_f_real4) nearint.lsword[1];
	} else {
		/*
		 * RP_RMZ = round toward zero=truncate.
		 * FE_UPWARD = round toward plus infinity=round up.
		 * FE_DOWNWARD = round toward minus infinity=round down.
		 */
		CALC_DINT();
		result.fpword = (_f_real4) tmp.ldword;
	}
	return(result.fpword);
}
#else	/* NOT __mips */
{
	/* Union defined to work with MIPS 128-bit floating point. */
	union _ieee_ldouble {
		_f_real16		ldword;
		_f_real8		dword[2];
		unsigned long long	llword[2];
		unsigned int		lword[4];
		long long		lsword[2];
	};
	/* Union defined to work with IEEE 32-bit floating point. */
	union _ieee_fdouble {
		_f_real4	fpword;
		unsigned int	l4word;
	};
	unsigned int	sign_x = 0X80000000;
	int	rounding;
	union _ieee_ldouble x_val, tmp, abs;
	union _ieee_fdouble result;
	_f_real16	da;
	_f_real16 _add_q(_f_real16 x, _f_real16 y);
	x_val.ldword	= argx;
	result.fpword	= 0.0;
	abs.ldword	= argx;
	abs.lword[0] &= ~sign_x;

	/* check x for infinity, NaN, zero, and denormal. */
	if (!isnormal128(x_val.ldword)) {

		if (isnan64(x_val.dword[0]) || isnan64(x_val.dword[1])) {
			return (_f_real4) (x_val.dword[0]+x_val.dword[1]);
		} else if ( abs.llword[0] == IEEE_64_INFINITY ||
		     abs.llword[1] == IEEE_64_INFINITY) {
			return (_f_real4) (x_val.dword[0]+x_val.dword[1]);
		} else if (x_val.ldword == 0.0) {
			result.fpword	= (_f_real4) x_val.ldword;
			return(result.fpword);
		}
	}

	/* get rounding mode. */
	rounding	= fegetround();

	/* MIPS HW/SW 128 bit may not respond to mode.
	 * Assume round to nearest works when set.
	 * RP_RMN = round toward nearest
	 * RP_RMZ = round toward zero=truncate
	 * FE_UPWARD = round toward plus infinity=round up
	 * FE_DOWNWARD = round toward minus infinity=round down
	 * Note: abs(value) >0.0, due to prior test
	 */
	if (rounding == FE_TONEAREST) {
		/* round abs(x) toward nearest where x .ne. 0.0.
		 * if x.ge.2**23 upper 24 bits contain no fraction,
		 *	cast x to real*4;
		 * else if (-0.5 .le. x .lt. 0.0) round to -0.0;
		 * else if ( 0.0 .lt. x .le. 0.5) round to +0.0;
		 * else
		 *	round at decimal point;
		 *	cast x to real*4;
		 */
		if (abs.ldword >= TWO_23)
			return (_f_real4) x_val.ldword;
 
		if (abs.ldword <= 0.5) {
			if (x_val.ldword > 0.0)
				return 0.0;
			else
				return 0.0*(-1.0);
		}
		/* Upper 24 bits of x contain fraction, possibly nonzero.
		 * In quad precision, round at the decimal.
		 * Note that up to 106 fraction bits can afffect rounding.
		 */

		/* round fraction */
#pragma noinline
		da =	_add_q(abs.ldword,TWO_106);
		da =	da - TWO_106;
		result.fpword = (_f_real4) da;
		if (x_val.ldword >= 0.0)
			return result.fpword;
		else
			return (-1.0)*result.fpword;
	}
 
	/* if |x| .le. 2**24, truncate with truncl (exact in this case)
	 * else zero the low 29 (53-24) bits out.
	 * If signs of upper and lower differ, decrement upper first.
	 * Make sure that -0.0 is produced where needed.
	 */
	if (abs.ldword <= TWO_24) {
		result.fpword= (_f_real4) truncl(x_val.ldword);
	} else {
		/* If |x|.lower < 0.0, decrement |x|.upper. */
		if (abs.llword[1] < 0.0)
			tmp.llword[0] = abs.llword[0] - 1;
		else
			tmp.llword[0] = abs.llword[0];
		tmp.llword[0] = (tmp.llword[0] >> 29) << 29;
		result.fpword= (_f_real4) tmp.dword[0];
		if (x_val.ldword < 0.0L)
			result.fpword= (-1.0) * result.fpword;
	}
 
	if (rounding == FE_TOWARDZERO || x_val.ldword == result.fpword)
		return result.fpword;
		/* if (rounding == FE_TOWARDZERO) -0.1 becomes -0.0 */
		/* if (rounding == FE_UPWARD) -0.1 doesn't get here */
 
	/* result .ne. argument and round mode is up or down.
	 * Note: we rely on 32-bit floating arithmetic to round
	 *      (result.fpword +/- 1.0) correctly when x .ge. 2**24.
	 */
	if (rounding == FE_DOWNWARD) {
		if (x_val.ldword < 0.0)
			result.fpword = result.fpword - 1.0;
		/* if (rounding == FE_DOWNWARD) -0.1 becomes -1.0 */
	}
	else {		/* if (rounding == FE_UPWARD) */
		if (x_val.ldword > 0.0)
			result.fpword = result.fpword + 1.0;
		/* if (rounding == FE_UPWARD) -0.1 becomes -0.0 */
	}
	return result.fpword;
}
#endif	/* NOT __mips */

_f_real8
_IEEE_INT_D_R(_f_real16 argx)
#ifndef	__mips
{
	/* Union defined to work with IEEE 128-bit floating point. */
	union _ieee_ldouble {
		struct {
                        unsigned int sign	: 1;
			unsigned int exponent	: IEEE_128_EXPO_BITS;
			unsigned int mantissa_up1 : IEEE_128_MANT_BTS_UP1;
			unsigned int mantissa_up2 : IEEE_128_MANT_BTS_UP2;
			unsigned int mantissa_lo1 : IEEE_128_MANT_BTS_LO1;
			unsigned int mantissa_lo2 : IEEE_128_MANT_BTS_LO2;
		} parts1;
		_f_real16		ldword;
		unsigned long long	llword[2];
		unsigned int		lword[4];
		long long		lsword[2];
	};
	/* Union defined to work with IEEE 64-bit floating point. */
	union _ieee_double {
		_f_real8		dword;
		unsigned long long	int8word;
		unsigned int		l8word[2];
	};

	static union _ieee_ldouble two_112 =
	   {0, 0x406F, 0x0000, 0x00000000, 0x00000000, 0x00000000};
	static union _ieee_ldouble div_112 =
	   {0, 0x3F8F, 0x0000, 0x00000000, 0x00000000, 0x00000000};
	static union _ieee_ldouble two_64 =
	   {0, 0x403F, 0x0000, 0x00000000, 0x00000000, 0x00000000};
	unsigned int	sign_x = 0X80000000;
	unsigned int	even_x = 0X00000001;
	int	rounding;
	union _ieee_ldouble x_val, evenchk, nearint, tmp, abs;
	union _ieee_double result;
	x_val.ldword	= argx;
	result.dword	= 0.0;
	abs.ldword	= argx;
	abs.lword[0] &= ~sign_x;

	/* check x for infinity, NaN and zero. */
	if (!isnormal128(x_val.ldword)) {
		if (isnan128(x_val.ldword)) {
			result.int8word	= _SGL_NaN;
			return(result.dword);
		} else if ((((x_val.llword[0] & ~IEEE_128_64_SIGN_BIT) ==
			    IEEE_128_64_EXPO) && x_val.llword[1] == 0)) {
			result.int8word	= IEEE_64_INFINITY;
			return(result.dword);
		} else if (x_val.ldword == 0.0) {
			/* x is zero. */
			result.dword	= (_f_real8) x_val.ldword;
			return(result.dword);
		}
	} else if (two_112.ldword <= abs.ldword) {
		/* value is already an integer. */
		result.dword	= (_f_real8) x_val.ldword;
		return(result.dword);
	}

	rounding	= fegetround();
	if (rounding == FE_TONEAREST) {
		/* round toward nearest */
		CALC_DNINT();
		result.dword = (_f_real8) nearint.lsword[1];
	} else {
		/*
		 * RP_RMZ = round toward zero=truncate.
		 * FE_UPWARD = round toward plus infinity=round up.
		 * FE_DOWNWARD = round toward minus infinity=round down.
		 */
		CALC_DINT();
		result.dword = (_f_real8) tmp.ldword;
	}
	return(result.dword);
}
#else	/* NOT __mips */
{
	/* Union defined to work with MIPS 128-bit floating point. */
	union _ieee_ldouble {
		_f_real16		ldword;
		_f_real8		dword[2];
		unsigned long long	llword[2];
		unsigned int		lword[4];
		long long		lsword[2];
	};
	/* Union defined to work with IEEE 64-bit floating point. */
	union _ieee_double {
		_f_real8		dpword;
		unsigned long long	int8word;
		unsigned int		l8word[2];
	};
	unsigned int	sign_x = 0X80000000;
	int	rounding;
	union _ieee_ldouble x_val, tmp, abs;
	union _ieee_double result;
	_f_real16	da;
	_f_real16 _add_q(_f_real16 x, _f_real16 y);
	x_val.ldword	= argx;
	result.dpword	= 0.0;
	abs.ldword	= argx;
	abs.lword[0] &= ~sign_x;

	/* check x for infinity, NaN, zero, and denormal. */
	if (!isnormal128(x_val.ldword)) {

		if (isnan64(x_val.dword[0]) || isnan64(x_val.dword[1])) {
			/* raise possible exception */
			return (x_val.dword[0]+x_val.dword[1]);
		} else if ( abs.llword[0] == IEEE_64_INFINITY ||
		     abs.llword[1] == IEEE_64_INFINITY) {
			return (x_val.dword[0]+x_val.dword[1]);
		} else if (x_val.ldword == 0.0) {
			result.dpword	= (_f_real8) x_val.ldword;
			return(result.dpword);
		}
	}

	/* get rounding mode. */
	rounding	= fegetround();

	/* MIPS HW/SW 128 bit may not respond to mode.
	 * Assume round to nearest works when set.
	 * RP_RMN = round toward nearest
	 * RP_RMZ = round toward zero=truncate
	 * FE_UPWARD = round toward plus infinity=round up
	 * FE_DOWNWARD = round toward minus infinity=round down
	 * Note: abs(value) >0.0, due to prior test
	 */
	if (rounding == FE_TONEAREST) {
		/* round abs(x) toward nearest where x .ne. 0.0.
		 * if x.ge.2**52 upper 53 bits contain no fraction,
		 *	cast x to real*8;
		 * else if (-0.5 .le. x .lt. 0.0) round to -0.0;
		 * else if ( 0.0 .lt. x .le. 0.5) round to +0.0;
		 * else
		 *	round at decimal point;
		 *	cast x to real*8;
		 */
		if (abs.ldword >= TWO_52)
			return (_f_real8) x_val.ldword;
 
		if (abs.ldword <= 0.5) {
			if (x_val.ldword > 0.0)
				return 0.0;
			else
				return 0.0*(-1.0);
		}
		/* Upper 53 bits of x contain fraction, possibly nonzero.
		 * In quad precision, round at the decimal.
		 * Note that up to 106 fraction bits can afffect rounding.
		 */

		/* round fraction */
#pragma noinline
		da =	_add_q(abs.ldword,TWO_106);
		da =	da - TWO_106;
		result.dpword = (_f_real8) da;
		if (x_val.ldword >= 0.0)
			return result.dpword;
		else
			return (-1.0)*result.dpword;
	}

	/* if |x| .le. 2**53, truncate with truncl (exact in this case)
	 * else use the upper 53 bits.
	 * If signs of upper and lower differ, decrement upper first.
	 * Make sure that -0.0 is produced where needed.
	 */
	if (abs.ldword <= TWO_53) {
		result.dpword= (_f_real8) truncl(x_val.ldword);
	} else {
		/* If |x|.lower < 0.0, decrement |x|.upper. */
		if (abs.llword[1] < 0.0)
			tmp.llword[0] = abs.llword[0] - 1;
		else
			tmp.llword[0] = abs.llword[0];
		result.dpword= (_f_real8) tmp.dword[0];
		if (x_val.ldword < 0.0L)
			result.dpword= (-1.0) * result.dpword;
	}
	if (rounding == FE_TOWARDZERO || x_val.ldword == result.dpword)
		return result.dpword;
		/* if (rounding == FE_TOWARDZERO) -0.1 becomes -0.0 */
		/* if (rounding == FE_UPWARD) -0.1 doesn't get here */
 
	/* result .ne. argument and round mode is up or down.
	 * Note: we rely on 64-bit floating arithmetic to round
	 *      (result.dpword +/- 1.0) correctly when x .ge. 2**53.
	 */
	if (rounding == FE_DOWNWARD) {
		if (x_val.ldword < 0.0)
			result.dpword = result.dpword - 1.0;
		/* if (rounding == FE_DOWNWARD) -0.1 becomes -1.0 */
	}
	else {		/* if (rounding == FE_UPWARD) */
		if (x_val.ldword > 0.0)
			result.dpword = result.dpword + 1.0;
		/* if (rounding == FE_UPWARD) -0.1 becomes -0.0 */
	}
	return result.dpword;
}
#endif	/* NOT __mips */

_f_real16
_IEEE_INT_H_D(_f_real4 argx)
#ifndef	__mips
{
	/* Union defined to work with IEEE 128-bit floating point. */
	union _ieee_ldouble {
		struct {
                        unsigned int sign	: 1;
			unsigned int exponent	: IEEE_128_EXPO_BITS;
			unsigned int mantissa_up1 : IEEE_128_MANT_BTS_UP1;
			unsigned int mantissa_up2 : IEEE_128_MANT_BTS_UP2;
			unsigned int mantissa_lo1 : IEEE_128_MANT_BTS_LO1;
			unsigned int mantissa_lo2 : IEEE_128_MANT_BTS_LO2;
		} parts1;
		_f_real16		ldword;
		unsigned long long	llword[2];
		unsigned int		lword[4];
		long long		lsword[2];
	};
	/* Union defined to work with IEEE 32-bit floating point. */
	union _ieee_fdouble {
		_f_real4	fpword;
		unsigned int	l4word;
		int		int4word;
	};

	static union _ieee_ldouble two_112 =
	   {0, 0x406F, 0x0000, 0x00000000, 0x00000000, 0x00000000};
	static union _ieee_ldouble div_112 =
	   {0, 0x3F8F, 0x0000, 0x00000000, 0x00000000, 0x00000000};
	unsigned int	sign_x = 0X80000000;
	unsigned int	even_x = 0X00000001;
	int	rounding;
	union _ieee_ldouble x_val, evenchk, nearint, tmp, abs, result;
	union _ieee_fdouble x4_val, abs4;
	x4_val.fpword	= argx;
	result.ldword	= 0.0;
	abs4.l4word	= x4_val.l4word & ~sign_x;

	/* check x for infinity, NaN, and zero. */
	if (!isnormal32(x4_val.l4word)) {
		if (isnan32(x4_val.fpword)) {
			result.ldword = _DBL_NaN;
			return(result.ldword);
		} else if ((x4_val.l4word & ~IEEE_32_SIGN_BIT) ==
				IEEE_32_INFINITY) {
			result.llword[0] = IEEE_128_64_EXPO;
			result.llword[1] = 0;
			if ((x4_val.l4word & IEEE_32_SIGN_BIT) == sign_x)
				result.parts1.sign = 1;
			return(result.ldword);
		} else if (x4_val.fpword == 0.0) {
			if ((x4_val.l4word & IEEE_32_SIGN_BIT) == sign_x)
				result.parts1.sign = 1;
			return(result.ldword);
		}
	} else if (abs4.fpword > 8388608) {
		/* value is already an integer. */
		result.ldword	= (_f_real16) x4_val.fpword;
		return(result.ldword);
	}

	x_val.ldword	= (_f_real16) x4_val.fpword;
	rounding	= fegetround();
	if (rounding == FE_TONEAREST) {
		/* round toward nearest */
		CALC_DNINT();
		result.ldword = (_f_real16) nearint.lsword[1];
	} else {
		/*
		 * RP_RMZ = round toward zero=truncate.
		 * FE_UPWARD = round toward plus infinity=round up.
		 * FE_DOWNWARD = round toward minus infinity=round down.
		 */
		CALC_DINT();
		result.ldword = tmp.ldword;
	}
	return(result.ldword);
}
#else	/* NOT __mips */
{
	/* Union defined to work with MIPS 128-bit floating point. */
	union _ieee_ldouble {
		_f_real16		ldword;
		_f_real8		dword[2];
		unsigned long long	llword[2];
		unsigned int		lword[4];
		long long		lsword[2];
	};
	/* Union defined to work with IEEE 32-bit floating point. */
	union _ieee_fdouble {
		_f_real4	fpword;
		unsigned int	l4word;
		int		int4word;
	};
	unsigned int	sign_x = 0X80000000;
	int	rounding;
	union _ieee_ldouble x_val, abs, result;
	union _ieee_fdouble x4_val, abs4;
	_f_real16	da;
	_f_real16 _add_q(_f_real16 x, _f_real16 y);
	x4_val.fpword	= argx;
	result.ldword	= 0.0;
	abs4.l4word	= x4_val.l4word & ~sign_x;

	/* check x for infinity, NaN, and zero. 2**23 is 8388608 */
	if (!isnormal32(x4_val.l4word) || abs4.fpword >= 8388608) {
		/* casting raises possible exception */
		return (_f_real16) x4_val.fpword;
	}
	x_val.ldword	= (_f_real16) x4_val.fpword;

	/* get rounding mode. */
	rounding	= fegetround();
	/* MIPS HW/SW 128 bit may not respond to mode.
	 * Assume round to nearest works when set.
	 * RP_RMN = round toward nearest
	 * RP_RMZ = round toward zero=truncate
	 * FE_UPWARD = round toward plus infinity=round up
	 * FE_DOWNWARD = round toward minus infinity=round down
	 * Note: abs(value) >0.0, due to prior test
	 */
	if (rounding == FE_TONEAREST) {
		/* round abs(x) toward nearest, then sign it.
		 * MIPS 128-bit is accurate to 107 bits.
		 *
		 * Under current SW rules, x+2**106 has no fraction,
		 * as abs(x) < 2**106 and 128-bit rounds to 107
		 * bits.  Rounding to 107 bits is required for
		 * x+2**106 to be rounded correctly in all cases.
		 * This rule may change in the future.
		 *
		 * truncl truncates to a whole number.
		 */
#pragma noinline
		da =	_add_q((_f_real16)abs4.fpword,TWO_106);
		da =	da - TWO_106;
		if( x_val.ldword < 0.0 )
			da = -da;
		return da;
	} else if (rounding == FE_TOWARDZERO) {
		return truncl(x_val.ldword);
	} else if( truncl(abs4.fpword) == abs4.fpword ) {
		return x_val.ldword;
	} else if (rounding == FE_DOWNWARD) {
		if( x_val.ldword > 0.0 )
			return truncl(x_val.ldword);
		if( x_val.ldword < 0.0 )
			return truncl(x_val.ldword)-1.0L;
	} else {		/* if (rounding == FE_UPWARD) */
		if( x_val.ldword < 0.0 )
			return truncl(x_val.ldword);
		if( x_val.ldword > 0.0 )
			return truncl(x_val.ldword)+1.0L;
	}
	return(result.ldword);
}
#endif	/* NOT __mips */

_f_real16
_IEEE_INT_R_D(_f_real8 argx)
#ifndef	__mips
{
	/* Union defined to work with IEEE 128-bit floating point. */
	union _ieee_ldouble {
		struct {
                        unsigned int sign	: 1;
			unsigned int exponent	: IEEE_128_EXPO_BITS;
			unsigned int mantissa_up1 : IEEE_128_MANT_BTS_UP1;
			unsigned int mantissa_up2 : IEEE_128_MANT_BTS_UP2;
			unsigned int mantissa_lo1 : IEEE_128_MANT_BTS_LO1;
			unsigned int mantissa_lo2 : IEEE_128_MANT_BTS_LO2;
		} parts1;
		_f_real16		ldword;
		unsigned long long	llword[2];
		unsigned int		lword[4];
		long long		lsword[2];
	};
	/* Union defined to work with IEEE 64-bit floating point. */
	union _ieee_double {
                struct {
#if __BYTE_ORDER == __LITTLE_ENDIAN
                         unsigned int mantissa_lo : IEEE_64_MANT_BTS2;
                         unsigned int mantissa_up : IEEE_64_MANT_BTS1;
                         unsigned int exponent8 : IEEE_64_EXPO_BITS;
                         unsigned int sign8     : 1;
#else
                         unsigned int sign8     : 1;
                         unsigned int exponent8 : IEEE_64_EXPO_BITS;
                         unsigned int mantissa_up : IEEE_64_MANT_BTS1;
                         unsigned int mantissa_lo : IEEE_64_MANT_BTS2;
#endif
                } parts81;
		_f_real8		dword;
		unsigned long long	int8word;
		unsigned int		l8word[2];
	};

	static union _ieee_ldouble two_112 =
	   {0, 0x406F, 0x0000, 0x00000000, 0x00000000, 0x00000000};
	static union _ieee_ldouble div_112 =
	   {0, 0x3F8F, 0x0000, 0x00000000, 0x00000000, 0x00000000};
	static union _ieee_double two_52 =
	   {0, 0x433, 0x00000, 0x00000000};
	unsigned int	sign_x = 0X80000000;
	unsigned int	even_x = 0X00000001;
	int	rounding;
	union _ieee_ldouble x_val, evenchk, nearint, tmp, abs, result;
	union _ieee_double x8_val, abs8;
	x8_val.dword	= argx;
	result.ldword	= 0.0;
	abs8.int8word	= x8_val.int8word & ~sign_x;

	/* check x for infinity, NaN, and zero. */
	if (!isnormal64(x8_val.int8word)) {
		if (isnan64(x8_val.dword)) {
			result.ldword = _DBL_NaN;
			return(result.ldword);
		} else if ((x8_val.int8word & ~IEEE_64_SIGN_BIT) ==
				IEEE_64_INFINITY) {
			result.llword[0] = IEEE_128_64_EXPO;
			result.llword[1] = 0;
			result.parts1.sign = x8_val.parts81.sign8;
			return(result.ldword);
		} else if (x8_val.dword == 0.0) {
			result.parts1.sign = x8_val.parts81.sign8;
			return(result.ldword);
		}
	} else if ((int) abs8.l8word[0] > (int) two_52.l8word[0]) {
		/* value is already an integer. */
		result.ldword	= (_f_real16) x8_val.dword;
		return(result.ldword);
	}

	x_val.ldword	= (_f_real16) x8_val.dword;
	rounding	= fegetround();
	if (rounding == FE_TONEAREST) {
		/* round toward nearest */
		CALC_DNINT();
		result.ldword = (_f_real16) nearint.lsword[1];
	} else {
		/*
		 * RP_RMZ = round toward zero=truncate.
		 * FE_UPWARD = round toward plus infinity=round up.
		 * FE_DOWNWARD = round toward minus infinity=round down.
		 */
		CALC_DINT();
		result.ldword = tmp.ldword;
	}
	return(result.ldword);
}
#else /* NOT mips */
{
	/* Union defined to work with MIPS 128-bit floating point. */
	union _ieee_ldouble {
		_f_real16		ldword;
		_f_real8		dword[2];
		unsigned long long	llword[2];
		unsigned int		lword[4];
		long long		lsword[2];
	};
	/* Union defined to work with IEEE 64-bit floating point. */
	union _ieee_double {
		_f_real8		dpword;
		unsigned long long	int8word;
		unsigned int		l8word[2];
	};
	unsigned int	sign_x = 0X80000000;
	int	rounding;
	union _ieee_ldouble x_val, abs, result;
	union _ieee_double x8_val, abs8;
	_f_real16	da;
	_f_real16 _add_q(_f_real16 x, _f_real16 y);
	x8_val.dpword	= argx;
	result.ldword	= 0.0;
	abs8.int8word	= x8_val.int8word & ~sign_x;

	/* check x for infinity, NaN, and zero. */
	if (!isnormal64(x8_val.int8word)) {
		if (isnan64(x8_val.dpword)) {
			result.llword[0] = x8_val.int8word;
			result.llword[1] = 0;
			return(result.ldword);
		} else if ((x8_val.int8word & ~IEEE_64_SIGN_BIT) ==
				IEEE_64_INFINITY) {
			result.llword[0] = x8_val.int8word;
			result.llword[1] = 0;
			return(result.ldword);
		} else if (x8_val.dpword == 0.0) {
			result.llword[0] = x8_val.int8word;
			result.llword[1] = 0;
			return(result.ldword);
		}
	} else if (abs8.dpword > TWO_52) {
		/* value is already an integer. */
		result.ldword	= (_f_real16) x8_val.dpword;
		return(result.ldword);
	}
	x_val.ldword	= (_f_real16) x8_val.dpword;

	/* get rounding mode. */
	rounding	= fegetround();
	/* MIPS HW/SW 128 bit may not respond to mode.
	 * Assume round to nearest works when set.
	 * Note: abs(value) >0.0, due to prior test
	 */
	if (rounding == FE_TONEAREST) {
		/* round abs(x) toward nearest, then sign it.
		 * MIPS 128-bit is accurate to 107 bits.
		 *
		 * Under current SW rules, x+2**106 has no fraction,
		 * as abs(x) < 2**106 and 128-bit rounds to 107
		 * bits.  Rounding to 107 bits is required for
		 * x+2**106 to be rounded correctly in all cases.
		 * This rule may change in the future.
		 *
		 * truncl truncates to a whole number.
		 */
#pragma noinline
		da =	_add_q((_f_real16)abs8.dpword,TWO_106);
		da =	da - TWO_106;
		if( x_val.ldword < 0.0 )
			da = -da;
		return da;
	} else if (rounding == FE_TOWARDZERO) {
		return truncl(x_val.ldword);
	} else if( truncl(abs8.dpword) == abs8.dpword ) {
		return x_val.ldword;
	} else if (rounding == FE_DOWNWARD) {
		if( x_val.ldword > 0.0 )
			return truncl(x_val.ldword);
		if( x_val.ldword < 0.0 )
			return truncl(x_val.ldword)-1.0L;
	} else {		/* if (rounding == FE_UPWARD) */
		if( x_val.ldword < 0.0 )
			return truncl(x_val.ldword);
		if( x_val.ldword > 0.0 )
			return truncl(x_val.ldword)+1.0L;
	}
	return(result.ldword);
}
#endif	/* NOT __mips */

#ifdef	__mips

/* For int results, rounding is ALWAYS at the decimal and 64 bit
 * is always enough to receive the result. This result can be casted 
 * once again to the proper size.
 *
 * if (|x|<2**63) {  ...else cast will cause exception...
 * 	if (RMN) x+2**106-2**106.
 * 	if (RMZ || (RMP && x<0) || (RMM && x>0) ) qtruncl(x).
 * 	if ( x == qtruncl(x) ) qtruncl(x).
 * 	if (RMM) x= qtruncl(x)-1.0 ...inexact only if result overflows..
 * 	if (RMZ) x= qtruncl(x)+1.0 ...inexact only if result overflows..
 * } else 
 * 	result = x<0 ? -HUGE : HUGE;
 * cast result;
 */

_f_int8
_d_to_int(_f_real16 argx, _f_int8  ihuge)
{
	/* Union defined to work with MIPS 128-bit floating point. */
	union _ieee_ldouble {
		_f_real16		ldword;
		_f_real8		dword[2];
		unsigned long long	llword[2];
		unsigned int		lword[4];
		long long		lsword[2];
	};
	unsigned int	sign_x = 0X80000000;
	int	rounding;
	union _ieee_ldouble x_val, abs;
	_f_int8 result8;
	_f_real16	da;
	_f_real16 _add_q(_f_real16 x, _f_real16 y);
	x_val.ldword	= argx;
	abs.ldword	= argx;
	abs.lword[0] &= ~sign_x;

/* For int results, rounding is ALWAYS at the decimal and 64 bit
 * is always enough to receive the result. This result can be casted 
 * once again to the proper size.
 *
 * if (|x|<2**63) {
 * 	if (RMN) x+2**106-2**106.
 * 	if (RMZ) truncl(x).
 * 	if ( x == truncl(x) ) truncl(x).
 * 	if (RMM) x= truncl(x)-1.0 .. inexact only if result overflows ..
 * 	if (RMZ) x= truncl(x)+1.0 .. inexact only if result overflows ..
 * } else {
 * 	result = x<0 ? -HUGE : HUGE;
 *	cast result;
 * }
 */
	if ( !(abs.ldword <= HUGE_REAL16_F90) ){
		/* raise invalid operation for Nan or Infinity.
		 * No error if invalid operation exception is not
		 * for signed infinity and zero for NaN.
		 * _lerror (_LELVL_ABORT, FEIEINTE)
		 * always signal !!!!!!!!!!!!!!!!!!!
		 */
		_f_real8        arg8 = 0.0;
		result8 = _raisinvld(arg8, arg8);

		if( x_val.ldword > 0.0 )
			return ihuge;
		else
			return -ihuge;
	}

	/* get rounding mode. */
	rounding	= fegetround();

	/* MIPS HW/SW 128 bit may not respond to mode.
	 * Assume round to nearest works when set.
	 * RP_RMN = round toward nearest
	 * RP_RMZ = round toward zero=truncate
	 * FE_UPWARD = round toward plus infinity=round up
	 * FE_DOWNWARD = round toward minus infinity=round down
	 */
	if (rounding == FE_TONEAREST) {
		/*
		 * In quad precision, round at the decimal.
		 * Note that up to 106 fraction bits can afffect rounding.
		 * If x is a whole number, always overflow.
		 */
		if (x_val.ldword >= 0.0) {

			/* round */
#pragma noinline
			da =	_add_q(x_val.ldword,TWO_106);
			da =	da - TWO_106;
		} else {

			/* round */
#pragma noinline
			da =	_add_q(x_val.ldword,-TWO_106);
			da =	da + TWO_106;
		}
		if (da < -ihuge)
			da = -1 - ihuge;
		return (_f_int8) da;
	}
 
	da =	truncl(x_val.ldword);	/* truncate */

	if (rounding == FE_TOWARDZERO || x_val.ldword == da) {
		if (da < -ihuge)
			da = -1 - ihuge;
		return (_f_int8) da;
	}
 
	/* trunc(x) != x and round mode is up or down. */
	if (rounding == FE_DOWNWARD && x_val.ldword < 0.0)
		da = da - 1.0;
	if (rounding == FE_UPWARD && x_val.ldword > 0.0)
		da = da + 1.0;
	if (da < -ihuge)
		da = -1 - ihuge;
	return (_f_int8) da;
}

/* Add this routine to ensure that the code to round is
 * executed.
 */
_f_real16
_add_q(_f_real16 x, _f_real16 y)
{
	return(x+y);
}

#endif
