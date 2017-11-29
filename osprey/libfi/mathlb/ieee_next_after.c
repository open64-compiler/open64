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


#pragma ident "@(#) libfi/mathlb/ieee_next_after.c	92.1	07/09/99 11:00:36"

#include <fmath.h>
#include <fortran.h>
#include <fp.h>
#include "inline.h"
#ifdef KEY /* Bug 10771, 10259, 11941 */
# include <math.h>
# include <fenv.h>

#if defined(TARG_IA32) || defined(TARG_X8664)
/* Workaround for bug in libm nextafter and nextafterf which fails to set IEEE
 * flags as required by the C standard. In FC3 and SuSE9 this was only broken
 * for -m32, but in FC4 and SuSE10 it's broken for -m64 too. */
#  define NEXTAFTER_SET_FLAGS(result) \
	{ \
	  int classification = fpclassify(result); \
	  if (classification == FP_INFINITE) { \
	    feraiseexcept(FE_OVERFLOW | FE_INEXACT); \
	  } \
	  else if (classification == FP_SUBNORMAL) { \
	    feraiseexcept(FE_UNDERFLOW | FE_INEXACT); \
	  } \
	}
# else /* defined(TARG_IA32) || defined(TARG_X8664) */
#  define NEXTAFTER_SET_FLAGS(result) /* No bug */
# endif /* defined(TARG_IA32) || defined(TARG_X8664) */
#endif /* KEY Bug 10771, 10259, 11941 */

extern _f_real4 _IEEE_NEXT_AFTER_H(_f_real4 x, _f_real4 s);
extern _f_real4 _IEEE_NEXT_AFTER_H_R(_f_real4 x, _f_real8 s);
extern _f_real8 _IEEE_NEXT_AFTER(_f_real8 x, _f_real8 s);
extern _f_real8 _IEEE_NEXT_AFTER_R_H(_f_real8 x, _f_real4 s);

static _f_real8 _raisinexct8(_f_real8 x, _f_real8 y);
static _f_real8 _raisovfl8(_f_real8 x);
static _f_real8 _raisunfl8(_f_real8 x, _f_real8 y);

static _f_real8 _raisinexct8(_f_real8 x, _f_real8 y)
{
        return (x / y);
}

static _f_real8 _raisovfl8(_f_real8 x)
{
        return (x * x);
}

static _f_real8 _raisunfl8(_f_real8 x, _f_real8 y)
{
        return (x / y);
}

/* NEXT_AFTER - return the nearest different machine representable number
 * 	        in a given direction s for 32, 64, and 128-bit values.
 * 	        Returns the argument x if x = s.  
 * 	        Returns the argument x if X = NaN.
 * 	        Returns the argument s if s = NaN.
 */
_f_real4
_IEEE_NEXT_AFTER_H(_f_real4 x, _f_real4 s)
{
#ifdef KEY /* Bug 10771 */
        _f_real4 result = nextafterf(x, s);
	NEXTAFTER_SET_FLAGS(result);
	return result;
#else /* KEY Bug 10771 */
	REGISTER_4 s1, s2;
	if (isnan32(x)) {
		return x;
	} else if (isnan32(s)) {
		return s;
	}
	s1.f = x;
	if (((s1.ui &= ~IEEE_32_SIGN_BIT) == IEEE_32_INFINITY) ||
	   (x == s)) {
		return x;
	} else if (s1.f == 0.0) {
		s1.f = TINY_REAL4_F90;
		if (x > s)
			s1.ui |= IEEE_32_SIGN_BIT;
		return s1.f;
	} else {
		_f_real8	result =	0;
		_f_real8	arg1 =	1.0;
		_f_real8	arg2 =	10.0;
		_f_real8	unfl8 =	TINY_REAL8_F90;
		_f_real8	ovfl8 =	HUGE_REAL8_F90;

		s1.f = x;

		if (x > 0 ) {
			s1.ui += (x > s) ? -(0x1) : 0x1;
		} else {
			s1.ui += (x > s) ? 0x1 : -(0x1);
		}

		if (isnormal32(s1.ui))
			return s1.f;

		/*
		 * Raise overflow exception for infinite result and
		 * underflow exception for too small result.  Raise
		 * inexact exception for both cases. Allow subnormal
		 * values to return without exception.
		 */
		s2.f = s1.f;
		if ((s2.ui &= ~IEEE_32_SIGN_BIT) == IEEE_32_INFINITY) {
			result =	_raisovfl8(ovfl8);
		} else if (s1.f == 0) {
			result =	_raisunfl8(unfl8, ovfl8);
		} else {
			return (s1.f);
		}
		result =	_raisinexct8(arg1, arg2);
		return (s1.f);
	}
#endif /* KEY Bug 10771 */
}

_f_real4
_IEEE_NEXT_AFTER_H_R(_f_real4 x, _f_real8 s)
{
	REGISTER_4 s1, s2;
	if (isnan32(x)) {
		return x;
	} else if (isnan64(s)) {
		/* create NaN using previous NaN information. */
		return _HALF_NaN;
	}
#ifdef KEY /* Bug 10771 */
	if (((_f_real8) x) == s)
	  return x;
	_f_int4 infinity =
	  signbit(s) ? (0x80000000 | IEEE_32_INFINITY) : IEEE_32_INFINITY;
	_f_real4 result = nextafterf(x,
	  (((_f_real8) x) < s) ?
	  (* (_f_real4 *) &infinity) :
	  (- * (_f_real4 *) &infinity)
	  );
	NEXTAFTER_SET_FLAGS(result);
	return result;
#else /* KEY Bug 10771 */
	s1.f = x;
	if (((s1.ui &= ~IEEE_32_SIGN_BIT) == IEEE_32_INFINITY) ||
	   ((_f_real8) x == s)) {
		return x;
	} else if (s1.f == 0.0) {
		s1.f = TINY_REAL4_F90;
		if ((_f_real8) x > s)
			s1.ui |= IEEE_32_SIGN_BIT;
		return s1.f;
	} else {
		_f_real8	result =	0;
		_f_real8	arg1 =	1.0;
		_f_real8	arg2 =	10.0;
		_f_real8	unfl8 =	TINY_REAL8_F90;
		_f_real8	ovfl8 =	HUGE_REAL8_F90;
		_f_real8	hold8;

		s1.f = x;

		hold8	= (_f_real8) x;

		if (x > 0 ) {
			s1.ui += (hold8 > s) ? -(0x1) : 0x1;
		} else {
			s1.ui += (hold8 > s) ? 0x1 : -(0x1);
		}

		if (isnormal32(s1.ui))
			return s1.f;

		/*
		 * Raise overflow exception for infinite result and
		 * underflow exception for too small result.  Raise
		 * inexact exception for both cases. Allow subnormal
		 * values to return without exception.
		 */
		s2.f = s1.f;
		if ((s2.ui &= ~IEEE_32_SIGN_BIT) == IEEE_32_INFINITY) {
			result =	_raisovfl8(ovfl8);
		} else if (s1.f == 0) {
			result =	_raisunfl8(unfl8, ovfl8);
		} else {
			return (s1.f);
		}
		result =	_raisinexct8(arg1, arg2);
		return (s1.f);
	}
#endif /* KEY Bug 10771 */
}

_f_real8
_IEEE_NEXT_AFTER(_f_real8 x, _f_real8 s)
{
#ifdef KEY /* Bug 10771 */
        _f_real8 result = nextafter(x, s);
	NEXTAFTER_SET_FLAGS(result);
	return result;
#else /* KEY Bug 10771 */
	REGISTER_8 s1, s2;
	if (isnan64(x)) {
		return x;
	} else if (isnan64(s)) {
		return s;
	}
	s1.f = x;
	if (((s1.ui &= ~IEEE_64_SIGN_BIT) == IEEE_64_INFINITY) ||
	   (x == s)) {
		return x;
	} else if (s1.f == 0.0) {
		s1.f = TINY_REAL8_F90;
		if (x > s)
			s1.ui |= IEEE_64_SIGN_BIT;
		return s1.f;
	} else {
		_f_real8	result =	0;
		_f_real8	arg1 =	1.0;
		_f_real8	arg2 =	10.0;
		_f_real8	unfl8 =	TINY_REAL8_F90;
		_f_real8	ovfl8 =	HUGE_REAL8_F90;

		s1.f = x;

		if (x > 0 ) {
			s1.ui +=
			   (x > s) ? -(LL_CONST(0x1)) : LL_CONST(0x1);
		} else {
			s1.ui +=
			   (x > s) ? LL_CONST(0x1) : -(LL_CONST(0x1));
		}

		if (isnormal64(s1.ui))
			return s1.f;

		/*
		 * Raise overflow exception for infinite result and
		 * underflow exception for too small result.  Raise
		 * inexact exception for both cases. Allow subnormal
		 * values to return without exception.
		 */
		s2.f = s1.f;
		if ((s2.ui &= ~IEEE_64_SIGN_BIT) == IEEE_64_INFINITY) {
			result =	_raisovfl8(ovfl8);
		} else if (s1.f == 0.0) {
			result =	_raisunfl8(unfl8, ovfl8);
		} else {
			return (s1.f);
		}
		result =	_raisinexct8(arg1, arg2);
		return (s1.f);
	}
#endif /* KEY Bug 10771 */
}

_f_real8
_IEEE_NEXT_AFTER_R_H(_f_real8 x, _f_real4 s)
{
#ifdef KEY /* Bug 10771 */
        _f_real8 result = nextafter(x, (double) s);
	NEXTAFTER_SET_FLAGS(result);
	return result;
#else /* KEY Bug 10771 */
	REGISTER_8 s1, s2;
	if (isnan64(x)) {
		return x;
	} else if (isnan32(s)) {
		/* create NaN using previous NaN information. */
		return _SGL_NaN;
	}
	s1.f = x;
	if (((s1.ui &= ~IEEE_64_SIGN_BIT) == IEEE_64_INFINITY) ||
	   (x == s)) {
		return x;
	} else if (s1.f == 0.0) {
		s1.f = TINY_REAL8_F90;
		if ((_f_real4) x > s)
			s1.ui |= IEEE_64_SIGN_BIT;
		return s1.f;
	} else {
		_f_real8	result =	0;
		_f_real8	arg1 =	1.0;
		_f_real8	arg2 =	10.0;
		_f_real8	unfl8 =	TINY_REAL8_F90;
		_f_real8	ovfl8 =	HUGE_REAL8_F90;
		_f_real8	hold8;

		s1.f = x;

		hold8	= (_f_real8) s;

		if (x > 0 ) {
			s1.ui += (x > hold8) ?
				-(LL_CONST(0x1)) : LL_CONST(0x1);
		} else {
			s1.ui += (x > hold8) ?
				LL_CONST(0x1) : -(LL_CONST(0x1));
		}

		if (isnormal64(s1.ui))
			return s1.f;

		/*
		 * Raise overflow exception for infinite result and
		 * underflow exception for too small result.  Raise
		 * inexact exception for both cases. Allow subnormal
		 * values to return without exception.
		 */
		s2.f = s1.f;
		if ((s2.ui &= ~IEEE_64_SIGN_BIT) == IEEE_64_INFINITY) {
			result =	_raisovfl8(ovfl8);
		} else if (s1.f == 0) {
			result =	_raisunfl8(unfl8, ovfl8);
		} else {
			return (s1.f);
		}
		result =	_raisinexct8(arg1, arg2);
		return (s1.f);
	}
#endif /* KEY Bug 10771 */
}
#if _F_REAL16 == 1

extern _f_real16 _IEEE_NEXT_AFTER_D(_f_real16 x, _f_real16 s);
extern _f_real16 _IEEE_NEXT_AFTER_D_H(_f_real16 x, _f_real4 s);
extern _f_real16 _IEEE_NEXT_AFTER_D_R(_f_real16 x, _f_real8 s);
extern _f_real4 _IEEE_NEXT_AFTER_H_D(_f_real4 x, _f_real16 s);
extern _f_real8 _IEEE_NEXT_AFTER_R_D(_f_real8 x, _f_real16 s);

_f_real16
_IEEE_NEXT_AFTER_D(_f_real16 x, _f_real16 s)
{
#if defined(_WORD32)
	union ldble_float {
		_f_real16		whole;
		unsigned long long	ui[2];
	} f,rslt;
	unsigned long long	s2, s3, s4;
#else
	union ldble_float {
		_f_real16		whole;
		unsigned long 		ui[2];
	} f,rslt;
	unsigned long 		s2, s3, s4;
#endif

	if (isnan128(x)) {
		return x;
	}else if (isnan128(s)) {
		return s;
	}
	f.whole =	x;
	rslt.whole =	x;
	if ((((f.ui[0] &= ~IEEE_128_64_SIGN_BIT) == IEEE_128_64_EXPO) &&
	   (f.ui[1] == 0)) || (x == s)) {
		return x;
	} else if (rslt.whole == 0.0) {
		rslt.whole = TINY_REAL16_F90;
		if (x > s) {
			rslt.ui[0] |= IEEE_128_64_SIGN_BIT;
		}
		return rslt.whole;
	} else {
		_f_real8	result = 	0;
		_f_real8	arg1 =	1.0;
		_f_real8	arg2 =	10.0;
		_f_real8	unfl8 =	TINY_REAL8_F90;
		_f_real8	ovfl8 =	HUGE_REAL8_F90;

		s2	= IEEE_128_64_MANT2;
		f.whole = x;

		/* move one bit in the correct direction.
		 ** Because of the way the implicit bit works,
		 ** the exponent field is handled correctly.
		 */
		if (x > 0 ) {
			s3	= -(LL_CONST(0x1));
			s4	= LL_CONST(0x1);
		} else {
			s3	= LL_CONST(0x1);
			s4	= -(LL_CONST(0x1));
		}

		rslt.ui[1] += (x > s) ? s3 : s4;

		if ((f.ui[1] == s2) || (rslt.ui[1] == s2)) {
				rslt.ui[0] += (x > s) ? s3 : s4;
		}

		if (isnormal128(rslt.whole))
			return rslt.whole;

		/*
		 * Raise overflow exception for infinite result and
		 * underflow exception for too small result.  Raise
		 * inexact exception for both cases. Allow subnormal
		 * values to return without exception.
		 */
		f.whole =	rslt.whole;
		if (((f.ui[0] &= ~IEEE_128_64_SIGN_BIT) ==
		   IEEE_128_64_EXPO) && (f.ui[1] == 0)) {
			result =	_raisovfl8(ovfl8);
		} else if (rslt.whole == 0) {
			result =	_raisunfl8(unfl8, ovfl8);
		} else {
			return (rslt.whole);
		}
		result =	_raisinexct8(arg1, arg2);
		return (rslt.whole);
	}
}

_f_real16
_IEEE_NEXT_AFTER_D_H(_f_real16 x, _f_real4 s)
{
#if defined(_WORD32)
	union ldble_float {
		_f_real16		whole;
		unsigned long long	ui[2];
	} f,rslt;
	unsigned long long	s2, s3, s4;
#else
	union ldble_float {
		_f_real16		whole;
		unsigned long 		ui[2];
	} f,rslt;
	unsigned long 		s2, s3, s4;
#endif

	if (isnan128(x)) {
		return x;
	}else if (isnan32(s)) {
		return _DBL_NaN;
	}
	f.whole =	x;
	rslt.whole =	x;
	if ((((f.ui[0] &= ~IEEE_128_64_SIGN_BIT) == IEEE_128_64_EXPO) &&
	   (f.ui[1] == 0)) || (x == s)) {
		return x;
	} else if (rslt.whole == 0.0) {
		rslt.whole = TINY_REAL16_F90;
		if ((_f_real4) x > s)
			rslt.ui[0] |= IEEE_128_64_SIGN_BIT;
		return rslt.whole;
	} else {
		_f_real8	result = 	0;
		_f_real8	arg1 =	1.0;
		_f_real8	arg2 =	10.0;
		_f_real8	unfl8 =	TINY_REAL8_F90;
		_f_real8	ovfl8 =	HUGE_REAL8_F90;
		_f_real16	hold16;

		s2	= IEEE_128_64_MANT2;
		f.whole = x;
		hold16	= (_f_real16) s;

		if (x > 0) {
			s3	= -(LL_CONST(0x1));
			s4	= LL_CONST(0x1);
		} else {
			s3	= LL_CONST(0x1);
			s4	= -(LL_CONST(0x1));
		}

		rslt.ui[1] += (x > hold16) ? s3 : s4;

		if ((f.ui[1] == s2) || (rslt.ui[1] == s2)) {
			rslt.ui[0] += (x > hold16) ? s3: s4;
		}

		if (isnormal128(rslt.whole))
			return rslt.whole;

		/*
		 * Raise overflow exception for infinite result and
		 * underflow exception for too small result.  Raise
		 * inexact exception for both cases. Allow subnormal
		 * values to return without exception.
		 */
		f.whole =	rslt.whole;
		if (((f.ui[0] &= ~IEEE_128_64_SIGN_BIT) ==
		   IEEE_128_64_EXPO) && (f.ui[1] == 0)) {
			result =	_raisovfl8(ovfl8);
		} else if (rslt.whole == 0) {
			result =	_raisunfl8(unfl8, ovfl8);
		} else {
			return (rslt.whole);
		}
		result =	_raisinexct8(arg1, arg2);
		return (rslt.whole);
	}
}

_f_real16
_IEEE_NEXT_AFTER_D_R(_f_real16 x, _f_real8 s)
{
#if defined(_WORD32)
	union ldble_float {
		_f_real16		whole;
		unsigned long long	ui[2];
	} f,rslt;
	unsigned long long	s2, s3, s4;
#else
	union ldble_float {
		_f_real16		whole;
		unsigned long 		ui[2];
	} f,rslt;
	unsigned long 		s2, s3, s4;
#endif

	if (isnan128(x)) {
		return x;
	}else if (isnan64(s)) {
		return _DBL_NaN;
	}
	f.whole =	x;
	rslt.whole =	x;
	if ((((f.ui[0] &= ~IEEE_128_64_SIGN_BIT) == IEEE_128_64_EXPO) &&
	   (f.ui[1] == 0)) || (x == s)) {
		return x;
	} else if (rslt.whole == 0.0) {
		rslt.whole = TINY_REAL16_F90;
		if ((_f_real8) x > s)
			rslt.ui[0] |= IEEE_128_64_SIGN_BIT;
		return rslt.whole;
	} else {
		_f_real8	result = 	0;
		_f_real8	arg1 =	1.0;
		_f_real8	arg2 =	10.0;
		_f_real8	unfl8 =	TINY_REAL8_F90;
		_f_real8	ovfl8 =	HUGE_REAL8_F90;
		_f_real16	hold16;

		s2	= IEEE_128_64_MANT2;
		f.whole	= x;
		hold16	= (_f_real16) s;

		if (x > 0) {
			s3	= -(LL_CONST(0x1));
			s4	= LL_CONST(0x1);
		} else {
			s3	= LL_CONST(0x1);
			s4	= -(LL_CONST(0x1));
		}

		rslt.ui[1] += (x > hold16) ? s3 : s4;

		if ((f.ui[1] == s2) || (rslt.ui[1] == s2)) {
			rslt.ui[0] += (x > hold16) ? s3 : s4;
		}
		if (isnormal128(rslt.whole))
			return rslt.whole;

		/*
		 * Raise overflow exception for infinite result and
		 * underflow exception for too small result.  Raise
		 * inexact exception for both cases. Allow subnormal
		 * values to return without exception.
		 */
		f.whole =	rslt.whole;
		if (((f.ui[0] &= ~IEEE_128_64_SIGN_BIT) ==
		   IEEE_128_64_EXPO) && (f.ui[1] == 0)) {
			result =	_raisovfl8(ovfl8);
		} else if (rslt.whole == 0) {
			result =	_raisunfl8(unfl8, ovfl8);
		} else {
			return (rslt.whole);
		}
		result =	_raisinexct8(arg1, arg2);
		return (rslt.whole);
	}
}

#ifdef _F_REAL4
_f_real4
_IEEE_NEXT_AFTER_H_D(_f_real4 x, _f_real16 s)
{
	REGISTER_4 s1, s2;
	if (isnan32(x)) {
		return x;
	} else if (isnan128(s)) {
		return _HALF_NaN;
	}
	s1.f = x;
	if (((s1.ui &= ~IEEE_32_SIGN_BIT) == IEEE_32_INFINITY) ||
	    (x == s)) {
		return x;
	} else if (s1.f == 0.0) {
		s1.f = TINY_REAL4_F90;
		if ((_f_real16) x > s)
			s1.ui |= IEEE_32_SIGN_BIT;
		return s1.f;
	} else {
		_f_real8	result =	0;
		_f_real8	arg1 =	1.0;
		_f_real8	arg2 =	10.0;
		_f_real8	unfl8 =	TINY_REAL8_F90;
		_f_real8	ovfl8 =	HUGE_REAL8_F90;
		_f_real16	hold16;

		s1.f = x;

		hold16 = (_f_real16) x;

		if (x > 0 ) {
			s1.ui += (hold16 > s) ? -(0x1) : 0x1;
		} else {
			s1.ui += (hold16 > s) ? 0x1 : -(0x1);
		}

		if (isnormal32(s1.ui))
			return s1.f;
		/*
		 * Raise overflow exception for infinite result and
		 * underflow exception for too small result.  Raise
		 * inexact exception for both cases. Allow subnormal
		 * values to return without exception.
		 */
		s2.f = s1.f;
		if ((s2.ui &= ~IEEE_32_SIGN_BIT) == IEEE_32_INFINITY) {
			result =	_raisovfl8(ovfl8);
		} else if (s1.f == 0) {
			result =	_raisunfl8(unfl8, ovfl8);
		} else {
			return (s1.f);
		}
		result =	_raisinexct8(arg1, arg2);
		return (s1.f);
	}
}
#endif

_f_real8
_IEEE_NEXT_AFTER_R_D(_f_real8 x, _f_real16 s)
{
	REGISTER_8 s1, s2;
	if (isnan64(x)) {
		return x;
	} else if (isnan128(s)) {
		return _SGL_NaN;
	}
	s1.f = x;
	if (((s1.ui &= ~IEEE_64_SIGN_BIT) == IEEE_64_INFINITY) ||
	    (x == s)) {
		return x;
	} else if (s1.f == 0.0) {
		s1.f = TINY_REAL8_F90;
		if ((_f_real16) x > s)
			s1.ui |= IEEE_64_SIGN_BIT;
		return s1.f;
	} else {
		_f_real8	result =	0;
		_f_real8	arg1 =	1.0;
		_f_real8	arg2 =	10.0;
		_f_real8	unfl8 =	TINY_REAL8_F90;
		_f_real8	ovfl8 =	HUGE_REAL8_F90;
		_f_real16	hold16;

		s1.f = x;

		hold16 = (_f_real16) x;

		if (x > 0 ) {
			s1.ui += (hold16 > s) ?
				-(LL_CONST(0x1)) : LL_CONST(0x1);
		} else {
			s1.ui += (hold16 > s) ?
				LL_CONST(0x1) : -(LL_CONST(0x1));
		}

		if (isnormal64(s1.ui))
			return s1.f;

		/*
		 * Raise overflow exception for infinite result and
		 * underflow exception for too small result.  Raise
		 * inexact exception for both cases. Allow subnormal
		 * values to return without exception.
		 */
		s2.f = s1.f;
		if ((s2.ui &= ~IEEE_64_SIGN_BIT) == IEEE_64_INFINITY) {
			result =	_raisovfl8(ovfl8);
		} else if (s1.f == 0) {
			result =	_raisunfl8(unfl8, ovfl8);
		} else {
			return (s1.f);
		}
		result =	_raisinexct8(arg1, arg2);
		return (s1.f);
	}
}

#endif
