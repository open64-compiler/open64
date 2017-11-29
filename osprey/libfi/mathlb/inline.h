/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
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


/* USMID @(#) libfi/mathlb/inline.h	92.0	10/08/98 14:37:14 */

#ifndef INLINE_H
#define INLINE_H

#include <fortran.h>

#ifdef _UNICOS
#define LL_CONST(c) c ## L
#else
#define LL_CONST(c) c ## LL
#endif

#if defined(__mips)
typedef union {
	_f_real4		f;
	unsigned int		ui;
	_f_int4			i;
}               REGISTER_4;

typedef union {
	_f_real8		f;
	unsigned long long	ui;
	_f_int8			i;
}               REGISTER_8;

typedef union {
	_f_real16		f;
	unsigned long long	ui[2];
	_f_int8			i[2];
}               REGISTER_16;

#elif defined(_WORD32)
typedef union {
	_f_real4		f;
	unsigned int		ui;
	_f_int4			i;
}               REGISTER_4;

typedef union {
	_f_real8		f;
	unsigned long long	ui;
	_f_int8			i;
}               REGISTER_8;

typedef union {
	_f_real16		f;
	unsigned long long	ui[2];
	_f_int8			i[2];
}               REGISTER_16;
#elif defined (_CRAY1) && defined(_CRAYIEEE)
typedef union {
	_f_real4		f;
	unsigned long		ui;
	_f_int4			i;
}		REGISTER_4;
typedef union {
	_f_real8		f;
	unsigned long		ui;
	_f_int8			i;
}		REGISTER_8;
typedef union {
	_f_real16		f;
	unsigned long 		ui[2];
	_f_int8			i[2];
}		REGISTER_16;
#else
#ifdef _F_REAL4
typedef union {
	_f_real4		f;
	unsigned short		ui;
	_f_int4			i;
}		REGISTER_4;
#endif  /* _F_REAL4 */
typedef union {
	_f_real8		f;
	unsigned long		ui;
	_f_int8			i;
}		REGISTER_8;
#if _F_REAL16 == 1
typedef union {
	_f_real16		f;
	unsigned long 		ui[2];
	_f_int8			i[2];
}		REGISTER_16;
#endif  /* _F_REAL16 */
#endif  /* else of __mips */

/**
 * Single-precision (32-bit) IEEE Storage Format
 * |                                |
 * | s| exp[30:23] | fraction[22:0] |
 * |31|32        23|22             0|
 *                                        s   e - 127
 * Normalized value (0 < e < 255)     (-1) * 2        * 1.f
 *
 *                                        s   -126
 * Subnormal value (e = 0)            (-1) * 2        * 0.f
 *
 * The difference between a normal number and a subnormal number is that the
 * implicit leading bit of a normal number is 1, and the leading bit of a
 * subnormal number is 0.
 **
 * double-precision (64-bit) Storage Format
 * |                                 |                |
 * | s| exp[52:62] | fraction[51:32] | fraction[31:0] |
 * |63|62        52|51             32|31             0|
 *                                        s   e - 1023
 * Normalized value (0 < e < 2047)     (-1) * 2        * 1.f
 *
 *                                        s   -1022
 * Subnormal value (e = 0)            (-1) * 2        * 0.f
 **
 * IEEE 128-bit quad-precision Storage Format
 * |                                |             |             |           |
 * |s  | exp[126:112]| frac[111:96] | frac[95:64] | frac[63:32] | frac[31:0]|
 * |127|126       112|111         96|95         64|63         32|31        0|
 *                                        s   e - 16383
 * Normalized value (0 < e < 32767)     (-1) * 2        * 1.f
 *
 *                                        s   -16382
 * Subnormal value (e = 0)            (-1) * 2        * 0.f
 *
 **
 *
 * Double double 128-bit quad-precision Storage Format
 * |                                 |                |
 * | s| exp[52:62] | fraction[51:32] | fraction[31:0] |
 * |63|62        52|51             32|31             0|
 *
 * |                                 |                |
 * | s| exp[52:62] | fraction[51:32] | fraction[31:0] |
 * |63|62        52|51             32|31             0|
 *
 *                                        s   e - 1023
 * Normalized value (0 < e < 2047)     (-1) * 2        * 1.f
 *
 *                                        s   -1022
 * Subnormal value (e = 0)            (-1) * 2        * 0.f
 **
 **/

/*
 * quad precision constants (128-bit)
 */

/* these require a 128-bit integer constant
 * #define IEEE_128_SIGN_BIT        (LL_CONST(0X80000000000000000000000000000000))
 * #define IEEE_128_EXPONENT        (LL_CONST(0X7FFF0000000000000000000000000000))
 * #define IEEE_128_MANTISSA        (LL_CONST(0X0000FFFFFFFFFFFFFFFFFFFFFFFFFFFF))
 * #define IEEE_128_IMPLICIT_BIT    (LL_CONST(0X00010000000000000000000000000000))
 * #define IEEE_128_MANT_MAX        (LL_CONST(0XFFFFFFFFFFFFFFFFFFFFFFFFFFFFF))
 * #define IEEE_128_EXPO_ALL_ONES(X) (((X)&IEEE_128_EXPONENT)==IEEE_128_EXPONENT)
 * #define IEEE_128_ABS_VALUE       (LL_CONST(0X7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF))
 * Value representing positive infinity *
 * Exponent all 1's, mantissa all zeros *
 * #define IEEE_128_INFINITY        (LL_CONST(0X7FFF0000000000000000000000000000))
 */

#if _F_REAL16 == 1
#define IEEE_128_EXPO_BIAS       16383 
#define IEEE_128_EXPO_BITS       15
#define IEEE_128_EXPO_MAX        0X7FFF
#define IEEE_128_MANT_BITS       112
/* Value representing 64-bit form of 128-bit mantissa */
#define IEEE_128_64_MANT1        (LL_CONST(0X0000FFFFFFFFFFFF))
#define IEEE_128_64_MANT2        (LL_CONST(0XFFFFFFFFFFFFFFFF))
/* Value representing 64-bit form of 128-bit exponent mask */
#define IEEE_128_64_EXPO         (LL_CONST(0X7FFF000000000000))
/* Value representing 64-bit form of 128-bit sign_bit mask */
#define IEEE_128_64_SIGN_BIT     (LL_CONST(0X8000000000000000))
/* Value representing 64-bit form of 128-bit implicit_bit mask */
#define IEEE_128_64_IMPLICIT_BIT (LL_CONST(0X0001000000000000))
#define IEEE_128_64_EXPO_ALL_ONES(X) (((X)&IEEE_128_64_EXPO)==IEEE_128_64_EXPO)
#define IEEE_128_MANT_BTS_UP1   16
#define IEEE_128_MANT_BTS_UP2   32
#define IEEE_128_MANT_BTS_LO1   32
#define IEEE_128_MANT_BTS_LO2   32
#endif  /* _F_REAL16 */

#ifdef	__mips
#define DBL_DBL_MANT_BITS       107
/* use 64-bit defines for double double as _f_real16 */
#endif	/* __mips */

/*
 * double precision constants (64-bit)
 */
#ifndef IEEE_64_SIGN_BIT
#define IEEE_64_SIGN_BIT        (LL_CONST(0X8000000000000000))
#endif
#ifndef IEEE_64_EXPONENT
#define IEEE_64_EXPONENT        (LL_CONST(0X7FF0000000000000))
#endif
#ifndef IEEE_64_MANTISSA
#define IEEE_64_MANTISSA        (LL_CONST(0X000FFFFFFFFFFFFF))
#endif
#ifndef IEEE_64_IMPLICIT_BIT
#define IEEE_64_IMPLICIT_BIT    (LL_CONST(0X0010000000000000))
#endif
#ifndef IEEE_64_EXPO_BIAS
#define IEEE_64_EXPO_BIAS       1023 
#endif
#ifndef IEEE_64_EXPO_BITS
#define IEEE_64_EXPO_BITS       11
#endif
#ifndef IEEE_64_EXPO_MAX
#define IEEE_64_EXPO_MAX        0X7FF
#endif
#ifndef IEEE_64_MANT_BITS
#define IEEE_64_MANT_BITS       52
#endif
#ifndef IEEE_64_MANT_MAX
#define IEEE_64_MANT_MAX        0XFFFFFFFFFFFFF
#endif
#ifndef IEEE_64_EXPO_ALL_ONES
#define IEEE_64_EXPO_ALL_ONES(X) (((X)&IEEE_64_EXPONENT)==IEEE_64_EXPONENT)
#endif
#ifndef IEEE_64_ABS_VALUE
#define IEEE_64_ABS_VALUE       (LL_CONST(0X7FFFFFFFFFFFFFFF))
#endif
/* Value representing positive infinity */
/* Exponent all 1's, mantissa all zeros */
#define IEEE_64_INFINITY        (LL_CONST(0X7FF0000000000000))
#define IEEE_64_MANT_BTS1       20
#define IEEE_64_MANT_BTS2       32

/*
 * single precision constants (32-bit)
 */
#define IEEE_32_SIGN_BIT        0X80000000
#define IEEE_32_EXPONENT        0X7F800000
#define IEEE_32_EXPO_RIGHT      0XFF
#define IEEE_32_MANTISSA        0X007FFFFF
#define IEEE_32_IMPLICIT_BIT    0X00800000
#define IEEE_32_EXPO_BIAS       127
#define IEEE_32_EXPO_BITS       8
#define IEEE_32_EXPO_MAX        0X7F8
#define IEEE_32_MANT_BITS       23
#define IEEE_32_MANT_MAX        0X7FFFFF
#define IEEE_32_EXPO_ALL_ONES(X) (((X)&IEEE_32_EXPONENT)==IEEE_32_EXPONENT)
#define IEEE_32_ABS_VALUE       0X7FFFFFFF
/* Value representing positive infinity */
/* Exponent all 1's, mantissa all zeros */
#define IEEE_32_INFINITY        0x7F800000

/* Union defined to work with IEEE 32 bit floating point. */
#if defined (_WORD32) || ((_CRAY1) && defined(_CRAYIEEE)) || defined(__mips)
union _ieee_f {
   _f_real4 dwd;
   _f_int4 lwd;
   struct {
#if __BYTE_ORDER == __LITTLE_ENDIAN
     unsigned int manta : IEEE_32_MANT_BITS;
     unsigned int expt : IEEE_32_EXPO_BITS;
     unsigned int sign : 1;
#else
     unsigned int sign : 1;
     unsigned int expt: IEEE_32_EXPO_BITS;
     unsigned int manta : IEEE_32_MANT_BITS;
#endif
   } pts;
};
#else
#ifdef _F_REAL4
union _ieee_f {
   _f_real4 dwd;
   _f_int4 lwd;
   struct {
#if __BYTE_ORDER == __LITTLE_ENDIAN
     unsigned int manta : IEEE_32_MANT_BITS;
     unsigned int expt : IEEE_32_EXPO_BITS;
     unsigned int sign : 1;
#else
     unsigned int sign : 1;
     unsigned int expt : IEEE_32_EXPO_BITS;
     unsigned int manta : IEEE_32_MANT_BITS;
#endif
   } pts;
};
#endif        /* end of _F_REAL4 */
#endif       /* end of _WORD32 or (CRAY1 and _CRAYIEEE) */

/* Union defined to work with IEEE 64 bit floating point. */
union _ieee_d {
   _f_real8 dwd;
   _f_int8 lwd;
   struct {
#if __BYTE_ORDER == __LITTLE_ENDIAN
     unsigned int manta2 : IEEE_64_MANT_BTS2;
     unsigned int manta1 : IEEE_64_MANT_BTS1;
     unsigned int expt : IEEE_64_EXPO_BITS;
     unsigned int sign : 1;
#else
     unsigned int sign : 1;
     unsigned int expt : IEEE_64_EXPO_BITS;
     unsigned int manta1 : IEEE_64_MANT_BTS1;
     unsigned int manta2 : IEEE_64_MANT_BTS2;
#endif
   } pts;
};

#if _F_REAL16 == 1
/* Union defined to work with IEEE 64 bit floating point. */
union _ieee_q {
   _f_real16 dwd;
   _f_int8 lwd[2];
   struct {
#if __BYTE_ORDER == __LITTLE_ENDIAN
     unsigned int manta4 : IEEE_128_MANT_BTS_LO2;
     unsigned int manta3 : IEEE_128_MANT_BTS_LO1;
     unsigned int manta2 : IEEE_128_MANT_BTS_UP2;
     unsigned int manta1 : IEEE_128_MANT_BTS_UP1;
     unsigned int expt : IEEE_128_EXPO_BITS;
     unsigned int sign : 1;
#else
     unsigned int sign : 1;
     unsigned int expt : IEEE_128_EXPO_BITS;
     unsigned int manta1 : IEEE_128_MANT_BTS_UP1;
     unsigned int manta2 : IEEE_128_MANT_BTS_UP2;
     unsigned int manta3 : IEEE_128_MANT_BTS_LO1;
     unsigned int manta4 : IEEE_128_MANT_BTS_LO2;
#endif
   } pts;
};
#endif	/* _F_REAL16 */

/* Macros which evaluate to a nonzero integer expression if its argument
 * value is normal: neither zero, subnormal, infinite, or NaN.
 *   Test if exponent is not All-1's or zero
 */
#ifndef	__mips
#if _F_REAL16 == 1
#define isnormal128(x) __is_normal128(x)
static _f_int8 __is_normal128(x)
_f_real16 x;
{
	union _ieee_q f;
	f.dwd = x;
	return((!(f.pts.expt == IEEE_128_EXPO_MAX)) &&
		(f.pts.expt != 0));
}
#endif  /* _F_REAL16 */
#else	/* NOT __mips */
#define isnormal128(x) __is_normal128(x)
static _f_int8 __is_normal128(x)
_f_real16 x;
{
	union _ieee_q f;
	f.dwd = x;
	if (IEEE_64_EXPO_ALL_ONES(f.lwd[0]) || IEEE_64_EXPO_ALL_ONES(f.lwd[1]))
		return 0;
	if (f.dwd == 0)
		return 0;
	return 1;
}
#endif	/* NOT __mips */

#define isnormal64(x) __is_normal64(x)
static _f_int8 __is_normal64(x)
#if defined(_WORD32) || defined(__mips)
unsigned long long x;
#else	/* _WORD32 or mips */
unsigned long x;
#endif	/* _WORD32 or mips */
{
   return(!IEEE_64_EXPO_ALL_ONES(x) && (x&IEEE_64_EXPONENT)!=0);
}

#ifdef  _F_REAL4
#define isnormal32(x) __is_normal32(x)
static _f_int4 __is_normal32(x)
#if defined(__mips)
unsigned int x;
#elif defined(_WORD32)
unsigned long x;
#else	/* _WORD32 */
unsigned int x;
#endif	/* __mips */
{
   return(!IEEE_32_EXPO_ALL_ONES(x) && (x&IEEE_32_EXPONENT)!=0);
}
#endif  /* _F_REAL4 */

/* Macros which evaluate to a nonzero integer expression if its
 * argument value is a NaN.
 *   Test if exponent All-1's and Fraction non-zero
 * Since NaNs are signaling, cannot do floating point operations on NaN
 */
#ifdef  _F_REAL4
#define isnan32(x) __is_nan32(x)
static _f_int4 __is_nan32(x)
_f_real4 x;
{
	union _ieee_f f;
	f.dwd	= x;
	return((f.pts.expt == IEEE_32_EXPO_RIGHT) &&
		(f.pts.manta != 0));
}
#ifdef  _CRAYMPP
#include "leadz.h"
static int
__ieee_real4i(_f_real4 x)
{
	int		i;
	REGISTER_4	s1, s2;
	s1.f	= x;

	/* clear sign bit. */
	s1.ui &= ~IEEE_32_SIGN_BIT;

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
	return ((int) s1.i);
}
#endif  /* _CRAYMPP */
#endif  /* _F_REAL4 */

#define isnan64(x) __is_nan64(x)
static _f_int8 __is_nan64(x)
_f_real8 x;
{
	union _ieee_d f;
	f.dwd	= x;
	return((f.pts.expt == IEEE_64_EXPO_MAX) &&
		((f.pts.manta1 != 0) || (f.pts.manta2 != 0)));
}

#if _F_REAL16 == 1
#define isnan128(x) __is_nan128(x)
static _f_int8 __is_nan128(x)
_f_real16 x;
{
	union _ieee_q f;
	f.dwd =	x;
	return((f.pts.expt == IEEE_128_EXPO_MAX) &&
		((f.pts.manta1 != 0) || (f.pts.manta2 != 0) ||
		(f.lwd[1] !=0)));
}
#endif  /* _F_REAL16 */

#ifdef  _F_REAL4
#define isfinite32(x) __is_finite32(x)
static _f_int4 __is_finite32(xarg)
#if defined(__mips)
unsigned int xarg;
#elif defined(_WORD32)
unsigned long xarg;
#else	/* _WORD32 */
unsigned int xarg;
#endif	/* __mips */
{
   return(!IEEE_32_EXPO_ALL_ONES(xarg));
}
#endif  /* _F_REAL4 */

#define isfinite64(x) __is_finite64(x)
static _f_int8 __is_finite64(xarg)
#if defined(_WORD32) || defined(__mips)
unsigned long long xarg;
#else	/* _WORD32 or __mips */
unsigned long xarg;
#endif	/* _WORD32 or __mips */
{
   return(!IEEE_64_EXPO_ALL_ONES(xarg));
}

#if _F_REAL16 == 1
/* macro which evaluates to nonzero int expression if its argument value
 * is finite: either zero, normal, or subnormal. */
/* Test if exponent not all 1's  - all 1's mean NaN or infinity */
#define isfinite128(x) __is_finite128(x)
static _f_int8 __is_finite128(xarg)
#if defined(_WORD32) || defined(__mips)
unsigned long long xarg;
#else	/* _WORD32 or __mips */
unsigned long xarg;
#endif	/* _WORD32 or __mips */
{
   return(!IEEE_128_64_EXPO_ALL_ONES(xarg));
}
#endif  /* _F_REAL16 */

#ifdef	__mips
typedef  union {
        struct {
                unsigned  sign  :1;
                unsigned  exp   :11;
                unsigned  hi    :20;
                unsigned  lo    :32;
        } fparts;
        struct {
                unsigned  sign  :1;
                unsigned  exp   :11;
                unsigned  qnan_bit      :1;
                unsigned  hi    :19;
                unsigned  lo    :32;
        } nparts;
        struct {
                unsigned hi;
                unsigned lo;
        } fwords;
        double  d;
} _dval;

typedef  union {
        struct {
                _dval   hi;
                _dval   lo;
        } qparts;
        struct {
                unsigned long long hi;
                unsigned long long lo;
        } fwords;
        long double     ldbl;
} _ldblval;

static _f_real16 _get_frac_and_exp(_f_real16 d, _f_int4 *e)
{
	_f_int4		expon;
	_f_real8	scalefactor;
	_f_int4		exponent;
	_f_int4		new_low_exponent;
	_ldblval	v;
	if (d == 0.0) {
		*e = 0;
		return (0.0L);
	}
	v.ldbl = d;
	exponent	= v.qparts.hi.fparts.exp;
	*e	= exponent - 1022;

	/* Scale down the parts so the high part has exponent 1022 */
	v.qparts.hi.fparts.exp	= 1022;
	if ( v.qparts.lo.d != 0.0) {
		new_low_exponent =
		   ((int) v.qparts.lo.fparts.exp + (1022-exponent));
		if (new_low_exponent <= 0) {
			v.qparts.lo.d	= 0.0;
		} else {
			v.qparts.lo.fparts.exp	= new_low_exponent;
		}
	}
	return (v.ldbl);
}
#endif	/* __mips */

#endif	/* !INLINE_H */
