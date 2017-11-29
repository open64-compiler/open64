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


/* USMID @(#) libu/numconv/mpp/qq_routines.h	92.3	10/28/99 13:52:51 */


#if defined(_WORD32) || defined(__mips) || defined(_LITTLE_ENDIAN)
/* Implementation's most efficient floating types */
/* These are defined in fp.h for CRAYIEEE systems */
#if ! defined(BUILD_OS_DARWIN)
/* Darwin, we get these definitions for free from math.h, though they may
 * not be what we'd like (e.g. -march=i386 uses long double). */
typedef double          float_t;   /* >=24 bit real */
typedef double          double_t;  /* >=48 bit real */
#endif /* ! defined(BUILD_OS_DARWIN) */

typedef unsigned long long      uint64;
typedef          long long      int64;
typedef unsigned int           uint32;
typedef          int           int32;

typedef double		ieee64_t;
typedef ieee64_t	FLOAT64;
typedef float		ieee32_t;
typedef ieee32_t	FLOAT32;

/* fp.h contains definitions of these functions for PVP and MPP. */

#ifdef	_ABSOFT
#define scalb	ldexp
#define logb	ilogb
int ilogb();
#elif defined(__mips) || defined(_LITTLE_ENDIAN)
#define SCALB(x,i)	_IE_SCALB(x,i)
#define LOGB(x)		_IE_LOGB(x)
#define scalb		_IE_SCALB
#define logb		_IE_LOGB
static FLOAT64		_IE_SCALB(FLOAT64 x, int n);
static int		_IE_LOGB(FLOAT64 x);
#else
#define SCALB(x,i)	scalbn(x,i)
#define LOGB(x)		ilogb(x)
#define scalb	scalbn
#define logb	ilogb
FLOAT64		scalbn(FLOAT64 x, int n);
int		ilogb(FLOAT64 x);
FLOAT64 copysign(FLOAT64 x, FLOAT64 y);
#endif

#if ! defined(BUILD_OS_DARWIN)
/* The following defines are in fp.h for PVP. */
/* number classifications */
#define FP_NAN		0
#define FP_INFINITE	1
#define FP_NORMAL	2
#define FP_SUBNORMAL	3
#define FP_ZERO		4
#endif /* ! defined(BUILD_OS_DARWIN) */

#if defined(_SOLARIS) || defined(__mips) || defined(_LITTLE_ENDIAN) || \
    (defined(_ABSOFT) && !defined(_LD64))  /* 128-bit for nonUNICOS */

typedef long double	ieee128_t;
typedef ieee128_t	FLOAT128;

/* SPARC scalbl and logbl are really scalbnl and ilogbl and cf90 2.0
 * intrinsics will be used rather than sparc 128-bit routines.
 */
#define scalbl		_IE_SCALBL
#define logbl		_IE_LOGBL
static FLOAT128	_IE_SCALBL(FLOAT128 x, int n);
static int		_IE_LOGBL(FLOAT128 x);
int		ISNANL(FLOAT128 x);

/* SPARC copysignl is not in unbundled library.  Use different macro */
FLOAT128 COPYSIGNL(FLOAT128 x, FLOAT128 y);

/* IEEE-128 floating-point definitions.
 * Definitions in this file assumes that quiet NaNs are not available.
 * Also assume big endian.
 * 
 * pieces of a IEEE 128 bit floating point number.
 * These are in fp.h for PVP.
 * Upper 64-bit pieces of a IEEE 128 bit floating point number
 * IEEE_128_SIGN_BIT		0X8000000000000000
 * IEEE_128_IMPLICIT_BIT	0X0001000000000000
 * IEEE_128_EXPONENT		0X7FFF000000000000
 * IEEE_128_MANTISSA		0X0000FFFFFFFFFFFF
 * IEEE_128_ABS_VALUE		0X7FFFFFFFFFFFFFFF
 * IEEE_128_MANT_MAX		0X0000FFFFFFFFFFFF
 */
#define IEEE_128_SIGN_BIT	( ((uint64)000001) << 63 )
#define IEEE_128_EXPONENT	( ((uint64)0X7FFF) << IEEE_128_MANT_TOP_BITS )
#define IEEE_128_IMPLICIT_BIT	( ((uint64)000001) << IEEE_128_MANT_TOP_BITS )
#define IEEE_128_EXPO_BITS	15
#define IEEE_128_EXPO_MAX	0X7FFF
#define IEEE_128_EXPO_BIAS	0X3FFF
#define IEEE_128_EXPO_ALL_ONES(X) (((X)&IEEE_128_EXPONENT)==IEEE_128_EXPONENT)
#define IEEE_128_ABS_VALUE	( IEEE_128_EXPONENT | IEEE_128_MANTISSA )
#define IEEE_128_TEN_HI32	((uint32)0x40024000)	/* high_32bits(10.) */
#define IEEE_128_ONE_HI64	(((uint64) 0x3fff)<< 48) /* high_64bits(1.) */
#endif	/* end of 128-bit stuff */
#else
/* for TRITON IEEE and MPP IEEE */
typedef long		int64;
typedef unsigned long	uint64;

/* NOTE: union logic such as "tmp.i32[0] < value" is used in the sd2*q
 * output routines, though rarely.  It is used all over for debug print.
 * TRITON IEEE has its own versions of the sd2* and defgu2s routines
 * and mpp does not have REAL*16 I/O.
 * This will have to be fixed if we ever move this code to either.
 *
 * uint32 should be short on the MPP.  That can be changed later.
 */
#if defined(_CRAYMPP)
/* for MPP */
typedef unsigned long	uint32;
typedef 	 long	int32;
#else
/* for TRITON IEEE */
typedef unsigned long	uint32;
typedef 	 long	int32;
#endif	/* end of 32-bit definition for MPP and TRITON IEEE */

#ifndef _LD64
typedef long double	ieee128_t;
#else
typedef double		ieee128_t;
#endif
typedef ieee128_t	FLOAT128;
typedef double		ieee64_t;
typedef ieee64_t	FLOAT64;
typedef float		ieee32_t;
typedef ieee32_t	FLOAT32;

/* The 64-bit SCALB in fp.h is used and matches the definition of scalbn
 * for sparc.  LOGB in fp.h returns a double.  The result of logb is
 * cast to a long in the routines.  The scalbl and logbl routines are
 * in fp.h.
 */
#endif /* end else for nonUNICOS */


#ifndef IEEE_64_MANTISSA
#define IEEE_64_MANTISSA	((((uint64) 1) << 52) - 1 )
#endif
#define IEEE_128_DEC_OUT_DIGITS	36
#define IEEE_64_DEC_OUT_DIGITS	18
#ifndef _LD64
/* pieces of a IEEE 128 bit floating point number */
#define IEEE_128_MANT_BITS	112
/* #define IEEE_128_MANT_TOP_BITS	(IEEE_128_MANT_BITS-64) */
#define IEEE_128_MANT_TOP_BITS	48
#define IEEE_128_MANTISSA	( IEEE_128_IMPLICIT_BIT-1 )
#define IEEE_128_MANT_MAX	IEEE_128_MANTISSA

/* Union defined to work with IEEE 128 bit floating point. */
union _ieee_quad {
   ieee128_t dword;
   uint64   lword[2];
   struct {
     unsigned int sign : 1;
     unsigned int exponent : IEEE_128_EXPO_BITS;
     uint32 mantissah : (IEEE_128_MANT_TOP_BITS-32);
     uint32 mantissal : 32;
   } parts;
};

#endif /* end _LD64 */


#if defined(_SOLARIS) || defined(__mips) || \
   (defined(_ABSOFT) && !defined(_LD64)) || defined(_LITTLE_ENDIAN)
/* the following macros are defined in fp.h for PVP systems. */

/* fpclassifyl() macro returns above number classification value */
#define fpclassifyl(x) ((sizeof(x)==sizeof(FLOAT128)) ?  __fpclassifyl(x) \
       : printf("%s %d: fpclassifyl() only supported for 128-bit\n", \
		__FILE__, __LINE__))
static int __fpclassifyl(x)
union _ieee_quad x;
{
	if (x.parts.exponent==0) {
		return ((x.parts.mantissah==0 && x.parts.mantissal==0 &&
		   x.lword[1] == 0)
			? FP_ZERO : FP_SUBNORMAL);
	}
	else if (IEEE_128_EXPO_ALL_ONES(x.lword[0])) {
		return ((x.parts.mantissah==0 && x.parts.mantissal==0 &&
		   x.lword[1] == 0)
			? FP_INFINITE : FP_NAN);
	} else {
		return (FP_NORMAL);
	}
}
/* end fpclassifyl macro */


/* signbit() macro in fp.h works on IEEE-128 as well:
 * Macro to an int expression that is non-zero if argument
 * is negative (including infinites, zeros, and NANs).
 * Test if sign bit set.  Use SPARC IEEE signbit on sparc.
 * 
 * define signbit(x) ((*(unsigned long *)&x) >> 63)
 */


/* isinfl() macro evaluates to a nonzero int expression if its argument
 * value is infinite: exponent is all 1's and mantissa is all zero.
 */
#define isinfl(x) __is_infinitel(x)
static int __is_infinitel(x)
union _ieee_quad x;
{
	return( x.lword[1]==0 &&
		(x.lword[0]&IEEE_128_ABS_VALUE)==IEEE_128_EXPONENT);
}
/* end isinfl macro */


/* isfinitel() macro which evaluates to a nonzero int expression if its
 * argument value is finite: either zero, normal, or subnormal.
 * Test if exponent not all 1's  - all 1's mean NaN or infinity.
 */
#define isfinitel(x) __is_finitel(x)
static int __is_finitel(x)
long x;
{
   return(!IEEE_128_EXPO_ALL_ONES(x));
}
/* end isfinitel macro */


/* isnormall() macro evaluates to a nonzero int expression if its
 * argument value is normal: neither zero, subnormal, infinite, or NaN.
 * Test if exponent is not All-1's or zero.
 */
#define isnormall(x) __is_normall(x)
static int __is_normall(x)
long x;
{
	return(!IEEE_128_EXPO_ALL_ONES(x) && (x&IEEE_128_EXPONENT)!=0);
}
/* end isnormall macro */


/* ISNANL() macro evaluates to a nonzero int expression if its argument
 * value is a NaN.
 * Test if exponent All-1's and Fraction non-zero.  Since
 * NaNs are signaling, cannot do any floating point operations on NaN.
 */
#define ISNANL(x) __is_nanl(x)
static int __is_nanl(x)
ieee128_t	x;
{
	union _ieee_quad xi;
	xi.dword = x;
	return( xi.parts.exponent == 0x7fff &&( xi.lword[1] != 0 ||
		xi.parts.mantissal != 0 || xi.parts.mantissah != 0) );
}
/* end ISNANL macro */

/* Use this copysignl function for sparc 128-bit real values.
 * function which produces a value with the magnitude of x and the sign
 * of y
 */
#define COPYSIGNL(x,y) __copy_signl(x,y)
static FLOAT128 __copy_signl(x, y)
FLOAT128	x;
FLOAT128	y;
{
	union _ieee_quad ysav;
	union _ieee_quad ysignx;
	ysignx.dword = x;
	ysav.dword = y;
	ysignx.parts.sign = ysav.parts.sign;
	return (ysignx.dword);
}

#endif /* end SOLARIS, mips, or ABSOFT for 128-bit functions */


/* qqroutines.c routines 
 *
 * scale qdatum into [1.0,20.0)
 * set scaleoverten if scale result>=10.0 .
 * set power_of_ten to the power of the divisor needed to
 * reach [1.0,10.0).
 */
void _qqscale20(
		FLOAT128 *qdatum,
		FLOAT128 *qresulthi,
		FLOAT128 *qresultlo,
		int32 *denorm_2_adjust,
		int32 *scaleoverten,
		long *power_of_ten      /* power of ten used */
);

int _qq_power5(int32 * power, FLOAT128 * powerhi, FLOAT128 * powerlo);


/************************************************************
 *  If the rounded number ended up >= 10, the digit output
 *  will compensate, so increment power_of_ten and continue.
 *  G as F needs to know if rounding made it over 10.
 *  Convert to fixed point in qq_as64[].
 ************************************************************/

/* QQCONVERT2AS64 macro: */
#define QQCONVERT2AS64( qpower, qq_as64, power_of_ten, scaleoverten, roundto10)\
	/* Check if .ge. 10.0L (ieee128_t) after round and not before */ \
	roundto10 = (qpower.i32[0] >= IEEE_128_TEN_HI32 ) & ~ scaleoverten; \
	power_of_ten += roundto10; \
	scaleoverten |= roundto10; \
        _qq_putdigits_init( &qpower.q[0], &qq_as64[0] ); \
/* end QQCONVERT2AS64 macro */


void _qq_putdigits_init( const FLOAT128 * qq_q, FLOAT64 * t);

void _qq_putdigits( FLOAT64 * t, const long * limit, long * digits_left,
	long ** position_pointer);

#define PUTDIGITLOOP( qq_as64, limit, position_pointer, digits_left ) \
_qq_putdigits( &qq_as64[0], limit, &digits_left, &position_pointer); \

/* end PUTDIGITLOOP macro */


/* DEBUGQ() macro */
#define DEBUGQ( format, qvalue) { \
        union{ FLOAT128 q; int64 i64[2]; int32 i32[4]; }tmpv; \
        if(DEBUGSW) { \
                tmpv.q = qvalue; \
                printf(format, \
                tmpv.i32[0], tmpv.i32[1], tmpv.i32[2], tmpv.i32[3]); \
         } \
} \
/* end  DEBUGQ macro */


/* QADEBUG() macro */
#define QADEBUG(strid,ndim,format,arrray) \
        if(DEBUGSW){ \
                register int j;  \
                printf("%s[0:%d]=" , strid,ndim-1) ; \
                for(j=0; j<ndim; j++ )  { \
                        printf( format, (uint32) arrray[j] ) ; \
                } \
                printf("\n") ; \
        } \
/* end  QADEBUG macro */


#if defined(_WORD32) || defined(__mips) || defined(_LITTLE_ENDIAN)
#define DEBUGPRTHEX64(tmpxx) printf( "%08x%08x", tmpxx.i32[0], tmpxx.i32[1]);
#else
#define DEBUGPRTHEX64(tmpxx) printf( "%16x", tmpxx.i64 );
#endif /* end _WORD32 */


/* DEBUGPRT64() macro */
#define DEBUGPRT64(msgh, msgl, datumh, datuml) \
if( DEBUGSW !=0) { \
		union{ FLOAT64 d; uint64 i64; uint32 i32[2]; } tmpxx;  \
		tmpxx.d = datumh; \
		printf( "\n%s: %29.17e ", msgh,  datumh); \
		DEBUGPRTHEX64( tmpxx) \
		tmpxx.d = datuml; \
		printf( "\n%s: %29.17e ", msgl,  datuml); \
		DEBUGPRTHEX64( tmpxx) \
} /* end DEBUGPRT64 macro */


/*
 * output the exponent field.  Common to all FP output routines.
 */
int _outputexponent(
	long	power10,
	long	*position_pointer,
	long	exponent_field_size,
	const long	*e);

void _qqfloat_uint250( FLOAT128 * x, int * inexact,
	const int64 * low_number,  const int64 * mid1_number,
	const int64 * mid2_number, const int64 * mid3_number,
	const int64 * high_number);

void _qq_addunsigned(
	FLOAT128 *shi, FLOAT128 *slo,
	FLOAT128 *lhi, FLOAT128 *llo,
	FLOAT128 *qresulthi, FLOAT128 *qresultlo
);

void _qmult3( const int *iround128,
	const FLOAT128 *thi, const FLOAT128 *tlo,
	const FLOAT128 *uhi, const FLOAT128 *ulo,
	FLOAT128 *vhi, FLOAT128 *vlo);

#if defined(__mips)
void _m128toi128(FLOAT128 *y, const FLOAT128 *x);
int _i128tom128( FLOAT128 *y, const FLOAT128 *x);
#endif

#if	!defined(_LD64)
#define QINFINITY           IEEE_128_EXPONENT
#define QMANTISSA_BITS      IEEE_128_MANT_BITS
#define QMANTISSA_TOPBITS   IEEE_128_MANT_TOP_BITS
#define QEXPONE             IEEE_128_EXPO_BIAS
#endif /* end not defined _LD64 */
#define QMAX_EXPONENT   16384   /* 128-bit < 2.0^16384 */
#define QMIN_EXPONENT   -16383  /* 128-bit >= is 2.0^-16382   */

#define QPORTABLE_INFINITY      0x7fff000000000000
#define QTABLELIM       81      /* number of 5^-n entries in _QHIPOWER table */
#define QTABLESTEP      64      /* _QHIPOWER_OF_FIVE_TABLE is 5^(64*j)*/
#define QTABLESTEP1     65              /* 1+QTABLESTEP for scalb */
#define QSTEPSHIFT      6               /* log2(QTABLESTEP) */
#define QSTEPMASK       (QTABLESTEP-1)

/* 10^EXPLIMIT is the maximum 10^i which can be constructed from
 *  multiplying _QHIPOWER_OF_FIVE_TABLE[i/128]*_QPOWER_OF_FIVE_TABLE[i&127].
 *  _QHIPOWER_OF_FIVE_TABLE steps by 128: 5^(-40*128,-3.9*128,...,40*128).
 *  _QPOWER_OF_FIVE_TABLE steps by 1: 5^(-127,...127).
 *  A factor from both tables is used for Enn when |nn|>127.
 *  There is a NaN entry at the end of each.
 *
 *  Smallest  64-bit: 2.^(-1024+2-112)=10^-341.368  (Table max is 400)
 *  Smallest 128-bit: 2.^(-16384+2-112)=10^-4965.79 (Table max is 5247)
 */
#define EXPLIMIT        (QTABLELIM*QTABLESTEP+256)  /* Coarse limit on Ennn */


#ifndef _LD64
#define LOGBL(qhi,intexp) { \
	union{ FLOAT128 q[2]; uint64 i64[4]; } tval; \
	tval.q[0] = qhi ; \
	jintexp = (tval.i64[0]>>IEEE_128_MANT_TOP_BITS) & 0x7fff) -QEXPONE ; \
} /* end LOGBL macro */
#endif /* end not defined _LD64 */


#define HALFPOWER48 (1.0/(65536.0*65536.0*65536.0))
#define HALFPOWER49 (0.5*HALFPOWER48)

/* mod: more accurate addition of quantities.
 * First try : add DBUMP to tableh*datuml
 *   DBUMP 4.0 caused error in 10^22.
 *   1.5* 2**-76 had a failing test case: 0.969760752958975E+13 for Ew.14.
 *   (2^-76+2^-100) * 2**-76 worked for all existing tests.
 *   define DBUMP((2.0+1/(65536.*256.))/(4096.*65536.*65536.*65536.*65536.))
 * final: tablel*datum increased to nearest(+inf) (reduce magnitude if
 *         negative)
 *
 * Worst case analysis of the alogrithm:
 *  datum(presumed exact)	is 53 bits    e.g. <3:-49>
 *  upper.d,lower.d(rounded)	is 53,53 bits e.g. <0:-52> +<-54:-106>
 *  tableh,tablel(rounded)	is 26,53 bits e.g. <0:-25> +<-26:-78>
 *  datumh,datuml(exact)	is 26,26 bits e.g. <3:-22> +<-23:-49>
 *  tableh*datumh   <3:-49> 				exact
 *  tableh*datuml <-22:-74> 				exact
 *  tablel*datum (rounds) is <-22:-74>			error +/-2^-75
 *  above + DBUMP giving <-22:-74> 			error +  2^-74
 *  tableh*datuml + (above)<-22:-74>			error +/-2^-75
 *  above + round (rounded)(rounds)<0:-105> 		error +/-2^-106
 * datuml + 1.5*2^-48 -1.5*2^-48 (rounds)<0:-100> 	error +/-2^-101
 *  so overall error is in [ -2^-100 , 2^-73 +2^-100 )
 *
 * Scale the value such that (1.0 <= value < 16.0).  Choose
 * a scaling power of ten by underestimating the logarithm, base 10.
 * If the result will scale to much over 10 (40% of the time?), correct
 * the estimate. If not, continue.  The routine to output digits takes
 * values in [1.0,20), but the correction factor for rounding down
 * assumes that the final number is less than 16 (no more than 4
 * bits before the decimal).
 *
 * Here x= mantissa -1.0  = scalb(datum, -(long)logb(datum))-1.0 : 
 *      and 1/ln(10) = .4343...  
 * Underestimates of 10^n :
 *       alog10_2*logb(datum)
 *       alog10_2*(logb(datum)+(x-x^2/2)*.4343;
 * This overestimate of 10^n was used before:
 *       alog10_2*logb(datum)+x*0.504); 
 * Other overestimates of 10^n 
 *       alog10_2*logb(datum)+x*.4343...); 
 *       alog10_2*logb(datum)+(x-x^2/2+x^3/3)/ln(10)); 
 * 
 * ALOG10_2 is alog10(2.0) used to (under)estimate log10(2**exponent).
 * Accuracy needed is at least 2*(number of exponent bits).
 */

#define ALOG10_2 ((FLOAT64)0.301029995663981195214)

/* SCALEBYPOWEROFTEN() macro */
#define SCALEBYPOWEROFTEN(datum, datumh, datuml, power_of_ten, denorm_adjust) \
{ \
	power_of_ten = (long) ( ALOG10_2*logb(datum)- ALOG10_2 ); \
	if (datum < 1.0) power_of_ten--; \
	upper.d = _POWER_OF_FIVE_TABLE[2*(-power_of_ten+EXP_LIM)]; \
	lower.d = _POWER_OF_FIVE_TABLE[2*(-power_of_ten+EXP_LIM)+1]; \
	delta = scalb(datum,-power_of_ten); \
	if ( ( delta * upper.d) >= 11.0 ){ \
		power_of_ten++; \
		upper.d = _POWER_OF_FIVE_TABLE[2*(-power_of_ten+EXP_LIM)]; \
		lower.d = _POWER_OF_FIVE_TABLE[2*(-power_of_ten+EXP_LIM)+1]; \
		delta = delta*0.5; \
	}; \
\
	tmp.d = delta; \
	tmp.i = tmp.i & upper_mask1; \
	datumh = tmp.d; \
	datuml = delta - datumh; \
	tmp.i = upper.i & upper_mask1; \
	tableh = tmp.d; \
	tablel = (upper.d - tableh) + lower.d; \
	power_of_ten -= denorm_adjust; \
/* \
 *	calculate [tableh+tablel]*[datumh+datuml] \
 *	(tableh*datuml ++ (tablel*datum+DBUMP) ++ \
 *			tableh*datumh) ++ datuml*tablel); \
 */ \
	tmp.d = tableh*datumh; \
	lower.d = tablel*delta; \
	if (DEBUGSW) \
		if( lower.d < 0.0 ) abort(); \
	if( lower.d > 0.0 ) lower.i += 1; \
	upper.d = tableh*datuml ; \
	delta = upper.d + lower.d; \
	datuml = (upper.d - delta) + lower.d; \
\
	datumh = tmp.d + delta  ; \
	datuml = ((tmp.d - datumh) + delta) + datuml; \
\
	MAKELOWPOSITIVE( datumh, datuml, upper ) \
	scaleoverten = ( datumh >=10.0); \
	power_of_ten += scaleoverten; \
} /* end SCALEBYPOWEROFTEN macro */ 


/* MAKELOWPOSITIVE() macro: */
#define MAKELOWPOSITIVE( datumh, datuml, upper ) \
	if (datuml <0.0) { \
		upper.d = datumh; \
		if( datumh != 0.0 ) upper.i--; \
		datuml = datuml + ( datumh - upper.d) ; \
		datumh = upper.d; \
	} \
/* end MAKELOWPOSITIVE() macro: */


/* DPADD() macro: */
#define DPADD( hi, lo, addin, tmp ) \
	tmp = hi; \
	hi = tmp + ( addin ) ; \
	lo = ( ( tmp - hi ) + ( addin ) ) + lo; \
/* end DPADD macro */


/* Round the scaled number.  Note that it can become >= 10.0 */
#define ROUNDSCALEDNUM( datumh, datuml, power, upper, tableh, tablel, scaleoverten, roundto10) \
{ \
	tableh = scalb(_POWER_OF_FIVE_TABLE[2*((power)+EXP_LIM)  ],(power)-1); \
	tablel = scalb(_POWER_OF_FIVE_TABLE[2*((power)+EXP_LIM)+1],(power)-1); \
	upper.d = tableh + datumh; \
	datuml = (((datumh - upper.d) + tableh) + tablel) + datuml; \
	\
	tablel = 1.5*HALFPOWER48; \
	datuml = ( tablel + datuml ) - tablel; \
	\
	datumh = upper.d + datuml; \
	datuml = (upper.d - datumh) + datuml; \
	MAKELOWPOSITIVE( datumh, datuml, upper ) \
	DEBUGPRT64("after round hi","after round lo",datumh, datuml) \
	roundto10 = ( datumh >= 10.0) - scaleoverten ; \
	scaleoverten = ( datumh >= 10.0) ; \
	power_of_ten += roundto10; \
}/* end ROUNDSCALEDNUM macro */


/* PUTDIGITLOOP64() macro: Output a number scaled to [1.0,20)
 * Use floor when cast does not truncate
 *    *position_pointer++ = ZERO+(long)floor(datumh);
 *  and
 *    tmp.d = datumh - (long)floor(datumh);
 */
#define PUTDIGITLOOP64(datumh, datuml, position_pointer, endposition, tmp, upper, lower, digits_left ) \
{ \
	if ( datumh >= 10.0 ) { \
		*position_pointer++ = ZERO+1; \
		digits_left--; \
		datumh = datumh -10.0; \
	} \
	while (position_pointer < (endposition)) { \
		*position_pointer++ = ZERO+(long)datumh; \
		digits_left--; \
                if( digits_left <= 0 ) datumh= 0.0; \
		tmp.d = datumh - (long)datumh; \
		upper.d = 8.0*tmp.d; \
		lower.d = 2.0*tmp.d; \
		datumh = upper.d+lower.d; \
		datuml = ((upper.d-datumh)+lower.d)+10.0*datuml; \
		upper.d = datumh + datuml; \
		datuml = (datumh - upper.d) + datuml; \
		datumh = (upper.d < 0.0) ? 0.0 : upper.d; \
		MAKELOWPOSITIVE( datumh, datuml, upper ) \
	}; \
}/* end PUTDIGITLOOP64 macro */

#if defined(_WORD32) || defined(__mips) || defined(_LITTLE_ENDIAN) || defined(__sv2)

extern FLOAT64 _IE_SCALB(FLOAT64 orig_number, int orig_scale_factor);
extern int _IE_LOGB(FLOAT64 x);

#ifndef IEEE_64_MAX_EXPONENT
#define IEEE_64_MAX_EXPONENT	2047
#endif

#ifndef IEEE_64_SIGN_BIT
#define IEEE_64_SIGN_BIT	0x8000000000000000LL
#endif

#ifndef IEEE_64_EXPONENT
#define IEEE_64_EXPONENT        0X7FF0000000000000LL
#endif

#ifndef IEEE_64_INFINITY
#define IEEE_64_INFINITY        0X7FF0000000000000LL
#endif

#ifndef IEEE_64_EXPO_BIAS
#define IEEE_64_EXPO_BIAS       1023
#endif

#ifndef IEEE_64_EXPO_BITS
#define IEEE_64_EXPO_BITS       11
#endif

#ifndef IEEE_64_MANT_BITS
#define IEEE_64_MANT_BITS       52
#endif

static int64
_OWN_LEADZ8(uint64 r01)
{
	uint64	r02 = 0, r03 = 0, r04 = 0;
	r02	= r01 >> 32;
	if (r02 > 0)
		r01	= r02;
	else if (r02 == 0)
		r04	= 32;
	r02	= r01 >> 16;
	r03	= r04;
	if (r02 > 0)
		r01	= r02;
	else if (r02 == 0)
		r04	= 16;
	r02	= r01 >> 8;
	r03 |= r04;
	if (r02 > 0)
		r01	= r02;
	else if (r02 == 0)
		r04	= 8;
	r02	= r01 >> 4;
	r03 |= r04;
	if (r02 > 0)
		r01	= r02;
	else if (r02 == 0)
		r04	= 4;
	r02	= r01 >> 2;
	r03 |= r04;
	if (r02 > 0)
		r01	= r02;
	else if (r02 == 0)
		r04	= 2;
	r02	= r01 < 2 ? 1 : 0;
	r01	= r01 < 1 ? 1 : 0;
	r03 |= r04;
	return r03 + r02 + r01;
}

/*** SCALE - return X * b**i	 ***/

static FLOAT64
_IE_SCALB(FLOAT64 orig_number, int orig_scale_factor)
{
	int32	lz, lzm, shift;
	union {FLOAT64 d; uint64 ui; int64 i;} number, scale_factor,
		expon, exponent, orig_mantissa, mantissa, sign_bit;

	/* orig_number is not zero, infinity, or nan, but may be denorm */
	number.d	= orig_number;

	/* extract sign bit. */
	sign_bit.ui	= IEEE_64_SIGN_BIT & number.ui;

	/* extract exponent. */
	exponent.ui	= IEEE_64_EXPONENT & number.ui;

	/* extract mantissa. */
	orig_mantissa.ui	= IEEE_64_MANTISSA & number.ui;

	scale_factor.i	= orig_scale_factor;

	/* check for unnormalized number */
	if (exponent.ui == 0) {

		/* Denormmal.  Get leading zeros in original mantissa */
		lz	= _OWN_LEADZ8(orig_mantissa.i);

		if (scale_factor.i > 0) {

			/* Scale number by a positive power of 2.  The
			 * result may need to be normalized.  Get
			 * leading zeros in mantissa = lzm.
			 */
			lzm	= lz - IEEE_64_EXPO_BITS - 1;

			/* Any leading zeros in mantissa? */
			if (lzm > 0) {

				/* check if number of leading zeros in
				 * mantissa allows scaling by shifting.
				 */
				if (scale_factor.i <= lzm) {

					/* Enough lead zeros to shift.
					 * Exponent is unaffected.
					 */
					shift	= scale_factor.i;
					expon.i	= 0;
				} else {

					/* Scale by shifting what we can
					 * and adjust exponent for rest.
					 * The result is a normalized
					 * number.
					 */
					shift	= lzm + 1;
					expon.i	= scale_factor.i - lzm;
				}
			/* No leading zeros in mantissa. */
			} else {

				/* Shift by 1 to normalize mantissa. Do
				 * rest of scaling through exponent.
				 */
				shift	=	1;
				expon.i	= scale_factor.i;
			}
			/* position the exponent. */
			exponent.ui	= expon.ui << IEEE_64_MANT_BITS;

			/* scale the mantissa. */
			mantissa.ui	= orig_mantissa.ui << shift;

		/* Scale_factor LE 0. */
		} else {

			/* scale mantissa. */
			mantissa.ui = orig_mantissa.ui >> (-scale_factor.i);
			if ((-scale_factor.ui != 0) &&
			    (orig_mantissa.ui & (0x1 <<
			    (-scale_factor.i - 1)))) {
					mantissa.ui++;
			}

		}

		/* mask out any bits that may have shifted to the left of
		 * the mantissa area.
		 */
		mantissa.ui &= IEEE_64_MANTISSA;

		/* OR the new mantissa, the new exponent, and the original
		 * sign bit together to create the result.
		 */
		number.ui = mantissa.ui | exponent.ui | sign_bit.ui;

	} else {

		/* Number is Normal.  Make an exponent an integer. */
		exponent.ui >>= IEEE_64_MANT_BITS;

		/* Add in scale factor. */
		exponent.i += scale_factor.i;

		if (exponent.i >= IEEE_64_MAX_EXPONENT) {

			/* raise overflow exception. */

			/* overflowed exponent. Return signed infinity. */
			number.ui = IEEE_64_INFINITY | sign_bit.ui;

		} else if (exponent.i <= 0) {

			/* raise underflow exception. */

			/* Scaled exponent is negative or zero.
			 * result.ui = (orig_mantissa.ui | (0x1 <<
			 *	IEEE_64_MANT_BITS)) >> (-exponent + 1);
			 * becomes zero on sparc.  Make 2 statements.
			 * Return denormal number. */
			number.ui = 0x1LL;
			number.ui = (((number.ui << IEEE_64_MANT_BITS) |
			   orig_mantissa.ui) >> (-exponent.i + 1));
		} else {

			/* Scaled exponent is positive and within range.
			 * Position into exponent.
			 */
			exponent.ui <<= IEEE_64_MANT_BITS;

			/* OR new mantissa, new exponent, and original 
			 * sign bit together to create result.
			 */
			number.ui = exponent.ui |
				orig_mantissa.ui | sign_bit.ui;
		}
	}
	return number.d;
}

static int
_IE_LOGB(ieee64_t x)
{
	int32		i;
	union {FLOAT64 d; uint64 ui; int64 i;} s1, s2;

	/* number x is not nan, infinity or zero, but may be denorm */
        s1.d = x;

        /* clear sign bit */
        s1.ui &= ~IEEE_64_SIGN_BIT;

        /* shift exponent bits to right. */
        s1.ui >>= IEEE_64_MANT_BITS;
        if (s1.ui == 0) {

                /* x is a subnormal number (implicit leading bit is zero
                 * and the exponent is zero).  Calculate the exponent
                 * based on normalized x.
                 *
                 * get mantissa
                 */
                s2.d    = x;
                s2.ui   = IEEE_64_MANTISSA & s2.ui;

                /* get leading zeros in mantissa part. */
                i       = _OWN_LEADZ8(s2.ui) - IEEE_64_EXPO_BITS;

                /* calculate exponent. */
                s1.i -= (IEEE_64_EXPO_BIAS + i);
        } else {
                /* subtract exponent bias. */
                s1.i -= (IEEE_64_EXPO_BIAS);
        }
        return((int) s1.i);
}
#endif

#if defined(_WORD32) || defined(__mips) || defined(_LITTLE_ENDIAN) || defined(__sv2)

extern FLOAT128 _IE_SCALBL(FLOAT128 orig_number, int orig_scale_factor);
extern int _IE_LOGBL(FLOAT128 x);

#ifndef IEEE_128_64_MANT1
#define IEEE_128_64_MANT1        0X0000FFFFFFFFFFFFLL
#endif

#ifndef IEEE_128_64_EXPO
#define IEEE_128_64_EXPO         0X7FFF000000000000LL
#endif

/*** SCALE - return X * b**i	 ***/

static FLOAT128
_IE_SCALBL(FLOAT128 orig_number, int orig_scale_factor)
{
	union ldble_float {
		ieee128_t		whole;
		unsigned long long	ui[2];
	} f, result;
	unsigned long long	sign_bit;
	unsigned long long	tmp_orig;
	long long		exponent;

	union ldble_float orig_mantissa, mantissa;
	int32	lz, lzm, ileadzcnt, shift, loopn, round;
	int32	scale_factor;
	int32	expon;
	int32	tmp =   0;
	static int	word_size =  64;

	/* if x is not a NaN, infinity or zero but may be denorm */
	f.whole	= orig_number;

	/* extract sign bit. */
	sign_bit	= IEEE_128_SIGN_BIT & f.ui[0];

	/* extract exponent. */
	exponent	= f.ui[0] & IEEE_128_64_EXPO;

	/* extract mantissa. */
	orig_mantissa.whole	= f.whole;
	orig_mantissa.ui[0] &= IEEE_128_64_MANT1;

	scale_factor	= orig_scale_factor;

	/* check for unnormalized number */
	if (exponent == 0x0LL) {

		/* Unnormmal.  Get leading zeros in original mantissa */
		lz	= 0;
		for (loopn = 0; loopn < 2; loopn++) {
			ileadzcnt = _OWN_LEADZ8(orig_mantissa.ui[loopn]);
			lz += ileadzcnt;
			if (ileadzcnt < word_size)
				break;
		}

		if (scale_factor > 0) {

                        /* Scale number by a positive power of 2.  The
                         * result may need to be normalized.  Get
                         * leading zeros in mantissa = lzm.
                         */
                        lzm = lz - IEEE_128_EXPO_BITS - 1;

                        /* Any leading zeros in mantissa? */
                        if (lzm > 0) {

                                /* check if number of leading zeros in
                                 * mantissa allows scaling by shifting.
                                 */
                                if (scale_factor <
                                        lzm || scale_factor == lzm) {

                                        /* Enough lead zeros to shift.
                                         * Exponent is unaffected.
                                         */
                                        shift = scale_factor;
                                        expon = 0;
                                } else {

                                        /* Scale by shifting what we can
                                         * and adjust exponent for rest.
                                         * The result is a normalized
                                         * number.
                                         */
                                        shift = lzm + 1;
                                        expon = scale_factor - lzm;
                                }
                        /* No leading zeros in mantissa. */
                        } else {

                                /* Shift by 1 to normalize mantissa. Do
                                 * rest of scaling through exponent.
                                 */
                                shift = 1;
                                expon = scale_factor;
                        }
                        /* position the exponent. */
                        exponent = expon <<
                                (IEEE_128_MANT_BITS - word_size);

                        /* scale the mantissa. */
                        tmp = word_size - shift;
                        if (tmp <= 0) {

                                /* load 64-bit zero as last 64 bits
                                 * of the mantissa when shift .GE 64.
                                 */
                                mantissa.ui[1] = 0x0LL;

                                /* load bits from word 2 of original
                                 * mantissa as word one of mantissa.
                                 */
                                mantissa.ui[0] = orig_mantissa.ui[1] <<
                                        (-tmp);
                        } else {
                                /* shift is .LT. 64 */
                                mantissa.ui[1] = orig_mantissa.ui[1] <<
                                        shift;

                                /* Shift word one of original mantissa
                                 * and OR shifted bits from word 2 of
                                 * original mantissa into word one.
                                 */
                                mantissa.ui[0] = (orig_mantissa.ui[0] <<
                                        shift) || (orig_mantissa.ui[1] >>
                                        tmp);
                        }

                /* Scale_factor LE 0. */
                } else {

                        /* scale mantissa. */
                        tmp = word_size + scale_factor;
                        if (-scale_factor >= word_size) {

                                /* -scale factor .GE. 64, so first word
                                 * of mantissa is zero and second word
                                 * is shifted part of word two of
                                 * original mantissa.
                                 */
                                mantissa.ui[0] =  0x0LL;
                                mantissa.ui[1] = (orig_mantissa.ui[0] >>
                                        (-tmp));
                                if (tmp == 0) {
                                        tmp_orig = orig_mantissa.ui[1];
                                        round = word_size - 1;
                                } else {
                                        tmp_orig = orig_mantissa.ui[0];
                                        round = (-tmp) - 1;
                                }
                        } else if (-scale_factor == 0) {
                                mantissa.ui[0] = orig_mantissa.ui[0];
                                mantissa.ui[1] = orig_mantissa.ui[1];
                        } else {
                                /* scale word one of mantissa. */
                                mantissa.ui[0] = orig_mantissa.ui[0] >>
                                        (-scale_factor);

                                /* scale word two of mantissa and OR in
                                 * contents from word one.
                                 */
                                mantissa.ui[1] = (orig_mantissa.ui[1] >>
                                        (-scale_factor)) ||
                                        (orig_mantissa.ui[0] << tmp);
                                tmp_orig = orig_mantissa.ui[1];
                                round = tmp - 1;
                        }

                        /* This is bit of magic that does some rounding.
                         * Get last bit to be shifted off to right.
                         * If it's 1, round mantissa up by adding 1.
                         */
                        if (((-scale_factor) != 0) &&
                                  (tmp_orig & (0x1 << round) != 0)) {
                                if (mantissa.ui[1] &
                                         (0x1 << (word_size - 1)) != 0) {
                                                round = 1;
                                } else {
                                        round = 0;
                                }
                                mantissa.ui[1] = mantissa.ui[1] + 1;
                                if ((mantissa.ui[1] &
                                         (0x1 << (word_size - 1)) != 0) &&
                                          (round == 1)) {
                                                mantissa.ui[0] =
                                                  mantissa.ui[0] + 1;
                                }
                        }
                }

                /* mask out any bits that may have shifted to the left of
                 * the mantissa area.
                 */
                mantissa.ui[0] &= IEEE_128_64_MANT1;

                /* OR the new mantissa, the new exponent, and the original
                 * sign bit together to create the result.
                 */
                result.whole = mantissa.whole;
                result.ui[0] = mantissa.ui[0] | exponent | sign_bit;

        } else {

                /* Number is Normal.  Make an exponent an integer. */
                exponent >>=    (IEEE_128_MANT_BITS - word_size);

                /* Add in the scale factor. */
                exponent += scale_factor;

                if (exponent >= IEEE_128_EXPO_MAX) {

                        /* raise overflow exception. */

                        /* overflowed exponent. Return signed infinity. */
                        result.whole = 0.0;
                        result.ui[0] = IEEE_128_64_EXPO | sign_bit;

                } else if (exponent <= 0x0LL) {

                        /* raise underflow exception. */

                        /* Scaled exponent is negative or zero.
                         * result.ui = (orig_mantissa.ui | (0x1 <<
                         *      IEEE_128_MANT_BITS)) >> (-exponent + 1);
                         * becomes zero on sparc.  Make 2 statements.
                         * Return denormal number. */
                        mantissa.ui[0] = 0x1LL;
                        mantissa.ui[1] = orig_mantissa.ui[1];
                        mantissa.ui[0] = (((mantissa.ui[0] <<
                                (IEEE_128_MANT_BITS - word_size)) |
                                orig_mantissa.ui[0]));
                        if (-exponent > 0x40LL) {
                                result.ui[0] = 0x0LL;
                                result.ui[1] = mantissa.ui[0] >>
                                        ((-exponent + 1) - word_size);
                        } else {
                                result.ui[0] = mantissa.ui[0] >>
                                        (-exponent + 1);
                                result.ui[1] = (mantissa.ui[1] >>
                                        (-exponent + 1)) ||
                                        (mantissa.ui[0] <<
                                        (word_size - (-exponent + 1)));
                        }

                } else {

                        /* Scaled exponent is positive and within range.
                         * Position into exponent.
                         */
                        exponent <<= (IEEE_128_MANT_BITS - word_size);

                        /* OR new mantissa, new exponent, and original
                         * sign bit together to create result.
                         */
                        result.whole = orig_mantissa.whole;
                        result.ui[0] = exponent |
                                orig_mantissa.ui[0] | sign_bit;
                }
        }
        return result.whole;
}

static int
_IE_LOGBL(FLOAT128 x)
{
        int32		i, ileadzcnt, loopn;
#if defined(_WORD32)
	union ldble_float {
		ieee128_t		whole;
		unsigned long long	ui[2];
		long long		si[2];
	} f, result;
#else
	union ldble_float {
		ieee128_t		whole;
		unsigned long		ui[2];
		long			si[2];
	} f, result;
#endif

	static int word_size	= 64;

	/* x is not a NaN, infinity, or zero */
	f.whole	= x;

	/* Get the absolute value of x by ANDing the upper half
	 * with the NOT of 0x8000000000000000 (the sign bit mask).
	 */
	f.ui[0] &= ~IEEE_128_SIGN_BIT;

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
			ileadzcnt	= _OWN_LEADZ8(f.ui[loopn]);
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
	return((int) result.si[0]);
}
#endif
