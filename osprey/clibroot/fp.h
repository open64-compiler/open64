/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
 */

/* USMID @(#)include/fp.h	100.0	07/11/97 00:26:19 */

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


#ifndef _FP_H
#define _FP_H

/* IEEE floating-point definitions. */
/* Based from SC22 WG14 N403 and X3J11/95-004 definition. */

#ifdef _CRAYIEEE

/* Floating-point number classifications returned by fpclassify. */
#define FP_NAN          0
#define FP_INFINITE     1
#define FP_NORMAL       2
#define FP_SUBNORMAL    3
#define FP_ZERO         4


/* DECIMAL_DIG  should be a number  n  such that  10^(n-1) > 2^m  where
m  is the precision of the widest applicable format (128 bits for PVP
machines, 64 bits for MPP machines).  Conversion from long double to
decimal-with-DECIMAL_DIG-digits and back to long double can be the
identity function.  LDBL_DIG (defined in float.h) is a number  m  such
that  2^(n-1) > 10^m  where now n is the precision of the binary
format.  It supports the other round-trip, namely that conversion from
decimal-with-LDBL_DIG-digits to long double and back to decimal-with-
LDBL_DIG-digits can be the identity function. */

#ifdef _LD64          /* 64 bits */
   #define DECIMAL_DIG    17
#else                 /* 128 bits */
   #define DECIMAL_DIG    36
#endif

#if defined(_CRAY1) || defined(_CRAYT3E)
#include <sys/cdefs.h>

__BEGIN_DECLS

/* Value representing quiet NaN of type double. */
extern const double __fp_nan;
#define NAN           (__fp_nan)
/* Value representing positive infinity */
extern const double __fp_infinity;
#define INFINITY      (__fp_infinity)

#else
/* Cray-T3D */

/* Value representing quiet NAN */
/* Exponent all 1's, mantissa all 1's */
#define NAN		0X7FFFFFFFFFFFFFFF.

/* Value representing positive infinity */
/* Exponent all 1's, mantissa all zeros */
#define INFINITY	0X7FF0000000000000.

#endif


#if defined(_CRAY1) 
/* Value representing positive infinity as a float. */
#ifndef HUGE_VALF
extern const float __fp_infinityf;
#define HUGE_VALF  (__fp_infinityf)
#endif

/* Value representing positive infinity as a double. */
#ifndef HUGE_VAL
#define HUGE_VAL      (__fp_infinity)
#endif

/* Value representing positive infinity as a long double. */
#ifndef HUGE_VALL
extern const long double __fp_infinityl;
#define HUGE_VALL  (__fp_infinityl)
#endif

#else

/* Cray-T3D and Cray-T3E */
/* Value representing largest useable number less than infinity */
/* Exponent 0x7FE,  mantissa all ones */
#ifndef HUGE_VAL
#define HUGE_VAL	0X7FEFFFFFFFFFFFFF.
#endif

#endif

#if defined(_CRAY1) || defined(_CRAYT3E)

/* Prototype declarations for intrinsics used in this header. */

extern int _fpclassifyf(float);
extern int _fpclassify(double);
extern int _fpclassifyl(long double);

extern int _signbitf(float);
extern int _signbit(double);
extern int _signbitl(long double);

extern int _isfinitef(float);
extern int _isfinite(double);
extern int _isfinitel(long double);

extern int _isnormalf(float);
extern int _isnormal(double);
extern int _isnormall(long double);

extern int _isnanf(float);
extern int _isnan(double);
extern int _isnanl(long double);

extern int _isgreaterf(float, float);
extern int _isgreater(double, double);
extern int _isgreaterl(long double, long double);

extern int _isgreaterequalf(float, float);
extern int _isgreaterequal(double, double);
extern int _isgreaterequall(long double, long double);

extern int _islessf(float, float);
extern int _isless(double, double);
extern int _islessl(long double, long double);

extern int _islessequalf(float, float);
extern int _islessequal(double, double);
extern int _islessequall(long double, long double);

extern int _islessgreaterf(float, float);
extern int _islessgreater(double, double);
extern int _islessgreaterl(long double, long double);

extern int _isunorderedf(float, float);
extern int _isunordered(double, double);
extern int _isunorderedl(long double, long double);

extern float _remainderf(float, float);
extern double _remainder(double, double);
extern long double _remainderl(long double, long double);

extern float _rintf(float);
extern double _rint(double);
extern long double _rintl(long double);

extern float _copysignf(float, float);
extern double _copysign(double, double);
extern long double _copysignl(long double, long double);

extern float _scalbf(float, long);
extern double _scalb(double, long);
extern long double _scalbl(long double, long);

extern float _logbf(float);
extern double _logb(double);
extern long double _logbl(long double);

extern float _nextafterf(float, float);
extern double _nextafter(double, double);
extern long double _nextafterl(long double, long double);

extern long int _rinttol(long double);

/* Macro definitions. */

#define fpclassify(X) ((sizeof(X) == sizeof(double)) ? _fpclassify(X)  : \
                       (sizeof(X) == sizeof(float)) ?  _fpclassifyf(X) : \
                                                       _fpclassifyl(X))

#define signbit(X) ((sizeof(X) == sizeof(double)) ? _signbit(X)  : \
                    (sizeof(X) == sizeof(float)) ?  _signbitf(X) : \
                                                    _signbitl(X))

#define isfinite(X) ((sizeof(X) == sizeof(double)) ? _isfinite(X)  : \
                     (sizeof(X) == sizeof(float)) ?  _isfinitef(X) : \
                                                     _isfinitel(X))

#define isnormal(X) ((sizeof(X) == sizeof(double)) ? _isnormal(X)  : \
                     (sizeof(X) == sizeof(float)) ?  _isnormalf(X) : \
                                                     _isnormall(X))

#define isnan(X) ((sizeof(X) == sizeof(double)) ? _isnan(X)  : \
                  (sizeof(X) == sizeof(float)) ?  _isnanf(X) : \
                                                  _isnanl(X))

#define isgreater(X,Y) ((sizeof(X) == sizeof(long double) ||                 \
                         sizeof(Y) == sizeof(long double)) ?                 \
                                                         _isgreaterl(X, Y) : \
                        (sizeof(X) == sizeof(double) ||                      \
                         sizeof(Y) == sizeof(double)) ?                      \
                                                         _isgreater(X, Y) :  \
                                                         _isgreaterf(X, Y))

#define isgreaterequal(X,Y) ((sizeof(X) == sizeof(long double) ||            \
                              sizeof(Y) == sizeof(long double)) ?            \
                                                    _isgreaterequall(X, Y) : \
                             (sizeof(X) == sizeof(double) ||                 \
                              sizeof(Y) == sizeof(double)) ?                 \
                                                    _isgreaterequal(X, Y) :  \
                                                    _isgreaterequalf(X, Y))

#define isless(X,Y) ((sizeof(X) == sizeof(long double) ||                    \
                      sizeof(Y) == sizeof(long double)) ?                    \
                                                            _islessl(X, Y) : \
                     (sizeof(X) == sizeof(double) ||                         \
                      sizeof(Y) == sizeof(double)) ?                         \
                                                            _isless(X, Y) :  \
                                                            _islessf(X, Y))

#define islessequal(X,Y) ((sizeof(X) == sizeof(long double) ||               \
                           sizeof(Y) == sizeof(long double)) ?               \
                                                       _islessequall(X, Y) : \
                          (sizeof(X) == sizeof(double) ||                    \
                           sizeof(Y) == sizeof(double)) ?                    \
                                                       _islessequal(X, Y) :  \
                                                       _islessequalf(X, Y))

#define islessgreater(X,Y) ((sizeof(X) == sizeof(long double) ||             \
                             sizeof(Y) == sizeof(long double)) ?             \
                                                     _islessgreaterl(X, Y) : \
                            (sizeof(X) == sizeof(double) ||                  \
                             sizeof(Y) == sizeof(double)) ?                  \
                                                     _islessgreater(X, Y) :  \
                                                     _islessgreaterf(X, Y))

#define isunordered(X,Y) ((sizeof(X) == sizeof(long double) ||               \
                           sizeof(Y) == sizeof(long double)) ?               \
                                                       _isunorderedl(X, Y) : \
                          (sizeof(X) == sizeof(double) ||                    \
                           sizeof(Y) == sizeof(double)) ?                    \
                                                       _isunordered(X, Y) :  \
                                                       _isunorderedf(X, Y))



/* Function declarations. */

extern float remainderf(float, float);
extern double remainder(double, double);
extern long double remainderl(long double, long double);

extern float rintf(float);
extern double rint(double);
extern long double rintl(long double);

extern float copysignf(float, float);
extern double copysign(double, double);
extern long double copysignl(long double, long double);

extern float scalbf(float, long);
extern double scalb(double, long);
extern long double scalbl(long double, long);

extern float logbf(float);
extern double logb(double);
extern long double logbl(long double);

extern float nextafterf(float, float);
extern double nextafter(double, double);
extern double nextafterd(double, double);
extern long double nextafterl(long double, long double);

extern long int rinttol(long double);



/* Macro versions of above functions (available for performance reasons). */

#define remainderf(X, Y) _remainderf(X, Y)
#define remainder(X, Y) _remainder(X, Y)
#define remainderl(X, Y) _remainderl(X, Y)

#define rintf(X) _rintf(X)
#define rint(X) _rint(X)
#define rintl(X) _rintl(X)

#define copysignf(X, Y) _copysignf(X, Y)
#define copysign(X, Y) _copysign(X, Y)
#define copysignl(X, Y) _copysignl(X, Y)

#define scalbf(X, Y) _scalbf(X, Y)
#define scalb(X, Y) _scalb(X, Y)
#define scalbl(X, Y) _scalbl(X, Y)

#define logbf(X) _logbf(X)
#define logb(X) _logb(X)
#define logbl(X) _logbl(X)

#define nextafterf(X, Y) _nextafterf(X, Y)
#define nextafter(X, Y) _nextafter(X, Y)
#define nextafterd(X, Y) _nextafter(X, Y)
#define nextafterl(X, Y) _nextafterl(X, Y)

#define rinttol(X) _rinttol(X)


__END_DECLS
#endif /* _CRAY1  || _CRAYT3E */

#if __STDC__ != 1
/* These definitions are not part of */
/* SC22 WG14 N403 and X3J11/95-004 definition. */

/* pieces of a IEEE 64 bit floating point number */

#define IEEE_64_SIGN_BIT	0X8000000000000000
#define IEEE_64_EXPONENT	0X7FF0000000000000
#define IEEE_64_MANTISSA	0X000FFFFFFFFFFFFF
#define IEEE_64_IMPLICIT_BIT	0X0010000000000000
#define IEEE_64_EXPO_BIAS	1023
#define IEEE_64_EXPO_BITS	11
#define IEEE_64_EXPO_MAX	0X7FF
#define IEEE_64_MANT_BITS	52
#define IEEE_64_MANT_MAX	0XFFFFFFFFFFFFF
#define IEEE_64_EXPO_ALL_ONES(X) (((X)&IEEE_64_EXPONENT)==IEEE_64_EXPONENT)
#define IEEE_64_ABS_VALUE       0X7FFFFFFFFFFFFFFF


/* Union defined to work with IEEE 64 bit floating point. */
union _ieee_double {
   double dword;
   long long   lword;
   struct {
#if __BYTE_ORDER == __LITTLE_ENDIAN
     unsigned int mantissa : IEEE_64_MANT_BITS;
     unsigned int exponent : IEEE_64_EXPO_BITS;
     unsigned int sign : 1;
#else
     unsigned int sign : 1;
     unsigned int exponent : IEEE_64_EXPO_BITS;
     unsigned int mantissa : IEEE_64_MANT_BITS;
#endif
   } parts;
};

#if !defined(_CRAY1) && !defined(_CRAYT3E)
/* assume big endian */
/* Implementation's most efficient floating types */
typedef double float_t;
typedef double double_t;


/* macro which returns classification value */
#define fpclassify(x) ((sizeof(x)==sizeof(double)) ?  __fpclassifyd(x) \
       : printf("%s %d: fpclassify() only supported for double\n", \
		__FILE__, __LINE__))
static int __fpclassifyd(x)
double x;
{
   union _ieee_double x_val;
   x_val.dword = x;
   if (x_val.parts.exponent==0) {
      return (x_val.parts.mantissa==0 ? FP_ZERO : FP_SUBNORMAL);
   }
   else if (IEEE_64_EXPO_ALL_ONES(x_val.lword)) {
      return (x_val.parts.mantissa==0 ? FP_INFINITE : FP_NAN);
   }
   else {
      return (FP_NORMAL);
   }
}

/* Returns non-zero 
 * if argument is negative (including infinites, zeros, and NANs) */
/* Test if sign bit set */
#pragma _CRI inline signbit
static int signbit(double xarg)
{
   	union _ieee_double x;
	x.dword = xarg;
	return( x.parts.sign);
}

/* macro which evaluates aint(x) */
#define aint(x) __aint(x)
static double __aint(x)
double x;
{
   double fabs(double);
#define _AINT_MAXVAL 4503599627370496.0
   return( (fabs(x) >= _AINT_MAXVAL) ? x : (double) ((long) x) );
}

/* macro which evaluates to a nonzero int expression if its argument value
* is infinite: exponent is all 1's and mantissa is all zero */
#define isinf(x) __is_infinite(x)
static int __is_infinite(double xarg)
{
   union _ieee_double x;
   x.dword = xarg;
   return( (x.lword&IEEE_64_ABS_VALUE)==IEEE_64_EXPONENT );
}


/* macro which evaluates to a nonzero int expression if its argument value
 * is finite: either zero, normal, or subnormal. */
/* Test if exponent not all 1's  - all 1's mean NaN or infinity */
#define isfinite(x) __is_finite(x)
static int __is_finite(double xarg)
{
   union _ieee_double x;
   x.dword = xarg;
   return(!IEEE_64_EXPO_ALL_ONES(x.lword));
}

/* macro which evaluates to a nonzero int expression if its argument value
 * is normal: neither zero, subnormal, infinite, or NaN. */
/* Test if exponent is not All-1's or zero */
#define isnormal(x) __is_normal(x)
static int __is_normal(double xarg)
{
   union _ieee_double x;
   x.dword = xarg;
   return(!IEEE_64_EXPO_ALL_ONES(x.lword) && (x.lword&IEEE_64_EXPONENT)!=0);
}

/* macro which evaluates to a nonzero int expression if its argument value
 * is a NaN */
/* Test if exponent All-1's and Fraction non-zero */
/* Since NaNs are signaling, cannot do any floating point operations on NaN */
#define isnan(x) __is_nan(x)
static int __is_nan(double xarg)
{
   union _ieee_double x;
   x.dword = xarg;
   return(IEEE_64_EXPO_ALL_ONES(x.lword) && (x.lword&IEEE_64_MANTISSA)!=0);
}

/* function which extracts the exponent of x, as a signed integral value.
 * If x is subnormal,it is treated as though it were normalized */
static double logb(x)
double x;
{  
   switch (fpclassify(x)) {
      case FP_NAN:
         return(x);

      case FP_INFINITE:
         return(INFINITY);

      case FP_NORMAL:
	 {
	   union _ieee_double x_val;
	   x_val.dword = x;

	   return(x_val.parts.exponent - IEEE_64_EXPO_BIAS);
	 }

      case FP_SUBNORMAL:
	 {
	   union _ieee_double x_val;
	   x_val.dword = x;

	   /* _leadz returns number of zeros before first "1" in the mantissa.*/
	   /* Add 11 to exclude exponent bits,  but count sign bit since
	   ** implicit bit needs to be counted. */
	   return(-IEEE_64_EXPO_BIAS - _leadz(x_val.parts.mantissa) +
							IEEE_64_EXPO_BITS);
	 }

      case FP_ZERO:
	 /* raise divide-by-zero exception */
	 {
	   union _ieee_double x_val;
	   x_val.dword = INFINITY;
	   x_val.parts.sign = 1;

	   return(x_val.dword);
	 }

   }
}

/* function which computes x * 2**n efficiently by not computing 2**n */
static double scalb(double x, long n)
{  
   int fp_class = fpclassify(x);

   if (fp_class==FP_NORMAL) {
      union _ieee_double x_val;
      long exponent;
      x_val.dword = x;

      exponent = x_val.parts.exponent + n;

      if (exponent <= 0) {
	 /* need to emit underflow exception */
         long full_mantissa = x_val.parts.mantissa;

	 /* add implicit bit to mantissa */
	 full_mantissa |=  IEEE_64_IMPLICIT_BIT;

	 /* shift mantissa over by exponent remaining */
	 full_mantissa >>= -exponent + 1;

	 /* return denormal number */
	 x_val.parts.exponent = 0;
         x_val.parts.mantissa = full_mantissa;
      }
      else if ((exponent>>IEEE_64_EXPO_BITS) != 0) {
	 /* need to emit overflow exception */

	 /* return infinity */
	 x_val.dword = INFINITY;
	 x_val.parts.sign = signbit(x);
      }
      else {
	 x_val.parts.exponent = exponent;
      }
      return(x_val.dword);
   }
   else if (fp_class==FP_SUBNORMAL) {
      union _ieee_double x_val;
      x_val.dword = x;

      if (n <= 0) {
         x_val.parts.mantissa = x_val.parts.mantissa >> (-n);
      }
      else {
	 long mantissa_bits_to_fill = _leadz(x_val.parts.mantissa);

	 if (mantissa_bits_to_fill >= n) {
            x_val.parts.mantissa = x_val.parts.mantissa << n;
	 }
	 else {
	    /* will be returning a normal number */
	    /* shift out implicit bit */
            x_val.parts.mantissa = x_val.parts.mantissa <<
						(mantissa_bits_to_fill + 1);
	    x_val.parts.exponent = n - mantissa_bits_to_fill + 1;
	 }
      }

      return(x_val.dword);
   }
   else { /* NAN, INFINITE, or ZERO */
      return(x);
   }
}

/* function which produces a value with the magnitude of x and the sign
 * of y */
#pragma _CRI inline copysign
static double copysign(double x, double y)
{  
   union _ieee_double x_val;

   x_val.dword = x;
   x_val.parts.sign = signbit(y);
   return x_val.dword;
}

/* function which determine the next representable value, after x in
 * the direction of y.  The nextafter function return y if x==y */
static double nextafter(double x, double y)
{  
   int x_fp_class = fpclassify(x);
   int y_fp_class = fpclassify(y);

   if (x_fp_class==FP_NAN) {
      return x;
   }
   else if (y_fp_class==FP_NAN) {
      return y;
   }
   else if (x_fp_class==FP_ZERO && y_fp_class==FP_ZERO) {
      return y;
   }
   else if (x==y || x_fp_class==FP_INFINITE) {
      return x;
   }
   else if (x_fp_class==FP_ZERO) {
      union _ieee_double x_val;

      x_val.parts.exponent = 1;
      x_val.parts.mantissa = 0;
      x_val.parts.sign = x > y;

      /* return smallest normal number */
      return(x_val.dword);
   }
   else {  /* first argument is normal or denormal */
      union _ieee_double x_val;
      long full_mantissa;

      x_val.dword = x;

      /* move one bit in the correct direction */
      /* note that because of the way the implicit bit works,
      ** the exponent field is handled correctly */
      x_val.lword += (x>y) ? -1 : 1;

      /* may want to test for underflow or overflow */

      return(x_val.dword);
   }
}
#endif  /* !_CRAY1  && !_CRAYT3E */
#ifndef _LD64
/* pieces of a IEEE 128 bit floating point number */
#define IEEE_128_SIGN_BIT	0X8000000000000000
#define IEEE_128_EXPONENT	0X7FFF000000000000
#define IEEE_128_MANTISSA_UP	0X0000FFFFFFFFFFFF
#define IEEE_128_MANTISSA_LOW	0XFFFFFFFFFFFFFFFF
#define IEEE_128_IMPLICIT_BIT	0X0001000000000000
#define IEEE_128_EXPO_BITS	15
#define IEEE_128_EXPO_MAX	0X7FFF
#define IEEE_128_EXPO_BIAS	16383
#define IEEE_128_MANT_BITS	112
#define IEEE_128_MANT_BITS_UP	48
#define IEEE_128_MANT_BITS_LOW	64
#define IEEE_128_EXPO_ALL_ONES(X) (((X)&IEEE_128_EXPONENT)==IEEE_128_EXPONENT)
#define IEEE_128_ABS_VALUE	0X7FFFFFFFFFFFFFFF

/* pieces of 128-bit positive infinity */
/* Exponent is all 1's, mantissa is all zeros */
#define INFINITY_128_UP		0X7FFF000000000000
#define INFINITY_128_LOW	0

/* Union defined to work with IEEE 128 bit floating point. */
union _ieee_ldouble {
   long double	dword;
   long long   	lword[2];
   struct {
#if __BYTE_ORDER == __LITTLE_ENDIAN
     unsigned long long mantissa_low : IEEE_128_MANT_BITS_LOW;
     unsigned long long mantissa_up  : IEEE_128_MANT_BITS_UP;
     unsigned long long exponent     : IEEE_128_EXPO_BITS;
     unsigned long long sign         : 1;
#else
     unsigned long long sign         : 1;
     unsigned long long exponent     : IEEE_128_EXPO_BITS;
     unsigned long long mantissa_up  : IEEE_128_MANT_BITS_UP;
     unsigned long long mantissa_low : IEEE_128_MANT_BITS_LOW;
#endif
   } parts;
};

#if defined(_CRAY1) || defined(_CRAYT3E)
#define fpclassifyl(x) _fpclassifyl(x)
#else
/* fpclassifyl() macro returns above number classification value */
#define fpclassifyl(x) ((sizeof(x)==sizeof(long double)) ?  __fpclassifyl(x) \
	: printf("%s %d: fpclassifyl() only supported for 128-bit\n", \
		__FILE__, __LINE__))
static int __fpclassifyl(x)
long double x;
{
	union _ieee_ldouble x_val;
	x_val.dword = x;
	if (x_val.parts.exponent==0) {
		return ((x_val.parts.mantissa_up==0 && x_val.parts.mantissa_low==0 &&
		   x_val.lword[1] == 0)
			? FP_ZERO : FP_SUBNORMAL);
	}
	else if (IEEE_128_EXPO_ALL_ONES(x_val.lword[0])) {
		return ((x_val.parts.mantissa_up==0 && x_val.parts.mantissa_low==0 &&
		   x_val.lword[1] == 0)
			? FP_INFINITE : FP_NAN);
	} else {
		return (FP_NORMAL);
	 }
}

/*
 * function which produces a value with the magnitude of x and the sign
 * of y 
 */
#pragma _CRI inline copysignl
static double copysignl(long double x, long double y)
{  
   union _ieee_ldouble x_val;

   x_val.dword = x;
   x_val.parts.sign = signbitl(y);
   return x_val.dword;
}

/*
 * function which extracts the exponent of x, as a signed integral value.
 * If x is subnormal,it is treated as though it were normalized 
 */
static long double logbl(long double x)
{
   switch (fpclassifyl(x)) {
      case FP_NAN:
         return(x);
      break;

      case FP_INFINITE:
	 {
	   union _ieee_ldouble x_val;
	   x_val.lword[0] = INFINITY_128_UP;
	   x_val.lword[1] = INFINITY_128_LOW;
           return(x_val.dword);
	 }
      break;

      case FP_NORMAL:
	 {
	   union _ieee_ldouble x_val;
	   x_val.dword = x;

	   return(x_val.parts.exponent - IEEE_128_EXPO_BIAS);
	 }
      break;

      case FP_SUBNORMAL:
	 {
	   union _ieee_ldouble x_val;
           int y;
	   x_val.dword = x;

	   /* _leadz returns number of zeros before first "1" in the mantissa.*/
	   /* Add IEEE_128_EXPO_BITS to exclude exponent bits,  but count     */
	   /* sign bit since implicit bit needs to be counted. */

	   y = _leadz(x_val.parts.mantissa_up);
           if (y == 64)	/* the entire upper part of the mantissa was zero */
	      y += _leadz(x_val.parts.mantissa_low);
	   return(-IEEE_128_EXPO_BIAS - y + IEEE_128_EXPO_BITS);
	 }
      break;

      case FP_ZERO:
	 /* raise divide-by-zero exception */
	 {
	   union _ieee_ldouble x_val;
	   x_val.lword[0] = INFINITY_128_UP;
	   x_val.lword[1] = INFINITY_128_LOW;
	   x_val.parts.sign = 1;

	   return(x_val.dword);
	 }
      break;
	
  }
}

/* function which computes x * 2**n efficiently by not computing 2**n */
static long double scalbl(long double x, long n)
{  
   int fp_class = fpclassifyl(x);

   if (fp_class==FP_NORMAL) {
      union _ieee_ldouble x_val;
      long exponent;
      x_val.dword = x;

      exponent = x_val.parts.exponent + n;

      if (exponent <= 0) {
	 /* need to emit underflow exception */
         unsigned long mantissa_bits;
         unsigned long mantissa_up = x_val.parts.mantissa_up;
         unsigned long mantissa_low = x_val.parts.mantissa_low;
         int shift;

	 /* add implicit bit to mantissa */
	 mantissa_up |=  IEEE_128_IMPLICIT_BIT;

	 /* shift mantissa over by exponent remaining */
	 shift = -exponent + 1;
	 if (shift <= 64) {
	    mantissa_bits = mantissa_up << (64 - shift);
	    mantissa_up >>= shift;
	    mantissa_low  = (mantissa_low >> shift) | mantissa_bits;
	 }
	 else {
	    mantissa_low  = mantissa_up >> (shift - 64);
	    mantissa_up  = 0;
	 }

	 /* return denormal number */
	 x_val.parts.exponent = 0;
         x_val.parts.mantissa_up = mantissa_up;
         x_val.parts.mantissa_low = mantissa_low;
      }
      else if ((exponent>>IEEE_128_EXPO_BITS) != 0) {
	 /* need to emit overflow exception */

	 /* return infinity */
	 x_val.lword[0] = INFINITY_128_UP;
	 x_val.lword[1] = INFINITY_128_LOW;
	 x_val.parts.sign = signbitl(x);
      }
      else {
	 x_val.parts.exponent = exponent;
      }
      return(x_val.dword);
   }
   else if (fp_class==FP_SUBNORMAL) {
      union _ieee_ldouble x_val;
      unsigned long mantissa_bits;
      unsigned long mantissa_up;
      unsigned long mantissa_low;

      x_val.dword = x;
      mantissa_up = x_val.parts.mantissa_up;
      mantissa_low = x_val.parts.mantissa_low;

      if (n <= 0) {
         n = -n;
         if (n <= 64) {
            mantissa_bits = mantissa_up << (64-n);
            mantissa_up >>= n;
            mantissa_low = mantissa_bits | (mantissa_low >> n);
         }
         else {
            mantissa_low = mantissa_up >> (n-64);
            mantissa_up = 0;
         }
         x_val.parts.mantissa_up = mantissa_up;
         x_val.parts.mantissa_low = mantissa_low;
      }
      else {
	 long mantissa_bits_to_fill = _leadz(mantissa_up);
	 if (mantissa_bits_to_fill == 64) {
            mantissa_bits_to_fill += _leadz(mantissa_low);
         }

	 if (mantissa_bits_to_fill >= n) {
            unsigned long mantissa_bits;
            if (n <= 64) {
               mantissa_bits = mantissa_low >> (64-n);
               mantissa_low <<= n;
               mantissa_up = mantissa_bits | (mantissa_up << n);
            }
            else {
               mantissa_up = mantissa_low << (n-64);
               mantissa_low = 0;
            }
	 }
	 else {
	    /* will be returning a normal number */
	    /* shift out implicit bit */
            int shift;
            shift = mantissa_bits_to_fill +1;
            if (shift <= 64) {
               mantissa_bits = mantissa_low << (64 - shift);
               mantissa_low <<= shift;
               mantissa_up = mantissa_bits | (mantissa_up << shift);
            }
            else {
               mantissa_up = mantissa_low << (shift-64);
               mantissa_low = 0;
            }
	    x_val.parts.exponent = n - mantissa_bits_to_fill + 1;
	 }
         x_val.parts.mantissa_low = mantissa_low;
         x_val.parts.mantissa_up = mantissa_up;
      }

      return(x_val.dword);
   }
   else { /* NAN, INFINITE, or ZERO */
      return(x);
   }
}
#endif	/* _CRAY1 || _CRAYT3E */
#endif	/* _LD64    */

#endif	/* STDC */
#endif /* _CRAYIEEE */


#if defined(_CRAYIEEE) || !defined(_UNICOS)
#if (__STDC__ != 1) || defined(__mips) || defined(_LITTLE_ENDIAN)
/* Values for quiet NANs for F90 library */

static const union { int i; float h; } _HNAN = { 0x7fc00000 };

#define _HALF_NaN _HNAN.h

#ifdef _CRAYIEEE
static const union {
	long i;
	double s;
} _SNAN = { 0x7ff8000000000000L };
#else
static const union {
	long long i;
	double s;
} _SNAN = { 0x7ff8000000000000LL };
#endif

#define _SGL_NaN _SNAN.s

#ifdef _CRAYIEEE
static const union {
	struct	{
		long	ireal1;
		long	ireal2;
	} dex;
	long double d;
} _DNAN = { 0x7fff800000000000L, 0x0000000000000000L };
#else
/* For Mips double-double, 128-bit NaN is a 64-bit NaN with
 * an empty second word.  Only the NaN in the first double
 * is recognized as the double-double NaN.  Note that 64-bit
 * integer constants need LL and long double constants need
 * L after the constant on mips.
 */
static const union {
	struct	{
		long long	ireal1;
		long long	ireal2;
	} dex;
	long double d;
#ifdef __mips
} _DNAN = { 0x7ff8000000000000LL, 0x0000000000000000LL };
#else
} _DNAN = { 0x7fff800000000000LL, 0x0000000000000000LL };
#endif /* __mips */
#endif /* _CRAYIEEE */
#define _DBL_NaN _DNAN.d
#endif /*__STDC__ */
#endif /* _CRAYIEEE or not _UNICOS */
#endif /* !_FP_H */
