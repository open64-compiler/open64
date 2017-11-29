/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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

/* ====================================================================
 * ====================================================================
 *
 * Module: lgamma.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:22-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.lgamma.c $
 *
 * Revision history:
 *  28-May-93 - Original Version
 *
 * Description:	source code for lgamma function
 *
 * ====================================================================
 * ====================================================================
 */

/* $Header: /home/bos/bk/kpro64-pending/libm/mips/lgamma.c 1.5 04/12/21 14:58:22-08:00 bos@eng-25.internal.keyresearch.com $ */

/*
	C program for floating point log Gamma function

	lgamma(x) computes the log of the absolute
	value of the Gamma function.
	The sign of the Gamma function is returned in the
	external quantity signgam.

	Calls log, log1p, and floor.
*/

#ifdef _CALL_MATHERR
#include <stdio.h>
#include <math.h>
#endif

#include <errno.h>
#include "libm.h"

#if defined(mips) && !defined(__GNUC__)
extern	int	signgam;
extern	double	gamma(double);
extern	double	lgamma(double);

#pragma weak signgam = __signgam
#pragma weak gamma = __gamma
#pragma weak lgamma = __lgamma
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern double __gamma(double);
extern double __lgamma(double);
#pragma weak gamma
#pragma weak lgamma
#pragma weak signgam
double gamma(double arg) {
  return __gamma(arg);
}
double lgamma( double arg ) {
  return __lgamma( arg );
}
int signgam = 0;
#elif defined(__GNUC__)
extern  double  __gamma(double);

double    gamma() __attribute__ ((weak, alias ("__gamma")));

extern  double  __lgamma(double);

double    lgamma() __attribute__ ((weak, alias ("__lgamma")));

extern	int  __signgam;

extern	int  signgam  __attribute__ ((weak, alias ("__signgam")));

#endif

extern	double	__floor(double);
extern	double	__log(double);
extern	double	__log1p(double);

extern const du __P4[4][2];
extern const du __P5[4][3];
extern const du __P6[4][4];
extern const du __P7[4][5];
extern const du __P8[6];

extern const du __S0[8][8];
extern const du __T0[8][7];

extern const du __S1[7];
extern const du __T1[8];

int	__signgam = 0;

double __lgamma(double);

static const du Qnan =
{D(QNANHI, QNANLO)};

static const du	Inf = {D(0x7ff00000, 0x00000000)};

static const du twopm54 = {D(0x3c900000, 0x00000000)};

static const du twop100 = {D(0x46300000, 0x00000000)};

static const du twop1000 = {D(0x7e700000, 0x00000000)};

static const du maxarg = {D(0x7f5754d9, 0x278b51a7)};

static const du halfln2pi = {D(0x3fed67f1, 0xc864beb5)};

static const double p1[] = {
	0.83333333333333101837e-1,
	-.277777777735865004e-2,
	0.793650576493454e-3,
	-.5951896861197e-3,
	0.83645878922e-3,
	-.1633436431e-2,
};

/* coefficients to approximate sin(pi*h)/pi on [-0.5, 0.5] */

static const du p2[] = 
{
D(0x3ff00000, 0x00000000),
D(0xbffa51a6, 0x625307d3),
D(0x3fe9f9cb, 0x402bc3f2),
D(0xbfc86a8e, 0x47208961),
D(0x3f9ac680, 0x5cbdb251),
D(0xbf633816, 0x96cdf74f),
D(0x3f237469, 0x5b384e5f),
D(0xbedd3e33, 0x5e638a70),
D(0x3e9070f4, 0x7be9b968),
};

double
__gamma(double arg)
{
double	result;

	result = __lgamma(arg);

	return result;
}


static double asym(double), neg(double), pos(double), lgmapp(int, double);

/* ====================================================================
 *
 * FunctionName		lgamma
 *
 * Description		computes log(|gamma(arg)|)
 *			sign(gamma(arg)) is returned in global signgam
 *
 * Based on the ideas in chapter 6.6 of Hart et al's book "Computer
 * Approximations", John Wiley and Sons, 1968.
 *
 * ====================================================================
 */

double
__lgamma( double arg )
{
#ifdef _CALL_MATHERR
struct exception	exstruct;
#endif

	signgam = 1;
#if defined(BUILD_OS_DARWIN)
	__signgam = signgam;
#endif /* defined(BUILD_OS_DARWIN) */

	if ( arg != arg )
	{
		/* arg is a NaN; return a quiet NaN */

#ifdef _CALL_MATHERR

		exstruct.type = DOMAIN;
		exstruct.name = "lgamma";
		exstruct.arg1 = arg;
		exstruct.retval = Qnan.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "domain error in lgamma\n");
			SETERRNO(EDOM);
		}

		return ( exstruct.retval );
#else
		NAN_SETERRNO(EDOM);

		return ( Qnan.d );
#endif
	}

	if ( arg <= 0.0 )
		return ( neg(arg) );
	else
		return ( pos(arg) );
}

static
double
pos( double arg )
{
int	j, j1, k;
double	x0, h;
double	result;

	/* computes ln(|gamma(x)|) for x > 0.0;
	 * uses rational approximations to gamma for
	 * 1.0 - 1/8 < x < 3.0 + 1/8.
	 * for other values of x, uses the recursion formula 
	 * gamma(x+1) = x*gamma(x) to reduce the arg to 
	 * something between .875 and 3.125 .
	 *
	 * The factors (x-1)*(x-2)*... have been rewritten as
	 * (ak + h -1)*(ak + h - 2)*... expanded in powers of h,
	 * where ak = nearest 4th to arg, and h = arg - ak.
	 * This lessens the amount of error in computing the product.
	 */

	if ( arg > 8.0 )
		return ( asym(arg) );

       /* special case values near 0.0 */

        if ( arg < twopm54.d )
        {
                return ( -__log(arg) );
        }

	if ( arg < .875 )
	{
		/* gamma(x) = gamma(x+1.0)/x */

		j = ROUND(4.0*arg);

		x0 = j/4.0;

		h = arg - x0;

		return ( lgmapp(j+4, h) - __log(arg) );
	}

	if ( (.875 <= arg) && (arg < 3.125) )
	{
		/* just use the correct approximation for log(gamma(x)) */

		if ( (1.875 <= arg) && (arg < 2.1875) )
		{
			/* use a special approximation around 2.0 */

			j = 8;
			x0 = 2.0;
		}
		else
		{
			j = ROUND(4.0*arg);
	
			x0 = j/4.0;
		}

		h = arg - x0;

		return ( lgmapp(j, h) );
	}

	/* between 3.125 and 8.0, use the reduction formula described above */

	j = ROUND(4.0*arg);
	k = j/4;
	x0 = j/4.0;

	h = arg - x0;

	j1 = j%4;


	switch( k )
	{
		case 3:
			result = __log(arg - 1.0) +
				 lgmapp(j-4, h);
			break;

		case 4:
			result = __log((h + __P4[j1][1].d)*h + __P4[j1][0].d) +
				 lgmapp(j-8, h);

			break;

		case 5:
			result = __log(((h + __P5[j1][2].d)*h +
				 __P5[j1][1].d)*h + __P5[j1][0].d) + 
				 lgmapp(j-12, h);

			break;

		case 6:
			result = __log((((h + __P6[j1][3].d)*h +
				 __P6[j1][2].d)*h + __P6[j1][1].d)*h +
				 __P6[j1][0].d) +
				 lgmapp(j-16, h);
			break;

		case 7:
			result = __log(((((h + __P7[j1][4].d)*h +
				 __P7[j1][3].d)*h + __P7[j1][2].d)*h +
				 __P7[j1][1].d)*h + __P7[j1][0].d) +
				 lgmapp(j-20, h);
			break;

		case 8:
			result = __log((((((h + __P8[5].d)*h +
				 __P8[4].d)*h + __P8[3].d)*h +
				 __P8[2].d)*h + __P8[1].d)*h +
				 __P8[0].d) +
				 lgmapp(j-24, h);
			break;
	}

	return ( result );
}

static
double
neg( double arg )
{
double	z, h;
double	hsq, result;
double	sinpoly;
long	n;
#ifdef _CALL_MATHERR
struct exception	exstruct;
#endif

	/* computes ln(|gamma(x)|) for negative x using the reflection
	 * formula gamma(-z) = pi/(-z*gamma(z)*sin(z*pi))
	 */

	z = __floor(arg);

	if ( z == arg )
	{
		/* If arg is a negative integer or zero, gamma(arg) is 
		 * undefined.
		 */

#ifdef _CALL_MATHERR

		exstruct.type = SING;
		exstruct.name = "lgamma";
		exstruct.arg1 = arg;
		exstruct.retval = Inf.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "singularity error in lgamma\n");
			SETERRNO(EDOM);
		}

		return ( exstruct.retval );
#else
		SETERRNO(EDOM);

		return ( Inf.d );
#endif
	}

	n = z;

	/* if floor(arg) is odd, sign(gamma(arg)) is negative */

	if ( n&1 ) {
		signgam = -1;
#if defined(BUILD_OS_DARWIN)
		__signgam = signgam;
#endif /* defined(BUILD_OS_DARWIN) */
	}

	h = arg - z;	/* 0.0 < h < 1.0 */

	/* notice that sin((n+h)*pi) = +/-sin(h*pi) */

	if ( h > 0.5 )
		h = 1.0 - h;

	/* special case values > -0.5 so that h is exact */

	if ( arg > -0.5 )
		h = -arg;

	/* result = log( pi/fabs(arg*sin(pi*h)) ) - pos(-arg) */

	hsq = h*h;

	sinpoly = (((((((p2[8].d*hsq + p2[7].d)*hsq + p2[6].d)*hsq + 
			 p2[5].d)*hsq + p2[4].d)*hsq + p2[3].d)*hsq +
			 p2[2].d)*hsq + p2[1].d)*hsq*h + h;

	result = -__log(fabs(arg*sinpoly)) - pos(-arg);

	return ( result );
}

static
double
asym( double x )
{
double	xsq;
double	y, num, denom;
double	phi, result;
#ifdef _CALL_MATHERR
struct exception	exstruct;
#endif

	if ( x < 1024.0 )
	{
		/* between 8 and 1024, we approximate phi by 
		 * 1/x*poly(1/(x*x))
		 */

		xsq = 1.0/(x*x);

		phi = (((((p1[5]*xsq + p1[4])*xsq + p1[3])*xsq + p1[2])*xsq +
			p1[1])*xsq + p1[0])/x;

	}
	else if ( x < twop100.d )
	{
		/* for large values, use a continued fraction to approximate phi */

		y = x*x;
		num = x*(11130.0*y + 8659.0);
		denom = y*(133560.0*y + 108360.0) + 2340.0;
		phi = num/denom;
	}
	else if ( x < twop1000.d )
	{
		/* for even larger values, approximate phi by 1/(12*x) */

		phi = 1.0/(12.0*x);
	}
	else if ( x <= maxarg.d )
	{
		/* For anything this large, phi is negligible compared to
		 * the rest of the terms.
		 */

		return ( x*(__log(x) - 1.0) );
	}
	else
	{
#ifdef _CALL_MATHERR

		exstruct.type = OVERFLOW;
		exstruct.name = "lgamma";
		exstruct.arg1 = x;
		exstruct.retval = Inf.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "overflow error in lgamma\n");
			SETERRNO(ERANGE);
		}

		return ( exstruct.retval );
#else
		SETERRNO(ERANGE);

		return ( Inf.d );
#endif
	}

	/* Formula 6.6.19 from Hart and Cheney's book */

	result = (x - 0.5)*__log(x) - x + halfln2pi.d + phi;

	return ( result );
}

static
double
lgmapp( int j, double h )
{
double n, d;
double result;

	/* approximates lgamma on [.875, 3.125] using one of 9 different
	 * rational approximations
	 */

	/* j = ROUND(4.0*arg);
	 * x0 = j/4.0;
	 * h = arg - x0 
	 */

	/* use a higher degree approximation around 1.0 */

	if ( j == 4 )
	{
		n = (((((__S1[6].d*h + __S1[5].d)*h + __S1[4].d)*h +
			 __S1[3].d)*h + __S1[2].d)*h + __S1[1].d)*h + __S1[0].d;

		d = ((((((__T1[7].d*h  + __T1[6].d)*h + __T1[5].d)*h +
			__T1[4].d)*h + __T1[3].d)*h + __T1[2].d)*h +
			__T1[1].d)*h + __T1[0].d;

		result = h*n/d;

		return ( __log1p(result) );
	}

	n = ((((__S0[j-5][7].d*h + __S0[j-5][6].d)*h + __S0[j-5][5].d)*h + 
		   __S0[j-5][4].d)*h + __S0[j-5][3].d)*h + __S0[j-5][2].d;

	d = (((((__T0[j-5][6].d*h + __T0[j-5][5].d)*h + __T0[j-5][4].d)*h + 
	    __T0[j-5][3].d)*h + __T0[j-5][2].d)*h + __T0[j-5][1].d)*h +
	    __T0[j-5][0].d;

	result = __S0[j-5][0].d + (__S0[j-5][1].d + h*n/d);

	return ( __log1p(result) );
}

#ifdef NO_LONG_DOUBLE

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern  long double  __gammal(long double);
extern  long double  __lgammal(long double);
#pragma weak gammal
#pragma weak lgammal
#pragma weak signgaml
long double gammal(long double arg) {
  return __gammal(arg);
}
long double lgammal( long double arg ) {
  return __lgammal( arg );
}
#elif defined(__GNUC__)
extern  long double  __gammal(long double);

long double    gammal() __attribute__ ((weak, alias ("__gammal")));

extern  long double  __lgammal(long double);

long double    lgammal() __attribute__ ((weak, alias ("__lgammal")));

extern	int  __signgaml;

extern	int  signgaml  __attribute__ ((weak, alias ("__signgaml")));

#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
int __signgaml = 0;
int signgaml = 0;
#elif (__GNUC__ > 3)
int	__signgaml = 0;
#else
int	signgaml = 0;
#endif

long double
__gammal(long double arg)
{
double	result;

	result = __gamma((double)arg);
#ifdef KEY /* Mac port */
	/* Bug in all versions--signgaml doesn't get set */
        signgaml = __signgaml = __signgam;
#endif /* KEY Mac port */

	return (long double)result;
}


long double
__lgammal( long double arg )
{
	long double result = (long double)__lgamma((double)arg);
#ifdef KEY /* Mac port */
	/* Bug in all versions--signgaml doesn't get set */
        signgaml = __signgaml = __signgam;
#endif /* KEY Mac port */
	return result;
}

#endif

