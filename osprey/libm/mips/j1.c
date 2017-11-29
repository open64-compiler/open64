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
 * Module: j1.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:22-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.j1.c $
 *
 * Revision history:
 *  28-May-93 - Original Version
 *
 * Description:	source code for bessel functions of first and second kind
 *		of order 1
 *
 * ====================================================================
 * ====================================================================
 */

/* $Header: /home/bos/bk/kpro64-pending/libm/mips/j1.c 1.5 04/12/21 14:58:22-08:00 bos@eng-25.internal.keyresearch.com $ */

/****************************  IMPORTANT NOTE  ****************************
 *
 *  - This file contains library routines which are "safe" for parallel
 *    processing.  This fact is indicated here and in 'math.h' by inclusion
 *    of the following directive(s):
 *
 *        #pragma no side effects (j1)
 *        #pragma no side effects (y1)
 *
 *  - To be considered safe a routine cannot do any I/O (including GL calls),
 *    modify any global location ('errno' for example) or have global state
 *    (as 'random' does).  Nor can it call any other routine which does any
 *    of these things.
 *
 *  - If in the future any of these routines are modified so that they are
 *    no longer "safe", then the appropriate 'pragma no side effects' must
 *    be manually removed from 'math.h' and this file.
 *
 *  - Note that this pragma only has meaning to Power C ('pca') and is
 *    properly passed on by 'cpp' & 'c++' and ignored by 'ccom'.
 *
 ******************************  END OF NOTE  ******************************/
/*
	floating point Bessel's function
	of the first and second kinds
	of order one

	j1:

	Calls sin, cos, sqrt.

	Coefficients are from Hart & Cheney.
	#6050 (20.98D)
	#6750 (19.19D)
	#7150 (19.35D)

	y1:

	Calls sin, cos, sqrt, log, j1.

	Coefficients are from Hart & Cheney.
	#6447 (22.18D)
	#6750 (19.19D)
	#7150 (19.35D)
*/

#ifdef _CALL_MATHERR
#include <stdio.h>
#include <math.h>
#endif

#include "libm.h"

#if defined(mips) && !defined(__GNUC__)
extern	double	j1(double);
extern	double	y1(double);

#pragma weak j1 = __j1
#pragma weak y1 = __y1
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern double __j1(double);
extern double __y1(double);
#pragma weak j1
#pragma weak y1
double j1( double arg ) {
  return __j1( arg );
}
double y1( double arg ) {
  return __y1( arg );
}
#elif defined(__GNUC__)
extern  double  __j1(double);

double    j1() __attribute__ ((weak, alias ("__j1")));

extern  double  __y1(double);

double    y1() __attribute__ ((weak, alias ("__y1")));

#endif

extern	double	__sin(double);
extern	double	__cos(double);
extern	double	__log(double);

static	double p1(double), q1(double);

static const	du	Qnan =
{D(QNANHI, QNANLO)};

static const	du	Neginf =
{D(0xfff00000, 0x00000000)};

static	const double twobypi =
	0.63661977236758134;

static	const double rsqrtpi =
	0.56418958354775629;

static double s1[] = {
	0.581199354001606143928050809e21,
	-.6672106568924916298020941484e20,
	0.2316433580634002297931815435e19,
	-.3588817569910106050743641413e17,
	0.2908795263834775409737601689e15,
	-.1322983480332126453125473247e13,
	0.3413234182301700539091292655e10,
	-.4695753530642995859767162166e7,
	0.2701122710892323414856790990e4,
};

static double t1[] = {
	0.1162398708003212287858529400e22,
	0.1185770712190320999837113348e20,
	0.6092061398917521746105196863e17,
	0.2081661221307607351240184229e15,
	0.5243710262167649715406728642e12,
	0.1013863514358673989967045588e10,
	0.1501793594998585505921097578e7,
	0.1606931573481487801970916749e4,
	1.0,
};

static double s2[] = {
         1.0,
         2.2467834456732897,
         1.4955846127260149,
         3.4576449324751314e-1,
         2.4969642938750994e-2,
         3.6696523370953188e-4,
};

static double t2[] = {
         1.0,
         2.2449523909857897,
         1.4915091861206051,
         3.4310990865393919e-1,
         2.4388513111364209e-2,
         3.3134568440823793e-4,
        -2.2792852359037530e-7,
};

static double s3[] = {
         4.6875e-2,
         1.2043362532202404e-1,
         9.3819655587348968e-2,
         2.6273811593862773e-2,
         2.4287960814178163e-3,
         5.0281167606313395e-5,
};

static double t3[] = {
         1.0,
         2.5735231344740127,
         2.0123005490948824,
         5.6866066418403355e-1,
         5.3928104387696746e-2,
         1.2314671563610277e-3,
         1.4272352241007432e-6,
};

static double s4[] = {
	-.9963753424306922225996744354e23,
	0.2655473831434854326894248968e23,
	-.1212297555414509577913561535e22,
	0.2193107339917797592111427556e20,
	-.1965887462722140658820322248e18,
	0.9569930239921683481121552788e15,
	-.2580681702194450950541426399e13,
	0.3639488548124002058278999428e10,
	-.2108847540133123652824139923e7,
};

static double t4[] = {
	0.5082067366941243245314424152e24,
	0.5435310377188854170800653097e22,
	0.2954987935897148674290758119e20,
	0.1082258259408819552553850180e18,
	0.2976632125647276729292742282e15,
	0.6465340881265275571961681500e12,
	0.1128686837169442121732366891e10,
	0.1563282754899580604737366452e7,
	0.1612361029677000859332072312e4,
	1.0,
};


/* ====================================================================
 *
 * FunctionName		j1
 *
 * Description		computes bessel function of first kind of order 1
 *
 * ====================================================================
 */

double
__j1( arg )
double arg;
{
#ifdef _32BIT_MACHINE
int	ix, xpt;
#else
long long ix, xpt;
#endif

double x, xsq;
double num, denom;
double c, s, t;
double result;
#ifdef _CALL_MATHERR
struct exception	exstruct;
#endif

	if( arg != arg )
	{
		/* arg is NaN */

#ifdef _CALL_MATHERR

		exstruct.type = DOMAIN;
		exstruct.name = "j1";
		exstruct.arg1 = arg;
		exstruct.retval = Qnan.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "domain error in j1\n");
			SETERRNO(EDOM);
		}

		return ( exstruct.retval );
#else
		NAN_SETERRNO(EDOM);

		return ( Qnan.d );
#endif
	}

	/* extract exponent of x for some quick screening */

#ifdef _32BIT_MACHINE

	DBLHI2INT(arg, ix);	/* copy MSW of arg to ix	*/
#else
	DBL2LL(arg, ix);	/* copy arg to ix	*/
#endif

	xpt = (ix >> DMANTWIDTH);
	xpt &= 0x7ff;

	x = fabs(arg);

	if ( xpt < 0x402 )
	{
		/*      |x| < 8.0	*/

		if ( xpt >= 0x3e4 )
		{
			/* |x| >= 2^(-27) */

			xsq = x*x;

			num = (((((((s1[8]*xsq  + s1[7])*xsq + s1[6])*xsq +
				s1[5])*xsq + s1[4])*xsq + s1[3])*xsq +
				s1[2])*xsq + s1[1])*xsq + s1[0];

			denom = (((((((t1[8]*xsq  + t1[7])*xsq + t1[6])*xsq +
				t1[5])*xsq + t1[4])*xsq + t1[3])*xsq +
				t1[2])*xsq + t1[1])*xsq + t1[0];

			result = x*num/denom;
		}
		else
		{
			result = 0.5*x;
		}
	}
	else if ( xpt < 0x430 )
	{

		c = __cos(x);
		s = __sin(x);
		t = 8.0/x;
	
		/* j1(x) = 1/sqrt(pi)*(p1(t)*(s-c) + q1(t)*(s+c))/sqrt(x) */
	
		/* Use identity sin(x) +/- cos(x) =
		   -cos(2*x)/(sin(x) -/+ cos(x))
		   to rewrite s +/- c to avoid loss of significance due to
		   subtraction.  Note that sin and cos are only defined
		   out to 2**50 on this system.
		*/
	
		if ( (c*s) >= 0.0 )
		{
			result = -p1(t)*(__cos(x + x)/(s + c)) + q1(t)*(s + c);
		}
		else
		{
			result = p1(t)*(s - c) - q1(t)*(__cos(x + x)/(s - c));
		}
	
		result = rsqrtpi*result/sqrt(x);
	}
	else
	{
#ifdef _CALL_MATHERR

		exstruct.type = TLOSS;
		exstruct.name = "j1";
		exstruct.arg1 = arg;
		exstruct.retval = 0.0;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "total loss of significance \
error in j1\n");
			SETERRNO(ERANGE);
		}

		return ( exstruct.retval );
#else
		SETERRNO(ERANGE);

		return ( 0.0 );
#endif
	}

	if ( ix >= 0 )
		return ( result );
	else
		return ( -result );

}


/* ====================================================================
 *
 * FunctionName		y1
 *
 * Description		computes bessel function of second kind of order 1
 *
 * ====================================================================
 */

double
__y1( arg )
double arg;
{
#ifdef _32BIT_MACHINE
int	ix, xpt;
#else
long long ix, xpt;
#endif

double x, xsq;
double num, denom, poly;
double s, c, t;
double result;
#ifdef _CALL_MATHERR
struct exception	exstruct;
#endif

	if( arg != arg )
	{
		/* arg is NaN */

#ifdef _CALL_MATHERR

		exstruct.type = DOMAIN;
		exstruct.name = "y1";
		exstruct.arg1 = arg;
		exstruct.retval = Qnan.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "domain error in y1\n");
			SETERRNO(EDOM);
		}

		return ( exstruct.retval );
#else
		NAN_SETERRNO(EDOM);

		return ( Qnan.d );
#endif
	}

	if ( arg == 0.0 )
	{
#ifdef _CALL_MATHERR

		exstruct.type = OVERFLOW;
		exstruct.name = "y1";
		exstruct.arg1 = arg;
		exstruct.retval = Neginf.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "overflow range error in y1\n");
			SETERRNO(ERANGE);
		}

		return ( exstruct.retval );
#else
		SETERRNO(ERANGE);

		return ( Neginf.d );
#endif
	}

	if ( arg < 0.0 )
	{
#ifdef _CALL_MATHERR

		exstruct.type = DOMAIN;
		exstruct.name = "y1";
		exstruct.arg1 = arg;
		exstruct.retval = Qnan.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "domain error in y1\n");
			SETERRNO(EDOM);
		}

		return ( exstruct.retval );
#else
		SETERRNO(EDOM);

		return ( Qnan.d );
#endif
	}

	x = arg;

	/* extract exponent of x for some quick screening */

#ifdef _32BIT_MACHINE

	DBLHI2INT(x, ix);	/* copy MSW of x to ix	*/
#else
	DBL2LL(x, ix);		/* copy x to ix	*/
#endif

	xpt = (ix >> DMANTWIDTH);
	xpt &= 0x7ff;

	if ( xpt < 0x402 )
	{
		/*      |x| < 8.0	*/

		if ( xpt >= 0x3c9 )
		{
			/* |x| >= 2^(-54) */

			xsq = x*x;

			num = (((((((s4[8]*xsq + s4[7])*xsq + s4[6])*xsq +
				s4[5])*xsq + s4[4])*xsq + s4[3])*xsq +
				s4[2])*xsq + s4[1])*xsq + s4[0];

			denom = ((((((((t4[9]*xsq + t4[8])*xsq + t4[7])*xsq +
				t4[6])*xsq + t4[5])*xsq + t4[4])*xsq + t4[3])*xsq +
				t4[2])*xsq + t4[1])*xsq + t4[0];

			poly = num/denom;

			result = x*poly + twobypi*(__j1(x)*__log(x) - 1.0/x);

			return ( result );
		}
		else
		{
			return ( -twobypi/x );
		}

	}
	else if ( xpt < 0x430 )
	{
		c = __cos(x);
		s = __sin(x);
		t = 8.0/x;
	
		/* y1(x) = 1/sqrt(pi)*(-p1(t)*(s+c) + q1(t)*(s-c))/sqrt(x) */
	
		/* Use identity sin(x) +/- cos(x) =
		   -cos(2*x)/(sin(x) -/+ cos(x))
		   to rewrite s +/- c to avoid loss of significance due to
		   subtraction.  Note that sin and cos are only defined
		   out to 2**50 on this system.
		*/
	
		if ( (c*s) >= 0.0 )
		{
			result = -p1(t)*(s + c) - q1(t)*(__cos(x + x)/(s + c));
		}
		else
		{
			result = p1(t)*(__cos(x + x)/(s - c)) + q1(t)*(s - c);
		}
	
		return ( rsqrtpi*result/sqrt(arg) );
	}
	else
	{
		/* |x| exceeds 2**49; return zero  */

#ifdef _CALL_MATHERR

		exstruct.type = TLOSS;
		exstruct.name = "y1";
		exstruct.arg1 = x;
		exstruct.retval = 0.0;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "total loss of significance \
error in y1\n");
			SETERRNO(ERANGE);
		}

		return ( exstruct.retval );
#else
		SETERRNO(ERANGE);

		return ( 0.0 );
#endif
	}
}

static
double
p1( double t )
{
double	tsq;
double	num, denom, result;

	tsq = t*t;

	num = ((((s2[5]*tsq + s2[4])*tsq + s2[3])*tsq +
		s2[2])*tsq + s2[1])*tsq + s2[0];

	denom = (((((t2[6]*tsq + t2[5])*tsq + t2[4])*tsq +
		t2[3])*tsq + t2[2])*tsq + t2[1])*tsq + t2[0];

	result = num/denom;

	return ( result );
}

static
double
q1( double t )
{
double	tsq;
double	num, denom, result;

	tsq = t*t;

	num = ((((s3[5]*tsq + s3[4])*tsq + s3[3])*tsq +
		s3[2])*tsq + s3[1])*tsq + s3[0];

	denom = (((((t3[6]*tsq + t3[5])*tsq + t3[4])*tsq +
		t3[3])*tsq + t3[2])*tsq + t3[1])*tsq + t3[0];

	result = t*num/denom;

	return ( result );
}

#ifdef NO_LONG_DOUBLE

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern long double __j1l(long double);
extern long double __y1l(long double);
#pragma weak j1l
#pragma weak y1l
long double j1l( long double x ) {
  return ( (long double)__j1((double)x) );
}
long double y1l( long double x ) {
  return ( (long double)__y1((double)x) );
}
#elif defined(__GNUC__)
extern  long double  __j1l(long double);

long double    j1l() __attribute__ ((weak, alias ("__j1l")));

extern  long double  __y1l(long double);

long double    y1l() __attribute__ ((weak, alias ("__y1l")));

#endif

long double
__j1l( long double x )
{	
	return ( (long double)__j1((double)x) );
}

long double
__y1l( long double x )
{	
	return ( (long double)__y1((double)x) );
}

#endif

