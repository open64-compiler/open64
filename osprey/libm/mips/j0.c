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
 * Module: j0.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:22-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.j0.c $
 *
 * Revision history:
 *  20-Mar-98 - Original Version
 *
 * Description:	source code for bessel functions of the first and second
 *	kinds of order zero
 *
 *	Based on the ideas in Hart & Cheney's book, Computer Approximations,
 *	John Wiley and Sons, 1968, Section 6.8.
 *
 *	j0:
 *
 *	Calls sin, cos, sqrt.
 *
 *	y0:
 *
 *	Calls sin, cos, sqrt, log, j0.
 *
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.j0.c $ $Revision: 1.5 $";

/****************************  IMPORTANT NOTE  ****************************
 *
 *  - This file contains library routines which are "safe" for parallel
 *    processing.  This fact is indicated here and in 'math.h' by inclusion
 *    of the following directive(s):
 *
 *        #pragma no side effects (j0)
 *        #pragma no side effects (y0)
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

#ifdef _CALL_MATHERR
#include <stdio.h>
#include <math.h>
#include <errno.h>
#endif

#include "libm.h"

#if defined(mips) && !defined(__GNUC__)
extern double j0(double);
extern double y0(double);

#pragma weak j0 = __j0
#pragma weak y0 = __y0
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern double __j0(double);
extern double __y0(double);
#pragma weak j0
#pragma weak y0
double j0( double arg ) {
  return __j0( arg );
}
double y0( double x ) {
  return __y0( x );
}
#elif defined(__GNUC__)
extern  double  __j0(double);

double    j0() __attribute__ ((weak, alias ("__j0")));

extern  double  __y0(double);

double    y0() __attribute__ ((weak, alias ("__y0")));

#endif

extern double __sin(double);
extern double __cos(double);
extern double __log(double);

static double p0(double), q0(double);

static const du twobypi =	/* 2.0/pi */
{
 D(0x3fe45f30, 0x6dc9c883)
};

static const du rsqrtpi =	/* 1.0/sqrt(pi) */
{
 D(0x3fe20dd7, 0x50429b6d)
};

static const du k0 =	/* 2.0/pi*(euler - log(2.0)) */
{
 D(0xbfb2e4d6, 0x99cbd01f)
};

/* coefficients for rational approximation of j0
   on +/- 8.0
*/

static const du s1[] =
{
 D(0xbfd00000, 0x00000000),
 D(0x3f8a5dd2, 0x07400a00),
 D(0xbf322e84, 0x5625ccf7),
 D(0x3ec86226, 0xdcfbebcf),
 D(0xbe519953, 0xd3612c5b),
 D(0x3dcbf06d, 0xa8ff6623),
 D(0xbd371d0d, 0x96e69a82),
 D(0x3c8f6396, 0x070cb706),
};

static const du t1[] =
{
 D(0x3ff00000, 0x00000000),
 D(0x3f8688b7, 0xe2ffd7ff),
 D(0x3f101192, 0xb33f1e37),
 D(0x3e8eac3a, 0x456bf6e2),
 D(0x3e05be02, 0xd273ac07),
 D(0x3d77e0cb, 0x01046ec0),
 D(0x3ce45590, 0x9323449e),
 D(0x3c497437, 0x5fe08079),
 D(0x3ba31c0e, 0x7b50a76b),
};

/* coefficients for rational approximation of p0 on (0.0, 1.0) */

static const du s2[] =
{
 D(0xbf51ffff, 0xffffffb8),
 D(0xbf6469e4, 0x36e62881),
 D(0xbf5b424a, 0x11c914b4),
 D(0xbf38b387, 0x1e57cade),
 D(0xbef99508, 0xd4953bc5),
 D(0xbe89282f, 0x2055d015),
};

static const du t2[] =
{
 D(0x3ff00000, 0x00000000),
 D(0x40025847, 0x4d3e52b1),
 D(0x3ff91cde, 0x901a39c4),
 D(0x3fd83087, 0x0c521255),
 D(0x3f9dde76, 0xb8de94f3),
 D(0x3f400cef, 0x4c143276),
 D(0x3e89282f, 0x2055d015),
};

/* coefficients for rational approximation of q0 on (0.0, 1.0) */

static const du s3[] = 
{
 D(0xbf900000, 0x00000000),
 D(0x3f22bfff, 0xfffffe5d),
 D(0x3f381dc6, 0x0bdd69d3),
 D(0x3f328f71, 0xc888d5b2),
 D(0x3f13cc8a, 0x06fa2970),
 D(0x3ed8e553, 0xf75ae5b9),
 D(0x3e70a26b, 0xd3f48e4f),
};

static const du t3[] = 
{
 D(0x3ff00000, 0x00000000),
 D(0x4004f783, 0x06b5e18d),
 D(0x4000ced3, 0x6118ee31),
 D(0x3fe3b5ca, 0x55784db2),
 D(0x3fafcd81, 0xa67601cb),
 D(0x3f5a87e4, 0x0ce687ca),
 D(0x3ed0a26b, 0xd3f48e4f),
};

/* coefficients for rational approximation of y0 on (0.0, 8.0) */

static const du s4[] =
{
 D(0xc0034066, 0x8510b96f),
 D(0x3fc86981, 0x11fdbb9b),
 D(0xbf747d16, 0xe102a52f),
 D(0x3f0f6db4, 0x4d204dbf),
 D(0xbe99068d, 0xa7ea12f5),
 D(0x3e1577e8, 0xd5bafc4a),
 D(0xbd82ec16, 0x2f225a36),
 D(0x3cdb1b0b, 0x6c893369),
};

static const du t4[] =
{
 D(0x3ff00000, 0x00000000),
 D(0x3f870a22, 0xf33840f5),
 D(0x3f10d4c2, 0x2520436c),
 D(0x3e907f07, 0xa7ebc30f),
 D(0x3e0815f6, 0x8930073b),
 D(0x3d7b583a, 0x90523001),
 D(0x3ce831c7, 0xe53c886c),
 D(0x3c4fb50f, 0x5f529cf6),
 D(0x3ba94ca1, 0x99c520de),
};

static const du Qnan =
{D(QNANHI, QNANLO)};

static const du Neginf =
{D(0xfff00000, 0x00000000)};


/* ====================================================================
 *
 * FunctionName		j0
 *
 * Description		computes bessel function of first kind of order 0
 *
 * ====================================================================
 */

double
__j0( double arg )
{
#ifdef _32BIT_MACHINE
int	ix, xpt;
#else
long long ix, xpt;
#endif
double x;
double xsq, rx;
double num, denom;
double	s, c, t;
double result;
#ifdef _CALL_MATHERR
struct exception	exstruct;
#endif

	if( arg != arg )
	{
		/* arg is NaN */

#ifdef _CALL_MATHERR

		exstruct.type = DOMAIN;
		exstruct.name = "j0";
		exstruct.arg1 = arg;
		exstruct.retval = Qnan.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "domain error in j0\n");
			SETERRNO(EDOM);
		}

		return ( exstruct.retval );
#else
		NAN_SETERRNO(EDOM);

		return ( Qnan.d );
#endif
	}

	x = fabs(arg);

	/* extract exponent of x for some quick screening */

#ifdef _32BIT_MACHINE
	DBLHI2INT(x, ix);	/* copy hi part of x to ix */
#else
	DBL2LL(x, ix);		/* copy x to ix	*/
#endif

	xpt = (ix >> DMANTWIDTH);
	xpt &= 0x7ff;

	if ( xpt < 0x402 )
	{
		/*      |x| < 8.0	*/

		if ( xpt >= 0x3e4 )
		{
			/* |x| >= 2^(-27) */

			rx = x;
			xsq = rx*rx;

			num = ((((((s1[7].d*xsq + s1[6].d)*xsq +
				s1[5].d)*xsq + s1[4].d)*xsq + s1[3].d)*xsq +
				s1[2].d)*xsq + s1[1].d)*xsq + s1[0].d;

			denom = (((((((t1[8].d*xsq + t1[7].d)*xsq + t1[6].d)*xsq +
				t1[5].d)*xsq + t1[4].d)*xsq + t1[3].d)*xsq +
				t1[2].d)*xsq + t1[1].d)*xsq + t1[0].d;

			result = 1.0 + xsq*(num/denom);

			return ( (double)result );
		}
		else
		{
			return ( 1.0 );
		}
	}
	else if ( xpt < 0x430 )
	{
		rx = x;
		c = __cos(x);
		s = __sin(x);
		t = 8.0/rx;
	
		/* j0(x) = 1/sqrt(pi)*(p0(t)*(s+c) - q0(t)*(s-c))/sqrt(x) */
	
		/* Use identity sin(x) +/- cos(x) =
		   -cos(2*x)/(sin(x) -/+ cos(x))
		   to rewrite s +/- c to avoid loss of significance due to
		   subtraction.  Note that sin and cos are only defined
		   out to 2**50 on this system.
		*/
	
		if ( (c*s) >= 0.0 )
		{
			result = 
			p0(t)*(s + c) + q0(t)*(__cos(x + x)/(s + c));
		}
		else
		{
			result = 
			-p0(t)*(__cos(x + x)/(s - c)) - q0(t)*(s - c);
		}
	
		return ( rsqrtpi.d*result/sqrt(rx) );
	}
	else
	{
		/* |x| exceeds 2**49; return zero  */

#ifdef _CALL_MATHERR

		exstruct.type = TLOSS;
		exstruct.name = "j0";
		exstruct.arg1 = arg;
		exstruct.retval = 0.0;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "total loss of significance \
error in j0\n");
			SETERRNO(ERANGE);
		}

		return ( exstruct.retval );
#else
		SETERRNO(ERANGE);

		return ( 0.0 );
#endif
	}

}


/* ====================================================================
 *
 * FunctionName		y0
 *
 * Description		computes bessel function of second kind of order 0
 *
 * ====================================================================
 */

double
__y0( double x )
{
#ifdef _32BIT_MACHINE
int	ix, xpt;
#else
long long ix, xpt;
#endif
double	rx;
double	xsq, t;
double	num, denom;
double	s, c;
double	result;
#ifdef _CALL_MATHERR
struct exception	exstruct;
#endif

	if( x != x )
	{
		/* arg is NaN */

#ifdef _CALL_MATHERR

		exstruct.type = DOMAIN;
		exstruct.name = "y0";
		exstruct.arg1 = x;
		exstruct.retval = Qnan.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "domain error in y0\n");
			SETERRNO(EDOM);
		}

		return ( exstruct.retval );
#else
		NAN_SETERRNO(EDOM);

		return ( Qnan.d );
#endif
	}

	if ( x == 0.0 )
	{
#ifdef _CALL_MATHERR

		exstruct.type = OVERFLOW;
		exstruct.name = "y0";
		exstruct.arg1 = x;
		exstruct.retval = Neginf.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "overflow range error in y0\n");
			SETERRNO(ERANGE);
		}

		return ( exstruct.retval );
#else
		SETERRNO(ERANGE);

		return ( Neginf.d );
#endif
	}

	if ( x < 0.0 )
	{
#ifdef _CALL_MATHERR

		exstruct.type = DOMAIN;
		exstruct.name = "y0";
		exstruct.arg1 = x;
		exstruct.retval = Qnan.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "domain error in y0\n");
			SETERRNO(EDOM);
		}

		return ( exstruct.retval );
#else
		SETERRNO(EDOM);

		return ( Qnan.d );
#endif
	}

	/* extract exponent of x for some quick screening */

#ifdef _32BIT_MACHINE
	DBLHI2INT(x, ix);	/* copy hi part of x to ix */
#else
	DBL2LL(x, ix);		/* copy x to ix	*/
#endif

	xpt = (ix >> DMANTWIDTH);
	xpt &= 0x7ff;

	if ( xpt < 0x402 )
	{
		/*      |x| < 8.0	*/

		if ( xpt >= 0x3e4 )
		{
			/* |x| >= 2^(-27) */

			rx = x;
			xsq = rx*rx;

			num = ((((((s4[7].d*xsq + s4[6].d)*xsq + s4[5].d)*xsq +
				s4[4].d)*xsq + s4[3].d)*xsq + s4[2].d)*xsq +
				s4[1].d)*xsq + s4[0].d;

			denom = (((((((t4[8].d*xsq + t4[7].d)*xsq + t4[6].d)*xsq +
				t4[5].d)*xsq + t4[4].d)*xsq + t4[3].d)*xsq +
				t4[2].d)*xsq + t4[1].d)*xsq + t4[0].d;

			result = k0.d + k0.d*(xsq*num/denom) +
				twobypi.d*(__j0(x)*__log(x));

			return ( (double)result );
		}
		else
		{
			result = k0.d + twobypi.d*__log(x);

			return ( (double)result );
		}
	}
	else if ( xpt < 0x430 )
	{
		rx = x;
		c = __cos(x);
		s = __sin(x);
		t = 8.0/rx;
	
		/* y0(x) = 1/sqrt(pi)*(p0(t)*(s-c) + q0(t)*(s+c))/sqrt(x) */
	
		/* Use identity sin(x) +/- cos(x) =
		   -cos(2*x)/(sin(x) -/+ cos(x))
		   to rewrite s +/- c to avoid loss of significance due to
		   subtraction.  Note that sin and cos are only defined
		   out to 2**50 on this system.
		*/
	
		if ( (c*s) >= 0.0 )
		{
			result = 
			-p0(t)*(__cos(x + x)/(s + c)) + q0(t)*(s + c);
		}
		else
		{
			result = 
			p0(t)*(s - c) - q0(t)*(__cos(x + x)/(s - c));
		}
	
		return ( rsqrtpi.d*result/sqrt(rx) );
	}
	else
	{
		/* |x| exceeds 2**49; return zero  */

#ifdef _CALL_MATHERR

		exstruct.type = TLOSS;
		exstruct.name = "y0";
		exstruct.arg1 = x;
		exstruct.retval = 0.0;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "total loss of significance \
error in y0\n");
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
p0( double t )
{
double	tsq;
double	num, denom, result;

	tsq = t*t;

	num = ((((s2[5].d*tsq + s2[4].d)*tsq + s2[3].d)*tsq +
			s2[2].d)*tsq + s2[1].d)*tsq + s2[0].d;

	denom = (((((t2[6].d*tsq + t2[5].d)*tsq + t2[4].d)*tsq +
			t2[3].d)*tsq  + t2[2].d)*tsq + t2[1].d)*tsq + 
			t2[0].d;


	result = 1.0 + tsq*(num/denom);

	return ( result );
}

static
double
q0( double t )
{
double	tsq;
double	num, denom, result;

	tsq = t*t;

	num = ((((s3[6].d*tsq + s3[5].d)*tsq + s3[4].d)*tsq +
			s3[3].d)*tsq + s3[2].d)*tsq + s3[1].d;

	denom = (((((t3[6].d*tsq + t3[5].d)*tsq + t3[4].d)*tsq +
			t3[3].d)*tsq  + t3[2].d)*tsq + t3[1].d)*tsq + 
			t3[0].d;


	result = s3[0].d*t + tsq*t*num/denom;

	return ( result );
}

#ifdef NO_LONG_DOUBLE

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern  long double  __j0l(long double);
extern  long double  __y0l(long double);
long double j0l( long double x ) {	
  return ( (long double)__j0((double)x) );
}

long double y0l( long double x ) {	
  return ( (long double)__y0((double)x) );
}
#elif defined(__GNUC__)
extern  long double  __j0l(long double);

long double    j0l() __attribute__ ((weak, alias ("__j0l")));

extern  long double  __y0l(long double);

long double    y0l() __attribute__ ((weak, alias ("__y0l")));

#endif

long double
__j0l( long double x )
{	
	return ( (long double)__j0((double)x) );
}

long double
__y0l( long double x )
{	
	return ( (long double)__y0((double)x) );
}

#endif

