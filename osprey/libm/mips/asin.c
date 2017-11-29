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

/* ====================================================================
 * ====================================================================
 *
 * Module: asin.c
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/libm/mips/asin.c,v $
 *
 * Revision history:
 *  09-Jun-93 - Original Version
 *
 * Description:	source code for asin function
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /proj/osprey/CVS/open64/osprey1.0/libm/mips/asin.c,v $ $Revision: 1.1.1.1 $";

#ifdef _CALL_MATHERR
#include <stdio.h>
#include <math.h>
#include <errno.h>
#endif

#include "libm.h"

#if defined(mips) && !defined(__GNUC__)
extern	double	asin(double);

#pragma weak asin = __asin
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern double __asin(double);
#pragma weak asin
double asin( double x ) {
  return __asin( x );
}
#elif defined(__GNUC__)
extern  double  __asin(double);

double    asin() __attribute__ ((weak, alias ("__asin")));

#endif

/* coefficients for rational approximation of asin on +/- 0.5    */

static const du	P[] =
{
{D(0x00000000, 0x00000000)},
{D(0x400cf4d7, 0x1166375d)},
{D(0xc019dde2, 0xfd680d32)},
{D(0x400c15ee, 0x4cc68a6a)},
{D(0xbfe20f7e, 0xdc1c40fe)},
{D(0x3f780cd5, 0x52bc78fd)},
};

static const du	Q[] =
{
{D(0x4035b7a1, 0x4d0ca925)},
{D(0xc0484954, 0xef63fb64)},
{D(0x40428d6e, 0x183c02d2)},
{D(0xc026104e, 0x748fbc12)},
{D(0x3ff00000, 0x00000000)},
};

/* coefficients for rational approximation of asin on +/- sqrt(2 - sqrt(3))/2 */

static const du	P2[] =
{
{D(0x00000000, 0x00000000)},
{D(0xc0097a02, 0x2e7a0e13)},
{D(0x4009aeb0, 0x0736a7c7)},
{D(0xbfe37279, 0x9e195a2e)},
};

static const du	Q2[] =
{
{D(0xc0331b81, 0xa2db8f8a)},
{D(0x403bdc31, 0x8eb7aeb5)},
{D(0xc0262173, 0xdc9ece8a)},
{D(0x3ff00000, 0x00000000)},
};

static const	du	Qnan =
{D(QNANHI, QNANLO)};

static const du	one =
{D(0x3ff00000, 0x00000000)};

static const du	m_one =
{D(0xbff00000, 0x00000000)};

static const du	piby4 =
{D(0x3fe921fb, 0x54442d18)};

static const du	piby2 =
{D(0x3ff921fb, 0x54442d18)};

static const du	m_piby2 =
{D(0xbff921fb, 0x54442d18)};

#ifdef _32BIT_MACHINE

static const int	root3by2 = 0x3febb67a;

#else

static const long long	root3by2 = 0x3febb67ae8584caall;

#endif


/* ====================================================================
 *
 * FunctionName		asin
 *
 * Description		computes arcsine of arg
 *
 * ====================================================================
 */

double
__asin( double x )
{
#ifdef _32BIT_MACHINE

int	ix, xpt;
unsigned int	iabsx;

#else

long long ix, xpt;
unsigned long long iabsx;

#endif

double	xsq, num, denom, result;
double	y, ysq;
double	q, absx;
#ifdef _CALL_MATHERR
struct exception	exstruct;
#endif

	/* extract exponent of x for some quick screening */

#ifdef _32BIT_MACHINE

	DBLHI2INT(x, ix);
#else
	DBL2LL(x, ix);
#endif

	iabsx = (ix << 1);
	iabsx >>= 1;
	xpt = (iabsx >> DMANTWIDTH);

	if ( xpt < 0x3fe )
	{
		/* |x| < 0.5    */

		if ( xpt >= 0x3e3 )
		{
			/* |x| >= 2^(-28)   */

			xsq = x*x;
			num = ((((P[5].d*xsq + P[4].d)*xsq + P[3].d)*xsq + P[2].d)*xsq +
				P[1].d);

			denom = ((((xsq + Q[3].d)*xsq + Q[2].d)*xsq + Q[1].d)*xsq +
				Q[0].d);

			result = x + x*(xsq*num)/denom;

			return ( result );
		}

		return ( x );
	}
	
	if ( xpt < 0x3ff )
	{
		/*  |x| < 1.0   */

		if ( iabsx < root3by2 )
		{
			/* |x| < sqrt(3)/2    */

			/* uses the identity asin(x) = 
			 * pi/4 + .5*asin(2*x**2 - 1)
			 */

			xsq = x*x;
			y = xsq + xsq - one.d;
			xsq = y*y;
	
			num = ((((P[5].d*xsq + P[4].d)*xsq + P[3].d)*xsq + P[2].d)*xsq +
				P[1].d);
	
			denom = ((((xsq + Q[3].d)*xsq + Q[2].d)*xsq + Q[1].d)*xsq +
				Q[0].d);
	
			result = y + y*(xsq*num)/denom;

			result = piby4.d + 0.5*result;

			if ( x < 0.0 )
				result = -result;
	
			return ( result );
		}

		/* uses the identity asin(x) = pi/2 - 2*asin(sqrt(.5*(1 - x)) */

		absx = fabs(x);

		ysq = 0.5*(one.d - absx);
		y = sqrt(ysq);

		num = ((P2[3].d*ysq + P2[2].d)*ysq + P2[1].d);
		denom = (((ysq + Q2[2].d)*ysq + Q2[1].d)*ysq + Q2[0].d);

		q = y + y*(ysq*num)/denom;

		result = piby2.d - (q + q);

		if ( x < 0.0 )
			result = -result;

		return ( result );
	}

	if ( x != x )
	{
		/* x is a NaN; return a quiet NaN */

#ifdef _CALL_MATHERR

		exstruct.type = DOMAIN;
		exstruct.name = "asin";
		exstruct.arg1 = x;
		exstruct.retval = Qnan.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "domain error in asin\n");
			SETERRNO(EDOM);
		}

		return ( exstruct.retval );
#else
		NAN_SETERRNO(EDOM);

		return ( Qnan.d );
#endif
	}

	if ( x == one.d )
		return ( piby2.d );

	if ( x == m_one.d )
		return ( m_piby2.d );

	/*  |x| > 1.0; return a NaN */

#ifdef _CALL_MATHERR

	exstruct.type = DOMAIN;
	exstruct.name = "asin";
	exstruct.arg1 = x;
	exstruct.retval = Qnan.d;

	if ( matherr( &exstruct ) == 0 )
	{
		fprintf(stderr, "domain error in asin\n");
		SETERRNO(EDOM);
	}

	return ( exstruct.retval );
#else
	SETERRNO(EDOM);

	return ( Qnan.d );
#endif
}

#ifdef NO_LONG_DOUBLE

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
#pragma weak asinl
long double asinl( long double x ) {	
	return ( (long double)__asin((double)x) );
}
#elif defined(__GNUC__)
extern  long double  __asinl(long double);

long double    asinl() __attribute__ ((weak, alias ("__asinl")));

#endif

long double
__asinl( long double x )
{	
	return ( (long double)__asin((double)x) );
}

#endif

