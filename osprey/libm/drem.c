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
 * Module: drem.c
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/libm/drem.c,v $
 *
 * Revision history:
 *  09-Jun-93 - Original Version
 *
 * Description:	source code for drem function
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /proj/osprey/CVS/open64/osprey1.0/libm/drem.c,v $ $Revision: 1.1.1.1 $";

#ifdef _CALL_MATHERR
#include <stdio.h>
#include <math.h>
#endif

#include "libm.h"

#if defined(mips) && !defined(__GNUC__)

#include <ieeefp.h>

extern	fp_rnd	swapRM(fp_rnd);
#endif

#ifdef __GNUC__

#include <fenv.h>

extern	int	swapRM(int);
#endif

#if defined(mips) && !defined(__GNUC__)
extern	double	drem(double, double);
extern	double	remainder(double, double);
extern	double	__remainder(double, double);

#pragma weak drem = __drem
#pragma weak remainder = __drem
#pragma weak __remainder = __drem
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern double __drem( double x, double y );
double drem( double x, double y ) {
  return __drem(x, y);
}
#pragma weak drem
double remainder( double x, double y ) {
  return __remainder(x, y);
}
#pragma weak remainder
#elif defined(__GNUC__)
extern  double  __drem(double, double);

double    drem() __attribute__ ((weak, alias ("__drem")));

extern  double  __remainder(double, double);

double    remainder() __attribute__ ((weak, alias ("__remainder")));

#endif

static const	du	Qnan =
{D(QNANHI, QNANLO)};

static	const	du	twopm1021 =
{D(0x00200000, 0x00000000)};

static	const	du	twopm104 =
{D(0x39700000, 0x00000000)};

static	const	du	twop52 =
{D(0x43300000, 0x00000000)};

static	const	du	twop104 =
{D(0x46700000, 0x00000000)};

static	const	du	twop920 =
{D(0x79700000, 0x00000000)};

#define	NO	0
#define	YES	1


/* ====================================================================
 *
 * FunctionName		drem
 *
 * Description		computes remainder function
 *
 * ====================================================================
 */

double
__drem( double x, double y )
{
#ifdef _32BIT_MACHINE

int	ix, iy;
int	xptx, xpty;
int	m;

#else

long long ix, iy;
long long xptx, xpty;
long long m;

#endif

#if defined(mips) && !defined(__GNUC__)
fp_rnd	rm;
#endif

#ifdef __GNUC__
int	rm;
#endif

int	scaled;
int	signx;
double	nd, yhi, ylo;
double	y1;
double	result;
int	n, q;
#ifdef _CALL_MATHERR
struct exception	exstruct;
#endif

	/* extract exponents of y and x for some quick screening */

#ifdef _32BIT_MACHINE

	DBLHI2INT(x, ix);	/* copy MSW's of x and y to integers	*/
	DBLHI2INT(y, iy);
#else
	DBL2LL(x, ix);		/* copy x and y to long longs	*/
	DBL2LL(y, iy);
#endif
	xptx = (ix >> DMANTWIDTH);
	signx = (xptx >> DEXPWIDTH);
	xptx &= 0x7ff;
	signx &= 1;

	xpty = (iy >> DMANTWIDTH);
	xpty &= 0x7ff;

	/* filter out Nans */

	if ( (xpty == 0x7ff) || (xptx == 0x7ff) )
	{
		if ( (y != y) || (x != x) )
		{
			/* y or x is a NaN; return a quiet NaN */

#ifdef _CALL_MATHERR

			exstruct.type = DOMAIN;
			exstruct.name = "drem";
			exstruct.arg1 = x;
			exstruct.arg2 = y;
			exstruct.retval = Qnan.d;

			if ( matherr( &exstruct ) == 0 )
			{
				fprintf(stderr, "domain error in drem\n");
				SETERRNO(EDOM);
			}

			return ( exstruct.retval );
#else
			NAN_SETERRNO(EDOM);

			return ( Qnan.d );
#endif
		}
	}

	if ( (xptx == 0x7ff) || ((xpty == 0) && (y == 0.0)) )
	{
		/* x == +/- infinity or y == +/- 0.0; return a quiet NaN */

#ifdef _CALL_MATHERR

		exstruct.type = DOMAIN;
		exstruct.name = "drem";
		exstruct.arg1 = x;
		exstruct.arg2 = y;
		exstruct.retval = Qnan.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "domain error in drem\n");
			SETERRNO(EDOM);
		}

		return ( exstruct.retval );
#else
		SETERRNO(EDOM);

		return ( Qnan.d );
#endif
	}

	y = fabs(y);
	x = fabs(x);

	/* set rounding mode to round to zero */
	
#if defined(mips) && !defined(__GNUC__)
	rm = swapRM( FP_RZ );
#endif

#ifdef __GNUC__
	rm = swapRM( FE_TOWARDZERO );
#endif

	scaled = NO;

	q = 0;

	if ( x < y )
		goto L;

	if ( xpty < 0x035 )
	{
		while ( x >= twop920.d )
		{
	
			/* scale y up and compute x = x - floor(x/k*y)*k*y
			*/
	
			y1 = twop52.d*y;


#ifdef _32BIT_MACHINE

			DBLHI2INT(y1, m);
			m &= DEXPMASK;
			m |= ((xptx - 25) << DMANTWIDTH);
			INT2DBLHI(m, y1);
#else
			DBL2LL(y1, m);
			m &= DEXPMASK;
			m |= ((xptx - 25) << DMANTWIDTH);
			LL2DBL(m, y1);
#endif
			nd = x/y1;
	
			yhi = y1;

#ifdef _32BIT_MACHINE

			DBLLO2INT(yhi, m);
			m >>= 27;
			m <<= 27;
			INT2DBLLO(m, yhi);
#else
			DBL2LL(yhi, m);
			m >>= 27;
			m <<= 27;
			LL2DBL(m, yhi);
#endif
			ylo = y1 - yhi;
		
			nd = n = nd;
		
			x = x - nd*yhi - nd*ylo;
	
#ifdef _32BIT_MACHINE

			DBLHI2INT(x, ix);
#else
			DBL2LL(x, ix);
#endif
			xptx = (ix >> DMANTWIDTH);
			xptx &= 0x7ff;
		}
	
		if ( x >= y )
		{
			/* scale both x and y up */
		
			x *= twop104.d;
			y *= twop104.d;
		
#ifdef _32BIT_MACHINE

			DBLHI2INT(x, ix);
			DBLHI2INT(y, iy);
#else
			DBL2LL(x, ix);
			DBL2LL(y, iy);
#endif
			xptx = (ix >> DMANTWIDTH);
			xptx &= 0x7ff;

			xpty = (iy >> DMANTWIDTH);
			xpty &= 0x7ff;
		
			scaled = YES;
		}
	}

	while ( xptx >= xpty + 26 )
	{
		/* scale y up and compute x = x - floor(x/k*y)*k*y
		*/

		y1 = y;

#ifdef _32BIT_MACHINE

		DBLHI2INT(y1, m);
		m &= DEXPMASK;
		m |= ((xptx - 25) << 20);
		INT2DBLHI(m, y1);
#else
		DBL2LL(y1, m);
		m &= DEXPMASK;
		m |= ((xptx - 25) << DMANTWIDTH);
		LL2DBL(m, y1);
#endif
		nd = x/y1;

		yhi = y1;

#ifdef _32BIT_MACHINE

		DBLLO2INT(yhi, m);
		m >>= 27;
		m <<= 27;
		INT2DBLLO(m, yhi);
#else
		DBL2LL(yhi, m);
		m >>= 27;
		m <<= 27;
		LL2DBL(m, yhi);
#endif
		ylo = y1 - yhi;

		nd = n = nd;

		x = x - nd*yhi - nd*ylo;

#ifdef _32BIT_MACHINE

		DBLHI2INT(x, ix);
#else
		DBL2LL(x, ix);
#endif
		xptx = (ix >> DMANTWIDTH);
		xptx &= 0x7ff;
	}

	if ( x >= y )
	{
		nd = x/y;

		yhi = y;

#ifdef _32BIT_MACHINE

		DBLLO2INT(yhi, m);
		m >>= 27;
		m <<= 27;
		INT2DBLLO(m, yhi);
#else
		DBL2LL(yhi, m);
		m >>= 27;
		m <<= 27;
		LL2DBL(m, yhi);
#endif
		ylo = y - yhi;

		nd = n = nd;
		q ^= n;

		x = x - nd*yhi - nd*ylo;
	}

L:
	if ( y > twopm1021.d )
	{
		if ( x < 0.5*y )
		{
			result = x;
		}
		else if ( (x > 0.5*y) || (q & 1) )
		{
			result = x - y;
		}
		else
		{
			result = x;
		}
	}
	else
	{
		if ( 2.0*x < y )
		{
			result = x;
		}
		else if ( (2.0*x > y) || (q & 1) )
		{
			result = x - y;
		}
		else
		{
			result = x;
		}
	}

	if ( scaled == YES )
		result *= twopm104.d;

	rm = swapRM( rm );

	if ( signx != 0 )
		result = -result;

	return ( result );
}

#ifdef __GNUC__

double
__remainder( double x, double y )
{
	return ( __drem(x, y) );
}

#endif

#ifdef NO_LONG_DOUBLE

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
long double dreml( long double x, long double y ) {
  return __dreml(x, y);
}
#pragma weak dreml
long double remainderl( long double x, long double y ) {
  return __remainderl(x, y);
}
#pragma weak remainderl
#elif defined(__GNUC__)
extern  long double  __dreml(long double, long double);

long double    dreml() __attribute__ ((weak, alias ("__dreml")));

extern  long double  __remainderl(long double, long double);

long double    remainderl() __attribute__ ((weak, alias ("__remainderl")));

#endif

long double
__dreml( long double x, long double y )
{
	return ( (long double)__drem((double)x, (double)y) );
}

long double
__remainderl( long double x, long double y )
{
	return ( (long double)__remainder((double)x, (double)y) );
}

#endif

