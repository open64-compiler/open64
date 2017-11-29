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
 * Module: fmod.c
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/libm/fmod.c,v $
 *
 * Revision history:
 *  09-Jun-93 - Original Version
 *
 * Description:	source code for fmod function
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /proj/osprey/CVS/open64/osprey1.0/libm/fmod.c,v $ $Revision: 1.1.1.1 $";

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
extern	double	fmod(double, double);

#pragma weak fmod = __fmod
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern double __fmod( double arg1, double arg2 );
#pragma weak fmod
double fmod( double arg1, double arg2 ) {
  return __fmod(arg1, arg2);
}
#elif defined(__GNUC__)
extern  double  __fmod(double, double);
double    fmod() __attribute__ ((weak, alias ("__fmod")));

#endif

static const	du	Qnan =
{D(QNANHI, QNANLO)};

static const	du 	twopm104 =
{D(0x39700000, 0x00000000)};

static const	du 	twop52 =
{D(0x43300000, 0x00000000)};

static const	du 	twop104 =
{D(0x46700000, 0x00000000)};

static const	du 	twop920 =
{D(0x79700000, 0x00000000)};

#define	NO	0
#define	YES	1


/* ====================================================================
 *
 * FunctionName		fmod
 *
 * Description		computes fmod function of args
 *
 * ====================================================================
 */

double
__fmod( arg1, arg2 )
double	arg1, arg2;
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

double	x, y;
int	scaled;
double	nd, yhi, ylo;
double	y1;
#ifdef _CALL_MATHERR
struct exception	exstruct;
#endif

	x = arg1;
	y = arg2;

	/* extract exponents of y and x for some quick screening */

#ifdef _32BIT_MACHINE

	DBLHI2INT(x, ix);	/* copy MSW's of x and y to integers	*/
	DBLHI2INT(y, iy);
#else
	DBL2LL(x, ix);		/* copy x and y to long longs	*/
	DBL2LL(y, iy);
#endif
	xpty = (iy >> DMANTWIDTH);
	xpty &= 0x7ff;

	xptx = (ix >> DMANTWIDTH);
	xptx &= 0x7ff;

	/* filter out Nans */

	if ( (xpty == 0x7ff) || (xptx == 0x7ff) )
	{
		if ( (y != y) || (x != x) )
		{
			/* y or x is a NaN; return a quiet NaN */

#ifdef _CALL_MATHERR

			exstruct.type = DOMAIN;
			exstruct.name = "fmod";
			exstruct.arg1 = x;
			exstruct.arg2 = y;
			exstruct.retval = Qnan.d;

			if ( matherr( &exstruct ) == 0 )
			{
				fprintf(stderr, "domain error in fmod\n");
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
		exstruct.name = "fmod";
		exstruct.arg1 = x;
		exstruct.arg2 = y;
		exstruct.retval = Qnan.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "domain error in fmod\n");
			SETERRNO(EDOM);
		}

		return ( exstruct.retval );
#else
		SETERRNO(EDOM);

		return ( Qnan.d );
#endif
	}

	y = fabs(y);

	if ( fabs(x) < y )
		return ( x );

	/* set rounding mode to round to zero */
	
#if defined(mips) && !defined(__GNUC__)
	rm = swapRM( FP_RZ );
#endif

#ifdef __GNUC__
	rm = swapRM( FE_TOWARDZERO );
#endif


	scaled = NO;

	if ( xpty >= 0x035 )
	{
L:
		if ( xptx < xpty + 26 )
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

			nd = (int)nd;

			x = x - nd*yhi - nd*ylo;

			rm = swapRM( rm );

			if ( scaled == YES )
				x *= twopm104.d;

			goto last;
		}
		else
		{
			do
			{
				/* scale y up and compute x = x - floor(x/k*y)*k*y
				*/
		
				y1 = y;

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
	
				nd = (int)nd;
	
				x = x - nd*yhi - nd*ylo;

#ifdef _32BIT_MACHINE

				DBLHI2INT(x, ix); /* copy MSW of x to an int */
#else
				DBL2LL(x, ix); /* copy x to a long long */
#endif
				xptx = (ix >> DMANTWIDTH);
				xptx &= 0x7ff;
			}
			while ( xptx >= xpty + 26 );

			if ( fabs(x) < y )
			{
				rm = swapRM( rm );

				if ( scaled == YES )
					x *= twopm104.d;

				goto last;
			}

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

			nd = (int)nd;

			x = x - nd*yhi - nd*ylo;

			rm = swapRM( rm );

			if ( scaled == YES )
				x *= twopm104.d;

			goto last;
		}
	}

	while ( fabs(x) >= twop920.d )
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
	
		nd = (int)nd;
	
		x = x - nd*yhi - nd*ylo;

#ifdef _32BIT_MACHINE

		DBLHI2INT(x, ix);	/* copy MSW of x to an int	*/
#else
		DBL2LL(x, ix);		/* copy x to a long long */
#endif
		xptx = (ix >> DMANTWIDTH);
		xptx &= 0x7ff;
	}

	if ( fabs(x) < y )
	{
		rm = swapRM( rm );

		goto last;
	}

	/* scale both x and y up */

	x *= twop104.d;
	y *= twop104.d;

#ifdef _32BIT_MACHINE

	DBLHI2INT(x, ix);	/* copy MSW's of x and y to integers	*/
	DBLHI2INT(y, iy);
#else
	DBL2LL(x, ix);		/* copy x and y to long longs	*/
	DBL2LL(y, iy);
#endif
	xptx = (ix >> DMANTWIDTH);
	xptx &= 0x7ff;

	xpty = (iy >> DMANTWIDTH);
	xpty &= 0x7ff;

	scaled = YES;
	goto L;

last:	if ( (arg1 < 0.0) && (x == 0.0) )
		x = -0.0;

	return ( x );
}

#ifdef NO_LONG_DOUBLE

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
long double fmodl(long double arg1, long double arg2) {
  return __fmodl(arg1, arg2);
}
#pragma weak fmodl
#elif defined(__GNUC__)
extern  long double  __fmodl(long double, long double);

long double    fmodl() __attribute__ ((weak, alias ("__fmodl")));

#endif

long double
__fmodl( arg1, arg2 )
long double	arg1, arg2;
{
	return ( (long double)__fmod((double)arg1, (double)arg2) );
}

#endif

