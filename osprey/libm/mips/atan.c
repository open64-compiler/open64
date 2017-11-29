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
 * Module: atan.c
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/libm/mips/atan.c,v $
 *
 * Revision history:
 *  09-Jun-93 - Original Version
 *
 * Description:	source code for atan function
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /proj/osprey/CVS/open64/osprey1.0/libm/mips/atan.c,v $ $Revision: 1.1.1.1 $";

#ifdef _CALL_MATHERR
#include <stdio.h>
#include <math.h>
#include <errno.h>
#endif

#include "libm.h"

#if defined(mips) && !defined(__GNUC__)
extern	double	atan(double);

#pragma weak atan = __atan
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern double __atan(double);
#pragma weak atan
double atan( double x ) {
  return __atan( x );
}
#elif defined(__GNUC__)
extern  double  __atan(double);

double    atan() __attribute__ ((weak, alias ("__atan")));

#endif

static const	du	Qnan =
{D(QNANHI, QNANLO)};

static const du x0 = {D(0x4005a045, 0x6bb5777e)};
static const du atanx0 = {D(0x3ff376b7, 0x14052e5f)};

static const du x1 = {D(0x401569f1, 0xdc77c11f)};
static const du atanx1 = {D(0x3ff62d96, 0x1fdc0458)};

static const du x2 = {D(0x40255a9e, 0xc859d828)};
static const du atanx2 = {D(0x3ff7a378, 0x109217cc)};

/* coefficients for rational approximation of atan on [-0.5, 0.5] */

static const du	P1[] =
{
{D(0xbfd55555, 0x5555554a)},
{D(0xbfe19716, 0xdbd2dae9)},
{D(0xbfd11086, 0xb3197c81)},
{D(0xbfa269e6, 0x2efde2b2)},
{D(0xbf24f1d1, 0x3d7b6dc8)},
};

static const du	Q1[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0x4001fe1d, 0xf1aaee0b)},
{D(0x3ffb884e, 0x717967d7)},
{D(0x3fe0504a, 0x395eff9f)},
{D(0x3fa779a6, 0x7019ef92)},
};

/* coefficients for rational approximation of atan on [-1/3, 1/3] */

static const du	P2[] =
{
{D(0x00000000, 0x00000000)},
{D(0xc003463c, 0xd05a4ebc)},
{D(0xc006a82b, 0xc06143d7)},
{D(0xbfe73f9d, 0x6d046093)},
{D(0xbf7d6f3f, 0x1d90fdcf)},
};

static const du	Q2[] =
{
{D(0x401ce95b, 0x38877730)},
{D(0x4029aa88, 0xfad77a71)},
{D(0x401b207c, 0xe379aaab)},
{D(0x3ff00000, 0x00000000)},
};

/* coefficients for polynomial approximation of atan on [-.10978, .10978] */

static const du	P3[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0xbfd55555, 0x55555538)},
{D(0x3fc99999, 0x999827a8)},
{D(0xbfc24924, 0x8f3fab6c)},
{D(0x3fbc71c1, 0x3cf9a795)},
{D(0xbfb742f1, 0x6d401761)},
{D(0x3fb2ff51, 0xeeb75b4f)},
};

/* coefficients for polynomial approximation of atan on [-1/16, 1/16] */

static const du	P4[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0xbfd55555, 0x55555537)},
{D(0x3fc99999, 0x9996312c)},
{D(0xbfc24924, 0x8296d3ad)},
{D(0x3fbc7187, 0xfb7f8b73)},
{D(0xbfb70c21, 0xce9d3f8f)},
};

static const du	piby4 =
{D(0x3fe921fb, 0x54442d18)};

static const du	piby2 =
{D(0x3ff921fb, 0x54442d18)};

static const du	m_piby2 =
{D(0xbff921fb, 0x54442d18)};


/* ====================================================================
 *
 * FunctionName		atan
 *
 * Description		computes arctangent of arg
 *			uses the identity atan(x) = atan(x0) +
 *			atan( (x - x0)/(1 + x*x0) )
 *
 * ====================================================================
 */

double
__atan( double x )
{
#ifdef _32BIT_MACHINE

int	ix, xpt;

#else

long long ix, xpt;

#endif

double	xsq, num, denom, result;
double	z, zsq;
#ifdef _CALL_MATHERR
struct exception	exstruct;
#endif

	/* extract exponent of x for some quick screening */

#ifdef _32BIT_MACHINE

	DBLHI2INT(x, ix);	/* move MSW of x to an integer	*/
#else
	DBL2LL(x, ix);		/* move x to a long long	*/
#endif
	xpt = (ix >> DMANTWIDTH);
	xpt &= 0x7ff;

	if ( xpt < 0x400 )
	{
		if ( xpt < 0x3fe )
		{
			/* |x| < 0.5 */
	
			if ( xpt >= 0x3e3 )
			{
				/* |x| >= 2^-28 */
	
				xsq = x*x;
	
				num = (((P1[4].d*xsq + P1[3].d)*xsq +
					P1[2].d)*xsq + P1[1].d)*xsq + P1[0].d;
	
				denom = (((Q1[4].d*xsq + Q1[3].d)*xsq +
					 Q1[2].d)*xsq + Q1[1].d)*xsq + Q1[0].d;
	
				result = x + x*xsq*num/denom;
	
				return ( result );
			}
	
			return (x);
		}
		else
		{
			/* .5 <= |x| < 2.0 */
	
			z = (fabs(x) - 1.0)/(1.0 + fabs(x));
	
			zsq = z*z;
			
			num = (((P2[4].d*zsq + P2[3].d)*zsq + P2[2].d)*zsq + P2[1].d);
	
			denom = ((Q2[3].d*zsq + Q2[2].d)*zsq + Q2[1].d)*zsq + Q2[0].d;
	
			result = z + ((z*zsq)*num)/denom;
	
			result = piby4.d + result;
	
			if ( x < 0.0 )
				result = -result;
	
			return ( result );
		}
	}
	else if ( xpt < 0x403 )
	{
		if ( xpt < 0x401 )
		{
			/* 2.0 <= |x| < 4.0 */
	
			z = (fabs(x) - x0.d)/(1.0 + fabs(x)*x0.d);
			zsq = z*z;
	
			result = (((((P3[6].d*zsq + P3[5].d)*zsq + P3[4].d)*zsq + 
				P3[3].d)*zsq + P3[2].d)*zsq + P3[1].d)*zsq;
	
			result = z*result + z + atanx0.d;
	
			if ( x < 0.0 )
				result = -result;
	
			return ( result );
		}
		else if ( xpt < 0x402 )
		{
			/* 4.0 <= |x| < 8.0 */
	
			z = (fabs(x) - x1.d)/(1.0 + fabs(x)*x1.d);
			zsq = z*z;
	
			result = ((((P4[5].d*zsq + P4[4].d)*zsq + 
				P4[3].d)*zsq + P4[2].d)*zsq + P4[1].d)*zsq;
	
			result = z*result + z + atanx1.d;
			
			if ( x < 0.0 )
				result = -result;
	
			return ( result );
		}
		else
		{
			/* 8.0 <= |x| < 16.0 */
	
			z = (fabs(x) - x2.d)/(1.0 + fabs(x)*x2.d);
			zsq = z*z;
	
			result = ((((P4[5].d*zsq + P4[4].d)*zsq + 
				P4[3].d)*zsq + P4[2].d)*zsq + P4[1].d)*zsq;
	
			result = z*result + z + atanx2.d;
			
			if ( x < 0.0 )
				result = -result;
	
			return ( result );
		}
	}
	else if ( xpt < 0x435 )
	{
		/* 16.0 <= |x| < 2^54 */

		/* atan(1/x) = pi/2 - atan(x) */

		z = -1.0/fabs(x);
		zsq = z*z;

		result = ((((P4[5].d*zsq + P4[4].d)*zsq + P4[3].d)*zsq + 
			P4[2].d)*zsq + P4[1].d)*zsq;

		result = result*z + z + piby4.d + piby4.d;

		if ( x < 0.0 )
			result = -result;

		return ( result );
	}

	if ( x != x )
	{
		/* x is a NaN; return a quiet NaN */

#ifdef _CALL_MATHERR

		exstruct.type = DOMAIN;
		exstruct.name = "atan";
		exstruct.arg1 = x;
		exstruct.retval = Qnan.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "domain error in atan\n");
			SETERRNO(EDOM);
		}

		return ( exstruct.retval );
#else
		NAN_SETERRNO(EDOM);

		return ( Qnan.d );
#endif
	}

	if ( ix > 0 )
		return ( piby2.d );

	return ( m_piby2.d );
}

#ifdef NO_LONG_DOUBLE

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
#pragma weak atanl
long double atanl( long double x ) {	
	return ( (long double)__atan((double)x) );
}
#elif defined(__GNUC__)
extern  long double  __atanl(long double);

long double    atanl() __attribute__ ((weak, alias ("__atanl")));

#endif

long double
__atanl( long double x )
{	
	return ( (long double)__atan((double)x) );
}

#endif

