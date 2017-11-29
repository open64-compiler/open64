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
 * Module: hypot.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:21-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.hypot.c $
 *
 * Revision history:
 *  09-Jun-93 - Original Version
 *
 * Description:	source code for hypot function
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.hypot.c $ $Revision: 1.5 $";

#ifdef _CALL_MATHERR
#include <stdio.h>
#include <math.h>
#include <errno.h>
#endif

#include "libm.h"

#if defined(mips) && !defined(__GNUC__)
extern	double	hypot(double, double);

#pragma weak hypot = __hypot
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern double __hypot(double, double);
#pragma weak hypot
double hypot( double arg1, double arg2) {
  return __hypot( arg1, arg2 );
}
#elif defined(__GNUC__)
extern  double  __hypot(double, double);

double    hypot() __attribute__ ((weak, alias ("__hypot")));

#endif

static const	du	Qnan =
{D(QNANHI, QNANLO)};

static const	du	Inf =
{D(0x7ff00000, 0x00000000)};

static	const	du	halfmaxdbl =
{D(0x7fdfffff, 0xffffffff)};

static	const	du	twop86 =
{D(0x45500000, 0x00000000)};

static	const	du	twopm86 =
{D(0x3a900000, 0x00000000)};

static	const	du	half =
{D(0x3fe00000, 0x00000000)};

static	const	du	one =
{D(0x3ff00000, 0x00000000)};

static	const	du	two =
{D(0x40000000, 0x00000000)};

static	const	du	sqrt2 =
{D(0x3ff6a09e, 0x667f3bcd)};

static	const	du	r2p1hi =
{D(0x4003504f, 0x333f9de6)};

static	const	du	r2p1lo =
{D(0x3ca21165, 0xf626cdd5)};


/* ====================================================================
 *
 * FunctionName		hypot
 *
 * Description		computes hypot function of args
 *
 * ====================================================================
 */

double
__hypot( arg1, arg2 )
double	arg1, arg2;
{
#ifdef _32BIT_MACHINE

int	ix, iy;
int	xptx, xpty;
int	itmp;

#else

long long ix, iy;
long long xptx, xpty;
long long itmp;

#endif

double	x, y;
double	tmp, q;
double	d, p, s, w;
double	result;
#ifdef _CALL_MATHERR
struct exception	exstruct;
#endif

	/* By definition, hypot(x, y) = sqrt(x*x + y*y), however this
	   form is not used because of round-off error; instead, we
	   use one of two formulae depending on the relative sizes of
	   x and y.
	*/

	/* extract exponents of x and y for some quick screening */

#ifdef _32BIT_MACHINE

	DBLHI2INT(arg1, ix);
	DBLHI2INT(arg2, iy);
#else
	DBL2LL(arg1, ix);
	DBL2LL(arg2, iy);
#endif

	xptx = (ix >> DMANTWIDTH);
	xptx &= 0x7ff;

	xpty = (iy >> DMANTWIDTH);
	xpty &= 0x7ff;

	if ( (xptx < 0x7ff) && (xpty < 0x7ff) )
	{
		x = fabs(arg1);
		y = fabs(arg2);

		/* if x < y, interchange x and y */

		if ( x < y )
		{
			tmp = x;
			x = y;
			y = tmp;

			itmp = xptx;
			xptx = xpty;
			xpty = itmp;
		}

		if ( xptx >= xpty + 31 )
			return ( x );

		if ( y == 0.0 )
			return ( x );

		if ( x > halfmaxdbl.d )
		{
			/* avoid overflow by scaling x and y down */

			x *= half.d;
			y *= half.d;

			if ( two.d*y < x )
			{
				q = x/y;

				result = x + y/(sqrt(one.d + q*q) + q);
			}
			else
			{
				d = x - y;
				q = d/y;
				p = q*(two.d + q);
				s = sqrt(two.d + p);
				w = p/(s + sqrt2.d);
				w = q + w + r2p1lo.d + r2p1hi.d;
				result = x + y/w;
			}


			/* now test for overflow in the result, returning
			   +infinity if overflow has occured
			*/

			if ( result > halfmaxdbl.d )
			{
#ifdef _CALL_MATHERR

				exstruct.type = OVERFLOW;
				exstruct.name = "hypot";
				exstruct.arg1 = arg1;
				exstruct.arg2 = arg2;
				exstruct.retval = Inf.d;

				if ( matherr( &exstruct ) == 0 )
				{
					fprintf(stderr, "overflow range error in hypot\n");
					SETERRNO(ERANGE);
				}

				return ( exstruct.retval );
#else
				SETERRNO(ERANGE);

				return ( Inf.d );
#endif
			}
			else
			{
				return ( two.d*result );
			}
		}
		else if ( xpty < 33 )
		{
			/* avoid underflow by scaling x and y up */

			x *= twop86.d;
			y *= twop86.d;

			if ( two.d*y < x )
			{
				q = x/y;

				result = x + y/(sqrt(one.d + q*q) + q);
			}
			else
			{
				d = x - y;
				q = d/y;
				p = q*(two.d + q);
				s = sqrt(two.d + p);
				w = p/(s + sqrt2.d);
				w = q + w + r2p1lo.d + r2p1hi.d;
				result = x + y/w;
			}

			result *= twopm86.d;

			return ( result );
		}
		else
		{
			if ( two.d*y < x )
			{
				q = x/y;

				result = x + y/(sqrt(one.d + q*q) + q);
			}
			else
			{

				d = x - y;
				q = d/y;
				p = q*(two.d + q);
				s = sqrt(two.d + p);
				w = p/(s + sqrt2.d);
				w = q + w + r2p1lo.d + r2p1hi.d;
				result = x + y/w;
			}

			return ( result );
		}
	}

	/* filter out infinities */

	if ( (fabs(arg1) == Inf.d) || (fabs(arg2) == Inf.d) )
	{
		/* arg1 or arg2 is infinite */

		return ( Inf.d );
	}

	/* arg1 or arg2 is a NaN; return a quiet NaN */

#ifdef _CALL_MATHERR

	exstruct.type = DOMAIN;
	exstruct.name = "hypot";
	exstruct.arg1 = arg1;
	exstruct.arg2 = arg2;
	exstruct.retval = Qnan.d;

	if ( matherr( &exstruct ) == 0 )
	{
		fprintf(stderr, "domain error in hypot\n");
		SETERRNO(EDOM);
	}

	return ( exstruct.retval );
#else
	NAN_SETERRNO(EDOM);

	return ( Qnan.d );
#endif
}

#ifdef NO_LONG_DOUBLE

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern  long double  __hypotl(long double, long double);
long double hypotl( long double arg1, long double arg2) {
  return ( (long double)__hypot((double)arg1, (double)arg2) );
}
#elif defined(__GNUC__)
extern  long double  __hypotl(long double, long double);
long double    hypotl() __attribute__ ((weak, alias ("__hypotl")));

#endif

long double
__hypotl( arg1, arg2 )
long double	arg1, arg2;
{
	return ( (long double)__hypot((double)arg1, (double)arg2) );
}

#endif

