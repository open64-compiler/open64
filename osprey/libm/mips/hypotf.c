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
 * Module: hypotf.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:22-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.hypotf.c $
 *
 * Revision history:
 *  09-Jun-93 - Original Version
 *
 * Description:	source code for hypotf function
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.hypotf.c $ $Revision: 1.5 $";

#ifdef _CALL_MATHERR
#include <stdio.h>
#include <math.h>
#include <errno.h>
#endif

#include "libm.h"

#if defined(mips) && !defined(__GNUC__)
extern	float	fhypot(float, float);
extern	float	hypotf(float, float);

#pragma weak fhypot = __hypotf
#pragma weak hypotf = __hypotf
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern float __hypotf(float, float);
#pragma weak hyptof
float hypotf( float x, float y ) {
  return __hypotf( x, y );
}
#elif defined(__GNUC__)
extern  float  __hypotf(float, float);
float    hypotf(float, float) __attribute__ ((weak, alias ("__hypotf")));
#endif

static	const du maxfltp = {D(0x47efffff, 0xf0000000)};

static const	fu	Qnan = {QNANF};

static const fu Inf = {0x7f800000};

#ifndef	_HDW_SQRT

#define	MAGIC	0x5fe6eb3b

#endif


/* ====================================================================
 *
 * FunctionName		hypotf
 *
 * Description		computes hypotf function of args
 *
 * ====================================================================
 */

float
__hypotf( float x, float y )
{
int	n;
int	ix, iy;
int	xptx, xpty;
double	dx, dy;
double	z, zsq, zsqby2;
double	q, Q;
#ifdef _CALL_MATHERR
struct exception	exstruct;
#endif

	/* by definition, hypotf(x, y) = sqrt(x^2 + y^2) */

	/* extract exponents of x and y for some quick screening */

	FLT2INT(x, ix);		/* copy first arg to an integer	*/
	xptx = (ix >> MANTWIDTH);
	xptx &= 0xff;

	FLT2INT(y, iy);		/* copy second arg to an integer */
	xpty = (iy >> MANTWIDTH);
	xpty &= 0xff;

	if ( (xptx < 0xff) && (xpty < 0xff) )
	{
		/* x and y are both finite	*/

		if ( xptx > xpty + 12 )
			return ( fabsf(x) );

		if ( xpty > xptx + 12 )
			return ( fabsf(y) );

		if ( y == 0.0f )
			return ( fabsf(x) );

		if ( x == 0.0f )
			return ( fabsf(y) );

		dx = x;
		dy = y;

		zsq = dx*dx + dy*dy;

#ifdef	_HDW_SQRT

		z = sqrt(zsq);
#else

		/* Compute z = sqrt(zsq) using Newton-Raphson method.
		*/

		/* First, approximate 1/sqrt(zsq) using a "magic"
		   constant.
		*/

		z = 0.0;
		DBLHI2INT(zsq, n);
		n >>= 1;
		n = MAGIC - n;
		INT2DBLHI(n, z);
		zsqby2 = 0.5*zsq;
	
		/* Improve estimate by iterating twice. */

		z = z*(1.5 - zsqby2*z*z);
		z = z*(1.5 - zsqby2*z*z);

		/* Do one final iteration, multiplying by zsq. */

		q = zsqby2*z;
		Q = q + q;
		z = Q*(1.5 - q*z);
#endif

		/* now test for overflow in the result, returning
		   +infinity if overflow has occured
		*/

		if ( z >= maxfltp.d )
		{
#ifdef _CALL_MATHERR
			exstruct.type = OVERFLOW;
			exstruct.name = "hypotf";
			exstruct.arg1 = x;
			exstruct.arg2 = y;
			exstruct.retval = Inf.f;

			if ( matherr( &exstruct ) == 0 )
			{
				fprintf(stderr, "overflow range error in hypotf\n");
				SETERRNO(ERANGE);
			}

			return ( exstruct.retval );
#else
			SETERRNO(ERANGE);

			return ( Inf.f );
#endif
		}
		else
		{
			return ( (float)z );
		}
	}

	/* filter out infinities */

	if ( (fabsf(x) == Inf.f) || (fabsf(y) == Inf.f) )
	{
		return ( Inf.f );
	}

	/* x or y is a NaN; return a quiet NaN */

#ifdef _CALL_MATHERR

	exstruct.type = DOMAIN;
	exstruct.name = "hypotf";
	exstruct.arg1 = x;
	exstruct.arg2 = y;
	exstruct.retval = Qnan.f;

	if ( matherr( &exstruct ) == 0 )
	{
		fprintf(stderr, "domain error in hypotf\n");
		SETERRNO(EDOM);
	}

	return ( exstruct.retval );
#else
	NAN_SETERRNO(EDOM);

	return ( Qnan.f );
#endif
}

