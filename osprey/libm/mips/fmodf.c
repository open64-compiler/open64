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
 * Module: fmodf.c
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/libm/mips/fmodf.c,v $
 *
 * Revision history:
 *  09-Jun-93 - Original Version
 *
 * Description:	source code for fmodf function
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /proj/osprey/CVS/open64/osprey1.0/libm/mips/fmodf.c,v $ $Revision: 1.1.1.1 $";

#ifdef _CALL_MATHERR
#include <stdio.h>
#include <math.h>
#include <errno.h>
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
extern	float	fmodf(float, float);

#pragma weak fmodf = __fmodf
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern float __fmodf(float, float);
#pragma weak fmodf
float fmodf( float x, float y ) {
  return __fmodf( x, y );
}
#elif defined(__GNUC__)
extern  float  __fmodf(float, float);
float   fmodf(float, float) __attribute__ ((weak, alias ("__fmodf")));
#endif

static const	fu	Qnan = {QNANF};


/* ====================================================================
 *
 * FunctionName		fmodf
 *
 * Description		computes fmodf function of args
 *
 * ====================================================================
 */

float
__fmodf( float x, float y )
{
#ifdef _32BIT_MACHINE

int	ix, iy, xptx, xpty, m;

#else

long long ix, iy, xptx, xpty, m;

#endif

#if defined(mips) && !defined(__GNUC__)
fp_rnd	rm;
#endif

#ifdef __GNUC__
int	rm;
#endif

double	dx, dy;
double	nd;
double	dy1;
#ifdef _CALL_MATHERR
struct exception	exstruct;
#endif

	/* filter out Nans */

	if ( (x != x) || (y != y) )
	{
		/* x or y is a NaN; return a quiet NaN */

#ifdef _CALL_MATHERR

                exstruct.type = DOMAIN;
                exstruct.name = "fmodf";
                exstruct.arg1 = x;
                exstruct.arg2 = y;
                exstruct.retval = Qnan.f;

                if ( matherr( &exstruct ) == 0 )
                {
                        fprintf(stderr, "domain error in fmodf\n");
                        SETERRNO(EDOM);
                }

                return ( exstruct.retval );
#else
		NAN_SETERRNO(EDOM);

		return ( Qnan.f );
#endif
	}

	dx = x;
	dy = y;

	/* extract exponent of dx for some quick screening */

#ifdef _32BIT_MACHINE

	DBLHI2INT(dx, ix);	/* copy MSW of dx to ix */
	DBLHI2INT(dy, iy);	/* copy MSW of dy to iy */
#else
	DBL2LL(dx, ix);		/* copy dx to ix */
	DBL2LL(dy, iy);		/* copy dy to iy */
#endif
	xptx = (ix >> DMANTWIDTH);
	xptx &= 0x7ff;

	xpty = (iy >> DMANTWIDTH);
	xpty &= 0x7ff;

	if ( (xptx == 0x7ff) || ((xpty == 0) && (y == 0.0f)) )
	{
		/* x == +/- infinity or y == +/- 0.0; return a quiet NaN */

#ifdef _CALL_MATHERR

		exstruct.type = DOMAIN;
		exstruct.name = "fmodf";
		exstruct.arg1 = x;
		exstruct.arg2 = y;
		exstruct.retval = Qnan.f;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "domain error in fmodf\n");
			SETERRNO(EDOM);
		}

		return ( exstruct.retval );
#else
		SETERRNO(EDOM);

		return ( Qnan.f );
#endif
	}

	dy = fabs(dy);

	if ( fabs(dx) < dy )
		return ( x );

	/* set rounding mode to round to zero */
	
#if defined(mips) && !defined(__GNUC__)
	rm = swapRM( FP_RZ );
#endif

#ifdef __GNUC__
	rm = swapRM( FE_TOWARDZERO );
#endif


	if ( xptx < xpty + 24 )
	{
		/* compute dx = dx - floor(dx/dy)*dy
		*/

		nd = dx/dy;

		nd = (int)nd;

		dx = dx - nd*dy;

		rm = swapRM( rm );

		goto last;
	}
	else do
	{
		/* scale dy up and compute dx = dx - floor(dx/(k*dy))*k*dy
		*/

		dy1 = dy;

#ifdef _32BIT_MACHINE

		DBLHI2INT(dy1, m);
	
#else
		DBL2LL(dy1, m);
#endif
		m &= DEXPMASK;	/* clear exponent of dy1	*/
		m |= ((xptx - 23) << DMANTWIDTH);

		/* set exponent of dy1 to xptx - 23	*/

#ifdef _32BIT_MACHINE

		INT2DBLHI(m, dy1);
#else
		LL2DBL(m, dy1);
#endif
		nd = dx/dy1;

		nd = (int)nd;

		dx = dx - nd*dy1;

#ifdef _32BIT_MACHINE

		DBLHI2INT(dx, ix);	/* copy MSW of dx to ix */
#else
		DBL2LL(dx, ix);		/* copy dx to ix */
#endif
		xptx = (ix >> DMANTWIDTH);
		xptx &= 0x7ff;

	} 
	while ( xptx >= xpty + 24 );

	if ( fabs(dx) < dy )
	{
		rm = swapRM( rm );

		goto last;
	}

	/* compute dx = dx - floor(dx/dy)*dy
	*/

	nd = dx/dy;

	nd = (int)nd;

	dx = dx - nd*dy;

	rm = swapRM( rm );

last:	if ( (x < 0.0f) && (dx == 0.0) )
		dx = -0.0;

	return ( (float)dx );
}

