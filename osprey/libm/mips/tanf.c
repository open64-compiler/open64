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
 * Module: tanf.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:23-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.tanf.c $
 *
 * Revision history:
 *  09-Jun-93 - Original Version
 *
 * Description:	source code for tanf function
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.tanf.c $ $Revision: 1.5 $";

#ifdef _CALL_MATHERR
#include <stdio.h>
#include <math.h>
#include <errno.h>
#endif

#include "libm.h"

#if defined(mips) && !defined(__GNUC__)
extern	float	ftan(float);
extern	float	tanf(float);

#pragma weak ftan = __tanf
#pragma weak tanf = __tanf
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern float __tanf(float);
#pragma weak tanf
float tanf( float x ) {
  return __tanf( x );
}
#elif defined(__GNUC__)
extern  float  __tanf(float);
float    tanf(float) __attribute__ ((weak, alias ("__tanf")));
#endif

/* coefficients for polynomial approximation to tan on +/- pi/4 */

static const du	P[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0x3fd5554d, 0xee0d8ef8)},
{D(0x3fc112db, 0xaacfc056)},
{D(0x3fab58b6, 0xc20538bd)},
{D(0x3f990447, 0xf756b2dd)},
{D(0x3f698e16, 0x80b8e679)},
{D(0x3f8338c7, 0xa092a7f3)},
};

/* coefficients for polynomial approximation to cot(z) - 1/z  on +/- pi/4 */

static const du	P2[] =
{
{D(0xbfd55555, 0x55555555)},
{D(0xbf96c1b2, 0x603d1494)},
{D(0xbf614116, 0xf382ec7a)},
{D(0xbf2f5d6f, 0x1e20e337)},
};

static const du	rpiby2 =
{D(0x3fe45f30, 0x6dc9c883)};

static const du	m_one =
{D(0xbff00000, 0x00000000)};

static const du	piby2hi =
{D(0x3ff921fb, 0x50000000)};

static const du	piby2lo =
{D(0x3e5110b4, 0x611a6263)};

static const	fu	Qnan = {QNANF};

static const fu Inf = {0x7f800000};


/* ====================================================================
 *
 * FunctionName		tanf
 *
 * Description		computes tangent of arg
 *
 * ====================================================================
 */

float
__tanf( float x )
{
int	ix, xpt;
int	n;
double	dx, xsq, poly;
double	dn;
double	result;
#ifdef _CALL_MATHERR
struct exception	exstruct;
#endif

	FLT2INT(x, ix);	/* copy arg to an integer	*/
	xpt = (ix >> (MANTWIDTH-1));
	xpt &= 0x1ff;

	/* xpt is exponent(x) + 1 bit of mantissa */

	if ( xpt < 0xfd )	
	{
		/* |x| < .75 */

		/* compute tan(x) using a polynomial approximation */

		dx = x;

		if ( xpt >= 0xe6 )
		{
			/* |x| >= 2^(-12) */

			xsq = dx*dx;

			poly = ((((P[6].d*xsq + P[5].d)*xsq + P[4].d)*xsq +
				      + P[3].d)*xsq + P[2].d)*xsq + P[1].d;

			result = dx + (dx*xsq)*poly;

			return ( (float)result );
		}

		return ( x );
	}

	if ( xpt < 0x136 )
	{
		/* |x| < 2^28 */

		/* reduce x to +/- pi/4 */

		dx = x;

		dn = dx*rpiby2.d;

		n = ROUND(dn);
		dn = n;

		dx = dx - dn*piby2hi.d;
		dx = dx - dn*piby2lo.d;	/* dx = x - n*pi/2 */

		if ( (n & 1) == 0 )
		{
			/* compute tan(dx) using a polynomial approximation */

			xsq = dx*dx;
	
			poly = ((((P[6].d*xsq + P[5].d)*xsq + P[4].d)*xsq +
				      + P[3].d)*xsq + P[2].d)*xsq + P[1].d;
	
			result = dx + (dx*xsq)*poly;
	
			return ( (float)result );
		}


		/* compute -cot(dx) using a Laurent expansion */

		xsq = dx*dx;

		result = m_one.d/dx;

		poly = (((P2[3].d*xsq + P2[2].d)*xsq + P2[1].d)*xsq +
				P2[0].d)*dx;

		result -= poly;

		return ( (float)result );
	}

	if ( (x != x) || (fabsf(x) == Inf.f) )
	{
		/* x is a NaN or +/-inf; return a quiet NaN */

#ifdef _CALL_MATHERR

                exstruct.type = DOMAIN;
                exstruct.name = "tanf";
                exstruct.arg1 = x;
                exstruct.retval = Qnan.f;

                if ( matherr( &exstruct ) == 0 )
                {
                        fprintf(stderr, "domain error in tanf\n");
                        SETERRNO(EDOM);
                }

                return ( exstruct.retval );
#else
		NAN_SETERRNO(EDOM);
		
		return ( Qnan.f );
#endif
	}

	/* just give up and return 0.0, setting errno = ERANGE */

#ifdef _CALL_MATHERR

		exstruct.type = TLOSS;
		exstruct.name = "tanf";
		exstruct.arg1 = x;
		exstruct.retval = 0.0;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "range error in tanf (total loss \
of significance)\n");
			SETERRNO(ERANGE);
		}

		return ( exstruct.retval );
#else
		SETERRNO(ERANGE);

		return ( 0.0f );
#endif
}

