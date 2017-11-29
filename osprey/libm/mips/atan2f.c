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
 * Module: atan2f.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:21-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.atan2f.c $
 *
 * Revision history:
 *  09-Jun-93 - Original Version
 *
 * Description:	source code for atan2f function
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.atan2f.c $ $Revision: 1.5 $";

#ifdef _CALL_MATHERR
#include <stdio.h>
#include <math.h>
#include <errno.h>
#endif

#include "libm.h"

#if defined(mips) && !defined(__GNUC__)
extern	float	fatan2(float, float);
extern	float	atan2f(float, float);

#pragma weak fatan2 = __atan2f
#pragma weak atan2f = __atan2f
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern float __atan2f(float, float);
#pragma weak atan2f
float atan2f( float y, float x ) {
  return __atan2f( y, x );
}
#elif defined(__GNUC__)
extern  float  __atan2f(float, float);
float    atan2f(float, float) __attribute__ ((weak, alias ("__atan2f")));
#endif

extern	const fu	_atan2fres0[2][2];
extern	const fu	_atan2fres1[2][2];
extern	const fu	_atan2fres2[2][2];
extern	const fu	_atan2fres4[4][4];

static	const du	twopm30 =
{D(0x3e100000, 0x00000000)};

static	const du	twopm150 =
{D(0x36900000, 0x00000000)};

static	const du	limit1 =
{D(0x3fc445f0, 0xfbb1cf92)};	/* tan(pi/20) */

static	const du	limit2 =
{D(0x3fe04e08, 0x50c1dd5c)};	/* tan(3*pi/20) */

static	const du	rlimit4 =
{D(0x3fe04e08, 0x50c1dd5c)};	/* 1/tan(7*pi/20) */

static	const du	rlimit5 =
{D(0x3fc445f0, 0xfbb1cf92)};	/* 1/tan(9*pi/20) */

/* the angles below have been chosen very carefully to minimize the
   ulps error of the entries in tantbl
*/

static	const du	angletbl[] =
{
{D(0x00000000, 0x00000000)},
{D(0x80000000, 0x00000000)},
{D(0x400921fb, 0x54442d18)},
{D(0xc00921fb, 0x54442d18)},

{D(0x3fd41b2f, 0x769ddfb2)},	/* ~pi/10 */
{D(0xbfd41b2f, 0x769ddfb2)},	/* ~-pi/10 */
{D(0x40069e95, 0x65707122)},	/* ~pi - pi/10 */
{D(0xc0069e95, 0x65707122)},	/* ~-(pi - pi/10) */

{D(0x3fe41b2f, 0x769cf2b1)},	/* ~2*pi/10 */
{D(0xbfe41b2f, 0x769cf2b1)},
{D(0x40041b2f, 0x769cf06c)},
{D(0xc0041b2f, 0x769cf06c)},

{D(0x3fee28c7, 0x31ec183d)},	/* ~3*pi/10 */
{D(0xbfee28c7, 0x31ec183d)},
{D(0x400197c9, 0x87c92709)},
{D(0xc00197c9, 0x87c92709)},

{D(0x3ff41b2f, 0x769cfe1f)},	/* ~4*pi/10 */
{D(0xbff41b2f, 0x769cfe1f)},
{D(0x3ffe28c7, 0x31eb5c12)},
{D(0xbffe28c7, 0x31eb5c12)},

{D(0x3ff921fb, 0x54442d18)},	/* pi/2 */
{D(0xbff921fb, 0x54442d18)},
{D(0x3ff921fb, 0x54442d18)},
{D(0xbff921fb, 0x54442d18)},
};

/* tangents of the angles in angletbl */

static	const du	tantbl[] =
{
{D(0x00000000, 0x00000000)},
{D(0x80000000, 0x00000000)},
{D(0x80000000, 0x00000000)},
{D(0x00000000, 0x00000000)},

{D(0x3fd4cb7b, 0xfb4a69b7)},
{D(0xbfd4cb7b, 0xfb4a69b7)},
{D(0xbfd4cb7b, 0xfb4a69b7)},
{D(0x3fd4cb7b, 0xfb4a69b7)},

{D(0x3fe73fd6, 0x1d9df809)},
{D(0xbfe73fd6, 0x1d9df809)},
{D(0xbfe73fd6, 0x1d9df809)},
{D(0x3fe73fd6, 0x1d9df809)},

{D(0x3ff605a9, 0x0c74a8a0)},
{D(0xbff605a9, 0x0c74a8a0)},
{D(0xbff605a9, 0x0c74a8a0)},
{D(0x3ff605a9, 0x0c74a8a0)},

{D(0x40089f18, 0x8bdd1d09)},
{D(0xc0089f18, 0x8bdd1d09)},
{D(0xc0089f18, 0x8bdd1d09)},
{D(0x40089f18, 0x8bdd1d09)},
};

/* coefficients for a 7th degree polynomial approximation of atan
   on the interval +/- tan(pi/20)
*/

static	const du	P[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0xbfd55554, 0x19e773fa)},
{D(0x3fc99738, 0x97341225)},
{D(0xbfc1a142, 0x184deb25)},
};

static const	fu	Qnan = {QNANF};


/* ====================================================================
 *
 * FunctionName		atan2f
 *
 * Description		computes arctangent of arg1/arg2
 *
 * ====================================================================
 */

float
__atan2f( float y, float x )
{
double	dx, dy;
int	ix, iy;
int	xptx, xpty;
int	i, j, k;
double	dabsx, dabsy;
int	signx, signy;
double	tk, zk;
double	poly;
double	u, v, s, ss;
double	result;
#ifdef _CALL_MATHERR
struct exception	exstruct;
#endif

	FLT2INT(y, iy);		/* copy first arg to an integer	*/
	xpty = (iy >> MANTWIDTH);
	xpty &= 0xff;

	FLT2INT(x, ix);		/* copy second arg to an integer */
	xptx = (ix >> MANTWIDTH);
	xptx &= 0xff;

	signy = (iy >> 31);
	signy = (signy & 1);

	signx = (ix >> 31);
	signx = (signx & 1);

	/* filter out Nans */

	if ( (y != y) || (x != x) )
	{
		/* y or x is a NaN; return a quiet NaN */

#ifdef _CALL_MATHERR

                exstruct.type = DOMAIN;
                exstruct.name = "atan2f";
                exstruct.arg1 = y;
                exstruct.arg2 = x;
                exstruct.retval = Qnan.f;

                if ( matherr( &exstruct ) == 0 )
                {
                        fprintf(stderr, "domain error in atan2f\n");
                        SETERRNO(EDOM);
                }

                return ( exstruct.retval );
#else
		NAN_SETERRNO(EDOM);

		return ( Qnan.f );
#endif
	}

	/* filter out zero arguments */

	if ( x == 0.0f )
	{
		if ( y == 0.0f )
		{
#ifdef _CALL_MATHERR
			exstruct.type = DOMAIN;
			exstruct.name = "atan2f";
			exstruct.arg1 = y;
			exstruct.arg2 = x;
			exstruct.retval = _atan2fres0[signx][signy].f;

			if ( matherr( &exstruct ) == 0 )
			{
				fprintf(stderr, "domain error in atan2f\n");
				SETERRNO(EDOM);
			}

			return ( exstruct.retval );
#else
			SETERRNO(EDOM);

			return ( _atan2fres0[signx][signy].f );
#endif
		}

		return ( _atan2fres1[signx][signy].f );
	}
	else if ( y == 0.0f )
	{
		result = _atan2fres2[signx][signy].f;
		return ( result );
	}

	/* filter out infinities */

	i = (xptx == 0xff);

	j = (xpty == 0xff);

	if ( (i + j) == 0 )
	{
		dx = x;
		dy = y;

		dabsx = fabs(dx);

		dabsy = fabs(dy);

		if ( dabsy < dabsx )
		{
			j = (dabsy >= dabsx*limit1.d);
			k = (dabsy >= dabsx*limit2.d);
		}
		else
		{
			j = (dabsx <= dabsy*rlimit4.d);
			k = (dabsx <= dabsy*rlimit5.d);
			j = j + 3;
		}

		k = j + k;

		k = 4*k + 2*signx + signy;

		if ( k < 4 )
		{
			s = dy/dx;
		}
		else if ( k > 19 )
		{
			s = -dx/dy;
		}
		else
		{
			tk = tantbl[k].d;
	
			u = dy - tk*dx;
	
			v = tk*dy + dx;
	
			s = u/v;
		}

		zk = angletbl[k].d;

		if ( fabs(s) < twopm30.d )
		{
			poly = s;
		}
		else
		{
			ss = s*s;
	
			poly = ((P[3].d*ss + P[2].d)*ss + P[1].d)*(ss*s) + s;
		}

		result = poly + zk;

		/* must check for underflow to avoid trapping */

		if ( fabs(result) <= twopm150.d )
			return ( (signx == signy) ? 0.0f : -0.0f );

		return ( (float)result );
		
	}
	else
	{
		i = i + i;
		i = i + signx;

		j = j + j;
		j = j + signy;

		return ( _atan2fres4[i][j].f );
	}
}

