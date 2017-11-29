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
 * Module: atan2.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:21-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.atan2.c $
 *
 * Revision history:
 *  09-Jun-93 - Original Version
 *
 * Description:	source code for atan2 function
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.atan2.c $ $Revision: 1.5 $";

#ifdef _CALL_MATHERR
#include <stdio.h>
#include <math.h>
#include <errno.h>
#endif

#include "libm.h"

#if defined(mips) && !defined(__GNUC__)
extern	double	atan2(double, double);

#pragma weak atan2 = __atan2
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern double __atan2(double, double);
#pragma weak atan2
double atan2(double y, double x) {
  return __atan2( y, x );
}
#elif defined(__GNUC__)
extern  double  __atan2(double, double);

double    atan2() __attribute__ ((weak, alias ("__atan2")));

#endif

extern	const du	_atan2res0[2][2];
extern	const du	_atan2res1[2][2];
extern	const du	_atan2res2[2][2];
extern	const du	_atan2res4[4][4];

static const	du	Qnan =
{D(QNANHI, QNANLO)};

static	const du	twop60 =
{D(0x43b00000, 0x00000000)};

static	const du	twopm28 =
{D(0x3e300000, 0x00000000)};

static	const du	limit1 =
{D(0x3fc445f0, 0xfbb1cf92)};	/* tan(pi/20) */

static	const du	limit2 =
{D(0x3fe04e08, 0x50c1dd5c)};	/* tan(3*pi/20) */

static	const du	rlimit4 =
{D(0x3fe04e08, 0x50c1dd5c)};	/* 1/tan(7*pi/20) */

static	const du	rlimit5 =
{D(0x3fc445f0, 0xfbb1cf92)};	/* 1/tan(9*pi/20) */

static const du	piby2 =
{D(0x3ff921fb, 0x54442d18)};

static const du	m_piby2 =
{D(0xbff921fb, 0x54442d18)};

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

/* coefficients for a 15th degree polynomial approximation of atan
   on the interval +/- tan(pi/20)
*/

static	const du	P[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0xbfd55555, 0x55555547)},
{D(0x3fc99999, 0x999924e3)},
{D(0xbfc24924, 0x91a937fe)},
{D(0x3fbc71c6, 0x4c76a27a)},
{D(0xbfb74589, 0x00fd881c)},
{D(0x3fb3a350, 0x167cd5be)},
{D(0xbfaf5682, 0x2746dfc3)},
};


/* ====================================================================
 *
 * FunctionName		atan2
 *
 * Description		computes arctangent of arg1/arg2
 *
 * ====================================================================
 */

double
__atan2( y, x )
double	y, x;
{
#ifdef _32BIT_MACHINE

int	ix, iy;
int	xptx, xpty, xpts;
int	signx, signy;
int	l;

#else

long long	ix, iy;
long long	xptx, xpty, xpts;
long long	signx, signy;
long long	l;

#endif

int	i, j, k;
double	absx, absy;
double	tk, zk;
double	poly;
double	u, v, s, ss;
double	result;
#ifdef _CALL_MATHERR
struct exception	exstruct;
#endif

	/* extract exponents of y and x for some quick screening */

#ifdef _32BIT_MACHINE

	DBLHI2INT(y, iy);	/* copy MSW of y to iy */
	DBLHI2INT(x, ix);	/* copy MSW of x to ix */
#else
	DBL2LL(y, iy);		/* copy y to iy */
	DBL2LL(x, ix);		/* copy x to ix */
#endif
	xpty = (iy >> DMANTWIDTH);
	xpty &= 0x7ff;

	xptx = (ix >> DMANTWIDTH);
	xptx &= 0x7ff;

	signy = (iy >> (DMANTWIDTH + DEXPWIDTH));
	signy = (signy & 1);

	signx = (ix >> (DMANTWIDTH + DEXPWIDTH));
	signx = (signx & 1);

	/* filter out Nans */

	if ( (xpty == 0x7ff) || (xptx == 0x7ff) )
	{
		if ( (y != y) || (x != x) )
		{
			/* y or x is a NaN; return a quiet NaN */

#ifdef _CALL_MATHERR
			exstruct.type = DOMAIN;
			exstruct.name = "atan2";
			exstruct.arg1 = y;
			exstruct.arg2 = x;
			exstruct.retval = Qnan.d;

			if ( matherr( &exstruct ) == 0 )
			{
				fprintf(stderr, "domain error in atan2\n");
				SETERRNO(EDOM);
			}

			return ( exstruct.retval );
#else
			NAN_SETERRNO(EDOM);

			return ( Qnan.d );
#endif
		}
	}

	/* filter out zero arguments */

	if ( x == 0.0 )
	{
		if ( y == 0.0 )
		{
#ifdef _CALL_MATHERR

			exstruct.type = DOMAIN;
			exstruct.name = "atan2";
			exstruct.arg1 = y;
			exstruct.arg2 = x;
			exstruct.retval = _atan2res0[signx][signy].d;

			if ( matherr( &exstruct ) == 0 )
			{
				fprintf(stderr, "domain error in atan2\n");
				SETERRNO(EDOM);
			}

			return ( exstruct.retval );
#else
			SETERRNO(EDOM);

			return ( _atan2res0[signx][signy].d );
#endif
		}

		return ( _atan2res1[signx][signy].d );
	}
	else if ( (xpty == 0) && (y == 0.0) )
	{
		result = _atan2res2[signx][signy].d;
		return ( result );
	}

	/* a crude check to avoid underflow of x/y */

	if ( xpty > xptx + 54 )
		return ( (signy == 0) ? piby2.d : m_piby2.d );

	/* a crude check for underflow of y/x */

	if ( xpty < xptx - 1075 )
		return ( _atan2res2[signx][signy].d );

	/* Get rid of denorms by scaling; the preceding tests
	   guarantee that no infinities will be generated by
	   the scaling.
	*/

	if ( (xpty == 0) || (xptx == 0) )
	{
		y = twop60.d*y;
		x = twop60.d*x;
	}

	/* filter out infinities */

	i = (xptx == 0x7ff);

	j = (xpty == 0x7ff);

	/* if either x or y is very large, scale them both down to
	   avoid overflow in the computation of s below
	*/

	if ( (xpty >= 0x7fc) || (xptx >= 0x7fc) )
	{
		y = 0.25*y;
		x = 0.25*x;
	}

	if ( (i + j) == 0 )
	{
		absx = fabs(x);

		absy = fabs(y);

		/* Note that the products in the next several lines
		   will neither underflow nor overflow due to the
		   earlier screening and scaling of arguments.
		*/

		if ( absy < absx )
		{
			j = (absy >= absx*limit1.d);
			k = (absy >= absx*limit2.d);
		}
		else
		{
			j = (absx <= absy*rlimit4.d);
			k = (absx <= absy*rlimit5.d);
			j = j + 3;
		}

		k = j + k;

		k = 4*k + 2*signx + signy;

		/*  compute reduced arg between +/- tan(pi/20)  */

		if ( k < 4 )
		{
			if ( xpty < (xptx - 1074) )
			{
				/* possible underflow of y/x */

				v = x;
		
#ifdef _32BIT_MACHINE

				DBLHI2INT(v, l);
				l &= DEXPMASK;
				l |= (0x3ff << DMANTWIDTH);
#else
				DBL2LL(v, l);
				l &= DEXPMASK;
				l |= (0x3ffll << DMANTWIDTH);
#endif
				s = y/v;

#ifdef _32BIT_MACHINE

				DBLHI2INT(s, xpts); /* copy MSW of s to xpts */
#else
				DBL2LL(s, xpts); /* copy s to xpts */
#endif
				xpts >>= DMANTWIDTH;
				xpts &= 0x7ff;

				if ( (xpts + xptx - DEXPBIAS) < -1075 )
				{
					/* y/x underflows; set s to +/- 0 */

					s = 0.0*s;
				}
				else
				{
					s = y/x;
				}
			}
			else
			{
				s = y/x;
			}
		}
		else if ( k > 19 )
		{
			s = -x/y;
		}
		else
		{
			tk = tantbl[k].d;
	
			u = y - tk*x;
	
			v = tk*y + x;
	
			s = u/v;
		}

		zk = angletbl[k].d;

		/* s = (y - tan(zk)*x)/(tan(zk)*y + x)
		   Note that atan(y/x) is zk + atan(s); this is a standard
		   trigonometric identity:
		   Let tk = tan(zk).
		   Using the formula for the tangent of the difference of
		   two angles, we have tan(atan(y/x) - zk) =
			(y/x - tk)/(1 + y/x*tk) = s, so 
		   atan(y/x) - zk = atan(s).
		*/

		if ( fabs(s) < twopm28.d )
			return ( s + zk );

		ss = s*s;


		poly = (((((P[7].d*ss + P[6].d)*ss + P[5].d)*ss + P[4].d)*ss +
			P[3].d)*ss + P[2].d)*ss + P[1].d;

		result = poly*(ss*s) + s + zk;

		return ( result );
		
	}
	else
	{
		i = i + i;
		i = i + signx;

		j = j + j;
		j = j + signy;

		return ( _atan2res4[i][j].d );
	}
}

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
#pragma weak atan2l
long double atan2l( long double y, long double x ) {
	return ( (long double)__atan2((double)y, (double)x) );
}
#elif defined(__GNUC__)
extern  long double  __atan2l(long double, long double);

long double    atan2l() __attribute__ ((weak, alias ("__atan2l")));

#endif

long double
__atan2l( y, x )
long double	y, x;
{
	return ( (long double)__atan2((double)y, (double)x) );
}

