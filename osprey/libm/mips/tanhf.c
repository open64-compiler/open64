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
 * Module: tanhf.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:23-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.tanhf.c $
 *
 * Revision history:
 *  09-Jun-93 - Original Version
 *
 * Description:	source code for tanhf function
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.tanhf.c $ $Revision: 1.5 $";

#ifdef _CALL_MATHERR
#include <stdio.h>
#include <math.h>
#include <errno.h>
#endif

#include "libm.h"

#if defined(mips) && !defined(__GNUC__)
extern	float	ftanh(float);
extern	float	tanhf(float);

#pragma weak ftanh = __tanhf
#pragma weak tanhf = __tanhf
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern float __tanhf(float);
#pragma weak tanhf
float tanhf( float x ) {
  return __tanhf( x );
}
#elif defined(__GNUC__)
extern  float  __tanhf(float);
float    tanhf(float) __attribute__ ((weak, alias ("__tanhf")));
#endif

/* coefficients for approximating exp
   on +/- ln(2)/64
*/

static const du	P[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0x3ff00000, 0x00000000)},
{D(0x3fe00008, 0x745da559)},
{D(0x3fc55569, 0x9fd0029e)},
};

/* coefficients for approximating tanh
   on +/- 0.5
*/

static const du	P2[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0xbfd55555, 0x10101b35)},
{D(0x3fc110f2, 0x1b30079f)},
{D(0xbfab98ac, 0xb894835d)},
{D(0x3f95cf23, 0xf79c6537)},
{D(0xbf7b3a65, 0x369caf92)},
};

static const du	rln2by32 =
{D(0x40471547, 0x652b82fe)};

static const du	ln2by32hi =
{D(0x3f962e42, 0xfef00000)};

static const du	ln2by32lo =
{D(0x3d8473de, 0x6af278ed)};

extern const du _expftab[];

static const	fu	Qnan = {QNANF};

static const fu	Ulimit = {0x41102cb3};

static const fu	Llimit = {0xc1102cb3};

static const fu	f_one = {0x3f800000};

static const fu	f_m_one = {0xbf800000};


/* ====================================================================
 *
 * FunctionName		tanhf
 *
 * Description		computes hyperbolic tangent of arg
 *
 * ====================================================================
 */

float
__tanhf( float x )
{
#ifdef _32BIT_MACHINE

int	l;

#else

long long l;

#endif

int	ix, xpt;
int	r, m, k;
double	dx, absdx;
double	twopk;
double	result;
double	md;
double	xsq;
double	y, t;
double	poly;
#ifdef _CALL_MATHERR
struct exception	exstruct;
#endif

	/* extract exponent of x for some quick screening */

	FLT2INT(x, ix);		/* copy arg to an integer	*/
	xpt = (ix >> MANTWIDTH);
	xpt &= 0xff;

	if ( xpt < 0x82 )
	{
		/* |x| < 8.0 */

		if ( xpt >= 0x7e )
		{
			/* |x| >= 0.5; compute tanh(x) as 1.0 - 2.0/(exp(2*x) + 1) */
L:
			dx = x;
			absdx = fabs(dx);	/* absdx = |x| */

			/* compute exp(2*|x|) */

			absdx += absdx;

			/* reduce absdx to +/- log(2)/64 	*/

			md = absdx*rln2by32.d;
			m = ROUND(md);
			md = m;

			r = m & 0x1f;

			/* express m as 32*k + r, where 0 <= r < 32 */

			k = m >> 5;

			y = (absdx - md*ln2by32hi.d) - md*ln2by32lo.d;

			/* fetch 2^(r/32) from table */

			t = _expftab[r].d;

			/* compute exp(y) by a polynomial approximation */

			poly = ((P[3].d*y + P[2].d)*y + P[1].d)*y;

			/* exp(2*|x|) = exp(y)*2^(r/32)*2^k */

			result = t + t*poly;

			l = k + DEXPBIAS;
			l <<= DMANTWIDTH;

			/* multiply result by 2**k	*/

#ifdef _32BIT_MACHINE

			twopk = 0.0;
			INT2DBLHI(l, twopk);
#else
			LL2DBL(l, twopk);
#endif
			result *= twopk;

			result = 1.0 - 2.0/(result + 1.0);

			if ( x < (float)0.0 )
				result = -result;

			return ( result );
		}

		if ( xpt >= 0x73 )
		{
			/* |x| >= 2^(-12) */

			/* compute tanh(x) using a polynomial approximation */

			dx = x;

			xsq = dx*dx;

			result = ((((P2[5].d*xsq + P2[4].d)*xsq + P2[3].d)*xsq +
					P2[2].d)*xsq + P2[1].d)*(xsq*dx) + dx;

			return ( (float)result );
		}

		return ( x );
	}

	if ( x != x )
	{
		/* x is a NaN; return a quiet NaN */

#ifdef _CALL_MATHERR

                exstruct.type = DOMAIN;
                exstruct.name = "tanhf";
                exstruct.arg1 = x;
                exstruct.retval = Qnan.f;

                if ( matherr( &exstruct ) == 0 )
                {
                        fprintf(stderr, "domain error in tanhf\n");
                        SETERRNO(EDOM);
                }

                return ( exstruct.retval );
#else
		NAN_SETERRNO(EDOM);
		
		return ( Qnan.f );
#endif
	}

	if ( x > Ulimit.f )
		return ( f_one.f );

	if ( x < Llimit.f )
		return ( f_m_one.f );

	goto L;

}

