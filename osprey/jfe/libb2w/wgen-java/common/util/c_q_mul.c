/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


/* =======================================================================
 * =======================================================================
 *
 *  Module: c_q_mul.c
 *  $Revision: 1.1.1.1 $
 *  $Date: 2005/10/21 19:00:00 $
 *  $Author: marcel $
 *  $Source: /proj/osprey/CVS/open64/osprey1.0/common/util/c_q_mul.c,v $
 *
 * =======================================================================
 * =======================================================================
 */


#include "defs.h"
#include "quad.h"

/* note that this routine must be kept in sync with the corresponding libc
 * routine, q_mul.
 */

typedef union
{
	struct
	{
		UINT32 hi;
		UINT32 lo;
	} word;

	double	d;
} du;


/* const1 is 1.0 + 2^(53 - 53/2), i.e. 1.0 + 2^27 */

static	const du	const1 =
{0x41a00000,	0x02000000};

static	const du	twop590 =
{0x64d00000,	0x00000000};

static	const du	twopm590 =
{0x1b100000,	0x00000000};

static const du		inf =
{0x7ff00000,	0x00000000};

#define	NO	0
#define	YES	1

extern	QUAD	c_q_mul(QUAD, QUAD, INT *);

#pragma weak c_q_mul = __c_q_mul
#define	c_q_mul __c_q_mul

double	fabs(double);
#pragma intrinsic (fabs)

QUAD
c_q_mul(QUAD x, QUAD y, INT *p_err )
{
INT32	ixhi, iyhi;
INT32	xptxhi, xptyhi;
INT32	xpthi, xptlo;
INT32	scaleup, scaledown;
INT64	iz, iw, iqulp;
double	xhi, xlo, yhi, ylo;
double	xfactor, yfactor;
double	hx, tx, hy, ty;
double	p, q;
double	w, ww;
double	qulp;
QUAD	z, u;
double	rem;
double	a, v, vv;

	/* Avoid underflows and overflows in forming the products
	   xhi*const1.d, yhi*const1.d, xhi*yhi, and hx*hy by scaling if
	   necessary.  x and y should also be scaled if tx*ty is
	   a denormal.
	*/

	*p_err = 0;

	xhi = x.hi; xlo = x.lo;
	yhi = y.hi; ylo = y.lo;

#ifdef QUAD_DEBUG
	printf("c_q_mul: xhi = %08x%08x\n", *(INT32 *)&xhi, *((INT32 *)&xhi + 1));
	printf("c_q_mul: xlo = %08x%08x\n", *(INT32 *)&xlo, *((INT32 *)&xlo + 1));
	printf("c_q_mul: yhi = %08x%08x\n", *(INT32 *)&yhi, *((INT32 *)&yhi + 1));
	printf("c_q_mul: ylo = %08x%08x\n", *(INT32 *)&ylo, *((INT32 *)&ylo + 1));
#endif


	iyhi = *(INT32 *)&yhi;
	xptyhi = (iyhi >> 20);
	xptyhi &= 0x7ff;

	ixhi = *(INT32 *)&xhi;
	xptxhi = (ixhi >> 20);
	xptxhi &= 0x7ff;

	xpthi = xptxhi;
	xptlo = xptyhi;

	if ( xptxhi < xptyhi )
	{
		xptlo = xptxhi;
		xpthi = xptyhi;
	}

	if ( (0x21a < xptlo) && (xpthi < 0x5fd) )
	{
		/* normal case */

		/* Form the exact product of xhi and yhi using
		 * Dekker's algorithm.
		 */

		p = xhi*const1.d;
	
		hx = (xhi - p) + p;
		tx = xhi - hx;
	
		q = yhi*const1.d;
	
		hy = (yhi - q) + q;
		ty = yhi - hy;
	
		u.hi = xhi*yhi;
		u.lo = (((hx*hy - u.hi) + hx*ty) + hy*tx) + tx*ty;

		/* Add the remaining pieces using the Kahan summation formula. */

		a = xhi*ylo;

		ww = u.lo + a;
		rem = u.lo - ww + a;

		v = u.hi + ww;
		vv = u.hi - v + ww;

		a = xlo*yhi + rem;
		u.lo = vv + a;

		rem = vv - u.lo + a;

		w = v + u.lo;
		ww = v - w + u.lo;

		a = xlo*ylo + rem;

		vv = ww + a;

		z.hi = w + vv;
		DBL2LL(z.hi, iz);
		z.lo = w - z.hi + vv;

		/* if necessary, round z.lo so that the sum of z.hi and z.lo has at most
		   107 significant bits
		*/

		/* first, compute a quarter ulp of z */

		iw = (iz >> DMANTWIDTH);
		iqulp = (iw & 0x7ff);
		iqulp -= 54;
		iqulp <<= DMANTWIDTH;

		if ( iqulp > 0 )
		{
			LL2DBL( iqulp, qulp );
			iw <<= DMANTWIDTH;

			/* Note that the size of an ulp changes at a
			 * power of two.
			 */

			if ( iw == iz )
				goto fix;

			if ( fabs(z.lo) >= qulp )
			{
				qulp = 0.0;
			}
			else if ( z.lo < 0.0 )
				qulp = -qulp;

			z.lo += qulp;
			z.lo -= qulp;
		}

#ifdef QUAD_DEBUG
	printf("c_q_mul: z.hi = %08x%08x\n", *(INT32 *)&z.hi, *((INT32 *)&z.hi + 1));
	printf("c_q_mul: z.lo = %08x%08x\n", *(INT32 *)&z.lo, *((INT32 *)&z.lo + 1));
#endif

		return ( z );
	}
	else if ( (xptxhi == 0x7ff) || (xhi == 0.0) || (yhi == 0.0) )
	{
		z.hi = xhi*yhi;
		z.lo = 0.0;

#ifdef QUAD_DEBUG
	printf("c_q_mul: z.hi = %08x%08x\n", *(INT32 *)&z.hi, *((INT32 *)&z.hi + 1));
	printf("c_q_mul: z.lo = %08x%08x\n", *(INT32 *)&z.lo, *((INT32 *)&z.lo + 1));
#endif

		return ( z );
	}
	else
	{
		xfactor = 1.0;
		yfactor = 1.0;
		scaleup = scaledown = NO;

		if ( xptxhi <= 0x21a )
		{
			xhi *= twop590.d;
			xlo *= twop590.d;
			xfactor = twopm590.d;
			scaleup = YES;
		}

		if ( xptyhi <= 0x21a )
		{
			yhi *= twop590.d;
			ylo *= twop590.d;
			yfactor = twopm590.d;
			scaleup = YES;
		}

		if ( xptxhi >= 0x5fd )
		{
			xhi *= twopm590.d;
			xlo *= twopm590.d;
			xfactor = twop590.d;
			scaledown = YES;
		}

		if ( xptyhi >= 0x5fd )
		{
			yhi *= twopm590.d;
			ylo *= twopm590.d;
			yfactor = twop590.d;
			scaledown = YES;
		}

		if ( (scaleup == YES) && (scaledown == YES) )
		{
			xfactor = yfactor = 1.0;
		}


		/* Form the exact product of xhi and yhi using
		 * Dekker's algorithm.
		 */

		p = xhi*const1.d;
	
		hx = (xhi - p) + p;
		tx = xhi - hx;
	
		q = yhi*const1.d;
	
		hy = (yhi - q) + q;
		ty = yhi - hy;
	
		u.hi = xhi*yhi;
		u.lo = (((hx*hy - u.hi) + hx*ty) + hy*tx) + tx*ty;

		/* Add the remaining pieces using the Kahan summation formula. */

		a = xhi*ylo;

		ww = u.lo + a;
		rem = u.lo - ww + a;

		v = u.hi + ww;
		vv = u.hi - v + ww;

		a = xlo*yhi + rem;
		u.lo = vv + a;

		rem = vv - u.lo + a;

		w = v + u.lo;
		ww = v - w + u.lo;

		a = xlo*ylo + rem;

		vv = ww + a;

		z.hi = w + vv;
		z.lo = w - z.hi + vv;

		/* Rescale z.hi and z.lo before rounding */

		w = z.hi*xfactor;
		w *= yfactor;

		if ( (w == 0.0) || (fabs(w) == inf.d) )
		{
			z.hi = w;
			z.lo = 0.0;

#ifdef QUAD_DEBUG
	printf("c_q_mul: z.hi = %08x%08x\n", *(INT32 *)&z.hi, *((INT32 *)&z.hi + 1));
	printf("c_q_mul: z.lo = %08x%08x\n", *(INT32 *)&z.lo, *((INT32 *)&z.lo + 1));
#endif

			return ( z );
		}

		ww = z.lo*xfactor;
		ww *= yfactor;

		z.hi = w + ww;
		DBL2LL(z.hi, iz);
		z.lo = w - z.hi + ww;

#ifdef QUAD_DEBUG
	printf("c_q_mul: z.hi = %08x%08x\n", *(INT32 *)&z.hi, *((INT32 *)&z.hi + 1));
	printf("c_q_mul: z.lo = %08x%08x\n", *(INT32 *)&z.lo, *((INT32 *)&z.lo + 1));
#endif

		/* if necessary, round z.lo so that the sum of z.hi and z.lo has at most
		   107 significant bits
		*/

		/* first, compute a quarter ulp of z */

		iw = (iz >> DMANTWIDTH);
		iqulp = (iw & 0x7ff);
		iqulp -= 54;
		iqulp <<= DMANTWIDTH;

		if ( iqulp > 0 )
		{
			LL2DBL( iqulp, qulp );
			iw <<= DMANTWIDTH;

			/* Note that the size of an ulp changes at a
			 * power of two.
			 */

			if ( iw == iz )
				goto fix2;

back:
			if ( fabs(z.lo) >= qulp )
			{
				qulp = 0.0;
			}
			else if ( z.lo < 0.0 )
				qulp = -qulp;

			z.lo += qulp;
			z.lo -= qulp;
		}

		return ( z );
	}

fix:
	if ( ((z.hi > 0.0) && (z.lo < 0.0)) || ((z.hi < 0.0) && (z.lo > 0.0)) )
		qulp *= 0.5;

	if ( fabs(z.lo) >= qulp )
	{
		qulp = 0.0;
	}
	else if ( z.lo < 0.0 )
		qulp = -qulp;

	z.lo += qulp;
	z.lo -= qulp;

	return ( z );

fix2:
	if ( ((z.hi > 0.0) && (z.lo < 0.0)) || ((z.hi < 0.0) && (z.lo > 0.0)) )
		qulp *= 0.5;

	goto back;
}

