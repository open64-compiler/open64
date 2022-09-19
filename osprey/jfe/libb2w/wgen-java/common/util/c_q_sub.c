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
 *  Module: c_q_sub.c
 *  $Revision: 1.1.1.1 $
 *  $Date: 2005/10/21 19:00:00 $
 *  $Author: marcel $
 *  $Source: /proj/osprey/CVS/open64/osprey1.0/common/util/c_q_sub.c,v $
 *
 * =======================================================================
 * =======================================================================
 */


#include "defs.h"
#include "quad.h"

/* note that this routine must be kept in sync with the corresponding libc
 * routine, q_sub.
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

static const du		twop914 =
{0x79100000,	0x00000000};

static const du		inf =
{0x7ff00000,	0x00000000};

extern	QUAD	c_q_sub(QUAD, QUAD, INT *);

#pragma weak c_q_sub = __c_q_sub
#define	c_q_sub __c_q_sub

double	fabs(double);
#pragma intrinsic (fabs)

QUAD
c_q_sub(QUAD x, QUAD y, INT *p_err )
{
double	xhi, xlo, yhi, ylo;
INT32	ixhi, iyhi;
INT32	xptxhi, xptyhi;
INT64	iz, iw, iqulp;
double	w, ww;
double	u, uu;
double	qulp;
QUAD	z;
double	tmp1, tmp2, lo, rem;

	/* adapted from T. J. Dekker's add2 subroutine */

	*p_err = 0;

	xhi = x.hi; xlo = x.lo;
	yhi = y.hi; ylo = y.lo;

	iyhi = *(INT32 *)&yhi;
	xptyhi = (iyhi >> 20);
	xptyhi &= 0x7ff;

	ixhi = *(INT32 *)&xhi;
	xptxhi = (ixhi >> 20);
	xptxhi &= 0x7ff;

#ifdef QUAD_DEBUG
	printf("c_q_sub: xhi = %08x%08x\n", *(INT32 *)&xhi, *((INT32 *)&xhi + 1));
	printf("c_q_sub: xlo = %08x%08x\n", *(INT32 *)&xlo, *((INT32 *)&xlo + 1));
	printf("c_q_sub: yhi = %08x%08x\n", *(INT32 *)&yhi, *((INT32 *)&yhi + 1));
	printf("c_q_sub: ylo = %08x%08x\n", *(INT32 *)&ylo, *((INT32 *)&ylo + 1));
#endif

	yhi = -yhi;
	ylo = -ylo;

	if ( xptxhi < xptyhi )
	{
		tmp1 = xhi;
		xhi = yhi;
		yhi = tmp1;
		xptxhi = xptyhi;
	}

	if ( fabs(xlo) < fabs(ylo) )
	{
		tmp2 = xlo;
		xlo = ylo;
		ylo = tmp2;
	}

	if ( xptxhi < 0x7fd )
	{
		z.hi = xhi + yhi;
		z.lo = xhi - z.hi + yhi;

		u = xlo + ylo;
		uu = xlo - u + ylo;

		lo = z.lo + u;

		w =  z.hi + lo;
		ww = z.hi - w + lo;

		rem = z.lo - lo + u;

		ww += rem + uu;
		z.hi = w + ww;
		DBL2LL( z.hi, iz );
		z.lo = w - z.hi + ww;

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
	printf("q_add: z.hi = %08x%08x\n", *(INT32 *)&z.hi, *((INT32 *)&z.hi + 1));
	printf("q_add: z.lo = %08x%08x\n", *(INT32 *)&z.lo, *((INT32 *)&z.lo + 1));
#endif

		return ( z );
	}
	else if ( xptxhi == 0x7ff )
	{
		z.hi = xhi + yhi;
		z.lo = 0.0;

#ifdef QUAD_DEBUG
	printf("q_add: z.hi = %08x%08x\n", *(INT32 *)&z.hi, *((INT32 *)&z.hi + 1));
	printf("q_add: z.lo = %08x%08x\n", *(INT32 *)&z.lo, *((INT32 *)&z.lo + 1));
#endif

		return ( z );
	}
	else
	{
		if ( fabs(yhi) < twop914.d )
		{
			z.hi = xhi;
			z.lo = xlo;

#ifdef QUAD_DEBUG
	printf("q_add: z.hi = %08x%08x\n", *(INT32 *)&z.hi, *((INT32 *)&z.hi + 1));
	printf("q_add: z.lo = %08x%08x\n", *(INT32 *)&z.lo, *((INT32 *)&z.lo + 1));
#endif

			return ( z );
		}

		/*	avoid overflow in intermediate computations by 
			computing 4.0*(.25*x + .25*y)
		*/

		xhi *= 0.25;
		xlo *= 0.25;
		yhi *= 0.25;
		ylo *= 0.25;

		z.hi = xhi + yhi;
		z.lo = xhi - z.hi + yhi;

		u = xlo + ylo;
		uu = xlo - u + ylo;

		lo = z.lo + u;

		w =  z.hi + lo;
		ww = z.hi - w + lo;

		rem = z.lo - lo + u;

		ww += rem + uu;
		z.hi = w + ww;
		DBL2LL( z.hi, iz );
		z.lo = w - z.hi + ww;

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

			if ( fabs(z.lo) >= qulp )
			{
				qulp = 0.0;
			}
			else if ( z.lo < 0.0 )
				qulp = -qulp;

			z.lo += qulp;
			z.lo -= qulp;
		}

		z.hi *= 4.0;

		if ( fabs(z.hi) == inf.d )
		{
			z.lo = 0.0;
			return ( z );
		}

		z.lo *= 4.0;

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

	if ( fabs(z.lo) >= qulp )
	{
		qulp = 0.0;
	}
	else if ( z.lo < 0.0 )
		qulp = -qulp;

	z.lo += qulp;
	z.lo -= qulp;

	z.hi *= 4.0;

	if ( fabs(z.hi) == inf.d )
	{
		z.lo = 0.0;
		return ( z );
	}

	z.lo *= 4.0;

	return ( z );
}

