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
 *  Module: c_q_div.c
 *  $Revision$
 *  $Date$
 *  $Author$
 *  $Source$
 *
 * =======================================================================
 * =======================================================================
 */


#include "defs.h"
#include "quad.h"

/* note that this routine must be kept in sync with the corresponding libc
 * routine, q_div.
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

static const du		twopm968 =
{0x03700000,	0x00000000};

static const du		twopm54 =
{0x3c900000,	0x00000000};

static const du		twop52 =
{0x43300000,	0x00000000};

static const du		inf =
{0x7ff00000,	0x00000000};

extern	QUAD	c_q_div(QUAD, QUAD, INT *);

#if defined(BUILD_OS_DARWIN)
/* Can't use "pragma weak" to create aliases in Mach-O */
QUAD c_q_div(QUAD x, QUAD y, INT *p_err );
QUAD __c_q_div(QUAD x, QUAD y, INT *p_err ) { return c_q_div(x, y, p_err); }
#else /* defined(BUILD_OS_DARWIN) */
#pragma weak c_q_div = __c_q_div
#define	c_q_div __c_q_div
#endif /* defined(BUILD_OS_DARWIN) */

double	fabs(double);
#pragma intrinsic (fabs)

#define	EXPBIAS	0x3ff

	/* computes the quotient of two long doubles */

QUAD
c_q_div(QUAD x, QUAD y, INT *p_err )
{
double	xhi, xlo, yhi, ylo;
INT32	n;
INT64	ixhi, iyhi;
INT64	xptxhi, xptyhi;
INT64	ix, iy, iz;
double	c, cc, w, ww;
double	quarterulp;
double	xfactor, yfactor;
double	p, hc, tc;
double	hyhi, tyhi;
double	z, zz;
QUAD	u;
QUAD	result;

	/* adapted from T. J. Dekker's div2 subroutine */

#ifdef QUAD_DEBUG
	printf("c_q_div: xhi = %08x%08x\n", *(INT32 *)&xhi, *((INT32 *)&xhi + 1));
	printf("c_q_div: xlo = %08x%08x\n", *(INT32 *)&xlo, *((INT32 *)&xlo + 1));
	printf("c_q_div: yhi = %08x%08x\n", *(INT32 *)&yhi, *((INT32 *)&yhi + 1));
	printf("c_q_div: ylo = %08x%08x\n", *(INT32 *)&ylo, *((INT32 *)&ylo + 1));
#endif

	*p_err = 0;

	xhi = x.hi; xlo = x.lo;
	yhi = y.hi; ylo = y.lo;

	/* extract exponents of y and x for some quick screening */

	DBL2LL(yhi, iyhi);
	xptyhi = (iyhi >> DMANTWIDTH);
	xptyhi &= 0x7ff;

	DBL2LL(xhi, ixhi);
	xptxhi = (ixhi >> DMANTWIDTH);
	xptxhi &= 0x7ff;

	/* Avoid underflows and overflows in forming the products
	   x*const1.d, y*const1.d, x*y, and hx*hy by scaling if
	   necessary.  x and y are also be scaled if tx*ty is
	   a denormal.
	*/

	if ( (0x30d < xptxhi) && (xptxhi < 0x4f1) && 
	     (0x30d < xptyhi) && (xptyhi < 0x4f1) 
	   )
	{
		/* normal case */

		c = xhi/yhi;
/*
		u.ld = __qprod(c, yhi);
*/
		p = c*const1.d;
	
		hc = (c - p) + p;
		tc = c - hc;
	
		p = yhi*const1.d;
	
		hyhi = (yhi - p) + p;
		tyhi = yhi - hyhi;
	
		u.hi = c*yhi;
		u.lo = (((hc*hyhi - u.hi) + hc*tyhi) + hyhi*tc) + tc*tyhi;

		cc = ((xhi - u.hi - u.lo + xlo) - c*ylo)/yhi;

		z = c + cc;
		zz = (c - z) + cc;

#ifdef QUAD_DEBUG
	printf("c_q_div: z = %08x%08x\n", *(INT32 *)&z, *((INT32 *)&z + 1));
	printf("c_q_div: zz = %08x%08x\n", *(INT32 *)&zz, *((INT32 *)&zz + 1));
#endif

		/* if necessary, round zz so that the sum of z and zz has
		   at most 107 significant bits
		*/

		if ( fabs(z) >= twopm968.d )
		{
			/* determine true exponent of z + zz as a 107 bit number */

			DBL2LL(z, iz);
			iz >>= DMANTWIDTH;
			iz <<= DMANTWIDTH;
			LL2DBL(iz, w); /* w = dtwofloor(z) */

			if ( (z == w) && ((z > 0.0 && zz < 0.0) ||
			     (z < 0.0 && zz > 0.0))
			   )
				w *= 0.5;

			/* round zz if it's less than 1/4 ulp of w */

			quarterulp = twopm54.d*fabs(w);

			if ( fabs(zz) < quarterulp )
			{
				if ( zz >= 0.0 )
				{
					zz = (quarterulp + zz) - quarterulp;
				}
				else
				{
					zz = quarterulp + (zz - quarterulp);
				}

				w = z + zz;
				ww = (z - w) + zz;

				z = w;
				zz = ww;
			}
		}

		result.hi = z;
		result.lo = zz;

#ifdef QUAD_DEBUG
	printf("c_q_div: result.hi = %08x%08x\n", *(INT32 *)&result.hi, *((INT32 *)&result.hi + 1));
	printf("c_q_div: result.lo = %08x%08x\n", *(INT32 *)&result.lo, *((INT32 *)&result.lo + 1));
#endif

		return ( result );
	}

	if ( (xptxhi < 0x7ff) && (xptyhi < 0x7ff) )
	{
		if ( (xhi == 0.0) || (yhi == 0.0) )
		{
			result.hi = xhi/yhi;
			result.lo = 0.0;

#ifdef QUAD_DEBUG
	printf("c_q_div: result.hi = %08x%08x\n", *(INT32 *)&result.hi, *((INT32 *)&result.hi + 1));
	printf("c_q_div: result.lo = %08x%08x\n", *(INT32 *)&result.lo, *((INT32 *)&result.lo + 1));
#endif

			return ( result );
		}

		xfactor = 1.0;
		yfactor = 1.0;

		/* Scale x and y appropriately and use previous algorithm.
		   Then unscale result.
		*/

		if ( xptxhi <= 0x30d )
		{
			xhi *= twop52.d;	/* first, make sure x is normal */
			xlo *= twop52.d;

			/* now scale x so that its exponent is 0x30e */

			DBL2LL(xhi, ix);
			ix >>= DMANTWIDTH;
			n = (ix & 0x7ff);

			ix = (0x30e - n) + EXPBIAS;
			ix <<= DMANTWIDTH;
			LL2DBL(ix, c);
			xhi *= c;
			xlo *= c;
			ix = (n - 52 - 0x30e) + EXPBIAS;
			ix <<= DMANTWIDTH;
			LL2DBL(ix, xfactor);
		}

		if ( xptyhi <= 0x30d )
		{
			yhi *= twop52.d;	/* first, make sure y is normal */
			ylo *= twop52.d;

			/* now scale y so that its exponent is 0x30e */

			DBL2LL(yhi, iy);
			iy >>= DMANTWIDTH;
			n = (iy & 0x7ff);

			iy = (0x30e - n) + EXPBIAS;
			iy <<= DMANTWIDTH;
			LL2DBL(iy, c);
			yhi *= c;
			ylo *= c;
			iy = (0x30e - n + 52) + EXPBIAS;
			iy <<= DMANTWIDTH;
			LL2DBL(iy, yfactor);
		}

		if ( xptxhi >= 0x4f1 )
		{
			/* scale x so that its exponent is 0x4f1 */

			DBL2LL(xhi, ix);
			ix >>= DMANTWIDTH;
			n = (ix & 0x7ff);

			ix = (0x4f1 - n) + EXPBIAS;
			ix <<= DMANTWIDTH;
			LL2DBL(ix, c);
			xhi *= c;
			xlo *= c;
			ix = (n - 0x4f1) + EXPBIAS;
			ix <<= DMANTWIDTH;
			LL2DBL(ix, xfactor);
		}

		if ( xptyhi >= 0x4f1 )
		{
			/* scale y so that its exponent is 0x4f1 */

			DBL2LL(yhi, iy);
			iy >>= DMANTWIDTH;
			n = (iy & 0x7ff);

			iy = (0x4f1 - n) + EXPBIAS;
			iy <<= DMANTWIDTH;
			LL2DBL(iy, c);
			yhi *= c;
			ylo *= c;
			iy = (0x4f1 - n) + EXPBIAS;
			iy <<= DMANTWIDTH;
			LL2DBL(iy, yfactor);
		}

		c = xhi/yhi;

/*
		u.ld = __qprod(c, yhi);
*/
		p = c*const1.d;
	
		hc = (c - p) + p;
		tc = c - hc;
	
		p = yhi*const1.d;
	
		hyhi = (yhi - p) + p;
		tyhi = yhi - hyhi;
	
		u.hi = c*yhi;
		u.lo = (((hc*hyhi - u.hi) + hc*tyhi) + hyhi*tc) + tc*tyhi;

		cc = ((xhi - u.hi - u.lo + xlo) - c*ylo)/yhi;

		z = c + cc;
		zz = (c - z) + cc;

#ifdef QUAD_DEBUG
	printf("c_q_div: z = %08x%08x\n", *(INT32 *)&z, *((INT32 *)&z + 1));
	printf("c_q_div: zz = %08x%08x\n", *(INT32 *)&zz, *((INT32 *)&zz + 1));
#endif

		/* if necessary, round zz so that the sum of z and zz has
		   at most 107 significant bits
		*/

		if ( fabs(z) >= twopm968.d )
		{
			/* determine true exponent of z + zz as a 107 bit number */

			DBL2LL(z, iz);
			iz >>= DMANTWIDTH;
			iz <<= DMANTWIDTH;
			LL2DBL(iz, w); /* w = dtwofloor(z) */

			if ( (z == w) && ((z > 0.0 && zz < 0.0) ||
			     (z < 0.0 && zz > 0.0))
			   )
				w *= 0.5;

			/* round zz if it's less than 1/4 ulp of w */

			quarterulp = twopm54.d*fabs(w);

			if ( fabs(zz) < quarterulp )
			{
				if ( zz >= 0.0 )
				{
					zz = (quarterulp + zz) - quarterulp;
				}
				else
				{
					zz = quarterulp + (zz - quarterulp);
				}

				w = z + zz;
				ww = (z - w) + zz;

				z = w;
				zz = ww;
			}
		}

		if ( ((xfactor <= 1.0) && (1.0 <= yfactor)) ||
		     ((yfactor <= 1.0) && (1.0 <= xfactor))
		   )
		{
			z = z*(xfactor*yfactor);
		}
		else
		{
			z *= xfactor;
			z *= yfactor;
		}

		if ( (z == 0.0) || (fabs(z) == inf.d) )
		{
			result.hi = z;
			result.lo = 0.0;

#ifdef QUAD_DEBUG
	printf("c_q_div: result.hi = %08x%08x\n", *(INT32 *)&result.hi, *((INT32 *)&result.hi + 1));
	printf("c_q_div: result.lo = %08x%08x\n", *(INT32 *)&result.lo, *((INT32 *)&result.lo + 1));
#endif

			return ( result );
		}

		if ( ((xfactor <= 1.0) && (1.0 <= yfactor)) ||
		     ((yfactor <= 1.0) && (1.0 <= xfactor))
		   )
		{
			zz = zz*(xfactor*yfactor);
		}
		else
		{
			zz *= xfactor;
			zz *= yfactor;
		}

		result.hi = z + zz;
		result.lo = (z - result.hi) + zz;

#ifdef QUAD_DEBUG
	printf("c_q_div: result.hi = %08x%08x\n", *(INT32 *)&result.hi, *((INT32 *)&result.hi + 1));
	printf("c_q_div: result.lo = %08x%08x\n", *(INT32 *)&result.lo, *((INT32 *)&result.lo + 1));
#endif

		return ( result );
	}

	result.hi = xhi/yhi;
	result.lo = 0.0;

#ifdef QUAD_DEBUG
	printf("c_q_div: result.hi = %08x%08x\n", *(INT32 *)&result.hi, *((INT32 *)&result.hi + 1));
	printf("c_q_div: result.lo = %08x%08x\n", *(INT32 *)&result.lo, *((INT32 *)&result.lo + 1));
#endif

	return ( result );
}

