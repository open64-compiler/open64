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
 *  Module: c_q_sqrt.c
 *  $Revision: 1.1.1.1 $
 *  $Date: 2005/10/21 19:00:00 $
 *  $Author: marcel $
 *  $Source: /proj/osprey/CVS/open64/osprey1.0/common/util/c_q_sqrt.c,v $
 *
 * =======================================================================
 * =======================================================================
 */


#ifndef _LP64
#include <sgidefs.h>
#endif /* _LP64 */
#include <math.h>
#include "defs.h"
#include "quad.h"

/* quad square root */

/* note that this routine must be kept in sync with the corresponding libm
 * routine, qsqrt.
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


#ifndef __MATH_H__
double  sqrt(double);

#if _MIPS_ISA != _MIPS_ISA_MIPS1
#pragma intrinsic (sqrt)
#endif
#endif /* __MATH_H__ */

static	double	__dtwofloor(double);

/* const1 is 1.0 + 2^(53 - 53/2), i.e. 1.0 + 2^27 */

static	const du	const1 =
{0x41a00000,	0x02000000};

static const du		twopm53 =
{0x3ca00000,	0x00000000};

static const du		twopm54 =
{0x3c900000,	0x00000000};

static const du		twopm6 =
{0x3f900000,	0x00000000};

static const du		twop3 =
{0x40200000,	0x00000000};

static const du		twop108 =
{0x46b00000,	0x00000000};

#pragma weak c_q_sqrt = __c_q_sqrt
#define	c_q_sqrt __c_q_sqrt

QUAD
c_q_sqrt(QUAD x, INT *p_err )
{
INT64	ix, xpt;
INT32	n;
double	xfactor;
double	w;
double	quarterulp;
double	p;
double	hc, tc;
QUAD	c;
QUAD	u, z;

	/* adapted from T. J. Dekker's sqrt2 subroutine */

	*p_err = 0;

	/* extract exponent of x for some quick screening */

	DBL2LL(x.hi, ix);
	xpt = (ix >> DMANTWIDTH);
	xpt &= 0x7ff;

	if ( ix < 0 )
	{
		z.hi = sqrt(x.hi);
		z.lo = 0.0;

		return ( z );
	}

	/* Avoid underflows and overflows in forming the products
	   c.hi*const1.d, c.hi*c.hi, and hc*hc by scaling if
	   necessary.  x should also be scaled if tc*tc is
	   a denormal.
	*/

	if ( (0x06d < xpt) && (xpt < 0x7fb) )
	{
		/* normal case */

		c.hi = sqrt(x.hi);

		/*u.ld = _prodl(c.hi, c.hi);*/

		p = c.hi*const1.d;
	
		hc = (c.hi - p) + p;
		tc = c.hi - hc;
	
		u.hi = c.hi*c.hi;
		u.lo = ((((hc*hc - u.hi) + hc*tc) + hc*tc) + tc*tc);

		c.lo = 0.5*(((x.hi - u.hi) - u.lo) + x.lo)/c.hi;

		/* if necessary, round c.lo so that the sum of c.hi and c.lo
		   has at most 107 significant bits
		*/

		/* Compute a quarter ulp of c.hi */

		DBL2LL(c.hi, ix);
		ix >>= DMANTWIDTH;
		ix <<= DMANTWIDTH;
		LL2DBL(ix, w); /* w = dtwofloor(c.hi) */

		/* Note that the size of an ulp changes at a
		 * power of two.
		 */

		if ( (c.hi == w) && (c.lo < 0.0) )
			w *= 0.5;

		/* round c.lo if it's less than 1/4 ulp of w */

		quarterulp = twopm54.d*fabs(w);

		if ( fabs(c.lo) < quarterulp )
		{
			if ( c.lo >= 0.0 )
			{
				c.lo = (quarterulp + c.lo) - quarterulp;
			}
			else
			{
				c.lo = quarterulp + (c.lo - quarterulp);
			}

		}

		z.hi = c.hi + c.lo;
		z.lo = (c.hi - z.hi) + c.lo;

		return ( z );
	}

	if ( xpt < 0x7ff )
	{
		if ( x.hi == 0.0 )
		{
			z.lo = 0.0;
			z.hi = x.hi;

			return ( z );
		}

		if ( xpt <= 0x06d )
		{
			x.hi *= twop108.d;
			x.lo *= twop108.d;
			xfactor = twopm54.d;
		}
		else
		{
			x.hi *= twopm6.d;
			x.lo *= twopm6.d;
			xfactor = twop3.d;
		}

		c.hi = sqrt(x.hi);

		/*u.ld = _prodl(c.hi, c.hi);*/
	
		p = c.hi*const1.d;
	
		hc = (c.hi - p) + p;
		tc = c.hi - hc;
	
		u.hi = c.hi*c.hi;
		u.lo = ((((hc*hc - u.hi) + hc*tc) + hc*tc) + tc*tc);

		c.lo = 0.5*(((x.hi - u.hi) - u.lo) + x.lo)/c.hi;

		/* if necessary, round c.lo so that the sum of c.hi and c.lo
		   has at most 107 significant bits
		*/

		/* Compute a quarter ulp of c.hi */

		DBL2LL(c.hi, ix);
		ix >>= DMANTWIDTH;
		ix <<= DMANTWIDTH;
		LL2DBL(ix, w); /* w = dtwofloor(c.hi) */

		/* Note that the size of an ulp changes at a
		 * power of two.
		 */

		if ( (c.hi == w) && (c.lo < 0.0) )
			w *= 0.5;

		/* round c.lo if it's less than 1/4 ulp of w */

		quarterulp = twopm54.d*fabs(w);

		if ( fabs(c.lo) < quarterulp )
		{
			if ( c.lo >= 0.0 )
			{
				c.lo = (quarterulp + c.lo) - quarterulp;
			}
			else
			{
				c.lo = quarterulp + (c.lo - quarterulp);
			}

		}

		c.hi *= xfactor;
		c.lo *= xfactor;

		z.hi = c.hi + c.lo;
		z.lo = (c.hi - z.hi) + c.lo;

		return ( z );
	}

	z.lo = 0.0;
	z.hi = sqrt(x.hi);

	return ( z );
}

