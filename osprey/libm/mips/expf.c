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
 * Module: expf.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:21-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.expf.c $
 *
 * Revision history:
 *  09-Jun-93 - Original Version
 *
 * Description:	source code for expf function
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.expf.c $ $Revision: 1.5 $";

#ifdef _CALL_MATHERR
#include <stdio.h>
#include <math.h>
#include <errno.h>
#endif

#include "libm.h"

/*	Algorithm adapted from
	"Table-driven Implementation of the Exponential Function in
	IEEE Floating Point Arithmetic", Peter Tang, ACM Transactions on
	Mathematical Software, Vol. 15, No. 2, June 1989
 */

#if defined(mips) && !defined(__GNUC__)
extern	float	fexp(float);
extern	float	expf(float);

#pragma weak fexp = __expf
#pragma weak expf = __expf
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern float __expf(float);
#pragma weak expf
float expf( float x ) {
  return __expf( x );
}
#elif defined(__GNUC__)
extern  float  __expf(float);
float    expf(float) __attribute__ ((weak, alias ("__expf")));
#endif

extern	const du	_expftab[];

/* coefficients for polynomial approximation of exp on +/- log(2)/64    */

static const du	P[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0x3ff00000, 0x00000000)},
{D(0x3fe00008, 0x745da559)},
{D(0x3fc55569, 0x9fd0029e)},
};

static const du	rln2by32 =
{D(0x40471547, 0x652b82fe)};

static const du	ln2by32hi =
{D(0x3f962e42, 0xfef00000)};

static const du	ln2by32lo =
{D(0x3d8473de, 0x6af278ed)};

static const	fu	Qnan = {QNANF};

static const fu Inf = {0x7f800000};

static const fu	f_one = {0x3f800000};

static const fu	Ulimit = {0x42b17217};

static const fu	Llimit = {0xc2cff1b4};


/* ====================================================================
 *
 * FunctionName		expf
 *
 * Description		computes exponential function of arg
 *
 * ====================================================================
 */

float
__expf( float x )
{
#ifdef _32BIT_MACHINE

int	l;

#else

long long l;

#endif

int	ix, xp, j, m, n;
double	twopm;
double	result, nd, y;
double	s;
double	dx;
double	poly;
#ifdef _CALL_MATHERR
struct exception	exstruct;
#endif

	/* extract exponent of x for some quick screening */

	FLT2INT(x, ix);		/* copy arg to an integer	*/
	xp = (ix >> MANTWIDTH);
	xp &= 0xff;

	if ( xp < 0x85 )
	{
		/* |x| < 64.0 */

		if ( xp >= 0x66 )
		{
			/* |x| >= 2^-25 */
L:
			dx = x;

			/* reduce x to +/- log(2)/64    */

			nd = dx*rln2by32.d;
			n = ROUND(nd);
			nd = n;

			y = x - nd*ln2by32hi.d - nd*ln2by32lo.d;

			j = n & 0x1f;
			m = n >> 5;

			s = _expftab[j].d;

			poly = (P[3].d*y + P[2].d)*(y*y) + y;

			l = m + DEXPBIAS;
			l <<= DMANTWIDTH;

#ifdef _32BIT_MACHINE

			twopm = 0.0;
			INT2DBLHI(l, twopm);
#else
			LL2DBL(l, twopm);
#endif
			result = s + s*poly;
			result *= twopm;

			return ( (float)result );
		}

		return ( f_one.f + x );
	}

	if ( x != x )
	{
		/* x is a NaN; return a quiet NaN */

#ifdef _CALL_MATHERR

                exstruct.type = DOMAIN;
                exstruct.name = "expf";
                exstruct.arg1 = x;
                exstruct.retval = Qnan.f;

                if ( matherr( &exstruct ) == 0 )
                {
                        fprintf(stderr, "domain error in expf\n");
                        SETERRNO(EDOM);
                }

                return ( exstruct.retval );
#else
		NAN_SETERRNO(EDOM);
		
		return ( Qnan.f );
#endif
	}

	if ( x > Ulimit.f )
	{
#ifdef _CALL_MATHERR

                exstruct.type = OVERFLOW;
                exstruct.name = "expf";
                exstruct.arg1 = x;
                exstruct.retval = Inf.f;

                if ( matherr( &exstruct ) == 0 )
                {
                        fprintf(stderr, "overflow error in expf\n");
                        SETERRNO(ERANGE);
                }

                return ( exstruct.retval );
#else
		SETERRNO(ERANGE);

		return ( Inf.f );
#endif
	}

	if ( x < Llimit.f )
	{
#ifdef _CALL_MATHERR

                exstruct.type = UNDERFLOW;
                exstruct.name = "expf";
                exstruct.arg1 = x;
                exstruct.retval = 0.0;

                if ( matherr( &exstruct ) == 0 )
                {
                        fprintf(stderr, "underflow error in expf\n");
                        SETERRNO(ERANGE);
                }

                return ( exstruct.retval );
#else
		SETERRNO(ERANGE);

		return ( 0.0f );
#endif
	}

	goto L;
}

