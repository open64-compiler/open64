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
 * Module: tanh.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:23-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.tanh.c $
 *
 * Revision history:
 *  09-Jun-93 - Original Version
 *
 * Description:	source code for tanh function
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.tanh.c $ $Revision: 1.5 $";

#ifdef _CALL_MATHERR
#include <stdio.h>
#include <math.h>
#include <errno.h>
#endif

#include "libm.h"

#if defined(mips) && !defined(__GNUC__)
extern	double	tanh(double);

#pragma weak tanh = __tanh
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern double __tanh(double);
#pragma weak tanh
double tanh( double x ) {
  return __tanh( x );
}
#elif defined(__GNUC__)
extern  double  __tanh(double);

double    tanh() __attribute__ ((weak, alias ("__tanh")));

#endif

extern	const du	_exptabhi[];
extern	const du	_exptablo[];

static const du	Qnan =
{D(QNANHI, QNANLO)};

static const du	rln2by32 =
{D(0x40471547, 0x652b82fe)};

static const du	ln2by32hi =
{D(0x3f962e42, 0xfef00000)};

static const du	ln2by32lo =
{D(0x3d8473de, 0x6af278ed)};

static const du	one =
{D(0x3ff00000, 0x00000000)};

static const du	m_one =
{D(0xbff00000, 0x00000000)};

/* coefficients for polynomial approximation of exp on +/- log(2)/64     */

static const du	P[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0x3ff00000, 0x00000000)},
{D(0x3fe00000, 0x00000000)},
{D(0x3fc55555, 0x55548f7c)},
{D(0x3fa55555, 0x55545d4e)},
{D(0x3f811115, 0xb7aa905e)},
{D(0x3f56c172, 0x8d739765)},
};

/* coefficients for rational approximation of tanh on +/- 0.5     */

static const du	a[] =
{
{D(0xc099355d, 0x63ff6958)},
{D(0xc058ce3d, 0x86e94fd8)},
{D(0xbfeedc25, 0xfda04bc9)},
};

static const du	b[] =
{
{D(0x40b2e806, 0x0aff8f02)},
{D(0x40a1735a, 0x9a3f1e85)},
{D(0x405c2f6d, 0x1c260d6d)},
};

static const du	bigarg =
{D(0x40330fc1, 0x931f09c9)};

static const du	m_bigarg =
{D(0xc0330fc1, 0x931f09c9)};


/* ====================================================================
 *
 * FunctionName		tanh
 *
 * Description		computes hyperbolic tangent of arg
 *
 * ====================================================================
 */

double
__tanh( x )
double	x;
{
#ifdef _32BIT_MACHINE

int	ix, xpt;
int	l;

#else

long long ix, xpt;
long long l;

#endif

int	n, m, j;
double	absx, xsq;
double	y1, y2, y;
double	p, q;
double	nd;
double	s, s_lead, s_trail;
double	result;
double	twopm;
double	num, denom;
#ifdef _CALL_MATHERR
struct exception	exstruct;
#endif

	/* extract exponent of x for some quick screening */

#ifdef _32BIT_MACHINE

	DBLHI2INT(x, ix);	/* copy MSW of x to ix	*/
#else
	DBL2LL(x, ix);		/* copy x to ix	*/
#endif
	xpt = (ix >> DMANTWIDTH);
	xpt &= 0x7ff;

	if ( xpt < 0x403 )
	{
		/* |x| < 16.0	*/

		if ( xpt >= 0x3fe )
		{
			/* |x| >= 0.5; compute tanh(x) as 1.0 - 2.0/(exp(2*x) + 1) */
L:
			absx = fabs(x);
			absx += absx;

			/* reduce |x| to +/- log(2)/64     */

			nd = absx*rln2by32.d;
			n = ROUND(nd);
			nd = n;

			y1 = absx - nd*ln2by32hi.d;
			y2 = nd*ln2by32lo.d;
			y = y1 - y2;

			j = n & 0x1f;
			m = n >> 5;

			s_lead = _exptabhi[j].d;
			s_trail = _exptablo[j].d;
			s = s_lead + s_trail;

			q = ((((P[6].d*y + P[5].d)*y + P[4].d)*y + P[3].d)*y +
				P[2].d)*(y*y);

			p = (q - y2) + y1;

			l = m + DEXPBIAS;
			l <<= DMANTWIDTH;

#ifdef _32BIT_MACHINE

			twopm = 0.0;
			INT2DBLHI(l, twopm);
#else
			LL2DBL(l, twopm);
#endif
			result = s_lead + (s_trail + s*p);
			result *= twopm;
		
			result = 1.0 - 2.0/(result + 1.0);

			if ( x < 0.0 )
				result = -result;

			return ( result );
		}

		if ( xpt >= 0x3e3 )
		{
			/* |x| >= 2^(-28)	*/

			xsq = x*x;

			num = (a[2].d*xsq + a[1].d)*xsq + a[0].d;
			denom = ((xsq + b[2].d)*xsq + b[1].d)*xsq + b[0].d;

			return ( x + x*((xsq*num)/denom) );
		}

		return ( x );
	}

	if ( x != x )
	{
		/* x is a NaN; return a quiet NaN */

#ifdef _CALL_MATHERR

		exstruct.type = DOMAIN;
		exstruct.name = "tanh";
		exstruct.arg1 = x;
		exstruct.retval = Qnan.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "domain error in tanh\n");
			SETERRNO(EDOM);
		}

		return ( exstruct.retval );
#else
		NAN_SETERRNO(EDOM);

		return ( Qnan.d );
#endif
	}

	if ( x > bigarg.d )
		return ( one.d );

	if ( x < m_bigarg.d )
		return ( m_one.d );

	goto L;

}

#ifdef NO_LONG_DOUBLE

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern long double __tanhl(long double);
long double tanhl( long double x ) {	
  return ( (long double)__tanh((double)x) );
}
#elif defined(__GNUC__)
extern  long double  __tanhl(long double);

long double    tanhl() __attribute__ ((weak, alias ("__tanhl")));

#endif

long double
__tanhl( long double x )
{	
	return ( (long double)__tanh((double)x) );
}

#endif

