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
 * Module: expm1.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:21-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.expm1.c $
 *
 * Revision history:
 *  09-Jun-93 - Original Version
 *
 * Description:	source code for expm1 function
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.expm1.c $ $Revision: 1.5 $";

#ifdef _CALL_MATHERR
#include <stdio.h>
#include <math.h>
#include <errno.h>
#endif

#include "libm.h"

/*	Algorithm adapted from
	"Table-driven Implementation of the Expm1 Function in IEEE
	Floating Point Arithmetic", Peter Tang, ACM Transactions on
	Mathematical Software, Vol. 18, No. 2, June 1992
 */

#if defined(mips) && !defined(__GNUC__)
extern	double	expm1(double);

#pragma weak expm1 = __expm1
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern double __expm1(double);
#pragma weak expm1
double expm1( double x ) {
  return __expm1( x );
}
#elif defined(__GNUC__)
extern  double  __expm1(double);

double    expm1() __attribute__ ((weak, alias ("__expm1")));

#endif

extern	const du	_exptabhi[];
extern	const du	_exptablo[];

static const	du	Qnan =
{D(QNANHI, QNANLO)};

static const	du	Inf =
{D(0x7ff00000, 0x00000000)};

/* log(1 - 1/4)   */

static const du	T1 =
{D(0xbfd26962, 0x1134db93)};

/* log(1 + 1/4)   */

static const du	T2 =
{D(0x3fcc8ff7, 0xc79a9a22)};

static const du	Twopm100 =
{D(0x39b00000, 0x00000000)};

static const du	Twopm7 =
{D(0x3f800000, 0x00000000)};

static const du	Ulimit =
{D(0x40862e42, 0xfefa39ef)};

static const du	Llimit =
{D(0xc042b708, 0x872320e1)};

static const du	rln2by32 =
{D(0x40471547, 0x652b82fe)};

static const du	ln2by32hi =
{D(0x3f962e42, 0xfef00000)};

static const du	ln2by32lo =
{D(0x3d8473de, 0x6af278ed)};

static const du	m_one =
{D(0xbff00000, 0x00000000)};

/* coefficients for polynomial approximation of exp(x) - 1 on the interval
   (log(1 - 1/4), log(1 + 1/4))
*/

static const du	P[] =
{
{D(0x3fc55555, 0x55555549)},
{D(0x3fa55555, 0x555554b6)},
{D(0x3f811111, 0x1111a9f3)},
{D(0x3f56c16c, 0x16ce14c6)},
{D(0x3f2a01a0, 0x1159dd2d)},
{D(0x3efa019f, 0x635825c4)},
{D(0x3ec71e14, 0xbfe3db59)},
{D(0x3e928295, 0x484734ea)},
{D(0x3e5a2836, 0xaa646b96)},
};

/* coefficients for polynomial approximation of exp on +/- log(2)/64     */

static const du	Q[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0x3ff00000, 0x00000000)},
{D(0x3fe00000, 0x00000000)},
{D(0x3fc55555, 0x55548f7c)},
{D(0x3fa55555, 0x55545d4e)},
{D(0x3f811115, 0xb7aa905e)},
{D(0x3f56c172, 0x8d739765)},
};

/* 2^(k-7), k = 0, ..., 60    */

static	const fu	twopowm[] =
{
{0x43000000},
{0x42800000},
{0x42000000},
{0x41800000},
{0x41000000},
{0x40800000},
{0x40000000},
{0x3f800000},
{0x3f000000},
{0x3e800000},
{0x3e000000},
{0x3d800000},
{0x3d000000},
{0x3c800000},
{0x3c000000},
{0x3b800000},
{0x3b000000},
{0x3a800000},
{0x3a000000},
{0x39800000},
{0x39000000},
{0x38800000},
{0x38000000},
{0x37800000},
{0x37000000},
{0x36800000},
{0x36000000},
{0x35800000},
{0x35000000},
{0x34800000},
{0x34000000},
{0x33800000},
{0x33000000},
{0x32800000},
{0x32000000},
{0x31800000},
{0x31000000},
{0x30800000},
{0x30000000},
{0x2f800000},
{0x2f000000},
{0x2e800000},
{0x2e000000},
{0x2d800000},
{0x2d000000},
{0x2c800000},
{0x2c000000},
{0x2b800000},
{0x2b000000},
{0x2a800000},
{0x2a000000},
{0x29800000},
{0x29000000},
{0x28800000},
{0x28000000},
{0x27800000},
{0x27000000},
{0x26800000},
{0x26000000},
{0x25800000},
};


/* ====================================================================
 *
 * FunctionName		expm1
 *
 * Description		computes expm1 function of arg
 *
 * ====================================================================
 */

double
__expm1( double x )
{
#ifdef _32BIT_MACHINE

int	ix, xpt;
int	k, l;

#else

long long ix, xpt;
long long k, l;

#endif

int	n, m, j;
double	y1, y2, y;
double	u, v, z;
double	p, q;
double	twopm;
double	twopmm;
double	nd;
double	s, s_lead, s_trail;
double	result;
#ifdef _CALL_MATHERR
struct exception	exstruct;
#endif

	/* extract exponent of x for some quick screening */

#ifdef _32BIT_MACHINE

	DBLHI2INT(x, ix);	/* copy MSW of arg to an int	*/
#else
	DBL2LL(x, ix);		/* copy arg to a long long	*/
#endif
	xpt = (ix >> DMANTWIDTH);	/* shift off mantissa	*/
	xpt &= 0x7ff;

	if ( xpt < 0x7ff )
	{
		/*    x is finite    */

		if ( (T1.d < x) && (x < T2.d) )
		{
			/* log(1 - 1/4) < x < log(1 + 1/4) */

			if ( xpt >= 0x3c9 )
			{
				/* |x| >= 2^(-54) */

				u = (float)x;
				v = x - u;
				y = u*u*0.5;
				z = v*(x + u)*0.5;

				q = ((((((((P[8].d*x + P[7].d)*x + P[6].d)*x + P[5].d)*x +
				P[4].d)*x + P[3].d)*x + P[2].d)*x + P[1].d)*x +
				P[0].d)*(x*x*x);

				if ( y >= Twopm7.d )
				{
					result = (u + y) + (q + (v + z));
					return ( result );
				}

				result = x + (y + (q + z));

				return ( result );

			}

			return ( x );
		}
		else if ( x > Ulimit.d )
		{
#ifdef _CALL_MATHERR
			exstruct.type = OVERFLOW;
			exstruct.name = "expm1";
			exstruct.arg1 = x;
			exstruct.retval = Inf.d;

			if ( matherr( &exstruct ) == 0 )
			{
				fprintf(stderr, "overflow range error in expm1\n");
				SETERRNO(ERANGE);
			}

			return ( exstruct.retval );
#else
			SETERRNO(ERANGE);

			return ( Inf.d );
#endif
		}
		else if ( x < Llimit.d )
		{
			return ( m_one.d + Twopm100.d );
		}
		else
		{
			nd = x*rln2by32.d;
			n = ROUND(nd);
			nd = n;

			y1 = x - nd*ln2by32hi.d;
			y2 = nd*ln2by32lo.d;
			y = y1 - y2;

			j = n & 0x1f;
			m = n >> 5;

			s_lead = _exptabhi[j].d;
			s_trail = _exptablo[j].d;

			q = ((((Q[6].d*y + Q[5].d)*y + Q[4].d)*y + Q[3].d)*y +
				Q[2].d)*(y*y);

			p = (q - y2) + y1;

			k = DEXPBIAS + m;
			k <<= DMANTWIDTH;

#ifdef _32BIT_MACHINE

			twopm = 0.0;
			INT2DBLHI(k, twopm);	/* copy k to MSW of twopm	*/
#else
			LL2DBL(k, twopm);	/* copy k to twopm	*/
#endif
			if ( m >= 53 )
			{
				l = DEXPBIAS - m;
				l <<= DMANTWIDTH;

#ifdef _32BIT_MACHINE

				twopmm = 0.0;
				INT2DBLHI(l, twopmm);	/* copy l to MSW of twopmm */
#else
				LL2DBL(l, twopmm);	/* copy l to twopmm */
#endif
				s = s_lead + s_trail;
				result = s_lead + (s*p + (s_trail - twopmm));
				result *= twopm;
				return ( result );
			}
			else if ( m >= -7 )
			{
				result = (s_lead - twopowm[m+7].f) +
					 (s_lead*p + s_trail*(1.0 + p));
				result *= twopm;
				return ( result );
			}
			else
			{
				s = s_lead + s_trail;
				result = s_lead + (s*p + s_trail);
				result *= twopm;
				result -= 1.0;
				return ( result );
			}
		}
	}
	else if ( x != x )
	{
		/* x is a NaN; return a quiet NaN */

#ifdef _CALL_MATHERR

		exstruct.type = DOMAIN;
		exstruct.name = "expm1";
		exstruct.arg1 = x;
		exstruct.retval = Qnan.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "domain error in expm1\n");
			SETERRNO(EDOM);
		}

		return ( exstruct.retval );
#else
		NAN_SETERRNO(EDOM);

		return ( Qnan.d );
#endif
	}
	else if ( x == Inf.d )
	{
#ifdef _CALL_MATHERR

		exstruct.type = OVERFLOW;
		exstruct.name = "expm1";
		exstruct.arg1 = x;
		exstruct.retval = Inf.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "overflow range error in expm1\n");
			SETERRNO(ERANGE);
		}

		return ( exstruct.retval );
#else
		SETERRNO(ERANGE);

		return ( Inf.d );
#endif
	}
	else	/*  x == -inf  */

		return ( m_one.d );
}

#ifdef NO_LONG_DOUBLE

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern long double __expm1l(long double);
long double expm1l( long double x ) {	
  return ( (long double)__expm1((double)x) );
}
#elif defined(__GNUC__)
extern  long double  __expm1l(long double);

long double    expm1l() __attribute__ ((weak, alias ("__expm1l")));

#endif

long double
__expm1l( long double x )
{	
	return ( (long double)__expm1((double)x) );
}

#endif

