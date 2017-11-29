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
 * Module: log1p.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:22-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.log1p.c $
 *
 * Revision history:
 *  09-Jun-93 - Original Version
 *
 * Description:	source code for log1p function
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.log1p.c $ $Revision: 1.5 $";

#ifdef _CALL_MATHERR
#include <stdio.h>
#include <math.h>
#include <errno.h>
#endif

#include "libm.h"

/*	Algorithm adapted from
	"Table-Driven Implementation of the Logarithm Function in IEEE
	Floating-Point Arithmetic", Peter Tang, Argonne National Laboratory,
	ACM Transactions on Mathematical Software, Vol. 16, No. 4,
	Dec. 1990
*/

#if defined(mips) && !defined(__GNUC__)
extern	double	log1p(double);

#pragma weak log1p = __log1p
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern double __log1p(double);
#pragma weak log1p
double log1p( double x ) {
  return __log1p( x );
}
#elif defined(__GNUC__)
extern  double  __log1p(double);

double    log1p() __attribute__ ((weak, alias ("__log1p")));

#endif

extern	const du	_logtabhi[];
extern	const du	_logtablo[];
extern	const du	_log_ru[];

static const du	Qnan =
{D(QNANHI, QNANLO)};

static const du Neginf =
{D(0xfff00000, 0x00000000)};

static const du Inf =
{D(0x7ff00000, 0x00000000)};

static const du	m_one =
{D(0xbff00000, 0x00000000)};

static const du	twop7 =
{D(0x40600000, 0x00000000)};

static const du	twopm7 =
{D(0x3f800000, 0x00000000)};

/* exp(-1/16) - 1   */

static const du	T1 =
{D(0xbfaf0540, 0x438fd5c4)};

/* exp(1/16) - 1   */

static const du	T2 =
{D(0x3fb082b5, 0x77d34ed8)};

static const du	T3 =
{D(0x43500000, 0x00000000)};

static const du	log2_lead =
{D(0x3fe62e42, 0xfefa4000)};

static const du	log2_trail =
{D(0xbd48432a, 0x1b0e2634)};

/* coefficients for polynomial approximation of log(1 + t) on +/- 1/256   */

static const du	P[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0xbfe00000, 0x00000001)},
{D(0x3fd55555, 0x55509ba5)},
{D(0xbfcfffff, 0xffeb6526)},
{D(0x3fc999b4, 0xdfed6fe4)},
{D(0xbfc55576, 0x66472e04)},
};

/* coefficients for polynomial approximation of log((1 + x/2)/(1 - x/2)) on 
   +/- 2*tanh(1/32)
*/

static const du	Q[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0x3fb55555, 0x555554ed)},
{D(0x3f899999, 0x99b929bd)},
{D(0x3f624923, 0x14d70150)},
{D(0x3f3c7ff7, 0xdaa9e72e)},
};

#ifdef _32BIT_MACHINE

static const int	one =
{0x3ff00000};

#else

static const long long	one =
{0x3ff0000000000000ll};

#endif


/* ====================================================================
 *
 * FunctionName		log1p
 *
 * Description		computes log1p function of arg
 *
 * ====================================================================
 */

double
__log1p( double x )
{
#ifdef _32BIT_MACHINE

int	xpt;
int	k, m, n;

#else

long long xpt;
long long k, m, n;

#endif
int	j;
double	g, u, v, x1, x2;
double	u1, u2;
double	q;
double	twopnegm;
double	result;
double	y, f, F;
double	l_lead, l_trail;
#ifdef _CALL_MATHERR
struct exception	exstruct;
#endif

	if ( x != x )
	{
		/* x is a NaN; return a quiet NaN */

#ifdef _CALL_MATHERR

		exstruct.type = DOMAIN;
		exstruct.name = "log1p";
		exstruct.arg1 = x;
		exstruct.retval = Qnan.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "domain error in log1p\n");
			SETERRNO(EDOM);
		}

		return ( exstruct.retval );
#else
		NAN_SETERRNO(EDOM);

		return ( Qnan.d );
#endif
	}

	if ( (T1.d < x) && (x < T2.d) )
	{
#ifdef _32BIT_MACHINE

		DBLHI2INT(x, xpt);	/* copy MSW of arg to an int	*/
#else
		DBL2LL(x, xpt);		/* copy arg to an int	*/
#endif
		xpt >>= DMANTWIDTH;	/* shift off mantissa	*/
		xpt &= 0x7ff;

		if ( xpt >= 0x3ca )
		{
			/* |x| >= 2^(-53) */

			g = 1.0/(2.0 + x);

			u = x*g;
			u = u + u;
			v = u*u;

			q = (((Q[4].d*v + Q[3].d)*v + Q[2].d)*v +
				Q[1].d)*v*u;

			u1 = (float)u;	/* round u to 24 bits */
			x1 = (float)x;	/* round x to 24 bits */

			x2 = x - x1;
			u2 = x - u1;
			u2 = u2 + u2;
			u2 = ((u2 - u1*x1) - u1*x2)*g;

			result = u1 + (u2 + q);

			return ( result );
		}

		return ( x );
	}
	else if ( (x > m_one.d) && (x != Inf.d) )
	{
		if ( x >= T3.d )
			y = x;
		else
			y = 1.0 + x;

#ifdef _32BIT_MACHINE

		DBLHI2INT(y, k);
#else
		DBL2LL(y, k);
#endif
		m = (k >> DMANTWIDTH);
		m -= DEXPBIAS;	/* save unbiased exponent */

		k &= DEXPMASK;	/* mask off exponent */
		k |= one;	/* set exponent to 0x3ff */

#ifdef _32BIT_MACHINE

		INT2DBLHI(k, y);	/* copy k to MSW of y	*/
#else
		LL2DBL(k, y);		/* copy k to y	*/
#endif
		u = twop7.d*y;

		j = ROUND(u);

		F = j;
		j -= 128;
	
		F = twopm7.d*F;

		if ( m <= -2 )
			f = y - F;
		else
		{
			n = DEXPBIAS - m;
			n <<= DMANTWIDTH;

#ifdef _32BIT_MACHINE

			twopnegm = 0.0;
			INT2DBLHI(n, twopnegm);	/* build 2^(-m) */
#else
			LL2DBL(n, twopnegm);	/* build 2^(-m) */
#endif
			if ( m <= 52 )
				f = (twopnegm - F) + twopnegm*x;
			else if ( m <= 108 )
				f = (twopnegm*x - F) + twopnegm;
			else
				f = y - F;
		}

		/* avoid loss of significance for values of x near two by
		   adjusting index; effectively x, f, and F are divided
		   by two.  The logtable has been adjusted for this.
		*/

		if ( j > 64 )
			m++;
	
		u = _log_ru[j].d*f;
	
		q = ((((P[5].d*u + P[4].d)*u + P[3].d)*u + 
				P[2].d)*u + P[1].d)*(u*u);
	
	
		l_lead = _logtabhi[j].d;
		l_trail = _logtablo[j].d;
	
		l_lead += m*log2_lead.d;
		l_trail += m*log2_trail.d;

		result = l_lead + (u + (q + l_trail));
	
		return ( result );
	}

	if ( x == Inf.d )
	{
#ifdef _CALL_MATHERR

		exstruct.type = DOMAIN;
		exstruct.name = "log1p";
		exstruct.arg1 = x;
		exstruct.retval = Inf.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "domain error in log1p\n");
			SETERRNO(EDOM);
		}

		return ( exstruct.retval );
#else
		SETERRNO(EDOM);

		return ( Inf.d );
#endif
	}

	if ( x == m_one.d )
	{
#ifdef _CALL_MATHERR

		exstruct.type = OVERFLOW;
		exstruct.name = "log1p";
		exstruct.arg1 = x;
		exstruct.retval = Neginf.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "overflow range error in log1p\n");
			SETERRNO(ERANGE);
		}

		return ( exstruct.retval );
#else
		SETERRNO(ERANGE);

		return ( Neginf.d );
#endif
	}

	if ( x == Neginf.d )
	{
#ifdef _CALL_MATHERR

		exstruct.type = DOMAIN;
		exstruct.name = "log1p";
		exstruct.arg1 = x;
		exstruct.retval = Qnan.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "domain error in log1p\n");
			SETERRNO(EDOM);
		}

		return ( exstruct.retval );
#else
		SETERRNO(EDOM);

		return ( Qnan.d );
#endif
	}

	/*  x < -1.0 */

#ifdef _CALL_MATHERR

	exstruct.type = DOMAIN;
	exstruct.name = "log1p";
	exstruct.arg1 = x;
	exstruct.retval = Qnan.d;

	if ( matherr( &exstruct ) == 0 )
	{
		fprintf(stderr, "domain error in log1p\n");
		SETERRNO(EDOM);
	}

	return ( exstruct.retval );
#else
	SETERRNO(EDOM);

	return ( Qnan.d );
#endif
}

#ifdef NO_LONG_DOUBLE

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern long double __log1pl(long double);
long double log1pl( long double x ) {	
  return ( (long double)__log1p((double)x) );
}
#elif defined(__GNUC__)
extern  long double  __log1pl(long double);

long double    log1pl() __attribute__ ((weak, alias ("__log1pl")));

#endif

long double
__log1pl( long double x )
{	
	return ( (long double)__log1p((double)x) );
}

#endif

