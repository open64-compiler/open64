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
 * Module: log10.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:22-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.log10.c $
 *
 * Revision history:
 *  09-Jun-93 - Original Version
 *  04-Sep-97 - Improved approximation by combining some constants
 *
 * Description:	source code for log10 function
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.log10.c $ $Revision: 1.5 $";

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
extern	double	log10(double);

#pragma weak log10 = __log10
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern double __log10(double);
#pragma weak log10
double log10( double x ) {
  return __log10( x );
}
#elif defined(__GNUC__)
extern  double  __log10(double);

double    log10() __attribute__ ((weak, alias ("__log10")));

#endif

extern	const du	_logtabhi[];
extern	const du	_logtablo[];
extern	const du	_log_ru[];

#ifdef _32BIT_MACHINE

/* exp(-1/16) */

static const int	lim1 =
{0x3fee0fab};

/* exp(1/16) */

static const int	lim2 =
{0x3ff1082b};

static const int	twop7 =
{0x40600000};

#else

/* exp(-1/16) */

static const long long	lim1 =
{0x3fee0fabfbc702a3ll};

/* exp(1/16) */

static const long long	lim2 =
{0x3ff1082b577d34eell};

static const long long	twop7 =
{0x4060000000000000ll};

#endif

static const	du	Qnan =
{D(QNANHI, QNANLO)};

static const	du	Neginf =
{D(0xfff00000, 0x00000000)};

static const	du	Inf =
{D(0x7ff00000, 0x00000000)};

static	const du	twopm7 =
{D(0x3f800000, 0x00000000)};

/* log10(2.0) high */
static const du	log10_2_lead =
{D(0x3fd34413, 0x509f6000)};

/* log10(2.0) low */
static const du	log10_2_trail =
{D(0x3d59fef3, 0x11f12b36)};

static const du	Scaleup =
{D(0x43300000, 0x00000000)};

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

static const du	Loge =
{D(0x3fdbcb7b, 0x1526e50e)};


/* ====================================================================
 *
 * FunctionName		log10
 *
 * Description		computes common logarithm of arg
 *
 * ====================================================================
 */

double
__log10( double x )
{
#ifdef _32BIT_MACHINE

int	ix, m;

#else

long long ix, m;

#endif

int	k;
double	g, u, v, x1, x2;
double	u1, u2;
double	t;
double	xmu;
double	q;
double	result;
double	l_lead, l_trail;
#ifdef _CALL_MATHERR
struct exception	exstruct;
#endif

	/* extract exponent and sign of x for some quick screening */

#ifdef _32BIT_MACHINE

	DBLHI2INT(x, ix);	/* copy MSW of x to ix */
#else
	DBL2LL(x, ix);		/* copy x to ix */
#endif
	m = (ix >> DMANTWIDTH);

	if ( (0 < m) && (m < 0x7ff) )
	{
		/* x is positive and finite */

		m -= DEXPBIAS;
L:
		if ( (ix >= lim2) || (ix <= lim1) )
		{
			/* normalize x and compute the nearest 1/128th to x */

			ix &= DEXPMASK;	/* mask off exponent bits of x */
			ix |= twop7;	/* set exponent of x to 0x406 */

#ifdef _32BIT_MACHINE

			INT2DBLHI(ix, x);
#else
			LL2DBL(ix, x);
#endif
			k = ROUND(x);

			u = k;

			k -= 128;

			xmu = twopm7.d*(x - u);

			t = _log_ru[k].d*xmu;

			/* avoid loss of significance for values of x near two
			   by adjusting index; effectively u is divided by two.
			   The logtable has been adjusted for this.
			*/

			if ( k > 64 )
				m++;

			q = ((((P[5].d*t + P[4].d)*t + P[3].d)*t + 
					P[2].d)*t + P[1].d)*(t*t);

			l_lead = Loge.d*_logtabhi[k].d;
			l_trail = Loge.d*_logtablo[k].d;

			l_lead += m*log10_2_lead.d;
			l_trail += m*log10_2_trail.d;

			result = l_lead + (Loge.d*t + (Loge.d*q + l_trail));

			return ( result );
		}

		if ( x == 1.0 )
			return ( 0.0 );

		x = x - 1.0;

		g = 1.0/(2.0 + x);

		u = x*g;
		u = u + u;
		v = u*u;

		q = (((Q[4].d*v + Q[3].d)*v + Q[2].d)*v +
			Q[1].d)*(v*u);

		u1 = (float)u;	/* round u to 24 bits */
		x1 = (float)x;	/* round x to 24 bits */

		x2 = x - x1;
		u2 = x - u1;
		u2 = u2 + u2;
		u2 = ((u2 - u1*x1) - u1*x2)*g;

		result = u1 + (u2 + q);

		return ( Loge.d*result );
	}

	if ( x != x )
	{
		/* x is a NaN; return a quiet NaN */

#ifdef _CALL_MATHERR

		exstruct.type = DOMAIN;
		exstruct.name = "log10";
		exstruct.arg1 = x;
		exstruct.retval = Qnan.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "domain error in log10\n");
			SETERRNO(EDOM);
		}

		return ( exstruct.retval );
#else
		NAN_SETERRNO(EDOM);

		return ( Qnan.d );
#endif
	}

	if ( x == Neginf.d )
	{
#ifdef _CALL_MATHERR

		exstruct.type = DOMAIN;
		exstruct.name = "log10";
		exstruct.arg1 = x;
		exstruct.retval = Qnan.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "domain error in log10\n");
			SETERRNO(EDOM);
		}

		return ( exstruct.retval );
#else
		SETERRNO(EDOM);

		return ( Qnan.d );
#endif
	}

	if ( x == 0.0 )
	{
zeroarg:

#ifdef _CALL_MATHERR

		exstruct.type = OVERFLOW;
		exstruct.name = "log10";
		exstruct.arg1 = x;
		exstruct.retval = Neginf.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "overflow range error in log10\n");
			SETERRNO(ERANGE);
		}

		return ( exstruct.retval );
#else
		SETERRNO(ERANGE);

		return ( Neginf.d );
#endif
	}

	if ( x == Inf.d )
	{
#ifdef _CALL_MATHERR

		exstruct.type = DOMAIN;
		exstruct.name = "log10";
		exstruct.arg1 = x;
		exstruct.retval = Inf.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "domain error in log10\n");
			SETERRNO(EDOM);
		}

		return ( exstruct.retval );
#else
		SETERRNO(EDOM);

		return ( Inf.d );
#endif
	}

	if ( x < 0.0 )
	{
#ifdef _CALL_MATHERR

		exstruct.type = DOMAIN;
		exstruct.name = "log10";
		exstruct.arg1 = x;
		exstruct.retval = Qnan.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "domain error in log10\n");
			SETERRNO(EDOM);
		}

		return ( exstruct.retval );
#else
		SETERRNO(EDOM);

		return ( Qnan.d );
#endif
	}

	/* take care of denormals by scaling them up and adjusting the unbiased
	   exponent of x
	*/

	x *= Scaleup.d;

	if ( x == 0.0 )
		goto zeroarg;

#ifdef _32BIT_MACHINE

	DBLHI2INT(x, ix);	/* copy MSW of x to ix	*/
#else
	DBL2LL(x, ix);		/* copy x to ix	*/
#endif
	m = (ix >> DMANTWIDTH);	/* shift off mantissa	*/
	m -= DEXPBIAS;
	m -= 52;	/* adjust for scaling	*/
	goto L;
}

#ifdef NO_LONG_DOUBLE

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern long double __log10l(long double);
long double log10l( long double x ) {	
  return ( (long double)__log10((double)x) );
}
#elif defined(__GNUC__)
extern  long double  __log10l(long double);

long double    log10l() __attribute__ ((weak, alias ("__log10l")));

#endif

long double
__log10l( long double x )
{	
	return ( (long double)__log10((double)x) );
}

#endif

