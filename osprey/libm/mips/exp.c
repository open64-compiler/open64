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
 * Module: exp.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:21-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.exp.c $
 *
 * Revision history:
 *  09-Jun-93 - Original Version
 *
 * Description:	source code for exp function
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.exp.c $ $Revision: 1.5 $";

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
extern	double	exp(double);

#pragma weak exp = __exp
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern double __exp(double);
#pragma weak exp
double exp( double x ) {
  return __exp( x );
}
#elif defined(__GNUC__)
extern  double  __exp(double);

double    exp() __attribute__ ((weak, alias ("__exp")));

#endif

extern	const du	_exptabhi[];
extern	const du	_exptablo[];

static const	du	Qnan =
{D(QNANHI, QNANLO)};

static const	du	Inf =
{D(0x7ff00000, 0x00000000)};

static const du	Ulimit =
{D(0x40862e42, 0xfefa39ef)};

static const du	Llimit =
{D(0xc0874910, 0xd52d3051)};

static const du	rln2by32 =
{D(0x40471547, 0x652b82fe)};

static const du	ln2by32hi =
{D(0x3f962e42, 0xfef00000)};

static const du	ln2by32lo =
{D(0x3d8473de, 0x6af278ed)};

static const du	one =
{D(0x3ff00000, 0x00000000)};

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


/* ====================================================================
 *
 * FunctionName		exp
 *
 * Description		computes exponential of arg
 *
 * ====================================================================
 */

double
__exp( double x )
{
#ifdef _32BIT_MACHINE

int	ix, xpt;
int	l;

#else

long long ix, xpt;
long long l;

#endif

double	y1, y2, y;
double	p, q;
int	n, m, j;
int	m1, m2;
double	nd;
double	s, s_lead, s_trail;
double	twopm;
double	twopm1, twopm2;
double	result;
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

	if ( xpt < 0x408 )
	{
		/*      |x| < 512.0	*/

		if ( xpt >= 0x3c8 )
		{
			/* |x| >= 2^(-54) */

			/* reduce x to +/- log(2)/64     */

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
			s = s_lead + s_trail;

			l = m + DEXPBIAS;
			l <<= DMANTWIDTH;

#ifdef _32BIT_MACHINE

			twopm = 0.0;
			INT2DBLHI(l, twopm);	/* copy MSW of l to twopm	*/
#else
			LL2DBL(l, twopm);	/* copy l to twopm	*/
#endif
			q = ((((P[6].d*y + P[5].d)*y + P[4].d)*y + P[3].d)*y +
				P[2].d)*(y*y);

			p = (q - y2) + y1;

			result = s_lead + (s_trail + s*p);
			result *= twopm;
			return ( result );
		}

		return ( one.d + x );
	}

	if ( x != x )
	{
		/* x is a NaN; return a quiet NaN */

#ifdef _CALL_MATHERR

		exstruct.type = DOMAIN;
		exstruct.name = "exp";
		exstruct.arg1 = x;
		exstruct.retval = Qnan.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "domain error in exp\n");
			SETERRNO(EDOM);
		}

		return ( exstruct.retval );
#else
		NAN_SETERRNO(EDOM);

		return ( Qnan.d );
#endif
	}

	if ( x > Ulimit.d )
	{
#ifdef _CALL_MATHERR

		exstruct.type = OVERFLOW;
		exstruct.name = "exp";
		exstruct.arg1 = x;
		exstruct.retval = Inf.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "overflow error in exp\n");
			SETERRNO(ERANGE);
		}

		return ( exstruct.retval );
#else
		SETERRNO(ERANGE);

		return ( Inf.d );
#endif
	}

	if ( x < Llimit.d )
	{
#ifdef _CALL_MATHERR

		exstruct.type = UNDERFLOW;
		exstruct.name = "exp";
		exstruct.arg1 = x;
		exstruct.retval = 0.0;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "underflow error in exp\n");
			SETERRNO(ERANGE);
		}

		return ( exstruct.retval );
#else
		SETERRNO(ERANGE);

		return ( 0.0 );
#endif
	}

	nd = x*rln2by32.d;
	n = ROUND(nd);
	nd = n;

	/* reduce x to +/- log(2)/64     */

	y1 = x - nd*ln2by32hi.d;
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

	result = s_lead + (s_trail + s*p);

	/* must be more careful here when forming 2^m*result;
	   the exponent may underflow or overflow
	*/

	m1 = (m >> 1);
	m2 = m - m1;

#ifdef _32BIT_MACHINE

	twopm1 = 0.0;
	l = m1 + DEXPBIAS;
	l <<= DMANTWIDTH;

	INT2DBLHI(l, twopm1);

	twopm2 = 0.0;
	l = m2 + DEXPBIAS;
	l <<= DMANTWIDTH;

	INT2DBLHI(l, twopm2);
#else
	l = m1 + DEXPBIAS;
	l <<= DMANTWIDTH;

	LL2DBL(l, twopm1);

	l = m2 + DEXPBIAS;
	l <<= DMANTWIDTH;

	LL2DBL(l, twopm2);
#endif
	result *= twopm1;
	result *= twopm2;

	return ( result );
}

#ifdef NO_LONG_DOUBLE

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern  long double  __expl(long double);
long double expl( long double x ) {	
  return ( (long double)__exp((double)x) );
}
#elif defined(__GNUC__)
extern  long double  __expl(long double);

long double    expl() __attribute__ ((weak, alias ("__expl")));

#endif

long double
__expl( long double x )
{	
	return ( (long double)__exp((double)x) );
}

#endif

