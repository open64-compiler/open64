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
 * Module: erf.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:21-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.erf.c $
 *
 * Revision history:
 *  28-May-93 - Original Version
 *
 * Description:	source code for erf and erfc functions
 *
 * ====================================================================
 * ====================================================================
 */

/* $Header: /home/bos/bk/kpro64-pending/libm/mips/erf.c 1.5 04/12/21 14:58:21-08:00 bos@eng-25.internal.keyresearch.com $ */

/*
	C program for floating point error function

	erf(x) returns the error function of its argument
	erfc(x) returns 1.0 - erf(x)

	erf(x) is defined by
	${2 over sqrt(pi)} int from 0 to x e sup {-t sup 2} dt$

	the entry for erfc is provided because of the
	extreme loss of relative accuracy if erf(x) is
	called for large x and the result subtracted
	from 1. (e.g. for x= 5, 12 places are lost).

	Calls exp.

	Based on the ideas in Hart & Cheney's book, Computer Approximations,
	John Wiley and Sons, 1968, Section 6.7.

	Uses rational approximations for erf on [0.0, 2.0], computing erfc(x)
	as 1.0 - erf(x).
	Beyond 2.0, computes erfc by exp(-x*x)*p(1/(x*x))/q(1/(x*x))/x
	and erf(x) = 1.0 - erfc(x).


*/

#ifdef _CALL_MATHERR
#include <stdio.h>
#include <math.h>
#endif

#include "libm.h"

#if defined(mips) && !defined(__GNUC__)
extern	double	erf(double);
extern	double	erfc(double);

#pragma weak erf = __erf
#pragma weak erfc = __erfc
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern double __erf(double);
extern double __erfc(double);
#pragma weak erf
#pragma weak erfc
double erf( double arg ) {
  return __erf( arg );
}
double erfc( double arg ) {
  return __erfc( arg );
}
#elif defined(__GNUC__)
extern  double  __erf(double);

double    erf() __attribute__ ((weak, alias ("__erf")));

extern  double  __erfc(double);

double    erfc() __attribute__ ((weak, alias ("__erfc")));

#endif

extern	double __exp(double);

static const	du	Qnan = 
{D(QNANHI, QNANLO)};

static du Rsqrtpi = {D(0x3fe20dd7, 0x50429b6d)}; /* 1.0/sqrt(pi) */

static du Ulimit1 = {D(0x4017afb4, 0x8dc96626)}; /* upper limit for erf */

static du Llimit2 = {D(0xc017744f, 0x8f74e94a)}; /* lower limit for erfc */

static du Ulimit2 = {D(0x403b39dc, 0x41e48bfc)}; /* upper limit for erfc */

static double poly(double);

/* coefficients for approximating erf on [-.75, .75] */

static du p1[] = {
D(0x3fc06eba, 0x8214db69),
D(0xbfd49cb4, 0x2e1bc6cd),
D(0xbfa23997, 0x1bd010b5),
D(0xbf7ec3b8, 0x695750e1),
D(0xbf3008ae, 0xd8b328e9),
D(0xbef0d02f, 0xdfcc8cc0),
D(0x3e800fef, 0x1ad6e676),
};

static du q1[]  = {
D(0x3ff00000, 0x00000000),
D(0x3fdaf37d, 0xbd166903),
D(0x3fb3db3d, 0x1babb8d7),
D(0x3f802445, 0xd3709b73),
D(0x3f3d4818, 0xc17333c6),
D(0x3ee80b2a, 0x2eab6d6a),
D(0x00000000, 0x00000000),
};

/* coefficients for approximating erf on [.75, 1.25] */

static du p2[] = {
D(0x3fc42261, 0x62fbddd5),
D(0x3feaf767, 0xa741088b),
D(0x3fda911f, 0x096fbc26),
D(0xbfc03d80, 0x8b1137e1),
D(0x3fbbacfa, 0x66a0d1d5),
D(0x3f9cfff4, 0x709b5c7d),
D(0xbf5f504f, 0xfe135596),
D(0x3f68d5ea, 0x7faa17b4),
D(0x3f323c70, 0x817aefdb),
};

static du q2[] = {
D(0x3ff00000, 0x00000000),
D(0x3fe63821, 0x150312cf),
D(0x3fe3e2f0, 0x81ee743e),
D(0x3fd2a8e6, 0xefe9fae2),
D(0x3fc0bc42, 0xfb9b210a),
D(0x3fa5740f, 0x9a722ffb),
D(0x3f83f746, 0x57cfbd94),
D(0x3f5e1c3e, 0xb6d4d49f),
};

/* coefficients for approximating erf on [1.25, 1.75] */

static du p3[] = {
D(0x3fa15aaa, 0x8ec85205),
D(0x3feeea55, 0x57137ae0),
D(0x3fbe7237, 0x26b824a9),
D(0xbfb9d1a8, 0xf32fd923),
D(0x3fb77c31, 0x3b778138),
D(0xbfa02178, 0x6e76caa3),
D(0x3f822691, 0x77ac5924),
D(0xbf46d6ce, 0xa8d3d9e2),
};

static du q3[] = {
D(0x3ff00000, 0x00000000),
D(0x3fe4dd02, 0xa28f292f),
D(0x3fe2a536, 0x3e16bc6f),
D(0x3fcc9a1d, 0x0eb16b1c),
D(0x3fb9731a, 0x8b41322c),
D(0x3f948ac7, 0x6117721e),
D(0x3f73235a, 0x07cc2fd9),
};

/* coefficients for approximating erf on [1.75, 2.0] */

static du p4[] = {
D(0x3f806784, 0x42cc256f),
D(0x3fefbe61, 0xeef4cf6a),
D(0x3fa12ceb, 0x37ff9baf),
D(0xbf9a06d6, 0x144eb107),
D(0x3f9ab0c8, 0x0cca31bd),
D(0xbf802a99, 0x5ed29ad3),
D(0x3f5b1f65, 0x7b1d0822),
};

static du q4[] = {
D(0x3ff00000, 0x00000000),
D(0x3ff1e094, 0x50d81c1d),
D(0x3feb91fe, 0xb4228831),
D(0x3fd930cb, 0xc83fe50a),
D(0x3fbf0110, 0x1cb1561b),
D(0x3f91650c, 0xa6ca26a8),
};

/* coefficients for approximating erfc on [2.0, inf) in powers
 * of 1/(x*x)
 */

static du p5[]  = {
D(0xbfdfffff, 0xfffffff0),
D(0xc0326677, 0xc6c50dc8),
D(0xc06f48fc, 0x52b0853c),
D(0xc098dbdb, 0xc3a76fc9),
D(0xc0b33dee, 0x8ef175aa),
D(0xc0bb7e1f, 0x01b13b59),
D(0xc0aeacc7, 0xc40e8bc7),
D(0xc08134cd, 0x96535f33),
};

static du q5[]  = {
D(0x3ff00000, 0x00000000),
D(0x40432677, 0xc6c50cd9),
D(0x40815219, 0x63facf1f),
D(0x40ae55a4, 0x447cc2b3),
D(0x40cb6c9f, 0x19945427),
D(0x40d94ebc, 0x318e1d47),
D(0x40d618aa, 0xd7fba8cd),
D(0x40be9acc, 0xf88372df),
D(0x40859695, 0xf5e67d06),
};

/* coefficients for polynomial approximation of expm1 on +/- 1.8e-4 */

static const du	P[] =
{
D(0x3ff00000, 0x00000000),
D(0x3fe00000, 0x00000000),
D(0x3fc55555, 0x55555555),
D(0x3fa55555, 0x55df1a6d),
D(0x3f811111, 0x11fd3e5f),
};


double
__erf( double arg )
{
double __erfc(double);
double sign;
double argsq;
double d, n, z;
#ifdef _CALL_MATHERR
struct exception	exstruct;
#endif

	if( arg != arg )
	{
		/* arg is a NaN */

#ifdef _CALL_MATHERR

		exstruct.type = DOMAIN;
		exstruct.name = "erf";
		exstruct.arg1 = arg;
		exstruct.retval = Qnan.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "domain error in erf\n");
			SETERRNO(EDOM);
		}

		return ( exstruct.retval );
#else
		NAN_SETERRNO(EDOM);

		return ( Qnan.d );
#endif
	}

	sign = 1.0;

	if( arg < 0. )
	{
		arg = -arg;
		sign = -1.0;
	}

	if ( arg < 0.75 )
	{
		argsq = arg*arg;


		n = (((((p1[6].d*argsq + p1[5].d)*argsq + p1[4].d)*argsq +
				p1[3].d)*argsq + p1[2].d)*argsq + p1[1].d)*
				argsq + p1[0].d;

		d = ((((q1[5].d*argsq + q1[4].d)*argsq + q1[3].d)*argsq +
				q1[2].d)*argsq + q1[1].d)*argsq + q1[0].d;

		return ( sign*(arg + arg*n/d) );

	}
	else if ( arg < 1.25 )
	{
		z = arg - 1.0;

		n = ((((((p2[8].d*z + p2[7].d)*z + p2[6].d)*z + p2[5].d)*z +
			p2[4].d)*z + p2[3].d)*z + p2[2].d)*z;
		d = ((((((q2[7].d*z + q2[6].d)*z + q2[5].d)*z + q2[4].d)*z +
			q2[3].d)*z + q2[2].d)*z + q2[1].d)*z + q2[0].d;

		return ( sign*(p2[1].d + n/d) );
	}
	else if ( arg < 1.75 )
	{
		z = arg - 1.5;

		n = (((((p3[7].d*z + p3[6].d)*z +
			p3[5].d)*z + p3[4].d)*z + p3[3].d)*z + p3[2].d)*z;
		d = (((((q3[6].d*z + q3[5].d)*z + q3[4].d)*z +
			q3[3].d)*z + q3[2].d)*z + q3[1].d)*z + q3[0].d;

		return ( sign*(p3[1].d + n/d) );
	}
	else if ( arg < 2.0 )
	{
		z = arg - 1.875;

		n = ((((p4[6].d*z + p4[5].d)*z +
			p4[4].d)*z + p4[3].d)*z + p4[2].d)*z;
		d = ((((q4[5].d*z + q4[4].d)*z + q4[3].d)*z +
			q4[2].d)*z + q4[1].d)*z + q4[0].d;

		return ( sign*(p4[1].d + n/d) );
	}

	if ( fabs(arg) <= Ulimit1.d )
		return( sign*(1.0 - __erfc(arg)) );
	else
		return ( sign );
}

double
__erfc( double arg )
{
double argsq;
double z;
double zsq;
double s, f, f1;
double n, d;
double result;
#ifdef _CALL_MATHERR
struct exception	exstruct;
#endif

	if( arg != arg )
	{
		/* arg is a NaN */

#ifdef _CALL_MATHERR

		exstruct.type = DOMAIN;
		exstruct.name = "erfc";
		exstruct.arg1 = arg;
		exstruct.retval = Qnan.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "domain error in erfc\n");
			SETERRNO(EDOM);
		}

		return ( exstruct.retval );
#else
		NAN_SETERRNO(EDOM);

		return ( Qnan.d );
#endif
	}

	if ( arg < Llimit2.d )
		return ( 2.0 );

	if ( arg <= -2.0 )
		return ( 2.0 - __erfc(-arg) );

	if ( arg < 0.25 )
		return( 1.0 - __erf(arg) );

	result = 0.0; /* default */

	if ( arg < 0.75 )
	{
		argsq = arg*arg;

		n = (((((p1[6].d*argsq + p1[5].d)*argsq + p1[4].d)*argsq +
				p1[3].d)*argsq + p1[2].d)*argsq + p1[1].d)*
				argsq + p1[0].d;

		d = ((((q1[5].d*argsq + q1[4].d)*argsq + q1[3].d)*argsq +
				q1[2].d)*argsq + q1[1].d)*argsq + q1[0].d;

		return ( 0.5 + ((0.5 - arg) - arg*n/d) );
	}
	else if ( arg < 1.25 )
	{
		z = arg - 1.0;

		n = ((((((p2[8].d*z + p2[7].d)*z + p2[6].d)*z + p2[5].d)*z +
			p2[4].d)*z + p2[3].d)*z + p2[2].d)*z;
		d = ((((((q2[7].d*z + q2[6].d)*z + q2[5].d)*z + q2[4].d)*z +
			q2[3].d)*z + q2[2].d)*z + q2[1].d)*z + q2[0].d;

		return ( p2[0].d - n/d );
	}
	else if ( arg < 1.75 )
	{
		z = arg - 1.5;

		n = (((((p3[7].d*z + p3[6].d)*z +
			p3[5].d)*z + p3[4].d)*z + p3[3].d)*z + p3[2].d)*z;
		d = (((((q3[6].d*z + q3[5].d)*z +  q3[4].d)*z +
			q3[3].d)*z + q3[2].d)*z + q3[1].d)*z + q3[0].d;

		return ( p3[0].d - n/d );
	}
	else if ( arg < 2.0 )
	{
		z = arg - 1.875;

		n = ((((p4[6].d*z + p4[5].d)*z +
			p4[4].d)*z + p4[3].d)*z + p4[2].d)*z;
		d = ((((q4[5].d*z + q4[4].d)*z + q4[3].d)*z +
			q4[2].d)*z + q4[1].d)*z + q4[0].d;

		return ( p4[0].d - n/d );
	}
	else if ( arg <= Ulimit2.d )
	{
		zsq = 1.0/(arg*arg);

		n = (((((((p5[7].d*zsq + p5[6].d)*zsq + p5[5].d)*zsq +
			p5[4].d)*zsq + p5[3].d)*zsq + p5[2].d)*zsq + p5[1].d)*zsq +
			p5[0].d)*zsq;

		d = (((((((q5[8].d*zsq + q5[7].d)*zsq + q5[6].d)*zsq + q5[5].d)*zsq +
			q5[4].d)*zsq + q5[3].d)*zsq + q5[2].d)*zsq + q5[1].d)*zsq +
			q5[0].d;

		s = (float)arg;

		/* Now the result is exp(-arg*arg)*(1.0 + n/d)/(arg*sqrt(pi)) */

		if ( arg <= 26.0 )
		{
			/* To avoid loss of precision in computing exp(-arg*arg),
			 * rewrite -arg*arg as -s*s + s*(s-arg) + arg*(s-arg)
			 * Note that -s*s is exact.
			 */

			/* Compute the second exp in the expression locally,
			 * since the argument is small.
			 */

			f1 = __exp(-s*s);
			f = f1 + f1*poly(s*(s-arg) + arg*(s-arg));

			return ( Rsqrtpi.d*(f + f*n/d)/arg );
		}
		else
		{
			/* Beyond 26.0, we have to worry about underflow in
			 * computing exp(-arg*arg); instead we'll compute 
			 * exp(-arg*arg/2.0) and multiply by it twice.
			 * The result here may underflow, depending on 
			 * whether the processor supports denormals or not.
			 */


			s = (float)arg;

			f1 = __exp(-0.5*s*s);
			f = f1 + f1*poly(0.5*s*(s-arg) + 0.5*arg*(s-arg));

			result = Rsqrtpi.d*(f + f*n/d)/arg*f;
		}
	}

	if ( result == 0.0 )

#ifdef _CALL_MATHERR

	{
		exstruct.type = UNDERFLOW;
		exstruct.name = "erfc";
		exstruct.arg1 = arg;
		exstruct.retval = 0.0;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "underflow error in erfc\n");
			SETERRNO(ERANGE);
		}

		return ( exstruct.retval );
	}
#else
	{
		SETERRNO(ERANGE);

		return ( 0.0 );
	}
#endif

	return ( result );
}

static
double
poly( double x )
{
double	result;

	/* returns expm1(x) for |x| <= 1.8e-4 */

	result = (((P[4].d*x + P[3].d)*x + P[2].d)*x + P[1].d)*x*x + x;

	return ( result );
}

#ifdef NO_LONG_DOUBLE

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern long double __erfl(long double);
extern long double __erfcl(long double);
long double erfl( long double x ) {	
  return ( (long double)__erf((double)x) );
}

long double erfcl( long double x ) {	
  return ( (long double)__erfc((double)x) );
}
#elif defined(__GNUC__)
extern  long double  __erfl(long double);

long double    erfl() __attribute__ ((weak, alias ("__erfl")));

extern  long double  __erfcl(long double);

long double    erfcl() __attribute__ ((weak, alias ("__erfcl")));

#endif

long double
__erfl( long double x )
{	
	return ( (long double)__erf((double)x) );
}

long double
__erfcl( long double x )
{	
	return ( (long double)__erfc((double)x) );
}

#endif

