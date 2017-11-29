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
 * Module: sinh.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:23-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.sinh.c $
 *
 * Revision history:
 *  09-Jun-93 - Original Version
 *
 * Description:	source code for sinh and cosh functions
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.sinh.c $ $Revision: 1.5 $";

#ifdef _CALL_MATHERR
#include <stdio.h>
#include <math.h>
#include <errno.h>
#endif

#include "libm.h"

#if defined(mips) && !defined(__GNUC__)
extern	double	sinh(double);
extern	double	cosh(double);

#pragma weak sinh = __sinh

#pragma weak cosh = __cosh
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern  double  __sinh(double);
extern  double  __cosh(double);
#pragma weak sinh
#pragma weak cosh
double sinh( double x ) {
  return __sinh( x );
}
double cosh( double x ) {
  return __cosh( x );
}
#elif defined(__GNUC__)
extern  double  __sinh(double);

double    sinh() __attribute__ ((weak, alias ("__sinh")));

extern  double  __cosh(double);

double    cosh() __attribute__ ((weak, alias ("__cosh")));

#endif

extern const du __sinhtab[];
extern const du __coshtab[];

static const du	Qnan =
{D(QNANHI, QNANLO)};

static const du Neginf =
{D(0xfff00000, 0x00000000)};

static const	du	Inf =
{D(0x7ff00000, 0x00000000)};

static const du	rln2 =
{D(0x3ff71547, 0x652b82fe)};

static const du	ln2hi =
{D(0x3fe62e42, 0xfefa3800)};

static const du	ln2lo = 
{D(0x3d2ef357, 0x93c76730)};

static const du	Llimit =
{D(0xc08633ce, 0x8fb9f87d)};

static const du	Ulimit =
{D(0x408633ce, 0x8fb9f87d)};

static const du	one =
{D(0x3ff00000, 0x00000000)};

static const du	twop45 =
{D(0x42c00000, 0x00000000)};

/* coefficients for polynomial approximation of cosh on +/- log(2)/2     */

static const du	C[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0x3fe00000, 0x00000000)},
{D(0x3fa55555, 0x5555555c)},
{D(0x3f56c16c, 0x16c13faa)},
{D(0x3efa01a0, 0x1b15c941)},
{D(0x3e927e4c, 0x543bef58)},
{D(0x3e21f94e, 0x7709f742)},
};

/* coefficients for polynomial approximation of sinh on +/- log(2)/2     */

static const du	S[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0x3fc55555, 0x55555559)},
{D(0x3f811111, 0x1110f656)},
{D(0x3f2a01a0, 0x1b0026c3)},
{D(0x3ec71ddf, 0x81445006)},
{D(0x3e5af5f1, 0xffe4c94c)},
};


/* ====================================================================
 *
 * FunctionName		sinh
 *
 * Description		computes hyperbolic sine of arg
 *
 * ====================================================================
 */

double
__sinh( double x )
{
#ifdef _32BIT_MACHINE

int	ix, xpt;
int	l;

#else

long long ix, xpt;
long long l;

#endif

int	n;
double	absx;
double	y1, y2;
double	result, md;
double	y, ysq;
double	symy, cym1;
double	twopnm1;
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

	if ( xpt < 0x404 )
	{
		/* |x| < 32.0 */

		if ( xpt >= 0x3e3 )
		{
			/* |x| >= 2^(-28) */

			absx = fabs(x);
			md = absx*rln2.d;
			n = ROUND(md);

			/* n = NINT(|x|/log(2)) */

			/* reduce argument to +/- log(2)/2 */

			md = n;

			/* compute y = |x| - n*log(2) */

			y1 = absx - md*ln2hi.d;
			y2 = md*ln2lo.d;
			y = y1 - y2;

			ysq = y*y;

			/* compute sinh(y) - y and cosh(y) - 1.0 */

			symy = ((((S[5].d*ysq + S[4].d)*ysq + S[3].d)*ysq +
				 S[2].d)*ysq + S[1].d)*(ysq*y);

			cym1 = (((((C[6].d*ysq + C[5].d)*ysq + C[4].d)*ysq +
				C[3].d)*ysq + C[2].d)*ysq + C[1].d)*ysq;

			/* sinh(x) = sinh(y)*__coshtab(n) + cosh(y)*__sinhtab(n) */

			result = symy*__coshtab[n].d + y*__coshtab[n].d + cym1*__sinhtab[n].d +
				__sinhtab[n].d;

			if ( x < 0.0 )
				result = -result;

			return ( result );
		}

		return ( x );
	}
	else if ( xpt < 0x408 )
	{
L:
		/* Here sinh(|x|) = 0.5*2^n*e^y to machine precision */
	
		absx = fabs(x);
	
		md = absx*rln2.d;
		n = ROUND(md);
		md = n;
	
		y1 = absx - md*ln2hi.d;
		y2 = md*ln2lo.d;
		y = y1 - y2;
	
		ysq = y*y;
	
		symy = ((((S[5].d*ysq + S[4].d)*ysq + S[3].d)*ysq + S[2].d)*ysq +
			S[1].d)*(ysq*y);
	
		cym1 = (((((C[6].d*ysq + C[5].d)*ysq + C[4].d)*ysq +
			C[3].d)*ysq + C[2].d)*ysq + C[1].d)*ysq;
	
		result = cym1 + symy - y2 + y1 + one.d;
		n -= 45;
		l = (n - 1) + DEXPBIAS;
		l <<= DMANTWIDTH;
	
#ifdef _32BIT_MACHINE
	
		twopnm1 = 0.0;
		INT2DBLHI(l, twopnm1);
#else
		LL2DBL(l, twopnm1);
#endif
		result *= twop45.d;
		result *= twopnm1;
	
		if ( x < 0.0 )
			result = -result;
	
		return ( result );
	}

	if ( x != x )
	{
		/* x is a NaN; return a quiet NaN */

#ifdef _CALL_MATHERR

		exstruct.type = DOMAIN;
		exstruct.name = "sinh";
		exstruct.arg1 = x;
		exstruct.retval = Qnan.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "domain error in sinh\n");
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
		exstruct.name = "sinh";
		exstruct.arg1 = x;
		exstruct.retval = Inf.d;

		if ( matherr( &exstruct ) == 0 )
		{
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

		exstruct.type = OVERFLOW;
		exstruct.name = "sinh";
		exstruct.arg1 = x;
		exstruct.retval = Neginf.d;

		if ( matherr( &exstruct ) == 0 )
		{
			SETERRNO(ERANGE);
		}

		return ( exstruct.retval );
#else
		SETERRNO(ERANGE);

		return ( Neginf.d );
#endif
	}

	goto L;

}


/* ====================================================================
 *
 * FunctionName		cosh
 *
 * Description		computes hyperbolic cosine of arg
 *
 * ====================================================================
 */

double
__cosh( double x )
{
#ifdef _32BIT_MACHINE

int	ix, xpt;
int	l;

#else

long long ix, xpt;
long long l;

#endif

int	n;
double	absx;
double	y1, y2;
double	result, md;
double	y, ysq;
double	symy, cym1;
double	twopnm1;
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

	if ( xpt < 0x404 )
	{
		/* |x| < 32.0 */

		if ( xpt >= 0x3e3 )
		{
			/* |x| >= 2^(-28) */

			absx = fabs(x);
			md = absx*rln2.d;
			n = ROUND(md);

			/* n = NINT(|x|/log(2)) */

			/* reduce argument to +/- log(2)/2 */

			md = n;

			/* compute y = |x| - n*log(2) */

			y1 = absx - md*ln2hi.d;
			y2 = md*ln2lo.d;
			y = y1 - y2;

			ysq = y*y;

			/* compute sinh(y) - y and cosh(y) - 1.0 */

			symy = ((((S[5].d*ysq + S[4].d)*ysq + S[3].d)*ysq +
				 S[2].d)*ysq + S[1].d)*(ysq*y);

			cym1 = (((((C[6].d*ysq + C[5].d)*ysq + C[4].d)*ysq +
				C[3].d)*ysq + C[2].d)*ysq + C[1].d)*ysq;

			/* cosh(x) = sinh(y)*__sinhtab(n) + cosh(y)*__coshtab(n) */

			result = symy*__sinhtab[n].d + y*__sinhtab[n].d + cym1*__coshtab[n].d +
				 __coshtab[n].d; 

			return ( result );
		}

		return ( one.d );
	}
	else if ( xpt < 0x408 )
	{
L:
		/* Here cosh(x) = 0.5*2^n*e^y to machine precision */
	
		absx = fabs(x);
	
		md = absx*rln2.d;
		n = ROUND(md);
		md = n;
	
		y1 = absx - md*ln2hi.d;
		y2 = md*ln2lo.d;
		y = y1 - y2;
	
		ysq = y*y;
	
		symy = ((((S[5].d*ysq + S[4].d)*ysq + S[3].d)*ysq + S[2].d)*ysq +
			S[1].d)*(ysq*y);
	
		cym1 = (((((C[6].d*ysq + C[5].d)*ysq + C[4].d)*ysq + C[3].d)*ysq +
			C[2].d)*ysq + C[1].d)*ysq;
	
		result = cym1 + symy - y2 + y1 + one.d;
	
		n -= 45;
		l = (n - 1) + DEXPBIAS;
		l <<= DMANTWIDTH;
	
#ifdef _32BIT_MACHINE
	
		twopnm1 = 0.0;
		INT2DBLHI(l, twopnm1);
#else
		LL2DBL(l, twopnm1);
#endif
		result *= twop45.d;
		result *= twopnm1;
	
		return ( result );
	}

	if ( x != x )
	{
#ifdef _CALL_MATHERR

		exstruct.type = DOMAIN;
		exstruct.name = "cosh";
		exstruct.arg1 = x;
		exstruct.retval = Qnan.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "domain error in cosh\n");
			SETERRNO(EDOM);
		}

		return ( exstruct.retval );
#else
		NAN_SETERRNO(EDOM);

		return ( Qnan.d );
#endif
	}

	if ( fabs(x) > Ulimit.d )
	{
#ifdef _CALL_MATHERR

		exstruct.type = OVERFLOW;
		exstruct.name = "cosh";
		exstruct.arg1 = x;
		exstruct.retval = Inf.d;

		if ( matherr( &exstruct ) == 0 )
		{
			SETERRNO(ERANGE);
		}

		return ( exstruct.retval );
#else
		SETERRNO(ERANGE);

		return ( Inf.d );
#endif
	}

	goto L;

}

#ifdef NO_LONG_DOUBLE

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern long double __sinhl(long double);
extern long double __coshl(long double);
long double sinhl( long double x ) {	
  return ( (long double)__sinh((double)x) );
}

long double coshl( long double x ) {	
  return ( (long double)__cosh((double)x) );
}
#elif defined(__GNUC__)
extern  long double  __sinhl(long double);

long double    sinhl() __attribute__ ((weak, alias ("__sinhl")));

extern  long double  __coshl(long double);

long double    coshl() __attribute__ ((weak, alias ("__coshl")));

#endif

long double
__sinhl( long double x )
{	
	return ( (long double)__sinh((double)x) );
}

long double
__coshl( long double x )
{	
	return ( (long double)__cosh((double)x) );
}

#endif

