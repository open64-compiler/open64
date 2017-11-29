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
 * Module: sinhf.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:23-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.sinhf.c $
 *
 * Revision history:
 *  09-Jun-93 - Original Version
 *
 * Description:	source code for sinhf function
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.sinhf.c $ $Revision: 1.5 $";

#ifdef _CALL_MATHERR
#include <stdio.h>
#include <math.h>
#include <errno.h>
#endif

#include "libm.h"

#if defined(mips) && !defined(__GNUC__)
extern	float	fsinh(float);
extern	float	sinhf(float);
extern	float	fcosh(float);
extern	float	coshf(float);

#pragma weak fsinh = __sinhf
#pragma weak sinhf = __sinhf

#pragma weak fcosh = __coshf
#pragma weak coshf = __coshf
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern  float  __sinhf(float);
extern  float  __coshf(float);
#pragma weak sinhf
#pragma weak coshf
float sinhf( float x ) {
  return __sinhf( x );
}
float coshf( float x ) {
  return __coshf( x );
}
#elif defined(__GNUC__)
extern  float  __sinhf(float);
float    sinhf(float) __attribute__ ((weak, alias ("__sinhf")));

extern  float  __coshf(float);
float    coshf(float) __attribute__ ((weak, alias ("__coshf")));
#endif

static const du	rln2 =
{D(0x3ff71547, 0x652b82fe)};

static const du	ln2 =
{D(0x3fe62e42, 0xfefa39ef)};


/* coefficients for polynomial approximation of cosh on +/- log(2)/2     */

static const du	C[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0x3fe00000, 0x042dcab5)},
{D(0x3fa55548, 0x0b2e33ce)},
{D(0x3f56d962, 0x2f30cebb)},
};

/* coefficients for polynomial approximation of sinh on +/- log(2)/2     */

static const du	S[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0x3fc55555, 0x5735979e)},
{D(0x3f81110b, 0x21321a53)},
{D(0x3f2a16f7, 0x7ea170a1)},
};

static const fu	f_one = {0x3f800000};

static const fu	Llimit = {0xc2b2d4fc};

static const fu	Ulimit = {0x42b2d4fc};

static const	du	sinhtab[] =
{
D(0x00000000, 0x00000000),
D(0x3fe80000, 0x00000000),
D(0x3ffe0000, 0x00000000),
D(0x400f8000, 0x00000000),
D(0x401fe000, 0x00000000),
D(0x402ff800, 0x00000000),
D(0x403ffe00, 0x00000000),
D(0x404fff80, 0x00000000),
D(0x405fffe0, 0x00000000),
D(0x406ffff8, 0x00000000),
D(0x407ffffe, 0x00000000),
D(0x408fffff, 0x80000000),
D(0x409fffff, 0xe0000000),
D(0x40afffff, 0xf8000000),
};

static const	du	coshtab[] =
{
D(0x3ff00000, 0x00000000),
D(0x3ff40000, 0x00000000),
D(0x40010000, 0x00000000),
D(0x40104000, 0x00000000),
D(0x40201000, 0x00000000),
D(0x40300400, 0x00000000),
D(0x40400100, 0x00000000),
D(0x40500040, 0x00000000),
D(0x40600010, 0x00000000),
D(0x40700004, 0x00000000),
D(0x40800001, 0x00000000),
D(0x40900000, 0x40000000),
D(0x40a00000, 0x10000000),
D(0x40b00000, 0x04000000),
};

static const	fu	Qnan = {QNANF};

static const fu Neginf = {0x7f800000};

static const fu Inf = {0x7f800000};


/* ====================================================================
 *
 * FunctionName		sinhf
 *
 * Description		computes hyperbolic sine of arg
 *
 * ====================================================================
 */

float
__sinhf( float x )
{
#ifdef _32BIT_MACHINE

int	m, n;

#else

long long m, n;

#endif

int	ix, xpt;
double	result;
double	nd;
double	dx, absdx;
double	y, ysq;
double	sy, cy;
#ifdef _CALL_MATHERR
struct exception	exstruct;
#endif

	/* extract exponent of x for some quick screening */

	FLT2INT(x, ix);	/* copy arg to an integer	*/
	xpt = (ix >> MANTWIDTH);	/* shift off mantissa bits	*/
	xpt &= 0xff;

	if ( xpt < 0x85 )
	{
		/* |x| < 64.0 */

		if ( xpt >= 0x73 )
		{
			/* |x| >= 2^(-12) */

			dx = x;
			absdx = fabs(dx);

			nd = absdx*rln2.d;
			n = ROUND(nd);

			/* n = NINT(|x|/log(2)) */

			/* reduce argument to +/- log(2)/2 */

			nd = n;

			y = absdx - nd*ln2.d;

			ysq = y*y;

			/* compute sinh(y) and cosh(y) - 1.0 */

			sy = ((S[3].d*ysq + S[2].d)*ysq + S[1].d)*(ysq*y) + y;

			cy = ((C[3].d*ysq + C[2].d)*ysq + C[1].d)*ysq + C[0].d;

			if ( n < 14 )
			{
				/* sinh(|x|) = sinh(y)*coshtab[n] + cosh(y)*sinhtab[n] */

				result = sy*coshtab[n].d + cy*sinhtab[n].d;

				if ( x < 0.0f )
					result = -result;

				return ( (float)result );
			}

L:
			/* sinh(|x|) = 0.5*2^n*e^y */

			result = cy + sy;

			/* multiply result by 2**(n-1)	*/

#ifdef _32BIT_MACHINE

			DBLHI2INT(result, m);
			m += ((n - 1) << DMANTWIDTH);
			INT2DBLHI(m, result);
#else
			DBL2LL(result, m);
			m += ((n - 1) << DMANTWIDTH);
			LL2DBL(m, result);
#endif
			if ( x < 0.0f )
				result = -result;

			return ( (float)result );
		}

		return ( x );
	}

	if ( x != x )
	{
		/* x is a NaN; return a quiet NaN */

#ifdef _CALL_MATHERR

                exstruct.type = DOMAIN;
                exstruct.name = "sinhf";
                exstruct.arg1 = x;
                exstruct.retval = Qnan.f;

                if ( matherr( &exstruct ) == 0 )
                {
                        fprintf(stderr, "domain error in sinhf\n");
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
                exstruct.name = "sinhf";
                exstruct.arg1 = x;
                exstruct.retval = Inf.f;

                if ( matherr( &exstruct ) == 0 )
                {
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

                exstruct.type = OVERFLOW;
                exstruct.name = "sinhf";
                exstruct.arg1 = x;
                exstruct.retval = Neginf.f;

                if ( matherr( &exstruct ) == 0 )
                {
                        SETERRNO(ERANGE);
                }

                return ( exstruct.retval );
#else
		SETERRNO(ERANGE);

		return ( Neginf.f );
#endif
	}

	/* essentially the same algorithm as above, however here n > 14,
	   so we can skip a couple of the tests 
	*/

	dx = x;
	absdx = fabs(dx);

	nd = absdx*rln2.d;
	n = ROUND(nd);
	nd = n;

	y = absdx - nd*ln2.d;

	ysq = y*y;

	sy = ((S[3].d*ysq + S[2].d)*ysq + S[1].d)*(ysq*y) + y;

	cy = ((C[3].d*ysq + C[2].d)*ysq + C[1].d)*ysq + C[0].d;

	goto L;
}


/* ====================================================================
 *
 * FunctionName		coshf
 *
 * Description		computes hyperbolic cosine of arg
 *
 * ====================================================================
 */

float
__coshf( float x )
{
#ifdef _32BIT_MACHINE

int	m, n;

#else

long long m, n;

#endif

int	ix, xpt;
double	result;
double	md;
double	dx, absdx;
double	y, ysq;
double	sy, cy;
#ifdef _CALL_MATHERR
struct exception	exstruct;
#endif

	/* extract exponent of x for some quick screening */

	FLT2INT(x, ix);	/* copy arg to an integer	*/
	xpt = (ix >> MANTWIDTH);	/* shift off mantissa	*/
	xpt &= 0xff;

	if ( xpt < 0x85 )
	{
		/* |x| < 64.0 */

		if ( xpt >= 0x66 )
		{
			/* |x| >= 2^(-25) */

			dx = x;

			absdx = fabs(dx);

			md = absdx*rln2.d;

			n = ROUND(md);

			/* n = NINT(x/log(2)) */

			/* reduce argument to +/- log(2)/2 */

			md = n;

			y = absdx - md*ln2.d;

			ysq = y*y;

			/* compute sinh(y) and cosh(y) - 1.0 */

			sy = ((S[3].d*ysq + S[2].d)*ysq + S[1].d)*(ysq*y) + y;

			cy = ((C[3].d*ysq + C[2].d)*ysq + C[1].d)*ysq + C[0].d;

			if ( n < 14 )
			{
				/* cosh(x) = cosh(y)*coshtab[n] + sinh(y)*sinhtab[n] */

				result = sy*sinhtab[n].d + cy*coshtab[n].d;

				return ( (float)result );
			}

L:
			/* cosh(x) = 0.5*2^n*e^y */

			result = cy + sy;

			/* multiply result by 2**(n-1)	*/

#ifdef _32BIT_MACHINE

			DBLHI2INT(result, m);
			m += ((n - 1) << DMANTWIDTH);
			INT2DBLHI(m, result);
#else
			DBL2LL(result, m);
			m += ((n - 1) << DMANTWIDTH);
			LL2DBL(m, result);
#endif
			return ( (float)result );
		}

		return ( f_one.f );
	}

	if ( x != x )
	{
		/* x is a NaN; return a quiet NaN */

#ifdef _CALL_MATHERR

                exstruct.type = DOMAIN;
                exstruct.name = "coshf";
                exstruct.arg1 = x;
                exstruct.retval = Qnan.f;

                if ( matherr( &exstruct ) == 0 )
                {
                        fprintf(stderr, "domain error in coshf\n");
                        SETERRNO(EDOM);
                }

                return ( exstruct.retval );
#else
		NAN_SETERRNO(EDOM);
		
		return ( Qnan.f );
#endif
	}

	if ( fabsf(x) > Ulimit.f )
	{
#ifdef _CALL_MATHERR

                exstruct.type = OVERFLOW;
                exstruct.name = "coshf";
                exstruct.arg1 = x;
                exstruct.retval = Inf.f;

                if ( matherr( &exstruct ) == 0 )
                {
                        SETERRNO(ERANGE);
                }

                return ( exstruct.retval );
#else
		SETERRNO(ERANGE);

		return ( Inf.f );
#endif
	}

	/* essentially the same algorithm as above, however here n > 14,
	   so we can skip a couple of the tests 
	*/

	dx = x;

	absdx = fabs(dx);

	md = absdx*rln2.d;
	n = ROUND(md);
	md = n;

	y = absdx - md*ln2.d;

	ysq = y*y;

	sy = ((S[3].d*ysq + S[2].d)*ysq + S[1].d)*(ysq*y) + y;

	cy = ((C[3].d*ysq + C[2].d)*ysq + C[1].d)*ysq + C[0].d;

	goto L;
}

