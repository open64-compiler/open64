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
 * Module: sincos.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:22-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.sincos.c $
 *
 * Revision history:
 *  10-Mar-00 - Original Version
 *
 * Description:	source code for sincos subroutine
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef _CALL_MATHERR
#include <stdio.h>
#include <math.h>
#include <errno.h>
#endif

#include "libm.h"
#include "complex.h"

#if defined(mips) && !defined(__GNUC__)
extern	void	sincos(double, double *, double *);

#pragma weak sincos = __sincos
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern void __sincos(double, double *, double *);
#pragma weak sincos
void sincos(double x, double *sinx, double *cosx) {
  return __sincos(x, sinx, cosx);
}
#elif defined(__GNUC__)
extern  void  __sincos(double, double *, double *);

void    sincos() __attribute__ ((weak, alias ("__sincos")));

#endif

static const	du	Qnan =
{D(QNANHI, QNANLO)};

static const du	half =
{D(0x3fe00000, 0x00000000)};

static const du	one =
{D(0x3ff00000, 0x00000000)};

static const du	Twop19xpi =
{D(0x413921fb, 0x54442d18)};

static const du	rpiby2 =
{D(0x3fe45f30, 0x6dc9c883)};

static const du	piby2hi =
{D(0x3ff921fb, 0x54400000)};

static const du	piby2lo =
{D(0x3dd0b461, 0x1a600000)};

static const du	piby2tiny =
{D(0x3ba3198a, 0x2e037073)};

static const du	ph =
{D(0x3ff921fb, 0x50000000)};

static const du	pl =
{D(0x3e5110b4, 0x60000000)};

static const du	pt =
{D(0x3c91a626, 0x30000000)};

static const du	pe =
{D(0x3ae8a2e0, 0x30000000)};

static const du	pe2 =
{D(0x394c1cd1, 0x29024e09)};

static const du	Ph =
{D(0x3ff921fb, 0x54000000)};

static const du	Pl =
{D(0x3e110b46, 0x10000000)};

static const du	Pt =
{D(0x3c5a6263, 0x3145c06e)};

/* coefficients for polynomial approximation of sin on +/- pi/4 */

static const du	S[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0xbfc55555, 0x55555548)},
{D(0x3f811111, 0x1110f7d0)},
{D(0xbf2a01a0, 0x19bfdf03)},
{D(0x3ec71de3, 0x567d4896)},
{D(0xbe5ae5e5, 0xa9291691)},
{D(0x3de5d8fd, 0x1fcf0ec1)},
};

/* coefficients for polynomial approximation of cos on +/- pi/4 */

static const du	C[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0xbfdfffff, 0xffffff96)},
{D(0x3fa55555, 0x5554f0ab)},
{D(0xbf56c16c, 0x1640aaca)},
{D(0x3efa019f, 0x81cb6a1d)},
{D(0xbe927df4, 0x609cb202)},
{D(0x3e21b8b9, 0x947ab5c8)},
};

#ifdef _32BIT_MACHINE

/* tables used to do argument reduction for args between +/- 16 radians;
   the sum of the high and low values of the kth entry is (k - 10)*pi/2
*/

static const du	tblh[] =
{
{D(0xc02f6a7a, 0x2955385e)},
{D(0xc02c463a, 0xbeccb2bb)},
{D(0xc02921fb, 0x54442d18)},
{D(0xc025fdbb, 0xe9bba775)},
{D(0xc022d97c, 0x7f3321d2)},
{D(0xc01f6a7a, 0x2955385e)},
{D(0xc01921fb, 0x54442d18)},
{D(0xc012d97c, 0x7f3321d2)},
{D(0xc00921fb, 0x54442d18)},
{D(0xbff921fb, 0x54442d18)},
{D(0x00000000, 0x00000000)},
{D(0x3ff921fb, 0x54442d18)},
{D(0x400921fb, 0x54442d18)},
{D(0x4012d97c, 0x7f3321d2)},
{D(0x401921fb, 0x54442d18)},
{D(0x401f6a7a, 0x2955385e)},
{D(0x4022d97c, 0x7f3321d2)},
{D(0x4025fdbb, 0xe9bba775)},
{D(0x402921fb, 0x54442d18)},
{D(0x402c463a, 0xbeccb2bb)},
{D(0x402f6a7a, 0x2955385e)},
};

static const du	tbll[] =
{
{D(0xbcc60faf, 0xbfd97309)},
{D(0xbcc3daea, 0xf976e788)},
{D(0xbcc1a626, 0x33145c07)},
{D(0xbcbee2c2, 0xd963a10c)},
{D(0xbcba7939, 0x4c9e8a0a)},
{D(0xbcb60faf, 0xbfd97309)},
{D(0xbcb1a626, 0x33145c07)},
{D(0xbcaa7939, 0x4c9e8a0a)},
{D(0xbca1a626, 0x33145c07)},
{D(0xbc91a626, 0x33145c07)},
{D(0x00000000, 0x00000000)},
{D(0x3c91a626, 0x33145c07)},
{D(0x3ca1a626, 0x33145c07)},
{D(0x3caa7939, 0x4c9e8a0a)},
{D(0x3cb1a626, 0x33145c07)},
{D(0x3cb60faf, 0xbfd97309)},
{D(0x3cba7939, 0x4c9e8a0a)},
{D(0x3cbee2c2, 0xd963a10c)},
{D(0x3cc1a626, 0x33145c07)},
{D(0x3cc3daea, 0xf976e788)},
{D(0x3cc60faf, 0xbfd97309)},
};
#endif


/* ====================================================================
 *
 * FunctionName    __sincos
 *
 * Description    computes sin(arg) , cos(arg)
 *
 * Note:  Routine __sincos() computes sin(arg) and cos(arg) and should
 *	  be kept in sync with sin() and cos() so that it returns 
 *	  the same result as one gets by calling sin() and cos() 
 *	  separately.
 *
 * ====================================================================
 */

void
__sincos(double x, double *sinx, double *cosx)
{
#ifdef _32BIT_MACHINE
int	ix, xpt, m, l;
#else
long long ix, xpt, m, l;
#endif

double  xsq;
double  cospoly, sinpoly;
int  n;
dcomplex  result;
double  absx;
double  z, dn;
double  dn1, dn2;
double	s, ss;
double	t, w, ww;
#ifdef _CALL_MATHERR
struct exception	exstruct;
#endif

	/* extract exponent of x and 1 bit of mantissa for some quick screening */

#ifdef _32BIT_MACHINE

	DBLHI2INT(x, ix);	/* copy MSW of x to ix	*/
#else
	DBL2LL(x, ix);		/* copy x to ix	*/
#endif
	xpt = (ix >> (DMANTWIDTH-1));
	xpt &= 0xfff;

	if ( xpt < 0x7fd )
	{
		/*   |x| < 0.75   */

		if ( xpt >= 0x7c2 )
		{
      			/*   |x| >= 2^(-30)   */

      			xsq = x*x;

			*cosx = (((((C[6].d*xsq + C[5].d)*xsq +
					C[4].d)*xsq + C[3].d)*xsq +
					C[2].d)*xsq + C[1].d)*xsq + C[0].d;

			*sinx = (((((S[6].d*xsq + S[5].d)*xsq +
					S[4].d)*xsq + S[3].d)*xsq +
					S[2].d)*xsq + S[1].d)*(xsq*x) + x;

			return;
		}
		else
		{
			*cosx = 1.0;
			*sinx = x;

			return;
		}
	}

#ifdef _32BIT_MACHINE

	if (xpt < 0x806)
	{
		/*   |x| < 16.0   */

		/*  do a table based argument reduction to +/- pi/4  */

		dn = x*rpiby2.d;

		n = ROUND(dn);

		/*  compute x - n*pi/2  */

		x = x - tblh[n+10].d;
		x = x - tbll[n+10].d;

		goto L;

	} else
#endif
	if ( xpt < 0x827 )
	{
		/*  |x| < 1.5*2^20  */

		dn = x*rpiby2.d;
		n = ROUND(dn);
		dn = n;

		x = x - dn*piby2hi.d;
		x = x - dn*piby2lo.d;
		x = x - dn*piby2tiny.d;	/* x = x - n*pi/2 */

L:
		xsq = x*x;

		cospoly = (((((C[6].d*xsq + C[5].d)*xsq +
				C[4].d)*xsq + C[3].d)*xsq +
				C[2].d)*xsq + C[1].d)*xsq + C[0].d;

		sinpoly = (((((S[6].d*xsq + S[5].d)*xsq +
				S[4].d)*xsq + S[3].d)*xsq +
				S[2].d)*xsq + S[1].d)*(xsq*x) + x;

		if ( n&1 )
		{
			if ( n&2 )
			{
				/*
				 *  n%4 = 3
				 *  result is (-cos(x), sin(x))
				 */

				*sinx = -cospoly;
				*cosx = sinpoly;
			}
			else
			{
				/*
				 *  n%4 = 1
				 *  result is (cos(x), -sin(x))
				 */

				*sinx = cospoly;
				*cosx = -sinpoly;
			}

			return;
		}

		if ( n&2 )
		{
			/*
			 *  n%4 = 2
			 *  result is (-sin(x), -cos(x))
			 */

			*sinx = -sinpoly;
			*cosx = -cospoly;
		}
		else
		{
			/*
			 *  n%4 = 0
			 *  result is (sin(x), cos(x))
			 */

			*sinx = sinpoly;
			*cosx = cospoly;
		}

		return;

	}
	else if ( xpt < 0x836 )
	{
		/*  |x| < 2^28  */

		dn = x*rpiby2.d;
		n = ROUND(dn);
		dn = n;

		x = x - dn*ph.d;
		x = x - dn*pl.d;
		x = x - dn*pt.d;
		x = x - dn*pe.d;
		x = x - dn*pe2.d;

		goto L;
	}
	else if ( xpt < 0x862 )
	{
		/*  |x| < 2^50  */

		absx = fabs(x);

		dn = z = absx*rpiby2.d;

		/* round dn to the nearest integer */

#ifdef _32BIT_MACHINE

		DBLHI2INT(dn, l);
		m = (l >> DMANTWIDTH);
		m &= 0x7ff;

		/* shift off fractional bits of dn */

		DBLLO2INT(dn, l);

		l >>= (0x433 - m);
		n = l;
		l <<= (0x433 - m);
		INT2DBLLO(l, dn);
#else
		DBL2LL(dn, l);
		m = (l >> DMANTWIDTH);
		m &= 0x7ff;

		/* shift off fractional bits of dn */

		l >>= (0x433 - m);
		n = l;
		l <<= (0x433 - m);
		LL2DBL(l, dn);
#endif
		/* adjust dn and n if the fractional part of dn 
		   was >= 0.5
		*/

		n &= 3;

		if ( (z - dn) >= half.d )
		{
			dn += one.d;
			n += 1;
		}

	/* compute x - dn*Ph - dn*Pl - dn*Pt by dividing dn into
	   two parts and using the Kahan summation formula
	*/

		/* split dn into 2 parts */

		dn1 = dn;

#ifdef _32BIT_MACHINE

		DBLLO2INT(dn1, m);
		m >>= 28;
		m <<= 28;
		INT2DBLLO(m, dn1);
#else
		DBL2LL(dn1, m);
		m >>= 28;
		m <<= 28;
		LL2DBL(m, dn1);
#endif
		dn2 = dn - dn1;

		z = absx - dn1*Ph.d;	/* this operation is exact */

		t = dn2*Ph.d;
		s = z - t;
		ss = z - s - t;		/* correction term */

		t = ss - dn1*Pl.d;
		w = s + t;
		ww = s - w + t;		/* correction term */

		t = ww - dn2*Pl.d;
		s = w + t;
		ss = w - s + t;		/* correction term */

		t = ss - dn1*Pt.d;
		w = s + t;
		ww = s - w + t;		/* correction term */

		t = ww - dn2*Pt.d;
		z = w + t;		/* z = reduced arg */

		if ( x < 0.0 )
		{	/* adjust for sign of arg */

			z = -z;
			n = -n;
		}

		x = z;

		goto L;
	}


	if (x != x)
	{
		/* x is a NaN; return a pair of quiet NaNs */

		NAN_SETERRNO(EDOM);

		*sinx = Qnan.d;
		*cosx = Qnan.d;
		return;
	}

	/* just give up and return 0.0 */

	*sinx = 0.0;
	*cosx = 0.0;
	return;
}

#ifdef NO_LONG_DOUBLE

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern void __sincosl(long double, long double *, long double *);
void sincosl(long double x, long double *sinxl, long double *cosxl) {
  double sinx, cosx;

   __sincos((double)x, &sinx, &cosx);

   *sinxl = sinx;
   *cosxl = cosx;
}
#elif defined(__GNUC__)
extern  void  __sincosl(long double, long double *, long double *);

void    sincosl() __attribute__ ((weak, alias ("__sincosl")));

#endif

void
__sincosl(long double x, long double *sinxl, long double *cosxl)
{
double	sinx, cosx;

	__sincos((double)x, &sinx, &cosx);

	*sinxl = sinx;
	*cosxl = cosx;
}

#endif

