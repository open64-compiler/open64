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
 * Module: tan.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:23-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.tan.c $
 *
 * Revision history:
 *  09-Jun-93 - Original Version
 *
 * Description:	source code for tan function
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.tan.c $ $Revision: 1.5 $";

#ifdef _CALL_MATHERR
#include <stdio.h>
#include <math.h>
#include <errno.h>
#endif

#include "libm.h"

#if defined(mips) && !defined(__GNUC__)
extern	double	tan(double);

#pragma weak tan = __tan
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern double __tan(double);
#pragma weak tan
double tan( double x ) {
  return __tan( x );
}
#elif defined(__GNUC__)
extern  double  __tan(double);

double    tan() __attribute__ ((weak, alias ("__tan")));

#endif

static const du	Qnan =
{D(QNANHI, QNANLO)};

static const du Inf =
{D(0x7ff00000, 0x00000000)};

static const du	half =
{D(0x3fe00000, 0x00000000)};

static const du	one =
{D(0x3ff00000, 0x00000000)};

static const du	twop19xpi =
{D(0x413921fb, 0x54442d18)};

static const du	twopm26 =
{D(0x3e500000, 0x00000000)};

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

/* coefficients for rational approximation of tan on +/- pi/4 */

static const du	p[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0xbfc06b8f, 0x5f225706)},
{D(0x3f66fc34, 0x2943627f)},
{D(0xbedf6255, 0x88fc315e)},
};

static const du	q[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0xbfdd8b1d, 0x04e680b5)},
{D(0x3f97e798, 0xc16cfadc)},
{D(0xbf2b51d1, 0xd7f65e57)},
};

#ifdef _TABLE_BASED_REDUCTION

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
 * FunctionName		tan
 *
 * Description		computes tangent of arg
 *
 * ====================================================================
 */

double
__tan( x )
double	x;
{
#ifdef _32BIT_MACHINE
int	ix, xpt, m, l;
#else
long long ix, xpt, m, l;
#endif

int	n;
double	xsq;
double	num, denom;
double	poly1, poly2;
double	result;
double	z, dn;
double	absx, dn1, dn2;
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

		if ( fabs(x) > twopm26.d )
		{
			/*   |x| > 2^(-26)   */

			/* just compute tan(x) */

			xsq = x*x;

			poly1 = ((p[3].d*xsq + p[2].d)*xsq + p[1].d)*(xsq*x) + x;
			poly2 = ((q[3].d*xsq + q[2].d)*xsq + q[1].d)*xsq + q[0].d;

			return ( poly1/poly2 );
		}

		return ( x );
	}

#ifdef _TABLE_BASED_REDUCTION

	if ( xpt < 0x806 )
	{
		/*   |x| < 16.0   */

		/*  do a table based argument reduction to +/- pi/4  */

		dn = x*rpiby2.d;
		n = ROUND(dn);

		/*  compute z = x - n*pi/2  */

		x = x - tblh[n+10].d;
		x = x - tbll[n+10].d;

		xsq = x*x;
	
		poly1 = ((p[3].d*xsq + p[2].d)*xsq + p[1].d)*(xsq*x) + x;
		poly2 = ((q[3].d*xsq + q[2].d)*xsq + q[1].d)*xsq + q[0].d;

		if ( (n&1) == 0 )
		{
			/* result is poly1/poly2 */

			num = poly1;
			denom = poly2;
		}

		if ( (n&1) != 0 )
		{
			/* result is -poly2/poly1 */

			denom = poly1;
			num = -poly2;
		}

		result = num/denom;

		return ( result );
	}
#endif

	if ( xpt < 0x827 )
	{
		/*  |x| < 1.5*2^20  */
cont:
		dn = x*rpiby2.d;
		n = ROUND(dn);
		dn = n;

		x = x - dn*piby2hi.d;
		x = x - dn*piby2lo.d;
		x = x - dn*piby2tiny.d;	/* x = x - n*pi/2 */

		xsq = x*x;
	
		poly1 = ((p[3].d*xsq + p[2].d)*xsq + p[1].d)*(xsq*x) + x;
		poly2 = ((q[3].d*xsq + q[2].d)*xsq + q[1].d)*xsq + q[0].d;

		if ( (n&1) == 0 )
		{
			/* result is poly1/poly2 */

			num = poly1;
			denom = poly2;
		}

		if ( (n&1) != 0 )
		{
			/* result is -poly2/poly1 */

			denom = poly1;
			num = -poly2;
		}

		result = num/denom;

		return ( result );
	}

	if ( xpt < 0x836 )
	{
		/*  |x| < 2^28  */

		if ( fabs(x) < twop19xpi.d )
			goto cont;

		dn = x*rpiby2.d;
		n = ROUND(dn);
		dn = n;

		x = x - dn*ph.d;
		x = x - dn*pl.d;
		x = x - dn*pt.d;
		x = x - dn*pe.d;
		x = x - dn*pe2.d;

		xsq = x*x;
	
		poly1 = ((p[3].d*xsq + p[2].d)*xsq + p[1].d)*(xsq*x) + x;
		poly2 = ((q[3].d*xsq + q[2].d)*xsq + q[1].d)*xsq + q[0].d;

		if ( (n&1) == 0 )
		{
			/* result is poly1/poly2 */

			num = poly1;
			denom = poly2;
		}

		if ( (n&1) != 0 )
		{
			/* result is -poly2/poly1 */

			denom = poly1;
			num = -poly2;
		}

		result = num/denom;

		return ( result );
	}

	if ( xpt < 0x862 )
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

		xsq = z*z;
	
		poly1 = ((p[3].d*xsq + p[2].d)*xsq + p[1].d)*(xsq*z) + z;
		poly2 = ((q[3].d*xsq + q[2].d)*xsq + q[1].d)*xsq + q[0].d;

		if ( (n&1) == 0 )
		{
			/* result is poly1/poly2 */

			num = poly1;
			denom = poly2;
		}

		if ( (n&1) != 0 )
		{
			/* result is -poly2/poly1 */

			denom = poly1;
			num = -poly2;
		}

		result = num/denom;

		return ( result );
	}

	if ( (x != x) || (fabs(x) == Inf.d) )
	{
		/* x is a NaN or +/-inf; return a quiet NaN */

#ifdef _CALL_MATHERR

		exstruct.type = DOMAIN;
		exstruct.name = "tan";
		exstruct.arg1 = x;
		exstruct.retval = Qnan.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "domain error in tan\n");
			SETERRNO(EDOM);
		}

		return ( exstruct.retval );
#else
		NAN_SETERRNO(EDOM);

		return ( Qnan.d );
#endif
	}

	/* just give up and return 0.0, setting errno = ERANGE */

#ifdef _CALL_MATHERR

		exstruct.type = TLOSS;
		exstruct.name = "tan";
		exstruct.arg1 = x;
		exstruct.retval = 0.0;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "range error in tan (total loss \
of significance)\n");
			SETERRNO(ERANGE);
		}

		return ( exstruct.retval );
#else
		SETERRNO(ERANGE);

		return ( 0.0 );
#endif
}

#ifdef NO_LONG_DOUBLE

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern long double __tanl(long double);
long double tanl( long double x ) {	
  return ( (long double)__tan((double)x) );
}
#elif defined(__GNUC__)
extern  long double  __tanl(long double);

long double    tanl() __attribute__ ((weak, alias ("__tanl")));

#endif

long double
__tanl( long double x )
{	
	return ( (long double)__tan((double)x) );
}

#endif

