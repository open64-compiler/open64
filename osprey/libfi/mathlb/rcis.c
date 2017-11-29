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


#pragma ident "@(#) libfi/mathlb/rcis.c	92.1	11/22/99 10:13:02"


#if	defined(_LITTLE_ENDIAN)
#include "mathdefs.h"
#if	defined(__mips)
#include <errno.h>
extern  _f_real4  __libm_qnan_f;
extern  _f_int4 *__errnoaddr;
#endif

double  fabs(double);
#pragma intrinsic (fabs)

#if	defined(__mips) && _COMPILER_VERSION >= 400
int  round(double);
#pragma intrinsic (round)
#define  ROUND(d)  round(d)
#else
#define  ROUND(d)  (int)(((d) >= 0.0) ? ((d) + 0.5) : ((d) - 0.5))
#define IEEE_32_QUIET_NAN 0X7FC00000
#endif

typedef union {
	struct {
		unsigned int hi;
		unsigned int lo;
	} word;
	double  d;
} du;

/* coefficients for polynomial approximation of sin on +/- pi/2 */

static const du  S[] =
{
{0x3ff00000,  0x00000000},
{0xbfc55554,  0x5268a030},
{0x3f811073,  0xafd14db9},
{0xbf29943e,  0x0fc79aa9},
};

static const du  C[] =
{
{0x3ff00000,  0x00000000},
{0xbfdffffb,  0x2a77e083},
{0x3fa553e7,  0xf02ac8aa},
{0xbf5644d6,  0x2993c4ad},
};

static const du  rpiby2 =
{0x3fe45f30,  0x6dc9c883};

static const du  piby2hi =
{0x3ff921fb,  0x50000000};

static const du  piby2lo =
{0x3e5110b4,  0x611a6263};

static const du  zero =
{0x00000000,  0x00000000};

static const du  half =
{0x3fe00000,  0x00000000};

static const du  one =
{0x3ff00000,  0x00000000};

static const du  twopm12 =
{0x3f300000,  0x00000000};

/* ====================================================================
 *
 * FunctionName    __rcis
 *
 * Description    computes cos(arg) + i*sin(arg)
 *
 * ====================================================================
 */

complex_t
__rcis(_f_real4	x)
{
	complex_t	result;
	double		dx, xsq;
	double		dn, dn1, dn2;
	double		z, absdx;
	double		sinpoly, cospoly;
	int 		m, n;
	int		ix, xpt;

	/*
	 * extract exponent of x and 1 bit of mantissa for
	 * some quick screening
	 */
	ix = *(int *)(&x);
	xpt = (ix >> 22);
	xpt &= 0x1ff;
	/*
	 * xpt is exponent(x) + 1 bit of mantissa
	 */
	if (xpt < 0xfd)  {
		/*
		 * |x| < .75
		 */
		dx = x;
		n = 0;
L:
		if (fabs(dx) >= twopm12.d) {
			/*
		 	 * |dx| >= 2^(-12)
			 */
			xsq = dx*dx;
			sinpoly = ((S[3].d*xsq + S[2].d)*xsq +
				S[1].d)*(xsq*dx) + dx;
			cospoly = ((C[3].d*xsq + C[2].d)*xsq +
				C[1].d)*xsq + one.d;
		} else {
			sinpoly = dx;
			cospoly = one.d;
		}

		if (n&1) {

			if (n&2) {
				/*
				 *  n%4 = 3
				 *  result is sin(x) - i*cos(x)
				 */
				result.real = sinpoly;
				result.imag = -cospoly;
			} else {
				/*
				 *  n%4 = 1
				 *  result is -sin(x) + i*cos(x)
				 */
				result.real = -sinpoly;
				result.imag = cospoly;
			}
			return (result);
		}

		if (n&2) {
			/*
			 *  n%4 = 2
			 *  result is -cos(x) - i*sin(x)
			 */
			result.real = -cospoly;
			result.imag = -sinpoly;
		} else {
			/*
			 *  n%4 = 0
			 *  result is cos(x) + i*sin(x)
			 */
			result.real = cospoly;
			result.imag = sinpoly;
		}
		return(result);
	}

	if (xpt < 0x136) {
		/*
		 *  |x| < 2^28
		 */
		dx = x;
		dn = dx*rpiby2.d;
		n = ROUND(dn);
		dn = n;
		dx = dx - dn*piby2hi.d;
		dx = dx - dn*piby2lo.d;		/* dx = x - n*piby2 */
		goto L;
	}

	if (xpt < 0x162) {
		/*
		 *  |x| < 2^50
		 */
		dx = x;
		absdx = fabs(dx);
		dn = z = absdx*rpiby2.d;
		/*
		 * round dn to the nearest integer
		 */
		m = *(int *)&dn;
		m >>= 20;
		m &= 0x7ff;
		/*
		 * shift off fractional bits of dn
		 */
		n = *((int *)&dn + 1);
		n >>= (0x433 - m);

		*((int *)&dn + 1) = (n << (0x433 - m));
		n &= 3;
		/*
		 * adjust dn and n if the fractional part of dn
		 * was >= 0.5
		*/
		if ((z - dn) >= half.d) {
			dn += one.d;
			n += 1;
		}
		/*
		 * split dn into 2 parts
		 */
		dn1 = dn;
		m = *((int *)&dn1 + 1);
		m >>= 27;
		m <<= 27;
		*((int *)&dn1 + 1) = m;
		dn2 = dn - dn1;
		z = absdx - dn1*piby2hi.d - dn2*piby2hi.d - dn*piby2lo.d;
		if (dx < zero.d) {
			z = -z;
			n = -n;
		}
		dx = z;
		goto L;
	}

	if (x != x) {
		/*
		 * x is a NaN; return a pair of quiet NaNs
		 */
#if	defined(__mips)
#if	defined(_IP_NAN_SETS_ERRNO)
		*__errnoaddr = EDOM;
#endif
		result.real = __libm_qnan_f;
		result.imag = __libm_qnan_f;
#elif	defined(_LITTLE_ENDIAN)
		result.real = IEEE_32_QUIET_NAN;
		result.imag = IEEE_32_QUIET_NAN;
#endif
		return (result);
	}
	/*
	 * just give up and return 0.0
	 */
	result.real = (_f_real4)0.0;
	result.imag = (_f_real4)0.0;
	return (result);
}
#endif
