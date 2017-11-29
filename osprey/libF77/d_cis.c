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



#include "cmplx.h"
#include <errno.h>
#include "moremath.h"

extern  double  __libm_qnan_d;
extern  int  *__errnoaddr;

double  fabs(double);
#pragma intrinsic (fabs)

int  round(double);
#pragma intrinsic (round)

#define  ROUND(d)  round(d)

typedef union
{
  struct
  {
    unsigned int hi;
    unsigned int lo;
  } word;

  double  d;
} du;

/* tables used to do argument reduction for args between +/- 16 radians;
   the sum of the high and low values of the kth entry is (k - 10)*pi/2
*/

static const du  tblh[] =
{
{D(0xc02f6a7a,  0x2955385e)},
{D(0xc02c463a,  0xbeccb2bb)},
{D(0xc02921fb,  0x54442d18)},
{D(0xc025fdbb,  0xe9bba775)},
{D(0xc022d97c,  0x7f3321d2)},
{D(0xc01f6a7a,  0x2955385e)},
{D(0xc01921fb,  0x54442d18)},
{D(0xc012d97c,  0x7f3321d2)},
{D(0xc00921fb,  0x54442d18)},
{D(0xbff921fb,  0x54442d18)},
{D(0x00000000,  0x00000000)},
{D(0x3ff921fb,  0x54442d18)},
{D(0x400921fb,  0x54442d18)},
{D(0x4012d97c,  0x7f3321d2)},
{D(0x401921fb,  0x54442d18)},
{D(0x401f6a7a,  0x2955385e)},
{D(0x4022d97c,  0x7f3321d2)},
{D(0x4025fdbb,  0xe9bba775)},
{D(0x402921fb,  0x54442d18)},
{D(0x402c463a,  0xbeccb2bb)},
{D(0x402f6a7a,  0x2955385e)},
};

static const du  tbll[] =
{
{D(0xbcc60faf,  0xbfd97309)},
{D(0xbcc3daea,  0xf976e788)},
{D(0xbcc1a626,  0x33145c07)},
{D(0xbcbee2c2,  0xd963a10c)},
{D(0xbcba7939,  0x4c9e8a0a)},
{D(0xbcb60faf,  0xbfd97309)},
{D(0xbcb1a626,  0x33145c07)},
{D(0xbcaa7939,  0x4c9e8a0a)},
{D(0xbca1a626,  0x33145c07)},
{D(0xbc91a626,  0x33145c07)},
{D(0x00000000,  0x00000000)},
{D(0x3c91a626,  0x33145c07)},
{D(0x3ca1a626,  0x33145c07)},
{D(0x3caa7939,  0x4c9e8a0a)},
{D(0x3cb1a626,  0x33145c07)},
{D(0x3cb60faf,  0xbfd97309)},
{D(0x3cba7939,  0x4c9e8a0a)},
{D(0x3cbee2c2,  0xd963a10c)},
{D(0x3cc1a626,  0x33145c07)},
{D(0x3cc3daea,  0xf976e788)},
{D(0x3cc60faf,  0xbfd97309)},
};

static const du  rpiby2 =
{D(0x3fe45f30,  0x6dc9c883)};

static const du  twopm23 =
{D(0x3e800000,  0x00000000)};

static const du  zero =
{D(0x00000000,  0x00000000)};

static const du  half =
{D(0x3fe00000,  0x00000000)};

static const du  one =
{D(0x3ff00000,  0x00000000)};

static const du  ph =
{D(0x3ff921fb,  0x50000000)};

static const du  pl =
{D(0x3e5110b4,  0x60000000)};

static const du  pt =
{D(0x3c91a626,  0x30000000)};

static const du  pe =
{D(0x3ae8a2e0,  0x30000000)};

static const du  pe2 =
{D(0x394c1cd1,  0x29024e09)};

static const du  Pt =
{D(0x3c91a626,  0x33145c07)};

static const du  piby2low =
{D(0x3e5110b4,  0x611a6263)};

/* coefficients for polynomial approximation of sin on +/- pi/2 */

static const du  S[] =
{
{D(0x3ff00000,  0x00000000)},
{D(0xbfc55555,  0x55555548)},
{D(0x3f811111,  0x1110f7d0)},
{D(0xbf2a01a0,  0x19bfdf03)},
{D(0x3ec71de3,  0x567d4896)},
{D(0xbe5ae5e5,  0xa9291691)},
{D(0x3de5d8fd,  0x1fcf0ec1)},
};

/* coefficients for polynomial approximation of cos on +/- pi/2 */

static const du  C[] =
{
{D(0x3ff00000,  0x00000000)},
{D(0xbfdfffff,  0xffffff96)},
{D(0x3fa55555,  0x5554f0ab)},
{D(0xbf56c16c,  0x1640aaca)},
{D(0x3efa019f,  0x81cb6a1d)},
{D(0xbe927df4,  0x609cb202)},
{D(0x3e21b8b9,  0x947ab5c8)},
};

/* ====================================================================
 *
 * FunctionName    __dcis
 *
 * Description    computes cos(arg) + i*sin(arg)
 *
 * ====================================================================
 */

dcomplex __dcis(double x)
{
  double  xsq;
  double  cospoly, sinpoly;
  int  m, n;
  dcomplex  result;
  double  absx;
  double  y, z, dn;
  double  dn1, dn2;
  double  zz;
  int  ix, xpt;

  /* extract exponent of x and 1 bit of mantissa for some quick screening */

  ix = *(int *)(&x);
  xpt = (ix >> 19);
  xpt &= 0xfff;

  if (xpt < 0x7fd) {
    /*   |x| < 0.75   */
    n = 0;
    if (xpt >= 0x7c2)
    {
      /*   |x| >= 2^(-30)   */
      xsq = x*x;
      sinpoly = (((((S[6].d*xsq + S[5].d)*xsq + S[4].d)*xsq +
        S[3].d)*xsq + S[2].d)*xsq + S[1].d)*(xsq*x) +
        x;
      cospoly = (((((C[6].d*xsq + C[5].d)*xsq + C[4].d)*xsq +
        C[3].d)*xsq + C[2].d)*xsq + C[1].d)*xsq +
        one.d;
    } else {
      sinpoly = x;
      cospoly = one.d;
    }
L:
    if (n&1) {
      if (n&2) {
        /*
         *  n%4 = 3
         *  result is sin(x) - i*cos(x)
         */
        result.dreal = sinpoly;
        result.dimag = -cospoly;
      } else {
        /*
         *  n%4 = 1
         *  result is -sin(x) + i*cos(x)
         */
        result.dreal = -sinpoly;
        result.dimag = cospoly;
      }
      return (result);
    }
    if (n&2) {
      /*
       *  n%4 = 2
       *  result is -cos(x) - i*sin(x)
       */
      result.dreal = -cospoly;
      result.dimag = -sinpoly;
    } else {
      /*
       *  n%4 = 0
       *  result is cos(x) + i*sin(x)
       */
      result.dreal = cospoly;
      result.dimag = sinpoly;
    }
    return(result);
  }

  if (xpt < 0x806) {
    /*   |x| < 16.0   */
    /*  do a table based argument reduction to +/- pi/4  */
    dn = x*rpiby2.d;
    n = ROUND(dn);
    /*  compute z = x - n*pi/2  */
    x = x - tblh[n+10].d;
    y = tbll[n+10].d;
    z = x - y;
    zz = (x - z) - y;
    /* z is reduced argument; zz is correction term */
cont:
    xsq = z*z;
    cospoly = (((((C[6].d*xsq + C[5].d)*xsq + C[4].d)*xsq +
        C[3].d)*xsq + C[2].d)*xsq + C[1].d)*xsq -
        zz*z + one.d;
    sinpoly = (((((S[6].d*xsq + S[5].d)*xsq + S[4].d)*xsq +
      S[3].d)*xsq + S[2].d)*xsq + S[1].d)*(xsq*z) + zz + z;
    goto L;
  }

  if (xpt < 0x836) {
    /*  |x| < 2^28  */
    dn = x*rpiby2.d;
    n = ROUND(dn);
    dn = n;
    x = x - dn*ph.d;
    x = x - dn*pl.d;
    if (fabs(x) < twopm23.d)
      goto fix;
    y = dn*Pt.d;
    z = x - y;
    zz = (x - z) - y;
    goto cont;
fix:
    y = dn*pt.d;
    z = x - y;
    zz = (x - z) - y;
    x = z;
    y = dn*pe.d;
    z = x - y;
    zz += (x - z) - y;
    x = z;
    y = dn*pe2.d;
    z = x - y;
    zz += (x - z) - y;
    goto cont;
  }

  if (xpt < 0x862) {
    /*  |x| < 2^50  */
    absx = fabs(x);
    dn = z = absx*rpiby2.d;
    /* round dn to the nearest integer */
    m = *(int *)&dn;
    m >>= 20;
    m &= 0x7ff;
    /* shift off fractional bits of dn */
    n = *((int *)&dn + 1);
    n >>= (0x433 - m);
    *((int *)&dn + 1) = (n << (0x433 - m));
    /* adjust dn and n if the fractional part of dn
       was >= 0.5
    */
    n &= 3;
    if ((z - dn) >= half.d) {
      dn += one.d;
      n += 1;
    }
    /* split dn into 2 parts */
    dn1 = dn;
    m = *((int *)&dn1 + 1);
    m >>= 27;
    m <<= 27;
    *((int *)&dn1 + 1) = m;
    dn2 = dn - dn1;
    z = absx - dn1*ph.d - dn2*ph.d - dn*piby2low.d;
    zz = zero.d;
    if (x < 0.0) {
      z = -z;
      n = -n;
    }
    goto cont;
  }

  if (x != x) {
    /* x is a NaN; return a pair of quiet NaNs */
#ifdef _IP_NAN_SETS_ERRNO
    *__errnoaddr = EDOM;
#endif
    result.dreal = __libm_qnan_d;
    result.dimag = __libm_qnan_d;
    return (result);
  }

  /* just give up and return 0.0 */
  result.dreal = 0.0;
  result.dimag = 0.0;
  return (result);
}
