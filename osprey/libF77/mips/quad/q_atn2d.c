/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


#include <errno.h>
#include <inttypes.h>
#include "quad.h"

static long double __qldexp(long double, int);

typedef union
{
  struct
  {
    uint32_t hi[2];
    uint32_t lo[2];
  } word;

  long double  ld;
} ldu;

static long double  qatn2dres1[2][2] =
{90.0L, -90.0L, 90.0L, -90.0L};

static long double  qatn2dres2[2][2] =
{0.0L, -0.0L, 180.0L, -180.0L};

static long double  qatn2dres3[4][4] =
{0.0L,    0.0L,    90.0L,  90.0L,
 0.0L,    0.0L,    90.0L,  90.0L,
 0.0L,   -0.0L,    45.0L,  45.0L,
 180.0L, -180.0L, 135.0L, 135.0L,
};

static const  ldu  degprad =
{0x404ca5dc,  0x1a63c1f8,
 0xbce1e7ab,  0x456405f9};

long double __qatan2d(long double arg1, long double arg2)
{
  ldquad  x, y;
  ldquad  x1, y1;
  int  ix, xptx;
  int  iy, xpty;
  int  signx, signy;
  int  i, j;
  long double  result;

  y.ld = arg1;
  x.ld = arg2;
  /* filter out Nans */
  if ((y.q.hi != y.q.hi) || (x.q.hi != x.q.hi)) {
    /* y or x is a NaN; return a quiet NaN */
#ifdef _IP_NAN_SETS_ERRNO
    *__errnoaddr = EDOM;
#endif
    return (__libm_qnan_ld);
  }

  /* extract exponents of y and x for some quick screening */
  iy = *(int *)&y.q.hi;
  xpty = (iy >> 20);
  xpty &= 0x7ff;
  ix = *(int *)&x.q.hi;
  xptx = (ix >> 20);
  xptx &= 0x7ff;
  signy = (iy >> 31);
  signy = (signy & 1);
  signx = (ix >> 31);
  signx = (signx & 1);

  /* filter out zero arguments */
  if (x.q.hi == 0.0) {
    if (y.q.hi == 0.0) {
      return (0.0L);
    }
    return (qatn2dres1[signx][signy]);
  } else if (y.q.hi == 0.0) {
    return (qatn2dres2[signx][signy]);
  }

  /* filter out infinities */
  i = (xptx == 0x7ff);
  j = (xpty == 0x7ff);
  if ((i + j) != 0) {
    i = i + i;
    i = i + signx;
    j = j + j;
    j = j + signy;
    return (qatn2dres3[i][j]);
  }

  y1.ld = __qabs(y.ld);
  x1.ld = __qabs(x.ld);

  if (y1.q.hi < x1.q.hi) {
    /* |y/x| < 1.0 */
    if (y1.ld <= __qldexp(x1.ld, -1075)) {
      /* |y/x| underflows */
      return (qatn2dres2[signx][signy]);
    }
  } else {
    /* |y/x| >= 1.0 */
    /* a crude check to avoid underflow of x/y */
    if (xpty > xptx + 108)
      return ((signy == 0) ? 90.0L : -90.0L);
  }
  result = __qatan2(y.ld, x.ld);
  return (result*degprad.ld);
}

long double __q_atn2d(long double *arg1, long double *arg2)
{
  return(__qatan2d(*arg1, *arg2));
}

static long double __qldexp(long double x, int n)
{
  ldquad  z;
  ldquad  result;
  double  ldexp(double, int);


  z.ld = x;
  result.q.hi = ldexp(z.q.hi, n);
  if (result.q.hi == 0.0)
    result.q.lo = 0.0;
  else
    result.q.lo = ldexp(z.q.lo, n);
  if (result.q.lo == 0.0)
    result.q.lo = 0.0;
  *__errnoaddr = 0;
  return(result.ld);
}
