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


#include <inttypes.h>
#include "quad.h"

/* intrinsic QPROD -- by value version */
/* multiplies doubles x and y, returning the long double product */

#ifdef FUSED_MADD
static double __sub(double, double, double);
#endif

typedef union
{
  struct
  {
    unsigned int hi;
    unsigned int lo;
  } word;

  double  d;
} du;

double  fabs(double);
#pragma intrinsic (fabs)

#ifndef FUSED_MADD
/* const1 is 1.0 + 2^(53 - 53/2), i.e. 1.0 + 2^27 */
static const du const1 = {0x41a00000, 0x02000000};
#endif

static const du twop590 = {0x64d00000, 0x00000000};
static const du twopm590 = {0x1b100000, 0x00000000};
static const du inf = {0x7ff00000, 0x00000000};

#define  NO  0
#define  YES  1

long double __qprod(double x, double y)
{
  ldquad z;
  int32_t ix, xptx, iy, xpty;
  double w, ww;
  double xfactor, yfactor;
  int32_t scaleup, scaledown;
  
#ifndef FUSED_MADD
  double p;
  double hx, hy, tx, ty;
#endif

#include "msg.h"

  /* Avoid underflows and overflows in forming the products
     x*const1.d, y*const1.d, x*y, and hx*hy by scaling if
     necessary.  x and y should also be scaled if tx*ty is
     a denormal.
  */
  ix = *(int32_t *)&x;
  xptx = (ix >> 20);
  xptx &= 0x7ff;
  iy = *(int32_t *)&y;
  xpty = (iy >> 20);
  xpty &= 0x7ff;
  if ((0x21a < xptx) && (xptx < 0x5fd) && (0x21a < xpty) && (xpty < 0x5fd)) {
    /* normal case */
#ifdef FUSED_MADD
    z.q.hi = x*y;
    z.q.lo = __sub(x, y, z.q.hi);
#else
    p = x*const1.d;
    hx = p + (x - p);
    tx = x - hx;
    p = y*const1.d;
    hy = p + (y - p);
    ty = y - hy;
    z.q.hi = x*y;
    z.q.lo = (((hx*hy - z.q.hi) + hx*ty) + hy*tx) + tx*ty;
#endif
    return (z.ld);
  }

  if ((xptx < 0x7ff) && (xpty < 0x7ff)) {
    if ((x == 0.0) || (y == 0.0)) {
      z.q.lo = 0.0;
      z.q.hi = x*y;
      return (z.ld);
    }
    xfactor = 1.0;
    yfactor = 1.0;
    scaleup = scaledown = NO;
    if (xptx <= 0x21a) {
      x *= twop590.d;
      xfactor = twopm590.d;
      scaleup = YES;
    }
    if (xpty <= 0x21a) {
      y *= twop590.d;
      yfactor = twopm590.d;
      scaleup = YES;
    }
    if (xptx >= 0x5fd) {
      x *= twopm590.d;
      xfactor = twop590.d;
      scaledown = YES;
    }
    if (xpty >= 0x5fd) {
      y *= twopm590.d;
      yfactor = twop590.d;
      scaledown = YES;
    }
    if ((scaleup == YES) && (scaledown == YES)) {
      xfactor = yfactor = 1.0;
    }
#ifdef FUSED_MADD
    w = x*y;
    ww = __sub(x, y, w);
#else
    p = x*const1.d;
    hx = p + (x - p);
    tx = x - hx;
    p = y*const1.d;
    hy = p + (y - p);
    ty = y - hy;
    w = x*y;
    ww = (((hx*hy - w) + hx*ty) + hy*tx) + tx*ty;
#endif
    w *= xfactor;
    w *= yfactor;
    if ((w == 0.0) || (fabs(w) == inf.d)) {
      z.q.hi = w;
      z.q.lo = 0.0;
      return (z.ld);
    }
    ww *= xfactor;
    ww *= yfactor;
    /* do to rounding, (w, ww) may not be normalized, so we have
       to normalize it again just to be safe
    */
    z.q.hi = w + ww;
    z.q.lo = w - z.q.hi + ww;
    return (z.ld);
  }
  z.q.lo = 0.0;
  z.q.hi = x*y;
  return (z.ld);
}

#ifdef FUSED_MADD
static double __sub(double x, double y, double z)
{
  return (x*y-z);
}
#endif
