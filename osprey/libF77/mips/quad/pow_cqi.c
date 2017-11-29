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


#include "cmplrs/host.h"
#include "qcmplx.h"
#include "cq_div.h"

qcomplex __powcqi(long double aqreal, long double aqimag, int32 n)   /* __powcqi = a**n  */
{
  long double t;
  qcomplex x, p;

  p.qreal = 1;
  p.qimag = 0;

  if(n == 0)
    return p;

  if(n < 0) {
    n = -n;
    x = __cqdiv(p.qreal, p.qimag, aqreal, aqimag);
  } else {
    x.qreal = aqreal;
    x.qimag = aqimag;
  }

  for( ; ; ) {
    if(n & 01) {
      t = p.qreal * x.qreal - p.qimag * x.qimag;
      p.qimag = p.qreal * x.qimag + p.qimag * x.qreal;
      p.qreal = t;
    }
    if(n >>= 1) {
      t = x.qreal * x.qreal - x.qimag * x.qimag;
      x.qimag = 2 * x.qreal * x.qimag;
      x.qreal = t;
    } else {
      break;
    }
  }
  return p;
}

qcomplex __powcql(long double aqreal, long double aqimag, int64 n)   /* __powcql = a**n  */
{
  long double t;
  qcomplex x, p;

  p.qreal = 1;
  p.qimag = 0;

  if(n == 0)
    return p;

  if(n < 0) {
    n = -n;
    x = __cqdiv(p.qreal, p.qimag, aqreal, aqimag);
  } else {
    x.qreal = aqreal;
    x.qimag = aqimag;
  }

  for( ; ; ) {
    if(n & 01) {
      t = p.qreal * x.qreal - p.qimag * x.qimag;
      p.qimag = p.qreal * x.qimag + p.qimag * x.qreal;
      p.qreal = t;
    }
    if(n >>= 1) {
      t = x.qreal * x.qreal - x.qimag * x.qimag;
      x.qimag = 2 * x.qreal * x.qimag;
      x.qreal = t;
    } else {
      break;
    }
  }
  return p;
}

void __pow_cqi(qcomplex *p, qcomplex *a, int32 *b)   /* p = a**b  */
{
  *p = __powcqi(a->qreal, a->qimag, *b);
}

void __pow_cql(qcomplex *p, qcomplex *a, int64 *b)   /* p = a**b  */
{
  *p = __powcql(a->qreal, a->qimag, *b);
}
