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


/* ====================================================================
 * ====================================================================
 *
 * Module: cq_sqrt.c
 *
 * Description:  source code for complex quad sqrt function
 *
 * ====================================================================
 * ====================================================================
 */

#include "quad.h"

/* intrinsic CQSQRT */

qcomplex __cqsqrt(long double zqreal, long double zqimag)
{
  long double mag;
  qcomplex result;

  if ((mag = __qhypot(zqreal, zqimag)) == 0.0L) {
    result.qreal = result.qimag = 0.0L;
  } else if (zqreal > 0.0L) {
    result.qreal = __qsqrt(0.5L*(mag + zqreal));
    result.qimag = (zqimag/result.qreal)*0.5L;
  } else {
    result.qimag = __qsqrt(0.5L*(mag - zqreal));
    if (zqimag < 0.0L)
      result.qimag = - result.qimag;
    result.qreal = (zqimag/result.qimag)*0.5L;
  }
  return result;
}

void __cq_sqrt(qcomplex *result, qcomplex *z)
{
  *result = __cqsqrt(z->qreal, z->qimag);
}
