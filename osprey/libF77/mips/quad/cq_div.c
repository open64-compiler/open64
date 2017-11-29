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
 * Module: cq_div.c
 *
 * Description:  source code for complex quad division
 *
 * ====================================================================
 * ====================================================================
 */

#include <sys/fpu.h>
#include "quad.h"

  /* complex *32 division */

/* ====================================================================
 *
 * FunctionName    cq_div
 *
 * Description    computes complex quotient of args
 *
 * ====================================================================
 */

qcomplex __cqdiv(long double xqreal, long double xqimag, long double yqreal, long double yqimag)
{
  long double tmp;
  unsigned int m, n;
  qcomplex result;

  if ((xqreal != xqreal) || (xqimag != xqimag) ||
       (yqreal != yqreal) || (yqimag != yqimag)) {
    result.qreal = __libm_qnan_ld;
    result.qimag = __libm_qnan_ld;
    return result;
  }

  if ((yqreal == 0.0L) && (yqimag == 0.0L)) {
    result.qreal = xqreal/__libm_zero_ld;
    result.qimag = xqimag/__libm_zero_ld;
    return result;
  }

  if (yqreal == 0.0L) {
    result.qreal = xqimag/yqimag;
    result.qimag = -(xqreal/yqimag);
    return result;
  }

  if (yqimag == 0.0L) {
    result.qreal = xqreal/yqreal;
    result.qimag = xqimag/yqreal;
    return result;
  }

  if (__qabs(yqreal) <= __qabs(yqimag)) {
    /* turn off traps on underflow while computing
       yqreal/yqimag
    */

    m = get_fpc_csr();
    n = (m & 0xfffffeff);
    (void)set_fpc_csr(n);
    tmp = yqreal/yqimag;
    (void)set_fpc_csr(m);
    result.qreal = (xqimag + xqreal*tmp)/(yqimag + yqreal*tmp);
    result.qimag = (-xqreal + xqimag*tmp)/(yqimag + yqreal*tmp);
    return result;
  }

  /* turn off traps on underflow while computing
     yqimag/yqreal
  */

  m = get_fpc_csr();
  n = (m & 0xfffffeff);
  (void)set_fpc_csr(n);

  tmp = yqimag/yqreal;
  (void)set_fpc_csr(m);

  result.qreal = (xqreal + xqimag*tmp)/(yqreal + yqimag*tmp);
  result.qimag = (xqimag - xqreal*tmp)/(yqreal + yqimag*tmp);

  return result;
}

void __cq_div(qcomplex *result, qcomplex *x, qcomplex *y)
{
  *result = __cqdiv(x->qreal, x->qimag, y->qreal, y->qimag);
}
