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


#include "quad.h"

/* various type conversions involving long doubles and complex or
   quad complex
*/

/* conversions to complex */

  /* R16 to X8 */

void __cmplx1_q(complex *result, long double *u)
{
  result->real = *u;
  result->imag = 0.0L;
}

void __cmplx2_q(complex *result, long double *u, long double *v)
{
  result->real = *u;
  result->imag = *v;
}

  /* X32 to X8 */

void __cmplx_cq(complex *result, qcomplex *u)
{
  result->real = u->qreal;
  result->imag = u->qimag;
}

/* conversions to double complex */

  /* R16 to X16 */

void __dcmplx1_q(dcomplex *result, long double *u)
{
  result->dreal = *u;
  result->dimag = 0.0L;
}

void __dcmplx2_q(dcomplex *result, long double *u, long double *v)
{
  result->dreal = *u;
  result->dimag = *v;
}

  /* X32 to X16 */

void __dcmplx_cq(dcomplex *result, qcomplex *u)
{
  result->dreal = u->qreal;
  result->dimag = u->qimag;
}

  /* I2 to X32 */

void __qcmplx1_i(qcomplex *result, short *u)
{
  result->qreal = *u;
  result->qimag = 0.0L;
}

void __qcmplx2_i(qcomplex *result, short *u, short *v)
{
  result->qreal = *u;
  result->qreal = *v;
}

  /* I4 to X32 */

void __qcmplx1_j(qcomplex *result, int *u)
{
  result->qreal = *u;
  result->qimag = 0.0L;
}

void __qcmplx2_j(qcomplex *result, int *u, int *v)
{
  result->qreal = *u;
  result->qreal = *v;
}

  /* I8 to X32 */

void __qcmplx1_k(qcomplex *result, long long *u)
{
  result->qreal = *u;
  result->qimag = 0.0L;
}

void __qcmplx2_k(qcomplex *result, long long *u, long long *v)
{
  result->qreal = *u;
  result->qreal = *v;
}

  /* R4 to X32 */

void __qcmplx1_r(qcomplex *result, float *u)
{
  result->qreal = *u;
  result->qimag = 0.0L;
}

void __qcmplx2_r(qcomplex *result, float *u, float *v)
{
  result->qreal = *u;
  result->qimag = *v;
}

  /* R8 to X32 */

void __qcmplx1_d(qcomplex *result, double *u)
{
  result->qreal = *u;
  result->qimag = 0.0L;
}

void __qcmplx2_d(qcomplex *result, double *u, double *v)
{
  result->qreal = *u;
  result->qimag = *v;
}

  /* R16 to X32 */

void __qcmplx1_q(qcomplex *result, long double *u)
{
  result->qreal = *u;
  result->qimag = 0.0L;
}

void __qcmplx2_q(qcomplex *result, long double *u, long double *v)
{
  result->qreal = *u;
  result->qimag = *v;
}

  /* X8 to X32 */

void __qcmplx_c(qcomplex *result, complex *u)
{
  result->qreal = u->real;
  result->qimag = u->imag;
}

  /* X16 to X32 */

void __qcmplx_z(qcomplex *result, dcomplex *u)
{
  result->qreal = u->dreal;
  result->qimag = u->dimag;
}

  /* X8 to R16 */

long double __q_c(complex *u)
{
  long double  __q_ext(float);

  return (__q_ext(u->real));
}

  /* X16 to R16 */

long double __q_z(dcomplex *u)
{
  long double  __q_extd(double);

  return (__q_extd(u->dreal));
}
