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


#ifndef __QUAD_H__
#define __QUAD_H__

#if _COMPILER_VERSION != 400
#include <stdio.h>
#include <stdlib.h>
#endif

#include <inttypes.h>
#include "cmplx.h"
#include "qcmplx.h"

typedef double double_t;

typedef	struct {
double	hi;
double	lo;
} quad;

typedef union {
	long double ld;
	quad q;
} ldquad;

extern	long double	__libm_zero_ld;
extern	long double	__libm_qnan_ld;
extern	long double	__libm_inf_ld;
extern	long double	__libm_neginf_ld;

extern	int32_t	*__errnoaddr;

extern double __lgamma(double);
extern double __trunc(double);
extern double __exp(double);
extern double d_acosd (double *);
extern double d_asind (double *);
extern double d_atand (double *);
extern double d_atn2d (double *, double *);
extern double d_cosd (double *);
extern double d_sind (double *);
extern double d_tand (double *);
extern double_t pow_di(double_t *, int32_t *);
extern double_t pow_dl(double_t *, int64_t *);
extern void pow_zi_(dcomplex *, dcomplex *, int32_t *);
extern void pow_zl_(dcomplex *, dcomplex *, int64_t *);
extern void pow_zz(dcomplex *, dcomplex *, dcomplex *);
extern double_t z_abs(dcomplex *);
extern void z_cos(dcomplex *, dcomplex *);
extern void z_exp(dcomplex *, dcomplex *);
extern void z_log(dcomplex *, dcomplex *);
extern void z_sin(dcomplex *, dcomplex *);
extern void z_sqrt(dcomplex *, dcomplex *);
extern int32_t __q_ge (double, double, double, double);
extern int32_t __q_le (double, double, double, double);
extern long double __qabs(long double);
extern long double __qmod(long double, long double);
extern long double __q_nint(long double *);
extern int16_t __ii_qint(double, double);
extern int32_t __ji_qint(double, double);
extern int16_t __iiqnnt(double, double);
extern int32_t __jiqnnt(double, double);
extern int64_t __kiqnnt(double, double);
extern long double __q_flotk(int64_t);
extern long double __qint(double, double);
extern long double __q_max1(double, double, double, double);
extern long double __q_min1(double, double, double, double);
extern long double __qsign(double, double, double, double);
extern long double __qnint(double, double);
extern long double __qacos(long double);
extern long double __qatan(long double);
extern long double __qatan2(long double, long double);
extern long double __qasin(long double);
extern long double __qcos(long double);
extern long double __qcosh(long double);
extern long double __qdim(double, double, double, double);
extern long double __qerf(long double);
extern long double __qerfc(long double);
extern long double __qexp(long double);
extern long double __qhypot(long double, long double);
extern long double __qlog10(long double);
extern long double __qlog(long double);
extern long double __qpow(long double, long double);
extern long double __qprod(double, double);
extern long double __qsin(long double);
extern long double __qsinh(long double);
extern long double __qsqrt(long double);
extern long double __qtan(long double);
extern long double __qtanh(long double);
extern long double __q_sub(double xhi, double xlo, double yhi, double ylo);
extern long double __q_add(double xhi, double xlo, double yhi, double ylo);
extern void __cq_conjg (qcomplex *, qcomplex *);
extern void __cq_mul (qcomplex *, qcomplex *, qcomplex *);

#endif /* !__QUAD_H__ */

