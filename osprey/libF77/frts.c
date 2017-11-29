/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
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


#include <stdarg.h>
#include <cmplrs/host.h>
#include "f77defs.h"

#ifdef MFEF77_C
	/* include these only if support for mfef77 ftn->c is needed */
#include <libftn.h>

int _con1 = 1;
int _con2 = 2;
int _con3 = 3;
int _con4 = 4;
int _con5 = 5;
int _con6 = 6;
int _con7 = 7;
int _con8 = 8;
int _con9 = 9;
int _con10 = 10;
int _con11 = 11;
int _con12 = 12;
int _con13 = 13;
int _con14 = 14;
int _con18 = 18;
int _con19 = 19;

long _lcon1 = 1;

int __jmax(int argcnt, ...)
{
  int arg, maxarg;
  va_list ap;

  va_start(ap, argcnt);
  maxarg = va_arg(ap, int);
  while (--argcnt > 0) {
    arg = va_arg(ap, int);
    if (arg > maxarg) maxarg = arg;
  }  /* while */
  va_end(ap);
  return maxarg;
}  /* __jmax */

long long __kmax(int argcnt, ...)
{
  long long arg, maxarg;
  va_list ap;

  va_start(ap, argcnt);
  maxarg = va_arg(ap, long long);
  while (--argcnt > 0) {
    arg = va_arg(ap, long long);
    if (arg > maxarg) maxarg = arg;
  }  /* while */
  va_end(ap);
  return maxarg;
}  /* __kmax */

float __rmax(int argcnt, ...)
{
  float  arg, maxarg;
  va_list ap;

  va_start(ap, argcnt);
  maxarg = va_arg(ap, double);
  while (--argcnt > 0) {
    arg = va_arg(ap, double);
    if (arg > maxarg) maxarg = arg;
  }  /* while */
  va_end(ap);
  return maxarg;
}  /* __rmax */

double __dmax(int argcnt, ...)
{
  double  arg, maxarg;
  va_list ap;

  va_start(ap, argcnt);
  maxarg = va_arg(ap, double);
  while (--argcnt > 0) {
    arg = va_arg(ap, double);
    if (arg > maxarg) maxarg = arg;
  }  /* while */
  va_end(ap);
  return maxarg;
}  /* __dmax */

float __ajmax0(int argcnt, ...)
{
  int arg, maxarg;
  va_list ap;

  va_start(ap, argcnt);
  maxarg = va_arg(ap, int);
  while (--argcnt > 0) {
    arg = va_arg(ap, int);
    if (arg > maxarg) maxarg = arg;
  }  /* while */
  va_end(ap);
  return (float)maxarg;
}  /* __ajmax0 */

float __akmax0(int argcnt, ...)
{
  long long arg, maxarg;
  va_list ap;

  va_start(ap, argcnt);
  maxarg = va_arg(ap, long long);
  while (--argcnt > 0) {
    arg = va_arg(ap, long long);
    if (arg > maxarg) maxarg = arg;
  }  /* while */
  va_end(ap);
  return (float)maxarg;
}  /* __akmax0 */

int __jmax1(int argcnt, ...)
{
  float  arg, maxarg;
  va_list ap;

  va_start(ap, argcnt);
  maxarg = va_arg(ap, double);
  while (--argcnt > 0) {
    arg = va_arg(ap, double);
    if (arg > maxarg) maxarg = arg;
  }  /* while */
  va_end(ap);
  return (int)maxarg;
}  /* __jmax1 */

long long __kmax1(int argcnt, ...)
{
  float  arg, maxarg;
  va_list ap;

  va_start(ap, argcnt);
  maxarg = va_arg(ap, double);
  while (--argcnt > 0) {
    arg = va_arg(ap, double);
    if (arg > maxarg) maxarg = arg;
  }  /* while */
  va_end(ap);
  return (long long)maxarg;
}  /* __kmax1 */

int __jmin(int argcnt, ...)
{
  int arg, minarg;
  va_list ap;

  va_start(ap, argcnt);
  minarg = va_arg(ap, int);
  while (--argcnt > 0) {
    arg = va_arg(ap, int);
    if (arg < minarg) minarg = arg;
  }  /* while */
  va_end(ap);
  return minarg;
}  /* __jmin */

long long __kmin(int argcnt, ...)
{
  long long arg, minarg;
  va_list ap;

  va_start(ap, argcnt);
  minarg = va_arg(ap, long long);
  while (--argcnt > 0) {
    arg = va_arg(ap, long long);
    if (arg < minarg) minarg = arg;
  }  /* while */
  va_end(ap);
  return minarg;
}  /* __kmin */

float __rmin(int argcnt, ...)
{
  float  arg, minarg;
  va_list ap;

  va_start(ap, argcnt);
  minarg = va_arg(ap, double);
  while (--argcnt > 0) {
    arg = va_arg(ap, double);
    if (arg < minarg) minarg = arg;
  }  /* while */
  va_end(ap);
  return minarg;
}  /* __rmin */

double __dmin(int argcnt, ...)
{
  double  arg, minarg;
  va_list ap;

  va_start(ap, argcnt);
  minarg = va_arg(ap, double);
  while (--argcnt > 0) {
    arg = va_arg(ap, double);
    if (arg < minarg) minarg = arg;
  }  /* while */
  va_end(ap);
  return minarg;
}  /* __dmin */

float __ajmin0(int argcnt, ...)
{
  int arg, minarg;
  va_list ap;

  va_start(ap, argcnt);
  minarg = va_arg(ap, int);
  while (--argcnt > 0) {
    arg = va_arg(ap, int);
    if (arg < minarg) minarg = arg;
  }  /* while */
  va_end(ap);
  return (float)minarg;
}  /* __ajmin0 */

float __akmin0(int argcnt, ...)
{
  long long arg, minarg;
  va_list ap;

  va_start(ap, argcnt);
  minarg = va_arg(ap, long long);
  while (--argcnt > 0) {
    arg = va_arg(ap, long long);
    if (arg < minarg) minarg = arg;
  }  /* while */
  va_end(ap);
  return (float)minarg;
}  /* __akmin0 */

int __jmin1(int argcnt, ...)
{
  float  arg, minarg;
  va_list ap;

  va_start(ap, argcnt);
  minarg = va_arg(ap, double);
  while (--argcnt > 0) {
    arg = va_arg(ap, double);
    if (arg < minarg) minarg = arg;
  }  /* while */
  va_end(ap);
  return (int)minarg;
}  /* __jmin1 */

long long __kmin1(int argcnt, ...)
{
  float  arg, minarg;
  va_list ap;

  va_start(ap, argcnt);
  minarg = va_arg(ap, double);
  while (--argcnt > 0) {
    arg = va_arg(ap, double);
    if (arg < minarg) minarg = arg;
  }  /* while */
  va_end(ap);
  return (long long)minarg;
}  /* __kmin1 */

/* Character concatenation. */
char *_concat(char *dp, int dl, int *dlp, char *ap, int al, char *bp, int bl)
/* (dp, dl) = (ap, al) // (dp, bl).  Length stored in *dlp. */
{
  char *p = dp;
  *dlp = al + bl;
  if (*dlp > dl) {
    fprintf(stderr, "Overflow in _concat.\n");
    exit(1);
  }  /* if */
  while (al-- > 0) *p++ = *ap++;
  while (bl-- > 0) *p++ = *bp++;
  return dp;
}  /* _concat */

/* Complex number constructors: */
struct _cpx_float _cpx_float(float r, float i)
{
  struct _cpx_float t;
  t.r = r;
  t.i = i;
  return t;
}  /* _cpx_float */

struct _cpx_float _cpx_make_float_from_double(struct _cpx_double d)
{
  struct _cpx_float t;
  t.r = d.r;
  t.i = d.i;
  return t;
}  /* _cpx_make_float_from_double */

struct _cpx_double _cpx_double(double r, double i)
{
  struct _cpx_double t;
  t.r = r;
  t.i = i;
  return t;
}  /* _cpx_double */

struct _cpx_double _cpx_make_double_from_float(struct _cpx_float f)
{
  struct _cpx_double t;
  t.r = f.r;
  t.i = f.i;
  return t;
}  /* _cpx_make_double_from_float */

struct _cpx_float _xnegate_float(struct _cpx_float a)
{
  struct _cpx_float t;
  t.r = -a.r;
  t.i = -a.i;
  return t;
}  /* _xnegate_float */

struct _cpx_double _xnegate_double(struct _cpx_double a)
{
  struct _cpx_double t;
  t.r = -a.r;
  t.i = -a.i;
  return t;
}  /* _xnegate_double */

struct _cpx_float _xadd_float(struct _cpx_float a, struct _cpx_float b)
{
  struct _cpx_float t;
  t.r = a.r + b.r;
  t.i = a.i + b.i;
  return t;
}  /* _xadd_float */

struct _cpx_double _xadd_double(struct _cpx_double a, struct _cpx_double b)
{
  struct _cpx_double t;
  t.r = a.r + b.r;
  t.i = a.i + b.i;
  return t;
}  /* _xadd_double */

struct _cpx_float _xsubtract_float(struct _cpx_float a, struct _cpx_float b)
{
  struct _cpx_float t;
  t.r = a.r - b.r;
  t.i = a.i - b.i;
  return t;
}  /* _xsubtract_float */

struct _cpx_double _xsubtract_double(struct _cpx_double a, struct _cpx_double b)
{
  struct _cpx_double t;
  t.r = a.r - b.r;
  t.i = a.i - b.i;
  return t;
}  /* _xsubtract_double */

struct _cpx_float _xmultiply_float(struct _cpx_float a, struct _cpx_float b)
{
  struct _cpx_float t;
  t.r = a.r*b.r - a.i*b.i;
  t.i = a.i*b.r + a.r*b.i;
  return t;
}  /* _xmultiply_float */

struct _cpx_double _xmultiply_double(struct _cpx_double a, struct _cpx_double b)
{
  struct _cpx_double t;
  t.r = a.r*b.r - a.i*b.i;
  t.i = a.i*b.r + a.r*b.i;
  return t;
}  /* _xmultiply_double */

struct _cpx_float _xdivide_float(struct _cpx_float a, struct _cpx_float b)
{
  struct _cpx_float t;
  float d = b.r*b.r+b.i*b.i;
  t.r = (a.r*b.r+a.i*b.i)/d;
  t.i = (a.i*b.r-a.r*b.i)/d;
  return t;
}  /* _xdivide_float */

struct _cpx_double _xdivide_double(struct _cpx_double a, struct _cpx_double b)
{
  struct _cpx_double t;
  double d = b.r*b.r+b.i*b.i;
  t.r = (a.r*b.r+a.i*b.i)/d;
  t.i = (a.i*b.r-a.r*b.i)/d;
  return t;
}  /* _xdivide_double */

int _xeq_float(struct _cpx_float a, struct _cpx_float b)
{
  return a.r == b.r && a.i == b.i;
}  /* _xeq_float */

int _xeq_double(struct _cpx_double a, struct _cpx_double b)
{
  return a.r == b.r && a.i == b.i;
}  /* _xeq_double */

int _xne_float(struct _cpx_float a, struct _cpx_float b)
{
  return a.r != b.r || a.i != b.i;
}  /* _xne_float */

int _xne_double(struct _cpx_double a, struct _cpx_double b)
{
  return a.r != b.r || a.i != b.i;
}  /* _xne_double */

/* Hack alert:  The complex exponentation functions take a pointer to the
   result as the first parameter.  Since c_gen_be.c doesn't realize it's
   a complex exponentiation until it's already put out the left hand side
   of the assignment statement, it was easiest to just code these stub
   routines to call the real functions. */

struct _cpx_float pow_ci_stub(struct _cpx_float *a, int *b)
{
struct _cpx_float temp;
pow_ci(&temp,a,b);
return temp;
}

struct _cpx_float pow_cl_stub(struct _cpx_float *a, long long *b)
{
struct _cpx_float temp;
pow_cl(&temp,a,b);
return temp;
}

struct _cpx_float pow_cc_stub(struct _cpx_float *a, struct _cpx_float *b)
{
struct _cpx_float temp1;
complex temp2;
temp2 = __powcc(a->r, a->i, b->r, b->i);
temp1.r = temp2.real;
temp1.i = temp2.imag;
return temp1;
}

struct _cpx_double pow_zi_stub(struct _cpx_double *a, int *b)
{
struct _cpx_double temp;
pow_zi_(&temp,a,b);
return temp;
}

struct _cpx_double pow_zl_stub(struct _cpx_double *a, long long *b)
{
struct _cpx_double temp;
pow_zl_(&temp,a,b);
return temp;
}

struct _cpx_double pow_zz_stub(struct _cpx_double *a, struct _cpx_double *b)
{
struct _cpx_double temp;
pow_zz(&temp,a,b);
return temp;
}

#endif /* MFEF77_C */

/* Routines for RSHIFT.  No error checking is done. */

int8 rshft_b(int8 *m, int8 *k)
{
  int8 l;
  l = -*k;
  return(shft_b(m, &l));
}

int16 rshft_h(int16 *m, int16 *k)
{
  int16 l;
  l = -*k;
  return(shft_h(m, &l));
}

int32 rshft_l(int32 *m, int32 *k)
{
  int32 l;
  l = -*k;
  return(shft_l(m, &l));
}

int64 rshft_ll(int64 *m, int64 *k)
{
  int64 l;
  l = -*k;
  return(shft_ll(m, &l));
}
