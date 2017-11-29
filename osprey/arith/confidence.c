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


/*
 *	Arithmetic library low-level and high-level confidence tests
 */

#include <stdio.h>
#include <string.h>
#include <signal.h>
#ifdef _CRAY
#include <complex.h>
extern float RANF ();
#else
#include <stdlib.h>
#endif
#include "arith.h"
#include "arith.internal.h"

#define RAND_ITERS	10000

#define	FLAGMASK	(AR_STAT_OVERFLOW | AR_STAT_UNDEFINED | \
			 AR_STAT_ZERO | AR_STAT_NEGATIVE)

#define	AR_ERR		(AR_STAT_OVERFLOW | AR_STAT_UNDEFINED)


void fadd (float *z, float *a, float *b) { *z = *a + *b; }
void fsub (float *z, float *a, float *b) { *z = *a - *b; }
void fmul (float *z, float *a, float *b) { *z = *a * *b; }
void fdiv (float *z, float *a, float *b) { *z = *a / *b; }

#if _CRAY
void dfadd (long double *z, long double *a, long double *b) { *z = *a + *b; }
void dfsub (long double *z, long double *a, long double *b) { *z = *a - *b; }
void dfmul (long double *z, long double *a, long double *b) { *z = *a * *b; }
void dfdiv (long double *z, long double *a, long double *b) { *z = *a / *b; }
void cadd (double complex *z, double complex *a, double complex *b) {
	*z = *a + *b;
}
void csub (double complex *z, double complex *a, double complex *b) {
	*z = *a - *b;
}
void cmul (double complex *z, double complex *a, double complex *b) {
	*z = *a * *b;
}
void cdiv (double complex *z, double complex *a, double complex *b) {
#if _ADDR32
	if (!cimag (*b)) {
		/* zero imaginary part - expect different answers (sigh) */
		*z = (creal (*a) / creal (*b)) +
			0.0i * (cimag (*a) / creal (*b));
	} else
#endif
		*z = *a / *b;
}

#else

void xdfadd (long double *z, long double *a, long double *b) { *z = *a + *b; }
void xdfsub (long double *z, long double *a, long double *b) { *z = *a - *b; }
void xdfmul (long double *z, long double *a, long double *b) { *z = *a * *b; }
void xdfdiv (long double *z, long double *a, long double *b) { *z = *a / *b; }

void dfadd (double *z, double *a, double *b) { *z = *a + *b; }
void dfsub (double *z, double *a, double *b) { *z = *a - *b; }
void dfmul (double *z, double *a, double *b) { *z = *a * *b; }
void dfdiv (double *z, double *a, double *b) { *z = *a / *b; }

#endif


/* Trap floating-point exceptions and set a flag */
static volatile int fp_error = 0;
void
fperr (int sig) {

	fp_error = 1;
	signal (SIGFPE, fperr);

}

#if _CRAY
extern void ARFMULT (float *, float *, float *);
extern void ARRMULT (float *, float *, float *);
extern void ARIMULT (float *, float *, float *);
extern void ARQMULT (float *, float *, float *);
extern void ARHRECIP (float *, float *);

void
trapmathlibabort (void) {
	fp_error = 1;
}
#else

/* Prevent printing of math library error messages on Suns */
int
matherr () {
	return 1;
}

#endif

#if _CRAY || _SPARC
extern void ARSQRT (float *, float *);
extern void ARDSQRT (long double *, long double *);
extern void ARLOG (float *, float *);
extern void ARDLOG (long double *, long double *);
extern void AREXP (float *, float *);
extern void ARDEXP (long double *, long double *);
#endif


/* Input routines */

int
getnewline (FILE *fp) {
	int ch;
	while ((ch = fgetc (fp)) == ' ' || ch == '\n' || ch == '\t') ;
	if (ch == '$' && fgetc (fp) == 't')
		return 1;
	return 0;
}

int
getcray64 (FILE *fp, AR_CRAY_64 *x) {
	int ct;
	unsigned long xsign, xexpo, xc [4];
	ct = fscanf (fp, " %1lo %5lo %1lo%5lo%5lo%5lo",
		     &xsign, &xexpo, xc + 0, xc + 1, xc + 2, xc + 3);
	x->sign = xsign;
	x->expo = xexpo;
	x->coeff0 = (xc [0] << 13) | (xc [1] >> 2);
	x->coeff1 = (xc [1] << 14) | (xc [2] >> 1);
	x->coeff2 = (xc [2] << 15) | xc [3];
	return ct == 6;
}

int
getcray128 (FILE *fp, AR_CRAY_128 *x) {
	int ct;
	unsigned long xsign, xexpo, xzero, xc [8];
	ct = fscanf (fp, " %1lo%5lo%1lo%5lo%5lo%5lo %6lo%1lo%5lo%5lo%5lo",
		     &xsign, &xexpo, xc + 0, xc + 1, xc + 2, xc + 3,
			&xzero, xc + 4, xc + 5, xc + 6, xc + 7);
	x->sign = xsign;
	x->expo = xexpo;
	x->coeff0 = (xc [0] << 13) | (xc [1] >> 2);
	x->coeff1 = (xc [1] << 14) | (xc [2] >> 1);
	x->coeff2 = (xc [2] << 15) | xc [3];
	x->zero = xzero;
	x->coeff3 = (xc [4] << 13) | (xc [5] >> 2);
	x->coeff4 = (xc [5] << 14) | (xc [6] >> 1);
	x->coeff5 = (xc [6] << 15) | xc [7];
	return ct == 11;
}

int
getieee32 (FILE *fp, AR_IEEE_32 *x) {
	int ct;
	unsigned long xsign, xexpo, xc [2];
	ct = fscanf (fp, " %1lo%3lo%3lo%4lo",
		     &xsign, &xexpo, xc + 0, xc + 1);
	x->sign = xsign >> 1;
	x->expo = (xsign << 7) | (xexpo >> 2);
	x->coeff0 = (xexpo << 5) | (xc [0] >> 4);
	x->coeff1 = (xc [0] << 12) | xc [1];
	return ct == 4;
}

int
getieee64 (FILE *fp, AR_IEEE_64 *x) {
	int ct;
	unsigned long xsign, xexpo, xc [4];
	ct = fscanf (fp, " %1lo%5lo%1lo%5lo%5lo%5lo",
		     &xsign, &xexpo, xc + 0, xc + 1, xc + 2, xc + 3);
	x->sign = xsign;
	x->expo = xexpo >> 4;
	x->coeff0 = xexpo;
	x->coeff1 = (xc [0] << 13) | (xc [1] >> 2);
	x->coeff2 = (xc [1] << 14) | (xc [2] >> 1);
	x->coeff3 = (xc [2] << 15) | xc [3];
	return ct == 6;
}

int
getint64 (FILE *fp, AR_INT_64 *x) {
	int ct;
	unsigned long xc [5];
	ct = fscanf (fp, " %6lo%1lo%5lo%5lo%5lo",
		     xc + 0, xc + 1, xc + 2, xc + 3, xc + 4);
	x->part1 = xc [0];
	x->part2 = (xc [1] << 13) | (xc [2] >> 2);
	x->part3 = (xc [2] << 14) | (xc [3] >> 1);
	x->part4 = (xc [3] << 15) | xc [4];
	return ct == 5;
}

int
getint (FILE *fp, int *x) {
	return fscanf (fp, " %o", x) == 1;
}

int
getcray64_2 (FILE *fp,
	     AR_CRAY_64 *x,
	     AR_CRAY_64 *y,
	     AR_CRAY_64 *res,
	     int *ansflags) {
	return getnewline (fp) &&
	       getcray64 (fp, x) &&
	       getcray64 (fp, y) &&
	       getcray64 (fp, res) &&
	       getint (fp, ansflags);
}

int
getcray64_1 (FILE *fp,
	     AR_CRAY_64 *x,
	     AR_CRAY_64 *res,
	     int *ansflags) {
	return getnewline (fp) &&
	       getcray64 (fp, x) &&
	       getcray64 (fp, res) &&
	       getint (fp, ansflags);
}

int
getcray64_i (FILE *fp,
	     AR_CRAY_64 *x,
	     AR_INT_64 *y,
	     AR_CRAY_64 *res,
	     int *ansflags) {
	return getnewline (fp) &&
	       getcray64 (fp, x) &&
	       getint64 (fp, y) &&
	       getcray64 (fp, res) &&
	       getint (fp, ansflags);
}

int
getcray64_i2 (FILE *fp,
	      AR_INT_64 *x,
	      AR_INT_64 *y,
	      AR_INT_64 *res,
	      int *ansflags) {
	return getnewline (fp) &&
	       getint64 (fp, x) &&
	       getint64 (fp, y) &&
	       getint64 (fp, res) &&
	       getint (fp, ansflags);
}

int
getcray128_2 (FILE *fp,
	      AR_CRAY_128 *x,
	      AR_CRAY_128 *y,
	      AR_CRAY_128 *res,
	      int *ansflags) {
	return getnewline (fp) &&
	       getcray128 (fp, x) &&
	       getcray128 (fp, y) &&
	       getcray128 (fp, res) &&
	       getint (fp, ansflags);
}

int
getcray128_1 (FILE *fp,
	      AR_CRAY_128 *x,
	      AR_CRAY_128 *res,
	      int *ansflags) {
	return getnewline (fp) &&
	       getcray128 (fp, x) &&
	       getcray128 (fp, res) &&
	       getint (fp, ansflags);
}

int
getcray128_1a (FILE *fp,
	       AR_INT_64 *x,
	       AR_CRAY_128 *res,
	       int *ansflags) {
	return getnewline (fp) &&
	       getint64 (fp, x) &&
	       getcray128 (fp, res) &&
	       getint (fp, ansflags);
}

int
getcray128_1b (FILE *fp,
	       AR_CRAY_128 *x,
	       AR_INT_64 *res,
	       int *ansflags) {
	return getnewline (fp) &&
	       getcray128 (fp, x) &&
	       getint64 (fp, res) &&
	       getint (fp, ansflags);
}

int
getcray128_1c (FILE *fp,
	       AR_CRAY_128 *x,
	       AR_IEEE_64 *res,
	       int *ansflags) {
	return getnewline (fp) &&
	       getcray128 (fp, x) &&
	       getieee64 (fp, res) &&
	       getint (fp, ansflags);
}

int
getcray128_i (FILE *fp,
	      AR_CRAY_128 *x,
	      AR_INT_64 *y,
	      AR_CRAY_128 *res,
	      int *ansflags) {
	return getnewline (fp) &&
	       getcray128 (fp, x) &&
	       getint64 (fp, y) &&
	       getcray128 (fp, res) &&
	       getint (fp, ansflags);
}

int
getcray128_64 (FILE *fp,
	       AR_CRAY_128 *x,
	       AR_CRAY_64 *y,
	       AR_CRAY_128 *res,
	       int *ansflags) {
	return getnewline (fp) &&
	       getcray128 (fp, x) &&
	       getcray64 (fp, y) &&
	       getcray128 (fp, res) &&
	       getint (fp, ansflags);
}

int
getieee64_2 (FILE *fp,
	     AR_IEEE_64 *x,
	     AR_IEEE_64 *y,
	     AR_IEEE_64 *res,
	     int *ansflags) {
	return getnewline (fp) &&
	       getieee64 (fp, x) &&
	       getieee64 (fp, y) &&
	       getieee64 (fp, res) &&
	       getint (fp, ansflags);
}

int
getieee64_2c (FILE *fp,
	     AR_CPLX_IEEE_64 *x,
	     AR_CPLX_IEEE_64 *y,
	     AR_CPLX_IEEE_64 *res,
	     int *ansflags) {
	return getnewline (fp) &&
	       getieee64 (fp, &x->real) &&
	       getieee64 (fp, &x->imag) &&
	       getieee64 (fp, &y->real) &&
	       getieee64 (fp, &y->imag) &&
	       getieee64 (fp, &res->real) &&
	       getieee64 (fp, &res->imag) &&
	       getint (fp, ansflags);
}

int
getieee64_1 (FILE *fp,
	     AR_IEEE_64 *x,
	     AR_IEEE_64 *res,
	     int *ansflags) {
	return getnewline (fp) &&
	       getieee64 (fp, x) &&
	       getieee64 (fp, res) &&
	       getint (fp, ansflags);
}

int
getieee32_2 (FILE *fp,
	     AR_IEEE_32 *x,
	     AR_IEEE_32 *y,
	     AR_IEEE_32 *res,
	     int *ansflags) {
	return getnewline (fp) &&
	       getieee32 (fp, x) &&
	       getieee32 (fp, y) &&
	       getieee32 (fp, res) &&
	       getint (fp, ansflags);
}

int
getieee32_2c (FILE *fp,
	     AR_CPLX_IEEE_32 *x,
	     AR_CPLX_IEEE_32 *y,
	     AR_CPLX_IEEE_32 *res,
	     int *ansflags) {
	     int status;
	     AR_IEEE_32 f;
	status  = getnewline (fp);
	status |= getieee32 (fp, &f);
		  x->rsign = f.sign;
		  x->rexpo = f.expo;
		  x->rcoeff0 = f.coeff0;
		  x->rcoeff1 = f.coeff1;
	status |= getieee32 (fp, &f);
		  x->isign = f.sign;
		  x->iexpo = f.expo;
		  x->icoeff0 = f.coeff0;
		  x->icoeff1 = f.coeff1;
	status |= getieee32 (fp, &f);
		  y->rsign = f.sign;
		  y->rexpo = f.expo;
		  y->rcoeff0 = f.coeff0;
		  y->rcoeff1 = f.coeff1;
	status |= getieee32 (fp, &f);
		  y->isign = f.sign;
		  y->iexpo = f.expo;
		  y->icoeff0 = f.coeff0;
		  y->icoeff1 = f.coeff1;
	status |= getieee32 (fp, &f);
		  res->rsign = f.sign;
		  res->rexpo = f.expo;
		  res->rcoeff0 = f.coeff0;
		  res->rcoeff1 = f.coeff1;
	status |= getieee32 (fp, &f);
		  res->isign = f.sign;
		  res->iexpo = f.expo;
		  res->icoeff0 = f.coeff0;
		  res->icoeff1 = f.coeff1;
	return status && getint (fp, ansflags);
}

int
getieee32_1 (FILE *fp,
	     AR_IEEE_32 *x,
	     AR_IEEE_32 *res,
	     int *ansflags) {
	return getnewline (fp) &&
	       getieee32 (fp, x) &&
	       getieee32 (fp, res) &&
	       getint (fp, ansflags);
}

int
getint64_1 (FILE *fp,
	    AR_INT_64 *x,
	    AR_INT_64 *res,
	    int *ansflags) {
	return getnewline (fp) &&
	       getint64 (fp, x) &&
	       getint64 (fp, res) &&
	       getint (fp, ansflags);
}


/* Result checking */

int
crayok64 (AR_CRAY_64 *ans,
	  AR_CRAY_64 *res,
	  int ansflags,
	  int resflags) {

	if (ansflags & AR_ERR)
		return ((resflags ^ ansflags) & AR_ERR) == 0;
	if ((resflags ^ ansflags) & FLAGMASK)
		return 0;
	return res->sign == ans->sign &&
	       res->expo == ans->expo &&
	       res->coeff0 == ans->coeff0 &&
	       res->coeff1 == ans->coeff1 &&
	       res->coeff2 == ans->coeff2;
}

int
crayok64i (AR_INT_64 *ans,
	   AR_INT_64 *res,
	   int ansflags,
	   int resflags) {

	if (ansflags & AR_ERR)
		return ((resflags ^ ansflags) & AR_ERR) == 0;
	if ((resflags ^ ansflags) & FLAGMASK)
		return 0;
	return res->part1 == ans->part1 &&
	       res->part2 == ans->part2 &&
	       res->part3 == ans->part3 &&
	       res->part4 == ans->part4;
}

int
crayok128 (AR_CRAY_128 *ans,
	   AR_CRAY_128 *res,
	   int ansflags,
	   int resflags) {

	if (ansflags & AR_ERR)
		return ((resflags ^ ansflags) & AR_ERR) == 0;
	if ((resflags ^ ansflags) & FLAGMASK)
		return 0;
	return res->sign == ans->sign &&
	       res->expo == ans->expo &&
	       res->coeff0 == ans->coeff0 &&
	       res->coeff1 == ans->coeff1 &&
	       res->coeff2 == ans->coeff2 &&
	       res->coeff3 == ans->coeff3 &&
	       res->coeff4 == ans->coeff4 &&
	       res->coeff5 == ans->coeff5;
}

int
ieeeok64 (AR_IEEE_64 *ans,
	  AR_IEEE_64 *res,
	  int ansflags,
	  int resflags) {

	if (ansflags & AR_ERR)
		return ((resflags ^ ansflags) & AR_ERR) == 0;
	if ((resflags ^ ansflags) & FLAGMASK)
		return 0;
	return res->sign == ans->sign &&
	       res->expo == ans->expo &&
	       res->coeff0 == ans->coeff0 &&
	       res->coeff1 == ans->coeff1 &&
	       res->coeff2 == ans->coeff2 &&
	       res->coeff3 == ans->coeff3;
}

int
ieeeok32 (AR_IEEE_32 *ans,
	  AR_IEEE_32 *res,
	  int ansflags,
	  int resflags) {

	if (ansflags & AR_ERR)
		return ((resflags ^ ansflags) & AR_ERR) == 0;
	if ((resflags ^ ansflags) & FLAGMASK)
		return 0;
	return res->sign == ans->sign &&
	       res->expo == ans->expo &&
	       res->coeff0 == ans->coeff0 &&
	       res->coeff1 == ans->coeff1;
}

int
intok64 (AR_INT_64 *ans,
	 AR_INT_64 *res,
	 int ansflags,
	 int resflags) {

	if (ansflags & AR_ERR)
		return ((resflags ^ ansflags) & AR_ERR) == 0;
	if ((resflags ^ ansflags) & FLAGMASK)
		return 0;
	return res->part1 == ans->part1 &&
	       res->part2 == ans->part2 &&
	       res->part3 == ans->part3 &&
	       res->part4 == ans->part4;
}


/* Error messages */

prcray64 (AR_CRAY_64 *x) {
	printf ("%o%05o%o%05o%05o%05o",
		x->sign, x->expo, x->coeff0 >> 13,
		MASKR (15) & ((x->coeff0 << 2) | (x->coeff1 >> 14)),
		MASKR (15) & ((x->coeff1 << 1) | (x->coeff2 >> 15)),
		MASKR (15) & x->coeff2);
}

prcray128 (AR_CRAY_128 *x) {
	printf ("%o%05o%o%05o%05o%05o %o%05o%05o%05o",
		x->sign, x->expo, x->coeff0 >> 13,
		MASKR (15) & ((x->coeff0 << 2) | (x->coeff1 >> 14)),
		MASKR (15) & ((x->coeff1 << 1) | (x->coeff2 >> 15)),
		MASKR (15) & x->coeff2,
		x->coeff3 >> 13,
		MASKR (15) & ((x->coeff3 << 2) | (x->coeff4 >> 14)),
		MASKR (15) & ((x->coeff4 << 1) | (x->coeff5 >> 15)),
		MASKR (15) & x->coeff5);
}

print64 (AR_INT_64 *x) {
	printf ("%06o%o%05o%05o%05o",
		x->part1,
		x->part2 >> 13,
		MASKR (15) & ((x->part2 << 2) | (x->part3 >> 14)),
		MASKR (15) & ((x->part3 << 1) | (x->part4 >> 15)),
		MASKR (15) & x->part4);
}

prieee64 (AR_IEEE_64 *x) {
	printf ("%o%05o%o%05o%05o%05o",
		x->sign, (x->expo << 4) | x->coeff0, x->coeff1 >> 13,
		MASKR (15) & ((x->coeff1 << 2) | (x->coeff2 >> 14)),
		MASKR (15) & ((x->coeff2 << 1) | (x->coeff3 >> 15)),
		MASKR (15) & x->coeff3);
}

prieee32 (AR_IEEE_32 *x) {
	printf ("%o%03o%02o%05o",
		(x->sign << 1) | (x->expo >> 7),
		MASKR (9) & ((x->expo << 2) | (x->coeff0 >> 5)),
		MASKR (6) & ((x->coeff0 << 1) | (x->coeff1 >> 15)),
		MASKR (15) & x->coeff1);
}

crayerr64_2 (char *lab,
	     AR_CRAY_64 *x,
	     AR_CRAY_64 *y,
	     AR_CRAY_64 *ans,
	     AR_CRAY_64 *res,
	     int ansflags,
	     int resflags) {
	printf ("%s (", lab);
	prcray64 (x);
	printf (", ");
	prcray64 (y);
	printf (")\n\t-> ");
	prcray64 (res);
	printf ("\n\t!= ");
	prcray64 (ans);
	printf (" (%o, %o)\n", ansflags, resflags);
}

crayerr64_1 (char *lab,
	     AR_CRAY_64 *x,
	     AR_CRAY_64 *ans,
	     AR_CRAY_64 *res,
	     int ansflags,
	     int resflags) {
	printf ("%s (", lab);
	prcray64 (x);
	printf (")\n\t-> ");
	prcray64 (res);
	printf ("\n\t!= ");
	prcray64 (ans);
	printf (" (%o, %o)\n", ansflags, resflags);
}

crayerr64_i (char *lab,
	     AR_CRAY_64 *x,
	     AR_INT_64 *y,
	     AR_CRAY_64 *ans,
	     AR_CRAY_64 *res,
	     int ansflags,
	     int resflags) {
	printf ("%s (", lab);
	prcray64 (x);
	printf (", ");
	print64 (y);
	printf (")\n\t-> ");
	prcray64 (res);
	printf ("\n\t!= ");
	prcray64 (ans);
	printf (" (%o, %o)\n", ansflags, resflags);
}

crayerr64_i2 (char *lab,
	      AR_INT_64 *x,
	      AR_INT_64 *y,
	      AR_INT_64 *ans,
	      AR_INT_64 *res,
	      int ansflags,
	      int resflags) {
	printf ("%s (", lab);
	print64 (x);
	printf (", ");
	print64 (y);
	printf (")\n\t-> ");
	print64 (res);
	printf ("\n\t!= ");
	print64 (ans);
	printf (" (%o, %o)\n", ansflags, resflags);
}

crayerr128_2 (char *lab,
	      AR_CRAY_128 *x,
	      AR_CRAY_128 *y,
	      AR_CRAY_128 *ans,
	      AR_CRAY_128 *res,
	      int ansflags,
	      int resflags) {
	printf ("%s (", lab);
	prcray128 (x);
	printf (", ");
	prcray128 (y);
	printf (")\n\t-> ");
	prcray128 (res);
	printf ("\n\t!= ");
	prcray128 (ans);
	printf (" (%o, %o)\n", ansflags, resflags);
}

crayerr128_1 (char *lab,
	      AR_CRAY_128 *x,
	      AR_CRAY_128 *ans,
	      AR_CRAY_128 *res,
	      int ansflags,
	      int resflags) {
	printf ("%s (", lab);
	prcray128 (x);
	printf (")\n\t-> ");
	prcray128 (res);
	printf ("\n\t!= ");
	prcray128 (ans);
	printf (" (%o, %o)\n", ansflags, resflags);
}

crayerr128_1a (char *lab,
	       AR_INT_64 *x,
	       AR_CRAY_128 *ans,
	       AR_CRAY_128 *res,
	       int ansflags,
	       int resflags) {
	printf ("%s (", lab);
	print64 (x);
	printf (")\n\t-> ");
	prcray128 (res);
	printf ("\n\t!= ");
	prcray128 (ans);
	printf (" (%o, %o)\n", ansflags, resflags);
}

crayerr128_1b (char *lab,
	       AR_CRAY_128 *x,
	       AR_INT_64 *ans,
	       AR_INT_64 *res,
	       int ansflags,
	       int resflags) {
	printf ("%s (", lab);
	prcray128 (x);
	printf (")\n\t-> ");
	print64 (res);
	printf ("\n\t!= ");
	print64 (ans);
	printf (" (%o, %o)\n", ansflags, resflags);
}

crayerr128_1c (char *lab,
	       AR_CRAY_128 *x,
	       AR_IEEE_64 *ans,
	       AR_IEEE_64 *res,
	       int ansflags,
	       int resflags) {
	printf ("%s (", lab);
	prcray128 (x);
	printf (")\n\t-> ");
	prieee64 (res);
	printf ("\n\t!= ");
	prieee64 (ans);
	printf (" (%o, %o)\n", ansflags, resflags);
}

crayerr128_i (char *lab,
	      AR_CRAY_128 *x,
	      AR_INT_64 *y,
	      AR_CRAY_128 *ans,
	      AR_CRAY_128 *res,
	      int ansflags,
	      int resflags) {
	printf ("%s (", lab);
	prcray128 (x);
	printf (", ");
	print64 (y);
	printf (")\n\t-> ");
	prcray128 (res);
	printf ("\n\t!= ");
	prcray128 (ans);
	printf (" (%o, %o)\n", ansflags, resflags);
}

crayerr128_64 (char *lab,
	       AR_CRAY_128 *x,
	       AR_CRAY_64 *y,
	       AR_CRAY_128 *ans,
	       AR_CRAY_128 *res,
	       int ansflags,
	       int resflags) {
	printf ("%s (", lab);
	prcray128 (x);
	printf (", ");
	prcray64 (y);
	printf (")\n\t-> ");
	prcray128 (res);
	printf ("\n\t!= ");
	prcray128 (ans);
	printf (" (%o, %o)\n", ansflags, resflags);
}

ieeeerr64_2 (char *lab,
	     AR_IEEE_64 *x,
	     AR_IEEE_64 *y,
	     AR_IEEE_64 *ans,
	     AR_IEEE_64 *res,
	     int ansflags,
	     int resflags) {
	printf ("%s (", lab);
	prieee64 (x);
	printf (", ");
	prieee64 (y);
	printf (")\n\t-> ");
	prieee64 (res);
	printf ("\n\t!= ");
	prieee64 (ans);
	printf (" (%o, %o)\n", ansflags, resflags);
}

ieeeerr64_2c (char *lab,
	     AR_CPLX_IEEE_64 *x,
	     AR_CPLX_IEEE_64 *y,
	     AR_CPLX_IEEE_64 *ans,
	     AR_CPLX_IEEE_64 *res,
	     int ansflags,
	     int resflags) {
	printf ("%s (", lab);
	prieee64 (&x->real);
	prieee64 (&x->imag);
	printf (", ");
	prieee64 (&y->real);
	prieee64 (&y->imag);
	printf (")\n\t-> ");
	prieee64 (&res->real);
	prieee64 (&res->imag);
	printf ("\n\t!= ");
	prieee64 (&ans->real);
	prieee64 (&ans->imag);
	printf (" (%o, %o)\n", ansflags, resflags);
}

ieeeerr64_1 (char *lab,
	     AR_IEEE_64 *x,
	     AR_IEEE_64 *ans,
	     AR_IEEE_64 *res,
	     int ansflags,
	     int resflags) {
	printf ("%s (", lab);
	prieee64 (x);
	printf (")\n\t-> ");
	prieee64 (res);
	printf ("\n\t!= ");
	prieee64 (ans);
	printf (" (%o, %o)\n", ansflags, resflags);
}

ieeeerr32_2 (char *lab,
	     AR_IEEE_32 *x,
	     AR_IEEE_32 *y,
	     AR_IEEE_32 *ans,
	     AR_IEEE_32 *res,
	     int ansflags,
	     int resflags) {
	printf ("%s (", lab);
	prieee32 (x);
	printf (", ");
	prieee32 (y);
	printf (")\n\t-> ");
	prieee32 (res);
	printf ("\n\t!= ");
	prieee32 (ans);
	printf (" (%o, %o)\n", ansflags, resflags);
}

ieeeerr32_2c (char *lab,
	     AR_CPLX_IEEE_32 *x,
	     AR_CPLX_IEEE_32 *y,
	     AR_CPLX_IEEE_32 *ans,
	     AR_CPLX_IEEE_32 *res,
	     int ansflags,
	     int resflags) {
	AR_IEEE_32 f;
	printf ("%s (", lab);
	f.sign = x->rsign;
	f.expo = x->rexpo;
	f.coeff0 = x->rcoeff0;
	f.coeff1 = x->rcoeff1;
	prieee32 (&f);
	printf (" ");
	f.sign = x->isign;
	f.expo = x->iexpo;
	f.coeff0 = x->icoeff0;
	f.coeff1 = x->icoeff1;
	prieee32 (&f);
	printf (", ");
	f.sign = y->rsign;
	f.expo = y->rexpo;
	f.coeff0 = y->rcoeff0;
	f.coeff1 = y->rcoeff1;
	prieee32 (&f);
	printf (" ");
	f.sign = y->isign;
	f.expo = y->iexpo;
	f.coeff0 = y->icoeff0;
	f.coeff1 = y->icoeff1;
	prieee32 (&f);
	printf (")\n\t-> ");
	f.sign = res->rsign;
	f.expo = res->rexpo;
	f.coeff0 = res->rcoeff0;
	f.coeff1 = res->rcoeff1;
	prieee32 (&f);
	printf (" ");
	f.sign = res->isign;
	f.expo = res->iexpo;
	f.coeff0 = res->icoeff0;
	f.coeff1 = res->icoeff1;
	prieee32 (&f);
	printf ("\n\t!= ");
	f.sign = ans->rsign;
	f.expo = ans->rexpo;
	f.coeff0 = ans->rcoeff0;
	f.coeff1 = ans->rcoeff1;
	prieee32 (&f);
	printf (" ");
	f.sign = ans->isign;
	f.expo = ans->iexpo;
	f.coeff0 = ans->icoeff0;
	f.coeff1 = ans->icoeff1;
	prieee32 (&f);
	printf (" (%o, %o)\n", ansflags, resflags);
}

ieeeerr32_1 (char *lab,
	     AR_IEEE_32 *x,
	     AR_IEEE_32 *ans,
	     AR_IEEE_32 *res,
	     int ansflags,
	     int resflags) {
	printf ("%s (", lab);
	prieee32 (x);
	printf (")\n\t-> ");
	prieee32 (res);
	printf ("\n\t!= ");
	prieee32 (ans);
	printf (" (%o, %o)\n", ansflags, resflags);
}

interr64_1 (char *lab,
	    AR_INT_64 *x,
	    AR_INT_64 *ans,
	    AR_INT_64 *res,
	    int ansflags,
	    int resflags) {
	printf ("%s (", lab);
	print64 (x);
	printf (")\n\t-> ");
	print64 (res);
	printf ("\n\t!= ");
	print64 (ans);
	printf (" (%o, %o)\n", ansflags, resflags);
}


/* Drivers */

craytest64_2 (char *fn,
	      char *lab,
	      int (*op) (),
	      int rounding,
	      int *tests,
	      int *failures) {

	int ansflags, resflags, i;
	AR_CRAY_64 res, x, y, ans;
	FILE *fp;

	if (fp = fopen (fn, "r")) {
		for (i = 0; getcray64_2 (fp, &x, &y, &ans, &ansflags); i++) {
			if(rounding < 0)
				resflags = op (&res, &x, &y);
			else
				resflags = op (&res, &x, &y, rounding);
			if (!crayok64 (&ans, &res, ansflags, resflags)) {
				++*failures;
				crayerr64_2 (lab, &x, &y, &ans, &res,
						ansflags, resflags);
			}
			++*tests;
		}
		printf ("end %d tests of %s from %s\n", i, lab, fn);
		fclose (fp);
	} else {
		printf ("can't open %s to test %s\n", fn, lab);
		/* ++*failures; */
	}
}

craytest64_2t (char *fn,
	       char *lab,
	       int (*op) (),
	       AR_TYPE type,
	       int *tests,
	       int *failures) {

	int ansflags, resflags, i;
	AR_CRAY_64 res, x, y, ans;
	FILE *fp;

	if (fp = fopen (fn, "r")) {
		for (i = 0; getcray64_2 (fp, &x, &y, &ans, &ansflags); i++) {
			resflags = op (&res, &type, &x, &type, &y, &type);
			if (!crayok64 (&ans, &res, ansflags, resflags)) {
				++*failures;
				crayerr64_2 (lab, &x, &y, &ans, &res,
						ansflags, resflags);
			}
			++*tests;
		}
		printf ("end %d tests of %s from %s\n", i, lab, fn);
		fclose (fp);
	} else {
		printf ("can't open %s to test %s\n", fn, lab);
		/* ++*failures; */
	}
}

craytest64_1 (char *fn,
	      char *lab,
	      int (*op) (),
	      int rounding,
	      int *tests,
	      int *failures) {

	int ansflags, resflags, i;
	AR_CRAY_64 res, x, ans;
	FILE *fp;

	if (fp = fopen (fn, "r")) {
		for (i = 0; getcray64_1 (fp, &x, &ans, &ansflags); i++) {
			resflags = op (&res, &x, rounding);
			if (!crayok64 (&ans, &res, ansflags, resflags)) {
				++*failures;
				crayerr64_1 (lab, &x, &ans, &res,
					     ansflags, resflags);
			}
			++*tests;
		}
		printf ("end %d tests of %s from %s\n", i, lab, fn);
		fclose (fp);
	} else {
		printf ("can't open %s to test %s\n", fn, lab);
		/* ++*failures; */
	}
}

craytest64_1t (char *fn,
	       char *lab,
	       int (*op) (),
	       AR_TYPE type,
	       int *tests,
	       int *failures) {

	int ansflags, resflags, i;
	AR_CRAY_64 res, x, ans;
	FILE *fp;

	if (fp = fopen (fn, "r")) {
		for (i = 0; getcray64_1 (fp, &x, &ans, &ansflags); i++) {
			resflags = op (&res, &type, &x, &type);
			if (!crayok64 (&ans, &res, ansflags, resflags)) {
				++*failures;
				crayerr64_1 (lab, &x, &ans, &res,
					     ansflags, resflags);
			}
			++*tests;
		}
		printf ("end %d tests of %s from %s\n", i, lab, fn);
		fclose (fp);
	} else {
		printf ("can't open %s to test %s\n", fn, lab);
		/* ++*failures; */
	}
}

craytest64_c (char *fn,
	      char *lab,
	      int (*op) (),
	      AR_TYPE type1,
	      AR_TYPE type2,
	      int *tests,
	      int *failures) {

	int ansflags, resflags, i;
	AR_CPLX_CRAY_64 x;
	AR_CRAY_64 res, ans;
	FILE *fp;

	if (fp = fopen (fn, "r")) {
		for (i = 0; getcray64_2 (fp, &x.real, &x.imag, &ans, &ansflags); i++) {
			resflags = op (&res, &type2, &x, &type1);
			if (!crayok64 (&ans, &res, ansflags, resflags)) {
				++*failures;
				crayerr64_2 (lab, &x.real, &x.imag, &ans, &res,
					     ansflags, resflags);
			}
			++*tests;
		}
		printf ("end %d tests of %s from %s\n", i, lab, fn);
		fclose (fp);
	} else {
		printf ("can't open %s to test %s\n", fn, lab);
		/* ++*failures; */
	}
}

craytest64_i (char *fn,
	      char *lab,
	      int (*op) (),
	      AR_TYPE type,
	      int *tests,
	      int *failures) {

	int ansflags, resflags, i;
	AR_CRAY_64 res, x, ans;
	AR_INT_64 y;
	AR_TYPE inttype = AR_Int_64_S;
	FILE *fp;

	if (fp = fopen (fn, "r")) {
		for (i = 0; getcray64_i (fp, &x, &y, &ans, &ansflags); i++) {
			resflags = op (&res, &type, &x, &type, &y, &inttype);
			if (!crayok64 (&ans, &res, ansflags, resflags)) {
				++*failures;
				crayerr64_i (lab, &x, &y, &ans, &res,
						ansflags, resflags);
			}
			++*tests;
		}
		printf ("end %d tests of %s from %s\n", i, lab, fn);
		fclose (fp);
	} else {
		printf ("can't open %s to test %s\n", fn, lab);
		/* ++*failures; */
	}
}

craytest64_i2 (char *fn,
	       char *lab,
	       int (*op) (),
	       AR_TYPE type,
	       int *tests,
	       int *failures) {

	int ansflags, resflags, i;
	AR_INT_64 res, x, y, ans;
	FILE *fp;

	if (fp = fopen (fn, "r")) {
		for (i = 0; getcray64_i2 (fp, &x, &y, &ans, &ansflags); i++) {
			resflags = op (&res, &type, &x, &type, &y, &type);
			if (!crayok64i (&ans, &res, ansflags, resflags)) {
				++*failures;
				crayerr64_i2 (lab, &x, &y, &ans, &res,
						ansflags, resflags);
			}
			++*tests;
		}
		printf ("end %d tests of %s from %s\n", i, lab, fn);
		fclose (fp);
	} else {
		printf ("can't open %s to test %s\n", fn, lab);
		/* ++*failures; */
	}
}

craytest128_64 (char *fn,
	        char *lab,
	        int (*op) (),
	        AR_TYPE type1,
	        AR_TYPE type2,
	        int *tests,
	        int *failures) {

	int ansflags, resflags, i;
	AR_CRAY_128 res, x, ans;
	AR_CRAY_64 y;
	FILE *fp;

	if (fp = fopen (fn, "r")) {
		for (i = 0; getcray128_64 (fp, &x, &y, &ans, &ansflags); i++) {
			resflags = op (&res, &type1, &x, &type1, &y, &type2);
			if (!crayok128 (&ans, &res, ansflags, resflags)) {
				++*failures;
				crayerr128_64 (lab, &x, &y, &ans, &res,
						ansflags, resflags);
			}
			++*tests;
		}
		printf ("end %d tests of %s from %s\n", i, lab, fn);
		fclose (fp);
	} else {
		printf ("can't open %s to test %s\n", fn, lab);
		/* ++*failures; */
	}
}

craytest128_2t (char *fn,
	        char *lab,
	        int (*op) (),
	        AR_TYPE type,
	        int *tests,
	        int *failures) {

	int ansflags, resflags, i;
	AR_CRAY_128 res, x, y, ans;
	FILE *fp;

	if (fp = fopen (fn, "r")) {
		for (i = 0; getcray128_2 (fp, &x, &y, &ans, &ansflags); i++) {
			resflags = op (&res, &type, &x, &type, &y, &type);
			if (!crayok128 (&ans, &res, ansflags, resflags)) {
				++*failures;
				crayerr128_2 (lab, &x, &y, &ans, &res,
						ansflags, resflags);
			}
			++*tests;
		}
		printf ("end %d tests of %s from %s\n", i, lab, fn);
		fclose (fp);
	} else {
		printf ("can't open %s to test %s\n", fn, lab);
		/* ++*failures; */
	}
}

craytest128_1 (char *fn,
	       char *lab,
	       int (*op) (),
	       int rounding,
	       int *tests,
	       int *failures) {

	int ansflags, resflags, i;
	AR_CRAY_128 res, x, ans;
	FILE *fp;

	if (fp = fopen (fn, "r")) {
		for (i = 0; getcray128_1 (fp, &x, &ans, &ansflags); i++) {
			resflags = op (&res, &x, rounding);
			if (!crayok128 (&ans, &res, ansflags, resflags)) {
				++*failures;
				crayerr128_1 (lab, &x, &ans, &res,
						ansflags, resflags);
			}
			++*tests;
		}
		printf ("end %d tests of %s from %s\n", i, lab, fn);
		fclose (fp);
	} else {
		printf ("can't open %s to test %s\n", fn, lab);
		/* ++*failures; */
	}
}

craytest128_1t (char *fn,
	        char *lab,
	        int (*op) (),
	        AR_TYPE type,
	        int *tests,
	        int *failures) {

	int ansflags, resflags, i;
	AR_CRAY_128 res, x, ans;
	FILE *fp;

	if (fp = fopen (fn, "r")) {
		for (i = 0; getcray128_1 (fp, &x, &ans, &ansflags); i++) {
			resflags = op (&res, &type, &x, &type);
			if (!crayok128 (&ans, &res, ansflags, resflags)) {
				++*failures;
				crayerr128_1 (lab, &x, &ans, &res,
						ansflags, resflags);
			}
			++*tests;
		}
		printf ("end %d tests of %s from %s\n", i, lab, fn);
		fclose (fp);
	} else {
		printf ("can't open %s to test %s\n", fn, lab);
		/* ++*failures; */
	}
}

craytest128_i (char *fn,
	       char *lab,
	       int (*op) (),
	       AR_TYPE type,
	       int *tests,
	       int *failures) {

	int ansflags, resflags, i;
	AR_CRAY_128 res, x, ans;
	AR_INT_64 y;
	AR_TYPE inttype = AR_Int_64_S;
	FILE *fp;

	if (fp = fopen (fn, "r")) {
		for (i = 0; getcray128_i (fp, &x, &y, &ans, &ansflags); i++) {
			resflags = op (&res, &type, &x, &type, &y, &inttype);
			if (!crayok128 (&ans, &res, ansflags, resflags)) {
				++*failures;
				crayerr128_i (lab, &x, &y, &ans, &res,
						ansflags, resflags);
			}
			++*tests;
		}
		printf ("end %d tests of %s from %s\n", i, lab, fn);
		fclose (fp);
	} else {
		printf ("can't open %s to test %s\n", fn, lab);
		/* ++*failures; */
	}
}

ieeetest64_2c (char *fn,
	       char *lab,
	       int (*op) (),
	       AR_TYPE type,
	       int *tests,
	       int *failures) {

	int ansflags, resflags, i;
	AR_CPLX_IEEE_64 res, x, y, ans;
	FILE *fp;

	if (fp = fopen (fn, "r")) {
		for (i = 0; getieee64_2c (fp, &x, &y, &ans, &ansflags); i++) {
			resflags = op (&res, &type, &x, &type, &y, &type);
			if (!ieeeok64(&ans.real, &res.real, ansflags,resflags) ||
			    !ieeeok64(&ans.imag, &res.imag, ansflags,resflags)) {
				++*failures;
				ieeeerr64_2c (lab, &x, &y, &ans, &res,
						ansflags, resflags);
			}
			++*tests;
		}
		printf ("end %d tests of %s from %s\n", i, lab, fn);
		fclose (fp);
	} else {
		printf ("can't open %s to test %s\n", fn, lab);
		/* ++*failures; */
	}
}

ieeetest64_2cmp (char *fn,
	       char *lab,
	       AR_COMPARE_TYPE (*op) (),
	       AR_TYPE type,
	       int *tests,
	       int *failures) {

	int ansflags, resflags, i;
	AR_IEEE_64 x, y, ans;
	FILE *fp;

	if (fp = fopen (fn, "r")) {
		for (i = 0; getieee64_2 (fp, &x, &y, &ans, &ansflags); i++) {
			resflags = op (&x, &type, &y, &type);
			switch (resflags) {
			case AR_Compare_LT:
				resflags = AR_STAT_NEGATIVE;
				break;
			case AR_Compare_EQ:
				resflags = AR_STAT_ZERO;
				break;
			case AR_Compare_GT:
				resflags = AR_STAT_OK;
				break;
			case AR_Compare_Unord:
				resflags = AR_STAT_UNDEFINED;
				break;
			}
			if (!ieeeok64 (&ans, &ans, ansflags, resflags)) {
				++*failures;
				ieeeerr64_2 (lab, &x, &y, &ans, &ans,
						ansflags, resflags);
			}
			++*tests;
		}
		printf ("end %d tests of %s from %s\n", i, lab, fn);
		fclose (fp);
	} else {
		printf ("can't open %s to test %s\n", fn, lab);
		/* ++*failures; */
	}
}

ieeetest64_2t (char *fn,
	       char *lab,
	       int (*op) (),
	       AR_TYPE type,
	       int *tests,
	       int *failures) {

	int ansflags, resflags, i;
	AR_IEEE_64 res, x, y, ans;
	FILE *fp;

	if (fp = fopen (fn, "r")) {
		for (i = 0; getieee64_2 (fp, &x, &y, &ans, &ansflags); i++) {
			resflags = op (&res, &type, &x, &type, &y, &type);
			if (!ieeeok64 (&ans, &res, ansflags, resflags)) {
				++*failures;
				ieeeerr64_2 (lab, &x, &y, &ans, &res,
						ansflags, resflags);
			}
			++*tests;
		}
		printf ("end %d tests of %s from %s\n", i, lab, fn);
		fclose (fp);
	} else {
		printf ("can't open %s to test %s\n", fn, lab);
		/* ++*failures; */
	}
}

ieeetest64_1t (char *fn,
	       char *lab,
	       int (*op) (),
	       AR_TYPE type,
	       int *tests,
	       int *failures) {

	int ansflags, resflags, i;
	AR_IEEE_64 res, x, ans;
	FILE *fp;

	if (fp = fopen (fn, "r")) {
		for (i = 0; getieee64_1 (fp, &x, &ans, &ansflags); i++) {
			resflags = op (&res, &type, &x, &type);
			if (!ieeeok64 (&ans, &res, ansflags, resflags)) {
				++*failures;
				ieeeerr64_1 (lab, &x, &ans, &res,
						ansflags, resflags);
			}
			++*tests;
		}
		printf ("end %d tests of %s from %s\n", i, lab, fn);
		fclose (fp);
	} else {
		printf ("can't open %s to test %s\n", fn, lab);
		/* ++*failures; */
	}
}

ieeetest32_2c (char *fn,
	       char *lab,
	       int (*op) (),
	       AR_TYPE type,
	       int *tests,
	       int *failures) {

	int ansflags, resflags, i;
	AR_CPLX_IEEE_32 res, x, y, ans;
	FILE *fp;

	if (fp = fopen (fn, "r")) {
		for (i = 0; getieee32_2c (fp, &x, &y, &ans, &ansflags); i++) {
			resflags = op (&res, &type, &x, &type, &y, &type);
			if (!ieeeok64((AR_IEEE_64*)&ans, (AR_IEEE_64*)&res, ansflags, resflags)) {
				++*failures;
				ieeeerr32_2c (lab, &x, &y, &ans, &res,
						ansflags, resflags);
			}
			++*tests;
		}
		printf ("end %d tests of %s from %s\n", i, lab, fn);
		fclose (fp);
	} else {
		printf ("can't open %s to test %s\n", fn, lab);
		/* ++*failures; */
	}
}

ieeetest32_2cmp (char *fn,
	       char *lab,
	       AR_COMPARE_TYPE (*op) (),
	       AR_TYPE type,
	       int *tests,
	       int *failures) {

	int ansflags, resflags, i;
	AR_IEEE_32 x, y, ans;
	FILE *fp;

	if (fp = fopen (fn, "r")) {
		for (i = 0; getieee32_2 (fp, &x, &y, &ans, &ansflags); i++) {
			resflags = op (&x, &type, &y, &type);
			switch (resflags) {
			case AR_Compare_LT:
				resflags = AR_STAT_NEGATIVE;
				break;
			case AR_Compare_EQ:
				resflags = AR_STAT_ZERO;
				break;
			case AR_Compare_GT:
				resflags = AR_STAT_OK;
				break;
			case AR_Compare_Unord:
				resflags = AR_STAT_UNDEFINED;
				break;
			}
			if (!ieeeok32 (&ans, &ans, ansflags, resflags)) {
				++*failures;
				ieeeerr32_2 (lab, &x, &y, &ans, &ans,
						ansflags, resflags);
			}
			++*tests;
		}
		printf ("end %d tests of %s from %s\n", i, lab, fn);
		fclose (fp);
	} else {
		printf ("can't open %s to test %s\n", fn, lab);
		/* ++*failures; */
	}
}

ieeetest32_2t (char *fn,
	       char *lab,
	       int (*op) (),
	       AR_TYPE type,
	       int *tests,
	       int *failures) {

	int ansflags, resflags, i;
	AR_IEEE_32 res, x, y, ans;
	FILE *fp;

	if (fp = fopen (fn, "r")) {
		for (i = 0; getieee32_2 (fp, &x, &y, &ans, &ansflags); i++) {
			resflags = op (&res, &type, &x, &type, &y, &type);
			if (!ieeeok32 (&ans, &res, ansflags, resflags)) {
				++*failures;
				ieeeerr32_2 (lab, &x, &y, &ans, &res,
						ansflags, resflags);
			}
			++*tests;
		}
		printf ("end %d tests of %s from %s\n", i, lab, fn);
		fclose (fp);
	} else {
		printf ("can't open %s to test %s\n", fn, lab);
		/* ++*failures; */
	}
}

ieeetest32_1t (char *fn,
	       char *lab,
	       int (*op) (),
	       AR_TYPE type,
	       int *tests,
	       int *failures) {

	int ansflags, resflags, i;
	AR_IEEE_32 res, x, ans;
	FILE *fp;

	if (fp = fopen (fn, "r")) {
		for (i = 0; getieee32_1 (fp, &x, &ans, &ansflags); i++) {
			resflags = op (&res, &type, &x, &type);
			if (!ieeeok32 (&ans, &res, ansflags, resflags)) {
				++*failures;
				ieeeerr32_1 (lab, &x, &ans, &res,
						ansflags, resflags);
			}
			++*tests;
		}
		printf ("end %d tests of %s from %s\n", i, lab, fn);
		fclose (fp);
	} else {
		printf ("can't open %s to test %s\n", fn, lab);
		/* ++*failures; */
	}
}


/* Random testing with native hardware */

#ifdef _CRAY

crayrand64_2 (char *lab,
	      int (*simop) (),
	      int rounding,
	      void (*realop) (),
	      int *tests,
	      int *lowp,
	      int *highp) {

	int i, simflags, realflags, low = 0, high = 0;
	float x, y, sim, real;

	for (i = 0; i < RAND_ITERS; i++) {
		x = (RANF()-0.5)*i*100;
		y = (RANF()-0.5)*i*100;
		fp_error = 0;
		realop (&real, &x, &y);
		realflags = !!fp_error;
		if(rounding < 0)
			simflags = simop (&sim, &x, &y);
		else
			simflags = simop (&sim, &x, &y, rounding);
		if (sim != real)
			printf ("rand test of %s (%022o, %022o)\n\t->%022o\n\t!=%022o (%d, %d)\n",
				lab, x, y, sim, real, realflags, simflags);
		if (sim < real)
			low++;
		else if (sim != real)
			high++;
		++*tests;
	}

	printf ("end %d random tests of %s: %d results low, %d high\n",
		i, lab, low, high);
	*lowp += low;
	*highp += high;
}

crayrand64_1 (char *lab,
	      int (*simop) (),
	      int rounding,
	      void (*realop) (),
	      int *tests,
	      int *lowp,
	      int *highp) {

	int i, simflags, realflags, low = 0, high = 0;
	float x, sim, real;

	for (i = 0; i < RAND_ITERS; i++) {
		x = (RANF()-0.5)*i*100;
		fp_error = 0;
		realop (&real, &x);
		realflags = !!fp_error;
		simflags = simop (&sim, &x, rounding);
		if (sim != real)
			printf ("rand test of %s (%022o)\n\t->%022o\n\t!=%022o (%d, %d)\n",
				lab, x, sim, real, realflags, simflags);
		if (sim < real)
			low++;
		else if (sim != real)
			high++;
		++*tests;
	}

	printf ("end %d random tests of %s: %d results low, %d high\n",
		i, lab, low, high);
	*lowp += low;
	*highp += high;
}

crayrand128_2 (char *lab,
	       int (*simop) (),
	       int rounding,
	       void (*realop) (),
	       int *tests,
	       int *lowp,
	       int *highp) {

	int i, simflags, realflags, low = 0, high = 0;
	long double x, y, sim, real;

	for (i = 0; i < RAND_ITERS; i++) {
		x = (long double) (RANF()-0.5)*i*100 -
			(long double) RANF () * 1.0e-19;
		y = (long double) (RANF()-0.5)*i*100 +
			(long double) RANF () * 1.0e-19;
		fp_error = 0;
		realop (&real, &x, &y);
		realflags = !!fp_error;
		if(rounding < 0)
			simflags = simop (&sim, &x, &y);
		else
			simflags = simop (&sim, &x, &y, rounding);
		if (sim < real)
			low++;
		else if (sim != real)
			high++;
		++*tests;
	}

	printf ("end %d random tests of %s: %d results low, %d high\n",
		i, lab, low, high);
	*lowp += low;
	*highp += high;
}

crayrandcplx (char *lab,
	      int (*simop) (),
	      AR_TYPE cplxtype,
	      void (*realop) (),
	      int *tests,
	      int *badp) {

	int i, simflags, realflags, bad = 0;
	double complex x, y, sim, real;

	for (i = 0; i < RAND_ITERS; i++) {
		x = (RANF()-0.5)*i*100;
		y = (RANF()-0.5)*i*100;
		if (i) {
			x += 0.0i * RANF ();
			y += 0.0i * RANF ();
		}
		fp_error = 0;
		realop (&real, &x, &y);
		realflags = !!fp_error;
		simflags = simop (&sim, &cplxtype,
				  &x, &cplxtype,
				  &y, &cplxtype);
		if (sim != real ||
		    realflags != !!(simflags & AR_ERR)) {
			printf ("rand test of %s ((%022o,%022o),(%022o,%022o))\n\t->(%022o,%022o)\n\t!=(%022o,%022o) (%d, %d)\n",
				lab, x, y, sim, real, realflags, simflags);
			bad++;
		}
		++*tests;
	}

	printf ("end %d random tests of %s: %d results bad\n", i, lab, bad);
	*badp += bad;
}


#else


ieeerand128_2 (char *lab,
	      int (*simop) (),
	      int rounding,
	      void (*realop) (),
	      int *tests,
	      int *lowp,
	      int *highp) {

	int i, simflags, realflags, low = 0, high = 0;
	long double x, y, sim, real;

	for (i = 0; i < RAND_ITERS; i++) {
		x = (long double)((drand48()-0.5)*i*100 - 1.0e-19 * drand48());
		y = (long double)((drand48()-0.5)*i*100 + 1.0e-19 * drand48());
		fp_error = 0;
		realop (&real, &x, &y);
		realflags = !!fp_error;
		simflags = simop (&sim, &x, &y, rounding);
		if (sim != real) {
			printf ("rand test of %s\n\t(%011lo %011lo %011lo %011lo,\n\t %011lo %011lo %011lo %011lo)\n\t->%011lo %011lo %011lo %011lo\n\t!=%011lo %011lo %011lo %011lo (%d, %d)\n",
				lab, x, y, sim, real, realflags, simflags);
			if (sim < real)
				low++;
			else
				high++;
		}
		++*tests;
	}

	printf ("end %d random tests of %s: %d results low, %d high\n",
		i, lab, low, high);
	*lowp += low;
	*highp += high;
}

ieeerand64_2 (char *lab,
	      int (*simop) (),
	      int rounding,
	      void (*realop) (),
	      int *tests,
	      int *lowp,
	      int *highp) {

	int i, simflags, realflags, low = 0, high = 0;
	double x, y, sim, real;

	for (i = 0; i < RAND_ITERS; i++) {
		x = (drand48()-0.5)*i*100 - 1.0e-19 * drand48 ();
		y = (drand48()-0.5)*i*100 + 1.0e-19 * drand48 ();
		fp_error = 0;
		realop (&real, &x, &y);
		realflags = !!fp_error;
		simflags = simop (&sim, &x, &y, rounding);
		if (sim != real)
			printf ("rand test of %s (%011lo %011lo, %011lo %011lo)\n\t->%011lo %011lo\n\t!=%011lo %011lo (%d, %d)\n",
				lab, x, y, sim, real, realflags, simflags);
		if (sim < real)
			low++;
		else if (sim != real)
			high++;
		++*tests;
	}

	printf ("end %d random tests of %s: %d results low, %d high\n",
		i, lab, low, high);
	*lowp += low;
	*highp += high;
}

ieeerand32_2 (char *lab,
	      int (*simop) (),
	      int rounding,
	      void (*realop) (),
	      int *tests,
	      int *lowp,
	      int *highp) {

	int i, simflags, realflags, low = 0, high = 0;
	struct floatstruct { float UNUSED; float f; };
	struct floatstruct x, y, sim, real;

	for (i = 0; i < RAND_ITERS; i++) {
		x.f = (drand48()-0.5)*i*100-1.0e-19;
		y.f = (drand48()-0.5)*i*100+1.0e-19;
		fp_error = 0;
		realop (&real.f, &x.f, &y.f);
		realflags = !!fp_error;
		simflags = simop (&sim, &x, &y, rounding);
		if (sim.f != real.f)
			printf ("rand test of %s (%011lo, %011lo)\n\t->%011lo\n\t!=%011lo (%d, %d)\n",
				lab, *(unsigned long *)&x.f,
				*(unsigned long *)&y.f, *(unsigned long *)&sim.f,
				*(unsigned long *)&real.f, realflags, simflags);
		if (sim.f < real.f)
			low++;
		else if (sim.f != real.f)
			high++;
		++*tests;
	}

	printf ("end %d random tests of %s: %d results low, %d high\n",
		i, lab, low, high);
	*lowp += low;
	*highp += high;
}

#endif


/* Conversion tests */

convtest64 (char *fn,
	    char *lab,
	    int (*op) (),
	    int opt1, int opt2,
	    int *tests,
	    int *failures) {

	int ansflags, resflags, i;
	AR_INT_64 x, ans, res;
	FILE *fp;

	if (fp = fopen (fn, "r")) {
		for (i = 0; getint64_1 (fp, &x, &ans, &ansflags); i++) {
			resflags = op (&res, &x, opt1, opt2);
			if (!intok64 (&ans, &res, ansflags, resflags)) {
				++*failures;
				interr64_1 (lab, &x, &ans, &res,
						ansflags, resflags);
			}
			++*tests;
		}
		printf ("end %d tests of %s from %s\n", i, lab, fn);
		fclose (fp);
	} else {
		printf ("can't open %s to test %s\n", fn, lab);
		/* ++*failures; */
	}
}

convtest128a (char *fn,
	      char *lab,
	      int (*op) (),
	      int opt,
	      int *tests,
	      int *failures) {

	int ansflags, resflags, i;
	AR_INT_64 x;
	AR_CRAY_128 ans, res;
	FILE *fp;

	if (fp = fopen (fn, "r")) {
		for (i = 0; getcray128_1a (fp, &x, &ans, &ansflags); i++) {
			resflags = op (&res, &x, opt);
			if (!crayok128 (&ans, &res, ansflags, resflags)) {
				++*failures;
				crayerr128_1a (lab, &x, &ans, &res,
						ansflags, resflags);
			}
			++*tests;
		}
		printf ("end %d tests of %s from %s\n", i, lab, fn);
		fclose (fp);
	} else {
		printf ("can't open %s to test %s\n", fn, lab);
		/* ++*failures; */
	}
}

convtest128b (char *fn,
	      char *lab,
	      int (*op) (),
	      int opt,
	      int *tests,
	      int *failures) {

	int ansflags, resflags, i;
	AR_CRAY_128 x;
	AR_INT_64 ans, res;
	FILE *fp;

	if (fp = fopen (fn, "r")) {
		for (i = 0; getcray128_1b (fp, &x, &ans, &ansflags); i++) {
			resflags = op (&res, &x, opt);
			if (!intok64 (&ans, &res, ansflags, resflags)) {
				++*failures;
				crayerr128_1b (lab, &x, &ans, &res,
						ansflags, resflags);
			}
			++*tests;
		}
		printf ("end %d tests of %s from %s\n", i, lab, fn);
		fclose (fp);
	} else {
		printf ("can't open %s to test %s\n", fn, lab);
		/* ++*failures; */
	}
}

convtest128c (char *fn,
	      char *lab,
	      int (*op) (),
	      int opt,
	      int *tests,
	      int *failures) {

	int ansflags, resflags, i;
	AR_CRAY_128 x;
	AR_IEEE_64 ans, res;
	FILE *fp;

	if (fp = fopen (fn, "r")) {
		for (i = 0; getcray128_1c (fp, &x, &ans, &ansflags); i++) {
			resflags = op (&res, &x, opt);
			if (!ieeeok64 (&ans, &res, ansflags, resflags)) {
				++*failures;
				crayerr128_1c (lab, &x, &ans, &res,
						ansflags, resflags);
			}
			++*tests;
		}
		printf ("end %d tests of %s from %s\n", i, lab, fn);
		fclose (fp);
	} else {
		printf ("can't open %s to test %s\n", fn, lab);
		/* ++*failures; */
	}
}

AR_TYPE  inttype;
static	misc_errors = 0;

misc_tests(int *failures) {
	ar_data result;
	ar_data precision = {0,0,0,12};
	ar_data range = {0,0,0,2000};

	/* Determine target's native integer size */
	inttype = AR_Int_32_S;
	if(AR_selected_real_kind((AR_DATA*)&result, &inttype,
							 (AR_DATA*)&precision, &inttype,
							 (AR_DATA*)&range, &inttype)&AR_STAT_INVALID_TYPE) {
		inttype = AR_Int_64_S;
		if(AR_selected_real_kind((AR_DATA*)&result, &inttype,
								 (AR_DATA*)&precision, &inttype,
								 (AR_DATA*)&range, &inttype) & AR_STAT_INVALID_TYPE) {
			fprintf(stderr, "Cannot determine target int type\n");
			failures++;
			return;
		}
		if(result.ar_i64.part4 == 0xfffe)
			printf("Target is a 64-bit IEEE system\n");
		else
			printf("Target is a 64-bit PVP system\n");
	}
	else
		printf("Target is a 32-bit sparc system\n");

	t_index();
	t_scan();
	t_verify();

	*failures += misc_errors;
}

t_index()
{
	char str1[] = "abcdefghijklmnopqrstuvwxyz";

	int	status;

	ar_data result;
	AR_DATA backward;

	ar_data len1 = {0,0,0,26};
	ar_data len2 = {0,0,0,0};

	AR_TYPE backtype = AR_Logical;

	printf("Testing AR_index\n");

	len2.ar_i64.part4 = 3;
	status = AR_index((AR_DATA*)&result, &inttype,
				str1, (AR_DATA*)&len1, &inttype,
			   "fgh", (AR_DATA*)&len2, &inttype,
				NULL, NULL);
	validate(status, 0, result.ar_i64.part4, 6);

	len2.ar_i64.part4 = 4;
	status = AR_index((AR_DATA*)&result, &inttype,
				str1, (AR_DATA*)&len1, &inttype,
			  "fgh0", (AR_DATA*)&len2, &inttype,
				NULL, NULL);
	validate(status, AR_STAT_ZERO, result.ar_i64.part4, 0);

	len2.ar_i64.part4 = 0;
	status = AR_index((AR_DATA*)&result, &inttype,
				str1, (AR_DATA*)&len1, &inttype,
				  "", (AR_DATA*)&len2, &inttype,
				NULL, NULL);
	validate(status, 0, result.ar_i64.part4, 1);

	len1.ar_i64.part4 = 0;
	len2.ar_i64.part4 = 3;
	status = AR_index((AR_DATA*)&result, &inttype,
				  "", (AR_DATA*)&len1, &inttype,
			   "fgh", (AR_DATA*)&len2, &inttype,
				NULL, NULL);
	validate(status, AR_STAT_ZERO, result.ar_i64.part4, 0);

	len2.ar_i64.part4 = 0;
	status = AR_index((AR_DATA*)&result, &inttype,
				  "", (AR_DATA*)&len1, &inttype,
				  "", (AR_DATA*)&len2, &inttype,
				NULL, NULL);
	validate(status, 0, result.ar_i64.part4, 1);

	len1.ar_i64.part4 = 26;
	len2.ar_i64.part4 = 1;
	status = AR_index((AR_DATA*)&result, &inttype,
				str1, (AR_DATA*)&len1, &inttype,
				 "z", (AR_DATA*)&len2, &inttype,
				NULL, NULL);
	validate(status, 0, result.ar_i64.part4, 26);

	len2.ar_i64.part4 = 1;
	status = AR_index((AR_DATA*)&result, &inttype,
				str1, (AR_DATA*)&len1, &inttype,
				 "a", (AR_DATA*)&len2, &inttype,
				NULL, NULL);
	validate(status, 0, result.ar_i64.part4, 1);

	backward = AR_const_false;
	len2.ar_i64.part4 = 2;
	status = AR_index((AR_DATA*)&result, &inttype,
				str1, (AR_DATA*)&len1, &inttype,
				"jk", (AR_DATA*)&len2, &inttype,
				(AR_DATA*)&backward, &backtype);
	validate(status, 0, result.ar_i64.part4, 10);

	backward = AR_const_true;
	len2.ar_i64.part4 = 2;
	status = AR_index((AR_DATA*)&result, &inttype,
				str1, (AR_DATA*)&len1, &inttype,
				"jk", (AR_DATA*)&len2, &inttype,
				(AR_DATA*)&backward, &backtype);
	validate(status, 0, result.ar_i64.part4, 10);

	backward = AR_const_true;
	len2.ar_i64.part4 = 1;
	status = AR_index((AR_DATA*)&result, &inttype,
				str1, (AR_DATA*)&len1, &inttype,
				 "x", (AR_DATA*)&len2, &inttype,
				(AR_DATA*)&backward, &backtype);
	validate(status, 0, result.ar_i64.part4, 24);

	backward = AR_const_true;
	len2.ar_i64.part4 = 3;
	status = AR_index((AR_DATA*)&result, &inttype,
				str1, (AR_DATA*)&len1, &inttype,
			   "jrk", (AR_DATA*)&len2, &inttype,
				(AR_DATA*)&backward, &backtype);
	validate(status, AR_STAT_ZERO, result.ar_i64.part4, 0);

	backward = AR_const_true;
	len2.ar_i64.part4 = 0;
	status = AR_index((AR_DATA*)&result, &inttype,
				str1, (AR_DATA*)&len1, &inttype,
				  "", (AR_DATA*)&len2, &inttype,
				(AR_DATA*)&backward, &backtype);
	validate(status, 0, result.ar_i64.part4, 27);

	backward = AR_const_true;
	len1.ar_i64.part4 = 0;
	len2.ar_i64.part4 = 0;
	status = AR_index((AR_DATA*)&result, &inttype,
				  "", (AR_DATA*)&len1, &inttype,
				  "", (AR_DATA*)&len2, &inttype,
				(AR_DATA*)&backward, &backtype);
	validate(status, 0, result.ar_i64.part4, 1);
}

t_scan()
{
	char str1[] = "abcdefghijklmnopqrstuvwxyz";

	int	status;

	ar_data result;
	AR_DATA backward;

	ar_data len1 = {0,0,0,26};
	ar_data len2 = {0,0,0,0};

	AR_TYPE backtype = AR_Logical;

	printf("Testing AR_scan\n");

	len2.ar_i64.part4 = 3;
	status = AR_scan((AR_DATA*)&result, &inttype,
			   str1, (AR_DATA*)&len1, &inttype,
			  "fgh", (AR_DATA*)&len2, &inttype,
				NULL, NULL);
	validate(status, 0, result.ar_i64.part4, 6);

	len2.ar_i64.part4 = 3;
	status = AR_scan((AR_DATA*)&result, &inttype,
			   str1, (AR_DATA*)&len1, &inttype,
			  "0.2", (AR_DATA*)&len2, &inttype,
				NULL, NULL);
	validate(status, AR_STAT_ZERO, result.ar_i64.part4, 0);

	len2.ar_i64.part4 = 0;
	status = AR_scan((AR_DATA*)&result, &inttype,
			   str1, (AR_DATA*)&len1, &inttype,
				"", (AR_DATA*)&len2, &inttype,
				NULL, NULL);
	validate(status, AR_STAT_ZERO, result.ar_i64.part4, 0);

	len1.ar_i64.part4 = 0;
	len2.ar_i64.part4 = 3;
	status = AR_scan((AR_DATA*)&result, &inttype,
				 "", (AR_DATA*)&len1, &inttype,
			  "fgh", (AR_DATA*)&len2, &inttype,
				NULL, NULL);
	validate(status, AR_STAT_ZERO, result.ar_i64.part4, 0);

	len2.ar_i64.part4 = 0;
	status = AR_scan((AR_DATA*)&result, &inttype,
				 "", (AR_DATA*)&len1, &inttype,
				 "", (AR_DATA*)&len2, &inttype,
				NULL, NULL);
	validate(status, AR_STAT_ZERO, result.ar_i64.part4, 0);

	len1.ar_i64.part4 = 26;
	len2.ar_i64.part4 = 1;
	status = AR_scan((AR_DATA*)&result, &inttype,
			   str1, (AR_DATA*)&len1, &inttype,
				"z", (AR_DATA*)&len2, &inttype,
				NULL, NULL);
	validate(status, 0, result.ar_i64.part4, 26);

	len2.ar_i64.part4 = 1;
	status = AR_scan((AR_DATA*)&result, &inttype,
			   str1, (AR_DATA*)&len1, &inttype,
				"a", (AR_DATA*)&len2, &inttype,
				NULL, NULL);
	validate(status, 0, result.ar_i64.part4, 1);

	backward = AR_const_false;
	len2.ar_i64.part4 = 2;
	status = AR_scan((AR_DATA*)&result, &inttype,
			   str1, (AR_DATA*)&len1, &inttype,
			   "kj", (AR_DATA*)&len2, &inttype,
			  (AR_DATA*)&backward, &backtype);
	validate(status, 0, result.ar_i64.part4, 10);

	backward = AR_const_true;
	len2.ar_i64.part4 = 2;
	status = AR_scan((AR_DATA*)&result, &inttype,
			   str1, (AR_DATA*)&len1, &inttype,
			   "jk", (AR_DATA*)&len2, &inttype,
			  (AR_DATA*)&backward, &backtype);
	validate(status, 0, result.ar_i64.part4, 11);

	backward = AR_const_true;
	len2.ar_i64.part4 = 1;
	status = AR_scan((AR_DATA*)&result, &inttype,
			   str1, (AR_DATA*)&len1, &inttype,
				"x", (AR_DATA*)&len2, &inttype,
			  (AR_DATA*)&backward, &backtype);
	validate(status, 0, result.ar_i64.part4, 24);

	backward = AR_const_true;
	len2.ar_i64.part4 = 3;
	status = AR_scan((AR_DATA*)&result, &inttype,
			   str1, (AR_DATA*)&len1, &inttype,
			  "jrk", (AR_DATA*)&len2, &inttype,
			  (AR_DATA*)&backward, &backtype);
	validate(status, 0, result.ar_i64.part4, 18);

	backward = AR_const_true;
	len2.ar_i64.part4 = 0;
	status = AR_scan((AR_DATA*)&result, &inttype,
			   str1, (AR_DATA*)&len1, &inttype,
				 "", (AR_DATA*)&len2, &inttype,
			  (AR_DATA*)&backward, &backtype);
	validate(status, AR_STAT_ZERO, result.ar_i64.part4, 0);

	backward = AR_const_true;
	len1.ar_i64.part4 = 0;
	len2.ar_i64.part4 = 0;
	status = AR_scan((AR_DATA*)&result, &inttype,
				 "", (AR_DATA*)&len1, &inttype,
				 "", (AR_DATA*)&len2, &inttype,
			  (AR_DATA*)&backward, &backtype);
	validate(status, AR_STAT_ZERO, result.ar_i64.part4, 0);
}

t_verify()
{
	char str1[] = "abcdefghijklmnopqrstuvwxyz";

	int	status;

	ar_data result;
	AR_DATA backward;

	ar_data len1 = {0,0,0,26};
	ar_data len2 = {0,0,0,0};

	AR_TYPE backtype = AR_Logical;

	printf("Testing AR_verify\n");

	len2.ar_i64.part4 = 3;
	status = AR_verify((AR_DATA*)&result, &inttype,
				 str1, (AR_DATA*)&len1, &inttype,
				"fgh", (AR_DATA*)&len2, &inttype,
				NULL, NULL);
	validate(status, 0, result.ar_i64.part4, 1);

	len2.ar_i64.part4 = 6;
	status = AR_verify((AR_DATA*)&result, &inttype,
				 str1, (AR_DATA*)&len1, &inttype,
			 "axbycz", (AR_DATA*)&len2, &inttype,
				NULL, NULL);
	validate(status, 0, result.ar_i64.part4, 4);

	len2.ar_i64.part4 = 0;
	status = AR_verify((AR_DATA*)&result, &inttype,
				 str1, (AR_DATA*)&len1, &inttype,
				   "", (AR_DATA*)&len2, &inttype,
				NULL, NULL);
	validate(status, 0, result.ar_i64.part4, 1);

	len1.ar_i64.part4 = 0;
	len2.ar_i64.part4 = 3;
	status = AR_verify((AR_DATA*)&result, &inttype,
				   "", (AR_DATA*)&len1, &inttype,
				"fgh", (AR_DATA*)&len2, &inttype,
				NULL, NULL);
	validate(status, AR_STAT_ZERO, result.ar_i64.part4, 0);

	len2.ar_i64.part4 = 0;
	status = AR_verify((AR_DATA*)&result, &inttype,
				   "", (AR_DATA*)&len1, &inttype,
				   "", (AR_DATA*)&len2, &inttype,
				NULL, NULL);
	validate(status, AR_STAT_ZERO, result.ar_i64.part4, 0);

	len1.ar_i64.part4 = 26;
	len2.ar_i64.part4 = 2;
	status = AR_verify((AR_DATA*)&result, &inttype,
				 str1, (AR_DATA*)&len1, &inttype,
				 "az", (AR_DATA*)&len2, &inttype,
				NULL, NULL);
	validate(status, 0, result.ar_i64.part4, 2);

	len2.ar_i64.part4 = 26;
	status = AR_verify((AR_DATA*)&result, &inttype,
				 str1, (AR_DATA*)&len1, &inttype,
				 str1, (AR_DATA*)&len1, &inttype,
				NULL, NULL);
	validate(status, AR_STAT_ZERO, result.ar_i64.part4, 0);

	backward = AR_const_false;
	len2.ar_i64.part4 = 2;
	status = AR_verify((AR_DATA*)&result, &inttype,
				 str1, (AR_DATA*)&len1, &inttype,
				 "jk", (AR_DATA*)&len2, &inttype,
			  (AR_DATA*)&backward, &backtype);
	validate(status, 0, result.ar_i64.part4, 1);

	backward = AR_const_true;
	len2.ar_i64.part4 = 2;
	status = AR_verify((AR_DATA*)&result, &inttype,
				 str1, (AR_DATA*)&len1, &inttype,
				 "jk", (AR_DATA*)&len2, &inttype,
			  (AR_DATA*)&backward, &backtype);
	validate(status, 0, result.ar_i64.part4, 26);

	backward = AR_const_true;
	len2.ar_i64.part4 = 3;
	status = AR_verify((AR_DATA*)&result, &inttype,
				 str1, (AR_DATA*)&len1, &inttype,
				"xyz", (AR_DATA*)&len2, &inttype,
			  (AR_DATA*)&backward, &backtype);
	validate(status, 0, result.ar_i64.part4, 23);

	backward = AR_const_true;
	len2.ar_i64.part4 = 4;
	status = AR_verify((AR_DATA*)&result, &inttype,
				 str1, (AR_DATA*)&len1, &inttype,
			   "jrkz", (AR_DATA*)&len2, &inttype,
			  (AR_DATA*)&backward, &backtype);
	validate(status, 0, result.ar_i64.part4, 25);

	backward = AR_const_true;
	len2.ar_i64.part4 = 0;
	status = AR_verify((AR_DATA*)&result, &inttype,
				 str1, (AR_DATA*)&len1, &inttype,
				   "", (AR_DATA*)&len2, &inttype,
			  (AR_DATA*)&backward, &backtype);
	validate(status, 0, result.ar_i64.part4, 26);

	backward = AR_const_true;
	len1.ar_i64.part4 = 0;
	len2.ar_i64.part4 = 0;
	status = AR_verify((AR_DATA*)&result, &inttype,
				   "", (AR_DATA*)&len1, &inttype,
				   "", (AR_DATA*)&len2, &inttype,
			  (AR_DATA*)&backward, &backtype);
	validate(status, AR_STAT_ZERO, result.ar_i64.part4, 0);
}

validate(int status, int xstatus, int result, int xresult)
{
	if(status != xstatus || result != xresult) {
	    fprintf(stderr,
	    	"status = 0%o, result = %d, expected status, result = 0%o, %d\n",
		status, result, xstatus, xresult);
	    misc_errors++;
	}
}


/* Main driver */

main (int argc, char **argv) {

	int tests = 0, failures = 0, rtests = 0, low = 0, high = 0;

	signal (SIGFPE, fperr);

	convtest64 ("results/cfix64_46", "Cray fix 46", ar_cfix64,
			46, 0, &tests, &failures);
	convtest64 ("results/cfix64_64", "Cray fix 64", ar_cfix64,
			64, 0, &tests, &failures);
	convtest64 ("results/cflts64", "Cray flt signed", ar_cflt64,
			0, 0, &tests, &failures);
	convtest64 ("results/cfltu64", "Cray flt unsigned", ar_cflt64,
			1, 0, &tests, &failures);
	convtest64 ("results/ifix64", "IEEE fix 64", ar_ifix64,
			64, 0, &tests, &failures);
	convtest64 ("results/iflts", "IEEE flt signed", ar_iflt64,
			0, 0, &tests, &failures);
	convtest64 ("results/ifltu", "IEEE flt unsigned", ar_iflt64,
			1, 0, &tests, &failures);
	convtest64 ("results/itoc64", "IEEE->Cray convert", ar_itoc64,
			AR_ROUND_NEAREST, 0, &tests, &failures);
	convtest64 ("results/ctoi64", "Cray->IEEE convert", ar_ctoi64,
			0, 0, &tests, &failures);
	convtest128a ("results/itoc128", "IEEE->Cray double convert", ar_i64toc128,
			0, &tests, &failures);
	convtest128a ("results/cflts128", "Cray double flt signed", ar_cflt128,
			0, &tests, &failures);
	convtest128a ("results/cfltu128", "Cray double flt unsigned", ar_cflt128,
			1, &tests, &failures);
	convtest128b ("results/cfix128_46", "Cray double fix 46", ar_cfix128,
			46, &tests, &failures);
	convtest128b ("results/cfix128_64", "Cray double fix 64", ar_cfix128,
			64, &tests, &failures);
	convtest128b ("results/c128to64", "Cray double round", ar_c128to64,
			0, &tests, &failures);
	convtest128c ("results/c128toi64", "Cray double -> IEEE", ar_c128toi64,
			0, &tests, &failures);

	craytest64_2 ("results/cray1_add64", "C1 fadd",
		      ar_cfadd64, -1,
		      &tests, &failures);
	craytest64_2 ("results/cray1_sub64", "C1 fsub",
		      ar_cfsub64, -1,
		      &tests, &failures);
	craytest64_2 ("results/cray1_mul64", "C1 fmul",
		      ar_cfmul64, AR_ROUNDED,
		      &tests, &failures);
	craytest64_2 ("results/cray1_recipiter", "C1 recipiter",
		      ar_cfmul64, AR_RECIPROCAL_ITERATION,
		      &tests, &failures);
	craytest64_1 ("results/cray1_recip", "C1 recip",
		      ar_c1frecip, AR_RECIPROCAL_ITERATION, /* unused arg */
		      &tests, &failures);
	craytest64_2 ("results/cray1_div64", "C1 fdiv",
		      ar_cfdiv64, AR_ROUNDED,
		      &tests, &failures);

	ieeetest64_2t ("results/ieee_add64", "IEEE 64 fadd",
		       AR_add, AR_Float_IEEE_NR_64,
		       &tests, &failures);
	ieeetest64_2t ("results/ieee_sub64", "IEEE 64 fsub",
		       AR_subtract, AR_Float_IEEE_NR_64,
		       &tests, &failures);
	ieeetest64_2t ("results/ieee_mul64", "IEEE 64 fmul",
		       AR_multiply, AR_Float_IEEE_NR_64,
		       &tests, &failures);
	ieeetest64_2t ("results/ieee_div64", "IEEE 64 fdiv",
		       AR_divide, AR_Float_IEEE_NR_64,
		       &tests, &failures);
	ieeetest64_2cmp ("results/ieee_cmp64", "IEEE 64 fcmp",
		       AR_compare, AR_Float_IEEE_NR_64,
		       &tests, &failures);
	ieeetest32_2t ("results/ieee_add32", "IEEE 32 fadd",
		       AR_add, AR_Float_IEEE_NR_32,
		       &tests, &failures);
	ieeetest32_2t ("results/ieee_sub32", "IEEE 32 fsub",
		       AR_subtract, AR_Float_IEEE_NR_32,
		       &tests, &failures);
	ieeetest32_2t ("results/ieee_mul32", "IEEE 32 fmul",
		       AR_multiply, AR_Float_IEEE_NR_32,
		       &tests, &failures);
	ieeetest32_2t ("results/ieee_div32", "IEEE 32 fdiv",
		       AR_divide, AR_Float_IEEE_NR_32,
		       &tests, &failures);
	ieeetest32_2cmp ("results/ieee_cmp32", "IEEE 32 fcmp",
		       AR_compare, AR_Float_IEEE_NR_32,
		       &tests, &failures);
	/* !! run the generated 32-64 conversion tests? !! */

	craytest128_2t ("results/cray1_add128", "C dpadd",
		        AR_add, AR_Float_Cray1_128,
		        &tests, &failures);
	craytest128_2t ("results/cray1_sub128", "C dpsub",
		        AR_subtract, AR_Float_Cray1_128,
		        &tests, &failures);
	craytest128_2t ("results/cray1_mul128", "C1 dpmul",
		        AR_multiply, AR_Float_Cray1_128,
		        &tests, &failures);
	craytest128_2t ("results/cray1_div128", "C1 dpdiv",
		        AR_divide, AR_Float_Cray1_128,
		        &tests, &failures);

#ifdef _CRAY1
	craytest64_1t ("results/cray1_sqrt", "C1 sqrt",
		       AR_sqrt, AR_Float_Cray1_64,
		       &tests, &failures);
	craytest64_c ("results/cray1_cabs", "C1 cabs",
		      AR_cabs, AR_Complex_Cray1_64, AR_Float_Cray1_64,
		      &tests, &failures);
	craytest64_1t ("results/cray1_log", "C1 log",
		       AR_log, AR_Float_Cray1_64,
		       &tests, &failures);
	craytest64_1t ("results/cray1_exp", "C1 exp",
		       AR_exp, AR_Float_Cray1_64,
		       &tests, &failures);
	craytest64_i2 ("results/cray1_powii", "C powii 46",
		       AR_power, AR_Int_46_S,
		       &tests, &failures);
	craytest64_i2 ("results/cray1_powii", "C powii 64",
		       AR_power, AR_Int_64_S,
		       &tests, &failures);
	craytest64_i ("results/cray1_powri", "C1 powri",
		      AR_power, AR_Float_Cray1_64,
		      &tests, &failures);
	craytest64_2t ("results/cray1_powrr", "C1 powrr",
		       AR_power, AR_Float_Cray1_64,
		       &tests, &failures);
	craytest128_1t ("results/cray1_dsqrt", "C1 dsqrt",
		       AR_sqrt, AR_Float_Cray1_128,
		       &tests, &failures);
	craytest128_1t ("results/cray1_dlog", "C1 dlog",
		       AR_log, AR_Float_Cray1_128,
		       &tests, &failures);
	craytest128_1t ("results/cray1_dexp", "C1 dexp",
		       AR_exp, AR_Float_Cray1_128,
		       &tests, &failures);
	craytest128_i ("results/cray1_powdi", "C1 powdi",
		       AR_power, AR_Float_Cray1_128,
		       &tests, &failures);
	craytest128_2t ("results/cray1_powdd", "C1 powdd",
		       AR_power, AR_Float_Cray1_128,
		       &tests, &failures);
	craytest128_64 ("results/cray1_powdr", "C1 powdr",
		       AR_power, AR_Float_Cray1_128, AR_Float_Cray1_64,
		       &tests, &failures);
#else
#ifdef __svr4__
	ieeetest32_2c ("results/ieee_cdiv32", "IEEE cdiv",
		       AR_divide, AR_Complex_IEEE_NR_32,
		       &tests, &failures);
	ieeetest64_2c ("results/ieee_cdiv64", "IEEE cddiv",
		       AR_divide, AR_Complex_IEEE_NR_64,
		       &tests, &failures);
   #define U "_solaris"
#else
   #define U "_sunOS"
#endif
	ieeetest64_1t ("results/ieee_sqrt64" U, "IEEE sqrt",
		       AR_sqrt, AR_Float_IEEE_NR_64,
		       &tests, &failures);
	ieeetest64_1t ("results/ieee_log64" U, "IEEE log",
		       AR_log, AR_Float_IEEE_NR_64,
		       &tests, &failures);
	ieeetest64_1t ("results/ieee_exp64" U, "IEEE exp",
		       AR_exp, AR_Float_IEEE_NR_64,
		       &tests, &failures);
	ieeetest64_2t ("results/ieee_powrr64" U, "IEEE powrr",
		       AR_power, AR_Float_IEEE_NR_64,
		       &tests, &failures);
#endif

#undef U

	printf ("end %d confidence tests (%d failures)\n", tests, failures);

#ifdef _CRAY1
	crayrand64_2 ("C1 fadd", ar_cfadd64, -1, fadd,
			&rtests, &low, &high);
	crayrand64_2 ("C1 fsub", ar_cfsub64, -1, fsub,
			&rtests, &low, &high);
	crayrand64_2 ("C1 *R mul", ar_cfmul64, AR_ROUNDED, ARRMULT,
			&rtests, &low, &high);
	crayrand64_2 ("C1 *F mul", ar_cfmul64, AR_UNROUNDED, ARFMULT,
			&rtests, &low, &high);
	crayrand64_2 ("C1 *I mul", ar_cfmul64, AR_RECIPROCAL_ITERATION, ARIMULT,
			&rtests, &low, &high);
	crayrand64_1 ("C1 recip", ar_c1frecip, AR_ROUNDED, ARHRECIP,
			&rtests, &low, &high);
	crayrand64_2 ("C1 fdiv", ar_cfdiv64, AR_ROUNDED, fdiv,
			&rtests, &low, &high);
	crayrand128_2 ("C1 dp fadd", ar_cfadd128, -1, dfadd,
			&rtests, &low, &high);
	crayrand128_2 ("C1 dp fsub", ar_cfsub128, -1, dfsub,
			&rtests, &low, &high);
	crayrand128_2 ("C1 dp fmul", ar_cfmul128, AR_ROUNDED, dfmul,
			&rtests, &low, &high);
	crayrand128_2 ("C1 dp fdiv", ar_cfdiv128, AR_ROUNDED, dfdiv,
			&rtests, &low, &high);
	crayrandcplx ("C1 complex add", AR_add, AR_Complex_Cray1_64, cadd,
			&rtests, &low);
	crayrandcplx ("C1 complex sub", AR_subtract, AR_Complex_Cray1_64, csub,
			&rtests, &low);
	crayrandcplx ("C1 complex mul", AR_multiply, AR_Complex_Cray1_64, cmul,
			&rtests, &low);
	crayrandcplx ("C1 complex div", AR_divide, AR_Complex_Cray1_64, cdiv,
			&rtests, &low);
#else
#if _Solaris || defined(__mips)
	ieeerand128_2 ("IEEE128 fadd", ar_ifadd128, AR_ROUND_NEAREST, xdfadd,
			&rtests, &low, &high);
	ieeerand128_2 ("IEEE128 fsub", ar_ifsub128, AR_ROUND_NEAREST, xdfsub,
			&rtests, &low, &high);
	ieeerand128_2 ("IEEE128 fmul", ar_ifmul128, AR_ROUND_NEAREST, xdfmul,
			&rtests, &low, &high);
	ieeerand128_2 ("IEEE128 fdiv", ar_ifdiv128, AR_ROUND_NEAREST, xdfdiv,
			&rtests, &low, &high);
#endif
	ieeerand64_2 ("IEEE64 fadd", ar_ifadd64, AR_ROUND_NEAREST, dfadd,
			&rtests, &low, &high);
	ieeerand64_2 ("IEEE64 fsub", ar_ifsub64, AR_ROUND_NEAREST, dfsub,
			&rtests, &low, &high);
	ieeerand64_2 ("IEEE64 fmul", ar_ifmul64, AR_ROUND_NEAREST, dfmul,
			&rtests, &low, &high);
	ieeerand64_2 ("IEEE64 fdiv", ar_ifdiv64, AR_ROUND_NEAREST, dfdiv,
			&rtests, &low, &high);
	ieeerand32_2 ("IEEE32 fadd", ar_ifadd32, AR_ROUND_NEAREST, fadd,
			&rtests, &low, &high);
	ieeerand32_2 ("IEEE32 fsub", ar_ifsub32, AR_ROUND_NEAREST, fsub,
			&rtests, &low, &high);
	ieeerand32_2 ("IEEE32 fmul", ar_ifmul32, AR_ROUND_NEAREST, fmul,
			&rtests, &low, &high);
	ieeerand32_2 ("IEEE32 fdiv", ar_ifdiv32, AR_ROUND_NEAREST, fdiv,
			&rtests, &low, &high);
	/* !! random 32-64 conversion tests? !! */
#endif

	printf ("end %d random tests; %d low, %d high\n", rtests, low, high);

	misc_tests(&failures);

	exit (!!failures | !!low | !!high);
}


static char USMID [] = "\n%Z%%M%	%I%	%G% %U%\n";
static char rcsid [] = "$Id: confidence.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $";
