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


#include <stdio.h>
#include <math.h>
#include <signal.h>
#include "arith.internal.h"

#define uint32	unsigned long
#define float64	double
#define float32	float
#define sqrt64 sqrt
#define log64 log
#define exp64 exp
#define pow64 pow

#ifdef __svr4__
extern void cdiv32_ ();
extern void cdiv64_ ();
#endif

/* Trap floating-point exceptions and set a flag */
static volatile int fp_error = AR_STAT_OK;
void
fperr (int sig) {

	fp_error = AR_STAT_UNDEFINED;
	signal (SIGFPE, fperr);

}

/* Prevent printing of math library error message */
int matherr (struct exception *X) {
	return 1;
}


/* Big-endian 64-bit IEEE FP - sign, expo, upper 20 bits of coeff, lower 32 */
#define V(sign,expo,c0,c1)	((uint32)(sign)<<31) | ((uint32)(expo)<<20) | (c0), (c1),

static uint32 t64vals [] = {

	V(0,0,0,0)
	V(1,0,0,0)
	V(0,01777,0,0)
	V(1,01777,0,0)
	V(0,01777,02000000,0)
	V(1,01777,02000000,0)
	V(0,01777,0,1)
	V(1,01777,0,1)
	V(0,01777,03777777,037777777777)
	V(1,01777,03777777,037777777777)
	V(0,01776,03777777,037777777777)
	V(1,01776,03777777,037777777777)
	V(0,02064,0,0)
	V(1,02064,0,0)
	V(0,01710,0,0)
	V(1,01710,0,0)
	V(0,02064,03777777,037777777777)
	V(1,02064,03777777,037777777777)
	V(0,01710,03777777,037777777777)
	V(1,01710,03777777,037777777777)
	V(0,1,0,0)
	V(1,1,0,0)
	V(0,03776,0,0)
	V(1,03776,0,0)
	V(0,03776,03777777,037777777777)
	V(1,03776,03777777,037777777777)
	V(0,03777,0,0)
	V(1,03777,0,0)
	V(0,03777,03777777,037777777777)
	V(1,03777,03777777,037777777777)
	V(0,0,02000000,0)
	V(1,0,02000000,0)
	V(0,0,0,1)
	V(1,0,0,1)
	V(0,0,00123400,0)

	0	/* must be last */
};

#undef V


/* Big-endian 32-bit IEEE FP - sign, expo, coeff */
#define	V(sign,expo,coeff)	((uint32)(sign)<<31) | ((uint32)(expo)<<23) | (coeff),

static uint32 t32vals [] = {

	V(0,0,0)
	V(1,0,0)
	V(0,0177,0)
	V(1,0177,0)
	V(0,0177,020000000)
	V(1,0177,020000000)
	V(0,0177,0)
	V(1,0177,0)
	V(0,0177,037777777)
	V(1,0177,037777777)
	V(0,0176,037777777)
	V(1,0176,037777777)
	V(0,0264,0)
	V(1,0264,0)
	V(0,0110,0)
	V(1,0110,0)
	V(0,0264,037777777)
	V(1,0264,037777777)
	V(0,0110,037777777)
	V(1,0110,037777777)
	V(0,1,0)
	V(1,1,0)
	V(0,0376,0)
	V(1,0376,0)
	V(0,0376,037777777)
	V(1,0376,037777777)
	V(0,0377,0)
	V(1,0377,0)
	V(0,0377,037777777)
	V(1,0377,037777777)
	V(0,0,020000000)
	V(1,0,020000000)
	V(0,0,0)
	V(1,0,0)
	V(0,0,001234000)

	0	/* must be last */

};

#undef V


static
float64
fadd64 (float64 x, float64 y) {
	return x + y;
}

static
float64
fsub64 (float64 x, float64 y) {
	return x - y;
}

static
float64
fmul64 (float64 x, float64 y) {
	return x * y;
}

static
float64
fdiv64 (float64 x, float64 y) {
	return x / y;
}

static
float64
fcmp64 (float64 x, float64 y) {
	if(isnan(x) || isnan(y)) {
		fp_error = AR_STAT_UNDEFINED;
		return -2.0;
	}
	if(x==y)
		return 0;
	if(x<y)
		return -1.0;
	return 1.0;
}

static
float32
fadd32 (float32 x, float32 y) {
	return x + y;
}

static
float32
fsub32 (float32 x, float32 y) {
	return x - y;
}

static
float32
fmul32 (float32 x, float32 y) {
	return x * y;
}

static
float32
fdiv32 (float32 x, float32 y) {
	return x / y;
}

static
float32
fcmp32 (float32 x, float32 y) {
	if(isnan(x) || isnan(y)) {
		fp_error = AR_STAT_UNDEFINED;
		return -2.0;
	}
	if(x==y)
		return 0;
	if(x<y)
		return -1.0;
	return 1.0;
}

static
float64
f32to64 (float32 x) {
	return x;
}

static
float32
f64to32 (float64 x) {
	return x;
}


static
prieee64 (FILE *fp, float64 val) {
	union { uint32 lv [2]; float64 fv; } u;
	u.fv = val;
	fprintf (fp, " %011lo%o%010lo",
		 u.lv [0] >> 1,
		 ((u.lv [0] & 1) << 2) | (u.lv [1] >> 30),
		 u.lv [1] & 07777777777);
}


static
prieee32 (FILE *fp, float32 val) {
	union { uint32 lv; float32 fv; } u;
	u.fv = val;
	fprintf (fp, " %011lo", u.lv);
}


static
setstat64 (float64 val) {
	union { uint32 lv [2]; float64 fv; } u;
	u.fv = val;
	if (val == (1.0/0.0) || val == (-1.0/0.0))
		fp_error |= AR_STAT_OVERFLOW;
	if (isnan (val))
		fp_error |= AR_STAT_UNDEFINED;
	else if (!(u.lv [0] | u.lv [1]))
		fp_error |= AR_STAT_ZERO;
	else if (!-val)
		fp_error |= AR_STAT_ZERO | AR_STAT_NEGATIVE;
	else if (val < 0)
		fp_error |= AR_STAT_NEGATIVE;
}


static
setstat32 (float32 val) {
	union { uint32 lv; float32 fv; } u;
	u.fv = val;
	if (val == (1.0/0.0) || val == (-1.0/0.0))
		fp_error |= AR_STAT_OVERFLOW;
	if (isnan (val))
		fp_error |= AR_STAT_UNDEFINED;
	else if (!u.lv)
		fp_error |= AR_STAT_ZERO;
	else if (!-val)
		fp_error |= AR_STAT_ZERO | AR_STAT_NEGATIVE;
	else if (val < 0)
		fp_error |= AR_STAT_NEGATIVE;
}


dotest64_2 (char *fn, float64 (*op)(float64, float64)) {

	int i, j;
	union { uint32 lv [2]; float64 fv; } u;
	float64 x, y, z;
	FILE *fp = fopen (fn, "w");

	if (!fp) {
		fprintf (stderr, "can't open %s\n", fn);
		exit (1);
	}

	for (i = 0; !i || t64vals [i]; i++) {
		u.lv [0] = t64vals [i++];
		u.lv [1] = t64vals [i];
		x = u.fv;
		for (j = 0; !j || t64vals [j]; j++) {
			u.lv [0] = t64vals [j++];
			u.lv [1] = t64vals [j];
			y = u.fv;
			z = op (x, y);
			setstat64 (z);
			fprintf (fp, "$t");
			prieee64 (fp, x);
			prieee64 (fp, y);
			prieee64 (fp, z);
			fprintf (fp, " %o\n", fp_error);
			fp_error = AR_STAT_OK;
		}
	}

	fclose (fp);
}


dotest64_2c (char *fn, void (*op)(float64*, float64*, float64*)) {

	int i, j;
	int save_status;
	union { uint32 lv [2]; float64 fv; } u;
	float64 x[2], y[2], z[2];
	FILE *fp = fopen (fn, "w");

	if (!fp) {
		fprintf (stderr, "can't open %s\n", fn);
		exit (1);
	}

	for (i = 0; !i || t64vals [i]; i++) {
		u.lv [0] = t64vals [i++];
		u.lv [1] = t64vals [i];
		x[0] = u.fv;
		x[1] = u.fv*(4.0-(i&7));
		for (j = 0; !j || t64vals [j]; j++) {
			u.lv [0] = t64vals [j++];
			u.lv [1] = t64vals [j];
			y[0] = u.fv;
			y[1] = u.fv*((i*3)-2.0);
			op (z, x, y);
			setstat64 (z[0]);
			save_status = fp_error;
			setstat64 (z[1]);
			save_status &= fp_error&AR_STAT_ZERO;
			fp_error = save_status | (fp_error&(
				   (AR_STAT_OVERFLOW | AR_STAT_UNDEFINED |
				    AR_STAT_UNDERFLOW | AR_STAT_INVALID_TYPE)));
			fprintf (fp, "$t");
			prieee64 (fp, x[0]);
			prieee64 (fp, x[1]);
			prieee64 (fp, y[0]);
			prieee64 (fp, y[1]);
			prieee64 (fp, z[0]);
			prieee64 (fp, z[1]);
			fprintf (fp, " %o\n", fp_error);
			fp_error = AR_STAT_OK;
		}
	}

	fclose (fp);
}



dotest64_1 (char *fn, float64 (*op)(float64)) {

	int i, j;
	union { uint32 lv [2]; float64 fv; } u;
	float64 x, z;
	FILE *fp = fopen (fn, "w");

	if (!fp) {
		fprintf (stderr, "can't open %s\n", fn);
		exit (1);
	}

	for (i = 0; !i || t64vals [i]; i++) {
		u.lv [0] = t64vals [i++];
		u.lv [1] = t64vals [i];
		x = u.fv;
		z = op (x);
		setstat64 (z);
		fprintf (fp, "$t");
		prieee64 (fp, x);
		prieee64 (fp, z);
		fprintf (fp, " %o\n", fp_error);
		fp_error = AR_STAT_OK;
	}

	fclose (fp);
}


dotest64_1s (char *fn, float64 (*op)(float32)) {

	int i, j;
	union { uint32 lv; float64 fv; } u;
	float32 x;
	float64 z;
	FILE *fp = fopen (fn, "w");

	if (!fp) {
		fprintf (stderr, "can't open %s\n", fn);
		exit (1);
	}

	for (i = 0; !i || t32vals [i]; i++) {
		u.lv = t32vals [i];
		x = u.fv;
		z = op (x);
		setstat64 (z);
		fprintf (fp, "$t");
		prieee32 (fp, x);
		prieee64 (fp, z);
		fprintf (fp, " %o\n", fp_error);
		fp_error = AR_STAT_OK;
	}

	fclose (fp);
}


dotest32_2 (char *fn, float32 (*op)(float32, float32)) {

	int i, j;
	union { uint32 lv; float32 fv; } u;
	float32 x, y, z;
	FILE *fp = fopen (fn, "w");

	if (!fp) {
		fprintf (stderr, "can't open %s\n", fn);
		exit (1);
	}

	for (i = 0; !i || t32vals [i]; i++) {
		u.lv = t32vals [i];
		x = u.fv;
		for (j = 0; !j || t32vals [j]; j++) {
			u.lv = t32vals [j];
			y = u.fv;
			z = op (x, y);
			setstat32 (z);
			fprintf (fp, "$t");
			prieee32 (fp, x);
			prieee32 (fp, y);
			prieee32 (fp, z);
			fprintf (fp, " %o\n", fp_error);
			fp_error = AR_STAT_OK;
		}
	}

	fclose (fp);
}


dotest32_2c (char *fn, void (*op)(float32*, float32*, float32*)) {

	int i, j;
	int save_status;
	union { uint32 lv; float32 fv; } u;
	float32 x[2], y[2], z[2];
	FILE *fp = fopen (fn, "w");

	if (!fp) {
		fprintf (stderr, "can't open %s\n", fn);
		exit (1);
	}

	for (i = 0; !i || t32vals [i]; i++) {
		u.lv = t32vals [i];
		x[0] = u.fv;
		x[1] = -u.fv;
		for (j = 0; !j || t32vals [j]; j++) {
			u.lv = t32vals [j];
			y[0] = u.fv-0.001;
			y[1] = u.fv+0.01;
			op (z, x, y);
			setstat32 (z[0]);
			save_status = fp_error;
			fp_error &= ~AR_STAT_ZERO;
			setstat32 (z[1]);
			save_status &= fp_error&AR_STAT_ZERO;
			fp_error = save_status | (fp_error&(
				   (AR_STAT_OVERFLOW | AR_STAT_UNDEFINED |
				    AR_STAT_UNDERFLOW | AR_STAT_INVALID_TYPE)));
			fprintf (fp, "$t");
			prieee32 (fp, x[0]);
			prieee32 (fp, x[1]);
			prieee32 (fp, y[0]);
			prieee32 (fp, y[1]);
			prieee32 (fp, z[0]);
			prieee32 (fp, z[1]);
			fprintf (fp, " %o\n", fp_error);
			fp_error = AR_STAT_OK;
		}
	}

	fclose (fp);
}


dotest32_1s (char *fn, float32 (*op)(float64)) {

	int i, j;
	union { uint32 lv [2]; float64 fv; } u;
	float64 x;
	float32 z;
	FILE *fp = fopen (fn, "w");

	if (!fp) {
		fprintf (stderr, "can't open %s\n", fn);
		exit (1);
	}

	for (i = 0; !i || t64vals [i]; i++) {
		u.lv [0] = t64vals [i++];
		u.lv [1] = t64vals [i];
		x = u.fv;
		z = op (x);
		setstat32 (z);
		fprintf (fp, "$t");
		prieee64 (fp, x);
		prieee32 (fp, z);
		fprintf (fp, " %o\n", fp_error);
		fp_error = AR_STAT_OK;
	}

	fclose (fp);
}


main () {
	signal (SIGFPE, fperr);

	dotest64_2 ("results/ieee_add64", fadd64);
	dotest64_2 ("results/ieee_sub64", fsub64);
	dotest64_2 ("results/ieee_mul64", fmul64);
	dotest64_2 ("results/ieee_div64", fdiv64);
	dotest64_2 ("results/ieee_cmp64", fcmp64);
	dotest64_1s ("results/ieee_32to64", f32to64);
	dotest32_2 ("results/ieee_add32", fadd32);
	dotest32_2 ("results/ieee_sub32", fsub32);
	dotest32_2 ("results/ieee_mul32", fmul32);
	dotest32_2 ("results/ieee_div32", fdiv32);
	dotest32_2 ("results/ieee_cmp32", fcmp32);
	dotest32_1s ("results/ieee_64to32", f64to32);

#ifdef __svr4__
	dotest32_2c ("results/ieee_cdiv32", cdiv32_);
	dotest64_2c ("results/ieee_cdiv64", cdiv64_);
   #define U "_solaris"
#else
   #define U "_sunOS"
#endif
	dotest64_1 ("results/ieee_sqrt64" U, sqrt64);
	dotest64_1 ("results/ieee_log64" U, log64);
	dotest64_1 ("results/ieee_exp64" U, exp64);
	dotest64_2 ("results/ieee_powrr64" U, pow64);
#undef U

	exit (0);
}


static char USMID [] = "\n%Z%%M%	%I%	%G% %U%\n";
static char rcsid [] = "$Id: itgen.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $";
