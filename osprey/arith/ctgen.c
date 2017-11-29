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
 *	Test generator for the Cray arithmetics
 */

#include <stdio.h>
#include <signal.h>
#include <math.h>
#include <setjmp.h>
#include <complex.h>
#include "arith.internal.h"


/* Trap floating-point exceptions and set a flag */
static volatile int fp_error = AR_STAT_OK;
static int jmp_on_err = 0;
static jmp_buf fperr_jmpbuf;
void
fperr (int sig) {
	fp_error |= AR_STAT_OVERFLOW;
	signal (sig, fperr);
	if (jmp_on_err)
		longjmp (fperr_jmpbuf, 1);
}

void
trapmathlibabort (void) {
	if (jmp_on_err) {
		fp_error |= AR_STAT_UNDEFINED;
		longjmp (fperr_jmpbuf, 1);
	}
	raise (SIGFPE);
}


/* "Interesting" 64-bit FP values */
static unsigned long tvals [] = {

	00000000000000000000000,
	00400014000000000000000,
	01400014000000000000000,
	00400004000000000000000,
	01400004000000000000000,
	00400016000000000000000,
	01400016000000000000000,
	00400014000000000000001,
	01400014000000000000001,
	00200004000000000000000,
	00577774000000000000000,
	00600004000000000000000,
	00177774000000000000000,
	01200004000000000000000,
	01577774000000000000000,
	01600004000000000000000,
	01177774000000000000000,
	00400010000000000000001,
	01400010000000000000001,
	00400000000000000000001,
	01400000000000000000001,
	00200000000000000000001,
	00577770000000000000001,
	00600000000000000000001,
	00177770000000000000001,
	01200000000000000000001,
	01577770000000000000001,
	01600000000000000000001,
	01177770000000000000001,
	00400017777777777777777,
	01400017777777777777777,
	00400007777777777777777,
	01400007777777777777777,
	00400614000000000000000,
	01400614000000000000000,
	00400604000000000000000,
	01400604000000000000000,
	00400617777777777777777,
	01400617777777777777777,
	00400607777777777777777,
	01400607777777777777777,
	00400624000000000000000,
	01400624000000000000000,
	00400627777777777777777,
	01400627777777777777777,
	00400617777777777777777,
	01400617777777777777777,
	00000000000000000000001,
	00000000012340000000000,
	01777777777777777777777,

	0	/* must be last */
};


/* "Interesting" lower-order bits for 128-bit DP values */
static unsigned long dtvals [] = {

	00000000000000000000000,
	00000004000000000000000,
	00000000000000000000001,
	00000007777777777777777,
	00000007777777777777776,
	00000000000001234123400,

	0	/* must be last */
};


/* "Interesting" integer values for exponents */
static unsigned long ivals [] = {

	00000000000000000000000,
	00000000000000000000001,
	00000000000000000000002,
	00000000000000000000041,
	00000000000000000000077,
	00000000000000000000100,
	00777777777777777777777,
	01777777777777777777777,
	01777777777777777777776,
	01000000000000000000000,

	0	/* must be last */
};


/* Operations */
void
fadd (float *z, float *x, float *y) {
	*z = *x + *y;
}

void
fsub (float *z, float *x, float *y) {
	*z = *x - *y;
}

void
fmul (float *z, float *x, float *y) {
	*z = *x * *y;
}

void
fdiv (float *z, float *x, float *y) {
	*z = *x / *y;
}

void
dfadd (long double *z, long double *x, long double *y) {
	*z = *x + *y;
}

void
dfsub (long double *z, long double *x, long double *y) {
	*z = *x - *y;
}

void
dfmul (long double *z, long double *x, long double *y) {
	*z = *x * *y;
}

void
dfdiv (long double *z, long double *x, long double *y) {
	*z = *x / *y;
}

extern void ARFMULT (float *, float *, float *);
extern void ARRMULT (float *, float *, float *);
extern void ARIMULT (float *, float *, float *);
extern void ARQMULT (float *, float *, float *);
extern void ARHRECIP (float *, float *);

extern void ARSQRT (float *, float *);
extern void ARDSQRT (long double *, long double *);
extern void ARLOG (float *, float *);
extern void ARDLOG (long double *, long double *);
extern void AREXP (float *, float *);
extern void ARDEXP (long double, long double *);
extern void ARPOWII (int *, int *, int *);
extern void ARPOWRI (float *, float *, long *);
extern void ARPOWRR (float *, float *, float *);
extern void ARPOWDI (long double *, long double *, long *);
extern void ARPOWDR (long double *, long double *, float *);
extern void ARPOWDD (long double *, long double *, long double *);
extern void ARCABS (float *, double complex *);


dotest64_1 (char *fn, void (*op)()) {
	int i;
	static volatile union { unsigned long lv; float fv; } u;
	float x, z;
	FILE *fp = fopen (fn, "w");
	if (!fp) {
		fprintf (stderr, "can't open %s\n", fn);
		exit (1);
	}
	for (i = 0; !i || tvals [i]; i++) {
		u.lv = tvals [i];
		x = u.fv;
		u.lv = 0;
		if (setjmp (fperr_jmpbuf))
			z = 0;
		else {
			op (&z, &x);
			u.fv = z;
			if (!u.lv)
				fp_error |= AR_STAT_ZERO;
			else if (u.lv >> 63)
				fp_error |= AR_STAT_NEGATIVE;
		}
		fprintf (fp, "$t %022o %022o %o\n", x, z, fp_error);
		fp_error = AR_STAT_OK;
	}
	fclose (fp);
}

dotest64_c (char *fn, void (*op)()) {
	int i, j;
	static volatile union {
		unsigned long lv [2];
		double complex cv;
		float fv;
	} u, v;
	float z;
	double complex x;
	FILE *fp = fopen (fn, "w");
	if (!fp) {
		fprintf (stderr, "can't open %s\n", fn);
		exit (1);
	}
	for (i = 0; !i || tvals [i]; i++) {
		u.lv [0] = tvals [i];
		for (j = 0; !j || tvals [j]; j++) {
			u.lv [1] = tvals [j];
			x = u.cv;
			v.lv [0] = v.lv [1] = 0;
			if (setjmp (fperr_jmpbuf))
				z = 0;
			else {
				op (&z, &x);
				v.fv = z;
				if (!v.lv [0])
					fp_error |= AR_STAT_ZERO;
			}
			fprintf (fp, "$t %022o %022o %022o %o\n",
				 x, z, fp_error);
			fp_error = AR_STAT_OK;
		}
	}
	fclose (fp);
}

dotest64_2 (char *fn, void (*op)()) {
	int i, j;
	static volatile union { unsigned long lv; float fv; } u;
	float x, y, z;
	FILE *fp = fopen (fn, "w");
	if (!fp) {
		fprintf (stderr, "can't open %s\n", fn);
		exit (1);
	}
	for (i = 0; !i || tvals [i]; i++) {
		u.lv = tvals [i];
		x = u.fv;
		for (j = 0; !j || tvals [j]; j++) {
			u.lv = tvals [j];
			y = u.fv;
			u.lv = 0;
			if (setjmp (fperr_jmpbuf))
				z = 0;
			else {
				op (&z, &x, &y);
				u.fv = z;
				if (!u.lv)
					fp_error |= AR_STAT_ZERO;
				else if (u.lv >> 63)
					fp_error |= AR_STAT_NEGATIVE;
			}
			fprintf (fp, "$t %022o %022o %022o %o\n",
				 x, y, z, fp_error);
			fp_error = AR_STAT_OK;
		}
	}
	fclose (fp);
}

dotest64_i (char *fn, void (*op)()) {
	int i, j, y;
	static volatile union { unsigned long lv; float fv; } u;
	float x, z;
	FILE *fp = fopen (fn, "w");
	if (!fp) {
		fprintf (stderr, "can't open %s\n", fn);
		exit (1);
	}
	for (i = 0; !i || tvals [i]; i++) {
		u.lv = tvals [i];
		x = u.fv;
		for (j = 0; !j || ivals [j]; j++) {
			y = ivals [j];
			u.lv = 0;
			if (setjmp (fperr_jmpbuf))
				z = 0;
			else {
				op (&z, &x, &y);
				u.fv = z;
				if (!u.lv)
					fp_error |= AR_STAT_ZERO;
				else if (u.lv >> 63)
					fp_error |= AR_STAT_NEGATIVE;
			}
			fprintf (fp, "$t %022o %022o %022o %o\n",
				 x, y, z, fp_error);
			fp_error = AR_STAT_OK;
		}
	}
	fclose (fp);
}

dotest64_i2 (char *fn, void (*op)()) {
	int i, j;
	static volatile unsigned long x, y, z;
	FILE *fp = fopen (fn, "w");
	if (!fp) {
		fprintf (stderr, "can't open %s\n", fn);
		exit (1);
	}
	for (i = 0; !i || ivals [i]; i++) {
		x = ivals [i];
		for (j = 0; !j || ivals [j]; j++) {
			y = ivals [j];
			fp_error = AR_STAT_OK;
			if (setjmp (fperr_jmpbuf))
				z = 0;
			else {
				op (&z, &x, &y);
				if (!z)
					fp_error |= AR_STAT_ZERO;
				else if (z >> 63)
					fp_error |= AR_STAT_NEGATIVE;
			}
			fprintf (fp, "$t %022o %022o %022o %o\n",
				 x, y, z, fp_error);
			fp_error = AR_STAT_OK;
		}
	}
	fclose (fp);
}

dotest128_1 (char *fn, void (*op)()) {
	int i, j;
	static volatile union { unsigned long lv [2]; long double fv; } u, v;
	long double x, z;
	FILE *fp = fopen (fn, "w");
	if (!fp) {
		fprintf (stderr, "can't open %s\n", fn);
		exit (1);
	}
	for (i = 0; !i || tvals [i]; i++)
		for (j = 0; !j || dtvals [j]; j++) {
			u.lv [0] = tvals [i];
			u.lv [1] = dtvals [j];
			x = u.fv;
			v.lv [0] = v.lv [1] = 0;
			if (setjmp (fperr_jmpbuf))
				z = 0;
			else {
				op (&z, &x);
				v.fv = z;
				if (!(v.lv [0] | v.lv [1]))
					fp_error |= AR_STAT_ZERO;
				else if (v.lv [0] >> 63)
					fp_error |= AR_STAT_NEGATIVE;
			}
			fprintf (fp, "$t %022o %022o %022o %022o %o\n",
				 u.lv [0], u.lv [1],
				 v.lv [0], v.lv [1],
				 fp_error);
			fp_error = AR_STAT_OK;
		}
	fclose (fp);
}

dotest128_2 (char *fn, void (*op)()) {
	int i, j, k;
	static volatile union { unsigned long lv [2]; long double fv; } u, v, w;
	long double x, y, z;
	FILE *fp = fopen (fn, "w");
	if (!fp) {
		fprintf (stderr, "can't open %s\n", fn);
		exit (1);
	}
	for (i = k = 0; !i || tvals [i]; i++) {
		u.lv [0] = tvals [i];
		u.lv [1] = dtvals [k++];
		if (!dtvals [k])
			k = 0;
		x = u.fv;
		for (j = 0; !j || tvals [j]; j++) {
			v.lv [0] = tvals [j];
			v.lv [1] = dtvals [k++];
			if (!dtvals [k])
				k = 0;
			y = v.fv;
			w.lv [0] = w.lv [1] = 0;
			if (setjmp (fperr_jmpbuf))
				z = 0;
			else {
				op (&z, &x, &y);
				w.fv = z;
				if (!(w.lv [0] | w.lv [1]))
					fp_error |= AR_STAT_ZERO;
				else if (w.lv [0] >> 63)
					fp_error |= AR_STAT_NEGATIVE;
			}
			fprintf (fp, "$t %022o %022o %022o %022o %022o %022o %o\n",
				 x, y, z, fp_error);
			fp_error = AR_STAT_OK;
		}
	}
	fclose (fp);
}

dotest128_r (char *fn, void (*op)()) {
	int i, j, k;
	static volatile union {
		unsigned long lv [2];
		long double fv;
		float sv;
	} u, v, w;
	float y;
	long double x, z;
	FILE *fp = fopen (fn, "w");
	if (!fp) {
		fprintf (stderr, "can't open %s\n", fn);
		exit (1);
	}
	for (i = k = 0; !i || tvals [i]; i++) {
		u.lv [0] = tvals [i];
		u.lv [1] = dtvals [k++];
		if (!dtvals [k])
			k = 0;
		x = u.fv;
		for (j = 0; !j || tvals [j]; j++) {
			v.lv [0] = tvals [j];
			y = v.sv;
			w.lv [0] = w.lv [1] = 0;
			if (setjmp (fperr_jmpbuf))
				z = 0;
			else {
				op (&z, &x, &y);
				w.fv = z;
				if (!(w.lv [0] | w.lv [1]))
					fp_error |= AR_STAT_ZERO;
				else if (w.lv [0] >> 63)
					fp_error |= AR_STAT_NEGATIVE;
			}
			fprintf (fp, "$t %022o %022o %022o %022o %022o %o\n",
				 x, y, z, fp_error);
			fp_error = AR_STAT_OK;
		}
	}
	fclose (fp);
}

dotest128_i (char *fn, void (*op)()) {
	int i, j, k, y;
	static volatile union {
		unsigned long lv [2];
		long double fv;
	} u, v, w;
	long double x, z;
	FILE *fp = fopen (fn, "w");
	if (!fp) {
		fprintf (stderr, "can't open %s\n", fn);
		exit (1);
	}
	for (i = k = 0; !i || tvals [i]; i++) {
		u.lv [0] = tvals [i];
		u.lv [1] = dtvals [k++];
		if (!dtvals [k])
			k = 0;
		x = u.fv;
		for (j = 0; !j || ivals [j]; j++) {
			y = ivals [j];
			w.lv [0] = w.lv [1] = 0;
			if (setjmp (fperr_jmpbuf))
				z = 0;
			else {
				op (&z, &x, &y);
				w.fv = z;
				if (!(w.lv [0] | w.lv [1]))
					fp_error |= AR_STAT_ZERO;
				else if (w.lv [0] >> 63)
					fp_error |= AR_STAT_NEGATIVE;
			}
			fprintf (fp, "$t %022o %022o %022o %022o %022o %o\n",
				 x, y, z, fp_error);
			fp_error = AR_STAT_OK;
		}
	}
	fclose (fp);
}


main () {

	signal (SIGFPE, fperr);
#if _UNICOS < 8
	signal (SIGERR, fperr);
#endif

	dotest64_2 ("results/cray1_add64", fadd);
	dotest64_2 ("results/cray1_sub64", fsub);
	dotest64_2 ("results/cray1_mul64", fmul);
	dotest64_2 ("results/cray1_recipiter", ARIMULT);
	dotest64_1 ("results/cray1_recip", ARHRECIP);
	dotest64_2 ("results/cray1_div64", fdiv);
	dotest128_2 ("results/cray1_add128", dfadd);
	dotest128_2 ("results/cray1_sub128", dfsub);
	dotest128_2 ("results/cray1_mul128", dfmul);
	dotest128_2 ("results/cray1_div128", dfdiv);
	jmp_on_err = 1;
	dotest64_c ("results/cray1_cabs", ARCABS);
	dotest64_1 ("results/cray1_sqrt", ARSQRT);
	dotest128_1 ("results/cray1_dsqrt", ARDSQRT);
	dotest64_1 ("results/cray1_log", ARLOG);
	dotest128_1 ("results/cray1_dlog", ARDLOG);
	dotest64_1 ("results/cray1_exp", AREXP);
	dotest128_1 ("results/cray1_dexp", ARDEXP);
	dotest64_2 ("results/cray1_powrr", ARPOWRR);
	dotest128_r ("results/cray1_powdr", ARPOWDR);
	dotest128_2 ("results/cray1_powdd", ARPOWDD);
	dotest64_i2 ("results/cray1_powii", ARPOWII);
	dotest64_i ("results/cray1_powri", ARPOWRI);
	dotest128_i ("results/cray1_powdi", ARPOWDI);
	jmp_on_err = 0;
#undef U

	exit (0);
}

#define _fcdtocp(f)     ((char *)(((long)(f))&0xfc000000ffffffff))
#define _fcdlen(f)      ((unsigned)((((long)(f))>>35)&0x7fffff))
AR_NOINTRIN_ERROR(char* intrin_name) {
	int	i;
	char*	name = _fcdtocp(intrin_name);
	for(i=0; i<_fcdlen(intrin_name); i++)
		if(!isalnum(name[i])) break;
	name[i] = '\0';
	ar_internal_error(2017, name, 1);
}

void
ar_internal_error (int msgnum, char *file, int line) {

        PRINTMSG(0, msgnum, Internal, 0, file, line);
        /* PRINTMSG does not return for internal errors */
}


static char USMID [] = "\n%Z%%M%	%I%	%G% %U%\n";
static char rcsid [] = "$Id: ctgen.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $";
