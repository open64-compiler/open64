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
 *	Cray division sequence driver
 *
 *	Sources: hardware reference manuals, double-precision library
 */

#include "arith.internal.h"
#include "int64.h"


int
ar_cfdiv64 (AR_CRAY_64 *x,
	    const AR_CRAY_64 *a,
	    const AR_CRAY_64 *b,
	    int roundmode) {

	int res = AR_STAT_OK;
	AR_CRAY_64 hrecip_b, corr, recip_b;

	res |= ar_c1frecip (&hrecip_b, b);
	res |= ar_cfmul64 (&corr, &hrecip_b, b, AR_RECIPROCAL_ITERATION);
	res |= ar_cfmul64 (&recip_b, &corr, &hrecip_b, AR_UNROUNDED);
	if (ar_state_register.ar_truncate_bits > 0)
	        ar_CRAY_64_trunc(&recip_b);
	res &= ~(AR_STAT_ZERO | AR_STAT_NEGATIVE);
	res |= ar_cfmul64 (x, &recip_b, a, roundmode);
	if (ar_state_register.ar_truncate_bits > 0)
		ar_CRAY_64_trunc(x);
	return res;
}


int
ar_cfdiv128 (AR_CRAY_128 *x,
	     const AR_CRAY_128 *a,
	     const AR_CRAY_128 *b,
	     int roundmode) {

	int res = AR_STAT_OK, carry, norm;
	AR_CRAY_64 sb, rb0, t0, t1, t2, t3;
	AR_CRAY_128 dt3, dt4, dcorr, drecip;

	CRAY128TO64 (sb, *b);
	if (carry = (b->coeff3 >> (AR_CRAY_C3_BITS - 1))) {
		norm = sb.coeff0 >> (AR_CRAY_C0_BITS - 1);
		INCCRAY64 (sb, carry);
		if (norm > (sb.coeff0 >> (AR_CRAY_C0_BITS - 1)))
			sb.expo++;
	}
	sb.coeff0 |= 1 << (AR_CRAY_C0_BITS - 1); /* force normal */
	res |= ar_c1frecip (&rb0, &sb);		       /* approx */
	res |= ar_cfmul64 (&t0, &rb0, &sb, roundmode); /* app * denom */
	res |= ar_cfmul64 (&t1, &rb0, &t0, roundmode); /* a*(a*d) */
	res |= ar_cfsub64 (&t2, &rb0, &t1);			   /* a-a*(a*d) */
	res |= ar_cfadd64 (&t3, &rb0, &t2); 		   /* 2a-a*(a*d) */

	/* Compute correction = T3 * denom */
	CRAY64TO128 (dt3, t3);
	res |= ar_cfmul128 (&dcorr, &dt3, b, roundmode);

	/* Compute funny correction factor (see DDSS code in DP libraries) */
	dt4.coeff0 = dt4.coeff1 = 0;		/* two's comp 64 low bits */
	dt4.coeff2 = ~ dcorr.coeff2;
	dt4.coeff3 = ~ dcorr.coeff3;
	dt4.coeff4 = ~ dcorr.coeff4;
	dt4.coeff5 = ~ dcorr.coeff5;
	carry = 1;
	INCCRAY128 (dt4, carry);
	SHLEFTCRAY128 (dt4);

	if (dt4.coeff1 & 1) {
		dt4.coeff0 = dt4.coeff1 = ~ 0;
		dt4.expo = AR_CRAY_EXPO_BIAS - 1;
	} else {
		carry = 2;
		INCCRAY128 (dt4, carry);
		dt4.coeff0 = dt4.coeff1 = 0;
		SHRIGHTCRAY128 (dt4);
		SHRIGHTCRAY128 (dt4);
		dt4.expo = AR_CRAY_EXPO_BIAS;
		dt4.coeff0 = 1 << (AR_CRAY_C0_BITS - 1);
	}
	dt4.sign = 0;
	dt4.zero = 0;
	
	/* Perform second Newton-Raphson iteration in DP */
	res |= ar_cfmul128 (&drecip, &dt4, &dt3, roundmode);

	/* We have the reciprocal of denom in drecip; multiply by numerator */
	res &= ~(AR_STAT_ZERO | AR_STAT_NEGATIVE);
	res |= ar_cfmul128 (x, &drecip, a, roundmode);

	return res;
}

/*
 *	Portable CRAY-1 half-precision reciprocal approximation emulation
 *
 *	Source: Dick Nelson's reciprocal emulator and an offline diagnostic
 */


/* Reciprocal lookup table, indexed by first seven bits of the argument
 * (after the normalization bit). The values in the table represent
 * approximations to the upper bits of the coefficients of the quantities
 *	2*(1/x) and (1/x)**2
 * to 9 and 18 bits of precision (respectively).
 */
#define LOOKUP_KEY_BITS	7
static unsigned long recip_lookup [1 << LOOKUP_KEY_BITS][2] = {
/*     2*(1/x)	(1/x)**2	*/
	0776,	0774004,
	0772,	0764044,
	0766,	0754144,
	0762,	0744304,
	0756,	0734504,
	0752,	0724744,
	0750,	0721100,
	0744,	0711420,
	0740,	0702000,
	0734,	0672420,
	0732,	0666644,
	0726,	0657344,
	0722,	0650104,
	0720,	0644400,
	0714,	0635220,
	0710,	0626100,
	0706,	0622444,
	0702,	0613404,
	0700,	0610000,
	0674,	0601020,
	0672,	0575444,
	0666,	0566544,
	0664,	0563220,
	0660,	0554400,
	0656,	0551104,
	0652,	0542344,
	0650,	0537100,
	0646,	0533644,
	0642,	0525204,
	0640,	0522000,
	0636,	0516604,
	0632,	0510244,
	0630,	0505100,
	0626,	0501744,
	0624,	0476620,
	0620,	0470400,
	0616,	0465304,
	0614,	0462220,
	0612,	0457144,
	0610,	0454100,
	0604,	0446020,
	0602,	0443004,
	0600,	0440000,
	0576,	0435004,
	0574,	0432020,
	0572,	0427044,
	0570,	0424100,
	0566,	0421144,
	0564,	0416220,
	0562,	0413304,
	0560,	0410400,
	0556,	0405504,
	0554,	0402620,
	0552,	0377744,
	0550,	0375100,
	0546,	0372244,
	0544,	0367420,
	0542,	0364604,
	0540,	0362000,
	0536,	0357204,
	0534,	0354420,
	0532,	0351644,
	0530,	0347100,
	0526,	0344344,
	0524,	0341620,
	0522,	0337104,
	0520,	0334400,
	0520,	0334400,
	0516,	0331704,
	0514,	0327220,
	0512,	0324544,
	0510,	0322100,
	0506,	0317444,
	0506,	0317444,
	0504,	0315020,
	0502,	0312404,
	0500,	0310000,
	0476,	0305404,
	0476,	0305404,
	0474,	0303020,
	0472,	0300444,
	0470,	0276100,
	0470,	0276100,
	0466,	0273544,
	0464,	0271220,
	0462,	0266704,
	0462,	0266704,
	0460,	0264400,
	0456,	0262104,
	0456,	0262104,
	0454,	0257620,
	0452,	0255344,
	0452,	0255344,
	0450,	0253100,
	0446,	0250644,
	0446,	0250644,
	0444,	0246420,
	0442,	0244204,
	0442,	0244204,
	0440,	0242000,
	0436,	0237604,
	0436,	0237604,
	0434,	0235420,
	0434,	0235420,
	0432,	0233244,
	0430,	0231100,
	0430,	0231100,
	0426,	0226744,
	0426,	0226744,
	0424,	0224620,
	0422,	0222504,
	0422,	0222504,
	0420,	0220400,
	0420,	0220400,
	0416,	0216304,
	0416,	0216304,
	0414,	0214220,
	0412,	0212144,
	0412,	0212144,
	0410,	0210100,
	0410,	0210100,
	0406,	0206044,
	0406,	0206044,
	0404,	0204020,
	0404,	0204020,
	0402,	0202004,
	0402,	0202004,
	0400,	0200000
};


int
ar_c1frecip (AR_CRAY_64 *x, const AR_CRAY_64 *b) {

	unsigned long twoa0_9;
	unsigned long t0, t1, t2, t3, t4, t5;
	unsigned int lk;
	int i, res = AR_STAT_OK, b_expo = b->expo, neg = b->sign;
	AR_CRAY_64 bt;

	/* Intermediate 64-bit results, chopped into 16-bit hunks */
	AR_INT_64 b_24, b_37, a0sq_16, a1_18, a1sq_36, twoa1, a2,
			bsave, acc, twoa1_37;

	/* Extract upper 24 and 37 bits of operand coefficient, and place
	 * them RJZF in b_24 and b_37.
	 */
	bt = *b;
	ZERO64 (b_24);
	ZERO64 (b_37);
	for (i = 0; i < 24; i++) {
		SHLEFT64 (b_24);
		SHLEFT64 (b_37);
		t0 = bt.coeff0 >> (AR_CRAY_C0_BITS - 1);
		b_24.part4 |= t0;
		b_37.part4 |= t0;
		SHLEFTCRAY64 (bt);
	}
	for (; i < 37; i++) {
		SHLEFT64 (b_37);
		t0 = bt.coeff0 >> (AR_CRAY_C0_BITS - 1);
		b_37.part4 |= t0;
		SHLEFTCRAY64 (bt);
	}
	b_37.part2 |= 020;	/* force normal */

	/* First step: table lookup, based on the upper seven bits of the
	 * operand (after the normalization bit). Load a nine-bit approximation
	 * to the upper coefficient bits of 2*(1/x) and an eighteen-bit
	 * approximation to the upper coefficient bits of (1/x)**2. The
	 * latter quantity is placed LJZF in a 64-bit field with its lower
	 * two bits truncated.
	 */
	lk = (b->coeff0 >> (AR_CRAY_C0_BITS - (LOOKUP_KEY_BITS + 1))) &
		MASKR (LOOKUP_KEY_BITS);
	twoa0_9 = recip_lookup [lk][0];		 /* 9 bits */
	ZERO64 (a0sq_16);
	a0sq_16.part1 = recip_lookup [lk][1] >> 2; /* 18 -> upper 16 bits */

	/* Do the next stage approximation via partial multiplication with
	 * peculiar rounding.
	 */
	ZERO64 (a1_18);
	SHRIGHT64 (a0sq_16);
	MULSTEP (a1_18, a0sq_16, b_24, 16);
	t0 = (((twoa0_9 << 8) + 1) << 8) +	/* 2*A0 with rounding bits */
		(twoa0_9 >> 1);
	ZERO64 (acc);
	acc.part3 = t0 >> 16;
	acc.part4 = t0;
	NOT64 (acc);
	ADD64 (a1_18, a1_18, acc);
	ZERO64 (acc);
	acc.part4 = 0400;			/* magic rounding */
	ADD64 (a1_18, a1_18, acc);
	NOT64 (a1_18);

	/* Shift right six bits and truncate to a RJZF 18-bit value */
	a1_18.part4 = (a1_18.part4 >> 6) | (a1_18.part3 << 10);
	a1_18.part3 = (a1_18.part3 >> 6) & MASKR (2);
	a1_18.part1 = a1_18.part2 = 0;		/* clip to 18 bits */

	/* Compute and align 2*A1 by left-justifying A1 in a 37-bit field
	 * (a left shift of 19 bits).
	 */
	twoa1_37.part1 = 0;
	twoa1_37.part2 = (a1_18.part3 << 3) | (a1_18.part4 >> 13);
	twoa1_37.part3 = a1_18.part4 << 3;
	twoa1_37.part4 = 0;

	/* Make a copy of the RJZF 18-bit value of A1, placed RJZF in the
	 * upper 19 bits of a scratch value, for use in multiply steps.
	 * Then shift A1 left 17 bits.
	 */
	acc.part1 = (a1_18.part3 << 13) | (a1_18.part4 >> 3);
	acc.part2 = a1_18.part4 << 13;
	acc.part3 = acc.part4 = 0;
	a1_18.part2 = (a1_18.part3 << 1) | (a1_18.part4 >> 15);
	a1_18.part3 = a1_18.part4 << 1;
	a1_18.part4 = 0;

	/* Compute A1**2 to full 36 bits (RJZF in a 64-bit field) */
	ZERO64 (a1sq_36);
	MULSTEP (a1sq_36, acc, a1_18, 18);

	/* Put A1**2 RJZF in the upper 37 bits of its 64-bit field, for
	 * use in multiplication steps. This is a 27-bit left shift.
	 */
	a1sq_36.part1 = (a1sq_36.part2 << 11) |
			(a1sq_36.part3 >> 5);
	a1sq_36.part2 = (a1sq_36.part3 << 11) |
			(a1sq_36.part4 >> 5);
	a1sq_36.part3 = a1sq_36.part4 << 11;
	a1sq_36.part4 = 0;

	/* We now compute the upper 37 bits of B*A1**2 with a partial
	 * pyramid and bizarre rounding. These steps were copied straight
	 * from a diagnostic simulation routine.
	 */
	ZERO64 (a2);
	SHLEFT64 (b_37);
	MULSTEP (a2, a1sq_36, b_37, 4);	/* bits 2**-1 through 2**-4 */
	a2.part4 &= ~MASKR (1);		/* clear lowest accumulator bit */
	COPY64 (bsave, b_37);
	bsave.part4 &= ~MASKR (2);
	SHRIGHT64 (b_37);
	SHRIGHT64 (b_37);

	MULSTEP (a2, a1sq_36, bsave, 4);	/* bits 2**-5 through 2**-8 */

	ZERO64 (acc);
	MULSTEP (acc, a1sq_36, b_37, 3);	/* bits 2**-9 through 2**-12 */
	SHRIGHT64 (acc);
	SHRIGHT64 (b_37);
	MULSTEP (acc, a1sq_36, b_37, 1);
	SHRIGHT64 (b_37);
	SHRIGHT64 (acc);
	ADD64 (a2, a2, acc);

	ZERO64 (acc);
	MULSTEP (acc, a1sq_36, b_37, 6);	/* bits 2**-13 through 2**-20 */
	b_37.part4 &= ~MASKR (1);
	MULSTEP (acc, a1sq_36, b_37, 2);
	acc.part4 &= ~MASKR (1);
	ADD64 (a2, a2, acc);

	COPY64 (bsave, b_37);
	bsave.part4 &= ~MASKR (2);
	SHRIGHT64 (b_37);
	SHRIGHT64 (b_37);
	MULSTEP (a2, a1sq_36, bsave, 4);	/* bits 2**-21 through 2**-24 */

	ZERO64 (acc);
	MULSTEP (acc, a1sq_36, b_37, 3);	/* bits 2**-25 through 2**-28 */
	SHRIGHT64 (acc);
	SHRIGHT64 (b_37);
	MULSTEP (acc, a1sq_36, b_37, 1);
	SHRIGHT64 (b_37);
	SHRIGHT64 (acc);
	ADD64 (a2, a2, acc);

	ZERO64 (acc);
	MULSTEP (acc, a1sq_36, b_37, 5);	/* bits 2**-29 through 2**-36 */
	SHLEFT64 (a1sq_36);			/* skip iter 34! */
	SHRIGHT64 (b_37);
	MULSTEP (acc, a1sq_36, b_37, 2);
	acc.part4 &= ~MASKR (1);
	ADD64 (a2, a2, acc);

	/* Rounding step in computation of A1**2 */
	SHRIGHT64 (a2);
	if (a2.part4 & 1) {
		SHRIGHT64 (a2);
		INC64 (a2);
	} else
		SHRIGHT64 (a2);
	a2.part4 &= ~MASKR (2);			/* knock off lower 2 bits */

	/* Prepare final coefficient == -(B*A1**2 - 2*A1) */
	NOT64 (twoa1_37);
	ADD64 (a2, a2, twoa1_37);
	INC64 (a2);
	NOT64 (a2);

	/* Move upper 33 bits of 37-bit final coefficient to result in
	 * CRAY-1 floating-point format.
	 */
	a2.part4 = (a2.part4 >> 3) | (a2.part3 << 13);
	a2.part3 = (a2.part3 >> 3) | (a2.part2 << 13);
	a2.part2 >>= 3;
	ZEROCRAY64 (*x);
	for (i = 0; i < 33; i++) {
		SHRIGHTCRAY64 (*x);
		x->coeff0 |= a2.part4 << (AR_CRAY_C0_BITS - 1);
		SHRIGHT64 (a2);
	}

	/* Copy sign bit */
	if (x->sign = neg)
		res |= AR_STAT_NEGATIVE;

	/* Compute exponent */
	if (b_expo > AR_CRAY_MAX_EXPO || b_expo <= AR_CRAY_MIN_EXPO + 1) {

		/* Set out of range exponent */
		x->expo = AR_CRAY_MAX_EXPO + 1;

		/* Clear normalization bit */
		x->coeff0 &= MASKR (AR_CRAY_C0_BITS - 1);

		return res | AR_STAT_OVERFLOW;

	}

	/* Force normalization bit */
	x->coeff0 |= 1 << (AR_CRAY_C0_BITS - 1);

	/* Exponent */
	x->expo = 2 * AR_CRAY_EXPO_BIAS - b_expo - 1;

	return res | AR_STAT_INEXACT;
}


static char USMID [] = "\n%Z%%M%	%I%	%G% %U%\n";
static char rcsid [] = "$Id: cray_fdiv.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $";
