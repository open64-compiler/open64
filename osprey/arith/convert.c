/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
 */

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
 *	Conversions between floating-point and two's-complement fixed
 *	point representations. All rounding is toward zero (i.e. chopped)
 *	for the Cray format conversions.
 */

#include <string.h>
#include "arith.internal.h"
#include "int64.h"


/* Cray single -> integer */
int
ar_cfix64 (AR_INT_64 *fix,
	   const AR_CRAY_64 *flt,
	   int bitsize) {

	int res = AR_STAT_OK, neg;
	int shift = AR_CRAY_EXPO_BIAS + AR_CRAY64_COEFF_BITS - flt->expo - 1;
	AR_CRAY_64 a = *flt;

	if (!(a.coeff0 | a.coeff1 | a.coeff2)) {
		ZERO64 (*fix);
		return AR_STAT_ZERO;
	}
	if (shift >= AR_CRAY64_COEFF_BITS) {
		ZERO64 (*fix);
		return AR_STAT_ZERO | AR_STAT_UNDERFLOW;
	}
	if (shift < AR_CRAY64_COEFF_BITS - bitsize)
		res |= AR_STAT_OVERFLOW;

	neg = a.sign;
	a.sign = 0;
	a.expo = 0;
	CRAY64TOINT64 (*fix, a);

	for ( ; shift < 0; shift++)
		SHLEFT64 (*fix);
	for ( ; shift > 0; shift--) {
		if (fix->part4 & 1)
			res |= AR_STAT_INEXACT;
		SHRIGHT64X (*fix);
	}

	if (fix->part1&0x8000)
		res |= (AR_STAT_OVERFLOW|AR_STAT_SEMIVALID);

	if (neg) {
		NOT64 (*fix);
		INC64 (*fix);
	}

	if (!(fix->part1 | fix->part2 | fix->part3 | fix->part4))
		res |= AR_STAT_ZERO;
	else if (neg)
		res |= AR_STAT_NEGATIVE;

	return res;
}


/* integer -> Cray single */
int
ar_cflt64 (AR_CRAY_64 *flt,
	   const AR_INT_64 *fix,
	   int is_unsigned) {

	int res = AR_STAT_OK, neg = 0;
	long expo = AR_CRAY_EXPO_BIAS + AR_CRAY64_COEFF_BITS - 1;
	AR_INT_64 val;

	if (!(fix->part1 | fix->part2 | fix->part3 | fix->part4)) {
		ZEROCRAY64 (*flt);
		return AR_STAT_ZERO;
	}

	COPY64 (val, *fix);
	if (SIGNBIT (*fix) && !is_unsigned) {
		NEG64 (val);
		neg = 1;
		res |= AR_STAT_NEGATIVE;
	}

	while (val.part1 >> (16 - (AR_CRAY_EXPO_BITS + 1))) {
		if (val.part4 & 1)
			res |= AR_STAT_INEXACT;
		expo++;
		SHRIGHT64 (val);
	}

	INT64TOCRAY64 (*flt, val);
	while (!(flt->coeff0 >> (AR_CRAY_C0_BITS - 1))) {
		expo--;
		SHLEFTCRAY64 (*flt);
	}

	flt->sign = neg;
	flt->expo = expo;
	return res;
}


/* Cray double -> integer */
int
ar_cfix128 (AR_INT_64 *fix,
	    const AR_CRAY_128 *flt,
	    int bitsize) {

	int res = AR_STAT_OK, neg;
	int shift = AR_CRAY_EXPO_BIAS + AR_CRAY64_COEFF_BITS - flt->expo - 1;
	AR_CRAY_128 a = *flt;
	AR_CRAY_64 b;

	if (!(a.coeff0 | a.coeff1 | a.coeff2 |
	      a.coeff3 | a.coeff4 | a.coeff5)) {
		ZERO64 (*fix);
		return AR_STAT_ZERO;
	}
	if (shift >= AR_CRAY64_COEFF_BITS) {
		ZERO64 (*fix);
		return AR_STAT_ZERO | AR_STAT_UNDERFLOW;
	}
	if (shift < AR_CRAY64_COEFF_BITS - bitsize)
		res |= AR_STAT_OVERFLOW;

	neg = a.sign;
	a.sign = 0;
	a.expo = 0;
	CRAY128TO64 (b, a);
	CRAY64TOINT64 (*fix, b);

	for ( ; shift < 0; shift++) {
		SHLEFT64 (*fix);
		SHLEFTCRAY128 (a);
		fix->part4 |= a.coeff2 & 1;
	}
	if (a.coeff3 | a.coeff4 | a.coeff5)
		res |= AR_STAT_INEXACT;
	for ( ; shift > 0; shift--) {
		if (fix->part4 & 1)
			res |= AR_STAT_INEXACT;
		SHRIGHT64X (*fix);
	}

	if (fix->part1&0x8000)
		res |= (AR_STAT_OVERFLOW|AR_STAT_SEMIVALID);

	if (neg) {
		NOT64 (*fix);
		INC64 (*fix);
	}

	if (!(fix->part1 | fix->part2 | fix->part3 | fix->part4))
		res |= AR_STAT_ZERO;
	else if (neg)
		res |= AR_STAT_NEGATIVE;

	return res;
}


/* integer -> Cray double */
int
ar_cflt128 (AR_CRAY_128 *flt,
	    const AR_INT_64 *fix,
	    int is_unsigned) {

	int res = AR_STAT_OK, neg = 0;
	long expo = AR_CRAY_EXPO_BIAS + AR_CRAY64_COEFF_BITS - 1;
	AR_INT_64 val;
	AR_CRAY_64 sing;
	unsigned long chop = 0;

	if (!(fix->part1 | fix->part2 | fix->part3 | fix->part4)) {
		ZEROCRAY128 (*flt);
		return AR_STAT_ZERO;
	}

	COPY64 (val, *fix);
	if (SIGNBIT (*fix) && !is_unsigned) {
		NEG64 (val);
		neg = 1;
		res |= AR_STAT_NEGATIVE;
	}

	while (val.part1 >> (16 - (AR_CRAY_EXPO_BITS + 1))) {
		chop = (chop >> 1) |
		       ((val.part4 & 1) << (AR_CRAY_C3_BITS - 1));
		expo++;
		SHRIGHT64 (val);
	}

	INT64TOCRAY64 (sing, val);
	CRAY64TO128 (*flt, sing);
	flt->coeff3 = chop;
	while (!(flt->coeff0 >> (AR_CRAY_C0_BITS - 1))) {
		expo--;
		SHLEFTCRAY128 (*flt);
	}

	flt->sign = neg;
	flt->expo = expo;
	return res;
}


/* IEEE 32 bit -> integer */
int
ar_ifix32 (AR_INT_64 *fix,
	   const AR_IEEE_32 *flt,
	   int bitsize,
	   int roundmode) {

	int res = AR_STAT_OK, neg;
	int shift = AR_IEEE32_EXPO_BIAS + AR_IEEE32_COEFF_BITS - flt->expo;
	unsigned long rbits = 0;
	AR_IEEE_32 a = *flt;

	if (a.expo == 0 && !IS_IEEE32_NZ_COEFF(&a)) {
		ZERO64 (*fix);
		return AR_STAT_ZERO;
	}
	if (shift > AR_IEEE32_COEFF_BITS + AR_IEEE32_ROUND_BITS)
		shift = AR_IEEE32_COEFF_BITS + AR_IEEE32_ROUND_BITS;

	if (shift < AR_IEEE32_COEFF_BITS + 1 - bitsize)
		res |= AR_STAT_OVERFLOW;

	neg = a.sign;
	a.sign = 0;
	a.expo = !!a.expo;
	IEEE32TOINT64 (*fix, a);

	for ( ; shift < 0; shift++)
		SHLEFT64 (*fix);
	for ( ; shift > 0; shift--) {
		rbits = rbits & 1 | (rbits >> 1) |
			((fix->part4 & 1) << (AR_IEEE32_ROUND_BITS - 1));
		SHRIGHT64 (*fix);
	}
	if (rbits)
		res |= AR_STAT_INEXACT;

	switch (roundmode) {
	case AR_ROUND_PLUS_INFINITY:
		if (!a.sign && rbits)
			INC64 (*fix);
		break;
	case AR_ROUND_MINUS_INFINITY:
		if (a.sign && rbits)
			DEC64 (*fix);
		break;
	case AR_ROUND_ZERO:
		break;
	default:
		if (rbits >> (AR_IEEE32_ROUND_BITS - 1) &&
		    (rbits & MASKR (AR_IEEE32_ROUND_BITS - 1) ||
		     fix->part4 & 1))
			INC64 (*fix);
		break;
	}

	if (fix->part1&0x8000)
		res |= (AR_STAT_OVERFLOW|AR_STAT_SEMIVALID);

	if (neg) {
		NOT64 (*fix);
		INC64 (*fix);
	}

	if (!(fix->part1 | fix->part2 | fix->part3 | fix->part4))
		res |= AR_STAT_ZERO;
	else if (neg)
		res |= AR_STAT_NEGATIVE;

	return res;
}


/* IEEE 64 bit -> integer */
int
ar_ifix64 (AR_INT_64 *fix,
	   const AR_IEEE_64 *flt,
	   int bitsize,
	   int roundmode) {

	int res = AR_STAT_OK, neg;
	int shift = AR_IEEE64_EXPO_BIAS + AR_IEEE64_COEFF_BITS - flt->expo;
	unsigned long rbits = 0;
	AR_IEEE_64 a = *flt;

	if (a.expo == 0 && !IS_IEEE64_NZ_COEFF(&a)) {
		ZERO64 (*fix);
		return AR_STAT_ZERO;
	}
	if (shift > AR_IEEE64_COEFF_BITS + AR_IEEE64_ROUND_BITS)
		shift = AR_IEEE64_COEFF_BITS + AR_IEEE64_ROUND_BITS;

	if (shift < AR_IEEE64_COEFF_BITS + 1 - bitsize)
		res |= AR_STAT_OVERFLOW;

	neg = a.sign;
	a.sign = 0;
	a.expo = !!a.expo;
	IEEE64TOINT64 (*fix, a);

	for ( ; shift < 0; shift++)
		SHLEFT64 (*fix);
	for ( ; shift > 0; shift--) {
		rbits = rbits & 1 | (rbits >> 1) |
			((fix->part4 & 1) << (AR_IEEE64_ROUND_BITS - 1));
		SHRIGHT64 (*fix);
	}
	if (rbits)
		res |= AR_STAT_INEXACT;

	switch (roundmode) {
	case AR_ROUND_PLUS_INFINITY:
		if (!a.sign && rbits)
			INC64 (*fix);
		break;
	case AR_ROUND_MINUS_INFINITY:
		if (a.sign && rbits)
			DEC64 (*fix);
		break;
	case AR_ROUND_ZERO:
		break;
	default:
		if (rbits >> (AR_IEEE64_ROUND_BITS - 1) &&
		    (rbits & MASKR (AR_IEEE64_ROUND_BITS - 1) ||
		     fix->part4 & 1))
			INC64 (*fix);
		break;
	}

	if (fix->part1&0x8000)
		res |= (AR_STAT_OVERFLOW|AR_STAT_SEMIVALID);

	if (neg) {
		NOT64 (*fix);
		INC64 (*fix);
	}

	if (!(fix->part1 | fix->part2 | fix->part3 | fix->part4))
		res |= AR_STAT_ZERO;
	else if (neg)
		res |= AR_STAT_NEGATIVE;

	return res;
}


/* IEEE 128 bit -> integer */
int
ar_ifix128 (AR_INT_64 *fix,
	    const AR_IEEE_128 *flt,
	    int bitsize,
	    int roundmode)
{
	int res = AR_STAT_OK, neg;
	int shift = AR_IEEE128_EXPO_BIAS + AR_IEEE128_COEFF_BITS-64+15 - flt->expo;
	unsigned long rbits = 0;
	AR_IEEE_128 a = *flt;

	/*
	 * Use native arithmetic for MIPS.
	 */
	if (HOST_IS_MIPS) {
		AR_TYPE ty = AR_Int_64_S;
		long double ld;

		ld = *(long double *) flt;
		*(long long *) fix = ld;
		return AR_status ((AR_DATA *) fix, &ty);
	}

	if (a.expo == 0 && !IS_IEEE128_NZ_COEFF(&a)) {
		ZERO64 (*fix);
		return AR_STAT_ZERO;
	}
	if (shift > AR_IEEE128_COEFF_BITS-64+15 + AR_IEEE128_ROUND_BITS)
		shift = AR_IEEE128_COEFF_BITS-64+15 + AR_IEEE128_ROUND_BITS;

	if (shift <= 0) {
		if(shift == 0)
			res |= AR_STAT_SEMIVALID;
		res |= AR_STAT_OVERFLOW;
		shift = 0;
	}

	neg = a.sign;
	fix->part1 = ((!!a.expo)<<15) | (a.coeff0>>1);
	fix->part2 =   (a.coeff0<<15) | (a.coeff1>>1);
	fix->part3 =   (a.coeff1<<15) | (a.coeff2>>1);
	fix->part4 =   (a.coeff2<<15) | (a.coeff3>>1);

	for ( ; shift > 0; shift--) {
		rbits = rbits & 1 | (rbits >> 1) |
			((fix->part4 & 1) << (AR_IEEE128_ROUND_BITS - 1));
		SHRIGHT64 (*fix);
	}
	if (rbits)
		res |= AR_STAT_INEXACT;

	switch (roundmode) {
	case AR_ROUND_PLUS_INFINITY:
		if (!a.sign && rbits)
			INC64 (*fix);
		break;
	case AR_ROUND_MINUS_INFINITY:
		if (a.sign && rbits)
			DEC64 (*fix);
		break;
	case AR_ROUND_ZERO:
		break;
	default:
		if (rbits >> (AR_IEEE128_ROUND_BITS - 1) &&
		    (rbits & MASKR (AR_IEEE128_ROUND_BITS - 1) ||
		     fix->part4 & 1))
			INC64 (*fix);
		break;
	}

	if (neg) {
		NOT64 (*fix);
		INC64 (*fix);
	}

	if (!(fix->part1 | fix->part2 | fix->part3 | fix->part4))
		res |= AR_STAT_ZERO;
	else if (neg)
		res |= AR_STAT_NEGATIVE;

	return res;
}


/* integer -> IEEE 32 bit */
int
ar_iflt32 (AR_IEEE_32 *flt,
	   const AR_INT_64 *fix,
	   int is_unsigned,
	   int roundmode) {

	int neg = 0;
	unsigned long lbits, rbits;
	AR_INT_64 val;

	COPY64 (val, *fix);
	if (SIGNBIT (val) && !is_unsigned) {
		NEG64 (val);
		neg = 1;
	}

	rbits = 0;
	while (val.part1 | val.part2) {
		rbits = rbits & 1 | (rbits >> 1) |
		        ((val.part4 & 1) << (AR_IEEE32_ROUND_BITS - 1));
		SHRIGHT64 (val);
	}

	INT64TOIEEE32 (*flt, val);
	lbits = val.part3 >> (16 - (AR_IEEE32_EXPO_BITS + 1));
	flt->sign = neg;

	return ar_i32norm (AR_IEEE32_EXPO_BIAS + AR_IEEE32_COEFF_BITS,
			   lbits,
			   rbits,
			   flt,
			   roundmode);
}


/* integer -> IEEE 64 bit */
int
ar_iflt64 (AR_IEEE_64 *flt,
	   const AR_INT_64 *fix,
	   int is_unsigned,
	   int roundmode) {

	int neg = 0;
	unsigned long lbits;
	AR_INT_64 val;

	if (SIGNBIT (*fix) && !is_unsigned) {
		COPY64 (val, *fix);
		NEG64 (val);
		fix = &val;
		neg = 1;
	}

	INT64TOIEEE64 (*flt, *fix);
	lbits = fix->part1 >> (16 - (AR_IEEE64_EXPO_BITS + 1));
	flt->sign = neg;

	return ar_i64norm (AR_IEEE64_EXPO_BIAS + AR_IEEE64_COEFF_BITS,
			   lbits,
			   0, /* rbits */
			   flt,
			   roundmode);
}


/* integer -> IEEE 128 bit */
int
ar_iflt128 (AR_IEEE_128 *flt,
	   const AR_INT_64 *fix,
	   int is_unsigned,
	   int roundmode) {

	int neg = 0;
	unsigned long lbits;
	AR_INT_64 val;

	/*
	 * Use native arithmetic for MIPS.
	 */
	if (HOST_IS_MIPS) {
		long double ld;
		long long li;
		AR_TYPE ty = AR_Float_IEEE_NR_128;

		li = *(long long *) fix;
		ld = li;
		memcpy(flt,&ld,sizeof(long double));
		return AR_status ((AR_DATA *) flt, &ty);
	}

	if (SIGNBIT (*fix) && !is_unsigned) {
		COPY64 (val, *fix);
		NEG64 (val);
		fix = &val;
		neg = 1;
	}

	flt->coeff3 = flt->coeff4 = flt->coeff5 = flt->coeff6 = 0;
	INT64TOIEEE128 (*flt, *fix);
	lbits = fix->part1 >> (16 - (AR_IEEE128_EXPO_BITS + 1));
	flt->sign = neg;

	return ar_i128norm (AR_IEEE128_EXPO_BIAS + AR_IEEE128_COEFF_BITS-64,
			   lbits,
			   0, /* rbits */
			   flt,
			   roundmode);
}


/* IEEE 64 -> Cray single */
int
ar_itoc64 (AR_CRAY_64 *to,
	   const AR_IEEE_64 *from,
	   int roundmode) {

	int toexpo;
	AR_INT_64 coeff;
	AR_IEEE_64 v, fact;
	AR_CRAY_64 zero;

	if (from->expo > AR_IEEE64_MAX_EXPO) {
		to->expo = AR_CRAY_MAX_EXPO + 1;
		to->coeff0 = 1 << (AR_CRAY_C0_BITS - 1);
		to->coeff1 = to->coeff2 = 0;
		return AR_STAT_OVERFLOW;
	}

	/* Extract the coefficient as an unsigned integer, rounded to
	 * Cray precision.
	 */
	v = *from;
	v.sign = 0;
	toexpo = from->expo + AR_CRAY_EXPO_BIAS - AR_IEEE64_EXPO_BIAS;
	if (!v.expo) {
		/* Denormalized value; try to normalize it */
		ZEROIEEE64 (fact);
		fact.expo = AR_IEEE64_EXPO_BIAS + AR_IEEE64_COEFF_BITS;
		(void) ar_ifmul64 (&v, &v, &fact, roundmode);
		if (!v.expo) {
			ZEROCRAY64 (*to);
			return AR_STAT_ZERO;
		}
		toexpo = v.expo + 1 + AR_CRAY_EXPO_BIAS - AR_IEEE64_EXPO_BIAS -
				AR_IEEE64_COEFF_BITS;
	}
	v.expo = AR_IEEE64_EXPO_BIAS + AR_CRAY64_COEFF_BITS - 1;
	(void) ar_ifix64 (&coeff, &v, AR_IEEE64_COEFF_BITS, roundmode);
	INT64TOCRAY64 (*to, coeff);
	if (to->expo) {
		SHRIGHTCRAY64 (*to);
		to->coeff0 |= 1 << (AR_CRAY_C0_BITS - 1);
		toexpo++;
	}
	to->sign = from->sign;
	to->expo = toexpo;

	/* Normalize and return */
	ZEROCRAY64 (zero);
	return ar_cfadd64 (to, to, &zero);
}


/* IEEE 128 -> Cray double */
int
ar_itoc128(AR_CRAY_128 *to,
	   const AR_IEEE_128 *from,
	   int roundmode) {

	int toexpo;
	AR_IEEE_128 v, fact;
	AR_CRAY_128 zero;

	if (from->expo > AR_IEEE128_MAX_EXPO) {
		to->expo = AR_CRAY_MAX_EXPO + 1;
		to->coeff0 = 1 << (AR_CRAY_C0_BITS - 1);
		to->coeff1 = to->coeff2 = to->coeff3 = to->coeff4 = to->coeff5 = 0;
		return AR_STAT_OVERFLOW;
	}

	v = *from;
	v.sign = 0;
	toexpo = from->expo + AR_CRAY_EXPO_BIAS - AR_IEEE128_EXPO_BIAS;
	if (!v.expo) {
		/* Denormalized value; try to normalize it */
		ZEROIEEE128 (fact);
		fact.expo = AR_IEEE128_EXPO_BIAS + AR_IEEE128_COEFF_BITS;
		(void) ar_ifmul128 (&v, &v, &fact, roundmode);
		if (!v.expo) {
			ZEROCRAY128 (*to);
			return AR_STAT_ZERO;
		}
		toexpo = v.expo + 2 + AR_CRAY_EXPO_BIAS - AR_IEEE128_EXPO_BIAS -
				AR_IEEE128_COEFF_BITS;
	}

	if(toexpo > AR_CRAY_MAX_EXPO) {
		to->expo = AR_CRAY_MAX_EXPO + 1;
		to->coeff0 = 1 << (AR_CRAY_C0_BITS - 1);
		to->coeff1 = to->coeff2 = to->coeff3 = to->coeff4 = to->coeff5 = 0;
		return AR_STAT_OVERFLOW;
	}

	to->coeff0 = v.coeff0;
	to->coeff1 = v.coeff1;
	to->coeff2 = v.coeff2;
	to->zero   = 0;
	to->coeff3 = v.coeff3;
	to->coeff4 = v.coeff4;
	to->coeff5 = v.coeff5;
	SHRIGHTCRAY128(*to);
	to->coeff0 |= 1 << (AR_CRAY_C0_BITS - 1);

	to->sign = from->sign;
	to->expo = toexpo;

	/* Normalize, set status, and return */
	ZEROCRAY128 (zero);
	return ar_cfadd128(to, to, &zero);
}


/* IEEE 64 -> Cray double */
int
ar_i64toc128 (AR_CRAY_128 *to, const AR_IEEE_64 *from) {

	int res, toexpo;
	AR_INT_64 coeff;
	AR_IEEE_64 v, fact;
	AR_CRAY_64 sing;
	AR_CRAY_128 zero;

	if (from->expo > AR_IEEE64_MAX_EXPO) {
		to->expo = AR_CRAY_MAX_EXPO + 1;
		to->coeff0 = 1 << (AR_CRAY_C0_BITS - 1);
		to->coeff1 = to->coeff2 = 0;
		return AR_STAT_OVERFLOW;
	}

	/* Extract the coefficient as an unsigned integer */
	v = *from;
	v.sign = 0;
	toexpo = from->expo + AR_CRAY_EXPO_BIAS - AR_IEEE64_EXPO_BIAS;
	if (!v.expo) {
		/* Denormalized value; try to normalize it */
		ZEROIEEE64 (fact);
		fact.expo = AR_IEEE64_EXPO_BIAS + AR_IEEE64_COEFF_BITS;
		(void) ar_ifmul64 (&v, &v, &fact, AR_ROUND_NEAREST);
		if (!v.expo) {
			ZEROCRAY128 (*to);
			return AR_STAT_ZERO;
		}
		toexpo = v.expo + 1 + AR_CRAY_EXPO_BIAS - AR_IEEE64_EXPO_BIAS -
				AR_IEEE64_COEFF_BITS;
	}
	v.expo = AR_IEEE64_EXPO_BIAS + AR_IEEE64_COEFF_BITS;
	(void) ar_ifix64 (&coeff, &v, AR_IEEE64_COEFF_BITS, AR_ROUND_NEAREST);
	INT64TOCRAY64 (sing, coeff);
	CRAY64TO128 (*to, sing);
	toexpo -= AR_IEEE64_COEFF_BITS + 1 - AR_CRAY64_COEFF_BITS;
	while (to->expo) {
		SHRIGHTCRAY128 (*to);
		to->coeff0 |= to->expo << (AR_CRAY_C0_BITS - 1);
		to->expo >>= 1;
		toexpo++;
	}
	to->sign = from->sign;
	to->expo = toexpo;

	/* Normalize and return */
	ZEROCRAY128 (zero);
	return ar_cfadd128 (to, to, &zero);
}


/* Cray 64 -> IEEE 64 */
int
ar_ctoi64 (AR_IEEE_64 *to, const AR_CRAY_64 *from) {

	int res = AR_STAT_OK;
	AR_INT_64 coeff;
	AR_CRAY_64 v = *from;
	int expo;

	if (from->expo > AR_CRAY_MAX_EXPO) {
		/* Overflow yields a quiet NaN */
		if (HOST_IS_MIPS) {
			QNaNIEEE64 (to);
		}
		else {
			ZEROIEEE64 (*to);
			if (to->sign = v.sign)
				res |= AR_STAT_NEGATIVE;
			to->expo = AR_IEEE64_MAX_EXPO + 1;
			to->coeff0 = 1 << (AR_IEEE64_C0_BITS - 1);
		}
		return res | AR_STAT_OVERFLOW;
	}

	v.sign = 0;
	v.expo = 0;
	CRAY64TOINT64 (coeff, v);
	INT64TOIEEE64 (*to, coeff);
	to->sign = from->sign;
	expo = from->expo + AR_IEEE64_EXPO_BIAS - AR_CRAY_EXPO_BIAS +
			    AR_IEEE64_COEFF_BITS + 1 - AR_CRAY64_COEFF_BITS;
	if (expo <= 0)
		expo--;

	return ar_i64norm (expo,
			   0, /* lbits */
			   0, /* rbits */
			   to,
			   AR_ROUND_NEAREST);
}


/* Cray 128 -> IEEE 128 */
int
ar_ctoi128 (AR_IEEE_128 *to, const AR_CRAY_128 *from) {

	int res = AR_STAT_OK;
	AR_CRAY_128 v = *from;
	int expo;

	if (from->expo > AR_CRAY_MAX_EXPO) {
		/* Overflow yields a quiet NaN */
		if (HOST_IS_MIPS) {
			QNaNIEEE128 (to);
		}
		else {
			ZEROIEEE128 (*to);
			if (to->sign = v.sign)
				res |= AR_STAT_NEGATIVE;
			to->expo = AR_IEEE128_MAX_EXPO + 1;
			to->coeff0 = 1 << (AR_IEEE128_C0_BITS - 1);
		}
		return res | AR_STAT_OVERFLOW;
	}

	to->sign = v.sign;
	to->expo = 0;
	to->coeff0 = v.coeff0;
	to->coeff1 = v.coeff1;
	to->coeff2 = v.coeff2;
	to->coeff3 = v.coeff3;
	to->coeff4 = v.coeff4;
	to->coeff5 = v.coeff5;
	to->coeff6 = 0;
	expo = v.expo + AR_IEEE128_EXPO_BIAS - AR_CRAY_EXPO_BIAS +
			    AR_IEEE128_COEFF_BITS + 1 - AR_CRAY128_COEFF_BITS;
	if (expo <= 0)
		expo--;

	return ar_i128norm (expo,
			   0, /* lbits */
			   0, /* rbits */
			   to,
			   AR_ROUND_NEAREST);
}


/* Cray 128 -> IEEE 64 */
int
ar_c128toi64 (AR_IEEE_64 *to, const AR_CRAY_128 *from) {

	int res = AR_STAT_OK;
	AR_INT_64 coeff;
	AR_CRAY_128 v = *from;
	AR_CRAY_64 v64;
	int i;
	int rbits;
	int expo;

	if (from->expo > AR_CRAY_MAX_EXPO) {
		/* Overflow yields a quiet NaN */
		if (HOST_IS_MIPS) {
			QNaNIEEE64 (to);
		}
		else {
			ZEROIEEE64 (*to);
			if (to->sign = v.sign)
				res |= AR_STAT_NEGATIVE;
			to->expo = AR_IEEE64_MAX_EXPO + 1;
			to->coeff0 = 1 << (AR_IEEE64_C0_BITS - 1);
		}
		return res | AR_STAT_OVERFLOW;
	}

	v.sign = 0;
	v.expo = 0;
	CRAY128TO64 (v64, v);
	CRAY64TOINT64 (coeff, v64);
	INT64TOIEEE64 (*to, coeff);

	/* Move upper bits of second part into low bits */
	for (i = 0; i < AR_IEEE64_COEFF_BITS - AR_CRAY64_COEFF_BITS; i++)
		SHLEFTIEEE64 (*to);
	to->coeff3 |= v.coeff3 >> (AR_CRAY_C3_BITS -
				   (AR_IEEE64_COEFF_BITS -
				    AR_CRAY64_COEFF_BITS));

	/* Compute guard, round, and sticky bits */
	rbits = (v.coeff3 >> (AR_CRAY_C3_BITS - AR_IEEE64_ROUND_BITS -
			      (AR_IEEE64_COEFF_BITS - AR_CRAY64_COEFF_BITS))) &
		MASKR (AR_IEEE64_ROUND_BITS);
	rbits |= !!(v.coeff3 & MASKR (AR_CRAY_C3_BITS - AR_IEEE64_ROUND_BITS -
				      (AR_IEEE64_COEFF_BITS -
				       AR_CRAY64_COEFF_BITS) - 1) |
		    v.coeff4 | v.coeff5);

	to->sign = from->sign;
	expo = from->expo + AR_IEEE64_EXPO_BIAS - AR_CRAY_EXPO_BIAS + 1;
	if (expo <= 0)
		expo--;

	return ar_i64norm (expo,
			   0, /* lbits */
			   rbits,
			   to,
			   AR_ROUND_NEAREST);
}


/* Cray double -> single */
int
ar_c128to64 (AR_CRAY_64 *s, const AR_CRAY_128 *d) {

	int res = AR_STAT_OK;

	if (!(d->sign | d->expo | d->coeff0 | d->coeff1 | d->coeff2))
		res |= AR_STAT_ZERO;
	else if (d->sign)
		res |= AR_STAT_NEGATIVE;

	/* Truncated value is returned since that is what all compilers do */
	CRAY128TO64 (*s, *d);

	return res;
}

/* Cray single -> double */
int
ar_c64to128 (AR_CRAY_128 *d, const AR_CRAY_64 *s) {

	int res = AR_STAT_OK;

	if (!(s->sign | s->expo | s->coeff0 | s->coeff1 | s->coeff2))
		res |= AR_STAT_ZERO;
	else if (s->sign)
		res |= AR_STAT_NEGATIVE;

	CRAY64TO128(*d, *s);

	return res;
}


/* IEEE 64 -> IEEE 32 */
int
ar_i64to32 (AR_IEEE_32 *s, const AR_IEEE_64 *d, const int roundmode) {

	int res = AR_STAT_OK;
	int expo;
	unsigned long lbits, rbits;

#	if AR_IEEE32_C0_BITS < AR_IEEE64_C0_BITS
#		error ar_i64to32 has coefficient shifts miscoded.
#	else
#		define COEFF_BIT_OFF  (AR_IEEE32_C0_BITS - AR_IEEE64_C0_BITS)
#	endif

	if (d->expo > AR_IEEE64_MAX_EXPO) {
		if (IS_IEEE64_NZ_COEFF(d)) {
			/* WARNING: the following code makes some assumptions about
			 * bit field sizes!
			 *
			 * Incoming quantity is a NaN; return a NaN with the
			 * same high AR_IEEE32_COEFF_BITS bits.  The result
			 * must have at least one non-zero coefficient bit.
			 */
			ZEROIEEE32 (*s);
			s->sign    = d->sign;
			s->expo    = AR_IEEE32_MAX_EXPO + 1;
			s->coeff1  = (d->coeff2 >> (AR_IEEE64_C2_BITS - COEFF_BIT_OFF)) |
						 (d->coeff1 << COEFF_BIT_OFF);
			s->coeff0  = (d->coeff1 >> (AR_IEEE64_C1_BITS - COEFF_BIT_OFF)) |
						 (d->coeff0 << COEFF_BIT_OFF);
			if (!IS_IEEE32_NZ_COEFF(s)) {
				s->coeff1 = 1;
			}
			return AR_STAT_UNDEFINED;
		} else {
			/* It's +/-Inf */
			ZEROIEEE32 (*s);
			s->expo = AR_IEEE32_MAX_EXPO + 1;
			if (s->sign = d->sign)
				res |= AR_STAT_NEGATIVE;
			return res | AR_STAT_OVERFLOW;
		}
	}

	if (d->sign)
		res |= AR_STAT_NEGATIVE;

	/* Incoming denorm must underflow to zero. */
	if (!d->expo) {
		s->sign = d->sign;
		s->zero = s->expo = s->coeff0 = s->coeff1 = 0;
		res |= AR_STAT_ZERO;
		if (IS_IEEE64_NZ_COEFF(d))
		    if(ar_state_register.ar_denorms_trap)
				res |= AR_STAT_UNDERFLOW;
		    else
				res |= AR_STAT_UNDEFINED;
		return res;
	}

	lbits = 1;
	expo = d->expo - AR_IEEE64_EXPO_BIAS + AR_IEEE32_EXPO_BIAS;
	if (expo <= 0)
		expo--;

	/* WARNING: the following code makes some assumptions about
	 * bit field sizes!
	 *
	 * Compress rightmost 29 bits of incoming coefficient into
	 * a 3-bit guard/round/sticky set of rounding bits.
	 */
	rbits = ((d->coeff2 >> 10) & 07) |			/* G and R */
		!!(d->coeff2 & MASKR (10) | d->coeff3);		/* sticky  */

	/* Move upper 23 bits of incoming coefficient into place */
	s->coeff1 = (d->coeff2 >> (AR_IEEE64_C2_BITS - COEFF_BIT_OFF)) |
				(d->coeff1 << COEFF_BIT_OFF);
	s->coeff0 = (d->coeff1 >> (AR_IEEE64_C2_BITS - COEFF_BIT_OFF)) |
				(d->coeff0 << COEFF_BIT_OFF);

	s->sign = d->sign;
        s->zero = 0;

	return ar_i32norm (expo, lbits, rbits, s, roundmode);

#	undef COEFF_BIT_OFF
}


/* IEEE 32 -> IEEE 64 */
int
ar_i32to64 (AR_IEEE_64 *d, const AR_IEEE_32 *s) {

	int expo;

#	if AR_IEEE32_C0_BITS < AR_IEEE64_C0_BITS
#		error ar_i32to64 has coefficient shifts miscoded.
#	else
#		define COEFF_BIT_OFF  (AR_IEEE32_C0_BITS - AR_IEEE64_C0_BITS)
#	endif

	if (s->expo > AR_IEEE32_MAX_EXPO) {
		if (IS_IEEE32_NZ_COEFF(s)) {
			/* WARNING: the following code makes some assumptions about
			 * bit field sizes!
			 *
			 * Incoming quantity is a NaN; return a NaN with the
			 * same high AR_IEEE32_COEFF_BITS bits.  The result
			 * must have at least one non-zero coefficient bit.
			 */
			ZEROIEEE64 (*d);
			d->sign   = s->sign;
			d->expo   = AR_IEEE64_MAX_EXPO + 1;
			d->coeff0 = s->coeff0 >> COEFF_BIT_OFF;
			d->coeff1 = (s->coeff0 << (AR_IEEE64_C1_BITS - COEFF_BIT_OFF)) |
						(s->coeff1 >> COEFF_BIT_OFF);
			d->coeff2 = s->coeff1 << (AR_IEEE64_C2_BITS - COEFF_BIT_OFF);
			d->coeff3 = 0;
			if (!IS_IEEE64_NZ_COEFF(d)) {
				d->coeff3 = 1;
			}
			return AR_STAT_UNDEFINED;
		} else {
			/* It's +/-Inf */
			ZEROIEEE64 (*d);
			d->expo = AR_IEEE64_MAX_EXPO + 1;
			if (d->sign = s->sign)
				return AR_STAT_OVERFLOW | AR_STAT_NEGATIVE;
			return AR_STAT_OVERFLOW;
		}
	}

	d->sign = s->sign;
	if (s->expo)
		expo = s->expo - AR_IEEE32_EXPO_BIAS + AR_IEEE64_EXPO_BIAS;
	else
		expo = 0;

	/* WARNING: the following code makes some assumptions about
	 * bit field sizes!
	 *
	 * Copy incoming coefficient into the upper 23 bits of the result.
	 */
	d->coeff0 = s->coeff0 >> COEFF_BIT_OFF;
	d->coeff1 = (s->coeff0 << (AR_IEEE64_C1_BITS - COEFF_BIT_OFF)) |
				(s->coeff1 >> COEFF_BIT_OFF);
	d->coeff2 = s->coeff1 << (AR_IEEE64_C2_BITS - COEFF_BIT_OFF);
	d->coeff3 = 0;

	return ar_i64norm (expo,
			   !!s->expo /* lbits */,
			   0 /* rbits */,
			   d,
			   AR_ROUND_NEAREST /* ignored */);

#	undef COEFF_BIT_OFF
}


/* IEEE 128 -> IEEE 64 */
int
ar_i128to64 (AR_IEEE_64 *d, const AR_IEEE_128 *q, const int roundmode) {

	int res = AR_STAT_OK;
	int expo;
	unsigned long lbits, rbits;

	/*
	 * Use native arithmetic for MIPS.
	 */
	if (HOST_IS_MIPS) {
		AR_TYPE ty = AR_Float_IEEE_NR_64;

		*(double *) d = *(long double *) q;
		return AR_status ((AR_DATA *) d, &ty);
	}

#	if AR_IEEE128_C0_BITS < AR_IEEE64_C0_BITS
#		error ar_i128to64 has coefficient shifts miscoded.
#	else
#		define COEFF_BIT_OFF  (AR_IEEE128_C0_BITS - AR_IEEE64_C0_BITS)
#	endif

	if (q->expo > AR_IEEE128_MAX_EXPO) {
		if (IS_IEEE128_NZ_COEFF(q)) {
			/* WARNING: the following code makes some assumptions about
			 * bit field sizes!
			 *
			 * Incoming quantity is a NaN; return a NaN with the
			 * same high AR_IEEE64_COEFF_BITS bits.  The result
			 * must have at least one non-zero coefficient bit.
			 */
			ZEROIEEE64 (*d);
			d->sign    = q->sign;
			d->expo    = AR_IEEE64_MAX_EXPO + 1;
			d->coeff3 = (q->coeff3 >> COEFF_BIT_OFF) |
						(q->coeff2 << AR_IEEE64_C0_BITS);
			d->coeff2 = (q->coeff2 >> COEFF_BIT_OFF) |
						(q->coeff1 << AR_IEEE64_C0_BITS);
			d->coeff1 = (q->coeff1 >> COEFF_BIT_OFF) |
						(q->coeff0 << AR_IEEE64_C0_BITS);
			d->coeff0 = (q->coeff0 >> COEFF_BIT_OFF);
			if (!IS_IEEE64_NZ_COEFF(d)) {
				d->coeff3 = 1;
			}
			return AR_STAT_UNDEFINED;
		} else {
			/* It's +/-Inf */
			ZEROIEEE64 (*d);
			d->expo = AR_IEEE64_MAX_EXPO + 1;
			if (d->sign = q->sign)
				res |= AR_STAT_NEGATIVE;
			return res | AR_STAT_OVERFLOW;
		}
	}

	if (q->sign)
		res |= AR_STAT_NEGATIVE;

	/* Incoming denorm must underflow to zero. */
	if (!q->expo) {
		d->sign = q->sign;
		d->expo = d->coeff0 = d->coeff1 = d->coeff2 = d->coeff3 = 0;
		res |= AR_STAT_ZERO;
		if (IS_IEEE128_NZ_COEFF(q))
		    if(ar_state_register.ar_denorms_trap)
				res |= AR_STAT_UNDERFLOW;
		    else
				res |= AR_STAT_UNDEFINED;
		return res;
	}

	lbits = 1;
	expo = q->expo - AR_IEEE128_EXPO_BIAS + AR_IEEE64_EXPO_BIAS;
	if (expo <= 0)
		expo--;

	/* WARNING: the following code makes some assumptions about
	 * bit field sizes!
	 *
	 * Compress rightmost bits of incoming coefficient into
	 * a 3-bit guard/round/sticky set of rounding bits.
	 */
	rbits = ((q->coeff3 >> 9) & 07) |			/* G and R */
		!!(q->coeff3 & MASKR (9) |
		   q->coeff4 | q->coeff5 | q->coeff6);		/* sticky */

	/* Move upper bits of incoming coefficient into place */
	d->coeff3 = (q->coeff3 >> COEFF_BIT_OFF) |
				(q->coeff2 << AR_IEEE64_C0_BITS);
	d->coeff2 = (q->coeff2 >> COEFF_BIT_OFF) |
				(q->coeff1 << AR_IEEE64_C0_BITS);
	d->coeff1 = (q->coeff1 >> COEFF_BIT_OFF) |
				(q->coeff0 << AR_IEEE64_C0_BITS);
	d->coeff0 = (q->coeff0 >> COEFF_BIT_OFF);

	d->sign = q->sign;

	return ar_i64norm (expo, lbits, rbits, d, roundmode);

#	undef COEFF_BIT_OFF
}


/* IEEE 64 -> IEEE 128 */
int
ar_i64to128 (AR_IEEE_128 *q, const AR_IEEE_64 *d) {

	int expo;

	/*
	 * Use native arithmetic for MIPS.
	 */
	if (HOST_IS_MIPS) {
		AR_TYPE ty = AR_Float_IEEE_NR_128;

		*(long double *) q = *(double *) d;
		return AR_status ((AR_DATA *) q, &ty);
	}

#	if AR_IEEE128_C0_BITS < AR_IEEE64_C0_BITS
#		error ar_i64to128 has coefficient shifts miscoded.
#	else
#		define COEFF_BIT_OFF  (AR_IEEE128_C0_BITS - AR_IEEE64_C0_BITS)
#	endif

	if (d->expo > AR_IEEE64_MAX_EXPO) {
		if (IS_IEEE64_NZ_COEFF(d)) {
			/* WARNING: the following code makes some assumptions about
			 * bit field sizes!
			 *
			 * Incoming quantity is a NaN; return a NaN with the
			 * same high AR_IEEE64_COEFF_BITS bits.  The result
			 * must have at least one non-zero coefficient bit.
			 */
			ZEROIEEE128 (*q);
			q->sign   = d->sign;
			q->expo   = AR_IEEE128_MAX_EXPO + 1;
			q->coeff0 = (d->coeff0 << COEFF_BIT_OFF) |
						(d->coeff1 >> AR_IEEE64_C0_BITS);
			q->coeff1 = (d->coeff1 << COEFF_BIT_OFF) |
						(d->coeff2 >> AR_IEEE64_C0_BITS);
			q->coeff2 = (d->coeff2 << COEFF_BIT_OFF) |
						(d->coeff3 >> AR_IEEE64_C0_BITS);
			q->coeff3 = (d->coeff3 << COEFF_BIT_OFF);
			q->coeff4 = 0;
			q->coeff5 = 0;
			q->coeff6 = 0;
			if (!IS_IEEE128_NZ_COEFF(q)) {
				q->coeff6 = 1;
			}
			return AR_STAT_UNDEFINED;
		} else {
			/* It's +/-Inf */
			ZEROIEEE128 (*q);
			q->expo = AR_IEEE128_MAX_EXPO + 1;
			if (q->sign = d->sign)
				return AR_STAT_OVERFLOW | AR_STAT_NEGATIVE;
			return AR_STAT_OVERFLOW;
		}
	}

	q->sign = d->sign;
	if (d->expo)
		expo = d->expo - AR_IEEE64_EXPO_BIAS + AR_IEEE128_EXPO_BIAS;
	else
		expo = 0;

	/* WARNING: the following code makes some assumptions about
	 * bit field sizes!
	 *
	 * Copy incoming coefficient into the upper bits of the result.
	 */
	q->coeff0 = (d->coeff0 << COEFF_BIT_OFF) |
				(d->coeff1 >> AR_IEEE64_C0_BITS);
	q->coeff1 = (d->coeff1 << COEFF_BIT_OFF) |
				(d->coeff2 >> AR_IEEE64_C0_BITS);
	q->coeff2 = (d->coeff2 << COEFF_BIT_OFF) |
				(d->coeff3 >> AR_IEEE64_C0_BITS);
	q->coeff3 = (d->coeff3 << COEFF_BIT_OFF);
	q->coeff4 = 0;
	q->coeff5 = 0;
	q->coeff6 = 0;

	return ar_i128norm (expo,
			   !!d->expo /* lbits */,
			   0 /* rbits */,
			   q,
			   AR_ROUND_NEAREST /* ignored */);

#	undef COEFF_BIT_OFF
}

#ifdef __mips

#endif /* __mips */

/* Cray single strange rounding for "rounded integer division", per
 * code sequences in the CMCS backend.
 */
int
ar_crnd64 (AR_CRAY_64 *rnd,
		   const AR_CRAY_64 *flt) {

	int res;
	AR_CRAY_64 a = *flt;

	/* Construct a floating value with only the low-order four bits
	 * left in the mantissa.
	 */
	a = *flt;
	a.coeff0 = 0;
	a.coeff1 = 0;
	a.coeff2 &= 017;

	res = ar_cfadd64 (rnd, flt, &a);

	/* Trim off the low-order four bits */
	rnd->coeff2 &= ~017;

	return res;
}

int
ar_crnd128 (AR_CRAY_128 *rnd,
	    const AR_CRAY_128 *flt) {

	int res;
	AR_CRAY_128 a = *flt;

	/* Construct a floating value with only the low-order four bits
	 * left in the mantissa.
	 */
	a = *flt;
	a.coeff0 = 0;
	a.coeff1 = 0;
	a.coeff2 = 0;
	a.coeff3 = 0;
	a.coeff4 = 0;
	a.coeff5 &= 017;

	res = ar_cfadd128 (rnd, flt, &a);

	/* Trim off the low-order four bits */
	rnd->coeff5 &= ~017;

	return res;
}


static char USMID [] = "\n%Z%%M%	%I%	%G% %U%\n";
static char rcsid [] = "$Id: convert.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $";
