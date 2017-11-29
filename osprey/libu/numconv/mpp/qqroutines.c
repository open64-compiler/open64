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


#pragma ident "@(#) libu/numconv/mpp/qqroutines.c	92.2	08/02/99 16:30:41"

#include <stdio.h>
#include <stdlib.h>
#ifndef _LD64
#if defined(_UNICOS)
#include <fp.h>
#else
#if defined(BUILD_OS_DARWIN)
#include <limits.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <values.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <math.h>
#endif

#include <cray/fmtconv.h>
#include "qq_routines.h"

#define DEBUGSW   	0 /* 1 is on, 0 is off */

#define BIT16           0x10000
#define BIT30		(  ((int32)1)<<30 )
#define BIT32      	(  ((int64)1)<<32 )
#define BIT49      	(  ((int64)1)<<49 )
#define BIT52      	(  ((int64)1)<<52 )
#define BIT60      	(  ((int64)1)<<60 )
#define IEEE_128_EXPO_INF	0x7fff
#define IEEE_128_EXPO_ONE	0x3fff
#define MASKLOW16	0xffff
#define MASKLOW49	(  BIT49-1  )
#define MASKLOW32	(  BIT32-1  )
#define MASKLOW52	(  BIT52-1  )
#define MASKLOW60	(  BIT60-1  )
#define U64_ONE		((uint64) 1)
#define D2POWER16	65536.0
#define D2POWER32	( D2POWER16*D2POWER16 )
#define D2POWER48	( D2POWER16*D2POWER32 )
#define D2POWER49	( D2POWER48*2.0 )
#define D2POWER52	( D2POWER48*16.0 )
#define DHALFPOWER48	( 1.0/D2POWER48 )
#define DHALFPOWER49	( 1.0/D2POWER49 )
#define DEXPONE 0x3ff
#define DEXPINF 0x7ff
#define ZERO '0'

#define IEEE_128_SCALB	scalbl
extern FLOAT128 _QHIPOWER_OF_FIVE_TABLE[];
extern FLOAT128 _QPOWER_OF_FIVE_TABLE[];
extern FLOAT64 _POWER_OF_TEN_TABLE[];

/***********************************************************
 * _qqscale20 scales a 256-bit float into the range [1,20)
 ***********************************************************/

void _qqscale20(
	FLOAT128 *qdatum,	/* item to scale */
	FLOAT128 *qresulthi,	/* scaled result high 128 bits */
	FLOAT128 *qresultlo,	/* scaled result low 128 bits */
	int32 *denorm_2_adjust,	/* power of 2 adjustment for denormals */
	int32 *scaleoverten,	/* result at least 10.0 */
	long *power_of_ten)	/* power of ten used */
{

#if defined(_WORD32) || defined(__mips) || defined(_LITTLE_ENDIAN)
#define alog10_2    0.30102999566398119521373889472449302676818988146210854131LL
#else
#define alog10_2    0.30102999566398119521373889472449302676818988146210854131L
#endif

#define alog10_2low 0.30102999566398

	register FLOAT128 qtmp;
	FLOAT128 thi,tlo, uhi,ulo, vhi, vlo;
	int iround128;
	int32 j, ixlo, ixhi;
	int32 jj;
	union { FLOAT128 q[2];
		uint64 i64[4];
		uint32 i32[8];
		ieee64_t d[4]; } u,t,v;

/*********************************************************************
 *  Need to find pow(x) such that x/10^pow(x) is in [1, 20].
 *  So,
 *        pow(x) = floor(log10(x)) .
 *  Now,
 *        logb(x) = floor(log2(x))
 *        logb(x) <= log2(x) = log10(x)/log10(2) < logb(x) +1
 *        logb(x)*log10(2) = log10(x) < logb(2x)* log10(2)
 *
 *  Taking the floor of all sides of the inequality we find:
 *
 *  floor(logb(x)*log10(2)) <= pow(x) <= floor(logb(2x)*log10(2))
 *
 *  One approximation formula is as follows 
 *
 *      power_of_ten = (long) (alog10_2*logb(datum)+(scalb(datum,
 *                              -(long)logb(datum))-1.0)*0.504);
 *      if (datum < 1.0 ) power_of_ten-- ;
 *
 *  2^n is not near enough to any power of ten to make multiplication
 *  rounding significant for the exponent.  We only need to be sure that
 *  we don't let rounding make the power of ten too large.  This is done
 *  by using a truncated IEEE-64 log10(2) = .301 .  If the mantissa is
 *  nearly 2, we are low by 0.301, and if the mantissa is 1.0 relative
 *  error .le. 10^-14, not enough to matter, since
 *  abs(unbiased exponent) < 2^15 .
 *
 *  So,  p =floor( log10(2)*logb(x) ) is either correct or low by 1. 
 *********************************************************************/

	if(DEBUGSW) {
		printf("qqscale20:denorm_2_adjust %d\n", *denorm_2_adjust);
		if( *qdatum <0 ) {
			printf("qqscale20:negative input\n");
			abort();
		}
	}
	u.q[0] = *qdatum;
	u.q[1] = 0;

	/* denormal must be prescaled; debug check for zero exponent */
	j = u.i64[0] >> (64 -1 - IEEE_128_EXPO_BITS);
	if(DEBUGSW) {
		if( j <= 0 )
			abort();
	}

	/* estimate log2(x) */
	j = j - IEEE_128_EXPO_ONE - *denorm_2_adjust;
        *power_of_ten = (int32) (alog10_2low * (FLOAT64)j) - (j<0);

	/* set t.q to 5.0^(-*power_of_ten) */
	jj = -*power_of_ten;
	jj = _qq_power5(&jj, &t.q[0], &t.q[1]);

	/* round to infinity at 256 */
	iround128= 2;
	_qmult3(&iround128, &t.q[0],&t.q[1], &u.q[0],&u.q[1],
		&v.q[0], &v.q[1]);

	/***************************************************************
	 *  On Solaris we do not create input denormals. On output,
	 *  the denormal has been multiplied by 2^denorm_2_adjust to
	 *  normalize it.
	 **************************************************************/

	DEBUGQ("into scalbl: %x %x %x %x\n", v.q[0]);
	v.q[0] = IEEE_128_SCALB(v.q[0], - *power_of_ten - *denorm_2_adjust );
	DEBUGQ("post scalbl: %x %x %x %x\n", v.q[0]);
	*scaleoverten = ( v.i32[0] >= IEEE_128_TEN_HI32 ); /* .ge. 10.0 */
	*power_of_ten += *scaleoverten;
	if (v.i64[0] < IEEE_128_ONE_HI64 ) {	/* .lt. 1.0 */
		if( DEBUGSW)
			printf( "\n_qqscale20 <1 internal ERROR\n");
		v.i64[0] = IEEE_128_ONE_HI64;
		v.i64[1] = 0;
		v.q[1] = 0.0;
	}
	if( DEBUGSW) {
		if( v.i32[0] >= 0x40034800 ) {  /* IEEE_128 20.5 */
			printf( "\n_qqscale20 >20.5 internal ERROR\n");
			abort();
		}
	}
	*qresulthi = v.q[0];
	*qresultlo = v.q[1];
} /* end qq_scale20 routine */


/***************************************************
 * _qq_power5 returns 5.0^power as a 256-bit variable,
 * the result of multiplying coarse and fine powers.
 * The result is never lower; error is [0,2] units in last place.
 *
 * If the exponent is too high, return -1 with +Infinty.
 * If the exponent is too low return -2 with 0.0.
 * If the exponent is in range return 0 with 5.0^power.
 ***************************************************/

int _qq_power5(int32 * power,
		FLOAT128 * powerhi,
		FLOAT128 * powerlo)
{
	int lo, hi, iround128;
	union{ FLOAT128 q[2];
		int64 i64[4]; } qval, sval;

	lo = *power & QSTEPMASK;

	/* Division must produce a positive remainder so negative values must
	 * be rounded up.
	 */
	hi = ( (*power + BIT30) >> QSTEPSHIFT ) - ( BIT30 >> QSTEPSHIFT);
	if (hi> QTABLELIM) {
		*powerlo = 0.0;
		/* load value for infinity */
		sval.i64[0] = IEEE_128_EXPONENT;
		sval.i64[1] = 0.0;
		*powerhi = sval.q[0];
		return(-1);
	}
	else if( hi < -QTABLELIM) {
		*powerlo = 0.0;
		*powerhi = 0.0;
		return(-2);
	}

	/* sval = 5.0^coarse.  Power Table is truncated, make
	 * result larger (unless it was exact).  No carry since 
	 * no table entry has an extension ending in 64 one bits.
	 */
	sval.q[0] = _QPOWER_OF_FIVE_TABLE[ 2*lo ];
	sval.q[1] = _QPOWER_OF_FIVE_TABLE[ 2*lo +1 ];
	if (lo > 103)
		sval.i64[3] += 1;

	/* qval = 5.0^fine. */
	if (hi == 0) { 
		*powerhi = sval.q[0];
		*powerlo = sval.q[1];
		return(0);
	}
	qval.q[0] = _QHIPOWER_OF_FIVE_TABLE[ 2*QTABLELIM +2*hi ];
	qval.q[1] = _QHIPOWER_OF_FIVE_TABLE[ 2*QTABLELIM +2*hi +1 ];

	/* make it larger (unless it was exact). no carry. */
	if (hi > (103>>QSTEPSHIFT) || hi < 0)
		qval.i64[3] += 1;

	/* powerhi = 5.0^coarse(sval) *5.0^fine.
	 * Don't multiply if 1.0 (5^0)
	 */
	if (lo == 0) {
		*powerhi = qval.q[0];
		*powerlo = qval.q[1];
	}
	else{

		/* round to signed infinity at 256 */
		iround128 = 2;
		_qmult3( &iround128, &qval.q[0], &qval.q[1], &sval.q[0], &sval.q[1],
			powerhi, powerlo) ;
	}
	return(0);
} /* end  _qq_power5 routine */


/******************************************************************
 * Add 256-bit unsigned integers in arrays of unsigned int32[8].
 * iu64r = iu64x + iu64y.  Result iu64r may be same as operands.
 * WARNING: routine depends on left truncation of 64-bit integers.
 *****************************************************************/
#define CARRYADD64(iu64x,iu64y,iu64r) { \
	register uint64 k; \
	register int carry,j; \
	carry = 0;  \
	for(j = 3; j >= 0; j--) { \
		k = iu64x[j] + carry; \
		carry = ( k < iu64x[j] ); \
		iu64r[j] = k + iu64y[j]; \
		carry = carry | ( iu64r[j] < k); \
	} \
} /* end CARRYADD64 macro */


#define NBITSX 64
#define NWORDSX (256/64)
/******************************************************
 *  add two unsigned 256-bit floats. result = s+l
 *******************************************************/

void _qq_addunsigned( FLOAT128 *shi, FLOAT128 *slo,
		FLOAT128 *lhi, FLOAT128 *llo,
		FLOAT128 *qresulthi, FLOAT128 *qresultlo)
{
	union{ FLOAT128 q[2];
		uint32 i32[8];
		struct {
			unsigned int upper32		: 32;
			unsigned int mantissa_up_2	: 32;
			unsigned int mantissa_low_1	: 32;
			unsigned int mantissa_low_2	: 32;
		} parts64;
		struct {
			unsigned int signexponent	: 1+IEEE_128_EXPO_BITS;
			unsigned int mantissa_up_1	: IEEE_128_MANT_TOP_BITS - 32;
			unsigned int mantissa_up_2	: 32;
			unsigned int mantissa_low_1	: 32;
			unsigned int mantissa_low_2	: 32;
			unsigned int mantissa_low_3	: 32;
			unsigned int mantissa_low_4	: 32;
			unsigned int mantissa_low_5	: 32;
			unsigned int mantissa_low_6	: 32;
			unsigned int mantissa_low_7	: 32;
		} parts;
		uint64 i64[4]; } lval, sval;

	long j, exps, expl, shiftrt;

	void _qq_iushiftrt64(uint64 *arg1, long *arg2, uint64 *arg3);
	sval.q[0] = *shi;
	sval.q[1] = *slo;
	lval.q[0] = *lhi;
	lval.q[1] = *llo;

	if (*shi > *lhi) {
		sval.q[0] = *lhi;
		sval.q[1] = *llo;
		lval.q[0] = *shi;
		lval.q[1] = *slo;
	}
	DEBUGQ("\n***qq_addunsigned: in L  %x %x %x %x\n", lval.q[0] )
	DEBUGQ("\n***qq_addunsigned: in S  %x %x %x %x\n", sval.q[0] )
	expl = lval.parts.signexponent;
	exps = sval.parts.signexponent;
	if (expl == 0 || exps == 0) { 
		if (DEBUGSW)
			printf("qq_addunsigned: denormal zeroed\n");
		*qresulthi = 0.0;
		*qresultlo = 0.0;
		return;
	}
	shiftrt = expl - exps;			/* >=0 */
	if( shiftrt > (113+128) ){
		*qresulthi = lval.q[0];
		*qresultlo = lval.q[1];
		return;
	}

	/* remove exponent and insert implicit bit */
	lval.parts64.upper32 = lval.parts.mantissa_up_1 + 0x10000;
	sval.parts64.upper32 = sval.parts.mantissa_up_1 + 0x10000;

	if (shiftrt > 0){
		_qq_iushiftrt64( &sval.i64[0], &shiftrt, &sval.i64[0] );
	}
	DEBUGQ("\n***qq_addunsigned: after shift: S  %x %x %x %x\n", sval.q[0])
	DEBUGQ(  "***qq_addunsigned: after shift: S  %x %x %x %x\n", sval.q[1])
	DEBUGQ(  "***qq_addunsigned: after shift: L  %x %x %x %x\n", lval.q[0])
	DEBUGQ(  "***qq_addunsigned: after shift: L  %x %x %x %x\n", lval.q[1])

	/* CARRYADD(lval.i32, sval.i32, lval.i32) */
	CARRYADD64(lval.i64, sval.i64, lval.i64)

	DEBUGQ("***qq_addunsigned: after add: L  %x %x %x %x\n", lval.q[0])
	DEBUGQ("***qq_addunsigned: after add: L  %x %x %x %x\n", lval.q[1])

	if (lval.parts.signexponent > 1){
		expl = expl + 1;
		shiftrt = 1;
		j = lval.i64[NWORDSX-1] & 1;
		_qq_iushiftrt64(&lval.i64[0], &shiftrt, &lval.i64[0]);
		lval.i64[NWORDSX-1] += j;
		DEBUGQ("\n***qq_addunsigned: normalize: L  %x %x %x %x\n", lval.q[0])
		DEBUGQ("\n***qq_addunsigned: normalize: L  %x %x %x %x\n", lval.q[1])
	}
	if (DEBUGSW) {
		if (expl >= 0x7fff)
			abort();
	}
	lval.parts.signexponent += expl -1;
	DEBUGQ("\n***qq_addunsigned: after exp added: L  %x %x %x %x\n", lval.q[0])
	*qresulthi = lval.q[0];
	*qresultlo = lval.q[1];
	return;
} /* end _qq_addunsigned macro */


/******************************************************
 * Convert a scaled 256-bit float in range [1,32) to 5 FLOAT64 integers
 * with implied decimal point 5 bits from top of first part:
 * <52:48>.<47:0> <48:0> <48:0> <48:0> <48:0> 
 ******************************************************************/

void _qq_putdigits_init(
			const FLOAT128 * qq_q,
			FLOAT64 * t ) 
{
	register short binexp, idigit, icarry, j;
	register FLOAT64 tmp;
	union { FLOAT128 q[2];
		int32 i32[8];
		struct {
			unsigned int signexponent	: 1+IEEE_128_EXPO_BITS;
			unsigned int mantissa_up_1	: IEEE_128_MANT_TOP_BITS- 32;
			unsigned int mantissa_up_2	: 32;
		} parts;
		uint64 i64[4]; } tmpu;

	tmpu.q[0] = qq_q[0];
	tmpu.q[1] = qq_q[1];
	binexp = tmpu.parts.signexponent - IEEE_128_EXPO_ONE;
	tmpu.i64[0] = (tmpu.i64[0]&IEEE_128_MANTISSA) +
			IEEE_128_IMPLICIT_BIT;
	DEBUGQ("\n***qq_putdigits:as fixed pt: %x %x %x %x\n", tmpu.q[0])
	DEBUGQ("\n***qq_putdigits:as fixed pt: %x %x %x %x\n", tmpu.q[1])

	/* note: binexp is 0,1,2,3, or 4 */
	t[0] = tmpu.i64[0]; 		 /* 49 bits -- 1+ 48 fraction */
	t[1] = tmpu.i64[1] >> (64-49);			/* 49 bits */
	t[2] = ( tmpu.i64[1] << (49-15)) & MASKLOW49;	/* 15 bits */
	t[2] += tmpu.i64[2] >> (64-34);			/* 34 bits */
	t[3] = ( tmpu.i64[2] << (49-30)) & MASKLOW49;	/* 30 bits */
	t[3] += tmpu.i64[3] >> (64-19);			/* 19 bits */
	t[4] = ( tmpu.i64[3] << (49-45)) & MASKLOW49;	/* 45 bits */

	if(DEBUGSW){
		for(j=0; j<5; j++){
			int32 i,ii;
			i = t[j]/scalb(1.0,24);
			ii = t[j]-(((ieee64_t)i)*scalb(1.0,24));
			printf(" t[]:%X %X ",i,ii);
		}
		printf("\n");
		if (binexp > 4 || binexp < 0)
			printf("qq_putdigits_init:scaled binexp not [0,4]:%x\n",
				binexp);
	}
	if (binexp > 0){
		tmp = 2.0;
		for( j=1; j<binexp; j++) tmp = tmp + tmp;
		icarry = 0;
		for( j=4; j>0; j--) {
			t[j] = t[j] * tmp + (FLOAT64) icarry;
			icarry = t[j] * DHALFPOWER49;
			t[j] -=  D2POWER49 * (FLOAT64) icarry;
		}
		t[0] = t[0] * tmp + (FLOAT64) icarry;
	}
} /* end _qq_putdigits_init routine */


/******************************************************
 * Loop, taking digits from the top of a scaled 
 * 256-bit float in range [1,16) held in five FLOAT64 integers.
 * with implied decimal point 4 bits from top of first part:
 * <51:48 . 47:0> <48:0> <48:0> <48:0> <48:0> 
 * Output zeroes after the first digits_left digits output.
 ******************************************************/

void _qq_putdigits(
	FLOAT64 * t,
	const long * limit,
	long * digits_left,
	long ** position_pointer)
{
	register short icarry, idigit, j;
	register uint64 times4, times5;

	idigit = (short) ( t[0] * DHALFPOWER48 );
	idigit = (short) (0.1000001 * (FLOAT64) idigit );
	if (idigit > 0 && *position_pointer < limit) {
		**position_pointer = ZERO + idigit;
		*position_pointer += 1;
		*digits_left = *digits_left - 1;
		t[0] -= (10.0 *D2POWER48) * (FLOAT64) idigit;
	}
	while( *position_pointer < limit ){
		idigit = (short) ( t[0] * DHALFPOWER48);
		t[0] -=  D2POWER48 * (FLOAT64) idigit;
		if(*digits_left > 0)
			**position_pointer = ZERO + idigit;
		else
			**position_pointer = ZERO;
		*position_pointer += 1;
		*digits_left = *digits_left - 1;
		icarry = 0;
		for( j=4; j>0; j--) {
			t[j] = t[j] *10.0 + (FLOAT64) icarry;
			icarry = (short) (t[j] * DHALFPOWER49);
			t[j] -= D2POWER49 * (FLOAT64) icarry;
		}
		t[0] = t[0] *10.0 + (FLOAT64) icarry;
	}
} /* end _qq_putdigits routine */


/**********************************************************
 * right shift fixed point 256-bit integer by shiftrt bits.
 * rounding up always.
 **********************************************************/
void _qq_iushiftrt(uint32 *iu32x,
		int * shiftrt,
		uint32 *iu32res)
{
	register long j, remain;
	uint32 mask, carry, t, stickybit;
	uint32 itemp[8];

	for(j=0; j<8; j++) {
		itemp[j] = iu32x[j];
		iu32res[j] = iu32x[j];
	}
	remain = *shiftrt;
	if (remain <= 0) return;

	stickybit = 0;
	while (remain >= 32){
		for(j=7; j>0; j--) {
			stickybit = stickybit | itemp[j];
			itemp[j] = itemp[j-1];
		}
		itemp[0] = 0;
		remain -= 32;
	}
	if (remain > 0){
		/* mask for low 'remain' bits */
		mask = ( ((int32)1) << remain ) -1;
		carry = 0;
		for(j=0; j<8; j++ ) {
			iu32res[j] = ( itemp[j] >> remain) | carry;
			carry = ( mask & itemp[j] )<<(32 - remain);
		}
		stickybit = stickybit | carry;
	} 
	else for(j=0; j<8; j++) iu32res[j] = itemp[j];

	if (stickybit != 0) {
		for( j=7; j>=0; j--) {
			iu32res[j] ++ ;
			if (iu32res[j] != 0) break;
		}
	}
}/* end _qq_iushiftrt routine */ 


void _qq_iushiftrt64(uint64 *iu64x,
		long * shiftrt,
		uint64 *iu64res)
{
	register long j, remain;
	uint64 mask, carry, t, stickybit;
	uint64 itemp[NWORDSX];

	for(j=0; j<NWORDSX; j++) {
		itemp[j] = iu64x[j];
		iu64res[j] = iu64x[j];
	}
	remain = *shiftrt;
	if (remain <= 0) return;

	stickybit = 0;
	while (remain >= NBITSX){
		for(j=NWORDSX-1; j>0; j--) {
			stickybit = stickybit | itemp[j];
			itemp[j] = itemp[j-1];
		}
		itemp[0] = 0;
		remain -= NBITSX;
	}
	if (remain > 0){
		/* mask for low 'remain' bits */
		mask = ( ((int64)1) << remain ) -1;
		carry = 0;
		for(j=0; j<NWORDSX; j++ ) {
			iu64res[j] = ( itemp[j] >> remain) | carry;
			carry = ( mask & itemp[j] )<<(NBITSX - remain);
		}
		stickybit = stickybit | carry;
	}
	else for(j=0; j<NWORDSX; j++) iu64res[j] = itemp[j];

	if (stickybit != 0) {
		for( j=NWORDSX-1; j>=0; j--) {
			iu64res[j] ++ ;
			if (iu64res[j] != 0) break;
		}
	}
} /* end _qq_iushiftrt64 routine */


/**********************************************************
 * _qmult3 routine for Nearly exact multiplication
 **********************************************************/

void _qmult3(
	const int *iround128,	/* bit 2^0:round at 128, bit 2^1:at 256 */
	const FLOAT128 *uhi,
	const FLOAT128 *ulo,
	const FLOAT128 *thi,
	const FLOAT128 *tlo,
	FLOAT128 *resulthi,
	FLOAT128 *resultlo)
{
	union{	FLOAT128 q[2];
		uint64 i64[4];
		uint32 i32[8];
		struct {
			unsigned int signexponent	: 1+IEEE_128_EXPO_BITS;
			unsigned int mantissa_up_1	: IEEE_128_MANT_TOP_BITS - 32;
			unsigned int mantissa_up_2	: 32;
			unsigned int mantissa_low_1	: 32;
			unsigned int mantissa_low_2	: 32;
		} parts;
	} tmpt, tmpx, tmpu;
	FLOAT64 temp64[32], result64[32];
	FLOAT64 t[8],u[16];
	int32 expth, sexpth, exptl, sexptl;
	int32 expuh, sexpuh, expul, sexpul;
	int32 j, jj, jjj, iroundinf, iround, normalized;
	int32 bitsroundedoff;
	int32 roundat;
	int32 stickybit;
	int32 iroundeven;
	register FLOAT64 dtmp, carry;

	DEBUGQ("QMULT:uhi:%08x%08x%08x%08x \n",(*uhi))
	DEBUGQ("QMULT:ulo:%08x%08x%08x%08x \n",(*ulo))
	DEBUGQ("QMULT:thi:%08x%08x%08x%08x \n",(*thi))
	DEBUGQ("QMULT:tlo:%08x%08x%08x%08x \n",(*tlo))

	/*  The bits of each quadword pair form an unbroken bit string
	 *  beginning with bits <47:0> of the upper word.
	 */
	tmpt.q[0] = *thi;
	tmpt.q[1] = *tlo;
	sexpth = tmpt.i64[0] >>IEEE_128_MANT_TOP_BITS;

	/*	sexpth = (tmpt.i32[0] >> 16) & 0xffff;
	 *	sexpuh = (tmpu.i32[0] >> 16) & 0xffff;
	 */
	expth = sexpth & 0x7fff;

	tmpu.q[0] = *uhi;
	tmpu.q[1] = *ulo;
	sexpuh = tmpu.i64[0] >>IEEE_128_MANT_TOP_BITS;
	expuh = sexpuh & 0x7fff;

/* (8 32-bit parts) *  (16 17-bit parts) into 16 53 bit (32+17+4) parts:
 *  53=32+17+ 4 (log2 of maximum of 15 additions for each term)
 *
 *  (t0*2^-16+...t8*2^-224) * (2^0+u1*2^-16+u2*2^-32+ ...+u15*2^-240)
 *   = t0 + t2 + ... + t8
 *        + u1 + u2 + ... + u15
 *        + (t1 + t2 + ... + t15)*(u1+u2+...+u15)
 *
 * (1+t1+...t15) * (1+u1+u2+...+u15) * (1+v1+...v15)
 *  = 1 + t1 + t2 + ... + t15
 *      + u1 + u2 + ... + u15
 *      + (t1 + t2 + ... + t15)*(u1+u2+...+u15)
 */

	for(j=0; j<4 ;j++ ) {
		t[2*j+0] = (FLOAT64) (tmpt.i64[j] >> 32);
		t[2*j+1] = (FLOAT64) (MASKLOW32 & tmpt.i64[j]);
		u[4*j+0] = (FLOAT64)(int32)(MASKLOW16&(tmpu.i64[j] >>48));
		u[4*j+1] = (FLOAT64)(int32)(MASKLOW16&(tmpu.i64[j] >>32));
		u[4*j+2] = (FLOAT64)(int32)(MASKLOW16&(tmpu.i64[j] >>16));
		u[4*j+3] = (FLOAT64)(int32)(MASKLOW16& tmpu.i64[j]      );
	}
	t[0] = (FLOAT64)(int)( MASKLOW16 &( tmpt.i64[0] >>32) | BIT16);
	u[0] = 1.0;

	if(DEBUGSW) for(j=0; j<8 ; j++ ) if( t[j] < 0.0 ) abort();
	if(DEBUGSW) for(j=0; j<16; j++ ) if( u[j] < 0.0 ) abort();

	QADEBUG("t",8, "%x," , t)
	QADEBUG("u",16, "%x," , u)

	for(j=0; j<32; j++ ) { temp64[j] = 0; }

	for( j=0; j<8; j++ ) { 
		for( jj=0; jj<16; jj++) {
			temp64[2*j+1+jj] += t[j] * u[jj];
		}	
	}
	if(DEBUGSW){
		printf("carry in,top 8:");
		for( j=0; j<8; j++) printf("%f,", temp64[j]);
		printf("\n");
	}
	/*
	 * All temp64[j] are less than: (8 adds)*2^16*2^32< 2^51.
	 * Propagate carry bits in two steps: <50:32> and <31:16>.
	 * Using two multiply steps allows hardware truncation via int32.
	 */
	for( j=31; j>0; j--) {
		carry = (ieee64_t)(int32)( (1.0/D2POWER32)*temp64[j] );
		temp64[j] -= D2POWER32*carry;
		temp64[j-1] += D2POWER16*carry;
		carry = (ieee64_t)(int32)( temp64[j] *(1.0/D2POWER16) );
		temp64[j] -= D2POWER16*carry;
		temp64[j-1] += carry ;
	}
	QADEBUG("temp64 after first carry",32, "%x,", temp64)

	stickybit = 0;
	normalized = 0;		/* 0 if no normalization done */

	/* if normalized, temp64[0] in [1,2) .
	 * If in [2,4) shift right one
	 */
	if (temp64[0] >= 2.0) {
		register int32 icarry;
		if (DEBUGSW) {
			if (temp64[0] >= 4.0)
				printf("qmult3:multiply>=2 ERROR\n");
		}
		icarry = 0;
		for( j=0; j<16; j++) {
			if( icarry != 0) temp64[j] += D2POWER16;
			icarry =  temp64[j];
			temp64[j] = ( icarry >>1 );
			icarry = icarry & 1;
		}
		QADEBUG("normalized",32, "%x," , temp64)
		stickybit = icarry;	/* !=0 if one bits shifted off */
		normalized = 1;		/* 1 if normalization done */
	}

	/*  compute sticky bit and round bit for 256-bit rounding
	 *  if <3:2> =0+0 round to signed infinity.
	 *  if <3:2> =0+4 round to nearest, biased (Add last bit
	 *    shifted off)
	 *  if <3:2> =8+0 undefined.
	 *  if <3:2> =8+4 round to nearest, (nearest even if tie).
	 *    Not tested.
	 */
	if (((*iround128)&2) != 0) roundat=16;
	if (((*iround128)&1) != 0) roundat=8;
	if (((*iround128)&3) != 0)
		for( j=roundat+1; j<32; j++)
			stickybit = stickybit | (temp64[j] != 0.0);

	/*  Sticky bit is the OR of all bits shifted off.  Round to
	 *  infinity rounds if the sticky bit !=0 or the round bit !=0.
	 *  Round to nearest (biased) always rounds by roundbit.
	 *  Round to nearest (even) rounds by roundbit unless sticky
	 *	bit == 0 and the current value is even.
	 *  lowest result bit is even.
	 */
	j = (8+4) & *iround128;			/* rounding mode */
	bitsroundedoff = temp64[roundat];

	/* sticky bit */
	stickybit = stickybit | (bitsroundedoff & 0x7fff);

	/* round to nearest (biased) */
	iround = bitsroundedoff >> 15;

	/* round to nearest (even) */
	if (j  == (8+4) && iround != 0 && stickybit == 0
		&& (1& ~(int) temp64[roundat-1] ) ) iround = 0;

	/* ==0 round to inf */
	if (j  == 0)
		iround = iround | stickybit;

#if defined(__mips)
/*  Note: when rounding to MIPS128, only biased rounding and RNM
 *  are implemented (*iround128 == 4+1 or 8+4+1).  Now produce a
 *  value which will round correctly in later RNM conversion.
 *  Rounding at 256 bits is the same as on other architectures.
 *
 *  RNM: or all dropped bits with the final bit. (RNM not tested)
 *  Biased rounding: set the final bit to 1.
 */
	if (( 7 & *iround128 ) == ( 4+1 )) {	/* 4+1 or 8+4+1 */
		bitsroundedoff =	(int) temp64[7];
		if ((8 & *iround128 ) != 0 )
			jj =	( temp64[8] !=0 ) | stickybit;
		else    jj =	1;
		temp64[7] =	(ieee64_t)( bitsroundedoff | jj );
		iround =	0;
	}
#endif /* __mips */

	if (iround !=0 ) {
		for( j=roundat-1; j>=0; j--) {
			temp64[j] += 1.0 ;
			if (temp64[j] < D2POWER16) break; /* exit loop */
			temp64[j] = 0.0;
		}
	}
	QADEBUG("rounded   ",32, "%x," , temp64)

	/* If rounding propagates to the implicit bit, all the
	 * other bits are zero.
	 */
	if (temp64[0] == 2.0) {
		temp64[0] = 1.0;
		normalized = 1;		/* 1 if normalization done */
	}

	/* Rounding is complete.  If rounding at 128 was requested
	 * the nonzero bits beyond 128 are returned anyway. Ignore them.
	 */
	for( j=0; j<4; j++) {		/* store into 16-bit array */
		temp64[4*j] = temp64[4*j] *D2POWER16 + temp64[4*j+1];
		temp64[4*j] = temp64[4*j] *D2POWER16 + temp64[4*j+2];
		tmpt.i64[j] = (( (uint64) temp64[4*j] )<<16)
					+ (int32) temp64[4*j+3];
	}

	/* if (exponent > 1) || exponent < 0) )  */
	j = tmpt.parts.signexponent;

	if(DEBUGSW) {
		if (j > 1 || j < 0)
			printf("exponent < 0:%x \n",j );
	}

	/*
	If 1=normalized, we right shifted the product one bit.
	Resulthi should not be infinite at this point, but...
	*/
	j = expth + expuh -IEEE_128_EXPO_ONE;
	j += normalized;
	if (( tmpt.i64[0] >> IEEE_128_MANT_TOP_BITS) > 1) j += 1;

	/* check for underflow */
	if(DEBUGSW)
		printf( "sexpth:%x seupth:%x j:%x\n",sexpth,sexpuh,j );
	if ( j < 1 ) {
		tmpt.q[0] = 0.0;
		tmpt.q[1] = 0.0;
	}

	else {
		if ( j < IEEE_128_EXPO_INF )
			tmpt.parts.signexponent = j;
		else { /* construct an infinity */
			tmpt.q[1] = 0.0;
			tmpt.i64[0] = IEEE_128_EXPONENT;
		}
	}

	*resulthi = tmpt.q[0];
	*resultlo = tmpt.q[1];

	/* resulthi&lo are the result of multiplication on integers.  The 
	 * two operands were converted to 241-bit integers (2*128-16+1)
	 */
	DEBUGQ("QMULT:resulthi:%08x%08x%08x%08x \n",(*resulthi))
	DEBUGQ("QMULT:resultlo:%08x%08x%08x%08x \n",(*resultlo))
} /* end _qmult3 routine */


/***** float an unsigned 250-bit integer into qq ******/
void _qqfloat_uint250( FLOAT128 * x, int * inexact,
	const int64 * low,  const int64 * mid1,
	const int64 * mid2, const int64 * mid3,
	const int64 * high)
{
#define BITS_INT	50
#define TOPSHIFT	( BITS_INT-1- QMANTISSA_TOPBITS )
#define QTOPBIT		( ( (int64)1 )<<TOPSHIFT)
#define TOOBIG		( QTOPBIT*2 )
#define NPARTS		5

union { FLOAT128 q[2] ; uint64 i64[4] ; int32 i32[8] ; } t2 ; 
union { uint64 i64 ; ieee64_t d; } tmp ; 
	register int	j, jj, leftshift, bits;
	register int32	exptemp;
	register uint64	mask;
	register uint64	low_number, high_number;
	register uint64	mid1_number, mid2_number, mid3_number;

	high_number =	*high;
	mid3_number =	*mid3;
	mid2_number =	*mid2;
	mid1_number =	*mid1;
	low_number =	*low;

	/* Float as qq format: < sign(1),exp(15),mantissa(240) >
	 * 1) truncate (no round) to 241 bits.
	 * 2) reformat as	( < 15*0, high<49:1> >,
	 *			  < high<0>,mid3<49:0>,mid2<49:37> >,
	 *			  < mid2<36:0>,mid1<49:23> >,
	 *			  < mid1<22:0>,low<49:9> > )
	 * 3) left or right shift leading bit to position <48>.
	 * 4) insert exponent: < 0, exp(15), mantissa(241) >
	 *
	 * The algorithm depends on 64-bit words: truncation at left.
	 */

	/* Truncate (not round) to 241 bits; *inexact = truncated bits.
	 * If any of the top 9 bits are set truncation may be needed.
	 * Create a 9 bit mask from high<49:41> by setting all the
	 * bits to the right of the leading bit and mask with low<8:0>.
	 * If high<49:41> is zero, no truncation is performed.
	 */
	jj = high_number>>(BITS_INT-9);
	jj = jj | (jj>>1) | (jj>>2);
	jj = jj | (jj>>3) | (jj>>6);
	*inexact = low_number & jj;
	low_number = low_number - *inexact ;

	/* Left shift by whole words till high_number nonzero.
	 * Up to NPARTS-1 passes:
	 * 		low->mid1, low->mid2, low->mid3, low->high
	 * Decrement exponent each pass.
	 */
	exptemp = NPARTS*BITS_INT -1;
	for( j=0; j<(NPARTS-1); j++ ) {
		if( high_number !=0)
			break;
		exptemp =	exptemp -BITS_INT;
		high_number =	mid3_number;
		mid3_number =	mid2_number;
		mid2_number =	mid1_number;
		mid1_number =	low_number;
		low_number =	0;
	}

	/* exit if zero */
	x[0] = 0.0;
	x[1] = 0.0;
	if( high_number == 0)
		return;
 
	/* reformat as ( 49, 1+50+13, 37+27, 23+41 ) */
	t2.i64[0] = high_number >> 1;
	t2.i64[1] = (high_number << BITS_INT) + mid3_number;
	t2.i64[1] = (  t2.i64[1] << 13) + (mid2_number >> (BITS_INT-13));
	t2.i64[2] = (mid2_number << 27) + (mid1_number >> (BITS_INT-27));
	t2.i64[3] = (mid1_number << 41) + ( low_number >> (BITS_INT-41));

	tmp.d = (ieee64_t)high_number;
	bits = ( tmp.i64 >>52) -DEXPONE;
	leftshift = 49-bits;
	exptemp = exptemp -leftshift;
	if (DEBUGSW)
		printf("leftshift,exptemp,bits: final=%X %X %X \n",
			leftshift, exptemp, bits);

	if (leftshift < 0) {			/* right shift */
		for(jj=3; jj>=0; jj--){
			t2.i64[jj] = (t2.i64[jj] >> (-leftshift));
			if (jj > 0)
				t2.i64[jj] += (t2.i64[jj-1] <<
						(64+leftshift));
		}
	}
	if (leftshift > 0 ) {			/* left shift */
		mask = ( ( (int64)1 ) << leftshift ) - 1;
		for(jj=0; jj<4; jj++){
			t2.i64[jj] = ( t2.i64[jj] << leftshift);
			if( jj < 3 )
				t2.i64[jj] += (t2.i64[jj+1] >>
					(64-leftshift) );
		}
	}
	if(DEBUGSW)
		printf("justified: final=%llX %llX %llX %llX \n",
			t2.i64[0], t2.i64[1], t2.i64[2], t2.i64[3]  );

	/* add exponent to the left bit of the normalized mantissa */
	t2.i64[0] += (( exptemp - 1 + (uint64) IEEE_128_EXPO_ONE ) << 48 );

	if(DEBUGSW)
		printf("create: final=%X %X %X %X %X %X %X %X\n",
			t2.i32[0], t2.i32[1], t2.i32[2], t2.i32[3],
			t2.i32[4], t2.i32[5], t2.i32[6], t2.i32[7] );

	x[0] = t2.q[0];
	x[1] = t2.q[1];

	DEBUGQ("_qqfloat250:return:%x %x %x %x \n",x[0])
	DEBUGQ("_qqfloat250:return:%x %x %x %x \n",x[1])

} /* end _qqfloat_uint250 routine */


#if defined(__mips)

/* convert a double-double 128-bit to an IEEE 128-bit float.
 *
 * Add(truncating) (mips128) x[0:1] giving (ieee128) result y[0:1]
 *
 * Call x[0] xhi and x[1] xlo: then xhi + xlo is precise to 107 bits.
 * Normal case: xhi.exponent .ge. xlo.exponent+53,
 *   unless xhi.exponent < 53.
 */

void _m128toi128(FLOAT128 *y, const FLOAT128 *x)
{
	int64	iehi, ielo;
	uint64	ixhi, ixlo, ishi, islo, imhi, imlo;
	uint64	jhi, jlo, itmp;
	int	j, ishift;
	uint64	NOTSB64 =	0x7fffffffffffffff;
	uint64	MASK64MANT =	0x000fffffffffffff;
	uint64	IEEEINF128_UP =	0x7fff000000000000;

	union {
		uint64		i;
		double		d;
	} tmp;

	union {
		uint64		i[2];
		double		d[2];
		FLOAT128	q;
	} qtmp;

	qtmp.q =	*x;
	ixhi =	qtmp.i[0];
	ixlo =	qtmp.i[1];

	jhi =	ixhi & NOTSB64;
	jlo =	ixlo & NOTSB64;

	/* swap ixhi and ixlo if the magnitude of ixlo .ge. ixhi */
	if ( jhi < jlo ) {
		itmp =	ixlo;
		ixlo =	ixhi;
		ixhi =	itmp;
	}
	jhi =	ixhi & NOTSB64;
	jlo =	ixlo & NOTSB64;
	ishi =	ixhi >> 63;
	islo =	ixlo >> 63;
	iehi =	jhi >> 52;
	ielo =	jlo >> 52;
	imhi =	jhi & MASK64MANT;
	imlo =	jlo & MASK64MANT;

/* Return NaN in ixhi if either or both is NaN.  ixlo is ignored.
 * else return Inf if both are Inf.  ixlo is ignored.
 * If ixhi = -ixlo, it violates the principle that Inf-Inf=NaN, but
 * (Inf,-Inf) and (-Inf,Inf) are not valid in MIPS128.
 */
	if ( iehi == 0x7ff ) {
		qtmp.i[0] =	ixhi | (ixhi >> 4);
		qtmp.i[1] =	ixhi << 60;
		*y =	qtmp.q;
		return;
	}

/* Return ieee128(xhi) when xlo==0.0 .
 * When signs differ, this ignores the principle of ieee 0 addition,
 * but otherwise -0.0 only would result in round down mode.
 */

	if ( jlo == 0 ) {

		/* Return xhi when xlo==0.0 and xhi==0.0 .  */
		if ( jhi == 0 ) {
			qtmp.i[0] =	ixhi;
			qtmp.i[1] =	0;
			*y =	qtmp.q;
			return;
		}
		else if ( iehi > 0 ) {
			itmp =	iehi + IEEE_128_EXPO_ONE - DEXPONE;
			qtmp.i[0] =
				(imhi >> 4) + (itmp << 48) + (ishi << 63);
			qtmp.i[1] = imhi << 60;
			*y =	qtmp.q;
			return;
		}

/* IEEE64 denormals are 0.f* 2**-1022 where f is a 52 bit integer.
 * This is f* 2**-52 * 2**-1022, so the IEEE128 exponent is
 * expo(IEEE64(f))-52-1022 + 3fff-3ff
 */

		else {
			tmp.d =	(double) jhi;
			itmp =	IEEE_128_EXPO_ONE - DEXPONE - 52 - 1022;
			itmp =	(tmp.i >> 4) + (itmp << 48);
			qtmp.i[0] =	itmp | (ishi << 63);
			qtmp.i[1] =	tmp.i << 60;
			*y =	qtmp.q;
			return;
		}
	}

	/* Return +0 when xhi == -xlo != 0.  */
	else if ( jhi == 0 || (jhi == jlo) & (ishi != islo) ) {

		if ( jhi == 0 )
			ishi = 0;

		qtmp.i[0] =	ishi << 63;
		qtmp.i[1] =	0;
		*y =	qtmp.q;
		return;
	}

/* At this point, xhi and xlo are finite and nonzero with
 * |xhi| > |xlo| >0.  The result will therefore be finite nonzero
 * and within the normalized IEEE128 range.
 *
 * The MIPS128 rule is that if xhi is a valid denormal, xlo is zero.
 * xlo==0 has already been processed.
 *
 * xlo is denormal or zero if exp(xhi)==0, so the mantissas can be
 * subtracted/added without precision loss. This fact is not used.
 */

/* Insert the implicit bit where the exponent is not 0 (and not 7ff.
 * Shift for denormal xlo (0.f*2**-1022) is same as for
 * exponent == 001 (1.f*2**-1022)
 */

	if ( iehi > 0 )
		imhi = imhi | (U64_ONE << 52);

	if ( ielo > 0 ) {
		imlo = imlo | (U64_ONE << 52);
		ishift =	iehi - ielo;
	}
	else
		ishift =	iehi - 1;

	jhi =	imlo;
	jlo =	0;

	if ( ishift >= 113 ) {
		jhi =	0;
		jlo =	0;
		ishift =	0;
	}

	if ( ishift >= 53 ) {
		jlo =	jhi << 11;
		jhi =	0;
		ishift =	ishift - 53;
	}

	/* At this point, 0 .le.  ishift  .le. 59=112-53 */
	if ( ishift > 0 ) {
		jlo =	(jhi << (64-ishift)) | (jlo >> ishift);
		jhi =	jhi >> ishift;
	}

/* If xhi and xlo follow MIPS128 rules, jhi will change by -1, or 0
 * when signs differ, and +1 or 0 when signs are the same.
 */

	if ( ishi != islo ) {

		jhi =	imhi- jhi - (jlo != 0);
		jlo =	-jlo;
		j =	4;

		if ( (jhi >> 52) == 0 )
			j = 3;

		if ( (jhi >> 51) == 0 )
			j = 2;

		if ( (jhi >> 50) == 0 )
			j = 1;

		if ( (jhi >> 49) == 0 )
			j = 0;

		iehi =	iehi + j - 4;

		if ( j > 0 ) {
			jlo =	(jlo >> j) | (jhi << (64-j) );
			jhi =	jhi >> j;
		}

		while ( (jhi >> 48) == 0 ) {
			jhi =	(jhi << 1) | (jlo >> (64-1) );
			jlo =	jlo << 1;
			iehi =	iehi -1;
		}
	}
	else {

		/* signs are the same */

		jhi =	imhi+ jhi;
		j =	4;

		if ( jhi >= (U64_ONE << 53) )
			j = 5;

		iehi =	iehi + j - 4;
		jlo =	(jlo >> j) | (jhi << (64-j) );
		jhi =	jhi >> j;
	}

	itmp =	iehi - (jhi >> 48) + IEEE_128_EXPO_ONE - DEXPONE;
	qtmp.i[0] =	jhi + (itmp << 48) | (ishi << 63);
	qtmp.i[1] =	jlo;
	*y =	qtmp.q;
} /* end _m128toi128 routine */
 
/*  _i128tom128: convert IEEE 128 x to MIPS 128 y rounding to nearest.
 *  The result may have a lower denormal part, but not upper.
 *  Inf and NaN will have a zero lower part.
 */
 
int _i128tom128( FLOAT128 *y, const FLOAT128 *x)
{
	union{
		uint64		i64[2];
		ieee64_t	d[2];
		FLOAT128	q;
		struct {
			unsigned int sign_hi		: 01;
			unsigned int exp_hi		: 15;
			unsigned int mantissa_hihi	: 16;
			unsigned int mantissa_hilow	: 32;
			unsigned int mantissa_lowhi	: 32;
			unsigned int mantissa_lowlow	: 32;
		} ieee128;
		struct {
			unsigned int sign_hi		: 01;
			unsigned int exp_hi		: 11;
			unsigned int mantissa_hihi	: 20;
			unsigned int mantissa_hilow	: 32;
			unsigned int sign_low		: 01;
			unsigned int exp_low		: 11;
			unsigned int mantissa_lowhi	: 20;
			unsigned int mantissa_lowlow	: 32;
		} mips128;
	} qtmp;
	register int64	iexp, jsigned;
	register int	iroundup, loss;
	register uint64	isign, mlo, mhi, sticky, guard, itmp, round;
 
	qtmp.q =	*x;
	isign =	((int64) qtmp.ieee128.sign_hi ) << 63;
	iexp =	qtmp.ieee128.exp_hi;
	mhi =	qtmp.i64[0] & IEEE_128_MANTISSA;
	mlo =	qtmp.i64[1];
 
	/* will 107 bit (53 if tiny) round increment result IEEE64 exponent? */

	if( iexp == (IEEE_128_EXPO_ONE-DEXPONE) )
		iroundup =
			(mhi == 0x0000ffffffffffff) & (mlo >= 0xf800000000000000);
	else
		iroundup =
			(mhi == 0x0000ffffffffffff) & (mlo >= 0xf7ffffffffffffe0);
 
	/* Test for Zero and underflows with no rounding potential.
	 *
	 * Overflow if (107  bit rounded x) >= 2**1024 -2**(1024-54)
	 * or x >= mips(0x7fef ffff ffff ffff + 0x7c90 0000 0000 0000)
	 * or x >= IEEE(0x43fe ffff ffff ffff f7ff ffff ffff ffe0)
	 * The mips128 upper part will be finite.
	 *
	 * Underflow if (53 bit rounded x) < 2**-1022 *(1 -2**-53)
	 * or x < 2**-1022 -2**(1022-53)
	 * or x < IEEE(0x3c00 ffff ffff ffff f7ff ffff ffff ffff)
	 * The mips128 upper part will be normal.
	 * The mips128 result will have at least 53 bits of precision
	 * and at most 107 bits.
	 *
	 * If we allowed the upper to be denormal the result would have 
	 * less than 53 bits of precision, up to 100% relative error.
	 *
	 * The low word of a MIPS128 zero will have the sign of the upper.
	 */

	if ( ( iexp + iroundup ) <= ( 0 + IEEE_128_EXPO_ONE - DEXPONE ) ) {
		qtmp.i64[0] =	isign;
		qtmp.i64[1] =	isign;
		*y =	qtmp.q;

		if (( mhi | mlo | iexp ) == 0)
			return EX_REAL128;	/* True zero */
		else
			return EX_EXPUFLO;	/* Underflow */
	}
 
/* Test for Inf, NaN, Overflow.
 * Mantissa will be <= 0000f..f f7f..fc0 after rounding.
 * If mantissa >= 0000f..f f7f..fe0, it rounds into exponent: 00010..0 0 .
 * Max IEEE128 exponent is 3fff-3ff+7fe = 43fe .
 * Do not clear the sign of a NaN.
 * The low word of a MIPS128 Inf or NaN result is zero.
 */
	if ( ( iexp + iroundup ) >= (DEXPINF + IEEE_128_EXPO_ONE - DEXPONE) ) {
		qtmp.i64[0] =	isign | (((int64) DEXPINF ) << 52);
		qtmp.i64[1] =	0;
		if ( iexp == IEEE_128_EXPO_INF && ( mhi | mlo ) != 0 ) {
			mhi =	( mhi << 4 ) | ( mlo >> 60 );	/* NaN */
			if ( mhi == 0 ) {
				if ( mlo < ( U64_ONE << 51) )
					mhi =	mlo;
				else
					mhi =	mlo >> 9;
			}
			qtmp.i64[0] |=	mhi;
		}
		*y =	qtmp.q;		/* Inf, NaN, Overflow */
		return EX_EXPOFLO;
	}
/*
 * At this point, we know the result is finite and nonzero.
 * Now round the 113 bit IEEE mantissa to 107 bits by IEEE rules.
 * 
 * Denormals need special rounding, since they round
 * after the 2**(-1074) bit.  Thus if x < 2**(106-1074)
 * we must avoid problems that arise from rounding twice (two rounds
 * in the same direction): 
 *
 * If the final bit (R) is in mlo, round mlo correctly;
 * Else compress the lower 61 sticky bits and use a hardware round.
 *
 * The compress step is needed only if upper word denormals can result.
 * 
 * Zero, NaN and Inf should not reach this routine
 * from _defgu2s, but allow them just in case.
 *
 * normal:         loss==0      mlo = <57*M> <R.G> <5*S>
 * lower denormal: loss=[1,53]  mlo = <(57-loss)*M> <R.G> <(5+loss)*S>
 * R is the final bit before the round (determines even/odd).
 * G is the guard bit.  If G is 0, there is no round.
 * S is a sticky bit. 
 * Round if G nonzero and (R is nonzero or any S is nonzero)
 */
	loss =	106 - 1022 - 52 + IEEE_128_EXPO_ONE - iexp;

	if( loss < 0 ) 
		loss =	0;	/* no loss of precision: 107 bits */

	if( loss <= 57 ) {
		/* R is in mlo; loss>0 indicates ylo will be denormal or 0 */
		itmp =	( (uint64) 32 ) << loss;	/* 1 in G position */
		sticky =	mlo & ( itmp - 1 );
		mlo =	mlo - sticky;
		sticky =	sticky + ( itmp - 1);	/* S!=0 to G posn. */
		round =	mlo >> 1;         		/* R to G position */
		guard =	mlo & itmp;     	    	/* G */
		round =	( round | sticky ) & guard;
		mlo =	mlo + round;	/* becomes zero if carry */
		if( mlo < round )	/* unsigned compare detects carry */
			mhi++;		/* may carry into exponent */
	}
	else {
		/* R is in mhi, so OR the low 60 sticky bits with mlo<60>.
		 * lower part will be zero.  upper will be denormal or zero.
		 * Round will be unchanged since S|S..|S is unchanged.
		 */
		sticky =	( mlo & MASKLOW60 ) + MASKLOW60;
		mlo =	mlo | sticky;		/* <60>= <60> | (low60>0) */
		mlo =	mlo & ~ MASKLOW60;	/* zero low 60 bits       */
	}
 
	/* Round the upper 53 bits to IEEE nearest */
	mhi =	( mhi << 4 ) | (mlo >> 60);
	guard =	( mlo >> 59 ) & 1;
	sticky =	(mlo << 5) != 0;	/* 1 if low 59 bits nonzero */
	jsigned =	(mhi | sticky) & guard;	/* 0 or 1 */
	mhi =	mhi + jsigned;
	mlo =	mlo & MASKLOW60;		/* low 60 bits       */
	mlo =	mlo -( jsigned << 60);		/* mlo can become negative */
 
/* At this point mhi is the ieee64 mantissa and mlo<59:6> is adjustment.
 * Exponent of yhi is exponent(x)-IEEE_128_EXPO_ONE+DEXPONE.
 * yhi cannot underflow or overflow here.
 * if ylo is zero, it will have the same sign as the upper word.
 *
 * ylo can underflow to zero.  This routine lets multiply create denormals
 * when needed by scaling ylo by 2**52, and descaling at the end.
 * This is safe since ylo always .le. yhi/2**53 and yhi will be <2**1024,
 * so ylo < 2**(1024-53).
 */
	qtmp.d[1] =	D2POWER52 * (ieee64_t) (int64) mlo;
	iexp =		iexp + DEXPONE - IEEE_128_EXPO_ONE;
	jsigned =	iexp + qtmp.mips128.exp_low - DEXPONE - 4 - 59 - 49;

	qtmp.i64[0] =	( iexp << 52) + mhi + isign;  /* result nonzero */

	if ( ( jsigned <= 0) | ( qtmp.mips128.exp_low == 0) )
		qtmp.i64[1] =	isign;	/* ylo is underflow or zero */
	else {
		qtmp.i64[1] =	( qtmp.i64[1] & IEEE_64_MANTISSA )
				+ (((int64) qtmp.mips128.sign_low ) << 63 )
				+ ( jsigned << 52 ) + isign;
		qtmp.d[1] *=	(1.0 / D2POWER52);
	}

	*y =	qtmp.q;			/* nonzero finite normal result */
	return EX_REAL128;
} /* end _i128tom128 routine */
#endif /* end __mips */

#endif  /* end not _LD64 */
