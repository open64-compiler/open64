/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


#pragma ident "@(#) libu/numconv/mpp/defgu2s.c	92.5	09/27/99 15:31:29"

#include <stdio.h>
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

#define DEBUGSW 0 /* 0=off, 1= on */

#if defined(_WORD32) || defined(__mips) || defined(_LITTLE_ENDIAN)
#define	PORTABLE_INFINITY	0x7ff0000000000000LL
#define	PORTABLE_FLOAT_MAX	0x47efffffefffffffLL
#define	PORTABLE_FLOAT_MIN	0x380fffffe0000000LL

#if defined(_SOLARIS) || defined(__mips) || \
   (defined(_ABSOFT) && !defined(_LD64)) || defined(_LITTLE_ENDIAN)
extern FLOAT128 _QHIPOWER_OF_FIVE_TABLE[];
extern FLOAT128 _QPOWER_OF_FIVE_TABLE[];
union{ FLOAT128 q ; int64 i64[2]; } qinfinity ;
#else
union {FLOAT64 d; int64 i; int32 i32[4]; } tmp;
#endif

#else
#define	PORTABLE_INFINITY	0x7ff0000000000000
#define	PORTABLE_FLOAT_MAX	0x47efffffefffffff
#define	PORTABLE_FLOAT_MIN	0x380fffffe0000000

#ifndef _LD64
extern FLOAT128 _QHIPOWER_OF_FIVE_TABLE[];
extern FLOAT128 _QPOWER_OF_FIVE_TABLE[];
union{ FLOAT128 q ; int64 i64[2]; } qinfinity ;
#else
union {FLOAT64 d; int64 i; int32 i32[4]; } tmp;
#endif		/* end of !LD64 */

#endif       /* end of if-else _WORD32 or __mips */

#define	BASE		10
/* bits in high_number -4 for BASE multiply */
#define	BITS_INT	50
#define	TABLELIM	400
#define MAX_EXPONENT	1024
#define MIN_EXPONENT	-1023
#define BLANK		' '
#define	DP_FOUND	-1
#define	DP_NOT_FOUND	0
#define	POSITIVE	0
#define	NEGATIVE	-1

#define MASKLOW16	0xffff
#define MASK32		0xffffffff

#define DIGITCARRY(low, high) { \
	uint64 carry_up ; \
	carry_up = ((uint64)low) >> BITS_INT; \
	low -= (carry_up << BITS_INT); \
	high = (high << 3) + (high << 1) + carry_up; \
} \
/* end DIGITCARRY macro */

/* XDIGITADD accumulate 250 bits (128) or 100 (64/32) */
#define XDIGITADD(digit) { \
	low_number = (low_number << 3) + (low_number << 1) + (digit); \
	if ((flags & MODEDP) != 0) { \
		DIGITCARRY(low_number,  mid1_number) \
		DIGITCARRY(mid1_number, mid2_number) \
		DIGITCARRY(mid2_number, mid3_number) \
		DIGITCARRY(mid3_number, high_number) \
\
		if(DEBUGSW)printf("add: %x  %e  %e %e %e %e %e\n", (int)digit, \
			scalb(1.0,200)*(ieee64_t) high_number \
			+scalb(1.0,150)*(ieee64_t) mid3_number \
			+scalb(1.0,100)*(ieee64_t) mid2_number \
			+scalb(1.0,50)* (ieee64_t) mid1_number \
			+(ieee64_t)low_number, \
			(ieee64_t) high_number, \
			(ieee64_t) mid3_number, \
			(ieee64_t) mid2_number, \
			(ieee64_t) mid1_number, \
			(ieee64_t) low_number); \
	} else { \
        	DIGITCARRY(low_number,high_number) \
	} \
	decimal_exponent = decimal_exponent + \
				decimal_point_indicator; \
	digit_found = POSITIVE; \
	if (((uint64)high_number >> (BITS_INT-4)) != 0) goto REALOVFL; \
} \
/* end XDIGITADD macro */


/* XDIGITADD accumulate 100 (64/32) */
#define XDIGITADD64(digit) { \
	low_number = (low_number << 3) + (low_number << 1) + (digit); \
        DIGITCARRY(low_number,high_number) \
	decimal_exponent = decimal_exponent + \
				decimal_point_indicator; \
	digit_found = POSITIVE; \
	if (((uint64)high_number >> (BITS_INT-4)) != 0) goto REALOVFL; \
} \
/* end XDIGITADD64 macro */


/*
	General:
		Scan from left to right and convert the ASCII character
		field to a binary floating point value. 

	Inputs:
		fca - first character address of character field
		fw - width of character field
		lcap1 - last character plus one
		mode - flags to determine how blanks are handled
			0 = blanks are delimiters
			1 = blanks are skipped
			2 = blanks are zeroes
		    add 4 if double precision (128-bit) conversion
		    add 0x20 if half precision (32-bit) conversion
		result - ignored on input
		status - ignored on input
		d - assumed decimal point position
		p - scale factor

	Outputs:
		fca - unchanged
		fw - unchanged
		lcap1 - last character looked at in the conversion plus one
		mode - unchanged
		result - result of conversion
		status - exit status(one of the following)
			 5 = 32-bit real
			 4 = 128-bit real
			 3 = 64-bit real
			-1 = illegal character input
			-3 = exponent underflow
			-4 = exponent overflow
			-5 = null field
		d - unchanged
		p - unchanged

	Examples:
		For the Fortran edit descriptor "BZ,-4pE30.20" we would have
		fw = 30, mode = 2, d = 20, and p = -4.

		For the Fortran edit descriptor "BN,f10.2" we would have
		fw = 10, mode = 1, d = 2, and p = 0.

		The other input parameters fca and lcap1 depend on where the
		character string resides in memory. lcap1 is used as a 
		boundary for valid input data(anything at or beyond lcap1 is
		assumed to be blanks and will be treated according to the way
		blanks are to be treated, this becomes significant if the
 *		fca and fw combination goes beyond the lcap1 boundary).
 *
 */
int
_defgu2sd(	const long	*fca,
		const long	*fw,
		long		**lcap1,
		const long	*mode,
		void		*result,
		long		*status,
		const long	*d,
		const long	*p)
{
#ifdef KEY /* Bug 9020 */
        /* Added initializations to local variable declarations; where
	 * possible moved the local declarations from the scope of the
	 * entire function to the most deeply nested scope possible.
	 * Unable to establish that this bug was due to an uninitialized
	 * variable rather than an optimizer bug, but the code is safer now. */
#endif /* KEY Bug 9020 */
	union{
		FLOAT128 q;
		FLOAT64 d;
		int64 i;
		int64 i64[2];
		int32 i32[4]; } tmp; /* init below */
	FLOAT128 qpre_result = 0;
	union { FLOAT128 q[2]; int64 i64[4]; int32 i32[8];} t2; /* init below */
	union {FLOAT64 d; int64 i;} infinity; /* init below */
	long *position_pointer = (long *)fca;
	long field_width = *fw;
	long flags = *mode;
	long *l_char_address_p1 = *lcap1;
	long decimal_places = *d;
	long scale_factor = *p;
	long decimal_point_indicator = DP_NOT_FOUND;
	long decimal_exponent = 0;
	long digit_found = NEGATIVE;
	int64 low_number = 0;
	int64 mid1_number = 0;
	int64 mid2_number = 0;
	int64 mid3_number = 0;
	int64 high_number = 0;
	long exitstatusreal; /* init below */
	int32 exp_number = 0;
	long blank = BLANK;
	long extra_zeroes = 0;
	long mantissa_sign = POSITIVE;
	long exponent_sign = POSITIVE;
	ieee64_t pre_result = 0;
	extern ieee64_t _POWER_OF_FIVE_TABLE[];
#ifdef KEY /* Bug 8105 */
	int found_comma = 0;
#endif /* KEY Bug 8105 */

/*
	set-up initial state
*/
	infinity.i = PORTABLE_INFINITY;
	tmp.i64[0] = tmp.i64[1] = 0;
	t2.i64[0] = t2.i64[1] = t2.i64[2] = t2.i64[3] = 0;

	if(DEBUGSW)
		printf("_def:posn=%llx fw=%x flag=%x lcap1=%p *lcap1=%p d=%x p=%x\n",
		       (long long)*fca, (int)*fw , (int)*mode, lcap1,
		       *lcap1, (int)*d, (int)*p );

#if defined(_SOLARIS) || (defined(_CRAYIEEE) && !defined(_LD64)) || \
    defined(__mips) || (defined(_ABSOFT) && !defined(_LD64)) || \
    defined(_LITTLE_ENDIAN)
	if( (flags&MODEDP) != 0)
		exitstatusreal = EX_REAL128;
	else
#endif
		exitstatusreal = EX_REAL64;

/*
	check for valid field width and special blank handling
*/
	if ((flags & (MODEBZ | MODEBN)) == 0) blank = 511; /* delimiters */
	if (field_width == 0) goto REALZERO;
	if (field_width < 0) goto NODATA;

/*
	determine if we have extra blanks or zeroes appended to the input 
	string and reset the field width if needed.
*/
	if (position_pointer+field_width <= l_char_address_p1) {
		l_char_address_p1 = position_pointer+field_width;
	}
	else {
		if ((flags & MODEBZ) != 0) extra_zeroes = position_pointer+
					field_width-l_char_address_p1;
		field_width = l_char_address_p1-position_pointer;
		if (field_width <= 0) goto REALZERO;
	}

/*
	skip leading blanks - unless blanks are delimiters
*/
	while ((*position_pointer == blank) &&
		(position_pointer < l_char_address_p1)) position_pointer++;
	if (position_pointer == l_char_address_p1) goto REALZERO;

/*
	check for mantissa sign
*/
	if (*position_pointer == '-' || *position_pointer == '+') {
		if (*position_pointer == '-') mantissa_sign = NEGATIVE;
		position_pointer++;
		if (position_pointer == l_char_address_p1) {
			if (extra_zeroes != 0) goto REALZERO;
			goto NODATA;
		}
	}

/*
	loop through characters and build the mantissa
*/
	while(position_pointer < l_char_address_p1) {
		if (*position_pointer <= '9' && *position_pointer >= '0') {
			/* accumulate 400 bits (128) or 100 (64) */
			/* No code should fall through to XDIGITADD64.
			 * This will cause problems if it does.  An
			 * error will be added for this case.
			 */
#if defined(_SOLARIS) || (defined(_CRAYIEEE) && !defined(_LD64)) || \
    defined(__mips) || (defined(_ABSOFT) && !defined(_LD64)) || \
    defined(_LITTLE_ENDIAN)
			XDIGITADD(*position_pointer-'0')
#else
			XDIGITADD64(*position_pointer-'0')
#endif
		}
		else { /* not a numerical digit, look for [.+-DdEe ] */
#ifdef KEY /* Bug 8105 */
			if (',' == *position_pointer) {
				found_comma = 1;
				break;
			}
			else
#endif /* KEY Bug 8105 */
			if ( *position_pointer == BLANK) { /* blank */
				if ((flags & (MODEBZ | MODEBN)) == 0) break;
				if ((flags & MODEBZ) != 0) { /* blanks = 0 */
					XDIGITADD(0)
				}
			}
			else { /* not a blank, look for [.] */
				if ( *position_pointer != '.') goto REALEXP;
				if ( decimal_point_indicator == DP_FOUND)
					goto BADCHAR;
				decimal_places = 0;
				decimal_point_indicator = DP_FOUND;
			}
		}
		position_pointer++;
	}

/*
	end of input field, so determine how to handle potential trailing zeroes
*/
	if (decimal_point_indicator == DP_FOUND) {
		if (digit_found != POSITIVE) {
			if (extra_zeroes != 0) goto REALZERO;
			goto NODATA;
		}
	}
	else{
		if (digit_found != POSITIVE) goto REALZERO;
		exp_number = extra_zeroes;
	}
	goto REALEND2;

/*
	This section is for taking care of the mantissa characters after the 
	mantissa has been saturated with data.
*/
REALOVFL:
	position_pointer++;

/*
	loop through characters that build the mantissa, but ignore the data 
	and keep track of the decimal positions.
*/
	while(position_pointer < l_char_address_p1) {
		if (*position_pointer <= '9' && *position_pointer >= '0') {
			decimal_exponent = decimal_exponent +
					decimal_point_indicator + 1;
		}
		else { /* not a numerical digit, look for [.+-DdEe ] */
#ifdef KEY /* Bug 8105 */
			if (',' == *position_pointer) {
				found_comma = 1;
				break;
			}
			else
#endif /* KEY Bug 8105 */
			if ( *position_pointer == BLANK) { /* blank */
				if ((flags & (MODEBZ | MODEBN)) == 0) break;
				if ((flags & MODEBZ) != 0) { /* blanks = 0 */
					decimal_exponent = decimal_exponent +
						decimal_point_indicator + 1;
				}
			}
			else { /* not a blank, look for [.] */
				if ( *position_pointer != '.') goto REALEXP;
				if ( decimal_point_indicator == DP_FOUND)
					goto BADCHAR;
				decimal_places = 0;
				decimal_point_indicator = DP_FOUND;
			}
		}
		position_pointer++;
	}

/*
	end of input field, so determine how to handle potential trailing zeroes
*/
	if (decimal_point_indicator == DP_NOT_FOUND) {
		exp_number = extra_zeroes;
	}
	goto REALEND2;


/*
	Constructing the mantissa is complete. Start constructing the exponent.
*/
REALEXP:
	if (*position_pointer != '-' && *position_pointer != '+') {
		if (*position_pointer != 'D' && *position_pointer != 'd' &&
			*position_pointer != 'E' && *position_pointer != 'e')
			goto BADCHAR;
		position_pointer++;
		while (position_pointer < l_char_address_p1) {
			if (*position_pointer != BLANK) goto EXPSIGN;
			if ((flags & (MODEBZ | MODEBN)) == 0) goto NODATA;
			if ((flags & MODEBZ) != 0) goto EXPSIGN;
			position_pointer++;
		}
		if (position_pointer == l_char_address_p1) {
			if ((extra_zeroes == 0) || (digit_found != POSITIVE))
				goto NODATA;
			goto REALEND2;
		}
	}

EXPSIGN:
	if (digit_found == NEGATIVE) goto NODATA;
	digit_found = NEGATIVE;
	scale_factor = 0;
	if (*position_pointer == '+' || *position_pointer == '-') {
		if (*position_pointer == '-') exponent_sign = NEGATIVE;
		position_pointer++;
		if (position_pointer == l_char_address_p1) {
			if (extra_zeroes == 0) goto NODATA;
			goto REALEND2;
		}
 	}
	while(position_pointer < l_char_address_p1) {
		if (*position_pointer <= '9' && *position_pointer >= '0') {
			exp_number = exp_number*BASE+(*position_pointer-'0');
			digit_found = POSITIVE;
			if (exp_number >= EXPLIMIT) goto EXPOVFLW;
			DEBUGQ("_def:building exponent:%x %x %x %x \n",exp_number)
		}
		else {
#ifdef KEY /* Bug 8105 */
			if (',' == *position_pointer) {
				found_comma = 1;
				goto REALEND2;
			}
			else
#endif /* KEY Bug 8105 */
			if ( *position_pointer != BLANK) goto BADCHAR;
			if ((flags & (MODEBZ | MODEBN)) == 0) {
				if (digit_found != POSITIVE) goto NODATA;
				goto REALEND2;
			}
			if ((flags & MODEBZ) != 0) {
				exp_number = exp_number*BASE;
				digit_found = POSITIVE;
				if (exp_number >= EXPLIMIT) goto EXPOVFLW;
			}
		}
		position_pointer++;
	}
	if (extra_zeroes != 0) {
		digit_found = POSITIVE;
		long count = 0;
		for (count = extra_zeroes; count != 0; count--) {
			exp_number = exp_number*BASE;
			if (exp_number >= EXPLIMIT) goto EXPOVFLW;
		}
	}
	if (digit_found != POSITIVE) goto NODATA;

REALEND2:
	if (exponent_sign == NEGATIVE) exp_number = -exp_number;
	exp_number = decimal_exponent+exp_number-decimal_places-scale_factor;
	*status = exitstatusreal;
#if defined(_SOLARIS) || (defined(_CRAYIEEE) && !defined(_LD64)) || \
    defined(__mips) || (defined(_ABSOFT) && !defined(_LD64)) || \
    defined(_LITTLE_ENDIAN)
	if( (flags&MODEDP) == 0 ){
#endif
		/* 64- or 32-bit (half precision) */
		ieee64_t exact_high_number = (ieee64_t)high_number;
		ieee64_t exact_low_number = (ieee64_t)low_number;
		exact_high_number *= scalb(1.0,BITS_INT);
		tmp.d = exact_high_number + exact_low_number;
		exact_low_number = (exact_high_number-tmp.d)+exact_low_number;
		exact_high_number = tmp.d;
		tmp.i = (tmp.i >> 27) << 27;
		ieee64_t x = tmp.d;
		ieee64_t y = exact_high_number - x;
		if (exp_number < -TABLELIM || exp_number > TABLELIM) {
			goto EXPOVFLW;
		}
		else {
			ieee64_t table_high = _POWER_OF_FIVE_TABLE[2*TABLELIM +
					exp_number*2];
			ieee64_t table_low = _POWER_OF_FIVE_TABLE[2*TABLELIM +
					exp_number*2+1];
			tmp.d = table_high;
			tmp.i = (tmp.i >> 27) << 27;
			ieee64_t a = tmp.d;
			ieee64_t b = table_high - a;
			pre_result = a*x + (a*y + x*b +(b*y +
				(table_high*exact_low_number +
				exact_high_number*table_low +
				table_low*exact_low_number)));
			if (pre_result != 0.0) {
				if ((long)logb(pre_result) >=
					(MAX_EXPONENT-exp_number) ||
		   	    		(long)logb(pre_result) <=
		   	    		(MIN_EXPONENT-exp_number))
						goto EXPOVFLW;
				pre_result *= scalb(1.0,exp_number);
			}
		}
#if defined(_SOLARIS) || (defined(_CRAYIEEE) && !defined(_LD64)) || \
    defined(__mips) || (defined(_ABSOFT) && !defined(_LD64)) || \
    defined(_LITTLE_ENDIAN)
	}
	else {
		/* begin else block for 128-bit IEEE version of above.
		 *
		 * Truncate 5-part integer digit string at 241 bits.
		 *
		 * n = digits_skipped-decimal_places+ P_scale_factor
		 * lo = n&63 ; hi = n>>6
		 *
		 * Multiply 5.0^hi * 5.0^lo, rounding at 256 bits.
		 *
		 * Multiply 5.0^n * float(digits) rounding at 128.
		 *
		 * Multiply by 2.0^n
		 *
		 * Denormals are caught and zeroed.
		 *
		 * Powers of five in the table are truncated but are
		 * incremented when used, if inexact, so at least the
		 * exact value is used.
		 * Error is in the range [+0, +2) ulps (239 bits).
		 *
		 * Paxson and Kahan show that 133 (2*53+27) mantissa
		 * bits are needed for at least one IEEE-64 input.
		 * It is very likely that some failing cases can be
		 * constructed since we have at most 2*113+15 bits
		 * of mantissa.
		 *
		 * Hopefully, they will be extremely rare in practice:
		 * 76-bit accuracy for IEEE-64 is apparently acceptable
		 * to customers.  Of most concern are n*2^k and n*10^k
		 * which have exact representations.  If n is greater
		 * than 241 bits, such digit strings are always truncated,
		 * but the result is rounded to 113 bits of accuracy.
		 * Paxson and Kahan did not prove that 133 bits was
		 * adequate for all inputs -- it merely was the most
		 * stringent they found among those they constructed.
		 *
		 * The following analysis is approximate:
		 *
		 * For the round of bit 24 to be inaccurate at 76 bits the
		 * true value must end in zero followed by 52 ones or
		 * one followed by 52 zeroes.  For a random number,
		 * this has probability 2^-52, or 1 in 10^15, but by
		 * chance many such numbers may exist, since there are
		 * 256*log10(2) exponents and 2^100 unique digit
		 * strings, perhaps as many as 10^16 =
		 * (256*log10(2))*2^100/2^52.  To reduce this
		 * to <.01, but not eliminate it, we would need 61
		 * more bits, for a total of 137 (three words).
		 *
		 * For the round of bit 53 to be inaccurate at 76 bits the
		 * true value must end in zero followed by 23 ones or
		 * one followed by 23 zeroes.  For a random number,
		 * this has probability 2^-23, or 1 in 10^7, but by
		 * chance many such numbers may exist, since there are
		 * 2048*log10(2) exponents and 2^100 unique digit
		 * strings, perhaps as many as 10^25 =
		 * (2048*log10(2))*2^100/2^23.  To reduce this
		 * to <.01, but not eliminate it, we would need 93
		 * more bits, for a total of 169 (three words).
		 *
		 * For the round of bit 112 to be inaccurate the true
		 * must end in zero followed by 127 ones or one
		 * followed by 127 zeroes.  For a random number,
		 * this has probability 2^-127, or 1 in 10^38, but by
		 * chance many such numbers may exist, since there are
		 * 32768*log10(2) exponents and 2^250 unique digit
		 * strings, perhaps as many as 10^42 =
		 * (32768*log10(2))*2^250/2^127.  To reduce this to
		 * <.01, but not eliminate it, we would need 143 more
		 * bits, for a total of 384 (six words).
		 */

		if ( (high_number | low_number | mid1_number |
			mid2_number | mid3_number) == 0)
				goto REALZERO;

		/* float the unsigned integer */
		int inexact = 0;
		_qqfloat_uint250( &t2.q[0], &inexact, &low_number,
			&mid1_number, &mid2_number, &mid3_number,
			&high_number);
		FLOAT128 qexact_high_number = t2.q[0];
		FLOAT128 qexact_low_number  = t2.q[1];

		DEBUGQ("_def:qexact_high_number:%x %x %x %x \n",
			qexact_high_number)
		DEBUGQ("_def:qexact_low_number:%x %x %x %x \n",
			qexact_low_number)
		if(DEBUGSW) printf("_def:EXPLIMIT, exp_number:%d %d \n",
			EXPLIMIT, exp_number);
		if (exp_number < (-EXPLIMIT) || (exp_number > EXPLIMIT))
			goto EXPOVFLW;
		else {
			FLOAT128 qhi = 0;
			FLOAT128 qlo = 0;
			FLOAT128 qx = 0;
			FLOAT128 qy = 0;
			if( _qq_power5( &exp_number, &qhi, &qlo ) < 0 )
				goto EXPOVFLW;
			DEBUGQ("_def:qexact_high_number:%x %x %x %x \n",
				qexact_high_number)
			DEBUGQ("_def:qexact_low_number:%x %x %x %x \n",
				qexact_low_number)
			DEBUGQ("_def:qhi:%x %x %x %x \n",qhi)
			DEBUGQ("_def:qlo:%x %x %x %x \n",qlo)

			/* <1:0> 01 round at 128,
			 * <1:0> 02 round at 256,
			 * <3:2> 00: round to infinity,
			 * <3:2> 01: round to nearest (biased),
			 * <3:2> 11: round to nearest (even, if tie),
			 * This is round to nearest (biased) at bit 128.
			 */
			int iround128 = 1+4;
			_qmult3( &iround128,&qhi,&qlo, \
				&qexact_high_number, &qexact_low_number, \
				&qx, &qy);
			qexact_high_number = qx;
			qexact_low_number = qy;
		}
		DEBUGQ("_def:qexact_high_number:%08x%08x%08x%08x\n",
			qexact_high_number)
		DEBUGQ("_def:qexact_low_number: %08x%08x%08x%08x\n",
			qexact_low_number)
		qpre_result = qexact_high_number;
		if (qpre_result != 0.0) {
			if ((long)logbl(qpre_result) >=
				(QMAX_EXPONENT-exp_number) ||
				(long)logbl(qpre_result) <=
				(QMIN_EXPONENT-exp_number))
					goto EXPOVFLW;
			qpre_result = scalbl(qpre_result,exp_number);
		}

	/* end else block for 128-bit IEEE */
        }
#endif

	goto DONE;

EXPOVFLW:
	if ((exponent_sign == POSITIVE) &&
		(low_number != 0 || high_number !=0 ||
		 mid1_number != 0 || mid2_number !=0 ||
		 mid3_number != 0)) {
			pre_result = infinity.d;
#if defined(_SOLARIS) || (defined(_CRAYIEEE) && !defined(_LD64)) || \
    (defined(_ABSOFT) && !defined(_LD64)) || defined(_LITTLE_ENDIAN)
			qinfinity.i64[0] = IEEE_128_EXPONENT;	/* ieee128 */
			qinfinity.i64[1] = 0;
			qpre_result = qinfinity.q;	/* 7fff0..0 */
#elif defined(__mips)
			qinfinity.i64[0] = PORTABLE_INFINITY;	/* mips128 */
			qinfinity.i64[1] = 0;
			qpre_result = qinfinity.q;	/* 7ff00..0 */
			/* status protects infinity from conversion at DONE. */
#endif
			*status = EX_EXPOFLO;
	}
	else {
		pre_result = 0.0;
#if defined(_SOLARIS) || (defined(_CRAYIEEE) && !defined(_LD64)) || \
    defined(__mips) || (defined(_ABSOFT) && !defined(_LD64)) || \
    defined(_LITTLE_ENDIAN)
		qpre_result = 0.0;
#endif
		if (low_number != 0 || high_number !=0 ||
			mid1_number != 0 || mid2_number !=0 ||
			mid3_number != 0) {
				*status = EX_EXPUFLO;
		}
		else {
			*status = exitstatusreal;
		};
	}
	goto DONE;

NODATA:
	pre_result = 0.0;
#if defined(_SOLARIS) || (defined(_CRAYIEEE) && !defined(_LD64)) || \
    defined(__mips) || (defined(_ABSOFT) && !defined(_LD64)) || \
    defined(_LITTLE_ENDIAN)
	qpre_result = 0.0;
#endif
	mantissa_sign = POSITIVE;
	*status = EX_NULLFLD;
	goto DONE;

BADCHAR:
	pre_result = 0.0;
#if defined(_SOLARIS) || (defined(_CRAYIEEE) && !defined(_LD64)) || \
    defined(__mips) || (defined(_ABSOFT) && !defined(_LD64)) || \
    defined(_LITTLE_ENDIAN)
	qpre_result = 0.0;
#endif
	mantissa_sign = POSITIVE;
	*status = EX_ILLCHAR;
	goto DONE;

REALZERO:
	pre_result = 0.0;
#if defined(_SOLARIS) || (defined(_CRAYIEEE) && !defined(_LD64)) || \
    defined(__mips) || (defined(_ABSOFT) && !defined(_LD64)) || \
    defined(_LITTLE_ENDIAN)
	qpre_result = 0.0;
#endif
	*status = exitstatusreal;

DONE:
#ifdef KEY /* Bug 8105 */
        if (found_comma) {
		*status = EX_ILLCHAR;
	}
#endif /* KEY Bug 8105 */
	*lcap1 = position_pointer;
	if ((flags & MODEHP) != 0) {

		/* MODEHP(0x10) !=0: half precision(32) */
		tmp.d = pre_result;
		if (tmp.i > PORTABLE_FLOAT_MAX) {
			*status = EX_EXPOFLO;
			int32 temp = (mantissa_sign == POSITIVE) ? 0x7f800000 : 0xff800000;
			*(ieee32_t *)result = *(ieee32_t *)&temp;
		}
		else {
			if (tmp.i < PORTABLE_FLOAT_MIN && tmp.i != 0) {
				*status = EX_EXPUFLO;
				pre_result = 0.0;
			}
			if (*status == EX_REAL64) *status = EX_REAL32;
			*(ieee32_t *)result = (mantissa_sign ==
				POSITIVE) ? pre_result :
				(pre_result * (-1));
		}
        }
	else {
#if defined(_SOLARIS) || (defined(_CRAYIEEE) && !defined(_LD64)) || \
    (defined(_ABSOFT) && !defined(_LD64)) || defined(_LITTLE_ENDIAN)
		DEBUGQ("done:qpre_result: %08x%08x%08x%08x\n",qpre_result)
		if ((flags & MODEDP) != 0) {
			/* MODEDP: double prec(128) */
			*(FLOAT128 *)result = (mantissa_sign ==
				POSITIVE) ? qpre_result :
				COPYSIGNL(qpre_result,-1.0L);
		}
		else {
			*(ieee64_t *)result = (mantissa_sign ==
				POSITIVE) ? pre_result :
				(pre_result * (-1));
		}
	}
#elif defined(__mips)
		DEBUGQ("done:qpre_result: %08x%08x%08x%08x\n",qpre_result)
		if ((flags & MODEDP) != 0) {
			/* MODEDP: double prec(128) */
			*(FLOAT128 *)result = (mantissa_sign ==
				POSITIVE) ? qpre_result :
				COPYSIGNL(qpre_result,-1.0L);
			/* convert IEEE 128-bit float to double-double.
			 * Make status reflect possible over/underflow.
			 */
			if ( *status == EX_REAL128 )
				*status =
				    _i128tom128((FLOAT128 *)result,
						(FLOAT128 *)result);
		}
		else {
			*(ieee64_t *)result = (mantissa_sign ==
				POSITIVE) ? pre_result :
				copysign(pre_result,-1.0);
		}
	}
#else
		*(ieee64_t *)result = (mantissa_sign == POSITIVE) ?
			pre_result : copysign(pre_result,-1.0);
	}
#endif
	return(*status);
}  /* end defgu2sd routine */
