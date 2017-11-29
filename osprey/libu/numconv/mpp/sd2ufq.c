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


#pragma ident "@(#) libu/numconv/mpp/sd2ufq.c	92.4	09/01/99 08:50:08"

#define DEBUGSW 0

#include <math.h>

#if defined(_UNICOS)
#include <fp.h>
#endif

#include <stdio.h>
#include <cray/fmtconv.h>
#include "qq_routines.h"

#if defined(_WORD32) || defined(__mips) || defined(_LITTLE_ENDIAN)
#define PORTABLE_INFINITY	0x7ff0000000000000LL
#define	NOT_SIGN_MASK	0x7fffffffffffffffLL
#define	SIGN_MASK	0x8000000000000000LL

#else
#define PORTABLE_INFINITY	0x7ff0000000000000
#define	NOT_SIGN_MASK	0x7fffffffffffffff
#define	SIGN_MASK	0x8000000000000000

#endif

#define	ZERO	'0'
#define	BLANK	' '
#define	SAFE_DENORM	4503599627370496.0

/*
	General:
		Convert the value to the formatted ASCII character field 
		specified.  Using the Fortran "Fw.d" format.

	Inputs:
		value - value to output
		fca - first character address of ASCII output field
		mode - flags to determine sign behavior
			0 = only minus sign required
			1 = sign required
			0200 = skip write of minus sign for -0.0
		w - total width of character field
		d - width of field after decimal point
		e - field width of exponent(ignored)
		p - scale factor

	Outputs:
		value - unchanged
		fca - unchanged
		mode - unchanged
		w - unchanged
		d - unchanged
		e - unchanged
		p - unchanged

        Examples:
		With value=0.105, mode=1, w=6, d=2, e=?, p=0 we will end up
		with a field filled with " +0.11" 

		With value=-9.995, mode=0, w=6, d=2, e=?, p=0 we will end up
		with a field filled with "-10.00" 

		With value=-0.005, mode=1, w=6, d=2, e=?, p=1 we will end up
		with a field filled with " -0.05" 

		With value=0.55, mode=0, w=6, d=2, e=?, p=-1 we will end up
		with a field filled with "  0.06" 

*/
long
*_sd2ufq(	const void	*value,
		long		*fca,
		const long	*mode,
		const long	*w,
		const long	*d,
		const long	*e,
		const long	*p)
{
	long	*position_pointer;
	long	*retval;
	long	field_width;
	long	flags;
	long	decimal_places;
	long	scale_factor;
	long	filler;
	long	*starting_integer_position;
	long	*starting_fraction_position;
	long	*end_pointer;
	FLOAT64 	qq_as64[5];
	FLOAT128	datum;
	FLOAT128	datumh;
	FLOAT128	datuml;
	FLOAT128	delta;
	char	sign;
	char	zsign;
	long	integer_digits;
	long	_OUTPUT_NAN_INF();
	long	power_of_ten;
	int32	denorm_2_adjust;
	long	digits_left;
	int32	roundby;
	int32	scaleoverten ;
	int32	roundto10;
	union	{FLOAT128 q[2]; uint64 i64[4]; uint32 i32[8];
		FLOAT128 d; uint64 i; FLOAT64 dbl; }	tmp,
					qpower,
					datumi,
					upper,
					lower,
					sign_bit;
	uint32	signexp;
/*
	set-up initial state
*/
#ifdef __mips
	/* convert double-double float to IEEE 128-bit float. */
	(void) _m128toi128(&datum, (FLOAT128 *)value);
#else
	datum = *(FLOAT128 *)value;
#endif
	if(DEBUGSW) printf("_sd2ufq:V:'%50.40Le' %d P f %d .%d e %d\n",
	datum,(int32)*p,(int32)*w,(int32)*d,(int32)*e );

	position_pointer = (long *)fca;
	field_width = *w;
	retval = position_pointer+field_width;
	flags = *mode;
	decimal_places = *d;
	scale_factor = *p;
	sign = BLANK;

/*
	perform some argument checking
*/
	if (decimal_places < 0) goto STARFILL;
	if (field_width < 0) goto STARFILL;

/*
	handle special case NaN and Infinity input data
*/
	tmp.d = datum;
	upper.i = tmp.i & SIGN_MASK ;
	tmp.i &= NOT_SIGN_MASK ;
	sign_bit.i = upper.i;
		if( DEBUGSW) printf( "upper.i, tmp.i: %x %x  %x %x\n",
			upper.i32[0], upper.i32[1], tmp.i32[0], tmp.i32[1]);
	if ( tmp.i >= QINFINITY) { /* if NaN or Inf, process */
		upper.i = upper.i | PORTABLE_INFINITY;
		if (ISNANL(datum) ) upper.i = upper.i | 1 ;  /* make it a NaN */
		if( DEBUGSW) printf( "upper.i, tmp.i: %x %x  %x %x\n",
			upper.i32[0], upper.i32[1], tmp.i32[0], tmp.i32[1]);
		(void)_OUTPUT_NAN_INF(upper.dbl,position_pointer,
			flags,field_width);
		goto DONE;
	};

/*
	convert denormals to normals and preserve the sign such that
	denormal = (2**-denorm_2_adjust)*normal. 
        We cannot count on hardware multiply to do this, since denormals
        aren't always allowed and can be treated as zero.
        Insert exponent(1.0) creating 1.xyyyyy (Don't trust scalbl for this)
        then while we know it's positive, convert to 0.xyyyyy .
        If x is 1 the exponent will be exp(0.5), if not, it will be less.
        In either case we have implicitly multiplied by 2^exp(0.5).
        Only after this is done can we sign the scaled datum.
        THIS WAS: denormal = (10**-denorm_adjust)*normal.

	_m128toi128 should never create an IEEE128 denormal, but
	test for it just the same.  The algorithm here depends on IEEE128
	arithmetic. Denormal support is not required.
*/
        denorm_2_adjust = 0;
        if ( ( tmp.i64[0] != 0 || tmp.i64[1] != 0) &&
                ( tmp.i64[0] >> QMANTISSA_TOPBITS ) == 0 ) {
                        denorm_2_adjust = QEXPONE-1 ;
                        tmp.i += ( ((int64) QEXPONE) << QMANTISSA_TOPBITS );
                        tmp.d = tmp.d - 1.0;
                        tmp.i += upper.i;
                        datum = tmp.d;
                        tmp.i -= upper.i;
#if defined(__mips)
			datum = 0.0;	/* IEEE128 underflow => 0.0 */
#endif
	}

/*
	determine sign
*/
	if ((flags & MODESN) != 0) sign = '+';
	zsign	= sign;
/*
 *	Use the unsigned integer sign value to check for sign, since
 *	the check for less than zero does not apply to a signed zero.
 *	For IRIX, the default is to skip the write of the minus sign
 *	for -0.0.  The mode bit MODEMSN indicates this skip.  If the
 *	assign option -Z on/off is present for external files, the
 *	handling of the minus sign for -0.0 can be switched for
 *	external files.
 *	if (datum < 0.0) {
 */
	if (sign_bit.i != 0) {
		if (((flags & MODEMSN) != 0) && (datum < 0.0) ||
		    ((flags & MODEMSN) == 0)) {
			sign = '-';
			datum = tmp.d ; /* remove sign. Don't use -datum on MIPS */
		}
	}

/*
	check for Zero input data
*/
	if (datum == 0.0) {
		if (field_width <= decimal_places+1) {
			if (field_width < decimal_places+1) goto STARFILL;
			if (sign != BLANK) goto STARFILL;
		};
		if (field_width < decimal_places+3) {
			filler = ZERO;
			if (sign != BLANK) filler = sign;
			if (field_width == decimal_places+2)
				*position_pointer++ = filler;
		}
		else {
			starting_integer_position = position_pointer+
						field_width-decimal_places-3;
			while (position_pointer < starting_integer_position) 
				*position_pointer++ = BLANK;
			*position_pointer++ = sign;
			*position_pointer++ = ZERO;
		};
		*position_pointer++ = '.';
		for ( ; decimal_places > 0; decimal_places--) 
			*position_pointer++ = ZERO;
		goto DONE;
        };

        if(DEBUGSW) printf(
        "_sd2ufq:scale in:'%50.40Le'/2^%d %d P f %d .%d e %d \n",
                datum, denorm_2_adjust, (int32)*p,(int32)*w,(int32)*d,(int32)*e );

/*
	scale the value such that (1.0 <= value < 10.0)
*/
        /****************************************************
         *  This scaler can be high on occasion.
	 *  It must never be lower than 1.0.
	 *  The digit creator can handle numbers in [1,20].
	 *  The power_of_ten is always correct.
	 *  A final /10 is implied if scaleoverten is 1.
         ****************************************************/
	_qqscale20( &datum, &tmp.q[0], &tmp.q[1], &denorm_2_adjust,
		&scaleoverten, &power_of_ten ) ;

	if(DEBUGSW)printf("_sd2ufq:scale out:'%50.40Le' %x *10^%d \n",
		tmp.q[0], tmp.i32[0], (int32) power_of_ten );
        if(DEBUGSW)printf("_sd2ufq:scale out:%x %x %x %x  %x %x %x %x \n",
                tmp.i32[0], tmp.i32[1], tmp.i32[2], tmp.i32[3],
                tmp.i32[4], tmp.i32[5], tmp.i32[6], tmp.i32[7]);

/*
	round the value according to decimal_places and scale_factor
*/
	if(DEBUGSW)printf("decimals+scale_factor + power_of_ten:%d %d %d\n",
		(int32) decimal_places, (int32) scale_factor, (int32) power_of_ten );

	filler = decimal_places + scale_factor + power_of_ten;
	if (filler >= IEEE_128_DEC_OUT_DIGITS) filler = IEEE_128_DEC_OUT_DIGITS-1;
	if (filler >= -1 ) {
/* begin rounding */
        /****************************************************
         *  The scaler can be high on occasion.
         *  10^-filler/2 = 5^-filler * 2^-(filler+1) is the round,
         *  if the scaled number is < 10; if >=10 use 10* the round.
         ****************************************************/
	if(DEBUGSW)printf("scaleoverten, filler:%d %d\n", scaleoverten, (int32) filler );
        roundby = scaleoverten-filler;
        /***************************************************
         * _qq_power5 returns 5.0^roundby as a 256-bit variable,
         * the result of multiplying coarse and fine powers.
         * (5^roundby)*(2^roundby)/2 = (10^roundby)/2 .
         ****************************************************/
        _qq_power5( &roundby, &qpower.q[0], &qpower.q[1] );

	/* scale */
/*	scalbl error:
        qpower.i64[0] += (((int64)roundby -1) << QMANTISSA_TOPBITS);
*/
        qpower.q[0] = scalbl(qpower.q[0], roundby -1);
	if(DEBUGSW)printf("rounding by %Le 0.5*10^%d\n",qpower.q[0], roundby );

        if(DEBUGSW) printf("_sd2ufq:round:'%50.40Le' %x *10^%d \n",
                qpower.q[0], qpower.i32[0], roundby );
        if(DEBUGSW)printf("_sd2ufq:round:%X %X %X %X  %X %X %X %X \n",
                qpower.i32[0], qpower.i32[1], qpower.i32[2], qpower.i32[3],
                qpower.i32[4], qpower.i32[5], qpower.i32[6], qpower.i32[7]);

        _qq_addunsigned( &qpower.q[0], &qpower.q[1],&tmp.q[0], &tmp.q[1],
                &qpower.q[0], &qpower.q[1] );

        if(DEBUGSW)printf("_sd2ufq:rounded:'%50.40Le' %x *10^%d \n",
                qpower.q[0], qpower.i32[0], (int32) power_of_ten );
        if(DEBUGSW)printf("_sd2ufq:rounded:%X %X %X %X  %X %X %X %X \n",
                qpower.i32[0], qpower.i32[1], qpower.i32[2], qpower.i32[3],
                qpower.i32[4], qpower.i32[5], qpower.i32[6], qpower.i32[7]);
	}
/* end rounding */
	else {
		qpower.q[0] = tmp.q[0];
		qpower.q[1] = tmp.q[1];
	}

        if(DEBUGSW)printf("_sd2ufq:rounded:%X %X %X %X  %X %X %X %X \n",
                qpower.i32[0], qpower.i32[1], qpower.i32[2], qpower.i32[3],
                qpower.i32[4], qpower.i32[5], qpower.i32[6], qpower.i32[7]);

        /****************************************************
         *  If the rounded number ended up >= 10, the digit output
         *  will compensate, so increment power_of_ten and continue.
         *  G as F needs to know if rounding made it over 10.
         *  Convert to fixed point in qq_as64[].
         ****************************************************/
	QQCONVERT2AS64( qpower, qq_as64, power_of_ten, scaleoverten, roundto10)
/*
	output the value
*/
	digits_left = IEEE_128_DEC_OUT_DIGITS;
	integer_digits = 1+scale_factor+power_of_ten;
	if (integer_digits > 0) { /* number > 1.0 */
		if (integer_digits >= (field_width-decimal_places-1))
			if ((integer_digits > (field_width-decimal_places-1))
				|| (sign != BLANK)) goto STARFILL;
		starting_integer_position = position_pointer+
				(field_width-decimal_places-1-integer_digits);
		while (position_pointer < (starting_integer_position-1))
			*position_pointer++ = BLANK;
		if (position_pointer < starting_integer_position)
			*position_pointer++ = sign;
		PUTDIGITLOOP( qq_as64,
			starting_integer_position+ integer_digits,
			position_pointer, digits_left )
		*position_pointer++ = '.';
		starting_fraction_position = position_pointer;
	}
        else { /* number < 1.0 */
		if (-integer_digits >= decimal_places) {
			integer_digits = -decimal_places;
			/* for small numbers, check flag for write
			 * of minus sign for -0.0.
			 */
			if ((flags & MODEMSN) != 0)
				sign	= zsign;
		}
		if (field_width <= (decimal_places+1))
			if ((field_width < (decimal_places+1)) ||
				(sign != BLANK)) goto STARFILL;
		starting_fraction_position = position_pointer+
				(field_width-decimal_places);
		while (position_pointer < (starting_fraction_position-3))
			*position_pointer++ = BLANK;
		if (position_pointer == (starting_fraction_position-3)) {
			*position_pointer++ = sign;
			*position_pointer++ = ZERO;
		}
		else {
			if (position_pointer == (starting_fraction_position-
				2)) {
				filler = ZERO;
				if (sign != BLANK) filler = sign;
				*position_pointer++ = filler;
			};
		};
		*position_pointer++ = '.';
		for (; integer_digits < 0; integer_digits++)
			*position_pointer++ = ZERO;
	};
/*********************** ELIMINATED CODE *********************
	while (position_pointer < (starting_fraction_position+decimal_places)) {
		GETONEDIGIT( position_pointer, datumh, datuml )
	};
*/
	PUTDIGITLOOP( qq_as64,
		starting_fraction_position+decimal_places,
		position_pointer, digits_left )
	goto DONE;

STARFILL:
	for ( ; field_width > 0; field_width--)
		*position_pointer++ = '*';
	goto DONE;

DONE:
	return(retval);
}
