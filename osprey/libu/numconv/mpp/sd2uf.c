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



#pragma ident "@(#) libu/numconv/mpp/sd2uf.c	92.4	09/01/99 08:50:08"

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#if defined(_UNICOS)
#include <fp.h>
#endif
#include <cray/fmtconv.h>
#include "qq_routines.h"

#define DEBUGSW 0

#if defined(_WORD32) || defined(__mips) || defined(_LITTLE_ENDIAN)
#define	UPPER_MASK	0xfffffffff8000000LL
#define PORTABLE_INFINITY	0x7ff0000000000000LL
#define	NOT_SIGN_MASK	0x7fffffffffffffffLL
#define	SIGN_MASK	0x8000000000000000LL
#define	CVT32_64_MASK	0x3810000000000000LL
#else
#define	UPPER_MASK	0xfffffffff8000000
#define PORTABLE_INFINITY	0x7ff0000000000000
#define	NOT_SIGN_MASK	0x7fffffffffffffff
#define	SIGN_MASK	0x8000000000000000
#define	CVT32_64_MASK	0x3810000000000000
#endif

#if defined(_SOLARIS) || (defined(_CRAYIEEE) && !defined(_LD64)) || \
    defined(__mips) || (defined(_ABSOFT) && !defined(_LD64)) || \
    defined(_LITTLE_ENDIAN)
/* _sd2ufq is the equivalent routine for FLOAT128 */
long *  _sd2ufq(
                const void      *value,
                long            *fca,
                const long      *mode,
                const long      *w,
                const long      *d,
                const long      *e,
                const long      *p);
#endif

#define	ZERO	'0'
#define	BLANK	' '
#define	EXP_LIM		400
#define	MANTISSA_BITS	52
#define	SAFE_DENORM	4503599627370496.0
/* double alog10_2 = .301029995663981195214; is in qq_routines.h */

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
*_sd2uf(	const void	*value,
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
	FLOAT64	datum;
#ifdef KEY /* Bug 8773 */
	volatile FLOAT64	datumh;
	volatile FLOAT64	datuml;
#else /* KEY Bug 8773 */
	FLOAT64	datumh;
	FLOAT64	datuml;
#endif /* KEY Bug 8773 */
	FLOAT64	tableh;
	FLOAT64	tablel;
	FLOAT64	delta;
	FLOAT64	datumhh;
	FLOAT64	datumhl;
	char	sign;
	char	zsign;
	long	integer_digits;
	long	_OUTPUT_NAN_INF();
	long	power_of_ten;
	long	denorm_adjust;
	long	digits_left;
	int32	scaleoverten;
	int32	roundto10;
	uint64	upper_mask1 = UPPER_MASK;
#ifdef KEY /* Bug 9019 */
        volatile
#endif /* KEY Bug 9019 */
	union	{FLOAT64 d; uint64 i;}	tmp,
					upper,
					lower,
					sign_bit;
	extern	FLOAT64 _POWER_OF_FIVE_TABLE[];

#if defined(_SOLARIS) || (defined(_CRAYIEEE) && !defined(_LD64)) || \
    defined(__mips) || (defined(_ABSOFT) && !defined(_LD64)) || \
    defined(_LITTLE_ENDIAN)
	/* set-up initial state */
	if ((*mode & MODEDP) != 0) {
		return( _sd2ufq( value, fca, mode, w, d, e, p) );
	}
#endif


/*
	set-up initial state
*/
	digits_left = IEEE_64_DEC_OUT_DIGITS;	/* nonzero digits for 32/64-bit */
	if ((*mode & MODEHP) != 0) {
		datum = (ieee64_t) (*(ieee32_t *)value);
	}
	else {
		datum = *(ieee64_t *)value;
	}
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
	sign_bit.i = tmp.i & SIGN_MASK;
	tmp.i &= NOT_SIGN_MASK;
	upper.i = PORTABLE_INFINITY;
	if (isnan(datum) || (tmp.i == upper.i)) {
		(void)_OUTPUT_NAN_INF(datum,position_pointer,flags,field_width);
		goto DONE;
	};

/*
	convert denormals to normals and preserve the sign such that
	denormal = (10**-denorm_adjust)*normal.

	In the case of a 64-bit denormal as a result of a 32-bit input, we
	have a conversion error. We will try to correct it by adding the
	proper exponent and removing the implicit bit. All this assumes
	that the improper conversion moved the 23-bit mantissa of the 32-bit
	number to the top 23 mantissa bits of the 64-bit number. This is
	exactly what the Alpha chip does.
*/
	denorm_adjust = 0;
	if (((uint64)tmp.i >> MANTISSA_BITS) == 0 && tmp.i != 0) {
		if ((*mode & MODEHP) != 0) {
			tmp.i |= CVT32_64_MASK;
			upper.i = CVT32_64_MASK;
			delta = tmp.d - upper.d; /* remove implicit bit */
		}
		else {
			delta = (FLOAT64) tmp.i;
			while (delta < SAFE_DENORM) {
				denorm_adjust++;
				delta *= 10.0;
			}
			delta = scalb(delta,-1022);
			delta = scalb(delta,-MANTISSA_BITS);
		}
		tmp.d = datum; 
		upper.d = delta; 
		tmp.i &= SIGN_MASK;
		tmp.i |= upper.i;
		datum = tmp.d;
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
			datum = -datum;
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

/*
	scale the value such that (1.0 <= value < 10.0)
*/
	SCALEBYPOWEROFTEN(datum, datumh, datuml, power_of_ten, denorm_adjust)

/*
	round the value according to decimal_places and scale_factor
*/
	filler = decimal_places + scale_factor + power_of_ten;
	if (filler >= IEEE_64_DEC_OUT_DIGITS) filler = IEEE_64_DEC_OUT_DIGITS-1;
	if (filler >= -1) {
		ROUNDSCALEDNUM( datumh, datuml, scaleoverten-filler, upper, \
			tableh, tablel, scaleoverten, roundto10 )
	}

/*
	output the value
*/
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

		PUTDIGITLOOP64( datumh, datuml, position_pointer, \
			starting_integer_position+integer_digits, \
			tmp, upper, lower, digits_left )

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
	PUTDIGITLOOP64( datumh, datuml, position_pointer, \
		starting_fraction_position+decimal_places, \
		tmp, upper, lower, digits_left )

	goto DONE;

STARFILL:
	for ( ; field_width > 0; field_width--)
		*position_pointer++ = '*';
	goto DONE;

DONE:
	return(retval);
}
