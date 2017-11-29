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


#pragma ident "@(#) libu/numconv/mpp/boizu2s.c	92.2	08/02/99 16:30:41"

#include <cray/fmtconv.h>
#include <cray/portdefs.h>

#define	BLANK	' '
#define	POSITIVE	0
#define	NEGATIVE	-1

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
#define WP_NOT_MASK 0XFFFFFFFFFFFF0000LL
#define BP_NOT_MASK 0XFFFFFFFFFFFFFF00LL
#endif

/*
	General:
		Scan from left to right and convert the ASCII character field
		to a binary value.

	Inputs:
		fca - first character address of character field
		fw - width of character field
		lcap1 - last character plus one
		mode - flags to determine how blanks are handled
		result - ignored on input
		status - ignored on input

	Outputs:
		fca - unchanged
		fw - unchanged
		lcap1 - last character looked at in the conversion plus one
		mode - unchanged
		result - result of conversion
		status - exit status(one of the following)
			 7 =  8-bit integer -- mips only
			 6 = 16-bit integer -- mips only
			 2 = 64-bit integer
			 1 = 32-bit integer
			-1 = illegal character input
			-2 = integer overflow
			-5 = null field

	Examples:

		For the Fortran edit descriptor "BZ,B30" we would have
		fw = 30 and mode = 2.

		For the Fortran edit descriptor "BN,B10.2" we would have
		fw = 10 and mode = 1.

		The other input parameters fca and lcap1 depend on where the
		character string resides in memory. lcap1 is used as a
		boundary for valid input data(anything at or beyond lcap1 is
		assumed to be blanks and will be treated according to the way
		blanks are to be treated, this becomes significant if the
		fca and fw combination goes beyond the lcap1 boundary).

*/
int
_bu2s(	const long *fca,
	const long *fw,
	long **lcap1,
	const long *mode,
	void *result,
	long *status,
	const long *digits,	/* UNUSED */
	const long *scale)	/* UNUSED */
{
	int64 ovfl;
	int64 pre_result;
	long blank;
	long *position_pointer;
	long *l_char_address_p1;
	long field_width;
	long flags;
	long sign;
	long extra_zeroes;

/*
	set-up initial state
*/
	position_pointer = (long *)fca;
	field_width = *fw;
	flags = *mode;
	l_char_address_p1 = *lcap1;
	pre_result = 0;
	ovfl = 0;
	sign = POSITIVE;
	blank = BLANK;
	extra_zeroes = 0;

/*
	check for valid field width and special blank handling
*/
	if ((flags & (MODEBZ | MODEBN)) == 0) blank = 511;
	if (field_width == 0) {
		*status = EX_INTL;
		goto DONE;
	}
	if (field_width < 0) {
		*status = EX_NULLFLD;
		goto DONE;
	}

/*
	determine if we have extra blanks or zeroes appended to the input
	string and reset the field width if needed.
*/
	if (position_pointer+field_width <= l_char_address_p1) {
		l_char_address_p1 = position_pointer+field_width;
	}
	else {
		if ((flags & MODEBZ) != 0) extra_zeroes = position_pointer +
							  field_width -
							  l_char_address_p1;
		field_width = l_char_address_p1-position_pointer;
		if (field_width <= 0) {
			*status = EX_INTL;
			goto DONE;
		}
	}

/*
	skip leading blanks - unless blanks are delimiters
*/
	while ((*position_pointer == blank) &&
	       (position_pointer < l_char_address_p1)) position_pointer++;
	if (position_pointer == l_char_address_p1) goto SETTYPE;

/*
	check for sign
*/
	if ((*position_pointer == '-') || (*position_pointer == '+')) {
		if (*position_pointer == '-') sign = NEGATIVE;
		position_pointer++;
		if (position_pointer == l_char_address_p1) {
			if (extra_zeroes != 0) { 
				*status = EX_INTL;
			}
			else {
				*status = EX_NULLFLD;
			}
			goto DONE;
		}
	}

/*
	loop through characters and build the mantissa
*/

	while (position_pointer < l_char_address_p1) {
		if ((*position_pointer == '0') || (*position_pointer == '1')) {
			ovfl = pre_result >> 63;
			pre_result = (pre_result << 1) +
				     (*position_pointer - '0');
		}
		else {
#ifdef KEY /* Bug 8105 */
			/* VMS extension allows numeric field
			 * to end prematurely with "," */
			if (',' == (*position_pointer)) {
				*status = EX_ILLCHAR;
				goto DONE;
			}
#endif /* KEY Bug 8105 */
			if ((*position_pointer) != BLANK) {
				pre_result = 0;
				*status = EX_ILLCHAR;
				goto DONE;
			} 
			if ((flags & MODEBZ) != 0) {
				ovfl = pre_result >> 63;
				pre_result = pre_result << 1; 
			}
			else {
				if ((flags & MODEBN) == 0) goto SETTYPE;
			}
		}
		if (ovfl != 0 ) {
			pre_result = 0;
			*status = EX_FIXOFLO;
			goto DONE;
		}
		position_pointer++;
	}

SETTYPE:
	if (extra_zeroes != 0) {
		while (extra_zeroes-- > 0) {
			ovfl = pre_result >> 63;
			pre_result = pre_result << 1; 
			if (ovfl != 0 ) {
				pre_result = 0;
				*status = EX_FIXOFLO;
				goto DONE;
			}
		}
	}
/*
	if returning other precisions, check for overflow before applying
	the sign.
*/
	if ((flags & MODEHP) != 0) {
		if ((uint64)pre_result >> 32 != 0) {
			pre_result = 0;
			*status = EX_FIXOFLO;
			goto DONE;
		}
	}
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
	else if ((flags & MODEWP) != 0) {
		if (((uint64)pre_result & WP_NOT_MASK) != 0) {
			pre_result = 0;
			*status = EX_FIXOFLO;
			goto DONE;
		}
	}
	else if ((flags & MODEBP) != 0) {
		if (((uint64)pre_result & BP_NOT_MASK) != 0) {
			pre_result = 0;
			*status = EX_FIXOFLO;
			goto DONE;
		}
	}
#endif
	if (sign == NEGATIVE) pre_result = -pre_result;
	*status = EX_INTL;

DONE:
	if ((flags & MODEHP) != 0) {
		if (*status == EX_INTL) *status = EX_INTS;
		*(int32 *)result = pre_result;
	}
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
	else if ((flags & MODEWP) != 0) {
		if (*status == EX_INTL) *status = EX_INT16;
		*(int16 *)result = pre_result;
	}
	else if ((flags & MODEBP) != 0) {
		if (*status == EX_INTL) *status = EX_INT8;
		*(int8 *)result = pre_result;
	}
#endif
	else
		*(int64 *)result = pre_result;
	*lcap1 = position_pointer;
	return(*status);
}




/******************************************************************************/
/*
	General:
		Scan from left to right and convert the ASCII character field
		to an octal value.

	Inputs:
		fca - first character address of character field
		fw - width of character field
		lcap1 - last character plus one
		mode - flags to determine how blanks are handled
		result - ignored on input
		status - ignored on input

	Outputs:
		fca - unchanged
		fw - unchanged
		lcap1 - last character looked at in the conversion plus one
		mode - unchanged
		result - result of conversion
		status - exit status(one of the following)
			 7 =  8-bit integer
			 6 = 16-bit integer
			 2 = 64-bit integer
			 1 = 32-bit integer
			-1 = illegal character input
			-2 = integer overflow
			-5 = null field

	Examples:

		For the Fortran edit descriptor "BZ,O30" we would have
		fw = 30 and mode = 2.

		For the Fortran edit descriptor "BN,O10.2" we would have
		fw = 10 and mode = 1.

		The other input parameters fca and lcap1 depend on where the
		character string resides in memory. lcap1 is used as a
		boundary for valid input data(anything at or beyond lcap1 is
		assumed to be blanks and will be treated according to the way
		blanks are to be treated, this becomes significant if the
		fca and fw combination goes beyond the lcap1 boundary).

*/
int
_ou2s(	const long *fca,
	const long *fw,
	long **lcap1,
	const long *mode,
	void *result,
	long *status,
	const long *digits,	/* UNUSED */
	const long *scale)	/* UNUSED */
{
	int64 ovfl;
	int64 pre_result;
	long blank;
	long *position_pointer;
	long *l_char_address_p1;
	long field_width;
	long flags;
	long sign;
	long extra_zeroes;

/*
	set-up initial state
*/
	position_pointer = (long *)fca;
	field_width = *fw;
	flags = *mode;
	l_char_address_p1 = *lcap1;
	pre_result = 0;
	ovfl = 0;
	sign = POSITIVE;
	blank = BLANK;
	extra_zeroes = 0;

/*
	check for valid field width and special blank handling
*/
	if ((flags & (MODEBZ | MODEBN)) == 0) blank = 511;
	if (field_width == 0) {
		*status = EX_INTL;
		goto DONE;
	}
	if (field_width < 0) {
		*status = EX_NULLFLD;
		goto DONE;
	}

/*
	determine if we have extra blanks or zeroes appended to the input 
	string and reset the field width if needed.
*/
	if (position_pointer+field_width <= l_char_address_p1) {
		l_char_address_p1 = position_pointer+field_width;
	}
	else {
		if ((flags & MODEBZ) != 0) extra_zeroes = position_pointer +
							  field_width -
							  l_char_address_p1;
		field_width = l_char_address_p1-position_pointer;
		if (field_width <= 0) {
			*status = EX_INTL;
			goto DONE;
		}
	}

/*
	skip leading blanks - unless blanks are delimiters
*/
	while ((*position_pointer == blank) &&
	       (position_pointer < l_char_address_p1)) position_pointer++;
	if (position_pointer == l_char_address_p1) goto SETTYPE;

/*
	check for sign
*/
	if ((*position_pointer == '-') || (*position_pointer == '+')) {
		if (*position_pointer == '-') sign = NEGATIVE;
		position_pointer++;
		if (position_pointer == l_char_address_p1) {
			if (extra_zeroes != 0) { 
				*status = EX_INTL;
			}
			else {
				*status = EX_NULLFLD;
			}
			goto DONE;
		}
	}

/*
	loop through characters and build the mantissa
*/

	while (position_pointer < l_char_address_p1) {
		if ((*position_pointer >= '0') && (*position_pointer <= '7')) {
			ovfl = pre_result >> 61;
			pre_result = (pre_result << 3) +
				     (*position_pointer - '0');
		}
		else {
#ifdef KEY /* Bug 8105 */
			/* VMS extension allows numeric field
			 * to end prematurely with "," */
			if (',' == (*position_pointer)) {
				*status = EX_ILLCHAR;
				goto DONE;
			}
#endif /* KEY Bug 8105 */
			if ((*position_pointer) != BLANK) {
				pre_result = 0;
				*status = EX_ILLCHAR;
				goto DONE;
			} 
			if ((flags & MODEBZ) != 0) {
				ovfl = pre_result >> 61;
				pre_result = pre_result << 3; 
			}
			else {
				if ((flags & MODEBN) == 0) goto SETTYPE;
			}
		}
		if (ovfl != 0 ) {
			pre_result = 0;
			*status = EX_FIXOFLO;
			goto DONE;
		}
		position_pointer++;
	}

SETTYPE:
	if (extra_zeroes != 0) {
		while (extra_zeroes-- > 0) {
			ovfl = pre_result >> 61;
			pre_result = pre_result << 3; 
			if (ovfl != 0 ) {
				pre_result = 0;
				*status = EX_FIXOFLO;
				goto DONE;
			}
		}
	}
/*
	if returning other precisions, check for overflow before applying
	the sign.
*/
	if ((flags & MODEHP) != 0) {
		if ((uint64)pre_result >> 32 != 0) {
			pre_result = 0;
			*status = EX_FIXOFLO;
			goto DONE;
		}
	}
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
	else if ((flags & MODEWP) != 0) {
		if (((uint64)pre_result & WP_NOT_MASK) != 0) {
			pre_result = 0;
			*status = EX_FIXOFLO;
			goto DONE;
		}
	}
	else if ((flags & MODEBP) != 0) {
		if (((uint64)pre_result & BP_NOT_MASK) != 0) {
			pre_result = 0;
			*status = EX_FIXOFLO;
			goto DONE;
		}
	}
#endif
	if (sign == NEGATIVE) pre_result = -pre_result;
	*status = EX_INTL;

DONE:
	if ((flags & MODEHP) != 0) {
		if (*status == EX_INTL) *status = EX_INTS;
		*(int32 *)result = pre_result;
	}
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
	else if ((flags & MODEWP) != 0) {
		if (*status == EX_INTL) *status = EX_INT16;
		*(int16 *)result = pre_result;
	}
	else if ((flags & MODEBP) != 0) {
		if (*status == EX_INTL) *status = EX_INT8;
		*(int8 *)result = pre_result;
	}
#endif
	else
        	*(int64 *)result = pre_result;
	*lcap1 = position_pointer;
	return(*status);
}


#ifdef KEY /* Bug 8767 */
/* Return nonzero if pre_result indicates an n-bit overflow, but allow the
 * largest-magnitude n-bit 2's complement negative number as a special case
 * (e.g. 16-bit -32768 should not cause overflow.) */
#define TEST_OVFL(neg_sign, pre_result, n) \
  ((neg_sign) && ((uint64)(pre_result)) == (((uint64) 1) << ((n) - 1))) ? \
  0 : \
  (((uint64)(pre_result)) >> ((n) - 1))
#endif /* KEY Bug 8767 */


/******************************************************************************/
/*
	General:
		Scan from left to right and convert the ASCII character field
		to an integer value.

	Inputs:
		fca - first character address of character field
		fw - width of character field
		lcap1 - last character plus one
		mode - flags to determine how blanks are handled
		result - ignored on input
		status - ignored on input

	Outputs:
		fca - unchanged
		fw - unchanged
		lcap1 - last character looked at in the conversion plus one
		mode - unchanged
		result - result of conversion
		status - exit status(one of the following)
			 7 =  8-bit integer
			 6 = 16-bit integer
			 2 = 64-bit integer
			 1 = 32-bit integer
			-1 = illegal character input
			-2 = integer overflow
			-5 = null field

	Examples:

		For the Fortran edit descriptor "BZ,I30" we would have
		fw = 30 and mode = 2.

		For the Fortran edit descriptor "BN,I10.2" we would have
		fw = 10 and mode = 1.

		The other input parameters fca and lcap1 depend on where the
		character string resides in memory. lcap1 is used as a
		boundary for valid input data(anything at or beyond lcap1 is
		assumed to be blanks and will be treated according to the way
		blanks are to be treated, this becomes significant if the
		fca and fw combination goes beyond the lcap1 boundary).

*/
int
_iu2s(	const long *fca,
	const long *fw,
	long **lcap1,
	const long *mode,
	void *result,
	long *status,
	const long *digits,	/* UNUSED */
	const long *scale)	/* UNUSED */
{
	int64 ovfl;
	int64 pre_result;
	long blank;
	long *position_pointer;
	long *l_char_address_p1;
	long field_width;
	long flags;
	long sign;
	long extra_zeroes;

/*
	set-up initial state
*/
	position_pointer = (long *)fca;
	field_width = *fw;
	flags = *mode;
	l_char_address_p1 = *lcap1;
	pre_result = 0;
	ovfl = 0;
	sign = POSITIVE;
	blank = BLANK;
	extra_zeroes = 0;

/*
	check for valid field width and special blank handling
*/
	if ((flags & (MODEBZ | MODEBN)) == 0) blank = 511;
	if (field_width == 0) {
		*status = EX_INTL;
		goto DONE;
	}
	if (field_width < 0) {
		*status = EX_NULLFLD;
		goto DONE;
	}

/*
	determine if we have extra blanks or zeroes appended to the input
	string and reset the field width if needed.
*/
	if (position_pointer+field_width <= l_char_address_p1) {
		l_char_address_p1 = position_pointer+field_width;
	}
	else {
		if ((flags & MODEBZ) != 0) extra_zeroes = position_pointer +
					                  field_width - 
							  l_char_address_p1;
		field_width = l_char_address_p1-position_pointer;
		if (field_width <= 0) {
			*status = EX_INTL;
			goto DONE;
		}
	}

/*
	skip leading blanks - unless blanks are delimiters
*/
	while ((*position_pointer == blank) &&
	       (position_pointer < l_char_address_p1)) position_pointer++;
	if (position_pointer == l_char_address_p1) goto SETTYPE;

/*
	check for sign
*/
	if ((*position_pointer == '-') || (*position_pointer == '+')) {
		if (*position_pointer == '-') sign = NEGATIVE;
		position_pointer++;
		if (position_pointer == l_char_address_p1) {
			if (extra_zeroes != 0) { 
				*status = EX_INTL;
			}
			else {
				*status = EX_NULLFLD;
			}
			goto DONE;
		}
	}
#ifdef KEY /* Bug 8767 */
        int neg_sign = (sign == NEGATIVE);
#endif /* KEY Bug 8767 */

/*
	loop through characters and build the mantissa
*/

	while (position_pointer < l_char_address_p1) {
		if ((*position_pointer >= '0') && (*position_pointer <= '9')) {
			ovfl = pre_result >> 60;
			pre_result = (pre_result * 10) +
				     (*position_pointer - '0');
#ifdef KEY /* Bug 8767 */
			ovfl += TEST_OVFL(neg_sign, pre_result, 64);
#else /* KEY Bug 8767 */
			ovfl += (pre_result >> 63);
#endif /* KEY Bug 8767 */
		}
		else {
#ifdef KEY /* Bug 8105 */
			/* VMS extension allows numeric field
			 * to end prematurely with "," */
			if (',' == *position_pointer) {
				*status = EX_ILLCHAR;
				goto DONE;
			}
#endif /* KEY Bug 8105 */
			if ((*position_pointer) != BLANK) {
				pre_result = 0;
				*status = EX_ILLCHAR;
				goto DONE;
			} 
			if ((flags & MODEBZ) != 0) {
				ovfl = pre_result >> 60;
				pre_result = pre_result * 10; 
#ifdef KEY /* Bug 8767 */
				ovfl = TEST_OVFL(neg_sign, pre_result, 64);
#else /* KEY Bug 8767 */
				ovfl += (pre_result >> 63);
#endif /* KEY Bug 8767 */
			}
			else {
				if ((flags & MODEBN) == 0) goto SETTYPE;
			}
		}
		if (ovfl != 0 ) {
			pre_result = 0;
			*status = EX_FIXOFLO;
			goto DONE;
		}
		position_pointer++;
	}

SETTYPE:
	if (extra_zeroes != 0) {
		while (extra_zeroes-- > 0) {
			ovfl = pre_result >> 60;
			pre_result = pre_result * 10; 
			ovfl += (pre_result >> 63);
			if (ovfl != 0 ) {
				pre_result = 0;
				*status = EX_FIXOFLO;
				goto DONE;
			}
		}
	}
/*
	if returning other precisions, check for overflow before applying
	the sign.
*/
	if ((flags & MODEHP) != 0) {
#ifdef KEY /* Bug 8767 */
                if (TEST_OVFL(neg_sign, pre_result, 32))
#else /* KEY Bug 8767 */
		if ((uint64)pre_result >> 31 != 0)
#endif /* KEY Bug 8767 */
		{
			pre_result = 0;
			*status = EX_FIXOFLO;
			goto DONE;
		}
	}
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
	else if ((flags & MODEWP) != 0) {
#ifdef KEY /* Bug 8767 */
                if (TEST_OVFL(neg_sign, pre_result, 16))
#else /* KEY Bug 8767 */
		if (((uint64)pre_result & WP_NOT_MASK) != 0)
#endif /* KEY Bug 8767 */
		{
			pre_result = 0;
			*status = EX_FIXOFLO;
			goto DONE;
		}
	}
	else if ((flags & MODEBP) != 0) {
#ifdef KEY /* Bug 8767 */
                if (TEST_OVFL(neg_sign, pre_result, 8))
#else /* KEY Bug 8767 */
		if (((uint64)pre_result & BP_NOT_MASK) != 0)
#endif /* KEY Bug 8767 */
		{
			pre_result = 0;
			*status = EX_FIXOFLO;
			goto DONE;
		}
	}
#endif
	if (sign == NEGATIVE) pre_result = -pre_result;
	*status = EX_INTL;

DONE:
	if ((flags & MODEHP) != 0) {
		if (*status == EX_INTL) *status = EX_INTS;
		*(int32 *)result = pre_result;
	}
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
	else if ((flags & MODEWP) != 0) {
		if (*status == EX_INTL) *status = EX_INT16;
		*(int16 *)result = pre_result;
	}
	else if ((flags & MODEBP) != 0) {
		if (*status == EX_INTL) *status = EX_INT8;
		*(int8 *)result = pre_result;
	}
#endif
	else
		*(int64 *)result = pre_result;
	*lcap1 = position_pointer;
	return(*status);
}



/******************************************************************************/
/*
	General:
		Scan from left to right and convert the ASCII character field
		to a hexadecimal value.

	Inputs:
		fca - first character address of character field
		fw - width of character field
		lcap1 - last character plus one
		mode - flags to determine how blanks are handled
		result - ignored on input
		status - ignored on input

	Outputs:
		fca - unchanged
		fw - unchanged
		lcap1 - last character looked at in the conversion plus one
		mode - unchanged
		result - result of conversion
		status - exit status(one of the following)
			 7 =  8-bit integer
			 6 = 16-bit integer
			 2 = 64-bit integer
			 1 = 32-bit integer
			-1 = illegal character input
			-2 = integer overflow
			-5 = null field

	Examples:

		For the Fortran edit descriptor "BZ,Z30" we would have
		fw = 30 and mode = 2.

		For the Fortran edit descriptor "BN,Z10.2" we would have
		fw = 10 and mode = 1.

		The other input parameters fca and lcap1 depend on where the
		character string resides in memory. lcap1 is used as a
		boundary for valid input data(anything at or beyond lcap1 is
		assumed to be blanks and will be treated according to the way
		blanks are to be treated, this becomes significant if the
		fca and fw combination goes beyond the lcap1 boundary).

*/
int
_zu2s(	const long *fca,
	const long *fw,
	long **lcap1,
	const long *mode,
	void *result,
	long *status,
	const long *digits,	/* UNUSED */
	const long *scale)	/* UNUSED */
{
	int64 ovfl;
	int64 pre_result;
	long blank;
	long *position_pointer;
	long *l_char_address_p1;
	long field_width;
	long flags;
	long sign;
	long extra_zeroes;
	long displacement;
	long alpha;

/*
	set-up initial state
*/
	position_pointer = (long *)fca;
	field_width = *fw;
	flags = *mode;
	l_char_address_p1 = *lcap1;
	pre_result = 0;
	ovfl = 0;
	sign = POSITIVE;
	blank = BLANK;
	extra_zeroes = 0;

/*
	check for valid field width and special blank handling
*/
	if ((flags & (MODEBZ | MODEBN)) == 0) blank = 511;
	if (field_width == 0) {
		*status = EX_INTL;
		goto DONE;
	}
	if (field_width < 0) {
		*status = EX_NULLFLD;
		goto DONE;
	}

/*
	determine if we have extra blanks or zeroes appended to the input
	string and reset the field width if needed.
*/
	if (position_pointer+field_width <= l_char_address_p1) {
		l_char_address_p1 = position_pointer+field_width;
	}
	else {
		if ((flags & MODEBZ) != 0) extra_zeroes = position_pointer +
							  field_width - 
							  l_char_address_p1;
		field_width = l_char_address_p1-position_pointer;
		if (field_width <= 0) {
			*status = EX_INTL;
			goto DONE;
		}
	}

/*
	skip leading blanks - unless blanks are delimiters
*/
	while ((*position_pointer == blank) &&
	       (position_pointer < l_char_address_p1)) position_pointer++;
	if (position_pointer == l_char_address_p1) goto SETTYPE;

/*
	check for sign
*/
	if ((*position_pointer == '-') || (*position_pointer == '+')) {
		if (*position_pointer == '-') sign = NEGATIVE;
		position_pointer++;
		if (position_pointer == l_char_address_p1) {
			if (extra_zeroes != 0) { 
				*status = EX_INTL;
			}
			else {
				*status = EX_NULLFLD;
			}
			goto DONE;
		}
	}

/*
	loop through characters and build the mantissa
*/

	while (position_pointer < l_char_address_p1) {
		displacement = 0;
		alpha = NEGATIVE;
		if ((*position_pointer >= 'a') && (*position_pointer <= 'f')) {
			displacement = 'a' - '0' - 10;
			alpha = POSITIVE;
		}
		if ((*position_pointer >= 'A') && (*position_pointer <= 'F')) {
			displacement = 'A' - '0' - 10;
			alpha = POSITIVE;
		}
		if (((*position_pointer >= '0') && (*position_pointer <= '9'))
		    || (alpha == POSITIVE)) {
			ovfl = pre_result >> 60;
			pre_result = (pre_result << 4) +
				     (*position_pointer - '0' - displacement);
		}
		else {
#ifdef KEY /* Bug 8105 */
			/* VMS extension allows numeric field
			 * to end prematurely with "," */
			if (',' == (*position_pointer)) {
				*status = EX_ILLCHAR;
				goto DONE;
			}
#endif /* KEY Bug 8105 */
			if ((*position_pointer) != BLANK) {
				pre_result = 0;
				*status = EX_ILLCHAR;
				goto DONE;
			} 
			if ((flags & MODEBZ) != 0) {
				ovfl = pre_result >> 60;
				pre_result = pre_result << 4; 
			}
			else {
				if ((flags & MODEBN) == 0) goto SETTYPE;
			}
		}
		if (ovfl != 0 ) {
			pre_result = 0;
			*status = EX_FIXOFLO;
			goto DONE;
		}
		position_pointer++;
	}

SETTYPE:
	if (extra_zeroes != 0) {
		while (extra_zeroes-- > 0) {
			ovfl = pre_result >> 60;
			pre_result = pre_result << 4; 
			if (ovfl != 0 ) {
				pre_result = 0;
				*status = EX_FIXOFLO;
				goto DONE;
			}
		}
	}
/*
	if returning other precisions, check for overflow before applying
	the sign.
*/
	if ((flags & MODEHP) != 0) {
		if ((uint64)pre_result >> 32 != 0) {
			pre_result = 0;
			*status = EX_FIXOFLO;
			goto DONE;
		}
	}
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
	else if ((flags & MODEWP) != 0) {
		if (((uint64)pre_result & WP_NOT_MASK) != 0) {
			pre_result = 0;
			*status = EX_FIXOFLO;
			goto DONE;
		}
	}
	else if ((flags & MODEBP) != 0) {
		if (((uint64)pre_result & BP_NOT_MASK) != 0) {
			pre_result = 0;
			*status = EX_FIXOFLO;
			goto DONE;
		}
	}
#endif
	if (sign == NEGATIVE) pre_result = -pre_result;
	*status = EX_INTL;

DONE:
	if ((flags & MODEHP) != 0) {
		if (*status == EX_INTL) *status = EX_INTS;
		*(int32 *)result = pre_result;
	}
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
	else if ((flags & MODEWP) != 0) {
		if (*status == EX_INTL) *status = EX_INT16;
		*(int16 *)result = pre_result;
	}
	else if ((flags & MODEBP) != 0) {
		if (*status == EX_INTL) *status = EX_INT8;
		*(int8 *)result = pre_result;
	}
#endif
	else
		*(int64 *)result = pre_result;
	*lcap1 = position_pointer;
	return(*status);
}
