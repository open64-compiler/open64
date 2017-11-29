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


static char USMID[] = "@(#) libu/numconv/mpp/s2uboiz.c	92.3	09/13/99 08:35:49";

#include <cray/fmtconv.h>
#include <cray/portdefs.h>

#if defined(_WORD32) || defined(__mips) || defined(_LITTLE_ENDIAN)
#define	CON(x)	x ## ULL
#else /* must be _CRAYMPP */
#define	CON(x)	x
#endif

#define	BLANK	' '
#define	ZERO	'0'
#define	STAR	'*'
#define	B_SIZE	1
#define	O_SIZE	3
#define	Z_SIZE	4
#define	B_MASK	1
#define	O_MASK	7
#define	Z_MASK	15
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
#define	WP_MASK	0XFFFF
#define	BP_MASK	0XFF
#endif
/*
	MAX_QUICK_INT:

	this value was gotten purely by checking the output of the routine
	against the correct output. It can certainly be extended some more
	if the test is run longer. The test was originally run on the Sun
	systems, so it would be nice to re-run on the MPP system when the
	performance is satisfactory(ie. we have a machine).
*/
#define	MAX_QUICK_INT	2147483647

/*
	General:
		Convert a binary value to an ASCII character field working from
		the right to the left.

	Inputs:
		value - value to output
		fca - first character address of character field
		mode - flags to determine how signs are handled
			0 = only '-' sign required
			1 = sign required
			2 = unsigned
		w - width of field
		m - minimum number of digits with zero fill if necessary

	Outputs:
		value - unchanged
		fca - unchanged
		mode - unchanged
		w - unchanged
		m - unchanged

		The memory in [fca,fca+w-1] is filled with the result of the
		conversion.

		The return value of the routine is a pointer to fca+w.

	Examples:

		For the Fortran edit descriptor "S,B30.5" we would have
		fw = 30, m = 5, mode = 0, and value equals the output datum. 

		For the Fortran edit descriptor "SP,B60" we would have
		fw = 60, m = 1, mode = 1, and value equals the output datum. 

		For the Fortran edit descriptor "SS,B55.0" we would have
		fw = 55, m = 0, mode = 0, and value equals the output datum. 

		The fca parameter depends on where you want to output the
		ASCII character data.

*/
long
*_s2ub(	const void	*value,
	long		*fca,
	const long	*mode,
	const long	*w,
	const long	*m,
	const long	*dummy1,
	const long	*dummy2)
{
	int64 datum;
	long *position_pointer;
	long field_width;
	long minimum_digits;
	long flags;
	long *return_val;
	char sign;

/*
	set up initial state
*/
	if ((*mode & MODEHP) != 0) {
		datum = (int64) *(int32 *)value;
	}
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
	else if ((*mode & MODEWP) != 0) {
		datum = (int64) *(int16 *)value;
	}
	else if ((*mode & MODEBP) != 0) {
		datum = (int64) *(int8 *)value;
	}
#endif
	else {
		datum = *(int64 *)value;
	}
	position_pointer = (long *)fca;
	flags = *mode;
	field_width = *w;
	minimum_digits = *m;
	sign = (long)BLANK;
	return_val = position_pointer + field_width;
	position_pointer = position_pointer + field_width - 1;

/*
	determine sign and pre-condition the value
*/
	if ((flags & MODEUN) == 0) {
		if (datum < 0) {
			datum = -datum;
			sign = '-';
		}
		else {
/*
 *	Provide plus sign for both zero and greater than zero.
 *	Do not use plus sign for (SP,w.m) if m is zero and the
 *	datum is zero.
*/
			if ((flags & MODESN) != 0) {
				if (datum != 0)
					sign = '+';
				else if (minimum_digits != 0)
					sign = '+';
			};
		};
	};
	if ((*mode & MODEHP) != 0) {
		datum = ((uint64)(datum << 32)) >> 32;
	}
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
	else if ((*mode & MODEWP) != 0) {
		datum = (uint64)(datum & WP_MASK);
	}
	else if ((*mode & MODEBP) != 0) {
		datum = (uint64)(datum & BP_MASK);
	}
#endif

/*
	fill field with value
*/
	while ((position_pointer >= fca) && (datum != 0)) {
		*position_pointer-- = (long)ZERO + (datum & B_MASK);
		datum = (uint64)datum >> B_SIZE;
		minimum_digits--;
	};

/*
	fill field with leading zeroes to satisfy minimum_digits requirement
*/
	while ((position_pointer >= fca) && (minimum_digits > 0)) {
		*position_pointer-- = (long)ZERO;
		minimum_digits--;
	};

/*
	fill field with sign and leading blanks
*/
	while (position_pointer >= fca) {
		*position_pointer-- = sign;
		sign = (long)BLANK;
	};

/*
	if field filling problems then star fill field
*/
	if ((datum != 0) || (minimum_digits > 0) || (sign != (long)BLANK)) {
		position_pointer++;
		while (field_width-- > 0) *position_pointer++ = STAR;
	};

	return(return_val);
}




/******************************************************************************/
/*
	General:
		Convert an octal value to an ASCII character field working from
		the right to the left.

	Inputs:
		value - value to output
		fca - first character address of character field
		mode - flags to determine how signs are handled
			0 = only '-' sign required
			1 = sign required
			2 = unsigned
		w - width of field
		m - minimum number of digits with zero fill if necessary

	Outputs:
		value - unchanged
		fca - unchanged
		mode - unchanged
		w - unchanged
		m - unchanged

		The memory in [fca,fca+w-1] is filled with the result of the
		conversion.

		The return value of the routine is a pointer to fca+w.

	Examples:

		For the Fortran edit descriptor "S,O30.5" we would have
		fw = 30, m = 5, mode = 0, and value equals the output datum. 

		For the Fortran edit descriptor "SP,O60" we would have
		fw = 60, m = 1, mode = 1, and value equals the output datum. 

		For the Fortran edit descriptor "SS,O55.0" we would have
		fw = 55, m = 0, mode = 0, and value equals the output datum. 

		The fca parameter depends on where you want to output the
		ASCII character data.

*/
long
*_s2uo(	const void	*value,
	long		*fca,
	const long	*mode,
	const long	*w,
	const long	*m,
	const long	*dummy1,
	const long	*dummy2)
{
	int64 datum;
	long *position_pointer;
	long field_width;
	long minimum_digits;
	long flags;
	long *return_val;
	char sign;

/*
	set up initial state
*/
	if ((*mode & MODEHP) != 0) {
		datum = (int64) *(int32 *)value;
	}
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
	else if ((*mode & MODEWP) != 0) {
		datum = (int64) *(int16 *)value;
	}
	else if ((*mode & MODEBP) != 0) {
		datum = (int64) *(int8 *)value;
	}
#endif
	else {
		datum = *(int64 *)value;
	}
	position_pointer = (long *)fca;
	flags = *mode;
	field_width = *w;
	minimum_digits = *m;
	sign = (long)BLANK;
	return_val = position_pointer + field_width;
	position_pointer = position_pointer + field_width - 1;

/*
	determine sign and precondition the value
*/
	if ((flags & MODEUN) == 0) {
		if (datum < 0) {
			datum = -datum;
			sign = '-';
		}
		else {
/*
 *	Provide plus sign for both zero and greater than zero.
 *	Do not use plus sign for (SP,w.m) if m is zero and the
 *	datum is zero.
*/
			if ((flags & MODESN) != 0) {
				if (datum != 0)
					sign = '+';
				else if (minimum_digits != 0)
					sign = '+';
			};
		};
	};
	if ((*mode & MODEHP) != 0) {
		datum = ((uint64)(datum << 32)) >> 32;
	}
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
	else if ((*mode & MODEWP) != 0) {
		datum = (uint64)(datum & WP_MASK);
	}
	else if ((*mode & MODEBP) != 0) {
		datum = (uint64)(datum & BP_MASK);
	}
#endif

/*
	fill field with value
*/
	while ((position_pointer >= fca) && (datum != 0)) {
		*position_pointer-- = (long)ZERO + (datum & O_MASK);
		datum = (uint64)datum >> O_SIZE;
		minimum_digits--;
	};

/*
	fill field with leading zeroes to satisfy minimum_digits requirement
*/
	while ((position_pointer >= fca) && (minimum_digits > 0)) {
		*position_pointer-- = (long)ZERO;
		minimum_digits--;
	};

/*
	fill field with sign and leading blanks
*/
	while (position_pointer >= fca) {
		*position_pointer-- = sign;
		sign = (long)BLANK;
	};

/*
	if field filling problems then star fill field
*/
	if ((datum != 0) || (minimum_digits > 0) || (sign != (long)BLANK)) {
		position_pointer++;
		while (field_width-- > 0) *position_pointer++ = STAR;
	};

	return(return_val);
}




/******************************************************************************/
/*
	General:
		Convert an integer value to an ASCII character field working 
		from the right to the left for cases less than MAX_QUICK_INT.
		For cases greater than MAX_QUICK_INT use a simple subtractive
		method in combination with a power of ten table.

	Inputs:
		value - value to output
		fca - first character address of character field
		mode - flags to determine how signs are handled
			0 = only '-' sign required
			1 = sign required
			2 = unsigned
		w - width of field
		m - minimum number of digits with zero fill if necessary

	Outputs:
		value - unchanged
		fca - unchanged
		mode - unchanged
		w - unchanged
		m - unchanged

		The memory in [fca,fca+w-1] is filled with the result of the
		conversion.

		The return value of the routine is a pointer to fca+w.

	Examples:

		For the Fortran edit descriptor "S,I30.5" we would have
		fw = 30, m = 5, mode = 0, and value equals the output datum. 

		For the Fortran edit descriptor "SP,I60" we would have
		fw = 60, m = 1, mode = 1, and value equals the output datum. 

		For the Fortran edit descriptor "SS,I55.0" we would have
		fw = 55, m = 0, mode = 0, and value equals the output datum. 

		The fca parameter depends on where you want to output the
		ASCII character data.

*/
long
*_s2ui(	const void	*value,
	long		*fca,
	const long	*mode,
	const long	*w,
	const long	*m,
	const long	*dummy1,
	const long	*dummy2)
{
	int64 datum;
	long *position_pointer;
	long field_width;
	long minimum_digits;
	long flags;
	long *return_val;
	char sign;

	long digit;
	long size;
	long length; 
	int64 pow10_multiple;
	int64 temporary[19];
	static uint64 pow10[] ={CON(1),			/* 10**0  */
				CON(10),		/* 10**1  */
				CON(100),		/* 10**2  */
				CON(1000),		/* 10**3  */
				CON(10000),		/* 10**4  */
				CON(100000),		/* 10**5  */
				CON(1000000),		/* 10**6  */
				CON(10000000),		/* 10**7  */
				CON(100000000),		/* 10**8  */
				CON(1000000000),	/* 10**9  */
				CON(10000000000),	/* 10**10 */
				CON(100000000000),	/* 10**11 */
				CON(1000000000000),	/* 10**12 */
				CON(10000000000000),	/* 10**13 */
				CON(100000000000000),	/* 10**14 */
				CON(1000000000000000),	/* 10**15 */
				CON(10000000000000000),	/* 10**16 */
				CON(100000000000000000),/* 10**17 */
				CON(1000000000000000000),/* 10**18 */
				CON(10000000000000000000)};/* 10**19 */

/*
	set up initial state
*/
	if ((*mode & MODEHP) != 0) {
		datum = (int64) *(int32 *)value;
	}
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
	else if ((*mode & MODEWP) != 0) {
		datum = (int64) *(int16 *)value;
	}
	else if ((*mode & MODEBP) != 0) {
		datum = (int64) *(int8 *)value;
	}
#endif
	else {
		datum = *(int64 *)value;
	}
	position_pointer = (long *)fca;
	flags = *mode;
	field_width = *w;
	minimum_digits = *m;
	sign = (long)BLANK;
	length = 18;
	return_val = position_pointer + field_width;
	position_pointer = position_pointer + field_width - 1;

/*
	determine sign and precondition the value
*/
	if ((flags & MODEUN) == 0) {
		if (datum < 0) {
			datum = -datum;
			sign = '-';
		}
		else {
/*
 *	Provide plus sign for both zero and greater than zero.
 *	Do not use plus sign for (SP,w.m) if m is zero and the
 *	datum is zero.
*/
			if ((flags & MODESN) != 0) {
				if (datum != 0)
					sign = '+';
				else if (minimum_digits != 0)
					sign = '+';
			};
		};
	};
	if ((*mode & MODEHP) != 0) {
		datum = ((uint64)(datum << 32)) >> 32;
	}
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
	else if ((*mode & MODEWP) != 0) {
		datum = (uint64)(datum & WP_MASK);
	}
	else if ((*mode & MODEBP) != 0) {
		datum = (uint64)(datum & BP_MASK);
	}
#endif

/*
	fill field with value
*/
	if ((datum >= MAX_QUICK_INT) || (datum < 0)) {
		if (((flags & MODEUN) != 0) && (datum < 0)) length = 19;
		for (; datum - (int64)pow10[length] < 0 ; length--);
		size = length;
		while (length > 0) {
			pow10_multiple = pow10[length] << 3; 
			digit = 0;
			if (datum - pow10_multiple >= 0) {
				datum -= pow10_multiple;
				digit = 8;
			}
			else {
				pow10_multiple = (uint64)pow10_multiple >> 1; 
				if (datum - pow10_multiple >= 0) {
					datum -= pow10_multiple;
					digit = 4;
				}
				pow10_multiple = (uint64)pow10_multiple >> 1; 
				if (datum - pow10_multiple >= 0) {
					datum -= pow10_multiple;
					digit += 2;
				}
			};
			if (datum - (int64)pow10[length] >= 0) {
				datum -= (int64)pow10[length];
				digit++;
			}
			temporary[length] = digit;
			length--;
		};
		temporary[length] = datum;
		datum = 0;
		while ((position_pointer >= fca) && (length <= size)) {
			*position_pointer-- = (long)ZERO + temporary[length];
			minimum_digits--;
			length++;
		};
		if (length <= size) datum = 1; /* field too small */
	}
	else {
		while ((position_pointer >= fca) && (datum != 0)) {
			digit = (double)datum * (double)0.1;
			*position_pointer-- = (long)ZERO + (datum - 10*digit);
			datum = digit;
			minimum_digits--;
		};
	};

/*
	fill field with leading zeroes to satisfy minimum_digits requirement
*/
	while ((position_pointer >= fca) && (minimum_digits > 0)) {
		*position_pointer-- = (long)ZERO;
		minimum_digits--;
	};

/*
	fill field with sign and leading blanks
*/
	while (position_pointer >= fca) {
		*position_pointer-- = sign;
		sign = (long)BLANK;
	};

/*
	if field filling problems then star fill field
*/
	if ((datum != 0) || (minimum_digits > 0) || (sign != (long)BLANK)) {
		position_pointer++;
		while (field_width-- > 0) *position_pointer++ = STAR;
	};

	return(return_val);
}




/******************************************************************************/
/*
	General:
		Convert a hexadecimal value to an ASCII character field working
		from the right to the left.

	Inputs:
		value - value to output
		fca - first character address of character field
		mode - flags to determine how signs are handled
			0 = only '-' sign required
			1 = sign required
			2 = unsigned
		w - width of field
		m - minimum number of digits with zero fill if necessary

	Outputs:
		value - unchanged
		fca - unchanged
		mode - unchanged
		w - unchanged
		m - unchanged

		The memory in [fca,fca+w-1] is filled with the result of the
		conversion.

		The return value of the routine is a pointer to fca+w.

	Examples:

		For the Fortran edit descriptor "S,Z30.5" we would have
		fw = 30, m = 5, mode = 0, and value equals the output datum. 

		For the Fortran edit descriptor "SP,Z60" we would have
		fw = 60, m = 1, mode = 1, and value equals the output datum. 

		For the Fortran edit descriptor "SS,Z55.0" we would have
		fw = 55, m = 0, mode = 0, and value equals the output datum. 

		The fca parameter depends on where you want to output the
		ASCII character data.

*/
long
*_s2uz(	const void	*value,
	long		*fca,
	const long	*mode,
	const long	*w,
	const long	*m,
	const long	*dummy1,
	const long	*dummy2)
{
	int64 datum;
	long *position_pointer;
	long field_width;
	long minimum_digits;
	long flags;
	long *return_val;
	char sign;

/*
	set up initial state
*/
	if ((*mode & MODEHP) != 0) {
		datum = (int64) *(int32 *)value;
	}
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
	else if ((*mode & MODEWP) != 0) {
		datum = (int64) *(int16 *)value;
	}
	else if ((*mode & MODEBP) != 0) {
		datum = (int64) *(int8 *)value;
	}
#endif
	else {
		datum = *(int64 *)value;
	}
	position_pointer = (long *)fca;
	flags = *mode;
	field_width = *w;
	minimum_digits = *m;
	sign = (long)BLANK;
	return_val = position_pointer + field_width;
	position_pointer = position_pointer + field_width - 1;

/*
	determine sign and precondition the value
*/
	if ((flags & MODEUN) == 0) {
		if (datum < 0) {
			datum = -datum;
			sign = '-';
		}
		else {
/*
 *	Provide plus sign for both zero and greater than zero.
 *	Do not use plus sign for (SP,w.m) if m is zero and the
 *	datum is zero.
*/
			if ((flags & MODESN) != 0) {
				if (datum != 0)
					sign = '+';
				else if (minimum_digits != 0)
					sign = '+';
			};
		};
	};
	if ((*mode & MODEHP) != 0) {
		datum = ((uint64)(datum << 32)) >> 32;
	}
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
	else if ((*mode & MODEWP) != 0) {
		datum = (uint64)(datum & WP_MASK);
	}
	else if ((*mode & MODEBP) != 0) {
		datum = (uint64)(datum & BP_MASK);
	}
#endif

/*
	fill field with value
*/
	while ((position_pointer >= fca) && (datum != 0)) {
		*position_pointer-- = (long)ZERO + ((datum & Z_MASK) > 9 ?
				      ((datum & Z_MASK) + 7) : (datum & Z_MASK));
		datum = (uint64)datum >> Z_SIZE;
		minimum_digits--;
	};

/*
	fill field with leading zeroes to satisfy minimum_digits requirement
*/
	while ((position_pointer >= fca) && (minimum_digits > 0)) {
		*position_pointer-- = (long)ZERO;
		minimum_digits--;
	};

/*
	fill field with sign and leading blanks
*/
	while (position_pointer >= fca) {
		*position_pointer-- = sign;
		sign = (long)BLANK;
	};

/*
	if field filling problems then star fill field
*/
	if ((datum != 0) || (minimum_digits > 0) || (sign != (long)BLANK)) {
		position_pointer++;
		while (field_width-- > 0) *position_pointer++ = STAR;
	};

	return(return_val);
}
