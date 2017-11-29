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


/* USMID @(#) libf/include/lio.h	92.0	10/08/98 14:30:10 */
  
/*
 * 	Constants.
 */

#define BLANK	((int) ' ')
#define COMMA	((int) ',')
#define DQUOTE	((int) '"')
#define LPAREN	((int) '(')
#define	MINUS	((int) '-')
#define	PERIOD	((int) '.')
#define	PLUS	((int) '+')
#define RPAREN	((int) ')')
#define SLASH	((int) '/')
#define SQUOTE	((int) '\'')
#define STAR	((int) '*')
#define ZERO	((int) '0')

/*
 *	Define some sets of characters and macros to set them.  This
 *	is done to provide quick, in-line versions of isdigit(),
 *	isspace(), etc.  It works for any characters whose ordinal
 *	values are less than the word size (64).  That is, digits,
 *	whitespace, and most separators.
 */

#ifdef	_CRAY
#define	BIT1	1L
#else
#define	BIT1	1LL
#endif

#define	DIGITS	((BIT1 << (int) '0') | \
		 (BIT1 << (int) '1') | \
		 (BIT1 << (int) '2') | \
		 (BIT1 << (int) '3') | \
		 (BIT1 << (int) '4') | \
		 (BIT1 << (int) '5') | \
		 (BIT1 << (int) '6') | \
		 (BIT1 << (int) '7') | \
		 (BIT1 << (int) '8') | \
		 (BIT1 << (int) '9') )

#define	SPACES	((BIT1 << (int) ' ' ) | \
		 (BIT1 << (int) '\t') | \
		 (BIT1 << (int) '\r') | \
		 (BIT1 << (int) '\n') | \
		 (BIT1 << (int) '\v') | \
		 (BIT1 << (int) '\f') )

#define	SEPS	( SPACES | \
		 (BIT1 << SLASH) | \
		 (BIT1 << COMMA) )

#define	DELIMS	( SEPS | \
		 (BIT1 << RPAREN) )

#define	STRDLM	((BIT1 << SQUOTE) | \
		 (BIT1 << DQUOTE) )
/*
 *	OUT_OF_RANGE returns nonzero iff an item (character) is too
 *	large for a word set operation (> 64).  Note that the CRAY
 *	PVP hardward always yields zero for shifts which exceed the
 *	word size, so this function can be a no-op.
 */

#ifdef	_CRAY1
#define	OUT_OF_RANGE(item)	0
#else
#define	OUT_OF_RANGE(item)	((item) & ~077)
#endif

/*
 *	IN_SET returns nonzero iff an item (character) is in a set of
 *	items (characters).  Note that the >> operator is undefined
 *	for counts larger than the size of the type--the CRAY PVP
 *	version of IN_SET takes advantage of the CRAY PVP hardware
 *	always yielding zero for shifts which exceed the word size.
 */

#define IN_SET(set, item) \
	(OUT_OF_RANGE(item) ? 0 : (((set) >> (item)) & 1))

/*
 *	Now define some character set operations for commonly used
 *	(list-directed I/O, namelist I/O, formatted I/O, etc) set
 *	tests.
 *
 *	IS_WHITESPACE	Is character whitespace?
 *	IS_DIGIT	Is character a digit?
 *	IS_SEPARATOR	Is character a separator (whitespace, slash, or comma)?
 *	IS_DELIMITER	Is character a delimiter (separator or rparen)?
 *	IS_STRING_DELIMITER	Is character a string delimiter (single
 *				or double quote)?
 */

#define	IS_WHITESPACE(x)	IN_SET(SPACES, (int) (x))
#define	IS_DIGIT(x)		IN_SET(DIGITS, (int) (x))
#define IS_SEPARATOR(x)		IN_SET(  SEPS, (int) (x))
#define IS_DELIMITER(x)		IN_SET(DELIMS, (int) (x))
#define IS_STRING_DELIMITER(x)	IN_SET(STRDLM, (int) (x))

/*
 * 	Field width constants for output conversions.
 */

#define WOCTWRD	22
#define WOCTHWD	11
#define WINT	21	/* field width large enough for -2**64 */

/*
 *	The following are the number of decimal to print for list directed
 *	output of real values.
 *
 *	The *_P_* macros represent the value of the PRECISION intrinsic
 *	function for variables of the specified type.
 *
 *	By default, list directed output of real values prints out all the 
 *	significant digits of precision available.   The number of decimal
 *	digits needed is 
 *
 *		2 + INT(p * log10(2))
 *
 *	where p is the number of bits in the mantissa.  Remember to count the
 *	1 implicit mantissa bit in IEEE formats.  (From Steele and White,
 *	"Proceedings of the ACM SIGPLAN'90 Conference on Programming Language 
 *	Design and Implementation", White Plains, New York, June 20-22, 1990)
 */

#define DREAL4_IEEE	 9	/*  4-byte IEEE REAL */
#define DREAL8_IEEE	17	/*  8-byte IEEE REAL */
#define DREAL16_IEEE   	36	/* 16-byte IEEE REAL */
#define DREAL16_DD   	34	/* 16-byte IEEE double double REAL */
#define DREAL4_P_IEEE	 6	/*  4-byte IEEE REAL partial precision */
#define DREAL8_P_IEEE	15	/*  8-byte IEEE REAL partial precision */
#define DREAL16_P_IEEE	33	/* 16-byte IEEE REAL partial precision */
#define DREAL16_P_DD	31	/* 16-byte double double partial precision */

#define DREAL8_CRI	16	/* 8-byte CRAY REAL */
#define DREAL16_CRI	30	/* 16-byte CRAY REAL */
#define DREAL8_P_CRI	14	/* 8-byte CRAY REAL partial precision */
#define DREAL16_P_CRI	28	/* 16-byte CRAY REAL partial precision */

#ifdef	IEEE_FLOATING_POINT	
#define DREAL4		DREAL4_IEEE
#define	DEXP4		2
#define DREAL8		DREAL8_IEEE
#define	DEXP8		3
#define DREAL4_P	DREAL4_P_IEEE
#define DREAL8_P	DREAL8_P_IEEE
#ifdef	__mips
#define DREAL16		DREAL16_DD
#define DREAL16_P	DREAL16_P_DD
#define	DEXP16		3
#else
#define DREAL16		DREAL16_IEEE
#define DREAL16_P	DREAL16_P_IEEE
#define	DEXP16		4
#endif
#else	/* CRAY floating-point */
#define DREAL8		DREAL8_CRI
#define DREAL16		DREAL16_CRI
#define DREAL8_P	DREAL8_P_CRI
#define DREAL16_P	DREAL16_P_CRI
#define DEXP8		4	/* Maximum digits in exponent */
#define DEXP16		4	/* Maximum digits in exponent */
#endif

#define DREAL8_YMP80	13	/* 8-byte  CRAY REAL (partial precision) */
#define DREAL16_YMP80	29	/* 16-byte CRAY REAL (partial precision) */

/*
 *	WREALx is the w part of the Ew.d edit descriptor.  We add five
 *	to provide room for a leading blank, a leading sign, a decimal
 *	point, the exponent ('E') designator, and the sign of the exponent.
 */

#ifdef	DREAL4
#define WREAL4	(DREAL4+DEXP4+5)
#endif
#define WREAL8	(DREAL8+DEXP8+5)
#define WREAL16 (DREAL16+DEXP16+5)

/*
 *	ITEMBUFSIZ is the maximum total field width for one numeric item.  Use
 *	complex double as a convenient absolute upper bound.  Leave room for 
 *	this and a possible integer repeat count and a '*'.  The 3 are for 
 *	'(,)' and the 10 are for a little breathing room.
 */

#define ITEMBUFSIZ	(WINT + 1 + 2*WREAL16 + 3 + 10)

/*
 *	Externs
 */

#ifdef	DREAL4
extern int _dreal4;
#endif
extern int _dreal8;
extern int _dreal16;
