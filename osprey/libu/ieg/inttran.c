/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
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


#pragma ident "@(#) libu/ieg/inttran.c	92.3	11/16/99 15:10:31"

#include <fortran.h>

/* Numeric conversion constants */

#define	MASK8	0xFF
#define	SIGN8	0x80
#define	MASK16	0xFFFF
#define	SIGN16	0x8000
#define	MASK32	0xFFFFFFFF
#define	SIGN32	0x80000000

struct overflow_count {
	_f_int4	oflows;		/* Count of overflows */
};

/*
 *	Endian swap indicator:
 *
 *	0  no swap
 *	1  DEC-style swap
 *	2  Intel-style swap
 */

struct endian_info_packet {
	_f_int4	swap;		/* Endian swap indicator */
};

#define	OVERFLOW_ENABLE	1	/* Enable detection of integer overflow */

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
struct overflow_count t$generic_;
#define	set_overflow()	(t$generic_.oflows++)
#if defined(BUILD_OS_DARWIN)
extern
#endif /* defined(BUILD_OS_DARWIN) */
struct endian_info_packet	t$endian_;
#define	uint16	unsigned short
#define	uint32	unsigned int
#define	uint64	unsigned long long
#else
#if	defined(_CRAY) && !defined(_CRAYMPP)
#pragma _CRI taskcommon T@GENERIC
#endif
struct overflow_count T@GENERIC;
#define	set_overflow()	(T@GENERIC.oflows++)
#endif

/*
 *	Generic integer resizing functions:
 *
 *	Note, on CRAY PVP systems these functions always use 64-bit
 *	containers.
 *
 *	Entry points (scalar, call-by-address):
 *
 *	GI8I$		Twos-complement 8-bit integer to twos-complement
 *			64-bit integer
 *	GI16I$ 		Twos-complement 16-bit integer to twos-complement
 *			64-bit integer
 *	GI32I$ 		Twos-complement 32-bit integer to twos-complement
 *			64-bit integer
 *
 *	GI8O$		Twos-complement 64-bit integer to twos-complement
 *			8-bit integer
 *	GI16O$ 		Twos-complement 64-bit integer to twos-complement
 *			16-bit integer
 *	GI32O$ 		Twos-complement 64-bit integer to twos-complement
 *			32-bit integer
 *
 *	Using 32-bit containers on input (for CRAY MPP systems)
 *	=======================================================
 *
 *	SI8O$		Twos-complement 32-bit integer to twos-complement
 *			8-bit integer
 *
 *	SI16O$ 		Twos-complement 32-bit integer to twos-complement
 *			16-bit integer
 *
 *	Using "natural" sized containers (for MIPS systems)
 *	===================================================
 *
 *	TI8O$		Twos-complement 16-bit integer to twos-complement
 *			8-bit integer
 */

/*
 *	GI8I$		Twos-complement 8-bit integer to twos-complement
 *			64-bit integer
 *
 *	Exceptions:	None
 */

_f_int8
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
gi8i$_(_f_int1 *datum)
#else
GI8I$(_f_int8 *datum)
#endif
{
	register _f_int8	word;

	word	= (_f_int8) *datum;

	return (((word & MASK8) ^ SIGN8) - SIGN8);
}

/*
 *	GI16I$ 		Twos-complement 16-bit integer to twos-complement
 *			64-bit integer
 *
 *	Exceptions:	None
 */

_f_int8
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
gi16i$_(_f_int2 *datum)
#else
GI16I$(_f_int8 *datum)
#endif
{
	register _f_int8	word;

	word	= (_f_int8) *datum;

	return (((word & MASK16) ^ SIGN16) - SIGN16);
}

/*
 *	GI32I$ 		Twos-complement 32-bit integer to twos-complement
 *			64-bit integer
 *
 *	Exceptions:	None
 */

_f_int8
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
gi32i$_(_f_int4 *datum)
#else
GI32I$(_f_int8 *datum)
#endif
{
	register _f_int8	word;

	word	= (_f_int8) *datum;

	return (((word & MASK32) ^ SIGN32) - SIGN32);
}

/*
 *	GI8O$		Twos-complement 64-bit integer to twos-complement
 *			8-bit integer
 *	SI8O$		Twos-complement 32-bit integer to twos-complement
 *			8-bit integer
 *	TI8O$		Twos-complement 16-bit integer to twos-complement
 *			8-bit integer
 *
 *	Exceptions:	None
 */

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
_f_int1
ti8o$_(_f_int2 *datum)
{
	register _f_int8	result;
	register _f_int8	word;

	word	= (_f_int8) *datum;
	result	= word & MASK8;
	word	= word & ~MASK8;

	if (word != 0 && word != ~MASK8) {	/* If overflow */
		result	= MASK8 ^ SIGN8;	/* Set largest value */
		if (word < 0)
			result	= SIGN8;
#if	OVERFLOW_ENABLE
		set_overflow();
#endif
	}

	return (result);
}
#endif

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
_f_int1
si8o$_(_f_int4 *datum)
#else
_f_int8
SI8O$(_f_int4 *datum)
#endif
{
	register _f_int8	result;
	register _f_int8	word;

	word	= (_f_int8) *datum;
	result	= word & MASK8;
	word	= word & ~MASK8;

	if (word != 0 && word != ~MASK8) {	/* If overflow */
		result	= MASK8 ^ SIGN8;	/* Set largest value */
		if (word < 0)
			result	= SIGN8;
#if	OVERFLOW_ENABLE
		set_overflow();
#endif
	}

	return (result);
}

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
_f_int1
gi8o$_(_f_int8 *datum)
#else
_f_int8
GI8O$(_f_int8 *datum)
#endif
{
	register _f_int8	result;
	register _f_int8	word;

	word	= (_f_int8) *datum;
	result	= word & MASK8;
	word	= word & ~MASK8;

	if (word != 0 && word != ~MASK8) {	/* If overflow */
		result	= MASK8 ^ SIGN8;	/* Set largest value */
		if (word < 0)
			result	= SIGN8;
#if	OVERFLOW_ENABLE
		set_overflow();
#endif
	}

	return (result);
}

/*
 *	GI16O$ 		Twos-complement 64-bit integer to twos-complement
 *			16-bit integer
 *	SI16O$ 		Twos-complement 32-bit integer to twos-complement
 *			16-bit integer
 *
 *	Exceptions:	None
 */

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
_f_int2
si16o$_(_f_int4 *datum)
#else
_f_int8
SI16O$(_f_int4 *datum)
#endif
{
	register _f_int8	result;
	register _f_int8	word;

	word	= (_f_int8) *datum;
	result	= word & MASK16;
	word	= word & ~MASK16;

	if (word != 0 && word != ~MASK16) {	/* If overflow */
		result	= MASK16 ^ SIGN16;	/* Set largest value */
		if (word < 0)
			result	= SIGN16;
#if	OVERFLOW_ENABLE
		set_overflow();
#endif
	}

	return (result);
}

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
_f_int2
gi16o$_(_f_int8 *datum)
#else
_f_int8
GI16O$(_f_int8 *datum)
#endif
{
	register _f_int8	result;
	register _f_int8	word;

	word	= (_f_int8) *datum;
	result	= word & MASK16;
	word	= word & ~MASK16;

	if (word != 0 && word != ~MASK16) {	/* If overflow */
		result	= MASK16 ^ SIGN16;	/* Set largest value */
		if (word < 0)
			result	= SIGN16;
#if	OVERFLOW_ENABLE
		set_overflow();
#endif
	}

	return (result);
}

/*
 *	GI32O$		Convert 64-bit long integer to generic 32-bit integer
 *
 *	Exceptions:	None
 */

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
_f_int4
gi32o$_(_f_int8 *datum)
#else
_f_int8
GI32O$(_f_int8 *datum)
#endif
{
	register _f_int8	result;
	register _f_int8	word;

	word	= (_f_int8) *datum;
	result	= word & MASK32;
	word	= word & ~MASK32;

	if (word != 0 && word != ~MASK32) {	/* If overflow */
		result	= MASK32 ^ SIGN32;	/* Set largest value */
		if (word < 0)
			result	= SIGN32;
#if	OVERFLOW_ENABLE
		set_overflow();
#endif
	}

	return (result);
}

/*
 *	CSWAP2$		16-bit little-endian <-> big-endian conversion
 *	CSWAP4$		32-bit little-endian <-> big-endian conversion
 *	CSWAP8$		64-bit little-endian <-> big-endian conversion
 *
 *	Exceptions:	None
 */

#if	defined(__mips) || defined(_LITTLE_ENDIAN)

_f_int2
cswap2$_(_f_int2 *datum)
{
	register short	swap;
	register uint16	word;
	_f_int2		result;

	swap	= t$endian_.swap;
	word	= *datum;

	if (swap == 1) {
		word	= ((word & 0x00FF) << 8) |
			  ((word & 0xFF00) >> 8);	/* Swap bytes */
	}

	result	= word;

	return(result);
}

_f_int4
cswap4$_(_f_int4 *datum)
{
	register short	swap;
	register uint32	word;
	_f_int4		result;

	swap	= t$endian_.swap;
	word	= *datum;

	if (swap != 0) {

		word	= (word << 16) | (word >> 16);	/* Swap words */

		if (swap == 1)	/* If DEC-style, also swap bytes */
			word	= ((word & 0x00FF00FF) << 8) |
				  ((word & 0xFF00FF00) >> 8);

	}

	result	= word;

	return(result);
}

_f_int8
cswap8$_(_f_int8 *datum)
{
	register short	swap;
	register uint64	word;
	_f_int8		result;

	swap	= t$endian_.swap;
	word	= *datum;

	if (swap != 0) {

		word	= (word << 32) | (word >> 32);	/* Swap longwords */

		word	= ((word & 0x0000FFFF0000FFFFLL) << 16) |
			  ((word & 0xFFFF0000FFFF0000LL) >> 16);	/* Swap words */

		if (swap == 1) /* If DEC-style, also swap bytes */
			word	= ((word & 0x00FF00FF00FF00FFLL) << 8) |
				  ((word & 0xFF00FF00FF00FF00LL) >> 8);

	}

	result	= word;

	return(result);
}

#endif
