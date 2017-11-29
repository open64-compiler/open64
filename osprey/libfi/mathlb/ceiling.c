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


#pragma ident "@(#) libfi/mathlb/ceiling.c	92.2	09/29/99 18:33:23"
#include <fmath.h>
#include <fortran.h>

#ifndef _TWO_POWER_63
#define _TWO_POWER_63 9223372036854775808.0
#endif

#ifndef _TWO_POWER_31
#define _TWO_POWER_31 2147483648.0
#endif

/* CEILING(REAL A [, KIND]) where KIND is added in F95.
 *
 *  Return the least integer greater than or equal to its argument
 *  where:
 *   
 *        A	shall be of type real.
 *        KIND	shall be a scalar integer initialization expression,
 *               i.e., it shall be known at compilation time.
 *   
 *  The result shall be INTEGER.  If KIND is present, the kind type
 *  is that specified by KIND.  Otherwise, the kind type parameter is
 *  that of default integer type.  The result value has a value equal
 *  to the least integer greater than or equal to A.  For example,
 *  CEILING(3.7) has the value 4 while CEILING(-3.7) has the value -3.
 *   
 *  On IEEE systems with IEEE754, conversion from float to integer
 *  format can signal an invalid operation exception when overflow,
 *  underflow, or NaN precludes a faithful representation in that
 *  format and this cannot otherwise be signaled.
 */

#if defined (_CRAY1) && !defined(_CRAYIEEE)
/* This is non-IEEE on CRAY ---------------------------------	*/

#include <math.h>
/* The 64-bit version is needed to circumvent the fastmd int.
 * NINT would give overflow message on PVP systems and set result
 * to properly signed HUGE_INT8_F90 in case NINT returns here.
 *		_NINT_(& 65536.0);
 * at some future time only.
 *	if (x > 0 && x < _TWO_POWER_63 && ia != x)
 *		result	= result + 1;
 * Use this method to return a value if the floating point argument
 * is GE 2**63 so that some value is returned since the standard
 * description of f95 does not indicate what to return in case of
 * the integer cannot contain the result of the conversion from
 * float to integer.  This mimics the values returned by IEEE side.
 */
#define	_CEIL8()					\
	_f_real8	ia;				\
	result		= x;				\
	ia	= result;				\
	if (fabs(x) < _TWO_POWER_63) {			\
		if (x >= 0.0 && ia != x)		\
			result	= result + 1;		\
	} else {					\
		if (x >= 0.0)				\
			result	= HUGE_INT8_F90;	\
		else					\
			result	= -HUGE_INT8_F90;	\
	}

#define _CEIL16()					\
	_f_real16	ia;				\
	result		= x;				\
	ia	= result;				\
	if (fabsl(x) < HUGE_INT8_F90) {			\
		if (x >= 0.0 && ia != x)		\
			result	= result + 1;		\
	} else {					\
		if (x >= 0.0)				\
			result	= HUGE_INT8_F90;	\
		else					\
			result	= -HUGE_INT8_F90;	\
	}

#elif	defined(_garbage) && (defined(__mips) || defined(_LITTLE_ENDIAN))
/* This is mips ---------------------------------------------	*/

/* Use largest precision for the conversion of float to int.
 * The hardware returns a HUGE of the proper size integer with
 * this code on IRIX.  The extra check prevents the subtraction
 * of one when result is already equal to MAXINT.
 */
#define _CEIL4()				\
	_f_real4	ia;			\
	result	= x;				\
	ia	= result;			\
	if (x > 0.0 && x < 8388608 && ia != x)	\
		result	= result + 1;

#define _CEIL8()					\
	_f_real8	ia;				\
	result	= x;					\
	ia	= result;				\
	if (x > 0.0 && x < 4503599627370496 && ia != x)	\
		result	= result + 1;

#define _CEIL16()					\
	_f_real16	ia;				\
	result	= x;					\
	ia	= result;				\
	if (x > 0.0 && x < _TWO_POWER_63 && ia != x)	\
		result	= result + 1;

#else
/* This is ieee but not irix -------------------------------	*/

/* Use this code for non-IRIX ieee systems so that there is some
 * commonality of value returned if the integer cannot contain
 * the result of the conversion of real to integer.  This conversion
 * may cause an INVALID_OPR exception on some systems.
 */

/*
 * if x is .GE. 2**23, it is an integer.  The value 23 is the size
 * of the mantissa.  Return ((x < 0.0 || x >= i) ? i : i + 1);
 *	if (x > 0 && x < 8388608 && ia != x)
 *			result	= result + 1;
 *
 * Return -_TWO_POWER_31 which is -HUGE_INT4_F90 - 1 for negative
 * values that are .LE. -_TWO_POWER_31.
 */
#include <math.h>
#if _F_REAL4 == 1
#define	_CEIL4()					\
	_f_real4	ia;				\
	result		= (typeof(result)) x;		\
	ia	= result;				\
	if (fabsf(x) < 8388608) {			\
		if (x >= 0.0 && ia != x)		\
			result	= result + 1;		\
	} else {					\
		if (x >= _TWO_POWER_31)			\
			result	= HUGE_INT4_F90;	\
		else if (x <= -_TWO_POWER_31)		\
			result	= -_TWO_POWER_31;	\
	}

#endif	/* end 32-bit real */

/* if x is .GE. 2**52, it is an integer.  The value 52 is the size
 * of the mantissa.  Return ((x < 0.0 || x >= i) ? i : i + 1);
 * If interrupts are not turned on, return the properly signed HUGE.
 *
 * Return -_TWO_POWER_63 which is -HUGE_INT8_F90 - 1 for negative
 * values that are .LE. -_TWO_POWER_63.
 * DO NOT USE (result = -_TWO_POWER_63) since c compiler produces
 * warnings about the conversion of -_TWO_POWER_63 to integer.
 */
#define	_CEIL8()					\
	_f_real8	ia;				\
	result		= (typeof(result)) x;		\
	ia	= result;				\
	if (fabs(x) < 4503599627370496LL) {		\
		if (x >= 0.0 && ia != x)		\
			result	= result + 1;		\
	} else {					\
		if ((long long) x >= _TWO_POWER_63)	\
			result	= (typeof(result)) HUGE_INT8_F90;	\
		else if ((long long) x <= -_TWO_POWER_63)\
			result	= (typeof(result))(-HUGE_INT8_F90 - 1);	\
	}

#if _F_REAL16 == 1
/* if x is .GE. 2**112, it is an integer.  The value 112 is the size
 * of the mantissa.  Return ((x < 0.0 || x >= i) ? i : i + 1);
 * If interrupts are not turned on, return the properly signed HUGE.
 *
 * Return -_TWO_POWER_63 which is -HUGE_INT8_F90 - 1 for negative
 * values that are .LE. -_TWO_POWER_63.
 * DO NOT USE (result = -_TWO_POWER_63) since c compiler produces
 * warnings about the conversion of -_TWO_POWER_63 to integer.
 */
#define _CEIL16()					\
	_f_real16	ia;				\
	result		= (typeof(result)) x;		\
	ia	= result;				\
	if (fabsl(x) <= HUGE_INT8_F90) {		\
		if (x >= 0.0 && ia != x)		\
			result	= result + 1;		\
	} else {					\
		if (x >= 0.0)				\
			result	= (typeof(result)) HUGE_INT8_F90;	\
		else if (x <= -_TWO_POWER_63)		\
			result	= (typeof(result))(-HUGE_INT8_F90 - 1);	\
	}

#endif	/* _F_REAL16 equal 1 */
/* End Macro code ------------------------------------------	*/
#endif	/* CRAY1, mips, other */

#ifndef	__mips
/* The following code was used by SPARC, MPP, and TRITON,ieee with F90.
 * It was inlined by YMP/C90.
 */

#if _F_REAL4 == 1
_f_int
_CEILING_4(_f_real4 x)
{
	_f_int result;
	_CEIL4();
	return (result);
}
#endif	/* _F_REAL4 present */

_f_int
_CEILING(_f_real8 x)
{
	_f_int result;
	_CEIL8();
	return (result);
}

#if _F_REAL16 == 1
_f_int
_CEILING_16(_f_real16 x)
{
	_f_int		result;
	_CEIL16();
	return (result);
}
#endif	/* _F_REAL16 present */
#endif	/* NOT mips */

#if _F_REAL4 == 1
/* New code for F95 standard to return the type of the kind argument. */
_f_int1
_CEILING_4_1(_f_real4 x)
{
	_f_int4		result;
	_CEIL4();
	return ((_f_int1)result);
}

_f_int2
_CEILING_4_2(_f_real4 x)
{
	_f_int4		result;
	_CEIL4();
	return ((_f_int2)result);
}

_f_int4
_CEILING_4_4(_f_real4 x)
{
	_f_int4		result;
	_CEIL4();
	return (result);
}

/* Use FLOOR8 rather than FLOOR4  when the result is integer(8) */
_f_int8
_CEILING_4_8(_f_real4 argx)
{
	_f_int8		result;
	_f_real8	x = (_f_real8)argx;
	_CEIL8();
	return (result);
}
#endif	/* _F_REAL4 present */

_f_int1
_CEILING_8_1(_f_real8 x)
{
	_f_int8		result;
	_CEIL8();
	return ((_f_int1)result);
}

_f_int2
_CEILING_8_2(_f_real8 x)
{
	_f_int8		result;
	_CEIL8();
	return ((_f_int2)result);
}

_f_int4
_CEILING_8_4(_f_real8 x)
{
	_f_int8		result;
	_CEIL8();
	return ((_f_int4)result);
}

_f_int8
_CEILING_8_8(_f_real8 x)
{
	_f_int8		result;
	_CEIL8();
	return (result);
}

#if _F_REAL16 == 1
_f_int1
_CEILING_16_1(_f_real16 x)
{
	_f_int8		result;
	_CEIL16();
	return ((_f_int1)result);
}

_f_int2
_CEILING_16_2(_f_real16 x)
{
	_f_int8		result;
	_CEIL16();
	return ((_f_int2)result);
}

_f_int4
_CEILING_16_4(_f_real16 x)
{
	_f_int4		result;
	_CEIL16();
	return (result);
}

_f_int8
_CEILING_16_8(_f_real16 x)
{
	_f_int8		result;
	_CEIL16();
	return (result);
}
#endif	/* _F_REAL16 present. */
