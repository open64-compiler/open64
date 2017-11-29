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


#pragma ident "@(#) libfi/mathlb/ieee_class_d.c	92.1	07/09/99 11:00:36"

#include <fortran.h>
#include "ieee_class_values.h"
#include "inline.h"

extern _f_int4 _IEEE_CLASS_I4_D( _f_real16 x);

/* _IEEE_CLASS_D returns a value indicating the class of the argument.
 * The classes are listed in MODULE CRI_IEEE_DEFINITIONS.
 * The argument is a 128-bit real value and the result is a 32-bit integer.
 */
_f_int4 _IEEE_CLASS_I4_D( _f_real16 x)
{
	/* Union defined to work with IEEE 128-bit floating point. */
	union _ieee_ldouble {
		_f_real16	ldword;
		struct {
			unsigned int sign       : 1;
			unsigned int exponent   : IEEE_128_EXPO_BITS;
			unsigned int q_bit	: 1;
			unsigned int mantissa_up1 : IEEE_128_MANT_BTS_UP1-1;
			unsigned int mantissa_up2 : IEEE_128_MANT_BTS_UP2;
			unsigned int mantissa_lo1 : IEEE_128_MANT_BTS_LO1;
			unsigned int mantissa_lo2 : IEEE_128_MANT_BTS_LO2;
		} parts;
	};
	union	_ieee_ldouble x_val;

	x_val.ldword		= x;
	if (x_val.parts.exponent == 0) {
		if (x_val.parts.mantissa_up1 == 0 &&
		    x_val.parts.mantissa_up2 == 0 &&
		    x_val.parts.mantissa_lo1 == 0 &&
		    x_val.parts.mantissa_lo2 == 0 &&
		    x_val.parts.q_bit == 0) {
			if (x_val.parts.sign)
				return (IEEE_CLASS_NEG_ZERO);
			else
				return (IEEE_CLASS_POS_ZERO);
		} else {
			if (x_val.parts.sign)
				return (IEEE_CLASS_NEG_DENORM);
			else
				return (IEEE_CLASS_POS_DENORM);
		}
	} else if (x_val.parts.exponent == IEEE_128_EXPO_MAX) {
		if (x_val.parts.mantissa_up1 == 0 &&
		    x_val.parts.mantissa_up2 == 0 &&
		    x_val.parts.mantissa_lo1 == 0 &&
		    x_val.parts.mantissa_lo2 == 0 &&
		    x_val.parts.q_bit == 0) {
			if (x_val.parts.sign)
				return (IEEE_CLASS_NEG_INFINITY);
			else
				return (IEEE_CLASS_POS_INFINITY);
		} else {
			if (x_val.parts.q_bit)
				return (IEEE_CLASS_QUIET_NAN);
			else
				return (IEEE_CLASS_SIGNALING_NAN);
		}
	} else if (x_val.parts.sign)
		return (IEEE_CLASS_NEG_NORM_NONZERO);
	else
		return (IEEE_CLASS_POS_NORM_NONZERO);
}


extern _f_int8 _IEEE_CLASS_I8_D( _f_real16 x);

/* The argument is a 128-bit real value and the result is a 64-bit integer.
 */
_f_int8 _IEEE_CLASS_I8_D( _f_real16 x)
{
	/* Union defined to work with IEEE 128-bit floating point. */
	union _ieee_ldouble {
		_f_real16	ldword;
		struct {
			unsigned int sign       : 1;
			unsigned int exponent   : IEEE_128_EXPO_BITS;
			unsigned int q_bit	: 1;
			unsigned int mantissa_up1 : IEEE_128_MANT_BTS_UP1-1;
			unsigned int mantissa_up2 : IEEE_128_MANT_BTS_UP2;
			unsigned int mantissa_lo1 : IEEE_128_MANT_BTS_LO1;
			unsigned int mantissa_lo2 : IEEE_128_MANT_BTS_LO2;
		} parts;
	};
	union	_ieee_ldouble x_val;

	x_val.ldword		= x;
	if (x_val.parts.exponent == 0) {
		if (x_val.parts.mantissa_up1 == 0 &&
		    x_val.parts.mantissa_up2 == 0 &&
		    x_val.parts.mantissa_lo1 == 0 &&
		    x_val.parts.mantissa_lo2 == 0 &&
		    x_val.parts.q_bit == 0) {
			if (x_val.parts.sign)
				return (IEEE_CLASS_NEG_ZERO);
			else
				return (IEEE_CLASS_POS_ZERO);
		} else {
			if (x_val.parts.sign)
				return (IEEE_CLASS_NEG_DENORM);
			else
				return (IEEE_CLASS_POS_DENORM);
		}
	} else if (x_val.parts.exponent == IEEE_128_EXPO_MAX) {
		if (x_val.parts.mantissa_up1 == 0 &&
		    x_val.parts.mantissa_up2 == 0 &&
		    x_val.parts.mantissa_lo1 == 0 &&
		    x_val.parts.mantissa_lo2 == 0 &&
		    x_val.parts.q_bit == 0) {
			if (x_val.parts.sign)
				return (IEEE_CLASS_NEG_INFINITY);
			else
				return (IEEE_CLASS_POS_INFINITY);
		} else {
			if (x_val.parts.q_bit)
				return (IEEE_CLASS_QUIET_NAN);
			else
				return (IEEE_CLASS_SIGNALING_NAN);
		}
	} else if (x_val.parts.sign)
		return (IEEE_CLASS_NEG_NORM_NONZERO);
	else
		return (IEEE_CLASS_POS_NORM_NONZERO);
}
