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


#pragma ident "@(#) libfi/mathlb/set_exponent.c	92.1	07/09/99 11:00:36"

#include <fortran.h>
#ifdef	__mips
#include <math.h>
#include "inline.h"
#endif

/* SET_EXPONENT - return model number whose fractional part is the
 * 		fractional part of the model representation for the
 * 		32-bit, 64-bit, or 128-bit input argument X and 
 *		whose exponent part is integer i.  
 ***/

#ifdef _F_REAL4
extern _f_real4 _SET_EXPONENT_4_I4(_f_real4 x, _f_int4 i);
extern _f_real4 _SET_EXPONENT_4(_f_real4 x, _f_int i);
extern _f_real4 _SET_EXPONENT_4_I8(_f_real4 x, _f_int8 i);

_f_real4
_SET_EXPONENT_4_I4(_f_real4 x, _f_int4 i)
{
	_f_real4 _SCALE_4(_f_real4 orig_number, _f_int orig_scale_factor);
	_f_real4 _FRACTION_4(_f_real4 x);
	_f_int	def_orig_scale;
	def_orig_scale =	i;
	return _SCALE_4(_FRACTION_4(x), def_orig_scale);
}

_f_real4
_SET_EXPONENT_4_I8(_f_real4 x, _f_int8 i)
{
	_f_real4 _SCALE_4(_f_real4 orig_number, _f_int orig_scale_factor);
	_f_real4 _FRACTION_4(_f_real4 x);
	_f_int	def_orig_scale;
	def_orig_scale =	i;
	return _SCALE_4(_FRACTION_4(x),  def_orig_scale);
}

_f_real4
_SET_EXPONENT_4(_f_real4 x, _f_int i)
{
	_f_real4 _SCALE_4(_f_real4 orig_number, _f_int orig_scale_factor);
	_f_real4 _FRACTION_4(_f_real4 x);
	return _SCALE_4(_FRACTION_4(x), i);
}
#endif	/* _F_REAL4 */

extern _f_real8 _SET_EXPONENT_8_I4(_f_real8 x, _f_int4 i);
extern _f_real8 _SET_EXPONENT_8(_f_real8 x, _f_int i);
extern _f_real8 _SET_EXPONENT(_f_real8 x, _f_int i);
extern _f_real8 _SET_EXPONENT_8_I8(_f_real8 x, _f_int8 i);

_f_real8
_SET_EXPONENT_8_I4(_f_real8 x, _f_int4 i)
{
	_f_real8 _SCALE(_f_real8 orig_number, _f_int orig_scale_factor);
	_f_real8 _FRACTION(_f_real8 x);
	_f_int	def_orig_scale;
	def_orig_scale =	i;
	return _SCALE(_FRACTION(x), def_orig_scale);
}

_f_real8
_SET_EXPONENT_8_I8(_f_real8 x, _f_int8 i)
{
	_f_real8 _SCALE(_f_real8 orig_number, _f_int orig_scale_factor);
	_f_real8 _FRACTION(_f_real8 x);
	_f_int	def_orig_scale;
	def_orig_scale =	i;
	return _SCALE(_FRACTION(x),  def_orig_scale);
}

/* Remove this entry when f90 compiler changes to SET_EXPONENT. */
_f_real8
_SET_EXPONENT_8(_f_real8 x, _f_int i)
{
	_f_real8 _SCALE(_f_real8 orig_number, _f_int orig_scale_factor);
	_f_real8 _FRACTION(_f_real8 x);
	return _SCALE(_FRACTION(x), i);
}

_f_real8
_SET_EXPONENT(_f_real8 x, _f_int i)
{
	_f_real8 _SCALE(_f_real8 orig_number, _f_int orig_scale_factor);
	_f_real8 _FRACTION(_f_real8 x);
	return _SCALE(_FRACTION(x), i);
}

#ifndef	__mips
#if _F_REAL16 == 1
extern _f_real16 _SET_EXPONENT_16_I4(_f_real16 x, _f_int4 i);
extern _f_real16 _SET_EXPONENT_16(_f_real16 x, _f_int i);
extern _f_real16 _SET_EXPONENT_16_I8(_f_real16 x, _f_int8 i);

_f_real16
_SET_EXPONENT_16_I4(_f_real16 x, _f_int4 i)
{
	_f_real16 _SCALE_16(_f_real16 orig_number, _f_int orig_scale_factor);
	_f_real16 _FRACTION_16(_f_real16 x);
	_f_int	def_orig_scale;
	def_orig_scale =	i;
	return _SCALE_16(_FRACTION_16(x), def_orig_scale);
}

_f_real16
_SET_EXPONENT_16_I8(_f_real16 x, _f_int8 i)
{
	_f_real16 _SCALE_16(_f_real16 orig_number, _f_int orig_scale_factor);
	_f_real16 _FRACTION_16(_f_real16 x);
	_f_int	def_orig_scale;
	def_orig_scale =	i;
	return _SCALE_16(_FRACTION_16(x),  def_orig_scale);
}

_f_real16
_SET_EXPONENT_16(_f_real16 x, _f_int i)
{
	_f_real16 _SCALE_16(_f_real16 orig_number, _f_int orig_scale_factor);
	_f_real16 _FRACTION_16(_f_real16 x);
	return _SCALE_16(_FRACTION_16(x), i);
}
#endif	/* _F_REAL16 == 1 */
#else	/* NOT mips */
/*
   This routine returns a 128-bit real with the same type parameter as
   X whose fractional part is the fractional part of the model
   representation for X and  whose exponent part is I, that is,
   X *b**(I-e)
*/
/* use ldexpl routine in libc prototyped in math.h */
_f_real16
_SET_EXPONENT_16(_f_real16 a, _f_int4 i)
{
	_f_int4		dummy;
	_f_real16	aa;
	if (a == 0.0) {
		aa	= 0.0;
	} else {
		aa	= ldexpl(_get_frac_and_exp(a,&dummy),i);
	}
	return (aa);
}  
#endif	/* NOT mips */
