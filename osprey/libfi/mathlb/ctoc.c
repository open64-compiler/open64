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


#pragma ident "@(#) libfi/mathlb/ctoc.c	92.1	07/09/99 11:00:36"


#include <fortran.h>
#include <fp.h>
#include <math.h>
#include "inline.h"
#include "mathdefs.h"

extern void _CTOC(c_complex_t *ret_val, c_complex_t x, c_complex_t y);

/*
 *      complex(kind=8) raised to a complex(kind=8) = _CTOC
 *	x = a+b*i
 *	y = c+d*i
 *
 *	if ((x == 0+0*i) && (y == 0+0*i)) return(NAN)
 *	if (x == 0+0*i) then return(0+0*i)
 *	if (y == 0+0*i) then return(1+0*i)
 */

void
_CTOC(c_complex_t *ret_val,
	c_complex_t x,
	c_complex_t y)
{
	_f_real8 __atan2(_f_real8 x, _f_real8 y);
	_f_real8 __cos(_f_real8 x);
	_f_real8 __exp(_f_real8 x);
	_f_real8 __log(_f_real8 x);
	_f_real8 __sin(_f_real8 x);
	_f_real8 __sqrt(_f_real8 x);
	_f_real8 a;
	_f_real8 b;
	_f_real8 c;
	_f_real8 d;
	_f_real8 one;
	_f_real8 two;
	REGISTER_8 realx;
	REGISTER_8 imagx;
	_f_real8 logabsx, xreal, ximag, atn2, exptwo;
	if (x.real == 0.0 && x.imag == 0.0) {
		if (y.real == 0.0 && y.imag == 0.0) {
			ret_val->real = _SGL_NaN;
			ret_val->imag = _SGL_NaN;
		}
		else {
			ret_val->real = (_f_real8) 0.0;
			ret_val->imag = (_f_real8) 0.0;
		}
		return;
	}
	realx.f = x.real;
	imagx.f = x.imag;
	a = x.real;
	b = x.imag;
	c = y.real;
	d = y.imag;
	realx.ui &= ~IEEE_64_SIGN_BIT;
	imagx.ui &= ~IEEE_64_SIGN_BIT;

	atn2 = __atan2(b,a);

	if (realx.f > imagx.f)
		logabsx = __log(realx.f *
		   __sqrt(1.0 + (imagx.f/realx.f) * (imagx.f/realx.f)));
	else
		logabsx = __log(imagx.f *
		   __sqrt(1.0 + (realx.f/imagx.f) * (realx.f/imagx.f)));
	one = d * logabsx + c * atn2;
	two = c * logabsx - d * atn2;
	exptwo = __exp(two);
	ret_val->real = exptwo * __cos(one);
	ret_val->imag = exptwo * __sin(one);
}
