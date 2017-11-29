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


#pragma ident "@(#) libfi/mathlb/chtoch.c	92.1	07/09/99 11:00:36"


#include <fortran.h>
#include <fp.h>
#include <math.h>
#include "inline.h"
#include "mathdefs.h"

extern void _CHTOCH(h_complex_t *ret_val, h_complex_t x, h_complex_t y);

/*
 * CHTOCH: complex(kind=4) raised to a complex(kind=4): pass by value
 */
void
_CHTOCH(h_complex_t *ret_val,
	h_complex_t x,
	h_complex_t y)
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
	REGISTER_4 realx;
	REGISTER_4 imagx;
	_f_real8 logabsx, xreal, ximag, atn2, exptwo;
	if (x.real == 0.0 && x.imag == 0.0) {
		if (y.real == 0.0 && y.imag == 0.0) {
			ret_val->real = _HALF_NaN;
			ret_val->imag = _HALF_NaN;
		}
		else {
			ret_val->real = (_f_real4) 0.0;
			ret_val->imag = (_f_real4) 0.0;
		}
		return;
	}
	realx.f = x.real;
	imagx.f = x.imag;
	a = (_f_real8) x.real;
	b = (_f_real8) x.imag;
	c = (_f_real8) y.real;
	d = (_f_real8) y.imag;
	realx.ui &= ~IEEE_32_SIGN_BIT;
	imagx.ui &= ~IEEE_32_SIGN_BIT;
	xreal = (_f_real8) realx.f;
	ximag = (_f_real8) imagx.f;

	atn2 = __atan2(b,a);

	if (realx.f > imagx.f)
		logabsx = __log(xreal * 
			__sqrt(1.0 + (ximag/xreal) * (ximag/xreal)));
	else
		logabsx = __log(ximag *
			__sqrt(1.0 + (xreal/ximag) * (xreal/ximag)));

	one = d * logabsx + c * atn2;
	two = c * logabsx - d * atn2;

	exptwo = __exp(two);

	ret_val->real = (_f_real4) (exptwo * __cos(one));
	ret_val->imag = (_f_real4) (exptwo * __sin(one));
}
