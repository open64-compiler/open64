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


#pragma ident "@(#) libfi/mathlb/cton.c	92.2	11/16/99 18:14:06"


#include <fortran.h>
#include <fp.h>
#include <math.h>
#include "mathdefs.h"

#if	!defined(__LITTLE_ENDIAN) && !defined(__mips)
extern void _CTON(c_complex_t *ret_val, c_complex_t x, _f_int4 *y );

/*
 *	complex(kind=8) raised to an Integer*32 = _CTON
 *
 *	x = a+b*i
 *
 *	if ((x == 0+0*i) && (i == 0)) then return(NAN)
 *	if (x == 0+0*i) then return(0+0*i)
 */
void
_CTON(c_complex_t *ret_val,
	c_complex_t x,
	_f_int4 *y )
{
	long   i =	*y;
	_f_real8 t, a, b;  /* temporary values */
	a =	x.real;
	b =	x.imag;
	if (a == (_f_real8) 0.0 && b == (_f_real8) 0.0) {
		if (i == 0) {
			ret_val->real =	_SGL_NaN;
			ret_val->imag =	_SGL_NaN;
		}
		else {
			ret_val->real =	(_f_real8) 0.0;
			ret_val->imag =	(_f_real8) 0.0;
		}
		return;
	}
	if (i <  0) {
		i =	-i;
		t =	a;
		a =	a / (a*a + b*b);
		b =	(- b) / (t*t + b*b);
	}
        ret_val->real =	(_f_real8) 1.0;
        ret_val->imag =	(_f_real8) 0.0;
	while (i !=  0) {
		if ((i & 1) == 1) {
			t =	ret_val->real;
			ret_val->real =	t * a - ret_val->imag * b;
			ret_val->imag =	ret_val->imag * a + t * b;
		}
		t =	a;
		a =	a*a - b*b;
		b =	b*t + t*b;
		i =	(unsigned) i >> 1;
	}
}

#elif	defined(_LITTLE_ENDIAN)
/* __powzi = a**n  */
dbl_complex_t
__powzi(_f_real8 arel, _f_real8 aimg, _f_int4 n)
{
	dbl_complex_t	x, ret_val;
	_f_int4		i;
	_f_real8	t, a, b;

	i	= n;
	a	= arel;
	b	= aimg;
	ret_val.real	= 1.0;
	ret_val.imag	= 0.0;

	if (i < 0) {
		i	= -i;
		t =	a;
		a =	a / (a*a + b*b);
		b =	(- b) / (t*t + b*b);
	}

	while (i !=  0) {
		if ((i & 1) == 1) {
			t =	ret_val.real;
			ret_val.real =	t * a - ret_val.imag * b;
			ret_val.imag =	ret_val.imag * a + t * b;
		}
		t =	a;
		a =	a*a - b*b;
		b =	b*t + t*b;
		i =	(unsigned) i >> 1;
	}

	return ret_val;
}
#endif
