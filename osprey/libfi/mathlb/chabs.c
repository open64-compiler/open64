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


#pragma ident "@(#) libfi/mathlb/chabs.c	92.1	07/09/99 11:00:36"
#include <fortran.h>
#include <math.h>
#include "mathdefs.h"

#ifdef _CRAYMPP
extern _f_real4 _CHABS_(_f_comp4 *z);
extern _f_real4 _CHABS(_f_comp4 z);

_f_real4
_CHABS_(_f_comp4 *z)
{
	return (_CHABS(*z));
}
#else
extern _f_real4 _CHABS_(h_complex_t *z);
extern _f_real4 _CHABS(h_complex_t z);

/*
 * _CHABS_:  complex(kind=4) absolute value, returns real(kind=4) value
 *        - pass by address
 */
_f_real4
_CHABS_(h_complex_t *z)
{
	return (_CHABS(*z));
}
#endif

/*
 * _CHABS:  complex(kind=4) absolute value, returns real(kind=4) value
 *        - pass by value
 */
_f_real4

#ifdef _CRAYMPP
_CHABS(_f_comp4 z)
{
	union hl_complx4 {
		_f_comp4	cpx4;
		struct {
			_f_real4	real;
			_f_real4	imag;
		} rlim4;
	} f;
	_f_real4 ret_val;
	_f_real8 _SQRT(_f_real8 x);
	_f_real8 real, imag;
	f.cpx4	= z;
	real = fabs((_f_real8) f.rlim4.real);
	imag = fabs((_f_real8) f.rlim4.imag);
	if (real == 0.0 && imag == 0.0) return((_f_real4) 0.0);
	if (real > imag)
		ret_val =
		 (_f_real4) (real * _SQRT((_f_real8) 1.0 +
				 (imag/real) * (imag/real)));
	else
		ret_val =
		 (_f_real4) (imag * _SQRT((_f_real8) 1.0 +
				 (real/imag) * (real/imag)));
#else
_CHABS(h_complex_t z)
{
	_f_real8 __fabs(_f_real8 x);
	_f_real8 __sqrt(_f_real8 x);
	_f_real8 real = __fabs((_f_real8) z.real);
	_f_real8 imag = __fabs((_f_real8) z.imag);
	_f_real4 ret_val;
	if (real == 0.0 && imag == 0.0) return((_f_real4) 0.0);
	if (real > imag)
		ret_val =
		 (_f_real4) (real * __sqrt((_f_real8) 1.0 +
				 (imag/real) * (imag/real)));
	else
		ret_val =
		 (_f_real4) (imag * __sqrt((_f_real8) 1.0 +
				 (real/imag) * (real/imag)));
#endif
	return (ret_val);
}
