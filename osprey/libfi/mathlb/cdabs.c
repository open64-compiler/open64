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


#pragma ident "@(#) libfi/mathlb/cdabs.c	92.1	07/09/99 11:00:36"

#include <fortran.h>
#include <math.h>
#include "mathdefs.h"

extern _f_real16 _CDABS(d_complex_t z);
extern _f_real16 _CDABS_(d_complex_t *z);

/*
 * CDABS:  complex(kind=16) absolute value, returns real(kind=16)
 * 128-bit float
 *        - pass by value
 */

_f_real16
_CDABS(d_complex_t z)
{
	_f_real16 __sqrtl(_f_real16 x);
	_f_real16 __fabsl(_f_real16 x);
	_f_real16 real = __fabsl(z.real);
	_f_real16 imag = __fabsl(z.imag);
	_f_real16 ret_val;

	if (real == 0.0 && imag == 0.0)
		return((_f_real16) 0.0);
	if (real > imag)
		ret_val = real * __sqrtl((_f_real16) 1.0 +
			(imag/real) * (imag/real));
	else
		ret_val = imag * __sqrtl((_f_real16) 1.0 +
			(real/imag) * (real/imag));

	return (ret_val);
}

/*
 * CDABS:  complex(kind=16) absolute value, returns real(kind=16)
 * 128-bit float
 *        - pass by address
 */

_f_real16
_CDABS_(d_complex_t *z)
{
	return (_CDABS(*z));
}
