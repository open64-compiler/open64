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


#pragma ident "@(#) libfi/mathlb/hconjg.c	92.1	07/09/99 11:00:36"


#include <fortran.h>
#include <math.h>
#include "mathdefs.h"

#ifdef _CRAYMPP
extern _f_comp4 _HCONJG_(_f_comp4 *z);

_f_comp4
_HCONJG_( _f_comp4 *z)
{
	union hl_complx4 {
		_f_comp4	cpx4;
		struct {
			_f_real4	real;
			_f_real4	imag;
		} rlim4;
	} ret_val, y;
	y.cpx4 = *z;
	ret_val.rlim4.real = y.rlim4.real;
	ret_val.rlim4.imag = -y.rlim4.imag;
	return (ret_val.cpx4);
}
#else

extern void _HCONJG_(h_complex_t *ret_val, h_complex_t *z);
/*
 * HCONJG  - complex(kind=4) - pass by value
 *
 * Semantics:    z = a+b*i
 *               conjg(z) = a-b*i
 */
void
_HCONJG_(h_complex_t *ret_val,
	h_complex_t *z)
{
	h_complex_t y = *z;
	ret_val->real = y.real;
	ret_val->imag = -y.imag;
}
#endif
