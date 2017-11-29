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


#pragma ident "@(#) libfi/mathlb/chtoh.c	92.1	07/09/99 11:00:36"


#include <fortran.h>
#include <math.h>
#include "mathdefs.h"

extern void _CHTOH(h_complex_t *ret_val, h_complex_t x, _f_real4 *r);

/*
 * complex(kind=4) raised to a real(kind=4):  _CHTOH
 */
void
_CHTOH(h_complex_t *ret_val,
	h_complex_t x,
	_f_real4 *r)
{
	_f_real8 y =	(_f_real8) *r;
	c_complex_t a, c;
	void _CTOR(c_complex_t *ret_val, c_complex_t x, _f_real8 *r);

	a.real = (_f_real8) x.real;
	a.imag = (_f_real8) x.imag;
	(void) _CTOR( &c, a, &y );
	ret_val->real = (_f_real4) c.real;
	ret_val->imag = (_f_real4) c.imag;
}
