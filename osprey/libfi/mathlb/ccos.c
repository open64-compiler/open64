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


#pragma ident "@(#) libfi/mathlb/ccos.c	92.1	07/09/99 11:00:36"


#include <fortran.h>
#include <math.h>
#include "mathdefs.h"

extern void _CCOS(c_complex_t *ret_val, c_complex_t z);
extern void _CCOS_(c_complex_t *ret_val, c_complex_t *z);

/*
 * CCOS  - complex(kind=8) - pass by value
 *
 * Semantics:  cos(z) = cos(a)*cosh(b)-sin(a)*sinh(b)*i
 *   (where z = a + b*i)
 */
void
_CCOS(c_complex_t *ret_val,
	c_complex_t z )
{
	_f_real8 __cos(_f_real8 x);
	_f_real8 __cosh(_f_real8 x);
	_f_real8 __sin(_f_real8 x);
	_f_real8 __sinh(_f_real8 x);
	ret_val->real = (__cos(z.real) * __cosh(z.imag));
	ret_val->imag = - (__sin(z.real) * __sinh(z.imag));
}

/*
 * CCOS  - complex(kind=8) - pass by address
 */
void
_CCOS_(c_complex_t *ret_val,
	c_complex_t *z)
{
	_CCOS(ret_val, *z);
}
