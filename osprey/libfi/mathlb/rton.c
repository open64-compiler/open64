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


#pragma ident "@(#) libfi/mathlb/rton.c	92.2	09/27/99 14:56:21"


#include <fortran.h>
#include <fp.h>
#include <math.h>

extern _f_real8 _RTON( _f_real8 r, _f_int4 i);

/*
 * RTON = real(kind=8) raised to an integer*32 
 */
_f_real8
_RTON( _f_real8 r,
	   _f_int4 i)
{
	_f_real8 base, result;
	if (i == 0) {
		if (r != 0.0) {
			result	= 1.0;
			/* use isnan since there are many forms
			 * of nans and cannot just compare against
			 * the fp.h _SGL_NaN.
			 */
			if (isnan(r))
				result	= r;
		}
		else {
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
			result	= 1.0;
#else
			result	= _SGL_NaN;
#endif
		}
		return(result);
	}
	result	= 1.0;
	base	= r;
	if (i < 0) {
		if (r != 0.0) {
			base	= 1.0/r;
			i	= -i;
		}
	}
	while (i != 0) {
		if ((i & 1) == 1)
			result	= result * base;
		i =	(unsigned) i >> 1;
		if (i != 0)
			base	= base * base;
	}
	return (result);
}

#if     defined(__mips) || defined(_LITTLE_ENDIAN)
_f_real8
__powdi( _f_real8 x,
	_f_int4 y )
{
	return(_RTON(x, y));
}

/* if needed for compatibility with f77 and pass by address */

#endif	/* end of mips or little endian */
