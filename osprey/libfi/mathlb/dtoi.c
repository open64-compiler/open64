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


#pragma ident "@(#) libfi/mathlb/dtoi.c	92.1	07/09/99 11:00:36"
 
#include <fortran.h>
#include <fp.h>
#include <math.h>
#include "inline.h"

extern _f_real16 _DTOI( _f_real16 r, _f_int8 *i);

/*
 * DTOI - Real(kind=16) raised to an integer(kind=8) power
 */
_f_real16
_DTOI( _f_real16 r,
	 _f_int8 *i)
{
	_f_real16 base, result;
	_f_int8 ipow;
	ipow =	*i;
	if (ipow == 0) {
		if (r != 0.0) {
			result =	1.0;
			/* retain isnan since _DBL_NaN
			 * is just one form of many
			 * possible nans.
			 */
			if (isnan128(r))
				result =        r;
		}
		else {
			result = _DBL_NaN;
		}
		return(result);
	}
	result =	1.0;
	base =  r;
	if (ipow < 0) {
		if (r != 0.0) {
			base =  1.0/r;
			ipow =     -ipow;
		}
	}
	while (ipow != 0) {
		if ((ipow & 1) == 1)
			result *=       base;
		ipow =	(unsigned) ipow >> 1;
		if (ipow != 0)
			base *= base;
	}
	return (result);
}

