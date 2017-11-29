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



#pragma ident "@(#) libu/numconv/mpp/sd2uese.c	92.1	06/28/99 15:00:40"

#include <cray/fmtconv.h>

/*
	General:
		Convert the value to the formatted ASCII character field 
		specified.  Using the Fortran "ESw.dEe" format.
		The "ESw.dEe" descriptor is the same as the "1pEw.dEe"
		descriptor, so a call is made to the _sd2udee routine
		with the scale factor p = 1.

	Inputs:
		value - value to output
		fca - first character address of ASCII output field
		mode - flags to determine sign behavior
			0 = only minus sign required
			1 = sign required
		w - total width of character field
		d - width of field after decimal point
		e - field width of exponent
		p - ignored

	Outputs:
		value - unchanged
		fca - unchanged
		mode - unchanged
		w - unchanged
		d - unchanged
		e - unchanged
		p - unchanged

        Examples:
		With value=0.105e10, mode=1, w=10, d=2, e=0 we will end up
		with a field filled with " +1.05E+09"

		With value=-9.995e-10, mode=0, w=10, d=2, e=2 we will end
		up with a field filled with " -1.00E-09"

		With value=-0.005e-5, mode=0, w=12, d=2, e=4 we will end up
		with a field filled with " -5.00E-0008"

		With value=0.55, mode=0, w=8, d=2, e=0 we will end up
		with a field filled with "5.50E-01"

*/
long
*_sd2uese(	const void	*value,
		long		*fca,
		const long	*mode,
		const long	*w,
		const long	*d,
		const long	*e,
		const long	*p)
{
	long tmp = 1;
	return(_sd2udee(value,fca,mode,w,d,e,&tmp));
}
