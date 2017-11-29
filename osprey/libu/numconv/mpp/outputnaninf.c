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



#pragma ident "@(#) libu/numconv/mpp/outputnaninf.c	92.1	06/28/99 15:00:40"

#ifdef	_UNICOS
#include <fp.h>
#endif
#include <cray/portdefs.h>
#include <cray/fmtconv.h>

/*
        General:
		Right justify fill an output field with NaN, Infinity, 
		+Infinity, or -Infinity. 

        Inputs:
		datum - NaN or [+-]Infinity
		position_pointer - first character address of field
		mode - flag to determine sign behavior for Infinity
			0 = only minus sign required
			1 = sign required
		field_width - field width

        Outputs:
		datum - unchanged
		position_pointer - unchanged
		mode - unchanged
		field_width - unchanged

        Examples:
		called with datum = NaN, mode = 1, and field_width = 5
		will result in a field of "  NaN"

		called with datum = Infinity, mode = 1, and field_width = 5
		will result in a field of "+Infi"

		called with datum = -Infinity, mode = 0, and field_width = 9
		will result in a field of "-Infinity"

		called with datum = Infinity, mode = 0, and field_width = 10
		will result in a field of "  Infinity"

*/
long
_OUTPUT_NAN_INF(const double	datum,
		long		*position_pointer,
		const long	mode,
		const long	field_width)
{
	union {double d; uint64 i;}	tmp;
	long	*string;
	long	*end_position;
	long	strsize;
	long	sign;
	static	long	NaN_str[] = {'N','a','N'};
	static	long	Infinity_str[] = {'I','n','f','i','n','i','t','y'};

/*
	determine if NaN or Infinity
*/
	sign = ' ';
	if (isnan(datum)) {
		strsize = 3;
		string = NaN_str;
	}
	else {
		strsize = 8;
		string = Infinity_str;
		if ((mode & MODESN) != 0) sign = '+';
		tmp.d = datum;
		if ((tmp.i >> 63) == 1) sign = '-';
		if (sign != ' ') strsize++;
	};

/*
	output appropriate character string right justified - in case the 
	string does not fit the field, then output the leftmost portion
	of the string
*/
	end_position = position_pointer + field_width;
	while (position_pointer < (end_position - strsize))
		*position_pointer++ = ' ';
	if (sign != ' ') *position_pointer++ = sign;
	while (position_pointer < end_position)
		*position_pointer++ = *string++;
	return(0);
}
