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


#pragma ident "@(#) libu/util/otb.c	92.1	07/07/99 13:18:33"

#include <fortran.h>

#define	ZERO	(short) ('0')
#define	SEVEN	(short) ('7')

#pragma _CRI duplicate OTB as _OTB

_f_int
OTB(_f_int *word, _f_int *flag)
{
	register long	error, result;
	register unsigned long mask, value;
	register short	ch, shift;

	value	= *word;
	mask	= 0xFF00000000000000;
	error	= 0;
	result	= 0;
	shift	= 64 - 8;

	do {
		ch	= (short) ((mask & value) >> shift);

		if (ch < ZERO || ch > SEVEN) {
			error	= -1;
			goto done;
		}

		result	= (result << 3) + (long) (ch - ZERO);
		value	= value & ~mask;
		shift	= shift - 8;
		mask	= mask >> 8;

	} while (value != 0);

done:

	if (error != 0)
		if (_numargs() > 1)
			*flag	= error;
		else
			abort();	/* TEMPORARY */

	return (result);
}
