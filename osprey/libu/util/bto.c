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


#pragma ident "@(#) libu/util/bto.c	92.1	07/07/99 13:18:33"

#include <fortran.h>

#define	BLANK	(short) (' ')
#define	ZERO	(short) ('0')

#pragma _CRI duplicate BTO as _BTO

_f_int
BTO(_f_int *word)
{
	register long	result, value;
	register short	ch, i;

	value	= *word & 077777777;

	result	= (value & 07) + ZERO;
	value	= value >> 3;

	for (i = 1; i < 8; i++) {

		if (value == 0)
			ch	= BLANK;
		else {
			ch	= (value & 07) + ZERO;
			value	= value >> 3;
		}

		result	= result | (ch << (i << 3));

	}

	return (result);
}

#pragma _CRI duplicate BTOL as _BTOL

_f_int
BTOL(_f_int *word)
{
	register long	result, value;
	register short	ch, i, shift;

	value	= *word & 077777777;

	result	= (value & 07) + ZERO;
	value	= value >> 3;
	shift	= 8;

	while (value != 0) {

		ch	= (value & 07) + ZERO;
		value	= value >> 3;
		result	= result | (ch << shift);
		shift	= shift + 8;

	}

	result	= result << (64 - shift);

	return (result);
}

#pragma _CRI duplicate BTOR as _BTOR

_f_int
BTOR(_f_int *word)
{
	register long	result, value;
	register short	ch, i, shift;

	value	= *word & 077777777;

	result	= (value & 07) + ZERO;
	value	= value >> 3;
	shift	= 0;

	while (value != 0) {

		ch	= (value & 07) + ZERO;
		value	= value >> 3;
		shift	= shift + 8;
		result	= result | (ch << shift);

	}

	return (result);
}
