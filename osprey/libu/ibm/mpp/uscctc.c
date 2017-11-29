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


static const char USMID[] = "@(#) libu/ibm/mpp/uscctc.c	92.0	10/08/98 14:57:41";
#include <fortran.h>
#include <stdlib.h>
#include <cray/portdefs.h>

extern uint64	_EBCDIC_TO_ASCII[32];
extern uint64	_EBCDIC_TO_ASCII_UC[32];

/*
 *	USCCTC	Fortran-callable routine to convert IBM EBCDIC
 *		character data to ASCII character data.
 *
 *	CALL USCCTC (SRC, ISB, DEST, NUM, NPW <,VAL>)
 *
 *	SRC	Variable or array of any type (except CHARACTER)
 *		containing the character data to be converted.
 *
 *	ISB	Integer variable or constant containing the starting
 *		byte number within SRC.  Bytes are numbered from left
 *		to right with the leftmost byte as byte 1 and the
 *		rightmost byte as byte 8.
 *
 *	DEST	Variable or array of any type (except CHARACTER)
 *		to receive the converted character data.  There are
 *		NPW converted characters per word.
 *
 *	NUM	Integer variable or constant containing the number
 *		of characters to be converted.
 *
 *	NPW	Integer variable or constant containing the number of
 *		characters per word in the output array.  If NPW is
 *		positive (1 to 8), NPW characters are placed in each
 *		DEST word, left-justified and blank-filled.  If NPW
 *		is negative (-1 to -8), -NPW characters are placed in
 *		each DEST word, right-justified and zero-filled.
 *
 *	VAL	(Optional parameter) Integer variable or constant
 *		which--if specified and nonzero (.TRUE.)--indicates
 *		that all lower case input is to be translated to upper
 *		case output.
 *
 *	Note that the following conditions must be met for any character
 *	data to be converted:
 *		NUM >= 0
 *		ISB > 0
 *		0 < |NPW| < 9
 */

#if	defined(__mips)
_f_int8
uscctc_(
#else
_f_int8
USCCTC(
#endif
	_f_int8	*src,
	_f_int8	*isb,
	_f_int8	*dest,
	_f_int8	*num,
	_f_int8	*npw,
	_f_int8	*val)
{
	register short	bytes;
	register short	ishft;
	register short	oshft;
	register short	lrflag;
	register int64	count;
	register uint64	input;
	register uint64	output;
#if	defined(__mips)
	const uint64	blanks = 0x2020202020202020L;
#else
	const uint64	blanks = 0x2020202020202020;
#endif
	uint64		*in;
	uint64		*out;
	uint64		*table;

	table	= _EBCDIC_TO_ASCII;

#ifdef	_CRAY
	if (_numargs() < 5)
		return((_f_int8) -1);
	else
		if (_numargs() > 5)
#endif
			if (val != NULL && *val != 0)
				table	= _EBCDIC_TO_ASCII_UC;

	count	= *num;

	if (*npw == 0 || *npw > 8 || *npw < -8 || *isb < 1 || count < 0)
		return((_f_int8) -1);

	if (count == 0)
		return((_f_int8) 0);

	lrflag	= (*npw < 0) ? 0 : 1;
	bytes	= (lrflag) ? *npw : -*npw;
	bytes	= (8 - bytes) << 3;
	ishft	= *isb - 1;
	out	= (uint64 *) dest;
	in	= (uint64 *) src + (ishft >> 3);
	ishft	= (8 - (ishft & 07)) << 3;
	input	= *in++;
	oshft	= 64;
	output	= 0;

	do {
		register short	ch;

		ishft	= ishft - 8;
		ch	= (input >> ishft) & 0377;
		ch	= (table[ch >> 3] >> ((ch & 07) << 3)) & 0377;
		output	= (output << 8) | ch;
		count	= count - 1;

		if (ishft == 0) {
			input	= *in++;
			ishft	= 64;
		}

		oshft	= oshft - 8;

		if (oshft == bytes) {
			output	= _dshiftl(output, blanks, oshft * lrflag);
			*out++	= output;
			oshft	= 64;
			output	= 0;
		}

	} while (count > 0);

	if (oshft != 64) {	/* If partial word available */
		output	= _dshiftl(output, blanks, oshft * lrflag);
		*out	= output;
	}

	return((_f_int8) 0);
}
