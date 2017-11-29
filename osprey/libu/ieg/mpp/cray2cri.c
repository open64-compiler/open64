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


#pragma static "@(#) libu/ieg/mpp/cray2cri.c	92.1	06/25/99 14:59:36"

#include <ffio.h>
#include <fortran.h>
#include <cray/dopevec.h>
#include <cray/fndc.h>

#define	DT_MAX	9	/* Maximum Fortran 77 data type */

extern	ftype_t	_f77_to_f90_type_cnvt[DT_MAX];
extern	short	_f77_type_len[DT_MAX];

_f_int
CRY2CRI(_f_int *type, _f_int *num, void *forn, _f_int *bitoff,
	void *native, _f_int *stride, _f_int *intlen, _f_int *extlen, ...);

/*
 *	The CRAY2CRI subroutine converts Fortran data types on a CRAY
 *	system using CRAY floating-point representation to a CRAY
 *	system using IEEE (T3D) floating-point representation.
 *
 *	Call from Fortran:
 *
 *	INTEGER CRAY2CRI
 *
 *	IERR = CRAY2CRI(TYPE, NUM, FORN, BITOFF, NATIVE [, STRIDE, [, NATIVECH]] )
 *
 *	The returned function value (IERR) is as follows:
 *
 *		-2	Parameter error; invalid TYPE
 *		-3	Parameter error; invalid NUM
 *		-4	Parameter error; invalid BITOFF
 *		-7	Unable to malloc() memory for translation
 *		 0	Translation complete; no errors
 *		>0	Translation complete; return value is the
 *			number of values that overflowed during
 *			translation.
 *
 *	TYPE
 *
 *		  0 = typeless (no translation).
 *		  1 = integer (64 bit)
 *		  2 = real (64 bit)
 *		  3 = double precision (128 bit)
 *		  4 = complex (2 x 64 bit)
 *		  5 = logical (64 bit)
 *		  6 = character (ASCII to ASCII; no translation).
 *		  7 = short integer (64 bit container size)
 *
 *
 *	NUM
 *
 *		Number of data items to convert.  Type integer variable,
 *		expression, or constant.
 *
 *	FORN
 *
 *		Variable or array of any type (except CHARACTER) or
 *		length containing the data to be converted.
 *
 *	BITOFF
 *
 *		Bit offset within FORN to begin the conversion.  Type
 *		integer variable, expression, or constant.  Bits are
 *		numbered from 0, beginning at the leftmost bit of FORN.
 *
 *	NATIVE
 *
 *		Variable or array to receive the converted values.
 *		This variable should be of a type corresponding to the
 *		TYPE parameter.
 *
 *	STRIDE
 *
 *		Integer parameter specifying a memory increment for
 *		storing the conversion results in NATIVE, in units of
 *		internal storage length.  The input bits are taken from
 *		FORN regardless of this parameter in a continuous bit
 *		stream.  For two- or four-word items, this is a stride
 *		of items, NOT of words.
 *
 *	NATIVECH
 *
 *		Optional CHARACTER parameter specifying NATIVE target
 *		variable if it is of type CHARACTER (TYPE = 6).  This
 *		parameter is ignored if type is not CHARACTER.
 */

_f_int
CRAY2CRI(
	_f_int	*type,
	_f_int	*num,
	void	*forn,
	_f_int	*bitoff,
	void	*native,
	_f_int	*stride,
	_fcd	nativech
)
{
	_f_int	extlen;
	_f_int	ierr;
	_f_int	intlen;
	_f_int	strd;
	_f_int	type77;
	_f_int	type90;

	type77	= *type;
	type90	= _f77_to_f90_type_cnvt[type77];
	intlen	= _f77_type_len[type77] << 3;			/* in bits */
	extlen	= __fndc_f77sz[NCV_CRAY]->typlen[type77];	/* in bits */
	strd	= (_numargs() > 5) ? *stride : 1;

	if (_numargs() > 6)
		ierr	= CRY2CRI(&type90, num, forn, bitoff, native, &strd,
				&intlen, &extlen, nativech);
	else
		ierr	= CRY2CRI(&type90, num, forn, bitoff, native, &strd,
				&intlen, &extlen);

	return (ierr);
}
