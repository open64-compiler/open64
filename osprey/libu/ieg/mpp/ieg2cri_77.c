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


#pragma ident "@(#) libu/ieg/mpp/ieg2cri_77.c	92.1	06/25/99 14:59:36"

#include <ffio.h>
#include <fortran.h>
#include <cray/dopevec.h>
#include <cray/fndc.h>

#define DT_SPECREAL	8	/* 64-bit to 64-bit real conversion */
#define DT_MAX		9	/* Maximum Fortran 77 data type */

extern	ftype_t	_f77_to_f90_type_cnvt[DT_MAX];
extern	short	_f77_type_len[DT_MAX];

extern _f_int
IEG2CRI(_f_int *type, _f_int *num, void *forn, _f_int *bitoff, void *iec,
	_f_int *stride, _f_int *intlen, _f_int *extlen, _fcd iecch);

/*
 *	The IEG2CRI_77 subroutine converts Fortran data types on a system
 *	using generic 32 bit IEEE floating point representation to a CRAY
 *	system using IEEE (T3D) floating-point representation.
 *
 *	Call from Fortran:
 *
 *	INTEGER IEG2CRI_77
 *
 *	IERR = IEG2CRI_77(TYPE, NUM, FORN, BITOFF, IEC, STRIDE, [, IECCH] )
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
 *								Size in Bits
 *
 *								CRI	IEG
 *		  0 = typeless (no translation).		64	64
 *		  1 = integer (64 bit)				64	32
 *		  2 = real (64 bit)				64	32
 *		  3 = double precision 	<deferred>		128	64
 *		  4 = complex 					2x64	2x32
 *		  5 = logical					64	32
 *		  6 = character (ASCII to ASCII; no conversion) 8	8
 *		  7 = short integer 				64	16
 *		  8 = special real 				64	64
 *
 *
 *	NUM
 *
 *		Number of data items to convert.  Type integer variable,
 *		expression, or constant.
 *
 *	FORN
 *
 *		T3D systems:  	Variable or array of any type (except CHARACTER)
 *				or length containing the 32 bit IEEE data to 
 *				be converted.
 *
 *	BITOFF
 *
 *		Bit offset within FORN to begin the conversion.  Type
 *		integer variable, expression, or constant.  Bits are
 *		numbered from 0, beginning at the leftmost bit of FORN.
 *
 *	NATIVE
 *
 *
 *		T3D systems:  	Variable or array to receive the converted
 *				values if type is not CHARACTER.
 *
 *		This variable contains the target Cray MPP systems array or 
 *		32 bit IEEE systems data to be converted,  whichever is
 *		native to the running system, for noncharacter types.
 *
 *		This variable should be of a type corresponding to the
 *		TYPE parameter.  This array is strided in accordance with
 *		the STRIDE argument.
 *
 *	STRIDE
 *
 *		Integer parameter specifying a memory increment for
 *		storing the conversion results in IEC, in units of
 *		internal storage length.  The input bits are taken from
 *		FORN regardless of this parameter in a continuous bit
 *		stream.  For two- or four-word items, this is a stride
 *		of items, NOT of words.
 *
 *	NATIVECH
 *
 *		Optional variable contains the target Cray MPP systems array or 
 *		32 bit IEEE systems data to be converted,  whichever is
 *		native to the running system, for CHARACTER types.
 *
 *		This parameter is ignored and may be omitted if type is not
 *		CHARACTER.
 */

_f_int
IEG2CRI_77(
	_f_int	*type,
	_f_int	*num,
	void	*forn,
	_f_int	*bitoff,
	void	*native,
	_f_int	*stride,
	_fcd	nativech
)
{
	_f_int	type77 = *type;
	_f_int	type90;
	_f_int	intlen;
	_f_int	extlen;

	if (type77 == DT_SPECREAL) {
		type90	= DVTYPE_REAL;
		intlen	= 64;
	}
	else {
		type90	= _f77_to_f90_type_cnvt[type77];
		intlen	= _f77_type_len[type77] << 3;	/* in bits */
	}

	extlen	= __fndc_f77sz[NCV_IEG]->typlen[type77];	/* in bits */

	return( IEG2CRI(&type90, num, forn, bitoff, native, stride, &intlen,
		&extlen, nativech) );
}
