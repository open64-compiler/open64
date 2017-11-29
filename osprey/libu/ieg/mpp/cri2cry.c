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


#pragma ident "@(#) libu/ieg/mpp/cri2cry.c	92.1	06/25/99 14:59:36"

#include <fortran.h>
#include <malloc.h>
#include <string.h>

extern	void	MOVBITZ(void *_A, long *_AZ, long *_N, void *_B, long *_BZ);
extern	void	_ITH64I(unsigned long _L, long *_LP);
extern	void	_ITF64I(unsigned long _L, long *_LP);
extern	void	_ITF128I(unsigned long _L, long *_LP);
extern	void	_ITSC128I(unsigned long _L, long *_LP);
extern	void	_ITSC256I(unsigned long _L, long *_LP);

/*
 *	The CRI2CRY subroutine converts Fortran data types on a CRAY
 *	system using IEEE (T3D) floating-point representation to a CRAY
 *	system using CRAY floating-point representation.
 *
 *	Call from Fortran:
 *
 *	INTEGER CRI2CRY
 *	IERR = CRI2CRY(TYPE, NUM, FORN, BITOFF, IEC, STRIDE, INTLEN,
 *			EXTLEN [, IECCH] )
 *
 *	The returned function value (IERR) is as follows:
 *
 *		-1	Parameter error; too few arguments or IECCH not
 *			specified with TYPE = 6.
 *		-2	Parameter error; invalid TYPE
 *		-3	Parameter error; invalid NUM
 *		-4	Parameter error; invalid BITOFF
 *		-5	Parameter error; invalid INTLEN
 *		-6	Parameter error; invalid EXTLEN
 *		-7	Unable to malloc() memory for translation
 *		-8	Combination of INTLEN and EXTLEN is invalid
 *		 0	Translation complete; no errors
 *		>0	Translation complete; return value is the
 *			number of values that overflowed during
 *			translation.
 *
 *	TYPE
 *
 *		  1 = typeless (no translation); INTLEN must equal
 *			EXTLEN and be 64, 128 or 256.
 *		  2 = integer (32- or 64-bit two's-complement to 64-bit
 *			two's-complement).
 *		  3 = real (32-, 64-, or 128-bit IEEE floating-point to
 *			64- or 128-bit CRAY floating-point).
 *		  4 = complex (2 x 32-, 64-, or 128-bit IEEE floating-
 *			point to 2 x 64- or 128-bit CRAY floating-point).
 *		  5 = logical (32- or 64-bit zero/nonzero logical to
 *			to 64-bit CRAY minus/positive logical).
 *		  6 = character (ASCII to ASCII; no translation).
 *
 *		The INTLEN and EXTLEN parameters select the size of the
 *		data.
 *
 *	NUM
 *
 *		Number of data items to convert.  Type integer variable,
 *		expression, or constant.
 *
 *	FORN
 *
 *		Variable or array of any type (except CHARACTER) or
 *		length to receive the converted data.
 *
 *	BITOFF
 *
 *		Bit offset within FORN to begin the conversion.  Type
 *		integer variable, expression, or constant.  Bits are
 *		numbered from 0, beginning at the leftmost bit of FORN.
 *
 *	IEC
 *
 *		Variable or array containing the values to be converted.
 *		This variable should be of a type corresponding to the
 *		TYPE parameter.
 *
 *	STRIDE
 *
 *		Integer parameter specifying a memory increment for
 *		loading the conversion results from IEC, in units of
 *		internal storage length.  The input bits are placed in
 *		FORN regardless of this parameter in a continuous bit
 *		stream.  For two- or four-word items, this is a stride
 *		of items, NOT of words.
 *
 *	INTLEN
 *
 *		Internal (iec) storage length, in bits.
 *
 *	EXTLEN
 *
 *		External (foreign) storage length, in bits.
 *
 *	IECCH
 *
 *		Optional CHARACTER parameter specifying IEC target
 *		variable if it is of type CHARACTER (TYPE = 6).  This
 *		parameter is ignored if type is not CHARACTER.
 */

/*
 *	Old name documented for users in UNICOS MAX 1.0.0.9
 */
#pragma _CRI duplicate CRI2CRY as IEC2CRAY 

_f_int
CRI2CRY(
	_f_int	*type,
	_f_int	*num,
	void	*forn,
	_f_int	*bitoff,
	void	*iec,
	_f_int	*stride,
	_f_int	*intlen,
	_f_int	*extlen,
	_fcd	iecch
)
{
	register int	bytes, count, fflg, i, ilen, incr, isize, j, osize;
	register long	*ilptr, *olptr;
	register short	*isptr;
	register char	*icptr, *ocptr;
	long		bits, zero;

	/* Validate arguments */

#ifdef	_CRAY
	if (_numargs() < 8)
		return((_f_int) -1);
#endif

	if (*type < 1 || *type > 6)
		return((_f_int) -2);

	if (*bitoff < 0 || *bitoff > 64)
		return((_f_int) -4);

	count	= *num;
	isize	= *intlen;
	osize	= *extlen;

	if (count < 0)
		return((_f_int) -3);

	if (count == 0)
		return((_f_int) 0);

	bits	= count * osize;
	bytes	= (bits + 07) >> 3;
	incr	= *stride << (isize >> 7);
	fflg	= 0;
	zero	= 0;

	/* Process data based on type */
  
	switch (*type) {

		case 1:		/* Typeless */
			if (isize != 64 && isize != 128 && isize != 256)
				return((_f_int) -5);

			if (isize != osize)
				return((_f_int) -8);

			if (incr == 1)
				MOVBITZ(iec, &zero, &bits, forn, (long *) bitoff);
			else {

				/* Get pointers to data */

				ilptr	= (long *) iec;

				if (*bitoff != 0) {	/* If not word aligned */
					olptr	= (long *) malloc(bytes);

					if (olptr == (long *) NULL)
						return((_f_int) -7);

					fflg	= 1;
				}
				else
					olptr	= (long *) forn;

				/* Move data */

				switch (osize) {

				case 64:
					for (i = 0; i < count; i++) {
						olptr[i]	= *ilptr;
						ilptr		= ilptr + incr;
					}
					break;

				case 128:
					for (i = 0, j = 0; i < count; i++) {
						olptr[j++]	= *ilptr;
						olptr[j++]	= *(ilptr + 1);
						ilptr		= ilptr + incr;
					}
					break;

				case 256:
					for (i = 0, j = 0; i < count; i++) {
						olptr[j++]	= *ilptr;
						olptr[j++]	= *(ilptr + 1);
						olptr[j++]	= *(ilptr + 2);
						olptr[j++]	= *(ilptr + 3);
						ilptr		= ilptr + incr;
					}
					break;

				} /* switch */
			}

			break;

		case 2:		/* Integer */
			if (isize != 32 && isize != 64)
				return((_f_int) -5);

			if (osize != 64)
				return((_f_int) -6);

			if (isize == osize && incr == 1)
				MOVBITZ(iec, &zero, &bits, forn, (long *) bitoff);
			else {

				/* Get pointers to data */

				if (*bitoff != 0) {	/* If not word aligned */
					olptr	= (long *) malloc(bytes);

					if (olptr == (long *) NULL)
						return((_f_int) -7);

					fflg	= 1;
				}
				else
					olptr	= (long *) forn;

				ilptr	= (long *) iec;
				isptr	= (short *) iec;

				/* Convert data */

				if (isize == 64)
					for (i = 0; i < count; i++) {
						olptr[i]	= *ilptr;
						ilptr		= ilptr + incr;
					}
				else	/* isize == 32 */
					for (i = 0; i < count; i++) {
						olptr[i]	= (long) *isptr;
						isptr		= isptr + incr;
					}
			}

			break;

		case 3:		/* Real */
			if (isize != 32 && isize != 64)
				return((_f_int) -5);	/* 128-bit not yet supported */

			if (osize != 64 && osize != 128)
				return((_f_int) -6);

			/* Get pointers to data */

			if (*bitoff != 0) {	/* If not word aligned */
				olptr	= (long *) malloc(bytes);

				if (olptr == (long *) NULL)
					return((_f_int) -7);

				fflg	= 1;
			}
			else
				olptr	= (long *) forn;

			ilptr	= (long *) iec;
			isptr	= (short *) iec;

			/* Convert data */

			if (isize == 32)
				if (osize == 64)
					for (i = 0; i < count; i++) {
						_ITH64I(*isptr, &olptr[i]);
						isptr	= isptr + incr;
					}
				else	/* isize == 32 && osize == 128 */
					for (i = 0, j = 0; i < count; i++) {
						_ITH64I(*isptr, &olptr[j++]);
						isptr		= isptr + incr;
						olptr[j++]	= 0;
					}
			else	/* isize == 64 */
				if (osize == 64)
					for (i = 0; i < count; i++) {
						_ITF64I(*ilptr, &olptr[i]);
						ilptr	= ilptr + incr;
					}
				else	/* isize == 64 && osize == 128 */
					for (i = 0; i < count; i++) {
						_ITF128I(*ilptr, &olptr[i]);
						ilptr	= ilptr + incr;
					}

			break;

		case 4:		/* Complex */
			if (isize != 64 && isize != 128)
				return((_f_int) -5);

			if (osize != 128 && osize != 256)
				return((_f_int) -6);

			/* Get pointers to data */

			if (*bitoff != 0) {	/* If not word aligned */
				olptr	= (long *) malloc(bytes);

				if (olptr == (long *) NULL)
					return((_f_int) -7);

				fflg	= 1;
			}
			else
				olptr	= (long *) forn;

			ilptr	= (long *) iec;
			isptr	= (short *) iec;

			/* Convert data */

			if (isize == 64)
				if (osize == 128)
					for (i = 0, j = 0; i < count; i++, j = j + 2) {
						_ITSC128I(*ilptr, &olptr[j]);
						ilptr	= ilptr + incr;
					}
				else	/* isize == 64 && osize == 256 */
					for (i = 0, j = 0; i < count; i++, j = j + 4) {
						_ITSC256I(*ilptr, &olptr[j]);
						ilptr	= ilptr + incr;
					}
			else	/* isize == 128 */
				if (osize == 128)
					for (i = 0, j = 0; i < count; i++) {
						_ITF64I( *ilptr,      &olptr[j++]);
						_ITF64I(*(ilptr + 1), &olptr[j++]);
						ilptr	= ilptr + incr;
					}
				else	/* isize == 128 && osize == 256 */
					for (i = 0, j = 0; i < count; i++, j = j + 4) {
						_ITF128I( *ilptr,      &olptr[j    ]);
						_ITF128I(*(ilptr + 1), &olptr[j + 2]);
						ilptr	= ilptr + incr;
					}

			break;

		case 5:		/* Logical */
			if (isize != 32 && isize != 64)
				return((_f_int) -5);

			if (osize != 64)
				return((_f_int) -6);

			/* Get pointers to data */

			if (*bitoff != 0) {	/* If not word aligned */
				olptr	= (long *) malloc(bytes);

				if (olptr == (long *) NULL)
					return((_f_int) -7);

				fflg	= 1;
			}
			else
				olptr	= (long *) forn;

			ilptr	= (long *) iec;
			isptr	= (short *) iec;

			/* Convert data */

			if (isize == 64)
				for (i = 0; i < count; i++) {
					olptr[i]	= (*ilptr == 0) ? 0 : -1;
					ilptr		= ilptr + incr;
				}
			else	/* isize == 32 */
				for (i = 0; i < count; i++) {
					olptr[i]	= (*isptr == 0) ? 0 : -1;
					isptr		= isptr + incr;
				}

			break;

		case 6:		/* Character */

#ifdef	_CRAY
			if (_numargs() < 9)
				return((_f_int) -1);
#endif

			if (isize != 8)
				return((_f_int) -5);

			if (osize != 8)
				return((_f_int) -6);

			icptr	= _fcdtocp(iecch);
			ilen	= _fcdlen (iecch);
			ocptr	= (char *) forn;
			bits	= bits * (ilen / 8);
			bytes	= bytes * ilen;

			if (incr == 1)
				(void) memcpy(ocptr, icptr, bytes);
			else
				for (i = 0; i < count; i++) {
					(void) memcpy(ocptr, icptr, ilen);
					ocptr	= ocptr + ilen;
					icptr	= icptr + (ilen * incr);
				}

			break;

	} /* switch */

	/* Free temporary storage, if allocated */

	if (fflg) {	/* If storage allocated */

		MOVBITZ(olptr, &zero, &bits, forn, (long *) bitoff);

		free(olptr);
	}

	return ((_f_int) 0);
}
