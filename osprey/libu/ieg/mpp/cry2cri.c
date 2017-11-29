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


#pragma ident "@(#) libu/ieg/mpp/cry2cri.c	92.1	06/25/99 14:59:36"

#include <fortran.h>
#include <malloc.h>
#include <string.h>
#include <stddef.h>
#include <values.h>
#include <cray/portdefs.h>

#ifdef __mips
#define MOVBITZ _movbitz	/* prototyped in portdefs.h */
#else
extern	void	MOVBITZ(void *_A, long *_AZ, long *_N, void *_B, long *_BZ);
#endif
extern	void	_ITH64O(uint64 _L, int32 *_SP);
extern	void	_ITF64O(uint64 _L, int64 *_LP);
extern	void	_ITF128O(uint64 _L1, uint64 _L2, int64 *_LP);
extern	void	_ITSC128O(uint64 _L1, uint64 _L2, int32 *_LP);

#ifdef __mips
#define OT3D __ot3d
#else
#define OT3D O@T3D
#endif
/* Is our pointer aligned on an 8-byte boundary? */
#if defined(__mips) || defined(_CRAYMPP)
#define ALIGN64(a) (!((ptrdiff_t)a & 07))
#endif

extern	long	OT3D;

#define MASK8   0xFF
#define MASK16  0xFFFF
#define MASK32  0xFFFFFFFF

/*
 *	The CRY2CRI subroutine converts Fortran data types on a CRAY
 *	system using CRAY Y-MP floating-point representation to a CRAY
 *	system using IEEE (T3D) floating-point representation.
 *
 *	Call from Fortran:
 *
 *	INTEGER CRY2CRI
 *	IERR = CRY2CRI(TYPE, NUM, FORN, BITOFF, IEC, STRIDE, INTLEN,
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
 *		  2 = integer (64-bit two's-complement to 32- or 64-bit
 *			two's-complement).
 *		  3 = real (64-, or 128-bit CRAY floating-point to 32-,
 *			64- or 128-bit IEEE floating-point).
 *		  4 = complex (2 x 64-, or 128-bit CRAY floating-point
 *			to 2 x 32-, 64- or 128-bit IEEE floating-point).
 *		  5 = logical (64-bit CRAY minus/positive logical
 *			to 32- or 64-bit zero/nonzero logical).
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
 *		length containing the data to be converted.
 *
 *	BITOFF
 *
 *		Bit offset within FORN to begin the conversion.  Type
 *		integer variable, expression, or constant.  Bits are
 *		numbered from 0, beginning at the leftmost bit of FORN.
 *
 *	IEC
 *
 *		Variable or array to receive the converted values.
 *		This variable should be of a type corresponding to the
 *		TYPE parameter.
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
#ifndef __mips
#pragma _CRI duplicate CRY2CRI as CRAY2IEC
#endif

_f_int
#ifdef __mips
cry2cri_(
#else
CRY2CRI(
#endif
	_f_int	*type,
	_f_int8	*num,
	void	*forn,
	_f_int	*bitoff,
	void	*iec,
	_f_int	*stride,
	_f_int	*intlen,
	_f_int	*extlen,
#ifdef __mips
	char	*iecch,
	int	iecchlen
#else
	_fcd	iecch
#endif
)
{
	register int	fflg,  incr, isize, osize;
	long		count, bytes, i, j;
	size_t		olen;
	int64		*ilptr, *olptr;
	int32 		*osptr;
	register char	*icptr, *ocptr;
	long		bits, oflow, zero, lbitoff;

	/* Validate arguments */

#ifdef	_CRAY
	if (_numargs() < 8)
		return((_f_int) -1);
#endif

	if (*type < 1 || *type > 6)
		return((_f_int) -2);

	if (*bitoff < 0 || *bitoff > 64)
		return((_f_int) -4);
	if (*num < 0 || *num > MAXLONG)
		return((_f_int) -3);

	lbitoff = *bitoff;
	count	= *num;
	isize	= *intlen;
	osize	= *extlen;


	if (count == 0)
		return((_f_int) 0);

	oflow	= OT3D;
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
				MOVBITZ(forn, &lbitoff, &bits, iec, &zero);
			else {

				/* Get pointers to data */

				if ((lbitoff != 0)||(!ALIGN64(forn))) {	
					/* not word aligned */
					ilptr	= (int64 *) malloc(bytes);

					if (ilptr == (int64 *) NULL)
						return((_f_int) -7);

					fflg	= 1;

					MOVBITZ(forn, &lbitoff, &bits, ilptr, &zero);
				}
				else
					ilptr	= (int64 *) forn;

				olptr	= (int64 *) iec;

				/* Move data */

				switch (osize) {

				case 64:
					for (i = 0; i < count; i++) {
						*olptr	= ilptr[i];
						olptr	= olptr + incr;
					}
					break;

				case 128:
					for (i = 0, j = 0; i < count; i++) {
						*olptr		= ilptr[j++];
						*(olptr + 1)	= ilptr[j++];
						olptr		= olptr + incr;
					}
					break;

				case 256:
					for (i = 0, j = 0; i < count; i++) {
						*olptr		= ilptr[j++];
						*(olptr + 1)	= ilptr[j++];
						*(olptr + 2)	= ilptr[j++];
						*(olptr + 3)	= ilptr[j++];
						olptr		= olptr + incr;
					}
					break;

				} /* switch */
			}

			break;

		case 2:		/* Integer */
#ifdef __mips
			if (isize != 32 && isize != 64 && isize != 16 &&
				isize != 8)
#else
			if (isize != 32 && isize != 64)
#endif
				return((_f_int) -5);

			if (osize != 64)
				return((_f_int) -6);

			if (isize == osize && incr == 1)
				MOVBITZ(forn, &lbitoff, &bits, iec, &zero);
			else {
#ifdef __mips
				int64 iword;
#endif
				/* Get pointers to data */

				if ((lbitoff != 0)||(!ALIGN64(forn))) {	
					/* not word aligned */
					ilptr	= (int64 *) malloc(bytes);

					if (ilptr == (int64 *) NULL)
						return((_f_int) -7);

					fflg	= 1;

					MOVBITZ(forn, &lbitoff, &bits, ilptr, &zero);
				}
				else
					ilptr	= (int64 *) forn;

				olptr	= (int64 *) iec;
				osptr	= (int32 *) iec;

				/* Convert data */

				if (isize == 64)
					for (i = 0; i < count; i++) {
						*olptr	= ilptr[i];
						olptr	= olptr + incr;
					}
#ifdef __mips
				else if (isize == 16) {
					short *o16ptr;
					o16ptr = (short *)iec;
					for (i = 0; i < count; i++) {
						/* Check for overflow */
						iword = ilptr[i] & ~MASK16;
						if (iword!=0 && iword!=~MASK16)
							OT3D++;
						*o16ptr	= (short) ilptr[i];
						o16ptr	= o16ptr + incr;
					}
				}
				else if (isize == 8) {
					char *o8ptr;
					o8ptr = (char *)iec;
					for (i = 0; i < count; i++) {
						/* Check for overflow */
						iword = ilptr[i] & ~MASK8;
						if (iword!=0 && iword!=~MASK8)
							OT3D++;
						*o8ptr	= (char) ilptr[i];
						o8ptr	= o8ptr + incr;
					}
				}
#endif
				else	/* isize == 32 */
					for (i = 0; i < count; i++) {
#ifdef __mips
						/* Check for overflow */
						iword = ilptr[i] & ~MASK32;
						if (iword!=0 && iword!=~MASK32)
							OT3D++;
#endif
						*osptr	= (int32) ilptr[i];
						osptr	= osptr + incr;
					}
			}

			break;

		case 3:		/* Real */
			if (isize != 32 && isize != 64)
				return((_f_int) -5);	/* 128-bit not yet supported */

			if (osize != 64 && osize != 128)
				return((_f_int) -6);

			/* Get pointers to data */

			if ((lbitoff != 0)||(!ALIGN64(forn))) {	
				/* not word aligned */
				ilptr	= (int64 *) malloc(bytes);

				if (ilptr == (int64 *) NULL)
					return((_f_int) -7);

				fflg	= 1;

				MOVBITZ(forn, &lbitoff, &bits, ilptr, &zero);
			}
			else
				ilptr	= (int64 *) forn;

			olptr	= (int64 *) iec;
			osptr	= (int32 *) iec;

			/* Convert data */

			if (isize == 32)
				if (osize == 64)
					for (i = 0; i < count; i++) {
						_ITH64O(ilptr[i], osptr);
						osptr	= osptr + incr;
					}
				else	/* isize == 32 && osize == 128 */
					/*
					 * N.B. We ignore the second word of the CRAY
					 * DOUBLE PRECISION value.
					 */
					for (i = 0, j = 0; i < count; i++, j = j + 2) {
						_ITH64O(ilptr[j], osptr);
						osptr	= osptr + incr;
					}
			else	/* isize == 64 */
				if (osize == 64)
					for (i = 0; i < count; i++) {
						_ITF64O(ilptr[i], olptr);
						olptr	= olptr + incr;
					}
				else	/* isize == 64 && osize == 128 */
					for (i = 0, j = 0; i < count; i++, j = j + 2) {
						_ITF128O(ilptr[j], ilptr[j + 1], olptr);
						olptr	= olptr + incr;
					}
			break;

		case 4:		/* Complex */
			if (isize != 64 && isize != 128)
				return((_f_int) -5);

			if (osize != 128 && osize != 256)
				return((_f_int) -6);

			/* Get pointers to data */

			if ((lbitoff != 0)||(!ALIGN64(forn))) {	
				/* not word aligned */
				ilptr	= (int64 *) malloc(bytes);

				if (ilptr == (int64 *) NULL)
					return((_f_int) -7);

				fflg	= 1;

				MOVBITZ(forn, &lbitoff, &bits, ilptr, &zero);
			}
			else
				ilptr	= (int64 *) forn;

			olptr	= (int64 *) iec;
			osptr	= (int32 *) iec;

			/* Convert data */

			if (isize == 64)
				if (osize == 128)
					for (i = 0, j = 0; i < count; i++, j = j + 2) {
						/* Converting 2x64 to 2x32 */
						_ITSC128O(ilptr[j], ilptr[j + 1], osptr);
						osptr	= osptr + 2*incr;
					}
				else	/* isize == 64 && osize == 256 */
					/*
					 * N.B. We ignore the second word of the CRAY
					 * DOUBLE COMPLEX values.
					 * Converting 2x128 to 2x32
					 */
					for (i = 0, j = 0; i < count; i++, j = j + 4) {
						_ITSC128O(ilptr[j], ilptr[j + 2], osptr);
						osptr	= osptr + 2*incr;
					}
			else	/* isize == 128 */
				if (osize == 128)
					for (i = 0, j = 0; i < count; i++, j = j + 2) {
						_ITF64O(ilptr[    j], olptr);
						_ITF64O(ilptr[j + 1], olptr + 1);
						olptr	= olptr + incr;
					}
				else	/* isize == 128 && osize == 256 */
					for (i = 0, j = 0; i < count; i++, j = j + 4) {
						_ITF128O(ilptr[    j], ilptr[j + 1], olptr);
						_ITF128O(ilptr[j + 2], ilptr[j + 3], olptr + 1);
						olptr	= olptr + incr;
					}

			break;

		case 5:		/* Logical */
#ifdef __mips
			if (isize != 32 && isize != 64 && isize != 8 && 
				isize != 16)
#else
			if (isize != 32 && isize != 64)
#endif
				return((_f_int) -5);

			if (osize != 64)
				return((_f_int) -6);

			/* Get pointers to data */

			if ((lbitoff != 0)||(!ALIGN64(forn))) {	
				/* not word aligned */
				ilptr	= (int64 *) malloc(bytes);

				if (ilptr == (int64 *) NULL)
					return((_f_int) -7);

				fflg	= 1;

				MOVBITZ(forn, &lbitoff, &bits, ilptr, &zero);
			}
			else
				ilptr	= (int64 *) forn;

			olptr	= (int64 *) iec;
			osptr	= (int32 *) iec;

			/* Convert data */

			if (isize == 64)
				for (i = 0; i < count; i++) {
					*olptr	= (ilptr[i] < 0) ? 1 : 0;
					olptr	= olptr + incr;
				}
#ifdef __mips
			else if (isize == 16) {
				short *o16ptr;
				o16ptr = (short *)iec;
				for (i = 0; i < count; i++) {
					*o16ptr	= (ilptr[i] < 0) ? 1 : 0;
					o16ptr	= o16ptr + incr;
				}
			}
			else if (isize == 8) {
				char *ocptr;
				ocptr = (char *)iec;
				for (i = 0; i < count; i++) {
					*ocptr	= (ilptr[i] < 0) ? 1 : 0;
					ocptr	= ocptr + incr;
				}
			}
#endif
			else	/* isize == 32 */
				for (i = 0; i < count; i++) {
					*osptr	= (ilptr[i] < 0) ? 1 : 0;
					osptr	= osptr + incr;
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

#ifdef __mips
			ocptr	= iecch;
			olen	= iecchlen;
#else
			ocptr	= _fcdtocp(iecch);
			olen	= _fcdlen (iecch);
#endif
			bytes	= bytes * olen;
			icptr	= (char *) forn;

			if (incr == 1)
				(void) memcpy(ocptr, icptr, bytes);
			else
				for (i = 0; i < count; i++) {
					(void) memcpy(ocptr, icptr, olen);
					ocptr	= ocptr + (incr * olen);
					icptr	= icptr + olen;
				}

			break;

	} /* switch */

	/* Free temporary storage, if allocated */

	if (fflg)	/* If storage allocated */
		free(ilptr);

	return ((_f_int) (OT3D - oflow));
}
