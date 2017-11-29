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


#pragma ident "@(#) libu/ieg/mpp/ieg2cri.c	92.1	06/25/99 14:59:36"

#include <fortran.h>
#include <malloc.h>
#include <string.h>

extern	void	MOVBITZ(void *_A, long *_AZ, long *_N, void *_B, long *_BZ);
extern	int	_IEG2CRI_16I64_ONCRI( \
		long *ilptr, long *olptr, int *numadd, int *strideadd);
extern	int	_CRI2IEG_64I16_ONCRI( \
		long *ilptr, long *olptr, int *numadd, int *strideadd);
extern	int	_IEG2CRI_16I32_ONCRI( \
		short *isptr, short *osptr, int *numadd, int *strideadd);
extern	int	_CRI2IEG_32I16_ONCRI( \
		short *isptr, short *osptr, int *numadd, int *strideadd);

extern	long	O@T3D;

/*
 * 11/94    nealg  Based on cry2cri.c
   02/15/95 nealg  Added integer*2   16->64 (pvp f90 & all f77). 
            PVP f77 uses a 64 bit internal "container" for *2,*4.
            PVP f90 uses a 64 bit internal "container" for *2,*4.
   02/21/95 nealg  Added integer*2/4 16->32 (mpp f90). 
            MPP f90 uses a 32 bit internal "container" for *2,*4.
            Note that the MPP Fortran 90 implicit interface does not yet support
            integer*2 IEG conversion, though it is supported by IEG2CRI/CRI2IEG,
	    because it uses IEG2CRI_77/CRI2IEG_77 and type 7 specifies 16<->64.
 */

/*
 *	The IEG2CRI subroutine converts Fortran data types on a CRAY
 *	system using IEEE (T3D) floating-point representation to a CRAY
 *	system using IEEE floating-point representation.
 *
 *	Call from Fortran:
 *
 *	INTEGER IEG2CRI
 *	IERR = IEG2CRI(TYPE, NUM, FORN, BITOFF, IEC, STRIDE, INTLEN,
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
 *		  2 = integer (16- or 32-bit two's-complement to 32- or
 *			64-bit two's-complement).
 *			Integer*2/4 are 64 bits internal with all f77.
 *			Integer*2/4 are 64 bits internal with pvp f90.
 *			Integer*2/4 are 32 bits internal with mpp f90.
 *		  3 = real (32-, or 64-bit IEEE floating-point to 32-,
 *			or 64-bit IEEE (T3D) floating-point).
 *		  4 = complex (2 x 32-, or 64-bit IEEE floating-point
 *			to 2 x 32-, or 64-bit IEEE (T3D) floating-point).
 *		  5 = logical ( 32-bit zero/nonzero logical
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
 *
 *	Note:	Overflow and Inexact conversion cannot occur, since the result 
 *		is least as large as the input.  Underflow of a denormal to zero
 *		can occur, but we do not yet set IEEE flags.
 */

_f_int
IEG2CRI(
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
	register int	bytes, fflg, i, isize, j, olen, osize;
	int		count, incr;
	register long	*ilptr, *olptr;
	register short	*osptr, *isptr;
	register char	*icptr, *ocptr;
	long		bits, oflow, zero;

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

	oflow	= O@T3D;
	bits	= count * osize;
	bytes	= (bits + 07) >> 3;
	incr	= *stride << (isize >> 7);
	fflg	= 0;
	zero	= 0;

	/* Process data based on type */
  
	switch (*type) {

		case 1:		/* Typeless */
			/* isize is internal(CRI) size, osize is IEG size */
			if (isize != 64 && isize != 128 && isize != 256)
				return((_f_int) -5);

			if (isize != osize)
				return((_f_int) -8);

			if (incr == 1 )
				/*	source=forn(bitoff:), dest=iec(0:) */
				MOVBITZ(forn, (long *) bitoff, &bits, iec, &zero);
			else {

				/* Get pointers to data */

				if (*bitoff != 0) {	/* If not word aligned */
					ilptr	= (long *) malloc(bytes);

					if (ilptr == (long *) NULL)
						return((_f_int) -7);

					fflg	= 1;

					MOVBITZ(forn, (long *) bitoff, &bits, ilptr, &zero);
				}
				else
					ilptr	= (long *) forn;

				olptr	= (long *) iec;

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

                        /* 64<-64/32/16 and 32<-32/16. */
                        /* integer*2/4 are internally in a larger, sign
                           extended variable called a "container".  */
                        /* cft77:     integer*2/4 are 64 bits internal */
                        /* cft90 pvp: integer*2/4 are 64 bits internal */
                        /* cft90 mpp: integer*2/4 are 32 bits internal */

			/* isize is internal(CRI) size, osize is IEG size */
			if (isize != 32 && isize != 64)
				return((_f_int) -5);

			if (osize != 16 && osize != 32 )
				return((_f_int) -6);
			if (osize > isize )
				return((_f_int) -8);

			if (isize == osize && incr == 1)
				MOVBITZ(forn, (long *) bitoff, &bits, iec, &zero);
			else {

				/* Get pointers to data */

				if (*bitoff != 0) {	/* If not word aligned */
					ilptr	= (long *) malloc(bytes);

					if (ilptr == (long *) NULL)
						return((_f_int) -7);

					fflg	= 1;

					MOVBITZ(forn, (long *) bitoff, &bits, ilptr, &zero);
				}
				else
					ilptr	= (long *) forn;

				isptr	= (short *) ilptr;
				olptr	= (long *) iec;
				osptr	= (short *) iec;
			/* i.ptr is the input (IEG), o.ptr is output(CRI) */
			/* isize is internal(CRI) size, osize is IEG size */

				/* Convert data */

				if (isize == 64) {
					if (osize == 32) _IEG2CRI_32I64_ONCRI(
						isptr, olptr, &count, &incr ) ;
				else	if (osize == 64)
						for (i = 0; i < count; i++) {
							*olptr	= ilptr[i];
							olptr	= olptr + incr;
                                       		 }
					else /* o == 16*/ _IEG2CRI_16I64_ONCRI(
						ilptr, olptr, &count, &incr ) ;
                                }
				else {  /* isize == 32 */
					if (osize == 32)
						for (i = 0; i < count; i++) {
							* osptr	= isptr[i];
							osptr	= osptr + incr;
						}
					else /* 16->32*/ _IEG2CRI_16I32_ONCRI(
						isptr, osptr, &count, &incr ) ;
                                }

			}

			break;

		case 3:		/* Real */
			/* isize is internal(CRI) size, osize is IEG size */
			if (isize != 32 && isize != 64)
				return((_f_int) -5);	/* 128-bit not yet supported */

			if (osize != 64 && osize != 32)
				return((_f_int) -6);

                       /* allow double-size or identical output */
                                if (( osize != isize )&&( isize != (2* osize)))
                                         return((_f_int) -8);

			/* Get pointers to data */

			if (*bitoff != 0) {	/* If not word aligned */
				ilptr	= (long *) malloc(bytes);

				if (ilptr == (long *) NULL)
					return((_f_int) -7);

				fflg	= 1;

				MOVBITZ(forn, (long *) bitoff, &bits, ilptr, &zero);
			}
			else
				ilptr	= (long *) forn;

			isptr	= (short *) ilptr;
			olptr	= (long *) iec;
			osptr	= (short *) iec;
			/* i.ptr is the input (IEG), o.ptr is output(CRI) */
			/* isize is internal(CRI) size, osize is IEG size */

			/* Convert data */

			if (isize == 64)
				if (osize == 32) _IEG2CRI_32R64_ONCRI(
                                        isptr, olptr, &count, &incr ) ;
				else	/* isize == 64, osize==64  */
					_IEG2CRI_64R64_ONCRI(
                                        	ilptr, olptr, &count, &incr ) ;
			else	/* isize == 32, osize == 32 */
				_IEG2CRI_32R32_ONCRI(
                                        isptr, osptr, &count, &incr ) ;
			break;

		case 4:		/* Complex */
			/* isize is internal(CRI) size, osize is IEG size */
			if (isize != (2*32) && isize != (2*64))
				return((_f_int) -5);

			if (osize != (2*32) && osize != (2*64))
				return((_f_int) -6);

                       /* allow double-size or identical output */
                                if (( osize != isize )&&( isize != (2* osize)))
                                         return((_f_int) -8);

			/* Get pointers to data */

			if (*bitoff != 0) {	/* If not word aligned */
				ilptr	= (long *) malloc(bytes);

				if (ilptr == (long *) NULL)
					return((_f_int) -7);

				fflg	= 1;

				MOVBITZ(forn, (long *) bitoff, &bits, ilptr, &zero);
			}
			else
				ilptr	= (long *) forn;

			isptr	= (short *) ilptr;
			olptr	= (long *) iec;
			osptr	= (short *) iec;
			/* i.ptr is the input (IEG), o.ptr is output(CRI) */
			/* isize is internal(CRI) size, osize is IEG size */

			/* Convert data */

			if (isize == (2*64))
				if (osize == (2*32)) _IEG2CRI_32C64_ONCRI(
                                        isptr, olptr, &count, &incr ) ;
				else	/* isize == 2*64, osize==2*64  */
					_IEG2CRI_64C64_ONCRI(
                                        	ilptr, olptr, &count, &incr ) ;
			else	/* isize == (2*32), osize == (2*32) */
				_IEG2CRI_32C32_ONCRI(
                                        isptr, osptr, &count, &incr ) ;
			break;

		case 5:		/* Logical */
			/* isize is internal(CRI) size, osize is IEG size */
			if (isize != 32 && isize != 64)
				return((_f_int) -5);

			if (osize != 32)
				return((_f_int) -6);

			/* Get pointers to data */

			if (*bitoff != 0) {	/* If not word aligned */
				ilptr	= (long *) malloc(bytes);

				if (ilptr == (long *) NULL)
					return((_f_int) -7);

				fflg	= 1;

				MOVBITZ(forn, (long *) bitoff, &bits, ilptr, &zero);
			}
			else
				ilptr	= (long *) forn;

			isptr	= (short *) ilptr;
			olptr	= (long *) iec;
			osptr	= (short *) iec;
			/* i.ptr is the input (IEG), o.ptr is output(CRI) */
			/* isize is internal(CRI) size, osize is IEG size */

			/* Convert data */

			if (isize == 64)
				for (i = 0; i < count; i++) {
					*olptr	= (isptr[i] != 0) ? 1 : 0;
					olptr	= olptr + incr;
				}
			else	/* isize == 32 */
				for (i = 0; i < count; i++) {
					*osptr	= (isptr[i] != 0) ? 1 : 0;
					osptr	= osptr + incr;
				}

			break;

		case 6:		/* Character */
			/* isize is internal(CRI) size, osize is IEG size */

#ifdef	_CRAY
			if (_numargs() < 9)
				return((_f_int) -1);
#endif

			if (isize != 8)
				return((_f_int) -5);

			if (osize != 8)
				return((_f_int) -6);

			ocptr	= _fcdtocp(iecch);
			olen	= _fcdlen (iecch);
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

	return ((_f_int) 0);	/* No overflow */
}
