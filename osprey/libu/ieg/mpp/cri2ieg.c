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


#pragma ident "@(#) libu/ieg/mpp/cri2ieg.c	92.1	06/25/99 14:59:36"
/*
 * This routine is for writing with "assign -N ieee_dp" or
 * "assign -N ieee" (DP deferred) on CRAY systems using IEEE-64.
 * It performs copies and size conversions assuming valid Cray
 * IEEE data (no denormals).  When size is reduced, overflow is
 * detected and floating point denormals may be generated. 
 * See IEG2CRI for further documentation.

 * The IEEE flags for overflow, underflow and inexact are not set.

 * nealg 10/31/94  Original based on cri2cry.
 * nealg  2/14/95  Add 64--16 integer conversion (f77 integer*2),
 *                 and misc comments.
 * nealg  2/21/95  Add 64--32 integer conversion (F90 integer*2)
 */

#include <fortran.h>
#include <malloc.h>
#include <string.h>

extern	void	MOVBITZ(void *_A, long *_AZ, long *_N, void *_B, long *_BZ);
extern	long	O@T3D ;
/*
 *	The CRI2IEG subroutine converts Fortran data types on a CRAY
 *	system using IEEE (T3D) floating-point representation to
 *	IEEE 32 bit (single) floating-point representation.
 *
 *	Call from Fortran:
 *
 *	INTEGER CRI2IEG
 *	IERR = CRI2IEG(TYPE, NUM, FORN, BITOFF, IEC, STRIDE, INTLEN,
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
 *		  2 = integer (32- or 64-bit two's-complement to 32-bit
 *			or 16-bit two's-complement).
 *		  3 = real (32-, 64-, or 128-bit IEEE floating-point to
 *			32-, 64- or 128-bit CRAY IEEE floating-point).
 *                      (128-bit deferred).
 *		  4 = complex (2 x 32-, 64-, or 128-bit IEEE floating-
 *			point to 2 x 32-, 64- or 128-bit CRAY IEEE
 *			floating-point).
 *		  5 = logical (32- or 64-bit zero/nonzero logical to
 *			to 32- or 64-bit CRAY IEEE zero/nonzero logical).
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
 *		Bitoff must be at least 0 and at most 63.
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

_f_int
CRI2IEG(
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
	register int	bytes, fflg, i, ilen, isize, j, osize;
	register long	*ilptr, *olptr;
	register short	*isptr;
	register short	*osptr;
	register char	*icptr, *ocptr;
	int		count, incr;
	long		bits, zero, oflow ;

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

        oflow   = O@T3D;
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

			/* 64->64/32/16 and 32->32/16. */
			/* integer*2/4 are internally in a larger, sign
			   extended variable called a "container".  */ 
			/* cft77 all: integer*2/4/8 are 64 bits internal */ 
			/* cft90 pvp: integer*2/4/8 are 64 bits internal */ 
			/* cft90 t3d: integer*2/4 are 32 bits internal */ 

                        /* isize is internal(CRI) size, osize is IEG size */
			if (isize != 32 && isize != 64)
				return((_f_int) -5);

			if (osize != 16 && osize != 32 && osize !=64)
				return((_f_int) -6);
			if (osize > isize )
				return((_f_int) -8);

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

				osptr	= (short *) olptr;
				ilptr	= (long *) iec;
				isptr	= (short *) iec;

				/* Convert data */

				if (isize == 64) {
				   if (osize == 32) _CRI2IEG_64I32_ONCRI(
					ilptr, osptr, &count, &incr ) ;
				   else	if (osize == 64) 
					for (i = 0; i < count; i++) {
						olptr[i]	= *ilptr;
						ilptr		= ilptr + incr;
					}
				   	else /* 64->16 */ _CRI2IEG_64I16_ONCRI(
						ilptr, olptr, &count, &incr ) ;
				}
				else {	/* isize == 32 */
				   if (osize == 32) 
					for (i = 0; i < count; i++) {
						osptr[i]	= *isptr;
						isptr		= isptr + incr;
					}
				   	else /* 32->16 */ _CRI2IEG_32I16_ONCRI(
						isptr, osptr, &count, &incr ) ;
				}
			}
			break;

		case 3:		/* Real */
		/* 128->128/64, 64->64/32 and 32->32 */
			if (isize != 32 && isize != 64)
				return((_f_int) -5);
				/* 128-bit not yet supported */

			if (osize !=32 && osize != 64 )
				/* 128-bit not yet supported */
				return((_f_int) -6);

			if (isize == osize && incr == 1)
				MOVBITZ(iec, &zero, &bits, forn, (long *) bitoff);
			else {
			/* allow half-size or identical output */
				if (( osize != isize )&&( isize != (2* osize)))
					 return((_f_int) -8);

				/* Get pointers to data */

				if (*bitoff != 0) {	/* If not word aligned */
					olptr	= (long *) malloc(bytes);

					if (olptr == (long *) NULL)
						return((_f_int) -7);

					fflg	= 1;
				}
				else
					olptr	= (long *) forn;
				osptr	= (short *) olptr;

				ilptr	= (long *) iec;
				isptr	= (short *) iec;

				/* Convert data */
				if (isize == 64) {
					if (osize == 32)
						_CRI2IEG_64R32_ONCRI( ilptr, osptr,
							&count, &incr);
					else 	/* isize == 64 && osize == 64 */
						for (i = 0; i < count; i++) {
							double * ofptr, * ifptr;
							ifptr =(double *) ilptr;
							ofptr =(double *) olptr;
							ofptr[i] = * ifptr;
							ilptr += incr;
						}

				}
				else {	/* isize == 32 && osize == 32 */
					for (i = 0; i < count; i++) {
						float * ofptr, * ifptr;
						ifptr = (float *)isptr;
						ofptr = (float *)osptr;
						ofptr[i] = * ifptr;
						isptr += incr;
					}
				}
			}
			break;

		case 4:	/* Complex 64= 2 x 32, 128= 2 x 64 */
			/* 128->128/64, 64->64, 256->256/128 */
			if (isize != 64 && isize != 128)
				return((_f_int) -5);
				/* 256-bit not yet supported */

			if (osize != 128 && osize != 64)
				return((_f_int) -6);
				/* 256-bit not yet supported */

			if (isize == osize && incr == 1)
				MOVBITZ(iec, &zero, &bits, forn, (long *) bitoff);
			else {
				/* allow half-size or identical output */
				if (( osize != isize )&&( isize != (2*osize) ))
					 return((_f_int) -8);


				/* Get pointers to data */

				if (*bitoff != 0) {	/* If not word aligned */
					olptr	= (long *) malloc(bytes);

					if (olptr == (long *) NULL)
						return((_f_int) -7);

					fflg	= 1;
				}
				else
					olptr	= (long *) forn;
				osptr	= (short *) olptr;

				ilptr	= (long *) iec;
				isptr	= (short *) iec;

				/* Convert data */

				if (isize == (64*2)) {
					if (osize == (32*2))
						_CRI2IEG_64C32_ONCRI( ilptr, osptr,
							&count, &incr);
					else
					/* isize == 64*2 && osize == 64*2 */
						for (i = 0; i < count; i++) {
							double *ofptr, *ifptr;
							ifptr =(double *) ilptr;
							ofptr =(double *) olptr;
							ofptr[i] = * ifptr;
							ofptr[i+1] =*( ifptr+1);
							ilptr += incr;
						}
				}
				else {	/* isize == 32*2 && osize == 32*2 */
					for (i = 0; i < count; i++) {
							float *ofptr, *ifptr;
							ifptr =(float *) isptr;
							ofptr =(float *) osptr;
							ofptr[i] = * isptr;
							ofptr[i+1] =*( isptr+1);
							isptr += incr;
					}
				}
			}
			break;

		case 5:		/* Logical */
			if (isize != 32 && isize != 64)
				return((_f_int) -5);

			if (osize != 64 && osize != 32)
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

			osptr	= (short *) olptr;
			ilptr	= (long *) iec;
			isptr	= (short *) iec;

			/* Convert data, both use 0/1 logic */

			if ( isize == 64 ) {
			  /* long tmp; Commented because never used */
				if ( osize == 32)
				 _CRI2IEG_64L32_ONCRI( ilptr, osptr, &count, &incr);
				else /* ( osize == 64) */
					for (i = 0; i < (count); i++) {
						olptr[i]	= *ilptr;
						ilptr		= ilptr + incr;
					}
			}
			else	{	/* isize == 32   osize == 32  */
					for (i = 0; i < (count); i++) {
						osptr[i]	= *isptr;
						isptr		= isptr + incr;
					}
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

/*
 * The T3x IEEE flags for invalid, exceptional input, overflow, underflow and
 * inexact cannot be set
 * at this time without taking an interrupt.  The overflow flag should be set
 * here when the return value is != 0.
 */
#define SETOVFLW(x) ;
	SETOVFLW( (O@T3D - oflow) !=0 )
	return ( (_f_int) (O@T3D - oflow) );
}
