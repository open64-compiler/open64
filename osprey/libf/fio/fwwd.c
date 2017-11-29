/*
 * Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

/*

  Copyright (C) 2000, 2001, Silicon Graphics, Inc.  All Rights Reserved.

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



#pragma ident "@(#) libf/fio/fwwd.c	92.3	09/29/99 19:50:24"

#include <errno.h>
#include <fortran.h>
#include <liberrno.h>
#include <cray/nassert.h>
#include "fio.h"
#ifdef	_ABSOFT
#include "ac_sysdep.h"
#else

#if	defined(_LITTLE_ENDIAN) && !defined(__sv2)
#ifndef FILE_FLAG
#define FILE_FLAG(__f)  (__f)->_flags
#endif
 
#ifndef IOREAD
#define IOREAD = _IO_CURRENTLY_APPENDING
#endif
#ifndef IORW
#define IORW = _IO_TIED_PUTGET
#endif

#else		/* LITTLE_ENDIAN and not sv2 */


#ifndef FILE_FLAG
#define FILE_FLAG(__f)  (__f)->_flag
#endif

#endif		/* LITTLE_ENDIAN and not sv2 */
#endif

#define TBFSZ	(4096 * sizeof(long))	/* Size of temporary buffer (bytes) */

/*
 *	_fwwd
 *		Write binary data to a record of a Fortran file.
 *
 *	Side effects
 *
 *		Increments cup->urecpos by the change in bit position within
 *		the record, including pad bits.
 *
 *		Clears cup->uend if the file supports multiple endfiles.
 *
 *	Return value
 *		items on success.  On error, -1 is returned, with errno set to 
 *		the specific error status.
 */
long
_fwwd(
unit		*cup,		/* Unit pointer */
void		*uda,		/* User data address */
type_packet	*tip,		/* Data conversion function index (0 if none) */
int		mode,		/* FULL or PARTIAL record mode */
int		*ubcret,	/* If non-NULL, pointer to unused bit count in
				 * the last item.  Normal item alignment 
				 * (normally at word boundaries) is disabled 
				 * when this parameter is passed. */
long		*unused_6,	/* Unused by _fwwd() */
int		*status)	/* Status return is either CNT or EOR */
{
	register int	buflim;
	register int	fdsize;		/* item size (bits) in file */
	register int	padbyts;
	register long	elsize;
	register long	items;
	register ftype_t type;
	register size_t	breq;
	register ssize_t ret;
	int		padubc;
	int		padval;
	int		ubc;
	FILE		*fptr;

	/* Assertions */

#ifdef	_UNICOS
	assert ( _numargs() == 7);
#endif
	assert ( mode == FULL || mode == PARTIAL );
	assert ( tip != NULL );
	assert ( status != NULL );

/*
 *	If positioned after an endfile, and the file does not support
 *	multiple endfiles, a write is invalid.
 */

	if (cup->uend && !cup->umultfil) {
		errno	= FEWRAFEN;
		return(IOERR);
	}

	type	= tip->type90;
	elsize	= tip->elsize;
	items	= tip->count;
	breq	= elsize * items;
	padbyts	= 0;
	padubc	= 0;
	ubc	= 0;

	if (ubcret != NULL) {

		if ((*ubcret % 8) != 0 && cup->ufs != FS_FDC) {
			errno	= FEUBCINV;
			return (IOERR);
		}

		if (type != DVTYPE_TYPELESS) {
			errno	= FEINTUNK;
			return (IOERR);
		}
/*
 *		Be sure to handle (ubc > 7)
 */
		breq	= breq - (*ubcret >> 3);
		ubc	= *ubcret % 8;
	}
#if	NUMERIC_DATA_CONVERSION_ENABLED
	else {
/*
 *		Pad word-aligned numeric data on word boundaries within
 *		the record for CRI and some foreign data formats.  Note
 *		that the elsize expression needs to be cleaned up to be
 *		something like:  external_size > granularity.
 */
		if ((cup->urecpos & cup->ualignmask) != 0 &&
		    type != DVTYPE_ASCII &&
		    items > 0 &&
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
		    elsize > 2 )
#else
		    elsize > 4 )
#endif
			COMPADD(cup, padbyts, padubc, padval);
	}
#endif	/* NUMERIC_DATA_CONVERSION_ENABLED */

	cup->ulastyp	= type;

	if (mode == FULL) {
		cup->ulastyp	= DVTYPE_TYPELESS;
		cup->urecpos	= 0;
	}
/*
 *	According to the file structure make the appropriate
 * 	write request.
 */
	*status	= CNT;

	switch ( cup->ufs ) {

	case FS_FDC:
		/*
		 * If a logical endfile record had just been read,
		 * replace it with a physical endfile record before
		 * starting the current data record.
		 */
		if (cup->uend == LOGICAL_ENDFILE) {
			if (XRCALL(cup->ufp.fdc, weofrtn)cup->ufp.fdc,
				&cup->uffsw) < 0){
				errno	= cup->uffsw.sw_error;
				return(IOERR);
			}
		}

		cup->uend	= BEFORE_ENDFILE;
/*
 *		If no items, still may have to terminate record
 */
		if (items == 0) {
			if (mode == FULL) {
				long		end;

				ret	= XRCALL(cup->ufp.fdc, writertn) 
						cup->ufp.fdc,
						CPTR2BP(&end), 0, &cup->uffsw,
						mode, &ubc);

				if (ret < 0) {		/* if an error */
					errno	= cup->uffsw.sw_error;
					return(IOERR);
				}

				*status	= EOR;
			}

			/* recpos does not change! */

			return(EOR);
		}
/*
 *		Align the data to a word boundary if required.
 */
		if (padbyts > 0) {

			ret	= XRCALL(cup->ufp.fdc, writertn) cup->ufp.fdc,
					CPTR2BP(&padval),
					padbyts, &cup->uffsw, PARTIAL, &padubc);

			if (ret < 0) {		/* if an error */
				errno	= cup->uffsw.sw_error;
				return(IOERR);
			}
			if (mode != FULL)
				cup->urecpos	+= ((uint64)ret << 3) - padubc;
		}
/*
 *		If no conversion to be done, just write out the data
 */
		if (tip->cnvindx == 0) {

			ubc	= 0;

			ret	= XRCALL(cup->ufp.fdc, writertn) cup->ufp.fdc,
				 	CPTR2BP(uda),
					breq, &cup->uffsw, mode, &ubc);

			if (ret < 0) {		/* if an error */
				errno	= cup->uffsw.sw_error;
				return(IOERR);
			}

			if (mode == FULL)
				*status	= EOR;
			else
				cup->urecpos	+= ((uint64)ret << 3) - ubc;

			return(items);	/* return number of items written */
		}

#if	NUMERIC_DATA_CONVERSION_ENABLED
/*
 *		Figure out the item size on the foreign side.
 *		First, change the conversion routine if character type
 */
		{
		register int	uoff;
		register int64	bits;
		register int64	totbits;
#ifdef KEY /* Bug 13352 */
/* Conversion buffer; ia2mips() assumes alignment */
                unsigned long long tbuf[
                  (TBFSZ + sizeof(unsigned long long) - 1) /
                  sizeof(unsigned long long)];
                unsigned char *tp = (unsigned char *) tbuf;
#else /* KEY Bug 13352 */
                unsigned char   tbuf[TBFSZ], *tp; /* Conversion buffer */
#endif /* KEY Bug 13352 */

		_f_int		dctype;
		int		(* cvt_fun)();	/* Conversion function */

		cvt_fun	= __fndc_ncfunc[tip->cnvindx].to_foreign;

#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
		if (!_loaded(cvt_fun)) {
			errno	= FELDDCNV;
			return(IOERR);
		}
#endif

		fdsize	= tip->extlen;
		dctype	= tip->cnvtype;

		if (fdsize == 0) {
			errno	= FDC_ERR_NCVRT;
			return(IOERR);
		}
#ifndef KEY /* Bug 13352 */
		tp	= tbuf;
#endif /* KEY Bug 13352 */
		buflim	= ((TBFSZ << 3) / fdsize) * fdsize;

/*
 *		totbits gets the total size of the foreign data which will
 *		be written out to the file.
 *		Convert the data by slicing it into parts and converting it
 *		into a working space, then write the resultant bits out.
 *		Note that character data is handled as a contiguous stream
 *		of bytes with no regard for the character element 
 *		boundaries.
 */
		totbits	= (items * fdsize) - ubc;

		if (type == DVTYPE_ASCII)
			totbits	= totbits * elsize;	/* must factor in char length */

		bits	= 0;
		uoff	= 0;

		while (bits < totbits) {
#ifdef KEY /* Bug 10310 */
			register long	slice;
#else /* KEY Bug 10310 */
			register int	slice;
#endif /* KEY Bug 10310 */
			register _f_int	numerr;
			int		locubc;
			int		locmode;
			_f_int		icount;
			const _f_int	bitoff = 0;
			const _f_int	stride = 1;
#ifdef	_CRAY
			_fcd		craychr;
#endif

			slice	= totbits - bits;
			locmode	= mode;

			if (slice > buflim) {
				slice	= buflim;
				locmode	= PARTIAL;
			}

			/* slice is number of bits in whole items */

			breq	= (slice + 7) >> 3;
			locubc	= (breq << 3) - slice;
/*
 *			convert the data.  slice/fdsize is the number of items
 *			to convert.
 */
			icount	= slice / fdsize;

#ifdef	_CRAY
			craychr	= _cptofcd((char *)uda + uoff, icount);
#endif

			if (tip->newfunc) {
				_f_int	flen;	/* Foreign length, in bits */
				_f_int	nlen;	/* Native length, in bits */

				flen	= fdsize;
				nlen	= tip->intlen;

				numerr	= cvt_fun(&dctype, &icount, (void *)tp,
						&bitoff, (char *)uda + uoff,
						&stride, &nlen, &flen,
#ifdef	_CRAY
						craychr);
#else
						(char *) uda + uoff, icount);
#endif
			}
			else
				numerr	= cvt_fun(&dctype, &icount, (void *)tp,
						&bitoff, (char *)uda + uoff,
						&stride,
#ifdef	_CRAY
						craychr);
#else
						(char *) uda + uoff, icount);
#endif

			if (numerr != 0) {
				errno	= (numerr < 0) ? FEINTUNK : FDC_ERR_NCVRT;
				return(IOERR);
			}
/*
 *			write out the data
 */
			ret	= XRCALL(cup->ufp.fdc, writertn) cup->ufp.fdc,
					CPTR2BP(tp), breq, &cup->uffsw,
					locmode, &locubc);

			if (ret < 0) {		/* if an error */
				errno	= cup->uffsw.sw_error;
				return(IOERR);
			}

			bits	= bits + slice;
			uoff	= uoff + (icount * elsize);
		}

		if (mode == FULL)
			*status	= EOR;

		cup->urecpos	= cup->urecpos + totbits;

		break;

		}
#endif	/* NUMERIC_DATA_CONVERSION_ENABLED */
 
	case STD:
		{

		fptr	= cup->ufp.std;		/* Get FILE pointer */
/*
 *		Switch the FILE structure out of read mode and into neutral.
 */
#if	!defined(_LITTLE_ENDIAN) || (defined(_LITTLE_ENDIAN) && defined(__sv2))
		if ((FILE_FLAG(fptr) & (_IOREAD | _IORW)) == (_IOREAD | _IORW) )
			(void) fseek(fptr, 0, SEEK_CUR);
#endif
/*
 *		If number of items to write is zero return to caller.
 */
		if (items == 0)
			return(EOR);
/*
 *		we must align on word boundaries sometimes
 */
		if (padbyts > 0) {

			ret	= fwrite("        ", 1, padbyts, fptr);

			if (ret <= 0) {
				if (errno == 0)
					errno 	= FESTIOER;
				return(IOERR);
			}

			cup->urecpos	+= (uint64)ret << 3;
		}

/*
 *		Use low-level binary i/o routine to write the requested
 *		amount of data.
 */

 		ret	= fwrite(uda, 1, breq, fptr);

 		if (ret != breq) {
			if (ret > 0 || errno == 0)
				errno 	= FESTIOER;
			return(IOERR);
		}

		cup->urecpos	+= (uint64)ret << 3;

		}
		break;

	case FS_AUX:
		errno	= FEMIXAUX;
		return (IOERR);
 	default:
		errno	= FEINTFST;
		return (IOERR);
	}
/*
 *	Normal return: return number of items written.
 */
	if (mode == FULL)
		cup->urecpos	= 0;

	return(items);
}
