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



#pragma ident "@(#) libf/fio/frwd.c	92.3	09/29/99 19:50:24"

#include <errno.h>
#include <fortran.h>
#include <liberrno.h>
#ifdef	_ABSOFT
#include <stdlib.h>
#endif
#include <cray/nassert.h>
#include "fio.h"

#include <stdlib.h>

#define TBFSZ	(4096 * sizeof(long))	/* Size of temporary buffer (bytes) */

static const _f_int	bitoff = 0;
static const _f_int	stride = 1;

/*
 *	_frwd 
 *		Read binary data from one record of a Fortran file.
 *
 *	Side effects
 *
 *		Increments cup->urecpos by the change in bit position within
 *		the record, including pad bits.
 *
 *		Sets *status as follows:
 *
 *			CNT - if the requested number of items were read, and
 *			      more data remains in the current record (if
 *			      the file has records).  
 *			EOR - if all remaining data in the current record was 
 *			      read by this request. 
 *			EOF - if a physical EOF mark was read and no data 
 *			      preceded the EOF mark.
 *			EOD - if the end-of-data mark was reached and no data 
 *			      was read.
 *
 *	Return value
 *		The number of items read.  -1 on error with errno set to
 *		the error code.
 *
 *	Note
 * 		If a nonzero amount of data was read and an EOF mark or the 
 *		end of file was encountered before reading the full amount
 *		of data requested and before encountering EOR, CNT status is 
 *		returned.  This is not well supported because we never detect 
 *		that we processed the EOF.  However, no FFIO layers currently 
 *		support embedded EOF marks inside a record.
 */

long
_frwd(
unit		*cup,		/* Unit pointer */
void		*uda,		/* User data address */
type_packet	*tip,		/* Type information packet */
int		mode,		/* FULL or PARTIAL record mode */
int		*ubcret,	/* If non-NULL, pointer to unused bit count in 
				 * the last item.  This is both an input and 
				 * output parameter.  On input it enables the 
				 * caller to request an arbitrary number of 
				 * bits.  On output it reflects the actual data
				 * transfer.
				 *  NOTE:  Normal item alignment (normally 
				 *	   forced to word boundaries) is 
				 * 	   disabled when this parameter is 
				 *	   passed.	*/
long		*wr,		/* If non-NULL, pointer to number of items read.
				 * This is an output parameter.  It is useful 
				 * because it is possible to have an error 
				 * return, but still return data.  This 
				 * parameter is set only if an error is 
				 * encountered with data being delivered.  It 
				 * is currently only set for FFIO files where 
				 * foreign data conversion is not active. */
int		*status)	/* assigned on return to CNT, EOR, EOF, or EOD*/
{
	register int	fdsize;		/* Item size (bits) in file */
	register int	padbyts;
	register long	elsize;
	register long	items;
	register ftype_t type;		/* Fortran data type */
	register size_t	breq;
	register ssize_t ret;
	register int64	totbits;
	int		padubc;
	int		ubc;
	_f_int		icount;

	/* Assertions */

#ifdef	_UNICOS
	assert ( _numargs() == 7 );
#endif
	assert ( mode == FULL || mode == PARTIAL );
	assert ( tip != NULL );
	assert ( status != NULL );

	type	= tip->type90;
	elsize	= tip->elsize;
	items	= tip->count;
	breq	= elsize * items;	/* bytes requested */
	padbyts	= 0;
	padubc	= 0;
	ubc	= 0;
/*
 *	If a ubc word is passed, this call is asking for typeless data; no 
 *	data conversion will be done.  The ubcret and wr arguments are used
 *	by CALL READ/READP.
 */
	if (ubcret != NULL) {
		if ((*ubcret & 7) != 0 && cup->ufs != FS_FDC) {
			errno	= FEUBCINV;
			return(IOERR);
		}
		if (type != DVTYPE_TYPELESS) {
			errno	= FEINTUNK;
			return(IOERR);
		}
/*
 *		Adjust breq for ubc input, which can be 0-63
 */
		breq	-= *ubcret >> 3;
		ubc	= *ubcret & 7;
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
		    elsize > 2 ) {
#else
		    elsize > 4 ) {
#endif

			long	blanks;
			COMPADD(cup, padbyts, padubc, blanks);
		}
	}
#endif	/* NUMERIC_DATA_CONVERSION_ENABLED */

	cup->ulastyp	= type;

	*status	= CNT;		/* default return status */
/*
 *	According to the file structure make the appropriate
 *	low level read request.
 */
	switch ( cup->ufs ) {

	case  STD:
/*
 *		If the number of items to read is zero return to caller. 
 *		Unicos binary files have no record structure.  Except for
 *		an end of file, a read request always returns CNT status.
 */
		if (items == 0)
			return(0);

		ret	= 1;

		if (padbyts > 0) {	/* Flush a few bytes */
			int	dummy;

			ret	= fread(&dummy, 1, padbyts, cup->ufp.std);

			if (ret > 0)
				cup->urecpos	+= (uint64)ret << 3;

		}
		if (ret > 0)	/* don't continue if last read failed. */
			ret	= fread(uda, 1, breq, cup->ufp.std );
 
		if (ret == 0) {
			if ( ferror(cup->ufp.std) ) {
				if (errno == 0)
					errno	= FESTIOER;
				return(IOERR);
			}
			*status	= EOD;
			return(0);
		}
/*
 *		If the read produced an integral number of items, return.
 *		else allow partial items only for typeless.
 */
		icount		= ret / elsize;
		cup->urecpos	= cup->urecpos + (uint64) (ret << 3);

		if ((ret % elsize) != 0)
			if (type == DVTYPE_TYPELESS) {

				icount	= icount + 1;

				if (ubcret != NULL)
					*ubcret	= (elsize - (ret % elsize)) << 3;
			}
			else
				if (icount == 0) {
					errno	= FERDPEOR;
					return(IOERR);
				}

		break;

	case FS_FDC:
/*
 *		Align the FD to a word boundary if required.
 */

		if (padbyts > 0) {
			long	paddval;

			ret	= XRCALL(cup->ufp.fdc, readrtn) cup->ufp.fdc,
				CPTR2BP(&paddval), padbyts, &cup->uffsw,
				PARTIAL, &padubc);

			if (ret != padbyts || FFSTAT(cup->uffsw) != FFCNT) {

				if (ret < 0) {
					errno	= cup->uffsw.sw_error;
					return(IOERR);
				}

				*status	= FF2FTNST(FFSTAT(cup->uffsw));

				return(0);
			}

			cup->urecpos	+= ((uint64)ret << 3) - padubc;
		}
/*
 *		If no conversion to be done, just read in the data
 */
		if (tip->cnvindx == 0) {
			register short	erret;
/*
 *			read in the data
 */
			ret	= XRCALL(cup->ufp.fdc, readrtn) cup->ufp.fdc,
					CPTR2BP(uda), breq,
					&cup->uffsw, mode, &ubc);

			*status	= FF2FTNST(FFSTAT(cup->uffsw));
			erret	= 0;

			if (*status == EOR)
				cup->ulastyp	= DVTYPE_TYPELESS;

			if (ret < 0) {		/* if an error */

				errno	= cup->uffsw.sw_error;

				if (errno == FETAPUTE) {
					ret	= cup->uffsw.sw_count;
					erret	= 1;
				}
				else
					return(IOERR);
			}
			else
				if (ret == 0)
					return(0);
/*
 *			Data was returned (ret > 0), process it
 *
 *			If the read produced an integral number of items,
 *			then return.  Else allow partial items only for
 *			typeless.
 */
			totbits		= ((uint64)ret << 3) - ubc;
			cup->urecpos	= cup->urecpos + totbits;
			icount		= totbits / (elsize << 3);

			if (type == DVTYPE_TYPELESS) {

				if ((((uint64)icount*elsize) << 3) != totbits) {

					icount	= icount + 1;

					if (ubcret != NULL)
						*ubcret	= (elsize << 3) -
						   (totbits % (elsize << 3));
				}

				if (wr != NULL)
					*wr	= icount;
			}
			else
				if (icount == 0) {
					errno	= FERDPEOR;
					erret	= 1;
				}

			if (erret == 1)
				return(IOERR);

			goto done;	/* Return number of items read */
		}

#if	NUMERIC_DATA_CONVERSION_ENABLED
/*
 *		Process numeric or character conversion.
 */
		{
		int		(* cvt_fun)();	/* Conversion function	*/
		_f_int		dctype;
		char		tbuf[TBFSZ];	/* Temporary buffer	*/
		char		*bptr;		/* Buffer pointer	*/

		cvt_fun	= __fndc_ncfunc[tip->cnvindx].to_native;

#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
		if (!_loaded(cvt_fun)) {
			errno	= FELDDCNV;
			return(IOERR);
		}
#endif

		fdsize  = tip->extlen;
		dctype	= tip->cnvtype;

/*
 *		Point the pointer to the tail end of the buffer by
 *		calculating the total bit count, and then rounding down
 *		to a word boundary.  Note that fdsize is the size of
 *		each byte for type==DVTYPE_ASCII, so we must multiply by
 *		elsize to get the real total bit count.
 */
		if (fdsize == 0) {
			errno	= FDC_ERR_NCVRT;
			return(IOERR);
		}

		totbits	= items * fdsize;	/* Bit size of foreign data to 
						 * be read from the file. */

		if (type == DVTYPE_ASCII)
			totbits	= totbits * tip->elsize;

		if (ubcret != NULL)
			totbits	= totbits - *ubcret;

		/*
		 * Round pointer down to the next byte if numeric conversion
		 * is requested.
		 */

		bptr	= tbuf;	/* Assume data will fit in temporary buffer */
		breq	= (totbits + 7) >> 3;
		ubc	= ((uint64)breq << 3) - totbits;

		if (breq > TBFSZ) {
			bptr	= (char *) malloc(breq);

			if (bptr == NULL) {	/* malloc() failed! */
				errno	= FENOMEMY;
				return(IOERR);
			}
		}
/*
 *		read in the data
 */
		ret	= XRCALL(cup->ufp.fdc, readrtn) cup->ufp.fdc,
				CPTR2BP(bptr), breq,
				&cup->uffsw, mode, &ubc);

		*status	= FF2FTNST(FFSTAT(cup->uffsw));

		if (*status == EOR)
			cup->ulastyp	= DVTYPE_TYPELESS;

		if (ret <= 0) {		/* If an error or no data */
			register long	stat = EOR;

			if (bptr != tbuf)	/* Free allocated space */
				free(bptr);

			if (ret < 0) {
				errno	= cup->uffsw.sw_error;
				stat	= IOERR;
			}

			return(stat);
		}

/*
 *		If the read produced an integral number of items, return.
 *		else diagnose partial items.
 */
		totbits	= ((uint64)ret << 3) - ubc;
		icount	= totbits / fdsize;

		if (((int64)icount * fdsize) != totbits) {
			if (bptr != tbuf)	/* Free allocated space */
				free(bptr);
			/* partial items with conversion on is weird */
			errno	= FDC_ERR_PITM;
			return(IOERR);
		}
/*
 *		Do the numeric conversion
 */
		{
		register _f_int	numerr;		/* Error code */
#ifdef	_CRAY
		_fcd		craychr;	/* Only used for char. conversion */

		craychr	= _cptofcd((char *)uda, items);
#endif

		if (tip->newfunc) {
			_f_int	flen;		/* Foreign length, in bits */
			_f_int	nlen;		/* Native length, in bits */

			flen	= fdsize;
			nlen	= tip->intlen;

			numerr	= cvt_fun(&dctype, &icount, (void *) bptr,
					&bitoff, (void *)uda, &stride, &nlen,
					&flen,
#ifdef	_CRAY
					craychr);
#else
					(char *)uda, items);
#endif
		}
		else {

			numerr	= cvt_fun(&dctype, &icount, (void *)bptr,
					&bitoff, (void *)uda, &stride,
#ifdef	_CRAY
					craychr);
#else
					(char *)uda, items);
#endif
		}

		if (bptr != tbuf)	/* Free allocated space */
			free(bptr);

		if (numerr != 0) {
			errno	= (numerr < 0) ? FEINTUNK : FDC_ERR_NCVRT;
			return(IOERR);
		}
		}
		}

		cup->urecpos	+= totbits;
#endif	/* NUMERIC_DATA_CONVERSION_ENABLED */

		break;

	case FS_AUX:
		errno	= FEMIXAUX;
		icount	= IOERR;
		break;

	default:
		errno	= FEINTFST;
		icount	= IOERR;
		break;
	} /* switch */

done:
	return(icount);		/* Return number of items read or status */
}
