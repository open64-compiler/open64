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



#ifndef INLINE
#pragma ident "@(#) libf/fio/rdunf.c	92.2	06/21/99 10:37:55"
#endif

#include <errno.h>
#include <liberrno.h>
#include <fortran.h>
#include <cray/nassert.h>
#ifdef	_CRAYT3D
#include <cray/mppsdd.h>
#endif
#include "fio.h"
#include "f90io.h"

#define LOCBUFLN 4096

/*
 *	_rdunf()	Read unformatted processing.
 *
 *	Return value
 *
 *		 0	normal return.
 *
 *		<0	if end of file condition and IOSTAT= or END=
 *			is specified.
 *
 *		>0	if error condition and IOSTAT= or ERR= is
 *			specified.
 */

#ifdef	INLINE
static int
_inline_rdunf(
#else
int
_rdunf(
#endif
	FIOSPTR		css,	/* Current Fortran I/O statement state */
	unit		*cup,	/* Unit pointer */
	void		*ptr,	/* Pointer to data */
	type_packet	*tip,	/* Type information packet */
	int		_Unused	/* Unused by this routine */
)
{
	register short	shared;		/* 1 iff ptr points to shared data */
	register int	errn;		/* Error number	*/
	register int64	fillen;		/* bit size of each element, on disk */
	register long	count;		/* Number of data items */
	register long	elsize;		/* element size in bytes */
	register long	i;
	register long	incb;		/* inc (in units of bytes) */
	register long	items;
	int		status;
	long		lbuf[LOCBUFLN];
	void		*frwdbuf;	/* ptr to buffer passed to _frwd */
#ifdef	_CRAYT3D
	register long	elwords;	/* element size in words */
#endif

	errn	= 0;		/* Clear error number */
	shared	= 0;		/* Assume data is not shared */
	count	= tip->count;
	elsize	= tip->elsize;
	fillen	= tip->extlen;

	if (count == 0)
		return(0);

	if (tip->type90 == DVTYPE_ASCII)
		fillen	= fillen * elsize;

	incb	= tip->stride * elsize;	/* Stride in bytes */

	if ( cup->ueor_found ) {
		errn	= FERDPEOR;
		goto done;
	}

	if (cup->useq == 0) {	/* If direct access file */
		register int64	newpos;
		register int64	recl;

		newpos	= cup->urecpos + count * fillen; /* in bits */
		recl	= (int64) (cup->urecl);

		if ((recl << 3) < newpos) {
			/*
			 * The user is asking for more data than can fit in a
			 * RECL-sized record, so we abort here.
			 */
			errn	= FERDPEOR;	
			goto done;
		}
	}

#ifdef	_CRAYT3D
	if (_issddptr(ptr)) {
		/* ptr points to shared data descriptor. */
		/* If we have a layer that handles sdds someday, we */
		/* could check for that here and not set shared. */
		/* We'd also probably want to make sure that we are */
		/* not doing foreign data conversion */
		css->f_shrdput	= 1;
		shared		= 1;
		elwords		= elsize / sizeof(long);
	}
#endif
/*
 *	If contiguous memory, transfer all data at once.
 */
	if ((shared == 0) && ((count == 1) || (incb == elsize))) {
		register long	ret;

		ret	= _frwd(cup, ptr, tip, PARTIAL, (int *) NULL,
				(long *) NULL, &status);

		if ( ret == IOERR ) {
			errn	= errno;
			goto done;
		}

		if ( status == EOR ) {
			cup->ueor_found	= YES;
			cup->uend	= BEFORE_ENDFILE;
		}
		else if ( status == CNT )
			cup->uend	= BEFORE_ENDFILE;

		if ( ret < count ) {
			if (status == EOF || status == EOD) 
				goto endfile_record;
			errn	= FERDPEOR;
			goto done;
		}

		return(0);
	}
/*
 *	Stride is such that memory is not contiguous, break the request
 *	into chunks and do a scatter on the items when read.
 */
	items	= (LOCBUFLN * sizeof(long)) / elsize; /* chop it into chunks */

	assert( ! (shared && items == 0) );	/* don't support shared char */
 
	if (items == 0)
		items	= 1;			/* must be character*BIG array*/

	frwdbuf	= lbuf;

	for ( i = 0; i < count; i += items ) {
		register long	ret;
	
		/* trim the item count if not a full buffer's worth */

		if (items > count - i)
			items	= count - i;

		tip->count	= items;

		/*
		 * Read data into lbuf, scatter items from lbuf into the
		 * user array, and then write out a chunk.  If items == 1,
		 * we suppress the extra data copy for performance and because 
		 * it might not fit in the lbuf if it is character*BIG data.
		 *
		 * We don't have to worry about shared data not fitting in
		 * lbuf since character data is never shared.
		 */
		if ((items == 1) && (shared == 0))
			frwdbuf	= ptr;	/* read directly to user array */

		ret	= _frwd(cup, frwdbuf, tip, PARTIAL, (int *) NULL,
				(long *) NULL, &status);

#ifdef	_CRAYT3D
		if (shared)
			_cpytosdd(ptr, lbuf, items, elwords, tip->stride, i);
		else
#endif
			if (items > 1)
				_scatter_data (ptr, items, incb, elsize, lbuf);

		if ( ret == IOERR ) {
			errn	= errno;
			goto done;
		}
		if ( status == EOR ) {
			cup->ueor_found	= YES;
			/* If not last iteration, this is an error */
			if ((i + ret) < count) {
				errn	= FERDPEOR;
				goto done;
			}
		}

		if (i == 0)
			if (status == EOR || status == CNT)
				cup->uend	= BEFORE_ENDFILE;

		/*
		 * We know that items > 0
		 */
		if ( ret < items ) {
			if (status == EOF || status == EOD) 
				goto endfile_record;
			errn	= FERDPEOR;
			goto done;
		}

		if (!shared)
			ptr	= (char *) ptr + (ret * incb);
	}

done:
	/* Process any error which occurred */

	if (errn > 0) {
		if ((cup->uflag & (_UERRF | _UIOSTF)) == 0)
			_ferr(css, errn);	/* Run-time error */
	}
	else if (errn < 0) {
		if ((cup->uflag & (_UENDF | _UIOSTF)) == 0)
			_ferr(css, errn);	/* EOF-type error */
	}

	return(errn);

endfile_record:
	/*
	 * EOF/EOD is an error on direct access, an end
	 * condition on sequential access.
	 */
	if (status == EOF) {
		cup->uend	= PHYSICAL_ENDFILE;
		errn		= FERDPEOF;
	}
	else {	/* End of data */
		if (cup->uend == 0) {
			cup->uend	= LOGICAL_ENDFILE;
			errn		= FERDPEOF;
		}
		else
			errn		= FERDENDR;
	}

	if (!(cup->useq))	/* If direct access */
		errn	= FENORECN;	/* Record does not exist */

	goto done;
}
