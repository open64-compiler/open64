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
#pragma ident "@(#) libf/fio/wrunf.c	92.2	06/21/99 10:37:55"
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
 *	_wrunf()	Write unformatted processing.
 *
 *	Return value
 *
 *		 0	normal return.
 *
 *		>0	if error condition and IOSTAT= or ERR= is
 *			specified.
 */

#ifdef	INLINE
static int
_inline_wrunf(
#else
int
_wrunf(
#endif
	FIOSPTR		css,	/* Current Fortran I/O statement state */
	unit		*cup,	/* Unit pointer */
	void		*ptr,	/* Pointer to data */
	type_packet	*tip,	/* Type information packet */
	int		mode	/* Mode argument to _fwwd() */
)
{
	register short	shared;		/* 1 iff ptr points to sdd */
	register int	errn;		/* Error number */
	register int64	fillen;		/* bit size of each element, on disk */
	register long	count;		/* Number of data items */
	register long	elsize;		/* element size in bytes */
	register long	i;
	register long	incb;		/* inc (in units of bytes) */
	register long	items;
	long		lbuf[LOCBUFLN]; 
	void		*fwwdbuf;	/* ptr to buffer passed to _fwwd */
#ifdef	_CRAYT3D
	register long	elwords;	/* element size in words */
#endif

	errn	= 0;
	shared	= 0;
	count	= tip->count;
	elsize	= tip->elsize;
	fillen	= tip->extlen;

	if (count == 0)
		return(0);

	if (tip->type90 == DVTYPE_ASCII)
		fillen	= fillen * elsize;

	incb	= tip->stride * elsize;	/* Stride in bytes */

	if (cup->useq == 0) {	/* If direct access file */
		register int64	newpos;
		register int64	recl;

		newpos	= cup->urecpos + count * fillen; /* in bits */
		recl	= (int64) (cup->urecl);

		if ((recl << 3) < newpos) {
			errn	= FEWRLONG;	/* output record too long */
			goto done;
		}
	}

#ifdef	_CRAYT3D
	if (_issddptr(ptr)) {
		/* ptr points to a shared data descriptor */
		/* If we have a layer that handles sdds someday, we */
		/* could check for that here and not set shared to one. */
		/* We'd also probably want to make sure that we're not */
		/* doing foreign data converion */
		shared	= 1;
		elwords	= elsize / sizeof(long);
	}
#endif

/*
 *	If only one item, or stride is such that data is contiguous,
 *	do it all at once
 */
	if ((shared == 0) && ((count == 1) || (incb == elsize))) {
		register long	ret;
		int		status;

		if (mode == FULL)
			cup->f_lastwritten = 1;

		ret	= _fwwd(cup, ptr, tip, mode, (int *) NULL,
				(long *) NULL, &status);

		if ( ret == IOERR ) {
			errn	= errno;
			goto done;
		}

		return(0);
	}

/*
 *	Stride is such that memory is not contiguous, break the request
 *	into chunks and do a gaterh on the items before writing.
 */

	items	= (LOCBUFLN * sizeof(long)) / elsize;	/* chop it in chunks */

	assert( ! (shared && items == 0) );	/* don't support shared char */

	if (items == 0)
		items	= 1;			/* must be character*BIG array*/

	fwwdbuf	= lbuf;

	for ( i = 0; i < count; i = i + items ) {
		register long	ret;
		int		status;

		/* trim the item count if not a full buffer's worth */

		if (items > count - i)
			items	= count - i;

		tip->count	= items;

		/*
		 * Gather items from user array into lbuf, and then write
		 * out a chunk.  If items == 1, we suppress the extra data
		 * copy for performance and because it might not fit in the
		 * lbuf if it is character*BIG data.
		 *
	 	 * We don't have to worry about shared data not fitting 
		 * in lbuf since character data is never shared.
		 */ 

#ifdef	_CRAYT3D
		if (shared)
			_cpyfrmsdd(ptr, lbuf, items, elwords, tip->stride, i);
		else
#endif
		{
			if (items > 1) 
				_gather_data (lbuf, items, incb, elsize, ptr);
			else 
				fwwdbuf	= ptr;
		
		}

		if ( mode == FULL && (i+items >= count)) {
			cup->f_lastwritten = 1;
			ret	= _fwwd(cup, fwwdbuf, tip, FULL, (int *) NULL,
					(long *) NULL, &status);
		}
		else
			ret	= _fwwd(cup, fwwdbuf, tip, PARTIAL,
					(int *) NULL, (long *) NULL, &status);

		if ( ret == IOERR ) {
			errn	= errno; 
			goto done;
		}

		if (!shared)
			ptr	= (char *)ptr + (ret * incb);
	}

done:
	if (errn > 0) {
		if ((cup->uflag & (_UERRF | _UIOSTF)) == 0)
			_ferr(css, errn);	/* Run-time error */
	}
		
	return(errn);
}
