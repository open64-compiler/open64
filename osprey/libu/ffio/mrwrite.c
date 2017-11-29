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


#pragma ident "@(#) libu/ffio/mrwrite.c	92.1	06/29/99 13:16:47"

#include <ffio.h>
#include <stdio.h>
#include "fssio.h"

#define MIN(a,b) ((a) < (b) ? (a) : (b))

#define SYNC 0
#define ASYNC 1

int __mr_write(struct fdinfo *fio, bitptr bufptr, int nbytes, struct ffsw *stat,
	int fulp, int *ubc, int sync);

/*
 * Write data to buffer resident file.
 *
 * Parameters:
 *      fio     - Pointer to fdinfo block
 *	bufptr	- bit pointer to where data is to go.
 *	nbytes	- Number of bytes to be written
 *	stat	- pointer to status return word
 *	fulp	- full or partial write mode flag
 *	ubc	- pointer to unused bit count
 */
_mr_write(
struct fdinfo *fio,
bitptr bufptr,
int nbytes,
struct ffsw *stat,
int fulp,
int *ubc)
{
#pragma _CRI inline __mr_write
	return(__mr_write(fio, bufptr, nbytes, stat, fulp, ubc, SYNC));
}

_mr_writea(
struct fdinfo *fio,
bitptr bufptr,
int nbytes,
struct ffsw *stat,
int fulp,
int *ubc)
{
#pragma _CRI inline __mr_write
	return(__mr_write(fio, bufptr, nbytes, stat, fulp, ubc, ASYNC));
}

__mr_write(
struct fdinfo *fio,
bitptr bufptr,
int nbytes,
struct ffsw *stat,
int fulp,
int *ubc,
int sync)
{
	int ret;
	int nb, locubc;
	int curbits, moved, nbits, clrlim;
	int seekpos;
	long neweof;
	long endreq;
	long ovbits;
	long mrinc;
	struct mr_f *mr_info;
	struct ffsw locstat;
	_lociosw *locptr;

	nbits = (nbytes << 3) - *ubc;

	mr_info = (struct mr_f *)fio->lyr_info;
/*
 *	If the request causes the file to be extended, up the buffer allocation
 */
	curbits = SUBT_BPTR(fio->_ptr, fio->_base);
	endreq = curbits + nbits;
	moved = 0;
	ovbits = 0;
	if (endreq > mr_info->mrsize) {
		if (mr_info->overflowed == NO) {
			mrinc = endreq - mr_info->mrsize;
			ret = 1;		/* assume overflow */
/*
 *			If not yet at max size, call grow routine
 */
			if (mr_info->mrsize < BLKS2BITS(mr_info->maxsize)) {
				ret = _mr_grow(fio, mrinc, &locstat);
				if (ret < 0) {
					goto erret;
				}	
			}
/*
 *			ret > 1 means that either we are already at max
 *			allocation, or the grow routine indicated
 *			overflow.  (Return code >0 from _mr_grow means we
 *			overflowed.) First, figure out what portion of the
 *			request must be passed to the lower layer, then go
 *			ahead and complete the request if part can reside in
 *			MR.  A return of ret == 0 means the grow was OK, and
 *			we now have enough space.
 */
			if (ret > 0) {
/*
 *				Do one-time overflow processing
 */
				ret = _fss_overflow(fio, &locstat);
				if (ret < 0) {
					goto erret;
				}
	
				/* Assume that entire request is in overflow */
				ovbits = nbits;
				seekpos = (curbits - mr_info->ovoff) >> 3;
				if ((endreq - mr_info->mrsize) <= nbits) {
					/* request split across end of alloc */
					ovbits = endreq - mr_info->mrsize;
					seekpos = (mr_info->mrsize - mr_info->ovoff) >> 3;
				}
/*
 *				Position the lower layer to where we will need
 *				to be when the lower level I/O kicks in.
 */
				ret = XRCALL(fio->fioptr, seekrtn)
						fio->fioptr, seekpos, 0, &locstat);
				if (ret < 0) {
					goto erret;
				}
				nbits -= ovbits;
			}
			/* fall thru here means split req, or grow was enuf */
		}
		else {	/* already overflowed */
/*
 *			Divide the request between the MR and the ovfl part.
 *			Lower layer should already be at proper
 *			position, so seek is not necessary.
 */
			ovbits = nbits;		/* Assume entirely in ovflow */
/*
 *			If request is split across end of alloc, ovbits
 *			is just the overflow part.
 */
			if ((endreq - mr_info->mrsize) <= nbits)
				ovbits = endreq - mr_info->mrsize;
			nbits -= ovbits;
		}
	}
/*
 *	If the file is being written starting at a point beyond the current
 *	EOF, then we must zero out the space in between.   But we need only
 *	zero it out up to the end of the MR buffer.  The underlying layer
 *	must take care of it's own zeroing.
 */

	clrlim = MIN(mr_info->mrsize, curbits);	/* end of region to be cleared*/

	if (clrlim > mr_info->mreof) {
		int ebits, nbyt;
		bitptr clrspot;
		char *clr;

		ebits = mr_info->mreof;
/*
 *		ebits is the bit offset of the first bit to be cleared.
 *		clrlim is the bit offset one past the last bit to be cleared.
 */
		clrlim = (clrlim + 7) & (~07);		/* round up to byte */
		ebits  = (ebits  + 7) & (~07);		/* round up to byte */

		nbyt   = (clrlim - ebits) >> 3;

		if (nbyt > 0) {
			SET_BPTR(clrspot, INC_BPTR(fio->_base, ebits));
			clr = BPTR2CP(clrspot);
			memset(clr, 0, nbyt);		/* set bytes to 0 */
		}
/*
 *		Handle the partial byte at offset mr_info->mreof.
 */
		ebits = ebits - mr_info->mreof;	/* rounded eof - real eof */
		if (ebits > 0) {
			SET_BPTR(clrspot, INC_BPTR(fio->_base, mr_info->mreof));
			PUT_BITS(clrspot, 0, ebits);	/* set bits to 0 */
		}
	}

	mr_info->mrdirty = 1;
/*
 *	Move data from user to the MR file buffer 
 */
	if (nbits !=  0) {
		PUTDATA(bufptr, fio, nbits);
		moved += nbits;
	}
/*
 *	Check for overflow.  Write to lower level if necessary.
 */
	if (ovbits != 0) {
		SET_BPTR(bufptr, INC_BPTR(bufptr, nbits));
		nb = (ovbits + 7) >> 3;
		locubc = (nb << 3) - ovbits;
		
		if (sync == SYNC) {
synchronous:
			ret = XRCALL(fio->fioptr, writertn) fio->fioptr,
				bufptr, nb, &locstat, fulp, &locubc);
			if (ret < 0) {
				goto erret;
			}
			moved += ovbits;
			SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, ovbits));
		}
		else {
			locptr = _mr_locsw_set(mr_info, stat, moved);
			if (locptr == NULL) {
				goto synchronous;
			}
			ret = XRCALL(fio->fioptr, writeartn) fio->fioptr, bufptr,
			  nb, &(locptr->local_sw), fulp, &locubc);
			if (ret < 0) {
				*stat = locptr->local_sw;
				_mr_locsw_clear(mr_info, locptr);
				return(ERR);
			}
			SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, ovbits));
			neweof = SUBT_BPTR(fio->_ptr, fio->_base);
			if (neweof > mr_info->mreof)
				mr_info->mreof = neweof;
			return(nbytes);
		}
	}
			
	neweof = SUBT_BPTR(fio->_ptr, fio->_base);
	if (neweof > mr_info->mreof)
		mr_info->mreof = neweof;
	SETSTAT(stat, FFCNT, (moved + 7) >> 3);
	return ((moved + 7) >> 3);

erret:
	*stat = locstat;
	return(ERR);
}

/*
 * Write an EOD.
 *	Truncate the file.  This is the 'real' end of the data.
 */
int
_mr_weod(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
{
	int ret;
	struct mr_f *mr_info;

	mr_info = (struct mr_f *)fio->lyr_info;

	mr_info->mreof = SUBT_BPTR(fio->_ptr, fio->_base);
	if (fio->fioptr != NULL && mr_info->mreof > mr_info->mrsize) {
		ret = XRCALL(fio->fioptr, weodrtn) fio->fioptr, stat);
		if (ret < 0) return(ERR);
	}

	mr_info->mrdirty = 1;
	fio->ateof = 0;
	fio->ateod = 1;
	return(0);
}
