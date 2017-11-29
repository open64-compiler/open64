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


#pragma ident "@(#) libu/ffio/mrread.c	92.1	06/29/99 13:16:47"

#include <stdio.h>
#include <ffio.h>
#include "fssio.h"

#define SYNC 0
#define ASYNC 1

int __mr_read(struct fdinfo *fio, bitptr bufptr, int nbytes, struct ffsw *stat,
	int fulp, int *ubc, int sync);

/*
 * Handle buffer resident read (MR)
 * Transfer data from the Memory Resident buffer to the user.  Data
 * beyond the buffer is treated as 'overflow', and requests for that
 * data are passed down to the next level.
 *
 * Parameters:
 *	fio	- Pointer to fdinfo structure
 *	bufptr	- bit pointer to where data is to go.
 *	nbytes	- Number of bytes to be read
 *	stat	- pointer to status return word
 *	fulp	- full or partial read mode flag
 *	ubc	- pointer to unused bit count
 */

_mr_read(
struct fdinfo *fio,
bitptr bufptr,
int nbytes,
struct ffsw *stat,
int fulp, 
int *ubc)
{
#pragma _CRI inline __mr_read
	return(__mr_read(fio, bufptr, nbytes, stat, fulp, ubc, SYNC));
}

_mr_reada(
struct fdinfo *fio,
bitptr bufptr,
int nbytes,
struct ffsw *stat,
int fulp, 
int *ubc)
{
#pragma _CRI inline __mr_read
	return(__mr_read(fio, bufptr, nbytes, stat, fulp, ubc, ASYNC));
}

int
__mr_read(
struct fdinfo *fio,
bitptr bufptr,
int nbytes,
struct ffsw *stat,
int fulp, 
int *ubc,
int sync)
{
	int bits, nbits, ret, lftbits, reqbits;
	int ovbits;
	int moved;
	int pos, epos;
	int nb;
	int locubc;
	struct mr_f *mr_info;
	struct ffsw locstat;
	_lociosw *locptr;

	mr_info = (struct mr_f *)fio->lyr_info;
	nbits = (nbytes << 3) - *ubc;

	if (nbits == 0) {
		SETSTAT(stat, FFCNT, 0);
		return(0);
	}

	moved = 0;
	ovbits = 0;
/*
 *      Guarantee that we will not go over the actual EOD on the file
 *      by reducing the request if necessary.
 *	Note that the test is '<' so that if nbits is zero, an EOD
 *	status will not be returned.
 */
	lftbits = mr_info->mreof - SUBT_BPTR(fio->_ptr, fio->_base);
	if (lftbits < nbits) {
		nbits = lftbits;
		if (lftbits <= 0)
			return(seteod(fio, stat));
	}
	reqbits = nbits;	/* number of bits requested */
/*
 *	Check for overflow
 */
	pos = SUBT_BPTR(fio->_ptr, fio->_base);	/* bit position in file */
	epos = pos + nbits;
	if (epos > mr_info->mrsize) {
		ovbits = epos - mr_info->mrsize;
		if (ovbits > nbits) ovbits = nbits;
		nbits -= ovbits;
	}
/*
 *	Move data from buffer to user
 */
	if (nbits != 0) {
		GETDATA(bufptr, fio, nbits);
		SET_BPTR(bufptr, INC_BPTR(bufptr, nbits));
		moved += nbits;
	}
	if (ovbits != 0) {
		/* bit position in file */
		pos = SUBT_BPTR(fio->_ptr, fio->_base);
		if (pos == mr_info->mrsize) {
			ret = XRCALL(fio->fioptr, seekrtn) fio->fioptr,
				(pos - mr_info->ovoff) >> 3, 0, &locstat);
			if (ret < 0) {
				goto erret;
			}
		}
		nb = (ovbits + 7) >> 3;
		locubc =  (nb << 3) - ovbits;
		if (sync == SYNC) {
synchronous:
			ret = XRCALL(fio->fioptr, readrtn) fio->fioptr, bufptr,
				nb, &locstat, fulp, &locubc);
			if (ret < 0) {
				goto erret;
			}
			bits = (ret << 3) - locubc;
			moved += bits;
			SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, bits));
		}
		else {
			locptr = _mr_locsw_set(mr_info, stat, moved);
			if (locptr == NULL) {
				goto synchronous;
			}
			ret = XRCALL(fio->fioptr, readartn) fio->fioptr, bufptr,
				nb, &(locptr->local_sw), fulp, &locubc);
			if (ret < 0) {
				*stat = locptr->local_sw;
				_mr_locsw_clear(mr_info, locptr);
				return(ERR);
			}
			bits = (nb << 3) - locubc;
			SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, bits));
			return((moved + bits + 7 ) >> 3);
		}
	}
	SETSTAT(stat, FFCNT, (moved + 7) >> 3);
	*ubc = ((moved+7) & ~(7)) - reqbits;
	return ((moved + 7) >> 3);

erret:
	*stat = locstat;
	return(ERR);
}

/*
 *	Common code to handle EOD return
 */
static
seteod(struct fdinfo *fio, struct ffsw *stat)
{
	fio->ateof = 0;
	fio->ateod = 1;
	SETSTAT(stat, FFEOD, 0)
	return(0);
}

/*
 * _mr_seek()
 * Perform a seek operation on MR file.
 */
_mr_seek(struct fdinfo *fio, int pos, int whence, struct ffsw *stat)
{
	int ret, newpos;
	long ovpos;
	bitptr newfptr;
	struct mr_f *mr_info;

	mr_info = (struct mr_f *)fio->lyr_info;
/*
 *	Do special MR class things
 */
	if (whence == 0) {
		FFSTAT(*stat) = FFBOD;
		newfptr = fio->_base;
	}
	else if (whence == 1) {
		FFSTAT(*stat) = FFCNT;
		newfptr = fio->_ptr;
	}
	else if (whence == 2) {
		FFSTAT(*stat) = FFEOD;
		newfptr = fio->_base;
		SET_BPTR(newfptr, INC_BPTR(newfptr, mr_info->mreof));
	}
	else {
		ERETURN(stat, FDC_ERR_BADSK, 0);
	}
	SET_BPTR(newfptr, INC_BPTR(newfptr, pos << 3));

/*
 *	Check the size. If we are seeking to a spot outside the buffer,
 *	make sure that the lower level layer is properly
 *	positioned.
 */
	newpos = SUBT_BPTR(newfptr, fio->_base);
	if (mr_info->overflowed == YES) {
		if (newpos >= mr_info->mrsize)
			ovpos = (newpos - mr_info->ovoff) >> 3;
		else
			ovpos = (mr_info->mrsize - mr_info->ovoff) >> 3;
		ret = XRCALL(fio->fioptr, seekrtn) fio->fioptr, ovpos, 0, stat);
		if (ret < 0) return(ret);
	}

	fio->_ptr = newfptr;
	fio->ateof = 0;
	fio->ateod = 0;
	ret = (newpos + 7) >> 3;
	return (ret);
}
