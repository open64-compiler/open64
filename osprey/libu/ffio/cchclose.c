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


#pragma ident "@(#) libu/ffio/cchclose.c	92.2	10/07/99 15:40:23"

#include <stdio.h>
#include <fcntl.h>
#include <malloc.h>
#include <ffio.h>
#include <liberrno.h>
#include "cchio.h"

/*
 *	close a cached file.
 *
 *      Returns: -1 if error occurred
 *		  0 if OK
 */
int
_cch_close(
struct fdinfo	*fio,
struct ffsw	*stat
)
{
	int		errv;
	struct fdinfo	*llfio;
	struct cch_f	*cch_info;

	cch_info = (struct cch_f *)fio->lyr_info;

	errv = 0;
	llfio  = fio->fioptr;

	/*
	 *	Flush all dirty buffers to the underlying layer.
	 */
	if (_cch_flush(fio, stat) == ERR)
		errv = errv ? errv : stat->sw_error;

	/*
	 *	Truncate the underlying file appropriately.  When buffer 
	 *	flushes occur, the underlying file is normally extended 
	 *	to a multiple of the buffer size.  Now we hack off the
	 *	excess at the end of the file.
 	 *
	 *	Suppress this truncation if the "-m on" assign attribute
	 *	is associated with this file.
	 */

	if (cch_info->fsize < cch_info->feof && cch_info->is_multup == 0) {
	
		if (XRCALL(llfio,seekrtn) llfio, BITS2BYTES(cch_info->fsize),
					SEEK_SET, stat) == ERR)
			errv = errv ? errv : stat->sw_error;

		if (XRCALL(llfio,weodrtn) llfio, stat) == ERR)
			errv = errv ? errv : stat->sw_error;
	}
	
	/*
	 *	Close the underlying layers.
	 */
	if (XRCALL(llfio,closertn) llfio, stat) == ERR)
		errv = errv ? errv : stat->sw_error;

	_cch_clfree(fio);

	if (errv)
		ERETURN(stat, errv, 0);

	return(0);
}

/*
 * _cch_clfree()
 *
 *	Free the memory blocks used by the cache layer and set corresponding
 * 	pointers to NULL..
 */
void
_cch_clfree(
struct fdinfo *fio
)
{
	int		i;
	int		nb;
	struct cch_f	*cch_info;
	struct cch_buf	*cbufs;

	cch_info = (struct cch_f *)fio->lyr_info;

	if (fio->lyr_info != NULL) {
		if (cch_info->bufs != NULL) {
			nb = cch_info->nbufs;
			cbufs = cch_info->bufs;
			if (cch_info->bufs_alloc) {
				/*
				 * Buffers were allocated in one chunk.
			 	 */
#ifdef CCH_SDS_SUPPORTED
				if (cch_info->optflags & CCHOPT_SDS) {
					int blknum, istat;
					blknum = (int)BPTR2WP(cbufs[0].buf)
						 / WPBLOCK;
					if (sdsfree(blknum, &istat) != 0)
						_lerror(_LELVL_ABORT, istat);
				}
				else 
#endif
					free(BPTR2CP(cbufs[0].buf));
			}
			for (i=0; i<nb; i++)
				SET_BPTR(cbufs[i].buf,  CPTR2BP(NULL));
			free(cch_info->bufs);
			cch_info->bufs = NULL;
		}
		if (cch_info->savearea != NULL) {
			free(cch_info->savearea);
			cch_info->savearea = NULL;
		}
		free(fio->lyr_info);
		fio->lyr_info = NULL;
	}
	if (fio->fioptr != NULL) {
		free(fio->fioptr);
		fio->fioptr = NULL;
	}
	return;
}

/*
 * _cch_flush
 *
 *	Flush all dirty buffers to the underlying layer.
 *
 *	Return value:	 0 on success.
 *			-1 on failure.
 */
int
_cch_flush(
struct fdinfo	*fio,
struct ffsw	*stat
)
{
        int		i, nb, bs, ret;
	int		errv;
        struct cch_f	*cch_info;
	struct cch_buf	*cbufs;

	errv	 = 0;
	cch_info = (struct cch_f *)fio->lyr_info;
	nb	 = cch_info->nbufs;
	bs	 = cch_info->bsize;

	/*
	 * Loop once to start the flush of each dirty buffer.  Then loop a
	 * second time to wait for the completions of all buffer flushes.
	 */
	cbufs = cch_info->bufs;
	for (i=0; i<nb; i++) {
		if (cbufs[i].filead >= 0 && (cbufs[i].flags & CCH_DIRTY))  {
			if (((cbufs[i].filead) < cch_info->fsize) &&
				((cbufs[i].filead + bs) > cch_info->fsize)){
				if ((cbufs[i].flags & CCH_ZEROED) == 0) {
                                        bitptr toptr;
                                        off_t eofaddr;
                                        int pgoff;
                                        eofaddr = CCHFLOOR(cch_info->fsize, bs);
                                        pgoff = cch_info->fsize - eofaddr;
                                        SET_BPTR(toptr,
                                           INC_BPTR(cbufs[i].buf,
                                           pgoff));
                                        CCH_MEMCLEAR(toptr,(bs - pgoff));
                                        cbufs[i].flags |= CCH_ZEROED;
				}
			}
                        ret = _cch_wrabuf(	cch_info,
						fio->fioptr,
						&cbufs[i],
                                        	BITS2BYTES(bs),
						BITS2BYTES(cbufs[i].filead),
						1,
						&cch_info->feof,
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
						's',		/*synchronous*/
#else
						'a',		/*asynchronous*/
#endif
						stat);

			if (ret == ERR) {
				errv = errv ? errv : stat->sw_error;
			}
		}
	}
	for (i=0; i<nb; i++) {
		if (cbufs[i].filead >= 0 && (cbufs[i].flags & CCH_WRITING)) {
			CCHWAITIO(fio->fioptr,(&cbufs[i]),stat,ret);
			if (ret == ERR) {
				errv = errv ? errv : stat->sw_error;
			}
		}
	}

	if (errv)
		ERETURN(stat, errv, 0);

	return(0);
}
