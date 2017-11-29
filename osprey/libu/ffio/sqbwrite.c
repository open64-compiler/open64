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


#pragma ident "@(#) libu/ffio/sqbwrite.c	92.2	10/29/99 21:40:31"


#include <ffio.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "sqbio.h"

#define MIN(a,b) ((a) < (b) ? (a) : (b))

/*
 *
 * Description:
 *	writes nbytes bytes, with *ubc unused bits, from bufptr to
 *	the next lower layer.
 * Parameters:
 *      fio     - Pointer to fdinfo block
 *	bufptr	- bit pointer to user's data
 *	nbytes	- Number of bytes to be written
 *	stat	- pointer to status return word
 *	fulp	- full or partial write mode flag
 *	ubc	- pointer to unused bit count
 * Returns:
 *	number of bytes written
 *	-1 if error
 */
ssize_t
_sqb_write(
struct fdinfo *fio,
bitptr bufptr,
size_t nbytes,
struct ffsw *stat,
int fulp,
int *ubc)
{
	int ret;
	int bs,  btomove;
	uint64 nbits;
	ssize_t moved;
	struct sqb_f *sqb_info;
	struct fdinfo *llfio;
	struct ffsw locstat;
	struct sqbio *sqbptr;
	int zero = 0;

	nbits = ((uint64)nbytes << 3) - *ubc;

	sqb_info = (struct sqb_f *)fio->lyr_info;
	llfio = fio->fioptr;

	moved = 0;

	if (fio->rwflag == READIN || fio->rwflag == POSITIN) {
		/* synchronize physical position with logical position */
		if (_sqb_sync(fio, &locstat, 1) < 0) {
			goto erret;
		}
	}

	fio->rwflag = WRITIN;
	bs = sqb_info->bufsiz>>3;
	sqbptr = sqb_info->sqbio_cur;
	while (nbits != 0) {
		if (sqbptr->status == IOACTIVE) {
			/* wait for the outstanding asynch i/o to complete */
			while (sqbptr->iostat.sw_flag == 0 || 
				sqbptr->iostat.sw_stat == 0) {
				ret = XRCALL(llfio,fcntlrtn) llfio, FC_RECALL, 
					&(sqbptr->iostat), &locstat);
				if (ret < 0) {
					goto erret;
				}
			}
			if (sqbptr->iostat.sw_error != 0) {
				ERETURN(stat, sqbptr->iostat.sw_error, 0);
			}
			if (sqbptr->iostat.sw_count != sqbptr->_iowritten) {
				ERETURN(stat, FDC_ERR_WRTERR, 0);
			}
			sqbptr->status = EMPTY;
			sqbptr->_cnt = sqb_info->bufsiz;
			CLRFFSTAT(sqbptr->iostat);
		}
		if (sqbptr->status == EMPTY) {
			sqbptr->_cnt = sqb_info->bufsiz;
		}
	
/*
 *		Move data from user to buffer
 */
		btomove = MIN(nbits, sqbptr->_cnt);
		MOV_BITS(sqb_info->_ptr, bufptr, btomove);
		SET_BPTR(bufptr, INC_BPTR(bufptr, btomove));
		nbits -= btomove;
		sqbptr->_cnt -= btomove;
		sqbptr->status = IODATA;
		if (sqbptr->_cnt == 0) {
			/* no room left in this buffer; start I/O on it */
			CLRFFSTAT(sqbptr->iostat);
			sqbptr->_iowritten = bs;
			if( XRCALL(llfio, writeartn) llfio,
				sqbptr->_base,(size_t) bs, &(sqbptr->iostat),
				FULL, &zero) < 0) {
				ERETURN(stat, sqbptr->iostat.sw_error,
				 (moved +7) >> 3);
			}
			sqbptr->status = IOACTIVE;
			sqb_info->sqbio_cur = sqb_info->sqbio_cur->nxt;
			sqbptr = sqb_info->sqbio_cur;
			sqb_info->_ptr = sqb_info->sqbio_cur->_base;
		}
		else {
			SET_BPTR(sqb_info->_ptr, 
				INC_BPTR(sqb_info->_ptr, btomove));
		}
		moved += btomove;
	}
	SETSTAT(stat, FFCNT, (moved + 7) >> 3);
	return ((moved + 7) >> 3);
erret:
	ERETURN(stat, locstat.sw_error, (moved +7) >> 3);

}

/*
 * Description: Flushes buffers if we have been writing. Waits for I/O
 *		to complete.
 * Parameters:
 *		fio     - Pointer to fdinfo block
 *		stat	- pointer to status return word
 * Returns: 	0 if OK
 *		ERR if error
 */
_sqb_flush(struct fdinfo *fio, struct ffsw *stat)
{
	struct sqb_f *sqb_info;
	struct fdinfo *llfio;
	struct sqbio *sqbptr;
	struct sqbio *sqborig;
	int ret, ubc;
	size_t bytes;

	if (fio->rwflag != WRITIN) 
		return(0); /* flush is a no-op if we haven't been writing */
/*
 * 	If there is data in the buffers, it will be written.
 * 	Outstanding asynchronous requests are waited for. 
 */
	sqb_info = (struct sqb_f *)fio->lyr_info;
	llfio = fio->fioptr;
	sqbptr = sqb_info->sqbio_cur;
	sqborig = sqbptr;
	do {
		if (sqbptr->status == IODATA) {
			if (sqbptr != sqborig)
				ERETURN(stat, FDC_ERR_INTERR, 0);
			bytes = (sqb_info->bufsiz - sqbptr->_cnt +7)>>3;
			ubc = bytes*8 - (sqb_info->bufsiz - sqbptr->_cnt);
			CLRFFSTAT(sqbptr->iostat);
			sqbptr->_iowritten = bytes;
			if ( XRCALL(llfio, writeartn) llfio,
				sqbptr->_base, (size_t)bytes, 
				&(sqbptr->iostat), FULL, &ubc) < 0) {
				ERETURN(stat, sqbptr->iostat.sw_error, 0);
			}
			sqbptr->status = IOACTIVE;
		}
		if (sqbptr->status == IOACTIVE) {
			while (sqbptr->iostat.sw_flag == 0 || 
			   sqbptr->iostat.sw_stat == 0) {
				ret = XRCALL(llfio,fcntlrtn) llfio, FC_RECALL, 
				&(sqbptr->iostat), stat);
				if (ret < 0) {
					return(ERR);
				}
			}
			if (sqbptr->iostat.sw_error != 0) {
				ERETURN(stat, sqbptr->iostat.sw_error, 0);
			}
			if (sqbptr->iostat.sw_count != sqbptr->_iowritten) {
				ERETURN(stat, FDC_ERR_WRTERR, 0);
			}
		}
		sqbptr->status = EMPTY;
		CLRFFSTAT(sqbptr->iostat);
		sqbptr = sqbptr->nxt;
	} while (sqbptr != sqborig);
	ret = XRCALL(llfio,flushrtn) llfio, stat);
	sqb_info->_ptr = sqbptr->_base;
	return(ret);
}

/*
 * Description:
 *	Waits for outstanding I/O to complete, and
 *	sets the status of each buffer to EMPTY.
 *	Should be called only if we were reading.
 *	Synchronizes physical and logical postions if
 *	the parameter sync is set.
 * Returns:
 *	0 or positive integer if OK
 *	-1 if error
 */
_ffseek_t
_sqb_sync(struct fdinfo *fio, struct ffsw *stat, int sync)
{
	struct sqb_f *sqb_info;
	struct fdinfo *llfio;
	struct sqbio *sqbptr;
	struct sqbio *sqborig;
	_ffseek_t ret = 0; 
	off_t bytes = 0;

	sqb_info = (struct sqb_f *)fio->lyr_info;
	llfio = fio->fioptr;
	sqbptr = sqb_info->sqbio_cur;
	sqborig = sqbptr;
	do {
		if (sqbptr->status == IOACTIVE) {
			while (sqbptr->iostat.sw_flag == 0 || 
			   sqbptr->iostat.sw_stat == 0) {
				if( XRCALL(llfio,fcntlrtn) llfio, FC_RECALL, 
					&(sqbptr->iostat), stat) < 0) {
					return(ERR);
				}
			}
			sqbptr->_cnt = sqbptr->iostat.sw_count<<3;
			sqbptr->status = IODATA;
		}
		if (sqbptr->status == IODATA) {
			bytes += sqbptr->_cnt>>3;
		}
		sqbptr->status = EMPTY;
		CLRFFSTAT(sqbptr->iostat);
		sqbptr = sqbptr->nxt;
	} while (sqbptr != sqborig);
	sqb_info->_ptr = sqbptr->_base;
	if (sync) {
		/* seek back to logical position */
		bytes = -bytes;
		if (sqb_info->ffci_flags & FFC_SEEKA) {
			ret = XRCALL(llfio, seekrtn) llfio, bytes, 1, stat);
		}
		else {
			ret = XRCALL(llfio, posrtn) llfio, FP_RSEEK, &bytes,
			 1, stat);
		}
	}
	return(ret);
}
