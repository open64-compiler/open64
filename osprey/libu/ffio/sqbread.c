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


#pragma ident "@(#) libu/ffio/sqbread.c	92.2	10/29/99 21:40:31"


#include <stdio.h>
#include <string.h>
#include <ffio.h>
#include "sqbio.h"

#define MIN(a,b) ((a) < (b) ? (a) : (b))

/*
 *	Buffering layer read. Reads nbytes bytes, with ubc
 *		unused bits, into bufptr. 
 *
 * Parameters:
 *      fio     - Pointer to fdinfo block
 *      bufptr  - bit pointer to user's data
 *      nbytes  - Number of bytes to be read
 *      stat    - pointer to status return word
 *      fulp    - full or partial read mode flag
 *      ubc     - pointer to unused bit count
 *	Returns: number of bytes moved
 *		-1 if error.
 */
ssize_t
_sqb_read(
struct fdinfo *fio, 
bitptr bufptr, 
size_t nbytes, 
struct ffsw *stat, 
int fulp,
int *ubc)
{
	uint64 nbits;
	int64  btomove;
	uint64 moved = 0;	/* bits that have been moved to the user */
	uint64 bs;
	int   totbs;
	struct sqb_f *sqb_info;
	struct ffsw locstat;
	struct fdinfo *llfio;
	struct sqbio *sqbptr;
	struct sqbio *sqbnxt;
	int zero= 0;

	sqb_info = (struct sqb_f *)fio->lyr_info;
	nbits = ((uint64)nbytes << 3) - *ubc; /* number of bits left to move */
	llfio = fio->fioptr;

	if (nbits == 0) {
		SETSTAT(stat, FFCNT, 0);
		return(0);
	}
	totbs =  sqb_info->bufsiz >> 3;	/* buffer size in bytes */

	if (fio->rwflag == WRITIN){
		/* read after write */
		/* Flush the last buffer and wait for writes to complete */
		if (_sqb_flush(fio, &locstat) < 0) {
			goto erret;
		}
	}
	else if (fio->rwflag == POSITIN) {
		/* could be some I/O hasn't been started */
		sqbptr = sqb_info->sqbio_cur;
		if (sqbptr->status != EMPTY ) {
			sqbnxt = sqbptr;
			do {
				if (sqbnxt->status == EMPTY) {
					CLRFFSTAT(sqbnxt->iostat);
					if (XRCALL(llfio, readartn) llfio,
					   sqbnxt->_base, (size_t) totbs,  
					   &(sqbnxt->iostat), FULL, &zero) < 0){
						ERETURN(stat,
						 sqbnxt->iostat.sw_error,
						 (moved +7)>>3);
					   }
					sqbnxt->status = IOACTIVE;
				}
				sqbnxt = sqbnxt->nxt;
			} while (sqbnxt != sqbptr);
		}
	}
	sqbptr = sqb_info->sqbio_cur;

	fio->rwflag = READIN;

	while (nbits != 0) {
		if (sqbptr->status == EMPTY) {
			/* if the user is asking for a large amount */
			/* of data, read directly into user's buffer */
			if (nbits >= sqb_info->bufsiz) {
				bs = nbits - nbits % sqb_info->bufsiz;
				CLRFFSTAT(sqbptr->iostat);
				if (XRCALL(llfio, readartn) llfio,
				   bufptr, (size_t)(bs>>3), 
				   &(sqbptr->iostat), FULL, &zero) < 0) {
					ERETURN(stat, sqbptr->iostat.sw_error, 0);
				}
				sqbptr->status  = IOACTIVE;
				sqbptr->userdata =  1;
				sqbnxt = sqbptr->nxt;
			}
			else
				sqbnxt = sqbptr;
			while (sqbnxt->status == EMPTY) {
				CLRFFSTAT(sqbnxt->iostat);
				if (XRCALL(llfio, readartn) llfio,
				   sqbnxt->_base, (size_t)totbs,  
				   &(sqbnxt->iostat), FULL, &zero) < 0) {
					ERETURN(stat, sqbnxt->iostat.sw_error,
					 (moved +7)>>3);
				}
				sqbnxt->status = IOACTIVE;
				sqbnxt = sqbnxt->nxt;
			}
		}
		if (sqbptr->status == IOACTIVE) {
			/* wait for the outstanding asynch i/o to complete */
			while (sqbptr->iostat.sw_flag == 0 ||
			sqbptr->iostat.sw_stat == 0) {
				if (XRCALL(llfio,fcntlrtn) llfio, FC_RECALL,
				   &(sqbptr->iostat), &locstat) < 0) 
					goto erret;
				}
			sqbptr->status = IODATA;
			if ((FFSTAT(sqbptr->iostat) == FFCNT) ||
			 (FFSTAT(sqbptr->iostat) == FFEOR)){
				sqbptr->_cnt = sqbptr->iostat.sw_count<<3;
			}
			else if (FFSTAT(sqbptr->iostat) == FFEOF ||
			 FFSTAT(sqbptr->iostat) == FFEOD) {
				sqbptr->_cnt = 0;
				sqbptr->userdata  = 0;
				goto done;	
			}
			else {
				*stat = sqbptr->iostat;
				sqbptr->_cnt = 0;
				sqbptr->userdata  = 0;
				return(ERR);
			}
		}
	
/*
 *		Move data from buffer to user
 */
		btomove = MIN(nbits, sqbptr->_cnt);
		if (!sqbptr->userdata) {
			MOV_BITS(bufptr, sqb_info->_ptr, btomove);
		}
		else {
			sqbptr->userdata = 0;
			if (sqbptr->_cnt > btomove)
				ERETURN(stat, FDC_ERR_INTERR, (moved+7)>>3);
		}
		SET_BPTR(bufptr, INC_BPTR(bufptr, btomove));
		moved += btomove;
		sqbptr->_cnt -= btomove;
		nbits -=btomove;
		if (sqbptr->_cnt == 0 && sqbptr->status != IOACTIVE) {
			/* This buffer is empty. Start I/O on it. */
			CLRFFSTAT(sqbptr->iostat);
			if (XRCALL(llfio, readartn) llfio,
			   sqbptr->_base, (size_t)(sqb_info->bufsiz>>3),  
			   &(sqbptr->iostat), FULL, &zero) < 0) {
				ERETURN(stat, sqbptr->iostat.sw_error,
				   (moved +7)>>3);
			}
			sqbptr->status = IOACTIVE;
			sqb_info->sqbio_cur = sqb_info->sqbio_cur->nxt;
			sqbptr = sqb_info->sqbio_cur;
			sqb_info->_ptr = sqb_info->sqbio_cur->_base;
		}
		else
			SET_BPTR(sqb_info->_ptr, 
				INC_BPTR(sqb_info->_ptr, btomove));
	}
	SETSTAT(stat, FFCNT, (moved + 7) >> 3);
	*ubc = ((moved+7) & ~(7)) - moved;
	return ((moved + 7) >> 3);

done:
	if (moved > 0) {
		SETSTAT(stat, FFCNT, (moved + 7) >> 3);
		*ubc = ((moved+7) & ~(7)) - moved;
		return((moved+7)>>3);
	}
	else {
		SETSTAT(stat, FFSTAT(sqbptr->iostat),0);
		*ubc = 0;
		return(0);
	}
erret:
	ERETURN(stat, locstat.sw_error, (moved +7)>>3);
}
