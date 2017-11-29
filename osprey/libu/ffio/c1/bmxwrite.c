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


static char USMID[] = "@(#) libu/ffio/c1/bmxwrite.c	92.0	10/08/98 14:57:41";


#include <sys/param.h>
#include <sys/types.h>
#include <sys/iosw.h>
#include <sys/bmxctl.h>
#include <sys/file.h>
#include <tapereq.h>
#include <errno.h>
#include <ffio.h>
#include "bmxio.h"
#include <liberrno.h>
/*
 *	_bmx_write() calls the tape handling routines, after doing
 *	appropriate conversions on the parameters.
 *
 */
_bmx_write(struct fdinfo *fio, bitptr bufptr, int nbytes, struct ffsw *stat, int fulp, int *ubc)
{
int ret, state;
char *buf;

	buf = BPTR2CP(bufptr);
	if ((BPBITOFF(bufptr) & 7) != 0 || *ubc != 0)
		ERETURN(stat, FDC_ERR_UBC, 0);

/*
 *	If the number of nbytes to write is zero and we are writing
 *	in full record mode, write an end of record.
 */
	fio->rwflag = WRITIN;
	if (fulp == FULL)
		state = BMX_EOR;
	else
		state = BMX_DATA;
	ret = __bmxwrite(fio->lyr_info, buf, nbytes, state);
	if (ret < 0)
		ERETURN(stat, errno, 0);
	if (fulp == FULL) {
		fio->last_recbits = fio->recbits;
		fio->recbits = 0;
	}
	else
		fio->recbits += ret *8;
	if (fulp == FULL) {
		SETSTAT(stat, FFEOR, nbytes);
	}
	else {
		SETSTAT(stat, FFCNT, nbytes);
	}
	return(ret);
}

/*
 *	write on-line tape file
 */

__bmxwrite(BMXFIL *f, const void *uda, long bytes, long state) 
{
 

	register struct bmxlist *lstptr;
        register int    lstcnt, bufcnt, n;
        register char   *bufptr;
	register struct bmxio *currentio;
	struct bmxio 	*nextio;

/*
 *	Move frequently used values into registers.
 */
	lstptr = f->bmx_lstptr;		/* pointer to current list     */
	lstcnt = f->bmx_lstcnt;		/* current offset in list      */
	bufptr  = f->bmx_bufptr;	/* current pointer into buffer */
	bufcnt  = f->bmx_bufcnt;	/* current byte count in buffer */
	f->bmx_flag &= ~BMXIO_ERRSEEN;
	currentio = f->bmx_iocurrent;

/*
 * 	Check for write after read 
 */
        if (( (f->bmx_flag & BMXIO_RW) == 0 ) && ((bufptr != 0) ||
		(f->bmx_flag & BMXIO_SPPROC)!= 0)) {
			bufptr = 0;
			if(_bmx_quiet(f) < 0){
				return(-1);
			}
			/* Issue a positioning request of 0 so that */
			/* we can take care of read-ahead */
			/* This also clears out the lists */
			if(_bmx_stpos(f,FP_TPOS_BACK,0,0,0,0)){
				return(-1);
			}
			currentio = f->bmx_iocurrent;
	}
/*
 *	Check for first i/o request or first write following positioning.
 */
	if ( bufptr == 0 ) {
		if ( f->bmx_tpos ) {
			/* Wait for positioning to finish */
			if( _tape_tpwait( f->bmx_fd, &(f->bmx_tpos)) != 0)
				return(-1);
		}
		f->bmx_flag |= BMXIO_RW;
		bufptr = currentio->bmx_base;
		lstcnt = f->bmx_lstsz;
		lstptr = currentio->bmx_list;
		bufcnt = f->bmx_bufsz;
		if (( f->bmx_flag & BMXIO_MODELE) && (f->bmx_flag & BMXIO_SPPROC)){
			/* Have we read anything from fake buffer memory? */
			if (f->bmx_totblk >= f->bmx_bmblk)
				f->bmx_totblk = f->bmx_bmblk;
			else{
				/* If we've read anything, then it's all gone */
				f->eov_current = NULL;
				f->bmx_totblk = 0;
				f->bmx_bmblk = 0;	
			}
		}
	}

/*
 *	Make sure there is no outstanding i/o on this buffer.
 */
	if (currentio->bmx_busy == ACTIVE){
		WAITBMXIO(f,currentio);
		currentio->bmx_busy = INACTIVE;
		lstcnt = f->bmx_lstsz;
		/* Clear the list */
		lstptr = currentio->bmx_list;
		memset(lstptr,0,sizeof(struct bmxlist) * lstcnt);

		if ( BMXIO_ERROR(currentio->bmx_iosw) ) {
			(void)_bmx_quiet(f);
			(void)_bmx_clrerr(f);
			_bmx_clear(f);	
			errno = currentio->bmx_iosw.sw_error;
			f->bmx_flag |= BMXIO_ERRSEEN;
			return(-1);
		}
	}

	/* If writing a tapemark */
	if ( state==BMX_EOF )  {
		lstptr->state = BMS_EOF;
		goto writeit;
	}

	if ( bytes > bufcnt ){ 		/* not enough room */
		errno = FETAPBSX;
		return(-1);
	}
/*
 *	Copy data from user into i/o buffer.  Update pointers to reflect
 *	the data moved.
 */

	F_TRACE("wcopy", bufptr, uda, bytes );

	(void) memcpy( bufptr, uda, bytes );
	bufcnt -= bytes;
	bufptr += bytes;
        lstptr->bytes += bytes;

	currentio->bmx_busy = DATA;

	if ( lstptr->bytes > f->bmx_mbs ){
		errno = FETAPBSX;
		return(-1);
	}
/*
 *	If BMX_EOR write has been requested, update pointers to skip to  
 *	the next sector boundry.  This should be where the next block will
 *	go.
 */	
	if ( state==BMX_EOR )  {
		n = rtoc(lstptr->bytes) - lstptr->bytes;
		bufptr += n;
		bufcnt -= n;
		lstptr->state = BMS_EOR; 
		lstptr++;
		lstcnt--;
/*
 *	If not enough room in the buffer for another block or the list
 *	size has been exhausted, go ahead and write the data.
 */
		if ( (bufcnt < f->bmx_mbs) || (lstcnt == 0) ) {
writeit:
			currentio->bmx_iosw.sw_count = 0;
			currentio->bmx_iosw.sw_error = 0;
			currentio->bmx_iosw.sw_flag = 0;
			f->bmx_listptr = currentio->bmx_list;
			n = writea( f->bmx_fd, currentio->bmx_base,
				f->bmx_bufsz, &currentio->bmx_iosw, NULL);
			if (n < 0){
				f->bmx_flag |= BMXIO_ERRSEEN;
				return(-1);
			}
			currentio->bmx_busy = ACTIVE;
			nextio = currentio->bmx_nxt;
			if (nextio == NULL){
				nextio = f->bmx_iofirst;
			}	
			f->bmx_iocurrent = nextio;
			bufptr = nextio->bmx_base;
			bufcnt = f->bmx_bufsz;
			lstptr = nextio->bmx_list;
			lstcnt = f->bmx_lstsz;
		}
	}

	f->bmx_bufptr = bufptr;
	f->bmx_bufcnt = bufcnt;
	f->bmx_lstptr = lstptr;
	f->bmx_lstcnt = lstcnt;
	return(bytes);

}

/*
 * bmx_flush()
 *	This is a no-op.
 */
_bmx_flush(fd, stat)
int fd;
struct ffsw *stat;
	{
	return(0);
	}

