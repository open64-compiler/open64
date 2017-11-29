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


static char USMID[] = "@(#) libu/ffio/c1/bmxread.c	92.0	10/08/98 14:57:41";


#include <stdio.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/bmxctl.h>
#include <sys/file.h>
#include <sys/iosw.h>
#include <tapereq.h>
#include <errno.h>
#include <liberrno.h>
#include <ffio.h>
#include "bmxio.h"

#define	 ERET(x) { ret = x; goto badret; }

/*
 *	read on-line tape file
 *
 *	Returns:   number of bytes read if all ok
 *		   0 if EOF or EOD. sw_stat field of iosw is set
 *		   ERR if error. sw_error field of iosw is set
 */

_bmx_read(fio, bbuf, bytes, stat, fulp, ubc)
struct fdinfo *fio;
int bytes, fulp, *ubc;
bitptr bbuf;
struct ffsw *stat;

{
 
#if TRACE
	struct bmxctl ctl;
#endif
	BMXFIL 	*f;
	char	*uda;
	long 	state;
	register struct bmxlist *lstptr;
        register char  *bufptr, *limit;
        register int    recl, lstcnt, bufcnt, n,  count, eor_found=0;
	register struct bmxio *currentio;
	long s;
	struct bmxio   *oldio;
	struct bmxio   *ioptr;
	int ret;

	F_TRACE("bmx_read",f,bytes,state);
	uda = BPTR2CP(bbuf);
	if ((BPBITOFF(bbuf) & 7) != 0 || *ubc != 0) {
		ERETURN(stat, FDC_ERR_UBC, 0);
	}
		
/*	
 *	If the number of bytes to read is zero and reading in full
 *	record mode, skip to the end of record.  If in partial
 *	record mode the position remains as is.
 */
	fio->rwflag = READIN;
	if (fulp == FULL)
		state = BMX_EOR;
	else
		state = BMX_DATA;
	
	f = (BMXFIL *)fio->lyr_info;
/*
 *      Move frequently used values into registers.
 */
        lstptr = f->bmx_lstptr;          /* pointer to current list	*/
        lstcnt = f->bmx_lstcnt;          /* current offset in list	*/
        bufptr = f->bmx_bufptr;          /* current pointer into buffer  */
        bufcnt = f->bmx_bufcnt;          /* current byte count in buffer */
        recl   = f->bmx_recl;            /* transferred record count	*/
	currentio = f->bmx_iocurrent;
	limit   = currentio->bmx_base + f->bmx_bufsz;
	count   = bytes;
/*
 *	 Clear the tape mark read status, EOR status, and error seen 
 */
	f->bmx_flag &= ~(BMXIO_TPMK | BMXIO_EOR | BMXIO_ERRSEEN);

/*	
 *	Check for reading after writing
 */
        if (( (f->bmx_flag & BMXIO_RW) != 0 ) && (bufptr != 0)) {
		if ((f->bmx_flag & BMXIO_SPPROC) !=0){
			/* Reading after writing is OK during EOV processing*/
			recl = 0;
			bufptr = 0;
			if (_bmx_quiet(f) < 0) {
				ERET(errno);
			}
		}
		else {
			/* Not in special EOV processing */
			ERET(FERDAFWR);
		}
	} 

/*
 * 	First read or first read following positioning 
 */
        if ( bufptr == 0 ) {
		if ( f->bmx_tpos ) {
			if( _tape_tpwait( f->bmx_fd, &(f->bmx_tpos)) != 0) {
				ERET(errno);
			}
		}

                f->bmx_flag &= ~(BMXIO_RW);
		if ((f->bmx_flag & BMXIO_SPPROC) && (f->bmx_flag & BMXIO_MODELE)) {
			/* Is the user asking to read data from the library
			 * buffer during special processing? 
			 */
			if ( (f->bmx_flag & BMXIO_WEOV) && (f->bmx_totblk == 0))
				goto spprocread;
		}
	}
	if (currentio->bmx_busy == INACTIVE){
		/* 
		 * start reading
		 */
		ioptr = currentio;
		if (currentio != f->bmx_iofirst){
			ERET(FEINTUNK);
		}
		lstcnt = f->bmx_lstsz;
		while (ioptr != NULL){
			if (ioptr->bmx_busy != INACTIVE){
				ERET(FEINTUNK);
			}
			/* Clear the list */
			memset(ioptr->bmx_list,0,sizeof(struct bmxlist)*lstcnt);

			f->bmx_listptr = ioptr->bmx_list;
			ioptr->bmx_iosw.sw_flag = 0;
			ioptr->bmx_iosw.sw_count = 0;
			ioptr->bmx_iosw.sw_error = 0;
			if (reada(f->bmx_fd, ioptr->bmx_base,
				 f->bmx_bufsz, &ioptr->bmx_iosw, 0) < 0) {
				ERET(errno);
			}
			ioptr->bmx_busy = ACTIVE;

			ioptr= ioptr->bmx_nxt;
		}
                bufptr = currentio->bmx_base;
		lstptr = currentio->bmx_list;
        }
	
/*
 *	Go through this loop at least once, in case the user
 *	specified a read of 0  bytes.
 */
	do{
/*
 *		Wait until i/o is finished for current buffer.
 */
		if ( currentio->bmx_busy == ACTIVE) {
			WAITBMXIO(f,currentio);
			currentio->bmx_busy = DATA;
			if (BMXIO_ERROR(currentio->bmx_iosw)){
				(void)_bmx_quiet(f);
				(void)_bmx_clrerr(f);
				_bmx_clear(f);
				f->bmx_flag |= BMXIO_ERRSEEN;
				ERET(currentio->bmx_iosw.sw_error);
			}
			currentio->bmx_iosw.sw_error = 0;

			bufptr = currentio->bmx_base;
					
/* 
 *			 Since one record does not span 2 list entries
 *			 set BMXIO_NXT
 */
			f->bmx_flag |= BMXIO_NXT;
		}	 /* if currentio->bmx_busy == ACTIVE */
		else if (currentio->bmx_busy == INACTIVE){
			ERET(FEINTUNK);
		}
/*
 *	If BMXIO_NXT, then the current block has been emptied and the next
 *	entry will be used.  Here is where we need to check for EOF
 *	and unrecovered data errors.
 */
		if ( f->bmx_flag & BMXIO_NXT ) {
			f->bmx_flag &= ~(BMXIO_NXT);
			recl = 0;
/*
 *	If EOF, clear bufptr to make sure that we issue another read
 *	request to fill the list on the next request.  Also indicate
 *	that an end of tape block was encountered. 
 */
			if ( lstptr->state == BMS_EOF ){
				ret = 0;
				f->bmx_flag |= BMXIO_NXT | BMXIO_TPMK;
				++lstptr;
				--lstcnt;
				f->bmx_recl=0;
				if (lstcnt != 0){
					if (lstptr->state != 0){
						/* We do not expect
					    	 * data following a tape mark
						 */
						errno=FEINTUNK;
						ret = -1;
					}
				}
				if ((f->bmx_flag & BMXIO_SPPROC) !=0){
					if (f->bmx_totblk > 0)
						f->bmx_totblk--;
				}
				ret |= _bmx_quiet(f);
				ret |= _bmx_clrerr(f);
				_bmx_clear(f);
				if (ret) {
					ERET(errno);
				}
				SETSTAT(stat, FFEOF, 0);
				return(0);
			}
			else if (lstptr->state == BMS_EOD){
				ret = 0;
				f->bmx_flag |= BMXIO_NXT;
				f->bmx_recl = 0;
				ret |= _bmx_quiet(f);
				ret |= _bmx_clrerr(f);;
				_bmx_clear(f);
				f->bmx_totblk=0;
				if (ret) {
					ERET(errno);
				}
				SETSTAT(stat, FFEOD, 0);
				return(0);
			}
			if ( lstptr->flags != 0 ) {
				ret = 0;
				if ( f->bmx_flag & BMXIO_SKIP )
				   (void)_bmx_bdr( f, NULL, &s, BMX_SKIPBAD );
				else if ( f->bmx_flag & BMXIO_ACPT )
				   ret = _bmx_bdr( f, uda, &s, BMX_ACPTBAD );
				f->bmx_flag |= BMXIO_ERRSEEN;
				ERETURN(stat, FETAPUTE, ret);
			}
			if ( lstptr->bytes == 0 ) {
				f->bmx_flag |= BMXIO_ERRSEEN;
				ERET(FEINTTAP);
			}
		}	/* If flag & BMXIO_NXT */
/*
 *	Determine if there is enough data in this list entry to satisfy
 *	this request.
 */
		if ( count <= (lstptr->bytes-recl) )
			n = count;
		else {				/* not enough data */
			n = lstptr->bytes-recl;
		}
/*
 *	Copy data out of the i/o buffer into the user's data area.  Update
 *	pointers to reflect the data moved.
 */
		if ( (bufptr >= limit) && (n != 0) ){
			ERET(FEINTUNK);
		}

		(void) memcpy( uda, bufptr, n );
		bufcnt -= n;
		bufptr += n;
		recl   += n;
		count  -= n;
		uda    += n;
/*
 *	If BMX_EOR read has been requested, or the list entry has been
 *	exhausted,  update pointers to skip to the next sector boundry.
 *	This should be the next block of the tape.
 */
		if ((lstptr->state == BMS_EOR ) &&
			((state == BMX_EOR) ||(recl == lstptr->bytes))){
			n    = rtoc(lstptr->bytes) - recl;
			bufptr += n;
			bufcnt -= n;
			if ( lstptr->state == BMS_EOR ) {
				eor_found = 1; 
				f->bmx_flag |= BMXIO_NXT;
			}
			if (recl == lstptr->bytes)
				f->bmx_flag |= BMXIO_EOR;
			lstcnt--;
			lstptr++;
/*
 *	If there is more room in the list, check the next list entry
 *	to make sure there is another block in the buffer.
 *	If there are no more blocks in the buffer,  point to the next
 *	queue.
 */
			if ((lstcnt == 0) || (lstptr->state == 0)) {
				lstcnt = f->bmx_lstsz;
				memset(currentio->bmx_list,0,
					sizeof(struct bmxlist)*lstcnt);
				currentio->bmx_iosw.sw_count = 0;
				currentio->bmx_iosw.sw_flag = 0;
				currentio->bmx_iosw.sw_error = 0;
				f->bmx_listptr = currentio->bmx_list;
				oldio = currentio;
				currentio = currentio->bmx_nxt;
				if (currentio == NULL)
					currentio = f->bmx_iofirst;
				f->bmx_iocurrent = currentio;
				oldio->bmx_busy = INACTIVE;
				if ((f->bmx_flag & BMXIO_SPPROC) == 0) {
					/* Issue a read to fill buffer */
					/* we just emptied. */
					n = reada(f->bmx_fd, 
						oldio->bmx_base, f->bmx_bufsz,
						&oldio->bmx_iosw, NULL);
					if (n < 0) {
						f->bmx_flag |= BMXIO_ERRSEEN;
						(void)_bmx_clrerr(f);
						ERET(errno);
					}
					oldio->bmx_busy = ACTIVE;
					bufptr = currentio->bmx_base;
					bufcnt = f->bmx_bufsz;
					lstptr = currentio->bmx_list;
					lstcnt = f->bmx_lstsz;
				}
				else {
					/* Don't do read ahead while */
					/* in special processing */
					memset(currentio->bmx_list,0,sizeof(struct bmxlist) * lstcnt);
					bufptr = 0;
					recl = 0;
					lstptr = currentio->bmx_list;
					lstcnt = f->bmx_lstsz;
					bufcnt = f->bmx_bufsz;
				}
			}
		}
	} while( (count != 0) && (!eor_found) );

/*
 *      Update pointers in the bmx file table.
 */

        f->bmx_bufptr = bufptr;
        f->bmx_bufcnt = bufcnt;
        f->bmx_lstptr = lstptr;
        f->bmx_lstcnt = lstcnt;
        f->bmx_recl   = recl;
	if ((f->bmx_flag & BMXIO_SPPROC) && (f->bmx_flag & BMXIO_MODELE))
		if ((f->bmx_flag & BMXIO_NXT) && (f->bmx_totblk > 0))
			f->bmx_totblk--;

	F_TRACE("return",bytes-count,bytes,count);

	ret = bytes - count;
	fio->recbits += ret *8;
	SETSTAT(stat, FFCNT, ret);
	if (f->bmx_flag & BMXIO_EOR) {
		SETSTAT(stat, FFEOR, ret);
		fio->last_recbits = fio->recbits;
		fio->recbits = 0;
	}
	return(ret);
badret:
	ERETURN(stat, ret, 0);

spprocread:
/* 
 * 	Read from the data buffered by the library.
 */
	if (f->bmx_buflstcnt == 0 || f->bmx_cbuflst->state == 0){
		if (f->eov_current == NULL) {
			f->bmx_buflstcnt = 0;
			SETSTAT(stat, FFEOD, 0);
			return(0);
		}
		f->eov_current = f->eov_current->eov_nxt;
		if (f->eov_current != NULL){
			f->bmx_cbuflst = f->eov_current->bmx_buflst;
			f->bmx_cbufmem = f->eov_current->bmx_bufmem;
			f->bmx_buflstcnt = f->bmx_lstsz;
			f->bmx_flag |= BMXIO_NXT;
		}
		else {
			SETSTAT(stat, FFEOD, 0);
			return(0);
		}
	}
	if (f->bmx_flag & BMXIO_NXT) {
		f->bmx_flag &= ~BMXIO_NXT;
		f->bmx_recl = 0;
		if (f->bmx_cbuflst->state == BMX_EOF) {
			f->bmx_cbuflst++;
			f->bmx_buflstcnt--;
			f->bmx_flag |= BMXIO_NXT | BMXIO_TPMK;
			f->bmx_recl = 0;
			SETSTAT(stat, FFEOF, 0);
			return(0);
		}
		else if (f->bmx_cbuflst->state == 0) {
			f->bmx_buflstcnt = 0;
			SETSTAT(stat, FFEOD, 0);
			return(0);
		}
	}
	if (f->bmx_cbuflst->bytes - f->bmx_recl > count) {
			n = count;
	}
	else {
		n = f->bmx_cbuflst->bytes - f->bmx_recl;
	}
	bufptr = f->bmx_cbufmem;
	(void) memcpy(uda, bufptr, n);
	f->bmx_cbufmem += n;
	f->bmx_recl += n;
	if ((state == BMX_EOR) || (f->bmx_recl == f->bmx_cbuflst->bytes)){
                if (f->bmx_cbuflst->state == BMX_EOR)
			f->bmx_flag |= BMXIO_NXT;
		if (f->bmx_recl == f->bmx_cbuflst->bytes)
			f->bmx_flag |= BMXIO_EOR;
		f->bmx_cbufmem += rtoc(f->bmx_cbuflst->bytes) - f->bmx_recl;
		f->bmx_cbuflst++;
		f->bmx_buflstcnt--;
	}
	fio->recbits += n *8;
	SETSTAT(stat, FFCNT, n);
	if (f->bmx_flag & BMXIO_EOR) {
		SETSTAT(stat, FFEOR, n);
		fio->last_recbits = fio->recbits;
		fio->recbits = 0;
	}
	return(n);
}
