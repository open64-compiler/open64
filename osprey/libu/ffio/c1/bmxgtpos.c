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


static char USMID[] = "@(#) libu/ffio/c1/bmxgtpos.c	92.0	10/08/98 14:57:41";


#include  <sys/types.h>
#include  <sys/bmxctl.h>
#include  <sys/file.h>
#include  <sys/iosw.h>
#include  <sys/stat.h>
#include  <sys/jtab.h>
#include  <sys/param.h>
#include  <fcntl.h>
#include  <tapereq.h>
#include  <stdio.h>
#include  <malloc.h>
#include  <errno.h>
#include  <liberrno.h>
#include  <ffio.h>
#include  "bmxio.h"
#include  "../sysio.h"

/*
 *	Get tape position
 *	Returns: 0 if OK
 *		-1 if error (errno is set)
 */

_bmx_gtpos(BMXFIL *f, long *pa, long palen, long synch)
{
	register int	lib, lstcnt;
	struct bmxlist	*lstptr;
	struct tsdata	tsdata;
	int 		vsn[MAXVSN];
	struct bmxio	*ioptr;
	struct eovbuf	*eovptr;
	int n;
	int partial;
	int eovret = 0;
	int syncdone = 0;
	struct bmxio *nextio;
	int ret;

	if ( f->bmx_tpos ) {
		if( _tape_tpwait( f->bmx_fd, &(f->bmx_tpos) ) != 0)
			return(-1); 
	}

/*
 *      If previous i/o request was a write request make sure the list
 *      is flushed before getting the position.
 */
 
	partial = NO;
        if ( f->bmx_flag & BMXIO_RW ) {
		if (f->bmx_lstptr->state == 0 &&
			f->bmx_lstptr->bytes != 0) {
				/* If we just wrote part of a record */
				/* don't try to flush to tape */
				partial = YES;
				lib = f->bmx_lstsz - f->bmx_lstcnt;
		}
                else  {
			if ( f->bmx_lstcnt != f->bmx_lstsz ) {
				f->bmx_flag &= ~BMXIO_ERRSEEN;
				f->bmx_listptr = f->bmx_iocurrent->bmx_list;
				f->bmx_iocurrent->bmx_iosw.sw_flag = 0;
				f->bmx_iocurrent->bmx_iosw.sw_error = 0;
				f->bmx_iocurrent->bmx_iosw.sw_count = 0;
                        	n = writea( f->bmx_fd, f->bmx_iocurrent->bmx_base, f->bmx_bufsz,
					&f->bmx_iocurrent->bmx_iosw,NULL);
				if (n < 0){
					f->bmx_flag |= BMXIO_ERRSEEN;
		                        return(-1);
				}
				f->bmx_iocurrent->bmx_busy = ACTIVE;
				nextio = f->bmx_iocurrent->bmx_nxt;
				if (nextio == NULL) {
					nextio = f->bmx_iofirst;
				}
				f->bmx_iocurrent = nextio;
				f->bmx_bufptr = nextio->bmx_base;
				f->bmx_bufcnt = f->bmx_bufsz;
				f->bmx_lstptr = nextio->bmx_list;
				f->bmx_lstcnt = f->bmx_lstsz;
			}
			/* Wait for i/o to be quiet. */
			if (f->bmx_bufptr != 0) {
				eovret = _bmx_eovq(f);
				if (eovret < 0)
					return(-1);
				_bmx_clear(f);
			}
                }
	}
		
	if (f->bmx_flag & BMXIO_RW){
		if (f->bmx_bufptr != 0) {
/*
 *			If synch was requested and we were writing, issue the
 *			synch request.
 */
			if (synch == 1){
				/* If we encountered EOV while waiting */
				/* for outstanding I/O, then don't */
				/* do the synch, it will just cause an */
				/* error. */
				if (eovret == 0) {
					if (partial == YES) {
						errno = FDC_ERR_NOTREC;
						return(-1);
					}
					if((ret = _tape_sync(f->bmx_fd)) < 0)
						return(-1);
					else if (ret == ENOSPC)
						eovret = 1;
					syncdone = 1;
				}
			}
		}
        } else {
		/* Wait for i/o to be quiet. */
		if( _bmx_eovq(f) < 0)
			return(-1);
		LOOP_SYSCALL(ret, ioctl( f->bmx_fd, BXC_WDN, 0 ));
		if (ret < 0)
			return(-1);
	}
	
	lib = 0;
        if ( (f->bmx_flag & BMXIO_RW) == 0) {
/*
 *		If we were reading, count the blocks that are in the
 *		library's buffer.
 */
		lstcnt = f->bmx_lstcnt;
		lstptr = f->bmx_lstptr;
		while (lstcnt != 0) {
			if (( lstptr->state == 0 ) || (lstptr->state == BMS_EOD))
				break;
			lstcnt--;
			lstptr++;
			lib++;
		}
		/* Now count all the outstanding blocks from read-ahead */
		ioptr = f->bmx_iofirst;
		while (ioptr != NULL){
			if (ioptr != f->bmx_iocurrent){
				if (ioptr->bmx_busy == DATA){
					lstcnt = f->bmx_lstsz;
					lstptr = ioptr->bmx_list;
					while (lstcnt != 0){
						if ((lstptr->state == 0) || (lstptr->state == BMS_EOD))
							break;
						lib++;
						lstcnt--;
						lstptr++;
					}
				}
			}
			ioptr = ioptr->bmx_nxt;
		}
	}
	if (f->bmx_flag & BMXIO_MODELE){
		if ((f->bmx_flag & BMXIO_EOVSEEN) &&
			((f->bmx_flag & BMXIO_SPPROC) == 0)){
			/* Count blocks in the library buffer. */
			lstcnt = f->bmx_buflstsz;
			eovptr = f->eov_first;
			while (eovptr != NULL){
				lstptr = eovptr->bmx_buflst;
				while (lstcnt > 0){
					if ((lstptr->state == 0) || (lstptr->state == BMS_EOD))
						break;
					lib++;
					lstcnt--;
					lstptr++;
				}
				eovptr = eovptr->eov_nxt;
				lstcnt = f->bmx_buflstsz;
			}
		}
	}
	if( _tape_tptsi (&(f->tsireq), &tsdata, vsn) != 0)
		return(-1);
	if (syncdone == 1) {
		/* We really should not have to check for tsdata.ts_bmblk > 0 */
		/* because sync should always tell us if we've reached eov. */
		/* Because that might not be true in older versions of Unicos */
		/* (before 8.0.x), keep this check around.  */
		if (eovret == 1 || tsdata.ts_bmblk > 0) {
	    	/* we reached EOV while doing the sync */
			/* Handle EOV */
			if (f->bmx_flag & BMXIO_MODELE)
				ioptr = f->bmx_iocurrent;
			else
				ioptr = f->bmx_iocurrent->bmx_prev;
			/* There should be no data in the library buffers. */
			/* but we will go through this code anyway, so */
			/* that special processing will look the same */
			if (_eov_buf(f,ioptr) < 0)
				return(-1);
		}
	}

	_tape_gtpos(&tsdata, vsn, lib, pa, palen);
	return(0);

}

