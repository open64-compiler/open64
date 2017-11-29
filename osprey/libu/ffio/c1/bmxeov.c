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


static char USMID[] = "@(#) libu/ffio/c1/bmxeov.c	92.0	10/08/98 14:57:41";


#include <sys/types.h>
#include <sys/stat.h>
#include <sys/iosw.h>
#include <sys/param.h>
#include <sys/bmxctl.h>
#include <fcntl.h>
#include <tapereq.h>
#include <errno.h>
#include <liberrno.h>
#include <malloc.h>
#include "bmxio.h"
#include "../sysio.h"

/*
 *	start eov processing - do this after EOT reached
 *	Similar to COS STARTSP macro
 *	Returns: 0 if successful
 *		 -1 if error (errno is set)
 */

_start_eov(int fd)
{

	struct 	dmn_comm	bmxeov;

	F_TRACE("strteov",0,0,0);

	bmxeov.EOV_REQ = TR_BEOV;
	bmxeov.EOV_REP = 0;
	bmxeov.EOV_STATUS = 0;

	if( ioctl(fd, BXC_DMN_REQ, &bmxeov) <0) {
		F_TRACE("tr_beov",errno,0,0);
		return(-1);
	}

	if( ioctl(fd, BXC_DMN_REP, &bmxeov) <0) {
		F_TRACE("eovrep",errno,0,0);
		return(-1);
	}
	if (bmxeov.EOV_REP != 0) {
		F_TRACE("eov_rep",bmxeov.EOV_REP,0,0);
		errno = bmxeov.EOV_REP;
		return(-1);
	}
	return(0);
}


/*
 *	enable eov processing - do this before i/o to hit EOT
 *	Similar to SETSP ON COS macro
 */

_enable_eov(int fd)
{

	struct 	dmn_comm	bmxeov;

	F_TRACE("ena_eov",0,0,0);
	bmxeov.EOV_REQ = TR_EOV;
	bmxeov.EOV_STATUS = EOV_ON;
	bmxeov.EOV_REP = 0;

	if( ioctl(fd, BXC_DMN_REQ, &bmxeov) <0) {
		F_TRACE("eovon",errno,0,0);
		return(-1);
	}

	if( ioctl(fd, BXC_DMN_REP, &bmxeov) < 0) {
		F_TRACE("on_rep",errno,0,0);
		return(-1);
	}
	if (bmxeov.EOV_REP != 0) {
		F_TRACE("treovrp",bmxeov.EOV_REP,0,0);
		errno = bmxeov.EOV_REP;
		return(-1);
	}
	return(0);

}

/*
 *	disable eov processing - do this after all eov processing is done
 *	Similar to SETSP OFF COS macro
 */

_disable_eov(int fd)
{

	struct 	dmn_comm	bmxeov;

	F_TRACE("dis_eov",0,0,0);
	bmxeov.EOV_REQ = TR_EOV;
	bmxeov.EOV_STATUS = EOV_OFF;
	bmxeov.EOV_REP = 0;

	if(ioctl(fd, BXC_DMN_REQ, &bmxeov) < 0) {
		F_TRACE("eovoff",errno,0,0);
		return(-1);
	}

	if(ioctl(fd, BXC_DMN_REP, &bmxeov) < 0) {
		F_TRACE("offictl",errno,0,0);
		return(-1);
	}
	if (bmxeov.EOV_REP != 0) {
		F_TRACE("offrep",bmxeov.EOV_REP,0,0);
		errno = bmxeov.EOV_REP;
		return(-1);
	}
	return(0);

}


/*
 *	end eov processing - do this after you're all done with eov
 *	Similar to ENDSP COS macro
 */

_end_eov(int fd)
{
	struct 	dmn_comm	bmxeov;

	F_TRACE("endeov",0,0,0);
	bmxeov.EOV_REQ = TR_EEOV;
	bmxeov.EOV_REP = 0;
	bmxeov.EOV_STATUS = 0;

	if( ioctl(fd, BXC_DMN_REQ, &bmxeov) < 0) {
		F_TRACE("treeov",errno,0,0);
		return(-1);
	}

	if( ioctl(fd, BXC_DMN_REP, &bmxeov) <0) {
		F_TRACE("endictl",errno,0,0);
		return(-1);
	}
	if (bmxeov.EOV_REP != 0) {
		F_TRACE("endrep",bmxeov.EOV_REP,0,0);
		errno = bmxeov.EOV_REP;
		return(-1);
	}	
	return(0);
}


/*
 * See if we got EOV status.
 * returns 0 if ok
 *	   -1 if error
 */
_bmx_wait(BMXFIL *f)
{
struct bmxio *ioptr;

	if (f->bmx_tpos) {
		if( _tape_tpwait( f->bmx_fd, &(f->bmx_tpos) ) != 0)
			return(-1);
	}
	if (f->bmx_flag & BMXIO_RW) {
		/* If we were writing */
		if ((f->bmx_flag & BMXIO_MODELE) != 0)
			ioptr = f->bmx_iocurrent;
		else {
			/* Model D */
			/* If we just did a write, wait for it to be complete. */
			/* This makes it like a single list write */
			ioptr = f->bmx_iocurrent->bmx_prev;
		}
		if(_eov_wait(f,ioptr) < 0)
			return(-1);		
	}	
	else {
		/* We want to check on the status of the readahead */
		if (f->bmx_iocurrent->bmx_busy == ACTIVE) {
			WAITBMXIO(f,f->bmx_iocurrent);
			if (BMXIO_ERROR(f->bmx_iocurrent->bmx_iosw)) {
				(void)_bmx_clrerr(f);
				if (f->bmx_iocurrent->bmx_iosw.sw_error == ENOSPC) {
					/* When we get EOV on a read, we */
					/* should not get any data. If we */
					/* do, we will not handle it correctly. */
					/* Make this an internal error for now */
					if (f->bmx_iocurrent->bmx_iosw.sw_count != 0) {
						errno = FEINTTAP;
						return(-1);
					}
					f->bmx_flag |= BMXIO_EOVSEEN;
					_bmx_clear(f);
				}
			}
		}
	}
	return(0);
}



/*
 * If the i/o described in the structure pointed to by ioptr is not
 * complete, wait for it to finish. If EOV is encountered, do the
 * following:
 *	On Model D IOS - flush the library's buffer to the IOS.
 *	On Model E IOS - If some of the data in the buffer described by
 *			 ioptr was not written, copy it to a buffer that
 *			 is part of the "fake" buffer memory list.
 *			 Change the linked list of bmxio pointers so that
 *			 other unwritten data is linked onto this "fake"
 *			 buffer memory list. 
 */
_eov_wait(BMXFIL *f, struct bmxio *ioptr)
{

	if (ioptr->bmx_busy == ACTIVE){
		WAITBMXIO(f,ioptr);
		ioptr->bmx_busy = INACTIVE;
		if (ioptr->bmx_iosw.sw_error == ENOSPC) {
			if(ioctl(f->bmx_fd,BXC_ACKERR,0)<0) {
				return(-1);
			}
			if (_eov_buf(f,ioptr) < 0)
				return(-1);
		}
		else {
			/* Clear the list */
			memset(ioptr->bmx_list,0,sizeof(struct bmxlist) * f->bmx_lstsz);
			if (BMXIO_ERROR(ioptr->bmx_iosw)) {
				(void)_bmx_quiet(f);
				(void)_bmx_clrerr(f);
				_bmx_clear(f);
				errno = ioptr->bmx_iosw.sw_error;
				f->bmx_flag |= BMXIO_ERRSEEN;
				return(-1);
			}
			/* EOV was not encountered. */
			f->bmx_lstptr = ioptr->bmx_list;
			f->bmx_bufptr = ioptr->bmx_base;
			f->bmx_bufcnt = f->bmx_bufsz;
			f->bmx_iocurrent = ioptr;
		}
		ioptr->bmx_iosw.sw_error = 0;	
		ioptr->bmx_iosw.sw_flag = 0;
		ioptr->bmx_iosw.sw_count = 0;
		f->bmx_lstcnt = f->bmx_lstsz;
	}
	return(0);
}

_eov_buf(BMXFIL *f, struct bmxio *ioptr)
{
int n, ret;
int count;
struct bmxlist *lstptr;
int lstcnt;
char *bufptr;
int cl;
struct bmxio *saveptr;
struct eovbuf *eovptr;
struct eovbuf *oldeov;
int newlstcnt;
struct bmxlist *newlstptr;

	f->bmx_flag |= BMXIO_EOVSEEN | BMXIO_WEOV;
	F_TRACE("EOVret",ioptr->bmx_iosw.sw_count,0,0);
	if ((f->bmx_flag & BMXIO_MODELE) != 0) {
		f->bmx_buflstcnt = 0;
		eovptr = (struct eovbuf *)malloc(sizeof(*eovptr));
		if (eovptr == NULL){
			errno = FENOMEMY;
			return(-1);
		}
		f->eov_first = eovptr;
		f->eov_current = eovptr;
		if( (eovptr->bmx_bufmem =
			malloc(f->bmx_bufsz)) == NULL) {
			free(f->eov_first);
			f->eov_first = NULL;
			f->eov_current = NULL;
			errno = FENOMEMY;
			return(-1);
		}
		if ((eovptr->bmx_buflst =
		  (struct bmxlist *)calloc(f->bmx_lstsz * 
		  sizeof(struct bmxlist),1)) == NULL) {
			errno = FENOMEMY;
			free(eovptr->bmx_bufmem);
			free(f->eov_first);
			f->eov_first = NULL;
			f->eov_current = NULL;
			return(-1);
		}
		f->bmx_cbuflst = eovptr->bmx_buflst;
		f->bmx_cbufmem = eovptr->bmx_bufmem;
		f->bmx_buflstsz = f->bmx_lstsz;
		f->bmx_buflstcnt = f->bmx_lstsz;
		if ((ioptr->bmx_iosw.sw_error == ENOSPC) &&
			(f->bmx_flag & BMXIO_RW)) {
			/* Go through list and buffer
			 * until we get to unwritten
			 * portion.
			 */
			lstcnt = f->bmx_lstsz;
			lstptr = ioptr->bmx_list;
			count = ioptr->bmx_iosw.sw_count;
			bufptr = ioptr->bmx_base;
			n = 0;
			while ((n < count) && (lstcnt)){
				n+=rtoc(lstptr->bytes);
				cl = rtoc(lstptr->bytes);
				bufptr+=cl;	/* move in sectors */
				lstptr++;
				lstcnt--;
			}	
	
			while (lstcnt) {
				if ((lstptr->state != BMS_EOF) ||
				  (lstptr->flags != BMF_WTM))
					break;
				lstptr++;
				lstcnt--;
			}
			/*
			 * Now move the list. Bufptr already 
			 * points to the data.
			 */ 
				
			memcpy(eovptr->bmx_bufmem, bufptr, f->bmx_bufsz-
				(bufptr-ioptr->bmx_base));
			memcpy(eovptr->bmx_buflst, lstptr,
				(f->bmx_lstsz -(f->bmx_lstsz-lstcnt))*
				sizeof(struct bmxlist));
		}

		/* Now go through the rest of the entries */
		saveptr = ioptr;
		ioptr = ioptr->bmx_nxt;
		if (ioptr == NULL){
			ioptr = f->bmx_iofirst;
		}
		while (ioptr != saveptr) {
			oldeov = eovptr;
			eovptr = (struct eovbuf *)malloc(sizeof(*eovptr));
			if (eovptr == NULL) {
				errno = FENOMEMY;
				return(-1);
			}
			oldeov->eov_nxt = eovptr;
			eovptr->bmx_bufmem= ioptr->bmx_base;
			eovptr->bmx_buflst= ioptr->bmx_list;
			ioptr=ioptr->bmx_nxt;
			if (ioptr == NULL)
				ioptr = f->bmx_iofirst;
		}
		eovptr->eov_nxt = NULL;
	}
	else {	/* Model D */
		if ((ioptr->bmx_iosw.sw_error == ENOSPC) && 
			(ioptr->bmx_iosw.sw_count != f->bmx_bufsz) &&
			(f->bmx_flag & BMXIO_RW)) {
			/* Go through list and buffer
			 * until we get to unwritten
			 * portion.
			 */
			lstcnt = f->bmx_lstsz;
			lstptr = ioptr->bmx_list;
			count = ioptr->bmx_iosw.sw_count;
			bufptr = ioptr->bmx_base;
			n = 0;
			if (ioptr->bmx_iosw.sw_count != 0) {
				while ((n < count) && (lstcnt)) {
					n+=rtoc(lstptr->bytes);
					cl = rtoc(lstptr->bytes);
					bufptr+=cl;	/* move in sectors */
					lstptr++;
					lstcnt--;
				}	
				/*
				 * Now move the list. Bufptr 
				 * already points to the data.
				 */ 
				newlstcnt = f->bmx_lstsz;
				newlstptr = ioptr->bmx_list;
				while (lstcnt) {
					newlstptr->bytes = lstptr->bytes;
					newlstptr->state = lstptr->state;
					newlstptr->flags = lstptr->flags;
					newlstptr++;
					newlstcnt--;
					lstptr++;
					lstcnt--;
				}
				/* Zero remainder of list */
				while (newlstcnt) {
					newlstptr->bytes = 0;
					newlstptr->state = 0;
					newlstptr->flags = 0;
					newlstcnt--;
					newlstptr++;
				}
				/* Now move the data. */
				memcpy(ioptr->bmx_base,bufptr,
					f->bmx_bufsz-(bufptr-
					ioptr->bmx_base));
			}
			f->bmx_flag &= ~BMXIO_ERRSEEN;
			ret =_loop_write(f, ioptr, f->bmx_bufsz);
			if (ret < 0){
				f->bmx_flag |= BMXIO_ERRSEEN;
				return(-1);
			}
		}
		saveptr = ioptr;
	}
	/* 
	 * "Unlink" all of the lists except the current one.
	 * For the Model E, these lists are in a separate list
	 * pointed to by f->eov_first during special processing.
	 * For the Model D, these lists do not contain any
	 * data, but we want to use only 1 list during special
	 * processing to simplify things.
	 */
	f->bmx_iosavefirst = f->bmx_iofirst;
	f->bmx_iocurrent = f->bmx_iofirst = saveptr;
	f->bmx_lstcont = f->bmx_iofirst->bmx_nxt;
	f->bmx_iofirst->bmx_nxt = NULL;
	f->bmx_iofirst->bmx_prev = f->bmx_iofirst;
	_bmx_clear(f);
	return(0);
}

/*
 *	This routine exists to provide hard references to eov
 *	routines. It is expected that it will always be called
 *	with i == 1
 */
void
_eov_load(int i)
{
	if (i != 1) {
		_bmx_checktp(NULL, NULL, NULL);
		_bmx_endsp(NULL);
		_bmx_startsp(NULL);
		_bmx_setsp(NULL, 0 );
	}
}
