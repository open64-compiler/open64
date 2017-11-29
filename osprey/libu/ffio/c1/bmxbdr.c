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


static char USMID[] = "@(#) libu/ffio/c1/bmxbdr.c	92.0	10/08/98 14:57:41";


#include  <stdarg.h>
#include  <sys/types.h>
#include  <sys/bmxctl.h>
#include  <sys/param.h>
#include  <sys/file.h>
#include  <sys/iosw.h>
#include  <errno.h>
#include  "bmxio.h"

static int _initiateio(BMXFIL *f);

/*
 *	bad data recovery for on-line tape files
 *      Returns:
 *	  if skipping bad data, returns the number of tape blocks skipped
 *	  if accepting bad data, returns the number of bytes read
 */
 

_bmx_bdr(BMXFIL *f, void *uda, long *termcnd, long state, ...)
{
 
	register struct bmxlist *lstptr;
        register int    lstcnt, count = 0;
	struct bmxio *currentio;
	struct iosw *stat;
	long acptcount;
	va_list args;

/*
 *      Move frequently used values into registers.
 */
        lstptr = f->bmx_lstptr;          /* pointer to current list	*/
        lstcnt = f->bmx_lstcnt;          /* current offset in list	*/

	if ( state == BMX_SKIPBAD ) {
		/* Skip all bad data blocks that have been read */
		while( lstptr->flags != 0 ) {
			if ( lstptr->state == BMS_EOR )
				count++;
			f->bmx_bufptr += rtoc(lstptr->bytes);
			f->bmx_bufcnt -= rtoc(lstptr->bytes);
			lstptr++;
			lstcnt--;
			if ((lstcnt == 0) || (lstptr->state == 0)){
				/* If this list is empty, issue another */
				/* reada to fill it. */
				if(_initiateio(f) < 0){
					*termcnd = -errno;
					return(count);
				}
				currentio = f->bmx_iocurrent;
				lstptr = f->bmx_iocurrent->bmx_list;
				lstcnt = f->bmx_lstsz;
				/* Wait for the oldest reada to finish */
				stat = &currentio->bmx_iosw;
				while (currentio->bmx_iosw.sw_flag == 0){
					recall(f->bmx_fd,1,&stat);
				}
				if (BMXIO_ERROR(currentio->bmx_iosw)) {
					*termcnd = -errno;
					(void) _bmx_quiet(f);
					(void) _bmx_clrerr(f);
					(void) _bmx_clear(f);;
					return(count);
				}
				currentio->bmx_busy = DATA;
			}
			if ( (f->bmx_flag & BMXIO_SKIP) && count &&
			   !(f->bmx_flag & BMXIO_SKIPALL))
				break;
		}
		*termcnd = 0;
		if ((lstptr->state == BMS_EOD) || (lstptr->state == BMS_EOF))
			*termcnd = 1;
	}
	else {	/* Acptbad */
		if ( lstptr->flags == 0 ) {
			/* No  bad data */
			*termcnd = 0;
			return(0);
		}

		/* Copy the bad data to the user's buffer. */
		/* Copy the minimum of data read and acptcount, if specified */
		count = lstptr->bytes;
		if (_numargs() > 4){
			va_start(args, state);
			acptcount = va_arg(args, long);
			va_end(args);
			if (acptcount < (count>>3))
				count = acptcount<<3;
		}
		(void) memcpy( uda, f->bmx_bufptr, count );

		if ( lstptr->state == BMS_EOR )
			*termcnd = 0;
		else
			*termcnd = -1;

		f->bmx_bufptr += rtoc(lstptr->bytes);
		f->bmx_bufcnt -= rtoc(lstptr->bytes);
		lstptr++;
		lstcnt--;
		if ((lstcnt == 0) || (lstptr->state == 0)){
			if(_initiateio(f) < 0){
				*termcnd = -errno;
				return(count);
			}
			lstptr = f->bmx_iocurrent->bmx_list;
			lstcnt = f->bmx_lstsz;
		}
	}

/*
 *      	Update pointers in the bmx file table.
 */
       	f->bmx_lstptr = lstptr;
       	f->bmx_lstcnt = lstcnt;
        f->bmx_recl   = 0;
        f->bmx_flag  |= (BMXIO_EOR | BMXIO_NXT);
	f->bmx_flag  &= ~(BMXIO_DATA);
        return(count);

}

/*
 * Issue a reada to fill the buffer and list described by f->bmx_iocurrent
 * Returns 0  if OK
 *	   -1 if error
 */
static int
_initiateio(BMXFIL *f)
{
	struct bmxio *currentio;
	struct bmxio *oldio;
	int n;

	currentio = f->bmx_iocurrent;
	memset(currentio->bmx_list,0,sizeof(struct bmxlist)*f->bmx_lstsz);

	currentio->bmx_iosw.sw_flag = 0;
	f->bmx_listptr = currentio->bmx_list;
	oldio = currentio;
	currentio = currentio->bmx_nxt;
	if (currentio == NULL)
		currentio = f->bmx_iofirst;
	f->bmx_iocurrent = currentio;

	oldio->bmx_busy = INACTIVE;
	n = reada(f->bmx_fd, oldio->bmx_base, f->bmx_bufsz,&oldio->bmx_iosw,0 );
	if ( n < 0 ) {
		(void) _bmx_clrerr(f);
		return(-1);
	}
	oldio->bmx_busy = ACTIVE;

       	f->bmx_bufptr = currentio->bmx_base;
	f->bmx_bufcnt = f->bmx_bufsz;
	return(0);
}
