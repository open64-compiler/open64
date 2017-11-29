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


static char USMID[] = "@(#) libu/ffio/c1/bmxclose.c	92.0	10/08/98 14:57:41";


#include <sys/param.h>
#include <sys/types.h>
#include <sys/iosw.h>
#include <sys/bmxctl.h>
#include <sys/file.h>
#include <tapereq.h>
#include <errno.h>
#include <ffio.h>
#include "bmxio.h"

/*
 *	close on-line tape file
 *
 */
 

_bmx_close(fio, stat) 
struct fdinfo *fio;
struct ffsw *stat;
{
 
	register int n = 0;
	BMXFIL *f;

	f = (BMXFIL *)fio->lyr_info;

	if ( f->bmx_tpos ) {	/* Wait for positioning to finish */
		if( _tape_tpwait( f->bmx_fd, &(f->bmx_tpos) ) != 0)
			ERETURN(stat, errno, 0);
	} 	
	
/*
 *	Make sure i/o is quiet before closing.
 */

	if ( _bmx_quiet(f) < 0) {
		/* Ignore read-ahead errors */
		if (f->bmx_flag & BMXIO_RW)
			n = -1;
	}
/*
 *      If previous i/o request was a write request make sure the list
 *      is flushed before issuing the close.  
 */ 
	n |= __bmxflush(f);

	if (f->bmxio_alloc != NULL) {
		free(f->bmxio_alloc->bmx_base);
		free(f->bmxio_alloc->bmx_list);
		free(f->bmxio_alloc);
	}
	/* Have we made a reply pipe? If so, remove it */
	if (f->tsireq.reply_fname != NULL){
		n |= unlink(f->tsireq.reply_fname);
		free(f->tsireq.reply_fname);
	}
	if (f->tsireq.request_fname != NULL){
		free(f->tsireq.request_fname);
	} 
	n |= close( f->bmx_fd );
	if ( n < 0 )
		ERETURN(stat, errno, 0);
	free((char *)f);

	return(0);

}
