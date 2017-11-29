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


static char USMID[] = "@(#) libu/ffio/c1/tapesync.c	92.0	10/08/98 14:57:41";


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


/*
 *	_tape_sync - flush buffers out to tape
 *      Does not return until they are written.
 *	Returns: 0 if OK
 *           ENOSPC if EOV reached on sync
 *		     -1 if error (errno is set)
 */

_tape_sync(int fd)
{
	struct 	dmn_comm	bmxeov;

#if TRACE
	f_trace("flshbuf",0,0,0);
#endif
	bmxeov.EOV_REQ = TR_SYNC;
	bmxeov.EOV_REP = 0;
	bmxeov.EOV_STATUS = 0;

	if( ioctl(fd, BXC_DMN_REQ, &bmxeov) <0) {
#if TRACE
		f_trace("trsync",errno,0,0);
#endif
		return(-1);
	}

	if( ioctl(fd, BXC_DMN_REP, &bmxeov) <0) {
#if TRACE
		f_trace("flsictl",errno,0,0);
#endif
		return(-1);
	}
	if (bmxeov.EOV_REP != 0){
#if TRACE
		f_trace("flsrep",bmxeov.EOV_REP,0,0);
#endif
		if (bmxeov.EOV_REP == ENOSPC) {
			if (ioctl(fd,BXC_ACKERR,0) < 0) {
				return(-1);
			}
			return(ENOSPC);
		}
		else {
			errno = bmxeov.EOV_REP;
			return(-1);
		}
	}
	return(0);
}
