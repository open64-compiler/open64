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


static char USMID[] = "@(#) libu/ffio/c1/taperewd.c	92.0	10/08/98 14:57:41";

#include  <ffio.h>
#include  <sys/types.h>
#include  <sys/param.h>
#include  <sys/bmxctl.h>
#include  <sys/file.h>
#include  <sys/iosw.h>
#include  <sys/stat.h>
#include  <sys/jtab.h>
#include  <fcntl.h>
#include  <stdio.h>
#include  <tapereq.h>
#include  <errno.h>
#include  <liberrno.h>
#include  "tapeio.h"
 
/*
 * Issue a rewind request for a bmx or er90 device.
 */

extern int G@INTIO;

_tape_rewd(int fd)
{
	struct bmxpos	ctl;
	int ret;

	ctl.pos_fcn = TR_RWD;

	do {
		ret = ioctl(fd, BXC_SPOS, &ctl);
	} while ((ret < 0) && (G@INTIO == 0) && (errno == EINTR));

	return(ret);

}


/*
 * Wait for the positioning request to finish.
 */
_tape_tpwait(int fd, int *tpos)
{
	struct bmxpos	ctl;

waitresponse:
	if ( ioctl( fd, BXC_GPOS, &ctl) < 0 )
		return(-1);

	if ((ctl.pos_rc == EINTR) && (*tpos == TPOS_REWIND) &&
	 (G@INTIO == 0)) {
		if (_tape_rewd(fd) == -1)
			return(-1);
		goto waitresponse;		
	}
	if (ctl.pos_rc != 0){
		errno = ctl.pos_rc;
		return(-1);
	}
	*tpos = TPOS_DONE;
	return(0);

}
