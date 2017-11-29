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


#pragma ident "@(#) libf/pxf/pxftcsetpgrp.c	92.2	06/29/99 11:36:06"

/*
 *  PXFTCSETPGRP  -- Set Foreground Process Group Id.
 *             (section 7.2.4 of Posix 1003.9-1992)
 *
 *  Synopsis:
 *
 *     SUBROUTINE PXFTCSETPGRP(ifildes, ipgid, ierror)
 *     INTEGER ifildes, ipgid, ierror
 *
 *  Description:
 *
 *  PXFTCSETPGRP uses the pxftcsetpgrp system call to return the
 *  value of the foreground process group.
 *
 *  The arguments are:
 *
 *      ifildes  - default integer input variable containing a file
 *                 descriptor.
 *      ipgid    - default input integer variable specifying a
 *                 foreground process group ID  Set the foreground
 *                 process group ID of the terminal specified by
 *                 ifildes to ipgid.
 *      ierror   - default integer output variable that contains zero
 *                 if the operation was successful or nonzero if the
 *                 operation was not successful.
 *
 *   PXFTCSETPGRP may return one of the following error values:
 *
 *   EBADF      - ifildes is not a valid file descriptor.
 *
 *   EINVAL     - ipgid is not a valid process group.
 *
 *   ENOTTY     - the file associated with ifildes is not a terminal.
 *
 *   EPERM      - ipgid does not match the process group of an existing
 *                process in the same session as the calling process.
 *              
 */

#include <errno.h>
#include <fortran.h>
#include <liberrno.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>
#include <sys/errno.h>
#include <sys/types.h>
#include <unistd.h>

#ifdef _UNICOS
void
PXFTCSETPGRP(
#else	/* _UNICOS */
void
pxftcsetpgrp_(
#endif	/* _UNICOS */
	_f_int	*ifildes,
	_f_int	*ipgid,
	_f_int	*ierror)
{
	int	fildes;
	int	pgid;
	int	stat;
	fildes	= *ifildes;
	*ierror	= 0;
	pgid	= *ipgid;
	if (stat = tcsetpgrp(fildes,pgid) == -1)
		*ierror	= errno;
	return;
}
