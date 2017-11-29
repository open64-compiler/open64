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


#pragma ident "@(#) libf/pxf/pxfpipe.c	92.1	06/29/99 11:36:06"
/*
 *  PXFPIPE -- Create an Inter-Process Channel (pipe) and return
 *             fds for the read and write ends of the pipe.
 *             (section 6.1 of Posix 1003.9-1992)
 *
 *  Synopsis:
 *
 *     SUBROUTINE PXFPIPE(ireadfd, iwrtfd, ierror)
 *     INTEGER ireadfd, iwrtfd, ierror
 *
 *  Where:
 *
 *      ireadfd -  default integer output variable containing the
 *                 file descriptor for the read end of the pipe.
 *      iwrtfd  -  default integer output variable containing the
 *                 file descriptor for the write end of the pipe.
 *      ierror  -  default integer output variable that contains zero
 *                 if the operation was successful or nonzero if the
 *                 operation was not successful.
 *
 *   PXFPIPE may return one of the following error values:
 *
 *   EMFILE     If too many file descriptors are currently open for the
 *              process.
 *
 *   ENFILE     If too many file descriptors are currently open for the
 *              system.
 *
 * On PVP systems, the following may also be returned:
 *
 *   ENOSPC     If no free space left on device.
 *
 *   EFLNEQ     If the active security label of the process does not fall
 *              within the security label range of the root file system.
 *
 */

#include <fortran.h>
#include <errno.h>
#include <liberrno.h>
#include <unistd.h>

#ifdef _UNICOS
void
PXFPIPE(
#else	/* _UNICOS */
void
pxfpipe_(
#endif	/* _UNICOS */
	   _f_int *ireadfd,
	   _f_int *iwrtfd,
	   _f_int *ierror)
{
	int	istat;
	int	fds[2];
	if (istat = pipe(fds) == -1) {
		*ierror		= errno;
	} else {
		*ireadfd	= fds[0];
		*iwrtfd		= fds[1];
		*ierror		= 0;
	}
	return;
}
