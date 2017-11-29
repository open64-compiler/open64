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


#pragma ident "@(#) libf/pxf/pxfclose.c	92.1	06/29/99 11:36:06"
/*
 *  PXFCLOSE -- Close a File
 *             (section 6.3.1 of Posix 1003.9-1992)
 *
 *  Synopsis:
 *
 *     SUBROUTINE PXFCLOSE(ifildes, ierror)
 *     INTEGER ifildes, ierror
 *
 *  Description:
 *
 *  PXFCLOSE uses the close system call to close a file descriptor.
 *
 *  The arguments are:
 *
 *      ifildes  -  default integer input variable containing a file
 *                  descriptor.
 *      ierror   -  default integer output variable that contains zero
 *                  if the operation was successful or nonzero if the
 *                  operation was not successful.
 *
 *   PXFCLOSE may return one of the following error values:
 *
 *   EBADF      If ifildes is not a valid open file descriptor.
 *
 *   EINTR      If the close system call was interrupted by a signal.
 *
 *   On IRIX systems, PXFCLOSE may also return:
 *
 *   ETIMEDOUT  If the object of the close system call is located on
 *              a remote system which is not available.
 *
 */

#include <fortran.h>
#include <errno.h>
#include <liberrno.h>
#include <unistd.h>

#ifdef _UNICOS
void
PXFCLOSE(
#else	/* _UNICOS */
void
pxfclose_(
#endif	/* _UNICOS */
	   _f_int *ifildes,
	   _f_int *ierror)
{
	int	istat;
	*ierror	= 0;
	istat	= close(*ifildes);
	if (istat < 0)
		*ierror	= errno;
	return;
}
