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


#pragma ident "@(#) libf/pxf/pxftcsendbreak.c	92.2	06/29/99 11:36:06"

/*
 *  Line Control Functions:
 *
 *  PXFTCSENDBREAK  -- Transmit stream of zeros for a specific duration.
 *  PXFTCDRAIN  -- Wait until all output written to specified file has
 *                 been transmitted.
 *  PXFTCFLUSH  -- Discard data written but not sent, or received by not
 *                 read depending on the queue_selector.
 *  PXFTCFLOW  -- Suspend transmission or reception of data on the file
 *                specified depending on the action specified.
 *             (section 7.2.2 of Posix 1003.9-1992)
 *
 *  Synopsis:
 *
 *     SUBROUTINE PXFTCSENDBREAK(ifildes, iduration, ierror)
 *     INTEGER ifildes, iduration, ierror
 *
 *     SUBROUTINE PXFTCDRAIN(ifildes, ierror)
 *     INTEGER ifildes, ierror
 *
 *     SUBROUTINE PXFTCFLUSH(ifildes, iqueue, ierror)
 *     INTEGER ifildes, iqueue, ierror
 *
 *     SUBROUTINE PXFTCFLOW(ifildes, iaction, ierror)
 *     INTEGER ifildes, iaction, ierror
 *
 *  Description:
 *
 *  PXFTCSENDBREAK, PXFTCDRAIN, PXFTCFLUSH, and PXFTCFLOW use c
 *  functions tcsendbreak(), tcdrain(), tcflush(), and tcflow() to
 *  provide terminal control.  The constant values for iqueue and
 *  iaction are available through the use of PXFCONST.
 *
 *  The arguments are:
 *
 *      ifildes   - default integer input variable containing a file
 *                  descriptor.
 *      iduration - default input integer variable specifying duration.
 *                  If duration = 0, zero-valued bits are transmitted
 *                  every 0.25 seconds and not more than 0.5 seconds.
 *                  Otherwise zero-valued bits are transmitted for an
 *                  implementation-defined period of time.
 *      iqueue    - default input integer variable containing the
 *                  queue selector.
 *      iaction   - default input integer variable containing the
 *                  action.
 *      ierror    - default integer output variable that contains zero
 *                  if the operation was successful or nonzero if the
 *                  operation was not successful.
 *
 *   PXFTCSENDBREAK, PXFTCDRAIN, PXFTCFLUSH, and PXFTCFLOW may
 *   return one of the following error values:
 *
 *   EBADF      If ifildes is not a valid file descriptor.
 *
 *   EINTR      If tcdrain was interrupted by a signal.
 *
 *   EINVAL     If iqueue or iaction is not a proper value.
 *
 *   ENOTTY     If the file associated with ifildes is not a terminal.
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
PXFTCSENDBREAK(
#else	/* _UNICOS */
void
pxftcsendbreak_(
#endif	/* _UNICOS */
	_f_int	*ifildes,
	_f_int	*iduration,
	_f_int	*ierror)
{
	int	fildes;
	int	durat;
	int	stat;
	durat	= *iduration;
	fildes	= *ifildes;
	*ierror	= 0;
	if (stat = tcsendbreak(fildes,durat) == -1)
		*ierror	= errno;
	return;
}

#ifdef _UNICOS
void
PXFTCDRAIN(
#else	/* _UNICOS */
void
pxftcdrain_(
#endif	/* _UNICOS */
	_f_int	*ifildes,
	_f_int	*ierror)
{
	int	fildes;
	int	stat;
	fildes	= *ifildes;
	*ierror	= 0;
	if (stat = tcdrain(fildes) == -1)
		*ierror	= errno;
	return;
}

#ifdef _UNICOS
void
PXFTCFLUSH(
#else	/* _UNICOS */
void
pxftcflush_(
#endif	/* _UNICOS */
	_f_int	*ifildes,
	_f_int	*iqueue,
	_f_int	*ierror)
{
	int	fildes;
	int	queue;
	int	stat;
	queue	= *iqueue;
	fildes	= *ifildes;
	*ierror	= 0;
	if (stat = tcflush(fildes,queue) == -1)
		*ierror	= errno;
	return;
}

#ifdef _UNICOS
void
PXFTCFLOW(
#else	/* _UNICOS */
void
pxftcflow_(
#endif	/* _UNICOS */
	_f_int	*ifildes,
	_f_int	*iaction,
	_f_int	*ierror)
{
	int	fildes;
	int	action;
	int	stat;
	action	= *iaction;
	fildes	= *ifildes;
	*ierror	= 0;
	if (stat = tcflow(fildes,action) == -1)
		*ierror	= errno;
	return;
}
