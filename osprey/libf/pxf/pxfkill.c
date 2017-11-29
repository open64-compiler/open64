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


#pragma ident "@(#) libf/pxf/pxfkill.c	92.1	06/29/99 11:36:06"


/*
 *  PXFKILL -- Send a signal to a process or group of processes
 *  (section 3.3.2 of Posix 1003.9-1992)
 *
 *  Synopsis:
 *
 *     SUBROUTINE PXFKILL(ipid, isig, ierror)
 *     INTEGER ipid, isig, ierror
 *
 *  Where:
 *
 *	ipid   - default integer input variable containing the
 *	         process pid.  The signal will be sent to a 
 *	         process or group of processes specified by ipid.
 *	isig   - default integer input variable containing the
 *	         signal to be sent.
 *	ierror - default integer output variable containing
 *	         status:
 *
 *               zero		- PXFKILL was successful.
 *
 *               nonzero	- PXFKILL was not successful.
 *
 *  PXFKILL may return any of the following error values:
 *
 *	EINVAL  The value of the isig argument is an invalid
 *              or unsupported signal number.
 *
 *	EPERM   The process does not have permission to send
 *              the isig signal to any receiving process.
 *
 *	ESRCH   No process or process group can be found
 *              corresponding to the process id specified by
 *              ipid.
 *
 *  On UNICOS and UNICOS/mk systems, kill() may also return:
 *
 *	EPERM   The value of the ipid argument is 1 (proc1)
 *              and sig is either SIGKILL or SIGSTOP.
 *
 *  On IRIX systems, kill() may also return:
 *
 *	EPERM   The value of the ipid argument is 1 (proc1)
 *              and sig is SIGKILL.
 *
 *	ESRCH   The process group was given as 0 but the 
 *              sending process does not have a process
 *              group.
 *
 *  DESCRIPTION:
 *
 *  The subroutine procedure PXFKILL uses kill() system call
 *  to send a signal to a process or group of processes.
 *
 *  NOTE:
 *
 *  Replace the use of subprocedure KILL() with the PXFKILL()
 *  interface in Fortran programs for portability.
 *
 *  On UNICOS and UNICOS/mk systems, kill() may be a subroutine
 *  or a function reference.  On IRIX systems, kill() is a
 *  function reference.
 */

#include <errno.h>
#include <fortran.h>
#include <sys/types.h>
#include <sys/signal.h>

#ifdef _UNICOS
void
PXFKILL(
#else	/* _UNICOS */
void
_PXFKILL(
#endif	/* _UNICOS */
	_f_int *ipid,
	_f_int *isig,
	_f_int *ierror
)
{
	int stat;
	*ierror = 0;
	if ((stat = (_f_int) kill(*ipid, *isig)) == -1) {
		*ierror = errno;
	}
	return;
}

#ifndef _UNICOS
void
pxfkill_(
	_f_int *ipid,
	_f_int *isig,
	_f_int *ierror
)
{
	_PXFKILL(ipid, isig, ierror);
}
#endif	/* not _UNICOS */
