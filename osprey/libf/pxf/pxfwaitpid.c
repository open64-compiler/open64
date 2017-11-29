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


#pragma ident "@(#) libf/pxf/pxfwaitpid.c	92.1	06/29/99 11:36:06"

#include <errno.h>
#include <liberrno.h>
#include <fortran.h>
#include <sys/types.h>
#include <sys/wait.h>

/*  PXFWAITPID  -- wait for process termination
 *  (section 3.2.1 of Posix 1003.9-1992)
 *
 *  Synopis:
 *     SUBROUTINE PXFWAITPID(IPID,ISTAT,IOPTIONS,IRETPID,IERROR)
 *     INTEGER IPID,ISTAT,IOPTIONS,IRETPID,IERROR
 *
 *  Function description:
 *  The routine PXFWAITPID uses the waitpid(2) system call to obtain information
 *  about one of the calling process's child processes.
 *
 *
 *
 *  Description of arguments:
 *
 *  IPID     is an input integer variable containing the child process ID for
 *           which information is requested.
 *
 *  ISTAT    is an output integer variable for the status information.
 *
 *  IOPTIONS is an input integer variable constructed from an inclusive OR of
 *           zero or more of the following:
 *
 *           WNOHANG             The waitpid system call does not suspend
 *                               execution of the calling process if status is
 *                               not immediately available for one of the calling
 *                               processes specified by pid.
 *
 *           WUNTRACED           If job control is supported, the status of any
 *                               child processes specified by pid that are
 *                               stopped, and with a status that has not yet been
 *                               reported since they stopped, are also reported
 *                               to the requesting process.
 *
 *           UNICOS only:
 *
 *           WMTWAIT             Waits for the children of any member of the
 *                               multitasking group.  In UNICOS 9.0 this is the
 *                               default behavior for both wait and waitpid.  The
 *                               flag is still provided for source compatibility.
 *                               To get the previous behavior, see the
 *                               description of the WLWPWAIT flag.
 *
 *           WLWPWAIT            Waits only for the immediate children of the
 *                               calling light-weight process (LWP).  This flag
 *                               is not recommended for general use.
 *
 *  IRETPID  is an output integer variable for the child process ID.
 *
 *  IERROR   is an output integer variable that will contain
 *           the status:
 *
 *            zero    - PXFWAITPID was successful.
 *
 *            nonzero - PXFWAITPID was not successful.
 *
 *           PXFWAITPID may return any of the following error
 *           values:
 *
 *           ECHILD  The process or process group specified by pid does
 *                   not exist or is not a child of the calling
 *                   process.
 *
 *           EINTR   The call was interrupted by a signal.
 *
 *           EINVAL  The value of the options argument is not valid.
 *
 */

#ifdef _UNICOS
void
PXFWAITPID(
#else
void
_PXFWAITPID(
#endif
	 _f_int *IPID,
	 _f_int *ISTAT,
	 _f_int *IOPTIONS,
	 _f_int *IRETPID,
	 _f_int *IERROR
)
{
  pid_t child_pid;
  int stat;

  if ((child_pid = waitpid(*IPID,&stat,*IOPTIONS)) == -1) {
    *IERROR = errno;
  } else {
    *ISTAT = stat;
    *IRETPID = child_pid;
    *IERROR = 0;
  }
}

#ifndef _UNICOS
void
pxfwaitpid_(
	 _f_int *IPID,
	 _f_int *ISTAT,
	 _f_int *IOPTIONS,
	 _f_int *IRETPID,
	 _f_int *IERROR
)
{
  _PXFWAITPID(IPID,ISTAT,IOPTIONS,IRETPID,IERROR);
}
#endif
