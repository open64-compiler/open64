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


#pragma ident "@(#) libf/pxf/pxffork.c	92.1	06/29/99 11:36:06"


#include <errno.h>
#include <liberrno.h>
#include <fortran.h>
#include <sys/types.h>
#include <unistd.h>

/*  PXFFORK  -- create process
 *  (section 3.1.1 of Posix 1003.9-1992)
 *
 *  Synopsis:
 *     SUBROUTINE PXFFORK(IPID,IERROR)
 *     INTEGER IPID, IERROR
 *
 *  Function description:
 *  The routine PXFFORK uses the fork(2) system call to create a new process.
 *  The child process is the same as the parent process except for the
 *  following:
 *
 *     The child process has a unique, currently unused process ID.
 *
 *     The child process has a different parent process ID. The child
 *     process's process ID is the parent process, or calling process, ID.
 *
 *     The child process has its own copy of the parent's file descriptors.
 *     Each of the child's file descriptors shares a common file pointer
 *     with the corresponding file descriptor of the parent process.
 *
 *     Process locks are not inherited by the child process (see plock(2))
 *
 *     The utime, stime, cutime, and cstime of the child process are set to 0.
 *     The time left until an alarm clock signal is reset to 0.
 *
 *     All semadj values are cleared (see semop(2)).
 *
 *     The parent's set of pending signals are not inherited by the child.
 *
 *     UNICOS only:
 *
 *     Record locks set by the parent process are not inherited by the
 *     child process (see fcntl(2) and lockf(3C)).
 *
 *     In a multitasking group, only the process that executed the fork
 *     system call is copied.
 *
 *     Each attached shared memory segment is attached and the value of
 *     shm_nattch in the data structure associated with the shared memory
 *     segment is incremented by 1.
 *
 *     IRIX only:
 *
 *     File locks previously set by the parent are not inherited by the
 *     child (see fcntl(2)).
 *
 *     Page locks are not inherited (see mpin(2)).
 *
 *     The time left until an itimer signal is reset to 0.
 *
 *     The child will not inherit the ability to make graphics calls.  The
 *     child process may receive a segmentation fault upon attempting to
 *     make a graphics call, unless it initializes itself as a graphics
 *     process via winopen() or ginit().  NOTE: Currently, if the parent is
 *     a graphics process, the child's attempt to become a graphics process
 *     will fail.
 *
 *     The share mask is set to 0 (see sproc(2)).
 *
 *  Description of arguments:
 *
 *  IPID     is an output integer variable. IPID will be zero for the
 *           child process and the process ID of the child for the parent
 *           process.
 *
 *  IERROR   is an output integer variable that will contain
 *           the status:
 *
 *            zero    - PXFFORK was unsuccessful.
 *
 *            nonzero - PXFFORK was not successful.
 *
 *           PXFFORK may return any of the following error
 *           values:
 *
 *           EAGAIN   If the system-imposed limit on the total number of
 *                    processes under execution in the whole system
 *                    (NPROC) is exceeded (IRIX see intro(2)).
 *
 *           EAGAIN   If the system-imposed limit on the total number of
 *                    processes under execution by one user (CHILD_MAX)
 *                    is exceeded.
 *
 *           Unicos only:
 *
 *           EBUSY    If an attempted to enable accounting when it is
 *                    already enabled, or if you issue a restart(2)
 *                    attempt when another job or process in the system
 *                    is using the jid or any pid associated with the job
 *                    (or process) to be restarted.
 *
 *           EINTR    If an asynchronous signal (such as interrupt or quit),
 *                    which you have elected to catch, occurred during a
 *                    fork system call.  When execution resumed after
 *                    processing the signal, the interrupted system call
 *                    returned this error condition.
 *
 *
 *           EMEMLIM  If more memory space was requested than is allowed
 *                    for the processes attached to this lnode. The
 *                    maximum value is set by the -c option of the
 *                    shradmin(8) command.  This error appears only on
 *                    systems running the fair-share scheduler.
 *
 *           ENOEXEC  If a request was made to execute a file that, although
 *                    it has the appropriate permissions, does not start with
 *                    a valid magic number (see a.out(5)).
 *
 *           ENOMEM   If during an exec(2) or sbreak(2) system call, a
 *                    program requested more space than the system could
 *                    supply.  This is not a temporary condition; the
 *                    maximum space specification is a system parameter.
 *
 *           EPROCLIM If more processes were requested than is allowed for
 *                    this lnode.  The maximum value is set by the -p
 *                    option of the shradmin(8) command. This error
 *                    appears only on systems running the fair-share scheduler.
 *
 *           IRIX only:
 *
 *           EAGAIN   If the amount of system memory required is temorarily
 *                    unavailable.
 *
 *           ENOSPC   If the caller is a member of a share group and the
 *                    total number of share group members plus children
 *                    exceeds the maximum number of users specified via
 *                    usconfig (3P) (8 by default).  Any changes via
 *                    usconfig (3P) must be done BEFORE the first sproc
 *                    is formed.
 *
 *           ENOLCK   There are not enough file locks in the system.
 *
 */

#ifdef _UNICOS
void
PXFFORK(
#else
void
_PXFFORK(
#endif
	 _f_int *IPID,
	 _f_int *IERROR
)
{
  pid_t cpid;

  if ((cpid = fork()) == -1) {
    *IERROR = errno;
  } else {
    *IERROR = 0;
    *IPID = cpid;
  }
}

#ifndef _UNICOS
void
pxffork_(
	 _f_int *IPID,
	 _f_int *IERROR
)
{
  _PXFFORK(IPID,IERROR);
}
#endif
