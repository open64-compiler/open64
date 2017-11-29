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


#pragma ident "@(#) libf/pxf/pxfsleep.c	92.1	06/29/99 11:36:06"


/*
 *  PXFSLEEP -- Delay Process Execution.
 *  (section 3.4.3 of Posix 1003.9-1992)
 *
 *  Synopsis:
 *
 *     SUBROUTINE PXFSLEEP(iseconds, isecleft, ierror)
 *     INTEGER iseconds, isecleft, ierror
 *
 *  Where:
 *
 *	iseconds -   default integer input variable containing
 *	             the number of real-time seconds that the
 *	             current process shall be suspended from
 *	             execution.
 *	isecleft -   default integer output variable containing
 *	             the unslept amount in seconds (requested
 *	             amount less amount actually slept).
 *	ierror   -   default integer output variable containing
 *	             status.  The function sleep is always
 *	             successful.
 *
 *           zero	- PXFSLEEP was successful.
 *
 *           nonzero	- PXFSLEEP was interrupted.
 *
 *  On UNICOS and UNICOS/mk systems, EINTR may interrupt the
 *  system call but this does not need to be signaled.  Only
 *  the time left needs to be noted.
 *
 *      EINTR -   System call interrupted.
 *
 *  DESCRIPTION:
 *
 *  The subroutine procedure PXFSLEEP uses the sleep() function
 *  to delay process execution.
 *
 *  NOTE:
 *
 *  Replace the use of subprocedure SLEEP() with the PXFSLEEP()
 *  interface in Fortran programs for portability.
 *
 *  On UNICOS and UNICOS/mk systems, sleep() is a function
 *  reference.  On IRIX systems, sleep() is a subroutine
 *  reference.
 */

#include <errno.h>
#include <fortran.h>
#include <sys/types.h>
#include <unistd.h>

#ifdef _UNICOS
void
PXFSLEEP(
#else	/* _UNICOS */
void
_PXFSLEEP(
#endif	/* _UNICOS */
	_f_int *iseconds,
	_f_int *isecleft,
	_f_int *ierror
)
{
	unsigned int sec, retsec;
	sec = *iseconds;
	*ierror = 0;
	*isecleft = 0;

	/* possible EINTR for interrupt */
	if ((retsec = sleep(sec)) > 0) {
		*isecleft = retsec;
	}
	return;
}

#ifndef _UNICOS
void
pxfsleep_(
	_f_int *iseconds,
	_f_int *isecleft,
	_f_int *ierror
)
{
	_PXFSLEEP(iseconds, isecleft, ierror);
}
#endif	/* not _UNICOS */
