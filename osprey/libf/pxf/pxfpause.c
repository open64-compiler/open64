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


#pragma ident "@(#) libf/pxf/pxfpause.c	92.1	06/29/99 11:36:06"


/*
 *  PXFPAUSE -- Suspend Process Execution.
 *  (section 3.4.2 of Posix 1003.9-1992)
 *
 *  Synopsis:
 *
 *     SUBROUTINE PXFPAUSE(ierror)
 *     INTEGER ierror
 *
 *  Where:
 *
 *	ierror   -   default integer output variable containing
 *	             status.  The pause function is never
 *	             successful.
 *
 *           nonzero	- PXFPAUSE returned.
 *
 *  The following error may be returned:
 *
 *      EINTR -   Signal is caught by the calling process and 
 *                control is returned from the signal-catching
 *                function.
 *
 *  DESCRIPTION:
 *
 *  The subroutine procedure PXFPAUSE uses the pause() system call
 *  to suspend process execution.
 *
 *  NOTE:
 *
 *  Replace the use of subprocedure PAUSE() with the PXFPAUSE()
 *  interface in Fortran programs for portability.
 *
 *  On UNICOS and UNICOS/mk systems, pause() is a function or
 *  a subroutine reference.  On IRIX systems, pause() is a
 *  function reference.
 */

#include <errno.h>
#include <fortran.h>
#include <sys/types.h>
#include <unistd.h>

#ifdef _UNICOS
void
PXFPAUSE(
#else	/* _UNICOS */
void
_PXFPAUSE(
#endif	/* _UNICOS */
	_f_int *ierror
)
{
	int stat;
	*ierror = 0;
	/* possible EINTR for interrupt */
	if ((stat = pause()) < 0) {
		*ierror = errno;
	}
	return;
}

#ifndef _UNICOS
void
pxfpause_(
	_f_int *ierror
)
{
	_PXFPAUSE(ierror);
}
#endif	/* not _UNICOS */
