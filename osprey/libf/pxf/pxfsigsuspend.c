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


#pragma ident "@(#) libf/pxf/pxfsigsuspend.c	92.1	06/29/99 11:36:06"


/*
 *  PXFSIGSIGSUSPEND -- Wait for a signal.
 *  (section 3.3.7 of Posix 1003.9-1992)
 *
 *  Synopsis:
 *
 *     SUBROUTINE PXFSIGSUSPEND(jsigset, ierror)
 *     INTEGER jsigset, ierror
 *
 *  Where:
 *
 *	jsigset - default integer input variable containing a
 *	          handle created by PXFSTRUCTCREATE('sigset',...)
 *	ierror  - default integer output variable containing
 *	          status:
 *
 *           zero	- PXFSIGSUSPEND was successful.
 *
 *           nonzero	- PXFSIGSUSPEND was not successful.
 *
 *  PXFSIGSUSPEND returns the following error:
 *
 *	EINTR      -  Signal is caught by the calling process and
 *                    control is returned from the signal-catching
 *                    function.
 *
 *      EBADHANDLE - The jsigset argument is invalid.
 *
 *  DESCRIPTION:
 *
 *  Subroutine procedure PXFSIGSUSPEND calls the sigsuspend()
 *  function to replace the signal mask of the process with the
 *  jsigset set of signals and to suspend the process indefinitely.
 *  PXFSIGSUSPEND always returns an error if control is returned.
 *
 */

#include <errno.h>
#include <fortran.h>
#include <liberrno.h>
#include <string.h>
#include <sys/signal.h>
#include "pxfstruct.h"
#include "pxfstructtable.h"
#include "table.h"

#ifndef _UNICOS
#include <stddef.h>
#endif

#ifdef _UNICOS
void
PXFSIGSUSPEND(
#else	/* _UNICOS */
void
_PXFSIGSUSPEND(
#endif	/* _UNICOS */
	_f_int *jsigset,
	_f_int *ierror
)
{
	int	stat;
	sigset_t mask;
	struct	pxfhandle pxfhand;
	*ierror = 0;
	pxfhand = _pxfhandle_table_lookup(&_pxfhandle_table, *jsigset);
	if (pxfhand.pxfstructptr == NULL || pxfhand.pxftype != PXF_SIGSET) {
		*ierror = EBADHANDLE;
		return;
	}
	mask = ((struct sig_set *)(pxfhand.pxfstructptr))->samask;
	if ((stat = (_f_int) sigsuspend(&mask)) == -1) {
		*ierror = errno;
	}
	return;
}

#ifndef _UNICOS
void
pxfsigsuspend_(
	_f_int *jsigset,
	_f_int *ierror
)
{
	_PXFSIGSUSPEND(jsigset, ierror);
}
#endif	/* not _UNICOS */
