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


#pragma ident "@(#) libf/pxf/pxfsigprocmask.c	92.2	10/29/99 21:41:49"


/*
 *  PXFSIGSIGPROCMASK -- Examine and change blocked signals.
 *  (section 3.3.5 of Posix 1003.9-1992)
 *
 *  Synopsis:
 *
 *     SUBROUTINE PXFSIGPROCMASK(ihow, jsigset, josigset, ierror)
 *     INTEGER ihow, jsigset, josigset, ierror
 *
 *  Where:
 *
 *	ihow     - default integer input variable containing
 *	           the manner in which the set is changed.  The
 *	           possible values are:
 *
 *	              SIG_BLOCK    - add to current mask set
 *	              SIG_UNBLOCK  - delete from current mask set
 *	              SIG_SETMASK  - Replace current mask set
 *
 *	           This argument is ignored if josigset is NULL.
 *	           If ihow is zero, the current set is not changed.
 *
 *                 On IRIX systems, the value may also be set to
 *
 *	              SIG_NOP      - do not alter current mask set
 *
 *	           This value is equivalent to providing a NULL
 *	           handle for josigset.
 *
 *	jsigset  - default integer input variable containing a
 *	           handle created by PXFSTRUCTCREATE('sigset',...)
 *	           If nonzero, the handle points to a set of signals
 *	           that can be used to change the currently blocked
 *	           set.
 *	josigset - default integer output variable containing a
 *	           handle created by PXFSTRUCTCREATE('sigset',...)
 *	           If nonzero, the handle will be set to the
 *	           currently blocked signal set.
 *	           If zero, the currently blocked signal set
 *	           will not be changed and the value of ihow
 *	           is ignored.
 *	ierror   - default integer output variable containing
 *	           status:
 *
 *           zero	- PXFSIGPROCMASK was successful.
 *
 *           nonzero	- PXFSIGPROCMASK was not successful.
 *
 *  PXFSIGPROCMASK may return one of the following error values:
 *
 *	EFAULT     The jsigset or josigset argument points outside
 *                 the allocated space.
 *
 *	EINVAL     The value of ihow is an invalid or unsupported
 *                 operation.
 *
 *	EBADHANDLE The jsigset argument is invalid.
 *
 *  DESCRIPTION:
 *
 *  Subroutine procedure PXFSIGPROCMASK uses sigprocmask() function
 *  to examine and change the currently blocked signal set.
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
PXFSIGPROCMASK(
#else	/* _UNICOS */
void
_PXFSIGPROCMASK(
#endif	/* _UNICOS */
	_f_int *ihow,
	_f_int *jsigset,
	_f_int *josigset,
	_f_int *ierror
)
{
	int	stat;
	sigset_t amask, bmask;
	sigset_t *amaskp, *bmaskp;
	struct	pxfhandle pxfhand1;
	struct	pxfhandle pxfhand2;
	*ierror = 0;
	if (*jsigset != 0) {
		pxfhand1 = _pxfhandle_table_lookup(&_pxfhandle_table, *jsigset);
		if (pxfhand1.pxftype != PXF_SIGSET) {
			*ierror = EBADHANDLE;
			return;
		}
		amask = ((struct sig_set *)(pxfhand1.pxfstructptr))->samask;
		amaskp = &amask;
	} else
		amaskp = NULL;
	if (*josigset != 0) {
		pxfhand2 = _pxfhandle_table_lookup(&_pxfhandle_table, *josigset);
		if (pxfhand2.pxftype != PXF_SIGSET) {
			*ierror = EBADHANDLE;
			return;
		}
		bmask = ((struct sig_set *)(pxfhand2.pxfstructptr))->samask;
		bmaskp = &bmask;
	} else
		bmaskp = NULL;
	if ((stat = (_f_int) sigprocmask(*ihow, amaskp, bmaskp)) == -1)
		*ierror = errno;
	else {
		if (bmaskp)
			((struct sig_set *)(pxfhand2.pxfstructptr))->samask = bmask;
	}
	return;
}
#ifndef _UNICOS
void
pxfsigprocmask_(
	_f_int *ihow,
	_f_int *jsigset,
	_f_int *josigset,
	_f_int *ierror
)
{
	_PXFSIGPROCMASK(ihow, jsigset, josigset, ierror);
}
#endif	/* not _UNICOS */
