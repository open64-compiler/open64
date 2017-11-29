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


#pragma ident "@(#) libf/pxf/pxfsysconf.c	92.1	06/29/99 11:36:06"

#include <errno.h>
#include <fortran.h>
#include <unistd.h>

/*
 *	PXFSYSCONF	PXF Interface to the sysconf(2) system call.
 *
 *	Call from Fortran:
 *
 *		SUBROUTINE PXFSYSCONF (NAME, IVAL, IERROR)
 *		INTEGER NAME, IVAL, IERROR
 *
 *	Where:
 *
 *	NAME	is the input integer variable containing the integer
 *              value of the symbolic constant for one or more of the
 *              following configurable system variables:
 *
 *			ARG_MAX
 *			CHILD_MAX
 *			CLK_TCK			(CRI extension)
 *			NGROUPS_MAX
 *			OPEN_MAX
 *			STREAM_MAX
 *			TZNAME_MAX
 *			_POSIX_JOB_CONTROL
 *			_POSIX_SAVED_IDS
 *			_POSIX_VERSION
 *
 *
 *	IVAL	is the output integer variable which receives the value
 *		of the system variable being queried.
 *
 *	IERROR	is an output integer variable that will contain the
 *		status:
 *
 *		  Zero    - PXFSYSCONF is successful.
 *
 *		  Nonzero - Error condition (errno) code.
 *
 */

#if	defined(_UNICOS)
void
PXFSYSCONF(
#else
void
_PXFSYSCONF(
#endif
	_f_int	*NAME,
	_f_int	*IVAL,
	_f_int	*IERROR
)
{
	*IVAL = sysconf(*NAME);
	if (*IVAL == -1)
		*IERROR = errno;
	else
		*IERROR = 0;
}

#if	!defined(_UNICOS)
/*
 *	PXFSYSCONF	PXF Interface to the sysconf(2) system call.
 *	Allow on non-UNICOS systems.
 */

void
pxfsysconf_(
	int	*NAME,
	int	*IVAL,
	int	*IERROR
)
{
	_PXFSYSCONF( NAME, IVAL, IERROR);
	return;
}
#endif
