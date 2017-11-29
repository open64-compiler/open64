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


#pragma ident "@(#) libf/pxf/pxftcgetattr.c	92.2	06/29/99 11:36:06"

/*
 *  PXFTCGETATTR  -- get state
 *  PXFTCSETATTR  -- set state
 *             (section 7.2.1 of Posix 1003.9-1992)
 *
 *  Synopsis:
 *
 *     SUBROUTINE PXFTCGETATTR(ifildes, jtermios, ierror)
 *     INTEGER ifildes, jtermios, ierror
 *
 *     SUBROUTINE PXFTCSETATTR(ifildes, ioptacts, jtermios, ierror)
 *     INTEGER ifildes, ioptacts, jtermios, ierror
 *
 *  Description:
 *
 *  PXFTCGETATTR uses the c function tcgetattr() to get parameters
 *  associated with a file descriptor and stores them in the termios
 *  structure.
 *
 *  PXFTCGETATTR uses the c function tcsetattr() to set the parameters
 *  associated with a terminal from the termios structure.
 *
 *  The arguments are:
 *
 *      ifildes  -  default integer input variable containing a file
 *                  descriptor.
 *      ioptacts -  default input integer variable for an optional
 *                  action.  Use PXFCONST to retrieve the values of
 *                  the optional actions.
 *      jtermios -  default integer input variable containing a handle
 *                  created by PXFSTRUCTCREATE('termios',...).
 *      ierror   -  default integer output variable that contains zero
 *                  if the operation was successful or nonzero if the
 *                  operation was not successful.
 *
 *   PXFTCGETATTR and PXFTCSETATTR may return one of the following
 *   error values:
 *
 *   EBADF      If ifildes is not a valid file descriptor.
 *
 *   EBADHANDLE The jtermios argument is invalid.
 *
 *   EINTR      A signal interrupted the tcsetattr() function.
 *
 *   EINVAL     The ioptacts argument is invalid.
 *
 *   ENOTTY     The file associated with ifilldes is not a terminal.
 *
 */

#include <errno.h>
#include <fortran.h>
#include <liberrno.h>
#include <string.h>
#include <sys/errno.h>
#include <sys/termios.h>
#include <termios.h>
#include "pxfstruct.h"
#include "table.h"

#ifdef _UNICOS
void
PXFTCGETATTR(
#else	/* _UNICOS */
void
pxftcgetattr_(
#endif	/* _UNICOS */
	_f_int	*ifildes,
	_f_int	*jtermios,
	_f_int	*ierror)
{
	int	fildes;
	int	stat;
	struct  pxfhandle pxfhand;
	struct termios *trmios;
	fildes	= *ifildes;
	*ierror	= 0;
	pxfhand	= _pxfhandle_table_lookup(&_pxfhandle_table, *jtermios);
	if (pxfhand.pxfstructptr == NULL || pxfhand.pxftype != PXF_TERMIOS) {
		*ierror	= EBADHANDLE;
		return;
	}

	trmios	= pxfhand.pxfstructptr;
	if (stat = tcgetattr(fildes, trmios) == -1)
		*ierror	= errno;
	return;
}

#ifdef _UNICOS
void
PXFTCSETATTR(
#else	/* _UNICOS */
void
pxftcsetattr_(
#endif	/* _UNICOS */
	_f_int	*ifildes,
	_f_int	*jtermios,
	_f_int	*ioptacts,
	_f_int	*ierror)
{
	int	fildes;
	int	stat;
	int	optact;
	struct  pxfhandle pxfhand;
	struct termios *trmios;
	fildes	= *ifildes;
	*ierror	= 0;
	optact	= *ioptacts;
	pxfhand	= _pxfhandle_table_lookup(&_pxfhandle_table, *jtermios);
	if (pxfhand.pxfstructptr == NULL || pxfhand.pxftype != PXF_TERMIOS) {
		*ierror	= EBADHANDLE;
		return;
	}

	trmios	= pxfhand.pxfstructptr;
	if (stat = tcsetattr(fildes,optact,trmios) == -1)
		*ierror	= errno;
	return;
}

