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


#pragma ident "@(#) libf/pxf/pxfcfgetospeed.c	92.2	06/29/99 11:36:06"

/*
 *  PXFCFGETISPEED  -- get input baud rate
 *  PXFCFSETISPEED  -- set input baud rate
 *  PXFCFGETOSPEED  -- get output baud rate
 *  PXFCFSETOSPEED  -- set output baud rate
 *             (section 7.1.3 of Posix 1003.9-1992)
 *
 *  Synopsis:
 *
 *     SUBROUTINE PXFCFGETISPEED(jtermios, iospeed, ierror)
 *     INTEGER jtermios, iospeed, ierror
 *
 *     SUBROUTINE PXFCFSETISPEED(jtermios, ispeed, ierror)
 *     INTEGER jtermios, ispeed, ierror
 *
 *     SUBROUTINE PXFCFGETOSPEED(jtermios, iospeed, ierror)
 *     INTEGER jtermios, iospeed, ierror
 *
 *     SUBROUTINE PXFCFSETOSPEED(jtermios, ispeed, ierror)
 *     INTEGER jtermios, ispeed, ierror
 *
 *  Description:
 *
 *  The PXFCF...SPEED routines use the c functions to get or set the
 *  baud rates in the termios structure.  Symbolic names for the baud
 *  rates can be obtained through calls to PXFCONST.
 *
 *  The arguments are:
 *
 *      jtermios -  default integer input variable containing a handle
 *                  created by PXFSTRUCTCREATE('termios',...).
 *      ispeed   -  default input integer variable for a baud rate.
 *      iospeed  -  default output integer variable specifying a baud
 *                  rate.
 *      ierror   -  default integer output variable that contains zero
 *                  if the operation was successful or nonzero if the
 *                  operation was not successful.
 *
 *   PXFCF...SPEED routines may return one of the following error values:
 *   No errors are returned for bad baud rates.  The PXFCFSETISPEED and
 *   PXFCFSETOSPEED return 0 if successful; otherwise, they return -1.
 *
 *   EBADHANDLE The jtermios argument is invalid.
 *
 *   EINVAL     Problems occurred with the baud rate.
 *
 */

#include <fortran.h>
#include <liberrno.h>
#include <string.h>
#include <sys/termios.h>
#include <termios.h>
#include "pxfstruct.h"
#include "table.h"

#ifdef _UNICOS
void
PXFCFGETISPEED(
#else	/* _UNICOS */
void
pxfcfgetispeed_(
#endif	/* _UNICOS */
	_f_int	*jtermios,
	_f_int	*iospeed,
	_f_int	*ierror)
{
	speed_t	stat;
	struct  pxfhandle pxfhand;
	struct termios *trmios;
	*ierror	= 0;
	*iospeed	= 0;
	pxfhand	= _pxfhandle_table_lookup(&_pxfhandle_table, *jtermios);
	if (pxfhand.pxfstructptr == NULL || pxfhand.pxftype != PXF_TERMIOS) {
		*ierror	= EBADHANDLE;
		return;
	}

	trmios	= pxfhand.pxfstructptr;
	if (stat = cfgetispeed(trmios) == -1)
		*ierror	= EINVAL;
	else
		*iospeed	= stat;
	return;
}

#ifdef _UNICOS
void
PXFCFGETOSPEED(
#else	/* _UNICOS */
void
pxfcfgetospeed_(
#endif	/* _UNICOS */
	_f_int	*jtermios,
	_f_int	*iospeed,
	_f_int	*ierror)
{
	speed_t	stat;
	struct  pxfhandle pxfhand;
	struct termios *trmios;
	*ierror	= 0;
	*iospeed	= 0;
	pxfhand	= _pxfhandle_table_lookup(&_pxfhandle_table, *jtermios);
	if (pxfhand.pxfstructptr == NULL || pxfhand.pxftype != PXF_TERMIOS) {
		*ierror	= EBADHANDLE;
		return;
	}

	trmios	= pxfhand.pxfstructptr;
	if (stat = cfgetospeed(trmios) == -1)
		*ierror	= EINVAL;
	else
		*iospeed	= stat;
	return;
}

#ifdef _UNICOS
void
PXFCFSETISPEED(
#else	/* _UNICOS */
void
pxfcfsetispeed_(
#endif	/* _UNICOS */
	_f_int	*jtermios,
	_f_int	*ispeed,
	_f_int	*ierror)
{
	int	stat;
	speed_t	newspeed;
	struct  pxfhandle pxfhand;
	struct termios *trmios;
	*ierror	= 0;
	newspeed	= *ispeed;
	pxfhand	= _pxfhandle_table_lookup(&_pxfhandle_table, *jtermios);
	if (pxfhand.pxfstructptr == NULL || pxfhand.pxftype != PXF_TERMIOS) {
		*ierror	= EBADHANDLE;
		return;
	}

	trmios	= pxfhand.pxfstructptr;
	if (stat = cfsetispeed(trmios,newspeed) == -1)
		*ierror	= EINVAL;
	return;
}

#ifdef _UNICOS
void
PXFCFSETOSPEED(
#else	/* _UNICOS */
void
pxfcfsetospeed_(
#endif	/* _UNICOS */
	_f_int	*jtermios,
	_f_int	*ispeed,
	_f_int	*ierror)
{
	int	stat;
	speed_t	newspeed;
	struct  pxfhandle pxfhand;
	struct termios *trmios;
	*ierror	= 0;
	newspeed	= *ispeed;
	pxfhand	= _pxfhandle_table_lookup(&_pxfhandle_table, *jtermios);
	if (pxfhand.pxfstructptr == NULL || pxfhand.pxftype != PXF_TERMIOS) {
		*ierror	= EBADHANDLE;
		return;
	}

	trmios	= pxfhand.pxfstructptr;
	if (stat = cfsetospeed(trmios,newspeed) == -1)
		*ierror	= EINVAL;
	return;
}

