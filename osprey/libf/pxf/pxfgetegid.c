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


#pragma ident "@(#) libf/pxf/pxfgetegid.c	92.1	06/29/99 11:36:06"


#include <fortran.h>
#include <errno.h>
#include <liberrno.h>
#include <sys/types.h>
#include <unistd.h>

/*
 *  PXFGETEGID  -- get effective group ID
 *  (section 4.2.1 of Posix 1003.9-1992)
 *
 *  Call from Fortran:
 *
 *      SUBROUTINE PXFGETEGID(IEGID, IERROR)
 *      INTEGER IEGID, IERROR
 *
 *  Where:
 *
 *  IEGID    is an output integer variable for the effective
 *           group ID for the current process.
 *
 *  IERROR   is an output integer variable that will contain
 *           the status:
 *
 *            zero    - PXFGETEGID was successful. 
 *
 */


#ifdef _UNICOS
void
PXFGETEGID(
#else
void
_PXFGETEGID(
#endif
	    _f_int *IEGID,
	    _f_int *IERROR
)
{
  *IERROR = 0;
  *IEGID = getegid();
}


#ifndef _UNICOS
void
pxfgetegid_(
	    _f_int *IEGID,
	    _f_int *IERROR
)
{
  _PXFGETEGID(IEGID, IERROR);
}
#endif
