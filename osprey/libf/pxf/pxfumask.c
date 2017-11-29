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


#pragma ident "@(#) libf/pxf/pxfumask.c	92.1	06/29/99 11:36:06"

#include <errno.h>
#include <liberrno.h>
#include <fortran.h>
#include <sys/types.h>
#include <sys/stat.h>

/*  PXFUMASK  -- set file creation mask
 *  (section 5.3.3 of Posix 1003.9-1992)
 *
 *  Synopsis:
 *     SUBROUTINE PXFUMASK(ICMASK,IPREVCMASK,IERROR)
 *     INTEGER ICMASK,IPREVCMASK,IERROR
 *
 *  Function description:
 *  The routine PXFUMASK uses the umask(2) system call to change
 *  the file mode creation mask of the calling process. Only the
 *  file permission bits of ICMASK are used.
 *
 *  Description of arguments:
 *  ICMASK     is an input integer variable for the new file mode
 *             creation mask.
 *
 *  IPREVCMASK is an output integer variable for the old file mode
 *             creation mask.
 *
 *  IERROR     is an output integer variable that will contain
 *             the status:
 *
 *              zero    - PXFUMASK was successful.
 *
 */

#ifdef _UNICOS
void
PXFUMASK(
#else
void
_PXFUMASK(
#endif
	  _f_int *ICMASK,
	  _f_int *IPREVCMASK,
	  _f_int *IERROR
)
{
  *IERROR = 0;
  *IPREVCMASK = umask(((mode_t)*ICMASK));
}

#ifndef _UNICOS
void
pxfumask_(
 	  _f_int *ICMASK,
	  _f_int *IPREVCMASK,
	  _f_int *IERROR
)
{
  _PXFUMASK(ICMASK, IPREVCMASK, IERROR);
}
#endif

