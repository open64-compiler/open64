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


#pragma ident "@(#) libf/pxf/pxfclosedir.c	92.1	06/29/99 11:36:06"

#include <errno.h>
#include <liberrno.h>
#include <fortran.h>
#include <sys/types.h>
#include <dirent.h>
#include <malloc.h>
#include "table.h"

/*  PXFCLOSEDIR  -- close directory
 *  (section 5.1.1 of Posix 1003.9-1992)
 *
 *  Synopsis:
 *     SUBROUTINE PXFCLOSEDIR(IDIRID,IERROR)
 *     INTEGER IDIRID,IERROR
 *
 *  Function description:
 *  The PXFCLOSEDIR routine uses closedir(3C) to close the directory
 *  stream referenced by IDIRID. Upon sucessful completion, IDIRID
 *  is undefined and the result of subsequent calls to PXFCLOSEDIR
 *  with IDIRID is not well defined.
 *
 *  Description of arguments:
 *  IDIRID is an input integer variable containing a unique ID for
 *         a directory stream created by PXFOPENDIR.
 *
 *  IERROR is an output integer variable that will contain the status:
 *
 *             zero    - PXFCLOSEDIR was successful.
 *
 *             nonzero - PXFCLOSEDIR was not successful.
 *
 *         PXFCLOSEDIR may return any of the following error
 *         values:
 *
 *          EBADF If, when detected, an invalid, unique directory
 *                stream ID was used for IDIRID.
 *
 *          EBADID The IDIRID argument is invalid.
 */


#ifdef _UNICOS
void
PXFCLOSEDIR(
#else
void
_PXFCLOSEDIR(
#endif
	     _f_int *IDIRID,
	     _f_int *IERROR
)
{
  int cdirid;
  DIR *dirp;

  /* make sure IDIRID is positive */
  if (*IDIRID < 0) {
    cdirid = -*IDIRID;
  } else {
    cdirid = *IDIRID;
  }

  dirp = (DIR *)_table_lookup(&_pxfdir_table, cdirid);

  if (dirp == NULL) {
    *IERROR = EBADID;
    return;
  }

  if (closedir(dirp) == -1) {
    *IERROR = errno;
    return;
  }

  _table_remove(&_pxfdir_table, *IDIRID);

  *IERROR = 0;
}

#ifndef _UNICOS
void
pxfclosedir_(
	     _f_int *IDIRID,
	     _f_int *IERROR
)
{
  _PXFCLOSEDIR(IDIRID,IERROR);
}
#endif
