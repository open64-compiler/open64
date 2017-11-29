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


#pragma ident "@(#) libf/pxf/pxfgetcwd.c	92.1	06/29/99 11:36:06"


#include <errno.h>
#include <fortran.h>
#include <liberrno.h>
#include <string.h>
#include <unistd.h>
#include <sys/param.h>

#ifndef _UNICOS
#include <stddef.h>
#endif

#define	BLANK	((int) ' ')

/*
 *  PXFGETCWD  -- get working directory pathname
 *  (section 5.2.2 of Posix 1003.9-1992)
 *
 *  Call from Fortran:
 *
 *      SUBROUTINE PXFGETCWD(BUF, ILEN, IERROR)
 *      CHARACTER*(*) BUF
 *      INTEGER ILEN, IERROR
 *
 *  Where:
 *
 *  BUF      is an output character variable or array element
 *           for the current working directory. The longest
 *           pathname will cannot be longer than PATH_MAX for
 *	    Unicos or MAXPATHLEN for Solaris and IRIX as
 *	    defined in <sys/param.h>.
 *
 *  ILEN     is an output integer variable containing the
 *           character length of BUF.
 *
 *  IERROR   is an output integer variable that will contain
 *           the status:
 *
 *            zero    - the working directory path was
 *                      successfully copied into BUF.
 *
 *            nonzero - PXFGETCWD was not successful.
 *
 *           PXFGETCWD may return any of the following error
 *           values:
 *
 *            ETRUNC  If the length of BUF is less than the
 *                    complete path length.
 *
 *            EACCESS If read or search permission for any
 *                    component of the current working directory
 *                    path was denied.
 *
 *
 */

#ifdef _UNICOS
void
PXFGETCWD(
#else
void
_PXFGETCWD(
#endif
	   _fcd BUF,       /* buffer for full path */
	   _f_int *ILEN,   /* buffer length in characters */
	   _f_int *IERROR  /* error status */
)
{
  int bufsize,      /* size of the fortran string */
      pathlen,      /* lenth of the current working directory path */
      movelen,      /* number of characters to copy from pathbuf to
		     * user variable */
#ifdef _UNICOS
      pathbufsize = PATH_MAX+1;
  char pathbuf[PATH_MAX+1];
#else
      pathbufsize = MAXPATHLEN;
  char pathbuf[MAXPATHLEN];
#endif
  char *bufptr;

  *IERROR = 0;
  bufsize = _fcdlen(BUF);
  bufptr = _fcdtocp(BUF);

  if (getcwd(pathbuf, pathbufsize) == NULL) {
    *IERROR = errno;
  } else {

    /* If path length for current working directory is larger
     * than user variable BUF's size, the path must be
     * truncated.
     */

    pathlen = strlen(pathbuf);
    *ILEN = pathlen;
    if (pathlen > bufsize) {
      *IERROR = ETRUNC;
      movelen = bufsize;
    } else {
      movelen = pathlen;
    }

    /* copy argument to user variable */

    if (movelen > 0) {
      (void) memcpy(bufptr, pathbuf, movelen);
    }

    
    /* blank-fill user variable BUF */
    if (bufsize > movelen) {
      (void) memset(bufptr + sizeof(char)*movelen, BLANK,
		    bufsize - movelen);
    }

  }
}


#ifndef _UNICOS
void
pxfgetcwd_(
	   char *BUF,
	   _f_int *ILEN,
	   _f_int *IERROR,
	   _f_int buflen)
{
  _PXFGETCWD(_cptofcd(BUF, buflen), ILEN, IERROR);
}

#endif
