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


#pragma ident "@(#) libf/pxf/pxfgetlogin.c	92.1	06/29/99 11:36:06"


#include <fortran.h>
#include <errno.h>
#include <liberrno.h>
#include <string.h>
#include <stdio.h>

#ifndef _UNICOS
#include <stddef.h>
#endif
#if defined(BUILD_OS_DARWIN)
#include <sys/types.h>
#include <unistd.h>
#include <pwd.h>
#endif /* defined(BUILD_OS_DARWIN) */

#define BLANK   ((int) ' ')

/*
 *  PXFGETLOGIN  -- get user name
 *  (section 4.2.4 of POSIX 1003.9-1992)
 *
 *  Call from Fortran:
 *
 *      SUBROUTINE PXFGETLOGIN(S,ILEN,IERROR)
 *      CHARACTER*(*) S
 *      INTEGER ILEN, IERROR
 *
 *  Where:
 *
 *  S      is an output character variable or array element
 *         for the user's login name.
 *
 *  ILEN   is an output integer variable containing the length
 *         of S.
 *
 *  IERROR is an output integer variable that will contain
 *         the status:
 *
 *          zero    - the user's login name was successful
 *                    acquired.
 *
 *          nonzero - PXFGETLOGIN was unsuccessful.
 *
 *         PXFGETLOGIN may return any of the following error
 *         values:
 *
 *          ETRUNC If the length of S is smaller than the
 *                 the length of the user's login name.
 *
 */

#ifdef _UNICOS
void
PXFGETLOGIN(
#else
void
_PXFGETLOGIN(
#endif
	     _fcd S,
	     _f_int *ILEN,
	     _f_int *IERROR
)
{
  char *loginptr, *sptr;
  int loginlen, slen, movelen, ierr;
  
  slen = _fcdlen(S);
  sptr = _fcdtocp(S);
  ierr = 0;

  /* get user's login name */
#if defined(BUILD_OS_DARWIN)
  struct passwd *p_passwd = getpwuid(geteuid());
  if (p_passwd != NULL & (loginptr = p_passwd->pw_name) != NULL)
#else /* defined(BUILD_OS_DARWIN) */
  if ((loginptr = cuserid(NULL)) != NULL)
#endif /* defined(BUILD_OS_DARWIN) */
  {
    loginlen = strlen(loginptr);
    *ILEN = loginlen;    

    /* check for string truncation */
    if (loginlen > slen) {
      ierr = ETRUNC;
      movelen = slen;
    } else {
      movelen = loginlen;
    }

    /* copy login name to S user variable */
    if (movelen > 0) {
      (void) memcpy(sptr, loginptr, movelen);
    }

    /* blank-fill user variable S */
    if (slen > movelen) {
      (void) memset(sptr + sizeof(char)*movelen, BLANK,
                    slen - movelen);
    }

  }
  *IERROR = ierr;
}


#ifndef _UNICOS
void
pxfgetlogin_(
	     char *S,
	     _f_int *ILEN,
	     _f_int *IERROR,
	     _f_int slen
)
{
  _PXFGETLOGIN(_cptofcd(S, slen), ILEN, IERROR);
}
#endif
