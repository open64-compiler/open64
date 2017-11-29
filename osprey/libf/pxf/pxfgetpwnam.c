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


#pragma ident "@(#) libf/pxf/pxfgetpwnam.c	92.3	10/29/99 21:41:49"


#include <fortran.h>
#include <errno.h>
#include <liberrno.h>
#include <malloc.h>
#include <pwd.h>
#include <string.h>
#include "pxfstruct.h"
#include "table.h"

extern char *_fc_acopy(_fcd f);

/*
 *  PXFGETPWNAM  -- get password information using a login name
 *  (section 9.2.2 of Posix 1003.9-1992)
 *
 *  Call from Fortran:
 *
 *     SUBROUTINE PXFGETPWNAM(NAME,ILEN,JPASSWD,IERROR)
 *     INTEGER ILEN,JPASSWD,IERROR
 *     CHARACTER*(*) NAME
 *
 *  Where:
 *
 *  NAME     is an input character variable or array element
 *           containing the login name for which password information
 *           is requested.
 *
 *  ILEN     is an input integer variable containing the character
 *           length of NAME. If ILEN is zero, training blanks are
 *           stripped.
 *
 *  JPASSWD  is a output handle of type 'passwd' created with
 *           PXFSTRUCTCREATE.
 *
 *  IERROR   is an output integer variable that will contain
 *           the status:
 *
 *            zero    - PXFGETPWNAM was successful.
 *
 *            nonzero - PXFGETPWNAM was not successful.
 *
 *           PXFGETPWNAM may return any of the following error
 *           values:
 *
 *            ENOENT If an entry matching the username in NAME was
 *                   not found.
 *
 *            EBADHANDLE If JPASSWD is invalid.
 *
 */


#ifdef _UNICOS
void
PXFGETPWNAM(
#else
void
_PXFGETPWNAM(
#endif
	     _fcd NAME,
	     _f_int *ILEN,
	     _f_int *JPASSWD,
	     _f_int *IERROR
)
{
  int cilen;
  char *cname;
  struct passwd *passwdsrc, passwdtemp, *cjpasswd;
  struct pxfhandle pxfhand;

  cilen = *ILEN;
  pxfhand = _pxfhandle_table_lookup(&_pxfhandle_table, *JPASSWD);
  if (pxfhand.pxfstructptr == NULL || pxfhand.pxftype != PXF_PASSWD) {
    *IERROR = EBADHANDLE;
    return;
  }
  cjpasswd = pxfhand.pxfstructptr;

  /* check for invalid range error on ILEN. */
  if (cilen < 0 || cilen > _fcdlen(NAME)) {
    *IERROR = EINVAL;
  } else {

    if (cilen == 0) {
      /*
       * If length is zero, user wants trailing blanks stripped.
       * Otherwise, malloc memory and copy the string adding a
       * NULL terminator.
       */
      cname = _fc_acopy(NAME);

    } else {
      cname = (char *) malloc (cilen + 1);
      if (cname != NULL) {
        (void)memcpy(cname, _fcdtocp(NAME), cilen);
        cname[cilen] ='\0';
      } else {
        *IERROR = ENOMEM;
        return;
      }
    }

    /* make call to getpwnam */
    if ((passwdsrc = getpwnam(cname)) != NULL) {

      free(cname);

      /* copy the structures components since static storage is used */
      /* component: pw_name (login name) */
      passwdtemp.pw_name =
	(char *) malloc((strlen(passwdsrc->pw_name)+1)*sizeof(char));
      if (passwdtemp.pw_name == NULL) {
	*IERROR = ENOMEM;
	return;
      }
      (void)strcpy(passwdtemp.pw_name, passwdsrc->pw_name);

      /* component: pw_uid (user ID) */
      passwdtemp.pw_uid = passwdsrc->pw_uid;
      
      /* component: pw_gid (group ID) */
      passwdtemp.pw_gid = passwdsrc->pw_gid;

      /* component: pw_dir (default login directory) */
      passwdtemp.pw_dir =
	(char *) malloc((strlen(passwdsrc->pw_dir)+1)*sizeof(char));
      if (passwdtemp.pw_dir == NULL) {
	*IERROR = ENOMEM;
	free(passwdtemp.pw_name);
	return;
      }
      (void)strcpy(passwdtemp.pw_dir, passwdsrc->pw_dir);

      /* component: pw_shell (default login shell) */
      passwdtemp.pw_shell =
	(char *) malloc((strlen(passwdsrc->pw_shell)+1)*sizeof(char));
      if (passwdtemp.pw_shell == NULL) {
	*IERROR = ENOMEM;
	free(passwdtemp.pw_name);
	free(passwdtemp.pw_dir);
	return;
      }
      (void)strcpy(passwdtemp.pw_shell, passwdsrc->pw_shell);

      /* components not supported in Posix 1003.9-1992, but supported in target OSes */
      /* component: pw_passwd (encrypted password) */
      passwdtemp.pw_passwd =
	(char *)malloc((strlen(passwdsrc->pw_passwd)+1)*sizeof(char));
      if (passwdtemp.pw_passwd == NULL) {
	*IERROR = ENOMEM;
	free(passwdtemp.pw_name);
	free(passwdtemp.pw_dir);
	free(passwdtemp.pw_shell);
	return;
      }
      (void)strcpy(passwdtemp.pw_passwd, passwdsrc->pw_passwd);

#ifndef	_LITTLE_ENDIAN
      /* component: pw_age (password age) */
      passwdtemp.pw_age =
	(char *)malloc((strlen(passwdsrc->pw_age)+1)*sizeof(char));
      if (passwdtemp.pw_age == NULL) {
	*IERROR = ENOMEM;
	free(passwdtemp.pw_name);
	free(passwdtemp.pw_dir);
	free(passwdtemp.pw_shell);
	free(passwdtemp.pw_passwd);
	return;
      }
      (void)strcpy(passwdtemp.pw_age, passwdsrc->pw_age);

      /* component: pw_comment (comment) */
      passwdtemp.pw_comment =
	(char *)malloc((strlen(passwdsrc->pw_comment)+1)*sizeof(char));
      if (passwdtemp.pw_comment == NULL) {
	*IERROR = ENOMEM;
	free(passwdtemp.pw_name);
	free(passwdtemp.pw_dir);
	free(passwdtemp.pw_shell);
	free(passwdtemp.pw_passwd);
	free(passwdtemp.pw_age);
	return;
      }
      (void)strcpy(passwdtemp.pw_comment, passwdsrc->pw_comment);
#endif	/* not _LITTLE_ENDIAN */

      /* component: pw_gecos */
      passwdtemp.pw_gecos =
	(char *)malloc((strlen(passwdsrc->pw_gecos)+1)*sizeof(char));
      if (passwdtemp.pw_gecos == NULL) {
	*IERROR = ENOMEM;
	free(passwdtemp.pw_name);
	free(passwdtemp.pw_dir);
	free(passwdtemp.pw_shell);
	free(passwdtemp.pw_passwd);
#ifndef	_LITTLE_ENDIAN
	free(passwdtemp.pw_age);
#endif	/* not _LITTLE_ENDIAN */
	free(passwdtemp.pw_gecos);
	return;
      }
      (void)strcpy(passwdtemp.pw_gecos, passwdsrc->pw_gecos);

    } else {
      *IERROR = errno;
      free(cname);
      return;
    }
  }

  /* free all components for the jpasswd handle. NOTE: free() as defined in ANSI C
   * checks for a NULL pointer so this extra check does not need to be performed. */
  free(cjpasswd->pw_name);
  free(cjpasswd->pw_passwd);
#ifndef	_LITTLE_ENDIAN
  free(cjpasswd->pw_age);
  free(cjpasswd->pw_comment);
#endif	/* not _LITTLE_ENDIAN */
  free(cjpasswd->pw_gecos);
  free(cjpasswd->pw_dir);
  free(cjpasswd->pw_shell);

  *cjpasswd = passwdtemp;
}

#ifndef _UNICOS
void
pxfgetpwnam_(
	     char *NAME,
	     _f_int *ILEN,
	     _f_int *JPASSWD,
	     _f_int *IERROR,
	     _f_int namelen
)
{
  _PXFGETPWNAM(_cptofcd(NAME,namelen),ILEN,JPASSWD,IERROR);
}
#endif

