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


#pragma ident "@(#) libf/pxf/pxfopendir.c	92.1	06/29/99 11:36:06"

#include <errno.h>
#include <liberrno.h>
#include <fortran.h>
#include <string.h>
#include <malloc.h>
#include <sys/types.h>
#include <dirent.h>
#include "table.h"

/*  PXFOPENDIR  -- open directory
 *  (section 5.1.2 of Posix 1003.9-1992)
 *
 *  Synopsis:
 *     SUBROUTINE PXFOPENDIR(DIRNAME,LENDIRNAME,IOPENDIRID,IERROR)
 *     CHARACTER*(*) DIRNAME
 *     INTEGER LENDIRNAME,IOPENDIR,IERROR
 *
 *  Function Description:
 *  The routine PXFOPENDIR uses opendir() (3C) to open a directory
 *  directory stream for the directory DIRNAME and positions the
 *  stream at the first directory entry.
 *
 *  Description of arguments:
 *  DIRNAME    is an input character array variable containing the path for
 *             directory to be opened.
 *
 *  LENDIRNAME is an input integer variable containing the length of
 *             DIRNAME.
 *
 *  IOPENDIRID is an output integer variable for the directory ID.
 *
 *  IERROR     is an output integer variable that will contain
 *             the status:
 *
 *              zero    - PXFOPENDIR was successful.
 *
 *              nonzero - PXFOPENDIR was not successful.
 *
 *          PXFOPENDIR may return any of the following error
 *          values:
 *
 *           EACCES       If a component of DIRNAME denies search permission.
 *
 *           ENAMETOOLONG If the length of the DIRNAME argument exceeds
 *                        PATH_MAX found in <limits.h>. (IRIX and Solaris only) 
 *
 *           ENOENT       If the directory in the DIRNAME argument does not exist.
 *
 *           ENOTDIR      If a component of DIRNAME is not a directory.
 *
 *           EINVAL       If LENDIRNAME < 0 or LENDIRNAME > LEN(DIRNAME).
 *
 *           ENOMEM       If memory needed by PXFOPENDIR could not be allocated.
 *
 *           ELOOP        If the the DIRNAME argument contains too many symbolic
 *                        links. (Solaris only)
 *
 *           EMFILE       If too many file descriptors are currently open for the process.
 *
 *           ENFILE       If too many file descriptors are currently open for the system.
 *                        (Solaris and IRIX only)
 *
 */

#ifdef _UNICOS
void
PXFOPENDIR(
#else
void
_PXFOPENDIR(
#endif
	    _fcd DIRNAME,
	    _f_int *LENDIRNAME,
	    _f_int *IOPENDIRID,
	    _f_int *IERROR
)
{
  int ilendirname;
  char *cdirname;
  DIR *dirptr;
  _f_int copendirid;
  
  ilendirname = *LENDIRNAME;
  
  /* check DIRNAME length */
  if (ilendirname < 0 || ilendirname > __fcdlen(DIRNAME)) {
    *IERROR = EINVAL;
    return;
  }
  
  if (*LENDIRNAME == 0) {
    /*
     * If length is zero, user wants trailing blanks stripped.
     * Otherwise, malloc memory and copy the string adding a
     * NULL terminator.
     */
    
    cdirname = _fc_acopy(DIRNAME);
    if (cdirname == NULL) {
      *IERROR = ENOMEM;
      return;
    }
    
  } else {
    
    cdirname = (char *)malloc(ilendirname + 1);
    if (cdirname != NULL) {
      (void)strncpy(cdirname, _fcdtocp(DIRNAME), ilendirname);
      cdirname[ilendirname] = '\0';
    } else {
      *IERROR = ENOMEM;
      return;
    }
    
  }
  
  if ((dirptr = opendir(cdirname)) == NULL) {
    free(cdirname);
    *IERROR = errno;
    return;
  }
  
  /* assign the dirptr. NOTE: The user is now responsible to manage the
   * the memory */
  copendirid = _table_add(&_pxfdir_table, dirptr);
  if (copendirid > 0) {
    /* negate the id value so readdir can determine if the corresponding
     * DIR structure has been used to read directory entries.
     */
    *IOPENDIRID = -copendirid;
    *IERROR = 0;
  } else {
    *IERROR = ENOMEM;
  }

  free(cdirname);
}

#ifndef _UNICOS
void
pxfopendir_(
	    char *DIRNAME,
	    _f_int *LENDIRNAME,
	    _f_int *IOPENDIRID,
	    _f_int *IERROR,
	    _f_int dirnamelen
)
{
  _PXFOPENDIR(_cptofcd(DIRNAME,dirnamelen),LENDIRNAME,
	      IOPENDIRID,IERROR);
}
#endif

