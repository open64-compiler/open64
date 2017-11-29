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


#pragma ident "@(#) libf/pxf/pxfexecvp.c	92.1	06/29/99 11:36:06"

#include <errno.h>
#include <liberrno.h>
#include <fortran.h>
#include <string.h>
#include <malloc.h>
#include <unistd.h>

extern char *_fc_acopy(_fcd f);

/*
 *  PXFEXECVP  -- execute a new process image file
 *  (section 3.1.2 of Posix 1003.9-1992)
 *
 *  Synopsis:
 *     SUBROUTINE PXFEXECVP (FILE,LENFILE,ARGV,LENARGV,IARGC,IERROR)
 *     INTEGER LENFILE,IARGC,LENARGV(0:IARGC-1),IERROR
 *     CHARACTER*(*) FILE,ARGV(0:IARGC-1)
 *
 *  Function description:
 *  The routine PXFEXECVP uses the execvp(2) system call to replace the
 *  current process image with a new process image.
 *
 *  Description of arguments:
 *  FILE     is an input character variable or array element containing
 *           the file of the of the new process image file. If FILE
 *           contains an slash character, FILE will be used as the pathname
 *           for the new process image file. Otherwise, the directories
 *           listed in the PATH environment variable are searched using
 *           each directory in PATH as a pathname prefix for FILE.
 *
 *  LENFILE  is an input integer variable for the length of FILE. If
 *           LENFILE is zero, trailing blanks are removed.
 *
 *  ARGV     is an input array of character strings. ARGV contains the
 *           arguments to be passed to the new process image.
 *
 *  LENARGV  is an input array of intergers. Each element in LENARGV
 *           contains the length of the corresponding character
 *           string in ARGV. If an element in LENARGV is zero, the
 *           corresponding element in ARGV will have all trailing
 *           blanks stripped.
 *
 *  IARGC    is an input integer variable. IARGC contains the number of
 *           arguments to pass to the new process image.
 *
 *  IERROR   is an output integer variable that will contain
 *           the status:
 *
 *            zero    - PXFEXECVP was unsuccessful.
 *
 *            nonzero - PXFEXECVP was not successful.
 *
 *           PXFEXECVP may return any of the following error
 *           values:
 *
 *           EACCES       If the new process file is not a regular file,
 *                        the new process image file mode denies execution
 *                        permission, or search permission is denied for
 *                        a directory listed in the new process image
 *                        file's path prefix.
 *
 *           ENOENT       If one or more components of the new process
 *                        image file's path name do not exist.
 *
 *           ENOMEM       If the memory needed to create structures used
 *                        by PXFEXECVP could not be allocated.
 *
 *           EINVAL       If LENFILE < 0 or LENFILE > LEN(FILE) or any
 *                        element of LENARGV is less than zero or greater
 *                        than the length of the corresponding element in
 *                        ARGV.
 *
 *           UNICOS 9 only errors:
 *
 *           E2BIG        If the number of bytes in ARGV is greater than
 *                        the system-imposed limit of ARG_MAX found in
 *                        <limits.h>.
 *
 *           EDMOFF       If the process image file is offline, and
 *                        the data migration facility is not configured
 *                        in the system.
 *
 *           EFAULT       If the new process image file is not as long as
 *                        indicated by the size values in it header.
 *
 *           ENOMEM       If the new process requires more memory than is
 *                        allowed by the system-imposed maximum MAXMEM.
 *
 *           EOFFLIN      If the process image file is offline, and
 *                        automatic file retrieval is disabled.
 *
 *           EOFLNDD      If the file is offline, and the data management
 *                        daemon is not currently executing.
 * 
 *           EOFLNNR      If the file is offline, and it is currently
 *                        unretrievable.
 *
 *           IRIX 6.2 only errors:
 *
 *           E2BIG        If the number of bytes in the new process's argument
 *                        list is greater than the system-imposed limit
 *                        {ARG_MAX} [see sysconf(2), intro(2), and limits.h].
 *                        The argument list limit is the sum of the size of
 *                        the argument list plus the size of the environment's
 *                        exported shell variables.
 *
 *           E2BIG        If the number of bytes in the first line of an
 *                        interpreter file is greater than 256 bytes.
 *
 *           EAGAIN       If there is not enough memory.
 *
 *           ELIBACC      If the required shared library does not have
 *                        execute permission.
 *
 *           ELIBEXEC     If PATH points to a shared library.
 *
 *           ELIBMAX      If the required number of shared libraries
 *                        exceeds the system imposed maximum {SHLIB_MAX)
 *                        [see intro(2)].
 *
 *           ELOOP        If too many symbolic links were encountered in
 *                        translating PATH.
 *
 *           ENAMETOOLONG If the length of PATH exceeds PATH_MAX found
 *                        in <limits.h>, or the length of a path
 *                        component exceeds NAME_MAX found in <limits.h>
 *                        while POSIX_NO_TRUNC is in effect.
 *
 *           ENOEXEC      If the executable process image file has badly
 *                        formed header information or the requested
 *                        virtual addresses are not available.
 *
 *           ENOMEM       If the new process image requires more virtual
 *                        space than is allowed either by the
 *                        system-imposed maximum or the process imposed
 *                        maximum PROCSIZE_MAX [see getrlimit(2) and
 *                        intro(2)].
 *
 *           EPERM        If a non-superuser tries to execute a setuid
 *                        file which belongs to some other user and the
 *                        file system in which then file resides has
 *                        been mounted with the nosuid option [see
 *                        fstab(4)], or if a non-superuser attempts to
 *                        execute a setuid or setgid shell script with
 *                        a uid or gid which is different than the user's
 *                        effective uid/gid, and the configured value for
 *                        nosuidshells is non-zero (the default) [see
 *                        intro(2) and lboot(1M)].
 *
 */

#ifdef _UNICOS
void
PXFEXECVP(
#else
void
_PXFEXECVP(
#endif
	  _fcd FILE,
	  _f_int *LENFILE,
	  _fcd ARGV,       /* packed array of fortran strings */
	  _f_int *LENARGV,
	  _f_int *IARGC,
	  _f_int *IERROR
)
{
  char **arg,        /* vector of argument strings for execv */
    *cfile,          /* filename for executable */
    *cstring_ARGV;   /* the C-style string for the ARGV fortran character
                      * descriptor */
  int clenfile,      /* equal to *LENFILE, the user defined length of FILE */
    i,               /* loop counter */
    position,        /* current position in the string cstring_ARGV */
    ciargc,          /* equal to *IARGC, the number of arguments for execv */
    cstring_lenargv, /* the length of the FCD ARGV. Note: This is the length of
		        an individual FCD in the array ARGV. */
    len;             /* length of string to copy from cstring_ARGV to a string
			in the arg vector of strings. */

  clenfile = *LENFILE;
  cstring_lenargv = _fcdlen(ARGV);
  ciargc = *IARGC;

  /* check for valid path length passed in by user */
  if (clenfile < 0 || clenfile > _fcdlen(FILE)) {
    *IERROR = EINVAL;
    return;
  } else {
    if (clenfile == 0) {
      /*
       * If length is zero, user wants trailing blanks stripped.
       * Otherwise, malloc memory and copy the string adding a
       * NULL terminator.
       */
      cfile = _fc_acopy(FILE);

    } else {
      
      cfile = (char *)malloc(clenfile + 1);
      if (cfile != NULL) {
	memcpy(cfile, _fcdtocp(FILE), clenfile);
	cfile[clenfile] = '\0';
      } else {
	*IERROR = ENOMEM;
	return;
      }

    }
  }

  /* attempt to copy all argument strings from ARGV */

  /* check the LENARGV array for proper values before copying ARGV strings */
  i = 0;
  while (i < ciargc) {
    len = LENARGV[i];
    if (len < 0 || len > cstring_lenargv) {
      *IERROR = EINVAL;
      free(cfile);
      return;
    }
    i++;
  }

  arg = (char **)calloc(ciargc + 1,sizeof(char *));
  if (arg == NULL) {
    *IERROR = ENOMEM;
    free(cfile);
    return;
  }

  cstring_ARGV = _fcdtocp(ARGV);
  
  /* malloc the memory for all the strings copy each Fortran string
   * into a C-style string */
  for (i = 0, position = 0; i < ciargc; position += cstring_lenargv, i++) {
    len = LENARGV[i];
    /* strip off trailing blanks */
    if (len == 0) {
      len = cstring_lenargv - 1;
      while ((len > 0) &&
	     cstring_ARGV[(i * cstring_lenargv) + len] == ' ') {
	len--;
      }
      len++;
    }
    if ((arg[i] = (char *)malloc((len+1)*sizeof(char))) == NULL) {
      for (; i >= 0; i--) {
	free(arg[i]);
      }
      free(arg);
      free(cfile);
      *IERROR = ENOMEM;
      return;
    }
    
    strncpy(arg[i], &cstring_ARGV[position], len);
    arg[i][len] = '\0';
  }
  if (execvp(cfile, arg) == -1) {
    for (i--; i >= 0; i--) {
      free(arg[i]);
    }
    free(arg);
    free(cfile);
    *IERROR = errno;
    return;
  }

  *IERROR = 0;
}

#ifndef _UNICOS
void
pxfexecvp_(
	  char *FILE,
	  _f_int *LENFILE,
	  char *ARGV,
	  _f_int *LENARGV,
	  _f_int *IARGC,
	  _f_int *IERROR,
	  _f_int filelen,
	  _f_int argvlen
)
{
  _PXFEXECVP( _cptofcd(FILE,filelen), LENFILE, _cptofcd(ARGV,argvlen),
	      LENARGV, IARGC, IERROR);
}
#endif





