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


#pragma ident "@(#) libf/pxf/pxfsystem.c	92.4	10/29/99 21:39:27"
/*
 *  PXFSYSTEM -- Issue a shell command
 *               (Extension to Posix 1003.9-1992)
 *
 *  Synopsis:
 *
 *     SUBROUTINE PXFSYSTEM(cmd, ilen, ierror)
 *     CHARACTER*N cmd
 *     INTEGER ilen, ierror
 *
 *  Description:
 *
 *  PXFSYSTEM uses the system function to pass the character string
 *            argument to a shell command processor as input.  The
 *            character argument is executed as if the argument is
 *            transmitted from a terminal.
 *
 *            Use an empty input string to determine if the command
 *            processor is present.
 *
 *  The arguments are:
 *
 *      cmd    -  default character input variable containing a
 *                string recognized by the command processor.
 *
 *      ilen   -  default integer input variable containing the
 *                length of cmd in characters.  If ILEN is zero,
 *                all trailing blanks at the end of cmd are removed.
 *
 *      ierror -  default integer output variable that contains zero
 *                if the operation was successful or nonzero if the
 *                operation was not successful.
 *
 *   PXFSYSTEM may return one of the following error values:
 *
 *   EINTR      If the fork system call was interrupted by a signal.
 *
 *   EINVAL     If ILEN is less than zero or greater than LEN(CMD).
 *
 *   ENOMEM     If PXFSYSTEM is unable to obtain memory to copy cmd.
 *
 *   On IRIX systems, PXFSYSTEM may also return:
 *
 *   EAGAIN     If the system-imposed limit on the total number of
 *              processes under execution by a single user would
 *              be exceeded.
 *
 *   ENOMEM     If the new process requires more memory than is
 *              allowed by the system-imposed maximum MAXMEM.
 *
 */

#include <fortran.h>
#include <errno.h>
#include <liberrno.h>
#include <unistd.h>
/* for malloc and memcpy */
#ifdef _LITTLE_ENDIAN
#include <stdlib.h>
#include <string.h>
#endif


#ifdef _UNICOS
void
PXFSYSTEM(
#else	/* _UNICOS */
void
_PXFSYSTEM(
#endif	/* _UNICOS */
	   _fcd cmd,		/* character variable string for shell */
	   _f_int *ilen,	/* length in characters of cmd */
	   _f_int *ierror)
{
	_f_int	arglen, errsts, length;
	char	*argstr, *pthstr;
	errsts	= 0;
	argstr	= _fcdtocp(cmd);
	arglen	= _fcdlen(cmd);
	length	= *ilen;
	/*
	 * if cmd is a zero length string, then check to see if shell
	 * command is present.  This should return a nonzero, positive
	 * value.
	 */
	if (arglen == 0) {
		if (errsts = system(NULL) == -1)
			errsts  = errno;
	} else if (length < 0 || length > arglen) {
		errsts	= EINVAL;
        } else {
		/*
		 * If length is zero, strip trailing blanks. Otherwise,
		 * malloc memory and copy the string; adding a NULL
		 * terminator.
		 */
		if (length == 0) {
			pthstr	= _fc_acopy(cmd);
		} else {
			pthstr	= (char *) malloc(length + 1);
		}
		if (pthstr == NULL)	/* If no memory allocated */
			errsts	= ENOMEM;
		else {
			if (length != 0) {	/* Copy argument */
				(void) memcpy(pthstr, argstr, length);
				pthstr[length]	= '\0';
			}
			/*
			 * If cmd is null string, call system(NULL).
			 * to see if shell command is present.  Else,
			 * call system(cmd).
			 */
			if (pthstr[0] == '\0') {
				if (errsts = system(NULL) == -1)
					errsts  = errno;
			} else {
				if (errsts = system(pthstr) == -1)
					errsts  = errno;
			}
			free(pthstr);
		}
	}
	*ierror	= errsts;
	return;
}

#ifndef _UNICOS
/*
 *	PXFSYSTEM       Use system to execute command
 *      Allow on non-UNICOS systems with f90 subroutine interface.
 */
void
pxfsystem_(
        char    *cmd,		/* Character ptr to command */
        _f_int  *ilen,          /* Length of cmd */
        _f_int  *ierror,        /* Error Status */
        int     lenpath)
{
        _PXFSYSTEM( _cptofcd(cmd, lenpath), ilen, ierror);
        return;
}
#endif
