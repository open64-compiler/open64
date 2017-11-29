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


#pragma ident "@(#) libf/pxf/pxfrmdir.c	92.1	06/29/99 11:36:06"

#include <errno.h>
#include <fortran.h>
#include <liberrno.h>
#include <malloc.h>
#include <string.h>
#include <unistd.h>

#ifndef _UNICOS
#include <stddef.h>
#endif

extern char *_fc_acopy(_fcd f);

/*
 *	PXFRMDIR	Remove a directory entry
 *
 *	Call from Fortran:
 *
 *		SUBROUTINE PXFRMDIR (PATH, ILEN, IERROR)
 *		CHARACTER * (*) PATH
 *		INTEGER ILEN, IERROR
 *
 *	Where:
 *
 *	PATH	is an input character variable or array element
 *		containing the name of the directory entry to be
 *		removed.
 *
 *	ILEN	is an input integer variable containing the length
 *		of PATH in characters.  If ILEN is zero, any and
 *		all trailing blanks are removed.
 *
 *	IERROR	is an output integer variable that will contain the
 *		status:  Zero if PXFRMDIR is successful; otherwise
 *		nonzero.
 *
 *		In addition to the error statuses returned by the
 *		rmdir(2) system call, PXFRMDIR may return the
 *		following error statuses:
 *
 *		EINVAL	If ILEN < 0 or ILEN > LEN(PATH)
 *
 *		ENOMEM	If PXFRMDIR is unable to obtain memory to
 *			copy PATH.
 */

#ifdef _UNICOS
void
PXFRMDIR(
#else
void
_PXFRMDIR(
#endif
	_fcd	PATH,		/* Character variable containing argument */
	_f_int	*ILEN,		/* Significant length of argument */
	_f_int	*IERROR		/* Error status */
)
{
	int	arglen, errsts, length;
	char	*argstr, *pthstr;

	errsts	= 0;
	argstr	= _fcdtocp(PATH);
	arglen	= _fcdlen (PATH);
	length	= *ILEN;

	if (length < 0 || length > arglen)
		errsts	= EINVAL;
	else {

		/*
		 * If length is zero, user wants trailing blanks stripped.
		 * Otherwise, malloc memory and copy the string; adding a
		 * NULL terminator.
		 */

		if (length == 0) {

			pthstr	= _fc_acopy(PATH);

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

			/* Unlink the file */

			if (rmdir(pthstr) == -1)
				errsts	= errno;

			free(pthstr);
		}
	}

	*IERROR	= errsts;

	return;
}

#ifndef _UNICOS
/*
 *      Allow on SPARC systems with its f77 subroutine interface
 */
void
pxfrmdir_(
        char    *PATH,          /* Character ptr to dir path */
        _f_int  *ILEN,          /* Length of pathname */
        _f_int  *IERROR,        /* Error Status */
        int     lenpath)
{
        _PXFRMDIR( _cptofcd(PATH, lenpath), ILEN, IERROR);
        return;
}

#endif
