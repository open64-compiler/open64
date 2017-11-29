/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
 */

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


#pragma ident "@(#) libf/pxf/pxfcreat.c	92.1	06/29/99 11:36:06"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <fortran.h>
#include <liberrno.h>
#include <malloc.h>
#include <string.h>

#include <unistd.h>

/*
 *	PXFCREAT 	PXF Interface to the creat(2) system call
 *	         	to create a new file or rewrite an existing
 *	         	file.
 *
 *	Call from Fortran:
 *
 *		SUBROUTINE PXFCREAT (PATH, ILEN, IMODE, IFILDES, IERROR)
 *		CHARACTER * (*) PATH
 *		INTEGER ILEN, IMODE, IFILDES, IERROR
 *
 *	Where:
 *
 *	PATH	is an input character variable or array element
 *		containing the name of a file.
 *
 *	ILEN	is an input integer variable containing the length
 *		of PATH in characters.  If ILEN is zero, all trailing
 *		blanks are removed before calling creat().
 *
 *	IMODE	is an input integer variable containing the integer
 *		value of the symbolic constant for one or more of the
 *		following file modes:  
 *
 *		USER:
 *		   READ permissions bit: S_IRUSR
 *		   WRITE permissions bit: S_IWUSR
 *		   SEARCH/EXECUTE permissions bit: S_IXUSR
 *		   Inclusive OR of READ/WRITE/EXECUTE: S_IRWXU
 *
 *		GROUP:
 *		   READ permissions bit: S_IRGRP
 *		   WRITE permissions bit: S_IWGRP
 *		   SEARCH/EXECUTE permissions bit: S_IXGRP
 *		   Inclusive OR of READ/WRITE/EXECUTE: S_IRWXG
 *
 *		OTHER:
 *		   READ permissions bit: S_IROTH
 *		   WRITE permissions bit: S_IWOTH
 *		   SEARCH/EXECUTE permissions bit: S_IXOTH
 *		   Inclusive OR of READ/WRITE/EXECUTE: S_IRWXO
 *
 *		SETID:
 *		   Set user ID on execution: S_ISUID
 *		   Set group ID on execution: S_ISGID
 *
 *		An integer value for each of these symbolic constants
 *		is retrieved through the use of PXFCONST or IPXFCONST.
 *		procedures.  The integer values may be combined through
 *		the use of a bitwise inclusive OR function.
 *
 *      IFILDES is an output integer variable containing the file
 *              descriptor returned by creat().
 *
 *	IERROR	is an output integer variable that will contain the
 *		status:
 *
 *		  Zero    - PXFCREAT is successful, i.e., the
 *		            file is created or written
 *
 *		  Nonzero - PXFCREAT is not successful.
 *
 *		In addition to the error statuses returned by the
 *		creat(2) system call, PXFCREAT may return one of
 *		the following error statuses:
 *
 *		EINVAL	If ILEN < 0 or ILEN > LEN(PATH)
 *
 *		ENOMEM	If PXFCREAT is unable to obtain memory to
 *			copy PATH.
 */

#ifdef _UNICOS
void
PXFCREAT(
#else
void
_PXFCREAT(
#endif
	_fcd	PATH,		/* Character variable containing argument */
	_f_int	*ILEN,		/* Significant length of character argument */
	_f_int	*IMODE,		/* bitwise inclusive OR of file modes */
	_f_int	*IFILDES,	/* integer containing file descriptor */
	_f_int	*IERROR		/* Error status */
)
{
	int	arglen, errsts, length;
	char	*argstr, *pthstr;

	errsts	= 0;
	argstr	= _fcdtocp(PATH);
	arglen	= _fcdlen (PATH);
	length	= *ILEN;
	*IFILDES = -1;

	if (length < 0 || length > arglen)
		errsts	= EINVAL;
	else {

		/*
		 * If length is zero, user wants trailing blanks stripped.
		 * Otherwise, malloc memory and copy the string; adding a
		 * NULL terminator.
		 */

		if (length == 0)
			pthstr	= _fc_acopy(PATH);
		else
			pthstr	= (char *) malloc(length + 1);

		if (pthstr == NULL)	/* If no memory allocated */
			errsts	= ENOMEM;
		else {

			if (length != 0) {	/* Copy argument */
				(void) memcpy(pthstr, argstr, length);
				pthstr[length]	= '\0';
			}

			/* Create or rewrite file with creat() */

			if ((*IFILDES = creat(pthstr,*IMODE)) == -1)
				errsts	= errno;

			free(pthstr);
		}
	}

	*IERROR	= errsts;

	return;
}


#ifndef _UNICOS
void
pxfcreat_(
	char	*PATH,		/* Character variable containing argument */
	_f_int	*ILEN,		/* Significant length of character argument */
	_f_int	*IMODE,		/* bitwise inclusive OR of file modes */
	_f_int	*IFILDES,	/* integer containing file descriptor */
	_f_int	*IERROR,
	_f_int  pathlen
)
{
  _PXFCREAT(_cptofcd(PATH,pathlen), ILEN, IMODE, IFILDES, IERROR);
}
#endif



