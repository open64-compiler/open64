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

#pragma ident "@(#) libu/clib/getcwd.c	92.1	07/01/99 13:42:20"
#include <fortran.h>
#include <memory.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#define	BLANK	((int) ' ')

/*
 *	GETCWD	Returns the current working directory
 *
 *	Call from Fortran:
 *
 *		CHARACTER * (*), CWD
 *			...
 *      	CALL GETCWD(CWD)
 *
 *			- or -
 *
 *		CHARACTER * (*), CWD
 *		INTEGER GETCWD, I
 *			...
 *		I = GETCWD(CWD)
 *
 *	Result:
 *
 *		CWD	Variable of type CHARACTER to receive the current
 *			working directory
 *
 *		I	Return value, type INTEGER, when called as a function
 *
 *			 -2	GETCWD called with no argument or called
 *				with an argument not of type CHARACTER
 *
 *			 -1	getcwd request failed, probably because
 *				the CWD variable was not long enough to
 *				contain the complete pathname (CWD will be
 *				set to all blanks)
 *
 *			 >0	Number of characters in the name of the
 *				current working directory
 *
 *		The CWD character variable will receive the current
 *		working directory, blank filled.  It must be at least
 *		one character larger than the current working directory.
 *
 *		Refer to the getcwd(3C) man page for a description of
 *		the getcwd request.
 */

_f_int
GETCWD (cwd)
_fcd	cwd;				/* Fortran character descriptor */
{
	int	rtrn, size;
	char	*cptr;

	if (_numargs() < 1)
		rtrn	= -2;				/* Invalid call */
	else {

		cptr	= _fcdtocp(cwd);
		size	= _fcdlen (cwd);

		if (getcwd(cptr, size) == NULL) {
			(void) memset(cptr, BLANK, size);
			rtrn	= -1;			/* Request failed */
		}
		else {
			rtrn	= strlen(cptr);
			(void) memset(cptr + rtrn, BLANK, size - rtrn);
		}
	}

	return((long) rtrn);
}

