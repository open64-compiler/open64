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

#pragma ident "@(#) libu/clib/gethost.c	92.1	07/01/99 13:42:20"
#include <fortran.h>
#include <memory.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#define	BLANK	((int) ' ')

/*
 *	GETHOST	Returns the name of the host mainframe
 *
 *	Call from Fortran:
 *
 *		CHARACTER * (*), HOST
 *			...
 *      	CALL GETHOST(HOST)
 *
 *			- or -
 *
 *		CHARACTER * (*), HOST
 *		INTEGER GETHOST, I
 *			...
 *		I = GETHOST(HOST)
 *
 *	Result:
 *
 *		HOST	Variable of type CHARACTER to receive the host
 *			name
 *
 *		I	Return value, type INTEGER, when called as a function
 *
 *			 -2	GETHOST called with no argument or called
 *				with an argument not of type CHARACTER
 *
 *			 -1	gethostname(3W) system request failed.  The
 *				HOST variable will be set to all blanks.
 *
 *			 >0	Number of characters in the host name.
 *
 *		The HOST character variable will receive the name of the
 *		current host, blank filled.  It should be at least as large
 *		as the host name.
 *
 *		Refer to the gethostname(3W) man page for a description of
 *		the gethostname(3W) request.
 */

_f_int
GETHOST (host)
_fcd	host;				/* Fortran character descriptor */
{
	int	rtrn, size;
	char	*cptr;

	if (_numargs() != sizeof(_fcd)/sizeof(long))
		rtrn	= -2;				/* Invalid call */
	else {

		cptr		= _fcdtocp(host);
		size		= _fcdlen(host);
		cptr[size-1]	= '\0';

		if (gethostname(cptr, size) == -1) {
			(void) memset(cptr, BLANK, size);
			rtrn	= -1;			/* Request failed */
		}
		else
			if (cptr[size-1] == '\0') {
				rtrn	= strlen(cptr);
				(void) memset(cptr + rtrn, BLANK, size - rtrn);
			}
			else
				rtrn	= size;
	}

	return( (_f_int) rtrn);
}
