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


#pragma ident "@(#) libu/ffio/ffstrerror.c	92.3	10/11/99 15:30:43"

#include <liberrno.h>
#include <nl_types.h>
#include <string.h>
#include <stdio.h>
#if	defined(_LITTLE_ENDIAN)
#include <cray/nlcatmsg.h>
#endif

char	__dfltmsg[32];	/* Default message */

/*
 *	ffstrerror
 *
 *	char *ffstrerror(int);
 *
 *	Returns a pointer to an error message string associated with the
 *	specified error number (returned previously via an ff* routine),
 *	suitable for printing, etc.  The message string should not be
 *	overwritten.
 */

char *ffstrerror(int errnum)
{
	char	*msgptr;	/* Pointer to error message string */

	(void) sprintf(__dfltmsg, "Unknown error %d", errnum);

	if (errnum < BASE) {	/* If not a library error message */
		msgptr	= strerror(errnum);

		if (msgptr == (char *) NULL)
			msgptr	= __dfltmsg;
	}
	else {
		nl_catd	mcfd;		/* Message catalog file descriptor */

		mcfd	= catopen(FEMCN, 0);
		msgptr	= catgets(mcfd, NL_MSGSET, errnum, __dfltmsg);
	}

	return(msgptr);
}
