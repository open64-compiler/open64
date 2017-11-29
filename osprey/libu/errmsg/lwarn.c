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


#pragma ident "@(#) libu/errmsg/lwarn.c	92.1	06/25/99 17:24:37"
#include <liberrno.h>
#include <errno.h>
#include <stdarg.h>
#include <cray/portdefs.h>

#ifdef KEY /* Bug 7479 */
/*
 * Issue informational "comment" message (imitating the front end) using
 * standard the error-message machinery
 *
 * errn		Error number (see liberrno.h)
 * ...		Optional arguments to be inserted into error message
 */
void
_lcomment(int errn, ...)
{
	va_list		args;		/* Variable argument list	*/
	va_start(args, errn);
	_lmessage(errn, "COMMENT", args);
	va_end(args);
}
#endif /* KEY Bug 7479 */

/*
 *	_lwarn - library run-time warning processor
 *
 *		Returns:
 *			 0  Warning printed
 *			-1  _lwarn() called with no arguments
 *			-2  _lwarn() called with a warning number outside
 *			    the range of library message numbers.
 */

int
_lwarn(int errn, ...)
{
	va_list		args;		/* Variable argument list	*/

#ifdef	_CRAY
	if (_numargs() < 1)
		return(-1);
#endif

	if (((errn < BASE) || (errn > (BASE+999))) &&
	    ((errn < FDC_ERRB) || (errn > (FDC_ERRB+999))))
		return(-2);

	va_start(args, errn);

	_lmessage(errn, "WARNING", args);

	va_end(args);

	return(0);
}

/* Provide duplicate entry point. Used by SCC */
#ifdef	_UNICOS
#pragma _CRI duplicate _lwarn as $lwarn
#endif
