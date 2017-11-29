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


#pragma ident "@(#) libu/errmsg/lerror.c	92.1	06/25/99 17:24:37"

#ifdef	_UNICOS
#include <infoblk.h>
#endif
#include <liberrno.h>
#include <errno.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
  
#ifdef	_CRAY2
#pragma soft errfile
extern FILE *errfile;
#endif
  
/*
 *	_lerror - library run-time error processor.  This routine
 *		  acts upon an error status as requested in the 'hndlcode'
 *		  argument.  The actions are as follows:
 *
 *		  MODE 		ACTION
 *
 *		  _LELVL_ABORT	Print the error message and then abort with
 *				traceback.  On the YMP, a traceback will be
 *				suppressed if _repriev is not loaded.
 *		  _LELVL_EXIT	Print the error message and then call exit with
 *				a nonzero status. This mode is used by commands.
 *		  _LELVL_MSG	Print the error message and then return.
 *		  _LELVL_RETURN	Return without printing anything.
 */
  
void
_lerror(int hndlcode, int errn, ...)
{
	va_list	args;		/* Variable argument list */
  
#ifdef	_CRAY
	if (_numargs() < 1)
		(void) abort();
#endif
	if (hndlcode >= _LELVL_MSG && errn != 0) {
		va_start(args, errn);
		_lmessage(errn, NULL, args);	/* Print error message */
		va_end(args);
	}

	if (hndlcode >= _LELVL_ABORT) {
		(void) abort();	      /* abort causes traceback on CX/CEA */
	}

	if (hndlcode >= _LELVL_EXIT)
		(void) exit(1);
}
