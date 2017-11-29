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


#pragma ident "@(#) libu/waio/errwa.c	92.1	06/23/99 13:55:03"

#include <stdio.h>
#include <liberrno.h>
#include "waio.h"

#ifdef	_CRAY2
extern	FILE	*errfile;
#else
#define	errfile	stderr
#endif

/*
 *	_errwa - Process errors in word-addressable package
 */
void
_errwa(routine, text, fp, errn)
char	*routine;	/* Name of routine finding error	*/
char	*text;		/* General text				*/
WAFIL	*fp;		/* WAFIL table entry 			*/
int	errn;		/* System or Fortran error code		*/
{
	if (fp->wa_idn[0] == '\0') 	/* This is a Fortran -s bin file */
		_lerror(_LELVL_ABORT, errn);
	else {
		(void) fprintf (errfile, " \n");
		(void) fprintf (errfile, "%s: %s file %.8s\n", routine, text,
					fp->wa_idn);
		(void) fprintf (errfile, " \n");
		_lerror(_LELVL_ABORT, errn);
	}
}

/*
 *	_errwa_abort	Process errors when we don't have a valid WAFIL table
 * 			entry.
 */
void
_errwa_abort(errn)
{
	_lerror(_LELVL_ABORT, errn);
}

/*
 *	_errwa_msg	Process errors when we don't have a valid WAFIL table
 * 			entry.
 */
void
_errwa_msg(errn)
{
	_lerror(_LELVL_MSG, errn);
}
