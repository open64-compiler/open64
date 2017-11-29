/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 2000, 2001, Silicon Graphics, Inc.  All Rights Reserved.

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



#pragma ident "@(#) libf/fio/ferror.c	92.2	06/18/99 18:41:40"

#include <liberrno.h>
#include <errno.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include "fio.h"

#ifdef	_CRAYMPP
static DECL_MPP_LOCK(ferrlock)	/* MPP: lock for _ferr() */
#endif

/*
 *	_ferr - Fortran statement run-time error processor
 *
 *	The funny ifdef'ing is used if _ferr is defined as a macro for
 *	debugging purposes.
 */
void
_ferr(FIOSPTR fiosp, int errn, ...)
{
	va_list	args;		/* Variable argument list */

#ifdef	_UNICOS
	if (_numargs() < 2)
		abort();
#endif

	MPP_LOCK(&ferrlock);		/* MPP: allow only 1 PE in _ferr() */

	fflush(stdout);
	fflush(stderr);			/* just in case */

	va_start(args, errn);

	_lmessage(errn, NULL, args);	/* Print error message */

	_fcontext(fiosp);		/* Print error context */

	va_end(args);

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
	/* we don't have atabort() on Irix, but we do want to clean up */
	/* the Fortran units before aborting */
	_fcleanup();
#endif
	abort();
}
