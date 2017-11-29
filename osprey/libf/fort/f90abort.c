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



#pragma ident "@(#) libf/fort/f90abort.c	92.2	10/06/99 12:11:07"

#include <fortran.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "defalias.h"

static int __abortflag = 0;	/* abort flag set if ABORT has been called */

/*	ABORT - User-callable abort routine
 *
 *	CALL ABORT([s])
 *
 *	where:
 *		s	Optional Fortran Character String
 *
 *	This equates to the following calling sequence:
 *
 *	call abort_(char *msg, int *len)
 *
 *	where:
 *		char* msg	Character pointer to string, Null if
 *				not present.
 *		int* len	Pointer to the length of the string.
 */

#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
void
abort_(char *msg, _f_int len)
{
	if(__abortflag !=0)
		abort();		/* prevent infinite recursion */
	__abortflag = 1;
	if (msg != NULL) {
		(void)write(fileno(stderr), msg, len);
		(void)write(fileno(stderr), "\n", 1);
	}
	abort();
}

#else
void
abort_msg__(char *msg, _f_int len)
{
	if(__abortflag !=0)
		abort();		/* prevent infinite recursion */
	__abortflag = 1;
	if (msg != NULL) {
		(void)write(fileno(stderr), msg, len);
		(void)write(fileno(stderr), "\n", 1);
	}
	abort();
}

#if defined(BUILD_OS_DARWIN)
/* Mach-O doesn't support aliases */
void abort_msg_(char *msg, _f_int len) {
  abort_msg__(msg, len);
}
#else /* defined(BUILD_OS_DARWIN) */
defalias(abort_msg__, abort_msg_);
#endif /* defined(BUILD_OS_DARWIN) */

void
abort_(void)
{
	abort();
}
#endif
