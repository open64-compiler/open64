/*
 * Copyright 2004, 2005 PathScale, Inc.  All Rights Reserved.
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



#pragma ident "@(#) libf/fio/fork.c	92.1	06/18/99 19:52:04"

/*
 *	fork_ - forks a copy of this process
 *
 * calling sequence:
 *
 *	INTEGER fork, ierror
 *	ierror = fork()
 * where:
 *
 *	ierror 		= child pid if parent and successful
 *			= 0 if child
 *			= -errno if unsuccessful
 * Entry point fork_ is called from f77 and from f90 when there is no
 *	 compatiblity module.
 *
 * Entry point forkf90_ is called from f90 when there is a
 *	 compatiblity module.
 *
 * Entry point forkf90_8_ is called from f90 when there is a
 *	 compatiblity module.
 */

#include <sys/types.h>
#include <unistd.h>
#include <errno.h>
#include <foreign.h>
#include <liberrno.h>
#include "fio.h"

extern void flush_connected_units (void);	/* From F77 library */
extern int fork_(void);
extern _f_int forkf90_(void);
extern _f_int8 forkf90_8_(void);

void _flushall(void);

#if     defined(_LITTLE_ENDIAN)

int
fork_(void)
{
	forkf90_();
}

#else

int
fork_(void)
{
	/* this should work if f77 -craylibs used or if f90 used. */
	void _flushall(void);

	/* defined in libI77/open.c and called from fork_ .
	 * This should get an error if used from fortran 90
	 * according to libu77/externals.h  but should work
	 * from Fortran77.
	 *
	 */
	flush_connected_units();

	/* fork a copy of this process */
	return( fork() );
}

#endif

_f_int
forkf90_(void)
{
	/* this should work if f77 -craylibs used or if f90 used. */
	_flushall();

	/* fork a copy of this process */
	return( fork() );
}

_f_int8
forkf90_8_(void)
{
	/* this should work if f77 -craylibs used or if f90 used. */
	_flushall();

	/* fork a copy of this process */
	return( fork() );
}

/*
 *  _flushall - flush all connected Fortran units except 100, 101, 102.
 */
void
_flushall(void)
{
	int	ret;
	register short	errflag;
	static short	pass = 0; /* incremented when _flushall is called */
	unit		*uptr;

	if (pass++ >= 1)
		return;

	errflag	= 0; 
/*
 *	Find all open Fortran units not connected by
 *	WOPEN/OPENMS/OPENDR/AQOPEN and flush them.
 */
	uptr	= _get_next_unit(NULL, 0, 0);

	while (uptr != NULL) {     /* while more open units */
#ifdef KEY /* Bug 6433 */
		_f_int4	unum;
#else /* KEY Bug 6433 */
		unum_t	unum;
#endif /* KEY Bug 6433 */
		
		unum	= uptr->uid;
		
		if (OPEN_UPTR(uptr) && uptr->ufs != FS_AUX) {
		        flush_( &unum );
		}
		uptr	= _get_next_unit(uptr, 0, 0);
	}
/*
 *	Flush C files on mips because the C cleanup routine will not
 *	be executed if the code is loaded using the f90 command.  So
 *	Fortran fork_ processing must flush stdout and any user C
 *	files in addition to the Fortran files.
 */	 
	(void) fflush(NULL);
	return;
}
