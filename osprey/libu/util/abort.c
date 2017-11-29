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


#pragma ident "@(#) libu/util/abort.c	92.1	07/07/99 13:18:33"

#include <fortran.h>
#include <stdio.h>
#include <stdlib.h>

static int _abortflag = 0;	/* abort flag: set if ABORT has been called */

/*
 *	ABORT	User-callable abort routine.
 *
 *	CALL	ABORT([s])
 *
 *		s	Optional Fortran Character string or NULL-terminated
 *			Hollerith.
 */

void
ABORT(s)
_fcd	s;
{
	char	*msg;
	int	len;

	if (_abortflag != 0)
		abort();		/* prevent infinite recursion */

	_abortflag = 1;

	if (_numargs() > 0) {
#ifdef	_ADDR64
		if (_numargs() * sizeof(long) >= sizeof(_fcd)) {
#else
		if (_isfcd(s)) {	/* If Fortran character */
#endif
			msg	= _fcdtocp(s);
			len	= _fcdlen (s);
		}
		else {
			msg	= *(char **) &s;
			len	= strlen(msg);
		}

		(void) write(fileno(stderr), msg, len);

		(void) write(fileno(stderr), "\n", 1);
	}

#ifdef	_CRAY2
	(void) _tracebk();
#endif
	abort();
}
