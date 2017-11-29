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


#pragma ident "@(#) libu/trbk/trbk.c	92.1	06/23/99 18:14:34"
#include <infoblk.h>
#include <signal.h>
#pragma	soft	fflush
#include <stdio.h>
#include <stdlib.h>
#include <cray/signal.h>
#include <cray/mtlock.h>

static DECL_LOCK(traceback_lock)	/* lock word for routine */

extern	int	_trbkdpth;

extern	int	_trbk(int _Fd, ...);

long
TRBK(long *depth)
{
	long	stat;

	MEM_LOCK(&traceback_lock);

	if (_numargs() > 0)	/* If depth specified */
		_trbkdpth	= *depth;
	else {
		char	*env;

        	if (env = getenv("TRBKDPTH"))
                	_trbkdpth	= atoi(env);
	}

	/* Flush all files */

	if (LOADED(fflush))
		(void) fflush(NULL);

	stat	= _trbk(fileno(stderr));

	MEM_UNLOCK(&traceback_lock);

	return(stat);
}

int
sigtrbk(FILE *stream, int depth)
{
	long	stat;
	FILE	*f;

	if (_numargs() > 0)	/* If stream specified */
		f	= stream;
	else
		f	= stderr;

	MEM_LOCK(&traceback_lock);

	if (_numargs() > 1)	/* If depth specified */
		_trbkdpth	= depth;
	else {
		char	*env;

        	if (env = getenv("TRBKDPTH"))
                	_trbkdpth	= atoi(env);
	}

	/* Flush all files */

	if (LOADED(fflush))
		(void) fflush(NULL);

	stat	= _trbk(fileno(f), _sigarea());

	MEM_UNLOCK(&traceback_lock);

	return((int)stat);
}

int
trbk(FILE *stream, int depth)
{
	long	stat;
	FILE	*f;

	if (_numargs() > 0)	/* If stream specified */
		f	= stream;
	else
		f	= stderr;

	MEM_LOCK(&traceback_lock);

	if (_numargs() > 1)	/* If depth specified */
		_trbkdpth	= depth;
	else {
		char	*env;

        	if (env = getenv("TRBKDPTH"))
                	_trbkdpth	= atoi(env);
	}

	/* Flush all files */

	if (LOADED(fflush))
		(void) fflush(NULL);

	stat	= _trbk(fileno(f));

	MEM_UNLOCK(&traceback_lock);

	return((int)stat);
}

long
$TRBK()
{
	long	stat;
	char	*env;

	MEM_LOCK(&traceback_lock);

        if (env = getenv("TRBKDPTH"))
               	_trbkdpth	= atoi(env);

	/* Flush all files */

	if (LOADED(fflush))
		(void) fflush(NULL);

	stat	= _trbk(fileno(stderr));

	MEM_UNLOCK(&traceback_lock);

	return(stat);
}
