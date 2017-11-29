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



#ifndef STOPALL
#pragma ident "@(#) libf/fort/stop.c	92.1	06/24/99 10:18:36"
#endif

#include <sys/types.h>
#include <signal.h>
#include <errno.h>
#include <fortran.h>
#include <stdio.h>
#include <time.h>
#include <unistd.h>
#include <sys/param.h>
#include <stdlib.h>
#ifdef	_UNICOS
#include <sys/category.h>
#endif
#include "fio.h"

#define MAX_STP_LEN	80	/* Maximum STOP message length		*/
#define	MAX_ENT_LEN	32	/* Maximum entry point name length	*/

#ifdef STOPALL
#define STOPMSG	"STOP_ALL"
#else
#define STOPMSG "STOP"
#endif

extern	long	_cptimes;
extern	int	_who_called_me();
extern	int	_print_statistics;

/*
 *	$STOP	CF77 entry point to process Fortran STOP statement
 *
 *	_STOP	F90 entry point to process Fortran STOP statement
 *
 *	Argument
 *
 *		s	Fortran character string for the optional "stop code".
 *			Note that Fortran accepts a digit string or a 
 *			character string for the stop code.  The compiler
 *			converts a digit string (integer) to a character
 *			string before calling _STOP/$STOP.
 *
 *			CF77 5.0 and later passes an FCD with 0 length
 *			if the user omitted the stop code.
 *
 *			F90 passes an FCD for ' ' if the user omitted the
 *			stop code.
 */

#ifdef STOPALL
static			/* don't want _STOP defined in STOP_ALL */
#elif	defined(_CRAY)
#pragma _CRI duplicate _STOP as $STOP
#endif
void
_STOP(s)
_fcd	s;
{
	int	len, lineno;
	char	*buff, *msg;
	char	name[MAX_ENT_LEN];
	char	buffer[MAX_STP_LEN + MAX_ENT_LEN + 100];

	/* Close files first so CPU and elapsed times are more accurate */

#ifdef	_CRAY
	len	= _who_called_me(&lineno, name, MAX_ENT_LEN, 1);

	if (len < 0) {		/* If there was an error */
		name[0]	= '?';
		name[1]	= '?';
		name[2]	= '?';
		len	= 3;
	}

	name[len]	= '\0';		/* Terminate name */

	if (_numargs() == 0) {
		msg	= "";
		len	= 0;
	}
	else 
#endif
	{
		msg	= _fcdtocp(s);
		len	= _fcdlen (s);

		if (len > MAX_STP_LEN)
			len	= MAX_STP_LEN;
	}

	/*
	 * Print a one- or two-line message based on the length of the
	 * optional user string.  We (somewhat) arbitrarily go to two
	 * lines if the optional user string is longer than 5 characters
	 * (the Fortran 77 standard allows only 1-5 digits on the STOP
	 * statement).  Note that the routine name can be up to 31
	 * characters long.
	 */

#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
	buff	= buffer + sprintf(buffer, " %s", STOPMSG);

#ifdef	_CRAYMPP
	buff	= buff + sprintf(buff, " (PE %d)", _my_pe());
#endif

	if (len > 0) {

		buff	= buff + sprintf(buff, " %.*s", len, msg);

		if (len > 5)
			buff	= buff + sprintf(buff, "\n %s", STOPMSG);
	}

#ifdef	_CRAY
	buff	= buff + sprintf(buff,
			" executed at line %d in Fortran routine '%s'",
			lineno, name);
#endif
	buff	= buff + sprintf(buff, "\n");
	fflush(NULL);	/* it's nice if STOP output follows all user output */

	(void) write(fileno(errfile), buffer, buff - buffer);

	/* Turn off the lights, let the cat out, and go to bed */

#if	defined(_CRAY1) 
	if (_cptimes)
		_print_statistics = 1;	/* trigger statistics in _fcleanup */
#endif

#if defined(_CRAYT3D) || defined(STOPALL)
/*
 * If we're in a master region (or if calling STOP_ALL),
 * send the SIGBUFIO signal to all PEs, causing them to
 * clean up and exit.  We need to use different killm()
 * categories under UNICOS-MAX and mk.
 */
	if (_num_pes() > 1) {
#ifndef STOPALL
		if (_in_parallel() == 0 && _in_doshared() == 0)
#endif
#ifdef _UNICOS_MAX
			killm(C_PROC, 0, SIGBUFIO);
#else
			killm(C_APTEAM, 0, SIGBUFIO);
#endif
	}
#endif
	exit(0);
}
#else	/* Not __mips */
	/* PRINT output only if literal present */
	if (len > 0) {
		/* len is empty for a null string */
		buff	= buffer + sprintf(buffer, " %s", STOPMSG);
		buff	= buff + sprintf(buff, " %.*s", len, msg);
		if (len > 5)
			buff	= buff + sprintf(buff, "\n %s", STOPMSG);
		buff	= buff + sprintf(buff, "\n");
		/* STOP output follows all user output */
		fflush(NULL);
		(void) write(fileno(errfile), buffer, buff - buffer);
	}
	/* Sometimes, if we've been using async i/o, the atexit routine */
	/* doesn't get executed (pv 479334). */
	/*  So call _fcleanup here for safety. */
	_fcleanup();
	exit(0);
}
#endif	/* Not __mips */
