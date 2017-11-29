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


#pragma ident "@(#) libu/util/tsecnd.c	92.1	07/07/99 13:18:33"
#include <fortran.h>

/*
 *	TSECND returns the elapsed CPU time in floating point 
 *	seconds for the calling task.
 *
 *	Called from Fortran:
 *
 *		REAL TSECND
 *
 *		x = TSECND()	
 *	or	CALL TSECND(x)			UNICOS systems only
 */

#ifdef	_UNICOS

extern	int	__hertz;	/* _rtc() hertz rate */

_f_real
TSECND(_f_real *time)
{
	double	timeval;
	long	_tskclks();	/* returns clocks for current task */

	timeval	= (double) _tskclks() / (double)__hertz;

	if (_numargs() > 0)
		*time	= (_f_real) timeval;

	return( (_f_real) timeval);
}

#elif	_SOLARIS

#include <unistd.h>
#include <sys/time.h>
#include <sys/times.h>

/*
 *	On Solaris systems, TSECND uses gethrvtime() to get light-weight
 *	process (LWP) CPU time.  This is valid because every multitasking task 
 *	is mapped to exactly one thread which is bound to an LWP.
 *
 *	1/23/94 kaf - this routine not yet activated for Solaris because
 *	gethrvtime() seems to be returning invalid numbers for non-multitasked
 *	programs.
 */
_f_real
tsecnd_(void)
{
	hrtime_t ret;
	ret = gethrvtime();
	return( (_f_real) ((double)ret * 10e-9) ); /* nanoseconds to seconds */
}

#endif	/* _SOLARIS */
