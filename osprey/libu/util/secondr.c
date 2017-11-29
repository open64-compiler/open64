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


#pragma ident "@(#) libu/util/secondr.c	92.1	07/07/99 13:18:33"
#include <fortran.h>
#include <stdio.h>



/*
 *	SECONDR - returns elapsed wall-clock time in seconds.  Returns -1.0 on 
 *		  failure.
 *
 *	Called from Fortran:
 *
 *		REAL SECONDR
 *
 *		x = SECONDR();
 *	or	CALL SECONDR(x)		(UNICOS only; deprecated usage)
 */

#ifdef	_UNICOS

double _sec_per_clock;
extern	int	__hertz;	/* clock cycles per second */

/*
 *	On UNICOS systems, SECONDR returns the value of the real-time clock 
 *	register converted to floating point seconds.  Because the real-time 
 *	clock value may exceed 48 bits, the floating point conversion done 
 * 	here often loses some precision.
 *
 *	The greatest possible error in the value returned by SECONDR is
 *	N seconds, where N is approximated by:
 *
 *		N = (machine clock period time in nanoseconds) * (1.4E-6)
 *
 */

_f_real
SECONDR(_f_real *time)
{
	double	timeval;

	if (_sec_per_clock == 0.0)
		_sec_per_clock = (double)1.0 / (double)__hertz;

	timeval = (double) _rtc() * _sec_per_clock;

	if (_numargs() > 0)
		*time	= (_f_real) timeval;

	return( (_f_real) timeval);
}


#elif __mips

extern long long _sysclock_fast(void);
extern long long _sysclock_nowrap(void);
extern double _fast_cycles_per_sec;
extern long long _fast_cycles_max;
extern double _nowrap_cycles_per_sec;

/*
 *	On MIPS systems, use 64 bit clock if available.  Else use gettimeofday.
 */
_f_real8
secondr_(void)
{
	static double sec_per_cycle;	/* reciprocal so we can multiply */

	if (sec_per_cycle == 0.0) {
		_init_hw_clock();
		sec_per_cycle = 1.0 / _nowrap_cycles_per_sec;
	}

	return (_f_real8)_sysclock_nowrap() * sec_per_cycle;

}

#else	/* Sparc or ABSOFT */

#include <unistd.h>
#include <sys/times.h>

#if defined(BUILD_OS_DARWIN)
extern
#endif /* defined(BUILD_OS_DARWIN) */
double _sec_per_clock;
/*
 *	On Sparc systems, SECONDR uses times() to get wall-clock time.
 */
_f_real
secondr_(void)
{
	clock_t ret;
	struct tms buf;

	ret = times(&buf);
	if (ret == -1)
		return((_f_real) -1.0);

	if (_sec_per_clock == 0.0)
		_sec_per_clock = 1.0 / (double) sysconf(_SC_CLK_TCK);

	return( (_f_real) ((double)ret * _sec_per_clock) );
}

#endif	/* non-UNICOS systems */
