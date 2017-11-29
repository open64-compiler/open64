/*
 * Copyright 2004, 2005 PathScale, Inc.  All Rights Reserved.
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


#pragma ident "@(#) libu/util/second.c	92.1	07/07/99 13:18:33"

#include <fortran.h>

double _sec_per_clock;


/*
 *	SECOND - returns the total elapsed CPU time in seconds of all 
 *	tasks in a multitasked group.
 *
 *	Called from Fortran:
 *
 *		REAL SECOND
 *
 *		x = SECOND();
 *	or	CALL SECOND(x)		UNICOS systems only
 */

#ifdef	_UNICOS

#include <sys/types.h>
#include <sys/times.h>
#ifdef _CRAY1
#include <NEW/mtimes.h>
#else
#include <sys/mtimes.h>
#endif

extern int __hertz;	/* clock ticks per second */
extern int __maxcpu;	/* number of physical CPUs */

#pragma _CRI duplicate SECOND as _second

_f_real
SECOND(_f_real *time)
{
	int		i, conn;
	long		rt, clocksum;
	float		timeval;
	struct mtms	cptimes;
#ifdef _CRAYMPP
	static int	first_time = 0;
	volatile struct mtms mbuf;
#endif
	/*
	 * Initialize '_sec_per_clock' variable if not already done
	 */
	if (_sec_per_clock == 0)
		_sec_per_clock = (double)1.0 / (double)__hertz;

#ifdef _CRAYMPP
	/*
	 * Do mtimes() syscall on first call to SECOND.
	 */
	if (first_time == 0) {
		if (_mtimes(&mbuf) > 0)
			first_time = 1;
		else
			first_time = -1;
	}
	/*
	 * If mtimes() succeeded, do a loop copying the mtimes
	 * buffer into 'cptimes' until we're not interrupted.
	 */
	if (first_time == 1) {
		do {
			rt = _rtc();
			cptimes = mbuf;
		} while (rt < cptimes.mtms_update);
#else
	/*
	 * Use _mtused() to do initial mtimes() call, and
	 * copy current mtimes buffer and RTC into 'cptimes' and 'rt'.
	 */
	if (_mtused(&cptimes, &rt) == 0) {
#endif
		/*
	 	 * Increment the CPU time in the currently active bucket.
	 	 */
		conn = cptimes.mtms_conn;	/* number of CPU's connected */
		cptimes.mtms_mutime[conn-1] += rt - cptimes.mtms_update;
		/*
 		 * The following summation loop assumes for each bucket that 
 		 *
 		 *    __maxcpu * number_of_clocks_per_bucket < 2**64
 		 *
 		 * With __maxcpu <= 64, and nsec_per_clock >= 2.0, this
		 * requires the accumulated CPU time per bucket
		 * to be < 18 years.
 		 */
		clocksum = 0;
		switch(__maxcpu) {
	
		case 8:
			clocksum += cptimes.mtms_mutime[7] * 8
			          + cptimes.mtms_mutime[6] * 7
			          + cptimes.mtms_mutime[5] * 6
			          + cptimes.mtms_mutime[4] * 5;
		case 4:
			clocksum += cptimes.mtms_mutime[3] * 4
			          + cptimes.mtms_mutime[2] * 3;
		case 2:
			clocksum += cptimes.mtms_mutime[1] * 2;
		case 1:
			clocksum += cptimes.mtms_mutime[0];
			break;
	
		default:
			for (i=0; i<__maxcpu; i++)
				clocksum += cptimes.mtms_mutime[i] * (i+1);
			break;
		}
		timeval = (float)clocksum * _sec_per_clock;
	} else {
		/*
		 * mtimes() failed - use times() syscall
		 */
		struct tms tms;

		if (_times(&tms) < 0)
			timeval = (float)-1;
		else
			timeval = (float)tms.tms_utime * _sec_per_clock;
	}

	if (_numargs() > 0)
		*time = timeval;
	return((_f_real)timeval);
}

#elif	__mips

#include <sys/resource.h>
#include <sys/times.h>

_f_real
second_(void)
{
	struct rusage ru;
	getrusage (RUSAGE_SELF, &ru);
	return (_f_real)
	 ((double)ru.ru_utime.tv_sec + ((double)ru.ru_utime.tv_usec * 1.0e-6));
}

#else 	/* non-IRIX, non-UNICOS systems */

#include <unistd.h>
#include <limits.h>
#include <sys/times.h>

#ifdef KEY /* Bug 1683 */

#include "../../libU77/pathf90_libU_intrin.h"

_f_real
pathf90_second(_f_real *seconds)
{
  _f_real junk, tarray[2];
  seconds = (0 == seconds) ? (&junk) : seconds;
  /* "etime" uses getrusage, which generates better results; g77 returns
   * only user time, so we do likewise */
  pathf90_etime(tarray);
  return *seconds = tarray[0];
}

#else

_f_real
second_(void)

{
	struct tms      buf;
	double		timeval;
	clock_t		ret;

	ret = times(&buf);

	if (_sec_per_clock == 0.0)
		_sec_per_clock = 1.0 / (double) sysconf(_SC_CLK_TCK);

	timeval = (double) buf.tms_utime * _sec_per_clock;

	return ((_f_real)timeval);
}
#endif	/* non-UNICOS systems */

#endif /* KEY Bug 1683 */
