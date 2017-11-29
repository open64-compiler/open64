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


#pragma ident "@(#) libfi/subrtn/cpu_time.c	92.1	07/13/99 10:47:32"

#include <fortran.h>
#include <sys/times.h>
#include <sys/types.h>
#include <unistd.h>
#include <cray/portdefs.h>	/* limits.h is included in portdefs.h */
#ifdef KEY /* Bug 1683 */
/* We want to use "getrusage", which gives better info than "times" */
#define __mips
#endif /* KEY Bug 1683 */
#ifdef	__mips
#include <sys/resource.h>
#endif

#ifndef	__mips
extern _f_real8	_sec_per_clock;
#endif	/* NOT __mips */

#ifdef _CRAY
#include <sys/mtimes.h>
extern	int __hertz;	/* clock ticks per second */
extern	int __maxcpu;	/* number of physical CPUs */
#endif	/* _CRAY */

#ifdef	__mips
/* For mips, getrusage returns user time used by the current process,
 * i.e., total amount spent executing in user mode.  Note that kind=16
 * has larger precision than default(0), 4, or 8 which assume 8.
 */
#define __CPUTME()							\
	struct rusage	ru;						\
	getrusage (RUSAGE_SELF, &ru);					\
	switch(kindtype) {						\
	case 16:							\
		timval	= ((_f_real16)ru.ru_utime.tv_sec + ((_f_real16)ru.ru_utime.tv_usec * 1.0e-6));	\
		break;							\
	default:							\
		timval	= ((_f_real8)ru.ru_utime.tv_sec + ((_f_real8)ru.ru_utime.tv_usec * 1.0e-6));	\
		break;							\
	}

#elif	_CRAY	/* __mips */
#ifdef	_CRAY1
/* For UNICOS non-MPP, use _mtused() to do initial mtimes() call, and copy
 * current mtimes buffer and RTC into 'cptimes' and 'rt'.  Update the
 * conn, i.e., increment the CPU time in the currently active bucket.
 *
 * The summation loop (switch statement) assumes for each bucket that
 * __maxcpu * number_of_clocks_per_bucket < 2**64.  With  __maxcpu <= 64,
 * and nsec_per_clock >= 2.0, the accumulated CPU time per bucket must
 * be < 18 years.
 */
#define __CPUTME()							\
	_f_int		i, conn;					\
	_f_int8		rt, clocksum;					\
	struct mtms	cptimes;					\
	if (_sec_per_clock == 0.0)					\
		_sec_per_clock = 1.0 / (_f_real8)__hertz;		\
	if (_mtused(&cptimes, &rt) == 0) {				\
		conn	= cptimes.mtms_conn;				\
		cptimes.mtms_mutime[conn-1] += rt - cptimes.mtms_update; \
		clocksum	= 0;					\
		switch(__maxcpu) {					\
		case 8:							\
		  clocksum += cptimes.mtms_mutime[7] * 8 + cptimes.mtms_mutime[6] * 7 + cptimes.mtms_mutime[5] * 6 + cptimes.mtms_mutime[4] * 5; \
		case 4:							\
		  clocksum += cptimes.mtms_mutime[3] * 4 + cptimes.mtms_mutime[2] * 3; \
		case 2:							\
			clocksum += cptimes.mtms_mutime[1] * 2;		\
		case 1:							\
			clocksum += cptimes.mtms_mutime[0];		\
			break;						\
		default:						\
			for (i=0; i<__maxcpu; i++)			\
				clocksum += cptimes.mtms_mutime[i] * (i+1); \
			break;						\
		}							\
		timval = ((_f_real16)clocksum * _sec_per_clock);	\
	} else {							\
		struct tms tms;						\
		if (_times(&tms) < 0)					\
			timval	= -1.0;					\
		else							\
			timval	= ((_f_real16)tms.tms_utime * _sec_per_clock); \
	}

#elif	_CRAYMPP	/* _CRAY1 */
/* For mpp, do mtimes() syscall on first call to CPU_TIME subroutine.  If
 * mtimes() succeeded, do a loop copying the mtimes buffer into cptimes
 * until this is not interrupted.  Update the conn, i.e., increment the
 * CPU time in the currently active bucket.
 *
 * The summation loop (switch statement) assumes for each bucket that
 * __maxcpu * number_of_clocks_per_bucket < 2**64.  With  __maxcpu <= 64,
 * and nsec_per_clock >= 2.0, the accumulated CPU time per bucket must
 * be < 18 years.
 */
#define __CPUTME()							\
	_f_int		i, conn;					\
	_f_int8		rt, clocksum;					\
	struct mtms	cptimes;					\
	static int	first_time	= 0;				\
	volatile struct	mtms mbuf;					\
	if (_sec_per_clock == 0)					\
		_sec_per_clock = (_f_real8)1.0 / (_f_real8)__hertz;	\
	if (first_time == 0) {						\
		if (_mtimes(&mbuf) > 0)					\
			first_time	= 1;				\
		else							\
			first_time	= -1;				\
	}								\
	if (first_time == 1) {						\
		do {							\
			rt	= _rtc();				\
			cptimes	= mbuf;					\
		} while (rt < cptimes.mtms_update);			\
		conn	= cptimes.mtms_conn;				\
		cptimes.mtms_mutime[conn-1] += rt - cptimes.mtms_update; \
		clocksum	= 0;					\
		switch(__maxcpu) {					\
		case 8:							\
		  clocksum += cptimes.mtms_mutime[7] * 8 + cptimes.mtms_mutime[6] * 7 + cptimes.mtms_mutime[5] * 6 + cptimes.mtms_mutime[4] * 5; \
		case 4:							\
		  clocksum += cptimes.mtms_mutime[3] * 4 + cptimes.mtms_mutime[2] * 3; \
		case 2:							\
			clocksum += cptimes.mtms_mutime[1] * 2;		\
		case 1:							\
			clocksum += cptimes.mtms_mutime[0];		\
			break;						\
		default:						\
			for (i=0; i<__maxcpu; i++)			\
				clocksum += cptimes.mtms_mutime[i] * (i+1); \
			break;						\
		}							\
		timval = ((_f_real8)clocksum * _sec_per_clock); 	\
	} else {							\
		struct tms tms;						\
		if (_times(&tms) < 0)					\
			timval	= (_f_real8)-1;				\
		else							\
			timval	= ((_f_real8)tms.tms_utime * _sec_per_clock); \
	}

#endif	/* _CRAY1 */
#else	/* NOT __mips and NOT CRAY */
/* For non-mips and non-CRAY, times returns the the CPU time used while
 * executing instructions in the user space of the calling process.
 */
#define __CPUTME()							\
	struct tms	buf;						\
	clock_t		ret;						\
	ret	= times(&buf);						\
	if (_sec_per_clock == 0.0)					\
		_sec_per_clock = 1.0 / (_f_real8) sysconf(_SC_CLK_TCK); \
	switch(kindtype) {						\
	case 16:							\
	   timval = ((_f_real16) buf.tms_utime * _sec_per_clock);	\
	   break;							\
	default:							\
	   timval = ((_f_real8) buf.tms_utime * _sec_per_clock);	\
	   break;							\
	}

#endif

/*
 * Fortran 95, Section 13.14.25 CPU_TIME(TIME) returns the processor time
 *
 * CALL CPU_TIME( real_arg )
 *
 * where real_arg is scalar and type real, an INTENT(OUT) argument.  It
 * is assigned a processor-dependent approximation to processor time in
 * seconds.  If a meaningful time cannot be given, a processor-dependent
 * negative value is returned.
 *
 * The exact definition of time is left imprecise since different
 * processors provide time in a wide variety of implementations. CPU_TIME
 * is used to time sections of code.  Time may or may not include system
 * overhead and has no obvious connection to elapsed wall clock time.
 *
 * CPU_TIME returns a -1 if it cannot return a meaningful time.  It
 * returns user time on UNICOS and UNICOS/mk systems, IRIX, and non-IRIX
 * systems.
 *
 * On non-IRIX systems, call CPU_TIME() at least once before the actual
 * timing loop to initialize the time structure.
 *
 * On UNICOS and UNICOS/mk systems, CPU_TIME includes time accumulated by
 * all processors in a multitasking program.
 */
void
_CPU_TIME(_f_real *time)
{
	int		kindtype = 0;	/* default kind type */
	_f_real		timval;
	__CPUTME();
	*time	= timval;
	return;
}

void
_CPU_TIME_4(_f_real4 *time)
{
	int		kindtype = 4;	/* kind type = 4 */
	_f_real4	timval;
	__CPUTME();
	*time	= timval;
	return;
}

void
_CPU_TIME_8(_f_real8 *time)
{
	int		kindtype = 8;	/* kind type = 8 */
	_f_real8	timval;
	__CPUTME();
	*time	= timval;
	return;
}

#if defined _F_REAL16 && _F_REAL16 != (-1)
void
_CPU_TIME_16(_f_real16 *time)
{
	int		kindtype = 16;	/* kind type = 16 */
	_f_real16	timval;
	__CPUTME();
	*time	= timval;
	return;
}
#endif	/* _REAL16 ... */
