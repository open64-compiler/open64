/*
 * Copyright 2002, 2003, 2004 PathScale, Inc.  All Rights Reserved.
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


#pragma ident "@(#) libu/util/rtc.c	92.4	10/29/99 21:40:31"



/*
 *	This module contains functions and global variables which give 
 *	access to the MIPS real-time cycle counter:
 */
 		long long _sysclock_fast(void);
 
 		long long _sysclock_nowrap(void);
 
 		double _fast_cycles_per_sec;
 
 		long long _fast_cycles_max;
 
 		double _nowrap_cycles_per_sec;
 
/*	_sysclock_fast returns the 64-bit integer value of the
 *	hardware cycle counter.  The fastest available clock is used.  The
 *	counter might wrap.
 *
 *	_sysclock_nowrap returns the value of the fastest available 64-bit 
 *	clock counter that does not wrap back to zero in a 20 year time
 *	period.
 *
 *	_fast_cycles_per_sec returns the number of clock cycles per second 
 *	for _sysclock_fast().
 *
 *	_fast_cycles_max returns the maximum integer value that the 
 * 	_sysclock_fast timer ever would return.  The value of
 *	_sysclock_fast is 0 at the next tick after _fast_cycles_max is
 *	returned.
 *	
 *	_nowrap_cycles_per_sec returns the number of clock cycles per second 
 *	for _sysclock_nowrap().
 *
 *	On MIPS systems that have a 64 bit cycle counter 
 *	_fast_cycles_per_sec and _nowrap_cycles_per_sec are equal, and they
 *	reflect the clock rate of the hardware cycle counter.
 */

#include <stdio.h>
#include <sys/time.h>
/* In Linux <sys/time.h> this is ifdef'ed out */
#if defined(KEY) && ! defined(BUILD_OS_DARWIN)
struct timezone
  {
    int tz_minuteswest;		/* Minutes west of GMT.  */
    int tz_dsttime;		/* Nonzero if DST is ever in effect.  */
  };
#endif


short _init_hw_clock_called;
long long *_rtc_clockaddr;		/* ptr to 64 bit clock, if any */
int *_rtc_clockaddr32;			/* ptr to 32 bit clock, if any */

#if !defined(__mips) /* Sparc and absoft and others */

#include <limits.h>

/*
 *	_init_hw_clock - initializes the exported clock variables.
 */
void
_init_hw_clock(void)
{
	if (_init_hw_clock_called != 0)
		return;

	_init_hw_clock_called = 1;

	_fast_cycles_per_sec = 1000000;
	_nowrap_cycles_per_sec = 1000000;
	_fast_cycles_max = INT_MAX;
}
#endif	/* !__mips */

#if	defined(__mips)

#include <fcntl.h>
#include <stdlib.h>		/* for atcheckpoint, atrestart */
#include <sys/mman.h>
#include <sys/syssgi.h>

extern int atcheckpoint(void (*func) (void));
#pragma weak atcheckpoint
extern int atrestart(void (*func) (void));
#pragma weak atrestart
extern void _u_sysclock_checkpoint();
extern void _u_sysclock_restart();

/*
 *	_init_hw_clock - initializes the exported clock variables.
 */
void
_init_hw_clock(void)
{
	__psunsigned_t phys_addr, raddr;
	int cycsz;
	int fd;
	int poffmask;
	unsigned int pico_per_clk;
	void *clockaddr;

	if (_init_hw_clock_called != 0)
		return;

	_init_hw_clock_called	= 1;

	/* register event handlers for checkpoint and restart.
	 * These are available at 6.4.
	 */
	if (atcheckpoint && atrestart) {
		if (atcheckpoint(_u_sysclock_checkpoint) < 0) {
			/* checkpoint registration failed,
			 * returns -1, errno = error
			 */
			return;
		}
		if (atrestart(_u_sysclock_restart) < 0) {
			/* restart registration failed,
			 * returns -1, errno = error
			 */
			return;
		}
	}

	/*
 	 * Assume gettimeofday granularity for now. 
	 */
	_fast_cycles_per_sec	= 1000000.; 
	_nowrap_cycles_per_sec	= 1000000.; 

	poffmask	= getpagesize() - 1;
	phys_addr	= syssgi(SGI_QUERY_CYCLECNTR, &pico_per_clk);
	cycsz	= syssgi(SGI_CYCLECNTR_SIZE);
	raddr	= phys_addr & ~poffmask;
	fd	= open("/dev/mmem", O_RDONLY);
	if (fd < 0) {
		/* could not open /dev/mmem !! */
		return;
	}
	clockaddr = mmap(0, poffmask, PROT_READ, MAP_PRIVATE, fd, (off_t)raddr);
	if ((void*)clockaddr == MAP_FAILED) {
		/* no HW clock */
		return;
	}

	clockaddr = (void *)((__psunsigned_t)clockaddr + (phys_addr&poffmask));

	if (cycsz == 64) {
		_rtc_clockaddr	= (long long *)clockaddr;
		_rtc_clockaddr32	= (int *)clockaddr + 1;
		_nowrap_cycles_per_sec	=
			1.0 / ( (double)pico_per_clk * 1.0e-12 );
	}
	else {  /* if (cycsz == 32) */
		_rtc_clockaddr32	= (int *)clockaddr;
	}

	_fast_cycles_per_sec = 1.0 / ( (double)pico_per_clk * 1.0e-12 );
}

#endif	/* __mips */

/*
 * 	_sysclock_fast
 *
 *	If the HW clock exists, use it.  Else use gettimeofday().
 */
long long
_sysclock_fast(void)
{
	if (_init_hw_clock_called == 0)
		_init_hw_clock();

#if	defined(__mips)
	if (_rtc_clockaddr != NULL)
		return *_rtc_clockaddr;
	else if (_rtc_clockaddr32 != NULL)
		return (long long) *_rtc_clockaddr32;
#endif

	return _sysclock_nowrap();
}

/*
 * 	_sysclock_nowrap
 *
 *	If the HW clock is 64 bits, use it.  Else use gettimeofday().
 */
long long
_sysclock_nowrap(void)
{
        static struct timeval  firstcall;
        static struct timezone  firstcallz;
        struct timeval  buf;
        struct timezone buf2;
        long long cval, highbits;

	if (_init_hw_clock_called == 0)
		_init_hw_clock();

#if	defined(__mips)
	if (_rtc_clockaddr != NULL)
		return *_rtc_clockaddr;
#endif

	if (firstcall.tv_usec == 0) 
        	(void) gettimeofday (&firstcall, &firstcallz);

        (void) gettimeofday (&buf, &buf2);
        cval = (long long)(buf.tv_sec - firstcall.tv_sec) * 1000000LL
		+ (long long)(buf.tv_usec - firstcall.tv_usec);

	return cval;
}

#if	defined(__mips)
void
_u_sysclock_checkpoint()
{
	__psunsigned_t vbase;
	int mask;
	int page;

	if (!_init_hw_clock_called)
		return;
	if (_rtc_clockaddr != NULL) {
		page	= getpagesize();
		mask	= page - 1;
		vbase	= (__psunsigned_t)_rtc_clockaddr & ~mask;
		munmap((void *)vbase, page);
	}
	return;
}

void
_u_sysclock_restart()
{
	__psunsigned_t vbase,  raddr, phys_addr;
	unsigned int pico_per_clk;
	int cycsz;
	int fd;
	int page;
	int poffmask;
	if (!_init_hw_clock_called)
		return;
	if (_rtc_clockaddr != NULL) {
		page	= getpagesize();
		poffmask = page - 1;
		phys_addr = syssgi(SGI_QUERY_CYCLECNTR, &pico_per_clk);
		cycsz	= syssgi(SGI_CYCLECNTR_SIZE);
		raddr	= phys_addr & ~poffmask;
		fd	= open("/dev/mmem", O_RDONLY);
		vbase	= (__psunsigned_t)_rtc_clockaddr & ~poffmask;
		mmap((void *)vbase, page, PROT_READ,
			MAP_PRIVATE | MAP_FIXED, fd, (off_t) raddr);
	}
	return;
}
#endif
