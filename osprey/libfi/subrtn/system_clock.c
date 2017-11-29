/*
 * Copyright 2002, 2003, 2004, 2005 PathScale, Inc.  All Rights Reserved.
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


#pragma ident "@(#) libfi/subrtn/system_clock.c	92.2	07/13/99 10:47:32"
#include <fortran.h>
#include <cray/portdefs.h>	/* limits.h is included in portdefs.h */
#include <assert.h>
#include <limits.h>


#ifdef	_UNICOS

#include <time.h>

extern	int	__hertz;		/* Clocks per second	*/

/*
 *	SYSCLOCK is the Fortran user entry point.
 */
void
SYSCLOCK(_f_int *val, _f_int *wrap)	
{
	assert( sizeof(int) == sizeof(_f_int) );
	*val = _rtc();
	*wrap = 0;
}

void
_SYSTEM_CLOCK (_f_int *c, _f_int *r, _f_int *m)
{

/*	If count argument present, return the value of real-time clock */

	if (c != NULL)
	    *c = _rtc ();

/*
 *	If count_rate argument present, return number of clocks per second.
 *	Use the global variable __hertz, which contains the number of clocks
 *	per second.
 */

	if (r != NULL)
	    *r = __hertz;

/*
 *	If count_max argument present, return maximum value clock can hold.
 *	This would be the largest integer the word can contain.
 */

	if (m != NULL)
	    *m = LONG_MAX;		/* 64 bit signed integer */
}

#endif

#if defined(__mips) || defined(_WORD32)

#include <sys/time.h>
#include <stddef.h>

#ifndef	__mips

/*
 *	_sysclock is the CRI internal entry point on non-UNICOS systems
 *	used by ATExpert code instrumentation.
 */
void
_sysclock(int *val, int *wrap)
{
	struct timeval	buf;
	struct timezone	buf2;
	long long cval;

	(void) gettimeofday (&buf, &buf2);

	/* cast long buf.tv_usec to unsigned long long to avoid sign
	 * extension which would cause the total to be less than
	 * tv_sec * 1000000.
	 */
	cval = (long long)buf.tv_sec * 1000000LL + (uint64)buf.tv_usec;

	*wrap = (int)(cval >> 31);
	*val  = (int)(cval & 0x7fffffffLL);
}

/*
 *	sysclock_ is the Fortran user entry point.
 */
void
sysclock_(_f_int *val, _f_int *wrap)	
{
	assert( sizeof(int) == sizeof(_f_int) );
	_sysclock(val, wrap);
}

#endif	/* ! __mips */


/*
 * 	Externs from util/rtc.c
 */
extern long long _sysclock_fast(void);
extern double _fast_cycles_per_sec;
extern long long _fast_cycles_max;


void
_SYSTEM_CLOCK (_f_int *c, _f_int *r, _f_int *m)
{

/*	If count argument present, return the value of real-time clock */

	if (c != NULL) 
	    *c = (int)_sysclock_fast() & INT_MAX;	/* trim to 31 bits */

/*
 *	If count_rate argument present, return number of clocks per second.
 */

	if (r != NULL)
	    *r = _fast_cycles_per_sec;

/*
 *	If count_max argument is present, return maximim value clock can hold.
 *	This would be the largest integer the word can contain.
 */

	if (m != NULL)
	    *m = INT_MAX;
}


#endif	/* _WORD32 or __mips */


#ifdef	_F_INT4
#ifdef	_UNICOS

/*
 *	SYSCLOCK is the Fortran user entry point.
 */
void
SYSCLOCK_4(_f_int4 *val, _f_int4 *wrap)	
{
	assert( sizeof(int) == sizeof(_f_int4) );
	*val = _rtc();
	*wrap = 0;
}

void
_SYSTEM_CLOCK_4 (_f_int4 *c, _f_int4 *r, _f_int4 *m)
{
	_f_int8 cval;

/*	If count argument present, return the value of real-time clock */

	if (c != NULL)
	    cval	= _rtc ();
	    cval	= (uint64)cval / 100L;
	    *c	= (_f_int4)(cval & 0x7fffffffL);


/*
 *	If count_rate argument present, return number of clocks per second.
 *	Use the global variable __hertz, which contains the number of clocks
 *	per second.
 */

	if (r != NULL)
	    *r = (_f_int4)((uint64)__hertz / 100L);

/*
 *	If count_max argument present, return maximum value clock can hold.
 *	This would be the largest integer the word can contain.
 */

	if (m != NULL)
	    *m = _INT_MAX_32;
}

#endif

#if defined(__mips)
/*
 *	_sysclock is the CRI internal entry point on MIPS systems used 
 *	by ATExpert code instrumentation.
 */
void
_sysclock_4(_f_int4 *val, _f_int4 *wrap)
{
	struct timeval	buf;
	struct timezone	buf2;
	_f_int8 cval;

	(void) gettimeofday (&buf, &buf2);

	/* cast long buf.tv_usec to unsigned long long to avoid sign
	 * extension which would cause the total to be less than
	 * tv_sec * 1000000.
	 */
	cval = (_f_int8)buf.tv_sec * 10000LL + (uint64)buf.tv_usec / 100LL;

	*wrap = (_f_int4) (cval >> 31);
	*val  = (_f_int4)(cval & 0x7fffffffLL);
}

/*
 *	sysclock_ is the Fortran user entry point.
 */
void
sysclock_4_(_f_int4 *val, _f_int4 *wrap)	
{
	assert( sizeof(int) == sizeof(_f_int) );
	_sysclock_4(val, wrap);
}

void
_SYSTEM_CLOCK_4 (_f_int4 *c, _f_int4 *r, _f_int4 *m)
{

/*	If count argument present, return the value of real-time clock */

	if (c != NULL) {
	    _f_int4 wrap, val;
	    _sysclock_4(&val, &wrap);
	    *c = val;
	}

/*
 *	If count_rate argument present, return number of clocks per second.
 */

/* return 100 microseconds per clock tick for mips. */
	if (r != NULL)
	    *r = 10000;		/* 100 microseconds per clock tick */

/*
 *	If count_max argument is present, return maximim value clock can hold.
 *	This would be the largest integer the word can contain.
 */

	if (m != NULL)
	    *m = INT_MAX;
}

#elif defined(_WORD32)

/*
 *	_sysclock is the CRI internal entry point on non-UNICOS systems used 
 *	by ATExpert code instrumentation.
 */
void
_sysclock_4(_f_int4 *val, _f_int4 *wrap)
{
	struct timeval	buf;
	struct timezone	buf2;
	long long cval;

	(void) gettimeofday (&buf, &buf2);

	/* cast long buf.tv_usec to unsigned long long to avoid sign
	 * extension which would cause the total to be less than
	 * tv_sec * 1000000.
	 */
	cval = (long long)buf.tv_sec * 10000LL + (uint64)buf.tv_usec / 100LL;

	*wrap = (_f_int4)(cval >> 31);
	*val  = (_f_int4)(cval & 0x7fffffffLL);
}

/*
 *	sysclock_ is the Fortran user entry point.
 */
void
sysclock_4_(_f_int4 *val, _f_int4 *wrap)	
{
	assert( sizeof(int) == sizeof(_f_int) );
	_sysclock_4(val, wrap);
}


void
_SYSTEM_CLOCK_4 (_f_int4 *c, _f_int4 *r, _f_int4 *m)
{

/*	If count argument present, return the value of real-time clock */

	if (c != NULL) {
	    _f_int4 wrap;
	    _sysclock_4(c, &wrap);
	}

/*
 *	If count_rate argument present, return number of clocks per second.
 */

	if (r != NULL)
	    *r = 10000;		/* 100 microsecond per clock tick */

/*
 *	If count_max argument is present, return maximim value clock can hold.
 *	This would be the largest integer the word can contain.
 */

	if (m != NULL)
	    *m = INT_MAX;
}


#endif	/* _WORD32  or MIPSEB */

#endif


#ifdef	_F_INT8
#ifdef	_UNICOS

/*
 *	SYSCLOCK is the Fortran user entry point.
 */
void
SYSCLOCK_8(_f_int8 *val, _f_int8 *wrap)	
{
	assert( sizeof(int) == sizeof(_f_int) );
	*val = _rtc();
	*wrap = 0;
}

void
_SYSTEM_CLOCK_8 (_f_int8 *c, _f_int8 *r, _f_int8 *m)
{

/*	If count argument present, return the value of real-time clock */

	if (c != NULL)
	    *c = _rtc ();

/*
 *	If count_rate argument present, return number of clocks per second.
 *	Use the global variable __hertz, which contains the number of clocks
 *	per second.
 */

	if (r != NULL)
	    *r = __hertz;

/*
 *	If count_max argument present, return maximum value clock can hold.
 *	This would be the largest integer the word can contain.
 */

	if (m != NULL)
	    *m = LONG_MAX;
}

#endif	/* _UNICOS */

#if defined(__mips)

/*
 *	_sysclock is the CRI internal entry point on non-UNICOS systems used 
 *	by ATExpert code instrumentation.
 */
void
_sysclock_8(_f_int8 *val, _f_int8 *wrap)
{
	struct timeval	buf;
	struct timezone	buf2;
	_f_int8 cval;

	(void) gettimeofday (&buf, &buf2);

	/* cast long buf.tv_usec to unsigned long long to avoid sign
	 * extension which would cause the total to be less than
	 * tv_sec * 1000000.
	 */
	cval = (_f_int8)buf.tv_sec * 1000000LL + (uint64)buf.tv_usec;

	*wrap = (_f_int8) ((_f_int4)(cval >> 31));
	*val  = (_f_int8)(cval & LONG_MAX);
}

/*
 *	sysclock_ is the Fortran user entry point.
 */
void
sysclock_8_(_f_int8 *val, _f_int8 *wrap)	
{
	assert( sizeof(int) == sizeof(_f_int) );
	_sysclock_8(val, wrap);
}


void
_SYSTEM_CLOCK_8 (_f_int8 *c, _f_int8 *r, _f_int8 *m)
{

/*	If count argument present, return the value of real-time clock */

	if (c != NULL) {
	    _f_int8 wrap;
	    _sysclock_8(c, &wrap);
	}

/*
 *	If count_rate argument present, return number of clocks per second.
 */

	if (r != NULL)
	    *r = 1000000;		/* 1 microsecond per clock tick */

/*
 *	If count_max argument is present, return maximim value clock can hold.
 *	This would be the largest integer the word can contain.
 */

	if (m != NULL)
	    *m = LONG_MAX;
}

#elif defined(_WORD32)

/*
 *	_sysclock is the CRI internal entry point on non-UNICOS systems used 
 *	by ATExpert code instrumentation.
 */
void
_sysclock_8(_f_int8 *val, _f_int8 *wrap)
{
	struct timeval	buf;
	struct timezone	buf2;
	long long cval;

	(void) gettimeofday (&buf, &buf2);

	/* cast long buf.tv_usec to unsigned long long to avoid sign
	 * extension which would cause the total to be less than
	 * tv_sec * 1000000.
	 */
	cval = (long long)buf.tv_sec * 1000000LL + (uint64)buf.tv_usec;

	*wrap = (_f_int8)(cval >> 31);
	*val  = (_f_int8)(cval & LONG_MAX);
}

/*
 *	sysclock_ is the Fortran user entry point.
 */
void
sysclock_8_(_f_int8 *val, _f_int8 *wrap)	
{
	assert( sizeof(int) == sizeof(_f_int) );
	_sysclock_8(val, wrap);
}


void
_SYSTEM_CLOCK_8 (_f_int8 *c, _f_int8 *r, _f_int8 *m)
{

/*	If count argument present, return the value of real-time clock */

	if (c != NULL) {
	    _f_int8 wrap;
	    _sysclock_8(c, &wrap);
	}

/*
 *	If count_rate argument present, return number of clocks per second.
 */

	if (r != NULL)
	    *r = 1000000;		/* 1 microsecond per clock tick */

/*
 *	If count_max argument is present, return maximim value clock can hold.
 *	This would be the largest integer the word can contain.
 */

	if (m != NULL)
	    *m = LONG_MAX;
}


#endif	/* _WORD32 or MIPSEB */

#endif

