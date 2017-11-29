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


#pragma ident "@(#) libfi/element/irtc.c	92.1	06/16/99 15:47:23"

#include <fortran.h>           

/*
 *	Duplicate names
 *
 *	_IRTC_	- for f90 intrinsic (not used on PVP & MPP)
 *	IRTC	- if called as a subroutine on PVP & MPP systems
 *	$IRTC	- for cf77 intrinsic
 *	irtc_	- if called as a subroutine on non-UNICOS systems
 */
#ifdef _UNICOS			/*****************************************/

extern long __hertz;
_f_int8
IRTC_RATE(void)
{
	return (_f_int8) __hertz;
}

#pragma _CRI duplicate _IRTC_ as IRTC
#pragma _CRI duplicate _IRTC_ as $IRTC
#pragma _CRI duplicate _IRTC_ as _IRTC

_f_int8
_IRTC_(long *result)
{
	long rt = (long)_rtc();

	if (_numargs() == 1)
		*result = rt;
	return(rt);
}

#elif	defined(__mips)		/*****************************************/

extern double _nowrap_cycles_per_sec;
extern long long _sysclock_nowrap(void);


_f_int8
irtc_rate_(void)
{
	if (_nowrap_cycles_per_sec == 0)
		_init_hw_clock();
	return (_f_int8) _nowrap_cycles_per_sec;
}

_f_int8
_IRTC_(void)
{
	return (_f_int8) _sysclock_nowrap();
}

void
irtc_(_f_int8 *result)
{
	*result = _sysclock_nowrap();
}

#else				/*****************************************/

#include <unistd.h>           
#include <sys/time.h>           
#include <sys/times.h>

_f_int
_IRTC_(void)
{
	clock_t ret;
	struct tms buf;
	ret = times(&buf);
	return( (_f_int) ret);
}

void
irtc_(long *result)
{
	*result = _IRTC_();
}

#endif
