/*
 * Copyright 2002, 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 1999-2001, Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  Further, any
  license provided herein, whether implied or otherwise, is limited to 
  this program in accordance with the express provisions of the 
  GNU Lesser General Public License.  

  Patent licenses, if any, provided herein do not apply to combinations 
  of this program with other product or programs, or any other product 
  whatsoever.  This program is distributed without any warranty that the 
  program is delivered free of the rightful claim of any third person by 
  way of infringement or the like.  

  See the GNU Lesser General Public License for more details.

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

*/

/* --------------------------------------------------- */
/* | All Rights Reserved.                            | */
/* --------------------------------------------------- */
/* $Header: /proj/osprey/CVS/open64/osprey1.0/libU77/itime_.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
/*
 *
 * return the current time in numerical form
 *
 * calling sequence:
 *	integer iarray(3)
 *	call itime(iarray)
 * where:
 *	iarray will receive the current time; hour, min, sec.
 */

#include <sys/types.h>
#if defined(_SYSV) || defined(_SYSTYPE_SVR4) || defined(KEY)
#include <time.h>
#else
#include <sys/time.h>
#endif

typedef struct {
 int ihr;
 int imin;
 int isec;
} itime_struct;

#ifdef KEY /* Bug 1683, 5019 */

#include "pathf90_libU_intrin.h"

void
pathf90_itime(pathf90_i4 *iar)
{
	struct tm lclt;
	time_t t;

	t = time(0);
	localtime_r(&t, &lclt);
	iar[0] = lclt.tm_hour;
	iar[1] = lclt.tm_min;
	iar[2] = lclt.tm_sec;
}

#else

extern void
itime_ (itime_struct *iar)
{
	struct tm *lclt;
	time_t t;

	t = time(0);
	lclt = localtime(&t);
	iar->ihr = lclt->tm_hour;
	iar->imin = lclt->tm_min;
	iar->isec = lclt->tm_sec;
}

#endif /* KEY Bug 1683, 5019 */
