/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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

/* $Header: /proj/osprey/CVS/open64/osprey1.0/libU77/ctime_.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
/*
 *
 * convert system time to ascii string
 *
 * calling sequence:
 *	character*24 string, ctime
 *	integer clock
 *	string = ctime (clock)
 * where:
 *	string will receive the ascii equivalent of the integer clock time.
*/

#include <sys/types.h>
#include <time.h>
#include "externals.h"

#ifdef KEY /* Bug 1683, 5019 */

#include "pathf90_libU_intrin.h"

#define CTIME_BUFLEN	26

void
pathf90_ctime4(char *str, int len, pathf90_i4 *clock)
{
	char buf[CTIME_BUFLEN];
	time_t ctemp = (time_t) *clock;
	char *s = ctime_r(&ctemp, buf);
	s[24] = '\0';
	b_char(s, str, len);
}

void
pathf90_subr_ctime4(pathf90_i4 *clock, char *str, int len)
{
  pathf90_ctime4(str, len, clock);
}

void
pathf90_ctime8(char *str, int len, pathf90_i8 *clock)
{
	char buf[CTIME_BUFLEN];
	time_t ctemp = (time_t) *clock;
	char *s = ctime_r(&ctemp, buf);
	s[24] = '\0';
	b_char(s, str, len);
}

void
pathf90_subr_ctime8(pathf90_i8 *clock, char *str, int len)
{
  pathf90_ctime8(str, len, clock);
}

#else

extern void
ctime_ (char *str, int len, int *clock)
{
	char *s = ctime((time_t *)clock);
	s[24] = '\0';
	b_char(s, str, len);
}
#endif /* KEY Bug 1683, 5019 */
