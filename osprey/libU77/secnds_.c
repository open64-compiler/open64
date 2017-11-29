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

#ifndef KEY
/* $Header: /proj/osprey/CVS/open64/osprey1.0/libU77/secnds_.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
/*
 *
 * return the elapsed time since midnight minus the argument as a real
 * number
 *
 * calling sequence:
 *	real elapsed, start
 *	elapsed = secnds(start)
 * where:
 *	start contains a starting time value used as a reference point,
 *	and elapsed receives the number of seconds elapsed since that point.
 */

#include <stdio.h>
#include <stdlib.h>
#include "defalias.h"
#include <sys/types.h>
#ifdef _BSD
#include <sys/time.h>
#else 
#include <time.h>
#endif  /* _BSD */

extern float 
secnds_vms (float *start)  
{
#ifdef _BSD
	struct timeval curtime;
	struct timezone zone;
#else
	struct tm *t;
#endif  /* _BSD */

	time_t hms;

#if defined(_SYSV) || defined(_SYSTYPE_SVR4)
	if ((hms = time(0)) == -1)	/* bugfix 1450 */
	  {
	  perror("secnds");
	  exit(99);
	  }
/* 8/8/89 fix bug 4876 */
	t = localtime(&hms);
	return((t->tm_sec+t->tm_min*60+t->tm_hour*3600) - *start);
#endif /* _SYSV || _SYSTYPE_SVR4 */

#ifdef _BSD
	if (gettimeofday(&curtime, &zone) == -1)	/* bugfix 1450 */
	  {
	  perror("secnds");
	  exit(99);
	  }
	hms = (time_t) curtime.tv_sec - (time_t) zone.tz_minuteswest * 60;
	hms %= 86400;
	if ( hms<0 ) hms+= 86400;
	return( (float) hms + ((float) curtime.tv_usec / 1000000.0) - *start );
#endif  /* _BSD */

#ifdef KEY /* Bug 5019 */
	if ((hms = time(0)) == -1)
	  {
	  perror("secnds");
	  exit(99);
	  }
	struct tm result;
	t = localtime_r(&hms, &result);
	return((t->tm_sec+t->tm_min*60+t->tm_hour*3600) - *start);
#endif  /* KEY Bug 5019 */
}

defalias(secnds_vms, secnds_);

double dsecnds_vms(start)  
double *start;
{
#ifdef _BSD
	struct timeval curtime;
	struct timezone zone;
#else
	struct tm *t;
#endif  /* _BSD */

	time_t hms;

#if defined(_SYSV) || defined(_SYSTYPE_SVR4)
	if ((hms = time(0)) == -1)	/* bugfix 1450 */
	  {
	  perror("secnds");
	  exit(99);
	  }
/* 8/8/89 fix bug 4876 */
	t = localtime(&hms);
	return((double) (t->tm_sec+t->tm_min*60+t->tm_hour*3600) - *start);
#endif /* _SYSV || _SYSTYPE_SVR4 */

#ifdef _BSD
	if (gettimeofday(&curtime, &zone) == -1)	/* bugfix 1450 */
	  {
	  perror("secnds");
	  exit(99);
	  }
	hms = (time_t) curtime.tv_sec - (time_t) zone.tz_minuteswest * 60;
	hms %= 86400;
	if ( hms<0 ) hms+= 86400;
	return( (double) hms + ((double) curtime.tv_usec / 1000000.0) - *start );
#endif  /* _BSD */

#ifdef KEY /* Bug 5019 */
	if ((hms = time(0)) == -1)
	  {
	  perror("secnds");
	  exit(99);
	  }
	struct tm result;
	t = localtime_r(&hms, &result);
	return((double) (t->tm_sec+t->tm_min*60+t->tm_hour*3600) - *start);
#endif  /* KEY Bug 5019 */
}

defalias(dsecnds_vms, dsecnds_);

#endif // KEY
