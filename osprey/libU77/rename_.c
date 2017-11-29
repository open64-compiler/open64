/*
 * Copyright 2004, 2005 PathScale, Inc.  All Rights Reserved.
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
/* $Header: /proj/osprey/CVS/open64/osprey1.0/libU77/rename_.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
/* $Header: /proj/osprey/CVS/open64/osprey1.0/libU77/rename_.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
/*
char	id_rename[] = "@(#)rename_.c	1.2";
 *
 * rename a file atomically
 *
 * synopsis:
 *	integer function rename (from, to)
 *	character*(*) from, to
 *
 * where:
 *	return value will be zero normally, an error number otherwise.
 */

#include <sys/types.h>
#include <stdio.h>
#if defined(BUILD_OS_DARWIN)
#include <stdlib.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <malloc.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <sys/param.h>
#ifndef	MAXPATHLEN
#define MAXPATHLEN	128
#endif
#include "cmplrs/f_errno.h"
#include "externals.h"

#ifdef KEY /* Bug 1683 */

#include "pathf90_libU_intrin.h"

pathf90_i4
pathf90_rename(char *from, char *to, pathf90_i4 *status, int frlen, int tolen)
{
	char	*frbuf, *tobuf;
	pathf90_i4 junk;
	status = (0 == status) ? (&junk) : status;

	if (frlen <= 0 || tolen <= 0 || *from == ' ' || *to == ' ')
		return ((*status = errno = F_ERARG));
	if (!bufarg && !(bufarg=malloc(bufarglen=frlen+tolen+2)))
		return(*status = (errno=F_ERSPACE));
	else if (bufarglen <= frlen+tolen+1 && !(bufarg=realloc(bufarg, 
			bufarglen=frlen+tolen+2)))
		return(*status = (errno=F_ERSPACE));
	frbuf = bufarg;
	tobuf = &bufarg[frlen+1];
	g_char (from, frlen, frbuf);
	g_char (to, tolen, tobuf);
	if (rename (frbuf, tobuf) != 0)
		return (*status = errno);
	return (*status = 0);
}

#else

extern int
rename_ (char *from, char *to, int frlen, int tolen)
{
	char	*frbuf, *tobuf;

	if (frlen <= 0 || tolen <= 0 || *from == ' ' || *to == ' ')
		return ((errno = F_ERARG));
	if (!bufarg && !(bufarg=malloc(bufarglen=frlen+tolen+2)))
		return((errno=F_ERSPACE));
	else if (bufarglen <= frlen+tolen+1 && !(bufarg=realloc(bufarg, 
			bufarglen=frlen+tolen+2)))
		return((errno=F_ERSPACE));
	frbuf = bufarg;
	tobuf = &bufarg[frlen+1];
	g_char (from, frlen, frbuf);
	g_char (to, tolen, tobuf);
	if (rename (frbuf, tobuf) != 0)
		return (errno);
	return (0);
}

#endif /* KEY Bug 1683 */
