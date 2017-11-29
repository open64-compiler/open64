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
/* $Header: /proj/osprey/CVS/open64/osprey1.0/libU77/chdir_.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
/* $Header: /proj/osprey/CVS/open64/osprey1.0/libU77/chdir_.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
/*
 *
 * change default directory
 *
 * calling sequence:
 *	integer chdir
 *	ierror = chdir(dirname)
 * where:
 *	ierror will receive a returned status (0 == OK)
 *	dirname is the directory name
 */

#include <unistd.h>
#if defined(BUILD_OS_DARWIN)
#include <stdlib.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <malloc.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include "cmplrs/f_errno.h"
#include <sys/param.h>
#ifndef	MAXPATHLEN
#define MAXPATHLEN	128
#endif
#include "externals.h"

#ifdef KEY /* Bug 1683 */

#include "pathf90_libU_intrin.h"

pathf90_i4
pathf90_chdir(char *dname, pathf90_i4 *status, int dnamlen)
{
	pathf90_i4 junk;
	status = (0 == status) ? &junk : status;
	if (!bufarg && !(bufarg=malloc(bufarglen=dnamlen+1)))
		return((*status = errno=F_ERSPACE));
	else if (bufarglen <= dnamlen && !(bufarg=realloc(bufarg, bufarglen=dnamlen+1)))
		return(*status = (errno=F_ERSPACE));
	g_char(dname, dnamlen, bufarg);
	if (chdir(bufarg) != 0) {
		return(*status = errno);
	}
	return(*status = 0);
}

#else

extern int 
chdir_ (char *dname, int dnamlen)
{
	if (!bufarg && !(bufarg=malloc(bufarglen=dnamlen+1)))
#ifdef __sgi
	{
		errno=F_ERSPACE;
		return(-1);
	}
#else
		return((errno=F_ERSPACE));
#endif
	else if (bufarglen <= dnamlen && !(bufarg=realloc(bufarg, bufarglen=dnamlen+1)))
#ifdef __sgi
	{
		errno=F_ERSPACE;
		return(-1);
	}
#else
		return((errno=F_ERSPACE));
#endif
	g_char(dname, dnamlen, bufarg);
#ifdef __sgi
	return(chdir(bufarg));
#else
	if (chdir(bufarg) != 0)
		return(errno);
	return(0);
#endif
}
#endif /* KEY Bug 1683 */
