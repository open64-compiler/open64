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

/* $Header: /proj/osprey/CVS/open64/osprey1.0/libU77/getcwd_.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
 /*
 * Get pathname of current working directory.
 *
 * calling sequence -32bit:
 *	character*128 path
 *	integer *4 getcwd, ierr
 *	ierr = getcwd(path)
 * calling sequence -64bit:
 *	character*128 path
 *	integer *8 getcwd, ierr
 *	ierr = getcwd(path)
 * or alternatively (-32bit and -64bit):
 *      character*128 path
 *	call getcwd(path)
 * where:
 *	path will receive the pathname of the current working directory.
 *	ierr will be non-zero if successful
 */

#include <errno.h>
#include <unistd.h>
#include <sys/param.h>
#ifndef	MAXPATHLEN
#define MAXPATHLEN	128
#endif
#include "externals.h"

#ifdef KEY /* Bug 1683 */

#include "pathf90_libU_intrin.h"

pathf90_i4
pathf90_getcwd(char *path, pathf90_i4 *status, int len)
{
	char	*p;
	char	pathname[MAXPATHLEN];
	pathf90_i4 junk;
	status = (0 == status) ? (&junk) : status;

/* Bug 3349: Modern Unix should have 2-arg getcwd; if the target OS is
 * unexpected, the code should fail instead of silently compiling with
 * neither getwd nor getcwd .
 */
#  if defined(__linux) || defined(BUILD_OS_DARWIN)
	p = getcwd(pathname,MAXPATHLEN);
#  else
#    error "Check function getwd/getcwd signature"
#  endif

	b_char(pathname, path, len);
	if (p)
		return(*status = 0);
	else
		return(*status = errno);
}

#else

extern long
getcwd_(char *path, int len)

{
	char	*p;
	char	pathname[MAXPATHLEN];

#ifdef KEY
/* Bug 3349: Modern Unix should have 2-arg getcwd; if the target OS is
 * unexpected, the code should fail instead of silently compiling with
 * neither getwd nor getcwd .
 */
#  ifdef __linux
	p = getcwd(pathname,MAXPATHLEN);
#  else
#    error "Check function getwd/getcwd signature"
#  endif
#else
#ifdef _BSD
	extern char	*getwd();		/* sjc #nit 27Jan88 */
	p = getwd(pathname);
#endif /* _BSD */
#if defined(_SYSV) || defined(_SYSTYPE_SVR4)
	p = getcwd(pathname,MAXPATHLEN);	/* AGC #710 2/17/87 */
#endif /* _SYSV */
#endif /* KEY */

	b_char(pathname, path, len);
#ifdef __sgi
	return((long)p);
#else
	if (p)
		return(0);
	else
		return(errno);
#endif
}

#endif /* KEY Bug 1683 */
