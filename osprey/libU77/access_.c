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
/* $Header: /proj/osprey/CVS/open64/osprey1.0/libU77/access_.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
/* $Header: /proj/osprey/CVS/open64/osprey1.0/libU77/access_.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
/*
 * determine accessability of a file
 *
 * calling format:
 *	integer access
 *	ierror = access(filename, mode)
 * where:
 *	ierror will be 0 for successful access; an error number otherwise.
 *	filename is a character string
 *	mode is a character string which may include any combination of
 *	'r', 'w', 'x', ' '. (' ' => test for existence)
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
char *bufarg;
long bufarglen;

#ifdef KEY /* Bug 1683 */
#include "pathf90_libU_intrin.h"

pathf90_i4
pathf90_access(char *name, char *mode, int namlen, int modlen)

#else

extern int 
access_ (char *name, char *mode, int namlen, int modlen)

#endif /* KEY Bug 1683 */
{
	int m = 0;

	if (!bufarg && !(bufarg=malloc(bufarglen=namlen+1)))
#ifdef __sgi
	{
		errno=F_ERSPACE;
		return(-1);
	}
#else
		return((errno=F_ERSPACE));
#endif
	else if (bufarglen <= namlen && !(bufarg=realloc(bufarg, bufarglen=namlen+1)))
#ifdef __sgi
	{
		errno=F_ERSPACE;
		return(-1);
	}
#else
		return((errno=F_ERSPACE));
#endif
	g_char(name, namlen, bufarg);
	if (bufarg[0] == '\0')
#ifdef __sgi
	{
		errno=ENOENT;
		return(-1);
	}
	if (access(bufarg, 0) < 0)
		return(-1);
#else
		return((errno=ENOENT));
	if (access(bufarg, 0) < 0)
		return(errno);
#endif
	while (modlen--) switch(*mode++)
	{
		case 'x':
			m |= 1;
			break;

		case 'w':
			m |= 2;
			break;

		case 'r':
			m |= 4;
			break;
#ifdef KEY /* Bug 1683 */
		case ' ':
			m |= F_OK;
		        break;
                default:
		        return errno = EINVAL;
#endif /* KEY Bug 1683 */
	}
#ifdef __sgi
	if (m > 0)
		return(access(bufarg, m));
#else
	if (m > 0 && access(bufarg, m) < 0)
		return(errno);
#endif
	return(0);
}
