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


#pragma ident "@(#) libu/util/ishell.c	92.2	10/29/99 21:40:31"
#include <errno.h>
#include <fortran.h>
#if !defined(_ABSOFT)
#include <malloc.h>
#endif
#include <stddef.h>
#include <stdlib.h>

/*
 *	ISHELL - Fortran callable routine to send a command
 *		to the shell.
 *		
 *         INPUT:  cmdargs--type character argument or type
 *                          other containing character data that
 *                          ends with a NULL character.
 *	   OUTPUT: integer variable containing status returned by
 *                 the system() call.  If system() returns -1,
 *                 the value of -errno is returned.
 */

#ifdef	_UNICOS
_f_int
ISHELL(_fcd cmdarg)
{
	char	*cptr;
	int	ret;
	int	fcdflag;

	/* Check number of arguments */

	if (_numargs() < 1)
		return( (_f_int) -1);

	/* IS cmdarg an _fcd ? */
	fcdflag	= 0;
#ifdef	_ADDR64
	if (_numargs() * sizeof(long) == sizeof(_fcd)) 
#else
	if (_isfcd(cmdarg))
#endif
		fcdflag	= 1;
  
	/* Convert argument to C character string */

	if (fcdflag) {		/* If Fortran character */
		cptr	= _fc_acopy(cmdarg);

		if (cptr == NULL)
			return( (_f_int) -1);
	}
	else
		cptr	= _fcdtocp(cmdarg);

	/* Run command using system() */

	ret	= system(cptr);

	if (ret == -1)
		ret	= -errno;

	if (fcdflag)
		free(cptr);

	return( (_f_int) ret);
}

#endif	/* _UNICOS */

#ifndef	_UNICOS
#include <string.h>
_f_int
ishell_(char* cmdcptr, int cmdlen)
{
	char	*cptr;
	int	ret;

	cptr = malloc(cmdlen + 1);
	if (cptr == NULL)
		return( (_f_int) -1);

	(void) memcpy(cptr, cmdcptr, cmdlen);
	cptr[cmdlen] = '\0';

	ret	= system(cptr);

	if (ret == -1)
		ret	= -errno;

	free(cptr);

	return( (_f_int) ret);
}
#endif	/* ! _UNICOS */
