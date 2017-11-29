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

#pragma ident "@(#) libu/clib/rename.c	92.1	07/01/99 13:42:20"
#include <fortran.h>
#include <malloc.h>
#include <stdio.h>

/*
 *	RENAME  - Fortran interface to rename(2) system call
 *   
 *      Call from Fortran:
 *		CHARACTER * (*), OLD, NEW
 *              CALL RENAME(OLD, NEW)
 *
 *                     -or-
 *
 *              CHARACTER * (*), OLD, NEW
 *              INTEGER RENAME
 *              I = RENAME(OLD, NEW)
 *
 *                     -or-
 *
 *              INTEGER OLD,NEW
 *              CALL RENAME(OLD, NEW)
 *
 *                     -or-
 *
 *              INTEGER OLD, NEW, RENAME
 *              I = RENAME(OLD, NEW)
 *      Arguments:
 *              OLD - path name of file to be renamed
 *              NEW - new path name of the file
 *      Returns:
 *              0  - success
 *              -1 - failure (see rename(2) for more information)
 */

_f_int
RENAME(old, new)
long	old, new;
{
	char	*oldptr, *newptr;
	int	ret;

	if (_numargs() < 2)
		return( (_f_int) -1);

	/* Process old parameter */

	if (_isfcd(old)) {	/* If old is a Fortran character */
		_fcd	fcdold;

		fcdold	= *(_fcd *) &old;
		oldptr	= _f2ccpy(fcdold);

		if (oldptr == NULL)
			return( (_f_int) -1);
	}
	else			/* Old is a hollerith */
		oldptr	= (char *) old;

	/* Process new parameter */

	if (_isfcd(new)) {	/* If new is a Fortran character */
		_fcd	fcdnew;

		fcdnew	= *(_fcd *) &new;
		newptr	= _f2ccpy(fcdnew);

		if (newptr == NULL) {

			if (_isfcd(old))
				free(oldptr);

			return( (_f_int) -1);
		}
	}
	else			/* New is a hollerith */
		newptr	= (char *) new;

	/* Do rename() */

	ret	= rename(oldptr, newptr);

	/* Free memory */

	if (_isfcd(old))
		free(oldptr);

	if (_isfcd(new))
		free(newptr);

	return( (_f_int) ret);
}
