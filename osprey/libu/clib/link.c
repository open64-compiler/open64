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

#pragma ident "@(#) libu/clib/link.c	92.1	07/01/99 13:42:20"
#include <fortran.h>
#include <malloc.h>
#include <unistd.h>

/*
 *	LINK - Fortran interface to link(2) system call
 *
 *	Call from Fortran:
 *		CHARACTER * (*), PATH1, PATH2
 *		CALL LINK(PATH1, PATH2)
 *
 *			-or-
 *		CHARACTER * (*), PATH1, PATH2
 *		INTEGER LINK
 *		I = LINK(PATH1, PATH2)
 *
 *			-or-
 *		INTEGER PATH1, PATH2
 *		CALL LINK(PATH1, PATH2)
 *			-or-
 *		INTEGER LINK, PATH1, PATH2
 *		I = LINK(PATH1, PATH2)
 */

_f_int
LINK(path1, path2)
long	path1, path2;
{
	char	*p1ptr, *p2ptr;
	int	ret;

	if (_numargs() < 2)
		return( (_f_int) -1);

	/* Process path1 parameter */

	if (_isfcd(path1)) {	/* If path1 is a Fortran character */
		_fcd	fcdp1;

		fcdp1	= *(_fcd *) &path1;
		p1ptr	= _f2ccpy(fcdp1);

		if (p1ptr == NULL)
			return( (_f_int) -1);
	}
	else			/* Path1 is a hollerith */
		p1ptr	= (char *) path1;

	/* Process path2 parameter */

	if (_isfcd(path2)) {	/* If path2 is a Fortran character */
		_fcd	fcdp2;

		fcdp2	= *(_fcd *) &path2;
		p2ptr	= _f2ccpy(fcdp2);

		if (p2ptr == NULL) {

			if (_isfcd(path1))
				free(p1ptr);

			return( (_f_int) -1);
		}
	}
	else			/* Path2 is a hollerith */
		p2ptr	= (char *) path2;

	/* Do link() */

	ret	= link(p1ptr, p2ptr);

	/* Free memory */

	if (_isfcd(path1))
		free(p1ptr);

	if (_isfcd(path2))
		free(p2ptr);

	return( (_f_int) ret);
}
