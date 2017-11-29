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

#pragma ident "@(#) libu/clib/tempnam.c	92.1	07/01/99 13:42:20"
#include <fortran.h>
#include <malloc.h>
#include <stdio.h>
#include <liberrno.h>

/*
 *	TEMPNAM - Fortran interface to tempnam(3) 
 *
 *	Call from Fortran:
 *		CHARACTER * (*), DIR, PFX
 *		INTEGER TEMPNAM
 *		I = TEMPNAM(DIR, PFX)
 *
 *			-or-
 *
 *		INTEGER DIR, PFX, TEMPNAM
 *		I = TEMPNAM(DIR, PFX)
 *
 *	Returns a pointer to the temporary name.
 */

_f_int
#ifdef _ADDR64
TEMPNAM(fcddir, fcdpfx)
_fcd	fcddir, fcdpfx;
#else
TEMPNAM(dir, pfx)
long	dir, pfx;
#endif
{
	char	*dirptr, *pfxptr;
	char	*tnam;
	int	ret;

	if (_numargs()*sizeof(long) != 2*sizeof(_fcd)) {
		_lerror(_LELVL_ABORT,FEARGLST,"TEMPNAM");
	}

	/* Process dir parameter */

#ifndef _ADDR64
	if (_isfcd(dir)) {	/* If dir is a Fortran character */
		_fcd	fcddir;

		fcddir	= *(_fcd *) &dir;
		dirptr	= _f2ccpy(fcddir);

		if (dirptr == NULL)
			return( (_f_int) -1);
	}
	else			/* Dir is a hollerith */
		dirptr	= (char *) dir;

	/* Process pfx parameter */

	if (_isfcd(pfx)) {	/* If pfx is a Fortran character */
		_fcd	fcdpfx;

		fcdpfx	= *(_fcd *) &pfx;
		pfxptr	= _f2ccpy(fcdpfx);

		if (pfxptr == NULL) {

			if (_isfcd(dir))
				free(dirptr);

			return( (_f_int) -1);
		}
	}
	else			/* Pfx is a hollerith */
		pfxptr	= (char *) pfx;
#else
		dirptr	= _f2ccpy(fcddir);

		if (dirptr == NULL)
			return( (_f_int) -1);

		pfxptr	= _f2ccpy(fcdpfx);

		if (pfxptr == NULL) {
			free(dirptr);
			return( (_f_int) -1);
		}
#endif

	/* Do tempnam() */

	tnam	= tempnam(dirptr, pfxptr);

	ret	= (tnam == NULL) ? -1 : (int) tnam;

	/* Free memory */

#ifndef _ADDR64
	if (_isfcd(dir))
#endif
		free(dirptr);

#ifndef _ADDR64
	if (_isfcd(pfx))
#endif
		free(pfxptr);

	return( (_f_int) ret);
}
