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

#pragma ident "@(#) libu/clib/fwrite.c	92.1	07/01/99 13:42:20"
#include <fortran.h>
#include <stdio.h>
#include <liberrno.h>

/*	FWRITE - Fortran callable interface to fwrite(3)
 * 
 *   	Call from Fortran:
 *					(non-64 bit address systems only)
 *		CHARACTER * (*) PTR	
 *		INTEGER SIZE, N, FP
 *		CALL FWRITE(PTR, SIZE, N, FP)
 *		
 *			-or-		(non-64 bit address systems only)
 *		CHARACTER * (*) PTR
 *		INTEGER SIZE, N, FP, FWRITE
 *		I = FWRITE(PTR, SIZE, N, FP)
 *
 *			-or-
 *		INTEGER PTR, SIZE, N, FP
 *		CALL FWRITE(PTR, SIZE, N, FP)
 *		
 *			-or-
 *		INTEGER PTR, SIZE, N, FP, FWRITE
 *		I= FWRITE(PTR, SIZE, N, FP)
 */

_f_int
FWRITE(ptr, size, n, fp)
long	ptr;
long	*size, *n, **fp;
{
	char	*cptr;
	int	ret;

	if (_numargs() * sizeof(long) !=
	    sizeof(long) + sizeof(long*) + sizeof(long*) + sizeof(long**))
			_lerror(_LELVL_ABORT,FEARGLST,"FWRITE");

#ifndef	_ADDR64
	if (_isfcd(ptr)) {	/* If character */
		_fcd	fcdptr;

		fcdptr	= *(_fcd *) &ptr;
		cptr	= _fcdtocp(fcdptr);
	}
	else			/* Hollerith */
#endif
		cptr	= (char *) ptr;

	ret	= fwrite(cptr, *size, *n, (FILE *) (*fp));

	return ( (_f_int) ret);
}

/*	FWRITEC - Fortran callable interface to fwrite(3).  Accepts
 *		  an argument of type character.
 * 
 *   	Call from Fortran:
 *		CHARACTER * (*) PTR
 *		INTEGER SIZE, N, FP
 *		CALL FWRITE(PTR, SIZE, N, FP)
 *		
 *			-or-
 *		CHARACTER * (*) PTR
 *		INTEGER SIZE, N, FP, FWRITE
 *		I = FWRITE(PTR, SIZE, N, FP)
 */

_f_int
FWRITEC(ptr, size, n, fp)
_fcd	ptr;
long	*size, *n, **fp;
{
	int	ret;

	if (_numargs() * sizeof(long) !=
	    sizeof(_fcd) + sizeof(long*) + sizeof(long*) + sizeof(long**))
			_lerror(_LELVL_ABORT,FEARGLST,"FWRITE");

	ret	= fwrite(_fcdtocp(ptr), *size, *n, (FILE *) (*fp));

	return ( (_f_int) ret);
}
