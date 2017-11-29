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

#pragma ident "@(#) libu/clib/fgets.c	92.1	07/01/99 13:42:20"

#include <stdio.h>
#include <fortran.h>
#include <stdarg.h>

/*	FGETS 
 *
 *	FGETS - Fortran interface to fgets
 *
 *	Call from Fortran:
 *		INTEGER BUF, N, FP
 *		CALL FGETS(BUF, N, FP)
 *		   -or-
 *		INTEGER BUF, N, FP, FGETS
 *		I = FGETS(BUF, N, FP)
 *		   -or-
 *		CHARACTER * (*) BUF
 *		INTEGER N, FP
 *		CALL FGETS(BUF, N, FP)
 *		   -or-
 *		CHARACTER * (*) BUF
 *		INTEGER  N, FP, FGETS
 *		I = FGETS(BUF, N, FP)
 */

_f_int
FGETS(long *buf, ...)
{
	va_list args;
	long *n;
	long **fp;
#ifdef _ADDR64
	long flen;

	va_start(args, buf);
	if (_numargs() * sizeof(long) == sizeof(_fcd) + sizeof(long *) +
		sizeof(long **)) {
		/* this technique is not recommended. It is used only */
		/* because our documentation says that we allow both */
		/* characters and integers */
		flen = va_arg(args, long);	/* get length of fcd */
	}
#else
	va_start(args, buf);
#endif
	n = va_arg(args, long *);
	fp = va_arg(args, long **);
	return((_f_int)fgets((char *)buf, (int)*n, (FILE *)(*fp)));
}

