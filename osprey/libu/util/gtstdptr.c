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


#pragma ident "@(#) libu/util/gtstdptr.c	92.1	07/07/99 13:18:33"
  
#include <stdio.h>
  
/*
 *	GTSTDPTR	Returns pointer to standard file
 *
 *	Call from Fortran:
 *
 *		INTEGER FD, STREAM, GTSTDPTR
 *      	STREAM = GTSTDPTR(FD)
 *
 *              FD	File descriptor, type INTEGER, as follows:
 *
 *			0  Return address of standard input (stdin)
 *			1  Return address of standard output (stdout)
 *			2  Return address of standard error (stderr)
 *
 *	Result:
 *
 *		STREAM	Address of FILE structure (stream pointer)
 *
 *
 *	Refer to the stdio(3) and fopen(3) man pages for a description of
 *	file descriptors and stream pointers.  This routine is intended for
 *	those applications that manipulate such data structures directly.
 *	For example, via the Fortran-callable FREAD(3) and FWRITE routines.
 */

FILE *
GTSTDPTR (fd)
long	*fd;			/* file descriptor */
{
	if (_numargs() == 1) {
		if (*fd >= 0 && *fd <= 2)
			return(_getfp(*fd));
	}
	return(NULL);
}
