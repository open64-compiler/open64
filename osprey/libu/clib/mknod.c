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

#pragma ident "@(#) libu/clib/mknod.c	92.1	07/01/99 13:42:20"

#include <fortran.h>
#include <malloc.h>
#include <unistd.h>
#include <liberrno.h>

/*
 *	MKNOD - Fortran interface to mknod(2) system call
 *
 *	Call from Fortran:
 *		CHARACTER * (*), PATH
 *		INTEGER MODE, DEV
 *		CALL MKNOD(PATH, MODE, DEV)
 *
 *			-or-
 *		CHARACTER * (*), PATH
 *		INTEGER MKNOD, MODE, DEV
 *		I = MKNOD(PATH, MODE, DEV)
 *
 *			-or, on systems that do not have 64-bit addresses: -
 *		INTEGER PATH, MODE, DEV
 *		CALL MKNOD(PATH, MODE, DEV)
 *
 *			-or-
 *		INTEGER MKNOD, PATH, MODE, DEV
 *		I = MKNOD(PATH, MODE, DEV)
 */

_f_int
MKNOD(path, mode, dev)
_fcd	path;
long	*mode, *dev;
{
#ifdef _ADDR64
char *pathptr;
_f_int ret;
	if (_numargs() != (sizeof(_fcd) + 2*sizeof(long *))/sizeof(long)) {
		_lerror(_LELVL_ABORT,FEARGLST,"MKNOD");
	}
	pathptr = _f2ccpy(path);
	if (pathptr == NULL)
		return( (_f_int) -1);
	ret = mknod(pathptr, *mode, *dev);
	free(pathptr);
	return((_f_int)ret);
#else
	if (_numargs() < 3) {
		_lerror(_LELVL_ABORT,FEARGLST,"MKNOD");
	}
	return((_f_int)_clib_call(mknod, path, *mode, *dev));
#endif
}
