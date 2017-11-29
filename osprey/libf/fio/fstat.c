/*
 * Copyright 2004, 2005 PathScale, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 2000, 2001, Silicon Graphics, Inc.  All Rights Reserved.

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



#pragma ident "@(#) libf/fio/fstat.c	92.2	08/02/99 10:38:18"
 
/*
 * get file status
 *
 * calling sequence:
 *
 *	integer fstat, statb(12), iunit
 *	call fstat (iunit, statb)
 *
 * where:
 *
 *	'statb' will receive the stat structure for file 'iunit'.
 *	actually it will contain most of the same information as the
 *	stat structure, there are some missing fields.
 *
 * __fstat_f90 will be called when there is no compatibility module
 * __fstat64_f90 will be called when there is no compatibility module
 * fstatf90_ will be called when there is a compatibility module
 * fstatf90_8_ will be called when there is a compatibility module
 */

#include <stdio.h>
#include <foreign.h>
#include <errno.h>
#include <liberrno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "fio.h"

extern int __fstat_f90(int *u, int *stbuf);
#ifdef KEY /* Bug 1683 */
static _f_int fstatf90_(_f_int *u, _f_int *stbuf);
#else
extern _f_int fstatf90_(_f_int *u, _f_int *stbuf);
#endif /* KEY Bug 1683 */
extern int __fstat64_f90(int *u, _f_int8 *stbuf);
extern _f_int fstatf90_4_4_8_(_f_int *u, _f_int8 *stbuf);
extern _f_int8 fstatf90_8_(_f_int8 *u, _f_int8 *stbuf);

#ifdef KEY /* Bug 1683 */

int
pathf90_fstat(int *u, int *stbuf, int *status)
{
  int junk;
  status = (0 == status) ? (&junk) : status;
  return *status = fstatf90_(u, stbuf);
}

#endif /* KEY Bug 1683 */

int 
__fstat_f90(int *u, int *stbuf)
{
	return fstatf90_(u,stbuf);
}

#ifdef KEY /* Bug 1683 */
/* Don't pollute the Fortran namespace */
static
#endif /* KEY Bug 1683 */
_f_int 
fstatf90_(_f_int *u, _f_int *stbuf)
{
	int 		n, retval;
	unum_t 		unum;
	unit		*cup;
	struct stat 	buf;
	struct fiostate	cfs;

	unum	= *u;
	STMT_BEGIN(unum, 0, T_INQU, NULL, &cfs, cup);

	if (cup == NULL && !GOOD_UNUM(unum))
		_ferr(&cfs, FEIVUNIT, unum);	/* invalid unit number */

	if (cup == NULL)
		retval	= -1;		/* unit is not open */
	else if (cup->usysfd == -1)
		retval	= -1;		/* file is not disk-resident */
	else {
		n	= fstat(cup->usysfd, &buf);
		if (n < 0)
			_ferr(&cfs, errno);
		*stbuf++ = buf.st_dev;
		*stbuf++ = buf.st_ino;
		*stbuf++ = buf.st_mode;
		*stbuf++ = buf.st_nlink;
		*stbuf++ = buf.st_uid;
		*stbuf++ = buf.st_gid;
		*stbuf++ = buf.st_rdev;
		*stbuf++ = buf.st_size;		/* may be inaccurate */
		*stbuf++ = buf.st_atime;	/* ??? - this compiles??? */
		*stbuf++ = buf.st_mtime;	/* ??? */
		*stbuf++ = buf.st_ctime;	/* ??? */
		*stbuf++ = buf.st_blksize;	/* ??? */
#ifdef KEY /* Bug 1683 */
		*stbuf++ = buf.st_blocks;
#endif /* KEY Bug 1683 */
		retval	= 0;
	}

	STMT_END(cup, T_INQU, NULL, &cfs);	/* unlock the unit */
	return(retval);
}

int 
__fstat64_f90(int *u, _f_int8 *stbuf)
{
	return fstatf90_4_4_8_(u,stbuf);
}

_f_int 
fstatf90_4_4_8_(_f_int *u, _f_int8 *stbuf)
{
	int 		n, retval;
	unum_t 		unum;
	unit		*cup;
#if	defined(_LITTLE_ENDIAN) && !defined(__sv2)
	struct stat 	buf;
#else
	struct stat64 	buf;
#endif
	struct fiostate	cfs;

	unum = *u;
	STMT_BEGIN(unum, 0, T_INQU, NULL, &cfs, cup);

	if (cup == NULL && !GOOD_UNUM(unum))
		_ferr(&cfs, FEIVUNIT, unum);	/* invalid unit number */

	if (cup == NULL)
		retval	= -1;		/* unit is not open */
	else if (cup->usysfd == -1)
		retval	= -1;		/* file is not disk-resident */
	else {
		n	= fstat(cup->usysfd, &buf);
		if (n < 0)
			_ferr(&cfs, errno);
		*stbuf++ = buf.st_dev;
		*stbuf++ = buf.st_ino;
		*stbuf++ = buf.st_mode;
		*stbuf++ = buf.st_nlink;
		*stbuf++ = buf.st_uid;
		*stbuf++ = buf.st_gid;
		*stbuf++ = buf.st_rdev;
		*stbuf++ = buf.st_size;		/* may be inaccurate */
		*stbuf++ = buf.st_atime;	/* ??? - this compiles??? */
		*stbuf++ = buf.st_mtime;	/* ??? */
		*stbuf++ = buf.st_ctime;	/* ??? */
		*stbuf++ = buf.st_blksize;	/* ??? */
#ifdef KEY /* Bug 1683 */
		*stbuf++ = buf.st_blocks;
#endif /* KEY Bug 1683 */
		retval	= 0;
	}

	STMT_END(cup, T_INQU, NULL, &cfs);	/* unlock the unit */
	return(retval);
}

_f_int8
fstatf90_8_(_f_int8 *u, _f_int8 *stbuf)
{
	int 		n, retval;
	unum_t 		unum;
	unit		*cup;
#if	defined(_LITTLE_ENDIAN) && !defined(__sv2)
	struct stat 	buf;
#else
	struct stat64 	buf;
#endif
	struct fiostate	cfs;

	unum = *u;
	STMT_BEGIN(unum, 0, T_INQU, NULL, &cfs, cup);

	if (cup == NULL && !GOOD_UNUM(unum))
		_ferr(&cfs, FEIVUNIT, unum);	/* invalid unit number */

	if (cup == NULL)
		retval	= -1;		/* unit is not open */
	else if (cup->usysfd == -1)
		retval	= -1;		/* file is not disk-resident */
	else {
		n	= fstat(cup->usysfd, &buf);
		if (n < 0)
			_ferr(&cfs, errno);
		*stbuf++ = buf.st_dev;
		*stbuf++ = buf.st_ino;
		*stbuf++ = buf.st_mode;
		*stbuf++ = buf.st_nlink;
		*stbuf++ = buf.st_uid;
		*stbuf++ = buf.st_gid;
		*stbuf++ = buf.st_rdev;
		*stbuf++ = buf.st_size;		/* may be inaccurate */
		*stbuf++ = buf.st_atime;	/* ??? - this compiles??? */
		*stbuf++ = buf.st_mtime;	/* ??? */
		*stbuf++ = buf.st_ctime;	/* ??? */
		*stbuf++ = buf.st_blksize;	/* ??? */
#ifdef KEY /* Bug 1683 */
		*stbuf++ = buf.st_blocks;
#endif /* KEY Bug 1683 */
		retval	= 0;
	}

	STMT_END(cup, T_INQU, NULL, &cfs);	/* unlock the unit */
	return( (_f_int8)retval );
}
