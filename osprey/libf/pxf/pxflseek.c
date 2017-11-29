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


#pragma ident "@(#) libf/pxf/pxflseek.c	92.3	06/29/99 11:36:06"

/*
 *  PXFLSEEK -- Reposition READ/WRITE File Offset
 *             (section 6.5.3 of Posix 1003.9-1992)
 *
 *  Synopsis:
 *
 *     SUBROUTINE PXFLSEEK(ifildes, ioffset, iwhence, iposition, ierror)
 *     INTEGER ifildes, ioffset, iwhence, iposition, ierror
 *
 *  Description:
 *
 *  PXFLSEEK uses the lseek() system call to move the read/write 
 *          file pointer.
 *
 *  The arguments are:
 *
 *      ifildes   -  default integer input variable containing a file
 *                   descriptor.
 *      ioffset   -  default input integer variable containing the
 *                   number of bytes associated with the pointer's
 *                   new location.
 *      iwhence   -  default input integer variable specifying the
 *                   value to indicate the pointer's new location.
 *
 *                   0 or SEEK_SET  - set pointer to ioffset bytes.
 *                   1 or SEEK_CUR  - set pointer to current location
 *                                    plus ioffset.
 *                   2 or SEEK_END  - set pointer to the file size
 *                                    plus ioffset.
 *
 *      iposition -  default output integer variable containing the
 *                   file pointer value as measured in bytes from
 *                   the beginning of the file.
 *      ierror    -  default integer output variable that contains zero
 *                   if the operation was successful or nonzero if the
 *                   operation was not successful.
 *
 *   PXFLSEEK may return one of the following error values:
 *
 *   EBADF      If ifildes is not a valid open file descriptor.
 *
 *   EINVAL     If the call contains an argument that is not valid.
 *              The iwhence argument is incorrect or the resulting
 *              file pointer would be negative.
 *
 *   ESPIPE     If the ifildes is associated with a FIFO special file.
 *
 */

#include <errno.h>
#include <fortran.h>
#include <liberrno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/errno.h>
#include <sys/types.h>
#include <unistd.h>

#ifdef _UNICOS
void
PXFLSEEK(
#else	/* _UNICOS */
void
pxflseek_(
#endif	/* _UNICOS */
	_f_int	*ifildes,
	_f_int	*ioffset,
	_f_int	*iwhence,
	_f_int	*iposition,
	_f_int	*ierror)
{
	_f_int	in_whence;
	off_t	errsts	= 0;
	off_t	in_offset;
	off_t	out_pos;

	*ierror	= 0;
	*iposition	= 0;
	in_whence	= *iwhence;
	in_offset	= *ioffset;
	out_pos	= 0;

	if (out_pos = lseek(*ifildes, in_offset, in_whence) == -1)
		errsts	= errno;
	else
		*iposition	= (_f_int)out_pos;
	*ierror	= (_f_int)errsts;
	return;
}

#ifndef _UNICOS
#ifdef	__mips
/* assume integer(kind=8). off64_t and lseek64 exist.  */
void
pxflseek64_(
	_f_int8	*ifildes,
	_f_int8	*ioffset,
	_f_int8	*iwhence,
	_f_int8	*iposition,
	_f_int8	*ierror)
{
	int	in_whence;
	int	in_fildes;
	off64_t	errsts	= 0;
	off64_t	in_offset;
	off64_t	out_pos;

	*ierror	= 0;
	*iposition	= 0;
	in_fildes	= *ifildes;
	in_offset	= *ioffset;
	in_whence	= *iwhence;
	out_pos	= 0;

	if (out_pos = lseek64(in_fildes, in_offset, in_whence) == -1)
		errsts	= errno;
	else
		*iposition	= (_f_int8)out_pos;
	*ierror	= (_f_int8)errsts;
	return;
}
#else	/* else not mips but not PVP */
/* assume integer(kind=8).  off64_t and lseek64 do not exist. */
void
pxflseek64_(
	_f_int8	*ifildes,
	_f_int8	*ioffset,
	_f_int8	*iwhence,
	_f_int8	*iposition,
	_f_int8	*ierror)
{
	int	in_whence;
	int	in_fildes;
	off_t	errsts	= 0;
	off_t	in_offset;
	off_t	out_pos;

	*ierror	= 0;
	*iposition	= 0;
	in_fildes	= *ifildes;
	in_offset	= *ioffset;
	in_whence	= *iwhence;
	out_pos	= 0;

	if (out_pos = lseek(in_fildes, in_offset, in_whence) == -1)
		errsts	= errno;
	else
		*iposition	= (_f_int8)out_pos;
	*ierror	= (_f_int8)errsts;
	return;
}
#endif	/* end mips and not mips */
#elif	defined(_UNICOS) && defined(_CRAYMPP)
/* assume integer(kind=4) arguments and default 64-bit integer */
void
PXFLSEEK32(
	_f_int4	*ifildes,
	_f_int4	*ioffset,
	_f_int4	*iwhence,
	_f_int4	*iposition,
	_f_int4	*ierror)
{
	_f_int	ifildes8;
	_f_int	newoffset;
	_f_int	newwhence;
	_f_int	newposition;
	_f_int	ierror8;
	ifildes8	= *ifildes;
	newoffset	= *ioffset;
	newwhence	= *iwhence;
	newposition	= *iposition;
	PXFLSEEK(&ifildes8, &newoffset, &newwhence, &newposition, &ierror8);
	*iposition	= newposition;
	*ierror	= ierror8;
}
#endif	/* end _UNICOS and _CRAYMPP */
