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


#pragma ident "@(#) libf/pxf/pxfmkfifo.c	92.2	06/29/99 11:36:06"

/*
 *  PXFMKFIFO -- Make a FIFO special File
 *               (section 5.4.2 of Posix 1003.9-1992)
 *
 *  Synopsis:
 *
 *     SUBROUTINE PXFMKFIFO(path, ilen, mode, ierror)
 *     INTEGER ilen, mode, ierror
 *     CHARACTER PATH*(*)
 *
 *  Description:
 *
 *  PXFMKFIFO uses the mkfifo() utility to create special first-in,
 *          first-out files.
 *
 *  The arguments are:
 *
 *      path     -  default character input variable containing the
 *                  path name of the FIFO special file to be created.
 *      ilen     -  default input integer variable containing the
 *                  length of path.
 *      mode    -  default input integer variable specifying the
 *                  mode for the new file.
 *      ierror   -  default integer output variable that contains zero
 *                  if the operation was successful or nonzero if the
 *                  operation was not successful.
 *
 *   PXFMKFIFO may return one of the following error values:
 *
 *   EACCES     If search permission is denied for a component of the
 *              path prefix or if write permission is denied to the
 *              parent directory.
 *
 *   EEXIST     If the specified file exists.
 *
 *   EFAULT     If path points outside the allocated process address
 *              space.
 *
 *   EINVAL     If the call contains an argument that is not valid or
 *              the special file would be on an NFS-mounted system.
 *
 *   ENOENT     If component of path does not exist or path is an
 *              empty string.
 *
 *   ENOMEM     If PXFREAD is unable to obtain memory to create an
 *              internal buffer.
 *
 *   ENOTDIR    If component of path is not a directory.
 *
 *
 *   ENXIO      If the device associated with ifildes is a character
 *              special file that does not exist or the file pointer
 *              is out of range.
 *
 *   On PVP systems, PXFMKFIFO may also return:
 *
 *   EFLNEQ     If active security label of calling process is outside
 *              range of the file system on which the file will reside.
 *
 *   EIO        If a physical I/O error has occurred, or the read is
 *              cannot access the device. or If ifildes has O_DIRECT
 *              or FDIRECT set and nbytes is greater than the number
 *              of bytes between the current file pointer position
 *              and the end of file.
 *              
 *   On IRIX systems, PXFMKFIFO may also return:
 *
 *   EROFS      If the parent directory for the new file is on a
 *              read-only system.
 *
 *   ENAMETOOLONG If the length of path or a pathname component
 *              exceeds the maximum allowed.
 *
 *   ENOSPC     If the parent directory cannot be extended because
 *              the file system has no space available.
 *
 *   EDQUOT     If the parent directory cannot be extended because
 *              the disk or inode quota on the file system has been
 *              exhausted.
 *
 */

#include <errno.h>
#include <fortran.h>
#include <liberrno.h>
#include <stdlib.h>
#include <string.h>
#include <sys/errno.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

extern char *_fc_acopy(_fcd f);

#ifdef _UNICOS
void
PXFMKFIFO(
#else	/* _UNICOS */
void
_PXFMKFIFO(
#endif	/* _UNICOS */
	_fcd	path,
	_f_int	*ilen,
	_f_int	*mode,
	_f_int	*ierror)
{
	char	*copy_path_adr;
	char	*path_adr;
	int	errsts	= 0;
	long	copy_ilen;
	long	path_len;
	mode_t	copy_mode;

	copy_ilen	= *ilen;
	*ierror	= 0;
	copy_mode	= (mode_t)*mode;
	path_len	= _fcdlen(path);
	path_adr	= _fcdtocp(path);

	if (copy_ilen < 0 || copy_ilen > path_len)
		errsts	= EINVAL;
	else {
		/* if ilen is zero, strip trailing blanks.
		 * Otherwise, malloc memory and copy the
		 * string and add a NULL terminator.
		 */
		if (copy_ilen == 0)
			copy_path_adr	= _fc_acopy(path);
		else
			copy_path_adr  = (char *) malloc(copy_ilen + 1);
		if (copy_path_adr == NULL)
			errsts	= ENOMEM;
		else {
			if (copy_ilen != 0) {

				/* copy the path argument */
				(void) memcpy(copy_path_adr, path_adr,
					copy_ilen);
				copy_path_adr[copy_ilen]	= '\0';
			}
			if ((errsts = mkfifo(copy_path_adr, copy_mode)) == -1);
				errsts	= errno;
			free(copy_path_adr);
		}
	}
	*ierror	= (_f_int)errsts;
	return;
}

#ifndef _UNICOS
/* assume default integer */
void
pxfmkfifo_(
	char	*path,
	_f_int	*ilen,
	_f_int	*mode,
	_f_int	*ierror,
	int	lenpath)
{
	_PXFMKFIFO(_cptofcd(path, lenpath), ilen, mode, ierror);
}

/* assume integer(kind=8) */
void
pxfmkfifo64_(
	char	*path,
	_f_int8	*ilen,
	_f_int8	*mode,
	_f_int8	*ierror,
	int	lenpath)
{
	_f_int	ierror4;
	_f_int	ilen4;
	_f_int	mode4;

	ilen4	= *ilen;
	mode4	= *mode;
	_PXFMKFIFO(_cptofcd(path, lenpath), &ilen4, &mode4, &ierror4);
	*ierror	= ierror4;
}
#elif	defined(_UNICOS) && defined(_CRAYMPP)
/* assume integer(kind=4) arguments and default 64-bit integer */
void
PXFMKFIFO32(
	_fcd	path,
	_f_int4	*ilen,
	_f_int4	*mode,
	_f_int4	*ierror)
{
	_f_int	ilen8;
	_f_int	mode8;
	_f_int	ierror8;

	ilen8	= *ilen;
	mode8	= *mode;
	PXFMKFIFO(path, &ilen8, &mode8, &ierror8);
	*ierror	= ierror8;
}
#endif	/* end _UNICOS  and _CRAYMPP */
