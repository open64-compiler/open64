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


#pragma ident "@(#) libf/pxf/pxffcntl.c	92.1	06/29/99 11:36:06"
#include <fortran.h>
#include <sys/types.h>
#include <fcntl.h>
#include <errno.h>
#include <liberrno.h>
#include <malloc.h>   /* pickup NULL definition */
#include "pxfstruct.h"
#include "table.h"

/*
 * Description:
 *	PXFFCNTL provides the functionality of fcntl(2). Currently,
 *		the only commands allowed are only F_GETLK, F_SETLK
 *		and F_SETLKW.
 * Standard:
 *	Section 6.5.2 of Posix 1003.9-1992
 * Parameters:
 *	ifildes	(input)	file descriptor
 *	icmd	(input) command
 *	iargin	(input)	command specific
 *	iargout (output) return value
 *	ierror  (output) error code
 */

#ifdef _UNICOS
void
PXFFCNTL(
#else
void
_PXFFCNTL(
#endif
	  _f_int *ifildes,
	  _f_int *icmd,
	  _f_int *iargin,
	  _f_int *iargout,
	  _f_int *ierror)
{
	int ret;
	struct pxfhandle pxfhand;

	*ierror = 0;

	switch(*icmd) {
		case F_GETLK:	
		case F_SETLK:	
		case F_SETLKW:	
		        pxfhand = _pxfhandle_table_lookup(&_pxfhandle_table,
							  *iargin);
			if (pxfhand.pxfstructptr == NULL ||
			    pxfhand.pxftype != PXF_FLOCK) {
			  *ierror = EBADHANDLE;
			  return;
			}

			ret = fcntl(*ifildes, *icmd,
				    (struct flock *)pxfhand.pxfstructptr);
			if (ret < 0) {
				*ierror = errno;
			}
			*iargout = (_f_int)ret;
			break;
		case F_DUPFD:
		case F_SETFD:
		case F_GETFD:
		case F_GETFL:
		case F_SETFL:
#ifdef _UNICOS
		case F_SETSB:
		case F_SETALF:
		case F_CLRALF:
#endif
			ret = fcntl(*ifildes, *icmd, *iargin);
			if (ret < 0) {
				*ierror = errno;
			}
			*iargout = (_f_int)ret;
			break;
		default:
			*ierror = EINVAL;
	}
}

#ifndef _UNICOS
void
pxffcntl_(
	  _f_int *ifildes,
	  _f_int *icmd,
	  _f_int *iargin,
	  _f_int *iargout,
	  _f_int *ierror)
{
  _PXFFCNTL(ifildes,icmd,iargin,iargout,ierror);
}
#endif
