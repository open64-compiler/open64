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


#pragma ident "@(#) libu/ffio/ffbksp.c	92.2	10/11/99 15:30:43"

#include <errno.h>
#include <stdarg.h>
#include <ffio.h>

int
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
ffbksp(int fd)
#else
ffbksp(int fd, ...)
#endif
	{
	struct fdinfo *fio;
	int ret, na;
	struct ffsw locstat, *pstat;
#ifdef _CRAY
	va_list ap;
#endif

	fio = GETIOB(fd);
#ifdef _CRAY
	NUMARG(na);
	if (na < 2)
		pstat = &locstat;
	else
		{
		va_start(ap, fd);
		pstat = va_arg(ap, struct ffsw *);
		}
#else
	pstat = &locstat;
	na = 1;
#endif
	CHECK_FIOPTR(fio, pstat);
	ret = XRCALL(fio, backrtn) fio, pstat);
	/* set errno only if stat was not passed */
	if (na < 2)
		errno = locstat.sw_error;
	return (ret);
	}

int
ffbkspf(int fd, struct ffsw *pstat)
{
	struct fdinfo *fio;
	fio = GETIOB(fd);
	CHECK_FIOPTR(fio, pstat);
	return(XRCALL(fio, backrtn) fio, pstat));
}
