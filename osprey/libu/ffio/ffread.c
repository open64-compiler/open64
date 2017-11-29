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


#pragma ident "@(#) libu/ffio/ffread.c	92.2	10/11/99 15:30:43"

#include <errno.h>
#include <stdarg.h>
#include <stdlib.h>
#include <ffio.h>
/*
 * This module contains a driver that starts the chain of calls
 * through the foreign dataset control layers.
 *
 */

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
ssize_t
ffread(int fd, void *buf, size_t nbytes)
#elif _USE_OLD_PROTOTYPE == 0
ssize_t
ffread(int fd, void *buf, size_t nbytes, ...)
#else
int
ffread(int fd, char *buf, int nbytes, ...)
#endif
	{
	struct fdinfo *fio;
	ssize_t ret;
	int  locfulp;
	bitptr bufptr;
	int locubc, *pubc, na;	/* need a place to put result */
	struct ffsw locstat, *pstat;	/* need a place to put result */
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
	va_list ap;
#endif

	fio = GETIOB(fd);
	SET_BPTR(bufptr, CPTR2BP(buf));
	/* adjust number of bits requested if ubc passed in */
#ifdef _CRAY
	NUMARG(na);
#elif	defined(__mips) || defined(_LITTLE_ENDIAN)
	na = 3;
#endif
	locubc = 0;
	pubc = &locubc;
	locfulp = FULL;
	pstat = &locstat;
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
	va_start(ap, nbytes);
	if (na < 3 || na > 6)
		{
		errno = FDC_ERR_NOPARM;
		return(ERR);
		}
	if (na > 3)
		pstat = va_arg(ap, struct ffsw *);
	if (na > 4)
		locfulp = va_arg(ap, int);
	if (na > 5)
		pubc = va_arg(ap, int *);
#endif
	CHECK_FIOPTR(fio, pstat);
	ret = XRCALL(fio, readrtn) fio, bufptr, nbytes,
						pstat, locfulp, pubc);
	if (na < 4)
		errno = locstat.sw_error;
	return (ret);
	}
/*
 * This routine is like ffread, except it expects all parameters .
 * If ubc == NULL, then do not return ubc information to user.
 */
ssize_t
ffreadf(int fd, void *buf, size_t nbytes, struct ffsw *pstat, int locfulp,
 int *ubc)
	{
	struct fdinfo *fio;
	ssize_t ret;
	bitptr bufptr;
	int locubc, *pubc;

	
	if (ubc == NULL) {
		pubc = &locubc;
		locubc = 0;
	}
	else {
		pubc = ubc;
	}
	fio = GETIOB(fd);
	SET_BPTR(bufptr, CPTR2BP(buf));
	CHECK_FIOPTR(fio, pstat);
	ret = XRCALL(fio, readrtn) fio, bufptr, nbytes,
						pstat, locfulp, pubc);
	return (ret);
	}
