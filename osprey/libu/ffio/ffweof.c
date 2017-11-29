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


#pragma ident "@(#) libu/ffio/ffweof.c	92.2	10/11/99 15:30:43"

#include <errno.h>
#include <stdarg.h>
#include <ffio.h>

/*
 * Write an EOF on the file.  Some of the layers have representations
 *	for an End-Of-File, others don't.  Do whatever is appropriate.
 */
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
ffweof(int fd)
#else
ffweof(int fd, ...)
#endif
	{
	struct fdinfo *fio;
	int ret, na;
	struct ffsw locstat, *pstat;
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
	va_list ap;
#endif

	fio = GETIOB(fd);
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
	na = 1;
	pstat = &locstat;
#else
	NUMARG(na);
	if (na < 2)
		pstat = &locstat;
	else
		{
		va_start(ap, fd);
		pstat = va_arg(ap, struct ffsw *);
		}
#endif
	CHECK_FIOPTR(fio, pstat);
	ret = XRCALL(fio, weofrtn) fio, pstat);
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
	if (na < 2)
		errno = locstat.sw_error;
#endif
	return (ret);
	}
/*
 * This routine is like ffweof, except it expects 2 parameters
 */
ffweoff(int fd, struct ffsw *pstat)
{
	struct fdinfo *fio;
	int ret;

	fio = GETIOB(fd);
	CHECK_FIOPTR(fio, pstat);
	ret = XRCALL(fio, weofrtn) fio, pstat);
	return (ret);
}

/*
 * Write an EOD on the file.  This usually amounts to truncating the file.
 *	The EOD is the 'hard' end of the file.  File space does not exist
 *	past this point.
 */
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
ffweod(int fd)
#else
ffweod(int fd, ...)
#endif
	{
	struct fdinfo *fio;
	int ret, na;
	struct ffsw locstat, *pstat;
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
	va_list ap;
#endif

	fio = GETIOB(fd);
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
	na = 1;
	pstat = &locstat;
#else
	NUMARG(na);
	if (na < 2)
		pstat = &locstat;
	else
		{
		va_start(ap, fd);
		pstat = va_arg(ap, struct ffsw *);
		}
#endif
	CHECK_FIOPTR(fio, pstat);
	ret = XRCALL(fio, weodrtn) fio, pstat);
	if (ret < 0)
		{
		if (na < 2)
			errno = locstat.sw_error;
		return(ret);
		}
	return (0);
	}
/*
 * This routine is like ffweod, except it expects 2 parameters
 */
ffweodf(int fd, struct ffsw *pstat)
	{
	struct fdinfo *fio;
	int ret;

	fio = GETIOB(fd);
	CHECK_FIOPTR(fio, pstat);
	ret = XRCALL(fio, weodrtn) fio, pstat);
	if (ret < 0)
		{
		return(ret);
		}
	return (0);
}
/*
 * This is a generic weof routine.  It handles only the simple
 * case where the layer has no representation for endfile.
 * The EOF information is passed down to the next lower layer in the
 * hope that it will handle it.
 */

_ff_weof(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
	{
	struct fdinfo *llfio;
	int ret;

/*
 *	If NOT writing or just positioned, then error.
 */
	if (fio->rwflag == READIN)
		{
		ERETURN(stat, FDC_ERR_WRARD, 0);
		}
	ret = XRCALL(fio, flushrtn) fio, stat);
	if (ret != 0) return(ERR);
	llfio = fio->fioptr;
	ret = XRCALL(llfio, weofrtn) llfio, stat);
	fio->rwflag = WRITIN;
	fio->ateof = 1;
	fio->last_recbits = fio->recbits;
	fio->recbits = 0;
	return (ret);
	}

/*
 * This is a generic weod routine.  It handles only the simple
 * truncation case.  (assuming that the lower levels do truncation.)
 *
 */

_ff_weod(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
	{
	struct fdinfo *llfio;
	int ret;

/*
 *	If NOT writing or just positioned, then error.
 */
	if (fio->rwflag == READIN)
		{
		ERETURN(stat, FDC_ERR_WRARD, 0);
		}
	ret = XRCALL(fio, flushrtn) fio, stat);
	if (ret != 0) return(ERR);
	llfio = fio->fioptr;
	ret = XRCALL(llfio, weodrtn) llfio, stat);
	if (ret < 0) return(ERR);

	fio->rwflag = WRITIN;
	fio->ateod = 1;
	fio->last_recbits = fio->recbits;
	fio->recbits = 0;
	return (0);
	}
