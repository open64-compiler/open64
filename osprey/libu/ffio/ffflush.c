/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
 */

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


#pragma ident "@(#) libu/ffio/ffflush.c	92.3	10/11/99 15:30:43"

#include <errno.h>
#include <stdarg.h>
#include <ffio.h>

/*
 * This module contains a driver for the flush function.
 *
 */

int
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
ffflush(int fd, struct ffsw *stat)
#else
ffflush(int fd, ...)
#endif
	{
	register int	ret;
	struct fdinfo	*fio;
	struct ffsw	*pstat;
#ifdef	_CRAY
	register int	na;
	struct ffsw	locstat;
	va_list		ap;

	NUMARG(na);

	if (na < 2)
		pstat	= &locstat;
	else
		{
		va_start(ap, fd);
		pstat	= va_arg(ap, struct ffsw *);
		}
#else
	pstat	= stat;
#endif

	fio	= GETIOB(fd);
	_PRAGMA_INLINE(__ffflush)
	ret	= __ffflush(fio, pstat);

	/* set errno only if stat was not passed */

#ifdef	_CRAY
	if (na < 2)
		errno	= locstat.sw_error;
#endif

	return (ret);
	}

int
__ffflush(struct fdinfo *fio, struct ffsw *pstat)
{
	register int	ret;

	CHECK_FIOPTR(fio, pstat);
	ret	= XRCALL(fio, flushrtn) fio, pstat);
	return (ret);
}

/*
 * _ff_flush()
 * Generic flush routine.  If no special handling is required.
 * It simply writes out the buffer and quits.  If in read mode,
 * it resets the buffer pointers.
 */

int
_ff_flush(struct fdinfo *fio, struct ffsw *stat)
	{
	int	ubc;
	ssize_t	wret;

	if(fio->rwflag == WRITIN)
		{
		ubc	= (8 - (fio->_cnt & 7)) & 7;
		if (fio->_cnt > 0)
			{
			WRITEBLK(wret, fio, (size_t)(fio->_cnt >> 3), stat, FULL, &ubc);
			if (wret < 0)
				return(-1);
			}
		}
	else
		{
/*
 *		If in read mode, simply throw away anything already in the
 *		buffer.
 */
		fio->_ptr	= fio->_base;
		fio->_cnt	= 0;
		fio->segbits	= 0;
		fio->last_recbits = fio->recbits;
		fio->recbits	= 0;
		fio->scc	= FULL;
		fio->ateof	= NO;
		fio->ateod	= NO;
		fio->lastscc	= FULL;
		fio->rwflag	= POSITIN;
		}

	return (0);
	}
