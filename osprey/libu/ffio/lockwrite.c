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


#pragma ident "@(#) libu/ffio/lockwrite.c	92.1	06/29/99 13:16:47"

#include <ffio.h>
#include "locklyr.h"

ssize_t
_lock_write(struct fdinfo *fio, bitptr bufptr, size_t nbytes, struct ffsw *stat,
int fulp, int *ubc)
{
	struct fdinfo *llfio;
	ssize_t ret;

	llfio = fio->fioptr;
	LYR_LOCK(fio);
	ret = XRCALL(llfio, writertn) llfio, bufptr, nbytes, stat, fulp, ubc);
	LYR_UNLOCK(fio);

	return(ret);
}

ssize_t
_lock_writea(struct fdinfo *fio, bitptr bufptr, size_t nbytes,
struct ffsw *stat, int fulp, int *ubc)
{
	struct fdinfo *llfio;
	ssize_t ret;

	llfio = fio->fioptr;
	LYR_LOCK(fio);
	ret = XRCALL(llfio, writeartn) llfio, bufptr, nbytes, stat, fulp, ubc);
	LYR_UNLOCK(fio);

	return(ret);
}


ssize_t
_lock_writec(struct fdinfo *fio, bitptr bufptr, size_t nbytes,
struct ffsw *stat, int fulp)
{
	struct fdinfo *llfio;
	ssize_t ret;

	llfio = fio->fioptr;
	LYR_LOCK(fio);
	ret = XRCALL(llfio, writecrtn)llfio,bufptr, nbytes, stat, fulp);
	LYR_UNLOCK(fio);

	return(ret);
}

int
_lock_flush(struct fdinfo *fio, struct ffsw *stat)
{
	struct fdinfo *llfio;
	int ret;

	llfio = fio->fioptr;
	LYR_LOCK(fio);
	ret = XRCALL(llfio, flushrtn) llfio, stat);
	LYR_UNLOCK(fio);
	return(ret);
}

int
_lock_weof(struct fdinfo *fio, struct ffsw *stat)
{
	struct fdinfo *llfio;
	int ret;

	llfio = fio->fioptr;
	LYR_LOCK(fio);
	ret = XRCALL(llfio, weofrtn) llfio, stat);
	LYR_UNLOCK(fio);
	return(ret);
}

int
_lock_weod(struct fdinfo *fio, struct ffsw *stat)
{
	struct fdinfo *llfio;
	int ret;

	llfio = fio->fioptr;
	LYR_LOCK(fio);
	ret = XRCALL(llfio, weodrtn) llfio, stat);
	LYR_UNLOCK(fio);
	return(ret);
}

