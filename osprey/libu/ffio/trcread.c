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


#pragma ident "@(#) libu/ffio/trcread.c	92.1	06/29/99 13:16:47"

#include <ffio.h>
#include "trcio.h"

/*
 * trace read requests
 *
 * Parameters:
 *      fio     - Pointer to fdinfo block
 *	bufptr	- bit pointer to where data is to go.
 *	nbytes	- Nuber of bytes to be read
 *	stat	- pointer to status return word
 *	fulp	- full or partial read mode flag
 *	ubc	- pointer to unused bit count
 */
_trc_read(fio, bufptr, nbytes, stat, fulp, ubc)
struct fdinfo *fio;
int nbytes, fulp, *ubc;
bitptr bufptr;
struct ffsw *stat;
	{
	struct fdinfo *llfio;
	struct trace_f *trc_info;
	int ret;

	llfio = fio->fioptr;
	trc_info = (struct trace_f *)fio->lyr_info;
	_trc_enter(fio, TRC_READ);
	_trc_pr_rww(fio, bufptr, nbytes, stat, fulp, ubc);
	ret = XRCALL(llfio, readrtn) llfio, bufptr, nbytes, stat, fulp, ubc);

	UPDPOS(fio, ret, reads);
	trc_info->lastseek = NO;

	_trc_exit(fio, ret, stat);
	return(ret);
	}

/*
 * trace reada requests
 *
 * Parameters:
 *      fio     - Pointer to fdinfo block
 *	bufptr	- bit pointer to where data is to go.
 *	nbytes	- Nuber of bytes to be read
 *	stat	- pointer to status return word
 *	fulp	- full or partial read mode flag
 *	ubc	- pointer to unused bit count
 */
_trc_reada(fio, bufptr, nbytes, stat, fulp, ubc)
struct fdinfo *fio;
int nbytes, fulp, *ubc;
bitptr bufptr;
struct ffsw *stat;
	{
	struct fdinfo *llfio;
	struct trace_f *trc_info;
	int ret;

	llfio = fio->fioptr;
	trc_info = (struct trace_f *)fio->lyr_info;
	_trc_enter(fio, TRC_READA);
	_trc_pr_rww(fio, bufptr, nbytes, stat, fulp, ubc);
	ret = XRCALL(llfio, readartn) llfio, bufptr, nbytes, stat, fulp, ubc);

	UPDPOS(fio, nbytes, readas);
	trc_info->lastseek = NO;

	_trc_exit(fio, ret, stat);
	return(ret);
	}

/*
 * trace readc requests
 *
 * Parameters:
 *      fio     - Pointer to fdinfo block
 *	bufptr	- bit pointer to where data is to go.
 *	nbytes	- Nuber of bytes to be read
 *	stat	- pointer to status return word
 *	fulp	- full or partial read mode flag
 */
_trc_readc(fio, bufptr, nbytes, stat, fulp)
struct fdinfo *fio;
int nbytes, fulp;
bitptr bufptr;
struct ffsw *stat;
	{
	struct fdinfo *llfio;
	struct trace_f *trc_info;
	int ret;

	llfio = fio->fioptr;
	trc_info = (struct trace_f *)fio->lyr_info;
	_trc_enter(fio, TRC_READC);
	_trc_pr_rwc(fio, bufptr, nbytes, stat, fulp);
	ret = XRCALL(llfio, readcrtn)llfio, bufptr, nbytes, stat, fulp);

	UPDPOS(fio, ret, reads);
	trc_info->lastseek = NO;

	_trc_exit(fio, ret, stat);
	return(ret);
	}

/*
 * _trc_seek()
 */
_trc_seek(fio, pos, whence, stat)
struct fdinfo *fio;
int pos, whence;
struct ffsw *stat;
	{
	struct fdinfo *llfio;
	struct trace_f *trc_info;
	int ret;

	llfio = fio->fioptr;
	trc_info = (struct trace_f *)fio->lyr_info;
	_trc_enter(fio, TRC_SEEK);
	_trc_info(fio, "pos=%d, ", pos);
	_trc_info(fio, "whence=%d\n", whence);

	ret = XRCALL(llfio, seekrtn) llfio, pos, whence, stat);

	if (ret >= 0) trc_info->curpos = ret;

	trc_info->lseeks++;
	if ((whence == 0) || (whence == 1  && pos != 0) || (whence == 2))
		trc_info->lastseek = YES;

	_trc_exit(fio, ret, stat);
	return(ret);
	}
