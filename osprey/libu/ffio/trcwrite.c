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


#pragma ident "@(#) libu/ffio/trcwrite.c	92.1	06/29/99 13:16:47"

#include <ffio.h>
#include "trcio.h"

/*
 * trace write requests
 *
 * Parameters:
 *      fio     - Pointer to fdinfo block
 *	bufptr	- bit pointer to where data is to go.
 *	nbytes	- Nuber of bytes to be written
 *	stat	- pointer to status return word
 *	fulp	- full or partial write mode flag
 *	ubc	- pointer to unused bit count (not used for IBM)
 */
_trc_write(fio, bufptr, nbytes, stat, fulp, ubc)
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
	_trc_enter(fio, TRC_WRITE);
	_trc_pr_rww(fio, bufptr, nbytes, stat, fulp, ubc);
	ret = XRCALL(llfio, writertn) llfio, bufptr, nbytes, stat, fulp, ubc);

	UPDPOS(fio, ret, writes);
	trc_info->lastseek = NO;

	_trc_exit(fio, ret, stat);
	return(ret);
	}

/*
 * trace writea requests
 *
 * Parameters:
 *      fio     - Pointer to fdinfo block
 *	bufptr	- bit pointer to where data is to go.
 *	nbytes	- Nuber of bytes to be written
 *	stat	- pointer to status return word
 *	fulp	- full or partial write mode flag
 *	ubc	- pointer to unused bit count (not used for IBM)
 */
_trc_writea(fio, bufptr, nbytes, stat, fulp, ubc)
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
	_trc_enter(fio, TRC_WRITEA);
	_trc_pr_rww(fio, bufptr, nbytes, stat, fulp, ubc);
	ret = XRCALL(llfio, writeartn) llfio, bufptr, nbytes, stat, fulp, ubc);

	UPDPOS(fio, ret, writeas);
	trc_info->lastseek = NO;

	_trc_exit(fio, ret, stat);
	return(ret);
	}

/*
 * trace writec requests
 *
 * Parameters:
 *      fio     - Pointer to fdinfo block
 *	bufptr	- bit pointer to where data is to go.
 *	nbytes	- Nuber of bytes to be written
 *	stat	- pointer to status return word
 *	fulp	- full or partial write mode flag
 */
_trc_writec(fio, bufptr, nbytes, stat, fulp)
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
	_trc_enter(fio, TRC_WRITEC);
	_trc_pr_rwc(fio, bufptr, nbytes, stat, fulp);
	ret = XRCALL(llfio, writecrtn)llfio,bufptr, nbytes, stat, fulp);

	UPDPOS(fio, ret, writes);
	trc_info->lastseek = NO;

	_trc_exit(fio, ret, stat);
	return(ret);
	}

/*
 * Flush the buffer and clean up
 *	This routine should return 0, or -1 on error.
 */
int
_trc_flush(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
	{
	struct fdinfo *llfio;
	struct trace_f *trc_info;
	int ret;

	llfio = fio->fioptr;
	trc_info = (struct trace_f *)fio->lyr_info;
	_trc_enter(fio, TRC_FLUSH);
	ret = XRCALL(llfio, flushrtn) llfio, stat);
	trc_info->lastseek = NO;
	_trc_exit(fio, ret, stat);
	return(ret);
	}

/*
 * trace WEOF calls
 */
int
_trc_weof(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
	{
	struct fdinfo *llfio;
	struct trace_f *trc_info;
	int ret;

	llfio = fio->fioptr;
	trc_info = (struct trace_f *)fio->lyr_info;
	_trc_enter(fio, TRC_WEOF);
	ret = XRCALL(llfio, weofrtn) llfio, stat);
	trc_info->lastseek = NO;
	_trc_exit(fio, ret, stat);
	return(ret);
	}

/*
 * trace WEOD calls
 */
int
_trc_weod(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
	{
	struct fdinfo *llfio;
	struct trace_f *trc_info;
	int ret;

	llfio = fio->fioptr;
	trc_info = (struct trace_f *)fio->lyr_info;
	_trc_enter(fio, TRC_WEOD);
	ret = XRCALL(llfio, weodrtn) llfio, stat);
	trc_info->lastseek = NO;
	_trc_exit(fio, ret, stat);
	return(ret);
	}
