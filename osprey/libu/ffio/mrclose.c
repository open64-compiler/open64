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


#pragma ident "@(#) libu/ffio/mrclose.c	92.1	06/29/99 13:16:47"

#include <stdio.h>
#include <fcntl.h>
#include <malloc.h>
#include <ffio.h>
#include "fssio.h"

extern void _mr_clfree();

/*
 *	close Memory Resident file
 *      Returns: -1 if error occurred
 *		  0 if OK
 */

_mr_close(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
{
	struct fdinfo *llfio;
	int ret, ret2, ret3, locret;
	int size;
	long tfl;
	struct mr_f *mr_info;
	_lociosw *locptr;
	struct ffsw locstat;
	int i;
	int sverrno;
	struct _loclink *loclink;
	
	llfio = fio->fioptr;
	mr_info = (struct mr_f *)fio->lyr_info;

	ret = ret2 = ret3 = 0;

/*
 *	Wait for asynchronous requests that have not been recalled.
 */
	loclink = mr_info->loclist;
	while (loclink != NULL) {
		locptr = loclink->loc_first;
		for (i = 0; i < _FSSASYNUM; i++) {
			if (locptr->user_sw != NULL) {
				/* wait for it */
				locret = XRCALL(llfio, fcntlrtn)
					llfio, FC_RECALL, &locptr->local_sw,
					&locstat);
				if (locret < 0) {
					sverrno = locstat.sw_error;
					ret2 = ERR;
				}
			}
			locptr++;	
		}
		loclink = loclink->loc_nxt;
	}
	
	if (fio->rtype == TR_FSS_SCR) { /* Scratch, Toss it... */
		if (llfio != NULL) {
			ret3 = XRCALL(llfio, closertn) llfio, stat);
		}
	}
	else if (fio->rtype == TR_FSS_SAVE) {
/*
 *		Flush/release buffer
 */
		tfl = fio->opn_flags &
				(O_RDWR | O_RDONLY | O_WRONLY | O_APPEND);
		if (tfl != O_RDONLY)
			{
/*
 *			Unload the amount of data in the MR buffer.
 *			Account for overflow.
 */
			size = mr_info->mreof;
			if (size > mr_info->mrsize) size = mr_info->mrsize;
			ret = _mr_unload(fio, size, stat);
			}
		ret3 = XRCALL(llfio, closertn) llfio, stat);
		}
	else				/* CACHE not done yet... */
		ERETURN(stat, FDC_ERR_INTERR, 0);

	_mr_clfree(fio);

	if ((ret | ret2 |ret3) == 0) return(0);
	if (ret2 == ERR)
		SETSTAT(stat, sverrno, 0);
	return(ERR);
}

/*
 * _mr_clfree()
 *	Free the memory blocks used by the mr layer.
 */
void
_mr_clfree(fio)
struct fdinfo *fio;
{
	struct mr_f *mr_info;
	struct _loclink *loclink;
	struct _loclink *locold;

	mr_info = (struct mr_f *)fio->lyr_info;

	if (mr_info->name != NULL) {
		free(mr_info->name);
		mr_info->name = NULL;
	}
	loclink = mr_info->loclist;
	while (loclink != NULL) {
		locold = loclink;
		loclink = loclink->loc_nxt;
		free(locold);
	}
	mr_info->loclist = NULL;
	if(BPTR2CP(fio->_base) != NULL) {
		free(BPTR2CP(fio->_base));
	}
	SET_BPTR(fio->_base, CPTR2BP(NULL));
	if (fio->fioptr != NULL) {
		free(fio->fioptr);
		fio->fioptr = NULL;
	}
	if (fio->lyr_info != NULL) {
		free(fio->lyr_info);
		fio->lyr_info = NULL;
	}
	return;
}

/*
 *	Unload the mr data, write all data to the lower level
 *	This routine is used in close processing and also when doing overflow
 *	handling.  It must not try to close the lower layer(s).  It only
 *	unloads the data from the buffer.
 */
_mr_unload(fio, unlsize, stat)
struct fdinfo *fio;
long unlsize;		/* number of bits to unload */
struct ffsw *stat;
	{
	struct fdinfo *llfio;
	int ret, ubc;
	long pos;
	long bits, bytes;
	struct mr_f *mr_info;

	llfio = fio->fioptr;
	mr_info = (struct mr_f *)fio->lyr_info;

	ret = XRCALL(llfio, seekrtn) llfio, 0, 0, stat);
	if (ret < 0) return(ERR);

	if (mr_info->mrdirty != 0)
		{
		bits = unlsize;
		bytes = (bits + 7) >> 3;
		ubc = bytes*8 - bits;
		ret = XRCALL(llfio, writertn) llfio,
			fio->_base, bytes, stat, PARTIAL,
			&ubc);
		if (ret < 0)
			return(ERR);
/*
 *		Seek to the logical EOF of the file.  This will be
 *		correct regardless of the overflow status.
 */
		pos = (mr_info->mreof + 7) >> 3;
		ret = XRCALL(llfio, seekrtn) llfio, pos, 0, stat);
		if (ret < 0) return(ERR);

		ret = XRCALL(llfio, weodrtn) llfio, stat);
		if (ret < 0) return(ERR);
		}
	return(0);
	}
