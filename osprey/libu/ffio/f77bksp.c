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


#pragma ident "@(#) libu/ffio/f77bksp.c	92.3	11/15/99 17:18:18"
 
#include <ffio.h>
#include "f77io.h"

static int zero = 0;

/*
 * backspace an f77 file
 * Parameters:
 *	fd	- file descriptor (dummy)
 *	stat	- pointer to status return word
 *
 * Returns:
 *	0 on success sw_error is EOR, EOF, or BOD
 *	-1 on error with sw_error containing error code
 */

_f77_xbksp(struct fdinfo *fio, struct ffsw *stat)
{
	struct fdinfo *llfio;
	ssize_t rret;
	int ret, whence;
	ff_offt pos, cwpos;
	long ii;
	struct f77_xf *xf_info;
	char *cb;
	int ijk;

	xf_info = (struct f77_xf *)fio->lyr_info;

/*
 *	If the file's been reading, determine whether our desired position is
 *	within the buffer.
 */
	if (fio->rwflag == READIN) {
		/* We want to be at last_lrdwaddr. However, we need to read the */
		/* control word before last_lrdwaddr. */
		/* Our current position is lrdwaddr  - rembytes */
		off_t offset;
		char *tmpptr;
		offset = xf_info->lrdwaddr -  xf_info->rembytes -
			xf_info->last_lrdwaddr;
		if ((xf_info->_ptr - xf_info->_base) >=	(offset+RDWLEN)){
			/* desired position is within the buffer */
			xf_info->lrdwaddr -= offset;
			xf_info->_ptr -= offset;
			if (xf_info->lrdwaddr == 0) {
				xf_info->last_lrdwaddr = 0;
			}
			else {
				/* read the trailing control word from the previous */
				/* record. */
				ii = 0;
				cb = (char *)&ii;
				tmpptr = xf_info->_ptr - RDWLEN;
				for (ijk = 0; ijk < RDWLEN; ijk++) {
					*cb++ = *tmpptr++;
				}
#if	!(defined(_LITTLE_ENDIAN) && defined(_LP64))
				ii = (unsigned long)ii >> ((sizeof(ii) -4) *8);
#endif
				if (fio->rtype == TR_UX_MIPS) SWAPB(ii);
				xf_info->last_lrdwaddr = xf_info->lrdwaddr - ii - 2*RDWLEN;
			}
			xf_info->_cnt += offset;
			xf_info->flag |= ATEOR;
			xf_info->recbytes = 0;
			xf_info->rembytes = 0;
			fio->ateod = 0;
			/* Leave flag as READIN, since we have data in the buffer */
			return(0);
		}
		else {
			/* desired position is not in the buffer, we can just drop */
			/* through */
		}
	}
/*
 *	If the file's been writing, flush previously written data to disk and
 *	truncate.
 */
	if (fio->rwflag == WRITIN) {
		ret = XRCALL(fio, flushrtn) fio, stat);
        	if (ret < 0) return(ERR);

		ret = XRCALL(fio, weodrtn) fio, stat);
        	if (ret < 0) return(ERR);
	}
	
	llfio = fio->fioptr;
	
	pos = xf_info->last_lrdwaddr;
/*
 *	Set up pos and rdw length
 */
	cwpos = pos;
	cwpos -= RDWLEN;
	if (cwpos < 0) cwpos = 0;
/*
 *	Seek to last RDW
 */
	whence = 0;
	if (XRCALL(llfio, seekrtn) llfio, cwpos, whence, stat) < 0)
		return(ERR);
/*
 *	Update current position
 */
	xf_info->lrdwaddr = pos;
/*
 *	read up the control info
 */
	xf_info->_ptr = xf_info->_base;
	rret = XRCALL(fio->fioptr, readrtn)fio->fioptr, CPTR2BP(xf_info->_ptr), RDWLEN,
		stat, PARTIAL, &zero);
	xf_info->_cnt = 0;
	if (rret < 0)
		ERETURN(stat, FDC_ERR_UXEND, 0);
	if (zero != 0)
		ERETURN(stat, FDC_ERR_FMT, 0);
	xf_info->_cnt = rret;
	if (pos == 0) {
		xf_info->last_lrdwaddr = 0;
		xf_info->lrdwaddr = 0;
	}
	else {
/*
 *		Read up the trailing control word from the
 *		previous record.
 */
		ii = 0;
		cb = (char *)&ii;
		for (ijk = 0; ijk < RDWLEN; ijk++) {
			*cb++ = *xf_info->_ptr++;
		}

#if	!(defined(_LITTLE_ENDIAN) && defined(_LP64))
		ii = (unsigned long)ii >> ((sizeof(ii) - 4 ) * 8);
#endif
		if (fio->rtype == TR_UX_MIPS) SWAPB(ii);
/*
 *		Update ptr to previous record
 */
		xf_info->last_lrdwaddr = (pos - ii) - (2 * RDWLEN);
	}
/*
 *	now seek back again.
 */
	if (XRCALL(llfio, seekrtn) llfio, pos, whence, stat) < 0)
		return(ERR);

	fio->rwflag = POSITIN;
	xf_info->_ptr = xf_info->_base;
	xf_info->_cnt = 0;
	xf_info->recbytes = 0;
	xf_info->rembytes = 0;
	fio->ateod = 0;
	return(0);
}
