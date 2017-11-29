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


#pragma ident "@(#) libu/ffio/cchweod.c	92.1	06/29/99 13:16:47"

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <ffio.h>
#include "cchio.h"

/*
 * Write an EOD to a cache file.
 *
 *	Truncate this layer at the current position.
 */
int
_cch_weod(
struct fdinfo	*fio,
struct ffsw	*stat)
{
	int		nbu, i;
	off_t		fsize;
	int		bs;
	struct cch_buf	*cbufs;
	struct cch_f	*cch_info;
	struct fdinfo	*llfio;

	cch_info = (struct cch_f *)fio->lyr_info;

	if ((fio->opn_flags & O_ACCMODE) == O_RDONLY)
		ERETURN(stat, EBADF, 0);

	if (cch_info->is_blkspec)
		ERETURN(stat, FDC_ERR_NOSUP, 0);

	cch_info->fsize = cch_info->cpos;

	fio->rwflag = WRITIN;
	fio->ateof = 0;
	fio->ateod = 1;
	fio->recbits = 0;
/*
 *	Fix up any cache page buffers for file pages which lie past the new EOD.
 */
	nbu	= cch_info->nbufs;
	bs	= cch_info->bsize;
	cbufs	= cch_info->bufs;
	fsize	= cch_info->fsize;

	for (i=0; i<nbu; i++) {
		off_t filead = cbufs[i].filead;
		if (filead >= 0) {

			/* If page is past EOD then mark it free */
			if (filead >= fsize)
				cbufs[i].filead = -1;

			/* If page straddles EOD then zero out part of it */
			else if (filead < fsize && filead + bs > fsize) {
				int valid_bytes = BITS2BYTES(fsize - filead);
#ifdef CCH_SDS_SUPPORTED
				if (cch_info->optflags & CCHOPT_SDS) {
				    int sds_offset;
				    int res;
				    sds_offset   = BPTR2CP(cbufs[i].buf) -
							(char *)NULL;
	
				    res = _sdsset(
					sds_offset + valid_bytes,
					0,
					BITS2BYTES(bs) - valid_bytes);

				    if (res == -1)
					ERETURN(stat, errno, 0);
				}
				else
#endif
				{
				    (void)memset(
					BPTR2CP(cbufs[i].buf) + valid_bytes,
					0,
					BITS2BYTES(bs) - valid_bytes);
				}
			}
		}
	}
/*
 *	Truncate the underlying layer at the same location.   For most layers,
 * 	this ensures that data past this EOD becomes zero if the underlying file
 *	is later extended such that a hole is left between the this EOD
 *	and the data written later.
 */
	llfio  = fio->fioptr;
	if (fsize < cch_info->feof) {

		if (XRCALL(llfio,seekrtn) llfio, BITS2BYTES(fsize), SEEK_SET,
					  stat) == ERR)
			return(ERR);

		if (XRCALL(llfio,weodrtn) llfio, stat) == ERR)
			return(ERR);

		cch_info->feof = fsize;
	}

	SETSTAT(stat, FFEOD, 0);
	return(0);
}
