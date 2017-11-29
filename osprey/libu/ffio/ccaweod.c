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


#pragma ident "@(#) libu/ffio/ccaweod.c	92.1	06/29/99 13:16:47"


#include <stdio.h>
#include <errno.h>
#include <ffio.h>
#include "ccaio.h"


/*
 * Write an EOD to a cache file.
 *
 *	Truncate this layer at the current position.
 */
int
_cca_weod(
struct fdinfo	*fio,
struct ffsw	*stat
)
{
	int		nbu, i;
	off_t		fsize;
	int		bs;
	struct cca_buf	*cbufs;
	struct cca_f	*cca_info;
	struct fdinfo	*llfio;
        off_t           filead;
        int             ret;
        off_t           well_formed_fsize;

	cca_info = (struct cca_f *)fio->lyr_info;
	llfio  = fio->fioptr;

        if ( CCA_SOFT_BYPASS )
        	return ( XRCALL(llfio,weodrtn) llfio, stat) );

	if ( cca_info->optflags.no_write )
		ERETURN(stat, EBADF, 0);

	if (cca_info->is_blkspec)
		ERETURN(stat, FDC_ERR_NOSUP, 0);

	cca_info->fsize = cca_info->cpos;

	fio->rwflag = WRITIN;
	fio->ateof = 0;
	fio->ateod = 1;
	fio->recbits = 0;
/*
 *	Fix up any cache page buffers for file pages which lie past the new EOD.
 */
	nbu	= cca_info->nbufs;
	bs	= cca_info->bsize;
	cbufs	= cca_info->bufs;
	fsize	= cca_info->fsize;

	for (i=0; i<nbu; i++) {
                if ( cbufs[i].file_page.parts.file_number !=
		     cca_info->file_number ) 
			continue;

		filead = (cbufs[i].file_page.parts.page_number) *
			 (cca_info->byte_per_pg)*8;

		if (filead >= 0) {

			/* If page is past EOD then mark it free */
			if (filead >= fsize)
                        {
                                ret = _cca_clear_page(cca_info, &cbufs[i],
						stat);
                                if ( ret == ERR ) return( ERR );
                        }

			/* If page straddles EOD then zero out part of it */
			else if (filead < fsize && filead + bs > fsize) {
				int valid_bytes = BITS2BYTES(fsize - filead);
#ifdef SDS_SUPPORTED
				if (cca_info->optflags.sds) {
				    int sds_offset;
				    int res;
				    sds_offset   = BPTR2CP(cbufs[i].buf) -
							(char *)NULL;
	
				    res = _sdsset_any(
					sds_offset + valid_bytes,
					0,
					BITS2BYTES(bs) - valid_bytes);

				    if (res == -1) {
					ERETURN(stat, errno, 0);
				    }
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
        well_formed_fsize = (fsize+cca_info->bits_per_sect-1) &
			    (~(cca_info->bits_per_sect-1));

	if (well_formed_fsize < cca_info->feof) {

		if (XRCALL(llfio,seekrtn) llfio, BITS2BYTES(well_formed_fsize),
					  SEEK_SET, stat) == ERR)
			return(ERR);

		if (XRCALL(llfio,weodrtn) llfio, stat) == ERR)
			return(ERR);

		cca_info->feof = well_formed_fsize;
	}

	SETSTAT(stat, FFEOD, 0);
	return(0);
}
