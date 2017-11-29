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


#pragma ident "@(#) libu/ffio/ccaread.c	92.2	10/11/99 15:29:41"


#include <stdio.h>
#include <errno.h>
#include <ffio.h>
#include "ccaio.h"


/*
 * _cca_read
 *
 * Process read requests for the cache layer.
 *
 * Return value:
 *
 *	The number of bytes transferred is returned upon successful completion.
 *	If an error occurs, -1 is returned.  
 *
 *	The stat->sw_stat field is set upon exit as follows:
 *
 *		FFCNT	- if >0 bits are read
 *		FFEOD	- if >0 bits are requested, and we are at EOD
 */
ssize_t
_cca_read(
struct fdinfo	*fio, 		/* ffio file descriptor. */
bitptr		bufptr,		/* bit pointer to where data is to go. */
size_t		nbytes,		/* Number of bytes to be read. */
struct ffsw	*stat,		/* pointer to status return word */
int		fulp,		/* full or partial read mode flag */
int		*ubcp		/* pointer to unused bit count.  On return, */
				/* *ubcp is updated to contain the unused bit */
				/* count in the data returned. */ 
)
{
	int64		j, nbits, lftbits;
	int		bs;
	int64		moved;		/* number of bits transfered */
	int64		bytes_moved;	/* number of bytes transfered */
	off_t		cpos;
	int		morebits;
	off_t		olpos;
	int		pgoff;		/* offset within page of the data */
	off_t		fileaddr;
	bitptr		frptr;
	struct fdinfo	*llfio;
	struct cca_f	*cca_info;
	struct cca_buf	*cubuf;
#ifdef _CCA_DIAGS
        int             unsynced_page_read_hit;
#endif
        off_t           first_dirty_sector, last_dirty_sector;
        FILE_PAGE       file_page;
        struct ffsw	getblk_stat;	/* pointer to status return word */
        struct ffsw	sync_stat;	/* pointer to status return word */
        int             sync_ret;
	int		err;

	cca_info = (struct cca_f *)fio->lyr_info;
	llfio	 = fio->fioptr;

        if(CCA_SOFT_BYPASS) {
             ssize_t ret;
             ret = XRCALL(llfio,readrtn) llfio, bufptr, nbytes, stat, fulp, ubcp );
             return( ret );
        }

        if( cca_info->optflags.no_read )
	    ERETURN(stat, EBADF, 0);

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
	/* Although this layer is capable of handling non-zero ubc */
	/* and bitptrs that aren't on a byte boundary, we are not */
	/* supporting this right now on mips systems. */
	if (*ubcp != 0){
		err = FDC_ERR_UBC;
		goto err1_ret;
	}
	if ((BPBITOFF(bufptr) & 07) != 0) {
		err = FDC_ERR_UBC;
		goto err1_ret;
	}
#endif
	nbits = ((uint64)nbytes << 3) - *ubcp;

	fio->rwflag = READIN;

	if (nbits == 0) {
		SETSTAT(stat, FFCNT, 0);
		return(0);
	}
/*
 *      Guarantee that we will not go past the actual EOD on the file
 *      by reducing the request if necessary
 */
	lftbits = cca_info->fsize - cca_info->cpos;

	if (lftbits < nbits) {
		if (lftbits <= 0) {
			fio->recbits = 0;
			fio->ateof = 0;
			fio->ateod = 1;
			SETSTAT(stat, FFEOD, 0)
			return(0);
		}
		nbits = lftbits;
	}
/*
 *	Move data from buffer to user
 */
	moved	 = 0;
	bs	 = cca_info->bsize;	/* bit size of each buffer */
	cpos	 = cca_info->cpos;	/* current file position */
	olpos	 = cpos;		/* save original position */
	fileaddr = CCAFLOOR(cpos,bs);	/* bit offset in file of start of 
					 * current page */

        file_page.parts.file_number = cca_info->file_number;

	while (nbits > 0) {

		int ret;
		pgoff	  = cpos - fileaddr;	/* offset within the page */

                file_page.parts.page_number = fileaddr/bs;

		CCA_FINDBLK(cca_info, file_page, cubuf, getblk_stat, ret);
		if (ret == ERR) {
			*stat = getblk_stat;
			goto err_ret;
		}

		if (cubuf == NULL) {	/* if data not buffer-resident*/
#ifdef _CCA_DIAGS
                        cca_info->read_miss ++;
#endif
			cubuf = _cca_getblk_p(cca_info,llfio, fileaddr, 
					    TRUE, &getblk_stat, NULL, 'r', 's');
                        if( cubuf == NULL ) {
				*stat = getblk_stat;
				goto err_ret;
			}
		}
#ifdef _CCA_DIAGS
		else {
                        cca_info->read_hit ++;
		}
#endif

		morebits = MIN(nbits, bs - pgoff);

                first_dirty_sector  = pgoff/cca_info->bits_per_sect;
                last_dirty_sector  = (pgoff+morebits-1)/cca_info->bits_per_sect;
                if( cubuf->pre_init == FALSE ) {
#ifdef _CCA_DIAGS
                   unsynced_page_read_hit = TRUE;
#endif

                   for(j=first_dirty_sector;j<=last_dirty_sector;j++) {
                      if(!(_GETBIT(j,cubuf->valid_sectors))) { 
                          sync_ret = _cca_sync_page(cca_info, cubuf,&sync_stat);
                          if( sync_ret == ERR ) {
			     *stat = sync_stat;
			     goto err_ret;
			  }
#ifdef _CCA_DIAGS
                          else
                             cca_info->cache_pages_synced_for_read += sync_ret;
                          unsynced_page_read_hit = FALSE;
#endif
                          break;
                      } 
                   }
#ifdef _CCA_DIAGS
                   if (unsynced_page_read_hit)
			cca_info->unsynced_pages_read ++;
#endif
                }

#ifdef _CCA_DIAGS
                cca_info->num_sectors_used +=
		    (last_dirty_sector-first_dirty_sector+1);
#endif
                for(j=first_dirty_sector;j<=last_dirty_sector;j++) {
#ifdef _CCA_DIAGS
                   if(_GETBIT(j,cubuf->sector_used))
                       cca_info->sect_reused ++;
                   else
#endif
                      _SETBIT(j,cubuf->sector_used);
                }


                if( cubuf->eligible || cubuf->adv_read ) {
                   cubuf = _cca_getblk_p(cca_info, llfio, fileaddr, TRUE,
					 &getblk_stat, cubuf , 'r' , 's' );
                   if( cubuf == NULL )
		   {
			*stat = getblk_stat;
			goto err_ret;
		   }
                }

                CHRONOMETER(cubuf,"reading"); /* adjust last access time*/

		SET_BPTR(frptr, INC_BPTR(cubuf->buf, pgoff));

#ifdef  SDS_SUPPORTED
		if (cca_info->optflags.sds) {
			if (_any_mem_fr_sds(bufptr, frptr, morebits) == -1) {
				ERETURN(stat, errno, 0);
			}
		}
		else
#endif
                {
			_CCA_MOV_BITS(bufptr, frptr, morebits); /* contiguous bufs */
                }

		SET_BPTR(bufptr, INC_BPTR(bufptr, morebits));

		cpos  += morebits;
		nbits -= morebits;
		fileaddr = cpos;	/* cpos is page-aligned after 1st pass*/
	}

	moved		 = cpos - olpos;
	cca_info->cpos   = cpos;
	fio->recbits	+= moved;

	bytes_moved = BITS2BYTES(moved);
	SETSTAT(stat, FFCNT, bytes_moved);
	*ubcp = (bytes_moved << 3) - moved;

#ifdef _CCA_DIAGS
        if( bytes_moved > 0 ) cca_info->bytes_read_from_cca += bytes_moved;
#endif
	return(bytes_moved);
err1_ret:
	ERETURN(stat, err, 0);
err_ret:
	return(ERR);
}
