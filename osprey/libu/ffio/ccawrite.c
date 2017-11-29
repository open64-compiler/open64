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


#pragma ident "@(#) libu/ffio/ccawrite.c	92.2	10/11/99 15:29:41"


#include <stdio.h>
#include <errno.h>
#include <ffio.h>
#include "ccaio.h"
/*
 * _cca_write
 *
 * Process write requests for the cache layer.
 *
 * Return value:
 *
 *	The number of bytes transferred is returned upon successful completion.
 *	If an error occurs, -1 is returned.  
 *
 *	The stat->sw_stat field is set to FFCNT upon normal return.
 */
ssize_t
_cca_write(
struct fdinfo	*fio, 		/* ffio file descriptor. */
bitptr		datptr,		/* bit pointer to the user's data. */
size_t		nbytes,		/* Number of bytes to be written. */
struct ffsw	*stat,		/* pointer to status return word */
int		fulp,		/* full or partial write mode flag */
int		*ubcp)		/* pointer to unused bit count.  On return, */
				/* *ubcp is updated to contain the unused bit */
				/* count in the data returned. */ 
{
	off_t		cpos;		/* bit position in file */
	int64		moved;		/* number of bits transfered */
	int64		bytes_moved;	/* number of bytes transfered */
	int		morebits;	/* bits moved in current iteration */
	int		pgoff;
	off_t		fileaddr;
	int		gb_rd;		/* nonzero if pages must be read */
	int64		nbits;
	int		bs;
	off_t		olpos, endpos, endoff;
	bitptr		toptr;
	struct fdinfo	*llfio;
	struct cca_f	*cca_info;
	struct cca_buf	*cubuf;
        int64           j;
        int             bits_per_sect;
        int64           first_dirty_sector , last_dirty_sector;
        FILE_PAGE       file_page;
        struct ffsw	getblk_stat;
        struct ffsw	wait_stat;
        struct ffsw	sync_stat;
        int             sync_ret;
        int             ret;
	int		err;

	cca_info = (struct cca_f *)fio->lyr_info;
	llfio	 = fio->fioptr;

        if (CCA_SOFT_BYPASS) {
             return( XRCALL(llfio,writertn) llfio, datptr, nbytes, stat, fulp,
					  ubcp ));
        }

        if ( cca_info->optflags.no_write )
	    ERETURN(stat, EBADF, 0);
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
        /* Although this layer is capable of handling non-zero ubc */
        /* and bitptrs that aren't on a byte boundary, we are not */
        /* supporting this right now on mips systems. */
        if (*ubcp != 0){
                err = FDC_ERR_UBC;
                goto err1_ret;
        }
        if ((BPBITOFF(datptr) & 07) != 0) {
                err = FDC_ERR_UBC;
                goto err1_ret;
        }
#endif

	nbits = BYTES2BITS(nbytes) - *ubcp;

	fio->rwflag = WRITIN;

	if (nbits == 0) {			/* quick return for nbits == 0*/
		SETSTAT(stat, FFCNT, 0);
		return(0);
	}

/*
 *	Move data from user to buffer 
 */
	bs	 = cca_info->bsize;	/* bit size of each buffer */
	cpos	 = cca_info->cpos;	/* current file position */
	olpos    = cpos;		/* save original position */
	fileaddr = CCAFLOOR(cpos,bs);	/* bit offset within the file of the 
					 * start of the current page */

	while (nbits > 0) {
		/*
		 * Find the cache buffer assigned to the current page.  If
		 * no buffer is currently assigned, then _cca_getblk assigns 
		 * one.
		 */
		pgoff	  = cpos - fileaddr;	/* offset within the page */

                file_page.parts.page_number = fileaddr/bs;
                file_page.parts.file_number = cca_info->file_number;

		CCA_FINDBLK(cca_info, file_page, cubuf, getblk_stat, ret);
		if (ret == ERR) {
			*stat = getblk_stat;
			goto err_ret;
		}

		if (cubuf == NULL) {	/* if data not buffer-resident*/

#ifdef _CCA_DIAGS
                        cca_info->write_miss ++;
#endif
			endpos = cpos + nbits; /*1 bit past the end*/
			endoff = endpos - CCAFLOOR(endpos,bs);

			gb_rd = (pgoff || endoff);
			cubuf = _cca_getblk_p(cca_info, llfio, fileaddr, gb_rd,
					      &getblk_stat, NULL , 'w' , 's' );
                        if ( cubuf == NULL ) {
			   *stat = getblk_stat;
			   goto err_ret;
                        }
		}
#ifdef _CCA_DIAGS
		else {
                        cca_info->write_hit ++;
		}
#endif

		morebits  = MIN(nbits, bs - pgoff);

                if ( cubuf->eligible || cubuf->adv_read ) {
	           cubuf = _cca_getblk_p(cca_info, llfio, fileaddr,
			   gb_rd, &getblk_stat, cubuf , 'w' , 's' );
                   if ( cubuf == NULL ) {
		      *stat = getblk_stat;
                      goto err_ret;
                   }
                }

		CHRONOMETER(cubuf,"writing"); /* adjust last access time */

		if ( cubuf->sw.rw_mode  &&
                     ( cubuf->sw.llfio != llfio ||
                       cubuf->sw.file_page.all != cubuf->file_page.all ) ) {

                   CCAWAITIO( cubuf->sw, &wait_stat, ret);
		   *stat = wait_stat;
                   if ( ret == ERR ) goto err_ret;
                }

                bits_per_sect = cca_info->bits_per_sect;

		/*
		 * If the page is partially valid and the request is not
		 * sector-aligned, then we force a pre-read of the page.
	 	 */
                if ( cubuf->pre_init == FALSE ) {
                   if ( (fileaddr+pgoff)&(bits_per_sect-1) ||
                       (morebits      )&(bits_per_sect-1)  ) {

                      sync_ret = _cca_sync_page( cca_info, cubuf, &sync_stat );
                      if ( sync_ret == ERR ) {
		   	 *stat = sync_stat;
                         goto err_ret;
                      }
#ifdef _CCA_DIAGS
                      else
                         cca_info->cache_pages_synced_for_ill_formed_write +=
				sync_ret;
#endif
                   }
                }

		cubuf->flags |= CCA_DIRTY;
                first_dirty_sector = pgoff/bits_per_sect;
                last_dirty_sector  = (pgoff+morebits-1)/bits_per_sect;
                for (j=first_dirty_sector; j<=last_dirty_sector; j++) {
                    _SETBIT(j,cubuf->valid_sectors);
                    _SETBIT(j,cubuf->unsynced_sectors);
                }
 

#ifdef _CCA_DIAGS
                cca_info->num_sectors_used +=
		    (last_dirty_sector-first_dirty_sector+1);
#endif

                for (j=first_dirty_sector; j<=last_dirty_sector; j++) {
#ifdef _CCA_DIAGS
                   if (_GETBIT(j,cubuf->sector_used))
                       cca_info->sect_reused ++;
                   else
#endif
                      _SETBIT(j,cubuf->sector_used);
                }


		SET_BPTR(toptr, INC_BPTR(cubuf->buf, pgoff));
		
#ifdef SDS_SUPPORTED
		if (cca_info->optflags.sds) {
			if (_any_sds_fr_mem(toptr, datptr, morebits) == ERR) {
				ERETURN(stat, errno, 0);
			}
		}
		else
#endif
                {
			_CCA_MOV_BITS(toptr, datptr, morebits); /* contiguous bufs */
                }

		SET_BPTR(datptr, INC_BPTR(datptr, morebits));

		cpos  += morebits;
	        cca_info->cpos   = cpos;
	        if (cpos > cca_info->fsize)
		    cca_info->fsize = cpos;
		nbits -= morebits;
		fileaddr = cpos;        /* cpos is page-aligned after 1st pass*/
	}
	moved		 = cpos - olpos;
	fio->recbits	+= moved;


	bytes_moved = BITS2BYTES(moved);
	SETSTAT(stat, FFCNT, bytes_moved);

#ifdef _CCA_DIAGS
        if ( bytes_moved > 0 ) cca_info->bytes_written_to_cca += bytes_moved;
#endif
	return(bytes_moved);
err1_ret:
	ERETURN(stat, err, 0);
err_ret:
	return(ERR);
}
