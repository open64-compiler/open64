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


#pragma ident "@(#) libu/ffio/ccagetblk.c	92.1	06/29/99 13:16:47"


#include <stdio.h>
#include <ffio.h>
#if defined(BUILD_OS_DARWIN)
#include <limits.h>
#define MAXLONG	LONG_MAX
#else /* defined(BUILD_OS_DARWIN) */
#include <values.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <errno.h>
#include "ccaio.h"

/*
 * _cca_getblk
 *
 * Return value:
 *
 *	A pointer to the first of the cache buffer pages on success.  On 
 *	failure, NULL is returned with the sw_error field of the stat structure
 *	set to the error status.
 *
 */

struct cca_buf *
_cca_getblk(
struct cca_f	*cca_info,	/* cca_f structure for the file */
struct fdinfo	*llfio,		/* ffio file descriptor for underlying layer */
off_t		fileaddr,	/* bit offset within the file of the buffer.
				 * This number must be a multiple of the buffer
				 * size. */
int		rd,		/* 0 if writing AND the entire page of data is
				 * being written.  != 0 if the pages might need
				 * to be pre-read. */
struct ffsw	*stat,	         /* pointer to status return word */
char            sync_mode,       /* sync io mode */
char            read_write_mode) /* called by ccaread('r') or ccawrite('w') */
{
	int		i, j, nbu, ret;
	int		bs;
	int		lru_id,lru_id_exempt;	/* buffer number of least 
						 * recently used buffer. */
	long		lru_tm,lru_tm_exempt;
	struct cca_buf	*cbufs;
	struct cca_buf	*fb;
	long            last_access;
	off_t           file_byte_pos;
	struct ffsw     wait_stat;

	nbu     = cca_info->nbufs;
	cbufs   = cca_info->bufs;
	bs	= cca_info->bsize;

/*
 *	Find the least-recently accessed buffers.
 *	Free buffers are counted as if their last access time was 0.
 */
	lru_tm         = MAXLONG;      /* min CHRONOMETER value */
	lru_id         = -1;
	lru_tm_exempt  = MAXLONG;      /* min CHRONOMETER value */
	lru_id_exempt  = -1;
	for (i=0; i<nbu; i++) {
		if( cbufs[i].protected ) continue;
		if( cbufs[i].eligible ) {
			if ( cbufs[i].atime < lru_tm_exempt) {
				lru_tm_exempt = cbufs[i].atime;
				lru_id_exempt = i;
			}
		}
		else {
			last_access = 0;
			if (cbufs[i].file_page.all != NULL_FILE_PAGE )
				last_access = cbufs[i].atime;
			if (last_access < lru_tm) {
				lru_tm = last_access;
				lru_id = i;
			}
		}
	}

	/* is an exempt cache page to be considered? */
	if (lru_id_exempt >= 0) {

		/* is the exempt page older than the non-exempt page? */
		if (lru_tm_exempt < lru_tm) {

			fb = &cbufs[lru_id_exempt];
			fb->exempt_count ++;

			/* has exemption expired ? */
			if(lru_id == -1 ||
			   fb->exempt_count > cca_info->max_lead+1) {
				lru_id = lru_id_exempt;
				if( fb->exempt_count > (cca_info->max_lead+1) )
					cca_info->exempts_failed ++;
	      		}
			else {
				if (fb->exempt_count == 1)
					cca_info->exempts_issued ++;
			}
		}
	}
/*
 *	Use the least recently used page buffer .  
 *	Flush this page buffer.  
 */
	fb = &cbufs[lru_id];
	if ( fb->file_page.all != NULL_FILE_PAGE ) {
		*(cca_info->spilled) = TRUE;
		ret =  _cca_clear_page( cca_info, fb, stat );
		if(ret==ERR)
			return((struct cca_buf *)NULL);
	}

        fb->file_page.parts.page_number = fileaddr/bs;
        fb->file_page.parts.file_number = cca_info->file_number;
/*
 *	Now start the reading of the file page into the buffer.  If
 *	all of the pages lie beyond the EOF, then suppress the read.
 */
        fb->pre_init = TRUE;
        for(j=0;j<cca_info->dirty_sectwds;j++) fb->valid_sectors[j] = 0;
        for(j=0;j<cca_info->dirty_sectwds;j++) fb->sector_used[j] = 0;

	if (rd) {
		if (fileaddr < cca_info->feof) {
                        if( read_write_mode == 'r' || cca_info->optflags.prw ) {
                           file_byte_pos = fb->file_page.parts.page_number *
					   cca_info->byte_per_pg;

                           if( fb->sw.llfio ) { /* wait for any pending I/O */
                              CCAWAITIO( fb->sw, &wait_stat, ret );
                              if (ret == ERR) {
                                 *stat = wait_stat;
			         return((struct cca_buf *)NULL);
                              }
                           }

			   fb->sw.sw_flag = 0;	/* indicate I/O in progress */
			   ret = _cca_rdabuf(cca_info, 
                                      llfio, fb, 
                                      cca_info->byte_per_pg,
				      file_byte_pos, 
                                      sync_mode,
                                      stat);
			   if (ret == ERR) return((struct cca_buf *)NULL);
                        }
                        else {
                           cca_info->cache_pages_not_preread ++;
                           fb->pre_init = FALSE;
                           ret = 0;
			}
		}
		else {				/* page lies beyond EOF */
			/*
			 * Zero the entire buffer.  
			 */
			if (fb->sw.llfio) {
				CCAWAITIO( fb->sw, &wait_stat, ret );
                                if (ret == ERR) {
                                   *stat = wait_stat;
			           return((struct cca_buf *)NULL);
                                }
			}
			_clear_buffer(fb->buf, cca_info->optflags.sds,
				   cca_info->byte_per_pg, stat);
	
			cca_info->cache_pages_not_preread ++;
			fb->pre_init = FALSE;
		}
	}
        else {
		/*fb->sw.sw_count = 0; */
        }

	cca_info->cubuf = fb;	/* remember this buffer */
	return(fb);
}
