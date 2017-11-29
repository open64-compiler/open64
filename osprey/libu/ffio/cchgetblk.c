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


#pragma ident "@(#) libu/ffio/cchgetblk.c	92.2	10/07/99 15:40:23"


#include <stdio.h>
#include <ffio.h>
#if defined(BUILD_OS_DARWIN)
#include <limits.h>
#define MAXLONG LONG_MAX
#else /* defined(BUILD_OS_DARWIN) */
#include <values.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <errno.h>
#include "cchio.h"

/*
 * _cch_getblk
 *
 * Searches for and assigns page buffers for one or more consecutive pages
 * of the file which are not already resident in buffer.   If one of the
 * pages are already buffer-resident, the request is processed only up to the
 * page preceding the first page which is already buffer-resident.  This is
 * done because the callers of _cch_getblk assume that *nblkp contiguous
 * buffers are assigned upon return from _cch_getblk.
 *
 * Return value:
 *
 *	A pointer to the first of the cache buffer pages on success.  On 
 *	failure, NULL is returned with the sw_error field of the stat structure
 *	set to the error status.
 *
 *	Also upon exit, *nblkp is set to the number of consecutive buffers 
 *	assigned.
 */

struct cch_buf *
_cch_getblk(
struct cch_f	*cch_info,	/* cch_f structure for the file */
struct fdinfo	*llfio,		/* ffio file descriptor for underlying layer */
off_t		fileaddr,	/* bit offset within the file of the buffer.
				 * This number must be a multiple of the buffer
				 * size. */
int64		*nblkp,		/* on input, the number of contiguous buffer 
				 * blocks sought.  On output, assigned the 
				 * actual number of contiguous buffer blocks
				 * assigned. */
int		rd,		/* 0 if all of the new blocks may be 
				 * assigned without reading the file page.  
				 * != 0 if the pages must be read. */
int		valid,		/* 0 if the CCH_VALIDBUFFER bit should */
				/* not be set in the new blocks */
struct ffsw	*stat		/* pointer to status return word */
)
{
	int		i, nbu, ret;
	int		bs;
	int		lru_id;		/* buffer number of least recently */
					/* used buffer. */
	int		limit;
	int64		nblk;
	off_t		endaddr, firstpaddr, faddr;
	long		*wptr;
	long		lru_tm;
	struct cch_buf	*cubuf;
	struct cch_buf	*cbufs;
	struct cch_buf	*fb;

	CCH_DEBUG(("_cch_getblk EN: to bit offset %d\n",fileaddr));

	nbu     = cch_info->nbufs;
	cbufs   = cch_info->bufs;
	bs	= cch_info->bsize;
	nblk	= *nblkp;

	if (nblk > 1) {
		/*
		 * Find the first page in the consecutive list of pages which
		 * is buffer-resident. 
		 */
		endaddr  = fileaddr + nblk * bs;

		firstpaddr = endaddr;

		for (i=0; i<nbu; i++) {
			off_t x;
			cubuf = &cbufs[i];
			x = cubuf->filead;
			if (fileaddr <= x && x < firstpaddr) 
				firstpaddr = x;
		}
	
		if (firstpaddr < endaddr)	/* a page is buffer resident */
			nblk = *nblkp = (firstpaddr - fileaddr) / bs;

		if (nblk <= 0)
			return((struct cch_buf *)NULL);	/* shouldn't happen ! */
	}
/*
 *	Find the least-recently accessed sequence of *nblkp contiguous buffers.
 *	Free buffers are counted as if their last access time was 0.
 *	Search the buffers in groups of size nblk to speed this search and
 *	reduce fragmentation of the cache.  When nblk>1, this algorithm
 *	approximates LRU and, most importantly, is deterministic.
 */
	lru_tm  = MAXLONG;	      /* min _rtc() value in upcoming loop */
	lru_id  = 0;
	for (i=0; i<(nbu-nblk+1); i+=nblk) {
		long last_access = 0;    /* free pages have last_access == 0 */
		if (cbufs[i].filead >= 0)
			last_access = cbufs[i].atime;
		if (last_access < lru_tm) {
			lru_tm = last_access;
			lru_id = i;
		}
	}
/*
 *	Use the least recently used page buffer or group of page buffers.  
 *	Flush any of these page buffers which have the dirty bit set.  When
 *	several adjacent buffers are dirty and correspond to adjacent pages
 *	in the file, they can be flushed with one request.
 */
	fb = &cbufs[lru_id];
	for (i=0; i<nblk; i++) {
		int contig = 0;	/* number of contiguous dirty buffers */
	
		faddr = fb[i].filead;
		if (faddr == -1) 
			continue;	/* buffer is free */

		while (i+contig < nblk && (fb[i+contig].flags & CCH_DIRTY) &&
		       fb[i+contig].filead == faddr) {
			if (fb[i+contig].lastdata || fb[i+contig].firstdata) {
				if (contig == 0)
					contig = 1;
				break;
			}
			contig++;
			faddr += bs;
		}
		if (contig > 0) {
			if (faddr  > cch_info->fsize) {
				/* eof is in the last buffer */
				/* clear it if necessary */
				if ((fb[i+contig-1].flags & CCH_ZEROED) == 0){
					bitptr toptr;
					off_t eofaddr;
					int pgoff;
					eofaddr = CCHFLOOR(cch_info->fsize, bs);
					pgoff = cch_info->fsize - eofaddr;	
					SET_BPTR(toptr,
					   INC_BPTR(fb[i+contig-1].buf, 
					   pgoff));
					CCH_MEMCLEAR(toptr,(bs - pgoff));
					fb[i+contig-1].flags |= CCH_ZEROED;
				}
			}
			ret = _cch_wrabuf(cch_info, llfio, &fb[i],
					BITS2BYTES(bs),
				  	BITS2BYTES(fb[i].filead),
					contig,
				  	&cch_info->feof,
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
					's',	/* flush synchronously */
#else
					'a',	/* flush asynchronously */
#endif
					stat);
			if (ret == ERR)
				return((struct cch_buf *)NULL);

			i += contig - 1;
		}
	}
/*
 *	Wait for any active page buffer I/O, and then requisition the buffers 
 *	for the appropriate file pages.
 */
	for (i=0; i<nblk; i++) {
		if (fb[i].flags & (CCH_WRITING | CCH_READING)) {
			CCHWAITIO(llfio,&fb[i],stat,ret);
			if (ret == ERR) 
				return((struct cch_buf *)NULL);
		}
		fb[i].filead  = fileaddr + i * bs;
		fb[i].flags   = CCH_VALID;
		fb[i].firstdata = fb[i].lastdata  = 0;
		if (valid)
			fb[i].flags |= CCH_VALIDBUFFER;
	}
/*
 *	Now start the synchronous reading of the file page into the buffer.  If
 *	all of the pages lie beyond the EOF, then suppress the read.
 */
	if (rd) {
		if (fileaddr < cch_info->feof) {
			int by_tran;
			fb->sw.sw_flag = 0;	/* indicate I/O in progress */
			ret = _cch_rdabuf(cch_info, llfio, fb, BITS2BYTES(bs),
				 	BITS2BYTES(fb->filead), nblk, 's',stat);
			if (ret == ERR)
				return((struct cch_buf *)NULL);

			/*
			 * Zero portions of the buffers past the end of file.
			 */
			by_tran = fb->sw.sw_count;
#ifdef CCH_SDS_SUPPORTED
			if (cch_info->optflags & CCHOPT_SDS) {
				int ret;
				ret = _sdsset(
					(BPTR2CP(fb->buf) - (char*)NULL) +
						by_tran,
					0,
					nblk * BITS2BYTES(bs) - by_tran);
				if (ret == ERR) {
					_SETERROR(stat, errno, 0);
					return((struct cch_buf *)NULL);
				}
			}	
			else
#endif
			{
			    if ((nblk*BITS2BYTES(bs)-by_tran) != 0)
				(void)memset(BPTR2CP(
					fb->buf) + by_tran,
					0,
					nblk * BITS2BYTES(bs) - by_tran);
			}
			for (i=0; i<nblk; i++) {
				fb[i].flags |= CCH_ZEROED;
			}
		}
		else {				/* page lies beyond EOF */
			/*
			 * Zero the entire buffer.  
			 */
#ifdef CCH_SDS_SUPPORTED
			if (cch_info->optflags & CCHOPT_SDS) {
				int ret;
				ret = _sdsset(
					(BPTR2CP(fb->buf) - (char*)NULL),
					0,
					nblk * BITS2BYTES(bs));
				if (ret == ERR) {
					_SETERROR(stat, errno, 0);
					return((struct cch_buf *)NULL);
				}
				for (i=0; i<nblk; i++) {
					fb[i].flags |= CCH_ZEROED;
				}
			}	
			else
#endif
			if (fileaddr < cch_info->fsize){
				/* this block is between cch_info->feof and */
				/* cch_info->fsize, so we must zero it */
				/* Logic in other parts of this layer will */
				/* only zero what is beyond cch_info->fsize */
#ifdef _CRAY1
                                wptr = BPTR2WP(fb->buf);
                                limit = (nblk * bs) >> 6; /* convert to words */
                                /* this loop vectorizes! */
                                for (i=0; i<limit; i++)
                                        wptr[i] = 0;
#else
                          	memset(BPTR2CP(fb->buf), 0,
				 (nblk * BITS2BYTES(bs)));
#endif
				for (i=0; i<nblk; i++) {
					fb[i].flags |= CCH_ZEROED;
				}
			}
		}
	}

	cubuf = fb;
	cch_info->cubuf = cubuf;	/* remember this buffer */
	CCH_DEBUG(("_cch_getblk EX: buffer # %d\n",cubuf-cbufs));
	return(cubuf);
}
