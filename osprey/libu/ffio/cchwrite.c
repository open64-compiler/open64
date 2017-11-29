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


#pragma ident "@(#) libu/ffio/cchwrite.c	92.3	10/15/99 10:22:38"

#include <stdio.h>
#include <ffio.h>
#include <errno.h>
#include "cchio.h"

#ifdef _UNICOS
#pragma _CRI duplicate _cch_write as _cch_writea
#elif __mips
#pragma weak _cch_writea=_cch_write
#endif

#ifdef __mips
/* If we have not opened with O_DIRECT, then anything is aligned. */
/* Otherwise, currently, we're saying nothing is aligned. */
/* This could be extended to check for proper disk and buffer alignment */
#define _CCH_ALIGN(a) (!cch_info->odirect)
#else
/* This could be extended on Unicos systems, so that if the disk */
/* position is sector aligned, and if the buffer position is properly */
/* aligned, it returns true. That would mean buffer position is word */
/* aligned on PVPs, 8-word aligned on MPPs */
#define _CCH_ALIGN(a) 0	
#endif
/*
 * _cch_write
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
_cch_write(
struct fdinfo	*fio, 		/* ffio file descriptor. */
bitptr		datptr,		/* bit pointer to the user's data. */
size_t		nbytes,		/* Nuber of bytes to be written. */
struct ffsw	*stat,		/* pointer to status return word */
int		fulp,		/* full or partial write mode flag */
int		*ubcp 		/* pointer to unused bit count.  On return, */
				/* *ubcp is updated to contain the unused bit */
				/* count in the data returned. */ 
)
{
	off_t		cpos;		/* bit position in file */
	int64		moved;		/* number of bits transfered */
	int64		bytes_moved;	/* number of bytes transfered */
	int64		morebits;	/* bits moved in current iteration */
	int64		numblocks;	/* num of pages to process this iter */
	int		pgoff;
	off_t		fileaddr;
	off_t		eofaddr;
	int		gb_rd;		/* nonzero if pages must be read */
	int		valid;		/* nonzero if CCH_VALIDBUFFER should */
					/* be set */
	int64		nbits;
	int64		i;
	int		bs, nbu;
	off_t		olpos, endpos, endoff;
	bitptr		toptr;
	struct ffsw	locstat;
	struct fdinfo	*llfio;
	struct cch_f	*cch_info;
	struct cch_buf	*cubuf;
	int		err;
	short		firsteof = 0;
	short		setfirst;

        CCH_DEBUG(("_cch_write EN: nbytes=%d fulp=%d ubc=%d\n",nbytes,fulp,
			*ubcp));
	CLRSTAT(locstat);
	cch_info = (struct cch_f *)fio->lyr_info;
	nbits = BYTES2BITS(nbytes) - *ubcp;

	fio->rwflag = WRITIN;
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
        /* Although this layer is capable of handling non-zero ubc */
        /* and bitptrs that aren't on a byte boundary, we are not */
        /* supporting this right now on mips systems. */
        if (*ubcp != 0){
                err = FDC_ERR_UBC;
                goto err1_ret;
        }
        if ((BPBITOFF(datptr) & 07) != 0) {
                err = FDC_ERR_REQ;
                goto err1_ret;
        }
#endif

	if (nbits == 0) {			/* quick return for nbits == 0*/
		SETSTAT(stat, FFCNT, 0);
		return(0);
	}

/*
 *	Move data from user to buffer 
 */
	llfio	 = fio->fioptr;
	bs	 = cch_info->bsize;	/* bit size of each buffer */
	cpos	 = cch_info->cpos;	/* current file position */
	olpos    = cpos;		/* save original position */
	fileaddr = CCHFLOOR(cpos,bs);	/* bit offset within the file of the 
					 * start of the current page */

	if (cpos > cch_info->fsize) {

		firsteof = 1;

		/* Is the page with eof in memory? */
		/* If so, zero out the portion beyond eof. */
		eofaddr = CCHFLOOR(cch_info->fsize, bs);
		CCH_FINDBLK(cch_info, eofaddr, cubuf);
		if (cubuf != NULL && (cubuf->flags & CCH_ZEROED) == 0){
#ifdef CCH_SDS_SUPPORTED
                        if (cch_info->optflags & CCHOPT_SDS) {
				/* should never happen */
				ERETURN(stat, FDC_ERR_INTERR, 0);
			}
#endif
			pgoff = cch_info->fsize - eofaddr; /* offset of eof */
							/* within the page */
			SET_BPTR(toptr, INC_BPTR(cubuf->buf, pgoff));
			morebits = bs - pgoff;
			if (morebits != 0){
				CCH_MEMCLEAR(toptr, morebits);
			}
			cubuf->flags |= CCH_ZEROED;
		}
	}
	while (nbits > 0) {
		/*
		 * Find the cache buffer assigned to the current page.  If
		 * no buffer is currently assigned, then _cch_getblk assigns 
		 * one.
		 */
		pgoff	  = cpos - fileaddr;	/* offset within the page */
		numblocks = 1;			/* number of of pages to prcess
						 * in this iteration */

		CCH_FINDBLK(cch_info, fileaddr, cubuf);

		if (cubuf == NULL) {	/* if data not buffer-resident*/

                        if (nbits > cch_info->bypasssize 
#ifdef CCH_SDS_SUPPORTED
			    && !(cch_info->optflags & CCHOPT_SDS)
#endif
				) {
                                /* Maybe we can bypass buffering */
                                if ((morebits= _cch_bypass(cch_info, nbits, cpos,
                                   datptr, fileaddr, 'w', llfio, &locstat))>0)
                                        goto adjust;
                                else if (morebits < 0) {
					/* Is it right to return the count */
					/* in locstat? Because we might */
					/* have read some data... */
					goto er1;
                                }
                                /* we weren't able to bypass buffering */
                        }

			morebits = nbits;
			endpos = cpos + morebits; /*1 bit past the end*/
			endoff = endpos - CCHFLOOR(endpos,bs);

			if (endpos > fileaddr + bs) {
				numblocks = (endpos-fileaddr-1)/bs + 1;
				nbu	  = cch_info->nbufs;
				/*
				 * Handle at most a cache full at a time
				 */
				if (numblocks > nbu) {
					numblocks = nbu;
					endpos    = fileaddr + nbu * bs;
					endoff    = 0;
					morebits  = endpos - cpos;
				}
			}

			/*
			 * It is possible that the first or last
			 * page must be read because the transfer
			 * fills only part of these pages.  In each
			 * iteration, _cch_getblk requires that 
			 * consecutive buffer pages must all be read,
			 * or else all be assigned without pre-reading.
			 * The following code breaks off the current
			 * portion of the transfer when necessary to
			 * accomplish this.
			 */  

			if (numblocks > 1) {

				if (numblocks == 2) {
					if ((pgoff == 0) != (endoff == 0)) {
						/* process only first page */
						numblocks  = 1;
						endoff     = 0;
						morebits   = bs - pgoff;
					}
				}
				else {
					if (pgoff) {
						/* process only first page */
						numblocks  = 1;
						endoff     = 0;
						morebits   = bs - pgoff;
					}
					else if (endoff) {
						/* process all but last page */
						numblocks -= 1;
						endoff     = 0;
						morebits  -= endoff;
					}
				}
			}

			/*
			 * Request that _cch_getblk read in the file
			 * pages if partial pages of data will be
			 * written.
			 */
			
			gb_rd = (pgoff || endoff);
			/* The pages will be valid if we do not */
			/* have to read them. That's because */
			/* we will be writing to the entire page */	
			/* The page will also be valid if we do read it */
			valid = 1;
			setfirst = 0;
			if (gb_rd &&
#ifdef CCH_SDS_SUPPORTED
 				!(cch_info->optflags & CCHOPT_SDS) &&
#endif
				(numblocks == 1) &&
				((fileaddr+bs) < cch_info->feof) &&
				(_CCH_ALIGN(pgoff) && _CCH_ALIGN(endoff))){
				/* do we really need to read the page in? */
				/* if pgoff and endoff are properly aligned, */
				/* we do not */
				/* Note that if any part of the page is */
				/* beyond feof, we want to read it in. */
				/* That's because code in _cch_rdabuf */
				/* that handles having a partially dirty */
				/* page expects to be able to read the */
				/* data preceding the dirty data */
				gb_rd = 0;
				valid = 0;	/* the page will not be valid */
				setfirst = 1;
			}
			cubuf = _cch_getblk(cch_info, llfio, fileaddr,
					    &numblocks, gb_rd, valid, &locstat);
			if (cubuf == NULL) {
				goto er1;
			}
			if (setfirst) {
				cubuf->firstdata = pgoff;
				if (endoff == 0)
					cubuf->lastdata = bs;
				else
				cubuf->lastdata = endoff;
			}

			if (firsteof  && pgoff != 0) {
				/* There is a gap between the eof and */
				/* this data. Zero it if necessary. */
				if ((cubuf->flags & CCH_ZEROED) == 0) {
					int zbits;
#ifdef CCH_SDS_SUPPORTED
                        		if (cch_info->optflags & CCHOPT_SDS) {
						/* should never happen */
						ERETURN(stat, FDC_ERR_INTERR, 0);
					}
#endif
					if ((eofaddr == fileaddr)) {
						/* the eof is on this page */

						zbits = bs - (cch_info->fsize - eofaddr);
						SET_BPTR(toptr, INC_BPTR(cubuf->buf, 
						(cch_info->fsize - eofaddr)));
					}
					else {
						/* the eof is not on this page */
						/* zero the entire page */
						zbits = bs;
						toptr = cubuf->buf;
					}
					CCH_MEMCLEAR(toptr, zbits);
					cubuf->flags |= CCH_ZEROED;
				}
			}
			morebits  = MIN(nbits, bs * numblocks - pgoff);

			/* remember the last buffer page for next time */
			cch_info->cubuf = cubuf + numblocks - 1;
		}
		else {
			morebits	= MIN(nbits, bs - pgoff);

			if (!(cubuf->flags & CCH_VALIDBUFFER)) {
				/* The buffer is there, but it */
				/* is not entirely valid, because */
				/* we never read into it. */
				/* We can continue to just dirty it, */
				/* provided that the dirty part is */
				/* contiguous, and is properly aligned */

				endoff = pgoff + morebits;
				if ((pgoff == cubuf->lastdata &&
					_CCH_ALIGN(endoff))|| (endoff == 
					cubuf->firstdata && _CCH_ALIGN(pgoff))
				 	|| (pgoff >= cubuf->firstdata &&
					endoff <=  cubuf->lastdata)){
					cubuf->firstdata = MIN(pgoff,
						cubuf->firstdata);
					cubuf->lastdata = MAX(endoff,
						cubuf->lastdata);
					if (cubuf->firstdata == 0 &&
					cubuf->lastdata == bs) {
						cubuf->lastdata = 0;
						cubuf->flags |=CCH_VALIDBUFFER;
					} 
				} else {

					/* We can't just keep on putting */
					/* stuff in the buffer without  */
					/* prereading it. So, we will call */
					/* _cch_rdabuf, which has the */
					/* smarts to read only the non-dirty */
					/* parts */
					if (_cch_rdabuf(cch_info, llfio, cubuf,
					BITS2BYTES(cch_info->bsize),
					BITS2BYTES(cubuf->filead), 1, 's',
					&locstat)) {
						goto er1;
					}
				}
			}
		}

		for (i=0; i<numblocks; i++) {
			/* adjust last access time */
			CCH_CHRONOMETER(cubuf[i],cch_info);
			cubuf[i].flags |= CCH_DIRTY;
		}

		SET_BPTR(toptr, INC_BPTR(cubuf->buf, pgoff));
		
#ifdef CCH_SDS_SUPPORTED
		if (cch_info->optflags & CCHOPT_SDS) {
			if (_sds_fr_mem(toptr, datptr, morebits) == ERR)
				ERETURN(stat, errno, 0);
		}
		else
			_CCH_MOV_BITS(toptr, datptr, morebits); /* contiguous bufs */
#else
		_CCH_MOV_BITS(toptr, datptr, morebits); /* contiguous bufs */
#endif
adjust:

		SET_BPTR(datptr, INC_BPTR(datptr, morebits));

		cpos  += morebits;
		nbits -= morebits;
		fileaddr = CCHFLOOR(cpos,bs);
			/* bit offset within the file of the page */
		firsteof = 0;
		if (cpos > cch_info->fsize){
			cch_info->fsize = cpos;
		}
	}
	cch_info->cpos   = cpos;
	moved		 = cpos - olpos;
	fio->recbits	+= moved;


	bytes_moved = BITS2BYTES(moved);
	SETSTAT(stat, FFCNT, bytes_moved);
	return(bytes_moved);
err1_ret:
	ERETURN(stat, err, 0);
er1:
	*stat = locstat;
	return(ERR);
}
