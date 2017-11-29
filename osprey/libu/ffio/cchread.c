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


#pragma ident "@(#) libu/ffio/cchread.c	92.3	10/15/99 10:22:38"

#include <stdio.h>
#include <ffio.h>
#include <errno.h>
#include "cchio.h"
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
#include <sys/listio.h>
#elif	defined(__mips)
#include "listio_mips.h"
#endif

#ifdef _UNICOS
#pragma _CRI duplicate _cch_read as _cch_reada
#elif	defined(__mips)
#pragma weak _cch_reada=_cch_read
#endif

/*
 * _cch_read
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
_cch_read(
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
	int		i;
	int64		nbits, lftbits;
	int		bs;
	int64		moved;		/* number of bits transfered */
	int64		bytes_moved;	/* number of bytes transfered */
	off_t		cpos;
	int64		numblocks;	/* number of pages to process per iter*/
	int64		morebits;
	off_t		olpos;
	int		pgoff;		/* offset within page of the data */
	off_t		fileaddr;
	bitptr		frptr;
	struct ffsw	locstat;
	struct fdinfo	*llfio;
	struct cch_f	*cch_info;
	struct cch_buf	*cubuf;
	int		err;

        CCH_DEBUG(("_cch_read EN: nbytes=%d fulp=%d ubc=%d\n",nbytes,fulp,
			*ubcp));
	CLRSTAT(locstat);
	cch_info = (struct cch_f *)fio->lyr_info;
	nbits = (nbytes << 3) - *ubcp;

	fio->rwflag = READIN;

	if (nbits == 0) {
		SETSTAT(stat, FFCNT, 0);
		return(0);
	}
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
        /* Although this layer is capable of handling non-zero ubc */
        /* and bitptrs that aren't on a byte boundary, we are not */
        /* supporting this right now on mips systems. */
        if (*ubcp != 0){
                err = FDC_ERR_UBC;
                goto err1_ret;
        }
        if ((BPBITOFF(bufptr) & 07) != 0) {
                err = FDC_ERR_REQ;
                goto err1_ret;
        }
#endif

/*
 *      Guarantee that we will not go past the actual EOD on the file
 *      by reducing the request if necessary
 */
	lftbits = cch_info->fsize - cch_info->cpos;

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
	llfio	 = fio->fioptr;
	moved	 = 0;
	bs	 = cch_info->bsize;	/* bit size of each buffer */
	cpos	 = cch_info->cpos;	/* current file position */
	olpos	 = cpos;		/* save original position */
	fileaddr = CCHFLOOR(cpos,bs);	/* bit offset in file of start of 
					 * current page */

	while (nbits > 0) {
		/*
		 * Find the cache buffer assigned to the current page.  If
		 * no buffer is currently assigned, then _cch_getblk assigns 
		 * one.
		 */

		pgoff	  = cpos - fileaddr;	/* offset within the page */
		numblocks = 1;			/* number of of pages to process
						 * in this iteration */

		CCH_FINDBLK(cch_info, fileaddr, cubuf);

		if (cubuf == NULL) {	/* if data not buffer-resident*/

			if (nbits > cch_info->bypasssize &&
			  !(cch_info->optflags & CCHOPT_SDS)) {
				/* Maybe we can bypass buffering */
				if ((morebits= _cch_bypass(cch_info, nbits, cpos,
				   bufptr, fileaddr, 'r', llfio, &locstat))>0)
					goto adjust;
				else if (morebits < 0) {
					/* I'm not sure this is the right */
					/* thing to do, but it is consistent */
					/* with other error returns in this */
					/* code. Maybe we've already read */
					/* some data - in that case, should */
					/* we return a positive count? */
					goto er1;
				}
				/* we weren't able to bypass buffering */
			}
			if (cpos + nbits > fileaddr + bs) {
				numblocks = (cpos + nbits - fileaddr -1)/bs + 1;
				if (numblocks > cch_info->nbufs)
					numblocks = cch_info->nbufs;
			}

			cubuf = _cch_getblk(cch_info,llfio, fileaddr, 
					    &numblocks, _TRUE, _TRUE, &locstat);
			if (cubuf == NULL) {
				goto er1;
			}

			morebits = MIN(nbits, bs * numblocks - pgoff);

			/* remember the last buffer page for next time */
			cch_info->cubuf = cubuf + numblocks - 1;
		}
		else {
			morebits	= MIN(nbits, bs - pgoff);
			if ((cubuf->flags & CCH_VALIDBUFFER) == 0){
				/* we have a buffer associated with this page */
				/* but we never read into it. */
				/* This can happen if wrote to this page. */
				/* We need to read it in, unless we are */
				/* totally within firstdata<->lastdata */
#ifdef CCH_SDS_SUPPORTED
				if (cch_info->optflags & CCHOPT_SDS) {
					/*should never happen */
					ERETURN(stat, FDC_ERR_INTERR, 0);
				}
#endif
				if (pgoff < cubuf->firstdata || pgoff+morebits > 
					(cubuf->lastdata)){
					if (_cch_rdabuf(cch_info, llfio, cubuf,
					   BITS2BYTES(cch_info->bsize),
					   BITS2BYTES(cubuf->filead), 1, 's',
					   &locstat)) {
						goto er1;
					   }
				}
			}
		}

		for (i=0; i<numblocks; i++){
			/* adjust last access time */
			CCH_CHRONOMETER(cubuf[i],cch_info);
		}

		SET_BPTR(frptr, INC_BPTR(cubuf->buf, pgoff));
#ifdef CCH_SDS_SUPPORTED
		if (cch_info->optflags & CCHOPT_SDS) {
			if (_mem_fr_sds(bufptr, frptr, morebits) == -1)
				ERETURN(stat, errno, 0);
		}
		else
#endif
		_CCH_MOV_BITS(bufptr, frptr, morebits); /* contiguous bufs */

adjust:
		SET_BPTR(bufptr, INC_BPTR(bufptr, morebits));

		cpos  += morebits;
		nbits -= morebits;
		fileaddr = CCHFLOOR(cpos,bs);	/* bit offset in file 
					of start of page */
	}

	moved		 = cpos - olpos;
	cch_info->cpos   = cpos;
	fio->recbits	+= moved;

	bytes_moved = BITS2BYTES(moved);
	SETSTAT(stat, FFCNT, bytes_moved);
	*ubcp = (bytes_moved << 3) - moved;
	return (bytes_moved);
err1_ret:
	ERETURN(stat, err, 0);
er1:
	*stat = locstat;
	return(ERR);
}
/*
 * returns the number of bits it wrote or read.
 * Returns -1 if error.
 */
int64 _cch_bypass( 
struct cch_f *cch_info,	/* cch_f structure for file */
int64 nbits,	/* maximum number of bits to read/write */
off_t cpos,	/* bit offset: current position */
bitptr bufptr,	/* user's data area */
off_t fileaddr, /* bit offset within the file of the buffer containing*/
		/* the first bit of user's data */
char rwmode,	/* 'r' for read, 'w' for write */
struct fdinfo *llfio,	/* fdinfo pointer for underlying layer */
struct ffsw *stat)
{
int64 nonbuf_bits;
int64 nonbuf_bytes;
int64 nblk;
off_t endaddr, firstpaddr;
int i;
struct cch_buf *cubuf;
struct cch_buf *cbufs;
off_t bytoff;
int bs;

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
#define MEMALIGN(a) (((BPBITOFF(a) & 007) == 0) && (((long)(BPTR2CP(a)) % \
	cch_info->memalign) == 0))
#else
#ifdef _CRAYMPP
/* For well-formed i/o, MPPs require buffer alignment on an 8-word boundary */
#define MEMALIGN(a) (((BPBITOFF(a) & 07) == 0) && (((long)(BPTR2CP(a)) & 0x3f) == 0))
#else
/* buffer should be aligned on a word boundary */
#define MEMALIGN(a) (((BPBITOFF(a) & 07) == 0) && (BPTR2CP(a) == (char *)(long *)BPTR2CP(a)))
#endif
#endif
#define DISKALIGN(a) ((a) % cch_info->diskalign == 0)

	/* If we are reading beyond feof, return 0 */
	if (rwmode == 'r' && (cpos > cch_info->feof))
		return 0;
	/* Is the data properly aligned? */
	bs = cch_info->bsize;
	if (cpos % 8 != 0)	/* cpos is a bit offset */
		return 0;
	bytoff = cpos/8;
	
	if (!DISKALIGN(bytoff) || !MEMALIGN(bufptr))
		return 0;	/* data is not aligned */

	/* Now see if any of the buffers related to this request are */
	/* in memory */
	nblk = (cpos + nbits - fileaddr -1)/bs+1;
	endaddr = fileaddr+nblk*bs;	
	firstpaddr = endaddr;
	cbufs = cch_info->bufs;
	nonbuf_bits = nbits;
	for (i = 0; i < cch_info->nbufs; i++) {
		off_t x;
		cubuf = &cbufs[i];
		x = cubuf->filead;
		if (fileaddr <=x && x < firstpaddr)
			firstpaddr = x;
	}
	if (firstpaddr < endaddr){
		/* a page containing some of this request is buffer resident */
		/* reduce the request to the part that is not buffer resident */
		nonbuf_bits = firstpaddr - cpos;
	}
	/* We know that the start of the request is aligned. */
	/* Now check out the rest of it */
	nonbuf_bytes = nonbuf_bits/8;
	if (cch_info->maxiosize > 0 && nonbuf_bytes > cch_info->maxiosize)
		nonbuf_bytes = cch_info->maxiosize;
	nonbuf_bytes = nonbuf_bytes - (nonbuf_bytes % cch_info->chunksize);
	if (nonbuf_bytes < cch_info->miniosize || 
	    nonbuf_bytes <= (cch_info->bypasssize)/8){
		return 0;
	}
	if (nonbuf_bytes > 0){
		int ubc = 0;
		ssize_t ret;
		struct fflistreq list_array[1];

		if (cch_info->do_sylistio){
#if	!defined(_LITTLE_ENDIAN)
			struct ffsw listat;
			if (rwmode == 'r')
				list_array[0].li_opcode = LO_READ;
			else
				list_array[0].li_opcode = LO_WRITE;
			list_array[0].li_flags = LF_LSEEK;
			list_array[0].li_offset = bytoff;
#ifdef __mips
			list_array[0].li_ffioptr = llfio;
#else
			list_array[0].li_fildes = (int)llfio;
#endif
			list_array[0].li_buf = BPTR2CP(bufptr);
			list_array[0].li_nbyte = (size_t) nonbuf_bytes;
			list_array[0].li_status = &listat;
			list_array[0].li_signo = 0;
			list_array[0].li_nstride = 1;
			list_array[0].li_filstride = 0;
			list_array[0].li_memstride = 0;
			if ( XRCALL(llfio, listiortn)LC_WAIT, list_array,
                                1, stat) < 0)
                                return(ERR);
			if (listat.sw_error != 0) {
				_SETERROR(stat, listat.sw_error, 0)
				return(ERR);
			}
			nonbuf_bits = listat.sw_count * 8;
#endif
		}
		else {
			/*
			 * Seek to proper location
			 */
			if (XRCALL(llfio, seekrtn) llfio, bytoff, SEEK_SET,  stat) == ERR)
				return(ERR);
			if (rwmode == 'r'){
				ret = XRCALL(llfio, readrtn) llfio, bufptr, (size_t) nonbuf_bytes,
					stat, PARTIAL, &ubc);
			}
			else
				ret = XRCALL(llfio, writertn) llfio, bufptr, (size_t) nonbuf_bytes,
					stat, PARTIAL, &ubc);
			if (ret < 0)
				return(ret);
			nonbuf_bits = ret*8;
		}
	}
	if (rwmode == 'w' && (cpos+nonbuf_bits > cch_info->feof))
		cch_info->feof = cpos + nonbuf_bits;
	return(nonbuf_bits);
}
