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


static char USMID[] = "@(#) libu/ffio/c1/sdswrite.c	92.0	10/08/98 14:57:41";

#include <malloc.h>
#include <stdio.h>
#include <ffio.h>
#include "../fssio.h"

#define SYNC 0
#define ASYNC 1

/*
 * Write data to SDS.
 *	Do a little buffering to prevent the user from having to do I/O in
 *	512 word multiples
 *
 * Parameters:
 *      fio     - Pointer to fdinfo block
 *	bufptr	- bit pointer to where data is to go.
 *	nbytes	- Number of bytes to be written
 *	stat	- pointer to status return word
 *	fulp	- full or partial write mode flag
 *	ubc	- pointer to unused bit count 
 */
_sds_write(fio, bufptr, nbytes, stat, fulp, ubc)
struct fdinfo *fio;
int nbytes, fulp, *ubc;
bitptr bufptr;
struct ffsw *stat;
	{
	return(__sds_write(fio, bufptr, nbytes, stat, fulp, ubc, SYNC));
	}

_sds_writea(fio, bufptr, nbytes, stat, fulp, ubc)
struct fdinfo *fio;
int nbytes, fulp, *ubc;
bitptr bufptr;
struct ffsw *stat;
	{
	return(__sds_write(fio, bufptr, nbytes, stat, fulp, ubc, ASYNC));
	}

__sds_write(fio, bufptr, nbytes, stat, fulp, ubc, sync)
struct fdinfo *fio;
int nbytes, fulp, *ubc;
bitptr bufptr;
struct ffsw *stat;
int sync;
	{
	int i, j, ret, blkoff;
	char *cp;
	struct fdinfo *llfio;
	int bits, moved, nbits;
	long endreq, numblks, blknum;
	long ovbits;
	int locubc;
	int sdsinc;
	int seekpos;
	int nb;
	struct sds_f *sds_info;
	struct ffsw locstat;
	_lociosw *locptr;

	nbits = (nbytes << 3) - *ubc;

	sds_info = (struct sds_f *)fio->lyr_info;
/*
 *	If NOT writing or just positioned, then switch
 *	direction in the buffer
 */
	if (fio->rwflag != WRITIN)
		{
		if (fio->_cnt > 0)
			{
			fio->_cnt = fio->_ffbufsiz - fio->_cnt;
			}
		}
/*
 *	If the request causes the file to be extended, up the SDS allocation
 */
	moved = 0;
	ovbits = 0;
	endreq = sds_info->fileptr + nbits;
	if (endreq > sds_info->sdssize)
		{
		if (sds_info->overflowed == NO)
			{
			sdsinc = endreq - sds_info->sdssize;
			ret = 1;		/* assume overflow */
/*
 *			If not yet at max size, call grow routine
 */
			if (sds_info->sdssize < BLKS2BITS(sds_info->maxsize))
				{
				ret = _sds_grow(fio, sdsinc, &locstat);
				if (ret < 0) 
					{
					goto erret;
					}
				}
/*
 *			ret > 1 means that either we are already at max
 *			allocation on SDS, or the grow routine indicated
 *			overflow.  (Return code >0 from _sds_grow means we
 *			overflowed.) First, figure out what portion of the
 *			request must be passed to the lower layer, then go
 *			ahead and complete the request if part can reside in
 *			SDS.  A return of ret == 0 means the grow was OK, and
 *			we now have enough space.
 */
			if (ret > 0)
				{
/*
 *				Do one-time overflow processing
 */
				ret = _fss_overflow(fio, &locstat);
				if (ret < 0)
					{
					goto erret;
					}
	
				/* Assume that entire request is in overflow */
				ovbits = nbits;
				seekpos = (sds_info->fileptr - sds_info->ovoff) >> 3;
				if ((endreq - sds_info->sdssize) <= nbits)
					{
					/* request split across end of alloc */
					ovbits = endreq - sds_info->sdssize;
					seekpos = (sds_info->sdssize - sds_info->ovoff) >> 3;
					}
/*
 *				Position the lower layer to where we will need
 *				to be when the lower level I/O kicks in.
 */
				llfio = fio->fioptr;
				ret = XRCALL(llfio, seekrtn)
						llfio, seekpos, 0, &locstat);
				if (ret < 0)
					{
					goto erret;
					}
/*
 *				If the request is completely outside
 *				the SDS area, jump to the ovfl code directly.
 */
				nbits -= ovbits;
				if (nbits == 0) goto ovfl;
				}
			/* fall thru here means split req, or grow was enuf */
			}
		else	/* already overflowed */
			{
			llfio = fio->fioptr;
/*
 *			Divide the request between the SDS and the ovfl part.
 *			Lower layer should already be at proper
 *			position, so seek is not necessary.
 */
			ovbits = nbits;		/* Assume entirely in ovflow */
/*
 *			If request is split across end of alloc, ovbits
 *			is just the overflow part.
 */
			if ((endreq - sds_info->sdssize) <= nbits)
				ovbits = endreq - sds_info->sdssize;
/*
 *			If the request is completely outside
 *			the SDS area, jump to the ovfl code directly.
 */
			nbits -= ovbits;
			if (nbits == 0) goto ovfl;
			}
		}

	fio->rwflag = WRITIN;
	sds_info->sdsdirty = 1;	/* this should be only set for actual sswrt? */

	blkoff = BLOCKOFF(sds_info->fileptr);
	blknum = BLOCKNUM(sds_info->fileptr);
/*
 *	Check for sector boundary, handle the leading partial sector
 */
	if (blkoff != 0)
		{
		bits = 512*64 - blkoff;
		if (bits > nbits) bits = nbits;
/*
 *		If the buffer does not contain the data in question, load it up
 */
		if (blknum != sds_info->sdsbufblk || fio->_cnt == 0)
			{
			SSREADM(ret, BPTR2WP(fio->_base),
				sds_info, blknum, 1);
			if (ret < 0) goto sdsio;
			sds_info->sdsbufblk = blknum;
/*
 *			Position to proper place in buffer
 */
			fio->_ptr = fio->_base;
			SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, blkoff));
			fio->_cnt = blkoff;
			}
/*
 *		Move data from user to buffer
 */
		PUTDATA(bufptr, fio, bits);
		SET_BPTR(bufptr, INC_BPTR(bufptr, bits));
		nbits -= bits;
		moved += bits;
		sds_info->fileptr += bits;
		if (fio->_cnt == 512*64)
			{
			SSWRITEM(ret, BPTR2WP(fio->_base),
				sds_info, blknum, 1);
			if (ret < 0) goto sdsio;
			fio->_cnt = 0;
			blknum++;
			}
		}
/*
 *	Move the meat of the request
 */
	numblks = BITS2BLKS(nbits);
	if (numblks > 0)
		{
		if (BPBITOFF(bufptr) == 0)
			{
			bits = BLKS2BITS(numblks);
			SSWRITEM(ret, BPTR2WP(bufptr),
					sds_info, blknum, numblks);
			if (ret < 0) goto sdsio;

			SET_BPTR(bufptr, INC_BPTR(bufptr, bits));
			nbits -= bits;
			moved += bits;
			sds_info->fileptr += bits;
			}
		else
			{
/*
 *			loop on reading N sectors and MOVBIT
 *			to user buffer
 */
			if (BPTR2CP(sds_info->locbuf) == (char *)0)
				{
				cp = malloc(BLKS2BYTES(SECBSZ));
				if (cp == NULL)
					ERETURN(stat, FDC_ERR_NOMEM, 0);
				SET_BPTR(sds_info->locbuf, CPTR2BP(cp));
				}
			j = (numblks-1)%SECBSZ + 1;
			for (i = 0 ; i < numblks ; i += SECBSZ)
				{
				bits = BLKS2BITS(j);
				MOV_BITS(sds_info->locbuf, bufptr, bits);
				SSWRITEM(ret, BPTR2WP(sds_info->locbuf),
					sds_info, blknum + i,j);
				if (ret < 0) goto sdsio;

				j = SECBSZ;
				SET_BPTR(bufptr, INC_BPTR(bufptr, bits));
				nbits -= bits;
				moved += bits;
				sds_info->fileptr += bits;
				}
			}
		blknum += numblks;
		}
/*
 *	Check for sector boundary again.  handle tail end
 */
	if (nbits != 0)
		{
#ifdef DEBUG
		blkoff = BLOCKOFF(sds_info->fileptr);
		if (blkoff != 0) abort();
#endif
		bits = nbits;	/* This should always be < 1 sect !!!*/
/*
 *		The buffer does not contain the data in question, load it up
 */
		SSREADM(ret, BPTR2WP(fio->_base),
			sds_info, blknum, 1);
		sds_info->sdsbufblk = blknum;
		if (ret < 0) goto sdsio;
/*
 *		Position to proper place in buffer
 */
		fio->_ptr = fio->_base;
		fio->_cnt = 0;
/*
 *		Move data from user to buffer
 */
		PUTDATA(bufptr, fio, bits);
		SET_BPTR(bufptr, INC_BPTR(bufptr, bits));
		nbits -= bits;
		moved += bits;
		sds_info->fileptr += bits;
		}
ovfl:
	if (ovbits > 0)
		{
		nb = (ovbits + 7) >> 3;
		locubc = (nb << 3) - ovbits;
		if (sync == SYNC)
			{
synchronous:
			ret = XRCALL(llfio, writertn) llfio,
				bufptr, nb, &locstat, fulp, &locubc);
			if (ret < 0)
				{
				goto erret;
				}
			moved += ovbits;
			sds_info->fileptr += ovbits;
			}
		else
			{
			locptr = _sds_locsw_set(sds_info, stat, moved);
			if (locptr == NULL)
				{
				goto synchronous;
				}
			ret = XRCALL(llfio, writeartn) llfio,
				bufptr, nb, &(locptr->local_sw), fulp, &locubc);
			if (ret < 0) 
				{
				*stat = locptr->local_sw;
				_sds_locsw_clear(sds_info, locptr);
				return(ERR);
				}
			sds_info->fileptr += ovbits;
			if (sds_info->fileptr > sds_info->sdseof)
				sds_info->sdseof = sds_info->fileptr;
			fio->recbits += moved + ovbits;
			return(nbytes);
			}
		}
done:
	if (sds_info->fileptr > sds_info->sdseof)
		sds_info->sdseof = sds_info->fileptr;
	fio->recbits += moved;
	SETSTAT(stat, FFCNT, moved >> 3);
			
	return ((moved + 7) >> 3);
	
sdsio:
	ERETURN(stat, FDC_ERR_SDSIO, 0);
erret:
	*stat = locstat;
	return(ERR);
	}

/*
 * Flush the buffer and clean up
 *	This routine should return 0, or -1 on error.
 */
int
_sds_flush(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
	{
	struct sds_f *sds_info;
	int ret;

	sds_info = (struct sds_f *)fio->lyr_info;
/*
 *	if reading, or just positioned, nothing to do...
 */
	if (fio->rwflag != WRITIN)
		{
		fio->_ptr = fio->_base;
		fio->_cnt = 0;
		fio->segbits = 0;
		fio->scc = FULL;
		fio->lastscc = FULL;
		return(0);
		}
/*
 *	In write mode.  write out the buffer
 */
	if (fio->_cnt != 0)
		{
		SSWRITEM(ret, BPTR2WP(fio->_base),
				sds_info, sds_info->sdsbufblk, 1);
		if (ret < 0) ERETURN(stat, FDC_ERR_SDSIO, 0);
		}
	fio->_cnt = 0;
	return(0);
	}

/*
 * Write an EOD.
 *	Truncate the file.  This is the 'real' end of the data.
 */
int
_sds_weod(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
	{
	int ret;
	struct sds_f *sds_info;

	sds_info = (struct sds_f *)fio->lyr_info;
	if (fio->_cnt > 0)
		{
		ret = XRCALL(fio, flushrtn) fio, stat);
		if (ret < 0) return(ret);
		}

	sds_info->sdseof = sds_info->fileptr;
	if (fio->fioptr != NULL && sds_info->sdseof > sds_info->sdssize)
		{
		ret = XRCALL(fio->fioptr, weodrtn) fio->fioptr, stat);
		if (ret < 0) return(ERR);
		}

	fio->rwflag = WRITIN;
	sds_info->sdsdirty = 1;
	fio->ateof = 0;
	fio->ateod = 1;
	fio->recbits = 0;
	return(0);
	}

/*
 * _sds_writeblks()
 *	Read blocks from the SDS space into memory.
 *	Use the slice descriptor array in sds_info->sdsalo and split up
 *	the request as necessary.
 */
_sds_writeblks(mem, sds_info, sds, blks)
long *mem;
struct sds_f *sds_info;
int sds, blks;
	{
	int i;
	int ct;
	int rb;
	int req;
	int ret;
	int asds;	/* actual sds address */
	struct sds_alo_s *alop;

	alop = sds_info->sdsalo;
	ct = blks;
	for (i = 0 ; i < sds_info->sdsalnum ; i++)
		{
		rb = alop[i].relbase;
		if (rb <= sds && (rb + alop[i].size) > sds)
			{
			req = (rb + alop[i].size) - sds;
			if (req > ct) req = ct;
			asds = alop[i].base + (sds - rb);
			ret = SSWRITE(mem, asds, req);
			if (ret < 0) return(ERR);
			mem += 512 * req;
			ct -= req;
			sds += req;
			if (ct == 0) return(blks);
			}
		}
/*
 *	If we fall through the loop, we have more data to write than
 *	there is SDS allocated!  serious trouble.
 */
	return(ERR);
	}

#ifdef DEBUG
sswrite_dbg(mem, sds, blks)
int mem, sds, blks;
	{
	extern int __fdctrace_enable;
	int ret, ret0;

	ret0 = sswrite(mem, sds, blks);
	if (FDCTRACE(TRACE_SDSIO))
		{
		_xrt_putf("SSWRITE: mem=%x, sds=%x, blks=%x\n",mem, sds, blks);
		}
	return(ret0);
	}
#endif
