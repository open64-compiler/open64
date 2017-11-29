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


static char USMID[] = "@(#) libu/ffio/c1/sdsread.c	92.0	10/08/98 14:57:41";


#include <stdio.h>
#include <malloc.h>
#include <ffio.h>
#include "../fssio.h"

#define SYNC 0
#define ASYNC 1
/*
 * Handle SDS read requests.
 * Transfer data from the SDS to the user's data area.
 *
 * Parameters:
 *      fio     - Pointer to fdinfo block
 *	bufptr	- bit pointer to where data is to go.
 *	nbytes	- Number of bytes to be read
 *	stat	- pointer to status return word
 *	fulp	- full or partial read mode flag
 *	ubc	- pointer to unused bit count
 */
_sds_read(fio, bufptr, nbytes, stat, fulp, ubc)
struct fdinfo *fio;
int nbytes, fulp, *ubc;
bitptr bufptr;
struct ffsw *stat;
	{
	return(__sds_read(fio, bufptr, nbytes, stat, fulp, ubc, SYNC));
	}
_sds_reada(fio, bufptr, nbytes, stat, fulp, ubc)
struct fdinfo *fio;
int nbytes, fulp, *ubc;
bitptr bufptr;
struct ffsw *stat;
	{
	return(__sds_read(fio, bufptr, nbytes, stat, fulp, ubc, ASYNC));
	}

__sds_read(fio, bufptr, nbytes, stat, fulp, ubc, sync)
struct fdinfo *fio;
int nbytes, fulp, *ubc;
bitptr bufptr;
struct ffsw *stat;
int sync;
	{
	long bits, moved, ret, blknum, blkoff, numblks;
	long nbits;	/* num of bits to move */
	long ovbits;
	long reqbits;
	long nb;
	long endreq;
	int locubc;
	int i, j;
	char *cp;
	struct sds_f *sds_info;
	struct ffsw locstat;
	_lociosw *locptr;

	sds_info = (struct sds_f *)fio->lyr_info;
	nbits = (nbytes << 3) - *ubc;

	if (nbits == 0)
		{
		SETSTAT(stat, FFCNT, 0);
		return(0);
		}
/*
 *	Don't try to reverse the buffer on switch from write to read.  Just
 *	flush it.
 */
	if (fio->rwflag == WRITIN)
		{
		/* read after write. Flush buffer. */
		if (fio->_cnt > 0)
			{
			SSWRITEM(ret, BPTR2WP(fio->_base),
				sds_info, sds_info->sdsbufblk, 1);
			if (ret < 0) goto sdsio;
			}
		fio->_cnt = 0;
		}
	fio->rwflag = READIN;
/*
 *	Guarantee that we will not go over the actual EOD on the file
 *	by reducing the request if necessary
 */
	if ((sds_info->fileptr + nbits) > sds_info->sdseof)
		{
		nbits = sds_info->sdseof - sds_info->fileptr;
		if (nbits <= 0) return(seteod(fio, stat));
		}

	reqbits = nbits;
/*
 *	Check for overflow.  Split the request if necessary.
 */
	endreq = sds_info->fileptr + nbits;
	ovbits = 0;
	moved = 0;
	if (endreq > sds_info->sdssize)
		{
		/* split the request... */
		ovbits =  endreq - sds_info->sdssize;
		if (ovbits > nbits) ovbits = nbits;
		nbits -= ovbits;
		/* if whole request in overflow... */
		if (sds_info->fileptr >= sds_info->sdssize) goto ovfl;
		}
/*
 *	Check for sector boundary, handle the leading partial sector
 */
	blkoff = BLOCKOFF(sds_info->fileptr);
	blknum = BLOCKNUM(sds_info->fileptr);
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
			sds_info->sdsbufblk = blknum;
			if (ret < 0) goto sdsio;
/*
 *			Position to proper place in buffer
 */
			fio->_ptr = fio->_base;
			SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, blkoff));
			fio->_cnt = (512*64) - blkoff;
			}
/*
 *		Move data from buffer to user
 */
		GETDATA(bufptr, fio, bits);
		SET_BPTR(bufptr, INC_BPTR(bufptr, bits));
		nbits -= bits;
		moved += bits;
		sds_info->fileptr += bits;
		if (fio->_cnt == 0) blknum++;
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
			SSREADM(ret, BPTR2WP(bufptr),
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
 *                      loop on writing N sectors and MOVBIT
 *                      from user buffer
 */
			if (BPTR2CP(sds_info->locbuf) == NULL)
				{
				cp = malloc(BLKS2BYTES(SECBSZ));
				if (cp == NULL)
					ERETURN(stat, FDC_ERR_NOMEM, 0);
			        SET_BPTR(sds_info->locbuf, CPTR2BP(cp));
				}
			j = (numblks-1)%SECBSZ + 1;
			for (i = 0 ; i < numblks ; i += SECBSZ)
				{
				SSREADM(ret, BPTR2WP(sds_info->locbuf),
					sds_info, blknum + i,j);
                                if (ret < 0) goto sdsio;

				bits = BLKS2BITS(j);
				MOV_BITS(bufptr, sds_info->locbuf, bits);

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
		bits = nbits;	/* This should always be */
/*
 *		The buffer does not contain the data in question, load it up
 */
		SSREADM(ret, BPTR2WP(fio->_base),
			sds_info, blknum, 1);
		if (ret < 0) goto sdsio;

		sds_info->sdsbufblk = blknum;
/*
 *		Position to proper place in buffer
 */
		fio->_ptr = fio->_base;
		fio->_cnt = 512*64;
/*
 *		Move data from buffer to user
 */
		GETDATA(bufptr, fio, bits);
		SET_BPTR(bufptr, INC_BPTR(bufptr, bits));
		nbits -= bits;
		moved += bits;
		sds_info->fileptr += bits;
		}
	if (ovbits != 0) goto ovfl;

done:
	ret = (moved + 7) >> 3;
	SETSTAT(stat, FFCNT, ret);
	fio->recbits += moved;
	*ubc = (ret << 3) - reqbits;
	return (ret);

sdsio:
	ERETURN(stat, FDC_ERR_SDSIO, 0);
erret:
	*stat = locstat;
	return(ERR);
ovfl:
	if (sds_info->fileptr == sds_info->sdssize)
		{
		ret = XRCALL(fio->fioptr, seekrtn) fio->fioptr,
			(sds_info->fileptr - sds_info->ovoff) >> 3, 0, &locstat);
		if (ret < 0)
			{
			goto erret;
			}
		}
	nb = (ovbits + 7) >> 3;
	locubc = (nb << 3) - ovbits;
	if (sync == SYNC)
		{
synchronous:
		ret = XRCALL(fio->fioptr, readrtn)
			fio->fioptr, bufptr, nb, &locstat, fulp, &locubc);
		if (ret < 0)
			{
			goto erret;
			}
		bits = (ret << 3) - locubc;
		moved += bits;
		sds_info->fileptr += bits;
		goto done;
		}
	else
		{
		/* asynchronous */
		locptr = _sds_locsw_set(sds_info, stat, moved);
		if (locptr == NULL)
			{
			goto synchronous;
			}
		ret = XRCALL(fio->fioptr, readartn)
			fio->fioptr, bufptr, nb, &(locptr->local_sw), fulp, 
			&locubc);
		if (ret < 0)
			{
			*stat = locptr->local_sw;
			_sds_locsw_clear(sds_info, locptr);
			return(ERR);
			}
		bits = (nb << 3) - locubc;
		sds_info->fileptr += bits;
		fio->recbits += moved + bits;
		return((moved + bits + 7 ) >> 3);
		}
	}

/*
 *	Common code to handle EOD return
 */
static
seteod(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
	{
	fio->segbits = 0;
	fio->recbits = 0;
	fio->ateof = 0;
	fio->ateod = 1;
	SETSTAT(stat, FFEOD, 0)
	return(0);
	}

/*
 * _sds_seek()
 * Perform a seek operation on SDS.
 */
_sds_seek(fio, pos, whence, stat)
struct fdinfo *fio;
int pos, whence;
struct ffsw *stat;
	{
	int ret;
	int ovpos;
	int newstat;
	long newfptr, base, limit, offset;
	struct sds_f *sds_info;

	sds_info = (struct sds_f *)fio->lyr_info;
/*
 *	Do special SDS class things
 */
	if (whence == 0)
		newfptr = pos << 3;
	else if (whence == 1)
		newfptr = sds_info->fileptr + (pos << 3);
	else if (whence == 2)
		newfptr = sds_info->sdseof + (pos << 3);
	else
		{
		ERETURN(stat, FDC_ERR_BADSK, 0);
		}
/*
 *	Set up FFSTAT for return, but don't set it yet.
 */
	newstat = FFCNT;
	if (newfptr == 0)
		newstat = FFBOD;
	else if (newfptr == sds_info->sdseof)
		newstat = FFEOD;
/*
 *	Make sure that the data in the local buffer gets flushed
 *	if the last operation was a write.  If the seek is outside the
 *	bounds of buffered data, invalidate the buffer.
 *	Else, make sure that _ptr and _cnt are set correctly.
 */
	if (fio->_cnt > 0)
		{
		if (fio->rwflag == WRITIN)
			{
			SSWRITEM(ret, BPTR2WP(fio->_base),
				sds_info, sds_info->sdsbufblk, 1);
			if (ret < 0) ERETURN(stat, FDC_ERR_SDSIO, 0);
			}
/*
 *		note that check is <= and not <.  SDS <-> memory
 *		is faster that memory <-> memory, so might as well
 *		flush if seeking to block boundary.  This also may allow
 *		A subsequent request that is large and well formed to fire
 *		unhindered, instead of having to flush the buffer first.
 */
		base = BLKS2BITS(sds_info->sdsbufblk);
		limit = base + 512*64 - 1;
		if (newfptr <= base || newfptr > limit)
			{
			fio->_cnt = 0;
			}
		else
			{
			offset = BLOCKOFF(newfptr);
			SET_BPTR(fio->_ptr, INC_BPTR(fio->_base, offset));
			fio->_cnt = 512*64 - offset;
			}
		}
/*
 *	If the EOF > size, then we have overflowed.  Make sure that the 
 *	lower level pointer is in the right spot.  If we have not yet
 *	overflowed, then we do *not* want to do the seek, as the end of
 *	SDS space might change on us, and this will affect the position
 *	on the underlying layer.  Note that we do the seek here, even though
 *	it may not be necessary.  Once overflow has occurred, any _sds_seek
 *	operation within the SDS space results in a seek on the lower level
 *	layer.  This should be fixed.
 */
	if (sds_info->overflowed == YES)
		{
		if (newfptr >= sds_info->sdssize)
			ovpos = (newfptr - sds_info->ovoff) >> 3;
		else
			ovpos = (sds_info->sdssize - sds_info->ovoff) >> 3;
		ret = XRCALL(fio->fioptr, seekrtn) fio->fioptr, ovpos, 0, stat);
		if (ret < 0) return(ERR);
		}
	sds_info->fileptr = newfptr;
	fio->rwflag = POSITIN;
	fio->ateof = 0;
	fio->ateod = 0;
	fio->recbits = 0;
	SETSTAT(stat, newstat, 0);
	return ((newfptr + 7) >> 3);
	}

/*
 * _sds_readblks()
 *	Read blocks from the SDS space into memory.
 *	Use the slice descriptor array in sds_info->sdsalo and split up
 *	the request as necessary.
 */
_sds_readblks(mem, sds_info, sds, blks)
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
			ret = SSREAD(mem, asds, req);
			if (ret < 0) return(ERR);
			mem += 512 * req;
			ct -= req;
			sds += req;
			if (ct == 0) return(blks);
			}
		}
/*
 *	If we fall through the loop, we have more data to read than
 *	there is SDS allocated!  serious trouble.
 */
	return(ERR);
	}

#ifdef DEBUG
ssread_dbg(mem, sds, blks)
int mem, sds, blks;
        {
        extern int __fdctrace_enable; 
        int ret, ret0; 
 
        ret0 = ssread(mem, sds, blks);
        if (FDCTRACE(TRACE_SDSIO))
                {
                _xrt_putf("SSREAD: mem=%x, sds=%x, blks=%x\n",mem, sds, blks);
                }
        return(ret0); 
        }
#endif
