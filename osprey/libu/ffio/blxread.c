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


#pragma ident "@(#) libu/ffio/blxread.c	92.2	08/18/99 09:55:58"

#include <stdio.h>
#include <memory.h>
#include <ffio.h>
#include "blxio.h"

/*
 * Global definitions.
 */
static int 	zero		= 0;

static 	int 	get_segment(), skip2eor();

/*
 * Read a blx "record"
 *
 *	In this layer "records" are internally thought of as the number of
 *	bits to the next blank expansion character, but to the caller
 *	there is no such thing as a FULL "blx" record.
 *
 *	"records" and blocks are supported only in multiples of 8 bits.
 *	There is no logical limit for the maximum record size.  The 
 *	maximum block size is determined arbitrarily to be 512 words.
 *
 * Parameters:
 *      fio     - fdinfo block pointer
 *      bufptr  - bit pointer to where data is to go.
 *      nbytes  - number of bytes to be read
 *      stat    - pointer to status return word
 *      fulp    - full or partial write mode flag
 *      ubc     - pointer to unused bit count (not used)
 */
ssize_t
_blx_read(struct fdinfo *fio, bitptr bufptr, size_t nbytes, struct ffsw *stat, int fulp, int *ubc)
	{
	int64 	skip_len,
		nbits, 
		bits,
		movdbits; 
	int     eorstat,
		ret;
	struct	blx_info *blx_dat;

	nbits = (uint64)nbytes << 3;	/* convert bytes to bits */
	movdbits = 0;		        /* number of bits given to caller */
	if (*ubc != 0)		        /* ubc should always be zero */
		ERETURN(stat, FDC_ERR_UBC, 0);

	if (fio->rwflag == WRITIN) 
		{ 
		/* read after write error */ 
		ERETURN(stat, FDC_ERR_RAWR, 0); 
		} 

	fio->rwflag = READIN;  /* set operation type */

/*
 *	Get blx_info.
 */
	blx_dat = (struct blx_info *)fio->lyr_info;

/*
 *	Loop until one of the following occurs:
 *		- the caller's request of nbits is satisfied
 *		- an EOF/EOD is found
 */
	while ( movdbits < nbits ) /* while caller is not satisfied */
		{
		bits = nbits - movdbits;	/* bits to move this pass */
/*
 * 		If segment is empty and we're all out of blanks, get the 
 *		next segment.
 */
		if ((fio->segbits == 0) && (blx_dat->u_blx == 0))
			{
			ret = get_segment(fio, stat);
			if (ret != 0)
				{
				if ((ret == FFEOF) || (ret == FFEOD))
					{
					if (movdbits != 0)
						{
						blx_dat->eor = _TRUE;
						goto chkdone;
						}
					fio->ateof = (ret == FFEOF ? 1:0);
					fio->ateod = (ret == FFEOD ? 1:0);
					fio->segbits = 0;
					SETSTAT(stat, ret, 0)
					return(0);
					}
				else
					return(ERR); /* Status should be set */
				}

			}
/*
 *		If part of the record is requested, decrement counters and
 *		return the requested bits (bytes) to the caller's buffer.
 */

		if (fio->segbits >= bits)
			{
			GETDATA(bufptr, fio, bits);		
			movdbits += bits;
			SET_BPTR(bufptr, INC_BPTR(bufptr, bits));
			}
/*
 *		If more bits are requested than are in the segment, return
 *		segment.  If the scc (segment operation from get_segment())
 *		equals SCCFULL and all the blanks have been delivered, get
 *		the next record.
 *
 *		If the number of bits that the caller wants is less than the
 *		number of blanks, then fill the request with blanks and 
 *		decrement the unused blank count (blx_dat->u_blx).
 *
 *		If the scc equals SCCMIDL (i.e., the fio buffer is empty and 
 *		no blx_dat->blx_char was found) put available data into callers 
 *		buffer and go on.
 *
 *		If the scc equals SCCLAST, (i.e., a blx_dat->blx_char was
 *		found but the number of blanks is not available in the current
 *		buffer) do functions similar to SCCFULL but only increment
 *		fio pointers one byte.
 *
 *		If the scc equals SCCFRST, (i.e., last return was SCCLAST and
 *		we now have the number of blanks), continue like SCCLAST. 
 */
		else
			{
			if (fio->segbits != 0)
				{
				bits = fio->segbits;
				GETDATA(bufptr, fio, bits);
				movdbits += bits;
				SET_BPTR(bufptr, INC_BPTR(bufptr, bits));
				}
			switch (fio->scc)
				{
				case SCCLAST:
				case SCCFRST:
				case SCCFULL:
					if (blx_dat->u_blx == 0)
						{
						fio->segbits = 0;
						}
					else
						{
						bits = nbits - movdbits;
						INSRT_BL(ret,bufptr,blx_dat,bits);
						movdbits += ret;
						}
					if (blx_dat->u_blx == 0)
						{
						if (fio->scc != SCCFULL)
							{
							skip_len = CHAR_LEN;
							/* 
							 * skip blx_dat->blx_char
							 * or next byte
							 *
							 */
							}
						else
							{
							skip_len = 2 * CHAR_LEN;
							/*
							 * skip both 
							 * blx_dat->blx_char
							 * and following byte
							 *
							 */
							}
						fio->_cnt -= skip_len;
						SET_BPTR(fio->_ptr, 
						  INC_BPTR(fio->_ptr, skip_len));
						}
					break;
				case SCCMIDL:
					break;
				default:
					ERETURN(stat, FDC_ERR_INTERR, 
						fio->segbits);
				}
			} 

		if (fio->_cnt == 0 && blx_dat->eor == _TRUE)
			{
			nbits = movdbits;
			}

		} /* end while */

chkdone:
		eorstat = FFCNT;
		if (fio->_cnt == 0 && blx_dat->eor == _TRUE)
			eorstat = FFEOR;

		if (fulp == FULL && (blx_dat->llblocked))
			{
			ret = skip2eor(fio, stat);
			if (ret < 0)
				return(ERR);
			}
		SETSTAT(stat, eorstat, (uint64)movdbits >> 3);
		return ((uint64)movdbits >> 3);

	} /* end of _blx_read */

/* 
 * get_segment grabs another "record" segment from the data stream.
 * fio->scc will be set as follows:
 *	SCCMIDL		fio->segbits = fio_cnt (all or rest of data in fio
 *			buffer) and no blx_dat->blx_char was found
 *	SCCFULL		a blx_dat->blx_char was found
 *	SCCLAST		The blx_dat->blx_char is the last byte in the buffer!
 *			Watch your step.
 *	SCCFRST		Don't like this either - the last return from 
 *			get_segment was SCCLAST, so the first byte minus
 *			blx_dat->blx_off is the number of blanks left.
 *
 * return values are as follows:
 *	FFEOF	encountered end of file
 *	FFEOD	encountered end of data
 *	0	segment (or part) is received
 *	ERR	encountered an error -> stat will be set
 */
static int
get_segment(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
	{
	int 	ckbits,
		tbits;
	ssize_t ret;

	unsigned char 	*cp, 
			*tptr;
	struct	blx_info *blx_dat;
		
/*
 *	Get blx_info.
 */
	blx_dat = (struct blx_info *)fio->lyr_info;

	if (fio->_cnt == 0) /* fio buffer is empty */
		{
		/* Reset _ptr and get a block of data. */
		fio->_ptr = fio->_base;
		READBLK(ret, fio, (fio->maxblksize >> 3), stat, PARTIAL, &zero);

		if (ret < 0)
			{
			return(ERR);
			}
/*
 *		Check the stat returned from the read and set blx_dat->eor or 
 *		return appropriately.
 */
		switch (stat->sw_stat)
			{
			case FFEOR:
				blx_dat->eor = _TRUE;
				break;
			case FFCNT:
				blx_dat->eor = _FALSE;
				break;
			case FFEOF:
				return(FFEOF);
			case FFEOD:
				return(FFEOD);
			default:
				return(ERR);
			}
		}

/*
 *	If we returned last time wtih fio->scc=SCCLAST, the first byte the
 *	fio buffer contains the number of blanks in the segment.  Return
 *	with blx_dat->u_blx set, fio->scc=SCCFRST, and fio->segbits=0.
 */
	if (fio->scc == SCCLAST)
		{
		fio->scc = SCCFRST;
		fio->segbits = 0;
		tptr = (unsigned char *)BPTR2CP(fio->_ptr);
		blx_dat->u_blx = *tptr - blx_dat->blx_off;
		return(0);
		}
/*
 *	Look for next blx_dat->blx_char. Use number in following byte minus 
 *	blx_dat->blx_off (bias) as the number of blanks to pad.	
 */
	tptr = (unsigned char *)BPTR2CP(fio->_ptr);
	cp = memchr(tptr, ((unsigned long)blx_dat->blx_char >> 56), (fio->_cnt >> 3));
	if (cp != NULL)
		{
		tbits = (cp - tptr) << 3;
		ckbits = fio->_cnt - tbits;
		}
	else
		{
		ckbits = 0;
		}

	switch (ckbits)
		{
		case 0:		/* should be SCCMID */
			if (cp != NULL)	/* something is very wrong */
				ERETURN(stat, FDC_ERR_INTERR, fio->segbits);
			fio->segbits = fio->_cnt;
			fio->scc = SCCMIDL;
			blx_dat->u_blx = 0;
			break;
		case 8:		/* should be SCCLAST */
			if (cp != NULL)	/* blx_dat->blx_char is last byte */
				{
				fio->segbits = tbits;
				fio->scc = SCCLAST;
				blx_dat->u_blx = 0; /* ...till next call */ 
				}
			else /* shouldn't be like this */
				ERETURN(stat, FDC_ERR_INTERR, fio->segbits);
			break;	
		default:	/* should be SCCFULL */
			if (cp != NULL)	/* blx_dat->blx_char found */
				{
				fio->segbits = tbits;
				fio->scc = SCCFULL;
				blx_dat->u_blx = *(cp + 1) - blx_dat->blx_off; 
				}
			else /* shouldn't be like this */
				ERETURN(stat, FDC_ERR_INTERR, fio->segbits);
		}

	return(0);

	} /* end of get_segment */

static int 
skip2eor(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
	{
	int ret;
	struct	blx_info *blx_dat;
		
/*
 *	Get blx_info.
 */
	blx_dat = (struct blx_info *)fio->lyr_info;
	if (fio->segbits > 0)
		{
		SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, fio->segbits));
		fio->_cnt -= fio->segbits;
		fio->segbits = 0;
		}	
	while (blx_dat->eor != _TRUE)
		{
		ret = get_segment(fio, stat);
		if ((ret < 0) || (ret == FFEOF) || (ret == FFEOD))
			return(ret);
		SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, fio->segbits));
		fio->_cnt -= fio->segbits;
		fio->segbits = 0;
		}
	
	}
		
/*
 * 	blx_seek()
 * 	Allow the user to rewind his foreign file.  No other positioning is
 * 	allowed.
 */
_ffseek_t
_blx_seek(fio, pos, whence, stat)
struct fdinfo *fio;
off_t pos;
int whence;
struct ffsw *stat;
	{
	struct fdinfo *llfio;
	_ffseek_t ret;

/*
 * Only allow a rewind to zero.
 */

	if (pos != 0 || whence != 0)
		ERETURN(stat, FDC_ERR_NOSUP, 0);

	ret = XRCALL(fio, flushrtn) fio, stat);
	if (ret < 0)
		return(ret);
/*
 * Set up bookkeeping stuff for blx records.
 */
	fio->scc = SCCFULL;
/*
 * Ask lower layer to do seek and return.
 */
	llfio = fio->fioptr;
	ret = XRCALL(llfio, seekrtn) llfio, pos, whence, stat);
	if (ret < 0)
		return(ret);
	fio->rwflag = POSITIN;
	fio->ateof = 0;
	fio->ateod = 0;
	fio->_ptr = fio->_base;
	fio->_cnt = 0;
	fio->segbits = 0;
	SETSTAT(stat, FFBOD, 0);
	return (ret);
	}
