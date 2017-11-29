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


#pragma ident "@(#) libu/ffio/txtread.c	92.1	06/29/99 13:16:47"

#include <stdio.h>
#include <memory.h>
#include <ffio.h>
#include "txtio.h"

static int	zero;

static 	int 	get_segment(struct fdinfo *fio, struct ffsw *stat),
	 	skip2eor(struct fdinfo *fio, struct ffsw *stat), 
		setend(struct fdinfo *fio, struct ffsw *stat);
/*
 * Read TEXT records
 *
 *	Records and blocks are supported only in multiples of 8 bits.
 *	There is no logical limit for the maximum record size.  The 
 *	maximum block size is determined arbitrarily to be 512 words.
 *
 * Parameters:
 *      fio     - Pointer to fdinfo block
 *      bufptr  - bit pointer to where data is to go.
 *      nbytes  - number of bytes to be read
 *      stat    - pointer to status return word
 *      fulp    - full or partial write mode flag
 *      ubc     - pointer to unused bit count (not used)
 */
ssize_t
_txt_read(struct fdinfo *fio, bitptr bufptr, size_t nbytes, struct ffsw *stat,int fulp, int *ubc)
	{
	int64 	nbits, 
		bits,
		movdbits;
	int	ret,
		eorstat;

	nbits = (uint64)nbytes << 3;	/* convert bytes to bits */
	movdbits = 0;
	if (*ubc != 0)		/* ubc should always be zero */
		ERETURN(stat, FDC_ERR_UBC, 0);

	/* read after write error */ 
	if (fio->rwflag == WRITIN) 
		ERETURN(stat, FDC_ERR_RAWR, 0); 

	fio->rwflag = READIN;  /* set operation type */
/*
 * 	If segment is empty, get the next segment.
 */
	fio->ateor = 0;
	if (fio->segbits == 0)
		{
		ret = get_segment(fio, stat);
		/* if EOF or EOD found */
		if (ret > 0)
			return(0);
		if (ret < 0)
			return(ret); /* stat set by get_segment */
		}
/*
 *	Loop until one of the following occurs:
 *		- the caller's request of nbits is satisfied
 *		- an EOR is found
 *		- an EOF is found
 *		- an EOD is found
 */

	eorstat = FFCNT;
	while ( nbits > 0 ) /* while caller is not satisfied */
		{
/*
 *		If more bits are requested than are in the segment, return
 *		segment.  If the scc (segment operation from get_segment())
 *		equals SCCFULL then return (i.e., hit end-of-record (EOR)).
 *		If the scc equals SCCMIDL (i.e., the fio buffer is empty and 
 *		no EOR was hit) subtract the number of bits moved from nbits
 *		and go on.
 */
		bits = nbits;
		if (fio->segbits < nbits)
			bits = fio->segbits;

		GETDATA(bufptr, fio, bits);
		movdbits += bits;
		SET_BPTR(bufptr, INC_BPTR(bufptr, bits));
		nbits -= bits; 

		if (fio->segbits == 0)
			{
			if (fio->scc == SCCFULL)
				{
				nbits = 0; /* return anyway */
				eorstat = FFEOR;
				}
			else
				{
				ret = get_segment(fio, stat);
				/* if EOF or EOD found */
				if (ret > 0)
					return(0);
				if (ret < 0)
					return(ret); /* stat set by get_segment */
				}
			}

		} /* end while */
/*
 *	Set status now, before doing any skip to EOR
 *	Must check EOR status again...
 */
	if ((fio->segbits == 0) && (fio->scc == SCCFULL))
		eorstat = FFEOR;

	fio->recbits += movdbits;
/*
 *	If the mode is FULL and more bits are
 *	available in the current record,  -or- if
 *	the number of bits requested just happpened to
 *      be the number of bits in the record,  skip to the next EOR. 
 *	If EOF/EOD found while skipping, set the status (this is an error)
 *	and return.
 */
	if ((fulp == FULL) || (eorstat == FFEOR))
		{
		ret = skip2eor(fio, stat);
		if (ret > 0) return(0);	/* EOF/EOD */
		if (ret < 0) return(ERR); /* Status should be set */
		fio->last_recbits = fio->recbits;
		fio->recbits = 0;
		}

	SETSTAT(stat, eorstat, (uint64)movdbits >> 3); /* assume CNT */
	return ((uint64)movdbits >> 3);

	} /* end of _txt_read */

/* 
 * get_segment grabs another record segment from the data stream.
 * fio->scc will be set as follows:
 *	SCCMIDL		fio->segbits = fio_cnt (all or rest of data in fio
 *			buffer) and no EOR was found
 *	SCCFULL		an EOR was found
 *
 * return values are as follows:
 *	2	encountered end of data -> stat will be set
 *	1	encountered end of file -> stat will be set
 *	0	segment (or part) is received -> stat will NOT be set
 *	ERR	encountered an error -> stat will be set
 */
static int
get_segment(struct fdinfo *fio, struct ffsw *stat)
	{
	long 	tword;
	int	left; 
	ssize_t	ret;

	unsigned char 	*cp;

	bitptr tbptr;

	struct text_f *text_info;

	text_info = (struct text_f *)fio->lyr_info;
	fio->lastscc = fio->scc;
/*
 *	If buffer is empty, or not enough to hold entire EOF marker,
 *	get more data.
 */
	if (fio->_cnt == 0 || fio->_cnt < text_info->eof_len)
		{
/*
 *		If num of bits not enough to hold EOF, move remainder
 *		to base of buffer and read in at base+remainder.  Adjust
 *		pointers and counts accordingly.
 */
		left = 0;
		if (fio->_cnt > 0)
			{
			bitptr tptr;

			left = fio->_cnt;
/*
 *			Move tail of data to the first word of the
 *			buffer (right justified).
 */
			GET_BITS(tword, fio->_ptr, left);
			SET_BPTR(tptr, fio->_base);
			PUT_BITS(tptr, tword, left);
			SET_BPTR(fio->_ptr, INC_BPTR(fio->_base, left));
			}
		else
			fio->_ptr = fio->_base;	/* reset _ptr */
		zero = 0;
		READBLK(ret, fio, (size_t)((uint64)fio->maxblksize >> 3), 
			stat, PARTIAL, &zero);
/*
 *		Add back in the 'extra' data
 */
		fio->_ptr = fio->_base;	/* reset _ptr */
		fio->_cnt = fio->_cnt + left;

		if (ret < (ssize_t)0)
			return(ERR);
		if (zero != 0)
			ERETURN(stat, FDC_ERR_UBC, 0);

		if (fio->_cnt == 0) /* must be at EOD */
			{
			return(setend(fio, stat));
			}
		}

/*
 *      EOF mark includes eor_char, if appropriate.  Check for this case first.
 */
	if (fio->rtype != TEXT_NL)
		{
		GET_BITS(tword, fio->_ptr, text_info->eof_len);
		if ((fio->lastscc == SCCFULL) && (tword == text_info->eof_mark))
			{
			SETSTAT(stat, FFEOF, 0);
			/* Skip EOF mark */
			SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, text_info->eof_len));
			fio->_cnt -= text_info->eof_len;
			fio->scc = SCCFULL;
			fio->segbits = 0;
			fio->last_recbits = fio->recbits;
			fio->recbits = 0;
			fio->ateof = 1;
			fio->ateod = 0;
			return(1);
			}
		}

/*
 *	Look for next EOR mark.
 */
	cp = memchr(BPTR2CP(fio->_ptr),
		 (int)((unsigned long) text_info->eor_char >> CHAR_JUSTIFY),
			(size_t)(fio->_cnt >> 3));
	if (cp != NULL)	/* EOR found */
		{
		SET_BPTR(tbptr, CPTR2BP(cp));
		fio->segbits = SUBT_BPTR(tbptr, fio->_ptr);
		fio->scc = SCCFULL;
		}
	else		/* EOR NOT found */
		{
		fio->segbits = fio->_cnt;
		fio->scc = SCCMIDL;
		}
	return(0);

	} /* end of get_segment */

		
/*
 * skip2eor skips to the next EOR mark and sets things up to point to the 
 * end of the record.
 *
 * possible input conditions are:
 *	fio->segbits > 0
 *	fio->segbits = 0
 *
 * possible return conditions are:
 *	2	EOD found before EOR (not expected)
 *	0	EOR found; _ptr and _cnt set appropriately
 *	ERR	some error
 */
static int
skip2eor(struct fdinfo *fio, struct ffsw *stat)
	{
	int 	ret,
		eorl;
	struct text_f *text_info;

	text_info = (struct text_f *)fio->lyr_info;
	if (fio->segbits > 0)
		{
		SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, fio->segbits));
		fio->_cnt -= fio->segbits;
		}
/*
 *	Loop until next EOR mark. 
 */
	while (fio->scc != SCCFULL)
		{
		ret = get_segment(fio, stat);
		if (ret > 0) return(0);
		if (ret < 0) return(ERR);

		SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, fio->segbits));
		fio->_cnt -= fio->segbits;

		} /* end while */
/*
 *	Eat the EOR character
 */
	eorl = text_info->eor_len;
	SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, eorl));
	fio->_cnt -= eorl;
	fio->segbits = 0;
	fio->ateor = 1;

	return(0);

	} /* end of skip2eor */

/*
 *	Common code to handle EOF and EOD return
 */
static
int
setend(struct fdinfo *fio, struct ffsw *stat)
	{
	fio->segbits = 0;
	fio->last_recbits = fio->recbits;
	fio->recbits = 0;
	if (stat->sw_stat == FFEOF)
		{
		fio->ateof = 1;
		fio->ateod = 0;
		return(1);
		}
	else if (stat->sw_stat == FFEOD)
		{
		fio->ateof = 0;
		fio->ateod = 1;
		return(2);
		}
	else
		ERETURN(stat, FDC_ERR_INTERR, 0);
	}
		
/*
 * txt_seek()
 *  Relative positioning is not allowed.
 */
_ffseek_t
_txt_seek(struct fdinfo *fio, off_t pos, int whence, struct ffsw *stat)
	{
	struct fdinfo *llfio;
	_ffseek_t ret;

	/* Rewind is done with 0,0. seek-to-end with 0,2 */
	if (whence != 0 && whence != 2){
		ERETURN(stat, FDC_ERR_NOSUP, 0);
	}
	if (XRCALL(fio, flushrtn) fio, stat) < 0)
		return(ERR);
/*
 * Set up bookkeeping stuff for txt records.
 */
	fio->scc = SCCFULL;
/*
 * Ask lower layer to do seek and return.
 */
	llfio = fio->fioptr;
	ret = XRCALL(llfio, seekrtn) llfio, pos, whence, stat);
	fio->rwflag = POSITIN;
	fio->ateof = 0;
	fio->ateod = 0;
	fio->ateor = 0;
	fio->last_recbits = fio->recbits;
	fio->recbits = 0;
	return (ret);
	}
