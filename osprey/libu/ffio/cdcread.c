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


#pragma ident "@(#) libu/ffio/cdcread.c	92.1	06/29/99 13:16:47"

#include <ffio.h>
#include "cdcio.h"

static int scc_convert[4] = {SCCFULL, SCCFRST, SCCMIDL, SCCLAST};

/*
 * Read a CDC format file.
 *
 *	CDC files are always in multiples of 60-bit words.  Requests
 *	are still made in terms of bytes.  For instance, to read
 *	2 CDC bytes (12 bits) nbytes=2 and ubc=4.  Because the underlying
 *	I/O does not generally handle non-byte requests (i.e. 4 bits)
 *	requests are rounded up to multiples of bytes when using the
 *	lower levels.
 *
 * For CZ records
 *	Zero byte terminators are not returned as part of the data.
 *
 *	Note: most notation that refers to words is to 60-bit words.
 *
 * Parameters:
 *      fio     - Pointer to fdinfo block
 *      bufptr  - bit pointer to where data is to go.
 *      nbytes  - Number of bytes to be read
 *      stat    - pointer to status return word
 *      fulp    - full or partial write mode flag
 *      ubc     - pointer to unused bit count
 */

_cdc_read(fio, bufptr, nbytes, stat, fulp, ubc)
struct fdinfo *fio;
int nbytes, fulp, *ubc;
bitptr bufptr;
struct ffsw *stat;
	{
	int bits, moved, nbits, ret, eorstat;

	nbits = (nbytes << 3) - *ubc;

	moved = 0;
/*
 *	If NOT writing or just positioned, then error.
 */
	if (fio->rwflag == WRITIN)
		{
		/* read after write error, not yet supported */
		ERETURN(stat, FDC_ERR_RAWR, 0);
		}
	fio->rwflag = READIN;
/*
 *	If previous segment was MT, get new segment
 *	For example, a Z record segment is the record, or to the
 *	end of block, whichever
 *	is shorter.  (The case where the last 6-bit byte in the block is
 *	zero is handled specially)
 */
	if (fio->segbits == 0)
		{
		ret = get_segment(fio, stat);
		if (ret > 0)
			return(0);	/* probable EOD */
		if (ret < 0)
			return(ret);	/* probable ERR */
		}
/*
 *	loop getting segments and moving data
 */
	eorstat = FFCNT;
	while (nbits > 0)
		{
		bits = nbits;
		if (fio->segbits < nbits)
			bits = fio->segbits;
/*
 *		If segbits is ever negative, we are in deep weeds.
 */
		SANITYCHK (fio->segbits,LT,0);
/*
 *		Get the data from the buffer
 */
		if (bits > 0)
			{
			GETDATA(bufptr, fio, bits);
			SET_BPTR(bufptr, INC_BPTR(bufptr, bits));
			nbits -= bits;
			moved += bits;
			}
/*
 *		Got some bits.  Depending on format, loop around again,
 *		or set status and prepare to return
 */
		if (fio->segbits == 0)
			{
			/* check if EOR found */
			if (fio->scc == SCCLAST || fio->scc == SCCFULL)
				{
				eorstat = FFEOR;
				nbits = 0; /* exit loop */
				}
			else
				{
				ret = get_segment(fio, stat);
				if (ret > 0)
					return(0); /* EOD? */
				if (ret < 0)
					return(ERR);
				}
			}
		} /* end while */
/*
 *	Need to check for zero length segment as first thing in the buffer
 */
	if (fio->segbits == 0 && (fio->scc == SCCLAST || fio->segbits == SCCFULL))
		eorstat = FFEOR;

	fio->recbits += moved;
/*
 *	At the end of every record we must skip the Z terminator
 *	control words, or whatever.
 *	So, call skip2eor if in FULL mode, or if PARTIAL and hit exactly EOR.
 */
	if (fulp == FULL || eorstat == FFEOR)
		{
		ret = skip2eor(fio, stat); /* eat ctrl word */
		if (ret > 0)
			return(0);	/* probable EOD */
		if (ret < 0)
			return(ERR);
		fio->last_recbits = fio->recbits;
		fio->recbits = 0;
		}

	ret = (moved+7)>>3;		/* # bytes */
	SETSTAT(stat, eorstat, ret);
	*ubc = ret*8 - moved;		/* calc ubc */
	return (ret);
	}

/*
 * Fetch a segment from the data stream.
 *	returns:
 *		>0 informative, usually EOD
 *		=0 got it!
 *		<0 some error.
 */
static int
get_segment(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
	{
	bitptr tptr;
	int i, seglen, scc, ztlen;
	int tcnt, justgotblock;
	unsigned long tword;
	union cdc_bt_u bterm;
	union cdc_wcw_u wcw;
	union cdc_icw_u icw;
	struct cdc_f *cdc_info;
	int save_segbits;

	cdc_info = (struct cdc_f *) fio->lyr_info;
/*
 *	If _cnt is zero, then a new block is needed for all of the
 *	record and block types.  The case where a CZ record must
 *	be determined thru 'lookahead' is handled elsewhere.
 */
again:
	justgotblock = NO;
	if (fio->_cnt == 0)
		{
/*
 *		The get_block() routine returns 'clean' data, with any
 *		block terminator stripped off
 *		Block terminator when stripped off is stored in the
 *		fdinfo structure.
 */
		fio->_ptr = fio->_base;
		i = get_block(fio,stat);
		if (i != 0) return(i); /* stat already set.. */
		justgotblock = YES;
		}
	switch(fio->rtype)
		{
		case TR_CDC_CZ:
/*
 *			Handle special case.  Where the last byte in a block
 *			is zero, the next word must be examined.  Do this by
 *			putting the last word in the front of the block, and
 *			then reading the next block in after it.
 */
			if (cdc_info->czflag == _TRUE)
				{
				GET_BITS(tword, fio->_ptr, 60)
				fio->_ptr = fio->_base;
/*
 *				Put the last word at the start of the buffer
 *
 *				align end of word to be on byte boundary.
 */
				SET_BPTR(tptr, INC_BPTR(fio->_base, 4));

				PUT_BITS(tptr, tword, 60);
				/* leave room */
				SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, 64));
				i = get_block(fio,stat);
				if (i < 0)
					return(i); /* stat is already set.. */
				if (i > 0)
					{
					ERETURN(stat, FDC_ERR_UXEND, 0);
					}
				fio->_ptr = tptr;
				fio->_cnt += 60;	/* add word back in */
				cdc_info->czflag = _FALSE;
				}
			seglen = 0;
			scc = -1;
			tptr = fio->_ptr;
			tcnt = fio->_cnt;
			ztlen = 0;
			while (scc == -1 && tcnt > 0)
				{
				GET_BITS(tword, tptr, 60);
				seglen += 60;
				tcnt -= 60;
				SET_BPTR(tptr, INC_BPTR(tptr, 60));
				/* if lowest 12 bits are zero */
				if ((tword << 48) == 0)
					{
					ztlen += trailz(tword);
					cdc_info->ztlen = ztlen;
					scc=SCCLAST;
					cdc_info->czflag = _FALSE;
					}
/*
 *				If lower 6 are zero, then we must look ahead
 *				to the next word to determine whether it is
 *				*really* an EOR
 */
				else if ((tword << 54) == 0)
					{
					if (tcnt < 60)
						{
/*
 *						Ignore current word until
 *						it can be seen with the words
 *						in the next block.  Set flag
 *						to handle this case.
 */
						seglen -= 60;
						ztlen = 0;
						cdc_info->czflag = _TRUE;
						scc = SCCMIDL; /* exit loop */
						}
					else
						{
						ztlen = 6;
						cdc_info->czflag = _FALSE;
						}
					}
				else
					{
					ztlen = 0;
					cdc_info->czflag = _FALSE;
					}
				}
			fio->segbits = seglen - ztlen;
			break;
		case TR_CDC_CS:
			fio->segbits = fio->_cnt;
/*
 *			Short block indicates EOR.  Short is anything less
 *			than 5120*6 bits
 */
			bterm = cdc_info->bt;
/*
 *			detect EOF blocks
 */
			if (fio->_cnt == 0 && bterm.fld.level == 017)
				return(seteof(fio, stat));

			if (cdc_info->eoslr == YES || fio->_cnt < 5120*6)
				scc = SCCLAST;
			else
				scc = SCCMIDL;
			break;
			
		case TR_CDC_IW:
		case TR_CDC_CW:
			if (justgotblock == YES)
				{
				if (fio->_cnt == 0) /* zero length block */
					return(seteof(fio, stat));
				if (fio->rtype == TR_CDC_IW)
					{
					GET_BITS(icw.wword, fio->_ptr, 60);
					SKIP_BITS(fio, 60);
					if (icw.fld.wrd_off != 1)
						{
						ERETURN(stat, FDC_ERR_NOSUP, 0);
						}
					}
				}
			tcnt = fio->_cnt%60;
/*
 *			Round up to next word
 */
			if (tcnt != 0) { SKIP_BITS(fio, tcnt); }
			GET_BITS(wcw.wword, fio->_ptr, 60);
			SKIP_BITS(fio, 60);
			fio->segbits = wcw.fld.wrd_cnt * 60 - wcw.fld.ubc;
			if ((fio->_cnt - fio->segbits) < 0)
				{
/*
 *	To handle the case where a word record is split over two blocks
 *	with no continuation, check for _cnt indicating the end of block,
 *	and segbits indicating that there is an even multiple of 60 bits
 *	left.  If this is true, save segbits, get the next block, and 
 *	restore segbits.
 */
				if (fio->_cnt == 0 && (fio->segbits % 60) == 0)
					{
					save_segbits = fio->segbits;
					fio->_ptr = fio->_base;
					i = get_block (fio, stat);
					if (i != 0) return (i);
					SKIP_BITS (fio, 60);
					fio->segbits = save_segbits;
					}
				else
					ERETURN(stat, FDC_ERR_CDCWCW, 0);
				}
			switch(wcw.fld.flags)
				{
				case CDC_WCW_DELETED:
					SKIP_BITS(fio, fio->segbits);
					/* segbits is zero now */
					/* skip this segment completely. */
					goto again;
				case CDC_WCW_EOP:
				case CDC_WCW_EOS:
/*
 *					After EOP, there MUST be a block
 *					terminator (zero length block)
 *					if the EOP control word is last
 *					in the block.
 *
 *					Flush it.
 */
					if (fio->_cnt == 0)
						{
						if (get_block(fio, stat) != 0)
							return(ERR);
						if (fio->_cnt != 0)
							return(ERR);
						}
					return(seteof(fio,stat));
				case CDC_WCW_NORMAL:
					scc = scc_convert[wcw.fld.scc];
				}
			break;
		}
	fio->lastscc = fio->scc;
	fio->scc = scc;
	if (fio->rtype == TR_CDC_CZ || fio->rtype == TR_CDC_CS)
		seglen = 0;	/* dummy statement.  Don't check CZ or CS */
	else if (fio->scc == SCCLAST || fio->scc == SCCMIDL)
		{
		if (fio->lastscc == SCCLAST || fio->lastscc == SCCFULL)
			{
			ERETURN(stat, FDC_ERR_SCC, 0);	/* BAD SCC */
			}
		}
	else if (fio->scc == SCCFRST || fio->scc == SCCFULL)
		{
		if (fio->lastscc == SCCFRST || fio->lastscc == SCCMIDL)
			{
			ERETURN(stat, FDC_ERR_SCC, 0);	/* BAD SCC */
			}
		}
	else
		{
		ERETURN(stat, FDC_ERR_SCC, 0);	/* BAD SCC */
		}
	return(0);
	}

/*
 * Calculate the trailing zero count in 6 bit bytes, in a word
 * Assume a left justified input word.
 */
static int
trailz(lword)
unsigned long lword;
	{
	long eor, mask, tz;
	long tmask = 01010101010101010101000;

	lword &= 0xfffffffffffffff0;
	mask = lword - 1;	/* make mask of ones where zeroes were */
	eor = lword ^ mask;	/* XOR with original */
	mask = mask & eor;	/* trim off extra flipped bit */
	mask = ~mask & tmask;	/* find next 6-bit boundary */
	lword = (unsigned long)mask >> 5;
	mask = lword - 1;	/* do it again */
	eor = lword ^ mask;
	mask = mask & eor;
	tz = 60 - _leadz(mask); /* trailing zero count */
	
	return(tz);
	}

/*
 *	Common code to handle EOF return
 */
static
seteof(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
	{
	fio->segbits = 0;	/* no data here.. */
	fio->last_recbits = fio->recbits;
	fio->recbits = 0;
	fio->ateof = 1;
	fio->ateod = 0;
	SETSTAT(stat, FFEOF, 0)
	return(FFEOF);
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
	fio->last_recbits = fio->recbits;
	fio->_cnt = 0;
	fio->ateof = 0;
	fio->ateod = 1;
	SETSTAT(stat, FFEOD, 0)
	return(FFEOD);
	}
/*
 * Read a block of 5120*6 bits plus block terminator, if applicable.
 * the _ptr field has already been set because of the lookahead
 * occasionally required with CZ records.
 */
static
get_block(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
	{
	int btlen, zero, ret, mode, left;
	int num2read;
	unsigned long stripbt();
	struct cdc_f *cdc_info;

	if (fio->ateod != 0)
		return(seteod(fio,stat));
	cdc_info = (struct cdc_f *)fio->lyr_info;
/*
 *	get block 'reads' the next block from the file.  For CDC
 *	all blocks are 5120*6 bits plus block terminator (if tape)
 */
	zero = 0;
	switch(fio->subtype)
		{
		case TR_CDC_BT_DISK:
			mode = PARTIAL;
			btlen = 0;	/* no terminator on disk */
			break;
		case TR_CDC_BT_SI:
			mode = FULL;
			btlen = 48;	/* maybe there */
			btlen += 64;	/* to detect lack of lower level */
					/* read whole word to avoid COS bug */
			break;
		case TR_CDC_BT_I:
			mode = FULL;
			btlen = 48;	/* Will be there */
			btlen += 64;	/* to detect lack of lower level */
					/* read whole word to avoid COS bug */
			break;
		default:
			ERETURN(stat, FDC_ERR_INTERR, 0);
		} /* end case */
/*
 *	For disk, must rely on count, and full mode.  For tape fmts
 *	if there is no underlying blocking, or device that is
 *	'record mode', then this is detected at open time.
 *	This is an error.
 *
 *	WARNING:  the read is rounded up to a byte boundary
 *		this *should* never cause a problem
 */
	num2read = ((fio->maxblksize + btlen + 7) >> 3);
	READBLK(ret, fio, num2read, stat, mode, &zero);
	fio->segbits = 0;
	if (ret < 0)
		return(ERR);
	if (fio->_cnt == 0)
		return(seteod(fio, stat));
/*
 *	Decide what to do about the block terminator.
 */
	switch(fio->subtype)
		{
		case TR_CDC_BT_DISK:
			cdc_info->bt.wword = 0;	/* zero blk term? */
			cdc_info->btflag = NO;
/*
 *			Blocks are always multiples of words.  Strip off
 *			extra 4-bit chunks if present
 *
 *		!!----- This should not be necessary when underlying blocking
 *		!!----- can handle bit length records.
 */
			left = fio->_cnt%60;
			if (left != 0)
				{
				SANITYCHK(left,NE,4);
				fio->_cnt -= left;
				}
/*
 *			Determine if the last read found the end of the System
 *			Logical Eor.
 */
			cdc_info->eoslr = NO;
			if (stat->sw_stat == FFEOR)
				cdc_info->eoslr = YES;
			break;
/*
 *		There is always an integral number of
 *		8-bit bytes in each block.
 */
		case TR_CDC_BT_SI:
			if (stat->sw_stat != FFEOR)
				{
				ERETURN(stat, FDC_ERR_NOBDRY, zero);
				}
			cdc_info->btflag = NO;
			if (fio->_cnt != fio->maxblksize)
				{
				if (stripbt(fio, stat) != 0)
					return(ERR);
				cdc_info->btflag = YES;
				}
			break;
		case TR_CDC_BT_I:
			if (stat->sw_stat != FFEOR)
				{
				ERETURN(stat, FDC_ERR_NOBDRY, zero);
				}
			if (stripbt(fio, stat) != 0)
				return(ERR);
			cdc_info->btflag = YES;
			break;
		} /* end case */
	return(0);
	}

/*
 * stripbt()
 *	Strip the block terminator from a block.  Return the block
 *	terminator.  Trim the block in the buffer to its data.
 */
static unsigned long
stripbt(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
	{
	unsigned long tword;
	union cdc_bt_u bt;
	int btbits, blkbits, i;
	bitptr btptr;
	struct cdc_f *cdc_info;

	cdc_info = (struct cdc_f *) fio->lyr_info;

	btbits = ((fio->_cnt - 1) % 60) + 1;
	if (btbits != 52 && btbits != 48 && btbits != 60)
		{
		ERETURN(stat, FDC_ERR_CDCBT, 0);
		}
	blkbits = fio->_cnt - btbits;
	SET_BPTR(btptr, INC_BPTR(fio->_ptr, blkbits));
	GET_BITS(tword, btptr, btbits);
	cdc_info->bt.wword = tword;
	bt.wword = tword;
	i = bt.fld.count;
		/*			It may be that we must */
		/* i = i + (i & 1);	round up to even value */
		/*			if 6000 or 7000 machines, only */
		/*			but this has not been demonstrated. */
	if (((blkbits/12) + 4) != i)
		{
		ERETURN(stat, FDC_ERR_CDCBT, 0);
		}
	fio->_cnt = blkbits;
	return(0);
	}
/*
 * Skip2eor skips ahead to the end of the record, reading up blocks and
 * records as needed until the SCC says FFEOR, or hit EOD.
 */
static
skip2eor(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
	{
	int ret;
	struct cdc_f *cdc_info;

	cdc_info = (struct cdc_f *) fio->lyr_info;
	if (fio->segbits > 0)
		{
		SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, fio->segbits));
		fio->_cnt -= fio->segbits;
		}
	while(fio->scc != SCCLAST && fio->scc != SCCFULL)
		{
		ret = get_segment(fio, stat);
		if (ret != 0)
			return(ret);	/* probable EOD */
		SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, fio->segbits));
		fio->_cnt -= fio->segbits;
		}
/*
 *	For Z records skip the zero terminator on the record.
 */
	if (fio->rtype == TR_CDC_CZ)
		{
		SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, cdc_info->ztlen));
		fio->_cnt -= cdc_info->ztlen;
		}
	fio->segbits = 0;
	return(0);
	}

/*
 * _cdc_seek()
 * Allow the user to rewind his foreign file.
 *	Input parameters are the same as the lseek(2) call with the addition
 *	of the stat parameter.
 */
_cdc_seek(fio, pos, whence, stat)
struct fdinfo *fio;
int pos, whence;
struct ffsw *stat;
	{
	struct fdinfo *llfio;
	struct cdc_f *cdc_info;
	int ret;

/*
 * For now, only allow rewind to zero
 */
	if (pos != 0 || whence != 0)
		{
		ERETURN(stat, FDC_ERR_NOSUP, 0);
		}

	if (fio->rwflag == WRITIN)
		{
		if (fio->ateod == 0)
			ret = XRCALL(fio, weodrtn) fio, stat);
		}
	else
		ret = XRCALL(fio, flushrtn) fio, stat);
	if (ret < 0) return(ERR);
/*
 *	Do special CDC class things
 */
	fio->scc = SCCFULL;
	fio->lastscc = SCCFULL;

	llfio = fio->fioptr;
	ret = XRCALL(llfio, seekrtn) llfio, pos, whence, stat);
	fio->rwflag = POSITIN;
	fio->ateof = 0;
	fio->ateod = 0;
	fio->recbits = 0;
	fio->last_recbits = fio->recbits;
	cdc_info = (struct cdc_f *)fio->lyr_info;
	cdc_info->ztlen = 0;
	cdc_info->czflag = _FALSE;
	cdc_info->lastchar = 0;
	cdc_info->needicw = NO;
	cdc_info->lastwc = 0;
	cdc_info->blknum = 0;
	cdc_info->recnum = 0;
	cdc_info->bt.wword = 0;
	cdc_info->btflag = NO;
	cdc_info->eoslr = NO;
	SETSTAT(stat, FFBOD, 0);
	return (ret);
	}
