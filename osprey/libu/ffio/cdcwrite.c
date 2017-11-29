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


#pragma ident "@(#) libu/ffio/cdcwrite.c	92.1	06/29/99 13:16:47"

#include <ffio.h>
#include "cdcio.h"

/*
 * Read a CDC format file.
 *
 *	This code handles the supported CDC record formats.
 *	CZ, CW, IW, and CS.
 *	to keep consistency with the other record formats, requests
 *	are still made in terms of bytes.  For instance, to write
 *	2 CDC bytes (12 bits) nbytes=2 and ubc=4.
 *
 * Parameters:
 *      fio     - Pointer to fdinfo block
 *      bufptr  - bit pointer to where data is to go.
 *      nbytes  - Nuber of bytes to be written
 *      stat    - pointer to status return word
 *      fulp    - full or partial write mode flag
 *      ubc     - pointer to unused bit count
 */

static int zero = 0;

static void init_block();

#define PCT	063
#define SPACE	055
#define COLON	000

#define LEV_ZERO	0
#define LEV_EOF		017

_cdc_write(fio, bufptr, nbytes, stat, fulp, ubc)
struct fdinfo *fio;
int nbytes, fulp, *ubc;
bitptr bufptr;
struct ffsw *stat;
	{
	int ret;
	int bits, moved, nbits, left;
	bitptr temptr;
	struct cdc_f *cdc_info;

	nbits = (nbytes << 3) - *ubc;

	cdc_info = (struct cdc_f *)fio->lyr_info;

	if (fio->ateod != 0)
		ERETURN(stat, FDC_ERR_WPEOD, 0);

/*
 *	Check for changes in direction!
 *	Something should be done here to handle this!
 */
	if (fio->rwflag == READIN)
		ERETURN(stat, FDC_ERR_WRARD, 0);

	fio->rwflag = WRITIN;
	moved = 0;
	fio->ateof = 0;

/*
 *	loop putting data in buffer and building segments
 */
	SANITYCHK (fio->maxblksize,LE,0);

	while (nbits > 0)
		{
/*
 *		bits tells when data has been moved.  Set it to zero
 *		unless someone moves some data in the loop
 */
		bits = 0;
/*
 *		initialize a new segment, if needed.
 */
		if (fio->segbits == 0)
			{
			if (init_seg(fio, stat) != 0)
				return(ERR);
			}
		left = fio->maxblksize - fio->_cnt;
/*
 *		If enough room for bits, put them in the buffer
 */
		if (left >= nbits)
			{
			bits = nbits;
			PUTDATA(bufptr, fio, bits);
			SET_BPTR(bufptr, INC_BPTR(bufptr, bits));
			if(fio->rtype == TR_CDC_CZ)
				{
/*
 *				save away the last character written.
 *				to determine if it needs special handling
 *				if followed by EOR
 */
				SET_BPTR(temptr, INC_BPTR(bufptr, -6));
				GET_BITS(cdc_info->lastchar, temptr, 6);
				}
			}
		else
			{
/*
 *			There is not enough room to put all of the data.
 *			Handle this case for each of the formats
 */
			if (left == 0)
				{
				ret = put_segment(fio, stat, PARTIAL,
							CDC_WCW_NORMAL);
				if (ret != 0)
					return(ERR);
				}
			else if (left > 0)
				{
				bits = left;
				PUTDATA(bufptr, fio, bits);
				SET_BPTR(bufptr, INC_BPTR(bufptr, bits));
				}
			else
				{
				ERETURN(stat, FDC_ERR_INTERR, 0);
				}
			}
		nbits -= bits;
		moved += bits;
		}

	if (fulp == FULL)
		{
/*
 *		Watch out for NULL writes!
 */
		if (fio->segbits == 0)
			if (init_seg(fio, stat) != 0)
				return(ERR);
		/* this is FULL */
		ret = put_segment(fio, stat, fulp,CDC_WCW_NORMAL);
		if (ret != 0)
			return(ERR);

		SETSTAT(stat, FFEOR, (moved+7) >> 3);
		fio->last_recbits = fio->recbits;
		fio->recbits = 0;
		}
	else
		{
		SETSTAT(stat, FFCNT, (moved+7) >> 3);
		fio->recbits += moved;
		}
			
	return ((moved+7) >> 3);
	}

/*
 *	init_seg checks if there is room for WCW (?).  If not, flush block
 *	Also, if no bits are yet in block, and there is data 
 *	to write, set aside room for the ICW. 
 */
static int
init_seg(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
	{
	struct cdc_f *cdc_info;

	cdc_info = (struct cdc_f *)fio->lyr_info;
	switch(fio->rtype)
		{
		case TR_CDC_CS:
			break;
		case TR_CDC_CZ:
			break;
		case TR_CDC_IW:
/*
 *			Save space for ICW, then drop down and do same for WCW
 */
			if (fio->_cnt == 0) init_block(fio,stat);
		case TR_CDC_CW:
			cdc_info->wcwaddr = fio->_ptr;
			SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, 60));
			fio->_cnt += 60;
			break;
		}
	
	return(0);
	}

/*
 * Initialize a new block.  Leave room for ICW, if appropriate,
 * and save ICW address
 */
static void
init_block(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
	{
	int icwlen;
	struct cdc_f *cdc_info;

	cdc_info = (struct cdc_f *)fio->lyr_info;
	switch(fio->rtype)
		{
		case TR_CDC_IW:
			icwlen = 60;
			cdc_info->icwaddr = fio->_ptr;
			SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, icwlen));
			fio->_cnt += icwlen;
			cdc_info->needicw = YES;
			break;
		case TR_CDC_CZ:
		case TR_CDC_CS:
		case TR_CDC_CW:
			break;
		}
	}
/*
 * Terminate a data segment by putting in the appropriate segment
 * terminator and adjusting counts
 */
static int
put_segment(fio, stat, fulp, flags)
struct fdinfo *fio;
struct ffsw *stat;
int fulp;
int flags;
	{
	union cdc_icw_u icw;
	union cdc_wcw_u wcw;
	struct cdc_f *cdc_info;
	bitptr pzero;
	int scc, left, ret;

	cdc_info = (struct cdc_f *)fio->lyr_info;
	switch(fio->lastscc)
		{
		case SCCFRST:
		case SCCMIDL:
			scc = SCCMIDL;
			break;
		case SCCLAST:
		case SCCFULL:
			scc = SCCFRST;
			break;
		}
	if (fulp == FULL)
		scc -= 1;	/* TRICK to convert MIDL to LAST or */
				/* FIRST to FULL */
/*
 *	Set pointer to place where wcw will go, build the SDW, and
 *	poke it into place.
 */
	switch(fio->rtype)
		{
		case TR_CDC_CZ:
			if (fulp == FULL)
				{
/*
 *				put in the zero terminator.  We are
 *				guaranteed that there will be enough room for
 *				the remainder of the word in the buffer
 *
 *				Note: if already on word boundary, left=60.
 *				This is exactly right for a terminator.
 */
				left = 60 - (fio->_cnt % 60);
				SANITYCHK(left%6,NE,0);
				SANITYCHK(fio->_cnt,GT,fio->maxblksize);
/*
 *				If the last char in the record is
 *				percent or colon, add a space.
 */
				if (cdc_info->lastchar == (PCT<<58) ||
					cdc_info->lastchar == (COLON<<58))
					{
					/* ADD A SPACE */
					if (fio->_cnt >= fio->maxblksize)
						{
						ret = put_block(fio,stat,
							fio->_cnt,LEV_ZERO);
						if (ret != 0)
							return(ERR);
						}
					PUT_BITS(fio->_ptr, zero, 6);
					SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, 6));
					fio->_cnt += 6;
					left = 60 - (fio->_cnt % 60);
						
					}
				if (fio->_cnt >= fio->maxblksize)
					{
					ret = put_block(fio,stat,
						fio->_cnt,LEV_ZERO);
					if (ret != 0)
						return(ERR);
					}
				SET_BPTR(pzero, CPTR2BP(&zero));
				if (left < 12)
					{
/*
 *					If there 6 or fewer bits left in the
 *					word, then we must build a 66 bit
 *					terminator.
 */
					PUT_BITS(fio->_ptr, zero, left);
					SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, left));
					fio->_cnt += left;
					if (fio->_cnt >= fio->maxblksize)
						{
						ret = put_block(fio,stat,
							fio->_cnt,LEV_ZERO);
						if (ret != 0)
							return(ERR);
						}
					left = 60;
					}
				PUT_BITS(fio->_ptr, zero, left);
				SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, left));
				fio->_cnt += left;
				scc = SCCFULL;
				}
			SANITYCHK(fio->_cnt,GT,fio->maxblksize);
			if (fio->_cnt >= fio->maxblksize)
				{
				ret = put_block(fio,stat,fio->_cnt,LEV_ZERO);
				if (ret != 0)
					return(ERR);
				}
			break;
		case TR_CDC_IW:
			/* Need to fill in ICW here because information in */
			/* ICW depends on data in block. */
			if (cdc_info->needicw == YES)
				{
				icw.wword = 0;
				icw.fld.blk_ord = cdc_info->blknum + 1;
				icw.fld.rec_num = cdc_info->recnum + 1;
				icw.fld.wrd_off = 1; /* WCW always follows */
				/* maintain odd parity */
				icw.fld.parity = _poppar(icw.wword) ^ 1;

				PUT_BITS(cdc_info->icwaddr,icw.wword, 60);
				cdc_info->needicw = NO;
				}
		case TR_CDC_CW:
			if (fulp == FULL)
				{
/*
 *				Round up to word boundary
 */
				left = 60 - (((fio->_cnt-1) % 60) + 1);
				if (left != 0)
					{
					PUT_BITS(fio->_ptr, zero, left);
					SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, left));
					fio->_cnt += left;
					}
				cdc_info->recnum += 1;
				}
			wcw.wword = 0;
			wcw.fld.flags = flags;
			wcw.fld.scc = _scc_tab [SCC_CDC] [scc];
			wcw.fld.wrd_cnt = (fio->segbits + 59)/60;
			wcw.fld.prv_siz = cdc_info->lastwc;
			cdc_info->lastwc = wcw.fld.wrd_cnt + 1;
			wcw.fld.ubc = wcw.fld.wrd_cnt * 60 - fio->segbits;
/*
 *			Do this last!  Maintain odd parity.
 */
			wcw.fld.parity = _poppar(wcw.wword) ^ 1;
/*
 *			Now put the WCW in the buffer.
 */
			PUT_BITS(cdc_info->wcwaddr, wcw.wword, 60);
			SANITYCHK (fio->_cnt,GT,fio->maxblksize);
			if (fio->_cnt >= fio->maxblksize)
				{
				ret = put_block(fio, stat, fio->_cnt, LEV_ZERO);
				if (ret != 0) return(ERR);
				}
			break;
		case TR_CDC_CS:
			if (fulp == FULL)
				{
/*
 *				Round up to word boundary
 */
				left = 60 - (((fio->_cnt-1) % 60) + 1);
				if (left != 0)
					{
					PUT_BITS(fio->_ptr, zero, left);
					SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, left));
					fio->_cnt += left;
					}
				scc = SCCFULL;
				}
			SANITYCHK (fio->_cnt,GT,fio->maxblksize);
			if (fio->_cnt >= fio->maxblksize)
				{
				ret = put_block(fio, stat, fio->_cnt, LEV_ZERO);
				if (ret != 0) return(ERR);
				}
/*
 *			If full record and exactly a block, need 'short'
 *			block to indicate EOR. Also if full mode, short
 *			block indicates EOR
 */
			if (fulp == FULL)
				{
				ret = put_block(fio, stat, fio->_cnt, LEV_ZERO);
				if (ret != 0) return(ERR);
				scc = SCCFRST;
				}
			break;
		}

	fio->scc = scc;
	fio->lastscc = fio->scc;

	fio->segbits = 0;
	return(0);
	}

/*
 * Write a block out.  Fill in the Block Terminator and do the I/O.
 */
static
put_block(fio, stat, bits, level)
struct fdinfo *fio;
struct ffsw *stat;
int level;
int bits;
	{
	int ret, btlen, mode, count, extra;
	union cdc_bt_u bt;
	struct cdc_f *cdc_info;

	cdc_info = (struct cdc_f *)fio->lyr_info;
/*
 *	The EOR write routine(s) have the responsibility of ensuring that
 *	every block ends on a 60-bit boundary.  Check it.
 */
	count = bits % 60;
	if (count != 0)
		ERETURN(stat, FDC_ERR_INTERR, 0);

/*
 *	Add 4 bits if the 'byte' count is odd.
 */
	extra = (bits/12 & 1) << 2; /* tricky way to get 0 or 4 */
	switch(fio->subtype)
		{
		case TR_CDC_BT_DISK:
			btlen = 0;
			if (bits < fio->maxblksize)
				mode = FULL;
			else
				mode = PARTIAL;
			break;
		case TR_CDC_BT_I:
			btlen = 48 + extra;
			mode = FULL;
			break;
		case TR_CDC_BT_SI:
			btlen = 0;
			if (bits < 512*60) /* short block? */
				btlen = 48 + extra;
			mode = FULL;
			break;
		}
/*
 *	Set pointer to place where BT will go, build the BT, and
 *	poke it into place.
 */
	if (btlen != 0)
		{
		bt.wword = 0;
		bt.fld.count = bits/12 + 4;
		bt.fld.blknum = cdc_info->blknum;
		bt.fld.zero = 0;
		bt.fld.level = level;
		PUT_BITS(fio->_ptr, bt.wword, btlen);
		bits += btlen;
		}
	cdc_info->blknum += 1;
/*
 *	Write it out. (rounded up to mult of bytes!)
 */
	WRITEBLK(ret, fio, (bits + 7) >> 3, stat, mode, &zero);
	if (ret < 0)
		return(ret);
	return(0);
	}

/*
 * Flush the buffer and clean up
 *	This routine should return 0, or -1 on error.
 */
int
_cdc_flush(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
	{

/*
 *	if reading, or just positioned, nothing to do...
 */
	if ((fio->rwflag & (READIN | POSITIN)) != 0)
		{
		fio->_ptr = fio->_base;
		fio->_cnt = 0;
		fio->segbits = 0;
		fio->scc = SCCFULL;
		fio->lastscc = SCCFULL;
		return(0);
		}
	fio->rwflag = WRITIN;
/*
 *	A flush on some formats makes no sense.  Return silently
 */
	if (fio->rtype == TR_CDC_IW || fio->rtype == TR_CDC_CW)
		return(0);

/*
 *	In write mode.  Terminate any uncompleted record.
 */
	if (fio->segbits != 0)
		if (put_segment(fio, stat, FULL, CDC_WCW_NORMAL))
			return(ERR);
	if (fio->_cnt != 0)
		if (put_block(fio, stat, fio->_cnt, 0) != LEV_ZERO)
			return(ERR);
	return(0);
	}

/*
 * Write an EOF.
 *	Place an EOF mark in the current layer, if defined.  If not
 *	defined, this routine truncates the dataset at the current point
 *	and sets the ateof flag.
 */
int
_cdc_weof(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
	{
	int ret;

	/* !! Write after Read!! */
	if (fio->rwflag == READIN)
		ERETURN(stat, FDC_ERR_WRARD,0);

	fio->rwflag = WRITIN;
/*
 *	Terminate any uncompleted segments
 */
	if (fio->segbits != 0)
		{
		ret = put_segment(fio, stat, FULL, CDC_WCW_NORMAL);
		if (ret != 0) return(ERR);
		}
/*
 *	Build an EOF or its equivalent for the various formats
 */
	switch(fio->rtype)
		{
		case TR_CDC_IW:
		case TR_CDC_CW:
/*
 *			For DISK files, the EOP is in the last control word.
 *			for I and SI tapes, the EOF is in the block terminators.
 */
			if (fio->subtype == TR_CDC_BT_DISK)
				ret = put_funny_deleted_record
					(fio, stat, CDC_WCW_DELETED);
			else
				ret = put_funny_deleted_record
					(fio, stat, CDC_WCW_EOP);
/*
 *			First flush the current block.  If it is MT, then
 *			a zero length block is generated to mark the EOR
 */
			if (put_block(fio, stat, fio->_cnt, LEV_ZERO) != 0)
				return(ERR);
			break;
		case TR_CDC_CS:
/*
 *			Put out a block terminator with level 017.
 */
			if (put_block(fio, stat, 0, LEV_EOF) != 0)
				return(ERR);
		case TR_CDC_CZ:
/* Should EOF be passed thru for CZ recs? */
			break;
		}
	fio->ateof = 1;
	fio->ateod = 0;
	fio->last_recbits = fio->recbits;
	fio->recbits = 0;
	return(0);
	}

/*
 * Write an EOD.
 *	Truncate the file.  This is the 'real' end of the data.
 */
int
_cdc_weod(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
	{
	int ret;
	struct fdinfo *llfio;

	/* !! Write after Read!! */
	if (fio->rwflag == READIN)
		ERETURN(stat, FDC_ERR_WRARD,0);

	fio->rwflag = WRITIN;
	if (fio->ateod != 0)
		{
		return(0); /* already at EOD. all done. */
		}
/*
 *	Terminate any uncompleted segments
 */
	if (fio->segbits != 0)
		{
		ret = put_segment(fio, stat, FULL,CDC_WCW_NORMAL);
		if (ret != 0) return(ERR);
		}
/*
 *	If the eof was not the last thing written, we must put out the funny
 *	deleted record thing.
 */
	if (fio->ateof == 0)
		{
/*
 *		The name says it all
 */
		ret = put_funny_deleted_record(fio,stat, CDC_WCW_DELETED);
		}

/*
 *	Note: Do NOT check for fio->_cnt being nonzero
 *	For CS, the record has already been terminated by a short block.
 *	For the others, the current block must be written.  If a full block
 *	has just gone out, then a zero length block is required.
 */
	if (fio->rtype != TR_CDC_CS)
		{
		ret = put_block(fio, stat, fio->_cnt, LEV_ZERO);
		if (ret != 0) return(ERR);
		}

	llfio = fio->fioptr;
	ret = XRCALL(llfio, weodrtn) llfio, stat);
	if (ret < 0) return(ret);

	fio->ateof = 0;
	fio->ateod = 1;
	fio->last_recbits = fio->recbits;
	fio->recbits = 0;
	return(0);
	}
/*
 *	Put out the 'dummy' 1 word deleted record.
 *	This routine MUST be called at a 60-bit wird boundary
 *	(after a call to put_segment)
 */
static int
put_funny_deleted_record(fio,stat,flags)
struct fdinfo *fio;
struct ffsw *stat;
int flags;
	{
	int left, ret;
	bitptr pword;

	SET_BPTR(pword, WPTR2BP(&zero));
	switch (fio->rtype)
		{

		case TR_CDC_CS:
		case TR_CDC_CZ:
			break;		/* nothing need be done */
		case TR_CDC_IW:
/*
 *			EOF is represented in the underlying blocking.
 *			On tapes, the CS 'records' that contain the data
 *			will have a level 17 terminator on the last block.
 *			A one word deleted record is put at the EOP
 *			to ensure that the last block is not 'too short'
 *			(??? I think ??? )
 *			
 *			Apparently the EOP wcw flag is only put out
 *			on disk.
 *
 *			put_segment will have written the previous
 *			block if it was full.
 */
			ret = init_seg(fio, stat);
			if (ret < 0) return(ERR);
			left = fio->maxblksize - fio->_cnt;
			SET_BPTR(pword, WPTR2BP(&zero));
			if (left < 2*60)
				{
/*
 *				Put in an extra deleted record to push
 *				the 'real'  'extra' deleted record to the
 *				beginning of the next block.
 */
				if (left > 0) /* if one word available */
					PUTDATA(pword, fio, 60);
				ret = put_segment(fio, stat, FULL,
							CDC_WCW_DELETED);
				if (ret < 0) return(ERR);
/*
 *				Init a new segment. Now at start of new block.
 */
				ret = init_seg(fio, stat);
				}
			PUTDATA(pword, fio, 60);
			ret = put_segment(fio,stat,FULL,
						CDC_WCW_DELETED);
			if (ret < 0) return(ERR);
		case TR_CDC_CW:
/*
 *			The following two calls should result in a
 *			single terminating control word.
 */
			ret = init_seg(fio, stat);
			if (ret < 0) return(ERR);
			ret = put_segment(fio,stat,FULL, flags);
			if (ret < 0) return(ERR);
		}
	return(0);
	}
