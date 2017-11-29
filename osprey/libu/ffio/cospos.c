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


#pragma ident "@(#) libu/ffio/cospos.c	92.2	10/07/99 22:14:49"
#include <ffio.h>
#include "cosio.h"

/*
 *	COS positioning requests
 */

_cos_pos(fio, cmd, arg, len, stat)
struct fdinfo *fio;
int cmd;
long *arg;
int len;
struct ffsw *stat;
	{
	struct fdinfo *llfio;
	int ret;
	long logpos,x;
	struct cos_f *cos_info;
	int bytpos;
	long bytes;

	cos_info = (struct cos_f *)fio->lyr_info;
	llfio = fio->fioptr;
	switch(cmd)
		{
/* For now, _cos_pos is not supported on MIPS. We need to work out */
/* what type "arg" should be. */
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
		case FP_GETPOS:
			if (cos_info->ffci_flags & FFC_SEEKA)
				{
				ret = _cos_seek(fio, 0, 1, stat);
				if (ret == -1){
					return(ERR);
				}
				*arg = ret >>3;
				return(0);
				}
			else
				{
				ASWAITUP(fio, stat, &cos_info->bstat, cos_info,
				  ret);


				/* Calculate the user's logical position */
				logpos = _calc_logpos(cos_info, fio);

				/* calculate the difference between the */
				/* physical position and the user's logical */
				/* position */
				x = logpos - cos_info->cos_diskpos;
				/* Get the position */
				if (XRCALL(llfio, posrtn) llfio, cmd,
				   (arg+2), len-2, stat) < 0)
					return(ERR);

				*arg = logpos>>3;
				/* save the offset from the "GETPOS" position */
				/* and the user's logical position */
				*(arg+1) = x;
				}
			break;

		case FP_BSEEK:
			if (cos_info->ffci_flags & FFC_SEEKA)
				{
                                if ((arg[0] & 7) != 0)
                                        ERETURN(stat, FDC_ERR_BADSK, 0);
				if (arg[0] < 0)
					{
					bytes = (-arg[0])>>3;
					bytes = -bytes;
					}
				else
					{
					bytes = arg[0]>>3;
					}
                                ret = _cos_seek(fio, bytes, arg[1], stat);
				return(_dshiftl(ret, ret, 3));
				}
			ERETURN(stat, FDC_ERR_NOSUP, 0);

		case FP_SETPOS:
			if (cos_info->ffci_flags & FFC_SEEKA)
				{
				return ( _cos_seek(fio, *arg<<3, 0, stat));
				}
			else
				{
				if ((*arg<<3 == 8) || (*arg == 0))
					return ( _cos_seek(fio, *arg<<3, 0, stat));

				bytpos  = *arg<<3;
				return(_cos_setpos(fio, cos_info, len, bytpos,
				  (arg + 1), stat));
				}
#endif
		default:
			ERETURN(stat, FDC_ERR_NOSUP, 0);
		}
	}

/*
 * Return the byte offset of the user's current position from
 * the beginning of the file.
 */
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
static int
_calc_logpos(
struct cos_f *cos_info,
struct fdinfo *fio)
{
	off_t locpos = 0;

	if (cos_info->cos_cbn == -1)
		return(0);
	/* get the "real" block number of the base of the buffer */
	locpos = cos_info->cos_cbn -
	   ((cos_info->cos_cbn & BNMASK) - GETBN(BPTR2WP(fio->_base)));

	/* Translate that to a byte offset of the beginning of  */
	/* the buffer. */
	locpos = locpos * BLKSZ >> 3;

	/* Now find the offset of our current position from the*/
	/* beginning of the buffer. */
	locpos = locpos + (SUBT_BPTR(fio->_ptr, fio->_base)>>3);

	return(locpos);
}
#endif

/*
 *
 */
int
_cos_setpos(
struct fdinfo *fio,
struct cos_f *cos_info,
int len,	/* number of words of positioning information */
off_t bytpos,	/* byte position */
long *pos,	/* position information*/
struct ffsw *stat)
	{
	int64 actbn, oactbn;
	off_t offset;
	off_t blkpos;	/* requested position in blocks */
	off_t bufbitpos;	/* buffer base position in bits */
	off_t bitpos;	/* requested position in bits */
	int ubc = 0;
	_cw_type *cwptr;
	_cw_type *targcw;
	int mode;
	int ret;
	int errcd;
	bitptr blklmt;
	ssize_t rret;
	int x;

/*
 *	Need to position to the RCW immediately preceeding the record
 *	to which we are going.  If positioning to the spot right after
 *	a BCW, back off one block.
 */
	blkpos = (bytpos - 8) >> BLKSZ2BY; /* bytes to blocks */
	if ((blkpos << BLKSZ2BY) == bytpos)
		{
		blkpos -= 1;
		bitpos = (bytpos << 3) - 2*CWSIZE;
		}
	else
		{
		bitpos = (bytpos << 3) - CWSIZE;
		}
/*
 *	calculate the actual block number of the base of the buffer
 *	and compare it to the BN in the BCW at the base of the buffer.
 */
	actbn = cos_info->cos_bufpos >> BLKSZ2BY;
/*
 *	Now the 'other' buffer
 */
	oactbn = cos_info->opos >> BLKSZ2BY;
/*
 *	See if position is already in buffer, If not, flush it, seek
 *	and reload the buffer with the right stuff.
 */
	ASWAITUP(fio, stat, &cos_info->bstat, cos_info, ret);
	if (actbn <= blkpos &&
			(actbn + (cos_info->cos_size >> BLKSZ2)) > blkpos)
		{
/*
 *		Desired position is in the buffer.  Set up pointers./
 */
		bufbitpos = actbn << BLKSZ2;
		offset = bitpos - bufbitpos;
		blklmt = INC_BPTR(fio->_base, (offset & ~(BLKSZM)));
		}
	else if ((cos_info->opos >= 0) && oactbn <= blkpos &&
			(oactbn + (cos_info->cos_size >> BLKSZ2)) > blkpos)
		{
		ASWTCH(fio, cos_info, fio->_base, cos_info->cos_bufpos,
			cos_info->cos_size);
/*
 *		Desired position is in the buffer.  Set up pointers./
 */
		bufbitpos = oactbn << BLKSZ2;
		offset = bitpos - bufbitpos;
		blklmt = INC_BPTR(fio->_base, (offset & ~(BLKSZM)));
		}
	else	/* data is in neither buffer */
		{
		if (cos_info->cos_flag & COS_IODIRTY) /* flush dirty buffer */
			{
			if (_cos_iflush(fio, stat) < 0)
				return(ERR);
			}
		if (cos_info->ffci_flags & FFC_SEEKA)
			{
			if( XRCALL(fio->fioptr, seekrtn) fio->fioptr,
						blkpos << BLKSZ2BY, 0, stat) < 0)
				return(ERR);
			}
		else
			{
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
			/* Not supported yet. */
			ERETURN(stat, FDC_ERR_NOSUP, 0);
#else
			if (XRCALL(fio->fioptr, posrtn) fio->fioptr, FP_SETPOS,
			   (pos+1), len-2, stat) < 0)
				return(ERR);
			/* We are not quite where we want to be. */
			/* The record to which we are going is *(pos) */
			/* bytes away from the position we just SETPOS'd to */
			/* We need to postion to the RCW immediately */
			/* preceding the record to which we are going. */
			/* If positioning to the spot right after a BCW, */
			/* back off one block. */
			x = (blkpos <<BLKSZ2BY) - bytpos;
			x = x + *(pos);

			ret = XRCALL(fio->fioptr, posrtn)
				fio->fioptr,
				FP_RSEEK,
				&(x),
				1,   
				stat
				);  
			if (ret < 0)
				return(ERR);
#endif
			}
		rret = XRCALL(fio->fioptr, readrtn)
				fio->fioptr,
				fio->_base,
				fio->_ffbufsiz >> 3,
				stat,
				PARTIAL,
				&ubc);
/*
 *		In this context, we *must* get data, zero is
 *		an error.
 */
		if (rret == 0) ERETURN(stat, FDC_ERR_UXEND, 0);
		if (rret < 0) return(ERR); /* stat already set */
		if (ubc != 0) ERETURN(stat, FDC_ERR_BADCOS, 0);

		cos_info->cos_bufpos = blkpos << BLKSZ2BY;
		cos_info->cos_diskpos = (blkpos << BLKSZ2BY) + rret;
		bufbitpos = blkpos << BLKSZ2;
		offset = bitpos - bufbitpos;
		cos_info->cos_size = rret << 3;

		blklmt = fio->_base;
		}
	fio->_ptr  = blklmt;
	cos_info->cos_cnt = cos_info->cos_size - SUBT_BPTR(blklmt, fio->_base);
	cwptr = (_cw_type *)BPTR2WP(blklmt);
	cos_info->cos_cbn = blkpos;
	SKIPBCW(cwptr, fio->_ptr, fio->_base, blklmt,
			cos_info->cos_cnt, cos_info->cos_cbn, stat);
/*
 *	Skip RCWs until we find the one we are seeking.  (a little pun)
 */
	targcw = (_cw_type *)BPTR2WP(INC_BPTR(fio->_base, offset));
	/* If the skip-remainder-of-segment bit is set, then */
	/* there are no more control words in this block, and we are */
	/* at the correct one. */
	while (cwptr != targcw)
		{
		if (cwptr >= (_cw_type *)BPTR2WP(blklmt))
			{
			errcd = FDC_ERR_BADCOS;
			if (cwptr == (_cw_type *)BPTR2WP(blklmt)) errcd = FDC_ERR_NOTREC;
			ERETURN(stat, errcd, 0);
			}
		cwptr += GETFWI(cwptr);
		}
/*
 *	Desired position is in the buffer, and we are pointing at the RCW
 *	immediately preceeding the record we want.  Set up pointers
 *	appropriately and skip the RCW.
 */

	cos_info->cos_pri = blkpos - GETPRI(cwptr);
	cos_info->cos_pfi = blkpos - GETPFI(cwptr);
	cos_info->cos_cbn = blkpos;
	mode = GETM(cwptr);
	cos_info->cos_cwptr = cwptr;
	SKIPRCW(cos_info->cos_cwptr, fio->_ptr, fio->_base, cos_info->cos_cnt, blklmt,
		cos_info->cos_size, stat);
	cos_info->cos_blklmt = blklmt;

	cos_info->cos_flag &= ~(COS_IOBOD | COS_IOEOF | COS_IOEOD | COS_IOWRITE);

	fio->atbod = NO;
	if (mode == CWEOR)
		{
		fio->ateor = YES;
		fio->ateof = NO;
		cos_info->cos_flag |= COS_IOEOR;
		}
	else if (mode == CWEOF)
		{
		fio->ateor = NO;
		fio->ateof = YES;
		cos_info->cos_flag |= COS_IOEOF;
		}
	else if (mode == CWEOD)
		{
		fio->ateor = NO;
		fio->ateof = YES;
		cos_info->cos_flag |= COS_IOEOF;
		}
	else
		ERETURN(stat, FDC_ERR_BADRCW, 0);
	fio->ateod = NO;

	return(0);
	}
