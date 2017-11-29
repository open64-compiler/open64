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


#pragma ident "@(#) libu/ffio/cosbksp.c	92.1	06/29/99 13:16:47"
 
#include <ffio.h>
#include "cosio.h"

#define RETURN(x)	{retv = x; goto retl;}

#if DEBUG
#define CTRACE(place)							\
	{								\
	if (FDCTRACE(TRACE_COS))					\
		_xrt_putf("\t_cos_bksp: %s: cbn=%x, cw=%x, ptr=%x+%x, base=%x+%x\n", \
			place,						\
			cos_info->cos_cbn, cos_info->cos_cwptr,		\
			BPTR2WP(fio->_ptr), BPBITOFF(fio->_ptr),	\
			BPTR2WP(fio->_base), BPBITOFF(fio->_base));	\
	}
#else
#define CTRACE(place)
#endif


static void cos__lrcw(struct fdinfo *fio, bitptr *ptr, _cw_type **cwptr,
	_cw_type *limcwptr);
static int cos__prcw(struct fdinfo *fio, bitptr *ptr, _cw_type **cwptr, 
	struct ffsw *stat);
static int cos__nextblock(struct fdinfo *fio, struct ffsw *stat);
static int cos__getblk(struct fdinfo *fio, int64 bn, struct ffsw *stat);
static int _cos_bkup(struct fdinfo *fio, int cwmode, struct ffsw *stat);

/*
 *	backspace COS blocked sequential dataset by one record
 *	i.e. position before the proceeding record.
 */
int
_cos_bksp(
struct fdinfo *fio,
struct ffsw *stat)
	{
	return(_cos_bkup(fio, CWEOR, stat));
	}
/*
 * _cos_bkfil()
 *	Backspace by one file.
 */
int
_cos_bkfil(
struct fdinfo *fio,
struct ffsw *stat)
	{
	return( _cos_bkup(fio, CWEOF, stat));
	}
/*
 *	Back up one control word as specified by mode.  This routine
 *	can back up files, or records.
 *
 *	Arguments:
 *
 *		fio	- FFIO file descriptor.
 *		cwmode 	- If CWEOR, back up to the preceding EOF or EOR 
 *			  control word.  If CWEOF, back up to the preceding
 *			  EOF control word.
 *		stat	- Output status.
 *
 *	Return value:
 *
 *		0 on success.   -1 on error.
 */
static int
_cos_bkup(
struct fdinfo *fio,
int cwmode,
struct ffsw *stat)
	{
	int		ubc, dummy;
	_cw_type 	*ncwptr;
	_cw_type	*lcwptr;
	bitptr		nptr, blkbase, blklmt, dumptr;
	int		retv = 0;
	int		mode=0;
	struct cos_f	*cos_info;

	cos_info = (struct cos_f *)fio->lyr_info;

	CTRACE("enter");

	if (cos_info->cos_cbn == -1)	/* if at BOD */
		{
		fio->rwflag = POSITIN;
		fio->last_recbits = fio->recbits;
		fio->recbits = 0;
		fio->ateof = NO;
		fio->ateod = NO;
		SETSTAT(stat, FFBOD, 0);
		CTRACE("exit1");
		return(0);
		}

/*
 *	If just did a write, ensure that the file is terminated with an
 *	EOR/EOF/EOD sequence.
 */
	if (cos_info->cos_flag & COS_IOWRITE)
		{
		if (XRCALL(fio, weodrtn) fio, stat) < 0)
			return(ERR);
		}
/*
 *	If EOR, EOF, or EOD are set, then we are positioned immediately 
 *	following an RCW or EOF.  In this case, just back up one word.
 *
 *	If EOR, EOF, and EOD are not set, then we are positioned partway
 *	through a record.  If so, find the closing RCW.
 */
	if ((cos_info->cos_flag & (COS_IOEOR | COS_IOEOF | COS_IOEOD)) != 0)
		{
		bitptr tptr;
/*
 *		We only need to back up one word.
 */
		SET_BPTR(blkbase, BLKBASE(fio->_ptr, fio->_base));
		lcwptr = (_cw_type *)BPTR2WP(fio->_ptr);
		if (SUBT_BPTR(fio->_ptr, blkbase) == NBTSPW && cos_info->cos_cbn > 0)
			{
			retv = cos__getblk(fio, cos_info->cos_cbn - 1, stat);
			if (retv)
				{
				CTRACE("exit2");
				return(retv);
				}
			SET_BPTR(tptr, BLKLMT(fio->_ptr, fio->_base));
			lcwptr = (_cw_type *)BPTR2WP(tptr);
			}
		}
	else
		{
/*
 *		find closing rcw.  While control word is beyond block limit,
 *		skip blocks.
 */
		SET_BPTR(blklmt, BLKLMT(fio->_ptr, fio->_base));
		while (cos_info->cos_cwptr >= (_cw_type *)BPTR2WP(blklmt))
			{
			retv = cos__nextblock(fio, stat);
			if (retv) return(retv);
			SET_BPTR(blklmt, BLKLMT(fio->_ptr, fio->_base));
			}
		lcwptr = cos_info->cos_cwptr + 1;
		}
/*
 *	Set the current cwptr and ptr to the closing RCW of the record.
 */
	cos__lrcw(fio, &nptr, &ncwptr, lcwptr);
/*
 *	If already at beginning of file... (ncwptr can only be == base if cbn
 *	is zero.)
 */
	if (ncwptr == (_cw_type *)BPTR2WP(fio->_base))
		{
		fio->_ptr = fio->_base;
		cos_info->cos_blklmt = fio->_base;
		cos_info->cos_cwptr = (_cw_type *)BPTR2WP(fio->_base);
		if (cos_info->cos_cbn >= 0)
			{
			SKIPBCW(cos_info->cos_cwptr, fio->_ptr, fio->_base,
				cos_info->cos_blklmt, cos_info->cos_cnt,
				cos_info->cos_cbn, stat);
			}
		mode = COS_IOBOD;
		RETURN(0);
		}

/*
 *	cos__lrcw returns cwptr -> RCW and nptr points to the bit
 *	immediately after the preceding CW.
 *	If the nptr is more than one word past the beginning of the
 *	block, then we know that it is following an RCW and the
 *	entire record is in this block.  Position to the beginning of the
 *	record and return.
 */
	SET_BPTR(blkbase, BLKBASE(fio->_ptr, fio->_base));
	if (SUBT_BPTR(nptr, blkbase) > NBTSPW)
		{
		fio->_ptr = nptr;	/* we're done */
		cos_info->cos_cwptr = ncwptr;
/*
 *		set EOF according to ncwptr
 */
		mode = COS_IOEOR;
		if (GETM(ncwptr) == CWEOF)
			{
			mode = COS_IOEOF;
			cos_info->cos_pfi = cos_info->cos_cbn - GETPFI(ncwptr);
			}
		SET_BPTR(cos_info->cos_blklmt, BLKLMT(fio->_ptr, fio->_base));
		RETURN(0);
		}
/*
 *	If nptr is == blkbase, then the beginning of the record is in
 *	a previous block.  Go to that block and find the beginning of
 *	the record.
 */
	cos_info->cos_cwptr = ncwptr;
	retv = cos__prcw(fio, &nptr, &ncwptr, stat);
	if (retv < 0) return(ERR);
	fio->_ptr = nptr;
	cos_info->cos_cwptr = ncwptr;
	cos_info->cos_flag &= ~(COS_IOEOF|COS_IOEOD|COS_IOBOD|COS_IOWEOF|COS_IOWRITE);
/*
 *	the above line is to allow us to use _cos_read to skip to
 *	the next record. Flags and stuff will be set properly there.
 */ 
	cos_info->cos_cnt = cos_info->cos_size -
				(SUBT_BPTR(fio->_ptr, fio->_base));
	SET_BPTR(cos_info->cos_blklmt, BLKLMT(fio->_ptr, fio->_base));
	if (ncwptr == (_cw_type *)BPTR2WP(fio->_base))
		{
		/* already at beginning of file */
		SKIPBCW(cos_info->cos_cwptr, fio->_ptr, fio->_base,
				cos_info->cos_blklmt, cos_info->cos_cnt,
				cos_info->cos_cbn, stat);
		cos_info->cos_flag |= COS_IOBOD;
		SETSTAT(stat, FFBOD, 0);
		}
	else
		{
/*
 *		FULL read to 'skip' to EOR, even though already there.
 */
		ubc = 0;
		cos_info->cos_pfi = GETPFI(ncwptr);
		SET_BPTR(dumptr, CPTR2BP(&dummy));
		if( XRCALL(fio, readrtn) fio, dumptr, 0, stat, FULL, &ubc) < 0)
			{
			CTRACE("exit3");
			return(ERR);
			}
		}
	fio->rwflag = POSITIN;
	fio->last_recbits = fio->recbits;
	fio->recbits = 0;
	fio->ateof = NO;
	fio->ateod = NO;
	CTRACE("exit4");
	return(retv);

retl:
	cos_info->cos_cnt = cos_info->cos_size -
			SUBT_BPTR(fio->_ptr, fio->_base);
/*
 *	Clear flags
 */
	cos_info->cos_flag &= ~(COS_IOEOR|COS_IOEOF|COS_IOBOD|COS_IOWEOF|COS_IOEOD|COS_IOWRITE);
	cos_info->cos_flag |= mode;	/* set EOF/EOR/BOD flag */
	if (mode == COS_IOEOR)
		{
		SETSTAT(stat, FFEOR, 0);
		}
	else
		SETSTAT(stat, FFEOF, 0);
	fio->rwflag = POSITIN;
	fio->last_recbits = fio->recbits;
	fio->recbits = 0;
	fio->ateof = NO;
	fio->ateod = NO;

	CTRACE("exit");

	return(retv);

}

/*
 *	cos__prcw - find previous rcw;
 *
 *	return args	*cwptr: position of the previous record control word
 *			     (can be position of BCW if at 1st block)
 *			*ptr: position of the beginning of the data of 
 *			      the record in that block
 *
 */

static int
cos__prcw(
struct fdinfo *fio,
bitptr *ptr,
_cw_type **cwptr,
struct ffsw *stat)
	{	
	int64 tbn;
	int retv;
	bitptr tptr;
	_cw_type *lcwptr;		/* last address to look for rcw */
	struct cos_f *cos_info;

	cos_info = (struct cos_f *)fio->lyr_info;
/*
 *	If we are at BOD, and looking for a previous RCW, big trouble.
 */
	if ( (cos_info->cos_cwptr == (_cw_type *)BPTR2WP(fio->_base)) &&
						(cos_info->cos_cbn == 0) )
		ERETURN(stat, FDC_ERR_INTERR, 0);

	tbn = cos_info->cos_cbn - GETPRI(cos_info->cos_cwptr);
	if (tbn<0) 
		ERETURN(stat, FDC_ERR_INTERR, 0);
	lcwptr = cos_info->cos_cwptr;
	if ( GETPRI(cos_info->cos_cwptr) )
		{
		retv = cos__getblk(fio, tbn, stat);
		if (retv) return(retv);
		SET_BPTR(tptr, BLKLMT(fio->_ptr, fio->_base));
		lcwptr = (_cw_type *)BPTR2WP(tptr);
		}
	/* now, we're at the block */
	cos__lrcw(fio, ptr, cwptr, lcwptr);
	SET_BPTR(tptr, BLKBASE(fio->_ptr, fio->_base));
/*
 *	while cwptr points to start of current block, follow the pri to
 *	the appropriate block and find the CW.
 */
	while (*cwptr == (_cw_type *)BPTR2WP(tptr))
		{
		if (tbn == 0) return(0);
		retv = cos__getblk(fio, --tbn, stat);	/* get previous block */
		if (retv) return(retv);
		SET_BPTR(tptr, BLKLMT(fio->_ptr, fio->_base));

		if (cos_info->cos_cwptr == (_cw_type *)BPTR2WP(tptr))
			{
			tbn -= 077777;		/* look 2^15 blocks away */
			if (tbn < 0) ERETURN(stat, FDC_ERR_BADPRI, 0);
			retv = cos__getblk(fio, tbn, stat);
			if (retv) return(retv);
			SET_BPTR(tptr, BLKLMT(fio->_ptr, fio->_base));
			cos__lrcw(fio, ptr, cwptr, (_cw_type *)BPTR2WP(tptr));
			/* If no rcw in block, back up tptr */
			if ((_cw_type *)BPTR2WP(tptr) - *cwptr == BLKSZMW + 1)
				tptr = tptr - (BLKSZM + 1);
			}
		else
			{
			cos__lrcw(fio, ptr, cwptr, (_cw_type *)BPTR2WP(tptr));
			if ((*cwptr +  GETFWI(*cwptr)) == (_cw_type *)BPTR2WP(tptr))
				return(0);	/* found */
			else 
				/* error: bad _pri */
				ERETURN(stat, FDC_ERR_BADPRI, 0);
			}
		} /* while */
	return(0);
	}

/*	cos__lrcw - find the last rcw in the current block
 *
 *	Returns (on success)
 *		ptr - the start of the corresponding record
 *		cwptr - ptr to the last rcw
 *	If no rcw is found, both ptr and cwptr point to the base of the block
 */
static void
cos__lrcw(
struct fdinfo *fio,
bitptr *ptr,
_cw_type **cwptr,
_cw_type *limcwptr)	/*Limit control word pointer. Do not exceed. */
	{
	_cw_type *tcwptr;
	_cw_type *loc_ptr, *loc_cwptr;
	bitptr tptr;

	SET_BPTR(tptr, BLKBASE(fio->_ptr, fio->_base));
	loc_cwptr = (_cw_type *)BPTR2WP(tptr);
	loc_ptr = (_cw_type *)BPTR2WP(tptr);
	tcwptr = loc_cwptr + GETFWI(loc_cwptr);
	while(tcwptr < limcwptr)
		{
		loc_ptr = loc_cwptr + (CWSIZE >> 6);
		loc_cwptr = tcwptr;
		tcwptr += GETFWI(tcwptr);
		}
	SET_BPTR(*ptr, WPTR2BP(loc_ptr));
	*cwptr = loc_cwptr;
	return;
	}
		
/*
 *
 */
static int
cos__nextblock(
struct fdinfo *fio,
struct ffsw *stat)
	{
	struct fdinfo *llfio;
	int ret, ubc = 0;
	off_t reqpos;
	struct cos_f *cos_info;
	off_t skpos;

	cos_info = (struct cos_f *)fio->lyr_info;

	reqpos = cos_info->cos_bufpos + (cos_info->cos_size >> 3);
	if ((cos_info->cos_cwptr - (_cw_type *)BPTR2WP(fio->_base)) >=
						(cos_info->cos_size >> 6))
		{
/*
 *		Go into single buffer mode after using alt buffer up.
 *		Note that if we are writing, this test will always fail.
 */
		if (cos_info->opos == reqpos)
			{					
			ASWAITUP(fio, stat, &cos_info->bstat, cos_info, ret);
			ASWTCH(fio, cos_info, fio->_base, cos_info->cos_bufpos,
				cos_info->cos_size);
			cos_info->opos = -1;	/* Don't re-use buffer */
			}					
		else
			{
			llfio = fio->fioptr;
                        ASWAITUP(fio, stat, &cos_info->bstat,
			   cos_info, ret);
			if (cos_info->cos_diskpos != reqpos)
				{
				if (cos_info->ffci_flags & FFC_SEEKA)
					{
					if( XRCALL(llfio, seekrtn)
								llfio,
								reqpos,
								0,
								stat
								) < 0)
						return(ERR);
					}
				else
					{
					skpos = reqpos - cos_info->cos_diskpos;
					if( XRCALL(llfio, posrtn)
								llfio,
								FP_RSEEK,
								&(skpos),
								1,
								stat
								) < 0)
						return(ERR);
					}
				}
			ret = XRCALL(llfio, readrtn)	llfio,
							fio->_base,
							fio->_ffbufsiz >> 3,
							stat,
							PARTIAL,
							&ubc);
	
			if (ret <= 0) return(ERR);
			if (ubc != 0) ERETURN(stat, FDC_ERR_BADCOS, 0);
			cos_info->cos_bufpos = reqpos;
			cos_info->cos_diskpos = reqpos + ret;
			}
		cos_info->cos_size = ret << 3;
		fio->_ptr  = fio->_base;
		}
	else 					/* in buffer */
		SET_BPTR(fio->_ptr, BLKLMT(fio->_ptr, fio->_base));

	cos_info->cos_cnt = cos_info->cos_size - 
			SUBT_BPTR(fio->_ptr, fio->_base);
	cos_info->cos_blklmt = fio->_ptr;
	cos_info->cos_cwptr = (_cw_type *)BPTR2WP(fio->_ptr);
	cos_info->cos_cbn++;
	SKIPBCW(cos_info->cos_cwptr, fio->_ptr, fio->_base,
		cos_info->cos_blklmt, cos_info->cos_cnt, cos_info->cos_cbn,
		stat);
	return(0);
	}

/*
 *	cos__getblk - read in target block;
 *
 *	if the block is in buffer,
 *		just adjust the info in *fio
 *	else
 *		count how many blocks are needed from the kernel
 *		flush out the blocks that are leaving the buffer
 *		make kernel request for needed blocks
 *		adjust the pointers
 */

static int
cos__getblk(
struct fdinfo *fio,
int64 bn,
struct ffsw *stat)
	{
	int retv, ubc = 0;
	int64 dif, abn;
	off_t l;
	struct fdinfo *llfio;
	struct cos_f *cos_info;
	off_t sk;
	ssize_t rret;

	cos_info = (struct cos_f *)fio->lyr_info;

	dif = bn - GETBN(BPTR2WP(fio->_base));
/*
 *	If block is in buffer...
 *	First calculate actual block number of base of buffer
 */
	abn = cos_info->cos_cbn -
		((cos_info->cos_cbn & BNMASK) - GETBN(BPTR2WP(fio->_base)));
	if((bn >= abn) && (bn <= cos_info->cos_cbn))
		{
		fio->_ptr = fio->_base;
		SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, dif*BLKSZ));
		cos_info->cos_cbn = bn;
		cos_info->cos_cwptr =
			(_cw_type *)BPTR2WP(fio->_ptr) + GETFWI(BPTR2WP(fio->_ptr));
		SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, CWSIZE));
		cos_info->cos_cnt = cos_info->cos_size -
				SUBT_BPTR(fio->_ptr, fio->_base);
		return(0);
		}
/*
 *	target block is outside the bounds of the buffer
 */
	if(cos_info->cos_flag & COS_IODIRTY)	/* flush dirty buffer */
		{
		retv = _cos_iflush(fio, stat);
		if(retv == -1) return(ERR);
		}
/*
 *	call kernel for needed blocks
 */
	l = (off_t)(bn*BLKSZ >> 3);
	llfio = fio->fioptr;
/*
 *	seek to the right block
 */
	ASWAITUP(fio, stat, &cos_info->bstat, cos_info, retv);
	if (cos_info->ffci_flags & FFC_SEEKA)
		{
		if(XRCALL(llfio, seekrtn) llfio, l, 0, stat) < 0)
			return(ERR);
		}
	else
		{
		sk = l - cos_info->cos_diskpos;
		if (  XRCALL(llfio, posrtn)
					llfio,
					FP_RSEEK,
					&(sk),
					1,
					stat
					) < 0)
			return(ERR);
		}
	rret = XRCALL(llfio, readrtn)	llfio,
					fio->_base,
					fio->_ffbufsiz >> 3,
					stat,
					PARTIAL,
					&ubc);
	if(rret <= 0) return(ERR);
	if (ubc != 0) ERETURN(stat, FDC_ERR_BADCOS, 0);

	cos_info->cos_bufpos = l;
	cos_info->cos_diskpos = l + rret;
	cos_info->cos_cnt = rret << 3;
	cos_info->cos_size = rret << 3;
	cos_info->cos_cbn = bn;
	fio->_ptr = fio->_base;
	cos_info->cos_blklmt = fio->_base;
	cos_info->cos_cwptr = (_cw_type *)BPTR2WP(fio->_base);
	SKIPBCW(cos_info->cos_cwptr, fio->_ptr, fio->_base,
		cos_info->cos_blklmt, cos_info->cos_cnt, cos_info->cos_cbn,
		stat);
	return(0);
	}
