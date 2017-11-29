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


#pragma ident "@(#) libu/ffio/coswrite.c	92.1	06/29/99 13:16:47"

#include <memory.h>
#include <ffio.h>
#include "cosio.h"

static int _cos_wrcw(struct fdinfo *fio, int cwmode, struct ffsw *stat);
static off_t _cos_wrard(struct fdinfo *fio, struct ffsw *stat);
 
static int zero = 0;

/*
 *	write Cos blocked sequential file
 *      
 *      Characters are copied from the user's space to a buffer,
 *      and are written to the file when the buffer becomes full.
 *
 *
 *      if (first i/o request or after rewind)
 *		set counters to indicate the entire buffer can be written to
 *	if (write after read)
 *		{
 *		find control word for record to be written
 *		seek to correct position in file
 *		}
 *	while (there is something left to write)
 *		{
 *		if (on a block boundary)
 *			{
 *                      Set the forward reference for the current block
 *                      if (the buffer is full)
 *				write it
 *			format the new block control word and update the block 
 *			number
 *			}
 *		copy as many bytes as will fit into current block
 *		update counters
 *		}
 *	If FULL mode
 *		write EOR
 *	return(nbytes)
 */

ssize_t
_cos_write(
struct fdinfo *fio,
bitptr bufptr,
size_t nbytes,
struct ffsw *stat,
int fulp, 
int *ubc)
	{
	register int64	pri_base;
	int64		cbn;
	register int	cnt,  flag,  ubits;
	register size_t nbits;
	register size_t bits;	/* number of bits left to write */
 	register int 	rbits; /* number of bits to write to this block */
	_cw_type	*cwptr;
	int64		 tword;
	register bitptr	ptr, base;
	struct cos_f *cos_info;

	cos_info = (struct cos_f *)fio->lyr_info;

#ifdef DEBUG
	if (FDCTRACE(TRACE_COS))
		_xrt_putf("\t_cos_write: enter: cbn=%x, cwptr=%x, ptr=%x+%x\n",
			cos_info->cos_cbn, cos_info->cos_cwptr,
			BPTR2WP(fio->_ptr), BPBITOFF(fio->_ptr));
#endif

	flag	= cos_info->cos_flag;
	if ((flag & COS_IOWRITE) == 0)
		{				/* write after read */
		if(_cos_wrard(fio, stat) < 0)
			return(ERR);
		flag	= cos_info->cos_flag;	/* _cos_wrard could chg flag! */
		}

	cbn 	= cos_info->cos_cbn;		/* current block number	*/
	cnt	= cos_info->cos_cnt;
	ptr	= fio->_ptr;
	base	= fio->_base;
	cwptr 	= cos_info->cos_cwptr;

	fio->rwflag = WRITIN;

	flag &= ~(COS_IOEOF|COS_IOEOD|COS_IOBOD|COS_IOWEOF|COS_IOEOR);

	bits = (nbytes << 3) - *ubc;		/* number of bits to move */
	nbits = bits;				/* number of bits to move */

	while (bits != 0)
		{
		rbits = BLKSZ - (SUBT_BPTR(ptr, base) & BLKSZM);

		if ((rbits & BLKSZM) == 0)
			{	/* block boundary */
		        cos_info->cos_size	= fio->_ffbufsiz - cnt;
			NEXTBLK(fio, base, fio->_ffbufsiz, stat);
			BLDBCW(cwptr, 0, cbn, 0);
			SET_BPTR(ptr, INC_BPTR(ptr, CWSIZE));
			cnt -= CWSIZE;
			rbits = BLKSZ - CWSIZE;
			}

		rbits = (rbits < bits) ? rbits : bits;
/*
 *		Move the bits from the user area to the buffer.
 */
		MOV_BITS(ptr, bufptr, rbits);

		SET_BPTR(ptr, INC_BPTR(ptr, rbits));
		SET_BPTR(bufptr, INC_BPTR(bufptr, rbits));
		cnt   -= rbits;
		bits -= rbits;
		}

	flag |= COS_IOWRITE | COS_IODIRTY;	/* indicate write & dirty */

	fio->recbits += nbits;
/*
 *	If no error and in full record mode, write an end of record.
 */
	SETSTAT(stat, FFCNT, nbytes);
	if (fulp == FULL)
		{
/*
 *		Round up the pointer to the next word boundary.
 */
		ubits = (NBTSPW - BPDWBITOFF(ptr)) & 077;
		if (ubits != 0)
			{
			tword = ((uint64)*(int64 *)BPTR2DWP(ptr)) >> ubits;
			*(int64 *)BPTR2DWP(ptr) = tword << ubits; /* zero rest of word */
			/* up ptr to next word */
			SET_BPTR(ptr, INC_BPTR(ptr,ubits));
			cnt -= ubits;			/* decrement count */
			}
/*
 *		Are we at a block boundary?
 */
		if ((SUBT_BPTR(ptr, base) & BLKSZM) == 0)
			{
			/* go to next block */
		        cos_info->cos_size	= fio->_ffbufsiz - cnt;
			NEXTBLK(fio, base, fio->_ffbufsiz, stat);
			SET_BPTR(ptr, INC_BPTR(ptr, CWSIZE));
			cnt -= CWSIZE;
			BLDBCW(cwptr, 0, cbn, 0);	/* build new BCW */
			}

		SANITYCHK(cnt,LT,0);
	
		/* link to new control word */
		SETFWI(cwptr, (_cw_type *)BPTR2WP(ptr) - cwptr);
		cwptr = (_cw_type *)BPTR2WP(ptr);
	
		/* build RCW */
		BLDRCW(	cwptr,			/* where to put it */
			CWEOR,			/* control word mode */
			ubits,			/* unused bit count */
			0,			/* transparent bit (unused) */
			0,			/* bad data flag */
			0,			/* srs flag */
			cbn - cos_info->cos_pfi,/* previous file index */
			cbn - cos_info->cos_pri,/* previous record index */
			0			/* forward word index */
			);
		SET_BPTR(ptr, INC_BPTR(ptr, CWSIZE));
		cnt -= CWSIZE;

		pri_base = cbn;
		/* block boundary again? */
		if ((SUBT_BPTR(ptr, base) & BLKSZM) == 0)
			pri_base++;
		cos_info->cos_pri	= pri_base;
		flag &= ~(COS_IOEOF|COS_IOWEOF);
		flag |= COS_IOEOR;	/* indicate just after EOR */
		SETSTAT(stat, FFEOR, nbytes);
		fio->last_recbits = fio->recbits;
		fio->recbits = 0;
		}

        cos_info->cos_cnt	= cnt;
        cos_info->cos_size	= fio->_ffbufsiz - cnt;
        fio->_ptr		= ptr;
        cos_info->cos_cbn	= cbn;          /* current block number */
        cos_info->cos_cwptr	= cwptr;
        cos_info->cos_flag	= flag;

	return (nbytes);
	}

static int
_cos_wrcw(
struct fdinfo *fio,
int	cwmode,		/* control word mode/type.  010=RCW, 016=EOF, 017=EOD */
struct ffsw *stat)
	{
        register int	cnt,  flag;
	register int64  cbn;
	register int64  pri_base;
	register int64  pfi_base;
	register bitptr	ptr, base;
	_cw_type *cwptr;
	int64 tword;
	register int ubits;			/* unused bit count */
	struct cos_f *cos_info;

	cos_info = (struct cos_f *)fio->lyr_info;

#ifdef DEBUG
	if (FDCTRACE(TRACE_COS))
		_xrt_putf("\t_cos_wrcw: enter: write %x, cbn=%x, cwptr=%x, ptr=%x+%x, base=%x+%x\n",
			cwmode, cos_info->cos_cbn, cos_info->cos_cwptr,
			BPTR2WP(fio->_ptr), BPBITOFF(fio->_ptr),
			BPTR2WP(fio->_base), BPBITOFF(fio->_base));
#endif

        cbn     = cos_info->cos_cbn;           /* current block number */
        flag    = cos_info->cos_flag;
        cnt     = cos_info->cos_cnt;
        ptr     = fio->_ptr;
        base     = fio->_base;
        cwptr   = cos_info->cos_cwptr;

/*
 *	Round up the pointer to the next word boundary.
 */
	ubits = (NBTSPW - BPDWBITOFF(ptr)) & 077;
	if (ubits > 0)
		{
		tword = ((uint64)*(int64 *)BPTR2DWP(ptr)) >> ubits;
		*(int64 *)BPTR2DWP(ptr) = tword << ubits; /* zero rest of word */
		/* up ptr to next word */
		SET_BPTR(ptr, INC_BPTR(ptr,ubits));
		cnt -= ubits;			/* decrement count */
		}

	if ((SUBT_BPTR(ptr, base) & BLKSZM) == 0)	/* block boundary? */
		{
		/* go to next block */
	        cos_info->cos_size	= fio->_ffbufsiz - cnt;
		NEXTBLK(fio, base, fio->_ffbufsiz, stat);
		SET_BPTR(ptr, INC_BPTR(ptr, CWSIZE));
		cnt -= CWSIZE;
		BLDBCW(cwptr, 0, cbn, 0);	/* build new BCW */
		cwptr = cwptr + (CWSIZE >> 6);
		}
	else
		{
		SETFWI(cwptr,(_cw_type *)BPTR2WP(ptr) - cwptr); /* link to new control word */
		cwptr = (_cw_type *)BPTR2WP(ptr);
		}

	SANITYCHK(cnt,LT,0);

	flag |= COS_IOWRITE | COS_IODIRTY;	/* indicate write & dirty */

	/* build RCW */
	BLDRCW(	cwptr,				/* where to put it */
		cwmode,				/* control word mode */
		ubits,				/* unused bit count */
		0,				/* transparent bit (unused) */
		0,				/* bad data flag */
		0,				/* srs flag */
		cbn - cos_info->cos_pfi,	/* previous file index */
		cbn - cos_info->cos_pri,	/* previous record index */
		0				/* forward word index */
		);
	SET_BPTR(ptr, INC_BPTR(ptr, CWSIZE));
	cnt -= CWSIZE;

	switch(cwmode) {

	case CWEOR:
		pri_base = cbn;
		if ((SUBT_BPTR(ptr, base) & BLKSZM) == 0)	/* block boundary? */
			pri_base++;
	        cos_info->cos_pri	= pri_base;
		flag &= ~(COS_IOEOF|COS_IOWEOF|COS_IOBOD);
		flag |= COS_IOEOR;	/* indicate just after EOR */
		break;

	case CWEOF:
		pfi_base = cbn;
		/* block boundary? */
		if ((SUBT_BPTR(ptr, base) & BLKSZM) == 0)
			pfi_base++;
        	cos_info->cos_pfi	= pfi_base;
	        cos_info->cos_pri	= pfi_base; /* same value */
		flag |= COS_IOEOF|COS_IOWEOF;
		flag &= ~(COS_IOEOR|COS_IOBOD);	/* indicate not just after EOR */
		break;

	case CWEOD:
		flag |= COS_IOEOD;
		flag &= ~(COS_IOEOR|COS_IOBOD);	/* indicate not just after EOR */
/*
 *		remember the 'last EOD position' for trunc on close.  
 */
		cos_info->cos_lasteod = cbn*BLKSZ +
			((SUBT_BPTR(ptr, base) - 1) % BLKSZ) + 1;

/*
 *		just wrote EOD. pretend to backspace and leave position
 *		*before* the EOD.  Also leave in READ mode.
 */ 
		SET_BPTR(ptr, INC_BPTR(ptr, -CWSIZE));	/* back up over EOD */
		cwptr	= (_cw_type *)BPTR2WP(ptr);
		cnt	+= CWSIZE;/* Note: don't change size, EOD still in buf*/
		flag	&= ~COS_IOWRITE;
		break;

	default:
		ERETURN(stat, FDC_ERR_BADRCW, 0);
	}



        cos_info->cos_cnt	= cnt;
        cos_info->cos_size	= fio->_ffbufsiz - cnt;
        fio->_ptr		= ptr;
        cos_info->cos_cbn	= cbn;           /* current block number */
        cos_info->cos_cwptr	= cwptr;
        cos_info->cos_flag	= flag;

	return (0);
	}

/*
 * _cos_weof()
 *	Write an EOF on the file
 */
int
_cos_weof(
struct fdinfo *fio,
struct ffsw *stat)
	{
	struct cos_f *cos_info;

	cos_info = (struct cos_f *)fio->lyr_info;

/*
 *	Check for initial write, or write after read/rewind/backspace.
 */
	if ((cos_info->cos_flag & COS_IOWRITE) == 0)	/* write after read */
		if (_cos_wrard(fio, stat) < 0) return(ERR);

/*
 *	Check for preceeding EOR/EOF/BOD.
 */
	if ((cos_info->cos_flag & COS_IOEOR) == 0)
		if ((cos_info->cos_flag & (COS_IOBOD|COS_IOEOF|COS_IOWEOF)) ==0)
			if (_cos_wrcw(fio, CWEOR, stat) < 0)
				return(ERR);
	if ( _cos_wrcw(fio, CWEOF, stat) < 0)
		return(ERR);

	fio->rwflag = WRITIN;
	fio->last_recbits = fio->recbits;
	fio->recbits = 0;
	fio->ateof = YES;
	fio->ateod = NO;
	SETSTAT(stat, FFEOF, 0);
	return(0);
	}

/*
 * _cos_weod()
 *	Write an EOD on the file.  Note that this is the only place that 
 *	ateof AND ateod are set.  This is because COS blocking does not
 *	allow you to be positioned after an EOD, so after writing an EOD, 
 *	your position remains between the EOF and EOD.
 */
int
_cos_weod(
struct fdinfo *fio,
struct ffsw *stat)
	{
	struct cos_f *cos_info;

	cos_info = (struct cos_f *)fio->lyr_info;

/*
 *	Check for initial write, or write after read/rewind/backspace.
 */
	if ((cos_info->cos_flag & COS_IOWRITE) == 0)
		if (_cos_wrard(fio, stat) < 0) return(ERR);

	if ((cos_info->cos_flag  & (COS_IOBOD|COS_IOEOF|COS_IOWEOF)) == 0)
		{
		if ((cos_info->cos_flag  & COS_IOEOR) == 0)
			if (_cos_wrcw(fio, CWEOR, stat) < 0)
				return(ERR);
		if (_cos_wrcw(fio, CWEOF, stat) < 0)
			return(ERR);
/*
 *		The write of the EOD will leave us in *read* mode, and
 *		positioned *before* the EOD.  This is special cased
 *		in _cos_wrcw.
 */
		if (_cos_wrcw(fio, CWEOD, stat) < 0)
			return(ERR);
/*
 *		WEOD should *not* change our position, so backspace over the
 *		EOF. Avoid doing this if we are in the close routine. There's
 *		no point, and if the file was only WRONLY, this can cause
 *		a read.
 */
		if ((cos_info->cos_flag & COS_IOCLOSE) == 0)
			if (XRCALL(fio, backrtn) fio, stat) < 0)
				return(ERR);
		}
	else
		{
		if (_cos_wrcw(fio, CWEOD, stat) < 0)
			return(ERR);
		}


	fio->rwflag = WRITIN; /* Wrong?? */
	fio->last_recbits = fio->recbits;
	fio->recbits = 0;
	fio->ateof = YES;
	fio->ateod = YES;
	SETSTAT(stat, FFEOD, 0);
	return(0);
	}

int
_cos_iflush(
struct fdinfo *fio,
struct ffsw *stat)
	{
	int ret;
	ssize_t ret2;
	struct fdinfo *llfio;
	struct cos_f *cos_info;
	off_t skpos;

	cos_info = (struct cos_f *)fio->lyr_info;
	llfio = fio->fioptr;

	ASWAITUP(fio, stat, &cos_info->bstat, cos_info, ret);

	if (cos_info->bstat.sw_error != 0)
		{
		ERETURN(stat, cos_info->bstat.sw_error, 0);
		}

	if (cos_info->cos_bufpos != cos_info->cos_diskpos)
		{
		off_t pos;
		if (cos_info->ffci_flags & FFC_SEEKA)
			{
			pos = XRCALL(fio->fioptr, seekrtn) fio->fioptr,
				cos_info->cos_bufpos, 0, stat);
			}
		else
			{
			skpos = cos_info->cos_bufpos - cos_info->cos_diskpos;
			pos = XRCALL(fio->fioptr,posrtn) fio->fioptr,
				FP_RSEEK, &(skpos), 1, stat);
			}
		if (pos < 0)
			return(ERR);
		cos_info->cos_diskpos = pos;
		}

/*
 *	Zero the portion of the final block which lies beyond the EOD control 
 *	word.   
 */
	if (cos_info->cos_lasteod) {
		int zbits;
		char *zptr;
		zbits = CEIL(cos_info->cos_lasteod, BLKSZ) -
			cos_info->cos_lasteod;
		zptr = BPTR2CP(fio->_base) + 
			((CEIL(cos_info->cos_size, BLKSZ) - zbits) >> 3);
		if (zbits > 0)
			memset(zptr, 0, zbits >> 3);
	}
/*
 *	Now flush the buffer.
 */
	ret2 = XRCALL(llfio, writertn) llfio,
				fio->_base,
				CEIL(cos_info->cos_size, BLKSZ) >> 3,
				stat,
				FULL,
				&zero);

	if (ret2 < 0) return(ERR);	/* write error */

	cos_info->cos_diskpos += ret2;
	fio->_ptr = fio->_base;
	cos_info->cos_cwptr = (_cw_type *)BPTR2WP(fio->_base);
	cos_info->cos_cnt = fio->_ffbufsiz;
	cos_info->cos_flag &= ~COS_IODIRTY;	/* clear dirty flag */
	return(0);
	}

/*
 *	_cos_wrard()
 *		handle write after read
 */
static off_t
_cos_wrard(
struct fdinfo *fio,
struct ffsw *stat)
	{
	_cw_type *tcwptr, *ncwptr;
	off_t bpos;
	long  ret;
	off_t pos;
	struct fdinfo *llfio;
	struct cos_f *cos_info;
	off_t skpos;

	cos_info = (struct cos_f *)fio->lyr_info;
	llfio = fio->fioptr;
/*
 *	Make sure async I/O is done.
 */
	ASWAITUP(fio, stat, &cos_info->bstat, cos_info, ret);
/*
 *	If file just opened or after rewind, don't bother.
 */
	if (cos_info->cos_cbn == -1)
		{
/*
 *		This is an initial write or write after a rewind/backspace.  
 *		Make sure that the lower level layer is positioned properly.  
 *		Do a seek only if not already in proper position for write.
 */
		if (cos_info->cos_diskpos != 0)
			{
			if (XRCALL(llfio, seekrtn) llfio, (off_t)0, 0, stat) < 0)
				return(ERR);
			}
		cos_info->cos_diskpos = 0;
		cos_info->cos_bufpos = 0;
		cos_info->cos_size = 0;
		cos_info->opos = -1;	/* Void 'other' buffer */
		cos_info->cos_cnt = fio->_ffbufsiz;
		cos_info->cos_flag |= COS_IOBOD;
		return(0);
		}

/*
 *	Switch from reading to writing.
 */
	ncwptr = (_cw_type *)BPTR2WP(fio->_base); /* move cwptr to previous cwptr */
	tcwptr = ncwptr + GETFWI(ncwptr);
	while(tcwptr < cos_info->cos_cwptr)
		{
		ncwptr = tcwptr;
		tcwptr += GETFWI(tcwptr);
		}
	SANITYCHK(tcwptr,NE,cos_info->cos_cwptr); /* if not, we got trouble! */
/*
 *	Write after read must begin on record boundary
 */
	switch(GETM(ncwptr))
		{
		case CWEOR:
			cos_info->cos_flag |= COS_IOEOR; /* set EOR */
/*
 *			Clear EOF, EOD, and BOD
 */
			cos_info->cos_flag &=
				~(COS_IOEOF | COS_IOEOD | COS_IOBOD);
			break;
		case CWBCW:
			if (GETM(cos_info->cos_cwptr) != CWEOD)
				{
/*
 *				If limit CW is BCW, the we *must* be at BOD
 */
				if (ncwptr == (_cw_type *)BPTR2WP(fio->_base)
					&& cos_info->cos_cbn == 0)
					{
/*
 *					Clear EOF and EOR, we are at BOD
 */
					cos_info->cos_flag &=
						~(COS_IOEOF | COS_IOEOR);
					cos_info->cos_flag |= COS_IOBOD;
					break;
					}
				else
					ERETURN(stat, FDC_ERR_BADCOS, 0);
				}
/*
 *			If it was EOD, previous *must* be EOF
 *			Fall into EOF processing.
 */
		case CWEOF:
/*
 *			Clear EOR and BOD
 */
			cos_info->cos_flag &= ~(COS_IOEOR | COS_IOBOD);
			cos_info->cos_flag |= COS_IOEOF; /* set EOF */
			break;
		case CWEOD:
			ERETURN(stat, FDC_ERR_BADCOS, 0);
		}
	cos_info->cos_cwptr = ncwptr;
	SET_BPTR(fio->_ptr, WPTR2BP(cos_info->cos_cwptr + (CWSIZE >> 6)));

	cos_info->cos_size = SUBT_BPTR(fio->_ptr, fio->_base);
	cos_info->cos_cnt = fio->_ffbufsiz - cos_info->cos_size;
/*
 *	Set up base block numbers for PRI/PFI
 *
 *	PRI base is the current block, unless cwpot is pointing to Control
 *	word.  In that case, the base is cbn + 1.
 *
 *	PFI must be relied upon to be accurate from the read process, 
 *	as there may not be any nearby RCWs.
 */
	cos_info->cos_pri = cos_info->cos_cbn;
	if ((SUBT_BPTR(fio->_ptr, fio->_base) & BLKSZM) == 0)
		{
		cos_info->cos_pri++;
		}
	bpos = cos_info->cos_bufpos << 3;			/* in bits */
#if DEBUG
	if (cos_info->cos_size > 0)
		SANITYCHK((long)(GETBN(BPTR2WP(fio->_base)) * BLKSZ),NE,bpos);
#endif
/*
 *	Seek lower layer to match position of base of the buffer.
 */
	
	if (cos_info->ffci_flags & FFC_SEEKA)
		{
		pos = XRCALL(llfio, seekrtn) llfio, bpos>>3, 0, stat);
		}
	else
		{
		skpos = (bpos>>3) - cos_info->cos_diskpos;
		pos = XRCALL(llfio,posrtn) llfio,
			FP_RSEEK, &(skpos), 1, stat);
		}
	if (pos >= 0) cos_info->cos_diskpos = (bpos>>3);
	return(pos);
	}
