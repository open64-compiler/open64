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


#pragma ident "@(#) libu/ffio/cosseek.c	92.1	06/29/99 13:16:47"

#include <ffio.h>
#include "cosio.h"


/*
 *	Rewind a cos_blocked file
 */
_ffseek_t
_cos_seek(
struct fdinfo *fio,
off_t pos, 
int whence,
struct ffsw *stat)
	{
	int ret, ubc = 0;
	ssize_t rret;
	off_t sret;
	off_t locpos;
	off_t epos, bpos;
	_cw_type *loc_cwptr, *loc_ptr, *lim_wd;
	struct fdinfo *llfio;
	struct cos_f *cos_info;

/*
 *	The 'whence' values 0, 1, and 2 are supported, but only for specific
 *	purposes.  Rewind is done with '0,0',  seek-to-end with '0,2', and
 *	getpos with '0,1'.  Setpos is always with a whence of 0.
 *		for:
 *			whence = 0, pos must be: anything positive
 *			whence = 1, pos must be: 0
 *			whence = 2, pos must be: 0
 */
	if (pos != 0 && whence != 0)
		{
		ERETURN(stat, FDC_ERR_NOSUP, 0);
		}
	if ((pos & 7) != 0 || pos < 0)
		ERETURN(stat, FDC_ERR_REQ, 0);

	llfio = fio->fioptr;
	cos_info = (struct cos_f *)fio->lyr_info;
/*
 *	Better wait for outstanding I/O...
 */
	ASWAITUP(fio, stat, &cos_info->bstat, cos_info, ret);
/*
 *	Get current position...
 */
	if (whence == 1)
		{
/*
 *		If we cannot do an absolute seek, then this operation
 *		is not currently supported. Tapes are an example
 */
		if ((cos_info->ffci_flags & FFC_SEEKA) == 0)
			{
			ERETURN(stat, FDC_ERR_NOGPOS, 0);
			}
#ifdef DEBUG
		locpos = XRCALL(llfio, seekrtn) llfio, pos, whence, stat);
		if (locpos < 0) return(locpos);
		SANITYCHK(cos_info->cos_diskpos, NE, locpos);
#endif

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
/*
 *	REWIND/SETPOS
 */
	else if (whence == 0)
		{
/*
 *		If last op was write, write an eod
 */
		if ( cos_info->cos_flag & COS_IOWRITE )
			{
			/* write EOD */
			if (XRCALL(fio, weodrtn) fio, stat) < 0)
				return(ERR);
			}
		ASWAITUP(fio, stat, &cos_info->bstat, cos_info, ret);
/*
 *		If pos is not zero, this is a setpos operation.  Go off and
 *		do it.
 *		Also check for moral equivalent of rewind.
 */
		if (pos != 8 && pos != 0)
			{
			return (_cos_setpos(fio, cos_info, 1, pos, (long *)0, stat));
			}
/*
 *		If we have the start of the file in memory, don't do any I/O.
 *		simply reset the pointers to the data already in buffer.
 */
		if (cos_info->cos_bufpos == 0 && cos_info->cos_size > 0)
			{
			cos_info->cos_cnt = cos_info->cos_size;
			}
		else if (cos_info->opos == 0 && cos_info->osiz > 0)
			{
			/* Note that we waited for outstanding I/O */
			/* (ASWAITUP) already */
			ASWTCH(fio, cos_info, fio->_base, cos_info->cos_bufpos,
				cos_info->cos_size);
			cos_info->cos_cnt = cos_info->cos_size;
			}
		else /* need to flush buffer if dirty */
			{
			if (cos_info->cos_flag & COS_IODIRTY) /* flush dirty buffer */
				{
				if (_cos_iflush(fio, stat) < 0)
					return(ERR);
				}
			if(XRCALL(llfio, seekrtn) llfio, (off_t)0, 0, stat) < 0)
				return(ERR);

			cos_info->cos_cnt = 0;
			cos_info->cos_size = 0;
			cos_info->cos_diskpos = 0;
			}
		cos_info->cos_pri = 0;
		cos_info->cos_pfi = 0;
		cos_info->cos_cbn = -1;
		cos_info->cos_cwptr = (_cw_type *)BPTR2WP(fio->_base);
		cos_info->cos_blklmt = fio->_base;
		fio->_ptr = fio->_base;

		cos_info->cos_flag &= ~(COS_IOEOR |
					COS_IOEOD | COS_IOWRITE);
		cos_info->cos_flag |= COS_IOEOF;	/* pretend EOF at BOD */
		cos_info->cos_flag |= COS_IOBOD;	/* pretend BOD is EOF */

		fio->ateof = NO;
		fio->ateod = NO;
		}
	else if (whence == 2)
		{
/*
 *		SEEK to END (to a position just prior to the EOD control word)
 */

/*
 *		If last op was write, write an eod
 */
		if ( cos_info->cos_flag & COS_IOWRITE )
			{
			/* write EOD */
			if (XRCALL(fio, weodrtn) fio, stat) < 0)
				return(ERR);
			}
		ASWAITUP(fio, stat, &cos_info->bstat, cos_info, ret);
		/* need to flush buffer if dirty */
		if (cos_info->cos_flag & COS_IODIRTY) /* flush dirty buffer */
			if (_cos_iflush(fio, stat) < 0)
				return(ERR);
/*
 *		If lasteod is zero, we have not yet written an EOD
 */
		if (cos_info->cos_lasteod > 0)
			epos = CEIL(cos_info->cos_lasteod, BLKSZ);
		else
			{
/*
 *			Figure out the end of the file
 */
			epos  = XRCALL(llfio, seekrtn)
					llfio, 0L, 2, stat);
			if ( epos == 0 ) return(0);
			cos_info->cos_diskpos = epos;
			epos = epos << 3;
			}
/*
 *		Find the beginning of the last block of the file.
 *		There must be at least one.  There must also be at
 *		least two words there.
 */
		bpos = FLOOR(epos-1, BLKSZ);
		if ((epos - bpos) < 2*NBTSPW) /* two words */
			ERETURN(stat, FDC_ERR_BADCOS, 0);

		if( XRCALL(llfio, seekrtn) llfio, bpos>>3, 0, stat) < 0)
			return(ERR);
/*
 *		Read in the block.
 */
		rret = XRCALL(llfio, readrtn)
				llfio,
				fio->_base,
				fio->_ffbufsiz >> 3,
				stat,
				PARTIAL,
				&ubc);
		if (rret < 0) return(ERR);
		if (ubc != 0) ERETURN(stat, FDC_ERR_BADCOS, 0);

		cos_info->cos_size = rret << 3;
		cos_info->cos_bufpos   = bpos >> 3;
/*
 *		Seek back to the beginning of the block.
 */
		sret = XRCALL(llfio, seekrtn) llfio, bpos>>3, 0, stat);
		if (sret < 0) return(ERR);
		fio->_ptr = fio->_base;
		cos_info->cos_diskpos   = sret;
		cos_info->cos_cwptr   = (_cw_type *)BPTR2WP(fio->_base);
		cos_info->cos_cbn  = bpos >> BLKSZ2;
/*
 *		Find the EOD control word.
 */
		loc_ptr = (_cw_type *)BPTR2WP(fio->_base);
		loc_cwptr = (_cw_type *)BPTR2WP(fio->_base);
		lim_wd = (_cw_type *)BPTR2WP(fio->_base) + (cos_info->cos_size >> 6);
		while(GETM(loc_cwptr) != CWEOD)
			{
			loc_ptr = loc_cwptr + (CWSIZE >> 6);
			loc_cwptr += GETFWI(loc_cwptr);
			if (loc_cwptr > lim_wd)
				{
				ERETURN(stat, FDC_ERR_BADRCW, 0);
				}
			}
		SET_BPTR(fio->_ptr, WPTR2BP(loc_ptr));
		cos_info->cos_cwptr = loc_cwptr;
		SET_BPTR(cos_info->cos_blklmt, BLKLMT(fio->_ptr, fio->_base));

		cos_info->cos_cnt  = cos_info->cos_size + SUBT_BPTR(fio->_base,fio->_ptr);
		cos_info->cos_flag &= ~(COS_IOEOR | COS_IOWRITE);
		cos_info->cos_flag |= COS_IOEOF | COS_IOEOD;	/* btwn EOF and EOD */
		fio->ateof = YES;
		fio->ateod = YES;
		}
	else
		ERETURN(stat, FDC_ERR_BADSK, 0);
	fio->rwflag = POSITIN;
	fio->last_recbits = fio->recbits;
	fio->recbits = 0;
	return(pos);
	}
/*
 * _cos_poschng() Changes position to "reqpos". 
 *		Waits for outstanding async i/o
 * Returns: 	ERR if an error. 
 *		0 if OK
 */
int
_cos_poschng(
struct cos_f *cos_info,
off_t reqpos,
struct fdinfo *fio,
struct ffsw *stat)
{
	int ret;
	off_t skpos;

	if (cos_info->ffci_flags & FFC_SEEKA) {
		if ( XRCALL(fio->fioptr, seekrtn)
			fio->fioptr,            
			reqpos,                
			0,                      
			stat                   
			) < 0)
			return(ERR);        
		ASWAITUP(fio, stat, &cos_info->bstat, cos_info, ret);
	}                               
	else {
		ASWAITUP(fio, stat, &cos_info->bstat, cos_info, ret);
		skpos = reqpos - cos_info->cos_diskpos;
		if(  XRCALL(fio->fioptr,posrtn)
			fio->fioptr,
			FP_RSEEK,
			&(skpos),
			1,      
			stat) < 0)
			return(ERR);            
	}                      
	cos_info->cos_diskpos = reqpos;         
	return(0);
}

