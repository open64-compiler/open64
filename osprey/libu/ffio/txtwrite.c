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


#pragma ident "@(#) libu/ffio/txtwrite.c	92.2	10/29/99 21:40:31"

#include <ffio.h>
#include <string.h>
#include "txtio.h"

/*
 * Write a TEXT format file.
 *	
 *	Records and blocks are supported only in multiples of 8 bits.
 *	No padding is done.  Record lengths are variable and have no
 *	logical limit.
 *
 * Parameters:
 *      fio     - Pointer to fdinfo block
 *      bufptr  - bit pointer to where data is to go.
 *      nbytes  - Number of bytes to be written
 *      stat    - pointer to status return word
 *      fulp    - full or partial write mode flag
 *      ubc     - pointer to unused bit count (not used for IBM)
 */

/*
 * Global definitions.
 */
static int 	zero = 0;

ssize_t
_txt_write(struct fdinfo *fio, bitptr bufptr, size_t nbytes, struct ffsw *stat, int fulp, int *ubc)
	{
	int64 	bits,
		nbits; 
	ssize_t	ret;
	struct text_f *text_info;
	struct fdinfo *llfio;

	nbits = (uint64)nbytes << 3;	/* convert to bits */
	if (*ubc != 0)		/* can only write in bytes */
		ERETURN(stat, FDC_ERR_UBC, 0);
		
	text_info = (struct text_f *)fio->lyr_info;

	llfio = fio->fioptr;
	if (fio->ateod != 0)
		ERETURN(stat, FDC_ERR_WPEOD, 0);

/*
 *	If NOT writing or just positioned, then error.
 */
	if (fio->rwflag == READIN)
		ERETURN(stat, FDC_ERR_WRARD, 0);
	if (fio->rwflag == POSITIN) {
/*
 *		Any data that is in the buffer is there because of a
 *		read. Adjust the position accordingly.
 */
		if (fio->_cnt != 0){
		   if (XRCALL(llfio, seekrtn)llfio, (off_t)-(fio->_cnt/8), 1, stat) < (_ffseek_t)0)
			return(ERR);
		}
		fio->_cnt = 0;
		fio->_ptr = fio->_base;
	}

	fio->rwflag = WRITIN; 	/* set operation flag */
/*
 *	Move the data to the buffer.  But first check to see if it will
 *	fit.  If not, write out the block.
 */
	while (nbits > 0)
		{
		bits = nbits;
		if ((fio->_cnt + nbits) > fio->_ffbufsiz) 
			bits = fio->_ffbufsiz - fio->_cnt;

		PUTDATA(bufptr, fio, bits);
		fio->recbits += bits;
		SET_BPTR(bufptr, INC_BPTR(bufptr, bits));
/*
 *		Check for full buffer
 */
		if (fio->_cnt == fio->_ffbufsiz) 
			{
			WRITEBLK(ret, fio,(size_t)(fio->_cnt >> 3), stat, FULL, &zero);
			if (ret < 0)
				return(ret);
			}
	
		nbits -= bits;
		}

/* 	If full record mode, write an EOR mark and skip to the beginning
 *	of the next record.
 */
	SETSTAT(stat, FFCNT, nbytes);
	fio->ateod = 0;
	fio->ateof = 0;
	if (fulp == FULL) 
		{
/*
 *		We know we have at least room for one byte, due to above check.
 *		Put EOR mark in buffer and increment stuff.
 */
		PUT_BITS(fio->_ptr, text_info->eor_char, CHAR_LEN);
		SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, CHAR_LEN));
		fio->_cnt += CHAR_LEN;

		if (fio->_cnt == fio->_ffbufsiz)
			{
			WRITEBLK(ret, fio,(size_t)(fio->_cnt >> 3), stat, FULL,&zero);
			if (ret < 0)
				return(ret);
			}
		fio->segbits = 0;
		fio->last_recbits = fio->recbits;
		fio->recbits = 0;
		fio->ateor = 1;

		SETSTAT(stat, FFEOR, nbytes);
		}
	else
		{
		fio->ateor = 0;
		}

	return (nbytes);
	}

/*
 * Make the EOF.
 */
int
_txt_weof(struct fdinfo *fio, struct ffsw *stat)
	{
	struct fdinfo 	*llfio;
	int 	ret;
	struct text_f *text_info;

	llfio = fio->fioptr;
	text_info = (struct text_f *)fio->lyr_info;
	if (fio->rwflag == POSITIN) {
/*
 *		Any data that is in the buffer is there because of a
 *		read. Adjust the position accordingly.
 */
		if (fio->_cnt != 0){
		   if (XRCALL(llfio, seekrtn)llfio, (off_t)-(fio->_cnt/8), 1, stat) < (_ffseek_t)0)
			return(ERR);
		}
		fio->_cnt = 0;
		fio->_ptr = fio->_base;
	}

	if (fio->rtype == TEXT_NL)	/* 'normal' single file text */
		{
		ret = XRCALL(llfio, weofrtn) llfio, stat);
		if (ret < 0)
			ERETURN(stat, FDC_ERR_NWEOF, 0);

		SETSTAT(stat, FFEOF, 0) 

		fio->segbits = 0;
		fio->last_recbits = fio->recbits;
		fio->recbits = 0;
		fio->ateof = 1;
		fio->ateod = 0;
		fio->ateor = 0;

		return(0);
		}
	/* Write after Read !! */
	if (fio->rwflag == READIN)
		ERETURN(stat, FDC_ERR_WRARD, 0);

	fio->rwflag = WRITIN;
/*
 *	Put EOF and EOR mark in buffer and increment stuff.
 *	Since the EOF includes the newline char, cheat.  Tell the write routine
 *	that it is a PARTIAL write to suppress the eor marker.
 */
	if(XRCALL(fio, writertn) fio, WPTR2BP(&text_info->eof_mark),
			(size_t)(text_info->eof_len >> 3), stat, PARTIAL, &zero) < (ssize_t)0)
		return(ERR);
	SETSTAT(stat, FFEOF, 0) 

	fio->segbits = 0;
	fio->last_recbits = fio->recbits;
	fio->recbits = 0;
	fio->ateof = 1;
	fio->ateod = 0;
	fio->ateor = 0;

	return(0);
	}

/*
 * Mark the EOD.
 */
int
_txt_weod(struct fdinfo *fio, struct ffsw *stat)
	{
	struct fdinfo *llfio;
	int ret;
	ssize_t wret;

	/* Write after Read !! */
	if (fio->rwflag == READIN)
		ERETURN(stat, FDC_ERR_WRARD, 0);
	llfio = fio->fioptr;
	if (fio->rwflag == POSITIN) {
/*
 *		Any data that is in the buffer is there because of a
 *		read. Adjust the position accordingly.
 */
		if (fio->_cnt != 0){
		   if (XRCALL(llfio, seekrtn)llfio, (off_t)-(fio->_cnt/8), 1, stat) < (_ffseek_t)0)
			return(ERR);
		}
		fio->_cnt = 0;
		fio->_ptr = fio->_base;
	}

	fio->rwflag = WRITIN;
	if (fio->_cnt != 0)
		{
		WRITEBLK(wret, fio, (size_t)(fio->_cnt >> 3), stat, FULL, &zero);
		if (wret < 0) return (ERR);
		}
/*
 *	Everybody gets truncated here (we hope).
 */
	ret = XRCALL(llfio, weodrtn) llfio, stat);
	if (ret < 0)
		return(ERR);
	fio->segbits = 0;
	fio->last_recbits = fio->recbits;
	fio->recbits = 0;
	fio->ateor = 0;
	fio->ateof = 0;
	fio->ateod = 1;

	return(0);
	}
