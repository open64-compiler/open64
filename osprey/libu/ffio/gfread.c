/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

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


#pragma ident "@(#) libu/ffio/gfread.c	92.1	06/29/99 13:16:47"

#include <ffio.h>
#include <string.h>
#include "gfio.h"

static int
check_pad_8( bitptr, int, char);

static
setend(struct fdinfo *fio, struct ffsw *stat);

/*
 * Read a generic F or FB format file.
 *	This routine takes advantage of the similarities between
 *	the following record structures:
 *		IBM:	F, FB
 *		VMS:	F.disk, F.tape, F.tr
 *	Records and blocks are supported only in multiples of 8 bits.
 *
 *	There are three primary variables used in processing.
 *		one is whether the format is blocked or unblocked
 *			(this is derived from MBS and maxrec)
 *		two is whether padding is accepted at the end of blocks or
 *			records
 *		If padding is accepted, what kind of padding is it.
 * Parameters:
 *      fd      - file descriptor (dummy)
 *      bufptr  - bit pointer to where data is to go.
 *      nbytes  - Nuber of bytes to be read
 *      stat    - pointer to status return word
 *      fulp    - full or partial write mode flag
 *      ubc     - pointer to unused bit count (not used)
 */

_gen_fread(fio, bufptr, nbytes, stat, fulp, ubc)
struct fdinfo *fio;
int nbytes, fulp, *ubc;
bitptr bufptr;
struct ffsw *stat;
	{
	int nbits, ret;

	nbits = nbytes << 3;
	if (*ubc != 0)
		ERETURN(stat, FDC_ERR_UBC, 0);

	if (fio->rwflag == WRITIN) 
		{ 
		/* read after write error */ 
		ERETURN(stat, FDC_ERR_RAWR, 0); 
		} 
	fio->rwflag = READIN;

/*
 * If buffer is MT, read the next block. This read must read exactly one block.
 * If the user has declared his block or record sizes incorrectly, errors will
 * result.
 */
newblock:
	if (fio->_cnt == 0)
		{
		fio->_ptr = fio->_base;
		READBLK(ret, fio, (fio->maxblksize >> 3), stat, FULL, ubc);
		if (ret < 0)
			return(ret);
		if (fio->_cnt > 0)
			fio->segbits = fio->maxrecsize;
		else /* nothing was read, must be EOF or EOD */
			{
			return(setend(fio, stat));
			}
		}

/*
 * The user may request more than the record length.  One record is all he gets.
 */
	if (nbits > fio->segbits) 
		{
		nbits = fio->segbits;
		}

/*
 *	Move the data to the caller.  If the request cannot be
 *	satisfied, then we are at EOF or EOD
 */
	if (fio->_cnt >= nbits)
		{
		GETDATA(bufptr, fio, nbits);
		}
	else if (fio->_cnt > 0)
		{
/*
 *		This is an error for formats with no padding.
 *		Blocks must be multiples of recsize
 */
		if (((struct gen_ff *)fio->lyr_info)->padd == YES)
			{
/*
 *			check the padding chars
 *			start at _ptr, for _cnt bits checking for pchar
 */
			if (check_pad_8(fio->_ptr,fio->_cnt,
				((struct gen_ff *)fio->lyr_info)->pchar) == YES)
				{
				/* skip remaining bits in block */
				fio->_cnt = 0;
				goto newblock;
				}
			else
				{
				fio->_cnt = 0;
				ERETURN(stat, FDC_ERR_FMT, 0);
				}
			}
		else
			{
			fio->_cnt = 0;
			ERETURN(stat, FDC_ERR_PADD, 0);
			}
		}
	else
		{
		/* count is zero, must be EOF or EOD */
		return(setend(fio, stat));
		}
/*
 *      check if we just hit FFEOR
 */
	if (fio->segbits == 0)
		{
		if (fio->_cnt > 0)
			{
			/* reset seg bit count */
			fio->segbits = fio->maxrecsize;
			if (fio->segbits > fio->_cnt)
				{
				fio->_cnt = 0;
				fio->segbits = 0;
				}
			}
		SETSTAT(stat, FFEOR, nbits >> 3);
		fio->last_recbits = fio->recbits;
		fio->recbits = 0;
		}
	else
		{
/*
 *		if full record mode, skip to beginnng of next record
 */
		if (fulp == FULL)
			{
			SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, fio->segbits));
			fio->_cnt -= fio->segbits;
			if (fio->_cnt > 0)
				{
				/* reset seg bit count */
				fio->segbits = fio->maxrecsize;
				if (fio->segbits > fio->_cnt)
					{
					fio->_cnt = 0;
					fio->segbits = 0;
					}
				}
			}
		SETSTAT(stat, FFCNT, nbits >> 3);
		fio->recbits += nbits;
		}
		
	return (nbits >> 3);
	}

/*
 * Check the padd characters starting at 'ptr', and for 'bits' bits.
 * if any are not equal to pchar, return (NO).
 */
static int
check_pad_8(ptr, bits, pchar)
bitptr ptr;
int bits;
char pchar;
	{
	char *cp;

	cp = BPTR2CP(ptr);
	while(bits > 0)
		{
		if (*cp != pchar) return(NO);
		bits -= 8;
		cp++;
		}
	return(YES);
	}

/*
 *	Common code to handle EOF and EOD return
 */
static
setend(struct fdinfo *fio, struct ffsw *stat)
	{
	fio->segbits = 0;
	fio->last_recbits = fio->recbits;
	fio->recbits = 0;
	if (stat->sw_stat == FFEOF)
		{
		fio->ateof = 1;
		fio->ateod = 0;
		return(0);
		}
	else if (stat->sw_stat == FFEOD)
		{
		fio->ateof = 0;
		fio->ateod = 1;
		return(0);
		}
	else
		ERETURN(stat, FDC_ERR_INTERR, 0);
	}
