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


#pragma ident "@(#) libu/ffio/gfwrite.c	92.1	06/29/99 13:16:47"

#include <ffio.h>
#include <string.h>
#include "gfio.h"

#ifdef KEY
static put_block( struct fdinfo *, struct ffsw * );
#endif

/*
 * Write a generic F or FB format file.
 *	This routine takes advantage of the similarities between
 *	the following record structures:
 *		IBM:	F, FB
 *		VMS:	F.disk, F.tape, F.tr
 *	Records and blocks are supported only in multiples of 8 bits.
 *
 *	There are three primary variables used in processing.
 *		one is whether the format is blocked or unblocked
 *			(this is derived from MBS and maxrec)
 *		two is whether padding is done at the end of blocks or
 *			records
 *		If padding is necessary, what kind of padding is done.
 * Parameters:
 *      fd      - file descriptor (dummy)
 *      bufptr  - bit pointer to where data is to go.
 *      nbytes  - Nuber of bytes to be written
 *      stat    - pointer to status return word
 *      fulp    - full or partial write mode flag
 *      ubc     - pointer to unused bit count (not used for IBM)
 */

_gen_fwrite(fio, bufptr, nbytes, stat, fulp, ubc)
struct fdinfo *fio;
int nbytes, fulp, *ubc;
bitptr bufptr;
struct ffsw *stat;
	{
	int nbits, ret;

	nbits = nbytes << 3;
	if (*ubc != 0)
		{
		ERETURN(stat, FDC_ERR_UBC, 0);
		}

		
	if (fio->ateod != 0)
		{
		ERETURN(stat, FDC_ERR_WPEOD, 0);
		}
/*
 *	If NOT writing or just positioned, then error.
 */
	if (fio->rwflag == READIN)
		{
		ERETURN(stat, FDC_ERR_WRARD, 0);
		}
	fio->rwflag = WRITIN;

/*
 *	The user may not request more than the record length.
 */
	if (nbits + fio->recbits > fio->maxrecsize) 
		{
		ERETURN(stat, FDC_ERR_MXREC, 0);
		}

/*
 *	Move the data to the buffer.
 */
	PUTDATA(bufptr, fio, nbits);
	fio->recbits += nbits;

	/* if full record mode, skip to beginnng of next record */
	if (fulp == FULL) 
		{
		if (fio->segbits < fio->maxrecsize)
			{
			int i;

			/* calc numb of bits to padd */
			i = fio->maxrecsize - fio->segbits;
			memset(BPTR2CP(fio->_ptr), 0, i >> 3);
			SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, i));
			fio->segbits += i;
			fio->recbits += i;
			fio->_cnt += i;
			}
/*
 *		If block is full, write it out.  Second check is for
 *		formats that allow padding, and MBS not a multiple
 *		of RS
 */
		if (fio->_cnt == fio->maxblksize || /* IBM.  Hit MBS */
			fio->_cnt + fio->maxrecsize > fio->maxblksize)
			{
			ret = put_block(fio, stat);
			if (ret < 0)
				{
				fio->recbits = fio->segbits;
				return(ret);
				}
			}

		fio->segbits = 0;
		fio->last_recbits = fio->recbits;
		fio->recbits = 0;
		SETSTAT(stat, FFEOR, nbits >> 3)
		}
	else	
		{
		SETSTAT(stat, FFCNT, nbits >> 3);
		}
	return (nbits >> 3);
	}

/*
 * Write out a block
 */
static
put_block(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
	{
	int zero = 0;
	int ret;

	if (((struct gen_ff *)fio->lyr_info)->padd == YES)
		{
		int i;

		/* calc numb of bits to padd */
		i = fio->maxblksize - fio->_cnt;
		if (i > 0)
			{
			memset(BPTR2CP(fio->_ptr),
				((struct gen_ff *)fio->lyr_info)->pchar,
				i >> 3);
			SET_BPTR(fio->_ptr, INC_BPTR(fio->_ptr, i));
			fio->_cnt += i;
			}
		}
	WRITEBLK(ret, fio, fio->_cnt >> 3, stat, FULL, &zero);
	return(ret);
	}

/*
 * Mark the EOD.
 */
int
_gen_fweod(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
	{
	struct fdinfo *llfio;
	int ret;

	/* Write after Read !! */
	if (fio->rwflag == READIN)
		{
		ERETURN(stat, FDC_ERR_WRARD, 0);
		}
	fio->rwflag = WRITIN;
	if (fio->_cnt != 0)
		if (put_block(fio, stat) < 0) return(ERR);
/*
 *	Everybody gets truncated here (we hope).
 */
	llfio = fio->fioptr;
	ret = XRCALL(llfio, weodrtn) llfio, stat);
	if (ret < 0)
		return(ERR);
	fio->ateod = 1;

	return(0);
	}
