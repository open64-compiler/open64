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


#pragma ident "@(#) libu/ffio/mrgrow.c	92.1	06/29/99 13:16:47"

#include <stdio.h>
#include <malloc.h>
#include <ffio.h>
#include "fssio.h"

/*
 * _mr_grow()
 *	Grow the MR allocation to accomodate the new bit size.
 *	grow by at least mininc sectors, and no more than maxsize.
 */
int
_mr_grow(fio, reqinc, stat)
struct fdinfo *fio;
long reqinc;
struct ffsw *stat;
	{
	int retval;
	char *newbase;
	long bits, mrsizeb, newsizeb, maxinc;
	struct mr_f *mr_info;

	retval = 0;
	mr_info = (struct mr_f *)fio->lyr_info;
/*
 *	If increment is zero, don't bother.
 */
	if (reqinc <= 0) return(retval);
/*
 *	If we have already overflowed, don't even try
 */
	if (mr_info->overflowed == YES) return(1);

	newsizeb = BITS2BLKS((mr_info->mrsize + reqinc) + (512*64 -1));
	if (newsizeb > mr_info->maxsize)
		{
		retval = 1;
		newsizeb = mr_info->maxsize;
		}
/*
 *	Bump the MR allocation, but don't overflow due to min inc.
 */
	mrsizeb = BITS2BLKS(mr_info->mrsize);	/* always mult of blks, so OK */
	maxinc = mr_info->maxsize - mrsizeb;
	if (maxinc <=0) return(retval);

	if ((newsizeb - mrsizeb) < mr_info->mininc)
		{
		if (mr_info->mininc < maxinc)
			newsizeb = mrsizeb + mr_info->mininc;
		else
			newsizeb = mrsizeb + maxinc;
		}
	if (mrsizeb == 0)
		{
		if (mr_info->minsize > newsizeb) newsizeb = mr_info->minsize;
		newbase = malloc(newsizeb * 4096);
		}
	else
		newbase = realloc(BPTR2CP(fio->_base), newsizeb*4096);
/*
 *	If allocation fails, just overflow.
 */
	if (newbase == NULL)
		return(1);
/*
 *	Remember that even if this is the first allocation, the fio->_ptr
 *	can be set off the end of the file.  This is an offset from a
 *	NULL base pointer, but is still the file position, and must be
 *	preserved.
 */
	bits = SUBT_BPTR(fio->_ptr, fio->_base); /* old ptr offset */
	SET_BPTR(fio->_base, CPTR2BP(newbase));		/* update base */
	SET_BPTR(fio->_ptr, INC_BPTR(fio->_base, bits)); /* ptr = base + bits */
	mr_info->mrsize = BLKS2BITS(newsizeb);
	if (fio->rtype != TR_FSS_SAVE)
		mr_info->ovoff = mr_info->mrsize;
	fio->_ffbufsiz = BLKS2BITS(newsizeb);
	return(retval);
	}
