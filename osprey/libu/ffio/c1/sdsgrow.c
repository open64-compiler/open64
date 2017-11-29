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


static char USMID[] = "@(#) libu/ffio/c1/sdsgrow.c	92.0	10/08/98 14:57:41";


#include <stdio.h>
#include <ffio.h>
#include "../fssio.h"

/*
 * _sds_grow()
 *	Grow the SDS allocation to accomodate the new bit size.
 *	grow by at least mininc sectors, andno more than maxsize.
 */
int
_sds_grow(fio, reqinc, stat)
struct fdinfo *fio;
long reqinc;
struct ffsw *stat;
	{
	int ret;
	long newsize, maxinc;
	long sdssizeb;			/* sds size in blocks */
	long newsizeb;			/* new sds size in blocks */
	int retval;
	struct sds_f *sds_info;

	retval = 0;
	sds_info = (struct sds_f *)fio->lyr_info;
/*
 *	If increment is zero, don't bother....
 */
	if (reqinc <= 0) return (retval);
/*
 *	If we have already overflowed, don't even try.
 */
	if (sds_info->overflowed == YES) return(1);

	newsize = sds_info->sdssize + reqinc;
	newsizeb = BITS2BLKS(newsize + (512*64 - 1));
/*
 *	Bump the SDS allocation, but don't overflow due to min inc.
 */
	sdssizeb = BITS2BLKS(sds_info->sdssize); /* sdssize is *always* even blks */
	maxinc = sds_info->maxsize - sdssizeb;
/*
 *	If request will put us over max, set overflow flag,
 *	and trim the request.
 */
	if (newsizeb > sds_info->maxsize)
		{
		retval = 1;
		newsizeb = sds_info->maxsize;
		}
	if (maxinc <= 0) return(retval);
/*
 *	Ask for at least the min increment, unless that is too much.
 */
	if ((newsizeb - sdssizeb) < sds_info->mininc)
		{
		if (sds_info->mininc < maxinc)
			newsizeb = sdssizeb + sds_info->mininc;
		else
			newsizeb = sdssizeb + maxinc;
		}
/*
 *	If this is the first allocation, get the *initial* size requested.
 */
	if (sdssizeb == 0)
		{
		if (sds_info->minsize > newsizeb) newsizeb = sds_info->minsize;
		}
	ret = addsdsspc(sds_info, ROUNDD60(newsizeb - sdssizeb, sds_info));
/*
 *	If ret < 0, some awful error ocurred, like no more memory.
 */
	if (ret < 0)
		ERETURN(stat, FDC_ERR_NOMEM, 0);
/*
 *	If the allocation or reallocation yields less than requested,
 *	just overflow.
 */
	if (BLKS2BITS(ret) < reqinc)
		retval = 1;

	sds_info->sdssize += BLKS2BITS(ret);	/* in bits */
	if (fio->rtype != TR_FSS_SAVE)
		sds_info->ovoff = sds_info->sdssize;
	return(retval);
	}

/*
 * addsdsspc(info, blks): add space to the list of allocated areas in SDS.
 *
 *	Returns: number of blocks obtained, which may be <= that requested.
 *	Returns ERR (-1) ONLY if malloc/realloc fails.
 */
addsdsspc(sds_info, blks)
struct sds_f *sds_info;
int blks;
	{
	int ret;
	int ct;
	int last;
	int got;
	int oldsize;
	int alo, max, req, newsize;
	int posreq;
	struct sds_alo_s *alop;

	ct = 0;
	alop = sds_info->sdsalo;
	if (sds_info->sdsalnum == 0)
		{
/*
 *		Allocate space for the list of descriptors
 */
		alop = (struct sds_alo_s *)
			malloc(sizeof(struct sds_alo_s) * ALOPINC);
		if (alop == NULL) return(ERR);
		sds_info->sdsalo = alop;
		sds_info->sdsalospc = ALOPINC;

		ret = sdsalloc(blks);
		alop[0].base = ret;
		alop[0].relbase = 0;
		sds_info->sdsalnum = 1;
		if (ret >= 0)		/* zero is valid address! */
			{
/*
 *			Got what we wanted, return
 */
			alop[0].size = blks;
			return(blks);
			}
		else
			{
/*
 *			Could not get request, try for one block.
 */
			ret = sdsalloc(sds_info->sdsklik);
			if (ret < 0)
				{
				free(alop);
				sds_info->sdsalnum = 0;
				sds_info->sdsalospc = 0;
				sds_info->sdsalo = NULL;
				return(0);
				}
			alop[0].base = ret;
			alop[0].size = sds_info->sdsklik;
			ct += sds_info->sdsklik;
			}
		}
/*
 *	First, try to extend the block on the end.
 */
	last = sds_info->sdsalnum - 1;
	oldsize = alop[last].size;
	req = oldsize + (blks - ct);
/*
 *	Don't ask for more than we know we can get
 *	Round down to nearest sds_info->sdsklik boundary
 */
	posreq = sdsmaxsize(alop[last].base);
	if (posreq < req) req = DROUNDD60(posreq, sds_info);
/*
 *	Resize the block to the max
 */
	if (req > alop[last].size)
		{
		newsize = sdsresize(alop[last].base, req);
		if (newsize > oldsize)
			{
			alop[last].size = newsize;
/*
 *			If we got enough, return number added.
 */
			ct += newsize - oldsize;
			}
		}
/*
 *	If we can't resize a current block, scavenge up anything that
 *	might be available.
 */
	while (ct < blks)
		{
		newsize = blks - ct;
		got = newsize;
		alo = sdsalloc(newsize);
		SANITYCHK(newsize,NE,ROUNDD60(newsize, sds_info));
		if (alo < 0)
			{
/*
 *			Try to get a smaller chunk
 */
			alo = sdsalloc(sds_info->sdsklik);
			if (alo < 0) return(ct);	/* no more space */
			max = sdsmaxsize(alo);
			newsize = blks - ct;
			if (max < newsize)
				newsize = DROUNDD60(max, sds_info);
			got = newsize;
			ret = sdsresize(alo, newsize);
			if (ret < 0)
				{
				sdsfree(alo);
				/* resize must have lied.  Quit now. */
				return(ct);
				}
			newsize = ret;
			}
/*
 *		At this point, either the sdsalloc, or the resize has
 *		managed to get some space.
 */
		if (alo >= 0)
			{
			last++;
			if (last >= sds_info->sdsalospc)
				{
				alop = (struct sds_alo_s *)realloc(alop,
					(last+ALOPINC)*sizeof(*alop));
				if (alop == NULL) return(ERR);
				sds_info->sdsalo = alop;
				sds_info->sdsalospc = last+ALOPINC;
				}
			alop[last].relbase = alop[last-1].relbase +
						alop[last-1].size;
			alop[last].base = alo;
			alop[last].size = newsize;
			sds_info->sdsalnum = last+1;
			ct += got;
			}
		else
			return(ct);
		}
	return(ct);
	}
