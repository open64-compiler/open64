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


#pragma ident "@(#) libu/ffio/tmfgtpos.c	92.1	06/29/99 13:16:47"


#include "tmfio.h"
#include "sysio.h"

/*
 *	Get tape position
 *	Parameters:
 *		fio	- fdinfo ptr for this file
 *		pa	- an array of 64-bit words. Tape info is 
 *			  returned here.
 *		palen	- number of 64-bit words in pa
 *		synch	- unused now. Kept for compatibility? with Unicos
 *
 *	Returns: 0 if OK
 *		-1 if error (errno is set)
 */
int
_tmf_gtpos(struct fdinfo *fio, long long *pa, int palen, int synch)
{
	register short	partial;
	int		lib;
	struct tsdata	tsdata;
	struct tmfio	*xf_info;
	char 		vsn[MAXVSN][L_MAXVSN];

	xf_info	= (struct tmfio *)fio->lyr_info;

	if ( xf_info->tmf_tpos ) {
		if ( _tmf_tpwait( xf_info) < 0)
			return(ERR); 
	}

/*
 *	If previous i/o request was a write request make sure the list
 *	is flushed before getting the position.
 */
 
	partial	= 0;

	if ( xf_info->rwflag == WRITIN ) {
		if (xf_info->tmf_bufptr != xf_info->tmf_base){
			/* If we just wrote part of a record */
			/* don't try to flush to tape */
			partial	= 1;
		}
		else {
			/* This could cause us to hit eov */
			if (_tmf_flush(fio, stat) < 0)
				return(ERR);
		}
	}
		
	
	lib	= 0;

	if (xf_info->rwflag == READIN) {
/*
 *		If we were reading, count the blocks that are in the
 *		library's buffer.
 */
		/* JAS: Should we count partial blocks?? */
		
		/* Without read-ahead, there will be no blocks in */
		/* the library's buffer */
	}
	/* If we've seen eov, but we haven't started eov processing, */
	/* then count up how many blocks we have buffered */
	if (xf_info->tmf_eovhit && (xf_info->tmf_speov == 0)){
		/* Count blocks in the library buffer. */
		/* Without write behind, the buffered data is either */
		/* a single full block or a single partial block */
		partial	= xf_info->tmf_partblk;
		if (partial == 0 && xf_info->eovbytes != 0)
			lib	= 1;
	}
	if ( _tmf_tptsi (xf_info, &tsdata, vsn) != 0)
		return(ERR);

	__gtpos(&tsdata, vsn, lib, partial, pa, palen);

	return(0);
}

void
__gtpos(
	struct tsdata	*tsdata,
	char		vsn[][L_MAXVSN],
	int		lib,
	int		partial,
	long long	*pa,
	int		palen)
{
	register short	llsize;
	register short	i;

	if (palen == 0) goto done;

	/* VSN of last block processed */

	llsize	= sizeof(long long);
	*pa	= 0;

	(void) strncpy((char *)pa, vsn[tsdata->ts_cvsn], L_MAXVSN);

	pa	= pa + 1;

	if (--palen == 0) goto done;
/* 
 *	Pathname - pa(2) through pa(7)
 */
	for (i = 0; i <= (5 * llsize); i = i + llsize) {
		*pa	= 0;
		(void) strncpy((char *)pa, &tsdata->ts_path[i], llsize);
		pa	= pa + 1;
		if (--palen == 0) goto done;
	}
/* 
 *	File section number - pa(8)
 */
	*pa++	= tsdata->ts_fsec;
	if (--palen == 0) goto done;
/*
 *	File sequence number - pa(9)
 */
	*pa++	= tsdata->ts_fseq;
	if (--palen == 0) goto done;
/*
 *	Block number - pa(10)
 */
	*pa++	= tsdata->ts_bnum;
	if (--palen == 0) goto done;
/*
 *	Number of blocks in library buffer - pa(11)
 */
	*pa++	= lib;
	if (--palen == 0) goto done;
/*
 *	Number of blocks in IOP buffer - pa(12)
 *	0 on Irix.
 */
	*pa++	= 0;
	if (--palen == 0) goto done;
/*
 *	Device ID
 */
	*pa++	= tsdata->ts_ord;
	if (--palen == 0) goto done;
/*
 *	Device identifier
 */
	*pa	= 0;
	(void) strncpy((char *)pa, tsdata->ts_dvn, llsize);
	pa	= pa + 1;
	if (--palen == 0) goto done;
/*
 *	 Generic device name
 */
	*pa	= 0;
	(void) strncpy((char *)pa, tsdata->ts_dgn, llsize);
	pa	= pa + 1;
	if (--palen == 0) goto done;
/*
 *	Last device function
 */
	*pa++	= tsdata->ts_fcn;
	if (--palen == 0) goto done;
/*
 *	Last device status
 */
	*pa++	= tsdata->ts_dst;
	if (--palen == 0) goto done;
/*
 *	Data transfer count: not available on Irix
 */
	*pa++	= 0;
	if (--palen == 0) goto done;
/*
 *	Buffer memory sector count
 */
	*pa++	= 0;
	if (--palen == 0) goto done;
/*
 *	Partial block bytes in BM
 */
	*pa++	= 0;
	if (--palen == 0) goto done;
/*
 *	Outstanding sector count
 */
	*pa++	= 0;
	if (--palen == 0) goto done;
/*
 *	Outstanding block count: not available on Irix
 */
	*pa++	= 0;
	if (--palen == 0) goto done;
/*
 *	User tape mark number: not available on Irix
 */
	*pa++	= 0;
	if (--palen == 0) goto done;
/*
 *	Direction from tape mark
 */
	*pa++	= tsdata->ts_fmdir;
	if (--palen == 0) goto done;
/*
 *	Today's year
 */
	*pa++	= tsdata->ts_year;
	if (--palen == 0) goto done;
/*
 *	Today's day
 */
	*pa++	= tsdata->ts_day;
	if (--palen == 0) goto done;
/*
 *	File id - first 8 characters only
 */
	*pa	= 0;
	(void) strncpy((char *)pa, tsdata->ts_fid, llsize);
	pa	= pa + 1;
	if (--palen == 0) goto done;
/*
 *	Record format
 */
	*pa	= 0;
	(void) strncpy((char *)pa, tsdata->ts_rf, llsize);
	pa	= pa + 1;
	if (--palen == 0) goto done;
/*
 *	Density
 */
	*pa++	= tsdata->ts_den;
	if (--palen == 0) goto done;
/*
 *	Maximum block size
 *	Not valid on Irix.
 */
	*pa++	= 0;
	if (--palen == 0) goto done;
/*
 *	Record length
 */
	*pa++	= tsdata->ts_rl;
	if (--palen == 0) goto done;
/*
 *	File status
 */
	*pa++	= tsdata->ts_fst;
	if (--palen == 0) goto done;
/*
 *	Label type
 */
	*pa++	= tsdata->ts_lb;
	if (--palen == 0) goto done;
/*
 *	Fseq of first file
 */
	*pa++	= tsdata->ts_ffseq;
	if (--palen == 0) goto done;
/*
 *	Ring
 */
	*pa++	= tsdata->ts_ring;
	if (--palen == 0) goto done;
/*
 *	Expiration year
 */
	*pa++	= tsdata->ts_xyear;
	if (--palen == 0) goto done;
/*
 *	Expiration day
 */
	*pa++	= tsdata->ts_xday;
	if (--palen == 0) goto done;
/*
 *	First vsn offset of file
 */
	*pa++	= (long long) tsdata->ts_first;
	if (--palen == 0) goto done;
/*
 *	User eov selected
 */
	*pa++	= tsdata->ts_eov;
	if (--palen == 0) goto done;
/*
 *	User eov enabled
 */
	*pa++	= tsdata->ts_eovproc;
	if (--palen == 0) goto done;
/*
 *	User read/write tape mark
 */
	*pa++	= tsdata->ts_urwfm;
	if (--palen == 0) goto done;
/*
 *	Block attribute
 */
	*pa	= 0;
	(void) strncpy((char *)pa, tsdata->ts_ba, llsize);
	pa	= pa + 1;
	if (--palen == 0) goto done;
/*
 *	File id.
 */
	for (i = 0; i <= (6 * llsize); i = i + llsize) {
		(void) strncpy((char *)pa, &tsdata->ts_fid[i], llsize);
		pa	= pa + 1;
		if (--palen == 0) goto done;
	}
/*
 *	Number of partial blocks in library
 */
	*pa	= partial;

done:
	return;
}
