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


static char USMID[] = "@(#) libu/ffio/c1/tapegtpos.c	92.0	10/08/98 14:57:41";


#include  <sys/types.h>
#include  <sys/bmxctl.h>
#include  <sys/file.h>
#include  <sys/iosw.h>
#include  <sys/stat.h>
#include  <sys/jtab.h>
#include  <sys/param.h>
#include  <fcntl.h>
#include  <tapereq.h>
#include  <stdio.h>
#include  <malloc.h>
#include  <errno.h>
#include  <liberrno.h>
#include  <ffio.h>

/*
 *	Get tape position. Works for er90 or bmx devices.
 */

void
_tape_gtpos(struct tsdata *tsdata, int vsn[], int lib, long *pa, long palen)
{
	int i;

	if ( palen == 0 ) goto done;
/*
 *	VSN of last block processed - pa(1)
 */
	*pa = vsn[tsdata->ts_cvsn];
	pa++;
	palen--;
	if ( palen == 0 ) goto done;
/*
 *	Pathname - pa(2) through pa(7)
 */
	for (i = 0; i <= 40; i+=8){
		strncpy((char *)pa, &tsdata->ts_path[i], NBPW);
		pa++;
		palen--;
		if ( palen == 0 ) goto done;
	}

/*
 *	File section number - pa(8)
 */
	*pa = tsdata->ts_fsec;
	pa++;
	palen--;
	if ( palen == 0 ) goto done;
/*
 *	File sequence number - pa(9)
 */
	*pa = tsdata->ts_fseq;
	pa++;
	palen--;
	if ( palen == 0 ) goto done;
/*
 *	Block number - pa(10)
 */
	*pa = tsdata->ts_bnum;
	pa++;
	palen--;
	if ( palen == 0 ) goto done;
/*
 *	Number of blocks in library buffer - pa(11)
 */
	*pa = lib;
	pa++;
	palen--;
	if ( palen == 0 ) goto done;
/*
 *	Number of blocks in IOP buffer - pa(12)
 */
	*pa = tsdata->ts_bmblk;
	pa++;
	palen--;
	if ( palen == 0 ) goto done;
/*
 *	Device ID
 */
	*pa = tsdata->ts_ord;
	pa++;
	palen--;
	if ( palen == 0 ) goto done;
/*
 *	Device identifier
 */
	*pa = 0;
	strncpy((char *)pa, tsdata->ts_dvn, NBPW);
	pa++;
	palen--;
	if ( palen == 0 ) goto done;
/*
 *	Generic device name
 */
	*pa = 0;
	strncpy((char *)pa, tsdata->ts_dgn, NBPW);
	pa++;
	palen--;
	if ( palen == 0 ) goto done;
/*
 *	Last device function
 */
	*pa = tsdata->ts_fcn;
	pa++;
	palen--;
	if ( palen == 0 ) goto done;
/*
 *	Last device status
 */
	*pa = tsdata->ts_dst;
	pa++;
	palen--;
	if ( palen == 0 ) goto done;
/*
 *	Data transfer count
 */
	*pa = tsdata->ts_dtr;
	pa++;
	palen--;
	if ( palen == 0 ) goto done;
/*
 *	Buffer memory sector count
 */
	*pa = tsdata->ts_bmsec;
	pa++;
	palen--;
	if ( palen == 0 ) goto done;
/*
 *	Partial block bytes in BM
 */
	*pa = tsdata->ts_pbmcnt;
	pa++;
	palen--;
	if ( palen == 0 ) goto done;
/*
 *	Outstanding sector count
 */
	*pa = tsdata->ts_orsc;
	pa++;
	palen--;
	if ( palen == 0 ) goto done;
/*
 *	Outstanding block count
 */
	*pa = tsdata->ts_orbc;
	pa++;
	palen--;
	if ( palen == 0 ) goto done;
/*
 *	User tape mark number
 */
	*pa = tsdata->ts_utmnum;
	pa++;
	palen--;
	if ( palen == 0 ) goto done;
/*
 *	Direction from tape mark
 */
	*pa = tsdata->ts_tmdir;
	pa++;
	palen--;
	if ( palen == 0 ) goto done;
/*
 *	Today's year
 */
	*pa = tsdata->ts_year;
	pa++;
	palen--;
	if ( palen == 0 ) goto done;
/*
 *	Today's day
 */
	*pa = tsdata->ts_day;
	pa++;
	palen--;
	if ( palen == 0 ) goto done;
/*
 *	File id - first 8 characters only
 */
	*pa = 0;
	strncpy((char *)pa, tsdata->ts_fid, NBPW);
	pa++;
	palen--;
	if ( palen == 0 ) goto done;
/*
 *	Record format
 */
	*pa = 0;
	strncpy((char *)pa, tsdata->ts_rf, NBPW);
	pa++;
	palen--;
	if ( palen == 0 ) goto done;
/*
 *	Density
 */
	*pa = tsdata->ts_den;
	pa++;
	palen--;
	if ( palen == 0 ) goto done;
/*
 *	Maximum block size
 */
	*pa = tsdata->ts_mbs;
	pa++;
	palen--;
	if ( palen == 0 ) goto done;
/*
 *	Record length
 */
	*pa = tsdata->ts_rl;
	pa++;
	palen--;
	if ( palen == 0 ) goto done;
/*
 *	File status
 */
	*pa = tsdata->ts_fst;
	pa++;
	palen--;
	if ( palen == 0 ) goto done;
/*
 *	Label type
 */
	*pa = tsdata->ts_lb;
	pa++;
	palen--;
	if ( palen == 0 ) goto done;
/*
 *	Fseq of first file
 */
	*pa = tsdata->ts_ffseq;
	pa++;
	palen--;
	if ( palen == 0 ) goto done;
/*
 *	Ring
 */
	*pa = tsdata->ts_ring;
	pa++;
	palen--;
	if ( palen == 0 ) goto done;
/*
 *	Expiration year
 */
	*pa = tsdata->ts_xyear;
	pa++;
	palen--;
	if ( palen == 0 ) goto done;
/*
 *	Expiration day
 */
	*pa = tsdata->ts_xday;
	pa++;
	palen--;
	if ( palen == 0 ) goto done;
/*
 *	First vsn of file
 */
	*pa = tsdata->ts_first;
	pa++;
	palen--;
	if ( palen == 0 ) goto done;
/*
 *	User eov selected
 */
	*pa = tsdata->ts_eov;
	pa++;
	palen--;
	if ( palen == 0 ) goto done;
/*
 *	User eov enabled
 */
	*pa = tsdata->ts_eovproc;
	pa++;
	palen--;
	if ( palen == 0 ) goto done;
/*
 *	User read/write tape mark
 */
	*pa = tsdata->ts_urwtm;
	pa++;
	palen--;
	if ( palen == 0 ) goto done;

/*
 *	Block attribute
 */
	strncpy((char *)pa, tsdata->ts_ba, NBPW);
	pa++;
	palen--;
	if ( palen == 0 ) goto done;

/*
 *	File id.
 */
	for (i = 0; i <= 48; i+=8){
		strncpy((char *)pa, &tsdata->ts_fid[i], NBPW);
		pa++;
		palen--;
		if ( palen == 0 ) goto done;
	}
done:
	return;

}
