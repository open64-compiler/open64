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


#pragma ident "@(#) libu/ffio/tmfpos.c	92.1	06/29/99 13:16:47"


#include "tmfio.h"

/*
 *	Tape positioning requests for Irix
 */

#define BMXLEN 13

_ffseek_t _tmf_pos(
	struct fdinfo	*fio,
	int		cmd,
	void		*arg,
	int		len,
	struct ffsw	*stat)
{
	int		abspos;
	int		locfcnt;
	int		resfcnt;
	int		usertm;
	struct tmfio	*xf_info;
	struct ffp_settp_s	*pos;
	struct ffp_skipf_s	*skp;
	struct ffp_skiptpmk_s	*tpmk;
	struct ffp_abs		*ffpabs;
	
	xf_info	= (struct tmfio *)fio->lyr_info;

	switch (cmd) {
		case FP_SKIPF:
/*JAS Test out what happens if we read a partial tape block */
			if (xf_info->tmf_speov) {
				ERETURN(stat, FDC_ERR_EOVALOW, 0);
			}
			skp	= (struct ffp_skipf_s *)arg;
			if (_tmf_skipf(fio, skp->ffp_nfil, &locfcnt, stat))
				return(ERR);
			skp->ffp_nfil	= locfcnt;
			skp->ffp_nrec	= 0;
			xf_info->rwflag	= POSITIN;
			break;

		case FP_SETTP:
/*JAS Test out what happens if we read a partial tape block */
			pos	= (struct ffp_settp_s *)arg;
			if (_tmf_stpos(fio, pos->ffp_nbs_p, pos->ffp_nb,
				pos->ffp_nvs_p, pos->ffp_nv, pos->ffp_vi,
				&usertm, stat))
				return(ERR);
			xf_info->rwflag	= POSITIN;
			break;

		case FP_SKIPTPMK:
/*JAS Test out what happens if we read a partial tape block */
			if (xf_info->tmf_speov) {
				ERETURN(stat, FDC_ERR_EOVALOW, 0);
			}
			tpmk	= (struct ffp_skiptpmk_s *)arg;
			/* Position to 0. This will cause us to wait for */
			/* any outstanding positioning, to flush any */
			/* unwritten data and if we're ever doing */
			/* read-ahead it will clear that out. */
			if (_tmf_stpos(fio, FP_TPOS_BACK, 0, 0, 0, 0, &usertm,
			    stat) < 0) {
				return(ERR);
			}
			if (_tmf_skiptpmk(xf_info->tmf_fd, tpmk->ffp_ntpmk, 
				&resfcnt, stat))
				return(ERR);
			tpmk->ffp_ntpmk	= resfcnt;
			xf_info->rwflag	= POSITIN;
			break;

		case FP_GETTAPEPOS:
			/* Get the tape position */
			{
			long long	pa[BMXLEN];
			struct ffp_tapepos *pos;

			pos	= (struct ffp_tapepos *)arg;
			if (_tmf_gtpos(fio, pa, BMXLEN, 1) < 0) {
				ERETURN(stat, errno, 0);
			}
			pos->ffp_type	= POSBYBLOCK;
			pos->ffp_vsn	= pa[0];
			/* Return the block number of the last block */
			/* processed. If we were reading, block number */
			/* is calculated from the physical block position */
			/* - (blocks in buffer memory + blocks in library) */
			/* If we were writing, block number is physical */
			/* block since we requested synch */
			if (xf_info->rwflag == WRITIN)
				pos->ffp_blockno	= pa[9]+1;
				
			else
				pos->ffp_blockno	= pa[9] - (pa[10] +
							  pa[11]) + 1;
			}
			break;

		case FP_SETTAPEPOS:
			/* Position to VSN, then position to block */
			{
			struct ffp_tapepos	*pos;

			pos	= (struct ffp_tapepos *)arg;
			if (pos->ffp_type != POSBYBLOCK)
				ERETURN(stat, FEBIOSNT, 0);

			if (_tmf_stpos(fio, FP_TPOS_ABS, pos->ffp_blockno, 0, 0,
			    pos->ffp_vsn, &usertm, stat) < 0)
				return(ERR);

			xf_info->rwflag	= POSITIN;
			}
			break;

		case FP_GABS:
/* JAS - we need to note in documentation that this will SYNC */
			/* Position to 0. This will cause us to wait for */
			/* any outstanding positioning, to flush any */
			/* unwritten data and if we're ever doing */
			/* read-ahead it will clear that out. */
			if (_tmf_stpos(fio, FP_TPOS_BACK, 0, 0, 0, 0, &usertm,
			    stat) < 0) {
				return(ERR);
			}
			if (_tmf_gabs(fio, &abspos) < 0)
				ERETURN(stat, errno, 0);
			ffpabs	= (struct ffp_abs *)arg;
			ffpabs->ffp_absaddr	= abspos;
			break;

		case FP_SABS:
			/* Position to 0. This will cause us to wait for */
			/* any outstanding positioning, to flush any */
			/* unwritten data and if we're ever doing */
			/* read-ahead it will clear that out. */
			if (_tmf_stpos(fio, FP_TPOS_BACK, 0, 0, 0, 0, &usertm,
			    stat) < 0) {
				return(ERR);
			}
			if (_tmf_sabs(fio, ((struct ffp_abs *)arg)->ffp_absaddr) < 0)
				ERETURN(stat, errno, 0);
			xf_info->rwflag	= POSITIN;
			break;

		default:
			ERETURN(stat, FDC_ERR_NOSUP, 0);
	}
	return(0);
}
