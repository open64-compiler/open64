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


#pragma ident "@(#) libu/ffio/tmfskipf.c	92.1	06/29/99 13:16:47"


#include "tmfio.h"

/*
 *	Tape positioning requests for Irix
 */

/*
 * Change the tape position by ntpmk tape marks 
 * Note that this routine does not set stat.sw_stat unless
 * there is an error. We could probably set it to EOD and BOD,
 * but setting it to EOF is ???
 * Skips the specified number of tape marks. If skipping forward,
 * it will position you directly after a user tape mark or at EOD.
 * If skipping backwards, it will position you directly before a
 * user tape mark or at BOD
 */
int
_tmf_skiptpmk(
	int		fd,
	int		ntpmk,
	int		*rescnt,
	struct ffsw	*stat)
{
	tmfpfm_t	ctl;

	*rescnt		= 0;	/* residual count */
	bzero(&ctl, sizeof(ctl));
	ctl.rh.request	= TR_PFMS;
	ctl.rh.length	= sizeof(tmfpfm_t) - sizeof(tmfreqhdr_t);
	ctl.count	= ntpmk;

	if (ioctl(fd, TMFC_DMNREQ, &ctl) < 0) {
		ERETURN(stat, errno, 0);
	}

	*rescnt	= ctl.rh.residual;

	if (ctl.rh.reply != 0) {
		ERETURN(stat, ctl.rh.reply, 0);
	}
		
	return(0);
}

/*
 * Skip file.
 * Note that this has slightly different meaning that skip tape mark.
 *	Except in the case where you request positioning past EOD,
 *	_tmp_skipf will position you at the beginning of a file. That is,
 *	_tmf_skipf will position you either at BOD, immediately after a
 *	user tape mark, or at EOD.
 */
int
_tmf_skipf(
	struct fdinfo	*fio,
	long		nb,
	int		*count,
	struct ffsw	*stat)
{
	int		rescnt;
	int		usertm;
	struct tmfio	*xf_info;

	xf_info	= (struct tmfio *)fio->lyr_info;
	*count	= 0;

	if (nb == 0) {
		/* Position to 0. This will cause us to wait for */
		/* any outstanding positioning, to flush any unwritten data, */
		/* and if we're ever doing */
		/* read-ahead it will clear that out. */

		if (_tmf_stpos(fio, FP_TPOS_BACK, 0, 0, 0, 0, &usertm, stat) < 0) {
				return(ERR);
		}
		return(0);
	}
	else if (nb > 0) {
		/* Positioning forward nb files.*/
		/* First position to 0. This will cause us to wait for */
		/* any outstanding positioning, to flush any unwritten data, */
		/* and if we're ever doing read-ahead it will */
		/* clear that out. */
		if (_tmf_stpos(fio, FP_TPOS_BACK, 0, 0, 0, 0, &usertm, stat) < 0) {
				return(ERR);
		}
		if (xf_info->tmf_rwtpmk == 0) {
			/* No user tape marks */
			ERETURN(stat, FETASKPF, 0);
		}
		if (_tmf_skiptpmk(xf_info->tmf_fd, nb, &rescnt, stat) < 0) {
			if (stat->sw_error != ETEOF) {
				return(ERR);
			}
		}
		*count	= nb - rescnt;
	}
	else {
		/* Positioning back nb files */
		/* First position back 1 record. If we were */
		/* immediately after a tapemark, then this */
		/* will put us in the previous file. */
		if (_tmf_stpos(fio, FP_TPOS_BACK, 1, 0, 0, 0, &usertm, stat) < 0) {
			if (stat->sw_error == ETBOF) {
				return(0);
			}
			return(ERR);
		}
		if (xf_info->tmf_rwtpmk == 0) {
			/* No user tape marks, just rewind */
			if (_tmf_seek(fio,0,0,stat) < 0) {
				*count	= -nb;
				return(ERR);
			}
			*count	= 0;
			return(0);
		}
		/* Now position back nb files */
		if (_tmf_skiptpmk(xf_info->tmf_fd, nb, &rescnt, stat) == 0) {
			/* Move to the other side of this tape mark */
			int	ut;
			if (_tmf_stpos(fio, FP_TPOS_FORW, 1, 0, 0, 0, &ut, stat) < 0) {
				return(ERR);
			}
		}
		else {
			if (stat->sw_error != ETBOF) {
				return(ERR);
			}
		}

		*count	= -nb - rescnt;
		if (usertm == 0) {
			/* The user was positioned in the middle of a file. */
			if (*count > 0)
				*count	= *count - 1;
		}

	}
	return(0);
}
