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


#pragma ident "@(#) libu/ffio/tmfstpos.c	92.1	06/29/99 13:16:47"


#include "tmfio.h"
#include "sysio.h"
#include <sys/mtio.h>

/*
 *	Tape positioning requests for Irix
 */

#define RETURN(x) { ret = x; goto done;}

int
_tmf_stpos(
	struct fdinfo	*fio,
	int		nbs,
	long		nb, 
	int		nvs,
	int		nv,
	long long	vi,
	int		*usertm,
	struct ffsw	*stat
)
{
	register short	lib = 0;
	register int	fd;
	register int	lib_tpmk = 0;
	register int	ret = 0;
	register long	nb_req;
	tmfpvsn_t	vsnctl;
	tmfpvol_t	volctl;
	struct tmfio	*xf_info;
	struct tsdata	tsi;
	long long	pa[11];	

	xf_info	= (struct tmfio *)fio->lyr_info;
	fd	= xf_info->tmf_fd;
	*usertm	= 0;

	/* Wait for outstanding positioning to complete if necessary */

	if (xf_info->tmf_tpos) {
		if (_tmf_tpwait(xf_info) < 0) {
			ERETURN(stat, errno, 0);
		}
	}

	/*
	 * If the file's been writing, flush out any
	 * unwritten data.
	 */
	if (xf_info->rwflag == WRITIN) {
		if (_tmf_flush(fio, stat) < 0) {
			return(ERR);
		}
	}
	/* Suppose we have been reading, and the last thing we */
	/* did was read a partial block. Then: */
	/* 1. if we position back a non-zero number of blocks, */
	/*    it is the same as if we had just done a full read*/
	/* 2. if we position forward a non-zero number of blocks, */
	/*	it is the same as if we had not read this block at all */
	/* 3. If we position back or forward 0 blocks, we are left */
	/*	at the beginning of the current partial block. This */
	/* 	is what Unicos did, so we keep doing it. */
	if (xf_info->rwflag == READIN && xf_info->tmf_cnt != 0) {
		if (nb != 0 && nbs == FP_TPOS_BACK) {
			lib	= 0;
		}
		else {
			lib	= 1;
		}
		/* get rid of what is in the library buffer */
		xf_info->tmf_cnt	= 0;
		xf_info->tmf_bufptr	= xf_info->tmf_base;
	}
	if (nb != 0) {
		if (nbs == FP_TPOS_BACK) {
			nb	= -nb;
		} else if (nbs == FP_TPOS_ABS) {
		   if ((nv == 0) && (vi == 0)) {
	
			/* Convert absolute block number to relative */
			/* block number from current position. First */
			/* find current position. */
			if (_tmf_gtpos(fio, pa, 10, 1) != 0)
				RETURN(ERR);
			nb	= nb - pa[9];	/* nb - ts_bnum */
			}
		   nb	= nb - 1;
		   lib	= 0;
		}
		nb_req	= nb - lib;
	} else {
		if ((nb == 0) && (vi == 0))
			nb_req	= nb - lib;
		else
			nb_req	= 0;
	}
	if (nv > 0) {
		/* Handle positioning by volume */
		if (nbs != FP_TPOS_ABS) {
			errno	= FETAPCMB;
			RETURN(ERR);
		}
		if (nvs != FP_TPOS_ABS) {
			if (_tmf_tptsi(xf_info,&tsi,NULL)) {
				RETURN(ERR);
			}
			if (nvs == FP_TPOS_BACK) {
				nv	= tsi.ts_cvsn - nv + 1;
				if (nv <= 0) {
					errno	= FETAPNVY;
					RETURN(ERR);
				}
			}
			else { /* must be FP_TPOS_FORW */
				nv	= tsi.ts_cvsn + nv + 1;
				if (nv > tsi.ts_numvsn) {
					errno	= FETAPNVY;
					RETURN(ERR);
				}
			}
		}
		(void) memset(&volctl, 0, sizeof(volctl));
		volctl.rh.request	= TR_PVOL;
		volctl.index		= nv;
		if (_tmf_tpvol (fd, &volctl )) {
			RETURN(ERR);
		}
		/* Position to the correct block */
		if ( nb_req != 0 ) {
			if (_tmf_tpblk(fd, nb_req, lib_tpmk, usertm)) {
				RETURN(ERR);
			}
		}

	} else if ( vi != 0 ) {

		(void) memset(&vsnctl, 0, sizeof(vsnctl));
		vsnctl.rh.request	= TR_PVSN;
		vsnctl.rh.length	= sizeof(tmfpvsn_t) -
					  sizeof(tmfreqhdr_t);
		(void) strncpy(vsnctl.evsn, (char *)&vi, L_MAXVSN);

		if (_tmf_tpvsn (fd, &vsnctl )) {
			RETURN(ERR);
		}

		/* Position to the correct block */

		if ( nb_req != 0 ) {
			if (_tmf_tpblk(fd, nb_req, lib_tpmk, usertm)) {
				RETURN(ERR);
			}
		}

	} else {
		if (_tmf_tpblk(fd, nb_req, lib_tpmk, usertm)) {
			RETURN(ERR);
		}
	}

	if (xf_info->tmf_speov) {
		/* We're in special eov processing. */
		/* If we backspaced, then we can just add this */
		/* to the number blocks on tape. */
		if (nb_req < 0)
			xf_info->spblocks	= xf_info->spblocks - nb_req;
		else {
			if (nb_req <= xf_info->spblocks)
				xf_info->spblocks	= xf_info->spblocks -
							  nb_req;
			else {
				xf_info->spblocks	= 0;
			}
		}
	}
done:
	xf_info->tmf_cnt	= 0;
	xf_info->tmf_bufptr	= xf_info->tmf_base;

	if (ret < 0) {
		ERETURN(stat, errno, 0);
	}
	return(ret);
}

/*
 * This routine is called when we've already initiated a
 * positioning request, but we haven't yet waited for the response
 * to see if it finished. It waits for the response.
 * The only positioning request like this currently is rewind.
 */
int 
_tmf_tpwait(
	struct tmfio *xf_info
)
{
	tmfrep_t	rep;
	tmfreqhdr_t	req;

waitresponse:
	(void) memset(&rep, 0, sizeof(tmfrep_t));
	if (ioctl(xf_info->tmf_fd, TMFC_DMNREP, &rep) < 0)
		return(ERR);
	if ((rep.reply == EINTR) && (xf_info->tmf_tpos == TPOS_REWD) &&
	    (INTIO == 0)) {
		(void) memset(&req, 0, sizeof(tmfreqhdr_t));
		req.request	= TR_RWD;
		req.async	= 1;
		if (ioctl(xf_info->tmf_fd, TMFC_DMNREQ, &req) < 0)
			return(ERR);
		goto waitresponse;
	}
	if (rep.reply != 0) {
		errno	= rep.reply;
		return(ERR);
	}
	
	xf_info->tmf_tpos	= 0;
	return(0);
}

int 
_tmf_tpvsn(
	int		fd,
	tmfpvsn_t	*ctl
)
{
	if (ioctl(fd, TMFC_DMNREQ, ctl) < 0) {
		return(ERR);
	}
	if (ctl->rh.reply != 0) {
		errno	= ctl->rh.reply;
		return(ERR);
	}
	return(0);
}

int 
_tmf_tpvol(
	int		fd,
	tmfpvol_t	*ctl
)
{
	if (ioctl(fd, TMFC_DMNREQ, ctl) < 0) {
		return(ERR);
	}
	if (ctl->rh.reply != 0) {
		errno	= ctl->rh.reply;
		return(ERR);
	}
	return(0);
}
/*
 * Change the tape position by nb blocks.
 * Parameters:
 *	fd:	file descriptor
 *	nb:	number of blocks to position
 *	lib_tpmk: number of library tape marks we need to position over
 *	usertm	: set to 1 if we position over a tape mark that was
 *		  not in the library's read ahead buffer
 */
int
_tmf_tpblk(
	int		fd,
	long		nb, 
	int		lib_tpmk,
	int		*usertm
)
{
	tmfpblk_t	ctl;

	if ((nb > 0) && (lib_tpmk > 0)) {
		/* Library read	-ahead has positioned past a */
		/* tape mark, but the user wants to position even */
		/* further ahead. */
		errno	= ETFMS;
		return(ERR);
	}

loop:
	(void) memset(&ctl, 0, sizeof(ctl));

	ctl.rh.request	= TR_PBLKS;
	ctl.rh.length	= sizeof(tmfpblk_t) - sizeof(tmfreqhdr_t);
	ctl.count	= nb;

	if (ioctl(fd, TMFC_DMNREQ, &ctl) < 0) {
		return(ERR);
	}

	if (ctl.rh.reply == ETFMS) {
		/* The tape subsystem will not position beyond a */
		/* tape mark. ctl.residual contains residual count. */
		if (ctl.rh.residual == 0) {
			if (lib_tpmk == 0) {
				/* The tape mark just positioned over */
				/* was not in the library's read ahead */
				*usertm	= 1;
			}
			return(0);
		}
		else if (lib_tpmk > 0) {
			/* Are we attempting to position over tape marks */
			/* due to the library's read ahead? */
			/* If so, keep going. If not, inform the user by */
			/* passing back an error. */
			lib_tpmk--;
			if (nb < 0) {
				nb	= -ctl.rh.residual;
				goto loop;
			} else {
				nb	= ctl.rh.residual;
				goto loop;
			}
		}
	}
	if (ctl.rh.reply != 0) {
		errno	= ctl.rh.reply;
		return(ERR);
	}
	return(0);
}

int
_tmf_gabs(struct fdinfo *fio, int *abspos)
{
	struct tmfio	*xf_info;
	struct mtget	mtget;

	xf_info	= (struct tmfio *)fio->lyr_info;

	if (ioctl(xf_info->tmf_fd, MTIOCGET, &mtget) < 0)
		return(ERR);

	*abspos	= mtget.mt_blkno;

	return(0);
}

int
_tmf_sabs(struct fdinfo *fio, int abspos)
{
	tmfpabs_t	posreq;
	struct tmfio	*xfinfo;

	xfinfo	= (struct tmfio *)fio->lyr_info;

	(void) memset(&posreq, 0, sizeof(posreq));

	posreq.rh.request	= TR_PABS;
	posreq.rh.length	= sizeof(tmfpabs_t) - sizeof(tmfreqhdr_t);
	posreq.blkaddr		= abspos;

	if (ioctl(xfinfo->tmf_fd, TMFC_DMNREQ , &posreq) < 0) {
		return(ERR);
	}

	if (posreq.rh.reply != 0) {
		errno	 = posreq.rh.reply;
		return(ERR);
	}

	return(0);
}
