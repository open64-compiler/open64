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


#pragma ident "@(#) libu/ffio/tmffcntl.c	92.1	06/29/99 13:16:47"

#include "tmfio.h"

/*
 * (IRIX) tape fcntl requests
 *
 * Parameters:
 *	fd	- file descriptor (dummy)
 *	cmd	- command code
 *	arg	- command specific parameter
 *	stat	- pointer to status return word
 */
int
_tmf_fcntl(struct fdinfo *fio, int cmd, void *arg, struct ffsw *stat)
{
	register int	iflag;
	register int	ret;
	struct ffc_info_s	*ffcp;
	struct tmfio		*xf_info;
	struct ffc_chkev_s	*chktpptr;
	struct ffc_gettp_s	*gtptr;

	xf_info	= (struct tmfio *)fio->lyr_info;
	ret	= 0;

	switch (cmd) {
		case FC_GETINFO:
			ffcp	= (struct ffc_info_s *)arg;
			ffcp->ffc_flags	= 
				FFC_REC |	/* records */
				FFC_WEOD |	/* can WEOD */

				FFC_BKSP |	/* can backspace */
				FFC_BKFIL |	/* can do backfile */
				FFC_POSREC |	/* can position by record no */
				FFC_POSFIL |	/* can position by file no */
				FFC_RWND |	/* can rewind */

				FFC_VAR |	/* can do variable len recs */
				FFC_BINARY |	/* can do binary */
				FFC_CODED |	/* can do chars */

				FFC_SEQ |	/* can do seq */
				FFC_WRTRUNC |	/* Write implies trunc */
				FFC_NOTRN |	/* Does no transformation */
				0;

			if (xf_info->tmf_rwtpmk)	/* Can write tp marks */
				ffcp->ffc_flags |= FFC_WEOF;
			ffcp->ffc_gran		= 8;	/* byte granularity */
			ffcp->ffc_reclen	= 0;	/* no rec length */
			ffcp->ffc_fd		= xf_info->tmf_fd; /* fd */
			break;

		case FC_STAT:
			ret	= fstat(xf_info->tmf_fd, arg);
			if (ret < 0)
				stat->sw_error	= errno;
			break;

		case FC_ASPOLL:
		case FC_RECALL:
			break;

		case FC_CHECKEOV:
			/* check for end of volume */
			chktpptr	= (struct ffc_chkev_s *)arg;
			if (_tmf_checkev(xf_info, chktpptr) != 0) {
				ERETURN(stat, errno, 0);
			}
			break;

		case FC_ENDSP:
			if (_tmf_endsp(fio, stat)) {
				return(ERR);
			}
			break;

		case FC_STARTSP:
			if (_tmf_startsp(xf_info)) {
				ERETURN(stat, errno, 0);
			}
			break;

		case FC_SETSP:
/* JAS - Note that arg is treated differently than the Fortran fnc expects */
/* Is this the way we want to treat arg?? */
/* On Unicos, we had a function ffsetsp, do we need that here? */
			iflag	= *(int *)arg;	
			if (_tmf_setsp(xf_info, iflag)) {
				ERETURN(stat, errno, 0);
			}
			break;

		case FC_TSYNC:
			if (_tmf_gtpos(fio, NULL, 0, 1)) {
				ERETURN(stat, errno, 0);
			}
			break;

		case FC_GETTP:
			/* get tape info */
			gtptr	= (struct ffc_gettp_s *)arg;
			if (_tmf_gtpos(fio, gtptr->ffc_pa, gtptr->ffc_glen,
				gtptr->ffc_synch) != 0)
				ERETURN(stat, errno, 0);
			break;

		case FC_CLOSEV:
			if (_tmf_closev(fio, stat) != 0)
				return(ERR);
			break;

		case FC_TSDATA:
			/* return the tsdata structure */
			/* This does not flush data or return info */
			/* about what might be in the library's buffers */
			if (xf_info->tmf_tpos) {
				if (_tmf_tpwait(xf_info) < 0)
					ERETURN(stat, errno, 0);
			}
			if (_tmf_tptsi(xf_info,(struct tsdata *)arg, NULL) < 0) {
				ERETURN(stat, errno, 0);
			}
			break;

		case FC_SKIPBAD:
		case FC_ACPTBAD:
		default:
			ERETURN(stat, FDC_ERR_NOSUP, 0);
	}

	return(ret);
}
