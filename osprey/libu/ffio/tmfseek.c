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


#pragma ident "@(#) libu/ffio/tmfseek.c	92.1	06/29/99 13:16:47"


#include "tmfio.h"
#include <stdlib.h>
#include <sys/types.h>

/*
 * The only type of "seek" we allow for a tape file is rewind.
 */
_ffseek_t
_tmf_seek(
	struct fdinfo	*fio,
	off_t		pos,
	int		whence,
	struct ffsw	*stat	
)
{
	tmfreqhdr_t	ctl;
	struct tmfio	*xfinfo;

	xfinfo	= (struct tmfio *)fio->lyr_info;

	if (xfinfo->tmf_tpos) {
		if (_tmf_tpwait(xfinfo) < 0) {
			ERETURN(stat, errno, 0);
		}
	}
/*
 *	If the file's been writing, flush out data.
 *	If we've been reading, clear out any unread data.
 */
	if (_tmf_flush(fio, stat) < 0) {
		return(ERR);
	}

	switch (whence) {
		case SEEK_SET:
			
			if (pos != 0) {
				ERETURN(stat, FDC_ERR_NOSUP, 0);
			}
			else {
				/* rewind */
				bzero(&ctl, sizeof(ctl));
				ctl.request	= TR_RWD;
				/* Set async. This means that this */
				/* call may return before the tape is rewound */
				ctl.async	= 1;
				if (ioctl(xfinfo->tmf_fd, TMFC_DMNREQ, &ctl) < 0) {
					ERETURN(stat, errno, 0);
				}
				/* The next time we do something to this tape */
				/* we need to verify that the rewind finished */
				xfinfo->tmf_tpos	= TPOS_REWD;
			}
			break;

		default:
			ERETURN(stat, FDC_ERR_NOSUP, 0);
	}

	xfinfo->rwflag	= POSITIN;

	return(0);	/* we can't return a byte position */
}
