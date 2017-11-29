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


#pragma ident "@(#) libu/ffio/tmfbksp.c	92.1	06/29/99 13:16:47"
 
#include "tmfio.h"

/*
 * backspace a tape file
 * Parameters:
 *	fd	- file descriptor (dummy)
 *	stat	- pointer to status return word
 *
 * Returns:
 *	0 on success 
 *	-1 on error with sw_error containing error code
 *	The sw_stat field is set only on error. Note that
 *	layers are not consistent about setting this after
 *	positioning. COS layer will set sw_stat to FFEOF if
 *	we just backspaced over an end of file. bmx layer doesn't
 *	do anything with it.
 */

int
_tmf_bksp(struct fdinfo *fio, struct ffsw *stat)
{
	struct tmfio	*xf_info;
	register int	ret;
	int		usertm;

	xf_info	= (struct tmfio *)fio->lyr_info;

	if (xf_info->tmf_tpos) {
		if (_tmf_tpwait (xf_info) < 0) {
			ERETURN(stat, errno, 0);
		}
	}
/*
 *	If the file's been writing, flush out any unwritten data.
 */
	if (xf_info->rwflag == WRITIN) {
		if (_tmf_flush(fio, stat) < 0) {
			return(ERR);
		}
	}
/*
 *	Now backspace.
 */	
	ret	= _tmf_stpos(fio, FP_TPOS_BACK, 1, 0, 0, 0, &usertm, stat);

	if (ret < 0 && stat->sw_error != ETBOF)
		return(ERR);

	xf_info->rwflag	= POSITIN;

	return(0);
}
