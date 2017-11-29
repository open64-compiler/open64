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


#pragma ident "@(#) libu/ffio/tmfclose.c	92.1	06/29/99 13:16:47"


#include "tmfio.h"
#include "fxlist.h"
#include <stdio.h>
#include <fcntl.h>
#include <malloc.h>

/*
 * Close a tape file.
 */
int
_tmf_close(struct fdinfo *fio, struct ffsw *stat)
{
	register int	ret;
	struct tmfio	*xfinfo;

	ret	= 0;
	SETSTAT(stat, 0, 0);
/*
 *	If the user closes while in eov processing, buffered memory is lost.
 */
	xfinfo	= (struct tmfio *)fio->lyr_info;

	if (xfinfo->tmf_tpos) {
		if (_tmf_tpwait (xfinfo) < 0) {
			ERETURN(stat, errno, 0);
		}
	}
/*
 *	If the file has been writing, flush and truncate after the last
 *	record written.
 */
	if (xfinfo->rwflag == WRITIN) {

		ret	= XRCALL(fio, flushrtn) fio, stat);
		if (ret != 0) return(ERR);

		ret	= XRCALL(fio, weodrtn) fio, stat);
		if (ret != 0) return(ERR);
	}

	ret	= close(xfinfo->tmf_fd);

	if (ret != 0) {
		SETSTAT(stat, errno, 0);
		return(ERR);
	}

	if (xfinfo->tmf_base != NULL)
		free(xfinfo->tmf_base);	/* free buffer */

	if (xfinfo->eovbase != NULL)
		free(xfinfo->eovbase);	/* free buffer */

	if (fio->lyr_info != NULL)
		free((char *)fio->lyr_info);	/* free private storage */

	return(0);
}
