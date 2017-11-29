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


#pragma ident "@(#) libu/ffio/trcclose.c	92.1	06/29/99 13:16:47"

#include <stdio.h>
#include <malloc.h>
#include <ffio.h>
#include "trcio.h"

/*
 *	trace close requests
 */

_trc_close(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
	{
	struct fdinfo *llfio;
	struct trace_f *trc_info;
	int ret;

	llfio = fio->fioptr;
	trc_info = (struct trace_f *)fio->lyr_info;

/*
 *	close file
 */
	_trc_enter(fio, TRC_CLOSE);
	_trc_pr_2p(fio, stat);

	ret = XRCALL(llfio, closertn) llfio, stat);

	free(trc_info->name);
	trc_info->name = NULL;
	free(trc_info->hist);
	trc_info->hist = NULL;
	free(trc_info);

	_trc_stats(trc_info);
	_trc_exit(fio, ret, stat);
	(void)close(trc_info->trcfd);
	free((char *)llfio);

	return(0);
}
