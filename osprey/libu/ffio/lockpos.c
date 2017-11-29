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


#pragma ident "@(#) libu/ffio/lockpos.c	92.1	06/29/99 13:16:47"


#include <ffio.h>
#include "locklyr.h"


_ffseek_t
_lock_pos(struct fdinfo *fio, int cmd, void *arg, int len, struct ffsw *stat)
{
	struct fdinfo *llfio;
	_ffseek_t ret;

	llfio = fio->fioptr;

	LYR_LOCK(fio);
	ret = XRCALL(llfio, posrtn) llfio, cmd, arg, len, stat);
	LYR_UNLOCK(fio);
	return(ret);
}

int
_lock_bksp(struct fdinfo *fio, struct ffsw *stat)
{
	struct fdinfo *llfio;
	int ret;

	llfio = fio->fioptr;
	LYR_LOCK(fio);
	ret = XRCALL(llfio, backrtn) llfio, stat);
	LYR_UNLOCK(fio);
	return(ret);
}
