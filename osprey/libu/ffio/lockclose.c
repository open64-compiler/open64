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


#pragma ident "@(#) libu/ffio/lockclose.c	92.1	06/29/99 13:16:47"

#include <ffio.h>
#include <stdlib.h>
#include <cray/nassert.h>
#include "locklyr.h"


_lock_close(struct fdinfo *fio, struct ffsw *stat)
{
	struct fdinfo *llfio;
	int ret;

	llfio = fio->fioptr;

	LYR_LOCK(fio);
	ret = XRCALL(llfio, closertn) llfio, stat);
	/* If the user called _locklyr_unlock, then don't unlock here */
	COND_LYR_UNLOCK(fio);
	if (fio->free_lock)
		free(fio->lyr_info);

	free((char *)llfio);
	return(ret);
}
/*
 * A special routine that allows a layer that is currently in its
 * close routine to unlock its lock word. It would want to do this
 * if it needs to free the lock word.
 */
void _locklyr_unlock(struct fdinfo *fio)
{
	struct fdinfo *parfio;
	parfio = fio->parptr;
	assert(parfio->class == CLASS_LOCK);
	LYR_UNLOCK(parfio);
	parfio->lyr_info = NULL;

}
