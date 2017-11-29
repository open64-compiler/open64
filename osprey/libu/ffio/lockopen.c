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


#pragma ident "@(#) libu/ffio/lockopen.c	92.3	10/29/99 21:40:31"


#include <ffio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include "fxlist.h"
#include "locklyr.h"

DECLARE(LOCK_XLIST);
struct xtr_s LOCK_XLIST_P = { LOCK_XLIST };

/*
 * Open the lock layer. This open routine is called directly from _ffopen.
 * It is not called with an XRCALL. The user cannot specify the lock layer.
 */

struct fdinfo *
_open_lock_lyr (struct fdinfo *fio, plock_t *lockptr)
{
	struct fdinfo *nfio;
	extern struct xtr_s *_recfm_tab[NUM_CLASSES];

	nfio = (struct fdinfo *)calloc(1,sizeof(*fio));
	if (nfio == NULL) 
		{
		errno = FDC_ERR_NOMEM;
		return(NULL);
		}
	memset(nfio, 0, sizeof(*fio));
	nfio->magic 	= MAGIC_ID;
	nfio->class 	= CLASS_LOCK;
	nfio->opn_flags	= fio->opn_flags;
	nfio->xr	= *_recfm_tab[CLASS_LOCK];
		
	/* Leave the nfio->lock field = 0. This layer does not need to have */
	/* a locking layer on top of it. */
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
	nfio->can_listio = 1;	/* the lock layer can listio */
#endif
	nfio->fioptr = fio;
	nfio->lyr_info = (void *)lockptr;
	/* Do not set up fio->parptr yet. That is done in the calling routine */
	return(nfio);
}
