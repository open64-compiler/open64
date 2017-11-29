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


#pragma ident "@(#) libu/ffio/cosclose.c	92.1	06/29/99 13:16:47"

#include <ffio.h>
#include <stdlib.h>
#include "cosio.h"



/*
 *	Close a COS blocked sequential file
 *      Returns: -1 if error occurred
 *		  0 if OK
 */

int
_cos_close(struct fdinfo *fio, struct ffsw *stat)
	{
	struct fdinfo *llfio;
	int ret;
	struct cos_f *cos_info;

	cos_info = (struct cos_f *)fio->lyr_info;
	llfio = fio->fioptr;

	cos_info->cos_flag |= COS_IOCLOSE;	
	if (cos_info->cos_flag & COS_IOWRITE) {	/* if just did a write	*/
		/*  ensure proper EOD	*/
		if (XRCALL(fio, weodrtn) fio, stat) < 0)
			goto badret;
	}

	if(cos_info->cos_flag & COS_IOWRT) {
		if(cos_info->cos_flag & COS_IODIRTY) {	/* flush buffer if dirty */
			if (_cos_iflush(fio, stat) < 0)
				goto badret;
		}
	}
/*
 *	We have recorded the location of the bit after the last EOD
 *	written.  Truncate the file.  The user
 *	may have written that EOD, then rewound and read.  No matter.
 *	We truncate by virtue of having written that EOD.  This means that
 *	truncation does not conflict with things like pre-allocation.
 */
	if (cos_info->cos_lasteod > 0) 
		{
		int lasteodb;	/* last eod byte */

		lasteodb = CEIL(cos_info->cos_lasteod, BLKSZ) >> 3;
		if (XRCALL(llfio, seekrtn) llfio, lasteodb, 0, stat) >= 0)
			{
			cos_info->cos_diskpos = lasteodb;
			if (XRCALL(llfio, weodrtn) llfio, stat) < 0)
				goto badret;
			}
		}

/*
 *	Make sure that any outstanding I/O is complete.  The close does
 *	*not* clear it.
 */
	ASWAIT(fio, &cos_info->bstat);

	ret = XRCALL(llfio, closertn) llfio, stat);	/* close file */

	_cos_clfree(fio);
	return(ret);

badret:
	XRCALL(llfio, closertn) llfio, stat);	/* be sure t'close it */
	_cos_clfree(fio);
	return(ERR);
	}

void
_cos_clfree(struct fdinfo *fio)
	{
	struct cos_f *cos_info;

	cos_info = (struct cos_f *)fio->lyr_info;
	if (cos_info != NULL)
		{
		if (BPTR2CP(cos_info->obuf) != NULL)
			{
			free(BPTR2CP(cos_info->obuf));
			SET_BPTR(cos_info->obuf, CPTR2BP(NULL));
			}
		}
	if (BPTR2CP(fio->_base) != NULL)
		{
		free(BPTR2CP(fio->_base));
		SET_BPTR(fio->_base, CPTR2BP(NULL));
		}
	if (fio->lyr_info != NULL)
		{
		free(fio->lyr_info);
		fio->lyr_info = NULL;
		}
	if (fio->fioptr != NULL)
		{
		free(fio->fioptr);
		fio->fioptr = NULL;
		}
	return;
	}
