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


#pragma ident "@(#) libu/ffio/sqbseek.c	92.1	06/29/99 13:16:47"


#include <stdio.h>
#include <ffio.h>
#include "sqbio.h"

/*
 *
 * Perform a seek operation on a buffered file.  
 *
 * Parameters:
 *	fio - fdinfo pointer
 *	pos - requested byte offset
 *	whence - SEEK_SET, SEEK_CUR, or SEEK_END
 *	stat - status return word
 *
 * Return value:
 *
 *	The new byte postion in the file.
 *	If an error occurs, -1 is returned, and the stat->sw_error field is 
 *	set.
 */
_ffseek_t
_sqb_seek(struct fdinfo *fio, off_t pos, int whence, struct ffsw *stat)
{
	_ffseek_t	ret;
	struct sqb_f	*sqb_info;
	int sync = 0;
	struct fdinfo *llfio;

	sqb_info = (struct sqb_f *)fio->lyr_info;
	llfio = fio->fioptr;

	/* We could probably do more record keeping, and */
	/* determine whether the desired position was already */
	/* in our buffers. */

	/* Wait for outstanding I/O */
        if (fio->rwflag == READIN || fio->rwflag == POSITIN) {
		if (whence == SEEK_CUR)
			sync = 1;	/* synchronize physical and logical */
					/* positions */
                if (_sqb_sync(fio, stat, sync) < 0) {
                        return(ERR);
                }
        }
        else if (fio->rwflag == WRITIN) {
                if (_sqb_flush(fio, stat) < 0) {
                        return(ERR);
                }
        }
	fio->rwflag = POSITIN;
	ret = XRCALL(llfio, seekrtn) llfio, pos, whence, stat);
	return(ret);	
}
