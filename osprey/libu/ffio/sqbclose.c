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


#pragma ident "@(#) libu/ffio/sqbclose.c	92.2	10/29/99 21:40:31"


#include <stdio.h>
#include <fcntl.h>
#include <malloc.h>
#include <ffio.h>
#include "sqbio.h"

void _sqb_clfree(struct fdinfo *fio);

/*
 *	Close the buffering layer.
 *      Returns: -1 if error occurred
 *		  0 if OK
 */

_sqb_close(struct fdinfo *fio, struct ffsw *stat)
{
	struct fdinfo *llfio;
	int ret, ret2;
	
	llfio = fio->fioptr;

	ret = ret2 = 0;

/*
 *	Flush any buffers and wait for any asynchronous requests.
 */
	if (fio->rwflag == WRITIN){
		ret =_sqb_flush(fio, stat);
	}
	else if (fio->rwflag == READIN || fio->rwflag == POSITIN) {
		if (_sqb_sync(fio, stat, 0) < 0)
			ret = -1;
	}
	
/*
 * close the file
 */
	ret2 = XRCALL(llfio, closertn) llfio, stat);

/*
 * free the space
 */
	_sqb_clfree(fio);
	if ((ret | ret2) == 0) return(0);
	return(ERR);
}

/*
 * Free the space associated with the buffering layer.
 */
void
_sqb_clfree(struct fdinfo *fio)
{
	struct sqb_f *sqb_info;

	if (fio->fioptr != NULL) {
		free(fio->fioptr);
		fio->fioptr = NULL;
	}
	sqb_info = (struct sqb_f *)fio->lyr_info;
	if (sqb_info != NULL) {
		if (sqb_info->sqb_buf != 0) {
			free(BPTR2CP(sqb_info->sqb_buf));
		}
		if (sqb_info->sqbio_base != NULL) {
			free(sqb_info->sqbio_base);
		}
		free(sqb_info);
		fio->lyr_info = NULL;
	}
	return;
}
