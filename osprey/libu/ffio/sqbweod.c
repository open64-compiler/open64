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


#pragma ident "@(#) libu/ffio/sqbweod.c	92.1	06/29/99 13:16:47"


#include <stdio.h>
#include <ffio.h>
#include <errno.h>
#include "sqbio.h"

/*
 * Write an EOD to a buffer-layer file.
 *
 */
int
_sqb_weod(struct fdinfo *fio, struct ffsw *stat)
{
	struct fdinfo	*llfio;


	if (fio->rwflag == WRITIN) {
		if (_sqb_flush(fio, stat) < 0) {
			return(ERR);
		}
	}
	else if (fio->rwflag == READIN || fio->rwflag == POSITIN) {
		/* synchronize physical position with logical */
		if (_sqb_sync(fio, stat, 1) < 0) {
			return(ERR);
		}
	}
	fio->rwflag = WRITIN;

/*
 *	Truncate the underlying layer at the same location.
 */
	llfio  = fio->fioptr;

	if (XRCALL(llfio,weodrtn) llfio, stat) == ERR)
		return(ERR);

	SETSTAT(stat, FFEOD, 0);
	return(0);
}
