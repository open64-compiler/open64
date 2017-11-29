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


static char USMID[] = "@(#) libu/ffio/c1/er90blseek.c	92.0	10/08/98 14:57:41";


#include <errno.h>
#include <ffio.h>
#include "er90by.h"

/*
 * _er90b_lseek() ER90 byte-stream "seeks". We can only rewind,
 * because we cannot know our current position relative to the
 * beginning of the file - this is because files can span volumes
 */

_er90b_lseek(struct fdinfo *fio, int pos, int whence, struct ffsw *stat)
{
int ret;

	if (whence == 0 && pos == 0) {
		ret = _er90b_rewd((ER90BYT *)fio->lyr_info);
		if (ret < 0)
			ERETURN(stat, errno, 0);
		fio->ateof = 0;
		fio->ateod = 0;
		SETSTAT(stat, FFBOD, 0);
	}
	else {
		ERETURN(stat, FDC_ERR_NOSUP, 0);
	}
	fio->rwflag = POSITIN;
	return (0);
}
