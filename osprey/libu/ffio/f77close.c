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


#pragma ident "@(#) libu/ffio/f77close.c	92.1	06/29/99 13:16:47"


#include <stdio.h>
#include <fcntl.h>
#if !defined(_ABSOFT)
#include <malloc.h>
#else
#include <stdlib.h>
#endif
#include <ffio.h>
#include "fxlist.h"
#include "f77io.h"


/*
 * Close an f77 file.
 */
int
_f77_close(struct fdinfo *fio, struct ffsw *stat)
{
        struct fdinfo *llfio;
        int ret;
	struct f77_xf *xfinfo;

        ret = 0;

/*
 *      If the file has been writing, flush and truncate after the last
 *      record written.
 */
	xfinfo = (struct f77_xf *)fio->lyr_info;
        if (fio->rwflag == WRITIN) {

                ret = XRCALL(fio, flushrtn) fio, stat);
                if (ret != 0) return(ERR);

                ret = XRCALL(fio, weodrtn) fio, stat);
                if (ret != 0) return(ERR);
        }

        if (xfinfo->_base != NULL)
                free(xfinfo->_base);      /* free buffer */
        if (fio->lyr_info != NULL)
                free((char *)fio->lyr_info);    /* free private storage */
        llfio = fio->fioptr;
        if (llfio != NULL) {
                ret = XRCALL(llfio, closertn) llfio, stat);
                free(llfio);            /* free info block */
        }
        return(ret);
}

