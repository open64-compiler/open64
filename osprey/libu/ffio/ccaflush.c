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


#pragma ident "@(#) libu/ffio/ccaflush.c	92.1	06/29/99 13:16:47"


#include <stdio.h>
#include <fcntl.h>
#include <malloc.h>
#include <unistd.h>
#include <ffio.h>
#include "ccaio.h"


/*
 * _cca_flush
 *
 *	Flush all dirty buffers to the underlying layer.
 *
 *	Return value:	 0 on success.
 *			-1 on failure.
 */
int
_cca_flush(
struct fdinfo	*fio,
struct ffsw	*stat
)
{
        int		i, nb, bs, ret;
	int		errv;
        struct cca_f	*cca_info;
	struct cca_buf	*cbufs;
        off_t           file_byte_pos;

        struct fdinfo   *llfio;

	errv	 = 0;
	cca_info = (struct cca_f *)fio->lyr_info;
	nb	 = cca_info->nbufs;
	bs	 = cca_info->bsize;

        llfio    = fio->fioptr;
	/*
	 * Loop once to start the flush of each dirty buffer.  Then loop a
	 * second time to wait for the completions of all buffer flushes.
	 */
           cbufs = cca_info->bufs;
	   for (i=0; i<nb; i++) 
           {
                if( cbufs[i].file_page.parts.file_number != cca_info->file_number ) continue;

                if( cca_info->optflags.scr == FALSE )
                {
                   if( cbufs[i].flags & CCA_DIRTY )  
                   {
                          if( cbufs[i].sw.llfio )
                          {
                             CCAWAITIO(cbufs[i].sw,stat,ret);
                             if (ret == ERR) errv = errv ? errv : stat->sw_error;
                          }

                          file_byte_pos = cbufs[i].file_page.parts.page_number *
					  cca_info->byte_per_pg;
                          ret = _cca_wrabuf(	cca_info,
                                        llfio,
					&cbufs[i],
                                        BITS2BYTES(bs),
					file_byte_pos,
					'a',		/*asynchronous*/
					stat);

                          if (ret == ERR) 
				errv = errv ? errv : stat->sw_error;
                   }
                }
	   }

	for (i=0; i<nb; i++)  /* wait for those still doing any I/O */
        {
          if(cbufs[i].sw.file_page.parts.file_number == cca_info->file_number )
          {
             CCAWAITIO(cbufs[i].sw,stat,ret);
             if (ret == ERR) errv = errv ? errv : stat->sw_error;
          }
	}

	if (errv) 
	{
		ERETURN(stat, errv, 0);
	}


        ret = XRCALL(llfio,flushrtn) llfio, stat);

        return( ret );
}
