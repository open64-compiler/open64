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


#pragma ident "@(#) libu/ffio/ccasyncpage.c	92.1	06/29/99 13:16:47"

#include <stdio.h>
#include <errno.h>
#include <ffio.h>
#include "ccaio.h"

/*
 * _cca_sync_page
 *
 *	Flush a cache page to the file.
 */
_cca_sync_page(
struct cca_f	*cca_info,
struct cca_buf	*cubuf,
struct ffsw     *stat		/* pointer to status return word */
)

{
   int			i;
   int          	ret;
   int          	partial_dirty;
   off_t          	file_byte_pos;
   struct fdinfo	*llfio;

   file_byte_pos = cubuf->file_page.parts.page_number * cca_info->byte_per_pg;
   llfio = cca_info->nextfio;

   if(cubuf->sw.rw_mode == FFSW_WRITING ) {
      CCAWAITIO(cubuf->sw,stat,ret);
      if(ret==ERR)return(ERR);
   }

   partial_dirty = FALSE;
   for(i=0;i<cca_info->dirty_sectwds;i++) {
      if( cubuf->unsynced_sectors[i] != cca_info->dirty_sectors_check[i] ) {
         partial_dirty = TRUE;
         break;
      }
   }

/*
 * If the page is partially valid, call _cca_wrabuf to flush the dirty
 * valid sectors and then call _cca_rdabuf to read back the entire page.
 *
 * (Why don't we just pre-read the invalid sectors and defer the syncing??
 *  this is done for writes, so the page is about to become dirty again...)
 */
   if( partial_dirty ) {
      if( cubuf->sw.llfio ) {
         CCAWAITIO(cubuf->sw,stat,ret);
         if(ret==ERR) return(ERR);
      }

      ret = _cca_wrabuf(cca_info,
             llfio, cubuf,
             cca_info->byte_per_pg ,
             file_byte_pos ,
             'a',
             stat);
       if( ret == ERR ) return( ERR );

       if( cubuf->sw.llfio ) {
          CCAWAITIO(cubuf->sw,stat,ret);
          if(ret==ERR) return(ERR);
       }

       ret = _cca_rdabuf(cca_info, 
               llfio, cubuf, 
               cca_info->byte_per_pg ,
               file_byte_pos ,
               's' , 
               stat);
       if( ret == ERR ) return( ERR );

       cubuf->pre_init = TRUE;
       return(1);
   }
   else {
      cubuf->pre_init = TRUE;
      return(0);
   }
}
