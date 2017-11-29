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


#pragma ident "@(#) libu/ffio/ccatracker.c	92.1	06/29/99 13:16:47"


#include <stdio.h>
#include <errno.h>
#include <ffio.h>
#include "ccaio.h"

/*
 *	_cca_start_tracker
 *
 *	Issues an asyncronous I/O request for a cache page.  
 *
 *	Called by _cca_writea and cca_reada
 *
 */

struct cca_async_tracker *
_cca_start_tracker(
struct fdinfo	*llfio,
struct cca_f	*cca_info,
struct cca_buf	*cubuf,
struct ffsw     *stat,
int             mode,
bitptr		buf_ptr	,
int		pgoff,
int		bit_count)
{
   struct cca_async_tracker *this_tracker;
   struct cca_async_tracker *last_tracker;
   struct cca_async_tracker *pointer;
   int ret;
   struct ffsw getblk_stat;
   int do_sync = FALSE;

   if( cubuf->eligible || cubuf->adv_read ) {
      off_t fileaddr;
      fileaddr = (cubuf->file_page.parts.page_number)*cca_info->bsize;
      cubuf = _cca_getblk_p(cca_info, llfio, fileaddr, TRUE, &getblk_stat,
			    cubuf, mode , 'a' );
      if( cubuf == NULL ) {
	 *stat = getblk_stat;
	 return( (struct cca_async_tracker *)(ERR) );
/*
         fprintf(stderr,"_cca_start_tracker : error starting adv_read\n");
         tracebk();
         exit(-1);
*/
      }
   }

   if( cubuf->sw.sw_flag ) { /* the underlying i/o is complete */
      do_sync = TRUE;
      goto branch_1;
   }

   this_tracker = cca_info->async_tracker;

   while( this_tracker ) {
      if( this_tracker->mode == CCA_TRACKER_FREE ) break;
      last_tracker = this_tracker;
      this_tracker = this_tracker->next_tracker;
   }

   if( this_tracker == NULL && cca_info->extend_trackers_failed == FALSE ) {
      if( (pointer = _cca_add_trackers( 5 ) ) == NULL ) {
         cca_info->extend_trackers_failed = TRUE;
      }
      else {
         last_tracker->next_tracker = pointer;
         this_tracker = pointer;
      }
   }

branch_1 :
   if( this_tracker == NULL || do_sync )  /* no tracker available, we have to do it syncronously */
   {
      if( mode == CCA_TRACKER_READA || mode == CCA_TRACKER_WRITEA ) {
         _cca_complete_tracker( cca_info, NULL , &ret , cubuf, mode, pgoff, buf_ptr, bit_count , stat );
         if( ret == ERR )
	    return( (struct cca_async_tracker *)(ERR) );
      }
      return(0);
   }
   else {
      this_tracker->mode        = mode;
      this_tracker->stat        = stat;
      this_tracker->buf_ptr      = buf_ptr;
      this_tracker->pgoff       = pgoff;
      this_tracker->bit_count   = bit_count;
      this_tracker->cubuf       = cubuf;
      this_tracker->file_page   = cubuf->file_page;
      cubuf->pending_asyncs ++;
   }

   return ( this_tracker );
}

void
_cca_complete_tracker(
struct cca_f *cca_info,
struct cca_async_tracker *this_tracker,
int *ret_value,
struct cca_buf *cubuf_in,
int mode_in,
int pgoff_in,
bitptr buf_ptr_in,
int bit_count_in,
struct ffsw *stat_in )
{
   struct cca_buf *cubuf;
   int first_dirty_sector,last_dirty_sector;
   bitptr cache_ptr,buf_ptr;
   struct ffsw wait_stat,sync_stat;
   int mode,j,ret,bit_count,pgoff;
   struct ffsw *sw;

   if( this_tracker )
   {
      cubuf = ((struct cca_async_tracker *)this_tracker)->cubuf;
      mode  = ((struct cca_async_tracker *)this_tracker)->mode;
      pgoff    = ((struct cca_async_tracker *)this_tracker)->pgoff;
      buf_ptr   = ((struct cca_async_tracker *)this_tracker)->buf_ptr;
      bit_count = ((struct cca_async_tracker *)this_tracker)->bit_count;
      ((struct cca_async_tracker *)this_tracker)->mode  = CCA_TRACKER_COMPLETED;
      ((struct cca_async_tracker *)this_tracker)->cubuf = NULL; 
      sw = this_tracker->stat;
   }
   else
   {
      cubuf = cubuf_in;
      mode  = mode_in;
      pgoff    = pgoff_in;
      buf_ptr   = buf_ptr_in;
      bit_count = bit_count_in;
      sw = stat_in;
   }
   *ret_value = 0;
   if(cubuf->sw.llfio)
   {
      CCAWAITIO(cubuf->sw,&wait_stat,ret);
      if( ret == ERR ) 
         {
         _SETERROR(sw, wait_stat.sw_error, 0);
         *ret_value = ERR;
         }
      else 
         {
         sw->sw_count += bit_count/8;
         }
   }
   else
   {
      sw->sw_count += bit_count/8;
   }
   first_dirty_sector = pgoff/cca_info->bits_per_sect;
   last_dirty_sector  = (pgoff+bit_count-1)/cca_info->bits_per_sect;
   if( cca_info->optflags.diags )
   {
#ifdef _CCA_DIAGS
      cca_info->num_sectors_used += (last_dirty_sector-first_dirty_sector+1);
#endif
      for(j=first_dirty_sector;j<=last_dirty_sector;j++)
      {
#ifdef _CCA_DIAGS
         if(_GETBIT(j,cubuf->sector_used))
            cca_info->sect_reused ++;
         else
#endif
            _SETBIT(j,cubuf->sector_used);
      }
   }
   SET_BPTR(cache_ptr, INC_BPTR(cubuf->buf, pgoff));
   if( mode == CCA_TRACKER_WRITEA && *ret_value == 0 ) 
   {

      if (cubuf->pre_init == FALSE) {
         /* if the page is partially valid and the request is not */
         /* sector aligned, then we force a pre-read of the page */
            if ((pgoff & (cca_info->bits_per_sect-1)) ||
              ((pgoff + bit_count -1) & (cca_info->bits_per_sect-1))) {
              int sync_ret;
              sync_ret = _cca_sync_page( cca_info, cubuf, &sync_stat);
              if (sync_ret == ERR)
                 *ret_value = ERR; 
            }
      }
      cubuf->flags |= CCA_DIRTY;
      for(j=first_dirty_sector;j<=last_dirty_sector;j++)
      {
         _SETBIT(j,cubuf->valid_sectors);
         _SETBIT(j,cubuf->unsynced_sectors);
      }
#ifdef  SDS_SUPPORTED
      if (cca_info->optflags.sds) {
	if (_any_sds_fr_mem(cache_ptr, buf_ptr, bit_count) == ERR) {
         	_SETERROR(sw, errno, 0);
		*ret_value=ERR;
	 }
      }
      else
#endif
         {MOV_BITS(cache_ptr, buf_ptr, bit_count);} 
      cca_info->bytes_written_to_cca += bit_count/8;
   }
   else if( mode == CCA_TRACKER_READA && *ret_value == 0 )
   {
      if( cubuf->pre_init == FALSE )
      {
         int unsynced_page_read_hit = TRUE;
         for(j=first_dirty_sector;j<=last_dirty_sector;j++)
         {
            if(!(_GETBIT(j,cubuf->valid_sectors)))
            { 
                int sync_ret;
                sync_ret = _cca_sync_page( cca_info, cubuf, &sync_stat );
                if( sync_ret == ERR )
                   { *ret_value = ERR; }
#ifdef _CCA_DIAGS
                else
                   cca_info->cache_pages_synced_for_read += sync_ret;
#endif
                unsynced_page_read_hit = FALSE;
                break;
            } 
         }
         if(unsynced_page_read_hit) cca_info->unsynced_pages_read ++;
      }
      if( *ret_value == 0 ) 
      { 
#ifdef  SDS_SUPPORTED
         if (cca_info->optflags.sds) {
	   if (_any_mem_fr_sds(buf_ptr, cache_ptr, bit_count) == -1) {
         	_SETERROR(sw, errno, 0);
		 *ret_value=ERR;
	   }
	 }
         else
#endif
            { MOV_BITS(buf_ptr, cache_ptr, bit_count); } 
      }
      cca_info->bytes_read_from_cca += bit_count/8;
   }
}
