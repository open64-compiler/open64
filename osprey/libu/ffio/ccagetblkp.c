/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
 */

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


#pragma ident "@(#) libu/ffio/ccagetblkp.c	92.3	10/29/99 21:40:31"


#include <stdio.h>
#if defined(BUILD_OS_DARWIN)
#include <limits.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <values.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <errno.h>
#include <ffio.h>
#include "ccaio.h"

struct cca_buf *
_cca_getblk_p(
struct cca_f	*cca_info,	/* cca_f structure for the file */
struct fdinfo	*llfio,		/* ffio file descriptor for underlying layer */
off_t		fileaddr,	/* bit offset within the file of the buffer.
				 * This number must be a multiple of the buffer
				 * size. */
int		rd,		/* 0 if writing AND the entire page of data is
				 * being written.  != 0 if the pages might need
				 * to be pre-read. */
struct ffsw	*stat,		/* pointer to status return word */
struct cca_buf *wait_cubuf,	/* If not NULL the buffer to wait on instead of
				 * read */
char            read_write_mode,/* was getblkp called by ccaread 'r' or 
				 * ccawrite 'w' */
char            sync_mode)	/* do we need to wait for the buffer requested*/
{

    struct cca_buf *cubuf;

    int i;
    off_t advance_addr;
    FILE_PAGE       file_page;
    FILE_PAGE       prev_file_page;
    FILE_PAGE       adv_page;
    struct cca_buf  *prev_cubuf;
    struct cca_buf  *current_cubuf;
    struct cca_buf  *advance_cubuf;
    int             ret;
    int             bs;
    int             wait;	
    int             reads_to_start;
    int             direction;
    off_t           file_byte_pos;
    int64           chain_pos;
    int             offset;
    int             threshold;
    struct ffsw     err_stat;

   direction = 999;
   bs        = cca_info->bsize;
   file_page.parts.page_number = fileaddr/bs;
   file_page.parts.file_number = cca_info->file_number;

   if ( wait_cubuf == NULL ) {
      current_cubuf = _cca_getblk( cca_info, llfio, fileaddr, rd, stat,
				   sync_mode, read_write_mode );

      if ( current_cubuf == NULL ) return( NULL );

      current_cubuf->file_page = file_page;
      if ( cca_info->max_lead <= 0 ) {
         cca_info->cubuf = current_cubuf;
         return( current_cubuf );
      }
      wait = FALSE;
      current_cubuf->eligible = TRUE;
      current_cubuf->adv_read = FALSE;
      current_cubuf->chain_position = 0;
      current_cubuf->direction = 0;
      current_cubuf->prev_cubuf = NULL;
      current_cubuf->prev_file_page.all = -1;
      current_cubuf->exempt_count = 0;
   }
   else {
       current_cubuf = wait_cubuf;
       if ( sync_mode == 's' )
          wait = TRUE;
       else
          wait = FALSE;
   }

   if ( current_cubuf->eligible == FALSE ) {
      goto WAIT;
   }

   CHRONOMETER(current_cubuf,"touching current_cubuf"); 
   cubuf = current_cubuf;

   if ( cubuf->direction == 0 ) { /* Not part of a sequence */
      direction = 0;
      prev_cubuf = cca_info->bufs;
      for(i=0;i<cca_info->nbufs;i++) {
         if ( prev_cubuf->direction == 0 ) { /* page is not part of a chain */
            if (     (prev_cubuf->file_page.all+1) == file_page.all ) {
               direction = 1;
               break;
            }
            else if ((prev_cubuf->file_page.all-1) == file_page.all ) {
               direction = -1;
               break;
            }
         }

         prev_cubuf ++;
      }

      if ( direction == 0 ) { /* could not figure out any chaining */
         cubuf->eligible = FALSE;
         goto WAIT;
      }

      prev_cubuf->direction    = direction;
      prev_cubuf->eligible    = FALSE;
      prev_cubuf->should_call.parts.file_number =
		prev_cubuf->file_page.parts.file_number;
      prev_cubuf->should_call.parts.page_number =
		prev_cubuf->file_page.parts.page_number + direction;
      prev_cubuf->chain_position = 1;
          cubuf->direction       = direction;
           cubuf->should_call.parts.file_number =
		cubuf->file_page.parts.file_number;
           cubuf->should_call.parts.page_number =
		cubuf->file_page.parts.page_number + direction;
	   /*
	    * Pretend prev_cubuf did call cubuf, for write behind's sake.
	    */
           cubuf->prev_cubuf     = cubuf;  
           cubuf->prev_file_page.all = prev_cubuf->file_page.all;
           cubuf->chain_position = prev_cubuf->chain_position + 1;
      cca_info->chain_start_count ++;
   }

   current_cubuf = cubuf;

   threshold = cca_info->max_lead;
   if ( cca_info->max_lead == 0 || ( current_cubuf->eligible == FALSE ) ) {
      reads_to_start = 0;
   }
   else if ( current_cubuf->chain_position-threshold >= cca_info->max_lead || 
            current_cubuf->chain_position           <= threshold ) {

      reads_to_start = 1;
   }
   else {
      reads_to_start = 2;
   }

   const FILE_PAGE marker = { { 0xff, 0xffffffffffffffLL } };
   
   if ( reads_to_start ) {
      current_cubuf->eligible = FALSE;
      direction = current_cubuf->direction;
      adv_page  = current_cubuf->should_call;
      for(i=0;i<reads_to_start;i++) {
         adv_page.parts.page_number += direction*i;  /* works only if i=0 or 1*/
         if ( adv_page.parts.page_number  == marker.parts.page_number )
	   break;
   
         CCA_FINDBLK(cca_info, adv_page, cubuf, err_stat, ret);
         if (ret == ERR) {
            *stat = err_stat;
            return(NULL);
         }
         advance_cubuf = cubuf;
   
         if (advance_cubuf == NULL ) {
            advance_addr = adv_page.parts.page_number * bs;
            if ((advance_addr > cca_info->fsize) && (read_write_mode == 'r') ) {
               break;
            }
            if (advance_addr < 0) {
               break;
            }
            current_cubuf->protected = TRUE;
            advance_cubuf = _cca_getblk(cca_info,llfio, advance_addr, 
                         TRUE, stat,'a',read_write_mode);
            current_cubuf->protected = FALSE;
            if ( advance_cubuf == NULL ) return( NULL );
     
            cca_info->adv_start_count ++;
            advance_cubuf->adv_read = TRUE;
            advance_cubuf->exempt_count = 0;
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
            advance_cubuf->read_start_rtc = _rtc();
#endif
            advance_cubuf->file_page.all = adv_page.all;
         }
   
         CHRONOMETER(advance_cubuf,"touching advance_cubuf"); 
   
         chain_pos = current_cubuf->chain_position + direction *
		(adv_page.parts.page_number -
		 current_cubuf->file_page.parts.page_number);

         if ( chain_pos-threshold >= cca_info->max_lead )
            offset = cca_info->max_lead;
         else if ( chain_pos <= threshold )
            offset = 1;
         else
            offset = chain_pos-threshold;

         advance_cubuf->chain_position = chain_pos;

         advance_cubuf->should_call.parts.file_number =
		adv_page.parts.file_number;
         advance_cubuf->should_call.parts.page_number =
		adv_page.parts.page_number +direction*offset;
         advance_cubuf->direction = direction;
         advance_cubuf->eligible = TRUE;
         advance_cubuf->exempt_count = 0;
         advance_cubuf->prev_file_page.all = adv_page.all - direction;
         if ( advance_cubuf->chain_position > cca_info->max_count )
		cca_info->max_count=advance_cubuf->chain_position;
     }
   }

  cubuf = current_cubuf;

WAIT :
   if ( current_cubuf->adv_read ) {
      current_cubuf->adv_read = FALSE;
      cubuf = current_cubuf;
      cca_info->adv_used_count ++;

#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
      cca_info->read_hide_time =
 	 cca_info->read_hide_time + _rtc() - current_cubuf->read_start_rtc;
#endif

      if ( cubuf->sw.llfio ) {
         CCAWAITIO( cubuf->sw, stat, ret);
      }

      /* write behind the previous block */
      if ( cca_info->optflags.wb ||
	  ( cca_info->optflags.hldwb && *(cca_info->spilled)) ) {

         prev_file_page.all = current_cubuf->prev_file_page.all;
         prev_cubuf = NULL;
         for(i=0;i<cca_info->nbufs;i++) {
            if ( cca_info->bufs[i].file_page.all == prev_file_page.all ) {
               prev_cubuf = &(cca_info->bufs[i]);
               break;
            }
         }
         if ( prev_cubuf ) {
            if ( ! (prev_cubuf->adv_read) ) {
               prev_cubuf->chain_position = 0;
               if ( ( prev_cubuf->flags & CCA_DIRTY ) &&
		   ( prev_cubuf->sw.rw_mode == 0 )) { 

		  /* if writing already, don't start a write behind */

                  cca_info->write_behind_count++;
                  file_byte_pos = (int64)prev_cubuf->file_page.parts.page_number *
				  cca_info->byte_per_pg;
                  ret = _cca_wrabuf(cca_info,
                               llfio, prev_cubuf,
                               BITS2BYTES(bs),
                               file_byte_pos,
                               'a',
                               stat);
                  if (ret == ERR) {return(NULL);}
               }
            }
         }
      }
   }

   cca_info->cubuf = current_cubuf;
   return( current_cubuf );
}
