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


#pragma ident "@(#) libu/ffio/ccaclosdiag.c	92.1	06/29/99 13:16:47"


#include <stdio.h>
#include <fcntl.h>
#include <malloc.h>
#include <unistd.h>
#include <ffio.h>
#include "ccaio.h"

#ifndef _CCA_DIAGS
#error _CCA_DIAGS needs to be defined
#endif
/*
 *	output cca diags
 *
 */
_cca_close_diags(
struct fdinfo	*fio,
struct cca_f	*cca_info,
int             file_header,
int             final)
{
   float wall, rtc_factor;
   float percent;
   int   total;
   int   shared_cache;
   int   file_number; 
   int   i;
   struct cca_f	*shared_cca_info;
   int divisor;
   char units[16];
   char PROGRAM[] = "program";
   char PARENT[]  = "parent";
   char CHILD[]   = "child";
   char *parent;
   char *child;
   char *cca;

   shared_cache = cca_info->shared_cache;
   file_number  = cca_info->file_number;

   if( shared_cache && file_header && final )
   {
      shared_cca_info = _CCA_scache[shared_cache].cca_info[0];

      if( cca_info->max_count > shared_cca_info->max_count ) shared_cca_info->max_count = cca_info->max_count;
      if( cca_info->max_lead > shared_cca_info->max_lead ) shared_cca_info->max_lead = cca_info->max_lead;
      shared_cca_info->chain_start_count        += cca_info->chain_start_count;
      shared_cca_info->adv_start_count          += cca_info->adv_start_count;
      shared_cca_info->adv_used_count           += cca_info->adv_used_count;
      shared_cca_info->exempts_failed           += cca_info->exempts_failed;
      shared_cca_info->exempts_issued           += cca_info->exempts_issued;
      shared_cca_info->write_behind_count       += cca_info->write_behind_count;
      shared_cca_info->cache_pages_not_preread  += cca_info->cache_pages_not_preread;
      shared_cca_info->unsynced_pages_read      += cca_info->unsynced_pages_read;
      shared_cca_info->partial_pages_written    += cca_info->partial_pages_written;
      shared_cca_info->cache_pages_synced_for_read    += cca_info->cache_pages_synced_for_read;
      shared_cca_info->cache_pages_synced_for_ill_formed_write    += 
                                 cca_info->cache_pages_synced_for_ill_formed_write;
      shared_cca_info->read_hide_time           += cca_info->read_hide_time;
      shared_cca_info->num_sectors_used         += cca_info->num_sectors_used;
      shared_cca_info->sect_reused       += cca_info->sect_reused;
      shared_cca_info->read_hit                 += cca_info->read_hit;
      shared_cca_info->read_miss                += cca_info->read_miss;
      shared_cca_info->write_hit                += cca_info->write_hit;
      shared_cca_info->write_miss               += cca_info->write_miss;
      shared_cca_info->unknown_recalls          += cca_info->unknown_recalls;
     
      shared_cca_info->bytes_read_by_cca        += cca_info->bytes_read_by_cca;
      shared_cca_info->bytes_written_by_cca     += cca_info->bytes_written_by_cca;
      shared_cca_info->bytes_written_to_cca     += cca_info->bytes_written_to_cca;
      shared_cca_info->bytes_read_from_cca      += cca_info->bytes_read_from_cca;
   }

   if( file_header )
   {
         if( final )
            fprintf(_GL_cca_logptr, "\nCCA final stats for file %16s\n", cca_info->file_name);
         else
            fprintf(_GL_cca_logptr, "\nCCA intermediate stats for file %16s\n", cca_info->file_name);

         if( cca_info->shared_cache == 0 )
            fprintf(_GL_cca_logptr," Used private cache\n");
         else
            fprintf(_GL_cca_logptr," Used shared cca cache %d\n",cca_info->shared_cache);
   }
   else
   {
         fprintf(_GL_cca_logptr,"\nCCA stats for Shared cache %d\n", shared_cache);
   }

         if( cca_info->optflags.sds )
            fprintf(_GL_cca_logptr, "     %4d sds ", cca_info->nbufs);
         else
            fprintf(_GL_cca_logptr, "     %4d memory ", cca_info->nbufs);
         fprintf(_GL_cca_logptr,
              "cache pages of %8d blocks (%d sectors)\n",
              cca_info->blks_per_pg,cca_info->sect_per_pg);
         fprintf(_GL_cca_logptr,
              "             maximum read ahead (pages) : %8d\n", cca_info->max_lead);

         if( cca_info->optflags.full_diags )
         {
         fprintf(_GL_cca_logptr,
              "             chains started             : %8d\n",cca_info->chain_start_count);
         fprintf(_GL_cca_logptr,
              "             maximum chain length       : %8d\n",cca_info->max_count);
         }

         percent = 0.0;
         if(cca_info->adv_start_count > 0 ) 
                     percent = 100.*((float)cca_info->adv_used_count/(float)cca_info->adv_start_count);
         fprintf(_GL_cca_logptr,
              "             advance reads used/started : %8d/%8d   %5.2f%%\n",
                 cca_info->adv_used_count,cca_info->adv_start_count,percent);
         if( cca_info->optflags.full_diags )
         {
         fprintf(_GL_cca_logptr,
              "             exempts overridden/issued  : %8d/%8d\n",cca_info->exempts_failed,cca_info->exempts_issued);
         fprintf(_GL_cca_logptr,
              "             write behinds started      : %8d\n"    ,cca_info->write_behind_count);


         fprintf(_GL_cca_logptr,
              "             cache pages not preread    : %8d\n"    ,cca_info->cache_pages_not_preread);
         fprintf(_GL_cca_logptr,
             "             partial cache pages written: %8d\n"    ,cca_info->partial_pages_written);
         fprintf(_GL_cca_logptr,
             "             page syncs forced by writes: %8d\n"    ,cca_info->cache_pages_synced_for_ill_formed_write);
         fprintf(_GL_cca_logptr,
             "             page syncs forced by reads : %8d\n"    ,cca_info->cache_pages_synced_for_read);
         fprintf(_GL_cca_logptr,
             "             unsynced cache pages read  : %8d\n"    ,cca_info->unsynced_pages_read);
         fprintf(_GL_cca_logptr,
             "             unknown async recalls      : %8d\n"    ,cca_info->unknown_recalls);



         rtc_factor = ( (float)sysconf(_SC_CRAY_CPCYCLE) ) / 1000000000000.;

         wall = ( (float)cca_info->read_hide_time*rtc_factor);
         fprintf(_GL_cca_logptr,
              "             advance read hide time     : %8.2f\n",wall);

         percent = 0.0;
         if(cca_info->num_sectors_used > 0 ) 
               percent = 100.*((float)cca_info->sect_reused/(float)cca_info->num_sectors_used);
         fprintf(_GL_cca_logptr,
              "             sectors reused/(total used): %8d/%8d   %5.2f%%\n",
                 cca_info->sect_reused,cca_info->num_sectors_used,percent);
         }

         total = cca_info->read_hit+cca_info->read_miss;
         percent = 0.0;
         if(total > 0 ) percent = 100.*((float)cca_info->read_hit/(float)total);
         fprintf(_GL_cca_logptr,
              "             read  hits/total           : %8d/%8d   %5.2f%%\n",
                 cca_info->read_hit ,total,percent);

         total = cca_info->write_hit+cca_info->write_miss;
         percent = 0.0;
         if(total > 0 ) percent = 100.*((float)cca_info->write_hit/(float)total);
         fprintf(_GL_cca_logptr,
              "             write hits/total           : %8d/%8d   %5.2f%%\n",
                 cca_info->write_hit,total,percent);

         if(      cca_info->output_units & CCA_flags_g )
         {
            divisor = 1024*1024*1024;
            strcpy( units, "g" );
         }
         else if( cca_info->output_units & CCA_flags_m )
         {
            divisor = 1024*1024;
            strcpy( units, "m" );
         }
         else if( cca_info->output_units & CCA_flags_k )
         {
            divisor = 1024;
            strcpy( units, "k" );
         }
         else
         {
            divisor = 1;
            strcpy( units, "" );
         }

         if(      cca_info->output_units & CCA_flags_words )
         {
            divisor = divisor*8;
            strcat( units, "words");
         }
         else if( cca_info->output_units & CCA_flags_blocks )
         {
            divisor = divisor*4096;
            strcat( units, "blocks");
         }
         else
         {
            strcat( units, "bytes");
         }

         parent = PARENT;
         child  = CHILD;
         cca = "cachea";
      
         fprintf(_GL_cca_logptr, "             Data transferred   ( %s )\n",units);

         fprintf(_GL_cca_logptr, "                %8s --> %3s --> %-8s\n", parent,cca,child );
         fprintf(_GL_cca_logptr, "                %12d %9d\n",
                                 (cca_info->bytes_written_to_cca+divisor-1)/divisor ,
                                 (cca_info->bytes_written_by_cca+divisor-1)/divisor );

         fprintf(_GL_cca_logptr, "                %12d %9d\n",
                                 (cca_info->bytes_read_from_cca+divisor-1)/divisor ,
                                 (cca_info->bytes_read_by_cca+divisor-1)/divisor );

         fprintf(_GL_cca_logptr, "                %8s <-- %3s <-- %-8s\n", parent,cca,child );

}
