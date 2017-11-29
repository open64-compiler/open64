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


#pragma ident "@(#) libu/ffio/ccadmpcache.c	92.1	06/29/99 13:16:47"

#include <stdio.h>
#include <ffio.h>
#include "ccaio.h"

/*
 * _cca_dump_cache
 *
 *	Dump current cache information.  This is for internal use only. 
 */
_cca_dump_cache(
char		*string,
struct cca_f	*cca_info)
{

	struct cca_buf *cb;
	int i, j;
	int prev_index;
	int buffer[512];
	int *first_word;
	long buf_bptr;
	int status;


	fprintf(_GL_cca_logptr,
"\ndump_cache : called by %s\n",string);
	fprintf(_GL_cca_logptr,
"Cache %2d : file %s\n", cca_info->shared_cache,cca_info->file_name);
	fprintf(_GL_cca_logptr,
"                    %d cache pages  of %d blocks\n",
		cca_info->nbufs,cca_info->blks_per_pg);
	fprintf(_GL_cca_logptr,
"---------------------------------------------------------------------------------------------\n");
	fprintf(_GL_cca_logptr,
" page  file    page  atime flags  adv chain  direct  should   pre          first word       dirty blocks\n");
	fprintf(_GL_cca_logptr,
"    #                            read   pos            call  init\n");

	for(i=0;i<cca_info->nbufs;i++) {
		cb = &(cca_info->bufs[i]);
		if(cb->prev_cubuf==NULL)
			prev_index = -1;
		else
			prev_index = (cb->prev_cubuf)->index;

#ifdef  SDS_SUPPORTED

		if( cca_info->optflags.sds ) {
			buf_bptr = (long) ((int)(buffer)<<6);
			status = _mem_fr_sds( buf_bptr, cb->buf, 2048*64 );
			first_word = buffer;
		}
		else
#endif
		{
			first_word = (int *)(cb->buf>>6);
		}

		fprintf(_GL_cca_logptr,"%5d %5d %5d %7d%6x%5d%6d%8d%8d%5d",
			cb->index,
			cb->file_page.parts.file_number,
			cb->file_page.parts.page_number,
			cb->atime,
			cb->flags,
			cb->adv_read,
			cb->chain_position,
			cb->direction,
			cb->should_call.parts.page_number,
			cb->pre_init);

		fprintf(_GL_cca_logptr,"  0x%16.16x",first_word[0]);

		for(j=0;j<cca_info->dirty_sectwds;j++)
			fprintf(_GL_cca_logptr, "  0x%16.16x",
				cb->valid_sectors[j]);
		fprintf(_GL_cca_logptr,"\n");
	}
}
