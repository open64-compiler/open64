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


#pragma ident "@(#) libu/ffio/evntatexit.c	92.2	10/07/99 22:15:19"


#include <ffio.h>
#include <errno.h>
#include <stdio.h>
#include "evntio.h"

/*
 * _evnt_atexit
 *
 * Called before the real atexit to flush the statistics files 
 *
 * Input:
 *
 * Output:
 *
 */
void 
_evnt_atexit()
{
	int     i;
	struct ffsw log_stat;
	int     log_ubc = 0;
	struct evnt_f evnt_info_dummy;	/* we need this for one of the MACROS */
	struct evnt_f *evnt_info;	/* we need this for one of the MACROS */

	evnt_info = &evnt_info_dummy;

	if (_GL_evnt_logptr)
	    fprintf(_GL_evnt_logptr, "\n");

	/* 
	 * close trace files
	 */
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
	for (i = 0; i < MAX_TRACE_FILES; i++) {
	    /*
	     * spit out (in diagnostics) how much space was 
	     * used for trace files
	     */
	    if (_GL_evnt_logptr) {
		if (_GL_evnt_trc_file[i].program_info.open_count) {
		    fprintf(_GL_evnt_logptr,
		        "  High water space used for files in event group %d =%d bytes\n",
		         i, _GL_evnt_trc_file[i].max_usage);
		}
	    }

	    /*
	     * we will flush trace file so everything
	     * is sync'd up, but not actually close the
	     * file because more tracing may occur after
	     * this point if a user has forgotten to close
	     * a file.  If we closed the trace files here
	     * and more tracing was done, the trace file could
	     * get corrupted
	     */
	    if (_GL_evnt_trc_file[i].ptr) {
#ifdef EVNT_DEBUG
		fprintf(stderr,"flushing trace file(s)\n");
#endif
		evnt_info->file_ptr = _GL_evnt_trc_file[i].ptr;
		EVNT_FLUSH_TRACE_INFO(i);
	    }
	}
#endif
}
