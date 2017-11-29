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


#pragma ident "@(#) libu/ffio/cmpclose.c	92.2	06/29/99 13:16:47"

#include <stdio.h>
#include <fcntl.h>
#include <malloc.h>
#include <ffio.h>
#include "cmpio.h"
 
int
_cmp_close(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
{
	cmp_lyr	*cinfo;
        int    	fulp = FULL;
	int	ubc = 0, ret;
        struct  fdinfo *llfio;
	char	*rtnName = "_cmp_close";
 
        llfio = fio->fioptr;
	cinfo = fio->lyr_info;

	TRC_ENTER;

	/*
	 * If we are reading a compressed file we want to skip writing 
	 * anything to it.
	 */
	if ((cinfo->OpnFlags & O_WRONLY) == 0) {
		TRC_INFO("Data Cache size %d, hits %d, misses %d", 
			 cinfo->DSegCacheSize, cinfo->Hit, cinfo->Miss);
		goto close;
	}

	/*
	 * If 'cinfo->WroteHeader' is FALSE this means that user only
	 * did an ffopen() and an ffclose. In this case, we do not want
	 * to write anything to the file. Just proceed to close it.
	 */
	if (cinfo->WroteHeader == FALSE) {
		goto close;
	}

	/*
	 * Update the segment descriptor for the last data segment. 
	 * We know this by looking at the value of 'cinfo->bytesInBuf'. 
	 * If this is non-zero it means we have one last data segment 
	 * to compress and write to the file.
	 */
	if (cinfo->BytesInBuf) {
		if (_writeDSeg(llfio, stat, fulp, &ubc, cinfo) == ERR) {
			return(ERR);
		}
	}

	/*
	 * Now write the trailer for this compressed file.
	 */
	if (_writeTrailer(llfio, stat, fulp, &ubc, cinfo) == ERR) {
		return(ERR);
	}

close:
	/*
	 * Free the compress layer information structure.
	 */
	_free_cmp_lyr(cinfo);

	/*
	 * Close the trace file is one was opened.
	 */
	_CloseTraceFile(cinfo);

	/*
	 *  close file
	 */
        ret = XRCALL(llfio, closertn) llfio, stat);

	TRC_LEAVE;

        return(ret);
}
