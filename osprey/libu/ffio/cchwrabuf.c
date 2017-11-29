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


#pragma ident "@(#) libu/ffio/cchwrabuf.c	92.2	10/07/99 22:13:28"

#include <stdio.h>
#include <ffio.h>
#include "cchio.h"
#include <cray/nassert.h>
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
#include <sys/listio.h>
#elif	defined(__mips)
#include "listio_mips.h"
#endif

/*
 * _cch_wrabuf
 *
 * Flushes to the underlying ffio layer one or more cache page buffers.  
 * The ffsw structure pointed to by stat receives the synchronous or
 * asynchronous completion status.
 * If the lastdata or firstdata fields in the buffer control block
 * are non-zero, then nblk had better be 1. In this case, we write only
 * the part of the buffer between firstdata and lastdata.
 *
 * When nblk > 1, _cch_wrabuf assumes that all page buffers get written to
 * contiguous parts of the file.
 *
 * Return value:
 *
 *	On normal completion 0 is returned. -1 is returned if an error is
 *	encountered, with the status set in stat.
 *
 * Side effects:
 *
 *	The CCH_DIRTY bit is cleared for all affected buffers.  If called
 *	in asynchronous mode, the buffers are placed in CCH_WRITING state.
 */
_cch_wrabuf(
struct cch_f	*cch_info,	/* cache info */
struct fdinfo	*llfio,		/* fdinfo pointer for underlying layer */
struct cch_buf	*bc,		/* buffer control block */
int		bytes,		/* number of bytes per page buffer */
off_t		bytoff,		/* byte offset within file */
int64		nblk,		/* number of contiguous buffers to flush */
off_t		*eof,		/* on input, contains the bit size of the 
				 * underlying file layer.  On output, this 
				 * size is updated if the file is extended. */
char		syncasync,	/* 's' for sync request, 'a' for async */
struct ffsw	*stat		/* io completion status structure */
)
{
	int i;
	ssize_t ret;
	int ubc;
	size_t tbytes, bytleft;
	size_t saveamt = 0;
	off_t end_of_data;	
	struct fflistreq list_array[1];
	char *bufptr;

	CCH_DEBUG(("_cch_wrabuf EN: bytes=%d (0%o)  bytoff=%d (0%o) \n",
		bytes,bytes,bytoff,bytoff));
	if (bc->firstdata || bc->lastdata) {
		assert(nblk <= 1);
		tbytes = (size_t)((bc->lastdata - bc->firstdata)/8);
		bytoff = bytoff + bc->firstdata/8;
		bufptr = BPTR2CP(bc->buf) + bc->firstdata/8;
	}
	else {
		tbytes = bytes * nblk;
		bufptr = BPTR2CP(bc->buf);
	}

#ifdef __mips
	if (cch_info->odirect && tbytes > cch_info->maxiosize){
		syncasync = 's';
	}
#endif
	ubc = 0;
	if (syncasync == 'a') {
		/*
		 * Seek to proper location
		 */
		if (XRCALL(llfio,seekrtn) llfio, bytoff, SEEK_SET, stat) == ERR)
			return(ERR);
		/*
		 * Start an asynchronous write.
		 */
		CLRFFSTAT(bc->sw);
		ret = XRCALL(llfio,writeartn) llfio, CPTR2BP(bufptr), tbytes,
					      &bc->sw, PARTIAL, &ubc);
		if (ret == ERR) {
			ERETURN(stat,bc->sw.sw_error,0);
		}
	
		bc[0].lnkcnt = nblk;			/* cnt of linked bufs */
		for (i=0; i<nblk; i++) {
			bc[i].flags |= CCH_WRITING;	/* update buffer stat */
			bc[i].flags &= ~CCH_DIRTY;	/* clear dirty flag */
			bc[i].lnk    = i;		/* chain several bufs */
		}
	}
	else {
#ifdef __mips
	    bytleft = tbytes;
	    do {
		/* The size of O_DIRECT requests is limited. */
		/* If we exceed that, break it up into multiple */
		/* requests.	*/
		if (cch_info->odirect && tbytes > cch_info->maxiosize){
			/* always work in multiples of pages */
			tbytes = bytes;
			while (tbytes + bytes <= cch_info->maxiosize) {
				tbytes += bytes;
                        };
		}
		bytleft -= tbytes;
	
#endif
		if (cch_info->do_sylistio){
#if	!defined(_LITTLE_ENDIAN)
                        list_array[0].li_opcode    = LO_WRITE;
                        list_array[0].li_flags     = LF_LSEEK;
                        list_array[0].li_offset    = bytoff;
#ifdef __mips
                        list_array[0].li_ffioptr   = llfio;
#else
                        list_array[0].li_fildes    = (int)llfio;
#endif
                        list_array[0].li_buf       = bufptr;
                        list_array[0].li_nbyte     = (size_t)tbytes;
                        list_array[0].li_status    = (struct ffsw *)(&bc->sw);
                        list_array[0].li_signo     = 0;
                        list_array[0].li_nstride   = 1;
                        list_array[0].li_filstride = 0;
                        list_array[0].li_memstride = 0;
                        if ( XRCALL(llfio, listiortn)LC_WAIT, list_array,
                                1, stat) < 0)
                                return(ERR);
                        if (bc->sw.sw_error != 0) {
				bc->sw.sw_count += saveamt;
                                _SETERROR(stat, bc->sw.sw_error, 0);
                                return(ERR);
                        }
#endif	/* NOT _LITTLE_ENDIAN */
		}

		else {
			/*
			 * Seek to proper location
			 */
			if (XRCALL(llfio,seekrtn) llfio, bytoff, SEEK_SET, stat) == ERR)
				return(ERR);
			/*
			 * Start a synchronous write.
			 */
			ret = XRCALL(llfio,writertn) llfio, CPTR2BP(bufptr),
					 tbytes, &bc->sw, PARTIAL, &ubc);
			if (ret == ERR) {
				bc->sw.sw_count += saveamt;
				ERETURN(stat,bc->sw.sw_error,0);
			}
		}	
#ifdef __mips
		bytoff += tbytes;
		bufptr += tbytes;
		saveamt += bc->sw.sw_count;
		tbytes = bytleft;
	    } while(bytleft > 0);
	    bc->sw.sw_count = saveamt;
#endif
	    for (i=0; i<nblk; i++) {
		bc[i].flags &= ~CCH_DIRTY;	/* clear dirty flag */
	    }
	}

	end_of_data = BYTES2BITS(bytoff + tbytes);	/* bit offset in file */
							/* of the end of the  */
							/* data being written */
	if (end_of_data > *eof)
		*eof = end_of_data;

	CCH_DEBUG(("_cch_wrabuf EX: returning %d\n",ret));
	return(0);
}
