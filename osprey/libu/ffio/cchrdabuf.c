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


#pragma ident "@(#) libu/ffio/cchrdabuf.c	92.2	10/07/99 22:13:28"

#include <stdio.h>
#include <stdlib.h>
#include <ffio.h>
#include "cchio.h"
#include <sys/types.h>
#include <cray/nassert.h>
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
#include <sys/listio.h>
#elif	defined(__mips)
#include "listio_mips.h"
#endif

/*
 * _cch_rdabuf
 *
 * Reads from a specified address in the underlying ffio layer into one or 
 * more page buffers.  The ffsw structure pointed to by stat receives the 
 * synchronous or asynchronous completion status.
 *
 * When nblk > 1, _cch_rdabuf assumes that all page buffers are read from 
 * contiguous pages of the file.
 *
 * Return value:
 *
 *	On normal completion a status of >= 0 is returned. -1 is returned if 
 *	an error is encountered, with the status set in stat.
 *
 * Side effects:
 *
 *	The affected buffer control blocks are placed in CCH_READING state
 *	if the request is asynchronous.
 */
int
_cch_rdabuf(
struct cch_f	*cch_info,	/* point to cache info */
struct fdinfo	*llfio,		
struct cch_buf	*bc,		/* buffer control block structure */
int		bytes,		/* number of bytes per page buffer */
off_t		bytoff,		/* byte offset within file */
int64		nblk,		/* number of contiguous page buffers to read */
char		syncasync,	/* 's' for synchronous, 'a' for asynchronous */
struct ffsw	*stat		/* io completion status structure */
)
{
	int	i;
	ssize_t	ret;
	int	ubc;
	struct fflistreq list_array[1];
	size_t saveamt = 0;
	char *bufptr;
	off_t	offset;
	size_t	nbytes;
	size_t	bytleft;

	CCH_DEBUG(("_cch_rdabuf EN: bytes=%d (o%o)  bytoff=%d (o%o) \n",
		bytes,bytes,bytoff,bytoff));
	ubc = 0;
#ifdef  DEBUG
	/* for debug only */
	if (nblk > 1) {
		for (i = 0; i < nblk; i++){
			if (bc[i].firstdata || bc[i].lastdata)
				abort();
		}
	}
#endif
	if (syncasync == 'a') {
		/* This path is currently never taken */
		/* If we ever do start using it, we should */
		/* add code similar to what was added in */
		/* the synchronous case - check firstdata and */
		/* lastdata. That's why I'm adding the abort here */
		/* We'll also need to verify that the write doesn't */
		/* exceed the max allowed when using O_DIRECT on MIPS */
		/* assert(!bc->firstdata && !bc->lastdata); */
		abort();
/*
 *		Seek to the proper location.
 */
		if(XRCALL(llfio,seekrtn) llfio, bytoff, SEEK_SET, stat) == ERR)
			return(ERR);
		/*
		 * Start an asynchronous read.  Request size is always a 
		 * multiple of a cache page size. 
		 */
		CLRFFSTAT(bc->sw);
		ret = XRCALL(llfio,readartn) llfio, bc->buf, 
					     (size_t)bytes * nblk,
					     &bc->sw, PARTIAL, &ubc);
		if (ret == ERR) {
			ERETURN(stat,bc->sw.sw_error,0);
		}

		bc[0].lnkcnt = nblk;
		for (i=0; i<nblk; i++) {
			bc[i].flags |= CCH_READING;
			bc[i].flags |= CCH_VALIDBUFFER;
			bc[i].firstdata = bc[i].lastdata = 0;
			bc[i].lnk    = i;
		}
	}
	else {
		short readtail = 0; 
		short copyback = 0; 
		if (bc->firstdata || bc->lastdata) {
			/* this can only happen when nblk == 1 */
			assert(nblk==1);
			if (bc->firstdata){
				/*
				 * The buffer has: invalid data, followed by
				 * dirty data, possibly followed by more
				 * invalid data.
				 */
				bufptr = BPTR2CP(bc->buf);	
				offset = bytoff;
				nbytes = bc->firstdata/8;
				if (bc->lastdata/8 != bytes) {
					/* we have dirty data in the middle */
					/* of the buffer. If the amount is */
					/* small enough, copy to another */
					/* temporary buffer, read the entire */
					/* buffer, then copy the dirty data */
					/* back. Otherwise, read the head */
					/* and tail of the buffer in 2 chunks */
					if (bc->lastdata - bc->firstdata <=
						_CCH_SMSIZ*8){
						copyback = 1;
						nbytes = bytes;
						memcpy(cch_info->savearea,
							BPTR2CP(bc->buf)+bc->firstdata/8,
							(bc->lastdata-bc->firstdata)/8);
					}
					else {
						readtail = 1;
					}
				}
			}
			else {
				/* the first part of the buffer contains */
				/* valid dirty data. */
				offset = bc->lastdata/8 + bytoff;
				bufptr = BPTR2CP(bc->buf)+bc->lastdata/8;
				nbytes = bytes - bc->lastdata/8;
			}
		}	
		else {
			/* just read the buffer */
			offset = bytoff;
			bufptr = BPTR2CP(bc->buf);
			nbytes = (size_t)bytes * nblk;
		}

#ifdef __mips
		bytleft = nbytes;
		do  {
			/* The size of O_DIRECT requests is limited. */
			/* If we exceed that, break it up into multiple */
			/* requests. */
			if (cch_info->odirect && nbytes > cch_info->maxiosize){
				/* always work in multiples of pages */
				nbytes = (size_t) bytes;
				while (nbytes + (size_t)bytes <= cch_info->maxiosize) {
					nbytes += (size_t) bytes;
				};
			}
			bytleft -= nbytes;
#endif
			if (cch_info->do_sylistio) {
#if	!defined(_LITTLE_ENDIAN)
				list_array[0].li_opcode    = LO_READ;
				list_array[0].li_flags     = LF_LSEEK;
				list_array[0].li_offset    = offset;
				list_array[0].li_buf       = bufptr;
				list_array[0].li_nbyte     = nbytes;
#ifdef __mips
				list_array[0].li_ffioptr   = llfio;
#else
				list_array[0].li_fildes    = (int)llfio;
#endif
				list_array[0].li_status    = (struct ffsw *)(&bc->sw);
				list_array[0].li_signo     = 0;
				list_array[0].li_nstride   = 1;
				list_array[0].li_filstride = 0;
				list_array[0].li_memstride = 0;
				if ( XRCALL(llfio, listiortn)LC_WAIT, 
					list_array, 
					1, stat) < 0)
					return(ERR);
				if (bc->sw.sw_error != 0) {
					bc->sw.sw_count += saveamt;
					_SETERROR(stat, bc->sw.sw_error, 0);
					return(ERR);
				}
#endif	/* NOT(little endian) */
			}
			else {
				/*
			         * Seek to the proper location.
			         */
			        if( XRCALL(llfio,seekrtn) llfio, offset, 
					SEEK_SET, stat) == ERR)
			        	return(ERR);
			        /*
			         * Start a synchronous read.  
			         */
			        ret = XRCALL(llfio,readrtn) llfio, 
					CPTR2BP(bufptr),
					nbytes,
					&bc->sw, PARTIAL, &ubc);
			        if (ret == ERR) {
					bc->sw.sw_count+= saveamt;
			    		ERETURN(stat,bc->sw.sw_error,0);
			        }
			}
#ifdef __mips
			offset += nbytes;
			bufptr += nbytes;
			saveamt += bc->sw.sw_count;
			nbytes = bytleft;
		} while (bytleft > 0);
		bc->sw.sw_count = saveamt;
#endif
		if (bc->firstdata || bc->lastdata) {
			saveamt = bc->sw.sw_count;
			if (bc->firstdata) {
			    if (saveamt < bc->firstdata/8){
				ERETURN(stat,FDC_ERR_RDERR, saveamt);
			    }
			    if (copyback) {
				memcpy(BPTR2CP(bc->buf)+bc->firstdata/8,
				    cch_info->savearea,
				    (bc->lastdata-bc->firstdata)/8);
				if (saveamt < (bc->lastdata/8)) {
					/* This should not happen */
					saveamt = bc->lastdata/8;
				}
			    }
			    else
			        saveamt += (bc->lastdata - bc->firstdata)/8;
			}
			if (readtail) {
				/* we need to read the tail of the buffer */
				offset = bytoff + bc->lastdata/8;
				bufptr = BPTR2CP(bc->buf)+bc->lastdata/8;
				nbytes = bytes - bc->lastdata/8;
				if (cch_info->do_sylistio) {
#if	!defined(_LITTLE_ENDIAN)
					list_array[0].li_offset    = offset;
					list_array[0].li_buf       = bufptr;
					list_array[0].li_nbyte     = nbytes;
					/* the other fields in list_array */
					/* should be set up already */
					if ( XRCALL(llfio, listiortn)LC_WAIT,
						list_array, 
						1, stat) < 0)
						return(ERR);
					if (bc->sw.sw_error != 0) {
						_SETERROR(stat, 
							bc->sw.sw_error, 0);
						return(ERR);
					}
#endif
				}
				else {
					/*
				         * Seek to the proper location.
				         */
		       			if( XRCALL(llfio,seekrtn) llfio,
					 offset,
					 SEEK_SET, stat) == ERR)
			        	return(ERR);
				         /*
				          * Start a synchronous read.  
				          */
				         ret = XRCALL(llfio,readrtn) llfio,
					    CPTR2BP(bufptr),
					    nbytes,
					    &bc->sw, PARTIAL, &ubc);
				         if (ret == ERR) {
				    	    ERETURN(stat,bc->sw.sw_error,0);
		       			  }
				}
				bc->sw.sw_count += saveamt;
			}
			else 
				bc->sw.sw_count = saveamt;

			bc->firstdata = bc->lastdata = 0;
		}
		for (i=0; i<nblk; i++) {
			bc[i].flags |= CCH_VALIDBUFFER;
			bc[i].firstdata = bc[i].lastdata = 0;
		}
	}

	CCH_DEBUG(("_cch_rdabuf EX: returning %d\n",ret));
	return(0);
}
