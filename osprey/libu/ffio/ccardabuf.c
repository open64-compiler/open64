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


#pragma ident "@(#) libu/ffio/ccardabuf.c	92.2	10/11/99 15:29:41"


#include <stdio.h>
#include <sys/types.h>
#if !defined(__mips) && !defined(_LITTLE_ENDIAN)
#include <sys/listio.h>
#elif	defined(__mips)
#include "listio_mips.h"
#endif
#include <ffio.h>
#include <stdlib.h>
#include "ccaio.h"
/*
 * _cca_rdabuf
 *
 * Reads from a specified address in the underlying ffio layer into one or 
 * more page buffers.  The ffsw structure pointed to by stat receives the 
 * synchronous or asynchronous completion status.
 *
 * Return value:
 *
 *	On normal completion a status of >= 0 is returned. -1 is returned if 
 *	an error is encountered, with the status set in stat.
 *
 */
int
_cca_rdabuf(
struct cca_f    *cca_info,      /* point to cache info */
struct fdinfo	*llfio,		
struct cca_buf	*bc,		/* buffer control block structure */
int		bytes,		/* number of bytes per page buffer */
off_t		bytoff,		/* byte offset within file */
char		syncasync,	/* 's' for synchronous, 'a' for asynchronous */
struct ffsw	*stat)		/* io completion status structure */
{
	int	i;
	int	ret;
	int	ubc;
	struct fflistreq list_array[1];
	int64 page_start_bit;
	int64 offset_to_eof;
	bitptr rbuf;
	int rbyte;
	ssize_t rret;

	ubc = 0;
	bc->sw.sw_sptr = 0;

	page_start_bit = bc->file_page.parts.page_number *
			 (uint64)cca_info->byte_per_pg << 3;

/*
 *	Deal with EOF.
 */
	offset_to_eof = ((cca_info->feof >> 3) - (page_start_bit >> 3));
	if( bytes > offset_to_eof) {
	   int xbytes = MAX(offset_to_eof, 0);
	   /*
	    * Zero the part of the buffer beyond feof.  (Maybe the entire page)
	    */
	   _clear_buffer(INC_BPTR(bc->buf, xbytes<<3),
			 cca_info->optflags.sds,
			 cca_info->byte_per_pg - xbytes,
			 stat);

	   if (offset_to_eof <= 0) 
	      return(0);		/* all done, no data to read from file*/
	   else if (cca_info->optflags.sds)
		bytes = offset_to_eof;
	}

	rbuf = bc->buf;
#ifdef SDS_SUPPORTED
	if( cca_info->optflags.sds) {

           /* Make sure that our back door I/O will be well-formed. */
           /* Use the frontdoor to read any portions that are not well-formed.*/

   	   int final_offset;

   	   final_offset = bytoff + bytes;
   	   if (bytoff & (cca_info->st_blksize -1 )) {
   	      /* We're not on a sector boundary. */
   	      /* Read in the portion of the file before the next sector bdry */
   	      if (cca_info->is_welfrm) {
                 _SETERROR(stat, FDC_ERR_GRAN, 0);
                 return(ERR);
   	      }

   	      rbyte = MIN(bytes, ((bytoff + (cca_info->st_blksize-1)) &
   	         ~(cca_info->st_blksize -1 )) - bytoff);
   	      ret = _cca_fdread_sds(rbyte, rbuf, bytoff, cca_info, stat);
   	      if (ret < 0) {
                 return(ERR);
   	      }
              bytoff += rbyte;
              bytes -= rbyte;
              cca_info->bytes_read_by_cca += ret;
              rbuf = INC_BPTR(rbuf, rbyte<<3);
	   }
   	   if (bytes & (cca_info->st_blksize -1 )) {
   	      /* We're not reading a sector multiple of bytes. */
   	      /* We are on a sector boundary, however. */
   	      /* Read the "tail" */
   	      if (cca_info->is_welfrm) {
                 _SETERROR(stat, FDC_ERR_GRAN, 0);
                 return(ERR);
   	      }

              rbyte = bytes - (bytes & ~(cca_info->st_blksize -1));
   	      ret = _cca_fdread_sds(rbyte, INC_BPTR(rbuf, ((bytes - rbyte)<<3)),
                 bytoff + bytes - rbyte, cca_info, stat);
   	      if (ret < 0) {
                 return(ERR);
   	      }
              bytes -= rbyte;
              cca_info->bytes_read_by_cca += ret;
   	   }
           if (bytes == 0) {
             /* seek the backdoor file position so it will be correct */
	      ret = XRCALL(llfio,seekrtn) llfio, final_offset, SEEK_SET, stat);
   	      if (ret == ERR) return(ERR);
              return (0);
           }
	}
#endif
/*
 *	Do setup for an asynchronous read request.
 */
	if( syncasync == 'a' ) {
	   CLRFFSTAT(bc->sw);
	   bc->sw.inuse   = &(cca_info->bytes_read_by_cca);
	   bc->sw.rw_mode = FFSW_READING;  /* the ffsw is watching a read */
	   bc->sw.file_page = bc->file_page;  /* the file_page being read */
	   bc->sw.llfio   = llfio;  /* the ffsw is used for async reads */
	}

/*
 *	Choose the listio or seek/read method of I/O.
 */
	if (cca_info->optflags.do_listio || (cca_info->optflags.do_sylistio )) {
#if	!defined(_LITTLE_ENDIAN)
	   list_array[0].li_opcode    = LO_READ;
	   list_array[0].li_flags     = LF_LSEEK;
	   list_array[0].li_offset    = bytoff;
#ifdef __mips
	   list_array[0].li_ffioptr   = llfio;
#else
	   list_array[0].li_fildes    = (int)llfio;
#endif
	   list_array[0].li_buf       = BPTR2CP(rbuf);
	   list_array[0].li_nbyte     = bytes;
	   list_array[0].li_status    = (struct ffsw *)(&bc->sw);
	   list_array[0].li_signo     = 0;
	   list_array[0].li_nstride   = 1;
	   list_array[0].li_filstride = 0;
	   list_array[0].li_memstride = 0;

	   if( syncasync == 'a' ) {
	      DO_LISTIO( llfio, LC_START, list_array, 1, stat, ret );
	   }
	   else {
	      DO_LISTIO( llfio, LC_WAIT , list_array, 1, stat, ret );
	      if (bc->sw.sw_error != 0) {
                 _SETERROR(stat, bc->sw.sw_error, 0);
                 return(ERR);
	      }
	      else
	         cca_info->bytes_read_by_cca += list_array[0].li_status->sw_count;
	   }

	   if (ret <= 0) return(ERR);
#endif	/* NOT (little endian) */
	}
	else {
	   if(XRCALL(llfio,seekrtn) llfio, bytoff, SEEK_SET, stat) == ERR)
	   	return(ERR);

	   if (syncasync == 'a') {
		   rret = XRCALL(llfio,readartn) llfio, rbuf, (size_t)bytes,
			        (struct ffsw *)(&bc->sw), PARTIAL, &ubc);
	   }
	   else {
	      bc->sw.llfio = NULL;  /* the ffsw is not use for sync reads */
	      rret = XRCALL(llfio,readrtn) llfio, rbuf, (size_t)bytes, 
		(struct ffsw*)(&bc->sw),PARTIAL, &ubc);
	      if( rret > 0 ) cca_info->bytes_read_by_cca += rret;
	   }
	   if (rret == ERR) {
		ERETURN(stat,bc->sw.sw_error,0);
	   }
	}


	return(0);
}
#ifdef SDS_SUPPORTED
/*
 * Returns: -1 if error. Otherwise, number of bytes read
 */
_cca_fdread_sds(
	int rbyte,			/* number of bytes to read */
	bitptr rbuf,			/* points to sds space */
	int bytoff,  			/* offset in file */
	struct cca_f    *cca_info,      /* point to cache info */
        struct ffsw *stat
)
{
	struct fflistreq list_array[1];
        int bytes_read;
        struct ffsw iosw;
        int ret;
        struct fdinfo	*fd_llfio;		
	int ubc;
	long sds_bit_offset;
	int blocks_to_read;
	char *sdsbuf;
	char *localbuf;
	ubc = 0;

	localbuf = malloc(rbyte);
	if (localbuf == NULL){
		ERETURN(stat, FDC_ERR_NOMEM, 0);
	}
	sdsbuf = malloc((rbyte + BYTPBLOCK -1) & ~(BYTPBLOCK-1));
	if (sdsbuf == NULL) {
		free(localbuf);
		ERETURN(stat, FDC_ERR_NOMEM, 0);
	}
	/* This read is not well-formed. */
	/* Read into a local buffer with the front door file descriptor. */
	/* Then transfer the data from the local buffer to sds. */
	fd_llfio = cca_info->frontdoorfio;
	if (cca_info->optflags.do_listio) {
		list_array[0].li_opcode    = LO_READ;
		list_array[0].li_flags     = LF_LSEEK;
		list_array[0].li_offset    = bytoff;
		list_array[0].li_fildes    = (int)(fd_llfio);
		list_array[0].li_buf       = localbuf;
		list_array[0].li_nbyte     = rbyte;
		list_array[0].li_status    = &iosw;
		list_array[0].li_signo     = 0;
		list_array[0].li_nstride   = 1;
		list_array[0].li_filstride = 0;
		list_array[0].li_memstride = 0;

		DO_LISTIO( cca_info->frontdoorfio, LC_WAIT , list_array, 1,
			stat, ret );
		if (iosw.sw_error != 0) {
			_SETERROR(stat, iosw.sw_error, 0);
			goto errret;
		}
		bytes_read = list_array[0].li_status->sw_count;
	}
	else {
		ret = XRCALL(fd_llfio,seekrtn) fd_llfio, bytoff, SEEK_SET, stat);
		if (ret == ERR) goto errret;

		ret = XRCALL(fd_llfio,readrtn) fd_llfio, localbuf,(size_t)rbyte, 
					  stat, PARTIAL, &ubc);
		bytes_read = ret;
	}
	if (ret < 0 ) 
		goto errret;
        if (bytes_read != rbyte) {
		_SETERROR(stat, FDC_ERR_RDERR, 0);
		goto errret;
	}
	/* Now transfer the data to SDS. Our SDS address will be on a */
	/* block boundary. First read what is currently in sds. */
	sds_bit_offset = SUBT_BPTR(rbuf, WPTR2BP(0));
	blocks_to_read = BYTES2BLOCKS(rbyte + BYTPBLOCK - 1);
	if (ssread(sdsbuf, BITS2BLOCKS(sds_bit_offset), blocks_to_read) < 0){
		_SETERROR(stat, errno, 0);
		goto errret;
	}
	/* Copy what we read from the file into the buffer that contains */
	/* current sds data */
	memcpy(sdsbuf, localbuf, rbyte);
	/* write the data to sds */
	if (sswrite(sdsbuf, BITS2BLOCKS(sds_bit_offset), blocks_to_read) < 0){
		_SETERROR(stat, errno, 0);
		goto errret;
	}
	free(localbuf);
	free(sdsbuf);
      	return(bytes_read); 
errret:
	free(localbuf);
	free(sdsbuf);
	return(ERR);
}
#endif
