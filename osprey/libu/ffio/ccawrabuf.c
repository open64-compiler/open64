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


#pragma ident "@(#) libu/ffio/ccawrabuf.c	92.3	10/25/99 11:30:27"


#include <ffio.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#if !defined(__mips) && !defined(_LITTLE_ENDIAN)
#include <sys/listio.h>
#else
#include "listio_mips.h"
#endif
#include "ccaio.h"

struct ffsw_link {
	struct ffsw_link *next_ffsw;
	struct ffsw_ext sw;
};

struct ffsw_link _GL_extra_ffsw;
plock_t _GL_extra_lock;

/*
 * _cca_wrabuf
 *
 * Flushes to the underlying ffio layer one cache page buffer.  
 * The ffsw structure pointed to by stat receives the synchronous or
 * asynchronous completion status.
 *
 * Return value:
 *
 *	On normal completion 0 is returned. -1 is returned if an error is
 *	encountered, with the status set in stat.
 *
 */
_cca_wrabuf(
struct cca_f *cca_info,	/* point to cache info */
struct fdinfo *llfio,	/* fdinfo pointer for underlying layer */
struct cca_buf *bc,	/* buffer control block */
int bytes, 		/* number of bytes per page buffer */
off_t bytoff, 		/* byte offset within file */
char syncasync, 	/* not used now 's' for sync request, 'a' for 
			 * async */
struct ffsw *stat)	/* io completion status structure */
{
	int	i;
	int	ret;
	off_t	end_of_data;
	int64	nsector, sector_unsynced;
	int64	current_sector, start_sector;
	int	sect_per_pg;
	int64	file_sector;
	off_t   file_byte;
	size_t	num_bytes;
	size_t	num_written;
	off_t	orig_byte_pos;
	int64	cache_bit_offset;
	bitptr	source_bit_ptr;
	int	num_sectors_unsynced;
	int	num_sectors_written;
	int	write_full_page;
	int	num_segments;
	off_t	page_start_bit;
	int64	last_sector_to_write;
	int	segment_num;
	struct ffsw_ext *stat_ptr;
	struct ffsw_ext *previous_stat;
	struct fflistreq *list_array;
	struct fflistreq singllist;
	struct ffsw_link *this_ffsw;
	struct ffsw_link *last_ffsw;
	int 	rbyte;

	sect_per_pg = cca_info->sect_per_pg;
	last_sector_to_write = sect_per_pg - 1;
	write_full_page = FALSE;

	/* all sectors past fsize can be flagged synced */
	page_start_bit = BYTES2BITS(bc->file_page.parts.page_number *
			 	    cca_info->byte_per_pg);
	if ( (page_start_bit ) > cca_info->fsize ) {
		goto no_work;
	}
	else if ( (page_start_bit + BYTES2BITS(cca_info->byte_per_pg)) >
	    	  cca_info->fsize ) {

		int64	last_file_sector;
		int64	first_sector_to_clear;

		last_file_sector = (cca_info->fsize - 1) /
				   (cca_info->bits_per_sect) + 1;
		first_sector_to_clear = last_file_sector -
					( (bc->file_page.parts.page_number) *
		    			  sect_per_pg );
		last_sector_to_write = first_sector_to_clear - 1;

		for (current_sector = first_sector_to_clear;
		     current_sector < cca_info->sect_per_pg;
		     current_sector++) {
			_CLRBIT(current_sector, bc->unsynced_sectors);
		}
	}

	if ( cca_info->optflags.pw == FALSE ) {
		num_segments = 1;
		write_full_page = TRUE;
	}
	else {
		num_segments       = 0;
		nsector             = 0;
		num_sectors_unsynced = 0;
		for ( current_sector = 0; 
		      current_sector < last_sector_to_write + 1; 
		      current_sector++) {

			sector_unsynced = _GETBIT( current_sector,
						   bc->unsynced_sectors );
			if ( sector_unsynced ) 
				nsector ++;
			if ( (sector_unsynced == FALSE) || 
			    (current_sector == last_sector_to_write) ) {
				if ( nsector ) {
					num_segments ++;
					num_sectors_unsynced += nsector;
					nsector = 0;
				}
			}
		}

		if ( num_segments == 0 ) {
			goto no_work;
		}

		/* check if all sectors are unsynced */
		if ( num_sectors_unsynced == last_sector_to_write + 1 ) {
			num_segments = 1;
			write_full_page = TRUE;
		}
		else {
			if ( bc->pre_init == FALSE ) {
				write_full_page = FALSE;
			}
			else {
				if ( num_segments <= 1 )
					write_full_page = FALSE;
				else if ( num_segments == 2 ) {
					write_full_page = FALSE;
				}
				else {
					num_segments = 1;
					write_full_page = TRUE;
				}
			}
		}

	}
	nsector = 0;
	start_sector = 0;
	num_sectors_written = 0;

	if ( cca_info->optflags.do_listio ) {
		list_array = calloc( num_segments, sizeof(struct fflistreq ));
		if ( list_array == NULL ) {
			/*
			 * Quietly deactivate listio calls from now on.
			 */  
			cca_info->optflags.do_listio = FALSE;
		}
	}

	segment_num = 0;
	previous_stat = NULL;
	for ( current_sector = 0;
	      current_sector < last_sector_to_write + 1;
	      current_sector++) {

		if ( write_full_page )  /* force the whole page out */ {
			start_sector = 0;
			nsector = last_sector_to_write + 1;
			current_sector = last_sector_to_write;
		}
		else {
			sector_unsynced = _GETBIT( current_sector,
						   bc->unsynced_sectors );

			if ( sector_unsynced ) {
				nsector = nsector + 1;
				if ( nsector == 1 ) 
					start_sector = current_sector;
			}
		}

		if ( (sector_unsynced == FALSE ||
		      current_sector == last_sector_to_write) && 
		     nsector > 0) {

			file_sector = bc->file_page.parts.page_number *
			    sect_per_pg + start_sector;
			file_byte = BITS2BYTES(file_sector *
					cca_info->bits_per_sect);
			num_bytes = BITS2BYTES(nsector *
					cca_info->bits_per_sect);
			num_written = num_bytes;
			orig_byte_pos = file_byte;
			cache_bit_offset = start_sector *
					cca_info->bits_per_sect;

			source_bit_ptr = INC_BPTR(bc->buf, cache_bit_offset);

#ifdef SDS_SUPPORTED
			if (cca_info->optflags.sds) {
				/* Handle non-well-formed parts of this i/o */
				if (file_byte & (cca_info->st_blksize -1)) {
					/* Write out the portion of the file */
					/* that precedes the next sector bdry */
					if (cca_info->is_welfrm) {
						_SETERROR(stat, FDC_ERR_GRAN, 0);
						return(ERR);	
					}
					rbyte = MIN(num_bytes, ((file_byte +
						(cca_info->st_blksize -1)) &
						~(cca_info->st_blksize -1)) -
						file_byte);
					ret = _cca_fdwrite_sds(rbyte,
						source_bit_ptr, file_byte,
						cca_info, stat);
					if (ret < 0) {
					   if (cca_info->optflags.do_listio) 
					      free(list_array);
					   return(ERR);
					}
					file_byte += rbyte;
					num_bytes -= rbyte;
					source_bit_ptr = INC_BPTR(source_bit_ptr, 
						rbyte << 3);
				}
				if (num_bytes & (cca_info->st_blksize -1)) {
					/* we're not reading a sector */
					/* multiple of bytes. */
					if (cca_info->is_welfrm) {
						_SETERROR(stat, FDC_ERR_GRAN, 0);
						return(ERR);	
					}
					rbyte = num_bytes - (num_bytes &
						~(cca_info->st_blksize -1 ));
					ret = _cca_fdwrite_sds(rbyte,
						INC_BPTR(source_bit_ptr,
						((num_bytes - rbyte)<<3)),
						file_byte + num_bytes - rbyte,
						cca_info, stat);
					if (ret < 0) {
					   if (cca_info->optflags.do_listio) 
					      free(list_array);
					   return(ERR);
					}
							
					num_bytes -= rbyte;
					
				}
				if (num_bytes == 0) {
					num_segments--;
					/* we've written all the data in */
					/* this segment */
					goto done_writing;
				}
			}
#endif
			if ( segment_num == 0 ) {
				stat_ptr = &bc->sw;
			}
			else {
				stat_ptr = NULL;
				this_ffsw = &(_GL_extra_ffsw);
				MEM_LOCK(&_GL_extra_lock);
				while (this_ffsw) {
					if ( this_ffsw->sw.inuse == NULL ) {
						stat_ptr = &this_ffsw->sw;
						break;
					}
					last_ffsw = this_ffsw;
					this_ffsw = this_ffsw->next_ffsw;
				}
				if ( stat_ptr == NULL ) {
					int	i;
					this_ffsw = calloc(5,
					     sizeof(struct ffsw_link));
					if ( this_ffsw == NULL ) {
						MEM_UNLOCK(&_GL_extra_lock);
						ERETURN(stat, FENOMEMY, 0);
					}
					stat_ptr = &this_ffsw->sw;
					last_ffsw->next_ffsw = this_ffsw;

					for (i = 0; i < 4; i++) {
						this_ffsw->next_ffsw =
						    this_ffsw + 1;
						this_ffsw++;
					}
				}
			}

			CLRFFSTAT(*stat_ptr);
			stat_ptr->bytes_req = num_bytes;
			stat_ptr->inuse     = &(cca_info->bytes_written_by_cca);
			/* in use writing file_page to llfio */
			if ( segment_num != 0 ) {
				MEM_UNLOCK(&_GL_extra_lock);
			}
			stat_ptr->rw_mode   = FFSW_WRITING;
			stat_ptr->file_page = bc->file_page;
			stat_ptr->llfio     = llfio;

			if (cca_info->optflags.do_listio) {
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
           /* this needs work for MIPS - in particular, the following: */
           /* list_array[0].li_fildes    = (int)llfio; */
           /* causes warnings for 64-bit compiles */
           ERETURN(stat,FDC_ERR_INTERR,0);
#else

				struct fflistreq *lreq;

				lreq = &list_array[segment_num];

				lreq->li_opcode    = LO_WRITE;
				lreq->li_flags     = LF_LSEEK;
				lreq->li_offset    = file_byte;
				lreq->li_fildes    = (int)llfio;
				lreq->li_buf       = BPTR2CP(source_bit_ptr);
				lreq->li_nbyte     = num_bytes;
				lreq->li_status    = (struct ffsw *)stat_ptr;
				lreq->li_signo     = 0;
				lreq->li_nstride   = 1;
				lreq->li_filstride = 0;
				lreq->li_memstride = 0;
#endif
			}
#if	defined(__mips)
			else if (cca_info->optflags.do_sylistio) {
				struct fflistreq *lreq;
				/* we can't issue more than 1 listio */
				/* at a time */
				lreq = &singllist;
				lreq->li_opcode    = LO_WRITE;
				lreq->li_flags     = LF_LSEEK;
				lreq->li_offset    = file_byte;
				lreq->li_ffioptr     = llfio;
				lreq->li_buf       = BPTR2CP(source_bit_ptr);
				lreq->li_nbyte     = num_bytes;
				lreq->li_status    = (struct ffsw *)stat_ptr;
				lreq->li_signo     = 0;
				lreq->li_nstride   = 1;
				lreq->li_filstride = 0;
				lreq->li_memstride = 0;
				DO_LISTIO( llfio, LC_START, lreq, 1, stat, ret)
				if ( ret == ERR ) {
				   /* mark request in error */
				   lreq->li_status->sw_stat = FFERR;
				   lreq->li_status->sw_flag = 1;
				   return( ERR );
				}
			}
#endif
			else {
				int	ubc;
				
				if( XRCALL(llfio, seekrtn) llfio,
				     file_byte, SEEK_SET, stat) == ERR)
					return(ERR);

				ubc = 0;
				if( XRCALL(llfio, writeartn) llfio,
				     source_bit_ptr, (size_t)num_bytes, 
				     (struct ffsw *)stat_ptr,
				     PARTIAL, &ubc) == ERR) {
					ERETURN(stat, stat_ptr->sw_error, 0); 
				}
			}


			if ( previous_stat != NULL ) 
				previous_stat->next = stat_ptr;
			previous_stat = stat_ptr;
			segment_num ++;
done_writing:
			end_of_data = BYTES2BITS(orig_byte_pos + num_written);
			if (end_of_data > cca_info->feof) {
				cca_info->feof = end_of_data;
			}
			num_sectors_written += nsector;
			nsector = 0;
		}
	}
	if ( cca_info->optflags.do_listio) {
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
		ERETURN(stat,FDC_ERR_INTERR,0); /* should never happen */
#else
		int	ret;
		if (num_segments > 0) {
			DO_LISTIO( llfio, LC_START, list_array, num_segments,
			stat, ret)
			if ( ret == ERR ) {
				for (i=0 ; i<num_segments; i++) {
				   /* mark all request in error */
				   list_array[i].li_status->sw_stat = FFERR;
				   list_array[i].li_status->sw_flag = 1;
				}
				free(list_array);
				return( ERR );
			}
		}
		free(list_array);
#endif
	}

	if ( num_sectors_written != sect_per_pg )
		cca_info->partial_pages_written ++;

no_work :
	bc->flags &= ~CCA_DIRTY;

	for (i = 0; i < cca_info->dirty_sectwds; i++)
		bc->unsynced_sectors[i] = 0;

	return(0);
}

#ifdef SDS_SUPPORTED
/*
 * Returns: -1 if error. Otherwise, number of bytes written
 */
_cca_fdwrite_sds(
	int rbyte,			/* number of bytes to write */
	bitptr rbuf,			/* points to sds space */
	int bytoff,  			/* offset in file */
	struct cca_f    *cca_info,      /* point to cache info */
        struct ffsw *stat
)
{
	struct fflistreq list_array[1];
        int bytes_written;
        struct ffsw iosw;
        int ret;
        struct fdinfo	*fd_llfio;		
	int ubc;
	long sds_bit_offset;
	int blocks_to_read;
	char *sdsbuf;
	ubc = 0;


	/* This write is not well-formed. */
	/* Read the data from sds into a local buffer.*/

	sdsbuf = malloc((rbyte + BYTPBLOCK -1) & ~(BYTPBLOCK-1));
	if (sdsbuf == NULL) {
		ERETURN(stat, FDC_ERR_NOMEM, 0);
	}

	sds_bit_offset = SUBT_BPTR(rbuf, WPTR2BP(0));
	blocks_to_read = BYTES2BLOCKS(rbyte + BYTPBLOCK - 1);
	if (ssread(sdsbuf, BITS2BLOCKS(sds_bit_offset), blocks_to_read) < 0){
		_SETERROR(stat, errno, 0);
		goto errret;
	}

	/* Write from the local buffer to the file, using the front door */
	/* file descriptor. */
	fd_llfio = cca_info->frontdoorfio;
	if (cca_info->optflags.do_listio) {
		list_array[0].li_opcode    = LO_WRITE;
		list_array[0].li_flags     = LF_LSEEK;
		list_array[0].li_offset    = bytoff;
		list_array[0].li_fildes    = (int)(fd_llfio);
		list_array[0].li_buf       = sdsbuf;
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
		bytes_written = list_array[0].li_status->sw_count;
	}
	else {
		ret = XRCALL(fd_llfio,seekrtn) fd_llfio, bytoff, SEEK_SET, stat);
		if (ret == ERR) goto errret;

		ret = XRCALL(fd_llfio,writertn) fd_llfio, sdsbuf, rbyte, 
					  stat, PARTIAL, &ubc);
		bytes_written = ret;
	}
	if (ret < 0 ) 
		goto errret;
        if (bytes_written != rbyte) {
		_SETERROR(stat, FDC_ERR_WRTERR, 0);
		goto errret;
	}
	free(sdsbuf);
      	return(bytes_written); 
errret:
	free(sdsbuf);
	return(ERR);
}
#endif
