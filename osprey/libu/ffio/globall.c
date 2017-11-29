/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


#pragma ident "@(#) libu/ffio/globall.c	92.2	10/14/99 15:22:06"


#include <assert.h>
#include <ffio.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <fortran.h>
#include <stdarg.h>
#include <malloc.h>
#include <sys/types.h>
#include <sys/fcntl.h>
#include <sys/stat.h>
#ifdef	_CRAY
#include <sys/iosw.h>
#endif
#include "globio.h"

/*
 *	Global variables.
 */
long _par_trace_lock;
struct _par_trace_info _par_trace_vars;
struct _par_trace_entry *_par_trace_buf, *_par_local_tracebuf;
_par_variables_t _par_vars;

short _glob_io_init_called;


/*
 *	Macros
 */
#ifndef MAX
#define MAX(a,b)	((a) > (b) ? (a) : (b))
#endif

/*
 *	Stub functions
 */
#ifdef	__mips
void tracebk(void) { }
#endif

#ifdef	_CRAY
#pragma _CRI duplicate _glob_write as _glob_writea
#else
#pragma weak _glob_writea = _glob_write
#endif

int 
_glob_write(
struct fdinfo   *fio,           /* ffio file descriptor. */
bitptr          bufarg,         /* bit pointer to the user's data. */
size_t          nbytes,         /* Nuber of bytes to be written. */
struct ffsw     *stat,          /* pointer to status return word */
int             fulp,           /* full or partial write mode flag */
int             *ubcp)          /* pointer to unused bit count.  On return, */
                                /* *ubcp is updated to contain the unused bit */
                                /* count in the data returned. */

{
	register short	algned4;	/* aligned on a 4 byte boundary */
	register short	first;
	register short	full_page;
	register short	hit;
	register int	amount;
	register int	count;
	register int	eof_page;
	register int	err;
	register int	max_count;
	register int	moved;
	register int	page_offset;
	register int	pgnum;
	register int	pgsize;
	register int	remote_pe;
	long		*last_word;
	long		*remote_last;
	int64_t		offset;
	struct glob_f	*glob_info = (struct glob_f *)fio->lyr_info;
        glio_arena_t	*arp = glob_info->arp;
	char		*bufptr = BPTR2CP(bufarg);
	struct par_file_data *fdptr;
	struct par_page_description *p;
	void		*remote_addr;

	fdptr	= &glob_info->fstate;
	first	= 1;
	err	= 0;
	moved	= 0;
	pgsize	= glob_info->pgsize_bytes;

	if ((glob_info->oflags & O_ACCMODE) == O_RDONLY) {
		err	= EBADF;
		goto err1_ret;
	}

	if (*ubcp != 0) {
		err	= FDC_ERR_UBC;
		goto err1_ret;
	}

	if (glob_info->oflags & O_APPEND) {
		_glio_set_lock(arp, &glob_info->fstate_lock);
		_shmem_get64(&offset, &glob_info->fstate.size, 1,
			glob_info->fstate_owner);
	}
	else
		offset	= glob_info->file_off;

	while (nbytes > 0) {
		hit	= 0;
		pgnum	= offset / pgsize;
		p	= _par_get_page_locked(fio, pgnum);
		/*
		 * It is o.k. that the shared fdata structure isn't locked.
		 * If multiple PEs are trying to write to EOF in parallel, we
		 * already have the page locked from calling
		 * _par_get_page_locked() above.
		 */
		_glio_shmem_get32(arp, fdptr, fdptr, sizeof(*fdptr)/4,
			glob_info->fstate_owner);

		remote_addr	= SHARED_BUF_ADDR(p->buf_num);
		remote_pe	= p->pe;

		if (p->flags & PG_VALID) {
			hit	= 1;
		} else {
			/*
			 * If we are writing to an invalid page that is
			 * currently in the file, we must pre-read the page,
			 * unless we are on a page boundary and we are writing
			 * at least a full page.
			 */
			if (fdptr->bytes_on_disk == 0)
				eof_page	= -1;
			else
				eof_page	= (fdptr->bytes_on_disk - 1) / 
							pgsize;
			full_page = (offset % pgsize) == 0 &&
				     nbytes >= pgsize;
			if (eof_page >= pgnum && (!full_page)) {
				/*
				 * Don't read past EOF, which causes ENXIO
				 * if Phase 2 I/O is used, even if the
				 * the environment variable MPP_AGENT_PH2IO
				 * is "on" (rather than "optimized").
				 */
				amount	= fdptr->bytes_on_disk -
				     ((offset / pgsize) * pgsize);
				if (amount > pgsize)
					amount	= pgsize;
				if (amount <= 0) 
					_par_abort(arp,"par_write: reading <= 0");
				_par_get_new_page(fio, p, amount);
			}
			if (eof_page < pgnum && (!full_page)) {
				(void) memset(glob_info->my_iobuf, 0, pgsize); 
				_glio_shmem_put32(
					arp,
					remote_addr,
					glob_info->my_iobuf,
					glob_info->pgsize / 4,
					remote_pe);
				if ((offset % 8) != 0 || ((nbytes % 8) != 0))
					/*
					 * We may need to read this data back
					 * to merge in data below.
					 */
					_glio_shmem_quiet();
			}
		}

		if (pgnum == 0)
			page_offset	= offset;
		else
			page_offset	= offset % pgsize;

		max_count	= pgsize - page_offset;

		if (max_count < nbytes)
			count	= max_count;
		else 
			count	= nbytes;

		algned4	= ((long long)bufptr % 4 == 0) &&
			  (offset % 4 == 0) &&
			  (count  % 4 == 0);

		if (algned4) {	
			_glio_shmem_put32(
				arp,
				(char *)(remote_addr) + page_offset,
				bufptr,
				count/4,
				remote_pe);
		} else {
			_glio_shmem_putmem(
				arp,
				(char *)(remote_addr) + page_offset,
				bufptr,
				count,
				remote_pe);
		}

		nbytes	= nbytes - count;
		offset	= offset + count;
		moved	= moved + count;
		bufptr	= bufptr + count;

		p->flags |= (PG_DIRTY | PG_VALID);

		glob_info->file_off	= offset;

		_glio_set_lock(arp, &glob_info->fstate_lock);
		_glio_shmem_get32(arp, fdptr, fdptr, sizeof(*fdptr)/4,
			glob_info->fstate_owner);

		if (offset > fdptr->size) {
			_glio_set_lock(arp, &glob_info->fsize_lock);
			fdptr->size	= offset;
			_glio_shmem_put32(arp, fdptr, fdptr, sizeof(*fdptr)/4,
				glob_info->fstate_owner);
			_glio_clear_lock(arp, &glob_info->fsize_lock);
		}

		if (first) {
			first		= 0;
			fdptr->nwrite	= fdptr->nwrite + 1;
		}

		fdptr->bytes_written	= fdptr->bytes_written + count;

		if (hit)
			fdptr->write_cache_hits++;
		else
			fdptr->write_cache_misses++;

		_glio_shmem_put32(arp, fdptr, fdptr, sizeof(*fdptr)/4,
			glob_info->fstate_owner);
		_glio_shmem_quiet();
		_glio_clear_lock(arp, &glob_info->fstate_lock);
		_par_unlock_page(fio, p);
	}

	if (glob_info->oflags & O_APPEND) 
		_glio_clear_lock(arp, &glob_info->fsize_lock);

	SETSTAT(stat, FFCNT, moved);
	return(moved);

err1_ret:
        ERETURN(stat, err, 0);
}

#ifdef	_CRAY
#pragma _CRI duplicate _glob_read as _glob_reada
#else
#pragma weak _glob_reada = _glob_read
#endif
int
_glob_read(
struct fdinfo   *fio,           /* ffio file descriptor. */
bitptr          bufarg,         /* bit pointer to where data is to go. */
int             nbytes,         /* Number of bytes to be read. */
struct ffsw     *stat,          /* pointer to status return word */
int             fulp,           /* full or partial read mode flag */
int             *ubcp)          /* pointer to unused bit count.  On return, */
                                /* *ubcp is updated to contain the unused bit */
                                /* count in the data returned. */
{
	register short	algned4;
	register short	hit;
	register short	first;
	register int	bytes_left;
	register int	moved;
	register int	count;
	register int	err;
	register int	max_count;
	register int	pgnum;
	register int	page_offset;
	register int	pgsize;
	register int	remain;
	register int	remote_pe;
	int64_t		offset;
	char		*bufptr = BPTR2CP(bufarg);
	struct glob_f	*glob_info = (struct glob_f *)fio->lyr_info;
        glio_arena_t	*arp = glob_info->arp;
	struct par_file_data *fdptr;
	struct par_page_description *p;

	fdptr		= &glob_info->fstate;
	err		= 0;
	moved	= 0;
	pgsize	= glob_info->pgsize_bytes;
	first		= 1;

	if ((glob_info->oflags & O_ACCMODE) == O_WRONLY) {
		err	= EBADF;
                goto err1_ret;
	}

        if (*ubcp != 0) {
                err	= FDC_ERR_UBC;
                goto err1_ret;
        }
	offset	= glob_info->file_off;
	bytes_left = nbytes;

	while (bytes_left > 0) {
		hit	= 0;
		pgnum	= offset / pgsize;
		p	= _par_get_page_locked(fio, pgnum);
		_glio_shmem_get32(arp, fdptr, fdptr, sizeof(*fdptr)/4,
			glob_info->fstate_owner);
		if (offset >= fdptr->size) {
			_par_unlock_page(fio, p);
			goto normal_ret;
		}
		if (!(p->flags & PG_VALID)) {
			register int	amount;
			/*
			 * Don't read past EOF, which causes ENXIO if Phase 2
			 * I/O is used.
			 */
			amount	= fdptr->bytes_on_disk -
				((offset / pgsize) * pgsize);
			if (amount > pgsize)
				amount	= pgsize;
			if (amount > 0) {
				_par_get_new_page(fio, p, amount);
				if (amount != pgsize)
					bzero(((char *) glob_info->my_iobuf) +
						amount,
						pgsize - amount);
			} else {
				/* this shouldn't happen */
				fprintf(stderr,"%s:\n\
    read past EOF at offset %lld. file disk size is %lld.\n\
    I/O size %d.\n", 			GLOBERRMSG,
					offset,
					(long long)fdptr->bytes_on_disk,
					bytes_left);

				_par_abort(arp, "par_read: EOF inconsistency");
			}
		} else
			hit++;
		page_offset = offset % pgsize;
		remain	= fdptr->size - offset;
		max_count = pgsize - page_offset;

		if (max_count < bytes_left)
			count	= max_count;
		else 
			count	= bytes_left;

		if (count > remain) {
			count	= remain; /* don't read past end of file */
			bytes_left	= 0;
		} else
			bytes_left	= bytes_left - count;

		offset	= offset + count;
		glob_info->file_off = offset;

		if (hit) {	/* get data from cache */
		    remote_pe	= p->pe;
		    algned4	= ((long long)bufptr % 4 == 0) &&
			      (offset % 4 == 0) &&
			      (count  % 4 == 0) ;
		    if (algned4) {	/* bypass memcpy */
			_glio_shmem_get32(
			    arp,
			    bufptr,
			    (char *)(SHARED_BUF_ADDR(p->buf_num)) + page_offset,
			    count / 4,
			    remote_pe);
		    } else {	
			_glio_shmem_getmem(
			    arp,
			    bufptr,
			    (char *)(SHARED_BUF_ADDR(p->buf_num)) + page_offset,
			    count,
			    remote_pe);

		    }
		} else		/* needed data is still in local buffer */
		    memcpy((void *) bufptr, (void *) 
			((char *) glob_info->my_iobuf + page_offset), count);

		moved	= moved + count;
		bufptr	= bufptr + count;

		_glio_set_lock(arp, &glob_info->fstate_lock);
		_glio_shmem_get32(arp, fdptr, fdptr, sizeof(*fdptr)/4,
			glob_info->fstate_owner);

		if (first) {
			first	= 0;
			fdptr->nread++;
		}

		fdptr->bytes_read	= fdptr->bytes_read + count;

		if (hit)
			fdptr->read_cache_hits++;
		else
			fdptr->read_cache_misses++;
		_glio_shmem_put32(arp, fdptr, fdptr, sizeof(*fdptr)/4,
			glob_info->fstate_owner);
		_glio_shmem_quiet();
		_glio_clear_lock(arp, &glob_info->fstate_lock);
		_par_unlock_page(fio, p);
	}

normal_ret:
	SETSTAT(stat, (moved == 0 ? FFEOD : FFCNT), moved);
	return(moved);
err1_ret:
        ERETURN(stat, err, 0);
}

/*
 *	_par_write_page
 *
 *	Writes data from a specified page to the file.  Then clears the
 *	PG_DIRTY bit.
 *
 *	Lock requirements: page must be locked 
 */
void
_par_write_page(struct fdinfo *fio, struct par_page_description *p)
{
	register int	pe;
	register long	base;
        struct glob_f	*glob_info = (struct glob_f *)fio->lyr_info;
        glio_arena_t	*arp = glob_info->arp;
	void		*remote_addr;

	pe	= p->pe;

	remote_addr = SHARED_BUF_ADDR(p->buf_num);

	_glio_shmem_get32(arp, glob_info->my_iobuf, remote_addr, 
		glob_info->pgsize_bytes / 4, pe);

	base	= p->page_num * glob_info->pgsize_bytes;

	if (_par_vars.do_tracing)
		_par_io_trace(fio, pe, p->buf_num, base, WRITE_IO);

	_par_do_io(fio->fioptr, base, glob_info->my_iobuf,
			glob_info->pgsize_bytes, WRITE_IO);
	_par_update_disk_stats(fio, base + glob_info->pgsize_bytes - 1,
			WRITE_IO, glob_info->pgsize_bytes);
	p->flags &= ~PG_DIRTY;

	return;
}	

/*
 * Read a page of the file into a page buffer.
 */
void
_par_get_new_page(struct fdinfo *fio, struct par_page_description *p,
	size_t size)
{
	register int	pe;
	register long	base;
        struct glob_f	*glob_info = (struct glob_f *)fio->lyr_info;
        glio_arena_t	*arp = glob_info->arp;
	void		*remote_addr;

	pe	= p->pe;
	remote_addr = SHARED_BUF_ADDR(p->buf_num);
	base	= p->page_num * glob_info->pgsize_bytes;
	if (_par_vars.do_tracing)
		_par_io_trace(fio, pe, p->buf_num, base, READ_IO);
	_par_do_io(fio->fioptr, base, glob_info->my_iobuf, size, READ_IO);
	_par_update_disk_stats(fio, base + size - 1, READ_IO, size);
	_glio_shmem_put32(arp, remote_addr, glob_info->my_iobuf,
		glob_info->pgsize_bytes/4, pe);
	p->flags |= PG_VALID;

	return;
}

void
_par_update_disk_stats(struct fdinfo *fio, int high_byte_offset,
			enum io_type io_direction, size_t size)
{
	register int	pgnum;
	register int	remote_pe;
	register int	remote_index;
	register int	seek_extend;
	register int	sequentially_extend;
	struct glob_f *glob_info = (struct glob_f *)fio->lyr_info;
        glio_arena_t *arp = glob_info->arp;
	struct par_file_data *fdptr = &glob_info->fstate;

	pgnum	= high_byte_offset / glob_info->pgsize_bytes;
	sequentially_extend = seek_extend = 0;

	_glio_set_lock(arp, &glob_info->fstate_lock);
	_glio_shmem_get32(arp, fdptr, fdptr, sizeof(*fdptr)/4,
		glob_info->fstate_owner);
	if (io_direction == WRITE_IO)
		fdptr->disk_bytes_written += size;
	else
		fdptr->disk_bytes_read += size;

	if (io_direction == WRITE_IO) {
		int max_page_on_disk;
		
		if (fdptr->bytes_on_disk == 0)
			max_page_on_disk = -1;
		else
			max_page_on_disk = 
			    (fdptr->bytes_on_disk - 1) / glob_info->pgsize_bytes;
		if (max_page_on_disk == pgnum - 1)
			/* trying to extend the file by one page */
			sequentially_extend = 1;
		else if (max_page_on_disk < pgnum - 1)
			seek_extend = 1;
		if (sequentially_extend || seek_extend) {
			if (sequentially_extend)
				fdptr->sequentially_extend++; 
			if (seek_extend)
				fdptr->seek_extend++; 
		} else {
			fdptr->overwrote_page++;
		}
		if (fdptr->bytes_on_disk < high_byte_offset + 1)
			fdptr->bytes_on_disk = high_byte_offset + 1;
	}
	_glio_shmem_put32(arp, fdptr, fdptr, sizeof(*fdptr)/4,
		glob_info->fstate_owner);
	_glio_shmem_quiet();
	_glio_clear_lock(arp, &glob_info->fstate_lock);

	return;
}

/*
 *	_par_do_io 
 *
 *	Flush or fill a cache page to/from the file. 
 */
void
_par_do_io(
	struct fdinfo *llfio,		/* underlying FFIO layer */
	off64_t	offset,			/* file offset where I/O will start */
	void	*addr,			/* address of memory data buffer */
	size_t	size,			/* bytes to transfer */
	enum io_type io_direction)	/* READ_IO or WRITE_IO */
{
	int	realfd;
	ssize_t	ret;
	char	*io_str;
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
	struct iosw reqstat, *statlist[1];
	struct listreq request;
#endif

	realfd	= llfio->realfd; /* assume system layer !!  TEMPORARY */
	io_str	= (io_direction == READ_IO) ? "read" : "write";

#ifdef	__mips
	ret	= lseek64(realfd, offset, SEEK_SET);  
	assert (ret != -1);

	if (io_direction == WRITE_IO) {
		ret	= write(realfd, addr, size);
		if (ret != size) {
			fprintf(stderr,"%s:\n\
    _par_do_io: write failed, returned %lld errno %d\n",
				GLOBERRMSG, (long long)ret, errno);
			abort();
		}
	} else {
		ret	= read(realfd, addr, size);
		if (ret == -1) {
			fprintf(stderr,"%s:\n\
    _par_do_io: read failed, errno %d\n", GLOBERRMSG, errno);
			abort();
		}
	}
#endif

#ifdef	_CRAY
	reqstat.sw_flag		= 0;
	reqstat.sw_error	= 0;
	reqstat.sw_count	= 0;

	bzero((char *) &request, sizeof(request));

	/* Set up the I/O request */
	if (io_direction == WRITE_IO)
		request.li_opcode = LO_WRITE; 
	else
		request.li_opcode = LO_READ;       

	request.li_flags	= LF_LSEEK;
	request.li_fildes	= realfd;             
	request.li_buf		= (char *) addr; 
	request.li_nbyte	= size;         
	request.li_status	= &reqstat;   
	request.li_offset	= offset;
	request.li_nstride	= 0; 

	statlist[0]		= &reqstat;

	if ((ret = listio(LC_START, &request, 1)) != 1) {
		fprintf(stderr,"%s:\n\
    _par_do_io: %s off %d: ret %d: errno: %d\n", 
			GLOBERRMSG, io_str, offset, ret, errno);

		/* sw_flag/sw_error may not have been set yet */
		fprintf(stderr, "\
    sw_flag: %d: sw_error: %d\n", reqstat.sw_flag, reqstat.sw_error);

		abort();
	}
	ret	= recall(realfd, 1, statlist);	/* wait for I/O to complete */ 
	if (ret == -1 || reqstat.sw_flag != 1 || reqstat.sw_error) {
		fprintf(stderr,"%s\n\
    _par_do_io: %s off %d: ", GLOBERRMSG, io_str, offset);
		fprintf(stderr, "\
    ret %d: sw_flag: %d: sw_error: %d\n", 
			ret, reqstat.sw_flag, reqstat.sw_error);
		abort();
	}
	if (reqstat.sw_count != size) {
		fprintf(stderr,"%s:\n\
    _par_do_io: %s off %d: expected: %d, sw_count: %d\n",
			GLOBERRMSG, io_str, offset, size, reqstat.sw_count);
		abort();
	}
#endif /* _CRAY */
	return;
}

/*
 * Truncate a global file.
 *
 *      Truncate this layer at the current position.
 */
int 
_glob_weod(
struct fdinfo   *fio,
struct ffsw     *stat
)
{
	struct glob_f *glob_info = (struct glob_f *)fio->lyr_info;
        glio_arena_t *arp = glob_info->arp;
	struct par_file_data *fdptr = &glob_info->fstate;
	long newsize;
        struct fdinfo  *llfio;

	if ((glob_info->oflags & O_ACCMODE) == O_RDONLY) {
		ERETURN(stat, EBADF, 0);
	}

	_glio_set_lock(arp, &glob_info->fsize_lock);

	_glio_shmem_get32(arp, fdptr, fdptr, sizeof(*fdptr)/4,
		glob_info->fstate_owner);

	newsize = glob_info->file_off;

	/*
 	 * If we are making the file shorter, we must deallocate
 	 * any cache pages allocated to regions of the file beyond the
	 * new EOD.
	 *
	 * A quick and dirty method is used to do this.  We deallocate all 
 	 * cached pages of the file.  Eventually we should handle this more 
	 * efficiently.   
	 */
	if (newsize < fdptr->size) {

		/* Deallocate pages */

		_par_flush_bufs(fio, glob_info->myrank, 1);

		/*
		 * Truncate the underlying file at the same location.   For 
	 	 * most layers, this ensures that data past this EOD becomes 
	 	 * zero if the underlying file is later extended such that 
	 	 * a hole is left between the this EOD and the data written 
		 * later.
		 */
		_glio_set_lock(arp, &glob_info->fstate_lock);
		_glio_shmem_get32(arp, fdptr, fdptr, sizeof(*fdptr)/4,
			glob_info->fstate_owner);

		if (newsize < fdptr->bytes_on_disk) {

        		llfio  = fio->fioptr;

                	if (XRCALL(llfio,seekrtn)
			    llfio, newsize, SEEK_SET, stat) == ERR)
				goto erret;

                	if (XRCALL(llfio,weodrtn) llfio, stat) == ERR)
				goto erret;

			fdptr->bytes_on_disk = newsize;
		}

		fdptr->size = newsize;
		_glio_shmem_put32(arp, fdptr, fdptr, sizeof(*fdptr)/4,
			glob_info->fstate_owner);
		_glio_clear_lock(arp, &glob_info->fstate_lock);
	}
	else if (newsize > fdptr->size) {
		_SETERROR(stat, EINTR, 0);	/* trunc beyond EOD */
		goto erret;
	}

        SETSTAT(stat, FFEOD, 0);
	_glio_clear_lock(arp, &glob_info->fsize_lock);
        return(0);

erret:
	_glio_clear_lock(arp, &glob_info->fsize_lock);
	return(ERR);
}


long
_glob_seek(
struct fdinfo   *fio,           /* ffio file descriptor. */
int             off,            /* requested byte offset */
int             whence,         /* SEEK_SET, SEEK_CUR, or SEEK_END */
struct ffsw     *stat)           /* status return word */
{
	struct glob_f *glob_info = (struct glob_f *)fio->lyr_info;
        glio_arena_t *arp = glob_info->arp;
	struct par_file_data *fdptr = &glob_info->fstate;
	long new_off;

	switch (whence) {
	case SEEK_SET:
		new_off = off;
		break;
	case SEEK_CUR:
		new_off = off + glob_info->file_off;
		break;
	case SEEK_END:
		_glio_set_lock(arp, &glob_info->fstate_lock);
		_glio_shmem_get32(arp, fdptr, fdptr, sizeof(*fdptr)/4,
			glob_info->fstate_owner);
		new_off = off + fdptr->size;
		_glio_clear_lock(arp, &glob_info->fstate_lock);
		break;
	default:
		fprintf(stderr, "%s:\n\
    par_seek: unknown whence value: %d\n", GLOBERRMSG, whence);
		return(-1);
	}
	glob_info->file_off = new_off;
	return(new_off);
}

/*
 *	_glob_close()
 *
 *	All PEs must participate in the close.
 */
int
_glob_close(
struct fdinfo   *fio,
struct ffsw     *stat)
{
        struct fdinfo   *llfio;
	struct glob_f *glob_info = (struct glob_f *)fio->lyr_info;
        glio_arena_t *arp = glob_info->arp;
	struct par_file_data *fdptr = &glob_info->fstate;
	int ret;
	int remote_index;
	int errv = 0;
#ifdef KEY /* Bug 1678 */
	/* Not used */
#else /* KEY Bug 1678 */
	struct ffc_stat_s statbuf;
#endif /* KEY Bug 1678 */

	_glio_barrier(arp);

	llfio	= fio->fioptr;

	_par_flush_bufs(fio, glob_info->myrank, 1 /* deallocate pages */ );

	_glio_barrier(arp);
	_glio_set_lock(arp, &glob_info->fstate_lock);
	_glio_shmem_get32(arp, fdptr, fdptr, sizeof(*fdptr)/4,
		glob_info->fstate_owner);
	_glio_clear_lock(arp, &glob_info->fstate_lock);

	if (_par_vars.print_stats)
		_par_stats_print(fio);

	/*
	 * Truncate underlying file to logical size if needed.  The 
	 * underlying file size is normally rounded out to a multiple of pages.
	 */
	if (glob_info->myrank == 0) {
		if (fdptr->bytes_on_disk > fdptr->size) {
			if (XRCALL(llfio,seekrtn)
				llfio, fdptr->size, SEEK_SET, stat) == -1)
				errv = errv ? errv : stat->sw_error;

			if (XRCALL(llfio,weodrtn) llfio, stat) == -1)
				errv = errv ? errv : stat->sw_error;
		}
	}

	_glio_barrier(arp);

	if (_par_vars.do_tracing) {
		_glio_set_lock(arp, &_par_trace_lock);
		_par_write_trace(fio);
		_glio_clear_lock(arp, &_par_trace_lock);
	}

        /*
         *      Close the underlying layers.
         */
        if (XRCALL(llfio,closertn) llfio, stat) == ERR)
                errv = errv ? errv : stat->sw_error;

	_glob_clfree(fio);
	_glio_arena_destroy(arp);

        if (errv)
                ERETURN(stat, errv, 0);

	return(0);
}

/*
 *	_glob_clfree()
 *
 *      Free the memory blocks used by the global layer and set corresponding
 *      pointers to NULL.  Do not destroy the symmetric memory arena, however.
 * 
 *	Return value is 0 if OK, 1 on error.
 */
int
_glob_clfree(struct fdinfo *fio)
{
	register int	i;
	struct glob_f	*glob_info;
 
	if (fio == NULL)
		return(0);

	glob_info = (struct glob_f *)fio->lyr_info;

	if (glob_info == NULL)
		return(0);

	for (i = 0; i < glob_info->memlistsz; i++) {
		free(glob_info->memlist[i]);
	}

	return(0);
}

/*
 *	_glob_flush()
 *
 *	Flush all buffers to disk.  This is not a collective operation.  
 *	Flush all buffers from all PEs.   We cycle around the PEs starting
 *	with glob_info->myrank so that IF all PEs happen to be calling this 
 *	at the same time they will all start flushing different pages.
 */
int
_glob_flush(struct fdinfo *fio)
{
        struct glob_f *glob_info = (struct glob_f *)fio->lyr_info;
        glio_arena_t *arp = glob_info->arp;
	int	remote_pe;
	int	real_fd;
	int	remote_index;
	int	errv = 0;
	int	ipage;
	int	ipe;
	struct par_page_description *ppd;

	ipe	= glob_info->myrank;

	for (;;) {

		for (ipage = 0; ipage < glob_info->pepages; ipage++) {

			ppd	= _par_get_pe_dirty_page(fio, ipe, 0);

			if (ppd == NULL)
				continue;
			_par_write_page(fio, ppd);
                        _par_unlock_page(fio, ppd);
		}

		ipe	= ipe + 1;

		if (ipe >= arp->grp.groupsz)
			ipe	= 0;

		if (ipe == glob_info->myrank)
			break;
	}
			
/*******/ /* what about error statuses ? */

	return(0);
}

void
_par_stats_print(struct fdinfo *fio)
{ 
        struct glob_f *glob_info = (struct glob_f *)fio->lyr_info;
        glio_arena_t *arp = glob_info->arp;
	struct par_file_data *fdptr = &glob_info->fstate;

	fprintf(stderr, "\nParallel I/O statistics for file %s\n",
		glob_info->fname);
	fprintf(stderr, "Write calls:            %12lld\n",
		(long long)fdptr->nwrite);
	fprintf(stderr, "Read calls:             %12lld\n",
		(long long)fdptr->nread);
	fprintf(stderr, "Bytes written to cache: %12lld\n",
		(long long)fdptr->bytes_written);
	fprintf(stderr, "Bytes read from cache:  %12lld\n",
		(long long)fdptr->bytes_read);
	fprintf(stderr, "Bytes written to disk:  %12lld\n",
		(long long)fdptr->disk_bytes_written);
	fprintf(stderr, "Bytes read from disk:   %12lld\n",
		(long long)fdptr->disk_bytes_read);
	fprintf(stderr, "Write cache hits:       %12lld\n",
		(long long)fdptr->write_cache_hits);
	fprintf(stderr, "Write cache misses:     %12lld\n",
		(long long)fdptr->write_cache_misses);
	fprintf(stderr, "Read cache hits:        %12lld\n",
		(long long)fdptr->read_cache_hits);
	fprintf(stderr, "Read cache misses:      %12lld\n",
		(long long)fdptr->read_cache_misses);
	fprintf(stderr, "Extended EOF sequentially:%10d\n",
		fdptr->sequentially_extend);
	fprintf(stderr, "Extended EOF by seeking:%12lld\n",
		(long long)fdptr->seek_extend);
	fprintf(stderr, "Overwrote existing page:%12lld\n",
		(long long)fdptr->overwrote_page);
	fprintf(stderr, "Page size (bytes):      %12lld\n",
		(long long)glob_info->pgsize_bytes);
	fprintf(stderr, "Number of pages per pe  %12lld\n",
		(long long)glob_info->pepages);
	return;
}

void
_par_abort(glio_arena_t *arp, const char * s)
{
	fprintf(stderr, "%s\n", s);
	fprintf(stderr,"\
Error (_par_abort):\n\
     Process with rank %d aborting due to internal library error.\n",
		arp->grp.myrank);
	abort();
}

/*
 *	_par_flush_bufs()
 *
 *	Flush cache pages for PE target_pe.  If target_pe is -1 this 
 *	means flush cache pages on all PEs.
 */
void
_par_flush_bufs(struct fdinfo *fio, int target_pe, int do_deallocate)
{
	register int	pe;
	register int	i;
	register int	max;
        struct glob_f	*glob_info = (struct glob_f *)fio->lyr_info;
        glio_arena_t	*arp = glob_info->arp;
	register int	pages_per_pe = glob_info->pepages;
	struct par_page_description	*p;

	if (target_pe == -1) {
		max	= arp->grp.groupsz;
		pe	= 0;
	} else {
		pe	= target_pe;
		max	= pe + 1;
	}

	for ( ; pe < max; pe++) {
		for (i = pages_per_pe - 1; i >= 0; i--) {

			p	= _par_get_pe_dirty_page(fio, pe, 0);

			if (p == NULL)
				break;
			_par_write_page(fio, p);

			if (do_deallocate) 
				_par_deallocate_page(fio, p);
			else 
				_par_unlock_page(fio, p);
		}	
	}

	return;
}
	
void
_par_deallocate_page(
	struct fdinfo   *fio,           /* ffio file descriptor. */
	struct par_page_description *p)	/* page to unlock */
{
	/* mark page unused */
	p->flags	= 0;
	_par_unlock_page(fio, p);

	return;
}

/*
 *	Assign a cache page to the current file page being accessed.
 *	Lock the page table lock for the PE that owns the page, mark the page
 *	BUSY, and then unlock the owning PE's page table lock.
 */
struct par_page_description *
_par_get_page_locked(struct fdinfo * fio, int pgnum)
{
	register short	free;
	register short	found;
	register short	busy;
	register int	free_index;
	register int	oldest_index;
	register int	pages_per_pe;
	register int	pe;
	register int	i;
	register long	oldest;
	struct glob_f *glob_info = (struct glob_f *)fio->lyr_info;
        glio_arena_t *arp = glob_info->arp;
	struct par_page_description *pptr = glob_info->myppages;

	pe		= pgnum % arp->grp.groupsz;
	pages_per_pe	= glob_info->pepages;

rescan:
	_glio_set_lock(arp, &glob_info->ppage_table_locks[pe]);
	_glio_shmem_get32(arp, pptr, glob_info->ppages, 
		glob_info->pepages * sizeof(struct par_page_description)/4,
		pe);
	free	= 0;
	found	= 0;
	busy	= 0;
	oldest	= -1;
	oldest_index = -1;
	free_index = -1;

	for (i = pages_per_pe - 1; i >= 0 ; i--) {
		if (pptr[i].flags == 0) {
			free	= 1;
			free_index = i;
			continue;
		}	
		if (pptr[i].page_num == pgnum ) {
			found	= 1;
			break;
		}	
		if (oldest == -1 || pptr[i].last_used < oldest) {
			oldest	= pptr[i].last_used;
			oldest_index = i;
		}
	}
	if (found == 0) {
		if (free) 
			i	= free_index;
		else
			i	= oldest_index;
	}
	/* "i" holds the index of the page we want to use */
	busy	= (pptr[i].flags & PG_BUSY);
	if (busy) {
		_par_wait_for_page(fio, pe);
		goto rescan;
	} else if (!found) {
		if (pptr[i].flags & PG_DIRTY) {
			pptr[i].flags |= PG_BUSY; 
			_glio_shmem_long_p(arp,
				&glob_info->ppages[i].flags,
				pptr[i].flags, pe);
			_glio_shmem_quiet();
			_glio_clear_lock(arp,
				&glob_info->ppage_table_locks[pe]);
			/* flushing some other page */
			_par_write_page(fio, &pptr[i]);
			_par_unlock_page(fio, &pptr[i]);
			goto rescan;
		} else /* safe to claim this page */ {
			pptr[i].pe = pe;
			pptr[i].buf_num = i;
			pptr[i].page_num = pgnum;
			pptr[i].flags = 0;
		}
	}
	pptr[i].flags |= PG_BUSY; 
	pptr[i].last_used = _rtc(); 
	_glio_shmem_put32(arp, &glob_info->ppages[i], &pptr[i],
		sizeof(struct par_page_description)/4, pe);
	_glio_shmem_quiet();
	_glio_clear_lock(arp, &glob_info->ppage_table_locks[pe]);

	return(&pptr[i]);
}

/*
 *	_par_wait_for_page
 *
 *	Wait for another PE to release a page.
 */
void
_par_wait_for_page(struct fdinfo *fio, int pe)
{
	struct glob_f *glob_info = (struct glob_f *)fio->lyr_info;
        glio_arena_t *arp = glob_info->arp;
	size_t	size;
	size	= (arp->grp.groupsz / BITPW) + 1;	/* in 64-bit words */

	_glio_shmem_get64(arp, glob_info->lwakeup_bitmap,
		glob_info->wakeup_bitmap, size, pe);
	SET_BIT(glob_info->lwakeup_bitmap, glob_info->myrank);
	_glio_shmem_put64(arp, glob_info->wakeup_bitmap,
		glob_info->lwakeup_bitmap, size, pe);

	glob_info->wakeword = 1;
	_glio_shmem_quiet();
	_glio_clear_lock(arp, &glob_info->ppage_table_locks[pe]);
	_glio_shmem_wait(&glob_info->wakeword, 1);

	if (glob_info->wakeword != PAR_WAKEUP_MAGIC) {
		fprintf(stderr, "%s:\n\
    Bad wakeup word: %lx\n", GLOBERRMSG, glob_info->wakeword);
		_par_abort(arp,"_par_wait_for_page: bad wakeup word.");
	}
	return;
}

/*
 *	Make an in-memory trace entry.
 */
void
_par_io_trace(struct fdinfo *fio, int pe, int buf_num, long offset,
			enum io_type io_direction)
{
	struct glob_f *glob_info = (struct glob_f *)fio->lyr_info;
        glio_arena_t *arp = glob_info->arp;
	_glio_set_lock(arp, &_par_trace_lock);
	if (glob_info->myrank != 0)
		_glio_shmem_get32(arp, &_par_trace_vars, &_par_trace_vars,
					sizeof(_par_trace_vars)/4, 0);

	if (_par_trace_vars.index == PAR_TRACE_ENTRIES &&
	    _par_trace_vars.pe == arp->grp.groupsz - 1) {
			_par_write_trace(fio);
			_par_trace_vars.index = 0;
			_par_trace_vars.pe = 0;
	} else if (_par_trace_vars.index == PAR_TRACE_ENTRIES &&
	    _par_trace_vars.pe < arp->grp.groupsz - 1) {
		_par_trace_vars.index = 0;
		_par_trace_vars.pe++;
	}	

	_par_local_tracebuf[0].magic = TRACE_REG_MAGIC;
	_par_local_tracebuf[0].data_pe = pe;
	_par_local_tracebuf[0].fd = glob_info->sysfd;
	_par_local_tracebuf[0].io_pe = glob_info->myrank;
	_par_local_tracebuf[0].io_buf = buf_num;
	_par_local_tracebuf[0].read = (io_direction == READ_IO) ? 1 : 0;
	_par_local_tracebuf[0].rtc = _rtc();
	_par_local_tracebuf[0].offset = offset;

	/* put trace entry on remote PE */

	_glio_shmem_put32(arp, &_par_trace_buf[_par_trace_vars.index],
	   	&_par_local_tracebuf[0], sizeof(struct _par_trace_entry)/4,
	   	_par_trace_vars.pe);
	_par_trace_vars.index++; 
	if (glob_info->myrank != 0) 
		_glio_shmem_put32(arp, &_par_trace_vars, &_par_trace_vars,
					sizeof(_par_trace_vars)/4, 0);
	_glio_shmem_quiet();
	_glio_clear_lock(arp, &_par_trace_lock);

	return;
}

/* 
 *	Output all traces.  Must be called under protection of _par_trace_lock.
 */
void
_par_write_trace(struct fdinfo *fio)
{
	struct glob_f *glob_info = (struct glob_f *)fio->lyr_info;
        glio_arena_t *arp = glob_info->arp;
	int	pe;
	int	size;

	assert(0);	/* _par_trace_vars are not symmetric */
	if (glob_info->myrank != 0)
		_glio_shmem_getmem(arp, &_par_trace_vars, &_par_trace_vars,
					sizeof(_par_trace_vars), 0);

	for (pe = 0; pe <= _par_trace_vars.pe;  pe++) {
		if (pe != _par_trace_vars.pe)
			size = sizeof(struct _par_trace_entry) * PAR_TRACE_ENTRIES;
		else
			size = sizeof(struct _par_trace_entry) * _par_trace_vars.index;
		_glio_shmem_getmem(arp, _par_local_tracebuf, _par_trace_buf,
		     		size, pe);

		(void) write(_par_vars.trace_fd, _par_local_tracebuf, size);
	}
	_par_trace_vars.index = 0;
	_par_trace_vars.pe = 0;
	if (glob_info->myrank != 0) 
		_glio_shmem_putmem(arp, &_par_trace_vars, &_par_trace_vars,
					sizeof(_par_trace_vars), 0);
	_glio_shmem_quiet();
	return;
}

void
_par_write_open_trace(struct fdinfo *fio, int flags, const char * fname)
{
	register int	len;
	struct glob_f *glob_info = (struct glob_f *)fio->lyr_info;
        glio_arena_t *arp = glob_info->arp;
	struct _par_trace_open_entry entry;
	char buf[PAR_MAX_FNAME + sizeof(entry)];

	_glio_set_lock(arp, &_par_trace_lock);
	_par_write_trace(fio);	
	entry.magic = TRACE_OPEN_MAGIC;
	entry.fd = glob_info->sysfd;
	len	= strlen(fname);
	entry.size = len;
	entry.flags = flags;
	entry.rtc = _rtc();
	entry.pe = glob_info->myrank;
	bcopy((char *) &entry, buf, sizeof(entry));
	bcopy(fname, ((char *) buf + sizeof(entry)), len);
	(void) write(_par_vars.trace_fd, buf, sizeof(entry) + len);
	_glio_clear_lock(arp, &_par_trace_lock);
	return;
}

#ifdef	_CRAY
void
_par_write_timestamp_trace(struct fdinfo *fio, int pe)
{
	struct glob_f *glob_info = (struct glob_f *)fio->lyr_info;
        glio_arena_t *arp = glob_info->arp;
	struct _par_trace_timestamp_entry entry;
	register long	before, after, halfway;

	_shmem_set_lock(&_par_trace_lock);
	_par_write_trace(fio);	
	_shmem_clear_lock(&_par_trace_lock);
	entry.magic = TRACE_TIMESTAMP_MAGIC;
	entry.pe = pe;
	_glio_barrier(arp);
	before = _rtc();
	entry.time_ticks = time(NULL);
	after = _rtc();
	halfway = (before + after) / 2;
	/* try to get rtc time in middle of system call */
	entry.time_rtc = halfway; 
	_glio_set_lock(arp, &_par_trace_lock);
	(void) write(_par_vars.trace_fd, (char *) &entry, sizeof(entry));
	_glio_clear_lock(arp, &_par_trace_lock);
	return;
}
#endif /* _CRAY */

/*
 *	_par_get_pe_dirty_page
 *
 *	Find dirty pages owned by a specified PE for this file. Skip 
 *	any busy pages.  A locked page will be returned.
 */
struct par_page_description *
_par_get_pe_dirty_page(
struct fdinfo *fio,		/* global file */
int pe,				/* pe to search for a dirty page */
int deallocate_valid_pages)	/* 1 if valid pages to be invalidated */
{
	register short	busy;
	register short	found;
	register int	i;
	struct glob_f *glob_info = (struct glob_f *)fio->lyr_info;
        glio_arena_t *arp = glob_info->arp;
	struct par_page_description *pptr = glob_info->myppages;
	int		pages_per_pe = glob_info->pepages;

rescan:
	_glio_set_lock(arp, &glob_info->ppage_table_locks[pe]);
	_glio_shmem_get32(arp, pptr, glob_info->ppages, 
		glob_info->pepages * sizeof(struct par_page_description)/4, pe);
	found	= 0;
	busy	= 0;

	for (i = 0; i < pages_per_pe; i++) { 
		if (glob_info->myppages[i].flags & PG_BUSY) {
			busy	= 1;
			break;
		} else if (glob_info->myppages[i].flags & PG_DIRTY) {
			found	= 1;
			break;
		} else if (glob_info->myppages[i].flags & PG_VALID) {
			/*
			 * Clear valid, non-dirty pages while looking
			 * for dirty ones.
			 */
			if (deallocate_valid_pages) {
				glob_info->myppages[i].flags = 0;
				_glio_shmem_long_p(
					arp,
					&glob_info->ppages[i].flags,
			           	glob_info->myppages[i].flags,
					pe);
			}
		} else if (glob_info->myppages[i].flags != 0) {
			fprintf(stderr,"%s\n\
    unknown flag value %lo\n", GLOBERRMSG, glob_info->myppages[i].flags);
			_par_abort(arp, "_par_get_pe_dirty_page: bad flags");
		}
	}
	if (busy) {
		/* wait for another PE to release the page */
		_par_wait_for_page(fio, pe);
		goto rescan;
	}
	if (found == 0) { 
		_glio_clear_lock(arp, &glob_info->ppage_table_locks[pe]);
		return(NULL);
	}
	glob_info->myppages[i].flags |= PG_BUSY; 
	_glio_shmem_long_p(arp, &glob_info->ppages[i].flags,
		 glob_info->myppages[i].flags, pe);
	_glio_shmem_quiet();
	_glio_clear_lock(arp, &glob_info->ppage_table_locks[pe]);

	return(&glob_info->myppages[i]);
}

/*
 * 	_par_unlock_page
 *
 *	Unlock a page previously locked by _par_get_page_locked().
 */
void
_par_unlock_page(
	struct fdinfo   *fio,           /* ffio file descriptor. */
	struct par_page_description *p)	/* page to unlock */
{
	register short	found;
	register int	index;
	register int	pe;
	register int	size;
	register int	waiting_pe;
	struct glob_f *glob_info = (struct glob_f *)fio->lyr_info;
        glio_arena_t *arp = glob_info->arp;

	found	= 0;
	index	= p->buf_num;
	pe	= p->pe;
	p->flags &= ~(PG_BUSY);

	_glio_set_lock(arp, &glob_info->ppage_table_locks[pe]);
	_glio_shmem_long_p(arp, &glob_info->ppages[index].flags,
			glob_info->myppages[index].flags, pe);

/*
 *	Now wakeup all PEs waiting for a page residing on the given PE.
 */
	size = (arp->grp.groupsz / BITPW) + 1;
        _glio_shmem_get64(arp, glob_info->lwakeup_bitmap,
		glob_info->wakeup_bitmap,  size, pe);
        while ((waiting_pe = _find_fbs_clr(glob_info->lwakeup_bitmap,
		arp->grp.groupsz)) != -1) {

                _glio_shmem_long_p(arp, &glob_info->wakeword, PAR_WAKEUP_MAGIC, 
			waiting_pe);
                found	= 1;
        }
        if (found)
                _glio_shmem_put64(arp, glob_info->wakeup_bitmap,
			glob_info->lwakeup_bitmap, size, pe);
        _glio_shmem_quiet();
        _glio_clear_lock(arp, &glob_info->ppage_table_locks[pe]);

	return;
}

/*
 *	_glob_io_init 
 *
 *	Initializes _par_vars with default values which apply
 *	to all global files which do not have numeric parameters specified.
 */
int
_glob_io_init()
{
	register int	rem;

	if (_glob_io_init_called)
		return(0);

	_glob_io_init_called = 1;

	_par_vars.pepages = GLOB_DEF_NBUF;
	_par_vars.pgsize = GLOB_DEF_BUFSIZ;
	_par_vars.pgsize_bytes = _par_vars.pgsize * YMPBLOCK;

	_par_environment_check();

	if (_par_vars.print_stats && _my_pe() == 0) {
		fprintf(stderr, "\nPAR_IO global cache parameters:\n");
		fprintf(stderr,
"par_io_init using %d pages of %d blocks each per process\n",
			_par_vars.pepages, _par_vars.pgsize/512);
	}
	if ( (rem = _par_vars.pgsize % _par_vars.disksect) != 0) {
		_par_vars.pgsize += ((_par_vars.disksect - rem)*512);
		if (_my_pe() == 0) {
			fprintf(stderr,"par_io_init: pgblks increased to %d, ",
				_par_vars.pgsize);
	    		fprintf(stderr,
				"a multiple of the declared sector size.\n");
		}

	}

	return(0);
}


/*
 *	_find_fbs_clr
 *
 *	Find first bit set and clear.  Returns -1 if no bits are set, returns
 *	bit offset of first set bit otherwise.
 */
int
_find_fbs_clr(long long * bitmap, int max_bits)
{
	register int	bit_set;
	register int	starting_bit;
	register int	i;
	register int	word;

	bit_set	= -1;
	starting_bit	= -1;

	/* quick check to see where first bit is set in bitmap */
	for (i = 0, word = 0; i < max_bits; i += BITPW, word++) {
		if (bitmap[word]) {
			starting_bit = i;
			break;
		}
	}

	if (starting_bit != -1) {
		for (i = starting_bit; i < max_bits; i++) {
			if (TST_BIT(bitmap, i)) {
				CLR_BIT(bitmap, i);
				bit_set = i;
				break;
			}
		}
	}

	return(bit_set);
}

void
_par_environment_check(void)
{
	register int	val;
	char		*s;

	s	= getenv("PAR_IO_STATS");
	if (s != NULL && strcmp(s, "1") == 0)
		_par_vars.print_stats	= 1;
	else
		_par_vars.print_stats	= 0;

	s	= getenv("PAR_IO_NPAGES");
	if (s != NULL) {
		fprintf(stderr,"%s:\n\
    PAR_IO_NPAGES environment variable is no longer supported.  It \n\
    will be ignored.\n", GLOBWARNMSG);
	}

	s	= getenv("PAR_IO_PEPAGES");
	if (s != NULL && (val = atoi(s)) != 0)
		_par_vars.pepages	= val;

	s	= getenv("PAR_IO_PGBLKS");
	if (s != NULL && (val = atoi(s)) != 0) {
		_par_vars.pgsize	= 512 * val;
		_par_vars.pgsize_bytes	= YMPBLOCK * val;
	}

	s	= getenv("PAR_IO_DISKSECT");
	if (s != NULL && (val = atoi(s)) > 0) 
		_par_vars.disksect	= val;
	else 
		_par_vars.disksect	= 1;

	s	= getenv("PAR_IO_TRACE");
	if (s != NULL && strcmp(s, "1") == 0) {
		_par_vars.do_tracing	= 1;
		(void) unlink(PAR_IO_TRACEFILE);
		_par_vars.trace_fd	=
			open(PAR_IO_TRACEFILE, O_CREAT|O_WRONLY|O_APPEND, 0644);
	}
	else 
		_par_vars.do_tracing	= 0;

	return;
}

