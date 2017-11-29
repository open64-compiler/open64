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


#pragma ident "@(#) libu/ffio/syslistio.c	92.2	10/14/99 15:22:06"


#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#if !defined(_ABSOFT) && !defined(__mips) && !defined(_LITTLE_ENDIAN)
#include <sys/listio.h>
#include <sys/iosw.h>
#elif !defined(__mips) && !defined(_LITTLE_ENDIAN)
#include <cray/listio.h>
#include <cray/iosw.h>
#endif
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
#include "listio_mips.h"
#endif
#include <fcntl.h>
#include <ffio.h>
#include "sysio.h"

#define LSIZ 100
#define MAXRECALL 100000 	/* lots and lots */
#ifdef __mips
extern short _ffaio_initialized; /* defined in syswritea.c */
#endif

/*
 * _sys_listio
 *
 *	Issue a listio request for the syscall layer.
 *
 * Return Value:
 *
 *	On success, nent is returned, and the contents of the stat structure are
 *	unspecified.
 *
 *	If an error in setup is encountered, stat is set as follows:
 *
 *		stat->sw_error	= error code
 *		stat->sw_stat	= FFERR
 *		stat->sw_flag	= 1
 *		stat->sw_count	= 0
 *
 *	If an error in I/O request I is detected, the list[I].li_stat
 *	structure will be set as follows:
 *
 *		list[I].li_stat->sw_error	= error code
 *		list[I].li_stat->sw_flag	= 1
 */
int
_sys_listio( 
int			cmd,		/* LC_WAIT or LC_START */
struct fflistreq	*list,		/* list of requests (see fflistio) */
int			nent,		/* number of requests */
struct ffsw		*stat)		/* status structure */
{
#ifdef __mips
	/* On SGI systems, this function is currently very limited. */
	/* For LC_WAIT, we only support 1 entry in the list, and LF_LSEEK */
	/* must be set. We turn this into a pread or pwrite system call */
	/* For LC_START, we also support only 1 entry in the list, and */
	/* LF_LSEEK must be set. We turn this into an aio_read or aio_write */
	/* The primary reason I'm supporting only 1 entry in the list */
	/* is that I haven't figured out how to handle errors like */
	/* EAGAIN. Also, I had problems with lio_listio */

	struct fdinfo	*fio;
	ssize_t ret;
	aiocb_t *curaio;
	int fd;
	ssize_t wret;
	int zero = 0;

	if (cmd == LC_WAIT) {
	    if (( nent > 1) || (list->li_flags & LF_LSEEK == 0)){
		_SETERROR(list->li_status, errno, 0);
		ERETURN(stat, FDC_ERR_NOSUP, 0);
	    }
	    fio = list->li_ffioptr;
	    fd = fio->realfd;
	    if (list->li_opcode == LO_READ) {
		if (((struct sys_f *)fio->lyr_info)->nointrio)
			ret = pread(fd, list->li_buf, list->li_nbyte,
				list->li_offset);
		else
			LOOP_SYSCALL(ret, pread(fd, list->li_buf, list->li_nbyte, list->li_offset));
		if (ret < 0){
			_SETERROR(list->li_status, errno, 0);
			ERETURN(stat, errno, 0);
		}
		((struct sys_f *)fio->lyr_info)->curpos = list->li_offset+ret;
		((struct sys_f *)fio->lyr_info)->needpos = 1;
	    }
	    else {
                if (((struct sys_f *)fio->lyr_info)->oappend) {
                        ((struct sys_f *)fio->lyr_info)->curpos =
                                ((struct sys_f *)fio->lyr_info)->endpos;
                }
		if (((struct sys_f *)fio->lyr_info)->nointrio)
			ret = pwrite(fd, list->li_buf, list->li_nbyte, 
				list->li_offset);
		else
			LOOP_SYSCALL(ret, pwrite(fd, list->li_buf,
				list->li_nbyte, list->li_offset));
		if (ret < 0) {
			_SETERROR(list->li_status, errno, 0);
			ERETURN(stat, errno, 0);
		}

		((struct sys_f *)fio->lyr_info)->curpos += ret;
		((struct sys_f *)fio->lyr_info)->needpos = 1;
		if (((struct sys_f *)fio->lyr_info)->curpos >
			((struct sys_f *)fio->lyr_info)->endpos)
			((struct sys_f *)fio->lyr_info)->endpos =
				((struct sys_f *)fio->lyr_info)->curpos;
		if (ret > 0 && ret != list->li_nbyte){
			/* we wrote some, but not all */
			/* Just call _sys_write to write the rest */
			char *buf;
			struct ffsw tmpstat;
			buf = list->li_buf + ret;
			wret = XRCALL(fio, writertn) fio, CPTR2BP(buf), 
				list->li_nbyte - ret, &tmpstat, PARTIAL, &zero);
			if (wret < 0) {
				_SETERROR(list->li_status, errno, tmpstat.sw_count+ret);
				ERETURN(stat, errno, tmpstat.sw_count+ret);
			}	
			ret += wret;
		}
	    }
	    if (ret == 0 && list->li_opcode == LO_READ && list->li_nbyte > 0) {
		SETSTAT(list->li_status, FFEOD, ret);
	    }
    	    else {
		SETSTAT(list->li_status, FFCNT, ret);
	    }
	}	/* LC_WAIT */
	else if (cmd == LC_START) {
	    if ((nent != 1)|| (list->li_flags & LF_LSEEK == 0)) {
		_SETERROR(list->li_status, errno, 0);
		ERETURN(stat, FDC_ERR_NOSUP, 0);
	    }
	    if (list->li_nstride != 1 || list->li_memstride != 0 ||
		list->li_filstride != 0) {
			_SETERROR(list->li_status, errno, 0);
			ERETURN(stat, FDC_ERR_NOSUP, 0);
            }
	    curaio = &(list[0].li_status->aiocbp);
	    AIO_INIT();
	    fio = list[0].li_ffioptr;
	    curaio->aio_fildes =fio->realfd;
	    curaio->aio_nbytes = list[0].li_nbyte;
	    curaio->aio_offset = list[0].li_offset;
	    curaio->aio_reqprio = 0;
	    curaio->aio_sigevent.sigev_notify = SIGEV_NONE;
	    curaio->aio_buf = list[0].li_buf;
	    if (list[0].li_opcode == LO_READ) {
		if (aio_read(curaio) == -1) {
			if (errno == EAGAIN) {
				/* Try it synchronously */
				    if (XRCALL(fio, seekrtn) fio,
					list[0].li_offset, SEEK_SET, stat) == -1) {
					_SETERROR(list->li_status, stat->sw_error, 0);
					return(-1);	
				    }
				    if (XRCALL(fio, readrtn) fio,
					CPTR2BP(list[0].li_buf), list[0].li_nbyte,
					list[0].li_status, FULL, &zero) == -1)
					ERETURN(stat,list[0].li_status->sw_error , 0);
				return(nent);
			}
			else {
				_SETERROR(list->li_status, errno, 0);
				ERETURN(stat, errno, 0);
			}
		}
		else {
			((struct sys_f *)fio->lyr_info)->curpos =
				list[0].li_offset;
	                if (((struct sys_f *)fio->lyr_info)->curpos + list[0].li_nbyte >
       	                	((struct sys_f *)fio->lyr_info)->endpos)
	                        ((struct sys_f *)fio->lyr_info)->curpos =
       	                         ((struct sys_f *) fio->lyr_info)->endpos;
                	else
                        	((struct sys_f *)fio->lyr_info)->curpos += list[0].li_nbyte;
	                ((struct sys_f *)fio->lyr_info)->needpos = 1;

			return(nent);
		}
            }
	    else {
               	if (((struct sys_f *)fio->lyr_info)->oappend) {
                       	((struct sys_f *)fio->lyr_info)->curpos =
                               ((struct sys_f *)fio->lyr_info)->endpos;
               	}
		else
			((struct sys_f *)fio->lyr_info)->curpos =
				list[0].li_offset;
		if (aio_write(curaio) == -1) {
			if (errno == EAGAIN) {
				/* Try it synchronously */
				if (XRCALL(fio, seekrtn) fio,
					list[0].li_offset, SEEK_SET, stat) == -1) {
					_SETERROR(list->li_status, stat->sw_error, 0);
					return(-1);	
				}
				if (XRCALL(fio, writertn) fio,
					CPTR2BP(list[0].li_buf), list[0].li_nbyte,
					list[0].li_status, FULL, &zero) == -1)
					ERETURN(stat,list[0].li_status->sw_error , 0);
				return(nent);
			}
			else {
				_SETERROR(list->li_status, errno, 0);
				ERETURN(stat, errno, 0);
			}
		}
		else {
			((struct sys_f *)fio->lyr_info)->curpos += 
				list[0].li_nbyte;
	                if (((struct sys_f *)fio->lyr_info)->curpos >
       	                 ((struct sys_f *)fio->lyr_info)->endpos)
       	                 ((struct sys_f *)fio->lyr_info)->endpos =
                                ((struct sys_f *)fio->lyr_info)->curpos;

			return(nent);
		}
	    }
	}
	else {
		_SETERROR(list->li_status, errno, 0);
		ERETURN(stat, FDC_ERR_NOSUP, 0);
	}
	return(nent);
#elif _UNICOS
	struct fdinfo	*fio;
	struct fdinfo	*newfio;
	int i, j, nb, ret, k;
	int fd;
	int slice;
	int flag = 0;
	int nstr;
	int pos;
	bitptr buf;
	int zero;
	int numdone;
	struct ffsw *istat;	/* stat on individual request(s) */
	struct ffsw locstat;	/* stat for 'internal' request(s) */
	struct ffsw dumstat;	/* Dummy status, bit bucket */
	struct fflistreq loclist[LSIZ];
	int istrt;

	fio = GETIOB(list[0].li_fildes);
	if (fio->xr.listiortn != _sys_listio) {
		ERETURN(stat, FDC_ERR_LSTIO, 0); /* not syscall layer */
	}
	for (i = 0; i < nent; i++) {
		newfio = GETIOB(list[i].li_fildes);
		if (newfio != fio) {
			/* We never expect different fio's */
			ERETURN(stat, FDC_ERR_LSTIO, 0); /* not syscall layer */
		}
	}
	numdone = 0;
/*
 *	We can actually fire a system call listio(2).  However, we must
 *	first copy the list entries to a private place and change a few
 *	things.
 */
	fd = fio->realfd;
	i = 0;
	while (i < nent) {
		slice = nent - i;
		if (slice > LSIZ) slice = LSIZ;
		for (j = 0 ; j < slice ; j++) {
			loclist[j] = list[i+j];
			loclist[j].li_fildes = fd;
			istat = list[i+j].li_status;
			CLRFFSTAT(*istat);
		}
		istrt = -1;
		ret = listio(cmd,(struct listreq *)loclist,slice);
		if (ret < 0) {
			if ((errno == EINTR) && (cmd == LC_WAIT) &&
			    (G@INTIO == 0)) {
				/* When we are finished with this */
				/* loop, we will wait for all of the */
				/* requests to complete. */
				ret = slice;	
			}
			else {
				if (numdone == 0)  {
					_SETERROR(stat, errno, 0);
					goto alldone; /* return number OK */
				}
				else {
					for (k = i; k < nent; k++) {
						istat = list[k].li_status;
						_SETERROR(istat,errno,0);
					}
					goto alldone;
				}
			}
		}
		else if (( ret < slice) && (G@INTIO == 0)) {
			/* See if any requests failed */
			/* because of EINTR. */
			/* If any did, then we will restart */
			/* them, providing all of the */
			/* subsequent requests also failed */
			/* because of EINTR and there were */
			/* no other kinds of failures. */
			for (j = 0; j < slice; j++) {
				fio = GETIOB(list[i+j].li_fildes);
				istat = list[i+j].li_status;
				if (istat->sw_error != 0 &&
				   	istat->sw_error != EINTR)
					break;
				if (istat->sw_error == EINTR) {
					if (list[i+j].li_nstride != 1) {
						/* The kernel breaks 
						 * up compound requests.
						 * This means we could
						 * get EINTR half way
						 * through a compound
						 * request. Make sure that
						 * we can safely restart it.
						 */
						if ((list[i+j].li_flags &
						   LF_LSEEK) == 0)
							break;
						if (list[i+j].li_opcode == LO_WRITE){
							if ((fio->opn_flags & 
							O_APPEND) != 0)
							         break;
						}
					}
					istrt = j;
					break;
				}
			   }
			if (istrt != -1) {
			   for (j = istrt; j < slice; j++) {
				istat = list[i+j].li_status;
				if (istat->sw_error != EINTR) 
					istrt = -1;
			   }
			}
		}
		if (istrt != -1)
			ret = slice = istrt; /* restarting some*/
		if (ret > 0)
			numdone += ret;
		i += slice;	/* Ones that failed should already */
				/* be marked. */
	}
alldone:

/*
 *	If command was LC_WAIT, we must wait for completion for all those
 *	requests that were successfully started.
 */
	if (cmd == LC_WAIT && numdone > 0) {
		for (i = 0 ; i < nent ; i++) {
			int ct = 0;
			fio = GETIOB(list[i].li_fildes);
			istat = list[i].li_status;
			while (FFSTAT(*istat) == 0) {
				XRCALL(fio, fcntlrtn)
					fio, FC_RECALL, istat, &dumstat);
/*
 *				Note that _SETERROR also sets FFSTAT
 *				and breaks out of the while loop.
 */
				if (ct++ > MAXRECALL)
					_SETERROR(istat, FDC_ERR_INTERR, 0);
			}
		}
	}
	if (numdone == 0) numdone = -1;	/* make error more obvious */
	return numdone;
#else
	ERETURN(stat, FDC_ERR_INTERR, 0); 
#endif

}
