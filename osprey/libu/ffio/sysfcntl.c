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


#pragma ident "@(#) libu/ffio/sysfcntl.c	92.2	10/14/99 15:22:06"


#include <errno.h>
#include <ffio.h>
#include <unistd.h>
#include <sys/types.h>
#if !defined(_ABSOFT)
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
#include <sys/iosw.h>
#elif	defined(__mips)
#include <aio.h>
#endif
#include <sys/file.h>
#else
#include <cray/iosw.h>
#endif
#include <sys/param.h>
#include <sys/stat.h>
#ifdef	_LITTLE_ENDIAN
#if	defined(__S_IFLNK) && !defined(S_ISLNK)
#define S_ISLNK(mode)  __S_ISTYPE((mode), __S_IFLNK)
#endif
#endif	/* LITTLE_ENDIAN */
#if defined(BUILD_OS_DARWIN)
#include <sys/mount.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <sys/statfs.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <fcntl.h>
#include "sysio.h"
// OSP does not have libfoobar
#ifdef VENDOR_PSC /* Bug 11654 */
#include "../../libfoobar/qk.h"
#endif /* KEY Bug 11654 */

int
_make_scratch(const char *name, int realfd, int *retflags, struct ffsw *iostat);

/*
 * sys fcntl requests
 *
 * Parameters:
 *	fio	- fdinfo pointer
 *	cmd	- command code
 *	arg	- command specific parameter
 *	iostat	- pointer to status return word
 */
_sys_fcntl(
struct fdinfo	*fio,
int		cmd, 
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
int		arg,
#else
void		*arg,
#endif
struct ffsw	*iostat)
{
	int ret;
	ssize_t szret;
	struct ffsw *uiostat;
	struct ffc_info_s *ffcp;
	struct ffc_mtinfo_s *mti;
	struct stat *stp;
	int *scrsp;
#ifdef __mips
	aiocb_t *a[1];
#endif

	ret = 0;
	switch(cmd) {
		case FC_GETINFO:
			ffcp = (struct ffc_info_s *)arg;
			ffcp->ffc_flags = 
				FFC_STRM |	/* stream */
				FFC_WEOD | 	/* can WEOD (trunc()) */

				FFC_SEEKA |	/* seek abs */
				FFC_SEEKR |	/* seek relative */
				FFC_SEEKE |	/* seek end */
				FFC_RWND |	/* can rewind */

				FFC_BINARY |	/* can do binary */
				FFC_CODED |	/* can do chars */

				FFC_RDM |	/* can do random */
				FFC_SEQ |	/* can do seq */
				FFC_ASYNC |	/* can do 'real' async I/O */

				FFC_NOTRN |	/* No data transformation */
				FFC_BCKDOOR |	/* Can handle O_SSD on open */
				0;

#ifdef _UNICOS
			ffcp->ffc_flags |=  FFC_CANLISTIO;
#endif
#if defined(_UNICOS)  || defined(__mips)
			/* FFC_CANSYLISTIO means that we can do a 1-entry */
			/* synchronous listio request */
			ffcp->ffc_flags |=  FFC_CANSYLISTIO;
#endif

			ffcp->ffc_gran	 = 8;		/* 1 byte granularity */
			ffcp->ffc_reclen = 0;		/* no record length */
			ffcp->ffc_fd	 = fio->realfd; /* real fd */
			break;
		case FC_STAT:
			stp = (struct stat *)arg;
			ret = fstat(fio->realfd, stp);
			if (ret < 0)
				iostat->sw_error = errno;
			break;
		case FC_RECALL:
#ifdef	__mips
			uiostat = (struct ffsw *)arg;
			if (uiostat->sw_flag == 0) {
				a[0] = &(uiostat->aiocbp);
again:
				if (aio_error(&uiostat->aiocbp) == EINPROGRESS)
				   if (aio_suspend((const aiocb_t **)a, 1, NULL) < 0) {
					if (errno == EAGAIN || errno == EINTR)
						goto again;
					else {
						/* I don't expect this to */
						/* happen, since no other */
						/* errors are documented */
						ERETURN(iostat, errno, 0);
					}
				   }
				szret = aio_return(&(uiostat->aiocbp));
				if (szret >= 0){
					uiostat->sw_error = 0;
					uiostat->sw_count = szret;
				}
				else {
					uiostat->sw_error = aio_error(&(uiostat->aiocbp));
					uiostat->sw_count = 0;
					ret = -1;
				}
				uiostat->sw_flag = 1;
			}
#endif
#ifdef	_CRAY
			uiostat = (struct ffsw *) arg;
			if (uiostat->sw_flag == 0) {
				_recall(fio->realfd, 1,
					(struct iosw **)&uiostat);
			}
#endif
		case FC_ASPOLL:
			uiostat = (struct ffsw *) arg;
#ifdef __mips
			if (uiostat->sw_flag == 0) {
				if (aio_error(&uiostat->aiocbp) == EINPROGRESS)
					break;
				/* must be done */
				szret = aio_return(&(uiostat->aiocbp));
				if (szret >= 0){
					uiostat->sw_error = 0;
					uiostat->sw_count = szret;
				}
				else {
					uiostat->sw_error = aio_error(&(uiostat->aiocbp));
					uiostat->sw_count = 0;
				
					ret = -1;
				}
				uiostat->sw_flag = 1;
			}
#else
			if (uiostat->sw_flag == 0)
				break;
#endif
/*
 *			If status is non-zero, it is already set.  Leave it
 *			alone.
 */
			if (FFSTAT(*uiostat) == 0) {
				if (uiostat->sw_error != 0) {
					FFSTAT(*uiostat) = FFERR;
				}
				else if (uiostat->sw_count == 0) {
					SETSTAT(uiostat, FFEOD, 0);
				}
				else {
					FFSTAT(*uiostat) = FFCNT;
				}
			}
			break;
		case FC_SCRATCH:
			scrsp	= (int *)arg;
			ret = _make_scratch(((struct sys_f *)fio->lyr_info)->name,
					    fio->realfd, scrsp, iostat);
			break;		
#ifdef _CRAY
		case FC_IALLOC:
			{
			struct ff_ialloc_struct *data;
			data = (struct ff_ialloc_struct *)arg;
			ret = data->ia_ret = ialloc(fio->realfd, data->ia_nb, 
				data->ia_flag, data->ia_part,
				&(data->ia_avl));
			break;
			}
#endif
		case FC_SETLK:
			ret = fcntl(fio->realfd, F_SETLK, (struct flock *)arg);
			if (ret < 0)
				iostat->sw_error = errno;
			break;		
		case FC_GETLK:
			ret = fcntl(fio->realfd, F_GETLK, (struct flock *)arg);
			if (ret < 0)
				iostat->sw_error = errno;
			break;		
		case FC_SETLKW:
			ret = fcntl(fio->realfd, F_SETLKW, (struct flock *)arg);
			if (ret < 0)
				iostat->sw_error = errno;
			break;		
		case FC_FSTATFS:
#if	defined(_LITTLE_ENDIAN)
#ifdef VENDOR_PSC /* Bug 11654 */
			/* Introduce a wrapper so we can avoid calling
			 * fstatfs in an environment where that causes a
			 * link-time warning. */
			ret = _Qk_fstatfs(fio->realfd, (struct statfs *)arg);
#else /* KEY Bug 11654 */
			ret = fstatfs(fio->realfd, (struct statfs *)arg);
#endif /* KEY Bug 11654 */
#else
			ret = fstatfs(fio->realfd, (struct statfs *)arg,
				sizeof(struct statfs), 0);
#endif
			if (ret < 0)
				iostat->sw_error = errno;
			break;		
#ifdef __mips
		case FC_DIOINFO:
			ret = fcntl(fio->realfd, F_DIOINFO,
				(struct dioattr *)arg);
			if (ret < 0)
				iostat->sw_error = errno;
			break;
#endif

		default:
			ERETURN(iostat, FDC_ERR_NOSUP, 0);
	}
	return(ret);
}

/*
 * _make_scratch
 *
 *	Validate that a system file can be made a scratch file, and then
 *	unlink it.
 *
 * Return value
 *
 *	 0 on success
 *	-1 on failure, with iostat->sw_error set to the error code.
 *
 *	On output, *retflags is set to a mask containing the following flags 
 *	which indicate the outcome.
 *
 *		SCR_UNLINKED    Set if this fffcntl() call has
 *				successfully unlinked the file.
 *
 *		SCR_SINGLELINK  Set if the file is not a pipe or a tty, 
 *				has a link count equal to one, and is
 *				not a symbolicly linked file.  
 */
int
_make_scratch(
const char	*name,
int		realfd,
int		*retflags,
struct ffsw	*iostat)
{
	int		ss;
	int		is_single_linked;
	ino_t		name_ino;
	dev_t		name_dev;
	struct stat	statbuf;

	*retflags = 0;
	is_single_linked = 1;
/*
 *	If this is the "fd" layer, we don't know the name.  So we can't 
 *	possibly unlink it.
 */
	if (name == NULL) {
		ERETURN(iostat, FDC_ERR_NOSCR, 0);
	}

	ss = lstat(name, &statbuf);
	if (ss == -1) 
		return(0);			/* must be unlinked already */

	if (S_ISLNK(statbuf.st_mode)) {
		is_single_linked = 0;
		/*
		 * If a symbolic link, we need to call stat to get st_ino/st_dev
		 */
		ss = stat(name, &statbuf);
		if (ss == -1) 
			return(0);		/* must be unlinked already */
	}

/*
 *	Verify that the filename still corresponds to the file we originally
 *	opened.  This might not be true if the file has been unlinked and
 *	re-created with the same name.
 */
	name_ino = statbuf.st_ino;
	name_dev = statbuf.st_dev;

	ss = fstat(realfd, &statbuf);
	if (ss != 0) {
		ERETURN(iostat, errno, 0);
	}

	if (statbuf.st_ino != name_ino || statbuf.st_dev != name_dev)
		return(0);			/* must be unlinked already */

	if (statbuf.st_nlink != 1)
		is_single_linked = 0;

	ss = unlink(name);
	if (ss != 0) {
		ERETURN(iostat, errno, 0);
	}

	*retflags |= SCR_UNLINKED;

	if (is_single_linked)
		*retflags |= SCR_SINGLELINK;

	return(0);
}
