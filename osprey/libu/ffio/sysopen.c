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


#pragma ident "@(#) libu/ffio/sysopen.c	92.2	10/14/99 15:22:06"

#include <errno.h>
#include <fcntl.h>
#include <ffio.h>
#ifdef	_UNICOS
#include <infoblk.h>
#endif
#include <stdio.h>
#include <string.h>
#include <liberrno.h>
#include "sysio.h"
#include "fdio.h"
#include <cray/assign.h>
#include <cray/portdefs.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef	__mips
#include <aio.h>
#endif
#include "fxlist.h"

DECLARE(SYSTEM_XLIST);
struct xtr_s SYSTEM_XLIST_P = { SYSTEM_XLIST };
DECLARE(SYSCALL_XLIST);
struct xtr_s SYSCALL_XLIST_P = { SYSCALL_XLIST };
DECLARE(NULL_XLIST);
struct xtr_s NULL_XLIST_P = { NULL_XLIST };
DECLARE(END_XLIST);
struct xtr_s END_XLIST_P = { END_XLIST };

#ifdef	_CRAY
int _gsys_qtape(struct stat *sb);
#endif

/*
 * System open routine.  Open a 'real' file descriptor, and set up the
 *	info block accordingly.  Handle options such as RAW mode. (NYI)
 */

_ffopen_t
_sys_open(
const char	*name,
int		flags,
mode_t		mode,
struct fdinfo	*fio,
union spec_u	*spec,
struct ffsw	*retstat,
long		cbits,
int		a_cblks,
struct gl_o_inf *a_oinf)	/* global open information for this file */

{
	int		narg;
	int		fd;
	int		ret;
	int		numarg;
	int		cblks;
	struct gl_o_inf *oinf;
	union spec_u	*nspec;
	struct stat 	statbuf;
	struct sys_f	*sys_info;
	int 		sys_flags ;

/*
 *	The last two arguments are optional for now, for the convenience of
 *	the NASTRAN applications people who have their own version of 
 *	_ffopen.c.   These arguments may be made non-optional in the 2.0
 *	CrayLibs release.
 */
#ifdef	_UNICOS
	NUMARG(narg);
#else
	narg = 9;
#endif
	cblks = 0;
	if (narg >= 8)
		cblks = a_cblks;

	oinf = NULL;
	if (narg >= 9)
		oinf = a_oinf;

#ifdef	__mips
	sys_flags = spec[0].class_info.info2;
#endif

	nspec = spec;
	NEXT_SPEC(nspec);

	sys_info = (struct sys_f *)calloc(sizeof(struct sys_f),1);
	if (sys_info == NULL) {
		_FFOPEN_ERETURN(retstat, FDC_ERR_NOMEM, 0);
	}
	fio->lyr_info = (char *)sys_info;

#ifdef	__mips
	sys_info->aio_callback = sys_flags & SYS_flags_AIO_CB;
#endif

	if (spec->fld.class != CLASS_END && nspec->fld.class == CLASS_FD) {
		fd = _fd_check(name, flags, mode, fio, nspec, retstat, cbits,
			cblks, oinf);
		if (fd == ERR)
			return(_FFOPEN_ERR);
		sys_info->name = NULL;	/* we don't know the real name*/
	}
	else {
		if (sys_info->tape == 0 && oinf != NULL && oinf->aip != NULL) {
#if	defined(_UNICOS) || defined(__mips)
			assign_info *aip = oinf->aip;

			_ae_setoflags(aip, &flags);
#endif
#ifdef	_UNICOS

			/*
			 * -p and -q assign options 
			 */
			if (aip->pr_partit_flg || aip->q_ocblks_flg) 
				flags |= O_PLACE;

			if (aip->pr_partit_flg) {
				cbits = aip->pr_partit;
				aip->pr_partit_flg |= ATTR_USED;
			}
			if (aip->q_ocblks_flg) {
				cblks = aip->q_ocblks;
				aip->q_ocblks_flg |= ATTR_USED;
			}
		
#endif	/* _UNICOS */
		}
		LOOP_SYSCALL(fd, open(name, flags, mode, cbits, cblks));
		/*
		 * Save the name in case we need it for the FC_SCRATCH 
		 * fffcntl() command.
		 */
		sys_info->name = strdup(name);
		if (sys_info->name == NULL) {
			if (fd >= 0)  {
				(void)close(fd);	
				free(fio->lyr_info);
			}
			_FFOPEN_ERETURN(retstat, FDC_ERR_NOMEM, 0);
		}
		if (flags & O_APPEND) {
			sys_info->oappend = 1;
		}
	}

	fio->realfd = fd;
	if (fd < 0) {
		if (sys_info->name != NULL)
			free(sys_info->name);
		free(fio->lyr_info);
		_FFOPEN_ERETURN(retstat, errno, 0);
	}
	ret = fstat(fd, &statbuf);
#ifdef __mips
	if (ret == 0) {
		sys_info->curpos = 0;
		sys_info->endpos = statbuf.st_size;
	}
#endif
#ifndef __mips
	if (ret == 0)   {
		/* If the file is a tape, it is not safe to reissue */
		/* interrupted i/o calls. */
		if (_gsys_qtape(&statbuf) == 1) {
			sys_info->nointrio = 1;
			sys_info->tape = 1;	/* remember that it is a tape */
		}
	}
#endif

	if (sys_info->tape == 0 && oinf != NULL && oinf->aip != NULL) {
		/*
		 * The -m assign option is handled by default, since the 
		 * syscall layer does not do any buffering.
		 */
		if (oinf->aip->m_multup_flg)
			oinf->aip->m_multup_flg |= ATTR_USED;
#ifdef	_UNICOS
		/*
		 * -n assign option 
		 */
		if ( (oinf->aip->n_preall_flg & ATTR_SET) &&
		    !(oinf->aip->n_preall_flg & ATTR_USED) ) {

			if (_prealloc(fd, 1, oinf->aip, &statbuf) == -1) {
				if (sys_info->name != NULL)
					free(sys_info->name);
				free(fio->lyr_info);
				_FFOPEN_ERETURN(retstat, errno, 0);
			}
		}

#endif /* _UNICOS */
	}

	fio->rwflag = POSITIN;
#ifdef	_UNICOS
	fio->lock = 0;	/* The system call layer is multitasking safe on */
			/* Unicos and Unicos/MK. */
			/* It is not multitasking safe on SPARC, because */
			/* that does not have listio */
			/* It is not multitasking safe on MIPS, because */
			/* that does not have listio, and because */
			/* the system call layer's implementation of */
			/* asynchronous i/o requires some calls (like  */
			/* _sys_write) to sometimes seek and then write. */
#endif
	DUMP_IOB(fio); /* debugging only */
	return(0); /* No pointer to next layer.  This is the end.... */
}

static int bmxstat = 0;
static struct stat _bmx_statbuf;

/* major part of a device, stolen from sys/sysmacros.h */
#define MAJOR(x)	(int)(((uint)x>>8)&0377)

/*
 * System open routine.  Open a 'real' file descriptor, and set up the
 *	info block accordingly.  Handle options such as RAW mode. (NYI)
 */

_ffopen_t
_gsys_open(
const char	*name,
int		flags,
mode_t		mode,
struct fdinfo	*fio,
union spec_u	*spec,
struct ffsw	*retstat,
long		cbits,
int		cblks,
struct gl_o_inf *oinf)		/* global open information for this file */
{
	int ret;
	_ffopen_t nextfio;
	struct stat statbuf;
	extern struct xtr_s *_recfm_tab[];
	union spec_u *nspec;
 
	nspec = spec;
	NEXT_SPEC(nspec);

/*
 *	Special case the FD layer.
 */
	if (spec->fld.class != CLASS_END && nspec->fld.class == CLASS_FD) {
/*
 *		See what the fstat syscall will tell us about the fd.
 */
		ret = fstat(FD(nspec), &statbuf);
	}
	else {
/*
 *		Let's not get fooled by /dev/null!
 */
		if (strcmp(name, "/dev/null") == 0) goto syscalls;
/*
 *		Try to open the given file according to what the stat syscall
 *		will tell us about the device
 */
		ret = stat(name, &statbuf);
	}
/*
 *	If stat() fails, assume that it is a regular file.
 */
	if (ret < 0) {
		goto syscalls;
	}
/*
 *	If it is a 'regular' file, use syscalls.
 */
	if (S_ISREG(statbuf.st_mode)) {
		goto syscalls;
	}
/*
 *	Is it a pipe?  Do system calls, no buffering should be attempted.
 */
	else if (S_ISFIFO(statbuf.st_mode)) {
		goto syscalls;
	}
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
/* 	
 *	No tape support yet on SGI
 */
	else if (_gsys_qtape(&statbuf) == 1) {

#ifdef	_CRAY1
		oinf->alreadyopen = 0;
		if ((ret = _er90_bmx(name, flags, oinf)) < 0) {
			_FFOPEN_ERETURN(retstat, errno, 0);
		} 
		if (!(LOADED_DATA(_recfm_tab[ret]))) {
			_FFOPEN_ERETURN(retstat, FDC_ERR_DISABL, 0);
		}
		fio->class = ret;
		fio->xr = *_recfm_tab[ret];
		oinf->alreadyopen = 1;
		nextfio = XRCALL(fio,openrtn) name, flags, mode, fio,
				spec, retstat, cbits, cblks, oinf);
		return(nextfio);
#else
/*
 *		It seems to be a tape, so use the BMX handlers.
 */
		fio->class = CLASS_BMX;
		fio->xr = *_recfm_tab[CLASS_BMX];
		nextfio = XRCALL(fio,openrtn) name, flags, mode, fio,
				spec, retstat, cbits, cblks, oinf);
		return(nextfio);
#endif
	}
#endif

syscalls:
	fio->class = CLASS_SYSCALL;
	fio->xr = *_recfm_tab[CLASS_SYSCALL];
	nextfio = XRCALL(fio,openrtn) name, flags, mode, fio,
			spec, retstat, cbits, cblks, oinf);
	return(nextfio);
}

#ifdef	_UNICOS
/*
 * _gsys_qtape()
 *	Determine whether the file represented by the stat pointer is a 'tape'.
 *	return 0 if not a tape.
 *	return 1 if it is a tape.
 */
int
_gsys_qtape(struct stat *sb)
{
/*
 *	If it is not a character special device, don't bother.
 */
	if (S_ISCHR(sb->st_mode) == 0)
		return(0);
#ifdef	_CRAY2
	if (MAJOR(sb->st_rdev) == 13) /* !!! */
		return(1);
#else
/*
 *	Do the stat on "/dev/bmxdem" only once.
 */
	if (bmxstat == 0) {
		bmxstat = 1;				/* don't do it again. */
		if (stat("/dev/bmxdem", &_bmx_statbuf) < 0)
			bmxstat = -1;			/* don't do it again. */
	}
/*
 *	See if the device is the same as "/dev/bmxdem".  If it is,
 *	it is a tape.
 */
	if (bmxstat > 0) {
		if (MAJOR(sb->st_rdev) == MAJOR(_bmx_statbuf.st_rdev))
			return(1);
	}
#endif
	return(0);
}
#endif	/* _UNICOS */
