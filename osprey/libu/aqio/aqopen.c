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


#pragma ident "@(#) libu/aqio/aqopen.c	92.1	07/01/99 13:50:36"

#include <fcntl.h>
#include <errno.h>
#include <malloc.h>
#include <liberrno.h>
#include <ffio.h>
#include <fortran.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/listio.h>
#include <sys/iosw.h>
#include <sys/stat.h>
#include <infoblk.h>
#include "aqio.h"
#ifdef _ADDR64
#include <stdarg.h>
#endif
#include <cray/assign.h>
#include <cray/nassert.h>

#pragma _CRI soft _setuptr
extern void	*_setuptr();

static AQFIL *_aqopen();

static int zero = 0;

#define RET(x) {retval = x; goto errret;}
#define RET1(x) {retval = x; goto erret1;}
#define SPECSZ 4	/* size of SDS spec */


/*
 *	AQOPENM - open file for asynchronous queued i/o. Use
 *      the oflags passed in by the user
 *      Parameters:
 *	aqp - 		OUTPUT parameter. handle to use when referencing
 *			the file in other AQIO calls.
 *	aqpsize - 	INPUT parameter. maximum number of queued requests.
 *	name		INPUT parameter. name of file to be opened
 *			must be a Fortran character string.
 *	mode		INPUT parameter. oflags bits to use when opening the 
 *			file (e.g., O_RDWR | O_RAW)
 *	ustatus -	OUTPUT parameter. Status. 0 == OK
 */
void
AQOPENM(void **aqp, long *aqpsize, _fcd name, long *mode, long *ustatus)
{
	char		nbuf[PATH_MAX+1];
	unum_t		uid;

	if (_numargs() != 4+sizeof(_fcd)/sizeof(long))
		_lerror(_LELVL_ABORT, FEARGLST, "AQOPENM");

	uid	= -1;
	_f2ccpy(name, nbuf, PATH_MAX);
	*aqp	= _aqopen(nbuf, uid, *aqpsize, 1, *mode, 1, ustatus);

	return;
}

/*
 *	AQOPEN - open file for asynchronous queued i/o
 */
#ifndef _ADDR64
void
AQOPEN(void **aqp, long *aqpsize, _fcd name, long *ustatus)
#else
void
/* We use varargs because the 3rd argument could be either _fcd or long * */
AQOPEN(void **aqp, long *aqpsize, ...)
#endif
{
#ifdef _ADDR64
	va_list		args;		/* variable argument list */
	long		*unumptr;
	_fcd		name;
#endif
	char		nbuf[PATH_MAX+1];
	int		statusflag;
	char		*nmstr;
	int		RBN@();
	unum_t		uid, unum;
	long		*status;
	long		dummy;

	nmstr	= &nbuf[0];

#ifdef	_ADDR64
	va_start(args, aqpsize);
	if (_numargs() == 4) {
		unumptr	= va_arg(args, long *);
		unum	= *unumptr;
#else
	if (!_isfcd(name)) {		/* If Hollerith */
		unum	= *(long *)name;
#endif

		/* If high character is nonzero, must be file name... */

		if ((unum >> 56) != 0) {
			uid	= RBN@(&unum);
			(void) memcpy(nbuf, (char *) &uid, sizeof(long));
			nbuf[sizeof(long)]	= '\0';	/* Terminate name */
		}
		else {
			uid	= unum;
			_fortname(nmstr, unum);
		}
	}
	else {
#ifdef	_ADDR64
		name	= va_arg(args, _fcd);
#endif
		uid	= -1;
		_f2ccpy(name, nbuf, PATH_MAX);
	}
#ifdef _ADDR64
	/* Because we want to allow either Fortran character descriptors
	 * or unit numbers as the 3rd argument, "status" is a required
	 * parameter on _ADDR64 machines.
	 */
	status	= va_arg(args, long *);
	va_end(args);
	statusflag	= 1;
#else
	/* Don't store to 'status' if not passed */
	status	= (_numargs() > 3) ? ustatus : &dummy;
	statusflag	= (_numargs() > 3) ? 1 : 0;
#endif
	*aqp	= (void *)_aqopen(nbuf, uid, *aqpsize, 0, 0, statusflag, status);
	return;
}


static
AQFIL *
_aqopen(
	char	*nbuf, 		/* name of file to be opened */
	unum_t	uid, 		/* unit number of file to be opened. */
	long	aqpsize, 	/* maximum number of queued requests */
	int	modeflag, 	/* 1=> use "mode" on the open(2). */
				/* 0=> open however you can. */
	long	mode, 		/* "mode" for open(2) */
	int	statusflag,	/* 1=> return status to user */
	long	*status 	/* status returned here */
)
{
	AQFIL		*f = NULL;
	struct fflistreq *list;
	struct ffsw	*iostat = NULL;
	struct ffsw	ffstat;
#ifdef _MPP_LSTIOKLUDGE
/* when we have compound listio, this can be deleted */
	struct compound *compound = NULL;
	struct fflistreq *cmp_list = NULL;
	struct ffsw *cmp_iostat = NULL;
	int maxcompound;
	char *cmpstr;
#endif

	int		fd, listsz;
	int		i;
	int		aqpsz;
	int		sz;
	int		aifound;
	assign_info	ai;
	char		*nmstr;
	char		*uname;
	struct stat	buf;
	int		retval;
	int		rfd;
	int		ffio;
	long		dummy;
	long		mask;
	struct fdinfo	*fio;
	struct ffc_info_s ffi;
	union spec_u	specspace[SPECSZ];
	union spec_u	*fdspec;
	struct gl_o_inf gloinf;
#ifndef _CRAYMPP
	extern union spec_u *_A_SDS_();
#endif

/*
 *	Check the file environment for user requested changes to the
 *	default file characteristics.  File structure changes are
 *	not allowed, unless 'U' is specified.
 */

	nmstr	= nbuf;
	aifound	= _assign_asgcmd_info(nbuf, uid, 
			ASN_G_ALL | ASN_G_AQ, &ai, NULL, 1);
	if (aifound == -1)
		RET1(errno);
	uname	= nmstr;
	if (aifound == 1 && ai.a_actfil_flg) {	/* if actual file assigned */
		nmstr	= ai.a_actfil;		/* change file name */
	}
	ffio	= NO;
	if (aifound == 1 && ai.F_filter_flg) {
		fdspec	= &ai.F_filter[0];
		ffio	= YES;
	}
#ifndef _CRAYMPP
	if (aifound && ai.a_sdsfil_flg) {
		int presize;

		if (ai.n_preall_flg)
			presize	= ai.n_preall;
		else
			RET1(FESDSFSS);	/* SDS allocation failure */

		fdspec	= _A_SDS_(specspace, presize, SPECSZ);
		ffio	= YES;
	}
#endif
/*
 *	Open the file.
 */
	if (ffio == NO) {
		int flag;
		int cbits = 0;
		int cblks = 0;
		/* See if the user specified assign -l */
		if (modeflag == 0)
			flag	= O_RAW;
		else
			flag	= mode;

		/* handle -r, -L, -x, -w, -B assign options */
		if (aifound) 
			_ae_setoflags(&ai, &flag);
	
		/* See if the user specified assign -p or -q */
		if (aifound && ai.pr_partit_flg) {
			flag   |= O_PLACE;
			cbits	= ai.pr_partit;
		}
		if (aifound && (ai.n_stride_flg || ai.q_ocblks_flg)) {
			flag   |= O_PLACE;
			if (ai.q_ocblks_flg)
				cblks	= ai.q_ocblks;
			else
				cblks	= ai.n_stride;
		}
		if (modeflag == 0) {
			fd	= open(nmstr, O_RDWR | O_CREAT | flag, 0666, cbits, cblks);
			if (fd < 0) {
				fd	= open(nmstr, O_RDONLY | flag, 0666, cbits, cblks);
				if (fd < 0) {
					fd	= open(nmstr, O_WRONLY | O_CREAT | flag, 0666, cbits, cblks);
				}
			}
		}
		else 
			fd	= open(nmstr, flag, 0666, cbits, cblks);
		if (fd < 0) {
			RET1(errno);
		}
		rfd	= fd;
		if (fstat(rfd, &buf) < 0)	/* Get size for stripe */
			RET(errno);
	} else {
/*
 *		Initialize global open information structure.
 */
		(void) memset(&gloinf, 0, sizeof(gloinf));
		gloinf.aip	= aifound ? &ai : NULL;

		if (modeflag == 0) {
			fd	= _ffopen(nmstr, O_RDWR | O_CREAT | O_RAW,
					0666, fdspec, &ffstat, 0, 0, NULL,
					&gloinf);
			if (fd < 0) {
				fd	= _ffopen(nmstr, O_RDONLY | O_RAW, 0666,
					fdspec, &ffstat, 0, 0, NULL, &gloinf);
				if (fd < 0) {
					fd	= _ffopen(nmstr, O_WRONLY | O_CREAT | O_RAW,
						0666, fdspec, &ffstat, 0, 0, NULL, 
						&gloinf);
				}
			}
		}
		else
			fd	= _ffopen(nmstr, mode, 0666, fdspec,
				&ffstat, 0, 0, NULL, &gloinf);
		if (fd < 0) {
			RET1(errno);
		}
/*
 *		Must be capable of seek, binary, stream and random
 */
		fio	= GETIOB(fd);
		mask	= FFC_STRM | FFC_SEEKA | FFC_BINARY | FFC_RDM;
		if (XRCALL(fio, fcntlrtn) fio, FC_GETINFO, &ffi, &ffstat) < 0)
			RET(ffstat.sw_error);
		if ((ffi.ffc_flags & mask) != mask)
			RET(FEOPCAPY);	/* insufficient capability */
		rfd	= ffi.ffc_fd;
		if (XRCALL(fio, fcntlrtn) fio, FC_STAT, &buf, &ffstat) < 0)
			RET(ffstat.sw_error);
	}
/*
 *	Since this is a circular queue, one entry will always be unused.
 *	make up for this by allocating one extra slot.
 */
	aqpsz	= aqpsize + 1;
	if (aqpsz <= 1)		/* Invalid aqpsize */
		RET(FEOPAQPZ);

/*
 *	These check that AQFIL are a multiple of words.  The compiler should
 *	optimize them away if the tests fail.
 */
	if (sizeof(struct fflistreq) & 7) abort();
	if (sizeof(struct listreq) != sizeof(struct fflistreq)) abort();
	if (sizeof(AQFIL) & 7) abort();
/*
 *	Malloc enough space for the AQFIL header, and all the queue
 *	elements.
 */
	listsz	= aqpsz * sizeof(struct fflistreq);
	f	= (AQFIL *) calloc(sizeof(AQFIL) + listsz, 1);
	if ( f == NULL)
		RET(FENOMEMY);
/*
 *	Start the list queue after the AQFIL header data.
 */
	sz	= sizeof(AQFIL) >> 3;		/* AQFIL size in words */
	list	= (struct fflistreq *) ((int *)f + sz);

	f->aq_base	= list;
	f->aq_busy	= list;
	f->aq_ptr	= list;
	f->aq_nxtq	= list;
	f->aq_limit	= list + aqpsz;

	iostat	= (struct ffsw *) calloc(aqpsz * sizeof(struct ffsw),1);
	if ( iostat == NULL)
		RET(FENOMEMY);

	f->aq_reqid	= (int *) calloc( aqpsz, sizeof(int));
	if ( f->aq_reqid == NULL)
		RET(FENOMEMY);

	f->aq_told	= (int *) calloc( aqpsz, sizeof(int));
	if ( f->aq_told == NULL)
		RET(FENOMEMY);
/*
 *	Set up those things in the list which are never changed, and
 *	those things that are assumed on first I/O.
 */
	for ( i = 0; i < aqpsz; i++ ) {
		iostat[i].sw_flag	= 1;
		FFSTAT(iostat[i])	= FFBOD;/* convenient, non-zero */
		list[i].li_flags	= LF_LSEEK;	/* always used. */
		list[i].li_fildes	= fd;	/* only one file allowed. */
		list[i].li_status	= &iostat[i];
		list[i].li_signo	= 0;	/* never uses signals */
	}

#ifdef _MPP_LSTIOKLUDGE
/* When we have support for compound listio, this can be removed */
	compound	= calloc(sizeof(struct compound),1);
	if (compound == NULL)	
		RET(FENOMEMY);
	f->aq_cmp_ptr	= compound;
	cmpstr	= getenv("MAXAQCMPND");
	if (cmpstr == NULL) {
		maxcompound	= DEFMAXCMP;
	}
	else {
		maxcompound	= atoi(cmpstr);
		if (maxcompound <= 0)
			maxcompound	= DEFMAXCMP;
	}
	compound->cmp_maxreq	= maxcompound;
	cmp_list	= calloc(maxcompound* sizeof(struct fflistreq),1);
	if (cmp_list == NULL)
		RET(FENOMEMY);
	compound->cmp_base	= cmp_list;
	compound->cmp_nxt	= compound->cmp_base;
	compound->cmp_unrcalld	= compound->cmp_base;
	compound->cmp_desc	= calloc(maxcompound * sizeof(struct fflistreq *),1);
	if (compound->cmp_desc == NULL)	
		RET(FENOMEMY);
	cmp_iostat	= (struct ffsw *) malloc(maxcompound * sizeof(struct ffsw));
	if ( cmp_iostat == NULL)
		RET(FENOMEMY);
	for ( i = 0; i < maxcompound; i++ ) {
		cmp_iostat[i].sw_flag	= 1;	/* done */
		cmp_list[i].li_flags	= LF_LSEEK;	/* always used. */
		cmp_list[i].li_fildes	= fd;		/* only one file allowed. */
		cmp_list[i].li_status	= &cmp_iostat[i];
		cmp_list[i].li_signo	= 0;		/* never uses signals */
		cmp_list[i].li_nstride	= 1;		/* Always a simple request */
		cmp_list[i].li_filstride	= 0;	/* Always a simple request */
		cmp_list[i].li_memstride	= 0;	/* Always a simple request */
	}
	
#endif
	f->aq_ffio	= ffio;

	if (uid >= 0) {
		if (LOADED(_setuptr))
			f->aq_up	= _setuptr(uid, rfd, uname, nmstr,
					(aifound ? &ai : NULL));
		if (f->aq_up == NULL)
			RET(errno);
	}
	else
		f->aq_up	= NULL;


	/* call to preallocate */

	if (rfd >= 0){
		/*
		 * Don't call _prealloc unless there is a valid file 
		 * descriptor. File types SDS.SCR or MS.SCR will not have 
		 * a valid file descriptor.
		 */
		if (_prealloc(rfd, aifound, &ai, &buf) != 0)
			RET(errno);
	}
	if (aifound ==1 && ai.t_tmpfil_flg && rfd >= 0) {
		(void) unlink(nmstr);
	}
/*
 *	Return the pointer to the user, and set status to OK.
 */
	*status	= 0;
	f->aq_chk	= AQ_CHK;
	return(f);

errret:
	if (f != NULL) {
		if (f->aq_reqid != NULL)
			free(f->aq_reqid);
		if (f->aq_told != NULL)
			free(f->aq_told);
		if (f->aq_up != NULL)
			_clruptr(f->aq_up);
#ifdef _MPP_LSTIOKLUDGE
/* When we have support for compound listio on the MPP, this can be deleted */
		if (f->aq_cmp_ptr != NULL) {
			if (cmp_list != NULL)
				free(cmp_list);
			if (compound->cmp_desc != NULL)
				free(compound->cmp_desc);
			if (cmp_iostat != NULL)
				free(cmp_iostat);
			free(f->aq_cmp_ptr);
		}
#endif
		free(f);
	}
	if (iostat != NULL)
		free(iostat);
	if (ffio == YES)
		(void)XRCALL(fio, closertn) fio, &ffstat);
	else
		(void)close(fd);
erret1:
	*status	= -retval;
	if (statusflag != 1)
		if (retval == FEOPNOFS)
			_lerror(_LELVL_ABORT, -(*status),
				"requested","asynchronous","queued");
		else		
			_lerror(_LELVL_ABORT, -(*status));
	return(NULL);
}
