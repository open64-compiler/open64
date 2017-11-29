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



#pragma ident "@(#) libu/waio/c1/openwa.c	92.1	08/19/99 14:46:33"

#include  <stdio.h>
#include  <fcntl.h>
#include  <string.h>
#include  <malloc.h>
#include  <errno.h>
#include  <ffio.h>
#include  <cray/assign.h>
#include  <sys/types.h>
#include  <sys/stat.h>
#include  <sys/listio.h>
#include  "../waio.h"

int G@DSNMAXD	= (int)&_dsnmax_d;	/* Max DRIO files (value)*/
int G@DSNMAXW	= (int)&_dsnmax_w;	/* Max WAIO/MSIO files (value)*/

WAFIL *wafils	= NULL;

void G@OPENWA();

/*
 *	OPENWA - open word addressable dataset   
 */

void *_setuptr();

void
OPENWA(
	long	*dn,	/* pointer to null-terminated file name */
	long	*index,
	long	*eoi,
	long	**addr,
	long	*blocks,
	long	*sector,
	long	*ier	/* optional error return. If this parameter is */
			/* not present, we abort on error */
)
{
	WAFIL		*f;
	assign_info	ai;
	int		aifound;
	char		*nmstr;
	unum_t		unitnum;
	unum_t		unitid;
	int		rfd;
	char		c;
	char		*ptr;
	struct ffc_info_s ffi;
	struct fdinfo	*fio;
	struct ffsw	iostat;
	int		errflg = 0;
	long		*erptr;

	unitnum	= -1;			/* assume no unit number */
	unitid	= *(unum_t *) dn;	/* unit ID is 'name' */

	if (_numargs() > 6)
		errflg	= 1;
/*
 *	Check the assign environment for user requested changes to the
 *	default file characteristics.  
 */
	if (strncmp((char *)dn, "fort.", 5) == 0) {
		register unum_t	unum;

		ptr	= (char *)dn + 5;
		unum	= 0;

		while (isdigit(c = *ptr++)) {
			unum	= unum * 10;
			unum	= unum + ((int) c - (int) '0');
		}
		if (c == '\0') {
			unitnum	= unum;
			unitid	= unum;
		}
	}


	aifound	= _assign_asgcmd_info((char *)dn, unitnum, ASN_G_ALL,
		&ai, NULL, 1);
	if (aifound == -1) {
		if (errflg) {
			*ier	= -errno;
			_errwa_msg(errno);
			return;
		}
		else
			_errwa_abort(errno);
	}

	if (aifound == 1 && ai.a_actfil_flg)	/* if actual file assigned */
		nmstr	= ai.a_actfil;
	else
		nmstr	= (char *)dn;

	if (errflg)
		erptr	= ier;
	else
		erptr	= NULL;

	G@OPENWA(nmstr, index, eoi, addr, blocks, &aifound, &ai,
		 NULL, erptr, sector);

	if (erptr && *erptr != 0) {
		_errwa_msg(-(*erptr));
		return;
	}

	f	= wafils + (*index-1);
/*
 *	The file name is stored only if we're not being called from libf
 *	via the '-s bin' mechanism.
 */
	(void) strncpy(f->wa_idn, (char*)dn, WA_NAMLEN);

	fio	= GETIOB(f->wa_fd);

	if (f->wa_fdc == YES) {
		if (XRCALL(fio, fcntlrtn)fio, FC_GETINFO, &ffi, &iostat) < 0) {
			if (errflg) {
				*ier	= -iostat.sw_error;
				_errwa_msg(iostat.sw_error);
				return;
			}
			else
				_errwa("OPENWA", "Fcntlrtn error on", f,
					iostat.sw_error);
		}
		rfd	= ffi.ffc_fd;
	}
	else
		rfd	=  f->wa_fd;

	f->wa_up	= _setuptr(unitid, rfd, (char *)dn, nmstr,
			(aifound ? &ai : NULL));

	if (f->wa_up == NULL) {
		if (errflg) {
			*ier	= -errno;
			_errwa_msg(errno);
		}
		else
			_errwa_abort(errno);
	}

	return;
}


/*
 *	Macros used in G@OPENWA.
 */

#define RET0(x,a,b,c) {				\
	if (istat == NULL)			\
		_lerror(_LELVL_ABORT,x,a,b,c);	\
	else {					\
		retval	= -x;			\
		goto ret;			\
	}					\
}

#define RET(x)  {retval = -x; goto ret;}
#define RET1(x)  {retval = -x; goto ret1;}

#define SPECSZ 4	/* need 4 words for SDS spec */

/*
 *	G@OPENWA - allows detailed handling of WAIO file opens
 */
void
G@OPENWA(
char	*nmstr,
long	*index,			/* (i) 1 based index into WAFIL table */
long	*eoi,			/* (o) assigned the 0-based index of the last
				 * 512-word block in the file.  -1 if the
				 * file size is 0. */
long	**addr,			/* (o) assigned the address of the buffer */
long	*blocks,		/* (i) buffer size in 512-word blocks */
long	*aifoundp,
assign_info *aip,
int	*sysfdp,		/* (o) assigned the system file descriptor
				 * if this is a disk file, -1 otherwise.  This
				 * parameter is NULL if WOPEN/OPENMS/OPENDR 
				 * was called by the user directly. */
long	*istat,			/* (o) assigned 0 on normal completion, the
				 * negative of the specific error code on
				 * error.  If this parameter is NULL, errors
				 * result in an abort. */
long	*sector)		/* (o) assigned the sector size in words
				 * from st_blksize of the stat structure */
{
	WAFIL		*f;
	long		bufsz,*tmp;
	struct stat 	buf;
	int		presize;
	int		flags;
	long		mask;
	struct fdinfo	*fio;
	struct ffsw	iostat;
	struct ffc_info_s ffi;
	extern char	*_g_asg_entry();
	extern char	*_g_fchar_opt();
	extern char	*_g_fchar_opts();
	union spec_u	*fdspec;
#ifndef _CRAYMPP
	union spec_u	*_A_SDS_();
#endif
	int		retval = 0, fd;
	union spec_u	sdsspec[SPECSZ];
	struct fflistreq *lp;
	struct gl_o_inf	gloinf;
	int		mode;
	int		cbits = 0;
	int		cblks = 0;

	if (istat != NULL)
		*istat	= 0;

	if (wafils == NULL) {
		wafils	= (WAFIL *) malloc(DSNMAX * sizeof(WAFIL) );

		if (wafils == NULL)
			RET1(FENOMEMY);
	}

	f	= wafils + (*index - 1);

	bufsz		= *blocks; /* default number of blocks in buffer */
	f->wa_buffer	= NULL;
	f->wa_fd	= -1;
	f->wa_idn[0]	= '\0';	/* initialize the name to the empty string */

/*
 *	Check the assign environment for user requested changes to the
 *	default file characteristics.  
 */
	f->wa_fdc	= NO;
	f->wa_up	= NULL;
	fdspec		= NULL;
	presize		= 0;

	if (*aifoundp == 1) {
		if (aip->F_filter_flg) {
			fdspec	= &aip->F_filter[0];
			f->wa_fdc	= YES;
		}
#ifndef _CRAYMPP
		if (aip->a_sdsfil_flg) {
/*
 *			Better not have both "-F sds" AND "-a SDS"
 */
			if (fdspec != NULL)
				RET(FEINTUNK);

			if (aip->n_preall_flg)
				presize	= aip->n_preall;
			else
				RET(FESDSFSS);
/*
 *			set up spec
 */
			fdspec	= _A_SDS_(sdsspec, presize, SPECSZ);

			if (fdspec == NULL)
				RET(FENOMEMY);

			f->wa_fdc	= YES;
		}
#endif
	}

/*
 *	Set asynch status to DONE in anticipation of first i/o
 *      request.  Clear error flag.  Note that FFSTAT is set non-zero.
 *	If FFIO is used, this is changed on I/O operations.  If not,
 *	then its being non-zero remains true and does not get in the way
 *	of async operations.
 */
	f->wa_iosw.sw_flag	= 1;
	f->wa_iosw.sw_error	= 0;
	f->wa_iosw.sw_count	= 0;
	FFSTAT(f->wa_iosw)	= FFBOD;	/* Set to non-zero value */

/*
 *      Initialize global open information structure.
 */
	(void) memset(&gloinf, 0, sizeof(gloinf));

	gloinf.aip	= *aifoundp ? aip : NULL;

/*
 *	Open the file, applying relevant assign options.  
 */
	mode	= O_RAW;

	if (*aifoundp)
		_ae_setoflags(aip, &mode);

	if (*aifoundp && aip->pr_partit_flg) {
		mode   |= O_PLACE;
		cbits	= aip->pr_partit;
	}

	if (*aifoundp && (aip->n_stride_flg || aip->q_ocblks_flg)) {
		mode   |= O_PLACE;
		if (aip->q_ocblks_flg)
			cblks	= aip->q_ocblks;
		else
			cblks	= aip->n_stride;
	}

	if (f->wa_fdc == YES) {
		flags		= O_RDWR | O_CREAT | mode;
		f->wa_fd	= _ffopen(nmstr, flags, 0666, fdspec, &iostat,
					cbits, cblks, NULL, &gloinf);
		if (f->wa_fd < 0) {
			flags		= O_RDONLY | mode;
			f->wa_fd	= _ffopen(nmstr, flags, 0666, fdspec,
						&iostat, cbits, cblks, NULL,
						&gloinf);

			if (f->wa_fd < 0)
				RET(errno);
		}
		fio	= GETIOB(f->wa_fd);
/*
 *		Must be capable of seek, binary, stream and random
 */
		mask	= FFC_STRM | FFC_SEEKA | FFC_BINARY | FFC_RDM;

		if (XRCALL(fio, fcntlrtn)fio, FC_GETINFO, &ffi, &iostat) < 0)
			RET(iostat.sw_error);

		if ((ffi.ffc_flags & mask) != mask)
			RET(FEOPCAPY);	/* insufficient capability */
/*
 *		Since this is a stream interface, this is OK!
 */
		if (XRCALL(fio, fcntlrtn)fio, FC_STAT, &buf, &iostat) < 0)
			RET(iostat.sw_error);

		fd	= ffi.ffc_fd;
	}
	else {
		flags		= O_RDWR | O_CREAT | mode;
		f->wa_fd	= open(nmstr, flags, 0666, cbits, cblks);

		if (f->wa_fd < 0) {
			flags		= O_RDONLY | mode;
			f->wa_fd	= open(nmstr, flags, 0666, cbits, cblks);
			if (f->wa_fd < 0)
				RET(errno);
		}

		if (fstat(f->wa_fd, &buf) < 0)
			RET(errno);

		fd	= f->wa_fd;
	}

/*
 *	Record the system file descriptor to the *sysfdp parameter.
 */
	if (sysfdp != NULL)
		*sysfdp	= fd;

/*
 *	Unlink if the -t attribute is assigned.
 */
	if (*aifoundp == 1 && aip->t_tmpfil_flg) /* if file is temp */
		(void) unlink(nmstr);

/*
 *	Allocate the WA buffer
 */
  	if (bufsz > 0) {
		f->wa_buffer	= malloc( ((unsigned int)bufsz * NBPBLK) );

		if (f->wa_buffer == NULL)
			RET(FENOMEMY);

  		tmp	= (long *)f->wa_buffer;   
		*addr	= tmp;
	}
/*
 *	Get size of file.  Pad the size up to the nearest block boundary.
 */
	*eoi	=  -1;

 	if (buf.st_size >  0)  
		*eoi	= ( (buf.st_size + NBPBLK - 1) / NBPBLK ) - 1;

	*sector	= buf.st_blksize / sizeof(long); /* sector size in words */
/*
 *	call to preallocate
 */
	if (fd >= 0) {
		/*
		 * Don't call _prealloc unless there is a valid file 
		 * descriptor. File types SDS.SCR or MR.SCR will not 
		 * have a valid file descriptor
		 */
		if (_prealloc(fd, *aifoundp, aip, &buf) != 0) {
			RET(errno);
		}
	}
/*
 *	Fill up the list with those things that will not change.
 */
	lp			= &f->wa_list;
	lp->li_fildes		= f->wa_fd;	/* really fio if ffio */
	lp->li_status		= &f->wa_iosw;
	lp->li_flags		= LF_LSEEK;
	lp->li_memstride	= 0;
	lp->li_filstride	= 0;
	lp->li_nstride		= 1;
	lp->li_signo		= 0;
 	return;				/* NORMAL EXIT --------*/

/*
 *	Error return point.  Release any allocated memory, close files, and
 *	return.
 */
ret:
	if (f->wa_buffer != NULL)
		free(f->wa_buffer);	

	if (f->wa_fd >= 0) {
		if (f->wa_fdc == YES)
			(void) XRCALL(fio, closertn)fio, &iostat);
		else
			(void) close(f->wa_fd);
	}

ret1:
	if (istat == NULL)
		_errwa_abort(-retval);

	*istat	= retval;

	return;
}
