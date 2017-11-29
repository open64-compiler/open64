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


#pragma ident "@(#) libu/ffio/ffopen.c	92.2	10/11/99 15:30:43"

#include <errno.h>
#include <fcntl.h>
#include <ffio.h>
#if !defined(_ABSOFT)
#include <malloc.h>
#else
#include <stdlib.h>
#endif
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <cray/assign.h>
#if defined(_UNICOS) || defined(__mips) || defined(_LITTLE_ENDIAN)
#include "fflock.h"
#endif
/*
 *	ffopen
 *
 *	Open an FFIO file.  Use any -F assign option attributes for the
 *	file name specified by the name argument.
 *
 *	Arguments:
 *
 *		char *name;		name of file to be opened
 *		int flags;		OS flags param.  Same as open(2).
 *		int mode;		(optional) same as open(2).
 *		The following arguments are not available on MIPS systems:
 *		long cbits;		(optional) same as open(2).
 *		struct ffsw *stat;	(optional) ffsw structure to return
 *					error code. If not passed, errno is 
 *					used.
 *		int cblks;		(optional) same as open(2).
 */

ffopen(const char *name, int flags, ...)
{
	int		narg;
	int		cblks;
	_ffopen_t	fd;
	int		retfd;
	int		aifound;
	mode_t		mode;
	long		cbits;
	va_list		ap;
	union spec_u	*fdspec;
	struct gl_o_inf	gloinf;
	assign_info	ai;
	struct fdinfo	*nfio;


	extern union spec_u *_g_fdc_spec();
	struct ffsw *pstat, locstat;

#ifdef	_CRAY
	NUMARG(narg);
#elif   defined(__mips) || defined(_LITTLE_ENDIAN)
	/* mode is passed only when O_CREAT is set */
	if (flags & O_CREAT)
		narg = 3;
	else
		narg = 2;
#else
	narg = 6;
#endif
	mode	= 0;
	cbits 	= 0;
	cblks 	= 0;
	pstat	= &locstat;
/*
 *	New usage only allows 5 params.	     (what does this mean ???)
 */
	va_start(ap, flags);
	if (narg >= 3)
#if defined(BUILD_OS_DARWIN)
		mode	= (mode_t) va_arg(ap, int);
#else /* defined(BUILD_OS_DARWIN) */
		mode	= va_arg(ap, mode_t);
#endif /* defined(BUILD_OS_DARWIN) */
	if (narg >= 4)
		cbits	= va_arg(ap, long);
	if (narg >= 5)
		pstat	= va_arg(ap, struct ffsw *);
	if (narg >= 6)
		cblks	= va_arg(ap, int);

	va_end(ap);

 	aifound = _assign_asgcmd_info(name, -1, ASN_G_FF | ASN_G_ALL, &ai,
			NULL, 1);
	if (aifound == -1) {
		ERETURN(pstat, errno, 0);
	}

	if (aifound == 1 && ai.F_filter_flg)
		fdspec = &ai.F_filter[0];
	else
		fdspec = NULL;

	(void) memset(&gloinf, 0, sizeof(gloinf));
        gloinf.aip	= aifound ? &ai : NULL;

	fd = _ffopen(name, flags, mode, fdspec, pstat, cbits, cblks, NULL,
		&gloinf);

#if defined(_CRAY1) || defined(__mips)
	if (fd != _FFOPEN_ERR && MULTI_ON) {
		nfio = NULL;
		if (_ff_top_lock(fd, &nfio, pstat) < 0)
			fd = _FFOPEN_ERR;	
		if (nfio != NULL)
			fd = (_ffopen_t)nfio;
	}
#endif
	/*
	 * ffopen returns an int. Call a routine which associates an
	 * int with what is returned by _ffopen 
	 */
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
	retfd = _ff_fdinfo_to_int(fd, pstat);
#else
	retfd = (int)fd;
#endif
	/* should check chain of layers here for sanity */
	if (narg < 4)
		errno = locstat.sw_error;

	return(retfd);
}
/*
 * This routine is like ffopen, except it expects all parameters
 */
ffopenf(const char *name, int flags, mode_t mode, long cbits, int cblks,
	struct ffsw *pstat)
{
	int		narg;
	_ffopen_t	fd;
	int		retfd;
	int		aifound;
	union spec_u	*fdspec;
	struct gl_o_inf	gloinf;
	assign_info	ai;
	struct fdinfo	*nfio;

	extern union spec_u *_g_fdc_spec();

 	aifound = _assign_asgcmd_info(name, -1, ASN_G_FF | ASN_G_ALL, &ai,
			NULL, 1);
	if (aifound == -1) {
		ERETURN(pstat, errno, 0);
	}

	if (aifound == 1 && ai.F_filter_flg)
		fdspec = &ai.F_filter[0];
	else
		fdspec = NULL;

	(void) memset(&gloinf, 0, sizeof(gloinf));
        gloinf.aip	= aifound ? &ai : NULL;

	fd = _ffopen(name, flags, mode, fdspec, pstat, cbits, cblks, NULL,
		&gloinf);

#if defined(_CRAY1) || defined(__mips)
	if (fd != _FFOPEN_ERR && MULTI_ON) {
		nfio = NULL;
		if (_ff_top_lock(fd, &nfio, pstat) < 0)
			fd = _FFOPEN_ERR;
		if (nfio != NULL)
			fd = (_ffopen_t)nfio;
	}
#endif
	/*
	 * ffopen returns an int. Call a routine which associates an
	 * int with what is returned by _ffopen 
	 */
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
	retfd = _ff_fdinfo_to_int(fd, pstat);
#else
	retfd = (int)fd;
#endif
	/* should check chain of layers here for sanity */

	return(retfd);
}
