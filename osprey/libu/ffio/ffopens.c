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


#pragma ident "@(#) libu/ffio/ffopens.c	92.3	10/29/99 21:40:31"

#include <ffio.h>
#include <liberrno.h>
#include <stdarg.h>
#include <string.h>
#include <unistd.h>
#include <cray/mtlock.h>
#include "spec_parse.h"
#if defined(_UNICOS) || defined(__mips) || defined(_LITTLE_ENDIAN)
#include "fflock.h"
#endif

#define MAX_SPEC	128	/* arbitrary limit on size of the layer list */

/*
 *	ffopens
 *
 *	Open an FFIO file.  Apply -F attribute specified by "cspec", which is 
 *	of the same form as that used in assign and asgcmd, without the '-F'.
 *	("cos:20,bmx")
 *
 *	Calling sequence:
 *
 *		fd = ffopens(name, flags, mode, cbits, [cblks,] stat, cspec);
 *
 *	Return value:
 *
 *		Positive integer (really a pointer on Cray MPP and PVP )
 *		on success.   -1 on failure with stat->sw_error
 *		set to the error code.
 */

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
int 
ffopens(
const char	*name,
int		flags,
mode_t		mode,
long		cbits,
int		cblks,
struct ffsw	*stat,
const char	*cspec)

#else
int
ffopens(
const char	*name,
int		flags,
int		mode,
long		cbits,
...)
#endif
{
	int		retfd, length;
	_ffopen_t	fd;
	union spec_u	specs[MAX_SPEC];
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
	char		*cspec;
	int		cblks;
	struct ffsw	*stat;
	va_list		ap;
#endif
	struct gl_o_inf	gloinf;
	struct fdinfo	*nfio;

/*
 *	The cblks argument was not passed in UNICOS 7.0 and previous.
 */
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
	va_start(ap, cbits);

	switch (_numargs()) {
	case 6:
		cblks	= 0;
		stat  	= va_arg(ap, struct ffsw *);
		cspec  	= va_arg(ap, char *);
		break;
	default:		/* 7 or more args */
		cblks	= va_arg(ap, int);
		stat  	= va_arg(ap, struct ffsw *);
		cspec  	= va_arg(ap, char *);
		break;
	}
	va_end(ap);
#endif

/*
 *	Initialize global open information structure.
 */
	(void) memset(&gloinf, 0, sizeof(gloinf));
	gloinf.aip	= NULL;

	if (cspec == NULL) cspec = "";
	length = _parse_forstr(specs, cspec, MAX_SPEC, NO, _LELVL_RETURN);
	if (length <= 0)
		ERETURN(stat, FDC_ERR_BADSPC, 0);

	fd = _ffopen(name, flags, mode, specs, stat, cbits, cblks, NULL,
		&gloinf);

#if defined(_CRAY1) || defined(__mips)
	if (fd != _FFOPEN_ERR  && MULTI_ON) {
		nfio = NULL;
		if (_ff_top_lock(fd, &nfio, stat) < 0)
			fd = _FFOPEN_ERR;	
		if (nfio != NULL)
			fd = (_ffopen_t)nfio;
	}
#endif
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
        /*
         * ffopens returns an int. Call a routine which associates an
         * int with what is returned by _ffopen
         */
        retfd = _ff_fdinfo_to_int(fd, stat);
#else
        retfd = (int)fd;
#endif

	return(retfd);
}

