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


static char USMID[] = "@(#) libu/ffio/c1/ffsetsp.c	92.0	10/08/98 14:57:41";


#include <errno.h>
#include <stdarg.h>
#include <ffio.h>
extern void _eov_load();

/*
 * Initiate special end of volume processing for tapes.
 * This is necessary so that eov routines can be soft referenced
 * in bmxfcntl.c
 */

ffsetsp(int fd, ...)
	{
	struct fdinfo *fio;
	struct ffsw locstat, *pstat;
	int ret, na;
	va_list ap;

	fio = GETIOB(fd);
	NUMARG(na);
	if (na < 2)
		pstat = &locstat;
	else
		{
		va_start(ap, fd);
		pstat = va_arg(ap, struct ffsw *);
		}
	CHECK_FIOPTR(fio, pstat);
	_eov_load(1);	/* Call a routine that provides hard references */
			/* to eov routines. */
	ret = XRCALL(fio, fcntlrtn) fio, FC_SETSP, 1, pstat);
	if (na < 2)
		errno = locstat.sw_error;
	return (ret);
	}
