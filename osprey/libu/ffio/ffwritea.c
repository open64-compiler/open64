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


#pragma ident "@(#) libu/ffio/ffwritea.c	92.1	06/29/99 13:16:47"

#include <errno.h>
#include <stdarg.h>
#include <ffio.h>
/*
 * ffwritea()
 * Write data to a file/dataset.  This is a driver that starts the chain
 * of calls down through the layers of foreign dataset routines.
 *
 */

ffwritea(int fd, char *buf, int nbytes, struct ffsw *stat, ...)
        { 
        struct fdinfo *fio;
        int ret; 
        bitptr bufptr;   
        int locubc, *pubc, na;        /* need a place to put result */ 
	int locfulp;
	va_list ap;

        fio = GETIOB(fd);
        SET_BPTR(bufptr, CPTR2BP(buf));
        /* adjust number of bits requested if ubc passed in */ 
        NUMARG(na);
	locubc = 0;
        pubc = &locubc;
        locfulp = FULL;
	if (na < 4 || na > 6)
		{
		errno = FDC_ERR_NOPARM;
		return(ERR);
                }
	va_start(ap, stat);
	if (na > 4)
		locfulp = va_arg(ap, int);
	if (na > 5)
		pubc = va_arg(ap, int *);
	CHECK_FIOPTR(fio, stat);
        ret = XRCALL(fio, writeartn) fio, bufptr, nbytes,
						stat, locfulp, pubc);
        return (ret);
        }
