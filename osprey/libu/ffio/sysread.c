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


#pragma ident "@(#) libu/ffio/sysread.c	92.1	06/29/99 13:16:47"

#include <errno.h>
#include <ffio.h>
#include <unistd.h>
#include "sysio.h"

/*
 * _sys_read() calls the system call read(2), after doing appropriate
 * conversions on the parameters
 */

ssize_t
_sys_read(
struct fdinfo *fio, 
bitptr bufptr, 
size_t nbytes, 
struct ffsw *retstat,
int fulp, 
int *ubc)
	{
	ssize_t ret = 0;
	char *buf;

	buf = BPTR2CP(bufptr);
	if ((BPBITOFF(bufptr) & 7) != 0 || *ubc != 0)
		ERETURN(retstat, FDC_ERR_UBC, 0);
#ifdef __mips
        /*
         * If our last i/o was asynchronous, then our file position
         * won't be what we expect. Seek to the right position. We
         * could use a pread instead of seeking, but that would also
         * not update the file position. I'm doing this because it seems
         * to me most 'expected' for the system call layer.
         */
        if (((struct sys_f *)fio->lyr_info)->needpos) {
                if (lseek( fio->realfd, ((struct sys_f *)fio->lyr_info)->curpos,
                        0)  < 0)
                        ERETURN(retstat, errno, 0);
                ((struct sys_f *)fio->lyr_info)->needpos = 0;
        }
#endif

	if (nbytes > 0)
		{
		if (((struct sys_f *)fio->lyr_info)->nointrio)
			ret = read(fio->realfd, buf, nbytes);
		else {
			LOOP_SYSCALL(ret, read(fio->realfd, buf, nbytes));
		}
		if (ret < 0)
			ERETURN(retstat, errno, 0);
		}

	if (ret == 0 && nbytes > 0) {
		SETSTAT(retstat, FFEOD, ret);
	}
	else {
		SETSTAT(retstat, FFCNT, ret);
#ifdef __mips
		((struct sys_f *)(fio->lyr_info))->curpos += ret;
#endif
	}
	return (ret);
	}
