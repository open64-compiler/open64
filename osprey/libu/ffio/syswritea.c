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


#pragma ident "@(#) libu/ffio/syswritea.c	92.1	06/29/99 13:16:47"

#include <sys/types.h>
#ifdef	_CRAY
#include <sys/iosw.h>
#endif
#include <errno.h>
#include <ffio.h>
#include "sysio.h"

#ifdef	__mips
short _ffaio_initialized;
#endif
/*
 * _sys_writea() calls the system call writea(2), after doing appropriate
 * conversions on the parameters
 */

ssize_t
_sys_writea(struct fdinfo *fio, bitptr bufptr, size_t nbytes, struct ffsw *retstat, int fulp, int *ubc)
	{
	int ret = 0;
	char *buf;
	struct sys_f *sys_info ;

	buf = BPTR2CP(bufptr);
	if ((BPBITOFF(bufptr) & 7) != 0 || *ubc != 0)
		ERETURN(retstat, FDC_ERR_UBC, 0);

	if (nbytes > 0)
		{
		CLRFFSTAT(*retstat);	/* flag async in progress */
#ifdef	__mips
		AIO_INIT();
                if (((struct sys_f *)fio->lyr_info)->oappend) {
                        ((struct sys_f *)fio->lyr_info)->curpos =
			((struct sys_f *) fio->lyr_info)->endpos;
                }
		retstat->aiocbp.aio_fildes = fio->realfd;
		retstat->aiocbp.aio_nbytes = nbytes;
		retstat->aiocbp.aio_offset = 
			((struct sys_f *)fio->lyr_info)->curpos;
		retstat->aiocbp.aio_reqprio = 0;
		sys_info = (struct sys_f *)fio->lyr_info ;
                if (sys_info->aio_callback) {
                   extern void _ffio_aio_callback();
                   retstat->aiocbp.aio_sigevent.sigev_notify = SIGEV_CALLBACK;
                   retstat->aiocbp.aio_sigevent.sigev_func  = _ffio_aio_callback ;
                   retstat->aiocbp.aio_sigevent.sigev_value.sival_ptr  = (void *)retstat ;
                }
                else {
                   retstat->aiocbp.aio_sigevent.sigev_notify = SIGEV_NONE;
                }
		retstat->aiocbp.aio_buf = buf;
		retstat->aiocbp.aio_lio_opcode = 0;
		ret = aio_write(&retstat->aiocbp);
		if (ret < 0) {
			if (errno == EAGAIN) {
                                /* The requested asynchronous I/O operation */
                                /* was not queued due to system resource */
                                /* limitations. */
                                /* What to do here? I will try switching */
                                /* to synchronous. */
                                return(XRCALL(fio, writertn) fio,
                                        bufptr, nbytes, retstat, fulp, ubc));
                        }

			ERETURN(retstat, errno, 0);
		}
		/* about all we can do is assume this will complete */
		/* successfully */
		((struct sys_f *)fio->lyr_info)->needpos = 1;
		((struct sys_f *)fio->lyr_info)->curpos += nbytes;
		if (((struct sys_f *)fio->lyr_info)->curpos > 
			((struct sys_f *)fio->lyr_info)->endpos)
			((struct sys_f *)fio->lyr_info)->endpos = 
			((struct sys_f *)fio->lyr_info)->curpos;
#else
		if (((struct sys_f *)fio->lyr_info)->nointrio)
			ret = writea(fio->realfd, buf, nbytes,
				(struct iosw *)retstat, 0);
		else  {
			LOOP_SYSCALL(ret, writea(fio->realfd, buf, nbytes,
				(struct iosw *)retstat, 0));
		}
		if (ret < 0)
			ERETURN(retstat, errno, 0);
#endif	/* __mips */
		}
	else	/* nbytes == 0 */
		{
/* Is this correct for O_APPEND?? */
		retstat->sw_flag = 1;
		FFSTAT(*retstat) = FFCNT; /* I/O is done, and other stat */
					/* fields are already set. */
		}

	return (ret);
	}
