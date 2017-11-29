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


/* USMID @(#) libu/ffio/sysio.h	92.0	10/08/98 14:57:41 */

#include <unistd.h>
#include <stdlib.h>
#ifdef	__mips
#include <aio.h>
#endif

#define SYS_flags_AIO_CB 1

#ifdef	_CRAY
#define INTIO	G@INTIO 
#else
#define INTIO	_intio 
#endif

extern int INTIO; /* zero if interrupted system calls should be restarted */

struct sys_f {
	unsigned 
		nointrio:1,	/* set if interrupted I/O should not be */
				/* restarted */
		tape:1,		/* 1 == is a tape */
		oappend:1,	/* 1 == is opened oappend */
		needpos:1,	/* 1 == we are not at the correct position */
				/* This happens when we use aio_ calls on mips*/
				/* or when we call pread/pwrite */
		aio_callback:1;	/* Use SIGEV_CALLBACK rather than SIGEV_NONE */
	char	*name;		/* file name, if known */
#ifdef	__mips
	off_t	curpos;		/* current position */
	off_t	endpos;		/* position of end of file */
#endif
	};
	
#define LOOP_SYSCALL(ret, action)				\
	do	{						\
		ret = action;					\
		} while(ret < 0 && INTIO == 0 && errno == EINTR)

#ifdef	__mips
/*
 * Initialize aio
 *
 * aio_threads is the number of threads that the aio library will use to
 * queue up operations.  Five is default under Irix 6.2.  aio_locks is
 * also set to default under Irix 6.2.  aio_numusers is the number of
 * sprocs or pthreads that the application contains.  According to the
 * man page:
 * Due to its use of IRIX semaphores (see usconfig(3P)) Multi-threaded
 * programs must pass the number of sprocs or pthreads that the application
 * contains to the AIO library.  Passing a number that is too small can
 * result in program deadlocks and other errors.  The current default is
 * 5.  We pick a larger number:  MAX(number of processors, 64)
 */

#define AIO_INIT(){\
	if ((!_ffaio_initialized)) {	\
		aioinit_t	ai;	\
		long	_sl;		\
		char	*userval;	\
		_ffaio_initialized = 1;	\
		ai.aio_threads = 5;	\
		ai.aio_locks = 3;	\
		_sl = sysconf(_SC_NPROC_CONF); \
		ai.aio_numusers = (64 >_sl ? 64 : _sl); \
		userval = getenv("FF_IO_AIO_THREADS");	\
		if (userval){	\
			ai.aio_threads = atoi(userval);	\
		}		\
		userval = getenv("FF_IO_AIO_NUMUSERS");	\
		if (userval){	\
			ai.aio_numusers = atoi(userval);	\
		}		\
		userval = getenv("FF_IO_AIO_LOCKS");	\
		if (userval){	\
			ai.aio_locks = atoi(userval);	\
		}		\
		aio_sgi_init(&ai); \
	}\
}
#endif
/*
 *	Prototypes
 */

extern _ffopen_t _sys_open(const char *name, int flags, mode_t mode,
	struct fdinfo *fio, union spec_u *spec, struct ffsw *retstat,
	long cbits, int cblks, struct gl_o_inf *oinf);

extern _ffopen_t _fd_open(const char *name, int flags, mode_t mode, 
	struct fdinfo *fio, union spec_u *spec, struct ffsw *retstat,
	long cbits, int cblks, struct gl_o_inf *oinf);

extern _ffopen_t _gsys_open(const char *name, int flags, mode_t mode,
	struct fdinfo *fio, union spec_u *spec, struct ffsw *retstat, 
	long cbits, int cblks, struct gl_o_inf *oinf);

extern int _fd_check(const char *name, int flags, mode_t mode,
	struct fdinfo *fio, union spec_u *spec, struct ffsw *retstat, 
	long cbits, int cblks, struct gl_o_inf *oinf);

extern ssize_t _sys_read(struct fdinfo *fio, bitptr bufptr, size_t nbytes,
	struct ffsw *retstat, int fulp, int *ubc);

extern ssize_t _sys_write(struct fdinfo *fio, bitptr bufptr, size_t nbytes,
	struct ffsw *retstat, int fulp, int *ubc);

extern _ffseek_t _sys_lseek(struct fdinfo *fio, off_t pos, int whence, 
	struct ffsw *stat);
