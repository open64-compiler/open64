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


#pragma ident "@(#) libu/ffio/syslseek.c	92.1	06/29/99 13:16:47"

#include <errno.h>
#include <ffio.h>
#include <unistd.h>
#include "sysio.h"

/*
 * _sys_lseek() calls the system call lseek(2) and returns status.
 */

_ffseek_t
_sys_lseek(struct fdinfo *fio, off_t pos, int whence, struct ffsw *stat)
	{
	off_t ret;
	
#ifdef __mips
	if ((((struct sys_f *)(fio->lyr_info))->needpos) &&
		whence == SEEK_CUR)
		ret = lseek(fio->realfd,
		    pos + ((struct sys_f *)(fio->lyr_info))->curpos,
		    SEEK_SET);
	else
		ret = lseek(fio->realfd, pos, whence);
#else
	ret = lseek(fio->realfd, pos, whence);
#endif
	if (ret < 0)
		{
		ERETURN(stat, errno, 0);
		}
#ifdef __mips
	((struct sys_f *)(fio->lyr_info))->curpos = ret;
	((struct sys_f *)(fio->lyr_info))->needpos = 0;
#endif
	fio->rwflag = POSITIN;
	if (whence == SEEK_SET && pos == 0)
		{
		fio->ateof = 0;
		fio->ateod = 0;
		SETSTAT(stat, FFBOD, 0);
		}
	else if (whence == SEEK_END && pos == 0)
		{
		fio->ateof = 1;
		fio->ateod = 1; /* seek to end, set stat */
		SETSTAT(stat, FFEOD, 0);
		}
	else
		SETSTAT(stat, FFCNT, 0);

	fio->last_recbits = fio->recbits;
	fio->recbits = 0;
	return (ret);
	}
