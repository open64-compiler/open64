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


#pragma ident"@(#) libu/ffio/ffpos.c	92.2	10/11/99 15:30:43"

#include <errno.h>
#include <ffio.h>

/*
 * ffpos()
 *	position a dataset/file.  returns zero on success.  ERR on failure
 *	with the stat->sw_error containing error code.
 *
 *	All parameters are mandatory
 *
 *	fd - file descriptor (dummy)
 *	cmd - command code
 *	arg - pointer to input or output data
 *	len - length of the space available at arg.  This is used
 *		primarily on output to avoid overwriting the available
 *		memory.
 *	stat - status return parameter
 */

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
off_t
#else
int
#endif
ffpos(
int fd,
int cmd,
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
void *arg,
#else
long *arg,
#endif
int len,
struct ffsw *stat
)
	{
	struct fdinfo *fio;
	_ffseek_t ret;

	fio = GETIOB(fd);
	CHECK_FIOPTR(fio, stat);
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
	if (cmd == FP_BSEEK)
		ERETURN(stat, FDC_ERR_NOSUP, 0);
#endif
	ret = XRCALL(fio, posrtn) fio, cmd, arg, len, stat);
	return (ret);
	}

_ffseek_t
_ff_pos(
struct fdinfo	*fio,
int		cmd,
void		*argp,
int		len,
struct ffsw	*stat)
{
	int ret;
	long bytes;
	long *arg;
	arg = (long *)argp;

	switch (cmd) {
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
	/* Before supporting this on MIPS, we need to look at what the */
	/* arguments should be. A long (for *arg) is not big enough */
	/* to hold the total file offset on 32-bit machines */
	case FP_BSEEK:
/*
 *		This should be changed someday to properly
 *		handle a bit position...
 */
		if ((*arg & 7) != 0)
			ERETURN(stat, FDC_ERR_BADSK, 0);
		if (*arg < 0) {
			bytes = (-(*arg))>>3;
			bytes = -bytes;
		}
		else
			bytes = *arg>>3;

		ret = XRCALL(fio, seekrtn) fio, bytes, *(arg+1), stat);

		return(_dshiftl(ret, ret, 3)); 

#endif
	default:
		ERETURN(stat, FDC_ERR_NOSUP, 0);
	}
}
