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


#pragma ident "@(#) libu/ffio/fferr.c	92.1	06/29/99 13:16:47"

#include <ffio.h>
#include <errno.h>
#include <stdlib.h>
#include "fxlist.h"

DECLARE(ERR_XLIST);
struct xtr_s ERR_XLIST_P = { ERR_XLIST };

/*
 * _ff_err() is a dummy routine placeholder.  It returns a bad status.
 * and is used in those routines like reada and writea where the
 * stat parameter is parameter #4
 */

int
_ff_err(struct fdinfo *fio, bitptr bufptr, size_t nbytes, struct ffsw *stat, int fulp, int *ubc)
{
	int na;

#ifdef	_CRAY
	NUMARG(na);
#else
	na = 6;
#endif
	if (na == 6 || na == 5) /* if read/write[ca] */
		_SETERROR(stat, FDC_ERR_NOSUP, 0)
	else
		abort();
	return(ERR);
}

/*
 * _ff_err2 is used when the stat parameter is param #2
 */
int
_ff_err2(
struct fdinfo	*fio,
struct ffsw	*stat)
{
	int na;

#ifdef	_UNICOS
	NUMARG(na);
#else
	na = 2;
#endif
	if (na == 2)
		_SETERROR(stat, FDC_ERR_NOSUP, 0)
	else
		abort();
	return(ERR);
}

/*
 * _ff_err_listio is used when the listio function is not supported
 */
int
_ff_err_listio(
int			cmd,		/* LC_WAIT or LC_START */
struct fflistreq	*list,		/* list of requests (see fflistio) */
int			nreq,		/* number of requests */
struct ffsw		*stat)		/* status structure */
{
	_SETERROR(stat, FDC_ERR_NOSUP, 0)
	return(ERR);
}

/*
 * This routine is called instead of the character conversion routines
 *	when that brand of conversion is disabled.
 */
int
_ff_err_c_disabl()
{
	return(FDC_ERR_CCVRT);
}

ssize_t
_ff_err_rw(struct fdinfo *fio, bitptr bufptr, size_t nbytes, struct ffsw *stat, int fulp, int *ubc)
{
	_SETERROR(stat, FDC_ERR_NOSUP, 0)
	return((ssize_t)-1);
}

ssize_t
_ff_err_rwc(struct fdinfo *fio, bitptr bufptr, size_t nbytes, struct ffsw *stat, int fulp)
{
	_SETERROR(stat, FDC_ERR_NOSUP, 0)
	return((ssize_t)-1);
}

_ffseek_t
_ff_err_seek(struct fdinfo *fio, off_t pos, int whence, struct ffsw *stat)
{
	_SETERROR(stat, FDC_ERR_NOSUP, 0)
	return((_ffseek_t)-1);
}

_ffseek_t
_ff_err_pos(struct fdinfo *fio, int cmd, void *arg, int l, struct ffsw *stat)
{
	_SETERROR(stat, FDC_ERR_NOSUP, 0)
	return((_ffseek_t)-1);
}


/*
 * _ff_err_open is used when the return type is _ffopen_t.
 */
_ffopen_t
_ff_err_open(const char *name, int flags, mode_t mode, struct fdinfo *fio,
 union spec_u *spec, struct ffsw *stat, long cbits, int cblks, 
 struct gl_o_inf *oinf)
{
	_SETERROR(stat, FDC_ERR_NOSUP, 0)
	return((_ffopen_t)-1);
}
int
_ff_err_fcntl(struct fdinfo *fio, int cmd, void *arg,  struct ffsw *stat)
{
	_SETERROR(stat, FDC_ERR_NOSUP, 0)
	return(-1);
}

