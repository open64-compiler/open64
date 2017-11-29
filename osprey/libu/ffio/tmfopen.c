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


#pragma ident "@(#) libu/ffio/tmfopen.c	92.1	06/29/99 13:16:47"


#include "tmfio.h"
#include "sysio.h"
#include "fxlist.h"
#include <stdio.h>
#include <fcntl.h>
#include <malloc.h>
#include <sys/types.h>
#include <sys/stat.h>

DECLARE(TMF_XLIST);
struct xtr_s TMF_XLIST_P = { TMF_XLIST };

/*
 * Open a tape file. 
 */
_ffopen_t
_tmf_open(
	const char	*name,
	int		flags,
	mode_t		mode,
	struct fdinfo	*fio,
	union spec_u	*spec,
	struct ffsw	*stat,
	long		cbits,
	int		cblks,
	struct gl_o_inf *oinf
)
{
	int		oflag;
	char		*ptr;
	struct tmfio	*xf_info;
	struct tsdata	tsi;
/*
 *	Allocate private storage
 */
	xf_info	= (struct tmfio *)calloc(sizeof(struct tmfio), 1);

	if (xf_info == NULL) goto nomem;
	fio->lyr_info	= (char *)xf_info;

	xf_info->rwflag	= POSITIN;
/*
 *	This is the lowest layer. Open the file.
 */
	oflag	= flags & (~O_CREAT);

	LOOP_SYSCALL(xf_info->tmf_fd, open(name, oflag, mode));

	if (xf_info->tmf_fd < 0) {
		free(xf_info);
		_SETERROR(stat, errno, 0);
		return(_FFOPEN_ERR);
	}
/*
 *	Determine if we can read/write tape marks
 */
	if (_tmf_tptsi(xf_info, &tsi, NULL)) {
		_SETERROR(stat, errno, 0);
		(void) close(xf_info->tmf_fd);
		free(xf_info);
		return(_FFOPEN_ERR);
	}
	if (tsi.ts_urwfm == 1)
		xf_info->tmf_rwtpmk	= 1;
/*
 *	The buffer size will be the maximum record size.
 *	So, it is also mbs.
 *	JAS: This means that we should NOT allow assign to specify -b.
 */
	xf_info->tmf_bufsiz	= tsi.ts_mbs;
	ptr	= malloc(xf_info->tmf_bufsiz);

	if (ptr == NULL) {
		close(xf_info->tmf_fd);
		goto nomem;
	}

	xf_info->tmf_base	= ptr;
	xf_info->tmf_bufptr	= xf_info->tmf_base;
	SETSTAT(stat, 0, 0);

	return(0);

nomem:
	free(xf_info);
	_SETERROR(stat, FDC_ERR_NOMEM, 0);
	return(_FFOPEN_ERR);
}
