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


static char USMID[] = "@(#) libf/tape/c1/checktp.c	92.0	10/08/98 14:30:10";

#include <errno.h>
#include <foreign.h>
#include <liberrno.h>
#include "fio.h"

/*
 *	CHECKTP - Checks tape I/O status
 *
 *      unump - unit number of file
 *
 *	istat
 *	  -1  No status
 *         0  EOV
 *	   1  tape off reel
 *         2  tape mark detected
 *	   3  blank tape detected
 *
 *      icbuf 
 *	   Number of blocks in library buffers 
 */

void
CHECKTP(long *unump, long *istat, long *icbuf)
{
	register int	ret;
	unit		*cup;
	FIOSPTR		css;
	struct ffc_chktp_s chktp;

	GET_FIOS_PTR(css);
	STMT_BEGIN(*unump, 0, T_TAPE, NULL, css, cup);

	if (cup == NULL)
		_ferr(css, FENOTOPN);

	switch(cup->ufs) {
		case FS_FDC:
			ret	= XRCALL(cup->ufp.fdc, fcntlrtn) cup->ufp.fdc,
					FC_CHECKTP, &chktp, &cup->uffsw);
			if (ret < 0) 
				_ferr(css, cup->uffsw.sw_error);
			*istat	= chktp.stat;
			*icbuf	= chktp.libblk;
			break;
		default:
                	_ferr(css, FECONNTP);
	}

	STMT_END(cup, T_TAPE, NULL, css);

	return;
}
