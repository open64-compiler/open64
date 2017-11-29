/*

  Copyright (C) 2000, 2001, Silicon Graphics, Inc.  All Rights Reserved.

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



#pragma ident "@(#) libf/fio/fffcntl.c	92.1	06/18/99 16:08:47"

/*
 *	Provide access to fffcntl call for FORTRAN units
 *
 *	>=0 OK (depends on fffcntl return)
 *	<0  bad call. (-2 means too few params)
 *		if *ustat = -1, unit was not open as FDC, or not open.
 */

#include <foreign.h>
#include <errno.h>
#include "fio.h"
 
long
FFFCNTL(unitnum, cmd, arg, len, ustat)
long 	*unitnum;
long 	*len;
long 	*cmd;
long 	*arg;
long 	*ustat;
	{
	unit 	*cup;
	int	ret;
	struct ffsw stat;

	if (_numargs() < 5)
		return(-2);	/* too few parameters */

	*ustat = -1;	/* assume unit not open...*/
	STMT_BEGIN(*unitnum, 0, T_MISC, NULL, NULL, cup);	/* lock unit */
	if (cup == NULL)
		return(-1);	/* invalid or unconnected unit */

	if (cup->ufs != FS_FDC) {
		ret = -1;
		goto done;
	}

	*ustat = 0;
/*
 *	If command code is zero, just return fdinfo pointer.
 */
	if (*cmd == 0) {
		*arg = (long)cup->ufp.fdc;
		ret  = 0;
		goto done;
	}

	ret = XRCALL(cup->ufp.fdc, fcntlrtn) cup->ufp.fdc, *cmd, arg, &stat);
	if (ret < 0)
		*ustat = stat.sw_error;

done:
	STMT_END(cup, T_MISC, NULL, NULL);
	return(ret);
}
