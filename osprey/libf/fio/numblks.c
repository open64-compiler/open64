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



#pragma ident "@(#) libf/fio/numblks.c	92.1	06/21/99 10:37:21"
 
#include <stdio.h>
#include <foreign.h>
#include <errno.h>
#include <liberrno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "fio.h"
 
#ifdef	_UNICOS
_f_int8
NUMBLKS(
#elif defined(__mips) || defined(_LITTLE_ENDIAN)
_f_int8
numblks_(
#else
_f_int
numblks_(
#endif
_f_int *unump
)
{
	int 		n;
#if	defined(_UNICOS) || defined(__mips) || defined(_LITTLE_ENDIAN)
	_f_int8		retval;
#else
	int 		retval;
#endif
	unum_t 		unum;
	unit		*cup;
	struct stat 	buf;
	struct fiostate	cfs;

	unum	= *unump;

	STMT_BEGIN(unum, 0, T_NUMBLKS, NULL, &cfs, cup);

	if (cup == NULL && !GOOD_UNUM(unum))
		_ferr(&cfs, FEIVUNIT, unum);	/* invalid unit number */

	if (cup == NULL)
		retval	= -1;			/* unit is not open */
	else if (cup->usysfd == -1)
		retval	= 0;			/* file is not disk-resident */
	else {
		n	= fstat(cup->usysfd, &buf);
		if (n < 0)
			_ferr(&cfs, errno);

		/* Convert bytes to blocks */

		retval	= (buf.st_size + 4095) >> 12;
	}

	STMT_END(cup, T_NUMBLKS, NULL, &cfs);	/* unlock the unit */
	return(retval);
}
