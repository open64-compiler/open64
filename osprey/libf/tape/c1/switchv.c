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


static char USMID[] = "@(#) libf/tape/c1/switchv.c	92.0	10/08/98 14:30:10";

#include <errno.h>
#include <ffio.h>
#include <liberrno.h>
#include "fio.h"

#define ERET(x) {				\
			if (narg > 1) {		\
				*istat	= x;	\
				goto done;	\
			}			\
			_ferr(css, x);		\
		}
/*
 *	CLOSEV - Switches tape volume
 *
 *      unump - name or unit number of file
 *
 *	istat
 *         0  OK
 *         nonzero- No error
 */

void
CLOSEV(_f_int *unump, _f_int *istat)
{
	register int	narg;
	register int	ret;
	FIOSPTR		css;
	unit		*cup;

#ifdef __mips
	narg = 2;
#else
	narg	= _numargs();

	if (narg > 1)
	
#endif
		*istat	= 0;

	GET_FIOS_PTR(css);
	STMT_BEGIN(*unump, 0, T_TAPE, NULL, css, cup);

	if (cup == NULL)
		_ferr(css, FENOTOPN);

        if (cup->ufs == FS_FDC ) {
		ret	= XRCALL(cup->ufp.fdc, fcntlrtn)cup->ufp.fdc,
				FC_CLOSEV, 0, &cup->uffsw);
		if (ret < 0)
			ERET(cup->uffsw.sw_error);
	}
	else
                _ferr(css, FECONNTP);

done:
	cup->uend	= BEFORE_ENDFILE;

	STMT_END(cup, T_TAPE, NULL, css);

	return;
}
