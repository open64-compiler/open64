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



#pragma ident "@(#) libf/fio/skipf.c	92.1	06/21/99 10:37:55"

#include <errno.h>
#include <ffio.h>
#include <liberrno.h>
#include "fio.h"

/*
 *	Skip files
 *	dn - unit 
 *	nb - number of files to skip. Negative for skip backwards.
 *	istat - optional 2 word array.
 *		 First word = number of records skipped (not always set)
 *		 Second word = number of files skipped. Only count when
 *			we cross a file boundary.
 *	retstat - optional return status
 */
#define ERET(x) { \
	if (narg > 3) {		\
		*retstat = x;	\
		 goto ret;	\
	} else			\
		_ferr(&cfs, x);	\
}

void
SKIPF(
	_f_int	*unump,
	_f_int8	*nb,
	_f_int	*istat,
	_f_int	*retstat)
{
	register int	narg;
	register unum_t	unum;
	unit		*cup;
	struct fiostate	cfs;
	struct ffp_skipf_s	skip_arg;

#ifdef	_UNICOS
	narg	= _numargs();
#else
	narg	= 4;
#endif
	if (narg > 2) {
		*(istat)	= 0;
		*(istat + 1)	= 0;
	}

	unum	= *unump;

	STMT_BEGIN(unum, 0, T_MISC, NULL, &cfs, cup);
/*
 *	If not connected, do an implicit open.  Abort if the open fails.
 */
	if (cup == NULL) {
		int	ostat;

		cup	= _imp_open(&cfs, SEQ, UNF, unum, 1, &ostat);

		if (cup == NULL) {
			ERET(ostat);
		}
	}

	switch (cup->ufs) {
		case FS_FDC:
			skip_arg.ffp_nfil	= *nb;
			skip_arg.ffp_nrec	= 0;
			if(XRCALL(cup->ufp.fdc, posrtn) cup->ufp.fdc,
				FP_SKIPF, &skip_arg, 2, &cup->uffsw) < 0)
				ERET(errno);	
			if (narg > 2) {
				*(istat+1)	= skip_arg.ffp_nfil;
				*(istat)	= skip_arg.ffp_nrec;
			}
			break;
		default:
			ERET(FENOSKPF);
	}

ret:
	cup->uwrt	= 0;
	cup->uend	= BEFORE_ENDFILE;

	STMT_END(cup, T_MISC, NULL, &cfs);

	return;
}
