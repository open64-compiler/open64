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



#pragma ident "@(#) libf/fio/backspace.c	92.1	06/18/99 15:49:57"

#include <errno.h>
#include <foreign.h>
#include <liberrno.h>
#include "fio.h"

/*
 *	_BACK   Fortran backspace
 */

#if	_CRAY 
#pragma _CRI duplicate _BACK as $BACK	/* For cf77 */
#endif

int
_BACK(
	_f_int  *unump,			/* Fortran unit number */
	_f_int  *iostat,		/* IOSTAT= variable address, or NULL */
	int     errf)			/* 1 if ERR= specifier is present */
{
	register int	errn;		/* nonzero when error is encountered */
	register unum_t	unum;
	unit		*cup;
	struct fiostate	cfs;

	errn	= 0; 
	unum	= *unump; 

	STMT_BEGIN(unum, 0, T_BACKSPACE, NULL, &cfs, cup); /* lock the unit */

	if (!GOOD_UNUM(unum)) {
		errn	= FEIVUNIT;	/* Invalid unit number */
		goto backspace_done;
	}

/*
 *	BACKSPACE on unopened unit is OK, and does nothing.  For opened units,
 *	call the low level backspace routine.
 */
	if (cup == NULL) 
		goto backspace_done;

 	if (cup->pnonadv) {		/* There is a current record */
 		if (cup->uwrt) {

 			errn	= _nonadv_endrec(&cfs, cup);

 			if (errn != 0)
 				goto backspace_done;
 		}
 		cup->pnonadv	= 0;	/* Flag no current record */
 	}

	errn	= _unit_bksp(cup);

backspace_done:
	if (iostat != NULL)
		*iostat	= errn;
	else
		if (errn != 0 && (errf == 0))
			_ferr(&cfs, errn, unum);	/* Pass unum to _ferr
						 * in case of FEIVUNIT error */

	STMT_END(cup, T_BACKSPACE, NULL, &cfs);	/* unlock the unit */

	errn	= (errn != 0) ? IO_ERR : IO_OKAY;/* 1 if error; 0 if no error */

	return(CFT77_RETVAL(errn));
}
