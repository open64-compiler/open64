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



#pragma ident "@(#) libf/fio/rewind.c	92.1	06/21/99 10:37:55"

#include <errno.h>
#include <foreign.h>
#include <liberrno.h>
#include "fio.h"

/*
 *	_REWF   Fortran rewind
 */

#ifdef 	_UNICOS
#pragma _CRI duplicate _REWF as $REWF
#endif

int
_REWF(
	_f_int  *unump,			/* Fortran unit number */
	_f_int  *iostat,		/* IOSTAT= variable address, or NULL */
	int     errf)			/* 1 if ERR= specifier is present */
{
	register int	errn;		/* Error status */
	register unum_t	unum;
	unit		*cup;
	struct ffsw	fst;
	struct fiostate	cfs;

	errn	= 0; 
	unum	= *unump; 

	STMT_BEGIN(unum, 0, T_REWIND, NULL, &cfs, cup); /* lock the unit */

	if (!GOOD_UNUM(unum)) {
		errn	= FEIVUNIT;	/* Invalid unit number */
		goto rewind_done;
	}

/*
 *	REWIND on unopened unit is OK, and does nothing.
 */
	if (cup == NULL) 
		goto rewind_done;

	if (cup->useq == 0) {    /* If file opened for direct access */
		errn	= FERWNDIV;	/* REWIND invalid on dir. acc.*/
		goto rewind_done;
	}

/*
 *      Wait for completion of a preceding asynchronous BUFFER IN/OUT.
 */
	WAITIO(cup, {errn = cup->uffsw.sw_error;});

	if (errn != 0) 
		goto rewind_done;

	if (cup->pnonadv) {		/* There is a current record */
		if (cup->uwrt) {

			errn	= _nonadv_endrec(&cfs, cup);

			if (errn != 0)
				goto rewind_done;
		}
		cup->pnonadv	= 0;	/* Flag no current record */
	}

	cup->uend	= BEFORE_ENDFILE;
	cup->ulastyp	= DT_NONE;
	cup->urecpos	= 0;

	if ( cup->uwrt ) {
		if (cup->utrunc) {
			/*
			 * Truncate file after last sequential write before 
			 * this REWIND.
			 */
			errn	= _unit_trunc(cup);

			if (errn != 0)
				goto rewind_done;
		}
		cup->uwrt	= 0;
	}

/*
 *	Switch on file structure
 */
	switch (cup->ufs) {

	case FS_FDC:
		errn	= XRCALL(cup->ufp.fdc, seekrtn) cup->ufp.fdc, 0, 0, &fst);
		if (errn < 0)
			errn	= fst.sw_error;
		break;

	case FS_TEXT:
	case STD:
		/*
		 * Can't rewind pipes, sockets, ttys.
		 */
		if (!cup->useek) {
			errn	= FENORWPI;	/* can't rewind pipes */ 
			goto rewind_done;
		}

		errn	= fseek(cup->ufp.std, 0, 0);

		if (errn != 0)
			errn	= errno;
		break;

	case FS_BIN:
		cup->uwaddr	= 1;
		break;

	case FS_AUX:
		errn	= FEMIXAUX;	/* REWIND not allowed on a WAIO, 
					 * MSIO, DRIO, or AQIO file. */
		break;

	default:
		errn	= FEINTFST;
		break;
	}

rewind_done:
	if (iostat != NULL)
		*iostat	= errn;
	else
		if (errn != 0 && (errf == 0))
			_ferr(&cfs, errn, unum);/* Pass unum to _ferr *
						 * in case of FEIVUNIT error */

	STMT_END(cup, T_REWIND, NULL, &cfs);	/* unlock the unit */

	errn	= (errn != 0) ? IO_ERR : IO_OKAY; /* Set error status */

	return(CFT77_RETVAL(errn));
}
