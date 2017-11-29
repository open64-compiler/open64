/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

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



#pragma ident "@(#) libf/fio/ftell.c	92.1	06/18/99 19:52:04"

#include <errno.h>
#include <ffio.h>
#include <liberrno.h>
#include "fio.h"
#include "fstats.h"

extern long long __ftell64_f90( _f_int *unump);
extern _f_int8 ftellf90_8_4_( _f_int *unump);
extern _f_int8 ftellf90_8_( _f_int8 *unump);
extern _f_int ftell90_ (_f_int *u);

#ifdef KEY /* Bug 1683 */

void
pathf90_ftell4(_f_int *unit, _f_int *offset) {
  _f_int junk;
  *offset = (_f_int) ftellf90_8_4_(unit);
  }

void
pathf90_ftell8(_f_int8 *unit, _f_int8 *offset) {
  *offset = ftellf90_8_(unit);
  }

void
pathf90_ftell48(_f_int *unit, _f_int8 *offset) {
  *offset = ftellf90_8_4_(unit);
  }

#endif /* KEY Bug 1683 */

/*
 * __ftell64_f90, _ftell
 *
 * ftell64_f90 - the ftell_ and ftell64_ functions use part of
 *	 	 the GETPOS routine to return the current file
 *		 position.
 *
 * A module interface will call these directly.
 *
 * calling sequence:
 *
 *	INTEGER CURPOS, FTELL, LUNIT
 *	CURPOS = ftell(LUNIT)
 *
 *	or:
 *
 *	INTEGER LUNIT
 *	INTEGER*8 FTELL64, CURPOS64
 *	CURPOS64 = ftell64(LUNIT)
 *
 * where:
 *
 *	lunit:
 *	     	- an open logical unit
 *
 *	curpos or curpos64:
 *
 *		- current offset in bytes from the start of the
 *		  file associated with that logical unit or a
 *		  negative system error code.
 *
 */

long long
__ftell64_f90( _f_int 	*unump)
{
	return ftellf90_8_4_(unump);
}

_f_int8
ftellf90_8_4_( _f_int 	*unump)
{
	_f_int8		pos;
	register unum_t	unum;
	unit 		*cup;
	struct fiostate	cfs;

	unum	= *unump;

	/* lock the unit */
	STMT_BEGIN(unum, 0, T_GETPOS, NULL, &cfs, cup);

/*
 *	If not connected, do implicit open.  Abort if open fails.
 */
	if (cup == NULL)
		cup	= _imp_open(&cfs, SEQ, UNF, unum, 0, NULL);

	/* if direct access file */
	if (cup->useq == 0)
		_ferr(&cfs, FEBIONDA, "GETPOS");

/*
 *	Make the appropriate call depending on file structure to
 * 	get the current file position.  Postion routines are file
 *	structure dependent.
 *
 *	Simulate the IPOS=GETPOS(IUN) call
 */
	pos	 = 0;
	switch( cup->ufs ) {

	case  FS_TEXT:
	case  STD:
#ifdef KEY /* Bug 1678 */
		pos	= ftello(cup->ufp.std);
#else /* KEY Bug 1678 */
		pos	= ftell(cup->ufp.std);
#endif /* KEY Bug 1678 */
		break;

	case FS_FDC:
		_ferr(&cfs, FDC_ERR_NOSUP);
		break;

	case FS_AUX:
		_ferr(&cfs, FEMIXAUX);
		break;
	default:
		_ferr(&cfs, FEINTFST);

	}

getpos_done: 
	STMT_END(cup, T_GETPOS, NULL, &cfs);
	return(pos);
}

_f_int8
ftellf90_8_( _f_int8 *unump)
{
	_f_int8		pos;
	register unum_t	unum;
	unit 		*cup;
	struct fiostate	cfs;

	unum	= *unump;

	/* lock the unit */
	STMT_BEGIN(unum, 0, T_GETPOS, NULL, &cfs, cup);

/*
 *	If not connected, do implicit open.  Abort if open fails.
 */
	if (cup == NULL)
		cup	= _imp_open(&cfs, SEQ, UNF, unum, 0, NULL);

	/* if direct access file */
	if (cup->useq == 0)
		_ferr(&cfs, FEBIONDA, "GETPOS");

/*
 *	Make the appropriate call depending on file structure to
 * 	get the current file position.  Postion routines are file
 *	structure dependent.
 *
 *	Simulate the IPOS=GETPOS(IUN) call
 */
	pos	 = 0;
	switch( cup->ufs ) {

	case  FS_TEXT:
	case  STD:
#ifdef KEY /* Bug 1678 */
		pos	= ftello(cup->ufp.std);
#else /* KEY Bug 1678 */
		pos	= ftell(cup->ufp.std);
#endif /* KEY Bug 1678 */
		break;

	case FS_FDC:
		_ferr(&cfs, FDC_ERR_NOSUP);
		break;

	case FS_AUX:
		_ferr(&cfs, FEMIXAUX);
		break;
	default:
		_ferr(&cfs, FEINTFST);

	}

getpos_done: 
	STMT_END(cup, T_GETPOS, NULL, &cfs);
	return(pos);
}

#ifdef KEY /* Bug 1683 */
/* Don't pollute the Fortran namespace */
_f_int
ftell90_(_f_int *u)
{
	return( (_f_int) ftellf90_8_4_(u));
}
#endif /* KEY Bug 1683 */

