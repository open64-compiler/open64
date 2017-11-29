/*
 * Copyright 2004, 2005 PathScale, Inc.  All Rights Reserved.
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



#pragma ident "@(#) libf/fio/isatty.c	92.1	06/21/99 10:37:55"
 
/*
 *
 * isatty_  - determine if stream is associated with tty (async port)
 *
 * calling sequence:
 *
 *	LOGICAL	isatty, val
 *	INTEGER	lunit
 *	val = isatty (lunit)
 *
 * where:
 *
 *	val will be .TRUE. if lunit is associated with a 'tty'
 *
 * Note: the return value will not indicate if there is an error,
 *       errno should be checked.
 *
 * Entry point __isatty_f90 is called for MIPS f77 or for f90 when
 *      there is no compatibility module.
 *
 * Entry point isattyf90_ is called for MIPS f90 when there is a
 *      compatibility module.
 *
 * Entry point isattyf90_8_ is called for MIPS f90 when there is a
 *      compatibility module.
 *
 */

#include <stdio.h>
#include <foreign.h>
#include <errno.h>
#include <liberrno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include "fio.h"

extern _f_int __isatty_f90 (_f_int *u);
#ifdef KEY /* Bug 1683 */
static _f_int isattyf90_(_f_int *u);
#else
extern _f_int isattyf90_(_f_int *u);
#endif /* KEY Bug 1683 */
extern _f_int8 isattyf90_8_(_f_int8 *u);

_f_int
__isatty_f90 (_f_int *u)
{
	return isattyf90_(u);
}

#ifdef KEY /* Bug 1683 */
/* Don't pollute the Fortran namespace */
static
#endif /* KEY Bug 1683 */
_f_int
isattyf90_(_f_int *u)
{
	int 		rtrn, errval;
	unum_t 		unum;
	unit		*cup;
	struct fiostate	cfs;

	unum	= *u;
	STMT_BEGIN(unum, 0, T_INQU, NULL, &cfs, cup);

	errval	= 0;
	if (cup == NULL && !GOOD_UNUM(unum))
		_ferr(&cfs, FEIVUNIT, unum);	/* invalid unit number */

	if (cup == NULL)
		errval	= FEIVUNIT;	/* unit is not open */
	else if (cup->usysfd == -1)
		errval	= FEIVUNIT;	/* file is not disk-resident */
	else {
		rtrn	= isatty(cup->usysfd);
	}

	STMT_END(cup, T_INQU, NULL, &cfs);	/* unlock the unit */

	if (errval!=0)
		errno	= errval;
	return (_btol(rtrn));
}

_f_int8
isattyf90_8_(_f_int8 *u)
{
	int 		rtrn, errval;
	unum_t 		unum;
	unit		*cup;
	struct fiostate	cfs;

	unum	= *u;
	STMT_BEGIN(unum, 0, T_INQU, NULL, &cfs, cup);

	errval	= 0;
	if (cup == NULL && !GOOD_UNUM(unum))
		_ferr(&cfs, FEIVUNIT, unum);	/* invalid unit number */

	if (cup == NULL)
		errval	= FEIVUNIT;	/* unit is not open */
	else if (cup->usysfd == -1)
		errval	= FEIVUNIT;	/* file is not disk-resident */
	else {
		rtrn	= isatty(cup->usysfd);
	}

	STMT_END(cup, T_INQU, NULL, &cfs);	/* unlock the unit */

	if (errval!=0)
		errno	= errval;
	return (_btol(rtrn));
}
