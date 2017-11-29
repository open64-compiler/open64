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



#pragma ident "@(#) libf/fio/ttynam.c	92.3	11/16/99 15:43:33"
 
/*
 * Return name of tty port associated with lunit
 *
 * calling sequence:
 *
 *	character*19 string, ttynam
 * 	string = ttynam (lunit)
 *
 * where:
 *
 *	the character string will be filled with the name of
 *	the port, preceded with '/dev/', and blank padded.
 *	(19 is the max length ever required)
 */

#include <stdio.h>
#include <foreign.h>
#include <errno.h>
#include <liberrno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include "fio.h"

extern void __ttynam_f90 (char *name, int strlen, int *u);
extern void ttynamf90_(char *name, _f_int strlen, int *u);
extern void ttynamf90_8_(char *name, _f_int strlen, _f_int8 *u);

#ifdef KEY /* Bug 1683 */

/* Provide subroutine interface as well as function interface */
void
pathf90_ttynam(int *u, char *name, int strlen)
{
  __ttynam_f90(name, strlen, u);
}

#endif /* KEY Bug 1683 */

void
__ttynam_f90 (char *name, int strlen, int *u)
{
	ttynamf90_(name,strlen,u);
	return;
}

void
ttynamf90_(char *name, int strlen, int *u)
{
	int 		rtrn, errval;
	unum_t 		unum;
	unit		*cup;
	struct fiostate	cfs;
	char *t = NULL;

#if	defined(_LITTLE_ENDIAN)
	char *ttyname(int);
#endif
	unum = *u;
	STMT_BEGIN(unum, 0, T_INQU, NULL, &cfs, cup);

	errval = 0;
	if (cup == NULL && !GOOD_UNUM(unum))
		_ferr(&cfs, FEIVUNIT, unum);	/* invalid unit number */

	if (cup == NULL)
		errval = FEIVUNIT;	/* unit is not open */
	else if (cup->usysfd == -1)
		errval = FEIVUNIT;	/* file is not disk-resident */
	else {
		t = ttyname(cup->usysfd);
	}

	STMT_END(cup, T_INQU, NULL, &cfs);	/* unlock the unit */

	if (t==NULL)
		t="";
	_b_char(t, name, strlen);
	return;
}

#if	defined(_LITTLE_ENDIAN)
void
ttynam_ (char *name, int strlen, int *u)
{
	ttynamf90_(name,strlen,u);
	return;
}
#endif

void
ttynamf90_8_(char *name, int strlen, _f_int8 *u)
{
	int 		rtrn, errval;
	unum_t 		unum;
	unit		*cup;
	struct fiostate	cfs;
	char *t = NULL;

#if	defined(_LITTLE_ENDIAN) && !defined(__sv2)
	char *ttyname(int);
#endif
	unum = *u;
	STMT_BEGIN(unum, 0, T_INQU, NULL, &cfs, cup);

	errval = 0;
	if (cup == NULL && !GOOD_UNUM(unum))
		_ferr(&cfs, FEIVUNIT, unum);	/* invalid unit number */

	if (cup == NULL)
		errval = FEIVUNIT;	/* unit is not open */
	else if (cup->usysfd == -1)
		errval = FEIVUNIT;	/* file is not disk-resident */
	else {
		t = ttyname(cup->usysfd);
	}

	STMT_END(cup, T_INQU, NULL, &cfs);	/* unlock the unit */

	if (t==NULL)
		t="";
	_b_char(t, name, strlen);
	return;
}
