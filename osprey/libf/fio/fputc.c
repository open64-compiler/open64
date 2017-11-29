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



#pragma ident "@(#) libf/fio/fputc.c	92.2	08/20/99 13:51:31"

/*
 * fputc_ - write a character to a logical unit bypassing f77
 * 	    formatted I/O. fputc is called from f77 and f90 programs
 * 	    on mips only.  fgetc only applies to unit 6.  The clen
 * 	    arg is ignored since only a single character is written.
 *
 * calling sequence:
 *
 *	INTEGER fputc, unit, ierror
 *	ierror = fputc (unit, char)
 *
 * where:
 *
 *	char	= write char to the logical unit
 *	ierror	= 0 if successful
 *		= a system error code otherwise.
 *
 * Call fputc_ from MIPS F77 or from MIPS F90 without a
 *      compatibility module.
 *
 * Call fputc_f90 from MIPS F90 without a compatibility module.
 *
 * Call fputcf90_ from MIPS F90 with a compatibility module.
 * Call fputcf90_8_ from MIPS F90 with a compatibility module.
 * Call fputcf90_8_4_ from MIPS F90 with a compatibility module.
 * Call fputcf90_4_8_ from MIPS F90 with a compatibility module.
 *
 */

#include "fio.h"

#ifdef KEY /* Bug 1683 */
static _f_int fputcf90_(_f_int *u, char *c, int clen);
extern _f_int8 fputcf90_8_(_f_int8 *u, char *c, int clen);
extern _f_int8 fputcf90_8_4_(_f_int4 *u, char *c, int clen);
extern _f_int4 fputcf90_4_8_(_f_int8 *u, char *c, int clen);
static int putc_(char *c, int clen);
static _f_int4 putcf90_(char *c, int clen);
extern _f_int8 putcf90_8_(char *c, int clen);
#else
extern int __fputc_f90(int *u, char *c, int clen);
extern _f_int fputcf90_(_f_int *u, char *c, int clen);
extern _f_int8 fputcf90_8_(_f_int8 *u, char *c, int clen);
extern _f_int8 fputcf90_8_4_(_f_int4 *u, char *c, int clen);
extern _f_int4 fputcf90_4_8_(_f_int8 *u, char *c, int clen);
extern int putc_(char *c, int clen);
extern _f_int4 putcf90_(char *c, int clen);
extern _f_int8 putcf90_8_(char *c, int clen);
#endif /* KEY Bug 1683 */

#ifdef KEY /* Bug 1683 */
int
__fputc_f90(int *u, char *c, int *status, int clen)
{
	int junk;
	status = (status == 0) ? (&junk) : status;
	return *status = fputcf90_(u, c, clen);
}

int
pathf90_fput(char *c, int *status, int clen)
{
	int junk;
	status = (status == 0) ? (&junk) : status;
	return *status = putc_(c, clen);
}

#else
int
__fputc_f90(int *u, char *c, int clen)
{
	return fputcf90_(u, c, clen);
}
#endif /* KEY Bug 1683 */

#ifdef KEY /* Bug 1683 */
/* Don't pollute the Fortran namespace with library functions */
static
#endif /* KEY Bug 1683 */
_f_int
fputcf90_(_f_int *u, char *c, int clen)
{
	_f_int		res;
	struct fiostate	cfs;		/* fiosp */
	unit		*cup;		/* Unit table pointer   */
	unum_t		unum;
	long		inpbuf;

	unum	= *u;
	res	= 0;
	
	/* lock the unit */
	STMT_BEGIN( unum, 0, T_WSF, NULL,  &cfs, cup);
#ifdef KEY /* Bug 1683 */
	/* Copied from rf90.c; list-directed uses SEQ, so we do too */
	if (cup == NULL) {	/* If not connected */
	  int stat;
	  int errf = 0;

	  cup= _imp_open(&cfs, SEQ, FMT, unum, errf, &stat);
	  if (0 == cup) {
	    return errno = stat;
	  }
	}
#endif /* KEY Bug 1683 */

	if (unum < 0 || !cup)
		return((errno=FEIVUNIT));

	/* move the character to a character per word for fwch */
	inpbuf	= (long) *c;
	if (_fwch(cup, &inpbuf, 1, PARTIAL) == -1)
		res	= errno;

	/* unlock the unit */
	STMT_END( cup, TF_WRITE, NULL,  &cfs);

	return(res);
}

_f_int8
fputcf90_8_(_f_int8 *u, char *c, int clen)
{
	_f_int8		res;
	struct fiostate	cfs;		/* fiosp */
	unit		*cup;		/* Unit table pointer   */
	unum_t		unum;

	unum	= *u;
	res	= 0;

	/* lock the unit */
	STMT_BEGIN( unum, 0, T_WSF, NULL,  &cfs, cup);

	if (unum < 0 || !cup)
		return((errno=FEIVUNIT));

	if (_fwch(cup, (long *)c, 1, PARTIAL) == -1)
		res	= errno;

	/* unlock the unit */
	STMT_END( cup, TF_WRITE, NULL,  &cfs);

	return(res);
}

_f_int8
fputcf90_8_4_(_f_int *u, char *c, int clen)
{
	return (_f_int8) fputcf90_(u, c, clen);
}

_f_int4
fputcf90_4_8_(_f_int8 *u, char *c, int clen)
{
	_f_int8 uunit;
	uunit = *u;
	return (_f_int4) fputcf90_8_(&uunit, c, clen);
}

/*
 * putc_ - write a character to the standard output on mips
 *
 * calling sequence:
 *
 *	INTEGER putc, ierror
 *	ierror =  putc (char)
 *
 * where:
 *
 *	char	= char to write to the standard output
 *	ierror	= 0 if successful
 *		= a system error code otherwise.
 *
 * Call putc_ from MIPS F77 or from MIPS F90 without a
 *      compatibility module.  Note that it calls fputc with unit 6.
 *
 * Call putcf90_ from MIPS F90 with a compatibility module.
 * Call putcf90_8_ from MIPS F90 with a compatibility module.
 *
 */

#ifdef KEY /* Bug 1683 */
/* Don't pollute the Fortran namespace with library functions */
static
#endif /* KEY Bug 1683 */
int
putc_(char *c, int clen)
{
	int stdout_unit = 6;
	return fputcf90_(&stdout_unit, c, clen);
}

#ifdef KEY /* Bug 1683 */
/* Don't pollute the Fortran namespace with library functions */
static
#endif /* KEY Bug 1683 */
_f_int4
putcf90_(char *c, int clen)
{
	_f_int4		res;
	struct fiostate	cfs;		/* fiosp */
	unit		*cup;		/* Unit table pointer   */
	unum_t		unum = 6;

	res	= 0;

	/* lock the unit */
	STMT_BEGIN( unum, 0, T_WSF, NULL,  &cfs, cup);

	if (!cup)
		return((errno=FEIVUNIT));

	if (_fwch(cup, (long *)c, 1, PARTIAL) == -1)
		res	= errno;

	/* unlock the unit */
	STMT_END( cup, TF_WRITE, NULL,  &cfs);

	return(res);
}

_f_int8
putcf90_8_(char *c, int clen)
{
	_f_int8		res;
	struct fiostate	cfs;		/* fiosp */
	unit		*cup;		/* Unit table pointer   */
	unum_t		unum = 6;

	res	= 0;

	/* lock the unit */
	STMT_BEGIN( unum, 0, T_WSF, NULL,  &cfs, cup);

	if (!cup)
		return((errno=FEIVUNIT));

	if (_fwch(cup, (long *)c, 1, PARTIAL) == -1)
		res	= errno;

	/* unlock the unit */
	STMT_END( cup, TF_WRITE, NULL,  &cfs);

	return(res);
}
