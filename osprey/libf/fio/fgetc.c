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



#pragma ident "@(#) libf/fio/fgetc.c	92.2	08/20/99 13:51:31"


/*
 * Get a character from a logical unit bypassing fortran formatted
 * I/O.  fgetc is called from f77 and f90 programs on mips only.
 * fgetc only applies to unit 5. The clen arg is ignored since only
 * a single character is read.
 *
 * calling sequence:
 *
 *	INTEGER FGETC, IERROR, IUNIT
 *	IERROR = FGETC (IUNIT, CHAR)
 *
 * where:
 *
 *	char 	- return a character from logical unit
 *	ierror 	.EQ. 0 	- fgetc successful.
 *		.LT. 0	- End-of-file, fgetc unsuccessful.
 *		.GT. 0	- system or F77 error code, fgetc unsuccessful.
 *
 * Call fgetc_ from MIPS F77 or from MIPS F90 without a
 *	compatibility module.
 *
 * Call fgetc_f90 from MIPS F90 without a compatibility module.
 *
 * Call fgetcf90_ from MIPS F90 with a compatibility module.
 * Call fgetcf90_8_ from MIPS F90 with a compatibility module.
 * Call fgetcf90_8_4_ from MIPS F90 with a compatibility module.
 * Call fgetcf90_4_8_ from MIPS F90 with a compatibility module.
 *
 */

#include "fio.h"

#ifdef KEY /* Bug 1683 */
#include <string.h>

static _f_int fgetcf90_(_f_int *u, char *c, int clen);
extern _f_int8 fgetcf90_8_(_f_int8 *u, char *c, int clen);
extern _f_int8 fgetcf90_8_4_(_f_int4 *u, char *c, int clen);
extern _f_int4 fgetcf90_4_8_(_f_int8 *u, char *c, int clen);
static int getc_(char *c, int clen);
static _f_int4 getcf90_(char *c, int clen);
extern _f_int8 getcf90_8_(char *c, int clen);
#else
extern int __fgetc_f90(_f_int *u, char *c, int clen);
extern _f_int fgetcf90_(_f_int *u, char *c, int clen);
extern _f_int8 fgetcf90_8_(_f_int8 *u, char *c, int clen);
extern _f_int8 fgetcf90_8_4_(_f_int4 *u, char *c, int clen);
extern _f_int4 fgetcf90_4_8_(_f_int8 *u, char *c, int clen);
extern int getc_(char *c, int clen);
extern _f_int4 getcf90_(char *c, int clen);
extern _f_int8 getcf90_8_(char *c, int clen);
#endif /* KEY Bug 1683 */

#ifdef KEY /* Bug 1683 */

int 
__fgetc_f90(_f_int *u, char *c, int *status, int clen)
{
	int junk;
	status = (status == 0) ? (&junk) : status;
	return *status = fgetcf90_(u, c, clen);
}

int
pathf90_fget(char *c, int *status, int clen)
{
	int junk;
	status = (status == 0) ? (&junk) : status;
	return *status = getc_(c, clen);
}

#else

int 
__fgetc_f90(_f_int *u, char *c, int clen)
{
	return fgetcf90_(u, c, clen);
}

#endif /* KEY Bug 1683 */

#ifdef KEY /* Bug 1683 */
/* Don't pollute the Fortran namespace with library functions */
static
#endif /* KEY Bug 1683 */
_f_int 
fgetcf90_(_f_int *u, char *c, int clen)
{
	_f_int		res;
	long		buf;
	long		status;
	struct fiostate	cfs;		/* fiosp */
	unit		*cup;		/* Unit table pointer   */
	unum_t		unum;

	unum	= *u;
	res	= 0;

	/* lock the unit */
	STMT_BEGIN( unum, 0, T_RSF, NULL,  &cfs, cup);
#ifdef KEY /* Bug 1683 */
        memset(c, ' ', clen);
	/* Copied from rf90.c; list-directed uses SEQ, so we do too */
	if (cup == NULL) {
	  int stat;
	  int errf = 0;
	  cup = _imp_open(&cfs, SEQ, FMT, unum, errf, &stat);
	  if (0 == cup) {
	    res = errno = stat;
	    goto done;
	  }
	}
#endif /* KEY Bug 1683 */

	if (unum < 0 || !cup) {
		res = (errno=FEIVUNIT);
		goto done;
	}
	
#ifdef KEY /* Bug 9015 */
        /* _frch sometimes gets into an endless loop returning character -1
	 * and non-EOF status. Also, the code following the call to _frch
	 * fails to test for 0 == status (end of record) and fails to
	 * distinguish EOF (status < 0 but _frch doesn't return -1) from
	 * error (_frch returns -1.) Since the code has never worked (SGI
	 * octane exhibits the same problem) just call stdio. But this
	 * works only for formatted I/O - still need a fix for binary or
	 * unformatted. */
	if (cup->ufmt) {
	  FILE *fd = cup->ufp.std;
	  res = fgetc(fd);
	  if (EOF == res) {
	     res = feof(fd) ? -1 : ferror(fd);
	     goto done;
	  }
	  *c = res;
	  res = 0;
	  goto done;
	}
#endif /* KEY Bug 9015 */
	if (_frch(cup, &buf, 1, PARTIAL, &status) == -1)
		res	= errno;

	*c	= (char)buf;

done:
	/* unlock the unit */
	STMT_END( cup, TF_READ, NULL,  &cfs);

	return(res);
}

_f_int8
fgetcf90_8_(_f_int8 *u, char *c, int clen)
{
	_f_int8		res;
	long		status;
	struct fiostate	cfs;		/* fiosp */
	unit		*cup;		/* Unit table pointer   */
	unum_t		unum;

	unum	= *u;
	res	= 0;

	/* lock the unit */
	STMT_BEGIN( unum, 0, T_RSF, NULL,  &cfs, cup);

	if (unum < 0 || !cup)
		return((errno=FEIVUNIT));

	if (_frch(cup, (long *)c, 1, PARTIAL, &status) == -1)
		res	= errno;

	/* unlock the unit */
	STMT_END( cup, TF_READ, NULL,  &cfs);

	return(res);
}

_f_int8
fgetcf90_8_4_(_f_int4 *u, char *c, int clen)
{
	return (_f_int8)fgetcf90_(u, c, clen);
}

_f_int4
fgetcf90_4_8_(_f_int8 *u, char *c, int clen)
{
	_f_int8 uunit;
	uunit = *u;
	return (_f_int4)fgetcf90_8_(&uunit, c, clen);
}

/*
 * getc - get a character from the standard input unit 5 on mips.
 *
 * calling sequence:
 *
 *	INTEGER getc
 *	ierror = getc (char)
 *
 * where:
 *
 *	char	= character read from standard input, usually a terminal
 *	ierror	= 0 if successful.
 *		= -1 if end-of-file read.
 *		> 0 if system error code returned.
 *
 * Call getc_ from MIPS F77 or from MIPS F90 without a
 *	compatibility module.  Note thatit calls fgetc with unit 5.
 *
 * Call getcf90_ from MIPS F90 with a compatibility module.
 * Call getcf90_8_ from MIPS F90 with a compatibility module.
 *
 */

#ifdef KEY /* Bug 1683 */
/* Don't pollute the Fortran namespace with library functions */
static
#endif /* KEY Bug 1683 */
int
getc_(char *c, int clen)
{
	int stdin_unit	= 5;
	return fgetcf90_(&stdin_unit, c, clen);
}

#ifdef KEY /* Bug 1683 */
/* Don't pollute the Fortran namespace with library functions */
static
#endif /* KEY Bug 1683 */
_f_int4
getcf90_(char *c, int clen)
{
	_f_int4		res;
	long		status;
	struct fiostate	cfs;		/* fiosp */
	unit		*cup;		/* Unit table pointer   */
	unum_t		unum = 5;

	res	= 0;

	/* lock the unit */
	STMT_BEGIN( unum, 0, T_RSF, NULL,  &cfs, cup);

	if (!cup)
		return((errno=FEIVUNIT));

	if (_frch(cup, (long *)c, 1, PARTIAL, &status) == -1)
		res	= errno;

	/* unlock the unit */
	STMT_END( cup, TF_READ, NULL,  &cfs);

	return(res);
}

_f_int8
getcf90_8_(char *c, int clen)
{
	_f_int8		res;
	long		status;
	struct fiostate	cfs;		/* fiosp */
	unit		*cup;		/* Unit table pointer   */
	unum_t		unum = 5;

	res	= 0;

	/* lock the unit */
	STMT_BEGIN( unum, 0, T_RSF, NULL,  &cfs, cup);

	if (!cup)
		return((errno=FEIVUNIT));

	if (_frch(cup, (long *)c, 1, PARTIAL, &status) == -1)
		res	= errno;

	/* unlock the unit */
	STMT_END( cup, TF_READ, NULL,  &cfs);

	return(res);
}
