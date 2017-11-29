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



#pragma ident "@(#) libf/fio/fseek.c	92.1	06/18/99 19:52:04"

/*
 *	 Position a file associated with a fortran logical unit with
 *	 MIPS F77 and MIPS f90.
 *
 * CALLING SEQUENCE:
 *
 *	INTEGER ierror, ioff, ifrom, lunit
 *	ierror = fseek(lunit, ioff, ifrom)
 *
 *	INTEGER ierror, ioff, ifrom, lunit
 *	INTEGER(KIND=8) lioff
 *	ierror = fseek64(lunit, lioff, ifrom)
 *
 * WHERE:
 *
 *	lunit	= an open logical unit
 *
 *	ioff 	= an offset in bytes relative to the position
 *		  specified by ifrom
 *
 *	lioff 	= an offset in bytes relative to the position
 *		  specified by ifrom
 *
 *	ifrom	= - 0 means 'beginning of the file'
 *		= - 1 means 'the current position'
 *		= - 2 means 'the end of the file'
 *
 *	ierror	= 0 if successful.
 *	        .NE. 0 a system error code if unsuccessful.
 *
 * NOTES: fseek64() only exists in the 64 bit library
 *
 *	 __fseek_f90 is called for mips f77 or if there is no
 *	 compatibility mode from mips f90.
 *
 *	 fseekf90_ is called for mips f90 if there is a
 *	 compatibility mode.
 *
 *	 __fseek64_f90 is called for mips f77 or if there is no
 *	 compatibility mode from mips f90.
 *
 *	 fseekf90_8_ is called for mips f90 if there is a
 *	 compatibility mode.
 *
 *	 fseekf90_4_8_4_ is called for mips f90 if there is a
 *	 compatibility mode.
 *
 */

#include "fio.h"

extern int __fseek_f90(int *u, int *off, int *from);
extern _f_int fseekf90_(_f_int *u, _f_int *off, _f_int *from);
extern _f_int __fseek64_f90(_f_int *u, _f_int8 *off, int *from);
extern _f_int8 fseekf90_8_(_f_int8 *u, _f_int8 *off, int8 *from);
extern _f_int fseekf90_4_8_4_(_f_int *u, _f_int8 *off, int *from);

static _f_int __setpos64( unit *cup, _f_int8 ioff,  _f_int whence);

int
__fseek_f90(int *u, int *off, int *from)
{
	return fseekf90_(u, off, from);
}

_f_int
fseekf90_(_f_int *u, _f_int *off, _f_int *from)
{
	unit            *cup;		/* Unit table pointer   */
	struct fiostate cfs;
	_f_int8		ioff;
	unum_t		unum;
	_f_int		rtrn;

	unum	= *u;
	ioff	= *off;
/*	lock the unit */
	STMT_BEGIN( unum, 0, T_SETPOS, NULL, &cfs, cup);

/*	If not connected, do an implicit open.  Abort if open fails. */
	if (cup == NULL)
		cup	= _imp_open(&cfs, SEQ, UNF, unum, 0, NULL);

/*	if direct access file, error. */
	if (cup->useq == 0)
		_ferr(&cfs, FEBIONDA, "SETPOS");

/*	Do the setpos. */
	if (__setpos64(cup, ioff, *from) != 0)
		rtrn	= errno;
	else 
		rtrn	= 0;

/*	unlock the unit. */
	STMT_END(cup, T_SETPOS, NULL, &cfs);
	return rtrn;
}

_f_int
__fseek64_f90(_f_int *u, _f_int8 *off, int *from)
{
	return fseekf90_4_8_4_(u, off, from);
}

_f_int
fseekf90_4_8_4_(_f_int *u, _f_int8 *off, int *from)
{
	unit            *cup;		/* Unit table pointer   */
	struct fiostate cfs;
	_f_int8		ioff;
	unum_t		unum;
	_f_int		rtrn;

	unum	= *u;
	ioff	= *off;

/*	lock the unit. */
	STMT_BEGIN( unum, 0, T_SETPOS, NULL, &cfs, cup);

/*	If not connected, do an implicit open.  Abort if open fails. */
	if (cup == NULL)
		cup = _imp_open(&cfs, SEQ, UNF, unum, 0, NULL);

	if (cup->useq == 0)	/* If direct access file */
		_ferr(&cfs, FEBIONDA, "SETPOS");

/*
 *	Do the setpos.
 */
	if (__setpos64(cup, ioff, *from) != 0)
		rtrn=errno;
	else 
		rtrn = 0;
	STMT_END(cup, T_SETPOS, NULL, &cfs);    /* unlock the unit */
	return rtrn;
}

_f_int8
fseekf90_8_(_f_int8 *u, _f_int8 *off, int8 *from)
{
	unit            *cup;		/* Unit table pointer   */
	struct fiostate cfs;
	_f_int8		ioff;
	unum_t		unum;
	_f_int		rtrn;
	_f_int		whenc;

	unum	= *u;
	ioff	= *off;
	whenc	= *from;

/*	lock the unit. */
	STMT_BEGIN( unum, 0, T_SETPOS, NULL, &cfs, cup);

/*	If not connected, do an implicit open.  Abort if open fails. */
	if (cup == NULL)
		cup = _imp_open(&cfs, SEQ, UNF, unum, 0, NULL);

	if (cup->useq == 0)	/* If direct access file */
		_ferr(&cfs, FEBIONDA, "SETPOS");

/*
 *	Do the setpos.
 */
	if (__setpos64(cup, ioff, *from) != 0)
		rtrn=errno;
	else 
		rtrn = 0;
	STMT_END(cup, T_SETPOS, NULL, &cfs);    /* unlock the unit */
	return rtrn;
}

/* 
 * __setpos64 is a 64-bit version of _setpos where user specifies a
 * "whence" parameter like the unix version of fseek().  Note: this
 * is NOT a general replacement for _setpos for MIPS.
 */

_f_int
__setpos64(
	unit	*cup,		/* unit pointer */
	_f_int8	ioff,		/* offset */
	_f_int	whence)		/* 0 - from beginning of file
				 * 1 - from the current position
				 * 2 - from the end of the file
				 */
{
	_f_int		ret;
	_f_int		retstat;
	struct fdinfo *fio;
	int		fp_parm[2];

	retstat	= 0;

/*      Wait for completion of a preceding asynchronous BUFFER IN/OUT. */
	WAITIO(cup, return(cup->uffsw.sw_error));

	cup->urecpos	= 0;
	if (cup->uwrt) {
		if (cup->utrunc) {
			ret = _unit_trunc(cup);
			if (ret != 0)
				return(ret);
		}
		cup->uwrt = 0;
	}
/*
 *	Make appropriate call to set file position according to the file
 *	structure.  Positioning routines are file-structure dependent.
 */
	switch( cup->ufs ) {

	case  FS_TEXT:
	case  STD:
/*
 *		If unformatted file, then positioning is done to a word
 *		boundary.  Converto to bytes
 */
		if ( cup->ufmt == NO )
			ioff <<= 3;

#ifdef KEY /* Bug 1678 */
		ret	= fseeko(cup->ufp.std, ioff, whence);
#else /* KEY Bug 1678 */
		ret	= fseek(cup->ufp.std, ioff, whence);
#endif /* KEY Bug 1678 */
		if (ret != 0)
			return(errno);

		break;

	case  FS_FDC:
		fio	= cup->ufp.fdc;

		if ((cup->uflagword & FFC_SEEKA) ||
		   (whence==2 && (cup->uflagword & FFC_SEEKE))){

			if (whence==2) {

				ret = XRCALL(fio, seekrtn) fio, ioff, 2,
				   &cup->uffsw);
				if (ret < 0)
					return(cup->uffsw.sw_error);

				/* bit position in file */
				ret <<= 3;
			} else {
        /* A couple of problems will need to be solved before we can */
        /* use the ffio posrtn on MIPS. That routine is currently */
        /* documented as expecting longs in the 3rd parameter for */
        /* FP_BSEEK . But more importantly, FP_BSEEK isn't supported */
        /* yet for MIPS, because  longs are not sufficiently large */
        /* to contain all possible file positions. */
				return(FDC_ERR_NOSUP);
			}
		}
		else {
/*
 *		Assume that positioning is done via a 'magic cookie'.
 *		Just pass it through.
 */
			ret = XRCALL(fio, posrtn)
			   fio, FP_SETPOS, &ioff, 1, &cup->uffsw);
		}

		if (ret < 0)
			return(cup->uffsw.sw_error);
		ret = 0;
		break;

	case FS_AUX:
		return(FEMIXAUX);
	default:
		return(FEINTFST);
	}

/*	Set the end flag if going to EOD, else clear it. */
	if ((ioff==0 && whence==2) ) {
		if (cup->ufs != FS_FDC)
			cup->uend = LOGICAL_ENDFILE;
		else {
			/*
			 * This is after terminal endfile record of the
			 * file, but we must decide if it's a logical or
			 * physical endfile record.
			 */
			if ((cup->uflagword & FFC_WEOF) == 0)
				cup->uend = LOGICAL_ENDFILE;
			else {
				/* byte offset within file */
				int fbytepos;

				switch (fio->class) {

				case CLASS_COS:
					/*
					 * Since seekrtn for a cos layer
					 * when called with 0,2 does NOT
					 * return a resulting byte offset,
					 * query the position with this
					 * seekrtn call.  The only case
					 * where 0 might be returned is
					 * when the file is empty.
					 */
					fbytepos = XRCALL(fio, seekrtn) fio, 0,
					   1, &cup->uffsw);
					if (fbytepos > 0)
						cup->uend = PHYSICAL_ENDFILE;
					else
						cup->uend = LOGICAL_ENDFILE;
					break;
				default:
					return(FEBIOSNT);
				}
			}
		}
	} else
		/*
		 * Sadly, we may be positioned after an endfile record,
		 * and not know it.   This should be fixed.
		 */
		cup->uend = BEFORE_ENDFILE;
	return(OK);
}
