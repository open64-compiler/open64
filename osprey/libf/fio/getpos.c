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



#pragma ident "@(#) libf/fio/getpos.c	92.1	06/21/99 10:37:21"

#include <errno.h>
#include <foreign.h>
#include <liberrno.h>
#include "fio.h"
#include "fstats.h"
 
#define ERET(val) {			\
	errn	= val;			\
	ret	= -1;			\
	goto getpos_done;		\
}

/*
 *	GETPOS 
 *
 *		Get the current postion of the file.  The position is the
 *		word offset within the file.
 *
 *			bits 0-2	- byte offset within the word.
 *			bits 3-63	- word offset within the file.
 *
 *	Can be called as:
 *
 *			IPOS = GETPOS(IUN)
 *			IPOS = GETPOS(IUN, PA)
 *			IPOS = GETPOS(IUN, LEN, PA [,ISTAT] )
 *
 *			CALL   GETPOS(IUN, PA)
 *			CALL   GETPOS(IUN, LEN, PA [,ISTAT] )
 *
 * 	Duplicate entry points
 *
 *      	_GETPOS_ - if user declares it INTRINSIC with CF90 
 *      	GETPOS   - if user calls it with > 1 argument
 *      	$GETPOS  - if user declares it INTRINSIC on with CF77
 *      	_GETPOS  - if user declares it INTRINSIC with CF77 6.0.0.3 or
 *			   previous on the T3D (obsolete)
 */
#pragma _CRI duplicate _GETPOS_ as GETPOS
#pragma _CRI duplicate _GETPOS_ as $GETPOS
#ifdef _CRAYMPP
#pragma _CRI duplicate _GETPOS_ as _GETPOS
#endif
long
_GETPOS_(
	_f_int 	*unump,
	_f_int	*arg2,
	_f_int	*arg3,
	_f_int	*arg4)
{
	register int	errn;
	register int	narg;
	long		fp_arg[32];
	long		len;
	long		pos;
	long		ret;		/* return value */
	_f_int		*pa;
	_f_int		*stat = NULL;
	register unum_t	unum;
	unit 		*cup;
	struct fiostate	cfs;

	unum	= *unump;

	STMT_BEGIN(unum, 0, T_GETPOS, NULL, &cfs, cup);  /* lock the unit */
/*
 *	If not connected, do an implicit open.  Abort if the open fails.
 */
	if (cup == NULL)
		cup	= _imp_open(&cfs, SEQ, UNF, unum, 0, NULL);

	if (cup->useq == 0)	/* If direct access file */
		_ferr(&cfs, FEBIONDA, "GETPOS");

	narg	= _numargs();

	switch (narg) {

	case 1:			/* IPOS = GETPOS(IUN) */
		len	= 0;
		pa	= NULL;
		break;

	case 2:			/* CALL GETPOS(IUN, PA) */
		len	= 1;
		pa	= arg2;
		break;

	case 3:			/* CALL GETPOS(IUN, LEN, PA) */
		len	= *arg2;
		pa	= arg3;
		break;

	case 4:			/* CALL GETPOS(IUN, LEN, PA, ISTAT) */
		len	= *arg2;
		pa	= arg3;
		stat	= arg4;
		break;

	default:
		_ferr(&cfs, FEARGCNT, "GETPOS", narg, "1, 2, 3 or 4");
	}

/*
 *	According to the file structure make the appropriate call to get
 * 	the current file position.  Postion routines are file structure
 *	dependent.
 */
	errn	= 0;

	if (narg != 1 && len <= 0) {
		ERET(FEBIOSNT);
	}

	switch ( cup->ufs ) {

	case  FS_TEXT:
	case  STD:
		pos	= ftell(cup->ufp.std);
/*
 *		If unformatted file, then positioning is done
 *		on a word boundry.  Therefore we need to calculate a word
 *		offset before returning to the caller.  For formatted files
 *		positioning must be done on a byte boundry.
 */
		if ( cup->ufmt == NO )	/* if unformatted */
			pos	= pos >> 3;

		if ( pa != NULL )
			*pa	= pos;
		ret	= pos;
		break;

	case FS_FDC:
/*
 *		If this is async I/O, wait for any outstanding requests.
 */
		WAITIO(cup, ERET(cup->uffsw.sw_error));


/*
 *		If we can seek, use the seek interface
 */
		if ((cup->uflagword & FFC_SEEKA) != 0)
			{
			fp_arg[0]	= 0;
			fp_arg[1]	= 1;
			ret	= XRCALL(cup->ufp.fdc, posrtn)
				cup->ufp.fdc, FP_BSEEK, fp_arg, 2, &cup->uffsw);
			if (ret < 0)
				ERET(cup->uffsw.sw_error);
/*
 *			If unformatted file, then positioning is done
 *			on a word boundry.  Therefore we need to calculate
 *			a word offset before returning to the caller.  For
 *			formatted files positioning must be done on a byte
 *			boundry.
 */
			if ( cup->ufmt == NO )
				ret	= _dshiftr(ret, ret, 6); /* to words (unfmtd) */
			else
				ret	= _dshiftr(ret, ret, 3); /* to bytes (fmtd) */
			if ( pa != NULL && len > 0)
				pa[0]	= ret;
			}
		else /* can't seek */
			{
			ret	= XRCALL(cup->ufp.fdc, posrtn)
				cup->ufp.fdc, FP_GETPOS, pa, len, &cup->uffsw);
			if (ret < 0)
				ERET(cup->uffsw.sw_error);
			}

		break;

	case FS_AUX:
		_ferr(&cfs, FEMIXAUX);
		break;
	default:
		_ferr(&cfs, FEINTFST);

	}

getpos_done: 
	if (stat != NULL)
		*stat	= errn;
	else if (errn != 0)
		_ferr(&cfs, errn);

	STMT_END(cup, T_GETPOS, NULL, &cfs);

	return(ret);
}
