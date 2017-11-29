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



#pragma ident "@(#) libf/fio/writec.c	92.3	06/21/99 10:37:55"

/*
 *	write characters
 */

#include <errno.h>
#include <foreign.h>
#include <liberrno.h>
#include "fio.h"

#define STATUS 4	/* argument number for optional status parameter */
 
static void __WRITEC();

/*
 *	write characters, full record mode
 */
void
WRITEC(
	_f_int	*unump,
	_f_int	*uda,
	_f_int	*charsp,
	_f_int	*status)
{
	_f_int	*statp;

	statp	= (_numargs() < STATUS) ? NULL : status;

	__WRITEC(FULL, unump, uda, charsp, statp);

	return;
}

/*
 *	write characters, partial record mode
 */
void
WRITECP(
	_f_int	*unump,
	_f_int	*uda,
	_f_int	*charsp,
	_f_int	*status)
{
	_f_int	*statp;

	statp	= (_numargs() < STATUS) ? NULL : status;

	__WRITEC(PARTIAL, unump, uda, charsp, statp);

	return;
}

/*
 *   Write characters, full or partial record mode
 */
static void
__WRITEC(
	int	fulp,
	_f_int	*unump,
	_f_int	*uda,
	_f_int	*charsp,
	_f_int	*status)
{
	register int	ret;
	register int	errn;
	long		chars;
	unum_t		unum;
	unit		*cup;
	struct fiostate	cfs;

	chars	= *charsp;
	unum	= *unump;
	errn	= 0;

	STMT_BEGIN(unum, 0, T_WSF, NULL, &cfs, cup);
/*
 *	If not connected, do an implicit open.  
 */
	if (cup == NULL) {
		int	ostat;

		cup	= _imp_open(&cfs, SEQ, FMT, unum, (status != NULL),
				&ostat);

		if (cup == NULL) {
			errn	= ostat;
			goto done;
		}
	}

	if (!cup->ok_wr_seq_fmt) {
		errn	= _get_mismatch_error(1, T_WSF, cup, &cfs);
		goto done;
	}

	cup->uwrt	= 1;

	if (cup->uend) {
		/*
		 * If positioned after an endfile, and the file does not
		 * support multiple endfiles, a write is invalid.
		 */
		if (!cup->umultfil && !cup->uspcproc) {
			errn	= FEWRAFEN;
			goto done;
		}
		/*
		 * If a logical endfile record had just been read,
		 * replace it with a physical endfile record before
		 * starting the current data record.
		 */
		if ((cup->uend == LOGICAL_ENDFILE) && !(cup->uspcproc)) {
			if (XRCALL(cup->ufp.fdc, weofrtn)cup->ufp.fdc,
				&cup->uffsw) < 0) {
				errn	= cup->uffsw.sw_error;
				goto done;
			}
		}
		cup->uend	= BEFORE_ENDFILE;
	}

	ret	= _fwch(cup, (long *) uda, chars, fulp);

	if ( ret == IOERR ) {
		errn	= errno;
		goto done;
	}

	if (status != NULL)
		*status	= 0;

done:
	if (errn != 0) {
		if (status == NULL)
			_ferr(&cfs, errn);	
		else
			*status	= errn;
	}

	STMT_END(cup, TF_WRITE, NULL, &cfs);

	return;
}
