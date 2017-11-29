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



#pragma ident "@(#) libf/fio/setpos.c	92.1	06/18/99 18:41:02"

#include <foreign.h>
#include <errno.h>
#include <liberrno.h>
#include "fio.h"

/*
 *      SETPOS
 *
 *		Seek to a position in the file.  If the position passed is
 *      	-1, seek to the end of the file.  The position is a byte offset
 *		within the file, stored in the style of C character pointers:
 *
 *                      bits 0-2        - byte offset within the word.
 *                      bits 3-63       - word offset within the file.
 *
 *      Can be called as:
 *
 *                      CALL SETPOS(IUN, PA)
 *                      CALL SETPOS(IUN, LEN, PA [,ISTAT] )
 */

#define OLD	2	/* number of arguments in old SETPOS version */
#define STAT 	4	/* argument number for optional stat parameter */

void
SETPOS(
	_f_int	*unump,
	_f_int	*len,
	_f_int	*pa,
	_f_int	*stat)
{
	register int	errn;
	register int	narg;
	register unum_t	unum;
	unit		*cup;
	struct fiostate	cfs;

	unum	= *unump;

	STMT_BEGIN(unum, 0, T_SETPOS, NULL, &cfs, cup);	/* lock the unit */
/*
 *	If not connected, do an implicit open.  Abort if the open fails.
 */
	if (cup == NULL)
		cup	= _imp_open(&cfs, SEQ, UNF, unum, 0, NULL);

	if (cup->useq == 0)	/* If direct access file */
		_ferr(&cfs, FEBIONDA, "SETPOS");

/*
 *	Do the setpos.
 */
	narg	= _numargs();

	if ( narg == OLD ) {
		errn	= _setpos(&cfs, cup, len, 1);
	}
	else {
		if (*len <= 0)
			errn	= FEBIOSNT;
		else
			errn	= _setpos(&cfs, cup, pa, *len);
	}

	if (narg >= STAT)
		*stat	= errn;
	else if (errn != OK)
		_ferr(&cfs, errn);

	STMT_END(cup, T_SETPOS, NULL, &cfs);	/* unlock the unit */
}
