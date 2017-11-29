/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

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


#pragma ident "@(#) libfi/char/adjustl.c	92.1	07/08/99 10:41:51"
#include <fortran.h>
#include <string.h>

/*
 *	Duplicate names
 *
 *	_ADJUSTL_	- for f90 
 *	_ADJUSTL	- for f90 3.0? and previous on PVP systems 
 */
#ifdef	_CRAY1
#pragma _CRI duplicate _ADJUSTL_ as _ADJUSTL
#endif

void
_ADJUSTL_(
	_fcd	result,
	_fcd	string)
{
	int	lens, lenr;
	int	i;
	char	*sptr, *rptr;
	char	*tptr;

/*	Split fcd of source and result into component parts		*/

	lens = _fcdlen (string);
	sptr = _fcdtocp (string);

	lenr = _fcdlen (result);
	rptr = _fcdtocp (result);

/*
 *	Determine number of bytes without leading 0's.  While doing
 *	it, update temporary pointer to point to first non-blank
 *	character.  Do not dereference tptr if zero length string.
 */

	tptr = sptr;
	for (i = lens ; i > 0 && *tptr == ' '; tptr++, i--) ;

/*
 *	Set result string to all ' ', and copy source (without leading
 *	blanks.
 */

	(void) memset (rptr, (int) ' ', lenr);
#ifdef KEY /* Bug 9723 */
	(void) memmove(rptr, tptr, i);
#else /* KEY Bug 9723 */
	(void) strncpy (rptr, tptr, i);
#endif /* KEY Bug 9723 */

/*	convert result to fcd					*/

	result = _cptofcd (rptr, lenr);

	return;
}
