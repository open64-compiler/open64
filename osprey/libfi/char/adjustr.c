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


#pragma ident "@(#) libfi/char/adjustr.c	92.1	07/08/99 10:41:51"
#include <fortran.h>
#include <string.h>

/*
 *	Duplicate names
 *
 *	_ADJUSTR_	- for f90 
 *	_ADJUSTR	- for f90 3.0? and previous on PVP systems 
 */
#ifdef	_CRAY1
#pragma _CRI duplicate _ADJUSTR_ as _ADJUSTR
#endif

void
_ADJUSTR_(
	_fcd	result,
	_fcd	string)
{
	char	*rptr, *sptr;
	int	lenr, lens;
	int	i, j;
	char	*tptr;

/*	Split fcds for source and result into component parts	*/

	lens = _fcdlen(string);
	sptr = _fcdtocp(string);

	lenr = _fcdlen(result);
	rptr = _fcdtocp(result);

/*
 *	Set temporary pointer to end of source string.  Work backwards
 *	until a non-blank character is found.  This will give the number
 *	of characters to copy into result.  Do not dereference tptr if
 *	it is a zero-length string.
 */

	tptr = (char *) sptr;
	tptr += lens - 1;
	for (i = lens; i > 0 && *tptr == ' '; tptr--, i--) ;

/*
 *	Set entire result to ' 's.  Then, set temporary pointer to point
 *	to the first character after the required number of blanks, and
 *	copy source (without trailing blanks) to this spot.
 */

	(void) memset (rptr, ' ', lenr);
	tptr = (char *) rptr;
	tptr += lenr - i;
	(void) strncpy (tptr, sptr, i);

/*	convert result to fcd					*/

	result = _cptofcd (rptr, lenr);

	return;
}
