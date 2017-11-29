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



#pragma ident "@(#) libf/fort/fcdstr.c	92.1	06/24/99 10:18:36"
#include <fortran.h>
#include <memory.h>
#include <string.h>

#define	BLANK	((int) ' ')

/*
 *	fcdstr		Fortran character string manipulation routines.
 *
 *	These routines are intended for use by a Fortran compiler to do
 *	low-level string comparisions and manipulations.
 *
 *	As of January 1994, the MPP Fortran 77 compiler is known to call
 *	these routines in generated code   It is believed that the Solaris 
 *	and MPP CF90 compilers also generate calls to these routines in 
 *	generated code.
 */

/*
 *	_fcd_cmp_eq	Compare two Fortran strings for equality, using
 *			the Fortran rules for comparisons.
 *
 *	Returns	0	If the first string is not equal to the second
 *		1	If the first string is equal to the second
 */
int
_fcd_cmp_eq(
	_fcd	fch1,	/* First character string	*/
	_fcd	fch2)	/* Second character string	*/
{
	char		*str1, *str2, *str3;
	unsigned int	len1, len2, len3, len;
	int		stat;

	str1	= _fcdtocp(fch1);
	str2	= _fcdtocp(fch2);
	len1	= _fcdlen (fch1);
	len2	= _fcdlen (fch2);

	/* Find out if strings are of equal length */

	if (len1 < len2) {
		len	= len1;
		len3	= len2 - len1;
		str3	= str2 + len;
	}
	else {
		len	= len2;
		len3	= len1 - len2;
		str3	= str1 + len;
	}

	/* Compare first part of strings */

	stat	= memcmp(str1, str2, len);

	/*
	 * If the string lengths are unequal and equality has not yet
	 * been resolved, then compare remnant against blanks.
	 */

	while (len3 > 0 && stat == 0) {	/* If further comparision necessary */
		register char	ch;

		ch	= *str3;
		str3	= str3 + 1;
		stat	= ((int) ch) ^ BLANK;
		len3	= len3 - 1;
	}

	return (stat == 0);
}

/*
 *	_fcd_cmp_gt	Compare two Fortran strings for greater-than, using
 *			the Fortran rules for comparisons.
 *
 *	Returns	0	If the first string is not greater than the second
 *		1	If the first string is greater than the second
 */
int
_fcd_cmp_gt(
	_fcd	fch1,	/* First character string	*/
	_fcd	fch2)	/* Second character string	*/
{
	char		*str1, *str2, *str3;
	unsigned int	len1, len2, len3, len;
	int		sign, stat;

	str1	= _fcdtocp(fch1);
	str2	= _fcdtocp(fch2);
	len1	= _fcdlen (fch1);
	len2	= _fcdlen (fch2);

	/* Find out if strings are of equal length */

	sign	= (len1 < len2);

	if (sign) {
		len	= len1;
		len3	= len2 - len1;
		str3	= str2 + len;
	}
	else {
		len	= len2;
		len3	= len1 - len2;
		str3	= str1 + len;
	}

	/* Compare first part of strings */

	stat	= memcmp(str1, str2, len);

	/*
	 * If the string lengths are unequal and equality has not yet
	 * been resolved, then compare remnant against blanks.
	 */

	while (len3 > 0 && stat == 0) {	/* If further comparision necessary */
		register char	ch;

		ch	= *str3;
		str3	= str3 + 1;
		stat	= sign ? (BLANK - (int) ch) : (((int) ch) - BLANK);
		len3	= len3 - 1;
	}

	return (stat > 0);
}

/*
 *	_fcd_cmp_lt	Compare two Fortran strings for less-than, using
 *			the Fortran rules for comparisons.
 *
 *	Returns	0	If the first string is not less than the second
 *		1	If the first string is less than the second
 */
int
_fcd_cmp_lt(
	_fcd	fch1,	/* First character string	*/
	_fcd	fch2)	/* Second character string	*/
{
	char		*str1, *str2, *str3;
	unsigned int	len1, len2, len3, len;
	int		sign, stat;

	str1	= _fcdtocp(fch1);
	str2	= _fcdtocp(fch2);
	len1	= _fcdlen (fch1);
	len2	= _fcdlen (fch2);

	/* Find out if strings are of equal length */

	sign	= (len1 < len2);

	if (sign) {
		len	= len1;
		len3	= len2 - len1;
		str3	= str2 + len;
	}
	else {
		len	= len2;
		len3	= len1 - len2;
		str3	= str1 + len;
	}

	/* Compare first part of strings */

	stat	= memcmp(str1, str2, len);

	/*
	 * If the string lengths are unequal and equality has not yet
	 * been resolved, then compare remnant against blanks.
	 */

	while (len3 > 0 && stat == 0) {	/* If further comparision necessary */
		register char	ch;

		ch	= *str3;
		str3	= str3 + 1;
		stat	= sign ? BLANK - (int) ch : ((int) ch) - BLANK;
		len3	= len3 - 1;
	}

	return (stat < 0);
}

/*
 *	_fcd_blank	Blanks a Fortran string
 */
void
_fcd_blank(
	_fcd	fch)	/* Character string to be blanked */
{
	char		*str;
	unsigned int	len;

	str	= _fcdtocp(fch);
	len	= _fcdlen (fch);

	if (len > 0)
		(void) memset (str, BLANK, len);

	return;
}

/*
 *	_fcd_copy	Copy a source Fortran string to a destination
 *			Fortran string and return the remainder (which
 *			may be of zero length).
 */
_fcd
_fcd_copy(
	_fcd	fch1,	/* Destination character descriptor		*/
	_fcd	fch2)	/* Source character descriptor			*/
{
	char		*str1, *str2;
	unsigned int	len1, len2, len;

	str1	= _fcdtocp(fch1);
	str2	= _fcdtocp(fch2);
	len1	= _fcdlen (fch1);
	len2	= _fcdlen (fch2);

	len	= (len1 > len2) ? len2 : len1;	/* MIN(len1, len2) */

	if (len > 0)
		(void) memcpy(str1, str2, len);

	return ( _cptofcd(str1 + len, len1 - len) );
}
