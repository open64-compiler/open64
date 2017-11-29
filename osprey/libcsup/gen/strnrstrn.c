#pragma ident "@(#)92/gen/strnrstrn.c	92.1	06/02/99 16:43:34"

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


#include <string.h>

#define	MAXCHRS	256	/* Size of character set */

/*
 *	strnrstrn	Search for the last occurance of a substring
 *			in another string.  The length of the strings
 *			are arguments and the strings are not necessarily
 *			null-terminated.
 *
 *	Algorithm:  This routine uses the Boyer-Moore algorithm as
 *		described in _Algorithms_ by Robert Sedgewick,
 *		Addison-Wesley, 2d Ed., 1988.  pp. 277-289.
 *
 *	Performance:  If M is the length of the substring and N is
 *		the length of the string; this algorithm's worst case
 *		is M+N character comparisons, and ``average-case'' is
 *		N/M comparisons.  The ``brute-force'' algorithm's
 *		worst case is about N*M comparisons.
 */

char *
strnrstrn(const char *string, size_t lenstr, const char *substr, size_t lensub)
{
	register short	i;
	register unsigned short	j;
	unsigned short	skip[MAXCHRS];

	if (lensub == 0)
		return( (char *) string + lenstr);

	/* Initialize skip array */

	for (i = 0; i < MAXCHRS; i++)	/* Should vectorize */
		skip[i]	= lensub;

	for (i = lensub - 1; i >= 0; i--)
		skip[(int)substr[i]]	= i;

	i	= lenstr - lensub;
	j	= 0;

	do {

		if (string[i] == substr[j]) {
			i	= i + 1;
			j	= j + 1;
		}
		else {
			if (j > skip[(int)string[i]])
				i	= i - (j + 1);
			else
				i	= i - skip[(int)string[i]];

			j	= 0;
		}

	} while (j < (unsigned) lensub && i >= 0);

	if (i < 0)
		return(NULL);
	else
		return( (char *) string + i - lensub);
}
