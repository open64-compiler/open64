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


#pragma ident "@(#) libfi/char/f90_fcd_cmp_eq.c	92.2	01/20/95 09:56:38"
#include <string.h>

#define	BLANK	((int) ' ')

int _F90_FCD_CMP_EQ( char *str1,
		     char *str2,
		     int  len1, 
		     int  len2 )
{
 	char		*str3;
	unsigned int	len3, len;
	int		stat;


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

