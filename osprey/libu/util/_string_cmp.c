/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
 */

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


#pragma ident "@(#) libu/util/_string_cmp.c	92.1	07/07/99 13:18:33"

#include <string.h>
#include <ctype.h>
#include "utildefs.h"

/*
 * Compare a C string (s1) to a Fortran string (s2), treating lower case
 * letters in the Fortran string as upper case letters and ignoring any
 * trailing blanks in the Fortran string.  Returns nonzero (true) if the
 * strings are equal.  Parameter n is length of Fortran string.
 */
int
_string_cmp(const char *s1, const char *s2, int n)
{
	int     i;
 
	while (*(s2 + n-1) == ' ')
		n--;	    	/* back over trailing blanks */
	if (strlen(s1) != n)
		return(0);      /* different length strings aren't equal */
	for(i = 0; i < n; i++)
		if(*s1++ != toupper(*s2++))
			return(0);      /* mismatch */
	return(*s1 == '\0');    /* strings match if no more characters */
}
