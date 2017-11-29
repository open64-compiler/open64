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


#pragma ident "@(#) libu/util/getarg.c	92.1	07/07/99 13:18:33"

#include <fortran.h>
#include <string.h>

/*
 * INTEGER FUNCTION GETARG(k, c [,ls])
 *
 * returns the kth unix command argument in fortran character
 * variable argument c.
 * This version of GETARG differs from the baseline source as follows:
 *    -the ls count is in words and is used only when the destination
 *     is not a character variable.
 *    -GETARG returns the number of characters in the field.
 *    -the format of FORTRAN character pointers is different.
 *    -the tail of the destination is padded with spaces or zeros.
 *    -the name of xargc and xargv are different in UNICOS.
 */

extern int _argc;	/* the number of argv fields */
extern char **_argv;	/* pointer to pointer to fields */

#ifndef	_ADDR64			/* not available if _isfcd is not available */

_f_int
GETARG(n, arg, ls)
long	*n;	/* argument number wanted */
_fcd	arg;	/* destination address */
long	*ls;	/* if destination is not a FORTRAN character, this is
                 * the number of 64 bit words in destination field */
{
	char	*src;
	char	*dest;
	char	pad;
	int	count;
	int	len;

	if (_numargs() < 2)
		return( (_f_int) -1);

	/* Determine destination length and address */

	if (_isfcd(arg)) {	/* If destination is a character variable */
		dest	= _fcdtocp(arg);
		count	= _fcdlen(arg);
		pad	= ' ';
	}
	else {			/* Destination is a hollerith variable */
		if (_numargs() < 3)
			return( (_f_int) -1);

		dest	= *(char **) &arg;
		count	= *ls * sizeof(long);
		pad	= '\0';
	}

	/* Pick up address of field needed */

	if (*n >= 0 && *n < _argc)
		src	= _argv[*n];
	else
		src	= "";		/* Null string */

	len	= strlen(src);

	/* Move string until destination full or NULL found */

	if (len < count) {
		(void) memcpy(dest, src, len);
		(void) memset(dest + len, pad, count - len);
	}
	else {
		(void) memcpy(dest, src, count);
		len	= count;
	}

	return( (_f_int) len);
}
#endif	/* _ADDR64 */

/*
 * INTEGER FUNCTION IARGC()
 *
 * returns the number of command line arguments
 */

_f_int
IARGC()
{
	return(_argc - 1);
}
