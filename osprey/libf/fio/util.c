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



#pragma ident "@(#) libf/fio/util.c	92.2	08/02/99 10:38:48"

#include <ctype.h>
#include <ffio.h>
#include <memory.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "fio.h"
  
/*
 *	_copy_n_trim
 *		copies "alen" characters from "a" into string
 *		"b", which gets null-terminated.  Trailing blanks
 *		are also removed.
 */
void
_copy_n_trim(char *a, ftnlen alen, char *b)
{
	register char	*p;
  
	*(b + alen)	= '\0';
  
	/* Copy alen characters or up to null byte */
  
	(void) strncpy(b, a, alen);	/* Copy a to b */
  
	/* Delete any trailing blanks */
  
	for (p = b + strlen(b) - 1; p >= b && *p == ' '; p--)
		*p	= '\0';
  
	return;
}
  

/* 
 *	_b_char	copies null-terminated string "a" into a blank-padded
 *		buffer "b" of length "blen".
 */
void
_b_char(char *a, char *b, ftnlen blen)
{
	register int	i;
  
	i	= strlen(a);			/* Find end of a */
	i	= MIN(i, blen);			/* Use shorter of the two */
  
	(void) strncpy(b, a, i);		/* Copy a to b */
  
	b	= b + i;
  
	(void) memset(b, (int) ' ', blen - i);	/* Blank rest of b */
  
	return;
}

/* 
 *	_is_file_name	
 *
 *		Determines whether an integer is an 8-byte hollerith file
 *		name.   Returns nonzero if it is and 0 if it is not.
 */

int
_is_file_name(long n)
{
#ifndef	_UNICOS
	return(0);
#else
	register short	i;
	long		rbn;
	char		*cp;
/*
 *	Convert trailing blanks to nulls.
 */
	rbn	= _RBN(&n);
	cp	= (char *)&rbn;

	for (i = (sizeof(long) - 1); i >= 0; i--) {
		if (cp[i] != '\0')
			break;
	}

	if (i < 0)
		return(0);

/*
 *	Check that all characters prior to the trailing nulls are printable 
 *	and therefore are valid file name characters.
 */
	while (i >= 0) {
		if (! isprint(cp[i]))
			return(0);
		i--;
	}

	return(1);			/* Yes!  It's a file name. */
#endif
}

/*
 * Removes trailing blanks from a character string.
 */

static void
g_skip(char *a, int alen)
{
	char	*p;

	for (p = a + alen - 1; p >= a && *p == ' '; p--)
		*p	= '\0';

	return;
}

/* 
 *	_fortname	
 *
 *		Makes a standard fortran file name (currently of the
 *		form "fort.n") in buffer "buf" using unit number "n".
 *		Buffer b must be of sufficient length to receive
 *		the name (MXUNITSZ).
 *
 *		"n" can be hollerith data.  The name generated is the 
 *		hollerith name trimmed of any trailing blanks.  This is
 *		a CRAY extension only.
 *
 *		Returns the number of characters in the name (excluding
 *		the \0) or a negative value if an error occurred.
 */

int
_fortname(
	char	*buf,	/* buffer to receive file name */
	unum_t	n)	/* unit number */
{
#ifdef	_UNICOS
	register short	lim;	/* Maximum size of a hollerith file name */

	lim	= sizeof(long);

	if (_is_file_name((long) n)) {
                buf[lim]	= '\0';
                (void) strncpy(buf, (char *)&n, lim);
		g_skip(buf, strlen(buf));  /* Delete trailing blanks */
		return(strlen(buf));
	}
	else
#endif
		return(sprintf(buf, "fort.%lld", n));
}
