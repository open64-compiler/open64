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


#pragma ident "@(#) libu/util/f2ccpy.c	92.1	07/07/99 13:18:33"

#include <stdarg.h>
#include <fortran.h>
#include <string.h>
#if !defined(_ABSOFT)
#include <malloc.h>
#else
#include <stdlib.h>
#endif

#ifdef	_UNICOS

/*
 *	char *
 *	_f2ccpy(f [, s, slen])
 *	_fcd f;
 *	char *s;
 *	int slen;
 *
 *	Returns:
 *	Pointer to a C string, if successful. NULL otherwise.
 *
 *	**NOTE**	Use of _f2ccpy should be phased out in favor of the more
 * 			portable _fc_acopy and _fc_copy functions which are 
 *			available on all systems starting with the CrayLibs 
 *			1.2 release.  
 *
 *	Copies the Fortran string described by Fortran character
 * 	descriptor "f" to a C string.  If the "s" and "slen"
 *	arguments are present, "s" points to an array of "slen"
 *	characters; otherwise, an array is allocated using malloc().
 *	If the string described by "f" contains a NULL character,
 *	that marks the end of the Fortran string. Trailing blanks
 *	are stripped before the string is copied, and the string
 *	pointed to by "s" is NULL terminated.
 */

char *
_f2ccpy(_fcd f, ...)
{
	va_list	ap;
	int	slen;
	char	*sptr;
	
	if (_numargs() * sizeof(long) == sizeof(_fcd))
		return ( _fc_acopy(f) );
	else {
		/* get 2nd and 3rd arguments */
		va_start(ap, f);
		sptr = va_arg(ap, char *);
		slen = va_arg(ap, int);
		va_end(ap);

		return ( _fc_copy(f, sptr, slen) );
	}
}
#endif

/*
 *	_fc_acopy
 *
 *	Returns:
 *	Pointer to a C string, if successful. NULL otherwise.
 *
 *	Copies the Fortran string described by Fortran character
 * 	descriptor "f" to an allocated, NULL-terminated  C string.  The 
 *	string is allocated using malloc().
 *
 *	If the string described by "f" contains a NULL character,
 *	that marks the end of the Fortran string. Trailing blanks
 *	are stripped before the string is copied.
 */
char *
_fc_acopy(_fcd f)
{
	long	len;
	char	*start, *end;
	char	*sptr;
	
#if	defined(_UNICOS)
#if	defined(_ADDR64)
	if (_numargs() * sizeof(long) != sizeof(_fcd))
		return(NULL);
#else
	if (!_isfcd(f))
		return(NULL);
#endif	/* _ADDR64 */
#endif	/* _UNICOS */

	len = _fcdlen(f);
	start = _fcdtocp(f);
	/* see if string contains a NULL */
	if ((end = memchr(start, '\0', len)) != NULL)
		len = end - start;
	/* remove trailing blanks */
	while ((len > 0) && (start[len-1] == ' '))
		len--;
	/* allocate space */
	if ((sptr = malloc(len+1)) == NULL)
		return(NULL);
	(void) strncpy(sptr, start, len);
	*(sptr + len) = '\0';
	return(sptr);
}

/*
 *	_fc_copy
 *
 *	Arguments
 *		f	The Fortran character descriptor
 *		sptr	Pointer to the C character array.
 *		slen	Length of the C character array.
 *
 *	Returns:
 *	Pointer to sptr if successful. NULL otherwise.
 *
 *	Copies the Fortran string described by Fortran character
 * 	descriptor "f" to sptr and NULL-terminates it.
 *
 *	If the string described by "f" contains a NULL character,
 *	that marks the end of the Fortran string. Trailing blanks
 *	are stripped before the string is copied.
 */
char *
_fc_copy(_fcd f, char *sptr, int slen)
{
	int	len;
	char	*start, *end;
	
#if	defined(_UNICOS)
#if	defined(_ADDR64)
	if (_numargs() * sizeof(long) != (sizeof(_fcd) + sizeof(int) +
	    sizeof(char*)))
		return(NULL);
#else
	if (!_isfcd(f))
		return(NULL);
#endif	/* _ADDR64 */
#endif	/* _UNICOS */

	len = _fcdlen(f);
	start = _fcdtocp(f);
	/* see if string contains a NULL */
	if ((end = memchr(start, '\0', len)) != NULL)
		len = end - start;
	/* remove trailing blanks */
	while ((len > 0) && (start[len-1] == ' '))
		len--;

	if (len >= slen)
		return(NULL);	/* return NULL if too small */

	(void) strncpy(sptr, start, len);
	*(sptr + len) = '\0';
	return(sptr);
}
