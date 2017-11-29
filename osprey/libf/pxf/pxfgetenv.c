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


#pragma ident "@(#) libf/pxf/pxfgetenv.c	92.1	06/29/99 11:36:06"

/*
 *  PXFGETENV  -- return value for environment name
 *
 *  Call from Fortran:
 *
 *              CHARACTER*n NAME, VALUE
 *              INTEGER LENNAME, LENVAL, IERROR
 *		CALL PXFGETENV( NAME, LENNAME, VALUE, LENVAL, IERROR )
 *
 *		where
 *
 *		NAME	is an input character variable or array element
 *			containing the name of an environment variable.
 *			GETENV searches for NAME in the environment.
 *
 *		LENNAME	is an input integer variable containing the length
 *			of NAME in characters.  If LENNAME is zero, strip
 *			trailing blanks.  Change the length of the input
 *			NAME to remove the number of blanks stripped.  If
 *			LENNAME is zero and NAME is all blanks, this is a
 *			NULL string.
 *
 *		VALUE	is an output character variable or array element 
 *			containing the value of the environment variable
 *			NAME.
 *
 *		LENVAL is an output integer variable containing the length
 *			of VALUE in characters.  If NAME is found but has
 *			no value, LENVAL is zero and VALUE contains all
 *			blanks to indicate a NULL string.
 *
 *		IERROR 	is output integer variable containing the status:
 *			Zero is returned if GETENV is successful, i.e.,
 *			NAME is found.
 *			EINVAL is returned if NAME is not in the
 *			environment list.
 *			ETRUNC is returned if the declared length of VALUE
 *			is insufficient to contain the string to be
 *			returned, the value of NAME shall be truncated to
 *			fit in VALUE, and LENVAL will contain the original
 *			length of the value of NAME before truncation.
 *
 */
#if !defined(_ABSOFT)
#include <malloc.h>
#endif
#include <fortran.h>
#include <errno.h>
#include <liberrno.h>
#include <stdlib.h>
#include <string.h>

extern char *_fc_acopy(_fcd f);

#ifndef _UNICOS
#include <stddef.h>

#if defined(BUILD_OS_DARWIN)
extern char **environ;
#else /* defined(BUILD_OS_DARWIN) */
extern char **_environ;
#endif /* defined(BUILD_OS_DARWIN) */

char *
_GETENV(char *name)
{
	char *s1, *s2;
#if defined(BUILD_OS_DARWIN)
	char **p = environ;
#else /* defined(BUILD_OS_DARWIN) */
	char **p = _environ;
#endif /* defined(BUILD_OS_DARWIN) */
	char *ret = NULL;
	if (p == NULL)
		return(ret);
	while (*p != NULL) {
		s1 = name;
		s2 = *p++;
		while (*s1 == *s2++)
			if (*s1++ == '=')
				return(s2);
		if (*s1 == '\0' && *(s2-1) == '=')
			return(s2);
	}
	return(ret);
}
#endif

#ifdef _UNICOS
void
PXFGETENV(
#else
void
_PXFGETENV(
#endif
	_fcd	name,
	_f_int	*lenname,
	_fcd	value,
	_f_int	*lenval,
	_f_int	*ierror)
{
	char *buf, *cp, *v;
	int i, lensrc, lenin, lentarg, lenv;

	lenin = *lenname;
	lensrc = _fcdlen(name);
	lentarg = _fcdlen(value);
	*ierror = 0;

	/* check if the length of the lenname input argument is valid. */
	if (lenin < 0 || lenin > lensrc) {
		*ierror = EINVAL;
		*lenval = 0;
		return;
	}

	if (lensrc != 0) {

		/* Copy input name.  If lenname = 0, the trailing blanks
		 * must be stripped and the string may be null when the
		 * trailing blanks are stripped.
		 */

		if ((buf = _fc_acopy(name)) == NULL) {

			*ierror = ENOMEM;
			*lenval = 0;
			return;
		}

		/* check for all blank input string */
		if (strlen(buf) == 0) {
			*lenval = 0;
			cp = _fcdtocp(value);
			(void) memset (cp, (int) ' ', lentarg);
			return;
	}

		/* get value of environment variable name */
#ifdef _UNICOS
		if ((v = getenv (buf)) == NULL) {
#else
		if ((v = _GETENV(buf)) == NULL) {
#endif

			/* name not found, return without
			 * setting other values.
			 */
			*ierror = EINVAL;
			*lenval = 0;
			return;
		}

		free(buf);

		lenv = strlen(v);
		*lenval = lenv;
		/*
		 * return ETRUNC when string length greater than size of VALUE
		 * but copy the string up to the size of VALUE
		 */
		if(lenv > lentarg)
			*ierror = ETRUNC;

		/* destination is a character variable */
		cp = _fcdtocp(value);
		for (i = 0; i < lentarg && *v != '\0'; i++){
				*cp++=*v++;
		}
	} else {
		/* zero-length FCD NAME, return null pointer */
		*lenval = 0;
		i=0;
		cp = _fcdtocp(value);
	}

	/* blank fill if necessary */
	for (;i<lentarg;i++){
		*cp++=' ';
	}
	return;
}

#ifndef _UNICOS

void
pxfgetenv_(
	char *NAME,
        _f_int *LENNAME,
        char *VALUE,
        _f_int *LENVAL,
        _f_int *IERROR,
        _f_int namelen,
        _f_int valuelen)
{
	_PXFGETENV( _cptofcd(NAME, namelen), LENNAME,
		    _cptofcd(VALUE, valuelen), LENVAL, IERROR);
	return;
}

#endif
