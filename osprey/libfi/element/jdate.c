/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


#pragma ident "@(#) libfi/element/jdate.c	92.1	06/16/99 15:47:23"
#include <fortran.h> 
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <sys/types.h>

#ifdef KEY /* Bug 8290 */
#  include "element.h"
#endif /* KEY Bug 8290 */

/*
 *	JDATE	Returns the current date in "YYDDD   " format.
 *		YY = Year modulus 100 (00 - 99).
 *		DDD = Julian day (001 - 366).
 *
 *		May be called as either a function or a subroutine.
 *		If called as a subroutine, the parameter may be
 *		either CHARACTER or INTEGER type.
 */

/*
 *	Duplicate names
 *
 *	_JDATE_		- for f90 intrinsic
 *	JDATE		- if called as a subroutine
 *	$JDATE		- for cf77 intrinsic
 *	_JDATE		- for f90 3.0? and previous on PVP systems 
 */
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
#pragma _CRI duplicate _JDATE_ as JDATE
#pragma _CRI duplicate _JDATE_ as $JDATE
#pragma _CRI duplicate _JDATE_ as _JDATE
#endif

/*
 *	NOTE:	For 32-bit architectures, the JDATE function is quite
 *		different from the 64-bit version.   Two parms vs. one,
 *		pointer to a parameter vs. not, and differnt return values.
 */

#ifdef _UNICOS

_f_int
_JDATE_(julianday)
_fcd	julianday;
{
 
	long		jdate; 
	struct tm	*sp;
	time_t		now;
	char		str[sizeof(long) + 1];
 
	now	= time((time_t *) NULL);
	sp	= localtime(&now);

        /*
         * Mod the year by 100 so it will be correct after year 1999;
         * user is required to know the century.
         */

	(void) sprintf(str, "%02d%03d   ", sp->tm_year % 100, sp->tm_yday + 1);

	jdate	= *(long *)str;

	if (_numargs() > 0)
#ifdef _ADDR64
		if (_numargs() > 1) {		/* If Fortran character */
#else
		if (_isfcd(julianday)) {	/* If Fortran character */
#endif
			unsigned int	len;
			char		*cp;

			cp	= _fcdtocp(julianday);
			len	= _fcdlen (julianday);

			(void) strncpy(cp, str, len);

			if (len > sizeof(long))
				(void) memset(cp + sizeof(long), (_f_int) ' ',
						len - sizeof(long));
		}
		else				/* Hollerith */
			**(long **) &julianday	= jdate;

	return ( (_f_int) jdate);
}
#else

#define	DATE_CHRS	8

_fcd
_JDATE_(julianday, iffcd)
	void	*julianday;		/* address of result, unless NULL    */
	int	iffcd;			/* zero     if 1st parm is long long */
					/* non-zero if 1st parm is _fcd      */
{
	struct tm	*sp;
	time_t		now;
	char		str[DATE_CHRS + 1];

	now = time((time_t *) NULL);
	sp = localtime(&now);

/*
 *	Mod the year by 100 so it will be correct after 1999.  User will be
 *	required to know the century.
 */

	(void) sprintf (str, "%02d%03d   ", sp->tm_year % 100, sp->tm_yday + 1);

	if (julianday != NULL)			/* a paramter was passed */
	    if (iffcd != 0) {			/* if Fortran character  */
		unsigned int	len;
		char		*cp;

		cp = (char *) julianday;
		len = iffcd;
		(void) strncpy (cp, str, len);
		if (len > DATE_CHRS)
		    (void) memset (cp + DATE_CHRS, (int) ' ', len - DATE_CHRS);
	    }

	return (_cptofcd (str, strlen(str)));
}
#endif
