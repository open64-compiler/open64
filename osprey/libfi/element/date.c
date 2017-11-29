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


#pragma ident "@(#) libfi/element/date.c	92.1	06/16/99 15:47:23"
#include <fortran.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <sys/types.h>

#ifdef KEY /* Bug 8290 */
#include "element.h"
#endif /* KEY Bug 8290 */

/*
 *	DATE	Returns the current date in "MM/DD/YY" format.
 *		MM = Month (1 - 12).
 *		DD = Day (01 - 31).
 *		YY = Year modulus 100 (00 - 99).
 *
 *		May be called either as a function or a subroutine.
 *		If called as a subroutine, the parameter may be
 *		CHARACTER or INTEGER type.
 */

/*
 *	Duplicate names
 *
 *	_DATE_	- for f90 intrinsic
 *	DATE	- if called as a subroutine
 *	$DATE	- for cf77 intrinsic
 *	_DATE	- for f90 3.0? and previous on PVP systems 
 */
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
#pragma _CRI duplicate _DATE_ as DATE
#pragma _CRI duplicate _DATE_ as $DATE
#pragma _CRI duplicate _DATE_ as _DATE
#endif


/*	NOTE:	For 32-bit architectures, the DATE function is quite
 *		different from the 64-bit version.  Two parameters vs
 *		one, pointer to a pararmater vs. not, and different
 *		return values.  It was decided that it would be much
 *		easier to deal with these problems with two separate
 *		routines, using #ifdefs.
 */

#ifdef _UNICOS

_f_int
_DATE_(dayofyear)
_fcd	dayofyear;
{
	long		date;
	struct tm	*sp;
	time_t		now;
	char		str[sizeof(long) + 1];

	now	= time((time_t *) NULL);
	sp	= localtime(&now);
 
        /*
         * Mod the year by 100 so it will be correct after year 1999;
         * user is required to know the century.
         */
  
	(void) sprintf(str, "%02d/%02d/%02d", sp->tm_mon+1, sp->tm_mday,
			sp->tm_year % 100);

        date	= *(long *) str;

	if ( _numargs() > 0)
#ifdef  _ADDR64
		if (_numargs() > 1) {		/* If Fortran character */
#else
		if (_isfcd(dayofyear)) {	/* If Fortran character */
#endif
			unsigned int	len;
			char		*cp;

			cp	= _fcdtocp(dayofyear);
			len	= _fcdlen (dayofyear);

			(void) strncpy(cp, str, len);

			if (len > sizeof(long))
				(void) memset(cp + sizeof(long), (_f_int) ' ',
						len - sizeof(long));
		}
		else				/* Hollerith */
			**(long **) &dayofyear	= date;

	return( (_f_int) date );
}

#else

#define	DATE_CHRS	8

_fcd
_DATE_(dayofyear, iffcd)
	void	*dayofyear;	/* address of result, unless NULL	*/
	int	iffcd;		/* zero     if 1st param is long long	*/
				/* non-zero if 1st param is _fcd	*/

{
	struct tm	*sp;
	time_t		now;
	char		str[DATE_CHRS + 1];

	now = time((time_t *) NULL);
	sp = localtime(&now);

/*
 *	Mod the year by 100 so it will be correct after 1999.  The use
 *	is required to know the century.
 */

	(void) sprintf(str, "%02d/%02d/%02d", sp->tm_mon+1, sp->tm_mday,
		sp->tm_year %100);

	if (dayofyear != NULL)			/* parameter was passed */
	    if (iffcd != 0) {			/* If Fortran character */
		unsigned int	len;
		char		*cp;

		cp = (char *) dayofyear;
		len = iffcd;
		(void) strncpy (cp, str, len);
		if (len > DATE_CHRS)
		    (void) memset (cp + DATE_CHRS, (int) ' ', len -DATE_CHRS);
	    }

	return (_cptofcd(str, strlen(str)));
}
#endif
