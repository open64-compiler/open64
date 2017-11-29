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


#pragma ident "@(#) libu/util/c1/unitts.c	92.1	07/07/99 13:22:26"
#include <fortran.h>
#include <malloc.h>
#include <string.h>

/*
 *      UNITTS - returns time stamp units in specified standard time units.
 *             A version of UNITTS existed on COS.  Timestamp units are
 *             nanoseconds.  Timestamps are nanoseconds since January 1, 1973.
 *
 *      Called from Fortran with a Hollerith value for units:
 *
 *              INTEGER UNITTS, periods, units
 *
 *              its = UNITTS(periods, units)
 *
 *      Called from Fortran with a character value for units:
 *
 *              INTEGER UNITTS, periods
 *
 *              CHARACTER units
 *
 *              its = UNITTS(periods, units)
 *
 *      INPUT ARGUMENTS:
 *
 *            its = 64-bit integer containing timestamp units
 *
 *            periods = Number of units to be converted to ts units
 *
 *            units = Hollerith constant containing:
 *
 *                         'DAYS'H
 *                         'HOURS'H
 *                         'MINUTES'H
 *                         'SECONDS'H
 *                         'MSEC'H           (milliseconds)
 *                         'USEC'H           (microseconds)
 *                         'USEC100'H        (100s of microseconds)
 *
 *        or  units = character variable, expression, or constant
 *                    containing:
 *
 *                         'DAYS'
 *                         'HOURS'
 *                         'MINUTES'
 *                         'SECONDS'
 *                         'MSEC'            (milliseconds)
 *                         'USEC'            (microseconds)
 *                         'USEC100'         (100s of microseconds)
 *
 *      RESULT:
 *
 *            Successful UNITTS:
 *
 *            its = 64-bit integer containing timestamp units
 *
 *            Unsuccessful UNITTS:
 *
 *            its = -1 indicates units is invalid for this function
 *      
 *      ALGORITHM:
 *
 *            1. Determine if units is Hollerith constant or character
 *               entity.
 *            2. If Hollerith constant, find length without trailing blanks
 *            3. If character, store the value of units in a temporary C
 *               character array with a null character at the end.  Return
 *               -2 if no allocation could occur for the temporary.
 *            4. Determine the value of units and pick up the timestamp
 *               units for that granularity.  Return -1 if units is an 
 *               invalid value for this function.
 *            5. Multiply the value in period by the timestamp units chosen
 *               by units.
 *            6. Free the temporary C character array, if present
 *            7. Return the result of the multiplication.
 *      
 */
_f_int
UNITTS(periods, units)
long	*periods;
long	units;
{
	int	u_fcd;
	char	*cptr;
	int len;
	_f_int	value;
	double	tsu;

	/* Check for character variable */
	u_fcd	= 0;
#ifdef	_ADDR64
	if (_numargs() * sizeof(long) == sizeof(long*) + sizeof(_fcd)) 
#else
	if (_isfcd(units))
#endif
		u_fcd	= 1;

	if (u_fcd) {		/* If character variable */
		_fcd	fcdunt;

		fcdunt	= *(_fcd *) &units;

		if ((cptr = _f2ccpy(fcdunt)) == 0 )
			return( (_f_int) -2);

		len	= strlen(cptr);
	}
	else {
		cptr	= (char *) units;
		len	= sizeof(long);

		while (len > 0 && cptr[len-1] == ' ')
			len--;
	}

	if( strncmp(cptr, "DAYS", len) == 0 )
		/* periods * 24 * 60 * 60 * 1.0E9, nanoseconds in a day */
		tsu = 86400.0E9;
	else if( strncmp(cptr, "HOURS", len) == 0)
		/* periods * 60 * 60 * 1.0E9, nanoseconds in an hour */
		tsu =  3600.0E9;
	else if( strncmp(cptr, "MINUTES", len) == 0 )
		/* periods * 60 * 1.0E9, nanoseconds in a minute */
		tsu = 60.0E9;
	else if( strncmp(cptr, "SECONDS", len) == 0)
		/* periods * 1.0E9, nanoseconds in a second */
		tsu =  1.0E9;
	else if( strncmp(cptr, "MSEC", len) == 0)
		/* periods * 1.0E6, nanoseconds in a millisecond */
		tsu = 1.0E6;
	else if( strncmp(cptr, "USEC", len) == 0 )
		/* periods * 1.0E3, nanoseconds in a microsecond */
		tsu =  1.0E3;
	else if( strncmp(cptr, "USEC100", len) == 0)
		/* periods * 1.0E5, nanoseconds in 100 microseconds */
		tsu = 1.0E5;
	else	{
		if (u_fcd)
			free(cptr);
		return( (_f_int) -1);
	}

	value	= (_f_int) ((double) *periods * tsu);

	if (u_fcd)
		free(cptr);

	return(value);
}
