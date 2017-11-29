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


#pragma ident "@(#) libu/trbk/interpret.c	92.1	06/23/99 18:14:34"
#include <ctype.h>
#include <float.h>
#include <math.h>
#include <stdio.h>
#include <string.h>

typedef struct {
#if __BYTE_ORDER == __LITTLE_ENDIAN
#ifdef	_CRAYIEEE
	unsigned int	mantissa : 52;
	unsigned int	exponent : 11;
#else
	unsigned int	mantissa : 48;
	unsigned int	exponent : 15;
#endif
	unsigned int	    sign : 1;
#else
	unsigned int	    sign : 1;
#ifdef	_CRAYIEEE
	unsigned int	exponent : 11;
	unsigned int	mantissa : 52;
#else
	unsigned int	exponent : 15;
	unsigned int	mantissa : 48;
#endif
#endif
} fpnum;

union whatever {
	long	slong;
	double	sdble;
	fpnum	snum;
	char	schar[sizeof(long)];
};

#define	INTVAL	1000000000	/* Cutoff point for integers */

#ifndef	_CRAYIEEE
#define	NBIT	0x800000000000	/* CRAY normalization bit */
#endif

#define	FLTLOW1	0.000001
#define	FLTUPP1	9999.999999

#define	SZOFLOW	9
static char	oflow[SZOFLOW]	= "Infinity";

#ifdef	_CRAYIEEE
#define	SZNAN	4
static char	nan[SZNAN]	= "NaN";
#endif

#define	SZWHAT	4
static char	what[SZWHAT]	= "???";

/*
 *	_interpret(word, string)
 *
 *	This routine does the bit-twiddling on a 64-bit word to get its
 *	best printable representation; either as an integer, floating-
 *	point number, or as a character string.  The interpretation is
 *	placed in string[] and terminated with a null character.  The
 *	actual number of characters in the representation is also
 *	returned as the function result.  The character array should be
 *	large enough to contain 20 characters.
 *
 *	This routine will work on 64-bit IEEE systems as well.
 *
 *	Typical use of _interpret() might look like:
 *
 *	long	word;
 *	char	string[20];
 *
 *	(void) _interpret(word, string);
 *
 *	printf(" word = %022o  %s\n", word, string);
 */

int
_interpret(
	long	word,
	char	*string
)
{
	register short	i, pchars;
	register char	*str;
	char		*fmt[2] = {"    %.8.6e", "   %.8.6e" };
	union whatever	w;

	pchars	= 0;
	w.slong	= word;
	str	= string;

	/* Count printable characters */

	for (i = 0; i < sizeof(long); i++)
		if (isprint(w.schar[i]))
			pchars	= pchars + 1;

	/* Check if word is a floating-point number */

#ifdef	_CRAYIEEE
	if ((w.snum.exponent == 03777 && w.snum.mantissa != 0) ||
	    (w.snum.exponent > 0 && pchars <= (sizeof(long)/2))) {
#else
	if ((w.slong & NBIT) != 0 && w.snum.exponent > 020000 &&
	    !(w.snum.sign == 1 && w.snum.exponent == 077777)) {
#endif

#ifdef	_CRAYIEEE

		/* Check for NaNs on IEEE systems */

		if (w.snum.exponent == 03777 && w.snum.mantissa != 0) {
			(void) strcpy(str, nan);
			return(SZNAN - 1);
		}

		/* Check for infinity/overflow */

		if (w.snum.exponent == 03777 && w.snum.mantissa == 0) {
#else
		if (w.snum.exponent >= 060000) {
#endif

			if (w.snum.sign == 1)
				*str++	= '-';

			(void) strcpy(str, oflow);

			return((str + SZOFLOW - 1) - string);
		}

		/* Check if word is a small floating-point number */

		if (fabs(w.sdble) <= FLTUPP1 && fabs(w.sdble) >= FLTLOW1) {

			i	= sprintf(str, "%12.6f", w.sdble) - 1;

			/* Trim trailing zeros */

			for ( ; i > 6; i--)
				if (str[i] != '0')
					break;
				else
					str[i]	= '\0';

			i	= i + 1;
		}
		else
			i	= sprintf(str, fmt[w.snum.sign], w.sdble);

		return(i);
	}

	/* Check if word is an integer */

	if (w.slong <= INTVAL && w.slong >= -INTVAL)
		return(sprintf(str, "%ld", w.slong));

	/* Interpret word as a character string */

	if (pchars == 0) {	/* If no printable characters */

		(void) strcpy(str, what);

		str	= str + SZWHAT - 1;
	}
	else {
		*str++	= '\'';

		for (i = 0; i < sizeof(long); i++)
			*str++	= isprint(w.schar[i]) ? w.schar[i] : '_';

		*str++	= '\'';
		*str	= '\0';
	}

	return(str - string);
}
