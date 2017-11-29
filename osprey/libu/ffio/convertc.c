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


#pragma ident "@(#) libu/ffio/convertc.c	92.2	10/07/99 22:14:49"

#include <errno.h>
#include <ffio.h>
#include <liberrno.h>
#include <stdlib.h>
#include <cray/fndc.h>
#include <cray/nassert.h>
#include <cray/portdefs.h>

#define BLANK	((long) ' ')
#define TBFSZ	1024

extern int
USCCTC(long *_Src, const long *_Offset, long *_Dest, const long *_Count, const long *_Npw);

extern int
USCCTI(long *_Src, long *_Dest, const long *_Offset, const long *_Count, const long *_Npw);

extern void
U6064(long *_Src, const long *_Offset, long *_Dest, const long *_Count, const long *_Stride);

extern void
P6460(long *_Src, long *_Dest, const long *_Offset, const long *_Count, const long *_Stride);

extern void
ASCDC(long *_Src, const long *_Offset, long *_Dest, const long *_Count);

extern void
DSASC(long *_Src, const long *_Offset, long *_Dest, const long *_Count);

/*
 * convert.c:	convert character data items (formatted I/O) to and
 * from foreign representations.
 *	Routines:
 *		_fdc_unpackc()	 convert foreign to ASCII, and unpack 
 *		_fdc_packc()	 convert ASCII to foreign, and pack 
 */

/*
 * _fdc_unpackc	Converts packed foreign characters to unpacked ASCII
 *		characters.  Output is unpacked ASCII data, one right-
 *		justified character per word.  'fillc' characters of
 *		blanks are added to the output buffer after 'count'
 *		characters have been converted.
 *
 *	Returns:  0 = OK
 *		 -1 = error (errno set)
 */
int _fdc_unpackc(
	void	*pbuf,		/* ptr to packed buffer (really a bitptr)*/
	long	*ubuf,		/* ptr to unpacked target area */
	long	count,		/* count of characters */
	int	fillc,		/* count of characters to blank fill */
	int	ctype)		/* conversion type */
{
	register int	status;
	register long	left;
	long		offset;
	const long	minus1 = -1;
	const long	one = 1;

	assert ( pbuf != NULL );
	assert ( ubuf != NULL );
	assert ( count > 0 );
	assert ( fillc >= 0 );
	assert ( ctype >= CS_NONE && ctype < CS_MAX );

	status	= 0;

	switch (ctype) {

		case CS_NONE:	/* No conversion */
		case CS_ASCII:
		case CS_USER:	/* Deferred */
		case CS_SITE:	/* Deferred */
			(void) _unpack((char *) pbuf, ubuf, count, minus1);
			break;

#if	NUMERIC_DATA_CONVERSION_ENABLED
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
		case CS_EBCDIC:
			offset	= (((unsigned long) pbuf) >> 61) + 1; /* Byte offset */
			status	= USCCTC((long *) pbuf, &offset, ubuf, &count,
					&minus1);
			break;
#endif	/* !__mips */

#if	(defined(_CRAY1) && !defined(_CRAYIEEE))
		case CS_CDC:
			left	= count;

			while (left > 0) {
				long	chunk;
				long	num;
				long	tbuf[TBFSZ+1];	/* Compensate for DSASC */

				chunk	= count;

				if (chunk > (TBFSZ * sizeof(long)))
					chunk	= TBFSZ * sizeof(long);

				offset	= ((unsigned long) pbuf) >> 58; /* Bit offset */
				num	= (chunk / 10) + 1;

				U6064((long *) pbuf, &offset, ubuf, &num, &one);

				offset	= 1;

				DSASC(ubuf, &offset, tbuf, &chunk); 

				(void) _unpack((char *) tbuf, ubuf, chunk, minus1);

				left	= left - chunk;
				pbuf	= (void *) (((char *) pbuf) + chunk);
				ubuf	= ubuf + chunk;
			}
			break;
#endif	/* defined(_CRAY1) && !defined(_CRAYIEEE) */

#endif	/* NUMERIC_DATA_CONVERSION_ENABLED */

		default:
			errno	= FELDDCNV;	/* Data conversion not available */
			status	= -1;
			break;
	} /* switch */

	/* Do blank padding (if no error) */

	if (status == 0 && fillc > 0) {
		register int	i;

		for (i = 0; i < fillc; i++)	/* Should vectorize */
			ubuf[count + i]	= BLANK;
	}

	return(status);
}
	
/*
 *	_fdc_packc	Converts unpacked ASCII characters to packed foreign
 *			characters.  Input is unpacked ASCII data, one right-
 *			justified character per word.
 *
 *	Returns:  0 = OK
 *		 -1 = error (errno set)
 */

int
_fdc_packc(
	char	*pbuf,	/* ptr to packed buffer (really a bitptr)*/
	long	*ubuf,	/* ptr to unpacked data */
	long	count,	/* count of characters */
	int	ctype)	/* conversion type */
{
	register int	status;
	register long	left;
	long		offset;
	const long	minus1 = -1;
	const long	one = 1;
 
	assert ( pbuf != NULL );
	assert ( ubuf != NULL );
	assert ( count > 0 );
	assert ( ctype >= CS_NONE && ctype < CS_MAX );

	status	= 0;

	switch (ctype) {

		case CS_NONE:	/* No conversion */
		case CS_ASCII:
		case CS_USER:	/* Deferred */
		case CS_SITE:	/* Deferred */
			(void) _pack(ubuf, (char *) pbuf, count, minus1);
			break;

#if	NUMERIC_DATA_CONVERSION_ENABLED
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
		case CS_EBCDIC:
			offset	= (((unsigned long) pbuf) >> 61) + 1; /* Byte offset */
			status	= USCCTI(ubuf, (long *) pbuf, &offset, &count,
					&minus1);
			break;
#endif	/* !__mips */

#if	(defined(_CRAY1) && !defined(_CRAYIEEE))
		case CS_CDC:
			left	= count;

			while (left > 0) {
				long		chunk;
				long		num;
				long		tbuf[TBFSZ+1];
				long		vbuf[TBFSZ+1];

				chunk	= count;

				if (chunk > (TBFSZ * sizeof(long)))
					chunk	= TBFSZ * sizeof(long);

				(void) _pack(ubuf, (char *) tbuf, chunk, minus1);

				offset	= 1;

				ASCDC(tbuf, &offset, vbuf, &chunk); 

				offset	= ((unsigned long) pbuf) >> 58; /* Bit offset */
				num	= (chunk / 10) + 1;

				P6460(vbuf, (long *) pbuf, &offset, &num, &one);

				left	= left - chunk;
				ubuf	= ubuf + chunk;
				pbuf	= (void *) (((char *) pbuf) + chunk);
			}
			break;
#endif	/* defined(_CRAY1) && !defined(_CRAYIEEE) */

#endif	/* NUMERIC_DATA_CONVERSION_ENABLED */

		default:
			errno	= FELDDCNV;	/* Data conversion not available */
			status	= -1;
			break;
	} /* switch */

	return(status);
}
