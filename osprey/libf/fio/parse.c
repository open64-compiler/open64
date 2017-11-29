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



#pragma ident "@(#) libf/fio/parse.c	92.2	06/21/99 10:37:21"
#include <liberrno.h>
#include <memory.h>
#include <cray/format.h>
#include <cray/mtlock.h>
#include "fio.h"

/*
 *	_parse		Parse a Fortran format; called during initialization
 *			of a formatted I/O statement.
 *
 *	Parameters:
 *
 *		css	Pointer Fortran I/O statement state.
 *		cup	Pointer to unit table.
 *		prsfmt	Pointer to pointer to the preparsed format.
 *
 *	Returns:
 *		library error number, or zero if no error.
 *
 *      Calls:
 *		_fmt_parse()
 */

int
_parse	(
	FIOSPTR		css,		/* Pointer to I/O statement state */
	unit		*cup,		/* Pointer to unit table */
	fmt_type	**prsfmt 	/* Pointer to pointer to parsed fmt. */
)
{
	char		*fptr;		/* Pointer to format */
	int		errn;		/* Error indicator */
	long		flen;		/* Length of format */
	fmt_type	*pfmt;		/* Pointer to parsed format */

	errn	= 0;
	fptr	= css->u.fmt.u.fe.fmtbuf;
	flen	= css->u.fmt.u.fe.fmtlen;

	PARSELOCK();		/* Lock parsing */

	/*
	 * If the format is a variable format, or if it has not yet
	 * been parsed, or if it was parsed by an incompatible version
	 * of the format parser, then parse it.
	 */

	if (prsfmt == NULL || *prsfmt == NULL ||
	    (**prsfmt).offset != PARSER_LEVEL) { /* If not parsed */

		msg_type	fmt_info;

		pfmt	= _fmt_parse(NULL, fptr, LIB_CALL, &flen, &fmt_info);

		if (pfmt == (fmt_type *) NULL) {	/* If error */

			/*
			 * The following statement will map a format parser
			 * error into one of the following errors:
			 *
			 * FEFMTELP	Expecting left parenthesis
			 * FEFMTERP	Expecting right parenthesis
			 * FEFMTEIN	Expecting integer
			 * FEFMTEPE	Expecting period
			 * FEFMTEPX	Expecting P or X
			 * FEFMTIRP	Invalid repetition count
			 * FEFMTZRP	Zero repetition count
			 * FEFMTZFW	Zero field width
			 * FEFMTFTL	Field too large
			 * FEFMTZMH	Zero or missing hollerith count
			 * FEFMTIED	Invalid edit descriptor
			 * FEFMTNLS	Nonterminated literal string
			 * FEFMTMEM	Unable to allocate memory
			 */

#define	OFFSET	(FEFMTBAS - FIRST_FATAL_MESSAGE)

#if	FEFMTELP != (OFFSET + EXPECTING_LEFT_PAREN)
#error			Error message alignment problem (FEFMTELP)
#endif

#if	FEFMTERP != (OFFSET + EXPECTING_RIGHT_PAREN)
#error			Error message alignment problem (FEFMTERP)
#endif

#if	FEFMTEIN != (OFFSET + EXPECTING_INTEGER)
#error			Error message alignment problem (FEFMTEIN)
#endif

#if	FEFMTEPE != (OFFSET + EXPECTING_PERIOD)
#error			Error message alignment problem (FEFMTEPE)
#endif

#if	FEFMTEPX != (OFFSET + EXPECTING_P_OR_X)
#error			Error message alignment problem (FEFMTEPX)
#endif

#if	FEFMTIRP != (OFFSET + INVALID_REP_COUNT)
#error			Error message alignment problem (FEFMTIRP)
#endif

#if	FEFMTZRP != (OFFSET + ZERO_REP_COUNT)
#error			Error message alignment problem (FEFMTZRP)
#endif

#if	FEFMTZFW != (OFFSET + FIELD_WIDTH_ZERO)
#error			Error message alignment problem (FEFMTZFW)
#endif

#if	FEFMTFTL != (OFFSET + FIELD_TOO_LARGE)
#error			Error message alignment problem (FEFMTFTL)
#endif

#if	FEFMTZMH != (OFFSET + ZERO_OR_NO_HOLLERITH_CNT)
#error			Error message alignment problem (FEFMTZMH)
#endif

#if	FEFMTIED != (OFFSET + UNKNOWN_EDIT_DESCRIPTOR)
#error			Error message alignment problem (FEFMTIED)
#endif

#if	FEFMTNLS != (OFFSET + NONTERMINATED_LITERAL)
#error			Error message alignment problem (FEFMTNLS)
#endif

#if	FEFMTMEM != (OFFSET + UNABLE_TO_MALLOC_MEMORY)
#error			Error message alignment problem (FEFMTMEM)
#endif

			errn	= fmt_info.msg_number + OFFSET;

			css->u.fmt.u.fe.fmtcol	= fmt_info.msg_column;
		}
		else {
			css->u.fmt.u.fe.pfmt	= pfmt;

			if (prsfmt == NULL)	/* If variable format */
				css->u.fmt.freepfmt	= 1; /* Dealloc flag */
			else			/* Else constant format */
				*prsfmt	= pfmt;
		}
	}
	else			/* if format was already parsed */
		css->u.fmt.u.fe.pfmt	= *prsfmt;

	PARSEUNLOCK();

	return (errn);
}
