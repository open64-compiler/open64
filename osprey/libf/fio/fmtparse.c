/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
 */

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



#ifndef	_OLD_ERROR_NUMBERS
#pragma ident "@(#) libf/fio/fmtparse.c	92.3	06/18/99 19:52:04"
#endif
#include "lio.h"	/* For spiffy IS_DIGIT() macro */
#include <ctype.h>
#ifndef	_ABSOFT
#include <malloc.h>
#else
#include <stdlib.h>
#endif
#include <string.h>
#include <cray/format.h>
#include <cray/nassert.h>
#include <cray/portdefs.h>

typedef struct	{
	char		fmt_ch;		/* Current character in format     */
	char		*fmt_ptr;	/* Pointer to current format char. */
	short		caller;		/* Caller is library or compiler   */
	short		depth;		/* Current format nesting depth    */
	short		maxdepth;	/* Maximum format nesting depth    */
	short		fatal_err;	/* Flag to indicate fatal error    */
	long		desc_col;	/* Pointer to actual edit desc.    */
	long		fmt_pos;	/* Current position in format      */
	long		fmt_len;	/* Number of characters in format  */
	fmt_type	*parsed;	/* Pointer to parsed format block  */
	fmt_type	*pptr;		/* Pointer to current parsed entry */
	fmt_type	*revert;	/* Pointer to reversion point      */
	msg_type	*stat;		/* Pointer to error status word    */
	_Error_function	*iss_msg;	/* Pointer to format error handler */
} parse_block;

/* Declare 'forward' functions for type-checking */

static void
fmterr (	parse_block	*pfmt,
		short		msg_num,
		short		code,
		long		column);

static short
process_paren_group (	parse_block	*pfmt,
			fmt_type	*ploc);

/* Table of nonrepeatable characters */

static int64 non_repeatable[2] = {
	0x00000000297EFFE0LL,	/* " $ ' ) * + , - . 0-9 : */
	0x0000180800001800LL	/* S T \ s t */
};

/*
 * Macros to return next character and gather up a number.
 * At start, fmt_ptr needs to be incremented.  At end,
 * fmt_ptr points to the returned character.  Input is
 * a Fortran character string, so EOS is not always '\0'.
 */

#define GET(P) {						\
	do {							\
		if (++P->fmt_pos > P->fmt_len) {		\
			P->fmt_ch	= '\0';			\
			P->fmt_pos--;				\
			break;					\
		}						\
		P->fmt_ch	= *(++P->fmt_ptr);		\
	} while (P->fmt_ch == ' ' || P->fmt_ch == '\t');	\
}

#define GETNUM(P, M) {						\
	do {							\
		M	= (M + M + (M << 3)) + ((int64) P->fmt_ch - ZERO);\
		GET(P);						\
	} while (IS_DIGIT(P->fmt_ch));				\
}

/* Bridge for symbol renaming */

#ifndef	E_WITH_D_NON_ANSI
#define	E_WITH_D_NON_ANSI	DW_IS_NON_ANSI
#endif


#ifndef __LITTLE_ENDIAN

// Byte 3 swaps with byte 0, and byte 2 swaps with byte 1
static void inline byte_swap (unsigned int *) __attribute__ ((always_inline));

static void inline byte_swap (unsigned int * iptr)
{
  unsigned char tmp;
#ifdef KEY /* Mac port */
  unsigned char * cptr = (unsigned char *) iptr;
#else /* KEY Mac port */
  unsigned char * cptr = (char *) iptr;
#endif /* KEY Mac port */

  tmp = cptr[0];
  cptr[0] = cptr[3];
  cptr[3] = tmp;

  tmp = cptr[1];
  cptr[1] = cptr[2];
  cptr[2] = tmp;
}

// 'in' is an array of length 'length' of struct fmt_entry. For each
// array element, convert little-endian field assignments to big-endian
// format. Skip array elements determined by STRING_ED.
static void
big_endian_store (struct fmt_entry * in, int length)
{
  unsigned int * iptr;
  struct fmt_entry tmp;
  int i = 0;

  while (i < length)
  {
    iptr = (unsigned int *) (&in[i]);
    bzero (&tmp, sizeof (struct fmt_entry));
    memcpy (&tmp, &in[i], sizeof (struct fmt_entry));
    bzero (&in[i], sizeof (struct fmt_entry));
    iptr[0] = (tmp.op_code << 25) | (tmp.default_digits << 24) | tmp.digits_field;
    iptr[1] = (tmp.exponent << 26) | (tmp.reserved2 << 24) | tmp.field_width;
    iptr[2] = (tmp.rgcdedf << 31) | (tmp.reserved3 << 16) | tmp.offset;
    iptr[3] = tmp.rep_count;

    byte_swap (&iptr[0]);
    byte_swap (&iptr[1]);
    byte_swap (&iptr[2]);
    byte_swap (&iptr[3]);

    if (tmp.op_code == STRING_ED)
      i += ((tmp.field_width + FMT_ENTRY_BYTE_SIZE - 1) / FMT_ENTRY_BYTE_SIZE) + 1;
    else 
      i++;
  }
}

#endif


/*
 *	_fmt_parse()
 *
 *	Description:	This routine initializes variables for the format
 *			parser, positions the input to the first character,
 *			calls the actual format parser and cleans up.
 *
 *	Called By:	cft77, cft90 and the I/O library.
 *
 *	Calls:		process_paren_group
 *			calloc
 *			realloc
 *			fmterr
 *
 *	Input parameters:
 *		msg_rtn		Pointer to a pointer to the message routine to 
 *				be called if this is a compiler call.  NULL if 
 *				this is a library call.
 *		format_str	Pointer to the format string to be parsed.
 *		routine_caller	Library/compiler flag.
 *
 *	Output parameters:
 *		fmt_str_len	The length in words of the parsed format.
 *		msg_out_ptr	Pointer to the list of messages found.
 *
 *	Returns:	A pointer to the parsed format string.  The first
 *			two-word entry of the parsed format is for control
 *			information; the first word is reserved for use by
 *			the compiler, the second word is reserved for use
 *			by the parser and library.
 *
 *	Note:		The input parameters are pointers to interface with
 *			Pascal.
 */

fmt_type *
_fmt_parse(
	_Error_function	**msg_rtn,
	char		*format_str,
	long		routine_caller,
	long		*fmt_str_len,
	msg_type	*lib_err_msg
)
{
	register short	length;
	parse_block	*pfmt, p;

	/* Basic assertions */

	assert (format_str != NULL);
	assert (routine_caller >= 0 && routine_caller <= MAX_CALL_FLAG);
	assert (fmt_str_len != NULL);
	assert (*fmt_str_len > 0);
	assert (routine_caller == LIB_CALL ? lib_err_msg != NULL : 1);
	assert (routine_caller != LIB_CALL ? msg_rtn != NULL : 1);

	/*
	 * If this routine is called from the library (routine_caller =
	 * LIB_CALL) then the parser returns the number and position of the
	 * first fatal error found--if any--and then exits.  Any other caller
	 * (e.g., compilers) must provide their own error routine by passing
	 * a pointer to the routine in the first argument.  The parser will
	 * continue parsing until the end of the format is reached.
	 */

	pfmt		= &p;

	pfmt->fmt_pos	= 0;
	pfmt->depth	= 0;
	pfmt->maxdepth	= 0;
	pfmt->fatal_err	= FALSE;
	pfmt->iss_msg	= (msg_rtn == NULL ? NULL : *msg_rtn);
	pfmt->stat	= lib_err_msg;
	pfmt->fmt_ptr	= format_str - 1; /* Set for first GET call */
	pfmt->fmt_len	= *fmt_str_len;
	pfmt->caller	= routine_caller;

	GET(pfmt);

	pfmt->desc_col	= pfmt->fmt_pos;

	if (pfmt->fmt_ch == '(') {
		GET(pfmt);
	}
	else {
		fmterr(pfmt, EXPECTING_LEFT_PAREN, FALL, 0);

		/* Library quits at first fatal error */

		if (pfmt->caller == LIB_CALL)
			return( (fmt_type *) NULL);
	}

	/*
	 * We need to allocate a parsed format structure with enough
	 * entries to accommodate this format (we'll free any unused
	 * entries when we're done).  Our initial guess is the number
	 * of characters in the format plus two (one for the header and
	 * one for the REVERT_OP entry).  Note that for a long, sparse
	 * format (with lots of blanks), this will allocate way too
	 * much space.
	 */

	pfmt->parsed	= (fmt_type *) calloc(pfmt->fmt_len + 2,
						sizeof(fmt_type));

	if (pfmt->parsed == NULL) {

		fmterr(pfmt, UNABLE_TO_MALLOC_MEMORY, FALL, 0);

		/* No place for format, so quit */

		return( (fmt_type *) NULL);
	}

	pfmt->pptr		= pfmt->parsed + 1;
	pfmt->revert		= pfmt->pptr;

	/* Parse the format string */

	(void) process_paren_group(pfmt, pfmt->pptr);

	if (pfmt->fatal_err) {
		free( (char *) pfmt->parsed);	/* Return memory */
		pfmt->parsed	= NULL;
		length		= 0;
	}
	else {
		length			= pfmt->pptr - pfmt->parsed;
		pfmt->parsed->offset	= PARSER_LEVEL;
		pfmt->parsed->rep_count	= pfmt->maxdepth + 1;

		if (pfmt->fmt_ch != '\0')
			fmterr(pfmt, TRAILING_CHARS, FALL, 0);

		if (pfmt->caller == LIB_CALL)
			pfmt->parsed	= (fmt_type *) realloc (
						(char *) pfmt->parsed,
						length * FMT_ENTRY_BYTE_SIZE );
	}

	*fmt_str_len	= length * FMT_ENTRY_WORD_SIZE;
#ifndef __LITTLE_ENDIAN
	if (pfmt->caller != LIB_CALL)
	  big_endian_store (pfmt->parsed, length);
#endif
	return(pfmt->parsed);

} /* _parsfmt */

/*
 *	fmterr()
 *
 *	Description:	This routine processes errors encountered while
 *			parsing formats.
 *
 *	Called By:	All routines in this file.
 *
 *	Input parameters:
 *		pfmt		Parsing information block
 *		msg_num		Message number (see format.h)
 *		code		Compiler applicability code
 *				FALL	Error/warning applies to all compilers
 *				F77	Error/warning applies only to f77
 *				F90	Error/warning applies only to f90
 *				F95	Error/warning applies only to f95
 *		column		Column number corresponding to error.  If
 *				zero, use current position.
 *
 *	Returns:	Nothing
 *
 *	Note:		Two column pointers are passed to the error processing
 *			routine.  The first points to the position in the
 *			format where the actual error occurred; the second
 *			points to the edit descriptor being processed.
 */

static void
fmterr(
	parse_block	*pfmt,
	short 		msg_num,
	short 		code,
	long 		column
)
{
	register short	callflg;

	callflg	= 0;	/* Assume no call to error function */

	if (msg_num >= FIRST_FATAL_MESSAGE)
		pfmt->fatal_err	= TRUE;

	if (column == 0)
		column	= pfmt->fmt_pos;

	switch (pfmt->caller) {

		case LIB_CALL:

			/* Ignore warnings and non-ANSI messages */

			if (msg_num >= FIRST_FATAL_MESSAGE) {
				pfmt->stat->msg_number	= msg_num;
				pfmt->stat->msg_column	= column;
				pfmt->stat->desc_column	= pfmt->desc_col;
			}
			break;

		case COMPILER_CALL_NO_ANSI:

			/* Ignore ANSI messages */

			callflg	= (msg_num < FIRST_NON_ANSI_MESSAGE ||
				   msg_num >= FIRST_FATAL_MESSAGE);
			break;

		case COMPILER_CALL_ANSI:

			/* Call compiler error routine */

			callflg	= 1;
			break;

		case COMPILER_CALL_ANSI_77:

			/* Ignore any non-f77 messages */

			callflg	= (code & F77);
			break;

		case COMPILER_CALL_ANSI_90:

			/* Ignore any non-f90 messages */

			callflg	= (code & F90);
			break;

		case COMPILER_CALL_ANSI_95:

			/* Ignore any non-f95 messages */

			callflg	= (code & F95);
			break;
	} /* switch */

	if (callflg != 0)
		(*pfmt->iss_msg) (msg_num, column, pfmt->desc_col);

	return;

} /* fmterr */

/*
 *	recover()
 *
 *	Description:	This routine tries to recover from a fatal error by
 *			searching for the character: ',', ')', '(', '"', '*',
 *			"'", or EOF after the error.  This should position
 *			the parser at a valid format descriptor.
 *
 *	Called By:	process_paren_group
 *			process_defg
 *			process_bioz
 *			process_arl
 *			process_minus
 *
 *	Input parameters:
 *		pfmt		Parsing information block
 *
 *	Returns:	Nothing
 */

static void
recover(
	parse_block	*pfmt
)
{
	register short	found_char;

	found_char	= FALSE;

	if (pfmt->caller != LIB_CALL)
		do {			
			switch (pfmt->fmt_ch) {
				case ',':		
				case ')':	
				case '(':
				case '"':			
				case '*':		
				case '\'':	
				case '\0':
					found_char	= TRUE;	
					break;	

				default:
					GET(pfmt);		
					break;	
			} /* switch */		
		} while (!found_char);

	return;

} /* recover */

/*
 *	nonzero_integer()
 *
 *	Description:	This routine finds and returns a nonzero integer
 *			or issues an error.
 *
 *	Called By:	process_t
 *
 *	Calls:		fmterr
 *
 *	Input parameters:
 *		pfmt		Parsing information block
 *
 *	Output parameters:
 *		size		The nonzero integer.
 *
 *	Returns:	TRUE if a nonzero integer is found, else FALSE.
 */

static short
nonzero_integer(
	parse_block	*pfmt,
	long		*size
)
{
	register short	return_val;
	register int64	value;
	register long	col;

	if (IS_DIGIT(pfmt->fmt_ch)) {

		col		= pfmt->fmt_pos;
		return_val	= TRUE;
		value		= *size;

		GETNUM(pfmt, value);

		if (value == 0) {
			fmterr(pfmt, FIELD_WIDTH_ZERO, FALL, col);
			value	= 1;
		}
		else
			if (value > MAX_FIELD_WIDTH) {
				fmterr(pfmt, FIELD_TOO_LARGE, FALL, col);
				value	= MAX_FIELD_WIDTH;
			}
	}
	else {
		fmterr(pfmt, EXPECTING_INTEGER, FALL, 0);
		return_val	= FALSE;
		value		= 1;
	}

	*size	= value;	/* Update value */

	return(return_val);

} /* nonzero_integer */

/*
 *	process_arl()
 *
 *	Description:	This does semantic checking and generates text for
 *			the A, L and R data edit-descriptors.
 *
 *	Called By:	process_paren_group
 *
 *	Calls:		fmterr
 *
 *	Input parameters:
 *		pfmt		Parsing information block
 *		op_code		Is it A, L or R?
 *
 *	Valid forms:
 *			A
 *			Aw
 *			L	(MIPSpro extension)
 *			Lw
 *			R	(MIPSpro extension)
 *			Rw
 *
 *	Returns:	Nothing
 */

static void
process_arl(
	parse_block	*pfmt,
	unsigned short	op_code
)
{
	register long 	col;
	register int64 	size;

	size	= 0;

	GET(pfmt);

	if (IS_DIGIT(pfmt->fmt_ch)) {

		col	= pfmt->fmt_pos;

		GETNUM(pfmt, size);

		if (size == 0) {
#ifdef	_OLD_ERROR_NUMBERS
			fmterr(pfmt, FIELD_WIDTH_ZERO, FALL, col);
			size	= 1;
#else
			fmterr(pfmt, ZERO_WIDTH_NON_ANSI, FALL, col);
#endif
		}
		else
			if (size > MAX_FIELD_WIDTH) {
				fmterr(pfmt, FIELD_TOO_LARGE, FALL, col);
				size	= MAX_FIELD_WIDTH;
			}
	}
	else
		if (op_code != A_ED) {
#ifdef	_OLD_ERROR_NUMBERS
			fmterr(pfmt, EXPECTING_INTEGER, FALL, 0);
			recover(pfmt);
#else
			fmterr(pfmt, MISSING_WIDTH_NON_ANSI, FALL, pfmt->fmt_pos);
#endif
		}

	pfmt->pptr->op_code	= op_code;
	pfmt->pptr->field_width	= size;
	pfmt->pptr		= pfmt->pptr + 1;

	return;

} /* process_arl */

/*
 *	process_defg()
 *
 *	Description:	This does semantic checking and generates text for
 *			the D, E, EN, ES, F, and G data edit-descriptors.
 *
 *	Called By:	process_paren_group
 *
 *	Calls:		fmterr
 *
 *	Input parameters:
 *		pfmt		Parsing information block
 *		op_code		Is it D, E, EN, ES, F or G?
 *
 *	Valid forms:
 *			D	(MIPSpro extension)
 *			Dw.d
 *			Dw.dEe	(Cray extension)
 *			E	(MIPSpro extension)
 *			Ew.d
 *			Ew.dEe
 *			EN	(f90 or later, MIPSpro-style extension)
 *			ENw.d	(f90 or later)
 *			ENw.dEe	(f90 or later)
 *			ES	(f90 or later, MIPSpro-style extension)
 *			ESw.d	(f90 or later)
 *			ESw.dEe	(f90 or later)
 *			F	(MIPSpro extension)
 *			Fw.d	(w can be zero in f95 or later)
 *			G	(MIPSpro extension)
 *			Gw.d
 *			Gw.dEe
 *
 *	Returns:	Nothing
 */

static void
process_defg(
	parse_block	*pfmt,
	unsigned short	op_code
)
{
	register short	dset;
	register long 	col;
	register int64 	esize;
	register int64 	dsize;
	register int64 	wsize;

	dset	= 1;
	dsize	= 0;
	esize	= 0;
	wsize	= 0;

	GET(pfmt);

	if (IS_DIGIT(pfmt->fmt_ch)) {

		col	= pfmt->fmt_pos;

		GETNUM(pfmt, wsize);

		if (wsize == 0) {
#ifdef	_OLD_ERROR_NUMBERS
			fmterr(pfmt, FIELD_WIDTH_ZERO, FALL, col);
			wsize	= 1;
#else
			register short	code;

			code	= (op_code == F_ED) ? (F77 | F90) : FALL;

			fmterr(pfmt, ZERO_WIDTH_NON_ANSI, code, col);
#endif
		}
		else
			if (wsize > MAX_FIELD_WIDTH) {
				fmterr(pfmt, FIELD_TOO_LARGE, FALL, col);
				wsize	= MAX_FIELD_WIDTH;
			}

		if (pfmt->fmt_ch == '.') {

			GET(pfmt);

			if (IS_DIGIT(pfmt->fmt_ch)) {

				col	= pfmt->fmt_pos;
				dset	= 0;
				dsize	= 0;

				GETNUM(pfmt, dsize);

				if (dsize > MAX_DECIMAL_FIELD) {
					fmterr(pfmt, FIELD_TOO_LARGE, FALL, col);
					dsize	= MAX_DECIMAL_FIELD;
				}

				if (toupper(pfmt->fmt_ch) == 'E' &&
				    op_code != F_ED) {
					register long	col_e;

					col_e	= pfmt->fmt_pos; /* Position of 'E' */

					GET(pfmt);

					if (IS_DIGIT(pfmt->fmt_ch)) {

						col	= pfmt->fmt_pos;

						GETNUM(pfmt, esize);

						if (esize == 0) {
							fmterr(pfmt,
								FIELD_WIDTH_ZERO,
								FALL,
								col);
							esize	= 1;
						}
						else
							if (esize > MAX_EXPONENT) {
								fmterr(pfmt,
								FIELD_TOO_LARGE,
								FALL, col);
								esize	= MAX_EXPONENT;
							}

						if (op_code == D_ED)
							fmterr(pfmt, E_WITH_D_NON_ANSI,
								FALL, col_e);
					}
					else {
						fmterr(pfmt, EXPECTING_INTEGER,
							FALL, 0);
						recover(pfmt);
					}
				}
			}
			else {
				fmterr(pfmt, EXPECTING_INTEGER, FALL, 0);
				recover(pfmt);
			}
		}
		else {
			fmterr(pfmt, EXPECTING_PERIOD, FALL, 0);
			recover(pfmt);
		}
	}
	else {
#ifdef	_OLD_ERROR_NUMBERS
		fmterr(pfmt, EXPECTING_INTEGER, FALL, 0);
		recover(pfmt);
#else
		fmterr(pfmt, MISSING_WIDTH_NON_ANSI, FALL, pfmt->fmt_pos);
#endif
	}

	pfmt->pptr->op_code		= op_code;
	pfmt->pptr->exponent		= esize;
	pfmt->pptr->field_width		= wsize;
	pfmt->pptr->digits_field	= dsize;
	pfmt->pptr->default_digits	= dset;
	pfmt->pptr			= pfmt->pptr + 1;

	return;

} /* process_defg */

/*
 *	process_bioz()
 *
 *	Description:	This does semantic checking and generates text for
 *			the B, I, O, and Z data edit-descriptors.
 *
 *	Called By:	process_paren_group
 *
 *	Calls:		fmterr
 *
 *	Input parameters:
 *		pfmt		Parsing information block
 *		op_code		Is it B, I, O or Z?
 *
 *	Valid forms:
 *			B	(MIPSpro extension)
 *			Bw	(w can be zero in f95 or later)
 *			Bw.m	(w can be zero in f95 or later)
 *			I	(MIPSpro extension)
 *			Iw	(w can be zero in f95 or later)
 *			Iw.m	(w can be zero in f95 or later)
 *			O	(MIPSpro extension)
 *			Ow	(w can be zero in f95 or later)
 *			Ow.m	(w can be zero in f95 or later)
 *			Z	(MIPSpro extension)
 *			Zw	(w can be zero in f95 or later)
 *			Zw.m	(w can be zero in f95 or later)
 *
 *	Returns:	Nothing
 */

static void
process_bioz(
	parse_block	*pfmt,
	unsigned short	op_code
)
{
	register short	dset;
	register long 	col;
	register int64 	dsize;
	register int64 	wsize;

	dset	= 1;
	dsize	= 1;
	wsize	= 0;

	GET(pfmt);

	if (IS_DIGIT(pfmt->fmt_ch)) {

		col	= pfmt->fmt_pos; 

		GETNUM(pfmt, wsize);

		if (wsize == 0) {
#ifdef	_OLD_ERROR_NUMBERS
			fmterr(pfmt, FIELD_WIDTH_ZERO, FALL, col);
			wsize	= 1;
#else
			fmterr(pfmt, ZERO_WIDTH_NON_ANSI, (F77 | F90), col);
#endif
		} else
			if (wsize > MAX_FIELD_WIDTH) {
				fmterr(pfmt, FIELD_TOO_LARGE, FALL, col);
				wsize	= MAX_FIELD_WIDTH;
			}

		if (pfmt->fmt_ch == '.') {

			GET(pfmt);

			if (IS_DIGIT(pfmt->fmt_ch)) {

				col	= pfmt->fmt_pos; 
				dsize	= 0;
				dset	= 0;

				GETNUM(pfmt, dsize);

				if (dsize > MAX_DECIMAL_FIELD) {
					fmterr(pfmt, FIELD_TOO_LARGE, FALL, col);
					dsize	= MAX_DECIMAL_FIELD;
				}
			}
			else {
				fmterr(pfmt, EXPECTING_INTEGER, FALL, 0);
				recover(pfmt);
			}
		}
	}
	else {
#ifdef	_OLD_ERROR_NUMBERS
		fmterr(pfmt, EXPECTING_INTEGER, FALL, 0);
		recover(pfmt);
#else
		fmterr(pfmt, MISSING_WIDTH_NON_ANSI, FALL, pfmt->fmt_pos); 
#endif
	}

	pfmt->pptr->op_code		= op_code;
	pfmt->pptr->field_width		= wsize;
	pfmt->pptr->digits_field	= dsize;
	pfmt->pptr->default_digits	= dset;
	pfmt->pptr			= pfmt->pptr + 1;

	return;

} /* process_bioz */

/*
 *	process_t()
 *
 *	Description:	This does semantic checking and generates text for
 *			the Tw, TLw and TRw edit-descriptors.
 *
 *	Called By:	process_paren_group
 *
 *	Calls:		nonzero_integer
 *
 *	Input parameters:
 *		pfmt		Parsing information block
 *
 *	Returns:	Nothing
 */

static void
process_t(
	parse_block	*pfmt
)
{
	long		size;
	register char	ch;

	size	= 0;

	GET(pfmt);

	ch	= toupper(pfmt->fmt_ch);

	if (ch == 'R') {

		GET(pfmt);

		if (nonzero_integer(pfmt, &size)) {
			pfmt->pptr->op_code	= TR_ED;
			pfmt->pptr->field_width	= size;
			pfmt->pptr		= pfmt->pptr + 1;
		}
	}
	else
		if (ch == 'L') {

			GET(pfmt);

			if (nonzero_integer(pfmt, &size)) {
				pfmt->pptr->op_code	= TL_ED;
				pfmt->pptr->field_width	= size;
				pfmt->pptr		= pfmt->pptr + 1;
			}
		}
		else
			if (nonzero_integer(pfmt, &size)) {
				pfmt->pptr->op_code	= T_ED;
				pfmt->pptr->field_width	= size;
				pfmt->pptr->rep_count	= 1;
				pfmt->pptr		= pfmt->pptr + 1;
			}

	return;

} /* process_t */

/*
 *	process_p()
 *
 *	Description:	This does semantic checking and generates text for
 *			the P edit-descriptor.  It also checks for the ANSI
 *			comma requirement.
 *
 *	Called By:	process_paren_group
 *			process_minus
 *
 *	Calls:		fmterr
 *
 *	Input parameters:
 *		pfmt		Parsing information block
 *		scale_factor	The P edit-descriptor scale factor.  Because
 *				the scale_factor is a signed quantity, it is
 *				stored in the rep_count field.
 *
 *	Returns:	Nothing
 */

static void
process_p(
	parse_block	*pfmt,
	long		scale_factor
)
{
	pfmt->pptr->op_code	= P_ED;
	pfmt->pptr->offset	= pfmt->fmt_pos;
	pfmt->pptr->rep_count	= scale_factor;
	pfmt->pptr		= pfmt->pptr + 1;

	GET(pfmt);

	switch (pfmt->fmt_ch) {
		case ',':
		case 'D':
		case 'E':
		case 'F':
		case 'G':
		case 'd':
		case 'e':
		case 'f':
		case 'g':
		case ')':
		case ':':
		case '/':
		case '\0':
			break;  /* Do Nothing */

		default:
			fmterr(pfmt, ANSI_COMMA_REQ, FALL, 0);
			break;
	} /* switch */

	return;

} /* process_p */

/*
 *	process_char_string()
 *
 *	Description:	This does semantic checking of and moves the
 *			character string to the intermediate text.
 *
 *	Called By:	process_paren_group
 *
 *	Calls:		fmterr
 *
 *	Input parameters:
 *		pfmt		Parsing information block
 *
 *	Returns:	Nothing
 */

static void
process_char_string(
	parse_block	*pfmt
)
{
	char		*str_ptr;
	register long	 size;

	size	= 0;
	str_ptr	= (char *) (pfmt->pptr + 1);

	for ( ; ; ) {

		if (++pfmt->fmt_pos > pfmt->fmt_len) {
			pfmt->fmt_pos	= pfmt->fmt_pos - 1;
			pfmt->fmt_ch	= '\0';
			fmterr(pfmt, NONTERMINATED_LITERAL, FALL, 0);
			break;
		}

		if (*(++pfmt->fmt_ptr) == pfmt->fmt_ch) {

			if (pfmt->fmt_pos == pfmt->fmt_len) {
				pfmt->fmt_ch	= '\0';
				break;
			}

			if (*(pfmt->fmt_ptr+1) != pfmt->fmt_ch) {
				GET(pfmt);
				break;
			}
			else {
				pfmt->fmt_pos	= pfmt->fmt_pos + 1;
				pfmt->fmt_ptr	= pfmt->fmt_ptr + 1;
			}
		}

		*str_ptr++	= *pfmt->fmt_ptr;
		size		= size + 1;
	} /* for */

	pfmt->pptr->op_code	= STRING_ED;
	pfmt->pptr->field_width	= size;
	pfmt->pptr		= pfmt->pptr +
		((size + FMT_ENTRY_BYTE_SIZE - 1) / FMT_ENTRY_BYTE_SIZE) + 1;

	return;

} /* process_char_string */

/*
 *	process_minus()
 *
 *	Description:	This does semantic checking for the use of the '-'
 *			in a format string.
 *
 *	Called By:	process_paren_group
 *
 *	Calls:		fmterr
 *			process_p
 *
 *	Input parameters:
 *		pfmt		Parsing information block
 *
 *	Returns:	FALSE if +/-P, because a comma is not required by
 *			the P edit-descriptor; TRUE otherwise, because a
 *			comma is required.
 */

static short
process_minus(
	parse_block	*pfmt
)
{
	register short	return_val;
	register long	col_m;
	register long	col_n;

	return_val	= TRUE;			/* Assume TRUE */
	col_m		= pfmt->fmt_pos;	/* Position of '-' */

	GET(pfmt);

	col_n		= pfmt->fmt_pos;	/* Position of token after '-' */

	if (IS_DIGIT(pfmt->fmt_ch)) {
		register int64	size;
		register char	ch;

		size	= 0;

		GETNUM(pfmt, size);

		ch	= toupper(pfmt->fmt_ch);

		if (ch == 'P') {

			pfmt->desc_col	= pfmt->fmt_pos;
			return_val	= FALSE;

			if (size > MAX_REP_COUNT) {
				fmterr(pfmt, FIELD_TOO_LARGE, FALL, col_n);
				size	= MAX_REP_COUNT;
			}

			process_p(pfmt, (long) -size);
		}
		else
			if (ch == 'X') {

				pfmt->desc_col	= pfmt->fmt_pos;

				fmterr(pfmt, MINUS_X_NON_ANSI, FALL, col_m);

				if (size == 0) {
					fmterr(pfmt, FIELD_WIDTH_ZERO, FALL,
						col_n);
					size	= 1;
				}
				else
					if (size > MAX_FIELD_WIDTH) {
						fmterr(pfmt, FIELD_TOO_LARGE,
							FALL, col_n);
						size	= MAX_FIELD_WIDTH;
					}

				pfmt->pptr->op_code	= TL_ED;
				pfmt->pptr->offset	= pfmt->fmt_pos;
				pfmt->pptr->field_width	= size;
				pfmt->pptr		= pfmt->pptr + 1;

				GET(pfmt);
			}
			else {
				fmterr(pfmt, EXPECTING_P_OR_X, FALL, col_n);
				recover(pfmt);
			}
	}
	else {
		fmterr(pfmt, EXPECTING_INTEGER, FALL, col_n);
		recover(pfmt);
	}

	return(return_val);

} /* process_minus */

/*
 *	process_paren_group()
 *
 *	Description:	This is a recursive routine that processes a
 *			parentheses group and all edit-descriptors in
 *			that group.  Whenever an open parenthesis is
 *			found (which is not immediately followed by a
 *			close parenthesis), this routine calls itself.
 *			It does semantic checking, ANSI checking, creates
 *			a parsed output that is directly usable by the
 *			run-time I/O libraries.  It also performs the
 *			following optimizations:
 *
 *			1) Discards empty parentheses groups,
 *			2) Combines edit-descriptors that are alike, such
 *			   as: X becomes TR, -X becomes TL, and Hollerith
 *			   and character strings become STRING,
 *			3) Changes things like  6(4a3) to 24a3.
 *
 *	Called By:	process_paren_group
 *			_parsfmt
 *
 *	Calls:		fmterr
 *			process_arl
 *			process_bioz
 *			process_char_string
 *			process_defg
 *			process_minus
 *			process_p
 *			process_t
 *
 *	Input parameters:
 *		pfmt	Parsing information block
 *		ploc	The location of the current open parenthesis
 *			in the parsed format.
 *
 *	Returns:	The number of edit-descriptors encountered.  If at
 *			least one of the edit-descriptors was a data edit-
 *			descriptor, then the number of edit-descriptors is
 *			negated.
 */

static short
process_paren_group(
	parse_block	*pfmt,
	fmt_type	*ploc
)
{
	register short	comma_req_flag;
	register short	data_ed;
	register short	found_rep_count;
	register short	outer_paren;
	register short	num_eds;
	register short	op_code;
	register short	temp;
	register long	num_start;
	register long	old_pos;
	register int64	repeat_count;
	register char	ch;
	char		*old_ptr;

	num_eds		= 0;
	data_ed		= FALSE;
	outer_paren	= (pfmt->pptr == ploc);

	do {	/* for each item in the parentheses group */

		num_start	= pfmt->fmt_pos;
		pfmt->desc_col	= pfmt->fmt_pos;
		comma_req_flag	= TRUE;
		num_eds		= num_eds + 1;	/* Assume an edit-descriptor */

		if (IS_DIGIT(pfmt->fmt_ch)) {
			register short	j, k;

			repeat_count	= 0;
			found_rep_count	= TRUE;

			GETNUM(pfmt, repeat_count);

			pfmt->desc_col	= pfmt->fmt_pos;

			/* Check if nonrepeatable edit-descriptor */

			j	= (((short) pfmt->fmt_ch) >> 6) & 1;
			k	= ((short) pfmt->fmt_ch) & 077;

			if ((non_repeatable[j] << k) < 0)
				fmterr(pfmt, INVALID_REP_COUNT, FALL, num_start);
			else {		/* 0P is valid */

				ch	= toupper(pfmt->fmt_ch);

				if (repeat_count == 0 && ch != 'P') {

					if (ch == 'H')
						fmterr(pfmt,
							ZERO_OR_NO_HOLLERITH_CNT,
							FALL, num_start);

					/*
					 * Do not issue message for B here
					 * because it may be a BN or BZ edit-
					 * descriptor.  The INVALID_REP_COUNT
					 * error will be issued, if necessary,
					 * when the B edit-descriptor is
					 * processed.
					 */

					else
						if (ch != 'B')
							fmterr(pfmt,
								ZERO_REP_COUNT,
								FALL, num_start);
				}

				/*
				 * Ensure that the repeat count hasn't overflowed.  We
				 * skip this check for the H, X and / edit-descriptors
				 * since they use the repeat count as the field width
				 * and will check it against a different limit.
				 */

				if (repeat_count > MAX_REP_COUNT)
					if (ch != 'X' && ch != 'H' && ch != '/') {
						fmterr(pfmt, FIELD_TOO_LARGE,
							FALL, num_start);
						repeat_count	= MAX_REP_COUNT;
					}
			} 
		}
		else {	/* not a digit */
			repeat_count	= 1;
			found_rep_count	= FALSE;
		}

		pfmt->pptr->offset	= pfmt->fmt_pos;
		pfmt->pptr->rep_count	= repeat_count;

		switch (toupper(pfmt->fmt_ch)) {

			case '(':	/* Start of parentheses group */ 

				num_eds			= num_eds - 1;
				pfmt->pptr->op_code	= REPEAT_OP;
				pfmt->pptr		= pfmt->pptr + 1;
				pfmt->depth		= pfmt->depth + 1;

				/*
				 * If level one parentheses group, then
				 * it's a possible reversion point.
				 */

				if (pfmt->depth == 1)
					data_ed	= FALSE;

				GET(pfmt);

				/*
				 * process_paren_group() is called
				 * recursively and returns the number
				 * of edit-descriptors found in the
				 * parentheses group.  The negative
				 * count of edit-descriptors is
				 * returned if at least one of them
				 * is a data edit-descriptor.
				 */

				temp	= process_paren_group(pfmt,
						pfmt->pptr - 1);

				/*
				 * Check if at least one data edit-
				 * descriptor was found.
				 */

				if (temp < 0) {
					data_ed	= TRUE;
					temp	= -temp;
				}

				num_eds	= num_eds + temp;
				break;

			case 'A':	/* A[w] data edit-descriptor */
				data_ed	= TRUE;
				process_arl(pfmt, A_ED);
				break;

			case 'D':	/* Dw.d[Ee] data edit-descriptor */
				data_ed	= TRUE;
				process_defg(pfmt, D_ED);
				break;

			case 'F':	/* Fw.d data edit-descriptor */
				data_ed	= TRUE;
				process_defg(pfmt, F_ED);
				break;

			case 'I':	/* Iw[.m] data edit-descriptor */
				data_ed	= TRUE;
				process_bioz(pfmt, I_ED);
				break;

			case 'X':	/* nX control edit-descriptor */

				if (!found_rep_count)
					fmterr(pfmt, NON_ANSI_EDIT_DESCRIPTOR,
						FALL, 0);
				else
					if (repeat_count > MAX_FIELD_WIDTH) {
						fmterr(pfmt, FIELD_TOO_LARGE,
							FALL, num_start);
						repeat_count	= MAX_FIELD_WIDTH;
					}

				pfmt->pptr->op_code	= TR_ED;
				pfmt->pptr->field_width	= repeat_count;
				pfmt->pptr->rep_count	= 1;
				pfmt->pptr		= pfmt->pptr + 1;

				GET(pfmt);
				break;

			case 'H':	/* nHc[c] string edit-descriptor */
				fmterr(pfmt, H_IS_OBSOLETE_IN_F90, F90, 0);
				fmterr(pfmt, NON_ANSI_EDIT_DESCRIPTOR, F95, 0);

				if (found_rep_count) {
					register int	left;

					if (repeat_count > MAX_FIELD_WIDTH) {
						fmterr(pfmt, FIELD_TOO_LARGE,
							FALL, num_start);
						repeat_count	= MAX_FIELD_WIDTH;
					}

					left	= pfmt->fmt_len - pfmt->fmt_pos;

					if (repeat_count > left)
						repeat_count	= (int64) left;

					pfmt->pptr->op_code	= STRING_ED;
					pfmt->pptr->field_width	= repeat_count;
					pfmt->pptr->rep_count	= 1;
					pfmt->pptr		= pfmt->pptr + 1;

					(void) strncpy((char *) pfmt->pptr,
						pfmt->fmt_ptr + 1, (int) repeat_count);

					pfmt->pptr	= pfmt->pptr + 1 +
						((repeat_count - 1) / FMT_ENTRY_BYTE_SIZE);
					pfmt->fmt_ptr	= pfmt->fmt_ptr + repeat_count;
					pfmt->fmt_pos	= pfmt->fmt_pos + repeat_count;

					GET(pfmt);

					if (pfmt->fmt_ch == '\0')
						fmterr(pfmt, NONTERMINATED_LITERAL,
							FALL, 0);
				}
				else {
					fmterr(pfmt, ZERO_OR_NO_HOLLERITH_CNT,
						FALL, num_start);
					recover(pfmt);
				}
				break;

			case '*':	/* *[c]* string edit-descriptor */
			case '"':	/* "[c]" string edit-descriptor */
				fmterr(pfmt, NON_ANSI_EDIT_DESCRIPTOR, FALL, 0);

			/* Break intentionally omitted - should fall through */

			case '\'':	/* '[c]' string edit-descriptor */
				process_char_string(pfmt);
				break;

			case 'G':	/* Gw.d[Ee] data edit-descriptor */
				data_ed	= TRUE;
				process_defg(pfmt, G_ED);
				break;

			case 'E':	/* Ew.d[Ee], ESw.d[Ee] or ENw.d[Ee]
						data edit-descriptors */
				data_ed	= TRUE;
				op_code	= E_ED;
				old_pos	= pfmt->fmt_pos;
				old_ptr	= pfmt->fmt_ptr;

				GET(pfmt);

				ch	= toupper(pfmt->fmt_ch);

				if (ch == 'N' || ch == 'S') {

					fmterr(pfmt, NON_ANSI_EDIT_DESCRIPTOR,
						F77, old_pos);

					op_code	= (ch == 'N') ? EN_ED : ES_ED;

				}
				else {	/* Back up */
					pfmt->fmt_pos	= old_pos;
					pfmt->fmt_ptr	= old_ptr;
				}

				process_defg(pfmt, op_code);
				break;

			case 'B':	/* Bw[.m] data edit-descriptor or BN or
						BZ control edit-descriptors */

				old_pos	= pfmt->fmt_pos;
				old_ptr	= pfmt->fmt_ptr;

				GET(pfmt);

				ch	= toupper(pfmt->fmt_ch);

				if (ch == 'N' || ch == 'Z') {

					if (found_rep_count)
						fmterr(pfmt, INVALID_REP_COUNT,
							FALL, num_start);

					pfmt->pptr->op_code	= (ch == 'N') ?
								BN_ED : BZ_ED;
					pfmt->pptr		= pfmt->pptr + 1;

					GET(pfmt);
				}
				else {
					if (repeat_count == 0)
						fmterr(pfmt, ZERO_REP_COUNT,
							FALL, num_start);

					/* Back up */

					pfmt->fmt_pos	= old_pos;
					pfmt->fmt_ptr	= old_ptr;

					fmterr(pfmt, NON_ANSI_EDIT_DESCRIPTOR,
						F77, 0);

					data_ed	= TRUE;
					process_bioz(pfmt, B_ED);
					break;
				}
				break;

			case 'R':	/* Rw data edit-descriptor */
				data_ed	= TRUE;
				fmterr(pfmt, NON_ANSI_EDIT_DESCRIPTOR, FALL, 0);
				process_arl(pfmt, R_ED);
				break;

			case 'L':	/* Lw data edit-descriptor */
				data_ed	= TRUE;
				process_arl(pfmt, L_ED);
				break;

			case 'P':	/* nP control edit-descriptor */
				if (!found_rep_count)
					fmterr(pfmt, EXPECTING_INTEGER, FALL,
							0);

				process_p(pfmt, (long) repeat_count);
				comma_req_flag	= FALSE;
				break;

			case 'O':	/* Ow[.m] data edit-descriptor */
				data_ed	= TRUE;
				fmterr(pfmt, NON_ANSI_EDIT_DESCRIPTOR, F77, 0);
				process_bioz(pfmt, O_ED);
				break;

			case 'Z':	/* Zw[.m] data edit-descriptor */
				data_ed	= TRUE;
				fmterr(pfmt, NON_ANSI_EDIT_DESCRIPTOR, F77, 0);
				process_bioz(pfmt, Z_ED);
				break;

			case '/':	/* [n]/ control edit-descriptor */
				if (found_rep_count) {

					if (repeat_count > MAX_FIELD_WIDTH) {
						fmterr(pfmt, FIELD_TOO_LARGE,
							FALL, num_start);
						repeat_count	= MAX_FIELD_WIDTH;
					}

					fmterr(pfmt, REP_SLASH_NON_ANSI,
						F77, num_start);
				}

				pfmt->pptr->op_code	= SLASH_ED;
				pfmt->pptr->field_width	= repeat_count;
				pfmt->pptr->rep_count	= 1;
				pfmt->pptr		= pfmt->pptr + 1;

				comma_req_flag		= FALSE;

				GET(pfmt);
				break;

			case '+': /* '+' valid only before P edit-descriptor */
				GET(pfmt);

				if (IS_DIGIT(pfmt->fmt_ch)) {
					register int64	size;

					size		= 0;
					num_start	= pfmt->fmt_pos;

					GETNUM(pfmt, size);

					if (toupper(pfmt->fmt_ch) == 'P') {

						pfmt->desc_col	= pfmt->fmt_pos;

						if (size > MAX_REP_COUNT) {
							fmterr(pfmt, FIELD_TOO_LARGE,
								FALL, num_start);
							size	= MAX_REP_COUNT;
						}

						process_p(pfmt, (long) size);

						comma_req_flag	= FALSE;
						break;	/*  Good exit */
					}

					fmterr(pfmt, EXPECTING_P_OR_X, FALL, 0);
				}
				else
					fmterr(pfmt, EXPECTING_INTEGER, FALL, 0);

				recover(pfmt);
				break;

			case '-':	/* '-' valid only before P or X edit-
						descriptors */
				comma_req_flag	= process_minus(pfmt);
				break;

			case ':':	/* : control edit-descriptor */
				pfmt->pptr->op_code	= COLON_ED;
				pfmt->pptr		= pfmt->pptr + 1;

				GET(pfmt);

				comma_req_flag		= FALSE;
				break;

			case 'Q':	/* Q control edit-descriptor */
				fmterr(pfmt, NON_ANSI_EDIT_DESCRIPTOR, FALL, 0);

				pfmt->pptr->op_code	= Q_ED;
				pfmt->pptr		= pfmt->pptr + 1;

				GET(pfmt);

				comma_req_flag		= FALSE;
				data_ed			= TRUE;	
				break;

			case '$':	/* $ control edit-descriptor */
			case '\\':	/* \ control edit-descriptor */
				fmterr(pfmt, NON_ANSI_EDIT_DESCRIPTOR, FALL, 0);

				pfmt->pptr->op_code	= DOLLAR_ED;
				pfmt->pptr		= pfmt->pptr + 1;

				GET(pfmt);

				comma_req_flag		= FALSE;
				break;

			case 'S': /* S, SP or SS control edit-descriptor */
				GET(pfmt);

				ch	= toupper(pfmt->fmt_ch);

				if (ch == 'S' || ch == 'P') {
					op_code	= (ch == 'S') ? SS_ED : SP_ED;
					GET(pfmt);
				}
				else
					op_code	= S_ED;

				pfmt->pptr->op_code	= op_code;
				pfmt->pptr		= pfmt->pptr + 1;
				break;

			case 'T': /* T, TL or TR control edit-descriptor */
				process_t(pfmt);
				break;

#ifndef	_OLD_ERROR_NUMBERS
			case ',': /* No edit descriptor, issue warning */
				fmterr(pfmt, NON_ANSI_NULL_DESCRIPTOR, FALL, 0);
				GET(pfmt);

				comma_req_flag		= FALSE;
				break;
#endif

			case ')':	/* End of parentheses group */
				num_eds	= num_eds - 1;

				if (num_eds == 0 && !outer_paren)
					fmterr(pfmt, ANSI_EMPTY_PAREN_MSG,
							FALL, 0);
				break;

			case '\0':	/* Oops, premature end of format */
				fmterr(pfmt, EXPECTING_RIGHT_PAREN, FALL, 0);
				return(0);

			default:	/* Unknown edit-descriptor */
				fmterr(pfmt, UNKNOWN_EDIT_DESCRIPTOR, FALL, 0);
				recover(pfmt);
				break;

		} /* switch */

		if (pfmt->fmt_ch == ',') {
			register long	col;

			col	= pfmt->fmt_pos;	/* Position of comma */

			GET(pfmt);

			if (pfmt->fmt_ch == ')') {
				pfmt->desc_col	= col;
				fmterr(pfmt, COMMA_NON_ANSI, FALL, col);
			}
		}
		else
			if (comma_req_flag)
				switch (pfmt->fmt_ch) {

					case ')':
					case ':':
					case '/':
					case '\0':
						break;

					default:
						fmterr(pfmt, ANSI_COMMA_REQ,
							FALL, 0);
						break;
				} /* switch */

		if (pfmt->fatal_err && pfmt->caller == LIB_CALL)
			return(0);

	} while (pfmt->fmt_ch != ')');

	if (outer_paren) {			/* End of format found */
		pfmt->pptr->op_code	= REVERT_OP;
		pfmt->pptr->rep_count	= pfmt->revert - pfmt->pptr;
		pfmt->pptr->offset	= pfmt->fmt_pos;
		pfmt->pptr->rgcdedf	= data_ed;
		pfmt->pptr		= pfmt->pptr + 1;
	}
	else {

	/*
	 * End of internal parentheses group.  Try to simplify the parsed
	 * format by attempting to coalesce entries.  Entries are coalesced
	 * if one of the following conditions applies:
	 *
	 * 1) Empty or redundant parentheses groups.  Constructs of the form:
	 *    '...()...' or '((...))' can be removed or simplified in the
	 *    parsed format.
	 *
	 * 2) Singly-repeated parentheses group.  Constructs of the form:
	 *    '(...)' or '1(...)' can be simplified to '...'.
	 *
	 * 3) Single edit-descriptor parentheses group.  Constructs of the
	 *    form:  '...n(e)...' or '...n(m(e))...', where 'e' is any single
	 *    edit-descriptor and 'n' and 'm' are repetition counts, can be
	 *    simplified to '...ne...' or '...n*me...', respectively.
	 *
	 *    a) If the single edit-descriptor is a P, BN, BZ, S, SP, SS, :
	 *	 or $ descriptor, the repeat count is ignored.  So a format
	 *	 of the form:  '...15(BZ)...' becomes '...BZ...'.
	 *
	 *    b) If the single edit-descriptor is a TL, TR, T or X edit-
	 *	 descriptor, the repeat count is folded into the count (width)
	 *	 field.  So a format of the form:  '...4(5X)...' becomes
	 *	 '...20X...'.
	 *
	 *    c) For all other edit-descriptors, the repeat count is folded
	 *	 into the existing repeat count.  So a format of the form:
	 *	 '...3(4F9.2)...' becomes '...12F9.2...'.
	 *
	 * Entries are NOT coalesced if their combined rep_count fields would
	 * exceed the maximum possible rep_count.  Note that any possible
	 * reversion point must be marked before a parentheses group is removed.
	 *
	 * Note that two adjacent identical edit descriptors which do not span
	 * a reversion point could be coalesced (e.g., 'I8,I8' to '2I8'); but
	 * in this case the offset field would be incorrect for some of the
	 * coalesced edit descriptors.  If a run-time error occurred while
	 * processing one of those edit descriptors the run-time diagnostic
	 * might point at the wrong edit descriptor.
	 */

		if (pfmt->depth == 1)
			pfmt->revert	= ploc;	/* Mark possible reversion point */

		if (ploc + 1 == pfmt->pptr && ploc->op_code == REPEAT_OP) {

			/* Clear empty parentheses group and remove it */

			pfmt->pptr	= pfmt->pptr - 1;

			(void) memset((void *) pfmt->pptr, 0, 2 * sizeof(fmt_type));

		}
		else {

			if ((num_eds == 1 ||	/* If one edit-descriptor or */
			    (ploc->op_code == REPEAT_OP &&
			     ploc->rep_count == 1) ) && /* Unary repeat count */
			    ploc->rep_count * (ploc+1)->rep_count <
				MAX_REP_COUNT) {

				unsigned int	size;
				fmt_type	*ppsp;

				/* Coalesce entries */

				pfmt->pptr	= pfmt->pptr - 1;
				ppsp		= ploc + 1;

				switch (ppsp->op_code) {

					case P_ED:
					case BN_ED:
					case BZ_ED:
					case COLON_ED:
					case S_ED:
					case SP_ED:
					case SS_ED:
					case T_ED:
					case DOLLAR_ED:

						/* Simply ignore rep_count */

						break;

					case SLASH_ED:
					case TL_ED:
					case TR_ED:

						/*
						 * Fold rep_count into width,
						 * if it'll fit.
						 */

						size	= ploc->rep_count *
							  ppsp->field_width;

						if (size < MAX_FIELD_WIDTH) {
							ppsp->field_width	=
								size;
							ppsp->rep_count	= 1;
						}
						else /* Fold into rep_count */
							ppsp->rep_count	=
								ppsp->rep_count *
								ploc->rep_count;

						break;

					default:

						/* Cascade rep_counts */

						ppsp->rep_count	=
							ppsp->rep_count *
							ploc->rep_count;
						break;

				} /* switch (ppsp->op_code) */

				/* Move entries */

				(void) memmove((void *) ploc, (void *) ppsp,
						(pfmt->pptr - ploc) * sizeof(fmt_type));

				/* Clear vacated entries */

				(void) memset((void *) pfmt->pptr, 0, 2 * sizeof(fmt_type));

			}
			else {
				pfmt->pptr->op_code	= ENDREP_OP;
				pfmt->pptr->rep_count	= ploc - pfmt->pptr;
				pfmt->pptr->offset	= pfmt->fmt_pos;
				pfmt->pptr		= pfmt->pptr + 1;

				if (pfmt->maxdepth < pfmt->depth)
					pfmt->maxdepth	= pfmt->depth;
			}
		}
	}

	pfmt->depth	= pfmt->depth - 1;

	GET(pfmt);

	if (data_ed)
		num_eds	= -num_eds;

	return(num_eds);

} /* process_paren_group */
