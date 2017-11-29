/*
 * Copyright 2002, 2003, 2004 PathScale, Inc.  All Rights Reserved.
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



#pragma ident "@(#) libf/fio/lread.c	92.3	06/18/99 15:49:57"

#include <limits.h>
#include <ctype.h>
#ifndef	_ABSOFT
#include <malloc.h>
#else
#include <stdlib.h>
#endif
#include <string.h>
#include <fortran.h>
#include <cray/fmtconv.h>
#include <cray/nassert.h>
#ifdef	_CRAYT3D
#include <cray/mppsdd.h>
#define MAXSH	512
#else
#define MAXSH	1
#endif
#include "fio.h"
#include "lio.h"
#include "f90io.h"

/*
 *	16-byte real is not currently supported on MPP
 */

#if	defined(_CRAYMPP) || (defined(_ABSOFT) && defined(_LD64))
#if	defined _F_REAL16 && _F_REAL16 == (-1)
#define	FAKE_REAL16
#endif
#endif

/* External functions */

extern int
_nicverr(const int _Nicverror);

extern void
_set_stride(void *dest, void *src, long count, int elsize, long inc);

/*
 *	This table is used to drive input conversion based on the type of the
 *	data.
 */
extern 
#ifndef KEY /* this can cause wrong func being called when compiled by gcc */
const 
#endif
ic_func	*_ilditab[DVTYPE_NTYPES];

/*
 *	_gen_real is a float (REAL) variable type of largest supported kind.
 *
 */

#if	!defined(_F_REAL16) || defined(FAKE_REAL16)
typedef _f_real8	_gen_real;
#else
typedef _f_real16	_gen_real;
#endif

/* 
 *	The repdata structure is used by scanning routines to manage repeated
 *	list-directed input data.
 */
struct repdata {

	long	repcnt;		/* The remaining repeat count */

	enum reptypes {

		REPNONE = 0,	/* Indicate no leftover repeat value */
		REPLINE,	/* Get input value from current line
				 * buffer; the value does NOT span lines. */
		REPCHAR,	/* Get character value from packed buffer */
		REPCPLX,	/* Complex value is repeated */
		REPNULL		/* Null value is repeated */

	} reptype;	/* type of repeated data */

	union {

		struct { /* for REPLINE */
			long	*lptr;	/* Pointer to input field */
			int	lcnt;	/* Characters left in record */
		} line;

		struct { /* for REPCHAR */
			char	*repchr;/* Pointer to buffer containing a 
					 * packed copy of a repeated input 
					 * quoted character string; NULL
					 * otherwise. */
			long	repsize;/* Number of characters in repeated
					 * value */
		} rchr;

		struct { /* for REPCPLX */
			_gen_real r[2];	/* Complex value */
		} cplx;
	} u;
};


/* Forward references for local functions */

void
_cmplx_convert(void *dest, int size, _gen_real src[2]);

long
_get_repcount(long *ptr, int limit, long *width);

int
_get_value( long *lptr, int lcnt, void *ptr, ftype_t type, int elsize,
	long *width);

int
_mr_scan_char(FIOSPTR css, unit *cup, char *ptr, int elsize,
	char **chptr, long *slen);

int
_mr_scan_complex(FIOSPTR css, unit *cup, void *cpxptr, int elsize,
	short is_mult);

int
_s_scan_extensions(void *ptr, ftype_t type, int elsize, long *begin,
	int left, long *size, long cmode);

/*
 *	Macros
 */

/*
 *	GENREALTO8 converts a _gen_real to a _f_real8.
 */

#ifdef	FAKE_REAL16
#define	GENREALTO8(x)	(*x)

#elif	!defined(_UNICOS)
#define	GENREALTO8(x)	((_f_real8)(*x))	/* cast to _f_real8 */

#else
#define SNGLR _SNGLR_

#endif

#ifdef	SNGLR
#define GENREALTO8	SNGLR
extern _f_real SNGLR(_f_real16 *);		
#endif

/*
 *	GENREALTO4 converts a _gen_real to a _f_real4.
 */

#ifdef	_F_REAL4
#define	GENREALTO4(x)	((_f_real4)(*x))	/* cast to _f_real4 */
#endif

/*
 *	ADVANCE_INPUT advances the file until it finds a non-whitespace
 *			character.
 */

#define	ADVANCE_INPUT(css, cup, lptr, lcnt)	\
	for (;;) {				\
		while (lcnt == 0) { /* Find a non-empty line */		\
			errn	= css->u.fmt.endrec(css, cup, 1);	\
			if (errn != 0) {				\
				if (errn > 0) RERROR(errn);		\
				if (errn < 0) REND(errn);		\
			}						\
			lptr	= cup->ulineptr;			\
			lcnt	= cup->ulinecnt;			\
		}							\
		if (! IS_WHITESPACE(*lptr))				\
			break;	/* Eureka! */				\
		lptr	= lptr + 1;					\
		lcnt	= lcnt - 1;					\
	}

/*
 *	_ld_read - read list formatted input.
 *
 *	return value:
 *		<0	end-of-file return
 *		 0	normal return
 *		>0	error return
 *	     abort	if error or end-of-file condition and user has not
 *			specified IOSTAT=/ERR=/END=
 */

int
_ld_read(
	FIOSPTR		css,	/* Current Fortran I/O statement state */
	unit		*cup,	/* Unit pointer */
	void		*dptr,	/* Pointer to start of destination data area */
	type_packet	*tip,	/* Type information packet */
	int		_Unused)/* Unused by this routine */
{
	register short	reptype;/* Local copy of cup->urepdata->reptype	*/
	register ftype_t type;	/* Fortran data type			*/
	register int	elsize;	/* Size of each data item (bytes)	*/
	register int	errn;	/* Error code				*/
	register int	lcnt;	/* Local copy of cup->ulinecnt		*/
	register long	count;	/* Number of data items			*/
	register long	repcnt;	/* Local copy of cup->urepdata->repcnt	*/
	register long	stride;	/* Stride between data items (bytes)	*/
	register long	vinc;	/* Virtual stride			*/
	long		*lptr;	/* Local copy of cup->ulineptr		*/
	char		*cptr;	/* Character pointer to datum		*/
	struct repdata	*rptr;	/* Local copy of cup->urepdata		*/
#ifdef	_CRAYT3D
	register short	shared;	/* Is variable shared?			*/
	register int	elwords;/* Number of words per data item	*/
	register int	offset;	/* Offset from address in item units	*/
	register int	tcount;	/* Number of items to move		*/
	long		shrd[MAXSH];	/* Shared data temp array	*/
#endif

	/* Assertions */

	assert ( css != NULL );
	assert ( cup != NULL );
	assert ( dptr != NULL );
	assert ( tip != NULL );

	cptr	= (char *) dptr;
	errn	= 0;

	lcnt	= cup->ulinecnt;
	lptr	= cup->ulineptr;

	type	= tip->type90;
	count	= tip->count;
	elsize	= tip->elsize;
	vinc	= tip->stride;

/*
 *	u.fmt.lcomma is 0 only if this is the first _ld_read call for the
 *	current list-directed READ statement.  Use this clue to be sure
 *	any old unexhausted repeat count is zeroed.
 */

	rptr	= cup->urepdata;

	if (css->u.fmt.lcomma == 0 && rptr != NULL)
		rptr->repcnt	= 0;

	if (rptr != NULL && rptr->repcnt != 0) {

		/*
		 * An unexhausted repeat count exists from a previous
		 * iteration or call to _ld_read.
		 */

		reptype	= rptr->reptype;
		repcnt	= rptr->repcnt;

		assert ( reptype == REPNONE || reptype == REPLINE ||
			 reptype == REPCHAR || reptype == REPCPLX ||
			 reptype == REPNULL );
		assert ( repcnt > 0 );
	}
	else {
		reptype	= REPNONE;	/* Indicate no leftover repeat count */
		repcnt	= 1;
	}

#ifdef	_CRAYT3D
	if (_issddptr(dptr)) {
		offset	= 0;
		elwords	= elsize / sizeof(long);
		tcount	= count;
		vinc	= 1;	/* We now have a unit stride */	
		shared	= 1;
		css->f_shrdput	= 1;
	}
	else
		shared	= 0;

   do	{
	if (shared) {	/* shared variable */
		/* we read the data into local array shrd */
		/* and later distribute it to shared memory */
		count	= MIN(MAXSH / elwords, (tcount - offset));
		cptr	= (char *) shrd;
	}
#endif

	stride	= elsize * vinc;

	/*
	 *	M A I N   L O O P
	 */

	while (count > 0) {	/* While more to read */
		register short	is_mult;/* Can complex scan advance? */
		register short	is_null;/* Is value a null value? */
		register long	nitems;	/* Number of repeated data items */
		long		width;	/* Field width of data */

		if (css->u.fmt.slash)	/* If we've encountered a slash */
			break;

		is_null	= 0;	/* Assume a non-null value */
		is_mult	= 1;	/* Complex scan may process multiple records */

		/*
		 * If there is no outstanding repeat count, we must scan
		 * ahead, past a possible new repeat count, to the first
		 * character of the input data.
		 */

		if (reptype == REPNONE) {

			/*
			 * Read until we find a record containing a non-blank
			 * character.
			 */

advance:
			ADVANCE_INPUT(css, cup, lptr, lcnt);

			/*
			 * If css->u.fmt.lcomma == 1 then the next 
			 * comma would not imply a null value.
			 */

			if (*lptr == COMMA && css->u.fmt.lcomma == 1) {
				css->u.fmt.lcomma	= 0;
				lptr			= lptr + 1;
				lcnt			= lcnt - 1;
				goto advance;
			}

			css->u.fmt.lcomma	= 1;
			repcnt			= 1;

			if (*lptr == SLASH) {
				css->u.fmt.slash	= 1;
				goto done;
			}

			/* Check for a possible repeat count in the input */

			if (IS_DIGIT(*lptr)) {

				repcnt	= _get_repcount(lptr, lcnt, &width);

				lcnt	= lcnt - width;
				lptr	= lptr + width;
			}
		}
		else if (reptype == REPLINE) {

			/*
			 * Reposition at the location of the repeated
			 * data item.  Then redo the usual input scan.
			 */

			lptr	= rptr->u.line.lptr;
			lcnt	= rptr->u.line.lcnt;

			/* Complex scan may not advance records */

			is_mult	= 0;
		}

		/*
		 * Scan the data at the current position in the current
		 * record.  We do this if there is no outstanding repeat
		 * count, or we are rescanning at the current record
		 * position to satisfy an outstanding repeat count of
		 * type REPLINE.
		 */

		if (reptype == REPNONE || reptype == REPLINE) {

			/* Check for a null value */

			if (lcnt == 0 || IS_SEPARATOR(*lptr))
				is_null	= 1;

			if (repcnt <= count || reptype == REPLINE) {
				/*
				 * Handle the case where this call to _ld_read
				 * consumes all of the (optionally repeated)
				 * data.
				 */

				if (is_null)
					errn	= 0;	/* Do nothing */
				else switch (type) {

				default:
					errn	= _get_value(
							lptr,
							lcnt,
							cptr,
							type,
							elsize,
							&width);

					lcnt	= lcnt - width;
					lptr	= lptr + width;
					break;

				case DVTYPE_COMPLEX:
					cup->ulinecnt	= lcnt;
					cup->ulineptr	= lptr;

					errn	= _mr_scan_complex(
							css,
							cup,
							cptr,
							elsize,
							is_mult);

					lcnt	= cup->ulinecnt;
					lptr	= cup->ulineptr;
					break;

				case DVTYPE_ASCII:
					cup->ulinecnt	= lcnt;
					cup->ulineptr	= lptr;

					errn	= _mr_scan_char(
							css,
							cup,
							cptr,
							elsize,
							NULL,
							NULL);

					lcnt	= cup->ulinecnt;
					lptr	= cup->ulineptr;
					break;

				} /* switch */

				if (errn != 0)	/* If EOF or error */
					goto done;
			}

			/*
			 * Else the repeat count exceeds the number of I/O
			 * list items, so create the repdata data structure.
			 * At the same time, read the data into the next I/O
			 * list item.
			 */

			else {
				if (rptr == NULL) {

					rptr	= (struct repdata *)
						  malloc(sizeof(struct repdata));

					if (rptr == NULL) {
						errn	= FENOMEMY;
						goto done;
					}

					cup->urepdata	= rptr;
				}

				if (is_null) {
					errn	= 0;
					reptype	= REPNULL;
				}
				else switch (type) {

				default:
					errn	= _get_value(
							lptr,
							lcnt,
							cptr,
							type,
							elsize,
							&width);

					reptype			= REPLINE;
					rptr->u.line.lcnt	= lcnt;
					rptr->u.line.lptr	= lptr;
					lcnt			= lcnt - width;
					lptr			= lptr + width;
					break;

				case DVTYPE_COMPLEX:
					reptype		= REPCPLX;
					cup->ulinecnt	= lcnt;
					cup->ulineptr	= lptr;

					errn	= _mr_scan_complex(
						css,
						cup,
						&rptr->u.cplx,
						sizeof(rptr->u.cplx),
						is_mult);

					lcnt	= cup->ulinecnt;
					lptr	= cup->ulineptr;

					_cmplx_convert(
						cptr,
						elsize,
						rptr->u.cplx.r);
					break;

				case DVTYPE_ASCII:
					rptr->u.rchr.repchr	= NULL;
					cup->ulinecnt		= lcnt;
					cup->ulineptr		= lptr;

					errn	= _mr_scan_char(
						css,
						cup,
						cptr,
						elsize,
						&rptr->u.rchr.repchr,
						&rptr->u.rchr.repsize);

					if (rptr->u.rchr.repchr != NULL)
						reptype			= REPCHAR;
					else {
						reptype			= REPLINE;
						rptr->u.line.lptr	= lptr;
						rptr->u.line.lcnt	= lcnt;
					}

					lcnt	= cup->ulinecnt;
					lptr	= cup->ulineptr;
					break;

				} /* switch */

				if (errn != 0)	/* If EOR or error */
					goto done;
			}
		}

		/*
 		 * Else satisfy the first I/O list item from the leftover
		 * repeat count from a previous call to _ld_read.
		 */

		else {
			if (reptype == REPNULL) {
				errn	= 0;
				is_null	= 1;
			}
			else switch (type) {

			case DVTYPE_COMPLEX:

				if (reptype != REPCPLX)
					errn	= FELDNOCX;
				else
					_cmplx_convert(
						cptr,
						elsize,
						rptr->u.cplx.r);
				break;

			case DVTYPE_ASCII:
				if (reptype != REPCHAR)
					errn	= FELDUNKI;
				else {
					register int	xfersz;

					xfersz	= MIN(elsize,
						rptr->u.rchr.repsize);

					if (xfersz > 0)
						(void) memcpy(
							cptr,
							rptr->u.rchr.repchr,
							xfersz);

					if (xfersz < elsize)
						(void) memset(
							cptr + xfersz,
							BLANK,
							elsize - xfersz);
				}
				break;

			default:
				errn	= FELDUNKI;	/* Deep weeds */
				break;

			} /* switch */

			if (errn != 0)	/* If EOR or error */
				goto done;
		}

		/*
		 * Repeat count processing is now wrapped up by distributing
		 * copies of the first I/O list item to the rest of the
		 * items.
		 */

		nitems	= MIN(repcnt, count);

		if (nitems > 1 && is_null == 0)
			_set_stride(cptr + stride, cptr, nitems - 1,
					elsize, stride);

		cptr	= cptr + (nitems * stride);
		count	= count - nitems;
		repcnt	= repcnt - nitems;

		if (repcnt == 0) {	/* If repeat count exhausted */

			if (reptype == REPCHAR)
				free(rptr->u.rchr.repchr);

			reptype = REPNONE;
		}
	} /* while */

done:
#ifdef	_CRAYT3D
	if (shared && (long *)cptr != shrd) {
		register int	items;

		/* Move the data to shared memory */

		items	= ((long *) cptr - shrd) / elwords;

		_cpytosdd(dptr, shrd, items, elwords, tip->stride, offset);

		offset	= offset + items;
	}

	if (css->u.fmt.slash)
		break;

   } while (errn == 0 && shared && offset < tcount);
#endif

	/*
	 * Update fields in unit table.
	 */

	cup->ulinecnt	= lcnt;
	cup->ulineptr	= lptr;

	if (rptr != NULL) {	/* If we have a repdata structure */

		if (repcnt == 0) {	/* If repcnt exhausted */

			if (reptype == REPCHAR)
				free(rptr->u.rchr.repchr);

			reptype = REPNONE;
		}

		rptr->repcnt	= repcnt;
		rptr->reptype	= (enum reptypes) reptype;
	}

	if (errn > 0)
		RERROR(errn);

	return(errn);
}

/*
 *	_get_repcount - scan text for a positive integer repeat count followed
 *			by an asterisk.
 *
 *	Return value:
 *		Repeat count (1 if no count found)
 *		Line position updated if repeat count found.
 */
 
long
_get_repcount(
	long	*ptr,	/* Pointer into current record buffer */
	int	limit,	/* Number of characters left in current record */
	long	*width)	/* Number of characters consumed by repeat count */
{
	register int	nchars;	/* Number of characters processed */
	register long	chr;	/* Current character */
	register long	count;	/* Repeat count */

	chr	= *ptr++;
	count	= 0;
	nchars	= 0;

	while (limit > 1 && IS_DIGIT(chr)) {
		count	= (count + count + (count << 3)) + (chr - ZERO);
		chr	= *ptr++;
		nchars	= nchars + 1;
		limit	= limit - 1;
	}

	/*
	 * If the repeat count is zero or not found, set the repeat count
	 * to 1 but do not update the line position.
	 */

	if (chr != STAR || count == 0) {	/* If no repeat count or zero */
		count	= 1;		/* Update line position */
		nchars	= 0;
	}
	else
		nchars	= nchars + 1;	/* Count the asterisk */

	*width	= nchars;

	return(count);	/* Return repeat count */
}

/*
 *	_get_value	- Read a real, integer, or logical value.
 *
 *	Return value:
 *		 0 on success
 *		>0 error code on error
 */

int
_get_value(
	long	*lptr,	/* Pointer to the unpacked text */
	int	lcnt,	/* Number of characters available to scan */
	void	*ptr,	/* Pointer to I/O list item */
	ftype_t	type,	/* Fortran data type */
	int	elsize,	/* Size in bytes of the I/O list item */
	long	*size)	/* Field width (output) */
{
	register int	errn;
	register int	nc;
	long		dummy;
	long		cmode;
	long 		zero = 0;
	long		width;
	long		*begin;
	long		*end;
#ifndef KEY /* this can cause wrong func being called when compiled by gcc */
	const 
#endif
	    ic_func 	*ngcf;

	begin	= lptr;			/* Mark start of field */
	ngcf	= _ilditab[type];	/* Conversion function */
	*size	= 0;
	nc	= 0;
	cmode	= 0;

	/* Find the trailing value separator */

	while ( nc < lcnt && !IS_DELIMITER(*lptr) ) {
		lptr	= lptr + 1;
		nc	= nc + 1;
	}

	end	= lptr;
	width	= nc;

	/* Set up cmode */

	switch (type) {

	case DVTYPE_REAL:

		switch (elsize) {

#ifdef	_F_REAL4
		case 4:
			cmode	= MODEHP;
			break;
#endif
		case 8:
			break;

		case 16:
			cmode	= MODEDP;
			break;

		default:
			return(FEKNTSUP);	/* kind not supported */
		}
		break;

	case DVTYPE_INTEGER:
	case DVTYPE_LOGICAL:

		switch (elsize) {

#if	(defined(_F_INT2) || defined(_F_LOG2)) && (defined(__mips) || \
	defined(_LITTLE_ENDIAN))
		case 1:		
			cmode	= MODEBP;
			break;
		case 2:		
			cmode	= MODEWP;
			break;
#endif
#if	defined(_F_INT4) || defined(_F_LOG4)
		case 4:		
			cmode	= MODEHP;
			break;
#endif
		case 8:
			break;

		default:
			return(FEKNTSUP);	/* kind not supported */
		}
		break;

	default:
		return(FEKNTSUP);	/* kind not supported */
	}

	/* Call the conversion function */

	errn	= ngcf(	begin, &width, &end, &cmode, ptr, &dummy,
			&zero, &zero);

	if (errn < 0)
		errn	= _nicverr(errn); 
	else
		errn	= 0;

/*
 *	If the scan failed, the input data might be hollerith or hex or
 *	octal.  Allow _s_scan_extensions to rescan the input and recompute
 *	the field width.
 */

	if (errn == FENICVIC || errn == FERDIVLG) {
		register int	errn2;

		errn2	= _s_scan_extensions(
			ptr,
			type,
			elsize,
			begin,
			lcnt,
			size,
			cmode);

		if (errn2 >= 0)
			errn	= errn2;
	}
	else
		*size	= end - begin;

	return(errn);
}

/*
 *	_s_scan_extensions - read a Cray extension format into an I/O list item.
 *
 *	Input forms accepted ('Y' yes or '-' no):
 * 
 *	Data types
 *	I R L C 	Format		Description
 *	- - - -		------		-----------
 *
 *	Y Y Y Y		(o0)'nnn[']	Octal bit pattern
 *	Y Y Y Y		(zZ)'nnn[']	Hexadecimal bit pattern
 *	Y Y - Y		nnn(bB)		Octal integer (may be converted to real)
 *	Y Y Y Y		("')xxx("')[hH]	Blank-filled Hollerith character data
 *	Y Y Y Y		("')xxx("')(lL)	Zero-filled character data
 *	Y Y Y Y		("')xxx("')(rR)	Right-justified character data
 *
 *	Input forms accepted for data item sizes:
 * 
 *	Size (words)
 *	1 2+		Format
 *	- - 		------	
 *
 *	Y - 		(o0)'nnn[']
 *	Y - 		(zZ)'nnn[']
 *	Y Y 		nnn(bB)	(but the integer value must fit in one word)
 *	Y - 		'xxxx'[hH]
 *	Y - 		'xxxx'(lL)
 *	Y - 		'xxxx'(rR)
 *
 *	Return value:
 *		 0 on success
 *		>0 error code
 *		-1 use previously assigned error code
 */
int
_s_scan_extensions(
	void	*ptr,		/* Pointer to user I/O list item */
	ftype_t	type,		/* Fortran data type */
	int	elsize,		/* Size in bytes of datum */
	long	*begin,		/* Pointer to start of input field */
	int	left,		/* Number of characters left in record */
	long	*size,		/* Field width (output) */
	long	cmode)		/* Mode from calling routine */
{
	register short	nchars;
	register int	errn;
	register int	i;
	register int	lcnt;
	register long	delim;
	long		dummy;
	long		fw;
	long		zero = 0;
	register char	first;
	register char	ht;
	_f_int8		intvalue;
	char		cbuf[sizeof(_f_int8)];
	long		*endptr;
	long		*lptr;
	void		*vptr;
	ic_func		*ncf;	/* Numeric conversion function */

	*size	= 0;
	errn	= 0;
	lptr	= begin;
	lcnt	= left;
	first	= (char) *lptr;

	switch (first) {

	case 'b': 
	case 'B': 			/* Binary, F90 only */
		if (first == 'b' || first == 'B')
			return (FELDUNKI);
		break;

	case 'o': 
	case 'O': 			/* Octal */
	case 'z': 
	case 'Z': 			/* Hexadecimal */

		if (lcnt < 3 || lptr[1] != SQUOTE)
			return(-1);

		lptr	= lptr + 2;		/* advance past the [oOzZ]' */
		lcnt	= lcnt - 2;

		for (i = 0; i < lcnt; i++) {
			if (IS_DELIMITER(lptr[i]))
				break;
		}

		if (lptr[i - 1] == SQUOTE)
			i	= i - 1;	/* Exclude trailing ' */

		if (i <= 0) 
			return (-1);		/* No sequence of digits found */

		if (first == 'b' || first == 'B')
			return (FELDUNKI);

		if (first == 'o' || first == 'O')
			ncf	= _ou2s;
		else	/* Assume hexadecimal */
			ncf	= _zu2s;

		endptr	= lptr + i;
		fw	= i;

		errn	= ncf(lptr, &fw, &endptr, &cmode, ptr, &dummy,
				&zero, &zero);

		if (errn < 0) {
			register int estat;
			estat	= _nicverr(errn);
			if (estat > 0)
				return(estat);
		}

		lptr	= lptr + fw;
		lcnt	= lcnt - fw;

		if (lcnt > 0 && *lptr == SQUOTE) { /* consume trailing ' */
			lptr	= lptr + 1;
			lcnt	= lcnt - 1;
		}

		break;

	case '\'':
	case '"':			/* Hollerith */
		delim	= (long) first;
		nchars	= 0; 

		for (;;) {
			lptr	= lptr + 1;
			lcnt	= lcnt - 1;

			if (lcnt == 0)
				return(-1);

			if (*lptr == delim) {
				lptr	= lptr + 1;
				lcnt	= lcnt - 1;

				if (lcnt == 0 || *lptr != delim)
					break;		/* loop exit */ 
			}

			if ((nchars >= sizeof(_f_int8)) || 
				(nchars >= elsize))
				return(FELDSTRL);	/* too long for 1 word*/

			cbuf[nchars]	= (char) *lptr;
			nchars		= nchars + 1;
		}

		if (lcnt == 0)
			ht	= 'h';
		else if (IS_SEPARATOR(*lptr))
			ht	= 'h';
		else {
			switch (*lptr) {
				case 'h':
				case 'H':
					ht	= 'h';
					break;

				case 'l':
				case 'L':
					ht	= 'l';
					break;

				case 'r':
				case 'R':
					ht	= 'r';
					break;

				default:
					return(FELDUNKI);
			}

			lptr	= lptr + 1;
		}

		/* pad with nulls */

		switch (elsize) {
#ifdef	_F_REAL4
			case 4:
				*(_f_int4 *)ptr = 0;
				break;
#endif
			case 8:
				*(_f_int8 *)ptr = 0;
				break;
#if	(defined(_F_INT2) || defined(_F_LOG2)) && (defined(__mips) || \
	defined(_LITTLE_ENDIAN))
			case 2:
				*(_f_int2 *)ptr = 0;
				break;
			case 1:
				*((char *)ptr) = '\0';
				break;
#endif
		}	

		if (nchars > 0) {

			if (ht == 'r'){			/* right justify */
				memcpy((char *)ptr+elsize-nchars, cbuf, nchars);
			}
			else
				(void) memcpy(ptr, cbuf, nchars);
		}

		if (ht == 'h' && nchars != sizeof(long)) {
			register int	pad;

			pad	= elsize - nchars;

			(void) memset((char *)ptr + nchars, BLANK, pad);
		}

		break;

	default:			/* Must be the nnnnnB form */
		for (i = 0; i < lcnt; i++) {
			if (IS_DELIMITER(lptr[i]))
				break;
		}

		i	= i - 1;		/* exclude 'b'/'B' */

		if (i == 0) 
			return (-1);		/* no digits prior to 'b'/'B' */

		if (lptr[i] != 'B' && lptr[i] != 'b')
			return (-1);		/* not terminated by 'b'/'B' */

		vptr	= &intvalue;
		endptr	= lptr + i;
		fw	= i;

		errn	= _ou2s(lptr, &fw, &endptr, &cmode, vptr, &dummy,
				&zero, &zero);

		if (errn < 0) {
			register int estat;
			estat	= _nicverr(errn);
			if (estat > 0)
				return(estat);
		}

		/*
		 * Unlike the z'nn and o'nn forms, nnB is converted to
		 * floating point for REAL input list items.
		 */

		if (type == DVTYPE_REAL) {
			switch (elsize) {
#ifdef	_F_REAL4
			case 4:
				*(_f_real4 *)ptr	= (_f_real4)intvalue;
				break;		
#endif
			case 8:
				*(_f_real8 *)ptr	= (_f_real8)intvalue;
				break;

#if	defined(_F_REAL16) && !defined(FAKE_REAL16)
			case 16:
				*(_f_real16 *)ptr	= (_f_real16)intvalue;
				break;
#endif
			default:
				return (FEKNTSUP);
			}
		}
		else {
			switch (elsize) {
#if	(defined(_F_INT2) || defined(_F_LOG2)) && (defined(__mips) || \
	defined(_LITTLE_ENDIAN))
			case 2:
				*(_f_int2 *)ptr	= (_f_int2)intvalue;
				break;
			case 1:	
				*(_f_int1 *)ptr	= (_f_int1)intvalue;
				break;
#endif
#ifdef	_F_INT4
			case 4:
				*(_f_int4 *)ptr	= (_f_int4)intvalue;
				break;
#endif

#ifdef	_F_INT8
			case 8:
				*(_f_int8 *)ptr	= intvalue;
				break;
#endif
			default:
				return (FEKNTSUP);
			}
		}
		
		lptr	= lptr + fw + 1;	/* Advance past nnnnB */

	}

	*size	= lptr - begin;

	return(0);
}

/*
 *	_mr_scan_complex	Read a complex value starting from the current
 *			position in the current record.  If is_mult is set,
 *			then scanning may continue into subsequent records.
 *
 *	Return value:
 *		 0 on success.
 *		>0 on error.
 *		<0 on end-of-file.
 *	     abort if error or end-of-file condition and user has not
 *		   specified IOSTAT=/ERR=/END=
 */

int
_mr_scan_complex(
	FIOSPTR	css,		/* Fortran statement state */
	unit	*cup,		/* unit pointer */
	void	*cpxptr,	/* pointer to the complex input list item */
	int	elsize,		/* size in bytes of each input list item */
	short	is_mult)	/* 1 if we may advance to the next record */
{
	register int	errn;
	register int	lcnt;
	long		fw;
	long		*lptr;

	lcnt	= cup->ulinecnt;
	lptr	= cup->ulineptr;

	if (*lptr != LPAREN) {	/* If no opening parenthesis */
		errn	= FELDNOCX;
		goto done;
	}

	lptr	= lptr + 1;
	lcnt	= lcnt - 1;

	/* Advance to the start of the numeric field for the real part */

	while (lcnt > 0 && IS_WHITESPACE(*lptr)) {
		lptr	= lptr + 1;
		lcnt	= lcnt - 1;
	}

	if (lcnt == 0) {
		errn	= FELDNOCX;
		goto done;
	}

	elsize	= elsize >> 1;		/* Size of each complex part */

	errn	= _get_value(lptr, lcnt, cpxptr, DVTYPE_REAL, elsize, &fw);

	if (errn != 0)
		goto done;

	lptr	= lptr + fw;
	lcnt	= lcnt - fw;

	/* Now advance to the comma */

	while (lcnt > 0 && IS_WHITESPACE(*lptr)) {
		lptr	= lptr + 1;
		lcnt	= lcnt - 1;
	}

	if (lcnt == 0) {	/* If at end of line */

		if (is_mult == 0) {
			errn	= FELDNOCX;
			goto done;
		}

		ADVANCE_INPUT(css, cup, lptr, lcnt);
	}

	if (*lptr != COMMA) {	/* If no comma between real and imaginary parts */
		errn	= FELDNOCX;
		goto done;
	}

	lptr	= lptr + 1;
	lcnt	= lcnt - 1;

	/* Advance to the start of the numeric field for the imaginary part */

	while (lcnt > 0 && IS_WHITESPACE(*lptr)) {
		lptr	= lptr + 1;
		lcnt	= lcnt - 1;
	}

	if (lcnt == 0) {	/* If at end of line */
		ADVANCE_INPUT(css, cup, lptr, lcnt);
	}

/*
 *	Scan the imaginary part.
 */
	cpxptr	= (char *) cpxptr + elsize;

	errn	= _get_value(lptr, lcnt, cpxptr, DVTYPE_REAL, elsize, &fw);

	if (errn != 0)
		goto done;

	lptr	= lptr + fw;
	lcnt	= lcnt - fw;

	/* Advance past the trailing parenthesis */

	while (lcnt > 0 && *lptr != RPAREN) {
		lptr	= lptr + 1;
		lcnt	= lcnt - 1;
	}

	if (lcnt == 0) {	/* Didn't find closing parenthesis! */
		errn	= FELDNOCX;
		goto done;
	}

	cup->ulineptr	= lptr + 1;
	cup->ulinecnt	= lcnt - 1;

done:
	if (errn > 0)
		RERROR(errn);

	return(0);
}

/*
 *	_mr_scan_char - read a character value.
 *
 *	This routine reads delimited or undelimited character strings for
 *	list-directed input.  Scanning starts from the current position in the
 *	current record.  If the string is delimitted by quotes or characters,
 *	additional records are read when necessary to reach the trailing 
 *	delimiter.
 *
 *	The character string is transferred to the I/O list item pointed
 *	to by ptr.  The I/O list item is properly padded with blanks if
 *	the string is shorter than the I/O list item.  If the I/O list item
 *	is shorter than the input string, the whole string is scanned 
 *	anyway with extra characters being discarded.
 *
 *	Return value:
 *		 0 on success.
 *		>0 on error.
 *		<0 on end-of-file.
 *	     abort if error or end-of-file condition and user has not
 *		   specified IOSTAT=/ERR=/END=
 */
int
_mr_scan_char(
	FIOSPTR	css,		/* Fortran statement state */
	unit	*cup,		/* unit pointer */
	char	*ptr,		/* pointer to the character input list item */
	int	elsize,		/* size in bytes of each input list item */
	char	**chptr,	/* (input) chptr is non-null if a copy of 
				 * multi-record strings should be saved.
				 * (output) *chptr is assigned NULL if the
				 * string didn't span records or string is
				 * of zero length.  Assigned a pointer to an 
				 * allocated buffer containing a copy of the 
				 * string. */
	long	*slen)		/* (output) size of string saved at *chptr */
{
	register short	span;	/* Input spanned records?		*/
	register int	errn;	/* Error code				*/
	register int	lcnt;	/* Local copy of cup->ulinecnt		*/
	register long	chlen;	/* Length of the character string	*/
	register long	delim;	/* Character string delimiter		*/
	register long	lsave;	/* Length of character save buffer	*/
	long		*lptr;	/* Local copy of cup->ulineptr		*/
	char		*csave;	/* Character save buffer		*/

	span	= 0;
	chlen	= 0;
	lsave	= 0;
	csave	= NULL;
	lptr	= cup->ulineptr;
	lcnt	= cup->ulinecnt;
	delim	= *lptr;	/* Possible delimiter */

	if (IS_STRING_DELIMITER(delim)) {	/* If quoted character */

		for (;;) {

			lptr	= lptr + 1;
			lcnt	= lcnt - 1;

			/* Advance to a nonempty record */

			while (lcnt == 0) {
				span	= 1; 

				errn	= css->u.fmt.endrec(css, cup, 1);

				if (errn != 0) 
					goto err_end_return;

				lptr	= cup->ulineptr;
				lcnt	= cup->ulinecnt;
			}

			if (*lptr == delim) {

				if (lcnt > 1 && *(lptr + 1) == delim) {
					lptr	= lptr + 1;
					lcnt	= lcnt - 1;
				}
				else
					break;		/* loop exit */ 
			}

			if (chlen < elsize)
				ptr[chlen]	= (char) *lptr;

			if (chptr != NULL) {	/* If saving input */

				if (csave == NULL) {
					lsave	= RECMAX;
					csave	= (char *) malloc(lsave);

					if (csave == NULL) {
						errn	= FENOMEMY;
						goto err_end_return;
					}
				}
				else {
					if (chlen > lsave) {
						lsave	= lsave + RECMAX;
						csave	= (char *) realloc(csave, lsave);

						if (csave == NULL) {
							errn	= FENOMEMY;
							goto err_end_return;
						}
					}
				}

				csave[chlen]	= (char) *lptr;
			}

			chlen	= chlen + 1;
		} /* for */

		lptr	= lptr + 1;	/* advance past trailing delimiter */
		lcnt	= lcnt - 1;

		if (span == 0) {	/* input didn't span records */
			if (csave != NULL)
				free(csave);	/* don't need it */
		}
		else {			/* input spanned records */
			if (chptr != NULL) {	/* If saving input */
				*chptr	= csave;	/* Character save buffer */
				*slen	= chlen;	/* Set length */
			}
		}
	}
	else {	/* Unquoted character string */
		while ( lcnt > 0 && !IS_SEPARATOR(*lptr) ) {

			if (chlen < elsize)
				ptr[chlen]	= (char) *lptr;

			chlen	= chlen + 1;
			lptr	= lptr + 1;
			lcnt	= lcnt - 1;
		}
	}

	/* If input shorter than variable, pad with blanks */

	if (chlen < elsize)
		(void) memset(ptr + chlen, BLANK, elsize - chlen);

	cup->ulineptr	= lptr;
	cup->ulinecnt	= lcnt;

	return(0);	/* normal return */

err_end_return:
	if (csave != NULL)
		free(csave);

	if (errn < 0) {
		REND(errn);
	}
	else if (errn > 0) {
		RERROR(errn);
	}
	else
		_ferr(css, FEINTUNK);

	return(0);		/* MIPS compiler needs a return statement */
}

_PRAGMA_INLINE(_cmplx_convert)
void
_cmplx_convert(
	void		*dest,
	int		size,
	_gen_real	src[2])
{
	/* Assertions */

	assert ( size <= (sizeof(_gen_real) << 1) );

	switch (size) {	/* case for each supported complex kind */ 

#ifdef	_F_COMP4
	case ( 2 * 4 ):					/* KIND=4 */
		((_f_real4 *)dest)[0]	= GENREALTO4(&src[0]);
		((_f_real4 *)dest)[1]	= GENREALTO4(&src[1]);
		break;
#endif

	case ( 2 * 8 ):					/* KIND=8 */
		((_f_real8 *)dest)[0]	= GENREALTO8(&src[0]);
		((_f_real8 *)dest)[1]	= GENREALTO8(&src[1]);
		break;

#ifdef	_F_COMP16
	case ( 2 * 16 ):				/* KIND=16 */
		((_f_real16 *)dest)[0]	= src[0];
		((_f_real16 *)dest)[1]	= src[1];
		break;
#endif

	default:
		assert ( 0 );	/* shouldn't happen */
	} /* switch */

	return;
}
