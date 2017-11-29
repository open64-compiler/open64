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



#pragma ident "@(#) libf/fio/lwrite.c	92.5	06/23/99 16:08:16"
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <fortran.h>
#include <cray/fmtconv.h>
#include <cray/nassert.h>
#ifdef	_CRAYT3D
#include <cray/mppsdd.h>
#define	MAXSH	512
#else
#define	MAXSH	1
#endif
#include "fio.h"
#include "fmt.h"
#include "f90io.h"
#include "lio.h"

short	_old_list_out_repcounts		= 0;
short	_90_char_nonchar_delim_blanks	= 1;
short	_blank_at_start_of_empty_rec	= 1;

extern oc_func	*_oldotab[DVTYPE_NTYPES];
extern oc_func	_sd2udee;

/*
 *	Forward reference
 */
int
_beautify(ftype_t type, long *plain, long *limit, long *pretty, short isf90);

int
_find_dupcnt(void *ptr, long count, long stride, int elsize, short ischar);

int
_write_delimited_char(FIOSPTR css, unit *cup, char *sptr, int len, long dchar);

/*
 *	COMPEQ compares the current output item with a saved output item.
 *	Evaluates to 1 if equal, 0 if not equal.
 */
#define COMPEQ(css, cptr, newtype, newelsize) ( \
	(css->u.fmt.u.le.type == newtype) && \
	(css->u.fmt.u.le.elsize == newelsize) && \
	(css->u.fmt.u.le.elsize > sizeof(css->u.fmt.u.le.u.value) ? \
		(memcmp(css->u.fmt.u.le.u.copy, cptr, newelsize) == 0) : \
		(memcmp(css->u.fmt.u.le.u.value, cptr, newelsize) == 0)))

#define WINT1	4
#define WINT2	6
#define WINT4	11
#define WDIG4	7
#define WDIG8	16
#define WDIG16	30
#define WRL4	15
#define WRL8	24
#define WRL16	41

/*
 *	_ld_write 
 *		List-directed write
 *
 *	Return Value:
 *		 0	normal return
 *		>0	error return
 */

int
_ld_write(
	FIOSPTR		css,	/* Pointer to current state		*/
	unit		*cup,	/* Unit pointer				*/
	void		*dptr,	/* Pointer to data			*/
	type_packet	*tip,	/* Type information packet		*/
	int		_Unused)/* Unused by this routine		*/
{
	register short	blanks;	/* Number of leading blanks		*/
	register short	ischar;	/* Is variable type CHARACTER?		*/
	register short	ndchar;	/* Local copy of css->u.fmt.u.le.ndchar	*/
	register ftype_t type;	/* Fortran data type			*/
	register int	elsize;	/* Size of each data time (bytes)	*/
	register int	i;	/* Loop index				*/
	register int	realsz;	/* size in bytes of the real data item	*/
	register int	repcnt;	/* Local copy of css->u.fmt.u.le.repcnt	*/
	register int	tbsz;	/* Number of characters in tbuf		*/
	register long	count;	/* Number of data items			*/
	register long	delim;	/* Delimiter, else 0 if none		*/
	register long	vinc;	/* Virtual stride			*/
	long		tbuf[ITEMBUFSIZ];
	long		plain[ITEMBUFSIZ]; /* buffer for numeric output	*/
	long		*tptr;	/* Pointer to location in tbuf		*/
	char		*cptr;	/* Character pointer to datum		*/
	const long	zero = 0;
#ifdef	_CRAYT3D
	register short	shared;	/* Is variable shared?			*/
	register int	elwords;/* Number of words per item		*/
	register int	offset;	/* Offset from address in item units	*/
	register int	tcount;	/* Number of items to move		*/
	long		shrd[MAXSH];	/* Shared data copy buffer	*/
#endif

	/* Assertions */

	assert ( cup != NULL );
	assert ( css != NULL );
	assert ( tip != NULL );

	cptr	= (char *) dptr;

	type	= tip->type90;
	count	= tip->count;
	elsize	= tip->elsize;
	vinc	= tip->stride;

	ischar	= (type == DVTYPE_ASCII) ? 1 : 0;

/*
 *	The ldwinit field is set to 1 the first time _ld_write is called
 *	during any WRITE statement.
 */
	if (css->u.fmt.u.le.ldwinit) {
		css->u.fmt.u.le.item1	= 1;
		css->u.fmt.u.le.repcnt	= 0;
		css->u.fmt.u.le.ndchar	= 0;
		css->u.fmt.u.le.ldwinit	= 0;
	}

	repcnt	= css->u.fmt.u.le.repcnt;
	ndchar	= css->u.fmt.u.le.ndchar;

/*
 *	Figure out if character variables would be delimited by a quote or
 *	a character.
 */
	delim	= 0;

	if (cup->udelim != OS_NONE)
		delim	= ((cup->udelim == OS_QUOTE) ? DQUOTE : SQUOTE);
	else if (css->f_iostmt == T_WNL && !cup->uft90)
		delim	= SQUOTE;

	if (cup->ulinemax > cup->uldwsize || cup->uldwsize <= 1)
		RERROR(FEWRLONG);	/* Record too long */

	if (count > 0 || repcnt > 0 || _blank_at_start_of_empty_rec) {
		if (cup->ulinemax == 0) {	/* If line empty */
			*(cup->ulineptr++)	= BLANK;
			cup->ulinemax		= cup->ulinemax + 1;
		}				
	}

/*
 *	If count is zero, then _ld_write is being called to terminate the
 *	current list-directed write statement processing.
 */
	if (count == 0) {
		if (repcnt > 0)
			goto print_saved_value;
		else
			goto fin;
	}

/*
 *	Special processing for T3D CRAFT shared variables.
 */

#ifdef	_CRAYT3D
	if (_issddptr(dptr)) {
		offset	= 0;
		elwords	= elsize / sizeof(long);
		tcount	= count;
		vinc	= 1;	/* We now have a unit stride */
		shared	= 1;
	}
	else
		shared	= 0;

   do {
	if (shared) {

		/* Copy data into local array shrd, and write from there */

		count	= MIN(MAXSH / elwords, (tcount - offset));
		_cpyfrmsdd(dptr, shrd, count, elwords, tip->stride, offset);
		offset	= offset + count;
		cptr	= (char *) shrd;
	}
#endif

	/*
	 *	M A I N   L O O P
	 */

	while (count > 0) {	/* While more to write */
		register int	dupcnt;	/* Number of duplicate data items */
		long		width;	/* Conversion field width */
		long		mode;	/* Conversion mode flags */
		long		digits;	/* Number of digits */
		long		expon;	/* Conversion exponent */
		long		scale;	/* Conversion scale factor */
		long		*newp;	/* Pointer to end of numeric conversion */
		oc_func	*gcf;	/* Generic NOCV-type conversion func */

		/*
		 * If there is a saved output value, and a nondelimited
		 * character is being printed or old output style is selected
		 * or the current output item is not equal to the stored
		 * output item, or the assign option to skip the
		 * repeat count is present, then print the stored
		 * output item.
		 */

		if (repcnt > 0 && ( _old_list_out_repcounts ||
		    !COMPEQ(css, cptr, type, elsize) ||
		    (cup->ufrptcnt !=0))) {
			register int	prevlen;
			register ftype_t prevtyp;
			char		*prevptr;
print_saved_value:

			prevlen	= css->u.fmt.u.le.elsize;
			prevtyp	= css->u.fmt.u.le.type;

			if (prevlen <= sizeof(css->u.fmt.u.le.u.value))
				prevptr	= (char *) &css->u.fmt.u.le.u.value[0];
			else
				prevptr	= (char *) css->u.fmt.u.le.u.copy;

			tptr	= tbuf;
			blanks	= 0;

			/*
			 * Now we handle the printing of a value separator
			 * between the last-printed value and the current
			 * value (possibly with a repeat count).  Value
			 * separators used are:
			 *
			 *  Adjacent types		  Value separator
			 *
			 *  nonchar, nonchar		  comma and 2 blanks
			 *  delim-char, delim-char	  comma and 2 blanks
			 *  delim-char, nonchar		  comma and 2 blanks
			 *  nondelim-char, nondelim-char  no delimiter
			 *  nondelim-char, other	  space (see note)
			 *
			 * Note: If we are in cf77 compatibility mode, no
			 * value separator is placed between a nondelimited 
			 * character and any other type.
			 *
			 * The comma separator is printed directly to the line
			 * buffer, but the blanks are deferred until we can
			 * determine if the next value fits on the current
			 * record or not.  If we must go to the next record,
			 * the blanks are not printed.
			 */

			if (css->u.fmt.u.le.item1 != 0)
				css->u.fmt.u.le.item1	= 0;

			/*
			 * No value separator between consecutive nondelimited 
			 * character.  One space is used as value separator.
			 */
			else if (ndchar && (prevtyp == DVTYPE_ASCII)) 
				blanks	= 0;
			/*
			 * Use one space as a value separator between 
			 * nondelimited character and non-character values.
			 */
			else if (ndchar || (delim == 0 && prevtyp == DVTYPE_ASCII)) {
				if (cup->ulinemax < cup->uldwsize && cup->uft90) 
					blanks	= _90_char_nonchar_delim_blanks;
				else
					blanks	= 0;
			}

			/*
			 * Else a comma and 2 blanks normally separate
			 * consecutive items.  If the previous output item 
			 * lies out at the very end of the record, we 
			 * suppress the comma.  The leading blank on 
			 * the next record serves as value separator.
			 */
			else {
				if (cup->ulinemax < cup->uldwsize) {
					if (cup->ufcomsep == 0) {
						*(cup->ulineptr++)	= COMMA;
						cup->ulinemax	= cup->ulinemax + 1;
						blanks		= 2;
					} else
						blanks		= 1;
				}
				else
					blanks		= 0;
			}

			/*
			 * Print the repeat count into the item buffer.
			 */

			if (repcnt > 1) {
				long	rcnt;

				rcnt	= repcnt;
				width	= WINT;
				digits	= 1;

				if (sizeof(rcnt) == 4)
					mode	= MODEHP;
#if     defined(_F_INT2) && (defined(__mips) || defined(_LITTLE_ENDIAN))
				else if (sizeof(rcnt) == 2)
					mode	= MODEWP;
				else if (sizeof(rcnt) == 1)
					mode	= MODEBP;
#endif	/* _f_int2 and mips or little endian */
				else
					mode	= 0;

				newp	= _s2ui(&rcnt, plain, &mode, &width,
						&digits, &zero, &zero);

				tptr	= tptr + _beautify(DVTYPE_INTEGER, plain,
							newp, tptr, cup->uft90);
				*tptr++	= STAR;
			}

			/*
			 * Print a saved (delimited) character value.
			 */

			if (prevtyp == DVTYPE_ASCII) {	/* delimited character */
				register int	errn;	/* Error code */

				/*
				 * Check that there's room on the current
				 * line for the blanks, the repeat count,
				 * the asterisk, and the starting delimiter.
				 */

				tbsz	= tptr - tbuf;

				if ((cup->ulinemax + blanks + tbsz + 1) >
				     cup->uldwsize) {

					/*
					 * Check that the repeat specification
					 * (with asterisk), the leading blank,
					 * and the leading delimiter fit on a
					 * single line.
					 */

					if ((tbsz + 2) > cup->uldwsize)
						RERROR(FEWRLONG); /* Record too long */

					/* Write record */

					errn	= (*css->u.fmt.endrec)(css, cup, 1);

					if (errn != 0)
						RERROR(errn);

					/* Write one space at start of new record */

					*(cup->ulineptr++)	= BLANK;
					cup->ulinemax		= cup->ulinemax + 1;
				}
				else {
					/* This loop should vectorize */
#ifdef	_CRAY
#pragma _CRI shortloop
#endif
					for (i = 0; i < blanks; i++)
						cup->ulineptr[i]	= BLANK;

					cup->ulinemax	= cup->ulinemax + blanks;
					cup->ulineptr	= cup->ulineptr + blanks;
				}

				/*
				 * Transfer the optional repeat count (with
				 * asterisk) and the starting delimiter to
				 * the line buffer.
				 */

				for (i = 0; i < tbsz; i++)	/* Should vectorize */
					cup->ulineptr[i]	= tbuf[i];

				cup->ulineptr	= cup->ulineptr + tbsz;
				cup->ulinemax	= cup->ulinemax + tbsz;

				errn	= _write_delimited_char(css, cup, prevptr,
						prevlen, delim);

				if (errn != 0)
					RERROR(errn);

				goto done_printing_saved_value;
			}

			/*
			 * Print a saved non-character value.
			 */

			gcf	= _oldotab[prevtyp];	/* Conversion function */
			mode	= 0;
			expon	= 0;
			scale	= 0;

			switch (prevtyp) {

			case DVTYPE_TYPELESS:
				switch (prevlen) {
				case 4:
					mode	= MODEUN | MODEHP;
					width	= WOCTHWD;
					break;

				case 8:
					mode	= MODEUN;
					width	= WOCTWRD;
					break;

				default:
					return(FEKNTSUP); /* kind not supported */
				}

				digits	= width;
				break;

			case DVTYPE_INTEGER:
				width	= WINT;	
				digits	= 1;

#ifdef	_F_INT4
				if (prevlen == 4) {
					mode	= MODEHP;
					if (cup->ufcomplen != 0)
						width	= WINT4;
#if     defined(_F_INT2) && (defined(__mips) || defined(_LITTLE_ENDIAN))
				} else if (prevlen == 2) {
					mode	= MODEWP;
					if (cup->ufcomplen != 0)
						width	= WINT2;
				} else if (prevlen == 1) {
					mode	= MODEBP;
					if (cup->ufcomplen != 0)
						width	= WINT1;
#endif	/* _F_INT2 and mips or little endian */
				}
#endif	/* _F_INT4 */
				break;

			case DVTYPE_REAL:
			case DVTYPE_COMPLEX:
				scale	= 1;
				realsz	= prevlen; /* bytes */

				if (prevtyp == DVTYPE_COMPLEX)
					realsz	= realsz >> 1;

				switch (realsz) {

					/* 
					 * Use G editing to print 'digits'
					 * of precision with G-as-F conversions
					 * and 'digits' + 1 precision with
					 * G-as-E conversions and a scale
					 * factor of 1.
					 *
					 * We put up with this inconsistency
					 * to avoid having to prescan the datum
					 * to determine its magnitude.
					 */
#ifdef	_F_REAL4
				case 4:
					mode	= MODEHP;

					/*
					 * if ignore-minus-flag of -0.0
					 * set, do not write minus.
					 */

					if (cup->ufnegzero != 0)
						mode	= mode | MODEMSN;

					expon	= DEXP4;

					if (cup->ufcomplen == 0) {
						width	= WREAL4;	
						digits	= _dreal4;
					} else {
						width	= WRL4;	
						digits	= WDIG4;
					}
					break;
#endif
				case 8:

					/* if ignore-minus-flag of -0.0
					 * set, do not write minus.
					 */

					if (cup->ufnegzero != 0)
						mode	= MODEMSN;

					expon	= DEXP8;

					if (cup->ufcomplen == 0) {
						width	= WREAL8;
						digits	= _dreal8;
					} else {
						width	= WRL8;	
						digits	= WDIG8;
					}
					break;

				case 16:
					/*
					 * When printing with D format,
					 * decrease the digits by one because
					 * we are setting the scale factor to
					 * 1.  This ensures that _dreal16
					 * digits of precision are printed.
					 */
					gcf	= _sd2udee;
					mode	= MODEDP;

					/* if ignore-minus-flag of -0.0
					 * set, do not write minus.
					 */

					if (cup->ufnegzero != 0)
						mode	= mode | MODEMSN;
					expon	= DEXP16;
					if (cup->ufcomplen == 0) {
						width	= WREAL16;	
						digits	= _dreal16 - 1;
					} else {
						width	= WRL16;	
						digits	= WDIG16;
					}
					break;

				default:
					return(FEKNTSUP); /* kind not supported */
				}
				break;
			} /* switch */

			/*
			 *	Perform the numeric output conversion.
			 */

			switch (prevtyp) {	/* set up for each data type */
				register long	ldatum;

			default:	/* Integer, Short Integer, Real, or Double */
	
				if (cup->ufcomplen == 0) {
					newp = gcf(prevptr, plain,
					   &mode, &width, &digits,
					   &expon, &scale);
					if (prevtyp == DVTYPE_TYPELESS)
						*newp++	= (int) 'B';
					tptr	= tptr + _beautify(prevtyp, plain,
						newp, tptr, cup->uft90);
				} else {
					newp = gcf(prevptr, tptr,
					   &mode, &width, &digits,
					   &expon, &scale);

					if (prevtyp == DVTYPE_TYPELESS)
						*newp++	= (int) 'B';
					tptr	= tptr + width;

					}
				break;	

			case DVTYPE_COMPLEX:
				*tptr++	= LPAREN;

				if (cup->ufcomplen == 0) {
					newp = gcf(prevptr, plain, &mode,
						&width, &digits, &expon,
						&scale);

					tptr = tptr + _beautify(prevtyp, plain,
						newp, tptr, cup->uft90);

					*tptr++	= COMMA;

					newp = gcf(((char *)prevptr + realsz),
						plain, &mode, &width, &digits,
						&expon, &scale);

					tptr = tptr + _beautify(prevtyp, plain,
						newp, tptr, cup->uft90);
				} else {
					newp = gcf(prevptr, tptr, &mode,
						&width, &digits, &expon,
						&scale);
					tptr	= tptr + width;
					*tptr++	= COMMA;
					newp = gcf(((char *)prevptr + realsz),
						tptr, &mode, &width, &digits,
						&expon, &scale);
					tptr	= tptr + width;
				}
				*tptr++	= RPAREN;

				break;

			case DVTYPE_LOGICAL:
				switch (prevlen) {

#ifdef	_F_LOG4
#if     defined(_F_LOG2) && (defined(__mips) || defined(_LITTLE_ENDIAN))
					case 1:
						ldatum	= *(_f_log1 *)prevptr;
						break;
					case 2:
						ldatum	= *(_f_log2 *)prevptr;
						break;
#endif	/* _F_LOG2 and mips or little endian */
					case 4:
						ldatum	= *(_f_log4 *)prevptr;
						break;
#endif
					case 8:
						ldatum	= *(_f_log8 *)prevptr;
						break;

					default:
						return(FEKNTSUP); /* kind not supported */
				}

				*tptr++ = _lvtob(ldatum) ? (long) 'T' : (long) 'F';
				break;

			} /* switch */

			tbsz	= tptr - tbuf;

			if ((cup->ulinemax + blanks + tbsz) > cup->uldwsize) {
				register int	errn;	/* Error code */

				/*
				 * Check that the item plus leading blank
				 * would fit on a single line.
				 */

				if (tbsz + 1 > cup->uldwsize)
					RERROR(FEWRLONG);	/* Record too long */

				/* Write record */

				errn	= (*css->u.fmt.endrec)(css, cup, 1);

				if (errn != 0)
					RERROR(errn);

				/* Leading blank */

				*(cup->ulineptr++)	= BLANK;
				cup->ulinemax		= cup->ulinemax + 1;
			}
			else {
				if ((cup->ulinemax + blanks) > cup->uldwsize)
					RERROR(FEWRLONG); /* Record too long */

#ifdef	_CRAY
#pragma _CRI shortloop
#endif
				for (i = 0; i < blanks; i++)
					cup->ulineptr[i]	= BLANK;

				cup->ulinemax	= cup->ulinemax + blanks;
				cup->ulineptr	= cup->ulineptr + blanks;
			}

			/*
			 * Now copy the current output from tbuf into the 
			 * line buffer.  We should never exceed the size
			 * of the item buffer since ITEMBUFSIZ is set up
			 * to be sufficiently large for all cases. 
			 */

			if (tbsz > ITEMBUFSIZ)
				_ferr(css, FEINTUNK);	/* deep weeds */

			for (i = 0; i < tbsz; i++)	/* Should vectorize */
				cup->ulineptr[i]	= tbuf[i];

			cup->ulineptr	= cup->ulineptr + tbsz;
			cup->ulinemax	= cup->ulinemax + tbsz;

done_printing_saved_value:
			if (prevlen > sizeof(css->u.fmt.u.le.u.value))
				free(css->u.fmt.u.le.u.copy);

			css->u.fmt.u.le.ndchar	= 0;	/* item was not nondelim char */
			css->u.fmt.u.le.repcnt	= 0;	/* value buffer is now empty */
			ndchar			= 0;
			repcnt			= 0;

		} /* End of saved output processing */

		/*
		 * If count is zero, we are completing list-directed output
		 * statement processing.  
		 */

		if (count == 0)
			goto fin;

		/*
		 * At this point, we are finally ready to handle the new 
		 * output value.  If it is nondelimited character, then we 
		 * print it right out.  Otherwise, we store the value in
		 * the css structure.
		 */

		if (ischar && delim == 0) {	/* If nondelimited character */
			register long	cnt;
			register long	stride;

			/*
			 * Take care of printing the possible value separator.
			 */

			blanks	= 0;

			if (css->u.fmt.u.le.item1)
				css->u.fmt.u.le.item1	= 0;
			else if (!ndchar && cup->uft90 &&
				  cup->ulinemax < cup->uldwsize) 
				blanks	= _90_char_nonchar_delim_blanks;

			/* blanks is always 0 or 1 here */

			assert ( blanks == 0 || blanks == 1 );

			if (blanks > 0) {
				*(cup->ulineptr++)	= BLANK;
				cup->ulinemax		= cup->ulinemax + 1;
			}

			/*
			 * Check for unit stride character data.  These can
			 * be coalesced and transferred as a single datum.
			 */

			cnt	= count;

			if (vinc == 0 || vinc == 1) {
				elsize	= elsize * cnt;
				cnt	= 1;
				vinc	= 1;
			}

			stride	= elsize * vinc;

			for (i = 0; i < cnt ; i++) {
				register int	j;

				for (j = 0; j < elsize; j++) {

					if (cup->ulinemax >= cup->uldwsize) {
						register int	errn;

						/* Write record */

						errn	= (*css->u.fmt.endrec)(css, cup, 1);

						if (errn != 0)
				        		RERROR(errn);

						/* Print blank in column 1 */

						*(cup->ulineptr++)	= BLANK;
						cup->ulinemax	= cup->ulinemax + 1;
					}

					*cup->ulineptr++	= (long) cptr[j];
					cup->ulinemax		= cup->ulinemax + 1;
				}

				cptr	= cptr + stride;
			}

			css->u.fmt.u.le.ndchar	= 1;	/* Set nondelim char */
			css->u.fmt.u.le.repcnt	= 0;	/* Value buffer is empty */

			goto fin;

		} /* End of nondelimited character processing */

		/*
		 * Find the number of consecutive duplicated values in the
		 * current batch of iolist items.
		 */

		if ((count > 1) && (cup->ufrptcnt == 0))
			dupcnt	= _find_dupcnt(cptr, count, vinc, elsize, ischar);
		else
			dupcnt	= 1;

	    	/*
		 * If repcnt is zero, then we save a new value in the css
		 * structure and set the repeat count appropriately.
		 *
		 * If repcnt is nonzero, then we know that the value pointed
		 * to by cptr is equal to that saved with the previous repeat
		 * count.   In this case we simply increase the repeat count
		 * to allow for current data.
		 */

		if (repcnt == 0) {
			void	*valptr;

			if (elsize > sizeof(css->u.fmt.u.le.u.value)) {

				valptr	= malloc(elsize);

				if (valptr == NULL) {
			        	RERROR(FENOMEMY);
				}

				css->u.fmt.u.le.u.copy	= valptr;
			}
			else
				valptr	= &css->u.fmt.u.le.u.value[0];

			/* Copy the possibly repeated value */

			if (ischar)	/* If character variable */
				(void) memcpy(valptr, cptr, elsize);
			else {
				/* On Mips systems, it's possible to have */
				/* elsize == sizeof(long), but the */
				/* item is not aligned on a long boundary */
				/* e.g., Complex data in the 64bit-ABI */
				if (elsize == sizeof(int))
					*(int *) valptr	= *(int *) cptr;
				else if (elsize == sizeof(short))
					*(short *) valptr	= *(short *) cptr;
				else
					(void) memcpy(valptr, cptr, elsize);
			}
		}

		repcnt			= repcnt + dupcnt;
		css->u.fmt.u.le.repcnt	= repcnt;
		css->u.fmt.u.le.type	= type;
		css->u.fmt.u.le.elsize	= elsize;

		/* Decrement count and advance data address */

done:
		count	= count - dupcnt;
		cptr	= cptr + (dupcnt * vinc * elsize);

	} /* while */

#ifdef	_CRAYT3D
	continue;
   } while (shared && offset < tcount);
#endif

fin:
	return(0);
}

/*
 *	_find_dupcnt
 *		Find and return the repeat count.  
 *
 *	Return Value
 *		The number of times the first datum is repeated
 * 		consecutively in the data list.  If the datum is
 *		not repeated, a value of one is returned.
 */ 

int
_find_dupcnt(
	void	*ptr,	/* Pointer to data		*/
	long	count,	/* Maximum number of data items	*/
	long	stride,	/* Stride between items (in units of elsize)	*/
	int	elsize,	/* Bytes per datum		*/
	short	ischar)	/* Is type == CHARACTER?	*/
{
	register long	i;

	/* Assertions */

	assert ( ptr != NULL );
	assert ( count > 1 );
	assert ( elsize > 0 );

	if (! ischar && elsize != sizeof(char)) {/* If not character or 1 byte*/
#if	(!defined(_WORD32) && ( defined(_F_INT4) || defined(_F_REAL4))) \
	|| defined(__mips) || defined(_LITTLE_ENDIAN)
		if (elsize == sizeof(short)) {
			register short	value;
			short		*sptr;

			sptr	= (short *) ptr;
			value	= *sptr;
 
                        for (i = 1; i < count; i++) {

				sptr	= sptr + stride;

				if (value != *sptr)
					break;
                        }
		}
		else
#endif	/* (not _word32 and (_f_int4 or _f_real4)) or mips or little endian */
		if (elsize == sizeof(int)){ 
			register int	value;
			int		*lptr;

			lptr	= (int *) ptr;
			value	= *lptr;

			for (i = 1; i < count; i++) {

				lptr	= lptr + stride;

				if (value != *lptr)
					break;
			}
		}
		else {				/* elsize > sizeof(int) */
			register int	words;
			register int	linc;
			int		*p1, *p2;

			words	= elsize / sizeof(int);
			linc	= stride * words;
			p1	= (int * ) ptr;
			p2	= p1 + linc;

			for (i = 1; i < count; i++) {
				register int	j;

#ifdef	_CRAY
#pragma _CRI shortloop
#endif
				for (j = 0; j < words; j++)
					if ((p1[j] != p2[j]))
						goto done;

				p2	= p2 + linc;
			}
		}
	}
	else {			/* Character */
		register long	cinc;
		char		*pchr;

		cinc	= elsize * stride;
		pchr	= ((char *) ptr) + cinc;

		for (i = 1; i < count; i++) {

			if (memcmp(ptr, pchr, elsize) != 0)
				break;

			pchr	= pchr + cinc;
		}
	}

done:
	return(i);
}

/*
 *	_beautify 
 *
 *		Beautify numeric output by deleting blanks and
 *		truncating unnecessary trailing zeroes.  The altered
 *		ascii number is placed in "pretty".
 *
 *		Input is in this form: {LH part}[{E}{exponent}]
 *
 *	Return value:
 *		The number of characters in the beautified output.
 */
int
_beautify(
	ftype_t	type,		/* Data type of the number */
	long	*plain,		/* Raw ascii representation of a number */
	long	*limit,		/* Pointer to one past end of ASCII data in plain */
	long	*pretty,	/* Receives the beautified output */
	short	isf90)		/* 1 iff Fortran 90 style printing of 0.E+0 */ 
{
	register short	i;
	register short	length;
	long		*p, *start, *exp, *end;

	/* Point start to the first nonblank character */

	start	= plain;

	while (*start == BLANK)
		start	= start + 1;

	/* Point end one past the last nonblank character */ 

	end	= limit;		/* find end point */

	while (*(end - 1) == BLANK)
		end	= end - 1;

	if (type == DVTYPE_TYPELESS || type == DVTYPE_INTEGER) {

		length	= end - start;

		/* The following loop should vectorize */

#ifdef	_MAXVL
		assert (length < 64);

#pragma _CRI shortloop
#endif
		for (i = 0; i < length; i++)
			pretty[i]	= start[i];

		return((int) length);
	}

/*
 *	Point exp to the 'E'.  Assign it NULL if there is not 'E' in the 
 *	number (integer or F format style).
 */
	exp	= NULL;

	for (p = end - 1; p > start; p--) {
		if (*p == (long) 'E') {
			exp	= p;
			break;
		}
	}

	if (exp != NULL) {	/* If E format output */
		long	*zero;

		zero	= exp;

		/*
		 * Point zero to one place past the last nonzero digit in 
		 * the LH part. 
		 */

		while ( *(zero - 1) == ZERO)
			zero	= zero - 1;		

		/*
		 * Copy 'E+(-)'.  Then zero is advanced to the future
		 * location of the exponent, and exp is pointed to the
		 * current location of the exponent.  
		 */

		*zero++	= *exp++;
		*zero++	= *exp++;

		/* 
		 * Remove all leading zeroes in the exponent.  Do not remove
		 * a zero exponent though.  Let the G editing output function
		 * called previously or the 0 beautification below be 
		 * responsible for choosing the F or E edit descriptor output 
		 * form.
		 */

		while (exp < (end - 1) && *exp == ZERO)  
			exp	= exp + 1;

		while (exp < end)
			*zero++	= *exp++;

		end	= zero;
	}
	else {			/* Else if F format output */
		while (*(end - 1) == ZERO)
			end	= end - 1;	/* trim trailing zeroes */
	}

	length	= end - start;

	/* The following loop should vectorize */

#ifdef	_MAXVL
		assert (length < 64);

#pragma _CRI shortloop
#endif
	for (i = 0; i < length; i++)
		pretty[i]	= start[i];

/*
 *	Floating point 0 values, both single and double precision, are
 *	printed as:
 *
 *		If in Fortran 90 mode:		 0.E+0
 *		If in CF77 mode:		 0.
 */

	if (pretty[0] == ZERO && pretty[1] == PERIOD &&
	    (length == 2 || (length > 2 && pretty[2] == (long) 'E'))) {

		length	= 2;

		if (isf90) {
			pretty[length++]	= (long) 'E';
			pretty[length++]	= PLUS;
			pretty[length++]	= ZERO;
		}
	}

	return (length);	/* return length of beautified output */
}

/*
 *	_write_delimited_char
 *		Write out to the line buffer a delimited character value
 *		with internal doubling of the delimiter characters.  If
 *		the value will not fit on the current line, spill to 
 *		subsequent lines as needed.  (On subsequent lines, no extra
 *		space character is is placed in column 1).
 *
 *	Return value:
 *
 *		 0 on success.  
 *		>0 error code if an error condition is encountered.
 */
int
_write_delimited_char(
	FIOSPTR	css,		/* Fortran statement state */
	unit	*cup,		/* Unit pointer */
	char	*sptr,		/* Pointer to string */
	int	len,		/* Length of string to be printed */
	long	dchar		/* delimiter character to use */
)
{
	register short	eoln;	/* End of line flag */
	register int	errn;	/* Error code */

/*
 *	Print out the opening delimiter.
 */
	if (cup->ulinemax >= cup->uldwsize) {

		errn	= (*css->u.fmt.endrec)(css, cup, 1);

		if (errn != 0)
			return(errn);

		if (css->f_iostmt == T_WNL && !cup->uft90) {
			cup->ulinemax           = cup->ulinemax + 1;
			*(cup->ulineptr++)      = BLANK;
		}
	}

	*(cup->ulineptr++)	= dchar;	
	cup->ulinemax		= cup->ulinemax + 1;
/*
 *	Print out the string.
 */
	eoln	= 0;

	while (len > 0) {
		if (eoln) {

			errn	= (*css->u.fmt.endrec)(css, cup, 1);

			if (errn != 0)
				return(errn);

			eoln	= 0;

			if (css->f_iostmt == T_WNL && !cup->uft90) {
				cup->ulinemax           = cup->ulinemax + 1;
				*(cup->ulineptr++)      = BLANK;
			}
		}

		if (*sptr == (char) dchar) {
			/*
			 * Next part of string is an imbedded delimiter 
			 * character.  Double the delimiter character.
			 */
			if ((cup->ulinemax + 2) > cup->uldwsize)
				eoln	= 1;
			else {
				*(cup->ulineptr++)	= dchar;	
				*(cup->ulineptr++)	= dchar;	
				cup->ulinemax		= cup->ulinemax + 2;
				len			= len - 1;
				sptr			= sptr + 1;
			}
		}
		else {
			/*
			 * Process a chunk of the string which contains no 
			 * imbedded delimiter characters and can fit on the 
			 * current line.
			 */

			if (cup->ulinemax >= cup->uldwsize)
				eoln	= 1;
			else {
				register int	chunk;
				char		*nxtdelm;

				chunk	= cup->uldwsize - cup->ulinemax;
				chunk	= (len < chunk) ? len : chunk;

				nxtdelm	= memchr(sptr, (int) dchar, chunk);

				if (nxtdelm != NULL)
					chunk	= nxtdelm - sptr;

				(void) _unpack(sptr, cup->ulineptr, chunk, -1);

				cup->ulinemax	= cup->ulinemax + chunk;
				cup->ulineptr	= cup->ulineptr + chunk;
				len		= len - chunk;
				sptr		= sptr + chunk;
			}
		}
	} /* while */

/*
 *	Print out the closing delimiter.
 */
	if (cup->ulinemax >= cup->uldwsize) {

		errn	= (*css->u.fmt.endrec)(css, cup, 1);

		if (errn != 0)
			return(errn);

		if (css->f_iostmt == T_WNL && !cup->uft90) {
			cup->ulinemax           = cup->ulinemax + 1;
			*(cup->ulineptr++)      = BLANK;
		}
	}

	*(cup->ulineptr++)	= dchar;	
	cup->ulinemax		= cup->ulinemax + 1;

	return(0);
}

/*
 *	_lwrite_setup
 *
 *		Access the LISTIO_PRECISION environment variable to choose 
 *		between styles of list directed output of reals.
 *
 *		FULL    - full precision output.   This ensures that all digits
 *			  with any precision (and possibly some with no
 *		  	  precision) are printed.  This is default.
 *		PRECISION - the number of digits printed is P or P+1,
 *			  depending on whether the library chooses
 *			  F or E format.  P is the value of the 
 *			  F90 PRECISION() intrinsic function.
 *		YMP80     - the number of digits printed is compatible with
 *			  UNICOS 8.0 and previous.   This is available only
 *			  on YMP/C90/TS systems.
 *		F77	  - the number of digits printed is mostly compatible 
 *			  with that chosen by the Sparc f77 compiler.  This
 *			  is available only on Sparc, and only for internal
 *			  use.
 *
 *
 *		This function is called by _initialize_fortran_io()
 */
void
_lwrite_setup(void)        
{
	char	*str;

#ifdef	_F_REAL4
	_dreal4		= DREAL4;
#endif

	_dreal8		= DREAL8;
	_dreal16	= DREAL16;

/*
 *	The LISTIO_PRECISION environment variable can be set to specify
 *	a choice in the number of digits of precision requested for real
 *	values being printed via list directed output.
 */
	str	= getenv("LISTIO_PRECISION");

	if (str != NULL) {
		if (strcmp(str, "FULL") == 0) {
			_dreal8		= DREAL8;
			_dreal16	= DREAL16;
		}
		else if (strcmp(str, "PRECISION") == 0) {
#ifdef	_F_REAL4
			_dreal4		= DREAL4_P;
#endif
			_dreal8		= DREAL8_P;
			_dreal16	= DREAL16_P;
		}
		else if (strcmp(str, "YMP80") == 0) {
			_dreal8		= DREAL8_YMP80;
			_dreal16	= DREAL16_YMP80;
		}
		else if (strcmp(str, "F77") == 0) {	/* internal use only */
#ifdef	_F_REAL4
			_dreal4		= 6;
#endif
			_dreal8		= 14;
		}
	}

/*
 *	The LISTIO_OUTPUT_STYLE environment variable can be set to 'OLD' 
 *	to cause list-directed output to be more consistent with CrayLibs 1.2.  
 *	Specifically, this will cause the following effects:
 *
 * 		1) Repeat counts will not span separate calls to _ld_write().
 *		2) A space will not be printed as a value separator between
 *		   non-delimited character and non-character output list items
 *		   in F90 mode (if cup->uft90==1).
 *		3) A space will not be printed in column 1 of a list-directed
 *		   WRITE statement containing no I/O list items.
 *
 */
	str	= getenv("LISTIO_OUTPUT_STYLE");

	if (str != NULL && strcmp(str, "OLD") == 0) {
		_old_list_out_repcounts		= 1;
		_90_char_nonchar_delim_blanks	= 0;
		_blank_at_start_of_empty_rec	= 0;
	}

	return;
}
