/*
 * Copyright 2002, 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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



#pragma ident "@(#) libf/fio/rdfmt.c	92.6	06/21/99 10:37:55"

#include <memory.h>
#include <stdlib.h>
#include <string.h>
#include <fortran.h>
#include <cray/fmtconv.h>
#include <cray/format.h>
#include <cray/nassert.h>
#ifdef	_CRAYT3D
#include <cray/mppsdd.h>
#define	MAXSH		512
#else
#define	MAXSH		1
#endif
#include "fio.h"
#include "fmt.h"
#include "f90io.h"
#include "lio.h"

extern	
#ifndef KEY /* this can cause wrong func being called when compiled by gcc */
const 
#endif
ic_func	*_iconvtab[LAST_DATA_ED + 1];
extern	const short	_idedtab[DVTYPE_NTYPES];

/*
 *	_rdfmt()	Read format processing
 *
 *		css	Current Fortran I/O statement state pointer
 *		cup	Unit pointer
 *		dptr	Pointer to data 
 *		tip	Type information packet
 *
 *	Return value
 *
 *		0		normal return.
 *
 *		FEEORCND	if end of record condition and IOSTAT= or  
 *				EOR= is specified.
 *
 *		other <0	if end of file condition and IOSTAT= or  
 *				END= is specified.
 *
 *		>0		if error condition and IOSTAT= or ERR= is
 *				specified.
 */
int
_rdfmt(
	FIOSPTR		css,	/* Current Fortran I/O statement state */
	unit		*cup,	/* Unit pointer */
	void		*dptr,	/* Pointer to data */
	type_packet	*tip,	/* Type information packet */
	int		_Unused	/* Unused by this routine */
)
{
	register short	cswitch;	/* 1 if complex data; else zero */
	register short	fmtop;		/* Current format operator */
	register short	part;		/* Part of datum (complex is 2-part) */
	register ftype_t type;		/* Fortran data type */
	register int32	chxfer;		/* Chars xferred by data edit descriptors */
	register int32	delta;		/* Length/field width difference */
	register int32	field;		/* Consecutive conversion field size */
	register int32	i;		/* Scratch loop variable */
	register int32	itemch;		/* Number of chars available for item */
	register int32	kount;		/* Number of consecutive conversions */
	register int32	length;		/* Length of datum in bytes */
	register int32	padcnt;		/* Number of pad bytes at end of item */
	register int32	repcnt;		/* Copy of *css->u.fmt.u.fe.pftocs */
	int		cinc[2];	/* Increments for datum parts */
	register int	stat;		/* Error code */
	register int	stride;		/* Stride between data in bytes */
	register char	*cptr;		/* Character pointer to datum */
	register char	*ctmp;		/* Temporary character pointer */
	long		digits;		/* Digits field of edit-descriptor */
	long		mode;		/* Mode word for conversion */
	long		*tptr;		/* Temporary line buffer pointer */
	long		width;		/* Width field of edit-descriptor */
	register long	count;		/* Number of data items */
	register long	dfmode;		/* MODEBZ or MODEBN mode bits */
	fmt_type	pfmt;		/* Current parsed format entry */
#ifdef	_CRAYT3D
	register short	shared;		/* Is variable shared? */
	register int	elwords;	/* Number of words per item */
	register int	offset;		/* Offset from address in item units */
	register int32	tcount;		/* Number of items to move */
	long		shrd[MAXSH];	/* Buffer for shared data */
#endif

	int	_nicverr(	/* Map NICV-type errors to Fortran errors */
			const int _Nicverror);

#ifndef KEY /* this can cause wrong func being called when compiled by gcc */
	const 
#endif
	    ic_func	*ngcf;		/* Generic NICV-type conversion func */

	/* If these assertions are not all true, then we're in deep doo-doo. */

	assert (cup != NULL);
	assert (tip != NULL);

	type	= tip->type90;
	count	= tip->count;

	chxfer	= 0;
	cswitch	= 0;
	stat	= 0;
	part	= 1;

	pfmt	= *css->u.fmt.u.fe.pfcp;
	repcnt	= *css->u.fmt.u.fe.pftocs;
	length	= tip->elsize;
	stride	= tip->stride * length;
	cinc[1]	= stride;

	/* If COMPLEX data type, set data length and increments */

	if (type == DVTYPE_COMPLEX) {
		length	= length / 2;
		cinc[0]	= length;
		cinc[1]	= stride - length;
		cswitch	= 1;
		part	= 0;
	}

	dfmode	= ((css->u.fmt.blank0 == 1) ? MODEBZ : MODEBN);

#ifdef	_CRAYT3D
	if (_issddptr(dptr)) {	/* shared variable */
		offset	= 0;
		elwords	= tip->elsize / sizeof(long);
		shared	= 1;
		stride	= tip->elsize;	
		tcount	= count;
		css->f_shrdput	= 1;
	}
	else
		shared	= 0;

   do	{
	if (shared) {	/* shared variable */
		/*
		 * We read the data into local array shrd and later 
		 * distribute it to shared memory.  We assume for now that
		 * shared data never has a container size smaller than a word.
		 */
		count	= MIN (MAXSH / elwords, (tcount - offset));	
		cptr	= (char *) shrd;
	}
	else 
#endif
	{
		cptr	= (char *) dptr;
	}

	do {	/*  M A I N   L O O P  */

		fmtop	= pfmt.op_code;		/* Get operator */
		width	= pfmt.field_width;	/* And main parameter */
		digits	= pfmt.digits_field;	/* And secondary parameter */

		/* Basic sanity check on the parsed format */

		if (fmtop > LAST_OP || fmtop < FIRST_DATA_ED) {
			stat	= FEINTIPF;	/* Invalid parsed format */
			goto done;
		}

		if (fmtop <= LAST_DATA_ED) {

			/*
			 * We have a data edit-descriptor and if the count
			 * is exhausted, then we're done for now.
			 */

			if (count == 0)
				goto done;

			/*
			 * Validate the data edit-descriptor against the
			 * data type and do the Fortran 90 mapping of the
			 * G data edit-descriptor.
			 */

			if (INVALID_RTYPE(fmtop, type)) {
				stat	= FERDTYPE; /* Type mismatch */
				goto done;
			}

			if (fmtop == G_ED) {

				fmtop	= _idedtab[type];

				if (type != DVTYPE_REAL &&
				    type != DVTYPE_COMPLEX)
					digits	= 1;
			}

			if (type == DVTYPE_ASCII) 
				mode	= 0;
			else {
				mode	= (long) _rd_ilchk[fmtop-1][length-1];

				if (mode == INVALID_INTLEN) {
					stat	= FERDTYPE; /* Type mismatch */
					goto done;
				}
			}

			mode	= mode | dfmode;

			/*
			 * Handle zero-width formats.
			 */

			if (width == 0) {
				register int	exp;

				switch (fmtop) {

				/*
				 * For character (A/R) data edit-
				 * descriptors, the width is the
				 * length of the datum.
				 */
				case A_ED:
				case R_ED:
					width	= length;
					break;

				/*
				 * For integer (B/I/O/Z) data edit-descriptors,
				 * the width is the maximum number of "digits"
				 * plus one for a leading blank and (I only)
				 * one for an optional sign.
				 */
				case B_ED:
				case I_ED:
				case O_ED:
				case Z_ED:
					width	= _rw_mxdgt[fmtop-1][length-1];

					/* Fix limitation in table */

					if (width == 127)
						width	= 128;

					/* Allow for blank and sign */

					width	= width + 1;

					if (fmtop == I_ED)
						width	= width + 1;

					if (pfmt.default_digits)
						digits	= 1;
					break;

				/*
				 * For floating-point (D/E/EN/ES/F/G) data
				 * edit-descriptors, the width is the number
				 * of significant digits plus the maximum
				 * size of the exponent plus six (for a
				 * leading blank, an optional sign, an
				 * optional leading zero, a decimal point,
				 * the 'E' exponent designator, and the
				 * exponent sign).
				 */
				case D_ED:
				case E_ED:
				case EN_ED:
				case ES_ED:
				case F_ED:
				case G_ED:
					if (pfmt.default_digits)
						digits	= _rw_mxdgt[fmtop-1][length-1];

					if (length == 16)
						exp	= DEXP16;
#ifdef	_F_REAL4
					else if (length == 4)
						exp	= DEXP4;
#endif
					else
						exp	= DEXP8;

					width	= digits + exp + 6;
					break;

				/*
				 * For logical (L) data edit-descriptors,
				 * the width is always two (one for the 'T'
				 * or 'F' and the other for a leading blank).
				 */
				case L_ED:
					width	= _rw_mxdgt[fmtop-1][length-1];
					break;

				/*
				 * For Q data edit-descriptors, the
				 * width is zero--no data is consumed.
				 */
				case Q_ED:
					width	= 0;
					break;

				/*
				 * Should never arrive here.
				 */
				default:
					width	= -1;
					break;
				} /* switch */

				/*
				 * Sanity check for valid width.
				 */
				if (width < 0) {
					stat	= FERDTYPE; /* Type mismatch */
					goto done;
				}
			}

			/*
			 * Check end-of-file and end-of-record conditions.
			 */

			if (cup->uend) {		/* If at EOF */
				stat	= FERDPEOF;	/* Read past EOF */
				goto done;
			}

			/*
			 * Set the number of consecutive data items, and be
			 * sure to adjust for the case when we're in the middle
			 * of a complex datum.
			 */

			kount	= MIN(repcnt,
				  ((count << cswitch) - (part & cswitch)));
			field	= width * kount;

			/*
			 * See if processing the current batch of edit-
			 * descriptors will exhaust the record.  If so,
			 * see if there's room for one more.
			 */

			if (field > cup->ulinecnt) {

				field	= width;
				kount	= 1;

				if (width > cup->ulinecnt) {

					/*
					 * If ADVANCE='NO' and the current
					 * edit descriptor requires data from
					 * beyond end of record, we have an
					 * EOR condition.  However, the EOR
					 * condition may be superseded by an
					 * error during data input conversion.  
					 */

					if (css->u.fmt.nonadv)
						stat	= FEEORCND;
					else
						if (cup->upad == OS_NO)
							stat	= FERDPEOR;

					/*
					 * If there are no characters left in
					 * the record and PAD='NO', then bypass
					 * the data transfer altogether.
					 */

					if (cup->ulinecnt <= 0 &&
					    cup->upad == OS_NO)
						goto done;
				}
			}
		}

		switch (fmtop) {

		/* Process numeric input */

		case B_ED:
		case D_ED:
		case E_ED:
		case EN_ED:
		case ES_ED:
		case F_ED:
		case G_ED:
		case I_ED:
		case L_ED:
		case O_ED:
		case Z_ED:

			ngcf	= _iconvtab[fmtop];

#ifdef	_CRAY
#pragma _CRI align
#endif

			for (i = 0; i < kount; i++) {	/* For consecutive items */
				register short	j;
				long		nstat;

				/* Clear subsequent words of a multi-word item */

				if (length > sizeof(int))
					for (j = 1; j < (length/sizeof(int)); j++)
						((int *) cptr)[j]	= 0;

				itemch	= MIN(MAX(0, cup->ulinecnt), width);
				tptr	= cup->ulineptr + itemch; /* end of field */
				nstat	= -1;

				(void) ngcf(cup->ulineptr, &width, &tptr, &mode,
					cptr, &nstat, &digits,
					&css->u.fmt.u.fe.scale);

				if (nstat < 0) {
#ifdef KEY /* Bug 8105 */
					/* VMS extension allows numeric field
					 * to end prematurely with "," */
					if (EX_ILLCHAR == nstat &&
					  ',' == (*(char *)tptr)) {
					  itemch = (tptr - cup->ulineptr) + 1;
					}
					else {
					  stat	= _nicverr(nstat);
					  if (stat > 0)
						  goto done;
					}
#else /* KEY Bug 8105 */
					stat	= _nicverr(nstat);
					if (stat > 0)
						goto done;
#endif /* KEY Bug 8105 */
				}

				/* Advance data addresses */

				cup->ulineptr	= cup->ulineptr + itemch;
				cup->ulinecnt	= cup->ulinecnt - itemch;
				chxfer		= chxfer + itemch;
				count		= count - part;
				cptr		= cptr + cinc[part];
				part		= part ^ cswitch;
			}

			repcnt	= repcnt - kount;

			break;

		/* Process nonnumeric (character) input */

		case A_ED:
		case R_ED:

			delta	= length - width;

			/*
			 * Check if format width equals data length and we have
			 * a stride of one.  If so, then we can move all of the
			 * data in one fell swoop.
			 */

			if (delta == 0 && tip->stride == 1 && cswitch == 0) {

				itemch	= MIN(MAX(0, cup->ulinecnt), field);

				(void) _pack(cup->ulineptr, cptr, itemch, -1);

				padcnt	= field - itemch;

				if (padcnt > 0)	/* If variable wider than field */
					(void) memset(cptr + itemch, BLANK, (size_t) padcnt);

				cup->ulineptr   = cup->ulineptr + itemch;
				cup->ulinecnt	= cup->ulinecnt - itemch;
				chxfer		= chxfer + itemch;
				count           = count - kount;
				cptr            = cptr + (stride * kount);
			}
			else

#ifdef	_CRAY
#pragma _CRI align
#endif

			for (i = 0; i < kount; i++) {	/* For consecutive items */

				ctmp	= cptr;
				itemch	= MIN(MAX(0, cup->ulinecnt), width);

				/*
				 * If the field width is wider than the length
				 * of the variable, we need to skip over part
				 * of the field.  However, make sure we don't
				 * skip past the end of the record.
				 */

				if (delta < 0) { /* If field wider than variable */
					itemch		= itemch + delta;
					cup->ulinecnt	= cup->ulinecnt + delta;
					cup->ulineptr	= cup->ulineptr - delta;
					padcnt		= (delta + width) - itemch;
				}

				/*
				 * If doing R format and the variable is larger
				 * than the field, we need to right-justify the
				 * data and fill-in the unused portion (we fill
				 * with blanks for character variables and zeros
				 * for all other data types).
				 */

				else {
					padcnt	= delta + (width - itemch);

					if (fmtop == R_ED && delta > 0) {
						register int	fill;

						fill	= (type == DVTYPE_ASCII) ?
								BLANK : 0;

						(void) memset(ctmp, fill, (size_t) delta);

						ctmp	= ctmp + delta;
						padcnt	= padcnt - delta;
					}
				}

				/* Move the actual data */

				if (itemch > 0)
					(void) _pack(cup->ulineptr, ctmp, itemch, -1);

				/*
				 * If the variable is wider than the field, or if there
				 * was insufficient data to satisfy the width, then pad
				 * out the variable with blanks.
				 */

				if (padcnt > 0)
					(void) memset(ctmp + itemch, BLANK, (size_t) padcnt);

				/* Advance data addresses */

				cup->ulineptr	= cup->ulineptr + itemch;
				cup->ulinecnt	= cup->ulinecnt - itemch;
				chxfer		= chxfer + itemch;
				count		= count - part;
				cptr		= cptr + cinc[part];
				part		= part ^ cswitch;
			}

			repcnt	= repcnt - kount;
			break;

		case Q_ED:
			if (length == 4)
				*(_f_int4 *)cptr	= MAX(cup->ulinecnt, 0);
			else if (length == 8)
				*(_f_int8 *)cptr	= MAX(cup->ulinecnt, 0);
			else if (length == 2)
				*(_f_int2 *)cptr	= MAX(cup->ulinecnt, 0);
			else	/* Assume length == 1 */
				*(_f_int1 *)cptr	= MAX(cup->ulinecnt, 0);

			/* Advance data addresses */

			count		= count - part;
			cptr		= cptr + cinc[part];
			part		= part ^ cswitch;
			repcnt		= repcnt - 1;
			break;

		case SLASH_ED:
			stat		= (*css->u.fmt.endrec)(css, cup, width);
			repcnt		= repcnt - 1;
			break;

		case TR_ED:
			cup->ulineptr	= cup->ulineptr + width;
			cup->ulinecnt	= cup->ulinecnt - width;
			repcnt		= repcnt - 1;
			break;

		case T_ED:
			tptr		= cup->ulineptr;	/* Old pos. */
			cup->ulineptr	= css->u.fmt.leftablim + width - 1;
			cup->ulinecnt	= cup->ulinecnt + (tptr - cup->ulineptr);
			repcnt		= 0;	/* Ignore repeat count */
			break;

		case TL_ED:
			cup->ulineptr	= cup->ulineptr - width;
			cup->ulinecnt	= cup->ulinecnt + width;
			/*
			 * If tabbed off the beginning of the record,
			 * move back to column 1.
			 */
			if (cup->ulineptr < css->u.fmt.leftablim) {
				cup->ulinecnt	= cup->ulinecnt -
						(css->u.fmt.leftablim - cup->ulineptr);
				cup->ulineptr	= css->u.fmt.leftablim;
			}
			repcnt		= repcnt - 1;
			break;

		case STRING_ED:
			/*
			 * Literals and H edit-descriptors are invalid in
			 * input formats.
			 */
// Bug 1883
# ifdef KEY
                        cup->ulineptr          += width;
# else
			stat			= FEFMTLII;
# endif
			repcnt			= repcnt - 1;
			break;

		case BN_ED:
			css->u.fmt.blank0	= 0;
			dfmode			= dfmode & ~MODEBZ;
			dfmode			= dfmode | MODEBN;
			repcnt			= 0;  /* Ignore repeat count */
			break;

		case BZ_ED:
			css->u.fmt.blank0	= 1;
			dfmode			= dfmode & ~MODEBN;
			dfmode			= dfmode | MODEBZ;
			repcnt			= 0;  /* Ignore repeat count */
			break;

		case DOLLAR_ED:		/* $ has no effect on input */
		case S_ED:		/* S, SS, SP have no effect on input */
		case SS_ED:
		case SP_ED:
			repcnt			= 0;  /* Ignore repeat count */
			break;

		case P_ED:
			css->u.fmt.u.fe.scale	= pfmt.rep_count;
			repcnt			= 0;  /* Force advancement */
			break;

		case COLON_ED:
			/*
			 * We have a colon edit-descriptor and, if the count
			 * is zero, we're done for now.
			 */
			if (count == 0)
				goto done;

			repcnt	= 0;	/* Ignore repeat count */
			break;

		case REPEAT_OP:
			/*
			 * Start of repeated format group.  Stack the repeat
			 * count and advance to the next format token.
			 */
			*css->u.fmt.u.fe.pftocs++	= pfmt.rep_count;
			repcnt				= 0; /* Force advance*/
			break;

		case ENDREP_OP:
			/*
			 * End of repeated format group.  Decrement the
			 * stacked count.  If the repeat count has not
			 * been satisfied then proceed to the first format
			 * token of the repeat group; otherwise unstack
			 * the repeat count and advance to the next format
			 * token.
			 */
			if ( --(*(css->u.fmt.u.fe.pftocs - 1)) < 1)
				css->u.fmt.u.fe.pftocs--; /* Pop the rep cnt */
			else
				css->u.fmt.u.fe.pfcp	= css->u.fmt.u.fe.pfcp +
							  pfmt.rep_count;

			repcnt	= repcnt - 1;

			break;

		case REVERT_OP:
			/*
			 * If the revert group does not contain any data
			 * edit-descriptors and iolist items remain
			 * (defined as a nonzero count), then we have an
			 * infinite format loop.
			 */
			if (pfmt.rgcdedf == 0 && count > 0)
				stat	= FEFMTILF; /* Infinite format loop */
			else {
				/*
				 * If the count is zero, then we exit with
				 * the format positioned at the REVERT_OP
				 * entry and subsequent calls can continue
				 * from there, if necessary.  If there are
				 * data items remaining (count > 0) then
				 * we flush the record, position the format
				 * to the reversion point, and continue
				 * processing.
				 */

				if (count == 0)
					goto done;

				/* Read the next record */

				stat	= (*css->u.fmt.endrec)(css, cup, 1);
				repcnt	= 0;  /* Force advancement */

				/* Position format to reversion point */

				css->u.fmt.u.fe.pfcp	= css->u.fmt.u.fe.pfcp +
							  pfmt.rep_count - 1;
			}
			break;

		default:
			stat	= FEINTIPF;	/* Invalid parsed format */
			break;

		} /* switch (fmtop) */

		/*
		 * If the repeat count has been exhausted then advance to
		 * the next format token.
		 */

		if (stat == 0 && repcnt < 1) {

			if (fmtop == STRING_ED)
				css->u.fmt.u.fe.pfcp	= css->u.fmt.u.fe.pfcp +
						 ((width +
						  FMT_ENTRY_BYTE_SIZE - 1) /
						  FMT_ENTRY_BYTE_SIZE);

			css->u.fmt.u.fe.pfcp	= css->u.fmt.u.fe.pfcp + 1;
			pfmt		= *css->u.fmt.u.fe.pfcp;
			repcnt		= pfmt.rep_count;
			css->u.fmt.u.fe.fmtcol	= pfmt.offset; /* New position*/
		}

	} while (stat == 0);
done:

#ifdef	_CRAYT3D
	if (shared && ((long *) cptr != shrd)) {
		register int	items;

		/* Move the data to shared memory, see if there is more to do */

		items	= ((long *) cptr - shrd) / elwords;
		(void) _cpytosdd(dptr, shrd, items, elwords, tip->stride, offset);
		offset	= offset + items;
	}

   } while (stat == 0 && shared && offset < tcount);
#endif

	/* Update unit table and statement state fields */

	*css->u.fmt.u.fe.pftocs	= repcnt;

	if (css->u.fmt.nonadv)	/* Increment the SIZE value */
		css->u.fmt.u.fe.charcnt	= css->u.fmt.u.fe.charcnt + chxfer;

	/* Process any error which occurred */

	if (stat == FEEORCND) {
		if ((cup->uflag & (_UEORF | _UIOSTF)) == 0)
			_ferr(css, stat);	/* end of record condition */

		cup->pnonadv	= 0;		/* flag no more current rec */ 
	}
	else if (stat > 0) {
		if ((cup->uflag & (_UERRF | _UIOSTF)) == 0)
			_ferr(css, stat);	/* Run-time error */
	}
	else if (stat < 0) {
		 if ((cup->uflag & (_UENDF | _UIOSTF)) == 0)
			_ferr(css, stat);	/* EOF-type error */
	}

	return(stat);
}

#if	defined(BUILD_OS_DARWIN)
#elif	defined(__mips) || (defined(_LITTLE_ENDIAN) && defined(__sv2))
#include <ieeefp.h>
#elif	defined(_LITTLE_ENDIAN) && !(__sv2)
#include <fpu_control.h>
#endif
/*
 *	_nicverr()  Map NICV-type errors to Fortran error codes.
 *    
 *      On mips with overflow and underflow, allow the return of zero
 *      or infinity if the csr is off for these two interrupts.  The
 *	conversion routines return the correct value but also return
 *	a negative value to indicate overflow or underflow.  Return a
 *	zero as the function result if the error is not to be given.
 *    
 */
int
_nicverr(const int nicverror)
{
	int	errn;

	switch (nicverror) {
		case EX_ILLCHAR:	/* Invalid (nonnumeric) character */
			errn	= FENICVIC;
			break;
		case EX_FIXOFLO:	/* Fixed-point overflow */
			errn	= FENICVOF;
			break;
		case EX_EXPUFLO:	/* Floating-point underflow */
			errn	= FENICVEU;
#if	defined(__mips) || (defined(_LITTLE_ENDIAN) && defined(__sv2))
			/* this returns only the mask bits */
			if ((fpgetmask() & FP_X_UFL) == 0)
				errn	= 0;
#elif	defined (_LITTLE_ENDIAN) && !defined(__sv2)
			errn	= 0; /* needs fixing for F2003 IEEE 754 support */
#endif
			break;
		case EX_EXPOFLO:	/* Floating-point overflow */
			errn	= FENICVEO;
#if	defined(__mips) || (defined(_LITTLE_ENDIAN) && defined(__sv2))
			/* this returns only the mask bits */
			if ((fpgetmask() & FP_X_OFL) == 0)
				errn	= 0;
#elif	defined (_LITTLE_ENDIAN) && !defined(__sv2)
			errn	= 0; /* needs fixing for F2003 IEEE 754 support */
#endif
			break;
		case EX_NULLFLD:	/* Null field */
			errn	= FENICVBK;
			break;
		case EX_INVLOGI:	/* Invalid logical input */
			errn	= FERDIVLG;
			break;
		default:	/* Unknown (internal) error */
			errn	= FEINTUNK;
			break;
	}

	return(errn);
}
