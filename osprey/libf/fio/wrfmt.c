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



#pragma ident "@(#) libf/fio/wrfmt.c	92.6	06/21/99 10:37:55"

#include <memory.h>
#include <stdlib.h>
#include <string.h>
#include <fortran.h>
#include <math.h>
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
oc_func	*_oconvtab[LAST_DATA_ED + 1];
extern	const short	_odedtab[DVTYPE_NTYPES];
extern	short		_o_sup_flg_tab[DVTYPE_NTYPES];
extern	long		_o_sup_val_tab[DVTYPE_NTYPES];

#ifdef KEY /* Bug 8309 */
/* Integer base raised to the power exp */
static int ipow(int base, int exp) {
  int result = 1;
  for (; exp > 0; exp -= 1) {
    result *= base;
  }
  return result;
}
#endif /* KEY Bug 8309 */

#undef	BLANK
#define	BLANK	((long) ' ')

/*
 *	_wrfmt()	Write format processing
 *
 *		uss	Current Fortran I/O statement state pointer
 *		cup	Unit pointer
 *		dptr	Pointer to data 
 *		tip	Type information packet
 */
int
_wrfmt(
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
	register short	supflg;		/* Is variable eligible to be suppressed */
	register ftype_t type;		/* Fortran data type */
	register int32	delta;		/* Length/field width difference */
	register int32	field;		/* Consecutive conversion field size */
	register int32	i;		/* Scratch loop variable */
	register int32	kount;		/* Number of consecutive conversions */
	register int32	length;		/* Length of datum in bytes */
	register int32	repcnt;		/* Local copy of *css->u.fmt.pftocs */
	int		cinc[2];	/* Increments for datum parts */
	register int	stat;		/* Error code */
	register int	stride;		/* Stride between data in bytes */
	register char	*cptr;		/* Character pointer to datum */
	register char	*ctmp;		/* Temporary character pointer */
	long		digits;		/* Digits field of edit-descriptor */
	long		exp;		/* Exponent field of edit-descriptor */
	long		mode;		/* Mode word for conversion */
	long		width;		/* Width field of edit-descriptor */
	register long	count;		/* Number of data items */
	register long	dfmode;		/* MODESN/MODE77 mode bits */
	fmt_type	pfmt;		/* Current parsed format entry */
#ifdef	_CRAYT3D
	register short	shared;		/* Is variable shared? */
	register int	elwords;	/* Number of words per item */
	register int	offset;		/* Offset from address in item units */
	register int32	tcount;		/* Number of items to move */
	long		shrd[MAXSH];	/* Buffer for shared data */
#endif

#ifndef KEY /* this causes wrong function being called when compiled by gcc */
	const 
#endif
	    oc_func	*ngcf;		/* Generic NOCV-type conversion func */

	/* If these assertions are not all true, then we're in deep doo-doo */

	assert (cup != NULL);
	assert (tip != NULL);

	type	= tip->type90;
	count	= tip->count;

	cswitch	= 0;
	stat	= 0;
	part	= 1;

	pfmt	= *css->u.fmt.u.fe.pfcp;
	repcnt	= *css->u.fmt.u.fe.pftocs;
	length	= tip->elsize;
	stride	= tip->stride * length;
	cinc[1]	= stride;
	supflg	= _o_sup_flg_tab[type] && (length == sizeof(long));
#ifdef KEY /* Bug 573 */
        register short width_zero_flag = FALSE;
#endif /* KEY Bug 573 */

	/* If COMPLEX data type, adjust data length and increments */

	if (type == DVTYPE_COMPLEX) {
		length	= length / 2;
		cinc[0]	= length;
		cinc[1]	= stride - length;
		cswitch	= 1;
		part	= 0;
	}

	dfmode	= ((cup->uft90 == 0) ? MODE77 : 0) |
		  ((css->u.fmt.cplus == 1) ? MODESN : 0);

#ifdef	_CRAYT3D
	if (_issddptr(dptr)) {
		offset	= 0;
		elwords	= tip->elsize / sizeof(long);
		shared	= 1;
		stride	= tip->elsize;	
		tcount	= count;
	}
	else
		shared	= 0;

   do	{
	if (shared) {
		/* we copy the data into local array shrd, and write */
		/* from there */
		count	= MIN(MAXSH/elwords, (tcount - offset));
		cptr	= (char *) shrd;

		(void) _cpyfrmsdd(dptr, shrd, count, elwords, tip->stride, offset);
		offset	= offset + count;
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
		exp	= pfmt.exponent;

		/* Basic sanity check on the parsed format */

		if (fmtop > LAST_OP || fmtop < FIRST_DATA_ED) {
			stat	= FEINTIPF;	/* Invalid parsed format */
			goto done;
		}

		if (fmtop <= LAST_DATA_ED || fmtop == STRING_ED) {

			if (fmtop == STRING_ED)

				kount	= repcnt;

			else {	/* fmtop <= LAST_DATA_ED */

				/*
				 * We have a data-edit descriptor and if the
				 * count is exhausted, then we're done (for
				 * the time being).
				 */

				if (count == 0)
					goto done;

				/*
				 * Validate the data edit-descriptor against
				 * the data type and do the Fortran 90 mapping
				 * G data edit-descriptor.
				 */

				if (INVALID_WTYPE(fmtop, type)) {
					/* Type mismatch */
					stat	= FEWRTYPE;
					goto done;
				}

				if (fmtop == G_ED) {

					fmtop	= _odedtab[type];

					if (type != DVTYPE_REAL &&
					    type != DVTYPE_COMPLEX)
						digits	= 1;
				}

				/*
				 * Set (or reset) the default mode for the
				 * numeric conversion routines.
				 */

				if (type == DVTYPE_ASCII)
					mode	= 0;
				else {
					mode	= (long) _wr_ilchk[fmtop-1][length-1];

					if (mode == INVALID_INTLEN) {
						/* Type mismatch */
						stat	= FEWRTYPE;
						goto done;
					}
				}

				/* if real and the flag to skip write
				 * of minus sign of -0.0 is set, set
				 * mode bit for conversion routines.
				 */

				if ((type == DVTYPE_REAL ||
				     type == DVTYPE_COMPLEX) &&
				     cup->ufnegzero != 0)
					mode	= mode | MODEMSN;

				mode	= mode | dfmode; /* Add defaults */

				/*
				 * Handle zero-width formats.
				 */

				if (width == 0) {
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
					 * For integer (B/I/O/Z) data edit-
					 * descriptors, the width is the
					 * maximum number of "digits" plus
					 * one for a leading blank and (I
					 * only) one for an optional sign.
					 */
					case B_ED:
					case I_ED:
					case O_ED:
					case Z_ED:
#ifdef KEY /* Bug 573 */
                                                width_zero_flag = TRUE;
#endif /* KEY Bug 573 */
						width	= _rw_mxdgt[fmtop-1][length-1];
						/* Fix limitation in table */

						if (width == 127)
							width	= 128;

						if (pfmt.default_digits)
							digits	= 1;
						else if (width < digits)
							width	= digits;

						/* Allow for blank and sign */

						width	= width + 1;

						if (fmtop == I_ED)
							width	= width + 1;

						/*
						 * The (f95) standard explicitly
						 * requires that {B|I|O|Z}0.0
						 * format with a zero datum
						 * produce exactly one blank.
						 * The only practical way to do
						 * this is to peek at the datum
						 * now and--if it's zero--adjust
						 * the field width accordingly.
						 */

						if (digits == 0) {
							register int64	datum;

							switch (length) {

							case 8:
								datum	= *(int64 *) cptr;
								break;

#ifndef	_CRAY1
							case 4:
								datum	= *(int32 *) cptr;
								break;
#endif

/* KEY: this should probably be #if _F_INT2 */
#if	defined(__mips) || defined(_SOLARIS) || defined(_LITTLE_ENDIAN)
							case 2:
								datum	= *(short *) cptr;
								break;

							case 1:
								datum	= *cptr;
								break;
#endif

							} /* switch */
#ifdef KEY /* Bug 573 */
							if (datum == 0) {
								width	= 1;
								width_zero_flag = FALSE;
							}
#else /* KEY Bug 573 */
							if (datum == 0)
								width	= 1;
#endif /* KEY Bug 573 */
						}
						break;

					/*
					 * For floating-point (D/E/EN/ES/F/G)
					 * data edit-descriptors, the width
					 * is the number of significant digits
					 * plus the max. size of the exponent
					 * plus six (for a leading blank, an
					 * optional sign, an optional leading
					 * zero, a decimal point, the 'E'
					 * exponent designator, and the
					 * exponent sign).
					 */
					case D_ED:
					case E_ED:
					case EN_ED:
					case ES_ED:
#ifndef KEY /* Bug 8309 */
					case F_ED:
#endif /* KEY Bug 8309 */
					case G_ED:
#ifdef KEY /* Bug 573 */
                                                width_zero_flag = TRUE;
#endif /* KEY Bug 573 */
						if (pfmt.default_digits)
							digits	= _rw_mxdgt[fmtop-1][length-1];

						if (exp == 0) {
							if (length == 16)
								exp	= DEXP16;
#ifdef	_F_REAL4
							else if (length == 4)
								exp	= DEXP4;
#endif
							else
								exp	= DEXP8;
						}

						width	= digits + exp + 6;
						break;

#ifdef KEY /* Bug 8309 */
					/* For F_ED, which does
					 * not have an exponent, we need
					 * the number of digits indicated
					 * by the maximum exponent.
					 */
					case F_ED:
                                                width_zero_flag = TRUE;
						if (4 == length) {
						  width = ipow(10, DEXP4);
						}
						else if (8 == length) {
						  width = ipow(10, DEXP8);
						}
						else {
						  width = pow(10, DEXP16);
						}
						width += digits + 1 /* sign */;
						break;
#endif /* KEY Bug 8309 */

					/*
					 * For logical (L) data edit-
					 * descriptors, the width is always
					 * two (one for the 'T' or 'F' and
					 * the other for a leading blank).
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
						stat	= FEWRTYPE;
						goto done;
					}
				}

				/*
				 * Set the number of consecutive data items, and be
				 * sure to adjust for the case when we're in the
				 * middle of a complex datum.
				 */

				kount	= MIN(repcnt,
					  ((count << cswitch) - (part & cswitch)));
			}

			field	= width * kount;

			/*
			 * Check to see if we have an outstanding TR condition.
			 * If so, then blank fill that portion of the record
			 * which extends beyond the existing highwater mark
			 * (cup->ulinemax); but in no case go beyond the end of
			 * the line buffer (cup->urecsize).
			 */

			if (cup->ulinecnt > cup->ulinemax) {
				register short	j, k;

				if (cup->ulinecnt > cup->urecsize) {
					stat	= FEWRLONG; /* Record too long*/
					goto done;
				}

				k	= cup->ulinecnt;

				/* The following loop should vectorize */

				for (j = cup->ulinemax; j < k; j++)
					cup->ulinebuf[j]	= BLANK;

				/* Update highwater mark */

				cup->ulinemax	= cup->ulinecnt;
			}

			/*
			 * See if processing the current batch of edit-
			 * descriptors will overflow the line.  If so,
			 * see if there's room for one more.
			 */

#ifdef KEY /* Bug 10965 */
			if ((!width_zero_flag) &&
			  (cup->ulinecnt + field) > cup->urecsize)
#else /* KEY Bug 10965 */
			if ((cup->ulinecnt + field) > cup->urecsize)
#endif /* KEY Bug 10965 */
			{

				if ((cup->ulinecnt + width) > cup->urecsize) {
					stat	= FEWRLONG;	/* Record too long */
					goto done;
				}
				else {	/* Do one item */
					kount	= 1;
					field	= width;
				}
			}
		}

		switch (fmtop) {

		/* Process numeric-type output */

		case B_ED:
		case O_ED:
		case Z_ED:
		case D_ED:
		case E_ED:
		case EN_ED:
		case ES_ED:
		case F_ED:
		case G_ED:
		case I_ED:
		case L_ED:

			ngcf	= _oconvtab[fmtop];

#ifdef	_CRAY
#pragma _CRI align
#endif

#ifdef KEY /* Bug 3992 */
			if (width_zero_flag) {
				field = 0;
			}
#endif /* KEY Bug 3992 */

			for (i = 0; i < kount; i++) { /* For consecutive items */

				/* Convert next item */

				if (supflg && (_o_sup_val_tab[type] == *(long *) cptr)) {
					register short	j;

#ifdef	_CRAY1
#pragma _CRI ivdep
#endif
					for (j = 0; j < width; j++)
						cup->ulineptr[j]	= BLANK;
				}
				else {
#ifdef KEY /* Bug 573, 3992, 7990, 10965 */
				    if (width_zero_flag) {
					long linebuf[width];
					(void) ngcf(cptr, linebuf, &mode,
						&width, &digits, &exp,
						&css->u.fmt.u.fe.scale);
					int k = 0;
					for (; k < width; k += 1) {
					  if (linebuf[k] != BLANK) {
					    field += 1;
					    /* Record too long? */
					    if ((cup->ulinecnt + field) >
					      cup->urecsize) {
					      stat = FEWRLONG;
					      goto done;
					    }
					    *cup->ulineptr++ = linebuf[k];
					  }
					}
				    }
				    else
				    {
#endif /* KEY Bug 573, 3992, 7990, 10965 */
					(void) ngcf(cptr, cup->ulineptr, &mode,
						&width, &digits, &exp,
						&css->u.fmt.u.fe.scale);
#ifdef KEY /* Bug 573, 3992, 7990, 10965 */
					cup->ulineptr = cup->ulineptr + width;
				    }
#endif /* KEY Bug 573, 3992, 7990, 10965 */
                                 }

				/* Advance data addresses */

#ifndef KEY /* Bug 573, 3992, 7990, 10965 */
				cup->ulineptr	= cup->ulineptr + width;
#endif /* KEY Bug 573, 3992, 7990, 10965 */
				count		= count - part;
				cptr		= cptr + cinc[part];
				part		= part ^ cswitch;
			}

			cup->ulinecnt	= cup->ulinecnt + field;

			/* Set new highwater mark, if necessary */

			if (cup->ulinecnt > cup->ulinemax)
				cup->ulinemax	= cup->ulinecnt;

			repcnt	= repcnt - kount;

			break;

		/* Process nonnumeric (character) output */

		case A_ED:
		case R_ED:

			delta	= width - length;

			/*
			 * Check if format width equals data length and we have
			 * a stride of one.  If so, then we can move all of the
			 * data in one fell swoop.
			 */

			if (delta == 0 && tip->stride == 1) {
				register short	knt;

				(void) _unpack(cptr, cup->ulineptr, field, -1);

				cup->ulineptr	= cup->ulineptr + field;
				knt		= kount >> cswitch;

				if (cswitch != 0 && ((kount & 01) != 0)) {

					/* If complex and odd count */

					count	= count - part;
					cptr	= cptr + cinc[part];
					part	= part ^ 1;
				}

				count	= count - knt;
				cptr	= cptr + (stride * knt);
			}
			else

#ifdef	_CRAY
#pragma _CRI align
#endif

			for (i = 0; i < kount; i++) {	/* For consecutive items */

				ctmp	= cptr;

				/*
				 * If the field width is wider than the length
				 * of the variable, we need to generate blanks
				 * for part of the field.
				 */

				if (delta > 0) {
					register short	j;

					/* The following loop should vectorize */

					for (j = 0; j < delta; j++)
						cup->ulineptr[j]	= BLANK;

					/* Move the actual data */

					(void) _unpack(ctmp, cup->ulineptr + delta,
						length, -1);
				}
				else {

					/*
					 * If doing R format and the variable is
					 * larger than the field, we need to
					 * right-justify the data in the field.
					 */

					if (fmtop == R_ED)
						ctmp	= ctmp - delta;

					/* Move the actual data */

					(void) _unpack(ctmp, cup->ulineptr, width, -1);
				}

				/* Advance data addresses */

				cup->ulineptr	= cup->ulineptr + width;
				count		= count - part;
				cptr		= cptr + cinc[part];
				part		= part ^ cswitch;
			}

			cup->ulinecnt	= cup->ulinecnt + field;

			/* Set new highwater mark, if necessary */

			if (cup->ulinecnt > cup->ulinemax)
				cup->ulinemax	= cup->ulinecnt;

			repcnt	= repcnt - kount;

			break;

		case SLASH_ED:
			stat	= (*css->u.fmt.endrec)(css, cup, width);
			repcnt	= repcnt - 1;
			break;

		case TR_ED:
			cup->ulinecnt	= cup->ulinecnt + width;
			cup->ulineptr	= cup->ulineptr + width;
			repcnt		= repcnt - 1;
			break;

		case T_ED:
			cup->ulinecnt	= width - 1;
			cup->ulineptr	= cup->ulinebuf + (width - 1);
			repcnt		= 1;	/* Ingore repeat count */
			goto check_left;

		case TL_ED:
			cup->ulinecnt	= cup->ulinecnt - width;
			cup->ulineptr	= cup->ulineptr - width;
check_left:
			/*
			 * If tabbed off the beginning of the record, then
			 * move back to column 1 (relative to left tab limit).
			 */
			if (cup->ulineptr < css->u.fmt.leftablim) {
				cup->ulineptr	= css->u.fmt.leftablim;
				cup->ulinecnt	= cup->ulineptr - cup->ulinebuf;
			}

			repcnt		= repcnt - 1;
			break;

		case STRING_ED:
			ctmp	= (char *) (css->u.fmt.u.fe.pfcp + 1);

			if (width > 0) {

				/* Copy literal to line buffer */

				for (i = 0; i < kount; i++) {

					(void) _unpack(ctmp, cup->ulineptr, width, -1);

					cup->ulineptr	= cup->ulineptr + width;
				}

				cup->ulinecnt	= cup->ulinecnt + field;

				/* Set new highwater mark, if necessary */

				if (cup->ulinecnt > cup->ulinemax)
					cup->ulinemax	= cup->ulinecnt;
			}

			repcnt	= repcnt - kount;
			break;

		case BN_ED:		/* BN and BZ have no effect on output */
		case BZ_ED:
			repcnt	= 0;	/* Ignore repeat count */
			break;

		case S_ED:
		case SS_ED:
			css->u.fmt.cplus	= 0;
			dfmode			= dfmode & ~MODESN;
			repcnt			= 0;	/* Ignore repeat count*/
			break;

		case SP_ED:
			css->u.fmt.cplus	= 1;
			dfmode			= dfmode | MODESN;
			repcnt			= 0;	/* Ignore repeat count*/
			break;

		case P_ED:
			css->u.fmt.u.fe.scale	= pfmt.rep_count;
			repcnt			= 0;	/* No repeat cnt for P*/
			break;

		case Q_ED:
#ifdef KEY /* Bug 8258 */
			/* The VMS manual says this edit descriptor is
			 * legal but ignored on output */
#else /* KEY Bug 8258 */
			/*
			 * The Q edit-descriptor is invalid on output.
			 */
			stat			= FEFMTQIO;
#endif /* KEY Bug 8258 */
			repcnt			= repcnt - 1;
			break;

		case COLON_ED:
			/*
			 * We have a colon edit-descriptor and, if the count
			 * is zero, we're done for now.
			 */

			if (count == 0)
				goto done;

			repcnt		= 0;	/* Ignore repeat count */
			break;

		case DOLLAR_ED:
			css->u.fmt.nonl	= 1;
			repcnt		= 0;	/* Ignore repeat count */
			break;

		case REPEAT_OP:
			/*
			 * Start of repeated format group.  Stack the repeat
			 * count and advance to the next format token.
			 */
			*css->u.fmt.u.fe.pftocs++	= pfmt.rep_count;
			repcnt				= 0; /* Force advance */
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
				css->u.fmt.u.fe.pftocs--; /* Pop repeat count */
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

				/* Write the record */

				stat	= (*css->u.fmt.endrec)(css, cup, 1);

				repcnt	= 0;	/* Force advancement */

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
			fmtop		= pfmt.op_code;
			width		= pfmt.field_width;
			repcnt		= pfmt.rep_count;
			css->u.fmt.u.fe.fmtcol	= pfmt.offset; /* pos in fmt */
		}

	} while (stat == 0);
done:

#ifdef	_CRAYT3D
   continue;
   } while (stat == 0 && shared && offset < tcount);
#endif

	/* Update unit table fields */

	*css->u.fmt.u.fe.pftocs	= repcnt;

	/* Process any error which occurred */

	if (stat > 0 && (cup->uflag & (_UERRF | _UIOSTF)) == 0)
		_ferr(css, stat);

	return(stat);
}
