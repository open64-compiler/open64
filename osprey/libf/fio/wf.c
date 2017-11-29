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



#pragma ident "@(#) libf/fio/wf.c	92.2	06/18/99 15:49:57"

#include <ctype.h>
#include <errno.h>
#include <liberrno.h>
#include <fortran.h>
#include <memory.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <cray/fmtconv.h>
#include <cray/format.h>
#include <cray/nassert.h>
#ifndef	_ABSOFT
#include <sys/unistd.h>
#endif
#include <cray/clibinc_config.h>
#include "fio.h"
#include "fmt.h"
#include "fstats.h"
#include "f90io.h"
#include "lio.h"
#ifdef	_CRAYMPP
#include <stdarg.h>
#endif

#ifdef	_UNICOS

#pragma	_CRI duplicate $WFI  as $WLI
#pragma	_CRI duplicate $WFA$ as $WLA$
#pragma	_CRI duplicate $WFA$ as $EFA$
#pragma	_CRI duplicate $WFF  as $WLF
#pragma	_CRI duplicate $WFF  as $EFF

#endif	/* _UNICOS */

#undef	BLANK
#define	BLANK		((long) ' ')
#undef	ZERO
#define	ZERO		((long) '0')

/*
 *	_newrec_listio_after_nonadvancing is 1 if list directed
 *	writes after a nonadvancing read or write will cause the record
 *	to be flushed before list-directed write processing begins, 0 if
 *	a list-directed write continues writing to the current record.
 *
 *	These variables are modified by _wf_setup.
 */
short _newrec_listio_after_nonadvancing = 0;

#ifdef	_UNICOS

int	$WFF(void);

#define	ERROR0(cond, n) {		\
	if (!(cond))			\
		_ferr(css, n);		\
	else				\
		goto error;		\
}

#define	ERROR1(cond, n, p) {		\
	if (!(cond))			\
		_ferr(css, (n), p);	\
	else				\
		goto error;		\
}

/* Define macros to convert _numargs() to number of arguments */ 
#define ARGS_6	(4 + 2*sizeof(_fcd)/sizeof(long))
#define ARGS_7	(5 + 2*sizeof(_fcd)/sizeof(long))
#define ARGS_8	(6 + 2*sizeof(_fcd)/sizeof(long))
#define ARGS_9	(7 + 2*sizeof(_fcd)/sizeof(long))

/*
 *	Here we do some things for upward compatibility with CFT77 5.0.2.
 */
#define IS_PFORM_BROKEN	(_numargs() < ARGS_9)	/* true if pform is broken */

/*
 *	$WFI - write formatted initialization
 *
 *	CALL	$WFI,(funit, format, err, _arg4, iostat, rec, pform, inumelt,
 *			inumcfe)
 *
 *		funit	Address of Fortran unit designator (integer unit
 *			number for external I/O or Fortran character
 *			descriptor (FCD) for internal I/O)
 *		format	Address of format (Fortran character descriptor or
 *			hollerith); NULL if list-directed
 *		err	Address of error address (ERR=label)
 *		_arg4	Unused
 *		iostat	Address of I/O status variable (integer variable)
 *		rec	Address of integer record number (NULL implies
 *			sequential I/O)
 *		pform	Address of address of parsed format (NULL if no
 *			compiler-supplied word; points to NULL if not yet
 *			parsed)
 *		inumelt Address of number of internal array elements
 *			(internal I/O only)
 *		inumcfe	Argument passed by new compilers to indicate that
 *			the pform argument is fixed, and to contain the
 *			number of array elements in a character format.
 *
 *	$WFI calls:
 *
 *		_imp_open77(), _unit_seek(), _parse(), _ferr()
 */

#ifdef	_CRAYMPP
$WFI(
_fcd		funit,		/* Address of unit number or FCD	*/
...
)
#else
int
$WFI(
_fcd		funit,		/* Address of unit number or FCD	*/
_fcd		format,		/* Address of format (FCD or hollerith)	*/
long		*err,		/* Address of error processing address	*/
long		*_arg4,		/* Unused				*/
_f_int		*iostat,	/* Address of IOSTAT variable		*/
_f_int		*rec,		/* Address of direct access record no.	*/
fmt_type	**pform,	/* Address of address of parsed format	*/
long		*inumelt,	/* Address of int. array element count	*/
long		*inumcfe	/* Address of number of format elements	*/
)
#endif
{
	register int	errf;		/* ERR processing flag	*/
	register int 	errn;		/* Error number		*/
	register int	iost;		/* I/O statement type	*/
	register int	iotp;		/* I/O type		*/
	register recn_t	recn;		/* Record number	*/
	register unum_t	unum;		/* Unit number		*/
	fmt_type	**prsfmt;	/* Parsed format info.	*/
	unit		*cup;		/* Unit table pointer	*/
	FIOSPTR		css;		/* I/O statement state	*/
#ifdef	_CRAYMPP
	va_list args;
	_fcd	format;		/* Address of format (FCD or hollerith) */
	long	*err;		/* Address of error processing address	*/
	long	*end;		/* Address of end processing address	*/
	_f_int	*iostat;	/* Address of IOSTAT variable		*/
	_f_int	*rec;		/* Address of direct access record no.	*/
	fmt_type **pform;	/* Address of address of parsed format	*/
	long	*inumelt;	/* Address of int. array element count	*/
	long	*inumcfe;	/* Address of number of format elements */
#endif

	GET_FIOS_PTR(css);

	/* Check if recursive triple-call I/O */

	if (css->f_iostmt != 0)
		_ferr(css, FEIOACTV);

#ifdef	_CRAYMPP
	va_start(args, funit);
	format	= va_arg(args, _fcd);
	err	= va_arg(args, long *);
	end	= va_arg(args, long *);
	iostat	= va_arg(args, _f_int *);
	rec	= va_arg(args, _f_int *);

	if (_numargs() > ARGS_6) {
		pform	= va_arg(args, fmt_type **);
		if (_numargs() > ARGS_7) {
			inumelt	= va_arg(args, long *);
			if (_numargs() > ARGS_8) {
				inumcfe	= va_arg(args, long *);
			}
		}
	}
	va_end(args);
#endif

	errn	= 0;

	/* Establish error processing options */

	if (iostat != NULL)
		*iostat	= 0;		/* Clear IOSTAT variable, if extant */

	errf	= ((err != NULL) || (iostat != NULL));

	/* Check if formatted or list-directed */

	iost	= (_fcdtocp(format) != NULL) ? T_WSF : T_WLIST;
	iotp	= SEQ;			/* Assume sequential */

	/* Check if we're doing internal I/O or external I/O */

	if (_fcdlen(funit)) {		/* If internal I/O */
		iotp	= INT;
		STMT_BEGIN(-1, 1, iost, NULL, css, cup);
	}
	else {				/* Else external I/O */
		unum	= **(_f_int **) &funit;

		if (rec != NULL) {	/* If direct access */
			iost	= T_WDF;	/* Set direct formatted read */
			iotp	= DIR;
			recn	= *rec;
		}

		STMT_BEGIN(unum, 0, iost, NULL, css, cup);

		if (cup == NULL) {	/* If not connected */
			int	stat;	/* Status */

			cup	= _imp_open77(css, iotp, FMT, unum, errf, &stat);

			/*
			 * If the open failed, cup is NULL and stat contains 
			 * the error number.
			 */

			if (cup == NULL) {
				errn	= stat;
				goto error;
			}
		}
	}

	/* All paths which lead here have set cup to a non-null value */

	assert (cup != NULL);

	/* Copy the user's error processing options into the unit table */

	cup->uflag	= (iostat != NULL ? _UIOSTF : 0) |
			  (   err != NULL ?  _UERRF : 0);
	cup->uiostat	= iostat;

	if (iotp != INT) {			/* If not internal I/O */

		/* If trying to write a file without write permission */

		if ((cup->uaction & OS_WRITE) == 0) {
			errn	= FENOWRIT;	/* No write permission */
			ERROR0(errf, errn);
		}

		/* If attempting formatted I/O on an unformatted file */

		if (!cup->ufmt) {
			errn	= FEFMTTIV;	/* Formatted not allowed */
			ERROR0(errf, errn);
		}
	}

	/* Initialize fields in the Fortran statement state structure */

	css->u.fmt.icp		= NULL;
	css->u.fmt.nonl		= 0;
	css->u.fmt.freepfmt	= 0;


	if (_fcdtocp(format) != NULL) {	/* If not list-directed output */
		char	*fptr;
		int	flen;
		int	fnum;
		int	stsz;

		/*
		 * Initialize fmtbuf before any call to _ferr().
		 */
		css->u.fmt.u.fe.fmtbuf	= NULL;
		css->u.fmt.u.fe.fmtnum	= 0;
		css->u.fmt.u.fe.fmtcol	= 0;
		css->u.fmt.u.fe.scale	= 0;
		css->u.fmt.cplus	= 0;

		/*
		 * For formats passed as hollerith (integer) variables,
		 * there is no rigorous definition of the "length" of the
		 * format, so we simply use strlen() as a first-order
		 * approximation.
		 *
		 * For static formats (FORMAT statements) or formats
		 * which are character constants or simple character
		 * variables, the length of the format is the length of
		 * the character string.
		 *
		 * For formats passed as character arrays, then the length
		 * of the format is the length of the entire array.  We
		 * compute this by multiplying the length of the element
		 * passed times the dimension of the array (inumcfe argument).
		 *
		 * We cannot distinguish the latter two cases without the
		 * the inumcfe argument (see SPR 52032), which was added to
		 * CF77 5.0.2.19.  If we do not have the inumcfe argument,
		 * we resort to a strlen() call.
		 */

		if (_fcdlen(format) == 0) {	/* If noncharacter format */
			fptr	= *(char **) &format;
			flen	= strlen(fptr);
		}
		else {			/* Else character format */
			register int	repl;

			if (_numargs() > ARGS_8 && inumcfe != NULL)
				repl	= *inumcfe;
			else
				repl	= -1;

			fptr	= _fcdtocp(format);
			flen	= (repl >= 0) ? repl * _fcdlen(format) :
						strlen(fptr);
		}

		/*
		 * The pform argument was not passed to the library in early
		 * versions of CFT77 (2.0 and earlier on CRAY-2's; 4.0 and
		 * earlier on CX/CEA's).  This check can be removed when we
		 * no longer support those compilers.
		 */

		if (_numargs() > ARGS_6) {
			prsfmt	= pform;
			/*
			 * The pform argument was passed incorrectly by the
			 * CFT77 5.0 compiler on CX/CEA systems.  The fixed
			 * compiler passes the inumcfe argument to indicate that
			 * pform is passed correctly.  If the inumcfe argument
			 * is not passed and pform != NULL then pform was 
			 * passed with one instead of two levels of 
			 * indirection.
			 */
			if (IS_PFORM_BROKEN && pform != NULL) {
				/* preparsed pform was passed incorrectly */
				if (*(long*)pform == -1)
					/* variable format */
					prsfmt	= NULL;
				else
					/* pre-parsed format */
					prsfmt	= (fmt_type**)&pform;
			}
		}
		else
			prsfmt	= NULL;

		/*
		 * Pull an optional statement number off of the beginning of
		 * the format and save it.  If a statement number is found,
		 * update the format string pointer and length.  Someday,
		 * Obi-wan, we'll do this only for static formats.
		 */

		fnum	= 0;

		while (isdigit(*fptr) && flen-- > 0)
			fnum	= (fnum * 10) + ((int) *fptr++ - ZERO);

		css->u.fmt.u.fe.fmtbuf	= fptr;
		css->u.fmt.u.fe.fmtlen	= flen;
		css->u.fmt.u.fe.fmtnum	= fnum;

		/*
		 * If the format is a variable format, or if it has not yet
		 * been parsed, or if it was parsed by an incompatible version
		 * of the format parser, then parse it.
		 */

		if (prsfmt == NULL || *prsfmt == NULL ||
		    (**prsfmt).offset != PARSER_LEVEL) { /* If not parsed */

			errn	= _parse(css, cup, prsfmt);

			if (errn != 0) {
				ERROR0(errf, errn);
			}
		}
		else	/* Use already-parsed format */
			css->u.fmt.u.fe.pfmt	= *prsfmt;

		/*
		 * Ensure that the format count stack is allocated and is
		 * large enough to accomodate the maximum nesting depth of
		 * this format.
		 */

		stsz	= (*css->u.fmt.u.fe.pfmt).rep_count;

		if (stsz > cup->upfcstsz) {

			cup->upfcstsz	= stsz;		/* Set new depth */

			if (cup->upfcstk != NULL)
				free(cup->upfcstk);	/* Free old stack */

			cup->upfcstk	= (int *) malloc(sizeof(int) * stsz);

			if (cup->upfcstk == NULL) {
				errn	= FENOMEMY;	/* No memory */
				ERROR0(errf, errn);
			}
		}

		css->u.fmt.u.fe.pftocs	= cup->upfcstk; /* Top of count stack */

		/* Skip first entry of parsed format */

		css->u.fmt.u.fe.pfcp	= css->u.fmt.u.fe.pfmt + 1;

		/* Set initial repeat count */

		*css->u.fmt.u.fe.pftocs	= css->u.fmt.u.fe.pfcp->rep_count;
	}
	else			/* Else list-directed output */
		css->u.fmt.u.le.ldwinit	= 1;

	/* Set processing functions */

	if (iotp == DIR) {

		if (cup->useq)	/* If direct attempted on seq. file */
			errn	= FEDIRTIV;	/* Direct access not allowed */
		else
			errn	= _unit_seek(cup, recn, iost);

		if (errn != 0) {
			ERROR1(errf, errn, recn);
		}

		cup->uend	= BEFORE_ENDFILE;
		cup->ulinecnt	= 0;		/* Num of characters written */
		cup->ulinemax	= 0;		/* Highwater mark */
		cup->ulineptr	= cup->ulinebuf;/* Current character position */
		css->u.fmt.endrec	= _dw_endrec;
	}
	else {

		/*
		 * The inumelt argument was not passed to the library in
		 * earlier (prior to 5.0) versions of CFT77.  The check
		 * can be removed when we no longer support those compilers.
		 * For encode statements, later compilers are passing a
		 * NULL value for inumelt.
		 */

		if (iotp == INT) {	/* If internal I/O */

			cup->ulinecnt	= 0;		/* Num chars written */
			cup->ulinemax	= 0;		/* Highwater mark */

			css->u.fmt.iiae		= 
				((_numargs() > ARGS_7) && (inumelt != NULL)) ?
				*inumelt : -1;
			css->u.fmt.endrec	= _iw_endrec;
			css->u.fmt.icp	= _fcdtocp(funit);
			css->u.fmt.icl	= _fcdlen (funit);

			/*
			 * If the size of the internal record is greater
			 * than the existing line buffer, then realloc()
			 * another one; else just decrease urecsize.
			 */
 
			if (css->u.fmt.icl > cup->urecsize) {
 
				cup->ulinebuf	= (long *) realloc(cup->ulinebuf,
							sizeof(long) *
							(css->u.fmt.icl + 1));

				if (cup->ulinebuf == NULL) { 
					errn	= FENOMEMY;	/* No memory */
					ERROR0(errf, errn);
				}
			}

			cup->urecsize	= css->u.fmt.icl;
			cup->ulineptr	= cup->ulinebuf;
		}
		else {			/* external sequential formatted I/O */

			if (cup->useq == 0) {	/* If direct access file */
				errn	= FESEQTIV; /* Sequential not allowed */
				ERROR0(errf, errn);
			}

			if (cup->uend != BEFORE_ENDFILE) {
				/*
				 * If positioned after an endfile, and the file
				 * does not support multiple endfiles, a write
				 * is invalid.
				 */
				if (!cup->umultfil && !cup->uspcproc) {
					errn	= FEWRAFEN;
					ERROR0(errf, errn);
				}

				/*
				 * If a logical endfile record had just been 
				 * read, replace it with a physical endfile 
				 * record before starting the current data 
				 * record.
				 */
				if ((cup->uend == LOGICAL_ENDFILE) &&
				    !(cup->uspcproc)) {
					struct ffsw	fst;	/* FFIO status block */

					if (XRCALL(cup->ufp.fdc, weofrtn)
					    cup->ufp.fdc, &fst) < 0) {

						errn	= fst.sw_error;

						ERROR0(errf, errn);
					}
				}
				cup->uend	= BEFORE_ENDFILE;
			}

			if (cup->pnonadv && cup->uwrt == 0) {
				register int	offset;
				/*
				 * A formatted or list-directed write statement
				 * follows a nonadvancing read.  Switch the 
				 * current line (record) from read to write 
				 * mode.  Then backspace the file so the 
				 * current record gets written back in place.
				 */

				offset		= cup->ulineptr - cup->ulinebuf;
				cup->ulinemax	= offset + cup->ulinecnt;
				cup->ulinecnt	= offset;
				cup->uflshptr	= cup->ulinebuf;

				errn	= _unit_bksp(cup);

				if (errn != 0) {
					ERROR0(errf, errn);
				}
			}
			else if (cup->pnonadv == 0) {
				/* 
				 * There is no current record (due to a prior
				 * nonadvancing read or write).  Initialize
				 * the empty line buffer.
				 */
				cup->ulinecnt	= 0;	/* Num chars written */
				cup->ulinemax	= 0;	/* Highwater mark */
				cup->ulineptr	= cup->ulinebuf;
				cup->uflshptr	= cup->ulinebuf;
			}

			/*
			 * If list-directed write and there is a current 
			 * record, then truncate the current record at the
			 * current position and flush it if the current record 
			 * is already beyond uldwsize.
			 */
			if (cup->pnonadv && (css->f_iostmt & TF_FMT) == 0) {
			 	errn	= _lw_after_nonadv(css, cup,
							cup->uldwsize, 0);
				if (errn != 0)
					goto error;
			}

			css->u.fmt.endrec	= _sw_endrec;
			cup->pnonadv 		= 0;
		}
	}

	css->u.fmt.leftablim	= cup->ulineptr;	/* set left tab limit */
	cup->uwrt		= 1;			/* Write mode */

	/* Normal return with 0 in S3 */

	return(CFT77_RETVAL(IO_OKAY));

error:
	/* Update IOSTAT variable, if specified, with error status */

	if (iostat != NULL)
		*iostat	= errn;

	if (cup != NULL)	/* If we have a unit, set status */
		cup->uflag	= cup->uflag | _UERRC;	/* Indicate error */

	/* Complete record and return */

	return(CFT77_RETVAL($WFF()));
}

/*
 *	$WFA$ - write formatted transfer
 *
 *	CALL	$WFA,(fwa, cnt, inc, typ)
 *
 *		fwa	First word address of datum (may be a Fortran
 *			character descriptor)
 *		cnt	Number of data items
 *		inc	Stride between data items
 *		typ	Type of data
 *
 *	$WFA$ calls:
 *
 *		_ld_write(), _wrfmt(), $WFF()
 */

int
$WFA$(
	_fcd	fwa,		/* Address of first word of data	*/
	long	*cnt,		/* Address of count of data items	*/
	long	*inc,		/* Address of stride between data items	*/
	long	*typ		/* Address of data type			*/
)
{
	register int	errn;		/* Error flag */
	type_packet	tip;		/* Type information packet */
	unit		*cup;		/* Pointer to unit table entry */
	void		*vaddr;		/* Data byte address */
	xfer_func	*xfunc;		/* Data transfer function */
	FIOSPTR		css;		/* Pointer to I/O state structure */

	/* Set unit table pointer */

	GET_FIOS_PTR(css);

	cup		= css->f_cu;
	tip.type77	= *typ & 017;
	tip.type90	= _f77_to_f90_type_cnvt[tip.type77];
	tip.count	= *cnt;
	tip.stride	= *inc;
	tip.intlen	= _f77_type_len[tip.type77];
	tip.extlen	= tip.intlen;
	tip.elsize	= tip.intlen;
	tip.cnvindx	= 0;

	if (tip.type77 == DT_CHAR) {
		vaddr		= _fcdtocp(fwa);
		tip.elsize	= tip.elsize * _fcdlen (fwa);
	}
	else
		vaddr		= *(void **) &fwa;

	xfunc	= (css->f_iostmt & TF_FMT) ? _wrfmt : _ld_write;
	errn	= xfunc(css, cup, vaddr, &tip, 0);

	if (errn == 0)
		return(CFT77_RETVAL(IO_OKAY));

	/* Update IOSTAT variable, if specified, with error status */

	if (cup->uiostat != NULL)
		*(cup->uiostat)	= errn;

	cup->uflag	= cup->uflag | _UERRC;	/* Indicate error */

	/* Complete record and return */

	return(CFT77_RETVAL($WFF()));
}

/*
 *	$WFF - write formatted finalization
 *
 *	CALL	$WFF,()
 *
 *	$WFF calls:
 *
 *		_wrfmt()
 */

int
$WFF(void)
{
	register int	errn;		/* Error flag */
	register long	flag;		/* Copy of cup->uflag */
	unit		*cup;		/* Pointer to unit table entry */
	FIOSPTR		css;		/* Pointer to I/O state structure */

	/* Set unit table pointer */

	GET_FIOS_PTR(css);
	cup	= css->f_cu;

	if (cup == NULL)		/* If unit not opened */
		flag	= _UERRC | _UERRF;
	else {

		/* If no error, complete processing */

		if ((cup->uflag & _UERRC) == 0) {
			xfer_func	*xfunc;

			/* If formatted I/O, ensure format complete */

			xfunc	= (css->f_iostmt & TF_FMT) ? _wrfmt : _ld_write;

			errn	= xfunc(css, cup, (void *) NULL, &__tip_null, 0);

			/* Complete record */

			if (errn == 0)
				errn	= (*css->u.fmt.endrec)(css, cup, 1);

			if (errn != 0) {

				/* Set IOSTAT variable */

				if (cup->uiostat != NULL)
					*(cup->uiostat)	= errn;

				/* Set error status */

				cup->uflag	= cup->uflag | _UERRC;
			}
		}

		/* If we allocated memory for a variable format, free it */

		if (css->u.fmt.freepfmt && css->u.fmt.u.fe.pfmt != NULL)
			free(css->u.fmt.u.fe.pfmt);

		flag	= cup->uflag;	/* Save status */
	}

	STMT_END(cup, TF_WRITE, NULL, css);	/* Unlock unit */

	/* Return proper status */

	if ((flag & _UERRC) == 0)	/* If no error */
		return(CFT77_RETVAL(IO_OKAY));
	else
		if ((flag & (_UIOSTF | _UERRF)) != 0)
			return(CFT77_RETVAL(IO_ERR));

	_ferr(css, FEINTUNK);		/* Deep weeds */
}

#endif	/* _UNICOS */

/*
 *	_dw_endrec(css, cup, count)
 *
 *		Process the end of a format or the slash edit-
 *		descriptor on a direct access write
 *
 * 		css	Current statement state pointer
 *		cup	Current unit pointer
 *		count	Count of records to write (1 if end of format else
 *			>= 1 for slash edit descriptor)
 *
 *	If no error, zero is returned.
 *	If error and user error processing is enabled, error number is returned.
 *	If error and no user error processing is enabled, _ferr() is called.
 *
 *	Calls:	_fwch()
 */
int
_dw_endrec(FIOSPTR css, unit *cup, int count)
{
	assert ( css != NULL );
	assert ( cup != NULL );
	assert ( count > 0 );

	/* Write current record */

	if (cup->ulinemax < cup->urecl) { /* If record length less then RECL */
		register int	i, j;
		long		*ptr;

		j	= cup->urecl - cup->ulinemax;
		ptr	= cup->ulinebuf + cup->ulinemax;

		/* The following loop should vectorize */

		for (i = 0; i < j; i++)
			ptr[i]	= BLANK;
	}

	if (_fwch(cup, cup->ulinebuf, cup->urecl, FULL) < 0)
		RERROR(errno);		/* Write error */

	if (count > 1) {	/* If more than one record to write */
		register int	i;

		if (cup->ulinemax > 0) { /* If the whole line isn't blank */
			long	*ptr;

			ptr	= cup->ulinebuf;

			/* The following loop should vectorize */

			for (i = 0; i <= cup->ulinemax; i++)
				ptr[i]	= BLANK;
		}

		for (i = 1; i < count; i++)
			if (_fwch(cup, cup->ulinebuf, cup->urecl, FULL) < 0)
				RERROR(errno);		/* Write failed */
	}

	cup->udalast	= cup->udalast + count;

	/* If we wrote beyond the last record, update last record */

	if (cup->udalast > cup->udamax)
		cup->udamax	= cup->udalast;

	cup->ulinecnt		= 0;
	cup->ulinemax		= 0;
	cup->ulineptr		= cup->ulinebuf;
	css->u.fmt.leftablim	= cup->ulinebuf;

	return(0);
}

/*
 *	_iw_endrec(css, cup, count)
 *
 *		Process the end of a format or a slash edit-
 *		descriptor on an internal write
 *
 * 		css	Current statement state pointer
 *		cup	Current unit pointer
 *		count	Count of records to write
 *
 *	If no error, zero is returned.
 *
 *	Calls:	_pack(), memset()
 */
int
_iw_endrec(FIOSPTR css, unit *cup, int count)
{
	register int	reclen;

	assert ( css != NULL );
	assert ( cup != NULL );
	assert ( count > 0 );

	reclen	= cup->ulinemax;

	/* If internal file is not array, cannot go to next record */

	if (css->u.fmt.iiae-- == 0)
		RERROR(FEWRIEND);	/* Internal write past end of array */

	(void) _pack(cup->ulinebuf, css->u.fmt.icp, reclen, -1);

	if (reclen < css->u.fmt.icl)
		(void) memset(css->u.fmt.icp + reclen, BLANK,
				css->u.fmt.icl - reclen);

	if (count > 1) {	/* If more than one record to write */
		register int	i;

		i	= count - 1;

		if (css->u.fmt.iiae < 0 || css->u.fmt.iiae > i) {
			css->u.fmt.iiae	= css->u.fmt.iiae - i;
			(void) memset(css->u.fmt.icp + css->u.fmt.icl, BLANK,
					css->u.fmt.icl * i);
			css->u.fmt.icp	= css->u.fmt.icp + (css->u.fmt.icl * i);
		}
		else	/* Write each record until error */
			for (i = 1; i < count; i++) {

				if (css->u.fmt.iiae-- == 0)
					RERROR(FEWRIEND); /* Write past EOF */

				css->u.fmt.icp	= css->u.fmt.icp + css->u.fmt.icl;

				(void) memset(css->u.fmt.icp, BLANK,
						css->u.fmt.icl);
			}
	}

	cup->ulinecnt		= 0;
	cup->ulinemax		= 0;
	cup->ulineptr		= cup->ulinebuf;
	css->u.fmt.leftablim	= cup->ulinebuf;
	css->u.fmt.icp		= css->u.fmt.icp + css->u.fmt.icl;

	return(0);
}

/*
 *	_sw_endrec(css, cup, count)
 *
 *		Process the end of a format or a slash edit-
 *		descriptor on a sequential write.
 *
 * 		css	Current statement state pointer
 * 		cup	Current unit pointer
 *		count	Count of records to write. 
 *
 *	If no error, zero is returned.
 *	If error and user error processing is enabled, error number is returned.
 *	If error and no user error processing is enabled, _ferr() is called.
 *
 *	Calls:	_fwch()
 */
int
_sw_endrec(FIOSPTR css, unit *cup, int count)
{
	register long	mode;
	register long	nchars;

	assert ( css != NULL );
	assert ( cup != NULL );
	assert ( count > 0 );

	mode	= css->u.fmt.nonl ? PARTIAL : FULL;
	nchars	= cup->ulinemax - (cup->uflshptr - cup->ulinebuf);

	if (_fwch(cup, cup->uflshptr, nchars, mode) < 0)
		RERROR(errno);		/* Write failed */

	if (count > 1) {	/* If more than one record to write */
		register int	i;

		for (i = 1; i < count; i++)
			if (_fwch(cup, cup->ulinebuf, 0, FULL) < 0)
				RERROR(errno);	/* Write failed */
	}

	cup->ulinecnt		= 0;
	cup->ulinemax		= 0;
	cup->ulineptr		= cup->ulinebuf;
	cup->uflshptr		= cup->ulinebuf;
	css->u.fmt.leftablim	= cup->ulineptr;
	css->u.fmt.nonl	= 0;

	return(0);
}

/*
 *	_nonadv_partrec(css, cup)
 *
 *		Process the end of a nonadvancing sequential write.
 *		The part of the line buffer between cup->uflshptr and
 *		cup->ulineptr is printed out.  If cup->ulineptr is positioned
 *		beyond the highwater mark because of a trailing TR or X edit 
 *		descriptor, print out only to the current highwater mark.
 *
 * 		css	Current statement state pointer
 * 		cup	Current unit pointer
 *
 *	If no error, zero is returned.
 *	If error and user error processing is enabled, error number is returned.
 *	If error and no user error processing is enabled, _ferr() is called.
 *
 *	Calls:	_fwch()
 */
int
_nonadv_partrec(FIOSPTR css, unit *cup)
{
	register int	nchars;
	register int	offset;

	assert ( css != NULL );
	assert ( cup != NULL );

	offset	= cup->ulineptr - cup->ulinebuf;

	if (cup->ulinemax < offset) {
		register int	i;
		register int	padcnt;
		long		*lbuff;

		/*
		 * Pad the area between ulinemax and ulineptr with blanks.
		 * The area in the line buffer beyond the highwater mark
		 * (ulinemax) would otherwise contain garbage.
		 */
		lbuff	= cup->ulinebuf + cup->ulinemax;
		nchars	= MIN(cup->ulinemax, cup->urecsize) -
			  (cup->uflshptr - cup->ulinebuf);
		padcnt	= MIN(offset, cup->urecsize) - cup->ulinemax;

		for (i = 0; i < padcnt; i++)
			lbuff[i] = BLANK;
	}
	else
		nchars	= cup->ulineptr - cup->uflshptr;

	if (_fwch(cup, cup->uflshptr, nchars, PARTIAL) < 0)
		RERROR(errno);	/* Write failed */

	cup->uflshptr	= cup->uflshptr + nchars;

	return(0);
}

/*
 *	_nonadv_endrec(css, cup)
 *
 *		Write out the "current record" at the start of REWIND,
 *		BACKSPACE, ENDFILE, or CLOSE processing when the previous
 *		operation was a a nonadvancing write.
 *
 * 		css	Current statement state pointer
 * 		cup	Current unit pointer
 *
 *	If no error, zero is returned.
 *	If error and user error processing is enabled, error number is returned.
 *	If error and no user error processing is enabled, _ferr() is called.
 *
 *	Calls:	_fwch()
 */
int
_nonadv_endrec(FIOSPTR css, unit *cup)
{
	register long	nchars;

	assert ( css != NULL );
	assert ( cup != NULL );

	nchars	= cup->ulinemax - (cup->uflshptr - cup->ulinebuf);

	if (_fwch(cup, cup->uflshptr, nchars, FULL) < 0)
		RERROR(errno);		/* Write failed */

	cup->pnonadv	= 0;

	return(0);
}

/*
 *	_lw_after_nonadv(css, cup)
 *
 *		Manage the transition from a formatted nonadvancing read or 
 *		write to a list directed write.  We blank out any part of
 *		the line buffer which will be flushed along with the 
 *		list-directed output which follows.  This blanking is needed
 *		only if a trailing TR or X edit descriptor in the prior 
 *		nonadvancing I/O statement left us positioned beyond the
 *		highwater mark in the record.
 *
 *	Calls:	_sw_endrec()
 */
int
_lw_after_nonadv(FIOSPTR css, unit *cup, int linelimit, int namelist)
{
	register int	errn;
	
	assert ( css != NULL );
	assert ( cup != NULL );

	if (_newrec_listio_after_nonadvancing && !namelist)
		errn	= _sw_endrec(css, cup, 1);
	else {
		register int	nchars;

		nchars	= cup->ulineptr - cup->ulinebuf;

		if (nchars > cup->urecsize)
			errn	= FEWRLONG;
		else {
			if (nchars > cup->ulinemax) {
				register int	i;
				register int	lmax;
				register int	nblanks;

				nblanks = nchars - cup->ulinemax;
				lmax	= cup->ulinemax;

				for (i = 0; i < nblanks; i++)
					cup->ulinebuf[lmax + i] = BLANK;

			}

			cup->ulinemax	= nchars;
			errn		= 0;

			if (cup->ulinemax > linelimit)
				errn	= _sw_endrec(css, cup, 1);
		}
	}

	return (errn);
}

/*
 *	_wf_setup
 *
 *		Access the LISTIO_AFTER_NONADVANCING environment variable to 
 *		establish what happens when a list-directed output statement
 *		follows a nonadvancing formatted READ or WRITE statment.
 *
 *		Access the ZERO_WIDTH_PRECISION environment variable to
 *		establish what happens when a zero-width format is used
 *		for floating-point output.
 *
 *		Access the FORMAT_TYPE_CHECKING environment variable to
 *		establish the conformance rules for data/format checking.
 *
 *		NEWREC	Cause the current record to be flushed to the file
 *			at the start of list-directed write processing.
 *		CURPOS	Cause the list-directed write processing to continue
 *			at the current position in the current record.
 *
 *		This function is called by _initialize_fortran_io()
 *
 *	Calls:	getenv(), memcpy(), strcmp().
 */
void
_wf_setup(void)
{
	register short		i;
	register signed char	d4, d8, d16;
	char			*str;

/*
 *	Flush of current rec before list directed write is default for
 *	pre 2.0 CrayLibs.  No flushing is default for CrayLibs 2.0 and higher.
 */
	_newrec_listio_after_nonadvancing = (_CRAYLIBS_RELEASE < 2000) ? 1 : 0;

	str	= getenv("LISTIO_AFTER_NONADVANCING");

	if (str != NULL) {
		if (strcmp(str, "NEWREC") == 0) 
			_newrec_listio_after_nonadvancing = 1;
		else if (strcmp(str, "CURPOS") == 0)
			_newrec_listio_after_nonadvancing = 0;
	}

/*
 *	Set default width for zero-width formats.  The user can alter
 *	these values via an environment variable, so they must be set
 *	at runtime.
 */

	/* Assume default (full) precision */

#ifdef	_F_REAL4
	d4	= DREAL4;
#else
	d4	= -1;
#endif
	d8	= DREAL8;
	d16	= DREAL16;

	str	= getenv("ZERO_WIDTH_PRECISION");

	if (str != NULL) {
		if (strcmp(str, "PRECISION") == 0) {
#ifdef	_F_REAL4
			d4	= DREAL4_P;
#endif
			d8	= DREAL8_P;
			d16	= DREAL16_P;
		}
		else if (strcmp(str, "HALF") == 0) {
#ifdef	_F_REAL4
			d4	= (d4 + 1) >> 1;
#endif
			d8	= (d8 + 1) >> 1;
			d16	= (d16 + 1) >> 1;
		}
	}

	for (i = D_ED; i <= G_ED; i++) {
		_rw_mxdgt[i-1][4-1]	= d4;
		_rw_mxdgt[i-1][8-1]	= d8;
		_rw_mxdgt[i-1][16-1]	= d16;
	}

/*
 *	Set conformance rules for data/format checking.  The user can select
 *	an alternate set of rules via an environment variable, so they must
 *	be set at runtime.
 */

	str	= getenv("FORMAT_TYPE_CHECKING");

	if (str != NULL) {
		register int	sz;

		sz	= sizeof(fmtchk_t) * DVTYPE_ASCII;

		if (strcmp(str, "RELAXED") == 0) {
			(void) memcpy( (void *) _RCHK, (void *)_RNOCHK, sz);
			(void) memcpy( (void *) _WCHK, (void *)_WNOCHK, sz);
		}
		else if (strcmp(str, "STRICT77") == 0) {
			(void) memcpy( (void *) _RCHK, (void *)_RCHK77, sz);
			(void) memcpy( (void *) _WCHK, (void *)_WCHK77, sz);
		}
		else if (strcmp(str, "STRICT90") == 0 ||
			 strcmp(str, "STRICT95") == 0) {
			(void) memcpy( (void *) _RCHK, (void *)_RCHK90, sz);
			(void) memcpy( (void *) _WCHK, (void *)_WCHK90, sz);
		}
	}

	return;
}
