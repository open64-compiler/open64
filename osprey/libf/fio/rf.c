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



#pragma ident "@(#) libf/fio/rf.c	92.5	09/07/99 15:26:57"

#include <ctype.h>
#include <errno.h>
#include <liberrno.h>
#include <fortran.h>
#if defined(BUILD_OS_DARWIN)
#include <stdlib.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <malloc.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <cray/fmtconv.h>
#include <cray/format.h>
#include <cray/nassert.h>
#ifndef	_ABSOFT
#include <sys/unistd.h>
#endif
#include "fio.h"
#include "fmt.h"
#include "fstats.h"
#include "f90io.h"
#ifdef	_CRAYMPP
#include <stdarg.h>
#endif

#ifdef	_UNICOS

#pragma	_CRI duplicate $RFI  as $RLI
#pragma	_CRI duplicate $RFA$ as $RLA$
#pragma	_CRI duplicate $RFA$ as $DFA$
#pragma	_CRI duplicate $RFF  as $RLF
#pragma	_CRI duplicate $RFF  as $DFF

/* Define macros to convert _numargs() to number of arguments */
#define ARGS_6	(4 + 2*sizeof(_fcd)/sizeof(long))
#define ARGS_7	(5 + 2*sizeof(_fcd)/sizeof(long))
#define ARGS_8	(6 + 2*sizeof(_fcd)/sizeof(long))
#define ARGS_9	(7 + 2*sizeof(_fcd)/sizeof(long))

#define ZERO	((int) '0')

int	$RFF(void);

#define	ERROR0(cond, n) {		\
	if (!(cond))			\
		_ferr(css, (n));	\
	else				\
		goto error;		\
}

#define	ERROR1(cond, n, p) {		\
	if (!(cond))			\
		_ferr(css, (n), p);	\
	else				\
		goto error;		\
}

/*
 *	Here we do some things for upward compatibility with CFT77 5.0.2.
 */
#define IS_PFORM_BROKEN	(_numargs() < ARGS_9)	/* true if pform is broken */

/*
 *	$RFI - read formatted initialization
 *
 *	CALL	$RFI,(funit, format, err, end, iostat, rec, pform, inumelt,
 *			inumcfe)
 *
 *		funit	Address of Fortran unit designator (integer unit
 *			number for external I/O or Fortran character
 *			descriptor (FCD) for internal I/O)
 *		format	Address of format (Fortran character descriptor or
 *			hollerith); NULL if list-directed
 *		err	Address of error address (ERR=label)
 *		end	Address of end address (END=label)
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
 *	$RFI calls:
 *
 *		_imp_open77(), _unit_seek(), _parse(), _ferr(), $RFF()
 */

#ifdef	_CRAYMPP
int
$RFI( 
_fcd		funit,		/* Address of unit number or FCD	*/
...
)
#else
int
$RFI(
_fcd		funit,		/* Address of unit number or FCD	*/
_fcd		format,		/* Address of format (FCD or hollerith)	*/
long		*err,		/* Address of error processing address	*/
long		*end,		/* Address of end processing address	*/
_f_int		*iostat,	/* Address of IOSTAT variable		*/
_f_int		*rec,		/* Address of direct access record no.	*/
fmt_type	**pform,	/* Address of address of parsed format	*/
long		*inumelt,	/* Address of int. array element count	*/
long		*inumcfe	/* Address of number of format elements	*/
)
#endif
{
	register int	endf;		/* END processing flag	*/
	register int	errf;		/* ERR processing flag	*/
	register int	errn;		/* Error number		*/
	register int	iost;		/* I/O statement type	*/
	register int	iotp;		/* I/O type		*/
	register recn_t	recn;		/* Record number	*/
	register unum_t	unum;		/* Unit number		*/
	fmt_type	**prsfmt;	/* Parsed format info.	*/
	unit		*cup;		/* Unit table pointer	*/
	FIOSPTR		css;		/* I/O statement state	*/
#ifdef	_CRAYMPP
	va_list	args;
	_fcd	format;		/* Address of format (FCD or hollerith)	*/
	long	*err;		/* Address of error processing address	*/
	long	*end;		/* Address of end processing address	*/
	_f_int	*iostat;	/* Address of IOSTAT variable		*/
	_f_int	*rec;		/* Address of direct access record no.	*/
	fmt_type **pform;	/* Address of address of parsed format	*/
	long	*inumelt;	/* Address of int. array element count	*/
	long	*inumcfe;	/* Address of number of format elements	*/
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
	endf	= ((end != NULL) || (iostat != NULL));

	/* Check if formatted or list-directed	*/

	iost	= (_fcdtocp(format) != NULL) ? T_RSF : T_RLIST;
	iotp	= SEQ;				/* Assume sequential */

	/* Check if we're doing internal I/O or external I/O */

	if (_fcdlen(funit) > 0) {		/* If internal I/O */
		iotp	= INT;
		STMT_BEGIN(-1, 1, iost, NULL, css, cup);
	}
	else {				/* Else external I/O */
		unum	= **(_f_int **) &funit;

		if (rec != NULL) {	/* If direct access */
			iost	= T_RDF;	/* Set direct formatted read */
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

	cup->uflag	= (err    != NULL ?  _UERRF : 0) |
			  (end    != NULL ?  _UENDF : 0) |
			  (iostat != NULL ? _UIOSTF : 0);
	cup->uiostat	= iostat;

	if (iotp != INT) {		/* If not internal I/O */

		/* If trying to read a file without read permission */

		if ((cup->uaction & OS_READ) == 0) {
			errn	= FENOREAD;	/* No read permission */
			ERROR0(errf, errn);
		}

		/* If attempting formatted I/O on an unformatted file */

		if (!cup->ufmt) {
			errn	= FEFMTTIV;	/* Formatted not allowed */
			ERROR0(errf, errn);
		}

		/* If sequential and writing, disallow read after write */

		if (cup->useq && cup->uwrt != 0) {
			errn	= FERDAFWR;	/* Read after write */
			ERROR0(errf, errn);
		}
	}

	/* Preset fields in unit table */

	cup->uwrt	= 0;

	/* Initialize fields in the Fortran statement state structure */

	css->u.fmt.icp		= NULL;
	css->u.fmt.blank0	= cup->ublnk;
	css->u.fmt.lcomma	= 0;
	css->u.fmt.slash	= 0;
	css->u.fmt.freepfmt	= 0;
#ifdef	_CRAYMPP
	css->f_shrdput		= 0;
#endif

	if (_fcdtocp(format) != NULL) {	/* If not list-directed input */
		char	*fptr;
		int	flen;
		int	fnum;
		int	stsz;

		/*
		 * Ensure that fmtbuf is initialized in case _ferr() is called.
		 */
		css->u.fmt.u.fe.fmtbuf	= NULL;
		css->u.fmt.u.fe.fmtnum	= 0;
		css->u.fmt.u.fe.fmtcol	= 0;
		css->u.fmt.u.fe.scale	= 0;
		css->u.fmt.u.fe.charcnt	= 0;

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
		else
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

	/* Set processing functions */

	if (iotp == DIR) {

		if (cup->useq)	/* If direct attempted on seq. file */
			errn	= FEDIRTIV;	/* Direct access not allowed */
		else
			errn	= _unit_seek(cup, recn, iost);

		if (errn != 0) {
			ERROR1(errf, errn, recn);
		}

		css->u.fmt.endrec	= _dr_endrec;
	}
	else {

		if (cup->useq == 0) {	/* If seq. attempted on direct file */
			errn	= FESEQTIV;	/* Sequential not allowed */
			ERROR0(errf, errn);
		}

		/*
		 * The inumelt argument was not passed to the library in
		 * earlier (prior to 5.0) versions of CFT77.  The check
		 * can be removed when we no longer support those compilers.
		 * For decode statements, later compilers are passing a
		 * NULL value for inumelt.
		 */

		if (iotp == INT) {	/* If internal I/O */

			css->u.fmt.iiae	= ((_numargs() > ARGS_7) &&
					   (inumelt != NULL)) ? *inumelt : -1;
			css->u.fmt.endrec	= _ir_endrec;
			css->u.fmt.icp	= _fcdtocp(funit);
			css->u.fmt.icl	= _fcdlen (funit);

			/*
			 * If the size of the internal record is greater
			 * than the existing line buffer, then realloc()
			 * another one; else just decrease urecsize.
			 */
 
			if (css->u.fmt.icl > cup->urecsize) {

				cup->ulinebuf	= (long*) realloc(cup->ulinebuf,
							sizeof(long) *
							(css->u.fmt.icl + 1));

				if (cup->ulinebuf == NULL) { 
					errn	= FENOMEMY;	/* No memory */
					ERROR0(errf, errn);
				}
			}

			cup->urecsize	= css->u.fmt.icl;
		}
		else		/* External sequential formatted I/O */
			css->u.fmt.endrec	= _sr_endrec;
	}

	if (cup->pnonadv == 0) {	/* if previous ADVNACE='YES' */
		errn	= (*css->u.fmt.endrec)(css, cup, 1); /* Read a record */
	}
	else {				/* else previous ADVANCE='NO' */
		css->u.fmt.leftablim	= cup->ulineptr; /* set left tab limit */
	}
 
	if (errn != 0)
		if (errn < 0 ) {
			ERROR0(endf, errn);
		}
		else {
			ERROR0(errf, errn);
		}

	cup->pnonadv	= 0;

	/* normal return with 0 in S3 */

	return(CFT77_RETVAL(IO_OKAY));

error:
	/* Update IOSTAT variable, if specified, with error status */

	if (iostat != NULL)
		*iostat	= errn;

	if (cup != NULL) 	/* If we have a unit, set status */
		cup->uflag	|= (errn < 0) ? _UENDC : _UERRC;

	/* Complete record and return */

	return(CFT77_RETVAL($RFF()));
}

/*
 *	$RFA$ - read formatted transfer
 *
 *	CALL	$RFA,(fwa, cnt, inc, typ)
 *
 *		fwa	First word address of datum (may be a Fortran
 *			character descriptor)
 *		cnt	Number of data items
 *		inc	Stride between data items
 *		typ	Type of data
 *
 *	$RFA$ calls:
 *
 *		_ld_read(), _rdfmt(), $RFF()
 */

int
$RFA$(
	_fcd	fwa,		/* Address of first word of data	*/
	long	*cnt,		/* Address of count of data items	*/
	long	*inc,		/* Address of stride between data items	*/
	long	*typ		/* Address of data type			*/
)
{
	register int	errn;		/* Error number */
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
		tip.elsize	= tip.elsize * _fcdlen(fwa);
	}
	else
		vaddr	= *(void **) &fwa;

	xfunc	= (css->f_iostmt & TF_FMT) ? _rdfmt : _ld_read;
	errn	= xfunc(css, cup, vaddr, &tip, 0);

	if (errn == 0)
		return(CFT77_RETVAL(IO_OKAY));

	/* Update IOSTAT variable, if specified, with error status */

	if (cup->uiostat != NULL)
		*(cup->uiostat)	= errn;

	/* Set error or EOF status */

	cup->uflag	|= (errn < 0) ? _UENDC : _UERRC;

	/* Complete record and return */

	return(CFT77_RETVAL($RFF()));
}

/*
 *	$RFF - read formatted finalization
 *
 *	CALL	$RFF,()
 *
 *	$RFF calls:
 *
 *		_rdfmt()
 */

int
$RFF(void)
{
	register int	errn;		/* Error number */
	register long	flag;		/* Error flag */
	unit		*cup;		/* Pointer to unit table entry */
	FIOSPTR		css;		/* Pointer to I/O state structure */

	/* Set unit table pointer */

	GET_FIOS_PTR(css);

	cup	= css->f_cu;

	if (cup == NULL)		/* If unit not opened */
		flag	= _UERRC | _UERRF;
		/* NB: You can't get an EOF error without a cup pointer */
	else {

		/* If formatted I/O and no error/EOF, complete processing */

#ifdef	_CRAYMPP
		if (css->f_shrdput) {
			css->f_shrdput	= 0;
			_remote_write_barrier();
		}
#endif
		if ((css->f_iostmt & TF_FMT) &&		/* If formatted and... */
		   (cup->uflag & (_UERRC | _UENDC)) == 0) { /* If no ERR/EOF */

			/* Complete format */
			errn	= _rdfmt(css, cup, (void *) NULL, &__tip_null,
					0);

			if (errn != 0) {

				/* Set IOSTAT variable */

				if (cup->uiostat != NULL)
					*(cup->uiostat)	= errn;

				/* Set error status */

				cup->uflag	|= (errn > 0) ? _UERRC : _UENDC;
			}
		}

		/* If we allocated memory for a variable format, free it */

		if (css->u.fmt.freepfmt && css->u.fmt.u.fe.pfmt != NULL)
			free(css->u.fmt.u.fe.pfmt);

		flag	= cup->uflag;	/* Save status */
	}

	STMT_END(cup, TF_READ, NULL, css);	/* Unlock unit */

	/* Return proper status */

	if ((flag & (_UERRC | _UENDC)) == 0)	/* If no error or EOF */
		return(CFT77_RETVAL(IO_OKAY));
	else
		if ((flag & _UERRC) != 0) {	/* If error */

			if ((flag & (_UIOSTF | _UERRF)) != 0)
				return(CFT77_RETVAL(IO_ERR));
		}
		else				/* Else EOF */
			if ((flag & (_UIOSTF | _UENDF)) != 0)
				return(CFT77_RETVAL(IO_END));

	_ferr(css, FEINTUNK);		/* Deep weeds */
}

#endif	/* _UNICOS */

/*
 *	_dr_endrec(css, cup, count)
 *
 *		Process the end of a format or the slash edit
 *		descriptor on a direct access read
 *
 *		css	Current statement state pointer
 *		cup	Current unit pointer
 *		count	Count of records to read (1 if end of format else
 *			>= 1 for slash edit descriptor)
 *
 *	If no error or end of file, zero is returned.
 *	If error and user error processing is enabled, error number is returned.
 *	If error and no user error processing is enabled, _ferr() is called.
 *	If EOF and user end processing is enabled, -(EOF number) is returned.
 *	If EOF and no user end processing is enabled, _ferr() is called.
 *
 *	Calls:	_frch()
 */
int
_dr_endrec(FIOSPTR css, unit *cup, int count)
{
	register int	i;
	register int	length;
	long		stat;

	assert ( css != NULL );
	assert ( cup != NULL );
	assert ( count > 0 );

	cup->udalast	= cup->udalast + count;
	length		= 0;

	if (cup->udalast > cup->udamax)	/* If trying to read nonexistent rec. */
		RERROR1(FENORECN, cup->udalast);

	for (i = 0; i < count; i++) {	/* For each record to be read... */

		length	= _frch(cup, cup->ulinebuf, cup->urecsize, FULL, &stat);
	
		switch (stat) {

			case EOR:	/* Normal case */
				if (length != cup->urecsize) {
					/* Should be an error */
				}
				break;

			case EOF:	/* End of file */
			case EOD:	/* End of data */
				/*
				 * It's possible that the check against
				 * udamax above will prevent this from
				 * ever occurring; in which case this
				 * path is never taken.
				 */
				RERROR1(FENORECN, cup->udalast);

			case CNT:	/* Malformed record */
				/*
				 * In full record mode, the only way we
				 * can get a CNT status back is if the
				 * record is malformed (e.g., missing
				 * EOR).
				 */
				RERROR(FERDMALR);

			default:	/* Read error */ 
				RERROR(errno);

		} /* switch */
	} /* for */

	cup->ulinecnt		= length;
	cup->ulineptr		= cup->ulinebuf;
	css->u.fmt.leftablim	= cup->ulinebuf;

	return(0);
}

/*
 *	_ir_endrec(css, cup, count)
 *
 *		Process the end of a format or the slash edit
 *		descriptor on an internal read
 *
 *		css	Current statement state pointer
 *		cup	Current unit pointer
 *		count	Count of records to read (1 if end of format else
 *			>= 1 for slash edit descriptor)
 *
 *	If no error or end of file, zero is returned.
 *	If error and user error processing is enabled, error number is returned.
 *	If error and no user error processing is enabled, _ferr() is called.
 *	If EOF and user end processing is enabled, -(EOF number) is returned.
 *	If EOF and no user end processing is enabled, _ferr() is called.
 *
 *	Calls:	_unpack()
 */
int
_ir_endrec(FIOSPTR css, unit *cup, int count)
{
	register int	i;

	assert ( css != NULL );
	assert ( cup != NULL );
	assert ( count > 0 );

	for (i = 0; i < count; i++) {	/* For each record to be read... */

		if (css->u.fmt.iiae-- == 0)
			REND(FERDIEOF);	/* Read past end of internal array */

		/* Skip all but last record */

		if (i != (count - 1))	/* If not last record */
			css->u.fmt.icp	= css->u.fmt.icp + cup->urecsize;
		else
			(void) _unpack(css->u.fmt.icp, cup->ulinebuf,
					css->u.fmt.icl, -1);

	} /* for */

	css->u.fmt.icp		= css->u.fmt.icp + css->u.fmt.icl;
	cup->ulinecnt		= css->u.fmt.icl;
	cup->ulineptr		= cup->ulinebuf;
	css->u.fmt.leftablim	= cup->ulinebuf;

	return(0);
}

/*
 *	_sr_endrec(css, cup, count)
 *
 *		Process the end of a format or the slash edit
 *		descriptor on a sequential read
 *
 *		css	Current statement state pointer
 *		cup	Current unit pointer
 *		count	Count of records to read (1 if end of format else
 *			>= 1 for slash edit descriptor)
 *
 *	If no error or end of file, zero is returned.
 *	If error and user error processing is enabled, error number is returned.
 *	If error and no user error processing is enabled, _ferr() is called.
 *	If EOF and user end processing is enabled, -(EOF number) is returned.
 *	If EOF and no user end processing is enabled, _ferr() is called.
 *
 *	Calls:	_frch()
 */
int
_sr_endrec(FIOSPTR css, unit *cup, int count)
{
	register int	eofstat;
	register long	length;
	register long	offset;
	long		stat;

	assert ( css != NULL );
	assert ( cup != NULL );
	assert ( count > 0 );

	cup->uend	= BEFORE_ENDFILE;

	while (count > 1) {	/* Skip all but last record */
		long		tbuf[2];	/* Dummy buffer */

		length	= _frch(cup, tbuf, 1, FULL, &stat);

		if (length == IOERR)
			RERROR(errno);

		switch (stat) {

			case EOR:	/* Normal case */
			case CNT:	/* Malformed record (no newline) */
				break;

			case EOF:	/* End of file */
				cup->uend	= PHYSICAL_ENDFILE;
				REND(FERDPEOF);

			case EOD:	/* End of data */
				if (cup->uend == BEFORE_ENDFILE) {
					cup->uend	= LOGICAL_ENDFILE;
					eofstat		= FERDPEOF;
				}
				else
					eofstat		= FERDENDR;

				REND(eofstat);

			default:	/* Read error */
				RERROR(errno);

		} /* switch */

		count	= count - 1;
	}

	offset	= 0;

	do {	/* Read last record */

		length	= _frch(cup, cup->ulinebuf + offset,
				cup->urecsize - offset, PARTIAL, &stat);

		if (length == IOERR)
			RERROR(errno);

		switch (stat) {
			register long	tlen;
			long		*tptr;

			case EOR:	/* Normal case */
				break;

			case EOF:	/* End of file */
				if (offset > 0)	/* Premature EOF */
					break;

				cup->uend	= PHYSICAL_ENDFILE;
				REND(FERDPEOF);

			case EOD:	/* End of data */
				if (offset > 0)	/* Premature EOD */
					break;

				if (cup->uend == BEFORE_ENDFILE) {
					cup->uend	= LOGICAL_ENDFILE;
					eofstat		= FERDPEOF;
				}
				else
					eofstat		= FERDENDR;

				REND(eofstat);

			case CNT:	/* Partial record */
				/*
				 * The record didn't fit into the line buffer,
				 * so we increase the size of the line buffer
				 * and try reading the rest of the record.
				 *
				 * Basically, we double the size of the line
				 * buffer on each iteration except that when
				 * we get above a million words, we ensure
				 * that the size of the memory request is a
				 * multiple of a megabyte (for purposes of
				 * memory alignment and allocation).
				 *
				 * Note that we have a one-word pad at the
				 * end of the line buffer.
				 */

#define	MB	01000000L	/* A million */

				if (length != (cup->urecsize - offset)) {
					/*
					 * We got a short count.  Most
					 * likely, a missing newline on
					 * the last record of the file.
					 * Treat it is as an EOR.
					 */
					stat	= EOR;
					break;
				}

				offset	= cup->urecsize;
				tlen	= offset;

				if (tlen >= (MB - 1))
					tlen	= (((tlen + 1) << 1) &
						   ~(MB - 1)) - 1;
				else {
					tlen	= tlen << 1;

					if (tlen > MB)
						tlen	= MB - 1;
				}

				if (tlen < offset)	/* Oops, overflow! */
					RERROR(FERDMEMY);

				tptr	= realloc(cup->ulinebuf, sizeof(long) *
							 (tlen + 1));

				if (tptr == (long *) NULL)	/* No memory */
					RERROR(FERDMEMY);

				cup->ulinebuf	= tptr;
				cup->urecsize	= tlen;

				break;

			default:	/* Read error */ 
				RERROR(errno);

		} /* switch */
	} while (stat == CNT);

	cup->uend		= BEFORE_ENDFILE;
	cup->ulinecnt		= length + offset;
	cup->ulineptr		= cup->ulinebuf;
	css->u.fmt.leftablim	= cup->ulinebuf;

	return(0);
}
