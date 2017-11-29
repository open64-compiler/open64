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



#pragma ident "@(#) libf/fio/wu.c	92.3	10/29/99 21:39:27"

#include <errno.h>
#include <liberrno.h>
#include <fortran.h>
#include "fio.h"
#ifdef	_CRAYMPP
#include <stdarg.h>
#endif
/* for malloc and memset prototype */
#ifdef	_LITTLE_ENDIAN
#include <stdlib.h>	
#include <string.h>	
#endif

#ifdef	_UNICOS
#pragma	_CRI duplicate _WUI as $WUI
#pragma	_CRI duplicate _WUF as $WUF
#endif

int	_WUF();

#define	ERROR0(cond, n) {	\
	if (!(cond))		\
		_ferr(css, (n));\
	else			\
		goto error;	\
}

#define	ERROR1(cond, n, p) {	\
	if (!(cond))		\
		_ferr(css, (n), p);\
	else			\
		goto error;	\
}

/*
 *	$WUI - write unformatted initialization
 *
 *	CALL	$WUI,(funit, _arg2, err, _arg4, iostat, rec)
 *
 *		funit	Address of Fortran unit designator (integer unit
 *			number)
 *		_arg2	Unused
 *		err	Address of error address (ERR=label)
 *		_arg4	Unused
 *		iostat	Address of I/O status variable (integer variable)
 *		rec	Address of integer record number (NULL implies
 *			sequential I/O)
 *
 *	$WUI calls:
 *
 *		_imp_open77(), _ferr(), _unit_seek()
 */

#ifdef	_CRAYMPP
int
_WUI(
_fcd		funit,		/* Address of unit number		*/
...)
#else
int
_WUI(
_fcd		funit,		/* Address of unit number		*/
_fcd		_arg2,		/* Unused				*/
long		*err,		/* Address of error processing address	*/
long		*_arg4,		/* Unused				*/
_f_int		*iostat,	/* Address of IOSTAT variable		*/
_f_int		*rec		/* Address of direct access record no.	*/
#ifndef _UNICOS
,FIOSPTR	cssa		/* Statement state structure */
#endif
)
#endif
{
	register int	errf;		/* Error processing flag	*/
	register int 	errn;		/* Error number			*/
	register int	iost;		/* I/O statement type		*/
	register int	iotp;		/* I/O type			*/
	register recn_t	recn;		/* Direct access record number	*/
	register unum_t	unum;		/* Actual unit number		*/
	unit		*cup;		/* Pointer to unit table entry	*/
	FIOSPTR		css;		/* Pointer to statement state	*/
#ifdef	_CRAYMPP
	va_list		args;
	_fcd		_arg2;		/* Unused			*/
	long		*err;		/* Address of error processing address*/
	long		*_arg4;		/* Unused			*/
	_f_int		*iostat;	/* Address of IOSTAT variable	*/
	_f_int		*rec;		/* Address of direct access record no.*/
#endif

#ifdef	_UNICOS
	GET_FIOS_PTR(css);

	/* Check if recursive triple-call I/O */

	if (css->f_iostmt != 0)
		_ferr(css, FEIOACTV);
#else
	css	= cssa;
#endif

#ifdef	_CRAYMPP
	va_start(args,funit);
	_arg2	= va_arg(args,_fcd);
	err	= va_arg(args, long *);
	_arg4	= va_arg(args, long *);
	iostat	= va_arg(args, _f_int *);
	rec	= va_arg(args, _f_int *);
	va_end(args);
#endif

	errn	= 0;
	unum	= **(_f_int **)&funit;

	/* Establish error processing options */

	if (iostat != NULL)
		*iostat	= 0;		/* Clear IOSTAT variable, if extant */

	errf	= ((err != NULL) || (iostat != NULL));
	iost	= T_WSU;
	iotp	= SEQ;			/* Assume sequential */

	if (rec != NULL) {	/* If direct access */
		iost	= T_WDU;	/* Set direct unformatted write */
		iotp	= DIR;
		recn	= *rec;
	}

	STMT_BEGIN(unum, 0, iost, NULL, css, cup);

	if (cup == NULL) {		/* If not connected */
		int	stat;

		cup	= _imp_open77(css, iotp, UNF, unum, errf, &stat);

		if (cup == NULL) {
			errn	= stat;
			goto error;
		}
	}

	/* Record error processing options in the unit */

	cup->uiostat	= iostat;
	cup->uflag	= (iostat != NULL ? _UIOSTF : 0) |
			  (   err != NULL ?  _UERRF : 0);

	/* Perform error checking */

	if (cup->ufs == FS_AUX) {
		errn	= FEMIXAUX;	/* Can't mix auxiliary and Fortran I/O */
		ERROR0(errf, errn);
	}

	if ((cup->uaction & OS_WRITE) == 0) {
		errn	= FENOWRIT;	/* No write permission */
		ERROR0(errf, errn);
	}

	if (cup->ufmt) {	/* If unformatted attempted on formatted file */
		errn	= FEUNFMIV;		/* Unformatted not allowed */
		ERROR0(errf, errn);
	}

	/* Preset fields in unit table */

	cup->uwrt	= 1;
	cup->ulastyp	= DVTYPE_TYPELESS;

	if (iotp == DIR) {	/* If direct access */

		if (cup->useq)		/* If direct attempted on seq. file */
			errn	= FEDIRTIV;	/* Direct access not allowed */
		else
			errn	= _unit_seek(cup, recn, iost);

		if (errn != 0)
			ERROR1(errf, errn, recn);

		if (cup->udalast > cup->udamax)	/* If new highwater mark */
			cup->udamax	= cup->udalast;

		cup->uend	= BEFORE_ENDFILE;
	}
	else {		/* Else sequential access */

		if (cup->useq == 0) {	/* If seq. attempted on direct file */
			errn	= FESEQTIV;	/* Sequential not allowed */
			ERROR0(errf, errn);
		}

		if (cup->uend != BEFORE_ENDFILE) {
			struct ffsw	fst;	/* FFIO status block */
			/*
			 * If positioned after an endfile, and the file does not
			 * support multiple endfiles, a write is invalid.
			 */
			if (!cup->umultfil && !cup->uspcproc) {
				errn	= FEWRAFEN;	/* Write after endfile */
				ERROR0(errf, errn);
			}
			/*
			 * If a logical endfile record had just been read,
			 * replace it with a physical endfile record before
			 * starting the current data record.
			 */
			if ((cup->uend == LOGICAL_ENDFILE) && !(cup->uspcproc)) {
				if (XRCALL(cup->ufp.fdc, weofrtn)cup->ufp.fdc, &fst) < 0) {
					errn	= fst.sw_error;
					ERROR0(errf, errn);
				}
			}

			cup->uend	= BEFORE_ENDFILE;
		}

#if	PURE_ENABLED
		if (cup->upure && cup->upuretype != P_RDWR) {
			/*
			 * Set the upuretype field to P_RDWR mode unless it has
			 * previously been set to P_BUFIO by a BUFFER IN/OUT
			 * statement.  This check prevents the intermixing of
			 * READ/WRITE I/O with BUFFER IN/BUFFER OUT I/O when
			 * '-s pure' is assigned.
			 */
			if (cup->upuretype == P_BUFIO)
				errn	= FEMIXBUF;
			else
				cup->upuretype	= P_RDWR;
		}
#endif

	}

	if (errn != 0)
		ERROR0(errf, errn);

	return(CFT77_RETVAL(IO_OKAY));

error:
	if (iostat != NULL)
		*iostat	= errn;		/* Set IOSTAT variable to error */

	if (cup != NULL)		/* If we have a unit */
		cup->uflag	= cup->uflag | _UERRC;	/* Indicate error */

#ifdef	_UNICOS
	return(CFT77_RETVAL(_WUF()));
#else
	return(CFT77_RETVAL(_WUF(css)));
#endif
}

#ifdef	_UNICOS
/*
 *	Definition of inlined function _inline_wrunf()
 */
#define INLINE
#include "wrunf.c"

/*
 *	$WUA$ - write unformatted transfer
 *
 *	CALL	$WUA$,(fwa, count, inc, type)
 *
 *		fwa	First word address of datum (may be a Fortran
 *			character descriptor)
 *		count 	Pointer to number of data items
 *		stride 	Pointer to stride between data items
 *		type 	Pointer to type of data
 *
 *	$WUA$ calls:
 *
 *		_ferr(), _wrunf(), _WUF()
 */

int
$WUA$(
	_fcd	fwa,		/* Address of first word of data	*/
	long	*count,		/* Address of count of data items	*/
	long	*stride,	/* Address of stride between data items	*/
	long	*type		/* Address of data type			*/
)
{
	register short	type77;		/* Fortran 77 data type */
	register int	errn;		/* Error number */
	type_packet	tip;		/* Type information packet */
	struct f90_type	ts;		/* F90 type structure */
	void		*dptr;
	unit		*cup;		/* Pointer to unit table entry	*/
	FIOSPTR		css;		/* Pointer to statement state	*/

	GET_FIOS_PTR(css);

	cup	= css->f_cu;
	type77	= *type & 017;

	CREATE_F90_INFO(ts, tip, type77);

	tip.count	= *count;
	tip.stride	= *stride;

	if (type77 == DT_CHAR) {
		dptr		= (void *) _fcdtocp(fwa);
		tip.elsize	= tip.elsize * _fcdlen(fwa);
	}
	else
		dptr		= *(void **)&fwa;

#if	NUMERIC_DATA_CONVERSION_ENABLED

	if (cup->unumcvrt || cup->ucharset) {

		errn	= _get_dc_param(css, cup, ts, &tip);

		if (errn != 0)
			goto error;
	}
#endif

#pragma _CRI inline _inline_wrunf
	errn	= _inline_wrunf(css, cup, dptr, &tip, PARTIAL);

	if (errn == 0)
		return(CFT77_RETVAL(IO_OKAY));

error:
	if (cup->uiostat != NULL)
		*(cup->uiostat)	= errn;

	cup->uflag	= cup->uflag | _UERRC;		/* Indicate error */

	if ((cup->uflag & (_UIOSTF | _UERRF)) != 0) 
		return(CFT77_RETVAL(_WUF()));

	_ferr(css, FEINTUNK);		/* Deep weeds */
}

#endif	/* _UNICOS */

/*
 *	_WUF - write unformatted finalization
 *
 *	CALL	_WUF,()
 *
 *	_WUF calls:
 *
 *		_ferr(), _fwwd(), fwrite()
 */

int
_WUF(
#ifndef _UNICOS
FIOSPTR		cssa		/* Statement state structure */
#endif
)
{
	register int	errn;		/* Error number			*/
	register long	flag;		/* Error flag			*/
	unit		*cup;		/* Pointer to unit table entry	*/
	FIOSPTR		css;		/* Pointer to statement state	*/

#ifdef	_UNICOS
	GET_FIOS_PTR(css);
#else
	css	= cssa;
#endif

	errn	= 0;
	cup	= css->f_cu;

	if (cup == NULL) {		/* If unit not opened */
		/*
		 * If unit not connected, assume we are catching errors with
		 * ERR= or IOSTAT= and that _WUF is being called from $WUI or
		 * $WUA$.
		 */
		flag	= _UERRC | _UERRF;
		goto finished;
	}

	cup->ulrecl	= cup->urecpos;
	cup->urecpos	= 0;

	if ((cup->uflag & _UERRC) == 0) {	/* If no error */
		register int	ret;		/* Return value */
		long		zero = 0;	/* Zero word */

		if (cup->useq) {	/* Sequential Access */
			if (cup->ublkd) {
				int	status;	/* Unused status */

				/* Terminate the record */

				ret	= _fwwd(cup, &zero, &__tip_null, FULL,
						(int *)NULL, &zero, &status); 

				if (ret == IOERR)
					errn	= errno;
			}
		}
		else {			/* Direct Access */
			register long	bleft;	/* Unwritten bytes in record */

			bleft	= cup->urecl - (cup->ulrecl >> 3);

			if (bleft > 0 && cup->udalast == cup->udamax) {
				register int	ret;	/* Return value */
				long		*zbuf;	/* Buffer pointer */
				struct ffsw	fst;	/* FFIO status block */

				/*
				 * If this is the last direct access record in
				 * the file and a short record was written, be
				 * sure it is padded out to its full width as
				 * required by the Fortran standard.
				 */

				zbuf	= &zero;	/* Assume short pad */

				if (bleft > sizeof(long)) {

					zbuf	= (long *) malloc(bleft);

					if (zbuf == NULL) {
						errn	= FENOMEMY; /* No memory */
						goto error;
					}
					else	/* Clear record */
						(void) memset((void *) zbuf, 0, bleft);
				}

				switch (cup->ufs) {	/* File structure */

				case FS_FDC:
					ret	= XRCALL(cup->ufp.fdc, writertn)
							cup->ufp.fdc,
							WPTR2BP(zbuf), bleft, &fst,
							FULL, (int *)&zero);

					if (ret != bleft)
						errn	= fst.sw_error;

					break;

				case STD:
					ret	= fwrite((void *) zbuf, 1, bleft,
							 cup->ufp.std);

					if (ret != bleft)
						errn	= errno;

					break;

				default:
					errn	= FEINTUNK;	/* Deep weeds */
					break;

				} /* switch */

				/* Free any allocated space */

				if (zbuf != &zero)
					free(zbuf);
			}

		}

error:
		if (errn != 0) {
			if (cup->uiostat != NULL)
				*(cup->uiostat)	= errn;

			if ((cup->uflag & (_UIOSTF | _UERRF)) == 0)
				_ferr(css, errn);
			else	/* Set status */
				cup->uflag	= cup->uflag | _UERRC;
		}
	}

	flag	= cup->uflag;	/* Save status */

finished:
	STMT_END(cup, TF_WRITE, NULL, css);	/* Unlock unit */

	if ((flag & _UERRC) == 0)	/* If no error */
		return(CFT77_RETVAL(IO_OKAY));
	else
		if ((flag & (_UIOSTF | _UERRF)) != 0)
			return(CFT77_RETVAL(IO_ERR));

	_ferr(css, FEINTUNK);		/* Deep weeds */

	return(CFT77_RETVAL(IO_ERR));	/* MIPS compiler requires this return */
}
