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



#pragma ident "@(#) libf/fio/ru.c	92.2	06/21/99 10:37:55"

#include <errno.h>
#include <liberrno.h>
#include <fortran.h>
#include "fio.h"
#ifdef	_CRAYMPP
#include <stdarg.h>
#endif

#ifdef	_UNICOS 
#pragma	_CRI duplicate _RUI as $RUI
#pragma	_CRI duplicate _RUF as $RUF
#endif

int	_RUF();

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

/*
 *	$RUI - read unformatted initialization Fortran-77 I/O interface
 *
 *	CALL	$RUI,(funit, _arg2, err, end, iostat, rec)
 *
 *		funit	Address of Fortran unit designator (integer unit
 *			number)
 *		_arg2	Unused
 *		err	Address of error address (ERR=label)
 *		end	Address of end address (END=label)
 *		iostat	Address of I/O status variable (integer variable)
 *		rec	Address of integer record number (NULL implies
 *			sequential I/O)
 *
 *	$RUI calls:
 *
 *		_imp_open77(), _ferr(), _unit_seek()
 */

#ifdef	_CRAYMPP
int
_RUI(
_fcd		_Unitid,	/* Pointer to unit identifier		*/
...)
#else
int
_RUI(
_fcd		_Unitid,	/* Pointer to unit identifier		*/
_fcd		_arg2,		/* Unused				*/
long		*err,		/* Address of error processing address	*/
long		*end,		/* Address of end processing address	*/
_f_int		*iostat,	/* Address of IOSTAT variable		*/
_f_int		*rec		/* Address of direct access record no.	*/
#ifndef	_UNICOS
,FIOSPTR	cssa		/* Statement state structure		*/
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
	FIOSPTR		css;		/* Statement state structure	*/
#ifdef	_CRAYMPP
	va_list		args;
	_fcd		_arg2;		/* Unused */
	long		*err;		/* Address of error processing address*/
	long		*end;		/* Address of end processing address */
	_f_int		*iostat;	/* Address of IOSTAT variable */
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
	va_start(args, _Unitid);
	_arg2	= va_arg(args, _fcd);
	err	= va_arg(args, long *);
	end	= va_arg(args, long *);
	iostat	= va_arg(args, _f_int *);
	rec	= va_arg(args, _f_int *);
	va_end(args);
#endif
	errn	= 0;

	/* Establish error processing options */

	if (iostat != NULL)
		*iostat	= 0;		/* Clear IOSTAT variable, if extant */

	errf	= ((err != NULL) || (iostat != NULL));
	iost	= T_RSU;
	iotp	= SEQ;			/* Assume sequential */
	unum	= **(_f_int **)&_Unitid;

	if (rec != NULL) {	/* If direct access */
		iost	= T_RDU;	/* Set direct unformatted read */
		iotp	= DIR;
		recn	= *rec;
	}

	STMT_BEGIN(unum, 0, iost, NULL, css, cup);

	if (cup == NULL) {	/* if not connected */
		int	stat;

		cup	= _imp_open77(css, iotp, UNF, unum, errf, &stat);

		if (cup == NULL) {
			errn	= stat;
			goto error;
		}
	}

	/* Record error processing options in the unit */

	cup->uiostat	= iostat;
	cup->uflag	= (err    != NULL ?  _UERRF : 0) |
			  (end    != NULL ?  _UENDF : 0) |
			  (iostat != NULL ? _UIOSTF : 0);

	/* Perform error checking */

	if (cup->ufs == FS_AUX) {
		errn	= FEMIXAUX;	/* Can't mix auxiliary and Fortran I/O */
		ERROR0(errf, errn);
	}

	if ((cup->uaction & OS_READ) == 0) {
		errn	= FENOREAD;	/* No read permission */
		ERROR0(errf, errn);
	}

	if (cup->ufmt) {	/* If unformatted attempted on formatted file */
		errn	= FEUNFMIV;		/* Unformatted not allowed */
		ERROR0(errf, errn);
	}

	/* If sequential and writing, disallow read after write */

	if (cup->useq && cup->uwrt != 0) {
		errn	= FERDAFWR;		/* Read after write */
		ERROR0(errf, errn);
	}

	/* Preset fields in unit table */

	cup->ueor_found	= NO;			/* Clear EOR */
	cup->uwrt	= 0;
	cup->ulastyp	= DVTYPE_TYPELESS;

	if (iotp == DIR) {	/* If direct access */

		if (cup->useq)		/* If direct attempted on seq. file */
			errn	= FEDIRTIV;	/* Direct access not allowed */
		else
			errn	= _unit_seek(cup, recn, iost);

		if (errn != 0) {
			ERROR1(errf, errn, recn);
		}
	}
	else {		/* Else sequential access */

		if (cup->useq == 0) {	/* If seq. attempted on direct file */
			errn	= FESEQTIV;	/* Sequential not allowed */
			ERROR0(errf, errn);
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
			if (cup->upuretype == P_BUFIO) {
				errn	= FEMIXBUF;
				ERROR0(errf, errn);
			}
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
		cup->uflag	|= (errn > 0) ? _UERRC : _UENDC;/* Set status */

#ifdef	_UNICOS
	return(CFT77_RETVAL(_RUF()));
#else
	return(CFT77_RETVAL(_RUF(css)));
#endif

}

#ifdef	_UNICOS

/*
 *	Definition of inlined function _inline_rdunf()
 */
#define INLINE
#include "rdunf.c"

/*
 *	$RUA$ - read unformatted transfer Fortran-77 I/O interface 
 *
 *	CALL	$RUA$,(fwa, count, inc, type)
 *
 *		fwa	First word address of datum (may be a Fortran
 *			character descriptor)
 *		count	Number of data items
 *		stride	Stride between data items
 *		type	Type of data
 *
 *	$RUA$ calls:
 *
 *		_ferr(), _RUF()
 */

int
$RUA$(
	_fcd	fwa,		/* Address of first word of data	*/
	long	*count,		/* Address of count of data items	*/
	long	*stride,	/* Address of stride between data items	*/
	long	*type		/* Address of data type			*/
)
{
	register short	type77;		/* Fortran 77 data type */
	register int	errn;		/* Error number	*/
	type_packet	tip;		/* Type information packet */
	struct f90_type	ts;		/* F90 type structure */
	void		*dptr;
	unit		*cup;		/* Pointer to unit table entry	*/
	FIOSPTR		css;

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

#pragma _CRI inline _inline_rdunf
	errn	= _inline_rdunf(css, cup, dptr, &tip, 0);

	if (errn == 0)
		return(CFT77_RETVAL(IO_OKAY));

error:
	if (cup->uiostat != NULL)
		*(cup->uiostat)	= errn;

	cup->uflag	|= (errn > 0) ? _UERRC : _UENDC;	/* Set status */

	if (cup->uflag & (_UIOSTF | _UERRF | _UENDF))
		return(CFT77_RETVAL(_RUF()));

	_ferr(css, FEINTUNK);		/* Deep weeds */
}

#endif	/* _UNICOS */

/*
 *	_RUF - read unformatted finalization Fortran-77 I/O interface
 *
 *	CALL	_RUF,()
 *
 *	_RUF calls:
 *
 *		_ferr()
 */

int
_RUF(
#ifndef	_UNICOS
FIOSPTR	cssa		/* Statement state structure	*/
#endif
)
{
	register int	errn;		/* Error number			*/
	register long	flag;		/* Error flag			*/
	unit		*cup;		/* Pointer to unit table entry	*/
	FIOSPTR		css;		/* Statement state structure	*/

#ifdef	_UNICOS
	GET_FIOS_PTR(css);
#else
	css	= cssa;
#endif
	cup	= css->f_cu;

	if (cup == NULL) {		/* If unit not opened */
		/*
		 * If unit not connected, assume we are catching errors with
		 * ERR= or IOSTAT= and that _RUF is being called from $RUI or
		 * $RUA$.
		 */
		flag	= _UERRC | _UERRF;
		goto finished;
	}

	cup->ulrecl	= cup->urecpos;
	cup->urecpos	= 0;

#ifdef	_CRAYMPP
	if (css->f_shrdput) {
		css->f_shrdput	= 0;
		_remote_write_barrier();
	}
#endif
	if ((cup->uflag & (_UERRC | _UENDC)) == 0) {	/* If no error or EOF */

		errn	= 0;

		switch (cup->ufs) {

		case FS_FDC:
			/*
			 * Do a full record read to advance to the
			 * end of the record for sequential access.
			 */
			if (cup->useq)	/* If sequential */
				if (cup->ublkd && !cup->ueor_found) {
					int		ubc = 0;
					char		dummy;
					struct ffsw	fst;	/* FFIO status block */

					(void) XRCALL(cup->ufp.fdc, readrtn)
						cup->ufp.fdc,
						CPTR2BP(&dummy), 0,
						&fst, FULL, &ubc);

					switch (fst.sw_stat) {
					case FFERR:
						errn		= fst.sw_error;
						break;

					case FFEOF:
						cup->uend	= PHYSICAL_ENDFILE;
						errn		= FERDPEOF;
						break;

					case FFEOD:
						if (cup->uend == BEFORE_ENDFILE) {
							cup->uend	= LOGICAL_ENDFILE;
							errn		= FERDPEOF;
						}
						else
							errn		= FERDENDR;
						break;
					}
				}
			break;

		default:
			break;
		} /* switch */

		if (errn != 0) {

			if (cup->uiostat != NULL)
				*(cup->uiostat)	= errn;

			flag	= (_UIOSTF | ((errn < 0) ? _UENDF : _UERRF));

			if ((cup->uflag & flag) == 0)
				_ferr(css, errn);
			else	/* Set status */
				cup->uflag	|= (errn < 0) ? _UENDC : _UERRC;
		}

	}

	flag	= cup->uflag;	/* Save status */

finished:
	STMT_END(cup, TF_READ, NULL, css);	/* Unlock unit */

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

	return(CFT77_RETVAL(IO_ERR));	/* MIPS compiler requires this return */
}
