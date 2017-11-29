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



#pragma ident "@(#) libf/fio/wu90.c	92.3	08/02/99 10:38:48"

#include <stdio.h>
#include "fio.h"
#include "f90io.h"

/*
 *	_FWU	Called by compiled Fortran programs to process an unformatted
 *		write statement.  Each statement is processed by one or more
 *		calls to _FWU.  If any of the calls to _FWU for a particular
 *		write statement result in an error return code, the compiler
 *		ensures that subsequent calls to _FWU are suppressed.
 *
 *	Synopsis
 *
 *		int _FWU(	ControlList	*cilist,
 *				iolist_header	*iolist,
 *				void		*stck);
 *		
 *		Where
 *
 *			cilist	Pointer to the control information list 
 *				information.  This describes the specifiers 
 *				for the current I/O statement.  This cilist
 *				is guaranteed by the compiler to reflect
 *				the original values of control information
 *				list variables for the duration of the I/O
 *				statement (ie through multiple calls). 
 *			iolist	Pointer to the I/O list information. 
 *			stck	Pointer to stack space which is passed
 *				to each call to _FWU for a particular 
 *				statement.  This space is used by the 
 *				library.
 *
 *	Return value
 *
 *		IO_OKAY or IO_ERR
 */

int
_FWU(ControlListType *cilist, iolist_header *iolist, void *stck)
{
	register int	errf;		/* ERR processing flag	*/
	register int	errn;		/* Error number		*/
	register int	iost;		/* I/O statement type	*/
	register int	retval;		/* _FWU return value	*/
	register recn_t	errarg;		/* Extra _ferr argument	*/
	register unum_t	unum;		/* Unit number		*/
	unit		*cup;		/* Unit table pointer	*/
	FIOSPTR		css;		/* I/O statement state	*/

/*
 *	Assertions 
 */
	/* Validate that the size of *stck is large enough */

	assert ( cilist->stksize >= sizeof(struct fiostate)/sizeof(long) );

	/* Validate correct unformatted I/O info from compiler */

	assert ( cilist->uflag == CI_UNITNUM );
	assert ( cilist->eorflag == 0 );
	assert ( cilist->endflag == 0 );


	css	= stck;
	errn	= 0;
	errarg	= 0;
	retval	= IO_OKAY;

	if (iolist->iolfirst == 0) {
		cup	= css->f_cu;
		goto data_transfer;
	}

/*******************************************************************************
 *
 *	Statement Initialization Section
 *
 ******************************************************************************/

	errf	= (cilist->errflag || cilist->iostatflg);
	unum	= *cilist->unit.wa;
	iost	= cilist->dflag ? T_WDU : T_WSU;

	STMT_BEGIN(unum, 0, iost, NULL, css, cup);

	if (cup == NULL) {		/* If not connected */
		int	stat;	/* Status */

		cup	= _imp_open(css, (cilist->dflag ? DIR : SEQ), UNF,
				unum, errf, &stat);
		/*
		 * If the open failed, cup is NULL and stat contains
		 * the error number.
		 */

		if (cup == NULL) {
			errn	= stat;
			goto handle_exception;
		}
	}

	cup->f_lastiolist = NULL;	/* Indicates whether we are on the last item */

	/*
	 * Record error processing options in the unit.  The _wrunf() etc.
	 * routines will still use cup->uflag.
	 */
	cup->uflag	= (cilist->errflag		? _UIOSTF : 0) |
			  (cilist->iostat_spec != NULL	?  _UERRF : 0);

	cup->uwrt	= 1;
	cup->ulastyp	= DVTYPE_TYPELESS;

	if (cilist->dflag) {	/* If direct access */

		if (!cup->ok_wr_dir_unf)
			errn	= _get_mismatch_error(errf, iost, cup, css);
		else {
			recn_t	recn;	/* Record number */

			recn	= (recn_t) *cilist->rec_spec;
			errarg	= recn;
			errn	= _unit_seek(cup, recn, iost);
		}

		if (cup->udalast > cup->udamax)	/* If new highwater mark */
			cup->udamax	= cup->udalast;

		cup->uend	= BEFORE_ENDFILE;
	}
	else {		/* Else sequential access */

		if (!cup->ok_wr_seq_unf) {
			errn	= _get_mismatch_error(errf, iost, cup, css);
			goto handle_exception;
		}

		if (cup->uend != BEFORE_ENDFILE) {
			struct ffsw	fst;	/* FFIO status block */
			/*
			 * If positioned after an endfile, and the file does not
			 * support multiple endfiles, a write is invalid.
			 */
			if (!cup->umultfil && !cup->uspcproc) {
				errn	= FEWRAFEN;	/* Write after endfile */
				goto handle_exception;
			}
			/*
			 * If a logical endfile record had just been read,
			 * replace it with a physical endfile record before
			 * starting the current data record.
			 */
			if ((cup->uend == LOGICAL_ENDFILE) && !(cup->uspcproc)) {
				if (XRCALL(cup->ufp.fdc, weofrtn)cup->ufp.fdc, &fst) < 0)
					errn	= fst.sw_error;
			}

			cup->uend	= BEFORE_ENDFILE;
		}
	}

	if (errn != 0)
		goto handle_exception;

/*******************************************************************************
 *
 *	Data Transfer Section 
 *
 ******************************************************************************/
data_transfer:

	cup->f_lastwritten = 0;
	if (iolist->iollast && !cilist->dflag && cup->ublkd) {
		cup->f_lastiolist = (long *)iolist + iolist->ioetsize;
	}
	errn	= _xfer_iolist(css, cup, iolist, _wrunf);

	if (errn != 0)
		goto handle_exception;

	if (! iolist->iollast)
		return(IO_OKAY);


/*******************************************************************************
 *
 *	Finalization
 *
 ******************************************************************************/
finalization:

	if (cup != NULL) {
		cup->ulrecl	= cup->urecpos;
		cup->urecpos	= 0;
		cup->f_lastiolist = NULL;	/* reset */
	}

	if (errn != 0)
		goto out_a_here;

	if (!cilist->dflag) {		/* Sequential Access */
		if (cup->ublkd && cup->f_lastwritten == 0) {
			register int	ret;	/* Return value */
			int		dummy;	/* Unused word */

			/* Terminate the record */

			ret	= _fwwd(cup, &dummy, &__tip_null, FULL,
					(int *) NULL, (long *) NULL, &dummy);

			if (ret == IOERR)
				errn	= errno;
		}
	}
	else {				/* Direct Access */
		register long	bleft;	/* bytes unwritten in record */

		bleft	= cup->urecl - (cup->ulrecl >> 3);

		if (bleft > 0 && cup->udalast == cup->udamax) {
			ssize_t		ret;	/* Return value */
			long		zero = 0; /* Zero word */
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
					goto handle_exception;
				}
				else	/* Clear record */
					(void) memset((void *) zbuf, 0, (size_t)bleft);
			}

			switch (cup->ufs) {	/* File structure */

			case FS_FDC:
				ret	= XRCALL(cup->ufp.fdc, writertn)
						cup->ufp.fdc, WPTR2BP(zbuf),
						bleft, &fst, FULL, (int *)&zero);

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

	if (errn != 0)
		goto handle_exception;

out_a_here:

	/* Set IOSTAT variable to 0 if no error, >0 error code otherwise */
 
	if (cilist->iostat_spec != NULL)
		*(cilist->iostat_spec)	= errn;

	STMT_END(cup, TF_WRITE, NULL, css);	/* Unlock unit */

	/* Return proper status */

	return (retval);

/*
 *	We put the error handling stuff here to reduce its impact when
 *	no errors are generated.  If we jump here, errn is set to a nonzero
 *	error, eor, or endfile status code.
 */
handle_exception:
 
	retval	= IO_ERR;
 
	if (! cilist->errflag && ! cilist->iostatflg)
		_ferr(css, errn, errarg);
 
	if (cup == NULL)
		goto out_a_here;
 
	goto finalization;
}
