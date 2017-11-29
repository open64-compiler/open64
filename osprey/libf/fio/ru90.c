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



#pragma ident "@(#) libf/fio/ru90.c	92.1	06/21/99 10:37:55"

#include <stdio.h>
#include "fio.h"
#include "f90io.h"

/*
 *	_FRU	Called by compiled Fortran programs to process an unformatted
 *		read statement.  Each statement is processed by one or more
 *		calls to _FRU.
 *
 *	Synopsis
 *
 *		int _FRU(	ControlList	*cilist,
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
 *				to each call to _FRU for a particular 
 *				statement.  This space is used by the
 *				library.
 *
 *	Return value
 *
 *		IO_OKAY, IO_END, or IO_ERR
 */

int
_FRU(ControlListType *cilist, iolist_header *iolist, void *stck)
{
	register int	errf;		/* ERR processing flag	*/
	register int	errn;		/* Error number		*/
	register int	endf;		/* END processing flag	*/
	register int	iost;		/* I/O statement type	*/
	register int	retval;		/* _FRU Return value	*/
	register recn_t	errarg;		/* Extra _ferr argument	*/
	register unum_t	unum;		/* Unit number		*/
	unit		*cup;		/* Unit table pointer	*/
	FIOSPTR		css;		/* I/O statement state	*/

/*
 *	Assertions
 */
	/* Validate that the size of *stck is large enough */
	assert ( cilist->stksize >= sizeof(struct fiostate)/sizeof(long) );


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
	endf	= (cilist->endflag || cilist->iostatflg);
	unum	= *cilist->unit.wa;
	iost	= cilist->dflag ? T_RDU : T_RSU;

	STMT_BEGIN(unum, 0, iost, NULL, css, cup);

	if (cup == NULL) {	/* If not connected */
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

	/* Record error processing options in the unit. (used in _rdunf()) */

	cup->uflag	= (cilist->errflag		?  _UERRF : 0) |
			  (cilist->endflag		?  _UENDF : 0) |
			  (cilist->iostat_spec != NULL	? _UIOSTF : 0);

	/* If sequential and writing, disallow read after write */

	if (cup->useq && cup->uwrt != 0) {
		errn	= FERDAFWR;		/* Read after write */
		goto handle_exception;
	}

	/* Preset fields in unit table */

	cup->ueor_found	= NO;			/* Clear EOR */
	cup->uwrt	= 0;
	cup->ulastyp	= DVTYPE_TYPELESS;

	if (cilist->dflag) {	/* If direct access */

		if (!cup->ok_rd_dir_unf)
			errn	= _get_mismatch_error(errf, iost, cup, css);
		else {
			register recn_t	recn;	/* Record number */

			recn	= (recn_t) *cilist->rec_spec;
			errarg	= recn;
			errn	= _unit_seek(cup, recn, iost);
		}
	}
	else			/* Else sequential access */
		if (!cup->ok_rd_seq_unf)
			errn	= _get_mismatch_error(errf, iost, cup, css);

	if (errn != 0)
		goto handle_exception;


/*******************************************************************************
 *
 *	Data Transfer Section
 *
 ******************************************************************************/
data_transfer:

	errn	= _xfer_iolist(css, cup, iolist, _rdunf);

	if (errn != 0)
		goto handle_exception;

	if (! iolist->iollast)
		return(IO_OKAY);

/******************************************************************************
 *
 *	Statement Finalization Section
 *
 ******************************************************************************/
finalization:

	if (cup != NULL) {
		cup->ulrecl	= cup->urecpos;
		cup->urecpos	= 0;
	}

#ifdef	_CRAYMPP
	if (css->f_shrdput) {
		css->f_shrdput	= 0;
		_remote_write_barrier();
	}
#endif

	if (errn == 0 && cup->useq) {

		if (cup->ufs == FS_FDC) {

			/*
			 * Do a full record read to advance to the
			 * end of the record for sequential access.
			 */
			if (cup->ublkd && !cup->ueor_found) {
				char	dummy;		/* Unused data */
				int	ubc = 0;	/* Unused bit count */
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
				} /* switch */
			}
		}

		if (errn != 0)
			goto handle_exception;
	}

out_a_here:

	/* Set IOSTAT variable to 0 if no error, >0 error code otherwise */
 
	if (cilist->iostat_spec != NULL)
		*cilist->iostat_spec	= errn;

	STMT_END(cup, TF_READ, NULL, css);	/* Unlock unit */

	return(retval);

/*
 *	We put the error handling stuff here to reduce its impact when
 *	no errors are generated.  If we jump here, errn is set to a nonzero
 *	error, eor, or endfile status code.
 */
handle_exception:
 
	retval	= (errn < 0) ? IO_END : IO_ERR;
 
	if (retval == IO_ERR && ! cilist->errflag && ! cilist->iostatflg)
		_ferr(css, errn, errarg);
 
	if (retval == IO_END && ! cilist->endflag && ! cilist->iostatflg)
		_ferr(css, errn, errarg);
 
	if (cup == NULL)
		goto out_a_here;
 
	goto finalization;
}
