/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
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



#pragma ident "@(#) libf/fio/wf90.c	92.4	06/18/99 10:01:44"

#include <stdio.h>
#include <cray/format.h>
#include <cray/nassert.h>
#include "fio.h"
#include "f90io.h"

/*
 *	_FWF	Called by compiled Fortran programs to process a formatted
 *		write statement.  Each statement is processed by one or 
 *		more calls to _FWF.
 *
 *	Synopsis
 *
 *		int _FWF(	ControlList	*cilist,
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
 *				to each call to _FWF for a particular
 *				statement.  This space is used by the
 *				library.
 *
 *	Return value
 *
 *		IO_OKAY or IO_ERR
 */

int
_FWF(ControlListType *cilist, iolist_header *iolist, void *stck)
{
	register int	errf;		/* ERR processing flag	*/
	register int	errn;		/* Error number		*/
	register int	iost;		/* I/O statement type	*/
	register int	retval;		/* _FWF return value	*/
	register recn_t	errarg;		/* Extra _ferr argument	*/
	register unum_t	unum;		/* Unit number		*/
	xfer_func	*xfunc;		/* Data transfer func.	*/
	unit		*cup;		/* Unit table pointer	*/
	FIOSPTR		css;		/* I/O statement state	*/

/*
 *	Assertions 
 */
	/* Validate that the size of *stck is large enough */

	assert ( cilist->stksize >= sizeof(struct fiostate)/sizeof(long) );

	/* The compiler disallows EOR= on WRITE */

	assert ( cilist->eorflag == 0 );

	/* The compiler disallows SIZE= on WRITE */

	assert ( cilist->size_spec == NULL );

	/* The compiler disallows ADVANCE= w/ internal files */

	assert( ! (cilist->advcode != CI_ADVYES && cilist->internal != 0));

	/* The compiler disallows ADVANCE= w/ list-directed */

	assert( ! (cilist->advcode != CI_ADVYES && cilist->fmt == CI_LISTDIR));

	css	= stck;
	errn	= 0;
	errarg	= 0;
	retval	= IO_OKAY;
	xfunc	= (cilist->fmt == CI_LISTDIR) ? _ld_write : _wrfmt;

	if (iolist->iolfirst == 0) {
		cup	= css->f_cu;
		/*
 		 * Copy the user's error processing options into the unit table 
		 */
		cup->uflag	= (cilist->errflag		?  _UERRF : 0) |
				  (cilist->iostat_spec != NULL	? _UIOSTF : 0);
		goto data_transfer;
	}

/*******************************************************************************
 *
 *	Statement Initialization Section
 *
 ******************************************************************************/

	errf	= (cilist->errflag || cilist->iostatflg);

	if (cilist->fmt == CI_LISTDIR) 
		iost	= T_WLIST;
	else if (cilist->dflag)
		iost	= T_WDF;
	else
		iost	= T_WSF;

	css->u.fmt.freefmtbuf	= 0;
	css->u.fmt.freepfmt	= 0;
	css->u.fmt.tempicp	= NULL;

	/* Check if we're doing internal I/O or external I/O */

	if (cilist->internal) {		/* If internal I/O */
		STMT_BEGIN(-1, 1, iost, NULL, css, cup);
		cup->uft90	= 1;	/* set F90 mode for internal file */
/* KEY: I'm not sure if this is correct. */
#if	!defined(__mips)
		cup->ufcompat	= 2;	/* set cf90 on internal file */
		cup->ufunilist	= 0;
		cup->ufcomsep	= 0;
		cup->ufcomplen	= 0;
		cup->ufrptcnt	= 0;
		cup->ufnegzero	= 1;	/* set default write of -0.0 */
#elif	defined(_LITTLE_ENDIAN)
		cup->ufcompat	= 0;	/* set no f90 on internal file */
		cup->ufunilist	= 0;
		cup->ufcomsep	= 0;
		cup->ufcomplen	= 0;
		cup->ufrptcnt	= 0;
		cup->ufnegzero	= 1;	/* set default write of -0.0 */
#else
		cup->ufcompat	= 4;	/* set irixf90 on internal file */
		cup->ufunilist	= 0;
		cup->ufcomsep	= 0;
		cup->ufcomplen	= 0;
		cup->ufrptcnt	= 0;
		cup->ufnegzero	= 1;	/* set default write of -0.0 */
#endif
	}
	else {				/* Else external I/O */
		if (cilist->uflag == CI_UNITASTERK)
			unum	= STDOUT_U;
		else
			unum	= *cilist->unit.wa;

		STMT_BEGIN(unum, 0, iost, NULL, css, cup);

		if (cup == NULL) {	/* If not connected */
			int	stat;

			cup	= _imp_open(	css,
						(cilist->dflag ? DIR : SEQ),
						FMT,
						unum,
						errf,
						&stat);

			/*
			 * If the open failed, cup is NULL and stat contains 
			 * the error number.
			 */
			if (cup == NULL) {
				errn	= stat;
				goto handle_exception;
			}
		}
	}

	/* All paths which lead here have set cup to a non-null value */

	assert (cup != NULL);

/*
 *	Copy the user's error processing options into the unit table 
 */
	cup->uflag	= (cilist->errflag		?  _UERRF : 0) |
			  (cilist->iostat_spec != NULL	? _UIOSTF : 0);

	/* Initialize fields in the Fortran statement state structure */

	css->u.fmt.icp		= NULL;
	css->u.fmt.nonl		= 0;

	/* Process the format and related specifiers */

	if (cilist->fmt != CI_LISTDIR) {	/* If formatted output */
		register int	stat;

		css->u.fmt.u.fe.fmtbuf	= NULL;
		css->u.fmt.u.fe.fmtnum	= 0;
		css->u.fmt.u.fe.fmtcol	= 0;
		css->u.fmt.u.fe.scale	= 0;
		css->u.fmt.cplus	= 0;

		errn	= setup_format(css, cup, cilist);

		if (errn == 0) { /* If no error, handle ADVANCE specifier */

			stat	= _is_nonadv(cilist);

			if (stat < 0) /* If invalid ADVANCE specifier */
				errn	= FEADVSPC; /* Invalid ADVANCE */
		}

		if (errn != 0)
			goto handle_exception;

		css->u.fmt.nonadv	= stat;
	}
	else {					/* Else list-directed output */
		css->u.fmt.u.le.ldwinit	= 1;
		css->u.fmt.nonadv	= 0;
	}

	/* Set processing functions */

	if (cilist->dflag) {

		if (!cup->ok_wr_dir_fmt)
			errn	= _get_mismatch_error(errf, iost, cup, css);
		else {
			recn_t	recn;	/* Record number */

			recn	= (recn_t) *cilist->rec_spec;
			errarg	= recn;
			errn	= _unit_seek(cup, recn, iost);
		}

		cup->uend	= BEFORE_ENDFILE;
		cup->ulinecnt	= 0;		/* Number of characters written */
		cup->ulinemax	= 0;		/* Highwater mark */
		cup->ulineptr	= cup->ulinebuf;/* Current character position */
		css->u.fmt.endrec	= _dw_endrec;
	}
	else {

		if (!cup->ok_wr_seq_fmt) {
			errn	= _get_mismatch_error(errf, iost, cup, css);
			goto handle_exception;
		}

		if (cilist->internal) {

			cup->ulinecnt	= 0;	/* Number characters written */
			cup->ulinemax	= 0;	/* Highwater mark */

			css->u.fmt.endrec	= _iw_endrec;

			if (cilist->uflag == CI_UNITCHAR) {
				css->u.fmt.iiae	= 1;
				css->u.fmt.icp	= _fcdtocp(cilist->unit.fcd);
				css->u.fmt.icl	= _fcdlen (cilist->unit.fcd);
			}
			else {
				DopeVectorType	*dv = cilist->unit.dv;
				void		*newar;
				int 		nocontig = 0;
				long 		extent = 0;
				long 		nbytes = 0;

				css->u.fmt.icp	= _fcdtocp(dv->base_addr.charptr);
				css->u.fmt.icl	= _fcdlen (dv->base_addr.charptr);

				/*
				 * check for contiguous array
				 */
				newar	= (void *) NULL;

				if (dv->p_or_a && (dv->assoc == 0))
					errn	= FEUNOTAL; /* Not allocated/associated */
				else
					errn	= _cntig_chk(dv, &newar, &nocontig,
								&extent, &nbytes);
				if (errn > 0)
					goto handle_exception;

				css->u.fmt.iiae		= extent;

				if (nocontig) {
					css->u.fmt.icp		= newar;
					css->u.fmt.tempicp	= newar;
				}
			}

			cup->uldwsize	= css->u.fmt.icl;

			/*
			 * If the size of the internal record is greater
			 * than the existing line buffer, then realloc()
			 * another one; else just decrease urecsize.
			 */
 
			if (css->u.fmt.icl > cup->urecsize) {
 
				cup->ulinebuf	= (long *)realloc(cup->ulinebuf,
						sizeof(long) * (css->u.fmt.icl +
						1));

				if (cup->ulinebuf == NULL)
					errn	= FENOMEMY;	/* No memory */
			}

			cup->urecsize	= css->u.fmt.icl;
			cup->ulineptr	= cup->ulinebuf;
		}
		else {			/* external sequential formatted I/O */

			if (cup->uend != BEFORE_ENDFILE) {
				/*
				 * If positioned after an endfile, and the file
				 * does not support multiple endfiles, a write
				 * is invalid.
				 */
				if (!cup->umultfil && !cup->uspcproc) {
					errn	= FEWRAFEN;
					goto handle_exception;
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
						goto handle_exception;
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
				errn		= _unit_bksp(cup);

				if (errn != 0)
					goto handle_exception;
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

			if (cup->pnonadv && cilist->fmt == CI_LISTDIR)
				errn	= _lw_after_nonadv(css, cup,
							cup->uldwsize, 0);

			css->u.fmt.endrec	= _sw_endrec;
			cup->pnonadv		= css->u.fmt.nonadv;
		}
	}

	if (errn != 0)
		goto handle_exception;

	css->u.fmt.leftablim	= cup->ulineptr;	/* Set left tab limit */
	cup->uwrt		= 1;			/* Set write mode */

/*******************************************************************************
 *
 *	Data Transfer Section
 *
 ******************************************************************************/
data_transfer:

	assert (cup != NULL);			/* cup assumed non-NULL */

	errn	= _xfer_iolist(css, cup, iolist, xfunc);

	if (errn != 0)
		goto handle_exception;

	if (! iolist->iollast)
		return (IO_OKAY);

/******************************************************************************
 *
 *	Statement Finalization Section
 *
 ******************************************************************************/
finalization:

	/* Assertion */
	assert ( cup != NULL );

	/* If formatted I/O and no error complete processing */

/*
 *	Complete formatted or list-directed output processing.
 */

	if (errn == 0) {
		errn	= xfunc(css, cup, (void *) NULL, &__tip_null, 0L);

		if (errn != 0)
			goto handle_exception;

		if (css->u.fmt.nonadv)
			errn	= _nonadv_partrec(css, cup);
		else
			errn	= (*css->u.fmt.endrec)(css, cup, 1);

		if (errn != 0)
			goto handle_exception;
	}

	if (cilist->fmt != CI_LISTDIR)	/* If formatted */
		if (css->u.fmt.freepfmt || css->u.fmt.freefmtbuf) {

			/* If we allocated memory for a variable format, free it */

			if (css->u.fmt.freepfmt && css->u.fmt.u.fe.pfmt != NULL)
				free(css->u.fmt.u.fe.pfmt);
			/*
			 * If we allocated memory for a noncontiguous format,
			 * free it.
			 */

			if (css->u.fmt.freefmtbuf &&
			    css->u.fmt.u.fe.fmtbuf != NULL)
				free(css->u.fmt.u.fe.fmtbuf);
		}

	/*
	 * If we allocated memory for an internal file, move
	 * the output file from the temporary array to the
	 * noncontiguous array and free the temporary array.
	 */

	if (cilist->internal && css->u.fmt.tempicp != NULL) {
		(void) _unpack_arry (css->u.fmt.tempicp, cilist->unit.dv);
		free(css->u.fmt.tempicp);
	}

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
