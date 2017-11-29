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



#pragma ident "@(#) libf/fio/rf90.c	92.4	06/18/99 15:49:57"

#include <ctype.h>
#include <errno.h>
#include <liberrno.h>
#include <fortran.h>
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

/*
 *	_FRF	Called by compiled Fortran programs to process a formatted
 *		or list-directed read statement.  Each statement is
 *		processed by one or more calls to _FRF.
 *
 *	Synopsis
 *
 *		int _FRF(	ControlList	*cilist,
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
 *				to each call to _FRF for a particular 
 *				statement.  This space is used by the
 *				library.
 *
 *	Return value
 *
 *		IO_OKAY, IO_ERR, IO_END, or IO_EOR 
 */
int
_FRF(ControlListType *cilist, iolist_header *iolist, void *stck)
{
	register int	errf;		/* ERR processing flag	*/
	register int 	errn;		/* Error number		*/
	register int	iost;		/* I/O statement type	*/
	register int	retval;		/* _FRF return value	*/
	register recn_t	errarg;		/* Extra _ferr argument	*/
	register unum_t	unum;		/* Unit number		*/
	unit		*cup;		/* Unit table pointer	*/
	FIOSPTR		css;		/* I/O statement state	*/

/*
 *	Assertions 
 */
	/* Validate that the size of *stck is large enough */
	assert ( cilist->stksize >= sizeof(struct fiostate)/sizeof(long) );

	/* The compiler disallows ADVANCE='YES' w/ SIZE= */
	assert ( ! (cilist->advcode == CI_ADVYES && cilist->size_spec != NULL));

	/* The compiler disallows ADVANCE='YES' w/ EOR= */
	assert ( ! (cilist->advcode == CI_ADVYES && cilist->eorflag));

	/* The compiler disallows ADVANCE= w/ internal files */
	assert( ! (cilist->advcode != CI_ADVYES && cilist->internal != 0));

	/* The compiler disallows ADVANCE= w/ list-directed */
	assert( ! (cilist->advcode != CI_ADVYES && cilist->fmt == CI_LISTDIR));

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

	/* Establish error processing options */

	errf	= (cilist->errflag || cilist->iostatflg);

	if (cilist->fmt == CI_LISTDIR) 
		iost	= T_RLIST;
	else if (cilist->dflag)
		iost	= T_RDF;
	else
		iost	= T_RSF;

	/* Zero the SIZE= value before any errors are encountered */

	if (iost & TF_FMT)
		css->u.fmt.u.fe.charcnt	= 0;

	css->u.fmt.freefmtbuf	= 0;
	css->u.fmt.freepfmt	= 0;
	css->u.fmt.tempicp	= NULL;

	/* Check if we're doing internal I/O or external I/O */

	if (cilist->internal) {		/* If internal I/O */
		STMT_BEGIN(-1, 1, iost, NULL, css, cup);
		cup->uft90	= 1;	/* set F90 mode for internal file */
#ifndef	__mips
		cup->ufcompat	= 2;	/* set CF90 on internal file */
		cup->ufunilist	= 0;
		cup->ufcomsep	= 0;
		cup->ufcomplen	= 0;
		cup->ufrptcnt	= 0;
#elif	defined(_LITTLE_ENDIAN)
		cup->ufcompat	= 0;	/* set no f90 on internal file */
		cup->ufunilist	= 0;
		cup->ufcomsep	= 0;
		cup->ufcomplen	= 0;
		cup->ufrptcnt	= 0;
#else
		cup->ufcompat	= 4;	/* set IRIXF90 on internal file */
		cup->ufunilist	= 0;
		cup->ufcomsep	= 0;
		cup->ufcomplen	= 0;
		cup->ufrptcnt	= 0;
#endif
	}
	else {				/* Else external I/O */
		if (cilist->uflag == CI_UNITASTERK)
			unum	= STDIN_U;
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

	/* Copy the user's error processing options into the unit table */

	cup->uflag	= (cilist->errflag		?  _UERRF : 0) |
			  (cilist->endflag		?  _UENDF : 0) |
			  (cilist->eorflag		?  _UEORF : 0) |
			  (cilist->iostat_spec != NULL	? _UIOSTF : 0);

	/* Initialize fields in the Fortran statement state structure */

	css->u.fmt.icp		= NULL;
	css->u.fmt.blank0	= cup->ublnk;
	css->u.fmt.lcomma	= 0;
	css->u.fmt.slash	= 0;
#ifdef	_CRAYMPP
	css->f_shrdput		= 0;
#endif

	/* Process the format and related specifiers */

	if (cilist->fmt != CI_LISTDIR) {	/* If formatted input */
		register int	stat;

		css->u.fmt.u.fe.fmtbuf	= NULL;
		css->u.fmt.u.fe.fmtnum	= 0;
		css->u.fmt.u.fe.fmtcol	= 0;
		css->u.fmt.u.fe.scale	= 0;

		errn	= setup_format(css, cup, cilist);

		if (errn > 0)
			goto handle_exception;

		/* Handle ADVANCE specifier */

		stat	= _is_nonadv(cilist);

		if (stat < 0)
			errn	= FEADVSPC;	/* Invalid ADVANCE specifier */

		if (cilist->advcode == CI_ADVVAR && stat == 0) {
			if (cilist->size_spec != NULL)
				errn	= FEADVSIZ; /* ADVANCE='YES' w/SIZE= */

			if (cilist->eorflag)
				errn	= FEADVEOR; /* ADVANCE='YES' w/EOR= */
		}

		if (errn != 0)
			goto handle_exception;

		css->u.fmt.nonadv	= stat;
	}
	else					/* Else list-directed input */
		css->u.fmt.nonadv	= 0;

	/* Set processing functions */

	if (cilist->dflag) {

		if (!cup->ok_rd_dir_fmt)
			errn	= _get_mismatch_error(errf, iost, cup, css);
		else {
			register recn_t	recn;	/* Record number */

			recn	= (recn_t) *cilist->rec_spec;
			errarg	= recn;
			errn	= _unit_seek(cup, recn, iost);
		}

		css->u.fmt.endrec	= _dr_endrec;
	}
	else {

		if (!cup->ok_rd_seq_fmt) {
			errn	= _get_mismatch_error(errf, iost, cup, css);
			goto handle_exception;
		}

		if (cilist->internal) {

			css->u.fmt.endrec	= _ir_endrec;

			if (cilist->uflag == CI_UNITCHAR) {
				css->u.fmt.iiae	= 1;
				css->u.fmt.icp	= _fcdtocp(cilist->unit.fcd);
				css->u.fmt.icl	= _fcdlen (cilist->unit.fcd);
			}
			else {
				DopeVectorType *dv = cilist->unit.dv;
				void	*newar;
				int	nocontig = 0;
				long	extent = 0;
				long	nbytes = 0;

				css->u.fmt.icp	= _fcdtocp(dv->base_addr.charptr);
				css->u.fmt.icl	= _fcdlen (dv->base_addr.charptr);

				newar	= (void *) NULL;

				if (dv->p_or_a && (dv->assoc == 0))
					errn	= FEUNOTAL; /* Not allocated/associated */
				else	/* Check for contiguous array */
					errn	= _cntig_chk(dv, &newar, &nocontig,
							&extent, &nbytes);

				if (errn != 0)
					goto handle_exception;

				/* Number of elements in array */

				css->u.fmt.iiae	= extent;

				if (nocontig) {
					css->u.fmt.icp		= newar;
					css->u.fmt.tempicp	= newar;
				}
			}

			/*
			 * If the size of the internal record is greater
			 * than the existing line buffer, then realloc()
			 * another one; else just decrease urecsize.
			 */
 
			if (css->u.fmt.icl > cup->urecsize) {
 
				cup->ulinebuf	= (long *) realloc(cup->ulinebuf,
							sizeof(long) *
							(css->u.fmt.icl + 1));

				if (cup->ulinebuf == NULL)
					errn	= FENOMEMY;	/* No memory */
			}

			cup->urecsize	= css->u.fmt.icl;
		}
		else {			/* external sequential formatted I/O */

			if (cup->uend != BEFORE_ENDFILE && !cup->umultfil) /* If after endfile */
				errn	= FERDENDR;	/* Read after endfile */
	
			if (cup->uwrt)	/* If writing */
				errn	= FERDAFWR;	/* Read after write */

			css->u.fmt.endrec	= _sr_endrec;

		}
	}

	if (errn != 0)
		goto handle_exception;

	if (cup->pnonadv == 0) {	/* If previous ADVANCE='YES' */
		errn	= (*css->u.fmt.endrec)(css, cup, 1); /* Read a record */

		if (errn != 0)
			goto handle_exception;
	}
	else			/* else previous ADVANCE='NO' */
		css->u.fmt.leftablim	= cup->ulineptr; /* Set left tab limit */

	cup->pnonadv	= css->u.fmt.nonadv;	/* Remember previous ADVANCE */
	cup->uwrt	= 0;			/* Set read status */

/*******************************************************************************
 *
 *	Data Transfer Section
 *
 ******************************************************************************/
data_transfer:

	errn	= _xfer_iolist(css, cup, iolist, (cilist->fmt == CI_LISTDIR) ?
				_ld_read : _rdfmt);

	if (errn != 0)
		goto handle_exception;

	if (! iolist->iollast )
		return (IO_OKAY);

/******************************************************************************
 *
 *	Statement Finalization Section
 *
 ******************************************************************************/
finalization:

#ifdef	_CRAYMPP
	if (css->f_shrdput) {
		css->f_shrdput	= 0;
		_remote_write_barrier();
	}
#endif
	/* If formatted I/O and no error/EOF/EOR, complete processing */

	if (cilist->fmt != CI_LISTDIR) {	/* If formatted */

		if (errn == 0)	/* Complete format */
			errn	= _rdfmt(css, cup, (void *) NULL, &__tip_null,
					0);

		/* If we allocated memory for a variable format, free it */

		if (css->u.fmt.freepfmt && css->u.fmt.u.fe.pfmt != NULL)
			free(css->u.fmt.u.fe.pfmt);

		/* If we allocated memory for a noncontiguous format,
		 * free it.
		 */

		if (css->u.fmt.freefmtbuf && css->u.fmt.u.fe.fmtbuf != NULL)
			free(css->u.fmt.u.fe.fmtbuf);
	}

	/*
	 * If we allocated memory for an internal file, move the output
	 * file from the temporary array to the noncontiguous array and
	 * free the temporary array.
	 */

	if (cilist->internal && css->u.fmt.tempicp != NULL) {
		(void) _unpack_arry (css->u.fmt.tempicp,
					cilist->unit.dv);
		free(css->u.fmt.tempicp);
	}

out_a_here:

	/* Set IOSTAT variable to 0 if no error, >0 error code otherwise */

	if (cilist->iostat_spec != NULL)
		*cilist->iostat_spec	= errn;

	/* Store character count in the SIZE= variable */

	if (cilist->size_spec != NULL)
		*cilist->size_spec	= css->u.fmt.u.fe.charcnt;

	STMT_END(cup, TF_READ, NULL, css);	/* Unlock unit */

	/* Return proper status */

	return (retval);

/*
 *	We put the error handling stuff here to reduce its impact when
 *	no errors are generated.  If we jump here, errn is set to a nonzero
 *	error, eor, or endfile status code.
 */
handle_exception:
	if (errn < 0) {	/* If EOF/EOR type error */

		/* No current record if EOF or EOR */

		if (cup != NULL) 
			cup->pnonadv	= 0;

		if (errn == FEEORCND)
			retval	= IO_EOR;
		else
			retval	= IO_END;
	}
	else 
		retval	= IO_ERR;

	if (retval == IO_ERR && ! cilist->errflag && ! cilist->iostatflg)
		_ferr(css, errn, errarg);

	if (retval == IO_EOR && ! cilist->eorflag && ! cilist->iostatflg)
		_ferr(css, errn, errarg);

	if (retval == IO_END && ! cilist->endflag && ! cilist->iostatflg)
		_ferr(css, errn, errarg);

	if (cup == NULL)
		goto out_a_here;

	goto finalization;
}
