/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
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



#pragma ident "@(#) libf/fio/inq.c	92.1	06/21/99 10:37:55"

#include <liberrno.h>
#include <fortran.h>
#include <string.h>
#include <cray/nassert.h>
#include "fio.h"
#include "f90io.h"
#ifdef _CRAYMPP
#include <stdarg.h>
#endif
/*
 *	_INQ - Fortran runtime entry for INQUIRE
 */
#ifdef _UNICOS
#pragma _CRI duplicate _INQ as $INQ		/* for cf77 */
#endif

#ifdef _CRAYMPP
_INQ(
_f_int	*unitn,
_f_int	*iostat,
int	errf,
_f_log	*exist,
_f_log	*opened,
_f_int	*number,
_f_log	*named,
...
)
#else
int _INQ(
_f_int	*unitn,
_f_int	*iostat,
int	errf,
_f_log	*exist,
_f_log	*opened,
_f_int	*number,
_f_log	*named,
_fcd	name,
_fcd	access,
_fcd	sequent,
_fcd	direct,
_fcd	form,
_fcd	formatt,
_fcd	unform,
_f_int	*recl,
_f_int	*nextrec,
_fcd	blank,
_fcd	file,
_fcd	pos,
_fcd	action,
_fcd	red,
_fcd	writ,
_fcd	redwrit,
_fcd	delim,
_fcd	pad
)
#endif
{
	inlist	a;		/* INQUIRE parameter list		*/
	int	errn;		/* IOSTAT error number			*/
	int	error;		/* Error flag				*/
	unum_t	unum;		/* Unit number				*/
	long	stmt;		/* Statement type			*/
	unit	*cup;		/* Unit pointer if inquire by unit	*/
	struct fiostate cfs;
#ifdef _CRAYMPP
	va_list	args;
	_fcd	name;
	_fcd	access;
	_fcd	sequent;
	_fcd	direct;
	_fcd	form;
	_fcd	formatt;
	_fcd	unform;
	_f_int	*recl;
	_f_int	*nextrec;
	_fcd	blank;
	_fcd	file;
	_fcd	pos;
	_fcd	action;
	_fcd	red;
	_fcd	writ;
	_fcd	redwrit;
	_fcd	delim;
	_fcd	pad;
	va_start(args,named);
	name	= va_arg(args, _fcd);
	access	= va_arg(args, _fcd);
	sequent	= va_arg(args, _fcd);
	direct	= va_arg(args, _fcd);
	form	= va_arg(args, _fcd);
	formatt	= va_arg(args, _fcd);
	unform	= va_arg(args, _fcd);
	recl	= va_arg(args, _f_int *);
	nextrec	= va_arg(args, _f_int *);
	blank	= va_arg(args, _fcd);
	file	= va_arg(args, _fcd);
#endif

	/* Initialize the inlist structure */

	(void) memset(&a, 0, sizeof(inlist));
	a.inunit	= -1;

	/* Determine type of INQUIRE */

	if (_fcdtocp(file) != NULL) {
		a.infile	= _fcdtocp(file);
		a.infilen	= _fcdlen (file);	/* CFT77 */
		stmt		= T_INQF;		/* INQUIRE by FILE */
		unum		= -1;
	}
	else {
		stmt		= T_INQU;		/* INQUIRE by UNIT */
		unum		= *unitn;
		a.inunit	= unum;
	}

/*
 *	Here unum is -1 if this is an inquire by file.  This will suppress
 *	any unit locking in STMT_BEGIN.
 */

	STMT_BEGIN(unum, 0, stmt, NULL, &cfs, cup);

	/* Process rest of parameters */

	if (_fcdtocp(name) != NULL) {
		a.inname	= _fcdtocp(name);
		a.innamlen	= _fcdlen (name);	/* CFT77 */

		if (a.innamlen == 0)
			a.innamlen	= strlen(a.inname);	/* CFT2 */
	}

	if (_fcdtocp(access) != NULL) {
		a.inacc		= _fcdtocp(access);
		a.inacclen	= _fcdlen (access);
	}

	if (_fcdtocp(sequent) != NULL) {
		a.inseq		= _fcdtocp(sequent);
		a.inseqlen	= _fcdlen (sequent);
	}

	if (_fcdtocp(direct) != NULL) {
		a.indir		= _fcdtocp(direct);
		a.indirlen	= _fcdlen (direct);
	}

	if (_fcdtocp(form) != NULL) {
		a.inform	= _fcdtocp(form);
		a.informlen	= _fcdlen (form);
	}

	if (_fcdtocp(formatt) != NULL) {
		a.infmt		= _fcdtocp(formatt);
		a.infmtlen	= _fcdlen (formatt);
	}

	if (_fcdtocp(unform) != NULL) {
		a.inunf		= _fcdtocp(unform);
		a.inunflen	= _fcdlen (unform);
	}

	if (_fcdtocp(blank) != NULL) {
		a.inblank	= _fcdtocp(blank);
		a.inblanklen	= _fcdlen (blank);
	}

#ifdef	_UNICOS
	if (_numargs() <= (9 + 9*sizeof(_fcd)/sizeof(long)))
		goto old_inq;
#endif

#ifdef _CRAYMPP
	pos	= va_arg(args, _fcd);
	action	= va_arg(args, _fcd);
	red	= va_arg(args, _fcd);
	writ	= va_arg(args, _fcd);
	redwrit	= va_arg(args, _fcd);
	delim	= va_arg(args, _fcd);
	pad	= va_arg(args, _fcd);
#endif
	if (_fcdtocp(pos) != NULL) {
		a.inposit	= _fcdtocp(pos);
		a.inpositlen	= _fcdlen (pos);
	}

	if (_fcdtocp(action) != NULL) {
		a.inaction	= _fcdtocp(action);
		a.inactonlen	= _fcdlen (action);
	}

	if (_fcdtocp(red) != NULL) {
		a.inread	= _fcdtocp(red);
		a.inreadlen	= _fcdlen (red);
	}

	if (_fcdtocp(writ) != NULL) {
		a.inwrite	= _fcdtocp(writ);
		a.inwritelen	= _fcdlen (writ);
	}

	if (_fcdtocp(redwrit) != NULL) {
		a.inredwrit	= _fcdtocp(redwrit);
		a.inrdwrtlen	= _fcdlen (redwrit);
	}

	if (_fcdtocp(delim) != NULL) {
		a.indelim	= _fcdtocp(delim);
		a.indelimlen	= _fcdlen (delim);
	}

	if (_fcdtocp(pad) != NULL) {
		a.inpad		= _fcdtocp(pad);
		a.inpadlen	= _fcdlen (pad);
	}

old_inq:

	a.inerr		= (errf || iostat) ? 1 : 0;
	a.inex		= exist;
	a.inopen	= opened;
	a.innum		= number;
	a.innamed	= named;
	a.inrecl	= recl;
	a.innrec	= nextrec;
	errn		= _f_inqu(&cfs, cup, &a);
	error		= (errn != 0) ? IO_ERR : IO_OKAY;

	if (iostat != NULL)
		*iostat	= errn;

#ifdef _CRAYMPP
	va_end(args);
#endif
	STMT_END(NULL, 0, NULL, NULL);	

	return(CFT77_RETVAL(error));
}

/*
 *	_INQUIRE - Fortran 90 INQUIRE statement processing.
 */
int
_INQUIRE(struct inquire_spec_list *i)
{
	assert( i->version == 0 );

	return( _INQ(i->unit, i->iostat, i->err, i->exist, i->opened,
		     i->number, i->named, i->name, i->access, i->sequential,
		     i->direct, i->form, i->formatted, i->unformatted, i->recl,
		     i->nextrec, i->blank, i->file, i->position, i->action,
		     i->read, i->write, i->readwrite, i->delim, i->pad) );
}
