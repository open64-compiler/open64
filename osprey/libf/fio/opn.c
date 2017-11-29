/*
 * Copyright 2003, 2004, 2005 PathScale, Inc.  All Rights Reserved.
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



#pragma ident "@(#) libf/fio/opn.c	92.3	06/23/99 16:08:16"

#include <errno.h>
#include <fortran.h>
#include <liberrno.h>
#include <string.h>
#include <stdarg.h>
#include <cray/nassert.h>
#ifdef KEY /* Bug 4260 */
#  include <cray/assign.h>
#endif /* KEY Bug 4260 */
#include "fmt.h"
#include "fio.h"
#include "f90io.h"

#define OPNERR(n) { \
	errn	= n;	\
	goto opn_done;	\
}

#define SETSPEC(_specifier, _default_value, _error_code, _nval, _spec_list) \
 \
	if (_fcdtocp(_specifier) == NULL) \
		a.o##_specifier	= _default_value; \
	else if (! findmatch(_specifier, (int*) &a.o##_specifier, _nval \
			     _spec_list)) \
		OPNERR(_error_code)

#undef S
#define S(_spec) , #_spec, OS_##_spec

/*
 *	PASSED_ARG(x) is nonzero if argument x was passed.
 */

#define ARGS_11		(4 + 7*sizeof(_fcd)/sizeof(long))
#define ARGS_12		(4 + 8*sizeof(_fcd)/sizeof(long))
#define ARGS_13		(4 + 9*sizeof(_fcd)/sizeof(long))
#define ARGS_16		(7 + 9*sizeof(_fcd)/sizeof(long))
#if _UNICOS
#define PASSED_ARG(x)	(_numargs() >= x)
#else
#define PASSED_ARG(x)	(1)
#endif


static int	findmatch(_fcd fortstring, int *result, int nval, ...);

/*
 *	$OPN - Fortran-77 runtime open routine.  Processes an OPEN statement.
 */

#ifdef	_UNICOS
#pragma _CRI duplicate __OPN as $OPN
#endif

#ifdef _CRAYMPP
__OPN(
_f_int	*unitn,
_f_int	*iostat,
int	*errf,
...
)
#else
int __OPN(
_f_int	*unitn,
_f_int	*iostat,
int	*errf,
_fcd	file,
_fcd	status,
_fcd	access,
_fcd	form,
_f_int	*recl,
_fcd	blank,
_fcd	position,
_fcd	action_arg,
_fcd	delim_arg,
_fcd	pad_arg,
int	unused1,	/* for a future CFT77 open specifier */
int	unused2,	/* for a future CFT77 open specifier */
int	isf90_arg)	/* =1 iff Fortran-90 OPEN */
#endif
{
	olist		a;		/* OPEN specifier list		*/
	long		fstrlen;	/* Length of Fortran string	*/
	int		errn;		/* IOSTAT error number		*/
	int		error;		/* Error flag			*/
	unum_t		unum;		/* Fortran unit number		*/
	_fcd		action;
	_fcd		delim;
	_fcd		pad;
	int		isf90;
	unit		*cup;		/* Pointer to unit table entry	*/
	enum form_spec	formdef;
	struct fiostate	cfs;

#ifdef _CRAYMPP
	va_list args;
	_fcd	file;
	_fcd	status;
	_fcd	access;
	_fcd	form;
	_f_int	*recl;
	_fcd	blank;
	_fcd	position;
	int	unused1;	/* for a future CFT77 open specifier */
	int	unused2;	/* for a future CFT77 open specifier */
	int	isf90_arg;	/* =1 iff Fortran-90 OPEN */
#endif

/*
 *	The ACTION, DELIM, and PAD specifiers are supported by CFT77
 *	release 5.0 and later on CX/CEA systems, and by CFT77 release 6.0 and
 *	later on CRAY-2 systems.
 */
	action	= _cptofcd(NULL, 0);
	delim	= _cptofcd(NULL, 0);
	pad	= _cptofcd(NULL, 0);
#ifdef _CRAYMPP
	va_start(args,errf);
	file	= va_arg(args, _fcd);
	status	= va_arg(args, _fcd);
	access	= va_arg(args, _fcd);
	form	= va_arg(args, _fcd);
	recl	= va_arg(args, _f_int *);
	blank	= va_arg(args, _fcd);
	position = va_arg(args, _fcd);
	
#endif
	if (PASSED_ARG(ARGS_11)) {
#ifdef _CRAYMPP
		action	= va_arg(args, _fcd);
#else
		action	= action_arg;
#endif
	}
	if (PASSED_ARG(ARGS_12)) {
#ifdef _CRAYMPP
		delim	= va_arg(args, _fcd);
#else
		delim	= delim_arg;
#endif
	}
	if (PASSED_ARG(ARGS_13)) {
#ifdef _CRAYMPP
		pad	= va_arg(args, _fcd);
#else
		pad	= pad_arg;
#endif
	}
/*
 *	The isf90 argument is not passed from CFT77.
 */
	isf90	= 0;

	if (PASSED_ARG(ARGS_16)) {
#ifdef _CRAYMPP
		unused1	= va_arg(args, int);
		unused2	= va_arg(args, int);
		isf90	= va_arg(args, int);
#else
		isf90	= isf90_arg;
#endif
	}
#ifdef _CRAYMPP
	va_end(args);
#endif
	errn	= 0;

	OPENLOCK();		/* prevent other OPENs or CLOSEs right now */

#ifdef KEY /* Bug 4260 */
	/* Before we open the first file in the course of execution, we must
	 * set byte-swapping based on __io_byteswap_value defined by Fortran
	 * main in response to command-line options like -byteswapio */
        __io_byteswap();
#endif /* KEY Bug 4260 */

	unum	= *unitn;	/* UNIT= is required by compiler */
	a.ounit	= unum;

	STMT_BEGIN(unum, 0, T_OPEN, NULL, &cfs, cup);	/* lock unit if open */

	if (!GOOD_UNUM(unum) || RSVD_UNUM(unum))
		OPNERR(FEIVUNTO);

	a.oerr	= (errf || iostat) ? 1 : 0;	/* Catch errs if ERR | IOSTAT */

/*
 *	Process FILE= and RECL= specifiers.
 */
	if (_fcdtocp(file) != NULL) {
	       	a.ofile		= _fcdtocp(file);
		a.ofilelen	= _fcdlen (file);
	}
	else {
		a.ofile		= NULL;
		a.ofilelen	= 0;
	}

	if (recl != NULL)
		a.orecl	= *recl;
	else
		a.orecl	= 0;	/* 0 means unspecified */

/*
 *	Process remaining specifiers.
 *
 *	Specifier		Default			Error Code
 * 	Value List
 */

	SETSPEC(status,		OS_UNKNOWN,		FEOPSTAT,	5,
	S(OLD) S(NEW) S(SCRATCH) S(UNKNOWN) S(REPLACE));

#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
	SETSPEC(access,		OS_SEQUENTIAL,		FEOPACCS,	2,
	S(DIRECT) S(SEQUENTIAL));
	SETSPEC(position,	OS_ASIS,		FEOPPOSN,	3,
	S(APPEND) S(ASIS) S(REWIND));
#else	/* not __mips and not little endian */
	SETSPEC(access,		OS_SEQUENTIAL,		FEOPACCS,	4,
	S(DIRECT) S(SEQUENTIAL) S(KEYED) S(APPEND));
	if ((_fcdtocp(access) != NULL) && (a.oaccess == OS_OAPPEND)) {
		if (_fcdtocp(position) != NULL) {
			OPNERR(FEOPACCS);	/* Invalid ACCESS */
		}
#ifdef KEY /* Bug 86 */
                /* The Fortran 90 standard does not place a constraint
		 * on the value of 'access=', so there's no need to
		 * issue an error message in normal or -ansi mode (and
		 * "isf90" seems always to be set anyway).  */
#else
		else if (isf90) {
			OPNERR(FEOPACCS);	/* Invalid ACCESS */
		}
#endif /* KEY */
		else {
			a.oposition	= OS_APPEND;
			a.oaccess	= OS_SEQUENTIAL;
		}
	}
	else {
	/* use POSITION= if ACCESS='APPEND' is not provided */
	SETSPEC(position,	OS_ASIS,		FEOPPOSN,	3,
	S(APPEND) S(ASIS) S(REWIND));
	}
#endif	/* not __mips and not little endian */

        formdef	= (a.oaccess == OS_SEQUENTIAL) ? OS_FORMATTED : OS_UNFORMATTED;

	SETSPEC(form,		formdef,		FEOPFORM,	4,
	S(UNFORMATTED) S(FORMATTED) S(BINARY) S(SYSTEM));

	SETSPEC(blank,		OS_NULL,		FEOPBLNK,	2,
	S(ZERO) S(NULL));

	SETSPEC(action,		OS_ACTION_UNSPECIFIED,	FEOPACTB,	3,
	S(READ) S(WRITE) S(READWRITE));

	SETSPEC(delim,		OS_NONE,		FEOPDLMB,	3,
	S(APOSTROPHE) S(QUOTE) S(NONE));

	SETSPEC(pad,		OS_YES,			FEOPPADB,	2,
	S(YES) S(NO));

/*
 *	Diagnose errors.
 */

	if (recl != NULL && a.orecl <= 0)
		OPNERR(FEOPRECL);		/* Invalid RECL */

	if (recl == NULL && a.oaccess == OS_DIRECT)
		OPNERR(FEOPRCRQ);		/* RECL required for direct */

	if (_fcdtocp(blank) != NULL && (a.oform == OS_UNFORMATTED ||
	   a.oform == OS_BINARY || a.oform == OS_SYSTEM))
		OPNERR(FEOPBKIV);		/* BLANK= invalid if unform. */
 
	if (_fcdtocp(delim) != NULL && (a.oform == OS_UNFORMATTED ||
	   a.oform == OS_BINARY || a.oform == OS_SYSTEM))
		OPNERR(FEOPDLMI);		/* DELIM invalid if unform. */

	if (_fcdtocp(pad) != NULL && (a.oform == OS_UNFORMATTED ||
	   a.oform == OS_BINARY || a.oform == OS_SYSTEM))
		OPNERR(FEOPPDIV);		/* PAD= invalid if unformatted*/

	if (_fcdtocp(position) != NULL && a.oaccess == OS_DIRECT)
		OPNERR(FEOPPSIV);		/* POSITION invalid on direct */

/*
 *	Done with OPEN specifiers.
 */
	if (OPEN_UPTR(cup) && cup->ufs == FS_AUX)
		OPNERR(FEOPAUXT);	/* Unit is opened by AQ/MS/DR/WA IO */

	if (OPEN_UPTR(cup) &&
	    (_fcdtocp(file) == NULL || (cup->ufnm != NULL &&
	     strncmp(cup->ufnm, a.ofile, a.ofilelen) == 0))) {
		/*
		 * A re-open of the same file occurs when the FILE= specifier
		 * is present and matches the name with which the file was
		 * originally opened, or if the FILE= specifier is absent
		 * (these are re-opens of the same file by definition).
		 *
		 * In this case, only a subset of the OPEN specifiers
		 * (the BLANK=, PAD=, and DELIM= specifiers) may be provided
	 	 * with values which are different from those currently in 
		 * effect.   Any new value passed with the BLANK=, PAD=, or
		 * DELIM= specifier will go into effect.
	 	 *
		 * An attempt to change the other OPEN specifers is an error.
		 */

		if (_fcdtocp(status) != NULL && a.ostatus != cup->uostatus) {
			if (a.ostatus == OS_NEW && cup->uostatus == OS_OLD) {
				OPNERR(FEOPNNEW); /* STATUS=NEW became OLD */
			}
			else
				OPNERR(FEOPCBNK); /* Can't change STATUS */
		}

		if (_fcdtocp(access) != NULL &&
		    ((a.oaccess == OS_SEQUENTIAL && cup->useq == 0 ) ||
		     (a.oaccess == OS_DIRECT     && cup->useq == 1)   ))
			OPNERR(FEOPCBNK);	/* Can't change ACCESS */

		if (_fcdtocp(form) != NULL &&
		    ((a.oform == OS_FORMATTED   && cup->ufmt == 0) ||
		     (a.oform == OS_UNFORMATTED && cup->ufmt == 1)   ))
			OPNERR(FEOPCBNK);	/* Can't change FORM */

		if (recl != NULL && a.orecl != cup->urecl)
			OPNERR(FEOPCBNK);	/* Can't change RECL */
			
		if (_fcdtocp(position) != NULL && a.oposition != cup->uposition)
			OPNERR(FEOPCBNK);	/* Can't change POSITION */

		if (_fcdtocp(action) != NULL && a.oaction != cup->uaction)
			OPNERR(FEOPCBNK);	/* Can't change ACTION */

		/*
		 * Place into effect any new BLANK=, DELIM=, or PAD= specifier
		 * provided on the OPEN statement.
		 */

		if (_fcdtocp(blank) != NULL)
			cup->ublnk	= (a.oblank == OS_ZERO);

		if (_fcdtocp(delim) != NULL)
			cup->udelim	= a.odelim;	

		if (_fcdtocp(pad)   != NULL)
			cup->upad	= a.opad;	
	}
	else {

		/*
		 * Open the unit.  If the unit is currently connected, it
		 * will be closed and then reopened for the new file.  
		 */

/* KEY: we do want this check */
#if	(!defined(__mips) && !defined(_LITTLE_ENDIAN)) || defined(KEY)
		/*
		 * SGI's F77 and old F90 allowed open with status=NEW,
		 * OLD, or REPLACE without FILE specifier, so we continue
		 * to allow it on MIPS systems.
		 */
		if (a.ostatus == OS_REPLACE && a.ofile == NULL)
			OPNERR(FEOPFNRQ); /* FILE= required for 'REPLACE' */

		if (a.ostatus == OS_OLD && a.ofile == NULL)
			OPNERR(FEOPFNRQ); /* FILE= required for 'OLD' */

		if (a.ostatus == OS_NEW && a.ofile == NULL)
			OPNERR(FEOPFNRQ); /* FILE= required for 'NEW' */
#endif
#ifdef	_CRAYMPP
		/*
		 * This check should be added for CX/CEA someday.
 		 */
		if (a.ostatus == OS_SCRATCH && a.ofile != NULL)
			OPNERR(FEOPFNIV); /* FILE= should not be specified */
#endif


		/*
 		 * We assume that _f_open does not change cfs.f_cu if
 		 * the unit was already open.
		 */
		errn	= _f_open(&cfs, &cup, &a, isf90);
	}

/*
 *	Process results
 */
opn_done:
	error	= (errn != 0) ? IO_ERR : IO_OKAY;

	if (iostat != NULL)
		*iostat	= errn;
	else
		if (error != IO_OKAY && errf == 0)
			if (errn == FEIVUNTO)
				_ferr(&cfs, errn, unum);
			else
				_ferr(&cfs, errn);

	STMT_END(cup, T_OPEN, NULL, NULL);	/* unlock unit */

	OPENUNLOCK();

	return(CFT77_RETVAL(error));
}

/*
 *	_OPEN - Fortran-90 runtime open routine.  Processes an OPEN statement.
 */
int
_OPEN(struct open_spec_list *o)
{
/*
 *	Pass value of 1 in argument #16 to indicate that this is a Fortran-90
 *	OPEN.
 */
	assert ( o->version == 0 );

	return( __OPN(o->unit, o->iostat, (int*)o->err, o->file, o->status,
		     o->access, o->form, o->recl, o->blank, o->position,
		     o->action, o->delim, o->pad, (int)NULL, (int)NULL, 1) );
}



/*
 *	findmatch - does a case-insensitive match of Fortran string fortstring 
 *	with a list of possible values.  The integer code of the matched 
 *	value is returned in *result.
 *
 *	Return value is 1 if a match was made, and 0 otherwise.
 */
static int 
findmatch(_fcd fortstring, int *result, int nval, ...)
{
	va_list ap;
	char	*fstring;       /* Pointer to Fortran string    */
	long	fstrlen;        /* Length of Fortran string     */
	int	_string_cmp();	/* String compare routine in libu */
	char	*next_string;
	int	next_value;
	int	ret, i;

	va_start(ap, nval);

	fstring	= _fcdtocp(fortstring);
	fstrlen	= _fcdlen (fortstring);

	ret	= 0;		/* assume match not found */

	for (i = 0; i < nval; i++) {
		next_string	= va_arg(ap, char *);
		next_value	= va_arg(ap, int);
		if (_string_cmp(next_string, fstring, fstrlen)) {
			*result	= next_value;
			ret	= 1;
			break;
		}
	}

	va_end(ap);

	return(ret);
}

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
/*
 *	_OPEN - IRIX f77 -craylibs runtime open routine.
 *	 Processes an OPEN statement.
 */
int
_OPENF77(struct open_spec_list *o)
{
/*
 *	Pass value of 0 in argument #16 to indicate that this
 *	is a Fortran-77 IRIX OPEN.
 */
	assert ( o->version == 0 );

	return( __OPN(o->unit, o->iostat, (int*)o->err, o->file, o->status,
		     o->access, o->form, o->recl, o->blank, o->position,
		     o->action, o->delim, o->pad, (int)NULL, (int)NULL, (int)NULL) );
}
#endif	/* mips  or little endian */
