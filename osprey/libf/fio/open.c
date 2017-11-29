/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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



#pragma ident "@(#) libf/fio/open.c	92.3	08/02/99 10:37:16"

#include <sys/param.h>	/* PATH_MAX */
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <liberrno.h>
#include <fcntl.h>
#include <unistd.h>
#include <cray/assign.h>
#include <sys/stat.h>
#include <stdio.h>
#include "fio.h"

#define FERROR(cond, n) {	\
	if (!cond)		\
		_ferr(css, n);	\
	else			\
		return(n);	\
}

#define FERROR1(cond, n, p) {	\
	if (!cond)		\
		_ferr(css, n, p); \
	else			\
		return(n);	\
	}

static void freeit(void *);		/* forward reference */

/*
 *	_f_open
 *
 *		Primary Fortran OPEN and implicit open processing routine.
 *		All external file units are connected via this routine.
 *
 *		It is assumed that _openlock is locked upon entry to this
 *		routine.  Upon successful exit from this routine, the unit 
 *		will be locked.
 *
 *	Side effect
 *
 *		*cup_p is assigned the pointer to a newly allocated (and 
 *		locked) unit structure on exit.  On input, *cup_p, if not
 *		null, is a valid open unit.  This unit is closed and unlocked
 *		prior to reassignment of this pointer to the new unit.   Note
 *		that the new unit may be a different unit with the same
 *		number.  The new unit may have thread, process, or application
 *		team scope, depending on what the -P assign option specifies.
 *	
 *	Return value is:
 *
 *		 0 - normal return
 *		>0 - error code if an error occurred
 */

int
_f_open(
	FIOSPTR	css,	/* Fortran statement state			      */
	unit	**cup_p,/* input: pointer to currently open unit.  output:    */
			/* pointer to new unit.				      */
	olist	*olptr,	/* OPEN information 				      */
	int	isf90)	/* 1 if being opened from CF90, 0 if CF77	      */
{
	register short	is_bin;		/* 1 if binary; else 0 		*/
	register short	is_fmt;		/* 1 if formatted; 0 if unformatted */
	register short	is_seq;		/* 1 if sequential; 0 if direct */
	register short	is_sys;		/* 1 if system; else 0 		*/
	register short	no_mem;		/* 1 if malloc() fails */
	register int	aifound;	/* 1 if assign/asgcmd info found */
	register int	errn;		/* Error code */
	register int	gamask;		/* Global assign mask */
	register int	oflags;		/* O_EXCL/O_CREAT */
	register int	P_value;	/* -P option value */
	register int	stdfn;		/* 1 if std file stdin/stdout/stderr */
	register int	stdfnum;	/* standard file descriptor number */
	register int	stat_ok;	/* 1 if statbuf is valid */
	register int	tufs;		/* requested file structure (default) */
	register int	uscope;		/* File scope */
	register unum_t	unum;		/* unit number */
	char		namebuf[MXUNITSZ]; /* buffer to construct file name */
	char		*fname;		/* FILE= specifier or default filename*/
	char		*aname;		/* actual file name */
	char		*atstr;		/* assign attributes string */
	unit		*cup;
	assign_info	ai;
	struct stat	statbuf;

	unum	= olptr->ounit;

	if (! GOOD_UNUM(unum))
		FERROR1(olptr->oerr, FEIVUNIT, unum);

/*
 *	Check for a re-open before initializing any unit table fields.
 */
	if (OPEN_UPTR(*cup_p)) {
		/*
		 * The unit is connected, but we have already checked in
		 * $OPN for reconnection to the same file with unchanged
		 * attributes.  Thus, we know that we may disconnect the unit
		 * here before continuing the set up of the new connection.
		 *
		 * We unlock it so that _alloc_unit may find it again and
		 * lock it.  
		 */

		errn	= _unit_close(*cup_p, CLST_UNSPEC, NULL);

		if (errn != 0)
			FERROR(olptr->oerr, errn);

		_release_cup(*cup_p);		/* unlock the unit */
	}

/*
 *	"aname" receives the actual name to be opened by the system.
 *	It starts out the same as fname, but might later be reassigned
 *	by assign.
 */

	aname	= NULL;
	fname	= NULL;
	stdfn	= 0;
	no_mem	= 0;

	if (olptr->ofile == NULL) {		/* If no name specified */

		if (olptr->ostatus == OS_SCRATCH) {	 /* If SCRATCH */
			int scratchfd;
			/*
			 * Scratch files have no name (see INQUIRE).
			 */
			fname	= NULL;
			aname = strdup("FXXXXXX");
		        scratchfd = mkstemp(aname);
			close(scratchfd); /* because mkstemp opens the file */
		}
		else if (unum == 0 || unum == 5 || unum == 6 ||
			 RSVD_UNUM(unum)) {
			stdfn	= 1;	/* Possible standard file */
			stdfnum	= -1;

			switch (unum) {

			case 5:	/* Connect 5 and 100 to stdin */
			case 100:
				stdfnum	= STDIN_FILENO;
				break;
			case 6:	/* Connect 6 and 101 to stdout */
			case 101:
				stdfnum	= STDOUT_FILENO;
				break;
			case 0:	/* Connect 0 and 102 to stderr/errfile */
			case 102:		/* (see finit.c) */
				stdfnum	= fileno(errfile);
				break;
			default:
				_ferr(css, FEINTUNK);	/* deep weeds */
			}
		}
		else {			/* not scratch nor standard file */

			(void) _fortname(namebuf, unum); /* Make default name */

			fname	= strdup(namebuf);
			aname	= strdup(namebuf);
			no_mem	= (aname == NULL) || (fname == NULL);
		}
	}
	else {				/* Copy user supplied name */
		if ((fname = malloc(olptr->ofilelen + 1)) != NULL) {
			_copy_n_trim(olptr->ofile, olptr->ofilelen, fname);
			aname	= strdup(fname);
		}

		no_mem	= (aname == NULL) || (fname == NULL);
	}

	if (no_mem) {			/* If malloc() failed */

		freeit(aname);
		freeit(fname);

		FERROR(olptr->oerr, FENOMEMY);		/* No memory */
	}


	is_bin	= (olptr->oform == OS_BINARY) ? 1 : 0;
	is_fmt	= (olptr->oform == OS_FORMATTED) ? 1 : 0;
	is_seq	= (olptr->oaccess == OS_SEQUENTIAL ? 1 : 0);
	is_sys	= (olptr->oform == OS_SYSTEM) ? 1 : 0;

/*
 *	The ASN_G_SF/SU/DF/DU masks map to the ACCESS/FORM specifiers on OPEN.
 */
	switch ((is_seq << 3) | is_fmt) {

		case 011:	/* Sequential Formatted */
			gamask	= ASN_G_SF;
			break;

		case 010:	/* Sequential Unformatted */
			gamask	= ASN_G_SU;
			break;

		case 001:	/* Direct Formatted */
			gamask	= ASN_G_DF;
			break;

		case 000:	/* Direct Unformatted */
			gamask	= ASN_G_DU;
			break;
	}

	gamask	= gamask | ASN_G_ALL;
	atstr	= NULL;
	aifound	= _assign_asgcmd_info(fname, unum, gamask, &ai, &atstr,
			     olptr->oerr);
#ifdef KEY /* Bug 4924 */
        /* Ignore "-F f77.mips" if the file is not sequential and thus has no
	 * headers. Otherwise, we would select an ffio layer which gives a
	 * runtime error on non-sequential files. Today f77.mips is the only
	 * value we support; if we supported some other value which permitted
	 * non-sequential access, this test would need to be made more precise.
	 */
        if (!is_seq) {
	  ai.F_filter_flg = 0;
	}
#endif /* KEY Bug 4924 */

	if (aifound == -1) {
		freeit(fname);
		freeit(aname);
		freeit(atstr);
		FERROR(olptr->oerr, errno);
	}

/*
 *	Set up the scoping of this unit.   -P process is default.
 */
	uscope	= AS_PROCESS;		/* actual scope */
	P_value	= AS_PROCESS;		/* -P option value, if any */

	if (aifound == 1 && ai.P_ioscop_flg) {
		uscope	= ai.P_ioscop;
		P_value	= ai.P_ioscop;
		/* Map -P private and -P global to the new spelling */
#ifdef _CRAYMPP
		if (ai.P_ioscop == AS_PRIVATE)
			uscope	= AS_PROCESS;
#else
		if (ai.P_ioscop == AS_PRIVATE)
			uscope	= AS_THREAD;

		if (ai.P_ioscop == AS_GLOBAL)
			uscope	= AS_PROCESS;
#endif
	}

#ifdef	_CRAYMPP
	if (uscope == AS_GLOBAL)
		FERROR(olptr->oerr, FENOGLOB);

	if (uscope == AS_THREAD)
		FERROR(olptr->oerr, FENOTHRD);

	if (uscope == AS_TEAM)
		FERROR(olptr->oerr, FENOTEAM);
#else
	if (uscope == AS_TEAM)
		FERROR(olptr->oerr, FENOTEAM);
#endif

/*
 *	Now that we know the unit number and scope we can get a pointer to the 
 *	unit table.
 */
#ifdef _CRAYMPP
	cup	= _alloc_unit(unum, 1);		/* TEMPORARY */
#else
	cup	= _alloc_unit(unum, (uscope == AS_THREAD));
#endif
	if (cup == NULL)
		FERROR1(olptr->oerr, errno, unum);

	*cup_p		= cup;

/*
 *	Record OPEN specifiers in unit table
 */
	cup->ubinary	= is_bin;
	cup->ufmt	= is_fmt;
	cup->useq	= is_seq;
	cup->usystem	= is_sys;
	cup->ublnk	= (olptr->oblank == OS_ZERO ? 1 : 0);
	cup->uposition	= olptr->oposition;
	cup->uaction	= olptr->oaction;
	cup->udelim	= olptr->odelim;
	cup->upad	= olptr->opad;
	cup->urecl	= olptr->orecl;

/*
 *	Initialize the cf77/f90 mode.  It might be changed in f_asgn() later.
 */
	cup->uft90	= isf90;

	if (aifound == 1 && ai.a_actfil_flg) {
		stdfn	= 0;	/* standard file overridden */

		freeit(aname);
		aname	= strdup(ai.a_actfil);

		if (aname == NULL) {
			freeit(atstr);
			freeit(fname);
			FERROR(olptr->oerr, FENOMEMY);
		}
	}
 
	if (aifound == 1 && ai.D_fildes_flg) {
		stdfn	= 1;	/* indicate standard file */
		stdfnum	= ai.D_fildes;

		freeit(aname);
		aname	= NULL;
	}

/*
 *	Units connected to stdin, stdout, or stderr may not have thread scope
 *	on PVP systems.
 */
#ifdef	_CRAYMPP
	if (stdfn && uscope == AS_TEAM) {
		freeit(fname);
		freeit(aname);
		freeit(atstr);
		FERROR(olptr->oerr, FENOTEAM);
	}
#else
	if (stdfn && uscope == AS_THREAD) {
		freeit(fname);
		freeit(aname);
		freeit(atstr);
		FERROR(olptr->oerr, (P_value==AS_PRIVATE)? FENOPRIV: FENOTHRD);
	}
#endif

/*
 *	Set up cup->urecsize, the maximum record size.  If RECL was
 *	specified (it's required on direct access files; optional
 *	on sequential access files), then RECL becomes the maximum
 *	record size for all formatted I/O on this unit.  Otherwise
 *	we use default values for the maximum record size for both
 *	regular I/O and list-directed/namelist output.
 */

	if (cup->ufmt) {	/* If formatted file */

		if (cup->urecl > 0) {	/* If RECL specified */
			cup->urecsize	= cup->urecl;
			cup->uldwsize	= cup->urecl;
		}
		else {			/* Else set defaults */
			cup->urecsize	= _f_rcsz;
			cup->uldwsize	= _f_ldsz;
		}

		/* Allocate line buffer for formatted files */

		cup->ulinebuf	= (long *) malloc(sizeof(long) *
						(cup->urecsize + 1));

		if (cup->ulinebuf == NULL) {
			freeit(fname);
			freeit(aname);
			freeit(atstr);
			FERROR(olptr->oerr, FENOMEMY);
		}
	}

/*
 *	See if the file exists.  We don't know the filename for sure if FFIO
 *	is being used though.
 */
	errn	= 0;
	stat_ok	= 0;

	if (stdfn) {
		errn	= fstat(stdfnum, &statbuf);
		stat_ok	= 1;
	}
	else if (aifound == 0 || ai.F_filter_flg == 0) {
		errn	= stat(aname, &statbuf);
		stat_ok	= 1;
	}

	/*
	 * ENOENT means the file doesn't exist.  EINTR means the request
	 * was interrupted.  If we got an EINTR error, retry the stat
	 * request a few times.  A persistent EINTR error or any other
	 * stat error besides ENOENT is fatal.
	 *
	 * On UNICOS and UNICOS/mk systems, a EINTR error should never
	 * occur on a stat request... but we've seen some on UNICOS/mk
	 * for a reason the kernel developers do not understand.
	 */

	if (stat_ok && errn == -1) {	/* If we did a stat and it failed */
		register short	retry = 0;

		while (errn == -1 && errno == EINTR && retry++ < 10) {
			if (stdfn)
				errn	= fstat(stdfnum, &statbuf);
			else 
				errn	= stat(aname, &statbuf);
		}

		if (errn == -1) {	/* We have a hard failure */

			stat_ok	= 0;

			if (errno != ENOENT) {	/* If not ENOENT, abort */
				freeit(fname);
				freeit(aname);
				freeit(atstr);
				freeit(cup->ulinebuf);
				FERROR(olptr->oerr, errno);
			}
		}
	}

	/* Select the file structure */

	if (aifound == 1 && (ai.s_fstrct_flg || ai.F_filter_flg)) {
		if (ai.F_filter_flg)
			tufs	= FS_FDC;
		else
			tufs	= ai.s_fstrct;
	}
	else {
		/* Select default file structure */

		if ( cup->ufmt )		/* if formatted */
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
			tufs	= (cup->useq) ? FS_TEXT : FS_UNBLOCKED;
#else
			tufs	= FS_TEXT;
#endif
		else {				/* else unformatted */
#ifdef	_UNICOS
 			tufs	= (cup->useq) ? FS_COS : FS_UNBLOCKED;
#else	/* else NOT _UNICOS */
 			tufs	= (cup->useq) ? FS_F77 : FS_UNBLOCKED;
#endif	/* END _UNICOS */
			if (is_bin || is_sys) {
				/*
				 * Use UNBLOCKED layer for direct or
				 * sequential unformatted IO that does
				 * not contain record control images.
				 * Formatted IO is not allowed (i.e., a) 
				 */
				tufs	= FS_UNBLOCKED;
			}

		}

		/* See if the device is a tape and handle it accordingly */

		if (stat_ok && _gsys_qtape(&statbuf) != 0)
			tufs	= FS_TAPE;
	}
 
	/*
	 * Process the open for standard files (STDIN_FILENO, STDOUT_FILENO
	 * or STDERR_FILENO)
	 */

	if (stdfn) {
		FILE	*stdf;

#if	defined(BUILD_OS_DARWIN)
		switch(stdfnum) {
		case(STDIN_FILENO):
			stdf	= stdin;
			break;
		case(STDOUT_FILENO):
			stdf	= stdout;
			break;
		default:
			stdf	= stderr;
			break;
		}
#elif	defined(_LITTLE_ENDIAN) && !defined(__sv2)
		/* iob not the same on non-sv2 little endian systems.
		 * Use the following:
		 */
		switch(stdfnum) {
		case(STDIN_FILENO):
			stdf	= _IO_stdin;
			break;
		case(STDOUT_FILENO):
			stdf	= _IO_stdout;
			break;
		default:
			stdf	= _IO_stderr;
			break;
		}
#else
		stdf	= &__iob[stdfnum];
#endif

		if (!cup->useq || !cup->ufmt ||
		    (tufs != FS_TEXT && tufs != STD)) {
			freeit(fname);
			freeit(aname);
			freeit(atstr);
			freeit(cup->ulinebuf);
			FERROR(olptr->oerr, FEOPSTFN); /* Std file mismatch */
		}

		if (stdfnum < STDIN_FILENO || stdfnum > STDERR_FILENO) {
			/* Should not happen because assign filters out */
			/* unsupported file descriptor values */
			freeit(fname);
			freeit(aname);
			freeit(atstr);
			freeit(cup->ulinebuf);
			FERROR(olptr->oerr,FEINTUNK);
		}

		if (fileno(stdf) != stdfnum) {
			/* Stdio file has been reopened with an alternate */
			/* file descriptor! */
			freeit(fname);
			freeit(aname);
			freeit(atstr);
			freeit(cup->ulinebuf);
			FERROR(olptr->oerr,FEINTUNK);
		}
		
		cup->ufp.std	= stdf;

		/*
		 * The auxiliary lock protects multiple units connected to
		 * the same standard file.
		 */
		switch (stdfnum) {
			case STDIN_FILENO:
				cup->auxlockp	= &_stdin_lock;
				break;

			case STDOUT_FILENO:
				cup->auxlockp	= &_stdout_lock;
				break;

			case STDERR_FILENO:
				cup->auxlockp	= &_stderr_lock;
				break;
		}

		/*
		 * Standard files have no Fortran name.
		 */

		freeit(aname);
		freeit(fname);
		aname	= NULL;
		fname	= NULL;
	}

	/*
	 * Record the file name and unit number in the unit.  The alfnm field
	 * might be changed later.
	 */

	cup->ufnm	= fname;
	cup->alfnm	= aname;
	cup->uid	= unum;

	/* Process the STATUS specifier */

	cup->uostatus	= olptr->ostatus;

	switch (olptr->ostatus) {

		case OS_UNKNOWN:
		default:
			oflags	= O_CREAT;
			break;
 
		case OS_SCRATCH:
			cup->uscrtch	= 1;
			oflags	= O_CREAT;
			break;

		case OS_OLD:
			oflags	= 0;
			break;

		case OS_NEW:
			/* Unless tape or pipe, file is not allowed to exist */

			if (stat_ok && !S_ISCHR(statbuf.st_mode) &&
			    !S_ISFIFO(statbuf.st_mode)) {
				freeit(fname);
				freeit(aname);
				freeit(atstr);
				freeit(cup->ulinebuf);
				FERROR(olptr->oerr, FEOPFNNX);
			}

			cup->uostatus	= OS_OLD;	/* NEW becomes OLD */

			oflags	= O_EXCL | O_CREAT;
			break;

		case OS_REPLACE:
			/*
			 * Fortran 90 addition to replace existing file with
			 * a new file or create new file if no file exists.
			 * The error check on use of filename with the new
			 * option is in opn.c.
			 */
			cup->uostatus	= OS_OLD;  /* REPLACE becomes OLD */

#ifdef KEY /* Bug 7749 */
			/* Later there's code to call _unit_trunc if the
			 * olptr->ostatus == OS_REPLACE, but _unit_trunc
			 * does nothing for direct access files (because
			 * it's used in other contexts where truncation
			 * is wrong for direct access.) So we must arrange
			 * to truncate when we open the file. */
			oflags	= O_CREAT | O_TRUNC;
#else /* KEY Bug 7749 */
			oflags	= O_CREAT;
#endif /* KEY Bug 7749 */
			break;

	} /* switch */

	/* Open the file; use whatever permissions are available */

	errn	= _f_opn(aname, cup, css, tufs, aifound, &ai, &statbuf, 
			stat_ok, olptr->oerr, oflags);

	if (errn != OK) {
		errn	= errno;
		/*
		 * Map EEXIST to something more meaningfull to Fortran users.
		 */
		if (errn == EEXIST)
			errn	= FEOPFNNX;

		goto open_error;
	}

#ifdef	_CRAYMPP
	/* Don't trunc() standard files on the MPP, since they're shared */
	if (stdfn)
		cup->utrunc	= 0;	/* Clear trunc() flag */
#endif

/*
 *	Check that another unit hasn't been opened to the same file
 */ 
	_set_device_and_inode(cup->usysfd, &cup->udevice, &cup->uinode);

	errn	= _uniqinod(cup, (aifound ? &ai : NULL));

	if (errn != 0)
		goto open_error;

/*
 *	Check that the same unit number is not opened at more than one
 *	scoping level.
 */
	errn	= _mixed_scope(cup);

	if (errn != 0)
		goto open_error;

/***************************************************************************
 *									   *
 *	The unit is now a valid connected unit.				   *
 *									   *
 ***************************************************************************/

/*
 *	Truncate the file to zero size if STATUS='REPLACE'.
 */
	if (olptr->ostatus == OS_REPLACE) {
		errn	= _unit_trunc(cup);

		if (errn != 0)
			goto open_error;

		/*
		 * Some FDC layers cannot read after write, and
		 * unit_trunc is like a write to FDC layers; so
		 * do a seek to clear the last-operation-was-a-write
		 * status.
		 */
		if (cup->ufs == FS_FDC) {
			struct ffsw	fst;

			XRCALL(cup->ufp.fdc, seekrtn) cup->ufp.fdc, 0, 0, &fst);
		}

		cup->udamax	= 0;
	}

/*
 *	Position the file as specified with the POSITION specifier 
 */
	switch (cup->uposition) {

		case OS_REWIND:
		case OS_ASIS:	/* do nothing */
			break;

		case OS_APPEND:	/* Position to end of file. */

			/* position to end */
			{
				int	neg1;

				/* Bug 4478. Ignore POSITION='APPEND' for things that can't seek. */

				if (cup->useek) {

				  neg1	= -1;
				  errn	= _setpos(css, cup, &neg1, 1);

				  if (errn != 0)
					goto open_error;

				  errn	= _unit_bksp(cup);

				  if (errn != 0)
					goto open_error;
				}
			}
			break;

	} /* switch */

/*
 *	If assigned with -t, unlink it now.
 */
	if (cup->utmpfil) {
		errn	= _unit_scratch(cup);

		if (errn != 0)
			goto open_error;
	}

/*
 *	Set up OK flags to speed up error checking in I/O statements.
 */
	_set_ok_flags(cup);

	if (FORTSTATS) {
		/*
		 * Now initialize statistics counting for this unit.
		 */
		if (_ft_stopen(cup, atstr) == -1) {
			errn	= errno;
			goto open_error;
		}
	}

	freeit(atstr);

/*
 *	The auxliary lock needs to be locked.   If this is a reopen,
 *	_unit_close() unlocked it.   Otherwise it has not yet been locked. 
 */

	if (cup->auxlockp != NULL)
		MEM_LOCK(cup->auxlockp);

	return(0);

	/* Process error(s) */

open_error:		/* Error code is in errn */
	freeit(atstr);
	(void)_unit_close(cup, CLST_UNSPEC, NULL);

	FERROR(olptr->oerr, errn);

	return(0);	/* not reached */
}

/*
 *	Free a pointer if it's non-null.
 */
static void
freeit(void *ptr)
{
	if (ptr != NULL)
		free(ptr);
}

/*
 *	_set_ok_flags
 *
 *		Initialize all cup->ok_* flags in a unit.  This function is 
 *		called by _f_open() and _init_internal_unit().
 */

void
_set_ok_flags(unit *cup)
{
	cup->ok_wr_seq_fmt = _get_mismatch_error(1, T_WSF, cup, NULL) == 0;
	cup->ok_wr_seq_unf = _get_mismatch_error(1, T_WSU, cup, NULL) == 0;
	cup->ok_wr_dir_fmt = _get_mismatch_error(1, T_WDF, cup, NULL) == 0;
	cup->ok_wr_dir_unf = _get_mismatch_error(1, T_WDU, cup, NULL) == 0;

	cup->ok_rd_seq_fmt = _get_mismatch_error(1, T_RSF, cup, NULL) == 0;
	cup->ok_rd_seq_unf = _get_mismatch_error(1, T_RSU, cup, NULL) == 0;
	cup->ok_rd_dir_fmt = _get_mismatch_error(1, T_RDF, cup, NULL) == 0;
	cup->ok_rd_dir_unf = _get_mismatch_error(1, T_RDU, cup, NULL) == 0;

	return;
}

/*
 *	_get_mismatch_error
 *
 *		This function evaluates the unchanging properties of
 *		a unit (ie. cup->ufmt, etc.), to determine whether particular
 *		types of I/O statements are valid for this unit.  This routine
 *		is called once at open time to set up the cup->ok_* fields in
 *		the unit table.  They are also called if an error is detected
 *		in _FWF, _FRF, _FWU, or _FRU.
 *
 *	Return Value:
 *
 *		The error number.  If the noabort argument is 0, then this
 *		function calls _ferr().
 */

int
_get_mismatch_error(
	int	noabort,/* nonzero if we should not abort on error */
	int	iost,	/* type of I/O statement (T_WSF, ...) */
	unit	*cup,	/* unit pointer */
	FIOSPTR	css)	/* Fortran I/O statement state; May be NULL if
			 * noabort is nonzero */ 
{
	register int	errn = 0;

	if (cup->ufs == FS_AUX) {
		errn	= FEMIXAUX;
		goto ret;
	}

	/* if write statement, else read statement ... */

	if (iost & TF_WRITE) {
		if ((cup->uaction & OS_WRITE) == 0) {
			errn	= FENOWRIT;	/* No write permission */
			goto ret;
		}
	}
	else {
		if ((cup->uaction & OS_READ) == 0) {
			errn	= FENOREAD;	/* No read permission */
			goto ret;
		}
	}

	/* if formatted I/O statement, else unformatted ... */

	if (iost & TF_FMT) {
		if (!cup->ufmt) {
			errn	= FEFMTTIV;	/* Formatted not allowed */
			goto ret;
		}
	}
	else {
		if (cup->ufmt) {
			errn	= FEUNFMIV;	/* Unformatted not allowed */
			goto ret;
		}
	}

	/* if direct access I/O statement, else sequential ... */

	if (iost == T_WDF || iost == T_WDU || iost == T_RDF || iost == T_RDU) {
		if (cup->useq) {
			errn	= FEDIRTIV;	/* Direct access not allowed */
			goto ret;
		}
	} else {
		if (cup->useq == 0) {
			errn	= FESEQTIV;	/* Sequential not allowed */
			goto ret;
		}
	}

ret:
	if (noabort)
		return(errn);
	else {
		if (errn == 0)
			errn	= FEINTUNK;	/* force an abort */
	
		_ferr(css, errn);
	}

	return(FEINTUNK);	/* not reached */
}

#ifdef	_UNICOS
/*
 *	The module open.c must have hard references to any routines which
 *	are needed for Fortran I/O initialization.  The functions referenced
 *	here are accessed with soft references by $START and are called at
 *	startup time only if they have hard reverences.
 *
 *	It is assumed that any Fortran program which does Fortran I/O has
 *	hard references to some entry point in this module (usually _f_open
 *	through _implicit_open).  Internal file I/O processing is integrated 
 *	with the external I/O path, so internal Fortran I/O statements DO 
 *	cause hard references to open.c.
 *
 *	__fio_hardrefs() should NEVER be called.
 */

void
__fio_hardrefs()
{
	(void) _finit();	/* Has a soft reference in $START.  It is
				 * called only for programs which do Fortran
				 * I/O. */

#if	defined(_CRAY1) 
	(void) _repriev();	/* A hard reference of _repriev causes the
				 * appropriate style of program abort
				 * handling for programs which do Fortran
				 * I/O. */
#endif
}

#endif	/* _UNICOS */
