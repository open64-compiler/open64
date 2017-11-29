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



#pragma ident "@(#) libf/fio/finit.c	92.4	11/16/99 15:43:33"

/*
 *	This module contains Fortran initialization routines.   There
 *	are three times Fortran initialization routines are called:
 *
 *	1) Called at first I/O
 *		_initialize_e_fortran_io()
 *		_initialize_i_fortran_io()
 *	2) Called from $START().
 *		_finit() on PVP and MPP systems
 *		_F90_INIT() on Sparc systems
 *		_initialize_e_fortran_io() on all systems
 *	3) Called from Fortran main program prologue code.
 *		f$init() on PVP and MPP systems
 *
 *	Note that _initialize_e_fortran_io() is called in startup/prologue
 *	code and at first I/O.   This is because on Sparc systems we might
 *	have a C main program which calls Fortran subroutines and the
 *	startup/prologue initializations would be bypassed.
 */

#ifdef	_UNICOS
#include <infoblk.h>
#include <sys/category.h>
#endif
#if	defined(_LITTLE_ENDIAN)
#include <stdlib.h>
#include <cray/portdefs.h>
#else		/* little endian */
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include <cray/mtlock.h>
#if	defined(_CRAYMPP) && defined(SA_SIGINFO)
#define GOT_SIGINFO
#include <siginfo.h>
#endif
#include "fio.h"
#endif		/* little endian */

#ifdef	_UNICOS
#pragma _CRI soft _lwrite_setup
#elif	!defined(_LITTLE_ENDIAN)
#pragma weak _lwrite_setup
#endif
extern	void	_lwrite_setup(void);	/* List-directed write setup routine */

#ifdef	_UNICOS
#pragma _CRI soft _wf_setup
#elif	!defined(_LITTLE_ENDIAN)
#pragma weak _wf_setup
#endif
extern	void	_wf_setup(void);	/* Formatted write setup routine */
extern	void	_fcleanup(void);	/* I/O cleanup routine */
#ifdef	__mips
extern	void	_fortclean(void);		/* flush stdio under lock */
#endif

/* Forward and external references */

#ifdef	GOT_SIGINFO
static void _f_sig(int, siginfo_t *, void *);
#else
static void _f_sig(int);
#endif
extern	void	_aborthandle(void);	/* Cleanup routine */
#ifdef	_CRAYMPP
/* _f_stopsig is referenced in libc/gen/exit.c */
void _f_stopsig(int);
extern void _abortcatch(int);
#endif

#ifdef	_CRAY1
#pragma	_CRI soft $WBUFLN, $RBUFLN
extern	int	$WBUFLN, $RBUFLN;
_f_int	$RFDCOM[1];
_f_int	$WFDCOM[1];
#endif	/* _CRAY1 */

#ifndef	_ABSOFT
extern	char	*sys_siglist[];		/* Array of signal messages */
#endif

extern	int	__fdctrace_enable;	/* FDC trace flag */

short	_fortran_io_is_init;	/* set to 1 by _initialize_fortran_io() */
short	_e_fortran_io_is_init;	/* set to 1 by _initialize_e_fortran_io() */
short	_i_fortran_io_is_init;	/* set to 1 by _initialize_i_fortran_io() */
short	_f_abort;		/* set to 1 by _f_sig() signal handler */

#ifdef	_CRAYMPP
/* These variables and defines are used for handling stopall */
volatile int _infio;
volatile int _needtostop;
extern int G@INTIO;
#endif

/*
 *	_initialize_fortran_io
 *
 *		Called the first time any external or internal Fortran unit is 
 *		connected or used.
 */
void
_initialize_fortran_io(void)
{
	_fortran_io_is_init	= 1;

	/* Set up the precision used for list-directed floating point output */

	if (LOADED(_lwrite_setup))
		_lwrite_setup();	

	/* Set up formatted write behavior */

	if (LOADED(_wf_setup))
		_wf_setup();	

#ifdef  _CRAY1
	/*
	 * $RBUFLN and $WBUFLN were external symbols controlling the
	 * length of the read and write formatted line buffers,
	 * respectively.  The user could increase the maximum allowable
	 * formatted record size by increasing the value of these
	 * symbols at load time with segldr.  Now, we simulate that
	 * behavior by increasing _f_rcsz if these symbols are defined
	 * and larger than _f_rcsz.  For historic reasons, this is done
	 * only on the CX/CEA.
	 *
	 * The $RFDCOM and $WFDCOM common blocks used to contain the
	 * line buffer.  We keep an existing 1-word block so that users
	 * can change it and still get a similar caution message.
	 *
	 * This crud can be deprecated someday since we now allow RECL
	 * on sequential files, which can accomplish the same thing.
	 */

	if (LOADED_DATA(&$RBUFLN))
		if (_VALUE($RBUFLN) > _f_rcsz)
			_f_rcsz	= _VALUE($RBUFLN);

	if (LOADED_DATA(&$WBUFLN))
		if (_VALUE($WBUFLN) > _f_rcsz)
			_f_rcsz	= _VALUE($WBUFLN);
#endif	/* _CRAY1 */

	return;
}

/*
 *	_initialize_e_fortran_io
 *
 *	Called the first time any external Fortran unit is connected.
 *
 *	Assumes that _openlock is locked to prevent multiple concurrent calls.
 */
void
_initialize_e_fortran_io(void)
{
	_e_fortran_io_is_init	= 1;

	if (! _fortran_io_is_init)
		_initialize_fortran_io();

	/* Register for cleanup routine at exit and abort time */

	(void) atexit(_fcleanup);
#ifdef	__mips
	(void) __ateachexit(_fortclean);
#endif
#ifdef	_UNICOS
	(void) atabort(_fcleanup);
#endif	/* _UNICOS */

	/* Conditionally set FFIO tracing */

#ifdef	DEBUG
	if (__fdctrace_enable < 0) {
		char	*estr;

		estr	= getenv("FDCTRACE");

		__fdctrace_enable	= (estr == NULL) ? 0 : atol(estr);
	}
#endif	/* DEBUG */

	return;
}

/*
 *	_initialize_i_fortran_io
 *
 *	Called the first time any internal Fortran unit is accessed.
 */
void
_initialize_i_fortran_io(void)
{
	_i_fortran_io_is_init	= 1;

	if (! _fortran_io_is_init)
		_initialize_fortran_io();

	return;
}

#ifdef	_UNICOS

/*
 *	_finit 
 *
 *	Fortran library initialization routine; called by $START().
 */
void
_finit(void)
{
/*
 *	Call _initialize_e_fortran_io here to ensure that atexit(_fcleanup)
 *	is called before any user atexit() call is made.  
 */
	_initialize_e_fortran_io();	
	return;
}

/*
 *	f$init
 *
 *	Called by the prologue in PVP and MPP Fortran main programs.
 *	It is here that we do the funny signal catching which ensures that 
 *	a traceback is printed when some specific signals are received.
 */
void
f$init(void)
{
	register short	catchem;
	char		*trace;

	catchem	= 1;

	/*
	 * If "TRACEBK" == 0 or "TRACEBK2" != 0 then don't do traceback.
	 */

	if (((trace = getenv("TRACEBK")) && atol(trace) == 0) ||
	    ((trace = getenv("TRACEBK2")) && atol(trace) != 0))
		catchem	= 0;
#ifdef	_CRAYMPP
	/*
	 * Always register for SIGBUFIO; this is used by STOP and
	 * globalexit() to get all PEs to clean up and exit.
	 */
	if (_num_pes() > 1) {
		struct sigaction act, oact;

		act.sa_handler	= _f_stopsig;
		act.sa_mask	= 0;
		act.sa_flags	= SA_RESETHAND | SA_CLEARPEND ;

		(void) sigaction(SIGBUFIO, &act, &oact);

		if (oact.sa_handler != SIG_DFL &&
		    oact.sa_handler != _f_stopsig)
			(void) sigaction(SIGBUFIO, &oact, 0);
	}
#endif
        if (catchem) {
#ifndef	SIGSMCE
#define SIGSMCE 38
#endif
#ifndef SIGAPTEAM
#define SIGAPTEAM 39
#endif
		unsigned long		mask;
		struct sigaction	act, oact;
		register int		sig;

                mask		= (SIG_DMPDFL | sigmask(SIGHUP) |
				  sigmask(SIGINT) | sigmask(SIGTERM) |
				  sigmask(SIGCPULIM) | sigmask(SIGPIPE) |
				  sigmask(SIGSMCE)) | sigmask(SIGAPTEAM)
				  & ~SIG_CANTMASK;
#ifdef	GOT_SIGINFO
                act.sa_sigaction	= _f_sig;
#else
                act.sa_handler	= _f_sig;
#endif
                act.sa_mask	= 0;
                act.sa_flags	= SA_RESETHAND | SA_NODEFER | SA_CLEARPEND;
#ifdef	GOT_SIGINFO
                act.sa_flags	|= SA_SIGINFO;
#endif

                for (sig = 1; mask != 0; sig++, mask >>= 1)
                        if (mask & 01) {
                                (void) sigaction(sig, &act, &oact);
#ifdef	_CRAYMPP
                                if (oact.sa_handler != SIG_DFL &&
				    oact.sa_handler != _abortcatch)
#else
                                if (oact.sa_handler != SIG_DFL)
#endif
                                        (void) sigaction(sig, &oact, 0);
                        }
        }

#ifdef	_CRAYMPP
	_barrier();		/* let all PEs catch up */
#endif

	return;
} 

#endif	/* _UNICOS */

/*
 *	_f_sig() is the Fortran run-time signal handler.
 */

#if	defined(_UNICOS) && !(defined(GOT_SIGINFO) || defined(_CRAYMPP))

static	short	_f_gotsig	= 0;		/* In _f_sig() flag word */
static	long	_f_siglock	= 0;		/* Lock word for _f_sig() */

static void
_f_sig(int sig)
{
	sigset_t	set;

	(void) sigoff();		/* Turn signals off */

	_semclr(2);			/* Clear TSKLK for lock calls */

	MEM_LOCK(&_f_siglock);		/* Protect global data update */

	if (_f_gotsig == 0) {		/* If no one has been here yet */

		_f_gotsig	= 1;	/* Close the door */
		_f_abort	= 1;	/* Indicate abnormal abort */

		MEM_UNLOCK(&_f_siglock);

		(void) sigon();
		(void) fflush(stdout);
		(void) fflush(stderr);	/* Just in case */

		if (sig > 0 && sig < NSIG) {
			(void) write(fileno(stderr), sys_siglist[sig],
					strlen(sys_siglist[sig]));
			(void) write(fileno(stderr), "\n", 1);
		}

		(void) sigtrbk(stderr);

		_aborthandle();		/* Do cleanup routines */

	}
	else {				/* Someone is already in */

		MEM_UNLOCK(&_f_siglock);

		(void) sigon();
	}

	set	= sigmask(sig);			/* Block signal */

	(void) sigprocmask(SIG_BLOCK, &set, NULL);
	(void) killm(C_PROC, 0, sig);		/* Resend signal */

	return;
}

#elif	defined(_CRAYMPP)

static DECL_LOCK(_f_siglock)		/* MPP: Lock word for _f_sig() */

void
_f_stopsig(int sig)
{
	/* Set G@INTIO so we do not retry any call that failed with EINTR */

	G@INTIO = 1;

	if (_infio) {
		/* We are in some i/o statement */
		/* Wait till we are finished to call _fcleanup */
		/* The STMT_BEGIN macro sets _infio and STMT_END */
		/* clears it. STMT_END checks to see if _needtostop */
		/* is set - if so, it calls this routine */
		struct sigaction act, oact;

		_needtostop	= 1;
		act.sa_handler	= _f_stopsig;
		act.sa_mask	= 0;
		act.sa_flags	= SA_RESETHAND | SA_CLEARPEND ;

		(void) sigaction(SIGBUFIO, &act, &oact);
		return;
	}

	_fcleanup();			/* all PEs cleanup and exit */

	exit(0);
}

#ifdef	GOT_SIGINFO
static void
_f_sig(int sig, siginfo_t *sip, void *ctx)
#else
static void
_f_sig(int sig)
#endif
{
	sigset_t	set;
	register long	locked;
	register long	start;
	register long	wait;
	char		buf[50];

	(void) sigoff();

	locked		= _shmem_test_lock(&_f_siglock);
	_f_abort	= 1;	/* Indicate abnormal abort */

	if (!locked) {
		/*
		 * The first PE in _f_sig prints out the signal,
		 * does a traceback, and runs cleanup routines
		 */
		(void) sigon();
		(void) fflush(stdout);
		(void) fflush(stderr);
#ifdef	GOT_SIGINFO
		psiginfo(sip, "SIGNAL");
#else
		if (sig > 0 && sig < NSIG) {
			(void) strncpy(buf, sys_siglist[sig], sizeof(buf) - 1);
			(void) strcat(buf, "\n");
			(void) write(fileno(stderr), buf, strlen(buf));
		}
#endif
		(void) sigtrbk(stderr);
		_aborthandle();
	} else {
		/*
		 * subsequent PEs in _f_sig call cleanup routines,
		 * synchronize on a barrier (UNICOS-MAX only),
		 * then call _localexit().
		 */
		(void) sigon();
		_aborthandle();
#ifdef	_UNICOS_MAX
		_barrier();
#endif
		_localexit(EXIT_FAILURE);
	}
	/*
	 * Under UNICOS-MAX, the first PE in _f_sig() blocks the
	 * signal, then re-sends the signal, waits for awhile
	 * to see if the other PEs have cleaned up, then returns
	 * from the signal handler to kill the application
	 *
	 * Under UNICOS/mk, we block the signal and re-send it,
	 * then exit the signal handler.  The kernel will restore
	 * the registers to the point-of-interrupt and restore
	 * the old signal mask (which has the signal unmasked).
	 * The calling PE will die with the signal; this will
	 * cause a SIGAPTEAM to be sent to all other PEs, which
	 * will bring them into this routine to clean up and exit.
	 */
	set	= sigmask(sig);

	(void) sigprocmask(SIG_BLOCK, &set, NULL);
	(void) killm(C_PROC, 0, sig);

#ifdef	_UNICOS_MAX
	_set_barrier();
	start	= _rtc();
	wait	= start + CLK_TCK;

	while (_rtc() >= start && _rtc() < wait) {
		if (_test_barrier())
			break;
	}
#endif

	return;
}
#endif	/* _CRAYMPP */

#ifdef	_SOLARIS
/*
 *      _F90_INIT():
 *
 *	The Fortran 90 library initialization routine for Sparc.
 *	It is called by the CRI startup code on Sparc for f90 programs.
 */

void
_F90_INIT()
{

/*
 *	We call _initialize_e_fortran_io here to ensure that atexit(_fcleanup)
 *	is called before any user atexit() call is made.  
 */
	_initialize_e_fortran_io();	

	return;
}

#endif	/* _SOLARIS */
