/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

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


#pragma ident "@(#) libu/trbk/c1/reprvtr.c	92.1	07/01/99 13:48:28"
#include <stdio.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdlib.h>
#include <signal.h>
#include <time.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/category.h>
#include <sys/machd.h>
#include <sys/xp.h>
#include <cray/signal.h>
#include <cray/stk.h>

#define	MAX_ENT_LEN	32	/* Maximum entry point name length + 1 */
#define CORE			/* Enable traceback and coredump option */
#define VECPRINT		/* Enable V-register printout */

extern int	_argc;
extern char	**_argv;
extern char	*sys_siglist[];
extern void	_aborthandle();
extern long	cpused();

#define	EOS	'\0'

/*
 *  _reprinit() - registers signals found in trbksigs bitmask,
 *  using _repriev() as the signal handler
 */

#define ALLTRBK		sigmask(SIGINT)|sigmask(SIGILL)|sigmask(SIGFPE)|\
			sigmask(SIGPRE)|sigmask(SIGORE)|sigmask(SIGSYS)|\
			sigmask(SIGTERM)|sigmask(SIGUME)|sigmask(SIGCPULIM)

#define TRBKSIGS	ALLTRBK|SIG_DMPDFL|sigmask(SIGHUP)

static long trbksigs	= TRBKSIGS;	/* bitmask of sigs to catch */
static long trbkcore	= 0;		/* do traceback AND core if on */
static long trbkvec	= 0;		/* print vector regs if on */
static long trbkdpth	= 25;		/* traceback depth  */

void
_reprinit()
{
	extern void	_repriev();
	register long	sig, mask;
	char		*env, *ptr;

	/* "TRACEBK2" must be set to do traceback */

	if (((env = getenv("TRACEBK2")) == 0) || atol(env) == 0)
		return;
	/*
	 *  Check out switches for traceback (all off by default):
	 *  TRBKCORE - do traceback AND core if on
	 *  TRBKVEC - print out vector registers w/ traceback
	 */

	if (env = getenv("TRBKCORE"))
		trbkcore	= atol(env);

	if (env = getenv("TRBKVEC"))
		trbkvec		= atol(env);

	if (env = getenv("TRBKDPTH"))
		trbkdpth	= atol(env);

	if (trbkdpth > 50)
		trbkdpth	= 25;

	/*
	 *  Add/subtract signals from trbksigs bitmask, using the
	 *  TRBKSIGS environment variable.  This variable has
	 *  the format 'TRBKSIGS=3,4,-10,-12'; negative numbers
	 *  mean remove the signal number from the bitmask.
	 */

	mask	= trbksigs;

	if (env = getenv("TRBKSIGS")) {
		while ((sig = strtol(env, &ptr, 10)) && sig > -NSIG && sig < NSIG) {
			if (sig > 0)
				mask	= mask | sigmask(sig);
			else
				mask	= mask & ~sigmask(-sig);

			env	= ++ptr;
		}
	}
	/*
	 *  Register signals in bitmask with system;
	 *  note that the signal will revert to dump on receipt
	 */

	for (sig = 1; mask != 0; sig++, mask >>= 1) {
		if ((mask & 01) == 0)
			continue;
		if (__sigctl(SCTL_REG, sig, _repriev) == SCTL_IGN)
			__sigctl(SCTL_IGN, sig, 0);
	}

	return;
}

/*
 *	Like printf(3C), except if the 'errout' variable is set,
 *	it will print its arguments to both stdout and stderr
 */

static int errout;		/* set if stdout != stderr */

static void
print(char *format, ...)
{
	va_list		ap;
	extern int	_doprnt();

	va_start(ap, format);
	_doprnt(format, ap, stdout);

	if (errout)
		_doprnt(format, ap, stderr);

	va_end(ap);

	return;
}

/*
 *	Print out the registers, a traceback, and other
 *	relevant information from a captured signal
 */

_reprvtr(struct sigarea_lib *rd)
{
	struct stack_frame	*callptr, *myptr;
	long			argcnt, baseaddr, lineno;
	xp_t			*realxp;
	register short		m1, m2, m3, m4;
	register short		la;
	register long		byreg, *arg, mask, mtstat, sig, sreg, vreg;
#ifdef	CORE
	register long		addrw, addrc;
#endif
	register double		rate;
	register char		ch, *msg, *p1, *p2, *argvsv;
	char			callee[MAX_ENT_LEN];
	char			string[20];
	struct stat		stdstat;

	/* Register all traceback signals for core-dump */

	mask	= trbksigs;

	for (sig = 1; mask != 0; sig++, mask >>= 1) {
		if (mask & 01)
			__sigctl(SCTL_DMP, sig, 0);
	}

	/* Check if stdout and stderr are the same!  */

	(void) fstat(1, &stdstat);

	sreg	= stdstat.st_ino;
	vreg	= stdstat.st_dev;

	(void) fstat(2, &stdstat);

	errout	= ((sreg != stdstat.st_ino) || (vreg != stdstat.st_dev));

	/* Set up miscellaneous variables for print */

	la	= (long) sbreak(0);
	mtstat	= 0;
	callptr	= (struct stack_frame *)rd->saveb[2];
	myptr	= callptr->prev;

	_subrnm(callptr, callee, &baseaddr, &argcnt, &lineno, la);

	realxp	= (xp_t *)&(rd->xp);
	sreg	= realxp->pn;
	m1	= (sreg >= 0 && sreg < MAXCPUS) ? (sreg + 'A') : '?';
	byreg	= 0;
	sreg	= rd->paddr;
	m4	= sreg;
	m2	= (m4 >> 2);
	m3	= (m4 & 3) + 'a';
	m4	= rd->signum;

	switch (m4) {
	case -1:
		msg	= "Unknown error condition";
		break;
	case -2:
		byreg	= 1;
		msg	= "MathLib exception";
		break;
	case -3:
		msg	= "SciLib exception";
		break;
	case -4:
		msg	= "Library exception";
		break;
	default:
		if (m4 > 0 && m4 < NSIG)
			msg	= sys_siglist[m4];
		else
			msg	= "";
		break;
	}

	/* Fetch the command name (remove unprintables)  */

	argvsv	= _argv[0];
	p1	= argvsv;
	p2	= string;

	while ((*p2 = *p1) && ((p2 - string) < 8)) {
		if (iscntrl(*p2))
			*p2	= '*';
		++p1;
		++p2;
	}

	if ((*p1) || (m2 > 077777)) {
		string[0]	= EOS;
		ch	= ' ';
	} else {
		ch	= '\"';
	}

	_argv[0]	= argvsv;

	/* Print the error message headline  */

	print(" %c%s%c %s: CPU-%c stopped at P = 0%o%c ",
	       ch, string, ch, msg, m1, m2, m3);
#ifdef	CORE
	addrw	= m2;
	addrc	= m3;
#endif

	m3	= sreg;
	m2	= baseaddr;

	if ((sreg == 0) || ((m3 = m3 - m2) < 1) || (callee[0] == EOS))
		print("in \"%s\"\n", callee);
	else {
		m2	= (m3 >> 2);
		m3	= (m3 & 3) + 'a';
		print("= \"%s\"+%o%c\n", callee, m2, m3);
	}

	/* Print A and S registers, and VL, VM (VM1), LA */

	_printreg(stdout, &(rd->xp.A0), &(rd->xp.s0),
		  0, rd->savevl, rd->savevm, la, rd->savevm1);

	if (errout)
		_printreg(stdout, &(rd->xp.A0), &(rd->xp.s0),
			  0, rd->savevl, rd->savevm, la, rd->savevm1);

	/* Print B and T registers  */

	_printreg(stdout, &(rd->saveb[0]), &(rd->savet[0]), 1);

	if (errout)
		_printreg(stdout, &(rd->saveb[0]), &(rd->savet[0]), 1);

#ifdef	VECPRINT
	if (trbkvec) {	/* Print the Vector registers  */

		_printvec(stdout, &(rd->savev[0][0]), 0, 7, rd->savevl);

		if (errout)
			_printvec(stdout, &(rd->savev[0][0]), 0, 7, rd->savevl);
	}
#endif /* VECPRINT */

	myptr	= (struct stack_frame *)rd->saveb[066];

	__tracebk(stdout, myptr, callptr, la, trbkdpth, byreg, -1, 1);

	if (errout)
		__tracebk(stderr, myptr, callptr, la, trbkdpth, byreg, -1, 1);

	/* Print CP-time and command line  */

	if ((rate = ((double) cpused() / CLK_TCK)) > 10.0)
		print(" Elapsed CP: %.3fs", rate);
	else
		print(" Elapsed CP: %.6fs", rate);

	arg	= (long *) *_argv;
	sreg	= (long) arg;

	if (_argc > 0 && _argc < 12 && sreg < la && **_argv != EOS) {

		printf(",   Command Line = \"");

		while (_argc-- > 0)
			printf("%s%s", *_argv++, _argc ? " " : "");

		putc('"', stdout);
	}

	putc('\n', stdout);

	if (errout)
		fprintf(stderr, "    <Registers and Traceback also written to \"stdout\">\n");

	_argv[0]	= argvsv;

#ifdef	CORE
	if (trbkcore) {
		print("\n CPU stopped at P = 0%o%c:  Buffers will be flushed and\n a core file written (via \"Error exit\").\n\n", addrw, addrc);
		_aborthandle();	/* do cleanup routines */
		return(0);
	}
#endif
	print("\n No core file requested, terminating job using SIGKILL.\n\n");

	_aborthandle();		/* do cleanup routines */

	killm(C_PROC,0,SIGKILL);
}
