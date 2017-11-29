/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
 */

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


/* $Header: /proj/osprey/CVS/open64/osprey1.0/libF77/main.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
/*	3.0 SID #	1.2	*/
/* STARTUP PROCEDURE FOR UNIX FORTRAN PROGRAMS */

#include <stdio.h>
#include <signal.h>
#include <stdlib.h>
#include <ulocks.h>
#include <unistd.h>
#include "cmplrs/host.h"

int f77argc;
char **f77argv;
extern int __trap_fpe_override;
extern void s_abort(void), MAIN__(void), f_exit(void);

/* To get a nice error instead of an RLD error */
#pragma weak MAIN__ = ___MAIN__ 

extern void     __mp_check_setup(void);
#pragma weak __mp_check_setup
static void (*__mp_ptr)() = __mp_check_setup;

static void sigdie(register char *, int32 );

static void sigfdie(void), sigidie(void), sigqdie(void), sigindie(void),
    sigtdie(void);	/* DAG */

main(int argc, char **argv, char **arge)
{

	f77argc = argc;
	f77argv = argv;
#ifdef sgi
/* do not override signal handling for trap handler,
   set by crt1text: readenv_sigfpe */

	if (! __trap_fpe_override )
		signal(SIGFPE, sigfdie);	/* ignore underflow, enable overflow */
#else
	signal(SIGFPE, sigfdie);	/* ignore underflow, enable overflow */
#endif
	signal(SIGIOT, sigidie);
	if( signal(SIGQUIT,sigqdie) == SIG_IGN ) signal(SIGQUIT, SIG_IGN);	/* DAG */
	if( signal(SIGINT, sigindie) == SIG_IGN ) signal(SIGINT, SIG_IGN);
	signal(SIGTERM,sigtdie);

/* Unresolved weak symbols are given the value zero */
	if (__mp_ptr != 0) (*__mp_ptr)();

	MAIN__();
#ifndef FTN90_IO
	f_exit();
#endif
	exit(0);
	return (0); /* make compiler shut up */
}


static void	/* DAG */
sigfdie(void)
{
	sigdie("Floating Exception", 1);
}



static void	/* DAG */
sigidie(void)
{
	sigdie("IOT Trap", 1);
}


static void	/* DAG */
sigqdie(void)
{
	sigdie("Quit signal", 1);
}



static void	/* DAG */
sigindie(void)
{
	sigdie("Interrupt", 0);
}



static void	/* DAG */
sigtdie(void)
{
	sigdie("Killed", 0);
}



void
sigdie(register char *s, int32 coredump)
{
/* print error message, then clear buffers */
	fprintf(stderr, "%s\n", s);
	fflush(stderr);

	if(coredump)
	{
		/* now get a core */
		signal(SIGIOT, SIG_DFL);	/* DAG */
		abort();
	}
	else
	{
		int gid;
		gid = getpgrp();
		signal(SIGINT, SIG_IGN );
		gid = kill( -gid, SIGINT );
#ifndef FTN90_IO
		f_exit();
#endif
		exit(1);
	}
}

void
f_abort(void)
{
  s_abort();
}


void ___MAIN__(void)
{
	fprintf(stderr, "No Fortran MAIN program to execute.\n");
	exit(1);
}
