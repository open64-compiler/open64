/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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

#pragma ident "@(#) libu/clib/signal.c	92.1	07/01/99 13:42:20"

/*    signal.c     */

#ifdef KEY /* Bug 1683 */

#include <signal.h>
#include <string.h>

/*
 * g77 documents only a two-argument "signal" intrinsic function.
 * Although g77 documents an optional third argument to a "signal"
 * intrinsic subroutine, in which the third argument returns an error status,
 * experiment shows that the compiler does not accept that third argument.
 *
 * Intel and PGI, meanwhile, define a "signal" intrinsic subroutine
 * in which the third argument chooses whether to use the second argument
 * as a signal handler, or whether to ignore the second argument and set
 * the signal to "default" or "ignore."
 *
 * g77 accepts an integer as the second argument; Intel/PGI do not.
 *
 * For the time being, we use the absence of the third argument to
 * select the g77 behavior; if we ever need to support a third argument which
 * returns status, we'll need a compiler flag to switch between g77 and
 * Intel/PGI behavior.
 *
 * We want people to be able to store the result in either integer*4 or
 * integer*8 variables. That means the function must return integer*8,
 * and the front end must believe that it returns integer*8 so that it will
 * convert to integer*4 if need be. If the return type were void *, then in
 * -m32 mode an integer*8 destination would contain high-order garbage.
 */

/* Function or subroutine form of "signal": 3-arg version with procedure
 * as second arg.  */
long long
pathf90_signal(int *sig, void (*func)(int), int *flag) {
	/* Missing optional arg: use "func" */
	int flag_val = (0 == flag) ? -1 : *flag;
	if (0 == flag_val) {
	  func = SIG_DFL;
	}
	else if (1 == flag_val) {
	  func = SIG_IGN;
	}
	struct sigaction act, oldact;
	act.sa_handler = func;
	memset(&(act.sa_mask), 0, sizeof act.sa_mask);
	act.sa_flags = 0;
	/* Circumvent stupid compiler warning in -m32 mode */
	union { long long l; void *p; } temp_result;
	temp_result.l = 0;
	if (0 != sigaction(*sig, &act, &oldact)) {
	  temp_result.p = SIG_ERR;
	  }
	else {
	  temp_result.p = oldact.sa_handler;
	  }
	return temp_result.l;
}

/* Function or subroutine form of "signal": 2-arg version with integer
 * as second arg.  */
long long
pathf90_signal8(int *sig, long long *func) {
	int flag = -1;
	void (*ourfunc)();
        long long func_val = *func;
	if (0 == func_val) {
	  flag = 0;
	  }
	else if (1 == func_val) {
	  flag = 1;
	  }
#if (_MIPS_SZPTR == _MIPS_SZLONG)
	ourfunc = (void (*)()) (long) func_val;
#else /* (_MIPS_SZPTR == _MIPS_SZLONG) */
	ourfunc = (void (*)()) func_val;
#endif /* (_MIPS_SZPTR == _MIPS_SZLONG) */
	return pathf90_signal(sig, ourfunc, &flag);
}

/* Function or subroutine form of "signal": 2-arg version with integer
 * as second arg.  */
long long
pathf90_signal4(int *sig, int *func) {
	long long ourfunc = *func;
	return pathf90_signal8(sig, &ourfunc);
}

#else

/* SIGNAL has been called from both FORTRAN and PASCAL but the two
 * compilers generate code that calls SIGNAL that is not compatible.
 * Because of this SIGNAL may vary from site to site and we discourage
 * its use. Codes should be changed to call either FSIGNAL or PSIGNAL.
 * This will allow codes to be ported to other CRAY sites. The use
 * of SIGNAL may give different results at different sites. */

SIGNAL (sig,func)
int  *sig, *func;
{
	return ((long)signal(*sig,*func));
}

#endif /* KEY Bug 1683 */

/* Remains for backward compatibility with previously-compiled binaries */
/* FSIGNAL is the FORTRAN callable link to 'signal' */
FSIGNAL (sig,func)
int  *sig, *func;
{
	return ((long)signal(*sig,(void (*)(int))func));
}

#ifdef KEY /* Bug 1683 */
#else
/* PSIGNAL is the PASCAL callable link to 'signal' */
PSIGNAL (sig,func)
int  *sig, *func;
{
	return ((long)signal(*sig,*func));
}

#endif /* KEY Bug 1683 */
