/*

  Copyright (C) 1999-2001, Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  Further, any
  license provided herein, whether implied or otherwise, is limited to 
  this program in accordance with the express provisions of the 
  GNU Lesser General Public License.  

  Patent licenses, if any, provided herein do not apply to combinations 
  of this program with other product or programs, or any other product 
  whatsoever.  This program is distributed without any warranty that the 
  program is delivered free of the rightful claim of any third person by 
  way of infringement or the like.  

  See the GNU Lesser General Public License for more details.

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

*/

/* $Header: /proj/osprey/CVS/open64/osprey1.0/libU77/exit_.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
/*
 *
 * terminate the program, close all files, and return to the OS
 *
 * calling sequence:
*	integer exitstatus
 *	call exit[(exitstatus)]
 * where:
 *	exitstatus is an integer argument which specifies the exit code
 */

#include <stdlib.h>
#ifndef FTN90_IO
#include "externals.h"
#endif

extern void exit_noargs(void)
{
#ifndef FTN90_IO
	/* we dont call f_exit() because the cleanup routine is already
	   registered via atexit and atabort */
  f_exit();
#endif
  exit(0);
}

extern void exit_byte(signed char *exitstatus)
{
#ifndef FTN90_IO
	/* we dont call f_exit() because the cleanup routine is already
	   registered via atexit and atabort */
  f_exit();
#endif
  exit(exitstatus ? (int)*exitstatus : 0);
}

extern void exit_short(short *exitstatus)
{
#ifndef FTN90_IO
	/* we dont call f_exit() because the cleanup routine is already
	   registered via atexit and atabort */
  f_exit();
#endif
  exit(exitstatus ? (int)*exitstatus : 0);
}

extern void exit_long(int *exitstatus)
{
#ifndef FTN90_IO
	/* we dont call f_exit() because the cleanup routine is already
	   registered via atexit and atabort */
  f_exit();
#endif
  exit(exitstatus ? (int)*exitstatus : 0);
}

extern void exit_long_long(long long *exitstatus)
{
#ifndef FTN90_IO
	/* we dont call f_exit() because the cleanup routine is already
	   registered via atexit and atabort */
  f_exit();
#endif
  exit(exitstatus ? (int)*exitstatus : 0);
}
