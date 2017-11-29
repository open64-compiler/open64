/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

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

#ifndef __EXTERNALS_H__
#define __EXTERNALS_H__

/* these come from charutil.c */
extern void g_char (char *a, int alen, char *b);
extern void b_char (char *a, char *b, int blen);

#ifndef FTN90_IO
/* these are defined in libI77 */
#include <cmplrs/fio.h>
extern int f77nowreading (unit *x);
extern int f77nowwriting (unit *x);

extern void f_exit (void);	/* from libI77/close.c */

extern unit *find_luno (ftnint luno);	/* libI77/open.c */

extern int _fio_du_flush(int);    /* libI77/fio_direct_io.c */

extern void flush_connected_units (void);	/* unknown */
extern void _cleanup (void);			/* unknown */
#else
	/* cause syntax error on any attempt to use */
#define f77nowreading 2U$
#define f77nowwriting 2U$
#define f_exit 2U$
#define find_luno 2U$
#define _fio_du_flush 2U$
#define flush_connected_units 2U$
#define _cleanup 2U$
#endif /* FTN90_IO */

#if defined(__mips)
# ifdef FTN90_IO
   extern int __Argc;
   extern char **__Argv;
# else
   extern int f77argc;		/* from libF77/main.c */
   extern char **f77argv;
# endif
#elif defined(__linux)
  extern int f__xargc;
  extern char **f__xargv;
#endif

#ifdef __mips
#ifdef FTN90_IO
#define ARGC __Argc
#else
#define ARGC f77argc
#endif
#elif defined(__linux)
#define ARGC f__xargc
#endif

#if defined(__mips)
#ifdef FTN90_IO
#define ARGC __Argc
#define ARGV __Argv
#else
#define ARGC f77argc
#define ARGV f77argv
#endif
#elif defined(__linux)
#define ARGC f__xargc
#define ARGV f__xargv 
#endif

#if defined(BUILD_OS_DARWIN)
/* Unlike the Linux environment, Darwin stores argc and argv into globals prior
 * to calling "main". By using those globals instead of the ones that the
 * Fortran runtime sets up, we can ensure that the Fortran intrinsic functions
 * which rely on them will work even if the main program is not written in
 * Fortan, and even if Fortran runtime library initialization code doesn't
 * execute.
 */
extern int NXArgc;
extern char **NXArgv;
#define ARGC NXArgc
#define ARGV NXArgv
#endif /* defined(BUILD_OS_DARWIN) */


/* these are set and used within libU77 */
extern char *bufarg;
extern long bufarglen;

#endif /* !__EXTERNALS_H__ */
