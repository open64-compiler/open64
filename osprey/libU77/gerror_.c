/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

/* Copyright (C) 1995 Free Software Foundation, Inc.
This file is part of GNU Fortran libU77 library.

This library is free software; you can redistribute it and/or modify it
under the terms of the GNU Library General Public License as published
by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

GNU Fortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with GNU Fortran; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include <errno.h>
#include <stddef.h>
#if HAVE_STRING_H
#  include <string.h>
#else
#  include <strings.h>
#endif
#include "f2c.h"

#ifndef HAVE_STRERROR
extern char *sys_errlist[];
#    define strerror(i) (sys_errlist[i])
#endif

#ifdef KEY /* Bug 1683, 5019 */
#include <alloca.h>
#endif /* KEY Bug 1683, 5019 */

extern void s_copy (register char *a, register char *b, ftnlen la, ftnlen lb);
/* Subroutine */ int
G77_gerror_0 (char *str, ftnlen Lstr)
{
#ifdef KEY /* Bug 1683, 5019 */
  char *buf = alloca(Lstr + 1);
#if defined(BUILD_OS_DARWIN)
  /* Standard version uses "buf" */
  strerror_r(errno, buf, Lstr + 1);
  char *s = buf;
#else /* KEY Mac port */
  /* GNU version returns "char *", may ignore "buf" */
  char *s = strerror_r(errno, buf, Lstr + 1);
#endif /* KEY Mac port */
#else
  char *s;

  s = strerror (errno);
#endif /* KEY Bug 1683, 5019 */
  s_copy (str, s, Lstr, strlen (s));
  return 0;
}
