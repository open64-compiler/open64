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
#if HAVE_STDLIB_H || defined(KEY) /* Bug 1683, 5019 */
#  include <stdlib.h>
#endif
#ifdef KEY /* Bug 5019 */
#include "cray/mtlock.h"
#include "pathf90_libU_intrin.h"
#endif /* KEY Bug 5019 */

#include "f2c.h"

/* We could presumably do much better than the traditional libc
   version, though at least the glibc one is reasonable, it seems.
   For the sake of the innocent, I'm not sure we should really do
   this... */

/* Note this is per SunOS -- other s may have no arg. */

integer
G77_irand_0 (integer * flag)
{
#ifdef KEY /* Bug 1683, 5019 */
  /* Experiment shows that g77 generates a zero (outside the library,
   * apparently) when the optional "flag" argument is missing */
  integer zero = 0;
  flag = (0 == flag) ? (&zero) : flag;
  MEM_LOCK(&pathf90_rand_mutex);
  switch (*flag)
    {
    case 0:
      break;
    case 1:
      srand (0);		/* Arbitrary choice of initialiser. */
      break;
    default:
      srand (*flag);
    }
  integer result = rand ();
  MEM_UNLOCK(&pathf90_rand_mutex);
  return result;
#else
  switch (*flag)
    {
    case 0:
      break;
    case 1:
      srand (0);		/* Arbitrary choice of initialiser. */
      break;
    default:
      srand (*flag);
    }
  return rand ();
#endif /* KEY Bug 1683, 5019 */
}
