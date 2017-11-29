/*
 * Copyright 2004, 2005 PathScale, Inc.  All Rights Reserved.
 */

/* Copyright (C) 1995, 1996 Free Software Foundation, Inc.
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

#include "f2c.h"
#include <math.h>

#ifdef KEY /* Bug 1683 */
/* Experiment shows that g77 calls the dp version, not the sp version,
 * converting precision outside the code generator. It seems to make a
 * difference in the low-order bits of the result. This file was
 * incorrect anyway when we got it from the G77 distribution: it passed
 * sp args to dp functions j0 etc. */
#endif /* KEY Bug 1683 */

float
G77_besj0_0 (const real * x)
{
  return (float) j0 ((double) *x);
}

float
G77_besj1_0 (const real * x)
{
  return (float) j1 ((double) *x);
}

float
G77_besjn_0 (const integer * n, real * x)
{
  return (float) jn (*n, (double) *x);
}

float
G77_besy0_0 (const real * x)
{
  return (float) y0 ((double) *x);
}

float
G77_besy1_0 (const real * x)
{
#if defined(BUILD_OS_DARWIN)
  return (float) y1 ((double) *x);
#else /* defined(BUILD_OS_DARWIN) */
  return y1f (*x);
#endif /* defined(BUILD_OS_DARWIN) */
}

float
G77_besyn_0 (const integer * n, real * x)
{
#if defined(BUILD_OS_DARWIN)
  return (float) yn (*n, (double) *x);
#else /* defined(BUILD_OS_DARWIN) */
  return ynf (*n, *x);
#endif /* defined(BUILD_OS_DARWIN) */
}
