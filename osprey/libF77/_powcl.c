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

#include <math.h>
#include "cmplrs/host.h"
#include "cmplx.h"
#include "c_div.h"

complex __powcl(float areal, float aimag, int64 n)   /* __powcl = a**n  */
{
  float t;
  complex x, p;

  p.real = 1;
  p.imag = 0;

  if(n == 0)
    return p;

  if(n < 0) {
    n = -n;
    x = __cdiv(p.real, p.imag, areal, aimag);
  } else {
    x.real = areal;
    x.imag = aimag;
  }

  for( ; ; ) {
    if(n & 01) {
      t = p.real * x.real - p.imag * x.imag;
      p.imag = p.real * x.imag + p.imag * x.real;
      p.real = t;
    }
    if(n >>= 1) {
      t = x.real * x.real - x.imag * x.imag;
      x.imag = 2 * x.real * x.imag;
      x.real = t;
    } else {
      break;
    }
  }

  return p;
}
