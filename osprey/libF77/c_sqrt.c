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
#include "moremath.h"
#include "cmplx.h"
#include "defalias.h"

complex __csqrt(float zreal, float zimag)
{
  register float mag;
  complex result;

  if ((mag = hypotf(zreal, zimag)) == 0.0F) {
    result.real = result.imag = 0.0F;
  } else if (zreal > 0.0F) {
    result.real = sqrtf(0.5F * (mag + zreal));
    result.imag = zimag/result.real/2.0F;
  } else {
    result.imag = sqrtf(0.5F * (mag - zreal));
    if(zimag < 0.0F)
      result.imag = - result.imag;
    result.real = zimag/result.imag/2.0F;
  }
  return result;
}

complex c_sqrt_(complex *z)
{
  return __csqrt(z->real, z->imag);
}

#if defined(BUILD_OS_DARWIN)
/* Mach-O doesn't support aliases */
complex c_sqrt(complex *z) { return c_sqrt_(z); }
#else /* defined(BUILD_OS_DARWIN) */
defalias(c_sqrt_, c_sqrt);
#endif /* defined(BUILD_OS_DARWIN) */
