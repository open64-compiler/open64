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
#include "defalias.h"
#include "moremath.h"

dcomplex __zsqrt(double zdreal, double zdimag)
{
  double mag;
  dcomplex result;

  if ((mag = hypot(zdreal, zdimag)) == 0.) {
    result.dreal = result.dimag = 0.;
  } else if (zdreal > 0.) {
    result.dreal = sqrt(0.5 * (mag + zdreal));
    result.dimag = zdimag/result.dreal/2;
  } else {
    result.dimag = sqrt(0.5 * (mag - zdreal));
    if (zdimag < 0.)
      result.dimag = - result.dimag;
    result.dreal = zdimag/result.dimag/2;
  }
  return result;
}

dcomplex z_sqrt_(dcomplex *z)
{
  return __zsqrt(z->dreal, z->dimag);
}

#if defined(BUILD_OS_DARWIN)
/* Mach-O doesn't support aliases */
dcomplex z_sqrt(dcomplex *z) { return z_sqrt_(z); }
#else /* defined(BUILD_OS_DARWIN) */
defalias(z_sqrt_, z_sqrt);
#endif /* defined(BUILD_OS_DARWIN) */
