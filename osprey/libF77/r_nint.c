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


/* $Header: /proj/osprey/CVS/open64/osprey1.0/libF77/r_nint.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
#include "cmplrs/host.h"
#include <math.h>
#include "moremath.h"

#define TWO_EXP_23 8388608.0F

float __rnint(float x)
{
  /* Need to worry about case where LSB of floating-point type
     represents 1.0, e.g., ANINT(8388609.0) needs to return 8388609.0
     (will return 8388610.0 without this additional check). */
  if (fabs(x) >= TWO_EXP_23)
    return (x);
  else
    return (x >= 0.0F ? floorf(x + .5F) : -floorf(.5F - x));
}

float r_nint(float *x)
{
  return (__rnint(*x));
}
