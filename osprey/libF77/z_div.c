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


/* $Header: /proj/osprey/CVS/open64/osprey1.0/libF77/z_div.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
#include <math.h>
#include <stdlib.h>
#include "cmplrs/host.h"
#include "cmplx.h"
#include "defalias.h"
#include "moremath.h"

#ifdef KEY
dcomplex __zdiv(double adreal, double adimag, double bdreal, double bdimag)
#else
dcomplex __zdiv(double adreal, double adimag, double_t bdreal, double_t bdimag)
#endif // KEY
{
  double ratio, den;
  double abr, abi;
  dcomplex c;
  
  if( (abr = bdreal) < 0.)
    abr = - abr;
  if( (abi = bdimag) < 0.)
    abi = - abi;
  if( abr <= abi ) {
    ratio = bdreal / bdimag ;
    den = bdimag * (1 + ratio*ratio);
    c.dreal = (adreal*ratio + adimag) / den;
    c.dimag = (adimag*ratio - adreal) / den;
  } else {
    ratio = bdimag / bdreal ;
    den = bdreal * (1 + ratio*ratio);
    c.dreal = (adreal + adimag*ratio) / den;
    c.dimag = (adimag - adreal*ratio) / den;
  }
  return c;
}

dcomplex z_div_(dcomplex *a, dcomplex *b)
{
  return __zdiv(a->dreal, a->dimag, b->dreal, b->dimag);
}

#if defined(BUILD_OS_DARWIN)
/* Mach-O doesn't support aliases */
dcomplex z_div(dcomplex *a, dcomplex *b) { return z_div_(a, b); }
#else /* defined(BUILD_OS_DARWIN) */
defalias(z_div_, z_div);
#endif /* defined(BUILD_OS_DARWIN) */
