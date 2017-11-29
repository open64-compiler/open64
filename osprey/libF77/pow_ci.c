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

extern complex __powci(float areal, float aimag, int32 n);
extern complex __powcl(float areal, float aimag, int64 n);

void pow_ci__(complex *p, complex *a, int32 *b)   /* p = a**b  */
{
	*p = __powci(a->real, a->imag, *b);
}

#if defined(BUILD_OS_DARWIN)
/* Mach-O doesn't support aliases */
void pow_ci_(complex *p, complex *a, int32 *b) { pow_ci__(p, a, b); }
void pow_ci(complex *p, complex *a, int32 *b) { pow_ci__(p, a, b); }
void pow_cif_(complex *p, complex *a, int32 *b) { pow_ci__(p, a, b); }
#else /* defined(BUILD_OS_DARWIN) */
defalias(pow_ci__, pow_ci_);
defalias(pow_ci__, pow_ci);
defalias(pow_ci__, pow_cif_);
#endif /* defined(BUILD_OS_DARWIN) */

void
pow_cl__(complex *p, complex *a, int64 *b)
{
        *p = __powcl(a->real, a->imag, *b);
}

#if defined(BUILD_OS_DARWIN)
/* Mach-O doesn't support aliases */
void pow_cl_(complex *p, complex *a, int64 *b) { pow_cl__(p, a, b); }
void pow_cl(complex *p, complex *a, int64 *b) { pow_cl__(p, a, b); }
void pow_clf_(complex *p, complex *a, int64 *b) { pow_cl__(p, a, b); }
#else /* defined(BUILD_OS_DARWIN) */
defalias(pow_cl__, pow_cl_);
defalias(pow_cl__, pow_cl);
defalias(pow_cl__, pow_clf_);
#endif /* defined(BUILD_OS_DARWIN) */
