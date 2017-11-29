/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


/* ====================================================================
 * ====================================================================
 *
 * Module: vsqrtf.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:21-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/SCCS/s.vsqrtf.c $
 *
 * Revision history:
 *  10-Feb-95 - Original Version
 *
 * Description:	source code for vector sqrtf function
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/libm/SCCS/s.vsqrtf.c $ $Revision: 1.5 $";

#include "libm.h"

#if defined(mips) && !defined(__GNUC__)
extern	void	vfsqrt(float *, float *, long, long, long);
extern	void	vsqrtf(float *, float *, long, long, long);

#pragma weak vfsqrt = __vsqrtf
#pragma weak vsqrtf = __vsqrtf
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern void __vsqrtf( float *x, float *y, long count, long stridex,
  long stridey );
#pragma weak vsqrtf
void vsqrtf( float *x, float *y, long count, long stridex, long stridey ) {
  __vsqrtf(x, y, count, stridex, stridey);
}
#elif defined(__GNUC__)
extern  void  __vsqrtf(float *, float *, long, long, long);
void    vsqrtf() __attribute__ ((weak, alias ("__vsqrtf")));
#endif

static const	fu	Inf = {0x7f800000};


/* ====================================================================
 *
 * FunctionName		vsqrtf
 *
 * Description		computes vector sqrtf of arg
 *
 * ====================================================================
 */

void
__vsqrtf( float *x, float *y, long count, long stridex, long stridey )
{
long	i;
float	u;
float	z;
float	result;

	/* i = 0, 1, ..., count-1; y[i*stridey] = sqrtf(x[i*stridex])	*/

	for ( i=0; i<count; i++ )
	{
#ifdef _PREFETCH
#pragma prefetch_ref=*(x+8)
#pragma prefetch_ref=*(y+8)
#endif

		u = z = *x;

#ifdef _USES_RECIP_SQRT

/* If _USES_RECIP_SQRT is defined, the compiler uses a reciprocal sqrt to
 * compute sqrt, so we have to special case zero and infinity.
 */

		if ( z == 0.0f )
			u = 1.0f;

		if ( z == Inf.f )
			u = 1.0f;
#endif
		result = sqrtf(u);

#ifdef _USES_RECIP_SQRT

		if ( z == 0.0f )
			result = z;

		if ( z == Inf.f )
			result = z;
#endif
		*y = result;

		x += stridex;
		y += stridey;
	}
}

