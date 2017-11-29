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
 * Module: vatanf.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:20-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/SCCS/s.vatanf.c $
 *
 * Revision history:
 *  06-Dec-94 - Original Version
 *
 * Description:	source code for vector arctangent function
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/libm/SCCS/s.vatanf.c $ $Revision: 1.5 $";

#include "libm.h"

#if defined(mips) && !defined(__GNUC__)
extern	void	vfatan(float *, float *, long, long, long);
extern	void	vatanf(float *, float *, long, long, long);

#pragma weak vfatan = __vatanf
#pragma weak vatanf = __vatanf
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern void __vatanf( float *x, float *y, long count, long stridex,
  long stridey );
#pragma weak vatanf
void vatanf( float *x, float *y, long count, long stridex, long stridey ) {
  __vatanf(x, y, count, stridex, stridey);
}
#elif defined(__GNUC__)
extern  void  __vatanf(float *, float *, long, long, long);
void    vatanf() __attribute__ ((weak, alias ("__vatanf")));
#endif

/* coefficients for polynomial approximation of atan on +/- .25	*/

static	const	du P[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0xbfd55554, 0xfa93267a)},
{D(0x3fc9992a, 0x5f168922)},
{D(0xbfc233d7, 0x10c23920)},
{D(0x3fb92edf, 0xe7e2c83b)},
};

/*	xk[j] = j*.5, j = 0, 1, ..., 8	*/

static	const	du xk[] =
{
{D(0x00000000, 0x00000000)},
{D(0x3fe00000, 0x00000000)},
{D(0x3ff00000, 0x00000000)},
{D(0x3ff80000, 0x00000000)},
{D(0x40000000, 0x00000000)},
{D(0x40040000, 0x00000000)},
{D(0x40080000, 0x00000000)},
{D(0x400c0000, 0x00000000)},
{D(0x40100000, 0x00000000)},
};

/*	yk[j] = atan(j*.5), j = 0, 1, ..., 8	*/

static	const	du yk[] =
{
{D(0x00000000, 0x00000000)},
{D(0x3fddac67, 0x0561bb4f)},
{D(0x3fe921fb, 0x54442d18)},
{D(0x3fef730b, 0xd281f69b)},
{D(0x3ff1b6e1, 0x92ebbe44)},
{D(0x3ff30b6d, 0x796a4da8)},
{D(0x3ff3fc17, 0x6b7a8560)},
{D(0x3ff4ae10, 0xfc6589a5)},
{D(0x3ff5368c, 0x951e9cfd)},
};

static	const	du	Twopm28 =
{D(0x3e300000, 0x00000000)};

static	const	du	Twop26 = 
{D(0x41900000, 0x00000000)};

static const	fu	Qnan = {QNANF};


/* ====================================================================
 *
 * FunctionName		vatanf
 *
 * Description		computes vector arctangent of arg
 *
 * ====================================================================
 */

void
__vatanf( float	*x, float *y, long count, long stridex, long stridey )
{
long	i;
int	j;
float	arg;
float	result;
double	dx, w;
double	z, zsq;
double	u, poly;

	/* i = 0, 1, ..., count-1; y[i*stridey] = atanf(x[i*stridex]) */

	for ( i=0; i<count; i++ )
	{
#ifdef _PREFETCH
#pragma prefetch_ref=*(x+8)
#pragma prefetch_ref=*(y+8)
#endif

		arg = *x;

		/* take care of negative args last	*/

		dx = fabsf(arg);

		if ( arg != arg )
			dx = 0.0;

		/* if arg > 2**26, result = pi/2	*/

		if ( dx > Twop26.d )
			dx = Twop26.d;

		w = dx;

		/* args > 4.0 use xk = 4.0	*/

		if ( dx > 4.0 )
			w = 4.0;

		j = ROUND(w+w);

		z = (dx - xk[j].d)/(1.0 + dx*xk[j].d);

		/* fatan(arg) = yk[j] + fatan(z)	*/

		u = z;

		/* guard against underflow for small args */

		if ( fabs(z) < Twopm28.d )
			u = 0.0;

		zsq = u*u;

		poly = (((P[4].d*zsq + P[3].d)*zsq + P[2].d)*zsq +
			  P[1].d)*(zsq*z) + z;

		result = yk[j].d + poly;

		if ( arg < 0.0f )
			result = -result;

		if ( arg != arg )
			result = Qnan.f;

		*y = result;

		x += stridex;
		y += stridey;
	}
}

