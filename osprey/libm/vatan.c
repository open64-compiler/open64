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
 * Module: vatan.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:20-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/SCCS/s.vatan.c $
 *
 * Revision history:
 *  01-Dec-94 - Original Version
 *
 * Description:	source code for vector arctangent function
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/libm/SCCS/s.vatan.c $ $Revision: 1.5 $";

#include "libm.h"

#if defined(mips) && !defined(__GNUC__)
extern	void	vatan(double *, double *, long, long, long);

#pragma weak vatan = __vatan
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern void __vatan( double *x, double *y, long count, long stridex,
  long stridey );
#pragma weak vatan
void vatan( double *x, double *y, long count, long stridex, long stridey ) {
  __vatan(x, y, count, stridex, stridey);
}
#elif defined(__GNUC__)
extern  void  __vatan(double *, double *, long, long, long);
void    vatan() __attribute__ ((weak, alias ("__vatan")));
#endif

/* coefficients for polynomial approximation of atan on +/- .125	*/

static	const	du P[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0xbfd55555, 0x555554cc)},
{D(0x3fc99999, 0x99945d66)},
{D(0xbfc24924, 0x89cbb155)},
{D(0x3fbc71ba, 0x706a40b4)},
{D(0xbfb74107, 0x237438a4)},
{D(0x3fb2cc16, 0x060e74d4)},
};

/*	xk[j] = j*.25, j = 0, 1, ..., 32	*/

static	const	du xk[] =
{
{D(0x00000000, 0x00000000)},
{D(0x3fd00000, 0x00000000)},
{D(0x3fe00000, 0x00000000)},
{D(0x3fe80000, 0x00000000)},
{D(0x3ff00000, 0x00000000)},
{D(0x3ff40000, 0x00000000)},
{D(0x3ff80000, 0x00000000)},
{D(0x3ffc0000, 0x00000000)},
{D(0x40000000, 0x00000000)},
{D(0x40020000, 0x00000000)},
{D(0x40040000, 0x00000000)},
{D(0x40060000, 0x00000000)},
{D(0x40080000, 0x00000000)},
{D(0x400a0000, 0x00000000)},
{D(0x400c0000, 0x00000000)},
{D(0x400e0000, 0x00000000)},
{D(0x40100000, 0x00000000)},
{D(0x40110000, 0x00000000)},
{D(0x40120000, 0x00000000)},
{D(0x40130000, 0x00000000)},
{D(0x40140000, 0x00000000)},
{D(0x40150000, 0x00000000)},
{D(0x40160000, 0x00000000)},
{D(0x40170000, 0x00000000)},
{D(0x40180000, 0x00000000)},
{D(0x40190000, 0x00000000)},
{D(0x401a0000, 0x00000000)},
{D(0x401b0000, 0x00000000)},
{D(0x401c0000, 0x00000000)},
{D(0x401d0000, 0x00000000)},
{D(0x401e0000, 0x00000000)},
{D(0x401f0000, 0x00000000)},
{D(0x40200000, 0x00000000)},
};

/*	yk[j] = atan(j*.25), j = 0, 1, ..., 32	*/

static	const	du yk[] =
{
{D(0x00000000, 0x00000000)},
{D(0x3fcf5b75, 0xf92c80dd)},
{D(0x3fddac67, 0x0561bb4f)},
{D(0x3fe4978f, 0xa3269ee1)},
{D(0x3fe921fb, 0x54442d18)},
{D(0x3fecac7c, 0x57846f9e)},
{D(0x3fef730b, 0xd281f69b)},
{D(0x3ff0d38f, 0x2c5ba09f)},
{D(0x3ff1b6e1, 0x92ebbe44)},
{D(0x3ff270ef, 0x55a53a25)},
{D(0x3ff30b6d, 0x796a4da8)},
{D(0x3ff38d6a, 0x6ce13353)},
{D(0x3ff3fc17, 0x6b7a8560)},
{D(0x3ff45b54, 0x837351a0)},
{D(0x3ff4ae10, 0xfc6589a5)},
{D(0x3ff4f68d, 0xea672617)},
{D(0x3ff5368c, 0x951e9cfd)},
{D(0x3ff56f6f, 0x33a3e6a7)},
{D(0x3ff5a250, 0x52114e60)},
{D(0x3ff5d013, 0xc41adabd)},
{D(0x3ff5f973, 0x15254857)},
{D(0x3ff61f06, 0xc6a92b89)},
{D(0x3ff6414d, 0x44094c7c)},
{D(0x3ff660b0, 0x2c736a06)},
{D(0x3ff67d88, 0x63bc99bd)},
{D(0x3ff69821, 0x3a9d5053)},
{D(0x3ff6b0ba, 0xe830c070)},
{D(0x3ff6c78c, 0x7edeb195)},
{D(0x3ff6dcc5, 0x7bb565fd)},
{D(0x3ff6f08f, 0x07435fec)},
{D(0x3ff7030c, 0xf9403197)},
{D(0x3ff7145e, 0xac2088a4)},
{D(0x3ff7249f, 0xaa996a21)},
};

static	const	du	Twop85 =
{D(0x45400000, 0x00000000)};

static	const	du	Twopm28 =
{D(0x3e300000, 0x00000000)};

static const	du	Qnan =
{D(QNANHI, QNANLO)};


/* ====================================================================
 *
 * FunctionName		vatan
 *
 * Description		computes vector arctangent of arg
 *
 * ====================================================================
 */

void
__vatan( double	*x, double *y, long count, long stridex, long stridey )
{
long	i;
int	j;
double	arg;
double	dx, w, z, zsq;
double	u, poly, result;

	/* i = 0, 1, ..., count-1; y[i*stridey] = atan(x[i*stridex]) */

	for ( i=0; i<count; i++ )
	{
#ifdef _PREFETCH
#pragma prefetch_ref=*(x+8)
#pragma prefetch_ref=*(y+8)
#endif

		arg = *x;

		/* take care of negative args last	*/

		dx = fabs(arg);

		if ( arg != arg )
			dx = 0.0;

		/* if arg > 2**85, result = pi/2	*/

		if ( dx > Twop85.d )
			dx = Twop85.d;

		w = dx;

		/* args > 8.0 use xk = 8.0	*/

		if ( dx > 8.0 )
			w = 8.0;

		j = ROUND(4.0*w);

		z = (dx - xk[j].d)/(1.0 + dx*xk[j].d);

		/* atan(arg) = yk[j] + atan(z)	*/

		u = z;

		/* guard against underflow for small args */

		if ( fabs(z) < Twopm28.d )
			u = 0.0;

		zsq = u*u;

		poly = (((((P[6].d*zsq + P[5].d)*zsq + P[4].d)*zsq +
			P[3].d)*zsq + P[2].d)*zsq + P[1].d)*(zsq*z) + z;

		result = yk[j].d + poly;

		if ( arg < 0.0 )
			result = -result;

		if ( arg != arg )
			result = Qnan.d;

		*y = result;

		x += stridex;
		y += stridey;
	}
}

