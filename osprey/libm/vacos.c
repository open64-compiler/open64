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
 * Module: vacos.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:20-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/SCCS/s.vacos.c $
 *
 * Revision history:
 *  01-Dec-94 - Original Version
 *
 * Description:	source code for vector arccosine function
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/libm/SCCS/s.vacos.c $ $Revision: 1.5 $";

#include "libm.h"

#if defined(mips) && !defined(__GNUC__)
extern	void	vacos(double *, double *, long, long, long);

#pragma weak vacos = __vacos
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern void __vacos( double *x, double *y, long count, long stridex,
  long stridey );
#pragma weak vacos
void vacos( double *x, double *y, long count, long stridex, long stridey ) {
  __vacos(x, y, count, stridex, stridey);
}
#elif defined(__GNUC__)
extern  void  __vacos(double *, double *, long, long, long);
void    vacos() __attribute__ ((weak, alias ("__vacos")));
#endif

/* coefficients for polynomial approximation of asin on +/- sqrt(1 - (63/64)**2)    */

static const du	P[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0x3fc55555, 0x55555578)},
{D(0x3fb33333, 0x333256c4)},
{D(0x3fa6db6d, 0xb8b92be5)},
{D(0x3f9f1c6f, 0xdcc39d32)},
{D(0x3f96e940, 0x7e05b546)},
{D(0x3f91b0c9, 0xc6fc9434)},
{D(0x3f8fb249, 0x579227f4)},
};

/* table of angles pi/2 - asin(j/32), j=-32, -31, ..., 31, 32	*/

/* Angles are chosen so that pi/2 - xk[j] is exact and the sum
   of the ulps error in sin(pi/2 - xk[j]) + cos(pi/2 - xk[j]) is
   minimized over the nearest 2000 angles; consequently
   most of the tabulated values are nearly exact, thus
   helping to minimize roundoff error in the computations.
*/

static const	du	xk[] =
{
{D(0x400921fb, 0x54442d18)},
{D(0x400720a3, 0x92c1dc0f)},
{D(0x40064a14, 0x4217a758)},
{D(0x4005a417, 0xdae318e6)},
{D(0x40051700, 0xe0c14cde)},
{D(0x400499a8, 0x730b6631)},
{D(0x4004275e, 0xffc0ee8f)},
{D(0x4003bd5c, 0xcb49304c)},
{D(0x400359d2, 0x6f93b542)},
{D(0x4002fb7e, 0x18d1d857)},
{D(0x4002a175, 0x576665a7)},
{D(0x40024b06, 0xf83146a6)},
{D(0x4001f7a9, 0x0695c713)},
{D(0x4001a6ed, 0x7686128b)},
{D(0x4001587a, 0xae1160c3)},
{D(0x40010c06, 0x6d3e6b91)},
{D(0x4000c152, 0x382d7710)},
{D(0x40007828, 0xbffbd2ca)},
{D(0x4000305b, 0xf8ae360d)},
{D(0x3fffd387, 0x4f46f6c2)},
{D(0x3fff4878, 0x8fb683ed)},
{D(0x3ffebf4c, 0x590e9662)},
{D(0x3ffe37c9, 0xa3286656)},
{D(0x3ffdb1bc, 0x9d35a0fd)},
{D(0x3ffd2cf5, 0xc7c711a7)},
{D(0x3ffca949, 0x36b98df7)},
{D(0x3ffc268d, 0xf1e5fc7f)},
{D(0x3ffba49d, 0x6da44bcb)},
{D(0x3ffb2353, 0x15c682d6)},
{D(0x3ffaa28b, 0xe6d56e62)},
{D(0x3ffa2226, 0x122d7c85)},
{D(0x3ff9a200, 0xaa3336e3)},
{D(0x3ff921fb, 0x54442d18)},
{D(0x3ff8a1f5, 0xfe55235d)},
{D(0x3ff821d0, 0x965ad5d4)},
{D(0x3ff7a16a, 0xc1b2ed89)},
{D(0x3ff720a3, 0x92c1d556)},
{D(0x3ff69f59, 0x3ae40b5c)},
{D(0x3ff61d68, 0xb6a25b97)},
{D(0x3ff59aad, 0x71cecc85)},
{D(0x3ff51700, 0xe0c14ece)},
{D(0x3ff4923a, 0x0b52b689)},
{D(0x3ff40c2d, 0x055ff519)},
{D(0x3ff384aa, 0x4f79ca70)},
{D(0x3ff2fb7e, 0x18d1d15d)},
{D(0x3ff2706f, 0x59416011)},
{D(0x3ff1e33e, 0xb72bf15a)},
{D(0x3ff153a5, 0x2890b215)},
{D(0x3ff0c152, 0x382d7710)},
{D(0x3ff02be9, 0xce0b8407)},
{D(0x3fef2602, 0x98cb2bce)},
{D(0x3fedec37, 0x76f87181)},
{D(0x3feca949, 0x36b98b15)},
{D(0x3feb5bd1, 0x704b98fd)},
{D(0x3fea0217, 0xf377125b)},
{D(0x3fe899f4, 0xedc96001)},
{D(0x3fe720a3, 0x92c1da51)},
{D(0x3fe5927a, 0x23ebea23)},
{D(0x3fe3ea71, 0x520cf7bf)},
{D(0x3fe2214b, 0x84e31e70)},
{D(0x3fe02be9, 0xce0b8b3b)},
{D(0x3fdbef1b, 0xcb088746)},
{D(0x3fd6bf38, 0x91641ebe)},
{D(0x3fd00abe, 0x0c12a216)},
{D(0x00000000, 0x00000000)},
};

/* sines of angles asin(j/32),  j=-32, -31, ..., 31, 32	*/

static const	du	sk[] =
{
{D(0xbff00000, 0x00000000)},
{D(0xbfef0000, 0x000002b5)},
{D(0xbfedffff, 0xfffffdc8)},
{D(0xbfecffff, 0xfffffacf)},
{D(0xbfec0000, 0x00000356)},
{D(0xbfeb0000, 0x00000261)},
{D(0xbfe9ffff, 0xfffffc51)},
{D(0xbfe8ffff, 0xfffff8cd)},
{D(0xbfe7ffff, 0xfffffc05)},
{D(0xbfe70000, 0x00000afd)},
{D(0xbfe5ffff, 0xfffff4e7)},
{D(0xbfe4ffff, 0xfffffd41)},
{D(0xbfe3ffff, 0xfffff51c)},
{D(0xbfe30000, 0x000008c4)},
{D(0xbfe1ffff, 0xfffff89b)},
{D(0xbfe10000, 0x0000080a)},
{D(0xbfe00000, 0x00000cb3)},
{D(0xbfde0000, 0x000002a2)},
{D(0xbfdbffff, 0xfffffdab)},
{D(0xbfd9ffff, 0xfffffc6b)},
{D(0xbfd7ffff, 0xfffff90c)},
{D(0xbfd60000, 0x00000bf2)},
{D(0xbfd40000, 0x0000022f)},
{D(0xbfd1ffff, 0xfffff3ea)},
{D(0xbfd00000, 0x00000a19)},
{D(0xbfcc0000, 0x00001ded)},
{D(0xbfc80000, 0x00000230)},
{D(0xbfc3ffff, 0xffffe1d3)},
{D(0xbfc00000, 0x00000fb0)},
{D(0xbfb80000, 0x00002f38)},
{D(0xbfafffff, 0xffff81a4)},
{D(0xbfa00000, 0x00007f7f)},
{D(0x00000000, 0x00000000)},
{D(0x3fa00000, 0x00007d91)},
{D(0x3fb00000, 0x00003e0c)},
{D(0x3fb80000, 0x000013b0)},
{D(0x3fc00000, 0x00001fb4)},
{D(0x3fc3ffff, 0xfffff9d3)},
{D(0x3fc80000, 0x000012b8)},
{D(0x3fcc0000, 0x00001ba0)},
{D(0x3fcfffff, 0xffffe3a6)},
{D(0x3fd1ffff, 0xfffffe26)},
{D(0x3fd3ffff, 0xfffffd75)},
{D(0x3fd5ffff, 0xfffff30a)},
{D(0x3fd80000, 0x00000b38)},
{D(0x3fda0000, 0x000008b8)},
{D(0x3fdbffff, 0xfffff1ee)},
{D(0x3fde0000, 0x00000b92)},
{D(0x3fdfffff, 0xfffff34d)},
{D(0x3fe10000, 0x00000665)},
{D(0x3fe1ffff, 0xfffffd2d)},
{D(0x3fe30000, 0x000002e5)},
{D(0x3fe3ffff, 0xffffff42)},
{D(0x3fe4ffff, 0xfffffddb)},
{D(0x3fe5ffff, 0xfffffd31)},
{D(0x3fe70000, 0x000001f6)},
{D(0x3fe7ffff, 0xffffff59)},
{D(0x3fe8ffff, 0xfffffe74)},
{D(0x3fe9ffff, 0xfffffdb7)},
{D(0x3feb0000, 0x000000dd)},
{D(0x3febffff, 0xfffffe57)},
{D(0x3fed0000, 0x0000005e)},
{D(0x3fee0000, 0x00000070)},
{D(0x3feeffff, 0xffffff82)},
{D(0x3ff00000, 0x00000000)},
};

/* cosines of angles asin(j/32),  j=-32, -31, ..., 31, 32	*/

static const	du	ck[] =
{
{D(0x00000000, 0x00000000)},
{D(0x3fcfbfbf, 0x7ebc4b14)},
{D(0x3fd64564, 0x0568cdb8)},
{D(0x3fdb0e35, 0x269b4f37)},
{D(0x3fdefbde, 0xb14f42cb)},
{D(0x3fe12cf1, 0xc3c69e56)},
{D(0x3fe2a79e, 0x3a2cd808)},
{D(0x3fe3f998, 0x9320c0fa)},
{D(0x3fe52a7f, 0xa9d2fd6d)},
{D(0x3fe63fa3, 0xf3c01e06)},
{D(0x3fe73ce7, 0x04fb85a5)},
{D(0x3fe82538, 0x78ae306c)},
{D(0x3fe8fae0, 0xc15adc42)},
{D(0x3fe9bfb0, 0x76d23073)},
{D(0x3fea751f, 0x9447bc2c)},
{D(0x3feb1c62, 0xdb255ff4)},
{D(0x3febb67a, 0xe8584555)},
{D(0x3fec443f, 0x1d4d21fc)},
{D(0x3fecc665, 0xb03286b3)},
{D(0x3fed3d89, 0xbe17613e)},
{D(0x3fedaa2f, 0xefaae340)},
{D(0x3fee0cca, 0x12e97665)},
{D(0x3fee65b9, 0xedeba332)},
{D(0x3feeb553, 0x7b14369f)},
{D(0x3feefbde, 0xb14f4d8c)},
{D(0x3fef3998, 0xf1b186bf)},
{D(0x3fef6eb6, 0x2d2772f2)},
{D(0x3fef9b61, 0xd0237382)},
{D(0x3fefbfbf, 0x7ebc74e0)},
{D(0x3fefdbeb, 0xa917c367)},
{D(0x3fefeffb, 0xfdfebf9e)},
{D(0x3feffbff, 0xbff7fe80)},
{D(0x3ff00000, 0x00000000)},
{D(0x3feffbff, 0xbff7fe81)},
{D(0x3fefeffb, 0xfdfebea3)},
{D(0x3fefdbeb, 0xa917c3ba)},
{D(0x3fefbfbf, 0x7ebc745f)},
{D(0x3fef9b61, 0xd023728f)},
{D(0x3fef6eb6, 0x2d277228)},
{D(0x3fef3998, 0xf1b186e0)},
{D(0x3feefbde, 0xb14f50ae)},
{D(0x3feeb553, 0x7b14351f)},
{D(0x3fee65b9, 0xedeba3f9)},
{D(0x3fee0cca, 0x12e97af4)},
{D(0x3fedaa2f, 0xefaadf93)},
{D(0x3fed3d89, 0xbe175e82)},
{D(0x3fecc665, 0xb032898e)},
{D(0x3fec443f, 0x1d4d1f9d)},
{D(0x3febb67a, 0xe8585055)},
{D(0x3feb1c62, 0xdb2560fc)},
{D(0x3fea751f, 0x9447b910)},
{D(0x3fe9bfb0, 0x76d234c8)},
{D(0x3fe8fae0, 0xc15ad422)},
{D(0x3fe82538, 0x78ae2fe6)},
{D(0x3fe73ce7, 0x04fb7dcc)},
{D(0x3fe63fa3, 0xf3c0275b)},
{D(0x3fe52a7f, 0xa9d2f9a7)},
{D(0x3fe3f998, 0x9320b9e7)},
{D(0x3fe2a79e, 0x3a2cd615)},
{D(0x3fe12cf1, 0xc3c6a0b8)},
{D(0x3fdefbde, 0xb14f54da)},
{D(0x3fdb0e35, 0x269b3762)},
{D(0x3fd64564, 0x0568bf68)},
{D(0x3fcfbfbf, 0x7ebc7d0f)},
{D(0x00000000, 0x00000000)},
};

static const	du	Twopm28 =
{D(0x3e300000, 0x00000000)};

static const	du	Qnan =
{D(QNANHI, QNANLO)};


/* ====================================================================
 *
 * FunctionName		vacos
 *
 * Description		computes vector arccosine of arg
 *
 * ====================================================================
 */

void
__vacos( double	*x, double *y, long count, long stridex, long stridey )
{
long	i;
int	j;
double	dx, dy, dz;
double	z, w;
double	zsq, poly;
double	result;

	/* i = 0, 1, ..., count-1; y[i*stridey] = acos(x[i*stridex]) */

	for ( i=0; i<count; i++ )
	{
#ifdef _PREFETCH
#pragma prefetch_ref=*(x+8)
#pragma prefetch_ref=*(y+8)
#endif

		dx = *x;

		if ( fabs(dx) > 1.0 )
			dx = Qnan.d;

		dy = dx;

		if ( dx != dx )
			dy = 0.0;

		j = ROUND(32.0*dy);
		j += 32;
		dz = fabs(dy);
	
		z = dy*ck[j].d - sk[j].d*sqrt(1.0 - dz + dz*(1.0 - dz));
		w = z;

		/* guard against underflow for small args	*/

		if ( fabs(z) < Twopm28.d )
			w = 0.0;

		zsq = w*w;
	
		poly = ((((((P[7].d*zsq + P[6].d)*zsq + P[5].d)*zsq + 
			P[4].d)*zsq + P[3].d)*zsq + P[2].d)*zsq +
			P[1].d)*(zsq*z) + z;
	
		result = xk[j].d - poly;

		if ( dx != dx )
			result = Qnan.d;

		*y = result;

		x += stridex;
		y += stridey;
	}
}

