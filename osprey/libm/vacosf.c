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
 * Module: vacosf.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:20-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/SCCS/s.vacosf.c $
 *
 * Revision history:
 *  06-Dec-94 - Original Version
 *
 * Description:	source code for vector arccosine function
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/libm/SCCS/s.vacosf.c $ $Revision: 1.5 $";

#include "libm.h"

#if defined(mips) && !defined(__GNUC__)
extern	void	vfacos(float *, float *, long, long, long);
extern	void	vacosf(float *, float *, long, long, long);

#pragma weak vfacos = __vacosf
#pragma weak vacosf = __vacosf
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern void __vacosf(float *x, float *y, long count, long stridex,
  long stridey);
#pragma weak vacosf
void  vacosf(float *x, float *y, long count, long stridex, long stridey) {
  return __vacosf(x, y, count, stridex, stridey);
}
#elif defined(__GNUC__)
extern  void  __vacosf(float *, float *, long, long, long);
void    vacosf() __attribute__ ((weak, alias ("__vacosf")));
#endif

/* coefficients for polynomial approximation of asin on +/- sqrt(1 - (31/32)**2)    */

static const du	P[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0x3fc55555, 0x1e90d17f)},
{D(0x3fb33376, 0x21b67e84)},
{D(0x3fa6c21f, 0x8479e804)},
{D(0x3fa16abf, 0x8af6da01)},
};

/* table of angles pi/2 - asin(j/16), j=-16, -15, ..., 15, 16	*/

static const	du	xk[] =
{
{D(0x400921fb, 0x54442d18)},
{D(0x40064a14, 0x4217a8f0)},
{D(0x40051700, 0xe0c14b25)},
{D(0x4004275e, 0xffc0f023)},
{D(0x400359d2, 0x6f93b6c3)},
{D(0x4002a175, 0x57666979)},
{D(0x4001f7a9, 0x0695ca90)},
{D(0x4001587a, 0xae1162ff)},
{D(0x4000c152, 0x382d7366)},
{D(0x4000305b, 0xf8ae3660)},
{D(0x3fff4878, 0x8fb685cd)},
{D(0x3ffe37c9, 0xa32865c3)},
{D(0x3ffd2cf5, 0xc7c70f0c)},
{D(0x3ffc268d, 0xf1e5fc38)},
{D(0x3ffb2353, 0x15c680dc)},
{D(0x3ffa2226, 0x122d807a)},
{D(0x3ff921fb, 0x54442d18)},
{D(0x3ff821d0, 0x965ad9b7)},
{D(0x3ff720a3, 0x92c1d955)},
{D(0x3ff61d68, 0xb6a25df9)},
{D(0x3ff51700, 0xe0c14b25)},
{D(0x3ff40c2d, 0x055ff46e)},
{D(0x3ff2fb7e, 0x18d1d464)},
{D(0x3ff1e33e, 0xb72bed71)},
{D(0x3ff0c152, 0x382d7366)},
{D(0x3fef2602, 0x98cb2864)},
{D(0x3feca949, 0x36b98a22)},
{D(0x3fea0217, 0xf3770e7d)},
{D(0x3fe720a3, 0x92c1d955)},
{D(0x3fe3ea71, 0x520cf3d3)},
{D(0x3fe02be9, 0xce0b87cd)},
{D(0x3fd6bf38, 0x91642142)},
{D(0x00000000, 0x00000000)},
};

/* sines of angles asin(j/16),  j=-16, -15, ..., 15, 16	*/

static const	du	sk[] =
{
{D(0xbff00000, 0x00000000)},
{D(0xbfee0000, 0x00000000)},
{D(0xbfec0000, 0x00000000)},
{D(0xbfea0000, 0x00000000)},
{D(0xbfe80000, 0x00000000)},
{D(0xbfe60000, 0x00000000)},
{D(0xbfe40000, 0x00000000)},
{D(0xbfe20000, 0x00000000)},
{D(0xbfe00000, 0x00000000)},
{D(0xbfdc0000, 0x00000000)},
{D(0xbfd80000, 0x00000000)},
{D(0xbfd40000, 0x00000000)},
{D(0xbfd00000, 0x00000000)},
{D(0xbfc80000, 0x00000000)},
{D(0xbfc00000, 0x00000000)},
{D(0xbfb00000, 0x00000000)},
{D(0x00000000, 0x00000000)},
{D(0x3fb00000, 0x00000000)},
{D(0x3fc00000, 0x00000000)},
{D(0x3fc80000, 0x00000000)},
{D(0x3fd00000, 0x00000000)},
{D(0x3fd40000, 0x00000000)},
{D(0x3fd80000, 0x00000000)},
{D(0x3fdc0000, 0x00000000)},
{D(0x3fe00000, 0x00000000)},
{D(0x3fe20000, 0x00000000)},
{D(0x3fe40000, 0x00000000)},
{D(0x3fe60000, 0x00000000)},
{D(0x3fe80000, 0x00000000)},
{D(0x3fea0000, 0x00000000)},
{D(0x3fec0000, 0x00000000)},
{D(0x3fee0000, 0x00000000)},
{D(0x3ff00000, 0x00000000)},
};

/* cosines of angles asin(j/16),  j=-16, -15, ..., 15, 16	*/

static const	du	ck[] =
{
{D(0x00000000, 0x00000000)},
{D(0x3fd64564, 0x0568c1c3)},
{D(0x3fdefbde, 0xb14f4eda)},
{D(0x3fe2a79e, 0x3a2cd2e6)},
{D(0x3fe52a7f, 0xa9d2f8ea)},
{D(0x3fe73ce7, 0x04fb7b23)},
{D(0x3fe8fae0, 0xc15ad38a)},
{D(0x3fea751f, 0x9447b724)},
{D(0x3febb67a, 0xe8584caa)},
{D(0x3fecc665, 0xb0328622)},
{D(0x3fedaa2f, 0xefaae1d8)},
{D(0x3fee65b9, 0xedeba38e)},
{D(0x3feefbde, 0xb14f4eda)},
{D(0x3fef6eb6, 0x2d27730d)},
{D(0x3fefbfbf, 0x7ebc755f)},
{D(0x3fefeffb, 0xfdfebf1f)},
{D(0x3ff00000, 0x00000000)},
{D(0x3fefeffb, 0xfdfebf1f)},
{D(0x3fefbfbf, 0x7ebc755f)},
{D(0x3fef6eb6, 0x2d27730d)},
{D(0x3feefbde, 0xb14f4eda)},
{D(0x3fee65b9, 0xedeba38e)},
{D(0x3fedaa2f, 0xefaae1d8)},
{D(0x3fecc665, 0xb0328622)},
{D(0x3febb67a, 0xe8584caa)},
{D(0x3fea751f, 0x9447b724)},
{D(0x3fe8fae0, 0xc15ad38a)},
{D(0x3fe73ce7, 0x04fb7b23)},
{D(0x3fe52a7f, 0xa9d2f8ea)},
{D(0x3fe2a79e, 0x3a2cd2e6)},
{D(0x3fdefbde, 0xb14f4eda)},
{D(0x3fd64564, 0x0568c1c3)},
{D(0x00000000, 0x00000000)},
};

static const	fu	Qnan = {QNANF};


/* ====================================================================
 *
 * FunctionName		vacosf
 *
 * Description		computes vector arccosine of arg
 *
 * ====================================================================
 */

void
__vacosf( float	*x, float *y, long count, long stridex, long stridey )
{
long	i;
int	j;
float	arg;
float	result;
double	dy, dz;
double	z, w;
double	zsq, poly;

	/* i = 0, 1, ..., count-1; y[i*stridey] = acosf(x[i*stridex]) */

	for ( i=0; i<count; i++ )
	{
#ifdef _PREFETCH
#pragma prefetch_ref=*(x+8)
#pragma prefetch_ref=*(y+8)
#endif

		arg = *x;

		if ( fabsf(arg) > (float)1.0 )
			arg = Qnan.f;

		dy = arg;

		if ( arg != arg )
			dy = 0.0;

		j = ROUND(16.0*dy);
		j += 16;
		dz = fabs(dy);
	
		z = dy*ck[j].d - sk[j].d*sqrt(1.0 - dz + dz*(1.0 - dz));
		w = z;

		zsq = w*w;
	
		poly = (((P[4].d*zsq + P[3].d)*zsq + P[2].d)*zsq + 
			P[1].d)*(zsq*z) + z;
	
		result = xk[j].d - poly;

		if ( arg != arg )
			result = Qnan.f;

		*y = result;

		x += stridex;
		y += stridey;
	}
}

