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
 * Module: vasinf.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:20-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/SCCS/s.vasinf.c $
 *
 * Revision history:
 *  06-Dec-94 - Original Version
 *
 * Description:	source code for vector arcsine function
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/libm/SCCS/s.vasinf.c $ $Revision: 1.5 $";

#include "libm.h"

#if defined(mips) && !defined(__GNUC__)
extern	void	vfasin(float *, float *, long, long, long);
extern	void	vasinf(float *, float *, long, long, long);

#pragma weak vfasin = __vasinf
#pragma weak vasinf = __vasinf
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern void __vasinf( float *x, float *y, long count, long stridex,
  long stridey );
#pragma weak vasinf
void vasinf( float *x, float *y, long count, long stridex, long stridey ) {
  __vasinf(x, y, count, stridex, stridey);
}
#elif defined(__GNUC__)
extern  void  __vasinf(float *, float *, long, long, long);
void    vasinf() __attribute__ ((weak, alias ("__vasinf")));
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

/* table of angles asin(j/16), j=-16, -15, ..., 15, 16	*/

static const	du	xk[] =
{
{D(0xbff921fb, 0x54442d18)},
{D(0xbff3722d, 0x2feb24c8)},
{D(0xbff10c06, 0x6d3e6932)},
{D(0xbfee5985, 0x567b665d)},
{D(0xbfeb2353, 0x15c680dc)},
{D(0xbfe841de, 0xb5114bb4)},
{D(0xbfe59aad, 0x71ced00f)},
{D(0xbfe31df4, 0x0fbd31cd)},
{D(0xbfe0c152, 0x382d7366)},
{D(0xbfdcfaf2, 0x7460fe9f)},
{D(0xbfd899f4, 0xedc962d3)},
{D(0xbfd45739, 0x3b90e2aa)},
{D(0xbfd02be9, 0xce0b87cd)},
{D(0xbfc82494, 0xed0e78fc)},
{D(0xbfc00abe, 0x0c129e1e)},
{D(0xbfb002ab, 0xde953619)},
{D(0x00000000, 0x00000000)},
{D(0x3fb002ab, 0xde953619)},
{D(0x3fc00abe, 0x0c129e1e)},
{D(0x3fc82494, 0xed0e78fc)},
{D(0x3fd02be9, 0xce0b87cd)},
{D(0x3fd45739, 0x3b90e2aa)},
{D(0x3fd899f4, 0xedc962d3)},
{D(0x3fdcfaf2, 0x7460fe9f)},
{D(0x3fe0c152, 0x382d7366)},
{D(0x3fe31df4, 0x0fbd31cd)},
{D(0x3fe59aad, 0x71ced00f)},
{D(0x3fe841de, 0xb5114bb4)},
{D(0x3feb2353, 0x15c680dc)},
{D(0x3fee5985, 0x567b665d)},
{D(0x3ff10c06, 0x6d3e6932)},
{D(0x3ff3722d, 0x2feb24c8)},
{D(0x3ff921fb, 0x54442d18)},
};

/* sines of the above angles */

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

/* cosines of the above angles */

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
 * FunctionName		vasinf
 *
 * Description		computes vector arcsine of arg
 *
 * ====================================================================
 */

void
__vasinf( float	*x, float *y, long count, long stridex, long stridey )
{
long	i;
int	j;
float	arg;
float	result;
double	dy, dz;
double	z, w;
double	zsq, poly;

	/* i = 0, 1, ..., count-1; y[i*stridey] = asinf(x[i*stridex]) */

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
	
		result = xk[j].d + poly;

		if ( arg != arg )
			result = Qnan.f;

		*y = result;

		x += stridex;
		y += stridey;
	}
}

