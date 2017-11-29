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
 * Module: vcisf.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:20-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/SCCS/s.vcisf.c $
 *
 * Revision history:
 *  06-Mar-98 - Original Version
 *
 * Description:	source code for vector cisf function
 *
 * ====================================================================
 * ====================================================================
 */

#include "libm.h"
#include "complex.h"

#if defined(mips) && !defined(__GNUC__)
extern	void	vfcis(float *, complex *, long, long, long);
extern	void	vcisf(float *, complex *, long, long, long);

#pragma weak vfcis = __vcisf
#pragma weak vcisf = __vcisf
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern void __vcisf( float *x, complex *y, long count, long stridex,
  long stridey );
#pragma weak vcisf
void vcisf( float *x, complex *y, long count, long stridex, long stridey ) {
  __vcisf(x, y, count, stridex, stridey);
}
#elif defined(__GNUC__)
extern  void  __vcisf(float *, complex *, long, long, long);
void    vcisf() __attribute__ ((weak, alias ("__vcisf")));
#endif

static const du	rpiby2 =
{D(0x3fe45f30, 0x6dc9c883)};

static const du	piby2hi =
{D(0x3ff921fb, 0x50000000)};

static const du	piby2lo =
{D(0x3e5110b4, 0x611a6263)};

static const fu	Twop28 = {0x4d800000};

/* coefficients for polynomial approximation of sin on +/- pi/4 */

static const du	P[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0xbfc55554, 0x5268a030)},
{D(0x3f811073, 0xafd14db9)},
{D(0xbf29943e, 0x0fc79aa9)},
};

/* coefficients for polynomial approximation of cos on +/- pi/4 */

static const du	Q[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0xbfdffffb, 0x2a77e083)},
{D(0x3fa553e7, 0xf02ac8aa)},
{D(0xbf5644d6, 0x2993c4ad)},
};

static const	fu	Qnan = {QNANF};


/* ====================================================================
 *
 * FunctionName		vcisf
 *
 * Description		computes vector cosine and sine of arg
 *
 * ====================================================================
 */

void
__vcisf( float	*x, complex *y, long count, long stridex, long stridey )
{
long	i;
int	n;
float	arg;
double	dx;
double	xsq;
double	sinpoly, cospoly;
complex result;
double	dn;

	/* i = 0, 1, ..., count-1; y[i*stridey] = cosf(x[i*stridex]) + i*sinf(x[i*stridex]) */

	for ( i=0; i<count; i++ )
	{
#ifdef _PREFETCH
#pragma prefetch_ref=*(x+8)
#pragma prefetch_ref=*(y+8)
#endif

		arg = *x;

		dx = arg;

		/* for large args, return 0.0	*/

		if ( fabsf(arg) >= Twop28.f )
			dx = 0.0;

		if ( arg != arg )
			dx = 0.0;

		/*  reduce argument to +/- pi/4  */
	
		dn = dx*rpiby2.d;
	
		n = ROUND(dn);
		dn = n;
	
		dx = dx - dn*piby2hi.d;
		dx = dx - dn*piby2lo.d;
	
		xsq = dx*dx;

		cospoly = ((Q[3].d*xsq + Q[2].d)*xsq + Q[1].d)*xsq + Q[0].d;
	
		sinpoly = ((P[3].d*xsq + P[2].d)*xsq + P[1].d)*(xsq*dx) + dx;

		result.real = cospoly;
		result.imag = sinpoly;

		if ( n&1 )
		{
			result.real = -sinpoly;
			result.imag = cospoly;
			n--;
		}

		if ( n&2 )
		{
			result.real = -result.real;
			result.imag = -result.imag;
		}
	
		if ( arg != arg )
		{
			result.real = Qnan.f;
			result.imag = Qnan.f;
		}
	
		y->real = result.real;
		y->imag = result.imag;
	
		x += stridex;
		y += stridey;
	}
}

