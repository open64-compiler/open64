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
 * Module: vcis.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:20-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/SCCS/s.vcis.c $
 *
 * Revision history:
 *  03-Mar-98 - Original Version
 *
 * Description:	source code for vector cis function
 *
 * ====================================================================
 * ====================================================================
 */

#include "libm.h"
#include "complex.h"

#if defined(mips) && !defined(__GNUC__)
extern	void	vcis(double *, dcomplex *, long, long, long);

#pragma weak vcis = __vcis
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern void __vcis( double *x, dcomplex *y, long count, long stridex,
  long stridey );
#pragma weak vcis
void vcis( double *x, dcomplex *y, long count, long stridex, long stridey ) {
  __vcis(x, y, count, stridex, stridey);
}
#elif defined(__GNUC__)
extern  void  __vcis(double *, dcomplex *, long, long, long);
void    vcis() __attribute__ ((weak, alias ("__vcis")));
#endif

static const du	rpiby2 =
{D(0x3fe45f30, 0x6dc9c883)};

static const du	piby2hi =
{D(0x3ff921fb, 0x54400000)};

static const du	piby2lo =
{D(0x3dd0b461, 0x1a600000)};

static const du	piby2tiny =
{D(0x3ba3198a, 0x2e037073)};

static const du	Twopm30 =
{D(0x3e100000, 0x00000000)};

static const du	Twop19xpi =
{D(0x413921fb, 0x54442d18)};

/* coefficients for polynomial approximation of sin on +/- pi/2 */

static const du	P[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0xbfc55555, 0x55555548)},
{D(0x3f811111, 0x1110f7d0)},
{D(0xbf2a01a0, 0x19bfdf03)},
{D(0x3ec71de3, 0x567d4896)},
{D(0xbe5ae5e5, 0xa9291691)},
{D(0x3de5d8fd, 0x1fcf0ec1)},
};

/* coefficients for polynomial approximation of cos on +/- pi/2 */

static const du	Q[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0xbfdfffff, 0xffffff96)},
{D(0x3fa55555, 0x5554f0ab)},
{D(0xbf56c16c, 0x1640aaca)},
{D(0x3efa019f, 0x81cb6a1d)},
{D(0xbe927df4, 0x609cb202)},
{D(0x3e21b8b9, 0x947ab5c8)},
};

static const	du	Qnan =
{D(QNANHI, QNANLO)};



/* ====================================================================
 *
 * FunctionName		vcis
 *
 * Description		computes vector sine and cosine of arg
 *
 * ====================================================================
 */

void
__vcis( double	*x, dcomplex *y, long count, long stridex, long stridey )
{
long	i;
int	n;
double	dx, arg;
double	w;
double	xsq;
double	sinpoly, cospoly;
double	resultr, resulti;
double	dn;

	/* i = 0, 1, ..., count-1; y[i*stridey] = cos(x[i*stridex]) + i*sin(x[i*stridex]) */

	for ( i=0; i<count; i++ )
	{
#ifdef _PREFETCH
#pragma prefetch_ref=*(x+8)
#pragma prefetch_ref=*(y+8)
#endif

		arg = *x;

		dx = arg;

		if ( arg != arg )
			dx = 0.0;

		/* for large args, return 0.0	*/

		if ( fabs(arg) > Twop19xpi.d )
			dx = 0.0;

		/*  reduce argument to +/- pi/4  */
	
		w = dx;

		if ( fabs(arg) < Twopm30.d )
			w = 0.0;

		dn = w*rpiby2.d;

		n = ROUND(dn);
		dn = n;
	
		dx = dx - dn*piby2hi.d;
		dx = dx - dn*piby2lo.d;
		dx = dx - dn*piby2tiny.d;	/* dx = x - n*pi/2 */
	
		w = dx;

		if ( fabs(arg) < Twopm30.d )
			w = 0.0;

		xsq = w*w;

		cospoly = (((((Q[6].d*xsq + Q[5].d)*xsq + Q[4].d)*xsq 
			+ Q[3].d)*xsq + Q[2].d)*xsq + Q[1].d)*xsq + Q[0].d;
	
		sinpoly = (((((P[6].d*xsq + P[5].d)*xsq + P[4].d)*xsq 
			+ P[3].d)*xsq + P[2].d)*xsq + P[1].d)*(xsq*dx) + dx;

		resultr = cospoly;
		resulti = sinpoly;

		if ( n&1 )
		{
			resultr = -sinpoly;
			resulti = cospoly;
			n--;
		}

		if ( n&2 )
		{
			resultr = -resultr;
			resulti = -resulti;
		}
	
		if ( arg != arg )
		{
			resultr = Qnan.d;
			resulti = Qnan.d;
		}
	
		y->dreal = resultr;
		y->dimag = resulti;
	
		x += stridex;
		y += stridey;
	}
}

