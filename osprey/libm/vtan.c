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
 * Module: vtan.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:21-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/SCCS/s.vtan.c $
 *
 * Revision history:
 *  01-Dec-94 - Original Version
 *
 * Description:	source code for vector tangent function
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/libm/SCCS/s.vtan.c $ $Revision: 1.5 $";

#include "libm.h"

#if defined(mips) && !defined(__GNUC__)
extern	void	vtan(double *, double *, long, long, long);

#pragma weak vtan = __vtan
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern void __vtan( double *x, double *y, long count, long stridex,
  long stridey );
#pragma weak vtan
void vtan( double *x, double *y, long count, long stridex, long stridey ) {
  __vtan(x, y, count, stridex, stridey);
}
#elif defined(__GNUC__)
extern  void  __vtan(double *, double *, long, long, long);
void    vtan() __attribute__ ((weak, alias ("__vtan")));
#endif

/* coefficients for rational approximation of tan on +/- pi/4 */

static const du	p[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0xbfc06b8f, 0x5f225706)},
{D(0x3f66fc34, 0x2943627f)},
{D(0xbedf6255, 0x88fc315e)},
};

static const du	q[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0xbfdd8b1d, 0x04e680b5)},
{D(0x3f97e798, 0xc16cfadc)},
{D(0xbf2b51d1, 0xd7f65e57)},
};

static const du	rpiby2 =
{D(0x3fe45f30, 0x6dc9c883)};

static const du	piby2hi =
{D(0x3ff921fb, 0x54400000)};

static const du	piby2lo =
{D(0x3dd0b461, 0x1a600000)};

static const du	piby2tiny =
{D(0x3ba3198a, 0x2e037073)};

static const du	Twop19xpi =
{D(0x413921fb, 0x54442d18)};

static const du	Twopm30 =
{D(0x3e100000, 0x00000000)};

static const du	Twopm85 =
{D(0x3aa00000, 0x00000000)};

static const	du	Qnan =
{D(QNANHI, QNANLO)};


/* ====================================================================
 *
 * FunctionName		vtan
 *
 * Description		computes vector tangent
 *
 * ====================================================================
 */

void
__vtan( double	*x, double *y, long count, long stridex, long stridey )
{
long	i;
int	n;
double	arg;
double	dx, w;
double	xsq;
double	num, denom;
double	dn;
double	poly1, poly2, result;

	/* i = 0, 1, ..., count-1; y[i*stridey] = tan(x[i*stridex])	*/

	for ( i=0; i<count; i++ )
	{
#ifdef _PREFETCH
#pragma prefetch_ref=*(x+8)
#pragma prefetch_ref=*(y+8)
#endif

		arg = *x;

		dx = arg;

		/* for large args, return 0.0	*/

		if ( fabs(arg) > Twop19xpi.d )
			dx = 0.0;

		if ( arg != arg )
			dx = 0.0;

		w = dx;

		if ( fabs(dx) < Twopm30.d )
			w = 0.0;

		dn = w*rpiby2.d;

		n = ROUND(dn);
		dn = n;

		dx = dx - dn*piby2hi.d;
		dx = dx - dn*piby2lo.d;
		dx = dx - dn*piby2tiny.d;	/* dx = x - n*piby2 */
	
		w = dx;

		/* guard against underflow in the computation of xsq	*/

		if ( fabs(dx) < Twopm85.d )
			w = 0.0;

		xsq = w*w;

		poly1 = ((p[3].d*xsq + p[2].d)*xsq + p[1].d)*(xsq*dx) + dx;
		poly2 = ((q[3].d*xsq + q[2].d)*xsq + q[1].d)*xsq + q[0].d;

		if ( (n&1) == 0 )
		{
			/* result is poly1/poly2 */

			num = poly1;
			denom = poly2;
		}

		if ( (n&1) != 0 )
		{
			/* result is -poly2/poly1 */

			denom = poly1;
			num = -poly2;
		}

		result = num/denom;

		if ( arg != arg )
			result = Qnan.d;
	
		*y = result;

		x += stridex;
		y += stridey;
	}
}

