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
 * Module: vtanf.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:21-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/SCCS/s.vtanf.c $
 *
 * Revision history:
 *  03-Dec-94 - Original Version
 *
 * Description:	source code for vector tangent function
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/libm/SCCS/s.vtanf.c $ $Revision: 1.5 $";

#include "libm.h"

#if defined(mips) && !defined(__GNUC__)
extern	void	vftan(float *, float *, long, long, long);
extern	void	vtanf(float *, float *, long, long, long);

#pragma weak vftan = __vtanf
#pragma weak vtanf = __vtanf
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern void __vtanf( float *x, float *y, long count, long stridex,
  long stridey );
#pragma weak vtanf
void vtanf( float *x, float *y, long count, long stridex, long stridey ) {
  __vtanf(x, y, count, stridex, stridey);
}
#elif defined(__GNUC__)
extern  void  __vtanf(float *, float *, long, long, long);
void    vtanf() __attribute__ ((weak, alias ("__vtanf")));
#endif

/* coefficients for rational approximation of tan on +/- pi/4 */

static const du	p[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0xbfbc81c6, 0x2c5816af)},
{D(0x3f519bb5, 0x82dc4bbf)},
};

static const du	q[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0xbfdc75c6, 0xdf240b98)},
{D(0x3f905aab, 0xa06cbb7f)},
};

static const du	rpiby2 =
{D(0x3fe45f30, 0x6dc9c883)};

static const du	piby2hi =
{D(0x3ff921fb, 0x50000000)};

static const du	piby2lo =
{D(0x3e5110b4, 0x611a6263)};

static const fu	Twop28 = {0x4d800000};

static const	fu	Qnan = {QNANF};


/* ====================================================================
 *
 * FunctionName		vtanf
 *
 * Description		computes vector tangent
 *
 * ====================================================================
 */

void
__vtanf( float	*x, float *y, long count, long stridex, long stridey )
{
long	i;
int	n;
float	arg, w;
double	dx;
double	xsq;
double	num, denom;
double	dn;
double	poly1, poly2;
float	result;

	/* i = 0, 1, ..., count-1; y[i*stridey] = tanf(x[i*stridex])	*/

	for ( i=0; i<count; i++ )
	{
#ifdef _PREFETCH
#pragma prefetch_ref=*(x+8)
#pragma prefetch_ref=*(y+8)
#endif

		arg = *x;
		w = arg;

		if ( arg != arg )
			w = 0.0f;

		/* for large args, return 0.0	*/

		if ( fabsf(w) >= Twop28.f )
			w = 0.0f;

		/* |x| < 2^28 */
	
		/* use the standard algorithm from Cody and Waite, doing
		   the computations in double precision
		*/

		dx = w;

		dn = dx*rpiby2.d;
		n = ROUND(dn);
		dn = n;

		dx = dx - dn*piby2hi.d;
		dx = dx - dn*piby2lo.d;	/* dx = x - n*piby2 */

		xsq = dx*dx;

		poly1 = (p[2].d*xsq + p[1].d)*xsq*dx + dx;
		poly2 = (q[2].d*xsq + q[1].d)*xsq + q[0].d;

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
			result = Qnan.f;
	
		*y = result;

		x += stridex;
		y += stridey;
	}
}

