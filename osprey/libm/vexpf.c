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
 * Module: vexpf.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:20-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/SCCS/s.vexpf.c $
 *
 * Revision history:
 *  03-Dec-94 - Original Version
 *
 * Description:	source code for vector exponential function
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/libm/SCCS/s.vexpf.c $ $Revision: 1.5 $";

#include "libm.h"

/*	Algorithm adapted from
	"Table-driven Implementation of the Exponential Function in
	IEEE Floating Point Arithmetic", Peter Tang, ACM Transactions on
	Mathematical Software, Vol. 15, No. 2, June 1989
 */

#if defined(mips) && !defined(__GNUC__)
extern	void	vfexp(float *, float *, long, long, long);
extern	void	vexpf(float *, float *, long, long, long);

#pragma weak vfexp = __vexpf
#pragma weak vexpf = __vexpf
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern void __vexpf( float *x, float *y, long count, long stridex,
  long stridey );
#pragma weak vexpf
void vexpf( float *x, float *y, long count, long stridex, long stridey ) {
  __vexpf(x, y, count, stridex, stridey);
}
#elif defined(__GNUC__)
extern  void  __vexpf(float *, float *, long, long, long);
void    vexpf() __attribute__ ((weak, alias ("__vexpf")));
#endif

extern	const du	__expftab[];

/* coefficients for polynomial approximation of exp on +/- log(2)/64    */

static const du	P[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0x3ff00000, 0x00000000)},
{D(0x3fe00008, 0x745da559)},
{D(0x3fc55569, 0x9fd0029e)},
};

static const du	rln2by32 =
{D(0x40471547, 0x652b82fe)};

static const du	ln2by32hi =
{D(0x3f962e42, 0xfef00000)};

static const du	ln2by32lo =
{D(0x3d8473de, 0x6af278ed)};

static const fu	Ulimit = {0x42b17218};

static const fu	Llimit = {0xc2cff1b5};

static const	fu	Qnan = {QNANF};


/* ====================================================================
 *
 * FunctionName		vexpf
 *
 * Description		computes vector exponential of arg
 *
 * ====================================================================
 */

void
__vexpf( float	*x, float *y, long count, long stridex, long stridey )
{
#ifdef _32BIT_MACHINE

int	l;

#else

long long l;

#endif

long	i;
int	j, m, n;
float	arg, w, result;
double	z;
double	dx;
double	nd;
double	poly;
double	s;
double	twopm;

	/* i = 0, 1, ..., count-1; y[i*stridey] = expf(x[i*stridex])	*/

	for ( i=0; i<count; i++ )
	{
#ifdef _PREFETCH
#pragma prefetch_ref=*(x+8)
#pragma prefetch_ref=*(y+8)
#endif

		arg = *x;
		w = arg;

		if ( arg > Ulimit.f )
			w = Ulimit.f;
			
		if ( arg < Llimit.f )
			w = Llimit.f;

		if ( arg != arg )
			w = 0.0f;

		dx = w;

		/* reduce dx to +/- log(2)/64    */

		nd = dx*rln2by32.d;
		n = ROUND(nd);
		nd = n;

		z = dx - nd*ln2by32hi.d - nd*ln2by32lo.d;

		j = n & 0x1f;
		m = n >> 5;

		s = __expftab[j].d;

		poly = (P[3].d*z + P[2].d)*(z*z) + z;

		l = m + DEXPBIAS;
		l <<= DMANTWIDTH;

#ifdef _32BIT_MACHINE

		twopm = 0.0;
		INT2DBLHI(l, twopm);
#else
		LL2DBL(l, twopm);
#endif
		result = twopm*(s + s*poly);

		if ( arg != arg )
			result = Qnan.f;

		*y = result;
	
		x += stridex;
		y += stridey;
	}
}

