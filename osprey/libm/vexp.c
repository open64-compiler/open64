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
 * Module: vexp.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:20-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/SCCS/s.vexp.c $
 *
 * Revision history:
 *  01-Dec-94 - Original Version
 *
 * Description:	source code for vector exponential function
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/libm/SCCS/s.vexp.c $ $Revision: 1.5 $";

#include "libm.h"

/*	Algorithm adapted from
	"Table-driven Implementation of the Exponential Function in
	IEEE Floating Point Arithmetic", Peter Tang, ACM Transactions on
	Mathematical Software, Vol. 15, No. 2, June 1989
 */

#if defined(mips) && !defined(__GNUC__)
extern	void	vexp(double *, double *, long, long, long);

#pragma weak vexp = __vexp
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern void __vexp( double *x, double *y, long count, long stridex,
  long stridey );
#pragma weak vexp
void vexp( double *x, double *y, long count, long stridex, long stridey ) {
  __vexp(x, y, count, stridex, stridey);
}
#elif defined(__GNUC__)
extern  void  __vexp(double *, double *, long, long, long);
void    vexp() __attribute__ ((weak, alias ("__vexp")));
#endif

extern	const du	__exptabhi[];
extern	const du	__exptablo[];

static const du	Ulimit =
{D(0x40862e42, 0xfefa3940)};

static const du	Llimit =
{D(0xc0874910, 0xd52d3052)};

static const du	rln2by32 =
{D(0x40471547, 0x652b82fe)};

static const du	ln2by32hi =
{D(0x3f962e42, 0xfef00000)};

static const du	ln2by32lo =
{D(0x3d8473de, 0x6af278ed)};

static const du	Twopm54 =
{D(0x3c900000, 0x00000000)};

static const	du	Qnan =
{D(QNANHI, QNANLO)};

/* coefficients for polynomial approximation of exp on +/- log(2)/64     */

static const du	P[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0x3ff00000, 0x00000000)},
{D(0x3fe00000, 0x00000000)},
{D(0x3fc55555, 0x55548f7c)},
{D(0x3fa55555, 0x55545d4e)},
{D(0x3f811115, 0xb7aa905e)},
{D(0x3f56c172, 0x8d739765)},
};


/* ====================================================================
 *
 * FunctionName		vexp
 *
 * Description		computes vector exponential of arg
 *
 * ====================================================================
 */

void
__vexp( double	*x, double *y, long count, long stridex, long stridey )
{
#ifdef _32BIT_MACHINE

int	l;

#else

long long l;

#endif

long i;
int	m, m1, m2;
int	j, n;
double	w;
double	y1, y2, z;
double	p, q;
double	dx;
double	nd;
double	s, s_lead, s_trail;
double	twopm1, twopm2;
double	result;

	/* i = 0, 1, ..., n-1; y[i*stridey] = exp(x[i*stridex])	*/

	for ( i=0; i<count; i++ )
	{
#ifdef _PREFETCH
#pragma prefetch_ref=*(x+8)
#pragma prefetch_ref=*(y+8)
#endif

		dx = w = *x;

		if ( dx != dx )
			dx = 0.0;

		if ( dx > Ulimit.d )
			dx = Ulimit.d;
			
		if ( dx < Llimit.d )
			dx = Llimit.d;

		if ( fabs(dx) < Twopm54.d )
			dx = 0.0;
	
		/* reduce dx to +/- log(2)/64     */

		nd = dx*rln2by32.d;
		n = ROUND(nd);
		nd = n;

		y1 = dx - nd*ln2by32hi.d;
		y2 = nd*ln2by32lo.d;
		z = y1 - y2;

		j = n & 0x1f;
		m = n >> 5;

		s_lead = __exptabhi[j].d;
		s_trail = __exptablo[j].d;
		s = s_lead + s_trail;

		q = ((((P[6].d*z + P[5].d)*z + P[4].d)*z + P[3].d)*z +
			P[2].d)*(z*z);

		p = (q - y2) + y1;

		result = s_lead + (s_trail + s*p);

		m1 = (m >> 1);
		m2 = m - m1;

#ifdef _32BIT_MACHINE

		twopm1 = 0.0;
		l = m1 + DEXPBIAS;
		l <<= DMANTWIDTH;

		INT2DBLHI(l, twopm1);

		twopm2 = 0.0;
		l = m2 + DEXPBIAS;
		l <<= DMANTWIDTH;

		INT2DBLHI(l, twopm2);
#else
		l = m1 + DEXPBIAS;
		l <<= DMANTWIDTH;

		LL2DBL(l, twopm1);

		l = m2 + DEXPBIAS;
		l <<= DMANTWIDTH;

		LL2DBL(l, twopm2);
#endif
		result *= twopm1;
		result *= twopm2;

		if ( w != w )
			result = Qnan.d;

		*y = result;

		x += stridex;
		y += stridey;
	}
}

