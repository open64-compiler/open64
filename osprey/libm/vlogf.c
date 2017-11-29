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
 * Module: vlogf.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:20-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/SCCS/s.vlogf.c $
 *
 * Revision history:
 *  02-Dec-94 - Original Version
 *
 * Description:	source code for vector log function
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/libm/SCCS/s.vlogf.c $ $Revision: 1.5 $";

#include "libm.h"

extern	const du        __log_ru[];
extern	const du        _logtab[];

#if defined(mips) && !defined(__GNUC__)
extern	void	vflog(float *, float *, long, long, long);
extern	void	vlogf(float *, float *, long, long, long);

#pragma weak vflog = __vlogf
#pragma weak vlogf = __vlogf
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern void __vlogf( float *x, float *y, long count, long stridex,
  long stridey );
#pragma weak vlogf
void vlogf( float *x, float *y, long count, long stridex, long stridey ) {
  __vlogf(x, y, count, stridex, stridey);
}
#elif defined(__GNUC__)
extern  void  __vlogf(float *, float *, long, long, long);
void    vlogf() __attribute__ ((weak, alias ("__vlogf")));
#endif

static const du	log2 =
{D(0x3fe62e42, 0xfefa39ef)};

/* coefficients for polynomial approximation of log(1 + t) on +/- 1/256   */

static const du	P[] =
{
{D(0x3fefffff, 0xffff6666)},
{D(0xbfe00006, 0x000349d2)},
{D(0x3fd55561, 0x555d346b)},
};

static const int twop7 = {0x43000000};

static const fu	twopm7 = {0x3c000000};

static const fu	Scaleup = {0x4b000000};

static const	fu	Qnan = {QNANF};

static const fu	Inf = {0x7f800000};

static const fu	Neginf = {0xff800000};

#define MAXEXP	0xffu

#define	MINEXP	0x01u


/* ====================================================================
 *
 * FunctionName		vlogf
 *
 * Description		computes vector logarithm of arg
 *
 * ====================================================================
 */

#ifdef _SW_PIPELINE

/* If compiler supports software pipelining, use this algorithm; note that
 * denormal args are not supported.
 */

void
__vlogf( float	*x, float *y, long count, long stridex, long stridey )
{
long	i;
unsigned int j;
unsigned int ix;
int	m;
int	k;
float	u;
double	t;
double	xmu;
double	q;
double	l_lead;
float	w;
float	result;

	/* i = 0, 1, ..., count-1; y[i*stridey] = logf(x[i*stridex])	*/

	for ( i=0; i<count; i++ )
	{
#ifdef _PREFETCH
#pragma prefetch_ref=*(x+8)
#pragma prefetch_ref=*(y+8)
#endif

		/* extract exponent and sign of x for some quick screening */

		ix = *(unsigned int *)x;	/* copy arg to an int	*/

		m = (ix >> MANTWIDTH);		/* shift off mantissa	*/
		j = m - MINEXP;

		m -= EXPBIAS;

		/* normalize x and compute the nearest 1/128th to x */
	
		ix &= (SIGNMASK & EXPMASK);	/* mask off sign and exponent
						 * bits of x
						 */
		ix |= twop7;	/* set exponent of x to 0x430 */

		INT2FLT(ix, w);	/* copy scaled arg to a float	*/
	
		k = ROUNDF(w);
	
		u = k;
	
		k -= 128;
	
		xmu = twopm7.f*(w - u);
	
		t = __log_ru[k].d*xmu;
	
		/* avoid loss of significance for values of x near two
		   by adjusting index; effectively u is divided by two.
		   The logtable has been adjusted for this.
		*/
	
		if ( k > 64 )
			m++;
	
		q = ((P[2].d*t + P[1].d)*t + P[0].d);
	
		l_lead = _logtab[k].d;
	
		l_lead += m*log2.d;
	
		result = l_lead + q*t;

		/* take care of negative args, NaNs, and Infinities     */

		if ( j >= (MAXEXP - MINEXP) )
			result = Qnan.f;

		if ( *x == 0.0f )
			result = Neginf.f;

		if ( *x == Inf.f )
			result = Inf.f;

		*y = result;

		x += stridex;
		y += stridey;
	}
}

#else

void
__vlogf( float	*x, float *y, long count, long stridex, long stridey )
{
long	i;
unsigned int j;
unsigned int ix;
int	m;
int	k;
float	u;
double	t;
double	xmu;
double	q;
double	l_lead;
float	w, z;
float	result;

	/* i = 0, 1, ..., count-1; y[i*stridey] = logf(x[i*stridex])	*/

	for ( i=0; i<count; i++ )
	{
		/* extract exponent and sign of x for some quick screening */

		w = z = *x;

		FLT2INT(w, ix);

		m = (ix >> MANTWIDTH);		/* shift off mantissa	*/
		j = m;

		if ( m == 0 )
		{
			z *= Scaleup.f;
			w = z;

			FLT2INT(w, ix); /* copy scaled arg to an int   */
			m = (ix >> MANTWIDTH); /* shift off mantissa	*/
			m -= 23;	/* adjust for scaling	*/
		}

		m -= EXPBIAS;

		/* normalize x and compute the nearest 1/128th to x */
	
		ix &= (SIGNMASK & EXPMASK);	/* mask off sign and exponent
						 * bits of x
						 */
		ix |= twop7;	/* set exponent of x to 0x430 */

		INT2FLT(ix, w);	/* copy scaled arg to a float	*/
	
		k = ROUNDF(w);
	
		u = k;
	
		k -= 128;
	
		xmu = twopm7.f*(w - u);
	
		t = __log_ru[k].d*xmu;
	
		/* avoid loss of significance for values of x near two
		   by adjusting index; effectively u is divided by two.
		   The logtable has been adjusted for this.
		*/
	
		if ( k > 64 )
			m++;
	
		q = ((P[2].d*t + P[1].d)*t + P[0].d);
	
		l_lead = _logtab[k].d;
	
		l_lead += m*log2.d;
	
		result = l_lead + q*t;

		/* take care of negative args, NaNs, and Infinities     */

		if ( j >= MAXEXP )
			result = Qnan.f;

		if ( z == 0.0f )
			result = Neginf.f;

		if ( z == Inf.f )
			result = Inf.f;

		*y = result;

		x += stridex;
		y += stridey;
	}
}
#endif

