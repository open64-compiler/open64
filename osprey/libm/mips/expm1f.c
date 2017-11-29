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
 * Module: expm1f.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:21-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.expm1f.c $
 *
 * Revision history:
 *  09-Jun-93 - Original Version
 *
 * Description:	source code for expm1f function
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.expm1f.c $ $Revision: 1.5 $";

#ifdef _CALL_MATHERR
#include <stdio.h>
#include <math.h>
#include <errno.h>
#endif

#include "libm.h"

/*	Algorithm adapted from
	"Table-driven Implementation of the Expm1 Function in IEEE
	Floating Point Arithmetic", Peter Tang, ACM Transactions on
	Mathematical Software, Vol. 18, No. 2, June 1992
 */

#if defined(mips) && !defined(__GNUC__)
extern	float	fexpm1(float);
extern	float	expm1f(float);

#pragma weak fexpm1 = __expm1f
#pragma weak expm1f = __expm1f
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern float __expm1f(float);
#pragma weak expm1f
float expm1f( float x ) {
  return __expm1f( x );
}
#elif defined(__GNUC__)
extern  float  __expm1f(float);
float   expm1f(float) __attribute__ ((weak, alias ("__expm1f")));
#endif

extern	const fu	_expftabhi[];
extern	const fu	_expftablo[];

static const du	rln2by32 =
{D(0x40471547, 0x652b82fe)};

static const du	ln2by32hi =
{D(0x3f962e42, 0xfef00000)};

static const du	ln2by32lo =
{D(0x3d8473de, 0x6af278ed)};

/* coefficients for polynomial approximation of exp on +/- log(2)/64     */

static const du	Q[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0x3ff00000, 0x00000000)},
{D(0x3fe00008, 0x745da559)},
{D(0x3fc55569, 0x9fd0029e)},
};

/* coefficients for polynomial approximation of exp(x) - 1 on the interval
   (log(1 - 1/4), log(1 + 1/4))
*/

static const du	P[] =
{
{D(0x3fc55555, 0x38c176b9)},
{D(0x3fa55554, 0x87c6c1df)},
{D(0x3f811170, 0xfb837cdf)},
{D(0x3f56c854, 0xfd0d216d)},
{D(0x3f28d63a, 0x66b2059b)},
};

static const	fu	Qnan = {QNANF};

static const fu Inf = {0x7f800000};

static const fu	m_one = {0xbf800000};

static const fu	Twopm100 = {0xd8000000};

/* log(1 - 1/4)   */

/* 2^-(k-7), k = 0, ..., 30    */

static	const fu	twopowmm[] =
{
{0x43000000},
{0x42800000},
{0x42000000},
{0x41800000},
{0x41000000},
{0x40800000},
{0x40000000},
{0x3f800000},
{0x3f000000},
{0x3e800000},
{0x3e000000},
{0x3d800000},
{0x3d000000},
{0x3c800000},
{0x3c000000},
{0x3b800000},
{0x3b000000},
{0x3a800000},
{0x3a000000},
{0x39800000},
{0x39000000},
{0x38800000},
{0x38000000},
{0x37800000},
{0x37000000},
{0x36800000},
{0x36000000},
{0x35800000},
{0x35000000},
{0x34800000},
{0x34000000},
};

static const fu	T1 = {0xbe934b11};

/* log(1 + 1/4)   */

static const fu	T2 = {0x3e647fbf};

static const fu	Ulimit = {0x42b17217};

static const fu	Llimit = {0xc18aa122};


/* ====================================================================
 *
 * FunctionName		expm1f
 *
 * Description		computes expm1 function of arg
 *
 * ====================================================================
 */

float
__expm1f( float x )
{
#ifdef _32BIT_MACHINE

int	l;

#else

long long l;

#endif

int	xpt, ix;
int	n, m, j, k;
float	twopmm;
float	s_lead, s_trail;
double	twopm;
double	dx;
double	y1, y2, y;
double	p, q;
double	nd;
double	s;
double	result;
#ifdef _CALL_MATHERR
struct exception	exstruct;
#endif

	FLT2INT(x, ix);		/* copy arg to an integer	*/
	xpt = (ix >> MANTWIDTH);
	xpt &= 0xff;

	if ( xpt < 0xff )
	{
		/* x is finite */

		if ( (T1.f < x) && (x < T2.f) )
		{
			/* log(1 - 1/4) < x < log(1 + 1/4) */

			if ( xpt >= 0x66 )
			{
				/* |x| >= 2^(-25) */

				dx = x;

				q = ((((P[4].d*dx + P[3].d)*dx + P[2].d)*dx +
					P[1].d)*dx + P[0].d)*(dx*dx*dx);

				result = q + 0.5*dx*dx + dx;

				return ( (float)result );

			}

			return ( x );
		}
		else if ( x > Ulimit.f )
		{
#ifdef _CALL_MATHERR
                        exstruct.type = OVERFLOW;
                        exstruct.name = "expm1f";
                        exstruct.arg1 = x;
                        exstruct.retval = Inf.f;

                        if ( matherr( &exstruct ) == 0 )
                        {
                                fprintf(stderr, "overflow range error in expm1f\n");
                                SETERRNO(ERANGE);
                        }

                        return ( exstruct.retval );
#else
			SETERRNO(ERANGE);
			
			return ( Inf.f );
#endif
		}
		else if ( x < Llimit.f )
		{
			return ( m_one.f + Twopm100.f );
		}
		else
		{
			dx = x;
			nd = dx*rln2by32.d;
			n = ROUND(nd);
			nd = n;

			y1 = dx - nd*ln2by32hi.d;
			y2 = nd*ln2by32lo.d;
			y = y1 - y2;

			j = n & 0x1f;
			m = n >> 5;

			s_lead = _expftabhi[j].f;
			s_trail = _expftablo[j].f;

			q = (Q[3].d*y + Q[2].d)*(y*y);

			p = (q - y2) + y1;

			l = m + DEXPBIAS;
			l <<= DMANTWIDTH;

#ifdef _32BIT_MACHINE

			twopm = 0.0;
			INT2DBLHI(l, twopm);
#else
			LL2DBL(l, twopm);
#endif
			if ( m >= 48 )
			{
				s = s_lead + s_trail;
				result = s_lead + (s*p + s_trail);
				result *= twopm;
				return ( (float)result );
			}
			else if ( m >= 24 )
			{
				s = s_lead + s_trail;
				k = ((EXPBIAS - m) << 23);
				twopmm = *(float *)&k;
				result = s_lead + (s*p + (s_trail - twopmm));
				result *= twopm;
				return ( (float)result );
			}
			else if ( m >= -7 )
			{
				result = (s_lead - twopowmm[m+7].f) +
					 (s_lead*p + s_trail*(1.0 + p));
				result *= twopm;
				return ( (float)result );
			}
			else
			{
				s = s_lead + s_trail;
				result = s_lead + (s*p + s_trail);
				result *= twopm;
				result -= 1.0;
				return ( (float)result );
			}
		}
	}
	else if ( x != x )
	{
		/* x is a NaN; return a quiet NaN */

#ifdef _CALL_MATHERR

                exstruct.type = DOMAIN;
                exstruct.name = "expm1f";
                exstruct.arg1 = x;
                exstruct.retval = Qnan.f;

                if ( matherr( &exstruct ) == 0 )
                {
                        fprintf(stderr, "domain error in expm1f\n");
                        SETERRNO(EDOM);
                }

                return ( exstruct.retval );
#else
		NAN_SETERRNO(EDOM);
		
		return ( Qnan.f );
#endif
	}
	else if ( x == Inf.f )
	{
#ifdef _CALL_MATHERR

                exstruct.type = DOMAIN;
                exstruct.name = "expm1f";
                exstruct.arg1 = x;
                exstruct.retval = Inf.f;

                if ( matherr( &exstruct ) == 0 )
                {
                        fprintf(stderr, "overflow range error in expm1f\n");
                        SETERRNO(ERANGE);
                }

                return ( exstruct.retval );
#else
		SETERRNO(ERANGE);

		return ( Inf.f );
#endif
	}
	else	/*  x == -inf  */

		return ( m_one.f );
}

