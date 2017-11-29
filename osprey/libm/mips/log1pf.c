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
 * Module: log1pf.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:22-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.log1pf.c $
 *
 * Revision history:
 *  09-Jun-93 - Original Version
 *
 * Description:	source code for log1pf function
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.log1pf.c $ $Revision: 1.5 $";

#ifdef _CALL_MATHERR
#include <stdio.h>
#include <math.h>
#include <errno.h>
#endif

#include "libm.h"

/*	Algorithm from
	"Table-Driven Implementation of the Logarithm Function in IEEE
	Floating-Point Arithmetic", Peter Tang, Argonne National Laboratory,
	ACM Transactions on Mathematical Software, Vol. 16, No. 4,
	Dec. 1990
*/

#if defined(mips) && !defined(__GNUC__)
extern	float	flog1p(float);
extern	float	log1pf(float);

#pragma weak flog1p = __log1pf
#pragma weak log1pf = __log1pf
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern float __log1pf(float);
#pragma weak log1pf
float log1pf( float x ) {
  return __log1pf( x );
}
#elif defined(__GNUC__)
extern  float  __log1pf(float);
float    log1pf(float) __attribute__ ((weak, alias ("__log1pf")));
#endif

extern	const fu	_logftabhi[];
extern	const fu	_logftablo[];

static const	fu	Qnan = {QNANF};

static const fu Neginf = {0x7f800000};

static const fu Inf = {0x7f800000};

static const fu	P[] =
{
{0x3f800000},
{0x3daaaaa9},
{0x3c4dffbb},
};

static const fu	Q[] =
{
{0x3f800000},
{0x3daaaac2},
};

static const fu	f_m_one = {0xbf800000};

static const fu	f_one = {0x3f800000};

/*  exp(-1/16) - 1   */

static const fu	T1 = {0xbd782a03};

/*  exp(1/16) - 1   */

static const fu	T2 = {0x3d8415ac};

static const fu	T3 = {0x4c800000};

static const fu	twop7 = {0x43000000};

static const fu	twopm7 = {0x3c000000};

static const fu	log2_lead = {0x3f317200};

static const fu	log2_trail = {0x35bfbe8e};


/* ====================================================================
 *
 * FunctionName		log1pf
 *
 * Description		computes log1p function of arg
 *
 * ====================================================================
 */

float
__log1pf( float x )
{
int	xpt;
float	u, v;
double	u1;
float	q;
float	result;
int	m, n;
float	y, f, F;
float	l_lead, l_trail;
int	j, k;
float	twopnegm;
float	md;
#ifdef _CALL_MATHERR
struct exception	exstruct;
#endif

	if ( x != x )
	{
		/* x is a NaN; return a quiet NaN */

#ifdef _CALL_MATHERR

                exstruct.type = DOMAIN;
                exstruct.name = "log1pf";
                exstruct.arg1 = x;
                exstruct.retval = Qnan.f;

                if ( matherr( &exstruct ) == 0 )
                {
                        fprintf(stderr, "domain error in log1pf\n");
                        SETERRNO(EDOM);
                }

                return ( exstruct.retval );
#else
		NAN_SETERRNO(EDOM);
		
		return ( Qnan.f );
#endif
	}

	if ( (T1.f < x) && (x < T2.f) )
	{
		/*  exp(-1/16) < 1 + x < exp(1/16)  */

		FLT2INT(x, xpt);	/* copy arg to an integer	*/
		xpt >>= MANTWIDTH;	/* shift off mantissa	*/
		xpt &= 0xff;

		if ( xpt >= 0x67 )
		{
			/* |x| >= 2^(-24) */

			u1 = x/(2.0 + x);
			u1 = u1 + u1;
			u = u1;
			v = u1*u1;

			q = (P[2].f*v + P[1].f)*(v*u);

			result = u1 + q;

			return ( result );
		}

		return ( x );
	}

	else if ( (x > f_m_one.f) && (x != Inf.f) )
	{
		if ( x >= T3.f )
			y = x;
		else
			y = f_one.f + x;

		FLT2INT(y, k);	/* copy y to an integer	*/
		m = (k >> MANTWIDTH);	/* shift off mantissa	*/
		m -= EXPBIAS;
		k &= 0x7fffff;
		k |= 0x3f800000;
		INT2FLT(k, y);

		u = twop7.f*y;

		j = ROUNDF(u);

		F = j;
		j -= 128;
	
		F = twopm7.f*F;

		if ( m <= -2 )
			f = y - F;
		else 
		{
			n = EXPBIAS - m;
			n <<= MANTWIDTH;

			INT2FLT(n, twopnegm);	/* build 2^(-m) */

			if ( m <= 23 )
				f = (twopnegm - F) + twopnegm*x;
			else if ( m <= 50 )
				f = (twopnegm*x - F) + twopnegm;
			else
				f = y - F;
		}

		md = m;

		l_lead = md*log2_lead.f;
		l_trail = md*log2_trail.f;

		u = (f + f)/(y + F);

		l_lead += _logftabhi[j].f;
		l_trail += _logftablo[j].f;

		v = u*u;

		q = Q[1].f*(v*u);

		result = l_lead + (u + (q + l_trail));

		return ( result );
	}

	if ( x == Inf.f )
	{
		return ( Inf.f );
	}

	if ( x == f_m_one.f )
	{
#ifdef _CALL_MATHERR

                exstruct.type = OVERFLOW;
                exstruct.name = "log1pf";
                exstruct.arg1 = x;
                exstruct.retval = Neginf.f;

                if ( matherr( &exstruct ) == 0 )
                {
                        fprintf(stderr, "overflow range error in log1pf\n");
                        SETERRNO(ERANGE);
                }

                return ( exstruct.retval );
#else

		SETERRNO(ERANGE);

		return ( Neginf.f );
#endif
	}

	/*  x < -1.0 */

#ifdef _CALL_MATHERR

        exstruct.type = DOMAIN;
        exstruct.name = "log1pf";
        exstruct.arg1 = x;
        exstruct.retval = Qnan.f;

        if ( matherr( &exstruct ) == 0 )
        {
                fprintf(stderr, "domain error in log1pf\n");
                SETERRNO(EDOM);
        }

        return ( exstruct.retval );
#else
	SETERRNO(EDOM);

	return ( Qnan.f );
#endif
}

