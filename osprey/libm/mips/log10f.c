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
 * Module: log10f.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:22-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.log10f.c $
 *
 * Revision history:
 *  09-Jun-93 - Original Version
 *
 * Description:	source code for log10f function
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.log10f.c $ $Revision: 1.5 $";

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
extern	float	flog10(float);
extern	float	log10f(float);

#pragma weak flog10 = __log10f
#pragma weak log10f = __log10f
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern float __log10f(float);
#pragma weak log10f
float log10f( float x ) {
  return __log10f( x );
}
#elif defined(__GNUC__)
extern  float  __log10f(float);
float    log10f(float) __attribute__ ((weak, alias ("__log10f")));
#endif

extern	const fu	_logftabhi[];
extern	const fu	_logftablo[];

static const	fu	Qnan = {QNANF};

static const fu Neginf = {0xff800000};

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

static const fu	log2_lead = {0x3f317200};

static const fu	log2_trail = {0x35bfbe8e};

static const fu	f_one = {0x3f800000};

static const fu	Scaleup = {0x4b000000};

/* exp(-1/16) */

static const int lim1 = {0x3f707d5f};

/* exp(1/16) */

static const int lim2 = {0x3f88415b};

static const int one = {0x3f800000};

static const du	Loge =
{D(0x3fdbcb7b, 0x1526e50e)};


/* ====================================================================
 *
 * FunctionName		log10f
 *
 * Description		computes common logarithm of arg
 *
 * ====================================================================
 */

float
__log10f( float x )
{
int	ix;
int	j, m;
float	u, v;
float	result;
float	f, F;
float	l_lead, l_trail;
float	q;
float	md;
double	u1;
#ifdef _CALL_MATHERR
struct exception	exstruct;
#endif

	FLT2INT(x, ix);		/* copy arg to an integer	*/
	m = (ix >> MANTWIDTH);	/* shift off mantissa	*/

	if ( (0 < m) && (m < 0xff) )
	{
		/* x is positive and finite */

		if ( (ix >= lim2) || (ix <= lim1) )
		{
	
			m -= EXPBIAS;
L:
			ix &= EXPMASK;
			ix |= 0x43000000;
			INT2FLT(ix, x);

			j = ROUNDF(x);

			F = j;
			j -= 128;
		
			f = x - F;

			md = m;

			u = (f + f)/(x + F);

			l_lead = md*log2_lead.f;
			l_trail = md*log2_trail.f;

			l_lead += _logftabhi[j].f;
			l_trail += _logftablo[j].f;

			v = u*u;

			q = Q[1].f*(v*u);

			result = l_lead + (u + (q + l_trail));

			return ( result*Loge.d );
	
		}

		if ( ix == one )
			return ( 0.0f );

		x = x - f_one.f;

		u1 = x/(2.0 + x);
		u1 = u1 + u1;
		u = u1;
		v = u1*u1;

		q = (P[2].f*v + P[1].f)*(v*u);

		result = u1 + q;

		return ( result*Loge.d );
	}

	if ( x != x )
	{
		/* x is a NaN; return a quiet NaN */

#ifdef _CALL_MATHERR

                exstruct.type = DOMAIN;
                exstruct.name = "log10f";
                exstruct.arg1 = x;
                exstruct.retval = Qnan.f;

                if ( matherr( &exstruct ) == 0 )
                {
                        fprintf(stderr, "domain error in log10f\n");
                        SETERRNO(EDOM);
                }

                return ( exstruct.retval );
#else
		NAN_SETERRNO(EDOM);
		
		return ( Qnan.f );
#endif
	}

	if ( x == Inf.f )
	{
		SETERRNO(EDOM);

		return ( Inf.f );
	}

	if ( x == 0.0f )
	{
zeroarg:

#ifdef _CALL_MATHERR

                exstruct.type = OVERFLOW;
                exstruct.name = "log10f";
                exstruct.arg1 = x;
                exstruct.retval = Neginf.f;

                if ( matherr( &exstruct ) == 0 )
                {
                        fprintf(stderr, "overflow range error in log10f\n");
                        SETERRNO(ERANGE);
                }

                return ( exstruct.retval );
#else
		SETERRNO(ERANGE);
		
		return ( Neginf.f );
#endif
	}

	if ( x < 0.0f )
	{
#ifdef _CALL_MATHERR

                exstruct.type = DOMAIN;
                exstruct.name = "log10f";
                exstruct.arg1 = x;
                exstruct.retval = Qnan.f;

                if ( matherr( &exstruct ) == 0 )
                {
                        fprintf(stderr, "domain error in log10f\n");
                        SETERRNO(EDOM);
                }

                return ( exstruct.retval );
#else
		SETERRNO(EDOM);

		return ( Qnan.f );
#endif
	}

	/* take care of denormals by scaling them up and adjusting the unbiased
	   exponent of x
	*/

	x *= Scaleup.f;	/* multiply x by 2^23 */

	if ( x == 0.0f )
		goto zeroarg;

	FLT2INT(x, ix);		/* copy x to an integer	*/
	m = (ix >> MANTWIDTH);	/* shift off mantissa   */
	m -= EXPBIAS;
	m -= 23;
	goto L;
}

