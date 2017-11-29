/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


/* =======================================================================
 * =======================================================================
 *
 *  Module: quadsim.c
 *  $Revision: 1.5 $
 *  $Date: 04/12/21 14:57:27-08:00 $
 *  $Author: bos@eng-25.internal.keyresearch.com $
 *  $Source: /home/bos/bk/kpro64-pending/common/util/SCCS/s.quadsim.c $
 *
 * =======================================================================
 * =======================================================================
 */



#include "quad.h"
#include "stdio.h"
#include "stdlib.h"
#include "math.h"
#include <fp_class.h>
#include "defs.h"
#include "quadsim.h"

/* For fp_class */

#define DMANTWIDTH      52
#define DEXPWIDTH       11
#define DSIGNMASK       0x7fffffffffffffffll
#define DEXPMASK        0x800fffffffffffffll
#define DQNANBITMASK    0xfff7ffffffffffffll

#define MANTWIDTH       23
#define EXPWIDTH        8
#define SIGNMASK        0x7fffffff
#define EXPMASK         0x807fffff
#define QNANBITMASK     0xffbfffff

typedef union
{
	struct
	{
		UINT32 hi;
		UINT32 lo;
	} word;

	double	d;
} du;

static const du		m_twop31 =
{0xc1e00000,	0x00000000};

static const du		twop31m1 =
{0x41dfffff,	0xffc00000};

static const du		twop32m1 =
{0x41efffff,	0xffe00000};

static const du		twop52 =
{0x43300000,	0x00000000};

static const	du	twop62 =
{0x43d00000,	0x00000000};

static const	du	m_twop63 =
{0xc3e00000,	0x00000000};

static const	du	twop63 =
{0x43e00000,	0x00000000};

static const	du	twop64 =
{0x43f00000,	0x00000000};

static const	du	twopm916 =
{0x06b00000,	0x00000000};

static const	du	infinity =
{0x7ff00000,	0x00000000};

#define STRINGIFY(str) #str
#ifndef TARG_LOONGSON
#if defined(BUILD_OS_DARWIN)
/* Can't use "pragma weak" to create aliases in Mach-O */
#define DECLARE(ret_type, fn_name, arg_types...) \
  ret_type __ ## fn_name(arg_types); \
  ret_type fn_name(arg_types) { return __ ## fn_name(x, p_err); }
extern	INT	c_q_le(QUAD x, QUAD y, INT *p_err);
extern	INT	c_q_ge(QUAD x, QUAD y, INT *p_err);
DECLARE(INT32,	c_ji_qint, QUAD x, INT *p_err );
INT32 __c_fp_class_q(QUAD);
INT32 c_fp_class_q(QUAD x) { return __c_fp_class_q(x); }
DECLARE(UINT32,	c_ji_quint, QUAD x, INT *p_err );
DECLARE(INT64,	c_ki_qint, QUAD x, INT *p_err );
DECLARE(UINT64,	c_ki_quint, QUAD x, INT *p_err );
DECLARE(float,	c_sngl_q, QUAD x, INT *p_err );
DECLARE(double,	c_dble_q, QUAD x, INT *p_err );
DECLARE(QUAD,	c_q_flotj, INT32 x, INT *p_err );
DECLARE(QUAD,	c_q_flotju, UINT32 x, INT *p_err );
DECLARE(QUAD,	c_q_flotk, INT64 x, INT *p_err );
DECLARE(QUAD,	c_q_flotku, UINT64 x, INT *p_err );
DECLARE(QUAD,	c_q_ext,  float x, INT *p_err );
DECLARE(QUAD,	c_q_extd, double x, INT *p_err );
DECLARE(QUAD,	c_q_trunc, QUAD x, INT *p_err );
#else /* defined(BUILD_OS_DARWIN) */
#define DECLARE(ret_type, fn_name, arg_types...) \
  ret_type __ ## fn_name(arg_types); \
  ret_type fn_name(arg_types) __attribute__((weak, alias("__" #fn_name)))

extern	INT	c_q_le(QUAD x, QUAD y, INT *p_err);
extern	INT	c_q_ge(QUAD x, QUAD y, INT *p_err);
DECLARE(INT32,	c_ji_qint, QUAD x, INT *p_err );
DECLARE(INT32,	c_fp_class_q, QUAD x);
DECLARE(UINT32,	c_ji_quint, QUAD x, INT *p_err );
DECLARE(INT64,	c_ki_qint, QUAD x, INT *p_err );
DECLARE(UINT64,	c_ki_quint, QUAD x, INT *p_err );
DECLARE(float,	c_sngl_q, QUAD x, INT *p_err );
DECLARE(double,	c_dble_q, QUAD x, INT *p_err );
DECLARE(QUAD,	c_q_flotj, INT32 n, INT *p_err );
DECLARE(QUAD,	c_q_flotju, UINT32 n, INT *p_err );
DECLARE(QUAD,	c_q_flotk, INT64 n, INT *p_err );
DECLARE(QUAD,	c_q_flotku, UINT64 n, INT *p_err );
DECLARE(QUAD,	c_q_ext,  float x, INT *p_err );
DECLARE(QUAD,	c_q_extd, double x, INT *p_err );
DECLARE(QUAD,	c_q_trunc, QUAD x, INT *p_err );
#endif /* defined(BUILD_OS_DARWIN) */
extern	double	trunc(double x);

/* quad to INT32 */

INT32
__c_ji_qint(QUAD x, INT *p_err )
{
QUAD	xi;
INT32	result;

	*p_err = 0;

	if ( x.hi != x.hi )
	{
		*p_err = 1;

		return ( (INT32)x.hi );
	}

	if ( fabs(x.hi) == infinity.d )
	{
		*p_err = 1;

		return ( (INT32)x.hi );
	}

	xi = __c_q_trunc(x, p_err);

	if ( xi.hi > twop31m1.d )
		*p_err = 1;

	if ( xi.hi < m_twop31.d )
		*p_err = 1;

	result = (INT32)xi.hi;

	return ( result );
}

/* quad to UINT32 */


UINT32
__c_ji_quint(QUAD x, INT *p_err )
{
QUAD	xi;
INT64	n;
UINT32	result;

	*p_err = 0;

	if ( x.hi != x.hi )
	{
		*p_err = 1;

		return ( (UINT32)x.hi );
	}

	if ( fabs(x.hi) == infinity.d )
	{
		*p_err = 1;

		return ( (UINT32)x.hi );
	}

	xi = __c_q_trunc(x, p_err);

	if ( xi.hi > twop32m1.d )
	{
		*p_err = 1;

		return ( (UINT32)x.hi );
	}

	if ( xi.hi < 0.0 )
	{
		*p_err = 1;

		return ( (UINT32)x.hi );
	}

	n = __c_ki_qint(xi, p_err);

	result = n & 0xffffffff;

	return ( result );
}

/* quad to INT64 */

INT64
__c_ki_qint(QUAD x, INT *p_err )
{
QUAD	xi;
INT64	t;

	*p_err = 0;

	if ( x.hi != x.hi )
	{
		*p_err = 1;

		return ( (INT64)x.hi );
	}

	if ( fabs(x.hi) == infinity.d )
	{
		*p_err = 1;

		return ( (INT64)x.hi );
	}

	xi = __c_q_trunc(x, p_err);

	if ( (xi.hi > twop63.d) ||
	     ((xi.hi == twop63.d) && (xi.lo >= 0.0))
	   )
	{
		/* we get overflow here, so just let the hardware do it */

		*p_err = 1;

		return ( (INT64)xi.hi );
	}

	if ( (xi.hi < m_twop63.d) ||
	     ((xi.hi == m_twop63.d) && (xi.lo < 0.0))
	   )
	{
		/* we get overflow here, so just let the hardware do it */

		if ( xi.hi == m_twop63.d )
		{
			xi.hi *= 2.0;
		}

		*p_err = 1;

		return ( (INT64)xi.hi );
	}

	if ( fabs(xi.hi) > twop62.d )
	{
		/* add things up very carefully to avoid overflow */

		t = 0.5*xi.hi;
		return ( ((INT64)xi.lo) + t + t );
	}

	/* just do it */

	return ( (INT64)xi.hi + (INT64)xi.lo );
}

/* quad to UINT64 */

UINT64
__c_ki_quint(QUAD x, INT *p_err )
{
QUAD	xi;
double	z;
UINT64	t;

	*p_err = 0;

	if ( x.hi != x.hi )
	{
		*p_err = 1;

		return ( (UINT64)x.hi );
	}

	if ( fabs(x.hi) == infinity.d )
	{
		*p_err = 1;

		return ( (UINT64)x.hi );
	}

	xi = __c_q_trunc(x, p_err);

	if ( (xi.hi > twop64.d) ||
	     ((xi.hi == twop64.d) && (xi.lo > -1.0))
	   )
	{
		*p_err = 1;

		return ( (UINT64)x.hi );
	}

	if ( xi.hi < 0.0 )
	{
		*p_err = 1;

		return ( (UINT64)x.hi );
	}

	if ( (xi.hi > twop63.d) ||
	     (xi.hi == twop63.d) && (xi.lo >= 0.0)
	   )
	{
		/* subtract 2**64	*/

		z = xi.hi - twop64.d;

		/* normalize result */

		xi.hi = z + xi.lo;
		xi.lo = z - xi.hi + xi.lo;
	}

	if ( fabs(xi.hi) > twop62.d )
	{
		/* add things up very carefully to avoid overflow */

		t = 0.5*xi.hi;
		return ( ((UINT64)xi.lo) + t + t );
	}

	/* just do it */

	return ( (UINT64)xi.hi + (UINT64)xi.lo );
}

/* quad to float */

float
__c_sngl_q(QUAD x, INT *p_err )
{
	*p_err = 0;

	return ( (float)x.hi );
}

/* quad to double */

double
__c_dble_q(QUAD x, INT *p_err )
{
	*p_err = 0;

	return ( x.hi );
}

/* INT32 to quad */

QUAD
__c_q_flotj(INT32 n, INT *p_err )
{
QUAD	result;

	*p_err = 0;

	result.hi = n;
	result.lo = 0.0;
	return ( result );
}

/* UINT32 to quad */

QUAD
__c_q_flotju(UINT32 n, INT *p_err )
{
QUAD	result;

	*p_err = 0;

	result.hi = n;
	result.lo = 0.0;
	return ( result );
}

/* INT64 to quad */

QUAD
__c_q_flotk(INT64 n, INT *p_err )
{
INT64 m;
double  w, ww;
QUAD  result;

	*p_err = 0;

	m = (n >> 11);
        m <<= 11;

        w = m;

	ww = n - m;

        /* normalize result */

        result.hi = w + ww;
        result.lo = w - result.hi + ww;

        return ( result );
}

/* UINT64 to QUAD */

QUAD
__c_q_flotku(UINT64 n, INT *p_err )
{
UINT64 m;
double  w, ww;
QUAD  result;

	*p_err = 0;

	m = (n >> 11);
        m <<= 11;

        w = m;

	ww = n - m;

        /* normalize result */

        result.hi = w + ww;
        result.lo = w - result.hi + ww;

        return ( result );
}

/* float to quad */

QUAD
__c_q_ext( float x, INT *p_err )
{
QUAD	result;

	*p_err = 0;

	result.hi = x;
	result.lo = 0.0;
	return ( result );
}

/* double to quad */

QUAD
__c_q_extd(double x, INT *p_err )
{
QUAD	result;

	*p_err = 0;

	result.hi = x;
	result.lo = 0.0;
	return ( result );
}

/* truncates a long double, i.e. computes the integral part of
   a long double
*/

QUAD
__c_q_trunc(QUAD x, INT *p_err )
{
double	uhi, ulo;
QUAD	result;

	uhi = x.hi;
	ulo = x.lo;

	*p_err = 0;

	if ( uhi != uhi )
	{
		result.hi = uhi;
		result.lo = ulo;

		return ( result );
	}

	if ( uhi >= 0.0 )
	{
		if ( uhi < twop52.d )
		{
			/* binary point occurs in uhi; truncate uhi to an integer
			*/

			result.hi = trunc(uhi);

			result.lo = 0.0;

			if ( result.hi < uhi )
				return ( result );

			/* must adjust result by one if ulo < 0.0 */

			if ( ulo < 0.0 )
			{
				result.hi -= 1.0;

				return ( result );
			}

			return ( result );
		}
		else if ( fabs(ulo) < twop52.d )
		{
			/* binary point occurs in ulo; truncate ulo to an integer
			*/

			result.hi = uhi;

			result.lo = trunc(ulo);

			if ( result.lo > ulo )
			{
				result.lo -= 1.0;
			}

			return ( result );
		}

		/* arg is an integer */

		result.hi = uhi;
		result.lo = ulo;

		return ( result );
	}
	else
	{
		if ( fabs(uhi) < twop52.d )
		{
			/* binary point occurs in uhi; truncate uhi to an integer
			*/

			result.hi = trunc(uhi);

			result.lo = 0.0;

			if ( result.hi > uhi )
				return ( result );

			/* must adjust result by one if ulo > 0.0 */

			if ( ulo > 0.0 )
			{
				result.hi += 1.0;

				return ( result );
			}

			return ( result );
		}
		else if ( fabs(ulo) < twop52.d )
		{
			/* binary point occurs in ulo; truncate ulo to an integer
			*/

			result.hi = uhi;

			result.lo = trunc(ulo);

			if ( result.lo < ulo )
			{
				result.lo += 1.0;
			}

			return ( result );
		}

		/* arg is an integer */

		result.hi = uhi;
		result.lo = ulo;

		return ( result );
	}
}
#endif
/* ====================================================================
 *
 * FunctionName: c_fp_class_q
 *
 * Description: c_fp_class_q(u) returns the floating point class 
 *   to which u belongs, and should work equivalently to the
 *   functions declared in <fp_class.h>.
 *
 * ====================================================================
 */


INT
fp_class_d( double x )
{
  UINT64 ll, exp, mantissa;
  INT32 sign;

  ll = *(UINT64*)&x;
  exp = (ll >> DMANTWIDTH);
  sign = (exp >> DEXPWIDTH);
  exp &= 0x7ff;
  mantissa = (ll & (DSIGNMASK & DEXPMASK));
  if ( exp == 0x7ff ) {
    /* result is an infinity, or a NaN */
    if ( mantissa == 0 )
      return ( (sign == 0) ? FP_POS_INF : FP_NEG_INF );
    else if ( mantissa & ~DQNANBITMASK )
      return ( FP_QNAN );
    else
      return ( FP_SNAN );
  }

  if ( exp == 0 ) {
    if ( mantissa == 0 )
      return ( (sign == 0) ? FP_POS_ZERO : FP_NEG_ZERO );
    else
      return ( (sign == 0) ? FP_POS_DENORM : FP_NEG_DENORM );
  }
  else
    return ( (sign == 0) ? FP_POS_NORM : FP_NEG_NORM );
}

#ifdef TARG_LOONGSON
/*for loongson's long double*/
#define LDMANTWIDTH 112
#define LDEXPWIDTH 15
#define LDSIGNMASK 0x7fffffffffffffffll
#define LDEXPMASK        0x8000ffffffffffffll
#define LDQNANBITMASK    0xffff7fffffffffffll

INT32
c_fp_class_q( QUAD x )
{
  UINT64 hi, lo, exp, mantissa;
  INT32 sign;

  lo = *(UINT64*)&x;
  hi = *((UINT64*)&x + 1);
  exp = (hi >> (LDMANTWIDTH - 64));
  sign = (exp >> LDEXPWIDTH);
  exp &= 0x7fff;
  mantissa = (hi & (LDSIGNMASK & LDEXPMASK));
  if ( exp == 0x7fff ) {
    /* result is an infinity, or a NaN */
    if ( mantissa == 0 && lo == 0)
      return ( (sign == 0) ? FP_POS_INF : FP_NEG_INF );
    else if ( mantissa & ~LDQNANBITMASK )
      return ( FP_QNAN );
    else
      return ( FP_SNAN );
  }

  if ( exp == 0 ) {
    if ( mantissa == 0 && lo == 0)
      return ( (sign == 0) ? FP_POS_ZERO : FP_NEG_ZERO );
    else
      return ( (sign == 0) ? FP_POS_DENORM : FP_NEG_DENORM );
  }
  else
    return ( (sign == 0) ? FP_POS_NORM : FP_NEG_NORM );
}
#else
INT32
__c_fp_class_q(QUAD x)
{	
QUAD	y;
INT	err;
INT32	class;

	/* Notice that the definition of denormal for quad precision is
	 * chosen so that normal quads have a full 107 bits precision
	 * in their mantissas.
	 */

	class = fp_class_d(x.hi);

	if ( (class != FP_POS_NORM) && (class != FP_NEG_NORM) )
		return ( class );

	class = fp_class_d(x.lo);

	if ( (class == FP_POS_DENORM) && (class == FP_NEG_DENORM) )
		return ( class );

	y.hi = twopm916.d;
	y.lo = 0.0;

	if ( c_q_ge(x, y, &err) )
		return ( FP_POS_NORM );

	y.hi = -y.hi;

	if ( c_q_le(x, y, &err) )
		return ( FP_POS_NORM );

	if ( x.hi > 0.0 )
		return ( FP_POS_DENORM );

	return ( FP_NEG_DENORM );
}
#endif

