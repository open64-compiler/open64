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
 * Module: pow.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:22-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.pow.c $
 *
 * Revision history:
 *  09-Jun-93 - Original Version
 *
 * Description:	source code for pow function
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.pow.c $ $Revision: 1.5 $";

#ifdef _CALL_MATHERR
#include <stdio.h>
#include <math.h>
#include <errno.h>
#endif

#include "libm.h"

/*	Algorithm based on
	"Table-driven Implementation of the Exponential Function in
	IEEE Floating Point Arithmetic", Peter Tang, ACM Transactions on
	Mathematical Software, Vol. 15, No. 2, June 1989
	and
	"Table-Driven Implementation of the Logarithm Function in IEEE
	Floating-Point Arithmetic", Peter Tang, Argonne National Laboratory,
	ACM Transactions on Mathematical Software, Vol. 16, No. 4, Dec. 1990
*/

#if defined(mips) && !defined(__GNUC__)
extern	double	pow(double, double);

#pragma weak pow = __pow
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern double __pow(double, double);
#pragma weak pow
double pow( double arg1, double arg2) {
  return __pow( arg1, arg2 );
}
#elif defined(__GNUC__)
extern  double  __pow(double, double);

double    pow() __attribute__ ((weak, alias ("__pow")));

#endif

extern	const du	_exptabhi[];
extern	const du	_exptablo[];
extern	const du	_logtabhi[];
extern	const du	_logtablo[];
extern	const du	_log_rulo[];
extern	const fu	_log_ruhi[];

static	const du	Qnan =
{D(QNANHI, QNANLO)};

static const	du	Neginf =
{D(0xfff00000, 0x00000000)};

static const	du	Inf =
{D(0x7ff00000, 0x00000000)};

static	const du	twopm7 =
{D(0x3f800000, 0x00000000)};

static	const du	twopm55 =
{D(0x3c800000, 0x00000000)};

static	const du	twop52 =
{D(0x43300000, 0x00000000)};

static	const du	twop53 =
{D(0x43400000, 0x00000000)};

static	const du	twopm54 =
{D(0x3c900000, 0x00000000)};

static	const du	twop512 =
{D(0x5ff00000, 0x00000000)};

static	const du	twopm1021 =
{D(0x00200000, 0x00000000)};

/* values < sqrt(2)/2^538 underflow when squared */

static	const du	root2by2p538 =
{D(0x1e56a09e, 0x667f3bcd)};

/* if x > 0.0, then x^y either underflows or overflows
   for values of y > ylimit 
*/

static	const du	ylimit =
{D(0x43d74910, 0xd52d3051)};

static	const du	three2703 =
{D(0x40dfefc0, 0x00000000)};

static	const du	twopm1024 =
{D(0x00040000, 0x00000000)};

static const du	log2_lead =
{D(0x3fe62e42, 0xfefa4000)};

static const du	log2_trail =
{D(0xbd48432a, 0x1b0e2634)};

static const du	ymin =
{D(0x3b500000, 0x00000000)};

static const du	Scaleup =
{D(0x43300000, 0x00000000)};

static const du	Magic =
{D(0x43380000, 0x00000000)};

/* exp(z) underflows for values of z < Llimit */

static const du	Llimit =
{D(0xc0874910, 0xd52d3052)};

/* exp(z) overflows for values of z > Ulimit */

static const du	Ulimit =
{D(0x40862e42, 0xfefa39ef)};

static const du	rln2by32 =
{D(0x40471547, 0x652b82fe)};

static const du	ln2by32hi =
{D(0x3f962e42, 0xfef00000)};

static const du	ln2by32lo =
{D(0x3d8473de, 0x6af278ed)};

static const du	one =
{D(0x3ff00000, 0x00000000)};

/*  coefficients for polynomial expansion of log(1 + t) on +/- 1/256  */

static const du	P[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0xbfe00000, 0x00000000)},
{D(0x3fd55555, 0x55555557)},
{D(0xbfd00000, 0x00000001)},
{D(0x3fc99999, 0x9989c6a5)},
{D(0xbfc55555, 0x554a41aa)},
{D(0x3fc2493f, 0xe154c5f3)},
{D(0xbfc00015, 0xd8d52d9d)},
};

/*  coefficients for polynomial expansion of exp on +/- log(2)/64  */

static const du	Q[] =
{
{D(0x3ff00000, 0x00000000)},
{D(0x3ff00000, 0x00000000)},
{D(0x3fe00000, 0x00000000)},
{D(0x3fc55555, 0x55548f7c)},
{D(0x3fa55555, 0x55545d4e)},
{D(0x3f811115, 0xb7aa905e)},
{D(0x3f56c172, 0x8d739765)},
};

#ifdef _32BIT_MACHINE

static const int	twop7 =
{0x40600000};

#else

static const long long	twop7 =
{0x4060000000000000ll};

#endif


/* ====================================================================
 *
 * FunctionName		pow
 *
 * Description		computes power function of args
 *
 * ====================================================================
 */

double
__pow( arg1, arg2 )
double	arg1, arg2;
{
#ifdef _32BIT_MACHINE

int	ix, iy, xptx, xpty;
int	l;
int	signx;

#else

long long	ix, iy, xptx, xpty;
long long	l;
long long	signx;

#endif

double	x, y;
int	i;
int	k;
int	j, m, n;
double	s, z, zz;
double	result;
double	u;
double	xmu, tlo, thi, t, tt;
double	q;
double	l_lead, l_trail;
double	low, high;
double	w, ww, v;
double	p;
double	nd;
double	s_lead, s_trail;
double	x1, x2;
double	twopm;
double	sign;
#ifndef _FUSED_MADD
double	ylo, yhi;
double	zhi, zlo;
#endif
#ifdef _CALL_MATHERR
struct exception	exstruct;
#endif


	/* computes x**y as exp(y*log(x)), calculating log(x) to extra precision */


	x = arg1;
	y = arg2;

	sign = 1.0;

	/* extract exponents of x and y for some quick screening */

#ifdef _32BIT_MACHINE

	DBLHI2INT(x, ix);	/* copy MSW of x to ix */
	DBLHI2INT(y, iy);	/* copy MSW of y to iy */
#else
	DBL2LL(x, ix);		/* copy x to ix */
	DBL2LL(y, iy);		/* copy y to iy */
#endif
	xptx = (ix >> DMANTWIDTH);
	signx = ((xptx >> DEXPWIDTH) & 1);
	xptx = (xptx & 0x7ff);

	xpty = (iy >> DMANTWIDTH);
	xpty &= 0x7ff;

	if ( xptx == 0 )
	{
		x *= Scaleup.d;
	
		/* depending on the mode of the processor, x may now be zero, so
		   we need another test
		*/

		if ( x != 0.0 )
		{
	
#ifdef _32BIT_MACHINE

			DBLHI2INT(x, ix);	/* copy MSW of x to ix */
#else
			DBL2LL(x, ix);		/* copy x to ix */
#endif
			xptx = (ix >> DMANTWIDTH);
			xptx &= 0x7ff;
			xptx -= 52;
		}
	}

	/* screen args for zeroes, denormals, infinities, nans, for x < 0
	   and for y == +/-1.0
	*/

	i = (x == 0.0) | (xptx == 0x7ff) | (ix <= 0);
	j = (y == 0.0) | (xpty == 0x7ff) | (fabs(y) == 1.0);

	if ( i + j != 0 )
		goto special;

L:

	/* screen for a few more easy cases */

	if ( x == 1.0 )
		goto xeqone;

	if ( y == 2.0 )
		goto yeqtwo;

	if ( fabs(y) > ylimit.d )
		goto overunder;

	if ( xpty < 0x3b5 )
	{
		if ( y > 0.0 )
			y = ymin.d;
		else
			y = -ymin.d;
	}

	xptx -= DEXPBIAS;
	ix &= (DSIGNMASK & DEXPMASK);
	ix |= twop7;

#ifdef _32BIT_MACHINE

	INT2DBLHI(ix, x);
#else
	LL2DBL(ix, x);
#endif
	k = ROUND(x);

	u = k;

	k -= 128;

	/* t = (x - u[k])/u[k], so 1 + t = x/u[k], and
	   log(x) = log(u[k]) + log(1 + t).  The first
	   term is tabulated, and the second is computed
	   in extra precision using an 8th degree polynomial.
	*/

	xmu = twopm7.d*(x - u);

	tlo = _log_rulo[k].d*xmu;
	thi = _log_ruhi[k].f*xmu;

	t = thi + tlo;
	tt = tlo - (t - thi);

	/* avoid loss of significance for values of x near two by
	   adjusting index;  The logtable has been adjusted for this.
	*/

	if ( k > 64 )
		xptx++;

	/* compute t*t/2 as s + z */

	u = (float)t;
	v = t - u;
	s = u*u*0.5;
	z = v*(t + u)*0.5;

	w = s + z;
	ww = z - (w - s);

	l_lead = _logtabhi[k].d;
	l_trail = _logtablo[k].d;

	l_lead += xptx*log2_lead.d;
	l_trail += xptx*log2_trail.d;

	/*  compute q = log(1 + t) - t - t*t  */

	q = (((((P[7].d*t + P[6].d)*t + P[5].d)*t +
		P[4].d)*t + P[3].d)*t + P[2].d)*(t*t*t);


/*
	Compute:
	high = l_lead + (t + (w + ww))
	low = (q + (l_trail + (tt - t*tt)))

	Note that -(t + tt)^2/2 = -t*t/2 - t*tt - tt*tt/2; the last term
	is insignificant, but the middle term must be added to the low
	part of the sum.
*/

	high = t - w;
	ww = w + (high - t) + ww;

	w = l_lead + high;
	ww = high - (w - l_lead) - ww;

	low =  (q + (l_trail + (tt - t*tt)));

	ww += low;

	/* multiply y*(w + ww) by splitting y and w + ww into pieces */

#ifdef _FUSED_MADD

	t = y*w;
	tt = y*w - t;

	tt += y*ww;

	/* normalize the result */

	z = t + tt;
	zz = t - z + tt;

#else

	zhi = (float)w;
	zlo = w - zhi + ww;

	yhi = (float)y;
	ylo = y - yhi;

	w = yhi*zhi;	/* this product is exact */
	ww = ylo*zhi + y*zlo;

	/* normalize the result */

	z = w + ww;
	zz = ww - (z - w);

#endif

	/* compute exp(z + zz) */

	if ( fabs(z) < twopm55.d )
		return ( sign*one.d );

	/* check for underflow/overflow of result */

	if ( z < Llimit.d )
	{
		/* indicate underflow */

#ifdef _CALL_MATHERR

		exstruct.type = UNDERFLOW;
		exstruct.name = "pow";
		exstruct.arg1 = arg1;
		exstruct.arg2 = arg2;
		exstruct.retval = (sign > 0.0) ? 0.0 : -0.0;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "underflow range error in pow\n");
			SETERRNO(ERANGE);
		}

		return ( exstruct.retval );
#else
		SETERRNO(ERANGE);

		return ( (sign > 0.0) ? 0.0 : -0.0 );
#endif
	}

	if ( z > Ulimit.d )
	{
		/* indicate overflow */

#ifdef _CALL_MATHERR

		exstruct.type = OVERFLOW;
		exstruct.name = "pow";
		exstruct.arg1 = arg1;
		exstruct.arg2 = arg2;
		exstruct.retval = (sign > 0.0) ? Inf.d : Neginf.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "overflow range error in pow\n");
			SETERRNO(ERANGE);
		}

		return ( exstruct.retval );
#else
		SETERRNO(ERANGE);

		return ( (sign > 0.0) ? Inf.d : Neginf.d );
#endif
	}

	nd = z*rln2by32.d;
	n = ROUND(nd);
	nd = n;

	j = n & 0x1f;
	m = n >> 5;

	s_lead = _exptabhi[j].d;
	s_trail = _exptablo[j].d;
	s = s_lead + s_trail;

	x1 = z - nd*ln2by32hi.d;

	x2 = nd*ln2by32lo.d;

	x2 -= zz;
	y = x1 - x2;

	/* reduced argument is y + yy */

	/*  compute q = exp(y) - y - 1.0  */

	q = ((((Q[6].d*y + Q[5].d)*y + Q[4].d)*y + Q[3].d)*y +
		Q[2].d)*(y*y);

	p = q - x2 + x1;

	result = s_lead + (s_trail + s*p);

	if ( fabs(nd) <= three2703.d )
	{
		/* Guarantees -1021 <= m <= 1021, ensuring that m can
		   be safely added to the exponent of z.
		*/

#ifdef _32BIT_MACHINE

		twopm = 0.0;
		l = DEXPBIAS + m;
		l <<= DMANTWIDTH;
		INT2DBLHI(l, twopm);
#else
		l = DEXPBIAS + m;
		l <<= DMANTWIDTH;
		LL2DBL(l, twopm);
#endif
		result *= twopm;

		return ( sign*result );
	}

	if ( m > 1021 )
	{
#ifdef _32BIT_MACHINE

		DBLHI2INT(result, l);
#else
		DBL2LL(result, l);
#endif
		n = (l >> DMANTWIDTH);
		n &= 0x7ff;

		if ( (m + n) > 0x7fe )
		{
			/* result overflows */

#ifdef _CALL_MATHERR

			exstruct.type = OVERFLOW;
			exstruct.name = "pow";
			exstruct.arg1 = arg1;
			exstruct.arg2 = arg2;
			exstruct.retval = (sign > 0.0) ? Inf.d : Neginf.d;

			if ( matherr( &exstruct ) == 0 )
			{
				fprintf(stderr, "overflow range error in pow\n");
				SETERRNO(ERANGE);
			}

			return ( exstruct.retval );
#else
			SETERRNO(ERANGE);

			return ( (sign > 0.0) ? Inf.d : Neginf.d );
#endif
		}

#ifdef _32BIT_MACHINE

		l += (m << DMANTWIDTH);
		INT2DBLHI(l, result);
#else
		l += ((long long)m << DMANTWIDTH);
		LL2DBL(l, result);
#endif

		return ( sign*result );
	}
	else /* m < -1021 */
	{
		m += 1021;

#ifdef _32BIT_MACHINE

		twopm = 0.0;
		l = DEXPBIAS + m;
		l <<= DMANTWIDTH;
		INT2DBLHI(l, twopm);
#else
		l = DEXPBIAS + m;
		l <<= DMANTWIDTH;
		LL2DBL(l, twopm);
#endif
		result *= twopm;

		if ( fabs(result) <= twopm54.d )
		{
			/* result underflows */

#ifdef _CALL_MATHERR

			exstruct.type = UNDERFLOW;
			exstruct.name = "pow";
			exstruct.arg1 = arg1;
			exstruct.arg2 = arg2;
			exstruct.retval = (sign > 0.0) ? 0.0 : -0.0;

			if ( matherr( &exstruct ) == 0 )
			{
				fprintf(stderr, "underflow range error in pow\n");
				SETERRNO(ERANGE);
			}

			return ( exstruct.retval );
#else
			SETERRNO(ERANGE);

			return ( (sign > 0.0) ? 0.0 : -0.0 );
#endif
		}

		return ( twopm1021.d*sign*result );
	}

special:
	if ( y == 0.0 )
		return ( one.d );

	if ( y == 1.0 )
		return ( arg1 );

	if ( (y != y) || (x != x) )
	{
		/* y or x is a NaN; return a quiet NaN */

#ifdef _CALL_MATHERR

		exstruct.type = DOMAIN;
		exstruct.name = "pow";
		exstruct.arg1 = arg1;
		exstruct.arg2 = arg2;
		exstruct.retval = Qnan.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "domain error in pow\n");
			SETERRNO(EDOM);
		}

		return ( exstruct.retval );
#else

		NAN_SETERRNO(EDOM);

		return ( Qnan.d );
#endif
	}

	if ( (y == Inf.d) && (x != 0.0) )
	{
		if ( fabs(x) > 1.0 )
		{
			/* indicate overflow */

#ifdef _CALL_MATHERR

			exstruct.type = OVERFLOW;
			exstruct.name = "pow";
			exstruct.arg1 = arg1;
			exstruct.arg2 = arg2;
			exstruct.retval = Inf.d;

			if ( matherr( &exstruct ) == 0 )
			{
				fprintf(stderr, "overflow range error in pow\n");
				SETERRNO(ERANGE);
			}

			return ( exstruct.retval );
#else
			SETERRNO(ERANGE);

			return ( Inf.d );
#endif
		}

		if ( fabs(x) < 1.0 )
		{
			/* indicate underflow */

#ifdef _CALL_MATHERR

			exstruct.type = UNDERFLOW;
			exstruct.name = "pow";
			exstruct.arg1 = arg1;
			exstruct.arg2 = arg2;
			exstruct.retval = 0.0;

			if ( matherr( &exstruct ) == 0 )
			{
				fprintf(stderr, "underflow range error in pow\n");
				SETERRNO(ERANGE);
			}

			return ( exstruct.retval );
#else
			SETERRNO(ERANGE);

			return ( 0.0 );
#endif
		}
	}

	if ( (y == Neginf.d) && (x != 0.0) )
	{
		if ( fabs(x) < 1.0  )
		{
			/* indicate overflow */

#ifdef _CALL_MATHERR

			exstruct.type = OVERFLOW;
			exstruct.name = "pow";
			exstruct.arg1 = arg1;
			exstruct.arg2 = arg2;
			exstruct.retval = Inf.d;

			if ( matherr( &exstruct ) == 0 )
			{
				fprintf(stderr, "overflow range error in pow\n");
				SETERRNO(ERANGE);
			}

			return ( exstruct.retval );
#else
			SETERRNO(ERANGE);

			return ( Inf.d );
#endif
		}

		if ( fabs(x) > 1.0 )
		{
			/* indicate underflow */

#ifdef _CALL_MATHERR

			exstruct.type = UNDERFLOW;
			exstruct.name = "pow";
			exstruct.arg1 = arg1;
			exstruct.arg2 = arg2;
			exstruct.retval = 0.0;
	
			if ( matherr( &exstruct ) == 0 )
			{
				fprintf(stderr, "underflow range error in pow\n");
				SETERRNO(ERANGE);
			}
	
			return ( exstruct.retval );
#else
			SETERRNO(ERANGE);

			return ( 0.0 );
#endif
		}
	}

	if ( (x == 1.0) && (fabs(y) == Inf.d) )
		return ( 1.0 );

	if ( (x == -1.0) && (fabs(y) == Inf.d) )
	{
		/* indicate domain error */

#ifdef _CALL_MATHERR

		exstruct.type = DOMAIN;
		exstruct.name = "pow";
		exstruct.arg1 = arg1;
		exstruct.arg2 = arg2;
		exstruct.retval = Qnan.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "domain error in pow\n");
			SETERRNO(ERANGE);
		}

		return ( exstruct.retval );
#else

		SETERRNO(ERANGE);

		return ( Qnan.d );
#endif
	}

	if ( (x == 0.0) && (signx == 0) )
	{
		if ( y > 0.0 )
			return ( 0.0 );

		if ( y < 0.0 )
		{
			/* indicate overflow */

#ifdef _CALL_MATHERR

                        exstruct.type = OVERFLOW;
                        exstruct.name = "pow";
                        exstruct.arg1 = arg1;
                        exstruct.arg2 = arg2;
                        exstruct.retval = Inf.d;

                        if ( matherr( &exstruct ) == 0 )
                        {
                                fprintf(stderr, "overflow range error in pow\n");
                                SETERRNO(ERANGE);
                        }

                        return ( exstruct.retval );
#else

			SETERRNO(ERANGE);

			return ( Inf.d );
#endif
		}
	}

	if ( y == -1.0 )
	{
		if ( fabs(arg1) > twopm1024.d )
		{
			return ( 1/arg1 );
		}

		/* result overflows */

#ifdef _CALL_MATHERR

                exstruct.type = OVERFLOW;
                exstruct.name = "pow";
                exstruct.arg1 = arg1;
                exstruct.arg2 = arg2;
                exstruct.retval = (x > 0.0) ? Inf.d : Neginf.d;

                if ( matherr( &exstruct ) == 0 )
                {
                        fprintf(stderr, "overflow range error in pow\n");
                        SETERRNO(ERANGE);
                }

                return ( exstruct.retval );
#else

		SETERRNO(ERANGE);

		if ( x > 0.0 )
			return ( Inf.d );
		else
			return ( Neginf.d );
#endif
	}

	if ( (x == 0.0) && (signx != 0) )
	{
		/* test for y an odd integer */

		if ( fabs(y) >= twop53.d )
		{
			goto next;
		}
		else if ( fabs(y) >= twop52.d )
		{
			/* y is an integer; rightmost bit determines parity */

#ifdef _32BIT_MACHINE

			DBLLO2INT(y, l);
#else
			DBL2LL(y, l);
#endif
		}
		else
		{
			u = y + Magic.d;

#ifdef _32BIT_MACHINE

			DBLLO2INT(u, l);
#else
			DBL2LL(u, l);
#endif
			u = u - Magic.d;

			if ( u != y )
			{
				/* y is not an integer */

				goto next;
			}

		}

		if ( (l&1) == 1 )
		{
			if ( y > 0.0 )
				return ( -0.0 );
			else
			{
				/* indicate overflow */

#ifdef _CALL_MATHERR

	                        exstruct.type = OVERFLOW;
	                        exstruct.name = "pow";
	                        exstruct.arg1 = arg1;
	                        exstruct.arg2 = arg2;
	                        exstruct.retval = Neginf.d;

	                        if ( matherr( &exstruct ) == 0 )
	                        {
	                                fprintf(stderr, "overflow range error in pow\n");
	                                SETERRNO(ERANGE);
	                        }

                        	return ( exstruct.retval );
#else

				SETERRNO(ERANGE);

				return ( Neginf.d );
#endif
			}

		}

next:
		if ( y > 0.0 )
			return ( 0.0 );

		if ( y < 0.0 )
		{
			/* indicate overflow */

#ifdef _CALL_MATHERR

                        exstruct.type = OVERFLOW;
                        exstruct.name = "pow";
                        exstruct.arg1 = arg1;
                        exstruct.arg2 = arg2;
                        exstruct.retval = Inf.d;

                        if ( matherr( &exstruct ) == 0 )
                        {
                                fprintf(stderr, "overflow range error in pow\n");
                                SETERRNO(ERANGE);
                        }

                        return ( exstruct.retval );
#else

			SETERRNO(ERANGE);

			return ( Inf.d );
#endif
		}
	}

	if ( x == Inf.d )
	{
		if ( y > 0.0 )
			return ( Inf.d );

		if ( y < 0.0 )
			return ( 0.0 );
	}

	if ( x == Neginf.d )
	{
		/* test for y an odd integer */

		if ( fabs(y) >= twop53.d )
		{
			goto next_2;
		}
		else if ( fabs(y) >= twop52.d )
		{
			/* y is an integer; rightmost bit determines parity */

#ifdef _32BIT_MACHINE

			DBLLO2INT(y, l);
#else
			DBL2LL(y, l);
#endif
		}
		else
		{
			u = y + Magic.d;

#ifdef _32BIT_MACHINE

			DBLLO2INT(u, l);
#else
			DBL2LL(u, l);
#endif
			u = u - Magic.d;

			if ( u != y )
			{
				/* y is not an integer */

				goto next_2;
			}
		}

		if ( (l&1) == 1 )
		{
			if ( y > 0.0 )
				return ( Neginf.d );
			else
				return ( -0.0 );

		}

next_2:
		if ( y > 0.0 )
			return ( Inf.d );

		if ( y < 0.0 )
			return ( 0.0 );
	}

	if ( x < 0.0 )
	{
		/* check if y is an integer */
	
		if ( fabs(y) >= twop53.d )
		{
			sign = 1.0;
		}
		else if ( fabs(y) >= twop52.d )
		{
			/* y is an integer; rightmost bit determines parity */

#ifdef _32BIT_MACHINE

			DBLLO2INT(y, l);
#else
			DBL2LL(y, l);
#endif
			if ( l%2 == 0 )
			{
				sign = 1.0;
			}
			else
			{
				sign = -1.0;
			}
		}
		else
		{
			/* determine nearest integer to y */
	
			u = y + Magic.d;

#ifdef _32BIT_MACHINE

			DBLLO2INT(u, l);
#else
			DBL2LL(u, l);
#endif
			u = u - Magic.d;
	
			if ( u != y )
			{
				/* y is not an integer */
	
#ifdef _CALL_MATHERR
				exstruct.type = DOMAIN;
				exstruct.name = "pow";
				exstruct.arg1 = x;
				exstruct.arg2 = y;
				exstruct.retval = Qnan.d;

				if ( matherr( &exstruct ) == 0 )
				{
					fprintf(stderr, "domain error in pow\n");
					SETERRNO(EDOM);
				}

				return ( exstruct.retval );
#else
				SETERRNO(EDOM);

				return ( Qnan.d );
#endif
			}
	
			if ( l%2 == 0 )
			{
				sign = 1.0;
			}
			else
			{
				sign = -1.0;
			}
		}

		x = fabs(x);
	}

	goto L;

xeqone:
	return ( sign*x );

yeqtwo:
	if ( fabs(arg1) >= twop512.d )
	{
		/* result overflows */

#ifdef _CALL_MATHERR

		exstruct.type = OVERFLOW;
		exstruct.name = "pow";
		exstruct.arg1 = arg1;
		exstruct.arg2 = arg2;
		exstruct.retval = Inf.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "overflow range error in pow\n");
			SETERRNO(ERANGE);
		}

		return ( exstruct.retval );
#else
		SETERRNO(ERANGE);

		return ( Inf.d );
#endif
	}

	if ( fabs(arg1) < root2by2p538.d )
	{
		/* result underflows */

#ifdef _CALL_MATHERR

		exstruct.type = UNDERFLOW;
		exstruct.name = "pow";
		exstruct.arg1 = arg1;
		exstruct.arg2 = arg2;
		exstruct.retval = 0.0;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "underflow range error in pow\n");
			SETERRNO(ERANGE);
		}

		return ( exstruct.retval );
#else
		SETERRNO(ERANGE);

		return ( 0.0 );
#endif
	}

	return ( arg1*arg1 );

overunder:

	/* result underflows or overflows depending on sign of y
	   and magnitude and sign of x
	*/

	if ( ((x > 1.0) && (y > 0.0)) ||
	     ((x < 1.0) && (y < 0.0))
	   )
	{
		/* result overflows */

#ifdef _CALL_MATHERR

		exstruct.type = OVERFLOW;
		exstruct.name = "pow";
		exstruct.arg1 = arg1;
		exstruct.arg2 = arg2;
		exstruct.retval = (sign > 0.0) ? Inf.d : Neginf.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "overflow error in pow\n");
			SETERRNO(ERANGE);
		}

		return ( exstruct.retval );
#else
		SETERRNO(ERANGE);

		return ( (sign > 0.0) ? Inf.d : Neginf.d );
#endif
	}

	/* result underflows */

#ifdef _CALL_MATHERR

	exstruct.type = UNDERFLOW;
	exstruct.name = "pow";
	exstruct.arg1 = arg1;
	exstruct.arg2 = arg2;
	exstruct.retval = (sign > 0.0) ? 0.0 : -0.0;

	if ( matherr( &exstruct ) == 0 )
	{
		fprintf(stderr, "underflow error in pow\n");
		SETERRNO(ERANGE);
	}

	return ( exstruct.retval );
#else
	SETERRNO(ERANGE);

	return ( (sign > 0.0) ? 0.0 : -0.0 );
#endif

}

#ifdef NO_LONG_DOUBLE

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern long double __powl(long double, long double);
long double powl( long double arg1, long double arg2) {
  return ( (long double)__pow((double)arg1, (double)arg2) );
}
#elif defined(__GNUC__)
extern  long double  __powl(long double, long double);

long double    powl() __attribute__ ((weak, alias ("__powl")));

#endif

long double
__powl( arg1, arg2 )
long double	arg1, arg2;
{
	return ( (long double)__pow((double)arg1, (double)arg2) );
}

#endif

