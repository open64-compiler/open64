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
 * Module: powf.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:22-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.powf.c $
 *
 * Revision history:
 *  09-Jun-93 - Original Version
 *
 * Description:	source code for powf function
 *
 * ====================================================================
 * ====================================================================
 */

static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.powf.c $ $Revision: 1.5 $";

#ifdef _CALL_MATHERR
#include <stdio.h>
#include <math.h>
#include <errno.h>
#endif

#include "libm.h"

extern	double	__log(double);
extern	double	__exp(double);

#if defined(mips) && !defined(__GNUC__)
extern  float   fpow(float, float);
extern  float   powf(float, float);

#pragma weak fpow = __powf
#pragma weak powf = __powf
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern float __powf(float, float);
#pragma weak powf
float powf( float x, float y ) {
  return __powf( x, y );
}
#elif defined(__GNUC__)
extern  float  __powf(float, float);
float    powf(float, float) __attribute__ ((weak, alias ("__powf")));
#endif

static const du Neginf =
{D(0xfff00000, 0x00000000)};

static const du Inf =
{D(0x7ff00000, 0x00000000)};

static const du	twopm25 =
{D(0x3c800000, 0x00000000)};

static const du log2 =
{D(0x3fe62e42, 0xfefa39ef)};

static const du	ylimit = 
{D(0x41d9fe36, 0x60000000)};

static const du	Ulimit = 
{D(0x40562e42, 0xfeda39ef)};

static const du	Llimit = 
{D(0xc059fe36, 0x82cd3be4)};

static const du	twopm128 = 
{D(0x37f00000, 0x00000000)};

static const du	twopm75 = 
{D(0x3b400000, 0x00000000)};

static const du	twop64 = 
{D(0x43f00000, 0x00000000)};

static const	fu	Qnan = {QNANF};

static const fu Neginf_s = {0x7f800000};

static const fu Inf_s = {0x7f800000};

static const fu	twop23 = {0x4b000000};

static const fu	twop24 = {0x4b800000};

static const fu	Magic = {0x4b400000};


/* ====================================================================
 *
 * FunctionName		powf
 *
 * Description		computes power function of args
 *
 * ====================================================================
 */

float
__powf( float x, float y )
{
int	iy, ix, xpty, xptx;
int	n;
int	i, j;
int	signx;
float	sign;
float	u;
float	fresult;
double	w;
double	dx, dy;
double	z;
double	result;
#ifdef _CALL_MATHERR
struct exception	exstruct;
#endif


	/* computes x**y as exp(y*log(x)), using double precision arithmetic */

	sign = 1.0f;

	FLT2INT(x, ix);		/* copy first arg to an int	*/
	n = (ix >> MANTWIDTH);
	signx = ((n >> EXPWIDTH) & 1);
	xptx = (n & 0xff);

	FLT2INT(y, iy);		/* copy second arg to an int     */
	xpty = (iy >> MANTWIDTH);
	xpty &= 0xff;

	dx = x;
	dy = y;

	/* screen args for zeroes, denormals, infinities, nans, for x < 0,
	   and for y == +/-1.0
	 */

	i = (dx == 0.0) | (xptx == 0xff) | (ix <= 0);
	j = (dy == 0.0) | (xpty == 0xff) | (fabs(dy) == 1.0);

	if ( i + j != 0 )
		goto special;

L:
	if ( dx == 1.0 )
		goto xeqone;

	if ( dx == 2.0 )
		goto xeqtwo;

	if ( dy == 2.0 )
		goto yeqtwo;

	if ( fabs(dy) > ylimit.d )
		goto overunder;

	w = __log( dx );

	z = dy*w;

	if ( fabs(z) < twopm25.d )
	{
		return ( sign*1.0f );
	}

	/* check for underflow/overflow of result */

	if ( z < Llimit.d )
	{
		/* result underflows */

#ifdef _CALL_MATHERR

                exstruct.type = UNDERFLOW;
                exstruct.name = "powf";
                exstruct.arg1 = x;
                exstruct.arg2 = y;
                exstruct.retval = sign*0.0f;

                if ( matherr( &exstruct ) == 0 )
                {
                        fprintf(stderr, "underflow range error in powf\n");
                        SETERRNO(ERANGE);
                }

                return ( exstruct.retval );
#else

		SETERRNO(ERANGE);

		return ( sign*0.0f );
#endif
	}

	if ( z > Ulimit.d )
	{
		/* result overflows */

#ifdef _CALL_MATHERR

                exstruct.type = OVERFLOW;
                exstruct.name = "powf";
                exstruct.arg1 = x;
                exstruct.arg2 = y;
                exstruct.retval = (sign > 0.0f) ? Inf_s.f : Neginf_s.f;

                if ( matherr( &exstruct ) == 0 )
                {
                        fprintf(stderr, "overflow range error in powf\n");
                        SETERRNO(ERANGE);
                }

                return ( exstruct.retval );
#else

		SETERRNO(ERANGE);

                return ( (sign > 0.0f) ? Inf_s.f : Neginf_s.f );
#endif
	}

	result = sign*__exp(z);

	return ( (float)result );

special:
	if ( dy == 0.0 )
		return ( 1.0f );

	if ( dy == 1.0 )
		return ( x );

	if ( (y != y) || (x != x) )
	{
		/* y or x is a NaN; return a quiet NaN */

#ifdef _CALL_MATHERR

                exstruct.type = DOMAIN;
                exstruct.name = "powf";
                exstruct.arg1 = x;
                exstruct.arg2 = y;
                exstruct.retval = Qnan.f;

                if ( matherr( &exstruct ) == 0 )
                {
                        fprintf(stderr, "domain error in powf\n");
                        SETERRNO(EDOM);
                }

                return ( exstruct.retval );
#else
		NAN_SETERRNO(EDOM);

		return ( Qnan.f );
#endif
	}

	if ( (dy == Inf.d) && (dx != 0.0) )
	{
		if ( fabs(dx) > 1.0 )
		{
			/* indicate overflow */

#ifdef _CALL_MATHERR

                        exstruct.type = OVERFLOW;
                        exstruct.name = "powf";
                        exstruct.arg1 = x;
                        exstruct.arg2 = y;
                        exstruct.retval = Inf_s.f;

                        if ( matherr( &exstruct ) == 0 )
                        {
                                fprintf(stderr, "overflow range error in powf\n");
                                SETERRNO(ERANGE);
                        }

                        return ( exstruct.retval );
#else

			SETERRNO(ERANGE);

			return ( Inf_s.f );
#endif
		}

		if ( fabs(dx) < 1.0 )
		{
			/* indicate underflow */

#ifdef _CALL_MATHERR

			exstruct.type = UNDERFLOW;
			exstruct.name = "powf";
			exstruct.arg1 = x;
			exstruct.arg2 = y;
			exstruct.retval = 0.0f;

			if ( matherr( &exstruct ) == 0 )
			{
				fprintf(stderr, "underflow range error in powf\n");
				SETERRNO(ERANGE);
			}

			return ( exstruct.retval );
#else

			SETERRNO(ERANGE);

			return ( 0.0f );
#endif
		}
	}

	if ( (dy == Neginf.d) && (dx != 0.0) )
	{
		if ( fabs(dx) < 1.0 )
		{
			/* indicate overflow */

#ifdef _CALL_MATHERR

                        exstruct.type = OVERFLOW;
                        exstruct.name = "powf";
                        exstruct.arg1 = x;
                        exstruct.arg2 = y;
                        exstruct.retval = Inf_s.f;

                        if ( matherr( &exstruct ) == 0 )
                        {
                                fprintf(stderr, "overflow range error in powf\n");
                                SETERRNO(ERANGE);
                        }

                        return ( exstruct.retval );
#else

			SETERRNO(ERANGE);

			return ( Inf_s.f );
#endif
		}

		if ( fabs(dx) > 1.0 )
		{
			/* indicate underflow */

#ifdef _CALL_MATHERR

                        exstruct.type = UNDERFLOW;
                        exstruct.name = "powf";
                        exstruct.arg1 = x;
                        exstruct.arg2 = y;
                        exstruct.retval = 0.0f;

                        if ( matherr( &exstruct ) == 0 )
                        {
                                fprintf(stderr, "underflow range error in powf\n");
                                SETERRNO(ERANGE);
                        }

                        return ( exstruct.retval );
#else

			SETERRNO(ERANGE);

			return ( 0.0f );
#endif
		}
	}

	if ( (dx == 1.0) && (fabs(dy) == Inf.d) )
		return ( 1.0f );

	if ( (dx == -1.0) && (fabs(dy) == Inf.d) )
	{
		/* indicate domain error */

#ifdef _CALL_MATHERR

		exstruct.type = DOMAIN;
		exstruct.name = "powf";
		exstruct.arg1 = x;
		exstruct.arg2 = y;
		exstruct.retval = Qnan.f;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "domain error in powf\n");
			SETERRNO(ERANGE);
		}

		return ( exstruct.retval );
#else

		SETERRNO(ERANGE);

		return ( Qnan.f );
#endif
	}

	if ( (dx == 0.0) && (signx == 0) )
	{
		if ( dy > 0.0 )
			return ( 0.0f );

		if ( dy < 0.0 )
		{
			/* indicate overflow */

#ifdef _CALL_MATHERR

                        exstruct.type = OVERFLOW;
                        exstruct.name = "powf";
                        exstruct.arg1 = x;
                        exstruct.arg2 = y;
                        exstruct.retval = Inf_s.f;

                        if ( matherr( &exstruct ) == 0 )
                        {
                                fprintf(stderr, "overflow range error in powf\n");
                                SETERRNO(ERANGE);
                        }

                        return ( exstruct.retval );
#else

			SETERRNO(ERANGE);

			return ( Inf_s.f );
#endif
		}
	}

	if ( dy == -1.0 )
	{
		if ( fabs(dx) > twopm128.d )
		{
			return ( 1/x );
		}

		/* result overflows */

#ifdef _CALL_MATHERR

                exstruct.type = OVERFLOW;
                exstruct.name = "powf";
                exstruct.arg1 = x;
                exstruct.arg2 = y;
                exstruct.retval = (dx > 0.0) ? Inf_s.f : Neginf_s.f;

                if ( matherr( &exstruct ) == 0 )
                {
                        fprintf(stderr, "overflow range error in powf\n");
                        SETERRNO(ERANGE);
                }

                return ( exstruct.retval );
#else

		SETERRNO(ERANGE);

		if ( dx > 0.0 )
			return ( Inf_s.f );
		else
			return ( Neginf_s.f );
#endif
	}

	if ( (x == 0.0) && (signx != 0) )
	{
		/* test for y an odd integer */

		if ( fabsf(y) >= twop24.f )
		{
			goto next;
		}
		else if ( fabsf(y) >= twop23.f )
		{
			/* y is an integer */

			n = y;
		}
		else
		{
			u = y + Magic.f;

			u = u - Magic.f;

			if ( u != y )
			{
				/* y is not an integer */

				goto next;
			}

			n = y;

		}

		if ( (n&1) == 1 )
		{
			if ( dy > 0.0 )
				return ( -0.0f );
			else
			{
				/* indicate overflow */

#ifdef _CALL_MATHERR

	                        exstruct.type = OVERFLOW;
	                        exstruct.name = "powf";
	                        exstruct.arg1 = x;
	                        exstruct.arg2 = y;
	                        exstruct.retval = Neginf_s.f;

	                        if ( matherr( &exstruct ) == 0 )
	                        {
	                                fprintf(stderr, "overflow range error in powf\n");
	                                SETERRNO(ERANGE);
	                        }

                        	return ( exstruct.retval );
#else

				SETERRNO(ERANGE);

				return ( Neginf_s.f );
#endif
			}

		}

next:
		if ( dy > 0.0 )
			return ( 0.0f );

		if ( dy < 0.0 )
		{
			/* indicate overflow */

#ifdef _CALL_MATHERR

                        exstruct.type = OVERFLOW;
                        exstruct.name = "powf";
                        exstruct.arg1 = x;
                        exstruct.arg2 = y;
                        exstruct.retval = Inf_s.f;

                        if ( matherr( &exstruct ) == 0 )
                        {
                                fprintf(stderr, "overflow range error in powf\n");
                                SETERRNO(ERANGE);
                        }

                        return ( exstruct.retval );
#else

			SETERRNO(ERANGE);

			return ( Inf_s.f );
#endif
		}
	}

	if ( dx == Inf.d )
	{
		if ( dy > 0.0 )
			return ( Inf_s.f );

		if ( dy < 0.0 )
			return ( 0.0f );
	}

	if ( dx == Neginf.d )
	{
		/* test for y an odd integer */

		if ( fabsf(y) >= twop24.f )
		{
			goto next_2;
		}
		else if ( fabsf(y) >= twop23.f )
		{
			/* y is an integer */

			n = y;
		}
		else
		{
			u = y + Magic.f;

			u = u - Magic.f;

			if ( u != y )
			{
				/* y is not an integer */

				goto next_2;
			}

			n = y;
		}

		if ( (n&1) == 1 )
		{
			if ( dy > 0.0 )
				return ( Neginf_s.f );
			else
				return ( -0.0f );

		}

next_2:
		if ( dy > 0.0 )
			return ( Inf_s.f );

		if ( dy < 0.0 )
			return ( 0.0f );
	}

	if ( dx < 0.0 )
	{
		/* check if y is an integer */
	
		if ( fabsf(y) >= twop24.f )
		{
			sign = 1.0f;
		}
		else if ( fabsf(y) >= twop23.f )
		{
			/* y is an integer; rightmost bit determines parity */

			n = y;

			if ( n%2 == 0 )
			{
				sign = 1.0f;
			}
			else
			{
				sign = -1.0f;
			}
		}
		else
		{
			/* determine nearest integer to y */

			u = y + Magic.f;
			u = u - Magic.f;
			n = u;
	
			if ( u != y )
			{
				/* y is not an integer */
	
#ifdef _CALL_MATHERR

                                exstruct.type = DOMAIN;
                                exstruct.name = "powf";
                                exstruct.arg1 = x;
                                exstruct.arg2 = y;
                                exstruct.retval = Qnan.f;

                                if ( matherr( &exstruct ) == 0 )
                                {
                                        fprintf(stderr, "domain error in powf\n");
                                        SETERRNO(EDOM);
                                }

                                return ( exstruct.retval );
#else

				SETERRNO(EDOM);
		
				return ( Qnan.f );
#endif
			}
	
			if ( n%2 == 0 )
			{
				sign = 1.0f;
			}
			else
			{
				sign = -1.0f;
			}
		}

		dx = fabs(dx);
	}

	goto L;

xeqone:
	return ( sign );

xeqtwo:
	w = log2.d;

	if ( fabs(dy) < twopm25.d )
	{
		return ( sign*1.0f );
	}

	/* check for underflow/overflow of result */

	if ( dy <= -150.0 )
	{
		/* result underflows */

#ifdef _CALL_MATHERR

                exstruct.type = UNDERFLOW;
                exstruct.name = "powf";
                exstruct.arg1 = x;
                exstruct.arg2 = y;
                exstruct.retval = sign*0.0f;

                if ( matherr( &exstruct ) == 0 )
                {
                        fprintf(stderr, "underflow range error in powf\n");
                        SETERRNO(ERANGE);
                }

                return ( exstruct.retval );
#else

		SETERRNO(ERANGE);

		return ( sign*0.0f );
#endif
	}

	if ( dy >= 1024.0 )
	{
		/* result overflows */

#ifdef _CALL_MATHERR

                exstruct.type = OVERFLOW;
                exstruct.name = "powf";
                exstruct.arg1 = x;
                exstruct.arg2 = y;
                exstruct.retval = (sign > 0.0f) ? Inf_s.f : Neginf_s.f;

                if ( matherr( &exstruct ) == 0 )
                {
                        fprintf(stderr, "overflow range error in powf\n");
                        SETERRNO(ERANGE);
                }

                return ( exstruct.retval );
#else

		SETERRNO(ERANGE);

                return ( (sign > 0.0f) ? Inf_s.f : Neginf_s.f );
#endif
	}

	z = dy*w;

	fresult = sign*__exp(z);

	if ( fresult == 0.0f )
	{
		/* result underflows */

#ifdef _CALL_MATHERR

                exstruct.type = UNDERFLOW;
                exstruct.name = "powf";
                exstruct.arg1 = x;
                exstruct.arg2 = y;
                exstruct.retval = fresult;

                if ( matherr( &exstruct ) == 0 )
                {
                        fprintf(stderr, "underflow range error in powf\n");
                        SETERRNO(ERANGE);
                }

                return ( exstruct.retval );
#else

		SETERRNO(ERANGE);

		return ( fresult );
#endif
	}

	return ( fresult );

yeqtwo:
	if ( fabs(dx) >= twop64.d )
	{
		/* result overflows */

#ifdef _CALL_MATHERR

                exstruct.type = OVERFLOW;
                exstruct.name = "powf";
                exstruct.arg1 = x;
                exstruct.arg2 = y;
                exstruct.retval = Inf_s.f;

                if ( matherr( &exstruct ) == 0 )
                {
                        fprintf(stderr, "overflow range error in powf\n");
                        SETERRNO(ERANGE);
                }

                return ( exstruct.retval );
#else

		SETERRNO(ERANGE);

		return ( Inf_s.f );
#endif
	}

	if ( fabs(dx) < twopm75.d )
	{
		/* result underflows */

#ifdef _CALL_MATHERR

                exstruct.type = UNDERFLOW;
                exstruct.name = "powf";
                exstruct.arg1 = x;
                exstruct.arg2 = y;
                exstruct.retval = 0.0f;

                if ( matherr( &exstruct ) == 0 )
                {
                        fprintf(stderr, "underflow range error in powf\n");
                        SETERRNO(ERANGE);
                }

                return ( exstruct.retval );
#else

		SETERRNO(ERANGE);

		return ( 0.0f );
#endif
	}

	return ( x*x );

overunder:

	/* result underflows or overflows depending on sign of y
	   and magnitude and sign of x
	*/

	if ( ((dx > 1.0) && (dy > 0.0)) ||
	     ((dx < 1.0) && (dy < 0.0))
	   )
	{
		/* result overflows */

#ifdef _CALL_MATHERR

                exstruct.type = OVERFLOW;
                exstruct.name = "powf";
                exstruct.arg1 = x;
                exstruct.arg2 = y;
                exstruct.retval = (sign > 0.0f) ? Inf_s.f : Neginf_s.f;
                if ( matherr( &exstruct ) == 0 )
                {
                        fprintf(stderr, "overflow error in powf\n");
                        SETERRNO(ERANGE);
                }

                return ( exstruct.retval );
#else

		SETERRNO(ERANGE);

		return ( (sign > 0.0f) ? Inf_s.f : Neginf_s.f );
#endif
	}

	/* result underflows */

#ifdef _CALL_MATHERR

        exstruct.type = UNDERFLOW;
        exstruct.name = "powf";
        exstruct.arg1 = x;
        exstruct.arg2 = y;
        exstruct.retval = sign*0.0f;

        if ( matherr( &exstruct ) == 0 )
        {
                fprintf(stderr, "underflow error in powf\n");
                SETERRNO(ERANGE);
        }

        return ( exstruct.retval );
#else

	SETERRNO(ERANGE);

	return ( sign*0.0f );
#endif

}

