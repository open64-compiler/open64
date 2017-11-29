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
 * Module: jn.c
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:58:22-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/libm/mips/SCCS/s.jn.c $
 *
 * Revision history:
 *  28-May-93 - Original Version
 *
 * Description:	source code for bessel functions of first and second kind
 *		of order n
 *
 * ====================================================================
 * ====================================================================
 */

/* $Header: /home/bos/bk/kpro64-pending/libm/mips/jn.c 1.5 04/12/21 14:58:22-08:00 bos@eng-25.internal.keyresearch.com $ */

/****************************  IMPORTANT NOTE  ****************************
 *
 *  - This file contains library routines which are "safe" for parallel
 *    processing.  This fact is indicated here and in 'math.h' by inclusion
 *    of the following directive(s):
 *
 *        #pragma no side effects (jn)
 *        #pragma no side effects (yn)
 *
 *  - To be considered safe a routine cannot do any I/O (including GL calls),
 *    modify any global location ('errno' for example) or have global state
 *    (as 'random' does).  Nor can it call any other routine which does any
 *    of these things.
 *
 *  - If in the future any of these routines are modified so that they are
 *    no longer "safe", then the appropriate 'pragma no side effects' must
 *    be manually removed from 'math.h' and this file.
 *
 *  - Note that this pragma only has meaning to Power C ('pca') and is
 *    properly passed on by 'cpp' & 'c++' and ignored by 'ccom'.
 *
 ******************************  END OF NOTE  ******************************/

/*
	floating point Bessel's function of
	the first and second kinds and of
	integer order.

	int n;
	double x;
	jn(n,x);

	returns the value of Jn(x) for all
	integer values of n and all real values
	of x.

	There are no error returns.
	Calls j0, j1.

	For n=0, j0(x) is called,
	for n=1, j1(x) is called,
	for n<x, forward recursion us used starting
	from values of j0(x) and j1(x).
	for n>x, a continued fraction approximation to
	j(n,x)/j(n-1,x) is evaluated and then backward
	recursion is used starting from a supposed value
	for j(n,x). The resulting value of j(0,x) is
	compared with the actual value to correct the
	supposed value of j(n,x).

	yn(n,x) is similar in all respects, except
	that forward recursion is used for all
	values of n>1.
*/

#ifdef _CALL_MATHERR
#include <stdio.h>
#include <math.h>
#endif

#include "libm.h"

#if defined(mips) && !defined(__GNUC__)
extern	double	jn(int, double);
extern	double	yn(int, double);

#pragma weak jn = __jn
#pragma weak yn = __yn
#endif

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern double __jn(int, double);
extern double __yn(int, double);
#pragma jn
#pragma yn
double jn( int n, double x ) {
  return __jn( n, x );
}
double yn( int n, double x ) {
  return __yn( n, x );
}
#elif defined(__GNUC__)
extern  double  __jn(int, double);

double    jn() __attribute__ ((weak, alias ("__jn")));

extern  double  __yn(int, double);

double    yn() __attribute__ ((weak, alias ("__yn")));

#endif

extern	double	__j0(double);
extern	double	__j1(double);
extern	double	__y0(double);
extern	double	__y1(double);

static const	du	Twop49 =
{D(0x43000000, 0x00000000)};

static const	du	Qnan =
{D(QNANHI, QNANLO)};

static const	du	Neginf =
{D(0xfff00000, 0x00000000)};

double
__jn( int n, double x )
{
int i;
double a, b, temp;
double	z;
double zsq, t;
double result;
#ifdef _CALL_MATHERR
struct exception	exstruct;
#endif

	if( x != x )
	{
		/* arg is NaN */

#ifdef _CALL_MATHERR

		exstruct.type = DOMAIN;
		exstruct.name = "jn";
		exstruct.arg1 = n;
		exstruct.arg2 = x;
		exstruct.retval = Qnan.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "domain error in jn\n");
			SETERRNO(EDOM);
		}

		return ( exstruct.retval );
#else
		NAN_SETERRNO(EDOM);

		return ( Qnan.d );
#endif
	}

        if ( fabs(x) >= Twop49.d )
	{
#ifdef _CALL_MATHERR

		exstruct.type = TLOSS;
		exstruct.name = "jn";
		exstruct.arg1 = n;
		exstruct.arg2 = x;
		exstruct.retval = 0.0;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "total loss of significance \
error in jn\n");
			SETERRNO(ERANGE);
		}

		return ( exstruct.retval );
#else
		SETERRNO(ERANGE);

		return ( 0.0 );
#endif
	}

	if ( n < 0 )
	{
		n = -n;
		x = -x;
	}

	if ( n == 0 ) return(__j0(x));
	if ( n == 1 ) return(__j1(x));
	if ( x  ==  0. ) return(0.);

	z = fabs(x);

	if ( n > z ) goto recurs;

	a = __j0(z);
	b = __j1(z);

	for(i=1;i<n;i++)
	{
		temp = b;
		b = (2.*i/z)*b - a;
		a = temp;
	}

	result = b;

	if ( (x < 0.0) && (n%2) )
		result = -result;

	return ( result );

recurs:
	zsq = z*z;

	for (t=0, i=n+16; i>n; i--)
	{
		t = zsq/(2.*i - t);
	}

	t = z/(2.*n-t);

	a = t;
	b = 1;

	for (i=n-1; i>0; i--)
	{
		temp = b;
		b = (2.*i/z)*b - a;
		a = temp;
	}

	result = t*__j0(z)/b;

	if ( (x < 0.0) && (n%2) )
		result = -result;

	return ( result );
}

double
__yn( int n, double x )
{
int i;
int sign;
double a, b, temp;
#ifdef _CALL_MATHERR
struct exception	exstruct;
#endif

	if( x != x )
	{
		/* arg is NaN */

#ifdef _CALL_MATHERR

		exstruct.type = DOMAIN;
		exstruct.name = "yn";
		exstruct.arg1 = n;
		exstruct.arg2 = x;
		exstruct.retval = Qnan.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "domain error in yn\n");
			SETERRNO(EDOM);
		}

		return ( exstruct.retval );
#else
		NAN_SETERRNO(EDOM);

		return ( Qnan.d );
#endif
	}

	if ( x == 0.0 )
	{
#ifdef _CALL_MATHERR

		exstruct.type = OVERFLOW;
		exstruct.name = "yn";
		exstruct.arg1 = n;
		exstruct.arg2 = x;
		exstruct.retval = Neginf.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "overflow range error in yn\n");
			SETERRNO(ERANGE);
		}

		return ( exstruct.retval );
#else
		SETERRNO(ERANGE);

		return ( Neginf.d );
#endif
	}

	if ( x < 0.0 )
	{
#ifdef _CALL_MATHERR

		exstruct.type = DOMAIN;
		exstruct.name = "yn";
		exstruct.arg1 = n;
		exstruct.arg2 = x;
		exstruct.retval = Qnan.d;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "domain error in yn\n");
			SETERRNO(EDOM);
		}

		return ( exstruct.retval );
#else
		SETERRNO(EDOM);

		return ( Qnan.d );
#endif
	}

	if ( x >= Twop49.d )
	{
#ifdef _CALL_MATHERR

		exstruct.type = TLOSS;
		exstruct.name = "yn";
		exstruct.arg1 = n;
		exstruct.arg2 = x;
		exstruct.retval = 0.0;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "total loss of significance \
error in yn\n");
			SETERRNO(ERANGE);
		}

		return ( exstruct.retval );
#else
		SETERRNO(ERANGE);

		return ( 0.0 );
#endif
	}

	sign = 1;

	if ( n < 0 )
	{
		n = -n;
		if ( (n%2) == 1 )
			sign = -1;
	}

	if ( n == 0) return ( __y0(x) );

	if ( n == 1 ) return ( sign*__y1(x) );

	a = __y0(x);
	b = __y1(x);

	for(i=1;i<n;i++)
	{
		if ( b == Neginf.d )
			break;
		temp = b;
		b = (2.*i/x)*b - a;
		a = temp;
	}

	if ( b == Neginf.d )
	{
#ifdef _CALL_MATHERR

		exstruct.type = OVERFLOW;
		exstruct.name = "yn";
		exstruct.arg1 = n;
		exstruct.arg2 = x;
		exstruct.retval = Neginf.d*sign;

		if ( matherr( &exstruct ) == 0 )
		{
			fprintf(stderr, "overflow range error in yn\n");
			SETERRNO(ERANGE);
		}

		return ( exstruct.retval );
#else
		SETERRNO(ERANGE);

		return ( Neginf.d*sign );
#endif
	}

	return (sign*b);
}

#ifdef NO_LONG_DOUBLE

#if defined(BUILD_OS_DARWIN) /* Mach-O doesn't support aliases */
extern long double __jnl(int, long double);
extern long double __ynl(int, long double);
#pragma weak jnl
#pragma weak ynl
long double jnl( int n, long double x ) {
  return __jnl( n, x );
}
long double ynl( int n, long double x ) {
  return __ynl( n, x );
}
#elif defined(__GNUC__)
extern  long double  __jnl(int, long double);

long double    jnl() __attribute__ ((weak, alias ("__jnl")));

extern  long double  __ynl(int, long double);

long double    ynl() __attribute__ ((weak, alias ("__ynl")));

#endif

long double
__jnl( int n, long double x )
{	
	return ( (long double)__jn(n, (double)x) );
}

long double
__ynl( int n, long double x )
{	
	return ( (long double)__yn(n, (double)x) );
}

#endif

