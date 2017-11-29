/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
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


#pragma ident "@(#) libfi/element/modulo.c	92.2	06/16/99 15:47:23"

#include <fortran.h>
/* keep the order of math.h and fp.h for huge_val */
#include <math.h>
#if !defined(__mips) && !defined(_LITTLE_ENDIAN)
#include <fp.h>
#endif		/* NOT __mips and NOT _LITTLE_ENDIAN */
#include <stddef.h>
#include <cray/portdefs.h>

#if !defined(__mips) && !defined(_LITTLE_ENDIAN)
#ifdef _WORD32
#if _F_REAL16 == 1
#define floorl	__floorl
_f_real16 __floorl(_f_real16 x);
extern long double __floorl();
#endif /* END _REAL16 == 1 */
#endif /* END _WORD32 */
#endif /* END NOT __mips and NOT _LITTLE_ENDIAN */

//Bug# 574
#ifdef KEY
#define __floor floor
#define __floorl floorl
#define __floorf floorf
#endif

/*
 *    _MODULO_X(A,P)  - called by compiled Fortran programs to calculate
 *                    the modulo of type integer or real arguments.
 *
 *          where X is:
 *                I4 for 32-bit integer
 *                I for 46-bit integer
 *                J for 64-bit integer
 *                S4 for 32-bit real
 *                S for 64-bit real
 *                D for 128-bit real
 *
 *              The standard definition for integer is:
 *                 If p != 0, modulo(a,p) = r, where A = q * p + r,
 *                                             q is an integer, 
 *                            0 <= r < p if p > 0
 *                            p < r <= 0 if p < 0
 *                 if p = 0, the result is processor dependent
 *              The standard definition for real is:
 *                 If p != 0, modulo(a,p) = A - FLOOR(a/p) * p
 *                 if p = 0, the result is processor dependent
 *
 *              The algorithm for integer is:
 *                If A and P have the same sign, use:
 *                   (a - INT(a/p) * p),
 *
 *                else when  A and P have different signs,
 *                  if a/p has a zero remainder, use:
 *                    a - INT(a/p)  * p,
 *
 *                  else a/p has a nonzero remainder, use:
 *                    a - (INT(a/p) - 1) * p.
 *
 *              The algorithm for real is:
 *                a - FLOOR(a/p) * p.
 *              For non-IEEE systems, check size of result
 */

#ifdef	_F_INT4
_f_int4
_MODULO_I4 (_f_int4 *arga, _f_int4 *argp)
{
	_f_int4 parg;
	_f_int4 aarg;
	_f_int4 res;
	_f_int4 inter;
	_f_int4 corectn;
	parg	= *argp;
	aarg	= *arga;

/*	prevent divide by zero */
	if (parg == 0)
		return(0);
	inter	= aarg/parg;
	res	= aarg - inter * parg;
	corectn	= (parg > 0) ? ((res >= 0) ? 0 : parg) :
	   ((res <= 0) ? 0 : parg);
	res	= res + corectn;
	return(res);
}
#endif		/* _F_INT4 */


#ifdef _F_INT6
_f_int6
_MODULO_I (_f_int6 *arga, _f_int6 *argp)
{
	_f_int6 parg;
	_f_int6 aarg;
	_f_int6 res;
	_f_int6 inter;
	_f_int6 corectn;
	parg	= *argp;
	aarg	= *arga;

/*	prevent divide by zero */
	if (parg == 0)
		return(0);
	inter	= aarg/parg;
	res	= aarg - inter * parg;
	corectn	= (parg > 0) ? ((res >= 0) ? 0 : parg) :
	   ((res <= 0) ? 0 : parg);
	res	= res + corectn;
	return(res);
}
#endif		/* _F_INT6 */


#ifdef	_F_INT8
_f_int8
_MODULO_J (_f_int8 *arga, _f_int8 *argp)
{
	_f_int8 parg;
	_f_int8 aarg;
	_f_int8 res;
	_f_int8 inter;
	_f_int8 corectn;
	parg	= *argp;
	aarg	= *arga;

/*	prevent divide by zero */
	if (parg == 0)
		return(0);
	inter	= aarg/parg;
	res	= aarg - inter * parg;
	corectn	= (parg > 0) ? ((res >= 0) ? 0 : parg) :
	   ((res <= 0) ? 0 : parg);
	res	= res + corectn;
	return(res);
}
#endif		/* _F_INT8 */


#ifdef	_F_REAL4
_f_real4
#if defined(__mips) || defined(_LITTLE_ENDIAN)
_modulo4 (_f_real4 arga, _f_real4 argp)
{
#if defined(__mips)
	return (arga - (floorf(arga/argp)) * argp);
#else		/* ELSE of __mips is LITTLE_ENDIAN. */
	_f_real4 __floorf(_f_real4 arg);
	return (arga - (__floorf(arga/argp)) * argp);
#endif		/* ENDIF of __mips */
}
#else		/* __mips or _LITTLE_ENDIAN */
_MODULO_S4 (_f_real4 *arga, _f_real4 *argp)
{
	 _f_real4 aarg;
	 _f_real4 parg;
	 _f_real4 res;
	parg	= *argp;
	aarg	= *arga;
	if (parg == 0)
#ifdef	IEEE_FLOATING_POINT
		return(_HALF_NaN);
#else
		return(0);
#endif
	res	= aarg - (floor((_f_real8)(aarg/parg)) * parg);
#ifndef	IEEE_FLOATING_POINT
	if (fabs((_f_real8)res) >= fabs((_f_real8)parg))
		res -= parg;
#endif
	return(res);
}
#endif		/* __mips or _LITTLE_ENDIAN */
#endif		/* _F_REAL4 */


#ifdef	_F_REAL8
_f_real8
#if defined(__mips) || defined(_LITTLE_ENDIAN)
_modulo8 (_f_real8 arga, _f_real8 argp)
{
#if defined(__mips)
	return (arga - (floor(arga/argp)) * argp);
#else		/* ELSE of __mips is LITTLE_ENDIAN. */
	return (arga - (__floor(arga/argp)) * argp);
#endif		/* ENDIF of __mips */
}
#else		/* __mips or _LITTLE_ENDIAN */
_MODULO_S (_f_real8 *arga, _f_real8 *argp)
{
	 _f_real8 aarg;
	 _f_real8 parg;
	 _f_real8 res;
	parg	= *argp;
	aarg	= *arga;
	if (parg == 0)
#ifdef	IEEE_FLOATING_POINT
		return(_SGL_NaN);
#else
		return((_f_real8) 0);
#endif
	res	= aarg - (floor((_f_real8)(aarg/parg)) * parg);
#ifndef	IEEE_FLOATING_POINT
	if (fabs((_f_real8)res) >= fabs((_f_real8)parg))
		res -= parg;
#endif
	return(res);
}
#endif		/* __mips or _LITTLE_ENDIAN */
#endif		/* _F_REAL8 */


#if _F_REAL16 == 1
_f_real16
#if defined(__mips) || defined(_LITTLE_ENDIAN)
_moduloq (_f_real16 arga, _f_real16 argp)
{
#if defined(__mips)
	return (arga - (floorl(arga/argp)) * argp);
#else		/* ELSE of __mips is LITTLE_ENDIAN. */
	_f_real16 __floorl(_f_real16 arg);
	return (arga - (__floorl(arga/argp)) * argp);
#endif		/* ENDIF of __mips */
}
#else		/* __mips or _LITTLE_ENDIAN */
_MODULO_D (_f_real16 *arga, _f_real16 *argp)
{
	 _f_real16 aarg;
	 _f_real16 parg;
	 _f_real16 res;
	parg	= *argp;
	aarg	= *arga;
	if (parg == 0)
#ifdef	IEEE_FLOATING_POINT
		return(_DBL_NaN);
#else
		return((_f_real16) 0);
#endif     /* IEEE_FLOATING_POINT */

	res	= aarg - (floorl((_f_real16)(aarg/parg)) * parg);

#ifndef	IEEE_FLOATING_POINT
	if (fabsl((_f_real16)res) >= fabsl((_f_real16)parg))
		res -= parg;
#endif     /* not IEEE_FLOATING_POINT */
	return(res);
}
#endif		/* __mips or _LITTLE_ENDIAN */
#endif		/* end of 128-bit float modulo */
