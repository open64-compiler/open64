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


#pragma ident "@(#) libfi/element/selrealkind.c	92.1	06/16/99 15:47:23"
#include <fortran.h>
#include <stddef.h>
#include <fmath.h>

static struct {
        int kind;
        int maxprecision;
        int maxexprange;
} values[] = {
        { 4,    PRECISION_REAL4_F90,            RANGE_REAL4_F90         },
        { 8,    PRECISION_REAL8_F90,            RANGE_REAL8_F90         },
#ifndef KEY
#if _F_REAL16 == 1
        { 16,   PRECISION_REAL16_F90,           RANGE_REAL16_F90        },
#endif	/* _F_REAL16 */
#endif
};


#define NUMKINDS        (signed)(sizeof(values)/sizeof(values[0]))

#define _CODE_SEL()							\
	if (prec != NULL)						\
	   precval	= *prec;					\
	if (range != NULL)						\
	   exprangval 	= *range;					\
	for (i=0; i<NUMKINDS; i++) {					\
	   if (exp_match == 0 && exprangval <= values[i].maxexprange) {	\
		exp_match = values[i].kind;				\
	   }								\
	   if (prec_match == 0 && precval <= values[i].maxprecision) {	\
		prec_match = values[i].kind;				\
	   }								\
	   if (exp_match && prec_match)					\
		break;							\
	}								\
	if (exp_match == 0 && prec_match == 0)				\
	   return(-3);							\
	else if (prec_match == 0)					\
	   return(-1);							\
	else if (exp_match == 0)					\
	   return(-2);							\
	return ( (exp_match > prec_match) ? exp_match : prec_match);

#define _CODE_SEL_MIPS()						\
	if (prec != NULL)						\
	   precval	= *prec;					\
	if (range != NULL)						\
	   exprangval 	= *range;					\
        for (i=0; i<NUMKINDS; i++) {					\
           if (exp_match == 0 && exprangval <= values[i].maxexprange) {	\
                        exp_match = values[i].kind;			\
			e_indx = i + 1;					\
	   }								\
	   if (prec_match == 0 && precval <= values[i].maxprecision) {	\
		prec_match = values[i].kind;				\
			p_indx = i + 1;					\
	   }								\
	   if (exp_match && prec_match)					\
		break;							\
	}								\
	if (exp_match == 0 && prec_match == 0)				\
	   return(-3);							\
	else if (prec_match == 0)					\
	   return(-1);							\
	else if (exp_match == 0)					\
	   return(-2);							\
	/* Check for a precision of kind=16 and an exponent of kind=8.	\
	 * The exponent range for kind=8 is larger than the exponent	\
	 * range for kind=16 on mips only.  The result of intrinsic	\
	 * selected_real_kind for this combination is being evaluated	\
	 * by the Fortran standards committee and the values returned	\
	 * here may change.						\
	 */								\
	if (exp_match > prec_match)					\
		result = e_indx - 1;					\
	else								\
		result = p_indx - 1;					\
	if (values[result].maxexprange < exprangval ) return (-2);	\
        if (values[result].maxprecision < precval  ) return (-1);	\
        return (values[result].kind);

/*
 *    SELECTED_REAL_KIND(P,R)
 *                Returns a value of the kind type parameter of a
 *                real data type with decimal precision of at least P and
 *                a decimal exponent range of at least R.
 *                P and R may be negative values according to F90
 *                interpretation 201.
 *       NONSUCCESSFUL RETURN VALUES:
 *                 -3 indicates that neither P nor R is available.
 *                 -2 indicates that R is not available.
 *                 -1 indicates that P is not available.
 *       SUCCESSFUL RETURN VALUES:
 *             Return the KIND type parameter for P and R.  If more than
 *             one KIND type parameter fits P and R, return the smallest
 *             of these kind values.
 */

#ifdef _UNICOS
/* Duplicate name to build the F90 compiler version 1.1 and previous */
#pragma _CRI duplicate _SELECTED_REAL_KIND_ as _SELECTED_REAL_KIND 
#endif	/* UNICOS */

_f_int
_SELECTED_REAL_KIND_ (_f_int *prec,
		      _f_int *range)
{
#if	defined(__mips)
	int e_indx	= 0;	/* found exponent range match */
	int p_indx	= 0;	/* found precision match */
	int result;
#endif	/* mips */
	int i;
	int exprangval	= 0;	/* exponent range argument */
	int precval	= 0;	/* precision value argument */
	int exp_match 	= 0;	/* found exponent range match */
	int prec_match 	= 0;	/* found precision match */
#if	defined(__mips)
	_CODE_SEL_MIPS();
#else	/* mips */
	_CODE_SEL();
#endif	/* mips */
}

#ifndef	_UNICOS
/* Duplicate name to build the Sparc F90 compiler prerelease version */
_f_int _SELECTED_REAL_KIND(_f_int *prec, _f_int *range) 
{
#if	defined(__mips)
	int e_indx	= 0;	/* found exponent range match */
	int p_indx	= 0;	/* found precision match */
	int result;
#endif	/* mips */
	int i;
	int exprangval	= 0;	/* exponent range argument */
	int precval	= 0;	/* precision value argument */
	int exp_match 	= 0;	/* found exponent range match */
	int prec_match 	= 0;	/* found precision match */
#if	defined(__mips)
	_CODE_SEL_MIPS();
#else	/* mips */
	_CODE_SEL();
#endif	/* mips */
}
#endif	/* not UNICOS */

#ifdef	_F_INT4
/*
 *    SELECTED_REAL_KIND_4(P,R)
 *                Returns a value of the kind type parameter of a
 *                real data type with decimal precision of at least P and
 *                a decimal exponent range of at least R.
 *                P and R may be negative values according to F90
 *                interpretation 201.
 *          The compiler casts the arguments to default integer.
 *       NONSUCCESSFUL RETURN VALUES:
 *                 -3 indicates that neither P nor R is available.
 *                 -2 indicates that R is not available.
 *                 -1 indicates that P is not available.
 *       SUCCESSFUL RETURN VALUES:
 *             Return the KIND type parameter for P and R.  If more than
 *             one KIND type parameter fits P and R, return the smallest
 *             of these kind values.
 */

#ifdef _UNICOS
/* Duplicate name to build the F90 compiler version 1.1 and previous */
#pragma _CRI duplicate _SELECTED_REAL_KIND_4_ as _SELECTED_REAL_KIND_4 
#endif	/* UNICOS */

_f_int4
_SELECTED_REAL_KIND_4_ (_f_int *prec,
		      _f_int *range)
{
#if	defined(__mips)
	int e_indx		= 0;	/* found exponent range match */
	int p_indx		= 0;	/* found precision match */
	int result;
#endif	/* mips */
	int i;
	int exprangval		= 0;	/* exponent range argument */
	int precval		= 0;	/* precision value argument */
	_f_int4 exp_match 	= 0;	/* found exponent range match */
	_f_int4 prec_match	= 0;	/* found precision match */
#if	defined(__mips)
	_CODE_SEL_MIPS();
#else	/* mips */
	_CODE_SEL();
#endif	/* mips */
}

#ifndef	_UNICOS
/* Duplicate name to build the Sparc F90 compiler prerelease version */
_f_int4 _SELECTED_REAL_KIND_4(_f_int *prec, _f_int *range) 
{
#if	defined(__mips)
	int e_indx		= 0;	/* found exponent range match */
	int p_indx		= 0;	/* found precision match */
	int result;
#endif	/* mips */
	int i;
	int exprangval		= 0;	/* exponent range argument */
	int precval		= 0;	/* precision value argument */
	_f_int4 exp_match 	= 0;	/* found exponent range match */
	_f_int4 prec_match	= 0;	/* found precision match */
#if	defined(__mips)
	_CODE_SEL_MIPS();
#else	/* mips */
	_CODE_SEL();
#endif	/* mips */
}
#endif	/* not UNICOS */

#endif	/* _F_INT4 */


#ifdef	_F_INT8
/*
 *    SELECTED_REAL_KIND_8(P,R)
 *                Returns a value of the kind type parameter of a
 *                real data type with decimal precision of at least P and
 *                a decimal exponent range of at least R.
 *                P and R may be negative values according to F90
 *                interpretation 201.
 *          The compiler casts the arguments to default integer.
 *       NONSUCCESSFUL RETURN VALUES:
 *                 -3 indicates that neither P nor R is available.
 *                 -2 indicates that R is not available.
 *                 -1 indicates that P is not available.
 *       SUCCESSFUL RETURN VALUES:
 *             Return the KIND type parameter for P and R.  If more than
 *             one KIND type parameter fits P and R, return the smallest
 *             of these kind values.
 */

#ifdef _UNICOS
/* Duplicate name to build the F90 compiler version 1.1 and previous */
#pragma _CRI duplicate _SELECTED_REAL_KIND_8_ as _SELECTED_REAL_KIND_8 
#endif	/* UNICOS */

_f_int8
_SELECTED_REAL_KIND_8_ (_f_int *prec,
		      _f_int *range)
{
#if	defined(__mips)
	int e_indx		= 0;	/* found exponent range match */
	int p_indx		= 0;	/* found precision match */
	int result;
#endif	/* mips */
	int i;
	int exprangval		= 0;	/* exponent range argument */
	int precval		= 0;	/* precision value argument */
	_f_int8 exp_match 	= 0;	/* found exponent range match */
	_f_int8 prec_match 	= 0;	/* found precision match */
#if	defined(__mips)
	_CODE_SEL_MIPS();
#else	/* mips */
	_CODE_SEL();
#endif	/* mips */
}

#ifndef	_UNICOS
/* Duplicate name to build the Sparc F90 compiler prerelease version */
_f_int8 _SELECTED_REAL_KIND_8(_f_int *prec, _f_int *range) 
{
#if	defined(__mips)
	int e_indx		= 0;	/* found exponent range match */
	int p_indx		= 0;	/* found precision match */
	int result;
#endif	/* mips */
	int i;
	int exprangval		= 0;	/* exponent range argument */
	int precval		= 0;	/* precision value argument */
	_f_int8 exp_match 	= 0;	/* found exponent range match */
	_f_int8 prec_match 	= 0;	/* found precision match */
#if	defined(__mips)
	_CODE_SEL_MIPS();
#else	/* mips */
	_CODE_SEL();
#endif	/* mips */
}
#endif	/* not UNICOS */

#endif	/* _F_INT8 */
