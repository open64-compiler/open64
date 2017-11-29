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


static const char USMID[] = "@(#) libfi/array/maxval.c	92.0	10/08/98 14:37:14";

#include <stddef.h>
#include <liberrno.h>
#include <fmath.h>
#include <cray/dopevec.h>
#include <cray/portdefs.h>
#include "arraydefs.h"

#define	USE_INT8	1
#define	USE_INT6	2
#define	USE_INT4	3
#define	USE_INT2	4
#define	USE_INT1	5
#define	USE_REAL16	6
#define	USE_REAL8	7
#define	USE_REAL4	8

/*
 *	INCREMENT calculates the offset for each dimension of the source,
 *	mask and result arrays.  The sum of the offsets gives the number
 *	of elements from the beginning of the array.
 */

#define INCREMENT()							\
	curdim[0]++;							\
	if (curdim[0] < src_ext[0]) {					\
	    src_off[0] = curdim[0] * src_strd[0];			\
	    msk_off[0] = curdim[0] * msk_strd[0];			\
	    res_off[0] = curdim[0] * res_strd[0];			\
	} else {							\
	    curdim[0] = 0;						\
	    src_off[0] = 0;						\
	    msk_off[0] = 0;						\
	    res_off[0] = 0;						\
	    curdim[1]++;						\
	    if (curdim[1] < src_ext[1]) {				\
		src_off[1] = curdim[1] * src_strd[1];			\
		msk_off[1] = curdim[1] * msk_strd[1];			\
		res_off[1] = curdim[1] * res_strd[1];			\
	    } else {							\
		curdim[1] = 0;						\
		src_off[1] = 0;						\
		msk_off[1] = 0;						\
		res_off[1] = 0;						\
		curdim[2]++;						\
		if (curdim[2] < src_ext[2]) {				\
		    src_off[2] = curdim[2] * src_strd[2];		\
		    msk_off[2] = curdim[2] * msk_strd[2];		\
		    res_off[2] = curdim[2] * res_strd[2];		\
		} else {						\
		    curdim[2] = 0;					\
		    src_off[2] = 0;					\
		    msk_off[2] = 0;					\
		    res_off[2] = 0;					\
		    curdim[3]++;					\
		    if (curdim[3] < src_ext[3]) {			\
			src_off[3] = curdim[3] * src_strd[3];		\
			msk_off[3] = curdim[3] * msk_strd[3];		\
			res_off[3] = curdim[3] * res_strd[3];		\
		    } else {						\
			curdim[3] = 0;					\
			src_off[3] = 0;					\
			msk_off[3] = 0;					\
			res_off[3] = 0;					\
			curdim[4]++;					\
			if (curdim[4] < src_ext[4]) {			\
			    src_off[4] = curdim[4] * src_strd[4];	\
			    msk_off[4] = curdim[4] * msk_strd[4];	\
			    res_off[4] = curdim[4] * res_strd[4];	\
			} else {					\
			    curdim[4] = 0;				\
			    src_off[4] = 0;				\
			    msk_off[4] = 0;				\
			    res_off[4] = 0;				\
			    curdim[5]++;				\
			    src_off[5] = curdim[5] * src_strd[5];	\
			    msk_off[5] = curdim[5] * msk_strd[5];	\
			    res_off[5] = curdim[5] * res_strd[5];	\
			}						\
		    }							\
		}							\
	    }								\
	}

/*	INCR_RES calculates the offset for the result array	*/

#define INCR_RES()							\
	curdim[0]++;							\
	if (curdim[0] < res_ext[0]) {					\
	    res_off[0] = curdim[0] * res_strd[0];			\
	} else {							\
	    curdim[0] = 0;						\
	    res_off[0] = 0;						\
	    curdim[1]++;						\
	    if (curdim[1] < res_ext[1]) {				\
		res_off[1] = curdim[1] * res_strd[1];			\
	    } else {							\
		curdim[1] = 0;						\
		res_off[1] = 0;						\
		curdim[2]++;						\
		if (curdim[2] < res_ext[2]) {				\
		    res_off[2] = curdim[2] * res_strd[2];		\
		} else {						\
		    curdim[2] = 0;					\
		    res_off[2] = 0;					\
		    curdim[3]++;					\
		    if (curdim[3] < res_ext[3]) {			\
			res_off[3] = curdim[3] * res_strd[3];		\
		    } else {						\
			curdim[3] = 0;					\
			res_off[3] = 0;					\
			curdim[4]++;					\
			if (curdim[4] < res_ext[4]) {			\
			    res_off[4] = curdim[4] * res_strd[4];	\
			} else {					\
			    curdim[4] = 0;				\
			    res_off[4] = 0;				\
			    curdim[5]++;				\
			    res_off[5] = curdim[5] * res_strd[5];	\
			}						\
		    }							\
		}							\
	    }								\
	}								\
	if (rank == 2)							\
	    rindx = res_off[0];						\
	else if (rank == 3)						\
	    rindx = res_off[0] + res_off[1];				\
	else if (rank == 4)						\
	    rindx = res_off[0] + res_off[1] + res_off[2];		\
	else if (rank == 5)						\
	    rindx = res_off[0] + res_off[1] +				\
		    res_off[2] + res_off[3];				\
	else if (rank == 6)						\
	    rindx = res_off[0] + res_off[1] + res_off[2] +		\
		    res_off[3] + res_off[4];				\
	else								\
	    rindx = res_off[0] + res_off[1] + res_off[2] +		\
		    res_off[3] + res_off[4] + res_off[5];

/*	LOAD_DM_MK correctly for P.E. 3.0 and above */

#define LOAD_DM_MK()							\
	dm = dimp;							\
	mk = mask;							\
	/* if last arg = NULL, is last-1 arg mask or dim? */		\
	if (mask == NULL) {						\
		/* last arg = NULL, is last-1 arg mask or dim? */	\
		if (dimp != NULL) {					\
			if (dimp->type_lens.type == DVTYPE_LOGICAL) {	\
				/* last-1 argument is mask. */		\
				mk = dimp;				\
				dm = mask;				\
			}						\
		}							\
	}								\
	if (dm != NULL)	{						\
		_f_int dmintlen;					\
		dmintlen = dm->type_lens.int_len >> 3;			\
		dimenp = (_f_int *) &dimenlc;				\
		if (dmintlen == sizeof(_f_int8)) {			\
			*dimenp	= *(_f_int8 *) dm->base_addr.a.ptr;	\
		} else if (dmintlen == sizeof(_f_int4)) {		\
			*dimenp	= *(_f_int4 *) dm->base_addr.a.ptr;	\
		} else if (dmintlen == sizeof(_f_int2)) {		\
			*dimenp	= *(_f_int2 *) dm->base_addr.a.ptr;	\
		} else if (dmintlen == sizeof(_f_int1)) {		\
			*dimenp	= *(_f_int1 *) dm->base_addr.a.ptr;	\
		}							\
	}


/*
 *	MAXVAL has six entry points.  There is an entry point for a dope
 *	vector return, and a scalar return for integer, real and double
 *	precision.
 */

/* P.E. 2.0 and earlier entry points for MAXVAL contain only a single
 * underbar after MAXVAL and before the TYPE letter.  Only these entry
 * points may specify DVSUBTYPE_INT46 and INTEGER(KIND=6).  P.E. 3.0
 * does not have INTEGER(KIND=6).
 *
 * P.E. 3.0 uses the double underbar between MAXVAL and the TYPE letter.
 * Note that we can never do a fast compare the way in which the routine
 * is written because it starts with with -HUGE as the first maxinum
 * value it is comparing against to find the maximum location.
 */

#ifdef _UNICOS
#pragma _CRI duplicate _MAXVAL_J as MAXVAL_J@
#endif
void
_MAXVAL_J (	DopeVectorType	* result,
		DopeVectorType	* source,
		_f_int		* dimp,
		DopeVectorType	* mask)
{
	void __maxval();
	(void) __maxval (result, source, dimp, mask, 0, DVSUBTYPE_INT64,
		USE_INT8);
}

void
_MAXVAL__J (	DopeVectorType	* result,
		DopeVectorType	* source,
		DopeVectorType	* dimp,
		DopeVectorType	* mask)
{
	void __maxval();
	DopeVectorType	*dm, *mk;
	_f_int *dimenp	= NULL;
	_f_int dimenlc	= 0;
	LOAD_DM_MK();
	(void) __maxval (result, source, dimenp, mk, 0, DVSUBTYPE_INT64,
		USE_INT8);
}


#ifdef _UNICOS
#pragma _CRI duplicate _MAXVAL_S as MAXVAL_S@
#endif
void
_MAXVAL_S (	DopeVectorType	* result,
		DopeVectorType	* source,
		_f_int		* dimp,
		DopeVectorType	* mask)
{
	void __maxval();
	(void) __maxval (result, source, dimp, mask, 0, DVSUBTYPE_REAL64,
		USE_REAL8);
}

void
_MAXVAL__S (	DopeVectorType	* result,
		DopeVectorType	* source,
		DopeVectorType	* dimp,
		DopeVectorType	* mask)
{
	void __maxval();
	DopeVectorType	*dm, *mk;
	_f_int *dimenp	= NULL;
	_f_int dimenlc	= 0;
	LOAD_DM_MK();
	(void) __maxval (result, source, dimenp, mk, 0, DVSUBTYPE_REAL64,
		USE_REAL8);
}

#if defined _F_REAL16 && _F_REAL16 != (-1)
#ifdef _UNICOS
#pragma _CRI duplicate _MAXVAL_D as MAXVAL_D@
#endif
void
_MAXVAL_D (	DopeVectorType	* result,
		DopeVectorType	* source,
		_f_int		* dimp,
		DopeVectorType	* mask)
{
	void __maxval();
	(void) __maxval (result, source, dimp, mask, 0, DVSUBTYPE_REAL128,
		USE_REAL16);
}

void
_MAXVAL__D (	DopeVectorType	* result,
		DopeVectorType	* source,
		DopeVectorType	* dimp,
		DopeVectorType	* mask)
{
	void __maxval();
	DopeVectorType	*dm, *mk;
	_f_int *dimenp	= NULL;
	_f_int dimenlc	= 0;
	LOAD_DM_MK();
	(void) __maxval (result, source, dimenp, mk, 0, DVSUBTYPE_REAL128,
		USE_REAL16);
}
#endif

#ifdef _UNICOS
#pragma _CRI duplicate _MAXVAL_I as MAXVAL_I@
#endif
void
_MAXVAL_I (	DopeVectorType	* result,
		DopeVectorType	* source,
		_f_int		* dimp,
		DopeVectorType	* mask)
{
	void __maxval();
#ifdef	_F_INT6
	(void) __maxval (result, source, dimp, mask, 0, DVSUBTYPE_INT46,
		USE_INT6);
#else
	(void) __maxval (result, source, dimp, mask, 0, DVSUBTYPE_INT64,
		USE_INT8);
#endif
}

void
_MAXVAL__I (	DopeVectorType	* result,
		DopeVectorType	* source,
		DopeVectorType	* dimp,
		DopeVectorType	* mask)
{
	void __maxval();
	DopeVectorType	*dm, *mk;
	_f_int *dimenp	= NULL;
	_f_int dimenlc	= 0;
	LOAD_DM_MK();
#ifdef	_F_INT6
	(void) __maxval (result, source, dimenp, mk, 0, DVSUBTYPE_INT46,
		USE_INT6);
#else
	(void) __maxval (result, source, dimenp, mk, 0, DVSUBTYPE_INT64,
		USE_INT8);
#endif
}

#ifndef	_F_INT6
#ifdef	__mips
typedef _f_int8	_f_int6;	/* integer of default kind */
#else
typedef int	_f_int6;	/* integer of default kind */
#endif	/* __mips */
#endif  /* Not _F_INT6 */

#ifdef _UNICOS
#pragma _CRI duplicate _MAXVAL0_I as MAXVAL0_I@
#endif
_f_int6
_MAXVAL0_I (	DopeVectorType	* source,
		_f_int		* dimp,
		DopeVectorType	* mask)
{
	void		__maxval();
	int		i = 1;
	_f_int6		i6;
	DopeVectorType  result, *res_ptr;

	i6 = (_f_int6) -HUGE_INT6_F90;
	res_ptr = (DopeVectorType *) &result;
	res_ptr->assoc = 1;
	res_ptr->base_addr.a.ptr = (_f_int6 *) &i6;
	res_ptr->base_addr.a.el_len = source->base_addr.a.el_len;
	res_ptr->ptr_alloc = 0;
	res_ptr->p_or_a = NOT_P_OR_A;
	res_ptr->n_dim = 0;
	res_ptr->type_lens.type = DVTYPE_INTEGER;
	res_ptr->type_lens.dpflag = source->type_lens.dpflag;
	res_ptr->type_lens.kind_or_star = source->type_lens.kind_or_star;
	res_ptr->type_lens.int_len = source->type_lens.int_len;
	res_ptr->type_lens.dec_len = source->type_lens.dec_len;
	res_ptr->orig_base = (_f_int6 *) NULL;
	res_ptr->orig_size = 0;
#ifdef	_F_INT6
	__maxval (res_ptr, source, dimp, mask, 1, DVSUBTYPE_INT46,
		USE_INT6);
#else
	(void) __maxval (res_ptr, source, dimp, mask, 0, DVSUBTYPE_INT64,
		USE_INT8);
#endif
	return (*(_f_int6 *) res_ptr->base_addr.a.ptr);
}

_f_int8
_MAXVAL0__I (	DopeVectorType	* source,
		DopeVectorType	* dimp,
		DopeVectorType	* mask)
{
	void		__maxval();
	int		i = 1;
	_f_int6		i6;
	DopeVectorType  result, *res_ptr;
	DopeVectorType	*dm, *mk;
	_f_int *dimenp	= NULL;
	_f_int dimenlc	= 0;

	LOAD_DM_MK();
	i6 = (_f_int6) -HUGE_INT6_F90;
	res_ptr = (DopeVectorType *) &result;
	res_ptr->assoc = 1;
	res_ptr->base_addr.a.ptr = (_f_int6 *) &i6;
	res_ptr->base_addr.a.el_len = source->base_addr.a.el_len;
	res_ptr->ptr_alloc = 0;
	res_ptr->p_or_a = NOT_P_OR_A;
	res_ptr->n_dim = 0;
	res_ptr->type_lens.type = DVTYPE_INTEGER;
	res_ptr->type_lens.dpflag = source->type_lens.dpflag;
	res_ptr->type_lens.kind_or_star = source->type_lens.kind_or_star;
	res_ptr->type_lens.int_len = source->type_lens.int_len;
	res_ptr->type_lens.dec_len = source->type_lens.dec_len;
	res_ptr->orig_base = (_f_int6 *) NULL;
	res_ptr->orig_size = 0;
#ifdef	_F_INT6
	__maxval (res_ptr, source, dimenp, mk, 1, DVSUBTYPE_INT46,
		USE_INT6);
#else
	(void) __maxval (res_ptr, source, dimenp, mk, 0, DVSUBTYPE_INT64,
		USE_INT8);
#endif
	return (*(_f_int6 *) res_ptr->base_addr.a.ptr);
}

#ifdef _UNICOS
#pragma _CRI duplicate _MAXVAL_I4 as MAXVAL_I4@
#endif
void
_MAXVAL_I4 (	DopeVectorType	* result,
		DopeVectorType	* source,
		_f_int		* dimp,
		DopeVectorType	* mask)
{
	void __maxval();
#ifndef	_F_INT4
	(void) __maxval (result, source, dimp, mask, 0, DVSUBTYPE_INT46,
		USE_INT4);
#else
	(void) __maxval (result, source, dimp, mask, 0, DVSUBTYPE_INT32,
		USE_INT4);
#endif
}

void
_MAXVAL__I4 (	DopeVectorType	* result,
		DopeVectorType	* source,
		DopeVectorType	* dimp,
		DopeVectorType	* mask)
{
	void __maxval();
	DopeVectorType	*dm, *mk;
	_f_int *dimenp	= NULL;
	_f_int dimenlc	= 0;
	LOAD_DM_MK();
#ifndef	_F_INT4
	(void) __maxval (result, source, dimenp, mk, 0, DVSUBTYPE_INT46,
		USE_INT4);
#else
	(void) __maxval (result, source, dimenp, mk, 0, DVSUBTYPE_INT32,
		USE_INT4);
#endif
}

#ifdef _UNICOS
#pragma _CRI duplicate _MAXVAL_I2 as MAXVAL_I2@
#endif
void
_MAXVAL_I2 (	DopeVectorType	* result,
		DopeVectorType	* source,
		_f_int		* dimp,
		DopeVectorType	* mask)
{
	void __maxval();
#ifndef	_F_INT4
	(void) __maxval (result, source, dimp, mask, 0, DVSUBTYPE_INT46,
		USE_INT2);
#else
	(void) __maxval (result, source, dimp, mask, 0, DVSUBTYPE_INT32,
		USE_INT2);
#endif
}

void
_MAXVAL__I2 (	DopeVectorType	* result,
		DopeVectorType	* source,
		DopeVectorType	* dimp,
		DopeVectorType	* mask)
{
	void __maxval();
	DopeVectorType	*dm, *mk;
	_f_int *dimenp	= NULL;
	_f_int dimenlc	= 0;
	LOAD_DM_MK();
#ifndef	_F_INT4
	(void) __maxval (result, source, dimenp, mk, 0, DVSUBTYPE_INT46,
		USE_INT2);
#else
	(void) __maxval (result, source, dimenp, mk, 0, DVSUBTYPE_INT32,
		USE_INT2);
#endif
}

#ifdef _UNICOS
#pragma _CRI duplicate _MAXVAL_I1 as MAXVAL_I1@
#endif
void
_MAXVAL_I1 (	DopeVectorType	* result,
		DopeVectorType	* source,
		_f_int		* dimp,
		DopeVectorType	* mask)
{
	void __maxval();
#ifndef	_F_INT4
	(void) __maxval (result, source, dimp, mask, 0, DVSUBTYPE_INT46,
		USE_INT1);
#else
	(void) __maxval (result, source, dimp, mask, 0, DVSUBTYPE_INT32,
		USE_INT1);
#endif
}

void
_MAXVAL__I1 (	DopeVectorType	* result,
		DopeVectorType	* source,
		DopeVectorType	* dimp,
		DopeVectorType	* mask)
{
	void __maxval();
	DopeVectorType	*dm, *mk;
	_f_int *dimenp	= NULL;
	_f_int dimenlc	= 0;
	LOAD_DM_MK();
#ifndef	_F_INT4
	(void) __maxval (result, source, dimenp, mk, 0, DVSUBTYPE_INT46,
		USE_INT1);
#else
	(void) __maxval (result, source, dimenp, mk, 0, DVSUBTYPE_INT32,
		USE_INT1);
#endif
}

#ifdef _UNICOS
#pragma _CRI duplicate _MAXVAL0_I4 as MAXVAL0_I4@
#endif
_f_int4
_MAXVAL0_I4 (	DopeVectorType	* source,
		_f_int		* dimp,
		DopeVectorType	* mask)
{
	void		__maxval();
	int		i = 1;
	_f_int4		i4;
	DopeVectorType  result, *res_ptr;

	i4 = (_f_int4) -HUGE_INT4_F90;
	res_ptr = (DopeVectorType *) &result;
	res_ptr->assoc = 1;
	res_ptr->base_addr.a.ptr = (_f_int4 *) &i4;
	res_ptr->base_addr.a.el_len = source->base_addr.a.el_len;
	res_ptr->ptr_alloc = 0;
	res_ptr->p_or_a = NOT_P_OR_A;
	res_ptr->n_dim = 0;
	res_ptr->type_lens.type = DVTYPE_INTEGER;
	res_ptr->type_lens.dpflag = source->type_lens.dpflag;
	res_ptr->type_lens.kind_or_star = source->type_lens.kind_or_star;
	res_ptr->type_lens.int_len = source->type_lens.int_len;
	res_ptr->type_lens.dec_len = source->type_lens.dec_len;
	res_ptr->orig_base = (_f_int4 *) NULL;
	res_ptr->orig_size = 0;
#ifndef	_F_INT4
	__maxval (res_ptr, source, dimp, mask, 1, DVSUBTYPE_INT46,
		USE_INT4);
#else
	__maxval (res_ptr, source, dimp, mask, 1, DVSUBTYPE_INT32,
		USE_INT4);
#endif
	return (*(_f_int4 *) res_ptr->base_addr.a.ptr);
}

_f_int4
_MAXVAL0__I4 (	DopeVectorType	* source,
		DopeVectorType	* dimp,
		DopeVectorType	* mask)
{
	void		__maxval();
	int		i = 1;
	_f_int4		i4;
	DopeVectorType  result, *res_ptr;
	DopeVectorType	*dm, *mk;
	_f_int *dimenp	= NULL;
	_f_int dimenlc	= 0;

	LOAD_DM_MK();
	i4 = (_f_int4) -HUGE_INT4_F90;
	res_ptr = (DopeVectorType *) &result;
	res_ptr->assoc = 1;
	res_ptr->base_addr.a.ptr = (_f_int4 *) &i4;
	res_ptr->base_addr.a.el_len = source->base_addr.a.el_len;
	res_ptr->ptr_alloc = 0;
	res_ptr->p_or_a = NOT_P_OR_A;
	res_ptr->n_dim = 0;
	res_ptr->type_lens.type = DVTYPE_INTEGER;
	res_ptr->type_lens.dpflag = source->type_lens.dpflag;
	res_ptr->type_lens.kind_or_star = source->type_lens.kind_or_star;
	res_ptr->type_lens.int_len = source->type_lens.int_len;
	res_ptr->type_lens.dec_len = source->type_lens.dec_len;
	res_ptr->orig_base = (_f_int4 *) NULL;
	res_ptr->orig_size = 0;
#ifndef	_F_INT4
	__maxval (res_ptr, source, dimenp, mk, 1, DVSUBTYPE_INT46,
		USE_INT4);
#else
	__maxval (res_ptr, source, dimenp, mk, 1, DVSUBTYPE_INT32,
		USE_INT4);
#endif
	return (*(_f_int4 *) res_ptr->base_addr.a.ptr);
}

#ifdef _UNICOS
#pragma _CRI duplicate _MAXVAL0_I2 as MAXVAL0_I2@
#endif
_f_int2
_MAXVAL0_I2 (	DopeVectorType	* source,
		_f_int		* dimp,
		DopeVectorType	* mask)
{
	void		__maxval();
	int		i = 1;
	_f_int2		i2;
	DopeVectorType  result, *res_ptr;

	i2 = (_f_int2) -HUGE_INT2_F90;
	res_ptr = (DopeVectorType *) &result;
	res_ptr->assoc = 1;
	res_ptr->base_addr.a.ptr = (_f_int2 *) &i2;
	res_ptr->base_addr.a.el_len = source->base_addr.a.el_len;
	res_ptr->ptr_alloc = 0;
	res_ptr->p_or_a = NOT_P_OR_A;
	res_ptr->n_dim = 0;
	res_ptr->type_lens.type = DVTYPE_INTEGER;
	res_ptr->type_lens.dpflag = source->type_lens.dpflag;
	res_ptr->type_lens.kind_or_star = source->type_lens.kind_or_star;
	res_ptr->type_lens.int_len = source->type_lens.int_len;
	res_ptr->type_lens.dec_len = source->type_lens.dec_len;
	res_ptr->orig_base = (_f_int2 *) NULL;
	res_ptr->orig_size = 0;
#ifndef	_F_INT4
	__maxval (res_ptr, source, dimp, mask, 1, DVSUBTYPE_INT46,
		USE_INT2);
#else
	__maxval (res_ptr, source, dimp, mask, 1, DVSUBTYPE_INT32,
		USE_INT2);
#endif
	return (*(_f_int2 *) res_ptr->base_addr.a.ptr);
}

_f_int2
_MAXVAL0__I2 (	DopeVectorType	* source,
		DopeVectorType	* dimp,
		DopeVectorType	* mask)
{
	void		__maxval();
	int		i = 1;
	_f_int2		i2;
	DopeVectorType  result, *res_ptr;
	DopeVectorType  *dm, *mk;
	_f_int *dimenp	= NULL;
	_f_int dimenlc	= 0;

	LOAD_DM_MK();
	i2 = (_f_int2) -HUGE_INT2_F90;
	res_ptr = (DopeVectorType *) &result;
	res_ptr->assoc = 1;
	res_ptr->base_addr.a.ptr = (_f_int2 *) &i2;
	res_ptr->base_addr.a.el_len = source->base_addr.a.el_len;
	res_ptr->ptr_alloc = 0;
	res_ptr->p_or_a = NOT_P_OR_A;
	res_ptr->n_dim = 0;
	res_ptr->type_lens.type = DVTYPE_INTEGER;
	res_ptr->type_lens.dpflag = source->type_lens.dpflag;
	res_ptr->type_lens.kind_or_star = source->type_lens.kind_or_star;
	res_ptr->type_lens.int_len = source->type_lens.int_len;
	res_ptr->type_lens.dec_len = source->type_lens.dec_len;
	res_ptr->orig_base = (_f_int2 *) NULL;
	res_ptr->orig_size = 0;
#ifndef	_F_INT4
	__maxval (res_ptr, source, dimenp, mk, 1, DVSUBTYPE_INT46,
		USE_INT2);
#else
	__maxval (res_ptr, source, dimenp, mk, 1, DVSUBTYPE_INT32,
		USE_INT2);
#endif
	return (*(_f_int2 *) res_ptr->base_addr.a.ptr);
}

#ifdef _UNICOS
#pragma _CRI duplicate _MAXVAL0_I1 as MAXVAL0_I1@
#endif
_f_int1
_MAXVAL0_I1 (	DopeVectorType	* source,
		_f_int		* dimp,
		DopeVectorType	* mask)
{
	void		__maxval();
	int		i = 1;
	_f_int1		i1;
	DopeVectorType  result, *res_ptr;

	i1 = (_f_int1) -HUGE_INT1_F90;
	res_ptr = (DopeVectorType *) &result;
	res_ptr->assoc = 1;
	res_ptr->base_addr.a.ptr = (_f_int1 *) &i1;
	res_ptr->base_addr.a.el_len = source->base_addr.a.el_len;
	res_ptr->ptr_alloc = 0;
	res_ptr->p_or_a = NOT_P_OR_A;
	res_ptr->n_dim = 0;
	res_ptr->type_lens.type = DVTYPE_INTEGER;
	res_ptr->type_lens.dpflag = source->type_lens.dpflag;
	res_ptr->type_lens.kind_or_star = source->type_lens.kind_or_star;
	res_ptr->type_lens.int_len = source->type_lens.int_len;
	res_ptr->type_lens.dec_len = source->type_lens.dec_len;
	res_ptr->orig_base = (_f_int1 *) NULL;
	res_ptr->orig_size = 0;
#ifndef	_F_INT4
	__maxval (res_ptr, source, dimp, mask, 1, DVSUBTYPE_INT46,
		USE_INT1);
#else
	__maxval (res_ptr, source, dimp, mask, 1, DVSUBTYPE_INT32,
		USE_INT1);
#endif
	return (*(_f_int1 *) res_ptr->base_addr.a.ptr);
}

_f_int1
_MAXVAL0__I1 (	DopeVectorType	* source,
		DopeVectorType	* dimp,
		DopeVectorType	* mask)
{
	void		__maxval();
	int		i = 1;
	_f_int1		i1;
	DopeVectorType  result, *res_ptr;
	DopeVectorType	*dm, *mk;
	_f_int *dimenp	= NULL;
	_f_int dimenlc	= 0;

	LOAD_DM_MK();
	i1 = (_f_int1) -HUGE_INT1_F90;
	res_ptr = (DopeVectorType *) &result;
	res_ptr->assoc = 1;
	res_ptr->base_addr.a.ptr = (_f_int1 *) &i1;
	res_ptr->base_addr.a.el_len = source->base_addr.a.el_len;
	res_ptr->ptr_alloc = 0;
	res_ptr->p_or_a = NOT_P_OR_A;
	res_ptr->n_dim = 0;
	res_ptr->type_lens.type = DVTYPE_INTEGER;
	res_ptr->type_lens.dpflag = source->type_lens.dpflag;
	res_ptr->type_lens.kind_or_star = source->type_lens.kind_or_star;
	res_ptr->type_lens.int_len = source->type_lens.int_len;
	res_ptr->type_lens.dec_len = source->type_lens.dec_len;
	res_ptr->orig_base = (_f_int1 *) NULL;
	res_ptr->orig_size = 0;
#ifndef	_F_INT4
	__maxval (res_ptr, source, dimenp, mk, 1, DVSUBTYPE_INT46,
		USE_INT1);
#else
	__maxval (res_ptr, source, dimenp, mk, 1, DVSUBTYPE_INT32,
		USE_INT1);
#endif
	return (*(_f_int1 *) res_ptr->base_addr.a.ptr);
}

#ifdef _UNICOS
#pragma _CRI duplicate _MAXVAL_S4 as MAXVAL_S4@
#endif
void
_MAXVAL_S4 (	DopeVectorType	* result,
		DopeVectorType	* source,
		_f_int		* dimp,
		DopeVectorType	* mask)
{
	void __maxval();
#ifndef	_F_REAL4
	(void) __maxval (result, source, dimp, mask, 0, DVSUBTYPE_REAL64,
		USE_REAL4);
#else
	(void) __maxval (result, source, dimp, mask, 0, DVSUBTYPE_REAL32,
		USE_REAL4);
#endif
}

void
_MAXVAL__S4 (	DopeVectorType	* result,
		DopeVectorType	* source,
		DopeVectorType	* dimp,
		DopeVectorType	* mask)
{
	void __maxval();
	DopeVectorType	*dm, *mk;
	_f_int *dimenp	= NULL;
	_f_int dimenlc	= 0;
	LOAD_DM_MK();
#ifndef	_F_REAL4
	(void) __maxval (result, source, dimenp, mk, 0, DVSUBTYPE_REAL64,
		USE_REAL4);
#else
	(void) __maxval (result, source, dimenp, mk, 0, DVSUBTYPE_REAL32,
		USE_REAL4);
#endif
}

#ifdef _UNICOS
#pragma _CRI duplicate _MAXVAL0_S4 as MAXVAL0_S4@
#endif
_f_real4
_MAXVAL0_S4 (	DopeVectorType	* source,
		_f_int		* dimp,
		DopeVectorType	* mask)
{
	void		__maxval();
	int		i = 1;
	_f_real4	s4;
	DopeVectorType  result, *res_ptr;

	s4 = (_f_real4) -HUGE_REAL4_F90;
	res_ptr = (DopeVectorType *) &result;
	res_ptr->assoc = 1;
	res_ptr->base_addr.a.ptr = (_f_real4 *) &s4;
	res_ptr->base_addr.a.el_len = source->base_addr.a.el_len;
	res_ptr->ptr_alloc = 0;
	res_ptr->p_or_a = NOT_P_OR_A;
	res_ptr->n_dim = 0;
	res_ptr->type_lens.type = DVTYPE_REAL;
	res_ptr->type_lens.dpflag = source->type_lens.dpflag;
	res_ptr->type_lens.kind_or_star = source->type_lens.kind_or_star;
	res_ptr->type_lens.int_len = source->type_lens.int_len;
	res_ptr->type_lens.dec_len = source->type_lens.dec_len;
	res_ptr->orig_base = (_f_real4 *) NULL;
	res_ptr->orig_size = 0;
#ifndef	_F_REAL4
	__maxval (res_ptr, source, dimp, mask, 1, DVSUBTYPE_REAL64,
		USE_REAL4);
#else
	__maxval (res_ptr, source, dimp, mask, 1, DVSUBTYPE_REAL32,
		USE_REAL4);
#endif
	return (*(_f_real4 *) res_ptr->base_addr.a.ptr);
}

_f_real4
_MAXVAL0__S4 (	DopeVectorType	* source,
		DopeVectorType	* dimp,
		DopeVectorType	* mask)
{
	void		__maxval();
	int		i = 1;
	_f_real4	s4;
	DopeVectorType  result, *res_ptr;
	DopeVectorType	*dm, *mk;
	_f_int *dimenp	= NULL;
	_f_int dimenlc	= 0;

	LOAD_DM_MK();
	s4 = (_f_real4) -HUGE_REAL4_F90;
	res_ptr = (DopeVectorType *) &result;
	res_ptr->assoc = 1;
	res_ptr->base_addr.a.ptr = (_f_real4 *) &s4;
	res_ptr->base_addr.a.el_len = source->base_addr.a.el_len;
	res_ptr->ptr_alloc = 0;
	res_ptr->p_or_a = NOT_P_OR_A;
	res_ptr->n_dim = 0;
	res_ptr->type_lens.type = DVTYPE_REAL;
	res_ptr->type_lens.dpflag = source->type_lens.dpflag;
	res_ptr->type_lens.kind_or_star = source->type_lens.kind_or_star;
	res_ptr->type_lens.int_len = source->type_lens.int_len;
	res_ptr->type_lens.dec_len = source->type_lens.dec_len;
	res_ptr->orig_base = (_f_real4 *) NULL;
	res_ptr->orig_size = 0;
#ifndef	_F_REAL4
	__maxval (res_ptr, source, dimenp, mk, 1, DVSUBTYPE_REAL64,
		USE_REAL4);
#else
	__maxval (res_ptr, source, dimenp, mk, 1, DVSUBTYPE_REAL32,
		USE_REAL4);
#endif
	return (*(_f_real4 *) res_ptr->base_addr.a.ptr);
}

#ifdef _UNICOS
#pragma _CRI duplicate _MAXVAL0_J as MAXVAL0_J@
#endif
_f_int8
_MAXVAL0_J (	DopeVectorType	* source,
		_f_int		* dimp,
		DopeVectorType	* mask)
{
	void		__maxval();
	int		i = 1;
	_f_int8		i8;
	DopeVectorType  result, *res_ptr;

	i8 = (_f_int8) -HUGE_INT8_F90;
	res_ptr = (DopeVectorType *) &result;
	res_ptr->assoc = 1;
	res_ptr->base_addr.a.ptr = (_f_int8 *) &i8;
	res_ptr->base_addr.a.el_len = source->base_addr.a.el_len;
	res_ptr->ptr_alloc = 0;
	res_ptr->p_or_a = NOT_P_OR_A;
	res_ptr->n_dim = 0;
	res_ptr->type_lens.type = DVTYPE_INTEGER;
	res_ptr->type_lens.dpflag = source->type_lens.dpflag;
	res_ptr->type_lens.kind_or_star = source->type_lens.kind_or_star;
	res_ptr->type_lens.int_len = source->type_lens.int_len;
	res_ptr->type_lens.dec_len = source->type_lens.dec_len;
	res_ptr->orig_base = (_f_int8 *) NULL;
	res_ptr->orig_size = 0;
	__maxval (res_ptr, source, dimp, mask, 1, DVSUBTYPE_INT64,
		USE_INT8);
	return(*(_f_int8 *) res_ptr->base_addr.a.ptr);
}

_f_int8
_MAXVAL0__J (	DopeVectorType	* source,
		DopeVectorType	* dimp,
		DopeVectorType	* mask)
{
	void		__maxval();
	int		i = 1;
	_f_int8		i8;
	DopeVectorType  result, *res_ptr;
	DopeVectorType	*dm, *mk;
	_f_int *dimenp	= NULL;
	_f_int dimenlc	= 0;

	LOAD_DM_MK();
	i8 = (_f_int8) -HUGE_INT8_F90;
	res_ptr = (DopeVectorType *) &result;
	res_ptr->assoc = 1;
	res_ptr->base_addr.a.ptr = (_f_int8 *) &i8;
	res_ptr->base_addr.a.el_len = source->base_addr.a.el_len;
	res_ptr->ptr_alloc = 0;
	res_ptr->p_or_a = NOT_P_OR_A;
	res_ptr->n_dim = 0;
	res_ptr->type_lens.type = DVTYPE_INTEGER;
	res_ptr->type_lens.dpflag = source->type_lens.dpflag;
	res_ptr->type_lens.kind_or_star = source->type_lens.kind_or_star;
	res_ptr->type_lens.int_len = source->type_lens.int_len;
	res_ptr->type_lens.dec_len = source->type_lens.dec_len;
	res_ptr->orig_base = (_f_int8 *) NULL;
	res_ptr->orig_size = 0;
	__maxval (res_ptr, source, dimenp, mk, 1, DVSUBTYPE_INT64,
		USE_INT8);
	return(*(_f_int8 *) res_ptr->base_addr.a.ptr);
}

#ifdef _UNICOS
#pragma _CRI duplicate _MAXVAL0_S as MAXVAL0_S@
#endif
_f_real8
_MAXVAL0_S (	DopeVectorType	* source,
		_f_int		* dimp,
		DopeVectorType	* mask)
{
	void		__maxval();
	int		i = 1;
	_f_real8	s8;
	DopeVectorType  result, *res_ptr;

	s8 = (_f_real8) -HUGE_REAL8_F90;
	res_ptr = (DopeVectorType *) &result;
	res_ptr->assoc = 1;
	res_ptr->base_addr.a.ptr = (_f_real8 *) &s8;
	res_ptr->base_addr.a.el_len = source->base_addr.a.el_len;
	res_ptr->ptr_alloc = 0;
	res_ptr->p_or_a = NOT_P_OR_A;
	res_ptr->n_dim = 0;
	res_ptr->type_lens.type = DVTYPE_REAL;
	res_ptr->type_lens.dpflag = source->type_lens.dpflag;
	res_ptr->type_lens.kind_or_star = source->type_lens.kind_or_star;
	res_ptr->type_lens.int_len = source->type_lens.int_len;
	res_ptr->type_lens.dec_len = source->type_lens.dec_len;
	res_ptr->orig_base = (_f_real8 *) NULL;
	res_ptr->orig_size = 0;
	__maxval (res_ptr, source, dimp, mask, 1, DVSUBTYPE_REAL64,
		USE_REAL8);
	return(*(_f_real8 *) res_ptr->base_addr.a.ptr);
}

_f_real8
_MAXVAL0__S (	DopeVectorType	* source,
		DopeVectorType	* dimp,
		DopeVectorType	* mask)
{
	void		__maxval();
	int		i = 1;
	_f_real8	s8;
	DopeVectorType  result, *res_ptr;
	DopeVectorType	*dm, *mk;
	_f_int *dimenp	= NULL;
	_f_int dimenlc	= 0;

	LOAD_DM_MK();
	s8 = (_f_real8) -HUGE_REAL8_F90;
	res_ptr = (DopeVectorType *) &result;
	res_ptr->assoc = 1;
	res_ptr->base_addr.a.ptr = (_f_real8 *) &s8;
	res_ptr->base_addr.a.el_len = source->base_addr.a.el_len;
	res_ptr->ptr_alloc = 0;
	res_ptr->p_or_a = NOT_P_OR_A;
	res_ptr->n_dim = 0;
	res_ptr->type_lens.type = DVTYPE_REAL;
	res_ptr->type_lens.dpflag = source->type_lens.dpflag;
	res_ptr->type_lens.kind_or_star = source->type_lens.kind_or_star;
	res_ptr->type_lens.int_len = source->type_lens.int_len;
	res_ptr->type_lens.dec_len = source->type_lens.dec_len;
	res_ptr->orig_base = (_f_real8 *) NULL;
	res_ptr->orig_size = 0;
	__maxval (res_ptr, source, dimenp, mk, 1, DVSUBTYPE_REAL64,
		USE_REAL8);
	return(*(_f_real8 *) res_ptr->base_addr.a.ptr);
}

#if defined _F_REAL16 && _F_REAL16 != (-1)
#ifdef _UNICOS
#pragma _CRI duplicate _MAXVAL0_D as MAXVAL0_D@
#endif
_f_real16
_MAXVAL0_D (	DopeVectorType	* source,
		_f_int		* dimp,
		DopeVectorType	* mask)
{
	void		__maxval();
	int		i = 1;
	_f_real16	s16;
	DopeVectorType  result, *res_ptr;

	s16 = (_f_real16) -HUGE_REAL16_F90;
	res_ptr = (DopeVectorType *) &result;
	res_ptr->assoc = 1;
	res_ptr->base_addr.a.ptr = (_f_real16 *) &s16;
	res_ptr->base_addr.a.el_len = source->base_addr.a.el_len;
	res_ptr->ptr_alloc = 0;
	res_ptr->p_or_a = NOT_P_OR_A;
	res_ptr->n_dim = 0;
	res_ptr->type_lens.type = DVTYPE_REAL;
	res_ptr->type_lens.dpflag = source->type_lens.dpflag;
	res_ptr->type_lens.kind_or_star = source->type_lens.kind_or_star;
	res_ptr->type_lens.int_len = source->type_lens.int_len;
	res_ptr->type_lens.dec_len = source->type_lens.dec_len;
	res_ptr->orig_base = (_f_real16 *) NULL;
	res_ptr->orig_size = 0;
	__maxval (res_ptr, source, dimp, mask, 1, DVSUBTYPE_REAL128,
		USE_REAL16);
	return(*(_f_real16 *) res_ptr->base_addr.a.ptr);
}

_f_real16
_MAXVAL0__D (	DopeVectorType	* source,
		DopeVectorType	* dimp,
		DopeVectorType	* mask)
{
	void		__maxval();
	int		i = 1;
	_f_real16	s16;
	DopeVectorType  result, *res_ptr;
	DopeVectorType	*dm, *mk;
	_f_int *dimenp	= NULL;
	_f_int dimenlc	= 0;

	LOAD_DM_MK();
	s16 = (_f_real16) -HUGE_REAL16_F90;
	res_ptr = (DopeVectorType *) &result;
	res_ptr->assoc = 1;
	res_ptr->base_addr.a.ptr = (_f_real16 *) &s16;
	res_ptr->base_addr.a.el_len = source->base_addr.a.el_len;
	res_ptr->ptr_alloc = 0;
	res_ptr->p_or_a = NOT_P_OR_A;
	res_ptr->n_dim = 0;
	res_ptr->type_lens.type = DVTYPE_REAL;
	res_ptr->type_lens.dpflag = source->type_lens.dpflag;
	res_ptr->type_lens.kind_or_star = source->type_lens.kind_or_star;
	res_ptr->type_lens.int_len = source->type_lens.int_len;
	res_ptr->type_lens.dec_len = source->type_lens.dec_len;
	res_ptr->orig_base = (_f_real16 *) NULL;
	res_ptr->orig_size = 0;
	__maxval (res_ptr, source, dimenp, mk, 1, DVSUBTYPE_REAL128,
		USE_REAL16);
	return(*(_f_real16 *) res_ptr->base_addr.a.ptr);
}
#endif

void
__maxval (	DopeVectorType	* result,
		DopeVectorType	* source,
		_f_int		* dimp,
		DopeVectorType	* mask,
		int		scalar,
		int		subtype,
		int		init_val)

{
	_f_int4		* restrict i4ptr1;	/* 32-bit int		*/
	_f_int4		* restrict i4ptr2;	/* 32-bit int		*/
#ifdef _F_INT6
	_f_int6		* restrict i6ptr1;	/* 46-bit int		*/
	_f_int6		* restrict i6ptr2;	/* 46-bit int		*/
#endif
	_f_int8		* restrict i8ptr1;	/* 64-bit int		*/
	_f_int8		* restrict i8ptr2;	/* 64-bit int		*/
	_f_real8	* restrict r8ptr1;	/* 64-bit real		*/
	_f_real8	* restrict r8ptr2;	/* 64-bit real		*/
#if defined _F_REAL16 && _F_REAL16 != (-1)
	_f_real16	* restrict r16ptr1;	/* 128-bit real		*/
	_f_real16	* restrict r16ptr2;	/* 128-bit real		*/
#endif
	_f_real4	* restrict r4ptr1;	/* 32-bit real		*/
	_f_real4	* restrict r4ptr2;	/* 32-bit real		*/
	_f_mask		* restrict lptr;	/* "logical"		*/
	void		* restrict sptr;	/* source		*/
	void		* restrict rptr;	/* result		*/
	void		* restrict mptr;	/* mask			*/
	_f_int		dim;		/* dimension to check		*/
	_f_int		bucketsize;	/* size of elements		*/
	long		nbytes;		/* number of bytes		*/
	long		sindx;		/* source index			*/
	long		rindx;		/* result index			*/
	long		mindx;		/* mask index			*/
	_f_int		type;		/* result type			*/
	_f_int		rank;		/* rank of source matrix	*/
	long		src_ext[MAXDIM];  /* extents for source		*/
	long		src_strd[MAXDIM]; /* strides for source		*/
	long		src_off[MAXDIM];  /* offset			*/
	long		res_ext[MAXDIM];  /* extents for source		*/
	long		res_strd[MAXDIM]; /* strides for source		*/
	long		res_off[MAXDIM];  /* offset			*/
	long		msk_ext[MAXDIM];  /* extents for source		*/
	long		msk_strd[MAXDIM]; /* strides for source		*/
	long		msk_off[MAXDIM];  /* offset			*/
	long		curdim[MAXDIM];   /* current index counter	*/
	long		src_dim_ext;	/* dim index extent		*/
	long		src_dim_strd;	/* dim index stride		*/
	long		msk_dim_ext;	/* dim index extent		*/
	long		msk_dim_strd;	/* dim index stride		*/
	long		indx1_src;	/* index for dim 1 of source	*/
	long		indx1_res;	/* index for dim 1 of source	*/
	long		indx1_msk;	/* index for dim 1 of source	*/
	long		tot_ext;	/* total extent counter		*/
	_f_int		ndim;		/* non_dim index		*/
	_f_int		first;		/* first flag			*/
	_f_int4		cur_maxi4;	/* current maximum value	*/
	_f_int4		i32_result;	/* scalar integer result	*/
	_f_int4		use_int4;	/* holder for default ret val	*/
#ifdef _F_INT6
	_f_int6		cur_maxi;	/* current maximum value	*/
	_f_int6		i46_result;	/* scalar integer result	*/
	_f_int6		use_int6;	/* holder for default ret val	*/
#endif
	_f_int8		cur_maxj;	/* current maximum value	*/
	_f_int8		i64_result;	/* scalar integer result	*/
	_f_real8	cur_maxf;	/* current maximum value	*/
	_f_real8	s64_result;	/* scalar float result		*/
	_f_real8	use_flt8;	/* holder for default ret val	*/
#if defined _F_REAL16 && _F_REAL16 != (-1)
	_f_real16	cur_maxd;	/* current maximum value	*/
	_f_real16	s128_result;	/* scalar double result		*/
#endif
	_f_real4	cur_maxs4;	/* current maximin value	*/
	_f_real4	s32_result;	/* scalar result		*/
	_f_int		use_mask;	/* use mask flag		*/
	long		i, j;		/* index variables		*/
	long		mask_el_len;
	_f_int		early_exit;	/* early exit flag		*/

/*
 *	Set up local copies of the number of dimensions in the source
 *	array (rank) and the source array data type (type).
 */

	type = source->type_lens.type;
	rank = source->n_dim;

/*
 *	If any extent in any matrix has value 0, we can exit without doing
 *	any work.
 */

	early_exit = 0;
	for (i = 0; i < rank; i++) {
	    if (source->dimension[i].extent == 0)
		early_exit = 1;
	}
	if (result->assoc) {
	    if (!scalar) {
		for (i = 0; i < result->n_dim; i++) {
		    if (result->dimension[i].extent == 0)
			early_exit = 1;
		}
	    }
	}
	if (mask) {
	    for (i = 0; i < mask->n_dim; i++) {
		if (mask->dimension[i].extent == 0)
		    early_exit = 1;
	    }
	}

/*
 *	Initialize every array element to 0.
 */

#ifdef _UNICOS
#pragma	_CRI	shortloop
#endif
	for (i = 0; i < MAXDIM; i++) {
	    curdim[i] = 0;
	    src_ext[i] = 0;
	    src_strd[i] = 0;
	    src_off[i] = 0;
	    res_ext[i] = 0;
	    res_strd[i] = 0;
	    res_off[i] = 0;
	    msk_ext[i] = 0;
	    msk_strd[i] = 0;
	    msk_off[i] = 0;
	}

/*
 *	Set up dim variable.  It must be decremented by one to account
 *	for the difference in reference between C and FORTRAN
 */

	if (dimp != NULL && rank > 0) {
	    dim = *dimp - 1;
	    if (dim < 0 || dim >= rank)
		_lerror (_LELVL_ABORT, FESCIDIM);
	} else {
	    dim = 0;
	}

/*	Set bucketsize scalar	*/

	bucketsize = source->type_lens.int_len / BITS_PER_WORD;
#ifdef _CRAYMPP
	if (bucketsize == 0)
	    bucketsize = 1;
#endif

/*	Set up dope vector for result array	*/

	if (!result->assoc) {
	    result->base_addr.a.ptr = (void *) NULL;
	    result->orig_base = 0;
	    result->orig_size = 0;

	    tot_ext = bucketsize;
	    nbytes = bucketsize * BYTES_PER_WORD;
	    if (dimp == NULL || rank == 1) {
		result->n_dim = 0;
	    } else {
		result->n_dim = rank - 1;
		for (i = 0; i < dim; i++) {
		    result->dimension[i].extent =
			source->dimension[i].extent;
		    result->dimension[i].low_bound = 1;
		    result->dimension[i].stride_mult = tot_ext;
		    tot_ext *= result->dimension[i].extent;
		    nbytes *= result->dimension[i].extent;
		}
		for ( ; i < rank-1; i++) {
		    result->dimension[i].extent =
				source->dimension[i+1].extent;
		    result->dimension[i].low_bound = 1;
		    result->dimension[i].stride_mult = tot_ext;
		    tot_ext *= result->dimension[i].extent;
		    nbytes *= result->dimension[i].extent;
		}
	    }

	    if (!scalar) {
		if (nbytes > 0) {
		    result->base_addr.a.ptr = (void *) malloc (nbytes);
		    if (result->base_addr.a.ptr == NULL)
			_lerror (_LELVL_ABORT, FENOMEMY);
		}
		result->assoc = 1;
	    } else {
		if (subtype == DVSUBTYPE_INT64)
		    result->base_addr.a.ptr = (_f_int8 *) &i64_result;
#ifdef _F_INT6
		else if (subtype == DVSUBTYPE_INT46)
		    result->base_addr.a.ptr = (_f_int6 *) &i46_result;
#endif
		else if (subtype == DVSUBTYPE_INT32)
		    result->base_addr.a.ptr = (_f_int4 *) &i32_result;
		else if (subtype == DVSUBTYPE_REAL32)
		    result->base_addr.a.ptr = (_f_real4 *) &s32_result;
#if defined _F_REAL16 && _F_REAL16 != (-1)
		else if (subtype == DVSUBTYPE_REAL128)
		    result->base_addr.a.ptr = (_f_real16 *) &s128_result;
#endif
		else
		    result->base_addr.a.ptr = (_f_real8 *) &s64_result;
	    }
	    result->base_addr.a.el_len = source->base_addr.a.el_len;
	    result->orig_base = (void *) result->base_addr.a.ptr;
	    result->orig_size = nbytes * BITS_PER_BYTE;
	} else {
	    tot_ext = bucketsize;
	    if (dimp == NULL || rank == 1) {
		for (i = 0; i < rank; i++)
		    tot_ext *= source->dimension[i].extent;
	    } else {
		for (i = 0; i < dim; i++)
		    tot_ext *= source->dimension[i].extent;
		for ( ; i < rank-1; i++)
		    tot_ext *= source->dimension[i].extent;
	    }
	}

/*	Set up scalar pointers to all of the argument data areas  */

	sptr = (void *) source->base_addr.a.ptr;
	rptr = (void *) result->base_addr.a.ptr;
	if (mask)
	    mptr = (void *) mask->base_addr.a.ptr;

/*
 *	If mask is present as a scalar, and its value is false, we can
 *	exit early.
 */

	if (mask) {
	    mask_el_len = mask->base_addr.a.el_len;
	    lptr = (_f_mask *) mptr;
	    if (mask->n_dim == 0 && !LTOB(mask_el_len, &lptr[0]))
		early_exit = 1;
	    else {
		if (mask->n_dim == 0)	/* scalar true mask	*/
		    use_mask = 0;	/* don't need to check	*/
		else
		    use_mask = 1;
	    }
	} else {
	    use_mask = 0;
	}

/*
 *	If any of our pre-defined early exit conditions have been met,
 *	we can exit now.
 */

	if (early_exit) {
	    if (scalar) {
		if (subtype == DVSUBTYPE_INT64) {
		    i8ptr2 = (_f_int8 *) rptr;
		    i8ptr2[0] = -HUGE_INT8_F90;
#ifdef _F_INT6
		} else if (subtype == DVSUBTYPE_INT46) {
		    i6ptr2 = (_f_int6 *) rptr;
		    if (init_val == USE_INT6)
			i6ptr2[0] = -HUGE_INT6_F90;
		    else if (init_val == USE_INT4)
			i6ptr2[0] = -HUGE_INT4_F90;
		    else if (init_val == USE_INT2)
			i6ptr2[0] = -HUGE_INT2_F90;
		    else
			i6ptr2[0] = -HUGE_INT1_F90;
#else
		} else if (subtype == DVSUBTYPE_INT32) {
		    i4ptr2 = (_f_int4 *) rptr;
		    if (init_val == USE_INT4)
			i4ptr2[0] = -HUGE_INT4_F90;
		    else if (init_val == USE_INT2)
			i4ptr2[0] = -HUGE_INT2_F90;
		    else
			i4ptr2[0] = -HUGE_INT1_F90;
#endif
#if defined _F_REAL16 && _F_REAL16 != (-1)
		} else if (subtype == DVSUBTYPE_REAL128) {
		    r16ptr2 = (_f_real16 *) rptr;
		    r16ptr2[0] = -HUGE_REAL16_F90;
#endif
		} else if (subtype == DVSUBTYPE_REAL32) {
		    r4ptr2 = (_f_real4 *) rptr;
		    r4ptr2[0] = -HUGE_REAL4_F90;
		} else {
		    r8ptr2 = (_f_real8 *) rptr;
		    if (init_val == USE_REAL8)
			r8ptr2[0] = -HUGE_REAL8_F90;
		    else
			r8ptr2[0] = -HUGE_REAL4_F90;
		}
	    } else {
		for (i = 0; i < rank-1; i++) {
		    res_strd[i] = result->dimension[i].stride_mult / bucketsize;
		    res_ext[i] = result->dimension[i].extent;
		    curdim[i] = 0;
		}
		rindx = 0;
		if (subtype == DVSUBTYPE_INT64) {
		    i8ptr2 = (_f_int8 *) rptr;
		    for (i = 0; i < tot_ext; i++) {
			i8ptr2[rindx] = -HUGE_INT8_F90;
			INCR_RES();
		    }
#ifdef _F_INT6
		} else if (subtype == DVSUBTYPE_INT46) {
		    i6ptr2 = (_f_int6 *) rptr;
		    if (init_val == USE_INT6)
			use_int6 = -HUGE_INT6_F90;
		    else if (init_val == USE_INT4)
			use_int6 = -HUGE_INT4_F90;
		    else if (init_val == USE_INT2)
			use_int6 = -HUGE_INT2_F90;
		    else
			use_int6 = -HUGE_INT1_F90;
		    for (i = 0; i < tot_ext; i++) {
			i6ptr2[rindx] = use_int6;
			INCR_RES();
		    }
#endif
		} else if (subtype == DVSUBTYPE_INT32) {
		    i4ptr2 = (_f_int4 *) rptr;
		    if (init_val == USE_INT4)
			use_int4 = -HUGE_INT4_F90;
		    else if (init_val == USE_INT2)
			use_int4 = -HUGE_INT2_F90;
		    else
			use_int4 = -HUGE_INT1_F90;
		    for (i = 0; i < tot_ext; i++) {
			i4ptr2[rindx] = use_int4;
			INCR_RES();
		    }
#if defined _F_REAL16 && _F_REAL16 != (-1)
		} else if (subtype == DVSUBTYPE_REAL128) {
		    r16ptr2 = (_f_real16 *) rptr;
		    for (i = 0; i < tot_ext; i++) {
			r16ptr2[rindx] = -HUGE_REAL16_F90;
			INCR_RES();
		    }
#endif
		} else if (subtype == DVSUBTYPE_REAL32) {
		    r4ptr2 = (_f_real4 *) rptr;
		    for (i = 0; i < tot_ext; i++) {
			r4ptr2[rindx] = -HUGE_REAL4_F90;
			INCR_RES();
		    }
		} else {
		    r8ptr2 = (_f_real8 *) rptr;
		    if (init_val == USE_REAL8)
			use_flt8 = -HUGE_REAL8_F90;
		    else
			use_flt8 = -HUGE_REAL4_F90;
		    for (i = 0; i < tot_ext; i++) {
			r8ptr2[rindx] = use_flt8;
			INCR_RES();
		    }
		}
	    }
	    return;
	}

	if (rank == 1) {
	    if (bucketsize > 1)
		src_strd[0] = source->dimension[0].stride_mult / bucketsize;
	    else
		src_strd[0] = source->dimension[0].stride_mult;
	    src_ext[0] = source->dimension[0].extent;
	    if (use_mask) {
		if (mask->n_dim > 0) {
		    msk_strd[0] = mask->dimension[0].stride_mult;
#ifdef	_CRAYMPP
		    if (mask_el_len == 64 && sizeof(lptr[0]) == 4)
			msk_strd[0] <<= 1;
#endif
		} else
		    msk_strd[0] = 0;
	    }

	    switch (subtype) {
#ifdef _F_INT6
		case DVSUBTYPE_INT46 :
		    i6ptr1 = (_f_int6 *) sptr;
		    i6ptr2 = (_f_int6 *) rptr;
		    if (init_val == USE_INT6)
			i6ptr2[0] = -HUGE_INT6_F90;
		    else if (init_val == USE_INT4)
			i6ptr2[0] = -HUGE_INT4_F90;
		    else if (init_val == USE_INT2)
			i6ptr2[0] = -HUGE_INT2_F90;
		    else
			i6ptr2[0] = -HUGE_INT1_F90;
		    if (use_mask) {
			lptr = (_f_mask *) mptr;
			for (i = 0; i < src_ext[0]; i++) {
			    mindx = i * msk_strd[0];
			    if (LTOB(mask_el_len, &lptr[mindx])) {
				sindx = i * src_strd[0];
				if (i6ptr1[sindx] > i6ptr2[0])
				    i6ptr2[0] = i6ptr1[sindx];
			    }
			}
		    } else {
			for (i = 0; i < src_ext[0]; i++) {
			    sindx = i * src_strd[0];
			    if (i6ptr1[sindx] > i6ptr2[0])
				i6ptr2[0] = i6ptr1[sindx];
			}
		    }
		    break;
#endif

		case DVSUBTYPE_INT32 :
		    i4ptr1 = (_f_int4 *) sptr;
		    i4ptr2 = (_f_int4 *) rptr;
		    if (use_mask) {
			lptr = (_f_mask *) mptr;
			if (init_val == USE_INT4)
			    i4ptr2[0] = -HUGE_INT4_F90;
			else if (init_val == USE_INT2)
			    i4ptr2[0] = -HUGE_INT2_F90;
			else
			    i4ptr2[0] = -HUGE_INT1_F90;
			for (i = 0; i < src_ext[0]; i++) {
			    mindx = i * msk_strd[0];
			    if (LTOB(mask_el_len, &lptr[mindx])) {
				sindx = i * src_strd[0];
				i4ptr2[0] = i4ptr1[sindx];
				break;
			    }
			}
			for ( ; i < src_ext[0]; i++) {
			    mindx = i * msk_strd[0];
			    if (LTOB(mask_el_len, &lptr[mindx])) {
				sindx = i * src_strd[0];
				if ((i4ptr1[sindx]^i4ptr2[0]) >= 0) {
				    if (i4ptr1[sindx] > i4ptr2[0])
					i4ptr2[0] = i4ptr1[sindx];
				} else {
				    if (i4ptr1[sindx] >= 0)
					i4ptr2[0] = i4ptr1[sindx];
				}
			    }
			}
		    } else {
			i4ptr2[0] = i4ptr1[0];
			for (i = 1; i < src_ext[0]; i++) {
			    sindx = i * src_strd[0];
			    if ((i4ptr1[sindx]^i4ptr2[0]) >= 0) {
				if (i4ptr1[sindx] > i4ptr2[0])
				    i4ptr2[0] = i4ptr1[sindx];
			    } else {
				if (i4ptr1[sindx] >= 0)
				    i4ptr2[0] = i4ptr1[sindx];
			    }
			}
		    }
		    break;

		case DVSUBTYPE_INT64 :
		    i8ptr1 = (_f_int8 *) sptr;
		    i8ptr2 = (_f_int8 *) rptr;
		    if (use_mask) {
			lptr = (_f_mask *) mptr;
			i8ptr2[0] = -HUGE_INT8_F90;
			for (i = 0; i < src_ext[0]; i++) {
			    mindx = i * msk_strd[0];
			    if (LTOB(mask_el_len, &lptr[mindx])) {
				sindx = i * src_strd[0];
				i8ptr2[0] = i8ptr1[sindx];
				break;
			    }
			}
			for ( ; i < src_ext[0]; i++) {
			    mindx = i * msk_strd[0];
			    if (LTOB(mask_el_len, &lptr[mindx])) {
				sindx = i * src_strd[0];
				if ((i8ptr1[sindx]^i8ptr2[0]) >= 0) {
				    if (i8ptr1[sindx] > i8ptr2[0])
					i8ptr2[0] = i8ptr1[sindx];
				} else {
				    if (i8ptr1[sindx] >= 0)
					i8ptr2[0] = i8ptr1[sindx];
				}
			    }
			}
		    } else {
			i8ptr2[0] = i8ptr1[0];
			for (i = 1; i < src_ext[0]; i++) {
			    sindx = i * src_strd[0];
			    if ((i8ptr1[sindx]^i8ptr2[0]) >= 0) {
				if (i8ptr1[sindx] > i8ptr2[0])
				    i8ptr2[0] = i8ptr1[sindx];
			    } else {
				if (i8ptr1[sindx] >= 0)
				    i8ptr2[0] = i8ptr1[sindx];
			    }
			}
		    }
		    break;

		case DVSUBTYPE_REAL64 :
		    r8ptr1 = (_f_real8 *) sptr;
		    r8ptr2 = (_f_real8 *) rptr;
		    if (init_val == USE_REAL8)
			r8ptr2[0] = -HUGE_REAL8_F90;
		    else
			r8ptr2[0] = -HUGE_REAL4_F90;
		    if (use_mask) {
			lptr = (_f_mask *) mptr;
			for (i = 0; i < src_ext[0]; i++) {
			    mindx = i * msk_strd[0];
			    if (LTOB(mask_el_len, &lptr[mindx])) {
				sindx = i * src_strd[0];
				if (r8ptr1[sindx] > r8ptr2[0])
				    r8ptr2[0] = r8ptr1[sindx];
			    }
			}
		    } else {
			for (i = 0; i < src_ext[0]; i++) {
			    sindx = i * src_strd[0];
			    if (r8ptr1[sindx] > r8ptr2[0])
				r8ptr2[0] = r8ptr1[sindx];
			}
		    }
		    break;

		case DVSUBTYPE_REAL32 :
		    r4ptr1 = (_f_real4 *) sptr;
		    r4ptr2 = (_f_real4 *) rptr;
		    r4ptr2[0] = -HUGE_REAL4_F90;
		    if (use_mask) {
			lptr = (_f_mask *) mptr;
			for (i = 0; i < src_ext[0]; i++) {
			    mindx = i * msk_strd[0];
			    if (LTOB(mask_el_len, &lptr[mindx])) {
				sindx = i * src_strd[0];
				if (r4ptr1[sindx] > r4ptr2[0])
				    r4ptr2[0] = r4ptr1[sindx];
			    }
			}
		    } else {
			for (i = 0; i < src_ext[0]; i++) {
			    sindx = i * src_strd[0];
			    if (r4ptr1[sindx] > r4ptr2[0])
				r4ptr2[0] = r4ptr1[sindx];
			}
		    }
		    break;

#if defined _F_REAL16 && _F_REAL16 != (-1)
		case DVSUBTYPE_REAL128 :
		    r16ptr1 = (_f_real16 *) sptr;
		    r16ptr2 = (_f_real16 *) rptr;
		    r16ptr2[0] = -HUGE_REAL16_F90;
		    if (use_mask) {
			lptr = (_f_mask *) mptr;
			for (i = 0; i < src_ext[0]; i++) {
			    mindx = i * msk_strd[0];
			    if (LTOB(mask_el_len, &lptr[mindx])) {
				sindx = i * src_strd[0];
				if (r16ptr1[sindx] > r16ptr2[0])
				    r16ptr2[0] = r16ptr1[sindx];
			    }
			}
		    } else {
			for (i = 0; i < src_ext[0]; i++) {
			    sindx = i * src_strd[0];
			    if (r16ptr1[sindx] > r16ptr2[0])
				r16ptr2[0] = r16ptr1[sindx];
			}
		    }
		    break;
#endif

		default :
		    _lerror (_LELVL_ABORT, FEINTDTY);
		}

	} else if (rank == 2) {
	    if (bucketsize > 1) {
		src_strd[0] = source->dimension[0].stride_mult / bucketsize;
		src_strd[1] = source->dimension[1].stride_mult / bucketsize;
	    } else {
		src_strd[0] = source->dimension[0].stride_mult;
		src_strd[1] = source->dimension[1].stride_mult;
	    }
	    src_ext[0] = source->dimension[0].extent;
	    src_ext[1] = source->dimension[1].extent;
	    if (use_mask) {
		if (mask->n_dim > 0) {
		    msk_strd[0] = mask->dimension[0].stride_mult;
		    msk_strd[1] = mask->dimension[1].stride_mult;
#ifdef	_CRAYMPP
		    if (mask_el_len == 64 && sizeof(lptr[0]) == 4) {
			msk_strd[0] <<= 1;
			msk_strd[1] <<= 1;
		    }
#endif
		    msk_ext[0] = mask->dimension[0].extent;
		    msk_ext[1] = mask->dimension[1].extent;
		} else {
		    msk_strd[0] = 0;
		    msk_strd[1] = 0;
		    msk_ext[0] = 0;
		    msk_ext[1] = 0;
		}
	    }

	    if (scalar) {
		dim = 0;
		ndim = 1;
	    } else {
		res_ext[0] = result->dimension[0].extent;
		if (bucketsize > 1)
		    res_strd[0] = result->dimension[0].stride_mult / bucketsize;
		else
		    res_strd[0] = result->dimension[0].stride_mult;
		if (dim == 0)
		    ndim = 1;
		else
		    ndim = 0;
	    }

	    switch (subtype) {
#ifdef _F_INT6
		case DVSUBTYPE_INT46 :
		    i6ptr2 = (_f_int6 *) rptr;
		    if (init_val == USE_INT6)
			cur_maxi = -HUGE_INT6_F90;
		    else if (init_val == USE_INT4)
			cur_maxi = -HUGE_INT4_F90;
		    else if (init_val == USE_INT2)
			cur_maxi = -HUGE_INT2_F90;
		    else
			cur_maxi = -HUGE_INT1_F90;
		    use_int6 = cur_maxi;
		    if (use_mask) {
			i6ptr1 = (_f_int6 *) sptr;
			lptr = (_f_mask *) mptr;
			for (i = 0; i < src_ext[ndim]; i++) {
			    indx1_src = i * src_strd[ndim];
			    indx1_msk = i * msk_strd[ndim];
			    for (j = 0; j < src_ext[dim]; j++) {
				mindx = indx1_msk + (j * msk_strd[dim]);
				if (LTOB(mask_el_len, &lptr[mindx])) {
				    sindx = indx1_src + (j * src_strd[dim]);
				    if (i6ptr1[sindx] > cur_maxi)
					cur_maxi = i6ptr1[sindx];
				}
			    }
			    if (!scalar) {
				rindx = i * res_strd[0];
				i6ptr2[rindx] = cur_maxi;
				cur_maxi = use_int6;
			    }
			}
		    } else {
			i6ptr1 = (_f_int6 *) sptr;
			for (i = 0; i < src_ext[ndim]; i++) {
			    indx1_src = i * src_strd[ndim];
			    for (j = 0; j < src_ext[dim]; j++) {
				sindx = indx1_src + (j * src_strd[dim]);
				if (i6ptr1[sindx] > cur_maxi)
				    cur_maxi = i6ptr1[sindx];
			    }
			    if (!scalar) {
				rindx = i * res_strd[0];
				i6ptr2[rindx] = cur_maxi;
				cur_maxi = use_int6;
			    }
			}
		    }
		    if (scalar)
			i6ptr2[0] = cur_maxi;
		    break;
#endif

		case DVSUBTYPE_INT32 :
		    i4ptr2 = (_f_int4 *) rptr;
		    if (init_val == USE_INT4)
			cur_maxi4 = -HUGE_INT4_F90;
		    else if (init_val == USE_INT2)
			cur_maxi4 = -HUGE_INT2_F90;
		    else
			cur_maxi4 = -HUGE_INT1_F90;
		    use_int4 = cur_maxi4;
		    if (use_mask) {
			i4ptr1 = (_f_int4 *) sptr;
			lptr = (_f_mask *) mptr;
			first = 1;
			for (i = 0; i < src_ext[ndim]; i++) {
			    indx1_src = i * src_strd[ndim];
			    indx1_msk = i * msk_strd[ndim];
			    for (j = 0; j < src_ext[dim]; j++) {
				mindx = indx1_msk + (j * msk_strd[dim]);
				if (LTOB(mask_el_len, &lptr[mindx])) {
				    sindx = indx1_src + (j * src_strd[dim]);
				    if (!first) {
					if ((i4ptr1[sindx]^cur_maxi4) >= 0) {
					    if (i4ptr1[sindx] > cur_maxi4)
						cur_maxi4 = i4ptr1[sindx];
					} else {
					    if (i4ptr1[sindx] >= 0)
						cur_maxi4 = i4ptr1[sindx];
					}
				    } else {
					cur_maxi4 = i4ptr1[sindx];
					first = 0;
				    }
				}
			    }
			    if (!scalar) {
				rindx = i * res_strd[0];
				i4ptr2[rindx] = cur_maxi4;
				cur_maxi4 = use_int4;
				first = 1;
			    }
			}
		    } else {
			i4ptr1 = (_f_int4 *) sptr;
			first = 1;
			for (i = 0; i < src_ext[ndim]; i++) {
			    indx1_src = i * src_strd[ndim];
			    for (j = 0; j < src_ext[dim]; j++) {
				sindx = indx1_src + (j * src_strd[dim]);
				if (!first) {
				    if ((i4ptr1[sindx]^cur_maxi4) >= 0) {
					if (i4ptr1[sindx] > cur_maxi4)
					    cur_maxi4 = i4ptr1[sindx];
				    } else {
					if (i4ptr1[sindx] >= 0)
					    cur_maxi4 = i4ptr1[sindx];
				    }
				} else {
				    cur_maxi4 = i4ptr1[sindx];
				    first = 0;
				}
			    }
			    if (!scalar) {
				rindx = i * res_strd[0];
				i4ptr2[rindx] = cur_maxi4;
				cur_maxi4 = use_int4;
				first = 1;
			    }
			}
		    }
		    if (scalar)
			i4ptr2[0] = cur_maxi4;
		    break;

		case DVSUBTYPE_INT64 :
		    i8ptr2 = (_f_int8 *) rptr;
		    if (use_mask) {
			cur_maxj = -HUGE_INT8_F90;
			i8ptr1 = (_f_int8 *) sptr;
			lptr = (_f_mask *) mptr;
			first = 1;
			for (i = 0; i < src_ext[ndim]; i++) {
			    indx1_src = i * src_strd[ndim];
			    indx1_msk = i * msk_strd[ndim];
			    for (j = 0; j < src_ext[dim]; j++) {
				mindx = indx1_msk + (j * msk_strd[dim]);
				if (LTOB(mask_el_len, &lptr[mindx])) {
				    sindx = indx1_src + (j * src_strd[dim]);
				    if (!first) {
					if ((i8ptr1[sindx]^cur_maxj) >= 0) {
					    if (i8ptr1[sindx] > cur_maxj)
						cur_maxj = i8ptr1[sindx];
					} else {
					    if (i8ptr1[sindx] >= 0)
						cur_maxj = i8ptr1[sindx];
					}
				    } else {
					cur_maxj = i8ptr1[sindx];
					first = 0;
				    }
				}
			    }
			    if (!scalar) {
				rindx = i * res_strd[0];
				i8ptr2[rindx] = cur_maxj;
				cur_maxj = -HUGE_INT8_F90;
				first = 1;
			    }
			}
		    } else {
			i8ptr1 = (_f_int8 *) sptr;
			first = 1;
			for (i = 0; i < src_ext[ndim]; i++) {
			    indx1_src = i * src_strd[ndim];
			    for (j = 0; j < src_ext[dim]; j++) {
				sindx = indx1_src + (j * src_strd[dim]);
				if (!first) {
				    if ((i8ptr1[sindx]^cur_maxj) >= 0) {
					if (i8ptr1[sindx] > cur_maxj)
					    cur_maxj = i8ptr1[sindx];
				    } else {
					if (i8ptr1[sindx] >= 0)
					    cur_maxj = i8ptr1[sindx];
				    }
				} else {
				    cur_maxj = i8ptr1[sindx];
				    first = 0;
				}
			    }
			    if (!scalar) {
				rindx = i * res_strd[0];
				i8ptr2[rindx] = cur_maxj;
				cur_maxj = -HUGE_INT8_F90;
				first = 1;
			    }
			}
		    }
		    if (scalar)
			i8ptr2[0] = cur_maxj;
		    break;

		case DVSUBTYPE_REAL64 :
		    r8ptr2 = (_f_real8 *) rptr;
		    if (init_val == USE_REAL8)
			cur_maxf = -HUGE_REAL8_F90;
		    else
			cur_maxf = -HUGE_REAL4_F90;
		    use_flt8 = cur_maxf;
		    if (use_mask) {
			r8ptr1 = (_f_real8 *) sptr;
			lptr = (_f_mask *) mptr;
			for (i = 0; i < src_ext[ndim]; i++) {
			    indx1_src = i * src_strd[ndim];
			    indx1_msk = i * msk_strd[ndim];
			    for (j = 0; j < src_ext[dim]; j++) {
				mindx = indx1_msk + (j * msk_strd[dim]);
				if (LTOB(mask_el_len, &lptr[mindx])) {
				    sindx = indx1_src + (j * src_strd[dim]);
				    if (r8ptr1[sindx] > cur_maxf)
					cur_maxf = r8ptr1[sindx];
				}
			    }
			    if (!scalar) {
				rindx = i * res_strd[0];
				r8ptr2[rindx] = cur_maxf;
				cur_maxf = use_flt8;
			    }
			}
		    } else {
			r8ptr1 = (_f_real8 *) sptr;
			for (i = 0; i < src_ext[ndim]; i++) {
			    indx1_src = i * src_strd[ndim];
			    for (j = 0; j < src_ext[dim]; j++) {
				sindx = indx1_src + (j * src_strd[dim]);
				if (r8ptr1[sindx] > cur_maxf)
				    cur_maxf = r8ptr1[sindx];
			    }
			    if (!scalar) {
				rindx = i * res_strd[0];
				r8ptr2[rindx] = cur_maxf;
				cur_maxf = use_flt8;
			    }
			}
		    }
		    if (scalar)
			r8ptr2[0] = cur_maxf;
		    break;

		case DVSUBTYPE_REAL32 :
		    r4ptr2 = (_f_real4 *) rptr;
		    cur_maxs4 = -HUGE_REAL4_F90;
		    if (use_mask) {
			r4ptr1 = (_f_real4 *) sptr;
			lptr = (_f_mask *) mptr;
			for (i = 0; i < src_ext[ndim]; i++) {
			    indx1_src = i * src_strd[ndim];
			    indx1_msk = i * msk_strd[ndim];
			    for (j = 0; j < src_ext[dim]; j++) {
				mindx = indx1_msk + (j * msk_strd[dim]);
				if (LTOB(mask_el_len, &lptr[mindx])) {
				    sindx = indx1_src + (j * src_strd[dim]);
				    if (r4ptr1[sindx] > cur_maxs4)
					cur_maxs4 = r4ptr1[sindx];
				}
			    }
			    if (!scalar) {
				rindx = i * res_strd[0];
				r4ptr2[rindx] = cur_maxs4;
				cur_maxs4 = -HUGE_REAL4_F90;
			    }
			}
		    } else {
			r4ptr1 = (_f_real4 *) sptr;
			for (i = 0; i < src_ext[ndim]; i++) {
			    indx1_src = i * src_strd[ndim];
			    for (j = 0; j < src_ext[dim]; j++) {
				sindx = indx1_src + (j * src_strd[dim]);
				if (r4ptr1[sindx] > cur_maxs4)
				    cur_maxs4 = r4ptr1[sindx];
			    }
			    if (!scalar) {
				rindx = i * res_strd[0];
				r4ptr2[rindx] = cur_maxs4;
				cur_maxs4 = -HUGE_REAL4_F90;
			    }
			}
		    }
		    if (scalar)
			r4ptr2[0] = cur_maxs4;
		    break;

#if defined _F_REAL16 && _F_REAL16 != (-1)
		case DVSUBTYPE_REAL128 :
		    r16ptr1 = (_f_real16 *) sptr;
		    r16ptr2 = (_f_real16 *) rptr;
		    if (mask)
			lptr = (_f_mask *) mptr;
		    cur_maxd = -HUGE_REAL16_F90;
		    for (i = 0; i < src_ext[ndim]; i++) {
			indx1_src = i * src_strd[ndim];
			indx1_msk = i * msk_strd[ndim];
			for (j = 0; j < src_ext[dim]; j++) {
			    if (use_mask) {
				mindx = indx1_msk + (j * msk_strd[dim]);
				if (LTOB(mask_el_len, &lptr[mindx])) {
				    sindx = indx1_src + (j * src_strd[dim]);
				    if (r16ptr1[sindx] > cur_maxd)
					cur_maxd = r16ptr1[sindx];
				}
			    } else {
				sindx = indx1_src + (j * src_strd[dim]);
				if (r16ptr1[sindx] > cur_maxd)
				    cur_maxd = r16ptr1[sindx];
			    }
			}
			if (!scalar) {
			    rindx = i * res_strd[0];
			    r16ptr2[rindx] = cur_maxd;
			    cur_maxd = -HUGE_REAL16_F90;
			}
		    }
		    if (scalar)
			r16ptr2[0] = cur_maxd;
		    break;
#endif

		default :
		    _lerror (_LELVL_ABORT, FEINTDTY);
	    }
	} else {
	    if (scalar) {
		dim = 0;
	    } else {
#ifdef _UNICOS
#pragma _CRI shortloop
#endif
		for (i = 0; i < rank-1; i++) {
		    res_off[i] = 0;
		    res_ext[i] = result->dimension[i].extent;
		    if (bucketsize > 1)
			res_strd[i]=result->dimension[i].stride_mult/bucketsize;
		    else
			res_strd[i] = result->dimension[i].stride_mult;
		}
	    }

	    tot_ext = 1;
	    for (i = 0; i < dim; i++) {
		src_off[i] = 0;
		src_ext[i] = source->dimension[i].extent;
		if (bucketsize > 1)
		    src_strd[i] = source->dimension[i].stride_mult / bucketsize;
		else
		    src_strd[i] = source->dimension[i].stride_mult;
		if (mask && mask->n_dim > 0) {
		    msk_ext[i] = mask->dimension[i].extent;
		    msk_strd[i] = mask->dimension[i].stride_mult;
#ifdef	_CRAYMPP
		    if (mask_el_len == 64 && sizeof(lptr[0]) == 4)
			msk_strd[i] <<= 1;
#endif
		    msk_off[i] = 0;
		} else {
		    msk_strd[i] = 0;
		    msk_off[i] = 0;
		}
		tot_ext *= src_ext[i];
	    }

	    src_dim_ext = source->dimension[dim].extent;
	    if (bucketsize > 1)
		src_dim_strd = source->dimension[dim].stride_mult / bucketsize;
	    else
		src_dim_strd = source->dimension[dim].stride_mult;
	    if (use_mask) {
		msk_dim_ext = mask->dimension[dim].extent;
		msk_dim_strd = mask->dimension[dim].stride_mult;
#ifdef	_CRAYMPP
		if (mask_el_len == 64 && sizeof(lptr[0]) == 4)
		    msk_dim_strd <<= 1;
#endif
	    }

	    for ( ; i < rank - 1; i++) {
		src_off[i] = 0;
		src_ext[i] = source->dimension[i+1].extent;
		if (bucketsize > 1)
		    src_strd[i] = source->dimension[i+1].stride_mult/bucketsize;
		else
		    src_strd[i] = source->dimension[i+1].stride_mult;
		if (mask && mask->n_dim > 1) {
		    msk_ext[i] = mask->dimension[i+1].extent;
		    msk_strd[i] = mask->dimension[i+1].stride_mult;
#ifdef	_CRAYMPP
		    if (mask_el_len == 64 && sizeof(lptr[0]) == 4)
			msk_strd[i] <<= 1;
#endif
		    msk_off[i] = 0;
		} else {
		    msk_strd[i] = 0;
		    msk_off[i] = 0;
		}
		tot_ext *= src_ext[i];
	    }

/*	Initialize curdim array		*/

	    for (i = 0; i < rank - 1; i++)
		curdim[i] = 0;

	    switch (subtype) {
#ifdef _F_INT6
		case DVSUBTYPE_INT46 :
		    i6ptr2 = (_f_int6 *) rptr;
		    if (init_val == USE_INT6)
			cur_maxi = -HUGE_INT6_F90;
		    else if (init_val == USE_INT4)
			cur_maxi = -HUGE_INT4_F90;
		    else if (init_val == USE_INT2)
			cur_maxi = -HUGE_INT2_F90;
		    else
			cur_maxi = -HUGE_INT1_F90;
		    use_int6 = cur_maxi;
		    if (use_mask) {
			i6ptr1 = (_f_int6 *) sptr;
			lptr = (_f_mask *) mptr;
			for (i = 0; i < tot_ext; i++) {
			    indx1_src = 0;
			    indx1_msk = 0;
			    for (j = 0; j < rank - 1; j++) {
				indx1_src += src_off[j];
				indx1_msk += msk_off[j];
			    }
			    for (j = 0; j < src_dim_ext; j++) {
				mindx = indx1_msk + (j * msk_dim_strd);
				if (LTOB(mask_el_len, &lptr[mindx])) {
				    sindx = indx1_src + (j * src_dim_strd);
				    if (i6ptr1[sindx] > cur_maxi)
					cur_maxi = i6ptr1[sindx];
				}
			    }
			    if (!scalar) {
				indx1_res = 0;
				for (j = 0; j < rank - 1; j++)
				    indx1_res += res_off[j];
				i6ptr2[indx1_res] = cur_maxi;
				cur_maxi = use_int6;
			    }
			    INCREMENT();
			}
		    } else {
			i6ptr1 = (_f_int6 *) sptr;
			for (i = 0; i < tot_ext; i++) {
			    indx1_src = 0;
			    for (j = 0; j < rank - 1; j++)
				indx1_src += src_off[j];
			    for (j = 0; j < src_dim_ext; j++) {
				sindx = indx1_src + (j * src_dim_strd);
				if (i6ptr1[sindx] > cur_maxi)
				    cur_maxi = i6ptr1[sindx];
			    }
			    if (!scalar) {
				indx1_res = 0;
				for (j = 0; j < rank - 1; j++)
				    indx1_res += res_off[j];
				i6ptr2[indx1_res] = cur_maxi;
				cur_maxi = use_int6;
			    }
			    INCREMENT();
			}
		    }
		    if (scalar)
			i6ptr2[0] = cur_maxi;
		    break;
#endif

		case DVSUBTYPE_INT32 :
		    i4ptr2 = (_f_int4 *) rptr;
		    if (init_val == USE_INT4)
			cur_maxi4 = -HUGE_INT4_F90;
		    else if (init_val == USE_INT2)
			cur_maxi4 = -HUGE_INT2_F90;
		    else
			cur_maxi4 = -HUGE_INT1_F90;
		    use_int4 = cur_maxi4;
		    if (use_mask) {
			i4ptr1 = (_f_int4 *) sptr;
			lptr = (_f_mask *) mptr;
			first = 1;
			for (i = 0; i < tot_ext; i++) {
			    indx1_src = 0;
			    indx1_msk = 0;
			    for (j = 0; j < rank - 1; j++) {
				indx1_src += src_off[j];
				indx1_msk += msk_off[j];
			    }
			    for (j = 0; j < src_dim_ext; j++) {
				mindx = indx1_msk + (j * msk_dim_strd);
				if (LTOB(mask_el_len, &lptr[mindx])) {
				    sindx = indx1_src + (j * src_dim_strd);
				    if (!first) {
					if ((i4ptr1[sindx]^cur_maxi4) >= 0) {
					    if (i4ptr1[sindx] > cur_maxi4)
						cur_maxi4 = i4ptr1[sindx];
					} else {
					    if (i4ptr1[sindx] >= 0)
						cur_maxi4 = i4ptr1[sindx];
					}
				    } else {
					cur_maxi4 = i4ptr1[sindx];
					first = 0;
				    }
				}
			    }
			    if (!scalar) {
				indx1_res = 0;
				for (j = 0; j < rank - 1; j++)
				    indx1_res += res_off[j];
				i4ptr2[indx1_res] = cur_maxi4;
				cur_maxi4 = use_int4;
				first = 1;
			    }
			    INCREMENT();
			}
		    } else {
			i4ptr1 = (_f_int4 *) sptr;
			first = 1;
			for (i = 0; i < tot_ext; i++) {
			    indx1_src = 0;
			    for (j = 0; j < rank - 1; j++)
				indx1_src += src_off[j];
			    for (j = 0; j < src_dim_ext; j++) {
				sindx = indx1_src + (j * src_dim_strd);
				if (!first) {
				    if ((i4ptr1[sindx]^cur_maxi4) >= 0) {
					if (i4ptr1[sindx] > cur_maxi4)
					    cur_maxi4 = i4ptr1[sindx];
				    } else {
					if (i4ptr1[sindx] >= 0)
					    cur_maxi4 = i4ptr1[sindx];
				    }
				} else {
				    cur_maxi4 = i4ptr1[sindx];
				    first = 0;
				}
			    }
			    if (!scalar) {
				indx1_res = 0;
				for (j = 0; j < rank - 1; j++)
				    indx1_res += res_off[j];
				i4ptr2[indx1_res] = cur_maxi4;
				cur_maxi4 = use_int4;
				first = 1;
			    }
			    INCREMENT();
			}
		    }
		    if (scalar)
			i4ptr2[0] = cur_maxi4;
		    break;

		case DVSUBTYPE_INT64 :
		    i8ptr2 = (_f_int8 *) rptr;
		    cur_maxj = -HUGE_INT8_F90;
		    if (use_mask) {
			i8ptr1 = (_f_int8 *) sptr;
			lptr = (_f_mask *) mptr;
			first = 1;
			for (i = 0; i < tot_ext; i++) {
			    indx1_src = 0;
			    indx1_msk = 0;
			    for (j = 0; j < rank - 1; j++) {
				indx1_src += src_off[j];
				indx1_msk += msk_off[j];
			    }
			    for (j = 0; j < src_dim_ext; j++) {
				mindx = indx1_msk + (j * msk_dim_strd);
				if (LTOB(mask_el_len, &lptr[mindx])) {
				    sindx = indx1_src + (j * src_dim_strd);
				    if (!first) {
					if ((i8ptr1[sindx]^cur_maxj) >= 0) {
					    if (i8ptr1[sindx] > cur_maxj)
						cur_maxj = i8ptr1[sindx];
					} else {
					    if (i8ptr1[sindx] >= 0)
						cur_maxj = i8ptr1[sindx];
					}
				    } else {
					cur_maxj = i8ptr1[sindx];
					first = 0;
				    }
				}
			    }
			    if (!scalar) {
				indx1_res = 0;
				for (j = 0; j < rank - 1; j++)
				    indx1_res += res_off[j];
				i8ptr2[indx1_res] = cur_maxj;
				cur_maxj = -HUGE_INT8_F90;
				first = 1;
			    }
			    INCREMENT();
			}
		    } else {
			i8ptr1 = (_f_int8 *) sptr;
			first = 1;
			for (i = 0; i < tot_ext; i++) {
			    indx1_src = 0;
			    for (j = 0; j < rank - 1; j++)
				indx1_src += src_off[j];
			    for (j = 0; j < src_dim_ext; j++) {
				sindx = indx1_src + (j * src_dim_strd);
				if (!first) {
				    if ((i8ptr1[sindx]^cur_maxj) >= 0) {
					if (i8ptr1[sindx] > cur_maxj)
					    cur_maxj = i8ptr1[sindx];
				    } else {
					if (i8ptr1[sindx] >= 0)
					    cur_maxj = i8ptr1[sindx];
				    }
				} else {
				    cur_maxj = i8ptr1[sindx];
				    first = 0;
				}
			    }
			    if (!scalar) {
				indx1_res = 0;
				for (j = 0; j < rank - 1; j++)
				    indx1_res += res_off[j];
				i8ptr2[indx1_res] = cur_maxj;
				cur_maxj = -HUGE_INT8_F90;
				first = 1;
			    }
			    INCREMENT();
			}
		    }
		    if (scalar)
			i8ptr2[0] = cur_maxj;
		    break;

		case DVSUBTYPE_REAL64 :
		    r8ptr2 = (_f_real8 *) rptr;
		    if (init_val == USE_REAL8)
			cur_maxf = -HUGE_REAL8_F90;
		    else
			cur_maxf = -HUGE_REAL4_F90;
		    use_flt8 = cur_maxf;
		    if (use_mask) {
			r8ptr1 = (_f_real8 *) sptr;
			lptr = (_f_mask *) mptr;
			for (i = 0; i < tot_ext; i++) {
			    indx1_src = 0;
			    indx1_msk = 0;
			    for (j = 0; j < rank - 1; j++) {
				indx1_src += src_off[j];
				indx1_msk += msk_off[j];
			    }
			    for (j = 0; j < src_dim_ext; j++) {
				mindx = indx1_msk + (j * msk_dim_strd);
				if (LTOB(mask_el_len, &lptr[mindx])) {
				    sindx = indx1_src + (j * src_dim_strd);
				    if (r8ptr1[sindx] > cur_maxf)
					cur_maxf = r8ptr1[sindx];
				}
			    }
			    if (!scalar) {
				indx1_res = 0;
				for (j = 0; j < rank - 1; j++)
				    indx1_res += res_off[j];
				r8ptr2[indx1_res] = cur_maxf;
				cur_maxf = use_flt8;
			    }
			    INCREMENT();
			}
		    } else {
			r8ptr1 = (_f_real8 *) sptr;
			for (i = 0; i < tot_ext; i++) {
			    indx1_src = 0;
			    for (j = 0; j < rank - 1; j++)
				indx1_src += src_off[j];
			    for (j = 0; j < src_dim_ext; j++) {
				sindx = indx1_src + (j * src_dim_strd);
				if (r8ptr1[sindx] > cur_maxf)
				    cur_maxf = r8ptr1[sindx];
			    }
			    if (!scalar) {
				indx1_res = 0;
				for (j = 0; j < rank - 1; j++)
				    indx1_res += res_off[j];
				r8ptr2[indx1_res] = cur_maxf;
				cur_maxf = use_flt8;
			    }
			    INCREMENT();
			}
		    }
		    if (scalar)
			r8ptr2[0] = cur_maxf;
		    break;

		case DVSUBTYPE_REAL32 :
		    r4ptr2 = (_f_real4 *) rptr;
		    cur_maxs4 = -HUGE_REAL4_F90;
		    if (use_mask) {
			r4ptr1 = (_f_real4 *) sptr;
			lptr = (_f_mask *) mptr;
			for (i = 0; i < tot_ext; i++) {
			    indx1_src = 0;
			    indx1_msk = 0;
			    for (j = 0; j < rank - 1; j++) {
				indx1_src += src_off[j];
				indx1_msk += msk_off[j];
			    }
			    for (j = 0; j < src_dim_ext; j++) {
				mindx = indx1_msk + (j * msk_dim_strd);
				if (LTOB(mask_el_len, &lptr[mindx])) {
				    sindx = indx1_src + (j * src_dim_strd);
				    if (r4ptr1[sindx] > cur_maxs4)
					cur_maxs4 = r4ptr1[sindx];
				}
			    }
			    if (!scalar) {
				indx1_res = 0;
				for (j = 0; j < rank - 1; j++)
				    indx1_res += res_off[j];
				r4ptr2[indx1_res] = cur_maxs4;
				cur_maxs4 = -HUGE_REAL4_F90;
			    }
			    INCREMENT();
			}
		    } else {
			r4ptr1 = (_f_real4 *) sptr;
			for (i = 0; i < tot_ext; i++) {
			    indx1_src = 0;
			    for (j = 0; j < rank - 1; j++)
				indx1_src += src_off[j];
			    for (j = 0; j < src_dim_ext; j++) {
				sindx = indx1_src + (j * src_dim_strd);
				if (r4ptr1[sindx] > cur_maxs4)
				    cur_maxs4 = r4ptr1[sindx];
			    }
			    if (!scalar) {
				indx1_res = 0;
				for (j = 0; j < rank - 1; j++)
				    indx1_res += res_off[j];
				r4ptr2[indx1_res] = cur_maxs4;
				cur_maxs4 = -HUGE_REAL4_F90;
			    }
			    INCREMENT();
			}
		    }
		    if (scalar)
			r4ptr2[0] = cur_maxs4;
		    break;

#if defined _F_REAL16 && _F_REAL16 != (-1)
		case DVSUBTYPE_REAL128 :
		    r16ptr1 = (_f_real16 *) sptr;
		    r16ptr2 = (_f_real16 *) rptr;
		    if (mask)
			lptr = (_f_mask *) mptr;
		    cur_maxd = -HUGE_REAL16_F90;
		    for (i = 0; i < tot_ext; i++) {
			indx1_src = 0;
			if (use_mask) {
			    indx1_msk = 0;
			    for (j = 0; j < rank - 1; j++) {
				indx1_src += src_off[j];
				indx1_msk += msk_off[j];
			    }
			} else {
			    for (j = 0; j < rank - 1; j++)
				indx1_src += src_off[j];
			}
			for (j = 0; j < src_dim_ext; j++) {
			    if (use_mask) {
				mindx = indx1_msk + (j * msk_dim_strd);
				if (LTOB(mask_el_len, &lptr[mindx])) {
				    sindx = indx1_src + (j * src_dim_strd);
				    if (r16ptr1[sindx] > cur_maxd)
					cur_maxd = r16ptr1[sindx];
				}
			    } else {
				sindx = indx1_src + (j * src_dim_strd);
				if (r16ptr1[sindx] > cur_maxd)
				    cur_maxd = r16ptr1[sindx];
			    }
			}
			if (!scalar) {
			    indx1_res = 0;
			    for (j = 0; j < rank - 1; j++)
				indx1_res += res_off[j];
			    r16ptr2[indx1_res] = cur_maxd;
			    cur_maxd = -HUGE_REAL16_F90;
			}
			INCREMENT();
		    }
		    if (scalar)
			r16ptr2[0] = cur_maxd;
		    break;
#endif

		default :
		    _lerror (_LELVL_ABORT, FEINTDTY);
	    }
	}
}
