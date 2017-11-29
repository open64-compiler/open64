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


static char USMID[] = "@(#) libfi/array/minloc.c	92.0	10/08/98 14:37:14";

#include <stddef.h>
#include <liberrno.h>
#include <fmath.h>
#include <cray/dopevec.h>
#include <cray/portdefs.h>
#include "arraydefs.h"

/*      _LOAD_DM_MK correctly for P.E. 3.0 and above */
#define _LOAD_DM_MK()							\
	dm = dimension;							\
	mk = mask;							\
	/* if last arg = NULL, is last-1 arg mask or dim? */		\
	if (mask == NULL) {						\
	    /* last arg = NULL, is last-1 arg mask or dim? */		\
	    if (dimension != NULL) {					\
		if (dimension->type_lens.type == DVTYPE_LOGICAL) {	\
		    /* last-1 argument is mask. */			\
		    mk = dimension;					\
		    dm = mask;						\
		}							\
	    }								\
	}

/*
 * Minloc function.  Determine the location of the first element of an array
 * having the minimum value of the elements identified by a mask variable.
 */

/* P.E. 2.0 and earlier entry points for MINLOC contain only a single
 * underbar after MINLOC and before the TYPE letter.  Only these entry
 * points may specify DVSUBTYPE_INT46 and INTEGER(KIND=6).  P.E. 3.0
 * does not have INTEGER(KIND=6).
 *
 * P.E. 3.0 uses the double underbar between MINLOC and the TYPE letter.
 * Note that we can never do a fast compare the way in which the routine
 * is written because it starts with with HUGE as the first mininum
 * value it is comparing against to find the minimum location.
 */
#ifdef _UNICOS
#pragma	_CRI duplicate _MINLOC_I4 as MINLOC_I4@
#endif
void
_MINLOC_I4(	DopeVectorType	* result,	/* integer-32		*/
		DopeVectorType	* source,
		DopeVectorType	* mask)
{
	void __minloc();
	_f_int	dimension = 0;
#ifndef	_F_INT4
	(void) __minloc (result, source, dimension, mask, DVSUBTYPE_INT46);
#else
	(void) __minloc (result, source, dimension, mask, DVSUBTYPE_INT32);
#endif
}

void
_MINLOC__I4(	DopeVectorType	* result,	/* integer-32		*/
		DopeVectorType	* source,
		DopeVectorType	* dimension,
		DopeVectorType	* mask)
{
	void __minloc();
	DopeVectorType	*dm, *mk;
	_LOAD_DM_MK();
#ifndef	_F_INT4
	(void) __minloc (result, source, dm, mk, DVSUBTYPE_INT46);
#else
	(void) __minloc (result, source, dm, mk, DVSUBTYPE_INT32);
#endif
}

void
_MINLOC_I2(	DopeVectorType	* result,	/* integer-16		*/
		DopeVectorType	* source,
		DopeVectorType	* mask)
{
	void __minloc();
	_f_int	dimension = 0;
#ifndef	_F_INT4
	(void) __minloc (result, source, dimension, mask, DVSUBTYPE_INT46);
#else
	(void) __minloc (result, source, dimension, mask, DVSUBTYPE_INT32);
#endif
}

void
_MINLOC__I2(	DopeVectorType	* result,	/* integer-16		*/
		DopeVectorType	* source,
		DopeVectorType	* dimension,
		DopeVectorType	* mask)
{
	void __minloc();
	DopeVectorType	*dm, *mk;
	_LOAD_DM_MK();
#if	defined(_F_INT2)
	(void) __minloc (result, source, dm, mk, DVSUBTYPE_BIT16);
#elif	!defined(_F_INT4)
	(void) __minloc (result, source, dm, mk, DVSUBTYPE_INT46);
#else
	(void) __minloc (result, source, dm, mk, DVSUBTYPE_INT32);
#endif
}

void
_MINLOC_I1(	DopeVectorType	* result,	/* integer-8		*/
		DopeVectorType	* source,
		DopeVectorType	* mask)
{
	void __minloc();
	_f_int	dimension = 0;
#ifndef	_F_INT4
	(void) __minloc (result, source, dimension, mask, DVSUBTYPE_INT46);
#else
	(void) __minloc (result, source, dimension, mask, DVSUBTYPE_INT32);
#endif
}

void
_MINLOC__I1(	DopeVectorType	* result,	/* integer-8		*/
		DopeVectorType	* source,
		DopeVectorType	* dimension,
		DopeVectorType	* mask)
{
	void __minloc();
	DopeVectorType	*dm, *mk;
	_LOAD_DM_MK();
#if	defined(_F_INT1)
	(void) __minloc (result, source, dm, mk, DVSUBTYPE_BIT8);
#elif	!defined(_F_INT4)
	(void) __minloc (result, source, dm, mk, DVSUBTYPE_INT46);
#else
	(void) __minloc (result, source, dm, mk, DVSUBTYPE_INT32);
#endif
}

#ifdef _UNICOS
#pragma	_CRI duplicate _MINLOC_I as MINLOC_I@
#endif
void
_MINLOC_I(	DopeVectorType	* result,	/* integer-46		*/
		DopeVectorType	* source,
		DopeVectorType	* mask)
{
	void __minloc();
	_f_int	dimension = 0;
#ifdef	_F_INT6
	(void) __minloc (result, source, dimension, mask, DVSUBTYPE_INT46);
#else
	(void) __minloc (result, source, dimension, mask, DVSUBTYPE_INT64);
#endif
}

void
_MINLOC__I(	DopeVectorType	* result,	/* integer-46		*/
		DopeVectorType	* source,
		DopeVectorType	* dimension,
		DopeVectorType	* mask)
{
	void __minloc();
	DopeVectorType	*dm, *mk;
	_LOAD_DM_MK();
#ifdef	_F_INT6
	(void) __minloc (result, source, dm, mk, DVSUBTYPE_INT46);
#else
	(void) __minloc (result, source, dm, mk, DVSUBTYPE_INT64);
#endif
}

#ifdef _UNICOS
#pragma	_CRI duplicate _MINLOC_J as MINLOC_J@
#endif
void
_MINLOC_J(	DopeVectorType	* result,	/* integer-64		*/
		DopeVectorType	* source,
		DopeVectorType	* mask)
{
	void __minloc();
	_f_int	dimension = 0;
	(void) __minloc (result, source, dimension, mask, DVSUBTYPE_INT64);
}

void
_MINLOC__J(	DopeVectorType	* result,	/* integer-64		*/
		DopeVectorType	* source,
		DopeVectorType	* dimension,
		DopeVectorType	* mask)
{
	void __minloc();
	DopeVectorType	*dm, *mk;
	_LOAD_DM_MK();
	(void) __minloc (result, source, dm, mk, DVSUBTYPE_INT64);
}

#ifdef _UNICOS
#pragma	_CRI duplicate _MINLOC_S as MINLOC_S@
#endif
void
_MINLOC_S(	DopeVectorType	* result,	/* 64-bit real		*/
		DopeVectorType	* source,
		DopeVectorType	* mask)
{
	void __minloc();
	_f_int	dimension = 0;
	(void) __minloc (result, source, dimension, mask, DVSUBTYPE_REAL64);
}

void
_MINLOC__S(	DopeVectorType	* result,	/* 64-bit real		*/
		DopeVectorType	* source,
		DopeVectorType	* dimension,
		DopeVectorType	* mask)
{
	void __minloc();
	DopeVectorType	*dm, *mk;
	_LOAD_DM_MK();
	(void) __minloc (result, source, dm, mk, DVSUBTYPE_REAL64);
}

#if defined _F_REAL16 && _F_REAL16 != (-1)
#ifdef _UNICOS
#pragma	_CRI duplicate _MINLOC_D as MINLOC_D@
#endif
void
_MINLOC_D(	DopeVectorType	* result,	/* 128-bit real		*/
		DopeVectorType	* source,
		DopeVectorType	* mask)
{
	void __minloc();
	_f_int	dimension = 0;
	(void) __minloc (result, source, dimension, mask, DVSUBTYPE_REAL128);
}

void
_MINLOC__D(	DopeVectorType	* result,	/* 128-bit real		*/
		DopeVectorType	* source,
		DopeVectorType	* dimension,
		DopeVectorType	* mask)
{
	void __minloc();
	DopeVectorType	*dm, *mk;
	_LOAD_DM_MK();
	(void) __minloc (result, source, dm, mk, DVSUBTYPE_REAL128);
}
#endif


#ifdef _UNICOS
#pragma	_CRI duplicate _MINLOC_S4 as MINLOC_S4@
#endif
void
_MINLOC_S4(	DopeVectorType	* result,	/* 32-bit real		*/
		DopeVectorType	* source,
		DopeVectorType	* mask)
{
	void __minloc();
	_f_int	dimension = 0;
#ifndef	_F_REAL4
	(void) __minloc (result, source, dimension, mask, DVSUBTYPE_REAL64);
#else
	(void) __minloc (result, source, dimension, mask, DVSUBTYPE_REAL32);
#endif
}

void
_MINLOC__S4(	DopeVectorType	* result,	/* 32-bit real		*/
		DopeVectorType	* source,
		DopeVectorType	* dimension,
		DopeVectorType	* mask)
{
	void __minloc();
	DopeVectorType	*dm, *mk;
	_LOAD_DM_MK();
#ifndef	_F_REAL4
	(void) __minloc (result, source, dm, mk, DVSUBTYPE_REAL64);
#else
	(void) __minloc (result, source, dm, mk, DVSUBTYPE_REAL32);
#endif
}

/*
 *	_SET_INDEX sets the temporary return values to the values of the
 *	current indices.
 */

#define _SET_INDEX()							\
	if (rank == 3) {						\
		rptr2 = curdim[2];					\
		rptr1 = curdim[1];					\
		rptr0 = curdim[0];					\
	} else if (rank == 4) {						\
		rptr3 = curdim[3];					\
		rptr2 = curdim[2];					\
		rptr1 = curdim[1];					\
		rptr0 = curdim[0];					\
	} else if (rank == 5) {						\
		rptr4 = curdim[4];					\
		rptr3 = curdim[3];					\
		rptr2 = curdim[2];					\
		rptr1 = curdim[1];					\
		rptr0 = curdim[0];					\
	} else if (rank == 6) {						\
		rptr5 = curdim[5];					\
		rptr4 = curdim[4];					\
		rptr3 = curdim[3];					\
		rptr2 = curdim[2];					\
		rptr1 = curdim[1];					\
		rptr0 = curdim[0];					\
	} else {							\
		rptr6 = curdim[6];					\
		rptr5 = curdim[5];					\
		rptr4 = curdim[4];					\
		rptr3 = curdim[3];					\
		rptr2 = curdim[2];					\
		rptr1 = curdim[1];					\
		rptr0 = curdim[0];					\
	}

/*
 *	_FINAL_INDEX sets the return values in the result array
 */

#ifndef __mips
#define _FINAL_INDEX()							\
	if (typeflag == 4) {						\
	    switch (rank) {						\
		case 7 :						\
		    rptr_4[6 * res_strd] = rptr6 + 1;			\
		case 6 :						\
		    rptr_4[5 * res_strd] = rptr5 + 1;			\
		case 5 :						\
		    rptr_4[4 * res_strd] = rptr4 + 1;			\
		case 4 :						\
		    rptr_4[3 * res_strd] = rptr3 + 1;			\
		default :						\
		    rptr_4[2 * res_strd] = rptr2 + 1;			\
		    rptr_4[res_strd] = rptr1 + 1;			\
		    rptr_4[0] = rptr0 + 1;				\
	    }								\
	} else {							\
	    switch (rank) {						\
		case 7 :						\
		    rptr_8[6 * res_strd] = rptr6 + 1;			\
		case 6 :						\
		    rptr_8[5 * res_strd] = rptr5 + 1;			\
		case 5 :						\
		    rptr_8[4 * res_strd] = rptr4 + 1;			\
		case 4 :						\
		    rptr_8[3 * res_strd] = rptr3 + 1;			\
		default :						\
		    rptr_8[2 * res_strd] = rptr2 + 1;			\
		    rptr_8[res_strd] = rptr1 + 1;			\
		    rptr_8[0] = rptr0 + 1;				\
	    }								\
	}
#else
#define _FINAL_INDEX()							\
	switch (typeflag) {						\
	case 4 :							\
	    switch (rank) {						\
		case 7 :						\
		    rptr_4[6 * res_strd] = rptr6 + 1;			\
		case 6 :						\
		    rptr_4[5 * res_strd] = rptr5 + 1;			\
		case 5 :						\
		    rptr_4[4 * res_strd] = rptr4 + 1;			\
		case 4 :						\
		    rptr_4[3 * res_strd] = rptr3 + 1;			\
		default :						\
		    rptr_4[2 * res_strd] = rptr2 + 1;			\
		    rptr_4[res_strd] = rptr1 + 1;			\
		    rptr_4[0] = rptr0 + 1;				\
	    }								\
	    break;							\
	case 2 :							\
	    switch (rank) {						\
		case 7 :						\
		    rptr_2[6 * res_strd] = rptr6 + 1;			\
		case 6 :						\
		    rptr_2[5 * res_strd] = rptr5 + 1;			\
		case 5 :						\
		    rptr_2[4 * res_strd] = rptr4 + 1;			\
		case 4 :						\
		    rptr_2[3 * res_strd] = rptr3 + 1;			\
		default :						\
		    rptr_2[2 * res_strd] = rptr2 + 1;			\
		    rptr_2[res_strd] = rptr1 + 1;			\
		    rptr_2[0] = rptr0 + 1;				\
	    }								\
	    break;							\
	case 1 :							\
	    switch (rank) {						\
		case 7 :						\
		    rptr_1[6 * res_strd] = rptr6 + 1;			\
		case 6 :						\
		    rptr_1[5 * res_strd] = rptr5 + 1;			\
		case 5 :						\
		    rptr_1[4 * res_strd] = rptr4 + 1;			\
		case 4 :						\
		    rptr_1[3 * res_strd] = rptr3 + 1;			\
		default :						\
		    rptr_1[2 * res_strd] = rptr2 + 1;			\
		    rptr_1[res_strd] = rptr1 + 1;			\
		    rptr_1[0] = rptr0 + 1;				\
	    }								\
	    break;							\
	case 8 :							\
	    switch (rank) {						\
		case 7 :						\
		    rptr_8[6 * res_strd] = rptr6 + 1;			\
		case 6 :						\
		    rptr_8[5 * res_strd] = rptr5 + 1;			\
		case 5 :						\
		    rptr_8[4 * res_strd] = rptr4 + 1;			\
		case 4 :						\
		    rptr_8[3 * res_strd] = rptr3 + 1;			\
		default :						\
		    rptr_8[2 * res_strd] = rptr2 + 1;			\
		    rptr_8[res_strd] = rptr1 + 1;			\
		    rptr_8[0] = rptr0 + 1;				\
	    }								\
	}
#endif

/*
 *	_INTERM_INDEX sets the return values in the result array
 *	when a DIM argument is present.
 */

#ifndef __mips
#define _INTERM_INDEX()							\
	switch (typeflag) {						\
	case 4 :							\
	    rptr_4[rindx] = res_indx1 + 1;				\
	    break;							\
	case 8 :							\
	    rptr_8[rindx] = res_indx1 + 1;				\
	}
#else
#define _INTERM_INDEX()							\
	switch (typeflag) {						\
	case 4 :							\
	    rptr_4[rindx] = res_indx1 + 1;				\
	    break;							\
	case 2 :							\
	    rptr_2[rindx] = res_indx1 + 1;				\
	    break;							\
	case 1 :							\
	    rptr_1[rindx] = res_indx1 + 1;				\
	    break;							\
	case 8 :							\
	    rptr_8[rindx] = res_indx1 + 1;				\
	}
#endif

/*
 *	_INCREMENT_ONE increments the curdim dimension counters and
 *	the source index variable for rank 3-7 arrays with no mask.
 */

#define _INCREMENT_ONE()						\
	curdim[0]++;							\
	if (curdim[0] < ext[0]) {					\
	    sindx += src_incr[0];					\
	} else {							\
	    curdim[0] = 0;						\
	    curdim[1]++;						\
	    if (curdim[1] < ext[1]) {					\
		sindx += src_incr[1];					\
	    } else {							\
		curdim[1] = 0;						\
		curdim[2]++;						\
		if (curdim[2] < ext[2]) {				\
		    sindx += src_incr[2];				\
		} else {						\
		    curdim[2] = 0;					\
		    curdim[3]++;					\
		    if (curdim[3] < ext[3]) {				\
			sindx += src_incr[3];				\
		    } else {						\
			curdim[3] = 0;					\
			curdim[4]++;					\
			if (curdim[4] < ext[4]) {			\
			    sindx += src_incr[4];			\
			} else {					\
			    curdim[4] = 0;				\
			    curdim[5]++;				\
			    if (curdim[5] < ext[5]) {			\
				sindx += src_incr[5];			\
			    } else {					\
				curdim[5] = 0;				\
				curdim[6]++;				\
				sindx += src_incr[6];			\
			    }						\
			}						\
		    }							\
		}							\
	    }								\
	}

/*
 *	_INCREMENT_TWO increments the curdim dimension counters and
 *	the source index variable for rank 3-7 arrays with mask.
 */

#define _INCREMENT_TWO()						\
	curdim[0]++;							\
	if (curdim[0] < ext[0]) {					\
	    sindx += src_incr[0];					\
	    mindx += msk_incr[0];					\
	} else {							\
	    curdim[0] = 0;						\
	    curdim[1]++;						\
	    if (curdim[1] < ext[1]) {					\
		sindx += src_incr[1];					\
		mindx += msk_incr[1];					\
	    } else {							\
		curdim[1] = 0;						\
		curdim[2]++;						\
		if (curdim[2] < ext[2]) {				\
		    sindx += src_incr[2];				\
		    mindx += msk_incr[2];				\
		} else {						\
		    curdim[2] = 0;					\
		    curdim[3]++;					\
		    if (curdim[3] < ext[3]) {				\
			sindx += src_incr[3];				\
			mindx += msk_incr[3];				\
		    } else {						\
			curdim[3] = 0;					\
			curdim[4]++;					\
			if (curdim[4] < ext[4]) {			\
			    sindx += src_incr[4];			\
			    mindx += msk_incr[4];			\
			} else {					\
			    curdim[4] = 0;				\
			    curdim[5]++;				\
			    if (curdim[5] < ext[5]) {			\
				sindx += src_incr[5];			\
				mindx += msk_incr[5];			\
			    } else {					\
				curdim[5] = 0;				\
				curdim[6]++;				\
				sindx += src_incr[6];			\
				mindx += msk_incr[6];			\
			    }						\
			}						\
		    }							\
		}							\
	    }								\
	}

/*	_INCREMENT_D_ONE increments the curdim dimension counters and
 *	the source index variable for rank 3-7 arrays with no mask and
 *	a dim argument is present.
 */

#define _INCREMENT_D_ONE()						\
	curdim[0]++;							\
	if (curdim[0] < ext[0]) {					\
	    src_off[0] = curdim[0] * src_strd[0];			\
	    res_off[0] = curdim[0] * res_strdm[0];			\
	} else {							\
	    curdim[0] = 0;						\
	    src_off[0] = 0;						\
	    res_off[0] = 0;						\
	    curdim[1]++;						\
	    if (curdim[1] < ext[1]) {					\
		src_off[1] = curdim[1] * src_strd[1];			\
		res_off[1] = curdim[1] * res_strdm[1];			\
	    } else {							\
		curdim[1] = 0;						\
		src_off[1] = 0;						\
		res_off[1] = 0;						\
		curdim[2]++;						\
		if (curdim[2] < ext[2]) {				\
		    src_off[2] = curdim[2] * src_strd[2];		\
		    res_off[2] = curdim[2] * res_strdm[2];		\
		} else {						\
		    curdim[2] = 0;					\
		    src_off[2] = 0;					\
		    res_off[2] = 0;					\
		    curdim[3]++;					\
		    if (curdim[3] < ext[3]) {				\
			src_off[3] = curdim[3] * src_strd[3];		\
			res_off[3] = curdim[3] * res_strdm[3];		\
		    } else {						\
			curdim[3] = 0;					\
			src_off[3] = 0;					\
			res_off[3] = 0;					\
			curdim[4]++;					\
			if (curdim[4] < ext[4]) {			\
			    src_off[4] = curdim[4] * src_strd[4];	\
			    res_off[4] = curdim[4] * res_strdm[4];	\
			} else {					\
			    curdim[4] = 0;				\
			    src_off[4] = 0;				\
			    res_off[4] = 0;				\
			    curdim[5]++;				\
			    if (curdim[5] < ext[5]) {			\
				src_off[5] = curdim[5] * src_strd[5];	\
				res_off[5] = curdim[5] * res_strdm[5];	\
			    } else {					\
				curdim[5] = 0;				\
				src_off[5] = 0;				\
				res_off[5] = 0;				\
				curdim[6]++;				\
				src_off[6] = curdim[6] * src_strd[6];	\
				res_off[6] = curdim[6] * res_strdm[6];	\
			    }						\
			}						\
		    }							\
		}							\
	    }								\
	}

/*
 *	_INCREMENT_D_TWO increments the curdim dimension counters and
 *	the source index variable for rank 3-7 arrays with mask and
 *	a dim argument is present.
 */

#define _INCREMENT_D_TWO()						\
	curdim[0]++;							\
	if (curdim[0] < ext[0]) {					\
	    src_off[0] = curdim[0] * src_strd[0];			\
	    msk_off[0] = curdim[0] * msk_strd[0];			\
	    res_off[0] = curdim[0] * res_strdm[0];			\
	} else {							\
	    curdim[0] = 0;						\
	    src_off[0] = 0;						\
	    msk_off[0] = 0;						\
	    res_off[0] = 0;						\
	    curdim[1]++;						\
	    if (curdim[1] < ext[1]) {					\
		src_off[1] = curdim[1] * src_strd[1];			\
		msk_off[1] = curdim[1] * msk_strd[1];			\
		res_off[1] = curdim[1] * res_strdm[1];			\
	    } else {							\
		curdim[1] = 0;						\
		src_off[1] = 0;						\
		msk_off[1] = 0;						\
		res_off[1] = 0;						\
		curdim[2]++;						\
		if (curdim[2] < ext[2]) {				\
		    src_off[2] = curdim[2] * src_strd[2];		\
		    msk_off[2] = curdim[2] * msk_strd[2];		\
		    res_off[2] = curdim[2] * res_strdm[2];		\
		} else {						\
		    curdim[2] = 0;					\
		    src_off[2] = 0;					\
		    msk_off[2] = 0;					\
		    res_off[2] = 0;					\
		    curdim[3]++;					\
		    if (curdim[3] < ext[3]) {				\
			src_off[3] = curdim[3] * src_strd[3];		\
			msk_off[3] = curdim[3] * msk_strd[3];		\
			res_off[3] = curdim[3] * res_strdm[3];		\
		    } else {						\
			curdim[3] = 0;					\
			src_off[3] = 0;					\
			msk_off[3] = 0;					\
			res_off[3] = 0;					\
			curdim[4]++;					\
			if (curdim[4] < ext[4]) {			\
			    src_off[4] = curdim[4] * src_strd[4];	\
			    msk_off[4] = curdim[4] * msk_strd[4];	\
			    res_off[4] = curdim[4] * res_strdm[4];	\
			} else {					\
			    curdim[4] = 0;				\
			    src_off[4] = 0;				\
			    msk_off[4] = 0;				\
			    res_off[4] = 0;				\
			    curdim[5]++;				\
			    if (curdim[5] < ext[5]) {			\
				src_off[5] = curdim[5] * src_strd[5];	\
				msk_off[5] = curdim[5] * msk_strd[5];	\
				res_off[5] = curdim[5] * res_strdm[5];	\
			    } else {					\
				curdim[5] = 0;				\
				src_off[5] = 0;				\
				msk_off[5] = 0;				\
				res_off[5] = 0;				\
				curdim[6]++;				\
				src_off[6] = curdim[6] * src_strd[6];	\
				msk_off[6] = curdim[6] * msk_strd[6];	\
				res_off[6] = curdim[6] * res_strdm[6];	\
			    }						\
			}						\
		    }							\
		}							\
	    }								\
	}


void
__minloc (	DopeVectorType	* result,
		DopeVectorType	* source,
		DopeVectorType	* dimension,
		DopeVectorType	* mask,
		_f_int		subtype)
{
	_f_int		* restrict rptr;	/* ptr to result array	*/
	_f_int4		* restrict rptr_4;	/* ptr to result array	*/
	_f_int8		* restrict rptr_8;	/* ptr to result array	*/
#ifdef _F_INT2
	_f_int2		* restrict rptr_2;	/* ptr to result array	*/
	_f_int2		* restrict i2ptr;	/* 16-bit integer	*/
	_f_int2		i2lval;			/* lowest int value	*/
#endif
#ifdef _F_INT1
	_f_int1		* restrict rptr_1;	/* ptr to result array	*/
	_f_int1		* restrict i1ptr;	/* 8-bit integer	*/
	_f_int1		i1lval;			/* lowest int value	*/
#endif
	_f_mask		* restrict mptr;	/* ptr to mask array	*/
	_f_int		bucketsize;		/* data element size	*/
	_f_int4		* restrict i4ptr;	/* 32-bit integer	*/
	_f_int4		i4lval;			/* lowest int value	*/
#ifdef _F_INT6
	_f_int6		* restrict i6ptr;	/* 46-bit integer	*/
	_f_int6		i6lval;			/* lowest int value	*/
#endif
	_f_int8		* restrict i8ptr;	/* 64-bit integer	*/
	_f_int8		i8lval;			/* lowest int value	*/
	_f_real8	* restrict r8ptr;	/* 64-bit float		*/
	_f_real8	r8lval;			/* lowest float value	*/
	_f_real16	* restrict r16ptr;	/* 128-bit float	*/
	_f_real16	r16lval;		/* lowest double value	*/
	_f_real4	* restrict r4ptr;	/* 32-bit real		*/
	_f_real4	r4lval;			/* lowest 32-bit value	*/
	long		nbytes;		/* # of bytes in data area	*/
	long		indx1;		/* i index value		*/
	long		sindx;		/* source index			*/
	long		mindx;		/* mask index			*/
	long		rindx;		/* result index			*/
	long		mndx1;		/* mask index counter		*/
	long		curdim[MAXDIM];	/* current indices		*/
	long		extent;		/* total extent count		*/
	long		stride;		/* stride			*/
	long		ext[MAXDIM];	/* stride by elements		*/
	long		src_strd[MAXDIM]; /* stride by elements		*/
	long		src_incr[MAXDIM]; /* increment counters		*/
	long		src_sub[MAXDIM];  /* temporary counter		*/
	long		src_off[MAXDIM];  /* source offset		*/
	long		msk_strd[MAXDIM]; /* mask stride		*/
	long		msk_incr[MAXDIM]; /* increment counters		*/
	long		msk_sub[MAXDIM];  /* temporary counter		*/
	long		msk_off[MAXDIM];  /* mask offset		*/
	long		res_strd;	/* result stride for nondim	*/
	long		res_strdm[MAXDIM]; /* result stride for dim	*/
	long		res_incr[MAXDIM]; /* result increment for dim	*/
	long		res_sub[MAXDIM];  /* result temp counter	*/
	long		res_off[MAXDIM];  /* result offset		*/
	long		msk_ext[MAXDIM];  /* mask extents		*/
	long		res_ext[MAXDIM];  /* result extents		*/
	long		src_dim_ext;	/* source extent for dimenlc	*/
	long		src_dim_strd;	/* source stride for dimenlc	*/
	long		msk_dim_ext;	/* mask extent for dimenlc	*/
	long		msk_dim_strd;	/* mask stride for dimenlc	*/
	_f_int		rank;		/* rank of source matrix	*/
	_f_int		type;		/* type of source matrix	*/
	long		tot_ext = 1;	/* total extent			*/
	long		rptr0;		/* result scalar		*/
	long		rptr1;		/* result scalar		*/
	long		rptr2;		/* result scalar		*/
	long		rptr3;		/* result scalar		*/
	long		rptr4;		/* result scalar		*/
	long		rptr5;		/* result scalar		*/
	long		rptr6;		/* result scalar		*/
	long		src_indx1;	/* temp value holder		*/
	long		msk_indx1;	/* temp value holder		*/
	long		res_indx1;	/* temp value holder		*/
	_f_int		use_mask;	/* mask use flag		*/
	long		i, j;		/* index1 variables		*/
	long		mask_el_len;
	_f_int		typeflag;	/* size of result flag		*/
	_f_int		resbucketsize;	/* result data element size	*/
	_f_int		dimenlc = 0;	/* dimension to check		*/
	_f_int		ndim;		/* nondim			*/
	_f_int		nodim;		/* no dim argument		*/

/*
 *	Set up local copies of the number of dimensions in the source
 *	array (rank) and the source array data type (type).
 */

	rank = source->n_dim;
	type = source->type_lens.type;

/*
 *	Set up typeflag for size of integer result.
 */

	typeflag = ((result->base_addr.a.el_len == 64) ? 8 :
		((result->base_addr.a.el_len == 32) ? 4 :
		((result->base_addr.a.el_len == 16) ? 2 :
		((result->base_addr.a.el_len == 8) ? 1 : -1))));

/*
 *	Set up the dim variable.  It must be decremented by one to
 *	account for the difference in reference between C and Fortran.
 */
	if (dimension != NULL && rank > 0) {
		_f_int dmintlen;
		dmintlen = dimension->type_lens.int_len >> 3;
		if (dmintlen == sizeof(_f_int8)) {
			dimenlc = *(_f_int8 *) dimension->base_addr.a.ptr;
		} else if (dmintlen == sizeof(_f_int4)) {
			dimenlc = *(_f_int4 *) dimension->base_addr.a.ptr;
		} else if (dmintlen == sizeof(_f_int2)) {
			dimenlc = *(_f_int2 *) dimension->base_addr.a.ptr;
		} else if (dmintlen == sizeof(_f_int1)) {
			dimenlc = *(_f_int1 *) dimension->base_addr.a.ptr;
		}
		dimenlc--;
		if (dimenlc < 0 || dimenlc >= rank)
			_lerror (_LELVL_ABORT, FESCIDIM);
	}

/*	Set bucket size scalar	*/

	bucketsize = source->type_lens.int_len / BITS_PER_WORD;
#if	defined(_CRAYMPP) || defined(__mips)
	if (bucketsize == 0)
	    bucketsize = 1;
#endif

/*	If necessary, fill result dope vector		*/

	if (!result->assoc) {
	    resbucketsize	= 1;
	    if (result->base_addr.a.el_len >= BITS_PER_WORD)
		resbucketsize = result->base_addr.a.el_len / BITS_PER_WORD;
#if	defined(_CRAYMPP) || defined(__mips)
	    if (resbucketsize == 0)
		resbucketsize	= 1;
#endif
	    result->base_addr.a.ptr = (void *) NULL;
	    result->orig_base = 0;
	    result->orig_size = 0;

	    tot_ext = resbucketsize;
	    nbytes = typeflag;

	    /* return a scalar only when the dim is present and rank=1 */
	    if (dimension != NULL && rank == 1)
		result->n_dim = 0;
	    else if (dimension != NULL) {
		result->n_dim = rank - 1;
		for (i = 0; i < dimenlc; i++) {
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
	    } else {
		result->dimension[0].low_bound = 1;
		result->dimension[0].extent = rank;
		/* Result rank-one array is size of source and
		 * type default integer
		 */
		result->dimension[0].stride_mult = resbucketsize;
		nbytes *= result->dimension[0].extent;
		tot_ext *= result->dimension[0].extent;
	    }

/*	allocate space	*/
	    if (nbytes > 0) {
		result->base_addr.a.ptr = (void *) malloc (nbytes);
		if (result->base_addr.a.ptr == NULL)
		    _lerror (_LELVL_ABORT, FENOMEMY);
	    }

	    result->assoc = 1;
	    result->orig_base = (void *) result->base_addr.a.ptr;
	    result->orig_size = nbytes * BITS_PER_BYTE;
	}

/*	initialize result array		*/

	switch (typeflag) {
	case 4:
	    rptr_4 = (_f_int4 *) result->base_addr.a.ptr;
	    for (i = 0; i < tot_ext; i++)
		rptr_4[i] = 0;
	    break;
#ifdef _F_INT2
	case 2:
	    rptr_2 = (_f_int2 *) result->base_addr.a.ptr;
	    for (i = 0; i < tot_ext; i++)
		rptr_2[i] = 0;
	    break;
#endif
#ifdef _F_INT1
	case 1:
	    rptr_1 = (_f_int1 *) result->base_addr.a.ptr;
	    for (i = 0; i < tot_ext; i++)
		rptr_1[i] = 0;
	    break;
#endif
	case 8:
	    rptr_8 = (_f_int8 *) result->base_addr.a.ptr;
	    for (i = 0; i < tot_ext; i++)
		rptr_8[i] = 0;
	}	/* switch typeflag */

/*
 *      If the extent field for any of the dope vectors is 0, we can
 *	exit early.
 */

#ifdef _UNICOS
#pragma	_CRI	shortloop
#endif
	for (i = 0; i < rank; i++) {
	    if (source->dimension[i].extent == 0)
		return;
	}
	if (result->assoc) {
	    if (result->n_dim != 0) {
		for (i = 0; i < result->n_dim; i++) {
		    if (result->dimension[i].extent == 0)
			return;
		}
	    }
	}
	if (mask) {
#ifdef _UNICOS
#pragma	_CRI	shortloop
#endif
	    for (i = 0; i < mask->n_dim; i++) {
		if (mask->dimension[i].extent == 0)
		    return;
	    }
	}

/*
 *	If mask is set and not scalar, set use_mask flag to true.  If it is
 *	scalar and true, all elements will pass mask check, so treat it as if no
 *	mask was given.  If it is false, all elements will fail, and return
 *	value will be all 0's.  Set them now, and return.  If no mask is
 *	specified, set use_mask to false.
 */
	if (mask) {
	    mask_el_len = mask->base_addr.a.el_len;
	    mptr = (_f_mask *) mask->base_addr.a.ptr;
	    if (mask->n_dim == 0) {
		if (LTOB(mask_el_len, &mptr[0]))
		    use_mask = 0;
		else
		    return;	/* already set to 0, just return */
	    } else
		use_mask = 1;
	} else
	    use_mask = 0;

/*	set up result stride scalar	*/

	if (resbucketsize > 1)
	    res_strd = result->dimension[0].stride_mult / resbucketsize;
	else
	    res_strd = result->dimension[0].stride_mult;

/*
 *	The program is broken down into three sections.  Arrays of rank 1,
 *	arrays of rank 2, and arrays of ranks 3-7.  Inside each section,
 *	they are broken down by data type.
 *
 *	Inside each data type area, the work is divided into two sections.
 *	If there is a mask specified, each element of the mask is checked,
 *	and for all true values, the comparable element in the source array
 *	is compared against the current minimum value.  If the source value
 *	is less, it replaces the current min value, and its index is stored
 *	in the temporary result counter.  If no mask is specified, no check
 *	of the mask is made, and all elements of the source are tested.
 *
 *	The temporary result scalars are initialized to -1.  This indicates
 *	that no minimum has yet been found.  The actual return values are
 *	set at the end of the loop.  If all values of mask are false, then
 *	these values are returned to indicate that no minimum value was
 *	found.
 */
	if (rank == 1) {

/*
 *	Stride is set up in actual words, rather than number of elements.
 *	Therefore, if type is double, we must divide the stride by two to
 *	get it to point to the number of two-word entities.
 */
	    if (bucketsize > 1)
		stride = source->dimension[0].stride_mult / bucketsize;
	    else
		stride = source->dimension[0].stride_mult;
	    extent = source->dimension[0].extent;

/*	Only initialize mask variables if a mask has been specified.	*/

	    if (use_mask) {
		mindx = 0;
		msk_strd[0] = mask->dimension[0].stride_mult;
#ifdef	_CRAYMPP
		if (mask_el_len == 64 && sizeof(mptr[0]) == 4)
		    msk_strd[0] <<= 1;
#endif
	    }

/*	Integer type		*/

	    switch (subtype) {
#ifdef _F_INT6
		case DVSUBTYPE_INT46 :
		    i6ptr = (_f_int6 *) source->base_addr.a.ptr;
		    i6lval = HUGE_INT6_F90;
		    if (use_mask) {		/* mask specified */
			rptr0 = -1;
			for (i = 0; i < extent; i++) {
			    mindx = i * msk_strd[0];
			    if (LTOB(mask_el_len, &mptr[mindx])) {
				sindx = i * stride;
				if (i6ptr[sindx] < i6lval) {
				    i6lval = i6ptr[sindx];
				    rptr0 = i;
				}
			    }
			}
		    } else {			/* no mask	*/
			rptr0 = 0;
			for (i = 0; i < extent; i++) {
			    sindx = i * stride;
			    if (i6ptr[sindx] < i6lval) {
				i6lval = i6ptr[sindx];
				rptr0 = i;
			    }
			}
		    }
		    switch (typeflag) {
		    case 4:
			rptr_4[0] = rptr0 + 1;
			break;
#ifdef _F_INT2
		    case 2:
			rptr_2[0] = rptr0 + 1;
			break;
#endif
#ifdef _F_INT1
		    case 1:
			rptr_1[0] = rptr0 + 1;
			break;
#endif
		    case 8:
			rptr_8[0] = rptr0 + 1;
		    }
		    break;
#endif

#ifdef _F_INT2
		case DVSUBTYPE_BIT16 :
		    i2ptr = (_f_int2 *) source->base_addr.a.ptr;
		    i2lval = HUGE_INT2_F90;
		    if (use_mask) {		/* mask specified */
			rptr0 = -1;
			for (i = 0; i < extent; i++) {
			    mindx = i * msk_strd[0];
			    if (LTOB(mask_el_len, &mptr[mindx])) {
				sindx = i * stride;
				if (i2ptr[sindx] < i2lval) {
				    i2lval = i2ptr[sindx];
				    rptr0 = i;
				}
			    }
			}
		    } else {			/* no mask	*/
			rptr0 = 0;
			for (i = 0; i < extent; i++) {
			    sindx = i * stride;
			    if (i2ptr[sindx] < i2lval) {
				i2lval = i2ptr[sindx];
				rptr0 = i;
			    }
			}
		    }
		    switch (typeflag) {
		    case 4:
			rptr_4[0] = rptr0 + 1;
			break;
		    case 2:
			rptr_2[0] = rptr0 + 1;
			break;
#ifdef _F_INT1
		    case 1:
			rptr_1[0] = rptr0 + 1;
			break;
#endif
		    case 8:
			rptr_8[0] = rptr0 + 1;
		    }
		    break;
#endif

#ifdef _F_INT1
		case DVSUBTYPE_BIT8 :
		    i1ptr = (_f_int1 *) source->base_addr.a.ptr;
		    i1lval = HUGE_INT1_F90;
		    if (use_mask) {		/* mask specified */
			rptr0 = -1;
			for (i = 0; i < extent; i++) {
			    mindx = i * msk_strd[0];
			    if (LTOB(mask_el_len, &mptr[mindx])) {
				sindx = i * stride;
				if (i1ptr[sindx] < i1lval) {
				    i1lval = i1ptr[sindx];
				    rptr0 = i;
				}
			    }
			}
		    } else {			/* no mask	*/
			rptr0 = 0;
			for (i = 0; i < extent; i++) {
			    sindx = i * stride;
			    if (i1ptr[sindx] < i1lval) {
				i1lval = i1ptr[sindx];
				rptr0 = i;
			    }
			}
		    }
		    switch (typeflag) {
		    case 4:
			rptr_4[0] = rptr0 + 1;
			break;
#ifdef _F_INT2
		    case 2:
			rptr_2[0] = rptr0 + 1;
			break;
#endif
		    case 1:
			rptr_1[0] = rptr0 + 1;
			break;
		    case 8:
			rptr_8[0] = rptr0 + 1;
		    }
		    break;
#endif

		case DVSUBTYPE_INT32 :
		    i4ptr = (_f_int4 *) source->base_addr.a.ptr;
		    i4lval = HUGE_INT4_F90;
		    if (use_mask) {		/* mask specified */
			rptr0 = -1;
			for (i = 0; i < extent; i++) {
			    mindx = i * msk_strd[0];
			    if (LTOB(mask_el_len, &mptr[mindx])) {
				sindx = i * stride;
				if ((i4ptr[sindx]^i4lval) >= 0) {
				    if (i4ptr[sindx] < i4lval) {
					i4lval = i4ptr[sindx];
					rptr0 = i;
				    }
				} else {
				    if (i4ptr[sindx] < 0) {
					i4lval = i4ptr[sindx];
					rptr0 = i;
				    }
				}
			    }
			}
		    } else {			    /* no mask      */
			rptr0 = 0;
			for (i = 0; i < extent; i++) {
			    sindx = i * stride;
			    if ((i4ptr[sindx]^i4lval) >= 0) {
				if (i4ptr[sindx] < i4lval) {
				    i4lval = i4ptr[sindx];
				    rptr0 = i;
				}
			    } else {
				if (i4ptr[sindx] < 0) {
				    i4lval = i4ptr[sindx];
				    rptr0 = i;
				}
			    }
			}
		    }
		    switch (typeflag) {
		    case 4:
			rptr_4[0] = rptr0 + 1;
			break;
#ifdef _F_INT2
		    case 2:
			rptr_2[0] = rptr0 + 1;
			break;
#endif
#ifdef _F_INT1
		    case 1:
			rptr_1[0] = rptr0 + 1;
			break;
#endif
		    case 8:
			rptr_8[0] = rptr0 + 1;
		    }
		    break;

		case DVSUBTYPE_INT64 :
		    i8ptr = (_f_int8 *) source->base_addr.a.ptr;
		    i8lval = HUGE_INT8_F90;
		    if (use_mask) {		/* mask specified */
			rptr0 = -1;
			for (i = 0; i < extent; i++) {
			    mindx = i * msk_strd[0];
			    if (LTOB(mask_el_len, &mptr[mindx])) {
				sindx = i * stride;
				if ((i8ptr[sindx]^i8lval) >= 0) {
				    if (i8ptr[sindx] < i8lval) {
					i8lval = i8ptr[sindx];
					rptr0 = i;
				    }
				} else {
				    if (i8ptr[sindx] < 0) {
					i8lval = i8ptr[sindx];
					rptr0 = i;
				    }
				}
			    }
			}
		    } else {			/* no mask	*/
			rptr0 = 0;
			for (i = 0; i < extent; i++) {
			    sindx = i * stride;
			    if ((i8ptr[sindx]^i8lval) >= 0) {
				if (i8ptr[sindx] < i8lval) {
				    i8lval = i8ptr[sindx];
				    rptr0 = i;
				}
			    } else {
				if (i8ptr[sindx] < 0) {
				    i8lval = i8ptr[sindx];
				    rptr0 = i;
				}
			    }
			}
		    }
		    switch (typeflag) {
		    case 4:
			rptr_4[0] = rptr0 + 1;
			break;
#ifdef _F_INT2
		    case 2:
			rptr_2[0] = rptr0 + 1;
			break;
#endif
#ifdef _F_INT1
		    case 1:
			rptr_1[0] = rptr0 + 1;
			break;
#endif
		    case 8:
			rptr_8[0] = rptr0 + 1;
		    }
		    break;

		case DVSUBTYPE_REAL64 :
		    r8ptr = (_f_real8 *) source->base_addr.a.ptr;
		    r8lval = HUGE_REAL8_F90;
		    if (use_mask) {		/* mask specified */
			rptr0 = -1;
			for (i = 0; i < extent; i++) {
			    mindx = i * msk_strd[0];
			    if (LTOB(mask_el_len, &mptr[mindx])) {
				sindx = i * stride;
				if (r8ptr[sindx] < r8lval) {
				    r8lval = r8ptr[sindx];
				    rptr0 = i;
				}
			    }
			}
		    } else {			/* no mask	*/
			rptr0 = 0;
			for (i = 0; i < extent; i++) {
			    sindx = i * stride;
			    if (r8ptr[sindx] < r8lval) {
				r8lval = r8ptr[sindx];
				rptr0 = i;
			    }
			}
		    }
		    switch (typeflag) {
		    case 4:
			rptr_4[0] = rptr0 + 1;
			break;
#ifdef _F_INT2
		    case 2:
			rptr_2[0] = rptr0 + 1;
			break;
#endif
#ifdef _F_INT1
		    case 1:
			rptr_1[0] = rptr0 + 1;
			break;
#endif
		    case 8:
			rptr_8[0] = rptr0 + 1;
		    }
		    break;

		case DVSUBTYPE_REAL32 :
		    r4ptr = (_f_real4 *) source->base_addr.a.ptr;
		    r4lval = HUGE_REAL4_F90;
		    if (use_mask) {		/* mask specified */
			rptr0 = -1;
			for (i = 0; i < extent; i++) {
			    mindx = i * msk_strd[0];
			    if (LTOB(mask_el_len, &mptr[mindx])) {
				sindx = i * stride;
				if (r4ptr[sindx] < r4lval) {
				    r4lval = r4ptr[sindx];
				    rptr0 = i;
				}
			    }
			}
		    } else {			/* no mask	*/
			rptr0 = 0;
			for (i = 0; i < extent; i++) {
			    sindx = i * stride;
			    if (r4ptr[sindx] < r4lval) {
				r4lval = r4ptr[sindx];
				rptr0 = i;
			    }
			}
		    }
		    switch (typeflag) {
		    case 4:
			rptr_4[0] = rptr0 + 1;
			break;
#ifdef _F_INT2
		    case 2:
			rptr_2[0] = rptr0 + 1;
			break;
#endif
#ifdef _F_INT1
		    case 1:
			rptr_1[0] = rptr0 + 1;
			break;
#endif
		    case 8:
			rptr_8[0] = rptr0 + 1;
		    }
		    break;

#if defined _F_REAL16 && _F_REAL16 != (-1)
		case DVSUBTYPE_REAL128 :
		    r16ptr = (_f_real16 *) source->base_addr.a.ptr;
		    r16lval = HUGE_REAL16_F90;
		    if (use_mask) {		/* mask specified */
			rptr0 = -1;
			for (i = 0; i < extent; i++) {
			    mindx = i * msk_strd[0];
			    if (LTOB(mask_el_len, &mptr[mindx])) {
				sindx = i * stride;
				if (r16ptr[sindx] < r16lval) {
				    r16lval = r16ptr[sindx];
				    rptr0 = i;
				}
			    }
			}
		    } else {			/* no mask	*/
			rptr0 = 0;
			for (i = 0; i < extent; i++) {
			    sindx = i * stride;
			    if (r16ptr[sindx] < r16lval) {
				r16lval = r16ptr[sindx];
				rptr0 = i;
			    }
			}
		    }
		    switch (typeflag) {
		    case 4:
			rptr_4[0] = rptr0 + 1;
			break;
#ifdef _F_INT2
		    case 2:
			rptr_2[0] = rptr0 + 1;
			break;
#endif
#ifdef _F_INT1
		    case 1:
			rptr_1[0] = rptr0 + 1;
			break;
#endif
		    case 8:
			rptr_8[0] = rptr0 + 1;
		    }
		    break;
#endif

		default :
		    _lerror (_LELVL_ABORT, FEINTDTY);
	    }

/*
 *	The rank 2 arrays are also broken down into each data type.  In
 *	addition, the separation is also made on whether a mask is specified.
 *
 *	The logic within the loops is similar to rank 1, except rank 2
 *	arrays have an additional loop.  The outer loop is set up on the
 *	second dimension.  The offset values for the second dimension are
 *	put into scalars, and they are used for each iteration of the inner
 *	loop.
 */

	} else if (rank == 2) {
	    indx1 = 0;
	    sindx = 0;

/*
 *	Stride is set up in actual words, rather than number of elements.
 *	Therefore, if type is double, we must divide the stride by two to
 *	get it to point to the number of two-word entities.
 */

	    ext[0] = source->dimension[0].extent;
	    ext[1] = source->dimension[1].extent;
	    if (bucketsize > 1) {
		src_strd[0] = source->dimension[0].stride_mult / bucketsize;
		src_strd[1] = source->dimension[1].stride_mult / bucketsize;
	    } else {
		src_strd[0] = source->dimension[0].stride_mult;
		src_strd[1] = source->dimension[1].stride_mult;
	    }

/*	Only intialize mask variables if a mask has been specified	*/

	    if (use_mask) {			/* if mask specified	*/
		if (mask->n_dim > 0) {
		    msk_strd[0] = mask->dimension[0].stride_mult;
		    msk_strd[1] = mask->dimension[1].stride_mult;
#ifdef	_CRAYMPP
		    if (mask_el_len == 64 && sizeof(mptr[0]) == 4) {
			msk_strd[0] <<= 1;
			msk_strd[1] <<= 1;
		    }
#endif
		} else {
		    msk_strd[0] = 0;
		    msk_strd[1] = 0;
		    msk_ext[0] = 0;
		    msk_ext[1] = 0;
		}
		mindx = 0;
		mndx1 = 0;
	    }
	    if (dimension == NULL) {
		/* no dimension argument.  dimenlc is zero.  */
		ndim = 1;
	    } else {
		if (dimenlc == 0)
		    ndim = 1;
		else
		    ndim = 0;
	    }

	    switch (subtype) {
#ifdef _F_INT6
		case DVSUBTYPE_INT46 :
		    i6ptr = (_f_int6 *) source->base_addr.a.ptr;
		    i6lval = HUGE_INT6_F90;
		    if (use_mask) {		/* mask specified	*/
			rptr0 = -1;
			rptr1 = -1;
			for (i = 0; i < ext[ndim]; i++) {
			    msk_indx1 = i * msk_strd[ndim];
			    src_indx1 = i * src_strd[ndim];
			    for (j = 0; j < ext[dimenlc]; j++) {
				mindx = msk_indx1 + (j * msk_strd[dimenlc]);
				if (LTOB(mask_el_len, &mptr[mindx])) {
				    sindx = src_indx1 + (j * src_strd[dimenlc]);
				    if (i6ptr[sindx] < i6lval) {
					rptr0 = j;
					rptr1 = i;
					i6lval = i6ptr[sindx];
				    }
				}
			    }
			    if (dimension != NULL) {
				rindx = i * res_strd;
				i6lval = HUGE_INT6_F90;
				switch (typeflag) {
				case 4:
				    rptr_4[rindx] = rptr0 + 1; /* set result */
				    break;
#ifdef _F_INT2
				case 2:
				    rptr_2[rindx] = rptr0 + 1; /* set result */
				    break;
#endif
#ifdef _F_INT1
				case 1:
				    rptr_1[rindx] = rptr0 + 1; /* set result */
				    break;
#endif
				case 8:
				    rptr_8[rindx] = rptr0 + 1; /* set result */
				}
				rptr0 = -1;
				rptr1 = -1;
			    }
			}
		    } else {			/* no mask	*/
			rptr0 = 0;
			rptr1 = 0;
			for (i = 0; i < ext[ndim]; i++) {
			    src_indx1 = i * src_strd[ndim];
			    for (j = 0; j < ext[dimenlc]; j++) {
				sindx = src_indx1 + (j * src_strd[dimenlc]);
				if (i6ptr[sindx] < i6lval) {
				    rptr0 = j;
				    rptr1 = i;
				    i6lval = i6ptr[sindx];
				}
			    }
			    if (dimension != NULL) {
				rindx = i * res_strd;
				i6lval = HUGE_INT6_F90;
				switch (typeflag) {
				case 4:
				    rptr_4[rindx] = rptr0 + 1; /* set result */
				    break;
#ifdef _F_INT2
				case 2:
				    rptr_2[rindx] = rptr0 + 1; /* set result */
				    break;
#endif
#ifdef _F_INT1
				case 1:
				    rptr_1[rindx] = rptr0 + 1; /* set result */
				    break;
#endif
				case 8:
				    rptr_8[rindx] = rptr0 + 1; /* set result */
				}
				rptr0 = 0;
				rptr1 = 0;
			    }
			}
		    }
		    if (dimension == NULL) {
			switch (typeflag) {
			case 4:
			    rptr_4[0] = rptr0 + 1; /* set result */
			    rptr_4[res_strd] = rptr1 + 1;
			    break;
#ifdef _F_INT2
			case 2:
			    rptr_2[0] = rptr0 + 1; /* set result */
			    rptr_2[res_strd] = rptr1 + 1;
			    break;
#endif
#ifdef _F_INT1
			case 1:
			    rptr_1[0] = rptr0 + 1; /* set result */
			    rptr_1[res_strd] = rptr1 + 1;
			    break;
#endif
			case 8:
			    rptr_8[0] = rptr0 + 1; /* set result */
			    rptr_8[res_strd] = rptr1 + 1;
			}
		    }
		    break;
#endif

		case DVSUBTYPE_INT32 :
		    i4ptr = (_f_int4 *) source->base_addr.a.ptr;
		    i4lval = HUGE_INT4_F90;
		    if (use_mask) {	     /* mask specified       */
			rptr0 = -1;
			rptr1 = -1;
			for (i = 0; i < ext[ndim]; i++) {
			    msk_indx1 = i * msk_strd[ndim];
			    src_indx1 = i * src_strd[ndim];
			    for (j = 0; j < ext[dimenlc]; j++) {
				mindx = msk_indx1 + (j * msk_strd[dimenlc]);
				if (LTOB(mask_el_len, &mptr[mindx])) {
				    sindx = src_indx1 + (j * src_strd[dimenlc]);
				    if ((i4ptr[sindx]^i4lval) >= 0) {
					if (i4ptr[sindx] < i4lval) {
					    rptr0 = j;
					    rptr1 = i;
					    i4lval = i4ptr[sindx];
					}
				    } else {
					if (i4ptr[sindx] < 0) {
					    rptr0 = j;
					    rptr1 = i;
					    i4lval = i4ptr[sindx];
					}
				    }
				}
			    }
			    if (dimension != NULL) {
				rindx = i * res_strd;
				i4lval = HUGE_INT4_F90;
				switch (typeflag) {
				case 4:
				    rptr_4[rindx] = rptr0 + 1; /* set result */
				    break;
#ifdef _F_INT2
				case 2:
				    rptr_2[rindx] = rptr0 + 1; /* set result */
				    break;
#endif
#ifdef _F_INT1
				case 1:
				    rptr_1[rindx] = rptr0 + 1; /* set result */
				    break;
#endif
				case 8:
				    rptr_8[rindx] = rptr0 + 1; /* set result */
				}
				rptr0 = -1;
				rptr1 = -1;
			    }
			}
		    } else {			    /* no mask      */
			rptr0 = 0;
			rptr1 = 0;
			for (i = 0; i < ext[ndim]; i++) {
			    src_indx1 = i * src_strd[ndim];
			    for (j = 0; j < ext[dimenlc]; j++) {
				sindx = src_indx1 + (j * src_strd[dimenlc]);
				if ((i4ptr[sindx]^i4lval) >= 0) {
				    if (i4ptr[sindx] < i4lval) {
					rptr0 = j;
					rptr1 = i;
					i4lval = i4ptr[sindx];
					}
				} else {
				    if (i4ptr[sindx] < 0) {
					rptr0 = j;
					rptr1 = i;
					i4lval = i4ptr[sindx];
				    }
				}
			    }
			    if (dimension != NULL) {
				rindx = i * res_strd;
				i4lval = HUGE_INT4_F90;
				switch (typeflag) {
				case 4:
				    rptr_4[rindx] = rptr0 + 1; /* set result */
				    break;
#ifdef _F_INT2
				case 2:
				    rptr_2[rindx] = rptr0 + 1; /* set result */
				    break;
#endif
#ifdef _F_INT1
				case 1:
				    rptr_1[rindx] = rptr0 + 1; /* set result */
				    break;
#endif
				case 8:
				    rptr_8[rindx] = rptr0 + 1; /* set result */
				}
				rptr0 = 0;
				rptr1 = 0;
			    }
			}
		    }
		    if (dimension == NULL) {
			switch (typeflag) {
			case 4:
			    rptr_4[0] = rptr0 + 1; /* set result */
			    rptr_4[res_strd] = rptr1 + 1;
			    break;
#ifdef _F_INT2
			case 2:
			    rptr_2[0] = rptr0 + 1; /* set result */
			    rptr_2[res_strd] = rptr1 + 1;
			    break;
#endif
#ifdef _F_INT1
			case 1:
			    rptr_1[0] = rptr0 + 1; /* set result */
			    rptr_1[res_strd] = rptr1 + 1;
			    break;
#endif
			case 8:
			    rptr_8[0] = rptr0 + 1; /* set result */
			    rptr_8[res_strd] = rptr1 + 1;
			}
		    }
		    break;

#ifdef _F_INT2
		case DVSUBTYPE_BIT16 :
		    i2ptr = (_f_int2 *) source->base_addr.a.ptr;
		    i2lval = HUGE_INT2_F90;
		    if (use_mask) {	     /* mask specified       */
			rptr0 = -1;
			rptr1 = -1;
			for (i = 0; i < ext[ndim]; i++) {
			    msk_indx1 = i * msk_strd[ndim];
			    src_indx1 = i * src_strd[ndim];
			    for (j = 0; j < ext[dimenlc]; j++) {
				mindx = msk_indx1 + (j * msk_strd[dimenlc]);
				if (LTOB(mask_el_len, &mptr[mindx])) {
				    sindx = src_indx1 + (j * src_strd[dimenlc]);
				    if ((i2ptr[sindx]^i2lval) >= 0) {
					if (i2ptr[sindx] < i2lval) {
					    rptr0 = j;
					    rptr1 = i;
					    i2lval = i2ptr[sindx];
					}
				    } else {
					if (i2ptr[sindx] < 0) {
					    rptr0 = j;
					    rptr1 = i;
					    i2lval = i2ptr[sindx];
					}
				    }
				}
			    }
			    if (dimension != NULL) {
				rindx = i * res_strd;
				i2lval = HUGE_INT2_F90;
				switch (typeflag) {
				case 4:
				    rptr_4[rindx] = rptr0 + 1; /* set result */
				    break;
				case 2:
				    rptr_2[rindx] = rptr0 + 1; /* set result */
				    break;
#ifdef _F_INT1
				case 1:
				    rptr_1[rindx] = rptr0 + 1; /* set result */
				    break;
#endif
				case 8:
				    rptr_8[rindx] = rptr0 + 1; /* set result */
				}
				rptr0 = -1;
				rptr1 = -1;
			    }
			}
		    } else {			    /* no mask      */
			rptr0 = 0;
			rptr1 = 0;
			for (i = 0; i < ext[ndim]; i++) {
			    src_indx1 = i * src_strd[ndim];
			    for (j = 0; j < ext[dimenlc]; j++) {
				sindx = src_indx1 + (j * src_strd[dimenlc]);
				if ((i2ptr[sindx]^i2lval) >= 0) {
				    if (i2ptr[sindx] < i2lval) {
					rptr0 = j;
					rptr1 = i;
					i2lval = i2ptr[sindx];
					}
				} else {
				    if (i2ptr[sindx] < 0) {
					rptr0 = j;
					rptr1 = i;
					i2lval = i2ptr[sindx];
				    }
				}
			    }
			    if (dimension != NULL) {
				rindx = i * res_strd;
				i2lval = HUGE_INT2_F90;
				switch (typeflag) {
				case 4:
				    rptr_4[rindx] = rptr0 + 1; /* set result */
				    break;
				case 2:
				    rptr_2[rindx] = rptr0 + 1; /* set result */
				    break;
#ifdef _F_INT1
				case 1:
				    rptr_1[rindx] = rptr0 + 1; /* set result */
				    break;
#endif
				case 8:
				    rptr_8[rindx] = rptr0 + 1; /* set result */
				}
				rptr0 = 0;
				rptr1 = 0;
			    }
			}
		    }
		    if (dimension == NULL) {
			switch (typeflag) {
			case 4:
			    rptr_4[0] = rptr0 + 1; /* set result */
			    rptr_4[res_strd] = rptr1 + 1;
			    break;
			case 2:
			    rptr_2[0] = rptr0 + 1; /* set result */
			    rptr_2[res_strd] = rptr1 + 1;
			    break;
#ifdef _F_INT1
			case 1:
			    rptr_1[0] = rptr0 + 1; /* set result */
			    rptr_1[res_strd] = rptr1 + 1;
			    break;
#endif
			case 8:
			    rptr_8[0] = rptr0 + 1; /* set result */
			    rptr_8[res_strd] = rptr1 + 1;
			}
		    }
		    break;
#endif

#ifdef _F_INT1
		case DVSUBTYPE_BIT8 :
		    i1ptr = (_f_int1 *) source->base_addr.a.ptr;
		    i1lval = HUGE_INT1_F90;
		    if (use_mask) {	     /* mask specified       */
			rptr0 = -1;
			rptr1 = -1;
			for (i = 0; i < ext[ndim]; i++) {
			    msk_indx1 = i * msk_strd[ndim];
			    src_indx1 = i * src_strd[ndim];
			    for (j = 0; j < ext[dimenlc]; j++) {
				mindx = msk_indx1 + (j * msk_strd[dimenlc]);
				if (LTOB(mask_el_len, &mptr[mindx])) {
				    sindx = src_indx1 + (j * src_strd[dimenlc]);
				    if ((i1ptr[sindx]^i1lval) >= 0) {
					if (i1ptr[sindx] < i1lval) {
					    rptr0 = j;
					    rptr1 = i;
					    i1lval = i1ptr[sindx];
					}
				    } else {
					if (i1ptr[sindx] < 0) {
					    rptr0 = j;
					    rptr1 = i;
					    i1lval = i1ptr[sindx];
					}
				    }
				}
			    }
			    if (dimension != NULL) {
				rindx = i * res_strd;
				i1lval = HUGE_INT1_F90;
				switch (typeflag) {
				case 4:
				    rptr_4[rindx] = rptr0 + 1; /* set result */
				    break;
				case 2:
				    rptr_2[rindx] = rptr0 + 1; /* set result */
				    break;
				case 1:
				    rptr_1[rindx] = rptr0 + 1; /* set result */
				    break;
				case 8:
				    rptr_8[rindx] = rptr0 + 1; /* set result */
				}
				rptr0 = -1;
				rptr1 = -1;
			    }
			}
		    } else {			    /* no mask      */
			rptr0 = 0;
			rptr1 = 0;
			for (i = 0; i < ext[ndim]; i++) {
			    src_indx1 = i * src_strd[ndim];
			    for (j = 0; j < ext[dimenlc]; j++) {
				sindx = src_indx1 + (j * src_strd[dimenlc]);
				if ((i1ptr[sindx]^i1lval) >= 0) {
				    if (i1ptr[sindx] < i1lval) {
					rptr0 = j;
					rptr1 = i;
					i1lval = i1ptr[sindx];
					}
				} else {
				    if (i1ptr[sindx] < 0) {
					rptr0 = j;
					rptr1 = i;
					i1lval = i1ptr[sindx];
				    }
				}
			    }
			    if (dimension != NULL) {
				rindx = i * res_strd;
				i1lval = HUGE_INT1_F90;
				switch (typeflag) {
				case 4:
				    rptr_4[rindx] = rptr0 + 1; /* set result */
				    break;
				case 2:
				    rptr_2[rindx] = rptr0 + 1; /* set result */
				    break;
				case 1:
				    rptr_1[rindx] = rptr0 + 1; /* set result */
				    break;
				case 8:
				    rptr_8[rindx] = rptr0 + 1; /* set result */
				}
				rptr0 = 0;
				rptr1 = 0;
			    }
			}
		    }
		    if (dimension == NULL) {
			switch (typeflag) {
			case 4:
			    rptr_4[0] = rptr0 + 1; /* set result */
			    rptr_4[res_strd] = rptr1 + 1;
			    break;
			case 2:
			    rptr_2[0] = rptr0 + 1; /* set result */
			    rptr_2[res_strd] = rptr1 + 1;
			    break;
			case 1:
			    rptr_1[0] = rptr0 + 1; /* set result */
			    rptr_1[res_strd] = rptr1 + 1;
			    break;
			case 8:
			    rptr_8[0] = rptr0 + 1; /* set result */
			    rptr_8[res_strd] = rptr1 + 1;
			}
		    }
		    break;
#endif

		case DVSUBTYPE_INT64 :
		    i8ptr = (_f_int8 *) source->base_addr.a.ptr;
		    i8lval = HUGE_INT8_F90;
		    if (use_mask) {		/* mask specified	*/
			rptr0 = -1;
			rptr1 = -1;
			for (i = 0; i < ext[ndim]; i++) {
			    msk_indx1 = i * msk_strd[ndim];
			    src_indx1 = i * src_strd[ndim];
			    for (j = 0; j < ext[dimenlc]; j++) {
				mindx = msk_indx1 + (j * msk_strd[dimenlc]);
				if (LTOB(mask_el_len, &mptr[mindx])) {
				    sindx = src_indx1 + (j * src_strd[dimenlc]);
				    if ((i8ptr[sindx]^i8lval) >= 0) {
					if (i8ptr[sindx] < i8lval) {
					    rptr0 = j;
					    rptr1 = i;
					    i8lval = i8ptr[sindx];
					}
				    } else {
					if (i8ptr[sindx] < 0) {
					    rptr0 = j;
					    rptr1 = i;
					    i8lval = i8ptr[sindx];
					}
				    }
				}
			    }
			    if (dimension != NULL) {
				rindx = i * res_strd;
				i8lval = HUGE_INT8_F90;
				switch (typeflag) {
				case 4:
				    rptr_4[rindx] = rptr0 + 1; /* set result */
				    break;
#ifdef _F_INT2
				case 2:
				    rptr_2[rindx] = rptr0 + 1; /* set result */
				    break;
#endif
#ifdef _F_INT1
				case 1:
				    rptr_1[rindx] = rptr0 + 1; /* set result */
				    break;
#endif
				case 8:
				    rptr_8[rindx] = rptr0 + 1; /* set result */
				}
				rptr0 = -1;
				rptr1 = -1;
			    }
			}
		    } else {				/* no mask	*/
			rptr0 = 0;
			rptr1 = 0;
			for (i = 0; i < ext[ndim]; i++) {
			    src_indx1 = i * src_strd[ndim];
			    for (j = 0; j < ext[dimenlc]; j++) {
				sindx = src_indx1 + (j * src_strd[dimenlc]);
				if ((i8ptr[sindx]^i8lval) >= 0) {
				    if (i8ptr[sindx] < i8lval) {
					rptr0 = j;
					rptr1 = i;
					i8lval = i8ptr[sindx];
					}
				} else {
				    if (i8ptr[sindx] < 0) {
					rptr0 = j;
					rptr1 = i;
					i8lval = i8ptr[sindx];
				    }
				}
			    }
			    if (dimension != NULL) {
				rindx = i * res_strd;
				i8lval = HUGE_INT8_F90;
				switch (typeflag) {
				case 4:
				    rptr_4[rindx] = rptr0 + 1; /* set result */
				    break;
#ifdef _F_INT2
				case 2:
				    rptr_2[rindx] = rptr0 + 1; /* set result */
				    break;
#endif
#ifdef _F_INT1
				case 1:
				    rptr_1[rindx] = rptr0 + 1; /* set result */
				    break;
#endif
				case 8:
				    rptr_8[rindx] = rptr0 + 1; /* set result */
				}
				rptr0 = 0;
				rptr1 = 0;
			    }
			}
		    }
		    if (dimension == NULL) {
			switch (typeflag) {
			case 4:
			    rptr_4[0] = rptr0 + 1; /* set result */
			    rptr_4[res_strd] = rptr1 + 1;
			    break;
#ifdef _F_INT2
			case 2:
			    rptr_2[0] = rptr0 + 1; /* set result */
			    rptr_2[res_strd] = rptr1 + 1;
			    break;
#endif
#ifdef _F_INT1
			case 1:
			    rptr_1[0] = rptr0 + 1; /* set result */
			    rptr_1[res_strd] = rptr1 + 1;
			    break;
#endif
			case 8:
			    rptr_8[0] = rptr0 + 1; /* set result */
			    rptr_8[res_strd] = rptr1 + 1;
			}
		    }
		    break;

		case DVSUBTYPE_REAL64 :
		    r8ptr = (_f_real8 *) source->base_addr.a.ptr;
		    r8lval = HUGE_REAL8_F90;
		    if (use_mask) {		/* mask specified	*/
			rptr0 = -1;
			rptr1 = -1;
			for (i = 0; i < ext[ndim]; i++) {
			    msk_indx1 = i * msk_strd[ndim];
			    src_indx1 = i * src_strd[ndim];
			    for (j = 0; j < ext[dimenlc]; j++) {
				mindx = msk_indx1 + (j * msk_strd[dimenlc]);
				if (LTOB(mask_el_len, &mptr[mindx])) {
				    sindx = src_indx1 + (j * src_strd[dimenlc]);
				    if (r8ptr[sindx] < r8lval) {
					rptr0 = j;
					rptr1 = i;
					r8lval = r8ptr[sindx];
				    }
				}
			    }
			    if (dimension != NULL) {
				rindx = i * res_strd;
				r8lval = HUGE_REAL8_F90;
				switch (typeflag) {
				case 4:
				    rptr_4[rindx] = rptr0 + 1; /* set result */
				    break;
#ifdef _F_INT2
				case 2:
				    rptr_2[rindx] = rptr0 + 1; /* set result */
				    break;
#endif
#ifdef _F_INT1
				case 1:
				    rptr_1[rindx] = rptr0 + 1; /* set result */
				    break;
#endif
				case 8:
				    rptr_8[rindx] = rptr0 + 1; /* set result */
				}
				rptr0 = -1;
				rptr1 = -1;
			    }
			}
		    } else {				/* no mask	*/
			rptr0 = 0;
			rptr1 = 0;
			for (i = 0; i < ext[ndim]; i++) {
			    src_indx1 = i * src_strd[ndim];
			    for (j = 0; j < ext[dimenlc]; j++) {
				sindx = src_indx1 + (j * src_strd[dimenlc]);
				if (r8ptr[sindx] < r8lval) {
				    rptr0 = j;
				    rptr1 = i;
				    r8lval = r8ptr[sindx];
				}
			    }
			    if (dimension != NULL) {
				rindx = i * res_strd;
				r8lval = HUGE_REAL8_F90;
				switch (typeflag) {
				case 4:
				    rptr_4[rindx] = rptr0 + 1; /* set result */
				    break;
#ifdef _F_INT2
				case 2:
				    rptr_2[rindx] = rptr0 + 1; /* set result */
				    break;
#endif
#ifdef _F_INT1
				case 1:
				    rptr_1[rindx] = rptr0 + 1; /* set result */
				    break;
#endif
				case 8:
				    rptr_8[rindx] = rptr0 + 1; /* set result */
				}
				rptr0 = 0;
				rptr1 = 0;
			    }
			}
		    }
		    if (dimension == NULL) {
			switch (typeflag) {
			case 4:
			    rptr_4[0] = rptr0 + 1; /* set result */
			    rptr_4[res_strd] = rptr1 + 1;
			    break;
#ifdef _F_INT2
			case 2:
			    rptr_2[0] = rptr0 + 1; /* set result */
			    rptr_2[res_strd] = rptr1 + 1;
			    break;
#endif
#ifdef _F_INT1
			case 1:
			    rptr_1[0] = rptr0 + 1; /* set result */
			    rptr_1[res_strd] = rptr1 + 1;
			    break;
#endif
			case 8:
			    rptr_8[0] = rptr0 + 1; /* set result */
			    rptr_8[res_strd] = rptr1 + 1;
			}
		    }
		    break;

		case DVSUBTYPE_REAL32 :
		    r4ptr = (_f_real4 *) source->base_addr.a.ptr;
		    r4lval = HUGE_REAL4_F90;
		    if (use_mask) {		/* mask specified	*/
			rptr0 = -1;
			rptr1 = -1;
			for (i = 0; i < ext[ndim]; i++) {
			    msk_indx1 = i * msk_strd[ndim];
			    src_indx1 = i * src_strd[ndim];
			    for (j = 0; j < ext[dimenlc]; j++) {
				mindx = msk_indx1 + (j * msk_strd[dimenlc]);
				if (LTOB(mask_el_len, &mptr[mindx])) {
				    sindx = src_indx1 + (j * src_strd[dimenlc]);
				    if (r4ptr[sindx] < r4lval) {
					rptr0 = j;
					rptr1 = i;
					r4lval = r4ptr[sindx];
				    }
				}
			    }
			    if (dimension != NULL) {
				rindx = i * res_strd;
				r4lval = HUGE_REAL4_F90;
				switch (typeflag) {
				case 4:
				    rptr_4[rindx] = rptr0 + 1; /* set result */
				    break;
#ifdef _F_INT2
				case 2:
				    rptr_2[rindx] = rptr0 + 1; /* set result */
				    break;
#endif
#ifdef _F_INT1
				case 1:
				    rptr_1[rindx] = rptr0 + 1; /* set result */
				    break;
#endif
				case 8:
				    rptr_8[rindx] = rptr0 + 1; /* set result */
				}
				rptr0 = -1;
				rptr1 = -1;
			    }
			}
		    } else {				/* no mask	*/
			rptr0 = 0;
			rptr1 = 0;
			for (i = 0; i < ext[ndim]; i++) {
			    src_indx1 = i * src_strd[ndim];
			    for (j = 0; j < ext[dimenlc]; j++) {
				sindx = src_indx1 + (j * src_strd[dimenlc]);
				if (r4ptr[sindx] < r4lval) {
				    rptr0 = j;
				    rptr1 = i;
				    r4lval = r4ptr[sindx];
				}
			    }
			    if (dimension != NULL) {
				rindx = i * res_strd;
				r4lval = HUGE_REAL4_F90;
				switch (typeflag) {
				case 4:
				    rptr_4[rindx] = rptr0 + 1; /* set result */
				    break;
#ifdef _F_INT2
				case 2:
				    rptr_2[rindx] = rptr0 + 1; /* set result */
				    break;
#endif
#ifdef _F_INT1
				case 1:
				    rptr_1[rindx] = rptr0 + 1; /* set result */
				    break;
#endif
				case 8:
				    rptr_8[rindx] = rptr0 + 1; /* set result */
				}
				rptr0 = 0;
				rptr1 = 0;
			    }
			}
		    }
		    if (dimension == NULL) {
			switch (typeflag) {
			case 4:
			    rptr_4[0] = rptr0 + 1; /* set result */
			    rptr_4[res_strd] = rptr1 + 1;
			    break;
#ifdef _F_INT2
			case 2:
			    rptr_2[0] = rptr0 + 1; /* set result */
			    rptr_2[res_strd] = rptr1 + 1;
			    break;
#endif
#ifdef _F_INT1
			case 1:
			    rptr_1[0] = rptr0 + 1; /* set result */
			    rptr_1[res_strd] = rptr1 + 1;
			    break;
#endif
			case 8:
			    rptr_8[0] = rptr0 + 1; /* set result */
			    rptr_8[res_strd] = rptr1 + 1;
			}
		    }
		    break;

#if defined _F_REAL16 && _F_REAL16 != (-1)
		case DVSUBTYPE_REAL128 :
		    r16ptr = (_f_real16 *) source->base_addr.a.ptr;
		    r16lval = HUGE_REAL16_F90;
		    if (use_mask) {		/* mask specified	*/
			rptr0 = -1;
			rptr1 = -1;
			for (i = 0; i < ext[ndim]; i++) {
			    msk_indx1 = i * msk_strd[ndim];
			    src_indx1 = i * src_strd[ndim];
			    for (j = 0; j < ext[dimenlc]; j++) {
				mindx = msk_indx1 + (j * msk_strd[dimenlc]);
				if (LTOB(mask_el_len, &mptr[mindx])) {
				    sindx = src_indx1 + (j * src_strd[dimenlc]);
				    if (r16ptr[sindx] < r16lval) {
					rptr0 = j;
					rptr1 = i;
					r16lval = r16ptr[sindx];
				    }
				}
			    }
			    if (dimension != NULL) {
				rindx = i * res_strd;
				r16lval = HUGE_REAL16_F90;
				switch (typeflag) {
				case 4:
				    rptr_4[rindx] = rptr0 + 1; /* set result */
				    break;
#ifdef _F_INT2
				case 2:
				    rptr_2[rindx] = rptr0 + 1; /* set result */
				    break;
#endif
#ifdef _F_INT1
				case 1:
				    rptr_1[rindx] = rptr0 + 1; /* set result */
				    break;
#endif
				case 8:
				    rptr_8[rindx] = rptr0 + 1; /* set result */
				}
				rptr0 = -1;
				rptr1 = -1;
			    }
			}
		    } else {				/* no mask	*/
			rptr0 = 0;
			rptr1 = 0;
			for (i = 0; i < ext[ndim]; i++) {
			    src_indx1 = i * src_strd[ndim];
			    for (j = 0; j < ext[dimenlc]; j++) {
				sindx = src_indx1 + (j * src_strd[dimenlc]);
				if (r16ptr[sindx] < r16lval) {
				    rptr0 = j;
				    rptr1 = i;
				    r16lval = r16ptr[sindx];
				}
			    }
			    if (dimension != NULL) {
				rindx = i * res_strd;
				r16lval = HUGE_REAL16_F90;
				switch (typeflag) {
				case 4:
				    rptr_4[rindx] = rptr0 + 1; /* set result */
				    break;
#ifdef _F_INT2
				case 2:
				    rptr_2[rindx] = rptr0 + 1; /* set result */
				    break;
#endif
#ifdef _F_INT1
				case 1:
				    rptr_1[rindx] = rptr0 + 1; /* set result */
				    break;
#endif
				case 8:
				    rptr_8[rindx] = rptr0 + 1; /* set result */
				}
				rptr0 = 0;
				rptr1 = 0;
			    }
			}
		    }
		    if (dimension == NULL) {
			switch (typeflag) {
			case 4:
			    rptr_4[0] = rptr0 + 1; /* set result */
			    rptr_4[res_strd] = rptr1 + 1;
			    break;
#ifdef _F_INT2
			case 2:
			    rptr_2[0] = rptr0 + 1; /* set result */
			    rptr_2[res_strd] = rptr1 + 1;
			    break;
#endif
#ifdef _F_INT1
			case 1:
			    rptr_1[0] = rptr0 + 1; /* set result */
			    rptr_1[res_strd] = rptr1 + 1;
			    break;
#endif
			case 8:
			    rptr_8[0] = rptr0 + 1; /* set result */
			    rptr_8[res_strd] = rptr1 + 1;
			}
		    }
		    break;
#endif

		default :
		    _lerror (_LELVL_ABORT, FEINTDTY);
	    }

/*
 *	Rank 3-7 arrays are all folded into one section for processing.
 *	These are also broken down by data type.
 *
 *	The processing for these ranks is all done in one loop.  The number of
 *	words added to the base address for each dimension are stored in an
 *	array.  There is one of these arrays for source (src_off), mask
 *	(msk_off).  These offsets are added in a macro before the values are
 *	referenced.  For each iteration, another macro is called to increment
 *	all of the indices.  At the conclusion of the loop, the return values
 *	are put into the result array.
 */

	} else {

/*	Initialize the curdim array		*/
	    for (i = 0; i < MAXDIM; i++)
		curdim[i] = 0;

/*	Initialize arrays used in block	for DIM argument entry. */

	  if (dimension != NULL) {
	    src_dim_ext = source->dimension[dimenlc].extent;
	    if (bucketsize > 1)
		src_dim_strd = source->dimension[dimenlc].stride_mult / bucketsize;
	    else
		src_dim_strd = source->dimension[dimenlc].stride_mult;
	    if (use_mask) {
		msk_dim_ext = mask->dimension[dimenlc].extent;
		msk_dim_strd = mask->dimension[dimenlc].stride_mult;
#ifdef  _CRAYMPP
		if (mask_el_len == 64 && sizeof(mptr[0]) == 4)
		    msk_dim_strd <<= 1;
#endif
	    }
	    for (i = 0; i < rank-1; i++) {
		res_ext[i] = result->dimension[i].extent;
		res_off[i] = 0;
		if (resbucketsize > 1)
		    res_strdm[i] = result->dimension[i].stride_mult / resbucketsize;
		else
		    res_strdm[i] = result->dimension[i].stride_mult;
	    }
	    for (i = 0, tot_ext = 1; i < dimenlc; i++) {
		if (bucketsize > 1)
		    src_strd[i] = source->dimension[i].stride_mult / bucketsize;
		else
		    src_strd[i] = source->dimension[i].stride_mult;
		ext[i] = source->dimension[i].extent;
		src_off[i] = 0;
		tot_ext *= ext[i];
	    }
	    for ( ; i < rank - 1; i++) {
		src_off[i] = 0;
		ext[i] = source->dimension[i+1].extent;
		if (bucketsize > 1)
		    src_strd[i] = source->dimension[i+1].stride_mult / bucketsize;
		else
		    src_strd[i] = source->dimension[i+1].stride_mult;
		tot_ext *= ext[i];
	    }

	    for ( ; i < MAXDIM; i++)
		ext[i] = 0;
	    if (use_mask) {
		for (i = 0; i < dimenlc; i++) {
		    msk_strd[i] = mask->dimension[i].stride_mult;
#ifdef	_CRAYMPP
		    if (mask_el_len == 64 && sizeof(mptr[0]) == 4)
			msk_strd[i] <<= 1;
#endif
		    msk_off[i] = 0;
		}

		for ( ; i < rank - 1; i++) {
		    msk_strd[i] = mask->dimension[i+1].stride_mult;
#ifdef	_CRAYMPP
		    if (mask_el_len == 64 && sizeof(mptr[0]) == 4)
			msk_strd[i] <<= 1;
#endif
		    msk_off[i] = 0;
		}
	    }
	  } else {		/* non-DIM entry */
	    for ( i = 0, tot_ext = 1; i < rank; i++) {
		ext[i] = source->dimension[i].extent;
		if (bucketsize > 1)
		    src_strd[i] = source->dimension[i].stride_mult / bucketsize;
		else
		    src_strd[i] = source->dimension[i].stride_mult;
		tot_ext *= ext[i];
	    }
	    for ( ; i < MAXDIM; i++)
		ext[i] = 0;
	    src_sub[0] = 0;
	    src_incr[0] = src_strd[0];
	    for (i = 1; i < rank; i++) {
		src_sub[i] = src_sub[i-1] + ((ext[i-1] - 1) * src_strd[i-1]);
		src_incr[i] = src_strd[i] - src_sub[i];
	    }
	    for ( ; i < MAXDIM; i++)
		src_incr[i] = 0;
	    if (use_mask) {
		for ( i = 0; i < rank; i++) {
		    msk_strd[i] = mask->dimension[i].stride_mult;
#ifdef	_CRAYMPP
		    if (mask_el_len == 64 && sizeof(mptr[0]) == 4)
			msk_strd[i] <<= 1;
#endif
		}
		msk_sub[0] = 0;
		msk_incr[0] = msk_strd[0];
		for (i = 1; i < rank; i++) {
		    msk_sub[i] = msk_sub[i-1] + ((ext[i-1] - 1) * msk_strd[i-1]);
		    msk_incr[i] = msk_strd[i] - msk_sub[i];
		}
		for ( ; i < MAXDIM; i++)
		    msk_incr[i] = 0;
	      }
	    }

/*	initialize result scalars for no dim argument. */

	    if (dimension == NULL) {
		if (use_mask) {
		    rptr0 = -1;
		    rptr1 = -1;
		    rptr2 = -1;
		    rptr3 = -1;
		    rptr4 = -1;
		    rptr5 = -1;
		    rptr6 = -1;
		} else {
		    rptr0 = 0;
		    rptr1 = 0;
		    rptr2 = 0;
		    rptr3 = 0;
		    rptr4 = 0;
		    rptr5 = 0;
		    rptr6 = 0;
		}
	    }

	    switch (subtype) {
#ifdef _F_INT6
		case DVSUBTYPE_INT46 :
		    i6ptr = (_f_int6 *) source->base_addr.a.ptr;
		    i6lval = HUGE_INT6_F90;
		    if (dimension == NULL) {
			sindx = 0;
			if (use_mask) {		/* mask specified	*/
			    mindx = 0;
			    for (i = 0; i < tot_ext; i++) {
				if (LTOB(mask_el_len, &mptr[mindx])) {
				    if (i6ptr[sindx] < i6lval) {
					i6lval = i6ptr[sindx];
					_SET_INDEX();
				    }
				}
				_INCREMENT_TWO();
			    }
			} else {
			    for (i = 0; i < tot_ext; i++) {
				if (i6ptr[sindx] < i6lval) {
				    i6lval = i6ptr[sindx];
				    _SET_INDEX();
				}
				_INCREMENT_ONE();
			    }
			}
			_FINAL_INDEX();	/* put temp values into result	*/
		    } else {
			if (use_mask) {		/* mask specified	*/
			  for (i = 0; i < tot_ext; i++) {
			    src_indx1 = 0;
			    msk_indx1 = 0;
			    res_indx1 = -1;
			    for (j = 0; j < rank - 1; j++) {
				src_indx1 += src_off[j];
				msk_indx1 += msk_off[j];
			    }
			    for (j = 0; j < src_dim_ext; j++) {
				mindx = msk_indx1 + (j * msk_dim_strd);
				if (LTOB(mask_el_len, &mptr[mindx])) {
				    sindx = src_indx1 + (j * src_dim_strd);
				    if (i6ptr[sindx] < i6lval) {
					i6lval = i6ptr[sindx];
					res_indx1 = j;
				    }
				}
			    }
			    rindx = 0;
			    for (j = 0; j < rank - 1; j++)
				rindx += res_off[j];
			    _INTERM_INDEX();
			    i6lval = HUGE_INT6_F90;
			    _INCREMENT_D_TWO();	/* increment indices	*/
			}
		    } else {
			for (i = 0; i < tot_ext; i++) {
			    src_indx1 = 0;
			    res_indx1 = -1;
			    for (j = 0; j < rank - 1; j++)
				src_indx1 += src_off[j];
			    for (j = 0; j < src_dim_ext; j++) {
				sindx = src_indx1 + (j * src_dim_strd);
				if (i6ptr[sindx] < i6lval) {
				    i6lval = i6ptr[sindx];
				    res_indx1 = j;
				}
			    }
			    rindx = 0;
			    for (j = 0; j < rank - 1; j++)
				rindx += res_off[j];
			    _INTERM_INDEX();
			    i6lval = HUGE_INT6_F90;
			    _INCREMENT_D_ONE();
			}
		      }
		    }
		    break;
#endif

		case DVSUBTYPE_INT32 :
		    i4ptr = (_f_int4 *) source->base_addr.a.ptr;
		    i4lval = HUGE_INT4_F90;
		    if (dimension == NULL) {
			sindx = 0;
			if (use_mask) {		/* mask specified	*/
			    mindx = 0;
			    for (i = 0; i < tot_ext; i++) {
				if (LTOB(mask_el_len, &mptr[mindx])) {
				    if ((i4ptr[sindx]^i4lval) >= 0) {
					if (i4ptr[sindx] < i4lval) {
					    i4lval = i4ptr[sindx];
					    _SET_INDEX();
					}
				    } else {
					if (i4ptr[sindx] < 0) {
					    i4lval = i4ptr[sindx];
					    _SET_INDEX();
					}
				    }
				}
				_INCREMENT_TWO();
			    }
			} else {
			    for (i = 0; i < tot_ext; i++) {
				if ((i4ptr[sindx]^i4lval) >= 0) {
				    if (i4ptr[sindx] < i4lval) {
					i4lval = i4ptr[sindx];
					_SET_INDEX();
				    }
				} else {
				    if (i4ptr[sindx] < 0) {
					i4lval = i4ptr[sindx];
					_SET_INDEX();
				    }
				}
				_INCREMENT_ONE();
			    }
			}
			_FINAL_INDEX();	/* put temp values into result	*/
		    } else {
			if (use_mask) {		/* mask specified	*/
			  for (i = 0; i < tot_ext; i++) {
			    src_indx1 = 0;
			    msk_indx1 = 0;
			    res_indx1 = -1;
			    for (j = 0; j < rank - 1; j++) {
				src_indx1 += src_off[j];
				msk_indx1 += msk_off[j];
			    }
			    for (j = 0; j < src_dim_ext; j++) {
				mindx = msk_indx1 + (j * msk_dim_strd);
				if (LTOB(mask_el_len, &mptr[mindx])) {
				    sindx = src_indx1 + (j * src_dim_strd);
				    if ((i4ptr[sindx]^i4lval) >= 0) {
					if (i4ptr[sindx] < i4lval) {
					    i4lval = i4ptr[sindx];
					    res_indx1 = j;
					}
				    } else {
					if (i4ptr[sindx] < 0) {
					    i4lval = i4ptr[sindx];
					    res_indx1 = j;
					}
				    }
				}
			    }
			    rindx = 0;
			    for (j = 0; j < rank - 1; j++)
				rindx += res_off[j];
			    _INTERM_INDEX();
			    i4lval = HUGE_INT4_F90;
			    _INCREMENT_D_TWO();	/* increment indices */
			}
		    } else {
			for (i = 0; i < tot_ext; i++) {
			    src_indx1 = 0;
			    res_indx1 = -1;
			    for (j = 0; j < rank - 1; j++)
				src_indx1 += src_off[j];
			    for (j = 0; j < src_dim_ext; j++) {
				sindx = src_indx1 + (j * src_dim_strd);
				if ((i4ptr[sindx]^i4lval) >= 0) {
				    if (i4ptr[sindx] < i4lval) {
					i4lval = i4ptr[sindx];
					res_indx1 = j;
				    }
				} else {
				    if (i4ptr[sindx] < 0) {
					i4lval = i4ptr[sindx];
					res_indx1 = j;
				    }
				}
			    }
			    rindx = 0;
			    for (j = 0; j < rank - 1; j++)
				rindx += res_off[j];
			    _INTERM_INDEX();
			    i4lval = HUGE_INT4_F90;
			    _INCREMENT_D_ONE();
			}
		      }
		    }
		    break;

#ifdef _F_INT2
		case DVSUBTYPE_BIT16 :
		    i2ptr = (_f_int2 *) source->base_addr.a.ptr;
		    i2lval = HUGE_INT2_F90;
		    if (dimension == NULL) {
			sindx = 0;
			if (use_mask) {		/* mask specified	*/
			    mindx = 0;
			    for (i = 0; i < tot_ext; i++) {
				if (LTOB(mask_el_len, &mptr[mindx])) {
				    if ((i2ptr[sindx]^i2lval) >= 0) {
					if (i2ptr[sindx] < i2lval) {
					    i2lval = i2ptr[sindx];
					    _SET_INDEX();
					}
				    } else {
					if (i2ptr[sindx] < 0) {
					    i2lval = i2ptr[sindx];
					    _SET_INDEX();
					}
				    }
				}
				_INCREMENT_TWO();
			    }
			} else {
			    for (i = 0; i < tot_ext; i++) {
				if ((i2ptr[sindx]^i2lval) >= 0) {
				    if (i2ptr[sindx] < i2lval) {
					i2lval = i2ptr[sindx];
					_SET_INDEX();
				    }
				} else {
				    if (i2ptr[sindx] < 0) {
					i2lval = i2ptr[sindx];
					_SET_INDEX();
				    }
				}
				_INCREMENT_ONE();
			    }
			}
			_FINAL_INDEX();	/* put temp values into result	*/
		    } else {
			if (use_mask) {	     /* mask specified       */
			  for (i = 0; i < tot_ext; i++) {
			    src_indx1 = 0;
			    msk_indx1 = 0;
			    res_indx1 = -1;
			    for (j = 0; j < rank - 1; j++) {
				src_indx1 += src_off[j];
				msk_indx1 += msk_off[j];
			    }
			    for (j = 0; j < src_dim_ext; j++) {
				mindx = msk_indx1 + (j * msk_dim_strd);
				if (LTOB(mask_el_len, &mptr[mindx])) {
				    sindx = src_indx1 + (j * src_dim_strd);
				    if ((i2ptr[sindx]^i2lval) >= 0) {
					if (i2ptr[sindx] < i2lval) {
					    i2lval = i2ptr[sindx];
					    res_indx1 = j;
					}
				    } else {
					if (i2ptr[sindx] < 0) {
					    i2lval = i2ptr[sindx];
					    res_indx1 = j;
					}
				    }
				}
			    }
			    rindx = 0;
			    for (j = 0; j < rank - 1; j++)
				rindx += res_off[j];
			    _INTERM_INDEX();
			    i2lval = HUGE_INT2_F90;
			    _INCREMENT_D_TWO();    /* increment indices */
			}
		    } else {
			for (i = 0; i < tot_ext; i++) {
			    src_indx1 = 0;
			    res_indx1 = -1;
			    for (j = 0; j < rank - 1; j++)
				src_indx1 += src_off[j];
			    for (j = 0; j < src_dim_ext; j++) {
				sindx = src_indx1 + (j * src_dim_strd);
				if ((i2ptr[sindx]^i2lval) >= 0) {
				    if (i2ptr[sindx] < i2lval) {
					i2lval = i2ptr[sindx];
					res_indx1 = j;
				    }
				} else {
				    if (i2ptr[sindx] < 0) {
					i2lval = i2ptr[sindx];
					res_indx1 = j;
				    }
				}
			    }
			    rindx = 0;
			    for (j = 0; j < rank - 1; j++)
				rindx += res_off[j];
			    _INTERM_INDEX();
			    i2lval = HUGE_INT2_F90;
			    _INCREMENT_D_ONE();
			}
		      }
		    }
		    break;
#endif

#ifdef _F_INT1
		case DVSUBTYPE_BIT8 :
		    i1ptr = (_f_int1 *) source->base_addr.a.ptr;
		    i1lval = HUGE_INT1_F90;
		    if (dimension == NULL) {
			sindx = 0;
			if (use_mask) {		/* mask specified	*/
			    mindx = 0;
			    for (i = 0; i < tot_ext; i++) {
				if (LTOB(mask_el_len, &mptr[mindx])) {
				    if ((i1ptr[sindx]^i1lval) >= 0) {
					if (i1ptr[sindx] < i1lval) {
					    i1lval = i1ptr[sindx];
					    _SET_INDEX();
					}
				    } else {
					if (i1ptr[sindx] < 0) {
					    i1lval = i1ptr[sindx];
					    _SET_INDEX();
					}
				    }
				}
				_INCREMENT_TWO();
			    }
			} else {
			    for (i = 0; i < tot_ext; i++) {
				if ((i1ptr[sindx]^i1lval) >= 0) {
				    if (i1ptr[sindx] < i1lval) {
					i1lval = i1ptr[sindx];
					_SET_INDEX();
				    }
				} else {
				    if (i1ptr[sindx] < 0) {
					i1lval = i1ptr[sindx];
					_SET_INDEX();
				    }
				}
				_INCREMENT_ONE();
			    }
			}
			_FINAL_INDEX();	/* put temp values into result	*/
		    } else {
			if (use_mask) {	     /* mask specified       */
			  for (i = 0; i < tot_ext; i++) {
			    src_indx1 = 0;
			    msk_indx1 = 0;
			    res_indx1 = -1;
			    for (j = 0; j < rank - 1; j++) {
				src_indx1 += src_off[j];
				msk_indx1 += msk_off[j];
			    }
			    for (j = 0; j < src_dim_ext; j++) {
				mindx = msk_indx1 + (j * msk_dim_strd);
				if (LTOB(mask_el_len, &mptr[mindx])) {
				    sindx = src_indx1 + (j * src_dim_strd);
				    if ((i1ptr[sindx]^i1lval) >= 0) {
					if (i1ptr[sindx] < i1lval) {
					    i1lval = i1ptr[sindx];
					    res_indx1 = j;
					}
				    } else {
					if (i1ptr[sindx] < 0) {
					    i1lval = i1ptr[sindx];
					    res_indx1 = j;
					}
				    }
				}
			    }
			    rindx = 0;
			    for (j = 0; j < rank - 1; j++)
				rindx += res_off[j];
			    _INTERM_INDEX();
			    i1lval = HUGE_INT1_F90;
			    _INCREMENT_D_TWO();    /* increment indices */
			}
		    } else {
			for (i = 0; i < tot_ext; i++) {
			    src_indx1 = 0;
			    res_indx1 = -1;
			    for (j = 0; j < rank - 1; j++)
				src_indx1 += src_off[j];
			    for (j = 0; j < src_dim_ext; j++) {
				sindx = src_indx1 + (j * src_dim_strd);
				if ((i1ptr[sindx]^i1lval) >= 0) {
				    if (i1ptr[sindx] < i1lval) {
					i1lval = i1ptr[sindx];
					res_indx1 = j;
				    }
				} else {
				    if (i1ptr[sindx] < 0) {
					i1lval = i1ptr[sindx];
					res_indx1 = j;
				    }
				}
			    }
			    rindx = 0;
			    for (j = 0; j < rank - 1; j++)
				rindx += res_off[j];
			    _INTERM_INDEX();
			    i1lval = HUGE_INT1_F90;
			    _INCREMENT_D_ONE();
			}
		      }
		    }
		    break;
#endif

		case DVSUBTYPE_INT64 :
		    i8ptr = (_f_int8 *) source->base_addr.a.ptr;
		    i8lval = HUGE_INT8_F90;
		    if (dimension == NULL) {
			sindx = 0;
			if (use_mask) {		/* mask specified	*/
			    mindx = 0;
			    for (i = 0; i < tot_ext; i++) {
				if (LTOB(mask_el_len, &mptr[mindx])) {
				    if ((i8ptr[sindx]^i8lval) >= 0) {
					if (i8ptr[sindx] < i8lval) {
					    i8lval = i8ptr[sindx];
					    _SET_INDEX();
					}
				    } else {
					if (i8ptr[sindx] < 0) {
					    i8lval = i8ptr[sindx];
					    _SET_INDEX();
					}
				    }
				}
				_INCREMENT_TWO();
			    }
			} else {
			    for (i = 0; i < tot_ext; i++) {
				if ((i8ptr[sindx]^i8lval) >= 0) {
				    if (i8ptr[sindx] < i8lval) {
					i8lval = i8ptr[sindx];
					_SET_INDEX();
				    }
				} else {
				    if (i8ptr[sindx] < 0) {
					i8lval = i8ptr[sindx];
					_SET_INDEX();
				    }
				}
				_INCREMENT_ONE();
			    }
			}
			_FINAL_INDEX();	/* put temp values into result	*/
		    } else {
			if (use_mask) {		/* mask specified	*/
			  for (i = 0; i < tot_ext; i++) {
			    src_indx1 = 0;
			    msk_indx1 = 0;
			    res_indx1 = -1;
			    for (j = 0; j < rank - 1; j++) {
				src_indx1 += src_off[j];
				msk_indx1 += msk_off[j];
			    }
			    for (j = 0; j < src_dim_ext; j++) {
				mindx = msk_indx1 + (j * msk_dim_strd);
				if (LTOB(mask_el_len, &mptr[mindx])) {
				    sindx = src_indx1 + (j * src_dim_strd);
				    if ((i8ptr[sindx]^i8lval) >= 0) {
					if (i8ptr[sindx] < i8lval) {
					    i8lval = i8ptr[sindx];
					    res_indx1 = j;
					}
				    } else {
					if (i8ptr[sindx] < 0) {
					    i8lval = i8ptr[sindx];
					    res_indx1 = j;
					}
				    }
				}
			    }
			    rindx = 0;
			    for (j = 0; j < rank - 1; j++)
				rindx += res_off[j];
			    _INTERM_INDEX();
			    i8lval = HUGE_INT8_F90;
			    _INCREMENT_D_TWO();	/* increment indices	*/
			}
		    } else {
			for (i = 0; i < tot_ext; i++) {
			    src_indx1 = 0;
			    res_indx1 = -1;
			    for (j = 0; j < rank - 1; j++)
				src_indx1 += src_off[j];
			    for (j = 0; j < src_dim_ext; j++) {
				sindx = src_indx1 + (j * src_dim_strd);
				if ((i8ptr[sindx]^i8lval) >= 0) {
				    if (i8ptr[sindx] < i8lval) {
					i8lval = i8ptr[sindx];
					res_indx1 = j;
				    }
				} else {
				    if (i8ptr[sindx] < 0) {
					i8lval = i8ptr[sindx];
					res_indx1 = j;
				    }
				}
			    }
			    rindx = 0;
			    for (j = 0; j < rank - 1; j++)
				rindx += res_off[j];
			    _INTERM_INDEX();
			    i8lval = HUGE_INT8_F90;
			    _INCREMENT_D_ONE();
			}
		      }
		    }
		    break;

		case DVSUBTYPE_REAL64 :
		    r8ptr = (_f_real8 *) source->base_addr.a.ptr;
		    r8lval = HUGE_REAL8_F90;
		    if (dimension == NULL) {
			sindx = 0;
			if (use_mask) {		/* mask specified	*/
			    mindx = 0;
			    for (i = 0; i < tot_ext; i++) {
				if (LTOB(mask_el_len, &mptr[mindx])) {
				    if (r8ptr[sindx] < r8lval) {
					r8lval = r8ptr[sindx];
					_SET_INDEX();
				    }
				}
				_INCREMENT_TWO();
			    }
			} else {
			    for (i = 0; i < tot_ext; i++) {
				if (r8ptr[sindx] < r8lval) {
				    r8lval = r8ptr[sindx];
				    _SET_INDEX();
				}
				_INCREMENT_ONE();
			    }
			}
			_FINAL_INDEX();	/* put temp values into result	*/
		    } else {
			if (use_mask) {		/* mask specified	*/
			  for (i = 0; i < tot_ext; i++) {
			    src_indx1 = 0;
			    msk_indx1 = 0;
			    res_indx1 = -1;
			    for (j = 0; j < rank - 1; j++) {
				src_indx1 += src_off[j];
				msk_indx1 += msk_off[j];
			    }
			    for (j = 0; j < src_dim_ext; j++) {
				mindx = msk_indx1 + (j * msk_dim_strd);
				if (LTOB(mask_el_len, &mptr[mindx])) {
				    sindx = src_indx1 + (j * src_dim_strd);
				    if (r8ptr[sindx] < r8lval) {
					r8lval = r8ptr[sindx];
					res_indx1 = j;
				    }
				}
			    }
			    rindx = 0;
			    for (j = 0; j < rank - 1; j++)
				rindx += res_off[j];
			    _INTERM_INDEX();
			    r8lval = HUGE_REAL8_F90;
			    _INCREMENT_D_TWO();	/* increment indices	*/
			}
		    } else {				/* no mask	*/
			for (i = 0; i < tot_ext; i++) {
			    src_indx1 = 0;
			    res_indx1 = -1;
			    for (j = 0; j < rank - 1; j++) {
				src_indx1 += src_off[j];
			    }
			    for (j = 0; j < src_dim_ext; j++) {
				sindx = src_indx1 + (j * src_dim_strd);
				if (r8ptr[sindx] < r8lval) {
				    r8lval = r8ptr[sindx];
				    res_indx1 = j;
				}
			    }
			    rindx = 0;
			    for (j = 0; j < rank - 1; j++)
				rindx += res_off[j];
			    _INTERM_INDEX();
			    r8lval = HUGE_REAL8_F90;
			    _INCREMENT_D_ONE();
			}
		      }
		    }
		    break;

		case DVSUBTYPE_REAL32 :
		    r4ptr = (_f_real4 *) source->base_addr.a.ptr;
		    r4lval = HUGE_REAL4_F90;
		    if (dimension == NULL) {
			sindx = 0;
			if (use_mask) {		/* mask specified	*/
			    mindx = 0;
			    for (i = 0; i < tot_ext; i++) {
				if (LTOB(mask_el_len, &mptr[mindx])) {
				    if (r4ptr[sindx] < r4lval) {
					r4lval = r4ptr[sindx];
					_SET_INDEX();
				    }
				}
				_INCREMENT_TWO();
			    }
			} else {
			    for (i = 0; i < tot_ext; i++) {
				if (r4ptr[sindx] < r4lval) {
				    r4lval = r4ptr[sindx];
				    _SET_INDEX();
				}
				_INCREMENT_ONE();
			    }
			}
			_FINAL_INDEX();	/* put temp values into result	*/
		    } else {
			if (use_mask) {		/* mask specified	*/
			  for (i = 0; i < tot_ext; i++) {
			    src_indx1 = 0;
			    msk_indx1 = 0;
			    res_indx1 = -1;
			    for (j = 0; j < rank - 1; j++) {
				src_indx1 += src_off[j];
				msk_indx1 += msk_off[j];
			    }
			    for (j = 0; j < src_dim_ext; j++) {
				mindx = msk_indx1 + (j * msk_dim_strd);
				if (LTOB(mask_el_len, &mptr[mindx])) {
				    sindx = src_indx1 + (j * src_dim_strd);
				    if (r4ptr[sindx] < r4lval) {
					r4lval = r4ptr[sindx];
					res_indx1 = j;
				    }
				}
			    }
			    rindx = 0;
			    for (j = 0; j < rank - 1; j++)
				rindx += res_off[j];
			    _INTERM_INDEX();
			    r4lval = HUGE_REAL4_F90;
			    _INCREMENT_D_TWO();	/* increment indices	*/
			}
		    } else {				/* no mask	*/
			for (i = 0; i < tot_ext; i++) {
			    src_indx1 = 0;
			    msk_indx1 = 0;
			    res_indx1 = -1;
			    for (j = 0; j < rank - 1; j++) {
				src_indx1 += src_off[j];
				msk_indx1 += msk_off[j];
			    }
			    for (j = 0; j < src_dim_ext; j++) {
				sindx = src_indx1 + (j * src_dim_strd);
				if (r4ptr[sindx] < r4lval) {
				    r4lval = r4ptr[sindx];
				    res_indx1 = j;
				}
			    }
			    rindx = 0;
			    for (j = 0; j < rank - 1; j++)
				rindx += res_off[j];
			    _INTERM_INDEX();
			    r4lval = HUGE_REAL4_F90;
			    _INCREMENT_D_ONE();	/* increment sindx	*/
			}
		      }
		    }
		    break;

#if defined _F_REAL16 && _F_REAL16 != (-1)
		case DVSUBTYPE_REAL128 :
		    r16ptr = (_f_real16 *) source->base_addr.a.ptr;
		    r16lval = HUGE_REAL16_F90;
		    if (dimension == NULL) {
			sindx = 0;
			if (use_mask) {		/* mask specified	*/
			    mindx = 0;
			    for (i = 0; i < tot_ext; i++) {
				if (LTOB(mask_el_len, &mptr[mindx])) {
				    if (r16ptr[sindx] < r16lval) {
					r16lval = r16ptr[sindx];
					_SET_INDEX();
				    }
				}
				_INCREMENT_TWO();
			    }
			} else {
			    for (i = 0; i < tot_ext; i++) {
				if (r16ptr[sindx] < r16lval) {
				    r16lval = r16ptr[sindx];
				    _SET_INDEX();
				}
				_INCREMENT_ONE();
			    }
			}
			_FINAL_INDEX();	/* put temp values into result	*/
		    } else {
			if (use_mask) {		/* mask specified	*/
			  for (i = 0; i < tot_ext; i++) {
			    src_indx1 = 0;
			    msk_indx1 = 0;
			    res_indx1 = -1;
			    for (j = 0; j < rank - 1; j++) {
				src_indx1 += src_off[j];
				msk_indx1 += msk_off[j];
			    }
			    for (j = 0; j < src_dim_ext; j++) {
				mindx = msk_indx1 + (j * msk_dim_strd);
				if (LTOB(mask_el_len, &mptr[mindx])) {
				    sindx = src_indx1 + (j * src_dim_strd);
				    if (r16ptr[sindx] < r16lval) {
					r16lval = r16ptr[sindx];
					res_indx1 = j;
				    }
				}
			    }
			    rindx = 0;
			    for (j = 0; j < rank - 1; j++)
				rindx += res_off[j];
			    _INTERM_INDEX();
			    r16lval = HUGE_REAL16_F90;
			    _INCREMENT_D_TWO();	/* increment indices	*/
			}
		    } else {				/* no mask	*/
			for (i = 0; i < tot_ext; i++) {
			    src_indx1 = 0;
			    msk_indx1 = 0;
			    res_indx1 = -1;
			    for (j = 0; j < rank - 1; j++) {
				src_indx1 += src_off[j];
				msk_indx1 += msk_off[j];
			    }
			    for (j = 0; j < src_dim_ext; j++) {
				sindx = src_indx1 + (j * src_dim_strd);
				if (r16ptr[sindx] < r16lval) {
				    r16lval = r16ptr[sindx];
				    res_indx1 = j;
				}
			    }
			    rindx = 0;
			    for (j = 0; j < rank - 1; j++)
				rindx += res_off[j];
			    _INTERM_INDEX();
			    r16lval = HUGE_REAL16_F90;
			    _INCREMENT_D_ONE();	/* increment sindx	*/
			}
		      }
		    }
		    break;
#endif

		default :
		    _lerror (_LELVL_ABORT, FEINTDTY);
	    }
	}
}
