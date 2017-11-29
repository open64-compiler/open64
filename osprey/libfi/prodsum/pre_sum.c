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


#pragma ident "@(#) libfi/prodsum/pre_sum.c	92.2	07/13/99 10:46:27"

#include <stddef.h>		/* for NULL	*/
#include <stdlib.h>		/* for malloc	*/
#include <cray/dopevec.h>	/* description of F90 Dope Vector	*/
#include <cray/portdefs.h>	/* portable definitions header file	*/
#include "intrin.h"		/* general header file for intrinsics	*/

extern void
_PRE_SUM(int *flag, DopeVectorType * RESULT, DopeVectorType * MATRIX_A,
	DopeVectorType *DIM, DopeVectorType * MASK, int *ndim,
	int *dimarg, int *dima, int *maskarg, int *scalar, long *loca,
	int lima[], int inca[], long *locb,
	int limb[], int incb[], long *locm,
	int limm[], int incm[], int *sizem, long *loc4m);
/************************************************************************
*  Author:  Math Software Group, Cray Research, Inc.                     *
*                                                                        *
*  Name:   _PRE_SUM                                                      *
*                                                                        *
*  Description:                                                          *
*  -----------                                                           *
*  Definition of Common block and preprocessing routine for the array    *
*  intrinsic functions SUM and PRODUCT. PREpreprocess dope vectors for   *
*  routines SUM__x and PROD__x.  This function is a Fortran-callable     *
*  function with call-by-address calling sequence).                      *
*                                                                        *
*  Parse dope vectors for the arguments, store the relevant parameters   *
*  in the variables loca, lima, etc.  Allocate space for the result      *
*  array, if necessary, and fill in its dope vector.                     *
*                                                                        *
*  Arguments:                                                            *
*  ---------                                                             *
*  flag            (address of) integer                                  *
*          0       if called from sum0_x/prod0_x, the result is a scalar *
*                  and the dope vector for result is a dummy variable.   *
*          1       if called from sum_x/prod_x, the result dope vector   *
*                  is present, and must be parsed.                       *
*                                                                        *
*  dv_result       (address of) dope vector for result array             *
*                                                                        *
*  dv_array        (address of) dope vector for array to be summed.      *
*                                                                        *
*  dv_dim          (address of) dope vector for (optional) number of the *
*                  dimension over which to sum or NULL if not specified. *
*                                                                        *
*  dv_mask         (address of) dope vector (optional) for MASK array    *
*                  or NULL if not specified.                             *
*                                                                        *
*  Both dim and mask are optional; if the fourth argument is NULL, the   *
*  type of the third argument is checked for a MASK or DIM argument.     *
*  This entry point is called by PE 3.0 and 3.0+ SUM and PRODUCT         *
*  routines.  Integer(kind=6) is no longer present for SUM or PRODUCT.   *
*                                                                        *
*************************************************************************/

/* If these arguments change, you must also change the files: sumcom.h,
 *   sum.h,    sum0.h,    prod.h,    and prod0.h,
 *   sum_90.h, sum0_90.h, prod_90.h, and prod0_90.h,
 *   int ndim;       number of dimensions
 *   int dimarg;     Fortran logical var. true of DIM arg. was given
 *   int dima;       dimension to sum (if dimarg was given)
 *   int maskarg;    Fortran logical var true if MASK arg. was given
 *   int scalar;     Fortran logical var true if sum is a scalar
 *   long *loca;     address of array A
 *   int  lima[MAXDIM];	extent(i) of array A
 *   int  inca[MAXDIM];	increment(i) of array A
 *   long *locb;     address of array B (the result)
 *   int  limb[MAXDIM];	extent(i) of array B
 *   int  incb[MAXDIM];	increment(i) of array B
 *   long *locm;     address of array MASK
 *   int  limm[MAXDIM];	extent(i) of array MASK
 *   int  incm[MAXDIM];	increment(i) of array MASK
 *   int sizem;      size of mask element in bytes
 *   long *loc4m;    address of array MASK
 */
#ifdef	_UNICOS
#pragma _CRI inline _PRE_SUM
#endif
void
_PRE_SUM(int *flag, DopeVectorType * RESULT, DopeVectorType * MATRIX_A,
	DopeVectorType *DIM, DopeVectorType * MASK, int *ndim,
	int *dimarg, int *dima, int *maskarg, int *scalar, long *loca,
	int lima[], int inca[], long *locb,
	int limb[], int incb[], long *locm,
	int limm[], int incm[], int *sizem, long *loc4m)
{
	int	nbits, nbytes, nwords;	/* size of RESULT matrix */
	int	i;		/* subscript */
	int	dim;		/* value of DIM argument */
	int	temp;		/* for temporary storage */
	void	*locab;		/* local store for veca */
	void	*locbb;		/* local store for vecb */
	void	*locmc;		/* local store for result */
	DopeVectorType	*dm, *mk;
	_f_int8 *dimenp8	= NULL;
	_f_int4 *dimenp4	= NULL;
	_f_int2 *dimenp2	= NULL;
	_f_int1 *dimenp1	= NULL;
	int	dmintlen;	/* internal length of DIM value */
	dm = DIM;
	mk = MASK;
	/* if last arg = NULL, is last-1 arg mask or dim? */
	if (MASK == NULL) {
		/* last arg = NULL, is last-1 arg mask or dim? */
		if (DIM != NULL) {
			if (DIM->type_lens.type == DVTYPE_LOGICAL) {
				/* last-1 argument is mask. */
				mk = DIM;
				dm = MASK;
			}
		}
	}
	if (dm != NULL) {
		dmintlen = dm->type_lens.int_len >> 3;
		if (dmintlen == sizeof(_f_int8)) {
			*dimarg	= _btol(1);     /* dimarg = .TRUE. */
			dimenp8	= (_f_int8 *) dm->base_addr.a.ptr;
			dim	= *dimenp8;     /* dimension to sum over */
			*dima	= *dimenp8;     /* dimension to sum over */
		} else if (dmintlen == sizeof(_f_int4)) {
			*dimarg	= _btol(1);     /* dimarg = .TRUE. */
			dimenp4	= (_f_int4 *) dm->base_addr.a.ptr;
			dim	= *dimenp4;     /* dimension to sum over */
			*dima	= *dimenp4;     /* dimension to sum over */
		} else if (dmintlen == sizeof(_f_int2)) {
			*dimarg	= _btol(1);     /* dimarg = .TRUE. */
			dimenp2	= (_f_int2 *) dm->base_addr.a.ptr;
			dim	= *dimenp2;     /* dimension to sum over */
			*dima	= *dimenp2;     /* dimension to sum over */
		} else if (dmintlen == sizeof(_f_int1)) {
			*dimarg	= _btol(1);     /* dimarg = .TRUE. */
			dimenp1	= (_f_int1 *) dm->base_addr.a.ptr;
			dim	= *dimenp1;     /* dimension to sum over */
			*dima	= *dimenp1;     /* dimension to sum over */
		}
	} else {
		*dimarg	= _btol(0);	/* dimarg = .FALSE. */
	}

	/* Parse the dope vector for MATRIX_A */
	/* base address of matrix a array */
	locab = (void *) MATRIX_A->base_addr.a.ptr;
	*loca = (long) locab;
	*ndim = MATRIX_A->n_dim;	/* number of dimensions */

	/* words per element = bits per element divided by 64 */
	if (MATRIX_A->type_lens.int_len >= BITS_PER_WORD) {
		nwords = MATRIX_A->type_lens.int_len / BITS_PER_WORD;
	} else
		nwords = 1;

#ifdef	_UNICOS
#pragma CRI     shortloop
#endif
	for (i = 0; i < *ndim; i++) {	/* for each dimension of A ... */
		/* number of elements */
		lima[i] = MATRIX_A->dimension[i].extent;
		/* increment */
		inca[i] = MATRIX_A->dimension[i].stride_mult / nwords;
	}
	if (mk != NULL) {
		*maskarg = _btol(1);	/* maskarg = .TRUE. */
		locmc = (void *) mk->base_addr.a.ptr;
		*locm = (long) locmc;
		*sizem = (long) mk->type_lens.int_len >> 3;
		*loc4m = (long) locmc;

#ifdef	_UNICOS
#pragma CRI     shortloop
#endif
		for (i = 0; i < *ndim; i++) {	/* per dimension of A */
			/* mask argument must conform to source argument. */
			if( mk->n_dim == MATRIX_A->n_dim ) {
				/* number of elements in mask */
				limm[i] = mk->dimension[i].extent;
				/* increment in mask */
				incm[i] = mk->dimension[i].stride_mult;
			} else {
				limm[i] = 1;
				incm[i] = 0;
			}
			if (mk->type_lens.int_len > BITS_PER_WORD)
				incm[i] >>= 1;
		/* NOTE - MASK is logical. stride_mult contains one per
		 *        element whether half-word or full word logical.
		 *        On some architectures, the stride can be 2-word
		 *        multiples.
		 */
		}
	} else {
		*maskarg = _btol(0);		/* maskarg = .FALSE. */
	}
	/* first argument tells whether result is a scalar or an array */
	if (*flag == 0) {
		*scalar = _btol(1);		/* scalar = .TRUE. */
	} else {
		*scalar = _btol(0);		/* scalar = .FALSE. */
	}
	/* Error checking */
	if (*dimarg) {
		if ((1 <= dim) && (dim <= *ndim));	/* no error */
		else {
			/* Value of dimension argument exceeded
			 * the number of dimensions.
			 */
			ERROR(FESCIDIM);
			return;
		}
	}

	/* If sum0/prod0 called PRESUM to compute a scalar result, exit
	 * because there is no RESULT dope vector.
	 */
	if (*flag == 0)
		return;

	/* Allocate result array, if result is not associated. */
	if (!RESULT->assoc) {

		/* clear base address and size. */
		RESULT->base_addr.a.ptr = (void *) NULL;
		RESULT->orig_base = (void *) NULL;
		RESULT->orig_size = 0;

		/* Contract the dimension over which we sum as we fill in
		 * dimension-specific information in RESULT dope vector.
		 * First dimensions of B are (up to dimension dim), are
		 * the same as for MATRIX_A.
		 */
#ifdef	_UNICOS
#pragma CRI     shortloop
#endif
		for (i = 0; i < dim - 1; i++) {
			limb[i] = lima[i];
			RESULT->dimension[i].extent = lima[i];
			RESULT->dimension[i].low_bound = 1;
		}

		/* The remaining dimensions of B (after dimension dim),
		 * are the same as the next dimension of MATRIX_A.
		 */
#ifdef	_UNICOS
#pragma CRI     shortloop
#endif
		for (i = dim - 1; i < *ndim - 1; i++) {
			limb[i] = lima[i + 1];
			RESULT->dimension[i].extent = lima[i + 1];
			RESULT->dimension[i].low_bound = 1;
		}
		incb[0] = 1;	/* first increment is 1 */
		for (i = 1; i < *ndim - 1; i++) {

			/* Next increment is product of previous
			 * increment and previous limit.
			 */
			incb[i] = incb[i - 1] * limb[i - 1];
		}

		/* Fill in stride multipliers (equal to increment times
		 * element length, in words).
		 */
#ifdef	_UNICOS
#pragma CRI     shortloop
#endif
		for (i = 0; i < *ndim - 1; i++) {
			RESULT->dimension[i].stride_mult = incb[i] *
				nwords;
		}

		/* Compute nbits = total space required for RESULT, in
		 * bits, as element length times products of all extents.
		 */
		nbits = RESULT->type_lens.int_len;
#ifdef	_UNICOS
#pragma CRI     shortloop
#endif
		for (i = 0; i < *ndim - 1; i++) {
			nbits *= limb[i];
		}

		/* number of bytes = number of bits divided by 8. */
		nbytes = nbits >> 3;

		/* Allocate the space for RESULT */
		locbb = (void *) NULL;
		if (nbytes != 0) {
			locbb = (void *) MALLOC(nbytes);
			if (locbb == NULL) {
				ERROR(FENOMEMY);
				return;
			}
		}

		/* set associated flag, address, and size of result */
		RESULT->assoc = 1;
		RESULT->base_addr.a.ptr = (void *) locbb;
		RESULT->orig_base = (void *) locbb;
		RESULT->orig_size = nbits;
		/* End of allocation of RESULT matrix. */
	} else {

		/* If RESULT matrix associated on input, return RESULT
		 * base address and dimension-specific information.
		 */
		locbb = (void *) RESULT->base_addr.a.ptr;

		/* number of bits per element */
		nbits = RESULT->type_lens.int_len;
#ifdef	_UNICOS
#pragma CRI     shortloop
#endif
		for (i = 0; i < RESULT->n_dim; i++) {
			limb[i] = RESULT->dimension[i].extent;

			/* increment is stride mult divided by number
			 * of words per element.
			 */
			incb[i] = RESULT->dimension[i].stride_mult /
				nwords;
		}
	}
	*locb = (long) locbb;
	/* Now, if DIM arg was specified, rotate the limits and strides
	 * (in the inc and lim arrays, not in the dope vectors, so the
	 * sum/prod is taken over the first dimension.
	 * NOTE:  This step is unnecessary if PRESUM was called from
	 * sum0/prod0 (when *flag == 0), because the result will be
	 * scalar, and the order of evaluation is irrelevant.
	 *
	 * Example:  
	 *  Array was given as array:
	 *	3 x 4 x 5 x 6 x 7 x 8 
	 *  with dim = 5.  Do a circular right shift on the first 5
	 *  limits and increments, so we can pretend it is an array:
	 *  	7 x 3 x 4 x 5 x 6 x 8 
	 *  which we will reduce on the first dimension, yielding array:
	 *	3 x 4 x 5 x 6 x 8 
	 */
	if (*dimarg && dim != 1) {
		dim -= 1;	/* use zero-based subscripts */
		/* rotate limits of input array. */
		temp = lima[dim];
		for(i = dim; i >= 1; --i)
			lima[i] = lima[i-1];
		lima[0] = temp;

		/* rotate increments of input array. */
		temp = inca[dim];
		for(i = dim; i >= 1; --i)
			inca[i] = inca[i-1];
		inca[0] = temp;
		if (*maskarg) {

			/* rotate mask limits and increments. */
			temp = limm[dim];
			for (i = dim; i >= 1; --i)
				limm[i] = limm[i-1];
			limm[0] = temp;
			temp = incm[dim];
			for (i = dim; i >= 1; --i)
				incm[i] = incm[i-1];
			incm[0] = temp;
		} 
		*dima = 1;
	}
	return;
}
