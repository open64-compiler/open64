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


#pragma ident "@(#) libfi/array/count.c	92.1	07/07/99 15:52:02"
#include <stddef.h>
#include <stdlib.h>
#include <liberrno.h>
#include <cray/dopevec.h>
#include <cray/portdefs.h>
#include "logical.h"

/* COUNT returns default type integer of kind=8 */

#ifdef _UNICOS
#pragma _CRI duplicate _COUNT as COUNT@
#endif
void
_COUNT (DopeVectorType * result,
	DopeVectorType * mask,
	_f_int	*dimension)
{
	void __count();
	(void) __count (result, mask, dimension);
}

#ifdef _UNICOS
#pragma _CRI duplicate _COUNT0 as COUNT0@
#endif
_f_int
_COUNT0(DopeVectorType * mask,
	_f_int	*dimension)
{
	void __count();
	DopeVectorType  result, *res_ptr;
	_f_int		intres;
	
	res_ptr = (DopeVectorType *) &result;
        res_ptr->base_addr.a.ptr = &intres;
        res_ptr->base_addr.a.el_len = sizeof(_f_int8) * BITS_PER_BYTE;
	res_ptr->assoc = 1;
	res_ptr->ptr_alloc = 0;
	res_ptr->p_or_a = NOT_P_OR_A;
	res_ptr->n_dim = 0;
	res_ptr->type_lens.type = DVTYPE_INTEGER;
        res_ptr->type_lens.dpflag = 0;
        res_ptr->type_lens.kind_or_star = DVD_DEFAULT;
	res_ptr->type_lens.int_len = sizeof(_f_int8) * BITS_PER_BYTE;
        res_ptr->type_lens.dec_len = 0;
        res_ptr->orig_base = res_ptr->base_addr.a.ptr;
        res_ptr->orig_size = 0;
	__count (res_ptr, mask, dimension);
	return (*(_f_int *) res_ptr->base_addr.a.ptr);
}


void
_COUNT_4 (DopeVectorType * result,
	DopeVectorType * mask,
	_f_int	*dimension)
{
	void __count();
	(void) __count (result, mask, dimension);
}

_f_int
_COUNT0_4(DopeVectorType * mask,
	_f_int	*dimension)
{
	void __count();
	DopeVectorType  result, *res_ptr;
	_f_int		intres;
	
	res_ptr = (DopeVectorType *) &result;
        res_ptr->base_addr.a.ptr = &intres;
        res_ptr->base_addr.a.el_len = sizeof(_f_int) * BITS_PER_BYTE;
	res_ptr->assoc = 1;
	res_ptr->ptr_alloc = 0;
	res_ptr->p_or_a = NOT_P_OR_A;
	res_ptr->n_dim = 0;
	res_ptr->type_lens.type = DVTYPE_INTEGER;
        res_ptr->type_lens.dpflag = 0;
        res_ptr->type_lens.kind_or_star = DVD_DEFAULT;
	res_ptr->type_lens.int_len = sizeof(_f_int4) * BITS_PER_BYTE;
        res_ptr->type_lens.dec_len = 0;
        res_ptr->orig_base = res_ptr->base_addr.a.ptr;
        res_ptr->orig_size = 0;
	__count (res_ptr, mask, dimension);
	return (*(_f_int4 *) res_ptr->base_addr.a.ptr);
}


void
_COUNT_8 (DopeVectorType * result,
	DopeVectorType * mask,
	_f_int	*dimension)
{
	void __count();
	(void) __count (result, mask, dimension);
}

_f_int
_COUNT0_8(DopeVectorType * mask,
	_f_int	*dimension)
{
	void __count();
	DopeVectorType  result, *res_ptr;
	_f_int8		intres;
	
	res_ptr = (DopeVectorType *) &result;
        res_ptr->base_addr.a.ptr = &intres;
        res_ptr->base_addr.a.el_len = sizeof(_f_int8) * BITS_PER_BYTE;
	res_ptr->assoc = 1;
	res_ptr->ptr_alloc = 0;
	res_ptr->p_or_a = NOT_P_OR_A;
	res_ptr->n_dim = 0;
	res_ptr->type_lens.type = DVTYPE_INTEGER;
        res_ptr->type_lens.dpflag = 0;
        res_ptr->type_lens.kind_or_star = DVD_DEFAULT;
	res_ptr->type_lens.int_len = sizeof(_f_int8) * BITS_PER_BYTE;
        res_ptr->type_lens.dec_len = 0;
        res_ptr->orig_base = res_ptr->base_addr.a.ptr;
        res_ptr->orig_size = 0;
	__count (res_ptr, mask, dimension);
	return (*(_f_int8 *) res_ptr->base_addr.a.ptr);
}


void
_COUNT_1 (DopeVectorType * result,
	DopeVectorType * mask,
	_f_int	*dimension)
{
	void __count();
	(void) __count (result, mask, dimension);
}

_f_int1
_COUNT0_1(DopeVectorType * mask,
	_f_int	*dimension)
{
	void __count();
	DopeVectorType  result, *res_ptr;
	_f_int1		intres;
	
	res_ptr = (DopeVectorType *) &result;
        res_ptr->base_addr.a.ptr = &intres;
        res_ptr->base_addr.a.el_len = sizeof(_f_int1) * BITS_PER_BYTE;
	res_ptr->assoc = 1;
	res_ptr->ptr_alloc = 0;
	res_ptr->p_or_a = NOT_P_OR_A;
	res_ptr->n_dim = 0;
	res_ptr->type_lens.type = DVTYPE_INTEGER;
        res_ptr->type_lens.dpflag = 0;
        res_ptr->type_lens.kind_or_star = DVD_DEFAULT;
	res_ptr->type_lens.int_len = sizeof(_f_int1) * BITS_PER_BYTE;
        res_ptr->type_lens.dec_len = 0;
        res_ptr->orig_base = res_ptr->base_addr.a.ptr;
        res_ptr->orig_size = 0;
	__count (res_ptr, mask, dimension);
	return (*(_f_int1 *) res_ptr->base_addr.a.ptr);
}


void
_COUNT_2 (DopeVectorType * result,
	DopeVectorType * mask,
	_f_int	*dimension)
{
	void __count();
	(void) __count (result, mask, dimension);
}

_f_int2
_COUNT0_2(DopeVectorType * mask,
	_f_int	*dimension)
{
	void __count();
	DopeVectorType  result, *res_ptr;
	_f_int2		intres;
	
	res_ptr = (DopeVectorType *) &result;
        res_ptr->base_addr.a.ptr = &intres;
        res_ptr->base_addr.a.el_len = sizeof(_f_int2) * BITS_PER_BYTE;
	res_ptr->assoc = 1;
	res_ptr->ptr_alloc = 0;
	res_ptr->p_or_a = NOT_P_OR_A;
	res_ptr->n_dim = 0;
	res_ptr->type_lens.type = DVTYPE_INTEGER;
        res_ptr->type_lens.dpflag = 0;
        res_ptr->type_lens.kind_or_star = DVD_DEFAULT;
	res_ptr->type_lens.int_len = sizeof(_f_int2) * BITS_PER_BYTE;
        res_ptr->type_lens.dec_len = 0;
        res_ptr->orig_base = res_ptr->base_addr.a.ptr;
        res_ptr->orig_size = 0;
	__count (res_ptr, mask, dimension);
	return (*(_f_int2 *) res_ptr->base_addr.a.ptr);
}


void
__count  (DopeVectorType * result,
	DopeVectorType * mask,
	_f_int	*dimension)
{
	int	c_dim;		/* C form of input dimension	*/
	int	other_dim;	/* other dimension in rank-2	*/
	int	num_strides = 1; /* strides through mask array	*/
	int	num_elts = 1;	/* elements in result array	*/
	long	nbytes = 0;	/* bytes to malloc		*/
	long	trues;		/* number of true elements	*/
	_f_int	* rptr;		/* ptr to result array		*/
#ifdef	_F_INT1
	_f_int1	* i1rptr;	/* ptr to result array		*/
	_f_log1 * i1mptr;	/* ptr to mask array		*/
#endif
#ifdef	_F_INT2
	_f_int2	* i2rptr;	/* ptr to result array		*/
	_f_log2 * i2mptr;	/* ptr to mask array		*/
#endif
	_f_int4	* i4rptr;	/* ptr to result array		*/
	_f_int8	* i8rptr;	/* ptr to result array		*/
	_f_log	* imptr;	/* ptr to mask array		*/
	_f_log4 * i4mptr;	/* ptr to mask array		*/
	_f_log8 * i8mptr;	/* ptr to mask array		*/
	long	i, j;		/* index variables		*/
	long	indx, jndx;	/* loop indices			*/
	int	done;		/* work done indicator		*/
	int     el_len;		/* LTOB length indicator        */
	int     mshftct=0;	/* mask amount to shift index	*/
	int     rshftct=0;	/* result amount to shift index	*/

	/* Per-dimension arrays	*/
	long	current_place[MAXDIM-1];	/* current place*/
	long	mask_offset[MAXDIM-1];		/* mask offset	*/
	long	mask_extent[MAXDIM-1];		/* mask extent	*/
	long	mask_stride[MAXDIM-1];		/* mask stride	*/
	long	result_offset[MAXDIM-1];	/* result offset*/
	long	result_stride[MAXDIM-1];	/* result stride*/
	long	cdim_mask_stride;		/* cdim stride	*/

	/* Validate dimension variable	*/
	if (dimension != NULL && mask->n_dim > 1) {
	    c_dim = *dimension - 1;
	    if (c_dim < 0 || c_dim >= mask->n_dim)
		_lerror (_LELVL_ABORT, FESCIDIM);
	} else {
		c_dim = 0;
		if (dimension != NULL) {
			if (*dimension  < 1 || *dimension > mask->n_dim)
				_lerror (_LELVL_ABORT, FESCIDIM);
		}
	}

	/* Setup dope vector for result array	*/
	if (!result->assoc) {
		int sm =	1;
		if (result->base_addr.a.el_len >= BITS_PER_WORD)
			sm = result->base_addr.a.el_len / BITS_PER_WORD;
		for (i = 0; i < c_dim; i++) {
			if (dimension != NULL) {
				result->dimension[i].extent =
				  mask->dimension[i].extent;
				result->dimension[i].low_bound = 1;
				result->dimension[i].stride_mult =
				  num_strides * sm;
			}
			num_strides *= mask->dimension[i].extent;
	    	}
		for ( ; i < mask->n_dim - 1; i++) {
			if (dimension != NULL) {
				result->dimension[i].extent =
		     		  mask->dimension[i+1].extent;
				result->dimension[i].low_bound = 1;
		 		result->dimension[i].stride_mult =
		 		  num_strides * sm;
			}
			num_strides *= mask->dimension[i+1].extent;
		}
		if (dimension != NULL)
			num_elts = num_strides;

		 result->base_addr.a.ptr =       (void *) NULL;

		nbytes = ((num_elts * result->base_addr.a.el_len) /
			BITS_PER_BYTE);
		if (nbytes != 0) {
			result->base_addr.a.ptr = (void *) malloc (nbytes);
			if (result->base_addr.a.ptr == NULL)
                        	_lerror(_LELVL_ABORT, FENOMEMY);
			result->assoc = 1;
		}
        	/* set fields for null array as well */
		result->orig_base = result->base_addr.a.ptr;
		result->orig_size = nbytes * BITS_PER_BYTE;
	} else {
		int rank;
		rank = mask->n_dim;
		for (i = 0; i < rank-1; i++)
			num_strides *= mask->dimension[i+1].extent;
	}


	/* Set pointer to result array and initialize result
	 * array to zeroes.
	 */
	rptr =	(void *) result->base_addr.a.ptr;
	switch (result->type_lens.int_len) {
		case 64 :
			i8rptr =	(_f_int8 *) rptr;
#ifdef	_F_INT4
			if (sizeof(_f_int) == sizeof(_f_log4))
				rshftct =	1;
#endif
#ifdef _UNICOS
#pragma _CRI     ivdep
#endif
			for (i = 0; i < num_elts; i++)
	     			i8rptr[i] = 0;
			break;
#ifdef	_F_INT2
		case 16 :
			i2rptr =	(_f_int2 *) rptr;
			for (i = 0; i < num_elts; i++)
	     			i2rptr[i] = 0;
			break;
#endif
#ifdef	_F_INT1
		case 8 :
			i1rptr =	(_f_int1 *) rptr;
			for (i = 0; i < num_elts; i++)
	     			i1rptr[i] = 0;
			break;
#endif
		case 32 :
		default :
			i4rptr =	(_f_int4 *) rptr;
#ifdef _UNICOS
#pragma _CRI     ivdep
#endif
			for (i = 0; i < num_elts; i++)
		     		i4rptr[i] = 0;
	}

	/* Set pointer to mask array and initialize el_len for LTOB */

	imptr =	(void *) mask->base_addr.a.ptr;
	switch (mask->type_lens.int_len) {
		case 64 :
			el_len =	sizeof(_f_log8) * BITS_PER_BYTE;
			i8mptr =	(_f_log8 *) imptr;
#ifdef	_F_LOG4
		/* Set mask shftct for COUNT with no size specified
		 * since no size means 64-bit logical value.  A default
		 * 32-bit logical has a stride_mult of two for 64-bit
		 * logical on WORD32.  Normally, the COUNT_8 entry point
		 * is used.  On MPP, the stride_mult is one for 32-bit
		 * or 64-bit logical.
		 */
			if (sizeof(_f_int) == sizeof(_f_log4))
				mshftct =	1;
#endif
			break;
#ifdef	_F_LOG2
		case 16 :
			el_len =        sizeof(_f_log2) * BITS_PER_BYTE;
			i2mptr =	(_f_log2 *) imptr;
			break;
#endif
#ifdef	_F_LOG1
		case 8 :
			el_len =        sizeof(_f_log1) * BITS_PER_BYTE;
			i1mptr =	(_f_log1 *) imptr;
			break;
#endif
		case 32 :
		default :
			el_len =        sizeof(_f_log4) * BITS_PER_BYTE;
			i4mptr =	(_f_log4 *) imptr;
	}

	/* Handle a rank-one mask array	*/

	if (mask->n_dim == 1) {
		/* Use local mask_stride and divide by two when two-word
		 * logical is being done.
		 */
#ifdef _F_LOG4
		mask_stride[0] = (mask->dimension[0].stride_mult) >> mshftct;
#else
		mask_stride[0] = mask->dimension[0].stride_mult;
#endif
		/* Scan array, counting TRUE elements */
		trues = 0;
		for (i = 0; i < mask->dimension[0].extent; i++) {
			indx = i * mask_stride[0];
			switch (mask->type_lens.int_len) {
			    case 64 :
				if (LTOB(el_len, (i8mptr + indx))) {
					/* true element */
					trues++;
				}
				break;
#ifdef _F_LOG2
			    case 16 :
				if (LTOB(el_len, (i2mptr + indx))) {
					/* true element */
					trues++;
				}
				break;
#endif
#ifdef _F_LOG1
			    case 8 :
				if (LTOB(el_len, (i1mptr + indx))) {
					/* true element */
					trues++;
				}
				break;
#endif
			    case 32 :
			    default :
				if (LTOB(el_len, (i4mptr + indx))) {
					/* true element */
					trues++;
				}
			}
		}

		/* store result depending on size of integer */
		switch (result->type_lens.int_len) {
			case 64 :
	 			i8rptr[0] =	trues;
				break;
#ifdef _F_INT2
			case 16 :
	 			i2rptr[0] =	trues;
				break;
#endif
#ifdef _F_INT1
			case 8 :
	 			i1rptr[0] =	trues;
				break;
#endif
			case 32 :
			default :
	 			i4rptr[0] =	trues;
		}

	/* Handle a rank-two mask array	*/
	} else if (mask->n_dim == 2) {

		/* Initialize data */
		if (c_dim == 0)
			other_dim = 1;
		else
			other_dim = 0;
#ifdef _F_LOG4
		mask_stride[0] = (mask->dimension[0].stride_mult) >> mshftct;
		mask_stride[1] = (mask->dimension[1].stride_mult) >> mshftct;
#else
		mask_stride[0] = mask->dimension[0].stride_mult;
		mask_stride[1] = mask->dimension[1].stride_mult;
#endif
		/* Scan array, counting TRUE elements	*/
		trues = 0;
		for (i = 0; i < num_strides; i++) {
			indx = i * mask_stride[other_dim];
			for (j = 0; j < mask->dimension[c_dim].extent; j++) {
				jndx = indx + j *
				  mask_stride[c_dim];
				switch (mask->type_lens.int_len) {
				    case 64 :
					if (LTOB(el_len, (i8mptr +
					   jndx))) {
						/* true element */
						trues++;
					}
					break;
#ifdef _F_LOG2
				    case 16 :
					if (LTOB(el_len, (i2mptr +
					    jndx))) {
						/* true element */
						trues++;
					}
					break;
#endif
#ifdef _F_LOG1
				    case 8 :
					if (LTOB(el_len, (i1mptr +
					    jndx))) {
						/* true element */
						trues++;
					}
					break;
#endif
				    case 32 :
				    default :
					if (LTOB(el_len, (i4mptr +
					    jndx))) {
						/* true element */
						trues++;
					}
				}
		 	}

			/* store result depending on size of integer */
		 	if (result->n_dim != 0) {
				switch (result->type_lens.int_len) {
				    case 64 :
	 				i8rptr[i] =	trues;
					break;
#ifdef _F_INT2
				    case 16 :
	 				i2rptr[i] =	trues;
					break;
#endif
#ifdef _F_INT1
				    case 8 :
	 				i1rptr[i] =	trues;
					break;
#endif
				    case 32 :
				    default :
	 				i4rptr[i] =	trues;
				}
		 		trues = 0;
		 	}
	 	}
		if (result->n_dim == 0) {
			switch (result->type_lens.int_len) {
			    case 64 :
	 			i8rptr[0] =	trues;
				break;
#ifdef _F_INT2
			    case 16 :
	 			i2rptr[0] =	trues;
				break;
#endif
#ifdef _F_INT1
			    case 8 :
	 			i1rptr[0] =	trues;
				break;
#endif
			    case 32 :
			    default :
	 			i4rptr[0] =	trues;
			}
		}

	/* Handle a rank-three through rank-seven mask array */
	} else {

		/* Initialize data */
		if (result->n_dim != 0)
#ifdef _UNICOS
#pragma _CRI     shortloop
#endif
			for (i = 0; i < result->n_dim; i++) {
				result_offset[i] = 0;
#ifdef _F_LOG4
				result_stride[i] =
				  result->dimension[i].stride_mult >> rshftct;
#else
				result_stride[i] =
				  result->dimension[i].stride_mult;
#endif
			}

		/* Initialize mask parameters based on which dimension
		 * has been requested
		 */
		if (c_dim == 0)
			i = 0;
		else
#ifdef _UNICOS
#pragma _CRI     shortloop
#endif
		   for (i = 0; i < c_dim; i++) {
			current_place[i] = 0;
			mask_offset[i] = 0;
			mask_extent[i] = mask->dimension[i].extent;
#ifdef _F_LOG4
			mask_stride[i] =
			   (mask->dimension[i].stride_mult) >> mshftct;
#else
			mask_stride[i] = mask->dimension[i].stride_mult;
#endif
		   }
		if (i < (mask->n_dim - 1))
#ifdef _UNICOS
#pragma _CRI     shortloop
#endif
		   for ( ; i < mask->n_dim - 1; i++) {
			current_place[i] = 0;
			mask_offset[i] = 0;
			mask_extent[i] = mask->dimension[i+1].extent;
#ifdef _F_LOG4
			mask_stride[i] =
			   (mask->dimension[i+1].stride_mult) >> mshftct;
#else
			mask_stride[i] = mask->dimension[i+1].stride_mult;
#endif
		   }
#ifdef	_F_LOG4
		cdim_mask_stride = mask->dimension[c_dim].stride_mult >> mshftct;
#else
		cdim_mask_stride = mask->dimension[c_dim].stride_mult;
#endif

		/* Scan array, counting TRUE elements	*/

		trues = 0;
		for (i = 0; i < num_strides; i++) {

			/* Determine starting point */

			indx = 0;
#ifdef _UNICOS
#pragma _CRI     shortloop
#endif
			for (j = 0; j < mask->n_dim - 1; j++)
				indx += mask_offset[j];

			/* Scan array, counting TRUE elements	*/
			for (j = 0; j < mask->dimension[c_dim].extent; j++) {
				jndx = indx + j * cdim_mask_stride;
				switch (mask->type_lens.int_len) {
				    case 64 :
					if (LTOB(el_len, (i8mptr +
					   jndx))) {
						/* true element */
						trues++;
					}
					break;
#ifdef _F_LOG2
				    case 16 :
					if (LTOB(el_len, (i2mptr +
					    jndx))) {
						/* true element */
						trues++;
					}
					break;
#endif
#ifdef _F_LOG1
				    case 8 :
					if (LTOB(el_len, (i1mptr +
					    jndx))) {
						/* true element */
						trues++;
					}
					break;
#endif
				    case 32 :
				    default :
					if (LTOB(el_len, (i4mptr +
					    jndx))) {
						/* true element */
						trues++;
					}
				}
			}
			if (result->n_dim != 0) {
				indx = 0;
#ifdef _UNICOS
#pragma _CRI     shortloop
#endif
				for (j = 0; j < mask->n_dim - 1; j++)
					indx += result_offset[j];

				/* store result for size of integer */
				switch (result->type_lens.int_len) {
				    case 64 :
	 				i8rptr[indx] =	trues;
					break;
#ifdef _F_INT2
				    case 16 :
	 				i2rptr[indx] =	trues;
					break;
#endif
#ifdef _F_INT1
				    case 8 :
	 				i1rptr[indx] =	trues;
					break;
#endif
				    case 32 :
				    default :
	 				i4rptr[indx] =	trues;
				}
				trues =	0;
			}
	
			/* Increment pointers for each dimension */
			j = 0;
			done = FALSE;
			while (done == FALSE && j < mask->n_dim - 1) {
				if (current_place[j] == mask_extent[j] - 1) {
					current_place[j] = 0;
					mask_offset[j] = 0;
					if (result->n_dim != 0)
						result_offset[j] = 0;
				} else {
					current_place[j]++;
					mask_offset[j] =
					  current_place[j] * mask_stride[j];
					if (result->n_dim != 0)
						result_offset[j] =
						  current_place[j] *
						    result_stride[j];
					done = TRUE;
				}
				j++;
			}
		}
		if (result->n_dim == 0) {
			/* store result for size of integer */
			switch (result->type_lens.int_len) {
			    case 64 :
	 			i8rptr[0] =	trues;
				break;
#ifdef _F_INT2
			    case 16 :
	 			i2rptr[0] =	trues;
				break;
#endif
#ifdef _F_INT1
			    case 8 :
	 			i1rptr[0] =	trues;
				break;
#endif
			    case 32 :
			    default :
	 			i4rptr[0] =	trues;
			}
		}
	}
}
