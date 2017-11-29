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


#pragma ident "@(#) libfi/mpp_util/allocate_routines.c	92.1	07/13/99 10:45:02"


/**************************************************************************
 *
 * This file contains utility subroutines that are used by the
 * MPP version of the Fortran 90 array intrinsics.
 *
 *************************************************************************/

#include <fortran.h>
#include <stdlib.h>
#include <liberrno.h>
#include <cray/dopevec.h>
#include <cray/portdefs.h>
#include "f90_macros.h"
#include <fmath.h>

#define	BITS_PER_BYTE	(BITS_PER_WORD / BYTES_PER_WORD)

/****************************************************************************
 *
 * _shmalloc_result() -	Allocate a result temporary in shared space,
 *			and fill in the result's dope vector.
 *
 * Arguments:	result		- ptr to dope vector for result array
 *		source		- ptr to dope vector for source array
 *		result_base_ptr	- ptr to result array
 *
 * Variables:	nbytes		- byte count to allocate memory for
 *		total_extent	- the total extent of result array
 *		dim		- dimension constant
 *		cnt		- block count of first block for dim i
 *		hi		- hi index of first block
 *		low		- low index of first block
 *
 ***************************************************************************/

_shmalloc_result(
		DopeVectorType	*res,
		DopeVectorType	*src,
		void 		**res_base_ptr)
{
	long	total_extent;
	int	nbytes, nwords, nwords_pe;
	int	one;
	int	i, j;
	int	dim, cnt, hi, low;

	nbytes = src->type_lens.int_len / BITS_PER_BYTE;
	for (i = 0; i < src->n_dim; i++) {
	    dim = i+1;
	    cnt = _blkct (src->base_addr.a.ptr,dim,0);
	    hi = _hiidx (src->base_addr.a.ptr,dim,0,1);
	    low = _lowidx (src->base_addr.a.ptr,dim,0,1);
	    nbytes *= cnt * ((hi - low) + 1);
	}
	if ((*res_base_ptr = (void *) shmalloc(nbytes)) == NULL) {
	    _lerror (_LELVL_ABORT, FENOMEMY);
	}
	    
	res->orig_base = (void *) NULL;
	res->orig_size = nbytes * BITS_PER_BYTE;
	res->n_dim = src->n_dim;
	res->assoc = 1;
	res->ptr_alloc = 1;
	total_extent = src->type_lens.int_len / BITS_PER_WORD;
#pragma _CRI shortloop
	for (i = 0; i < src->n_dim; i++) {
	    res->dimension[i].extent = src->dimension[i].extent;
	    res->dimension[i].low_bound = 1;
	    res->dimension[i].stride_mult = total_extent;
	    total_extent *= res->dimension[i].extent;
	}
}

/****************************************************************************
 *
 * _malloc_result() -	Allocate a result temporary in private space,
 *			and fill in the result's dope vector.
 *
 * Arguments:	result		- ptr to dope vector for result array
 *		source		- ptr to dope vector for source array
 *		result_base_ptr	- ptr to result array
 *
 * Variables:	nbytes		- byte count to allocate memory for
 *		total_extent	- the total extent of result array
 *
 ***************************************************************************/

_malloc_result(
		DopeVectorType	*result,
		DopeVectorType	*source,
		void 		**result_base_ptr)
{
	long	nbytes, total_extent;
	int	i;

	if (!result->assoc) {
	    nbytes = source->type_lens.int_len / BITS_PER_BYTE;
#pragma _CRI	shortloop
	    for (i = 0; i < source->n_dim; i++)
		nbytes *= source->dimension[i].extent;
	    if ((*result_base_ptr = (void *) malloc(nbytes)) == NULL) {
		_lerror(_LELVL_ABORT, FENOMEMY);
	    }
	    result->orig_base = (void *) NULL;
	    result->orig_size = nbytes * BITS_PER_BYTE;
	    result->n_dim = source->n_dim;
	    result->assoc = 1;
	    result->ptr_alloc = 1;
	    total_extent = source->type_lens.int_len / BITS_PER_WORD;
#pragma _CRI shortloop
	    for (i = 0; i < source->n_dim; i++) {
		result->dimension[i].extent = source->dimension[i].extent;
		result->dimension[i].low_bound = 1;
		result->dimension[i].stride_mult = total_extent;
		total_extent *= result->dimension[i].extent;
	    }
	}
}


/**************************************************************************
 *
 * _shmalloc_reduced_result() - Allocate a result temporary in shared
 *                     space and fill in the result's dope vector.
 *
 * Arguments:      result	- ptr to dope vector for result array
 *                 source	- ptr to dope vector for source array
 *                 dim		- the dimension being operated over
 *                 result_base_ptr - ptr result array
 *
 * Variables:      nbytes	- byte count to allocate memory for
 *                 total_extent - the total extent of result array
 *
 *************************************************************************/

_shmalloc_reduced_result(DopeVectorType * result,
		 DopeVectorType * source,
	         long * dim,
		 void ** result_base_ptr)
{
	long nbytes_pe, nwords_pe, total_nwords, total_extent;
	int i;

	if (!result->assoc) {
	    total_nwords = 1;
#pragma _CRI shortloop
	    for (i=0; i < source->n_dim; i++) {
		if (i != *dim-1) {
		    total_nwords *= source->dimension[i].extent;
		}
	    }
	    nwords_pe = total_nwords / _N_PES;
	    if ((nwords_pe * _N_PES) < total_nwords) {
		nwords_pe++;
	    }
	    nbytes_pe = nwords_pe << 3;
	    if ((*result_base_ptr = (void *)shmalloc(nbytes_pe)) == NULL) {
		_lerror(_LELVL_ABORT,FENOMEMY);
	    }
	    result->orig_base = (void *)NULL;
	    result->orig_size = nbytes_pe;
	    result->n_dim = (source->n_dim) - 1;
	    result->assoc = 1;
	    result->ptr_alloc = 1;
	    total_extent = 1;
#pragma _CRI shortloop
	    for (i=0; i < *dim-1; i++) {
		result->dimension[i].extent =
			source->dimension[i].extent;
		result->dimension[i].low_bound = 1;
		result->dimension[i].stride_mult = total_extent;
		total_extent *= source->dimension[i].extent;
	    }
#pragma _CRI shortloop
	    for (i=*dim; i < (source->n_dim); i++) {
		result->dimension[i-1].extent = 
			source->dimension[i].extent;
		result->dimension[i-1].low_bound = 1;
		result->dimension[i-1].stride_mult = total_extent;
		total_extent *= source->dimension[i].extent;
	    }
	}
}



/**************************************************************************
 *
 * _malloc_reduced_result() - Allocate a result temporary in private
 *                   space and fill in the result's dope vector.
 *
 * Arguments:      result	- ptr to dope vector for result array
 *                 source	- ptr to dope vector for source array
 *                 dim		- the dimension being operated over        
 *                 result_base_ptr - ptr result array
 *
 * Variables:      nbytes	- byte count to allocate memory for
 *                 total_extent - the total extent of result array
 *
 *************************************************************************/

_malloc_reduced_result(DopeVectorType * result,
	       DopeVectorType * source,
	       long * dim,
	       void ** result_base_ptr)
{
	long nbytes, total_extent;
	int i;

	if (!result->assoc) {
	    nbytes = 1;
#pragma _CRI shortloop
	    for (i=0; i < source->n_dim; i++) {
		if (i != *dim-1) {
		nbytes *= source->dimension[i].extent;
		}
	    }
	    nbytes = nbytes << 3;
	    if ((*result_base_ptr = malloc(nbytes)) == NULL) {
		_lerror(_LELVL_ABORT,FENOMEMY);
	    }
	    result->base_addr.a.ptr = result_base_ptr;
	    result->orig_base = 0;
	    result->orig_size = nbytes;
	    result->n_dim = (source->n_dim) - 1;
	    result->assoc = 1;
	    result->ptr_alloc = 1;
	    total_extent = 1;
#pragma _CRI shortloop
	    for (i=0; i < *dim-1; i++) {
		result->dimension[i].extent =
			source->dimension[i].extent;
		result->dimension[i].low_bound = 1;
		result->dimension[i].stride_mult = total_extent;
		total_extent *= source->dimension[i].extent;
	    }
#pragma _CRI shortloop
	    for (i=*dim; i < (source->n_dim)-1; i++) {
		result->dimension[i].extent = 
			source->dimension[i+1].extent;
		result->dimension[i].low_bound = 1;
		result->dimension[i].stride_mult = total_extent;
		total_extent *= source->dimension[i+1].extent;
	    }
	}
}


/**************************************************************************
 *
 * _shmalloc_loc_result() - Allocate a result temporary in shared
 *                  space and fill in the result's dope vector.
 *
 *                  Used in the distributed versions of MAXLOC
 *                  and MINLOC.
 *
 * Arguments:      result       - ptr to dope vector for result array
 *                 source       - ptr to dope vector for source array
 *                 result_base_ptr - ptr result array
 *
 *************************************************************************/

_shmalloc_loc_result(DopeVectorType * result,
                     DopeVectorType * source,
                     long * result_base_ptr)
{
        if (!result->assoc) {
            *result_base_ptr = (long) shmalloc(8<<3);
            if (result_base_ptr == NULL) {
                     _lerror(_LELVL_ABORT,FENOMEMY);
            }
            result->orig_base = 0;
            result->orig_size = 0;
            result->n_dim = 1;
            result->dimension[0].low_bound = 1;
            result->dimension[0].extent = source->n_dim;
            result->dimension[0].stride_mult = 1;
            result->assoc = 1;
            result->ptr_alloc = 1;
        }
}


/**************************************************************************
 *
 * _malloc_loc_result() - Allocate a result temporary in private
 *                  space and fill in the result's dope vector.
 *
 *                  Used in the single processor versions of
 *                  MAXLOC and MINLOC.
 *
 * Arguments:      result       - ptr to dope vector for result array
 *                 source       - ptr to dope vector for source array
 *                 result_base_ptr - ptr result array
 *
 *************************************************************************/

_malloc_loc_result(DopeVectorType * result,
                   DopeVectorType * source,
                   long * result_base_ptr)
{
        if (!result->assoc) {
            if ((*result_base_ptr = (long)malloc
                        (source->n_dim << 3)) == NULL) {
                    _lerror(_LELVL_ABORT,FENOMEMY);
            }
            result->base_addr.a.ptr = result_base_ptr;
            result->orig_base = 0;
            result->orig_size = 0;
            result->n_dim = 1;
            result->dimension[0].low_bound = 1;
            result->dimension[0].extent = source->n_dim;
            result->dimension[0].stride_mult = 1;
            result->assoc = 1;
            result->ptr_alloc = 1;
        }
}
