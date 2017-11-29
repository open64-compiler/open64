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


#pragma ident "@(#) libfi/array/lbound.c	92.2	07/07/99 15:52:02"
#include <liberrno.h>
#include <stddef.h>
#include <cray/dopevec.h>
#include <cray/portdefs.h>

#include <stdlib.h>

#define BITS_PER_BYTE   (BITS_PER_WORD / BYTES_PER_WORD)

/*
 *      LBOUND  Returns the low_bounds for a source array in a rank one
 *                result array. A null argument DIM is present but not used.
 *              If source pointer/allocatable array is not 
 *                associated/allocated, return an error.
 *              If result rank one array has not been allocated, fill parts
 *                of result dope vector and allocate space for result.
 *              When extent of a dimension is nonzero, lbound = low_bound.
 *                Otherwise, lbound=one.
 */

void
_LBOUND (DopeVectorType * result,
	 DopeVectorType * source,
	_f_int	*dimptr)
{
	int rank;
	int numbytes;
	int *destarry;
	_f_int4	*resptr4;
	_f_int8	*resptr8;
	int loopj;

	/* If source is a pointer/allocatable array, it must be
	 * associated/allocated.  */
	if (source->p_or_a  &&  !source->assoc)
		_lerror ( _LELVL_ABORT, FENMPTAR, "LBOUND");

	/* target is rank-one array with extent source.n_dim */
	rank = source->n_dim;

	/* If result array is not allocated */
	if (!result->assoc) {
		result->base_addr.a.ptr  = (void *) NULL;
		result->dimension[0].extent = rank;
		result->dimension[0].low_bound = 1;
		result->dimension[0].stride_mult =
		  result->type_lens.int_len / (sizeof(_f_int) *
		    BITS_PER_BYTE);
		numbytes = rank * BYTES_PER_WORD;
		/* allocate rank in bytes for temporary array */
		destarry = (void *) malloc (numbytes);
		if (destarry == NULL)
			_lerror(_LELVL_ABORT,FENOMEMY);
		result->base_addr.a.ptr = (void *) destarry;
		result->assoc = 1;
	}

	if (result->type_lens.kind_or_star == 0) {
		if (result->type_lens.int_len == 64) {
			resptr8 = (_f_int8 *) result->base_addr.a.ptr;
			for (loopj = 0; loopj < rank; loopj++)
				if( source->dimension[loopj].extent != 0)
					resptr8[loopj] = (_f_int8)
					source->dimension[loopj].low_bound;
				else
					resptr8[loopj] = (_f_int8) 1;
		} else {
			resptr4 = (_f_int4 *) result->base_addr.a.ptr;
			for (loopj = 0; loopj < rank; loopj++)
				if( source->dimension[loopj].extent != 0)
					resptr4[loopj] = (_f_int4)
					source->dimension[loopj].low_bound;
				else
					resptr4[loopj] = (_f_int4) 1;
		}
	} else {
		if (result->type_lens.dec_len == 8) {
			resptr8 = (_f_int8 *) result->base_addr.a.ptr;
			for (loopj = 0; loopj < rank; loopj++)
				if( source->dimension[loopj].extent != 0)
					resptr8[loopj] = (_f_int8)
					source->dimension[loopj].low_bound;
				else
					resptr8[loopj] = (_f_int8) 1;
		} else if (result->type_lens.dec_len == 4) {
			resptr4 = (_f_int4 *) result->base_addr.a.ptr;
			for (loopj = 0; loopj < rank; loopj++)
				if( source->dimension[loopj].extent != 0)
					resptr4[loopj] = (_f_int4)
					source->dimension[loopj].low_bound;
				else
					resptr4[loopj] = (_f_int4) 1;
		}
	}
}

/*
 *      LBOUND0 Returns scalar low_bound for a specified dimension DIM of
 *                an array.
 *              If the source pointer/allocatable array is not 
 *                associated/allocated,  return an error.
 *              If DIM is outside the rank of the array, return an error.
 *              When extent of specified dimension is nonzero, return
 *                lbound=low_bound.  Otherwise, return lbound=one.
 */

_f_int
_LBOUND0(DopeVectorType * source,
	_f_int	*dimptr)
{
        int iresult;
        int dim;
        int rank;

	/* If source is a pointer/allocatable array, it must be
	 * associated/allocated. */
	if (source->p_or_a  &&  !source->assoc)
		_lerror ( _LELVL_ABORT, FENMPTAR, "LBOUND");

	/* argument DIM must be within source array rank */
	rank = source->n_dim;
	dim = *dimptr - 1;
	if (dim < 0 || dim >= rank)
                _lerror (_LELVL_ABORT, FENMSCDM, "LBOUND");

	/* Return low_bound for nonzero extent, else return one */
	if(source->dimension[dim].extent != 0)
		iresult = source->dimension[dim].low_bound;
	else
		iresult = 1;

        return(iresult);
}

/*
 *      LBOUND0_4 Returns scalar low_bound for a specified dimension DIM of
 *                an array.
 *              If the source pointer/allocatable array is not 
 *                associated/allocated,  return an error.
 *              If DIM is outside the rank of the array, return an error.
 *              When extent of specified dimension is nonzero, return
 *                lbound=low_bound.  Otherwise, return lbound=one.
 */

#if defined (_UNICOS)
#pragma duplicate _LBOUND0_4 as _LBOUND0_2
#pragma duplicate _LBOUND0_4 as _LBOUND0_1
#endif

_f_int4
_LBOUND0_4(DopeVectorType * source,
	_f_int	*dimptr)
{
        _f_int4 iresult;
        int dim;
        int rank;

	/* If source is a pointer/allocatable array, it must be
	 * associated/allocated. */
	if (source->p_or_a  &&  !source->assoc)
		_lerror ( _LELVL_ABORT, FENMPTAR, "LBOUND");

	/* argument DIM must be within source array rank */
	rank = source->n_dim;
	dim = *dimptr - 1;
	if (dim < 0 || dim >= rank)
                _lerror (_LELVL_ABORT, FENMSCDM, "LBOUND");

	/* Return low_bound for nonzero extent, else return one */
	if(source->dimension[dim].extent != 0)
		iresult = (_f_int4) source->dimension[dim].low_bound;
	else
		iresult = (_f_int4) 1;

        return(iresult);
}

/*
 *      LBOUND0_8 Returns scalar low_bound for a specified dimension DIM of
 *                an array.
 *              If the source pointer/allocatable array is not 
 *                associated/allocated,  return an error.
 *              If DIM is outside the rank of the array, return an error.
 *              When extent of specified dimension is nonzero, return
 *                lbound=low_bound.  Otherwise, return lbound=one.
 */

_f_int8
_LBOUND0_8(DopeVectorType * source,
	_f_int	*dimptr)
{
        _f_int8 iresult;
        int dim;
        int rank;

	/* If source is a pointer/allocatable array, it must be
	 * associated/allocated. */
	if (source->p_or_a  &&  !source->assoc)
		_lerror ( _LELVL_ABORT, FENMPTAR, "LBOUND");

	/* argument DIM must be within source array rank */
	rank = source->n_dim;
	dim = *dimptr - 1;
	if (dim < 0 || dim >= rank)
                _lerror (_LELVL_ABORT, FENMSCDM, "LBOUND");

	/* Return low_bound for nonzero extent, else return one */
	if(source->dimension[dim].extent != 0)
		iresult = (_f_int8) source->dimension[dim].low_bound;
	else
		iresult = (_f_int8) 1;

        return(iresult);
}
