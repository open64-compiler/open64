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


#pragma ident "@(#) libfi/array/cshift.c	92.1	07/07/99 15:52:02"

#include <stddef.h>
#include <liberrno.h>
#include <cray/dopevec.h>
#include <cray/portdefs.h>
#include "arraydefs.h"

/*
 *    Cshift function.  Perform a circular shift of an array
 *    expression of rank one or perform circular shifts on all
 *    complete rank one sections along a given dimension of an array
 *    expression of rank two or greater.  Elements shifted out at one
 *    end of a section are shifted in at the other end.  Different
 *    sections may be shifted by different amounts in different
 *    directions.
 */

#ifdef _UNICOS
#pragma	_CRI duplicate _CSHIFT as CSHIFT@
#endif
void
_CSHIFT (DopeVectorType * result,
	DopeVectorType * source,
	DopeVectorType * shift,
	_f_int	*dimp)
{
	char	*cs;	    	/* char ptr to source array	*/
	char	*cr;	    	/* char ptr to result array	*/
	char		* restrict cptr1;	/* char			*/
	char		* restrict cptr2;	/* char			*/
	_f_int8		* restrict uptr1;	/* 64-bit		*/
	_f_int8		* restrict uptr2;	/* 64-bit		*/
	_f_int		* restrict wptr1;	/* generic integer	*/
	_f_int		* restrict wptr2;	/* generic integer	*/
	_f_real16	* restrict dptr1;	/* 128-bit		*/
	_f_real16	* restrict dptr2;	/* 128-bit		*/
#ifdef _F_COMP16
	dblcmplx	* restrict xptr1;	/* 256-bit	     	*/
	dblcmplx	* restrict xptr2;	/* 256-bit	     	*/
#endif
	_f_int4		* restrict hptr1;	/* 32-bit		*/
	_f_int4		* restrict hptr2;	/* 32-bit		*/
	void		* restrict sptr;	/* ptr to src data area	*/
	void		* restrict rptr;	/* ptr to res data area	*/
	long		* restrict shptr;	/* ptr to shift area	*/
	_f_int8		* restrict save_uptr1;	/* save copy of uptr1	*/
	_f_int8		* restrict save_uptr2a;	/* save copy of uptr1	*/
	_f_int8		* restrict save_uptr2b;	/* save copy of uptr1	*/
	_f_int		* restrict save_wptr1;	/* save copy of wptr1	*/
	_f_int		* restrict save_wptr2a;	/* save copy of wptr1	*/
	_f_int		* restrict save_wptr2b;	/* save copy of wptr1	*/
	_f_int4		* restrict iptr4;    /* ptr to shift data area	*/
	_f_int8		* restrict iptr8;    /* ptr to shift data area	*/
	_f_int		bucketsize;	/* size of each data element	*/
	_f_int		shft_size;	/* size of stride elements	*/
	long	nbytes;		/* # of bytes in data area	*/
	_f_int	num;		/* index value			*/
	_f_int	bytealligned;	/* byte alligned flag		*/
	long	sindx;		/* source index			*/
	long	sindx2;		/* source index			*/
	long	shindx;		/* shift index			*/
	long	rindx;		/* result array index		*/
	long	rindx2;		/* result array index		*/
	_f_int	shft;		/* shift value			*/
	_f_int	dim;		/* dim value			*/
	_f_int	non_dim;	/* non-shift dim for 2x2	*/
	long	curdim[MAXDIM-1];   /* current indices		*/
	long	src_strd[MAXDIM-1]; /* index stride		*/
	long	src_ext[MAXDIM-1];  /* extents for source array	*/
	long	src_off[MAXDIM-1];  /* source offset		*/
	long	res_strd[MAXDIM-1]; /* index stride		*/
	long	res_off[MAXDIM-1];  /* result offset		*/
	long	shft_strd[MAXDIM-1];/* stride for shift array	*/
	long	shft_off[MAXDIM-1]; /* shift offset		*/
	_f_int	rank;		/* rank of source matrix	*/
	_f_int	type;		/* type				*/
	_f_int	subtype;	/* sub-type			*/
	_f_int	arithmetic;	/* arithmetic data type		*/
	long	extent;		/* extent temporary		*/
	long	extlow;		/* lower bound of extent	*/
	long	exthi;		/* upper bound of extent	*/
	long	src_dim_strd;	/* stride for source index	*/
	long	res_dim_strd;	/* stride for result index	*/
	long	shft_dim;	/* shift dimension		*/
	long	shft_dim_strd;	/* shift stride			*/
	long	tot_ext;	/* total extent			*/
	long	i, j, k, l;	/* index variables		*/

/*    Set type and dimension global variables	*/

	rank = source->n_dim;
	type = source->type_lens.type;

/*    Size calculation is based on variable type.	*/

	switch (type) {
	    case DVTYPE_ASCII :
		bytealligned = 1;
		bucketsize = _fcdlen (source->base_addr.charptr); /* bytes */
		subtype = DVSUBTYPE_CHAR;
		arithmetic = 0;
		break;
	    case DVTYPE_DERIVEDBYTE :
		bytealligned = 1;
		bucketsize = source->base_addr.a.el_len / BITS_PER_BYTE;
		subtype = DVSUBTYPE_CHAR;
		arithmetic = 0;
		break;
	    case DVTYPE_DERIVEDWORD :
		bytealligned = 0;
		bucketsize = source->base_addr.a.el_len / BITS_PER_WORD;
		subtype = DVSUBTYPE_DERIVED;
		arithmetic = 0;
		break;
	    default :
		bytealligned = 0;
		bucketsize = source->type_lens.int_len / BITS_PER_WORD;
		if (source->type_lens.int_len == 64) {
		    subtype = DVSUBTYPE_BIT64;
		} else if (source->type_lens.int_len == 32) {
		    subtype = DVSUBTYPE_BIT32;
		    bucketsize = 1;
		} else if (source->type_lens.int_len == 256) {
		    subtype = DVSUBTYPE_BIT256;
		} else {
		    subtype = DVSUBTYPE_BIT128;
		}
		arithmetic = 1;
	}
	shft_size = shift->base_addr.a.el_len / BITS_PER_WORD;
#ifdef	_CRAYMPP
	if (shft_size == 0)
	    shft_size = 1;
#endif

/*    If necessary, fill result dope vector	*/

	if (!result->assoc) {
	    result->base_addr.a.ptr  = (void *) NULL;
	    result->orig_base	   = 0;
	    result->orig_size	   = 0;
#ifdef _UNICOS
#pragma _CRI shortloop
#endif
	    for (i = 0, tot_ext = bucketsize; i < rank; i++) {
		result->dimension[i].extent = source->dimension[i].extent;
		result->dimension[i].low_bound = 1;
		result->dimension[i].stride_mult = tot_ext;
		tot_ext *= result->dimension[i].extent;
	    }

/*    Determine size of space to allocate    */

	    if (!bytealligned) {
		nbytes = bucketsize * BYTES_PER_WORD;
#ifdef _CRAYMPP
		if (subtype == DVSUBTYPE_BIT32)
		    nbytes /= 2;
#endif
	    } else {
		nbytes = bucketsize;
	    }
#ifdef _UNICOS
#pragma _CRI shortloop
#endif
	    for (i = 0; i < rank; i++)
		nbytes *= result->dimension[i].extent;
	    if (nbytes > 0) {
		result->base_addr.a.ptr = (void *) malloc(nbytes);
		if (result->base_addr.a.ptr == NULL)
		    _lerror(_LELVL_ABORT, FENOMEMY);
	    }

	    result->assoc = 1;
	    result->base_addr.a.el_len = source->base_addr.a.el_len;
	    if (type == DVTYPE_ASCII) {
		cr = (char *) result->base_addr.a.ptr;
		result->base_addr.charptr = _cptofcd (cr, bucketsize);
	    }
	    result->orig_base = result->base_addr.a.ptr;
	    result->orig_size = nbytes * BITS_PER_BYTE;
	}

/*
 *	Check to see if any of the matrices have size 0.  If any do,
 *	return without doing anything.
 */

#ifdef _UNICOS
#pragma	_CRI	shortloop
#endif
	for (i = 0; i < rank; i++) {
	    if (!source->dimension[i].extent)
		return;
	}
	if (result->assoc) {
#ifdef _UNICOS
#pragma	_CRI	shortloop
#endif
	    for (i = 0; i < rank; i++)
		if (!result->dimension[i].extent)
		    return;
	}
	if (shift->n_dim > 1) {
#ifdef _UNICOS
#pragma	_CRI	shortloop
#endif
	    for (i = 0; i < rank-1; i++)
		if (!shift->dimension[i].extent)
		    return;
	}

/*    Set up scalar pointers to all of the argument data areas    */

	if (!bytealligned) {
	    sptr = (void *) source->base_addr.a.ptr;
	    rptr = (void *) result->base_addr.a.ptr;
	} else {
	    if (type == DVTYPE_ASCII) {
		cs = _fcdtocp (source->base_addr.charptr);
		cr = _fcdtocp (result->base_addr.charptr);
	    } else {
		cs = (char *) source->base_addr.a.ptr;
		cr = (char *) result->base_addr.a.ptr;
	    }
	}
	shptr = (void *) shift->base_addr.a.ptr;

/*    If dim argument is not present, set it to 1 (0 for C)	*/

	if (dimp == NULL)
	    dim = 0;
	else {
	    if (*dimp < 1 || *dimp > rank)
		_lerror (_LELVL_ABORT, FESCIDIM);
	    dim = *dimp - 1;
	}

/*    If source is a 1-dimensional array    */

	if (rank == 1) {

/*
 *    Get shift value which will be used for each loop iteration.
 *    Once the value is obtained, make sure that it is positive.  This
 *    will ensure that the final calculated value can only be positive,
 *    thus eliminating a test from the inner loop.
 */

	    if (shift->base_addr.a.el_len == 64) {
		iptr8 = (_f_int8 *) shptr;
		shft = *iptr8 % source->dimension[0].extent;
	    } else {
		iptr4 = (_f_int4 *) shptr;
		shft = *iptr4 % source->dimension[0].extent;
	    }
	    if (shft < 0)
		shft += source->dimension[0].extent;

/*
 *    The index calculation for each element of the source and result
 *    matrices make use of some 'shortcuts'.  These involved the use
 *    of some variables which contain information about indices.  One
 *    of these variables exists for each dimension of the source and
 *    result matrices.
 *
 *    strd -	stride value based in terms of elements rather than
 *		words.
 */

	    if (bucketsize > 1 && arithmetic) {
		src_strd[0] = source->dimension[0].stride_mult / bucketsize;
		res_strd[0] = result->dimension[0].stride_mult / bucketsize;
	    } else {
		src_strd[0] = source->dimension[0].stride_mult;
		res_strd[0] = result->dimension[0].stride_mult;
	    }

/*
 *    In order to assist vectorization, the following value has been
 *    taken out of the array variable, and put into a scalar.
 */

	    extent = source->dimension[0].extent;

/*
 *    Each data type will be handled with its own inner loop.
 */

	    switch (subtype) {
		case DVSUBTYPE_CHAR :
		    cptr1 = (char *) cs;
		    cptr2 = (char *) cr + (extent - shft) * bucketsize;
		    src_dim_strd = source->dimension[0].stride_mult;
		    res_dim_strd = result->dimension[0].stride_mult;
		    for (i = 0; i < shft; i++) {
			(void) memcpy (cptr2, cptr1, bucketsize);
			cptr1 += src_dim_strd;
			cptr2 += res_dim_strd;
		    }
		    cptr2 = (char *) cr;
		    for ( ; i < extent; i++) {
			(void) memcpy (cptr2, cptr1, bucketsize);
			cptr1 += src_dim_strd;
			cptr2 += res_dim_strd;
		    }
		    break;

		case DVSUBTYPE_DERIVED :
		    src_dim_strd = source->dimension[0].stride_mult;
		    res_dim_strd = result->dimension[0].stride_mult;
		    for (i = 0; i < bucketsize; i++) {
			wptr1 = (_f_int *) sptr;
			wptr2 = (_f_int *) rptr + (extent-shft) *
					bucketsize;
			for (j = 0; j < shft; j++) {
			    sindx = i + (j * src_dim_strd);
			    rindx = i + (j * res_dim_strd);
			    wptr2[rindx] = wptr1[sindx];
			}
			wptr2 = (_f_int *) rptr;
			for (k = 0 ; j < extent; j++, k++) {
			    sindx = i + (j * src_dim_strd);
			    rindx = i + (k * res_dim_strd);
			    wptr2[rindx] = wptr1[sindx];
			}
		    }
		    break;

		case DVSUBTYPE_BIT64 :
		    uptr1 = (_f_int8 *) sptr;
		    uptr2 = (_f_int8 *) rptr + ((extent - shft) *
				res_strd[0]);
		    src_dim_strd = src_strd[0];
		    res_dim_strd = res_strd[0];
#ifndef CRAY2
		    for (i = 0; i < shft; i++) {
			sindx = i * src_dim_strd;
			rindx = i * res_dim_strd;
			uptr2[rindx] = uptr1[sindx];
		    }
		    uptr2 = (_f_int8 *) rptr;
		    for (j = 0 ; i < extent; i++, j++) {
			sindx = i * src_dim_strd;
			rindx = j * res_dim_strd;
			uptr2[rindx] = uptr1[sindx];
		    }
#else
		    memstride (uptr2, res_dim_strd, uptr1, src_dim_strd, shft);
		    uptr2 = (_f_int8 *) rptr;
		    uptr1 = (_f_int8 *) sptr + (shft * src_dim_strd);
		    shft = extent - shft;
		    memstride (uptr2, res_dim_strd, uptr1, src_dim_strd, shft);
#endif
		    break;

		case DVSUBTYPE_BIT32 :
		    hptr1 = (_f_int4 *) sptr;
		    hptr2 = (_f_int4 *) rptr + ((extent - shft) * res_strd[0]);
		    src_dim_strd = src_strd[0];
		    res_dim_strd = res_strd[0];
		    for (i = 0; i < shft; i++) {
			sindx = i * src_dim_strd;
			rindx = i * res_dim_strd;
			hptr2[rindx] = hptr1[sindx];
		    }
		    hptr2 = (_f_int4 *) rptr;
		    for (j = 0 ; i < extent; i++, j++) {
			sindx = i * src_dim_strd;
			rindx = j * res_dim_strd;
			hptr2[rindx] = hptr1[sindx];
		    }
		    break;

		case DVSUBTYPE_BIT128 :
		    dptr1 = (_f_real16 *) sptr;
		    dptr2 = (_f_real16 *) rptr +
				((extent - shft) * res_strd[0]);
		    src_dim_strd = src_strd[0];
		    res_dim_strd = res_strd[0];
		    for (i = 0; i < shft; i++) {
			sindx = i * src_dim_strd;
			rindx = i * res_dim_strd;
			dptr2[rindx] = dptr1[sindx];
		    }
		    dptr2 = (_f_real16 *) rptr;
		    for (j = 0 ; i < extent; i++, j++) {
			sindx = i * src_dim_strd;
			rindx = j * res_dim_strd;
			dptr2[rindx] = dptr1[sindx];
		    }
		    break;

#ifdef _F_COMP16
		case DVSUBTYPE_BIT256 :
		    xptr1 = (dblcmplx *) sptr;
		    xptr2 = (dblcmplx *) rptr + ((extent - shft) * res_strd[0]);
		    src_dim_strd = src_strd[0];
		    res_dim_strd = res_strd[0];
		    for (i = 0; i < shft; i++) {
			sindx = i * src_dim_strd;
			rindx = i * res_dim_strd;
			xptr2[rindx].re = xptr1[sindx].re;
			xptr2[rindx].im = xptr1[sindx].im;
		    }
		    xptr2 = (dblcmplx *) rptr;
		    for (j = 0 ; i < extent; i++, j++) {
			sindx = i * src_dim_strd;
			rindx = j * res_dim_strd;
			xptr2[rindx].re = xptr1[sindx].re;
			xptr2[rindx].im = xptr1[sindx].im;
		    }
		    break;
#endif

		default :
		    _lerror (_LELVL_ABORT, FEINTDTY);
	    }

/*    Arrays with rank of 2	*/

	} else if (rank == 2) {

/*    Set dimension and non_dimension indices	*/

	    if (dim == 0)
		non_dim = 1;
	    else
		non_dim = 0;

/*    Set up shortcut variables for both array dimensions    */

	    if (bucketsize > 1 && arithmetic) {
		src_strd[0] = source->dimension[0].stride_mult / bucketsize;
		src_strd[1] = source->dimension[1].stride_mult / bucketsize;
		res_strd[0] = result->dimension[0].stride_mult / bucketsize;
		res_strd[1] = result->dimension[1].stride_mult / bucketsize;
	    } else {
		src_strd[0] = source->dimension[0].stride_mult;
		src_strd[1] = source->dimension[1].stride_mult;
		res_strd[0] = result->dimension[0].stride_mult;
		res_strd[1] = result->dimension[1].stride_mult;
	    }
	    shft_strd[0] = shift->dimension[0].stride_mult / shft_size;

/*    Put information about DIM index in scalars for vectorization */

	    src_dim_strd = src_strd[dim];
	    res_dim_strd = res_strd[dim];

/*    Set up extent temporary variables    */

	    extent = source->dimension[dim].extent;
	    extlow = -extent;
	    exthi  = 2 * extent;

/*	Set up shift variables		*/

	    if (shift->base_addr.a.el_len == 64)
		iptr8 = (_f_int8 *) shptr;
	    else
		iptr4 = (_f_int4 *) shptr;
	    if (shift->n_dim == 1) {
		shft_dim = 1;
		shft_dim_strd = shift->dimension[0].stride_mult / shft_size;
		shindx = 0;
	    } else {			/* scalar value, only get it once */
		shft_dim = 0;
		if (shift->base_addr.a.el_len == 64)
		    shft = iptr8[0];
		else
		    shft = iptr4[0];
		if (shft >= extent) {
		    if (shft < exthi)
			shft -= extent;
		    else
			shft %= extent;
		} else if (shft < 0) {
		    if (shft >= extlow)
			shft += extent;
		    else {
			shft %= extent;
			if (shft < 0)
			    shft += extent;
		    }
		}
	    }

/*    Outer loop is for dimension not being shifted    */

	    for (i = 0; i < source->dimension[non_dim].extent; i++) {

/*	Get shift value if it is not a scalar	*/

		if (shft_dim == 1) {
		    if (shift->base_addr.a.el_len == 64)
			shft = iptr8[shindx];
		    else
			shft = iptr4[shindx];
		    if (shft >= extent) {
			if (shft < exthi)
			    shft -= extent;
			else
			    shft %= extent;
		    } else if (shft < 0) {
			if (shft >= extlow)
			    shft += extent;
			else {
			    shft %= extent;
			    if (shft < 0)
				shft += extent;
			}
		    }
		    shindx += shft_dim_strd;
		}

		switch (subtype) {
		    case DVSUBTYPE_CHAR :
			cptr1 = (char *) cs + (i * src_strd[non_dim]);
			cptr2 = (char *) cr + (i * res_strd[non_dim]) +
				((extent - shft) * res_dim_strd);
			for (j = 0; j < shft; j++) {
			    (void) memcpy (cptr2, cptr1, bucketsize);
			    cptr1 += src_dim_strd;
			    cptr2 += src_dim_strd;
			}
			cptr2 = (char *) cr + (i * res_strd[non_dim]);
			for ( ; j < extent; j++) {
			    (void) memcpy (cptr2, cptr1, bucketsize);
			    cptr1 += src_dim_strd;
			    cptr2 += src_dim_strd;
			}
			break;

		    case DVSUBTYPE_DERIVED :
			wptr1 = (_f_int *) sptr +
				(i * src_strd[non_dim]);
			save_wptr1 = wptr1;
			wptr2 = (_f_int *) rptr +
				(i * res_strd[non_dim]) +
				((extent - shft) * res_dim_strd);
			save_wptr2a = wptr2;
			wptr2 = (_f_int *) rptr +
				(i * res_strd[non_dim]);
			save_wptr2b = wptr2;
			for (j = 0; j < bucketsize; j++) {
			    wptr1 = save_wptr1;
			    wptr2 = save_wptr2a;
			    for (k = 0; k < shft; k++) {
				sindx = j + (k * src_dim_strd);
				rindx = j + (k * res_dim_strd);
				wptr2[rindx] = wptr1[sindx];
			    }
			    wptr2 = save_wptr2b;
			    for (l = 0; k < extent; k++, l++) {
				sindx = j + (k * src_dim_strd);
				rindx = j + (l * res_dim_strd);
				wptr2[rindx] = wptr1[sindx];
			    }
			}
			break;

		    case DVSUBTYPE_BIT64 :
			uptr1 = (_f_int8 *) sptr +
				(i * src_strd[non_dim]);
			save_uptr1 = uptr1;
			uptr2 = (_f_int8 *) rptr +
				(i * res_strd[non_dim]) +
				 ((extent - shft) * res_dim_strd);
#ifndef CRAY2
			for (j = 0; j < shft; j++) {
			    sindx = j * src_dim_strd;
			    rindx = j * res_dim_strd;
			    uptr2[rindx] = uptr1[sindx];
			}
			uptr2 = (_f_int8 *) rptr +
				(i * res_strd[non_dim]);
			for (k = 0 ; j < extent; j++, k++) {
			    sindx = j * src_dim_strd;
			    rindx = k * res_dim_strd;
			    uptr2[rindx] = uptr1[sindx];
			}
#else
			memstride (uptr2, res_dim_strd, uptr1, src_dim_strd,
				shft);
			uptr2 = (_f_int8 *) rptr +
				(i * res_strd[non_dim]);
			uptr1 = save_uptr1 + (shft * src_dim_strd);
			memstride (uptr2, res_dim_strd, uptr1, src_dim_strd,
				extent-shft);
#endif
			break;

		    case DVSUBTYPE_BIT32 :
			hptr1 = (_f_int4 *) sptr + (i * src_strd[non_dim]);
			hptr2 = (_f_int4 *) rptr + (i * res_strd[non_dim]) +
				((extent - shft) * res_dim_strd);
			for (j = 0; j < shft; j++) {
			    sindx = j * src_dim_strd;
			    rindx = j * res_dim_strd;
			    hptr2[rindx] = hptr1[sindx];
			}
			hptr2 = (_f_int4 *) rptr + (i * res_strd[non_dim]);
			for (k = 0 ; j < extent; j++, k++) {
			    sindx = j * src_dim_strd;
			    rindx = k * res_dim_strd;
			    hptr2[rindx] = hptr1[sindx];
			}
			break;

		    case DVSUBTYPE_BIT128 :
			dptr1 = (_f_real16 *) sptr + (i * src_strd[non_dim]);
			dptr2 = (_f_real16 *) rptr + (i * res_strd[non_dim]) +
				((extent - shft) * res_dim_strd);
			for (j = 0; j < shft; j++) {
			    sindx = j * src_dim_strd;
			    rindx = j * res_dim_strd;
			    dptr2[rindx] = dptr1[sindx];
			}
			dptr2 = (_f_real16 *) rptr + (i * res_strd[non_dim]);
			for (k = 0 ; j < extent; j++, k++) {
			    sindx = j * src_dim_strd;
			    rindx = k * res_dim_strd;
			    dptr2[rindx] = dptr1[sindx];
			}
			break;

#ifdef _F_COMP16
		    case DVSUBTYPE_BIT256 :
			xptr1 = (dblcmplx *) sptr + (i * src_strd[non_dim]);
			xptr2 = (dblcmplx *) rptr + (i * res_strd[non_dim]) +
				((extent - shft) * res_dim_strd);
			for (j = 0; j < shft; j++) {
			    sindx = j * src_dim_strd;
			    rindx = j * res_dim_strd;
			    xptr2[rindx].re = xptr1[sindx].re;
			    xptr2[rindx].im = xptr1[sindx].im;
			}
			xptr2 = (dblcmplx *) rptr + (i * res_strd[non_dim]);
			for (k = 0 ; j < extent; j++, k++) {
			    sindx = j * src_dim_strd;
			    rindx = k * res_dim_strd;
			    xptr2[rindx].re = xptr1[sindx].re;
			    xptr2[rindx].im = xptr1[sindx].im;
			}
			break;
#endif

		    default :
			_lerror (_LELVL_ABORT, FEINTDTY);
		    }
		}

/*    Arrays with rank of 3-7	*/

	} else {

	    if (dim == 0) {
		i = 0;
		tot_ext = 1;
	    } else
#ifdef _UNICOS
#pragma _CRI    shortloop
#endif
	    for (i = 0, tot_ext = 1; i < dim; i++) {
		tot_ext *= source->dimension[i].extent;
		src_ext[i] = source->dimension[i].extent;
		if (bucketsize > 1 && arithmetic) {
		    src_strd[i] = source->dimension[i].stride_mult / bucketsize;
		    res_strd[i] = result->dimension[i].stride_mult / bucketsize;
		} else {
		    src_strd[i] = source->dimension[i].stride_mult;
		    res_strd[i] = result->dimension[i].stride_mult;
		}
	    }
	    if (i < (rank - 1))
#ifdef _UNICOS
#pragma _CRI    shortloop
#endif
	      for ( ; i < rank-1; i++) {
		tot_ext *= source->dimension[i+1].extent;
		src_ext[i] = source->dimension[i+1].extent;
		if (bucketsize > 1 && arithmetic) {
		    src_strd[i] = source->dimension[i+1].stride_mult/bucketsize;
		    res_strd[i] = result->dimension[i+1].stride_mult/bucketsize;
		} else {
		    src_strd[i] = source->dimension[i+1].stride_mult;
		    res_strd[i] = result->dimension[i+1].stride_mult;
		}
	      }

/*    Set up some scalars which contain information about DIM    */

	    extent = source->dimension[dim].extent;
	    extlow = -extent;
	    exthi = 2 * extent;

	    if (bucketsize > 1 && arithmetic) {
		src_dim_strd = source->dimension[dim].stride_mult / bucketsize;
		res_dim_strd = result->dimension[dim].stride_mult / bucketsize;
	    } else {
		src_dim_strd = source->dimension[dim].stride_mult;
		res_dim_strd = result->dimension[dim].stride_mult;
	    }

/*	Set up the shift variables		*/

	    if (shift->base_addr.a.el_len == 64)
	        iptr8 = (_f_int8 *) shptr;
	    else
	        iptr4 = (_f_int4 *) shptr;
	    if (shift->n_dim == 0) {
		shft_dim = 0;
		if (shift->base_addr.a.el_len == 64)
		    shft = iptr8[0];
		else
		    shft = iptr4[0];
		if (shft >= extent) {
		    if (shft < exthi)
			shft -= extent;
		    else
			shft %= extent;
		} else if (shft < 0) {
		    if (shft >= extlow)
			shft += extent;
		    else {
			shft %= extent;
			if (shft < 0)
			    shft += extent;
		    }
		}
	    } else {
		shft_dim = 1;
		shindx = 0;
#ifdef _UNICOS
#pragma	_CRI	shortloop
#endif
		for (i = 0; i < rank-1; i++) {
		    shft_strd[i] = shift->dimension[i].stride_mult;
		}
	    }

/*	Initialize the curdim and offset arrays to 0		*/

	    for (i = 0; i < rank-1; i++) {
		curdim[i] = 0;
		src_off[i] = 0;
		res_off[i] = 0;
		shft_off[i] = 0;
	    }

/*
 *    The outer loop will be executed once for each combination of
 *    indices not including the DIM dimension.
 */

	    for (i = 0; i < tot_ext; i++) {

/*    Calculate the shift value used throughout the inner loop    */

		if (shft_dim) {
		    switch (rank) {
			case 3 :
			    shindx = shft_off[0] + shft_off[1];
			    break;
			case 4 :
			    shindx = shft_off[0] + shft_off[1] + shft_off[2];
			    break;
			case 5 :
			    shindx = shft_off[0] + shft_off[1] +
				     shft_off[2] + shft_off[3];
			    break;
			case 6 :
			    shindx = shft_off[0] + shft_off[1] + shft_off[2] +
				     shft_off[3] + shft_off[4];
			    break;
			default :
			    shindx = shft_off[0] + shft_off[1] + shft_off[2] +
				     shft_off[3] + shft_off[4] + shft_off[5];
		    }
		    shft = shptr[shindx];
		    if (shft >= extent) {
			if (shft < exthi)
			    shft -= extent;
			else
			    shft %= extent;
		    } else if (shft < 0) {
			if (shft >= extlow)
			    shft += extent;
			else {
			    shft %= extent;
			    if (shft < 0)
				shft += extent;
			}
		    }
		}

		switch (rank) {
		    case 3 :
			sindx = src_off[0] + src_off[1];
			rindx = res_off[0] + res_off[1];
			break;
		    case 4 :
			sindx = src_off[0] + src_off[1] + src_off[2];
			rindx = res_off[0] + res_off[1] + res_off[2];
			break;
		    case 5 :
			sindx = src_off[0] + src_off[1] +
				src_off[2] + src_off[3];
			rindx = res_off[0] + res_off[1] +
				res_off[2] + res_off[3];
			break;
		    case 6 :
			sindx = src_off[0] + src_off[1] + src_off[2] +
				src_off[3] + src_off[4];
			rindx = res_off[0] + res_off[1] + res_off[2] +
				res_off[3] + res_off[4];
			break;
		    default :
			sindx = src_off[0] + src_off[1] + src_off[2] +
				src_off[3] + src_off[4] + src_off[5];
			rindx = res_off[0] + res_off[1] + res_off[2] +
				res_off[3] + res_off[4] + res_off[5];
		}

		switch (subtype) {
		    case DVSUBTYPE_CHAR :
			cptr1 = (char *) cs + sindx;
			cptr2 = (char *) cr + rindx +
				((extent - shft) * res_dim_strd);
			for (j = 0; j < shft; j++) {
			    (void) memcpy (cptr2, cptr1, bucketsize);
			    cptr1 += src_dim_strd;
			    cptr2 += res_dim_strd;
			}
			cptr2 = (char *) cr + rindx;
			for ( ; j < extent; j++) {
			    (void) memcpy (cptr2, cptr1, bucketsize);
			    cptr1 += src_dim_strd;
			    cptr2 += res_dim_strd;
			}
			break;

		    case DVSUBTYPE_DERIVED :
			wptr1 = (_f_int *) sptr + sindx;
			save_wptr1 = wptr1;
			wptr2 = (_f_int *) rptr + rindx +
				((extent - shft) * res_dim_strd);
			save_wptr2a = wptr2;
			wptr2 = (_f_int *) rptr + rindx;
			save_wptr2b = wptr2;
			for (j = 0; j < bucketsize; j++) {
			    wptr1 = save_wptr1;
			    wptr2 = save_wptr2a;
			    for (k = 0; k < shft; k++) {
				sindx2 = j + (k * src_dim_strd);
				rindx2 = j + (k * res_dim_strd);
				wptr2[rindx2] = wptr1[sindx2];
			    }
			    wptr2 = save_wptr2b;
			    for (l = 0; k < extent; k++, l++) {
				sindx2 = j + (k * src_dim_strd);
				rindx2 = j + (l * res_dim_strd);
				wptr2[rindx2] = wptr1[sindx2];
			    }
			}
			break;

		    case DVSUBTYPE_BIT64 :
			uptr1 = (_f_int8 *) sptr + sindx;
			save_uptr1 = uptr1;
			uptr2 = (_f_int8 *) rptr + rindx +
				((extent - shft) * res_dim_strd);
#ifndef CRAY2
			for (j = 0; j < shft; j++) {
			    sindx2 = j * src_dim_strd;
			    rindx2 = j * res_dim_strd;
			    uptr2[rindx2] = uptr1[sindx2];
			}
			uptr2 = (_f_int8 *) rptr + rindx;
			for (k = 0 ; j < extent; j++, k++) {
			    sindx2 = j * src_dim_strd;
			    rindx2 = k * res_dim_strd;
			    uptr2[rindx2] = uptr1[sindx2];
			}
#else
			memstride (uptr2, res_dim_strd, uptr1, src_dim_strd,
				shft);
			uptr2 = (_f_int8 *) rptr + rindx;
			uptr1 = save_uptr1 + (shft * src_dim_strd);
			memstride (uptr2, res_dim_strd, uptr1, src_dim_strd,
				extent-shft);
#endif
			break;

		    case DVSUBTYPE_BIT32 :
			hptr1 = (_f_int4 *) sptr + sindx;
			hptr2 = (_f_int4 *) rptr + rindx +
				((extent - shft) * res_dim_strd);
			for (j = 0; j < shft; j++) {
			    sindx2 = j * src_dim_strd;
			    rindx2 = j * res_dim_strd;
			    hptr2[rindx2] = hptr1[sindx2];
			}
			hptr2 = (_f_int4 *) rptr + rindx;
			for (k = 0; j < extent; j++, k++) {
			    sindx2 = j * src_dim_strd;
			    rindx2 = k * res_dim_strd;
			    hptr2[rindx2] = hptr1[sindx2];
			}
			break;

		    case DVSUBTYPE_BIT128 :
			dptr1 = (_f_real16 *) sptr + sindx;
			dptr2 = (_f_real16 *) rptr + rindx +
				((extent - shft) * res_dim_strd);
			for (j = 0; j < shft; j++) {
			    sindx2 = j * src_dim_strd;
			    rindx2 = j * res_dim_strd;
			    dptr2[rindx2] = dptr1[sindx2];
			}
			dptr2 = (_f_real16 *) rptr + rindx;
			for (k = 0; j < extent; j++, k++) {
			    sindx2 = j * src_dim_strd;
			    rindx2 = k * res_dim_strd;
			    dptr2[rindx2] = dptr1[sindx2];
			}
			break;

#ifdef _F_COMP16
		    case DVSUBTYPE_BIT256 :
			xptr1 = (dblcmplx *) sptr + sindx;
			xptr2 = (dblcmplx *) rptr + rindx +
				((extent - shft) * res_dim_strd);
			for (j = 0; j < shft; j++) {
			    sindx2 = j * src_dim_strd;
			    rindx2 = j * res_dim_strd;
			    xptr2[rindx2].re = xptr1[sindx2].re;
			    xptr2[rindx2].im = xptr1[sindx2].im;
			}
			xptr2 = (dblcmplx *) rptr + rindx;
			for (k = 0; j < extent; j++, k++) {
			    sindx2 = j * src_dim_strd;
			    rindx2 = k * res_dim_strd;
			    xptr2[rindx2].re = xptr1[sindx2].re;
			    xptr2[rindx2].im = xptr1[sindx2].im;
			}
			break;
#endif

		    default :
			_lerror (_LELVL_ABORT, FEINTDTY);
		}

/*    Increment the current dimension counter.    */

		curdim[0]++;
		if (curdim[0] < src_ext[0]) {
		    src_off[0] += src_strd[0];
		    res_off[0] += res_strd[0];
		    shft_off[0] += shft_strd[0];
		} else {
		    curdim[0] = 0;
		    src_off[0] = 0;
		    res_off[0] = 0;
		    shft_off[0] = 0;
		    curdim[1]++;
		    if (curdim[1] < src_ext[1]) {
			src_off[1] += src_strd[1];
			res_off[1] += res_strd[1];
			shft_off[1] += shft_strd[1];
		    } else {
			curdim[1] = 0;
			src_off[1] = 0;
			res_off[1] = 0;
			shft_off[1] = 0;
			curdim[2]++;
			if (curdim[2] < src_ext[2]) {
			    src_off[2] += src_strd[2];
			    res_off[2] += res_strd[2];
			    shft_off[2] += shft_strd[2];
			} else {
			    curdim[2] = 0;
			    src_off[2] = 0;
			    res_off[2] = 0;
			    shft_off[2] = 0;
			    curdim[3]++;
			    if (curdim[3] < src_ext[3]) {
				src_off[3] += src_strd[3];
				res_off[3] += res_strd[3];
				shft_off[3] += shft_strd[3];
			    } else {
				curdim[3] = 0;
				src_off[3] = 0;
				res_off[3] = 0;
				shft_off[3] = 0;
				curdim[4]++;
				if (curdim[4] < src_ext[4]) {
		    		    src_off[4] += src_strd[4];
				    res_off[4] += res_strd[4];
				    shft_off[4] += shft_strd[4];
				} else {
				    curdim[4] = 0;
				    src_off[4] = 0;
				    res_off[4] = 0;
				    shft_off[4] = 0;
				    curdim[5]++;
				    if (curdim[5] < src_ext[5]) {
					src_off[5] += src_strd[5];
					res_off[5] += res_strd[5];
					shft_off[5] += shft_strd[5];
				    }
				}
			    }
			}
		    }
		}
	    }
	}
}
