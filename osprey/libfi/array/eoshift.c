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


#pragma ident "@(#) libfi/array/eoshift.c	92.1	07/07/99 15:52:02"

#include <stddef.h>
#include <liberrno.h>
#include <cray/dopevec.h>
#include <cray/portdefs.h>
#include "arraydefs.h"

/*
 *	EOSHIFT routine.  Perform an end-off shift of an array expression of
 *	rank one or perform end-off shifts on all the complete rank-one
 *	sections along a given dimension of an array expression of rank two
 *	or greater.  Elements are shifted off at one end of a section and
 *	copies of a boundary value are shifted in at the other end.  Different
 *	sections may have different boundary values and may be shifted by
 *	different amounts and in different directions.
 */

/*	Declare constants used for default boundary values	*/

const _f_int1	defaulti1 = 0;
const _f_int2	defaulti2 = 0;
const _f_int4	defaulti4 = 0;
const _f_int8	defaulti8 = 0;
const _f_log1	defaultl1 = _btol(0);
const _f_log2	defaultl2 = _btol(0);
const _f_log4	defaultl4 = _btol(0);
const _f_log8	defaultl8 = _btol(0);
const _f_real4	defaultr4 = 0.0;
const _f_real8	defaultr8 = 0.0;

#if defined _F_REAL16 && _F_REAL16 != (-1)
#if defined(_WORD32) || defined(__mips)
const _f_comp8	defaultr16 = {0.0,0.0};
#else
const _f_comp8	defaultr16 = 0.0 + 0.0i;
#endif /* _WORD32 or __mips */
#endif

#ifdef _F_COMP4
#if _F_COMP4 > 0
const _f_comp4	defaultc4 = 0.0 + 0.0i;
#else
const _f_comp4	defaultc4 = {0.0, 0.0};
#endif
#endif

#ifdef _F_COMP8
#if _F_COMP8 > 0
const _f_comp8	defaultc8 = 0.0 + 0.0i;
#else
const _f_comp8	defaultc8 = {0.0, 0.0};
#endif
#endif

#ifdef _F_COMP4
#define	BIT64_DEFAULT()							\
	if (type == DVTYPE_INTEGER)					\
	    bnd64 = *(_f_int8 *) &defaulti8;				\
	else if (type == DVTYPE_REAL)					\
	    bnd64 = *(_f_int8 *) &defaultr8;				\
	else if (type == DVTYPE_COMPLEX) {				\
	    bnd64 = *(_f_int8 *) &defaultc4;				\
	} else								\
	    bnd64 = *(_f_int8 *) &defaultl8;
#else
#define	BIT64_DEFAULT()							\
	if (type == DVTYPE_INTEGER)					\
	    bnd64 = *(_f_int8 *) &defaulti8;				\
	else if (type == DVTYPE_REAL)					\
	    bnd64 = *(_f_int8 *) &defaultr8;				\
	else								\
	    bnd64 = *(_f_int8 *) &defaultl8;
#endif

#if defined _F_REAL16
#if _F_REAL16 > 1
#define	BIT128_DEFAULT()						\
	if (type == DVTYPE_REAL)					\
	    bnd128 = *(_f_comp8 *) &defaultr16;				\
	else								\
	    bnd128 = *(_f_comp8 *) &defaultc8;
#else
#define BIT128_DEFAULT()						\
	bnd128 = *(_f_comp8 *) &defaultc8;
#endif
#endif

#ifdef _F_COMP16
#define BIT256_DEFAULT()						\
	bnd256.re = *(_f_real16 *) &defaultr16;				\
	bnd256.im = *(_f_real16 *) &defaultr16;
#endif

#define	BIT32_DEFAULT()							\
	if (type == DVTYPE_INTEGER) {					\
	    bnd32 = *(_f_int4 *) &defaulti4;				\
	} else if (type == DVTYPE_REAL) {				\
	    bnd32 = *(_f_int4 *) &defaultr4;				\
	} else {							\
	    bnd32 = *(_f_int4 *) &defaultl4;				\
	}

#define	BIT16_DEFAULT()							\
	if (type == DVTYPE_INTEGER) {					\
	    bnd16 = *(_f_int2 *) &defaulti2;				\
	} else {							\
	    bnd16 = *(_f_int2 *) &defaultl2;				\
	}

#define	BIT8_DEFAULT()							\
	if (type == DVTYPE_INTEGER) {					\
	    bnd8 = *(_f_int1 *) &defaulti1;				\
	} else {							\
	    bnd8 = *(_f_int1 *) &defaultl1;				\
	}


#ifdef _UNICOS
#pragma	_CRI duplicate _EOSHIFT as EOSHIFT@
#endif
void
_EOSHIFT (DopeVectorType * result,
	DopeVectorType * source,
	DopeVectorType * shift,
	DopeVectorType * boundary,
	_f_int	*dimp)
{
	char	*cs;			/* char ptr to source array	*/
	char	*cr;			/* char ptr to result array	*/
	char	*cb;			/* char ptr to boundary array	*/
	char		* restrict cptr1;	/* char			*/
	char		* restrict cptr2;	/* char			*/
	char		* restrict cptr3;	/* char			*/
	_f_int8		* restrict uptr1;	/* 64-bit		*/
	_f_int8		* restrict uptr2;	/* 64-bit		*/
	_f_int8		* restrict uptr3;	/* 64-bit		*/
	_f_comp8	* restrict xptr1;	/* 128-bit		*/
	_f_comp8	* restrict xptr2;	/* 128-bit		*/
	_f_comp8	* restrict xptr3;	/* 128-bit		*/
	_f_int		* restrict fptr1;	/* default word		*/
	_f_int		* restrict fptr2;	/* default word		*/
	_f_int		* restrict fptr3;	/* default word		*/
#ifdef _F_COMP16
	dblcmplx	* restrict dxptr1;	/* 256-bit		*/
	dblcmplx	* restrict dxptr2;	/* 256-bit		*/
	dblcmplx	* restrict dxptr3;	/* 256-bit		*/
#endif
	_f_int4		* restrict hptr1;	/* 32-bit		*/
	_f_int4		* restrict hptr2;	/* 32-bit		*/
	_f_int4		* restrict hptr3;	/* 32-bit		*/
	void		* restrict sptr;    /* ptr to src data area	*/
	void		* restrict rptr;    /* ptr to res data area	*/
	_f_int		* restrict shptr;   /* ptr to shift data area	*/
	_f_int		* restrict bptr;    /* ptr to boundary area	*/
	_f_int8		* restrict save_uptr1;	/* save copy of uptr1	*/
	_f_int8		* restrict save_uptr2a;	/* save copy of uptr2	*/
	_f_int8		* restrict save_uptr2b;	/* save copy of fptr2	*/
	_f_int		* restrict save_fptr1;	/* save copy of fptr1	*/
	_f_int		* restrict save_fptr2a;	/* save copy of fptr2	*/
	_f_int		* restrict save_fptr2b;	/* save copy of uptr2	*/
	char		* restrict save_cptr1;  /* save copy of cptr1	*/
	char		* restrict save_cptr2a; /* save copy of cptr2	*/
	char		* restrict save_cptr2b; /* save copy of cptr2	*/
	_f_comp8	* restrict save_xptr;	/* save copy of xptr2	*/
#ifdef _F_COMP16
	dblcmplx	* restrict save_dxptr;  /* save copy of dxptr2	*/
#endif
	_f_int4		* restrict save_hptr;	/* save copy of hptr2	*/
	_f_int4		* restrict save_hptr1;	/* save copy of hptr2	*/
	_f_int4		* restrict save_hptr2;	/* save copy of hptr2	*/
	_f_int4		* restrict i4ptr;   /* ptr to shift data area	*/
	_f_int8		* restrict i8ptr;   /* ptr to shift data area	*/
	_f_int	bucketsize;		/* size of each data element	*/
	_f_int	shft_size;		/* size of each strd element	*/
	_f_int	nbytes;			/* # of bytes in data area	*/
	_f_int	bytealligned;		/* byte alligned flag		*/
	long	sindx;			/* source index			*/
	long	sindx2;			/* source index			*/
	long	shindx;			/* shift index			*/
	long	rindx;			/* result array index		*/
	long	rindx2;			/* result array index		*/
	long	bindx;			/* boundary array index		*/
	long	shft;			/* shift value			*/
	_f_int	dim;			/* dim value			*/
	_f_int	non_dim;		/* non-shift dim for 2x2	*/
	long	curdim[MAXDIM-1	];	/* current indices		*/
	long	src_strd[MAXDIM-1];	/* index stride			*/
	long	src_ext[MAXDIM-1];	/* extents for source array	*/
	long	src_off[MAXDIM-1];	/* source offset		*/
	long	res_strd[MAXDIM-1];	/* index stride			*/
	long	res_off[MAXDIM-1];	/* result offset		*/
	long	shft_strd[MAXDIM-1];	/* stride for shift array	*/
	long	shft_off[MAXDIM-1];	/* shift offset			*/
	long	bnd_strd[MAXDIM-1];	/* stride for shift array	*/
	long	bnd_off[MAXDIM-1];	/* shift offset			*/
	_f_int	rank;			/* rank of source matrix	*/
	_f_int	type;			/* type				*/
	_f_int	subtype;		/* sub-type			*/
	_f_int	arithmetic;		/* arithmetic data type		*/
	long	extent;			/* extent temporary		*/
	long	src_dim_strd;		/* stride for source index	*/
	long	res_dim_strd;		/* stride for result index	*/
	long	src_use_strd;		/* stride for source index	*/
	long	res_use_strd;		/* stride for result index	*/
	long	shft_dim;		/* shift dimension		*/
	long	shft_dim_strd;		/* shift stride			*/
	_f_int	bnd_dim;		/* boundary dimension		*/
	long	bnd_dim_strd;		/* boundary stride		*/
	long	tot_ext;		/* total extent			*/
	long	shft_cnt;		/* shift count index		*/
	long	src_tmp;		/* temporary value for src	*/
	long	res_tmp;		/* temporary value for res	*/
	_f_int1		bnd8;		/* boundary value holder	*/
	_f_int2		bnd16;		/* boundary value holder	*/
	_f_int4		bnd32;		/* boundary value holder	*/
	_f_int8		bnd64;		/* boundary value holder	*/
	_f_comp8	bnd128;		/* boundary value holder	*/
#ifdef _F_COMP16
	dblcmplx 	bnd256;		/* boundary value holder	*/
#endif
	long	i, j, k, l;		/* index variables		*/

/*    Set type and dimension global variables	*/

	rank = source->n_dim;
	type = source->type_lens.type;

/*	Size calculation is based on variable type	*/

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
	shft_size = shift->type_lens.int_len / BITS_PER_WORD;
#ifdef	_CRAYMPP
	if (shft_size == 0)
	    shft_size = 1;
#endif

/*	If necessary, fill result dope vector	*/

	if (!result->assoc) {
	    result->base_addr.a.ptr = (void *) NULL;
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

/*	Determine size of space to allocate    */

	    if (!bytealligned) {
		nbytes = bucketsize * BYTES_PER_WORD;
#ifdef _CRAYMPP
		if (subtype == DVSUBTYPE_BIT32)
		    nbytes /= 2;
#endif
	    } else
		nbytes = bucketsize;
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
	    result->orig_base = (void *) result->base_addr.a.ptr;
	    result->orig_size = nbytes * BITS_PER_BYTE;

	    result->assoc = 1;
	    result->base_addr.a.el_len = source->base_addr.a.el_len;
	    if (type == DVTYPE_ASCII) {
		cr = (char *) result->base_addr.a.ptr;
		result->base_addr.charptr = _cptofcd (cr, bucketsize);
	    }
	}

/*
 *      Check to see if any of the matrices have size 0.  If any do,
 *      return without doing anything.
 */

#ifdef _UNICOS
#pragma _CRI     shortloop
#endif
	for (i = 0; i < rank; i++) {
	    if (!source->dimension[i].extent)
		return;
	}
	if (result->assoc) {
#ifdef _UNICOS
#pragma _CRI     shortloop
#endif
	    for (i = 0; i < rank; i++) {
		if (!result->dimension[i].extent)
		    return;
	    }
	}
	if (shift->n_dim > 1) {
#ifdef _UNICOS
#pragma _CRI     shortloop
#endif
	    for (i = 0; i < rank-1; i++) {
		if (!shift->dimension[i].extent)
		    return;
	    }
	}
	if (boundary) {
	    if (boundary->n_dim > 1) {
#ifdef _UNICOS
#pragma _CRI	shortloop
#endif
		for (i = 0; i < rank-1; i++) {
		    if (!boundary->dimension[i].extent)
			return;
		}
	    }
	}

/*	Set up scalar pointers to all of the argument data areas    */

	if (!bytealligned) {
	    sptr = (void *) source->base_addr.a.ptr;
	    rptr = (void *) result->base_addr.a.ptr;
	    if (boundary)
		bptr = (void *) boundary->base_addr.a.ptr;
	} else {
	    if (type == DVTYPE_ASCII) {
		cs = _fcdtocp (source->base_addr.charptr);
		cr = _fcdtocp (result->base_addr.charptr);
		if (boundary)
		    cb = _fcdtocp (boundary->base_addr.charptr);
	    } else {
		cs = (char *) source->base_addr.a.ptr;
		cr = (char *) result->base_addr.a.ptr;
		if (boundary)
		    cb = (char *) boundary->base_addr.a.ptr;
	    }
	}
	shptr = (void *) shift->base_addr.a.ptr;

/*	If dim argument is not present, set it to 1 (0 for C)	*/

	if (dimp == NULL)
	    dim = 0;
	else {
	    if (*dimp < 1 || *dimp > rank)
		_lerror (_LELVL_ABORT, FESCIDIM);
	    dim = *dimp - 1;
	}

/*	If source is a 1-dimensional array    */

	if (rank == 1) {

/*
 *	In order to assist vectorization, the following value has been
 *	taken out of the array variable, and put into a scalar.
 */

	    extent = source->dimension[0].extent;

/*
 *	Get shift value which will be used for each loop iteration.
 *	Once the value is obtained, make sure that it is positive.  This
 *	will ensure that the final calculated value can only be positive,
 *	thus eliminating a test from the inner loop.
 */

	    if (shift->type_lens.int_len == 64) {
		i8ptr = (_f_int8 *) shptr;
		shft = *i8ptr;
	    } else {
		i4ptr = (_f_int4 *) shptr;
		shft = *i4ptr;
	    }
	    if (shft > extent)
		shft = extent;
	    else if (shft < -extent)
		shft = -extent;

/*
 *	The index calculation for each element of the source and result
 *	matrices make use of some 'shortcuts'.  These involved the use
 *	of some variables which contain information about indices.  One
 *	of these variables exists for each dimension of the source and
 *	result matrices.
 *
 *    strd -	stride value based in terms of elements rather than
 *		words.
 */

	    if (bucketsize > 1 && arithmetic) {
		src_dim_strd = source->dimension[0].stride_mult / bucketsize;
		res_dim_strd = result->dimension[0].stride_mult / bucketsize;
	    } else {
		src_dim_strd = source->dimension[0].stride_mult;
		res_dim_strd = result->dimension[0].stride_mult;
	    }

/*
 *	Each data type will be handled with its own inner loop.
 */

	    switch (subtype) {
		case DVSUBTYPE_CHAR :
		    if (shft >= 0) {
			cptr1 = (char *) cs;
			cptr2 = (char *) cr + (extent - shft) * bucketsize;
			save_cptr2a = (char *) cr;
			src_use_strd = src_dim_strd;
			res_use_strd = res_dim_strd;
		    } else {
			shft = -shft;
			cptr1 = (char *) cs + ((extent - 1) * src_dim_strd);
			cptr2 = (char *) cr + ((shft - 1) * res_dim_strd);
			save_cptr2a = (char *) cr +
					((extent - 1) * res_dim_strd);
			src_use_strd = -src_dim_strd;
			res_use_strd = -res_dim_strd;
		    }
		    if (boundary)
			cptr3 = (char *) cb;
		    if (shft < extent)
			shft_cnt = shft;
		    else
			shft_cnt = extent;
		    for (i = 0; i < shft_cnt; i++) {
			if (boundary)
			    (void) memcpy (cptr2, cptr3, bucketsize);
			else
			    (void) memset (cptr2, ' ', bucketsize);
			cptr1 += src_use_strd;
			cptr2 += res_use_strd;
		    }
		    cptr2 = save_cptr2a;
		    for ( ; i < extent; i++) {
			(void) memcpy (cptr2, cptr1, bucketsize);
			cptr1 += src_use_strd;
			cptr2 += res_use_strd;
		    }

		    break;

		case DVSUBTYPE_DERIVED :
		    if (shft >= 0) {
			save_fptr1 = (_f_int *) sptr;
			save_fptr2a = (_f_int *) rptr +
					(extent-shft) * bucketsize;
			save_fptr2b = (_f_int *) rptr;
			src_use_strd = src_dim_strd;
			res_use_strd = res_dim_strd;
		    } else {
			shft = -shft;
			save_fptr1 = (_f_int *) sptr +
					((extent - 1) * src_dim_strd);
			save_fptr2a = (_f_int *) rptr +
					((shft - 1) * res_dim_strd);
			save_fptr2b = (_f_int *) rptr +
					((extent - 1) * res_dim_strd);
			src_use_strd = -src_dim_strd;
			res_use_strd = -res_dim_strd;
		    }
		    if (boundary)
			fptr3 = (_f_int *) bptr;
		    if (shft < extent)
			shft_cnt = shft;
		    else
			shft_cnt = extent;
		    for (i = 0; i < bucketsize; i++) {
			fptr1 = save_fptr1;
			fptr2 = save_fptr2a;
			for (j = 0; j < shft_cnt; j++) {
			    rindx = (j * res_use_strd) + i;
			    fptr2[rindx] = fptr3[i];
			}
			fptr2 = save_fptr2b;
			for (k = 0; j < extent; j++, k++) {
			    sindx = (j * src_use_strd) + i;
			    rindx = (k * res_use_strd) + i;
			    fptr2[rindx] = fptr1[sindx];
			}
		    }
		    break;

		case DVSUBTYPE_BIT64 :
		    if (shft >= 0) {
			uptr1 = (_f_int8 *) sptr;
			save_uptr1 = uptr1;
			uptr2 = (_f_int8 *) rptr +
				((extent - shft) * res_dim_strd);
			save_uptr2b = (_f_int8 *) rptr;
			src_use_strd = src_dim_strd;
			res_use_strd = res_dim_strd;
		    } else {
			shft = -shft;
			uptr1 = (_f_int8 *) sptr +
				((extent - 1) * src_dim_strd);
			save_uptr1 = uptr1;
			uptr2 = (_f_int8 *) rptr +
				((shft - 1) * res_dim_strd);
			save_uptr2b = (_f_int8 *) rptr +
					((extent - 1) * res_dim_strd);
			src_use_strd = -src_dim_strd;
			res_use_strd = -res_dim_strd;
		    }
		    if (shft < extent)
			shft_cnt = shft;
		    else
			shft_cnt = extent;

		    if (boundary) {
			uptr3 = (_f_int8 *) bptr;
			bnd64 = uptr3[0];
		    } else {
			BIT64_DEFAULT();
		    }

		    for (i = 0; i < shft_cnt; i++) {
			rindx = i * res_use_strd;
			uptr2[rindx] = bnd64;
		    }
		    uptr2 = save_uptr2b;
#ifndef CRAY2
		    for (j = 0; i < extent; i++, j++) {
			sindx = i * src_use_strd;
			rindx = j * res_use_strd;
			uptr2[rindx] = uptr1[sindx];
		    }
#else
		    uptr1 = save_uptr1 + (shft_cnt * res_use_strd);
		    shft_cnt = extent - shft_cnt;
		    memstride ( uptr2, res_use_strd,
				uptr1, src_use_strd, shft_cnt);
#endif
		    break;

		case DVSUBTYPE_BIT32 :
		    if (shft >= 0) {
			hptr1 = (_f_int4 *) sptr;
			hptr2 = (_f_int4 *) rptr +
				((extent - shft) * res_dim_strd);
			save_hptr = (_f_int4 *) rptr;
			src_use_strd = src_dim_strd;
			res_use_strd = res_dim_strd;
		    } else {
			shft = -shft;
			hptr1 = (_f_int4 *) sptr +
				((extent - 1) * src_dim_strd);
			hptr2 = (_f_int4 *) rptr +
				((shft - 1) * res_dim_strd);
			save_hptr = (_f_int4 *) rptr +
					((extent - 1) * res_dim_strd);
			src_use_strd = -src_dim_strd;
			res_use_strd = -res_dim_strd;
		    }
		    if (shft < extent)
			shft_cnt = shft;
		    else
			shft_cnt = extent;

		    if (boundary) {
			hptr3 = (_f_int4 *) bptr;
			bnd32 = hptr3[0];
		    } else {
			BIT32_DEFAULT();
		    }

		    for (i = 0; i < shft_cnt; i++) {
			rindx = i * res_use_strd;
			hptr2[rindx] = bnd32;
		    }
		    hptr2 = save_hptr;
		    for (j = 0; i < extent ; i++, j++) {
			sindx = i * src_use_strd;
			rindx = j * res_use_strd;
			hptr2[rindx] = hptr1[sindx];
		    }
		    break;

		case DVSUBTYPE_BIT128 :
		    if (shft >= 0) {
			xptr1 = (_f_comp8 *) sptr;
			xptr2 = (_f_comp8 *) rptr +
				((extent - shft) * res_dim_strd);
			save_xptr = (_f_comp8 *) rptr;
			src_use_strd = src_dim_strd;
			res_use_strd = res_dim_strd;
		    } else {
			shft = -shft;
			xptr1 = (_f_comp8 *) sptr +
				((extent - 1) * src_dim_strd);
			xptr2 = (_f_comp8 *) rptr +
				((shft - 1) * res_dim_strd);
			save_xptr = (_f_comp8 *) rptr +
					((extent - 1) * res_dim_strd);
			src_use_strd = -src_dim_strd;
			res_use_strd = -res_dim_strd;
		    }
		    if (shft < extent)
			shft_cnt = shft;
		    else
			shft_cnt = extent;

		    if (boundary) {
			xptr3 = (_f_comp8 *) bptr;
			bnd128 = xptr3[0];
		    } else {
			BIT128_DEFAULT();
		    }

		    for (i = 0; i < shft_cnt; i++) {
			rindx = i * res_use_strd;
			xptr2[rindx] = bnd128;
		    }
		    xptr2 = save_xptr;
		    for (j = 0; i < extent ; i++, j++) {
			sindx = i * src_use_strd;
			rindx = j * res_use_strd;
			xptr2[rindx] = xptr1[sindx];
		    }
		    break;

#ifdef _F_COMP16
		case DVSUBTYPE_BIT256 :
		    if (shft >= 0) {
			dxptr1 = (dblcmplx *) sptr;
			dxptr2 = (dblcmplx *) rptr +
				((extent - shft) * res_dim_strd);
			save_dxptr = (dblcmplx *) rptr;
			src_use_strd = src_dim_strd;
			res_use_strd = res_dim_strd;
		    } else {
			shft = -shft;
			dxptr1 = (dblcmplx *) sptr +
				((extent - 1) * src_dim_strd);
			dxptr2 = (dblcmplx *) rptr +
				((shft - 1) * res_dim_strd);
			save_dxptr = (dblcmplx *) rptr +
					((extent - 1) * res_dim_strd);
			src_use_strd = -src_dim_strd;
			res_use_strd = -res_dim_strd;
		    }
		    if (shft < extent)
			shft_cnt = shft;
		    else
			shft_cnt = extent;

		    if (boundary) {
			dxptr3 = (dblcmplx *) bptr;
			bnd256.re = dxptr3[0].re;
			bnd256.im = dxptr3[0].im;
		    } else {
			BIT256_DEFAULT();
		    }
		    for (i = 0; i < shft_cnt; i++) {
			rindx = i * res_use_strd;
			dxptr2[rindx].re = bnd256.re;
			dxptr2[rindx].im = bnd256.im;
		    }
		    dxptr2 = save_dxptr;
		    for (j = 0; i < extent ; i++, j++) {
			sindx = i * src_use_strd;
			rindx = j * res_use_strd;
			dxptr2[rindx].re = dxptr1[sindx].re;
			dxptr2[rindx].im = dxptr1[sindx].im;
		    }
		    break;
#endif

		default:
		    _lerror(_LELVL_ABORT, FEINTDTY);
	    }

/*	Arrays with rank of 2	*/

	} else if (rank == 2) {

/*	Set dimension and non_dimension indices	*/

	    if (dim == 0)
		non_dim = 1;
	    else
		non_dim = 0;

/*	Set up shortcut variables for both array dimensions    */

	    if (bucketsize > 1 && arithmetic) {
		src_strd[0] = source->dimension[0].stride_mult / bucketsize;
		src_strd[1] = source->dimension[1].stride_mult / bucketsize;
		res_strd[0] = result->dimension[0].stride_mult / bucketsize;
		res_strd[1] = result->dimension[1].stride_mult / bucketsize;
		if (boundary)
		    bnd_strd[0] = boundary->dimension[0].stride_mult/bucketsize;
	    } else {
		src_strd[0] = source->dimension[0].stride_mult;
		src_strd[1] = source->dimension[1].stride_mult;
		res_strd[0] = result->dimension[0].stride_mult;
		res_strd[1] = result->dimension[1].stride_mult;
		if (boundary)
		    bnd_strd[0] = boundary->dimension[0].stride_mult;
	    }
	    shft_strd[0] = shift->dimension[0].stride_mult / shft_size;

/*	Put information about DIM index in scalars for vectorization */

	    src_dim_strd = src_strd[dim];
	    res_dim_strd = res_strd[dim];
	    if (boundary)
		bnd_dim_strd = bnd_strd[0];

/*	Set up extent temporary variables    */

	    extent = source->dimension[dim].extent;

/*
 *	The following expressions are used repeatedly in this section, and
 *	can be put into scalar variables so that they do not have to be
 *	recalculated every time they are used.
 */

	    src_tmp = (extent - 1) * src_strd[dim];
	    res_tmp = (extent - 1) * res_strd[dim];

/*	Set up shift variables		*/

	    if (shift->n_dim == 1) {
		shft_dim = 1;
		shft_dim_strd = shift->dimension[0].stride_mult / shft_size;
		shindx = 0;
	    } else {			/* scalar value, only get it once */
		shft_dim = 0;
	    }
	    if (shift->type_lens.int_len == 64) {
		i8ptr = (_f_int8 *) shptr;
		shft = i8ptr[0];
	    } else {
		i4ptr = (_f_int4 *) shptr;
		shft = i4ptr[0];
	    }

/*	Set up boundary index	*/

	    if (boundary) {
		bindx = 0;
		if (boundary->n_dim > 0)
		    bnd_dim = 1;
		else
		    bnd_dim = 0;
	    }

/*	Outer loop is for dimension not being shifted    */

	    for (i = 0; i < source->dimension[non_dim].extent; i++) {

/*	Get shift value if it is not a scalar	*/

		if (shift->type_lens.int_len == 64) {
		    if (shft_dim == 1) {
			shft = i8ptr[shindx];
			shindx += shft_dim_strd;
		    } else
			shft = i8ptr[0];
		} else {
		    if (shft_dim == 1) {
			shft = i4ptr[shindx];
			shindx += shft_dim_strd;
		    } else
			shft = i4ptr[0];
		}
		if (shft > extent)
		    shft = extent;
		else if (shft < -extent)
		    shft = -extent;

		switch (subtype) {
		    case DVSUBTYPE_CHAR :
			if (shft >= 0) {
			    cptr1 = (char *) cs + (i * src_strd[non_dim]);
			    cptr2 = (char *) cr + (i * res_strd[non_dim]) +
				    ((extent - shft) * res_strd[dim]);
			    save_cptr2a = (char *) cr + (i * res_strd[non_dim]);
			    src_use_strd = src_dim_strd;
			    res_use_strd = res_dim_strd;
			} else {
			    shft = -shft;
			    cptr1 = (char *) cs + (i * src_strd[non_dim]) +
					src_tmp;
			    cptr2 = ( char *) cr +
					(i * res_strd[non_dim]) +
					((shft - 1) * res_strd[dim]);
			    save_cptr2a = (char *) cr + (i * res_strd[non_dim])
					  + res_tmp;
			    src_use_strd = -src_dim_strd;
			    res_use_strd = -res_dim_strd;
			}
			if (boundary)
			    cptr3 = (char *) cb + bindx;

			if (shft < extent)
			    shft_cnt = shft;
			else
			    shft_cnt = extent;
			for (j = 0; j < shft_cnt; j++) {
			    if (boundary)
				(void) memcpy (cptr2, cptr3, bucketsize);
			    else
				(void) memset (cptr2, ' ', bucketsize);
			    cptr1 += src_use_strd;
			    cptr2 += res_use_strd;
			}
			cptr2 = save_cptr2a;
			for ( ; j < extent; j++) {
			    (void) memcpy (cptr2, cptr1, bucketsize);
			    cptr1 += src_use_strd;
			    cptr2 += res_use_strd;
			}
			break;

		    case DVSUBTYPE_DERIVED :
			if (shft >= 0) {
			    save_fptr1 = (_f_int *) sptr +
					  (i * src_strd[non_dim]);
			    save_fptr2a = (_f_int *) rptr +
					  (i * res_strd[non_dim]) +
					  ((extent - shft) * res_dim_strd);
			    save_fptr2b = (_f_int *) rptr +
					  (i * res_strd[non_dim]);
			    src_use_strd = src_dim_strd;
			    res_use_strd = res_dim_strd;
			} else {
			    shft = -shft;
			    save_fptr1 = (_f_int *) sptr +
					  (i * src_strd[non_dim]) + src_tmp;
			    save_fptr2a = (_f_int *) rptr +
					  (i * res_strd[non_dim]) +
					  ((shft - 1) * res_strd[dim]);
			    save_fptr2b = (_f_int *) rptr +
					  (i * src_strd[non_dim]) + res_tmp;
			    src_use_strd = -src_dim_strd;
			    res_use_strd = -res_dim_strd;
			}
			fptr3 = (_f_int *) bptr + bindx;

			if (shft < extent)
			    shft_cnt = shft;
			else
			    shft_cnt = extent;

			for (j = 0; j < bucketsize; j++) {
			    fptr1 = save_fptr1;
			    fptr2 = save_fptr2a;
			    for (k = 0; k < shft_cnt; k++) {
				rindx = (k * res_use_strd) + j;
				fptr2[rindx] = fptr3[j];
			    }
			    fptr2 = save_fptr2b;
			    for (l = 0; k < extent; k++, l++) {
				sindx = (k * src_use_strd) + j;
				rindx = (l * res_use_strd) + j;
				fptr2[rindx] = fptr1[sindx];
			    }
			}
			break;

		    case DVSUBTYPE_BIT64 :
			if (shft >= 0) {
			    uptr1 = (_f_int8 *) sptr +
					(i * src_strd[non_dim]);
			    save_uptr1 = uptr1;
			    uptr2 = (_f_int8 *) rptr +
					(i * res_strd[non_dim]) +
					((extent - shft) * res_dim_strd);
			    save_uptr2b = (_f_int8 *) rptr +
					(i * res_strd[non_dim]);
			    src_use_strd = src_dim_strd;
			    res_use_strd = res_dim_strd;
			} else {
			    shft = -shft;
			    uptr1 = (_f_int8 *) sptr +
					(i * src_strd[non_dim]) + src_tmp;
			    save_uptr1 = uptr1;
			    uptr2 = (_f_int8 *) rptr +
					(i * res_strd[non_dim]) +
					((shft - 1) * res_strd[dim]);
			    save_uptr2b = (_f_int8 *) rptr +
					  (i * res_strd[non_dim]) + res_tmp;
			    src_use_strd = -src_dim_strd;
			    res_use_strd = -res_dim_strd;
			}

			if (shft < extent)
			    shft_cnt = shft;
			else
			    shft_cnt = extent;

			if (boundary) {
			    uptr3 = (_f_int8 *) bptr + bindx;
			    bnd64 = uptr3[0];
			} else {
			    BIT64_DEFAULT();
			}

			for (j = 0; j < shft_cnt; j++) {
			    rindx = (j * res_use_strd);
			    uptr2[rindx] = bnd64;
			}
			uptr2 = save_uptr2b;
#ifndef CRAY2
			for (k = 0; j < extent; j++, k++) {
			    sindx = (j * src_use_strd);
			    rindx = (k * res_use_strd);
			    uptr2[rindx] = uptr1[sindx];
			}
#else
		    uptr1 = save_uptr1 + (shft_cnt * src_use_strd);
		    shft_cnt = extent - shft_cnt;
		    memstride ( uptr2, res_use_strd,
				uptr1, src_use_strd, shft_cnt);
#endif
			break;

		    case DVSUBTYPE_BIT32 :
			if (shft >= 0) {
			    hptr1 = (_f_int4 *) sptr +
					(i * src_strd[non_dim]);
			    hptr2 = (_f_int4 *) rptr +
					(i * res_strd[non_dim]) +
					((extent - shft) * res_dim_strd);
			    save_hptr = (_f_int4 *) rptr +
					(i * res_strd[non_dim]);
			    src_use_strd = src_dim_strd;
			    res_use_strd = res_dim_strd;
			} else {
			    shft = -shft;
			    hptr1 = (_f_int4 *) sptr +
					(i * src_strd[non_dim]) + src_tmp;
			    hptr2 = (_f_int4 *) rptr +
					(i * res_strd[non_dim]) +
					((shft - 1) * res_strd[dim]);
			    save_hptr = (_f_int4 *) rptr +
					(i * res_strd[non_dim]) + res_tmp;
			    src_use_strd = -src_dim_strd;
			    res_use_strd = -res_dim_strd;
			}

			if (shft < extent)
			    shft_cnt = shft;
			else
			    shft_cnt = extent;

			if (boundary) {
			    hptr3 = (_f_int4 *) bptr + bindx;
			    bnd32 = hptr3[0];
			} else {
			    BIT32_DEFAULT();
			}

			for (j = 0; j < shft_cnt; j++) {
			    rindx = j * res_use_strd;
			    hptr2[rindx] = bnd32;
			}
			hptr2 = save_hptr;
			for (k = 0; j < extent; j++, k++) {
			    sindx = j * src_use_strd;
			    rindx = k * res_use_strd;
			    hptr2[rindx] = hptr1[sindx];
			}
			break;

		    case DVSUBTYPE_BIT128 :
			if (shft >= 0) {
			    xptr1 = (_f_comp8 *) sptr +
					(i * src_strd[non_dim]);
			    xptr2 = (_f_comp8 *) rptr +
					(i * res_strd[non_dim]) +
					((extent - shft) * res_dim_strd);
			    save_xptr = (_f_comp8 *) rptr +
					(i * res_strd[non_dim]);
			    src_use_strd = src_dim_strd;
			    res_use_strd = res_dim_strd;
			} else {
			    shft = -shft;
			    xptr1 = (_f_comp8 *) sptr +
					(i * src_strd[non_dim]) + src_tmp;
			    xptr2 = (_f_comp8 *) rptr +
					(i * res_strd[non_dim]) +
					((shft - 1) * res_strd[dim]);
			    save_xptr = (_f_comp8 *) rptr +
					(i * res_strd[non_dim]) + res_tmp;
			    src_use_strd = -src_dim_strd;
			    res_use_strd = -res_dim_strd;
			}

			if (shft < extent)
			    shft_cnt = shft;
			else
			    shft_cnt = extent;

			if (boundary) {
			    xptr3 = (_f_comp8 *) bptr + bindx;
			    bnd128 = xptr3[0];
			} else {
			    BIT128_DEFAULT();
			}

			for (j = 0; j < shft_cnt; j++) {
			    rindx = j * res_use_strd;
			    xptr2[rindx] = bnd128;
			}
			xptr2 = save_xptr;
			for (k = 0; j < extent; j++, k++) {
			    sindx = j * src_use_strd;
			    rindx = k * res_use_strd;
			    xptr2[rindx] = xptr1[sindx];
			}
			break;

#ifdef _F_COMP16
		    case DVSUBTYPE_BIT256 :
			if (shft >= 0) {
			    dxptr1 = (dblcmplx *) sptr +
					(i * src_strd[non_dim]);
			    dxptr2 = (dblcmplx *) rptr +
					(i * res_strd[non_dim]) +
					((extent - shft) * res_dim_strd);
			    save_dxptr = (dblcmplx *) rptr +
					(i * res_strd[non_dim]);
			    src_use_strd = src_dim_strd;
			    res_use_strd = res_dim_strd;
			} else {
			    shft = -shft;
			    dxptr1 = (dblcmplx *) sptr +
					(i * src_strd[non_dim]) + src_tmp;
			    dxptr2 = (dblcmplx *) rptr +
					(i * res_strd[non_dim]) +
					((shft - 1) * res_strd[dim]);
			    save_dxptr = (dblcmplx *) rptr +
					(i * res_strd[non_dim]) + res_tmp;
			    src_use_strd = -src_dim_strd;
			    res_use_strd = -res_dim_strd;
			}

			if (shft < extent)
			    shft_cnt = shft;
			else
			    shft_cnt = extent;

			if (boundary) {
			    dxptr3 = (dblcmplx *) bptr + bindx;
			    bnd256.re = dxptr3[0].re;
			    bnd256.im = dxptr3[0].im;
			} else {
			    BIT256_DEFAULT();
			}

			for (j = 0; j < shft_cnt; j++) {
			    rindx = j * res_use_strd;
			    dxptr2[rindx].re = bnd256.re;
			    dxptr2[rindx].im = bnd256.im;
			}
			dxptr2 = save_dxptr;
			for (k = 0; j < extent; j++, k++) {
			    sindx = j * src_use_strd;
			    rindx = k * res_use_strd;
			    dxptr2[rindx].re = dxptr1[sindx].re;
			    dxptr2[rindx].im = dxptr1[sindx].im;
			}
			break;
#endif

		    default :
			_lerror(_LELVL_ABORT, FEINTDTY);
		}

/*	Increment boundary stride index		*/

		if (bnd_dim)
		    bindx += bnd_dim_strd;
	    }

/*	Arrays with rank of 3-7	*/

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
		    if (boundary)
			bnd_strd[i] =
			    boundary->dimension[i].stride_mult / bucketsize;
		    else
			bnd_strd[i] = 0;
		} else {
		    src_strd[i] = source->dimension[i].stride_mult;
		    res_strd[i] = result->dimension[i].stride_mult;
		    if (boundary)
			bnd_strd[i] = boundary->dimension[i].stride_mult;
		    else
			bnd_strd[i] = 0;
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
		    if (boundary)
			bnd_strd[i] =
			    boundary->dimension[i].stride_mult / bucketsize;
		    else
			bnd_strd[i] = 0;
		} else {
		    src_strd[i] = source->dimension[i+1].stride_mult;
		    res_strd[i] = result->dimension[i+1].stride_mult;
		    if (boundary)
			bnd_strd[i] = boundary->dimension[i].stride_mult;
		    else
			bnd_strd[i] = 0;
		}
	      }

/*	Initialize all counters to 0	*/

#ifdef _UNICOS
#pragma _CRI	shortloop
#endif
	    for (i = 0; i < MAXDIM-1; i++) {
		src_off[i] = 0;
		res_off[i] = 0;
		bnd_off[i] = 0;
		shft_off[i] = 0;
		shft_strd[i] = 0;
		curdim[i] = 0;
	    }

/*	Set up some scalars which contain information about DIM    */

	    extent = source->dimension[dim].extent;

	    if (bucketsize > 1 && arithmetic) {
		src_dim_strd = source->dimension[dim].stride_mult / bucketsize;
		res_dim_strd = result->dimension[dim].stride_mult / bucketsize;
	    } else {
		src_dim_strd = source->dimension[dim].stride_mult;
		res_dim_strd = result->dimension[dim].stride_mult;
	    }

/*
 *	The following expressions are used throughout this loop.  They can
 *	be put into scalars to avoid recalculating them every time they are
 *	used.
 */

	    src_tmp = (extent - 1) * src_dim_strd;
	    res_tmp = (extent - 1) * res_dim_strd;

/*	Set up the shift variables		*/

	    if (shift->n_dim == 0) {
		shft_dim = 0;
	    } else {
		shft_dim = 1;
		shindx = 0;
#ifdef _UNICOS
#pragma	_CRI	shortloop
#endif
		for (i = 0; i < rank-1; i++) {
		    shft_strd[i] = shift->dimension[i].stride_mult / shft_size;
		}
	    }

/*	Initialize correct pointer type to shift data	*/

	    if (shift->type_lens.int_len == 64) {
		i8ptr = (_f_int8 *) shptr;
		shft = i8ptr[0];
	    } else {
		i4ptr = (_f_int4 *) shptr;
		shft = i4ptr[0];
	    }

/*	Set up boundary values	*/

	    if (boundary) {
		bindx = 0;
		if (boundary->n_dim > 0)
		    bnd_dim = 1;
		else
		    bnd_dim = 0;
	    }

/*
 *	The outer loop will be executed once for each combination of
 *	indices not including the DIM dimension.
 */

	    for (i = 0; i < tot_ext; i++) {

/*	Calculate the shift value used throughout the inner loop    */

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
		    if (shift->type_lens.int_len == 64) 
			shft = i8ptr[shindx];
		    else
			shft = i4ptr[shindx];
		} else
		    if (shift->type_lens.int_len == 64) 
			shft = i8ptr[0];
		    else
			shft = i4ptr[0];
		if (shft > extent)
		    shft = extent;
		else if (shft < -extent)
		    shft = -extent;

/*	If necessary, get boundary index	*/

		if (bnd_dim) {
		    switch (rank) {
			case 3 :
			    bindx = bnd_off[0] + bnd_off[1];
			    break;
			case 4 :
			    bindx = bnd_off[0] + bnd_off[1] + bnd_off[2];
			    break;
			case 5 :
			    bindx = bnd_off[0] + bnd_off[1] +
				     bnd_off[2] + bnd_off[3];
			    break;
			case 6 :
			    bindx = bnd_off[0] + bnd_off[1] + bnd_off[2] +
				     bnd_off[3] + bnd_off[4];
			    break;
			default :
			    bindx = bnd_off[0] + bnd_off[1] + bnd_off[2] +
				     bnd_off[3] + bnd_off[4] + bnd_off[5];
		    }
		}

/*	Calculate source and result index values	*/

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
			if (shft >= 0) {
			    cptr1 = (char *) cs + sindx;
			    save_cptr1 = cptr1;
			    cptr2 = (char *) cr + rindx +
				    ((extent - shft) * res_dim_strd);
			    save_cptr2a = cptr2;
			    save_cptr2b = (char *) cr + rindx;
			    src_use_strd = src_dim_strd;
			    res_use_strd = res_dim_strd;
			} else {
			    shft = -shft;
			    cptr1 = (char *) cs + sindx + src_tmp;
			    save_cptr1 = cptr1;
			    cptr2 = (char *) cr + rindx +
					((shft - 1) * res_dim_strd);
			    save_cptr2a = cptr2;
			    save_cptr2b = (char *) cr + rindx + res_tmp;
			    src_use_strd = -src_dim_strd;
			    res_use_strd = -res_dim_strd;
			}

			if (boundary)
			    cptr3 = (char *) cb + bindx;

			if (shft < extent)
			    shft_cnt = shft;
			else
			    shft_cnt = extent;
			for (j = 0; j < shft_cnt; j++) {
			    cptr2 = save_cptr2a + (j * res_use_strd);
			    if (boundary)
				(void) memcpy (cptr2, cptr3, bucketsize);
			    else
				(void) memset (cptr2, ' ', bucketsize);
			}
			cptr2 = save_cptr2b;
			for (k = 0; j < extent; j++, k++) {
			    cptr1 = save_cptr1 + (j * src_use_strd);
			    cptr2 = save_cptr2b + (k * res_use_strd);
			    (void) memcpy (cptr2, cptr1, bucketsize);
			}
			break;

		    case DVSUBTYPE_DERIVED :
			if (shft >= 0) {
			    save_fptr1 = (_f_int *) sptr + sindx;
			    save_fptr2a = (_f_int *) rptr + rindx +
				((extent - shft) * res_dim_strd);
			    save_fptr2b = (_f_int *) rptr + rindx;
			    src_use_strd = src_dim_strd;
			    res_use_strd = res_dim_strd;
			} else {
			    shft = -shft;
			    save_fptr1 = (_f_int *) sptr + sindx +
						src_tmp;
			    save_fptr2a = (_f_int *) rptr + rindx +
					  ((shft - 1) * res_dim_strd);
			    save_fptr2b = (_f_int *) rptr + rindx +
						res_tmp;
			    src_use_strd = -src_dim_strd;
			    res_use_strd = -res_dim_strd;
			}
			fptr3 = (_f_int *) bptr + bindx;

			if (shft < extent)
			    shft_cnt = shft;
			else
			    shft_cnt = extent;

			for (j = 0; j < bucketsize; j++) {
			    fptr1 = save_fptr1;
			    fptr2 = save_fptr2a;
			    for (k = 0; k < shft_cnt; k++) {
				rindx2 = (k * res_use_strd) + j;
				fptr2[rindx2] = fptr3[j];
			    }
			    fptr2 = save_fptr2b;
			    for (l = 0; k < extent; k++, l++) {
				sindx2 = (k * src_use_strd) + j;
				rindx2 = (l * res_use_strd) + j;
				fptr2[rindx2] = fptr1[sindx2];
			    }
			}
			break;

		    case DVSUBTYPE_BIT64 :
			if (shft >= 0) {
			    uptr1 = (_f_int8 *) sptr + sindx;
			    save_uptr1 = uptr1;
			    uptr2 = (_f_int8 *) rptr + rindx +
					((extent - shft) * res_dim_strd);
			    save_uptr2b = (_f_int8 *) rptr + rindx;
			    src_use_strd = src_dim_strd;
			    res_use_strd = res_dim_strd;
			} else {
			    shft = -shft;
			    uptr1 = (_f_int8 *) sptr + sindx + src_tmp;
			    save_uptr1 = uptr1;
			    uptr2 = (_f_int8 *) rptr + rindx +
					((shft - 1) * res_dim_strd);
			    save_uptr2b = (_f_int8 *) rptr + rindx +
						res_tmp;
			    src_use_strd = -src_dim_strd;
			    res_use_strd = -res_dim_strd;
			}

			if (shft < extent)
			    shft_cnt = shft;
			else
			    shft_cnt = extent;

			if (boundary) {
			    uptr3 = (_f_int8 *) bptr + bindx;
			    bnd64 = uptr3[0];
			} else {
			    BIT64_DEFAULT();
			}

			for (j = 0; j < shft_cnt; j++) {
			    rindx2 = j * res_use_strd;
			    uptr2[rindx2] = bnd64;
			}
			uptr2 = save_uptr2b;
#ifndef CRAY2
			for (k = 0; j < extent; j++, k++) {
			    sindx2 = j * src_use_strd;
			    rindx2 = k * res_use_strd;
			    uptr2[rindx2] = uptr1[sindx2];
			}
#else
			uptr1 = save_uptr1 + (shft_cnt * src_use_strd);
			shft_cnt = extent - shft_cnt;
			memstride ( uptr2, res_use_strd,
				uptr1, src_use_strd, shft_cnt);
#endif
			break;

		    case DVSUBTYPE_BIT32 :
			if (shft >= 0) {
			    hptr1 = (_f_int4 *) sptr + sindx;
			    hptr2 = (_f_int4 *) rptr + rindx +
					((extent - shft) * res_dim_strd);
			    save_hptr = (_f_int4 *) rptr + rindx;
			    src_use_strd = src_dim_strd;
			    res_use_strd = res_dim_strd;
			} else {
			    shft = -shft;
			    hptr1 = (_f_int4 *) sptr + sindx + src_tmp;
			    hptr2 = (_f_int4 *) rptr + rindx +
					((shft - 1) * res_dim_strd);
			    save_hptr = (_f_int4 *) rptr + rindx + res_tmp;
			    src_use_strd = -src_dim_strd;
			    res_use_strd = -res_dim_strd;
			}

			if (shft < extent)
			    shft_cnt = shft;
			else
			    shft_cnt = extent;

			if (boundary) {
			    hptr3 = (_f_int4 *) bptr + bindx;
			    bnd32 = hptr3[0];
			} else {
			    BIT32_DEFAULT();
			}

			for (j = 0; j < shft_cnt; j++) {
			    rindx2 = j * res_use_strd;
			    hptr2[rindx2] = bnd32;
			}
			hptr2 = save_hptr;
			for (k = 0; j < extent; j++, k++) {
			    sindx2 = j * src_use_strd;
			    rindx2 = k * res_use_strd;
			    hptr2[rindx2] = hptr1[sindx2];
			}
			break;

		    case DVSUBTYPE_BIT128 :
			if (shft >= 0) {
			    xptr1 = (_f_comp8 *) sptr + sindx;
			    xptr2 = (_f_comp8 *) rptr + rindx +
					((extent - shft) * res_dim_strd);
			    save_xptr = (_f_comp8 *) rptr + rindx;
			    src_use_strd = src_dim_strd;
			    res_use_strd = res_dim_strd;
			} else {
			    shft = -shft;
			    xptr1 = (_f_comp8 *) sptr + sindx + src_tmp;
			    xptr2 = (_f_comp8 *) rptr + rindx +
					((shft - 1) * res_dim_strd);
			    save_xptr = (_f_comp8 *) rptr + rindx + res_tmp;
			    src_use_strd = -src_dim_strd;
			    res_use_strd = -res_dim_strd;
			}

			if (shft < extent)
			    shft_cnt = shft;
			else
			    shft_cnt = extent;

			if (boundary) {
			    xptr3 = (_f_comp8 *) bptr + bindx;
			    bnd128 = xptr3[0];
			} else {
			    BIT128_DEFAULT();
			}

			for (j = 0; j < shft_cnt; j++) {
			    rindx2 = j * res_use_strd;
			    xptr2[rindx2] = bnd128;
			}
			xptr2 = save_xptr;
			for (k = 0; j < extent; j++, k++) {
			    sindx2 = j * src_use_strd;
			    rindx2 = k * res_use_strd;
			    xptr2[rindx2] = xptr1[sindx2];
			}
			break;

#ifdef _F_COMP16
		    case DVSUBTYPE_BIT256 :
			if (shft >= 0) {
			    dxptr1 = (dblcmplx *) sptr + sindx;
			    dxptr2 = (dblcmplx *) rptr + rindx +
					((extent - shft) * res_dim_strd);
			    save_dxptr = (dblcmplx *) rptr + rindx;
			    src_use_strd = src_dim_strd;
			    res_use_strd = res_dim_strd;
			} else {
			    shft = -shft;
			    dxptr1 = (dblcmplx *) sptr + sindx + src_tmp;
			    dxptr2 = (dblcmplx *) rptr + rindx +
					((shft - 1) * res_dim_strd);
			    save_dxptr = (dblcmplx *) rptr + rindx + res_tmp;
			    src_use_strd = -src_dim_strd;
			    res_use_strd = -res_dim_strd;
			}

			if (shft < extent)
			    shft_cnt = shft;
			else
			    shft_cnt = extent;

			if (boundary) {
			    dxptr3 = (dblcmplx *) bptr + bindx;
			    bnd256.re = dxptr3[0].re;
			    bnd256.im = dxptr3[0].im;
			} else {
			    BIT256_DEFAULT();
			}

			for (j = 0; j < shft_cnt; j++) {
			    rindx2 = j * res_use_strd;
			    dxptr2[rindx2].re = bnd256.re;
			    dxptr2[rindx2].im = bnd256.im;
			}
			dxptr2 = save_dxptr;
			for (k = 0; j < extent; j++, k++) {
			    sindx2 = j * src_use_strd;
			    rindx2 = k * res_use_strd;
			    dxptr2[rindx2].re = dxptr1[sindx2].re;
			    dxptr2[rindx2].im = dxptr1[sindx2].im;
			}
			break;
#endif

		    default :
			_lerror(_LELVL_ABORT, FEINTDTY);
		}

/*	Increment the current dimension counter.    */

		curdim[0]++;
		if (curdim[0] < src_ext[0]) {
		    src_off[0] += src_strd[0];
		    res_off[0] += res_strd[0];
		    shft_off[0] += shft_strd[0];
		    bnd_off[0] += bnd_strd[0];
		} else {
		    curdim[0] = 0;
		    src_off[0] = 0;
		    res_off[0] = 0;
		    shft_off[0] = 0;
		    bnd_off[0] = 0;
		    curdim[1]++;
		    if (curdim[1] < src_ext[1]) {
			src_off[1] += src_strd[1];
			res_off[1] += res_strd[1];
			shft_off[1] += shft_strd[1];
			bnd_off[1] += bnd_strd[1];
		    } else {
			curdim[1] = 0;
			src_off[1] = 0;
			res_off[1] = 0;
			shft_off[1] = 0;
			bnd_off[1] = 0;
			curdim[2]++;
			if (curdim[2] < src_ext[2]) {
			    src_off[2] += src_strd[2];
			    res_off[2] += res_strd[2];
			    shft_off[2] += shft_strd[2];
			    bnd_off[2] += bnd_strd[2];
			} else {
			    curdim[2] = 0;
			    src_off[2] = 0;
			    res_off[2] = 0;
			    shft_off[2] = 0;
			    bnd_off[2] = 0;
			    curdim[3]++;
			    if (curdim[3] < src_ext[3]) {
				src_off[3] += src_strd[3];
				res_off[3] += res_strd[3];
				shft_off[3] += shft_strd[3];
				bnd_off[3] += bnd_strd[3];
			    } else {
				curdim[3] = 0;
				src_off[3] = 0;
				res_off[3] = 0;
				shft_off[3] = 0;
				bnd_off[3] = 0;
				curdim[4]++;
				if (curdim[4] < src_ext[4]) {
				    src_off[4] += src_strd[4];
				    res_off[4] += res_strd[4];
				    shft_off[4] += shft_strd[4];
				    bnd_off[4] += bnd_strd[4];
				} else {
				    curdim[4] = 0;
				    src_off[4] = 0;
				    res_off[4] = 0;
				    shft_off[4] = 0;
				    bnd_off[4] = 0;
				    curdim[5]++;
				    if (curdim[5] < src_ext[5]) {
					src_off[5] += src_strd[5];
					res_off[5] += res_strd[5];
					shft_off[5] += shft_strd[5];
					bnd_off[5] += bnd_strd[5];
				    }
				}
			    }
			}
		    }
		}
	    }
	}
}
