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


static const char USMID[] = "@(#) libfi/array/reshape.c	92.0	10/08/98 14:37:14";

#include <stddef.h>
#include <liberrno.h>
#include <stddef.h>
#include <cray/dopevec.h>
#include <cray/portdefs.h>
#include "arraydefs.h"

/*
 *	INCR_PAD increments the index for the pad array, and calculates
 *	the offset into the pad array.
 */

#define INCR_PAD()							\
	pad_indx[0]++;							\
	if (pad_indx[0] < pad_ext[0])					\
	    pad_off[0] = pad_indx[0] * pad_strd[0];			\
	else {								\
	    pad_indx[0] = 0;						\
	    pad_off[0] = 0;						\
	    pad_indx[1]++;						\
	    if (pad_indx[1] < pad_ext[1])				\
		pad_off[1] = pad_indx[1] * pad_strd[1];			\
	    else {							\
		pad_indx[1] = 0;					\
		pad_off[1] = 0;						\
		pad_indx[2]++;						\
		if (pad_indx[2] < pad_ext[2])				\
		    pad_off[2] = pad_indx[2] * pad_strd[2];		\
		else {							\
		    pad_indx[2] = 0;					\
		    pad_off[2] = 0;					\
		    pad_indx[3]++;					\
		    if (pad_indx[3] < pad_ext[3])			\
			pad_off[3] = pad_indx[3] * pad_strd[3];		\
		    else {						\
			pad_indx[3] = 0;				\
			pad_off[3] = 0;					\
			pad_indx[4]++;					\
			if (pad_indx[4] < pad_ext[4])			\
			    pad_off[4] = pad_indx[4] * pad_strd[4];	\
			else {						\
			    pad_indx[4] = 0;				\
			    pad_off[4] = 0;				\
			    pad_indx[5]++;				\
			    if (pad_indx[5] < pad_ext[5])		\
				pad_off[5] = pad_indx[5] * pad_strd[5];	\
			    else {					\
				pad_indx[5] = 0;			\
				pad_off[5] = 0;				\
				pad_indx[6]++;				\
				if (pad_indx[6] < pad_ext[6])		\
				    pad_off[6] = pad_indx[6] * pad_strd[6]; \
				else					\
				    pad_indx[6] = 0;			\
				    pad_off[6] = 0;			\
			    }						\
			}						\
		    }							\
		}							\
	    }								\
	}

/*
 *	ADD_INDEX calculates the index from the begging of an array.  The
 *	array is passed in as the first argument.
 */

#define ADD_INDEX(indx,off,rank)					\
	if (rank == 1) {						\
	    indx = off[0];						\
	} else if (rank == 2) {						\
	    indx = off[0] + off[1];					\
	} else if (rank == 3) {						\
	    indx = off[0] + off[1] + off[2];				\
	} else if (rank == 4) {						\
	    indx = off[0] + off[1] + off[2] + off[3];			\
	} else if (rank == 5) {						\
	    indx = off[0] + off[1] + off[2] + off[3] + off[4];		\
	} else if (rank == 6) {						\
	    indx = off[0] + off[1] + off[2] +				\
		   off[3] + off[4] + off[5];				\
	} else {							\
	    indx = off[0] + off[1] + off[2] + off[3] +			\
		   off[4] + off[5] + off[6];				\
	}

/*
 *	INCR_SRC increments the index for the source array, and calculates
 *	the offset into the source array.
 */

#define INCR_SRC()							\
    src_indx[0]++;							\
    if (src_indx[0] < src_ext[0])					\
	src_off[0] = src_indx[0] * src_strd[0];				\
    else {								\
	src_indx[0] = 0;						\
	src_off[0] = 0;							\
	src_indx[1]++;							\
	if (src_indx[1] < src_ext[1])					\
	    src_off[1] = src_indx[1] * src_strd[1];			\
	else {								\
	    src_indx[1] = 0;						\
	    src_off[1] = 0;						\
	    src_indx[2]++;						\
	    if (src_indx[2] < src_ext[2])				\
		src_off[2] = src_indx[2] * src_strd[2];			\
	    else {							\
		src_indx[2] = 0;					\
		src_off[2] = 0;						\
		src_indx[3]++;						\
		if (src_indx[3] < src_ext[3])				\
		    src_off[3] = src_indx[3] * src_strd[3];		\
		else {							\
		    src_indx[3] = 0;					\
		    src_off[3] = 0;					\
		    src_indx[4]++;					\
		    if (src_indx[4] < src_ext[4])			\
			src_off[4] = src_indx[4] * src_strd[4];		\
		    else {						\
			src_indx[4] = 0;				\
			src_off[4] = 0;					\
			src_indx[5]++;					\
			if (src_indx[5] < src_ext[5])			\
			    src_off[5] = src_indx[5] * src_strd[5];	\
			else {						\
			    src_indx[5] = 0;				\
			    src_off[5] = 0;				\
			    src_indx[6]++;				\
			    if (src_indx[6] < src_ext[6])		\
				src_off[6] = src_indx[6] * src_strd[6];	\
			}						\
		    }							\
		}							\
	    }								\
	}								\
    }

/*
 *	INCR_RES increments the index for the result array, and calculates
 *	the offset into the result array.
 */

#define INCR_RES()							\
    res_indx[0]++;							\
    if (res_indx[0] < res_ext[0])					\
	res_off[0] = res_indx[0] * res_strd[0];				\
    else {								\
	res_indx[0] = 0;						\
	res_off[0] = 0;							\
	res_indx[1]++;							\
	if (res_indx[1] < res_ext[1])					\
	    res_off[1] = res_indx[1] * res_strd[1];			\
	else {								\
	    res_indx[1] = 0;						\
	    res_off[1] = 0;						\
	    res_indx[2]++;						\
	    if (res_indx[2] < res_ext[2])				\
		res_off[2] = res_indx[2] * res_strd[2];			\
	    else {							\
		res_indx[2] = 0;					\
		res_off[2] = 0;						\
		res_indx[3]++;						\
		if (res_indx[3] < res_ext[3])				\
		    res_off[3] = res_indx[3] * res_strd[3];		\
		else {							\
		    res_indx[3] = 0;					\
		    res_off[3] = 0;					\
		    res_indx[4]++;					\
		    if (res_indx[4] < res_ext[4])			\
			res_off[4] = res_indx[4] * res_strd[4];		\
		    else {						\
			res_indx[4] = 0;				\
			res_off[4] = 0;					\
			res_indx[5]++;					\
			if (res_indx[5] < res_ext[5])			\
			    res_off[5] = res_indx[5] * res_strd[5];	\
			else {						\
			    res_indx[5] = 0;				\
			    res_off[5] = 0;				\
			    res_indx[6]++;				\
			    if (res_indx[6] < res_ext[6])		\
				res_off[6] = res_indx[6] * res_strd[6];	\
			}						\
		    }							\
		}							\
	    }								\
	}								\
    }

#if defined _F_INT4 && defined _F_INT8
#define VALUE(size, ptr) \
((int) (size == 64 ? (*((_f_int8 *) (ptr))) : (*((_f_int4 *) (ptr)))))
#else
#define VALUE(size, ptr) ((int) (*((_f_int *) (ptr))))
#endif

#ifdef _UNICOS
#pragma	_CRI duplicate _RESHAPE as RESHAPE@
#endif
void
_RESHAPE (	DopeVectorType	*result,
		DopeVectorType	*source,
		DopeVectorType	*shape,
		DopeVectorType	*pad,
		DopeVectorType	*order)

{
	int	*sh;		/* ptr to shape array		*/
	char	*cs;		/* char ptr to source array	*/
	char	*cr;		/* char ptr to result array	*/
	char	*cp;		/* char ptr to pad array	*/
	char		* restrict cptr1;	/* char 	*/
	char		* restrict cptr2;	/* char 	*/
	char		* restrict cptr3;	/* char 	*/
	_f_int8		* restrict uptr1;	/* 64-bit	*/
	_f_int8		* restrict uptr2;	/* 64-bit	*/
	_f_int8		* restrict uptr3;	/* 64-bit	*/
	_f_int		* restrict fptr1;	/* def kind int	*/
	_f_int		* restrict fptr2;	/* def kind int	*/
	_f_int		* restrict fptr3;	/* def kind int	*/
	_f_real16	* restrict dptr1;	/* 128-bit	*/
	_f_real16	* restrict dptr2;	/* 128-bit	*/
	_f_real16	* restrict dptr3;	/* 128-bit	*/
#ifdef _F_COMP16
	dblcmplx	* restrict xptr1;	/* 256-bit	*/
	dblcmplx	* restrict xptr2;	/* 256-bit	*/
	dblcmplx	* restrict xptr3;	/* 256-bit	*/
#endif
	_f_int4		* restrict hptr1;	/* 32-bit	*/
	_f_int4		* restrict hptr2;	/* 32-bit	*/
	_f_int4		* restrict hptr3;	/* 32-bit	*/
	void		* restrict sptr;	/* ptr to src	*/
	void		* restrict rptr;	/* ptr to res	*/
	void		* restrict pptr;	/* ptr to pad	*/
	_f_int4		* restrict optr4;	/* ptr to odr	*/
	_f_int8		* restrict optr8;	/* ptr to odr	*/
	_f_int4		* restrict shptr4;	/* ptr to odr	*/
	_f_int8		* restrict shptr8;	/* ptr to odr	*/
	long	sindx;		/* source index			*/
	long	rindx;		/* result index			*/
	long	shindx;		/* shape index			*/
	long	pindx;		/* pad index			*/
	long	oindx;		/* order index			*/
	_f_int	bucketsize;	/* size of each data element	*/
	long	nbytes;		/* # of bytes in result array	*/
	_f_int	bytealligned;	/* byte aligned flag		*/
	_f_int	type;		/* data type			*/
	_f_int	subtype;	/* sub-type			*/
	_f_int	arithmetic;	/* arithmetic data type		*/
	_f_int	rank;		/* rank of result matrix	*/
	long	src_ext[MAXDIM];   /* extents for source	*/
	long	src_strd[MAXDIM];  /* stride for source		*/
	long	src_off[MAXDIM];   /* source offset		*/
	long	src_indx[MAXDIM];  /* source index		*/
	long	res_ext[MAXDIM];   /* extents for result	*/
	long	res_strd[MAXDIM];  /* stride for result		*/
	long	res_off[MAXDIM];   /* result offset		*/
	long	res_indx[MAXDIM];  /* result index		*/
	long	pad_ext[MAXDIM];   /* extents for pad		*/
	long	pad_strd[MAXDIM];  /* stride for pad		*/
	long	pad_off[MAXDIM];   /* pad offset		*/
	long	pad_indx[MAXDIM];  /* pad index			*/
	_f_int	src_rank;	/* rank of source matrix	*/
	_f_int	res_rank;	/* rank of result matrix	*/
	_f_int	pad_rank;	/* rank of pad matrix		*/
	long	shp_strd;	/* stride for shape		*/
	long	ord_strd;	/* stride for order		*/
	long	tot_ext;	/* total extent counter		*/
	long	tot_src;
	long	tot_shp;
	long	total;
	_f_int	early_src;
	_f_int	early_pad;
	_f_int	early_shp;
	_f_int	early_ord;
	long	i, j, k;
	long	shape_len;
	long	order_len;
	_f_int	shbucket;
	_f_int	obucket;
	_f_int	order_chk[7];
	long	shp_vals[MAXDIM];

/*	Set type and rank variables	*/

	type = source->type_lens.type;
	rank = shape->dimension[0].extent;
	shape_len = shape->base_addr.a.el_len;

/*
 *	If the extent for any index of any array is 0, we can exit now without
 *	having to spend time doing any work.
 *
 *	If any of the indices passed in the shape array are less than 0,
 *	call the error routine.
 */

	if (shape->dimension[0].extent == 0) {
	    _lerror (_LELVL_ABORT, FESHPSZZ);
	}
/*
 *	Set up an array of the values of shape.  This will be done so that
 *	they can be done once, and referenced wherever they are needed.
 */

	if (shape->type_lens.int_len == 64) {
	    shptr8 = (_f_int8 *) shape->base_addr.a.ptr;
#ifdef _UNICOS
#pragma	_CRI	shortloop
#endif
	    for (i = 0; i < rank; i++) {
		shindx = i * (shape->dimension[0].stride_mult /
			(shape->type_lens.int_len / BITS_PER_WORD));
		shp_vals[i] = VALUE(shape_len,(shptr8 + shindx));
	    }
	} else {
	    shptr4 = (_f_int4 *) shape->base_addr.a.ptr;
	    shbucket = shape->type_lens.int_len / BITS_PER_WORD;
	    if (shbucket == 0)
		shbucket = 1;
#ifdef _UNICOS
#pragma	_CRI	shortloop
#endif
	    for (i = 0; i < rank; i++) {
		shindx = i * (shape->dimension[0].stride_mult / shbucket);
		shp_vals[i] = VALUE(shape_len,(shptr4 + shindx));
	    }
	}
		    
#ifdef _UNICOS
#pragma	_CRI	shortloop
#endif
	for (i = 0; i < rank; i++) {
	    if (shp_vals[i] < 0)
		_lerror (_LELVL_ABORT, FERSHNEG);
	}
	early_shp = 0;
#ifdef _UNICOS
#pragma	_CRI	shortloop
#endif
	for (i = 0; i < rank; i++) {
	    if (shp_vals[i] == 0)
		early_shp = 1;
	}

	early_src = 0;
#ifdef _UNICOS
#pragma	_CRI	shortloop
#endif
	for (i = 0; i < source->n_dim; i++) {
	    if (source->dimension[i].extent == 0) {
		early_src = 1;
	    }
	}

	early_pad = 0;
	if (pad) {
#ifdef _UNICOS
#pragma	_CRI	shortloop
#endif
	    for (i = 0; i < pad->n_dim; i++) {
		if (pad->dimension[i].extent == 0)
		    early_pad = 1;
	    }
	}

	early_ord = 0;
	if (order) {
	    if (order->dimension[0].extent == 0)
		early_ord = 1;
	}

/*
 *      Initialize every array element to 0.
 */

#ifdef _UNICOS
#pragma	_CRI	shortloop
#endif
        for (i = 0; i < MAXDIM; i++) {
            src_ext[i] = 0;
            src_strd[i] = 0;
            src_off[i] = 0;
            src_indx[i] = 0;
            res_ext[i] = 0;
            res_strd[i] = 0;
            res_off[i] = 0;
            res_indx[i] = 0;
            pad_ext[i] = 0;
            pad_strd[i] = 0;
            pad_off[i] = 0;
            pad_indx[i] = 0;
        }

/*	Size calculation is based on variable type	*/

	switch (type) {
	    case DVTYPE_ASCII :
		bytealligned = 1;
		bucketsize = _fcdlen(source->base_addr.charptr);  /* bytes */
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
#ifdef _F_COMP16
		} else if (source->type_lens.int_len == 256) {
		    subtype = DVSUBTYPE_BIT256;
#endif
		} else {
		    subtype = DVSUBTYPE_BIT128;
		}
		arithmetic = 1;
	}

/*	If not allocated, set up dope vector for result		*/

	if (!result->assoc) {
	    result->base_addr.a.ptr = (void *) NULL;
	    result->n_dim = rank;
	    result->orig_base = 0;
	    result->orig_size = 0;

/*	Determine size of space to allocate	*/

	    if (!bytealligned)
		nbytes = bucketsize * BYTES_PER_WORD;
	    else
		nbytes = bucketsize;
	    shindx = 0;
	    if (shape->type_lens.int_len == 64)
		shptr8 = (_f_int8 *) shape->base_addr.a.ptr;
	    else
		shptr4 = (_f_int4 *) shape->base_addr.a.ptr;
	    shbucket = shape->type_lens.int_len / BITS_PER_WORD;
	    if (shbucket == 0)
		shbucket = 1;
	    shp_strd = shape->dimension[0].stride_mult / shbucket;
	    tot_ext = 1;
	    for (i = 0; i < rank; i++) {
		result->dimension[i].extent = shp_vals[i];
		result->dimension[i].low_bound = 1;
		result->dimension[i].stride_mult = tot_ext * bucketsize;
		tot_ext *= shp_vals[i];
		nbytes *= shp_vals[i];
	    }
	    if (nbytes > 0 && early_ord != 1) {
		result->base_addr.a.ptr = (void *) malloc (nbytes);
		if (result->base_addr.a.ptr == NULL)
		    _lerror (_LELVL_ABORT, FENOMEMY);
	        result->assoc = 1;
	    } else
		result->base_addr.a.ptr = (void *) NULL;

	    result->base_addr.a.el_len = source->base_addr.a.el_len;
	    if (type == DVTYPE_ASCII) {
		cr = (char *) result->base_addr.a.ptr;
		result->base_addr.charptr = _cptofcd (cr, bucketsize);
	    }
	    result->orig_base = (void *) result->base_addr.a.ptr;
	    result->orig_size = nbytes * BITS_PER_BYTE;
	}

/*	Check to see if we can exit early	*/

	if (early_shp || early_ord)
	    return;
	if (early_src && (!pad || early_pad))
		_lerror (_LELVL_ABORT, FERSHNPD);

/*	Set up non-dopevector variables to hold index information	*/

	for (i = 0, tot_src = 1; i < source->n_dim; i++) {
	    src_ext[i] = source->dimension[i].extent;
	    if (bucketsize > 1 && arithmetic) {
		src_strd[i] = source->dimension[i].stride_mult / bucketsize;
	    } else {
		src_strd[i] = source->dimension[i].stride_mult;
	    }
	    tot_src *= src_ext[i];
	}
	if (order == NULL) {
	    for (i = 0; i < result->n_dim; i++) {
		res_ext[i] = result->dimension[i].extent;
		if (bucketsize > 1 && arithmetic) {
		    res_strd[i] = result->dimension[i].stride_mult / bucketsize;
		} else {
		    res_strd[i] = result->dimension[i].stride_mult;
		}
	    }
	} else {
	    oindx = 0;
	    obucket = order->type_lens.int_len / BITS_PER_WORD;
	    if (obucket == 0)
		obucket = 1;
	    ord_strd = order->dimension[0].stride_mult / obucket;
	    if (order->type_lens.int_len == 64) 
		optr8 = (_f_int8 *) order->base_addr.a.ptr;
	    else
		optr4 = (_f_int4 *) order->base_addr.a.ptr;
	    order_len = order->base_addr.a.el_len;
	    for (i = 0; i < result->n_dim; i++)
		order_chk[i] = -1;
	    for (i = 0; i < result->n_dim; i++) {
		if (order->type_lens.int_len == 64)
		    j = VALUE(order_len, (optr8 + oindx)) - 1;
		else
		    j = VALUE(order_len, (optr4 + oindx)) - 1;
		if (j < 0 || j >= result->n_dim) 
		    _lerror (_LELVL_ABORT, FEBDORDR);
		order_chk[j] = 1;
		oindx += ord_strd;
		res_ext[i] = result->dimension[j].extent;
		if (bucketsize > 1 && arithmetic) {
		    res_strd[i] = result->dimension[j].stride_mult / bucketsize;
		} else {
		    res_strd[i] = result->dimension[j].stride_mult;
		}
	    }
	    for (i = 0; i < result->n_dim; i++) {
		if (order_chk[i] != 1) {
		    _lerror (_LELVL_ABORT, FEBDORDR);
		}
	    }
	}
	if (pad && !early_pad) {
	    for (i = 0; i < pad->n_dim; i++) {
		pad_ext[i] = pad->dimension[i].extent;
		if (bucketsize > 1 && arithmetic) {
		    pad_strd[i] = pad->dimension[i].stride_mult / bucketsize;
		} else {
		    pad_strd[i] = pad->dimension[i].stride_mult;
		}
	    }
	} else {
	    tot_shp = 1;
	    for (i = 0, tot_shp = 1; i < shape->dimension[0].extent; i++) {
		tot_shp *= shp_vals[i];
	    }
	    if (tot_shp > tot_src) {
		_lerror (_LELVL_ABORT, FERSHNPD);
	    }
	}


/*	Initialize pointers to data areas		*/

	if (!bytealligned) {
	    sptr = (void *) source->base_addr.a.ptr;
	    rptr = (void *) result->base_addr.a.ptr;
	    if (pad)
		pptr = (void *) pad->base_addr.a.ptr;
	} else {
	    if (type == DVTYPE_ASCII) {
		cs = _fcdtocp (source->base_addr.charptr);
		cr = _fcdtocp (result->base_addr.charptr);
		if (pad)
		    cp = _fcdtocp (pad->base_addr.charptr);
	    } else {
		cs = (char *) source->base_addr.a.ptr;
		cr = (char *) result->base_addr.a.ptr;
		if (pad)
		    cp = (char *) pad->base_addr.a.ptr;
	    }
	}

/*	Initialize index counter variables		*/

	for (i = 0; i < MAXDIM; i++) {
	    res_off[i] = 0;
	    src_off[i] = 0;
	    pad_off[i] = 0;
	    res_indx[i] = 0;
	    src_indx[i] = 0;
	    pad_indx[i] = 0;
	}

/*	Initialize rank scalars		*/

	src_rank = source->n_dim;
	res_rank = result->n_dim;
	if (pad)
	    pad_rank = pad->n_dim;
	else
	    pad_rank = 0;

/*	Determine how many elements from source need to be moved	*/

	if (tot_src < tot_ext)
	    total = tot_src;
	else
	    total = tot_ext;

/*
 *	RESHAPE cannot be broken down by rank, because any rank can be put
 *	into any other rank.  Therefore, the only breakdown has been made by
 *	data type.  Since the values of the source array are not used, the
 *	breakdown is made by element size.  32-bit data is handled as one
 *	type, 64-bit data is another type, etc.  Character and derived byte
 *	are handled as one type, and derived word is a separate type.
 *
 *	The work is done by first determining how many source elements need
 *	to be moved.  This value is used as the loop counter for the first
 *	loop.  Inside the loop, the index values for the source and result
 *	elements are calculated, and that value is moved.  Then, the new
 *	values for source and result are calculated.  When all of the source
 *	elements have been moved, determine whether any pad values must be
 *	used.  If so, use a similar loop as the first one, but with the pad
 *	array rather than the source array.
 */

	switch (subtype) {

	    case DVSUBTYPE_BIT64 :
		uptr1 = (_f_int8 *) sptr;
		uptr2 = (_f_int8 *) rptr;
		for (i = 0; i < total; i++) {		/* move source	*/
		    ADD_INDEX(sindx,src_off,src_rank);
		    ADD_INDEX(rindx,res_off,res_rank);
		    uptr2[rindx] = uptr1[sindx];
		    INCR_SRC();
		    INCR_RES();
		}
		if (tot_src < tot_ext) {
		    uptr3 = (_f_int8 *) pptr;
		    for ( ; i < tot_ext; i++) {		/* move pad	*/
			ADD_INDEX(pindx,pad_off,pad_rank);
			ADD_INDEX(rindx,res_off,res_rank);
			uptr2[rindx] = uptr3[pindx];
			INCR_PAD();
			INCR_RES();
		    }
		}
		break;

	    case DVSUBTYPE_BIT32 :
		hptr1 = (_f_int4 *) sptr;
		hptr2 = (_f_int4 *) rptr;
		for (i = 0; i < total; i++) {		/* move source	*/
		    ADD_INDEX(sindx,src_off,src_rank);
		    ADD_INDEX(rindx,res_off,res_rank);
		    hptr2[rindx] = hptr1[sindx];
		    INCR_SRC();
		    INCR_RES();
		}
		if (tot_src < tot_ext) {
		    hptr3 = (_f_int4 *) pptr;
		    for ( ; i < tot_ext; i++) {		/* move pad	*/
			ADD_INDEX(pindx,pad_off,pad_rank);
			ADD_INDEX(rindx,res_off,res_rank);
			hptr2[rindx] = hptr3[pindx];
			INCR_PAD();
			INCR_RES();
		    }
		}
		break;

	    case DVSUBTYPE_BIT128 :
		dptr1 = (_f_real16 *) sptr;
		dptr2 = (_f_real16 *) rptr;
		for (i = 0; i < total; i++) {		/* move source	*/
		    ADD_INDEX(sindx,src_off,src_rank);
		    ADD_INDEX(rindx,res_off,res_rank);
		    dptr2[rindx] = dptr1[sindx];
		    INCR_SRC();
		    INCR_RES();
		}
		if (tot_src < tot_ext) {
		    dptr3 = (_f_real16 *) pptr;
		    for ( ; i < tot_ext; i++) {		/* move pad	*/
			ADD_INDEX(pindx,pad_off,pad_rank);
			ADD_INDEX(rindx,res_off,res_rank);
			dptr2[rindx] = dptr3[pindx];
			INCR_PAD();
			INCR_RES();
		    }
		}
		break;

	    case DVSUBTYPE_CHAR :
		for (i = 0; i < total; i++) {		/* move source	*/
		    ADD_INDEX(sindx,src_off,src_rank);
		    ADD_INDEX(rindx,res_off,res_rank);
		    cptr1 = (char *) cs + sindx;
		    cptr2 = (char *) cr + rindx;
		    (void) memcpy (cptr2, cptr1, bucketsize);
		    INCR_SRC();
		    INCR_RES();
		}
		if (tot_src < tot_ext) {
		    for ( ; i < tot_ext; i++) {
			ADD_INDEX(pindx,pad_off,pad_rank);
			ADD_INDEX(rindx,res_off,res_rank);
			cptr2 = (char *) cr + rindx;
			cptr3 = (char *) cp + pindx;
			(void) memcpy (cptr2, cptr3, bucketsize);
			INCR_PAD();
			INCR_RES();
		    }
		}
		break;

	    case DVSUBTYPE_DERIVED :
		for (i = 0; i < bucketsize; i++) {
		    fptr1 = (_f_int *) sptr + i;
		    fptr2 = (_f_int *) rptr + i;
#ifdef _UNICOS
#pragma _CRI	shortloop
#endif
		    for (j = 0; j < MAXDIM; j++) {
			src_indx[j] = 0;
			src_off[j] = 0;
		    }
#ifdef _UNICOS
#pragma _CRI	shortloop
#endif
		    for (j = 0; j < MAXDIM; j++) {
			res_indx[j] = 0;
			res_off[j] = 0;
		    }
		    for (j = 0; j < total; j++) {	/* move source	*/
			ADD_INDEX(sindx,src_off,src_rank)
			ADD_INDEX(rindx,res_off,res_rank)
			fptr2[rindx] = fptr1[sindx];
			INCR_SRC();
			INCR_RES();
		    }
		    if (tot_src < tot_ext) {
			fptr3 = (_f_int *) pptr + i;
#ifdef _UNICOS
#pragma _CRI	shortloop
#endif
			for (k = 0; k < pad_rank; k++) {
			    pad_indx[k] = 0;
			    pad_off[k] = 0;
			}
			for ( ; j < tot_ext; j++) {	/* move pad	*/
			    ADD_INDEX(pindx,pad_off,pad_rank);
			    ADD_INDEX(rindx,res_off,res_rank);
			    fptr2[rindx] = fptr3[pindx];
			    INCR_PAD();
			    INCR_RES();
			}
		    }
		}
		break;

#ifdef _F_COMP16
	    case DVSUBTYPE_BIT256 :
		xptr1 = (dblcmplx *) sptr;
	 	xptr2 = (dblcmplx *) rptr;
		for (i = 0; i < total; i++) {		/* move source	*/
		    ADD_INDEX(sindx,src_off,src_rank);
		    ADD_INDEX(rindx,res_off,res_rank);
		    xptr2[rindx].re = xptr1[sindx].re;
		    xptr2[rindx].im = xptr1[sindx].im;
		    INCR_SRC();
		    INCR_RES();
		}
		if (tot_src < tot_ext) {
		    xptr3 = (dblcmplx *) pptr;
		    for ( ; i < tot_ext; i++) {		/* move pad	*/
			ADD_INDEX(pindx,pad_off,pad_rank);
			ADD_INDEX(rindx,res_off,res_rank);
			xptr2[rindx].re = xptr3[pindx].re;
			xptr2[rindx].im = xptr3[pindx].im;
			INCR_PAD();
			INCR_RES();
		    }
		}
		break;
#endif

	    default :
		_lerror (_LELVL_ABORT, FEINTDTY);
	}
}
