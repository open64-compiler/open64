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


#pragma ident "@(#) libfi/array/pack.c	92.1	07/07/99 15:52:02"

#include <stddef.h>
#include <liberrno.h>
#include <cray/dopevec.h>
#include <cray/portdefs.h>
#include "arraydefs.h"

/*
 *	Pack an array into an array of rank one under the control of a
 *	mask.
 *
 *	Calculate the indices which will be used in the outer loop
 *	of the rank 3-7 block.  Indices are calculated for the mask
 *	and source matrices.
 */

#define FIND_INDX()							\
	switch (rank) {							\
	    case 3 :							\
		indx1_msk = msk_off[0] + msk_off[1];			\
		indx1_src = src_off[0] + src_off[1];			\
		break;							\
	    case 4 :							\
		indx1_msk = msk_off[0] + msk_off[1] + msk_off[2];	\
		indx1_src = src_off[0] + src_off[1] + src_off[2];	\
		break;							\
	    case 5 :							\
		indx1_msk = msk_off[0] + msk_off[1] +			\
			    msk_off[2] + msk_off[3];			\
		indx1_src = src_off[0] + src_off[1] +			\
			    src_off[2] + src_off[3];			\
		break;							\
	    case 6 :							\
		indx1_msk = msk_off[0] + msk_off[1] + msk_off[2] +	\
			    msk_off[3] + msk_off[4];			\
		indx1_src = src_off[0] + src_off[1] + src_off[2] +	\
			    src_off[3] + src_off[4];			\
		break;							\
	    default :							\
		indx1_msk = msk_off[0] + msk_off[1] + msk_off[2] +	\
			    msk_off[3] + msk_off[4] + msk_off[5];	\
		indx1_src = src_off[0] + src_off[1] + src_off[2] +	\
			    src_off[3] + src_off[4] + src_off[5];	\
	}

/*
 *	Increment the dimension counters used in the rank 3-7 block.
 *	In addition, calculate the offsets for each of the 2-rank
 *	dimensions.  These offsets are used in the FIND_INDX macro
 *	to calculate the index to be used for the mask and the source
 *	matrices.
 */

#define	INCREMENT()							\
	curdim[0]++;							\
	if (curdim[0] < src_ext[1]) {					\
	    msk_off[0] = curdim[0] * msk_strd[1];			\
	    src_off[0] = curdim[0] * src_strd[1];			\
	} else {							\
	    curdim[0] = 0;						\
	    msk_off[0] = 0;						\
	    src_off[0] = 0;						\
	    curdim[1]++;						\
	    if (curdim[1] < src_ext[2]) {				\
		msk_off[1] = curdim[1] * msk_strd[2];			\
		src_off[1] = curdim[1] * src_strd[2];			\
	    } else {							\
		curdim[1] = 0;						\
		msk_off[1] = 0;						\
		src_off[1] = 0;						\
		curdim[2]++;						\
		if (curdim[2] < src_ext[3]) {				\
		    msk_off[2] = curdim[2] * msk_strd[3];		\
		    src_off[2] = curdim[2] * src_strd[3];		\
		} else {						\
		    curdim[2] = 0;					\
		    msk_off[2] = 0;					\
		    src_off[2] = 0;					\
		    curdim[3]++;					\
		    if (curdim[3] < src_ext[4]) {			\
			msk_off[3] = curdim[3] * msk_strd[4];		\
			src_off[3] = curdim[3] * src_strd[4];		\
		    } else {						\
			curdim[3] = 0;					\
			msk_off[3] = 0;					\
			src_off[3] = 0;					\
			curdim[4]++;					\
			if (curdim[4] < src_ext[5]) {			\
			    msk_off[4] = curdim[4] * msk_strd[5];	\
			    src_off[4] = curdim[4] * src_strd[5];	\
			} else {					\
			    curdim[4] = 0;				\
			    msk_off[4] = 0;				\
			    src_off[4] = 0;				\
			    curdim[5]++;				\
			    if (curdim[5] < src_ext[6]) {		\
				msk_off[5] = curdim[5] * msk_strd[6];	\
				src_off[5] = curdim[5] * src_strd[6];	\
			    }						\
			}						\
		    }							\
		}							\
	    }								\
	}

#ifdef _UNICOS
#pragma	_CRI duplicate _PACK as PACK@
#endif
void
_PACK (	DopeVectorType * result,
	DopeVectorType * source,
	DopeVectorType * mask,
	DopeVectorType * vector)

{
	char	*cs;		/* char ptr to source array	*/
	char	*cr;		/* char ptr to result array	*/
	char	*cv;		/* char ptr to vector array	*/
	char		* restrict cptr1;	/* char			*/
	char		* restrict cptr2;	/* char			*/
	char		* restrict cptr3;	/* char			*/
	_f_int8		* restrict uptr1;	/* 64-bit		*/
	_f_int8		* restrict uptr2;	/* 64-bit		*/
	_f_int8		* restrict uptr3;	/* 64-bit		*/
	_f_int		* restrict fptr1;	/* default-size		*/
	_f_int		* restrict fptr2;	/* default-size		*/
	_f_int		* restrict fptr3;	/* default-size		*/
	_f_real16	* restrict dptr1;	/* 128-bit		*/
	_f_real16	* restrict dptr2;	/* 128-bit		*/
	_f_real16	* restrict dptr3;	/* 128-bit		*/
#ifdef _F_COMP16
	dblcmplx	* restrict xptr1;	/* 256-bit		*/
	dblcmplx	* restrict xptr2;	/* 256-bit		*/
	dblcmplx	* restrict xptr3;	/* 256-bit		*/
#endif
	_f_int4		* restrict hptr1;	/* 32-bit		*/
	_f_int4		* restrict hptr2;	/* 32-bit		*/
	_f_int4		* restrict hptr3;	/* 32-bit		*/
	_f_mask		* restrict iptr4;	/* def kind mask	*/
	void		* restrict sptr;	/* ptr to source	*/
	void		* restrict rptr;	/* ptr to result	*/
	void		* restrict mptr;	/* ptr to mask		*/
	void		* restrict vptr;	/* ptr to vector	*/
	_f_int	bucketsize;	/* size of each data element	*/
	long	nbytes;		/* # of bytes in data array	*/
	long	nwords;		/* # of words in data array	*/
	long	curdim[MAXDIM];	/* current indices		*/
	_f_int	bytealligned;	/* byte alligned flag		*/
	long	sindx;		/* source index			*/
	long	rindx;		/* result index			*/
	long	mindx;		/* mask index			*/
	long	vindx;		/* vector index			*/
	_f_int	type;		/* type scalar			*/
	_f_int	subtype;	/* sub-type			*/
	_f_int	arithmetic;	/* arithmetic			*/
	_f_int	rank;		/* dimension of source scalar	*/
	long	i, j, k;	/* index variables		*/
	long	res_strd;	/* element stride for result	*/
	long	vec_strd;	/* element stride for result	*/
	long	src_ext[MAXDIM];   /* extents for source	*/
	long	src_strd[MAXDIM];  /* element stride for source	*/
	long	src_off[MAXDIM];   /* offset values for source	*/
	long	msk_strd[MAXDIM];  /* element stride for mask	*/
	long	msk_off[MAXDIM];   /* offset values for mask	*/
	long	indx1_src;	/* index for dim 1 of source	*/
	long	indx2_src;	/* index for dim 2 of source	*/
	long	indx1_vec;	/* index for dim 1 of vector	*/
	long	indx2_vec;	/* index for dim 2 of vector	*/
	long	indx1_res;	/* index for dim 1 of result	*/
	long	indx2_res;	/* index for dim 2 of result	*/
	long	indx1_msk;	/* index for dim 1 of msk	*/
	long	indx2_msk;	/* index for dim 2 of msk	*/
	long	total_ext;	/* total extent counter		*/
	long	src_ext1;	/* extent for dim 1 of source	*/
	long	src_ext2;	/* extent for dim 1 of source	*/
	long	found;		/* count of # entries in result */
	long	mask_el_len;
	_f_int	early_exit;	/* early exit flag		*/

/*	Set type and dimension global variables		*/

	type = source->type_lens.type;
	rank = source->n_dim;
	mask_el_len = mask->base_addr.a.el_len;

/*
 *      Check to see if any of the matrices have size 0.  If any do,
 *      return without doing anything.
 */

	early_exit = 0;
#ifdef _UNICOS
#pragma _CRI     shortloop
#endif
	for (i = 0; i < rank; i++) {
	    if (!source->dimension[i].extent)
		early_exit = 1;
	}
	if (result->assoc) {
	    if (!result->dimension[0].extent)
		early_exit = 1;
	}
	if (vector) {
	    if (!vector->dimension[0].extent)
		early_exit = 1;
	}
	if (mask) {
	    if (mask->n_dim > 1) {
#ifdef _UNICOS
#pragma _CRI     shortloop
#endif
		for (i = 0; i < rank; i++)
		    if (!mask->dimension[i].extent)
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
	    msk_strd[i] = 0;
	    msk_off[i] = 0;
	}

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

/*	If necessary, fill result dope vector		*/

	if (!result->assoc) {
	    result->base_addr.a.ptr = (void *) NULL;
	    result->orig_base = 0;
	    result->orig_size = 0;

/*	Determine size of space to allocate	*/

	    if (!bytealligned) {
		nbytes = bucketsize * BYTES_PER_WORD;
#ifdef _CRAYMPP
		if (subtype == DVSUBTYPE_BIT32)
		    nbytes /= 2;
#endif
	    } else {
		nbytes = bucketsize;
	    }
	    if (vector) {
		nbytes *= vector->dimension[0].extent;
		nwords = vector->dimension[0].extent;
	    } else {
#ifdef _UNICOS
#pragma	_CRI	shortloop
#endif
		for (i = 0; i < rank; i++)
		    nbytes *= source->dimension[i].extent;
		nwords = nbytes / BYTES_PER_WORD;
	    }
	    if (nbytes > 0) {
		result->base_addr.a.ptr = (void *) malloc (nbytes);
		if (result->base_addr.a.ptr == NULL)
		    _lerror (_LELVL_ABORT, FENOMEMY);
	    }

	    result->assoc = 1;
	    result->base_addr.a.el_len = source->base_addr.a.el_len;
	    if (type == DVTYPE_ASCII) {
		cr = (char *) result->base_addr.a.ptr;
		result->base_addr.charptr = _cptofcd (cr, bucketsize);
	    }
	    result->orig_size = nbytes * BITS_PER_BYTE;

/*
 *	These are initial values which may be changed when it is
 *	determined how big the result array actually is.
 */
	    result->dimension[0].low_bound = 1;
	    result->dimension[0].extent = nwords;
	    result->dimension[0].stride_mult = bucketsize;

/*	if result array is already allocated	*/

	} else {
	    if (!bytealligned)
		nbytes = bucketsize * BYTES_PER_WORD;
	    else
		nbytes = bucketsize;
	    if (vector) {
		nbytes *= vector->dimension[0].extent;
		nwords = vector->dimension[0].extent;
	    } else {
	 	nwords = 1;
		for (i = 0; i < rank; i++) {
		    nbytes *= source->dimension[i].extent;
		    nwords *= source->dimension[i].extent;
		}
	    }
	}

/*	If early exit is required, exit now	*/

	if (early_exit)
	    return;
	if (mask) {
	    iptr4 = (_f_mask *) mask->base_addr.a.ptr;
	    if (mask->n_dim == 0 && !(vector) &&
		!LTOB(mask_el_len, &iptr4[0])) {
		result->dimension[0].extent = 0;
		return;
	    }
	}

/*	Set up scalar pointers to all of the argument data areas	*/

	if (mask)
	    mptr = (void *) mask->base_addr.a.ptr;
	if (!bytealligned) {
	    sptr = (void *) source->base_addr.a.ptr;
	    rptr = (void *) result->base_addr.a.ptr;
	    if (vector)
		vptr = (void *) vector->base_addr.a.ptr;
	} else {
	    if (type == DVTYPE_ASCII) {
		cs = _fcdtocp (source->base_addr.charptr);
		cr = _fcdtocp (result->base_addr.charptr);
		if (vector)
		    cv = _fcdtocp (vector->base_addr.charptr);
	    } else {
		cs = (char *) source->base_addr.a.ptr;
		cr = (char *) result->base_addr.a.ptr;
		if (vector)
		    cv = (char *) vector->base_addr.a.ptr;
	    }
	}

/*	Set up some 'shortcut' variables used for index calculation	*/

	if (bucketsize > 1 && arithmetic) {
	    res_strd = result->dimension[0].stride_mult / bucketsize;
	    if (vector)
		vec_strd = vector->dimension[0].stride_mult / bucketsize;
	} else {
	    res_strd = result->dimension[0].stride_mult;
	    if (vector)
		vec_strd = vector->dimension[0].stride_mult;
	}

#ifdef _UNICOS
#pragma _CRI	shortloop
#endif
	for (i = 0; i < rank; i++) {
	    src_ext[i] = source->dimension[i].extent;
	    if (bucketsize > 1 && arithmetic) {
		src_strd[i] = source->dimension[i].stride_mult / bucketsize;
	    } else {
		src_strd[i] = source->dimension[i].stride_mult;
	    }
	}
	if (mask->n_dim > 0) {
#ifdef _UNICOS
#pragma _CRI	shortloop
#endif
	    for (i = 0; i < rank; i++) {
		msk_strd[i] = mask->dimension[i].stride_mult;
		iptr4 = (_f_mask *) mptr;
#ifdef	_CRAYMPP
		if (mask_el_len == 64 && sizeof (iptr4[0]) == 4)
		    msk_strd[i] <<= 1;
#endif
	    }
	}

/*
 *	The program is divided up into three blocks.  The first block deals
 *	with arrays of rank 1.  Inside each block, the data types are broken
 *	up into groups based on container size.  Integer, real, and logical
 *	types are all one word, and the actual value is not used, so they
 *	are all grouped together and treated as long.  The same is
 *	true for double and complex, as well as ascii and derivedbyte.
 *
 *	For each group, the mask array is checked for true values.  When one
 *	is encountered, the corresponding value from the source array is put
 *	into the next available position in the result array.  If no vector
 *	is passed, the routine is finished at this point with the result
 *	array length set to the number of true elements in the mask.  If a
 *	vector is furnished, the size of the vector determines the size of
 *	the result array.  If this size has been reached, the routine is done.
 *	If not, elements from the vector array are put into the result array
 *	until it is full.
 */

	if (rank == 1) {
	    found = 0;
	    iptr4 = (_f_mask *) mptr;
	    switch (subtype) {
		case DVSUBTYPE_BIT64 :
		    uptr1 = (_f_int8 *) sptr;
		    uptr2 = (_f_int8 *) vptr;
		    uptr3 = (_f_int8 *) rptr;
		    rindx = 0;
		    mindx = 0;
		    vindx = 0;
		    sindx = 0;
		    src_ext1 = source->dimension[0].extent;
		    for (i = 0; i < src_ext1; i++) {
			if (LTOB(mask_el_len, &iptr4[mindx])) {
			    sindx = i * src_strd[0];
			    uptr3[rindx] = uptr1[sindx];
			    rindx += res_strd;
			    found++;
			}
			mindx += msk_strd[0];
		    }
		    if (!vector || found == nwords) {
			result->dimension[0].extent = found;
		    } else {
			vindx = found * vec_strd;
			for ( ; found < nwords; found++) {
			    uptr3[rindx] = uptr2[vindx];
			    rindx += res_strd;
			    vindx += vec_strd;
			}
		    }
		    break;

		case DVSUBTYPE_BIT32 :
		    hptr1 = (_f_int4 *) sptr;
		    hptr2 = (_f_int4 *) vptr;
		    hptr3 = (_f_int4 *) rptr;
		    rindx = 0;
		    mindx = 0;
		    vindx = 0;
		    sindx = 0;
		    src_ext1 = source->dimension[0].extent;
		    for (i = 0; i < src_ext1; i++) {
			if (LTOB(mask_el_len, &iptr4[mindx])) {
			    sindx = i * src_strd[0];
			    hptr3[rindx] = hptr1[sindx];
			    rindx += res_strd;
			    found++;
			}
			mindx += msk_strd[0];
		    }
		    if (!vector || found == nwords) {
			result->dimension[0].extent = found;
		    } else {
			vindx = found * vec_strd;
			for ( ; found < nwords; found++) {
			    hptr3[rindx] = hptr2[vindx];
			    rindx += res_strd;
			    vindx += vec_strd;
			}
		    }
		    break;

		case DVSUBTYPE_BIT128 :
		    dptr1 = (_f_real16 *) sptr;
		    dptr2 = (_f_real16 *) vptr;
		    dptr3 = (_f_real16 *) rptr;
		    rindx = 0;
		    mindx = 0;
		    vindx = 0;
		    sindx = 0;
		    src_ext1 = source->dimension[0].extent;
		    for (i = 0; i < src_ext1; i++) {
			if (LTOB(mask_el_len, &iptr4[mindx])) {
			    sindx = i * src_strd[0];
			    dptr3[rindx] = dptr1[sindx];
			    rindx += res_strd;
			    found++;
			}
			mindx += msk_strd[0];
		    }
		    if (!vector || found == nwords) {
			result->dimension[0].extent = found;
		    } else {
			vindx = found * vec_strd;
			for ( ; found < nwords; found++) {
			    dptr3[rindx] = dptr2[vindx];
			    rindx += res_strd;
			    vindx += vec_strd;
			}
		    }
		    break;

		case DVSUBTYPE_CHAR :
		    rindx = 0;
		    mindx = 0;
		    vindx = 0;
		    sindx = 0;
		    src_ext1 = source->dimension[0].extent;
		    for (i = 0; i < src_ext1; i++) {
			if (LTOB(mask_el_len, &iptr4[mindx])) {
			    cptr3 = (char *) cr + rindx;
			    cptr1 = (char *) cs + (i * src_strd[0]);
			    (void) memcpy (cptr3, cptr1, bucketsize);
			    rindx += res_strd;
			    found++;
			}
			mindx += msk_strd[0];
		    }
		    if (!vector || found == nwords) {
			result->dimension[0].extent = found;
		    } else {
			vindx = found * vec_strd;
			for ( ; found < nwords; found++) {
			    cptr3 = (char *) cr + rindx;
			    cptr2 = (char *) cv + vindx;
			    (void) memcpy (cptr3, cptr2, bucketsize);
			    rindx += res_strd;
			    vindx += vec_strd;
			}
		    }
		    break;

		case DVSUBTYPE_DERIVED :
		    fptr1 = (_f_int *) sptr;
		    fptr2 = (_f_int *) vptr;
		    fptr3 = (_f_int *) rptr;
		    src_ext1 = source->dimension[0].extent;
		    indx1_res = 0;
/*
 *	The derived word type is handled the same as the other types except
 *	that another loop is added.  The assumption was made that extent of
 *	the array would be larger than the number of words in the derived
 *	type.  Therefore, to try and make this routine optimal, the first
 *	loop uses the extent as its inner loop, which should provide better
 *	optimization.  The second loop is also done this way.
 */
		    for (i = 0; i < bucketsize; i++) {
			rindx = i;
			mindx = 0;
			for (j = 0; j < src_ext1; j++) {
			    if (LTOB(mask_el_len, &iptr4[mindx])) {
				sindx = i + (j * src_strd[0]);
				fptr3[rindx] = fptr1[sindx];
				rindx += res_strd;
				if (i == 0) {
				    indx1_res = rindx;
				    found++;
				}
			    }
			    mindx += msk_strd[0];
			}
		    }
		    if (!vector || found == nwords) {
			result->dimension[0].extent = found;
		    } else {
			indx1_vec = found * vec_strd;
			found = nwords - found;
			for (i = 0; i < bucketsize; i++) {
			    rindx = indx1_res + i;
			    vindx = indx1_vec + i;
			    for (j = 0; j < found; j++) {
				fptr3[rindx] = fptr2[vindx];
				rindx += res_strd;
				vindx += vec_strd;
			    }
			}
		    }
		    break;

#ifdef _F_COMP16
		case DVSUBTYPE_BIT256 :
		    xptr1 = (dblcmplx *) sptr;
		    xptr2 = (dblcmplx *) vptr;
		    xptr3 = (dblcmplx *) rptr;
		    rindx = 0;
		    mindx = 0;
		    vindx = 0;
		    sindx = 0;
		    src_ext1 = source->dimension[0].extent;
		    for (i = 0; i < src_ext1; i++) {
			if (LTOB(mask_el_len, &iptr4[mindx])) {
			    sindx = i * src_strd[0];
			    xptr3[rindx].re = xptr1[sindx].re;
			    xptr3[rindx].im = xptr1[sindx].im;
			    rindx += res_strd;
			    found++;
			}
			mindx += msk_strd[0];
		    }
		    if (!vector || found == nwords) {
			result->dimension[0].extent = found;
		    } else {
			vindx = found * vec_strd;
			for ( ; found < nwords; found++) {
			    xptr3[rindx].re = xptr2[vindx].re;
			    xptr3[rindx].im = xptr2[vindx].im;
			    rindx += res_strd;
			    vindx += vec_strd;
			}
		    }
		    break;
#endif

		default :
		    _lerror (_LELVL_ABORT, FEINTDTY);
	    }
	} else if (rank == 2) {

/*
 *	Rank 2 matrices are handled in a manner similar to rank 1 arrays,
 *	except that the first loop in each data type is a nested loop, with
 *	the outer loop being the second dimension, and the inner loop being
 *	the first.  This preserves the storage order which is necessary for
 *	pack to work.  The second part of each block is not affected by the
 *	number of dimensions in the source matrix.
 */

	    found = 0;
	    iptr4 = (_f_mask *) mptr;
	    switch (subtype) {
		case DVSUBTYPE_BIT64 :
		    uptr1 = (_f_int8 *) sptr;
		    uptr2 = (_f_int8 *) vptr;
		    uptr3 = (_f_int8 *) rptr;
		    indx2_msk = 0;
		    indx2_src = 0;
		    rindx = 0;
		    src_ext1 = src_ext[0];
		    src_ext2 = src_ext[1];
		    for (i = 0; i < src_ext2; i++) {
			indx1_msk = 0;
			for (j = 0; j < src_ext1; j++) {
			    mindx = indx1_msk + indx2_msk;
			    if (LTOB(mask_el_len, &iptr4[mindx])) {
				sindx = indx2_src + (j * src_strd[0]);
				uptr3[rindx] = uptr1[sindx];
				rindx += res_strd;
				found++;
			    }
			    indx1_msk += msk_strd[0];
			}
			indx2_msk += msk_strd[1];
			indx2_src += src_strd[1];
		    }
		    if (!vector || found == nwords) {
			result->dimension[0].extent = found;
		    } else {
			vindx = found * vec_strd;
			for ( ; found < nwords; found++) {
			    uptr3[rindx] = uptr2[vindx];
			    rindx += res_strd;
			    vindx += vec_strd;
			}
		    }
		    break;

		case DVSUBTYPE_BIT32 :
		    hptr1 = (_f_int4 *) sptr;
		    hptr2 = (_f_int4 *) vptr;
		    hptr3 = (_f_int4 *) rptr;
		    indx2_msk = 0;
		    indx2_src = 0;
		    rindx = 0;
		    src_ext1 = src_ext[0];
		    src_ext2 = src_ext[1];
		    for (i = 0; i < src_ext2; i++) {
			indx1_msk = 0;
			for (j = 0; j < src_ext1; j++) {
			    mindx = indx1_msk + indx2_msk;
			    if (LTOB(mask_el_len, &iptr4[mindx])) {
				sindx = indx2_src + (j * src_strd[0]);
				hptr3[rindx] = hptr1[sindx];
				rindx += res_strd;
				found++;
			    }
			    indx1_msk += msk_strd[0];
			}
			indx2_msk += msk_strd[1];
			indx2_src += src_strd[1];
		    }
		    if (!vector || found == nwords) {
			result->dimension[0].extent = found;
		    } else {
			vindx = found * vec_strd;
			for ( ; found < nwords; found++) {
			    hptr3[rindx] = hptr2[vindx];
			    rindx += res_strd;
			    vindx += vec_strd;
			}
		    }
		    break;

		case DVSUBTYPE_BIT128 :
		    dptr1 = (_f_real16 *) sptr;
		    dptr2 = (_f_real16 *) vptr;
		    dptr3 = (_f_real16 *) rptr;
		    indx2_msk = 0;
		    indx2_src = 0;
		    rindx = 0;
		    src_ext1 = src_ext[0];
		    src_ext2 = src_ext[1];
		    for (i = 0; i < src_ext2; i++) {
			indx1_msk = 0;
			for (j = 0; j < src_ext1; j++) {
			    mindx = indx1_msk + indx2_msk;
			    if (LTOB(mask_el_len, &iptr4[mindx])) {
				sindx = indx2_src + (j * src_strd[0]);
				dptr3[rindx] = dptr1[sindx];
				rindx += res_strd;
				found++;
			    }
			    indx1_msk += msk_strd[0];
			}
			indx2_msk += msk_strd[1];
			indx2_src += src_strd[1];
		    }
		    if (!vector || found == nwords) {
			result->dimension[0].extent = found;
		    } else {
			vindx = found * vec_strd;
			for ( ; found < nwords; found++) {
			    dptr3[rindx] = dptr2[vindx];
			    rindx += res_strd;
			    vindx += vec_strd;
			}
		    }
		    break;

		case DVSUBTYPE_CHAR :
		    indx2_msk = 0;
		    indx2_src = 0;
		    rindx = 0;
		    src_ext1 = src_ext[0];
		    src_ext2 = src_ext[1];
		    for (i = 0; i < src_ext2; i++) {
			indx1_msk = 0;
			for (j = 0; j < src_ext1; j++) {
			    mindx = indx1_msk + indx2_msk;
			    if (LTOB(mask_el_len, &iptr4[mindx])) {
				sindx = indx2_src + (j * src_strd[0]);
				cptr1 = (char *) cs + sindx;
				cptr3 = (char *) cr + rindx;
				(void) memcpy (cptr3, cptr1, bucketsize);
				rindx += res_strd;
				found++;
			    }
			    indx1_msk += msk_strd[0];
			}
			indx2_msk += msk_strd[1];
			indx2_src += src_strd[1];
		    }
		    if (!vector || found == nwords) {
			result->dimension[0].extent = found;
		    } else {
			vindx = found * vec_strd;
			for ( ; found < nwords; found++) {
			    cptr2 = (char *) cv + vindx;
			    cptr3 = (char *) cr + rindx;
			    (void) memcpy (cptr3, cptr2, bucketsize);
			    rindx += res_strd;
			    vindx += vec_strd;
			}
		    }
		    break;

		case DVSUBTYPE_DERIVED :
		    fptr1 = (_f_int *) sptr;
		    fptr2 = (_f_int *) vptr;
		    fptr3 = (_f_int *) rptr;
		    src_ext1 = src_ext[0];
		    src_ext2 = src_ext[1];
		    for (i = 0; i < bucketsize; i++) {
			indx2_msk = 0;
			indx2_src = 0;
			rindx = i;
			for (j = 0; j < src_ext2; j++) {
			    indx1_msk = 0;
			    for (k = 0; k < src_ext1; k++) {
				mindx = indx1_msk + indx2_msk;
				if (LTOB(mask_el_len, &iptr4[mindx])) {
				    sindx = indx2_src + i + (k * src_strd[0]);
				    fptr3[rindx] = fptr1[sindx];
				    rindx += res_strd;
				    if (i == 0)
					found++;
				}
				indx1_msk += msk_strd[0];
			    }
			    indx2_msk += msk_strd[1];
			    indx2_src += src_strd[1];
			}
		    }
		    if (!vector || found == nwords) {
			result->dimension[0].extent = found;
		    } else {
			indx1_res = found * res_strd;
			indx1_vec = found * vec_strd;
			found = nwords - found;
			for (i = 0; i < bucketsize; i++) {
			    rindx = indx1_res + i;
			    vindx = indx1_vec + i;
			    for (j = 0; j < found; j++) {
				fptr3[rindx] = fptr2[vindx];
				rindx += res_strd;
				vindx += vec_strd;
			    }
			}
		    }
		    break;

#ifdef _F_COMP16
		case DVSUBTYPE_BIT256 :
		    xptr1 = (dblcmplx *) sptr;
		    xptr2 = (dblcmplx *) vptr;
		    xptr3 = (dblcmplx *) rptr;
		    indx2_msk = 0;
		    indx2_src = 0;
		    rindx = 0;
		    src_ext1 = src_ext[0];
		    src_ext2 = src_ext[1];
		    for (i = 0; i < src_ext2; i++) {
			indx1_msk = 0;
			for (j = 0; j < src_ext1; j++) {
			    mindx = indx1_msk + indx2_msk;
			    if (LTOB(mask_el_len, &iptr4[mindx])) {
				sindx = indx2_src + (j * src_strd[0]);
				xptr3[rindx].re = xptr1[sindx].re;
				xptr3[rindx].im = xptr1[sindx].im;
				rindx += res_strd;
				found++;
			    }
			    indx1_msk += msk_strd[0];
			}
			indx2_msk += msk_strd[1];
			indx2_src += src_strd[1];
		    }
		    if (!vector || found == nwords) {
			result->dimension[0].extent = found;
		    } else {
			vindx = found * vec_strd;
			for ( ; found < nwords; found++) {
			    xptr3[rindx].re = xptr2[vindx].re;
			    xptr3[rindx].im = xptr2[vindx].im;
			    rindx += res_strd;
			    vindx += vec_strd;
			}
		    }
		    break;
#endif

		default :
		    _lerror (_LELVL_ABORT, FEINTDTY);
	    }
	} else {			/* rank 3-7 */

/*
 *	Ranks 3 through 7 are all handled in this last block.  It was assumed
 *	that ranks 1 and 2 would account for the majority of calls to pack,
 *	and that the remaining ranks could be done in one block.
 *
 *	The logic behind these blocks is the same as for the other ranks.
 *	The first part of the routine uses two nested loops, with the inner
 *	loop being the first dimension, and the outer loop being the product
 *	of all of the remaining dimensions.  A array of counters keeps track
 *	of the values for each of the dimensions.  Two macros are used in
 *	this block.  INCREMENT are used to calculate the values of each of
 *	the dimension counters, and to calculate the offsets into the array
 *	for each index.  FIND_INDX sums these offsets into one offset, which
 *	is used for each iteration of the inner loop.  As with the other two
 *	blocks, the second part of each section is not affected by the number
 *	of dimensions in the source matrix.
 *
 *	Calculate the product of each of the dimensions 2-n.  This is the
 *	number of times the outer loop will be executed.  Also, initialize
 *	the offset and dimension counter arrays.
 */
	    total_ext = 1;
#ifdef _UNICOS
#pragma _CRI	shortloop
#endif
	    for (i = 0; i < MAXDIM; i++) {
		curdim[i] = 0;
		msk_off[i] = 0;
		src_off[i] = 0;
	    }

#ifdef _UNICOS
#pragma _CRI	shortloop
#endif
	    for (i = 1; i < rank; i++)
		total_ext *= source->dimension[i].extent;
	    iptr4 = (_f_mask *) mptr;
	    found = 0;

	    switch (subtype) {
		case DVSUBTYPE_BIT64 :
		    uptr2 = (_f_int8 *) vptr;
		    uptr3 = (_f_int8 *) rptr;
		    rindx = 0;
		    for (i = 0; i < total_ext; i++) {
			FIND_INDX();
			uptr1 = (_f_int8 *) sptr + indx1_src;
			iptr4 = (_f_mask *) mptr + indx1_msk;
			for (j = 0; j < src_ext[0]; j++) {
			    mindx = j * msk_strd[0];
			    if (LTOB(mask_el_len, &iptr4[mindx])) {
				sindx = j * src_strd[0];
				uptr3[rindx] = uptr1[sindx];
				rindx += res_strd;
				found++;
			    }
			}
			INCREMENT();
		    }
		    if (!vector || found == nwords) {
			result->dimension[0].extent = found;
		    } else {
			vindx = found * vec_strd;
			for ( ; found < nwords; found++) {
			    uptr3[rindx] = uptr2[vindx];
			    rindx += res_strd;
			    vindx += vec_strd;
			}
		    }
		    break;

		case DVSUBTYPE_BIT32 :
		    hptr2 = (_f_int4 *) vptr;
		    hptr3 = (_f_int4 *) rptr;
		    rindx = 0;
		    for (i = 0; i < total_ext; i++) {
			FIND_INDX();
			hptr1 = (_f_int4 *) sptr + indx1_src;
			iptr4 = (_f_mask *) mptr + indx1_msk;
			for (j = 0; j < src_ext[0]; j++) {
			    mindx = j * msk_strd[0];
			    if (LTOB(mask_el_len, &iptr4[mindx])) {
				sindx = j * src_strd[0];
				hptr3[rindx] = hptr1[sindx];
				rindx += res_strd;
				found++;
			    }
			}
			INCREMENT();
		    }
		    if (!vector || found == nwords) {
			result->dimension[0].extent = found;
		    } else {
			vindx = found * vec_strd;
			for ( ; found < nwords; found++) {
			    hptr3[rindx] = hptr2[vindx];
			    rindx += res_strd;
			    vindx += vec_strd;
			}
		    }
		    break;

		case DVSUBTYPE_BIT128 :
		    dptr2 = (_f_real16 *) vptr;
		    dptr3 = (_f_real16 *) rptr;
		    rindx = 0;
		    for (i = 0; i < total_ext; i++) {
			FIND_INDX();
			dptr1 = (_f_real16 *) sptr + indx1_src;
			iptr4 = (_f_mask *) mptr + indx1_msk;
			for (j = 0; j < src_ext[0]; j++) {
			    mindx = j * msk_strd[0];
			    if (LTOB(mask_el_len, &iptr4[mindx])) {
				sindx = j * src_strd[0];
				dptr3[rindx] = dptr1[sindx];
				rindx += res_strd;
				found++;
			    }
			}
			INCREMENT();
		    }
		    if (!vector || found == nwords) {
			result->dimension[0].extent = found;
		    } else {
			vindx = found * vec_strd;
			for ( ; found < nwords; found++) {
			    dptr3[rindx] = dptr2[vindx];
			    rindx += res_strd;
			    vindx += vec_strd;
			}
		    }
		    break;

		case DVSUBTYPE_CHAR :
		    cptr2 = (char *) vptr;
		    cptr3 = (char *) rptr;
		    rindx = 0;
		    for (i = 0; i < total_ext; i++) {
			FIND_INDX();
			iptr4 = (_f_mask *) mptr + indx1_msk;
			for (j = 0; j < src_ext[0]; j++) {
			    mindx = j * msk_strd[0];
			    if (LTOB(mask_el_len, &iptr4[mindx])) {
				sindx = indx1_src + (j * src_strd[0]);
				cptr1 = (char *) cs + sindx;
				cptr3 = (char *) cr + rindx;
				(void) memcpy (cptr3, cptr1, bucketsize);
				rindx += res_strd;
				found++;
			    }
			}
			INCREMENT();
		    }
		    if (!vector || found == nwords) {
			result->dimension[0].extent = found;
		    } else {
			vindx = found * vec_strd;
			for ( ; found < nwords; found++) {
			    cptr3 = (char *) cr + rindx;
			    cptr2 = (char *) cv + vindx;
			    (void) memcpy (cptr3, cptr2, bucketsize);
			    rindx += res_strd;
			    vindx += vec_strd;
			}
		    }
		    break;

		case DVSUBTYPE_DERIVED :
		    fptr2 = (_f_int *) vptr;
		    fptr3 = (_f_int *) rptr;
		    for (i = 0; i < bucketsize; i++) {
			rindx = i;
			for (j = 0; j < rank; j++) {
			    msk_off[j] = 0;
			    src_off[j] = 0;
			    curdim[j] = 0;
			}
			for (j = 0; j < total_ext; j++) {
			    FIND_INDX();
			    fptr1 = (_f_int *) sptr + i + indx1_src;
			    iptr4 = (_f_mask *) mptr + indx1_msk;
			    for (k = 0; k < src_ext[0]; k++) {
				mindx = k * msk_strd[0];
				if (LTOB(mask_el_len, &iptr4[mindx])) {
				    sindx = k * src_strd[0];
				    fptr3[rindx] = fptr1[sindx];
				    rindx += res_strd;
				    if (i == 0)
					found++;
				}
			    }
			    INCREMENT();
			}
		    }
		    if (!vector || found == nwords) {
			result->dimension[0].extent = found;
		    } else {
			indx1_res = found * res_strd;
			indx1_vec = found * vec_strd;
			found = nwords - found;
			for (i = 0; i < bucketsize; i++) {
			    rindx = indx1_res + i;
			    vindx = indx1_vec + i;
			    for (j = 0; j < found; j++) {
				fptr3[rindx] = fptr2[vindx];
				rindx += res_strd;
				vindx += vec_strd;
			    }
			}
		    }
		    break;

#ifdef _F_COMP16
		case DVSUBTYPE_BIT256 :
		    xptr2 = (dblcmplx *) vptr;
		    xptr3 = (dblcmplx *) rptr;
		    rindx = 0;
		    for (i = 0; i < total_ext; i++) {
			FIND_INDX();
			xptr1 = (dblcmplx *) sptr + indx1_src;
			iptr4 = (_f_mask *) mptr + indx1_msk;
			for (j = 0; j < src_ext[0]; j++) {
			    mindx = j * msk_strd[0];
			    if (LTOB(mask_el_len, &iptr4[mindx])) {
				sindx = j * src_strd[0];
				xptr3[rindx].re = xptr1[sindx].re;
				xptr3[rindx].im = xptr1[sindx].im;
				rindx += res_strd;
				found++;
			    }
			}
			INCREMENT();
		    }
		    if (!vector || found == nwords) {
			result->dimension[0].extent = found;
		    } else {
			vindx = found * vec_strd;
			for ( ; found < nwords; found++) {
			    xptr3[rindx].re = xptr2[vindx].re;
			    xptr3[rindx].im = xptr2[vindx].im;
			    rindx += res_strd;
			    vindx += vec_strd;
			}
		    }
		    if (!vector || found == nwords) {
			result->dimension[0].extent = found;
		    } else {
			vindx = found * vec_strd;
			for ( ; found < nwords; found++) {
			    xptr3[rindx].re = xptr2[vindx].re;
			    xptr3[rindx].im = xptr2[vindx].im;
			    rindx += res_strd;
			    vindx += vec_strd;
			}
		    }
		    break;
#endif

		default :
		    _lerror (_LELVL_ABORT, FEINTDTY);
	    }
	}
}
