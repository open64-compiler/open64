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


#pragma ident "@(#) libfi/array/unpack.c	92.1	07/07/99 15:52:02"

#include <stddef.h>
#include <liberrno.h>
#include <cray/dopevec.h>
#include <cray/portdefs.h>
#include "arraydefs.h"

/*
 *	Determine the total offset of dimensions 2-rank for mask, field
 *	and result matrices.
 */

#define FIND_INDX()						  	\
	switch (rank) {							\
	    case 3 :							\
		indx1_msk = msk_off[0] + msk_off[1];			\
		indx1_fld = fld_off[0] + fld_off[1];			\
		indx1_res = res_off[0] + res_off[1];			\
		break;							\
	    case 4 :							\
		indx1_msk = msk_off[0] + msk_off[1] + msk_off[2];       \
		indx1_fld = fld_off[0] + fld_off[1] + fld_off[2];       \
		indx1_res = res_off[0] + res_off[1] + res_off[2];       \
		break;							\
	    case 5 :							\
		indx1_msk = msk_off[0] + msk_off[1] +			\
			    msk_off[2] + msk_off[3];			\
		indx1_fld = fld_off[0] + fld_off[1] +			\
			    fld_off[2] + fld_off[3];			\
		indx1_res = res_off[0] + res_off[1] +			\
			    res_off[2] + res_off[3];			\
		break;							\
	    case 6 :							\
		indx1_msk = msk_off[0] + msk_off[1] + msk_off[2] +      \
			    msk_off[3] + msk_off[4];			\
		indx1_fld = fld_off[0] + fld_off[1] + fld_off[2] +      \
			    fld_off[3] + fld_off[4];			\
		indx1_res = res_off[0] + res_off[1] + res_off[2] +      \
			    res_off[3] + res_off[4];			\
		break;							\
	    default :							\
		indx1_msk = msk_off[0] + msk_off[1] + msk_off[2] +      \
			    msk_off[3] + msk_off[4] + msk_off[5];       \
		indx1_fld = fld_off[0] + fld_off[1] + fld_off[2] +      \
			    fld_off[3] + fld_off[4] + fld_off[5];       \
		indx1_res = res_off[0] + res_off[1] + res_off[2] +      \
			    res_off[3] + res_off[4] + res_off[5];       \
	}


/*
 *	Determine the value for each index, as well as the offset for
 *	each dimension 2-rank for mask, field and result matrices.
 */

/*
 *	INCREMENT calculates the offset for each dimension of the mask,
 *	field and result arrays.  The sum of each of the offset arrays
 *	gives the offset from the beginning of the array for each of the
 *	arrays.
 */

#define INCREMENT()							\
	curdim[0]++;							\
	if (curdim[0] < msk_ext[1]) {					\
	    msk_off[0] = curdim[0] * msk_strd[1];			\
	    fld_off[0] = curdim[0] * fld_strd[1];			\
	    res_off[0] = curdim[0] * res_strd[1];			\
	} else {							\
	    curdim[0] = 0;						\
	    msk_off[0] = 0;						\
	    fld_off[0] = 0;						\
	    res_off[0] = 0;						\
	    curdim[1]++;						\
	    if (curdim[1] < msk_ext[2]) {				\
		msk_off[1] = curdim[1] * msk_strd[2];			\
		fld_off[1] = curdim[1] * fld_strd[2];			\
		res_off[1] = curdim[1] * res_strd[2];			\
	    } else {							\
		curdim[1] = 0;						\
		msk_off[1] = 0;						\
		fld_off[1] = 0;						\
		res_off[1] = 0;						\
		curdim[2]++;						\
		if (curdim[2] < msk_ext[3]) {				\
		    msk_off[2] = curdim[2] * msk_strd[3];		\
		    fld_off[2] = curdim[2] * fld_strd[3];		\
		    res_off[2] = curdim[2] * res_strd[3];		\
		} else {						\
		    curdim[2] = 0;					\
		    msk_off[2] = 0;					\
		    fld_off[2] = 0;					\
		    res_off[2] = 0;					\
		    curdim[3]++;					\
		    if (curdim[3] < msk_ext[4]) {			\
			msk_off[3] = curdim[3] * msk_strd[4];		\
			fld_off[3] = curdim[3] * fld_strd[4];		\
			res_off[3] = curdim[3] * res_strd[4];		\
		    } else {						\
			curdim[3] = 0;					\
			msk_off[3] = 0;					\
			fld_off[3] = 0;					\
			res_off[3] = 0;					\
			curdim[4]++;					\
			if (curdim[4] < msk_ext[5]) {			\
			    msk_off[4] = curdim[4] * msk_strd[5];	\
			    fld_off[4] = curdim[4] * fld_strd[5];	\
			    res_off[4] = curdim[4] * res_strd[5];	\
			} else {					\
			    curdim[4] = 0;				\
			    msk_off[4] = 0;				\
			    fld_off[4] = 0;				\
			    res_off[4] = 0;				\
			    curdim[5]++;				\
			    msk_off[5] = curdim[5] * msk_strd[6];       \
			    fld_off[5] = curdim[5] * fld_strd[6];       \
			    res_off[5] = curdim[5] * res_strd[6];       \
			}						\
		    }							\
		}							\
	    }								\
	}

#ifdef _UNICOS
#pragma	_CRI duplicate _UNPACK as UNPACK@
#endif
void
_UNPACK (	DopeVectorType * result,
		DopeVectorType * vector,
		DopeVectorType * mask,
		DopeVectorType * field)

{
	char	*cf;		/* char ptr to field array	*/
	char	*cr;		/* char ptr to result array	*/
	char	*cv;		/* char ptr to vector array	*/
	char		* restrict cptr1;	/* char		*/
	char		* restrict cptr2;	/* char		*/
	char		* restrict cptr3;	/* char		*/
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
	_f_mask		* restrict iptr4;	/* int		*/
	void		* restrict fptr;	/* field	*/
	void		* restrict rptr;	/* result	*/
	void		* restrict vptr;	/* vector	*/
	void		* restrict mptr;	/* mask		*/
	_f_int	bucketsize;	/* size of element		*/
	long	nbytes;		/* number of bytes		*/
	long	nwords;		/* number of words		*/
	long	curdim[MAXDIM];	/* current indices		*/
	_f_int	bytealligned;	/* byte aligned flag		*/
	long	findx;		/* field index			*/
	long	rindx;		/* result index			*/
	long	mindx;		/* mask index			*/
	long	vindx;		/* vector index			*/
	_f_int	type;		/* data type			*/
	_f_int	subtype;	/* sub-type			*/
	_f_int	arithmetic;	/* arithmetic data type		*/
	_f_int	rank;		/* rank of result matrix	*/
	long	i, j, k;	/* index variables		*/
	long	vec_strd;	/* stride of vector		*/
	long	fld_ext[MAXDIM];  /* extents for field		*/
	long	fld_strd[MAXDIM]; /* element stride for field	*/
	long	fld_incr[MAXDIM]; /* incr for each index	*/
	long	fld_off[MAXDIM];  /* incr for each index	*/
	long	msk_ext[MAXDIM];  /* extents for field		*/
	long	msk_strd[MAXDIM]; /* element stride for field	*/
	long	msk_incr[MAXDIM]; /* incr for each index	*/
	long	msk_off[MAXDIM];  /* incr for each index	*/
	long	res_ext[MAXDIM];  /* extents for field		*/
	long	res_strd[MAXDIM]; /* element stride for field	*/
	long	res_incr[MAXDIM]; /* incr for each index	*/
	long	res_off[MAXDIM];  /* incr for each index	*/
	long	fld_cum_decr;	/* field cumulative decrement	*/
	long	msk_cum_decr;	/* mask cumulative decrement	*/
	long	res_cum_decr;	/* result cumulative decrement	*/
	long	indx1_fld;	/* index for dim 1 of field	*/
	long	indx2_fld;	/* index for dim 2 of field	*/
	long	fld2_off;	/* offset for field dim 2	*/
	long	indx1_res;	/* index for dim 1 of result	*/
	long	indx2_res;	/* index for dim 2 of result	*/
	long	res2_off;	/* offset for result dim 2	*/
	long	indx1_msk;	/* index for dim 1 of mask	*/
	long	indx2_msk;	/* index for dim 2 of mask	*/
	long	msk2_off;	/* offset for mask dim 2	*/
	long	tot_ext;	/* total mask extent counter	*/
	long	vec_ext;	/* total vector extent counter	*/
	long	mask_el_len;
	_f_int	early_exit;	/* early exit flag		*/

/*	Set type and rank global variables	*/

	type = field->type_lens.type;
	rank = mask->n_dim;
	mask_el_len = mask->base_addr.a.el_len;

/*
 *	If any result array extents are zero (zero-sized array) and
 *	the result dope vector is associated, exit without any load/store
 *	since the result array (dope vector contents) cannot be changed.
 *	If an extent from a mask array is zero, exit without doing any
 *	load/store since the result must have the same shape as MASK.
 *	If the vector source array is zero sized, an early exit is not
 *	possible unless mask is zero-sized. If mask is all false but not
 *	zero sized, field is used to fill the result array.  The cases:
 *	1. RESULT array is associated and zero sized (cannot change
 *	   assoc array): - early exit with no change to RESULT array.
 *	2. MASK is scalar (MASK must be array - diagnosed at compilation.
 *	3. MASK is nonzero-sized array and FIELD does not conform to MASK
 *	   - diagnosed with -RC option only at execution possibly.
 *	4. MASK array is zero sized (RESULT array must be same shape as
 *	   MASK): - early exit with RESULT same shape as MASK.
 *	5. MASK nonzero-sized array and any size VECTOR array.  VECTOR
 *	   array must have as many elements as there are TRUE elements
 *	   in MASK.  FIELD values are used to fill elements of RESULT
 *	   that correspond to FALSE elements of MASK.
 *		- WRITE elements of VECTOR to RESULT.  
 *		- WRITE elements of FIELD to RESULT.
 *	ERROR if VECTOR does not have enough elements for TRUE elements
 *	elements of MASK.  A scalar FIELD always has enough elements.
 */

	early_exit = 0;
	if (result->assoc) {
#ifdef _UNICOS
#pragma	_CRI	shortloop
#endif
	    for (i = 0; i < rank; i++) {
		if (result->dimension[i].extent == 0)
		    early_exit = 1;
	    }
	}
#ifdef _UNICOS
#pragma	_CRI	shortloop
#endif
	for (i = 0; i < rank; i++) {
	    if (mask->dimension[i].extent == 0)
		early_exit = 1;
	}

/*
 *	Initialize every array element to 0.
 */

#ifdef _UNICOS
#pragma	_CRI	shortloop
#endif
	for (i = 0; i < MAXDIM; i++) {
	    curdim[i] = 0;
	    fld_ext[i] = 0;
	    fld_strd[i] = 0;
	    fld_incr[i] = 0;
	    fld_off[i] = 0;
	    msk_ext[i] = 0;
	    msk_strd[i] = 0;
	    msk_incr[i] = 0;
	    msk_off[i] = 0;
	    res_ext[i] = 0;
	    res_strd[i] = 0;
	    res_incr[i] = 0;
	    res_off[i] = 0;
	}

/*	Size calculation is based on variable type	*/

	switch (type) {
	    case DVTYPE_ASCII :
		bytealligned = 1;
		bucketsize = _fcdlen (field->base_addr.charptr);  /* bytes */
		subtype = DVSUBTYPE_CHAR;
		arithmetic = 0;
		break;
	    case DVTYPE_DERIVEDBYTE :
		bytealligned = 1;
		bucketsize = field->base_addr.a.el_len / BITS_PER_BYTE;
		subtype = DVSUBTYPE_CHAR;
		arithmetic = 0;
		break;
	    case DVTYPE_DERIVEDWORD :
		bytealligned = 0;
		bucketsize = field->base_addr.a.el_len / BITS_PER_WORD;
		subtype = DVSUBTYPE_DERIVED;
		arithmetic = 0;
		break;
	    default :
		bytealligned = 0;
		bucketsize = field->type_lens.int_len / BITS_PER_WORD;
		if (field->type_lens.int_len == 64) {
		    subtype = DVSUBTYPE_BIT64;
		} else if (field->type_lens.int_len == 32) {
		    subtype = DVSUBTYPE_BIT32;
		    bucketsize = 1;
		} else if (field->type_lens.int_len == 256) {
		    subtype = DVSUBTYPE_BIT256;
		} else {
		    subtype = DVSUBTYPE_BIT128;
		}
		arithmetic = 1;
	}

/*	Set up dope vector for result array	*/

	if (!result->assoc) {
	    result->base_addr.a.ptr = (void *) NULL;
	    result->orig_base = 0;
	    result->orig_size = 0;
#ifdef _UNICOS
#pragma _CRI	shortloop
#endif
	    for (i = 0, tot_ext = bucketsize; i < rank; i++) {
		result->dimension[i].extent = mask->dimension[i].extent;
		result->dimension[i].low_bound = 1;
		result->dimension[i].stride_mult = tot_ext;
		tot_ext *= result->dimension[i].extent;
	    }

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
	    for (i = 0; i < rank; i++)
		nbytes *= mask->dimension[i].extent;
	    nwords = nbytes / BYTES_PER_WORD;
	    if (nbytes > 0) {
		result->base_addr.a.ptr = (void *) malloc (nbytes);
		if (result->base_addr.a.ptr == NULL)
		    _lerror (_LELVL_ABORT, FENOMEMY);
	    }

	    result->assoc = 1;
	    result->base_addr.a.el_len = field->base_addr.a.el_len;
	    if (type == DVTYPE_ASCII) {
		cr = (char *) result->base_addr.a.ptr;
		result->base_addr.charptr = _cptofcd (cr, bucketsize);
	    }
	    result->orig_base = (void *) result->base_addr.a.ptr;
	    result->orig_size = nbytes * BITS_PER_BYTE;
	}

/*	If one of our early exit conditions is met, exit now	*/

	if (early_exit)
	    return;

/*	Initialize scalar pointers to all of the argument data areas	*/

	if (!bytealligned) {
	    fptr = (void *) field->base_addr.a.ptr;
	    rptr = (void *) result->base_addr.a.ptr;
	    vptr = (void *) vector->base_addr.a.ptr;
	} else {
	    if (type == DVTYPE_ASCII) {
		cf = _fcdtocp (field->base_addr.charptr);
		cv = _fcdtocp ( vector->base_addr.charptr);
		cr = _fcdtocp (result->base_addr.charptr);
	    } else {
		cf = (char *) field->base_addr.a.ptr;
		cv = (char *) vector->base_addr.a.ptr;
		cr = (char *) result->base_addr.a.ptr;
	    }
	}
	if (mask)
	    mptr = (void *) mask->base_addr.a.ptr;

/*	Initialize 'shortcut variables used for index calculation	*/

	vec_ext = vector->dimension[0].extent;
	if (bucketsize > 1 && arithmetic) {
	    vec_strd = vector->dimension[0].stride_mult / bucketsize;
	} else {
	    vec_strd = vector->dimension[0].stride_mult;
	}

#ifdef _UNICOS
#pragma _CRI	shortloop
#endif
	for (i = 0; i < rank; i++) {
	    msk_ext[i] = mask->dimension[i].extent;
	    msk_strd[i] = mask->dimension[i].stride_mult;
#ifdef	_CRAYMPP
	    if (mask_el_len == 64 && sizeof(iptr4[0]) == 4)
		msk_strd[i] <<= 1;
#endif
	    res_ext[i] = result->dimension[i].extent;
	    if (bucketsize > 1 && arithmetic) {
		res_strd[i] = result->dimension[i].stride_mult / bucketsize;
	    } else {
		res_strd[i] = result->dimension[i].stride_mult;
	    }
	}

	if (field->n_dim > 0) {
#ifdef _UNICOS
#pragma _CRI	shortloop
#endif
	    for (i = 0; i < rank; i++) {
		fld_ext[i] = field->dimension[i].extent;
		if (bucketsize > 1 && arithmetic) {
		    fld_strd[i] = field->dimension[i].stride_mult / bucketsize;
		} else {
		    fld_strd[i] = field->dimension[i].stride_mult;
		}
	    }
	} else {
#ifdef _UNICOS
#pragma _CRI	shortloop
#endif
	    for (i = 0; i < rank; i++) {
		fld_ext[i] = 0;
		fld_strd[i] = 0;
	    }
	}

/*	Calculate total number of elements to move	*/

	tot_ext = 1;
#ifdef _UNICOS
#pragma _CRI	novector
#endif
	for (i = 0; i < rank; i++)
	    tot_ext *= msk_ext[i];

/*
 *      The program is divided up into three blocks.  The first block deals
 *      with arrays of rank 1.  Inside each block, the data types are broken
 *      up into groups based on container size.  Integer, real, and logical
 *      types are all one word, and the actual value is not used, so they
 *      are all grouped together and treated as long.  The same is true
 *      for double and complex, as well as ascii and derivedbyte.
 *
 *	For each group, the mask array is checked for a true value.  When
 *	one is encountered, the next value from the vector array is put into
 *	the result array.  If the mask is false, the corresponding element of
 *	the field array is put into the result.
 */

	if (rank == 1) {
	    iptr4 = (_f_mask *) mptr;
	    switch (subtype) {
		case DVSUBTYPE_BIT64 :
		    uptr1 = (_f_int8 *) vptr;
		    uptr2 = (_f_int8 *) fptr;
		    uptr3 = (_f_int8 *) rptr;
		    rindx = 0;
		    mindx = 0;
		    vindx = 0;
		    findx = 0;
		    for (i = 0; i < tot_ext; i++) {
			if (LTOB(mask_el_len, &iptr4[mindx])) {
			    if (vec_ext-- > 0) {
			    	uptr3[rindx] = uptr1[vindx];
			    	vindx += vec_strd;	/* use vector	*/
			    } else {
				_lerror (_LELVL_ABORT, FEVECUNP);
			    }
			} else {
			    findx = i * fld_strd[0];
			    uptr3[rindx] = uptr2[findx]; /* use field	*/
			}
			rindx += res_strd[0];
			mindx += msk_strd[0];
		    }
		    break;

		case DVSUBTYPE_BIT32 :
		    hptr1 = (_f_int4 *) vptr;
		    hptr2 = (_f_int4 *) fptr;
		    hptr3 = (_f_int4 *) rptr;
		    rindx = 0;
		    mindx = 0;
		    vindx = 0;
		    findx = 0;
		    for (i = 0; i < tot_ext; i++) {
			if (LTOB(mask_el_len, &iptr4[mindx])) {
			    if (vec_ext-- > 0) {
			    	hptr3[rindx] = hptr1[vindx];
			    	vindx += vec_strd;
			    } else {
				_lerror (_LELVL_ABORT, FEVECUNP);
			    }
			} else {
			    findx = i * fld_strd[0];
			    hptr3[rindx] = hptr2[findx];
			}
			rindx += res_strd[0];
			mindx += msk_strd[0];
		    }
		    break;

		case DVSUBTYPE_BIT128 :
		    dptr1 = (_f_real16 *) vptr;
		    dptr2 = (_f_real16 *) fptr;
		    dptr3 = (_f_real16 *) rptr;
		    rindx = 0;
		    mindx = 0;
		    vindx = 0;
		    findx = 0;
		    for (i = 0; i < tot_ext; i++) {
			if (LTOB(mask_el_len, &iptr4[mindx])) {
			    if (vec_ext-- > 0) {
			    	dptr3[rindx] = dptr1[vindx];
			    	vindx += vec_strd;
			    } else {
				_lerror (_LELVL_ABORT, FEVECUNP);
			    }
			} else {
			    findx = i * fld_strd[0];
			    dptr3[rindx] = dptr2[findx];
			}
			rindx += res_strd[0];
			mindx += msk_strd[0];
		    }
		    break;

		case DVSUBTYPE_CHAR :
		    rindx = 0;
		    mindx = 0;
		    vindx = 0;
		    findx = 0;
		    for (i = 0; i < tot_ext; i++) {
			cptr3 = (char *) cr + rindx;
			if (LTOB(mask_el_len, &iptr4[mindx])) {
			    if (vec_ext-- > 0) {
			    	cptr1 = (char *) cv + vindx;
			    	(void) memcpy (cptr3, cptr1, bucketsize);
			    	vindx += vec_strd;
			    } else {
				_lerror (_LELVL_ABORT, FEVECUNP);
			    }
			} else {
			    findx = i * fld_strd[0];
			    cptr2 = (char *) cf + findx;
			    (void) memcpy (cptr3, cptr2, bucketsize);
			}
			rindx += res_strd[0];
			mindx += msk_strd[0];
		    }
		    break;

		case DVSUBTYPE_DERIVED :
		    fptr1 = (_f_int *) vptr;
		    fptr2 = (_f_int *) fptr;
		    fptr3 = (_f_int *) rptr;
/*
 *	For this type, the assumption was made that the extent size would
 *	more often than not be larger than the word size of the structure.
 *	Therefore, an outer loop was added for the container size.  This
 *	will make the extent the inner loop, which should help optimization.
 */
		    for (i = 0; i < bucketsize; i++) {
			rindx = i;
			mindx = 0;
			vindx = i;
			vec_ext = vector->dimension[0].extent;
			for (j = 0; j < tot_ext; j++) {
			    if (LTOB(mask_el_len, &iptr4[mindx])) {
			    	if (vec_ext-- > 0) {
					fptr3[rindx] = fptr1[vindx];
					vindx += vec_strd;
			   	} else {
				    _lerror (_LELVL_ABORT, FEVECUNP);
			   	}
			    } else {
				findx = (j * fld_strd[0]) + i;
				fptr3[rindx] = fptr2[findx];
			    }
			    rindx += res_strd[0];
			    mindx += msk_strd[0];
			}
		    }
		    break;

#ifdef _F_COMP16
		case DVSUBTYPE_BIT256 :
		    xptr1 = (dblcmplx *) vptr;
		    xptr2 = (dblcmplx *) fptr;
		    xptr3 = (dblcmplx *) rptr;
		    rindx = 0;
		    mindx = 0;
		    vindx = 0;
		    findx = 0;
		    for (i = 0; i < tot_ext; i++) {
			if (LTOB(mask_el_len, &iptr4[mindx])) {
			    if (vec_ext-- > 0) {
			    	xptr3[rindx].re = xptr1[vindx].re;
			    	xptr3[rindx].im = xptr1[vindx].im;
			    	vindx += vec_strd;
			    } else {
				_lerror (_LELVL_ABORT, FEVECUNP);
			    }
			} else {
			    findx = i * fld_strd[0];
			    xptr3[rindx].re = xptr2[findx].re;
			    xptr3[rindx].im = xptr2[findx].im;
			}
			rindx += res_strd[0];
			mindx += msk_strd[0];
		    }
		    break;
#endif

		default :
		    _lerror (_LELVL_ABORT, FEINTDTY);
	    }

	} else if (rank == 2) {

/*
 *	Rank two matrices are handled in a manner similar to rank one arrays,
 *	but differ in that they are nested with the first index being the
 *	inner loop.  Scalar variables are set to the offsets of the second
 *	dimension, and these are used for each iteration of the inner loop.
 */

	    iptr4 = (_f_mask *) mptr;
	    switch (subtype) {
		case DVSUBTYPE_BIT64 :
		    uptr1 = (_f_int8 *) vptr;
		    uptr2 = (_f_int8 *) fptr;
		    uptr3 = (_f_int8 *) rptr;
		    vindx = 0;
		    for (i = 0; i < msk_ext[1]; i++) {
			indx2_msk = i * msk_strd[1];
			indx2_fld = i * fld_strd[1];
			indx2_res = i * res_strd[1];
			indx1_msk = 0;
			indx1_res = 0;
			for (j = 0; j < msk_ext[0]; j++) {
			    mindx = indx1_msk + indx2_msk;
			    rindx = indx1_res + indx2_res;
			    if (LTOB(mask_el_len, &iptr4[mindx])) {
			    	if (vec_ext-- > 0) {
					uptr3[rindx] = uptr1[vindx];
					vindx += vec_strd;
			    	} else {
				    _lerror (_LELVL_ABORT, FEVECUNP);
			    	}
			    } else {
				findx = indx2_fld + (j * fld_strd[0]);
				uptr3[rindx] = uptr2[findx];
			    }
			    indx1_msk += msk_strd[0];
			    indx1_res += res_strd[0];
			}
		    }
		    break;

		case DVSUBTYPE_BIT32 :
		    hptr1 = (_f_int4 *) vptr;
		    hptr2 = (_f_int4 *) fptr;
		    hptr3 = (_f_int4 *) rptr;
		    vindx = 0;
		    for (i = 0; i < msk_ext[1]; i++) {
			indx2_msk = i * msk_strd[1];
			indx2_fld = i * fld_strd[1];
			indx2_res = i * res_strd[1];
			indx1_msk = 0;
			indx1_res = 0;
			for (j = 0; j < msk_ext[0]; j++) {
			    mindx = indx1_msk + indx2_msk;
			    rindx = indx1_res + indx2_res;
			    if (LTOB(mask_el_len, &iptr4[mindx])) {
			    	if (vec_ext-- > 0) {
					hptr3[rindx] = hptr1[vindx];
					vindx += vec_strd;
			    	} else {
				    _lerror (_LELVL_ABORT, FEVECUNP);
			    	}
			    } else {
				findx = indx2_fld + (j * fld_strd[0]);
				hptr3[rindx] = hptr2[findx];
			    }
			    indx1_msk += msk_strd[0];
			    indx1_res += res_strd[0];
			}
		    }
		    break;

		case DVSUBTYPE_BIT128 :
		    dptr1 = (_f_real16 *) vptr;
		    dptr2 = (_f_real16 *) fptr;
		    dptr3 = (_f_real16 *) rptr;
		    vindx = 0;
		    for (i = 0; i < msk_ext[1]; i++) {
			indx2_msk = i * msk_strd[1];
			indx2_fld = i * fld_strd[1];
			indx2_res = i * res_strd[1];
			indx1_msk = 0;
			indx1_res = 0;
			for (j = 0; j < msk_ext[0]; j++) {
			    mindx = indx1_msk + indx2_msk;
			    rindx = indx1_res + indx2_res;
			    if (LTOB(mask_el_len, &iptr4[mindx])) {
			    	if (vec_ext-- > 0) {
					dptr3[rindx] = dptr1[vindx];
					vindx += vec_strd;
			    	} else {
				    _lerror (_LELVL_ABORT, FEVECUNP);
			    	}
			    } else {
				findx = indx2_fld + (j * fld_strd[0]);
				dptr3[rindx] = dptr2[findx];
			    }
			    indx1_msk += msk_strd[0];
			    indx1_res += res_strd[0];
			}
		    }
		    break;

		case DVSUBTYPE_CHAR :
		    vindx = 0;
		    for (i = 0; i < msk_ext[1]; i++) {
			indx2_msk = i * msk_strd[1];
			indx2_fld = i * fld_strd[1];
			indx2_res = i * res_strd[1];
			indx1_msk = 0;
			indx1_res = 0;
			for (j = 0; j < msk_ext[0]; j++) {
			    mindx = indx1_msk + indx2_msk;
			    rindx = indx1_res + indx2_res;
			    if (LTOB(mask_el_len, &iptr4[mindx])) {
			    	if (vec_ext-- > 0) {
					cptr1 = (char *) cv + vindx;
					cptr3 = (char *) cr + rindx;
					(void) memcpy (cptr3, cptr1,
						bucketsize);
					vindx += vec_strd;
			    	} else {
				    _lerror (_LELVL_ABORT, FEVECUNP);
			    	}
			    } else {
				findx = indx2_fld + (j * fld_strd[0]);
				cptr2 = (char *) cf + findx;
				cptr3 = (char *) cr + rindx;
				(void) memcpy (cptr3, cptr2, bucketsize);
			    }
			    indx1_msk += msk_strd[0];
			    indx1_res += res_strd[0];
			}
		    }
		    break;

		case DVSUBTYPE_DERIVED :
		    fptr1 = (_f_int *) vptr;
		    fptr2 = (_f_int *) fptr;
		    fptr3 = (_f_int *) rptr;
		    for (i = 0; i < bucketsize; i++) {
			vec_ext = vector->dimension[0].extent;
			vindx = i;
			for (j = 0; j < msk_ext[1]; j++) {
			    indx1_msk = 0;
			    indx1_res = i;
			    indx2_msk = j * msk_strd[1];
			    indx2_fld = j * fld_strd[1] + i;
			    indx2_res = j * res_strd[1];
			    for (k = 0; k < msk_ext[0]; k++) {
				mindx = indx1_msk + indx2_msk;
				rindx = indx1_res + indx2_res;
				if (LTOB(mask_el_len, &iptr4[mindx])) {
			    	    if (vec_ext-- > 0) {
					fptr3[rindx] = fptr1[vindx];
					vindx += vec_strd;
				    } else {
					_lerror (_LELVL_ABORT, FEVECUNP);
			    	    }
				} else {
				    findx = indx2_fld + (k * fld_strd[0]);
				    fptr3[rindx] = fptr2[findx];
				}
				indx1_msk += msk_strd[0];
				indx1_res += res_strd[0];
			    }
			}
		    }
		    break;

#ifdef _F_COMP16
		case DVSUBTYPE_BIT256 :
		    xptr1 = (dblcmplx *) vptr;
		    xptr2 = (dblcmplx *) fptr;
		    xptr3 = (dblcmplx *) rptr;
		    vindx = 0;
		    for (i = 0; i < msk_ext[1]; i++) {
			indx2_msk = i * msk_strd[1];
			indx2_fld = i * fld_strd[1];
			indx2_res = i * res_strd[1];
			indx1_msk = 0;
			indx1_res = 0;
			for (j = 0; j < msk_ext[0]; j++) {
			    mindx = indx1_msk + indx2_msk;
			    rindx = indx1_res + indx2_res;
			    if (LTOB(mask_el_len, &iptr4[mindx])) {
			    	if (vec_ext-- > 0) {
				    xptr3[rindx].re = xptr1[vindx].re;
				    xptr3[rindx].im = xptr1[vindx].im;
				    vindx += vec_strd;
			    	} else {
				    _lerror (_LELVL_ABORT, FEVECUNP);
			    	}
			    } else {
				findx = indx2_fld + (j * fld_strd[0]);
				xptr3[rindx].re = xptr2[findx].re;
				xptr3[rindx].im = xptr2[findx].im;
			    }
			    indx1_msk += msk_strd[0];
			    indx1_res += res_strd[0];
			}
		    }
		    break;
#endif

		default :
		    _lerror (_LELVL_ABORT, FEINTDTY);
	    }
	} else {

/*
 *	Ranks 3-7 are all handled in this section.  All of them are done as
 *	a double nested loop, with the first dimension being the inner loop,
 *	and the outer loop being the product of the remaining extents.
 *
 *	Two macros are used in this section.  INCREMENT determines the values
 *	for each of the outer dimensions, as well as the offset for each
 *	dimension.  FIND_INDX calculates the sum total of all of these offsets.
 *
 *	Calculate the product of the extents which will be used as the loop
 *	terminator.  Also initialize the offset variables.
 */

#ifdef _UNICOS
#pragma _CRI	shortloop
#endif
	    for (i = 1, tot_ext = 1; i < rank; i++) {
		j = i - 1;
		tot_ext *= mask->dimension[i].extent;
		curdim[j] = 0;
		msk_off[j] = 0;
		fld_off[j] = 0;
		res_off[j] = 0;
	    }

	    switch (subtype) {
		case DVSUBTYPE_BIT64 :
		    uptr1 = (_f_int8 *) vptr;
		    vindx = 0;
		    for (i = 0; i < tot_ext; i++) {
			FIND_INDX();
			uptr2 = (_f_int8 *) fptr + indx1_fld;
			uptr3 = (_f_int8 *) rptr + indx1_res;
			iptr4 = (_f_mask *) mptr + indx1_msk;
			for (j = 0; j < msk_ext[0]; j++) {
			    mindx = j * msk_strd[0];
			    if (LTOB(mask_el_len, &iptr4[mindx])) {
			    	if (vec_ext-- > 0) {
				    rindx = j * res_strd[0];
				    uptr3[rindx] = uptr1[vindx];
				    vindx += vec_strd;
			    	} else {
				    _lerror (_LELVL_ABORT, FEVECUNP);
			    	}
			    } else {
				findx = j * fld_strd[0];
				rindx = j * res_strd[0];
				uptr3[rindx] = uptr2[findx];
			    }
			}
			INCREMENT();
		    }
		    break;

		case DVSUBTYPE_BIT32 :
		    hptr1 = (_f_int4 *) vptr;
		    vindx = 0;
		    for (i = 0; i < tot_ext; i++) {
			FIND_INDX();
			hptr2 = (_f_int4 *) fptr + indx1_fld;
			hptr3 = (_f_int4 *) rptr + indx1_res;
			iptr4 = (_f_mask *) mptr + indx1_msk;
			for (j = 0; j < msk_ext[0]; j++) {
			    mindx = j * msk_strd[0];
			    if (LTOB(mask_el_len, &iptr4[mindx])) {
			    	if (vec_ext-- > 0) {
				    rindx = j * res_strd[0];
				    hptr3[rindx] = hptr1[vindx];
				    vindx += vec_strd;
			    	} else {
				    _lerror (_LELVL_ABORT, FEVECUNP);
			    	}
			    } else {
				findx = j * fld_strd[0];
				rindx = j * res_strd[0];
				hptr3[rindx] = hptr2[findx];
			    }
			}
			INCREMENT();
		    }
		    break;

		case DVSUBTYPE_BIT128 :
		    dptr1 = (_f_real16 *) vptr;
		    vindx = 0;
		    for (i = 0; i < tot_ext; i++) {
			FIND_INDX();
			dptr2 = (_f_real16 *) fptr + indx1_fld;
			dptr3 = (_f_real16 *) rptr + indx1_res;
			iptr4 = (_f_mask *) mptr + indx1_msk;
			for (j = 0; j < msk_ext[0]; j++) {
			    mindx = j * msk_strd[0];
			    if (LTOB(mask_el_len, &iptr4[mindx])) {
			    	if (vec_ext-- > 0) {
				    rindx = j * res_strd[0];
				    dptr3[rindx] = dptr1[vindx];
				    vindx += vec_strd;
			    	} else {
				    _lerror (_LELVL_ABORT, FEVECUNP);
			    	}
			    } else {
				findx = j * fld_strd[0];
				rindx = j * res_strd[0];
				dptr3[rindx] = dptr2[findx];
			    }
			}
			INCREMENT();
		    }
		    break;

		case DVSUBTYPE_CHAR :
		    vindx = 0;
		    for (i = 0; i < tot_ext; i++) {
			FIND_INDX();
			iptr4 = (_f_mask *) mptr + indx1_msk;
			for (j = 0; j < msk_ext[0]; j++) {
			    mindx = j * msk_strd[0];
			    rindx = j * res_strd[0];
			    cptr3 = (char *) cr + indx1_res + rindx;
			    if (LTOB(mask_el_len, &iptr4[mindx])) {
			    	if (vec_ext-- > 0) {
				    cptr1 = (char *) cv + vindx;
				    (void) memcpy (cptr3, cptr1,
				    	bucketsize);
				    vindx += vec_strd;
			    	} else {
				    _lerror (_LELVL_ABORT, FEVECUNP);
			    	}
			    } else {
				findx = j * fld_strd[0];
				cptr2 = (char *) cf + indx1_fld + findx;
				(void) memcpy (cptr3, cptr2, bucketsize);
			    }
			}
			INCREMENT();
		    }
		    break;

		case DVSUBTYPE_DERIVED :
		    fptr1 = (_f_int *) vptr;
		    for (i = 0; i < bucketsize; i++) {
			vec_ext = vector->dimension[0].extent;
			vindx = i;
			for (j = 0; j < MAXDIM; j++) {
			    curdim[j] = 0;
			    res_off[j] = 0;
			    msk_off[j] = 0;
			    fld_off[j] = 0;
			}
			for (j = 0; j < tot_ext; j++) {
			    FIND_INDX();
			    fptr2 = (_f_int *) fptr + i + indx1_fld;
			    fptr3 = (_f_int *) rptr + i + indx1_res;
			    iptr4 = (_f_mask *) mptr + indx1_msk;
			    for (k = 0; k < msk_ext[0]; k++) {
				mindx = k * msk_strd[0];
				if (LTOB(mask_el_len, &iptr4[mindx])) {
			    	    if (vec_ext-- > 0) {
					rindx = k * res_strd[0];
					fptr3[rindx] = fptr1[vindx];
					vindx += vec_strd;
			    	    } else {
					_lerror (_LELVL_ABORT, FEVECUNP);
			    	    }
				} else {
				    findx = k * fld_strd[0];
			    	    rindx = k * res_strd[0];
				    fptr3[rindx] = fptr2[findx];
				}
			    }
			    INCREMENT();
		        }
		    }
	 	    break;


#ifdef _F_COMP16
		case DVSUBTYPE_BIT256 :
                    xptr1 = (dblcmplx *) vptr;
                    vindx = 0;
                    for (i = 0; i < tot_ext; i++) {
                        FIND_INDX();
                        xptr2 = (dblcmplx *) fptr + indx1_fld;
                        xptr3 = (dblcmplx *) rptr + indx1_res;
                        iptr4 = (_f_mask *) mptr + indx1_msk;
                        for (j = 0; j < msk_ext[0]; j++) {
                            mindx = j * msk_strd[0];
			    if (LTOB(mask_el_len, &iptr4[mindx])) {
			    	if (vec_ext-- > 0) {
				    rindx = j * res_strd[0];
				    xptr3[rindx].re = xptr1[vindx].re;
				    xptr3[rindx].im = xptr1[vindx].im;
				    vindx += vec_strd;
			    	} else {
				    _lerror (_LELVL_ABORT, FEVECUNP);
			    	}
                            } else {
                                findx = j * fld_strd[0];
                        	rindx = j * res_strd[0];
                                xptr3[rindx].re = xptr2[findx].re;
                                xptr3[rindx].im = xptr2[findx].im;
                            }
                        }
                        INCREMENT();
                    }
		    break;
#endif

		default :
		    _lerror (_LELVL_ABORT, FEINTDTY);
		}
	}
}
