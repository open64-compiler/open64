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


static const char USMID[] = "@(#) libfi/array/spread.c	92.0	10/08/98 14:37:14";

#include <stddef.h>
#include <liberrno.h>
#include <cray/dopevec.h>
#include <cray/portdefs.h>
#include "arraydefs.h"

/*
 *	Replicates an array by adding a dimension.  Broadcasts several
 *	copies of SOURCE along a specified dimension and thus forms an
 *	array of rank one greater.
 *
 *	Calculate the offsets for each index of the source and result
 *	matrices.  The index values are maintained in an array named
 *	curdim.  The total offset for the source array is stored in
 *	variable sindx, and the offset for the non-added dimension
 *	indices is stored in res_tmp2;
 */

#define INCREMENT()							\
	curdim[0]++;							\
	if (curdim[0] < src_ext[0]) {					\
	    src_indx[0] += src_strd[0];					\
	    sindx += src_strd[0];					\
	    res_indx[0] += res_strd[0];					\
	    res_tmp2 += res_strd[0];					\
	} else {							\
	    sindx -= src_indx[0];					\
	    res_tmp2 -= res_indx[0];					\
	    curdim[0] = 0;						\
	    src_indx[0] = 0;						\
	    res_indx[0] = 0;						\
	    curdim[1]++;						\
	    if (curdim[1] < src_ext[1]) {				\
		src_indx[1] += src_strd[1];				\
		sindx += src_strd[1];					\
		res_indx[1] += res_strd[1];				\
		res_tmp2 += res_strd[1];				\
	    } else {							\
		sindx -= src_indx[1];					\
		res_tmp2 -= res_indx[1];				\
		curdim[1] = 0;						\
		src_indx[1] = 0;					\
		res_indx[1] = 0;					\
		curdim[2]++;						\
		if (curdim[2] < src_ext[2]) {				\
		    src_indx[2] += src_strd[2];				\
		    sindx += src_strd[2];				\
		    res_indx[2] += res_strd[2];				\
		    res_tmp2 += res_strd[2];				\
		} else {						\
		    sindx -= src_indx[2];				\
		    res_tmp2 -= res_indx[2];				\
		    curdim[2] = 0;					\
		    src_indx[2] = 0;					\
		    res_indx[2] = 0;					\
		    curdim[3]++;					\
		    if (curdim[3] < src_ext[3]) {			\
			src_indx[3] += src_strd[3];			\
			sindx += src_strd[3];				\
			res_indx[3] += res_strd[3];			\
			res_tmp2 += res_strd[3];			\
		    } else {						\
			sindx -= src_indx[3];				\
			res_tmp2 -= res_indx[3];			\
			curdim[3] = 0;					\
			src_indx[3] = 0;				\
			res_indx[3] = 0;				\
			curdim[4]++;					\
			if (curdim[4] < src_ext[4]) {			\
			    src_indx[4] += src_strd[4];			\
			    sindx += src_strd[4];			\
			    res_indx[4] += res_strd[4];			\
			    res_tmp2 += res_strd[4];			\
			} else {					\
			    sindx -= src_indx[4];			\
			    res_tmp2 -= res_indx[4];			\
			    curdim[4] = 0;				\
			    src_indx[4] = 0;				\
			    res_indx[4] = 0;				\
			    curdim[5]++;				\
			    src_indx[5] += src_strd[5];			\
			    sindx += src_strd[5];			\
			    res_indx[5] += res_strd[5];			\
			    res_tmp2 += res_strd[5];			\
			}						\
		    }							\
		}							\
	    }								\
	}

#ifdef _UNICOS
#pragma	_CRI duplicate _SPREAD as SPREAD@
#endif
void
_SPREAD (	DopeVectorType * result,
		DopeVectorType * source,
		_f_int		*dimp,
		_f_int		*ncopiesp)
{
	char	*cs;		/* char ptr to source array	*/
	char	*cr;		/* char ptr to result array	*/
	char		* restrict cptr1;	/* char		*/
	char		* restrict cptr2;	/* char		*/
	_f_int8		* restrict uptr1;	/* full word	*/
	_f_int8		* restrict uptr2;	/* full word	*/
	_f_int		* restrict fptr1;	/* full word	*/
	_f_int		* restrict fptr2;	/* full word	*/
	_f_real16	* restrict dptr1;	/* double word	*/
	_f_real16	* restrict dptr2;	/* double word	*/
#ifdef _F_COMP16
	dblcmplx	* restrict xptr1;	/* quad word	*/
	dblcmplx	* restrict xptr2;	/* quad word	*/
#endif
	_f_int4		* restrict hptr1;	/* half word	*/
	_f_int4		* restrict hptr2;	/* half word	*/
	void		* restrict sptr;	/* ptr to src	*/
	void		* restrict rptr;	/* ptr to res	*/
	_f_int	dim;		/* dimension value		*/
	long	ncopies;	/* ncopies value		*/
	_f_int	rank;		/* rank of source matrix	*/
	_f_int	type;		/* type of source matrix	*/
	_f_int	subtype;	/* sub-type			*/
	_f_int	arithmetic;	/* arithmetic data type		*/
	long	nbytes;		/* # bytes in data area		*/
	long	sindx;		/* source index			*/
	long	sindx2;		/* source index	+ 1		*/
	long	rindx;		/* result index			*/
	_f_int	bytealligned;	/* byte aligned flag		*/
	_f_int	bucketsize;	/* element size			*/
	long	src_strd[MAXDIM];	/* index stride		*/
	long	src_ext[MAXDIM];	/* extents		*/
	long	src_indx[MAXDIM];	/* index counters	*/
	long	res_strd[MAXDIM];	/* index stride		*/
	long	res_ext[MAXDIM];	/* extents		*/
	long	res_indx[MAXDIM];	/* index counters	*/
	long	curdim[MAXDIM];		/* current dimension	*/
	long	tot_ext;		/* total extent counter	*/
	long	src_tot_ext;		/* total source extent	*/
	_f_int	ndim;			/* non-dim dimension	*/
	long	res_tmp1;		/* temp value		*/
	long	res_tmp2;		/* temp value		*/
	long	res_dim_strd;		/* stride for dim index	*/
	long	cnt;			/* counter		*/
	long	i, j, k;		/* index variables	*/
	_f_int	early_exit;		/* early exit flag	*/

/*	Set type and dimension global variables	*/

	rank = source->n_dim;
	type = source->type_lens.type;

/*	See if we can use the shortcut exit	*/

	early_exit = 0;
	for (i = 0; i < rank; i++) {
	    if (source->dimension[i].extent == 0)
		early_exit = 1;
	}
	if (result->assoc) {
	    for (i = 0; i < result->n_dim; i++) {
		if (result->dimension[i].extent == 0)
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
	    src_strd[i] = 0;
	    src_ext[i] = 0;
	    src_indx[i] = 0;
	    res_strd[i] = 0;
	    res_ext[i] = 0;
	    res_indx[i] = 0;
	}

/*	Set up scalars for dim and ncopies	*/

	if (*dimp < 1 || *dimp > rank+2)
	    _lerror (_LELVL_ABORT, FESCIDIM);
	dim = *dimp - 1;
	if (*ncopiesp > 0)
	    ncopies = *ncopiesp;
	else
	    ncopies = 0;

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

/*	If necessary, fill result dope vector	*/
	if (!result->assoc) {
	    result->base_addr.a.ptr = (void *) NULL;
	    result->orig_base	   = 0;
	    result->orig_size	   = 0;

	    tot_ext = 1;
#ifdef _UNICOS
#pragma _CRI novector
#endif
	    for (i = 0; i < dim; i++) {
		result->dimension[i].extent = source->dimension[i].extent;
		result->dimension[i].stride_mult = tot_ext * bucketsize;
		result->dimension[i].low_bound = 1;
		tot_ext *= source->dimension[i].extent;
	    }
	    result->dimension[i].extent = ncopies;
	    result->dimension[i].stride_mult = tot_ext * bucketsize;
	    result->dimension[i].low_bound = 1;
	    tot_ext *= ncopies;
	    i++;
#ifdef _UNICOS
#pragma _CRI novector
#endif
	    for ( ; i < rank+1; i++) {
		result->dimension[i].extent = source->dimension[i-1].extent;
		result->dimension[i].stride_mult = tot_ext * bucketsize;
		result->dimension[i].low_bound = 1;
		tot_ext *= source->dimension[i-1].extent;
	    }

	    if (!bytealligned) {
		nbytes = bucketsize * BYTES_PER_WORD;
#ifdef _CRAYMPP
		if (subtype == DVSUBTYPE_BIT32)
		    nbytes /= 2;
#endif
	    } else {
		nbytes = bucketsize;
	    }
	    for (i = 0; i < result->n_dim; i++)
		nbytes *= result->dimension[i].extent;
	    if (ncopies > 0 && nbytes > 0) {
		result->base_addr.a.ptr = (void *) malloc (nbytes);
		if (result->base_addr.a.ptr == NULL)
		    _lerror (_LELVL_ABORT, FENOMEMY);
	    } else {
		result->base_addr.a.ptr = NULL;
	    }

	    result->assoc = 1;
	    result->base_addr.a.el_len = source->base_addr.a.el_len;
	    if (type == DVTYPE_ASCII) {
		result->base_addr.a.ptr = (void *) result->base_addr.a.ptr;
		cr = (char *) result->base_addr.a.ptr;
		result->base_addr.charptr = _cptofcd (cr, bucketsize);
	    }
	    result->orig_base = (void *) result->base_addr.a.ptr;
	    result->orig_size = nbytes * BITS_PER_BYTE;
	}

/*	If ncopies is 0 or early exit conditions met, return	*/

	if (ncopies == 0 || early_exit == 1)
	    return;

/*	Set up scalar pointers to the argument data areas	*/

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

/*
 *	This program is divided up into three sections.  The first handles
 *	scalars being expanded into 1-dimensional arrays.  The second treats
 *	arrays being spread into 2-dimensional matrices.  The last section
 *	deals with all other cases.  Inside each section, the data types
 *	are broken down into groups based on their bit size.  All 64-bit
 *	entities are handled as the same data type, as are all 128-bit, and
 *	where applicable, all 32-bit and all 256-bit data types are treated
 *	together.  Character and derived types are also handled separately.
 */

	if (rank == 0) {
	    if (bucketsize > 1 && arithmetic) {
		res_strd[0] = result->dimension[0].stride_mult / bucketsize;
	    } else {
		res_strd[0] = result->dimension[0].stride_mult;
	    }
	    switch (subtype) {
		case DVSUBTYPE_BIT64 :
		    uptr1 = (_f_int8 *) sptr;
		    uptr2 = (_f_int8 *) rptr;
		    for (i = 0; i < ncopies; i++) {
			rindx = i * res_strd[0];
			uptr2[rindx] = uptr1[0];
		    }
		    break;

		case DVSUBTYPE_BIT32 :
		    hptr1 = (_f_int4 *) sptr;
		    hptr2 = (_f_int4 *) rptr;
		    for (i = 0; i < ncopies; i++) {
			rindx = i * res_strd[0];
			hptr2[rindx] = hptr1[0];
		    }
		    break;

		case DVSUBTYPE_BIT128 :
		    dptr1 = (_f_real16 *) sptr;
		    dptr2 = (_f_real16 *) rptr;
		    for (i = 0; i < ncopies; i++) {
			rindx = i * res_strd[0];
			dptr2[rindx] = dptr1[0];
		    }
		    break;

		case DVSUBTYPE_CHAR :
		    cptr1 = (char *) cs;
		    for (i = 0; i < ncopies; i++) {
			cptr2 = (char *) cr + (i * res_strd[0]);
			(void) memcpy (cptr2, cptr1, bucketsize);
		    }
		break;

		case DVSUBTYPE_DERIVED :
		    for (i = 0; i < bucketsize; i++) {
			fptr1 = (_f_int *) sptr + i;
			fptr2 = (_f_int *) rptr + i;
			for (j = 0; j < ncopies; j++) {
			    rindx = j * res_strd[0];
			    fptr2[rindx] = fptr1[0];
			}
		    }
		    break;

#ifdef _F_COMP16
		case DVSUBTYPE_BIT256 :
		    xptr1 = (dblcmplx *) sptr;
		    xptr2 = (dblcmplx *) rptr;
		    for (i = 0; i < ncopies; i++) {
			rindx = i * res_strd[0];
			xptr2[rindx].re = xptr1[0].re;
			xptr2[rindx].im = xptr1[0].im;
		    }
		    break;
#endif

		default :
		    _lerror (_LELVL_ABORT, FEINTDTY);
	    }
	} else if (rank == 1) {
	    src_ext[0] = source->dimension[0].extent;
	    if (bucketsize > 1 && arithmetic) {
		src_strd[0] = source->dimension[0].stride_mult / bucketsize;
		res_strd[0] = result->dimension[0].stride_mult / bucketsize;
		res_strd[1] = result->dimension[1].stride_mult / bucketsize;
	    } else {
		src_strd[0] = source->dimension[0].stride_mult;
		res_strd[0] = result->dimension[0].stride_mult;
		res_strd[1] = result->dimension[1].stride_mult;
	    }

	    if (dim == 0)
		ndim = 1;
	    else
		ndim = 0;

	    switch (subtype) {
		case DVSUBTYPE_BIT64 :
		    uptr1 = (_f_int8 *) sptr;
		    uptr2 = (_f_int8 *) rptr;
		    for (i = 0; i < src_ext[0]; i++) {
			res_tmp1 = i * res_strd[ndim];
			sindx = i * src_strd[0];
			for (j = 0; j < ncopies; j++) {
			    rindx = res_tmp1 + (j * res_strd[dim]);
			    uptr2[rindx] = uptr1[sindx];
			}
		    }
		    break;

		case DVSUBTYPE_BIT32 :
		    hptr1 = (_f_int4 *) sptr;
		    hptr2 = (_f_int4 *) rptr;
		    for (i = 0; i < src_ext[0]; i++) {
			res_tmp1 = i * res_strd[ndim];
			sindx = i * src_strd[0];
			for (j = 0; j < ncopies; j++) {
			    rindx = res_tmp1 + (j * res_strd[dim]);
			    hptr2[rindx] = hptr1[sindx];
			}
		    }
		    break;

		case DVSUBTYPE_BIT128 :
		    dptr1 = (_f_real16 *) sptr;
		    dptr2 = (_f_real16 *) rptr;
		    for (i = 0; i < src_ext[0]; i++) {
			res_tmp1 = i * res_strd[ndim];
			sindx = i * src_strd[0];
			for (j = 0; j < ncopies; j++) {
			    rindx = res_tmp1 + (j * res_strd[dim]);
			    dptr2[rindx] = dptr1[sindx];
			}
		    }
		    break;

		case DVSUBTYPE_CHAR :
		    for (i = 0; i < src_ext[0]; i++) {
			res_tmp1 = i * res_strd[ndim];
			sindx = i * src_strd[0];
			cptr1 = (char *) cs + sindx;
			for (j = 0; j < ncopies; j++) {
			    rindx = res_tmp1 + (j * res_strd[dim]);
			    cptr2 = (char *) cr + rindx;
			    (void) memcpy (cptr2, cptr1, bucketsize);
			}
		    }
		    break;

		case DVSUBTYPE_DERIVED :
		    for (i = 0; i < bucketsize; i++) {
			fptr1 = (_f_int *) sptr + i;
			fptr2 = (_f_int *) rptr + i;
			for (j = 0; j < src_ext[0]; j++) {
			    res_tmp1 = j * res_strd[ndim];
			    sindx = j * src_strd[0];
			    for (k = 0; k < ncopies; k++) {
				rindx = res_tmp1 + (k * res_strd[dim]);
				fptr2[rindx] = fptr1[sindx];
			    }
			}
		    }
		    break;

#ifdef _F_COMP16
		case DVSUBTYPE_BIT256 :
		    xptr1 = (dblcmplx *) sptr;
		    xptr2 = (dblcmplx *) rptr;
		    for (i = 0; i < src_ext[0]; i++) {
			res_tmp1 = i * res_strd[ndim];
			sindx = i * src_strd[0];
			for (j = 0; j < ncopies; j++) {
			    rindx = res_tmp1 + (j * res_strd[dim]);
			    xptr2[rindx].re = xptr1[sindx].re;
			    xptr2[rindx].im = xptr1[sindx].im;
			}
		    }
		    break;
#endif

		default :
		    _lerror (_LELVL_ABORT, FEINTDTY);
	    }
	} else {
	    src_tot_ext = 1;
#ifdef _UNICOS
#pragma	_CRI	shortloop
#endif
	    for (i = 0; i < rank; i++) {
		if (bucketsize > 1 && arithmetic) {
		    src_strd[i] = source->dimension[i].stride_mult / bucketsize;
		} else {
		    src_strd[i] = source->dimension[i].stride_mult;
		}
		src_ext[i] = source->dimension[i].extent;
		src_tot_ext *= src_ext[i];
		src_indx[i] = 0;
		curdim[i] = 0;
	    }

	    cnt = 0;
	    tot_ext = 1;
#ifdef _UNICOS
#pragma	_CRI	shortloop
#endif
	    for (i = 0; i <= rank; i++) {
		res_indx[i] = 0;
		if (i != dim) {
		    if (type == DVTYPE_ASCII || type == DVTYPE_DERIVEDBYTE ||
			type == DVTYPE_DERIVEDWORD)
			res_strd[cnt] = tot_ext * bucketsize;
		    else
			res_strd[cnt] = tot_ext;
		    res_ext[cnt] = result->dimension[i].extent;
		    tot_ext *= res_ext[cnt];
		    cnt++;
		} else {
		    if (type == DVTYPE_ASCII || type == DVTYPE_DERIVEDBYTE ||
			type == DVTYPE_DERIVEDWORD)
			res_dim_strd = tot_ext * bucketsize;
		    else
			res_dim_strd = tot_ext;
		    tot_ext *= ncopies;
		}
	    }

	    switch (subtype) {
		case DVSUBTYPE_BIT64 :
		    uptr1 = (_f_int8 *) sptr;
		    uptr2 = (_f_int8 *) rptr;
		    sindx = 0;
		    res_tmp2 = 0;
		    for (i = 0; i < src_tot_ext; i++) {
			for (j = 0; j < ncopies; j++) {
			    rindx = res_tmp2 + (j * res_dim_strd);
			    uptr2[rindx] = uptr1[sindx];
			}
			INCREMENT();
		    }
		    break;

		case DVSUBTYPE_BIT32 :
		    hptr1 = (_f_int4 *) sptr;
		    hptr2 = (_f_int4 *) rptr;
		    sindx = 0;
		    res_tmp2 = 0;
		    for (i = 0; i < src_tot_ext; i++) {
			for (j = 0; j < ncopies; j++) {
			    rindx = res_tmp2 + (j * res_dim_strd);
			    hptr2[rindx] = hptr1[sindx];
			}
			INCREMENT();
		    }
		    break;

		case DVSUBTYPE_BIT128 :
		    dptr1 = (_f_real16 *) sptr;
		    dptr2 = (_f_real16 *) rptr;
		    sindx = 0;
		    res_tmp2 = 0;
		    for (i = 0; i < src_tot_ext; i++) {
			for (j = 0; j < ncopies; j++) {
			    rindx = res_tmp2 + (j * res_dim_strd);
			    dptr2[rindx] = dptr1[sindx];
			}
			INCREMENT();
		    }
		    break;

		case DVSUBTYPE_CHAR :
		    sindx = 0;
		    res_tmp2 = 0;
		    for (i = 0; i < src_tot_ext; i++) {
			cptr1 = (char *) cs + sindx;
			for (j = 0; j < ncopies; j++) {
			    rindx = res_tmp2 + (j * res_dim_strd);
			    cptr2 = (char *) cr + rindx;
			    (void) memcpy (cptr2, cptr1, bucketsize);
			}
			INCREMENT();
		    }
		    break;

		case DVSUBTYPE_DERIVED :
		    for (i = 0; i < bucketsize; i++) {
			fptr1 = (_f_int *) sptr + i;
			fptr2 = (_f_int *) rptr + i;
			sindx = 0;
			res_tmp2 = 0;
			for (j = 0; j < src_tot_ext; j++) {
			    for (k = 0; k < ncopies; k++) {
				rindx = res_tmp2 + (k * res_dim_strd);
				fptr2[rindx] = fptr1[sindx];
			    }
			    INCREMENT();
			}
		    }
		    break;

#ifdef _F_COMP16
		case DVSUBTYPE_BIT256 :
		    xptr1 = (dblcmplx *) sptr;
		    xptr2 = (dblcmplx *) rptr;
		    sindx = 0;
		    res_tmp2 = 0;
		    for (i = 0; i < src_tot_ext; i++) {
			for (j = 0; j < ncopies; j++) {
			    rindx = res_tmp2 + (j * res_dim_strd);
			    xptr2[rindx].re = xptr1[sindx].re;
			    xptr2[rindx].im = xptr1[sindx].im;
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
