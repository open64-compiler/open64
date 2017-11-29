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


#pragma ident "@(#) libfi/element/merge.c	92.1	07/08/99 15:50:39"

#include <stdlib.h>
#include <liberrno.h>
#include <cray/dopevec.h>
#include <cray/portdefs.h>

#define	DVSUBTYPE_DOUBLE 9
#define DVSUBTYPE_DBLCOMPLEX	10

typedef	struct	{
	_f_real16	re;	/* real part		*/
	_f_real16	im;	/* imaginary part	*/
	} dblcmplx;

#define INCREMENT_N()						\
	curdim[0]++;						\
	if (curdim[0] < msk_ext[0]) {				\
	    mindx += msk_incr[0];				\
	    findx += fls_incr[0];			 	\
	    rindx += res_incr[0];				\
	    tindx += tru_incr[0];				\
	} else {						\
	    curdim[0] = 0;					\
	    curdim[1]++;					\
	    if (curdim[1] < msk_ext[1]) {			\
		mindx += msk_incr[1];				\
		findx += fls_incr[1];				\
		rindx += res_incr[1];				\
		tindx += tru_incr[1];				\
	    } else {						\
		curdim[1] = 0;					\
		curdim[2]++;					\
		if (curdim[2] < msk_ext[2]) {			\
		    mindx += msk_incr[2];			\
		    findx += fls_incr[2];			\
		    rindx += res_incr[2];			\
		    tindx += tru_incr[2];			\
		} else {					\
		    curdim[2] = 0;				\
		    curdim[3]++;				\
		    if (curdim[3] < msk_ext[3]) {		\
			mindx += msk_incr[3];			\
			findx += fls_incr[3];			\
			rindx += res_incr[3];			\
			tindx += tru_incr[3];			\
		    } else {					\
			curdim[3] = 0;				\
			curdim[4]++;				\
			if (curdim[4] < msk_ext[4]) {		\
			    mindx += msk_incr[4];		\
			    findx += fls_incr[4];		\
			    rindx += res_incr[4];		\
			    tindx += tru_incr[4];		\
			} else {				\
			    curdim[4] = 0;			\
			    curdim[5]++;			\
			    if (curdim[5] < msk_ext[5]) {	\
				mindx += msk_incr[5];		\
				findx += fls_incr[5];		\
				rindx += res_incr[5];		\
				tindx += tru_incr[5];		\
			    } else {				\
				curdim[5] = 0;			\
				curdim[6]++;			\
				mindx += msk_incr[6];		\
				findx += fls_incr[6];		\
				rindx += res_incr[6];		\
				tindx += tru_incr[6];		\
			    }					\
			}					\
		    }						\
		}						\
	    }							\
	}
		
void
_MERGE (DopeVectorType * result,
	DopeVectorType * tsource,
	DopeVectorType * fsource,
	DopeVectorType * mask)

{
	char	*cf;		/* char ptr to fsource array	*/
	char	*ct;		/* char ptr to tsource array	*/
	char	*cr;		/* char ptr to result array	*/
	int	chrlenf;	/* length from fsource		*/
	int	chrlenr;	/* length from result		*/
	char		* restrict cptr1;	/* char		*/
	unsigned long	* restrict uptr1;	/* unsigned	*/
	_f_real16	* restrict dptr1;	/* double	*/
	dblcmplx	* restrict xptr1;	/* double cmplx	*/
	char		* restrict cptr2;	/* char		*/
	unsigned long	* restrict uptr2;	/* unsigned	*/
	_f_real16	* restrict dptr2;	/* double	*/
	dblcmplx	* restrict xptr2;	/* double cmplx	*/
	char		* restrict cptr3;	/* char		*/
	unsigned long	* restrict uptr3;	/* unsigned	*/
	_f_real16	* restrict dptr3;	/* double	*/
	dblcmplx	* restrict xptr3;	/* double cmplx	*/
	_f_int		* restrict iptr4;	/* int		*/
	unsigned long	* restrict fptr;	/* fsource	*/
	unsigned long	* restrict rptr;	/* result	*/
	unsigned long	* restrict tptr;	/* tsource	*/
	unsigned long	* restrict mptr;	/* mask		*/
	int	bucketsize;	/* size of element		*/
	int	nbytes;		/* number of bytes		*/
	int	nwords;		/* number of words		*/
	int	curdim[7];	/* current indices		*/
	int	bytealligned;	/* byte aligned flag		*/
	int	findx;		/* fsource index		*/
	int	rindx;		/* result index			*/
	int	mindx;		/* mask index			*/
	int	tindx;		/* tsource index		*/
	int	type;		/* data type			*/
	int	rank;		/* rank of result matrix	*/
	int	i, j, k;	/* index variables		*/
	int	fls_ext[MAXDIM];  /* extents for fsource	*/
	int	fls_strd[MAXDIM]; /* element stride for field	*/
	int	fls_incr[MAXDIM]; /* incr for each index	*/
	int	msk_ext[MAXDIM];  /* extents for fsource	*/
	int	msk_strd[MAXDIM]; /* element stride for field	*/
	int	msk_incr[MAXDIM]; /* incr for each index	*/
	int	res_ext[MAXDIM];  /* extents for fsource	*/
	int	res_strd[MAXDIM]; /* element stride for field	*/
	int	res_incr[MAXDIM]; /* incr for each index	*/
	int	tru_ext[MAXDIM];  /* extents for fsource	*/
	int	tru_strd[MAXDIM]; /* element stride for field	*/
	int	tru_incr[MAXDIM]; /* incr for each index	*/
	int	fls_cum_decr;	/* fsource cumulative decrement	*/
	int	msk_cum_decr;	/* mask cumulative decrement	*/
	int	res_cum_decr;	/* result cumulative decrement	*/
	int	tru_cum_decr;	/* tsource cumulative decrement	*/
	int	tot_ext;	/* total extent counter		*/
	int	msk_0_strd;	/* scaler stride variable	*/
	int	res_0_strd;	/* scaler stride variable	*/
	int	tru_0_strd;	/* scaler stride variable	*/
	int	fls_0_strd;	/* scaler stride variable	*/
	int	one;		/* index holder			*/
	int	zero;		/* index holder			*/

/*	Set type and rank global variables	*/

	type = fsource->type_lens.type;
	rank = mask->n_dim;

/*
 *	Initialize every element of every array to try and minimize problem
 *	in compiler.
 */

	for (i = 0; i < MAXDIM; i++) {
	    fls_ext[i] = 0;
	    fls_strd[i] = 0;
	    fls_incr[i] = 0;
	    msk_ext[i] = 0;
	    msk_strd[i] = 0;
	    msk_incr[i] = 0;
	    res_ext[i] = 0;
	    res_strd[i] = 0;
	    res_incr[i] = 0;
	    tru_ext[i] = 0;
	    tru_strd[i] = 0;
	    tru_incr[i] = 0;
	}

/*	Size calculation is based on variable type	*/

	switch (type) {
	    case DVTYPE_ASCII :
		bytealligned = 1;
		bucketsize = _fcdlen (fsource->base_addr.charptr);
		break;
	    case DVTYPE_DERIVEDBYTE :
		bytealligned = 1;
#ifndef	_ADDR64
		bucketsize = fsource->base_addr.a.el_len >> 3;	  /* bytes */
#else
		bucketsize = _fcdlen (fsource->base_addr.charptr);/* bytes */
#endif
		break;
	    case DVTYPE_DERIVEDWORD :
		bytealligned = 0;
#ifndef	_ADDR64
		bucketsize = fsource->base_addr.a.el_len >> 6;	  /* words */
#else
		bucketsize = _fcdlen (fsource->base_addr.charptr);/* bytes */
		bucketsize >>= 3;				  /* words */
#endif
		break;
	    default :
		bytealligned = 0;
		bucketsize = fsource->type_lens.int_len >> 6;	  /* words */
	}

/*	Set up dope vector for result array	*/

	if (!result->assoc) {
	    result->base_addr.a.ptr = (void *) NULL;
	    result->orig_base = 0;
	    result->orig_size = 0;
	    for (i = 0; i < rank; i++) {
		result->dimension[i].extent = mask->dimension[i].extent;
		result->dimension[i].low_bound = 1;
		result->dimension[i].stride_mult =
			mask->dimension[i].stride_mult * bucketsize;
	    }

/*	Determine size of space to allocate	*/

	    if (!bytealligned)
		nbytes = bucketsize << 3;
	    else
		nbytes = bucketsize;
	    for (i = 0; i < rank; i++)
		nbytes *= mask->dimension[i].extent;
	    nwords = nbytes >> 3;
	    result->base_addr.a.ptr = (void *) malloc (nbytes);
	    if (result->base_addr.a.ptr == NULL)
		_lerror (_LELVL_ABORT, FENOMEMY);

	    result->assoc = 1;
	    if (bytealligned) {
		cr = (char *) result->base_addr.a.ptr;
		result->base_addr.charptr = _cptofcd (cr, bucketsize);
	    }
	    result->orig_base = (void *) result->base_addr.a.ptr;
	    result->orig_size = nwords;
	} else {
	    if (bytealligned) {
		cr = _fcdtocp (result->base_addr.charptr);
		bucketsize = _fcdlen (result->base_addr.charptr);
	    }
	}

/*	Set up subtypes for double precision and double complex	*/

	if (type == DVTYPE_REAL && bucketsize == 2)
	    type = DVSUBTYPE_DOUBLE;
	else if (type == DVTYPE_COMPLEX && bucketsize == 4)
	    type = DVSUBTYPE_DBLCOMPLEX;

/*	Set up scalar pointers to all of the argument data areas	*/

	if (!bytealligned) {
	    fptr = (void *) fsource->base_addr.a.ptr;
	    rptr = (void *) result->base_addr.a.ptr;
	    tptr = (void *) tsource->base_addr.a.ptr;
	} else {
	    cf = _fcdtocp (fsource->base_addr.charptr);
	    ct = _fcdtocp (tsource->base_addr.charptr);
	    cr = _fcdtocp (result->base_addr.charptr);
	}
	mptr = (void *) mask->base_addr.a.ptr;

	msk_0_strd = mask->dimension[0].stride_mult;
	if (type == DVTYPE_COMPLEX || type == DVSUBTYPE_DOUBLE) {
	    res_0_strd = result->dimension[0].stride_mult / 2;
	    tru_0_strd = tsource->dimension[0].stride_mult / 2;
	    fls_0_strd = fsource->dimension[0].stride_mult / 2;
	} else if (type == DVSUBTYPE_DBLCOMPLEX) {
	    res_0_strd = result->dimension[0].stride_mult / 4;
	    tru_0_strd = tsource->dimension[0].stride_mult / 4;
	    fls_0_strd = fsource->dimension[0].stride_mult / 4;
	} else {
	    res_0_strd = result->dimension[0].stride_mult;
	    tru_0_strd = tsource->dimension[0].stride_mult;
	    fls_0_strd = fsource->dimension[0].stride_mult;
	}

	tot_ext = 1;
#pragma _CRI	novector
	for (i = 0; i < rank; i++)
	    tot_ext *= msk_ext[i];

	if (rank == 1) {
	    switch (type) {
		case DVTYPE_INTEGER :	
		case DVTYPE_REAL :
		case DVTYPE_LOGICAL :
		    uptr1 = (unsigned long *) tptr;
		    uptr2 = (unsigned long *) fptr;
		    uptr3 = (unsigned long *) rptr;
		    iptr4 = (_f_int *) mptr;
		    for (i = 0; i < tot_ext; i++) {
			mindx = i * msk_0_strd;
			rindx = i * res_0_strd;
			if (_ltob(&iptr4[mindx])) {
			    tindx = i * tru_0_strd;
			    uptr3[rindx] = uptr1[tindx];
			} else {
			    findx = i * fls_0_strd;
			    uptr3[rindx] = uptr2[findx];
			}
		    }
		    break;

		case DVTYPE_COMPLEX :
		case DVSUBTYPE_DOUBLE :
		    dptr1 = (_f_real16 *) tptr;
		    dptr2 = (_f_real16 *) fptr;
		    dptr3 = (_f_real16 *) rptr;
		    iptr4 = (_f_int *) mptr;
		    for (i = 0; i < tot_ext; i++) {
			mindx = i * msk_0_strd;
			rindx = i * res_0_strd;
			if (_ltob(&iptr4[mindx])) {
			    tindx = i * tru_0_strd;
			    dptr3[rindx] = dptr1[tindx];
			} else {
			    findx = i * fls_0_strd;
			    dptr3[rindx] = dptr2[findx];
			}
		    }
		    break;

		case DVSUBTYPE_DBLCOMPLEX :
		    xptr1 = (dblcmplx *) tptr;
		    xptr2 = (dblcmplx *) fptr;
		    xptr3 = (dblcmplx *) rptr;
		    iptr4 = (_f_int *) mptr;
		    for (i = 0; i < tot_ext; i++) {
			mindx = i * msk_0_strd;
			if (_ltob(&iptr4[mindx])) {
			    rindx = i * res_0_strd;
			    tindx = i * tru_0_strd;
			    xptr3[rindx].re = xptr1[tindx].re;
			    xptr3[rindx].im = xptr1[tindx].im;
			} else {
			    rindx = i * res_0_strd;
			    findx = i * fls_0_strd;
			    xptr3[rindx].re = xptr2[findx].re;
			    xptr3[rindx].im = xptr2[findx].im;
			}
		    }
		    break;

		case DVTYPE_ASCII :
		case DVTYPE_DERIVEDBYTE :
		    iptr4 = (_f_int *) mptr;
		    for (i = 0; i < tot_ext; i++) {
			mindx = i * msk_0_strd;
			rindx = i * res_0_strd;
			if (_ltob(&iptr4[mindx])) {
			    tindx = i * tru_0_strd;
			    cptr3 = (char *) cr + rindx;
			    cptr1 = (char *) ct + tindx;
			    (void) memcpy (cptr3, cptr1, bucketsize);
			} else {
			    findx = i * fls_0_strd;
			    cptr3 = (char *) cr + rindx;
			    cptr2 = (char *) cf + findx;
			    (void) memcpy (cptr3, cptr2, bucketsize);
			}
		    }
		    break;

		default :
		    uptr1 = (unsigned long *) tptr;
		    uptr2 = (unsigned long *) fptr;
		    uptr3 = (unsigned long *) rptr;
		    iptr4 = (_f_int *) mptr;
		    for (i = 0; i < tot_ext; i++) {
			mindx = i * msk_0_strd;
			rindx = i * res_0_strd;
			if (_ltob(&iptr4[mindx])) {
			    tindx = i * tru_0_strd;
			    for (j = 0; j < bucketsize; j++)
				uptr3[rindx+j] = uptr1[tindx+j];
			} else {
			    findx = i * fls_0_strd;
			    for (j = 0; j < bucketsize; j++)
				uptr3[rindx+j] = uptr2[findx+j];
			}
		    }
		    break;
	    }
	} else if (rank == 2) {
	    if (mask->dimension[0].extent < mask->dimension[1].extent) {
		zero = 0;
		one = 1;
	    } else {
		zero = 1;
		one = 0;
	    }
	    msk_ext[zero] = mask->dimension[0].extent;
	    msk_ext[one] = mask->dimension[1].extent;
	    msk_strd[zero] = mask->dimension[0].stride_mult;
	    msk_strd[one] = mask->dimension[1].stride_mult;
	    if (type == DVTYPE_COMPLEX || type == DVSUBTYPE_DOUBLE) {
		res_strd[zero] = result->dimension[0].stride_mult / 2;
		res_strd[one] = result->dimension[1].stride_mult / 2;
	    } else if (type == DVSUBTYPE_DBLCOMPLEX) {
		res_strd[zero] = result->dimension[0].stride_mult / 4;
		res_strd[one] = result->dimension[1].stride_mult / 4;
	    } else {
		res_strd[zero] = result->dimension[0].stride_mult;
		res_strd[one] = result->dimension[1].stride_mult;
	    }
	    if (tsource->n_dim > 0) {
		if (type == DVTYPE_COMPLEX || type == DVSUBTYPE_DOUBLE) {
		    tru_strd[zero] = tsource->dimension[0].stride_mult / 2;
		    tru_strd[one] = tsource->dimension[1].stride_mult / 2;
		} else if (type == DVSUBTYPE_DBLCOMPLEX) {
		    tru_strd[zero] = tsource->dimension[0].stride_mult / 4;
		    tru_strd[one] = tsource->dimension[1].stride_mult / 4;
		} else {
		    tru_strd[zero] = tsource->dimension[0].stride_mult;
		    tru_strd[one] = tsource->dimension[1].stride_mult;
		}
	    } else {
		tru_strd[zero] = 0;
		tru_strd[one] = 0;
	    }
	    if (fsource->n_dim > 0) {
		if (type == DVTYPE_COMPLEX || type == DVSUBTYPE_DOUBLE) {
		    fls_strd[zero] = fsource->dimension[0].stride_mult / 2;
		    fls_strd[one] = fsource->dimension[1].stride_mult / 2;
		} else if (type == DVSUBTYPE_DBLCOMPLEX) {
		    fls_strd[zero] = fsource->dimension[0].stride_mult / 4;
		    fls_strd[one] = fsource->dimension[1].stride_mult / 4;
		} else {
		    fls_strd[zero] = fsource->dimension[0].stride_mult;
		    fls_strd[one] = fsource->dimension[1].stride_mult;
		}
	    } else {
		fls_strd[zero] = 0;
		fls_strd[one] = 0;
	    }

	    switch (type) {
		case DVTYPE_INTEGER :
		case DVTYPE_REAL :
		case DVTYPE_LOGICAL :
		    for (i = 0; i < msk_ext[0]; i++) {
			uptr1 = (unsigned long *) tptr;
			uptr2 = (unsigned long *) fptr;
			uptr3 = (unsigned long *) rptr;
			iptr4 = (_f_int *) mptr + (i * msk_strd[0]);
			for (j = 0; j < msk_ext[1]; j++) {
			    mindx = j * msk_strd[1];
			    if (_ltob(&iptr4[mindx])) {
			        rindx = j * res_strd[1];
				tindx = j * tru_strd[1];
				uptr3[rindx] = uptr1[tindx];
			    } else {
			        rindx = j * res_strd[1];
				findx = j * fls_strd[1];
				uptr3[rindx] = uptr2[findx];
			    }
			}
		    }
		    break;

		case DVTYPE_COMPLEX :
		case DVSUBTYPE_DOUBLE :
		    for (i = 0; i < msk_ext[0]; i++) {
			dptr1 = (_f_real16 *) tptr;
			dptr2 = (_f_real16 *) fptr;
			dptr3 = (_f_real16 *) rptr;
			iptr4 = (_f_int *) mptr + (i * msk_strd[0]);
			for (j = 0; j < msk_ext[1]; j++) {
			    mindx = j * msk_strd[1];
			    if (_ltob(&iptr4[mindx])) {
				rindx = j * res_strd[1];
				tindx = j * tru_strd[1];
				dptr3[rindx] = dptr1[tindx];
			    } else {
				rindx = j * res_strd[1];
				findx = j * fls_strd[1];
				dptr3[rindx] = dptr2[findx];
			    }
			}
		    }
		    break;

		case DVSUBTYPE_DBLCOMPLEX :
		    for (i = 0; i < msk_ext[0]; i++) {
			xptr1 = (dblcmplx *) tptr;
			xptr2 = (dblcmplx *) fptr;
			xptr3 = (dblcmplx *) rptr;
			iptr4 = (_f_int *) mptr + (i * msk_strd[0]);
			for (j = 0; j < msk_ext[1]; j++) {
			    mindx = j * msk_strd[1];
			    if (_ltob(&iptr4[mindx])) {
				rindx = j * res_strd[1];
				tindx = j * tru_strd[1];
				xptr3[rindx].re = xptr1[tindx].re;
				xptr3[rindx].im = xptr1[tindx].im;
			    } else {
				rindx = j * res_strd[1];
				findx = j * fls_strd[1];
				xptr3[rindx].re = xptr2[findx].re;
				xptr3[rindx].im = xptr2[findx].im;
			    }
			}
		    }
		    break;

		case DVTYPE_ASCII :
		case DVTYPE_DERIVEDBYTE :
		    for (i = 0; i < msk_ext[0]; i++) {
			iptr4 = (_f_int *) mptr + (i * msk_strd[0]);
			for (j = 0; j < msk_ext[1]; j++) {
			    mindx = j * msk_strd[1];
			    if (_ltob(&iptr4[mindx])) {
				rindx = j * res_strd[1];
				tindx = j * tru_strd[1];
				cptr1 = (char *) ct + tindx;
				cptr3 = (char *) cr + rindx;
				(void) memcpy (cptr3, cptr1, bucketsize);
			    } else {
				rindx = j * res_strd[1];
				findx = j * fls_strd[1];
				cptr2 = (char *) cf + findx;
				cptr3 = (char *) cr + rindx;
				(void) memcpy (cptr3, cptr2, bucketsize);
			    }
			}
		    }
		    break;

		default :
		    for (i = 0; i < msk_ext[0]; i++) {
			uptr1 = (unsigned long *) tptr;
			uptr2 = (unsigned long *) fptr;
			uptr3 = (unsigned long *) rptr;
			iptr4 = (_f_int *) mptr;
			for (j = 0; j < msk_ext[1]; j++) {
			    mindx = j * msk_strd[1];
			    if (_ltob(&iptr4[mindx])) {
				rindx = j * res_strd[1];
				tindx = j * tru_strd[1];
				for (k = 0; k < bucketsize; k++)
				    uptr3[rindx+k] = uptr1[tindx+k];
			    } else {
				rindx = j * res_strd[1];
				findx = j * fls_strd[1];
				for (k = 0; k < bucketsize; k++)
				    uptr3[rindx+k] = uptr2[findx+k];
			    }
			}
		    }
	    }
	} else {
	    mindx = 0;
	    findx = 0;
	    rindx = 0;
	    tindx = 0;
#pragma	_CRI	shortloop
	    for (i = 0; i < rank; i++)
		curdim[i] = 0;

#pragma _CRI	shortloop
	    for (i = 0; i < rank; i++) {
		if (type == DVTYPE_COMPLEX || type == DVSUBTYPE_DOUBLE)
		    res_strd[i] = result->dimension[i].stride_mult / 2;
		else if (type == DVSUBTYPE_DBLCOMPLEX)
		    res_strd[i] = result->dimension[i].stride_mult / 4;
		else
		    res_strd[i] = result->dimension[i].stride_mult;
	    }

	    if (fsource->n_dim > 0) {
#pragma	_CRI	shortloop
		for (i = 0; i < rank; i++) {
		    if (type == DVTYPE_COMPLEX || type == DVSUBTYPE_DOUBLE)
			fls_strd[i] = fsource->dimension[i].stride_mult / 2;
		    else if (type == DVSUBTYPE_DBLCOMPLEX)
			fls_strd[i] = fsource->dimension[i].stride_mult / 4;
		    else
			fls_strd[i] = fsource->dimension[i].stride_mult;
		}
	    } else {
#pragma	_CRI	shortloop
		for (i = 0; i < rank; i++)
		    fls_strd[i] = 0;
	    }

	    if (tsource->n_dim > 0) {
#pragma	_CRI	shortloop
		for (i = 0; i < rank; i++) {
		    if (type == DVTYPE_COMPLEX || type == DVSUBTYPE_DOUBLE)
			tru_strd[i] = tsource->dimension[i].stride_mult / 2;
		    else if (type == DVSUBTYPE_DBLCOMPLEX)
			tru_strd[i] = tsource->dimension[i].stride_mult / 4;
		    else
			tru_strd[i] = tsource->dimension[i].stride_mult;
		}
	    } else {
#pragma	_CRI	shortloop
		for (i = 0; i < rank; i++)
		    tru_strd[i] = 0;
	    }

	    msk_incr[0] = msk_strd[0];
	    msk_cum_decr = 0;
	    fls_incr[0] = fls_strd[0];
	    fls_cum_decr = 0;
	    res_incr[0] = res_strd[0];
	    res_cum_decr = 0;
	    tru_incr[0] = tru_strd[0];
	    tru_cum_decr = 0;
	
#pragma _CRI	novector
	    for (i = 1; i < rank; i++) {
		msk_cum_decr += (msk_ext[i-1] - 1) * msk_strd[i-1];
		msk_incr[i] = msk_strd[i] - msk_cum_decr;
		fls_cum_decr += (fls_ext[i-1] - 1) * fls_strd[i-1];
		fls_incr[i] = fls_strd[i] - fls_cum_decr;
		res_cum_decr += (res_ext[i-1] - 1) * res_strd[i-1];
		res_incr[i] = res_strd[i] - res_cum_decr;
		tru_cum_decr += (tru_ext[i-1] - 1) * tru_strd[i-1];
		tru_incr[i] = tru_strd[i] - tru_cum_decr;
	    }

	    iptr4 = (_f_int *) mptr + mindx;
	    switch (type) {
		case DVTYPE_INTEGER :
		case DVTYPE_REAL :
		case DVTYPE_LOGICAL :
		    uptr1 = (unsigned long *) fptr;
		    uptr2 = (unsigned long *) tptr;
		    uptr3 = (unsigned long *) rptr;
		    for (i = 0; i < tot_ext; i++) {
			if (_ltob(&iptr4[mindx])) {
			    uptr3[rindx] = uptr2[tindx];
			} else {
			    uptr3[rindx] = uptr1[findx];
			}
			INCREMENT_N();
		    }
		    break;

		case DVTYPE_COMPLEX :
		case DVSUBTYPE_DOUBLE :
		    dptr1 = (_f_real16 *) fptr;
		    dptr2 = (_f_real16 *) tptr;
		    dptr3 = (_f_real16 *) rptr;
		    for (i = 0; i < tot_ext; i++) {
			if (_ltob(&iptr4[mindx])) {
			    dptr3[rindx] = dptr2[tindx];
			} else {
			    dptr3[rindx] = dptr1[findx];
			}
			INCREMENT_N();
		    }
		    break;

		case DVTYPE_ASCII :
		case DVTYPE_DERIVEDBYTE :
		    for (i = 0; i < tot_ext; i++) {
			cptr3 = (char *) cr + rindx;
			if (_ltob(&iptr4[mindx])) {
			    cptr2 = (char *) ct + tindx;
			    (void) memcpy (cptr3, cptr2, bucketsize);
			} else {
			    cptr1 = (char *) cf + findx;
			    (void) memcpy (cptr3, cptr1, bucketsize);
			}
			INCREMENT_N();
		    }
		    break;

		case DVSUBTYPE_DBLCOMPLEX :
		    xptr1 = (dblcmplx *) fptr;
		    xptr2 = (dblcmplx *) tptr;
		    xptr3 = (dblcmplx *) rptr;
		    for (i = 0; i < tot_ext; i++) {
			if (_ltob(&iptr4[mindx])) {
			    xptr3[rindx].re = xptr2[tindx].re;
			    xptr3[rindx].im = xptr2[tindx].im;
			} else {
			    xptr3[rindx].re = xptr1[findx].re;
			    xptr3[rindx].im = xptr1[findx].im;
			}
			INCREMENT_N();
		    }
		    break;

		default :
		    uptr1 = (unsigned long *) fptr;
		    uptr2 = (unsigned long *) tptr;
		    uptr3 = (unsigned long *) rptr;
		    for (i = 0; i < tot_ext; i++) {
			if (_ltob(&iptr4[mindx])) {
			    for (j = 0; j < bucketsize; j++)
				uptr3[rindx+j] = uptr2[tindx+j];
			} else {
			    for (j = 0; j < bucketsize; j++)
				uptr3[rindx+j] = uptr1[findx+j];
			}
			INCREMENT_N();
		    }
	    }
	}
}
