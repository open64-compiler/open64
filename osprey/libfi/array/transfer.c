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


#pragma ident "@(#) libfi/array/transfer.c	92.1	07/07/99 15:52:02"

#include <liberrno.h>
#include <stddef.h>
#include <cray/dopevec.h>
#include <cray/portdefs.h>
#include "arraydefs.h"

#if defined _F_INT4 && defined _ADDR64
#define		BITS_PER_HALFWORD	(BITS_PER_WORD / 2)
#define		BYTES_PER_HALFWORD	(BYTES_PER_WORD / 2)
#endif

#define	CALC_SINDX()							\
    if (src_rank == 1)							\
	sindx = src_dim[0] * src_strd[0];				\
    else if (src_rank == 2)						\
	sindx = src_dim[0] * src_strd[0] +				\
		src_dim[1] * src_strd[1];				\
    else if (src_rank == 3)						\
	sindx = src_dim[0] * src_strd[0] +				\
		src_dim[1] * src_strd[1] +				\
		src_dim[2] * src_strd[2];				\
    else if (src_rank == 4)						\
	sindx = src_dim[0] * src_strd[0] +				\
		src_dim[1] * src_strd[1] +				\
		src_dim[2] * src_strd[2] +				\
		src_dim[3] * src_strd[3];				\
    else if (src_rank == 5)						\
	sindx = src_dim[0] * src_strd[0] +				\
		src_dim[1] * src_strd[1] +				\
		src_dim[2] * src_strd[2] +				\
		src_dim[3] * src_strd[3] +				\
		src_dim[4] * src_strd[4];				\
    else if (src_rank == 6)						\
	sindx = src_dim[0] * src_strd[0] +				\
		src_dim[1] * src_strd[1] +				\
		src_dim[2] * src_strd[2] +				\
		src_dim[3] * src_strd[3] +				\
		src_dim[4] * src_strd[4] +				\
		src_dim[5] * src_strd[5];				\
    else								\
	sindx = src_dim[0] * src_strd[0] +				\
		src_dim[1] * src_strd[1] +				\
		src_dim[2] * src_strd[2] +				\
		src_dim[3] * src_strd[3] +				\
		src_dim[4] * src_strd[4] +				\
		src_dim[5] * src_strd[5] +				\
		src_dim[6] * src_strd[6];

#define INCR_SRC()							\
	src_dim[0]++;							\
	if (src_dim[0] == src_ext[0]) {					\
	    src_dim[0] = 0;						\
	    src_dim[1]++;						\
	    if (src_dim[1] == src_ext[1]) {				\
		src_dim[1] = 0;						\
		src_dim[2]++;						\
		if (src_dim[2] == src_ext[2]) {				\
		    src_dim[2] = 0;					\
		    src_dim[3]++;					\
		    if (src_dim[3] == src_ext[3]) {			\
			src_dim[3] = 0;					\
			src_dim[4]++;					\
			if (src_dim[4] == src_ext[4]) {			\
			    src_dim[4] = 0;				\
			    src_dim[5]++;				\
			    if (src_dim[5] == src_ext[5]) {		\
				src_dim[5] = 0;				\
				src_dim[6]++;				\
			    }						\
			}						\
		    }							\
		}							\
	    }								\
	}

/*
 *	TRANSFER returns a result with the physical representation identical
 *	to that of the source but interpreted with the type of the mold
 *	argument.
 */

#ifdef _UNICOS
#pragma	_CRI duplicate _TRANSFER as TRANSFER_@
#endif
void
_TRANSFER  (DopeVectorType * result,
	   DopeVectorType * source,
	   DopeVectorType * mold,
	   int		*siz)
{
	void	__transfer();
	__transfer (result, source, mold, siz, 0);
}

#if defined(_WORD32) || defined(_MIPSEB)
_f_int
_TRANSFER1_0_4 (DopeVectorType        *source,
	     DopeVectorType     *mold,
	     int		*siz)
{
	void            __transfer();
	DopeVectorType  result, *res_ptr;

	res_ptr = (DopeVectorType *) &result;
	res_ptr->assoc = 0;
	res_ptr->base_addr.a.el_len = mold->base_addr.a.el_len;
	res_ptr->ptr_alloc = 0;
	res_ptr->p_or_a = NOT_P_OR_A;
	res_ptr->n_dim = 0;
	res_ptr->type_lens.type = mold->type_lens.type;
	res_ptr->type_lens.dpflag = 0;
	res_ptr->type_lens.kind_or_star = DVD_DEFAULT;
	res_ptr->type_lens.int_len = mold->type_lens.int_len;
	res_ptr->type_lens.dec_len = 0;
	res_ptr->orig_base = (_f_int4 *) NULL;
	res_ptr->orig_size = 0;
	__transfer (res_ptr, source, mold, siz, 1);
	return(*(_f_int *) res_ptr->base_addr.a.ptr);
}
#endif

#if defined(_WORD32) || defined(_MIPSEB)
typedef _f_int 		RETURN_TYPE;
#else
typedef _f_int		RETURN_TYPE;
#endif

#ifdef _UNICOS
#pragma	_CRI duplicate _TRANSFER1_0 as TRANSFER1_0@
#endif
RETURN_TYPE
_TRANSFER1_0 (DopeVectorType	*source,
	     DopeVectorType	*mold,
	     int		*siz)
{
	void		__transfer();
	DopeVectorType	result, *res_ptr;

	res_ptr = (DopeVectorType *) &result;
	res_ptr->assoc = 0;
	res_ptr->base_addr.a.el_len = mold->base_addr.a.el_len;
	res_ptr->ptr_alloc = 0;
	res_ptr->p_or_a = NOT_P_OR_A;
	res_ptr->n_dim = 0;
	res_ptr->type_lens.type = mold->type_lens.type;
	res_ptr->type_lens.dpflag = 0;
	res_ptr->type_lens.kind_or_star = DVD_DEFAULT;
	res_ptr->type_lens.int_len = mold->type_lens.int_len;
	res_ptr->type_lens.dec_len = 0;
	res_ptr->orig_base = (_f_int8 *) NULL;
	res_ptr->orig_size = 0;
	__transfer (res_ptr, source, mold, siz, 1);
	return(*(RETURN_TYPE *) res_ptr->base_addr.a.ptr);
}

#ifdef _UNICOS
#pragma	_CRI duplicate _TRANSFER2_0 as TRANSFER2_0@
#endif
_f_dble
_TRANSFER2_0 (DopeVectorType	*source,
	      DopeVectorType	*mold,
	      int		*siz)
{
	void		__transfer();
	DopeVectorType	result, *res_ptr;

	res_ptr = (DopeVectorType *) &result;
	res_ptr->assoc = 0;
	res_ptr->base_addr.a.el_len = mold->base_addr.a.el_len;
	res_ptr->ptr_alloc = 0;
	res_ptr->p_or_a = NOT_P_OR_A;
	res_ptr->n_dim = 0;
	res_ptr->type_lens.type = mold->type_lens.type;
	res_ptr->type_lens.dpflag = 0;
	res_ptr->type_lens.kind_or_star = DVD_DEFAULT;
	res_ptr->type_lens.int_len = mold->type_lens.int_len;
	res_ptr->type_lens.dec_len = 0;
	res_ptr->orig_base = (_f_dble *) NULL;
	res_ptr->orig_size = 0;
	__transfer (res_ptr, source, mold, siz, 2);
	return(*(_f_dble *) res_ptr->base_addr.a.ptr);
}

#ifdef _F_COMP16
#ifdef _UNICOS
#pragma	_CRI duplicate _TRANSFER4_0 as TRANSFER4_0@
#endif
dblcmplx
_TRANSFER4_0 (DopeVectorType	*source,
	      DopeVectorType	*mold,
	      int		*siz)
{
	void		__transfer();
	DopeVectorType	result, *res_ptr;

	res_ptr = (DopeVectorType *) &result;
	res_ptr->assoc = 0;
	res_ptr->base_addr.a.el_len = mold->base_addr.a.el_len;
	res_ptr->ptr_alloc = 0;
	res_ptr->p_or_a = NOT_P_OR_A;
	res_ptr->n_dim = 0;
	res_ptr->type_lens.type = mold->type_lens.type;
	res_ptr->type_lens.dpflag = 0;
	res_ptr->type_lens.kind_or_star = DVD_DEFAULT;
	res_ptr->type_lens.int_len = mold->type_lens.int_len;
	res_ptr->type_lens.dec_len = 0;
	res_ptr->orig_base = (dblcmplx *) NULL;
	res_ptr->orig_size = 0;
	__transfer (res_ptr, source, mold, siz, 4);
	return(*(dblcmplx *) res_ptr->base_addr.a.ptr);
}
#endif

void
__transfer  (DopeVectorType * result,
	   DopeVectorType * source,
	   DopeVectorType * mold,
	   int		*siz,
	   int scalar)
{
	void		* restrict sptr;	/* source pointer	*/
	void		* restrict rptr;	/* result pointer	*/
	char		* restrict cptr1;	/* char			*/
	char		* restrict cptr2;	/* char			*/
	_f_int		* restrict uptr1;	/* unsigned		*/
	_f_int		* restrict uptr2;	/* unsigned		*/
#if defined _F_INT4 && defined _ADDR64
	_f_int4		* restrict hptr1;	/* halfword		*/
	_f_int4		* restrict hptr2;	/* halfword		*/
	_f_int		src_halfword;		/* src halfword flag	*/
	_f_int		mld_halfword;		/* mld halfword flag	*/
	_f_int		halfword;		/* halfword flag	*/
	_f_int		mld_size_half;		/* size in halfwords	*/
	_f_int		src_size_half;		/* size in halfwords	*/
#endif
	char	*cr;			/* char ptr to result array	*/
	char	*cs;			/* char ptr to source array	*/
	_f_int	bytealigned;		/* byte aligned flag		*/
	_f_int	src_bytealigned;	/* source alignment flag	*/
	_f_int	mld_bytealigned;	/* mold alignment flag		*/
	long	nbytes;			/* bytes to allocate		*/
	long	nwords;			/* bytes to allocate		*/
	long	extent;			/* extent counter		*/
	_f_int	mld_size;		/* size of each data element	*/
	_f_int	mld_size_bytes;		/* size of each data element	*/
	_f_int	src_size;		/* size of each data element	*/
	_f_int	src_size_bytes;		/* size of each data element	*/
	long	sindx;			/* source index			*/
	long	rindx;			/* result index			*/
	_f_int	scnt;			/* source word count		*/
	_f_int	rcnt;			/* result word count		*/
	long	res_dim;		/* current result indices	*/
	long	res_strd;		/* stride for each dimension	*/
	long	src_dim[MAXDIM];	/* current source indices	*/
	long	src_ext[MAXDIM];	/* extent for each dimension	*/
	long	src_strd[MAXDIM];	/* stride for each dimension	*/
	long	src_xtnt;		/* extent of src array		*/
	long	mld_xtnt;		/* extent of mold array		*/
	_f_int	src_rank;		/* rank of source array		*/
	long	tot_src_bytes;		/* bytes in source array	*/
	long	leftover;		/* leftover bytes		*/
	_f_int	adjust;			/* word to byte adjust flag	*/
	_f_int	early_exit;		/* flag for early exit		*/
	long	i, j, k;		/* index veriables		*/

/*	Determine whether byte, word or half-word aligned	*/

	bytealigned = 0;
#if defined _F_INT4 && defined _ADDR64
	halfword = 0;
#endif
	if (source->type_lens.type == DVTYPE_ASCII ||
		source->type_lens.type == DVTYPE_DERIVEDBYTE) {
	    bytealigned = 1;
	    src_bytealigned = 1;
	} else {
	    src_bytealigned = 0;
#if defined _F_INT4 && defined _ADDR64
	    if (source->type_lens.int_len == 32) {
		src_halfword = 1;
		halfword = 1;
	    } else
		src_halfword = 0;
#endif
	}
	if (mold->type_lens.type == DVTYPE_ASCII ||
		mold->type_lens.type == DVTYPE_DERIVEDBYTE) {
	    bytealigned = 1;
	    mld_bytealigned = 1;
	} else {
	    mld_bytealigned = 0;
#if defined _F_INT4 && defined _ADDR64
	    if (mold->type_lens.int_len == 32) {
		mld_halfword = 1;
		halfword = 1;
	    } else
		mld_halfword = 0;
#endif
	}

	if (result->assoc) {
	    if (!result->dimension[0].extent)
		return;
	}

/*
 *	Initialize every array element to 0.
 */

#ifdef _UNICOS
#pragma	_CRI	shortloop
#endif
	for (i = 0; i < MAXDIM; i++) {
	    src_dim[i] = 0;
	    src_ext[i] = 0;
	    src_strd[i] = 0;
	}

/*
 *	Determine size of each element, in bytes and words (where appropriate)
 *	Mold and source types must be done separately.
 */

	switch (mold->type_lens.type) {
	    case DVTYPE_ASCII :
		mld_size_bytes = _fcdlen (mold->base_addr.charptr);
		break;
	    case DVTYPE_DERIVEDBYTE :
		mld_size_bytes = mold->base_addr.a.el_len / BITS_PER_BYTE;
		break;
	    case DVTYPE_DERIVEDWORD :
#if defined _F_INT4 && defined _ADDR64
		if (mld_halfword) {
		    mld_size = mold->base_addr.a.el_len / BITS_PER_HALFWORD;
		    mld_size_bytes = mld_size * BYTES_PER_HALFWORD;
		    mld_size_half = mld_size;
		} else {
		    mld_size = mold->base_addr.a.el_len / BITS_PER_WORD;
		    mld_size_bytes = mld_size * BYTES_PER_WORD;
		    if (src_halfword)
			mld_size_half = mld_size << 1;
		}
#else
		mld_size = mold->base_addr.a.el_len / BITS_PER_WORD;
		mld_size_bytes = mld_size * BYTES_PER_WORD;
#endif
		break;
	    default :
#if defined _F_INT4 && defined _ADDR64
		if (mld_halfword) {
		    mld_size = mold->type_lens.int_len / BITS_PER_HALFWORD;
		    mld_size_bytes = mld_size * BYTES_PER_HALFWORD;
		    mld_size_half = mld_size;
		} else {
		    mld_size = mold->type_lens.int_len / BITS_PER_WORD;
		    mld_size_bytes = mld_size * BYTES_PER_WORD;
		    if (src_halfword)
			mld_size_half = mld_size << 1;
		}
#else
		mld_size = mold->type_lens.int_len / BITS_PER_WORD;
		mld_size_bytes = mld_size * BYTES_PER_WORD;
#endif
	}
	switch (source->type_lens.type) {
	    case DVTYPE_ASCII :
		src_size_bytes = _fcdlen (source->base_addr.charptr);
		break;
	    case DVTYPE_DERIVEDBYTE :
		src_size_bytes = source->base_addr.a.el_len /BITS_PER_BYTE;
		break;
	    case DVTYPE_DERIVEDWORD :
#if defined _F_INT4 && defined _ADDR64
		if (src_halfword) {
		    src_size = source->base_addr.a.el_len / BITS_PER_HALFWORD;
		    src_size_bytes = src_size * BYTES_PER_HALFWORD;
		    src_size_half = src_size;
		} else {
		    src_size = source->base_addr.a.el_len / BITS_PER_WORD;
		    src_size_bytes = src_size * BYTES_PER_WORD;
		    if (mld_halfword)
			src_size_half = src_size << 1;
		}
#else
		src_size = source->base_addr.a.el_len / BITS_PER_WORD;
		src_size_bytes = src_size * BYTES_PER_WORD;
#endif
		break;
	    default :
#if defined _F_INT4 && defined _ADDR64
		if (src_halfword) {
		    src_size = source->type_lens.int_len / BITS_PER_HALFWORD;
		    src_size_bytes = src_size * BYTES_PER_HALFWORD;
		    src_size_half = src_size;
		} else {
		    src_size = source->type_lens.int_len / BITS_PER_WORD;
		    src_size_bytes = src_size * BYTES_PER_WORD;
		    if (mld_halfword)
			src_size_half = src_size << 1;
		}
#else
		src_size = source->type_lens.int_len / BITS_PER_WORD;
		src_size_bytes = src_size * BYTES_PER_WORD;
#endif
	}

/*	Calculate total number of bytes in source	*/

	src_xtnt = 1;
	src_rank = source->n_dim;
	for (i = 0; i < src_rank; i++)
	    src_xtnt *= source->dimension[i].extent;
	if (bytealigned)
	    tot_src_bytes = src_xtnt * src_size_bytes;
	else
	    tot_src_bytes = (src_xtnt * src_size_bytes);

/*
 *	If size is specified, total number of bytes to be moved is set to
 *	size * element size (in bytes).
 */

	if (siz) {			/*	size is specified	*/
	    nbytes = *siz * mld_size_bytes;

/*
 *	If size is not specified, and mold is an array, calculate the least
 *	number of bytes which will contain all of the source.  If the total
 *	size of the mold and source arrays is the same, that will be the
 *	number of bytes moved.  If the total size of the source array is a
 *	multiple of the mold size, the number of bytes will be the total
 *	source size, otherwise, calculate the size to be the minimum number
 *	of mold elements needed to completely contain the entire source array.
 */
	} else {
	    if (mold->n_dim > 0) {		/*	mold is array	*/
		if (mld_size_bytes == 0) 
		    nbytes = 0;
		else {
		    if ((tot_src_bytes % mld_size_bytes) == 0) {
			nbytes = tot_src_bytes;
		    } else {
			nbytes = ((tot_src_bytes / mld_size_bytes) + 1) *
				 mld_size_bytes;
		    }
		}

/*
 *	If mold is scalar, result is scalar, and number of bytes is the
 *	number of bytes in one element.
 */
	    } else {			/*	mold is scalar	*/
		nbytes = mld_size_bytes;
	    }
	}

/*
 *	Determine if we can exit the routine early.  This can be done if 
 *	either the source or the mold is a zero-sized array, the mold is
 *	a zero-sized scalar, or the siz argument is illegal.  The conditions
 *	for an early exit are as follows:
 *
 *		SIZ argument present
 *			value of SIZ <= 0
 *			SOURCE is array-valued, and one or more extents = 0
 *			SOURCE is scalar, character oriented, and of size 0
 *			MOLD is array-valued, and one or more extents = 0
 *			MOLD is scalar, character oriented, and of size 0
 *		SIZ argument is not present
 *			SOURCE is array-valued, and one or more extents = 0
 *			SOURCE is scalar, character oriented, and of size 0
 *		
 *	The following conditions are considered to be an error (FEBADMLD):
 *
 *		SIZ is not present, SOURCE is non-0 sized, MOLD is array-
 *			valued, character oriented, and of size 0
 */

	early_exit = 0;
	if (siz) {
	    if (*siz <= 0) {
		result->base_addr.a.ptr = (void *) NULL;
		result->dimension[0].extent = 0;
		result->dimension[0].low_bound = 1;
		if (mld_bytealigned)
		    result->dimension[0].stride_mult = mld_size_bytes;
		else
		    result->dimension[0].stride_mult = mld_size;
		return;
	    }
	    if (source->n_dim > 0) {
#ifdef _UNICOS
#pragma _CRI shortloop
#endif
		for (i = 0; i < source->n_dim; i++) {
		    if (!source->dimension[i].extent)
			early_exit = 1;
		}
	    } else if (src_bytealigned && src_size_bytes == 0) {
		early_exit = 1;
	    }
	    if (mold->n_dim > 0) {
#ifdef _UNICOS
#pragma	_CRI shortloop
#endif
		for (i = 0; i < mold->n_dim; i++) {
		    if (!mold->dimension[i].extent)
			early_exit = 1;
		}
	    } else if (mld_bytealigned && mld_size_bytes == 0)
		early_exit = 1;
	    if (early_exit == 1) {
		result->dimension[0].extent = *siz;
		result->dimension[0].low_bound = 1;
		if (mold->type_lens.type == DVTYPE_ASCII) {
		    result->base_addr.charptr = _cptofcd (NULL, mld_size_bytes);
		    result->dimension[0].stride_mult = mld_size_bytes;
		} else if (mold->type_lens.type == DVTYPE_DERIVEDBYTE) {
		    result->base_addr.a.ptr = (void *) NULL;
		    result->dimension[0].stride_mult = mld_size_bytes;
		} else {
		    result->base_addr.a.ptr = (void *) NULL;
		    result->dimension[0].stride_mult = mld_size;
		}
		return;
	    }
	} else {
	    if (source->n_dim > 0) {
#ifdef _UNICOS
#pragma _CRI shortloop
#endif
		for (i = 0; i < source->n_dim; i++) {
		    if (!source->dimension[i].extent)
			early_exit = 1;
		}
	    } else if (src_bytealigned && src_size_bytes == 0) {
		early_exit = 1;
	    }
	    if (early_exit == 1) {
		if (mold->type_lens.type == DVTYPE_ASCII) {
		    result->base_addr.charptr = _cptofcd (NULL, mld_size_bytes);
		} else {
		    result->base_addr.a.ptr = (void *) NULL;
		}
		if (mold->n_dim > 0) {
		    if (mld_size_bytes > 0)
			result->dimension[0].extent = nbytes / mld_size_bytes;
		    else
			result->dimension[0].extent = 0;
		    result->dimension[0].low_bound = 1;
		    if (mld_bytealigned)
			result->dimension[0].stride_mult = mld_size_bytes;
		    else
			result->dimension[0].stride_mult = mld_size;
		}	    
		return;
	    }
	    if (mold->n_dim > 0) {
		if (mld_bytealigned && mld_size_bytes == 0) {
		    _lerror (_LELVL_ABORT, FEBADMLD);
		}
	    } else {
		if (mld_bytealigned && mld_size_bytes == 0) {
		    result->base_addr.charptr = _cptofcd (NULL, 0);
		    return;
		}
	    }
	}

/*
 *	If result is not associated, allocate space and set up dimension
 *	information.
 */

	if (!result->assoc) {
	    result->base_addr.a.ptr = (void *) NULL;
	    result->orig_base     = 0;
	    result->orig_size     = 0;
	    if (siz) {
		result->dimension[0].extent = *siz;
	 	result->dimension[0].low_bound = 1;
		if (mld_bytealigned)
		    result->dimension[0].stride_mult = mld_size_bytes;
		else
		    result->dimension[0].stride_mult = mld_size;
	    } else {
		if (mold->n_dim > 0) {
		    result->dimension[0].extent = nbytes / mld_size_bytes;
		    result->dimension[0].low_bound = 1;
		    if (mld_bytealigned)
			result->dimension[0].stride_mult = mld_size_bytes;
		    else
			result->dimension[0].stride_mult = mld_size;
		}
	    }
	    result->base_addr.a.ptr = (void *) malloc (nbytes);
	    if (result->base_addr.a.ptr == NULL)
		_lerror (_LELVL_ABORT, FENOMEMY);
	    result->assoc = 1;
	    result->base_addr.a.el_len = mold->base_addr.a.el_len;
	    if (mold->type_lens.type == DVTYPE_ASCII) {
		cr = (char *) result->base_addr.a.ptr;
		result->base_addr.charptr = _cptofcd (cr, mld_size_bytes);
	    }
	    result->orig_base = result->base_addr.a.ptr;
	    result->orig_size = nbytes * BITS_PER_BYTE;
	}

/*
 *	Set up source arrays containing dimension information.  These
 *	temp arrays will be byte/word based, depending on what type of
 *	transfer will be done.  They will not be strictly based on the
 *	type of the source array.
 */

	if (src_rank > 0) {
	    if (src_bytealigned || !bytealigned) {
		for (i = 0; i < src_rank; i++) {
		    src_ext[i] = source->dimension[i].extent;
		    src_strd[i] = source->dimension[i].stride_mult;
		    src_dim[i] = 0;
		}
	    } else {
		for (i = 0; i < src_rank; i++) {
		    src_ext[i] = source->dimension[i].extent;
		    src_strd[i] =
			source->dimension[i].stride_mult * BYTES_PER_WORD;
		    src_dim[i] = 0;
		}
	    }
	}

/*
 *	The actual work will be broken down by word and byte transfers.
 *	The first section will be word oriented transfers.  Inside this
 *	block, the work will be divided by whether the mold variable is
 *	a vector, or a scalar.
 */

	if (!bytealigned) {
#if defined _F_INT4 && defined _ADDR64
	    if (!halfword) {
#endif
		uptr1 = (_f_int *) source->base_addr.a.ptr;
		uptr2 = (_f_int *) result->base_addr.a.ptr;
		if (result->n_dim == 0) {		/* scalar mold */
		    if (mld_size <= src_size) {
			for (i = 0; i < mld_size; i++)
			    uptr2[i] = uptr1[i];
		    } else {
			if (src_rank == 0) {		/* scalar source */
			    for (i = 0; i < src_size; i++)
				uptr2[i] = uptr1[i];
			} else {			/* vector source */
			    extent = mld_size / src_size;
			    leftover = mld_size % src_size;
			    rindx = 0;
			    sindx = 0;
			    for (i = 0; i < extent; i++) {
				CALC_SINDX ();
				for (j = 0; j < src_size; j++)
				    uptr2[rindx++] = uptr1[sindx++];
				INCR_SRC();
			    }
			    if (leftover) {
				CALC_SINDX ();
				for (j = 0; j < leftover; j++)
				    uptr2[rindx++] = uptr1[sindx++];
			    }
			}
		    }
		} else {				/* vector mold	*/
		    res_strd = result->dimension[0].stride_mult;
		    res_dim = 0;
		    sindx = 0;
		    rindx = 0;
		    rcnt = 0;
		    if (nbytes <= tot_src_bytes)
			nwords = nbytes / BYTES_PER_WORD;
		    else
			nwords = tot_src_bytes / BYTES_PER_WORD;
		    if (src_rank == 0) {		/* scalar source */
			for (i = 0; i < nwords; i++) {
			    uptr2[rindx++] = uptr1[sindx++];
			    rcnt++;
			    if (rcnt == mld_size) {
				rcnt = 0;
				res_dim++;
				rindx = res_dim * res_strd;
			    }
			}
		    } else {				/* vector source */
			scnt = 0;
			rcnt = 0;
			for (i = 0; i < nwords; i++) {
			    uptr2[rindx++] = uptr1[sindx++];
			    rcnt++;
			    if (rcnt == mld_size) {
				rcnt = 0;
				res_dim++;
				rindx = res_dim * res_strd;
			    }
			    scnt++;
			    if (scnt == src_size) {
				scnt = 0;
				INCR_SRC();
				CALC_SINDX();
			    }
			}
		    }
		}

/*	If either of the data types is a 32-bit type, and the word size
 *	is 64 bits, we will have to treat the whole affair as 32-bit.
 *	This will be done identically to the 64-bit section, only using
 *	half-word pointers.
 */

#if defined _F_INT4 && defined _ADDR64
	    } else {
		hptr1 = (_f_int4 *) source->base_addr.a.ptr;
		hptr2 = (_f_int4 *) result->base_addr.a.ptr;

/*	If source is 64 bits, double all strides to refer to halfwords	*/

		if (!src_halfword)
		    for (i = 0; i < src_rank; i++)
			src_strd[i] <<= 1;

		if (result->n_dim == 0) {		/* scalar mold */
		    if (mld_size_half <= src_size_half) {
			for (i = 0; i < mld_size_half; i++)
			    hptr2[i] = hptr1[i];
		    } else {
			if (src_rank == 0) {		/* scalar source */
			    for (i = 0; i < src_size_half; i++)
				hptr2[i] = hptr1[i];
			} else {			/* vector source */
			    extent = mld_size_half / src_size_half;
			    leftover = mld_size_half % src_size_half;
			    rindx = 0;
			    sindx = 0;
			    for (i = 0; i < extent; i++) {
				CALC_SINDX ();
				for (j = 0; j < src_size_half; j++)
				    hptr2[rindx++] = hptr1[sindx++];
				INCR_SRC();
			    }
			    if (leftover) {
				CALC_SINDX ();
				for (j = 0; j < leftover; j++)
				    hptr2[rindx++] = hptr1[sindx++];
			    }
			}
		    }
		} else {				/* vector mold	*/
		    if (!mld_halfword) 
			res_strd = result->dimension[0].stride_mult << 1;
		    else
			res_strd = result->dimension[0].stride_mult;
		    res_dim = 0;
		    sindx = 0;
		    rindx = 0;
		    rcnt = 0;
		    if (nbytes <= tot_src_bytes)
			nwords = nbytes / BYTES_PER_HALFWORD;
		    else
			nwords = tot_src_bytes / BYTES_PER_HALFWORD;
		    if (src_rank == 0) {		/* scalar source */
			for (i = 0; i < nwords; i++) {
			    hptr2[rindx++] = hptr1[sindx++];
			    rcnt++;
			    if (rcnt == mld_size_half) {
				rcnt = 0;
				res_dim++;
				rindx = res_dim * res_strd;
			    }
			}
		    } else {				/* vector source */
			scnt = 0;
			rcnt = 0;
			for (i = 0; i < nwords; i++) {
			    hptr2[rindx++] = hptr1[sindx++];
			    rcnt++;
			    if (rcnt == mld_size_half) {
				rcnt = 0;
				res_dim++;
				rindx = res_dim * res_strd;
			    }
			    scnt++;
			    if (scnt == src_size_half) {
				scnt = 0;
				INCR_SRC();
				CALC_SINDX();
			    }
			}
		    }
		}
	    }
#endif
/*
 *	The second block will be for byte transfers.  It is also broken
 *	down by scalar and vector mold.
 */

	} else {

/*	Initialize character pointers to source and result	*/

	    if (src_bytealigned) {
		cs = _fcdtocp (source->base_addr.charptr);
		adjust = 1;
	    } else {
		cs = (char *) source->base_addr.a.ptr;
		adjust = BYTES_PER_WORD;
	    }
	    if (mld_bytealigned)
		cr = _fcdtocp (result->base_addr.charptr);
	    else
		cr = (char *) result->base_addr.a.ptr;

/*	Initialize stride dimension variables	*/

	    if (src_rank > 0) {
		for (i = 0; i < src_rank; i++) {
		    src_strd[i] = source->dimension[i].stride_mult * adjust;
		    src_ext[i]  = source->dimension[i].extent;
		}
	    }

	    if (result->n_dim == 0) {			/* scalar mold	*/
		if (src_rank == 0) {		/* scalar source */
		    if (src_size_bytes >= mld_size_bytes)
			extent = mld_size_bytes;
		    else
			extent = src_size_bytes;
		    (void) memcpy (cr, cs, extent);
		} else {				/* vector source */
		    extent = mld_size_bytes / src_size_bytes;
		    leftover = mld_size_bytes % src_size_bytes;
		    rindx = 0;
		    for (i = 0; i < extent; i++) {
			CALC_SINDX ();
			cptr1 = (char *) cs + sindx;
			cptr2 = (char *) cr + rindx;
			(void) memcpy (cptr2, cptr1, src_size_bytes);
			INCR_SRC ();
			rindx += src_size_bytes;
		    }
		    if (leftover > 0) {
			CALC_SINDX ();
			cptr1 = (char *) cs + sindx;
			cptr2 = (char *) cr + rindx;
			(void) memcpy (cptr2, cptr1, leftover);
		    }
		}
	    } else {					/* vector mold	*/
		if (mld_bytealigned)
		    res_strd = result->dimension[0].stride_mult;
		else
		    res_strd =
			result->dimension[0].stride_mult * BYTES_PER_WORD;

		if (src_rank == 0) {			/* scalar source */
		    extent = src_size_bytes / mld_size_bytes;
		    leftover = src_size_bytes % mld_size_bytes;
		    rindx = 0;
		    sindx = 0;
		    for (i = 0; i < extent; i++) {
			cptr1 = (char *) cs + sindx;
			cptr2 = (char *) cr + rindx;
			(void) memcpy (cptr2, cptr1, mld_size_bytes);
			sindx += mld_size_bytes;
			rindx += res_strd;
		    }
		    if (leftover) {
			cptr1 = (char *) cs + sindx;
			cptr2 = (char *) cr + (extent * res_strd);
			(void) memcpy (cptr2, cptr1, leftover);
		    }
		} else {				/* vector source */
		    sindx = 0;
		    rindx = 0;
		    scnt = 0;
		    rcnt = 0;
		    cptr1 = (char *) cs;
		    cptr2 = (char *) cr;
		    for (i = 0; i < tot_src_bytes; i++) {
			cptr2[rindx+rcnt] = cptr1[sindx+scnt];
			rcnt++;
			if (rcnt == mld_size_bytes) {
			    rcnt = 0;
			    rindx += res_strd;
			}
			scnt ++;
			if (scnt == src_size_bytes) {
			    scnt = 0;
			    INCR_SRC ();
			    CALC_SINDX ();
			}
		    }
		}
	    }
	}
}
