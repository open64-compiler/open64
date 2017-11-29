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

/* automatically generated file, do not edit */

#include "f90_intrinsic.h"

void 
_PACK(
DopeVectorType	*result,
DopeVectorType	*array,
DopeVectorType	*mask,
DopeVectorType	*vector)
{
  char  * result_p, * result_b ;
  char  * array_p, * array_b ;
  char * mask_p, * mask_b ;
  char  * vector_p, * vector_b ;

  size_t src_extent [MAX_NARY_DIMS] ;
  size_t src_stride [MAX_NARY_DIMS] ;
  size_t src_offset [MAX_NARY_DIMS] ;
  size_t counter[MAX_NARY_DIMS] ;

  size_t res_stride [MAX_NARY_DIMS] ;
  size_t res_extent [MAX_NARY_DIMS] ;
  size_t res_offset [MAX_NARY_DIMS] ;

  size_t msk_stride [MAX_NARY_DIMS] ;
  size_t msk_extent [MAX_NARY_DIMS] ;
  size_t msk_offset [MAX_NARY_DIMS] ;

  int32_t j,ii;
  char *rp, *ap ;
  int32_t res_rank ;
  int32_t src_rank = GET_RANK_FROM_DESC(array) - 1;

  size_t typ_sz   = GET_ELEMENT_SZ_FROM_DESC(array);

  size_t a_size,a_stride,r_stride, i,k ;
  int8_t  zero_szd_source = FALSE;
  int8_t  byte_aligned = FALSE;

  int32_t ddim ;

  size_t  num_trues ;
  int32_t local_alloc ;
  size_t  tot_ext ;
  size_t  str_sz  ;

  size_t src_size ;
  size_t m_stride ;
  int32_t msk_rank ;

  size_t  res_sz;
  size_t  xfer_sz;
  size_t  tot_sz;

  src_size = 1 ;

  for ( j = 0 ; j <= src_rank ; j ++  ) {
    src_extent[j]  = GET_EXTENT_FROM_DESC(array,j) ;
    src_stride[j]  = GET_STRIDE_FROM_DESC(array,j) ;
    src_size *= src_extent[j];
    counter[j] = 0 ;
    zero_szd_source = zero_szd_source || (src_extent[j] == 0) ;
  }

  for ( j = 1 ; j <= src_rank ; j ++  )
    src_offset[j-1] = src_stride[j] - (src_stride [j-1] * (src_extent[j-1]))  ;

  byte_aligned  = GET_BYTEALIGNED_FROM_DESC(array) ;
  tot_ext       = src_size ;
  num_trues     = 0 ;
  local_alloc   = FALSE;

  if (vector)
    tot_ext = GET_EXTENT_FROM_DESC(vector,0) ;

  if (!GET_ASSOCIATED_FROM_DESC(result)) {

    size_t  nbytes  ;
    char    *p      ;

    SET_ADDRESS_IN_DESC(result,NULL);
    SET_ORIG_BS_IN_DESC(result,NULL) ;
    SET_ORIG_SZ_IN_DESC(result,0) ;

    p = NULL ;
    local_alloc = TRUE ;
    nbytes  = typ_sz * tot_ext ;
    str_sz  = MK_STRIDE(byte_aligned,typ_sz);

    SET_LBOUND_IN_DESC(result,0,1);
    SET_EXTENT_IN_DESC(result,0,tot_ext);
    SET_STRMULT_IN_DESC(result,0,str_sz);

    if (nbytes > 0 ){
      p =  malloc (nbytes);
      if (p == NULL)
	ERROR(_LELVL_ABORT, FENOMEMY);

      SET_ADDRESS_IN_DESC(result,p);
    }

    SET_CONTIG_IN_DESC(result);
    SET_ASSOCIATED_IN_DESC(result);
    if (GET_DV_ASCII_FROM_DESC(array)) {
      SET_CHARPTR_IN_DESC(result,p,typ_sz);
    }
    SET_ORIG_BS_IN_DESC(result,p) ;
    SET_ORIG_SZ_IN_DESC(result,nbytes * 8) ;
  }

  res_stride[0] = GET_STRIDE_FROM_DESC(result,0) ;

  if (mask != NULL) {
    size_t msk_typ_sz;

    msk_typ_sz = GET_ELEMENT_SZ_FROM_DESC(mask);
    mask_b   = (char *) GET_ADDRESS_FROM_DESC(mask) + OFFSET_TO_TF_BYTE(msk_typ_sz);

    if (GET_RANK_FROM_DESC(mask) == 0) {
      if (*mask_b) {
	for ( j = 0 ; j <= src_rank ; j ++  ) {
	  msk_stride[j] = 0;
	  msk_offset[j] = 0;
	}
      } else
	zero_szd_source = TRUE;

    } else {

      for ( j = 0 ; j <= src_rank ; j ++  ) {
	msk_stride[j] = GET_STRIDE_FROM_DESC(mask,j) ;
      }
      for ( j = 1 ; j <= src_rank ; j ++  ) {
	msk_offset[j-1] = msk_stride[j] - (msk_stride [j-1] * (src_extent[j-1]))  ;
      }
    }
  }

  if (zero_szd_source)
    return ;

  a_size   = src_extent[0] ;
  a_stride = src_stride[0] ;
  r_stride = res_stride[0] ;
  m_stride = msk_stride[0] ;
  array_p = GET_ADDRESS_FROM_DESC(array);
  result_p = GET_ADDRESS_FROM_DESC(result);
  mask_p   = mask_b ;

  if (typ_sz == sizeof(i1) && ALIGNED_i1(array_p) &&  ALIGNED_i1(result_p)) {

    while (counter[src_rank] < src_extent[src_rank] ) {
      for ( i = 0 ; i < a_size ; i ++ ) {
	if (*mask_p) {
	  num_trues ++ ;
	  *(i1 *)result_p = *(i1 *)array_p ;
	  result_p += r_stride ;
	}
	mask_p += m_stride ;
	array_p += a_stride ;
      }

      counter[0] = a_size  ;
      j = 0 ;
      while ((counter[j] == src_extent[j]) && (j < src_rank)) {
	array_p += src_offset[j] ;
	mask_p   += msk_offset[j] ;
	counter[j+1]++ ;
	counter[j] = 0 ;
	j ++ ;
      }
    }

    {

      size_t v_stride,ll1,ll2 ;

      if (vector != NULL) {
	result_b = GET_ADDRESS_FROM_DESC(result);
	vector_b = GET_ADDRESS_FROM_DESC(vector) ;
	v_stride = GET_STRIDE_FROM_DESC(vector,0) ;
	ll1 = (result_p-result_b)/r_stride ;
	vector_p = vector_b + (v_stride * ll1) ;
	ll2 = GET_EXTENT_FROM_DESC(vector,0) ;
	if (ALIGNED_i1(vector_p)) {
	  for ( i = 0 ; i < ll2-ll1 ; i ++ ) {
	    *(i1 *)result_p = *(i1 *)vector_p ;
	    result_p += r_stride ;
	    vector_p += v_stride ;
	  }
	} else {
	  ap = vector_p ;
	  rp = result_p ;
	  for (j = 0 ; j < typ_sz ; j ++)  *rp++ = *ap ++ ;
	  result_p += r_stride ;
	  vector_p += v_stride ;
	}

      } else if (local_alloc) {
	SET_EXTENT_IN_DESC(result,0,num_trues);
      }
    }         
  } else if (typ_sz == sizeof(i2) && ALIGNED_i2(array_p) &&  ALIGNED_i2(result_p) ) {

    while (counter[src_rank] < src_extent[src_rank] ) {
      for ( i = 0 ; i < a_size ; i ++ ) {
	if (*mask_p) {
	  num_trues ++ ;
	  *(i2 *)result_p = *(i2 *)array_p ;
	  result_p += r_stride ;
	}
	mask_p += m_stride ;
	array_p += a_stride ;
      }

      counter[0] = a_size  ;
      j = 0 ;
      while ((counter[j] == src_extent[j]) && (j < src_rank)) {
	array_p += src_offset[j] ;
	mask_p   += msk_offset[j] ;
	counter[j+1]++ ;
	counter[j] = 0 ;
	j ++ ;
      }
    }

    {

      size_t v_stride,ll1,ll2 ;

      if (vector != NULL) {
	result_b = GET_ADDRESS_FROM_DESC(result);
	vector_b = GET_ADDRESS_FROM_DESC(vector) ;
	v_stride = GET_STRIDE_FROM_DESC(vector,0) ;
	ll1 = (result_p-result_b)/r_stride ;
	vector_p = vector_b + (v_stride * ll1) ;
	ll2 = GET_EXTENT_FROM_DESC(vector,0) ;
	if (ALIGNED_i2(vector_p)) {
	  for ( i = 0 ; i < ll2-ll1 ; i ++ ) {
	    *(i2 *)result_p = *(i2 *)vector_p ;
	    result_p += r_stride ;
	    vector_p += v_stride ;
	  }
	} else {
	  ap = vector_p ;
	  rp = result_p ;
	  for (j = 0 ; j < typ_sz ; j ++)  *rp++ = *ap ++ ;
	  result_p += r_stride ;
	  vector_p += v_stride ;
	}

      } else if (local_alloc) {
	SET_EXTENT_IN_DESC(result,0,num_trues);
      }
    }         
  } else if (typ_sz == sizeof(r4) && ALIGNED_r4(array_p) &&  ALIGNED_r4(result_p) ) {

    while (counter[src_rank] < src_extent[src_rank] ) {
      for ( i = 0 ; i < a_size ; i ++ ) {
	if (*mask_p) {
	  num_trues ++ ;
	  *(r4 *)result_p = *(r4 *)array_p ;
	  result_p += r_stride ;
	}
	mask_p += m_stride ;
	array_p += a_stride ;
      }

      counter[0] = a_size  ;
      j = 0 ;
      while ((counter[j] == src_extent[j]) && (j < src_rank)) {
	array_p += src_offset[j] ;
	mask_p   += msk_offset[j] ;
	counter[j+1]++ ;
	counter[j] = 0 ;
	j ++ ;
      }
    }

    {

      size_t v_stride,ll1,ll2 ;

      if (vector != NULL) {
	result_b = GET_ADDRESS_FROM_DESC(result);
	vector_b = GET_ADDRESS_FROM_DESC(vector) ;
	v_stride = GET_STRIDE_FROM_DESC(vector,0) ;
	ll1 = (result_p-result_b)/r_stride ;
	vector_p = vector_b + (v_stride * ll1) ;
	ll2 = GET_EXTENT_FROM_DESC(vector,0) ;
	if (ALIGNED_r4(vector_p)) {
	  for ( i = 0 ; i < ll2-ll1 ; i ++ ) {
	    *(r4 *)result_p = *(r4 *)vector_p ;
	    result_p += r_stride ;
	    vector_p += v_stride ;
	  }
	} else {
	  ap = vector_p ;
	  rp = result_p ;
	  for (j = 0 ; j < typ_sz ; j ++)  *rp++ = *ap ++ ;
	  result_p += r_stride ;
	  vector_p += v_stride ;
	}

      } else if (local_alloc) {
	SET_EXTENT_IN_DESC(result,0,num_trues);
      }
    }         
  } else if (typ_sz == sizeof(r8) && ALIGNED_r8(array_p) &&  ALIGNED_r8(result_p) ) {

    while (counter[src_rank] < src_extent[src_rank] ) {
      for ( i = 0 ; i < a_size ; i ++ ) {
	if (*mask_p) {
	  num_trues ++ ;
	  *(r8 *)result_p = *(r8 *)array_p ;
	  result_p += r_stride ;
	}
	mask_p += m_stride ;
	array_p += a_stride ;
      }

      counter[0] = a_size  ;
      j = 0 ;
      while ((counter[j] == src_extent[j]) && (j < src_rank)) {
	array_p += src_offset[j] ;
	mask_p   += msk_offset[j] ;
	counter[j+1]++ ;
	counter[j] = 0 ;
	j ++ ;
      }
    }

    {

      size_t v_stride,ll1,ll2 ;

      if (vector != NULL) {
	result_b = GET_ADDRESS_FROM_DESC(result);
	vector_b = GET_ADDRESS_FROM_DESC(vector) ;
	v_stride = GET_STRIDE_FROM_DESC(vector,0) ;
	ll1 = (result_p-result_b)/r_stride ;
	vector_p = vector_b + (v_stride * ll1) ;
	ll2 = GET_EXTENT_FROM_DESC(vector,0) ;
	if (ALIGNED_r8(vector_p)) {
	  for ( i = 0 ; i < ll2-ll1 ; i ++ ) {
	    *(r8 *)result_p = *(r8 *)vector_p ;
	    result_p += r_stride ;
	    vector_p += v_stride ;
	  }
	} else {
	  ap = vector_p ;
	  rp = result_p ;
	  for (j = 0 ; j < typ_sz ; j ++)  *rp++ = *ap ++ ;
	  result_p += r_stride ;
	  vector_p += v_stride ;
	}

      } else if (local_alloc) {
	SET_EXTENT_IN_DESC(result,0,num_trues);
      }
    }         
  } else if (typ_sz == sizeof(r16) && ALIGNED_r16(array_p) &&  ALIGNED_r16(result_p) ) {

    while (counter[src_rank] < src_extent[src_rank] ) {
      for ( i = 0 ; i < a_size ; i ++ ) {
	if (*mask_p) {
	  num_trues ++ ;
	  *(r16 *)result_p = *(r16 *)array_p ;
	  result_p += r_stride ;
	}
	mask_p += m_stride ;
	array_p += a_stride ;
      }

      counter[0] = a_size  ;
      j = 0 ;
      while ((counter[j] == src_extent[j]) && (j < src_rank)) {
	array_p += src_offset[j] ;
	mask_p   += msk_offset[j] ;
	counter[j+1]++ ;
	counter[j] = 0 ;
	j ++ ;
      }
    }

    {

      size_t v_stride,ll1,ll2 ;

      if (vector != NULL) {
	result_b = GET_ADDRESS_FROM_DESC(result);
	vector_b = GET_ADDRESS_FROM_DESC(vector) ;
	v_stride = GET_STRIDE_FROM_DESC(vector,0) ;
	ll1 = (result_p-result_b)/r_stride ;
	vector_p = vector_b + (v_stride * ll1) ;
	ll2 = GET_EXTENT_FROM_DESC(vector,0) ;
	if (ALIGNED_r16(vector_p)) {
	  for ( i = 0 ; i < ll2-ll1 ; i ++ ) {
	    *(r16 *)result_p = *(r16 *)vector_p ;
	    result_p += r_stride ;
	    vector_p += v_stride ;
	  }
	} else {
	  ap = vector_p ;
	  rp = result_p ;
	  for (j = 0 ; j < typ_sz ; j ++)  *rp++ = *ap ++ ;
	  result_p += r_stride ;
	  vector_p += v_stride ;
	}

      } else if (local_alloc) {
	SET_EXTENT_IN_DESC(result,0,num_trues);
      }
    }         
  } else {
    while (counter[src_rank] < src_extent[src_rank] ) {
      for ( i = 0 ; i < a_size ; i ++ ) {
	if (*mask_p) {
	  num_trues ++ ;
	  ap = array_p ;
	  rp = result_p ;
	  if (typ_sz > BIGDEFAULTSZ)
	    (void) memcpy (rp, ap, typ_sz);
	  else
	    for (j = 0 ; j < typ_sz ; j ++)  *rp++ = *ap ++ ;
	  result_p += r_stride ;
	}
	mask_p += m_stride ;
	array_p += a_stride ;
      }

      counter[0] = a_size  ;
      j = 0 ;
      while ((counter[j] == src_extent[j]) && (j < src_rank)) {
	array_p += src_offset[j] ;
	mask_p   += msk_offset[j] ;
	counter[j+1]++ ;
	counter[j] = 0 ;
	j ++ ;
      }
    }

    {

      size_t v_stride,ll1,ll2 ;

      if (vector != NULL) {
	result_b = GET_ADDRESS_FROM_DESC(result);
	vector_b = GET_ADDRESS_FROM_DESC(vector) ;
	v_stride = GET_STRIDE_FROM_DESC(vector,0) ;
	ll1 = (result_p-result_b)/r_stride ;
	vector_p = vector_b + (v_stride * ll1) ;
	ll2 = GET_EXTENT_FROM_DESC(vector,0) ;

	for ( i = 0 ; i < ll2-ll1 ; i ++ ) {
	  ap = vector_p ;
	  rp = result_p ;
	  if (typ_sz > BIGDEFAULTSZ)
	    (void) memcpy (rp, ap, typ_sz);
	  else
	    for (j = 0 ; j < typ_sz ; j ++)  *rp++ = *ap ++ ;
	  result_p += r_stride ;
	  vector_p += v_stride ;
	}
      } else if (local_alloc) {
	SET_EXTENT_IN_DESC(result,0,num_trues);
      }
    }
  }
}
