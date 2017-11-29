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
_CSHIFT(
DopeVectorType	*result,
DopeVectorType	*array,
DopeVectorType	*shift,
i4 *dim)
{
  char  * result_p, * result_b ;
  char  * array_p, * array_b ;
  char * shift_p, * shift_b ;

  size_t src_extent [MAX_NARY_DIMS] ;
  size_t src_stride [MAX_NARY_DIMS] ;
  size_t src_offset [MAX_NARY_DIMS] ;
  size_t counter[MAX_NARY_DIMS] ;

  size_t res_stride [MAX_NARY_DIMS] ;
  size_t res_extent [MAX_NARY_DIMS] ;
  size_t res_offset [MAX_NARY_DIMS] ;

  int32_t j,ii;
  char *rp, *ap ;
  int32_t res_rank ;
  int32_t shf_rank ;
  int32_t src_rank = GET_RANK_FROM_DESC(array) - 1;

  size_t typ_sz   = GET_ELEMENT_SZ_FROM_DESC(array);

  size_t a_size,a_stride,r_stride, i,k ;
  int8_t  zero_szd_source = FALSE;
  int8_t  byte_aligned = FALSE;

  size_t  a_offs,a_bump,r_bump ;
  size_t ll1,ll2;
  int64_t shft,shf_typ_sz  ;
  char  *rp1,  *ap1, *ap2  ;
  int32_t ddim ;

  size_t shf_stride [MAX_NARY_DIMS] ;
  size_t shf_offset [MAX_NARY_DIMS] ;
  size_t  num_trues ;
  int32_t local_alloc ;
  size_t  tot_ext ;
  size_t  str_sz  ;

  size_t src_size ;

  size_t  res_sz;
  size_t  xfer_sz;
  size_t  tot_sz;

  int8_t computed_shift = FALSE ;
  ddim = (*dim) - 1 ;

  if ((ddim > src_rank) || (ddim < 0))
    ERROR(_LELVL_ABORT,FESCIDIM);

  src_extent[0] = GET_EXTENT_FROM_DESC(array,ddim) ;
  src_stride[0] = GET_STRIDE_FROM_DESC(array,ddim) ;
  byte_aligned  = GET_BYTEALIGNED_FROM_DESC(array) ;

  for ( j = 0, k = 1 ; j <= src_rank ; j ++  ) {
    if (j != ddim ) {
      src_extent[k] = GET_EXTENT_FROM_DESC(array,j) ;
      src_stride[k] = GET_STRIDE_FROM_DESC(array,j) ;
      src_offset[k-1] = src_stride[k] - (src_stride [k-1] * (src_extent[k-1]))  ;
      k++ ;
    }
    counter[j] = 0 ;
    shf_offset[j] = 0 ;
    zero_szd_source = zero_szd_source || (src_extent[j] == 0) ;
  }

  if (!GET_ASSOCIATED_FROM_DESC(result)) {

    size_t  nbytes  ;
    size_t  ext  ;
    char    *p      ;

    SET_ADDRESS_IN_DESC(result,NULL);
    SET_ORIG_BS_IN_DESC(result,NULL) ;
    SET_ORIG_SZ_IN_DESC(result,0) ;

    p = NULL ;
    tot_ext = 1 ;
    nbytes  = typ_sz ;
    str_sz  = MK_STRIDE(byte_aligned,typ_sz);

    for ( i = 0 ; i <= src_rank ; i ++) {
      ext = GET_EXTENT_FROM_DESC(array,i) ;
      SET_LBOUND_IN_DESC(result,i,1);
      SET_EXTENT_IN_DESC(result,i,ext);
      SET_STRMULT_IN_DESC(result,i,tot_ext * str_sz );
      tot_ext *= ext;
      nbytes  *= ext;
    }

    if (nbytes > 0) {
      p = (void *) malloc (nbytes);
      if (p == NULL)
	ERROR(_LELVL_ABORT, FENOMEMY);

      SET_ADDRESS_IN_DESC(result,p);
    }

    SET_ASSOCIATED_IN_DESC(result);
    SET_CONTIG_IN_DESC(result);
    if (GET_DV_ASCII_FROM_DESC(array)) {
      SET_CHARPTR_IN_DESC(result,p,typ_sz);
    }
    SET_ORIG_BS_IN_DESC(result,p) ;
    SET_ORIG_SZ_IN_DESC(result,nbytes * 8) ;
  }

  res_stride[0] = GET_STRIDE_FROM_DESC(result,ddim) ;

  for ( j = 0, k = 1  ; j <= src_rank ; j ++  ) {
    if (j != ddim ) {
      res_stride[k] = GET_STRIDE_FROM_DESC(result,j) ;
      res_offset[k-1] = res_stride[k] - (res_stride [k-1] * (src_extent[k-1])) ;
      k++ ;
    }
  }

  shf_typ_sz = GET_ELEMENT_SZ_FROM_DESC(shift);
  shf_rank   = GET_RANK_FROM_DESC(shift);
  shift_p    = GET_ADDRESS_FROM_DESC(shift);

  shf_stride[0] = 0  ;
  for ( j = 0 ; j < shf_rank ; j ++  ) {
    shf_stride[j] = GET_STRIDE_FROM_DESC(shift,j) ;
  }

  for ( j = 1 ; j < shf_rank ; j ++  ) {
    shf_offset[j] = shf_stride[j] - (shf_stride [j-1] * (src_extent[j])) ;
  }

  a_bump = src_extent[0] * src_stride[0] ;
  r_bump = src_extent[0] * res_stride[0] ;

  if (zero_szd_source)
    return ;

  a_size   = src_extent[0] ;
  a_stride = src_stride[0] ;
  r_stride = res_stride[0] ;
  array_p = GET_ADDRESS_FROM_DESC(array);
  result_p = GET_ADDRESS_FROM_DESC(result);

  if (typ_sz == sizeof(i1) && ALIGNED_i1(array_p) &&  ALIGNED_i1(result_p)) {

    while (counter[src_rank] < src_extent[src_rank] ) {
      if (!computed_shift) {
	switch (shf_typ_sz) {
	case sizeof(i1) : 
	  shft = * (i1 *)shift_p ; 
	  break ;
	case sizeof(i2) : 
	  shft = * (i2 *)shift_p ; 
	  break ;
	case sizeof(i4) : 
	  shft = * (i4 *)shift_p ; 
	  break ;
	case sizeof(i8) : 
	  shft = * (i8 *)shift_p ; 
	  break ;
	}

	shft = shft % (int64_t)src_extent[0];
	if (shft < 0 ) {
	  ll1 = abs(shft) ;
	  ll2 = (int64_t)src_extent[0] - abs(shft)  ;

	} else {
	  ll1 = (int64_t)src_extent[0] -  shft ;
	  ll2 = shft ;
	}
	a_offs   = a_stride * ll2 ;
	shift_p  += shf_stride[0] ;

	if (shf_rank == 0)
	  computed_shift = TRUE;
      }

      ap1 = array_p + a_offs ;

      for ( k = 0 ; k < ll1 ; k ++ )  {
	*(i1 *)result_p = *(i1 *)ap1 ;
	result_p += r_stride ;
	ap1 += a_stride ;
      }

      ap2 = array_p ;

      for ( k = 0 ; k < ll2 ; k ++ ) {
	*(i1 *)result_p = *(i1 *)ap2 ;
	result_p += r_stride ;

	ap2 += a_stride ;
      }
      array_p += a_bump ;

      counter[0] = a_size  ;
      j = 0 ;
      while ((counter[j] == src_extent[j]) && (j < src_rank)) {
	array_p += src_offset[j] ;
	result_p += res_offset[j] ;
	shift_p  += shf_offset[j] ;
	counter[j+1]++ ;
	counter[j] = 0 ;
	j ++ ;
      }

    }
  } else if (typ_sz == sizeof(i2) && ALIGNED_i2(array_p) &&  ALIGNED_i2(result_p) ) {

    while (counter[src_rank] < src_extent[src_rank] ) {
      if (!computed_shift) {
	switch (shf_typ_sz) {
	case sizeof(i1) : 
	  shft = * (i1 *)shift_p ; 
	  break ;
	case sizeof(i2) : 
	  shft = * (i2 *)shift_p ; 
	  break ;
	case sizeof(i4) : 
	  shft = * (i4 *)shift_p ; 
	  break ;
	case sizeof(i8) : 
	  shft = * (i8 *)shift_p ; 
	  break ;
	}

	shft = shft % (int64_t)src_extent[0];
	if (shft < 0 ) {
	  ll1 = abs(shft) ;
	  ll2 = (int64_t)src_extent[0] - abs(shft)  ;

	} else {
	  ll1 = (int64_t)src_extent[0] -  shft ;
	  ll2 = shft ;
	}
	a_offs   = a_stride * ll2 ;
	shift_p  += shf_stride[0] ;

	if (shf_rank == 0)
	  computed_shift = TRUE;
      }

      ap1 = array_p + a_offs ;

      for ( k = 0 ; k < ll1 ; k ++ )  {
	*(i2 *)result_p = *(i2 *)ap1 ;
	result_p += r_stride ;
	ap1 += a_stride ;
      }

      ap2 = array_p ;

      for ( k = 0 ; k < ll2 ; k ++ ) {
	*(i2 *)result_p = *(i2 *)ap2 ;
	result_p += r_stride ;

	ap2 += a_stride ;
      }
      array_p += a_bump ;

      counter[0] = a_size  ;
      j = 0 ;
      while ((counter[j] == src_extent[j]) && (j < src_rank)) {
	array_p += src_offset[j] ;
	result_p += res_offset[j] ;
	shift_p  += shf_offset[j] ;
	counter[j+1]++ ;
	counter[j] = 0 ;
	j ++ ;
      }

    }
  } else if (typ_sz == sizeof(r4) && ALIGNED_r4(array_p) &&  ALIGNED_r4(result_p) ) {

    while (counter[src_rank] < src_extent[src_rank] ) {
      if (!computed_shift) {
	switch (shf_typ_sz) {
	case sizeof(i1) : 
	  shft = * (i1 *)shift_p ; 
	  break ;
	case sizeof(i2) : 
	  shft = * (i2 *)shift_p ; 
	  break ;
	case sizeof(i4) : 
	  shft = * (i4 *)shift_p ; 
	  break ;
	case sizeof(i8) : 
	  shft = * (i8 *)shift_p ; 
	  break ;
	}

	shft = shft % (int64_t)src_extent[0];
	if (shft < 0 ) {
	  ll1 = abs(shft) ;
	  ll2 = (int64_t)src_extent[0] - abs(shft)  ;

	} else {
	  ll1 = (int64_t)src_extent[0] -  shft ;
	  ll2 = shft ;
	}
	a_offs   = a_stride * ll2 ;
	shift_p  += shf_stride[0] ;

	if (shf_rank == 0)
	  computed_shift = TRUE;
      }

      ap1 = array_p + a_offs ;

      for ( k = 0 ; k < ll1 ; k ++ )  {
	*(r4 *)result_p = *(r4 *)ap1 ;
	result_p += r_stride ;
	ap1 += a_stride ;
      }

      ap2 = array_p ;

      for ( k = 0 ; k < ll2 ; k ++ ) {
	*(r4 *)result_p = *(r4 *)ap2 ;
	result_p += r_stride ;

	ap2 += a_stride ;
      }
      array_p += a_bump ;

      counter[0] = a_size  ;
      j = 0 ;
      while ((counter[j] == src_extent[j]) && (j < src_rank)) {
	array_p += src_offset[j] ;
	result_p += res_offset[j] ;
	shift_p  += shf_offset[j] ;
	counter[j+1]++ ;
	counter[j] = 0 ;
	j ++ ;
      }

    }
  } else if (typ_sz == sizeof(r8) && ALIGNED_r8(array_p) &&  ALIGNED_r8(result_p) ) {

    while (counter[src_rank] < src_extent[src_rank] ) {
      if (!computed_shift) {
	switch (shf_typ_sz) {
	case sizeof(i1) : 
	  shft = * (i1 *)shift_p ; 
	  break ;
	case sizeof(i2) : 
	  shft = * (i2 *)shift_p ; 
	  break ;
	case sizeof(i4) : 
	  shft = * (i4 *)shift_p ; 
	  break ;
	case sizeof(i8) : 
	  shft = * (i8 *)shift_p ; 
	  break ;
	}

	shft = shft % (int64_t)src_extent[0];
	if (shft < 0 ) {
	  ll1 = abs(shft) ;
	  ll2 = (int64_t)src_extent[0] - abs(shft)  ;

	} else {
	  ll1 = (int64_t)src_extent[0] -  shft ;
	  ll2 = shft ;
	}
	a_offs   = a_stride * ll2 ;
	shift_p  += shf_stride[0] ;

	if (shf_rank == 0)
	  computed_shift = TRUE;
      }

      ap1 = array_p + a_offs ;

      for ( k = 0 ; k < ll1 ; k ++ )  {
	*(r8 *)result_p = *(r8 *)ap1 ;
	result_p += r_stride ;
	ap1 += a_stride ;
      }

      ap2 = array_p ;

      for ( k = 0 ; k < ll2 ; k ++ ) {
	*(r8 *)result_p = *(r8 *)ap2 ;
	result_p += r_stride ;

	ap2 += a_stride ;
      }
      array_p += a_bump ;

      counter[0] = a_size  ;
      j = 0 ;
      while ((counter[j] == src_extent[j]) && (j < src_rank)) {
	array_p += src_offset[j] ;
	result_p += res_offset[j] ;
	shift_p  += shf_offset[j] ;
	counter[j+1]++ ;
	counter[j] = 0 ;
	j ++ ;
      }

    }
  } else if (typ_sz == sizeof(r16) && ALIGNED_r16(array_p) &&  ALIGNED_r16(result_p) ) {

    while (counter[src_rank] < src_extent[src_rank] ) {
      if (!computed_shift) {
	switch (shf_typ_sz) {
	case sizeof(i1) : 
	  shft = * (i1 *)shift_p ; 
	  break ;
	case sizeof(i2) : 
	  shft = * (i2 *)shift_p ; 
	  break ;
	case sizeof(i4) : 
	  shft = * (i4 *)shift_p ; 
	  break ;
	case sizeof(i8) : 
	  shft = * (i8 *)shift_p ; 
	  break ;
	}

	shft = shft % (int64_t)src_extent[0];
	if (shft < 0 ) {
	  ll1 = abs(shft) ;
	  ll2 = (int64_t)src_extent[0] - abs(shft)  ;

	} else {
	  ll1 = (int64_t)src_extent[0] -  shft ;
	  ll2 = shft ;
	}
	a_offs   = a_stride * ll2 ;
	shift_p  += shf_stride[0] ;

	if (shf_rank == 0)
	  computed_shift = TRUE;
      }

      ap1 = array_p + a_offs ;

      for ( k = 0 ; k < ll1 ; k ++ )  {
	*(r16 *)result_p = *(r16 *)ap1 ;
	result_p += r_stride ;
	ap1 += a_stride ;
      }

      ap2 = array_p ;

      for ( k = 0 ; k < ll2 ; k ++ ) {
	*(r16 *)result_p = *(r16 *)ap2 ;
	result_p += r_stride ;

	ap2 += a_stride ;
      }
      array_p += a_bump ;

      counter[0] = a_size  ;
      j = 0 ;
      while ((counter[j] == src_extent[j]) && (j < src_rank)) {
	array_p += src_offset[j] ;
	result_p += res_offset[j] ;
	shift_p  += shf_offset[j] ;
	counter[j+1]++ ;
	counter[j] = 0 ;
	j ++ ;
      }

    }
  } else {
    while (counter[src_rank] < src_extent[src_rank] ) {
      if (!computed_shift) {
	switch (shf_typ_sz) {
	case sizeof(i1) : 
	  shft = * (i1 *)shift_p ; 
	  break ;
	case sizeof(i2) : 
	  shft = * (i2 *)shift_p ; 
	  break ;
	case sizeof(i4) : 
	  shft = * (i4 *)shift_p ; 
	  break ;
	case sizeof(i8) : 
	  shft = * (i8 *)shift_p ; 
	  break ;
	}

	shft = shft % (int64_t)src_extent[0];
	if (shft < 0 ) {
	  ll1 = abs(shft) ;
	  ll2 = (int64_t)src_extent[0] - abs(shft)  ;

	} else {
	  ll1 = (int64_t)src_extent[0] -  shft ;
	  ll2 = shft ;
	}
	a_offs   = a_stride * ll2 ;
	shift_p  += shf_stride[0] ;

	if (shf_rank == 0)
	  computed_shift = TRUE;
      }

      ap1 = array_p + a_offs ;

      for ( k = 0 ; k < ll1 ; k ++ )  {
	rp = result_p ;
	ap = ap1 ;
	if (typ_sz > BIGDEFAULTSZ)
	  (void) memcpy (rp, ap, typ_sz);
	else
	  for (j = 0 ; j < typ_sz ; j ++)  *rp++ = *ap ++ ;
	result_p += r_stride ;
	ap1 += a_stride ;
      }

      ap2 = array_p ;

      for ( k = 0 ; k < ll2 ; k ++ ) {
	rp = result_p ;
	ap = ap2 ;
	if (typ_sz > BIGDEFAULTSZ)
	  (void) memcpy (rp, ap, typ_sz);
	else
	  for (j = 0 ; j < typ_sz ; j ++)  *rp++ = *ap ++ ;
	result_p += r_stride ;

	ap2 += a_stride ;
      }
      array_p += a_bump ;

      counter[0] = a_size  ;
      j = 0 ;
      while ((counter[j] == src_extent[j]) && (j < src_rank)) {
	array_p += src_offset[j] ;
	result_p += res_offset[j] ;
	shift_p  += shf_offset[j] ;
	counter[j+1]++ ;
	counter[j] = 0 ;
	j ++ ;
      }

    }
  }
}
