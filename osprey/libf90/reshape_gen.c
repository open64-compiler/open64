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
_RESHAPE(
DopeVectorType	*result,
DopeVectorType	*array,
DopeVectorType	*shape,
DopeVectorType	*pad,
DopeVectorType	*order)
{
  char  * result_p, * result_b ;
  char  * array_p, * array_b ;
  i4 * shape_p, * shape_b ;
  char * pad_p, * pad_b ;
  i4 * order_p, * order_b ;

  size_t src_extent [MAX_NARY_DIMS] ;
  size_t src_stride [MAX_NARY_DIMS] ;
  size_t src_offset [MAX_NARY_DIMS] ;
  size_t counter[MAX_NARY_DIMS] ;

  size_t res_stride [MAX_NARY_DIMS] ;
  size_t res_extent [MAX_NARY_DIMS] ;
  size_t res_offset [MAX_NARY_DIMS] ;
  size_t res_counter[MAX_NARY_DIMS] ;

  size_t pad_stride [MAX_NARY_DIMS] ;
  size_t pad_extent [MAX_NARY_DIMS] ;
  size_t pad_offset [MAX_NARY_DIMS] ;

  int32_t  l_order[MAX_NARY_DIMS] ;
  int32_t  l_order_chk[MAX_NARY_DIMS] ;
  int32_t  l_shape[MAX_NARY_DIMS] ;

  int32_t j,ii;
  char *rp, *ap ;
  int32_t *gp1  ;
  int32_t pad_rank ;
  int32_t shp_rank ;
  int32_t res_rank ;
  int32_t src_rank = GET_RANK_FROM_DESC(array) - 1;

  size_t typ_sz   = GET_ELEMENT_SZ_FROM_DESC(array);

  size_t a_size,a_stride,r_stride, i,k ;
  size_t l_size,l_stride;
  size_t tot_shape, tot_source;
  char *   l_p ;
  int8_t  zero_szd_shape = FALSE;
  int8_t  zero_szd_order = FALSE;
  int8_t  zero_szd_source = FALSE;
  int8_t  zero_szd_pad = FALSE;
  int8_t  byte_aligned = FALSE;

  int32_t ddim ;

  size_t  num_trues ;
  int32_t local_alloc ;
  size_t  tot_ext ;
  size_t  str_sz  ;

  size_t src_size ;

  size_t  res_sz;
  size_t  xfer_sz;
  size_t  tot_sz;

  tot_source = 1 ;
  for( j = 0 ; j <= src_rank ; j ++  ) {
    src_extent[j]  = GET_EXTENT_FROM_DESC(array,j) ;
    src_stride[j]  = GET_STRIDE_FROM_DESC(array,j) ;
    counter[j] = 0 ;
    zero_szd_source = zero_szd_source || (src_extent[j] == 0) ;
    tot_source *= src_extent[j];
  }

  for ( j = 1 ; j <= src_rank ; j ++  )
    src_offset[j-1] = src_stride[j] - (src_stride [j-1] * (src_extent[j-1]))  ;

  byte_aligned = GET_BYTEALIGNED_FROM_DESC(array) ;

  res_rank = GET_EXTENT_FROM_DESC(shape,0) ;
  if (res_rank == 0)
    ERROR(_LELVL_ABORT, FESHPSZZ);

  l_stride = GET_STRIDE_FROM_DESC(shape,0);
  l_size   = GET_ELEMENT_SZ_FROM_DESC(shape);
  l_p      = GET_ADDRESS_FROM_DESC(shape);

  switch (l_size) {
  case 1:
    for (j = 0 ; j < res_rank ; j++ ) {
      l_shape[j] = * (int8_t *) l_p ;
      l_p += l_stride;
    }
    break;

  case 2:
    for (j = 0 ; j < res_rank ; j++ ) {
      l_shape[j] = * (int16_t *) l_p ;
      l_p += l_stride;
    }
    break;

  case 4:
    for (j = 0 ; j < res_rank ; j++ ) {
      l_shape[j] = * (int32_t *) l_p ;
      l_p += l_stride;
    }
    break;

  case 8:
    for (j = 0 ; j < res_rank ; j++ ) {
      l_shape[j] = * (int64_t *) l_p ;
      l_p += l_stride;
    }
    break;
  }

  tot_shape = 1;
  for (i = 0; i < res_rank; i++) {
    if (l_shape[i] < 0)
      ERROR (_LELVL_ABORT, FERSHNEG);
    zero_szd_shape = zero_szd_shape || (l_shape[i] == 0);
    tot_shape *= l_shape[i];

  }

  if (order == NULL) {
    for (j = 0 ; j < res_rank ; j++ )
      l_order[j] = j + 1;

  } else {

    l_stride = GET_STRIDE_FROM_DESC(order,0);
    l_size   = GET_ELEMENT_SZ_FROM_DESC(order);
    l_p      = GET_ADDRESS_FROM_DESC(order);

    switch (l_size) {
    case 1:
      for (j = 0 ; j < res_rank ; j++ ) {
	l_order_chk[j] = FALSE;
	l_order[j] = * (int8_t *) l_p ;
	l_p += l_stride;
      }
      break;

    case 2:
      for (j = 0 ; j < res_rank ; j++ ) {
	l_order_chk[j] = FALSE;
	l_order[j] = * (int16_t *) l_p ;
	l_p += l_stride;
      }
      break;

    case 4:
      for (j = 0 ; j < res_rank ; j++ ) {
	l_order_chk[j] = FALSE;
	l_order[j] = * (int32_t *) l_p ;
	l_p += l_stride;
      }
      break;

    case 8:
      for (j = 0 ; j < res_rank ; j++ ) {
	l_order_chk[j] = FALSE;
	l_order[j] = * (int64_t *) l_p ;
	l_p += l_stride;
      }
      break;
    }

    for (i = 0; i < res_rank; i++) {
      if (l_order[i] <= 0 || l_order[i] > res_rank)
	ERROR(_LELVL_ABORT, FEBDORDR);
      l_order_chk[l_order[i]-1] = TRUE;
      zero_szd_order = zero_szd_order || (l_order[i] == 0) ;
    }
    for (i = 0; i < res_rank; i++) {
      if (!l_order_chk[i])
	ERROR(_LELVL_ABORT, FEBDORDR);
    }
  }

  if (pad != NULL ) {
    pad_p = GET_ADDRESS_FROM_DESC(pad);
    pad_rank = GET_RANK_FROM_DESC(pad) - 1;
    for ( j = 0 ; j <= pad_rank ; j ++  ) {
      pad_extent[j]  = GET_EXTENT_FROM_DESC(pad,j) ;
      pad_stride[j]  = GET_STRIDE_FROM_DESC(pad,j) ;
      zero_szd_pad   = zero_szd_pad || (pad_extent[j] == 0) ;

    }
    for ( j = 1 ; j <= pad_rank ; j ++  )
      pad_offset[j-1] = pad_stride[j] - (pad_stride [j-1] * (pad_extent[j-1]))  ;

  } else if (tot_shape > tot_source) {
    ERROR(_LELVL_ABORT, FERSHNPD);
  }

  if (!GET_ASSOCIATED_FROM_DESC(result)) {

    size_t  nbytes  ;
    char    *p      ;

    SET_ADDRESS_IN_DESC(result,NULL);
    SET_ORIG_BS_IN_DESC(result,NULL) ;
    SET_ORIG_SZ_IN_DESC(result,0) ;
    SET_RANK_IN_DESC(result,res_rank) ;

    p = NULL ;
    tot_ext = 1 ;
    nbytes  = typ_sz ;
    str_sz  = MK_STRIDE(byte_aligned,typ_sz);

    for ( i = 0 ; i < res_rank ; i ++) {
      SET_LBOUND_IN_DESC(result,i,1);
      SET_EXTENT_IN_DESC(result,i,l_shape[i]);
      SET_STRMULT_IN_DESC(result,i,tot_ext * str_sz );
      tot_ext *= l_shape[i];
      nbytes  *= l_shape[i];
    }

    if (nbytes > 0 && !zero_szd_order) {
      p = (void *) malloc (nbytes);
      if (p == NULL)
	ERROR(_LELVL_ABORT, FENOMEMY);

      SET_ADDRESS_IN_DESC(result,p);
    }

    SET_ASSOCIATED_IN_DESC(result);
    SET_CONTIG_IN_DESC(result);
    SET_ALEN_IN_DESC(result,GET_ALEN_FROM_DESC(array));
    if (GET_DV_ASCII_FROM_DESC(array)) {
      SET_CHARPTR_IN_DESC(result,p,typ_sz);
    }
    SET_ORIG_BS_IN_DESC(result,p) ;
    SET_ORIG_SZ_IN_DESC(result,nbytes*8) ;
  }

  if (zero_szd_shape || zero_szd_order)
    return ;

  if (zero_szd_source && (pad == NULL || zero_szd_pad))
    ERROR(_LELVL_ABORT, FERSHNPD);

  for ( j = 0 , gp1 = l_order ; j < res_rank ; j ++  ) {
    if (gp1 == NULL)
      ii = j ;
    else 
      ii = (*gp1++)-1  ;

    res_stride[j]  = GET_STRIDE_FROM_DESC(result,ii) ;
    res_extent[j]  = GET_EXTENT_FROM_DESC(result,ii) ;
    res_counter[j] = 0 ;
  }

  for ( j = 1 ; j < res_rank ; j ++  )
    res_offset[j-1] = res_stride[j] - (res_stride [j-1] * (res_extent[j-1]))  ;
  res_rank -- ;
  if (zero_szd_source)
    if (pad != NULL)
      for (i = 0 ; i <= src_rank ; i ++) src_extent[i] = 0;
    else
      return ;

  a_size   = src_extent[0] ;
  a_stride = src_stride[0] ;
  r_stride = res_stride[0] ;
  array_p = GET_ADDRESS_FROM_DESC(array);
  result_p = GET_ADDRESS_FROM_DESC(result);

  if (typ_sz == sizeof(i1) && ALIGNED_i1(array_p) &&  ALIGNED_i1(result_p) &&  ((pad_p == NULL) || ALIGNED_i1(pad_p))) {

    for (;;) {
      while (counter[src_rank] < src_extent[src_rank] ) {
	for ( i = 0 ; i < a_size ; i ++ ) {
	  *(i1 *)result_p = *(i1 *)array_p ;
	  array_p += a_stride ;
	  result_p += r_stride ;

	  j = 0 ;
	  res_counter[0] ++ ;

	  while (res_counter[j] == res_extent[j]) {
	    if (j == res_rank ) return ;
	    result_p += res_offset[j] ;
	    res_counter[j+1]++ ;
	    res_counter[j] = 0 ;
	    j ++ ;
	  }
	}
	counter[0] = a_size  ;
	j = 0 ;
	while ((counter[j] == src_extent[j]) && (j < src_rank)) {
	  array_p += src_offset[j] ;
	  counter[j+1]++ ;
	  counter[j] = 0 ;
	  j ++ ;
	}

      }
      if (pad != NULL)  {

	src_rank = pad_rank ;

	for ( j = 0 ; j <= src_rank ; j ++  ) {
	  src_extent [j] = pad_extent[j] ;
	  src_stride [j] = pad_stride[j] ;
	  counter[j] =  0 ;
	  src_offset [j] = pad_offset[j] ;
	}
	array_p = pad_p ;
	a_size    = src_extent [0]  ;
	a_stride  = src_stride [0]  ;
      }
    }
  } else if (typ_sz == sizeof(i2) && ALIGNED_i2(array_p) &&  ALIGNED_i2(result_p) && ((pad_p == NULL) || ALIGNED_i2(pad_p))) {

    for (;;) {
      while (counter[src_rank] < src_extent[src_rank] ) {
	for ( i = 0 ; i < a_size ; i ++ ) {
	  *(i2 *)result_p = *(i2 *)array_p ;
	  array_p += a_stride ;
	  result_p += r_stride ;

	  j = 0 ;
	  res_counter[0] ++ ;

	  while (res_counter[j] == res_extent[j]) {
	    if (j == res_rank ) return ;
	    result_p += res_offset[j] ;
	    res_counter[j+1]++ ;
	    res_counter[j] = 0 ;
	    j ++ ;
	  }
	}
	counter[0] = a_size  ;
	j = 0 ;
	while ((counter[j] == src_extent[j]) && (j < src_rank)) {
	  array_p += src_offset[j] ;
	  counter[j+1]++ ;
	  counter[j] = 0 ;
	  j ++ ;
	}

      }
      if (pad != NULL)  {

	src_rank = pad_rank ;

	for ( j = 0 ; j <= src_rank ; j ++  ) {
	  src_extent [j] = pad_extent[j] ;
	  src_stride [j] = pad_stride[j] ;
	  counter[j] =  0 ;
	  src_offset [j] = pad_offset[j] ;
	}
	array_p = pad_p ;
	a_size    = src_extent [0]  ;
	a_stride  = src_stride [0]  ;
      }
    }
  } else if (typ_sz == sizeof(r4) && ALIGNED_r4(array_p) &&  ALIGNED_r4(result_p) && ((pad_p == NULL) || ALIGNED_r4(pad_p))) {

    for (;;) {
      while (counter[src_rank] < src_extent[src_rank] ) {
	for ( i = 0 ; i < a_size ; i ++ ) {
	  *(r4 *)result_p = *(r4 *)array_p ;
	  array_p += a_stride ;
	  result_p += r_stride ;

	  j = 0 ;
	  res_counter[0] ++ ;

	  while (res_counter[j] == res_extent[j]) {
	    if (j == res_rank ) return ;
	    result_p += res_offset[j] ;
	    res_counter[j+1]++ ;
	    res_counter[j] = 0 ;
	    j ++ ;
	  }
	}
	counter[0] = a_size  ;
	j = 0 ;
	while ((counter[j] == src_extent[j]) && (j < src_rank)) {
	  array_p += src_offset[j] ;
	  counter[j+1]++ ;
	  counter[j] = 0 ;
	  j ++ ;
	}

      }
      if (pad != NULL)  {

	src_rank = pad_rank ;

	for ( j = 0 ; j <= src_rank ; j ++  ) {
	  src_extent [j] = pad_extent[j] ;
	  src_stride [j] = pad_stride[j] ;
	  counter[j] =  0 ;
	  src_offset [j] = pad_offset[j] ;
	}
	array_p = pad_p ;
	a_size    = src_extent [0]  ;
	a_stride  = src_stride [0]  ;
      }
    }
  } else if (typ_sz == sizeof(r8) && ALIGNED_r8(array_p) &&  ALIGNED_r8(result_p) && ((pad_p == NULL) || ALIGNED_r8(pad_p))) {

    for (;;) {
      while (counter[src_rank] < src_extent[src_rank] ) {
	for ( i = 0 ; i < a_size ; i ++ ) {
	  *(r8 *)result_p = *(r8 *)array_p ;
	  array_p += a_stride ;
	  result_p += r_stride ;

	  j = 0 ;
	  res_counter[0] ++ ;

	  while (res_counter[j] == res_extent[j]) {
	    if (j == res_rank ) return ;
	    result_p += res_offset[j] ;
	    res_counter[j+1]++ ;
	    res_counter[j] = 0 ;
	    j ++ ;
	  }
	}
	counter[0] = a_size  ;
	j = 0 ;
	while ((counter[j] == src_extent[j]) && (j < src_rank)) {
	  array_p += src_offset[j] ;
	  counter[j+1]++ ;
	  counter[j] = 0 ;
	  j ++ ;
	}

      }
      if (pad != NULL)  {

	src_rank = pad_rank ;

	for ( j = 0 ; j <= src_rank ; j ++  ) {
	  src_extent [j] = pad_extent[j] ;
	  src_stride [j] = pad_stride[j] ;
	  counter[j] =  0 ;
	  src_offset [j] = pad_offset[j] ;
	}
	array_p = pad_p ;
	a_size    = src_extent [0]  ;
	a_stride  = src_stride [0]  ;
      }
    }
  } else if (typ_sz == sizeof(r16) && ALIGNED_r16(array_p) &&  ALIGNED_r16(result_p) && ((pad_p == NULL) || ALIGNED_r16(pad_p))) {

    for (;;) {
      while (counter[src_rank] < src_extent[src_rank] ) {
	for ( i = 0 ; i < a_size ; i ++ ) {
	  *(r16 *)result_p = *(r16 *)array_p ;
	  array_p += a_stride ;
	  result_p += r_stride ;

	  j = 0 ;
	  res_counter[0] ++ ;

	  while (res_counter[j] == res_extent[j]) {
	    if (j == res_rank ) return ;
	    result_p += res_offset[j] ;
	    res_counter[j+1]++ ;
	    res_counter[j] = 0 ;
	    j ++ ;
	  }
	}
	counter[0] = a_size  ;
	j = 0 ;
	while ((counter[j] == src_extent[j]) && (j < src_rank)) {
	  array_p += src_offset[j] ;
	  counter[j+1]++ ;
	  counter[j] = 0 ;
	  j ++ ;
	}

      }
      if (pad != NULL)  {

	src_rank = pad_rank ;

	for ( j = 0 ; j <= src_rank ; j ++  ) {
	  src_extent [j] = pad_extent[j] ;
	  src_stride [j] = pad_stride[j] ;
	  counter[j] =  0 ;
	  src_offset [j] = pad_offset[j] ;
	}
	array_p = pad_p ;
	a_size    = src_extent [0]  ;
	a_stride  = src_stride [0]  ;
      }
    }
  } else {
    for (;;) {
      while (counter[src_rank] < src_extent[src_rank] ) {
	for ( i = 0 ; i < a_size ; i ++ ) {
	  ap = array_p ;
	  rp = result_p ;
	  if (typ_sz > BIGDEFAULTSZ)
	    (void) memcpy (rp, ap, typ_sz);
	  else
	    for (j = 0 ; j < typ_sz ; j ++)  *rp++ = *ap ++ ;
	  array_p += a_stride ;
	  result_p += r_stride ;

	  j = 0 ;
	  res_counter[0] ++ ;

	  while (res_counter[j] == res_extent[j]) {
	    if (j == res_rank ) return ;
	    result_p += res_offset[j] ;
	    res_counter[j+1]++ ;
	    res_counter[j] = 0 ;
	    j ++ ;
	  }
	}
	counter[0] = a_size  ;
	j = 0 ;
	while ((counter[j] == src_extent[j]) && (j < src_rank)) {
	  array_p += src_offset[j] ;
	  counter[j+1]++ ;
	  counter[j] = 0 ;
	  j ++ ;
	}

      }
      if (pad != NULL)  {

	src_rank = pad_rank ;

	for ( j = 0 ; j <= src_rank ; j ++  ) {
	  src_extent [j] = pad_extent[j] ;
	  src_stride [j] = pad_stride[j] ;
	  counter[j] =  0 ;
	  src_offset [j] = pad_offset[j] ;
	}
	array_p = pad_p ;
	a_size    = src_extent [0]  ;
	a_stride  = src_stride [0]  ;
      }
    }
  }
}
