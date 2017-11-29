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

static size_t read_source_desc(DopeVectorType	* array,
size_t src_extent[MAX_NARY_DIMS],
size_t src_stride[MAX_NARY_DIMS],
size_t src_offset[MAX_NARY_DIMS],
int32_t ddim)  ;

static void
get_offset_and_stride(DopeVectorType	* array,
size_t src_extent[MAX_NARY_DIMS],
size_t src_stride[MAX_NARY_DIMS],
size_t src_offset[MAX_NARY_DIMS],
int32_t ddim) ;

static void alloc_res(DopeVectorType	* result, 
size_t src_extent[MAX_NARY_DIMS]);

void 
_ALL_1(
DopeVectorType	*result,
DopeVectorType	*array,
i4 *dim)
{
  char  * result_p, * result_b ;
  char  * array_p, * array_b ;

  size_t src_extent [MAX_NARY_DIMS] ;
  size_t counter    [MAX_NARY_DIMS] ;
  size_t src_offset [MAX_NARY_DIMS] ;
  size_t src_stride [MAX_NARY_DIMS] ;
  size_t src_size ;

  size_t res_stride [MAX_NARY_DIMS] ;
  size_t res_offset [MAX_NARY_DIMS] ;

  int32_t ddim ;
  uint32_t src_rank ;
  uint32_t res_rank ;

  size_t j,k,i ;

  l1 accum ;
  l1 const initv = 1 ;
  size_t a_size,a_stride;

  l1 temp,new ;

  if (dim != NULL) {
    ddim = (* dim) - 1 ;
  } else 
    ddim = 0 ;

  array_b  = (char *) GET_ADDRESS_FROM_DESC(array) ;
  src_rank = GET_RANK_FROM_DESC(array) - 1;

  src_size = read_source_desc(array,	 src_extent, src_stride, src_offset, ddim);

  for (i = 0 ; i <= src_rank ; i ++)
    counter[i] = 0 ;

  if ((ddim > src_rank ) || (ddim < 0))
    ERROR(_LELVL_ABORT,FESCIDIM);

  res_rank = GET_RANK_FROM_DESC(result);

  if (!GET_ASSOCIATED_FROM_DESC(result))  {
    alloc_res(result,src_extent);
  }

  res_stride[0] = 0;
  for (j = 0 ; j <= src_rank; j ++ ) res_offset[j] = 0 ;
  for (j = 0 ; j < res_rank ; j ++ ) {
    res_stride[j] = GET_STRIDE_FROM_DESC(result,j) ;
  }

  res_offset[0] = res_stride[0] ;
  for ( j = 1 ; j < res_rank ; j ++ )
    res_offset[j] = res_stride[j] - (res_stride[j-1]*(src_extent[j])) ;

  result_b = GET_ADDRESS_FROM_DESC(result);

  accum = initv ;

  if (src_size == 0 ) {
    for (i = 1 ; i <= src_rank ; i ++ )
      if (src_extent[i] == 0)
	return ;
  }
  array_p = array_b ;
  result_p = result_b ;

  a_size   = src_extent[0] ;
  a_stride = src_stride[0] ;

  while (counter[src_rank] < src_extent[src_rank] ) {

    if(res_rank != 0) accum = initv ;

    for ( i = 0 ; i < a_size ; i ++ ) {
      accum &= *(l1 *)array_p ;

      array_p += a_stride ;
    }
    *(l1 *) result_p = accum ;
    counter[0] = a_size  ;
    j = 0 ;
    while ((counter[j] == src_extent[j]) && (j < src_rank)) {
      array_p += src_offset[j] ;
      result_p += res_offset[j] ;
      counter[j+1]++ ;
      counter[j] = 0 ;
      j ++ ;
    }
  }
}
void 
_ALL_2(
DopeVectorType	*result,
DopeVectorType	*array,
i4 *dim)
{
  char  * result_p, * result_b ;
  char  * array_p, * array_b ;

  size_t src_extent [MAX_NARY_DIMS] ;
  size_t counter    [MAX_NARY_DIMS] ;
  size_t src_offset [MAX_NARY_DIMS] ;
  size_t src_stride [MAX_NARY_DIMS] ;
  size_t src_size ;

  size_t res_stride [MAX_NARY_DIMS] ;
  size_t res_offset [MAX_NARY_DIMS] ;

  int32_t ddim ;
  uint32_t src_rank ;
  uint32_t res_rank ;

  size_t j,k,i ;

  l2 accum ;
  l2 const initv = 1 ;
  size_t a_size,a_stride;

  l2 temp,new ;

  if (dim != NULL) {
    ddim = (* dim) - 1 ;
  } else 
    ddim = 0 ;

  array_b  = (char *) GET_ADDRESS_FROM_DESC(array) ;
  src_rank = GET_RANK_FROM_DESC(array) - 1;

  src_size = read_source_desc(array,	 src_extent, src_stride, src_offset, ddim);

  for (i = 0 ; i <= src_rank ; i ++)
    counter[i] = 0 ;

  if ((ddim > src_rank ) || (ddim < 0))
    ERROR(_LELVL_ABORT,FESCIDIM);

  res_rank = GET_RANK_FROM_DESC(result);

  if (!GET_ASSOCIATED_FROM_DESC(result))  {
    alloc_res(result,src_extent);
  }

  res_stride[0] = 0;
  for (j = 0 ; j <= src_rank; j ++ ) res_offset[j] = 0 ;
  for (j = 0 ; j < res_rank ; j ++ ) {
    res_stride[j] = GET_STRIDE_FROM_DESC(result,j) ;
  }

  res_offset[0] = res_stride[0] ;
  for ( j = 1 ; j < res_rank ; j ++ )
    res_offset[j] = res_stride[j] - (res_stride[j-1]*(src_extent[j])) ;

  result_b = GET_ADDRESS_FROM_DESC(result);

  accum = initv ;

  if (src_size == 0 ) {
    for (i = 1 ; i <= src_rank ; i ++ )
      if (src_extent[i] == 0)
	return ;
  }
  array_p = array_b ;
  result_p = result_b ;

  a_size   = src_extent[0] ;
  a_stride = src_stride[0] ;

  while (counter[src_rank] < src_extent[src_rank] ) {

    if(res_rank != 0) accum = initv ;

    for ( i = 0 ; i < a_size ; i ++ ) {
      accum &= *(l2 *)array_p ;

      array_p += a_stride ;
    }
    *(l2 *) result_p = accum ;
    counter[0] = a_size  ;
    j = 0 ;
    while ((counter[j] == src_extent[j]) && (j < src_rank)) {
      array_p += src_offset[j] ;
      result_p += res_offset[j] ;
      counter[j+1]++ ;
      counter[j] = 0 ;
      j ++ ;
    }
  }
}
void 
_ALL_4(
DopeVectorType	*result,
DopeVectorType	*array,
i4 *dim)
{
  char  * result_p, * result_b ;
  char  * array_p, * array_b ;

  size_t src_extent [MAX_NARY_DIMS] ;
  size_t counter    [MAX_NARY_DIMS] ;
  size_t src_offset [MAX_NARY_DIMS] ;
  size_t src_stride [MAX_NARY_DIMS] ;
  size_t src_size ;

  size_t res_stride [MAX_NARY_DIMS] ;
  size_t res_offset [MAX_NARY_DIMS] ;

  int32_t ddim ;
  uint32_t src_rank ;
  uint32_t res_rank ;

  size_t j,k,i ;

  l4 accum ;
  l4 const initv = 1 ;
  size_t a_size,a_stride;

  l4 temp,new ;

  if (dim != NULL) {
    ddim = (* dim) - 1 ;
  } else 
    ddim = 0 ;

  array_b  = (char *) GET_ADDRESS_FROM_DESC(array) ;
  src_rank = GET_RANK_FROM_DESC(array) - 1;

  src_size = read_source_desc(array,	 src_extent, src_stride, src_offset, ddim);

  for (i = 0 ; i <= src_rank ; i ++)
    counter[i] = 0 ;

  if ((ddim > src_rank ) || (ddim < 0))
    ERROR(_LELVL_ABORT,FESCIDIM);

  res_rank = GET_RANK_FROM_DESC(result);

  if (!GET_ASSOCIATED_FROM_DESC(result))  {
    alloc_res(result,src_extent);
  }

  res_stride[0] = 0;
  for (j = 0 ; j <= src_rank; j ++ ) res_offset[j] = 0 ;
  for (j = 0 ; j < res_rank ; j ++ ) {
    res_stride[j] = GET_STRIDE_FROM_DESC(result,j) ;
  }

  res_offset[0] = res_stride[0] ;
  for ( j = 1 ; j < res_rank ; j ++ )
    res_offset[j] = res_stride[j] - (res_stride[j-1]*(src_extent[j])) ;

  result_b = GET_ADDRESS_FROM_DESC(result);

  accum = initv ;

  if (src_size == 0 ) {
    for (i = 1 ; i <= src_rank ; i ++ )
      if (src_extent[i] == 0)
	return ;
  }
  array_p = array_b ;
  result_p = result_b ;

  a_size   = src_extent[0] ;
  a_stride = src_stride[0] ;

  while (counter[src_rank] < src_extent[src_rank] ) {

    if(res_rank != 0) accum = initv ;

    for ( i = 0 ; i < a_size ; i ++ ) {
      accum &= *(l4 *)array_p ;

      array_p += a_stride ;
    }
    *(l4 *) result_p = accum ;
    counter[0] = a_size  ;
    j = 0 ;
    while ((counter[j] == src_extent[j]) && (j < src_rank)) {
      array_p += src_offset[j] ;
      result_p += res_offset[j] ;
      counter[j+1]++ ;
      counter[j] = 0 ;
      j ++ ;
    }
  }
}
void 
_ALL(
DopeVectorType	*result,
DopeVectorType	*array,
i4 *dim)
{
  char  * result_p, * result_b ;
  char  * array_p, * array_b ;

  size_t src_extent [MAX_NARY_DIMS] ;
  size_t counter    [MAX_NARY_DIMS] ;
  size_t src_offset [MAX_NARY_DIMS] ;
  size_t src_stride [MAX_NARY_DIMS] ;
  size_t src_size ;

  size_t res_stride [MAX_NARY_DIMS] ;
  size_t res_offset [MAX_NARY_DIMS] ;

  int32_t ddim ;
  uint32_t src_rank ;
  uint32_t res_rank ;

  size_t j,k,i ;

  l8 accum ;
  l8 const initv = 1 ;
  size_t a_size,a_stride;

  l8 temp,new ;

  if (dim != NULL) {
    ddim = (* dim) - 1 ;
  } else 
    ddim = 0 ;

  array_b  = (char *) GET_ADDRESS_FROM_DESC(array) ;
  src_rank = GET_RANK_FROM_DESC(array) - 1;

  src_size = read_source_desc(array,	 src_extent, src_stride, src_offset, ddim);

  for (i = 0 ; i <= src_rank ; i ++)
    counter[i] = 0 ;

  if ((ddim > src_rank ) || (ddim < 0))
    ERROR(_LELVL_ABORT,FESCIDIM);

  res_rank = GET_RANK_FROM_DESC(result);

  if (!GET_ASSOCIATED_FROM_DESC(result))  {
    alloc_res(result,src_extent);
  }

  res_stride[0] = 0;
  for (j = 0 ; j <= src_rank; j ++ ) res_offset[j] = 0 ;
  for (j = 0 ; j < res_rank ; j ++ ) {
    res_stride[j] = GET_STRIDE_FROM_DESC(result,j) ;
  }

  res_offset[0] = res_stride[0] ;
  for ( j = 1 ; j < res_rank ; j ++ )
    res_offset[j] = res_stride[j] - (res_stride[j-1]*(src_extent[j])) ;

  result_b = GET_ADDRESS_FROM_DESC(result);

  accum = initv ;

  if (src_size == 0 ) {
    for (i = 1 ; i <= src_rank ; i ++ )
      if (src_extent[i] == 0)
	return ;
  }
  array_p = array_b ;
  result_p = result_b ;

  a_size   = src_extent[0] ;
  a_stride = src_stride[0] ;

  while (counter[src_rank] < src_extent[src_rank] ) {

    if(res_rank != 0) accum = initv ;

    for ( i = 0 ; i < a_size ; i ++ ) {
      accum &= *(l8 *)array_p ;

      array_p += a_stride ;
    }
    *(l8 *) result_p = accum ;
    counter[0] = a_size  ;
    j = 0 ;
    while ((counter[j] == src_extent[j]) && (j < src_rank)) {
      array_p += src_offset[j] ;
      result_p += res_offset[j] ;
      counter[j+1]++ ;
      counter[j] = 0 ;
      j ++ ;
    }
  }
}   
static void
alloc_res(DopeVectorType	* result, 
size_t src_extent[MAX_NARY_DIMS])
{
  size_t  tot_ext  ;
  size_t  str_sz   ;
  size_t  nbytes   ;
  size_t  esz      ;
  int32_t res_rank ;
  char   *p = NULL ;
  int32_t i        ;

  SET_ADDRESS_IN_DESC(result,NULL);
  SET_ORIG_BS_IN_DESC(result,NULL) ;
  SET_ORIG_SZ_IN_DESC(result,0) ;

  res_rank = GET_RANK_FROM_DESC(result);
  tot_ext  = 1 ;
  esz      = GET_ALEN_FROM_DESC(result) >> 3 ;
  nbytes   = esz ;
  str_sz   = MK_STRIDE(FALSE,esz);

  for ( i = 0 ; i < res_rank ; i ++) {
    SET_LBOUND_IN_DESC(result,i,1);
    SET_EXTENT_IN_DESC(result,i,src_extent[i+1]);
    SET_STRMULT_IN_DESC(result,i,tot_ext * str_sz );
    tot_ext *= src_extent[i+1] ;
  }
  nbytes *= tot_ext;
  if (nbytes > 0 ) {
    p = (void *) malloc (nbytes);
    if (p == NULL)
      ERROR(_LELVL_ABORT, FENOMEMY);

    SET_ADDRESS_IN_DESC(result,p);
  }
  SET_ASSOCIATED_IN_DESC(result);
  SET_CONTIG_IN_DESC(result);
  SET_ORIG_BS_IN_DESC(result,p) ;
  SET_ORIG_SZ_IN_DESC(result,nbytes * 8) ;
}

static size_t
read_source_desc(DopeVectorType	* array,
size_t src_extent[MAX_NARY_DIMS],
size_t src_stride[MAX_NARY_DIMS],
size_t src_offset[MAX_NARY_DIMS],
int32_t ddim)
{
  int32_t src_rank ,k,j ;
  size_t  src_size ;

  src_extent[0] = GET_EXTENT_FROM_DESC(array,ddim) ;
  src_rank      = GET_RANK_FROM_DESC(array);

  src_size = src_extent[0];

  for ( k = 1, j = 0  ; j < src_rank ; j ++  ) {
    if (j != ddim ) {
      src_extent[k] = GET_EXTENT_FROM_DESC(array,j) ;
      src_size *= src_extent[k];
      k++ ;
    }
  }
  get_offset_and_stride(array, src_extent, src_stride, src_offset, ddim);

  return src_size;
}

static void
get_offset_and_stride(DopeVectorType	* array,
size_t src_extent[MAX_NARY_DIMS],
size_t src_stride[MAX_NARY_DIMS],
size_t src_offset[MAX_NARY_DIMS],
int32_t ddim)
{

  int32_t src_rank ,k,j ;

  src_stride[0] = GET_STRIDE_FROM_DESC(array,ddim) ;
  src_offset[0] = 0;
  src_rank      = GET_RANK_FROM_DESC(array);

  for ( k = 1, j = 0  ; j < src_rank ; j ++  ) {
    if (j != ddim ) {
      src_stride[k] = GET_STRIDE_FROM_DESC(array,j) ;
      src_offset[k-1] = src_stride[k] - (src_stride [k-1] * (src_extent[k-1]))  ;
      k++ ;
    }
  }
}
