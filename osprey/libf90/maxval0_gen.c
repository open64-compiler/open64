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

static uint32_t
find_contig_axes(size_t src_extent[MAX_NARY_DIMS],
size_t src_stride[MAX_NARY_DIMS],
size_t src_offset[MAX_NARY_DIMS],
uint32_t src_rank,
size_t typ_sz,
size_t msk_stride[MAX_NARY_DIMS],
size_t msk_offset[MAX_NARY_DIMS],
size_t msk_typ_sz) ;

static int32_t read_dim(DopeVectorType	* dim) ;

i1 
_MAXVAL0__I1(
DopeVectorType	*array,
DopeVectorType	*dim,
DopeVectorType	*mask)
{
  char  * array_p, * array_b ;
  char * dim_p, * dim_b ;
  char * mask_p, * mask_b ;

  size_t src_extent [MAX_NARY_DIMS] ;
  size_t counter    [MAX_NARY_DIMS] ;
  size_t src_offset [MAX_NARY_DIMS] ;
  size_t src_stride [MAX_NARY_DIMS] ;
  size_t src_size ;

  size_t msk_stride [MAX_NARY_DIMS] ;
  size_t msk_offset [MAX_NARY_DIMS] ;

  int32_t ddim ;
  uint32_t src_rank ;
  uint32_t res_rank ;
  uint32_t jrank ;

  size_t j,k,i ;
  size_t typ_sz;
  size_t msk_typ_sz;

  i1 accum ;
  i1 const initv = INT8_MIN ;
  size_t a_size,a_stride;
  size_t m_stride ;

  i1 temp,new ;

  if (mask == NULL) {   /* is third arg mask or dim? */
    if (dim != NULL) {
      if (GET_DV_LOGICAL_FROM_DESC(dim)) {
	mask = (DopeVectorType	*) dim ;
	dim = NULL;
      }
    }
  }

  if (dim != NULL) {
    ddim = read_dim(dim);
  } else 
    ddim = 0 ;

  array_b  = (char *) GET_ADDRESS_FROM_DESC(array) ;
  src_rank = GET_RANK_FROM_DESC(array) - 1;
  typ_sz   = GET_ELEMENT_SZ_FROM_DESC(array);

  src_size = read_source_desc(array,	 src_extent, src_stride, src_offset, ddim);

  for (i = 0 ; i <= src_rank ; i ++)
    counter[i] = 0 ;

  if (mask != NULL) {

    msk_typ_sz = GET_ELEMENT_SZ_FROM_DESC(mask);
    mask_b     = (char *) GET_ADDRESS_FROM_DESC(mask) + OFFSET_TO_TF_BYTE(msk_typ_sz) ;

    if (GET_RANK_FROM_DESC(mask) == 0) {
      if (*mask_b) {
	mask = NULL;
      } else {
	src_size = 0;
	for (j = 0 ; j <= src_rank ; j ++) {
	  msk_stride[j] = 0 ;
	  msk_offset[j] = 0 ;
	}
      }

    } else {

      get_offset_and_stride(mask, src_extent, msk_stride,                  msk_offset,  ddim);
    }
  }

  accum = initv ;

  if (src_size == 0 ) {
    return accum;
  }
  array_p = array_b ;
  if (mask == NULL) {

    {
      size_t *p1 = NULL;
      size_t *p2 = NULL;
      size_t p3  = 0;

      src_rank = find_contig_axes(src_extent, src_stride, src_offset, src_rank, typ_sz, p1,p2,p3) ;
    }

    a_size   = src_extent[0] ;
    a_stride = src_stride[0] ;

    while (counter[src_rank] < src_extent[src_rank] ) {

      for ( i = 0 ; i < a_size ; i ++ ) {
	if (*(i1 *)array_p > accum ) {
	  accum = * (i1 *)array_p ;
	}         
	array_p += a_stride ;
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
  } else {

    {
      size_t *p1 = NULL;
      size_t *p2 = NULL;
      size_t p3  = 0;

      p1 =  msk_stride ;
      p2 =  msk_offset ;
      p3 =  msk_typ_sz ;

      src_rank = find_contig_axes(src_extent, src_stride, src_offset, src_rank, typ_sz, p1,p2,p3) ;
    }

    a_size   = src_extent[0] ;
    a_stride = src_stride[0] ;
    m_stride = msk_stride[0] ;
    mask_p   = mask_b   ;

    while (counter[src_rank] < src_extent[src_rank] ) {

      for ( i = 0 ; i < a_size ; i ++ ) {
	if (*mask_p) {
	  if (*(i1 *)array_p > accum ) {
	    accum = * (i1 *)array_p ;
	  }          
	}
	array_p += a_stride ;
	mask_p += m_stride ;
      }
      counter[0] = a_size  ;
      j = 0 ;
      while ((counter[j] == src_extent[j]) && (j < src_rank)) {
	array_p += src_offset[j] ;
	mask_p += msk_offset[j] ;
	counter[j+1]++ ;
	counter[j] = 0 ;
	j ++ ;
      }
    }
  }
  return accum ;
}
i2 
_MAXVAL0__I2(
DopeVectorType	*array,
DopeVectorType	*dim,
DopeVectorType	*mask)
{
  char  * array_p, * array_b ;
  char * dim_p, * dim_b ;
  char * mask_p, * mask_b ;

  size_t src_extent [MAX_NARY_DIMS] ;
  size_t counter    [MAX_NARY_DIMS] ;
  size_t src_offset [MAX_NARY_DIMS] ;
  size_t src_stride [MAX_NARY_DIMS] ;
  size_t src_size ;

  size_t msk_stride [MAX_NARY_DIMS] ;
  size_t msk_offset [MAX_NARY_DIMS] ;

  int32_t ddim ;
  uint32_t src_rank ;
  uint32_t res_rank ;
  uint32_t jrank ;

  size_t j,k,i ;
  size_t typ_sz;
  size_t msk_typ_sz;

  i2 accum ;
  i2 const initv = INT16_MIN ;
  size_t a_size,a_stride;
  size_t m_stride ;

  i2 temp,new ;

  if (mask == NULL) {   /* is third arg mask or dim? */
    if (dim != NULL) {
      if (GET_DV_LOGICAL_FROM_DESC(dim)) {
	mask = (DopeVectorType	*) dim ;
	dim = NULL;
      }
    }
  }

  if (dim != NULL) {
    ddim = read_dim(dim);
  } else 
    ddim = 0 ;

  array_b  = (char *) GET_ADDRESS_FROM_DESC(array) ;
  src_rank = GET_RANK_FROM_DESC(array) - 1;
  typ_sz   = GET_ELEMENT_SZ_FROM_DESC(array);

  src_size = read_source_desc(array,	 src_extent, src_stride, src_offset, ddim);

  for (i = 0 ; i <= src_rank ; i ++)
    counter[i] = 0 ;

  if (mask != NULL) {

    msk_typ_sz = GET_ELEMENT_SZ_FROM_DESC(mask);
    mask_b     = (char *) GET_ADDRESS_FROM_DESC(mask) + OFFSET_TO_TF_BYTE(msk_typ_sz) ;

    if (GET_RANK_FROM_DESC(mask) == 0) {
      if (*mask_b) {
	mask = NULL;
      } else {
	src_size = 0;
	for (j = 0 ; j <= src_rank ; j ++) {
	  msk_stride[j] = 0 ;
	  msk_offset[j] = 0 ;
	}
      }

    } else {

      get_offset_and_stride(mask, src_extent, msk_stride,                  msk_offset,  ddim);
    }
  }

  accum = initv ;

  if (src_size == 0 ) {
    return accum;
  }
  array_p = array_b ;
  if (mask == NULL) {

    {
      size_t *p1 = NULL;
      size_t *p2 = NULL;
      size_t p3  = 0;

      src_rank = find_contig_axes(src_extent, src_stride, src_offset, src_rank, typ_sz, p1,p2,p3) ;
    }

    a_size   = src_extent[0] ;
    a_stride = src_stride[0] ;

    while (counter[src_rank] < src_extent[src_rank] ) {

      for ( i = 0 ; i < a_size ; i ++ ) {
	if (*(i2 *)array_p > accum ) {
	  accum = * (i2 *)array_p ;
	}         
	array_p += a_stride ;
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
  } else {

    {
      size_t *p1 = NULL;
      size_t *p2 = NULL;
      size_t p3  = 0;

      p1 =  msk_stride ;
      p2 =  msk_offset ;
      p3 =  msk_typ_sz ;

      src_rank = find_contig_axes(src_extent, src_stride, src_offset, src_rank, typ_sz, p1,p2,p3) ;
    }

    a_size   = src_extent[0] ;
    a_stride = src_stride[0] ;
    m_stride = msk_stride[0] ;
    mask_p   = mask_b   ;

    while (counter[src_rank] < src_extent[src_rank] ) {

      for ( i = 0 ; i < a_size ; i ++ ) {
	if (*mask_p) {
	  if (*(i2 *)array_p > accum ) {
	    accum = * (i2 *)array_p ;
	  }          
	}
	array_p += a_stride ;
	mask_p += m_stride ;
      }
      counter[0] = a_size  ;
      j = 0 ;
      while ((counter[j] == src_extent[j]) && (j < src_rank)) {
	array_p += src_offset[j] ;
	mask_p += msk_offset[j] ;
	counter[j+1]++ ;
	counter[j] = 0 ;
	j ++ ;
      }
    }
  }
  return accum ;
}
i4 
_MAXVAL0__I4(
DopeVectorType	*array,
DopeVectorType	*dim,
DopeVectorType	*mask)
{
  char  * array_p, * array_b ;
  char * dim_p, * dim_b ;
  char * mask_p, * mask_b ;

  size_t src_extent [MAX_NARY_DIMS] ;
  size_t counter    [MAX_NARY_DIMS] ;
  size_t src_offset [MAX_NARY_DIMS] ;
  size_t src_stride [MAX_NARY_DIMS] ;
  size_t src_size ;

  size_t msk_stride [MAX_NARY_DIMS] ;
  size_t msk_offset [MAX_NARY_DIMS] ;

  int32_t ddim ;
  uint32_t src_rank ;
  uint32_t res_rank ;
  uint32_t jrank ;

  size_t j,k,i ;
  size_t typ_sz;
  size_t msk_typ_sz;

  i4 accum ;
  i4 const initv = INT32_MIN ;
  size_t a_size,a_stride;
  size_t m_stride ;

  i4 temp,new ;

  if (mask == NULL) {   /* is third arg mask or dim? */
    if (dim != NULL) {
      if (GET_DV_LOGICAL_FROM_DESC(dim)) {
	mask = (DopeVectorType	*) dim ;
	dim = NULL;
      }
    }
  }

  if (dim != NULL) {
    ddim = read_dim(dim);
  } else 
    ddim = 0 ;

  array_b  = (char *) GET_ADDRESS_FROM_DESC(array) ;
  src_rank = GET_RANK_FROM_DESC(array) - 1;
  typ_sz   = GET_ELEMENT_SZ_FROM_DESC(array);

  src_size = read_source_desc(array,	 src_extent, src_stride, src_offset, ddim);

  for (i = 0 ; i <= src_rank ; i ++)
    counter[i] = 0 ;

  if (mask != NULL) {

    msk_typ_sz = GET_ELEMENT_SZ_FROM_DESC(mask);
    mask_b     = (char *) GET_ADDRESS_FROM_DESC(mask) + OFFSET_TO_TF_BYTE(msk_typ_sz) ;

    if (GET_RANK_FROM_DESC(mask) == 0) {
      if (*mask_b) {
	mask = NULL;
      } else {
	src_size = 0;
	for (j = 0 ; j <= src_rank ; j ++) {
	  msk_stride[j] = 0 ;
	  msk_offset[j] = 0 ;
	}
      }

    } else {

      get_offset_and_stride(mask, src_extent, msk_stride,                  msk_offset,  ddim);
    }
  }

  accum = initv ;

  if (src_size == 0 ) {
    return accum;
  }
  array_p = array_b ;
  if (mask == NULL) {

    {
      size_t *p1 = NULL;
      size_t *p2 = NULL;
      size_t p3  = 0;

      src_rank = find_contig_axes(src_extent, src_stride, src_offset, src_rank, typ_sz, p1,p2,p3) ;
    }

    a_size   = src_extent[0] ;
    a_stride = src_stride[0] ;

    while (counter[src_rank] < src_extent[src_rank] ) {

      for ( i = 0 ; i < a_size ; i ++ ) {
	if (*(i4 *)array_p > accum ) {
	  accum = * (i4 *)array_p ;
	}         
	array_p += a_stride ;
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
  } else {

    {
      size_t *p1 = NULL;
      size_t *p2 = NULL;
      size_t p3  = 0;

      p1 =  msk_stride ;
      p2 =  msk_offset ;
      p3 =  msk_typ_sz ;

      src_rank = find_contig_axes(src_extent, src_stride, src_offset, src_rank, typ_sz, p1,p2,p3) ;
    }

    a_size   = src_extent[0] ;
    a_stride = src_stride[0] ;
    m_stride = msk_stride[0] ;
    mask_p   = mask_b   ;

    while (counter[src_rank] < src_extent[src_rank] ) {

      for ( i = 0 ; i < a_size ; i ++ ) {
	if (*mask_p) {
	  if (*(i4 *)array_p > accum ) {
	    accum = * (i4 *)array_p ;
	  }          
	}
	array_p += a_stride ;
	mask_p += m_stride ;
      }
      counter[0] = a_size  ;
      j = 0 ;
      while ((counter[j] == src_extent[j]) && (j < src_rank)) {
	array_p += src_offset[j] ;
	mask_p += msk_offset[j] ;
	counter[j+1]++ ;
	counter[j] = 0 ;
	j ++ ;
      }
    }
  }
  return accum ;
}
i8 
_MAXVAL0__J(
DopeVectorType	*array,
DopeVectorType	*dim,
DopeVectorType	*mask)
{
  char  * array_p, * array_b ;
  char * dim_p, * dim_b ;
  char * mask_p, * mask_b ;

  size_t src_extent [MAX_NARY_DIMS] ;
  size_t counter    [MAX_NARY_DIMS] ;
  size_t src_offset [MAX_NARY_DIMS] ;
  size_t src_stride [MAX_NARY_DIMS] ;
  size_t src_size ;

  size_t msk_stride [MAX_NARY_DIMS] ;
  size_t msk_offset [MAX_NARY_DIMS] ;

  int32_t ddim ;
  uint32_t src_rank ;
  uint32_t res_rank ;
  uint32_t jrank ;

  size_t j,k,i ;
  size_t typ_sz;
  size_t msk_typ_sz;

  i8 accum ;
  i8 const initv = INT64_MIN ;
  size_t a_size,a_stride;
  size_t m_stride ;

  i8 temp,new ;

  if (mask == NULL) {   /* is third arg mask or dim? */
    if (dim != NULL) {
      if (GET_DV_LOGICAL_FROM_DESC(dim)) {
	mask = (DopeVectorType	*) dim ;
	dim = NULL;
      }
    }
  }

  if (dim != NULL) {
    ddim = read_dim(dim);
  } else 
    ddim = 0 ;

  array_b  = (char *) GET_ADDRESS_FROM_DESC(array) ;
  src_rank = GET_RANK_FROM_DESC(array) - 1;
  typ_sz   = GET_ELEMENT_SZ_FROM_DESC(array);

  src_size = read_source_desc(array,	 src_extent, src_stride, src_offset, ddim);

  for (i = 0 ; i <= src_rank ; i ++)
    counter[i] = 0 ;

  if (mask != NULL) {

    msk_typ_sz = GET_ELEMENT_SZ_FROM_DESC(mask);
    mask_b     = (char *) GET_ADDRESS_FROM_DESC(mask) + OFFSET_TO_TF_BYTE(msk_typ_sz) ;

    if (GET_RANK_FROM_DESC(mask) == 0) {
      if (*mask_b) {
	mask = NULL;
      } else {
	src_size = 0;
	for (j = 0 ; j <= src_rank ; j ++) {
	  msk_stride[j] = 0 ;
	  msk_offset[j] = 0 ;
	}
      }

    } else {

      get_offset_and_stride(mask, src_extent, msk_stride,                  msk_offset,  ddim);
    }
  }

  accum = initv ;

  if (src_size == 0 ) {
    return accum;
  }
  array_p = array_b ;
  if (mask == NULL) {

    {
      size_t *p1 = NULL;
      size_t *p2 = NULL;
      size_t p3  = 0;

      src_rank = find_contig_axes(src_extent, src_stride, src_offset, src_rank, typ_sz, p1,p2,p3) ;
    }

    a_size   = src_extent[0] ;
    a_stride = src_stride[0] ;

    while (counter[src_rank] < src_extent[src_rank] ) {

      for ( i = 0 ; i < a_size ; i ++ ) {
	if (*(i8 *)array_p > accum ) {
	  accum = * (i8 *)array_p ;
	}         
	array_p += a_stride ;
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
  } else {

    {
      size_t *p1 = NULL;
      size_t *p2 = NULL;
      size_t p3  = 0;

      p1 =  msk_stride ;
      p2 =  msk_offset ;
      p3 =  msk_typ_sz ;

      src_rank = find_contig_axes(src_extent, src_stride, src_offset, src_rank, typ_sz, p1,p2,p3) ;
    }

    a_size   = src_extent[0] ;
    a_stride = src_stride[0] ;
    m_stride = msk_stride[0] ;
    mask_p   = mask_b   ;

    while (counter[src_rank] < src_extent[src_rank] ) {

      for ( i = 0 ; i < a_size ; i ++ ) {
	if (*mask_p) {
	  if (*(i8 *)array_p > accum ) {
	    accum = * (i8 *)array_p ;
	  }          
	}
	array_p += a_stride ;
	mask_p += m_stride ;
      }
      counter[0] = a_size  ;
      j = 0 ;
      while ((counter[j] == src_extent[j]) && (j < src_rank)) {
	array_p += src_offset[j] ;
	mask_p += msk_offset[j] ;
	counter[j+1]++ ;
	counter[j] = 0 ;
	j ++ ;
      }
    }
  }
  return accum ;
}
r4 
_MAXVAL0__S4(
DopeVectorType	*array,
DopeVectorType	*dim,
DopeVectorType	*mask)
{
  char  * array_p, * array_b ;
  char * dim_p, * dim_b ;
  char * mask_p, * mask_b ;

  size_t src_extent [MAX_NARY_DIMS] ;
  size_t counter    [MAX_NARY_DIMS] ;
  size_t src_offset [MAX_NARY_DIMS] ;
  size_t src_stride [MAX_NARY_DIMS] ;
  size_t src_size ;

  size_t msk_stride [MAX_NARY_DIMS] ;
  size_t msk_offset [MAX_NARY_DIMS] ;

  int32_t ddim ;
  uint32_t src_rank ;
  uint32_t res_rank ;
  uint32_t jrank ;

  size_t j,k,i ;
  size_t typ_sz;
  size_t msk_typ_sz;

  r4 accum ;
  r4 const initv = -HUGE_REAL4_F90 ;
  size_t a_size,a_stride;
  size_t m_stride ;

  r4 temp,new ;

  if (mask == NULL) {   /* is third arg mask or dim? */
    if (dim != NULL) {
      if (GET_DV_LOGICAL_FROM_DESC(dim)) {
	mask = (DopeVectorType	*) dim ;
	dim = NULL;
      }
    }
  }

  if (dim != NULL) {
    ddim = read_dim(dim);
  } else 
    ddim = 0 ;

  array_b  = (char *) GET_ADDRESS_FROM_DESC(array) ;
  src_rank = GET_RANK_FROM_DESC(array) - 1;
  typ_sz   = GET_ELEMENT_SZ_FROM_DESC(array);

  src_size = read_source_desc(array,	 src_extent, src_stride, src_offset, ddim);

  for (i = 0 ; i <= src_rank ; i ++)
    counter[i] = 0 ;

  if (mask != NULL) {

    msk_typ_sz = GET_ELEMENT_SZ_FROM_DESC(mask);
    mask_b     = (char *) GET_ADDRESS_FROM_DESC(mask) + OFFSET_TO_TF_BYTE(msk_typ_sz) ;

    if (GET_RANK_FROM_DESC(mask) == 0) {
      if (*mask_b) {
	mask = NULL;
      } else {
	src_size = 0;
	for (j = 0 ; j <= src_rank ; j ++) {
	  msk_stride[j] = 0 ;
	  msk_offset[j] = 0 ;
	}
      }

    } else {

      get_offset_and_stride(mask, src_extent, msk_stride,                  msk_offset,  ddim);
    }
  }

  accum = initv ;

  if (src_size == 0 ) {
    return accum;
  }
  array_p = array_b ;
  if (mask == NULL) {

    {
      size_t *p1 = NULL;
      size_t *p2 = NULL;
      size_t p3  = 0;

      src_rank = find_contig_axes(src_extent, src_stride, src_offset, src_rank, typ_sz, p1,p2,p3) ;
    }

    a_size   = src_extent[0] ;
    a_stride = src_stride[0] ;

    while (counter[src_rank] < src_extent[src_rank] ) {

      for ( i = 0 ; i < a_size ; i ++ ) {
	if (*(r4 *)array_p > accum ) {
	  accum = * (r4 *)array_p ;
	}         
	array_p += a_stride ;
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
  } else {

    {
      size_t *p1 = NULL;
      size_t *p2 = NULL;
      size_t p3  = 0;

      p1 =  msk_stride ;
      p2 =  msk_offset ;
      p3 =  msk_typ_sz ;

      src_rank = find_contig_axes(src_extent, src_stride, src_offset, src_rank, typ_sz, p1,p2,p3) ;
    }

    a_size   = src_extent[0] ;
    a_stride = src_stride[0] ;
    m_stride = msk_stride[0] ;
    mask_p   = mask_b   ;

    while (counter[src_rank] < src_extent[src_rank] ) {

      for ( i = 0 ; i < a_size ; i ++ ) {
	if (*mask_p) {
	  if (*(r4 *)array_p > accum ) {
	    accum = * (r4 *)array_p ;
	  }          
	}
	array_p += a_stride ;
	mask_p += m_stride ;
      }
      counter[0] = a_size  ;
      j = 0 ;
      while ((counter[j] == src_extent[j]) && (j < src_rank)) {
	array_p += src_offset[j] ;
	mask_p += msk_offset[j] ;
	counter[j+1]++ ;
	counter[j] = 0 ;
	j ++ ;
      }
    }
  }
  return accum ;
}
r8 
_MAXVAL0__S(
DopeVectorType	*array,
DopeVectorType	*dim,
DopeVectorType	*mask)
{
  char  * array_p, * array_b ;
  char * dim_p, * dim_b ;
  char * mask_p, * mask_b ;

  size_t src_extent [MAX_NARY_DIMS] ;
  size_t counter    [MAX_NARY_DIMS] ;
  size_t src_offset [MAX_NARY_DIMS] ;
  size_t src_stride [MAX_NARY_DIMS] ;
  size_t src_size ;

  size_t msk_stride [MAX_NARY_DIMS] ;
  size_t msk_offset [MAX_NARY_DIMS] ;

  int32_t ddim ;
  uint32_t src_rank ;
  uint32_t res_rank ;
  uint32_t jrank ;

  size_t j,k,i ;
  size_t typ_sz;
  size_t msk_typ_sz;

  r8 accum ;
  r8 const initv = -HUGE_REAL8_F90 ;
  size_t a_size,a_stride;
  size_t m_stride ;

  r8 temp,new ;

  if (mask == NULL) {   /* is third arg mask or dim? */
    if (dim != NULL) {
      if (GET_DV_LOGICAL_FROM_DESC(dim)) {
	mask = (DopeVectorType	*) dim ;
	dim = NULL;
      }
    }
  }

  if (dim != NULL) {
    ddim = read_dim(dim);
  } else 
    ddim = 0 ;

  array_b  = (char *) GET_ADDRESS_FROM_DESC(array) ;
  src_rank = GET_RANK_FROM_DESC(array) - 1;
  typ_sz   = GET_ELEMENT_SZ_FROM_DESC(array);

  src_size = read_source_desc(array,	 src_extent, src_stride, src_offset, ddim);

  for (i = 0 ; i <= src_rank ; i ++)
    counter[i] = 0 ;

  if (mask != NULL) {

    msk_typ_sz = GET_ELEMENT_SZ_FROM_DESC(mask);
    mask_b     = (char *) GET_ADDRESS_FROM_DESC(mask) + OFFSET_TO_TF_BYTE(msk_typ_sz) ;

    if (GET_RANK_FROM_DESC(mask) == 0) {
      if (*mask_b) {
	mask = NULL;
      } else {
	src_size = 0;
	for (j = 0 ; j <= src_rank ; j ++) {
	  msk_stride[j] = 0 ;
	  msk_offset[j] = 0 ;
	}
      }

    } else {

      get_offset_and_stride(mask, src_extent, msk_stride,                  msk_offset,  ddim);
    }
  }

  accum = initv ;

  if (src_size == 0 ) {
    return accum;
  }
  array_p = array_b ;
  if (mask == NULL) {

    {
      size_t *p1 = NULL;
      size_t *p2 = NULL;
      size_t p3  = 0;

      src_rank = find_contig_axes(src_extent, src_stride, src_offset, src_rank, typ_sz, p1,p2,p3) ;
    }

    a_size   = src_extent[0] ;
    a_stride = src_stride[0] ;

    while (counter[src_rank] < src_extent[src_rank] ) {

      for ( i = 0 ; i < a_size ; i ++ ) {
	if (*(r8 *)array_p > accum ) {
	  accum = * (r8 *)array_p ;
	}         
	array_p += a_stride ;
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
  } else {

    {
      size_t *p1 = NULL;
      size_t *p2 = NULL;
      size_t p3  = 0;

      p1 =  msk_stride ;
      p2 =  msk_offset ;
      p3 =  msk_typ_sz ;

      src_rank = find_contig_axes(src_extent, src_stride, src_offset, src_rank, typ_sz, p1,p2,p3) ;
    }

    a_size   = src_extent[0] ;
    a_stride = src_stride[0] ;
    m_stride = msk_stride[0] ;
    mask_p   = mask_b   ;

    while (counter[src_rank] < src_extent[src_rank] ) {

      for ( i = 0 ; i < a_size ; i ++ ) {
	if (*mask_p) {
	  if (*(r8 *)array_p > accum ) {
	    accum = * (r8 *)array_p ;
	  }          
	}
	array_p += a_stride ;
	mask_p += m_stride ;
      }
      counter[0] = a_size  ;
      j = 0 ;
      while ((counter[j] == src_extent[j]) && (j < src_rank)) {
	array_p += src_offset[j] ;
	mask_p += msk_offset[j] ;
	counter[j+1]++ ;
	counter[j] = 0 ;
	j ++ ;
      }
    }
  }
  return accum ;
}
r16 
_MAXVAL0__D(
DopeVectorType	*array,
DopeVectorType	*dim,
DopeVectorType	*mask)
{
  char  * array_p, * array_b ;
  char * dim_p, * dim_b ;
  char * mask_p, * mask_b ;

  size_t src_extent [MAX_NARY_DIMS] ;
  size_t counter    [MAX_NARY_DIMS] ;
  size_t src_offset [MAX_NARY_DIMS] ;
  size_t src_stride [MAX_NARY_DIMS] ;
  size_t src_size ;

  size_t msk_stride [MAX_NARY_DIMS] ;
  size_t msk_offset [MAX_NARY_DIMS] ;

  int32_t ddim ;
  uint32_t src_rank ;
  uint32_t res_rank ;
  uint32_t jrank ;

  size_t j,k,i ;
  size_t typ_sz;
  size_t msk_typ_sz;

  r16 accum ;
  r16 const initv = -HUGE_REAL16_F90 ;
  size_t a_size,a_stride;
  size_t m_stride ;

  r16 temp,new ;

  if (mask == NULL) {   /* is third arg mask or dim? */
    if (dim != NULL) {
      if (GET_DV_LOGICAL_FROM_DESC(dim)) {
	mask = (DopeVectorType	*) dim ;
	dim = NULL;
      }
    }
  }

  if (dim != NULL) {
    ddim = read_dim(dim);
  } else 
    ddim = 0 ;

  array_b  = (char *) GET_ADDRESS_FROM_DESC(array) ;
  src_rank = GET_RANK_FROM_DESC(array) - 1;
  typ_sz   = GET_ELEMENT_SZ_FROM_DESC(array);

  src_size = read_source_desc(array,	 src_extent, src_stride, src_offset, ddim);

  for (i = 0 ; i <= src_rank ; i ++)
    counter[i] = 0 ;

  if (mask != NULL) {

    msk_typ_sz = GET_ELEMENT_SZ_FROM_DESC(mask);
    mask_b     = (char *) GET_ADDRESS_FROM_DESC(mask) + OFFSET_TO_TF_BYTE(msk_typ_sz) ;

    if (GET_RANK_FROM_DESC(mask) == 0) {
      if (*mask_b) {
	mask = NULL;
      } else {
	src_size = 0;
	for (j = 0 ; j <= src_rank ; j ++) {
	  msk_stride[j] = 0 ;
	  msk_offset[j] = 0 ;
	}
      }

    } else {

      get_offset_and_stride(mask, src_extent, msk_stride,                  msk_offset,  ddim);
    }
  }

  accum = initv ;

  if (src_size == 0 ) {
    return accum;
  }
  array_p = array_b ;
  if (mask == NULL) {

    {
      size_t *p1 = NULL;
      size_t *p2 = NULL;
      size_t p3  = 0;

      src_rank = find_contig_axes(src_extent, src_stride, src_offset, src_rank, typ_sz, p1,p2,p3) ;
    }

    a_size   = src_extent[0] ;
    a_stride = src_stride[0] ;

    while (counter[src_rank] < src_extent[src_rank] ) {

      for ( i = 0 ; i < a_size ; i ++ ) {
	if (*(r16 *)array_p > accum ) {
	  accum = * (r16 *)array_p ;
	}         
	array_p += a_stride ;
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
  } else {

    {
      size_t *p1 = NULL;
      size_t *p2 = NULL;
      size_t p3  = 0;

      p1 =  msk_stride ;
      p2 =  msk_offset ;
      p3 =  msk_typ_sz ;

      src_rank = find_contig_axes(src_extent, src_stride, src_offset, src_rank, typ_sz, p1,p2,p3) ;
    }

    a_size   = src_extent[0] ;
    a_stride = src_stride[0] ;
    m_stride = msk_stride[0] ;
    mask_p   = mask_b   ;

    while (counter[src_rank] < src_extent[src_rank] ) {

      for ( i = 0 ; i < a_size ; i ++ ) {
	if (*mask_p) {
	  if (*(r16 *)array_p > accum ) {
	    accum = * (r16 *)array_p ;
	  }          
	}
	array_p += a_stride ;
	mask_p += m_stride ;
      }
      counter[0] = a_size  ;
      j = 0 ;
      while ((counter[j] == src_extent[j]) && (j < src_rank)) {
	array_p += src_offset[j] ;
	mask_p += msk_offset[j] ;
	counter[j+1]++ ;
	counter[j] = 0 ;
	j ++ ;
      }
    }
  }
  return accum ;
}

static int32_t
read_dim(DopeVectorType	* dim)
{
  int32_t ddim ;
  char * dim_p ;

  dim_p  = (char *) GET_ADDRESS_FROM_DESC(dim) ;

  switch (GET_ELEMENT_SZ_FROM_DESC(dim)) {
  case sizeof(int8_t):
    ddim = * (int8_t *) dim_p ;
    break;

  case sizeof(int16_t):
    ddim = * (int16_t *) dim_p ;
    break;

  case sizeof(int32_t):
    ddim = * (int32_t *) dim_p ;
    break;

  case sizeof(int64_t):
    ddim = * (int64_t *) dim_p ;
    break;
  }

  return (ddim - 1) ;
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

static uint32_t
find_contig_axes(size_t src_extent[MAX_NARY_DIMS],
size_t src_stride[MAX_NARY_DIMS],
size_t src_offset[MAX_NARY_DIMS],
uint32_t src_rank,
size_t typ_sz,
size_t msk_stride[MAX_NARY_DIMS],
size_t msk_offset[MAX_NARY_DIMS],
size_t msk_typ_sz)
{
  uint32_t i,j ;
  size_t jrank;
  size_t * p ;

  p = msk_stride ;

  jrank = src_rank ;
  j = 1 ;
  while ((j <= jrank) &&((src_extent[0] * typ_sz)  == src_stride[j]) &&(((p == NULL) ||((src_extent[0] * msk_typ_sz) ==
      msk_stride[j]))) ) {
    src_extent[0] *= src_extent[j] ;
    src_offset[0]  = src_offset[j] ;
    if (p != NULL)
      msk_offset[0] = msk_offset[j] ;
    src_rank -- ;
    j++ ;
  }
  for (i = j  ; ( i <= jrank && i > 1)  ; i ++ ) {
    src_stride[i-j+1] = src_stride[i] ;
    src_offset[i-j+1] = src_offset[i] ;
    src_extent[i-j+1] = src_extent[i] ;
    if (p != NULL) {
      msk_stride[i-j+1] = msk_stride[i] ;
      msk_offset[i-j+1] = msk_offset[i] ;
    }
  }
  return src_rank ;
}
