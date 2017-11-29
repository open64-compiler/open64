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
_TRANS(
DopeVectorType	*result,
DopeVectorType	*matrix)
{
  char  * result_p, * result_b ;
  char  * matrix_p, * matrix_b ;

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
  int32_t src_rank = GET_RANK_FROM_DESC(matrix) - 1;

  size_t typ_sz   = GET_ELEMENT_SZ_FROM_DESC(matrix);

  size_t a_size,a_stride,r_stride, i,k ;
  int8_t  zero_szd_source = FALSE;
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

  for ( j = 0 ; j <= src_rank ; j ++  ) {
    src_extent[j]  = GET_EXTENT_FROM_DESC(matrix,j) ;
    src_stride[j]  = GET_STRIDE_FROM_DESC(matrix,j) ;
    counter[j]     = 0  ;
    zero_szd_source = zero_szd_source || (src_extent[j] == 0) ;
  }

  byte_aligned = GET_BYTEALIGNED_FROM_DESC(matrix) ;

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
      ext = GET_EXTENT_FROM_DESC(matrix,src_rank-i) ;
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

    SET_CONTIG_IN_DESC(result);
    SET_ASSOCIATED_IN_DESC(result);
    if (GET_DV_ASCII_FROM_DESC(matrix)) {
      SET_CHARPTR_IN_DESC(result,p,typ_sz);
    }
    SET_ORIG_BS_IN_DESC(result,p) ;
    SET_ORIG_SZ_IN_DESC(result,nbytes * 8) ;
  }

  for ( j = 0 ; j <= src_rank ; j ++  ) {
    res_stride [j] = GET_STRIDE_FROM_DESC(result,src_rank - j) ;
  }

  for ( j = 1 ; j <= src_rank ; j ++  ) {
    src_offset[j-1] = src_stride[j] - (src_stride [j-1] * (src_extent[j-1]))  ;
    res_offset[j-1] = res_stride[j] - (res_stride [j-1] * (src_extent[j-1]))  ;
  }

  if (zero_szd_source)
    return ;

  a_size   = src_extent[0] ;
  a_stride = src_stride[0] ;
  r_stride = res_stride[0] ;
  matrix_p = GET_ADDRESS_FROM_DESC(matrix);
  result_p = GET_ADDRESS_FROM_DESC(result);

  {
    while (counter[src_rank] < src_extent[src_rank] ) {
      for ( i = 0 ; i < a_size ; i ++ ) {
	ap = matrix_p ;
	rp = result_p ;
	if (typ_sz > BIGDEFAULTSZ)
	  (void) memcpy (rp, ap, typ_sz);
	else
	  for (j = 0 ; j < typ_sz ; j ++)  *rp++ = *ap ++ ;
	matrix_p += a_stride ;
	result_p += r_stride ;
      }
      counter[0] = a_size  ;
      j = 0 ;
      while ((counter[j] == src_extent[j]) && (j < src_rank)) {
	matrix_p += src_offset[j] ;
	result_p += res_offset[j] ;
	counter[j+1]++ ;
	counter[j] = 0 ;
	j ++ ;
      }

    }
  }
}
