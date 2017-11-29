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


/* USMID @(#) libfi/mpp_reduce/any_p.h	92.0	10/08/98 14:37:14 */


#define GLUE(X,Y) X ## Y
#define XGLUE(X,Y) GLUE(X,Y)

void
XGLUE(_ANY_LP,RANK)(DopeVectorType * result,
	    DopeVectorType * source,
	    long * dim)
{
	long i, dim1_tmp, dim2_tmp;
	long weights[RANK-1], blknums[RANK-1];
	long src_extents[RANK], res_extents[RANK-1];
	long dim_bcnt[RANK-1], blkcnts[RANK];
	void *result_sdd_ptr, *source_sdd_ptr;
	void *result_base_ptr;

	/*
	 * If any of the extents for the source array argument is
	 * zero, then return to the caller. Since we allocate
	 * the result array, there is no need to check that.
	 */

	EXTENT_CHK(source);

	/*
	 * Check to insure that DIM contains a legal value from
	 * 1 through the extent of the source array.
	 */

	DIM_CHK(dim);

	/*
	 * Allocate the result shared temporary and fill in the
	 * dope vector.
	 */

	_shmalloc_reduced_result(result, source, dim, &result_base_ptr);

	/*
	 * Set up arrays of extents for the source and result array.
	 * Macro SETUP_EXTENTS uses variables result, source, src_extents,
	 * res_extents, and dim.
	 */

	SETUP_EXTENTS();

	/*
	 * Initialize an array of block counts which contain the
	 * number of blocks of the distributed source array on MY_PE
	 * for each dimension. Macro SETUP_BLKCNTS() uses variables
	 * source, blkcnts, dim, and dim_bcnt.
	 */

	SETUP_BLKCNTS();

	/*
	 * Initialize the sdd for the shared result temporary so that
	 * it describes one distributed array that is dimensioned
	 * the same as the source array excluding DIM and has each
	 * dimension distributed :BLOCK. The macro INIT_SDD uses
	 * variables result, result_base_ptr, weights, blknums, and
	 * res_extents.
	 */

	INIT_SDD();

	/*
	 * Set up local pointers to the source and result arrays.
	 */

	i = (long)result->base_addr.a.ptr | SIGN_BIT;
	result_sdd_ptr = (void *)i;
	source_sdd_ptr = source->base_addr.a.ptr;

	/*
	 * Call the Fortran work routine.
	 */

	XGLUE(XGLUE(ANY_I_LP,RANK),@)(result_sdd_ptr, source_sdd_ptr, dim,
			res_extents, src_extents, blkcnts, dim_bcnt);

}
