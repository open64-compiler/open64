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


/* USMID @(#) libfi/mpp_scan/segmented_scan_p.h	92.0	10/08/98 14:37:14 */

	long	i, j;
	long	mask_flag, stop_flag;
	long	blkcnts[RANK];
	long	src_extents[RANK];
	void	*result_sdd_ptr;
	void	*source_sdd_ptr;
	void	*mask_sdd_ptr;
	void	*stop_sdd_ptr;
	void	*result_base_ptr;
	long	dim_val;

	/*
	 * If any of the extents for the source or mask array 
	 * arguments are zero, then return to the caller. Since
	 * we allocate the result array, there is no need to
	 * check that.
	 */

	EXTENT_CHK(source);
	EXTENT_CHK(stop);
	if (mask) {
	    EXTENT_CHK(mask);
	}
	if (result->assoc) {
	    EXTENT_CHK(result);
	}

	/*
	 * Check to insure that DIM contains a legal value from
	 * 1 through the extent of the source array.
	 */

	if (dim) {
	    DIM_CHK (dim);
	    dim_val = *dim;
	} else {
	    dim_val = 0;
	}

	/*
	 * Allocate the result shared temporary and fill in the
	 * dope vector.
	 */

	if (!result->assoc)
	    _shmalloc_result(result, source, &result_base_ptr);

	/*
	 * Set up the extent arrays for the source array.
	 */

	for (i = 0; i < RANK; i++) {
	    src_extents[i] = source->dimension[i].extent;
	}

	/*
	 * Initialize an array of block counts which contain the
	 * number of blocks of the distributed source array on MY_PE
	 * for each dimension. Macro SETUP_BLKCNTS() uses variables
	 * source, and blkcnts.
	 */

	SETUP_PRE_BLKCNTS();

	/*
	 * Initialize the sdd for the shared result temporary so that
	 * it describes one distributed array that is dimensioned
	 * the same as the source array.
	 */

	_sdd_write_base (result->base_addr.a.ptr, result_base_ptr);
	_sdd_write_offset (result->base_addr.a.ptr, 0);
	_sdd_write_canon (result->base_addr.a.ptr, 0);
	for (i = 1; i < 8; i++) {
	    j = _sdd_read_blk_ebp (source->base_addr.a.ptr, i);
	    _sdd_write_blk_ebp (result->base_addr.a.ptr, i, j);
	    j = _sdd_read_pe_bcnt (source->base_addr.a.ptr, i);
	    _sdd_write_pe_bcnt (result->base_addr.a.ptr, i, j);
	    j = _sdd_read_cyc_ebp (source->base_addr.a.ptr, i);
	    _sdd_write_cyc_ebp (result->base_addr.a.ptr, i, j);
	}

	/*
	 * Set up local pointers to the source, result, and mask arrays.
	 * If a mask argument was provided by the user, then check
	 * to see if it is an array or a scalar. If the mask is a
	 * non-zero scalar, then treat as if no mask was provided.
	 * If the mask is a scalar with a value of zero, then set the
	 * result to 0 and return to caller. If the mask is a shared
	 * array, then set up the mask's sdd pointer.
	 */

	result_sdd_ptr = result->base_addr.a.ptr;
	source_sdd_ptr = source->base_addr.a.ptr;
	CHECK_STOP(stop_sdd_ptr, stop_flag);
	if (stop_flag < 0)
	    return;
	CHECK_MASK(mask_sdd_ptr, mask_flag);
	if (mask_flag < 0)
	    return;
