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


/* USMID @(#) libfi/mpp_shift/cshift_p.h	92.0	05/26/95 17:16:53 */


	int		i;
	int		src_extents[RANK];
	int		shft_extents[RANK];
	int		tmp;
	int		dim_val;
	int		shflag, shftval, *shptr;
	int		mype;
	void		*result_base_ptr;
	void		*result_sdd_ptr;
	void		*source_sdd_ptr;
	void		*shift_sdd_ptr;
	int		*iptr;
	_f_real		*fptr;
	_f_comp		*cptr;
	void		*vptr;
	int		special;
/*
 *	If any of the extents for the source, or shift array arguments
 *	are zero, then return to the caller.  Since we allocate the
 *	result array, there is no need to check that.
 */

	EXTENT_CHK(source);
	if (shift)
	    if (shift->n_dim > 0) {
		EXTENT_CHK(shift);
	    }

/*
 *	Allocate the result shared temporary and fill in the dope vector.
 */

	if (!result->assoc)
	    _shmalloc_result(result, source, &result_base_ptr);

/*
 *	Initialize an array of block counts which contain the number of
 *	blocks of the distributed source array on MY_PE for each dimension.
 *	Macro SETUP_BLKCNTS() uses variables source, blkcnts, dim, and
 *	dim_bcnt.
 */


/*
 *	Initialize the sdd for the shared result temporary so that it
 *	matches that of the source array.
 */

	_sdd_write_base (result->base_addr.a.ptr, result_base_ptr);
	tmp = _sdd_read_canon (source->base_addr.a.ptr);
	_sdd_write_canon (result->base_addr.a.ptr, tmp);
	tmp = _sdd_read_offset (source->base_addr.a.ptr);
	_sdd_write_offset (result->base_addr.a.ptr, tmp);
_Pragma ("shortloop");
	for (i = 1; i <= 7; i++) {
	    tmp = _sdd_read_cyc_ebp (source->base_addr.a.ptr, i);
	    _sdd_write_cyc_ebp (result->base_addr.a.ptr, i, tmp);
	    tmp = _sdd_read_pe_bcnt (source->base_addr.a.ptr, i);
	    _sdd_write_pe_bcnt (result->base_addr.a.ptr, i, tmp);
	    tmp = _sdd_read_blk_ebp (source->base_addr.a.ptr, i);
	    _sdd_write_blk_ebp (result->base_addr.a.ptr, i, tmp);
	}


	source_sdd_ptr = source->base_addr.a.ptr;
	result_sdd_ptr = result->base_addr.a.ptr;
	shift_sdd_ptr = shift->base_addr.a.ptr;

/*
 *	Determine if the shift count is a scalar or an array.  If scalar,
 *	set up the flag and the scalar value counter.
 */

	INIT_SHIFT_COUNT();

/*
 *	Set up the dim scalar.  If the dim argument is not present, it
 *	defaults to 1.
 */

	INIT_DIM_COUNT();

/*
 *	Set up the extent arrays
 */

#pragma	_CRI	shortloop
	for (i = 0; i < RANK; i++) {
	    src_extents[i] = source->dimension[i].extent;
	}
	if (shflag) {
	    for (i = 0; i < RANK-1; i++) {
		shft_extents[i] = 1;
	    }
	} else {
	    for (i = 0; i < RANK-1; i++) {
		shft_extents[i] = shift->dimension[i].extent;
	    }
	}
