C
C
C  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
C
C  This program is free software; you can redistribute it and/or modify it
C  under the terms of version 2.1 of the GNU Lesser General Public License 
C  as published by the Free Software Foundation.
C
C  This program is distributed in the hope that it would be useful, but
C  WITHOUT ANY WARRANTY; without even the implied warranty of
C  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
C
C  Further, this software is distributed without any warranty that it is
C  free of the rightful claim of any third person regarding infringement 
C  or the like.  Any license provided herein, whether implied or 
C  otherwise, applies only to this software file.  Patent licenses, if
C  any, provided herein do not apply to combinations of this program with 
C  other software, or any other product whatsoever.  
C
C  You should have received a copy of the GNU Lesser General Public 
C  License along with this program; if not, write the Free Software 
C  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
C  USA.
C
C  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
C  Mountain View, CA 94043, or:
C
C  http://www.sgi.com
C
C  For further information regarding this notice, see:
C
C  http://oss.sgi.com/projects/GenInfo/NoticeExplan
C
C


	subroutine SCANSUM_MASK_JP6@(result,source,stop,dim,mask,sx,blkcnt)
*
CDIR$ ID "@(#) libfi/mpp_scan/scansum_p_6@.f	92.0	10/08/98 14:37:14"
************************************************************************
*
* Purpose:	Find the sum of the elements identified by mask,
*		while saving all intermediate results.  This routine handles
*		integer source arrays of rank 6. It returns an array result.
*		This routine is called by the C routine _SCANSUM_JP6.
*
* Arguments:	result	- integer array of rank 6.
*		source	- integer array of rank 6.
*		stop	- logical array of rank 1.
*		dim	- integer scalar: dimension to search along.
*		mask	- logical array of rank 6.
*		sx	- integer array of rank 1:
*			  extents of the source array.
*		blkcnt	- integer array of rank 1:
*			  number of blocks for each dimension.
*
* Variables:	hi	- integer scalar: lowest index in each block
*		low	- integer scalar: highext index in each block
*		carryout- carry to be passed to next block.
*		carryin	- carry received from previous block.
*		carry	- working variable to save current value of carry
*		init_val- initial value of carry variable
*		ack	- message acknowledged flag
*		gotit	- set value for acknowledge
*		comin	- carry receipt flag
*		comout	- carry receipt value passed to receiving PE
*		pe_to	- PE to send carry results to
*		pe_from	- PE from whence carry results arrive
*		zero	- the value 0
*		comsize	- size of comm block
*
* Intrinsic functions called:
*		my_pe()
*		hiidx()
*		lowidx()
*		home()
*
* Documentation:
*		T3D Fortran 90 Array Intrinsics Interface Description
*		T3D Fortran 90 Array Intrinsics Design Description
*
* Author:	Kenneth Bussart
*		System Libraries Section
*		Cray Research, Inc.
*
************************************************************************
	implicit	none
!	intrinsic	my_pe, hiidx, lowidx, home
	integer		sx(6)
	integer		blkcnt(6)
!	integer		mype
	integer		dim
	integer		source(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	integer		result(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	logical		stop(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	logical		mask(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	integer		i, j, k, l, m, n, p
!	integer		hi(6), low(6)
!	integer		pe_to, pe_from
!	integer		i1, j1, k1
	integer		carry, init_val
!	integer		tcarry
!	integer		carryin, carryout
!	integer		ack, gotit, zero
!	integer		comin, comout, comsize
*
	integer		FUNC, arg1, arg2
*
!	common		/inint@/ carryin, comin
!	common		/outint@/ carryout, comout
*
cdir$	parallel_only
cdir$	unknown_shared	source, stop, mask, result
*
	FUNC (arg1,arg2) = arg1 + arg2
*
!	ack = 0
!	gotit = 1
!	zero = 0
!	comsize = 1
!	comout = 1
!	comin = 0
!cdir$	barrier
	init_val = 0
*
cdir$	master
	if (dim .eq. 0) then
	    include "segscan_s6_dim0_mask.fh"
	else if (dim .eq. 1) then
	    include "segscan_s6_dim1_mask.fh"
	else if (dim .eq. 2) then
	    include "segscan_s6_dim2_mask.fh"
	else if (dim .eq. 3) then
	    include "segscan_s6_dim3_mask.fh"
	else if (dim .eq. 4) then
	    include "segscan_s6_dim4_mask.fh"
	else if (dim .eq. 5) then
	    include "segscan_s6_dim5_mask.fh"
	else
	    include "segscan_s6_dim6_mask.fh"
	endif
cdir$	end master
*
	return
	end


	subroutine SCANSUM_NOMASK_JP6@(result,source,stop,dim,sx,blkcnt)
*
************************************************************************
*
* Purpose:	Find the sum of the elements in the array section,
*		while saving all intermediate results.  This routine handles
*		integer source arrays of rank 6.  It returns an array result.
*		This routine is called by the C routine _SCANSUM_JP6.
*
* Arguments:	result	- integer array of rank 6.
*		source	- integer array of rank 6.
*		stop	- logical array of rank 1.
*		dim	- integer scalar: dimension to search along.
*		sx	- integer array of rank 1:
*			  extents of the source array.
*		blkcnt	- integer array of rank 1:
*			  number of blocks for each dimension.
*
* Variables:	hi	- integer scalar: lowest index in each block
*		low	- integer scalar: highext index in each block
*		carryout- carry to be passed to next block.
*		carryin	- carry received from previous block.
*		carry	- working variable to save current value of carry
*		init_val- initial value of carry variable
*		ack	- message acknowledged flag
*		gotit	- set value for acknowledge
*		comin	- carry receipt flag
*		comout	- carry receipt value passed to receiving PE
*		pe_to	- PE to send carry results to
*		pe_from	- PE from whence carry results arrive
*		zero	- the value 0
*		comsize	- size of comm block
*
* Intrinsic functions called:
*		my_pe()
*		hiidx()
*		lowidx()
*		home()
*
* Documentation:
*		T3D Fortran 90 Array Intrinsics Interface Description
*		T3D Fortran 90 Array Intrinsics Design Description
*
* Author:	Kenneth Bussart
*		System Libraries Section
*		Cray Research, Inc.
*
************************************************************************
	implicit	none
!	intrinsic	my_pe, hiidx, lowidx, home
	integer		sx(6)
	integer		blkcnt(6)
!	integer		mype
	integer		dim
	integer		source(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	integer		result(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	logical		stop(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	integer		i, j, k, l, m, n, p
!	integer		hi(6), low(6)
!	integer		pe_to, pe_from
!	integer		i1, j1, k1
	integer		carry, init_val
!	integer		tcarry
!	integer		carryin, carryout
!	integer		ack, gotit, zero
!	integer		comin, comout, comsize
*
	integer		FUNC, arg1, arg2
*
!	common		/inint@/ carryin, comin
!	common		/outint@/ carryout, comout
*
cdir$	parallel_only
cdir$	unknown_shared	source, stop, result
*
	FUNC (arg1,arg2) = arg1 + arg2
*
!	ack = 0
!	gotit = 1
!	zero = 0
!	comsize = 1
!	comout = 1
!	comin = 0
!cdir$	barrier
	init_val = 0
*
cdir$	master
	if (dim .eq. 0) then
	    include "segscan_s6_dim0_nomask.fh"
	else if (dim .eq. 1) then
	    include "segscan_s6_dim1_nomask.fh"
	else if (dim .eq. 2) then
	    include "segscan_s6_dim2_nomask.fh"
	else if (dim .eq. 3) then
	    include "segscan_s6_dim3_nomask.fh"
	else if (dim .eq. 4) then
	    include "segscan_s6_dim4_nomask.fh"
	else if (dim .eq. 5) then
	    include "segscan_s6_dim5_nomask.fh"
	else
	    include "segscan_s6_dim6_nomask.fh"
	endif
cdir$	end master
*
	return
	end


	subroutine SCANSUM_MASK_SP6@(result,source,stop,dim,mask,sx,blkcnt)
*
************************************************************************
*
* Purpose:	Find the sum of the elements identified by mask,
*		while saving all intermediate results.  This routine handles
*		real source arrays of rank 6. It returns an array result.
*		This routine is called by the C routine _SCANSUM_SP6.
*
* Arguments:	result	- real array of rank 6.
*		source	- real array of rank 6.
*		stop	- logical array of rank 1.
*		dim	- integer scalar: dimension to search along.
*		mask	- logical array of rank 6.
*		sx	- integer array of rank 1:
*			  extents of the source array.
*		blkcnt	- integer array of rank 1:
*			  number of blocks for each dimension.
*
* Variables:	hi	- integer scalar: lowest index in each block
*		low	- integer scalar: highext index in each block
*		carryout- carry to be passed to next block.
*		carryin	- carry received from previous block.
*		carry	- working variable to save current value of carry
*		init_val- initial value of carry variable
*		ack	- message acknowledged flag
*		gotit	- set value for acknowledge
*		comin	- carry receipt flag
*		comout	- carry receipt value passed to receiving PE
*		pe_to	- PE to send carry results to
*		pe_from	- PE from whence carry results arrive
*		zero	- the value 0
*		comsize	- size of comm block
*
* Intrinsic functions called:
*		my_pe()
*		hiidx()
*		lowidx()
*		home()
*
* Documentation:
*		T3D Fortran 90 Array Intrinsics Interface Description
*		T3D Fortran 90 Array Intrinsics Design Description
*
* Author:	Kenneth Bussart
*		System Libraries Section
*		Cray Research, Inc.
*
************************************************************************
	implicit	none
!	intrinsic	my_pe, hiidx, lowidx, home
	integer		sx(6)
	integer		blkcnt(6)
!	integer		mype
	integer		dim
	real		source(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	real		result(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	logical		stop(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	logical		mask(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	integer		i, j, k, l, m, n, p
!	integer		hi(6), low(6)
!	integer		pe_to, pe_from
!	integer		i1, j1, k1
	real		carry, init_val
!	real		tcarry
!	real		carryin, carryout
!	integer		ack, gotit, zero
!	integer		comin, comout, comsize
*
	real		FUNC, arg1, arg2
*
!	common		/inflt@/ carryin, comin
!	common		/outflt@/ carryout, comout
*
cdir$	parallel_only
cdir$	unknown_shared	source, stop, mask, result
*
	FUNC (arg1,arg2) = arg1 + arg2
*
!	ack = 0
!	gotit = 1
!	zero = 0
!	comsize = 1
!	comout = 1
!	comin = 0
!cdir$	barrier
	init_val = 0.0
*
cdir$	master
	if (dim .eq. 0) then
	    include "segscan_s6_dim0_mask.fh"
	else if (dim .eq. 1) then
	    include "segscan_s6_dim1_mask.fh"
	else if (dim .eq. 2) then
	    include "segscan_s6_dim2_mask.fh"
	else if (dim .eq. 3) then
	    include "segscan_s6_dim3_mask.fh"
	else if (dim .eq. 4) then
	    include "segscan_s6_dim4_mask.fh"
	else if (dim .eq. 5) then
	    include "segscan_s6_dim5_mask.fh"
	else
	    include "segscan_s6_dim6_mask.fh"
	endif
cdir$	end master
*
	return
	end


	subroutine SCANSUM_NOMASK_SP6@(result,source,stop,dim,sx,blkcnt)
*
************************************************************************
*
* Purpose:	Find the sum of the elements in the array section,
*		while saving all intermediate results.  This routine handles
*		real source arrays of rank 6.  It returns an array result.
*		This routine is called by the C routine _SCANSUM_SP6.
*
* Arguments:	result	- real array of rank 6.
*		source	- real array of rank 6.
*		stop	- logical array of rank 1.
*		dim	- integer scalar: dimension to search along.
*		sx	- integer array of rank 1:
*			  extents of the source array.
*		blkcnt	- integer array of rank 1:
*			  number of blocks for each dimension.
*
* Variables:	hi	- integer scalar: lowest index in each block
*		low	- integer scalar: highext index in each block
*		carryout- carry to be passed to next block.
*		carryin	- carry received from previous block.
*		carry	- working variable to save current value of carry
*		init_val- initial value of carry variable
*		ack	- message acknowledged flag
*		gotit	- set value for acknowledge
*		comin	- carry receipt flag
*		comout	- carry receipt value passed to receiving PE
*		pe_to	- PE to send carry results to
*		pe_from	- PE from whence carry results arrive
*		zero	- the value 0
*		comsize	- size of comm block
*
* Intrinsic functions called:
*		my_pe()
*		hiidx()
*		lowidx()
*		home()
*
* Documentation:
*		T3D Fortran 90 Array Intrinsics Interface Description
*		T3D Fortran 90 Array Intrinsics Design Description
*
* Author:	Kenneth Bussart
*		System Libraries Section
*		Cray Research, Inc.
*
************************************************************************
	implicit	none
!	intrinsic	my_pe, hiidx, lowidx, home
	integer		sx(6)
	integer		blkcnt(6)
!	integer		mype
	integer		dim
	real		source(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	real		result(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	logical		stop(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	integer		i, j, k, l, m, n, p
!	integer		hi(6), low(6)
!	integer		pe_to, pe_from
!	integer		i1, j1, k1
	real		carry, init_val
!	real		tcarry
!	real		carryin, carryout
!	integer		ack, gotit, zero
!	integer		comin, comout, comsize
*
	real		FUNC, arg1, arg2
*
!	common		/inflt@/ carryin, comin
!	common		/outflt@/ carryout, comout
*
cdir$	parallel_only
cdir$	unknown_shared	source, stop, result
*
	FUNC (arg1,arg2) = arg1 + arg2
*
!	ack = 0
!	gotit = 1
!	zero = 0
!	comsize = 1
!	comout = 1
!	comin = 0
!cdir$	barrier
	init_val = 0.0
*
cdir$	master
	if (dim .eq. 0) then
	    include "segscan_s6_dim0_nomask.fh"
	else if (dim .eq. 1) then
	    include "segscan_s6_dim1_nomask.fh"
	else if (dim .eq. 2) then
	    include "segscan_s6_dim2_nomask.fh"
	else if (dim .eq. 3) then
	    include "segscan_s6_dim3_nomask.fh"
	else if (dim .eq. 4) then
	    include "segscan_s6_dim4_nomask.fh"
	else if (dim .eq. 5) then
	    include "segscan_s6_dim5_nomask.fh"
	else
	    include "segscan_s6_dim6_nomask.fh"
	endif
cdir$	end master
*
	return
	end


	subroutine SCANSUM_MASK_CP6@(result,source,stop,dim,mask,sx,blkcnt)
*
************************************************************************
*
* Purpose:	Find the sum of the elements identified by mask,
*		while saving all intermediate results.  This routine handles
*		complex source arrays of rank 6. It returns an array result.
*		This routine is called by the C routine _SCANSUM_CP6.
*
* Arguments:	result	- complex array of rank 6.
*		source	- complex array of rank 6.
*		stop	- logical array of rank 1.
*		dim	- integer scalar: dimension to search along.
*		mask	- logical array of rank 6.
*		sx	- integer array of rank 1:
*			  extents of the source array.
*		blkcnt	- integer array of rank 1:
*			  number of blocks for each dimension.
*
* Variables:	hi	- integer scalar: lowest index in each block
*		low	- integer scalar: highext index in each block
*		carryout- carry to be passed to next block.
*		carryin	- carry received from previous block.
*		carry	- working variable to save current value of carry
*		init_val- initial value of carry variable
*		ack	- message acknowledged flag
*		gotit	- set value for acknowledge
*		comin	- carry receipt flag
*		comout	- carry receipt value passed to receiving PE
*		pe_to	- PE to send carry results to
*		pe_from	- PE from whence carry results arrive
*		zero	- the value 0
*		comsize	- size of comm block
*
* Intrinsic functions called:
*		my_pe()
*		hiidx()
*		lowidx()
*		home()
*
* Documentation:
*		T3D Fortran 90 Array Intrinsics Interface Description
*		T3D Fortran 90 Array Intrinsics Design Description
*
* Author:	Kenneth Bussart
*		System Libraries Section
*		Cray Research, Inc.
*
************************************************************************
	implicit	none
!	intrinsic	my_pe, hiidx, lowidx, home
	integer		sx(6)
	integer		blkcnt(6)
!	integer		mype
	integer		dim
	complex		source(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	complex		result(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	logical		stop(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	logical		mask(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	integer		i, j, k, l, m, n, p
!	integer		hi(6), low(6)
!	integer		pe_to, pe_from
!	integer		i1, j1, k1
	complex		carry, init_val
!	complex		tcarry
!	complex		carryin, carryout
!	integer		ack, gotit, zero
!	integer		comin, comout, comsize
*
	complex		FUNC, arg1, arg2
*
!	common		/incpx@/ carryin, comin
!	common		/outcpx@/ carryout, comout
*
cdir$	parallel_only
cdir$	unknown_shared	source, stop, mask, result
*
	FUNC (arg1,arg2) = arg1 + arg2
*
!	ack = 0
!	gotit = 1
!	zero = 0
!	comsize = 2
!	comout = 1
!	comin = 0
!cdir$	barrier
	init_val = cmplx (0.0,0.0)
*
cdir$	master
	if (dim .eq. 0) then
	    include "segscan_s6_dim0_mask.fh"
	else if (dim .eq. 1) then
	    include "segscan_s6_dim1_mask.fh"
	else if (dim .eq. 2) then
	    include "segscan_s6_dim2_mask.fh"
	else if (dim .eq. 3) then
	    include "segscan_s6_dim3_mask.fh"
	else if (dim .eq. 4) then
	    include "segscan_s6_dim4_mask.fh"
	else if (dim .eq. 5) then
	    include "segscan_s6_dim5_mask.fh"
	else
	    include "segscan_s6_dim6_mask.fh"
	endif
cdir$	end master
*
	return
	end


	subroutine SCANSUM_NOMASK_CP6@(result,source,stop,dim,sx,blkcnt)
*
************************************************************************
*
* Purpose:	Find the sum of the elements in the array section,
*		while saving all intermediate results.  This routine handles
*		integer source arrays of rank 6.  It returns an array result.
*		This routine is called by the C routine _SCANSUM_CP6.
*
* Arguments:	result	- integer array of rank 6.
*		source	- integer array of rank 6.
*		stop	- logical array of rank 1.
*		dim	- integer scalar: dimension to search along.
*		sx	- integer array of rank 1:
*			  extents of the source array.
*		blkcnt	- integer array of rank 1:
*			  number of blocks for each dimension.
*
* Variables:	hi	- integer scalar: lowest index in each block
*		low	- integer scalar: highext index in each block
*		carryout- carry to be passed to next block.
*		carryin	- carry received from previous block.
*		carry	- working variable to save current value of carry
*		init_val- initial value of carry variable
*		ack	- message acknowledged flag
*		gotit	- set value for acknowledge
*		comin	- carry receipt flag
*		comout	- carry receipt value passed to receiving PE
*		pe_to	- PE to send carry results to
*		pe_from	- PE from whence carry results arrive
*		zero	- the value 0
*		comsize	- size of comm block
*
* Intrinsic functions called:
*		my_pe()
*		hiidx()
*		lowidx()
*		home()
*
* Documentation:
*		T3D Fortran 90 Array Intrinsics Interface Description
*		T3D Fortran 90 Array Intrinsics Design Description
*
* Author:	Kenneth Bussart
*		System Libraries Section
*		Cray Research, Inc.
*
************************************************************************
	implicit	none
!	intrinsic	my_pe, hiidx, lowidx, home
	integer		sx(6)
	integer		blkcnt(6)
!	integer		mype
	integer		dim
	complex		source(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	complex		result(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	logical		stop(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	integer		i, j, k, l, m, n, p
!	integer		hi(6), low(6)
!	integer		pe_to, pe_from
!	integer		i1, j1, k1
	complex		carry, init_val
!	complex		tcarry
!	complex		carryin, carryout
!	integer		ack, gotit, zero
!	integer		comin, comout, comsize
*
	complex		FUNC, arg1, arg2
*
!	common		/incpx@/ carryin, comin
!	common		/outcpx@/ carryout, comout
*
cdir$	parallel_only
cdir$	unknown_shared	source, stop, result
*
	FUNC (arg1,arg2) = arg1 + arg2
*
!	ack = 0
!	gotit = 1
!	zero = 0
!	comsize = 2
!	comout = 1
!	comin = 0
!cdir$	barrier
	init_val = cmplx (0.0,0.0)
*
cdir$	master
	if (dim .eq. 0) then
	    include "segscan_s6_dim0_nomask.fh"
	else if (dim .eq. 1) then
	    include "segscan_s6_dim1_nomask.fh"
	else if (dim .eq. 2) then
	    include "segscan_s6_dim2_nomask.fh"
	else if (dim .eq. 3) then
	    include "segscan_s6_dim3_nomask.fh"
	else if (dim .eq. 4) then
	    include "segscan_s6_dim4_nomask.fh"
	else if (dim .eq. 5) then
	    include "segscan_s6_dim5_nomask.fh"
	else
	    include "segscan_s6_dim6_nomask.fh"
	endif
cdir$	end master
*
	return
	end
