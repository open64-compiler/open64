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


	subroutine SCANSUM_MASK_JS6@(result,source,stop,dim,mask,sx)
*
CDIR$ ID "@(#) libfi/mpp_scan/scansum_s_6@.f	92.0	10/08/98 14:37:14"
************************************************************************
*
* Purpose:	Find the maximum value of the elements identified by mask,
*		while saving all intermediate results.  This routine handles
*		integer source arrays of rank 6. It returns an array result.
*		This routine is called by the C routine _SCANSUM_JS6.
*
* Arguments:	result	- integer array of rank 6.
*		source	- integer array of rank 6.
*		stop	- logical array of rank 1.
*		dim	- integer scalar: dimension to search along.
*		mask	- logical array of rank 6.
*		sx	- integer array of rank 1:
*			  extents of the source array.
*
* Variables:	carry	- working variable to save current value of carry
*		init_val- initial value of carry variable
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
	integer		sx(6)
	integer		dim
	integer		source(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	integer		result(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	logical		stop(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	logical		mask(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	integer		i, j, k, l, m, n, p
	integer		carry
	integer		init_val
*
	integer		FUNC, arg1, arg2
*
cdir$	serial_only
*
	FUNC (arg1,arg2) = arg1 + arg2
*
	init_val = 0
*
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
*
	return
	end


	subroutine SCANSUM_NOMASK_JS6@(result,source,stop,dim,sx)
*
************************************************************************
*
* Purpose:	Find the maximum value of the elements in the array section,
*		while saving all intermediate results.  This routine handles
*		integer source arrays of rank 6.  It returns an array result.
*		This routine is called by the C routine _SCANSUM_JS6.
*
* Arguments:	result	- integer array of rank 6.
*		source	- integer array of rank 6.
*		stop	- logical array of rank 1.
*		dim	- integer scalar: dimension to search along.
*		sx	- integer array of rank 1:
*			  extents of the source array.
*
* Variables:	carry	- working variable to save current value of carry
*		init_val- initial value of carry variable
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
	integer		sx(6)
	integer		dim
	integer		source(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	integer		result(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	logical		stop(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	integer		i, j, k, l, m, n, p
	integer		carry
	integer		init_val
*
	integer		FUNC, arg1, arg2
*
cdir$	serial_only
*
	FUNC (arg1,arg2) = arg1 + arg2
*
	init_val = 0
*
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
*
	return
	end


	subroutine SCANSUM_MASK_SS6@(result,source,stop,dim,mask,sx)
*
************************************************************************
*
* Purpose:	Find the maximum value of the elements identified by mask,
*		while saving all intermediate results.  This routine handles
*		real source arrays of rank 6. It returns an array result.
*		This routine is called by the C routine _SCANSUM_SS6.
*
* Arguments:	result	- real array of rank 6.
*		source	- real array of rank 6.
*		stop	- logical array of rank 1.
*		dim	- integer scalar: dimension to search along.
*		mask	- logical array of rank 6.
*		sx	- integer array of rank 1:
*			  extents of the source array.
*
* Variables:	carry	- working variable to save current value of carry
*		init_val- initial value of carry variable
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
	integer		sx(6)
	integer		dim
	real		source(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	real		result(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	logical		stop(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	logical		mask(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	integer		i, j, k, l, m, n, p
	real		carry
	real		init_val
*
	real		FUNC, arg1, arg2
*
cdir$	serial_only
*
	FUNC (arg1,arg2) = arg1 + arg2
*
	init_val = 0.0
*
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
*
	return
	end


	subroutine SCANSUM_NOMASK_SS6@(result,source,stop,dim,sx)
*
************************************************************************
*
* Purpose:	Find the maximum value of the elements in the array section,
*		while saving all intermediate results.  This routine handles
*		real source arrays of rank 6.  It returns an array result.
*		This routine is called by the C routine _SCANSUM_SS6.
*
* Arguments:	result	- real array of rank 6.
*		source	- real array of rank 6.
*		stop	- logical array of rank 1.
*		dim	- integer scalar: dimension to search along.
*		sx	- integer array of rank 1:
*			  extents of the source array.
*
* Variables:	carry	- working variable to save current value of carry
*		init_val- initial value of carry variable
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
	integer		sx(6)
	integer		dim
	real		source(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	real		result(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	logical		stop(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	integer		i, j, k, l, m, n, p
	real		carry
	real		init_val
*
	real		FUNC, arg1, arg2
*
cdir$	serial_only
*
	FUNC (arg1,arg2) = arg1 + arg2
*
	init_val = 0.0
*
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
*
	return
	end


	subroutine SCANSUM_MASK_CS6@(result,source,stop,dim,mask,sx)
*
************************************************************************
*
* Purpose:	Find the maximum value of the elements identified by mask,
*		while saving all intermediate results.  This routine handles
*		complex source arrays of rank 6. It returns an array result.
*		This routine is called by the C routine _SCANSUM_CS6.
*
* Arguments:	result	- complex array of rank 6.
*		source	- complex array of rank 6.
*		stop	- logical array of rank 1.
*		dim	- integer scalar: dimension to search along.
*		mask	- logical array of rank 6.
*		sx	- integer array of rank 1:
*			  extents of the source array.
*
* Variables:	carry	- working variable to save current value of carry
*		init_val- initial value of carry variable
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
	integer		sx(6)
	integer		dim
	complex		source(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	complex		result(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	logical		stop(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	logical		mask(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	integer		i, j, k, l, m, n, p
	complex		carry
	complex		init_val
*
	complex		FUNC, arg1, arg2
*
cdir$	serial_only
*
	FUNC (arg1,arg2) = arg1 + arg2
*
	init_val = cmplx (0.0,0.0)
*
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
*
	return
	end


	subroutine SCANSUM_NOMASK_CS6@(result,source,stop,dim,sx)
*
************************************************************************
*
* Purpose:	Find the maximum value of the elements in the array section,
*		while saving all intermediate results.  This routine handles
*		integer source arrays of rank 6.  It returns an array result.
*		This routine is called by the C routine _SCANSUM_CS6.
*
* Arguments:	result	- integer array of rank 6.
*		source	- integer array of rank 6.
*		stop	- logical array of rank 1.
*		dim	- integer scalar: dimension to search along.
*		sx	- integer array of rank 1:
*			  extents of the source array.
*
* Variables:	carry	- working variable to save current value of carry
*		init_val- initial value of carry variable
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
	integer		sx(6)
	integer		dim
	complex		source(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	complex		result(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	logical		stop(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
	integer		i, j, k, l, m, n, p
	complex		carry
	complex		init_val
*
	complex		FUNC, arg1, arg2
*
cdir$	serial_only
*
	FUNC (arg1,arg2) = arg1 + arg2
*
	init_val = cmplx (0.0,0.0)
*
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
*
	return
	end


!	subroutine SCANSUM_SH_MASK_JS6@(result,source,stop,dim,mask,sx)
!*
!************************************************************************
!*
!* Purpose:	Find the maximum value of the elements identified by mask,
!*		while saving all intermediate results.  This routine handles
!*		integer source arrays of rank 6. It returns an array result.
!*		This routine is called by the C routine _SCANSUM_JS6.
!*
!* Arguments:	result	- integer array of rank 6.
!*		source	- integer array of rank 6.
!*		stop	- integer array of rank 2.
!*		dim	- integer scalar: dimension to search along.
!*		mask	- logical array of rank 6.
!*		sx	- integer array of rank 1:
!*			  extents of the source array.
!*
!* Variables:	carry	- working variable to save current value of carry
!*		init_val- initial value of carry variable
!*
!* Intrinsic functions called:
!*		my_pe()
!*		hiidx()
!*		lowidx()
!*		home()
!*
!* Documentation:
!*		T3D Fortran 90 Array Intrinsics Interface Description
!*		T3D Fortran 90 Array Intrinsics Design Description
!*
!* Author:	Kenneth Bussart
!*		System Libraries Section
!*		Cray Research, Inc.
!*
!************************************************************************
!	implicit	none
!	integer		sx(6)
!	integer		dim
!	integer		source(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
!	integer		result(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
!	logical		stop(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
!	logical		mask(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
!	integer		i, j, k, l, m, n, p
!	integer		carry
!	integer		init_val
!*
!	integer		FUNC, arg1, arg2
!*
!cdir$	serial_only
!cdir$	unknown_shared	source, stop, mask
!*
!	FUNC (arg1,arg2) = arg1 + arg2
!*
!	init_val = 0
!*
!	if (dim .eq. 0) then
!	    include "segscan_s6_dim0_mask.fh"
!	else if (dim .eq. 1) then
!	    include "segscan_s6_dim1_mask.fh"
!	else if (dim .eq. 2) then
!	    include "segscan_s6_dim2_mask.fh"
!	else if (dim .eq. 3) then
!	    include "segscan_s6_dim3_mask.fh"
!	else if (dim .eq. 4) then
!	    include "segscan_s6_dim4_mask.fh"
!	else if (dim .eq. 5) then
!	    include "segscan_s6_dim5_mask.fh"
!	else
!	    include "segscan_s6_dim6_mask.fh"
!	endif
!*
!	return
!	end
!
!
!	subroutine SCANSUM_SH_NOMASK_JS6@(result,source,stop,dim,sx)
!*
!************************************************************************
!*
!* Purpose:	Find the maximum value of the elements in the array section,
!*		while saving all intermediate results.  This routine handles
!*		integer source arrays of rank 6.  It returns an array result.
!*		This routine is called by the C routine _SCANSUM_JS6.
!*
!* Arguments:	result	- integer array of rank 6.
!*		source	- integer array of rank 6.
!*		stop	- integer array of rank 2.
!*		dim	- integer scalar: dimension to search along.
!*		sx	- integer array of rank 1:
!*			  extents of the source array.
!*
!* Variables:	carry	- working variable to save current value of carry
!*		init_val- initial value of carry variable
!*
!* Intrinsic functions called:
!*		my_pe()
!*		hiidx()
!*		lowidx()
!*		home()
!*
!* Documentation:
!*		T3D Fortran 90 Array Intrinsics Interface Description
!*		T3D Fortran 90 Array Intrinsics Design Description
!*
!* Author:	Kenneth Bussart
!*		System Libraries Section
!*		Cray Research, Inc.
!*
!************************************************************************
!	implicit	none
!	integer		sx(6)
!	integer		dim
!	integer		source(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
!	integer		result(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
!	logical		stop(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
!	integer		i, j, k, l, m, n, p
!	integer		carry
!	integer		init_val
!*
!	integer		FUNC, arg1, arg2
!*
!cdir$	serial_only
!cdir$	unknown_shared	source
!*
!	FUNC (arg1,arg2) = arg1 + arg2
!*
!	init_val = 0
!*
!	if (dim .eq. 0) then
!	    include "segscan_s6_dim0_nomask.fh"
!	else if (dim .eq. 1) then
!	    include "segscan_s6_dim1_nomask.fh"
!	else if (dim .eq. 2) then
!	    include "segscan_s6_dim2_nomask.fh"
!	else if (dim .eq. 3) then
!	    include "segscan_s6_dim3_nomask.fh"
!	else if (dim .eq. 4) then
!	    include "segscan_s6_dim4_nomask.fh"
!	else if (dim .eq. 5) then
!	    include "segscan_s6_dim5_nomask.fh"
!	else
!	    include "segscan_s6_dim6_nomask.fh"
!	endif
!*
!	return
!	end
!
!
!	subroutine SCANSUM_SH_MASK_SS6@(result,source,stop,dim,mask,sx)
!*
!************************************************************************
!*
!* Purpose:	Find the maximum value of the elements identified by mask,
!*		while saving all intermediate results.  This routine handles
!*		real source arrays of rank 6. It returns an array result.
!*		This routine is called by the C routine _SCANSUM_SS6.
!*
!* Arguments:	result	- real array of rank 6.
!*		source	- real array of rank 6.
!*		stop	- integer array of rank 2.
!*		dim	- integer scalar: dimension to search along.
!*		mask	- logical array of rank 6.
!*		sx	- integer array of rank 1:
!*			  extents of the source array.
!*
!* Variables:	carry	- working variable to save current value of carry
!*		init_val- initial value of carry variable
!*
!* Intrinsic functions called:
!*		my_pe()
!*		hiidx()
!*		lowidx()
!*		home()
!*
!* Documentation:
!*		T3D Fortran 90 Array Intrinsics Interface Description
!*		T3D Fortran 90 Array Intrinsics Design Description
!*
!* Author:	Kenneth Bussart
!*		System Libraries Section
!*		Cray Research, Inc.
!*
!************************************************************************
!	implicit	none
!	integer		sx(6)
!	integer		dim
!	real		source(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
!	real		result(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
!	logical		stop(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
!	logical		mask(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
!	integer		i, j, k, l, m, n, p
!	real		carry
!	real		init_val
!*
!	real		FUNC, arg1, arg2
!*
!cdir$	serial_only
!cdir$	unknown_shared	source, stop, mask
!*
!	FUNC (arg1,arg2) = arg1 + arg2
!*
!	init_val = 0.0
!*
!	if (dim .eq. 0) then
!	    include "segscan_s6_dim0_mask.fh"
!	else if (dim .eq. 1) then
!	    include "segscan_s6_dim1_mask.fh"
!	else if (dim .eq. 2) then
!	    include "segscan_s6_dim2_mask.fh"
!	else if (dim .eq. 3) then
!	    include "segscan_s6_dim3_mask.fh"
!	else if (dim .eq. 4) then
!	    include "segscan_s6_dim4_mask.fh"
!	else if (dim .eq. 5) then
!	    include "segscan_s6_dim5_mask.fh"
!	else
!	    include "segscan_s6_dim6_mask.fh"
!	endif
!*
!	return
!	end
!
!
!	subroutine SCANSUM_SH_NOMASK_SS6@(result,source,stop,dim,sx)
!*
!************************************************************************
!*
!* Purpose:	Find the maximum value of the elements in the array section,
!*		while saving all intermediate results.  This routine handles
!*		real source arrays of rank 6.  It returns an array result.
!*		This routine is called by the C routine _SCANSUM_SS6.
!*
!* Arguments:	result	- real array of rank 6.
!*		source	- real array of rank 6.
!*		stop	- integer array of rank 2.
!*		dim	- integer scalar: dimension to search along.
!*		sx	- integer array of rank 1:
!*			  extents of the source array.
!*
!* Variables:	carry	- working variable to save current value of carry
!*		init_val- initial value of carry variable
!*
!* Intrinsic functions called:
!*		my_pe()
!*		hiidx()
!*		lowidx()
!*		home()
!*
!* Documentation:
!*		T3D Fortran 90 Array Intrinsics Interface Description
!*		T3D Fortran 90 Array Intrinsics Design Description
!*
!* Author:	Kenneth Bussart
!*		System Libraries Section
!*		Cray Research, Inc.
!*
!************************************************************************
!	implicit	none
!	integer		sx(6)
!	integer		dim
!	real		source(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
!	real		result(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
!	logical		stop(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
!	integer		i, j, k, l, m, n, p
!	real		carry
!	real		init_val
!*
!	real		FUNC, arg1, arg2
!*
!cdir$	serial_only
!cdir$	unknown_shared	source
!*
!	FUNC (arg1,arg2) = arg1 + arg2
!*
!	init_val = 0.0
!*
!	if (dim .eq. 0) then
!	    include "segscan_s6_dim0_nomask.fh"
!	else if (dim .eq. 1) then
!	    include "segscan_s6_dim1_nomask.fh"
!	else if (dim .eq. 2) then
!	    include "segscan_s6_dim2_nomask.fh"
!	else if (dim .eq. 3) then
!	    include "segscan_s6_dim3_nomask.fh"
!	else if (dim .eq. 4) then
!	    include "segscan_s6_dim4_nomask.fh"
!	else if (dim .eq. 5) then
!	    include "segscan_s6_dim5_nomask.fh"
!	else
!	    include "segscan_s6_dim6_nomask.fh"
!	endif
!*
!	return
!	end
!
!
!	subroutine SCANSUM_SH_NOMASK_CS6@(result,source,stop,dim,sx)
!*
!************************************************************************
!*
!* Purpose:	Find the maximum value of the elements in the array section,
!*		while saving all intermediate results.  This routine handles
!*		integer source arrays of rank 6.  It returns an array result.
!*		This routine is called by the C routine _SCANSUM_CS6.
!*
!* Arguments:	result	- integer array of rank 6.
!*		source	- integer array of rank 6.
!*		stop	- integer array of rank 2.
!*		dim	- integer scalar: dimension to search along.
!*		sx	- integer array of rank 1:
!*			  extents of the source array.
!*
!* Variables:	carry	- working variable to save current value of carry
!*		init_val- initial value of carry variable
!*
!* Intrinsic functions called:
!*		my_pe()
!*		hiidx()
!*		lowidx()
!*		home()
!*
!* Documentation:
!*		T3D Fortran 90 Array Intrinsics Interface Description
!*		T3D Fortran 90 Array Intrinsics Design Description
!*
!* Author:	Kenneth Bussart
!*		System Libraries Section
!*		Cray Research, Inc.
!*
!************************************************************************
!	implicit	none
!	integer		sx(6)
!	integer		dim
!	complex		source(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
!	complex		result(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
!	logical		stop(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
!	integer		i, j, k, l, m, n, p
!	complex		carry
!	complex		init_val
!*
!	complex		FUNC, arg1, arg2
!*
!cdir$	serial_only
!cdir$	unknown_shared	source
!*
!	FUNC (arg1,arg2) = arg1 + arg2
!*
!	init_val = cmplx (0.0,0.0)
!*
!	if (dim .eq. 0) then
!	    include "segscan_s6_dim0_nomask.fh"
!	else if (dim .eq. 1) then
!	    include "segscan_s6_dim1_nomask.fh"
!	else if (dim .eq. 2) then
!	    include "segscan_s6_dim2_nomask.fh"
!	else if (dim .eq. 3) then
!	    include "segscan_s6_dim3_nomask.fh"
!	else if (dim .eq. 4) then
!	    include "segscan_s6_dim4_nomask.fh"
!	else if (dim .eq. 5) then
!	    include "segscan_s6_dim5_nomask.fh"
!	else
!	    include "segscan_s6_dim6_nomask.fh"
!	endif
!*
!	return
!	end
!
!
!	subroutine SCANSUM_SH_MASK_CS6@(result,source,stop,dim,mask,sx)
!*
!************************************************************************
!*
!* Purpose:	Find the maximum value of the elements identified by mask,
!*		while saving all intermediate results.  This routine handles
!*		complex source arrays of rank 6. It returns an array result.
!*		This routine is called by the C routine _SCANSUM_CS6.
!*
!* Arguments:	result	- complex array of rank 6.
!*		source	- complex array of rank 6.
!*		stop	- integer array of rank 2.
!*		dim	- integer scalar: dimension to search along.
!*		mask	- logical array of rank 6.
!*		sx	- integer array of rank 1:
!*			  extents of the source array.
!*
!* Variables:	carry	- working variable to save current value of carry
!*		init_val- initial value of carry variable
!*
!* Intrinsic functions called:
!*		my_pe()
!*		hiidx()
!*		lowidx()
!*		home()
!*
!* Documentation:
!*		T3D Fortran 90 Array Intrinsics Interface Description
!*		T3D Fortran 90 Array Intrinsics Design Description
!*
!* Author:	Kenneth Bussart
!*		System Libraries Section
!*		Cray Research, Inc.
!*
!************************************************************************
!	implicit	none
!	integer		sx(6)
!	integer		dim
!	complex		source(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
!	complex		result(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
!	logical		stop(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
!	logical		mask(sx(1),sx(2),sx(3),sx(4),sx(5),sx(6))
!	integer		i, j, k, l, m, n, p
!	complex		carry
!	complex		init_val
!*
!	complex		FUNC, arg1, arg2
!*
!cdir$	serial_only
!cdir$	unknown_shared	source, stop, mask
!*
!	FUNC (arg1,arg2) = arg1 + arg2
!*
!	init_val = cmplx (0.0,0.0)
!*
!	if (dim .eq. 0) then
!	    include "segscan_s6_dim0_mask.fh"
!	else if (dim .eq. 1) then
!	    include "segscan_s6_dim1_mask.fh"
!	else if (dim .eq. 2) then
!	    include "segscan_s6_dim2_mask.fh"
!	else if (dim .eq. 3) then
!	    include "segscan_s6_dim3_mask.fh"
!	else if (dim .eq. 4) then
!	    include "segscan_s6_dim4_mask.fh"
!	else if (dim .eq. 5) then
!	    include "segscan_s6_dim5_mask.fh"
!	else
!	    include "segscan_s6_dim6_mask.fh"
!	endif
!*
!	return
!	end
!
!
