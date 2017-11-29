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


	subroutine PRESUM_MASK_JS1@(result,source,dim,mask,sext)
*
CDIR$ ID "@(#) libfi/mpp_pre/presum_s_1@.f	92.0	10/08/98 14:37:14"
************************************************************************
*
* Purpose:	Find the maximum value of the elements identified by mask,
*		while saving all intermediate results.  This routine handles
*		integer source arrays of rank 1. It returns an array result.
*		This routine is called by the C routine _PRESUM_JS1.
*
* Arguments:	result	- integer array of rank 1.
*		source	- integer array of rank 1.
*		dim	- integer scalar: dimension to search along.
*		mask	- logical array of rank 1.
*		sext	- integer array of rank 1:
*			  extents of the source array.
*
* Variables:	carry	- working variable to save current value of carry
*		init_val- initial value of carry variable
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
	integer		sext(1)
	integer		dim
	integer		source(sext(1))
	integer		result(sext(1))
	logical		mask(sext(1))
	integer		init_val
	integer		carry
	integer		i, j, k
*
	integer		FUNC, arg1, arg2
*
cdir$	serial_only
*
	FUNC (arg1,arg2) = arg1 + arg2
*
	init_val = 0
*
	include "parapre_s1_dim0_mask.fh"
*
	return
	end


	subroutine PRESUM_NOMASK_JS1@(result,source,dim,sext)
*
************************************************************************
*
* Purpose:	Find the maximum value of the elements in the array section,
*		while saving all intermediate results.  This routine handles
*		integer source arrays of rank 1.  It returns an array result.
*		This routine is called by the C routine _PRESUM_JS1.
*
* Arguments:	result	- integer array of rank 1.
*		source	- integer array of rank 1.
*		dim	- integer scalar: dimension to search along.
*		sext	- integer array of rank 1:
*			  extents of the source array.
*
* Variables:	carry	- working variable to save current value of carry
*		init_val- initial value of carry variable
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
	integer		sext(1)
	integer		dim
	integer		source(sext(1))
	integer		result(sext(1))
	integer		init_val
	integer		carry
	integer		i, j, k
	integer		loc
*
	integer		FUNC, arg1, arg2
*
cdir$	serial_only
*
	FUNC (arg1,arg2) = arg1 + arg2
*
	init_val = 0
*
	include "parapre_s1_dim0_nomask.fh"
*

	return
	end


	subroutine PRESUM_MASK_SS1@(result,source,dim,mask,sext)
*
************************************************************************
*
* Purpose:	Find the maximum value of the elements identified by mask,
*		while saving all intermediate results.  This routine handles
*		real source arrays of rank 1. It returns an array result.
*		This routine is called by the C routine _PRESUM_SS1.
*
* Arguments:	result	- real array of rank 1.
*		source	- real array of rank 1.
*		dim	- integer scalar: dimension to search along.
*		mask	- logical array of rank 1.
*		sext	- integer array of rank 1:
*			  extents of the source array.
*
* Variables:	carry	- working variable to save current value of carry
*		init_val- initial value of carry variable
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
	integer		sext(1)
	integer		dim
	real		source(sext(1))
	real		result(sext(1))
	logical		mask(sext(1))
	real		init_val
	real		carry
	integer		i, j, k
*
	real		FUNC, arg1, arg2
*
cdir$	serial_only
*
	FUNC (arg1,arg2) = arg1 + arg2
*
	init_val = 0.0
*
	include "parapre_s1_dim0_mask.fh"
*
	return
	end


	subroutine PRESUM_NOMASK_SS1@(result,source,dim,sext)
*
************************************************************************
*
* Purpose:	Find the maximum value of the elements in the array section,
*		while saving all intermediate results.  This routine handles
*		real source arrays of rank 1.  It returns an array result.
*		This routine is called by the C routine _PRESUM_SS1.
*
* Arguments:	result	- real array of rank 1.
*		source	- real array of rank 1.
*		dim	- integer scalar: dimension to search along.
*		sext	- integer array of rank 1:
*			  extents of the source array.
*
* Variables:	carry	- working variable to save current value of carry
*		init_val- initial value of carry variable
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
	integer		sext(1)
	integer		dim
	real		source(sext(1))
	real		result(sext(1))
	real		init_val
	real		carry
	integer		i, j, k
*
	real		FUNC, arg1, arg2
*
cdir$	serial_only
*
	FUNC (arg1,arg2) = arg1 + arg2
*
	init_val = 0.0
*
	include "parapre_s1_dim0_nomask.fh"
*

	return
	end


	subroutine PRESUM_MASK_CS1@(result,source,dim,mask,sext)
*
************************************************************************
*
* Purpose:	Find the maximum value of the elements identified by mask,
*		while saving all intermediate results.  This routine handles
*		complex source arrays of rank 1. It returns an array result.
*		This routine is called by the C routine _PRESUM_CS1.
*
* Arguments:	result	- complex array of rank 1.
*		source	- complex array of rank 1.
*		dim	- integer scalar: dimension to search along.
*		mask	- logical array of rank 1.
*		sext	- integer array of rank 1:
*			  extents of the source array.
*
* Variables:	carry	- working variable to save current value of carry
*		init_val- initial value of carry variable
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
	integer		sext(1)
	integer		dim
	complex		source(sext(1))
	complex		result(sext(1))
	logical		mask(sext(1))
	complex		init_val
	complex		carry
	integer		i, j, k
*
	complex		FUNC, arg1, arg2
*
cdir$	serial_only
*
	FUNC (arg1,arg2) = arg1 + arg2
*
	init_val = cmplx (0.0,0.0)
*
	include "parapre_s1_dim0_mask.fh"
*
	return
	end


	subroutine PRESUM_NOMASK_CS1@(result,source,dim,sext)
*
************************************************************************
*
* Purpose:	Find the maximum value of the elements in the array section,
*		while saving all intermediate results.  This routine handles
*		complex source arrays of rank 1.  It returns an array result.
*		This routine is called by the C routine _PRESUM_CS1.
*
* Arguments:	result	- complex array of rank 1.
*		source	- complex array of rank 1.
*		dim	- integer scalar: dimension to search along.
*		sext	- integer array of rank 1:
*			  extents of the source array.
*
* Variables:	carry	- working variable to save current value of carry
*		init_val- initial value of carry variable
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
	integer		sext(1)
	integer		dim
	complex		source(sext(1))
	complex		result(sext(1))
	complex		init_val
	complex		carry
	integer		i, j, k
*
	complex		FUNC, arg1, arg2
*
cdir$	serial_only
*
	FUNC (arg1,arg2) = arg1 + arg2
*
	init_val = cmplx (0.0,0.0)
*
	include "parapre_s1_dim0_nomask.fh"
*

	return
	end


!	subroutine PRESUM_SH_MASK_JS1@(result,source,dim,mask,sext)
!*
!************************************************************************
!*
!* Purpose:	Find the maximum value of the elements identified by mask,
!*		while saving all intermediate results.  This routine handles
!*		integer source arrays of rank 1. It returns an array result.
!*		This routine is called by the C routine _PRESUM_JS1.
!*
!* Arguments:	result	- integer array of rank 1.
!*		source	- integer array of rank 1.
!*		dim	- integer scalar: dimension to search along.
!*		mask	- logical array of rank 1.
!*		sext	- integer array of rank 1:
!*			  extents of the source array.
!*
!* Variables:	carry	- working variable to save current value of carry
!*		init_val- initial value of carry variable
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
!	integer		sext(1)
!	integer		dim
!	integer		source(sext(1))
!	integer		result(sext(1))
!	logical		mask(sext(1))
!	integer		init_val
!	integer		carry
!	integer		i, j, k
!*
!	integer		FUNC, arg1, arg2
!*
!cdir$	serial_only
!cdir$	unknown_shared	source, mask
!*
!	FUNC (arg1,arg2) = arg1 + arg2
!*
!	init_val = 0
!*
!	include "parapre_s1_dim0_mask.fh"
!*
!	return
!	end
!
!
!	subroutine PRESUM_SH_NOMASK_JS1@(result,source,dim,sext)
!*
!************************************************************************
!*
!* Purpose:	Find the maximum value of the elements in the array section,
!*		while saving all intermediate results.  This routine handles
!*		integer source arrays of rank 1.  It returns an array result.
!*		This routine is called by the C routine _PRESUM_JS1.
!*
!* Arguments:	result	- integer array of rank 1.
!*		source	- integer array of rank 1.
!*		dim	- integer scalar: dimension to search along.
!*		sext	- integer array of rank 1:
!*			  extents of the source array.
!*
!* Variables:	carry	- working variable to save current value of carry
!*		init_val- initial value of carry variable
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
!	integer		sext(1)
!	integer		dim
!	integer		source(sext(1))
!	integer		result(sext(1))
!	integer		init_val
!	integer		carry
!	integer		i, j, k
!*
!	integer		FUNC, arg1, arg2
!*
!!cdir$	serial_only
!cdir$	unknown_shared	source
!*
!	FUNC (arg1,arg2) = arg1 + arg2
!*
!	init_val = 0
!*
!	include "parapre_s1_dim0_nomask.fh"
!*
!
!	return
!	end
!
!
!	subroutine PRESUM_SH_MASK_SS1@(result,source,dim,mask,sext)
!*
!************************************************************************
!*
!* Purpose:	Find the maximum value of the elements identified by mask,
!*		while saving all intermediate results.  This routine handles
!*		real source arrays of rank 1. It returns an array result.
!*		This routine is called by the C routine _PRESUM_SS1.
!*
!* Arguments:	result	- real array of rank 1.
!*		source	- real array of rank 1.
!*		dim	- integer scalar: dimension to search along.
!*		mask	- logical array of rank 1.
!*		sext	- integer array of rank 1:
!*			  extents of the source array.
!*
!* Variables:	carry	- working variable to save current value of carry
!*		init_val- initial value of carry variable
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
!	integer		sext(1)
!	integer		dim
!	real		source(sext(1))
!	real		result(sext(1))
!	logical		mask(sext(1))
!	real		init_val
!	real		carry
!	integer		i, j, k
!*
!	real		FUNC, arg1, arg2
!*
!cdir$	serial_only
!cdir$	unknown_shared	source, mask
!*
!	FUNC (arg1,arg2) = arg1 + arg2
!*
!	init_val = 0.0
!*
!	include "parapre_s1_dim0_mask.fh"
!*
!	return
!	end
!
!
!	subroutine PRESUM_SH_NOMASK_SS1@(result,source,dim,sext)
!*
!************************************************************************
!*
!* Purpose:	Find the maximum value of the elements in the array section,
!*		while saving all intermediate results.  This routine handles
!*		real source arrays of rank 1.  It returns an array result.
!*		This routine is called by the C routine _PRESUM_SS1.
!*
!* Arguments:	result	- real array of rank 1.
!*		source	- real array of rank 1.
!*		dim	- integer scalar: dimension to search along.
!*		sext	- integer array of rank 1:
!*			  extents of the source array.
!*
!* Variables:	carry	- working variable to save current value of carry
!*		init_val- initial value of carry variable
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
!	integer		sext(1)
!	integer		dim
!	real		source(sext(1))
!	real		result(sext(1))
!	real		init_val
!	real		carry
!	integer		i, j, k
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
!	include "parapre_s1_dim0_nomask.fh"
!*
!
!	return
!	end
!
!
!	subroutine PRESUM_SH_MASK_CS1@(result,source,dim,mask,sext)
!*
!************************************************************************
!*
!* Purpose:	Find the maximum value of the elements identified by mask,
!*		while saving all intermediate results.  This routine handles
!*		complex source arrays of rank 1. It returns an array result.
!*		This routine is called by the C routine _PRESUM_CS1.
!*
!* Arguments:	result	- complex array of rank 1.
!*		source	- complex array of rank 1.
!*		dim	- integer scalar: dimension to search along.
!*		mask	- logical array of rank 1.
!*		sext	- integer array of rank 1:
!*			  extents of the source array.
!*
!* Variables:	carry	- working variable to save current value of carry
!*		init_val- initial value of carry variable
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
!	integer		sext(1)
!	integer		dim
!	complex		source(sext(1))
!	complex		result(sext(1))
!	logical		mask(sext(1))
!	complex		init_val
!	complex		carry
!	integer		i, j, k
!*
!	complex		FUNC, arg1, arg2
!*
!cdir$	serial_only
!cdir$	unknown_shared	source, mask
!*
!	FUNC (arg1,arg2) = arg1 + arg2
!*
!	init_val = cmplx (0.0,0.0)
!*
!	include "parapre_s1_dim0_mask.fh"
!*
!	return
!	end
!
!
!	subroutine PRESUM_SH_NOMASK_CS1@(result,source,dim,sext)
!*
!************************************************************************
!*
!* Purpose:	Find the maximum value of the elements in the array section,
!*		while saving all intermediate results.  This routine handles
!*		complex source arrays of rank 1.  It returns an array result.
!*		This routine is called by the C routine _PRESUM_CS1.
!*
!* Arguments:	result	- complex array of rank 1.
!*		source	- complex array of rank 1.
!*		dim	- integer scalar: dimension to search along.
!*		sext	- integer array of rank 1:
!*			  extents of the source array.
!*
!* Variables:	carry	- working variable to save current value of carry
!*		init_val- initial value of carry variable
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
!	integer		sext(1)
!	integer		dim
!	complex		source(sext(1))
!	complex		result(sext(1))
!	complex		init_val
!	complex		carry
!	integer		i, j, k
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
!	include "parapre_s1_dim0_nomask.fh"
!*
!
!	return
!	end
