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


	subroutine cshift_wd1_s1@(result, source, dim, sext, shftval)
*
CDIR$ ID "@(#) libfi/mpp_shift/cshift_s_1@.f	92.0	10/08/98 14:37:14"
*
**************************************************************************
*
* Purpose:	Perform a circular shift of an array in argument SOURCE
*		by SHIFT elements along dimension DIM.  This routine handles
*		integer source arrays of rank 1.  This routine is called
*		from the C routine _CSHIFT_JP1.
*
* Arguments:	result	- integer array of rank 1.
*		source	- integer array of rank 1.
*		dim	- integer scalar: dimension to shift over
*		sext	- integer array of rank 1.
*		shftval - integer scalar: scalar shift value (if scalar)
*
* Variables:	mype	- integer scalar: logical PE number.
*
* Intrinsic functions called:
*		my_pe()
*		in_parallel()
*
* Documentation:
*		T3D Fortran 90 Array Intrinsics Interface Description
*		T3D Fortran 90 Array Intrinsics Design Description
*************************************************************************
	intrinsic	my_pe, in_parallel, home
	integer		sext(1)
	integer		lshft, rshft
	integer		result(sext(1))
	integer		source(sext(1))
	integer		shift
	integer		dim
	integer		shftval
	integer		r_indx, s_indx
	integer		negsext
*
cdir$	unknown_shared	source
*
	include "cshift_s1_dim1@.fh"
*
	return
	end


	subroutine cshift_wd2_s1@(result, source, dim, sext, shftval)
*
**************************************************************************
*
* Purpose:	Perform a circular shift of an array in argument SOURCE
*		by SHIFT elements along dimension DIM.  This routine handles
*		complex source arrays of rank 1.  This routine is called
*		from the C routine _CSHIFT_CP1.
*
* Arguments:	result	- complex array of rank 1.
*		source	- complex array of rank 1.
*		shift	- integer array of rank 0.
*		dim	- integer scalar: dimension to shift over
*		sext	- integer array of rank 1.
*		shftval - integer scalar: scalar shift value (if scalar)
*		
*
* Variables:	mype	- integer scalar: logical PE number.
*
* Intrinsic functions called:
*		my_pe()
*		in_parallel()
*
* Documentation:
*		T3D Fortran 90 Array Intrinsics Interface Description
*		T3D Fortran 90 Array Intrinsics Design Description
*************************************************************************
	intrinsic	my_pe, in_parallel, home
	integer		sext(1)
	integer		lshft, rshft
	complex		result(sext(1))
	complex		source(sext(1))
	integer		shift
	integer		dim
	integer		shftval
	integer		r_indx, s_indx
	integer		negsext
*
cdir$	unknown_shared	source
*
	include "cshift_s1_dim1@.fh"
*
	return
	end
