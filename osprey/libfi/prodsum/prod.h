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


* USMID @(#) libfi/prodsum/prod.h	92.0	10/08/98 14:37:14 
*********************************************************************
*                                                                   *
*  Name:   PROD__x@                                                 *
*                                                                   *
*  Description:                                                     *
*          Header file for the Fortran 90 array intrinsic function  *
*          PRODUCT.  This file is included in all of the specific   *
*          PRODUCT intrinsics in which the result is an array.      * 
*                                                                   *
*  Arguments:                                                       *
*     dv_result    Dope vector describing the result array.         *
*                                                                   *
*     dv_array     Dope vector describing the array to prod         *
*                                                                   *
*     dim          integer (optional),                              *
*                  Dimension over which to prod                     *
*                                                                   *
*     dv_mask      dope vector (optional) describing MASK array.    *
*                                                                   *
*          Author:  Math Software Group, Cray Research, Inc.        *
*                                                                   *
*********************************************************************
*	.. arguments ..
	integer dv_result(*)	! dope vector for result
	integer dv_array(*)	! dope vector for array
	integer dim		! dimension on which to prod
	logical dv_mask(*)	! dope vector for mask

*	.. subscripts ..	
	integer i, j, k, ia, jb, km
	integer i1, i2, i3, i4, i5, i6, i7

*	.. dummy argument ..
	integer dummy

*	.. functions ..
	external presum@

	include "sumcom.h"	! include variables declaration

*	.. begin execution ..
*
*       Call routine PRESUM@ to parse the dope vectors and do preliminary
*       processing.  Always pass the same number of arguments to presum,
*       ie., dim and mask are present or are zero.  Pass 14 local 
*       arguments rather than use task common. The first argument is one
*       to indicate result is an array , not a scalar.
*
	call presum@(1, dv_result, dv_array, dim, dv_mask,
     +       ndim, dimarg, dimx, maskarg, scalar, loca, lima, inca,
     +       locb, limb, incb, locm, limm, incm, sizem, loc4m)
*
*	Assign addresses to pointers for arrays a, b, mask.
*
	p_a = loca
	if (.not. scalar) p_b = locb
	if (maskarg) p_mask = locm

	if (.not. maskarg) then
*
*	    Case: Mask argument IS NOT given
*
	    if	(ndim .eq. 2) then
*
*		Array is a matrix (2 dimensions).
*		Sum over first dimension.
*
		do, i1 = 0, limb(1) - 1
		    b(i1*incb(1)) = 1.0
		end do

		do, i1 = 0, lima(1) - 1
*-----------------------------------------------------------------------
		    ia = i1*inca(1)
		    do, i2 = 0, lima(2) - 1
			b(i2*incb(1)) = 
     &			b(i2*incb(1)) * a(i2*inca(2) + ia)
		    end do
*-----------------------------------------------------------------------
		end do

		return
	    else 
*
*		Array has more than 2 dimensions --
*	        treat it as though all 7 dimensions were
*		given.  Sum over first dimension.
*
*		b(i2,i3,i4,i5,i6,i7) = Sum a(i1,i2,i3,i4,i5,i6,i7)
*                                       i1
*		Vectorize on i2 (inner loop)
*
*		Fill in default values for limit and increment
*
		do, i = ndim + 1, maxdim
		    lima(i) = 1
		    inca(i) = 0
		end do
		do, i = ndim, maxdim
		    limb(i) = 1
		    incb(i) = 0
		end do
*
		do, i6 = 0, limb(6) - 1
		do, i5 = 0, limb(5) - 1
		do, i4 = 0, limb(4) - 1
		do, i3 = 0, limb(3) - 1
		do, i2 = 0, limb(2) - 1
*-----------------------------------------------------------------------
		    jb = i2*incb(2) + i3*incb(3) + i4*incb(4) +
     &			 i5*incb(5) + i6*incb(6)
		    do, i1 = 0, limb(1) - 1
			b(i1*incb(1) + jb) = 1.0
		    end do
*-----------------------------------------------------------------------
		end do
		end do
		end do
		end do
		end do
		
		do, i7 = 0, lima(7) - 1
		do, i6 = 0, lima(6) - 1
		do, i5 = 0, lima(5) - 1
		do, i4 = 0, lima(4) - 1
		do, i3 = 0, lima(3) - 1
		do, i1 = 0, lima(1) - 1
*-----------------------------------------------------------------------
		    jb = i3*incb(2) + i4*incb(3) + i5*incb(4) + 
     &			 i6*incb(5) + i7*incb(6)
		    ia = i1*inca(1) + i3*inca(3) + i4*inca(4) + 
     &			 i5*inca(5) + i6*inca(6) + i7*inca(7)
		    do, i2 = 0, lima(2) - 1
			b(i2*incb(1) + jb) = 
     &			b(i2*incb(1) + jb) * a(i2*inca(2) + ia)
		    end do
*-----------------------------------------------------------------------
		end do
		end do
		end do
		end do
		end do
		end do
	    return

	    end if

	else if (maskarg) then
*
*	    Case: Result is an array, mask argument IS given
*
	    if	(ndim .eq. 2) then
*
*		Array is a matrix (2 dimensions).
*		Sum over first dimension.
*
		do, i1 = 0, limb(1) - 1
		    b(i1*incb(1)) = 1.0
		end do

		do, i1 = 0, lima(1) - 1
*-----------------------------------------------------------------------
		    ia = i1*inca(1)
		    km = i1*incm(1)
		    do, i2 = 0, lima(2) - 1
			if ( mask(i2*incm(2) + km) ) then
			    b(i2*incb(1)) = 
     &			    b(i2*incb(1)) * a(i2*inca(2) + ia)
			end if
		    end do
*-----------------------------------------------------------------------
		end do

		return
	    else 
*
*		Array has more than 2 dimensions --
*	        treat it as though all 7 dimensions were
*		given.  Sum over first dimension.
*
*		b(i2,i3,i4,i5,i6,i7) = Sum a(i1,i2,i3,i4,i5,i6,i7)
*                                       i1
*
*		Fill in default values for limit and increment
*
		do, i = ndim + 1, maxdim
		    lima(i) = 1
		    inca(i) = 0
		    limm(i) = 1
		    incm(i) = 0
		end do
		do, i = ndim, maxdim
		    limb(i) = 1
		    incb(i) = 0
		end do
*
*		Vectorize on i2 (inner loop)
*
		do, i6 = 0, limb(6) - 1
		do, i5 = 0, limb(5) - 1
		do, i4 = 0, limb(4) - 1
		do, i3 = 0, limb(3) - 1
		do, i2 = 0, limb(2) - 1
*-----------------------------------------------------------------------
		    jb = i2*incb(2) + i3*incb(3) + i4*incb(4) +
     &			 i5*incb(5) + i6*incb(6)
		    do, i1 = 0, limb(1) - 1
			b(i1*incb(1) + jb) = 1.0
		    end do
*-----------------------------------------------------------------------
		end do
		end do
		end do
		end do
		end do
		
		do, i7 = 0, lima(7) - 1
		do, i6 = 0, lima(6) - 1
		do, i5 = 0, lima(5) - 1
		do, i4 = 0, lima(4) - 1
		do, i3 = 0, lima(3) - 1
		do, i1 = 0, lima(1) - 1
*-----------------------------------------------------------------------
		    jb = i3*incb(2) + i4*incb(3) + i5*incb(4) + 
     &			 i6*incb(5) + i7*incb(6)
		    ia = i1*inca(1) + i3*inca(3) + i4*inca(4) + 
     &			 i5*inca(5) + i6*inca(6) + i7*inca(7)
		    km = i1*incm(1) + i3*incm(3) + i4*incm(4) + 
     &			 i5*incm(5) + i6*incm(6) + i7*incm(7)

		    do, i2 = 0, lima(2) - 1
			if ( mask(i2*incm(2) + km) ) then
			    b(i2*incb(1) + jb) = 
     &			    b(i2*incb(1) + jb) * a(i2*inca(2) + ia)
			end if
		    end do
*-----------------------------------------------------------------------
		end do
		end do
		end do
		end do
		end do
		end do

		return

	    end if
	end if
*	.. end of prod_x@ ..
