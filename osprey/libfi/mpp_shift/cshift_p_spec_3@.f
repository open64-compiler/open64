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


	subroutine cshift_spec_wd1_p3@(result, source, dim, sext, shftval)
*
CDIR$ ID "@(#) libfi/mpp_shift/cshift_p_spec_3@.f	92.0	"
CDIR$ ID "10/08/98 14:37:14"
*
**************************************************************************
*
* Purpose:	Perform a circular shift of an array in argument SOURCE
*		by SHIFT elements.  This routine handles
*		integer source arrays of rank 3.  This routine is called
*		from the C routine _CSHIFT_JP3.
*
* Arguments:	result	- integer array of rank 3.
*		source	- integer array of rank 3.
*		dim	- integer scalar: dimension to shift over
*		sext	- integer array of rank 1.
*		shftval	- integer scalar: scalar shift value
*
* Variables:	mype	- integer scalar: logical PE number.
*		blksize - integer array of rank 1: PE block sizes
*		pe_low	- integer scalar: PE receiving data on +1 shift
*		pe_hi	- integer scalar: PE receiving data on -1 shift
*		low	- integer array of rank 1: low indices
*		hi	- integer array of rank 1: hi indices
*		
*
* Intrinsic functions called:
*		my_pe()
*		blkct()
*		lowidx()
*		hiidx()
*		home()
*
* Documentation:
*		T3D Fortran 90 Array Intrinsics Interface Description
*		T3D Fortran 90 Array Intrinsics Design Description
*************************************************************************
	implicit	none
	intrinsic	my_pe, blkct, lowidx, hiidx, home
	integer		sext(3)
	integer		result(sext(1),sext(2),sext(3))
	integer		source(sext(1),sext(2),sext(3))
	integer		dim
	integer		shftval
	integer		blksize(3), mype
	integer		pe_low, pe_hi
	integer		low(3), hi(3)
*
cdir$	unknown_shared	source, result
*
	mype = my_pe()
*
*	Only proceed if there is data on the PE
*
	if (blkct (source,1,mype) .gt. 0) then
*
*	Determine hi and low indices for each dimension, as well as block size
*
	    low(1) = lowidx(source,1,mype,1)
	    low(2) = lowidx(source,2,mype,1)
	    low(3) = lowidx(source,3,mype,1)
	    hi(1) = hiidx (source,1,mype,1)
	    hi(2) = hiidx (source,2,mype,1)
	    hi(3) = hiidx (source,3,mype,1)
	    blksize(1) = hi(1) - low(1) + 1
	    blksize(2) = hi(2) - low(2) + 1
	    blksize(3) = hi(3) - low(3) + 1
*
*	Determine PE which will receive data for +1 and -1 shifts
*
	    if (dim .eq. 1) then
		if (low(1) .gt. 1) then
		    pe_low = home (source(low(1)-1,low(2),low(3)))
		else
		    pe_low = home (source(sext(1),low(2),low(3)))
		endif
		if (hi(1) .lt. sext(1)) then
		    pe_hi = home (source(hi(1)+1,low(2),low(3)))
		else
		    pe_hi = home (source(1,low(2),low(3)))
		endif
	    else if (dim .eq. 2) then
		if (low(2) .gt. 1) then
		    pe_low = home (source(low(1),low(2)-1,low(3)))
		else
		    pe_low = home (source(low(1),sext(2),low(3)))
		endif
		if (hi(2) .lt. sext(2)) then
		    pe_hi = home (source(low(1),hi(2)+1,low(3)))
		else
		    pe_hi = home (source(low(1),1,low(3)))
		endif
	    else
		if (low(3) .gt. 1) then
		    pe_low = home (source(low(1),low(2),low(3)-1))
		else
		    pe_low = home (source(low(1),low(2),sext(3)))
		endif
		if (hi(3) .lt. sext(3)) then
		    pe_hi = home (source(low(1),low(2),hi(3)+1))
		else
		    pe_hi = home (source(low(1),low(2),1))
		endif
	    endif
*
	    call cshift_i_spec_wd1_p3@ (result, source, dim, sext, shftval,
     &		blksize, pe_low, pe_hi)
	endif
*
cdir$	barrier
	call shmem_udcflush()
*
	return
	end


	subroutine cshift_i_spec_wd1_p3@ (result, source, dim, sext, shftval, 
     &		blksize, pe_low, pe_hi)
*
**************************************************************************
*
* Purpose:	Coerce a shared array to private, and call a work routine
*		which will perform a cshift on the private arrays.
*
* Arguments:	result	- integer array of rank 3.
*		source	- integer array of rank 3.
*		dim	- integer scalar: dimension to shift over
*		sext	- integer array of rank 1.
*		shftval	- integer scalar: scalar shift value
*		blksize	- integer array of rank 1: block sizes of array
*		pe_low	- integer scalar: PE receiving data on +1 shift
*		pe_hi	- integer scalar: PE receiving data on -1 shift
*
* Variables:	None
*
* Intrinsic functions called:
*		my_pe()
*
* Documentation:
*		T3D Fortran 90 Array Intrinsics Interface Description
*		T3D Fortran 90 Array Intrinsics Design Description
*************************************************************************
*
	implicit	none
	integer		sext(3)
	integer		dim
	integer		result(sext(1),sext(2),sext(3))
	integer		source(sext(1),sext(2),sext(3))
	integer		shftval
	integer		blksize(3)
	integer		pe_low, pe_hi
*
cdir$	unknown_shared	source, result
cdir$	pe_resident	source, result
*
	call cshift_spec_w_wd1_p3@ (result(1,1,1), source(1,1,1), dim, blksize,
     &		shftval, pe_low, pe_hi)
*
	return
	end


	subroutine cshift_spec_w_wd1_p3@ (result, source, dim, sext, shftval,
     &		pe_low, pe_hi)
*
**************************************************************************
*
* Purpose:	Perform a cshift on a coerced shared to private section
*		of a rank 3 array.  This routine will only be called for
*		scalar shift counts of -1, 0, or 1.  The PE number of the
*		PE which will receive the data passed out of this block
*		is found in pe_low for shifts of 1, and pe_hi for shifts
*		of -1.  Data will be transferred to the other PE's via
*		shmem_iputs.
*
*		This routine is divided into three main sections: shift
*		counts of -1, 1, and 0.  Within those sections are two
*		major divisions: Entire extent of dimension being cshifted
*		over on 1 PE, and extent being spread over 2 or more PEs.
*		Lastly, these divisions have three parts: dim 1, dim 2, and
*		dim 3.
*
* Arguments:	result	- integer array of rank 3.
*		source	- integer array of rank 3.
*		dim	- integer scalar: dimension to shift over
*		sext	- integer array of rank 1.
*		shftval	- integer scalar: scalar shift value
*		blksize	- integer array of rank 1: block sizes of array
*		pe_low	- integer scalar: PE receiving data on +1 shift
*		pe_hi	- integer scalar: PE receiving data on -1 shift
*
* Variables:	mype	- integer scalar: PE on which we are working
*
* Intrinsic functions called:
*		my_pe()
*
* Documentation:
*		T3D Fortran 90 Array Intrinsics Interface Description
*		T3D Fortran 90 Array Intrinsics Design Description
*************************************************************************
*
	implicit	none
	intrinsic	my_pe
	integer		sext(3)
	integer		dim
	integer		result(sext(1),sext(2),sext(3))
	integer		source(sext(1),sext(2),sext(3))
	integer		shftval
	integer		pe_low, pe_hi
	integer		i, j, k
	integer		totsx1, totsx2, totsx3, str1
	integer		mype
*
cdir$	pe_private	source, result
*
	mype = my_pe()
*
	totsx1 = sext(1)
	totsx2 = sext(2)
	totsx3 = sext(3)
	str1 = 1
	include "cshift_spec_3@.fh"
*
	return
	end


	subroutine cshift_spec_wd2_p3@(result, source, dim, sext, shftval)
*
**************************************************************************
*
* Purpose:	Perform a circular shift of an array in argument SOURCE
*		by SHIFT elements.  This routine handles
*		complex source arrays of rank 3.  This routine is called
*		from the C routine _CSHIFT_JP3.
*
* Arguments:	result	- complex array of rank 3.
*		source	- complex array of rank 3.
*		dim	- integer scalar: dimension to shift over
*		sext	- integer array of rank 1.
*		shftval	- integer scalar: scalar shift value
*
* Variables:	mype	- integer scalar: logical PE number.
*		blksize - integer array of rank 1: PE block sizes
*		pe_low	- integer scalar: PE receiving data on +1 shift
*		pe_hi	- integer scalar: PE receiving data on -1 shift
*		low	- integer array of rank 1: low indices
*		hi	- integer array of rank 1: hi indices
*		
*
* Intrinsic functions called:
*		my_pe()
*		blkct()
*		lowidx()
*		hiidx()
*		home()
*
* Documentation:
*		T3D Fortran 90 Array Intrinsics Interface Description
*		T3D Fortran 90 Array Intrinsics Design Description
*************************************************************************
	implicit	none
	intrinsic	my_pe, blkct, lowidx, hiidx, home
	integer		sext(3)
	complex		result(sext(1),sext(2),sext(3))
	complex		source(sext(1),sext(2),sext(3))
	integer		dim
	integer		shftval
	integer		blksize(3), mype
	integer		pe_low, pe_hi
	integer		low(3), hi(3)
*
cdir$	unknown_shared	source, result
*
	mype = my_pe()
*
*	Only proceed if there is data on the PE
*
	if (blkct (source,1,mype) .gt. 0) then
*
*	Determine hi and low indices for each dimension, as well as block size
*
	    low(1) = lowidx(source,1,mype,1)
	    low(2) = lowidx(source,2,mype,1)
	    low(3) = lowidx(source,3,mype,1)
	    hi(1) = hiidx (source,1,mype,1)
	    hi(2) = hiidx (source,2,mype,1)
	    hi(3) = hiidx (source,3,mype,1)
	    blksize(1) = hi(1) - low(1) + 1
	    blksize(2) = hi(2) - low(2) + 1
	    blksize(3) = hi(3) - low(3) + 1
*
*	Determine PE which will receive data for +1 and -1 shifts
*
	    if (dim .eq. 1) then
		if (low(1) .gt. 1) then
		    pe_low = home (source(low(1)-1,low(2),low(3)))
		else
		    pe_low = home (source(sext(1),low(2),low(3)))
		endif
		if (hi(1) .lt. sext(1)) then
		    pe_hi = home (source(hi(1)+1,low(2),low(3)))
		else
		    pe_hi = home (source(1,low(2),low(3)))
		endif
	    else if (dim .eq. 2) then
		if (low(2) .gt. 1) then
		    pe_low = home (source(low(1),low(2)-1,low(3)))
		else
		    pe_low = home (source(low(1),sext(2),low(3)))
		endif
		if (hi(2) .lt. sext(2)) then
		    pe_hi = home (source(low(1),hi(2)+1,low(3)))
		else
		    pe_hi = home (source(low(1),1,low(3)))
		endif
	    else
		if (low(3) .gt. 1) then
		    pe_low = home (source(low(1),low(2),low(3)-1))
		else
		    pe_low = home (source(low(1),low(2),sext(3)))
		endif
		if (hi(3) .lt. sext(3)) then
		    pe_hi = home (source(low(1),low(2),hi(3)+1))
		else
		    pe_hi = home (source(low(1),low(2),1))
		endif
	    endif
*
	    call cshift_i_spec_wd2_p3@ (result, source, dim, sext, shftval,
     &		blksize, pe_low, pe_hi)
	endif
*
cdir$	barrier
	call shmem_udcflush()
*
	return
	end


	subroutine cshift_i_spec_wd2_p3@ (result, source, dim, sext, shftval, 
     &		blksize, pe_low, pe_hi)
*
**************************************************************************
*
* Purpose:	Coerce a shared array to private, and call a work routine
*		which will perform a cshift on the private arrays.
*
* Arguments:	result	- complex array of rank 3.
*		source	- complex array of rank 3.
*		dim	- integer scalar: dimension to shift over
*		sext	- integer array of rank 1.
*		shftval	- integer scalar: scalar shift value
*		blksize	- integer array of rank 1: block sizes of array
*		pe_low	- integer scalar: PE receiving data on +1 shift
*		pe_hi	- integer scalar: PE receiving data on -1 shift
*
* Variables:	None
*
* Intrinsic functions called:
*		my_pe()
*
* Documentation:
*		T3D Fortran 90 Array Intrinsics Interface Description
*		T3D Fortran 90 Array Intrinsics Design Description
*************************************************************************
*
	implicit	none
	integer		sext(3)
	integer		dim
	complex		result(sext(1),sext(2),sext(3))
	complex		source(sext(1),sext(2),sext(3))
	integer		shftval
	integer		blksize(3)
	integer		pe_low, pe_hi
*
cdir$	unknown_shared	source, result
cdir$	pe_resident	source, result
*
	call cshift_spec_w_wd2_p3@ (result(1,1,1), source(1,1,1), dim, blksize,
     &		shftval, pe_low, pe_hi)
*
	return
	end


	subroutine cshift_spec_w_wd2_p3@ (result, source, dim, sext, shftval,
     &		pe_low, pe_hi)
*
**************************************************************************
*
* Purpose:	Perform a cshift on a coerced shared to private section
*		of a rank 3 array.  This routine will only be called for
*		scalar shift counts of -1, 0, or 1.  The PE number of the
*		PE which will receive the data passed out of this block
*		is found in pe_low for shifts of 1, and pe_hi for shifts
*		of -1.  Data will be transferred to the other PE's via
*		shmem_iputs.
*
*		This routine is divided into three main sections: shift
*		counts of -1, 1, and 0.  Within those sections are two
*		major divisions: Entire extent of dimension being cshifted
*		over on 1 PE, and extent being spread over 2 or more PEs.
*		Lastly, these divisions have three parts: dim 1, dim 2, and
*		dim 3.
*
* Arguments:	result	- complex array of rank 3.
*		source	- complex array of rank 3.
*		dim	- integer scalar: dimension to shift over
*		sext	- integer array of rank 1.
*		shftval	- integer scalar: scalar shift value
*		blksize	- integer array of rank 1: block sizes of array
*		pe_low	- integer scalar: PE receiving data on +1 shift
*		pe_hi	- integer scalar: PE receiving data on -1 shift
*
* Variables:	mype	- integer scalar: PE on which we are working
*
* Intrinsic functions called:
*		my_pe()
*
* Documentation:
*		T3D Fortran 90 Array Intrinsics Interface Description
*		T3D Fortran 90 Array Intrinsics Design Description
*************************************************************************
*
	implicit	none
	intrinsic	my_pe
	integer		sext(3)
	integer		dim
	complex		result(sext(1),sext(2),sext(3))
	complex		source(sext(1),sext(2),sext(3))
	integer		shftval
	integer		pe_low, pe_hi
	integer		i, j, k
	integer		totsx1, totsx2, totsx3, str1
	integer		mype
*
cdir$	pe_private	source, result
*
	mype = my_pe()
*
	totsx1 = sext(1) * 2
	totsx2 = sext(2) * 2
	totsx3 = sext(3) * 2
	str1 = 2
	include "cshift_spec_3@.fh"
*
	return
	end
