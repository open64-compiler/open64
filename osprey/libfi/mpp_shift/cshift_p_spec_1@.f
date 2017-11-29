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


	subroutine cshift_spec_wd1_p1@(result, source, sext, shftval)
*
CDIR$ ID "@(#) libfi/mpp_shift/cshift_p_spec_1@.f	92.0	"
CDIR$ ID "10/08/98 14:37:14"
*
**************************************************************************
*
* Purpose:	Perform a circular shift of an array in argument SOURCE
*		by SHIFT elements.  This routine handles
*		integer source arrays of rank 1.  This routine is called
*		from the C routine _CSHIFT_JP1.
*
* Arguments:	result	- integer array of rank 1.
*		source	- integer array of rank 1.
*		sext	- integer array of rank 1.
*		shftval	- integer scalar: scalar shift value
*
* Variables:	mype	- integer scalar: logical PE number.
*		blksize	- integer scalar: block size on PE
*		pe_low	- integer scalar: PE receiving data on +1 shift
*		pe_hi	- integer scalar: PE receiving data on -1 shift
*		low	- integer scalar: low index on PE
*		hi	- integer scalar: high index on PE
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
	integer		sext(1)
	integer		result(sext(1))
	integer		source(sext(1))
	integer		shftval
	integer		blksize, mype
	integer		pe_low, pe_hi
	integer		low, hi
*
cdir$	unknown_shared	source, result
*
	mype = my_pe()
	if (blkct (source, 1, mype) .gt. 0) then
	    low = lowidx(source,1,mype,1)
	    hi = hiidx (source,1,mype,1)
	    blksize = hi - low + 1
	    if (low .gt. 1) then
		pe_low = home (source(low-1))
	    else
		pe_low = home (source(sext(1)))
	    endif
	    if (hi .lt. sext(1)) then
		pe_hi = home (source(hi+1))
	    else
		pe_hi = home (source(1))
	    endif
*
	    call cshift_i_spec_wd1_p1@ (result, source, sext, shftval, blksize,
     &		pe_low, pe_hi)
	endif
*
cdir$	barrier
	call shmem_udcflush()
*
	return
	end


	subroutine cshift_i_spec_wd1_p1@ (result, source, sext, shftval,
     &		blksize, pe_low, pe_hi)
*
**************************************************************************
*
* Purpose:	Coerce a shared array to private, and call a work routine
*		which will perform a cshift on the private arrays.
*
* Arguments:	result	- integer array of rank 1.
*		source	- integer array of rank 1.
*		sext	- integer scalar: extent
*		shftval	- integer scalar: scalar shift value
*		blksize	- integer scalar: block sizes of array
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
	integer		sext(1)
	integer		result(sext(1))
	integer		source(sext(1))
	integer		shftval
	integer		blksize
	integer		pe_low, pe_hi
*
cdir$	unknown_shared	source, result
cdir$	pe_resident	source, result
*
	call cshift_spec_w_wd1_p1@ (result(1), source(1), blksize, shftval,
     &		pe_low, pe_hi)
*
	return
	end


	subroutine cshift_spec_w_wd1_p1@ (result, source, sext, shftval,
     &		pe_low, pe_hi)
*
**************************************************************************
*
* Purpose:	Perform a cshift on a coerced shared to private section
*		of a rank 1 array.  This routine will only be called for
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
*
* Arguments:	result	- integer array of rank 1.
*		source	- integer array of rank 1.
*		sext	- integer scalar: extent
*		shftval	- integer scalar: scalar shift value
*		blksize	- integer scalar: block size of array
*		pe_low	- integer scalar: PE receiving data on +1 shift
*		pe_hi	- integer scalar: PE receiving data on -1 shift
*
* Variables:	mype	- integer scalar: PE on which we are working
*		str1	- integer scalar: number of words in 1 element
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
	integer		sext
	integer		result(sext)
	integer		source(sext)
	integer		shftval
	integer		pe_low, pe_hi
	integer		i, j
	integer		str1
	integer		mype
*
cdir$	pe_private	source, result
*
	mype = my_pe()
*
	str1 = 1
	include "cshift_spec_1@.fh"
*
	return
	end


	subroutine cshift_spec_wd2_p1@(result, source, sext, shftval)
*
**************************************************************************
*
* Purpose:	Perform a circular shift of an array in argument SOURCE
*		by SHIFT elements.  This routine handles
*		complex source arrays of rank 1.  This routine is called
*		from the C routine _CSHIFT_JP1.
*
* Arguments:	result	- complex array of rank 1.
*		source	- complex array of rank 1.
*		sext	- integer array of rank 1.
*		shftval	- integer scalar: scalar shift value
*
* Variables:	mype	- integer scalar: logical PE number.
*		blksize	- integer scalar: block size on PE
*		pe_low	- integer scalar: PE receiving data on +1 shift
*		pe_hi	- integer scalar: PE receiving data on -1 shift
*		low	- integer scalar: low index on PE
*		hi	- integer scalar: high index on PE
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
	integer		sext(1)
	complex		result(sext(1))
	complex		source(sext(1))
	integer		shftval
	integer		blksize, mype
	integer		pe_low, pe_hi
	integer		low, hi
*
cdir$	unknown_shared	source, result
*
	mype = my_pe()
	if (blkct (source, 1, mype) .gt. 0) then
	    low = lowidx(source,1,mype,1)
	    hi = hiidx (source,1,mype,1)
	    blksize = hi - low + 1
	    if (low .gt. 1) then
		pe_low = home (source(low-1))
	    else
		pe_low = home (source(sext(1)))
	    endif
	    if (hi .lt. sext(1)) then
		pe_hi = home (source(hi+1))
	    else
		pe_hi = home (source(1))
	    endif
*
	    call cshift_i_spec_wd2_p1@ (result, source, sext, shftval, blksize,
     &		pe_low, pe_hi)
	endif
*
cdir$	barrier
	call shmem_udcflush()
*
	return
	end


	subroutine cshift_i_spec_wd2_p1@ (result, source, sext, shftval,
     &		blksize, pe_low, pe_hi)
*
**************************************************************************
*
* Purpose:	Coerce a shared array to private, and call a work routine
*		which will perform a cshift on the private arrays.
*
* Arguments:	result	- complex array of rank 1.
*		source	- complex array of rank 1.
*		sext	- integer scalar: extent
*		shftval	- integer scalar: scalar shift value
*		blksize	- integer scalar: block sizes of array
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
	integer		sext(1)
	complex		result(sext(1))
	complex		source(sext(1))
	integer		shftval
	integer		blksize
	integer		pe_low, pe_hi
*
cdir$	unknown_shared	source, result
cdir$	pe_resident	source, result
*
	call cshift_spec_w_wd2_p1@ (result(1), source(1), blksize, shftval,
     &		pe_low, pe_hi)
*
	return
	end


	subroutine cshift_spec_w_wd2_p1@ (result, source, sext, shftval,
     &		pe_low, pe_hi)
*
**************************************************************************
*
* Purpose:	Perform a cshift on a coerced shared to private section
*		of a rank 1 array.  This routine will only be called for
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
*
* Arguments:	result	- complex array of rank 1.
*		source	- complex array of rank 1.
*		sext	- integer scalar: extent
*		shftval	- integer scalar: scalar shift value
*		blksize	- integer scalar: block size of array
*		pe_low	- integer scalar: PE receiving data on +1 shift
*		pe_hi	- integer scalar: PE receiving data on -1 shift
*
* Variables:	mype	- integer scalar: PE on which we are working
*		str1	- integer scalar: number of words in 1 element
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
	integer		sext
	complex		result(sext)
	complex		source(sext)
	integer		shftval
	integer		pe_low, pe_hi
	integer		i, j
	integer		str1
	integer		mype
*
cdir$	pe_private	source, result
*
	mype = my_pe()
*
	str1 = 2
	include "cshift_spec_1@.fh"
*
	return
	end
