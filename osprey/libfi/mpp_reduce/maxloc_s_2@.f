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


      subroutine MAXLOC_JS2@(fresult,source,mask,maskflg,extent)
*
CDIR$ ID "@(#) libfi/mpp_reduce/maxloc_s_2@.f	92.0	10/08/98 14:37:14"
************************************************************************
*
* Purpose: Determine the location of the first element of source
*          having the maximum value of the elements identified by
*          mask. This routine handles integer source arrays of rank
*          2. It returns an array result. This routine is called by
*          the C routine _MAXLOC_JS2.
*
*          This version is called when executing in a master region
*          but called with a source array that is distributed.
*
* Arguments: fresult   - integer array: location of maximum value.
*            source    - integer array of rank 2.
*            mask      - logical array of rank 2.
*            maskflg   - integer scalar:
*                        -1 = set result to zero and return
*                         0 = don't use the mask array
*                         1 = use the mask array
*            extent    - integer array: extents of the source array.
*
* Variables: local_idx - integer array: location of local maximum.
*            local_max - integer scalar: local maximum value.
*	     result    - integer array: location of maximum value
*            mod_factor - integer scalar: modulus factor for computing
*                        the next level of PEs in the binary fan-out
*                        tree.
*            offset    - integer scalar: offset to neighbor PE in the
*                        binary fan-out tree.
*            mype      - integer scalar: logical PE number.
*            tripcnt   - integer scalar: number of loop iterations in
*                        the binary tree fan-out algorithm.
*            far_idx   - integer array: neighbor PE's index to its
*                        local maximum.
*            far_max   - integer scalar: neighbor PE's local maximum.
*
* Intrinsic functions called:
*            my_pe()
*            in_parallel()
*
* Documentation:
*            T3D Fortran 90 Array Intrinsics Interface Description
*            T3D Fortran 90 Array Intrinsics Design Description
*
* Author:    Ray Barriuso
*            Multiprocessing Software Section
*            Cray Research, Inc.
*
************************************************************************
      intrinsic my_pe, in_parallel
      integer result(0:0,2), maskflg, extent(2), local_idx(2)
      integer fresult(8)
      integer mod_factor, offset, mype
      integer tripcnt, far_idx(2)
      logical mask(extent(1),extent(2)), in_parallel
      integer max_value(0:0), source(extent(1),extent(2))
      integer HUGE_INT
      integer local_max, far_max
*
cdir$ unknown_shared source, mask
*
      local_max = -HUGE_INT()
      include "maxloc_2@.fh"
*
      return
      end


      subroutine MAXLOC_SS2@(fresult,source,mask,maskflg,extent)
************************************************************************
*
* Purpose: Determine the location of the first element of source
*          having the maximum value of the elements identified by
*          mask. This routine handles real source arrays of rank
*          2. It returns an array result. This routine is called by
*          the C routine _MAXLOC_SS2.
*
*          This version is called when executing in a master region
*          but called with a source array that is distributed.
*
* Arguments: fresult   - integer array: location of maximum value.
*            source    - real array of rank 2.
*            mask      - logical array of rank 2.
*            maskflg   - integer scalar:
*                        -1 = set result to zero and return
*                         0 = don't use the mask array
*                         1 = use the mask array
*            extent    - integer array: extents of the source array.
*
* Variables: local_idx - integer array: location of local maximum.
*            local_max - real scalar: local maximum value.
*	     result    - integer array: location of maximum value
*            mod_factor - integer scalar: modulus factor for computing
*                        the next level of PEs in the binary fan-out
*                        tree.
*            offset    - integer scalar: offset to neighbor PE in the
*                        binary fan-out tree.
*            mype      - integer scalar: logical PE number.
*            tripcnt   - integer scalar: number of loop iterations in
*                        the binary tree fan-out algorithm.
*            far_idx   - integer array: neighbor PE's index to its
*                        local maximum.
*            far_max   - real scalar: neighbor PE's local maximum.
*
* Intrinsic functions called:
*            my_pe()
*            in_parallel()
*
* Documentation:
*            T3D Fortran 90 Array Intrinsics Interface Description
*            T3D Fortran 90 Array Intrinsics Design Description
*
* Author:    Ray Barriuso
*            Multiprocessing Software Section
*            Cray Research, Inc.
*
************************************************************************
      intrinsic my_pe, in_parallel
      integer result(0:0,2), maskflg, extent(2), local_idx(2)
      integer fresult(8)
      integer mod_factor, offset, mype
      integer tripcnt, far_idx(2)
      logical mask(extent(1),extent(2)), in_parallel
      real max_value(0:0), source(extent(1),extent(2))
      real local_max, far_max
      real HUGE_FLT
*
cdir$ unknown_shared source, mask
*
      local_max = -HUGE_FLT()
      include "maxloc_2@.fh"
*
      return
      end


      subroutine MAXLOC_NM_JS2@(fresult,source,extent)
************************************************************************
*
* Purpose: Determine the location of the first element of source
*          having the maximum value of the elements in the array.
*          This routine handles integer source arrays of rank
*          2. It returns an array result. This routine is called by
*          the C routine _MAXLOC_JS2.
*
*          This version is called when executing in a master region
*          but called with a source array that is distributed.
*
* Arguments: fresult   - integer array: location of maximum value.
*            source    - integer array of rank 2.
*            extent    - integer array: extents of the source array.
*
* Variables: local_idx - integer array: location of local maximum.
*            local_max - integer scalar: local maximum value.
*	     result    - integer array: location of maximum value
*            mod_factor - integer scalar: modulus factor for computing
*                        the next level of PEs in the binary fan-out
*                        tree.
*            offset    - integer scalar: offset to neighbor PE in the
*                        binary fan-out tree.
*            mype      - integer scalar: logical PE number.
*            tripcnt   - integer scalar: number of loop iterations in
*                        the binary tree fan-out algorithm.
*            far_idx   - integer array: neighbor PE's index to its
*                        local maximum.
*            far_max   - integer scalar: neighbor PE's local maximum.
*
* Intrinsic functions called:
*            my_pe()
*            in_parallel()
*
* Documentation:
*            T3D Fortran 90 Array Intrinsics Interface Description
*            T3D Fortran 90 Array Intrinsics Design Description
*
* Author:    Ray Barriuso
*            Multiprocessing Software Section
*            Cray Research, Inc.
*
************************************************************************
      intrinsic my_pe, in_parallel
      integer result(0:0,2), extent(2), local_idx(2)
      integer fresult(8)
      integer mod_factor, offset, mype
      integer tripcnt, far_idx(2)
      logical in_parallel
      integer max_value(0:0), source(extent(1),extent(2))
      integer HUGE_INT
      integer local_max, far_max
*
cdir$ unknown_shared source
*
      local_max = -HUGE_INT()
      include "maxloc_nomask_2@.fh"
*
      return
      end


      subroutine MAXLOC_NM_SS2@(fresult,source,extent)
************************************************************************
*
* Purpose: Determine the location of the first element of source
*          having the maximum value of the elements in the array.
*          This routine handles real source arrays of rank
*          2. It returns an array result. This routine is called by
*          the C routine _MAXLOC_SS2.
*
*          This version is called when executing in a master region
*          but called with a source array that is distributed.
*
* Arguments: fresult   - integer array: location of maximum value.
*            source    - real array of rank 2.
*            extent    - integer array: extents of the source array.
*
* Variables: local_idx - integer array: location of local maximum.
*            local_max - real scalar: local maximum value.
*	     result    - integer array: location of maximum value
*            mod_factor - integer scalar: modulus factor for computing
*                        the next level of PEs in the binary fan-out
*                        tree.
*            offset    - integer scalar: offset to neighbor PE in the
*                        binary fan-out tree.
*            mype      - integer scalar: logical PE number.
*            tripcnt   - integer scalar: number of loop iterations in
*                        the binary tree fan-out algorithm.
*            far_idx   - integer array: neighbor PE's index to its
*                        local maximum.
*            far_max   - real scalar: neighbor PE's local maximum.
*
* Intrinsic functions called:
*            my_pe()
*            in_parallel()
*
* Documentation:
*            T3D Fortran 90 Array Intrinsics Interface Description
*            T3D Fortran 90 Array Intrinsics Design Description
*
* Author:    Ray Barriuso
*            Multiprocessing Software Section
*            Cray Research, Inc.
*
************************************************************************
      intrinsic my_pe, in_parallel
      integer result(0:0,2), extent(2), local_idx(2)
      integer fresult(8)
      integer mod_factor, offset, mype
      integer tripcnt, far_idx(2)
      logical in_parallel
      real max_value(0:0), source(extent(1),extent(2))
      real local_max, far_max
      real HUGE_FLT
*
cdir$ unknown_shared source
*
      local_max = -HUGE_FLT()
      include "maxloc_nomask_2@.fh"
*
      return
      end

