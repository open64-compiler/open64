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


      subroutine MINLOC_JP1@(fresult,source,mask,maskflg,extent)
*
CDIR$ ID "@(#) libfi/mpp_reduce/minloc_p_1@.f	92.0	10/08/98 14:37:14"
************************************************************************
*
* Purpose: Determine the location of the first element of source
*          having the minimum value of the elements identified by
*          mask. This routine handles integer source arrays of rank
*          1. It returns a scalar result. This routine is called by
*          the C routine _MINLOC_JP1.
*
* Arguments: fresult   - integer scalar: location of minimum value.
*            source    - integer array of rank 1.
*            mask      - logical array of rank 1.
*            maskflg   - integer scalar:
*                        -1 = set result to zero and return
*                         0 = don't use the mask array
*                         1 = use the mask array
*            extent    - integer scalar: extent of the source array.
*
* Variables: local_idx - integer scalar: location of local minimum.
*            local_min - integer scalar: local minimum value.
*	     result    - integer scalar: index of local minimum
*            mod_factor - integer scalar: modulus factor for computing
*                        the next level of PEs in the binary fan-out
*                        tree.
*            offset    - integer scalar: offset to neighbor PE in the
*                        binary fan-out tree.
*            mype      - integer scalar: logical PE number.
*            tripcnt   - integer scalar: number of loop iterations in
*                        the binary tree fan-out algorithm.
*            far_idx   - integer scalar: neighbor PE's index to its
*                        local minimum.
*            far_min   - integer scalar: neighbor PE's local minimum.
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
      integer result(0:n$pes-1), maskflg, extent, local_idx
      integer fresult(8)
      integer mod_factor, offset, mype
      integer tripcnt, far_idx
      logical mask(extent), in_parallel
      integer min_value(0:n$pes-1), source(extent), local_min, far_min
      integer HUGE_INT
*
cdir$ unknown_shared source, mask
cdir$ shared result(:block(1)), min_value(:block(1))
cdir$ shared fresult(:block)
cdir$ pe_resident source, mask
*
      local_min = HUGE_INT()
      include "minloc_1@.fh"
*
      return
      end


      subroutine MINLOC_SP1@(fresult,source,mask,maskflg,extent)
************************************************************************
*
* Purpose: Determine the location of the first element of source
*          having the minimum value of the elements identified by
*          mask. This routine handles real source arrays of rank
*          1. It returns a scalar result. This routine is called by
*          the C routine _MINLOC_SP1.
*
* Arguments: fresult   - integer scalar: location of minimum value.
*            source    - real array of rank 1.
*            mask      - logical array of rank 1.
*            maskflg   - integer scalar:
*                        -1 = set result to zero and return
*                         0 = don't use the mask array
*                         1 = use the mask array
*            extent    - integer scalar: extent of the source array.
*
* Variables: local_idx - integer scalar: location of local minimum.
*            local_min - real scalar: local minimum value.
*	     result    - integer scalar: index of local minimum
*            mod_factor - integer scalar: modulus factor for computing
*                        the next level of PEs in the binary fan-out
*                        tree.
*            offset    - integer scalar: offset to neighbor PE in the
*                        binary fan-out tree.
*            mype      - integer scalar: logical PE number.
*            tripcnt   - integer scalar: number of loop iterations in
*                        the binary tree fan-out algorithm.
*            far_idx   - integer scalar: neighbor PE's index to itis
*                        local minimum.
*            far_min   - real scalar: neighbor PE's local minimum.
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
      integer result(0:n$pes-1), maskflg, extent, local_idx
      integer fresult(8)
      integer mod_factor, offset, mype
      integer tripcnt, far_idx
      logical mask(extent), in_parallel
      real min_value(0:n$pes-1), source(extent), local_min, far_min
      real HUGE_FLT
*
cdir$ unknown_shared source, mask
cdir$ shared result(:block(1)), min_value(:block(1))
cdir$ shared fresult(:block)
cdir$ pe_resident source, mask
*
      local_min = HUGE_FLT()
      include "minloc_1@.fh"
*
      return
      end


      subroutine MINLOC_NM_JP1@(fresult,source,extent)
************************************************************************
*
* Purpose: Determine the location of the first element of source
*          having the minimum value of the elements.
*          This routine handles integer source arrays of rank
*          1. It returns a scalar result. This routine is called by
*          the C routine _MINLOC_JP1.
*
* Arguments: fresult   - integer scalar: location of minimum value.
*            source    - integer array of rank 1.
*            extent    - integer scalar: extent of the source array.
*
* Variables: local_idx - integer scalar: location of local minimum.
*            local_min - integer scalar: local minimum value.
*	     result    - integer scalar: index of local minimum
*            mod_factor - integer scalar: modulus factor for computing
*                        the next level of PEs in the binary fan-out
*                        tree.
*            offset    - integer scalar: offset to neighbor PE in the
*                        binary fan-out tree.
*            mype      - integer scalar: logical PE number.
*            tripcnt   - integer scalar: number of loop iterations in
*                        the binary tree fan-out algorithm.
*            far_idx   - integer scalar: neighbor PE's index to its
*                        local minimum.
*            far_min   - integer scalar: neighbor PE's local minimum.
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
      integer result(0:n$pes-1), extent, local_idx
      integer fresult(8)
      integer mod_factor, offset, mype
      integer tripcnt, far_idx
      logical in_parallel
      integer min_value(0:n$pes-1), source(extent), local_min, far_min
      integer HUGE_INT
*
cdir$ unknown_shared source
cdir$ shared result(:block(1)), min_value(:block(1))
cdir$ shared fresult(:block)
cdir$ pe_resident source
*
      local_min = HUGE_INT()
      include "minloc_nomask_1@.fh"
*
      return
      end


      subroutine MINLOC_NM_SP1@(fresult,source,extent)
************************************************************************
*
* Purpose: Determine the location of the first element of source
*          having the minimum value of the elements.
*          This routine handles real source arrays of rank
*          1. It returns a scalar result. This routine is called by
*          the C routine _MINLOC_SP1.
*
* Arguments: fresult   - integer scalar: location of minimum value.
*            source    - real array of rank 1.
*            extent    - integer scalar: extent of the source array.
*
* Variables: local_idx - integer scalar: location of local minimum.
*            local_min - real scalar: local minimum value.
*	     result    - integer scalar: index of local minimum
*            mod_factor - integer scalar: modulus factor for computing
*                        the next level of PEs in the binary fan-out
*                        tree.
*            offset    - integer scalar: offset to neighbor PE in the
*                        binary fan-out tree.
*            mype      - integer scalar: logical PE number.
*            tripcnt   - integer scalar: number of loop iterations in
*                        the binary tree fan-out algorithm.
*            far_idx   - integer scalar: neighbor PE's index to itis
*                        local minimum.
*            far_min   - real scalar: neighbor PE's local minimum.
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
      integer result(0:n$pes-1), extent, local_idx
      integer fresult(8)
      integer mod_factor, offset, mype
      integer tripcnt, far_idx
      logical in_parallel
      real min_value(0:n$pes-1), source(extent), local_min, far_min
      real HUGE_FLT
*
cdir$ unknown_shared source
cdir$ shared result(:block(1)), min_value(:block(1))
cdir$ shared fresult(:block)
cdir$ pe_resident source
*
      local_min = HUGE_FLT()
      include "minloc_nomask_1@.fh"
*
      return
      end

