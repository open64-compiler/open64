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



      subroutine SUM_W_JS3@(result,source,dim,mask,maskflg,rext,
     +                   sext,blkcnts,npes,hi1,low1,hi2,low2,lmext,
     +                   dist_cnt)
CDIR$ ID "@(#) libfi/mpp_reduce/sum_w_s3@.f	92.0	10/08/98 14:37:14"
************************************************************************
*
* Purpose:   Compute the sum of the elements identified by mask.
*            This routine handles integer source arrays of rank
*            3. It returns an array result. This routine is called
*            by the Fortran routine SUM_I_JS3@.
*
* Arguments: result  - integer array of rank 2.
*            source  - integer array of rank 3.
*            dim     - integer scalar: dimension to compute along.
*            mask    - logical array of rank 3.
*            maskflg - integer scalar:
*                      -1 = set result to zero and return
*                       0 = don't use the mask array
*                       1 = use the mask array
*            rext    - integer array of rank 1:
*                      extents of the result array.
*            sext    - integer array of rank 1:
*                      local extents of the source array.
*            blkcnts - integer array of rank 1:
*                      number of blocks for each dimension of the source
*                      array except for dimension dim.
*            npes    - integer scalar:
*                      number of PEs in dimension dim.
*            hix     - integer array of rank 1:
*                      the high index for each distributed block in each
*                      dimension except for dimension dim.
*            lowx    - integer array of rank 1:
*                      the low index for each distributed block in each
*                      dimension except for dimension dim.
*            lmext   - integer array of rank 1:
*                      extents of the local sum array if called within
*                      a parallel region, else it is the same as rext.
*            dist_cnt- the number of dimensions that are distributed over
*                      more than one processor between dimension 1 and
*                      dimension dim.
*
* Variables: local_sum - integer array of rank 2: array of local sums.
*            length  - integer scalar: length of block subgroups for
*                      global update of result array.
*            offset  - integer scalar: offset into result array when
*                      (blkcnt < npes) or offset into local_sum when
*                      (blkcnt > npes).
*            j_offset - integer scalar: offset within a block when
*                      (blkcnt < npes).
*            pe_offset - integer scalar: offset for each processor used
*                      when computing offset.
*            j_block - integer scalar: used to compute next block during
*                      the global update.
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
*            System Libraries Section
*            Cray Research, Inc.
*
************************************************************************
      intrinsic my_pe, in_parallel
      integer blkcnts(2), length, npes, mype, maskflg, dim
      integer sext(3), rext(2), lmext(2), dist_cnt
      integer offset, j_offset, pe_offset, j_block
      logical in_parallel
      logical mask(sext(1),sext(2),sext(3))
      integer source(sext(1),sext(2),sext(3))
      integer hi1(blkcnts(1)), low1(blkcnts(1))
      integer hi2(blkcnts(2)), low2(blkcnts(2))
      integer result(rext(1),rext(2)), local_sum(lmext(1),lmext(2))
      pointer (res_ptr,local_sum)
*
cdir$ serial_only
cdir$ unknown_shared source, mask
*
*     Since execution is taking place in a serial region of code,
*     equivalence the local_sum array to the result array.
*
      res_ptr = loc(result)
*
*     Initialize the result array
*
      do j = 1, lmext(2)
          do i = 1, lmext(1)
              local_sum(i,j) = 0
          enddo
      enddo
*
      include "sum_w_3@.fh"
*
      return
      end


      subroutine SUM_W_SS3@(result,source,dim,mask,maskflg,rext,
     +                   sext,blkcnts,npes,hi1,low1,hi2,low2,lmext,
     +                   dist_cnt)
************************************************************************
*
* Purpose:   Compute the sum of the elements identified by mask.
*            This routine handles integer source arrays of rank
*            3. It returns an array result. This routine is called
*            by the Fortran routine SUM_I_SS3@.
*
* Arguments: result  - real array of rank 2.
*            source  - real array of rank 3.
*            dim     - integer scalar: dimension to compute along.
*            mask    - logical array of rank 3.
*            maskflg - integer scalar:
*                      -1 = set result to zero and return
*                       0 = don't use the mask array
*                       1 = use the mask array
*            rext    - integer array of rank 1:
*                      extents of the result array.
*            sext    - integer array of rank 1:
*                      local extents of the source array.
*            blkcnts - integer array of rank 1:
*                      number of blocks for each dimension of the source
*                      array except for dimension dim.
*            npes    - integer scalar:
*                      number of PEs in dimension dim.
*            hix     - integer array of rank 1:
*                      the high index for each distributed block in each
*                      dimension except for dimension dim.
*            lowx    - integer array of rank 1:
*                      the low index for each distributed block in each
*                      dimension except for dimension dim.
*            lmext   - integer array of rank 1:
*                      extents of the local sum array if called within
*                      a parallel region, else it is the same as rext.
*            dist_cnt- the number of dimensions that are distributed over
*                      more than one processor between dimension 1 and
*                      dimension dim.
*
* Variables: local_sum - integer array of rank 2: array of local sums.
*            length  - integer scalar: length of block subgroups for
*                      global update of result array.
*            offset  - integer scalar: offset into result array when
*                      (blkcnt < npes) or offset into local_sum when
*                      (blkcnt > npes).
*            j_offset - integer scalar: offset within a block when
*                      (blkcnt < npes).
*            pe_offset - integer scalar: offset for each processor used
*                      when computing offset.
*            j_block - integer scalar: used to compute next block during
*                      the global update.
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
*            System Libraries Section
*            Cray Research, Inc.
*
************************************************************************
      intrinsic my_pe, in_parallel
      integer blkcnts(2), length, npes, mype, maskflg, dim
      integer sext(3), rext(2), lmext(2), dist_cnt
      integer offset, j_offset, pe_offset, j_block
      logical in_parallel
      logical mask(sext(1),sext(2),sext(3))
      integer hi1(blkcnts(1)), low1(blkcnts(1))
      integer hi2(blkcnts(2)), low2(blkcnts(2))
      real source(sext(1),sext(2),sext(3))
      real result(rext(1),rext(2)), local_sum(lmext(1),lmext(2))
      pointer (res_ptr,local_sum)
*
cdir$ serial_only
cdir$ unknown_shared source, mask
*
*     Since execution is taking place in a serial region of code,
*     equivalence the local_sum array to the result array.
*
      res_ptr = loc(result)
*
*     Initialize the result array
*
      do j = 1, lmext(2)
          do i = 1, lmext(1)
              local_sum(i,j) = 0.0
          enddo
      enddo
*
      include "sum_w_3@.fh"
*
      return
      end
