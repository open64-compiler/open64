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



      subroutine PROD_W_JS5@(result,source,dim,mask,maskflg,rext,
     +                   sext,blkcnts,npes,hi1,low1,hi2,low2,hi3,
     +                   low3,hi4,low4,lmext,dist_cnt)
CDIR$ ID "@(#) libfi/mpp_reduce/prod_w_s5@.f	92.0	10/08/98 14:37:14"
************************************************************************
*
* Purpose:   Compute the product of the elements identified by mask.
*            This routine handles integer source arrays of rank
*            5. It returns an array result. This routine is called
*            by the Fortran routine PROD_I_JS5@.
*
* Arguments: result  - integer array of rank 4.
*            source  - integer array of rank 5.
*            dim     - integer scalar: dimension to compute along.
*            mask    - logical array of rank 5.
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
*                      extents of the local product array if called within
*                      a parallel region, else it is the same as rext.
*            dist_cnt- the number of dimensions that are distributed over
*                      more than one processor between dimension 1 and
*                      dimension dim.
*
* Variables: local_prod - integer array of rank 4: array of local products.
*            length  - integer scalar: length of block subgroups for
*                      global update of result array.
*            offset  - integer scalar: offset into result array when
*                      (blkcnt < npes) or offset into local_prod when
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
      integer blkcnts(4), length, npes, mype, maskflg, dim
      integer sext(5), rext(4), lmext(4), dist_cnt
      integer offset, j_offset, pe_offset, j_block
      logical in_parallel
      logical mask(sext(1),sext(2),sext(3),sext(4),sext(5))
      integer source(sext(1),sext(2),sext(3),sext(4),sext(5))
      integer hi1(blkcnts(1)), low1(blkcnts(1))
      integer hi2(blkcnts(2)), low2(blkcnts(2))
      integer hi3(blkcnts(3)), low3(blkcnts(3))
      integer hi4(blkcnts(4)), low4(blkcnts(4))
      integer result(rext(1),rext(2),rext(3),rext(4))
      integer local_prod(lmext(1),lmext(2),lmext(3),lmext(4))
      pointer (res_ptr,local_prod)
*
cdir$ serial_only
cdir$ unknown_shared source, mask
*
*     Since execution is taking place in a serial region of code,
*     equivalence the local_prod array to the result array.
*
      res_ptr = loc(result)
*
*     Initialize the result array
*
      do i4 = 1, lmext(4)
      do i3 = 1, lmext(3)
      do i2 = 1, lmext(2)
      do i1 = 1, lmext(1)
          local_prod(i1,i2,i3,i4) = 1
      enddo
      enddo
      enddo
      enddo
*
      include "prod_w_5@.fh"
*
      return
      end


      subroutine PROD_W_SS5@(result,source,dim,mask,maskflg,rext,
     +                   sext,blkcnts,npes,hi1,low1,hi2,low2,hi3,
     +                   low3,hi4,low4,lmext,dist_cnt)
************************************************************************
*
* Purpose:   Compute the product of the elements identified by mask.
*            This routine handles integer source arrays of rank
*            5. It returns an array result. This routine is called
*            by the Fortran routine PROD_I_SS5@.
*
* Arguments: result  - real array of rank 4.
*            source  - real array of rank 5.
*            dim     - integer scalar: dimension to compute along.
*            mask    - logical array of rank 5.
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
*                      extents of the local product array if called within
*                      a parallel region, else it is the same as rext.
*            dist_cnt- the number of dimensions that are distributed over
*                      more than one processor between dimension 1 and
*                      dimension dim.
*
* Variables: local_prod - integer array of rank 4: array of local products.
*            length  - integer scalar: length of block subgroups for
*                      global update of result array.
*            offset  - integer scalar: offset into result array when
*                      (blkcnt < npes) or offset into local_prod when
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
      integer blkcnts(4), length, npes, mype, maskflg, dim
      integer sext(5), rext(4), lmext(4), dist_cnt
      integer offset, j_offset, pe_offset, j_block
      logical in_parallel
      real source(sext(1),sext(2),sext(3),sext(4),sext(5))
      logical mask(sext(1),sext(2),sext(3),sext(4),sext(5))
      integer hi1(blkcnts(1)), low1(blkcnts(1))
      integer hi2(blkcnts(2)), low2(blkcnts(2))
      integer hi3(blkcnts(3)), low3(blkcnts(3))
      integer hi4(blkcnts(4)), low4(blkcnts(4))
      real result(rext(1),rext(2),rext(3),rext(4))
      real local_prod(lmext(1),lmext(2),lmext(3),lmext(4))
      pointer (res_ptr,local_prod)
*
cdir$ serial_only
cdir$ unknown_shared source, mask
*
*     Since execution is taking place in a serial region of code,
*     equivalence the local_prod array to the result array.
*
      res_ptr = loc(result)
*
*     Initialize the result array
*
      do i4 = 1, lmext(4)
      do i3 = 1, lmext(3)
      do i2 = 1, lmext(2)
      do i1 = 1, lmext(1)
          local_prod(i1,i2,i3,i4) = 1.0
      enddo
      enddo
      enddo
      enddo
*
      include "prod_w_5@.fh"
*
      return
      end
