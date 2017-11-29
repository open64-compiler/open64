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



      subroutine ALL_W_LP2@(result,source,dim,rext,sext,blkcnt,
     +                      npes,hi,low,lmext,dist_cnt)
CDIR$ ID "@(#) libfi/mpp_reduce/all_w_p2@.f	92.0	10/08/98 14:37:14"
************************************************************************
*
* Purpose:   Determine whether all values are true in source along
*            dimension dim. This routine handles source arrays of
*            rank 2. It returns an array result. This routine is
*            called by the Fortran routine ALL_I_LP2@.
*
* Arguments: result  - logical array of rank 1.
*            source  - logical array of rank 2.
*            dim     - integer scalar: dimension to search along.
*            rext    - integer scalar: extents of the result array.
*            sext    - integer array of rank 1:
*                      local extents of the source array.
*            blkcnt - integer scalar:
*                      number of blocks for dimension 2 of the source
*                      array if dim = 1 else the number of blocks for
*                      dimension 1.
*            npes    - integer scalar:
*                      number of PEs in dimension dim.
*            hi      - integer array of rank 1:
*                      the high index for each distributed block in
*                      the source array for dimension 2 if dim = 1 else
*                      the number of blocks in dimension 1.
*            low     - integer array of rank 1:
*                      the low index for each distributed block in each
*                      dimension except for dimension dim.
*            lmext   - integer scalar:
*                      extent of the local_all array if called within
*                      a parallel region, else it is the same as rext.
*            dist_cnt- the number of dimensions that are distributed over
*                      more than one processor between dimension 1 and
*                      dimension dim.
*
* Variables: local_all - logical array of rank 1: array of local all
*                      values.
*            length  - integer scalar: length of block subgroups for
*                      global update of result array.
*            offset  - integer scalar: offset into result array when
*                      (blkcnt < npes) or offset into local_all when
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
      integer blkcnt, length, npes, mype, dim
      integer sext(2), rext, lmext, dist_cnt
      integer offset, j_offset, pe_offset, j_block
      integer hi(blkcnt), low(blkcnt)
      logical in_parallel
      logical source(sext(1),sext(2))
      logical result(rext), local_all(lmext)
*
cdir$ parallel_only
cdir$ shared result(:block)
*
*     Initialize the local and global result arrays
*
cdir$ doshared (i) on result(i)
      do i = 1, rext
          result(i) = .true.
      enddo
      do i = 1, lmext
          local_all(i) = .true.
      enddo
*
      include "all_w_2@.fh"
*
      return
      end
