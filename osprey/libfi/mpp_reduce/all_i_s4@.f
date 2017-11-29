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



      subroutine ALL_I_LS4@(result,source,dim,rext,sext,
     +                      blkcnts,dim_bcnt)
CDIR$ ID "@(#) libfi/mpp_reduce/all_i_s4@.f	92.0	10/08/98 14:37:14"
************************************************************************
*
* Purpose:   Determine whether all value is true in source along
*            diminsion dim. This routine handles source arrays of
*            rank 4. It returns an array result. This routine is
*            called by the C routine _ALL_LS4.
*
* Arguments: result  - logical array of rank 3.
*            source  - logical array of rank 4.
*            dim     - integer scalar: dimension to search along.
*            rext    - integer array of rank 1:
*                      extents of the result array.
*            sext    - integer array of rank 1:
*                      extents of the source array.
*            blkcnts - integer array: number of blocks for each dimension.
*            dim_bcnt- integer scalar: number of blocks in the first
*                      dimension if dim != 1 or the number of blocks in
*                      the second dimension if dim = 1.
*
* Variables: npes    - integer scalar:
*                      If dim = 1, then npes = number of PEs in the
*                      first dimension, else the number of PEs in
*                      the second dimension.
*            hix     - integer array of rank 1: the high index for each
*                      distributed block in each dimension.
*            lowx    - integer array of rank 1: the low index for each
*                      distributed block in each dimension.
*            hi_rx   - integer array of rank 1: the high index for each
*                      distributed block in each dimension of the result
*                      array.
*            low_rx  - integer array of rank 1: the low index for each
*                      distributed block in each dimension of the result
*                      array.
*            lmext   - integer array of rank 1:
*                      extents of the local-all array if called within
*                      a parallel region, else it is the same as rext.
*            loc_sext- integer array of rank 1:
*                      local extents for the source array.
*            dist_cnt- the number of dimensions that are distributed over
*                      more than one processor between dimension 1 and
*                      dimension dim.
*
* Intrinsic functions called:
*            my_pe()
*            in_parallel()
*            hiidx()
*            lowidx()
*            pes()
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
      intrinsic my_pe, in_parallel, hiidx, lowidx, pes
      integer blkcnts(4), npes, mype
      integer dim, dim_bcnt(3), dist_cnt
      integer sext(4), loc_sext(4), rext(3), lmext(3)
      logical in_parallel
      integer hi1(blkcnts(1)), low1(blkcnts(1))
      integer hi2(blkcnts(2)), low2(blkcnts(2))
      integer hi3(blkcnts(3)), low3(blkcnts(3))
      integer hi4(blkcnts(4)), low4(blkcnts(4))
      integer hi_r1(dim_bcnt(1)), low_r1(dim_bcnt(1))
      integer hi_r2(dim_bcnt(2)), low_r2(dim_bcnt(2))
      integer hi_r3(dim_bcnt(3)), low_r3(dim_bcnt(3))
      logical source(sext(1),sext(2),sext(3),sext(4))
      logical result(rext(1),rext(2),rext(3))
      data loc_sext/4*0/
*
cdir$ serial_only
cdir$ unknown_shared source
*
      include "setup_4@.fh"
*
      call ALL_W_LS4@(result,source,dim,rext,loc_sext,dim_bcnt,npes,
     +                hi_r1,low_r1,hi_r2,low_r2,hi_r3,low_r3,lmext,
     +                dist_cnt)
*
      return
      end
