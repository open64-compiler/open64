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



      subroutine PROD_I_JP5@(result,source,dim,mask,maskflg,rext,
     +                       sext,blkcnts,dim_bcnt)
CDIR$ ID "@(#) libfi/mpp_reduce/prod_i_p5@.f	92.0	10/08/98 14:37:14"
************************************************************************
*
* Purpose:   Compute the product of the elements identified by mask.
*            This routine handles integer source arrays of rank
*            5. It returns an array result. This routine is called
*            by the C routine _PROD_JP5.
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
*                      extents of the source array.
*            blkcnts - integer array or rank 1: number of blocks for
*                      each dimension.
*            dim_bcnt- integer array of rank 1: number of blocks for
*                      each dimensions excluding dimension dim.
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
*                      extents of the local product array if called within
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
      integer blkcnts(5), npes, mype
      integer maskflg, dim, dim_bcnt(4), dist_cnt
      integer sext(5), loc_sext(5), rext(4), lmext(4)
      logical in_parallel
      logical mask(sext(1),sext(2),sext(3),sext(4),sext(5))
      integer hi1(blkcnts(1)), low1(blkcnts(1))
      integer hi2(blkcnts(2)), low2(blkcnts(2))
      integer hi3(blkcnts(3)), low3(blkcnts(3))
      integer hi4(blkcnts(4)), low4(blkcnts(4))
      integer hi5(blkcnts(5)), low5(blkcnts(5))
      integer hi_r1(dim_bcnt(1)), low_r1(dim_bcnt(1))
      integer hi_r2(dim_bcnt(2)), low_r2(dim_bcnt(2))
      integer hi_r3(dim_bcnt(3)), low_r3(dim_bcnt(3))
      integer hi_r4(dim_bcnt(4)), low_r4(dim_bcnt(4))
      integer source(sext(1),sext(2),sext(3),sext(4),sext(5))
      integer result(rext(1),rext(2),rext(3),rext(4))
      data loc_sext/5*0/
*
cdir$ parallel_only
cdir$ shared result(:block,:block,:block,:block)
cdir$ unknown_shared source
cdir$ unknown mask
*
      include "setup_5@.fh"
*
      call PROD_W_JP5@(result,source,dim,mask,maskflg,rext,loc_sext,
     +                 dim_bcnt,npes,hi_r1,low_r1,hi_r2,low_r2,hi_r3,
     +                 low_r3,hi_r4,low_r4,lmext,dist_cnt)
*
      return
      end


      subroutine PROD_I_SP5@(result,source,dim,mask,maskflg,rext,
     +                       sext,blkcnts,dim_bcnt)
************************************************************************
*
* Purpose:   Compute the product of the elements identified by mask.
*            This routine handles integer source arrays of rank
*            5. It returns an array result. This routine is called
*            by the C routine _PROD_SP5.
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
*                      extents of the source array.
*            blkcnts - integer array of rank 1: number of blocks for
*                      each dimension.
*            dim_bcnt- integer array of rank 1: number of blocks for
*                      each dimension excluding dimension dim.
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
*                      extents of the local product array if called within
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
      integer blkcnts(5),npes, mype
      integer maskflg, dim, dim_bcnt(4), dist_cnt
      integer sext(5), loc_sext(5), rext(4), lmext(4)
      logical in_parallel
      logical mask(sext(1),sext(2),sext(3),sext(4),sext(5))
      integer hi1(blkcnts(1)), low1(blkcnts(1))
      integer hi2(blkcnts(2)), low2(blkcnts(2))
      integer hi3(blkcnts(3)), low3(blkcnts(3))
      integer hi4(blkcnts(4)), low4(blkcnts(4))
      integer hi5(blkcnts(5)), low5(blkcnts(5))
      integer hi_r1(dim_bcnt(1)), low_r1(dim_bcnt(1))
      integer hi_r2(dim_bcnt(2)), low_r2(dim_bcnt(2))
      integer hi_r3(dim_bcnt(3)), low_r3(dim_bcnt(3))
      integer hi_r4(dim_bcnt(4)), low_r4(dim_bcnt(4))
      real source(sext(1),sext(2),sext(3),sext(4),sext(5))
      real result(rext(1),rext(2),rext(3),rext(4))
      data loc_sext/5*0/
*
cdir$ parallel_only
cdir$ shared result(:block,:block,:block,:block)
cdir$ unknown_shared source
cdir$ unknown mask
*
      include "setup_5@.fh"
*
      call PROD_W_SP5@(result,source,dim,mask,maskflg,rext,loc_sext,
     +                 dim_bcnt,npes,hi_r1,low_r1,hi_r2,low_r2,hi_r3,
     +                 low_r3,hi_r4,low_r4,lmext,dist_cnt)
*
      return
      end
