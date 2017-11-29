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



      subroutine MAXVAL_I_JS2@(result,source,dim,mask,maskflg,rext,sext,
     +                       blkcnts,dim_bcnt)
CDIR$ ID "@(#) libfi/mpp_reduce/maxval_i_s2@.f	92.0	10/08/98 14:37:14"
************************************************************************
*
* Purpose:   Find the maximum value of the elements identified by
*            mask. This routine handles integer source arrays of rank
*            2. It returns an array result. This routine is called by
*            the C routine _MAXVAL_JS2.
*
* Arguments: result  - integer array of rank 1.
*            source  - integer array of rank 2.
*            dim     - integer scalar: dimension to search along.
*            mask    - logical array of rank 2.
*            maskflg - integer scalar:
*                      -1 = set result to zero and return
*                       0 = don't use the mask array
*                       1 = use the mask array
*            rext    - integer scalar: extent of the result array.
*            sext    - integer array of rank 1:
*                      extents of the source array.
*            blkcnts - integer array: number of blocks for each dimension.
*            dim_bcnt- integer scalar: number of blocks in the first
*                      dimension if dim = 2 or the number of blocks in
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
*            lmext   - integer scalar: extent of the local-maximum array
*                      if called within a parallel region, else it is
*                      the same as rext.
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
      integer blkcnts(2), npes, mype
      integer maskflg, dim, dim_bcnt, dist_cnt
      integer sext(2), loc_sext(2), rext, lmext
      logical in_parallel
      logical mask(sext(1),sext(2))
      integer hi1(blkcnts(1)), low1(blkcnts(1))
      integer hi2(blkcnts(2)), low2(blkcnts(2))
      integer hi(dim_bcnt), low(dim_bcnt)
      integer source(sext(1),sext(2))
      integer result(rext)
      data loc_sext/2*0/
*
cdir$ serial_only
cdir$ unknown_shared source
cdir$ unknown mask
*
      include "setup_2@.fh"
*
      call MAXVAL_W_JS2@(result,source,dim,mask,maskflg,rext,loc_sext,
     +                 dim_bcnt,npes,hi,low,lmext,dist_cnt)
*
      return
      end


      subroutine MAXVAL_I_SS2@(result,source,dim,mask,maskflg,rext,sext,
     +                       blkcnts,dim_bcnt)
************************************************************************
*
* Purpose:   Find the maximum value of the elements identified by
*            mask. This routine handles integer source arrays of rank
*            2. It returns an array result. This routine is called by
*            the C routine _MAXVAL_SS2.
*
* Arguments: result  - real array of rank 1.
*            source  - real array of rank 2.
*            dim     - integer scalar: dimension to search along.
*            mask    - logical array of rank 2.
*            maskflg - integer scalar:
*                      -1 = set result to zero and return
*                       0 = don't use the mask array
*                       1 = use the mask array
*            rext    - integer scalar: extent of the result array.
*            sext    - integer array of rank 1:
*                      extents of the source array.
*            blkcnts - integer array: number of blocks for each dimension.
*            dim_bcnt- integer scalar: number of blocks in the first
*                      dimension if dim = 2 or the number of blocks in
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
*            lmext   - integer scalar: extent of the local-maximum array
*                      if called within a parallel region, else it is
*                      the same as rext.
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
      integer blkcnts(2),npes, mype
      integer maskflg, dim, dim_bcnt, dist_cnt
      integer sext(2), loc_sext(2), rext, lmext
      logical in_parallel
      logical mask(sext(1),sext(2))
      integer hi1(blkcnts(1)), low1(blkcnts(1))
      integer hi2(blkcnts(2)), low2(blkcnts(2))
      integer hi(dim_bcnt), low(dim_bcnt)
      real source(sext(1),sext(2))
      real result(rext)
      data loc_sext/2*0/
*
cdir$ serial_only
cdir$ unknown_shared source
cdir$ unknown mask
*
      include "setup_2@.fh"
*
      call MAXVAL_W_SS2@(result,source,dim,mask,maskflg,rext,loc_sext,
     +                 dim_bcnt,npes,hi,low,lmext,dist_cnt)
*
      return
      end
