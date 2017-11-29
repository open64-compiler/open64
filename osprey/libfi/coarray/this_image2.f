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


      integer function _THIS_IMAGE2(list) result(q)
!DIR$ ID "@(#) libfi/coarray/this_image2.f	92.0	10/08/98 14:37:14"
!
!
      implicit none
      integer                                 :: list(:)
      integer                                 :: dim,i,j,me
      integer,pointer                         :: q(:)
      integer,dimension(0:size(list)/2)       :: k

      allocate(q(size(list)/2))

      me = this_image() - 1

      dim = size(list)/2
      k(0) = 1
      do j=1,dim-1
         k(j) = k(j-1)*(list(2*j)-list(2*j-1)+1)
      enddo

      do j=dim,1,-1
         i = me/k(j-1)
         me = me - i*k(j-1)
         q(j) = i + list(2*j-1)
      enddo
      end
