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


      subroutine clusfle( n, x, incx, target, index, nclus )
*     ...Scalar arguments...
        integer n, incx, nclus
        real    target

*     ...Vector arguments...
        integer index( 2,* )
        real    x( * )
*
*     ...Local variables...
        integer i, ib, ie, nb, ne
*
*     -------------
*     USM What Line
*     -------------
CDIR$ ID "@(#) libu/sci/c2/clusfle.f	92.0	10/08/98 14:57:41"
*
*     ...Begin execution...
        if ( n .lt. 1 ) return
*
        if( incx.ge.0 ) then
          ib = 1
          ie = 1
        else
          ib = 1 - incx*( n-1 )
          ie = 1 - incx*( n-1 )
        endif
*
*       Find beginnings of the clusters
        if( x( ib ).le.target ) then
          nb = 1
          index( 1,nb ) = 1
        else
          nb = 0
        endif
        ib = ib + incx
*
        do i = 2, n
          if( x( ib-incx ).gt.target .and. x( ib ).le.target ) then
            nb = nb + 1
            index( 1,nb ) = i
          endif
          ib = ib + incx
        end do
*
*       Find endings of the clusters
        if( nb.gt.0 ) then
          ne = 0
          do i = 1, n-1
            if( x( ie ).le.target .and. x( ie+incx ).gt.target ) then
              ne = ne + 1
              index( 2,ne ) = i
            endif
            ie = ie + incx
          end do

          if( x( ie ).le.target ) then
            ne = ne + 1
            index( 2,ne ) = n
          endif
        endif
        nclus = nb
      return
      end
