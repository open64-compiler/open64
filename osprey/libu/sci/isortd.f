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


      subroutine isortd( ad, n, l, h, x, incx, id, incd, c )
      implicit none

*     ... Scalar arguments ...
        character*1 ad
        integer     l, h, incd, incx, n

*     ... Vector arguments ...
        integer     x( * ), id( * ), c( l:h )

*     ... Local variables ...
        integer     i, info, j, w
        logical     des, asc

*     ... Begin execution ...
*
CDIR$ ID "@(#) libu/sci/isortd.f	92.0	10/08/98 14:57:41"
*
*       Test input parameters
        des = ad .eq. 'D' .or. ad .eq. 'd'
        asc = ad .eq. 'A' .or. ad .eq. 'a'

        info = 0
        if( .not.des .and. .not.asc ) then
          info = 1
        elseif( n .le. 0 ) then
          info = 2
        elseif( l .gt. h ) then
          info = 3
        elseif( incx .le. 0 ) then
          info = 6
        elseif( incd .le. 0 ) then
          info = 8
        endif
        if( info .ne. 0 ) then
          call xersor( 'ISORTD', info )
          goto 100
        endif

        if( n .eq. 1 ) then
          id( 1 ) = 1
          return
        endif


*       Zero out the counting array.
        do 10 i = l, h
          c( i ) = 0
10      continue

*       Start accumulating the numbers of x-values encountered
        do 20 i = 1, 1+incx*(n-1), incx
          c( x( i ) ) = c( x( i ) ) + 1
20      continue

*       This sets up the counting array such that each value
*       encountered is the relative place in the array.
        if( asc ) then 
          do 30 i = (l+1), h
            c( i ) = c( i ) + c( i-1 )
30        continue
        elseif( des ) then
          do 40 i = h, l+1, -1
            c( i-1 ) = c( i-1 ) + c( i )
40        continue
        endif

*       Load results into the id() array.
        j = 1 + incx*( n-1 )
        do 50 i = n, 1, -1
          w = x( j )
          id( 1+incd*( c( w )-1 ) ) = i
          c( w ) = c( w ) - 1
          j = j - incx
50      continue

100     continue
      return
      end
