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


       integer function inflmax( n, ix, inc, mask, shift )
       integer n, inc, mask, shift
       integer ix( * )

*..... Local Variables
       integer i, j, val, max, index


*..... Executable Statements
*
*     -------------
*     USM What Line
*     -------------
CDIR$ ID "@(#) libu/sci/c2/inflmax.f	92.0	10/08/98 14:57:41"

         if( n .lt. 0 ) then
           write( *, 10 )
10         format( ' ** On entry to INFLMAX, N was negative.')
           goto 50
         elseif ( n .eq. 0 ) then
           write( *, 20 )
20         format( ' ** On entry to INFLMAX, N was 0.')
           goto 50
         elseif( inc .lt. 1 ) then
           write( *, 30 )
30         format( ' ** On entry to INFLMAX, INC was nonpositive.')
           goto 50
         endif

         max = and( shiftr( ix( 1 ), shift ), mask )
         index = 1
         j = 1 + inc
         do 40 i = 2, n
           val = and( shiftr( ix( j ), shift ), mask )
           if( val .gt. max ) then
             index = i
             max = val
           endif
           j = j + inc
40       continue
         inflmax = index

*..... Done
50     continue
       return
       end


