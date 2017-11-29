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


      subroutine isortb( ad, n, x, incx, id, incd )
      implicit none

*     ... Scalar arguments ...
        integer     incx, incd, n
        character*1 ad

*     ... Vector arguments ...
        integer     id( 1 )
        integer     x( * )

*     ... Local vector ...

*       For use in a call to GETTMC().  Acquire system information,
*       specifically, the number of banks.

*     ... Local variables ...
        integer ldn1, iend, iord, k, kiord
        integer i1, i2, j, kk, ll, kbd
        integer iarg, imin, imax, info
        logical asc, des
        integer amin, amax, x1, x2

*     ... External routines ...
	integer ihost_banks@

*     ... Begin Execution ...
*
CDIR$ ID "@(#) libu/sci/isortb.f	92.1	10/20/99 12:36:37"
*
        iarg = numarg()

*       Acquire banksize.  GETTMC is a system call the retreives
*       system information.  You pass it the address of the 128-word 
*       array, MCTABLE, and it returns the information.  In location
*       2 of MCTABLE, is the bank size of the target system.  I 
*       need this value in order to effectively govern when
*       I am going to do loop inversions.
        kbd = ihost_banks@()

*       Test input parameters
        des = ad .eq. 'D' .or. ad .eq. 'd'
        asc = ad .eq. 'A' .or. ad .eq. 'a'

        info = 0
        if( .not.des .and. .not.asc ) then
          info = 1
        elseif( n .le. 0 ) then
          info = 2
        elseif( incx .le. 0 ) then
          info = 4
        elseif( iarg .eq. 6 ) then
	  if( incd .le. 0 ) info = 6
        elseif( iarg.ne.4 ) then
          info = 7
        endif
        if( info .ne. 0 ) then
          call xersor( 'ISORTB ', info )
          goto 1500
        endif

        ldn1 = int( log( real( n ) - 0.5 ) / log( 2. ) )
*       Ascending order
        if( asc ) then
          if( iarg .eq. 4 ) goto 10
          if( iarg .eq. 6 ) goto 400

*         In-place sort
10        continue
          if( n .eq. 1 ) return

cmic$ parallel shared( incx, kbd, ldn1, n, x )
cmic$1       private ( amax, amin, i1, i2, iend, iord, j, k, kiord, kk )
cmic$2       private ( ll, x1, x2 )

*         This is the outer loop.  This governs the size of
*         the segments to be merged.
          do 300 ll = ldn1, 0, -1
            iord = 2**ll
            if( iord .ge. kbd .or. 2*iord*iord .gt. n ) goto 130
*
*           NOTE 1:
*           Suppose iord is 8.  What this doubly nested DO-loop
*           does, in effect, is that it starts out with the
*           first 8 elements.  It splits this segment in two
*           and compares each of the first 4 elements with
*           the corresponding element from the second set of
*           4 elements.  It then goes to the next segment of
*           8 elements and repeats the process until the end of
*           the array is reached.
*
cmic$ do parallel
            do 120 j = 1, iord
cdir$ ivdep
              do 110 i1 = j, n-iord, 2*iord
                i2 = i1 + iord
                x1 = x( 1+incx*(i1-1) ) 
                x2 = x( 1+incx*(i2-1) ) 
                amin = cvmgm( x2, x1, x2-x1 )
                amax = cvmgm( x1, x2, x2-x1 )

                x( 1+incx*(i1-1) ) = amin 
                x( 1+incx*(i2-1) ) = amax 
110           continue
120         continue
            goto 160

*
*           NOTE 2:
*           This is an inverted version of the above loop.  Loop
*           inversion helps with performance.  I always try to
*           keep the inner loops long for vectorization and
*           the outer loops short for parallel processing.
*
130         continue 
cmic$ do parallel
            do 150 j = 1, n-iord, 2*iord
              iend = min( j+iord-1, n-iord )
cdir$ ivdep
              do 140 i1 = j, iend
                i2 = i1 + iord
                x1 = x( 1+incx*(i1-1) ) 
                x2 = x( 1+incx*(i2-1) ) 
                amin = cvmgm( x2, x1, x2-x1 )
                amax = cvmgm( x1, x2, x2-x1 )

                x( 1+incx*(i1-1) ) = amin 
                x( 1+incx*(i2-1) ) = amax 
140           continue
150         continue


*
*           NOTE 3:
*           This triply nested DO-loop performs the task of "merging"
*           the results from the previous steps.  Going back to
*           segment length of 8, it finds out how many such segments
*           exist in the array.  For the sake of convenience, suppose
*           the size of the array is 64.  That means there are 8
*           segments of length 8.  What this part of the code
*           does is to first merge the first segment and the fifth
*           segment, the second segment and the sixth, etc.  Then it
*           does the first and third, the second and fourth, etc.
*           Finally it does the first and second, the third and
*           fourth.  When doing the merge of the say the first
*           and second segments of 8, the upper 4 elements of
*           the first segment are compared-exchanged with the
*           lower 4-elements of the second segment and so it goes.
*           The illustration in Knuth is instructive.
*
160         continue
            do 230 kk = ldn1-ll, 1, -1
              k = 2**kk-1
              kiord = k*iord
              if( iord .ge. kbd .or. 2*iord*iord .gt. n ) goto 190 

cmic$ do parallel
              do 180 j = 1, iord
cdir$ ivdep  
                do 170 i1 = j+iord, n-kiord, 2*iord
                  i2 = i1 + kiord
                  x1 = x( 1+incx*(i1-1) ) 
                  x2 = x( 1+incx*(i2-1) ) 
                  amin = cvmgm( x2, x1, x2-x1 )
                  amax = cvmgm( x1, x2, x2-x1 )

                  x( 1+incx*(i1-1) ) = amin 
                  x( 1+incx*(i2-1) ) = amax 
170             continue
180           continue
              goto 230

*
*             NOTE 4:
*             Inverted version of the previous doubly nested DO-loop.
*             The rest of the code consists of variations of the
*             present code except that it may either being doing
*             the sort in descending order and/or indexed.
*
190           continue
cmic$ do parallel
              do 210 j = 1, n-kiord-iord, 2*iord
                iend = min( j+2*iord-1, n-kiord )
cdir$ ivdep
                  do 200 i1 = j+iord, iend
                  i2 = i1 + kiord
                  x1 = x( 1+incx*(i1-1) ) 
                  x2 = x( 1+incx*(i2-1) ) 
                  amin = cvmgm( x2, x1, x2-x1 )
                  amax = cvmgm( x1, x2, x2-x1 )

                  x( 1+incx*(i1-1) ) = amin 
                  x( 1+incx*(i2-1) ) = amax 
200             continue
210           continue
230         continue
300       continue
cmic$ end parallel
          goto 610


*         Indexed sort
400       continue
          if( n .eq. 1 ) then
            id( 1 ) = 1
            return
          endif
*
          do 405 i1 = 1, n
            id( 1+incd*(i1-1) ) = i1
405       continue
*
cmic$ parallel shared ( id, incd, incx, kbd, ldn1, n, x )
cmic$1    private ( i1, i2, iend, imax, imin, iord, j, k, kiord, kk )
cmic$2    private ( ll, x1, x2 )
          do 600 ll = ldn1, 0, -1
            iord = 2**ll
            if( iord .ge. kbd .or. 2*iord*iord .gt. n ) goto 430
*
cmic$ do parallel
            do 420 j = 1, iord
cdir$ ivdep
              do 410 i1 = j, n-iord, 2*iord
                i2 = i1 + iord
                x1 = x( 1+incx*( id( 1+incd*( i1-1 ) )-1 ) )
                x2 = x( 1+incx*( id( 1+incd*( i2-1 ) )-1 ) )

                imin = cvmgm( id( 1+incd*(i2-1) ),
     &                        id( 1+incd*(i1-1) ),
     &                        x2-x1 ) 

                imax = cvmgm( id( 1+incd*(i1-1) ),
     &                        id( 1+incd*(i2-1) ),
     &                        x2-x1 )

                id( 1+incd*( i1-1 ) ) = imin
                id( 1+incd*( i2-1 ) ) = imax
410           continue
420         continue
            goto 460

430         continue 
cmic$ do parallel
            do 450 j = 1, n-iord, 2*iord
              iend = min( j+iord-1, n-iord )
cdir$ ivdep
              do 440 i1 = j, iend
                i2 = i1 + iord
                x1 = x( 1+incx*( id( 1+incd*( i1-1 ) )-1 ) )
                x2 = x( 1+incx*( id( 1+incd*( i2-1 ) )-1 ) )

                imin = cvmgm( id( 1+incd*(i2-1) ),
     &                        id( 1+incd*(i1-1) ),
     &                        x2-x1 ) 

                imax = cvmgm( id( 1+incd*(i1-1) ),
     &                        id( 1+incd*(i2-1) ),
     &                        x2-x1 )

                id( 1+incd*( i1-1 ) ) = imin
                id( 1+incd*( i2-1 ) ) = imax
440           continue
450         continue


460         continue
            do 530 kk = ldn1-ll, 1, -1
              k = 2**kk-1
              kiord = k*iord
              if( iord .ge. kbd .or. 2*iord*iord .gt. n ) goto 490 

cmic$ do parallel
              do 480 j = 1, iord
cdir$ ivdep  
                do 470 i1 = j+iord, n-kiord, 2*iord
                  i2 = i1 + kiord
                  x1 = x( 1+incx*( id( 1+incd*( i1-1 ) )-1 ) )
                  x2 = x( 1+incx*( id( 1+incd*( i2-1 ) )-1 ) )

                  imin = cvmgm( id( 1+incd*(i2-1) ),
     &                          id( 1+incd*(i1-1) ),
     &                          x2-x1 ) 

                  imax = cvmgm( id( 1+incd*(i1-1) ),
     &                          id( 1+incd*(i2-1) ),
     &                          x2-x1 )

                  id( 1+incd*( i1-1 ) ) = imin
                  id( 1+incd*( i2-1 ) ) = imax
470             continue
480           continue
              goto 530
  
490           continue
cmic$ do parallel
              do 510 j = 1, n-kiord-iord, 2*iord
                iend = min( j+2*iord-1, n-kiord )
cdir$ ivdep
                do 500 i1 = j+iord, iend
                  i2 = i1 + kiord
                  x1 = x( 1+incx*( id( 1+incd*( i1-1 ) )-1 ) )
                  x2 = x( 1+incx*( id( 1+incd*( i2-1 ) )-1 ) )

                  imin = cvmgm( id( 1+incd*(i2-1) ),
     &                          id( 1+incd*(i1-1) ),
     &                          x2-x1 ) 

                  imax = cvmgm( id( 1+incd*(i1-1) ),
     &                          id( 1+incd*(i2-1) ),
     &                          x2-x1 )

                  id( 1+incd*( i1-1 ) ) = imin
                  id( 1+incd*( i2-1 ) ) = imax
500             continue
510           continue
530         continue
600       continue
cmic$ end parallel

610       continue
        elseif( des ) then
          if( iarg .eq. 4 ) goto 710
          if( iarg .eq. 6 ) goto 1100

*         In-place sort
710       continue
cmic$ parallel shared( incx, kbd, ldn1, n, x )
cmic$1       private ( amax, amin, i1, i2, iend, iord, j, k, kiord, kk )
cmic$2       private ( ll, x1, x2 )
          do 1000 ll = ldn1, 0, -1
            iord = 2**ll
            if( iord .ge. kbd .or. 2*iord*iord .gt. n ) goto 830
*
cmic$ do parallel
            do 820 j = 1, iord
cdir$ ivdep
              do 810 i1 = j, n-iord, 2*iord
                i2 = i1 + iord
                x1 = x( 1+incx*(i1-1) ) 
                x2 = x( 1+incx*(i2-1) ) 
                amin = cvmgm( x2, x1, x2-x1 )
                amax = cvmgm( x1, x2, x2-x1 )

                x( 1+incx*(i1-1) ) = amax 
                x( 1+incx*(i2-1) ) = amin 
810           continue
820         continue
            goto 860

830         continue 
cmic$ do parallel
            do 850 j = 1, n-iord, 2*iord
              iend = min( j+iord-1, n-iord )
cdir$ ivdep
              do 840 i1 = j, iend
                i2 = i1 + iord
                x1 = x( 1+incx*(i1-1) ) 
                x2 = x( 1+incx*(i2-1) ) 
                amin = cvmgm( x2, x1, x2-x1 )
                amax = cvmgm( x1, x2, x2-x1 )

                x( 1+incx*(i1-1) ) = amax
                x( 1+incx*(i2-1) ) = amin
840           continue
850         continue


860         continue
            do 930 kk = ldn1-ll, 1, -1
              k = 2**kk-1
              kiord = k*iord
              if( iord .ge. kbd .or. 2*iord*iord .gt. n ) goto 890 

cmic$ do parallel
              do 880 j = 1, iord
cdir$ ivdep  
                do 870 i1 = j+iord, n-kiord, 2*iord
                  i2 = i1 + kiord
                  x1 = x( 1+incx*(i1-1) ) 
                  x2 = x( 1+incx*(i2-1) ) 
                  amin = cvmgm( x2, x1, x2-x1 )
                  amax = cvmgm( x1, x2, x2-x1 )

                  x( 1+incx*(i1-1) ) = amax
                  x( 1+incx*(i2-1) ) = amin
870             continue
880           continue
              goto 930

890           continue
cmic$ do parallel
              do 910 j = 1, n-kiord-iord, 2*iord
                iend = min( j+2*iord-1, n-kiord )
cdir$ ivdep
                  do 900 i1 = j+iord, iend
                  i2 = i1 + kiord
                  x1 = x( 1+incx*(i1-1) ) 
                  x2 = x( 1+incx*(i2-1) ) 
                  amin = cvmgm( x2, x1, x2-x1 )
                  amax = cvmgm( x1, x2, x2-x1 )

                  x( 1+incx*(i1-1) ) = amax
                  x( 1+incx*(i2-1) ) = amin
900             continue
910           continue
930         continue
1000      continue
cmic$ end parallel
          goto 1310


*         Indexed sort
1100       continue
*
          i2 = 1
          do 1105 i1 = 1, n
            id( i2 ) = i1
            i2 = i2 + incd
1105      continue
*
cmic$ parallel shared ( id, incd, incx, kbd, ldn1, n, x )
cmic$1       private ( i1, i2, iend, imax, imin, iord, j, k, kiord, kk )
cmic$2       private ( ll, x1, x2 )
          do 1300 ll = ldn1, 0, -1
            iord = 2**ll
            if( iord .ge. kbd .or. 2*iord*iord .gt. n ) goto 1130
*
cmic$ do parallel
            do 1120 j = 1, iord
cdir$ ivdep
              do 1110 i1 = j, n-iord, 2*iord
                i2 = i1 + iord
                x1 = x( 1+incx*( id( 1+incd*( i1-1 ) )-1 ) )
                x2 = x( 1+incx*( id( 1+incd*( i2-1 ) )-1 ) )

                imin = cvmgm( id( 1+incd*(i2-1) ),
     &                        id( 1+incd*(i1-1) ),
     &                        x2-x1 ) 

                imax = cvmgm( id( 1+incd*(i1-1) ),
     &                        id( 1+incd*(i2-1) ),
     &                        x2-x1 )

                id( 1+incd*( i1-1 ) ) = imax
                id( 1+incd*( i2-1 ) ) = imin
1110          continue
1120        continue
            goto 1160

1130        continue 
cmic$ do parallel
            do 1150 j = 1, n-iord, 2*iord
              iend = min( j+iord-1, n-iord )
cdir$ ivdep
              do 1140 i1 = j, iend
                i2 = i1 + iord
                x1 = x( 1+incx*( id( 1+incd*( i1-1 ) )-1 ) )
                x2 = x( 1+incx*( id( 1+incd*( i2-1 ) )-1 ) )

                imin = cvmgm( id( 1+incd*(i2-1) ),
     &                        id( 1+incd*(i1-1) ),
     &                        x2-x1 ) 

                imax = cvmgm( id( 1+incd*(i1-1) ),
     &                        id( 1+incd*(i2-1) ),
     &                        x2-x1 )

                id( 1+incd*( i1-1 ) ) = imax
                id( 1+incd*( i2-1 ) ) = imin
1140          continue
1150        continue


1160        continue
            do 1230 kk = ldn1-ll, 1, -1
              k = 2**kk-1
              kiord = k*iord
              if( iord .ge. kbd .or. 2*iord*iord .gt. n ) goto 1190 

cmic$ do parallel
              do 1180 j = 1, iord
cdir$ ivdep  
                do 1170 i1 = j+iord, n-kiord, 2*iord
                  i2 = i1 + kiord
                  x1 = x( 1+incx*( id( 1+incd*( i1-1 ) )-1 ) )
                  x2 = x( 1+incx*( id( 1+incd*( i2-1 ) )-1 ) )

                  imin = cvmgm( id( 1+incd*(i2-1) ),
     &                          id( 1+incd*(i1-1) ),
     &                          x2-x1 ) 

                  imax = cvmgm( id( 1+incd*(i1-1) ),
     &                          id( 1+incd*(i2-1) ),
     &                          x2-x1 )

                  id( 1+incd*( i1-1 ) ) = imax
                  id( 1+incd*( i2-1 ) ) = imin
1170            continue
1180          continue
              goto 1230
  
1190          continue
cmic$ do parallel
              do 1210 j = 1, n-kiord-iord, 2*iord
                iend = min( j+2*iord-1, n-kiord )
cdir$ ivdep
                do 1200 i1 = j+iord, iend
                  i2 = i1 + kiord
                  x1 = x( 1+incx*( id( 1+incd*( i1-1 ) )-1 ) )
                  x2 = x( 1+incx*( id( 1+incd*( i2-1 ) )-1 ) )

                  imin = cvmgm( id( 1+incd*(i2-1) ),
     &                          id( 1+incd*(i1-1) ),
     &                          x2-x1 ) 

                  imax = cvmgm( id( 1+incd*(i1-1) ),
     &                          id( 1+incd*(i2-1) ),
     &                          x2-x1 )

                  id( 1+incd*( i1-1 ) ) = imax
                  id( 1+incd*( i2-1 ) ) = imin
1200            continue
1210          continue
1230        continue
1300      continue
cmic$ end parallel

1310      continue
        endif 

1500  continue
      return
      end
