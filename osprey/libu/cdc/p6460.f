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


      subroutine P6460(SOURCE, DEST, ISBIT, NUM, ISTRD)
      implicit  none
      integer SOURCE (*)
      integer   DEST (*)
      integer  ISBIT
      integer    NUM
      integer  ISTRD

C     P6460 - Pack 60-bit quantities into 64-bit words
C
C     SOURCE - Beginning location of array containing 60-bit quantities
C              left-justified in a 64-bit Cray word.
C
C     DEST   - Destination array to contain the packed 60-bit quantities.
C
C     ISBIT  - The starting bit position within the first word of the
C              destination array to use as the leftmost 60-bit
C              quantity.                                                
C
C     NUM    - Number of 60-bit quantities to pack.
C
C     ISTRD  - Optional stride for the source array, default is 1.
C
C     The routine operates on 960 bits at a time (960 = 16*60 = 15*64).
C
C     The algorithm used requires that the source and destination arrays
C     do not overlap.

      integer  mask60
      integer  sbit
      integer  nbits
      integer  destlwa
      integer  stride
      integer  lastsbit
      integer  firstwd
      integer  lastwd
      integer  sbt1
      integer  sbt2
      integer  i, j, k, l, m, n

      external abort
      integer  numarg
      external tracebk

      mask60(i) = and(mask(60), i)

      firstwd  = DEST(1)
      stride   = 1
      sbit     = ISBIT
      nbits    = (60 * NUM) + sbit
      destlwa  = shiftr(nbits+63, 6)
      lastsbit = and(63, nbits)
      lastwd   = DEST(destlwa)

C     Validate minimum number of arguments

      if (numarg() .lt. 4) then
        call tracebk
        call abort('P6460 must be called with at least 4 arguments.')
      else
        if (numarg() .gt. 4) then
          stride = ISTRD
        endif
      endif

C     Check for valid ISB

      if (sbit .lt. 0 .or. sbit .ge. 64) then
C       call uterp(47)
        call tracebk
        call abort('P6460 ISB parameter must be 0 <= ISB <= 63.')
      endif

C     Check for overlapping arrays

      if ((loc(DEST) .le. loc(SOURCE)+((NUM-1)*stride)) .and.
     +    (loc(SOURCE) .le. loc(DEST)+destlwa-1)) then
C       call uterp(48)
        call tracebk
        call abort('Arrays passed to P6460 must not overlap.')
      endif

C     The same pattern of bit shuffling holds as each 16 word input
C     group is moved to the corresponding 15 word output group.
C     The outer loop operates on groups of 960 (960 = 16*60 = 15*64)
C     bits at a time.  Expressed in a pseudo-language, the following
C     sequence is representative of outer loop execution:
C
C       DEST( 1) = SOURCE( 1)[00:59]<<00 | SOURCE( 2)[00:03]>>60
C       DEST( 2) = SOURCE( 2)[04:59]<<04 | SOURCE( 3)[00:07]>>56
C       DEST( 3) = SOURCE( 3)[08:59]<<08 | SOURCE( 4)[00:11]>>52
C       DEST( 4) = SOURCE( 4)[12:59]<<12 | SOURCE( 5)[00:15]>>48
C       DEST( 5) = SOURCE( 5)[16:59]<<16 | SOURCE( 6)[00:19]>>44
C       DEST( 6) = SOURCE( 6)[20:59]<<20 | SOURCE( 7)[00:23]>>40
C       DEST( 7) = SOURCE( 7)[24:59]<<24 | SOURCE( 8)[00:27]>>36
C       DEST( 8) = SOURCE( 8)[28:59]<<28 | SOURCE( 9)[00:31]>>32
C       DEST( 9) = SOURCE( 9)[32:59]<<32 | SOURCE(10)[00:35]>>28
C       DEST(10) = SOURCE(10)[36:59]<<36 | SOURCE(11)[00:39]>>24
C       DEST(11) = SOURCE(11)[40:59]<<40 | SOURCE(12)[00:43]>>20
C       DEST(12) = SOURCE(12)[44:59]<<44 | SOURCE(13)[00:47]>>16
C       DEST(13) = SOURCE(13)[48:59]<<48 | SOURCE(14)[00:51]>>12
C       DEST(14) = SOURCE(14)[52:59]<<52 | SOURCE(15)[00:55]>>08
C       DEST(15) = SOURCE(15)[56:59]<<56 | SOURCE(16)[00:59]>>04
C
C     The inner loop will execute the appropriate one of the above
C     lines for all groups.  The outer loop will be over all of the
C     lines.
C
C     Note that the outer loop is going to be executed 15 times
C     regardless.  If the destination array contains an appropriate
C     word to be filled in, the inner loop will execute; otherwise,
C     the loop will execute only as many times as necessary (possibly
C     none).
C
C     For example, consider the N=1 case.  The inner loop will be
C     executed only once per iteration of the outer loop.
C
C     Because of this algorithm, the source and destination arrays
C     cannot overlap.
C
C     The data is moved as if the destination is word-aligned and,
C     if necessary, is adjusted later.

      sbt1 = 0
      sbt2 = 60

      do 200 i = 1, 15
        j    = 0
        do 100 k = i, NUM, 16  !  Over all equally patterned segments
          l       = i + (j * 15)
          m       = i + (j * 16)
          n       = ((m - 1) * stride) + 1
          DEST(l) = or(shiftl(mask60(SOURCE(n)      ), sbt1),
     $                 shiftr(       SOURCE(n+stride), sbt2))
          j       = j + 1
  100   continue
        sbt2 = sbt2 - 4
        sbt1 = sbt1 + 4
  200 continue
 
C     If destination is not word-aligned, then move it.

      if (sbit .gt. 0) then
        sbt2 = 64 - sbit

CDIR$   IVDEP                                                             
        do 300 k = destlwa, 1, -1
          DEST(k) = or(shiftr(DEST(k  ), sbit),                        
     $                 shiftl(DEST(k-1), sbt2))
  300   continue

C       Patch first word

        DEST(1) = or(and(mask(     sbit), firstwd),
     $               and(mask(64 + sbit), DEST(1)))
      endif

C     Patch last word

      if (lastsbit .ne. 0) then
        DEST(destlwa) = or(and(mask(64 + lastsbit), lastwd),
     $                     and(mask(lastsbit), DEST(destlwa)))    
      endif

      return

CDIR$ ID "@(#) libu/cdc/p6460.f	92.0	10/08/98 14:57:41"

      end
