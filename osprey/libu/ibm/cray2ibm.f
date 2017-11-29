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



      INTEGER FUNCTION
     +CRAY2IBM(TYPE, NUM, FORN, BITOFF, CRAY, STRD, CRAYCHAR)

CCCCCC      IMPLICIT NONE

C
C     The CRAY2IBM subroutine converts CRAY numbers to IBM format
C
C     The returned function value is 0 if OK, >0 if numeric errors
C     occurred, and <0 if an error.  The error number is the negative
C     of the return value.
C
      INTEGER TYPE                ! The variable type code used by the
C                                 ! libraries 0=typeless
C                                 !           1=integer
C                                 !           2=real
C                                 !           3=double precision
C                                 !           4=complex
C                                 !           5=logical
C                                 !           6=character
C                                 !           7=short int
C                                 !           8=single to double precision  
C                                 !
      INTEGER NUM                 ! Number of data items to
C                                 ! convert.  Type integer variable,
C                                 ! expression, or constant.
C                                 !
      INTEGER FORN(0:0)           ! Variable or array of any type
C                                 ! or length to receive the converted
C                                 ! IBM numbers.
C                                 !
      INTEGER BITOFF              ! Bit number within FORN to place the
C                                 ! converted numbers.  Type integer var.
C                                 ! expression, or constant.  Bits are
C                                 ! numbered from 0, beginning at the
C                                 ! leftmost bit of FORN.
C                                 !
      INTEGER CRAY(0:0)           ! Variable or array to
C                                 ! contain the converted values.
C                                 !
      INTEGER STRD                ! Memory increment for loading the
C                                 ! CRAY items to be converted. The
C                                 ! bits are placed in FORN regardless
C                                 ! of this parameter in a continuous bit
C                                 ! stream.  (No striding is done on
C                                 ! output.  For double word items this
C                                 ! is a stride of items, NOT of words.
C                                 ! Default stride for COMPLEX input
C                                 ! is one.  Optional
C                                 ! parameter of type integer variable,
C                                 ! expression, or constant.  Default
C                                 ! value is 1.
C                                 !
      CHARACTER*1 CRAYCHAR(0:0)   ! Optional parameter specifying
C                                 ! CRAY character variable if it is of
C                                 ! CHARACTER type

C     Vector Functions
C     These all must be declared integer for the vector loops to work

      INTEGER           II32O
      INTEGER           IF32O
      INTEGER           ID64O
      INTEGER           IC64O
      INTEGER           IL32O
C     CHARACTER*1       ICHRO(0:0)
      INTEGER           ICHRO
      INTEGER           II16O     ! really integer*2

CDIR$ VFUNCTION II32O
CDIR$ VFUNCTION IF32O
CDIR$ VFUNCTION ID64O
CDIR$ VFUNCTION IC64O
CDIR$ VFUNCTION IL32O
CDIR$ VFUNCTION ICHRO
CDIR$ VFUNCTION II16O

      EXTERNAL MOVBITZ

      REAL             CRAYR(0:0)
      DOUBLE PRECISION CRAYDBL(0:0)
      COMPLEX          CRAYCPL(0:0)
      INTEGER          CRAYL(0:0)

      POINTER (RPTR, CRAYR)
      POINTER (DPTR, CRAYDBL)
      POINTER (CPTR, CRAYCPL)
      POINTER (LPTR, CRAYL)

      INTEGER I, J, K, AND, MYCHAR, IIN, JNC, ISIZE(0:8), FULLW, ISTART
      INTEGER BO, LEFT, SHIFTC, MYSIZE
      INTEGER NSIZ
      INTEGER ALSTUF
      INTEGER PADVAL

      COMMON /G@IBM@SZ/ NSIZ,ISIZE
      DATA NSIZ  /9/
      DATA ISIZE /64, 32, 32, 64, 64, 32, 8, 16, 64/

      COMMON /G@IBM@AL/ ALSTUF,PADVAL
      DATA ALSTUF,PADVAL /0,0/     ! IBM does no alignment

CDIR$ EJECT

      IF (NUM .LE. 0) RETURN
      IF (BITOFF .GE. 64) GOTO 9000

      IF (NUMARG() .EQ. 5) THEN
        JNC = 1
      ELSE IF (NUMARG() .GE. 6) THEN
        JNC = STRD
      ELSE
        GOTO 9000
      ENDIF

      MYSIZE = ISIZE(TYPE)

      IF (TYPE .EQ. 6 .AND. NUMARG() .LT. 7)
     +  GOTO 9000             ! need CRAYCHAR to do characters

      IIN = 0
      RPTR = LOC(CRAY(0))
      DPTR = LOC(CRAY(0))
      CPTR = LOC(CRAY(0))
      LPTR = LOC(CRAY(0))

C     Determine how many full words there are to process.

      IF ((NUM*MYSIZE - AND((64 - BITOFF),77B)) .LT.0) THEN
        FULLW = 0
      ELSE
        FULLW = (NUM*MYSIZE - AND((64 - BITOFF),77B))/64
      ENDIF

      GOTO (100,200,300,400,500,600,700,800,900),TYPE+1


C     TYPE = 0  (typeless)             function IB64O               ************

  100 CONTINUE
      IF (BITOFF.EQ.0) THEN
        DO 105 I=0,NUM-1
          FORN(I) = CRAY(IIN)
          IIN = IIN + JNC
  105   CONTINUE
      ELSE

C       Patch up the first word
  
        FORN(0) = CSMG(FORN(0),
     +                 SHIFTR(CRAY(0),BITOFF),
     +                 MASK(BITOFF))
        DO 115 I=1,NUM-1
          FORN(I) = OR(SHIFTL(CRAY(IIN),    64-BITOFF),
     +                 SHIFTR(CRAY(IIN+JNC),BITOFF))
          IIN = IIN + JNC
  115   CONTINUE

C       Patch up the LAST word
  
        FORN(NUM) = CSMG(FORN(NUM),
     +                 SHIFTL(CRAY(IIN),64-BITOFF),
     +                 MASK(64+BITOFF)) ! mask from right
      ENDIF
      GOTO 1000

C     TYPE = 1  (integer)              function II32O               ************

  200 CONTINUE
C
C       Pretend that there is no bit offset, we will fix that later
C
      ISTART = 0
      IF(BITOFF.NE.0) ISTART = 1
      DO 210 I=ISTART,FULLW-1+ISTART
        FORN(I) = OR(SHIFTL(II32O(CRAY(IIN    )),32),
     +                      II32O(CRAY(IIN+JNC)))
        IIN = IIN + JNC*2
  210 CONTINUE

      IF(BITOFF .NE. 0) THEN

C       If the bit offset is > 0, must shift result up, and fix up tail
C       First fix up first word

        FORN(0) = CSMG(FORN(0),
     +                 SHIFTR(FORN(1),BITOFF),
     +                 MASK(BITOFF))
CDIR$ IVDEP
        DO 220 I=1,FULLW
          FORN(I) = OR(SHIFTL(FORN(I),  64-BITOFF),
     +                 SHIFTR(FORN(I+1),BITOFF))
  220   CONTINUE
      ENDIF

C     Patch up the last elements/words

      BO = BITOFF
      LEFT = (NUM*MYSIZE - (FULLW*64))/MYSIZE
      DO 230 I=1,LEFT
        J = II32O(CRAY(IIN))
        CALL MOVBITZ(J,32,MYSIZE,
     +               FORN(FULLW+SHIFTR(BO,6)),AND(BO,77B))
        BO = BO + MYSIZE
        IIN = IIN + JNC
  230 CONTINUE
      GOTO 1000

C     TYPE = 2  (real)                 function IF32O               ************

  300 CONTINUE
C
C       Pretend that there is no bit offset, we will fix that later
C
      ISTART = 0
      IF(BITOFF.NE.0) ISTART = 1
      DO 310 I=ISTART,FULLW-1+ISTART
        FORN(I) = OR(SHIFTL(IF32O(CRAY(IIN    )),32),
     +                      IF32O(CRAY(IIN+JNC)))
        IIN = IIN + JNC*2
  310 CONTINUE

      IF(BITOFF .NE. 0) THEN

C       If the bit offset is > 0, must shift result up, and fix up tail
C       First fix up first word

        FORN(0) = CSMG(FORN(0),
     +                 SHIFTR(FORN(1),BITOFF),
     +                 MASK(BITOFF))
CDIR$ IVDEP
        DO 320 I=1,FULLW
          FORN(I) = OR(SHIFTL(FORN(I),  64-BITOFF),
     +                 SHIFTR(FORN(I+1),BITOFF))
  320   CONTINUE
      ENDIF

C     Patch up the last elements/words

      BO = BITOFF
      LEFT = (NUM*MYSIZE - (FULLW*64))/MYSIZE
      DO 330 I=1,LEFT
        J = IF32O(CRAY(IIN))
        CALL MOVBITZ(J,32,MYSIZE,
     +               FORN(FULLW+SHIFTR(BO,6)),AND(BO,77B))
        BO = BO + MYSIZE
        IIN = IIN + JNC
  330 CONTINUE
      GOTO 1000

C     TYPE = 3  (double precision)     function ID64O               ************

  400 CONTINUE
      IF (BITOFF.EQ.0) THEN
        DO 405 I=0,NUM-1
          FORN(I) = ID64O(CRAYDBL(IIN))
          IIN = IIN + JNC
  405   CONTINUE
      ELSE

C       Patch up the first word
  
        FORN(0) = CSMG(FORN(0),
     +                 SHIFTR(ID64O(CRAYDBL(0)),BITOFF),
     +                 MASK(BITOFF))
        DO 415 I=1,NUM-1
          FORN(I) = OR(SHIFTL(ID64O(CRAYDBL(IIN)),    64-BITOFF),
     +                 SHIFTR(ID64O(CRAYDBL(IIN+JNC)),BITOFF))
          IIN = IIN + JNC
  415   CONTINUE

C       Patch up the LAST word
  
        FORN(NUM) = CSMG(FORN(NUM),
     +                 SHIFTL(ID64O(CRAYDBL(IIN)),64-BITOFF),
     +                 MASK(64+BITOFF)) ! mask from right
      ENDIF
      GOTO 1000

C     TYPE = 4  (complex)              function IC64O               ************

  500 CONTINUE
      IF (BITOFF.EQ.0) THEN
        DO 505 I=0,NUM-1
          FORN(I) = IC64O(CRAYCPL(IIN))
          IIN = IIN + JNC
  505   CONTINUE
      ELSE

C       Patch up the first word
  
        FORN(0) = CSMG(FORN(0),
     +                 SHIFTR(IC64O(CRAYCPL(0)),BITOFF),
     +                 MASK(BITOFF))
        DO 515 I=1,NUM-1
          FORN(I) = OR(SHIFTL(IC64O(CRAYCPL(IIN)),    64-BITOFF),
     +                 SHIFTR(IC64O(CRAYCPL(IIN+JNC)),BITOFF))
          IIN = IIN + JNC
  515   CONTINUE

C       Patch up the LAST word
  
        FORN(NUM) = CSMG(FORN(NUM),
     +                 SHIFTL(IC64O(CRAYCPL(IIN)),64-BITOFF),
     +                 MASK(64+BITOFF)) ! mask from right
      ENDIF
      GOTO 1000

C     TYPE = 5  (logical)              function IL32O               ************

  600 CONTINUE
C
C       Pretend that there is no bit offset, we will fix that later
C
      ISTART = 0
      IF(BITOFF.NE.0) ISTART = 1
      DO 610 I=ISTART,FULLW-1+ISTART
        FORN(I) = OR(SHIFTL(IL32O(CRAY(IIN    )),32),
     +                      IL32O(CRAY(IIN+JNC)))
        IIN = IIN + JNC*2
  610 CONTINUE

      IF(BITOFF .NE. 0) THEN

C       If the bit offset is > 0, must shift result up, and fix up tail
C       First fix up first word

        FORN(0) = CSMG(FORN(0),
     +                 SHIFTR(FORN(1),BITOFF),
     +                 MASK(BITOFF))
CDIR$ IVDEP
        DO 620 I=1,FULLW
          FORN(I) = OR(SHIFTL(FORN(I),  64-BITOFF),
     +                 SHIFTR(FORN(I+1),BITOFF))
  620   CONTINUE
      ENDIF

C     Patch up the last elements/words

      BO = BITOFF
      LEFT = (NUM*MYSIZE - (FULLW*64))/MYSIZE
      DO 630 I=1,LEFT
        J = IL32O(CRAY(IIN))
        CALL MOVBITZ(J,32,MYSIZE,
     +               FORN(FULLW+SHIFTR(BO,6)),AND(BO,77B))
        BO = BO + MYSIZE
        IIN = IIN + JNC
  630 CONTINUE
      GOTO 1000

C     TYPE = 6  (character)            function ICHRO               ************

  700 CONTINUE
C
C Convert character items.  This goes from packed to packed
C
      DO 710 I=0,NUM-1
        SHIFTC = 56 - AND(I+BITOFF/8,7)*8
        J = (I*8+BITOFF)/64
        MYCHAR = ICHAR(CRAYCHAR(IIN))
        MYCHAR = ICHRO(MYCHAR)
        FORN(J) = CSMG(SHIFTL(MYCHAR,     SHIFTC),
     +                 FORN(J),
     +                 SHIFTL(MASK(128-8),SHIFTC))
        IIN = IIN + JNC
  710 CONTINUE
      GOTO 1000

C     TYPE = 7  (short integer)        function II16O               ************

  800 CONTINUE
C
C       Pretend that there is no bit offset, we will fix that later
C
      ISTART = 0
      IF(BITOFF.NE.0) ISTART = 1
      DO 810 I=ISTART,FULLW-1+ISTART
        J = OR(SHIFTL(II16O(CRAY(IIN      )),48),
     +         SHIFTL(II16O(CRAY(IIN+JNC  )),32))
        K = OR(SHIFTL(II16O(CRAY(IIN+JNC*2)),16),
     +                II16O(CRAY(IIN+JNC*3)))
        FORN(I) = OR(J, K)
        IIN = IIN + JNC*4
  810 CONTINUE

C     Adjust the result words.
C     Patch up the trailing partial words/items.

      IF(BITOFF .NE. 0) THEN

C       If the bit offset is > 0, must shift result down
C       First fix up first word

        FORN(0) = CSMG(FORN(0),
     +                 SHIFTR(FORN(ISTART),BITOFF),
     +                 MASK(BITOFF))
CDIR$ IVDEP
        DO 820 I=1,FULLW
          FORN(I) = OR(SHIFTL(FORN(I),  64-BITOFF),
     +                 SHIFTR(FORN(I+1),BITOFF))
  820   CONTINUE
      ENDIF
C     Patch up the last elements/words

      BO = BITOFF
      LEFT = (NUM*MYSIZE - (FULLW*64))/MYSIZE
      DO 830 I=1,LEFT
        J = II16O(CRAY(IIN))
        CALL MOVBITZ(J,48,MYSIZE,
     +               FORN(FULLW+SHIFTR(BO,6)),AND(BO,77B))
        BO = BO + MYSIZE
        IIN = IIN + JNC
  830 CONTINUE
      GOTO 1000

C     TYPE = 8  (single-precision to double-precision)              ************

  900 CONTINUE
      IF (BITOFF.EQ.0) THEN
        DO I=0,NUM-1
          FORN(I) = ID64O(DBLE(CRAYR(IIN)))
          IIN = IIN + JNC
        ENDDO
      ELSE

C       Patch up the first word

        FORN(0) = CSMG(FORN(0),
     +                 SHIFTR(ID64O(DBLE(CRAYR(0))),BITOFF),
     +                 MASK(BITOFF))
        DO I=1,NUM-1
          FORN(I) = OR(SHIFTL(ID64O(DBLE(CRAYR(IIN))),    64-BITOFF),
     +                 SHIFTR(ID64O(DBLE(CRAYR(IIN+JNC))),BITOFF))
          IIN = IIN + JNC
        ENDDO

C       Patch up the LAST word

        FORN(NUM) = CSMG(FORN(NUM),
     +                 SHIFTL(ID64O(DBLE(CRAYR(IIN))),64-BITOFF),
     +                 MASK(64+BITOFF)) ! mask from right
      ENDIF
      GOTO 1000

 1000 CONTINUE
      CRAY2IBM = 0    ! we don't return status yet...
      RETURN

 9000 CONTINUE
      CRAY2IBM = -1    ! some error in params
      RETURN
CDIR$ ID "@(#) libu/ibm/cray2ibm.f	92.0	10/08/98 14:57:41"
      END
