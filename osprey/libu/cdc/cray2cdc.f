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
     +CRAY2CDC(TYPE, NUM, FORN, BITOFF, CRAY, STRD, CRAYCHAR)

CCCCCC      IMPLICIT NONE

      INTEGER DTMPSIZ
      INTEGER ITMPSIZ
      PARAMETER (DTMPSIZ=256)
      PARAMETER (ITMPSIZ=2*DTMPSIZ)
C
C     The CRAY2CDC subroutine converts CRAY numbers to CDC format
C
C     The returned function value is 0 if OK, >0 if numer errors
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
C                                 !
      INTEGER NUM                 ! Number of data items to
C                                 ! convert.  Type integer variable,
C                                 ! expression, or constant.
C                                 !
      INTEGER FORN(0:0)           ! Variable or array of any type
C                                 ! or length to receive the converted
C                                 ! CDC numbers.
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

      INTEGER           CI60O
      REAL              CF60O
      DOUBLE PRECISION  CD120O
      COMPLEX           CC120O
      INTEGER           CL60O
C     CHARACTER*1       CCHRO(0:0)
      INTEGER           CCHRO
C     INTEGER           CI60O     ! really integer*2

CDIR$ VFUNCTION CI60O
CDIR$ VFUNCTION CF60O
CDIR$ VFUNCTION CD120O
CDIR$ VFUNCTION CC120O
CDIR$ VFUNCTION CL60O
CDIR$ VFUNCTION CCHRO

      EXTERNAL MOVBITZ

      REAL             CRAYR(0:0)
      DOUBLE PRECISION CRAYDBL(0:0)
      COMPLEX          CRAYCPL(0:0)
      INTEGER          CRAYL(0:0)

      POINTER (RPTR, CRAYR)
      POINTER (DPTR, CRAYDBL)
      POINTER (CPTR, CRAYCPL)
      POINTER (LPTR, CRAYL)

      INTEGER ITEMP(0:ITMPSIZ)
      REAL RTEMP(0:ITMPSIZ)
      LOGICAL LTEMP(0:ITMPSIZ)
      DOUBLE PRECISION DTEMP(0:DTMPSIZ)
      COMPLEX CTEMP(0:DTMPSIZ)

      EQUIVALENCE (ITEMP(0),RTEMP(0),LTEMP(0),DTEMP(0),CTEMP(0))
      INTEGER I, J, K, AND, MYCHAR, IIN, JNC, ISIZE(0:8)
      INTEGER BO, LEFT, MYSIZE, OWORD, CDCSC
      INTEGER NSIZ
      INTEGER ALSTUF
      INTEGER ALVALU
      INTEGER PADVAL

      COMMON /G@CDC@SZ/ NSIZ,ISIZE
      DATA NSIZ  /8/
      DATA ISIZE /64, 60, 60, 120, 120, 60, 6, 60/

      PARAMETER (ALVALU =  X'8000000000000000'.OR.60)
      COMMON /G@CDC@AL/ ALSTUF,PADVAL
      DATA ALSTUF,PADVAL /ALVALU,1333333333333333333320B/

CDIR$ EJECT

      IF (NUM .LE. 0) RETURN
      IF (BITOFF .GE. 64) GOTO 999 ! CALL UTERP(034)

      IF (NUMARG() .EQ. 5) THEN
        JNC = 1
      ELSE IF (NUMARG() .GE. 6) THEN
        JNC = STRD
      ELSE
        GOTO 999 ! UTERP(034)
      ENDIF

      MYSIZE = ISIZE(TYPE)

      IF (TYPE .EQ. 6 .AND. NUMARG() .LT. 7)
     +  GOTO 999 ! UTERP(034) ! need CRAYCHAR to do characters

      IIN = 0
      RPTR = LOC(CRAY(0))
      DPTR = LOC(CRAY(0))
      CPTR = LOC(CRAY(0))
      LPTR = LOC(CRAY(0))

      GOTO (100,200,300,400,500,600,700,800),TYPE+1
C                                 !  CB64O    0=typeless
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
      GOTO 900

C     =========================== !  CI60O    1=integer
  200 CONTINUE
C
C     Strip mine a loop that uses the convert -> temp,
C     pack temp -> target
C
      OWORD = 0
      LEFT = NUM
      DO 230 ISTRIP = 0,NUM-1,ITMPSIZ
        ICHUNK = ITMPSIZ
        IF (ICHUNK.GT.LEFT) ICHUNK=LEFT
        DO 210 J=0,ICHUNK-1
          ITEMP(J) = SHIFTL(CI60O(CRAY(IIN)),4)
          IIN = IIN + JNC
  210   CONTINUE
        BO = AND(MYSIZE * ISTRIP + BITOFF, 63)
        OWORD = SHIFTR(MYSIZE * ISTRIP + BITOFF, 6)
        CALL P6460(ITEMP(0),FORN(OWORD),BO,ICHUNK)
        LEFT = LEFT - ICHUNK
  230 CONTINUE

      GOTO 900
C     =========================== !  CF60O    2=real
  300 CONTINUE
C
C     Strip mine a loop that uses the convert -> temp,
C     pack temp -> target
C
      OWORD = 0
      LEFT = NUM
      DO 330 ISTRIP = 0,NUM-1,ITMPSIZ
        ICHUNK = ITMPSIZ
        IF (ICHUNK.GT.LEFT) ICHUNK=LEFT
        DO 310 J=0,ICHUNK-1
          RTEMP(J) = SHIFTL(CF60O(CRAYR(IIN)),4)
          IIN = IIN + JNC
  310   CONTINUE
        BO = AND(MYSIZE * ISTRIP + BITOFF, 63)
        OWORD = SHIFTR(MYSIZE * ISTRIP + BITOFF, 6)
        CALL P6460(RTEMP(0),FORN(OWORD),BO,ICHUNK)
        LEFT = LEFT - ICHUNK
  330 CONTINUE
      GOTO 900

C     =========================== !  CD120O    3=double precision
  400 CONTINUE
C
C     Strip mine a loop that uses the convert -> temp,
C     pack temp -> target
C
      OWORD = 0
      LEFT = NUM
      DO 430 ISTRIP = 0,NUM-1,DTMPSIZ
        ICHUNK = DTMPSIZ
        IF (ICHUNK.GT.LEFT) ICHUNK=LEFT
        DO 410 J=0,ICHUNK-1
          DTEMP(J) = CD120O(CRAYDBL(IIN))
          IIN = IIN + JNC
  410   CONTINUE
C       Left justify result.  note EQUIVALENCE!
        DO 420 J=0,ICHUNK-1
          ITEMP(J*2  ) = SHIFTL(ITEMP(J*2  ),4)
          ITEMP(J*2+1) = SHIFTL(ITEMP(J*2+1),4)
  420   CONTINUE
        BO = AND(MYSIZE * ISTRIP + BITOFF, 63)
        OWORD = SHIFTR(MYSIZE * ISTRIP + BITOFF, 6)
        CALL P6460(DTEMP(0),FORN(OWORD),BO,ICHUNK*2)
        LEFT = LEFT - ICHUNK
  430 CONTINUE
      GOTO 900
C     =========================== !  CC120O    4=complex
  500 CONTINUE
C
C     Strip mine a loop that uses the convert -> temp,
C     pack temp -> target
C
      OWORD = 0
      LEFT = NUM
      DO 530 ISTRIP = 0,NUM-1,DTMPSIZ
        ICHUNK = DTMPSIZ
        IF (ICHUNK.GT.LEFT) ICHUNK=LEFT
        DO 510 J=0,ICHUNK-1
          CTEMP(J) = CC120O(CRAYCPL(IIN))
          IIN = IIN + JNC
  510   CONTINUE
C       Left justify result.  note EQUIVALENCE!
        DO 520 J=0,ICHUNK-1
          ITEMP(J*2  ) = SHIFTL(ITEMP(J*2  ),4)
          ITEMP(J*2+1) = SHIFTL(ITEMP(J*2+1),4)
  520   CONTINUE
        BO = AND(MYSIZE * ISTRIP + BITOFF, 63)
        OWORD = SHIFTR(MYSIZE * ISTRIP + BITOFF, 6)
        CALL P6460(CTEMP(0),FORN(OWORD),BO,ICHUNK*2)
        LEFT = LEFT - ICHUNK
  530 CONTINUE
      GOTO 900
C     =========================== !  CL60O    5=logical
  600 CONTINUE
C
C     Strip mine a loop that uses the convert -> temp,
C     pack temp -> target
C
      OWORD = 0
      LEFT = NUM
      DO 630 ISTRIP = 0,NUM-1,ITMPSIZ
        ICHUNK = ITMPSIZ
        IF (ICHUNK.GT.LEFT) ICHUNK=LEFT
        DO 610 J=0,ICHUNK-1
          ITEMP(J) = SHIFTL(CL60O(CRAY(IIN)),4)  ! pretend int = logical
          IIN = IIN + JNC
  610   CONTINUE
        BO = AND(MYSIZE * ISTRIP + BITOFF, 63)
        OWORD = SHIFTR(MYSIZE * ISTRIP + BITOFF, 6)
        CALL P6460(LTEMP(0),FORN(OWORD),BO,ICHUNK)
        LEFT = LEFT - ICHUNK
  630 CONTINUE
      GOTO 900
C     =========================== !  CCHRO    6=character
  700 CONTINUE
C
C Convert character items.  This goes from packed to packed
C
      CDCSC = 0
      DO 710 I=0,NUM-1
        J = (I*6+BITOFF)/64
        BO = AND(I*6+BITOFF,63)
        MYCHAR = CCHRO(ICHAR(CRAYCHAR(IIN)))
        IF (BO.LE.58) THEN
          FORN(J) = CSMG(SHIFTL(MYCHAR,     58-BO),
     +                   FORN(J),
     +                   SHIFTL(MASK(128-6),58-BO))
        ELSE
          FORN(J) = CSMG(SHIFTR(MYCHAR,     BO-58),
     +                   FORN(J),
     +                   SHIFTR(MASK(128-6),BO-58))
          FORN(J+1) = CSMG(SHIFTL(MYCHAR,   122-BO),
     +                   FORN(J+1),
     +                   SHIFTL(MASK(128-6),122-BO))
        ENDIF
        IIN = IIN + JNC
  710 CONTINUE
      GOTO 900
C     =========================== !  CI60O    7=short int
  800 CONTINUE
C
C     Strip mine a loop that uses the convert -> temp,
C     pack temp -> target
C
      OWORD = 0
      LEFT = NUM
      DO 830 ISTRIP = 0,NUM-1,ITMPSIZ
        ICHUNK = ITMPSIZ
        IF (ICHUNK.GT.LEFT) ICHUNK=LEFT
        DO 810 J=0,ICHUNK-1
          ITEMP(J) = SHIFTL(CI60O(CRAY(IIN)),4)
          IIN = IIN + JNC
  810   CONTINUE
        BO = AND(MYSIZE * ISTRIP + BITOFF, 63)
        OWORD = SHIFTR(MYSIZE * ISTRIP + BITOFF, 6)
        CALL P6460(ITEMP(0),FORN(OWORD),BO,ICHUNK)
        LEFT = LEFT - ICHUNK
  830 CONTINUE

  900 CONTINUE
      CRAY2CDC = 0    ! we don't return status yet...
      RETURN

  999 CONTINUE
      CRAY2CDC = -1    ! some error in params
      RETURN
CDIR$ ID "@(#) libu/cdc/cray2cdc.f	92.0	10/08/98 14:57:41"
      END
