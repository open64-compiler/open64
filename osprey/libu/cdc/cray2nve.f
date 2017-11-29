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
     +CRAY2NVE(TYPE, NUM, FORN, BITOFF, CRAY, STRD, CRAYCH)
  
      IMPLICIT NONE
  
C
C     The CRAY2NVE subroutine converts CRAY numbers to NOS/VE format.
C
C     Return values:   <0 error in arguments
C                      =0 OK
c
      INTEGER TYPE                ! The variable type code used by the
C                                 ! libraries:
C                                 !   0 = typeless
C                                 !   1 = integer
C                                 !   2 = real
C                                 !   3 = double precision
C                                 !   4 = complex
C                                 !   5 = logical
C                                 !   6 = character
C                                 !   7 = short integer
C                                 !
      INTEGER NUM                 ! Number of data items to convert.
C                                 ! Type integer variable, expression,
C                                 ! or constant.
C                                 !
      INTEGER FORN(0:0)           ! Variable or array of any type or
C                                 ! length to receive the converted
C                                 ! NOS/VE numbers.
C                                 !
      INTEGER BITOFF              ! Bit offset within FORN to place the
C                                 ! converted numbers.  Type integer
C                                 ! variable, expression, or constant.
C                                 ! Bits are numbered from 0, beginning
C                                 ! at the leftmost bit of FORN.
C                                 !
      INTEGER CRAY(0:0)           ! Variable or array to contain the
C                                 ! converted values.
C                                 !
      INTEGER STRD                ! Memory increment for loading the
C                                 ! CRAY items to be converted.  The
C                                 ! bits are placed in FORN regardless
C                                 ! of this parameter in a continuous bit
C                                 ! stream (no striding is done on
C                                 ! output).  For two-word items, this
C                                 ! is a stride of items, NOT of words.
C                                 ! Optional parameter of type integer
C                                 ! variable, expression, or constant.
C                                 ! Default value is 1.
C                                 !
      CHARACTER * (*) CRAYCH(0:0) ! Optional parameter specifying CRAY
C                                 ! character variable if it is of type
C                                 ! CHARACTER.
  
      INTEGER          IVL
      PARAMETER        (IVL = 512)
  
      INTEGER          NF64O
  
      REAL             CRAYR(0:0)
      DOUBLE PRECISION CRAYDBL(0:0)
      COMPLEX          CRAYCPL(0:0)
      LOGICAL          CRAYL(0:0)
C     INTEGER * 2      CRAYS(0:0)       ! Short integer
  
      POINTER (RPTR, CRAYR)
      POINTER (DPTR, CRAYDBL)
      POINTER (CPTR, CRAYCPL)
      POINTER (LPTR, CRAYL)
C     POINTER (SPTR, CRAYS)             ! Short integer
  
      INTEGER CHRLEN, CHROFF
      INTEGER IWVAR, IDUMMY
      POINTER (IWADD,IWVAR)
      INTEGER AND, I, II, IIN, ISIZE(0:8), J, JNC, K, LEFT, OR
      INTEGER MYSIZE, NT, NF, TEMP(0:(2*IVL)-1)
      INTEGER NSIZ
      INTEGER ALSTUF
      INTEGER PADVAL

      EXTERNAL         MOVBITZ
  
      COMMON /G@NVE@SZ/ NSIZ,ISIZE
      DATA NSIZ  /8/
      DATA ISIZE /64, 64, 64, 128, 128, 64, 8, 64/

      COMMON /G@NVE@AL/ ALSTUF,PADVAL
      DATA ALSTUF,PADVAL  /0,0/    ! NOS/VE does no alignment

      NF64O(I) = I                 ! No-op for now; maybe a VFUNCTION later
  
CDIR$ EJECT
  
      IF (   NUM .LT.  0) GOTO 999 ! CALL UTERP(034)
      IF (BITOFF .GE. 64) GOTO 999 ! CALL UTERP(034)
  
      IF (NUMARG() .EQ. 5) THEN
        JNC = 1
        ELSE IF (NUMARG() .GE. 6) THEN
          JNC = STRD
          ELSE
            GOTO 999 ! UTERP(034)
      ENDIF
  
      IF ((TYPE .LT. 0) .OR. (TYPE .GT. 7)) GOTO 999 ! CALL UTERP(034)
  
      IF (TYPE .EQ. 6 .AND. NUMARG() .LT. 7)
     +  GOTO 999 ! UTERP(034) ! need CRAYCH to do characters
  
      IF (NUM .EQ. 0) RETURN
  
      IIN    = 0
      MYSIZE = ISIZE(TYPE)
      RPTR   = LOC(CRAY(0))
      DPTR   = LOC(CRAY(0))
      CPTR   = LOC(CRAY(0))
      LPTR   = LOC(CRAY(0))
C     SPTR   = LOC(CRAY(0))             ! Short integer
  
C     Process type of data
  
      GOTO (100,200,300,400,500,600,700,800),TYPE+1
  
C     Type = 0  (typeless)                                  ************
C     Type = 1  (integer)                                   ************
  
  100 CONTINUE
  200 CONTINUE
      IF (JNC .EQ. 1) THEN
        CALL MOVBITZ(CRAY, 0, NUM*MYSIZE, FORN, BITOFF)
      ELSE
  
        IF (BITOFF .EQ. 0) THEN
          DO 110 I = 0, NUM-1
            FORN(I) = CRAY(IIN)
            IIN     = IIN + JNC
  110     CONTINUE
        ELSE
  
          FORN(0) = CSMG(FORN(0),
     +                   SHIFTR(CRAY(IIN), BITOFF),
     +                   MASK(BITOFF))
  
          DO 120 I = 1, NUM-1
            FORN(I) = OR(SHIFTL(CRAY(IIN    ), 64-BITOFF),
     +                   SHIFTR(CRAY(IIN+JNC), BITOFF))
            IIN     = IIN + JNC
  120     CONTINUE
  
          FORN(NUM) = CSMG(FORN(NUM),
     +                     SHIFTL(CRAY(IIN), 64-BITOFF),
     +                     MASK(64+BITOFF))
        ENDIF
  
      ENDIF
      GOTO 900
  
C     Type = 2  (real)                                      ************
  
  300 CONTINUE
      IF (BITOFF .EQ. 0) THEN
        DO 310 I = 0, NUM-1
          FORN(I) = NF64O(CRAY(IIN))
          IIN     = IIN + JNC
  310   CONTINUE
  
      ELSE
  
        FORN(0) = CSMG(FORN(0),
     +                 SHIFTR(NF64O(CRAY(0)), BITOFF),
     +                 MASK(BITOFF))
  
        DO 320 I = 1, NUM-1
          FORN(I) = OR(SHIFTL(NF64O(CRAY(IIN)),     64-BITOFF),
     +                 SHIFTR(NF64O(CRAY(IIN+JNC)), BITOFF))
          IIN     = IIN + JNC
  320   CONTINUE
      ENDIF
  
        FORN(NUM) = CSMG(FORN(NUM),
     +                 SHIFTL(NF64O(CRAY(IIN)), 64-BITOFF),
     +                 MASK(64+BITOFF))
      GOTO 900
  
C     Type = 3  (double precision)                          ************
  
  400 CONTINUE
      IF (BITOFF .EQ. 0) THEN
        J = 0
        DO 410 I = 0, NUM-1
          FORN(J)   = NF64O(CRAY(IIN))
          FORN(J+1) = OR(AND(MASK(16), FORN(J)), CRAY(IIN+1))
          IIN       = IIN + (2*JNC)
          J         = J + 2
  410   CONTINUE
      ELSE
  
        I    = NUM
        II   = 0
        LEFT = MOD(NUM, IVL)
        IF (LEFT .EQ. 0) LEFT = IVL
  
C       Strip mine a vector
  
  420   CONTINUE
        J = 0
        DO 430 K = 1, LEFT
          TEMP(J)   = NF64O(CRAY(IIN))
          TEMP(J+1) = OR(AND(MASK(16), TEMP(J)), CRAY(IIN+1))
          IIN       = IIN + (2*JNC)
          J         = J + 2
  430   CONTINUE
  
        CALL MOVBITZ(TEMP, 0, LEFT*MYSIZE, FORN(II), BITOFF)
  
        II   = II + (2*LEFT)
        I    = I - LEFT
        LEFT = IVL
        IF (I .GT. 0) GOTO 420
  
      ENDIF
      GOTO 900
  
C     Type = 4  (complex)                                   ************
  
  500 CONTINUE
      J = 0
      IF (BITOFF .EQ. 0) THEN
        DO 510 I = 0, NUM-1
          FORN(J)   = NF64O(CRAY(IIN  ))
          FORN(J+1) = NF64O(CRAY(IIN+1))
          IIN       = IIN + (2*JNC)
          J         = J + 2
  510   CONTINUE
      ELSE
  
        I    = NUM
        II   = 0
        LEFT = MOD(NUM, IVL)
        IF (LEFT .EQ. 0) LEFT = IVL
  
C       Strip mine a vector
  
  520   CONTINUE
        J = 0
        DO 530 K = 1, LEFT
          TEMP(J)   = NF64O(CRAY(IIN  ))
          TEMP(J+1) = NF64O(CRAY(IIN+1))
          IIN       = IIN + (2*JNC)
          J         = J + 2
  530   CONTINUE
  
        CALL MOVBITZ(TEMP, 0, LEFT*MYSIZE, FORN(II), BITOFF)
  
        II   = II + (2*LEFT)
        I    = I - LEFT
        LEFT = IVL
        IF (I .GT. 0) GOTO 520
  
      ENDIF
      GOTO 900
  
C     Type = 5  (logical)                                   ************
  
  600 CONTINUE
      NT   = MASK(64) ! NOS/VE .TRUE.
      NF   = 0        ! NOS/VE .FALSE.
      IF (BITOFF .EQ. 0) THEN
        DO 605 I = 0, NUM-1
          FORN(I) = CVMGT(NT, NF, CRAYL(IIN))
          IIN     = IIN + JNC
  605   CONTINUE
      ELSE
  
C       Patch up first word
  
        FORN(0) = CSMG(FORN(0), SHIFTR(CVMGT(NT, NF, CRAYL(0)), BITOFF),
     +                 MASK(BITOFF))
  
        DO 615 I = 1, NUM-1
          FORN(I) = OR(SHIFTL(CVMGT(NT, NF, CRAYL(IIN)),     64-BITOFF),
     +                 SHIFTR(CVMGT(NT, NF, CRAYL(IIN+JNC)), BITOFF))
          IIN     = IIN + JNC
  615   CONTINUE
  
C       Patch up last word
  
        FORN(NUM) = CSMG(FORN(NUM),
     +                   SHIFTL(CVMGT(NT, NF, CRAYL(IIN)), 64-BITOFF),
     +                   MASK(64+BITOFF)) ! mask from right
      ENDIF
      GOTO 900
  
C     Type = 6  (character)                                 ************
  
  700 CONTINUE
      CHRLEN = MYSIZE * LEN(CRAYCH(0))          ! Length of each element
      IF (JNC .EQ. 1) THEN
        CALL G@CHRPCK(CRAYCH,IWADD,IDUMMY,CHROFF)
        CALL MOVBITZ(IWVAR, CHROFF, NUM*CHRLEN, FORN, BITOFF)
      ELSE
  
        J = 0
  
        DO 710 I = 0, NUM-1
          CALL G@CHRPCK(CRAYCH(IIN),IWADD,IDUMMY,CHROFF)
          CALL MOVBITZ(IWVAR, CHROFF, CHRLEN, FORN(J), BITOFF)
          BITOFF = BITOFF + CHRLEN
          IF (BITOFF .GE. 64) THEN
            J      = J + (BITOFF / 64)
            BITOFF = MOD (BITOFF, 64)
          ENDIF
          IIN    = IIN + JNC
  710   CONTINUE
  
      ENDIF
      GOTO 900
  
C     Type = 7  (short integer)                             ************
  
  800 CONTINUE
  
C     If bit 2**31 is set, assume short integer is negative and propagate
C     sign bit.  In any case, assume number is less than 2**31.
  
      IF (BITOFF .EQ. 0) THEN
        DO 810 I = 0, NUM-1
          FORN(I) = CVMGN(AND(  MASK(64+32), CRAY(IIN)),
     +                     OR(  MASK(   32), CRAY(IIN)),
     +                    AND(SHIFTL(1, 31), CRAY(IIN)))
          IIN     = IIN + JNC
  810   CONTINUE
  
      ELSE
  
C       Patch up first word
  
        FORN(0) = CSMG(FORN(0),
     +                 SHIFTR(CRAY(0), BITOFF),
     +                 MASK(BITOFF))
  
        DO 820 I = 1, NUM-1
          FORN(I) = OR(SHIFTL(CRAY(IIN),     64-BITOFF),
     +                 SHIFTR(CRAY(IIN+JNC), BITOFF))
          IIN     = IIN + JNC
  820   CONTINUE
  
C       Patch up last word
  
        FORN(NUM) = CSMG(FORN(NUM),
     +                   SHIFTL(CRAY(IIN), 64-BITOFF),
     +                   MASK(64+BITOFF)) ! mask from right
      ENDIF
  
C     GOTO 900

  900 CONTINUE
      CRAY2NVE = 0     ! we don't yet return status
      RETURN

  999 CONTINUE
      CRAY2NVE = -1    ! some error in parameters
      RETURN
  
CDIR$ ID "@(#) libu/cdc/cray2nve.f	92.0	10/08/98 14:57:41"
      END
