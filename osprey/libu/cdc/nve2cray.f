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
     +NVE2CRAY(TYPE, NUM, FORN, BITOFF, CRAY, STRD, CRAYCH)
  
      IMPLICIT NONE
  
C
C     The NVE2CRAY subroutine converts NOS/VE numbers to CRAY format
C
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
      INTEGER NUM                 ! Number of foreign data items to
C                                 ! convert.  Type integer variable,
C                                 ! expression, or constant.
C                                 !
      INTEGER FORN(0:0)           ! Variable or array of any type or
C                                 ! length containing NOS/VE format
C                                 ! numbers to convert.
C                                 !
      INTEGER BITOFF              ! Bit number within FORN to begin the
C                                 ! conversion.  Type integer variable,
C                                 ! expression, or constant.  Bits are
C                                 ! numbered from 0, beginning at the
C                                 ! leftmost bit of FORN.
C                                 !
      INTEGER CRAY(0:0)           ! Variable or array to contain the
C                                 ! converted values.
C                                 !
      INTEGER STRD                ! Memory increment for storing the
C                                 ! conversion results in CRAY.  The
C                                 ! input bits are taken from FORN
C                                 ! regardless of this parameter in a
C                                 ! continuous bit stream.  For two-
C                                 ! word items, this is a stride of
C                                 ! items, NOT of words.  Optional
C                                 ! parameter of type integer variable,
C                                 ! expression, or constant.  Default
C                                 ! value is 1.
C                                 !
      CHARACTER * (*) CRAYCH(0:0) ! Optional parameter specifying CRAY
C                                 ! target variable if it is of type
C                                 ! CHARACTER.
  
      REAL             CRAYR(0:0)
      DOUBLE PRECISION CRAYDBL(0:0)
      COMPLEX          CRAYCPL(0:0)
      LOGICAL          CRAYL(0:0)
C     INTEGER * 2      CRAYS(0:0)        ! Short integer
  
      POINTER (RPTR, CRAYR)
      POINTER (DPTR, CRAYDBL)
      POINTER (CPTR, CRAYCPL)
      POINTER (LPTR, CRAYL)
C     POINTER (SPTR, CRAYS)              ! Short integer
  
C     Vector Functions
  
      INTEGER           NF64I
  
      INTEGER AND, CHRLEN, CHROFF, I, IOUT, J, JNC, MYSIZE, OR
      INTEGER IWVAR, IDUMMY, NVETRUE
      POINTER(IWADD,IWVAR)
      EXTERNAL MOVBITZ
  
C     Statement Function

      NF64I(I) = I
  
CDIR$ EJECT
  
      IF (   NUM .LT.  0) GOTO 999 ! CALL UTERP(034)
      IF (BITOFF .GE. 64) GOTO 999 ! CALL UTERP(034)
  
      IF (NUMARG() .EQ. 5) THEN
        JNC = 1  
      ELSE IF (NUMARG() .GE. 6) THEN
        JNC = STRD
      ELSE  
        GOTO 999 ! CALL UTERP(034)
      ENDIF 
  
      IF ((TYPE .LT. 0) .OR. (TYPE .GT. 7)) GOTO 999 ! CALL UTERP(034)
  
      IF (TYPE .EQ. 6 .AND. NUMARG() .LT. 7)
     +  GOTO 999 ! CALL UTERP(034) ! need CRAYCH to do characters
  
      IF (NUM .EQ. 0) RETURN
  
      IOUT = 0
      RPTR = LOC(CRAY(0))
      DPTR = LOC(CRAY(0))
      CPTR = LOC(CRAY(0))
      LPTR = LOC(CRAY(0))
C     SPTR = LOC(CRAY(0))                ! Short integer
  
      GOTO (100,200,300,400,500,600,700,800),TYPE+1
  
C     TYPE = 0  (typeless)                                  ************
C     TYPE = 1  (integer)                                   ************
  
  100 CONTINUE
  200 CONTINUE
      IF (JNC .EQ. 1) THEN
        CALL MOVBITZ(FORN, BITOFF, NUM*64, CRAY, 0)
      ELSE
  
        IF (BITOFF .EQ. 0) THEN
          DO 110 I = 0, NUM-1
            CRAY(IOUT) = FORN(I)
            IOUT       = IOUT + JNC
  110     CONTINUE
        ELSE
          DO 120 I = 0, NUM-1
            CRAY(IOUT) = OR(SHIFTL(FORN(I  ), BITOFF),
     +                      SHIFTR(FORN(I+1), 64-BITOFF))
            IOUT       = IOUT + JNC
  120     CONTINUE
        ENDIF
  
      ENDIF
      GOTO 900
  
C     TYPE = 2  (real)                                      ************
  
  300 CONTINUE
      IF (BITOFF .EQ. 0) THEN
        DO 310 I = 0, NUM-1
          CRAY(IOUT) = NF64I(FORN(I))
          IOUT       = IOUT + JNC
  310   CONTINUE
      ELSE
        DO 320 I = 0, NUM-1
          CRAY(IOUT) = NF64I(OR(SHIFTL(FORN(I  ), BITOFF),
     +                          SHIFTR(FORN(I+1), 64-BITOFF)))
          IOUT       = IOUT + JNC
  320   CONTINUE
      ENDIF
      GOTO 900
  
C     TYPE = 3  (double precision)                          ************
  
  400 CONTINUE
      J = 0
      IF (BITOFF .EQ. 0) THEN
        DO 410 I = 0, NUM-1
          CRAY(IOUT  ) = NF64I(FORN(J))
          CRAY(IOUT+1) = AND(MASK(128-48), FORN(J+1))
          IOUT         = IOUT + (2*JNC)
          J            = J + 2
  410   CONTINUE
      ELSE
        DO 420 I = 0, NUM-1
          CRAY(IOUT  ) = NF64I(OR(SHIFTL(FORN(J  ), BITOFF),
     +                            SHIFTR(FORN(J+1), 64-BITOFF)))
          CRAY(IOUT+1) = AND(MASK(128-48),
     +                         OR(SHIFTL(FORN(J+1), BITOFF),
     +                            SHIFTR(FORN(J+2), 64-BITOFF)))
          IOUT         = IOUT + (2*JNC)
          J            = J + 2
  420   CONTINUE
      ENDIF
      GOTO 900
  
C     TYPE = 4  (complex)                                   ************
  
  500 CONTINUE
      J = 0
      IF (BITOFF .EQ. 0) THEN
        DO 510 I = 0, NUM-1
          CRAY(IOUT  ) = NF64I(FORN(J  ))
          CRAY(IOUT+1) = NF64I(FORN(J+1))
          IOUT         = IOUT + (2*JNC)
          J            = J + 2
  510   CONTINUE
      ELSE
        DO 520 I = 0, NUM-1
          CRAY(IOUT  ) = NF64I(OR(SHIFTL(FORN(J  ), BITOFF),
     +                            SHIFTR(FORN(J+1), 64-BITOFF)))
          CRAY(IOUT+1) = NF64I(OR(SHIFTL(FORN(J+1), BITOFF),
     +                            SHIFTR(FORN(J+2), 64-BITOFF)))
          IOUT         = IOUT + (2*JNC)
          J            = J + 2
  520   CONTINUE
      ENDIF
      GOTO 900
  
C     TYPE = 5  (logical)                                   ************
  
  600 CONTINUE
      NVETRUE = MASK(64)                        ! NOS/VE .TRUE. value
      IF (BITOFF .EQ. 0) THEN
        DO 610 I = 0, NUM-1
          CRAYL(IOUT) = (FORN(I) .EQ. NVETRUE)
          IOUT        = IOUT + JNC
  610   CONTINUE
      ELSE
        DO 620 I = 0, NUM-1
          CRAYL(IOUT) = (OR(SHIFTL(FORN(I  ), BITOFF),
     +                      SHIFTR(FORN(I+1), 64-BITOFF)) .EQ. NVETRUE)
          IOUT        = IOUT + JNC
  620   CONTINUE
      ENDIF
      GOTO 900
  
C     TYPE = 6  (character)                                 ************
  
  700 CONTINUE
      MYSIZE = 8                                ! Bits per character
      CHRLEN = MYSIZE * LEN(CRAYCH(0))          ! Length of each element
      IF (JNC .EQ. 1) THEN
        CALL G@CHRPCK(CRAYCH,IWADD,IDUMMY,CHROFF)
        CALL MOVBITZ(FORN, BITOFF, NUM*CHRLEN, IWVAR, CHROFF)
      ELSE
  
        J = 0
  
        DO 710 I = 0, NUM-1
          CALL G@CHRPCK(CRAYCH(IOUT),IWADD,IDUMMY,CHROFF)
          CALL MOVBITZ(FORN(J), BITOFF, CHRLEN, IWVAR, CHROFF)
          BITOFF = BITOFF + CHRLEN
          IF (BITOFF .GE. 64) THEN
            J      = J + (BITOFF / 64)
            BITOFF = MOD (BITOFF, 64)
          ENDIF
          IOUT   = IOUT + JNC
  710   CONTINUE
  
      ENDIF
      GOTO 900
  
C     TYPE = 7  (short integer)                             ************
  
  800 CONTINUE
      IF (BITOFF .EQ. 0) THEN
        DO 810 I = 0, NUM-1
          CRAY(IOUT) = AND(MASK(64+32), FORN(I))
          IOUT       = IOUT + JNC
  810   CONTINUE
      ELSE
        DO 820 I = 0, NUM-1
          CRAY(IOUT) = AND(MASK(64+32),
     +                     OR(SHIFTL(FORN(I  ), BITOFF),
     +                        SHIFTR(FORN(I+1), 64-BITOFF)))
          IOUT       = IOUT + JNC
  820   CONTINUE
      ENDIF
C     GOTO 900

  900 CONTINUE
      NVE2CRAY = 0     ! we don't yet return status
      RETURN

  999 CONTINUE
      NVE2CRAY = -1    ! some parameter error
      RETURN
  
CDIR$ ID "@(#) libu/cdc/nve2cray.f	92.0	10/08/98 14:57:41"
      END
