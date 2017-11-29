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
     +CRAY2ETA(TYPE, NUM, FORN, BITOFF, CRAY, STRD, CRAYCH)

      IMPLICIT NONE

C
C     The CRAY2ETA subroutine converts CRAY numbers to ETA/C205 format.
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
C                                 !       (used for half-precision!)
C                                 !
      INTEGER NUM                 ! Number of data items to convert.
C                                 ! Type integer variable, expression,
C                                 ! or constant.
C                                 !
      INTEGER FORN(0:0)           ! Variable or array of any type or
C                                 ! length to receive the converted
C                                 ! ETA/C205 numbers.
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

      INTEGER SPECVT
      INTEGER SPCCVT
      INTEGER HPECVT
      INTEGER HPCCVT
      INTEGER EI64O
      INTEGER EF64O
      INTEGER EF32O
      INTEGER SGNBIT
      INTEGER DPEOFF

      REAL             CRAYR(0:0)
      DOUBLE PRECISION CRAYDBL(0:0)
      COMPLEX          CRAYCPL(0:0)
      LOGICAL          CRAYL(0:0)

      POINTER (RPTR, CRAYR)
      POINTER (DPTR, CRAYDBL)
      POINTER (CPTR, CRAYCPL)
      POINTER (LPTR, CRAYL)

      INTEGER CHRLEN, CHROFF
      INTEGER IWVAR,IDUMMY
      POINTER (IWADD,IWVAR)
      INTEGER AND, OR
      INTEGER I, II, IIN, ISIZE(0:8), J, JNC, K, LEFT
      INTEGER MYSIZE, NT, NF, TEMP(0:(2*IVL)-1)
      INTEGER SBP
      INTEGER EBIT
      INTEGER SBIT
      INTEGER HICOEF
      INTEGER LOCOEF
      INTEGER ISTART, FULLW, BO
      INTEGER NSIZ
      INTEGER ALSTUF
      INTEGER PADVAL

      EXTERNAL         MOVBITZ

      COMMON /G@ETA@SZ/ NSIZ,ISIZE
      DATA NSIZ  /8/
      DATA ISIZE /64, 64, 64, 128, 128, 64, 8, 32/

      COMMON /G@ETA@AL/ ALSTUF,PADVAL
      DATA ALSTUF,PADVAL  /0,0/             ! ETA does no alignment

      PARAMETER (SGNBIT=X'8000000000000000')
      PARAMETER (DPEOFF=X'002F000000000000')

      EI64O(I) = AND(I,X'0000FFFFFFFFFFFF') ! mask off upper 16 bits

C     Convert Single precision exponent value. Note: X'402F' = O'40057'

      SPECVT(I) = AND(I,X'7FFF000000000000') - X'402F000000000000'

C     Convert Single precision coeff.

      SPCCVT(I,SBP) = CVMGZ(
     +                  AND(+SHIFTR(I,1),X'00007FFFFFFFFFFF'),
     +                  AND(-SHIFTR(I,1),X'00007FFFFFFFFFFF'),
     +                  SBP             ! Sign bit
     +                  )

C     Convert Single precision numbers

      EF64O(I) =
     +         CVMGN(
     +           OR(
     +             OR(
     +               SPECVT(I),
     +               SHIFTR(
     +                 AND(I,SGNBIT), ! Sign bit
     +                 16)                         ! Shift to SP place
     +               ),
     +             SPCCVT(I,AND(I,SGNBIT))
     +             ),
     +           X'8F81000000000000',
     +           I
     +           )

C     Convert Half precision exponent value.

      HPECVT(I) = AND(
     +              SHIFTR(
     +                AND(I,X'7FFF000000000000') - X'4017000000000000',
     +                24
     +                ),
     +              X'FF000000'
     +              )

C     Convert Half precision coeff.

      HPCCVT(I,SBP) = CVMGZ(
     +              AND(+SHIFTR(I,25),X'00000000007FFFFF'),
     +              AND(-SHIFTR(I,25),X'00000000007FFFFF'),
     +              SBP                           ! Sign bit
     +              )
C     Convert Half precision numbers

      EF32O(I) =
     +         CVMGN(
     +           OR(
     +             OR(
     +               HPECVT(I),
     +               SHIFTR(
     +                 AND(I,SGNBIT), ! Sign bit
     +                 40)                         ! Shift to HP place
     +               ),
     +             HPCCVT(I,AND(I,SGNBIT))
     +             ),
     +           X'80000000',
     +           I
     +           )

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

C     Process type of data

      GOTO (100,200,300,400,500,600,700,800),TYPE+1

C     Type = 0  (typeless)                                  ************

  100 CONTINUE
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

C     Type = 1  (integer)                                   ************

  200 CONTINUE

      IF (BITOFF .EQ. 0) THEN
        DO 210 I = 0, NUM-1
          FORN(I) = EI64O(CRAY(IIN))
          IIN     = IIN + JNC
  210   CONTINUE
      ELSE

        FORN(0) = CSMG(FORN(0),
     +                 SHIFTR(CRAY(IIN), BITOFF),
     +                 MASK(BITOFF))

        DO 220 I = 1, NUM-1
          FORN(I) = OR(SHIFTL(EI64O(CRAY(IIN    )), 64-BITOFF),
     +                 SHIFTR(EI64O(CRAY(IIN+JNC)), BITOFF))
          IIN     = IIN + JNC
  220   CONTINUE

        FORN(NUM) = CSMG(FORN(NUM),
     +                   SHIFTL(EI64O(CRAY(IIN)), 64-BITOFF),
     +                   MASK(64+BITOFF))
      ENDIF

      GOTO 900

C     Type = 2  (real)                                      ************

  300 CONTINUE
      IF (BITOFF .EQ. 0) THEN
        DO 310 I = 0, NUM-1
          FORN(I) = EF64O(CRAY(IIN))
          IIN     = IIN + JNC
  310   CONTINUE

      ELSE

        FORN(0) = CSMG(FORN(0),
     +                 SHIFTR(EF64O(CRAY(0)), BITOFF),
     +                 MASK(BITOFF))

        DO 320 I = 1, NUM-1
          FORN(I) = OR(SHIFTL(EF64O(CRAY(IIN)),     64-BITOFF),
     +                 SHIFTR(EF64O(CRAY(IIN+JNC)), BITOFF))
          IIN     = IIN + JNC
  320   CONTINUE
      ENDIF

        FORN(NUM) = CSMG(FORN(NUM),
     +                 SHIFTL(EF64O(CRAY(IIN)), 64-BITOFF),
     +                 MASK(64+BITOFF))
      GOTO 900

C     Type = 3  (double precision)                          ************

  400 CONTINUE
      IF (BITOFF .EQ. 0) THEN
        J = 0
        DO 410 I = 0, NUM-1
          SBIT   = AND(CRAY(IIN),SGNBIT)        !Sign bit of input
          EBIT   = SHIFTL(AND(CRAY(IIN),1),46)
          HICOEF =
     +           CVMGZ(
     +             AND(      SHIFTR(CRAY(IIN),1) ,X'00007FFFFFFFFFFF'),
     +             AND(COMPL(SHIFTR(CRAY(IIN),1)),X'00007FFFFFFFFFFF'),
     +             SBIT
     +             )
          LOCOEF = OR(SHIFTR(CRAY(IIN+1),2),EBIT)
          LOCOEF =
     +             CVMGZ(
     +               AND(    LOCOEF,X'0000FFFFFFFFFFFF'),
     +               AND(   -LOCOEF,X'0000FFFFFFFFFFFF'),
     +               SBIT
     +               )

C         If low coefficient is zero, and number is negative,
C         propogate carry.

          HICOEF = CVMGZ(
     +               CVMGN(
     +                 HICOEF+1,
     +                 HICOEF,
     +                 SBIT
     +                 ),
     +               HICOEF,
     +               LOCOEF
     +               )
          FORN(J) =
     +         CVMGN(
     +           OR(
     +             OR(
     +               SPECVT(CRAY(IIN)),
     +               SHIFTR(
     +                 SBIT,
     +                 16)                         ! Shift to SP place
     +               ),
     +             HICOEF
     +             ),
     +           X'8F8F000000000000',
     +           CRAY(IIN)
     +           )
          FORN(J+1) =
     +          CVMGN(
     +            OR(
     +              SPECVT(CRAY(IIN))-DPEOFF,
     +              LOCOEF
     +              ),
     +            X'8F80000000000000',
     +            CRAY(IIN)
     +            )
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
          SBIT   = AND(CRAY(IIN),SGNBIT)        !Sign bit of input
          EBIT   = SHIFTL(AND(CRAY(IIN),1),46)
          HICOEF =
     +           CVMGZ(
     +             AND(      SHIFTR(CRAY(IIN),1) ,X'00007FFFFFFFFFFF'),
     +             AND(COMPL(SHIFTR(CRAY(IIN),1)),X'00007FFFFFFFFFFF'),
     +             SBIT
     +             )
          LOCOEF = OR(SHIFTR(CRAY(IIN+1),2),EBIT)
          LOCOEF =
     +             CVMGZ(
     +               AND(    LOCOEF,X'0000FFFFFFFFFFFF'),
     +               AND(   -LOCOEF,X'0000FFFFFFFFFFFF'),
     +               SBIT
     +               )

C         If low coefficient is zero, and number is negative,
C         propogate carry.

          HICOEF = CVMGZ(
     +               CVMGN(
     +                 HICOEF+1,
     +                 HICOEF,
     +                 SBIT
     +                 ),
     +               HICOEF,
     +               LOCOEF
     +               )
          TEMP(J) =
     +         CVMGN(
     +           OR(
     +             OR(
     +               SPECVT(CRAY(IIN)),
     +               SHIFTR(
     +                 SBIT,
     +                 16)                         ! Shift to SP place
     +               ),
     +             HICOEF
     +             ),
     +           X'8F8F000000000000',
     +           CRAY(IIN)
     +           )
          TEMP(J+1) =
     +          CVMGN(
     +            OR(
     +              SPECVT(CRAY(IIN))-DPEOFF,
     +              LOCOEF
     +              ),
     +            X'8F80000000000000',
     +            CRAY(IIN)
     +            )
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
          FORN(J)   = EF64O(CRAY(IIN  ))
          FORN(J+1) = EF64O(CRAY(IIN+1))
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
          TEMP(J)   = EF64O(CRAY(IIN  ))
          TEMP(J+1) = EF64O(CRAY(IIN+1))
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
      NT   = 1             ! ETA/C205 .TRUE.
      NF   = 0             ! ETA/C205 .FALSE.
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

C     Determine how many full words there are to process.

      IF ((NUM*MYSIZE - AND((64 - BITOFF),77B)) .LT.0) THEN
        FULLW = 0
      ELSE
        FULLW = (NUM*MYSIZE - AND((64 - BITOFF),77B))/64
      ENDIF

C
C       Pretend that there is no bit offset, we will fix that later
C
      ISTART = 0
      IF(BITOFF.NE.0) ISTART = 1
      DO 810 I=ISTART,FULLW-1+ISTART
        FORN(I) = OR(SHIFTL(EF32O(CRAY(IIN    )),32),
     +                      EF32O(CRAY(IIN+JNC)))
        IIN = IIN + JNC*2
  810 CONTINUE

      IF(BITOFF .NE. 0) THEN

C       If the bit offset is > 0, must shift result up, and fix up tail
C       First fix up first word

        FORN(0) = CSMG(FORN(0),
     +                 SHIFTR(FORN(1),BITOFF),
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
        J = EF32O(CRAY(IIN))
        CALL MOVBITZ(J,32,MYSIZE,
     +               FORN(FULLW+SHIFTR(BO,6)),AND(BO,77B))
        BO = BO + MYSIZE
        IIN = IIN + JNC
  830 CONTINUE

C     GOTO 900

  900 CONTINUE
      CRAY2ETA = 0     ! we don't yet return status
      RETURN

  999 CONTINUE
      CRAY2ETA = -1    ! some error in parameters
      RETURN

CDIR$ ID "@(#) libu/cdc/cray2eta.f	92.0	10/08/98 14:57:41"
      END
