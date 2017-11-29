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
     +ETA2CRAY(TYPE, NUM, FORN, BITOFF, CRAY, STRD, CRAYCH)
  
      IMPLICIT NONE
  
C
C     The ETA2CRAY subroutine converts ETA/C205 numbers to CRAY format
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
C                                 !       (used for half-precision real)
C                                 !
      INTEGER NUM                 ! Number of foreign data items to
C                                 ! convert.  Type integer variable,
C                                 ! expression, or constant.
C                                 !
      INTEGER FORN(0:0)           ! Variable or array of any type or
C                                 ! length containing ETA/C205 format
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
  
      INTEGER AND
      INTEGER OR
      INTEGER XOR
      INTEGER IADD
      INTEGER CHRLEN, CHROFF, I, IOUT, J, JNC, MYSIZE, BO
      INTEGER IWVAR, IDUMMY
      POINTER (IWADD,IWVAR)
      INTEGER II,JJ
      INTEGER FN1
      INTEGER FN2
      INTEGER DUMMY
      INTEGER COEMSK
      INTEGER SPEMSK
      INTEGER SPSMSK
      INTEGER SGNBIT
      INTEGER SBP
      INTEGER SBIT
      INTEGER EBIT
      INTEGER LOCOEF
      INTEGER HICOEF

      INTEGER HPEMSK
      INTEGER HPESMSK
      INTEGER HPSMSK
      INTEGER HPESIGN
      INTEGER HPSGNBIT

      INTEGER HINIB
      INTEGER HI70
      INTEGER HI80
      INTEGER HPHINIB
      INTEGER HPHI70
      INTEGER HPHI80

      INTEGER CRBIAS
      INTEGER HPCRBIAS
      INTEGER CRAYIND
      INTEGER CRPOW2

      INTEGER MASK32

      INTEGER EF64I
      INTEGER EF32I
      INTEGER SPSGNEXT
      INTEGER COECVT
      INTEGER EXPCVT

      INTEGER COECVT2
      INTEGER EXPCVT2

      INTEGER ETAT                       ! ETA .TRUE.

      EXTERNAL MOVBITZ

      PARAMETER (MASK32 = X'FFFFFFFF')
      PARAMETER (ETAT = 1)

C     Single precision masks

      PARAMETER (COEMSK = X'0000FFFFFFFFFFFF')    ! Coef. mask
      PARAMETER (SPEMSK = X'FFFF000000000000')    ! Exp. mask
      PARAMETER (SPSMSK = X'FFFF800000000000')    ! sign ext. mask
      PARAMETER (SGNBIT = X'0000800000000000')    ! sign bit. mask

      PARAMETER (HINIB    = X'F000000000000000')
      PARAMETER (HI80     = X'8000000000000000')    ! machine zero
      PARAMETER (HI70     = X'7000000000000000')    ! indef

C     Half precision masks

      PARAMETER (HPEMSK   = X'00000000FF000000')    ! Exp. mask
      PARAMETER (HPESMSK  = X'FFFFFFFF80000000')    ! Exp. sgn ext mask
      PARAMETER (HPSMSK   = X'FFFFFFFFFF800000')    ! sign ext. mask
      PARAMETER (HPESIGN  = X'0000000080000000')    ! sign bit. mask
      PARAMETER (HPSGNBIT = X'0000000000800000')    ! sign bit. mask

      PARAMETER (HPHINIB  = X'00000000F0000000')
      PARAMETER (HPHI80   = X'0000000080000000')    ! machine zero
      PARAMETER (HPHI70   = X'0000000070000000')    ! indef

C     CRAY masks

      PARAMETER (CRAYIND  = X'6000800000000000')    ! indef

      PARAMETER (CRBIAS   = X'402F000000000000') ! CRAY bias
      PARAMETER (HPCRBIAS = X'4017000000000000') ! Half Prec CRAY bias

      PARAMETER (CRPOW2   = X'0001800000000000') ! magic constant to fix
                                                 ! numbers that are a power
                                                 ! of 2

C     Statement Functions

      IADD(II,JJ) = II + JJ                      ! integer add

C     SPSGNEXT 'smears' the sign bit in the C205/ETA word over the
C     high 16 bits of the result word, sign extending the low 48.
C     If the number is positive, it is left alone.

      SPSGNEXT(I) =
     +           CVMGN(
     +             XOR(AND(I,COEMSK),SPSMSK) + SGNBIT,  ! if negative
     +             I,                                   ! if positive
     +             AND(I,SGNBIT)
     +             )

C     Convert single prec. exponent

      EXPCVT(I) = AND(I,SPEMSK)+CRBIAS

C     Convert single prec. Coefficient

      COECVT(I,SBP) = 
     +             CVMGZ(
     +               AND(SHIFTL(+I,1),COEMSK), ! If sign zero
     +               CVMGZ(                    ! If sign non-zero
     +                 CRPOW2,                 ! magic constant
     +                 AND(SHIFTL(-I,1),COEMSK),
     +                 AND(I,COMPL(SPSMSK))
     +                 ),
     +               SBP                       ! ETA Sign bit
     +               )

C     EF64I Converts a C205/ETA real to the CRAY format

      EF64I(I) = CVMGZ(
     +             0,                             ! If high nibble is 0x8
     +             CVMGZ(
     +               CRAYIND,                     ! If high nibble is 0x7
     +               IADD(                        ! makes real number
     +                 OR(
     +                   EXPCVT(I),               ! Convert exp
     +                   SHIFTL(AND(I,SGNBIT),16) ! Sign bit
     +                   ),
     +                 COECVT(I,AND(I,SGNBIT))    ! Convert coef.
     +                 ),
     +               XOR(AND(I,HINIB),HI70)
     +               ),
     +             XOR(AND(I,HINIB),HI80)
     +             )

C     Convert half prec. exponent

      EXPCVT2(I) = SHIFTL(               ! Align left for CRAY
     +               XOR(                ! XOR extend part
     +                 AND(I,HPEMSK),    ! mask off junk
     +                 HPESMSK
     +                 )
     +               + HPESIGN,          ! Add sign bit back in
     +               24
     +               )
     +               + HPCRBIAS          ! Add bias

C     Convert half prec. Coefficient

      COECVT2(I) = 
     +             CVMGZ(
     +               AND(SHIFTL(+I,25),COEMSK), ! If zero
     +               CVMGZ(                     ! If sign non-zero
     +                 CRPOW2,                  ! magic constant
     +                 AND(SHIFTL(-I,25),COEMSK),
     +                 AND(I,COMPL(HPSMSK))
     +                 ),
     +               AND(I,HPSGNBIT)            ! ETA Sign bit
     +               )

C     EF32I Converts a C205/ETA half prec. real to the CRAY format

      EF32I(I) = CVMGZ(
     +             0,                             ! If high nibble is 0x8
     +             CVMGZ(
     +               CRAYIND,                     ! If high nibble is 0x7
     +               IADD(
     +                 OR(
     +                   EXPCVT2(I),               ! Convert exp
     +                   SHIFTL(AND(I,HPSGNBIT),40) ! Sign bit
     +                   ),
     +                 COECVT2(I)                  ! Convert coef.
     +                 ),
     +               XOR(AND(I,HPHINIB),HPHI70)
     +               ),
     +             XOR(AND(I,HPHINIB),HPHI80)
     +             )
  
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
  
  100 CONTINUE
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
  
C     TYPE = 1  (integer)                                   ************

  200 CONTINUE
      IF (BITOFF .EQ. 0) THEN
        DO 210 I = 0, NUM-1
          CRAY(IOUT) = SPSGNEXT(FORN(I))
          IOUT       = IOUT + JNC
  210   CONTINUE
      ELSE
        DO 220 I = 0, NUM-1
          CRAY(IOUT) = SPSGNEXT(
     +                    OR(SHIFTL(FORN(I  ), BITOFF),
     +                       SHIFTR(FORN(I+1), 64-BITOFF))
     +                    )
          IOUT       = IOUT + JNC
  220   CONTINUE
      ENDIF

      GOTO 900
  
C     TYPE = 2  (real)                                      ************
  
  300 CONTINUE
      IF (BITOFF .EQ. 0) THEN
        DO 310 I = 0, NUM-1
          CRAY(IOUT) = EF64I(FORN(I))
          IOUT       = IOUT + JNC
  310   CONTINUE
      ELSE
        DO 320 I = 0, NUM-1
          CRAY(IOUT) = EF64I(OR(SHIFTL(FORN(I  ), BITOFF),
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
          SBIT   = AND(FORN(J),SGNBIT)
          HICOEF =
     +             CVMGZ(
     +               AND(SHIFTL(      FORN(J) ,1),COEMSK),
     +               AND(SHIFTL(COMPL(FORN(J)),1),COEMSK),
     +               SBIT
     +               )
          LOCOEF =
     +             CVMGZ(
     +               AND( FORN(J+1),COEMSK),
     +               AND(-FORN(J+1),COEMSK), ! Low word sign must be 0!
     +               SBIT
     +               )

          EBIT   = AND(SHIFTR(LOCOEF,46),1)

C         Do carry if LOCOEF is zero and converting negative number

          HICOEF =
     +             CVMGN(
     +               CVMGZ(
     +                 OR(HICOEF + 2,EBIT),
     +                 OR(HICOEF    ,EBIT),
     +                 LOCOEF
     +                 ),
     +               OR(HICOEF    ,EBIT),
     +               SBIT
     +               )

C         If Hi Coefficient is zero, set it to magic constant instead to
C         fix normalization problems.  Note that if the coeffs are both
C         zero in the low 48 bits, there will be a stray carry bit
C         up in the exponent field that must be eliminated.

          HICOEF =
     +             CVMGZ(
     +               CRPOW2,                    ! Magic constant
     +               HICOEF,
     +               AND(OR(HICOEF,LOCOEF),COEMSK)
     +               )

          CRAY(IOUT)   = 
     +                 CVMGZ(
     +                   0,                     ! If high nibble is 0x8
     +                   CVMGZ(
     +                     CRAYIND,             ! If high nibble is 0x7
     +                     IADD(                ! makes real number
     +                       OR(
     +                         EXPCVT(FORN(J)),
     +                         SHIFTL(SBIT,16)  ! Sign bit
     +                         ),
     +                       HICOEF
     +                       ),
     +                     XOR(AND(FORN(J),HINIB),HI70)
     +                     ),
     +                   XOR(AND(FORN(J),HINIB),HI80)
     +                   )
          CRAY(IOUT+1) = AND(SHIFTL(LOCOEF,2),COEMSK)

          IOUT         = IOUT + (2*JNC)
          J            = J + 2
  410   CONTINUE
      ELSE
        DO 420 I = 0, NUM-1
          FN1 = OR(SHIFTL(FORN(J  ), BITOFF),
     +             SHIFTR(FORN(J+1), 64-BITOFF))
          FN2 = OR(SHIFTL(FORN(J+1), BITOFF),
     +             SHIFTR(FORN(J+2), 64-BITOFF))

          SBIT   = AND(FN1,SGNBIT)
          HICOEF =
     +             CVMGZ(
     +               AND(SHIFTL(      FN1 ,1),COEMSK),
     +               AND(SHIFTL(COMPL(FN1),1),COEMSK),
     +               SBIT
     +               )
          LOCOEF =
     +             CVMGZ(
     +               AND( FN2,COEMSK),
     +               AND(-FN2,COEMSK), ! Low word sign must be 0!
     +               SBIT
     +               )

          EBIT   = AND(SHIFTR(LOCOEF,46),1)

C         Do carry if LOCOEF is zero and converting negative number

          HICOEF =
     +             CVMGN(
     +               CVMGZ(
     +                 OR(HICOEF + 2,EBIT),
     +                 OR(HICOEF    ,EBIT),
     +                 LOCOEF
     +                 ),
     +               OR(HICOEF    ,EBIT),
     +               SBIT
     +               )

C         If Hi Coefficient is zero, set it to magic constant instead to
C         fix normalization problems.

          HICOEF =
     +             CVMGZ(
     +               CRPOW2,                    ! Magic constant
     +               HICOEF,
     +               HICOEF+LOCOEF
     +               )

          CRAY(IOUT)   = 
     +                 CVMGZ(
     +                   0,                     ! If high nibble is 0x8
     +                   CVMGZ(
     +                     CRAYIND,             ! If high nibble is 0x7
     +                     IADD(                  ! real number
     +                       OR(
     +                         EXPCVT(FN1),
     +                         SHIFTL(SBIT,16)  ! Sign bit
     +                         ),
     +                       HICOEF
     +                       ),
     +                     XOR(AND(FN1,HINIB),HI70)
     +                     ),
     +                   XOR(AND(FN1,HINIB),HI80)
     +                   )
          CRAY(IOUT+1) = AND(SHIFTL(LOCOEF,2),COEMSK)

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
          CRAY(IOUT  ) = EF64I(FORN(J  ))
          CRAY(IOUT+1) = EF64I(FORN(J+1))
          IOUT         = IOUT + (2*JNC)
          J            = J + 2
  510   CONTINUE
      ELSE
        DO 520 I = 0, NUM-1
          CRAY(IOUT  ) = EF64I(OR(SHIFTL(FORN(J  ), BITOFF),
     +                            SHIFTR(FORN(J+1), 64-BITOFF)))
          CRAY(IOUT+1) = EF64I(OR(SHIFTL(FORN(J+1), BITOFF),
     +                            SHIFTR(FORN(J+2), 64-BITOFF)))
          IOUT         = IOUT + (2*JNC)
          J            = J + 2
  520   CONTINUE
      ENDIF
      GOTO 900
  
C     TYPE = 5  (logical)                                   ************
  
  600 CONTINUE
      IF (BITOFF .EQ. 0) THEN
        DO 610 I = 0, NUM-1
          CRAYL(IOUT) = (FORN(I) .EQ. ETAT)
          IOUT        = IOUT + JNC
  610   CONTINUE
      ELSE
        DO 620 I = 0, NUM-1
          CRAYL(IOUT) = (OR(SHIFTL(FORN(I  ), BITOFF),
     +                      SHIFTR(FORN(I+1), 64-BITOFF)) .EQ. ETAT)
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
  
C     TYPE = 7  (Half-precision)                             ************
  
  800 CONTINUE
C
C Special case the handling of the first element, as the main loop
C works on pairs.  Adjust the initial values accordingly
C
      BO = BITOFF
      J = 0
      IOUT = 0
      IF (AND(NUM,1).NE.0) THEN
        IF (BO.LT.32) THEN
          CRAY(0) = EF32I(AND(SHIFTR(FORN(J),32-BO),MASK32))
          BO = BO+32
        ELSE ! bitoff >= 32
          CRAY(0) = EF32I(AND(OR(SHIFTL(FORN(J),BO-32),
     +                            SHIFTR(FORN(J+1),96-BO)),
     +                            MASK32))
          BO = BO-32
          J = J + 1
        ENDIF
        IOUT = IOUT+JNC
      ENDIF
C
C Main loops: Two outputs for each input 64bit word
C
      I = J
      IF (BO.LT.32) THEN
        DO 810 DUMMY=J,NUM-1,2
          CRAY(IOUT)     = EF32I(AND(SHIFTR(FORN(I),32-BO),
     +                         MASK32))
          CRAY(IOUT+JNC) = EF32I(AND(OR(SHIFTL(FORN(I),BO),
     +                                   SHIFTR(FORN(I+1),64-BO)),
     +                                   MASK32))
          IOUT = IOUT + JNC*2
          I = I + 1
  810   CONTINUE
      ELSE
        DO 820 DUMMY=J,NUM-1,2
          CRAY(IOUT)     = EF32I(AND(OR(SHIFTL(FORN(I),BO-32),
     +                              SHIFTR(FORN(I+1),96-BO)),
     +                              MASK32))
          CRAY(IOUT+JNC) = EF32I(AND(SHIFTR(FORN(I+1),64-BO),
     +                         MASK32))
          IOUT = IOUT + JNC*2
          I = I + 1
  820   CONTINUE
      ENDIF
C     GOTO 900

  900 CONTINUE
      ETA2CRAY = 0     ! we don't yet return status
      RETURN

  999 CONTINUE
      ETA2CRAY = -1    ! some parameter error
      RETURN
  
CDIR$ ID "@(#) libu/cdc/eta2cray.f	92.0	10/08/98 14:57:41"
      END
