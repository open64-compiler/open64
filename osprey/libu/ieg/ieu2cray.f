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
     +IEU2CRAY(TYPE, NUM, FORN, BITOFF, CRAY, STRIDE, CRAYCH)
  
      IMPLICIT NONE
  
C     The IEU2CRAY subroutine converts Fortran data types from Ultrix
C     32-bit little-endian format to CRAY Fortran data types.
C
C     The returned function value is as follows:
C
C         <0  Parameter error; no translation performed
C          0  Translation complete; no errors
  
      INTEGER TYPE
  
C         Type code:
C           0 = typeless (no translation)
C           1 = integer (32-bit two's-complement to 64-bit two's-
C               complement).
C           2 = real (32-bit single-precision IEEE floating-point
C               to 64-bit single-precision CRAY floating-point).
C           3 = double (64-bit double-precision IEEE floating-point
C               to 128-bit double-precision CRAY floating-point).
C           4 = complex (2 x 32-bit single-precision IEEE floating-
C               point to 2 x 64-bit single-precision CRAY floating-
C               point).
C           5 = logical (32-bit generic logical to 64-bit CRAY
C               logical; all nonzero values are converted to CRAY
C               logical trues and all zero values are converted to
C               CRAY logical falses).
C           6 = character (ASCII to ASCII; no translation)
C           7 = short integer (16-bit two's-complement to 32-bit
C               two's-complement).
C           8 = special (64-bit double-precision IEEE floating-point
C               to 64-bit single-precision CRAY floating-point).
  
      INTEGER NUM
  
C         Number of data items to convert.  Type integer variable,
C         expression, or constant.
  
      INTEGER FORN(0:0)
  
C         Variable or array of any type (except CHARACTER) or length
C         containing the data to be converted.
  
      INTEGER BITOFF
  
C         Bit offset within FORN to begin the conversion.  Type
C         integer variable, expression, or constant.  Bits are
C         numbered from 0 to 63, beginning at the leftmost bit of
C         FORN.
  
      INTEGER CRAY(0:0)
  
C         Variable or array to receive the converted values.  This
C         variable should be of a type corresponding to the TYPE
C         parameter.
  
      INTEGER STRIDE
  
C         Memory increment for storing the conversion results in
C         CRAY.  The input bits are taken from FORN regardless of
C         this parameter in a continuous bit stream.  For two-word
C         items, this is a stride of items, NOT of words.  Optional
C         parameter of type integer variable, expression, or
C         constant.  Default value is 1.
  
      CHARACTER * (*) CRAYCH(0:0)
  
C         Optional parameter specifying CRAY target variable if it
C         is of type CHARACTER (TYPE = 6).
  
      INTEGER          IVL
      PARAMETER       (IVL = 1024)
  
      REAL             CRAYR(0:0)
      DOUBLE PRECISION CRAYDBL(0:0)
      COMPLEX          CRAYCPL(0:0)

      POINTER (RPTR, CRAYR)
      POINTER (DPTR, CRAYDBL)
      POINTER (CPTR, CRAYCPL)
  
C     Vector Functions
  
      INTEGER           GLI32I
      REAL              IUF32I
      DOUBLE PRECISION  IUD64I
      COMPLEX           IUC64I
      INTEGER           GLL32I
      INTEGER           GLI16I       ! Really INTEGER*2
      REAL              SNGLR
  
CDIR$ VFUNCTION GLI32I
CDIR$ VFUNCTION IUF32I
CDIR$ VFUNCTION IUD64I
CDIR$ VFUNCTION IUC64I
CDIR$ VFUNCTION GLL32I
CDIR$ VFUNCTION GLI16I
CDIR$ VFUNCTION SNGLR
  
      EXTERNAL MOVBITZ
  
      INTEGER AND, BO, CHROFF, CHRLEN, FTMP, I, INC, IOUT, J
      INTEGER K, LEFT, N, OR, SHIFTL, SHIFTR, SZ, TEMP(IVL)
      INTEGER IDUMMY, IWVAR
      POINTER (IWADD,IWVAR)
      INTEGER VTEMP1, VTEMP2
  
CDIR$ EJECT
  
      IF (   NUM .LT.  0) GOTO 9000  !  CALL UTERP(034)
      IF (BITOFF .GE. 64) GOTO 9000  !  CALL UTERP(034)

      IF (NUMARG() .EQ. 5) THEN
        INC = 1  
        ELSE IF (NUMARG() .GE. 6) THEN
          INC = STRIDE
          ELSE  
            GOTO 9000  !  CALL UTERP(034)
      ENDIF 
  
      IF ((TYPE .LT. 0) .OR. (TYPE .GT. 8)) GOTO 9000  !  CALL UTERP(034)
  
      IF (TYPE .EQ. 6 .AND. NUMARG() .LT. 7)
     +   GOTO 9000  !  CALL UTERP(034)  !  need CRAYCH to do characters
  
      IF (NUM .EQ. 0) GOTO 1000

      IOUT = 0
      RPTR = LOC(CRAY(0))
      DPTR = LOC(CRAY(0))
      CPTR = LOC(CRAY(0))
  
C     Process type of data
  
      GOTO (100,200,300,400,500,600,700,800,900),TYPE+1
  
C     TYPE = 0  (typeless)                                  ************
  
  100 CONTINUE
      IF (INC .EQ. 1) THEN
        CALL MOVBITZ(FORN, BITOFF, NUM*64, CRAY, 0)
      ELSE
  
        IF (BITOFF .EQ. 0) THEN
          DO 110 I = 0, NUM-1
            CRAY(IOUT) = FORN(I)
            IOUT       = IOUT + INC
  110     CONTINUE
        ELSE
          DO 120 I = 0, NUM-1
            CRAY(IOUT) = OR(SHIFTL(FORN(I  ),    BITOFF),
     +                      SHIFTR(FORN(I+1), 64-BITOFF))
            IOUT       = IOUT + INC
  120     CONTINUE
        ENDIF
  
      ENDIF
      GOTO 1000
  
C     TYPE = 1  (integer)                                   ************
  
  200 CONTINUE
  
      N = NUM / 2

      IF (N .EQ. 0) GOTO 230

C     Main loop: Two output words for each input word

C     WARNING: The following loops are structured to avoid problems
C     due to potential overlap in the FORN and CRAY arrays.
C     VTEMP2 is passed to the first VFUNCTION to encourage the compiler
C     to compute VTEMP2 before storing CRAY.  Do not change this loop
C     without considering this overlap

      IF (BITOFF .EQ. 0) THEN
  
        DO 205 I = 0, N-1
          VTEMP1 = SHIFTR(FORN(I), 32)
          VTEMP2 = FORN(I)
          CRAY(IOUT    ) = GLI32I(VTEMP1,VTEMP2)
          CRAY(IOUT+INC) = GLI32I(VTEMP2)
          IOUT           = IOUT + (2 * INC)
  205   CONTINUE

      ELSE IF (BITOFF .LT. 32) THEN
        DO 210 I = 0, N-1
          VTEMP1         =    SHIFTR(FORN(I), 32-BITOFF)
          VTEMP2         = OR(SHIFTL(FORN(I),    BITOFF),
     +                        SHIFTR(FORN(I+1), 64-BITOFF))
          CRAY(IOUT)     = GLI32I(VTEMP1,VTEMP2)
          CRAY(IOUT+INC) = GLI32I(VTEMP2)
          IOUT           = IOUT + (2 * INC)
  210   CONTINUE
      ELSE
        DO 220 I = 0, N-1
          VTEMP1 = OR(SHIFTL(FORN(  I), BITOFF-32),
     +                              SHIFTR(FORN(I+1), 96-BITOFF))
          VTEMP2 =    SHIFTR(FORN(I+1), 64-BITOFF)
          CRAY(IOUT)     = GLI32I(VTEMP1,VTEMP2)
          CRAY(IOUT+INC) = GLI32I(VTEMP2)
          IOUT           = IOUT + (2 * INC)
  220   CONTINUE
      ENDIF

  230 CONTINUE
      IF (AND(NUM, 1) .NE. 0) THEN      !  Do odd element if extant
  
        N = NUM - 1
        J = N / 2
  
        IF (BITOFF .LE. 32) THEN
          CRAY(IOUT) = GLI32I(   SHIFTR(FORN(  J), 32-BITOFF))
        ELSE
          CRAY(IOUT) = GLI32I(OR(SHIFTL(FORN(  J), BITOFF-32),
     +                             SHIFTR(FORN(J+1), 96-BITOFF)))
        ENDIF
  
      ENDIF
  
      GOTO 1000
  
C     TYPE = 2  (single-precision)                          ************
  
  300 CONTINUE
      N = NUM / 2
  
      IF (N .EQ. 0) GOTO 360       !  If there was only one number
  
C     Main loop: two output words for each input word
  
C     WARNING: The following loops are structured to avoid problems
C     due to potential overlap in the FORN and CRAY arrays.
C     VTEMP2 is passed to the first VFUNCTION to encourage the compiler
C     to compute VTEMP2 before storing CRAY.  Do not change this loop
C     without considering this overlap

      IF (BITOFF .EQ. 0) THEN
  
        DO 310 I = 0, N-1
          VTEMP1 = SHIFTR(FORN(I), 32)
          VTEMP2 =        FORN(I)
          CRAYR(IOUT    ) = IUF32I(VTEMP1,VTEMP2)
          CRAYR(IOUT+INC) = IUF32I(VTEMP2)
          IOUT            = IOUT + (2 * INC)
  310   CONTINUE
  
      ELSE IF (BITOFF .LE. 32) THEN
        DO 320 I = 0, N-1
          VTEMP1 =    SHIFTR(FORN(  I), 32-BITOFF)
          VTEMP2 = OR(SHIFTL(FORN(  I),    BITOFF),
     +                              SHIFTR(FORN(I+1), 64-BITOFF))
          CRAYR(IOUT)     = IUF32I(VTEMP1,VTEMP2)
          CRAYR(IOUT+INC) = IUF32I(VTEMP2)
          IOUT           = IOUT + (2 * INC)
  320   CONTINUE
      ELSE
        DO 330 I = 0, N-1
          VTEMP1 = OR(SHIFTL(FORN(  I), BITOFF-32),
     +                              SHIFTR(FORN(I+1), 96-BITOFF))
          VTEMP2 =    SHIFTR(FORN(I+1), 64-BITOFF)
          CRAYR(IOUT)     = IUF32I(VTEMP1,VTEMP2)
          CRAYR(IOUT+INC) = IUF32I(VTEMP2)
          IOUT           = IOUT + (2 * INC)
  330   CONTINUE
      ENDIF

  360 CONTINUE
      IF (AND(NUM, 1) .NE. 0) THEN      !  Do odd element if extant
  
        N = NUM - 1
        J = N / 2

        IF (BITOFF .LE. 32) THEN
          CRAYR(IOUT) = IUF32I(   SHIFTR(FORN(  J), 32-BITOFF))
        ELSE
          CRAYR(IOUT) = IUF32I(OR(SHIFTL(FORN(  J), BITOFF-32),
     +                             SHIFTR(FORN(J+1), 96-BITOFF)))
        ENDIF
  
      ENDIF
  
      GOTO 1000
  
C     TYPE = 3  (double-precision)                          ************
  
  400 CONTINUE
  
      IF (BITOFF .EQ. 0) THEN
  
        DO 410 I = 0, NUM-1
          CRAYDBL(IOUT) = IUD64I(FORN(I))
          IOUT          = IOUT + INC
  410   CONTINUE
  
      ELSE
  
        DO 420 I = 0, NUM-1
  
          CRAYDBL(IOUT) = IUD64I(OR(SHIFTL(FORN(I  ),    BITOFF),
     +                              SHIFTR(FORN(I+1), 64-BITOFF)))
          IOUT          = IOUT + INC
  420   CONTINUE
  
      ENDIF
      GOTO 1000
  
C     TYPE = 4  (complex)                                   ************
  
  500 CONTINUE
  
      IF (BITOFF .EQ. 0) THEN
  
        DO 510 I = 0, NUM-1
          CRAYCPL(IOUT) = IUC64I(FORN(I))
          IOUT          = IOUT + INC
  510   CONTINUE
  
      ELSE
  
        DO 520 I = 0, NUM-1
          CRAYCPL(IOUT) = IUC64I(OR(SHIFTL(FORN(I  ),    BITOFF),
     +                              SHIFTR(FORN(I+1), 64-BITOFF)))
          IOUT          = IOUT + INC
  520   CONTINUE
  
      ENDIF
      GOTO 1000
  
C     TYPE = 5  (logical)                                   ************
  
  600 CONTINUE
      N = NUM / 2
  
      IF (N .EQ. 0) GOTO 630

C     Main loop: Two output words for each input word
  
C     WARNING: The following loops are structured to avoid problems
C     due to potential overlap in the FORN and CRAY arrays.
C     VTEMP2 is passed to the first VFUNCTION to encourage the compiler
C     to compute VTEMP2 before storing CRAY.  Do not change this loop
C     without considering this overlap

      IF (BITOFF .EQ. 0) THEN
  
        DO 605 I = 0, N-1
          VTEMP1 = SHIFTR(FORN(I), 32)
          VTEMP2 =        FORN(I)
          CRAY(IOUT    ) = GLL32I(VTEMP1,VTEMP2)
          CRAY(IOUT+INC) = GLL32I(VTEMP2)
          IOUT           = IOUT + (2 * INC)
  605   CONTINUE

      ELSE IF (BITOFF .LT. 32) THEN
  
        DO 610 I = 0, N-1
          VTEMP1 =    SHIFTR(FORN(  I), 32-BITOFF)
          VTEMP2 = OR(SHIFTL(FORN(  I),    BITOFF),
     +                SHIFTR(FORN(I+1), 64-BITOFF))
          CRAY(IOUT)     = GLL32I(VTEMP1,VTEMP2)
          CRAY(IOUT+INC) = GLL32I(VTEMP2)
          IOUT           = IOUT + (2 * INC)
  610   CONTINUE
  
      ELSE
  
        DO 620 I = 0, N-1
          VTEMP1 = OR(SHIFTL(FORN(  I), BITOFF-32),
     +                SHIFTR(FORN(I+1), 96-BITOFF))
          VTEMP2 =    SHIFTR(FORN(I+1), 64-BITOFF)
          CRAY(IOUT)     = GLL32I(VTEMP1,VTEMP2)
          CRAY(IOUT+INC) = GLL32I(VTEMP2)
          IOUT           = IOUT + (2 * INC)
  620   CONTINUE
  
      ENDIF

  630 CONTINUE
      IF (AND(NUM, 1) .NE. 0) THEN      !  Do odd element if extant
  
        N = NUM - 1
        J = N / 2
  
        IF (BITOFF .LE. 32) THEN
          CRAY(IOUT) = GLL32I(   SHIFTR(FORN(  J), 32-BITOFF))
        ELSE
          CRAY(IOUT) = GLL32I(OR(SHIFTL(FORN(  J), BITOFF-32),
     +                             SHIFTR(FORN(J+1), 96-BITOFF)))
        ENDIF
  
      ENDIF
  
      GOTO 1000
  
C     TYPE = 6  (character)                                 ************
  
  700 CONTINUE
      BO     = BITOFF
      SZ     = 8                               !  Bits per character
      CHRLEN = SZ * LEN(CRAYCH(0))             !  Length of each element
      IF (INC .EQ. 1) THEN
        CALL G@CHRPCK(CRAYCH,IWADD,IDUMMY,CHROFF)
        CALL MOVBITZ(FORN, BO, NUM*CHRLEN, IWVAR, CHROFF)
      ELSE
  
        J = 0
  
        DO 710 I = 0, NUM-1

          CALL G@CHRPCK(CRAYCH(IOUT),IWADD,IDUMMY,CHROFF)

          CALL MOVBITZ(FORN(J), BO, CHRLEN, IWVAR, CHROFF)
  
          IOUT   = IOUT + INC
          BO     = BO + CHRLEN
  
          IF (BO .GE. 64) THEN
            J    = J + (BO / 64)
            BO   = MOD(BO, 64)
          ENDIF
  
  710   CONTINUE
  
      ENDIF
      GOTO 1000
  
C     TYPE = 7  (short integer)                             ************
  
  800 CONTINUE
      SZ = 16
      N = NUM / 4

      IF (N .EQ. 0) GOTO 850 
C     Main loop: Four output words for each input word
  
      IF (BITOFF .EQ. 0) THEN
  
        DO 820 I = 0, N-1
          VTEMP1 = FORN(I)
          CRAY(IOUT)         = GLI16I(SHIFTR(VTEMP1, 48))
          CRAY(IOUT+INC)     = GLI16I(SHIFTR(VTEMP1, 32))
          CRAY(IOUT+(2*INC)) = GLI16I(SHIFTR(VTEMP1, 16))
          CRAY(IOUT+(3*INC)) = GLI16I(       VTEMP1     )
          IOUT               = IOUT + (4 * INC)
  820   CONTINUE
  
      ELSE
  
        J    = 0
        LEFT = MOD(N, IVL)
        IF (LEFT .EQ. 0) LEFT = IVL
  
C       Strip mine the array
  
  830   CONTINUE
  
        CALL MOVBITZ(FORN(J), BITOFF, LEFT*SZ*4, TEMP, 0)
  
        DO 840 K = 1, LEFT
          CRAY(IOUT)         = GLI16I(SHIFTR(TEMP(K), 48))
          CRAY(IOUT+INC)     = GLI16I(SHIFTR(TEMP(K), 32))
          CRAY(IOUT+(2*INC)) = GLI16I(SHIFTR(TEMP(K), 16))
          CRAY(IOUT+(3*INC)) = GLI16I(       TEMP(K)     )
          IOUT               = IOUT + (4 * INC)
  840   CONTINUE
  
        J    = J + LEFT
        N    = N - LEFT
        LEFT = IVL
        IF (N .GT. 0) GOTO 830
  
      ENDIF

  850   CONTINUE
      IF (AND(NUM,3) .NE. 0) THEN      !  Do odd elements if extant
        J = 0
	N = NUM - AND(NUM,3)
  860   CONTINUE
        K = N/4
        BO = BITOFF + (J * SZ)
  
        IF (BO .GE. 64) THEN
          BO = BO - 64
          K  = K + 1
        ENDIF
  
        CALL MOVBITZ(FORN(K), BO, SZ, FTMP, 64-SZ)
  
        CRAY(IOUT) = GLI16I(FTMP)
        IOUT       = IOUT + INC

        J = J + 1
        N = N + 1
  
        IF (N .LT. NUM) GOTO 860   !  If not done
  
      ENDIF
  
      GOTO 1000
  
C     TYPE = 8  (double-precision to single-precision)      ************
  
  900 CONTINUE
  
      IF (BITOFF .EQ. 0) THEN
  
        DO 910 I = 0, NUM-1
          CRAYR(IOUT) = SNGLR(IUD64I(FORN(I)))
          IOUT        = IOUT + INC
  910   CONTINUE
  
      ELSE
  
        DO 920 I = 0, NUM-1
          CRAYR(IOUT) = SNGLR(IUD64I(OR(SHIFTL(FORN(I  ),    BITOFF),
     +                                 SHIFTR(FORN(I+1), 64-BITOFF))))
          IOUT        = IOUT + INC
  920   CONTINUE
  
      ENDIF
C     GOTO 1000
  
 1000 CONTINUE
      IEU2CRAY = 0                 !  All done; no errors
      RETURN
  
 9000 CONTINUE
      IEU2CRAY = -1                !  Some parameter error
      RETURN
  
CDIR$ ID "@(#) libu/ieg/ieu2cray.f	92.0	10/08/98 14:57:41"
      END
