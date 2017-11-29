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
     +CRAY2IEG(TYPE, NUM, FORN, BITOFF, CRAY, STRIDE, CRAYCH)

      IMPLICIT NONE

C     The CRAY2IEG function converts CRAY Fortran data types to a
C     generic 32-bit platform with IEEE floating-point representation
C     data types.
C
C     The returned function value is as follows:
C
C         <0  Parameter error; no translation performed
C          0  Translation complete; no errors
C         >0  Translation complete; return value is the number of
C             values that completely overflowed or completely under-
C             flowed.

      INTEGER TYPE

C         Type code:
C           0 = typeless (no translation)
C           1 = integer (64-bit two's-complement to 32-bit two's-
C               complement)
C           2 = real (64-bit single-precision CRAY floating-point
C               to 32-bit single-precision IEEE floating-point)
C           3 = double (128-bit double-precision CRAY floating-point
C               to 64-bit double-precision IEEE floating-point)
C           4 = complex (2 x 64-bit single-precision CRAY floating-
C               point to 2 x 32-bit single-precision IEEE floating-
C               point).
C           5 = logical (64-bit CRAY logical to 32-bit generic
C               logical; all nonzero CRAY values are converted to a
C               value of 1, all zero CRAY values remain unchanged).
C           6 = character (ASCII to ASCII; no translation)
C           7 = short integer (32-bit two's-complement to 16-bit
C               two's-complement)
C           8 = special (64-bit single-precision CRAY floating-point
C               to 64-bit double-precision IEEE floating-point)

      INTEGER NUM

C         Number of data items to convert.  Type integer variable,
C         expression, or constant.

      INTEGER FORN(0:0)

C         Variable or array of any type (except CHARACTER) or length
C         to receive the converted data.

      INTEGER BITOFF

C         Bit offset within FORN to begin placing the converted
C         data.  Type integer variable, expression, or constant.
C         Bits are numbered from 0 to 63, beginning at the leftmost
C         bit of FORN.

      INTEGER CRAY(0:0)

C         Variable or array containing the values to be converted.
C         This variable should be of a type corresponding to the
C         TYPE parameter.

      INTEGER STRIDE

C         Memory increment for loading the CRAY items to be converted.
C         The bits are placed in FORN regardless of this parameter in
C         a continuous bit stream (no striding is done on output).
C         For two-word items, this is a stride of items, NOT of words.
C         Optional parameter of type integer variable, expression, or
C         constant.  Default value is 1.

      CHARACTER * (*) CRAYCH(0:0)

C         Optional parameter specifying CRAY character variable if it
C         is of type CHARACTER (TYPE = 6).

      INTEGER           IVL
      PARAMETER        (IVL = 1024)

C     Vector Functions
C     These all must be declared integer for the vector loops to work

      INTEGER           GI32O
      INTEGER           IEF32O
      INTEGER           IED64O
      INTEGER           IEC64O
      INTEGER           GL32O
      INTEGER           GI16O     ! really integer*2

C     VFUNCTION overflow/underflow counts

      INTEGER           GEN32OF, DENORM, IEEEOFL

      INTEGER           NSIZ
      INTEGER           ISIZE(0:8)
      INTEGER           ALSTUF
      INTEGER           PADVAL

      TASK COMMON  /T@GENERIC/ GEN32OF
      TASK COMMON  /T@IEEE/    DENORM, IEEEOFL
      COMMON       /G@IEG@SZ/  NSIZ, ISIZE

      DATA NSIZ /9/
      DATA ISIZE /64, 32, 32, 64, 64, 32, 8, 16, 64/

      COMMON       /G@IEG@AL/  ALSTUF, PADVAL
      DATA ALSTUF, PADVAL /0, 0/         ! IEEE/generic does no alignment

CDIR$ VFUNCTION GI32O
CDIR$ VFUNCTION IEF32O
CDIR$ VFUNCTION IED64O
CDIR$ VFUNCTION IEC64O
CDIR$ VFUNCTION GL32O
CDIR$ VFUNCTION GI16O

      EXTERNAL MOVBITZ

      REAL             CRAYR(0:0)
      DOUBLE PRECISION CRAYDBL(0:0)
      COMPLEX          CRAYCPL(0:0)

      POINTER (RPTR, CRAYR)
      POINTER (DPTR, CRAYDBL)
      POINTER (CPTR, CRAYCPL)

      INTEGER AND, BO, CHROFF, CHRLEN, F, I, IIN, INC, J, K
      INTEGER LEFT, N, O1, O2, SHIFTL, SHIFTR, SZ, TEMP(IVL)
      INTEGER IDUMMY, IWVAR
      POINTER (IWADD,IWVAR)

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
  
      O1 = 0                            !  Initial overflow count
      O2 = 0                            !  Final overflow count

      IF (NUM .EQ. 0) GOTO 1000

      IIN  = 0
      RPTR = LOC(CRAY(0))
      DPTR = LOC(CRAY(0))
      CPTR = LOC(CRAY(0))

      GOTO (100,200,300,400,500,600,700,800,900),TYPE+1

C     TYPE = 0  (typeless)                                  ************

  100 CONTINUE
      SZ = 64

      IF (INC .EQ. 1) THEN

        CALL MOVBITZ(CRAY, 0, NUM*SZ, FORN, BITOFF)

      ELSE

        IF (BITOFF .EQ. 0) THEN

          DO I = 0, NUM-1
            FORN(I) = CRAY(IIN)
            IIN     = IIN + INC
          ENDDO

        ELSE

        FORN(0) = CSMG(FORN(0),
     +                 SHIFTR(CRAY(IIN), BITOFF),
     +                 MASK(BITOFF))

        DO I = 1, NUM-1
          FORN(I) = OR(SHIFTL(CRAY(IIN    ), 64-BITOFF),
     +                 SHIFTR(CRAY(IIN+INC),    BITOFF))
          IIN     = IIN + INC
        ENDDO

        FORN(NUM) = CSMG(FORN(NUM),
     +                   SHIFTL(CRAY(IIN), 64-BITOFF),
     +                   MASK(64+BITOFF))
  
        ENDIF

      ENDIF
      GOTO 1000

C     TYPE = 1  (integer)                                   ************

  200 CONTINUE
      O1 = GEN32OF
      SZ = 32
      N  = NUM

      IF (AND(N, 1) .NE. 0) THEN    !  Do odd element if extant

        N = N - 1
        F = GI32O(CRAY(N * INC))

        CALL MOVBITZ(F, 32, SZ, FORN(N/2), BITOFF)

        IF (N .EQ. 0) GOTO 290      !  If there was only one number

      ENDIF

      N = N / 2

C     Main loop:  one output word for each two input words

      IF (BITOFF .EQ. 0) THEN

        DO I = 0, N-1
          FORN(I) = OR(SHIFTL(GI32O(CRAY(IIN    )), SZ),
     +                        GI32O(CRAY(IIN+INC)))
          IIN     = IIN + (2 * INC)
        ENDDO

      ELSE

        J    = 0
        LEFT = MOD(N, IVL)
        IF (LEFT .EQ. 0) LEFT = IVL

C       Strip mine the array

  220   CONTINUE
        DO K = 1, LEFT
          TEMP(K) = OR(SHIFTL(GI32O(CRAY(IIN    )), SZ),
     +                        GI32O(CRAY(IIN+INC)))
          IIN     = IIN + (2 * INC)
        ENDDO

        CALL MOVBITZ(TEMP, 0, LEFT*SZ*2, FORN(J), BITOFF)

        J    = J + LEFT
        N    = N - LEFT
        LEFT = IVL
        IF (N .GT. 0) GOTO 220

      ENDIF

  290 CONTINUE

CDIR$ SUPPRESS GEN32OF
      O2 = GEN32OF
      GOTO 1000

C     TYPE = 2  (single-precision)                          ************

  300 CONTINUE
      O1 = IEEEOFL
      SZ = 32
      N  = NUM

      IF (AND(N, 1) .NE. 0) THEN    !  Do odd element if extant

        N = N - 1
        F = IEF32O(CRAYR(N * INC))

        CALL MOVBITZ(F, 32, SZ, FORN(N/2), BITOFF)

        IF (N .EQ. 0) GOTO 390      !  If there was only one number

      ENDIF

      N = N / 2

C     Main loop:  one output word for each two input words

      IF (BITOFF .EQ. 0) THEN

        DO I = 0, N-1
          FORN(I) = IEC64O(CMPLX(CRAYR(IIN), CRAYR(IIN+INC)))
          IIN     = IIN + (2 * INC)
        ENDDO

      ELSE

        J    = 0
        LEFT = MOD(N, IVL)
        IF (LEFT .EQ. 0) LEFT = IVL

C       Strip mine the array

  320   CONTINUE
        DO K = 1, LEFT
          TEMP(K) = IEC64O(CMPLX(CRAYR(IIN), CRAYR(IIN+INC)))
          IIN     = IIN + (2 * INC)
        ENDDO

        CALL MOVBITZ(TEMP, 0, LEFT*SZ*2, FORN(J), BITOFF)

        J    = J + LEFT
        N    = N - LEFT
        LEFT = IVL
        IF (N .GT. 0) GOTO 320

      ENDIF

  390 CONTINUE

CDIR$ SUPPRESS IEEEOFL
      O2 = IEEEOFL
      GOTO 1000

C     TYPE = 3  (double-precision)                          ************

  400 CONTINUE
      O1 = IEEEOFL
      SZ = 64

      IF (BITOFF .EQ. 0) THEN

        DO I = 0, NUM-1
          FORN(I) = IED64O(CRAYDBL(IIN))
          IIN     = IIN + INC
        ENDDO

      ELSE

        J    = 0
        N    = NUM
        LEFT = MOD(N, IVL)
        IF (LEFT .EQ. 0) LEFT = IVL

C       Strip mine the array

  420   CONTINUE
        DO K = 1, LEFT
          TEMP(K) = IED64O(CRAYDBL(IIN))
          IIN     = IIN + INC
        ENDDO

        CALL MOVBITZ(TEMP, 0, LEFT*SZ, FORN(J), BITOFF)

        J    = J + LEFT
        N    = N - LEFT
        LEFT = IVL
        IF (N .GT. 0) GOTO 420

      ENDIF

CDIR$ SUPPRESS IEEEOFL
      O2 = IEEEOFL
      GOTO 1000

C     TYPE = 4  (complex)                                   ************

  500 CONTINUE
      O1 = IEEEOFL
      SZ = 64

      IF (BITOFF .EQ. 0) THEN

        DO I = 0, NUM-1
          FORN(I) = IEC64O(CRAYCPL(IIN))
          IIN     = IIN + INC
        ENDDO

      ELSE

        J    = 0
        N    = NUM
        LEFT = MOD(N, IVL)
        IF (LEFT .EQ. 0) LEFT = IVL

C       Strip mine the array

  520   CONTINUE
        DO K = 1, LEFT
          TEMP(K) = IEC64O(CRAYCPL(IIN))
          IIN     = IIN + INC
        ENDDO

        CALL MOVBITZ(TEMP, 0, LEFT*SZ, FORN(J), BITOFF)

        J    = J + LEFT
        N    = N - LEFT
        LEFT = IVL
        IF (N .GT. 0) GOTO 520

      ENDIF

CDIR$ SUPPRESS IEEEOFL
      O2 = IEEEOFL
      GOTO 1000

C     TYPE = 5  (logical)                                   ************

  600 CONTINUE
      SZ = 32
      N  = NUM

      IF (AND(N, 1) .NE. 0) THEN    !  Do odd element if extant

        N = N - 1
        F = GL32O(CRAY(N * INC))

        CALL MOVBITZ(F, 32, SZ, FORN(N/2), BITOFF)

        IF( N .EQ. 0) GOTO 1000     !  If there was only one number

      ENDIF

      N = N / 2

C     Main loop:  one output word for each two input words

      IF (BITOFF .EQ. 0) THEN

        DO I = 0, N-1
          FORN(I) = OR(SHIFTL(GL32O(CRAY(IIN    )), SZ),
     +                        GL32O(CRAY(IIN+INC)))
          IIN     = IIN + (2 * INC)
        ENDDO

      ELSE

        J    = 0
        LEFT = MOD(N, IVL)
        IF (LEFT .EQ. 0) LEFT = IVL

C       Strip mine the array

  620   CONTINUE
        DO K = 1, LEFT
          TEMP(K) = OR(SHIFTL(GL32O(CRAY(IIN    )), SZ),
     +                        GL32O(CRAY(IIN+INC)))
          IIN     = IIN + (2 * INC)
        ENDDO

        CALL MOVBITZ(TEMP, 0, LEFT*SZ*2, FORN(J), BITOFF)

        J    = J + LEFT
        N    = N - LEFT
        LEFT = IVL
        IF (N .GT. 0) GOTO 620

      ENDIF
      GOTO 1000

C     TYPE = 6  (character)                                 ************

  700 CONTINUE
      BO     = BITOFF                          !  Bit offset
      SZ     = 8                               !  Bits per character
      CHRLEN = SZ * LEN(CRAYCH(0))             !  Length of each element

      IF (INC .EQ. 1) THEN

        CALL G@CHRPCK(CRAYCH,IWADD,IDUMMY,CHROFF) ! get bit offset

        CALL MOVBITZ(IWVAR, CHROFF, NUM*CHRLEN, FORN, BO)

      ELSE

        J = 0

        DO I = 0, NUM-1

          CALL G@CHRPCK(CRAYCH(IIN),IWADD,IDUMMY,CHROFF) ! get bit offset

          CALL MOVBITZ(IWVAR, CHROFF, CHRLEN, FORN(J), BO)

          IIN    = IIN + INC
          BO     = BO + CHRLEN

          IF (BO .GE. 64) THEN
            J    = J + (BO / 64)
            BO   = MOD(BO, 64)
          ENDIF

        ENDDO

      ENDIF
      GOTO 1000

C     TYPE = 7  (short integer)                             ************

  800 CONTINUE
      O1 = GEN32OF
      SZ = 16
      N  = NUM
      J  = AND(NUM, 3)

      IF (J .NE. 0) THEN    !  Do remainder elements if extant
 
  810   CONTINUE
        J  = J - 1
        N  = N - 1
        K  = N / 4
        F  = GI16O(CRAY(N * INC))
        BO = BITOFF + (J * SZ)

        IF (BO .GE. 64) THEN
          BO = BO - 64
          K  = K + 1
        ENDIF

        CALL MOVBITZ(F, 64-16, SZ, FORN(K), BO)

        IF (N .EQ. 0) GOTO 890    !  If all done

        IF (J .GT. 0) GOTO 810

      ENDIF

      N = N / 4

C     Main loop:  one output word for each four input words

      IF (BITOFF .EQ. 0) THEN

        DO I = 0, N-1
          FORN(I) = OR(OR(SHIFTL(GI16O(CRAY(IIN      )), 48),
     +                    SHIFTL(GI16O(CRAY(IIN+INC  )), 32)),
     +                 OR(SHIFTL(GI16O(CRAY(IIN+INC*2)), 16),
     +                           GI16O(CRAY(IIN+INC*3))))
          IIN     = IIN + (4 * INC)
        ENDDO

      ELSE

        J    = 0
        LEFT = MOD(N, IVL)
        IF (LEFT .EQ. 0) LEFT = IVL

C       Strip mine the array

  830   CONTINUE
        DO K = 1, LEFT
          TEMP(K) = OR(OR(SHIFTL(GI16O(CRAY(IIN      )), 48),
     +                    SHIFTL(GI16O(CRAY(IIN+INC  )), 32)),
     +                 OR(SHIFTL(GI16O(CRAY(IIN+INC*2)), 16),
     +                           GI16O(CRAY(IIN+INC*3))))
          IIN     = IIN + (4 * INC)
        ENDDO

        CALL MOVBITZ(TEMP, 0, LEFT*SZ*4, FORN(J), BITOFF)

        J    = J + LEFT
        N    = N - LEFT
        LEFT = IVL
        IF (N .GT. 0) GOTO 830

      ENDIF

  890 CONTINUE

CDIR$ SUPPRESS GEN32OF
      O2 = GEN32OF
      GOTO 1000

C     TYPE = 8  (single-precision to double-precision)      ************

  900 CONTINUE
      O1 = IEEEOFL

      IF (BITOFF .EQ. 0) THEN

        DO I = 0, NUM-1
          FORN(I) = IED64O(DBLE(CRAYR(IIN)))
          IIN     = IIN + INC
        ENDDO

      ELSE

        J    = 0
        SZ   = 64
        N    = NUM
        LEFT = MOD(N, IVL)
        IF (LEFT .EQ. 0) LEFT = IVL

C       Strip mine the array

  920   CONTINUE
        DO K = 1, LEFT
          TEMP(K) = IED64O(DBLE(CRAYR(IIN)))
          IIN     = IIN + INC
        ENDDO

        CALL MOVBITZ(TEMP, 0, LEFT*SZ, FORN(J), BITOFF)

        J    = J + LEFT
        N    = N - LEFT
        LEFT = IVL
        IF (N .GT. 0) GOTO 920

      ENDIF

CDIR$ SUPPRESS IEEEOFL
      O2 = IEEEOFL
C     GOTO 1000

 1000 CONTINUE
      CRAY2IEG = O2 - O1        !  Return error count
      RETURN

 9000 CONTINUE
      CRAY2IEG = -1             !  Some parameter error
      RETURN

CDIR$ ID "@(#) libu/ieg/cray2ieg.f	92.0	10/08/98 14:57:41"
      END
