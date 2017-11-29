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

      INTEGER FUNCTION MIPS2CRY(
     +TYPE, NUM, FORN, BITOFF, NATIVE, STRIDE, NATLEN, FORLEN, NATIVECH)

      IMPLICIT NONE

C     This function converts MIPS IEEE Fortran data types to CRAY Fortran
C     data types.
C
C     The returned function value is as follows:
C
C         <0  Parameter error; no translation performed
C             -1      Parameter error; too few arguments or NATIVECH
C                     not specified with TYPE = 6 (CRAY only).
C             -2      Parameter error; invalid TYPE
C             -3      Parameter error; invalid NUM
C             -4      Parameter error; invalid BITOFF
C             -5      Parameter error; invalid NATLEN
C             -6      Parameter error; invalid FORLEN
C             -7      Unable to malloc() memory for translation
C             -8      Combination of NATLEN and FORLEN is invalid
C             -9      NATIVE must be 64-bit word-aligned (MPP only)
C             -10     FORN must be 64-bit word-aligned (MPP only)
C          0  Translation complete; no errors
C         >0  Translation complete; return value is the number of
C             values that overflowed during translation.

      INTEGER TYPE
      INTENT (IN) TYPE

C         Type code:
C           1 = typeless (no translation); NATLEN must equal FORLEN
C               and be 64, 128 or 256.
C           2 = integer (8-, 16-, 32- or 64-bit twos-complement to 64-
C               bit twos-complement)
C           3 = real (32-, 64- or 128-bit IEEE floating-point to 64-
C               or 128-bit CRAY floating-point)
C           4 = complex (2 x 32-, 64- or 128-bit IEEE floating-point
C               to 2 x 64- or 128-bit CRAY floating-point).
C           5 = logical (8-, 16-, 32- or 64-bit zero/nonzero logical to
C               64-bit positive/negative logical).
C           6 = character (ASCII to ASCII; no translation)
C
C         The NATLEN and FORLEN parameters (see below) select the size
C         of the data.

      INTEGER NUM
      INTENT (IN) NUM

C         Number of data items to convert.  Type integer variable,
C         expression, or constant.

      INTEGER (KIND=1) FORN(0:*)
      INTENT (IN) FORN

C         Variable or array of any type (except CHARACTER) or length
C         that contains (or will contain) the foreign data.

      INTEGER BITOFF
      INTENT (IN) BITOFF

C         Bit offset within FORN to begin placing the converted
C         data.  Type integer variable, expression, or constant.
C         Bits are numbered from 0 to 63, beginning at the leftmost
C         bit of FORN.

      INTEGER (KIND=1) NATIVE(0:*)
      INTENT (OUT) NATIVE

C         Variable or array that contains (or will contain) the
C         native data.  This variable should be of a type that
C         corresponds to the TYPE parameter.  If TYPE = 6
C         (CHARACTER), this should be a dummy INTEGER variable and
C         the optional NATIVECH parameter should be a CHARACTER
C         variable or array that contains (or will contain) the
C         the native data.

      INTEGER STRIDE
      INTENT (IN) STRIDE

C         Memory increment the NATIVE items to be converted.  For
C         two- or four-word items, this is a stride of items, NOT
C         of words.

      INTEGER NATLEN
      INTENT (IN) NATLEN

C         Internal (NATIVE) storage length, in bits.

      INTEGER FORLEN
      INTENT (IN) FORLEN

C         External (FORN) storage length, in bits.

      CHARACTER * (*) NATIVECH(0:*)
      INTENT (OUT) NATIVECH
      OPTIONAL NATIVECH

C         Optional parameter specifying NATIVE character variable if
C         it is of type CHARACTER (TYPE = 6).  This parameter is
C         ignored if TYPE is not 6 (CHARACTER).

C
C     Constants
C
      INTEGER           GRANSZ
#ifdef	__mips
      PARAMETER        (GRANSZ = 32)   !  Smallest native data granularity
#else
      PARAMETER        (GRANSZ = 64)   !  Smallest native data granularity
#endif

      INTEGER           TEMPSZ
      PARAMETER        (TEMPSZ = 2048) !  Size of temporary buffer

      INTEGER           WORDSZ
      PARAMETER        (WORDSZ = 64)   !  Size of word

      INTEGER           WORDSZM1
      PARAMETER        (WORDSZM1 = WORDSZ - 1)

      INTEGER           WORDSZB
      PARAMETER        (WORDSZB = 6)   !  Log (base 2) of WORDSZ

      INTEGER           CINC, IINC
#ifdef	__mips
      PARAMETER        (CINC = 1)
#else
      PARAMETER        (IINC = 1)
#endif

      INTEGER (KIND=8)  COFF, IOFF, ZERO
#ifdef	__mips
      PARAMETER        (IOFF = 0)
#else
      PARAMETER        (COFF = 0)
#endif
      PARAMETER        (ZERO = 0)

      INTEGER (KIND=8)  CT, CF         !  Cray .TRUE. and .FALSE.
      PARAMETER        (CT = -1)
      PARAMETER        (CF = 0)

      INTEGER (KIND=8)  M08
      PARAMETER        (M08 = X'FF')

      INTEGER (KIND=8)  M16
      PARAMETER        (M16 = X'FFFF')

      INTEGER (KIND=8)  M32
      PARAMETER        (M32 = X'FFFFFFFF')

C     Conversion Functions
C
C     These all must be declared of the proper type for the vector
C     loops to work

      INTEGER (KIND=8)  GI8I$
      INTEGER (KIND=8)  GI16I$
      INTEGER (KIND=8)  GI32I$
      REAL    (KIND=8)  IEF32I$
      REAL    (KIND=8)  IEF64I$
      REAL    (KIND=8)  IEF128I$
      REAL    (KIND=16) IED32I$
      REAL    (KIND=16) IED64I$
      REAL    (KIND=16) IED128I$

C     MIPS double double -> IEEE 128-bit conversion function

      REAL    (KIND=16) IEM128O$

#ifdef	_MAXVL
CDIR$ VFUNCTION GI8I$
CDIR$ VFUNCTION GI16I$
CDIR$ VFUNCTION GI32I$
CDIR$ VFUNCTION IEF32I$
CDIR$ VFUNCTION IEF64I$
CDIR$ VFUNCTION IEF128I$
CDIR$ VFUNCTION IED32I$
CDIR$ VFUNCTION IED64I$
CDIR$ VFUNCTION IED128I$
#endif

C     Conversion function control flags and overflow counts

      INTEGER (KIND=4)         GN32OF
      INTEGER (KIND=4)         DENORM, IEEEOF
#ifdef	__mips
           COMMON  /T$GENERIC/ GN32OF
           COMMON  /T$IEEE/    DENORM, IEEEOF
#else
      TASK COMMON  /T@GENERIC/ GN32OF
      TASK COMMON  /T@IEEE/    DENORM, IEEEOF
#endif

C     Aliasing pointers

      INTEGER (KIND=8)  NCRAY(0:*), NIEEE(0:*)
      INTEGER (KIND=8)  CRAYI(0:*), IEEEI(0:*)
      INTEGER (KIND=4)              IEEEJ(0:*)
      INTEGER (KIND=2)              IEEEK(0:*)
      INTEGER (KIND=1)              IEEEH(0:*)
      REAL    (KIND=8)  CRAYR(0:*), IEEED(0:*)
      REAL    (KIND=4)              IEEES(0:*)
      REAL    (KIND=16) CRAYD(0:*), IEEEQ(0:*)
      COMPLEX (KIND=8)  CRAYC(0:*), IEEEC(0:*)
      COMPLEX (KIND=4)              IEEEF(0:*)
      COMPLEX (KIND=16) CRAYE(0:*), IEEEE(0:*)
      LOGICAL (KIND=8)  CRAYL(0:*), IEEEL(0:*)
      LOGICAL (KIND=4)              IEEEM(0:*)
      LOGICAL (KIND=2)              IEEEN(0:*)
      LOGICAL (KIND=1)              IEEEO(0:*)

      POINTER ( CPTR, NCRAY), ( NPTR, NIEEE)
      POINTER (CIPTR, CRAYI), (IIPTR, IEEEI)
      POINTER                 (IJPTR, IEEEJ)
      POINTER                 (IKPTR, IEEEK)
      POINTER                 (IHPTR, IEEEH)
      POINTER (CRPTR, CRAYR), (IDPTR, IEEED)
      POINTER                 (ISPTR, IEEES)
      POINTER (CDPTR, CRAYD), (IQPTR, IEEEQ)
      POINTER (CCPTR, CRAYC), (ICPTR, IEEEC)
      POINTER                 (IFPTR, IEEEF)
      POINTER (CEPTR, CRAYE), (IEPTR, IEEEE)
      POINTER (CLPTR, CRAYL), (ILPTR, IEEEL)
      POINTER                 (IMPTR, IEEEM)
      POINTER                 (INPTR, IEEEN)
      POINTER                 (IOPTR, IEEEO)

      INTEGER           CHNK, CI, CLENB, I, II, ILENB, INDX
      INTEGER           LEFT, LOGB2, NMBR, REMR, SCALB, SHFT
      INTEGER (KIND=8)  CHLEN, CCOFF, CLEN, IBTL, ICOFF, ILEN
      INTEGER (KIND=8)  IWAD, SCAL, TEMP(0:TEMPSZ-1), ITMP
      LOGICAL           MIPS2CRY_NATIVE_TYPE_OK$
      LOGICAL           MIPS2CRY_FOREIGN_TYPE_OK$

      EXTERNAL MOVBITZ
      EXTERNAL MIPS2CRY_NATIVE_TYPE_OK$, MIPS2CRY_FOREIGN_TYPE_OK$

C     Statement function to compute the Log (base 2) of I

      LOGB2(SCAL) = WORDSZM1 - LEADZ(SCAL)

C     Check argument validity

#ifdef	_CRAY
      IF (NUMARG() .LT. 8) GOTO 9001
#endif

      IF (NUM .LT. 0) GOTO 9003

      IF ((BITOFF .LT. 0) .OR. (BITOFF .GE. WORDSZ)) GOTO 9004

      IF ((TYPE .LT. 1) .OR. (TYPE .GT. 6)) GOTO 9002

C     Ensure lengths are powers of two and otherwise valid

      IF (POPCNT(NATLEN) .NE. 1) GOTO 9005
      IF (.NOT. MIPS2CRY_NATIVE_TYPE_OK$(TYPE, NATLEN)) GOTO 9005

      IF (POPCNT(FORLEN) .NE. 1) GOTO 9006
      IF (.NOT. MIPS2CRY_FOREIGN_TYPE_OK$(TYPE, FORLEN)) GOTO 9006

      CI     = 0                        !  Set indicies
      II     = 0
      GN32OF = 0                        !  Clear overflow counts
      IEEEOF = 0
      NMBR   = NUM

      IF (NMBR .EQ. 0) GOTO 9000

#ifdef	__mips
      NPTR   = LOC(NATIVE(0))
      CPTR   = LOC(FORN(0))
#else
      NPTR   = LOC(FORN(0))
      CPTR   = LOC(NATIVE(0))
#endif

      CIPTR  = CPTR
      CRPTR  = CPTR
      CDPTR  = CPTR
      CCPTR  = CPTR
      CEPTR  = CPTR
      CLPTR  = CPTR

      IIPTR  = NPTR
      IJPTR  = NPTR
      IKPTR  = NPTR
      IHPTR  = NPTR
      IDPTR  = NPTR
      ISPTR  = NPTR
      IQPTR  = NPTR
      ICPTR  = NPTR
      IFPTR  = NPTR
      IEPTR  = NPTR
      ILPTR  = NPTR
      IMPTR  = NPTR
      INPTR  = NPTR
      IOPTR  = NPTR

C     Set lengths, stride and bit offsets

#ifdef	__mips
      CLEN   = FORLEN
      COFF   = BITOFF
      IINC   = STRIDE
      ILEN   = NATLEN
#else
      CINC   = STRIDE
      CLEN   = NATLEN
      ILEN   = FORLEN
      IOFF   = BITOFF
#endif

      CLENB  = LOGB2(CLEN)              !  Log (base 2) of CLEN
      ILENB  = LOGB2(ILEN)              !  Log (base 2) of ILEN

      GOTO (100,200,300,400,500,600), TYPE

C     TYPE = 1  (Typeless)                                  ************
C
C     Convert MIPS typeless to CRAY typeless
C
C     MIPS    |                 CRAY
C             |  64-bit   128-bit   256-bit
C     --------+------------------------------
C     64-bit  |   copy  | INVALID | INVALID |
C             +---------+---------+---------+
C     128-bit | INVALID |   copy  | INVALID |
C             +---------+---------+---------+
C     256-bit | INVALID | INVALID |  copy   |
C     --------+------------------------------

  100 CONTINUE

      IF (CLEN .NE. ILEN) GOTO 9008

C     TYPE = 2  (Integer)                                   ************
C
C     Convert MIPS integers to CRAY integers
C
C     MIPS              |   CRAY
C                       |  64-bit
C     ------------------+----------
C      8-bit INTEGER    |  GI8I$  |
C                       +----------
C     16-bit INTEGER    |  GI16I$ |
C                       +----------
C     32-bit INTEGER    |  GI32I$ |
C                       +----------
C     64-bit INTEGER    |   none  |
C                        ----------

  200 CONTINUE

C     Special case a simple block move

      IF (STRIDE.EQ.1 .AND. ILEN.EQ.CLEN .AND. CLEN.GE.GRANSZ) THEN

        CALL MOVBITZ(NIEEE(0), IOFF, NMBR * ILEN, NCRAY(0), COFF)
        GOTO 9000

      ENDIF

C     Do odd partial-word element(s), if extant

#ifdef	_CRAY
      IF (ILEN .LT. GRANSZ) THEN

        SCAL  = ISHFT(GRANSZ, -ILENB)    ! GRANSZ / ILEN
        SCALB = LOGB2(SCAL)              ! Log (base 2) of SCAL
        I     = SCAL - 1
        REMR  = IAND(NMBR, I)            ! Remainder (NMBR mod SCAL)

        IF (REMR .GT. 0) THEN

          NMBR  = NMBR - REMR
          INDX  = ISHFT(NMBR, -SCALB)

          CALL MOVBITZ(IEEEI(INDX), IOFF, REMR * ILEN, ITMP, ILEN)

          SELECT CASE (ILEN)

            CASE (8)
              SHFT  = -48
#ifdef	_MAXVL
CDIR$         SHORTLOOP
#endif
              DO I = 0, REMR - 1
                CRAYI((NMBR+I)*CINC) = GI8I$(ISHFT(ITMP, SHFT))
                SHFT  = SHFT + 8
              ENDDO

            CASE (16)
              SHFT  = -32
#ifdef  _MAXVL
CDIR$         SHORTLOOP
#endif
              DO I = 0, REMR - 1
                CRAYI((NMBR+I)*CINC) = GI16I$(ISHFT(ITMP, SHFT))
                SHFT  = SHFT + 16
              ENDDO

            CASE (32)
              CRAYI(NMBR * CINC) = GI32I$(ITMP)

          END SELECT

          IF (NMBR .EQ. 0) GOTO 9000

        ENDIF

      ENDIF
#endif

C     If output array is not word-aligned, then the output from the
C     conversion functions must be redirected to a temporary array
C     prior to the final transfer.

      LEFT  = NMBR

      IF (BITOFF .NE. 0) THEN       !  Strip mine

        IF (IOFF .NE. 0) THEN
          IIPTR = LOC(TEMP(0))
          IJPTR = LOC(TEMP(0))
          IKPTR = LOC(TEMP(0))
          IHPTR = LOC(TEMP(0))
          IDPTR = LOC(TEMP(0))
          ISPTR = LOC(TEMP(0))
          IQPTR = LOC(TEMP(0))
          ICPTR = LOC(TEMP(0))
          IFPTR = LOC(TEMP(0))
          IEPTR = LOC(TEMP(0))
          ILPTR = LOC(TEMP(0))
          IMPTR = LOC(TEMP(0))
          INPTR = LOC(TEMP(0))
          IOPTR = LOC(TEMP(0))
          CHNK  = ISHFT(ISHFT(TEMPSZ, WORDSZB), -ILENB)
        ELSE  !  (COFF .NE. 0)
          CIPTR = LOC(TEMP(0))
          CRPTR = LOC(TEMP(0))
          CDPTR = LOC(TEMP(0))
          CCPTR = LOC(TEMP(0))
          CEPTR = LOC(TEMP(0))
          CLPTR = LOC(TEMP(0))
          CHNK  = ISHFT(ISHFT(TEMPSZ, WORDSZB), -CLENB)
        ENDIF

        NMBR  = MOD(NMBR, CHNK)

        IF (NMBR .EQ. 0) NMBR  = CHNK    !  Size of first chunk

        INDX  = 0

      ENDIF

C
C     Main loop for Typeless and Integer conversions
C

      DO WHILE (LEFT .GT. 0)

        IF (IOFF .NE. 0) THEN
          CALL MOVBITZ(NIEEE(INDX), IOFF, NMBR * ILEN, TEMP(0), ZERO)
          II    = 0
        ENDIF

        SELECT CASE (ILEN)

          CASE (8)
#ifdef	__mips
            DO I = 0, NMBR - 1      !  CLEN .EQ. 64 .AND. ILEN .EQ. 8
              CRAYI(CI) = GI8I$(IEEEH(II))
              CI        = CI + CINC
              II        = II + IINC
            ENDDO
#else
            DO I = 0, NMBR - 1, 8   !  CLEN .EQ. 64 .AND. ILEN .EQ. 8
              CRAYI(CI)        = GI8I$(ISHFT(IEEEI(II), -56))
              CRAYI(CI+CINC)   = GI8I$(ISHFT(IEEEI(II), -48))
              CRAYI(CI+CINC*2) = GI8I$(ISHFT(IEEEI(II), -40))
              CRAYI(CI+CINC*3) = GI8I$(ISHFT(IEEEI(II), -32))
              CRAYI(CI+CINC*4) = GI8I$(ISHFT(IEEEI(II), -24))
              CRAYI(CI+CINC*5) = GI8I$(ISHFT(IEEEI(II), -16))
              CRAYI(CI+CINC*6) = GI8I$(ISHFT(IEEEI(II), -8))
              CRAYI(CI+CINC*7) = GI8I$(      IEEEI(II))
              CI               = CI + (8 * CINC)
              II               = II + IINC
            ENDDO
#endif

          CASE (16)
#ifdef	__mips
            DO I = 0, NMBR - 1      !  CLEN .EQ. 64 .AND. ILEN .EQ. 16
              CRAYI(CI) = GI16I$(IEEEK(II))
              CI        = CI + CINC
              II        = II + IINC
            ENDDO
#else
            DO I = 0, NMBR - 1, 4   !  CLEN .EQ. 64 .AND. ILEN .EQ. 16
              CRAYI(CI)        = GI16I$(ISHFT(IEEEI(II), -48))
              CRAYI(CI+CINC)   = GI16I$(ISHFT(IEEEI(II), -32))
              CRAYI(CI+CINC*2) = GI16I$(ISHFT(IEEEI(II), -16))
              CRAYI(CI+CINC*3) = GI16I$(      IEEEI(II))
              CI               = CI + (4 * CINC)
              II               = II + IINC
            ENDDO
#endif

          CASE (32)
#ifdef	__mips
            DO I = 0, NMBR - 1      !  CLEN .EQ. 64 .AND. ILEN .EQ. 32
              CRAYI(CI) = GI32I$(IEEEJ(II))
              CI        = CI + CINC
              II        = II + IINC
            ENDDO
#else
            DO I = 0, NMBR - 1, 2   !  CLEN .EQ. 64 .AND. ILEN .EQ. 32
              CRAYI(CI)        = GI32I$(ISHFT(IEEEI(II), -32))
              CRAYI(CI + CINC) = GI32I$(      IEEEI(II))
              CI               = CI + (2 * CINC)
              II               = II + IINC
            ENDDO
#endif

          CASE (64)
            DO I = 0, NMBR - 1
              CRAYI(CI) = IEEEI(II)
              CI        = CI + CINC
              II        = II + IINC
            ENDDO

          CASE (128)
            DO I = 0, NMBR - 1
              CRAYD(CI) = IEEEQ(II)
              CI        = CI + CINC
              II        = II + IINC
            ENDDO

          CASE (256)
            DO I = 0, NMBR - 1
              CRAYE(CI) = IEEEE(II)
              CI        = CI + CINC
              II        = II + IINC
            ENDDO

        END SELECT

        LEFT  = LEFT - NMBR

        IF (BITOFF .NE. 0) THEN       !  Complete strip mine

          IF (COFF .NE. 0) THEN       !  Complete transfer
            CALL MOVBITZ(TEMP(0), ZERO, NMBR * CLEN, NCRAY(INDX), COFF)
            CI    = 0
          ENDIF

          INDX  = INDX + NMBR
          NMBR  = CHNK
        ENDIF

      ENDDO

      GOTO 9000

C     TYPE = 3  (Real)                                      ************
C
C     Convert MIPS IEEE floating-point numbers to CRAY floating-point
C     numbers
C
C     MIPS IEEE         |             CRAY
C                       |    64-bit          128-bit
C                       |   (single)         (double)
C                       |    KIND=8          KIND=16
C     ------------------+-------------------------------
C     32-bit (single)   |    IEF32I$    |    IED32I$
C                       +---------------+---------------
C     64-bit (double)   |    IEF64I$    |    IED64I$
C                       +---------------+---------------
C     128-bit (quad)    |   IEF128I$    |   IED128I$
C                        -------------------------------

  300 CONTINUE

C     Do odd half-word element, if extant

#ifdef	_CRAY
      IF (ILEN .EQ. 32 .AND. IAND(NMBR, 1) .NE. 0) THEN

        NMBR  = NMBR - 1
        INDX  = ISHFT(NMBR, -1)
        ITMP  = -1

        CALL MOVBITZ(IEEEI(INDX), IOFF, ILEN, ITMP, WORDSZ - ILEN)

        SELECT CASE (CLEN)

          CASE (64)                 !  CLEN .EQ. 64
            CRAYR(NMBR * CINC) = IEF32I$(ITMP)

          CASE (128)                !  CLEN .EQ. 128
            CRAYD(NMBR * CINC) = IED32I$(ITMP)

        END SELECT

        IF (NMBR .EQ. 0) GOTO 9000

      ENDIF
#endif

C     If output array is not word-aligned, then the output from the
C     conversion functions must be redirected to a temporary array
C     prior to the final transfer.

      LEFT  = NMBR

      IF (BITOFF .NE. 0) THEN       !  Strip mine

        IF (IOFF .NE. 0) THEN
          IIPTR = LOC(TEMP(0))
          IJPTR = LOC(TEMP(0))
          IKPTR = LOC(TEMP(0))
          IHPTR = LOC(TEMP(0))
          IDPTR = LOC(TEMP(0))
          ISPTR = LOC(TEMP(0))
          IQPTR = LOC(TEMP(0))
          ICPTR = LOC(TEMP(0))
          IFPTR = LOC(TEMP(0))
          IEPTR = LOC(TEMP(0))
          ILPTR = LOC(TEMP(0))
          IMPTR = LOC(TEMP(0))
          INPTR = LOC(TEMP(0))
          IOPTR = LOC(TEMP(0))
          CHNK  = ISHFT(ISHFT(TEMPSZ, WORDSZB), -ILENB)
        ELSE  !  (COFF .NE. 0)
          CIPTR = LOC(TEMP(0))
          CRPTR = LOC(TEMP(0))
          CDPTR = LOC(TEMP(0))
          CCPTR = LOC(TEMP(0))
          CEPTR = LOC(TEMP(0))
          CLPTR = LOC(TEMP(0))
          CHNK  = ISHFT(ISHFT(TEMPSZ, WORDSZB), -CLENB)
        ENDIF

        NMBR  = MOD(NMBR, CHNK)

        IF (NMBR .EQ. 0) NMBR  = CHNK   !  Size of first chunk

        INDX  = 0
      ENDIF

C
C     Main loop for Real (floating-point) conversions
C

      DO WHILE (LEFT .GT. 0)

        IF (IOFF .NE. 0) THEN
          CALL MOVBITZ(NIEEE(INDX), IOFF, NMBR * ILEN, TEMP(0), ZERO)
          II    = 0
        ENDIF

        SELECT CASE (CLEN)

          CASE (64)                 !  CLEN .EQ. 64

          SELECT CASE (ILEN)

            CASE (32)               !  CLEN .EQ. 64 .AND. ILEN .EQ. 32
#ifdef	__mips
              DO I = 0, NMBR - 1
                CRAYR(CI) = IEF32I$(IEEES(II))
                CI        = CI + CINC
                II        = II + IINC
              ENDDO
#else
              DO I = 0, NMBR - 1, 2
                CRAYR(CI)      = IEF32I$(ISHFT(IEEEI(II), -32))
                CRAYR(CI+CINC) = IEF32I$(      IEEEI(II))
                CI             = CI + (2 * CINC)
                II             = II + IINC
              ENDDO
#endif

            CASE (64)               !  CLEN .EQ. 64 .AND. ILEN .EQ. 64
              DO I = 0, NMBR - 1
                CRAYR(CI) = IEF64I$(IEEED(II))
                CI        = CI + CINC
                II        = II + IINC
              ENDDO

            CASE (128)              !  CLEN .EQ. 64 .AND. ILEN .EQ. 128
              DO I = 0, NMBR - 1
                CRAYR(CI) = IEF128I$(IEM128O$(IEEEQ(II)))
                CI        = CI + CINC
                II        = II + IINC
              ENDDO

          END SELECT

          CASE (128)                !  CLEN .EQ. 128

          SELECT CASE (ILEN)

            CASE (32)               !  CLEN .EQ. 128 .AND. ILEN .EQ. 32
#ifdef	__mips
              DO I = 0, NMBR - 1
                CRAYD(CI) = IED32I$(IEEES(II))
                CI        = CI + CINC
                II        = II + IINC
              ENDDO
#else
              DO I = 0, NMBR - 1, 2
                CRAYD(CI)      = IED32I$(ISHFT(IEEEI(II), -32))
                CRAYD(CI+CINC) = IED32I$(      IEEEI(II))
                CI             = CI + (2 * CINC)
                II             = II + IINC
              ENDDO
#endif

            CASE (64)               !  CLEN .EQ. 128 .AND. ILEN .EQ. 64
              DO I = 0, NMBR - 1
                CRAYD(CI) = IED64I$(IEEED(II))
                CI        = CI + CINC
                II        = II + IINC
              ENDDO

            CASE (128)              !  CLEN .EQ. 128 .AND. ILEN .EQ. 128
              DO I = 0, NMBR - 1
                CRAYD(CI) = IED128I$(IEM128O$(IEEEQ(II)))
                CI        = CI + CINC
                II        = II + IINC
              ENDDO

          END SELECT

        END SELECT

        LEFT  = LEFT - NMBR

        IF (BITOFF .NE. 0) THEN       !  Complete strip mine

          IF (COFF .NE. 0) THEN       !  Complete transfer
            CALL MOVBITZ(TEMP(0), ZERO, NMBR * CLEN, NCRAY(INDX), COFF)
            CI    = 0
          ENDIF

          INDX  = INDX + NMBR
          NMBR  = CHNK
        ENDIF

      ENDDO

      GOTO 9000

C     TYPE = 4  (Complex)                                   ************
C
C     Convert MIPS IEEE complex floating-point numbers to CRAY complex
C     floating-point numbers
C
C     MIPS IEEE         |             CRAY
C                       |   2x64-bit        2x128-bit
C                       |   (single)         (double)
C                       |    KIND=8          KIND=16
C     ------------------+-------------------------------
C     2x32-bit (single) |   2xIEF32I$   |   2xIED32I$
C                       +---------------+---------------
C     2x64-bit (double) |   2xIEF64I$   |   2xIED64I$
C                       +---------------+---------------
C     2x128-bit (quad)  |  2xIEF128I$   |  2xIED128I$
C                        -------------------------------

  400 CONTINUE

C     If output array is not word-aligned, then the output from the
C     conversion functions must be redirected to a temporary array
C     prior to the final transfer.

      LEFT  = NMBR

      IF (BITOFF .NE. 0) THEN       !  Strip mine

        IF (IOFF .NE. 0) THEN
          IIPTR = LOC(TEMP(0))
          IJPTR = LOC(TEMP(0))
          IKPTR = LOC(TEMP(0))
          IHPTR = LOC(TEMP(0))
          IDPTR = LOC(TEMP(0))
          ISPTR = LOC(TEMP(0))
          IQPTR = LOC(TEMP(0))
          ICPTR = LOC(TEMP(0))
          IFPTR = LOC(TEMP(0))
          IEPTR = LOC(TEMP(0))
          ILPTR = LOC(TEMP(0))
          IMPTR = LOC(TEMP(0))
          INPTR = LOC(TEMP(0))
          IOPTR = LOC(TEMP(0))
          CHNK  = ISHFT(ISHFT(TEMPSZ, WORDSZB), -ILENB)
        ELSE  !  (COFF .NE. 0)
          CIPTR = LOC(TEMP(0))
          CRPTR = LOC(TEMP(0))
          CDPTR = LOC(TEMP(0))
          CCPTR = LOC(TEMP(0))
          CEPTR = LOC(TEMP(0))
          CLPTR = LOC(TEMP(0))
          CHNK  = ISHFT(ISHFT(TEMPSZ, WORDSZB), -CLENB)
        ENDIF

        NMBR  = MOD(NMBR, CHNK)

        IF (NMBR .EQ. 0) NMBR  = CHNK   !  Size of first chunk

        INDX  = 0

      ENDIF

C
C     Main loop for Complex conversions
C

      DO WHILE (LEFT .GT. 0)

        IF (IOFF .NE. 0) THEN
          CALL MOVBITZ(NIEEE(INDX), IOFF, NMBR * ILEN, TEMP(0), 0)
          II    = 0
        ENDIF

        SELECT CASE (CLEN)

        CASE (128)

          SELECT CASE (ILEN)

          CASE (64)               !  CLEN .EQ. 128 .AND. ILEN .EQ. 64
            DO I = 0, NMBR - 1
#ifdef	__mips
              CRAYC(CI) = CMPLX(IEF32I$(REAL (IEEEF(II))),
     1                          IEF32I$(AIMAG(IEEEF(II))),
     2                          KIND=8)
#else
              CRAYC(CI) = CMPLX(IEF32I$(ISHFT(IEEEI(II), -32)),
     1                          IEF32I$(      IEEEI(II)),
     2                          KIND=8)
#endif
              CI        = CI + CINC
              II        = II + IINC
            ENDDO

          CASE (128)              !  CLEN .EQ. 128 .AND. ILEN .EQ. 128
            DO I = 0, NMBR - 1
              CRAYC(CI) = CMPLX(IEF64I$(REAL (IEEEC(II))),
     1                          IEF64I$(AIMAG(IEEEC(II))),
     2                          KIND=8)
              CI        = CI + CINC
              II        = II + IINC
            ENDDO

          CASE (256)              !  CLEN .EQ. 128 .AND. ILEN .EQ. 256
            DO I = 0, NMBR - 1
              CRAYC(CI) = CMPLX(IEF128I$(IEM128O$(REAL (IEEEE(II)))),
     1                          IEF128I$(IEM128O$(AIMAG(IEEEE(II)))),
     2                          KIND=8)
              CI        = CI + CINC
              II        = II + IINC
            ENDDO

          END SELECT

        CASE (256)

          SELECT CASE (ILEN)

          CASE (64)               !  CLEN .EQ. 256 .AND. ILEN .EQ. 64
            DO I = 0, NMBR - 1
#ifdef	__mips
              CRAYE(CI) = CMPLX(IED32I$(REAL (IEEEF(II))),
     1                          IED32I$(AIMAG(IEEEF(II))),
     2                          KIND=16)
#else
              CRAYE(CI) = CMPLX(IED32I$(ISHFT(IEEEI(II), -32)),
     1                          IED32I$(      IEEEI(II)),
     2                          KIND=16)
#endif
              CI        = CI + CINC
              II        = II + IINC
            ENDDO

          CASE (128)              !  CLEN .EQ. 256 .AND. ILEN .EQ. 128
            DO I = 0, NMBR - 1
              CRAYE(CI) = CMPLX(IED64I$(REAL (IEEEC(II))),
     1                          IED64I$(AIMAG(IEEEC(II))),
     2                          KIND=16)
              CI        = CI + CINC
              II        = II + IINC
            ENDDO

          CASE (256)              !  CLEN .EQ. 256 .AND. ILEN .EQ. 256
            DO I = 0, NMBR - 1
              CRAYE(CI) = CMPLX(IED128I$(IEM128O$(REAL (IEEEE(II)))),
     1                          IED128I$(IEM128O$(AIMAG(IEEEE(II)))),
     2                          KIND=16)
              CI        = CI + CINC
              II        = II + IINC
            ENDDO

          END SELECT

        END SELECT

        LEFT  = LEFT - NMBR

        IF (BITOFF .NE. 0) THEN       !  Complete strip mine

          IF (COFF .NE. 0) THEN       !  Complete transfer
            CALL MOVBITZ(TEMP(0), ZERO, NMBR * CLEN, NCRAY(INDX), COFF)
            CI    = 0
          ENDIF

          INDX  = INDX + NMBR
          NMBR  = CHNK
        ENDIF

      ENDDO

      GOTO 9000

C     TYPE = 5  (Logical)                                   ************
C
C     Convert MIPS logicals to CRAY logicals
C
C     MIPS              |   CRAY
C                       |  64-bit
C     ------------------+----------
C      8-bit LOGICAL    |  inline
C                       +----------
C     16-bit LOGICAL    |  inline
C                       +----------
C     32-bit LOGICAL    |  inline
C                       +----------
C     64-bit LOGICAL    |  inline
C                        ----------

  500 CONTINUE

C     Do odd partial-word element(s), if extant

#ifdef	_CRAY
      IF (ILEN .LT. GRANSZ) THEN

        SCAL  = ISHFT(GRANSZ, -ILENB)    ! GRANSZ / ILEN
        SCALB = LOGB2(SCAL)              ! Log (base 2) of SCAL
        I     = SCAL - 1
        REMR  = IAND(NMBR, I)            ! Remainder (NMBR mod SCAL)

        IF (REMR .GT. 0) THEN

          NMBR  = NMBR - REMR
          INDX  = ISHFT(NMBR, -SCALB)
          ITMP  = 0

          CALL MOVBITZ(IEEEI(INDX), IOFF, REMR * ILEN, ITMP, ILEN)

          SELECT CASE (ILEN)

            CASE (8)
              SHFT  = -48
#ifdef	_MAXVL
CDIR$         SHORTLOOP
#endif
              DO I = 0, REMR - 1
                CRAYI((NMBR + I) * CINC) = CVMGN(CT, CF,
     1                                     IAND(ISHFT(ITMP, SHFT), M08))
                SHFT  = SHFT + 8
              ENDDO

            CASE (16)
              SHFT  = -32
#ifdef	_MAXVL
CDIR$         SHORTLOOP
#endif
              DO I = 0, REMR - 1
                CRAYI((NMBR + I) * CINC) = CVMGN(CT, CF,
     1                                     IAND(ISHFT(ITMP, SHFT), M16))
                SHFT  = SHFT + 16
              ENDDO

            CASE (32)
              CRAYI(NMBR * CINC) = CVMGN(CT, CF, ITMP)

            CASE (64)
              CRAYI(NMBR * CINC) = CVMGN(CT, CF, ITMP)

          END SELECT

          IF (NMBR .EQ. 0) GOTO 9000

        ENDIF

      ENDIF
#endif

C     If output array is not word-aligned, then the output from the
C     conversion functions must be redirected to a temporary array
C     prior to the final transfer.

      LEFT  = NMBR

      IF (BITOFF .NE. 0) THEN       !  Strip mine

        IF (IOFF .NE. 0) THEN
          IIPTR = LOC(TEMP(0))
          IJPTR = LOC(TEMP(0))
          IKPTR = LOC(TEMP(0))
          IHPTR = LOC(TEMP(0))
          IDPTR = LOC(TEMP(0))
          ISPTR = LOC(TEMP(0))
          IQPTR = LOC(TEMP(0))
          ICPTR = LOC(TEMP(0))
          IFPTR = LOC(TEMP(0))
          IEPTR = LOC(TEMP(0))
          ILPTR = LOC(TEMP(0))
          IMPTR = LOC(TEMP(0))
          INPTR = LOC(TEMP(0))
          IOPTR = LOC(TEMP(0))
          CHNK  = ISHFT(ISHFT(TEMPSZ, WORDSZB), -ILENB)
        ELSE  !  (COFF .NE. 0)
          CIPTR = LOC(TEMP(0))
          CRPTR = LOC(TEMP(0))
          CDPTR = LOC(TEMP(0))
          CCPTR = LOC(TEMP(0))
          CEPTR = LOC(TEMP(0))
          CLPTR = LOC(TEMP(0))
          CHNK  = ISHFT(ISHFT(TEMPSZ, WORDSZB), -CLENB)
        ENDIF

        NMBR  = MOD(NMBR, CHNK)

        IF (NMBR .EQ. 0) NMBR  = CHNK   !  Size of first chunk

        INDX  = 0

      ENDIF

C
C     Main loop for Logical conversions
C

      DO WHILE (LEFT .GT. 0)

        IF (IOFF .NE. 0) THEN
          CALL MOVBITZ(NIEEE(INDX), IOFF, NMBR * ILEN, TEMP(0), ZERO)
          II    = 0
        ENDIF

        SELECT CASE (ILEN)

        CASE (8)
#ifdef	__mips
          DO I = 0, NMBR - 1
            CRAYI(CI) = CVMGT(CT, CF, IEEEO(II))
            CI        = CI + CINC
            II        = II + IINC
          ENDDO
#else
          DO I = 0, NMBR - 1, 8
      CRAYI(CI       ) = CVMGN(CT, CF,      ISHFT(IEEEI(II), -56))
      CRAYI(CI+CINC  ) = CVMGN(CT, CF, IAND(ISHFT(IEEEI(II), -48), M08))
      CRAYI(CI+CINC*2) = CVMGN(CT, CF, IAND(ISHFT(IEEEI(II), -40), M08))
      CRAYI(CI+CINC*3) = CVMGN(CT, CF, IAND(ISHFT(IEEEI(II), -32), M08))
      CRAYI(CI+CINC*4) = CVMGN(CT, CF, IAND(ISHFT(IEEEI(II), -24), M08))
      CRAYI(CI+CINC*5) = CVMGN(CT, CF, IAND(ISHFT(IEEEI(II), -16), M08))
      CRAYI(CI+CINC*6) = CVMGN(CT, CF, IAND(ISHFT(IEEEI(II),  -8), M08))
      CRAYI(CI+CINC*7) = CVMGN(CT, CF, IAND(      IEEEI(II), M08))
            CI         = CI + (8 * CINC)
            II         = II + IINC
          ENDDO
#endif

        CASE (16)
#ifdef	__mips
          DO I = 0, NMBR - 1
            CRAYI(CI) = CVMGT(CT, CF, IEEEN(II))
            CI        = CI + CINC
            II        = II + IINC
          ENDDO
#else
          DO I = 0, NMBR - 1, 4
      CRAYI(CI)        = CVMGN(CT, CF,      ISHFT(IEEEI(II), -48))
      CRAYI(CI+CINC)   = CVMGN(CT, CF, IAND(ISHFT(IEEEI(II), -32), M16))
      CRAYI(CI+CINC*2) = CVMGN(CT, CF, IAND(ISHFT(IEEEI(II), -16), M16))
      CRAYI(CI+CINC*3) = CVMGN(CT, CF, IAND(      IEEEI(II), M16))
            CI         = CI + (4 * CINC)
            II         = II + IINC
          ENDDO
#endif

        CASE (32)
#ifdef	__mips
          DO I = 0, NMBR - 1
            CRAYI(CI) = CVMGT(CT, CF, IEEEM(II))
            CI        = CI + CINC
            II        = II + IINC
          ENDDO
#else
          DO I = 0, NMBR - 1, 2
            CRAYI(CI)      = CVMGN(CT, CF, ISHFT(IEEEI(II), -32))
            CRAYI(CI+CINC) = CVMGN(CT, CF,  IAND(IEEEI(II), M32))
            CI             = CI + (2 * CINC)
            II             = II + IINC
          ENDDO
#endif

        CASE (64)
          DO I = 0, NMBR - 1
#ifdef	__mips
            CRAYI(CI) = CVMGT(CT, CF, IEEEL(II))
#else
            CRAYI(CI) = CVMGN(CT, CF, IEEEI(II))
#endif
            CI        = CI + CINC
            II        = II + IINC
          ENDDO

        END SELECT

        LEFT  = LEFT - NMBR

        IF (BITOFF .NE. 0) THEN       !  Complete strip mine

          IF (COFF .NE. 0) THEN       !  Complete transfer
            CALL MOVBITZ(TEMP(0), ZERO, NMBR * CLEN, NCRAY(INDX), COFF)
            CI    = 0
          ENDIF

          INDX  = INDX + NMBR
          NMBR  = CHNK
        ENDIF

      ENDDO

      GOTO 9000

C     TYPE = 6  (Character)                                 ************

  600 CONTINUE

      IF (CLEN .NE. ILEN) GOTO 9008

#ifdef	__mips
      CALL G$CHRPCK(NATIVECH, IWAD, IBTL, ICOFF)  ! Get character offset
      CCOFF = COFF
      IIPTR = IWAD
#else
      CALL G@CHRPCK(NATIVECH, IWAD, IBTL, CCOFF)  ! Get character offset
      ICOFF = IOFF
      CIPTR = IWAD
#endif

      CHLEN = ILEN * LEN(NATIVECH(0))             ! Get character length

      IF (STRIDE .EQ. 1) THEN

        CALL MOVBITZ(IEEEI(0), ICOFF, NMBR * CHLEN, CRAYI(0), CCOFF)

      ELSE

        DO I = 0, NMBR - 1

          CALL MOVBITZ(IEEEI(II), ICOFF, CHLEN, CRAYI(CI), CCOFF)

          CCOFF = CCOFF + (CHLEN * CINC)
          CI    = CI + ISHFT(CCOFF, -WORDSZB)
          CCOFF = MOD(CCOFF, INT(WORDSZ, KIND=8))
          ICOFF = ICOFF + (CHLEN * IINC)
          II    = II + ISHFT(ICOFF, -WORDSZB)
          ICOFF = MOD(ICOFF, INT(WORDSZ, KIND=8))

        ENDDO

      ENDIF
      GOTO 9000

 9010 CONTINUE
      MIPS2CRY = -10              !  FORN must be 64-bit word-aligned
      GOTO 10000

 9009 CONTINUE
      MIPS2CRY = -9               !  NATIVE must be 64-bit word-aligned
      GOTO 10000

 9008 CONTINUE
      MIPS2CRY = -8               !  Parameter error, NATLEN/FORLEN invalid
      GOTO 10000

C9007 CONTINUE
C     MIPS2CRY = -7               !  Unable to allocate memory (unused in
C     GOTO 10000                  !  this version of the routine)

 9006 CONTINUE
      MIPS2CRY = -6               !  Parameter error, invalid FORLEN
      GOTO 10000

 9005 CONTINUE
      MIPS2CRY = -5               !  Parameter error, invalid NATLEN
      GOTO 10000

 9004 CONTINUE
      MIPS2CRY = -4               !  Parameter error, invalid BITOFF
      GOTO 10000

 9003 CONTINUE
      MIPS2CRY = -3               !  Parameter error, invalid NUM
      GOTO 10000

 9002 CONTINUE
      MIPS2CRY = -2               !  Parameter error, invalid TYPE
      GOTO 10000

 9001 CONTINUE
      MIPS2CRY = -1               !  Parameter error
      GOTO 10000

 9000 CONTINUE
      MIPS2CRY = GN32OF + IEEEOF  !  Return error count
C     GOTO 10000

10000 CONTINUE
      RETURN

CDIR$ ID "@(#) libu/ieg/mips2cry.F	92.0	10/08/98 14:57:41"
      END
C
C     Auxiliary functions
C
      LOGICAL FUNCTION MIPS2CRY_AUXILIARY_FUNCTION$()
      IMPLICIT NONE
      LOGICAL NOK, FOK
      INTEGER NTYPE, NLEN, FTYPE, FLEN
      INTEGER (KIND=8) ONE
      PARAMETER       (ONE = 1)
C
C     Configuration information for these routines
C
      INTEGER (KIND=8) NSIZE(6)
      INTEGER (KIND=8) FSIZE(6)
      EXTERNAL MIPS2CRY_INIT
C
#ifdef	__mips
      COMMON /G$MIPS2CRY/ NSIZE, FSIZE
      SAVE   /G$MIPS2CRY/
#else
      COMMON /G@MIPS2CRY/ NSIZE, FSIZE
      SAVE   /G@MIPS2CRY/
#endif
C
      MIPS2CRY_AUXILIARY_FUNCTION$ = .TRUE.
      RETURN
C
      ENTRY MIPS2CRY_NATIVE_TYPE_OK$(NTYPE, NLEN) RESULT (NOK)
      IF (IAND(ISHFT(ONE,((NLEN/8)-1)), NSIZE(NTYPE)) .NE. 0) THEN
        NOK = .TRUE.
      ELSE
        NOK = .FALSE.
      ENDIF
      RETURN
C
      ENTRY MIPS2CRY_FOREIGN_TYPE_OK$(FTYPE, FLEN) RESULT (FOK)
      IF (IAND(ISHFT(ONE,((FLEN/8)-1)), FSIZE(FTYPE)) .NE. 0) THEN
        FOK = .TRUE.
      ELSE
        FOK = .FALSE.
      ENDIF
      RETURN
C
      END
C
      BLOCK DATA MIPS2CRY_INIT
C
      INTEGER (KIND=8) NSIZE(6)
      INTEGER (KIND=8) FSIZE(6)
#ifdef	__mips
      COMMON /G$MIPS2CRY/ NSIZE, FSIZE
      SAVE   /G$MIPS2CRY/
#else
      COMMON /G@MIPS2CRY/ NSIZE, FSIZE
      SAVE   /G@MIPS2CRY/
#endif
C
C     The following masks indicate the allowable values for the NATLEN
C     and FORLEN arguments.  External routines (e.g., implicit data
C     conversion) can query these values to determine acceptable sizes.
C     Each bit corresponds to a accepted size (in bytes).  Thus, the
C     rightmost bit (bit 1), if set, indicates that a length of 1 byte
C     (8 bits) is valid.
C
#ifdef	__mips
      DATA NSIZE /
#else
      DATA FSIZE /
#endif
     1          O'20000100200',   ! Typeless:  256-, 128- and 64-bit
     2          O'        213',   ! Integer:  64-, 32-, 16- and 8-bit
     3          O'     100210',   ! Real:  128-, 64- and 32-bit
     4          O'20000100200',   ! Complex:  256-, 128- and 64-bit
     5          O'        213',   ! Logical:  64-, 32-, 16- and 8-bit
     6          O'        001'/   ! Character:  8-bit
C
#ifdef	__mips
      DATA FSIZE /
#else
      DATA NSIZE /
#endif
     1          O'20000100200',   ! Typeless:  256-, 128- and 64-bit
     2          O'        200',   ! Integer:  64-bit
     3          O'     100200',   ! Real:  128- and 64-bit
     4          O'20000100000',   ! Complex:  256- and 128-bit
     5          O'        200',   ! Logical:  64-bit
     6          O'        001'/   ! Character:  8-bit
      END BLOCK DATA MIPS2CRY_INIT
