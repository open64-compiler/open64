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

      INTEGER FUNCTION CRY2MIPS(
     +TYPE, NUM, FORN, BITOFF, NATIVE, STRIDE, NATLEN, FORLEN, NATIVECH)

      IMPLICIT NONE

C     This function converts CRAY Fortran data types to MIPS IEEE
C     Fortran data types.
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
C             -9      NATIVE must be 64-bit word-aligned (CRAY only)
C             -10     FORN must be 64-bit word-aligned
C          0  Translation complete; no errors
C         >0  Translation complete; return value is the number of
C             values that overflowed during translation.

      INTEGER TYPE
      INTENT (IN) TYPE

C         Type code:
C           1 = typeless (no translation); NATLEN must equal FORLEN
C               and be 64, 128 or 256.
C           2 = integer (64-bit twos-complement to 64-, 32-, 16-, or
C               8-bit twos-complement)
C           3 = real (64- or 128-bit CRAY floating-point to 32-, 64-
C               or 128-bit MIPS IEEE floating-point)
C           4 = complex (2 x 64- or 128-bit CRAY floating-point to
C               2 x 32-, 64- or 128-bit MIPS IEEE floating-point).
C           5 = logical (64-bit positive/negative logical to 64-, 32-,
C               16- or 8-bit zero/nonzero logical).
C           6 = character (ASCII to ASCII; no translation)
C
C         The NATLEN and FORLEN parameters (see below) select the size
C         of the data.

      INTEGER NUM
      INTENT (IN) NUM

C         Number of data items to convert.  Type integer variable,
C         expression, or constant.

      INTEGER (KIND=1) FORN(0:*)
      INTENT (OUT) FORN

C         Variable or array of any type (except CHARACTER) or length
C         that contains (or will contain) the foreign data.

      INTEGER BITOFF
      INTENT (IN) BITOFF

C         Bit offset within FORN to begin placing the converted
C         data.  Type integer variable, expression, or constant.
C         Bits are numbered from 0 to 63, beginning at the leftmost
C         bit of FORN.

      INTEGER (KIND=1) NATIVE(0:*)
      INTENT (IN) NATIVE

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
      INTENT (IN) NATIVECH
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

      INTEGER (KIND=8)  B00
      PARAMETER        (B00 = X'0')

      INTEGER (KIND=8)  B01
      PARAMETER        (B01 = X'1')

      INTEGER (KIND=8)  B08
      PARAMETER        (B08 = X'100')

      INTEGER (KIND=8)  B16
      PARAMETER        (B16 = X'10000')

      INTEGER (KIND=8)  B24
      PARAMETER        (B24 = X'1000000')

      INTEGER (KIND=8)  B32
      PARAMETER        (B32 = X'100000000')

      INTEGER (KIND=8)  B40
      PARAMETER        (B40 = X'10000000000')

      INTEGER (KIND=8)  B48
      PARAMETER        (B48 = X'1000000000000')

      INTEGER (KIND=8)  B56
      PARAMETER        (B56 = X'100000000000000')

      INTEGER (KIND=8)  M32
      PARAMETER        (M32 = X'FFFFFFFF')

C     Conversion Functions
C
C     These all must be declared of the proper type for the data
C     conversion loops to work

#ifdef	__mips
      INTEGER (KIND=1)  GI8O$
      INTEGER (KIND=2)  GI16O$
      INTEGER (KIND=4)  GI32O$
      REAL    (KIND=4)  IEF32O$
      REAL    (KIND=4)  IED32O$
#else
      INTEGER (KIND=8)  GI8O$
      INTEGER (KIND=8)  GI16O$
      INTEGER (KIND=8)  GI32O$
      INTEGER (KIND=8)  IEF32O$
      INTEGER (KIND=8)  IED32O$
#endif
      REAL    (KIND=8)  IEF64O$
      REAL    (KIND=16) IEF128O$
      REAL    (KIND=8)  IED64O$
      REAL    (KIND=16) IED128O$

C     IEEE 128-bit -> MIPS double double conversion function

      REAL    (KIND=16) IEM128I$

#ifdef	_MAXVL
CDIR$ VFUNCTION GI8O$
CDIR$ VFUNCTION GI16O$
CDIR$ VFUNCTION GI32O$
CDIR$ VFUNCTION IEF32O$
CDIR$ VFUNCTION IEF64O$
CDIR$ VFUNCTION IEF128O$
CDIR$ VFUNCTION IED32O$
CDIR$ VFUNCTION IED64O$
CDIR$ VFUNCTION IED128O$
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

      POINTER ( CPTR, NCRAY), ( IPTR, NIEEE)
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
      INTEGER           LEFT, LOGB2, NMBR, REMR, SCALB
      INTEGER (KIND=8)  CHLEN, CCOFF, CLEN, IBTL, ICOFF, ILEN, IWAD
      INTEGER (KIND=8)  SCAL, TEMP(0:TEMPSZ-1), ITMP, TINT(0:6)
      LOGICAL (KIND=8)  TLOG(0:6)
      LOGICAL           CRY2MIPS_NATIVE_TYPE_OK$
      LOGICAL           CRY2MIPS_FOREIGN_TYPE_OK$

      EXTERNAL MOVBITZ
      EXTERNAL CRY2MIPS_NATIVE_TYPE_OK$, CRY2MIPS_FOREIGN_TYPE_OK$

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
      IF (.NOT. CRY2MIPS_NATIVE_TYPE_OK$(TYPE, NATLEN)) GOTO 9005

      IF (POPCNT(FORLEN) .NE. 1) GOTO 9006
      IF (.NOT. CRY2MIPS_FOREIGN_TYPE_OK$(TYPE, FORLEN)) GOTO 9006

      CI     = 0                        !  Set indicies
      II     = 0
      GN32OF = 0                        !  Clear overflow counts
      IEEEOF = 0
      NMBR   = NUM

      IF (NMBR .EQ. 0) GOTO 9000

#ifdef	__mips
      IPTR   = LOC(NATIVE(0))
      CPTR   = LOC(FORN(0))
#else
      IPTR   = LOC(FORN(0))
      CPTR   = LOC(NATIVE(0))
#endif

      CIPTR  = CPTR
      CRPTR  = CPTR
      CDPTR  = CPTR
      CCPTR  = CPTR
      CEPTR  = CPTR
      CLPTR  = CPTR

      IIPTR  = IPTR
      IJPTR  = IPTR
      IKPTR  = IPTR
      IHPTR  = IPTR
      IDPTR  = IPTR
      ISPTR  = IPTR
      IQPTR  = IPTR
      ICPTR  = IPTR
      IFPTR  = IPTR
      IEPTR  = IPTR
      ILPTR  = IPTR
      IMPTR  = IPTR
      INPTR  = IPTR
      IOPTR  = IPTR

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
C     Convert CRAY typeless to MIPS typeless
C
C     MIPS    |            CRAY
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
C     Convert CRAY integers to MIPS integers
C
C     CRAY              |                   MIPS
C                       |   8-bit     16-bit     32-bit     64-bit
C     ------------------+-----------------------------------------------
C     64-bit INTEGER    |   GI8O$  |  GI16O$  |  GI32O$  |   none
C                        -----------------------------------------------

  200 CONTINUE

C     Special case a simple block move

      IF (STRIDE.EQ.1 .AND. ILEN.EQ.CLEN .AND. CLEN.GE.GRANSZ) THEN

        CALL MOVBITZ(NCRAY(0), COFF, NMBR * ILEN, NIEEE(0), IOFF)
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

          TINT  = 0
          NMBR  = NMBR - REMR
          INDX  = ISHFT(NMBR, -SCALB)

#ifdef	_CRAY1
CDIR$     SHORTLOOP
#endif
          DO I = 0, REMR - 1
            TINT(I) = CRAYI((NMBR + I) * CINC)
          ENDDO

          SELECT CASE (ILEN)

            CASE (8)
            ITMP = IOR(IOR(IOR(ISHFT(GI8O$(TINT(0)), 48),
     1                         ISHFT(GI8O$(TINT(1)), 40)),
     2                     IOR(ISHFT(GI8O$(TINT(2)), 32),
     3                         ISHFT(GI8O$(TINT(3)), 24)) ),
     4                 IOR(IOR(ISHFT(GI8O$(TINT(4)), 16),
     5                         ISHFT(GI8O$(TINT(5)),  8)),
     6                               GI8O$(TINT(6))     ) )

            CASE (16)
              ITMP = IOR(IOR(ISHFT(GI16O$(TINT(0)), 32),
     1                       ISHFT(GI16O$(TINT(1)), 16) ),
     2                             GI16O$(TINT(2))      )

            CASE (32)
              ITMP = GI32O$(TINT(0))

          END SELECT

          CALL MOVBITZ(ITMP, ILEN, REMR * ILEN, NIEEE(INDX), IOFF)

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

        IF (COFF .NE. 0) THEN
          CALL MOVBITZ(NCRAY(INDX), COFF, NMBR * CLEN, TEMP(0), ZERO)
          CI    = 0
        ENDIF

        SELECT CASE (ILEN)

          CASE (8)
#ifdef	__mips
            DO I = 0, NMBR - 1
              IEEEH(II) = GI8O$(CRAYI(CI))
              CI        = CI + CINC
              II        = II + IINC
            ENDDO
#else
            DO I = 0, NMBR - 1, 8
           IEEEI(II) = IOR(IOR(IOR(ISHFT(GI8O$(CRAYI(CI       )), 56),
     1                             ISHFT(GI8O$(CRAYI(CI+CINC  )), 48)),
     2                         IOR(ISHFT(GI8O$(CRAYI(CI+CINC*2)), 40),
     3                             ISHFT(GI8O$(CRAYI(CI+CINC*3)), 32))),
     4                     IOR(IOR(ISHFT(GI8O$(CRAYI(CI+CINC*4)), 24),
     5                             ISHFT(GI8O$(CRAYI(CI+CINC*5)), 16)),
     6                         IOR(ISHFT(GI8O$(CRAYI(CI+CINC*6)),  8),
     7                                   GI8O$(CRAYI(CI+CINC*7))    )))
              CI   = CI + (8 * CINC)
              II   = II + IINC
            ENDDO
#endif

          CASE (16)
#ifdef	__mips
            DO I = 0, NMBR - 1
              IEEEK(II) = GI16O$(CRAYI(CI))
              CI        = CI + CINC
              II        = II + IINC
            ENDDO
#else
            DO I = 0, NMBR - 1, 4
              IEEEI(II) = IOR(IOR(ISHFT(GI16O$(CRAYI(CI       )), 48),
     1                            ISHFT(GI16O$(CRAYI(CI+CINC  )), 32)),
     2                        IOR(ISHFT(GI16O$(CRAYI(CI+CINC*2)), 16),
     3                                  GI16O$(CRAYI(CI+CINC*3))    ))
              CI        = CI + (4 * CINC)
              II        = II + IINC
            ENDDO
#endif

          CASE (32)
#ifdef	__mips
            DO I = 0, NMBR - 1
              IEEEJ(II) = GI32O$(CRAYI(CI))
              CI        = CI + CINC
              II        = II + IINC
            ENDDO
#else
            DO I = 0, NMBR - 1, 2
              IEEEI(II) = IOR(ISHFT(GI32O$(CRAYI(CI       )), 32),
     1                              GI32O$(CRAYI(CI + CINC)))
              CI        = CI + (2 * CINC)
              II        = II + IINC
            ENDDO
#endif

          CASE (64)
            DO I = 0, NMBR - 1
              IEEEI(II) = CRAYI(CI)
              CI        = CI + CINC
              II        = II + IINC
            ENDDO

          CASE (128)
            DO I = 0, NMBR-1
              IEEEQ(II) = CRAYD(CI)
              CI        = CI + CINC
              II        = II + IINC
            ENDDO

          CASE (256)
            DO I = 0, NMBR-1
              IEEEE(II) = CRAYE(CI)
              CI        = CI + CINC
              II        = II + IINC
            ENDDO

        END SELECT

        LEFT  = LEFT - NMBR

        IF (BITOFF .NE. 0) THEN      !  Complete strip mine

          IF (IOFF .NE. 0) THEN      !  Complete transfer
            CALL MOVBITZ(TEMP(0), ZERO, NMBR * ILEN, NIEEE(INDX), IOFF)
            II    = 0
          ENDIF

          INDX  = INDX + NMBR
          NMBR  = CHNK
        ENDIF

      ENDDO

      GOTO 9000

C     TYPE = 3  (Real)                                      ************
C
C     Convert CRAY floating-point numbers to MIPS IEEE floating-point
C     numbers
C
C     CRAY              |                  MIPS IEEE
C                       |    32-bit          64-bit          128-bit
C                       |   (single)        (double)         (quad)
C     ------------------+-----------------------------------------------
C     64-bit (KIND=8)   |    IEF32O$    |    IEF64O$    |    IEF128O$
C                       +---------------+---------------+---------------
C     128-bit (KIND=16) |    IED32O$    |    IED64O$    |    IED128O$
C                        -----------------------------------------------

  300 CONTINUE

C     Do odd half-word element, if extant

#ifdef	_CRAY
      IF (ILEN .EQ. 32 .AND. IAND(NMBR, 1) .NE. 0) THEN

        NMBR    = NMBR - 1
        INDX    = ISHFT(NMBR, -1) * IINC

        SELECT CASE (CLEN)

          CASE (64)                 !  CLEN .EQ. 64
            ITMP = IEF32O$(CRAYR(NMBR * CINC))

          CASE (128)                !  CLEN .EQ. 128
            ITMP = IED64O$(CRAYD(NMBR * CINC))

        END SELECT

        CALL MOVBITZ(ITMP, WORDSZ - ILEN, ILEN, NIEEE(INDX), IOFF)

        IF (NMBR. EQ. 0) GOTO 9000

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

        IF (COFF .NE. 0) THEN
          CALL MOVBITZ(NCRAY(INDX), COFF, NMBR * CLEN, TEMP(0), ZERO)
          CI    = 0
        ENDIF

        SELECT CASE (CLEN)
          CASE (64)                 !  CLEN .EQ. 64

          SELECT CASE (ILEN)

            CASE (32)               !  CLEN .EQ. 64 .AND. ILEN .EQ. 32
#ifdef	__mips
              DO I = 0, NMBR - 1
                IEEES(II) = IEF32O$(CRAYR(CI))
                CI        = CI + CINC
                II        = II + IINC
              ENDDO
#else
              DO I = 0, NMBR - 1, 2
                IEEEI(II) = IOR(ISHFT(IEF32O$(CRAYR(CI)), 32),
     1                               (IEF32O$(CRAYR(CI + CINC))))
                CI        = CI + (2 * CINC)
                II        = II + IINC
              ENDDO
#endif

            CASE (64)               !  CLEN .EQ. 64 .AND. ILEN .EQ. 64
              DO I = 0, NMBR - 1
                IEEED(II) = IEF64O$(CRAYR(CI))
                CI        = CI + CINC
                II        = II + IINC
              ENDDO

            CASE (128)              !  CLEN .EQ. 64 .AND. ILEN .EQ. 128
              DO I = 0, NMBR - 1
                IEEEQ(II) = IEM128I$(IEF128O$(CRAYR(CI)))
                CI        = CI + CINC
                II        = II + IINC
              ENDDO

          END SELECT

          CASE (128)                !  CLEN .EQ. 128

          SELECT CASE (ILEN)

            CASE (32)               !  CLEN .EQ. 128 .AND. ILEN .EQ. 32
#ifdef	__mips
              DO I = 0, NMBR - 1
                IEEES(II) = IED32O$(CRAYD(CI))
                CI        = CI + CINC
                II        = II + IINC
              ENDDO
#else
              DO I = 0, NMBR - 1, 2
                IEEEI(II) = IOR(ISHFT(IED32O$(CRAYD(CI)), 32),
     1                               (IED32O$(CRAYD(CI + CINC))))
                CI        = CI + (2 * CINC)
                II        = II + IINC
              ENDDO
#endif

            CASE (64)               !  CLEN .EQ. 128 .AND. ILEN .EQ. 64
              DO I = 0, NMBR - 1
                IEEED(II) = IED64O$(CRAYD(CI))
                CI        = CI + CINC
                II        = II + IINC
              ENDDO

            CASE (128)              !  CLEN .EQ. 128 .AND. ILEN .EQ. 128
              DO I = 0, NMBR - 1
                IEEEQ(II) = IEM128I$(IED128O$(CRAYD(CI)))
                CI        = CI + CINC
                II        = II + IINC
              ENDDO

          END SELECT

        END SELECT

        LEFT  = LEFT - NMBR

        IF (BITOFF .NE. 0) THEN       !  Complete strip mine

          IF (IOFF .NE. 0) THEN       !  Complete transfer
            CALL MOVBITZ(TEMP(0), ZERO, NMBR * ILEN, NIEEE(INDX), IOFF)
            II    = 0
          ENDIF

          INDX  = INDX + NMBR
          NMBR  = CHNK
        ENDIF

      ENDDO

      GOTO 9000

C     TYPE = 4  (Complex)                                   ************
C
C     Convert CRAY complex floating-point numbers to MIPS IEEE complex
C     floating-point numbers
C
C     CRAY              |                  MIPS IEEE
C                       |   2x32-bit        2x64-bit        2x128-bit
C                       |   (single)        (double)         (quad)   
C     ------------------+-----------------------------------------------
C     2x64-bit (KIND=8) |   2xIEF32O$   |   2xIEF64O$   |   2xIEF128O$
C                       +---------------+---------------+---------------
C     2x128-bit (KIND=16|   2xIED32O$   |   2xIED64O$   |   2xIED128O$
C                        -----------------------------------------------

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

        IF (COFF .NE. 0) THEN
          CALL MOVBITZ(NCRAY(INDX), COFF, NMBR * CLEN, TEMP(0), ZERO)
          CI    = 0
        ENDIF

        SELECT CASE (CLEN)

        CASE (128)

          SELECT CASE (ILEN)

          CASE (64)               !  CLEN .EQ. 128 .AND. ILEN .EQ. 64
#ifdef	__mips
            DO I = 0, NMBR - 1
              IEEEF(II) = CMPLX(IEF32O$(REAL (CRAYC(CI))),
     1                          IEF32O$(AIMAG(CRAYC(CI))),
     2                          KIND=4)
              CI        = CI + CINC
              II        = II + IINC
            ENDDO
#else
            DO I = 0, NMBR - 1
              IEEEI(II) = IOR(ISHFT(IEF32O$(REAL (CRAYC(CI))), 32),
     1                              IEF32O$(AIMAG(CRAYC(CI)))      )
              CI        = CI + CINC
              II        = II + IINC
            ENDDO
#endif

          CASE (128)              !  CLEN .EQ. 128 .AND. ILEN .EQ. 128
            DO I = 0, NMBR - 1
              IEEEC(II) = CMPLX(IEF64O$(REAL (CRAYC(CI))),
     1                          IEF64O$(AIMAG(CRAYC(CI))),
     2                          KIND=8)
              CI        = CI + CINC
              II        = II + IINC
            ENDDO

          CASE (256)              !  CLEN .EQ. 128 .AND. ILEN .EQ. 256
            DO I = 0, NMBR - 1
              IEEEE(II) = CMPLX(IEM128I$(IEF128O$(REAL (CRAYC(CI)))),
     1                          IEM128I$(IEF128O$(AIMAG(CRAYC(CI)))),
     2                          KIND=16)
              CI        = CI + CINC
              II        = II + IINC
            ENDDO

          END SELECT

        CASE (256)

          SELECT CASE (ILEN)

          CASE (64)               !  CLEN .EQ. 256 .AND. ILEN .EQ. 64
            DO I = 0, NMBR - 1
#ifdef	__mips
              IEEEF(II) = CMPLX(IED32O$(REAL (CRAYE(CI))),
     1                          IED32O$(AIMAG(CRAYE(CI))),
     2                          KIND=4)
#else
              IEEEI(II) = IOR(ISHFT(IED32O$(REAL (CRAYE(CI))), 32),
     1                              IED32O$(AIMAG(CRAYE(CI)))      )
#endif
              CI        = CI + CINC
              II        = II + IINC
            ENDDO

          CASE (128)              !  CLEN .EQ. 256 .AND. ILEN .EQ. 128
            DO I = 0, NMBR - 1
              IEEEC(II) = CMPLX(IED64O$(REAL (CRAYE(CI))),
     1                          IED64O$(AIMAG(CRAYE(CI))),
     2                          KIND=8)
              CI        = CI + CINC
              II        = II + IINC
            ENDDO

          CASE (256)              !  CLEN .EQ. 256 .AND. ILEN .EQ. 256
            DO I = 0, NMBR - 1
              IEEEE(II) = CMPLX(IEM128I$(IED128O$(REAL (CRAYE(CI)))),
     1                          IEM128I$(IED128O$(AIMAG(CRAYE(CI)))),
     2                          KIND=16)
              CI        = CI + CINC
              II        = II + IINC
            ENDDO

          END SELECT

        END SELECT

        LEFT  = LEFT - NMBR

        IF (BITOFF .NE. 0) THEN       !  Complete strip mine

          IF (IOFF .NE. 0) THEN       !  Complete transfer
            CALL MOVBITZ(TEMP(0), ZERO, NMBR * ILEN, NIEEE(INDX), IOFF)
            II    = 0
          ENDIF

          INDX  = INDX + NMBR
          NMBR  = CHNK
        ENDIF

      ENDDO

      GOTO 9000

C     TYPE = 5  (Logical)                                   ************
C
C     Convert CRAY logicals to MIPS logicals
C
C     CRAY              |                   MIPS
C                       |   8-bit     16-bit     32-bit     64-bit
C     ------------------+-----------------------------------------------
C     64-bit LOGICAL    |  inline  |  inline  |  inline  |  inline
C                        -----------------------------------------------

  500 CONTINUE

C     Do odd partial-word element(s), if extant

#ifdef	_CRAY
      IF (ILEN .LT. GRANSZ) THEN

        SCAL  = ISHFT(GRANSZ, -ILENB)    ! GRANSZ / ILEN
        SCALB = LOGB2(SCAL)              ! Log (base 2) of SCAL
        I     = SCAL - 1
        REMR  = IAND(NMBR, I)            ! Remainder (NMBR mod SCAL)

        IF (REMR .GT. 0) THEN

          TLOG  = .FALSE.
          NMBR  = NMBR - REMR
          INDX  = ISHFT(NMBR, -SCALB)

#ifdef	_CRAY1
CDIR$     SHORTLOOP
#endif
          DO I = 0, REMR - 1
            TLOG(I) = CRAYL((NMBR + I) * CINC)
          ENDDO

          SELECT CASE (ILEN)

            CASE (8)
              ITMP = IOR(IOR(IOR(CVMGT(B48, B00, TLOG(0)),
     1                           CVMGT(B40, B00, TLOG(1)) ),
     2                       IOR(CVMGT(B32, B00, TLOG(2)),
     3                           CVMGT(B24, B00, TLOG(3)) )),
     4                   IOR(IOR(CVMGT(B16, B00, TLOG(4)),
     5                           CVMGT(B08, B00, TLOG(5)) ),
     6                       IOR(CVMGT(B01, B00, TLOG(6)),
     7                           CVMGT(B00, B00, .FALSE.) )))

            CASE (16)
              ITMP = IOR(IOR(CVMGT(B32, B00, TLOG(0)),
     1                       CVMGT(B16, B00, TLOG(1)) ),
     2                   IOR(CVMGT(B01, B00, TLOG(2)),
     3                       CVMGT(B00, B00, .FALSE.  ) ))

            CASE (32)
              ITMP = CVMGT(B01, B00, TLOG(0))

          END SELECT

          CALL MOVBITZ(ITMP, ILEN, REMR * ILEN, NIEEE(INDX), IOFF)

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

        IF (COFF .NE. 0) THEN
          CALL MOVBITZ(NCRAY(INDX), COFF, NMBR * CLEN, TEMP(0), ZERO)
          CI    = 0
        ENDIF

        SELECT CASE (ILEN)

        CASE (8)
#ifdef	__mips
          DO I = 0, NMBR - 1
            IEEEO(II) = CVMGM(.TRUE., .FALSE., CRAYL(CI))
            CI        = CI + CINC
            II        = II + IINC
          ENDDO
#else
          DO I = 0, NMBR - 1, 8
          IEEEI(II) = IOR(IOR(IOR(CVMGT(B56, B00, CRAYL(CI       )),
     1                            CVMGT(B48, B00, CRAYL(CI+CINC  )) ),
     2                        IOR(CVMGT(B40, B00, CRAYL(CI+CINC*2)),
     3                            CVMGT(B32, B00, CRAYL(CI+CINC*3)) )),
     4                    IOR(IOR(CVMGT(B24, B00, CRAYL(CI+CINC*4)),
     5                            CVMGT(B16, B00, CRAYL(CI+CINC*5)) ),
     6                        IOR(CVMGT(B08, B00, CRAYL(CI+CINC*6)),
     7                            CVMGT(B01, B00, CRAYL(CI+CINC*7)) )))
            CI        = CI + (8 * CINC)
            II        = II + IINC
          ENDDO
#endif

        CASE (16)
#ifdef	__mips
          DO I = 0, NMBR - 1
            IEEEN(II) = CVMGM(.TRUE., .FALSE., CRAYL(CI))
            CI        = CI + CINC
            II        = II + IINC
          ENDDO
#else
          DO I = 0, NMBR - 1, 4
            IEEEI(II) = IOR(IOR(CVMGT(B48, B00, CRAYL(CI       )),
     1                          CVMGT(B32, B00, CRAYL(CI+CINC  )) ),
     2                      IOR(CVMGT(B16, B00, CRAYL(CI+CINC*2)),
     3                          CVMGT(B01, B00, CRAYL(CI+CINC*3)) ))
            CI        = CI + (4 * CINC)
            II        = II + IINC
          ENDDO
#endif

        CASE (32)
#ifdef	__mips
          DO I = 0, NMBR - 1
            IEEEM(II) = CVMGM(.TRUE., .FALSE., CRAYL(CI))
            CI        = CI + CINC
            II        = II + IINC
          ENDDO
#else
          DO I = 0, NMBR - 1, 2
            IEEEI(II) = IOR(CVMGT(B32, B00, CRAYL(CI)),
     1                      CVMGT(B01, B00, CRAYL(CI + CINC)) )
            CI        = CI + (2 * CINC)
            II        = II + IINC
          ENDDO
#endif

        CASE (64)
          DO I = 0, NMBR - 1
#ifdef	__mips
            IEEEL(II) = CVMGM(.TRUE., .FALSE., CRAYL(CI))
#else
            IEEEI(II) = CVMGT(B01, B00, CRAYL(CI))
#endif
            CI        = CI + CINC
            II        = II + IINC
          ENDDO

        END SELECT

        LEFT  = LEFT - NMBR

        IF (BITOFF .NE. 0) THEN       !  Complete strip mine

          IF (IOFF .NE. 0) THEN       !  Complete transfer
            CALL MOVBITZ(TEMP, ZERO, NMBR * ILEN, NIEEE(INDX), IOFF)
            II    = 0
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

        CALL MOVBITZ(CRAYI(0), CCOFF, NMBR * CHLEN, IEEEI(0), ICOFF)

      ELSE

        DO I = 0, NMBR - 1

          CALL MOVBITZ(CRAYI(CI), CCOFF, CHLEN, IEEEI(II), ICOFF)

          CCOFF = CCOFF + (CHLEN * CINC)
          CI    = CI + ISHFT(CCOFF, -WORDSZB)
          CCOFF = MOD(CCOFF, INT(WORDSZ, KIND=8))
          ICOFF = ICOFF + (CHLEN * IINC)
          II    = II + ISHFT(ICOFF, - WORDSZB)
          ICOFF = MOD(ICOFF, INT(WORDSZ, KIND=8))

        ENDDO

      ENDIF
      GOTO 9000

 9010 CONTINUE
      CRY2MIPS = -10              !  FORN must be 64-bit word-aligned
      GOTO 10000

 9009 CONTINUE
      CRY2MIPS = -9               !  NATIVE must be 64-bit word-aligned
      GOTO 10000

 9008 CONTINUE
      CRY2MIPS = -8               !  Parameter error, NATLEN/FORLEN invalid
      GOTO 10000

C9007 CONTINUE
C     CRY2MIPS = -7               !  Unable to allocate memory (unused in
C     GOTO 10000                  !  this version of the routine)

 9006 CONTINUE
      CRY2MIPS = -6               !  Parameter error, invalid FORLEN
      GOTO 10000

 9005 CONTINUE
      CRY2MIPS = -5               !  Parameter error, invalid NATLEN
      GOTO 10000

 9004 CONTINUE
      CRY2MIPS = -4               !  Parameter error, invalid BITOFF
      GOTO 10000

 9003 CONTINUE
      CRY2MIPS = -3               !  Parameter error, invalid NUM
      GOTO 10000

 9002 CONTINUE
      CRY2MIPS = -2               !  Parameter error, invalid TYPE
      GOTO 10000

 9001 CONTINUE
      CRY2MIPS = -1               !  Parameter error
      GOTO 10000

 9000 CONTINUE
      CRY2MIPS = GN32OF + IEEEOF  !  Return error count
C     GOTO 10000

10000 CONTINUE
      RETURN

CDIR$ ID "@(#) libu/ieg/cry2mips.F	92.1	05/25/99 15:15:13"
      END
C
C     Auxiliary functions
C
      LOGICAL FUNCTION CRY2MIPS_AUXILIARY_FUNCTION$()
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
      EXTERNAL CRY2MIPS_INIT
C
#ifdef	__mips
      COMMON /G$CRY2MIPS/ NSIZE, FSIZE
      SAVE   /G$CRY2MIPS/
#else
      COMMON /G@CRY2MIPS/ NSIZE, FSIZE
      SAVE   /G@CRY2MIPS/
#endif
C
      CRY2MIPS_AUXILIARY_FUNCTION$ = .TRUE.
      RETURN
C
      ENTRY CRY2MIPS_NATIVE_TYPE_OK$(NTYPE, NLEN) RESULT (NOK)
      IF (IAND(ISHFT(ONE,((NLEN/8)-1)), NSIZE(NTYPE)) .NE. 0) THEN
        NOK = .TRUE.
      ELSE
        NOK = .FALSE.
      ENDIF
      RETURN
C
      ENTRY CRY2MIPS_FOREIGN_TYPE_OK$(FTYPE, FLEN) RESULT (FOK)
      IF (IAND(ISHFT(ONE,((FLEN/8)-1)), FSIZE(FTYPE)) .NE. 0) THEN
        FOK = .TRUE.
      ELSE
        FOK = .FALSE.
      ENDIF
      RETURN
C
      END
C
      BLOCK DATA CRY2MIPS_INIT
      INTEGER (KIND=8) NSIZE(6)
      INTEGER (KIND=8) FSIZE(6)
#ifdef	__mips
      COMMON /G$CRY2MIPS/ NSIZE, FSIZE
      SAVE   /G$CRY2MIPS/
#else
      COMMON /G@CRY2MIPS/ NSIZE, FSIZE
      SAVE   /G@CRY2MIPS/
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
C
      END BLOCK DATA CRY2MIPS_INIT
