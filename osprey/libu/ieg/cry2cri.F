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

      INTEGER FUNCTION CRY2CRI(
     +TYPE, NUM, FORN, BITOFF, NATIVE, STRIDE, NATLEN, FORLEN, NATIVECH)

      IMPLICIT NONE

C     This function converts CRAY Fortran data types to IEEE Fortran
C     data types.
C
C     The returned function value is as follows:
C
C         <0  Parameter error; no translation performed
C             -1      Parameter error; too few arguments or NATIVECH
C                     not specified with TYPE = 6.
C             -2      Parameter error; invalid TYPE
C             -3      Parameter error; invalid NUM
C             -4      Parameter error; invalid BITOFF
C             -5      Parameter error; invalid NATLEN
C             -6      Parameter error; invalid FORLEN
C             -7      Unable to malloc() memory for translation
C             -8      Combination of NATLEN and FORLEN is invalid
C          0  Translation complete; no errors
C         >0  Translation complete; return value is the number of
C             values that overflowed during translation.

      INTEGER TYPE
      INTENT (IN) TYPE

C         Type code:
C           1 = typeless (no translation); NATLEN must equal FORLEN
C               and be 64, 128 or 256.
C           2 = integer (64-bit twos-complement to 64-, 32-, 16- or
C               8-bit twos-complement)
C           3 = real (64- or 128-bit CRAY floating-point to 32-, 64-
C               or 128-bit IEEE floating-point)
C           4 = complex (2 x 64- or 128-bit CRAY floating-point to
C               2 x 32-, 64- or 128-bit IEEE floating-point).
C           5 = logical (64-bit CRAY minus/positive logical to 64-,
C               32-, 16- or 8-bit zero/nonzero logical).
C           6 = character (ASCII to ASCII; no translation)
C
C         The NATLEN and FORLEN parameters (see below) select the size
C         of the data.

      INTEGER NUM
      INTENT (IN) NUM

C         Number of data items to convert.  Type integer variable,
C         expression, or constant.

      INTEGER FORN(0:*)
#ifndef	_CRAYIEEE
      INTENT (OUT) FORN
#else
      INTENT (IN) FORN
#endif

C         Variable or array of any type (except CHARACTER) or length
C         that contains (or will contain) the foreign data.

      INTEGER BITOFF
      INTENT (IN) BITOFF

C         Bit offset within FORN to begin placing the converted
C         data.  Type integer variable, expression, or constant.
C         Bits are numbered from 0 to 63, beginning at the leftmost
C         bit of FORN.

      INTEGER NATIVE(0:*)
#ifndef	_CRAYIEEE
      INTENT (IN) NATIVE
#else
      INTENT (OUT) NATIVE
#endif

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
#ifndef	_CRAYIEEE
      INTENT (IN) NATIVECH
#else
      INTENT (OUT) NATIVECH
#endif

C         Optional parameter specifying NATIVE character variable if
C         it is of type CHARACTER (TYPE = 6).  This parameter is
C         ignored if TYPE is not 6 (CHARACTER).

C
C     Constants
C
      INTEGER           TEMPSZ
      PARAMETER        (TEMPSZ = 2048) !  Size of temporary buffer

      INTEGER           WORDSZ
      PARAMETER        (WORDSZ = 64)   !  Size of word

      INTEGER           WORDSZM1
      PARAMETER        (WORDSZM1 = WORDSZ - 1)

      INTEGER           WORDSZB
      PARAMETER        (WORDSZB = 6)   !  Log (base 2) of WORDSZ

      INTEGER           CINC, IINC
#ifndef	_CRAYIEEE
      PARAMETER        (IINC = 1)
#else
      PARAMETER        (CINC = 1)
#endif

C     Vector Functions
C
C     These all must be declared of the proper type for the vector
C     loops to work

      INTEGER (KIND=8)  GI8O         ! Nominally: INTEGER (KIND=1)
      INTEGER (KIND=8)  GI16O        ! Nominally: INTEGER (KIND=2)
      INTEGER (KIND=8)  GI32O        ! Nominally: INTEGER (KIND=4)
      INTEGER (KIND=8)  GL8O         ! Nominally: LOGICAL (KIND=1)
      INTEGER (KIND=8)  GL16O        ! Nominally: LOGICAL (KIND=2)
      INTEGER (KIND=8)  GL32O        ! Nominally: LOGICAL (KIND=4)
      INTEGER (KIND=8)  IEF32O       ! Nominally: REAL (KIND=4)
      REAL    (KIND=8)  IEF64O
      REAL    (KIND=16) IEF128O
      INTEGER (KIND=8)  IED32O       ! Nominally: REAL (KIND=4)
      REAL    (KIND=8)  IED64O
      REAL    (KIND=16) IED128O
      INTEGER (KIND=8)  IEC64O       ! Nominally: COMPLEX (KIND=4)
      INTEGER (KIND=8)  IECD64O      ! Nominally: COMPLEX (KIND=4)

#ifdef	_MAXVL
CDIR$ VFUNCTION GI8O
CDIR$ VFUNCTION GI16O
CDIR$ VFUNCTION GI32O
CDIR$ VFUNCTION GL8O
CDIR$ VFUNCTION GL16O
CDIR$ VFUNCTION GL32O
CDIR$ VFUNCTION IEF32O
CDIR$ VFUNCTION IEF64O
CDIR$ VFUNCTION IEF128O
CDIR$ VFUNCTION IED32O
CDIR$ VFUNCTION IED64O
CDIR$ VFUNCTION IED128O
CDIR$ VFUNCTION IEC64O
CDIR$ VFUNCTION IECD64O
#endif

C     VFUNCTION overflow/underflow counts and control flags

      INTEGER           GN32OF
      INTEGER           DENORM, IEEEOF
      TASK COMMON  /T@GENERIC/ GN32OF
      TASK COMMON  /T@IEEE/  DENORM, IEEEOF

C     Aliasing pointers

      INTEGER (KIND=8)  CRAYI(0:*), IEEEI(0:*)
      REAL    (KIND=8)  CRAYR(0:*), IEEER(0:*)
      REAL    (KIND=16) CRAYD(0:*), IEEED(0:*)
      COMPLEX (KIND=8)  CRAYC(0:*), IEEEC(0:*)
      COMPLEX (KIND=16) CRAYE(0:*), IEEEE(0:*)

      POINTER (CIPTR, CRAYI), (IIPTR, IEEEI)
      POINTER (CRPTR, CRAYR), (IRPTR, IEEER)
      POINTER (CDPTR, CRAYD), (IDPTR, IEEED)
      POINTER (CCPTR, CRAYC), (ICPTR, IEEEC)
      POINTER (CEPTR, CRAYE), (IEPTR, IEEEE)

      INTEGER CHLEN, CHOFF, CI, CLEN, CLENB, COFF, II, ILEN, ILENB, IOFF
      INTEGER BITO, CHNK, I, IBTL, INDX, IWAD, LEFT, LOGB2
      INTEGER NMBR, REMR, SCAL, SCALB
      INTEGER (KIND=8) TEMP(0:TEMPSZ-1), ITMP(0:6)
      LOGICAL CRY2CRI_NATIVE_TYPE_OK$, CRY2CRI_FOREIGN_TYPE_OK$

      EXTERNAL MOVBITZ
      EXTERNAL CRY2CRI_NATIVE_TYPE_OK$, CRY2CRI_FOREIGN_TYPE_OK$

C     Statement function to compute the Log (base 2) of I

      LOGB2(I) = WORDSZM1 - LEADZ(I)

C     Check argument validity

      IF (NUMARG() .LT. 8) GOTO 9001

      IF (NUM .LT. 0) GOTO 9003

      IF ((BITOFF .LT. 0) .OR. (BITOFF .GE. WORDSZ)) GOTO 9004

      IF ((TYPE .LT. 1) .OR. (TYPE .GT. 6)) GOTO 9002

C     Ensure lengths are powers of two and otherwise valid

      IF (POPCNT(NATLEN) .NE. 1) GOTO 9005
      IF (.NOT. CRY2CRI_NATIVE_TYPE_OK$(TYPE, NATLEN)) GOTO 9005

      IF (POPCNT(FORLEN) .NE. 1) GOTO 9006
      IF (.NOT. CRY2CRI_FOREIGN_TYPE_OK$(TYPE, FORLEN)) GOTO 9006

      CI     = 0                        !  Set indicies
      II     = 0
      GN32OF = 0                        !  Clear overflow counts
      IEEEOF = 0
      NMBR   = NUM

      IF (NMBR .EQ. 0) GOTO 9000

#ifndef	_CRAYIEEE
      CIPTR  = LOC(NATIVE(0))
      CRPTR  = LOC(NATIVE(0))
      CDPTR  = LOC(NATIVE(0))
      CCPTR  = LOC(NATIVE(0))
      CEPTR  = LOC(NATIVE(0))

      IIPTR  = LOC(FORN(0))
      IRPTR  = LOC(FORN(0))
      IDPTR  = LOC(FORN(0))
      ICPTR  = LOC(FORN(0))
      IEPTR  = LOC(FORN(0))

      CINC   = STRIDE
      CLEN   = NATLEN                   !  Set lengths and bit offsets
      CLENB  = LOGB2(CLEN)              !  Log (base 2) of CLEN
      COFF   = 0
      ILEN   = FORLEN
      ILENB  = LOGB2(ILEN)              !  Log (base 2) of ILEN
      IOFF   = BITOFF
      BITO   = BITOFF
#else
      CIPTR  = LOC(FORN(0))
      CRPTR  = LOC(FORN(0))
      CDPTR  = LOC(FORN(0))
      CCPTR  = LOC(FORN(0))
      CEPTR  = LOC(FORN(0))

      IIPTR  = LOC(NATIVE(0))
      IRPTR  = LOC(NATIVE(0))
      IDPTR  = LOC(NATIVE(0))
      ICPTR  = LOC(NATIVE(0))
      IEPTR  = LOC(NATIVE(0))

      CLEN   = FORLEN                   !  Set lengths and bit offsets
      CLENB  = LOGB2(CLEN)              !  Log (base 2) of CLEN
      COFF   = BITOFF
      BITO   = BITOFF
      IINC   = STRIDE
      ILEN   = NATLEN
      ILENB  = LOGB2(ILEN)              !  Log (base 2) of ILEN
      IOFF   = 0
#endif

      GOTO (100,200,300,400,500,600), TYPE

C     TYPE = 1  (typeless)                                  ************
C
C     Convert CRAY typeless to GENERIC typeless
C
C     CRAY              |                   GENERIC
C                       |    64-bit         128-bit          256-bit
C     ------------------+-----------------------------------------------
C     64-bit            |no translation |   INVALID     |    INVALID
C                       +---------------+---------------+---------------
C     128-bit           |   INVALID     |no translation |    INVALID
C                       +---------------+---------------+---------------
C     256-bit           |   INVALID     |   INVALID     |no translation
C                        -----------------------------------------------

  100 CONTINUE

      IF (CLEN .NE. ILEN) GOTO 9008

C     TYPE = 2  (integer)                                   ************
C
C     Convert CRAY integers to GENERIC integers
C
C     CRAY              |                  GENERIC
C                       |   8-bit     16-bit     32-bit     64-bit
C     ------------------+-----------------------------------------------
C     64-bit INTEGER    |    GI8O  |   GI16O  |   GI32O  |no translation
C                        -----------------------------------------------

  200 CONTINUE

C     Special case a simple block move

      IF (STRIDE .EQ. 1 .AND. ILEN .GE. WORDSZ) THEN

        CALL MOVBITZ(CRAYI(0), COFF, NMBR * ILEN, IEEEI(0), IOFF)
        GOTO 9000

      ENDIF

C     Do odd partial-word element(s), if extant

      IF (ILEN .LT. WORDSZ) THEN

        SCAL  = ISHFT(WORDSZ, -ILENB)    ! 64 / ILEN
        SCALB = LOGB2(SCAL)              ! Log (base 2) of SCAL
        REMR  = IAND(NMBR, SCAL - 1)     ! Remainder (NMBR mod SCAL)

        IF (REMR .GT. 0) THEN

          NMBR  = NMBR - REMR
          INDX  = ISHFT(NMBR, -SCALB)
          ITMP  = 0

#ifndef	_CRAYIEEE
CDIR$     SHORTLOOP
          DO I = 0, REMR - 1
            ITMP(I) = CRAYI((NMBR + I) * CINC)
          ENDDO
#else
          CALL MOVBITZ(FORN(INDX), BITOFF, CLEN * REMR, ITMP(0), 0)
#endif

          SELECT CASE (ILEN)

            CASE (8)
CDIR$         SHORTLOOP
              DO I = 0, REMR - 1
                ITMP(I) = GI8O (ITMP(I))
              ENDDO

              TEMP(0) = IOR(IOR(IOR(ISHFT(ITMP(0), 56),
     1                              ISHFT(ITMP(1), 48)),
     2                          IOR(ISHFT(ITMP(2), 40),
     3                              ISHFT(ITMP(3), 32))),
     4                      IOR(IOR(ISHFT(ITMP(4), 24),
     5                              ISHFT(ITMP(5), 16)),
     6                              ISHFT(ITMP(6),  8)))

            CASE (16)
CDIR$         SHORTLOOP
              DO I = 0, REMR - 1
                ITMP(I) = GI16O(ITMP(I))
              ENDDO

              TEMP(0) = IOR(IOR(ISHFT(ITMP(0), 48),
     1                          ISHFT(ITMP(1), 32)),
     2                          ISHFT(ITMP(2), 16))

            CASE (32)
              TEMP(0) = ISHFT(GI32O(ITMP(0)), 32)

          END SELECT

          CALL MOVBITZ(TEMP(0), 0, ILEN * REMR, FORN(INDX), BITOFF)

        ENDIF

      ENDIF

C     If data is not word-aligned, then it must be run through a
C     temporary working array to align it for the VFUNCTIONS.

      LEFT  = NMBR

      IF (BITOFF .NE. 0) THEN     !  Strip mine

#ifndef	_CRAYIEEE
        IIPTR = LOC(TEMP(0))
        IRPTR = LOC(TEMP(0))
        IDPTR = LOC(TEMP(0))
        ICPTR = LOC(TEMP(0))
        IEPTR = LOC(TEMP(0))
        CHNK  = ISHFT(ISHFT(TEMPSZ, WORDSZB), -ILENB)
#else
        CIPTR = LOC(TEMP(0))
        CRPTR = LOC(TEMP(0))
        CDPTR = LOC(TEMP(0))
        CCPTR = LOC(TEMP(0))
        CEPTR = LOC(TEMP(0))
        CHNK  = ISHFT(ISHFT(TEMPSZ, WORDSZB), -CLENB)
#endif

        NMBR  = MOD(NMBR, CHNK)

        IF (NMBR .EQ. 0) NMBR  = CHNK    !  Size of first chunk

        INDX  = 0

      ENDIF

C
C     Main loop for Typeless and Integer conversions
C

      DO WHILE (LEFT .GT. 0)

#ifdef	_CRAYIEEE
        IF (BITOFF .NE. 0) THEN
          CALL MOVBITZ(FORN(INDX), BITOFF, NMBR * CLEN, TEMP(0), 0)
          CI    = 0
        ENDIF
#endif

        SELECT CASE (ILEN)

          CASE (8)
            DO I = 0, NMBR - 1, 8
            IEEEI(II) = IOR(IOR(IOR(ISHFT(GI8O(CRAYI(CI       )), 56),
     1                              ISHFT(GI8O(CRAYI(CI+CINC  )), 48)),
     2                          IOR(ISHFT(GI8O(CRAYI(CI+CINC*2)), 40),
     3                              ISHFT(GI8O(CRAYI(CI+CINC*3)), 32))),
     4                      IOR(IOR(ISHFT(GI8O(CRAYI(CI+CINC*4)), 24),
     5                              ISHFT(GI8O(CRAYI(CI+CINC*5)), 16)),
     6                          IOR(ISHFT(GI8O(CRAYI(CI+CINC*6)),  8),
     7                                    GI8O(CRAYI(CI+CINC*7))    )))
            CI        = CI + (8 * CINC)
            II        = II + IINC
            ENDDO

          CASE (16)
            DO I = 0, NMBR - 1, 4
              IEEEI(II) = IOR(IOR(ISHFT(GI16O(CRAYI(CI       )), 48),
     1                            ISHFT(GI16O(CRAYI(CI+CINC  )), 32)),
     2                        IOR(ISHFT(GI16O(CRAYI(CI+CINC*2)), 16),
     3                                  GI16O(CRAYI(CI+CINC*3))    ))
              CI       = CI + (4 * CINC)
              II       = II + IINC
            ENDDO

          CASE (32)
            DO I = 0, NMBR - 1, 2
              IEEEI(II) = IOR(ISHFT(GI32O(CRAYI(CI       )), 32),
     1                              GI32O(CRAYI(CI + CINC)))
              CI        = CI + (2 * CINC)
              II        = II + IINC
            ENDDO

          CASE (64)
            DO I = 0, NMBR - 1
              IEEEI(II) = CRAYI(CI)
              CI        = CI + CINC
              II        = II + IINC
            ENDDO

          CASE (128)
            DO I = 0, NMBR - 1
              IEEED(II) = CRAYD(CI)
              CI        = CI + CINC
              II        = II + IINC
            ENDDO

          CASE (256)
            DO I = 0, NMBR - 1
              IEEEE(II) = CRAYE(CI)
              CI        = CI + CINC
              II        = II + IINC
            ENDDO

        END SELECT

        LEFT  = LEFT - NMBR

        IF (BITOFF .NE. 0) THEN     !  Complete strip mine

#ifndef	_CRAYIEEE
          CALL MOVBITZ(TEMP(0), 0, NMBR * ILEN, FORN(INDX), BITOFF)
          II    = 0
#endif

          INDX  = INDX + NMBR
          NMBR  = CHNK
        ENDIF

      ENDDO

      GOTO 9000

C     TYPE = 3  (real)                                      ************
C
C     Convert CRAY floating-point numbers to IEEE floating-point numbers
C
C     CRAY              |                     IEEE
C                       |    32-bit          64-bit          128-bit
C     ------------------+-----------------------------------------------
C     64-bit (REAL)     |    IEF32O     |    IEF64O     |    IEF128O
C                       +---------------+---------------+---------------
C     128-bit (DOUBLE)  |    IED32O     |    IED64O     |    IED128O
C                        -----------------------------------------------
C
C     Note: IEF32O and IED32O are identical, since there are more than
C     enough bits in a CRAY single-precision number.

  300 CONTINUE

C     Do odd half-word element, if extant

      IF (ILEN .EQ. 32 .AND. IAND(NMBR, 1) .NE. 0) THEN

        NMBR    = NMBR - 1
        INDX    = ISHFT(NMBR, -1) * IINC

#ifndef	_CRAYIEEE
        TEMP(0) = IEF32O(CRAYI(NMBR * CINC))
#else
        IF (COFF .EQ. 0) THEN
          TEMP(0) = IEF32O(CRAYI(NMBR * CINC))
        ELSE
          CALL MOVBITZ(CRAYI(NMBR * CINC), COFF, CLEN, TEMP(1), 0)
          TEMP(0) = IEF32O(TEMP(1))
        ENDIF
#endif

        CALL MOVBITZ(TEMP(0), ILEN, ILEN, IEEEI(INDX), IOFF)

      ENDIF

C     If data is not word-aligned, then it must be run through a
C     temporary working array to align it for the VFUNCTIONS.

      LEFT  = NMBR

      IF (BITOFF .NE. 0) THEN     !  Strip mine

#ifndef	_CRAYIEEE
        IIPTR = LOC(TEMP(0))
        IRPTR = LOC(TEMP(0))
        IDPTR = LOC(TEMP(0))
        ICPTR = LOC(TEMP(0))
        IEPTR = LOC(TEMP(0))
        CHNK  = ISHFT(ISHFT(TEMPSZ, WORDSZB), -ILENB)
#else
        CIPTR = LOC(TEMP(0))
        CRPTR = LOC(TEMP(0))
        CDPTR = LOC(TEMP(0))
        CCPTR = LOC(TEMP(0))
        CEPTR = LOC(TEMP(0))
        CHNK  = ISHFT(ISHFT(TEMPSZ, WORDSZB), -CLENB)
#endif

        NMBR  = MOD(NMBR, CHNK)

        IF (NMBR .EQ. 0) NMBR  = CHNK   !  Size of first chunk

        INDX  = 0
      ENDIF

C
C     Main loop for Real (floating-point) conversions
C

      DO WHILE (LEFT .GT. 0)

#ifdef  _CRAYIEEE
        IF (BITOFF .NE. 0) THEN
          CALL MOVBITZ(FORN(INDX), BITOFF, NMBR * CLEN, TEMP(0), 0)
          CI    = 0
        ENDIF
#endif

        IF (CLEN .EQ. 64) THEN

          SELECT CASE (ILEN)

            CASE (32)
              DO I = 0, NMBR - 1, 2
                IEEEI(II) = IEC64O(CMPLX(CRAYR(CI), CRAYR(CI + CINC)))
C                          (use complex function to do two at a time)
                CI        = CI + (2 * CINC)
                II        = II + IINC
              ENDDO

            CASE (64)
              DO I = 0, NMBR - 1
                IEEER(II) = IEF64O(CRAYR(CI))
                CI        = CI + CINC
                II        = II + IINC
              ENDDO

            CASE (128)
              DO I = 0, NMBR - 1
                IEEED(II) = IEF128O(CRAYR(CI))
                CI        = CI + CINC
                II        = II + IINC
              ENDDO

          END SELECT

        ELSE                        !  CLEN .EQ. 128

          SELECT CASE (ILEN)

            CASE (32)
              DO I = 0, NMBR - 1, 2
                IEEEI(II) = IECD64O(CMPLX(CRAYD(CI), CRAYD(CI + CINC)))
C                          (use complex function to do two at a time)
                CI        = CI + (2 * CINC)
                II        = II + IINC
              ENDDO

            CASE (64)
              DO I = 0, NMBR - 1
                IEEER(II) = IED64O(CRAYD(CI))
                CI        = CI + CINC
                II        = II + IINC
              ENDDO

            CASE (128)
              DO I = 0, NMBR - 1
                IEEED(II) = IED128O(CRAYD(CI))
                CI        = CI + CINC
                II        = II + IINC
              ENDDO

          END SELECT

        ENDIF

        LEFT  = LEFT - NMBR

        IF (BITOFF .NE. 0) THEN     !  Complete strip mine

#ifndef	_CRAYIEEE
          CALL MOVBITZ(TEMP(0), 0, NMBR * ILEN, FORN(INDX), BITOFF)
          II    = 0
#endif

          INDX  = INDX + NMBR
          NMBR  = CHNK
        ENDIF

      ENDDO

      GOTO 9000

C     TYPE = 4  (complex)                                   ************
C
C     Convert CRAY floating-point numbers to IEEE floating-point numbers
C
C     CRAY              |                     IEEE
C                       |   2x32-bit        2x64-bit        2x128-bit
C     ------------------+-----------------------------------------------
C     2x64-bit (COMPLEX)|    IEC64O     |   2xIEF64O    |   2xIEF128O
C                       +---------------+---------------+---------------
C     2x128-bit (DOUBLE |   ICED64O     |   2xIED64O    |   2xIED128O
C              COMPLEX)  -----------------------------------------------

  400 CONTINUE

C     If data is not word-aligned, then it must be run through a
C     temporary working array to align it for the VFUNCTIONS.

      LEFT  = NMBR

      IF (BITOFF .NE. 0) THEN     !  Strip mine

#ifndef	_CRAYIEEE
        IIPTR = LOC(TEMP(0))
        IRPTR = LOC(TEMP(0))
        IDPTR = LOC(TEMP(0))
        ICPTR = LOC(TEMP(0))
        IEPTR = LOC(TEMP(0))
        CHNK  = ISHFT(ISHFT(TEMPSZ, WORDSZB), -ILENB)
#else
        CIPTR = LOC(TEMP(0))
        CRPTR = LOC(TEMP(0))
        CDPTR = LOC(TEMP(0))
        CCPTR = LOC(TEMP(0))
        CEPTR = LOC(TEMP(0))
        CHNK  = ISHFT(ISHFT(TEMPSZ, WORDSZB), -CLENB)
#endif

        NMBR  = MOD(NMBR, CHNK)

        IF (NMBR .EQ. 0) NMBR  = CHNK   !  Size of first chunk

        INDX  = 0

      ENDIF

C
C     Main loop for Complex conversions
C

      DO WHILE (LEFT .GT. 0)

#ifdef  _CRAYIEEE
        IF (BITOFF .NE. 0) THEN
          CALL MOVBITZ(FORN(INDX), BITOFF, NMBR * CLEN, TEMP(0), 0)
          CI    = 0
        ENDIF
#endif

        IF (CLEN .EQ. 128) THEN

          SELECT CASE (ILEN)

            CASE (64)
              DO I = 0, NMBR - 1
                IEEEI(II) = IEC64O(CRAYC(CI))
                CI        = CI + CINC
                II        = II + IINC
              ENDDO

            CASE (128)
              DO I = 0, NMBR - 1
                IEEEC(II) = CMPLX(IEF64O(REAL (CRAYC(CI))),
     1                            IEF64O(AIMAG(CRAYC(CI))))
                CI        = CI + CINC
                II        = II + IINC
              ENDDO

            CASE (256)
              DO I = 0, NMBR - 1
                IEEEE(II) = CMPLX(IEF128O(REAL (CRAYC(CI))),
     1                            IEF128O(AIMAG(CRAYC(CI))),
     2                            KIND=16)
                CI        = CI + CINC
                II        = II + IINC
              ENDDO

          END SELECT

        ELSE                        !  CLEN .EQ. 256

          SELECT CASE (ILEN)

            CASE (64)
              DO I = 0, NMBR - 1
                IEEEI(II) = IECD64O(CRAYE(CI))
                CI        = CI + CINC
                II        = II + IINC
              ENDDO

            CASE (128)
              DO I = 0, NMBR - 1
                IEEEC(II) = CMPLX(IED64O(REAL (CRAYE(CI))),
     1                            IED64O(AIMAG(CRAYE(CI))))
                CI        = CI + CINC
                II        = II + IINC
              ENDDO

            CASE (256)
              DO I = 0, NMBR - 1
                IEEEE(II) = CMPLX(IED128O(REAL (CRAYE(CI))),
     1                            IED128O(AIMAG(CRAYE(CI))),
     2                            KIND=16)
                CI        = CI + CINC
                II        = II + IINC
              ENDDO

          END SELECT

        ENDIF

        LEFT  = LEFT - NMBR

        IF (BITOFF .NE. 0) THEN     !  Complete strip mine

#ifndef	_CRAYIEEE
          CALL MOVBITZ(TEMP(0), 0, NMBR * ILEN, FORN(INDX), BITOFF)
          II    = 0
#endif

          INDX  = INDX + NMBR
          NMBR  = CHNK
        ENDIF

      ENDDO

      GOTO 9000

C     TYPE = 5  (logical)                                   ************
C
C     Convert CRAY logicals to GENERIC logicals
C
C     CRAY              |                  GENERIC
C                       |   8-bit     16-bit     32-bit     64-bit
C     ------------------+-----------------------------------------------
C     64-bit LOGICAL    |    GL8O  |   GL16O  |   GL32O  |  inline
C                        -----------------------------------------------

  500 CONTINUE

C     Do odd partial-word element(s), if extant

      IF (ILEN .LT. WORDSZ) THEN

        SCAL  = ISHFT(WORDSZ, -ILENB)    ! 64 / ILEN
        SCALB = LOGB2(SCAL)              ! Log (base 2) of SCAL
        REMR  = IAND(NMBR, SCAL - 1)     ! Remainder (NMBR mod SCAL)

        IF (REMR .GT. 0) THEN

          NMBR  = NMBR - REMR
          INDX  = ISHFT(NMBR, -SCALB)
          ITMP  = 0

#ifndef _CRAYIEEE
CDIR$     SHORTLOOP
          DO I = 0, REMR - 1
            ITMP(I) = CRAYI((NMBR + I) * CINC)
          ENDDO
#else
          CALL MOVBITZ(FORN(INDX), BITOFF, CLEN * REMR, ITMP(0), 0)
#endif

          SELECT CASE (ILEN)

            CASE (8)
CDIR$         SHORTLOOP
              DO I = 0, REMR - 1
                ITMP(I) = GL8O (ITMP(I))
              ENDDO

              TEMP(0) = IOR(IOR(IOR(ISHFT(ITMP(0), 56),
     1                              ISHFT(ITMP(1), 48)),
     2                          IOR(ISHFT(ITMP(2), 40),
     3                              ISHFT(ITMP(3), 32))),
     4                      IOR(IOR(ISHFT(ITMP(4), 24),
     5                              ISHFT(ITMP(5), 16)),
     6                              ISHFT(ITMP(6),  8)))

            CASE (16)
CDIR$         SHORTLOOP
              DO I = 0, REMR - 1
                ITMP(I) = GL16O(ITMP(I))
              ENDDO

              TEMP(0) = IOR(IOR(ISHFT(ITMP(0), 48),
     1                          ISHFT(ITMP(1), 32)),
     2                          ISHFT(ITMP(2), 16))

            CASE (32)
              TEMP(0) = ISHFT(GL32O(ITMP(0)), 32)

          END SELECT

          CALL MOVBITZ(TEMP(0), 0, ILEN * REMR, FORN(INDX), BITOFF)

        ENDIF

      ENDIF

C     If data is not word-aligned, then it must be run through a
C     temporary working array to align it for the VFUNCTIONS.

      LEFT  = NMBR

      IF (BITOFF .NE. 0) THEN     !  Strip mine

#ifndef	_CRAYIEEE
        IIPTR = LOC(TEMP(0))
        IRPTR = LOC(TEMP(0))
        IDPTR = LOC(TEMP(0))
        ICPTR = LOC(TEMP(0))
        IEPTR = LOC(TEMP(0))
        CHNK  = ISHFT(ISHFT(TEMPSZ, WORDSZB), -ILENB)
#else
        CIPTR = LOC(TEMP(0))
        CRPTR = LOC(TEMP(0))
        CDPTR = LOC(TEMP(0))
        CCPTR = LOC(TEMP(0))
        CEPTR = LOC(TEMP(0))
        CHNK  = ISHFT(ISHFT(TEMPSZ, WORDSZB), -CLENB)
#endif

        NMBR  = MOD(NMBR, CHNK)

        IF (NMBR .EQ. 0) NMBR  = CHNK   !  Size of first chunk

        INDX  = 0

      ENDIF

C
C     Main loop for Logical conversions
C

      DO WHILE (LEFT .GT. 0)

#ifdef  _CRAYIEEE
        IF (BITOFF .NE. 0) THEN
          CALL MOVBITZ(FORN(INDX), BITOFF, NMBR * CLEN, TEMP(0), 0)
          CI    = 0
        ENDIF
#endif

        SELECT CASE (ILEN)

          CASE (8)
            DO I = 0, NMBR - 1, 8
            IEEEI(II) = IOR(IOR(IOR(ISHFT(GL8O(CRAYI(CI       )), 56),
     1                              ISHFT(GL8O(CRAYI(CI+CINC  )), 48)),
     2                          IOR(ISHFT(GL8O(CRAYI(CI+CINC*2)), 40),
     3                              ISHFT(GL8O(CRAYI(CI+CINC*3)), 32))),
     4                      IOR(IOR(ISHFT(GL8O(CRAYI(CI+CINC*4)), 24),
     5                              ISHFT(GL8O(CRAYI(CI+CINC*5)), 16)),
     6                          IOR(ISHFT(GL8O(CRAYI(CI+CINC*6)),  8),
     7                                    GL8O(CRAYI(CI+CINC*7))    )))
            CI        = CI + (8 * CINC)
            II        = II + IINC
            ENDDO

          CASE (16)
            DO I = 0, NMBR - 1, 4
              IEEEI(II) = IOR(IOR(ISHFT(GL16O(CRAYI(CI       )), 48),
     1                            ISHFT(GL16O(CRAYI(CI+CINC  )), 32)),
     2                        IOR(ISHFT(GL16O(CRAYI(CI+CINC*2)), 16),
     3                                  GL16O(CRAYI(CI+CINC*3))    ))
              CI        = CI + (4 * CINC)
              II        = II + IINC
            ENDDO

          CASE (32)
            DO I = 0, NMBR - 1, 2
              IEEEI(II) = IOR(ISHFT(GL32O(CRAYI(CI       )), 32),
     1                              GL32O(CRAYI(CI + CINC)))
              CI        = CI + (2 * CINC)
              II        = II + IINC
            ENDDO

          CASE (64)
            DO I = 0, NMBR - 1
              IEEEI(II) = ISHFT(CRAYI(CI), -WORDSZM1)
              CI        = CI + CINC
              II        = II + IINC
            ENDDO

        END SELECT

        LEFT  = LEFT - NMBR

        IF (BITOFF .NE. 0) THEN     !  Complete strip mine

#ifndef	_CRAYIEEE
          CALL MOVBITZ(TEMP(0), 0, NMBR * ILEN, FORN(INDX), BITOFF)
          II    = 0
#endif

          INDX  = INDX + NMBR
          NMBR  = CHNK
        ENDIF

      ENDDO

      GOTO 9000

C     TYPE = 6  (character)                                 ************

  600 CONTINUE

      IF (CLEN .NE. ILEN) GOTO 9008

      CALL G@CHRPCK(NATIVECH, IWAD, IBTL, CHOFF)  ! Get character offset

#ifndef	_CRAYIEEE
      CIPTR = IWAD
      COFF  = CHOFF
#else
      IIPTR = IWAD
      IOFF  = CHOFF
#endif

      CHLEN = ILEN * LEN(NATIVECH(0))             ! Get character length

      IF (STRIDE .EQ. 1) THEN

        CALL MOVBITZ(CRAYI(0), COFF, NMBR * CHLEN, IEEEI(0), IOFF)

      ELSE

        DO I = 0, NMBR - 1

          CALL MOVBITZ(CRAYI(CI), COFF, CHLEN, IEEEI(II), IOFF)

          CHOFF = CHOFF + (CHLEN * STRIDE)
          IWAD  = IWAD + ISHFT(CHOFF, -WORDSZB)
          CHOFF = MOD(CHOFF, WORDSZ)

#ifndef	_CRAYIEEE
          CIPTR = IWAD
          COFF  = CHOFF
          II    = II + IINC
#else
          IIPTR = IWAD
          IOFF  = CHOFF
          CI    = CI + CINC
#endif

        ENDDO

      ENDIF
      GOTO 9000

 9008 CONTINUE
      CRY2CRI = -8             !  Parameter error, NATLEN/FORLEN invalid
      GOTO 10000

C9007 CONTINUE
C     CRY2CRI = -7             !  Unable to allocate memory (unused in
C     GOTO 10000               !  this version of the routine)

 9006 CONTINUE
      CRY2CRI = -6             !  Parameter error, invalid FORLEN
      GOTO 10000

 9005 CONTINUE
      CRY2CRI = -5             !  Parameter error, invalid NATLEN
      GOTO 10000

 9004 CONTINUE
      CRY2CRI = -4             !  Parameter error, invalid BITOFF
      GOTO 10000

 9003 CONTINUE
      CRY2CRI = -3             !  Parameter error, invalid NUM
      GOTO 10000

 9002 CONTINUE
      CRY2CRI = -2             !  Parameter error, invalid TYPE
      GOTO 10000

 9001 CONTINUE
      CRY2CRI = -1             !  Parameter error
      GOTO 10000

 9000 CONTINUE
      CRY2CRI = GN32OF + IEEEOF  !  Return error count
C     GOTO 10000

10000 CONTINUE
      RETURN

CDIR$ ID "@(#) libu/ieg/cry2cri.F	92.0	10/08/98 14:57:41"
      END
C
C     Auxiliary functions
C
      LOGICAL FUNCTION CRY2CRI_AUXILIARY_FUNCTION$()
      LOGICAL NOK, FOK
      INTEGER NTYPE, NLEN, FTYPE, FLEN
C
C     Configuration information for these routines
C
      INTEGER NSIZE(6)
      INTEGER FSIZE(6)
      COMMON /G@CRY2CRI/ NSIZE, FSIZE
      SAVE   /G@CRY2CRI/
C
C     The following masks indicate the allowable values for the NATLEN
C     and FORLEN arguments.  External routines (e.g., implicit data
C     conversion) can query these values to determine acceptable sizes.
C     Each bit corresponds to a accepted size (in bytes).  Thus, the
C     rightmost bit (bit 1), if set, indicates that a length of 1 byte
C     (8 bits) is valid.
C
#ifndef	_CRAYIEEE
      DATA NSIZE /
#else
      DATA FSIZE /
#endif
     1            20000100200B,   ! Typeless:  256-, 128- and 64-bit
     2                    200B,   ! Integer:  64-bit
     3                 100200B,   ! Real:  128- and 64-bit
     4            20000100000B,   ! Complex:  256- and 128-bit
     5                    200B,   ! Logical:  64-bit
     6                    001B/   ! Character:  8-bit
C
#ifndef	_CRAYIEEE
      DATA FSIZE /
#else
      DATA NSIZE /
#endif
     1            20000100200B,   ! Typeless:  256-, 128- and 64-bit
     2                    213B,   ! Integer:  64-, 32-, 16- and 8-bit
     3                 100210B,   ! Real:  128-, 64- and 32-bit
     4            20000100200B,   ! Complex:  256-, 128- and 64-bit
     5                    213B,   ! Logical:  64-, 32-, 16- and 8-bit
     6                    001B/   ! Character:  8-bit
C
      CRY2CRI_AUXILIARY_FUNCTION$ = .TRUE.
      RETURN
C
      ENTRY CRY2CRI_NATIVE_TYPE_OK$(NTYPE, NLEN) RESULT (NOK)
      IF (IAND(ISHFT(1,((NLEN/8)-1)), NSIZE(NTYPE)) .NE. 0) THEN
        NOK = .TRUE.
      ELSE
        NOK = .FALSE.
      ENDIF
      RETURN
C
      ENTRY CRY2CRI_FOREIGN_TYPE_OK$(FTYPE, FLEN) RESULT (FOK)
      IF (IAND(ISHFT(1,((FLEN/8)-1)), FSIZE(FTYPE)) .NE. 0) THEN
        FOK = .TRUE.
      ELSE
        FOK = .FALSE.
      ENDIF
      RETURN
C
      END
