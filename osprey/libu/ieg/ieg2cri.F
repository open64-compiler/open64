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

      INTEGER FUNCTION IEG2CRI(
     +TYPE, NUM, FORN, BITOFF, NATIVE, STRIDE, NATLEN, FORLEN, NATIVECH)

      IMPLICIT NONE

C     This function converts GENERIC IEEE Fortran data types to CRAY IEEE
C     Fortran data types.
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
C             -9      NATIVE must be 64-bit word-aligned (MPP only)
C             -10     FORN must be 64-bit word-aligned (MPP only)
C          0  Translation complete; no errors
C         >0  Translation complete; return value is the number of
C             values that overflowed during translation.

      INTEGER TYPE
      INTENT (IN) TYPE

C         Type code:
C           1 = typeless (no translation); NATLEN must equal FORLEN
C               and be 32, 64, 128 or 256.  32-bit typeless is not
C               supported on CRAY PVP systems and 256-bit typeless is
C               not supported on CRAY MPP systems.
C           2 = integer (8-, 16-, 32- or 64-bit twos-complement to 32-,
C               or 64-bit twos-complement)
C           3 = real (32-, 64- or 128-bit IEEE floating-point to 32-,
C               64- or 128-bit IEEE floating-point)
C           4 = complex (2 x 32-, 64- or 128-bit IEEE floating-point
C               to 2 x 32-, 64- or 128-bit CRAY IEEE floating-point).
C           5 = logical (8-, 16-, 32- or 64-bit zero/nonzero logical to
C               32- or 64-bit zero/nonzero logical).
C           6 = character (ASCII to ASCII; no translation)
C
C         The NATLEN and FORLEN parameters (see below) select the size
C         of the data.

      INTEGER NUM
      INTENT (IN) NUM

C         Number of data items to convert.  Type integer variable,
C         expression, or constant.

      INTEGER (KIND=4) FORN(0:*)
      INTENT (IN) FORN

C         Variable or array of any type (except CHARACTER) or length
C         that contains (or will contain) the foreign data.

      INTEGER BITOFF
      INTENT (IN) BITOFF

C         Bit offset within FORN to begin placing the converted
C         data.  Type integer variable, expression, or constant.
C         Bits are numbered from 0 to 63, beginning at the leftmost
C         bit of FORN.

      INTEGER (KIND=4) NATIVE(0:*)
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

C         Optional parameter specifying NATIVE character variable if
C         it is of type CHARACTER (TYPE = 6).  This parameter is
C         ignored if TYPE is not 6 (CHARACTER).

C
C     Constants
C
      INTEGER           GRANSZ
#ifdef	_CRAYMPP
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
      PARAMETER        (IINC = 1)

      INTEGER           COFF, IOFF
      PARAMETER        (COFF = 0)

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

      INTEGER (KIND=8)  GI8I$     !  Actually, INTEGER(KIND=1)
      INTEGER (KIND=8)  GI16I$    !  Actually, INTEGER(KIND=2)
      INTEGER (KIND=8)  GI32I$
#ifdef	_CRAYMPP
      INTEGER (KIND=4)  GI32O$
      REAL    (KIND=4)  IEEESS$
      REAL    (KIND=4)  IEEEDS$
      REAL    (KIND=4)  IEEEQS$
#else
      INTEGER (KIND=8)  GI32O$    !  Actually, INTEGER (KIND=4)
      INTEGER (KIND=8)  IEEESS$   !  Actually, REAL (KIND=4)
      INTEGER (KIND=8)  IEEEDS$   !  Actually, REAL (KIND=4)
      INTEGER (KIND=8)  IEEEQS$   !  Actually, REAL (KIND=4)
#endif
      REAL    (KIND=8)  IEEESD$
      REAL    (KIND=8)  IEEEDD$
      REAL    (KIND=8)  IEEEQD$
#ifdef	_CRAYMPP
      COMPLEX (KIND=8) 	IEEEDQ$   ! Actually, REAL(KIND=16)
      COMPLEX (KIND=8) 	IEEESQ$   ! Actually, REAL(KIND=16)
      COMPLEX (KIND=8) 	IEEEQQ$   ! Actually, REAL(KIND=16)
#else
      REAL    (KIND=16) IEEEDQ$
      REAL    (KIND=16) IEEESQ$
      REAL    (KIND=16) IEEEQQ$
#endif

#ifdef	_MAXVL
CDIR$ VFUNCTION GI8I$
CDIR$ VFUNCTION GI16I$
CDIR$ VFUNCTION GI32O$
CDIR$ VFUNCTION GI32I$
CDIR$ VFUNCTION IEEESS$
CDIR$ VFUNCTION IEEESD$
CDIR$ VFUNCTION IEEESQ$
CDIR$ VFUNCTION IEEEDS$
CDIR$ VFUNCTION IEEEDD$
CDIR$ VFUNCTION IEEEDQ$
CDIR$ VFUNCTION IEEEQS$
CDIR$ VFUNCTION IEEEQD$
CDIR$ VFUNCTION IEEEQQ$
#endif

C     Conversion function control flags and overflow counts

      INTEGER (KIND=4)         GN32OF
      INTEGER (KIND=4)         DENORM, IEEEOF
#ifdef	_CRAYMPP
           COMMON  /T@GENERIC/ GN32OF
           COMMON  /T@IEEE/    DENORM, IEEEOF
#else
      TASK COMMON  /T@GENERIC/ GN32OF
      TASK COMMON  /T@IEEE/    DENORM, IEEEOF
#endif

C     Aliasing pointers

      INTEGER (KIND=8)  NFORN(0:*)
      INTEGER (KIND=4)  CRAYJ(0:*), IEEEJ(0:*)
      INTEGER (KIND=8)  CRAYI(0:*), IEEEI(0:*)
      REAL    (KIND=4)  CRAYH(0:*), IEEES(0:*)
      REAL    (KIND=8)  CRAYR(0:*), IEEED(0:*)
#ifdef	_CRAYMPP
      COMPLEX (KIND=8)  CRAYD(0:*), IEEEQ(0:*)  !  Actually, REAL (KIND=16)
#else
      REAL    (KIND=16) CRAYD(0:*), IEEEQ(0:*)
#endif
      COMPLEX (KIND=4)  CRAYF(0:*), IEEEF(0:*)
      COMPLEX (KIND=8)  CRAYC(0:*), IEEEC(0:*)
#ifdef	_CRAYMPP
      COMPLEX (KIND=8)  CRAYE(0:*), IEEEE(0:*)  !  Not used
#else
      COMPLEX (KIND=16) CRAYE(0:*), IEEEE(0:*)
#endif
      LOGICAL (KIND=4)  CRAYM(0:*), IEEEM(0:*)
      LOGICAL (KIND=8)  CRAYL(0:*), IEEEL(0:*)

      POINTER ( FPTR, NFORN)
      POINTER (CJPTR, CRAYJ), (IJPTR, IEEEJ)
      POINTER (CIPTR, CRAYI), (IIPTR, IEEEI)
      POINTER (CHPTR, CRAYH), (ISPTR, IEEES)
      POINTER (CRPTR, CRAYR), (IDPTR, IEEED)
      POINTER (CDPTR, CRAYD), (IQPTR, IEEEQ)
      POINTER (CCPTR, CRAYC), (ICPTR, IEEEC)
      POINTER (CEPTR, CRAYE), (IEPTR, IEEEE)
      POINTER (CFPTR, CRAYF), (IFPTR, IEEEF)
      POINTER (CMPTR, CRAYM), (IMPTR, IEEEM)
      POINTER (CLPTR, CRAYL), (ILPTR, IEEEL)

      INTEGER CHLEN, CHOFF, CI, CLEN, CLENB, II, ILEN, ILENB
      INTEGER CHNK, I, IBTL, INDX, IWAD, LEFT, LOGB2
      INTEGER NMBR, REMR, SCAL, SCALB, SHFT
      INTEGER (KIND=8)  TEMP(0:TEMPSZ-1), ITMP
      LOGICAL IEG2CRI_NATIVE_TYPE_OK$, IEG2CRI_FOREIGN_TYPE_OK$

      EXTERNAL MOVBITZ
      EXTERNAL IEG2CRI_NATIVE_TYPE_OK$, IEG2CRI_FOREIGN_TYPE_OK$

C     Statement function to compute the Log (base 2) of I

      LOGB2(I) = WORDSZM1 - LEADZ(I)

C     Check argument validity

      IF (NUMARG() .LT. 8) GOTO 9001

      IF (NUM .LT. 0) GOTO 9003

      IF ((BITOFF .LT. 0) .OR. (BITOFF .GE. WORDSZ)) GOTO 9004

      IF ((TYPE .LT. 1) .OR. (TYPE .GT. 6)) GOTO 9002

C     Ensure lengths are powers of two and otherwise valid

      IF (POPCNT(NATLEN) .NE. 1) GOTO 9005
      IF (.NOT. IEG2CRI_NATIVE_TYPE_OK$(TYPE, NATLEN)) GOTO 9005

      IF (POPCNT(FORLEN) .NE. 1) GOTO 9006
      IF (.NOT. IEG2CRI_FOREIGN_TYPE_OK$(TYPE, FORLEN)) GOTO 9006

      CI     = 0                        !  Set indicies
      II     = 0
      GN32OF = 0                        !  Clear overflow counts
      IEEEOF = 0
      NMBR   = NUM

      IF (NMBR .EQ. 0) GOTO 9000

      ITMP   = LOC(NATIVE)

#ifdef	_CRAYMPP
      IF (IAND(ITMP, 7) .NE. 0) GOTO 9009  !  If NATIVE not word-aligned
#endif

      CJPTR  = ITMP
      CIPTR  = ITMP
      CHPTR  = ITMP
      CRPTR  = ITMP
      CDPTR  = ITMP
      CCPTR  = ITMP
      CEPTR  = ITMP
      CFPTR  = ITMP
      CMPTR  = ITMP
      CLPTR  = ITMP

      IOFF   = BITOFF

#ifdef	_CRAYMPP
      ITMP   = LOC(FORN(0))

      IF (IAND(ITMP, 7) .NE. 0) THEN       !  If FORN not word-aligned

        IF (IAND(ITMP, 3) .NE. 0) GOTO 9010  !  If not halfword-aligned

        ITMP   = IBCLR(ITMP, 2)        ! Word-align address
        IOFF   = IOFF + (WORDSZ / 2)   ! Adjust bit offset to compensate

        IF (IOFF .GE. WORDSZ) THEN
          IOFF   = IOFF - WORDSZ
          ITMP   = ITMP + (WORDSZ / 8)
        ENDIF

      ENDIF

      FPTR   = ITMP
#else
      FPTR   = LOC(FORN(0))
#endif

      IJPTR  = FPTR
      IIPTR  = FPTR
      ISPTR  = FPTR
      IDPTR  = FPTR
      IQPTR  = FPTR
      ICPTR  = FPTR
      IEPTR  = FPTR
      IFPTR  = FPTR
      IMPTR  = FPTR
      ILPTR  = FPTR

      CLEN   = NATLEN                   !  Set lengths and bit offsets
      CLENB  = LOGB2(CLEN)              !  Log (base 2) of CLEN
      CINC   = STRIDE
      ILEN   = FORLEN
      ILENB  = LOGB2(ILEN)              !  Log (base 2) of ILEN

      GOTO (100,200,300,400,500,600), TYPE

C     TYPE = 1  (Typeless)                                  ************
C
C     Convert GENERIC typeless to CRAY typeless
C
C     GENERIC |                 CRAY
C             |  32-bit    64-bit   128-bit   256-bit
C     --------+----------------------------------------
C     32-bit  |   copy  | INVALID | INVALID | INVALID |
C             +---------+---------+---------+---------+
C     64-bit  | INVALID |   copy  | INVALID | INVALID |
C             +---------+---------+---------+---------+
C     128-bit | INVALID | INVALID |   copy  | INVALID |
C             +---------+---------+---------+---------+
C     256-bit | INVALID | INVALID | INVALID |  copy   |
C     --------+----------------------------------------
C
C     Note that 32-bit typeless is not supported on CRAY PVP systems and
C     256-bit typeless is not supported on CRAY MPP systems.

  100 CONTINUE

      IF (CLEN .NE. ILEN) GOTO 9008

C     TYPE = 2  (Integer)                                   ************
C
C     Convert GENERIC integers to CRAY integers
C
C     GENERIC           |         CRAY
C                       |   32-bit     64-bit
C     ------------------+----------------------
C      8-bit INTEGER    |   GI8I$   |  GI8I$
C                       +-----------+----------
C     16-bit INTEGER    |   GI16I$  |  GI16I$
C                       +-----------+----------
C     32-bit INTEGER    |   none    |  GI32I$
C                       +-----------+----------
C     64-bit INTEGER    |   GI32O$  |   none
C                        ----------------------

  200 CONTINUE

C     Special case a simple block move

      IF (STRIDE.EQ.1 .AND. ILEN.EQ.CLEN .AND. CLEN.GE.GRANSZ) THEN

        CALL MOVBITZ(NFORN(0), IOFF, NMBR * ILEN, NATIVE(0), COFF)
        GOTO 9000

      ENDIF

C     Do odd partial-word element(s), if extant

      IF (ILEN .LT. GRANSZ) THEN

        SCAL  = ISHFT(GRANSZ, -ILENB)    ! GRANSZ / ILEN
        SCALB = LOGB2(SCAL)              ! Log (base 2) of SCAL
        REMR  = IAND(NMBR, SCAL - 1)     ! Remainder (NMBR mod SCAL)

        IF (REMR .GT. 0) THEN

          NMBR  = NMBR - REMR
          INDX  = ISHFT(NMBR, -SCALB)

          CALL MOVBITZ(IEEEI(INDX), IOFF, ILEN * REMR, ITMP, ILEN)

          SELECT CASE (ILEN)

            CASE (8)
              SHFT  = -48
#ifdef	_MAXVL
CDIR$         SHORTLOOP
#endif
              DO I = 0, REMR - 1
#ifdef  _CRAYMPP
              IF (CLEN .EQ. 64) THEN  !  CLEN .EQ. 64 .AND. ILEN .EQ. 8
                CRAYI((NMBR+I)*CINC) = GI8I$(ISHFT(ITMP, SHFT))
              ELSE                    !  CLEN .EQ. 32 .AND. ILEN .EQ. 8
                CRAYJ((NMBR+I)*CINC) = GI8I$(ISHFT(ITMP, SHFT))
              ENDIF
#else
              CRAYI((NMBR+I)*CINC) = GI8I$(ISHFT(ITMP, SHFT))
#endif
                SHFT  = SHFT + 8
              ENDDO

            CASE (16)
              SHFT  = -32
#ifdef  _MAXVL
CDIR$         SHORTLOOP
#endif
              DO I = 0, REMR - 1
#ifdef  _CRAYMPP
                IF (CLEN .EQ. 64) THEN  !  CLEN .EQ. 64 .AND. ILEN .EQ. 16
                  CRAYI((NMBR+I)*CINC) = GI16I$(ISHFT(ITMP, SHFT))
                ELSE                    !  CLEN .EQ. 32 .AND. ILEN .EQ. 16
                  CRAYJ((NMBR+I)*CINC) = GI16I$(ISHFT(ITMP, SHFT))
                ENDIF
#else
                CRAYI((NMBR+I)*CINC) = GI16I$(ISHFT(ITMP, SHFT))
#endif
                SHFT  = SHFT + 16
              ENDDO

            CASE (32)
#ifdef	_CRAYMPP
              IF (CLEN .EQ. 64) THEN  !  CLEN .EQ. 64 .AND. ILEN .EQ. 32
                CRAYI(NMBR * CINC) = GI32I$(ITMP)
              ELSE                    !  CLEN .EQ. 32 .AND. ILEN .EQ. 32
                CRAYJ(NMBR * CINC) = GI32I$(ITMP)
              ENDIF
#else
              CRAYI(NMBR * CINC) = GI32I$(ITMP)
#endif

          END SELECT

          IF (NMBR .EQ. 0) GOTO 9000

        ENDIF

      ENDIF

C     If output array is not word-aligned, then the output from the
C     conversion functions must be redirected to a temporary array
C     prior to the final transfer.

      LEFT  = NMBR

      IF (IOFF .NE. 0) THEN       !  Strip mine

        IJPTR = LOC(TEMP(0))
        IIPTR = LOC(TEMP(0))
        ISPTR = LOC(TEMP(0))
        IDPTR = LOC(TEMP(0))
        IQPTR = LOC(TEMP(0))
        ICPTR = LOC(TEMP(0))
        IEPTR = LOC(TEMP(0))
        IFPTR = LOC(TEMP(0))
        IMPTR = LOC(TEMP(0))
        ILPTR = LOC(TEMP(0))
        CHNK  = ISHFT(ISHFT(TEMPSZ, WORDSZB), -ILENB)
        NMBR  = MOD(NMBR, CHNK)

        IF (NMBR .EQ. 0) NMBR  = CHNK    !  Size of first chunk

        INDX  = 0

      ENDIF

C
C     Main loop for Typeless and Integer conversions
C

      DO WHILE (LEFT .GT. 0)

        IF (IOFF .NE. 0) THEN
          CALL MOVBITZ(NFORN(INDX), IOFF, NMBR * ILEN, TEMP(0), 0)
          II    = 0
        ENDIF

        SELECT CASE (ILEN)

          CASE (8)
#ifdef	_CRAYMPP
            IF (CLEN .EQ. 64) THEN  !  CLEN .EQ. 64 .AND. ILEN .EQ. 8
              DO I = 0, NMBR - 1, 8
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
            ELSE                    !  CLEN .EQ. 32 .AND. ILEN .EQ. 8
              DO I = 0, NMBR - 1, 4
                CRAYJ(CI)        = GI8I$(ISHFT(IEEEJ(II), -24))
                CRAYJ(CI+CINC)   = GI8I$(ISHFT(IEEEJ(II), -16))
                CRAYJ(CI+CINC*2) = GI8I$(ISHFT(IEEEJ(II), -8))
                CRAYJ(CI+CINC*3) = GI8I$(      IEEEJ(II))
                CI               = CI + (4 * CINC)
                II               = II + IINC
              ENDDO
            ENDIF
#else
            DO I = 0, NMBR - 1, 8   !  CLEN .EQ. 32|64 .AND. ILEN .EQ. 8
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
#ifdef  _CRAYMPP
            IF (CLEN .EQ. 64) THEN  !  CLEN .EQ. 64 .AND. ILEN .EQ. 16
              DO I = 0, NMBR - 1, 4
                CRAYI(CI)        = GI16I$(ISHFT(IEEEI(II), -48))
                CRAYI(CI+CINC)   = GI16I$(ISHFT(IEEEI(II), -32))
                CRAYI(CI+CINC*2) = GI16I$(ISHFT(IEEEI(II), -16))
                CRAYI(CI+CINC*3) = GI16I$(      IEEEI(II))
                CI               = CI + (4 * CINC)
                II               = II + IINC
              ENDDO
            ELSE                    !  CLEN .EQ. 32 .AND. ILEN .EQ. 16
              DO I = 0, NMBR - 1, 2
                CRAYJ(CI)      = GI16I$(ISHFT(IEEEJ(II), -16))
                CRAYJ(CI+CINC) = GI16I$(      IEEEJ(II))
                CI             = CI + (2 * CINC)
                II             = II + IINC
              ENDDO
            ENDIF
#else
            DO I = 0, NMBR - 1, 4   !  CLEN .EQ. 32|64 .AND. ILEN .EQ. 16
              CRAYI(CI)        = GI16I$(ISHFT(IEEEI(II), -48))
              CRAYI(CI+CINC)   = GI16I$(ISHFT(IEEEI(II), -32))
              CRAYI(CI+CINC*2) = GI16I$(ISHFT(IEEEI(II), -16))
              CRAYI(CI+CINC*3) = GI16I$(      IEEEI(II))
              CI               = CI + (4 * CINC)
              II               = II + IINC
            ENDDO
#endif

          CASE (32)
#ifdef  _CRAYMPP
            IF (CLEN .EQ. 64) THEN  !  CLEN .EQ. 64 .AND. ILEN .EQ. 32
              DO I = 0, NMBR - 1
                CRAYI(CI) = GI32I$(IEEEJ(II))
                CI        = CI + CINC
                II        = II + IINC
              ENDDO
            ELSE                    !  CLEN .EQ. 32 .AND. ILEN .EQ. 32
              DO I = 0, NMBR - 1
                CRAYJ(CI) = IEEEJ(II)
                CI        = CI + CINC
                II        = II + IINC
              ENDDO
            ENDIF
#else
            DO I = 0, NMBR - 1, 2   !  CLEN .EQ. 32|64 .AND. ILEN .EQ. 32
              CRAYI(CI)        = GI32I$(ISHFT(IEEEI(II), -32))
              CRAYI(CI + CINC) = GI32I$(      IEEEI(II))
              CI               = CI + (2 * CINC)
              II               = II + IINC
            ENDDO
#endif

          CASE (64)
            IF (CLEN .EQ. 64) THEN  !  CLEN .EQ. 64 .AND. ILEN .EQ. 64
              DO I = 0, NMBR - 1
                CRAYI(CI) = IEEEI(II)
                CI        = CI + CINC
                II        = II + IINC
              ENDDO
            ELSE                    !  CLEN .EQ. 32 .AND. ILEN .EQ. 64
              DO I = 0, NMBR - 1
                CRAYJ(CI) = GI32O$(IEEEI(II))
                CI        = CI + CINC
                II        = II + IINC
              ENDDO
            ENDIF

          CASE (128)
            DO I = 0, NMBR - 1
              CRAYD(CI) = IEEEQ(II)
              CI        = CI + CINC
              II        = II + IINC
            ENDDO

#ifndef	_CRAYMPP
          CASE (256)
            DO I = 0, NMBR - 1
              CRAYE(CI) = IEEEE(II)
              CI        = CI + CINC
              II        = II + IINC
            ENDDO
#endif

        END SELECT

        LEFT  = LEFT - NMBR

        IF (IOFF .NE. 0) THEN       !  Complete transfer
          INDX  = INDX + NMBR
          NMBR  = CHNK
        ENDIF

      ENDDO

      GOTO 9000

C     TYPE = 3  (Real)                                      ************
C
C     Convert GENERIC IEEE floating-point numbers to CRAY IEEE floating-
C     point numbers
C
C     GENERIC IEEE      |                   CRAY IEEE
C                       |    32-bit          64-bit          128-bit
C                       |    (half)         (single)         (double)
C                       |    KIND=4          KIND=8          KIND=16
C     ------------------+-----------------------------------------------
C     32-bit (single)   |    IEEESS$    |    IEEESD$    |    IEEESQ$
C                       +---------------+---------------+---------------
C     64-bit (double)   |    IEEEDS$    |    IEEEDD$    |    IEEEDQ$
C                       +---------------+---------------+---------------
C     128-bit (quad)    |    IEEEQS$    |    IEEEQD$    |    IEEEQQ$
C                        -----------------------------------------------

  300 CONTINUE

C     Do odd half-word element, if extant

#ifndef	_CRAYMPP
      IF (ILEN .EQ. 32 .AND. IAND(NMBR, 1) .NE. 0) THEN

        NMBR  = NMBR - 1
        INDX  = ISHFT(NMBR, -1)

        CALL MOVBITZ(IEEEI(INDX), IOFF, ILEN, ITMP, ILEN)

        SELECT CASE (CLEN)

          CASE (32)                 !  CLEN .EQ. 32
            CRAYI(NMBR * CINC) = IEEESS$(ITMP)

          CASE (64)                 !  CLEN .EQ. 64
            CRAYR(NMBR * CINC) = IEEESD$(ITMP)

          CASE (128)                !  CLEN .EQ. 128
            CRAYD(NMBR * CINC) = IEEESQ$(ITMP)

        END SELECT

        IF (NMBR .EQ. 0) GOTO 9000

      ENDIF
#endif

C     If output array is not word-aligned, then the output from the
C     conversion functions must be redirected to a temporary array
C     prior to the final transfer.

      LEFT  = NMBR

      IF (IOFF .NE. 0) THEN       !  Strip mine

        IJPTR = LOC(TEMP(0))
        IIPTR = LOC(TEMP(0))
        ISPTR = LOC(TEMP(0))
        IDPTR = LOC(TEMP(0))
        IQPTR = LOC(TEMP(0))
        ICPTR = LOC(TEMP(0))
        IEPTR = LOC(TEMP(0))
        IFPTR = LOC(TEMP(0))
        IMPTR = LOC(TEMP(0))
        ILPTR = LOC(TEMP(0))
        CHNK  = ISHFT(ISHFT(TEMPSZ, WORDSZB), -ILENB)
        NMBR  = MOD(NMBR, CHNK)

        IF (NMBR .EQ. 0) NMBR  = CHNK   !  Size of first chunk

        INDX  = 0
      ENDIF

C
C     Main loop for Real (floating-point) conversions
C

      DO WHILE (LEFT .GT. 0)

        IF (IOFF .NE. 0) THEN
          CALL MOVBITZ(NFORN(INDX), IOFF, NMBR * ILEN, TEMP(0), 0)
          II    = 0
        ENDIF

        SELECT CASE (CLEN)
          CASE (32)                 !  CLEN .EQ. 32

          SELECT CASE (ILEN)
            CASE (32)               !  CLEN .EQ. 32 .AND. ILEN .EQ. 32
#ifdef	_CRAYMPP
              DO I = 0, NMBR - 1
                CRAYH(CI) = IEEESS$(IEEES(II))
                CI        = CI + CINC
                II        = II + IINC
              ENDDO
#else
              DO I = 0, NMBR - 1, 2
                CRAYI(CI)      = IEEESS$(ISHFT(IEEEI(II), -32))
                CRAYI(CI+CINC) = IEEESS$(      IEEEI(II))
                CI             = CI + (2 * CINC)
                II             = II + IINC
              ENDDO
#endif

            CASE (64)               !  CLEN .EQ. 32 .AND. ILEN .EQ. 64
              DO I = 0, NMBR - 1
                CRAYH(CI) = IEEEDS$(IEEED(II))
                CI        = CI + CINC
                II        = II + IINC
              ENDDO

            CASE (128)              !  CLEN .EQ. 32 .AND. ILEN .EQ. 128
              DO I = 0, NMBR - 1
                CRAYH(CI) = IEEEQS$(IEEEQ(II))
                CI        = CI + CINC
                II        = II + IINC
              ENDDO

          END SELECT

          CASE (64)                 !  CLEN .EQ. 64

          SELECT CASE (ILEN)

            CASE (32)               !  CLEN .EQ. 64 .AND. ILEN .EQ. 32
#ifdef	_CRAYMPP
              DO I = 0, NMBR - 1
                CRAYR(CI) = IEEESD$(IEEES(II))
                CI        = CI + CINC
                II        = II + IINC
              ENDDO
#else
              DO I = 0, NMBR - 1, 2
                CRAYR(CI)      = IEEESD$(ISHFT(IEEEI(II), -32))
                CRAYR(CI+CINC) = IEEESD$(      IEEEI(II))
                CI             = CI + (2 * CINC)
                II             = II + IINC
              ENDDO
#endif

            CASE (64)               !  CLEN .EQ. 64 .AND. ILEN .EQ. 64
              DO I = 0, NMBR - 1
                CRAYR(CI) = IEEEDD$(IEEED(II))
                CI        = CI + CINC
                II        = II + IINC
              ENDDO

            CASE (128)              !  CLEN .EQ. 64 .AND. ILEN .EQ. 128
              DO I = 0, NMBR - 1
                CRAYR(CI) = IEEEQD$(IEEEQ(II))
                CI        = CI + CINC
                II        = II + IINC
              ENDDO

          END SELECT

          CASE (128)                !  CLEN .EQ. 128

          SELECT CASE (ILEN)

            CASE (32)               !  CLEN .EQ. 128 .AND. ILEN .EQ. 32
#ifdef	_CRAYMPP
              DO I = 0, NMBR - 1
                CRAYD(CI) = IEEESQ$(IEEES(II))
                CI        = CI + CINC
                II        = II + IINC
              ENDDO
#else
              DO I = 0, NMBR - 1, 2
                CRAYD(CI)      = IEEESQ$(ISHFT(IEEEI(II), -32))
                CRAYD(CI+CINC) = IEEESQ$(      IEEEI(II))
                CI             = CI + (2 * CINC)
                II             = II + IINC
              ENDDO
#endif

            CASE (64)               !  CLEN .EQ. 128 .AND. ILEN .EQ. 64
              DO I = 0, NMBR - 1
                CRAYD(CI) = IEEEDQ$(IEEED(II))
                CI        = CI + CINC
                II        = II + IINC
              ENDDO

            CASE (128)              !  CLEN .EQ. 128 .AND. ILEN .EQ. 128
              DO I = 0, NMBR - 1
                CRAYD(CI) = IEEEQQ$(IEEEQ(II))
                CI        = CI + CINC
                II        = II + IINC
              ENDDO

          END SELECT

        END SELECT

        LEFT  = LEFT - NMBR

        IF (IOFF .NE. 0) THEN       !  Complete transfer
          INDX  = INDX + NMBR
          NMBR  = CHNK
        ENDIF

      ENDDO

      GOTO 9000

C     TYPE = 4  (Complex)                                   ************
C
C     Convert GENERIC IEEE complex floating-point numbers to CRAY IEEE
C     complex floating-point numbers
C
C     GENERIC IEEE      |                  CRAY IEEE
C                       |   2x32-bit        2x64-bit        2x128-bit
C                       |    (half)         (single)         (double)
C                       |    KIND=4          KIND=8          KIND=16
C     ------------------+-----------------------------------------------
C     2x32-bit (single) |   2xIEEESS$   |   2xIEEESD$   |   2xIEEESQ$
C                       +---------------+---------------+---------------
C     2x64-bit (double) |   2xIEEEDS$   |   2xIEEEDD$   |   2xIEEEDQ$
C                       +---------------+---------------+---------------
C     2x128-bit (quad)  |   2xIEEEQS$   |   2xIEEEQD$   |   2xIEEEQQ$
C                        -----------------------------------------------

  400 CONTINUE

C     If output array is not word-aligned, then the output from the
C     conversion functions must be redirected to a temporary array
C     prior to the final transfer.

      LEFT  = NMBR

      IF (IOFF .NE. 0) THEN       !  Strip mine

        IJPTR = LOC(TEMP(0))
        IIPTR = LOC(TEMP(0))
        ISPTR = LOC(TEMP(0))
        IDPTR = LOC(TEMP(0))
        IQPTR = LOC(TEMP(0))
        ICPTR = LOC(TEMP(0))
        IEPTR = LOC(TEMP(0))
        IFPTR = LOC(TEMP(0))
        IMPTR = LOC(TEMP(0))
        ILPTR = LOC(TEMP(0))
        CHNK  = ISHFT(ISHFT(TEMPSZ, WORDSZB), -ILENB)
        NMBR  = MOD(NMBR, CHNK)

        IF (NMBR .EQ. 0) NMBR  = CHNK   !  Size of first chunk

        INDX  = 0

      ENDIF

C
C     Main loop for Complex conversions
C

      DO WHILE (LEFT .GT. 0)

        IF (IOFF .NE. 0) THEN
          CALL MOVBITZ(NFORN(INDX), IOFF, NMBR * ILEN, TEMP(0), 0)
          II    = 0
        ENDIF

        SELECT CASE (CLEN)

        CASE (64)

          SELECT CASE (ILEN)

          CASE (64)               !  CLEN .EQ. 64 .AND. ILEN .EQ. 64
            DO I = 0, NMBR - 1
#ifdef	_CRAYMPP
              CRAYF(CI) = CMPLX(IEEESS$(REAL (IEEEF(II))),
     1                          IEEESS$(AIMAG(IEEEF(II))),
     2                          KIND=4)
#else
              CRAYI(CI) = IOR(ISHFT(IEEESS$(ISHFT(IEEEI(II), -32)), 32),
     1                              IEEESS$(      IEEEI(II))           )
#endif
              CI        = CI + CINC
              II        = II + IINC
            ENDDO

          CASE (128)              !  CLEN .EQ. 64 .AND. ILEN .EQ. 128
            DO I = 0, NMBR - 1
#ifdef	_CRAYMPP
              CRAYF(CI) = CMPLX(IEEEDS$(REAL (IEEEC(II))),
     1                          IEEEDS$(AIMAG(IEEEC(II))),
     2                          KIND=4)
#else
              CRAYI(CI) = IOR(ISHFT(IEEEDS$(REAL (IEEEC(II))), 32),
     1                              IEEEDS$(AIMAG(IEEEC(II)))      )
#endif
              CI        = CI + CINC
              II        = II + IINC
            ENDDO

#ifndef	_CRAYMPP
          CASE (256)              !  CLEN .EQ. 64 .AND. ILEN .EQ. 256
            DO I = 0, NMBR - 1
              CRAYI(CI) = IOR(ISHFT(IEEEQS$(REAL (IEEEE(II))), 32),
     1                              IEEEQS$(AIMAG(IEEEE(II)))      )
              CI        = CI + CINC
              II        = II + IINC
            ENDDO
#endif

          END SELECT

        CASE (128)

          SELECT CASE (ILEN)

          CASE (64)               !  CLEN .EQ. 128 .AND. ILEN .EQ. 64
            DO I = 0, NMBR - 1
#ifdef	_CRAYMPP
              CRAYC(CI) = CMPLX(IEEESD$(REAL (IEEEF(II))),
     1                          IEEESD$(AIMAG(IEEEF(II))),
     2                          KIND=8)
#else
              CRAYC(CI) = CMPLX(IEEESD$(ISHFT(IEEEI(II), -32)),
     1                          IEEESD$(      IEEEI(II)),
     2                          KIND=8)
#endif
              CI        = CI + CINC
              II        = II + IINC
            ENDDO

          CASE (128)              !  CLEN .EQ. 128 .AND. ILEN .EQ. 128
            DO I = 0, NMBR - 1
              CRAYC(CI) = CMPLX(IEEEDD$(REAL (IEEEC(II))),
     1                          IEEEDD$(AIMAG(IEEEC(II))),
     2                          KIND=8)
              CI        = CI + CINC
              II        = II + IINC
            ENDDO

#ifndef	_CRAYMPP
          CASE (256)              !  CLEN .EQ. 128 .AND. ILEN .EQ. 256
            DO I = 0, NMBR - 1
              CRAYC(CI) = CMPLX(IEEEQD$(REAL (IEEEE(II))),
     1                          IEEEQD$(AIMAG(IEEEE(II))),
     2                          KIND=8)
              CI        = CI + CINC
              II        = II + IINC
            ENDDO
#endif

          END SELECT

#ifndef	_CRAYMPP
        CASE (256)

          SELECT CASE (ILEN)

          CASE (64)               !  CLEN .EQ. 256 .AND. ILEN .EQ. 64
            DO I = 0, NMBR - 1
              CRAYE(CI) = CMPLX(IEEESQ$(ISHFT(IEEEI(II), -32)),
     1                          IEEESQ$(      IEEEI(II)),
     2                          KIND=16)
              CI        = CI + CINC
              II        = II + IINC
            ENDDO

          CASE (128)              !  CLEN .EQ. 256 .AND. ILEN .EQ. 128
            DO I = 0, NMBR - 1
              CRAYE(CI) = CMPLX(IEEEDQ$(REAL (IEEEC(II))),
     1                          IEEEDQ$(AIMAG(IEEEC(II))),
     2                          KIND=16)
              CI        = CI + CINC
              II        = II + IINC
            ENDDO

          CASE (256)              !  CLEN .EQ. 256 .AND. ILEN .EQ. 256
            DO I = 0, NMBR - 1
              CRAYE(CI) = CMPLX(IEEEQQ$(REAL (IEEEE(II))),
     1                          IEEEQQ$(AIMAG(IEEEE(II))),
     2                          KIND=16)
              CI        = CI + CINC
              II        = II + IINC
            ENDDO

          END SELECT
#endif

        END SELECT

        LEFT  = LEFT - NMBR

        IF (IOFF .NE. 0) THEN       !  Complete transfer
          INDX  = INDX + NMBR
          NMBR  = CHNK
        ENDIF

      ENDDO

      GOTO 9000

C     TYPE = 5  (Logical)                                   ************
C
C     Convert GENERIC logicals to CRAY logicals
C
C     GENERIC           |         CRAY
C                       |   32-bit     64-bit
C     ------------------+----------------------
C      8-bit LOGICAL    |   inline  |  inline
C                       +-----------+----------
C     16-bit LOGICAL    |   inline  |  inline
C                       +-----------+----------
C     32-bit LOGICAL    |   none    |  inline
C                       +-----------+----------
C     64-bit LOGICAL    |   inline  |   none
C                        ----------------------

  500 CONTINUE

C     Special case a simple block move

      IF (STRIDE.EQ.1 .AND. ILEN.EQ.CLEN .AND. CLEN.GE.GRANSZ) THEN

        CALL MOVBITZ(NFORN(0), IOFF, NMBR * ILEN, NATIVE(0), COFF)
        GOTO 9000

      ENDIF

C     Do odd partial-word element(s), if extant

      IF (ILEN .LT. GRANSZ) THEN

        SCAL  = ISHFT(GRANSZ, -ILENB)    ! GRANSZ / ILEN
        SCALB = LOGB2(SCAL)              ! Log (base 2) of SCAL
        REMR  = IAND(NMBR, SCAL - 1)     ! Remainder (NMBR mod SCAL)

        IF (REMR .GT. 0) THEN

          NMBR  = NMBR - REMR
          INDX  = ISHFT(NMBR, -SCALB)

          CALL MOVBITZ(IEEEI(INDX), IOFF, ILEN * REMR, ITMP, ILEN)

          SELECT CASE (ILEN)

            CASE (8)
              SHFT  = -48
#ifndef _CRAYMPP
CDIR$         SHORTLOOP
#endif
              DO I = 0, REMR - 1
#ifdef  _CRAYMPP
                IF (CLEN .EQ. 64) THEN  ! CLEN .EQ. 64 .AND. ILEN .EQ. 8
        CRAYI((NMBR+I)*CINC) = CVMGN(1, 0, IAND(ISHFT(ITMP, SHFT), M08))
                ELSE                    ! CLEN .EQ. 32 .AND. ILEN .EQ. 8
        CRAYJ((NMBR+I)*CINC) = CVMGN(1, 0, IAND(ISHFT(ITMP, SHFT), M08))
                ENDIF
#else
        CRAYI((NMBR+I)*CINC) = CVMGN(1, 0, IAND(ISHFT(ITMP, SHFT), M08))
#endif
                SHFT  = SHFT + 8
              ENDDO

            CASE (16)
              SHFT  = -32
#ifndef _CRAYMPP
CDIR$         SHORTLOOP
#endif
              DO I = 0, REMR - 1
#ifdef  _CRAYMPP
                IF (CLEN .EQ. 64) THEN ! CLEN .EQ. 64 .AND. ILEN .EQ. 16
        CRAYI((NMBR+I)*CINC) = CVMGN(1, 0, IAND(ISHFT(ITMP, SHFT), M16))
                ELSE                   ! CLEN .EQ. 32 .AND. ILEN .EQ. 16
        CRAYJ((NMBR+I)*CINC) = CVMGN(1, 0, IAND(ISHFT(ITMP, SHFT), M16))
                ENDIF
#else
        CRAYI((NMBR+I)*CINC) = CVMGN(1, 0, IAND(ISHFT(ITMP, SHFT), M16))
#endif
                SHFT  = SHFT + 16
              ENDDO

            CASE (32)
#ifdef	_CRAYMPP
              IF (CLEN .EQ. 64) THEN  !  CLEN .EQ. 64 .AND. ILEN .EQ. 32
                CRAYI(NMBR * CINC) = CVMGN(1, 0, ITMP)
              ELSE                    !  CLEN .EQ. 32 .AND. ILEN .EQ. 32
                CRAYJ(NMBR * CINC) = CVMGN(1, 0, ITMP)
              ENDIF
#else
              CRAYI(NMBR * CINC) = CVMGN(1, 0, ITMP)
#endif

          END SELECT

          IF (NMBR .EQ. 0) GOTO 9000

        ENDIF

      ENDIF

C     If output array is not word-aligned, then the output from the
C     conversion functions must be redirected to a temporary array
C     prior to the final transfer.

      LEFT  = NMBR

      IF (IOFF .NE. 0) THEN       !  Strip mine

        IJPTR = LOC(TEMP(0))
        IIPTR = LOC(TEMP(0))
        ISPTR = LOC(TEMP(0))
        IDPTR = LOC(TEMP(0))
        IQPTR = LOC(TEMP(0))
        ICPTR = LOC(TEMP(0))
        IEPTR = LOC(TEMP(0))
        IFPTR = LOC(TEMP(0))
        IMPTR = LOC(TEMP(0))
        ILPTR = LOC(TEMP(0))
        CHNK  = ISHFT(ISHFT(TEMPSZ, WORDSZB), -ILENB)
        NMBR  = MOD(NMBR, CHNK)

        IF (NMBR .EQ. 0) NMBR  = CHNK   !  Size of first chunk

        INDX  = 0

      ENDIF

C
C     Main loop for Logical conversions
C

      DO WHILE (LEFT .GT. 0)

        IF (IOFF .NE. 0) THEN
          CALL MOVBITZ(NFORN(INDX), IOFF, NMBR * ILEN, TEMP(0), 0)
          II    = 0
        ENDIF

        SELECT CASE (ILEN)

        CASE (8)
#ifdef	_CRAYMPP
          IF (CLEN .EQ. 64) THEN  !  CLEN .EQ. 64 .AND. ILEN .EQ. 8
            DO I = 0, NMBR - 1, 8
        CRAYI(CI)        = CVMGN(1, 0,      ISHFT(IEEEI(II), -56))
        CRAYI(CI+CINC)   = CVMGN(1, 0, IAND(ISHFT(IEEEI(II), -48), M08))
        CRAYI(CI+CINC*2) = CVMGN(1, 0, IAND(ISHFT(IEEEI(II), -40), M08))
        CRAYI(CI+CINC*3) = CVMGN(1, 0, IAND(ISHFT(IEEEI(II), -32), M08))
        CRAYI(CI+CINC*4) = CVMGN(1, 0, IAND(ISHFT(IEEEI(II), -24), M08))
        CRAYI(CI+CINC*5) = CVMGN(1, 0, IAND(ISHFT(IEEEI(II), -16), M08))
        CRAYI(CI+CINC*6) = CVMGN(1, 0, IAND(ISHFT(IEEEI(II),  -8), M08))
        CRAYI(CI+CINC*7) = CVMGN(1, 0, IAND(      IEEEI(II), M08))
              CI         = CI + (8 * CINC)
              II         = II + IINC
            ENDDO
          ELSE                    !  CLEN .EQ. 32 .AND. ILEN .EQ. 8
            DO I = 0, NMBR - 1, 8
        CRAYJ(CI)        = CVMGN(1, 0,      ISHFT(IEEEI(II), -56))
        CRAYJ(CI+CINC)   = CVMGN(1, 0, IAND(ISHFT(IEEEI(II), -48), M08))
        CRAYJ(CI+CINC*2) = CVMGN(1, 0, IAND(ISHFT(IEEEI(II), -40), M08))
        CRAYJ(CI+CINC*3) = CVMGN(1, 0, IAND(ISHFT(IEEEI(II), -32), M08))
        CRAYJ(CI+CINC*4) = CVMGN(1, 0, IAND(ISHFT(IEEEI(II), -24), M08))
        CRAYJ(CI+CINC*5) = CVMGN(1, 0, IAND(ISHFT(IEEEI(II), -16), M08))
        CRAYJ(CI+CINC*6) = CVMGN(1, 0, IAND(ISHFT(IEEEI(II),  -8), M08))
        CRAYJ(CI+CINC*7) = CVMGN(1, 0, IAND(      IEEEI(II), M08))
              CI         = CI + (8 * CINC)
              II         = II + IINC
            ENDDO
          ENDIF
#else
            DO I = 0, NMBR - 1, 8
        CRAYI(CI)        = CVMGN(1, 0,      ISHFT(IEEEI(II), -56))
        CRAYI(CI+CINC)   = CVMGN(1, 0, IAND(ISHFT(IEEEI(II), -48), M08))
        CRAYI(CI+CINC*2) = CVMGN(1, 0, IAND(ISHFT(IEEEI(II), -40), M08))
        CRAYI(CI+CINC*3) = CVMGN(1, 0, IAND(ISHFT(IEEEI(II), -32), M08))
        CRAYI(CI+CINC*4) = CVMGN(1, 0, IAND(ISHFT(IEEEI(II), -24), M08))
        CRAYI(CI+CINC*5) = CVMGN(1, 0, IAND(ISHFT(IEEEI(II), -16), M08))
        CRAYI(CI+CINC*6) = CVMGN(1, 0, IAND(ISHFT(IEEEI(II),  -8), M08))
        CRAYI(CI+CINC*7) = CVMGN(1, 0, IAND(      IEEEI(II), M08))
              CI         = CI + (4 * CINC)
              II         = II + IINC
            ENDDO
#endif

        CASE (16)
#ifdef	_CRAYMPP
          IF (CLEN .EQ. 64) THEN  !  CLEN .EQ. 64 .AND. ILEN .EQ. 16
            DO I = 0, NMBR - 1, 4
        CRAYI(CI)        = CVMGN(1, 0,      ISHFT(IEEEI(II), -48))
        CRAYI(CI+CINC)   = CVMGN(1, 0, IAND(ISHFT(IEEEI(II), -32), M16))
        CRAYI(CI+CINC*2) = CVMGN(1, 0, IAND(ISHFT(IEEEI(II), -16), M16))
        CRAYI(CI+CINC*3) = CVMGN(1, 0, IAND(      IEEEI(II), M16))
              CI         = CI + (4 * CINC)
              II         = II + IINC
            ENDDO
          ELSE                    !  CLEN .EQ. 32 .AND. ILEN .EQ. 16
            DO I = 0, NMBR - 1, 4
        CRAYJ(CI)        = CVMGN(1, 0,      ISHFT(IEEEI(II), -48))
        CRAYJ(CI+CINC)   = CVMGN(1, 0, IAND(ISHFT(IEEEI(II), -32), M16))
        CRAYJ(CI+CINC*2) = CVMGN(1, 0, IAND(ISHFT(IEEEI(II), -16), M16))
        CRAYJ(CI+CINC*3) = CVMGN(1, 0, IAND(      IEEEI(II), M16))
              CI         = CI + (4 * CINC)
              II         = II + IINC
            ENDDO
          ENDIF
#else
          DO I = 0, NMBR - 1, 4
        CRAYI(CI)        = CVMGN(1, 0,      ISHFT(IEEEI(II), -48))
        CRAYI(CI+CINC)   = CVMGN(1, 0, IAND(ISHFT(IEEEI(II), -32), M16))
        CRAYI(CI+CINC*2) = CVMGN(1, 0, IAND(ISHFT(IEEEI(II), -16), M16))
        CRAYI(CI+CINC*3) = CVMGN(1, 0, IAND(      IEEEI(II), M16))
            CI           = CI + (4 * CINC)
            II           = II + IINC
          ENDDO
#endif

        CASE (32)
#ifdef	_CRAYMPP
          IF (CLEN .EQ. 64) THEN  !  CLEN .EQ. 64 .AND. ILEN .EQ. 32
            DO I = 0, NMBR - 1
              CRAYI(CI) = CVMGN(1, 0, IEEEJ(II))
              CI        = CI + CINC
              II        = II + IINC
            ENDDO
          ELSE                    !  CLEN .EQ. 32 .AND. ILEN .EQ. 32
            DO I = 0, NMBR - 1
              CRAYJ(CI) = CVMGN(1, 0, IEEEJ(II))
              CI        = CI + CINC
              II        = II + IINC
            ENDDO
          ENDIF
#else
          DO I = 0, NMBR - 1, 2
            CRAYI(CI)      = CVMGN(1, 0, ISHFT(IEEEI(II), -32))
            CRAYI(CI+CINC) = CVMGN(1, 0,  IAND(IEEEI(II), M32))
            CI             = CI + (2 * CINC)
            II             = II + IINC
          ENDDO
#endif

        CASE (64)
#ifdef	_CRAYMPP
          IF (CLEN .EQ. 64) THEN  !  CLEN .EQ. 64 .AND. ILEN .EQ. 64
            DO I = 0, NMBR - 1
              CRAYI(CI) = CVMGN(1, 0, IEEEI(II))
              CI        = CI + CINC
              II        = II + IINC
            ENDDO
          ELSE                    !  CLEN .EQ. 32 .AND. ILEN .EQ. 64
            DO I = 0, NMBR - 1
              CRAYJ(CI) = CVMGN(1, 0, IEEEI(II))
              CI        = CI + CINC
              II        = II + IINC
            ENDDO
          ENDIF
#else
          DO I = 0, NMBR - 1
            CRAYI(CI) = CVMGN(1, 0, IEEEI(II))
            CI        = CI + CINC
            II        = II + IINC
          ENDDO
#endif

        END SELECT

        LEFT  = LEFT - NMBR

        IF (IOFF .NE. 0) THEN       !  Complete transfer
          INDX  = INDX + NMBR
          NMBR  = CHNK
        ENDIF

      ENDDO

      GOTO 9000

C     TYPE = 6  (Character)                                 ************

  600 CONTINUE

      IF (CLEN .NE. ILEN) GOTO 9008

      CALL G@CHRPCK(NATIVECH, IWAD, IBTL, CHOFF)  ! Get character offset

      CIPTR = IWAD
      CHLEN = ILEN * LEN(NATIVECH(0))             ! Get character length

      IF (STRIDE .EQ. 1) THEN

        CALL MOVBITZ(IEEEI(0), IOFF, NMBR * CHLEN, CRAYI(0), CHOFF)

      ELSE

        DO I = 0, NMBR - 1

          CALL MOVBITZ(IEEEI(II), IOFF, CHLEN, CRAYI(CI), CHOFF)

          CHOFF = CHOFF + (CHLEN * CINC)
          CI    = CI + ISHFT(CHOFF, -WORDSZB)
          CHOFF = MOD(CHOFF, WORDSZ)
          IOFF  = IOFF + (CHLEN * IINC)
          II    = II + ISHFT(IOFF, -WORDSZB)
          IOFF  = MOD(IOFF, WORDSZ)

        ENDDO

      ENDIF
      GOTO 9000

 9010 CONTINUE
      IEG2CRI = -10               !  FORN must be 64-bit word-aligned
      GOTO 10000

 9009 CONTINUE
      IEG2CRI = -9                !  NATIVE must be 64-bit word-aligned
      GOTO 10000

 9008 CONTINUE
      IEG2CRI = -8                !  Parameter error, NATLEN/FORLEN invalid
      GOTO 10000

C9007 CONTINUE
C     IEG2CRI = -7                !  Unable to allocate memory (unused in
C     GOTO 10000                  !  this version of the routine)

 9006 CONTINUE
      IEG2CRI = -6                !  Parameter error, invalid FORLEN
      GOTO 10000

 9005 CONTINUE
      IEG2CRI = -5                !  Parameter error, invalid NATLEN
      GOTO 10000

 9004 CONTINUE
      IEG2CRI = -4                !  Parameter error, invalid BITOFF
      GOTO 10000

 9003 CONTINUE
      IEG2CRI = -3                !  Parameter error, invalid NUM
      GOTO 10000

 9002 CONTINUE
      IEG2CRI = -2                !  Parameter error, invalid TYPE
      GOTO 10000

 9001 CONTINUE
      IEG2CRI = -1                !  Parameter error
      GOTO 10000

 9000 CONTINUE
      IEG2CRI = GN32OF + IEEEOF   !  Return error count
C     GOTO 10000

10000 CONTINUE
      RETURN

CDIR$ ID "@(#) libu/ieg/ieg2cri.F	92.0	10/08/98 14:57:41"
      END
C
C     Auxiliary functions
C
      LOGICAL FUNCTION IEG2CRI_AUXILIARY_FUNCTION$()
      LOGICAL NOK, FOK
      INTEGER NTYPE, NLEN, FTYPE, FLEN
C
C     Configuration information for these routines
C
      INTEGER NSIZE(6)
      INTEGER FSIZE(6)
      COMMON /G@IEG2CRI/ NSIZE, FSIZE
      SAVE   /G@IEG2CRI/
C
C     The following masks indicate the allowable values for the NATLEN
C     and FORLEN arguments.  External routines (e.g., implicit data
C     conversion) can query these values to determine acceptable sizes.
C     Each bit corresponds to a accepted size (in bytes).  Thus, the
C     rightmost bit (bit 1), if set, indicates that a length of 1 byte
C     (8 bits) is valid.
C
      DATA FSIZE /
#ifdef  _CRAYMPP
     1                 100210B,   ! Typeless:  128-, 64- and 32-bit
#else
     1            20000100200B,   ! Typeless:  256-, 128- and 64-bit
#endif
     2                    213B,   ! Integer:  64-, 32-, 16- and 8-bit
     3                 100210B,   ! Real:  128-, 64- and 32-bit
#ifdef  _CRAYMPP
     4                 100200B,   ! Complex:  128- and 64-bit
#else
     4            20000100200B,   ! Complex:  256-, 128- and 64-bit
#endif
     5                    213B,   ! Logical:  64-, 32-, 16- and 8-bit
     6                    001B/   ! Character:  8-bit
C
      DATA NSIZE /
#ifdef  _CRAYMPP
     1                 100210B,   ! Typeless:  128-, 64- and 32-bit
#else
     1            20000100200B,   ! Typeless:  256-, 128- and 64-bit
#endif
     2                    210B,   ! Integer:  64- and 32-bit
     3                 100210B,   ! Real:  128-, 64- and 32-bit
#ifdef  _CRAYMPP
     4                 100200B,   ! Complex:  128- and 64-bit
#else
     4            20000100200B,   ! Complex:  256-, 128- and 64-bit
#endif
     5                    210B,   ! Logical:  64- and 32-bit
     6                    001B/   ! Character:  8-bit
C
      IEG2CRI_AUXILIARY_FUNCTION$ = .TRUE.
      RETURN
C
      ENTRY IEG2CRI_NATIVE_TYPE_OK$(NTYPE, NLEN) RESULT (NOK)
      IF (IAND(ISHFT(1,((NLEN/8)-1)), NSIZE(NTYPE)) .NE. 0) THEN
        NOK = .TRUE.
      ELSE
        NOK = .FALSE.
      ENDIF
      RETURN
C
      ENTRY IEG2CRI_FOREIGN_TYPE_OK$(FTYPE, FLEN) RESULT (FOK)
      IF (IAND(ISHFT(1,((FLEN/8)-1)), FSIZE(FTYPE)) .NE. 0) THEN
        FOK = .TRUE.
      ELSE
        FOK = .FALSE.
      ENDIF
      RETURN
C
      END
