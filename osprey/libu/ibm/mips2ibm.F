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

      INTEGER FUNCTION MIPS2IBM(
     +TYPE, NUM, FORN, BITOFF, NATIVE, STRIDE, NATLEN, FORLEN, NATIVECH)

      IMPLICIT NONE

C     This function converts MIPS IEEE Fortran data types to IBM Fortran
C     data types.
C
C     The returned function value is as follows:
C
C         <0  Parameter error; no translation performed
C             -1      Parameter error; too few arguments or NATIVECH
C                     not specified with TYPE = 6 (IEEE only).
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
C               and be 8, 16, 32, 64, 128 or 256.
C           2 = integer (8-, 16-, 32- or 64-bit twos-complement to 8-,
C		16-, 32- or 64-bit twos-complement).
C           3 = real (32-, 64- or 128-bit IEEE floating-point to 32-,
C		64- or 128-bit IBM floating-point)
C           4 = complex (2 x 32-, 64- or 128-bit IEEE floating-point
C               to 2 x 32-, 64- or 128-bit IBM floating-point).
C           5 = logical (8-, 16-, 32- or 64-bit zero/nonzero logical to
C               8-, 16-, 32- or 64-bit zero/nonzero logical).
C           6 = character (ASCII to EBCDIC)
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
      PARAMETER        (GRANSZ = 32)   !  Smallest native data granularity

      INTEGER           TEMPSZ
      PARAMETER        (TEMPSZ = 2048) !  Size of temporary buffer

      INTEGER           WORDSZ
      PARAMETER        (WORDSZ = 64)   !  Size of word

      INTEGER           WORDSZM1
      PARAMETER        (WORDSZM1 = WORDSZ - 1)

      INTEGER           WORDSZB
      PARAMETER        (WORDSZB = 6)   !  Log (base 2) of WORDSZ

      INTEGER           IINC, MINC
      PARAMETER        (IINC = 1)

      INTEGER (KIND=8)  IOFF, MOFF, ZERO
      PARAMETER        (MOFF = 0)
      PARAMETER        (ZERO = 0)

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

      INTEGER (KIND=1)  GI8O$, SI8O$, TI8O$
      INTEGER (KIND=2)  GI16O$, SI16O$
      INTEGER (KIND=4)  GI32O$

C     These floating-point functions are mostly resizing functions,
C     but they also handle flushing of denorms and endian swaps,
C     when and where necessary.

      REAL    (KIND=4)  SSIBM$, DSIBM$, QSIBM$
      REAL    (KIND=8)  SDIBM$, DDIBM$, QDIBM$
      REAL    (KIND=16) SQIBM$, DQIBM$, QQIBM$

C     MIPS double double -> IEEE 128-bit conversion function

      REAL    (KIND=16) IEM128O$

C     Conversion function control flags and overflow counts

      INTEGER (KIND=4)         GN32OF
      INTEGER (KIND=4)         IBMOF
           COMMON  /T$GENERIC/ GN32OF
           COMMON  /T$IBM/     IBMOF

C     Aliasing pointers

      INTEGER (KIND=8)  NIBM(0:*), NMIPS(0:*)
      INTEGER (KIND=8)  IBMI(0:*), MIPSI(0:*)
      INTEGER (KIND=4)  IBMJ(0:*), MIPSJ(0:*)
      INTEGER (KIND=2)  IBMK(0:*), MIPSK(0:*)
      INTEGER (KIND=1)  IBMH(0:*), MIPSH(0:*)
      REAL    (KIND=4)  IBMS(0:*), MIPSS(0:*)
      REAL    (KIND=8)  IBMD(0:*), MIPSD(0:*)
      REAL    (KIND=16) IBMQ(0:*), MIPSQ(0:*)
      COMPLEX (KIND=4)  IBMF(0:*), MIPSF(0:*)
      COMPLEX (KIND=8)  IBMC(0:*), MIPSC(0:*)
      COMPLEX (KIND=16) IBME(0:*), MIPSE(0:*)
      LOGICAL (KIND=8)  IBML(0:*), MIPSL(0:*)
      LOGICAL (KIND=4)  IBMM(0:*), MIPSM(0:*)
      LOGICAL (KIND=2)  IBMN(0:*), MIPSN(0:*)
      LOGICAL (KIND=1)  IBMO(0:*), MIPSO(0:*)

      POINTER ( IPTR, NIBM), ( MPTR, NMIPS)
      POINTER (IIPTR, IBMI), (MIPTR, MIPSI)
      POINTER (IJPTR, IBMJ), (MJPTR, MIPSJ)
      POINTER (IKPTR, IBMK), (MKPTR, MIPSK)
      POINTER (IHPTR, IBMH), (MHPTR, MIPSH)
      POINTER (IDPTR, IBMD), (MDPTR, MIPSD)
      POINTER (ISPTR, IBMS), (MSPTR, MIPSS)
      POINTER (IQPTR, IBMQ), (MQPTR, MIPSQ)
      POINTER (ICPTR, IBMC), (MCPTR, MIPSC)
      POINTER (IFPTR, IBMF), (MFPTR, MIPSF)
      POINTER (IEPTR, IBME), (MEPTR, MIPSE)
      POINTER (ILPTR, IBML), (MLPTR, MIPSL)
      POINTER (IMPTR, IBMM), (MMPTR, MIPSM)
      POINTER (INPTR, IBMN), (MNPTR, MIPSN)
      POINTER (IOPTR, IBMO), (MOPTR, MIPSO)

      INTEGER           CHNK, II, ILENB, I, MI, MLENB, INDX
      INTEGER           LEFT, LOGB2, NMBR, REMR, SCALB, SHFT
      INTEGER (KIND=8)  CHLEN, ICOFF, ILEN, MBTL, MCOFF, MLEN
      INTEGER (KIND=8)  MWAD, SCAL, TEMP(0:TEMPSZ-1), ITMP
      LOGICAL           MIPS2IBM_NATIVE_TYPE_OK$
      LOGICAL           MIPS2IBM_FOREIGN_TYPE_OK$
      LOGICAL           POWER_OF_TWO

      EXTERNAL AS2EB$
      EXTERNAL MIPS2IBM_NATIVE_TYPE_OK$, MIPS2IBM_FOREIGN_TYPE_OK$
      EXTERNAL MOVBITZ

C     Statement function to compute the Log (base 2) of I

      LOGB2(SCAL) = WORDSZM1 - LEADZ(SCAL)

C     Statement function to determine if N is a power of two

      POWER_OF_TWO(I) = ((IAND(I, I-1) .EQ. 0) .AND. (I .NE. 0))

C     Check argument validity

      IF (NUM .LT. 0) GOTO 9003

      IF ((BITOFF .LT. 0) .OR. (BITOFF .GE. WORDSZ)) GOTO 9004

      IF ((TYPE .LT. 1) .OR. (TYPE .GT. 6)) GOTO 9002

C     Ensure lengths are powers of two and otherwise valid

      IF (.NOT. POWER_OF_TWO(NATLEN)) GOTO 9005
      IF (.NOT. MIPS2IBM_NATIVE_TYPE_OK$(TYPE, NATLEN)) GOTO 9005

      IF (.NOT. POWER_OF_TWO(FORLEN)) GOTO 9006
      IF (.NOT. MIPS2IBM_FOREIGN_TYPE_OK$(TYPE, FORLEN)) GOTO 9006

      II     = 0                        !  Set indicies
      MI     = 0
      GN32OF = 0                        !  Clear overflow counts
      IBMOF  = 0
      NMBR   = NUM

      IF (NMBR .EQ. 0) GOTO 9000

      MPTR   = LOC(NATIVE(0))
      IPTR   = LOC(FORN(0))

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

      MIPTR  = MPTR
      MJPTR  = MPTR
      MKPTR  = MPTR
      MHPTR  = MPTR
      MDPTR  = MPTR
      MSPTR  = MPTR
      MQPTR  = MPTR
      MCPTR  = MPTR
      MFPTR  = MPTR
      MEPTR  = MPTR
      MLPTR  = MPTR
      MMPTR  = MPTR
      MNPTR  = MPTR
      MOPTR  = MPTR

C     Set lengths, stride and bit offsets

      ILEN   = FORLEN
      IOFF   = BITOFF
      MINC   = STRIDE
      MLEN   = NATLEN

      ILENB  = LOGB2(ILEN)              !  Log (base 2) of ILEN
      MLENB  = LOGB2(MLEN)              !  Log (base 2) of MLEN

      GOTO (100,200,300,400,500,600), TYPE

C     TYPE = 1  (Typeless)                                  ************
C
C     Convert MIPS typeless to IBM typeless
C
C     MIPS    |                            IBM
C             |   8-bit    16-bit    32-bit    64-bit   128-bit   256-bit
C     --------+-----------------------------------------------------------
C      8-bit  |   copy  | INVALID | INVALID | INVALID | INVALID | INVALID
C             +---------+---------+---------+---------+---------+---------
C     16-bit  | INVALID |   copy  | INVALID | INVALID | INVALID | INVALID
C             +---------+---------+---------+---------+---------+---------
C     32-bit  | INVALID | INVALID |   copy  | INVALID | INVALID | INVALID
C             +---------+---------+---------+---------+---------+---------
C     64-bit  | INVALID | INVALID | INVALID |   copy  | INVALID | INVALID
C             +---------+---------+---------+---------+---------+---------
C     128-bit | INVALID | INVALID | INVALID | INVALID |   copy  | INVALID
C             +---------+---------+---------+---------+---------+---------
C     256-bit | INVALID | INVALID | INVALID | INVALID | INVALID |  copy
C     --------+-----------------------------------------------------------

  100 CONTINUE

      IF (ILEN .NE. MLEN) GOTO 9008

C     Special case a simple block move

      IF (STRIDE .EQ. 1 .AND. MLEN .EQ. ILEN) THEN

        CALL MOVBITZ(NMIPS(0), MOFF, NMBR * MLEN, NIBM(0), IOFF)
        GOTO 9000

      ENDIF

C     If output array is not word-aligned, then the output from the
C     conversion functions must be redirected to a temporary array
C     prior to the final transfer.

      LEFT  = NMBR

      IF (BITOFF .NE. 0) THEN       !  Strip mine

        IF (MOFF .NE. 0) THEN
          MIPTR = LOC(TEMP(0))
          MJPTR = LOC(TEMP(0))
          MKPTR = LOC(TEMP(0))
          MHPTR = LOC(TEMP(0))
          MDPTR = LOC(TEMP(0))
          MSPTR = LOC(TEMP(0))
          MQPTR = LOC(TEMP(0))
          MCPTR = LOC(TEMP(0))
          MFPTR = LOC(TEMP(0))
          MEPTR = LOC(TEMP(0))
          MLPTR = LOC(TEMP(0))
          MMPTR = LOC(TEMP(0))
          MNPTR = LOC(TEMP(0))
          MOPTR = LOC(TEMP(0))
          CHNK  = ISHFT(ISHFT(TEMPSZ, WORDSZB), -MLENB)
        ELSE  !  (IOFF .NE. 0)
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
        ENDIF

        NMBR  = MOD(NMBR, CHNK)

        IF (NMBR .EQ. 0) NMBR  = CHNK    !  Size of first chunk

        INDX  = 0

      ENDIF

C
C     Main loop for Typeless conversions
C

      DO WHILE (LEFT .GT. 0)

        IF (MOFF .NE. 0) THEN
          CALL MOVBITZ(NMIPS(INDX), MOFF, NMBR * MLEN, TEMP(0), ZERO)
          MI    = 0
        ENDIF

        SELECT CASE (ILEN)
          
          CASE (  8)                !  ILEN .EQ.   8 .AND. MLEN .EQ.   8
            DO I = 0, NMBR - 1
              IBMH(II) = MIPSH(MI)
              II       = II + IINC
              MI       = MI + MINC
            ENDDO

          CASE ( 16)                !  ILEN .EQ.  16 .AND. MLEN .EQ.  16
            DO I = 0, NMBR - 1
              IBMK(II) = MIPSK(MI)
              II       = II + IINC
              MI       = MI + MINC
            ENDDO

          CASE ( 32)                !  ILEN .EQ.  32 .AND. MLEN .EQ.  32
            DO I = 0, NMBR - 1
              IBMJ(II) = MIPSJ(MI)
              II       = II + IINC
              MI       = MI + MINC
            ENDDO

          CASE ( 64)                !  ILEN .EQ.  64 .AND. MLEN .EQ.  64
            DO I = 0, NMBR - 1
              IBMI(II) = MIPSI(MI)
              II       = II + IINC
              MI       = MI + MINC
            ENDDO

          CASE (128)                !  ILEN .EQ. 128 .AND. MLEN .EQ. 128
            DO I = 0, NMBR - 1
              IBMQ(II) = MIPSQ(MI)
              II       = II + IINC
              MI       = MI + MINC
            ENDDO

          CASE (256)                !  ILEN .EQ. 256 .AND. MLEN .EQ. 256
            DO I = 0, NMBR - 1
              IBME(II) = MIPSE(MI)
              II       = II + IINC
              MI       = MI + MINC
            ENDDO

        END SELECT

        LEFT  = LEFT - NMBR

        IF (BITOFF .NE. 0) THEN       !  Complete strip mine

          IF (IOFF .NE. 0) THEN       !  Complete transfer
            CALL MOVBITZ(TEMP(0), ZERO, NMBR * ILEN, NIBM(INDX), IOFF)
            II    = 0
          ENDIF

          INDX  = INDX + NMBR
          NMBR  = CHNK
        ENDIF

      ENDDO

      GOTO 9000

C     TYPE = 2  (Integer)                                   ************
C
C     Convert MIPS integers to IBM integers
C
C     MIPS              |                   IBM
C                       |  8-bit     16-bit     32-bit     64-bit
C     ------------------+------------------------------------------
C      8-bit INTEGER    |   copy  |  resize  |  resize  |  resize
C                       +---------+----------+----------+----------
C     16-bit INTEGER    |  resize |   copy   |  resize  |  resize
C                       +---------+----------+----------+----------
C     32-bit INTEGER    |  resize |  resize  |   copy   |  resize
C                       +---------+----------+----------+----------
C     64-bit INTEGER    |  resize |  resize  |  resize  |   copy
C     ------------------+------------------------------------------

  200 CONTINUE

C     Special case a simple block move

      IF (STRIDE .EQ. 1 .AND. MLEN .EQ. ILEN ) THEN

        CALL MOVBITZ(NMIPS(0), MOFF, NMBR * MLEN, NIBM(0), IOFF)
        GOTO 9000

      ENDIF

C     If output array is not word-aligned, then the output from the
C     conversion functions must be redirected to a temporary array
C     prior to the final transfer.

      LEFT  = NMBR

      IF (BITOFF .NE. 0) THEN       !  Strip mine

        IF (MOFF .NE. 0) THEN
          MIPTR = LOC(TEMP(0))
          MJPTR = LOC(TEMP(0))
          MKPTR = LOC(TEMP(0))
          MHPTR = LOC(TEMP(0))
          MDPTR = LOC(TEMP(0))
          MSPTR = LOC(TEMP(0))
          MQPTR = LOC(TEMP(0))
          MCPTR = LOC(TEMP(0))
          MFPTR = LOC(TEMP(0))
          MEPTR = LOC(TEMP(0))
          MLPTR = LOC(TEMP(0))
          MMPTR = LOC(TEMP(0))
          MNPTR = LOC(TEMP(0))
          MOPTR = LOC(TEMP(0))
          CHNK  = ISHFT(ISHFT(TEMPSZ, WORDSZB), -MLENB)
        ELSE  !  (IOFF .NE. 0)
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
        ENDIF

        NMBR  = MOD(NMBR, CHNK)

        IF (NMBR .EQ. 0) NMBR  = CHNK    !  Size of first chunk

        INDX  = 0

      ENDIF

C
C     Main loop for Integer conversions
C

      DO WHILE (LEFT .GT. 0)

        IF (MOFF .NE. 0) THEN
          CALL MOVBITZ(NMIPS(INDX), MOFF, NMBR * MLEN, TEMP(0), ZERO)
          MI    = 0
        ENDIF

        SELECT CASE (ILEN)
          
          CASE (8)                  !  ILEN .EQ.  8

          SELECT CASE (MLEN)

          CASE (8)                  !  ILEN .EQ.  8 .AND. MLEN .EQ.  8
            DO I = 0, NMBR - 1
              IBMH(II) = MIPSH(MI)
              II       = II + IINC
              MI       = MI + MINC
            ENDDO

          CASE (16)                 !  ILEN .EQ.  8 .AND. MLEN .EQ. 16
            DO I = 0, NMBR - 1
              IBMH(II) = TI8O$(MIPSK(MI))
              II       = II + IINC
              MI       = MI + MINC
            ENDDO

          CASE (32)                 !  ILEN .EQ.  8 .AND. MLEN .EQ. 32
            DO I = 0, NMBR - 1
              IBMH(II) = SI8O$(MIPSJ(MI))
              II       = II + IINC
              MI       = MI + MINC
            ENDDO

          CASE (64)                 !  ILEN .EQ.  8 .AND. MLEN .EQ. 64
            DO I = 0, NMBR - 1
              IBMH(II) = GI8O$(MIPSI(MI))
              II       = II + IINC
              MI       = MI + MINC
            ENDDO

          END SELECT

          CASE (16)                 !  ILEN .EQ. 16

          SELECT CASE (MLEN)

          CASE (8)                  !  ILEN .EQ. 16 .AND. MLEN .EQ.  8
            DO I = 0, NMBR - 1
              IBMK(II) = INT(MIPSH(MI),KIND=2)
              II       = II + IINC
              MI       = MI + MINC
            ENDDO

          CASE (16)                 !  ILEN .EQ. 16 .AND. MLEN .EQ. 16
            DO I = 0, NMBR - 1
              IBMK(II) = MIPSK(MI)
              II       = II + IINC
              MI       = MI + MINC
            ENDDO

          CASE (32)                 !  ILEN .EQ. 16 .AND. MLEN .EQ. 32
            DO I = 0, NMBR - 1
              IBMK(II) = SI16O$(MIPSJ(MI))
              II       = II + IINC
              MI       = MI + MINC
            ENDDO

          CASE (64)                 !  ILEN .EQ. 16 .AND. MLEN .EQ. 64
            DO I = 0, NMBR - 1
              IBMK(II) = GI16O$(MIPSI(MI))
              II       = II + IINC
              MI       = MI + MINC
            ENDDO

          END SELECT

          CASE (32)                 !  ILEN .EQ. 32

          SELECT CASE (MLEN)

          CASE ( 8)                 !  ILEN .EQ. 32 .AND. MLEN .EQ.  8
            DO I = 0, NMBR - 1
              IBMJ(II) = INT(MIPSH(MI),KIND=4)
              II       = II + IINC
              MI       = MI + MINC
            ENDDO

          CASE (16)                 !  ILEN .EQ. 32 .AND. MLEN .EQ. 16
            DO I = 0, NMBR - 1
              IBMJ(II) = INT(MIPSK(MI),KIND=4)
              II       = II + IINC
              MI       = MI + MINC
            ENDDO

          CASE (32)                 !  ILEN .EQ. 32 .AND. MLEN .EQ. 32
            DO I = 0, NMBR - 1
              IBMJ(II) = MIPSJ(MI)
              II       = II + IINC
              MI       = MI + MINC
            ENDDO

          CASE (64)                 !  ILEN .EQ. 32 .AND. MLEN .EQ. 64
            DO I = 0, NMBR - 1
              IBMJ(II) = GI32O$(MIPSI(MI))
              II       = II + IINC
              MI       = MI + MINC
            ENDDO

          END SELECT

          CASE (64)                 !  ILEN .EQ. 64

          SELECT CASE (MLEN)

          CASE ( 8)                 !  ILEN .EQ. 64 .AND. MLEN .EQ.  8
            DO I = 0, NMBR - 1
              IBMI(II) = INT(MIPSH(MI),KIND=8)
              II       = II + IINC
              MI       = MI + MINC
            ENDDO

          CASE (16)                 !  ILEN .EQ. 64 .AND. MLEN .EQ. 16
            DO I = 0, NMBR - 1
              IBMI(II) = INT(MIPSK(MI),KIND=8)
              II       = II + IINC
              MI       = MI + MINC
            ENDDO

          CASE (32)                 !  ILEN .EQ. 64 .AND. MLEN .EQ. 32
            DO I = 0, NMBR - 1
              IBMI(II) = INT(MIPSJ(MI),KIND=8)
              II       = II + IINC
              MI       = MI + MINC
            ENDDO

          CASE (64)                 !  ILEN .EQ. 64 .AND. MLEN .EQ. 64
            DO I = 0, NMBR - 1
              IBMI(II) = MIPSI(MI)
              II       = II + IINC
              MI       = MI + MINC
            ENDDO

          END SELECT

        END SELECT

        LEFT  = LEFT - NMBR

        IF (BITOFF .NE. 0) THEN       !  Complete strip mine

          IF (IOFF .NE. 0) THEN       !  Complete transfer
            CALL MOVBITZ(TEMP(0), ZERO, NMBR * ILEN, NIBM(INDX), IOFF)
            II    = 0
          ENDIF

          INDX  = INDX + NMBR
          NMBR  = CHNK
        ENDIF

      ENDDO

      GOTO 9000

C     TYPE = 3  (Real)                                      ************
C
C     Convert MIPS IEEE floating-point numbers to IBM floating-point
C     numbers
C
C     MIPS IEEE         |              IBM
C                       |  32-bit     64-bit    128-bit
C                       | (single)   (double)    (quad)
C                       |  KIND=4     KIND=8    KIND=16
C     ------------------+--------------------------------
C     32-bit (single)   |  SSIBM$  |  SDIBM$  |  SQIBM$
C                       +----------+----------+----------
C     64-bit (double)   |  DSIBM$  |  DDIBM$  |  DQIBM$
C                       +----------+----------+----------
C     128-bit (quad)    |  QSIBM$  |  QDIBM$  |  QQIBM$
C                        --------------------------------

  300 CONTINUE

C     If output array is not word-aligned, then the output from the
C     conversion functions must be redirected to a temporary array
C     prior to the final transfer.

      LEFT  = NMBR

      IF (BITOFF .NE. 0) THEN       !  Strip mine

        IF (MOFF .NE. 0) THEN
          MIPTR = LOC(TEMP(0))
          MJPTR = LOC(TEMP(0))
          MKPTR = LOC(TEMP(0))
          MHPTR = LOC(TEMP(0))
          MDPTR = LOC(TEMP(0))
          MSPTR = LOC(TEMP(0))
          MQPTR = LOC(TEMP(0))
          MCPTR = LOC(TEMP(0))
          MFPTR = LOC(TEMP(0))
          MEPTR = LOC(TEMP(0))
          MLPTR = LOC(TEMP(0))
          MMPTR = LOC(TEMP(0))
          MNPTR = LOC(TEMP(0))
          MOPTR = LOC(TEMP(0))
          CHNK  = ISHFT(ISHFT(TEMPSZ, WORDSZB), -MLENB)
        ELSE  !  (IOFF .NE. 0)
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
        ENDIF

        NMBR  = MOD(NMBR, CHNK)

        IF (NMBR .EQ. 0) NMBR  = CHNK   !  Size of first chunk

        INDX  = 0
      ENDIF

C
C     Main loop for Real (floating-point) conversions
C

      DO WHILE (LEFT .GT. 0)

        IF (MOFF .NE. 0) THEN
          CALL MOVBITZ(NMIPS(INDX), MOFF, NMBR * MLEN, TEMP(0), ZERO)
          MI    = 0
        ENDIF

        SELECT CASE (ILEN)

          CASE (32)                 !  ILEN .EQ. 32

          SELECT CASE (MLEN)

            CASE (32)               !  ILEN .EQ. 32 .AND. MLEN .EQ. 32
              DO I = 0, NMBR - 1
                IBMS(II) = SSIBM$(MIPSS(MI))
                II       = II + IINC
                MI       = MI + MINC
              ENDDO

            CASE (64)               !  ILEN .EQ. 32 .AND. MLEN .EQ. 64
              DO I = 0, NMBR - 1
                IBMS(II) = DSIBM$(MIPSD(MI))
                II       = II + IINC
                MI       = MI + MINC
              ENDDO

            CASE (128)              !  ILEN .EQ. 32 .AND. MLEN .EQ. 128
              DO I = 0, NMBR - 1
                IBMS(II) = QSIBM$(IEM128O$(MIPSQ(MI)))
                II       = II + IINC
                MI       = MI + MINC
              ENDDO

          END SELECT

          CASE (64)                 !  ILEN .EQ. 64

          SELECT CASE (MLEN)

            CASE (32)               !  ILEN .EQ. 64 .AND. MLEN .EQ. 32
              DO I = 0, NMBR - 1
                IBMD(II) = SDIBM$(MIPSS(MI))
                II       = II + IINC
                MI       = MI + MINC
              ENDDO

            CASE (64)               !  ILEN .EQ. 64 .AND. MLEN .EQ. 64
              DO I = 0, NMBR - 1
                IBMD(II) = DDIBM$(MIPSD(MI))
                II       = II + IINC
                MI       = MI + MINC
              ENDDO

            CASE (128)              !  ILEN .EQ. 64 .AND. MLEN .EQ. 128
              DO I = 0, NMBR - 1
                IBMD(II) = QDIBM$(IEM128O$(MIPSQ(MI)))
                II       = II + IINC
                MI       = MI + MINC
              ENDDO

          END SELECT

          CASE (128)                !  ILEN .EQ. 128

          SELECT CASE (MLEN)

            CASE (32)               !  ILEN .EQ. 128 .AND. MLEN .EQ. 32
              DO I = 0, NMBR - 1
                IBMQ(II) = SQIBM$(MIPSS(MI))
                II       = II + IINC
                MI       = MI + MINC
              ENDDO

            CASE (64)               !  ILEN .EQ. 128 .AND. MLEN .EQ. 64
              DO I = 0, NMBR - 1
                IBMQ(II) = DQIBM$(MIPSD(MI))
                II       = II + IINC
                MI       = MI + MINC
              ENDDO

            CASE (128)              !  ILEN .EQ. 128 .AND. MLEN .EQ. 128
              DO I = 0, NMBR - 1
                IBMQ(II) = QQIBM$(IEM128O$(MIPSQ(MI)))
                II       = II + IINC
                MI       = MI + MINC
              ENDDO

          END SELECT

        END SELECT

        LEFT  = LEFT - NMBR

        IF (BITOFF .NE. 0) THEN       !  Complete strip mine

          IF (IOFF .NE. 0) THEN       !  Complete transfer
            CALL MOVBITZ(TEMP(0), ZERO, NMBR * ILEN, NIBM(INDX), IOFF)
            II    = 0
          ENDIF

          INDX  = INDX + NMBR
          NMBR  = CHNK
        ENDIF

      ENDDO

      GOTO 9000

C     TYPE = 4  (Complex)                                   ************
C
C     Convert MIPS IEEE complex floating-point numbers to IBM complex
C     floating-point numbers
C
C     MIPS IEEE         |                 IBM
C                       |  2x32-bit     2x64-bit     2x128-bit
C                       |  (single)     (double)      (quad)
C                       |   KIND=4       KIND=8       KIND=16
C     ------------------+---------------------------------------
C     2x32-bit (single) |  2xSSIBM$  |  2xSDIBM$  |  2xSQIBM$
C                       +------------+------------+-------------
C     2x64-bit (double) |  2xDSIBM$  |  2xDDIBM$  |  2xDQIBM$
C                       +------------+------------+-------------
C     2x128-bit (quad)  |  2xQSIBM$  |  2xQDIBM$  |  2xQQIBM$
C     ----------------+-----------------------------------------

  400 CONTINUE

C     If output array is not word-aligned, then the output from the
C     conversion functions must be redirected to a temporary array
C     prior to the final transfer.

      LEFT  = NMBR

      IF (BITOFF .NE. 0) THEN       !  Strip mine

        IF (MOFF .NE. 0) THEN
          MIPTR = LOC(TEMP(0))
          MJPTR = LOC(TEMP(0))
          MKPTR = LOC(TEMP(0))
          MHPTR = LOC(TEMP(0))
          MDPTR = LOC(TEMP(0))
          MSPTR = LOC(TEMP(0))
          MQPTR = LOC(TEMP(0))
          MCPTR = LOC(TEMP(0))
          MFPTR = LOC(TEMP(0))
          MEPTR = LOC(TEMP(0))
          MLPTR = LOC(TEMP(0))
          MMPTR = LOC(TEMP(0))
          MNPTR = LOC(TEMP(0))
          MOPTR = LOC(TEMP(0))
          CHNK  = ISHFT(ISHFT(TEMPSZ, WORDSZB), -MLENB)
        ELSE  !  (IOFF .NE. 0)
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
        ENDIF

        NMBR  = MOD(NMBR, CHNK)

        IF (NMBR .EQ. 0) NMBR  = CHNK   !  Size of first chunk

        INDX  = 0

      ENDIF

C
C     Main loop for Complex conversions
C

      DO WHILE (LEFT .GT. 0)

        IF (MOFF .NE. 0) THEN
          CALL MOVBITZ(NMIPS(INDX), MOFF, NMBR * MLEN, TEMP(0), 0)
          MI    = 0
        ENDIF

        SELECT CASE (ILEN)

        CASE ( 64)

          SELECT CASE (MLEN)

          CASE (64)               !  ILEN .EQ.  64 .AND. MLEN .EQ.  64
            DO I = 0, NMBR - 1
              IBMF(II) = CMPLX(SSIBM$(REAL (MIPSF(MI))),
     1                         SSIBM$(AIMAG(MIPSF(MI))),
     2                         KIND=4)
              II       = II + IINC
              MI       = MI + MINC
            ENDDO

          CASE (128)              !  ILEN .EQ.  64 .AND. MLEN .EQ. 128
            DO I = 0, NMBR - 1
              IBMF(II) = CMPLX(DSIBM$(REAL (MIPSC(MI))),
     1                         DSIBM$(AIMAG(MIPSC(MI))),
     2                         KIND=4)
              II       = II + IINC
              MI       = MI + MINC
            ENDDO

          CASE (256)              !  ILEN .EQ.  64 .AND. MLEN .EQ. 256
            DO I = 0, NMBR - 1
              IBMF(II) = CMPLX(QSIBM$(IEM128O$(REAL (MIPSE(MI)))),
     1                         QSIBM$(IEM128O$(AIMAG(MIPSE(MI)))),
     2                         KIND=4)
              II       = II + IINC
              MI       = MI + MINC
            ENDDO

          END SELECT

        CASE (128)

          SELECT CASE (MLEN)

          CASE ( 64)              !  ILEN .EQ. 128 .AND. MLEN .EQ.  64
            DO I = 0, NMBR - 1
              IBMC(II) = CMPLX(SDIBM$(REAL (MIPSF(MI))),
     1                         SDIBM$(AIMAG(MIPSF(MI))),
     2                         KIND=8)
              II       = II + IINC
              MI       = MI + MINC
            ENDDO

          CASE (128)              !  ILEN .EQ. 128 .AND. MLEN .EQ. 128
            DO I = 0, NMBR - 1
              IBMC(II) = CMPLX(DDIBM$(REAL (MIPSC(MI))),
     1                         DDIBM$(AIMAG(MIPSC(MI))),
     2                         KIND=8)
              II       = II + IINC
              MI       = MI + MINC
            ENDDO

          CASE (256)              !  ILEN .EQ. 128 .AND. MLEN .EQ. 256
            DO I = 0, NMBR - 1
              IBMC(II) = CMPLX(QDIBM$(IEM128O$(REAL (MIPSE(MI)))),
     1                         QDIBM$(IEM128O$(AIMAG(MIPSE(MI)))),
     2                         KIND=8)
              II       = II + IINC
              MI       = MI + MINC
            ENDDO

          END SELECT

        CASE (256)

          SELECT CASE (MLEN)

          CASE ( 64)              !  ILEN .EQ. 256 .AND. MLEN .EQ.  64
            DO I = 0, NMBR - 1
              IBME(II) = CMPLX(SQIBM$(REAL (MIPSF(MI))),
     1                         SQIBM$(AIMAG(MIPSF(MI))),
     2                         KIND=16)
              II       = II + IINC
              MI       = MI + MINC
            ENDDO

          CASE (128)              !  ILEN .EQ. 256 .AND. MLEN .EQ. 128
            DO I = 0, NMBR - 1
              IBME(II) = CMPLX(DQIBM$(REAL (MIPSC(MI))),
     1                         DQIBM$(AIMAG(MIPSC(MI))),
     2                         KIND=16)
              II        = II + IINC
              MI        = MI + MINC
            ENDDO

          CASE (256)              !  ILEN .EQ. 256 .AND. MLEN .EQ. 256
            DO I = 0, NMBR - 1
              IBME(II) = CMPLX(QQIBM$(IEM128O$(REAL (MIPSE(MI)))),
     1                         QQIBM$(IEM128O$(AIMAG(MIPSE(MI)))),
     2                         KIND=16)
              II       = II + IINC
              MI       = MI + MINC
            ENDDO

          END SELECT

        END SELECT

        LEFT  = LEFT - NMBR

        IF (BITOFF .NE. 0) THEN       !  Complete strip mine

          IF (IOFF .NE. 0) THEN       !  Complete transfer
            CALL MOVBITZ(TEMP(0), ZERO, NMBR * ILEN, NIBM(INDX), IOFF)
            II    = 0
          ENDIF

          INDX  = INDX + NMBR
          NMBR  = CHNK
        ENDIF

      ENDDO

      GOTO 9000

C     TYPE = 5  (Logical)                                   ************
C
C     Convert MIPS logicals to IBM logicals
C
C     MIPS              |                    IBM
C                       |   8-bit     16-bit     32-bit     64-bit
C     ------------------+-------------------------------------------
C      8-bit LOGICAL    |   copy   |  inline  |  inline  |  inline
C                       +----------+----------+----------+----------
C     16-bit LOGICAL    |  inline  |   copy   |  inline  |  inline
C                       +----------+----------+----------+----------
C     32-bit LOGICAL    |  inline  |  inline  |   copy   |  inline
C                       +----------+----------+----------+----------
C     64-bit LOGICAL    |  inline  |  inline  |  inline  |   copy
C     ------------------+-------------------------------------------

  500 CONTINUE

C     Special case a simple block move

      IF (STRIDE .EQ. 1 .AND. MLEN .EQ. ILEN) THEN

        CALL MOVBITZ(NMIPS(0), MOFF, NMBR * MLEN, NIBM(0), IOFF)
        GOTO 9000

      ENDIF

C     If output array is not word-aligned, then the output from the
C     conversion functions must be redirected to a temporary array
C     prior to the final transfer.

      LEFT  = NMBR

      IF (BITOFF .NE. 0) THEN       !  Strip mine

        IF (MOFF .NE. 0) THEN
          MIPTR = LOC(TEMP(0))
          MJPTR = LOC(TEMP(0))
          MKPTR = LOC(TEMP(0))
          MHPTR = LOC(TEMP(0))
          MDPTR = LOC(TEMP(0))
          MSPTR = LOC(TEMP(0))
          MQPTR = LOC(TEMP(0))
          MCPTR = LOC(TEMP(0))
          MFPTR = LOC(TEMP(0))
          MEPTR = LOC(TEMP(0))
          MLPTR = LOC(TEMP(0))
          MMPTR = LOC(TEMP(0))
          MNPTR = LOC(TEMP(0))
          MOPTR = LOC(TEMP(0))
          CHNK  = ISHFT(ISHFT(TEMPSZ, WORDSZB), -MLENB)
        ELSE  !  (IOFF .NE. 0)
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
        ENDIF

        NMBR  = MOD(NMBR, CHNK)

        IF (NMBR .EQ. 0) NMBR  = CHNK   !  Size of first chunk

        INDX  = 0

      ENDIF

C
C     Main loop for Logical conversions
C

      DO WHILE (LEFT .GT. 0)

        IF (MOFF .NE. 0) THEN
          CALL MOVBITZ(NMIPS(INDX), MOFF, NMBR * MLEN, TEMP(0), ZERO)
          MI    = 0
        ENDIF

        SELECT CASE (ILEN)

          CASE ( 8)                 !  ILEN .EQ.  8

          SELECT CASE (MLEN)

            CASE ( 8)               !  ILEN .EQ.  8 .AND. MLEN .EQ.  8
              DO I = 0, NMBR - 1
                IBMO(II) = LOGICAL(MIPSO(MI),KIND=1)
                II       = II + IINC
                MI       = MI + MINC
              ENDDO

            CASE (16)               !  ILEN .EQ.  8 .AND. MLEN .EQ. 16
              DO I = 0, NMBR - 1
                IBMO(II) = LOGICAL(MIPSN(MI),KIND=1)
                II       = II + IINC
                MI       = MI + MINC
              ENDDO

            CASE (32)               !  ILEN .EQ.  8 .AND. MLEN .EQ. 32
              DO I = 0, NMBR - 1
                IBMO(II) = LOGICAL(MIPSM(MI),KIND=1)
                II       = II + IINC
                MI       = MI + MINC
              ENDDO

            CASE (64)               !  ILEN .EQ.  8 .AND. MLEN .EQ. 64
              DO I = 0, NMBR - 1
                IBMO(II) = LOGICAL(MIPSL(MI),KIND=1)
                II       = II + IINC
                MI       = MI + MINC
              ENDDO

          END SELECT

          CASE (16)                 !  ILEN .EQ. 16

          SELECT CASE (MLEN)

            CASE ( 8)               !  ILEN .EQ. 16 .AND. MLEN .EQ.  8
              DO I = 0, NMBR - 1
                IBMN(II) = LOGICAL(MIPSO(MI),KIND=2)
                II       = II + IINC
                MI       = MI + MINC
              ENDDO

            CASE (16)               !  ILEN .EQ. 16 .AND. MLEN .EQ. 16
              DO I = 0, NMBR - 1
                IBMN(II) = LOGICAL(MIPSN(MI),KIND=2)
                II       = II + IINC
                MI       = MI + MINC
              ENDDO

            CASE (32)               !  ILEN .EQ. 16 .AND. MLEN .EQ. 32
              DO I = 0, NMBR - 1
                IBMN(II) = LOGICAL(MIPSM(MI),KIND=2)
                II       = II + IINC
                MI       = MI + MINC
              ENDDO

            CASE (64)               !  ILEN .EQ. 16 .AND. MLEN .EQ. 64
              DO I = 0, NMBR - 1
                IBMN(II) = LOGICAL(MIPSL(MI),KIND=2)
                II       = II + IINC
                MI       = MI + MINC
              ENDDO

          END SELECT

          CASE (32)                 !  ILEN .EQ. 32

          SELECT CASE (MLEN)

            CASE ( 8)               !  ILEN .EQ. 32 .AND. MLEN .EQ.  8
              DO I = 0, NMBR - 1
                IBMM(II) = LOGICAL(MIPSO(MI),KIND=4)
                II       = II + IINC
                MI       = MI + MINC
              ENDDO

            CASE (16)               !  ILEN .EQ. 32 .AND. MLEN .EQ. 16
              DO I = 0, NMBR - 1
                IBMM(II) = LOGICAL(MIPSN(MI),KIND=4)
                II       = II + IINC
                MI       = MI + MINC
              ENDDO

            CASE (32)               !  ILEN .EQ. 32 .AND. MLEN .EQ. 32
              DO I = 0, NMBR - 1
                IBMM(II) = LOGICAL(MIPSM(MI),KIND=4)
                II       = II + IINC
                MI       = MI + MINC
              ENDDO

            CASE (64)               !  ILEN .EQ. 32 .AND. MLEN .EQ. 64
              DO I = 0, NMBR - 1
                IBMM(II) = LOGICAL(MIPSL(MI),KIND=4)
                II       = II + IINC
                MI       = MI + MINC
              ENDDO

          END SELECT

          CASE (64)                 !  ILEN .EQ. 64

          SELECT CASE (MLEN)

            CASE ( 8)               !  ILEN .EQ. 64 .AND. MLEN .EQ.  8
              DO I = 0, NMBR - 1
                IBML(II) = LOGICAL(MIPSO(MI),KIND=8)
                II       = II + IINC
                MI       = MI + MINC
              ENDDO

            CASE (16)               !  ILEN .EQ. 64 .AND. MLEN .EQ. 16
              DO I = 0, NMBR - 1
                IBML(II) = LOGICAL(MIPSN(MI),KIND=8)
                II       = II + IINC
                MI       = MI + MINC
              ENDDO

            CASE (32)               !  ILEN .EQ. 64 .AND. MLEN .EQ. 32
              DO I = 0, NMBR - 1
                IBML(II) = LOGICAL(MIPSM(MI),KIND=8)
                II       = II + IINC
                MI       = MI + MINC
              ENDDO

            CASE (64)               !  ILEN .EQ. 64 .AND. MLEN .EQ. 64
              DO I = 0, NMBR - 1
                IBML(II) = LOGICAL(MIPSL(MI),KIND=8)
                II       = II + IINC
                MI       = MI + MINC
              ENDDO

          END SELECT

        END SELECT

        LEFT  = LEFT - NMBR

        IF (BITOFF .NE. 0) THEN       !  Complete strip mine

          IF (IOFF .NE. 0) THEN       !  Complete transfer
            CALL MOVBITZ(TEMP(0), ZERO, NMBR * ILEN, NIBM(INDX), IOFF)
            II    = 0
          ENDIF

          INDX  = INDX + NMBR
          NMBR  = CHNK
        ENDIF

      ENDDO

      GOTO 9000

C     TYPE = 6  (Character)                                 ************

  600 CONTINUE

      IF (ILEN .NE. MLEN) GOTO 9008

      CALL G$CHRPCK(NATIVECH, MWAD, MBTL, MCOFF)  ! Get character offset
      ICOFF = IOFF
      IIPTR = IPTR
      MIPTR = MWAD

      CHLEN = MLEN * LEN(NATIVECH(0))             ! Get character length

      IF (STRIDE .EQ. 1) THEN

C       CALL MOVBITZ(MIPSI(0), MCOFF, NMBR * CHLEN, IBMI(0), ICOFF)
        CALL AS2EB$(MIPSI(0), MCOFF/8, IBMI(0), ICOFF/8, (NMBR*CHLEN)/8)

      ELSE

        DO I = 0, NMBR - 1

C         CALL MOVBITZ(MIPSI(MI), MCOFF, CHLEN, IBMI(II), ICOFF)
          CALL AS2EB$(MIPSI(MI), MCOFF/8, IBMI(II), ICOFF/8, CHLEN/8)

          ICOFF = ICOFF + (CHLEN * IINC)
          II    = II + ISHFT(ICOFF, -WORDSZB)
          ICOFF = MOD(ICOFF, INT(WORDSZ, KIND=8))
          MCOFF = MCOFF + (CHLEN * MINC)
          MI    = MI + ISHFT(MCOFF, -WORDSZB)
          MCOFF = MOD(MCOFF, INT(WORDSZ, KIND=8))

        ENDDO

      ENDIF
      GOTO 9000

 9010 CONTINUE
      MIPS2IBM = -10              !  FORN must be 64-bit word-aligned
      GOTO 10000

 9009 CONTINUE
      MIPS2IBM = -9               !  NATIVE must be 64-bit word-aligned
      GOTO 10000

 9008 CONTINUE
      MIPS2IBM = -8               !  Parameter error, NATLEN/FORLEN invalid
      GOTO 10000

C9007 CONTINUE
C     MIPS2IBM = -7               !  Unable to allocate memory (unused in
C     GOTO 10000                  !  this version of the routine)

 9006 CONTINUE
      MIPS2IBM = -6               !  Parameter error, invalid FORLEN
      GOTO 10000

 9005 CONTINUE
      MIPS2IBM = -5               !  Parameter error, invalid NATLEN
      GOTO 10000

 9004 CONTINUE
      MIPS2IBM = -4               !  Parameter error, invalid BITOFF
      GOTO 10000

 9003 CONTINUE
      MIPS2IBM = -3               !  Parameter error, invalid NUM
      GOTO 10000

 9002 CONTINUE
      MIPS2IBM = -2               !  Parameter error, invalid TYPE
      GOTO 10000

 9001 CONTINUE
      MIPS2IBM = -1               !  Parameter error
      GOTO 10000

 9000 CONTINUE
      MIPS2IBM = GN32OF + IBMOF   !  Return error count
C     GOTO 10000

10000 CONTINUE
      RETURN

CDIR$ ID "@(#) libu/ibm/mips2ibm.F	92.1	09/29/99 20:40:37"
      END
C
C     Auxiliary functions
C
      LOGICAL FUNCTION MIPS2IBM_AUXILIARY_FUNCTION$()
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
      EXTERNAL MIPS2IBM_INIT
C
      COMMON /G$MIPS2IBM/ NSIZE, FSIZE
      SAVE   /G$MIPS2IBM/
C
      MIPS2IBM_AUXILIARY_FUNCTION$ = .TRUE.
      RETURN
C
      ENTRY MIPS2IBM_NATIVE_TYPE_OK$(NTYPE, NLEN) RESULT (NOK)
      IF (IAND(ISHFT(ONE,((NLEN/8)-1)), NSIZE(NTYPE)) .NE. 0) THEN
        NOK = .TRUE.
      ELSE
        NOK = .FALSE.
      ENDIF
      RETURN
C
      ENTRY MIPS2IBM_FOREIGN_TYPE_OK$(FTYPE, FLEN) RESULT (FOK)
      IF (IAND(ISHFT(ONE,((FLEN/8)-1)), FSIZE(FTYPE)) .NE. 0) THEN
        FOK = .TRUE.
      ELSE
        FOK = .FALSE.
      ENDIF
      RETURN
C
      END
C
      BLOCK DATA MIPS2IBM_INIT
C
      INTEGER (KIND=8) NSIZE(6)
      INTEGER (KIND=8) FSIZE(6)
      COMMON /G$MIPS2IBM/ NSIZE, FSIZE
      SAVE   /G$MIPS2IBM/
C
C     The following masks indicate the allowable values for the NATLEN
C     and FORLEN arguments.  External routines (e.g., implicit data
C     conversion) can query these values to determine acceptable sizes.
C     Each bit corresponds to a accepted size (in bytes).  Thus, the
C     rightmost bit (bit 1), if set, indicates that a length of 1 byte
C     (8 bits) is valid.
C
      DATA NSIZE /
     1          O'20000100213',   ! Typeless:  256-, 128-, 64-, 32-,
                                  !            16- and 8-bit
     2          O'        213',   ! Integer:  64-, 32-, 16- and 8-bit
     3          O'     100210',   ! Real:  128-, 64- and 32-bit
     4          O'20000100200',   ! Complex:  256-, 128- and 64-bit
     5          O'        213',   ! Logical:  64-, 32-, 16- and 8-bit
     6          O'        001'/   ! Character:  8-bit
C
      DATA FSIZE /
     1          O'20000100213',   ! Typeless:  256-, 128-, 64-, 32-,
                                  !            16- and 8-bit
     2          O'        213',   ! Integer:  64-, 32-, 16- and 8-bit
     3          O'     100210',   ! Real:  128-, 64- and 32-bit
     4          O'20000100200',   ! Complex:  256-, 128- and 64-bit
     5          O'        213',   ! Logical:  64-, 32-, 16- and 8-bit
     6          O'        001'/   ! Character:  8-bit
      END BLOCK DATA MIPS2IBM_INIT
