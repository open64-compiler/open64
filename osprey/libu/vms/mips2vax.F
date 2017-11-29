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

      INTEGER FUNCTION MIPS2VAX(
     +TYPE, NUM, FORN, BITOFF, NATIVE, STRIDE, NATLEN, FORLEN, NATIVECH)

      IMPLICIT NONE

C     This function converts MIPS IEEE Fortran data types to VAX Fortran
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
C           3 = real (32-, 64- or 128-bit IEEE floating-point to 32- or
C		64-bit VAX floating-point)
C           4 = complex (2 x 32-, 64- or 128-bit IEEE floating-point
C               to 2 x 32- or 128-bit VAX floating-point).
C           5 = logical (8-, 16-, 32- or 64-bit zero/nonzero logical to
C               8-, 16-, 32- or 64-bit zero/nonzero logical).
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
      PARAMETER        (GRANSZ = 32)   !  Smallest native data granularity

      INTEGER           TEMPSZ
      PARAMETER        (TEMPSZ = 2048) !  Size of temporary buffer

      INTEGER           WORDSZ
      PARAMETER        (WORDSZ = 64)   !  Size of word

      INTEGER           WORDSZM1
      PARAMETER        (WORDSZM1 = WORDSZ - 1)

      INTEGER           WORDSZB
      PARAMETER        (WORDSZB = 6)   !  Log (base 2) of WORDSZ

      INTEGER           VINC, MINC
      PARAMETER        (VINC = 1)

      INTEGER (KIND=8)  VOFF, MOFF, ZERO
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

      REAL    (KIND=4)  IS2VF$, ID2VF$, IQ2VF$
      REAL    (KIND=8)  IS2VD$, ID2VD$, IQ2VD$
      REAL    (KIND=16) IS2VG$, ID2VG$, IQ2VG$

C     These functions conditionally handle endian swaps.

      INTEGER (KIND=2)  CSWAP2$
      INTEGER (KIND=4)  CSWAP4$
      INTEGER (KIND=8)  CSWAP8$

C     MIPS double double -> IEEE 128-bit conversion function

      REAL    (KIND=16) IEM128O$

C     Conversion function control flags and overflow counts

      INTEGER (KIND=4)         GN32OF
      INTEGER (KIND=4)         VAXOF
      INTEGER (KIND=4)         SWAP
           COMMON  /T$GENERIC/ GN32OF
           COMMON  /T$VAX/     VAXOF
           COMMON  /T$ENDIAN/  SWAP

C     Aliasing pointers

      INTEGER (KIND=8)  NNVAX(0:*), NMIPS(0:*)
      INTEGER (KIND=8)  IVAXQ(0:*), MIPSI(0:*)
      INTEGER (KIND=4)  IVAXL(0:*), MIPSJ(0:*)
      INTEGER (KIND=2)  IVAXW(0:*), MIPSK(0:*)
      INTEGER (KIND=1)  IVAXB(0:*), MIPSH(0:*)
      REAL    (KIND=4)  RVAXF(0:*), MIPSS(0:*)
      REAL    (KIND=8)  RVAXD(0:*), MIPSD(0:*)
      REAL    (KIND=16) RVAXG(0:*), MIPSQ(0:*)
      COMPLEX (KIND=4)  CVAXF(0:*), MIPSF(0:*)
      COMPLEX (KIND=8)  CVAXD(0:*), MIPSC(0:*)
      COMPLEX (KIND=16) CVAXG(0:*), MIPSE(0:*)
      LOGICAL (KIND=8)  LVAXQ(0:*), MIPSL(0:*)
      LOGICAL (KIND=4)  LVAXL(0:*), MIPSM(0:*)
      LOGICAL (KIND=2)  LVAXW(0:*), MIPSN(0:*)
      LOGICAL (KIND=1)  LVAXB(0:*), MIPSO(0:*)

      POINTER ( VPTR, NNVAX), ( MPTR, NMIPS)
      POINTER (VIPTR, IVAXQ), (MIPTR, MIPSI)
      POINTER (VJPTR, IVAXL), (MJPTR, MIPSJ)
      POINTER (VKPTR, IVAXW), (MKPTR, MIPSK)
      POINTER (VHPTR, IVAXB), (MHPTR, MIPSH)
      POINTER (VDPTR, RVAXD), (MDPTR, MIPSD)
      POINTER (VSPTR, RVAXF), (MSPTR, MIPSS)
      POINTER (VQPTR, RVAXG), (MQPTR, MIPSQ)
      POINTER (VCPTR, CVAXD), (MCPTR, MIPSC)
      POINTER (VFPTR, CVAXF), (MFPTR, MIPSF)
      POINTER (VEPTR, CVAXG), (MEPTR, MIPSE)
      POINTER (VLPTR, LVAXQ), (MLPTR, MIPSL)
      POINTER (VMPTR, LVAXL), (MMPTR, MIPSM)
      POINTER (VNPTR, LVAXW), (MNPTR, MIPSN)
      POINTER (VOPTR, LVAXB), (MOPTR, MIPSO)

      INTEGER           CHNK, VI, VLENB, I, MI, MLENB, INDX
      INTEGER           LEFT, LOGB2, NMBR, REMR, SCALB, SHFT
      INTEGER (KIND=8)  CHLEN, VCOFF, VLEN, MBTL, MCOFF, MLEN
      INTEGER (KIND=8)  MWAD, SCAL, TEMP(0:TEMPSZ-1), ITMP
      LOGICAL           MIPS2VAX_NATIVE_TYPE_OK$
      LOGICAL           MIPS2VAX_FOREIGN_TYPE_OK$

      EXTERNAL MOVBITZ
      EXTERNAL MIPS2VAX_NATIVE_TYPE_OK$, MIPS2VAX_FOREIGN_TYPE_OK$

C     Statement function to compute the Log (base 2) of I

      LOGB2(SCAL) = WORDSZM1 - LEADZ(SCAL)

C     Check argument validity

      IF (NUM .LT. 0) GOTO 9003

      IF ((BITOFF .LT. 0) .OR. (BITOFF .GE. WORDSZ)) GOTO 9004

      IF ((TYPE .LT. 1) .OR. (TYPE .GT. 6)) GOTO 9002

C     Ensure lengths are powers of two and otherwise valid

      IF (POPCNT(NATLEN) .NE. 1) GOTO 9005
      IF (.NOT. MIPS2VAX_NATIVE_TYPE_OK$(TYPE, NATLEN)) GOTO 9005

      IF (POPCNT(FORLEN) .NE. 1) GOTO 9006
      IF (.NOT. MIPS2VAX_FOREIGN_TYPE_OK$(TYPE, FORLEN)) GOTO 9006

      VI     = 0                        !  Set indicies
      MI     = 0
      GN32OF = 0                        !  Clear overflow counts
      VAXOF  = 0
      SWAP   = 1                        !  Swap endian
      NMBR   = NUM

      IF (NMBR .EQ. 0) GOTO 9000

      MPTR   = LOC(NATIVE(0))
      VPTR   = LOC(FORN(0))

      VIPTR  = VPTR
      VJPTR  = VPTR
      VKPTR  = VPTR
      VHPTR  = VPTR
      VDPTR  = VPTR
      VSPTR  = VPTR
      VQPTR  = VPTR
      VCPTR  = VPTR
      VFPTR  = VPTR
      VEPTR  = VPTR
      VLPTR  = VPTR
      VMPTR  = VPTR
      VNPTR  = VPTR
      VOPTR  = VPTR

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

      VLEN   = FORLEN
      VOFF   = BITOFF
      MINC   = STRIDE
      MLEN   = NATLEN

      VLENB  = LOGB2(VLEN)              !  Log (base 2) of VLEN
      MLENB  = LOGB2(MLEN)              !  Log (base 2) of MLEN

      GOTO (100,200,300,400,500,600), TYPE

C     TYPE = 1  (Typeless)                                  ************
C
C     Convert MIPS typeless to VAX typeless
C
C     MIPS    |                            VAX
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
C     256-bit | INVALID | INVALID | INVALID | INVALID | INVALID |   copy
C     --------+-----------------------------------------------------------

  100 CONTINUE

      IF (VLEN .NE. MLEN) GOTO 9008

C     Special case a simple block move

      IF (STRIDE .EQ. 1) THEN

        CALL MOVBITZ(NMIPS(0), MOFF, NMBR * MLEN, NNVAX(0), VOFF)
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
        ELSE  !  (VOFF .NE. 0)
          VIPTR = LOC(TEMP(0))
          VJPTR = LOC(TEMP(0))
          VKPTR = LOC(TEMP(0))
          VHPTR = LOC(TEMP(0))
          VDPTR = LOC(TEMP(0))
          VSPTR = LOC(TEMP(0))
          VQPTR = LOC(TEMP(0))
          VCPTR = LOC(TEMP(0))
          VFPTR = LOC(TEMP(0))
          VEPTR = LOC(TEMP(0))
          VLPTR = LOC(TEMP(0))
          VMPTR = LOC(TEMP(0))
          VNPTR = LOC(TEMP(0))
          VOPTR = LOC(TEMP(0))
          CHNK  = ISHFT(ISHFT(TEMPSZ, WORDSZB), -VLENB)
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

        SELECT CASE (VLEN)
          
          CASE (  8)                !  VLEN .EQ.   8 .AND. MLEN .EQ.   8
            DO I = 0, NMBR - 1
              IVAXB(VI) = MIPSH(MI)
              VI        = VI + VINC
              MI        = MI + MINC
            ENDDO

          CASE ( 16)                !  VLEN .EQ.  16 .AND. MLEN .EQ.  16
            DO I = 0, NMBR - 1
              IVAXW(VI) = MIPSK(MI)
              VI        = VI + VINC
              MI        = MI + MINC
            ENDDO

          CASE ( 32)                !  VLEN .EQ.  32 .AND. MLEN .EQ.  32
            DO I = 0, NMBR - 1
              IVAXL(VI) = MIPSJ(MI)
              VI        = VI + VINC
              MI        = MI + MINC
            ENDDO

          CASE ( 64)                !  VLEN .EQ.  64 .AND. MLEN .EQ.  64
            DO I = 0, NMBR - 1
              IVAXQ(VI) = MIPSI(MI)
              VI        = VI + VINC
              MI        = MI + MINC
            ENDDO

          CASE (128)                !  VLEN .EQ. 128 .AND. MLEN .EQ. 128
            DO I = 0, NMBR - 1
              RVAXG(VI) = MIPSQ(MI)
              VI        = VI + VINC
              MI        = MI + MINC
            ENDDO

          CASE (256)                !  VLEN .EQ. 256 .AND. MLEN .EQ. 256
            DO I = 0, NMBR - 1
              CVAXG(VI) = MIPSE(MI)
              VI        = VI + VINC
              MI        = MI + MINC
            ENDDO

        END SELECT

        LEFT  = LEFT - NMBR

        IF (BITOFF .NE. 0) THEN       !  Complete strip mine

          IF (VOFF .NE. 0) THEN       !  Complete transfer
            CALL MOVBITZ(TEMP(0), ZERO, NMBR * VLEN, NNVAX(INDX), VOFF)
            VI    = 0
          ENDIF

          INDX  = INDX + NMBR
          NMBR  = CHNK
        ENDIF

      ENDDO

      GOTO 9000

C     TYPE = 2  (Integer)                                   ************
C
C     Convert MIPS integers to VAX integers
C
C     MIPS              |                  IEEE
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
        ELSE  !  (VOFF .NE. 0)
          VIPTR = LOC(TEMP(0))
          VJPTR = LOC(TEMP(0))
          VKPTR = LOC(TEMP(0))
          VHPTR = LOC(TEMP(0))
          VDPTR = LOC(TEMP(0))
          VSPTR = LOC(TEMP(0))
          VQPTR = LOC(TEMP(0))
          VCPTR = LOC(TEMP(0))
          VFPTR = LOC(TEMP(0))
          VEPTR = LOC(TEMP(0))
          VLPTR = LOC(TEMP(0))
          VMPTR = LOC(TEMP(0))
          VNPTR = LOC(TEMP(0))
          VOPTR = LOC(TEMP(0))
          CHNK  = ISHFT(ISHFT(TEMPSZ, WORDSZB), -VLENB)
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

        SELECT CASE (VLEN)
          
          CASE (8)                  !  VLEN .EQ.  8

          SELECT CASE (MLEN)

          CASE (8)                  !  VLEN .EQ.  8 .AND. MLEN .EQ.  8
            DO I = 0, NMBR - 1
              IVAXB(VI) = MIPSH(MI)
              VI        = VI + VINC
              MI        = MI + MINC
            ENDDO

          CASE (16)                 !  VLEN .EQ.  8 .AND. MLEN .EQ. 16
            DO I = 0, NMBR - 1
              IVAXB(VI) = TI8O$(MIPSK(MI))
              VI        = VI + VINC
              MI        = MI + MINC
            ENDDO

          CASE (32)                 !  VLEN .EQ.  8 .AND. MLEN .EQ. 32
            DO I = 0, NMBR - 1
              IVAXB(VI) = SI8O$(MIPSJ(MI))
              VI        = VI + VINC
              MI        = MI + MINC
            ENDDO

          CASE (64)                 !  VLEN .EQ.  8 .AND. MLEN .EQ. 64
            DO I = 0, NMBR - 1
              IVAXB(VI) = GI8O$(MIPSI(MI))
              VI        = VI + VINC
              MI        = MI + MINC
            ENDDO

          END SELECT

          CASE (16)                 !  VLEN .EQ. 16

          SELECT CASE (MLEN)

          CASE (8)                  !  VLEN .EQ. 16 .AND. MLEN .EQ.  8
            DO I = 0, NMBR - 1
              IVAXW(VI) = CSWAP2$(INT(MIPSH(MI),KIND=2))
              VI        = VI + VINC
              MI        = MI + MINC
            ENDDO

          CASE (16)                 !  VLEN .EQ. 16 .AND. MLEN .EQ. 16
            DO I = 0, NMBR - 1
              IVAXW(VI) = CSWAP2$(MIPSK(MI))
              VI        = VI + VINC
              MI        = MI + MINC
            ENDDO

          CASE (32)                 !  VLEN .EQ. 16 .AND. MLEN .EQ. 32
            DO I = 0, NMBR - 1
              IVAXW(VI) = CSWAP2$(SI16O$(MIPSJ(MI)))
              VI        = VI + VINC
              MI        = MI + MINC
            ENDDO

          CASE (64)                 !  VLEN .EQ. 16 .AND. MLEN .EQ. 64
            DO I = 0, NMBR - 1
              IVAXW(VI) = CSWAP2$(GI16O$(MIPSI(MI)))
              VI        = VI + VINC
              MI        = MI + MINC
            ENDDO

          END SELECT

          CASE (32)                 !  VLEN .EQ. 32

          SELECT CASE (MLEN)

          CASE ( 8)                 !  VLEN .EQ. 32 .AND. MLEN .EQ.  8
            DO I = 0, NMBR - 1
              IVAXL(VI) = CSWAP4$(INT(MIPSH(MI),KIND=4))
              VI        = VI + VINC
              MI        = MI + MINC
            ENDDO

          CASE (16)                 !  VLEN .EQ. 32 .AND. MLEN .EQ. 16
            DO I = 0, NMBR - 1
              IVAXL(VI) = CSWAP4$(INT(MIPSK(MI),KIND=4))
              VI        = VI + VINC
              MI        = MI + MINC
            ENDDO

          CASE (32)                 !  VLEN .EQ. 32 .AND. MLEN .EQ. 32
            DO I = 0, NMBR - 1
              IVAXL(VI) = CSWAP4$(MIPSJ(MI))
              VI        = VI + VINC
              MI        = MI + MINC
            ENDDO

          CASE (64)                 !  VLEN .EQ. 32 .AND. MLEN .EQ. 64
            DO I = 0, NMBR - 1
              IVAXL(VI) = CSWAP4$(GI32O$(MIPSI(MI)))
              VI        = VI + VINC
              MI        = MI + MINC
            ENDDO

          END SELECT

          CASE (64)                 !  VLEN .EQ. 64

          SELECT CASE (MLEN)

          CASE ( 8)                 !  VLEN .EQ. 64 .AND. MLEN .EQ.  8
            DO I = 0, NMBR - 1
              IVAXQ(VI) = CSWAP8$(INT(MIPSH(MI),KIND=8))
              VI        = VI + VINC
              MI        = MI + MINC
            ENDDO

          CASE (16)                 !  VLEN .EQ. 64 .AND. MLEN .EQ. 16
            DO I = 0, NMBR - 1
              IVAXQ(VI) = CSWAP8$(INT(MIPSK(MI),KIND=8))
              VI        = VI + VINC
              MI        = MI + MINC
            ENDDO

          CASE (32)                 !  VLEN .EQ. 64 .AND. MLEN .EQ. 32
            DO I = 0, NMBR - 1
              IVAXQ(VI) = CSWAP8$(INT(MIPSJ(MI),KIND=8))
              VI        = VI + VINC
              MI        = MI + MINC
            ENDDO

          CASE (64)                 !  VLEN .EQ. 64 .AND. MLEN .EQ. 64
            DO I = 0, NMBR - 1
              IVAXQ(VI) = CSWAP8$(MIPSI(MI))
              VI        = VI + VINC
              MI        = MI + MINC
            ENDDO

          END SELECT

        END SELECT

        LEFT  = LEFT - NMBR

        IF (BITOFF .NE. 0) THEN       !  Complete strip mine

          IF (VOFF .NE. 0) THEN       !  Complete transfer
            CALL MOVBITZ(TEMP(0), ZERO, NMBR * VLEN, NNVAX(INDX), VOFF)
            VI    = 0
          ENDIF

          INDX  = INDX + NMBR
          NMBR  = CHNK
        ENDIF

      ENDDO

      GOTO 9000

C     TYPE = 3  (Real)                                      ************
C
C     Convert MIPS IEEE floating-point numbers to VAX floating-point
C     numbers
C
C     MIPS IEEE         |              VAX
C                       |  32-bit     64-bit     128-bit
C                       | (single)   (double)    (quad)
C                       |  KIND=4     KIND=8     KIND=16
C     ------------------+--------------------------------
C     32-bit (single)   |  IS2VF$  |  IS2VD$  |  IS2VG$
C                       +----------+----------+----------
C     64-bit (double)   |  ID2VF$  |  ID2VD$  |  ID2VG$
C                       +----------+----------+----------
C     128-bit (quad)    |  IQ2VF$  |  IQ2VD$  |  IQ2VG$
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
        ELSE  !  (VOFF .NE. 0)
          VIPTR = LOC(TEMP(0))
          VJPTR = LOC(TEMP(0))
          VKPTR = LOC(TEMP(0))
          VHPTR = LOC(TEMP(0))
          VDPTR = LOC(TEMP(0))
          VSPTR = LOC(TEMP(0))
          VQPTR = LOC(TEMP(0))
          VCPTR = LOC(TEMP(0))
          VFPTR = LOC(TEMP(0))
          VEPTR = LOC(TEMP(0))
          VLPTR = LOC(TEMP(0))
          VMPTR = LOC(TEMP(0))
          VNPTR = LOC(TEMP(0))
          VOPTR = LOC(TEMP(0))
          CHNK  = ISHFT(ISHFT(TEMPSZ, WORDSZB), -VLENB)
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

        SELECT CASE (VLEN)

          CASE (32)                 !  VLEN .EQ. 32

          SELECT CASE (MLEN)

            CASE (32)               !  VLEN .EQ. 32 .AND. MLEN .EQ. 32
              DO I = 0, NMBR - 1
                RVAXF(VI) = IS2VF$(MIPSS(MI))
                VI        = VI + VINC
                MI        = MI + MINC
              ENDDO

            CASE (64)               !  VLEN .EQ. 32 .AND. MLEN .EQ. 64
              DO I = 0, NMBR - 1
                RVAXF(VI) = ID2VF$(MIPSD(MI))
                VI        = VI + VINC
                MI        = MI + MINC
              ENDDO

            CASE (128)              !  VLEN .EQ. 32 .AND. MLEN .EQ. 128
              DO I = 0, NMBR - 1
                RVAXF(VI) = IQ2VF$(IEM128O$(MIPSQ(MI)))
                VI        = VI + VINC
                MI        = MI + MINC
              ENDDO

          END SELECT

          CASE (64)                 !  VLEN .EQ. 64

          SELECT CASE (MLEN)

            CASE (32)               !  VLEN .EQ. 64 .AND. MLEN .EQ. 32
              DO I = 0, NMBR - 1
                RVAXD(VI) = IS2VD$(MIPSS(MI))
                VI        = VI + VINC
                MI        = MI + MINC
              ENDDO

            CASE (64)               !  VLEN .EQ. 64 .AND. MLEN .EQ. 64
              DO I = 0, NMBR - 1
                RVAXD(VI) = ID2VD$(MIPSD(MI))
                VI        = VI + VINC
                MI        = MI + MINC
              ENDDO

            CASE (128)              !  VLEN .EQ. 64 .AND. MLEN .EQ. 128
              DO I = 0, NMBR - 1
                RVAXD(VI) = IQ2VD$(IEM128O$(MIPSQ(MI)))
                VI        = VI + VINC
                MI        = MI + MINC
              ENDDO

          END SELECT

          CASE (128)                !  VLEN .EQ. 128

          SELECT CASE (MLEN)

            CASE (32)               !  VLEN .EQ. 128 .AND. MLEN .EQ. 32
              DO I = 0, NMBR - 1
                RVAXG(VI) = IS2VG$(MIPSS(MI))
                VI        = VI + VINC
                MI        = MI + MINC
              ENDDO

            CASE (64)               !  VLEN .EQ. 128 .AND. MLEN .EQ. 64
              DO I = 0, NMBR - 1
                RVAXG(VI) = ID2VG$(MIPSD(MI))
                VI        = VI + VINC
                MI        = MI + MINC
              ENDDO

            CASE (128)              !  VLEN .EQ. 128 .AND. MLEN .EQ. 128
              DO I = 0, NMBR - 1
                RVAXG(VI) = IQ2VG$(IEM128O$(MIPSQ(MI)))
                VI        = VI + VINC
                MI        = MI + MINC
              ENDDO

          END SELECT

        END SELECT

        LEFT  = LEFT - NMBR

        IF (BITOFF .NE. 0) THEN       !  Complete strip mine

          IF (VOFF .NE. 0) THEN       !  Complete transfer
            CALL MOVBITZ(TEMP(0), ZERO, NMBR * VLEN, NNVAX(INDX), VOFF)
            VI    = 0
          ENDIF

          INDX  = INDX + NMBR
          NMBR  = CHNK
        ENDIF

      ENDDO

      GOTO 9000

C     TYPE = 4  (Complex)                                   ************
C
C     Convert MIPS IEEE complex floating-point numbers to VAX complex
C     floating-point numbers
C
C     MIPS IEEE         |                 VAX
C                       |  2x32-bit     2x64-bit     2x128-bit
C                       |  (single)     (double)      (quad)
C                       |   KIND=4       KIND=8       KIND=16
C     ------------------+--------------------------------------
C     2x32-bit (single) |  2xIS2VF$  |  2xIS2VD$  |  2xIS2VG$
C                       +------------+------------+------------
C     2x64-bit (double) |  2xID2VF$  |  2xID2VD$  |  2xID2VG$
C                       +------------+------------+------------
C     2x128-bit (quad)  |  2xIQ2VF$  |  2xIQ2VD$  |  2xIQ2VG$
C     ------------------+--------------------------------------

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
        ELSE  !  (VOFF .NE. 0)
          VIPTR = LOC(TEMP(0))
          VJPTR = LOC(TEMP(0))
          VKPTR = LOC(TEMP(0))
          VHPTR = LOC(TEMP(0))
          VDPTR = LOC(TEMP(0))
          VSPTR = LOC(TEMP(0))
          VQPTR = LOC(TEMP(0))
          VCPTR = LOC(TEMP(0))
          VFPTR = LOC(TEMP(0))
          VEPTR = LOC(TEMP(0))
          VLPTR = LOC(TEMP(0))
          VMPTR = LOC(TEMP(0))
          VNPTR = LOC(TEMP(0))
          VOPTR = LOC(TEMP(0))
          CHNK  = ISHFT(ISHFT(TEMPSZ, WORDSZB), -VLENB)
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

        SELECT CASE (VLEN)

        CASE ( 64)                !  VLEN .EQ.  64

          SELECT CASE (MLEN)

          CASE (64)               !  VLEN .EQ.  64 .AND. MLEN .EQ.  64
            DO I = 0, NMBR - 1
              CVAXF(VI) = CMPLX(IS2VF$(REAL (MIPSF(MI))),
     1                          IS2VF$(AIMAG(MIPSF(MI))),
     2                          KIND=4)
              VI        = VI + VINC
              MI        = MI + MINC
            ENDDO

          CASE (128)              !  VLEN .EQ.  64 .AND. MLEN .EQ. 128
            DO I = 0, NMBR - 1
              CVAXF(VI) = CMPLX(ID2VF$(REAL (MIPSC(MI))),
     1                          ID2VF$(AIMAG(MIPSC(MI))),
     2                          KIND=4)
              VI        = VI + VINC
              MI        = MI + MINC
            ENDDO

          CASE (256)              !  VLEN .EQ.  64 .AND. MLEN .EQ. 256
            DO I = 0, NMBR - 1
              CVAXF(VI) = CMPLX(IQ2VF$(IEM128O$(REAL (MIPSE(MI)))),
     1                          IQ2VF$(IEM128O$(AIMAG(MIPSE(MI)))),
     2                          KIND=4)
              VI        = VI + VINC
              MI        = MI + MINC
            ENDDO

          END SELECT

        CASE (128)                !  VLEN .EQ. 128

          SELECT CASE (MLEN)

          CASE ( 64)              !  VLEN .EQ. 128 .AND. MLEN .EQ.  64
            DO I = 0, NMBR - 1
              CVAXD(VI) = CMPLX(IS2VD$(REAL (MIPSF(MI))),
     1                          IS2VD$(AIMAG(MIPSF(MI))),
     2                          KIND=8)
              VI        = VI + VINC
              MI        = MI + MINC
            ENDDO

          CASE (128)              !  VLEN .EQ. 128 .AND. MLEN .EQ. 128
            DO I = 0, NMBR - 1
              CVAXD(VI) = CMPLX(ID2VD$(REAL (MIPSC(MI))),
     1                          ID2VD$(AIMAG(MIPSC(MI))),
     2                          KIND=8)
              VI        = VI + VINC
              MI        = MI + MINC
            ENDDO

          CASE (256)              !  VLEN .EQ. 128 .AND. MLEN .EQ. 256
            DO I = 0, NMBR - 1
              CVAXD(VI) = CMPLX(IQ2VD$(IEM128O$(REAL (MIPSE(MI)))),
     1                          IQ2VD$(IEM128O$(AIMAG(MIPSE(MI)))),
     2                          KIND=8)
              VI        = VI + VINC
              MI        = MI + MINC
            ENDDO

          END SELECT

        CASE (256)                !  VLEN .EQ. 256

          SELECT CASE (MLEN)

          CASE ( 64)              !  VLEN .EQ. 256 .AND. MLEN .EQ.  64
            DO I = 0, NMBR - 1
              CVAXG(VI) = CMPLX(IS2VG$(REAL (MIPSF(MI))),
     1                          IS2VG$(AIMAG(MIPSF(MI))),
     2                          KIND=8)
              VI        = VI + VINC
              MI        = MI + MINC
            ENDDO

          CASE (128)              !  VLEN .EQ. 256 .AND. MLEN .EQ. 128
            DO I = 0, NMBR - 1
              CVAXG(VI) = CMPLX(ID2VG$(REAL (MIPSC(MI))),
     1                          ID2VG$(AIMAG(MIPSC(MI))),
     2                          KIND=8)
              VI        = VI + VINC
              MI        = MI + MINC
            ENDDO

          CASE (256)              !  VLEN .EQ. 256 .AND. MLEN .EQ. 256
            DO I = 0, NMBR - 1
              CVAXG(VI) = CMPLX(IQ2VG$(IEM128O$(REAL (MIPSE(MI)))),
     1                          IQ2VG$(IEM128O$(AIMAG(MIPSE(MI)))),
     2                          KIND=8)
              VI        = VI + VINC
              MI        = MI + MINC
            ENDDO

          END SELECT

        END SELECT

        LEFT  = LEFT - NMBR

        IF (BITOFF .NE. 0) THEN       !  Complete strip mine

          IF (VOFF .NE. 0) THEN       !  Complete transfer
            CALL MOVBITZ(TEMP(0), ZERO, NMBR * VLEN, NNVAX(INDX), VOFF)
            VI    = 0
          ENDIF

          INDX  = INDX + NMBR
          NMBR  = CHNK
        ENDIF

      ENDDO

      GOTO 9000

C     TYPE = 5  (Logical)                                   ************
C
C     Convert MIPS logicals to VAX logicals
C
C     MIPS              |                    VAX
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
C
C     Note: We don't worry about endian swaps with logicals since the
C     definition of LOGICAL (zero => .TRUE. and nonzero => .FALSE.) is
C     otherwise unaffected by endian swapping.

  500 CONTINUE

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
        ELSE  !  (VOFF .NE. 0)
          VIPTR = LOC(TEMP(0))
          VJPTR = LOC(TEMP(0))
          VKPTR = LOC(TEMP(0))
          VHPTR = LOC(TEMP(0))
          VDPTR = LOC(TEMP(0))
          VSPTR = LOC(TEMP(0))
          VQPTR = LOC(TEMP(0))
          VCPTR = LOC(TEMP(0))
          VFPTR = LOC(TEMP(0))
          VEPTR = LOC(TEMP(0))
          VLPTR = LOC(TEMP(0))
          VMPTR = LOC(TEMP(0))
          VNPTR = LOC(TEMP(0))
          VOPTR = LOC(TEMP(0))
          CHNK  = ISHFT(ISHFT(TEMPSZ, WORDSZB), -VLENB)
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

        SELECT CASE (VLEN)

          CASE ( 8)                 !  VLEN .EQ.  8

          SELECT CASE (MLEN)

            CASE ( 8)               !  VLEN .EQ.  8 .AND. MLEN .EQ.  8
              DO I = 0, NMBR - 1
                LVAXB(VI) = LOGICAL(MIPSO(MI),KIND=1)
                VI        = VI + VINC
                MI        = MI + MINC
              ENDDO

            CASE (16)               !  VLEN .EQ.  8 .AND. MLEN .EQ. 16
              DO I = 0, NMBR - 1
                LVAXB(VI) = LOGICAL(MIPSN(MI),KIND=1)
                VI        = VI + VINC
                MI        = MI + MINC
              ENDDO

            CASE (32)               !  VLEN .EQ.  8 .AND. MLEN .EQ. 32
              DO I = 0, NMBR - 1
                LVAXB(VI) = LOGICAL(MIPSM(MI),KIND=1)
                VI        = VI + VINC
                MI        = MI + MINC
              ENDDO

            CASE (64)               !  VLEN .EQ.  8 .AND. MLEN .EQ. 64
              DO I = 0, NMBR - 1
                LVAXB(VI) = LOGICAL(MIPSL(MI),KIND=1)
                VI        = VI + VINC
                MI        = MI + MINC
              ENDDO

          END SELECT

          CASE (16)                 !  VLEN .EQ. 16

          SELECT CASE (MLEN)

            CASE ( 8)               !  VLEN .EQ. 16 .AND. MLEN .EQ.  8
              DO I = 0, NMBR - 1
                LVAXW(VI) = LOGICAL(MIPSO(MI),KIND=2)
                VI        = VI + VINC
                MI        = MI + MINC
              ENDDO

            CASE (16)               !  VLEN .EQ. 16 .AND. MLEN .EQ. 16
              DO I = 0, NMBR - 1
                LVAXW(VI) = LOGICAL(MIPSN(MI),KIND=2)
                VI        = VI + VINC
                MI        = MI + MINC
              ENDDO

            CASE (32)               !  VLEN .EQ. 16 .AND. MLEN .EQ. 32
              DO I = 0, NMBR - 1
                LVAXW(VI) = LOGICAL(MIPSM(MI),KIND=2)
                VI        = VI + VINC
                MI        = MI + MINC
              ENDDO

            CASE (64)               !  VLEN .EQ. 16 .AND. MLEN .EQ. 64
              DO I = 0, NMBR - 1
                LVAXW(VI) = LOGICAL(MIPSL(MI),KIND=2)
                VI        = VI + VINC
                MI        = MI + MINC
              ENDDO

          END SELECT

          CASE (32)                 !  VLEN .EQ. 32

          SELECT CASE (MLEN)

            CASE ( 8)               !  VLEN .EQ. 32 .AND. MLEN .EQ.  8
              DO I = 0, NMBR - 1
                LVAXL(VI) = LOGICAL(MIPSO(MI),KIND=4)
                VI        = VI + VINC
                MI        = MI + MINC
              ENDDO

            CASE (16)               !  VLEN .EQ. 32 .AND. MLEN .EQ. 16
              DO I = 0, NMBR - 1
                LVAXL(VI) = LOGICAL(MIPSN(MI),KIND=4)
                VI        = VI + VINC
                MI        = MI + MINC
              ENDDO

            CASE (32)               !  VLEN .EQ. 32 .AND. MLEN .EQ. 32
              DO I = 0, NMBR - 1
                LVAXL(VI) = LOGICAL(MIPSM(MI),KIND=4)
                VI        = VI + VINC
                MI        = MI + MINC
              ENDDO

            CASE (64)               !  VLEN .EQ. 32 .AND. MLEN .EQ. 64
              DO I = 0, NMBR - 1
                LVAXL(VI) = LOGICAL(MIPSL(MI),KIND=4)
                VI        = VI + VINC
                MI        = MI + MINC
              ENDDO

          END SELECT

          CASE (64)                 !  VLEN .EQ. 64

          SELECT CASE (MLEN)

            CASE ( 8)               !  VLEN .EQ. 64 .AND. MLEN .EQ.  8
              DO I = 0, NMBR - 1
                LVAXQ(VI) = LOGICAL(MIPSO(MI),KIND=8)
                VI        = VI + VINC
                MI        = MI + MINC
              ENDDO

            CASE (16)               !  VLEN .EQ. 64 .AND. MLEN .EQ. 16
              DO I = 0, NMBR - 1
                LVAXQ(VI) = LOGICAL(MIPSN(MI),KIND=8)
                VI        = VI + VINC
                MI        = MI + MINC
              ENDDO

            CASE (32)               !  VLEN .EQ. 64 .AND. MLEN .EQ. 32
              DO I = 0, NMBR - 1
                LVAXQ(VI) = LOGICAL(MIPSM(MI),KIND=8)
                VI        = VI + VINC
                MI        = MI + MINC
              ENDDO

            CASE (64)               !  VLEN .EQ. 64 .AND. MLEN .EQ. 64
              DO I = 0, NMBR - 1
                LVAXQ(VI) = LOGICAL(MIPSL(MI),KIND=8)
                VI        = VI + VINC
                MI        = MI + MINC
              ENDDO

          END SELECT

        END SELECT

        LEFT  = LEFT - NMBR

        IF (BITOFF .NE. 0) THEN       !  Complete strip mine

          IF (VOFF .NE. 0) THEN       !  Complete transfer
            CALL MOVBITZ(TEMP(0), ZERO, NMBR * VLEN, NNVAX(INDX), VOFF)
            VI    = 0
          ENDIF

          INDX  = INDX + NMBR
          NMBR  = CHNK
        ENDIF

      ENDDO

      GOTO 9000

C     TYPE = 6  (Character)                                 ************

  600 CONTINUE

      IF (VLEN .NE. MLEN) GOTO 9008

      CALL G$CHRPCK(NATIVECH, MWAD, MBTL, MCOFF)  ! Get character offset
      VCOFF = VOFF
      MIPTR = MWAD

      CHLEN = MLEN * LEN(NATIVECH(0))             ! Get character length

      IF (STRIDE .EQ. 1) THEN

        CALL MOVBITZ(MIPSI(0), MCOFF, NMBR * CHLEN, IVAXQ(0), VCOFF)

      ELSE

        DO I = 0, NMBR - 1

          CALL MOVBITZ(MIPSI(MI), MCOFF, CHLEN, IVAXQ(VI), VCOFF)

          VCOFF = VCOFF + (CHLEN * VINC)
          VI    = VI + ISHFT(VCOFF, -WORDSZB)
          VCOFF = MOD(VCOFF, INT(WORDSZ, KIND=8))
          MCOFF = MCOFF + (CHLEN * MINC)
          MI    = MI + ISHFT(MCOFF, -WORDSZB)
          MCOFF = MOD(MCOFF, INT(WORDSZ, KIND=8))

        ENDDO

      ENDIF
      GOTO 9000

 9010 CONTINUE
      MIPS2VAX = -10              !  FORN must be 64-bit word-aligned
      GOTO 10000

 9009 CONTINUE
      MIPS2VAX = -9               !  NATIVE must be 64-bit word-aligned
      GOTO 10000

 9008 CONTINUE
      MIPS2VAX = -8               !  Parameter error, NATLEN/FORLEN invalid
      GOTO 10000

C9007 CONTINUE
C     MIPS2VAX = -7               !  Unable to allocate memory (unused in
C     GOTO 10000                  !  this version of the routine)

 9006 CONTINUE
      MIPS2VAX = -6               !  Parameter error, invalid FORLEN
      GOTO 10000

 9005 CONTINUE
      MIPS2VAX = -5               !  Parameter error, invalid NATLEN
      GOTO 10000

 9004 CONTINUE
      MIPS2VAX = -4               !  Parameter error, invalid BITOFF
      GOTO 10000

 9003 CONTINUE
      MIPS2VAX = -3               !  Parameter error, invalid NUM
      GOTO 10000

 9002 CONTINUE
      MIPS2VAX = -2               !  Parameter error, invalid TYPE
      GOTO 10000

 9001 CONTINUE
      MIPS2VAX = -1               !  Parameter error
      GOTO 10000

 9000 CONTINUE
      MIPS2VAX = GN32OF + VAXOF   !  Return error count
C     GOTO 10000

10000 CONTINUE
      RETURN

CDIR$ ID "@(#) libu/vms/mips2vax.F	92.2	01/14/99 16:12:10"
      END
C
C     Auxiliary functions
C
      LOGICAL FUNCTION MIPS2VAX_AUXILIARY_FUNCTION$()
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
      EXTERNAL MIPS2VAX_INIT
C
      COMMON /G$MIPS2VAX/ NSIZE, FSIZE
      SAVE   /G$MIPS2VAX/
C
      MIPS2VAX_AUXILIARY_FUNCTION$ = .TRUE.
      RETURN
C
      ENTRY MIPS2VAX_NATIVE_TYPE_OK$(NTYPE, NLEN) RESULT (NOK)
      IF (IAND(ISHFT(ONE,((NLEN/8)-1)), NSIZE(NTYPE)) .NE. 0) THEN
        NOK = .TRUE.
      ELSE
        NOK = .FALSE.
      ENDIF
      RETURN
C
      ENTRY MIPS2VAX_FOREIGN_TYPE_OK$(FTYPE, FLEN) RESULT (FOK)
      IF (IAND(ISHFT(ONE,((FLEN/8)-1)), FSIZE(FTYPE)) .NE. 0) THEN
        FOK = .TRUE.
      ELSE
        FOK = .FALSE.
      ENDIF
      RETURN
C
      END
C
      BLOCK DATA MIPS2VAX_INIT
C
      INTEGER (KIND=8) NSIZE(6)
      INTEGER (KIND=8) FSIZE(6)
      COMMON /G$MIPS2VAX/ NSIZE, FSIZE
      SAVE   /G$MIPS2VAX/
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
C                                   16- and 8-bit
     2          O'        213',   ! Integer:  64-, 32-, 16- and 8-bit
     3          O'     100210',   ! Real:  128-, 64- and 32-bit
     4          O'20000100200',   ! Complex:  256-, 128- and 64-bit
     5          O'        213',   ! Logical:  64-, 32-, 16- and 8-bit
     6          O'        001'/   ! Character:  8-bit
C
      DATA FSIZE /
     1          O'20000100213',   ! Typeless:  256-, 128-, 64-, 32-,
C                                   16- and 8-bit
     2          O'        213',   ! Integer:  64-, 32-, 16- and 8-bit
     3          O'     100210',   ! Real:  128-, 64- and 32-bit
     4          O'20000100200',   ! Complex:  256-, 128- and 64-bit
     5          O'        213',   ! Logical:  64-, 32-, 16- and 8-bit
     6          O'        001'/   ! Character:  8-bit
      END BLOCK DATA MIPS2VAX_INIT
