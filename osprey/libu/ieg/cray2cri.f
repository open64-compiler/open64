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
     +CRAY2CRI(TYPE, NUM, FORN, BITOFF, CRAY, STRIDE, CRAYCH)
      IMPLICIT NONE
C     The CRAY2CRI function converts CRAY Fortran data types to a
C     generic 64-bit platform with IEEE floating-point representation
C     data types by using the CRAY2IEG routine, and suppressing
C     errors.
C
C     The returned function value is as follows:
C
C         <0  Parameter error; no translation performed
C          0  Translation complete; no errors
C         >0  Translation complete; return value once was the number of
C             values that overflowed or completely underflowed.
C             The 7.0 IEG2CRAY/CRAY2IEG routines return zero.
C             NOTE: Unicos I/O libraries abort when NONZERO returned.
      INTEGER TYPE
C         Type code:
C           0 = typeless (no translation)
C           1 = integer (64-bit unchanged)
C           2 = real (64-bit single-precision CRAY floating-point
C               to 64-bit double-precision IEEE floating-point)
C           3 = double (128-bit double-precision CRAY floating-point
C               to 64-bit double-precision IEEE floating-point)
C           4 = complex (2 x 64-bit single-precision CRAY 
C               floating-point to 2x64-bit double-precision IEEE 
C               floating-point)
C           5 = logical (64-bit CRAY logical to 64-bit 1/0 logic)
C               Converts negative Cray values to 1 (TRUE).
C           6 = character (ASCII to ASCII; no translation)
C           7 = short integer (32-bit two's-complement to 16-bit
C               two's-complement)
C           8 = special real*8 (64-bit single-precision CRAY floating-point
C               to 64-bit double-precision IEEE floating-point)
C           9 = complex (2 x 64-bit single-precision CRAY 
C               floating-point to 2x32-bit single-precision IEEE 
C               floating-point)
C          10 = logical (64-bit CRAY logical to 32-bit 1/0 logic)
C               Converts nonzero Cray values to 1 (TRUE).
C          11 = integer (64-bit CRAY integer to 32-bit integer)
C          12 = real (64-bit single-precision CRAY floating-point
C               to 32-bit single-precision IEEE floating-point)
      INTEGER NUM
C         Number of data items to convert.  Type integer variable,
C         expression, or constant.
      INTEGER (KIND=8) FORN(0:1)
C         Variable or array of any type (except CHARACTER) or length
C         to receive the converted data.
      INTEGER BITOFF
C         Bit offset within FORN to begin placing the converted
C         data.  Type integer variable, expression, or constant.
C         Bits are numbered from 0 to 63, beginning at the leftmost
C         bit of FORN.
      INTEGER (KIND=8) CRAY(0:1)
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

      INTEGER           INC   ! STRIDE to use

      INTEGER           CRAY2IEG

      INTEGER           NSIZ
      INTEGER           ISIZE(0:8)
      INTEGER           ALSTUF
      INTEGER           PADVAL

      COMMON  /G@CRI@SZ/ NSIZ,ISIZE
C     ISIZE and NSIZ are used by the I/O library to size the foreign data
C     ISIZE is the number of user bits per item
C     NSIZ is the number of Cray types the libraries use
C         (Users may call with TYPE= 0 through MAXTYPE)

      REAL DNRML32,DNRML64       ! x denormal if 0<ABS(x))<DNRMLii
      INTEGER IDNRM32,IDNRM64    ! x denormal if 0<ABS(x))<DNRMLii
      PARAMETER ( DNRML32=0.5**126, DNRML64=0.5**1022)

      INTEGER (KIND=8) ITEMP, ITMP2, ITMP5    ! temporaries
      INTEGER          I, J, IT, JJ, LNCHNK   ! temporaries
      PARAMETER( LNCHNK=64)
      INTEGER (KIND=8) ITMP4(0:1,0:LNCHNK-1)  ! Temporary storage
      REAL    TEMP3, TEMP4                    ! temporaries
      INTEGER MAXTYPE
      PARAMETER( MAXTYPE=13 )

      INTEGER MAP(0:MAXTYPE)  ! TYPE for CRAY2IEG/IEG2CRAY call
ccccc MAP():0-8: TYPE code for CRAY2IEG/IEG2CRAY call
c             9: indicates complex*16 (2@real*8 per item)
c            10: indicates logical*8 (requires translation)

c     If no denormal testing is desired, zero DENORM
      INTEGER DENORM(0:MAXTYPE)  ! # IEEE reals/item
ccccc DENORM: 1: one IEEE-64 real  per item
c             2: two IEEE-64 reals per item
c            -1: one IEEE-32 real  per item
c            -2: two IEEE-32 reals per item
c             0: not IEEE-32/64 real

ccccc type:        0,  1   2    3    4   5   6   7   8   9   10  11  12 13
ccccc cray:      b*8 i*8 r*8 d*16 c*16 l*8 a*8 i*2 r*8 c*16 l*8 i*8 r*8 i*2

ccccc forn:      b*8 i*8 r*8  r*8 c*16 l*8 a*8 i*4 r*8  c*8 l*4 i*4 r*4 i*2
      DATA MAP/    0,  0,  8,   3,   9, 10,  6,  1,  8,  4,   5, 1, 2,7/

      DATA DENORM/ 0,  0,  1,   1,   2,  0,  0,  0,  1, -2,   0, 0,-1,0/

      DATA ISIZE /64, 64, 64,  64, 128, 64,  8, 32, 64/ !forn bits/item
C*generic IEEE32: 64, 32, 32,  64,  64, 32,  8, 16, 64  !forn bits/item

      DATA NSIZ /9/        ! number of type codes libraries use: 0...8

      EXTERNAL          CRAY2IEG

      INTEGER IEXP64, IEXPU32, IEPXL32, MEXPU32,MEXPL32,MEXP64
      INTEGER MDENU32,MDENL32,MDEN64
      PARAMETER( mexp64=  z'7ff0000000000000')  ! mask for ieee-64 exponent
      PARAMETER( mexpu32= z'7f80000000000000')  ! mask for ieee-32 exponent
      PARAMETER( mexpl32= z'000000007f800000')  ! mask for ieee-32 exponent
      PARAMETER( mden64=  z'8000000000000000')  ! mask for ieee-64 denormal
      PARAMETER( mdenu32= z'80000000ffffffff')  ! mask for ieee-32 denormal
      PARAMETER( mdenl32= z'ffffffff80000000')  ! mask for ieee-32 denormal

      LOGICAL ZTRUE,ZFALSE
      INTEGER ITRUE,IFALSE
      INTEGER NUM32, NUMHI, NUMLO, JJJ
      EQUIVALENCE( ITRUE, ZTRUE ), (IFALSE, ZFALSE)
      DATA ZTRUE,ZFALSE/ .true. , .false./ ! for testing assumptions

C     The following data is used by the I/O library to determine padding
C     requirements between character and non-character items in the
C     foreign fie.

      COMMON  /G@CRI@AL/ ALSTUF,PADVAL
      DATA ALSTUF,PADVAL /0,0/           ! IEEE/generic does no alignment

      CRAY2CRI = -1           !  Some parameter error, assume failure
      IF (NUMARG() .LT. 5 .or. NUMARG() .GT. 7) GOTO 9000  !UTERP(034)
      INC = 1
      IF (NUMARG() .GE. 6) INC = STRIDE

      IF (TYPE .LT. 0 .OR. TYPE .GT. MAXTYPE) GOTO 9000  !UTERP(034)
c     need CRAYCH to do characters
      IF (TYPE .EQ. 6 .AND. NUMARG() .LT. 7) GOTO 9000  !UTERP(034)
      IF (NUM .LE. 0) RETURN

      CRAY2CRI = 0
      IF (NUM .EQ. 0) RETURN

      I = MAP(TYPE)
      IF( I .EQ. 9 ) GOTO 9004  ! complex(4) handler
      IF( I .GT. 9 ) GOTO 9005  ! logical(5) handler

c     All other types map to an CRAY2IEG/IEG2CRAY call:
  100 CRAY2CRI = CRAY2IEG( I, NUM, FORN, BITOFF, CRAY, INC, CRAYCH)

c     Test DENORM(TYPE)*NUM results and zero "denormal" values
c       ieee-64 denormal: 11-bit exponent zero
c       ieee-32 denormal:  8-bit exponent zero
c     Underflow negatives to 0.0, as does CRAY2IEG
c     Underflow does not change CRAY2CRI or CRAY2IEG

  200 CONTINUE 
      IT=iabs(DENORM(TYPE))               !number of floats per item
      IF( DENORM(TYPE) .EQ. 0 ) GOTO 300   ! not floating point

      IT=DENORM(TYPE)*NUM
      IF( DENORM(TYPE) .GT. 0 ) THEN   
        IF( BITOFF .LE. 0 ) THEN

c       FORN is IEEE-64 float with zero BIT offset
          DO J=1,IT                     ! IEEE-64: 11 bit exponent
          IF(         IAND(FORN(J-1), z'7ff0000000000000') .EQ. 0)
     +      FORN(J-1)=IAND(FORN(J-1), z'8000000000000000')
          ENDDO

        ELSE

c       FORN is IEEE-64 float with positive BIT offset. zero if < 2^-1022 .
c       dshiftl(i,j,n): shift logical doubleword (i,j) left n bits
c            and return the upper word.  Retains type of argument.

           JJJ = dshiftl(0,forn(0),bitoff)   !save leading bits on right
           DO J = 0,IT-1
             ITMP2 = dshiftl( FORN(J),FORN(J+1),BITOFF)
             IF( and( ITMP2,mexp64 ) .EQ. 0) THEN      ! check exponent
                ITMP2  = and(mden64,ITMP2)             ! zero bits
             ENDIF
             forn(J)=ITMP2                   ! store as one shifted word
           ENDDO

           forn(IT) = shiftl( forn(IT), bitoff )   ! eliminate leading bits
           DO J = IT,1,-1                    ! shift result back
             FORN(J) = dshiftr(FORN(J-1),FORN(J),BITOFF)
           ENDDO
           FORN(0) = dshiftr(JJJ,FORN(0),BITOFF) ! restore leading bits

        ENDIF

      ELSE
         IT= -IT                           ! number of IEEE-32 values  
         IF( BITOFF .NE. 0 ) GOTO 211   ! go handle bit offset

         DO J=1,IT/2+IAND(IT,1)            ! test upper 32-bit values
          IF(         IAND(FORN(J-1), z'7f80000000000000') .EQ. 0)
     +      FORN(J-1)= IAND(FORN(J-1), z'80000000ffffffff')
         ENDDO

         DO J=1,IT/2                        ! test lower 32-bit values
          IF(         IAND(FORN(J-1), z'000000007f800000') .EQ. 0)
     +      FORN(J-1)= IAND(FORN(J-1), z'ffffffff80000000')
         ENDDO

      ENDIF

  300 CONTINUE 
 9000 CONTINUE
      RETURN

  800 CRAY2CRI = I   ! return with error code in I
      GOTO 9000


  211 CONTINUE           ! IEEE-32 with nonzero BIT offset into FORN
c     Test for IEEE-32 denormal: <2.0**-126
c     Zero 31 bits after sign, if denormal, producing +/- 0.0
c     Store the unshifted result in forn, then shift forn right.

      jjj   = dshiftl( 0,forn(0), bitoff ) ! save leading bitoff bits
      num32 = -num * denorm(type)
      numlo = shiftr(num32,1)           ! number in lower halfwords
      numhi = num32 - numlo             ! number in upper halfwords

      DO j=0, numlo-1                    ! process in pairs
        jj = dshiftl( forn(j),forn(j+1), bitoff )
        IF(    and(z'7f80000000000000',jj) .EQ. 0 )
     +    jj = and(z'80000000ffffffff',jj)
        IF(    and(z'000000007f800000',jj) .EQ. 0 )
     +    jj = and(z'ffffffff80000000',jj)
        forn(j)= jj
      ENDDO
      IF( numlo .NE. numhi ) THEN       ! one final word to shift
        jj = dshiftl( forn(numhi-1),forn(numhi), bitoff )
        IF(               and(z'7f80000000000000',jj) .EQ. 0 )
     +    jj            = and(z'80000000ffffffff',jj)
        forn(numhi-1)= jj
      ENDIF

c     Shift the bit array forn(1:num) right. Fill with bits from forn(j-1)
      IF( bitoff .GT. 0 ) THEN
         forn(numhi) = shiftl( forn(numhi), bitoff )   ! eliminate leading bits
         DO j=numhi,1,-1                       !  Reverse order vectorizes
           forn(j) = dshiftr(forn(j-1),forn(j), bitoff)
         ENDDO
         forn(0) =  dshiftr( jjj,forn(0), bitoff) ! Restore leading bits
      ENDIF

      GOTO 300


c     The following cases have no CRAY2IEG equivalent,
c     so special handling is required.
      
 9004 CONTINUE ! cray complex*16 (2 real*8): --> 2 IEEE-64 real
      IF( INC .EQ. 1) THEN
        CRAY2CRI = CRAY2IEG(8, NUM*2, FORN, BITOFF, CRAY, 1, CRAYCH)
        GOTO 200                    !  check denormals
      ELSE

c     complex*16 STRIDE>1: LNCHNK-item contiguous batches:
c                     (good performance: NUM/64 calls to conversion)

          IF( LNCHNK .GT. 64 ) I=-99999  !internal error: remove shortloop
          IF( LNCHNK .GT. 64 ) GOTO 800  ! needed due to shortloop below
          DO J = 0, NUM-1, LNCHNK
            ITMP5 = MIN(LNCHNK, NUM-J)   ! number of complex in this batch
CDIR$       SHORTLOOP
            DO JJ = J, ITMP5-1+J
                ITMP4(0,JJ-J) = CRAY(2*INC*JJ)
                ITMP4(1,JJ-J) = CRAY(2*INC*JJ+1)
            ENDDO
            I = CRAY2IEG(8, ITMP5*2, FORN(2*J),BITOFF,ITMP4,1,CRAYCH)
            IF( I .LT. 0 ) GOTO 800   ! I<0:  terminate with error code -I
            CRAY2CRI = I + CRAY2CRI   ! I>=0: may be underflow/overflow count
          ENDDO
        ENDIF

      GOTO 200                      !  check for denormals

 9005 CONTINUE ! cray logical*8: --> t3d logical*8

c     convert nonzero cray logicals to 1 (.true.) 
c     cray-t3d /CRAY-2:   cft77 .true. is 1
c     cray-y/x/1: cft77 .true. is -1, but z'8000000000000000' occurs
c     from internal computation.  I don't know about the others.

c     Test basic assumptions.
c     compiler should not generate code when it is false
      IF( 0 .NE. ifalse ) THEN
              I = -99997
              GOTO 800
      ENDIF

c     If BITOFF = 0: logical(j) is in forn(j), j=0,1,...,num-1.

c     If BITOFF > 0: logical(j) is spread over forn(j) and forn(j+1).
c     FORN(j) has 64-bitoff zero bits on the right,j=0,...,num-1.
c     FORN(j+1) has the t/f bit, with bitoff-1 zero bits on left.
c     forn(      0) = < (bitoff)*x,        (64-bitoff)*0 >
c     forn(1:num-1) = < (bitoff-1)*0, 1|0, (64-bitoff)*0 >
c     forn(    num) = < (bitoff-1)*0, 1|0, (64-bitoff)*x >

c     Insert 1 or 0 in first BITOFF bits of the second FORN word.
c     DSHIFTR shifts doubleword(i,j) right and takes the right part.

      jj = 0                   ! aligned: put bits in forn(0:num-1)
      IF( bitoff .GT. 0 ) THEN
        jj = 1                 ! unaligned: put bits in forn(1:num)
        FORN(0)=AND(FORN(0),MASK(BITOFF))    !Zero right 64-BITOFF bits

        i = 0
        IF( cray((num-1)*inc) .NE. 0 ) i=1
        j = shiftl( forn(num), bitoff )     !eliminate old leading bits
        forn(num) = dshiftr( i, j, bitoff)  !insert leading bits from i
      ENDIF

c     bitoff>0: set forn(1:num-1) to (bitoff-1)*0,1/0,(64-bitoff)*0
c     bitoff=0: set forn(0:num-1) with one bit each
      DO j = jj, num-1
          IF( cray( (j-jj)*inc ) .NE. 0 ) THEN
             forn(j) = dshiftr(1,1,bitoff)  ! set bitoff-th bit on left
          ELSE
             forn(j) = 0
          ENDIF
      ENDDO
      GOTO 9000

CDIR$ ID "@(#) libu/ieg/cray2cri.f	92.1	10/19/98 16:39:32"
      END
