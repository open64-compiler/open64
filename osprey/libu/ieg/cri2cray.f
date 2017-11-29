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


C     Modified User Layer for T3D numerical conversion.

      INTEGER FUNCTION
     +CRI2CRAY(TYPE, NUM, FORN, BITOFF, CRAY, STRIDE, CRAYCH)
      IMPLICIT NONE
      INTEGER TYPE
      INTEGER NUM
      INTEGER FORN(0:0)
      INTEGER BITOFF
      INTEGER CRAY(0:0)
      INTEGER STRIDE
      INTEGER IEG2CRAY

      INTEGER INC          ! Holds STRIDE of items
      INTEGER I,J,JJ,JJJ,JJJJ,IT,ITMP5,ITEMP       ! Temporary

      INTEGER LNCHNK
      PARAMETER( LNCHNK=64 ) 
      INTEGER ITMP4(0:1,0:LNCHNK-1)  !Temporary storage

      INTEGER MAXTYPE
      PARAMETER( MAXTYPE=13 )

      INTEGER MAP(0:MAXTYPE)  ! Maps TYPE to IEG2CRAY type code
ccccc     type: 0,1 2 3  4  5 6 7 8 9 10 11 12 13
      DATA MAP/ 0,0,8,3, 9,10,6,1,8,4, 5, 1, 2, 7/
ccccc           b i r d  c  l a h s 

      LOGICAL ZTRUE,ZFALSE
      INTEGER ITRUE,IFALSE
      EQUIVALENCE( ITRUE, ZTRUE ), (IFALSE, ZFALSE)
      DATA ZTRUE,ZFALSE/ .true. , .false./ ! for testing assumptions
      
      CHARACTER * (*) CRAYCH(0:0)
      EXTERNAL IEG2CRAY

      CRI2CRAY = -1   !  Some parameter error, assume failure
      IF (NUMARG() .LT. 5 .or. NUMARG() .GT. 7) GOTO 9000  !UTERP(034)
      INC=1
      IF (NUMARG() .GE. 6) INC=STRIDE
      IF( NUM .LT. 0 ) GOTO 9000
      
      CRI2CRAY = 0
      IF( NUM .EQ. 0 ) GOTO 9000   !  Return with no results.

      IF( TYPE .LT. 0 .or. TYPE .GT. MAXTYPE ) GOTO 9000 !UTERP(034)

      I = MAP(TYPE)
      IF( I .EQ. 9 ) GO TO 9004  ! complex(4)
      IF( I .GT. 9 ) GO TO 9005  ! logical(5)
  100 CRI2CRAY = IEG2CRAY( I, NUM, FORN, BITOFF, CRAY, INC, CRAYCH)

 9000 CONTINUE
      RETURN

c     LOGICAL*8 (nonzero=.true. 0=.false.)-> LOGICAL*8 (1=.true. 0=.false.)

 9005 CONTINUE
c     CRAY-t3d and CRAY-2:   cft77 .true. is 1 
c     CRAY-y/x/1: cft77 .true. is -1 ( all bits set ) 
c     Some cft77 ymp logicals have only sign bit set, so test if nonzero,
c        just in case.

      DO j = 0, num-1
            IF( DSHIFTL(FORN(J),FORN(J+1),BITOFF) .NE. 0 ) THEN
               CRAY(J*INC) = ITRUE
            ELSE
               CRAY(J*INC) = IFALSE
            ENDIF
      ENDDO
      GOTO 9000

 9004 CONTINUE
        IF( INC .EQ. 1) THEN

c       unit stride: treat as real*8 array of length 2*NUM stride 1:
          CRI2CRAY = IEG2CRAY(8, NUM*2, FORN, BITOFF, CRAY, 1, CRAYCH)
        ELSE

c     alternative 2: complex*16 STRIDE>1: LNCHNK-item contiguous batches:
c                     (good performance: NUM/64 calls to conversion)

          I=-99999                       !internal error: remove shortloop
          IF( LNCHNK .GT. 64 ) GOTO 800  ! needed due to shortloop below
          CRI2CRAY = 0
          DO J=0,NUM-1,LNCHNK
            ITMP5=MIN(LNCHNK,NUM-J)  ! number of complex in this batch
            I = IEG2CRAY(8, ITMP5*2, FORN(2*J),BITOFF,ITMP4,1,CRAYCH)
            IF( I .LT. 0 ) GOTO 800   ! I<0:  terminate with error code -I
            CRI2CRAY = I + CRI2CRAY   ! I>=0: may be underflow/overflow count
cdir$       shortloop
            DO JJ=J,ITMP5-1+J
              CRAY(INC*2*JJ ) = ITMP4(0,JJ-J)
            ENDDO
cdir$       shortloop
            DO JJ=J,ITMP5-1+J         ! loop split to allow short vectorization
              CRAY(INC*2*JJ+1 ) = ITMP4(1,JJ-J)
            ENDDO
          ENDDO
        ENDIF

cc    alternative 4: if num is much larger than 64, consider using
cc    CRAY(J*2*inc+1) (j=k,num) to convert (num-k)/2 complex. This converts
cc    with log2(num) calls to ieg2cray:
cc    k=0
cc    DO WHILE ( num-k >1 )
cc      kount=(num-k)/2
cc      ieg2cray(8, 2*kount, forn(2*k), bitoff, cray(2*inc*k), 2*inc, craych)
cc      DO j=0,kount-1                                ! move imaginary parts
cc         {cray(2*inc*(j+k)+1)=cray(2*inc*(2*j+k+1)) }
cc      DO j=1,kount-1                                ! move real parts
cc        { cray(2*inc*(j+k)  )=cray(2*inc*(2*j+k  )) }
cc      k=k+kount
cc    ENDDO
cc    IF( num-k .EQ. 1 )                        ! convert final complex
cc      ieg2cray(8, 2, forn(2*k), bitoff, cray(2*inc*k), 1, craych)

cc    Instead of this final conversion, it could switch over to the
cc    64*2 buffered method above when num-k < 65.  This would save for
cc    lesser values of num.


cc     alternative 3: treat as 2 REAL arrays of length NUM and stride 2*STRIDE
cc           (good performance: 2 calls to conversion, but extra work below)
cc     The results will be incorrectly ordered, so we need to permute them.
cc     I tried a permutation f(j) = 2*j mod (2*num -1), but
cc     multiple cycles exist for many values of num (always for num=2**k):
cc     NUM=5: 1 2 4 8 7 5 1      3 6 3
cc     NUM=4: 1 2 4 1            3 6 5 3
cc     NUM=3: 1 2 4 3 1          (single cycle)
cc     This idea needs more work.

        GO TO 9000

  800 CRI2CRAY = I  !fail, return with negative error code
c      PRINT *,"CRI2CRAY 800:",CRI2CRAY
c          print *, ( 0.0+OR(CRAY(J*INC*2+0),0),J=0,NUM-1)
c          print *, ( 0.0+OR(CRAY(J*INC*2+1),0),J=0,NUM-1)
      
      GO TO 9000

CDIR$ ID "@(#) libu/ieg/cri2cray.f	92.0	10/08/98 14:57:41"
      END
