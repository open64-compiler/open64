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
     +VAX2CRAY(TYPE, NUM, FORN, BITOFF, CRAY, STRD, CRAYCHAR)

CCCCCC      IMPLICIT NONE

      INTEGER MASK32
      PARAMETER (MASK32 = 37777777777B)
C
C     The VAX2CRAY subroutine converts VAX/VMS numbers to CRAY format
C
C     The returned function value is 0 if OK, >0 if numer errors
C     occurred, and <0 if an error.  The error number is the negative
C     of the return value.
C
      INTEGER TYPE                ! The variable type code used by the
C                                 ! libraries 0=typeless
C                                 !           1=integer
C                                 !           2=real
C                                 !           3=double precision
C                                 !           4=complex
C                                 !           5=logical
C                                 !           6=character
C                                 !           7=short int
C                                 !           8=double to single precision
C                                 !
      INTEGER NUM                 ! Number of foreign data items to
C                                 ! convert.  Type integer variable,
C                                 ! expression, or constant.
C                                 !
      INTEGER FORN(0:0)           ! Variable or array of any type
C                                 ! or length containing VAX D_format
C                                 ! numbers to convert.
C                                 !
      INTEGER BITOFF              ! Bit number within FORN to begin the
C                                 ! conversion.  Type integer variable,
C                                 ! expression, or constant.  Bits are
C                                 ! numbered from 0, beginning at the
C                                 ! leftmost bit of FORN.
C                                 !
      INTEGER CRAY(0:0)           ! Variable or array to
C                                 ! contain the converted values.
C                                 !
      INTEGER STRD                ! Memory increment for storing the
C                                 ! conversion results in CRAY. The input
C                                 ! bits are taken from FORN regardless
C                                 ! of this parameter in a continuous bit
C                                 ! stream.  Optional
C                                 ! parameter of type integer variable,
C                                 ! expression, or constant.  Default
C                                 ! value is 1.
C                                 !
      CHARACTER*1 CRAYCHAR(0:0)   ! Optional parameter specifying
C                                 ! CRAY target variable if it is of
C                                 ! CHARACTER type

      REAL    CRAYR(0:0)
      DOUBLE PRECISION CRAYDBL(0:0)
      COMPLEX CRAYCPL(0:0)
      INTEGER CRAYL(0:0)            ! cheat

      POINTER (RPTR,CRAYR)
      POINTER (DPPTR,CRAYDBL)
      POINTER (CPTR, CRAYCPL)
      POINTER (LPTR, CRAYL)
C
C     Vector Functions
C
      INTEGER           VM64I       ! boolean
      INTEGER           VI32I
      REAL              VF32I
      DOUBLE PRECISION  VD64I
      COMPLEX           VC64I
      INTEGER           VL32I
C     CHARACTER*1       VCHRI(0:0)
      INTEGER           VCHRI
      INTEGER           VI16I       ! Really INTEGER*2
      REAL              SNGLR

CDIR$ VFUNCTION VM64I
CDIR$ VFUNCTION VI32I
CDIR$ VFUNCTION VF32I
CDIR$ VFUNCTION VD64I
CDIR$ VFUNCTION VC64I
CDIR$ VFUNCTION VL32I
CDIR$ VFUNCTION VCHRI
CDIR$ VFUNCTION VI16I
CDIR$ VFUNCTION SNGLR
CCCCCC      EXTERNAL VM64I,VI32I,VF32I,VD64I,VC64I,VL32I,VCHRI,VI16I,SNGLR

      INTEGER I, J, AND, MYCHAR,IOUT,JNC, BO, DUMMY, ISTART
      EXTERNAL VXUNPACK

CDIR$ EJECT

      IF (NUM .LE. 0) RETURN
      IF (BITOFF .GE. 64) GOTO 9000 ! CALL UTERP(034)

      IF (NUMARG() .EQ. 5) THEN
        JNC = 1  
      ELSE IF (NUMARG() .GE. 6) THEN
        JNC = STRD
      ELSE  
        GOTO 9000 ! CALL UTERP(034)
      ENDIF
   
      IF (TYPE .EQ. 6 .AND. NUMARG() .LT. 7)
     +  GOTO 9000 ! CALL UTERP(034) ! need CRAYCHAR to do characters

      IOUT = 0
      RPTR = LOC(CRAY(0))
      DPPTR = LOC(CRAY(0))
      CPTR = LOC(CRAY(0))
      LPTR = LOC(CRAY(0))
      GOTO (100,200,300,400,500,600,700,800,900),TYPE+1
C                                 !  VM64I    0=typeless
  100 CONTINUE
      IF (BITOFF.EQ.0) THEN
        DO 105 I=0,NUM-1
          CRAY(IOUT) = FORN(I)
          IOUT = IOUT + JNC
  105   CONTINUE
      ELSE
        DO 110 I=0,NUM-1
          CRAY(IOUT) = VM64I(OR(SHIFTL(FORN(I  ),BITOFF),
     +                          SHIFTR(FORN(I+1),64-BITOFF)))
          IOUT = IOUT + JNC
  110   CONTINUE
      ENDIF
      GOTO 1000
C                                 !  VI32I    1=integer
  200 CONTINUE
C
C Special case the handling of the first element, as the main loop
C works on pairs.  Adjust the initial values accordingly
C
      BO = BITOFF
      J = 0
      IOUT = 0
      ISTART = 0
      IF (AND(NUM,1).NE.0) THEN
        IF (BO.LT.32) THEN
          CRAY(0) = VI32I(AND(SHIFTR(FORN(J),32-BO),MASK32))
          BO = BO+32
        ELSE ! bitoff >= 32
          CRAY(0) = VI32I(AND(OR(SHIFTL(FORN(J),BO-32),
     +                           SHIFTR(FORN(J+1),96-BO)),
     +                           MASK32))
          BO = BO-32
          J = J + 1
        ENDIF
        IOUT = IOUT+JNC
        ISTART = 1
      ENDIF
C
C Main loops: Two outputs for each input 64bit word
C
C WARNING: The following loops are structured to avoid problems
C due to potential overlap in the FORN and CRAY arrays.
C VTEMP2 is passed to the first VFUNCTION to encourage the compiler
C to compute VTEMP2 before storing CRAY.  Do not change this loop
C without considering this overlap
C
      I = J
      IF (BO.LT.32) THEN
        DO 210 DUMMY=ISTART,NUM-1,2
          VTEMP1 = AND(SHIFTR(FORN(I),32-BO),MASK32)
          VTEMP2 = AND(OR(SHIFTL(FORN(I),BO),
     +                    SHIFTR(FORN(I+1),64-BO)),MASK32)
          CRAY(IOUT)     = VI32I(VTEMP1,VTEMP2)
          CRAY(IOUT+JNC) = VI32I(VTEMP2)
          IOUT = IOUT + JNC*2
          I = I + 1
  210   CONTINUE
      ELSE
        DO 220 DUMMY=ISTART,NUM-1,2
          VTEMP1 = AND(OR(SHIFTL(FORN(I),BO-32),
     +                        SHIFTR(FORN(I+1),96-BO)),MASK32)
          VTEMP2 = AND(SHIFTR(FORN(I+1),64-BO),MASK32)
          CRAY(IOUT)     = VI32I(VTEMP1,VTEMP2)
          CRAY(IOUT+JNC) = VI32I(VTEMP2)
          IOUT = IOUT + JNC*2
          I = I + 1
  220   CONTINUE
      ENDIF
      GOTO 1000
C                                 !  VF32I    2=real
  300 CONTINUE
C
C Special case the handling of the first element, as the main loop
C works on pairs.  Adjust the initial values accordingly
C
      BO = BITOFF
      J = 0
      IOUT = 0
      ISTART = 0
      IF (AND(NUM,1).NE.0) THEN
        IF (BO.LT.32) THEN
          CRAYR(0) = VF32I(AND(SHIFTR(FORN(J),32-BO),MASK32))
          BO = BO+32
        ELSE ! bitoff >= 32
          CRAYR(0) = VF32I(AND(OR(SHIFTL(FORN(J),BO-32),
     +                            SHIFTR(FORN(J+1),96-BO)),
     +                            MASK32))
          BO = BO-32
          J = J + 1
        ENDIF
        IOUT = IOUT+JNC
        ISTART = 1
      ENDIF
C
C Main loops: Two outputs for each input 64bit word
C
C WARNING: The following loops are structured to avoid problems
C due to potential overlap in the FORN and CRAY arrays.
C VTEMP2 is passed to the first VFUNCTION to encourage the compiler
C to compute VTEMP2 before storing CRAY.  Do not change this loop
C without considering this overlap
C
      I = J
      IF (BO.LT.32) THEN
        DO 310 DUMMY=ISTART,NUM-1,2
          VTEMP1 = AND(SHIFTR(FORN(I),32-BO),MASK32)
          VTEMP2 = AND(OR(SHIFTL(FORN(I),BO),
     +                             SHIFTR(FORN(I+1),64-BO)),MASK32)
          CRAYR(IOUT)     = VF32I(VTEMP1,VTEMP2)
          CRAYR(IOUT+JNC) = VF32I(VTEMP2)
          IOUT = IOUT + JNC*2
          I = I + 1
  310   CONTINUE
      ELSE
        DO 320 DUMMY=ISTART,NUM-1,2
          VTEMP1 = AND(OR(SHIFTL(FORN(I),BO-32),
     +               SHIFTR(FORN(I+1),96-BO)),MASK32)
          VTEMP2 = AND(SHIFTR(FORN(I+1),64-BO),MASK32)
          CRAYR(IOUT)     = VF32I(VTEMP1,VTEMP2)
          CRAYR(IOUT+JNC) = VF32I(VTEMP2)
          IOUT = IOUT + JNC*2
          I = I + 1
  320   CONTINUE
      ENDIF
      GOTO 1000
C                                 !  VD64I    3=double precision
  400 CONTINUE
      IF (BITOFF.EQ.0) THEN
        DO 405 I=0,NUM-1
          CRAYDBL(IOUT) = VD64I(FORN(I))
          IOUT = IOUT + JNC
  405   CONTINUE
      ELSE
        DO 410 I=0,NUM-1
          CRAYDBL(IOUT) = VD64I(OR(SHIFTL(FORN(I  ),BITOFF),
     +                          SHIFTR(FORN(I+1),64-BITOFF)))
          IOUT = IOUT + JNC
  410   CONTINUE
      ENDIF
      GOTO 1000
C                                 !  VC64I    4=complex
  500 CONTINUE
      IF (BITOFF.EQ.0) THEN
        DO 505 I=0,NUM-1
          CRAYCPL(IOUT) = VC64I(FORN(I))
          IOUT = IOUT + JNC
  505   CONTINUE
      ELSE
        DO 510 I=0,NUM-1
          CRAYCPL(IOUT) = VC64I(OR(SHIFTL(FORN(I  ),BITOFF),
     +                             SHIFTR(FORN(I+1),64-BITOFF)))
          IOUT = IOUT + JNC
  510   CONTINUE
      ENDIF
      GOTO 1000
C                                 !  VL32I    5=logical
  600 CONTINUE
C
C Special case the handling of the first element, as the main loop
C works on pairs.  Adjust the initial values accordingly
C
      BO = BITOFF
      J = 0
      IOUT = 0
      ISTART = 0
      IF (AND(NUM,1).NE.0) THEN
        IF (BO.LT.32) THEN
          CRAY(0) = VL32I(AND(SHIFTR(FORN(J),32-BO),MASK32))
          BO = BO+32
        ELSE ! bitoff >= 32
          CRAY(0) = VL32I(AND(OR(SHIFTL(FORN(J),BO-32),
     +                            SHIFTR(FORN(J+1),96-BO)),
     +                            MASK32))
          BO = BO-32
          J = J + 1
        ENDIF
        IOUT = IOUT+JNC
        ISTART = 1
      ENDIF
C
C Main loops: Two outputs for each input 64bit word
C
C WARNING: The following loops are structured to avoid problems
C due to potential overlap in the FORN and CRAY arrays.
C VTEMP2 is passed to the first VFUNCTION to encourage the compiler
C to compute VTEMP2 before storing CRAY.  Do not change this loop
C without considering this overlap
C
      I = J
      IF (BO.LT.32) THEN
        DO 610 DUMMY=ISTART,NUM-1,2
          VTEMP1 = AND(SHIFTR(FORN(I),32-BO),MASK32)
          VTEMP2 = AND(OR(SHIFTL(FORN(I),BO),
     +                             SHIFTR(FORN(I+1),64-BO)),MASK32)
          CRAY(IOUT)     = VL32I(VTEMP1,VTEMP2)
          CRAY(IOUT+JNC) = VL32I(VTEMP2)
          IOUT = IOUT + JNC*2
          I = I + 1
  610   CONTINUE
      ELSE
        DO 620 DUMMY=ISTART,NUM-1,2
          VTEMP1 = AND(OR(SHIFTL(FORN(I),BO-32),
     +                        SHIFTR(FORN(I+1),96-BO)),MASK32)
          VTEMP2 = AND(SHIFTR(FORN(I),64-BO),MASK32)
          CRAY(IOUT)     = VL32I(VTEMP1,VTEMP2)
          CRAY(IOUT+JNC) = VL32I(VTEMP2)
          IOUT = IOUT + JNC*2
          I = I + 1
  620   CONTINUE
      ENDIF
      GOTO 1000
C                                 !  VCHRI    6=character
  700 CONTINUE
C
C Convert character items.  This goes from packed to packed
C
      DO 710 I=0,NUM-1
        J = (I*8+BITOFF)/64
        MYCHAR = SHIFTR(FORN(J),56-AND(I+BITOFF/8,7)*8)
        MYCHAR = VCHRI(MYCHAR)
        CRAYCHAR(IOUT) = CHAR(MYCHAR)
        IOUT = IOUT + JNC
  710 CONTINUE
      GOTO 1000
C                                 !  VI16I    7=short int
  800 CONTINUE
      CALL VXUNPACK(FORN,(BITOFF/8)+1,CRAY,NUM,16,JNC)
      IOUT = 0
      DO 810 I = 0,NUM-1
        CRAY(IOUT) = VI16I(CRAY(IOUT))
        IOUT = IOUT + JNC
  810 CONTINUE
      GOTO 1000
C                                 !  VD64I    8=double to single precision
  900 CONTINUE
      IF (BITOFF.EQ.0) THEN
        DO I=0,NUM-1
          CRAYR(IOUT) = SNGLR(VD64I(FORN(I)))
          IOUT = IOUT + JNC
        ENDDO
      ELSE
        DO I=0,NUM-1
          CRAYR(IOUT) = SNGLR(VD64I(OR(SHIFTL(FORN(I  ),BITOFF),
     +                                SHIFTR(FORN(I+1),64-BITOFF))))
          IOUT = IOUT + JNC
        ENDDO
      ENDIF
      GOTO 1000


 1000 CONTINUE
      VAX2CRAY = 0    ! we don't return status yet...
      RETURN

 9000 CONTINUE
      VAX2CRAY = -1    ! parameter problems
      RETURN
CDIR$ ID "@(#) libu/vms/vax2cray.f	92.0	10/08/98 14:57:41"
      END
