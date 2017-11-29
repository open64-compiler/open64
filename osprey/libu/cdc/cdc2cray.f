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
     +CDC2CRAY(TYPE, NUM, FORN, BITOFF, CRAY, STRD, CRAYCHAR)

CCCCCC      IMPLICIT NONE

      INTEGER MASK60
      PARAMETER (MASK60 = 77777777777777777777B)
C
C     The CDC2CRAY subroutine converts CDC numbers to CRAY format
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
C                                 !
      INTEGER NUM                 ! Number of foreign data items to
C                                 ! convert.  Type integer variable,
C                                 ! expression, or constant.
C                                 !
      INTEGER FORN(0:0)           ! Variable or array of any type
C                                 ! or length containing CDC format
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
      INTEGER           CB64I       ! boolean
      INTEGER           CI60I
      REAL              CF60I
      DOUBLE PRECISION  CD120I
      COMPLEX           CC120I
      INTEGER           CL60I
C     CHARACTER*1       CCHRI(0:0)
      INTEGER           CCHRI
C     INTEGER           CI60I       ! Really INTEGER*2

CDIR$ VFUNCTION CB64I
CDIR$ VFUNCTION CI60I
CDIR$ VFUNCTION CF60I
CDIR$ VFUNCTION CD120I
CDIR$ VFUNCTION CC120I
CDIR$ VFUNCTION CL60I
CDIR$ VFUNCTION CCHRI
CCCCCC      EXTERNAL CB64I,CI60I,CF60I,CD120I,CC120I,CL60I,CCHRI,CI60I

      INTEGER I, J, AND, MYCHAR,IOUT,JNC, BO, LEFT
      INTEGER FWORD, FWORD2, LASTJ
      
CDIR$ EJECT

      IF (NUM .LE. 0) RETURN
      IF (BITOFF .GE. 64) GOTO 999 ! CALL UTERP(034)

      IF (NUMARG() .EQ. 5) THEN
        JNC = 1  
      ELSE IF (NUMARG() .GE. 6) THEN
        JNC = STRD
      ELSE  
        GOTO 999 ! CALL UTERP(034)
      ENDIF 
   
      IF (TYPE .EQ. 6 .AND. NUMARG() .LT. 7)
     +  GOTO 999 ! CALL UTERP(034) ! need CRAYCHAR to do characters

      IOUT = 0
      RPTR = LOC(CRAY(0))
      DPPTR = LOC(CRAY(0))
      CPTR = LOC(CRAY(0))
      LPTR = LOC(CRAY(0))
      GOTO (100,200,300,400,500,600,700,800),TYPE+1
C                                 !  CB64I    0=typeless
  100 CONTINUE
      IF (BITOFF.EQ.0) THEN
        DO 105 I=0,NUM-1
          CRAY(IOUT) = FORN(I)
          IOUT = IOUT + JNC
  105   CONTINUE
      ELSE
        DO 110 I=0,NUM-1
          CRAY(IOUT) = OR(SHIFTL(FORN(I  ),BITOFF),
     +                          SHIFTR(FORN(I+1),64-BITOFF))
          IOUT = IOUT + JNC
  110   CONTINUE
      ENDIF
      GOTO 900
C                                 !  CI60I    1=integer
  200 CONTINUE
C
C Unpack the cyber data to the user's array, and convert it there.
C
      CALL U6064(FORN(0), BITOFF, CRAY(0), NUM, JNC)
      DO 210 I = 0,NUM-1
        CRAY(IOUT)     = CI60I((SHIFTR(CRAY(IOUT),4)))
        IOUT = IOUT + JNC
  210 CONTINUE
      GOTO 900
C                                 !  CF60I    2=real
  300 CONTINUE
C
C Unpack the cyber data to the user's array, and convert it there.
C
      CALL U6064(FORN(0), BITOFF, CRAYR(0), NUM, JNC)
      DO 310 I = 0,NUM-1
        CRAYR(IOUT)     = CF60I((SHIFTR(CRAY(IOUT),4)))
        IOUT = IOUT + JNC
  310 CONTINUE
      GOTO 900
C                                 !  CD120I    3=double precision
  400 CONTINUE
C
C Unpack the cyber data to the user's array, and convert it there.
C
      IF (JNC.NE.1) GOTO 999      ! Not supported yet!!
C
      CALL U6064(FORN(0), BITOFF, CRAYDBL(0), NUM*2)
      DO 410 I = 0,NUM-1
        CRAYDBL(IOUT)     = CD120I(CMPLX(
     +                             FLOAT(SHIFTR(CRAY(IOUT*2),4)),
     +                             FLOAT(SHIFTR(CRAY(IOUT*2+1),4))))
        IOUT = IOUT + JNC
  410 CONTINUE
      GOTO 900
C                                 !  CC120I    4=complex
  500 CONTINUE
C
C Unpack the cyber data to the user's array, and convert it there.
C
      IF (JNC.NE.1) GOTO 999      ! Not supported yet!!
C
      CALL U6064(FORN(0), BITOFF, CRAYCPL(0), NUM*2)
      DO 510 I = 0,NUM-1
        CRAYCPL(IOUT)     = CC120I(CMPLX(
     +                             FLOAT(SHIFTR(CRAY(IOUT*2),4)),
     +                             FLOAT(SHIFTR(CRAY(IOUT*2+1),4))))
        IOUT = IOUT + JNC
  510 CONTINUE
      GOTO 900
C                                 !  CL60I    5=logical
  600 CONTINUE
C
C Unpack the cyber data to the user's array, and convert it there.
C
      CALL U6064(FORN(0), BITOFF, CRAYL(0), NUM, JNC)
      DO 610 I = 0,NUM-1
        CRAYL(IOUT)     = CL60I((SHIFTR(CRAY(IOUT),4)))
        IOUT = IOUT + JNC
  610 CONTINUE
      GOTO 900
C                                 !  CCHRI    6=character
  700 CONTINUE
C
C Convert character items.  This goes from packed to packed
C Because this must work for conversions in place, check and compensate
C for the case where the last two characters might get trashed in the
C conversion.  We can't just run the loop backwards because the CRAY
C characters may extend beyond both ends of the CDC character data.
C This is a temporary fix that really should be done in some other way.
C
      IOUT = 0
      LASTJ = -1
      FWORD2 = FORN(BITOFF/64)
      DO 710 I=0,NUM-1
        J = (I*6+BITOFF)/64
        IF (J.NE.LASTJ) THEN
          FWORD = FWORD2
          FWORD2 = FORN(J+1)
          LASTJ = J
        ENDIF
        BO = AND(BITOFF+I*6,63)
        IF (BO.LE.58) THEN
          MYCHAR = SHIFTR(FWORD,58-BO)
        ELSE
          MYCHAR = SHIFTR(OR(SHIFTL(FWORD ,BO),
     +                       SHIFTR(FWORD2,64-BO)),58)
        ENDIF
        MYCHAR = CCHRI(AND(MYCHAR,63))
        CRAYCHAR(IOUT) = CHAR(MYCHAR)
        IOUT = IOUT + JNC
  710 CONTINUE
      GOTO 900
C                                 !  CI60I    7=short int
  800 CONTINUE
C
C Unpack the cyber data to the user's array, and convert it there.
C
      CALL U6064(FORN(0), BITOFF, CRAY(0), NUM, JNC)
      DO 810 I = 0,NUM-1
        CRAY(IOUT)     = CI60I((SHIFTR(CRAY(IOUT),4)))
        IOUT = IOUT + JNC
  810 CONTINUE

  900 CONTINUE
      CDC2CRAY = 0    ! we don't return status yet...
      RETURN

  999 CONTINUE
      CDC2CRAY = -1    ! some parameter error
      RETURN
CDIR$ ID "@(#) libu/cdc/cdc2cray.f	92.0	10/08/98 14:57:41"
      END
