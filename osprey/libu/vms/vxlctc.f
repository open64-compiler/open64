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


      SUBROUTINE VXLCTC(SRC,ISB,DEST,NUM,LEN,INC)
C
C     The VXLCTC subroutine converts VAX logical values to Cray
C     64-bit logical values.

      INTEGER SRC(1)              ! Variable or array of any 
C                                 ! non-character type and of any
C                                 ! length containing VAX logical values
C                                 ! to convert.
C
      INTEGER ISB                 ! Byte number to begin the conversion.
C                                 ! Type integer variable, expression,
C                                 ! or constant.  Bytes are numbered
C                                 ! from 1, beginning at the leftmost
C                                 ! byte position of SRC.
C
      LOGICAL DEST(1)             ! Variable or array of type logical to
C                                 ! contain the converted values.
C
      INTEGER NUM                 ! Number of VAX logical values to be
C                                 ! converted.  Type integer variable,
C                                 ! expression, or constant.
C
      INTEGER LEN                 ! Size of the VAX logical values to
C                                 ! convert.  At present, this parameter
C                                 ! must be set to 4, indicating that
C                                 ! 32-bit logical values are to be
C                                 ! converted.  Type integer variable,
C                                 ! expression, or constant.
C
      INTEGER INC                 ! Memory increment for storing the
C                                 ! conversion results in DEST.
C                                 ! Optional parameter of type integer
C                                 ! variable, expression, or constant.
C                                 ! Default value is 1.

CDIR$ VFUNCTION VL32I
      LOGICAL   VL32I

      IF (NUMARG() .EQ. 5) THEN
        JNC = 1
      ELSE IF (NUMARG() .EQ. 6) THEN
        JNC = INC
      ELSE
        CALL ABORT('VAX conversion routine called with incorrect number 
     +of arguments')
      ENDIF

      IF ((NUM .LT. 0) .OR. (LEN .NE. 4)) THEN
        CALL ABORT('Invalid input to VAX conversion routine')
      ENDIF

      IF (NUM .EQ. 0) RETURN

      IOUT = 1

      CALL VXUNPACK(SRC,ISB,DEST,NUM,32,JNC)

      DO 180 I = 1, NUM
        DEST(IOUT) = VL32I(DEST(IOUT))
        IOUT       = IOUT + JNC
  180 CONTINUE

      RETURN
CDIR$ ID "@(#) libu/vms/vxlctc.f	92.0	10/08/98 14:57:41"
      END
