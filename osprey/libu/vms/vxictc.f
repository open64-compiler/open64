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


      SUBROUTINE VXICTC(IN,ISB,DEST,NUM,LEN,INC)
C
C     VAX INTEGER TO CRAY INTEGER
C
C     The VXICTC subroutine converts VAX 16- and 32-bit integers to
C     CRAY 64-bit integers.
C
      INTEGER IN(1)               ! Variable or array of any type or
C                                 ! length containing VAX 16- or 32-
C                                 ! bit integers.
C
      INTEGER ISB                 ! Byte number to begin the conversion.
C                                 ! Type integer variable, expression,
C                                 ! or constant.  Bytes are numbered
C                                 ! from 1, beginning at the leftmost
C                                 ! byte position of IN.
C
      INTEGER DEST(1)             ! Variable or array of type integer
C                                 ! to contain the converted values.
C
      INTEGER NUM                 ! Number of VAX integers to convert.
C                                 ! Type integer variable, expression,
C                                 ! or constant.
C
      INTEGER LEN                 ! Size of the VAX numbers to convert.
C                                 ! This value must be 2 or 4.  A value
C                                 ! of 2 indicates input integers are
C                                 ! 16-bit.  A value of 4 indicates
C                                 ! input integers are 32-bit.  Type
C                                 ! integer variable, expression, or
C                                 ! constant.
C
      INTEGER INC                 ! Memory increment for storing
C                                 ! conversion results in DEST.  Optional
C                                 ! parameter of type integer variable,
C                                 ! expression, or constant.  Default
C                                 ! value is 1.

CDIR$ VFUNCTION VI16I
CDIR$ VFUNCTION VI32I
      INTEGER   VI16I
      INTEGER   VI32I

      IF (NUMARG() .EQ. 5) THEN
        JNC = 1
      ELSE IF (NUMARG() .EQ. 6) THEN
        JNC = INC
      ELSE
        CALL ABORT('VAX conversion routine called with incorrect number 
     +of arguments')
      ENDIF

      IF (NUM .LT. 0) THEN
        CALL ABORT('Invalid input to VAX conversion routine')
      ENDIF

      IF (NUM .EQ. 0) RETURN

      IOUT = 1

      IF (LEN .EQ. 2) THEN
        CALL VXUNPACK(IN,ISB,DEST,NUM,16,JNC)
        DO 120 I = 1, NUM
          DEST(IOUT) = VI16I(DEST(IOUT))
          IOUT       = IOUT + JNC
  120   CONTINUE
      ELSE IF (LEN .EQ. 4) THEN
        CALL VXUNPACK(IN,ISB,DEST,NUM,32,JNC)
        DO 180 I = 1, NUM
          DEST(IOUT) = VI32I(DEST(IOUT))
          IOUT       = IOUT + JNC
  180   CONTINUE
      ELSE
        CALL ABORT('Invalid input to VAX conversion routine')
      ENDIF

      RETURN
CDIR$ ID "@(#) libu/vms/vxictc.f	92.0	10/08/98 14:57:41"
      END
