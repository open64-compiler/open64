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


      SUBROUTINE VXICTI(IN,DEST,ISB,NUM,LEN,IER,INC)
C
C     The VXICTI subroutine converts Cray 64-bit integer numbers into
C     either VAX INTEGER*2 or INTEGER*4 numbers.  Numbers that produce
C     an overflow when converted to VAX format are converted to the
C     largest VAX integer representation with the sign bit set if
C     negative.  [Deferred implementation:  An error parameter returns
C     nonzero to indicate that one or more numbers converted produced an
C     overflow.  (At present, you must supply the parameter, but it is
C     always returned as zero.)]
C
      INTEGER IN(1)               ! Variable or array of any length and
C                                 ! type integer, containing Cray
C                                 ! integer numbers to convert.
C
      INTEGER DEST(1)             ! Variable or array of type integer to
C                                 ! contain the converted values.
C
      INTEGER ISB                 ! Byte number at which to begin
C                                 ! storing the converted results.  Type
C                                 ! integer variable, expression, or
C                                 ! constant.  Bytes are numbered from
C                                 ! 1, beginning at the leftmost byte
C                                 ! position of dest.
C
      INTEGER NUM                 ! Number of Cray integers to convert.
C                                 ! Type integer variable, expression,
C                                 ! or constant.
C
      INTEGER LEN                 ! Size of the VAX result numbers.
C                                 ! This value must be 2 or 4.  A value
C                                 ! of 2 indicates output integers are
C                                 ! INTEGER*2 (16-bit).  A value of 4
C                                 ! indicates output integers are
C                                 ! INTEGER*4 (32-bit).  Type integer
C                                 ! variable, expression, or constant.
C
      INTEGER IER                 ! Overflow indicator of type integer.
C                                 ! Value is 0 if all Cray values are
C                                 ! converted to VAX values without
C                                 ! overflow.  Value is nonzero if one
C                                 ! or more Cray values overflowed in
C                                 ! the conversion.
C
      INTEGER INC                 ! Memory increment for fetching the
C                                 ! number to be converted.  Optional
C                                 ! parameter of type integer variable,
C                                 ! expression, or constant.  Default
C                                 ! value is 1.

      INTEGER ITEMP(64)
      INTEGER JTEMP(32)

CDIR$ VFUNCTION VI16O
CDIR$ VFUNCTION VI32O

      INTEGER   VI16O
      INTEGER   VI32O

      IF (NUMARG() .EQ. 6) THEN
        JNC = 1
      ELSE IF (NUMARG() .EQ. 7) THEN
        JNC = INC
      ELSE
        CALL ABORT('VAX conversion routine called with incorrect number 
     +of arguments')
      ENDIF

      IF (NUM .LT. 0) THEN
        CALL ABORT('Invalid input to VAX conversion routine')
      ENDIF

      NUMC = NUM
      JSB  = ISB
      IPTR = 1
      IER  = 0
 
      IF (NUM .EQ. 0) RETURN

   40 CONTINUE

      NUMP = MIN0(NUMC,64)

      IF (LEN .EQ. 2) THEN
CDIR$ SHORTLOOP
        DO 80 I = 1, NUMP
          ITEMP(I) = VI16O(IN(IPTR))
          IPTR     = IPTR + JNC
   80   CONTINUE
          CALL PACK(JTEMP,16,ITEMP,AND(MASK(62),NUMP+3))
          IMOV = NUMP*2
      ELSE IF (LEN .EQ. 4) THEN
CDIR$ SHORTLOOP
        DO 120 I = 1, NUMP
          ITEMP(I) = VI32O(IN(IPTR))
          IPTR     = IPTR + JNC
  120   CONTINUE
        CALL PACK(JTEMP,32,ITEMP,AND(MASK(63),NUMP+1))
        IMOV = NUMP*4
      ELSE
        CALL ABORT('Invalid input to VAX conversion routine')
      ENDIF
      CALL VXMOVE00(JTEMP,1,IMOV,DEST,JSB)

      JSB = JSB + IMOV
      NUMC = NUMC - NUMP
      IF (NUMC .GT. 0) GO TO 40

      RETURN
CDIR$ ID "@(#) libu/vms/vxicti.f	92.0	10/08/98 14:57:41"
      END
