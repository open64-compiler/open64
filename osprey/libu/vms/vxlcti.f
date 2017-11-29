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


      SUBROUTINE VXLCTI(SRC,DEST,ISB,NUM,LEN,IER,INC)
      INTEGER SRC(1)
      INTEGER DEST(1)
      INTEGER ISB
      INTEGER NUM
      INTEGER LEN
      INTEGER INC

      INTEGER ITEMP(64), JTEMP(32)

CDIR$ VFUNCTION VL32O
      INTEGER   VL32O

      IF (NUMARG() .EQ. 6) THEN
        JNC = 1
      ELSE IF (NUMARG() .EQ. 7) THEN
        JNC = INC
      ELSE
        CALL ABORT('VAX conversion routine called with incorrect number 
     +of arguments')
      ENDIF

      IF ((NUM .LT. 0) .OR. (LEN .NE. 4)) THEN
        CALL ABORT('Invalid input to VAX conversion routine')
      ENDIF

      NUMC = NUM
      JSB  = ISB
      IPTR = 1
      IER  = 0

      IF (NUM .EQ. 0) RETURN

   40 CONTINUE

      NUMP = MIN0(NUMC,64)

CDIR$ SHORTLOOP
      DO 100 I = 1, NUMP
        ITEMP(I) = VL32O(SRC(IPTR))
        IPTR     = IPTR + JNC
  100 CONTINUE

      CALL PACK(JTEMP,32,ITEMP,AND(MASK(63),NUMP+1))
      CALL VXMOVE00(JTEMP,1,NUMP*LEN,DEST,JSB)

      JSB  = JSB + NUMP*LEN
      NUMC = NUMC - NUMP
      IF (NUMC .GT. 0) GO TO 40

      RETURN
CDIR$ ID "@(#) libu/vms/vxlcti.f	92.0	10/08/98 14:57:41"
      END
