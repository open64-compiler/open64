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


      INTEGER FUNCTION OVERZ000(LEVEL,NUMBER,OTABLE,RECALL)
      IMPLICIT INTEGER(A-Z)
      INTEGER LEVEL
      INTEGER NUMBER
      INTEGER OTABLE(1)
      INTEGER RECALL
C
C     OTABLE(1) == 1 or 2, depending on overlay type
C     OTABLE(2) == Scratch location (level of current overlay)
C     OTABLE(3) == Scratch
C     OTABLE(4) == Scratch
C     OTABLE(5...  Overlay name/entry address triplets
C

      EXTERNAL ABORT
      LOGICAL  EXPEL
      INTEGER  FINDSDT
      EXTERNAL FINDSDT
      INTEGER  I
      INTEGER  IFMT
      INTEGER  NUMARG
      INTEGER  OLDMSK
      INTEGER  OVLNO
      EXTERNAL REMARKF
      INTEGER  RNB
      EXTERNAL RNB
      INTEGER  SDT(0:0)
      INTEGER  SDTBASE
      POINTER (SDTPTR,SDT)
      INTEGER  SHIFTN
      INTEGER  SUCC
      INTEGER  TESTB00

      INTEGER  W, S, N, V
      INTEGER  GET, PUT

      GET(W,S,N)=AND(MASK(128-N),SHIFTR(W,64-S-N))
      PUT(W,S,N,V)=CSMG(SHIFTL(V,64-S-N),W,SHIFTR(MASK(N),S))


      IF (NUMARG() .EQ. 4) THEN
        IF (RECALL .EQ. 'RECALL'L) THEN
          EXPEL = .FALSE.
        ELSE
          EXPEL = .TRUE.
        ENDIF
      ELSE
        EXPEL = .TRUE.
      ENDIF

      IF (OTABLE(1) .EQ. 1) THEN
        IF (LEVEL.LT.1 .OR. LEVEL.GT.999) THEN
          ASSIGN 905 TO IFMT
          CALL REMARKF(IFMT)
          CALL ABORT
        ENDIF
        IF (NUMBER.LT.0 .OR. LEVEL.GT.999) THEN
          ASSIGN 906 TO IFMT
          CALL REMARKF(IFMT)
          CALL ABORT
        ENDIF
        OVLNO = 1000*LEVEL+NUMBER
      ELSE IF (OTABLE(1) .EQ. 2) THEN
        IF (LEVEL.LT.0 .OR. LEVEL.GT.10) THEN
          ASSIGN 902 TO IFMT
          CALL REMARKF(IFMT)
          CALL ABORT
        ENDIF
        IF (NUMBER .LT. 0 .OR. LEVEL .GT. 62) THEN
          ASSIGN 903 TO IFMT
          CALL REMARKF(IFMT)
          CALL ABORT
        ENDIF
        SHIFTN = (LEVEL-1)*6
        OLDMSK = MASK(128-SHIFTN)
        OVLNO  = OR(AND(OTABLE(2),OLDMSK),SHIFTL(NUMBER,SHIFTN))
        OTABLE(2) = OVLNO
      ELSE
        ASSIGN 901 TO IFMT
        CALL REMARKF(IFMT)
        CALL ABORT
      ENDIF
      I = 5
  100 CONTINUE
        IF (OTABLE(I) .EQ. 0) THEN
          ASSIGN 904 TO IFMT
          CALL REMARKF(IFMT,LEVEL,NUMBER)
          CALL ABORT
        ELSE IF (OTABLE(I) .EQ. OVLNO) THEN
          OVERZ000 = OTABLE(I+1)
          IF (EXPEL) THEN
            TESTB00  = OTABLE(I+2)
            SDTPTR   = FINDSDT(TESTB00,SDTBASE)
            IF (SDT(0) .EQ. 0) THEN
              ASSIGN 907 TO IFMT
              CALL REMARKF(IFMT)
              CALL ABORT
            ELSE
  140         CONTINUE
              IF (SDT(2) .LT. 0) THEN
*               The segment is resident.  We want to reset the
*               residence bit of it and of all of its successors.
* --------------- ASSIGN 908 TO IFMT
* --------------- CALL REMARKF(IFMT,RNB(SDT(0)))
                  SUCC = GET(SDT(3),32,16)
                  SDT(2) = PUT(SDT(2),0,1,0) ! Reset residence bit
                  IF (SUCC .EQ. 0) GO TO 180
                  SDT(3) = PUT(SDT(3),32,16,0)
                  SDTPTR = SUCC + SDTBASE
                  GO TO 140
  180           CONTINUE
              ENDIF
            ENDIF
          ENDIF
          RETURN
        ELSE
          I = I + 3
        ENDIF
      GO TO 100
  901 FORMAT('LD127 - OVERLAY TYPE FIELD DESTROYED')
  902 FORMAT('LD128 - INVALID LEVEL IN TYPE 2 OVERLAY')
  903 FORMAT('LD129 - INVALID NUMBER IN TYPE 2 OVERLAY')
  904 FORMAT('LD130 - OVERLAY (',I4,',',I4,') NOT FOUND')
  905 FORMAT('LD131 - INVALID LEVEL IN TYPE 1 OVERLAY')
  906 FORMAT('LD132 - INVALID NUMBER IN TYPE 1 OVERLAY')
  907 FORMAT('LD134 - NO SEGMENT FOUND FOR OVERLAY')
*-908 FORMAT('LD135 - EXPELLING SEGMENT ',A8)
CDIR$ ID "@(#) libu/util/c1/overz000.f	92.0	10/08/98 14:57:41"
      END
