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


      SUBROUTINE TMPTC
      IMPLICIT INTEGER (A-Z)
C
CDIR$ ID "@(#) libu/tblmgr/tmptc.f	92.0	10/08/98 14:57:41"
C
C     TMPTC   Process table collisions
C
C     Call:   CALL TMPTC
C
C     Entry:  None.
C
C     Exit:   All managed tables have been checked to make sure that the
C             current size of the pad area is not less than zero or
C             greater than the specified PAD.  Any allocated areas that
C             were in violation of this have been adjusted so that they
C             are LTAB plus PAD words in length for the specified table.

      EXTERNAL HPDEALLC, HPNEWLEN, HPALLOC, HPCLMOVE

      INCLUDE 'tblmgr.fh'

C     Analyze each table for pad violations.

      DO I = NTAB, 1, -1
        ACTPAD = ALEN(I) - LTAB(I)                   ! get actual pad
        IF (ACTPAD .GT. PAD(I)) THEN                

C     Deallocate space.
        
          IF ((PAD(I) .EQ. 0) .AND.                  ! deallocate table
     +       (LTAB(I) .EQ. 0)) THEN
            CALL HPDEALLC(BTAB(I), STATUS, 1)
            BTAB(I) = 0
            NEWLEN = 0
          ELSE                                       ! shrink table
            IF (PAD(I) .EQ. 0) THEN                  ! give a pad of 1
              NEWLEN = LTAB(I) + 1
            ELSE
              NEWLEN = LTAB(I) + PAD(I)
            ENDIF
            CALL HPNEWLEN(BTAB(I), NEWLEN, STATUS, 1)
          ENDIF

C     Set new allocated length for tables.
        
          ALEN(I) = NEWLEN
        ELSEIF (ACTPAD .LT. 0) THEN

C     Allocate space.
        
          IF (BTAB(I) .EQ. 0) THEN                   ! create table
            NEWLEN = LTAB(I) + PAD(I)
            CALL HPALLOC(BTAB(I), NEWLEN, STATUS, 1)
          ELSE                                       ! extend table
            IF (PAD(I) .EQ. 0) THEN                  ! give a pad of 1
              NEWLEN = LTAB(I) + 1
            ELSE
              NEWLEN = LTAB(I) + PAD(I)
            ENDIF
            CALL HPCLMOVE(BTAB(I), NEWLEN, STATUS, 1)
            IF (STATUS .EQ. 1) THEN                  ! table had to be moved
              NTM = NTM + 1
              NWM = NWM + ALEN(I)
            ENDIF
          ENDIF

C     Set new allocated length for tables.
        
          ALEN(I) = NEWLEN
        ENDIF
      ENDDO
     
C     Calculate total allocated length.
        
      LENGTH = 0 

      DO I = 1, NTAB
        LENGTH = LENGTH + LTAB(I)
      ENDDO
 
C     Update high-water mark, if necessary.

      IF (LENGTH .GT. HWM) THEN
        HWM = LENGTH
      ENDIF

      RETURN
      END
