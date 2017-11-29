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


      SUBROUTINE TMINIT
      IMPLICIT INTEGER (A-Z)
C
CDIR$ ID "@(#) libu/tblmgr/tminit.f	92.0	10/08/98 14:57:41"
C
C     TMINIT  Initialize managed tables
C
C     Call:   CALL TMINIT
C
C     Entry:  BTAB array contains the desired interspace value
C             for the corresponding table.
C
C     Exit:   BTAB contains the base address for the corresponding table.
C             LTAB is zeroed for all tables.  
C             PAD contains the interspace value for the corresponding table.
C             ALEN contains the actual allocated length for the
C             corresponding table.

      EXTERNAL HPALLOC

      INCLUDE 'tblmgr.fh'

C     Validate number of arguments and NTAB.

      IF (NUMARG() .NE. 0) THEN
        CALL TMERR('TMINIT', FETBARGS)
      ENDIF

      IF ((NTAB .LT. 1) .OR. (NTAB .GT. 64)) THEN
        CALL TMERR('TMINIT', FETBNTAB)
      ENDIF

C     Reset statistics to zero.

      HWM = 0
      NTM = 0
      NWM = 0

C     Allocate space for each of the tables.

      DO I = NTAB, 1, -1

        PAD(I) = BTAB(I)

        IF (PAD(I) .NE. 0) THEN
          CALL HPALLOC(BTAB(I), PAD(I), STATUS, 1)
        ELSE
          BTAB(I) = 0
        ENDIF

        ALEN(I) = PAD(I)
        LTAB(I) = 0

      ENDDO

      RETURN
      END
