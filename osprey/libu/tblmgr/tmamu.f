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


      SUBROUTINE TMAMU(LENGTH, TABU, TABM, HWMARK, WORDSM)
      IMPLICIT INTEGER (A-Z)
C
CDIR$ ID "@(#) libu/tblmgr/tmamu.f	92.0	10/08/98 14:57:41"
C
C     TMAMU   Report table management statistics
C
C     Call:   CALL TMAMU(LENGTH, TABU, TABM, HWMARK, WORDSM)
C
C     Entry:  None of the parameters need to be initialized.
C
C     Exit:   LENGTH contains the total allocated length of the tables.
C             TABU contains the number of tables used.
C             TABM contains the number of tables moved.
C             HWMARK contains the high-water mark of the tables in memory.
C             WORDSM contains the number of words moved.

      INCLUDE 'tblmgr.fh'

      INTEGER LENGTH, TABU, TABM, HWMARK, WORDSM
      INTEGER I

C     Validate number of arguments and NTAB.

      IF (NUMARG() .NE. 5) THEN
        CALL TMERR('TMAMU', FETBARGS)
      ENDIF

      IF ((NTAB .LT. 1) .OR. (NTAB .GT. 64)) THEN
        CALL TMERR('TMAMU', FETBNTAB)
      ENDIF

C     Gather simple statistics.

      TABU = NTAB
      TABM = NTM
      HWMARK = HWM
      WORDSM = NWM

C     Calculate total allocated length.

      LENGTH = 0

      DO I = 1, NTAB
        LENGTH = LENGTH + LTAB(I)
      ENDDO

      RETURN
      END
