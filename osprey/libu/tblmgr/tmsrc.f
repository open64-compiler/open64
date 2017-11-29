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


      INTEGER FUNCTION TMSRC(TABNUM, SARG, NWORDS, OFFSET, MSK)
      IMPLICIT INTEGER (A-Z)
C
CDIR$ ID "@(#) libu/tblmgr/tmsrc.f	92.0	10/08/98 14:57:41"
C
C     TMSRC   Search a table, using an optional mask, to locate a
C             specific field within an entry, using an optional offset
C             into the table.
C
C     Call:   INDEX = TMSRC(TABNUM, SARG, NWORDS, OFFSET, MSK)
C
C     Entry:  TABNUM contains the table number.
C             SARG contains the search argument.
C             (optional) NWORDS contains the number of words per entry
C                        group.
C             (optional) OFFSET contains the offset into the table.
C             (optional) MSK contains the mask defining a field.
C
C     Exit:   INDEX contains the zero-based index of the word in the
C             table, if found; else -1 if no match is found.

      INCLUDE 'tblmgr.fh'

C     Validate and initialize arguments.

      NMARG = NUMARG()

      IF ((NMARG .LT. 2) .OR. (NMARG .GT. 5)) THEN
        CALL TMERR('TMSRC', FETBARGS)
      ENDIF

      SNWORDS = 1
      SOFFSET = 0
      SMSK    = MASK(64)

      IF (NMARG .GE. 3) THEN
        SNWORDS = NWORDS
        IF (NMARG .GE. 4) THEN
          SOFFSET = OFFSET
          IF (NMARG .GE. 5) THEN
            SMSK = MSK
          ENDIF
        ENDIF
      ENDIF

C     Validate NTAB and TABNUM.

      IF ((NTAB .LT. 1) .OR. (NTAB .GT. 64)) THEN
        CALL TMERR('TMSRC', FETBNTAB)
      ENDIF

      IF ((TABNUM .LT. 1) .OR. (TABNUM .GT. NTAB)) THEN
        CALL TMERR('TMSRC', FETBTNUM)
      ENDIF

C     Call TMMSC to find argument.

      TMSRC = TMMSC(TABNUM, SMSK, SARG, SNWORDS, SOFFSET)

      RETURN
      END
