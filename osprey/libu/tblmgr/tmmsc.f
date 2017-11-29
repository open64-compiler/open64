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


      INTEGER FUNCTION TMMSC(TABNUM, MASK, SWORD, NWORD, OFFSET)
      IMPLICIT INTEGER (A-Z)
C
CDIR$ ID "@(#) libu/tblmgr/tmmsc.f	92.0	10/08/98 14:57:41"
C
C     TMMSC   Search a table using a mask to locate a specific field
C             within an entry
C
C     Call:   INDEX = TMMSC(TABNUM, MASK, SWORD, NWORD, OFFSET)
C
C     Entry:  TABNUM contains the table number.
C             MASK contains the mask defining a field.
C             SWORD contains the search word.
C             NWORD contains the number of words per entry group.
C             (optional) OFFSET contains the offset into the table.
C
C     Exit:   INDEX contains the zero-based index of the word in the
C             table, if found; else -1 if no match is found.

      INCLUDE 'tblmgr.fh'

      POINTER (PTR, TABLE(0:0))

C     Validate number of arguments, NTAB, and TABNUM.

      IF (NUMARG() .NE. 5) THEN
        IF (NUMARG() .EQ. 4) THEN
          MOFFSET = 0
        ELSE
          CALL TMERR('TMMSC', FETBARGS)
        ENDIF
      ELSE
        MOFFSET = OFFSET
      ENDIF

      IF ((NTAB .LT. 1) .OR. (NTAB .GT. 64)) THEN
        CALL TMERR('TMMSC', FETBNTAB)
      ENDIF

      IF ((TABNUM .LT. 1) .OR. (TABNUM .GT. NTAB)) THEN
        CALL TMERR('TMMSC', FETBTNUM)
      ENDIF

C     If the table length is zero, set INDEX to -1.

      IF (LTAB(TABNUM) .EQ. 0) THEN
        TMMSC = -1
      ELSE
        PTR   = BTAB(TABNUM)

        DO INDEX = MOFFSET, LTAB(TABNUM) - 1, NWORD
          IF (AND(MASK, TABLE(INDEX)) .EQ. SWORD) GOTO 10
        ENDDO

   10   CONTINUE

        IF (INDEX .GE. LTAB(TABNUM)) THEN
          TMMSC = -1
        ELSE
          TMMSC = INDEX
        ENDIF

      ENDIF

      RETURN
      END
