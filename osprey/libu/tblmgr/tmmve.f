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


      SUBROUTINE TMMVE(PSRCE, PDEST, COUNT)
      IMPLICIT INTEGER (A-Z)
C
CDIR$ ID "@(#) libu/tblmgr/tmmve.f	92.0	10/08/98 14:57:41"
C
C     TMMVE   Copy (table) memory
C
C     Call:  CALL TMMVE(PSRCE, PDEST, COUNT)
C
C     Entry:  PSRCE is a pointer to the source address for copy.
C             PDEST is a pointer to the destination address for copy.
C             COUNT contains the number of words to copy.
C
C     Exit:   COUNT words have been copied from PSRCE to PDEST.  If
C             PSRCE and PDEST overlap, the copy will be done in such a
C             way that PDEST is completely valid.  Note that this is
C             actually a copy, not a move routine.  No change is made
C             to the PSRCE memory unless it overlaps the PDEST memory.

      INCLUDE 'tblmgr.fh'

      POINTER (PSRCE, SOURCE(COUNT))
      POINTER (PDEST, DEST(COUNT))

C     Validate number of arguments and NTAB.

      IF (NUMARG() .NE. 3) THEN
        CALL TMERR('TMMVE', FETBARGS)
      ENDIF

      IF ((NTAB .LT. 1) .OR. (NTAB .GT. 64)) THEN
        CALL TMERR('TMMVE', FETBNTAB)
      ENDIF

      IF (PSRCE .LT. PDEST) THEN

C       Copying to higher memory, so copy from end to beginning,
C       so that copy is valid.

        DO I = COUNT, 1, -1
          DEST(I) = SOURCE(I)
        ENDDO

      ELSEIF (PSRCE .GT. PDEST) THEN

C       Copying to lower memory, so copy from beginning to end,
C       so that copy is valid.

        DO I = 1, COUNT
          DEST(I) = SOURCE(I)
        ENDDO

      ENDIF

      RETURN
      END
