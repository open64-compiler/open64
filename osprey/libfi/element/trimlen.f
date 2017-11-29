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


      INTEGER FUNCTION TRIMLEN(STRING)
      CHARACTER*(*) STRING

C     Return the length of a character string, excluding trailing blanks.
C     If the string contains no non-blank characters, then return 0 if
C     the length of the variable is 0, otherwise 1.   The value of 1 is
C     returned in those cases for the benefit of the cf77 compiler.
C     In Fortran 77, zero length character values were not allowed.
C
C     This function is very similar to Fortran 90's LEN_TRIM intrinsic 
C     function.

       
      IF(LEN(STRING) .EQ. 0) THEN
        TRIMLEN = 0
        RETURN
      ENDIF

      DO I = LEN(STRING),1,-1
        IF (STRING(I:I) .NE. ' ') THEN
          TRIMLEN  = I
          RETURN
        ENDIF
      ENDDO
      TRIMLEN  = 1                           ! Leave one blank for Fortran 77
      RETURN
CDIR$ ID "@(#) libfi/element/trimlen.f	92.0	10/08/98 14:37:14"
      END
