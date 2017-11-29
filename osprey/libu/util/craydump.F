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

      SUBROUTINE CRAYDUMP (FWORD, LWORD, UNIT)
C
C NAME
C     CRAYDUMP - Dump memory
C
C SYNOPSIS
C     DIMENSION ARRAY(1000)
C     CALL CRAYDUMP(ARRAY(10), ARRAY(812), 101)
C
C IMPLEMENTATION
C     All Cray Research systems
C
C DESCRIPTION
C     CRAYDUMP is a Fortran-callable routine to dump arrays of memory.
C     Four words of memory are displayed per line of output.  Each word
C     is displayed in octal (hexadecimal on Cray MPP systems) and ASCII.
C     No line will exceed 132 characters in length.
C
C     The first and second arguments are the first and last words of
C     memory to be dumped.  If the last word address is greater than the
C     first word address, no memory is dumped.  The third argument is
C     the Fortran unit number to which the dump is to be written.  The
C     unit should be 101 (stdout) or any unit which is (or can be)
C     opened for sequential, formatted output.
C
C NOTES
C     On Cray PVP systems, memory dumps are formatted in octal.  On Cray
C     MPP systems, memory dumps are formatted in hexadecimal.
C
C CAUTIONS
C     On Cray MPP systems, care should be taken such that only contiguous
C     regions of memory should be dumped.  That is, only memory with a single
C     array or common block.
C
      IMPLICIT NONE
      CHARACTER * 132, LINE
#ifndef	_CRAYMPP
      CHARACTER * 23, NUMERIC(4)
#else
      CHARACTER * 17, NUMERIC(4)
#endif
      CHARACTER * 16, FMT3
      CHARACTER * 20, FMT4
      CHARACTER * 8, ASCII(4), FMT1, FMT2
      INTEGER FACTR, LAB
#ifndef	_CRAYMPP
      PARAMETER (FACTR = 0)
      PARAMETER (LAB   = 3 * 5)
#else
      PARAMETER (FACTR = 3)
      PARAMETER (LAB   = 4 * 5)
#endif
      INTEGER FWORD, LWORD, UNIT, IUNIT
      INTEGER I, J, K
      INTEGER MEMORY, MEM1, MEM2, OLD(4), OLDER(4)
      INTEGER COUNT, FADDR, LADDR, SLIP, SLOP
      INTEGER LOC, LOGB2, NUMARG
      LOGICAL DUP, REPEAT
      POINTER (PTR, MEMORY(4))
      POINTER (PTR1, MEM1(*))
      POINTER (PTR2, MEM2(*))
      LOGB2(I) = 63 - LEADZ(I)    !  Log(base 2) of I
C
C     Validate arguments
C
      IF (NUMARG() .LT. 2) RETURN
C
      IF (NUMARG() .LT. 3) THEN
        IUNIT = 101
      ELSE
        IUNIT = UNIT
      ENDIF
C
      REPEAT = .FALSE.
#ifndef	_CRAYMPP
      FMT1   = '(O23.23)'
      FMT2   = '(O23.22)'
      FMT3   = '(50X,''- same -'')'
      FMT4   = '(X,O5.5,'':'',4A,X,4A)'
#else
      FMT1   = '(Z17.17)'
      FMT2   = '(Z17.16)'
      FMT3   = '(38X,''- same -'')'
      FMT4   = '(X,Z5.5,'':'',4A,X,4A)'
#endif
C
C     Set the starting addresses for the area to be dumped
C
      REPEAT = .FALSE.
      PTR1   = LOC(FWORD)
      PTR2   = LOC(LWORD)
C
C     Print the header line for the dump
C
      WRITE (NUMERIC(1), FMT1) PTR1
      WRITE (NUMERIC(2), FMT1) PTR2
      J = 1
      DO WHILE (NUMERIC(1)(J:J) .EQ. '0')
        J = J + 1
      ENDDO
      K = 1
      DO WHILE (NUMERIC(2)(K:K) .EQ. '0')
        K = K + 1
      ENDDO
C
#ifdef	_CRAYMPP
      J = J - 1
      K = K - 1
      NUMERIC(1)(J:J) = 'x'
      NUMERIC(2)(K:K) = 'x'
#endif
C
      WRITE (IUNIT, 1000) NUMERIC(1)(J-1:), NUMERIC(2)(K-1:)
 1000 FORMAT(X,'Dump of user memory, address ',A,' through ',A,':')
C
      DO I = 1, 4
        NUMERIC(I) = ' '
        ASCII(I)   = ' '
      ENDDO
C
C     Align addresses to multiple of 4, compensating for word or byte
C     addressing.
C
      SLIP  = 1
      SLOP  = 4
C
      PTR    = PTR1
      FADDR  = PTR
      DO WHILE (MOD(FADDR, ISHFT(4, FACTR)) .NE. 0)
        PTR   = PTR - 1
        FADDR = PTR
        SLIP  = SLIP + 1
      ENDDO
C
      LADDR = PTR2
      COUNT = ISHFT((LADDR - FADDR), -FACTR)
      IF (COUNT .LT. 0) RETURN
C
      OLD(1)   = XOR(MEMORY(1), -1)
      OLDER(4) = XOR(MEMORY(4), -1)
C
C     Loop from the first to the last memory location
C
      DO I = 0, COUNT, 4
C
        DUP = .TRUE.
C
        IF ((I + 3) .GE. COUNT) THEN    !  If last trip
          OLD(1) = XOR(MEMORY(1), -1)
          SLOP   = 1 + ISHFT((LADDR - FADDR), -FACTR)
          DO J = SLOP, 4
            NUMERIC(J) = ' '
            ASCII(J)   = ' '
          ENDDO
        ENDIF
C
        DO J = SLIP, SLOP
          WRITE (NUMERIC(J), FMT2) MEMORY(J)
          WRITE (ASCII(J), '(A8)') MEMORY(J)
        ENDDO
C
C       Remove non-printing characters
C
        DO J = 1, 4
          DO K = 1, 8
            IF (ASCII(J)(K:K) .LT. ' ' .OR. ASCII(J)(K:K) .GT. '~') THEN
              ASCII(J)(K:K) = ' '
            ENDIF
          ENDDO
          DUP  = DUP .AND.
     X           (MEMORY(J) .EQ. OLD(J) .AND. OLD(J) .EQ. OLDER(J))
        ENDDO
C
        IF (DUP) THEN
          IF (.NOT. REPEAT) THEN
            WRITE (IUNIT, FMT3)
            REPEAT = .TRUE.
          ENDIF
        ELSE
          WRITE (IUNIT, FMT4) IBITS(FADDR, 0, LAB), NUMERIC, ASCII
          REPEAT = .FALSE.
        ENDIF
C
	OLDER = OLD
	OLD   = MEMORY
        PTR   = PTR + 4
        FADDR = PTR
        SLIP  = 1
C
      ENDDO
C
      RETURN
CDIR$ ID "@(#) libu/util/craydump.F	92.0	10/08/98 14:57:41"
      END
