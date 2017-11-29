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


!DIR$ ID "@(#) libu/multi/posix/mactlstf.f	92.0	10/08/98 14:57:41"

!DIR$ FREE


!========================================
!     _MACT_DO_TSKLIST
!
!     This routine reports the status of all active macrotasks.  It
!     must be compiled by the CraySoft Fortran-90 compiler, so that
!     the output it produces is properly synchronized with any other
!     output produced by the program.
!
!     This routine is derived from the original TSKLIST from the PVP
!     environment.
!
!     This source file must be compiled with the f90 -eQ switch, to
!     allow identifiers with leading underbars to be acceptable.  This
!     means that the external symbol for _MACT_DO_TSKLIST, in C terms,
!     is "_MACT_DO_TSKLIST".
!
      SUBROUTINE _MACT_DO_TSKLIST(NUM_TASKS,                      &
                                  MTIDS, STATES, STIDS, UTIDS,    &
                                  WAIT_OBJS, WAIT_COUNTS          &
                                 )
      INTEGER               NUM_TASKS
      INTEGER               MTIDS      (NUM_TASKS)
      INTEGER               STATES     (NUM_TASKS)
      INTEGER               STIDS      (NUM_TASKS)
      INTEGER               UTIDS      (NUM_TASKS)
      INTEGER               WAIT_OBJS  (NUM_TASKS)
      INTEGER               WAIT_COUNTS(NUM_TASKS)

      INTEGER, PARAMETER :: TS_NONEXISTENT = 0
      INTEGER, PARAMETER :: TS_READY       = 1
      INTEGER, PARAMETER :: TS_RUNNING     = 2
      INTEGER, PARAMETER :: TS_WAITLOCK    = 3
      INTEGER, PARAMETER :: TS_WAITEV      = 4
      INTEGER, PARAMETER :: TS_WAITBAR     = 5
      INTEGER, PARAMETER :: TS_WAITTASK    = 6
      INTEGER, PARAMETER :: TS_COMPLETE    = 7

      INTEGER               OUTFILE

 9000 FORMAT(//' STATUS OF EXISTING TASKS',                          &
             //' Internal  User defined task val',38X,'# tasks'      &
              /' task ID   Hexadecimal     ASCII    Task status',    &
               23X,'waiting'                                         &
              /' -------   ---------------------    -----------',    &
               23X,'-------'                                         &
            )
 9001 FORMAT(1X,I6,4X,Z8,9X,A4,4X,'ready to run',              23X,'      ')
 9002 FORMAT(1X,I6,4X,Z8,9X,A4,4X,'running on logical CPU ',I5, 6X,'      ')
 9003 FORMAT(1X,I6,4X,Z8,9X,A4,4X,'waiting for lock at ',Z8,    7X,I6)
 9004 FORMAT(1X,I6,4X,Z8,9X,A4,4X,'waiting for event at ',Z8,   6X,I6)
 9005 FORMAT(1X,I6,4X,Z8,9X,A4,4X,'waiting for barrier at ',Z8, 4X,I6)
 9006 FORMAT(1X,I6,4X,Z8,9X,A4,4X,'waiting for task ',I6,      12X,I6)
 9007 FORMAT(1X,I6,4X,Z8,9X,A4,4X,'complete',                  27X,'      ')
 9008 FORMAT(1X,I6,4X,Z8,9X,A4,4X,'unknown',                   28X,'      ')

      OUTFILE = 6

!
!     Write information about each task.
!
      WRITE(OUTFILE,9000)
      DO I = 1, NUM_TASKS
        SELECT CASE (STATES(I))
          CASE (TS_READY)
               WRITE(OUTFILE, 9001) MTIDS(I), UTIDS(I), FIX_UTID(UTIDS(I))

          CASE (TS_RUNNING)
               WRITE(OUTFILE, 9002) MTIDS(I), UTIDS(I), FIX_UTID(UTIDS(I)),   &
                                    WAIT_OBJS(I)

          CASE (TS_WAITLOCK)
               WRITE(OUTFILE, 9003) MTIDS(I), UTIDS(I), FIX_UTID(UTIDS(I)),   &
                                    WAIT_OBJS(I), WAIT_COUNTS(I)

          CASE (TS_WAITEV)
               WRITE(OUTFILE, 9004) MTIDS(I), UTIDS(I), FIX_UTID(UTIDS(I)),   &
                                    WAIT_OBJS(I), WAIT_COUNTS(I)

          CASE (TS_WAITBAR)
               WRITE(OUTFILE, 9005) MTIDS(I), UTIDS(I), FIX_UTID(UTIDS(I)),   &
                                    WAIT_OBJS(I), WAIT_COUNTS(I)

          CASE (TS_WAITTASK)
               WRITE(OUTFILE, 9006) MTIDS(I), UTIDS(I), FIX_UTID(UTIDS(I)),   &
                                    WAIT_OBJS(I), WAIT_COUNTS(I)

          CASE (TS_COMPLETE)
               WRITE(OUTFILE, 9007) MTIDS(I), UTIDS(I), FIX_UTID(UTIDS(I))

          CASE DEFAULT
               WRITE(OUTFILE, 9008) MTIDS(I), UTIDS(I), FIX_UTID(UTIDS(I))
        END SELECT
      ENDDO

      RETURN

!     ==============================================================
      CONTAINS

        CHARACTER(4) FUNCTION FIX_UTID(UTID)
        INTEGER      UTID
        CHARACTER(4) A_UTID
        INTEGER      I

        INTRINSIC    TRANSFER
        INTRINSIC    ICHAR

        A_UTID = TRANSFER(UTID, A_UTID)
        DO I = 1, 4
          IF (ICHAR(A_UTID(I:I)) .LE. 31) THEN
            A_UTID(I:I) = '.'
          ENDIF
        ENDDO

        FIX_UTID = A_UTID

        END FUNCTION FIX_UTID

      END SUBROUTINE _MACT_DO_TSKLIST
