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


       SUBROUTINE WCLOSE(FIL,IERR)
CDIR$ ID "@(#) libu/waio/wclose.f	92.0	10/08/98 14:57:41"

CC     WCLOSE  -  CLOSE WORD ADDRESSABLE FILE.
C
C      ENTRY   -  *FIL*  = FILE NUMBER OR ASCII NAME TO CLOSE.
C
C      EXIT    -  *IERR* =   0 IF NO ERRORS FOUND.
C                        = INFO ONLY  CLOSE ON FILE ALREADY CLOSED.
C                        =  -1 IF INVALID UNIT NUMBER.
C                        =  -6 IF INVALID ASCII NAME.
C
C      CALLS   -  *WFLUSH*  TO FLUSH BUFFERS IN WRITE MODE TO DISK.
C
C                 *WDSET*   TO CONVERT USER NAME TO SYSTEM FILE NAME.
C
C                 *WDSETB*  TO FIND BUFFER ADDRESS IN CASE OF MOVE.
C
C                 *WUNIT*   TO WAIT FOR I/O COMPLETION.
C
C                 *CLOSEWA* TO DO CLOSE MACRO AND FREE BUFFERS.
C
C      METHOD:
C        FIRST THE INPUT PARAMETER IS CONVERTED TO SYSTEM FILE NAME BY
C      A CALL TO *WDSET*. IF ERROR ON CONVERSION MESSAGE IS ISSUED AND
C      WE THEN RETURN.  THEN THE *FET* TABLE IS SEARCHED FOR A MATCH
C      TO  *FIL*.  IF NONE IS FOUND ISSUE ERROR MESSAGE AND RETURN. IF
C      FOUND WE WAIT FOR I/O QUIET, FLUSH THE BUFFERS, CLOSE THE FILE,
C      AND IF STATISTICS WERE REQUESTED ON THE OPEN CALL WE PRINT OUT
C      THE STATS ON *$STATS*.  THE ENTRY FOR THIS FILE IS THEN ZEROED
C      OUT AND CONTROL RETURNED TO USER.
C
C      FLOW:
C
C      1.  CALL *WDSET* TO CONVERT FILE NUMBER. IF ERROR FOUND ISSUE
C          ERROR MESSAGE, SET ERROR CODE AND IF ARGUMENT COUNT OK
C          RETURN.  IF ARGUMENT COUNT NOT OK AND ERROR FOUND - ABORT.
C
C      2.  SEARCH THE *FARRAY* ARRAY FOR MATCHING FILE ENTRY.  IF NOT
C          FOUND ISSUE ERROR MSG AND RETURN.
C
C      3.  IF FOUND SET BUFFER COUNT AND WORD ADDRESS ARRAY POINTERS.
C
C      4.  IF I/O ACTIVE CALL *WUNIT* TO WAIT FOR COMPLETION.
C
C      5.  CALL *WFLUSH* TO FLUSH OUT ANY BUFFERS IN WRITE MODE TO DISK.
C
C      6.  CALL *CLOSEWA* TO CLOSE THE FILE.
C
C      7.  IF STATS FLAG SET IN *FET* TABLE THEN DUMP THE FILE STATS
C          TO *$STATS* FILE.
C
C      8.  PRINT MESSAGE TO INDICATE FILE CLOSED AND CALL THE MEMORY
C          SQUEEZE ROUTINE TO RELEASE MEMORY.
C
C      9.  RETURN.
C
CDIR$  EJECT
       IMPLICIT INTEGER (A-Z)

       INCLUDE "wavars.fh"

       REAL AVAT,TOTA,BONUS

       logical long

       INTEGER I@CLKTCK

C      FIRST FIND FILE INDEX IN FARRAY TABLE

       FILE  = FIL
       IF(NUMARG().EQ.2) THEN
         IERR = 0
       ENDIF
       IERR1 = 0
       CALL WDSET(FILE,IDN)
       IF(IDN.EQ.0)
     +   THEN
           WRITE (102,*) 'WCLOSE - INVALID UNIT NUMBER'
           IF(NUMARG().LT.2) CALL ABORT
           IERR = -1
           RETURN
         ENDIF
       IF(IDN.EQ.1)
     +   THEN
           WRITE (102,*) 'WCLOSE - INVALID ASCII FILE NAME  '
           IF(NUMARG().LT.2) CALL ABORT
           IERR = -6
           RETURN
         ENDIF

       INDEX = 0
       DO 85 I = 1,G@DSNMAXW
         IF(FET(1,I) .EQ. IDN)
     +     THEN
             INDEX = I
             FET(1,I) = 0
           ENDIF
85     CONTINUE
       FILE = IDN
       IF(INDEX.EQ.0)
     +   THEN
           ASSIGN 1000 TO MESS
           WRITE (102,MESS) FILE

C    INFORMATION ONLY MESSAGE

C          IF(NUMARG().LT.2) CALL ABORT
C          IERR  = 2
           RETURN
         ENDIF

       CALL WDSETB(FILE,INDEX,IADDR)
       FET(18,INDEX)=IADDR
       WABB    = IADDR
       BUFFERS = SHIFTR(FET(2,INDEX),32).A.MASK(128-28)
       PBLKS   = BUFFERS/BSIZE+1
       WABA    = WABB+PBLKS*BSIZE
       IF(FET(2,INDEX).LT.0)
     +   THEN
           IT1 = IRTC()
           CALL WUNIT(INDEX)
           FET(10,INDEX) = FET(10,INDEX) + (IRTC()-IT1)
           FET(2,INDEX) = FET(2,INDEX).AND.MASK(128-63)
         ENDIF

       EOIBW = FET(16,INDEX)
       CALL WFLUSH(1,BUFFERS)
       CALL CLOSEWA(INDEX)
       IF(SHIFTL(FET(2,INDEX),1).LT.0)
     +   THEN
           MSK1    = MASK(128-32)
           TOTA    = FET(10,INDEX) / FLOAT(I@CLKTCK())	! divide by clock rate
           MISSES  = SHIFTR(FET(3,INDEX),32).AND.MSK1
           PUTS    = FET(3,INDEX).AND.MSK1
           HITS    = SHIFTR(FET(4,INDEX),32).AND.MSK1
           GETS    = FET(4,INDEX).AND.MSK1
           APUTS   = FET(15,INDEX).AND.MASK(128-24)
           APTOVF  = SHIFTR(FET(15,INDEX),24).AND.MASK(128-20)
           ASKOVF  = SHIFTR(FET(15,INDEX),44)


           PARHITS = SHIFTR(FET(5,INDEX),32).AND.MSK1
           WRITEWA = FET(5,INDEX).AND.MSK1
           FLUSHES = SHIFTR(FET(6,INDEX),32).AND.MSK1
           READWA  = FET(6,INDEX).AND.MSK1
           EOIADD  = SHIFTR(FET(7,INDEX),36).AND.MASK(128-28)
           FINDS   = SHIFTR(FET(7,INDEX),12).AND.MASK(128-24)
           ITOTW   = FET(8,INDEX) + FET(9,INDEX)
           IDISKW  = FET(13,INDEX) + FET(14,INDEX)

           AVDR    = 0
           AVDW    = 0
           BONUS   = 0.0
           IF(READWA.NE.0)  AVDR    = FET(13,INDEX)/READWA
           IF(WRITEWA.NE.0) AVDW    = FET(14,INDEX)/WRITEWA
           IF(IDISKW.NE.0)  BONUS   = (ITOTW * 100)/IDISKW

           AVAT    = 0.0
           ACCESS  = READWA + WRITEWA
           IF(ACCESS.NE.0) AVAT = TOTA/(ACCESS)*1000.0 

           WRITE(102,8000) FILE, BUFFERS, GETS + PUTS
           WRITE(102,8001) GETS, PUTS, FINDS
           WRITE(102,8008) APUTS, APTOVF, ASKOVF
           WRITE(102,8002) HITS, MISSES, PARHITS
           WRITE(102,8004) FET(8,INDEX), FET(9,INDEX), ITOTW
           WRITE(102,8003) READWA, WRITEWA, FLUSHES
           WRITE(102,8006) FET(13,INDEX), FET(14,INDEX), IDISKW
           WRITE(102,8007) AVDR, AVDW, BONUS
           WRITE(102,8005) TOTA, AVAT, EOIADD
         ENDIF

C      ASSIGN 1010 TO MESS
C      WRITE (102,MESS) RNB(FILE)
       RETURN


8000  FORMAT(1H1,' SUMMARY FOR W.A. DATASET     ',A8,
     2 '  BUFFERS USED     =  ',I12,'   TOTAL ACCESSES   =  ',I12)
8001  FORMAT(1H0,'           GETS       =   ',I12,
     2 '  PUTS             =  ',I12,'   FINDS            =  ',I12)
8002  FORMAT(1H0,'  WFIND SUMMARY: HITS =   ',I12,
     2 '  MISSES           =  ',I12,'   PARTIAL HITS     =  ',I12)
8003  FORMAT(1H0,'  DISK READS          =   ',I12,
     2 '  DISK WRITES      =  ',I12,'   BUFFER FLUSHES   =  ',I12)
8004  FORMAT(1H0,'  WORDS READ          =   ',I12,
     2 '  WORDS WRITTEN    =  ',I12,'   TOTAL WORDS      =  ',I12)
8005  FORMAT(1H0,'  TOTAL ACCESS TIME   =   ',F12.3,
     2 '  AVER ACCESS TIME =  ',F12.3,'   EOI BLOCK NUMB.  =  ',I12,
     3         /,'   IN SECONDS.                        ',
     4 '   IN MILLISECONDS.    ')
8006  FORMAT(1H0,'  DISK WORDS READ     =   ',I12,
     2 '  DISK WDS WRITTEN =  ',I12,'   TOTAL DISK XFERS =  ',I12)
8007  FORMAT(1H0,'  AVER PHYS DISK READ =   ',I12,
     2 '  AV PHYS DISK WRIT=  ',I12,'   BUFFER BONUS %   =  ',F12.2)
8008  FORMAT(1H0,'  APUTWA CALLS        =   ',I12,
     2 '  APUTWA OVERFLOWS =  ',I12,'   SEEK OVERFLOWS   =  ',I12)

1000   FORMAT('WARNING - TRYING TO CLOSE UNOPENED FILE = ',A8)
1010   FORMAT('W.A. FILE CLOSED. FILE = ',A7)

CDIR$  EJECT
       ENTRY WOPEN(FIL,BLOCKS,STATS,IERR)

CC     WOPEN  -  OPEN WORD ADDRESSABLE FILE.
C
C      ENTRY  -   *FIL*   =  FILE NUMBER OR ASCII CHARACTER STRING.
C
C               *BLOCKS*  =  SIZE OF BUFFER TO USE FOR THIS DATASET.
C                            MUST BE .GT. 1 AND .LT. BUFFMAX. DEFAULT
C                            IS 16 BUFFERS.
C
C                *STATS*  =  STATISTICS FLAG.  IF *STATS* EQUAL 1
C                            THEN STATISTICS WILL BE PRODUCED ON $STATS
C                            WHEN THIS FILE IS CLOSED.
C
C                 *IERR*  =  ERROR FLAG FOR RETURN OF ERROR TO CALLER.
C
C
C      EXIT -   *IERR*  =  0  IF NO ERRORS FOUND.
C                       = -1  IF INVALID UNIT NUMBER DETECTED.
C                       = -2  IF MAXIMUM NUMBER OF FILES EXCEEDED.
C                       = -4  IF WORD ADDRESS .LE. 0.
C                       = -5  IF WORD COUNT .GT. MAX RECORD.
C                       = -6  IF INVALID ASCII FILE NAME FOUND.
C                       = -7  IF WORD COUNT .LE. 0.
C                    WARNING  IF CALL TO FILE ALREADY OPEN.
C                    WARNING  IF BLOCKS REQUESTED .GT. MAX.
C                    WARNING  IF BLOCKS REQUESTED .LE. 1.
C
C      CALLS  -   *WINIT*  TO OPEN THE FILE.
C
C                 *WDSET*  TO CONVERT THE FILE NAME/NUMBER.
C
C                  *SEEK*  TO LOAD THE BUFFERS IF FILE EXISTS.
C
C      METHOD:
C
C        WOPEN FIRST CHECKS FILE NUMBER TO SEE IF IT IS LEGAL. IF FILE
C      NUMBER IS OK IT THEN CHECKS TO SEE IF REQUESTED NUMBER OF BLOCKS
C      IS WITHIN RANGE.  A CALL IS THEN MADE TO *WINIT* TO INITIALIZE
C      THE FILE, AND THE STATISTICS FLAG IS SAVED IN THE FET TABLE. IF
C      FILE EXISTS *SEEK* IS CALLED TO LOAD THE BUFFERS, AND CONTROL IS
C      IS RETURNED TO THE USER.
C
C      FLOW:
C
C      1.  CHECK FILE NAME/NUMBER IF ERROR ISSUE MESSAGE AND IF THE
C          NUMBER OF ARGUMENTS IS OK RETURN WITH ERROR CODE.
C          IF ERROR AND ARGUMENT COUNT BAD ABORT.
C
C      2.  CHECK BLOCK COUNT. IF .GT. BUFFMAX OR .LT. 1 ISSUE MSG SET
C          BLOCK COUNT TO DEFAULT. (PAGES PARAMETER FROM COMMON BLK)
C
C      3.  CALL *WINIT* TO INITIALIZE THE FILE.
C
C      4.  IF ERROR IN *WINIT* RETURN IF NUMBER OF ARGUMENTS OK.
C          IF ERROR IN *WINIT* AND NUMBER OF ARGS NOT OK - ABORT.
C
C      5.  SAVE STATISTICS OPTION.
C
C      6.  IF FILE EXISTS CALL *SEEK* TO LOAD THE BUFFERS.
C
C      7.  RETURN.
C

       long = .false.
C
       go to 2
       entry g@lwopen(fil,lname,blocks,stats,ierr,aifound,aip,isysfd)
       long = .true.
    2  continue
C
       BUFFS = PAGES
       FILE  = FIL
       WRITE = .FALSE.
       IERR1 = 0
       CALL WDSET(FILE,IDN)
       IF(IDN.EQ.0)
     +   THEN
           WRITE (102,*) 'WOPEN - INVALID UNIT NUMBER  '
           IF(NUMARG().LT.4) CALL ABORT
           IERR = -1
           RETURN
         ENDIF
       IF(IDN.EQ.1)
     +   THEN
           WRITE (102,*) 'WOPEN - INVALID ASCII FILE NAME  '
           IF(NUMARG().LT.4) CALL ABORT
           IERR = -6
           RETURN
         ENDIF

       FILE    = IDN
       COUNT   = 10
       ADDRESS = 1
       READ    = .TRUE.
       IF(BLOCKS.GT.BUFFMAX)
     +   THEN
           ASSIGN 1001 TO MESS
           WRITE (102,MESS)
           ASSIGN 1002 TO MESS
           WRITE (102,MESS) BLOCKS, FILE
           BUFFS  = BUFFMAX
         ELSE
           IF(BLOCKS.LE.1)
     +       THEN
               ASSIGN 1003 TO MESS
               WRITE (102,MESS)
               ASSIGN 1004 TO MESS
               WRITE (102,MESS) BLOCKS, FILE
               BUFFS = 2
             ELSE
               BUFFS  = BLOCKS
             ENDIF
         ENDIF
       FILE    = FIL
C
       if ( long ) then
         call g@lwinit(lname,ierr1,buffs,0,STATS,aifound,aip,isysfd)
       else
         CALL WINIT(IERR1,BUFFS,0,STATS)
       endif
C
       IF(NUMARG().GE.4) IERR = IERR1
       IF(IERR1.LT.0)
     +   THEN
           IF(NUMARG().LT.4)
     +       THEN
               CALL ABORT
             ELSE
               RETURN
             ENDIF
         ENDIF
C
C if stats is zero, turn off stats. Bit was set by WINIT. 
C
      IF(STATS.EQ.0) THEN   
         FET(2,INDEX)=FET(2,INDEX).AND.(COMPL(SHIFTR(MASK(1),1)))  
      ENDIF                                 
       IF(EOIB.GT.0) CALL SEEK(FIL,1,BSIZE*MIN0(EOIB,BUFFERS),IERRO)
       RETURN

C      ERRORS.

1001   FORMAT('WARNING - BUFFERS REQUESTED .GT. MAXIMUM, DEFAULT USED')
1002   FORMAT(' BLOCKS = ',I20,' FILE = ',A8)
1003   FORMAT('WARNING - BUFFERS REQUESTED .LT. MINIMUM, DEFAULT USED')
1004   FORMAT(' BLOCKS = ',I4,' FILE = ',A8)
       END
