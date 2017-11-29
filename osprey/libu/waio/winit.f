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


      SUBROUTINE WINIT(IERR,BLOCKS,INIT,STATS)
CDIR$ ID "@(#) libu/waio/winit.f	92.0	10/08/98 14:57:41"

CC     WINIT -  WORD ADDRESSABLE FILE INITIALIZE.
C
C      ENTRY -  *IERR*  ERROR FLAG FOR RETURN OF ERROR. VALUE ON
C                       ENTRY UNIMPORTANT.
C
C             *BLOCKS*  NUMBER OF BLOCKS TO RESERVE FOR BUFFER SPACE.
C
C               *INIT*  INITALIZE CHECK FLAG.  IF 0 *WINIT* WILL CHECK
C                       TO BE SURE THE FILE IS NOT ALREADY OPEN.  IF
C                       NON ZERO NO CHECK IS MADE.
C
C              *STATS*  non-zero means that an informative message
C                       will be put out onto stderr
C
C      EXIT -   *IERR*  =  0  IF NO ERRORS FOUND.
C                       = -1  IF INVALID UNIT NUMBER DETECTED.
C                       = -2  IF MAXIMUM NUMBER OF FILES EXCEEDED.
C                       = -3  IF WORD ADDRESS .LE. 0.
C                       = -5  IF WORD COUNT .GT. MAX RECORD.
C                       = -6  IF INVALID ASCII FILE NAME FOUND.
C                       = -7  IF WORD COUNT .LE. 0.
C                       = -8  IF NUMBER OF 512-WORD BLOCKS .GT. BUFFMAX
C                    WARNING  IF INITIAL CALL TO FILE ALREADY OPEN.
C
C      CALLS -   *OPENWA*  TO DO SYSTEM OPEN WORD ADDRESSABLE FILE.
C
C                *WDSET*   TO CONVERT USER NAME/NUMBER TO ASCII NAME.
C
C                *WUNIT*   TO WAIT FOR I/O COMPLETION ON FILE.
C
C                *WDSETB*  TO FIND BUFFER ADDRESS OF FILE IN CASE IT
C                          HAS BEEN MOVED BY THE SYSTEM.
C
C                *EXIT*    IF FATAL ERROR FOUND.
C
C
C
C      METHOD:
C
C        WINIT CHECKS THE PARAMETERS (STORED IN COMMON) TO SEE IF
C      THEY ARE CORRECT. IF ALL IS WELL IT THEN SEARCHS THE FILE
C      ARRAY (*FET*) TO SEE IF THE REQUESTED UNIT NUMBER/NAME EXISTS.
C      IF UNIT NUMBER IS NOT THERE THE FILE HAS NOT BEEN OPENED AND
C      *OPENWA* IS CALLED TO DO SYSTEM OPEN AND ALLOCATE A BUFFER BY
C      OPENING A DUMMY BLOCKED FILE AND USING THE BUFFER ASSIGNED TO
C      IT.  WINIT THEN CALCULATES BLOCK COUNT, FIRST BLOCK NUMBER,LAST
C      BLOCK NUMBER, AND OTHER INTERESTING CONSTANTS FOR THIS CALL.
C      THE LAST THING  DONE IS TO  CHECK IF I/O IS ACTIVE ON THIS FILE
C      (*SEEK* CALL WAS DONE).  IF SO WE CALL *WUNIT* TO WAIT FOR
C      COMPLETION AND THEN CLEAR THE FLAG IN THE FET ARRAY TO INDICATE
C      I/O PENDING.  CONTROL THEN RETURNS TO CALLER.
C
C      FLOW:
C
C      1. CHECK WORD ADDRESS.  IF .LE. 0 ISSUE MSG, SET ERROR FLAG
C         AND EXIT.
C
C      2. CHECK WORD COUNT.  IF .GE. MAXREC ISSUE MESSAGE, SET ERROR
C         CODE AND EXIT.
C
C      3. CHECK WORD COUNT.  IF .LE. 0 ISSUE MSG, SET ERROR FLAG AND
C         EXIT.
C
C      4. SET INITIAL TO FALSE.  SEARCH FILE ARRAY(*FET*) FOR NUMBER
C         MATCHING THE USER SUPPLIED NUMBER OR ASCII STRING.
C
C      5. IF MATCH BETWEEN FILE ARRAY AND USER SUPPLIED NUMBER SAVE
C         INDEX OF FILE ARRAY ENTRY AND SKIP TO 8.
C
C      6. IF FIRST ZERO ENTRY IN FILE ARRAY SAVE THE INDEX TO THIS ZERO.
C
C      7. IF NOT SEARCHED TO *G@DSNMAXW* LOOP BACK TO 5.
C
C      8. CHECK IF MATCHING ENTRY FOUND IN FET TABLE.  IF SO SKIP TO 14.
C
C      9. CALL *WDSET* TO CONVERT FILE NAME/NUMBER TO ASCII NAME.
C
C     10. IF ERROR ON CONVERSION OF FILE NAME/NUMBER ISSUE  *WA100*
C         ERROR AND SET ERROR CODE AND EXIT.
C
C     11. CHECK IF ZERO ENTRY FOUND IN FET TABLE SEARCH.  IF NO ZERO
C         FOUND SKIP TO 13.
C
C     12. IF ZERO WAS FOUND PUT THE USERS NUMBER/NAME HERE, SET THE
C         INITIAL FLAG = .TRUE., AND SKIP TO 14
C
C     13. FET TABLE IS FULL BECAUSE USER HAS EXCEEDED MAX NUMBER OF
C         FILES ALLOWED.  ISSUE MESSAGE SET ERROR CODE AND EXIT.
C
C     13A. CHECK NUMBER OF 512-WORD BLOCKS BECAUSE FET TABLE SLOT ONLY
C         HAS 28 BITS FOR THE NUMBER.
C
C     14. IF FILE HAS BEEN INITIALIZED SKIP TO 21.
C
C     16. CALL *OPENWA* TO DO SYSTEM OPEN, SET RANDOM AND UNBLOCKED
C         MODE FOR THIS FILE, AND OPEN A DUMMY BLOCKED FILE TO OBTAIN
C         BUFFER TO USE.
C
C     17. BUFFER ADDRESS RETURNED FROM *OPENWA* IS SAVED IN THE *FET*
C         ARRAY FOR USE THROUGH THE POINTER VARIABLE FEATURE OF CFT.
C
C     18. *OPENWA* ALSO RETURNS THE LAST BLOCK NUMBER OF THE FILE JUST
C         OPENED, OR A NEGATIVE NUMBER IF THE FILE DOES NOT EXIST.  THIS
C         LAST BLOCK NUMBER (OR 0) IS ALSO SAVED IN THE *FET* ARRAY. THE
C         *FET* ARRAY ENTRY FOR THIS FILE AND THE PAGE BUFFER TABLE IN
C         THE BUFFER AREA ARE THEN CLEARED TO ZERO.
C
C     19. POST A MESSAGE TO THE LOG FILE INDICATING THAT THIS FILE HAS
C         BEEN OPENED.
C
C     20. SKIP TO 22.
C
C     21. FILE WAS OPEN.  IF *INIT* IS ZERO WE POST *WORD ADDRESSABLE
C         FILE ALREADY OPEN * MESSAGE, SET ERROR FLAG TO 2 AND RETURN.
C         THIS CHECK IS NEEDED TO INSURE SOMEONE DOES NOT TRY TO CHANGE
C         BUFFER SIZES BY A CALL TO *WOPEN* ON AN ALREADY OPEN FILE.
C
C     22. COMPUTE CONSTANTS FOR THIS TRANSFER. THESE INCLUDE (BASED ON
C         BSIZE = 512) THE FOLLOWING.
C
C         *FIRST*  =  BLOCK NUMBER OF FIRST DISK BLOCK OF THIS TRANSFER.
C                     *FIRST* GETS INCREMENTED AS EACH BLOCK IS MOVED.
C                     FOR EXAMPLE IF WORD ADDRESS = 10,   FIRST = 1.
C                                 IF WORD ADDRESS = 1000, FIRST = 2
C                                 IF WORD ADDRESS = 1500, FIRST = 3.
C
C         *START*  =  BLOCK NUMBER OF STARTING DISK BLOCK. SAME AS FIRST
C                     BUT DOES NOT GET INCREMENTED DURING THE TRANSFER.
C
C         *LAST*   =  BLOCK NUMBER OF LAST DISK BLOCK INVOLVED IN THIS
C                     TRANSFER.  IF WORD ADDRESS FOR EXAMPLE IS 1000 AND
C                     AND COUNT OF TRANSFER IS 2000 *LAST* WILL BE 6.
C
C
C
C         *BLKCNT* =  NUMBER OF DISK BLOCKS INVOLVED IN THIS TRANSFER.
C
C         *MOVED*  =  INCREMENT INTO USER ARRAY. STARTS AT 1 OF
C                     COURSE, AND INCREMENTS BY 1 FOR FOR EACH WORD
C                     MOVED TO/FROM USER.
C
C         *PARTF*  =  PARTIAL FIRST BLOCK FLAG.  IF TRUE THIS MEANS
C                     FIRST WORD ADDRESS OF TRANSFER DOES NOT START
C                     ON DISK BLOCK BOUNDARY.
C
C         *PARTEND*=  PARTIAL END FLAG. IF TRUE THIS MEANS THE END OF
C                     THE TRANSFER DOES NOT FALL EXACTLY ON A DISK
C                     BLOCK BOUNDARY.
C
C         *FBWA*   =  THE WORD ADDRESS OF THE FIRST BLOCK TO TRANSFER.
C                     IF FIRST BLOCK FOR EXAMPLE  = 1  FBWA = 1
C                                IF FIRST BLOCK   = 2  FBWA = 513.
C                                IF FIRST BLOCK   = 3  FBWA = 1025.
C
C         *LBWA*   =  THE WORD ADDRESS OF THE LAST BLOCK INVOLVED IN
C                     THIS TRANSFER.
C
C         *EOIB*   =  THE BLOCK NUMBER OF THE EOI BLOCK BEFORE THIS
C                     TRANSFER IS STARTED.
C
C         *EOIBLK* =  ARRAY TO SAVE THE EOI BLOCK NUMBER FOR EACH FILE.
C                     IF WE ARE WRITING TO THE FILE THIS WILL BE
C                     INCREMENTED IF *LAST* IS GREATER THAN *EOIB*.
C
C         *END*    =  WORD ADDRESS OF END OF THE TRANSFER.
C
C     23. CHECK IF I/O IS ACTIVE ON THIS FILE.  IF SO CALL *WUNIT* TO
C         WAIT FOR COMPLETION AND CLEAR I/O ACTIVE FLAG.
C
C     24. RETURN.
C
CDIR$  EJECT
      IMPLICIT INTEGER (A-Z)

      INCLUDE "wavars.fh"

      POINTER (FETPTR2,FET2(1))

      INTEGER RBN
      INTEGER RNB

      INTEGER INAME(2)
      
      logical long
c      character*(*) lname

      long = .false.
C
      go to 1
      entry g@lwinit(lname,ierr,blocks,init,stats,aifound,aip,isysfd)
      long = .true.
    1 continue
C
C     Allocate FET if not already done
C
      IF (FETPTR.EQ.0) THEN
        CALL HPALLOC(FETPTR, G@DSNMAXW*B1MAX, IERR, 0)
        IF(IERR.NE.0)THEN
          FETPTR=0
          WRITE (102,*) 
     +  'THE PROGRAM WAS UNABLE TO REQUEST MORE MEMORY SPACE'
          IF(LONG)THEN
            IERR = -FENOMEMY
          ENDIF
          RETURN
        ENDIF
        FETPTR2 = FETPTR
        DO 2 I=1,G@DSNMAXW*B1MAX
          FET2(I) = 0
  2     CONTINUE
      ENDIF

      CALL WDSET(FILE,IDN)

      IF(IDN.EQ.0) THEN
          WRITE (102,*) 'WINIT - ',FILE,' IS AN INVALID UNIT NUMBER'
          IERR = -1
          RETURN
      ENDIF
      IF(IDN.EQ.1) THEN
          WRITE (102,*) 'WINIT - INVALID ASCII FILE NAME '
          IERR = -6
          RETURN
      ENDIF
      IF(ADDRESS.LE.0) THEN
          ASSIGN 1000 TO MESS
          WRITE (102,MESS) RNB(IDN)
          IERR = -4
          RETURN
      ENDIF
      IF(COUNT.GT.MAXREC) THEN
          ASSIGN 1001 TO MESS
          WRITE (102,MESS) RNB(IDN)
          IERR = -5
          RETURN
      ENDIF
      IF(COUNT.LE.0)
     +  THEN
          ASSIGN 1005 TO MESS
          WRITE (102,MESS) RNB(IDN)
          IERR = -7
          RETURN
        ENDIF

C     NOW FIND DATASET NUMBER IN *FARRAY* OR A ZERO ENTRY.

      INITIAL = .FALSE.
      K       = 0
      L       = 0

      DO 10 I = 1,G@DSNMAXW
        IF(FET(1,I) .EQ. IDN) THEN
          K = I
          GO TO 11
        ENDIF
        IF(FET(1,I) .EQ. 0 .AND. L .EQ. 0)L = I
10    CONTINUE
11    CONTINUE
      IF(K.EQ.0) THEN
          IF(L.NE.0)
     +       THEN
              INITIAL = .TRUE.
              K       = L
              FET(1,L)= IDN
              FILE    = IDN
            ELSE

C             IF HERE ERROR WITH TOO MANY DATASETS.

              ASSIGN 1002 TO MESS
              FILE    = IDN
              WRITE (102,MESS) G@DSNMAXW, RNB(FILE)
              IF(LONG)THEN
                IERR = -FEWTOMNY
              ELSE
                IERR = -2
              ENDIF
              RETURN
            ENDIF
        ENDIF

C     DATASET NUMBER IS FOUND - PROCEED

      INDEX = K

C     FET TABLE SPACE FOR NUMBER OF BLOCKS IS 28 BITS

      IF(BLOCKS.GT.BUFFMAX)
     +  THEN
          WRITE(102,*) 'WINIT - MORE THAN ',BUFFMAX,' BLOCKS   '
          IF(LONG)THEN
            IERR = -FEWABLKS
          ELSE
            IERR = -8
          ENDIF
          RETURN
        ENDIF
      IF(INITIAL)
     +  THEN
          PBLKS         = BLOCKS / BSIZE + 1

          if ( long ) then
            call g@openwa(lname,index,ieoi,iaddr,blocks+pblks,
     +      aifound,aip,isysfd,ierr,isector)
            IF (IERR.LT.0)THEN
              FET(1,L) =  0
              RETURN
            ENDIF
          else

C           The filename passed to OPENWA must be null-terminated.  On CRAY-2
C           systems, we allow names to be up to 8 characters in length.

            INAME(1) = FILE 
            INAME(2) = 0                  ! CRAY-2 allows 8 character filenames
c           Note: IDSP is no longer returned or used
            CALL OPENWA(INAME,INDEX,IEOI,IADDR,BLOCKS+PBLKS,IDSP,IERR)
            IF (IERR.LT.0)THEN
              FET(1,L) =  0
              RETURN
            ENDIF
          endif

          FET(2,INDEX)  = SHIFTL(1,62) + SHIFTL(BLOCKS,32)
          FET(18,INDEX)  = IADDR
          EOIB          = IEOI + 1
          WABB          = IADDR
          IF(IEOI.LT.0) EOIB = 0
          DO 30 I = 3,B1MAX
30         FET(I,INDEX)  = 0

          DO 35 I = 1,BLOCKS
35         WABP(I)       = 0
          FET(7,INDEX)  = SHIFTL(EOIB,36)
          FET(16,INDEX) = EOIB
          FET(17,INDEX) = 1
          FILES         = RNB(FILE)
          FET(11,INDEX) = 0
          FET(12,INDEX) = SHIFTL(1,32) + 0

C         ASSIGN 1003 TO MESS
C         IF (STATS.NE.0) WRITE (102,MESS) FILES, IEOI, IADDR
        ELSE
          CALL WDSETB(FILE,INDEX,IADDR)
          WABB         = IADDR
          FET(18,INDEX) = IADDR
          IF(INIT.EQ.0)
     +       THEN
              ASSIGN 1004 TO MESS
              WRITE (102,MESS) FILE
              RETURN
            ENDIF
        ENDIF

C     SET INITIAL VALUES AND CONSTANTS.

      BUFFERS = SHIFTR(FET(2,INDEX),32).A.MASK(128-28)
      PBLKS   = BUFFERS/BSIZE+1
      WABA    = WABB + PBLKS * BSIZE
      FIRST   = (ADDRESS + (BSIZE - 1)) / BSIZE
      START   = FIRST
      MOVED   = 1
      PARTF   = .FALSE.
      PARTEND = .FALSE.
      FBWA    = BSIZE * (FIRST - 1) + 1
      EOIB    = SHIFTR(FET(7,INDEX),36)
      EOIBW   = FET(16,INDEX)
      END     = ADDRESS + COUNT
      LAST    = (ADDRESS + (BSIZE - 2) + COUNT) / BSIZE
      BLKCNT  = LAST - FIRST + 1
      LBWA    = BSIZE * (LAST  - 1) + 1
      IF(FBWA.NE.ADDRESS)
     +  THEN
         IF(FIRST.LE.EOIB) PARTF = .TRUE.
       ENDIF

      IF(LAST.LE.EOIB)
     +  THEN
         IF((LBWA + BSIZE).NE.END) PARTEND = .TRUE.
       ENDIF
      IF(WRITE)
     +  THEN
         EOIBM = SHIFTL(MAX0(LAST,EOIB),36)
         FET(7,INDEX) = (FET(7,INDEX).AND.MASK(128-36))+ EOIBM
       ENDIF

      IF(FET(2,INDEX).LT.0)
     +  THEN

C         I/O IS ACTIVE - WAIT FOR QUIET AND DROP OUT BUSY BIT.

         IT1 = IRTC()
         CALL WUNIT(INDEX)
         FET(10,INDEX) = FET(10,INDEX) + (IRTC()-IT1)
         FET(2,INDEX)  = FET(2,INDEX).AND.MASK(128-63)
       ENDIF

      RETURN

C     ERROR FORMATS FOR PRINTED MESSAGES FOLLOW.

1000   FORMAT('WORD ADDRESS .LE. 0 ON FILE = ',A8)

1001   FORMAT('WORD COUNT .GT. MAXIMUM ON FILE = ',A8)

1002   FORMAT('MAXIMUM NUMBER OF FILES = ',I6,' FILE = ',A8)

1003   FORMAT('W.A. FILE OPENED. FILE = ',A8,' EOI = ',I6,' BUFFER= ',
     2  O12)

1004   FORMAT('WARNING - OPEN ON ALREADY OPENED FILE = ',A8)

1005   FORMAT('WORD COUNT .LE. 0 ON FILE = ',A8)
      END
