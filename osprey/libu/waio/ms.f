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


      SUBROUTINE OPENMS(UNIT,INDEX,LENGTH,IT,IERRF)
CDIR$ ID "@(#) libu/waio/ms.f	92.0	10/08/98 14:57:41"

CC        READMS/WRITMS -  CRAY RANDOM I/O
C
C          FEATURES:
C
C          1. NAMED OR NUMBERED INDEX TYPES.
C          2. THREE WRITE/REWRITE OPTIONS.
C                a. REWRITE RECORD IN PLACE.
C                b. WRITE/REWRITE RECORD AT EOI.
C                c. REWRITE RECORD IN PLACE IF IT FITS, AT EOI IF IT
C                   DOES NOT FIT.
C          3. SUBINDEXS SUPPORTED.
C          4. DATASET UNIT NUMBERS OR HOLLERITH NAMES SUPPORTED.
C          5. RECORD INDEX SAVED AT THE END OF THE DATASET.
C          6. VARIABLE LENGTH RECORDS.
C          7. ASYNCHRONOUS ACCESS OPTION.
C
C          METHOD:
C
C            DATASET MUST BE OPENED BY A CALL TO OPENMS.  OPENMS CHECKS
C          TO SEE IF THE DATASET EXISTS BY A CALL TO THE SYSTEM TO
C          OBTAIN THE LAST BLOCK NUMBER OF THE DATASET.  IF THE DATASET
C          EXISTS THEN IT SHOULD HAVE AN INDEX AND OPENMS WILL THEN READ
C          THE INDEX AND PUT IT IN THE USERS INDEX ARRAY. IF THE DATASET
C          DOES NOT EXIST THE INDEX ARRAY WILL BE ZEROED OUT AND THE
C          FIRST PRU ON THE DATASET IS BLOCKED OUT TO SAVE THE INDEX
C          ADDRESS AT CLOSE TIME.
C
C            ACCESSING THE DATASET IS THEN DONE THRU 'READMS' AND
C          'WRITMS' CALLS.  'CLOSMS' MUST BE CALLED AT JOB END IF
C          THE DATASET HAS BEEN WRITTEN ON TO INSURE THAT THE INDEX
C          WILL BE UPDATED TO THE CORRECT VALUES.  IF THE DATASET
C          IS READ ONLY 'CLOSMS' WILL NOT UPDATE THE INDEX.
C
C            THE READMS/WRITMS ROUTINES MAKE CALLS TO ANOTHER PACKAGE
C          TO DO WORD ADDRESSABLE TYPE I/O.  THE DATASET FORMAT IS
C          THAT OF AN UNBLOCKED/RANDOM COS DATASET. THE FIRST BLOCK IS
C          LEFT EMPTY WHEN THE FILE IS CREATED BY OPENMS AND WHEN CLOSMS
C          IS CALLED IT UPDATES THE FIRST FIVE WORDS WITH THE INDEX
C          VARIABLES.
C
C             IF A USER WISHES TO USE ASYNCHRONOUS ACCESS OF HIS DATA-
C          SETS HE CAN  DO THIS IN TWO  DIFFERENT WAYS. ASYNCHRONOUS
C          READS  CAN BE DONE AT ANY TIME  BY USING A  *FINDMS*  CALL
C          BEFORE A *READMS* CALL.  SINCE READMS/WRITMS USES PAGED FILE
C          BUFFERS, AN ASYNCHRONOUS READ HAS TO LOAD THE PAGED BUFFERS
C          WITH THE  INFORMATION  THE USERS WANTS  TO NEXT READ.   BY
C          MAKING  THE USER DO THE *FINDMS*  CALL AND THEN THE *READMS*
C          CALL THIS PACKAGE DOES NOT HAVE TO SAVE ANY PARAMETERS ACROSS
C          USER CALLS.  ASYNCHRONOUS WRITES CAN BE DONE IF A USER IS
C          IN ASYNC MODE.  A USER CAN GET INTO ASYNC MODE AT ANY TIME
C          BY DOING A CALL TO THE *ASYNCMS* ENTRY, OR HE CAN REQUEST
C          ASYNCHRONOUS  MODE AT OPEN TIME.   A USER CAN ALSO SWITCH
C          BACK TO  SYNCHRONOUS  MODE BY A CALL TO *SYNCMS* ENTRY, OR
C          HE CAN  REQUEST SYNCHRONOUS  MODE AT OPEN TIME.  ON EITHER
C          THE *FINDMS*  CALLS OR *WRITMS*  CALLS IN ASYNC MODE THEIR
C          IS A  POSSIBILITY THAT THE RECORD  REQUESTED WILL NOT FIT
C          INTO THE PAGE TABLE.  IF THIS HAPPENS THE TRANSFER CAN NOT
C          BE DONE IN ONE DISK ACCESS AND CAN NOT BE TRULY ASYNCHRONOUS.
C          THIS IS DETECTED BY THE PACKAGE AND STATUS IS PASSED BACK TO
C          THE USER AS LONG AS THE USER PASSES IN THE ERROR FLAG PARAM.
C
C            IN SUMMARY THEN:
C
C           READMS CALLS ARE SYNCHRONOUS BY DEFAULT.  ASYNCHRONOUS
C           READS CAN BE INITIATED AT ANY TIME BY CALLS TO FINDMS.
C           FINDMS EMPTYS OUT THE PAGE TABLE AND ISSUES A READ OF
C           REQUIRED PAGES.  THE FINDMS CALL IS DESIGNED TO BE USED
C           IN TANDEM WITH READMS CALLS.  AFTER A FINDMS CALL THE
C           NEXT READMS CALL WILL WAIT FOR FILE ACTIVITY TO CEASE
C           AND TRANSFER THE DATA TO THE USER.
C
C           WRITMS CALLS ARE SYNCHRONOUS BY DEFAULT.  IF A USER
C           OVERIDES THE DEFAULT ON HIS OPENMS CALL OR CALLS
C           ASYNCMS TO CHANGE TO ASYNCHRONOUS MODE, THAN ALL
C           WRITES TO DISK ARE ATTEMPTED IN ASYNC MODE.  IF THE
C           RECORD SIZE EXCEEDS THE PAGE SIZE THE WRITE CAN NOT
C           BE DONE IN ASYNC MODE.  THIS IS DETECTED AND A STATUS
C           IS RETURNED TO THE USER.
C
C
C
C            AN EXAMPLE OF A TYPICAL DATASET IS THUS:
C
C          WORD 1 --    WORD ADDRESS OF THE INDEX RECORD  --
C               2 --          LENGTH OF THE INDEX RECORD  --
C               3 --          DATE OF LAST UPDATE         --
C               4 --          TIME OF LAST UPDATE         --
C               5 --         CHECKSUM FOR THIS RECORD     --
C               6 --         FLAG FOR WRITMS/WRITDR       --
C               : --             NULL                     --
C               : --             NULL                     --
C             512 --             NULL                     --
C             513 --        RECORD ONE STARTS HERE        --
C               : --                                      --
C               : --                                      --
C             550 --        RECORD ONE ENDS               --
C             551 --        RECORD TWO BEGINS             --
C               : --                                      --
C               : --                                      --
C               : --                                      --
C               : --                                      --
C               : --                                      --
C             750 --        RECORD TWO ENDS               --
C               : --                                      --
C            4280 --        RECORD N BEGINS               --
C               : --                                      --
C            5188 --        RECORD N ENDS                 --
C            5189 --        INDEX RECORD BEGINS           --
C               : --                                      --
C        5189+N-1 --        INDEX RECORD ENDS             --
C
C
C
CC        TABLES POINTERS VARIABLES ETC.
C
C         VARIABLE *G@DSNMAXW* SETS THE MAXIMUM NUMBER OF ACTIVE DATASETS.
C         IF THE RELEASED VALUE OF 40 IS INADEQUATE THEN THE PARAMATER
C         STATEMENT DEFINING IT IN THIS ROUTINE MUST BE CHANGED. THE
C         SAME VARIABLE MUST ALSO BE CHANGED IN THE GETWA/PUTWA PACKAGE
C         ALSO.
C
C
C         FIT - FILE INFORMATION TABLE.
C
C         CONTAINS INFORMATION PERTINENT TO READMS/WRITMS FILES.
C
C             FORMAT = 12 (B1MAX) WORDS PER ENTRY.  DESCRIBED BELOW IN COS
C                      FORMAT WHERE FIRST ARGUMENT IS THE WORD OFFSET,
C                      SECOND ARGUMENT IS BIT OFFSET, AND THIRD ARGUMENT
C                      IS LENGTH IN BITS OF THE FIELD.NOTE IF THIS TABLE
C                      IS EXPANDED THE PARAMETER *B1MAX* IN COMMON BLOCK
C                      COMBLK1 MUST CHANGED ALSO.
C
C           DATASET NAME        =  1,0,64     LEFT JUSTIFIED FILE NAME
C           INDEX TYPE          =  2,0,1      1= NAMED INDEX. 0= NUMBERE
C           INDEX LENGTH        =  2,1,63     LENGTH OF INDEX ARRAY.
C           SUB INDEX TYPE      =  3,0,1      1= NAMED INDEX. 0= NUMBERE
C           SUB INDEX LENGTH    =  3,1,63     LENGTH OF SUB INDEX ARRAY
C           WRITE FLAG          =  4,0,1      IF ONE FILE WRITE WAS DONE
C           EOI ADDRESS         =  4,22,42    WORD ADDRESS OF EOI OF FIL
C           SYNCHRONOUS FLAG    =  5,0,64     0 IF SYNC. 1 IF ASYNC.
C           TOTAL ACCESS TIME   =  6,0,64     TIME IN CLOCK PERIODS.
C           # REWRITES IN PLACE =  7,0,18     INTEGER NUMBER OF REWRITES
C           WRITES TO EOI       =  7,18,18    NUMBER OF WRITES TO EOI.
C           MAXIMUM RECORDS SIZE=  7,36,28    MAXIMUM RECORD SIZE.
C           SEQUENTIAL READS    =  8,0,18     NUMBER OF SEQUENTIAL READS
C           SEQUENTIAL WRITES   =  8,18,18    NUMBER OF SEQUENTIAL WRITE
C           MINIMUM RECORD SIZE =  8,36,28    MINIMUM RECORD SIZE.
C           NUMBER OF READS     =  9,0,18     TOTAL NUMBER OF READS DONE
C           TOTAL WORDS MOVED   =  9,18,46    WORDS MOVED TO/FROM BUFFER
C           WRITE FLAG          =  10,0,1     IF A ONE LAST OP WAS A WRI
C           LAST RECORD NUMBER  =  10,1,63    NUMBER OF LAST RECORD.
C           INDEX ADDRESS       =  11,0,64    INDEX ARRAY ADDRESS.
C           SUB INDEX ADDRESS   =  12,0,64    ADDRESS OF SUBINDEX ARRAY
C
C
CDIR$  EJECT
CC        OPENMS ENTRY
C
C         CALL OPENMS(UNIT,INDEX,LENGTH,IT,IERRF)
C
C            WHERE :   *UNIT*   =  FILE NUMBER OR HOLLERITH NAME TO OPEN
C                      *INDEX*  =  ARRAY TO HOLD FILE INDEX.
C                      *LENGTH* =  LENGTH OF INDEX ARRAY.
C                      *IT*     =  0 IF NUMBERED STYLE INDEX AND SYNC.
C                      *IT*     =  1 IF NAMED STYLE INDEX AND SYNC.
C                      *IT*     =  2 IF NUMBERED STYLE INDEX AND ASYNC.
C                      *IT*     =  3 IF NAMED STYLE INDEX AND ASYNC.
C                      *IERRF*  =  OPTION ERROR STATUS RETURN.
C                                  IF PARAMETER PASSED, ERROR STATUS
C                                  WILL BE RETURNED TO THE USER.  IF
C                                  IERRF IS NOT USED PROGRAM WILL ABORT.
C
C                                  ON INPUT, IF IERRF > 0 ONLY ERROR
C                                  NUMBER RETURNED, NO LOGFILE MESSAGE
C                                  POSTED.
C
C                                  ON RETURN, IF IERRF < 0, ERROR HAS
C                                  BEEN ENCOUNTERED
C
C                THE ASYNC OR SYNC FLAG AFFECTS THE WRITMS CALLS
C                ONLY.  THE READS CAN BE ASYNC IN EITHER MODE BY
C                USING THE *FINDMS* CALLS.
C                RETURN WITH INDEX(1) THRU INDEX(LENGTH) = 0
C                IF FILE IS NULL, OTHERWISE INDEX(1) THRU
C                INDEX(LENGTH) WILL CONTAIN THE INDEXS FOR FILE.
C
C                ERROR MESSAGES POSTED OR RETURNED TO USER:
C
C                 INVALID UNIT NUMBER OR NAME;            IERRF = -1
C                 INDEX LENGTH .LE. 0;                    IERRF = -2
C                 MAXIMUM NUMBER OF DATASETS EXCEEDED;    IERRF = -3
C                 INDEX LENGTH .GT. USER LENGTH;          IERRF = -4
C                 USER INDEX  .GT. DATASET INDEX LENGTH;  IERRF = -5
C                 INDEX WORD ADDRESS .LE. ZERO;           IERRF = -11
C                 INDEX LENGTH .LE. ZERO;                 IERRF = -12
C                 CHECKSUM ERROR ON DATASET;              IERRF = -13
C                 UNIT ALREADY OPENED BY OPENMS;          IERRF = -14
C                 FILE CREATED BY WRITDR NOT WRITMS;      IERRF = -20
C                 NO MEMORY                               IERRF = -21
C
C         FLOW.
C
C         1. IF INVALID UNIT NAME OR NUMBER - ABORT OR RETURN ERROR.
C         2. CHECK IF FILE ALREADY OPEN. IF SO ABORT OR RETURN ERROR.
C         3. CHECK IF MAX NUMBER OF DATASETS EXCEEDED. IF SO ABORT
C            OR RETURN ERROR CODE TO USER.
C         4. CHECK INDEX LENGTH. IF .LE. ZERO - ABORT OR RETURN ERROR.
C         5. SAVE INDEX TYPE, LENGTH, AND ADDRESS.
C         6. ZERO THE INDEX ARRAY.
C         7. SET A LARGE MINIMUM RECORD SO TRUE MINIMUM WILL BE FOUND.
C         8. READ THE FIRST FIVE WORDS OF THIS DATASET.
C         9. IF NO ERROR ON THE READ THE INDEX EXISTS SO SKIP TO 11.
C        10. DATASET DOES NOT EXIST. WRITE AN EMPTY PRU AND SET THE
C            EOI WORD ADDRESS TO 513.  RETURN.
C        11. THE FIVE WORDS READ FROM THE DATASET ARE CHECKED FOR
C            ERRORS. THE FIRST WORD IS THE FILE INDEX WORD ADDRESS,
C            THIS IS CHECKED TO BE SURE IT IS NOT .LE. ZERO.  THE
C            SECOND WORD CONTAINS THE FILE INDEX LENGTH, THIS IS ALSO
C            CHECKED FOR .LE. ZERO.  THEN THE FIRST FOUR WORDS ARE
C            CHECKSUMED AND COMPARED TO THE VALUE IN WORD FIVE. LASTLY
C            THE INDEX LENGTH FROM THE USER IS COMPARED TO THE ONE
C            READ FROM THE FILE.
C        12. READ IN THE INDEX.  SET THE EOI ADDRESS TO THE BEGINNING
C            ADDRESS OF THE INDEX.  RETURN.
C
C


      INTEGER B1MAX
      PARAMETER (B1MAX=12)
      INTEGER G@DSNMAXW
      COMMON /G@DSNMAXW/G@DSNMAXW
      INTEGER UNIT,UNITN,ISC(10),R,S,RR,FIT(B1MAX,1),SIGNBIT,MAXRECLEN
      INTEGER FIT2(1)
      INTEGER IDXUSR(1)
      DIMENSION INDEX(1),IFWA(1)
      LOGICAL IFLAG,EOI
      PARAMETER(SIGNBIT = 1000000000000000000000B)
      PARAMETER(MAXRECLEN = 2**22-1)
      SAVE ISC
      DATA (ISC(I),I=1,10)/10*0/

      POINTER (FITPTR,FIT)
      POINTER (FITPTR2,FIT2)
      POINTER (IDXPTR,IDXUSR)
      DATA FITPTR/0/

      INTEGER I@CLKTCK

      IABORT  = 1
      INOM    = 0
      IF( NUMARG() .EQ. 5 )THEN
        IABORT = 0
        IF( IERRF .GT. 0 )THEN
          INOM = 1
        ENDIF
        IERRF = 0
      ENDIF

C     Allocate the FIT

      IF(FITPTR.EQ.0) THEN
        CALL HPALLOC(FITPTR, G@DSNMAXW*B1MAX, IERR, 0)
        IF(IERR.NE.0)THEN
          FITPTR=0
          IF(INOM.EQ.0)
     +      THEN
              WRITE (102,*) 'OPENMS - NO MEMORY AVAILABLE'
            ENDIF
          IF(IABORT.EQ.1) CALL ABORT
          IERRF=-21
          RETURN
        ENDIF
        FITPTR2 = FITPTR
        DO 2 I=1,G@DSNMAXW*B1MAX
          FIT2(I) = 0
    2   CONTINUE
      ENDIF

C     CONVERT USER SUPPLIED THING TO ASCII NAME

      CALL WDSET(UNIT,IDN)

      IF( (IDN .EQ. 0) .OR. (IDN .EQ. 1) )
     +  THEN
          IF(INOM.EQ.0)
     +      THEN
              WRITE (102,*) 'OPENMS - INVALID UNIT NUMBER OR NAME '
            ENDIF
          IF(IABORT.EQ.1) CALL ERREXIT
          IERRF = -1
          RETURN
        ENDIF

      IF(LENGTH.LE.0)
     +  THEN

C         ERROR ' INDEX LENGTH .LE. 0 '

          IF(INOM.EQ.0)
     +      THEN
              ASSIGN 9000 TO MSG
              WRITE (102,MSG) IDN
            ENDIF
          IF(IABORT.EQ.1) CALL ERREXIT
          IERRF = -2
          RETURN
        ENDIF

      DO 5 I = 1,G@DSNMAXW
      IF(FIT(1,I).EQ.IDN)
     +  THEN

C         ERROR ' UNIT ALREADY OPENED BY OPENMS '

          IF(INOM.EQ.0)
     +      THEN
              ASSIGN 9013 TO MSG
              WRITE (102,MSG) IDN
            ENDIF
          IF(IABORT.EQ.1) CALL ERREXIT
          IERRF = -14
          RETURN
        ENDIF
      IF(FIT(1,I).EQ.0)
     +  THEN
          FIT(1,I) = IDN
          UNITN    = I
          GO TO 6
        ENDIF
5     CONTINUE
      IF(INOM.EQ.0)
     +  THEN
          WRITE (102,*) 'MAXIMUM NUMBER OF DATASETS EXCEEDED '
        ENDIF
      IF(IABORT.EQ.1) CALL ERREXIT
      IERRF = -3
      RETURN

6     CONTINUE

C     NOW SAVE INDEX TYPE, INDEX LENGTH, AND INDEX ADDRESS.

      INDEXL = LENGTH
      FIT(2,UNITN) = SHIFTL(IT,63) + INDEXL
      FIT(11,UNITN) = LOC(INDEX)

      DO 10 I=1,INDEXL
10    INDEX(I)=0

      DO 15 I = 3,B1MAX
      IF (I.NE.11)FIT(I,UNITN) = 0
15    CONTINUE

C     SET A LARGE MINIMUM RECORD SO WE FIND THE TRUE MIN.
C     SAVE THE USERS SYNC/ASYNC FLAG ALSO.

      FIT(8,UNITN) = 777777777B
      FIT(5,UNITN) = SHIFTR(IT,1)

C     READ IN THE FIRST SIX WORDS OF THE FILE WHICH CONTAIN FWA OF
C     THE INDEX, THE LENGTH OF THE INDEX, THE DATE OF LAST UPDATE,
C     THE TIME OF THE LAST UPDATE, AND A CHECKSUM.

      IERR = 63
      CALL GETWA(UNIT,ISC,1,6,IERR)
      IF(IERR.NE.0)
     +  THEN

C         DATASET DOES NOT EXIST.  BLANK OUT THE FIRST PRU AND SET THE
C         EOI ADDRESS.

          CALL PUTWA(UNIT,INDEX,1,512,IERR)
          FIT(4,UNITN) = OR(SIGNBIT, 513)
          RETURN
        ENDIF
      LENG = INDEXL

C     IF NO ERROR THEN FILE EXISTS.  READ IN INDEX IF EVERYTHING CHECKS.

      IF(ISC(1).LE.0)
     +  THEN

C         ERROR ' INDEX WORD ADDRESS .LE. ZERO  '

          IF(INOM.EQ.0)
     +      THEN
              ASSIGN 9010 TO MSG
              WRITE (102,MSG) IDN
              ASSIGN 9016 TO MSG
              WRITE (102,MSG) IDN
            ENDIF
          IF(IABORT.EQ.1) CALL ERREXIT
          IERRF = -11
          RETURN
        ENDIF

      IF(ISC(2).LE.0)
     +  THEN

C         ERROR ' INDEX LENGTH .LE. ZERO '

          IF(INOM.EQ.0)
     +      THEN
              ASSIGN 9011 TO MSG
              WRITE (102,MSG) IDN
              ASSIGN 9016 TO MSG
              WRITE (102,MSG) IDN
            ENDIF
          IF(IABORT.EQ.1) CALL ERREXIT
          IERRF = -12
          RETURN
        ENDIF

C     CHECK THE CHECKSUM

      ICHK = 0
      DO 20 I = 1,4
      ICHK = ICHK + SHIFTR(ISC(I),16)
20    CONTINUE
      IF(ICHK.NE.ISC(5))
     +  THEN

C         ERROR  'CHECKSUM ERROR ON DATASET '

          IF(INOM.EQ.0)
     +      THEN
              ASSIGN 9012 TO MSG
              WRITE (102,MSG) IDN
              ASSIGN 9016 TO MSG
              WRITE (102,MSG) IDN
            ENDIF
          IF(IABORT.EQ.1) CALL ERREXIT
          IERRF = -13
          RETURN
        ENDIF

      IF(ISC(2).GT.INDEXL)
     +  THEN

C         TRIVIAL ERROR  ' DATASET INDEX LENGTH .GT. USER INDEX LENGTH '

          IF(INOM.EQ.0)
     +      THEN
              ASSIGN 9001 TO MSG
              WRITE (102,MSG) IDN
              ASSIGN 9002 TO MSG
              WRITE (102,MSG) ISC(2), LENGTH
            ENDIF
          IF(IABORT.EQ.0) IERRF = -4
        ENDIF

      IF(INDEXL.GT.ISC(2))
     +  THEN

C         TRIVIAL ERROR ' USER INDEX LENGTH .GT. DATASET INDEX LENGTH'

          IF(INOM.EQ.0)
     +      THEN
              ASSIGN 9003 TO MSG
              WRITE (102,MSG) IDN
              ASSIGN 9002 TO MSG
              WRITE (102,MSG) ISC(2), LENGTH
            ENDIF
          IF(IABORT.EQ.0) IERRF = -5
          LENG = ISC(2)
        ENDIF

      IF(ISC(6).NE.0)
     +  THEN

C         ERROR ' DATASET CREATED BY READDR/WRITDR NOT READMS/WRITMS'
C         THIS IS UNSAFE CONDITION BECAUSE IF THE USER UPDATES A
C         RECORD THE INDEXES WILL NOT LINE UP ON PRU BOUNDARYS.

          IF(INOM.EQ.0)
     +      THEN
              ASSIGN 9021 TO MSG
              WRITE (102,MSG) IDN
            ENDIF
          IF(IABORT.EQ.1) CALL ERREXIT
          IERRF = - 20
          RETURN
        ENDIF

C     READ THE ACTUAL INDEX IN HERE.

      LENFIL = ISC(2) + ISC(1)
      ASSIGN 9017 TO MSG
      WRITE (102,MSG) ISC(3), ISC(4), LENFIL, ISC(2)
      CALL GETWA(UNIT,INDEX,ISC(1),LENG,IERR)
      FIT(4,UNITN) = ISC(1)
      RETURN
CDIR$ EJECT
CC    STINDX ENTRY.
C
C     CHANGE THE INDEX TO A SUBINDEX.
C
C     CALL STINDX(UNIT,INDEX,LENGTH,IT,IERRF)
C
C     WHERE:   UNIT = UNIT NUMBER OR NAME TO CHANGE.
C             INDEX = NEW INDEX ADDRESS.
C            LENGTH = LENGTH OF INDEX ARRAY.
C               IT  = 0 IF NUMBERED STYLE INDEX AND SYNC.
C               IT  = 1 IF NAMED STYLE INDEX AND SYNC.
C               IT  = 2 IF NUMBERED STYLE INDEX AND ASYNC.
C               IT  = 3 IF NAMED STYLE INDEX AND ASYNC.
C             IERRF =  OPTION ERROR STATUS RETURN.
C                      IF PARAMETER PASSED, ERROR STATUS
C                      WILL BE RETURNED TO THE USER.  IF
C                      IERRF IS NOT USED PROGRAM WILL ABORT.
C
C                      ON INPUT, IF IERRF > 0 ONLY ERROR
C                      NUMBER RETURNED, NO LOGFILE MESSAGE
C                      POSTED.
C
C                      ON RETURN, IF IERRF < 0, ERROR HAS
C                      BEEN ENCOUNTERED
C
C        ERROR MESSAGES:
C
C           INVALID UNIT NUMBER OR NAME;             IERRF = -1
C           OPENMS NOT CALLED;                       IERRF = -15
C           STINDX CALL CANNOT CHANGE INDEX TYPE;    IERRF = -16
C
C
C     FLOW:
C
C     1.  CHECK UNIT NAME/NUMBER. IF ERROR ABORT.
C     2.  IF UNIT NOT OPENED BY OPENMS ABORT.
C     2.  CHECK IF NEW TYPE MATCHES OLD TYPE. IF NOT ABORT.
C     3.  IF FIRST SUBINDEX CALL MOVE MASTER INDEX SAVE AREA.
C     4.  SAVE NEW INDEX INFO AS PRIMARY INDEX INFO.
C     5.  EXIT.
C

      ENTRY STINDX(UNIT,INDEX,LENGTH,IT,IERRF)

      IABORT  = 1
      INOM    = 0
      IF( NUMARG() .EQ. 5 )THEN
        IABORT = 0
        IF( IERRF .GT. 0 )THEN
          INOM = 1
        ENDIF
        IERRF = 0
      ENDIF
      CALL WDSET(UNIT,IDN)
      IF( (IDN .EQ. 0) .OR. (IDN .EQ. 1) )
     +  THEN
          IF(INOM.EQ.0)
     +      THEN
              WRITE (102,*) 'STINDX - INVALID UNIT NUMBER OR NAME '
            ENDIF
          IF(IABORT.EQ.1) CALL ERREXIT
          IERRF = -1
          RETURN
        ENDIF
      UNITN = 0
      DO 100 I = 1,G@DSNMAXW
      IF(FIT(1,I).EQ.IDN) UNITN = I
100   CONTINUE
      IF(UNITN.EQ.0)
     +  THEN
          IF(INOM.EQ.0)
     +      THEN
              ASSIGN 9014 TO MSG
              WRITE (102,MSG) IDN
            ENDIF
          IF(IABORT.EQ.1) CALL ERREXIT
          IERRF = -15
          RETURN
        ENDIF
      ITOLD = SHIFTR(FIT(2,UNITN),63)
      ITD = MOD(IT,2)
      IF(ITOLD.NE.ITD)
     +  THEN
          IF(INOM.EQ.0)
     +      THEN
              ASSIGN 9015 TO MSG
              WRITE (102,MSG) IDN
            ENDIF
          IF(IABORT.EQ.1) CALL ERREXIT
          IERRF = -16
          RETURN
        ENDIF
      FIT(5,UNITN) = SHIFTR(IT,1)
      IF((FIT(3,UNITN).EQ.0).AND.(FIT(12,UNITN).EQ.0))THEN
          FIT(3,UNITN) = FIT(2,UNITN)
          FIT(12,UNITN) = FIT(11,UNITN)
      ENDIF
      FIT(2,UNITN) = SHIFTL(IT,63) + LENGTH
      FIT(11,UNITN) = LOC(INDEX)
      RETURN
CDIR$ EJECT
CC    CHECKMS/WAITMS ENTRY.
C
C     CHECKMS.
C
C     CHECK THE STATUS OF FILE TRANSFER.
C
C     CALL CHECKMS(UNIT,ISTAT,IERRF)
C
C     WHERE:   UNIT = UNIT NUMBER OR NAME TO CHECK.
C             ISTAT = STATUS PASSED BACK TO USER.
C                     IF ISTAT = 0 THERE IS NO FILE ACTIVITY.
C                     IF ISTAT = 1 THERE IS FILE ACTIVITY.
C                     (NO RECALL IS DONE THIS IS A CHECK.)
C            IERRF  =  OPTIONAL ERROR STATUS RETURN
C                      IF PARAMETER PASSED, ERROR STATUS
C                      WILL BE RETURNED TO THE USER.  IF
C                      IERRF IS NOT USED PROGRAM WILL ABORT.
C
C                      ON INPUT, IF IERRF > 0 ONLY ERROR
C                      NUMBER RETURNED, NO LOGFILE MESSAGE
C                      POSTED.
C
C                      ON RETURN, IF IERRF < 0, ERROR HAS
C                      BEEN ENCOUNTERED.
C
C        ERROR MESSAGES:
C
C           INVALID UNIT NUMBER OR NAME       IERRF = -1
C           OPENDR NON CALLED                 IERRF = -15
C
C
C     WAITMS.
C
C     WAIT FOR I/O QUIET ON FILE.
C
C     CALL WAITMS(UNIT,ISTAT,IERRF)
C
C     WHERE:   UNIT = UNIT NUMBER OR NAME TO WAIT FOR I/O QUIET.
C             ISTAT = STATUS PASSED BACK TO USER.
C                     IF ISTAT = 0 THERE WAS NO FILE ERROR.
C                     IF ISTAT = 1 THERE WAS AN ERROR.
C            IERRF  = OPTIONAL ERROR STATUS RETURN
C                     IF PARAMETER PASSED, ERROR STATUS
C                     WILL BE RETURNED TO THE USER.  IF
C                     IERRF IS NOT USED PROGRAM WILL ABORT.
C
C                     ON INPUT, IF IERRF > 0 ONLY ERROR
C                     NUMBER RETURNED, NO LOGFILE MESSAGE
C                     POSTED.
C
C                     ON RETURN, IF IERRF < 0, ERROR HAS
C                     BEEN ENCOUNTERED.
C
C        ERROR MESSAGES:
C
C
C           INVALID UNIT NUMBER OR NAME       IERRF = -1
C           OPENDR NON CALLED                 IERRF = -15
C
C     FLOW:
C
C     1.  RECORD ENTRY. CHECK UNIT NAME/NUMBER. IF ERROR ABORT.
C     2.  IF UNIT NOT OPENED BY OPENMS ABORT.
C     3.  CHECK FILE ACTIVITY AND SET ISTAT ACCORDINGLY.
C     4.  EXIT.
C

      ENTRY CHECKMS(UNIT,ISTAT,IERRF)
      ICHECK = 1
      GO TO 11

      ENTRY WAITMS(UNIT,ISTAT,IERRF)
      ICHECK  = 0
11    CONTINUE
      IABORT  = 1
      INOM    = 0
      IF( NUMARG() .EQ. 3 )THEN
        IABORT = 0
        IF( IERRF .GT. 0 )THEN
          INOM = 1
        ENDIF
        IERRF = 0
      ENDIF
      CALL WDSET(UNIT,IDN)
      IF(IDN.EQ.0)
     +  THEN
          IF(INOM.EQ.0)
     +     THEN
            WRITE (102,*)
     +            'CHECKMS, WAITMS - INVALID UNIT NUMBER OR NAME'
           ENDIF
          IF(IABORT.EQ.1) CALL ERREXIT
          IERRF = -1
          RETURN
        ENDIF
      UNITN = 0
      DO 190 I = 1,G@DSNMAXW
      IF(FIT(1,I).EQ.IDN)
     +  THEN
          UNITN    = I
        ENDIF
190   CONTINUE
      IF(UNITN.EQ.0)
     +  THEN
          IF(INOM.EQ.0)
     +      THEN
              ASSIGN 9014 TO MSG
              WRITE (102,MSG) IDN
            ENDIF
          IF(IABORT.EQ.1) CALL ERREXIT
          IERRF = -15
          RETURN
        ENDIF
      ISTAT = 0
      IF(ICHECK.EQ.0)
     +  THEN

C         CALL THE WAIT ROUTINE TO WAIT FOR I/O QUIET.

          CALL WUNIT(UNITN)
        ELSE

C         NOW CALL CAL ROUTINE TO SEE IF FILE IS ACTIVE.

          CALL CHKUNIT(UNITN,ISTATF)
          IF(ISTATF.EQ.0)ISTAT = 1
        ENDIF
      RETURN
CDIR$ EJECT
CC    ASYNCMS/SYNCMS ENTRY.
C
C     ASYNCMS.
C
C     SET THE WRITE ACCESS MODE TO ASYNCHRONOUS.
C     (ASYNCHRONOUS READ CALLS ARE DONE WITH THE FINDMS CALLS)
C
C     CALL ASYNCMS(UNIT,IERRF)
C
C     WHERE:   UNIT = UNIT NUMBER OR NAME TO CHANGE.
C             IERRF = ERROR FLAG. IF IERRF = 63 ON INPUT NO
C                     LOG MESSAGE WILL BE POSTED, ON OUTPUT
C                     IERRF WILL HAVE ERROR NUMBER IF ONE IS
C                     FOUND, ZERO IF NO ERROR.
C
C        ERROR CODES:
C
C        INVALID UNIT NUMBER OR NAME;         IERRF = -1
C        OPENMS NOT CALLED;                   IERRF = -15
C
C
C     SYNCMS.
C
C     SET THE WRITE ACCESS MODE TO SYNCHRONOUS.
C
C     CALL SYNCMS(UNIT,IERRF)
C
C     WHERE:   UNIT = UNIT NUMBER OR NAME TO CHANGE.
C             IERRF = ERROR FLAG. IF IERRF = 63 ON INPUT NO
C                     LOG MESSAGE WILL BE POSTED, ON OUTPUT
C                     IERRF WILL HAVE ERROR NUMBER IF ONE IS
C                     FOUND, ZERO IF NO ERROR.
C
C        ERROR CODES:
C
C        INVALID UNIT NUMBER OR NAME;         IERRF = -1
C        OPENMS NOT CALLED;                   IERRF = -15
C
C     FLOW:
C
C     1.  RECORD ENTRY. CHECK UNIT NAME/NUMBER. IF ERROR ABORT.
C     2.  IF UNIT NOT OPENED BY OPENMS ABORT.
C     3.  SET NEW MODE IN THE FIT TABLE.
C     4.  EXIT.
C

      ENTRY ASYNCMS(UNIT,IERRF)
      IASYNC = 1
      GO TO 17

      ENTRY SYNCMS(UNIT,IERRF)
      IASYNC  = 0
17    CONTINUE
      IABORT  = 1
      INOM    = 0
      IF( NUMARG() .EQ. 2 )THEN
        IABORT = 0
        IF( IERRF .GT. 0 )THEN
          INOM = 1
        ENDIF
        IERRF = 0
      ENDIF
      CALL WDSET(UNIT,IDN)
      IF(IDN.EQ.0)
     +  THEN
          IF(INOM.EQ.0)
     +      THEN
              WRITE (102,*) 'RA100 ERROR - INVALID UNIT NUMBER '
            ENDIF
          IF(IABORT.EQ.1) CALL ERREXIT
          IERRF =-1
          RETURN
        ENDIF
      UNITN = 0
      DO 195 I = 1,G@DSNMAXW
      IF(FIT(1,I).EQ.IDN)
     +  THEN
          UNITN    = I
        ENDIF
195   CONTINUE
      IF(UNITN.EQ.0)
     +  THEN
          IF(INOM.EQ.0)
     +      THEN
              ASSIGN 9014 TO MSG
              WRITE (102,MSG) IDN
            ENDIF
          IF(IABORT.EQ.1) CALL ERREXIT
          IERRF = -15
          RETURN
        ENDIF

C     NOW SET NEW STATUS IN THE FIT TABLE.

      FIT(5,UNITN) = IASYNC
      RETURN
CDIR$ EJECT
CC    WRITMS ENTRY
C
C      CALL WRITMS(UNIT,IFWA,N,KK,RR,S,IERRF)
C
C      WHERE:   UNIT = UNIT NUMBER 1 TO 99, OR DATASET NAME.
C               IFWA = SOURCE ARRAY TO WRITE FROM.
C                  N = NUMBER OF WORDS TO WRITE.
C                 KK = RECORD NUMBER OR 64 BIT NAMED RECORD.
C                 RR = REWRITE CONTROL FLAG.
C                      IF RR = 0 - REWRITE THE RECORD AT EOI.(DEFAULT)
C                      IF RR = 1 - REWRITE THE RECORD IN PLACE IF IT
C                                  FITS. ABORT IF IT DOES NOT FIT.
C                      IF RR =-1 - REWRITE THE RECORD IN PLACE IF IT
C                                  FITS, OR REWRITE IT AT EOI IF IT
C                                  DOES NOT FIT.
C                  S = SUBINDEX FLAG.  (DEFERRED IMPLEMENTATION)
C
C              IERRF = OPTION ERROR STATUS RETURN.
C                      IF PARAMETER PASSED, ERROR STATUS
C                      WILL BE RETURNED TO THE USER.  IF
C                      IERRF IS NOT USED PROGRAM WILL ABORT.
C
C                      ON INPUT, IF IERRF > 0 ONLY ERROR
C                      NUMBER RETURNED, NO LOGFILE MESSAGE
C                      POSTED.
C
C                      ON RETURN, IF IERRF < 0, ERROR HAS
C                      BEEN ENCOUNTERED
C
C           ERROR CODES:
C
C           INVALID UNIT NUMBER OR NAME;         IERRF = -1
C           INVALID NAMED INDEX;                 IERRF = -6
C           RECORD INDEX ARRAY FULL;             IERRF = -7
C           INDEX NUMBER .GT. MAXIMUM;           IERRF = -8
C           REWRITE RECORD EXCEEDS ORIGINAL;     IERRF = -9
C           OPENMS NOT CALLED;                   IERRF = -15
C           INDEX ENTRY .LE. 0;                  IERRF = -17
C           RECORD LENGTH .LE. 0;                IERRF = -18
C           INDEX OFFSET .LE. 0;                 IERRF = -19
C           ASYNCHRONOUS WRITE .GT. PAGE SIZE.   IERRF = -21  (Warning)
C           REQUESTED RECORD LEN .GT. MAXRECLEN  IERRF = -26
C
C     FLOW:
C
C     1. IF INVALID DATASET NAME OR OPENMS NOT CALLED ABORT.
C     2. SET WRITE = .TRUE. FOR THIS UNIT. THE INDEX WILL BE UPDATED
C        WHEN CLOSMS IS CALLED FOR THIS DATASET.
C     3. COMPUTE INDEX ADDRESS FOR THIS DATASET.
C     4. CHECK IF THIS IS A NAMED OR NUMBERED INDEX DATASET.
C     5. IF NUMBERED SKIP TO 12.
C     6. NAMED INDEX.  IF INDEX = +0 OR -0  -  ABORT.
C     7. SEARCH INDEX ARRAY FOR MATCH. IF FOUND SET LENGTH AND
C        RECORD ADDRESS.  GO TO 11.
C     8. IF ZERO ENTRY FOUND WE ARE AT THE END OF THE INDEX ENTRIES,
C        SO PUT NAME HERE AND SKIP TO 11.
C     9. LOOP BACK TO 7 IF NOT TO END.
C    10. INDEX ARRAY OVERFLOW. -  ABORT.
C    11. SKIP TO 14
C    12. IF INDEX NUMBER .LE. 0 - ABORT. IF RECORD NUMBER .GT. MAXIMUM,
C        INDEX ARRAY OVERFLOW - ABORT.
C    13. SET WORD ADDRESS AND RECORD LENGTH FROM INDEX ARRAY.
C    14. IF REWRITE FLAG .EQ. -1 AND RECORD LENGTH LESS THAN OR EQUAL
C        THE OLD RECORD LENGTH SET FLAG TO REWRITE IN PLACE.  IF RECORD
C        DOES NOT FIT SET REWRITE TO EOI FLAG.
C    15. IF REWRITE FLAG .EQ. 1 SET THE REWRITE IN PLACE FLAG AND THEN
C        CHECK IF THE RECORD WILL FIT. IF NOT ISSUE MSG AND ABORT.
C    16. IF REWRITE FLAG .NE. 1 OR REWRITE FLAG .NE. -1, SET REWRITE
C        FLAG TO 0 (THE DEFAULT) TO SIGNAL WRITE TO EOI.
C    17. IF EOI FLAG TRUE (DEFAULT VALUE) THEN WRITE AT EOI UPDATE
C        THE INDEX ENTRY AND INCREASE THE EOI ADDRESS FOR THIS DATASET.
C        IF THE USER IS IN ASYNC MODE CHECK IF THE RECORD FIT INTO THE
C        PAGE TABLE.  THIS IS DONE SO THE USER KNOWS IF HE EXCEEDS THE
C        RECORD SIZE FOR ASYNCHRONOUS WRITES.
C    18. IF EOI FLAG FALSE THEN REWRITE THE RECORD IN PLACE.  IF USER
C        IS IN ASYNC MODE CHECK IF THE RECORD FIT IN THE PAGE TABLE.
C    19. UPDATE FILE STATISTICS AND THEN RETURN.
C


      ENTRY WRITMS(UNIT,IFWA,N,KK,RR,S,IERRF)


      IABORT  = 1
      INOM    = 0
      IF( NUMARG() .EQ. 7 )THEN
        IABORT = 0
        IF( IERRF .GT. 0 )THEN
          INOM = 1
        ENDIF
        IERRF = 0
      ENDIF
      K = KK
      R = RR
      CALL WDSET(UNIT,IDN)
      IF( (IDN .EQ. 0) .OR. (IDN .EQ. 1) ) THEN
          IF(INOM.EQ.0) THEN
              WRITE (102,*) 'WRITMS - INVALID UNIT NUMBER OR NAME '
          ENDIF
          IF(IABORT.EQ.1) CALL ERREXIT
          IERRF = -1
          RETURN
      ENDIF

      IF ( N.GT.MAXRECLEN ) THEN
          IF(INOM.EQ.0) THEN
              WRITE (102, *) ' THE REQUESTED RECORD SIZE OF ',N, 
     .           ' WORDS EXCEEDS THE MAXIMUM SUPPORTED SIZE '
              WRITE (102, *) ' OF ',MAXRECLEN,' WORDS.'
          ENDIF
          IF(IABORT.EQ.1) CALL ERREXIT
          IERRF = -26
          RETURN
      ENDIF

      UNITN = 0
      DO 250 I = 1,G@DSNMAXW
      IF(FIT(1,I).EQ.IDN)
     +  THEN
          UNITN = I
          GO TO 260
        ENDIF
250   CONTINUE
260   IF(UNITN.EQ.0)
     +  THEN

C         ERROR  ' UNIT NOT INITIALIZED BY OPENMS '

          IF(INOM.EQ.0)
     +      THEN
              ASSIGN 9014 TO MSG
              WRITE (102,MSG) IDN
            ENDIF
          IF(IABORT.EQ.1) CALL ERREXIT
          IERRF = -15
          RETURN
        ENDIF

C     SET WRITE FLAG SO INDEX WILL BE REWRITTEN AT CLOSEMS TIME

      FIT(4,UNITN) = FIT(4,UNITN) .OR. SIGNBIT
      IND   = FIT(11,UNITN)
      INDL  = FIT(2,UNITN).AND.MASK(128-63)
      ITYPE = FIT(2,UNITN)
      IEOI  = FIT(4,UNITN).AND.MASK(128-42)
      IDXPTR= FIT(11,UNITN)
      IF(ITYPE.LT.0)
     +  THEN

C         FILE WILL BE REFERENCED THRU NAMED INDEX.
C         START OF THE NAMED INDEX CODE.

          IF(K.EQ.0.OR.K.EQ.-0)
     +      THEN

C             ERROR  ' INVALID NAMED INDEX ON UNIT '

              IF(INOM.EQ.0)
     +          THEN
                  ASSIGN 9004 TO MSG
                  WRITE (102,MSG) IDN
                ENDIF
              IF(IABORT.EQ.1) CALL ERREXIT
              IERRF = -6
              RETURN
            ENDIF
          LRA = 0
          LENG = 0

C         SEARCH INDEX ARRAY FOR A MATCHING NAME.

          DO 300 I = 1,INDL,2
          IF(IDXUSR(I).EQ.K)
     +      THEN
              K   = I + 1
              LRA = IDXUSR(I+1).AND.MASK(128-42)
              LENG = SHIFTR(IDXUSR(I+1),42)
              GO TO 301
            ENDIF
          IF(IDXUSR(I).EQ.0)
     +      THEN
              IDXUSR(I) = K
              K    = I + 1
              GO TO 301
            ENDIF
300       CONTINUE

C         IF WE GET HERE WE DID NOT FIND A MATCH OR A ZERO ENTRY, ISSUE
C         ERROR  '  NAMED RECORD INDEX ARRAY OVERFLOW '  AND ABORT.

          IF(INOM.EQ.0)
     +      THEN
              ASSIGN 9005 TO MSG
              WRITE (102,MSG) IDN
            ENDIF
          IF(IABORT.EQ.1) CALL ERREXIT
          IERRF = -7
          RETURN
301       CONTINUE
        ELSE

C       END OF NAMED INDEX CODE.



C         START OF CODE FOR NUMBERED INDEX RANDOM FILE.

          IF(K.LE.0)
     +      THEN

C             ERROR  '  INDEX NUMBER .LE. 0 ON NUMBERED INDEX'

              IF(INOM.EQ.0)
     +          THEN
                  ASSIGN 9020 TO MSG
                  WRITE (102,MSG) IDN
                ENDIF
              IF(IABORT.EQ.1) CALL ERREXIT
              IERRF = -19
              RETURN
            ENDIF
          IF(K.GT.INDL)
     +      THEN

C             ERROR  '  INDEX ARRAY OVERFLOW ON NUMBERED INDEX '

              IF(INOM.EQ.0)
     +          THEN
                  ASSIGN 9006 TO MSG
                  WRITE (102,MSG) IDN
                ENDIF
              IF(IABORT.EQ.1) CALL ERREXIT
              IERRF = -8
              RETURN
            ENDIF
          LRA = IDXUSR(K).AND.MASK(128-42)
          LENG = SHIFTR(IDXUSR(K),42)
        ENDIF

C     NOW EVALUATE THE REWRITE IN PLACE PARAMATER.

      IF(R.EQ.-1)
     +  THEN

C         IF R = -1 WE WILL REWRITE IN PLACE IF RECORD FITS, OTHERWISE
C         WRITE TO EOI.

          EOI = .TRUE.
          IF(N.LE.LENG) EOI = .FALSE.
        ENDIF

      IF(R.EQ.1)
     +  THEN

C         IF R = 1, REWRITE IN PLACE IF IT FITS, ABORT IF IT DOES NOT.

          EOI = .FALSE.
          IF(N.GT.LENG)
     +      THEN

C             ERROR  '  REWRITE IN PLACE OF LONGER RECORD '

              IF(INOM.EQ.0)
     +          THEN
                  ASSIGN 9007 TO MSG
                  WRITE (102,MSG) IDN
                  ASSIGN 9008 TO MSG
                  WRITE (102,MSG) LENG, N
                ENDIF
              IF(IABORT.EQ.1) CALL ERREXIT
              IERRF = -9
              RETURN
            ENDIF
        ENDIF

C     IF R = 0  WRITE TO EOI.

      IF(R.NE.-1.AND.R.NE.1)EOI = .TRUE.

      IF(N.LE.0)
     +  THEN
          IF(INOM.EQ.0)
     +      THEN
              ASSIGN 9019 TO MSG
              WRITE (102,MSG) IDN, K
            ENDIF
          IF(IABORT.EQ.1) CALL ERREXIT
          IERRF = -18
          RETURN
        ENDIF

      IF(EOI)
     +  THEN

C         WRITE THE RECORD AT *EOI*

          IUPDAT1 = SHIFTL(1,28)
          IT1     = IRTC()
          IF(IEOI.LE.0)
     +      THEN
              IF(INOM.EQ.0)
     +          THEN
                  ASSIGN 9018 TO MSG
                  WRITE (102,MSG) IDN, K
                ENDIF
              IF(IABORT.EQ.1) CALL ERREXIT
              IERRF = -17
              RETURN
            ENDIF

C         NOW CHECK SYNCH FLAG AND IF 0 DO CALL SYNCHRONOUSLY.

          IF(FIT(5,UNITN).EQ.0)
     +      THEN
              CALL PUTWA(UNIT,IFWA,IEOI,N,IERR)
              ITIME        = IRTC() - IT1
            ELSE
              CALL APUTWA(UNIT,IFWA,IEOI,N,IERR)
              ITIME        = IRTC() - IT1
              IF(IERR.EQ.1)
     +          THEN

C                 IERR = 1 MEANS THE APUTWA CALL WAS FOR MORE
C                 WORDS THAN WOULD FIT IN THE PAGE TABLE.  THE
C                 CALL WAS DONE SEQUENTIALLY.

                  IF(INOM.EQ.0)
     +              THEN
                      ASSIGN 9022 TO MSG
                      WRITE (102,MSG) IDN
                    ENDIF
                  IF(IABORT.EQ.0) IERRF = -21           ! Warning
                ENDIF
            ENDIF
          IDXUSR(K)  = (SHIFTL(N,42)) + IEOI
          IEOI         = IEOI + N
          FIT(4,UNITN) = (FIT(4,UNITN).AND.MASK(22)) + IEOI
        ELSE

C         REWRITE IN PLACE.

          IUPDAT1 = SHIFTL(1,46)
          IT1     = IRTC()
          IF(LRA.LE.0)
     +      THEN
              IF(INOM.EQ.0)
     +          THEN
                  ASSIGN 9018 TO MSG
                  WRITE (102,MSG) IDN, K
                ENDIF
              IF(IABORT.EQ.1) CALL ERREXIT
              IERRF = -17
              RETURN
            ENDIF

C         NOW CHECK SYNCH FLAG AND IF 0 DO CALL SYNCHRONOUSLY.

          IF(FIT(5,UNITN).EQ.0)
     +      THEN
              CALL PUTWA(UNIT,IFWA,LRA,N,IERR)
              ITIME        = IRTC() - IT1
            ELSE
              CALL APUTWA(UNIT,IFWA,LRA,N,IERR)
              ITIME        = IRTC() - IT1
              IF(IERR.EQ.1)
     +          THEN

C                 IERR = 1 MEANS THE APUTWA CALL WAS FOR MORE
C                 WORDS THAN WOULD FIT IN THE PAGE TABLE.  THE
C                 CALL WAS DONE SEQUENTIALLY.

                  IF(INOM.EQ.0)
     +              THEN
                      ASSIGN 9022 TO MSG
                      WRITE (102,MSG) IDN
                    ENDIF
                  IF(IABORT.EQ.0) IERRF = -21           ! Warning
                ENDIF
            ENDIF
        ENDIF

C     UPDATE STATISTICS

      MAXREC = FIT(7,UNITN).AND.MASK(128-28)
      MINREC = FIT(8,UNITN).AND.MASK(128-28)
      IF(N.GT.MAXREC) FIT(7,UNITN) =(FIT(7,UNITN).AND.MASK(36)) + N
      IF(N.LT.MINREC) FIT(8,UNITN) =(FIT(8,UNITN).AND.MASK(36)) + N
      FIT(6,UNITN) = FIT(6,UNITN) + ITIME
      FIT(7,UNITN) = FIT(7,UNITN) + IUPDAT1
      IUP3 = 0
      IF(FIT(10,UNITN).LT.0)
     +  THEN
          IF((FIT(10,UNITN).AND.MASK(128-32))+1.EQ.K)IUP3 =SHIFTL(1,28)
        ENDIF
      FIT(8,UNITN) = FIT(8,UNITN) + IUP3
      FIT(9,UNITN) = FIT(9,UNITN) + N
      FIT(10,UNITN) = K + 1000000 00000 00000 00000B
      RETURN

CDIR$  EJECT
CC    READMS AND FINDMS ENTRY HERE.
C
C     CALL READMS(UNIT,IFWA,N,KK,IERRF)
C
C     WHERE:  UNIT = UNIT NUMBER FROM 1 TO 99, OR DATASET NAME.
C             IFWA = ARRAY TO RECEIVE DATA.
C                N = NUMBER OF WORDS TO READ.
C               KK = RECORD NUMBER OR NAMED RECORD TO READ.
C            IERRF = OPTION ERROR STATUS RETURN.
C                    IF PARAMETER PASSED, ERROR STATUS
C                    WILL BE RETURNED TO THE USER.  IF
C                    IERRF IS NOT USED PROGRAM WILL ABORT.
C
C                    ON INPUT, IF IERRF > 0 ONLY ERROR
C                    NUMBER RETURNED, NO LOGFILE MESSAGE
C                    POSTED.
C
C                    ON RETURN, IF IERRF < 0, ERROR HAS
C                    BEEN ENCOUNTERED
C
C
CC    FINDMS. ASYNC LOAD OF RECORD TO BE NEXT READMS CALL.
C
C     CALL FINDMS(UNIT,N,KK,IERRF)
C
C     WHERE:  UNIT = UNIT NUMBER FROM 1 TO 99, OR DATASET NAME.
C                N = NUMBER OF WORDS TO READ.
C               KK = RECORD NUMBER OR NAMED RECORD TO READ.
C            IERRF = OPTION ERROR STATUS RETURN.
C                    IF PARAMETER PASSED, ERROR STATUS
C                    WILL BE RETURNED TO THE USER.  IF
C                    IERRF IS NOT USED PROGRAM WILL ABORT.
C
C                    ON INPUT, IF IERRF > 0 ONLY ERROR
C                    NUMBER RETURNED, NO LOGFILE MESSAGE
C                    POSTED.
C
C                    ON RETURN, IF IERRF < 0, ERROR HAS
C                    BEEN ENCOUNTERED
C
C
C           ERROR MESSAGES:
C
C           INVALID UNIT NUMBER OR NAME;         IERRF = -1
C           INVALID NAMED INDEX;                 IERRF = -6
C           INDEX NUMBER .GT. MAXIMUM;           IERRF = -8
C           REWRITE RECORD EXCEEDS ORIGINAL;     IERRF = -9
C           NAMED RECORD NOT FOUND;              IERRF = -10
C           OPENMS NOT CALLED;                   IERRF = -15
C           INDEX ENTRY .LE. 0;                  IERRF = -17
C           RECORD LENGTH .LE. 0;                IERRF = -18
C           INDEX OFFSET .LE. 0;                 IERRF = -19
C           RECORD SIZE .GT. PAGE SIZE.          IERRF = -22  (Warning)
C
C
C     FLOW:
C
C     1. IF INVALID DATASET NAME OR OPENMS NOT CALLED ABORT.
C     2. COMPUTE INDEX ARRAY ADDRESS AND TYPE FOR THIS UNIT.
C     3. IF NUMBERED INDEX DATASET SKIP TO 10.
C     4. IF INDEX NAME IS +0 OR -0  -  ABORT.
C     5. SEARCH INDEX ARRAY FOR A MATCHING NAME.
C     6. IF MATCH SET RECORD LENGTH AND WORD ADDRESS. SKIP TO 9.
C     7. LOOP BACK TO 6 UNTIL INDEX LIMIT.
C     8. NAME NOT FOUND IN INDEX ARRAY  -  ABORT.
C     9. SKIP TO 12.
C    10. DATASET WILL BE REFERENCED BY NUMBERED INDEX.  IF INDEX.LE. 0
C        OR INDEX .GT. LIMIT AS SET AT OPENMS TIME  -  ABORT.
C    11. SET RECORD LENGTH AND WORD ADDRESS FROM INDEX ARRAY.
C    12. ABORT IF EITHER RECORD LENGTH OR WORD ADDRESS IS ZERO.
C    13. IF REQUESTED LENGTH IS .GT. RECORD LENGTH ONLY TRANSFER
C        THE ACTUAL RECORD LENGTH.
C    14. IF FIND CALL DO AN ASYNC READ AND THEN RETURN.
C    15. READ IN THE RECORD FROM WORD ADDRESSABLE DATASET.
C    16. UPDATE THE FILE STATISTICS AND RETURN.
C
C


      ENTRY FINDMS(UNIT,N,KK,IERRF)
      FIND    = 1
      IABORT  = 1
      IF(NUMARG().EQ.4)IABORT = 0
      GO TO 1
      ENTRY READMS(UNIT,IFWA,N,KK,IERRF)
      FIND    = 0
      IABORT  = 1
      IF(NUMARG().EQ.5)IABORT = 0
1     CONTINUE
      K       = KK
      INOM    = 0
      IF( IABORT .EQ. 0 )THEN
        IF( IERRF .GT. 0 )THEN
          INOM = 1
        ENDIF
        IERRF = 0
      ENDIF
      CALL WDSET(UNIT,IDN)
      IF( (IDN .EQ. 0) .OR. (IDN .EQ. 1) )
     +  THEN
          IF(INOM.EQ.0)
     +      THEN
              WRITE (102,*)
     +              'READMS,FINDMS - INVALID UNIT NUMBER OR NAME'
            ENDIF
          IF(IABORT.EQ.1) CALL ERREXIT
          IERRF = -1
          RETURN
        ENDIF

      UNITN = 0
      DO 390 I = 1,G@DSNMAXW
      IF(FIT(1,I).EQ.IDN)
     +  THEN
          UNITN = I
          GO TO 395
        ENDIF
390   CONTINUE
395   IF(UNITN.EQ.0)
     +  THEN

C         ERROR  ' UNIT NOT INITIALIZED BY OPENMS '

          IF(INOM.EQ.0)
     +      THEN
              ASSIGN 9014 TO MSG
              WRITE (102,MSG) IDN
            ENDIF
          IF(IABORT.EQ.1) CALL ERREXIT
          IERRF = -15
          RETURN
        ENDIF

      IND   = FIT(11,UNITN)
      IDXPTR= FIT(11,UNITN)
      INDL  = FIT(2,UNITN).AND.MASK(128-63)
      ITYPE = FIT(2,UNITN)
      IF(ITYPE.LT.0)
     +  THEN

C         FILE WILL BE REFERENCED THRU NAMED INDEX.

          IF(K.EQ.0.OR.K.EQ.-0)
     +      THEN

C             ERROR  '  INVALID NAMED INDEX '

              IF(INOM.EQ.0)
     +          THEN
                  ASSIGN 9004 TO MSG
                  WRITE (102,MSG) IDN
                ENDIF
              IF(IABORT.EQ.1) CALL ERREXIT
              IERRF = -6
              RETURN
            ENDIF
          LRA = 0
          LENG = 0

C         SEARCH THE INDEX ARRAY FOR A MATCHING NAME.

          DO 400 I = 1,INDL,2
          IF(IDXUSR(I).EQ.K)
     +      THEN
              LRA = IDXUSR(I+1).AND.MASK(128-42)
              LENG = SHIFTR(IDXUSR(I+1),42)
              K = SHIFTR((I+1),1)
              GO TO 401
            ENDIF
400       CONTINUE

C         HERE WE ARE AT THE END OF THE ARRAY AND NO MATCH  - ISSUE
C         ERROR  ' NAMED RECORD NOT FOUND IN INDEX ARRAY '

              IF(INOM.EQ.0)
     +          THEN
                  ASSIGN 9009 TO MSG
                  WRITE (102,MSG) K, IDN
                ENDIF
              IF(IABORT.EQ.1) CALL ERREXIT
              IERRF = -10
              RETURN
401       CONTINUE
        ELSE

C         FILE WILL BE REFERENCED THRU NUMBERED INDEX.

          IF(K.LE.0)
     +      THEN

C             ERROR  '  INDEX NUMBER .LE. 0 ON NUMBERED INDEX'

              IF(INOM.EQ.0)
     +          THEN
                  ASSIGN 9020 TO MSG
                  WRITE (102,MSG) IDN
                ENDIF
              IF(IABORT.EQ.1) CALL ERREXIT
              IERRF = -19
              RETURN
            ENDIF
          IF(K.GT.INDL)
     +      THEN

C             ERROR  ' INDEX NUMBER .GT. MAXIMUM '

              IF(INOM.EQ.0)
     +          THEN
                  ASSIGN 9006 TO MSG
                  WRITE (102,MSG) IDN
                ENDIF
              IF(IABORT.EQ.1) CALL ERREXIT
              IERRF = -8
              RETURN
            ENDIF
          LRA  = IDXUSR(K).AND.MASK(128-42)
          LENG = SHIFTR(IDXUSR(K),42)
        ENDIF

      NN    = N
      IF(NN.GT.LENG) NN = LENG
      IF(LRA.LE.0)
     +  THEN
          IF(INOM.EQ.0)
     +      THEN
              ASSIGN 9018 TO MSG
              WRITE (102,MSG) IDN, K
            ENDIF
          IF(IABORT.EQ.1) CALL ERREXIT
          IERRF = -17
          RETURN
        ENDIF

      IF(NN.LE.0)
     +  THEN
          IF(INOM.EQ.0)
     +      THEN
              ASSIGN 9019 TO MSG
              WRITE (102,MSG) IDN, K
            ENDIF
          IF(IABORT.EQ.1) CALL ERREXIT
          IERRF = -18
          RETURN
        ENDIF

      IF(FIND.EQ.1)
     +  THEN
          CALL SEEK(UNIT,LRA,NN,IERR)
          IF(IERR.EQ.1)
     +      THEN
              IF(INOM.EQ.0)
     +          THEN
                  ASSIGN 9023 TO MSG
                  WRITE (102,MSG) IDN
                ENDIF
              IF(IABORT.EQ.0) IERRF = -22
            ENDIF
          RETURN
        ENDIF
      IT1   = IRTC()
      CALL GETWA(UNIT,IFWA,LRA,NN,IERR)
      ITIME = IRTC() - IT1

      MAXREC = FIT(7,UNITN).AND.MASK(128-28)
      MINREC = FIT(8,UNITN).AND.MASK(128-28)
      IF(N.GT.MAXREC) FIT(7,UNITN) =(FIT(7,UNITN).AND.MASK(36)) + NN
      IF(N.LT.MINREC) FIT(8,UNITN) =(FIT(8,UNITN).AND.MASK(36)) + NN
      FIT(6,UNITN) = FIT(6,UNITN) + ITIME
      IUP2 = SHIFTL(1,46)
      IUP3 = 0
      IF(FIT(10,UNITN).GE.0)
     +  THEN
          IF((FIT(10,UNITN).AND.MASK(128-32))+1.EQ.K)IUP3=SHIFTL(1,46)
        ENDIF
      FIT(9,UNITN) = FIT(9,UNITN) + IUP2 + N
      FIT(8,UNITN) = FIT(8,UNITN) + IUP3
      FIT(10,UNITN) = K
      RETURN
CDIR$ EJECT
CC    CLOSMS ENTRY
C
C     CALL CLOSMS(UNIT,IERRF,ISTAT)
C
C     WHERE:  UNIT = UNIT NUMBER FROM 0 TO 99 OR FILE NAME TO CLOSE.
C
C            IERRF = OPTION ERROR STATUS RETURN.
C                    IF PARAMETER PASSED, ERROR STATUS
C                    WILL BE RETURNED TO THE USER.  IF
C                    IERRF IS NOT USED PROGRAM WILL ABORT.
C
C                    ON INPUT, IF IERRF > 0 ONLY ERROR
C                    NUMBER RETURNED, NO LOGFILE MESSAGE
C                    POSTED.
C
C                    ON RETURN, IF IERRF < 0, ERROR HAS
C                    BEEN ENCOUNTERED
C
C            ISTAT = IF ZERO IS PASSED, THEN STATS ARE TURNED OFF.
C
C        ERROR MESSAGES:
C
C           INVALID UNIT NUMBER OR NAME;        IERRF = -1
C           OPENMS NOT CALLED;                  IERRF = -15
C
C
C     FLOW:
C
C     1. IF BAD UNIT NUMBER OR NAME -  ABORT.
C        IF DATASET NOT OPENED BY CALL TO OPENMS - ABORT.
C     2. IF DATASET NEVER WRITTEN ON SKIP TO 6.
C     3. COMPUTE EOI WORD ADDRESS, THE INDEX ARRAY ADDRESS, AND
C        THE LENGTH OF THE INDEX ARRAY FOR THIS DATASET.
C     4. REWRITE THE FIRST FIVE WORDS OF THE DATASET.  THE FIRST
C        WORD WILL CONTAIN THE WORD ADDRESS OF THE INDEX ARRAY,
C        AND THE SECOND WORD WILL CONTAIN THE LENGTH OF THE INDEX.
C        WORD 3 WILL HAVE TODAYS DATE, WORD 4 WILL HAVE THE TIME,
C        AND WORD 5 WILL HAVE A CHECKSUM OF FIRST 4 WORDS.
C     5. WRITE THE INDEX ARRAY TO THE EOI WORD ADDRESS.
C     6. CALL WCLOSE. WRITE STATISTICS RECORD ON FILE $STATS, ZERO
C        OUT THE FILE NAME IN THE FIT AND RETURN.
C

      ENTRY CLOSMS(UNIT,IERRF,ISTAT)

      IABORT  = 1
      INOM    = 0
      IF( NUMARG() .GE. 2 )THEN
        IABORT = 0
        IF( IERRF .GT. 0 )THEN
          INOM = 1
        ENDIF
        IERRF = 0
      ENDIF
      CALL WDSET(UNIT,IDN)
      IF( (IDN .EQ. 0) .OR. (IDN .EQ. 1) )
     +  THEN
          IF(INOM.EQ.0)
     +      THEN
              WRITE (102,*) 'CLOSMS - INVALID UNIT NUMBER OR NAME '
            ENDIF
          IF(IABORT.EQ.1) CALL ERREXIT
          IERRF = -1
          RETURN
        ENDIF

      UNITN = 0
      DO 450 I = 1,G@DSNMAXW
      IF(FIT(1,I).EQ.IDN) UNITN = I
450   CONTINUE
      IT1 = 0

      IF(UNITN.EQ.0)
     +  THEN

C         ERROR  ' UNIT NOT INITIALIZED BY OPENMS '

          IF(INOM.EQ.0)
     +      THEN
              ASSIGN 9014 TO MSG
              WRITE (102,MSG) IDN
            ENDIF
          IF(IABORT.EQ.1) CALL ERREXIT
          IERRF = -15
          RETURN
        ENDIF

      IF(FIT(4,UNITN).LT.0)
     +  THEN

C         DATASET WAS WRITTEN ON SO WE MUST UPDATE THE INDEX.
C         FIRST BUILD THE FIVE WORDS WORDS IN THE FIRST PRU.

          IF((FIT(3,UNITN).NE.0).OR.(FIT(12,UNITN).NE.0))THEN
             FIT(2,UNITN) = FIT(3,UNITN)
             FIT(11,UNITN)=FIT(12,UNITN)
          ENDIF
          IEOIA   = FIT(4,UNITN).AND.MASK(128-42)
          IND     = FIT(11,UNITN)
          IDXPTR  = FIT(11,UNITN)
          ISC(1)  = IEOIA
          INDL    = FIT(2,UNITN).AND.MASK(128-63)
          ISC(2)  = INDL
          ISC(3)  = DATE()
          ISC(4)  = CLOCK()

          ICHK    = 0
          DO 1000 I = 1,4
          ICHK    = ICHK + SHIFTR(ISC(I),16)
1000      CONTINUE
          ISC(5)  = ICHK
          ISC(6)  = 0


C         NOW REWRITE THE FIRST SIX WORDS ON THE DATASET.
C         THESE ARE  (1) = WORD ADDRESS OF START OF INDEX.
C                    (2) = LENGTH OF THE INDEX.
C                    (3) = TODAYS DATE.
C                    (4) = TIME.
C                    (5) = CHECKSUM.
C                    (6) = 0 TO SIGNIFY FILE CREATED BY WRITMS.

          IT0 = IRTC()
          CALL PUTWA(UNIT,ISC,1,6,IERR)
          IT1 = IT1 + IRTC() - IT0

C         NOW WRITE THE INDEX AT THE EOI ADDRESS.

          IT0 = IRTC()
          CALL PUTWA(UNIT,IDXUSR(1),IEOIA,INDL,IERR)
          IT1 = IT1 + IRTC() - IT0
          FIT(4,UNITN) = 0
        ENDIF

      IT0 = IRTC()
      CALL WCLOSE(UNIT)
      IT1 = IT1 + IRTC() - IT0
      FIT(6,UNITN) = FIT(6,UNITN) + IT1
      FIT(1,UNITN) = 0

      TOTA   = FIT(6,UNITN) / FLOAT(I@CLKTCK())	! divide by clock rate	

      IRWIP  = SHIFTR(FIT(7,UNITN),46)
      IWEOI  = (SHIFTR(FIT(7,UNITN),28)).AND.MASK(128-18)
      IMAXR  = FIT(7,UNITN).AND.MASK(128-28)
      IWRIT  = IRWIP + IWEOI

      ISEQR  = SHIFTR(FIT(8,UNITN),46)
      ISEQW  = (SHIFTR(FIT(8,UNITN),28)).AND.MASK(128-18)
      IMINR  = FIT(8,UNITN).AND.MASK(128-28)

      IREAD  = SHIFTR(FIT(9,UNITN),46)
      ITOTW  = FIT(9,UNITN).AND.MASK(128-45)

      AVAT = 0.0
      IF((IWRIT + IREAD).NE.0) AVAT = TOTA / (IWRIT + IREAD) * 1000.0
      IF((IREAD + IWRIT).EQ.0) IMINR = 0
      IF (NUMARG().GT.2) THEN
          IF (ISTAT.EQ.0) RETURN
      ENDIF
      WRITE(102,8000) IDN
      WRITE(102,8001) IWRIT + IREAD, IREAD, IWRIT
      WRITE(102,8002) ISEQR, ISEQW, IRWIP, IWEOI
      WRITE(102,8003) ITOTW, IMINR, IMAXR
      WRITE(102,8004) TOTA, AVAT
      RETURN

8000  FORMAT(1H1,'  READMS/WRITMS SUMMARY FOR DATASET ',A8)
8001  FORMAT(1H0,'  TOTAL ACCESSES =    ',I12,'  READS =             '
     2,I8,'  WRITES =            ',I6)
8002  FORMAT(1H0,'  SEQUENTIAL READS =  ',I12,'  SEQUENTIAL WRITES = '
     2 I8,'  REWRITES IN PLACE = ',I6,' WRITES TO EOI = ',I6)
8003  FORMAT(1H0,'  TOTAL WORDS MOVED = ',I12,'  MINIMUM RECORD  =   '
     2,I8,'  MAXIMUM RECORD  = ',I8)
8004  FORMAT(1H0,'  TOTAL ACCESS TIME = ',F12.3,'  SECONDS.  AVERAGE '
     2 ,'ACCESS TIME =  ',F10.3,' MILLISECONDS ')


C     ERROR MESSAGES FOLLOW.

C --- USED BY OPENMS
9000  FORMAT('INDEX LENGTH .LE. 0 ON UNIT = ',A8)

C --- USED BY OPENMS
9001  FORMAT('INDEX LENGTH .GT. USER INDEX LENGTH ON UNIT =', A8)

C --- USED BY OPENMS
9002  FORMAT('DATASET INDEX LENGTH = ',I6,' USER LENGTH = ',I6)

C --- USED BY OPENMS
9003  FORMAT('USER INDEX LENGTH .GT. INDEX LENGTH ON UNIT=', A8)

C --- USED BY WRITMS, READMS AND FINDMS
9004  FORMAT('INVALID NAMED INDEX ON UNIT = ',A8)

C --- USED BY WRITMS
9005  FORMAT('RECORD INDEX ARRAY FULL ON UNIT = ',A8)

C --- USED BY WRITMS, READMS AND FINDMS
9006  FORMAT('INDEX NUMBER .GT. MAXIMUM ON UNIT = ',A8)

C --- USED BY WRITMS
9007  FORMAT('REWRITE RECORD EXCEEDS ORIGINAL ON UNIT =', A8)

C --- USED BY WRITMS
9008  FORMAT('OLD RECORD SIZE = ',I8,'  NEW RECORD SIZE = ',I8)

C --- USED BY READMS AND FINDMS
9009  FORMAT('NAMED RECORD NOT FOUND, NAME = ',A8,' UNIT = ',A8)

C --- USED BY OPENMS
9010  FORMAT('INDEX WORD ADDRESS .LE. 0 UNIT = ', A8)

C --- USED BY OPENMS
9011  FORMAT('INDEX LENGTH .LE. 0 ON UNIT = ', A8)

C --- USED BY OPENMS
9012  FORMAT('CHECKSUM ERROR ON UNIT = ',A8)

C --- USED BY OPENMS
9013  FORMAT('DATASET ALREADY OPEN,  UNIT = ', A8)

C --- USED BY STINDX, WRITMS, READMS, FINDMS AND CLOSMS
9014  FORMAT('OPENMS NOT CALLED ON UNIT = ', A8)

C --- USED BY STINDX
9015  FORMAT('STINDX CALL CANNOT CHANGE INDEX TYPE,  UNIT = ', A8)

C --- USED BY OPENMS
9016  FORMAT('DATASET PROBABLY NOT CREATED BY READMS/WRITMS ', A8)

C --- USED BY OPENMS
9017  FORMAT(' LAST UPDATE ',A8,' TIME ',A8,' FILE LGTH= '
     2 ,I8,' INDX LG= ',I6)

C --- USED BY WRITMS, READMS AND FINDMS
9018  FORMAT('INDEX ENTRY LE 0,  UNIT= ', A8,' INDEX NUMBER = ',I6)

C --- USED BY WRITMS, READMS AND FINDMS
9019  FORMAT('RECORD LENGTH .LE. 0 ON UNIT = ', A8,
     2 ' INDEX RECORD NUMBER = ',I6)

C --- USED BY WRITMS, READMS AND FINDMS
9020  FORMAT('INDEX NUMBER .LE. 0 ON UNIT = ',A8)
C
C ----USED BY OPENMS
9021  FORMAT(' DATASET CREATED BY WRITDR, NOT WRITMS = ',A8)

C ----USED BY WRITMS IN ASYNC MODE.
9022  FORMAT(' WRITE RECORD EXCEEDS PAGE SIZE  ',A8)

C ----USED BY FINDMS
9023  FORMAT(' FINDMS RECORD EXCEEDS PAGE SIZE  ',A8)
C
      END
