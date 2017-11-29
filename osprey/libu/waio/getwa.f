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


       SUBROUTINE GETWA(FIL,DEST,ADDR,COUN,IERR)                        
CDIR$ ID "@(#) libu/waio/getwa.f	92.0	10/08/98 14:57:41"
                                                                        
CC     WORD ADDRESSABLE FILES PACKAGE.                                  
C                                                                       
C      FEATURES:                                                        
C                                                                       
C      1. GETS AND PUTS ANYWHERE ON A FILE. IE A VIRTUAL FILE.          
C                                                                       
C      2. DYNAMIC BUFFER ALLOCATION.                                    
C                                                                       
C      3. DYNAMIC DISK ALLOCATION.  IF WORD ADDRESS OF PUT IS BEYOND    
C         EOI ADDRESS THE INTERVENING DISK SPACED IS BLANKED OUT.       
C                                                                       
C      4. OPTIMIZED BUFFER MANAGEMENT TO MINIMIZE DISK TRAFFIC.         
C                                                                       
C      5. BUFFER SIZES CAN BE VARIED FOR EACH DATASET BY A USER CALL.   
C                                                                       
C      6. READ AHEAD THRU *SEEK* CALLS.                                 
C                                                                       
C      7. ASYNCHRONOUS PUTS THROUGH *APUTWA* CALLS.                     
C                                                                       
C      USER CALLS:                                                      
C                                                                       
C          CALL WOPEN(FILE,BLOCKS,STATS,IERR)                           
C                                                                       
C               WHERE: *FILE* = INTEGER FILE NUMBER TO OPEN. MUST BE    
C                               .GE. 0 AND .LE. 99, OR HOLLERITH NAME   
C                               LEFT JUSTIFIED WITH ZERO FILL.          
C                    *BLOCKS* = INTEGER NUMBER OF PAGES TO USE FOR THIS 
C                               FILE.  MUST BE .GE. 2 AND .LE. 4000.    
C                     *STATS* = FLAG TO TURN ON STATISTICS MONITORING   
C                               FOR THIS FILE. IF NONZERO STATS WILL BE 
C                               DUMPED TO FILE $STATS.                  
C                      *IERR* = ERROR FLAG. IF NEGATIVE SOME ERROR HAS  
C                               OCCURRED.  SEE EXIT OF GETWA/PUTWA.     
C                                                                       
C                                                                       
C          CALL WCLOSE(FILE,IERR)                                       
C                                                                       
C               WHERE:  *FILE* = FILE UNIT NUMBER OR ASCII NAME.        
C                       *IERR* = ERROR FLAG. IF NEGATIVE ERROR FOUND.   
C                                                                       
C                                                                       
C          CALL PUTWA(FILE,SOURCE,ADDR,COUNT,IERR)                      
C                                                                       
C          CALL APUTWA(FILE,SOURCE,ADDR,COUNT,IERR)                     
C                                                                       
C          CALL GETWA(FILE,SOURCE,ADDR,COUNT,IERR)                      
C                                                                       
C               WHERE: *FILE* = FILE UNIT NUMBER/NAME TO READ/WRITE ON. 
C                               MUST BE .GE. 0 AND .LE. 99, OR A NAME.  
C                    *SOURCE* = USER ARRAY THAT WILL RECEIVE OR         
C                               SUPPLY DATA.                            
C                      *ADDR* = WORD ADDRESS OF THE FILE TO RECEIVE     
C                               OR SUPPLY DATA.                         
C                     *COUNT* = COUNT OF THE NUMBER OF WORDS TO         
C                               TRANSFER.                               
C                      *IERR* = ERROR FLAG. IF NEGATIVE ON EXIT         
C                               SOME ERROR HAS OCCURRED.                
C                                                                       
C          CALL SEEK(FILE,ADDRESS,COUNT,IERR)                           
C                                                                       
C              WHERE: *FILE*  = UNIT NUMBER OR ASCII NAME.              
C                  *ADDRESS*  = WORD ADDRESS OF DATA TO READ.           
C                    *COUNT*  = NUMBER OF WORDS TO LOAD.                
C                     *IERR*  = ERROR FLAG. WILL BE 0 AT EXIT IF OK.    
C                                                                       
C                                                                       
C                                                                       
C                                                                       
C                                                                       
C      METHOD:                                                          
C                                                                       
C        THIS WORD ADDRESSABLE PACKAGE USES THE UNBLOCKED RANDOM        
C      DATASET FEATURE OF *COS* TO DO ALL INPUT/OUTPUT.  EACH FILE      
C      IS LINKED TO A DUMMY BLOCKED FILE WHEN IT IS OPENED. THE         
C      PACKAGE USES THE BUFFER THAT THE SYSTEM ASSIGNS TO THE DUMMY     
C      FILE FOR ITS PAGED BUFFERS. TO FACILITATE HANDLING OF LARGE      
C      NUMBERS OF PAGES AND LARGE NUMBER OF FILES THE PAGE TABLE        
C      IS ALSO KEPT IN THE DUMMY BUFFER AREA. THE ADDRESS OF THE        
C      START OF THIS BUFFER IS SAVED AND IS UTILIZED THROUGH THE        
C      *POINTER* STATEMENT OF *CFT*.  EACH FILE HAS A WORD ADDRESS      
C      BUFFER POINTER ARRAY (*WABP*) THAT KEEPS TRACK OF WHAT PAGE      
C      HAS WHAT WORD  ADDRESS IN  IT AND IF THE DATA NEEDS TO BE        
C      BE WRITTEN TO THE DISK OR NOT.  WHEN THE FILE IS CLOSED THE      
C      PAGES  ARE ALL FLUSHED TO DISK AS  NEEDED TO INSURE FILE         
C      INTEGRITY.                                                       
C                                                                       
C                                                                       
CC    TABLES POINTERS AND VARIABLES DESCRIPTIONS.                       
C                                                                       
C                                                                       
CC    FET  -  FILE ENVIRONMENT TABLE.                                   
C                                                                       
C        FORMAT  =  B1MAX WORDS PER DATASET ENTRY DESCRIBED BELOW          
C                   IN COS FORMAT WHERE FIRST ARGUMENT IS THE WORD      
C                   OFFSET, THE SECOND ARGUMENT IS THE BIT OFFSET       
C                   WITHIN THE WORD OF THE START OF THIS PARAMETER,     
C                   AND THE THIRD ARGUMENT IS THE LENGTH IN BITS OF     
C                   THE FIELD FOR THIS PARAMETER.                       
C                                                                       
C        DATASET NAME        =  1,0,64      7 CHARACTER MAX NAME.       
C        DATASET BUSY        =  2,0,1       IF BUSY BIT IS SET.         
C        STATISTICS FLAG     =  2,1,1       IF SET WRITE $STATS FILE    
C        NUMBER OF 512 BLKS  =  2,4,28      BUFFER SIZE IN BLOCKS.      
C        NUMBER OF MISSES    =  3,0,32      *WFIND* FIGURES THIS.       
C        NUMBER OF PUTS      =  3,32,32     USER CALLS TO PUT.          
C        NUMBER OF HITS      =  4,0,32      WORD ADDRESSES FOUND IN BUFF
C        NUMBER OF GETS      =  4,32,32     USER CALLS TO GET.          
C        NUMBER OF PART HITS =  5,0,32      PARTIAL SUCCESS IN WFIND.   
C        CALLS TO WRITE DISK =  5,32,32     CALLS TO *WRITEWA*.         
C        NUMBER OF FLUSHES   =  6,0,32      BUFFER FLUSH CALLS.         
C        CALLS TO READ DISK  =  6,32,32     CALLS TO *READWA*.          
C        EOI BLOCK NUMBER    =  7,0,28      EOI ADDRESS.                
C        NUMBER OF SEEKS     =  7,28,24     USER CALLS TO *SEEK*.       
C                               7,52,12     UNUSED
C        TOTAL WORDS GOT     =  8,0,64      WORD COUNT BUFFERS TO USER. 
C        WRITE MODE          =  9,1,1       IF BIT SET WRITE DONE.      
C        TOTAL WORDS PUT     =  9,2,62      WORD COUNT USER TO BUFFERS. 
C        TOTAL ACCESS TIME   =  10,0,64     ACCESS TIME TO DISK IN CYCLE
C        BLOCKS SEQ. COUNT   =  11,0,32     NUMBER OF SEQ. BLOCKS READ. 
C        LAST BLOCK READ     =  11,32,32    NUMBER OF LAST PHYS BLK READ
C        UNUSED FLAG         =  12,0,32     IF ZERO ALL PAGES ARE USED. 
C        LAST SEQ FLAG       =  12,32,32    AVERAGING READ AHEAD COUNTER
C        DISK READ COUNT     =  13,0,64     WORD COUNT OF PHYSICAL XFERS
C        DISK WRITE COUNT    =  14,0,64     WORD COUNT OF DISK WRITES.  
C        SEEK OVERLFOWS      =  15,0,20     USER COUNT EXCEEDS PAGE SIZE
C        APUTWA OVERFLOWS    =  15,20,20    USER PUTWA CALL EXCEEDS PAGE
C        NUMBER OF APUT CALLS=  15,40,24    USER ASYNC PUTWA CALLS.     
C        LAST BLOCK WRITTEN  =  16,0,64     LAST BLOCK ACTUALLY WRITTEN 
C        LAST BUFFER NUMBER  =  17,0,64     BLOCK NUMBER TO START SEARCH
C        BUFFER ADDRESS      =  18,0,64     ABSOLUTE ADDR OF FILE BUFFER
C                                                                       
C                                                                       
CDIR$  EJECT                                                            
CC     GETWA/PUTWA ENTRY PARAMETERS.                                    
C                                                                       
C                                                                       
C      CALL PUTWA(FILE,DEST,ADDR,COUNT,IERR)                            
C                                                                       
C      CALL GETWA(FILE,SOURCE,ADDR,COUNT,IERR)                          
C                                                                       
C      GETWA  -  GET DATA FROM WORD ADDRESSABLE FILE TO USER.           
C                                                                       
C      PUTWA  -  PUT DATA FROM USER TO WORD ADDRESSABLE FILE.           
C                                                                       
C      ENTRY  -  *FILE*  =  FILE NUMBER OR A HOLLERITH NAME.            
C                                                                       
C                *DEST*  =  DESTINATION ARRAY.  DATA PUT HERE FROM DISK 
C                           ON THE GET CALL.                            
C                                                                       
C                *SOURCE*=  SOURCE OF DATA ON WRITE TO DISK.            
C                                                                       
C                                                                       
C                *ADDR*  =  WORD ADDRESS ON FILE TO READ FROM. STARTS   
C                           WITH 1 NOT 0.                               
C                                                                       
C                *COUNT* =  COUNT OF WORDS TO READ.                     
C                                                                       
C                *IERR*  =  ERROR FLAG.  IF IERR > 0 THEN END OF DATA   
C                           DETECTED ON A GET CALL WILL NOT ISSUE A     
C                           LOG MESSAGE.  IF IERR .LE. 0 THE END OF     
C                           DATA WILL ISSUE LOG MESSAGE.                
C                                                                       
C      EXIT   -  *IERR*  =   0 IF NO ERRORS FOUND.                      
C                        =  -1 IF INVALID UNIT NUMBER FOUND.            
C                        =  -2 IF MAX NUMBER OF FILES EXCEEDED.         
C                        =  -3 IF ATTEMPT TO READ PAST EOI.             
C                        =  -4 IF WORD ADDRESS .LE. 0 ON CALL.          
C                        =  -5 IF WORD COUNT .GT. MAXIMUM.              
C                        =  -6 IF INVALID ASCII FILE NAME.              
C                        =  -7 IF WORD COUNT .LE. 0.                    
C                                                                       
C      CALLS  -  *WINIT*   TO INITIALIZE THE FILE AND CONSTANTS.        
C                                                                       
C                *WRITEWA* TO BLANK OUT THE DISK.                       
C                                                                       
C                *WGETB*   TO GET BUFFERS FROM THE PAGED BUFFER AREA.   
C                                                                       
C                *WFIND*   TO FIND IF ANY PAGES ARE LEFT AROUND WITH    
C                          THE CORRECT ADDRESSES.                       
C                                                                       
C                *WMOVE*   TO MOVE DATA FROM USER AREA TO PAGED BUFFERS.
C                                                                       
C                *WFLUSH*  TO FLUSH THE BUFFERS IN WRITE MODE TO DISK.  
C                                                                       
C      FLOW:                                                            
C                                                                       
C      1.  *GETWA* ENTRY. SET WRITE = .FALSE. GO TO 3                   
C      2.  *PUTWA* ENTRY. SET WRITE = .TRUE.                            
C      3.  CALL *WINIT*.  IF NO ERROR FOUND SKIP TO 4. IF THERE WAS AN  
C          ERROR RETURN TO CALLER UNLESS ERROR IS .LT. 0 AND ARGUMENT   
C          COUNT IS .LT. 5 IN WHICH CASE WE WILL ABORT.                 
C      4.  IF NOT WRITING TO DISK SKIP TO 16.                           
C      5.  IF FIRST BLOCK TO WRITE ON IS LESS THAN THE CURRENT          
C          EOI BLOCK + 1 SKIP TO  7.                                    
C      6.  WRITE DISK BLOCKS UNTIL THE GAP BETWEEN CURRENT EOI AND      
C          NEW FIRST IS FILLED IN.                                      
C      7.  IF FIRST BLOCK NUMBER IS .GT. EOI BLOCK NUMBER CALL          
C          *WGETB* TO GET A NEW BLOCK OR BLOCKS. IF FIRST IS            
C          NOT .GT. EOI THAN THE BLOCK IS AN EXISTING ONE AND           
C          WE CALL *WFIND* TO SEE IF IT IS STILL AROUND.                
C      8.  CALL *WMOVE* TO MOVE THE DATA TO THE BLOCK BUFFER.           
C      9.  IF RECALL = .TRUE. AND BLOCK COUNT NOT EQUAL ZERO            
C          *WFIND* IS CALLED AGAIN TO SEE IF IT CAN FIND ANY            
C          MORE BUFFERS.  IF BLKCNT = 0 OR RECALL = .FALSE. SKIP        
C          TO 11.  RECALL = .TRUE. IS A FLAG FROM WFIND THAT            
C          MEANS WE FOUND SOME BUFFER MATCHES BUT NOT ALL, AND          
C          THE ONES WE FOUND WERE UP AT THE TOP END OF THE BUFFER       
C          POOL.  THE STRATEGY IS TO MOVE THESE AND THEN ON THE         
C          NEXT PASS SEE IF SOME OF THE NEW TARGET BUFFERS ARE IN       
C          THE LOWER PART OF THE BUFFER POOL.                           
C     10.  CALL *WMOVE* TO MOVE DATA TO THE BUFFERS.                    
C     11.  IF BLOCK COUNT IS ZERO SKIP TO 29.                           
C     12.  CALL *WGETB* TO GET BLOCKS TO WRITE ON.                      
C     13.  CALL *WMOVE* TO MOVE DATA TO THE BLOCK BUFFERS.              
C     14.  LOOP TO 12 UNTIL ALL BLOCKS MOVED.                           
C     15   SKIP TO 29.                                                  
C     16.  READ CODE. IF THE LAST BLOCK NUMBER REQUIRED IS .GT.         
C          THE CURRENT EOI THE USER IS READING BEYOND EOI. IF USER      
C          CALLED WITH *IERR* > 0 WE SUPPRESS LOG MESSAGE.  IF          
C          *IERR* .LE. 0 WE PUT OUT LOG MESSAGE SET ERROR CODE          
C          TO -3 AND RETURN.                                            
C     17.  COMPUTE READ AHEAD SEQUENCES.  IF THE BLOCKS TO BE READ      
C          ON THIS CALL ARE NOT IN SEQUENCE TO THE LAST READ CALL       
C          SKIP TO 19.                                                  
C     18.  SEQUENTIAL READ.  INCREASE THE SEQUENTIAL READ COUNT         
C          BY THE BLOCK COUNT OF THIS TRANSFER.  SKIP TO 20.            
C     19.  NON SEQUENTIAL READ.  SET SEQUENTIAL READ COUNT TO THE       
C          BLOCK COUNT OF THIS TRANSFER AND ZERO OUT THE READ AHEAD     
C          COUNTER.                                                     
C     20.  SAVE THE LAST BLOCK COUNTER.                                 
C     21.  CALL *WFIND* TO SEE IF ANY OF THE PAGES STILL AROUND.        
C     22.  CALL *WMOVE* TO MOVE THE DATA TO THE USERS BUFFERS.          
C     23.  IF RECALL = .TRUE. AND BLOCK COUNT NOT EQUAL ZERO            
C          *WFIND* IS CALLED AGAIN TO SEE IF IT CAN FIND ANY            
C          MORE BUFFERS.  IF BLKCNT = 0 OR RECALL = .FALSE. SKIP        
C          TO 25.                                                       
C     24.  CALL *WMOVE* TO MOVE THE DATA.                               
C     25.  IF BLKCNT IS ZERO WE ARE DONE - SKIP TO 29                   
C     26.  CALL *WGETB* TO GET MORE BUFFERS.                            
C     27.  CALL *WMOVE* TO MOVE THE DATA.                               
C     28.  LOOP TO 26 UNTIL DONE.                                       
C     29.  RETURN.                                                      
                                                                        
                                                                        
CDIR$ EJECT                                                             
                                                                        
       IMPLICIT INTEGER (A-Z)                                           
       DIMENSION DEST(COUN),SOURCE(COUN)                                
                                                                        
       INCLUDE "wavars.fh"
                                                                        
       LOGICAL RECALL                                                   
       DATA FETPTR /0/
      SAVE INDEXL                                                       
       DATA INDEXL/0/                                                   
       DATA MSK/777777777777777777777B/                                     
       DATA SIGNBIT/1000000000000000000000B/                            
C                                                                       
C      ENTRY FOR GET - SET WRITE FALSE AND PROCEED                      
C                                                                       
       READ  = .TRUE.                                                   
       WRITE = .FALSE.                                                  
       GO TO 1                                                          
                                                                        
                                                                        
       ENTRY PUTWA(FIL,SOURCE,ADDR,COUN,IERR)                           
       READ  = .FALSE.                                                  
       WRITE = .TRUE.                                                   
                                                                        
1      CONTINUE                                                         
       ADDRESS = ADDR                                                   
       FIND    = 0                                                      
       FILE    = FIL                                                    
       COUNT   = COUN                                                   
       INOM = 0                                                         
       IF(NUMARG().GE.5)THEN                                        
         INOM = 1                                                      
         IERR = 0                                                       
       ENDIF                                                            
       IERR1 = 0                                                        
       CALL WINIT(IERR1,PAGES,1,0)
                                                                        
C      CHECK FOR ERROR FOUND IN INITIALIZE ROUTINE                      
                                                                        
       IF(IERR1 .NE. 0)                                                 
     +   THEN                                                           
           IF(NUMARG().LT.5)                                            
     +       THEN                                                       
               CALL ABORT                                               
             ELSE                                                       
               IERR = IERR1                                             
               RETURN                                                   
             ENDIF                                                      
         ENDIF                                                          
                                                                        
C      NOW SEE WHERE THE BLOCK SHOULD GO.                               
                                                                        
       IF(WRITE)                                                        
     +   THEN                                                           

C          NOW MOVE THE DATA.                                           
                                                                        
           FET(3,INDEX) = FET(3,INDEX) + 1                              
           FET(9,INDEX) = FET(9,INDEX) + COUNT                          
           IF(FIRST.GT.EOIB)                                            
     +       THEN                                                       
               CALL WGETB(0)                                            
             ELSE                                                       
               CALL WFIND(RECALL)                                       
             ENDIF                                                      
           CALL WMOVE(DEST,SOURCE,COUNT)                                
                                                                        
           IF(RECALL.AND.BLKCNT.NE.0)                                   
     +       THEN                                                       
               CALL WFIND(RECALL)                                       
               CALL WMOVE(DEST,SOURCE,COUNT)                            
             ENDIF                                                      
                                                                        
           IF(BLKCNT.NE.0)                                              
     +       THEN                                                       
                                                                        
C              LOOP FOR REST.                                           
                                                                        
               DO 30 I=FIRST,LAST,BUFFERS                               
               CALL WGETB(0)                                            
               CALL WMOVE(DEST,SOURCE,COUNT)                            
30             CONTINUE                                                 
             ENDIF                                                      
                                                                        
         ELSE                                                           
                                                                        
C          READ CODE.                                                   
                                                                        
           IF(LAST.GT.EOIB)                                             
     +       THEN                                                       
                                                                        
C              ERROR - USER IS READING PAST EOI.                        
                                                                        
               IF(INOM.EQ.0)                                            
     +           THEN                                                   
                   ASSIGN 103 TO MESS                                   
                   CALL WDSET(FILE,IDN)                                 
                   WRITE(102,MESS) IDN
                 ENDIF                                                  
               IF(NUMARG().LT.5)CALL ABORT                              
               IERR = -3                                                
               RETURN                                                   
             ENDIF                                                      
                                                                        
C          COMPUTE SEQUENCE LENGTHS FOR READ-AHEAD.                     
                                                                        
           LBR  = FET(11,INDEX).AND.MASK(128-32)                        
           NSEQ = SHIFTR(FET(11,INDEX),32)                              
           IF((START.EQ.LBR).OR.(START.EQ.LBR+1))                       
     +       THEN                                                       
               NSEQ = NSEQ + LAST - LBR                                 
             ELSE                                                       
               NSEQ = LAST - START + 1                                  
               FET(12,INDEX) = FET(12,INDEX).AND.MASK(32)               
             ENDIF                                                      
                                                                        
C          NOW UPDATE THE NUMBER OF  SEQUENTIAL READS AND LAST BLK.     
                                                                        
           FET(11,INDEX) = SHIFTL(NSEQ,32) + LAST                       
                                                                        
           FET(4,INDEX) = FET(4,INDEX) + 1                              
           FET(8,INDEX) = FET(8,INDEX) + COUNT                          
           CALL WFIND(RECALL)                                           
           CALL WMOVE(DEST,SOURCE,COUNT)                                
                                                                        
           IF(RECALL.AND.BLKCNT.NE.0)                                   
     +       THEN                                                       
               CALL WFIND(RECALL)                                       
               CALL WMOVE(DEST,SOURCE,COUNT)                            
             ENDIF                                                      
                                                                        
           IF(BLKCNT.NE.0)                                              
     +       THEN                                                       
                                                                        
C              LOOP FOR REST.                                           
                                                                        
               DO 40 I=FIRST,LAST,BUFFERS                               
               CALL WGETB(0)                                            
               CALL WMOVE(DEST,SOURCE,COUNT)                            
40             CONTINUE                                                 
             ENDIF                                                      
                                                                        
                                                                        
C        END OF READ CODE                                               
                                                                        
         ENDIF                                                          
                                                                        
       RETURN                                                           
                                                                        
103    FORMAT('ATTEMPT TO READ PAST EOI ON DATASET = ',A8)              
                                                                        
CDIR$ EJECT                                                             
                                                                        
CC     SEEK.  -  LOAD WORD ADDRESS INTO BUFFERS ASYNCRONOUSLY.          
C                                                                       
C      CALL SEEK(FILE,ADDRESS,COUNT,IERR)                               
C                                                                       
C      ENTRY:  *FILE*  = FILE NUMBER OR ASCII NAME.                     
C           *ADDRESS*  = WORD ADDRESS TO LOAD.                          
C             *COUNT*  = NUMBER OF WORDS TO LOAD.                       
C              *IERR*  = ERROR FLAG.  IF IERR > 0 ON ENTRY THEN         
C                        SEEK PAST END OF DATA WILL NOT ISSUE AN        
C                        ERROR MSG.  IF .LE. 0 ERR ON EOD WILL          
C                        ISSUE LOG MESSAGE.                             
C                                                                       
C      EXIT  - *IERR*  =   0 IF NO ERRORS FOUND.                        
C                      =   1 IF WORD COUNT TO BIG FOR PAGE BUFFERS.     
C                      =  -1 IF INVALID UNIT NUMBER FOUND.              
C                      =  -2 IF MAX NUMBER OF FILES EXCEEDED.           
C                      =  -3 IF ATTEMPT TO READ PAST EOI.               
C                      =  -4 IF WORD ADDRESS .LE. 0 ON CALL.            
C                      =  -5 IF WORD COUNT .GT. MAXIMUM.                
C                      =  -6 IF INVALID ASCII FILE NAME.                
C                      =  -7 IF WORD COUNT .LE. 0.                      
C                                                                       
C                                                                       
C      FLOW:                                                            
C      1. SET FIND FLAG. SET READ FLAG (FIND IMPLYS READ). SET ERROR 0. 
C      2. CALL *WINIT* TO INITIALIZE VALUES.                            
C      3. CHECK IF ERROR FOUND IN WINIT. IF ERROR .LT. 0 RETURN TO      
C         USER UNLESS ARGUMENT LIST WAS LESS THAN 4.  IF SO ABORT.      
C      4. CHECK IF SEEK WILL GO BEYOND EOD.  IF NOT SKIP TO 5.          
C         IF SEEK BEYOND EOD ISSUE ERROR MESSAGE IF THE ENTRY VALUE     
C         OF IERR .LE. 0. IF NUMBER OF ENTRY ARGUMENTS WAS .LT. 4       
C         ABORT.  OTHERWISE SET IERR =  -3 AND RETURN.                  
C      5. UPDATE SEEK CALLS COUNT IN THE FET TABLE.                     
C      6. CALL *WFLUSH* TO FLUSH BUFFERS TO DISK.                       
C      5. CALL *WGETB* WITH PARAMETER TO TELL IT TO READ DATA TO FRONT  
C         OF THE PAGE TABLE SO THAT USER CAN GET MAXIMUM EFFECT OF SEEK 
C         CALL.                                                         
C      7. EXIT.                                                         
C                                                                       
                                                                        
       ENTRY SEEK(FIL,ADDR,COUN,IERR)                                   
       ADDRESS = ADDR                                                   
       FILE    = FIL                                                    
       COUNT   = COUN                                                   
       FIND    = 1                                                      
       INOM = 0                                                         
       IF(NUMARG().GE.4)THEN                                        
         INOM = 1                                                      
         IERR = 0                                                       
       ENDIF                                                            
       READ    = .TRUE.                                                 
       WRITE   = .FALSE.                                                
       IERR1 = 0                                                        
       CALL WINIT(IERR1,PAGES,1,0)
                                                                        
C      CHECK FOR ERROR FOUND IN INITIALIZE ROUTINE                      
                                                                        
       IF(IERR1 .NE. 0)                                                 
     +   THEN                                                           
           IF(NUMARG().LT.4)                                            
     +       THEN                                                       
               CALL ABORT                                               
             ELSE                                                       
               IERR = IERR1                                             
               RETURN                                                   
             ENDIF                                                      
         ENDIF                                                          
                                                                        
       IF(LAST.GT.EOIB)                                                 
     +   THEN                                                           
                                                                        
C          ERROR - USER IS SEEKING PAST EOI.                            
                                                                        
           IF(INOM.EQ.0)                                                
     +       THEN                                                       
               ASSIGN 103 TO MESS                                       
               CALL WDSET(FILE,IDN)                                     
               WRITE(102,MESS) IDN
             ENDIF                                                      
           IF(NUMARG().LT.4)CALL ABORT                                  
           IERR = -3                                                    
           RETURN                                                       
         ENDIF                                                          
                                                                        
C      FCOUNT       = SHIFTR(FET(7,INDEX),12)                           
       FET(7,INDEX) = FET(7,INDEX)+SHIFTL(1,12)                         
       CALL WFLUSH(1,BUFFERS)                                           
       CALL WGETB(1)                                                    
                                                                        
C      NOW CHECK IF THE BLOCK COUNT OF THIS XFER IS GREATER THAN        
C      THE PAGE TABLE SIZE.  IF SO RETURN IERR = 1 TO INDICATE          
C      THE SEEK WAS TO BIG FOR THE PAGE BUFFERS AND THUS MAY            
C      SERIOUSLY AFFECT I/O PERFORMANCE.                                
                                                                        
       IF(NUMARG().GE.4) IERR   = 0                                     
       IF(BLKCNT.GT.BUFFERS)                                            
     +   THEN                                                           
           IF(NUMARG().GE.4) IERR = 1                                   
           FET(15,INDEX) = FET(15,INDEX) + SHIFTL(1,44)                 
         ENDIF                                                          
       RETURN                                                           
C                                                                       
CDIR$  EJECT                                                            
CC     APUTWA ENTRY PARAMETERS.                                         
C                                                                       
C                                                                       
C      CALL APUTWA(FILE,DEST,ADDR,COUNT,IERR)                           
C                                                                       
C      APUTWA  -  ASYNC PUT DATA FROM USER TO WORD ADDRESSABLE FILE.    
C                                                                       
C      ENTRY  -  *FILE*  =  FILE NUMBER OR A HOLLERITH NAME.            
C                                                                       
C                *SOURCE*=  SOURCE OF DATA ON WRITE TO DISK.            
C                                                                       
C                *ADDR*  =  WORD ADDRESS ON FILE TO READ FROM. STARTS   
C                           WITH 1 NOT 0.                               
C                                                                       
C                *COUNT* =  COUNT OF WORDS TO READ.                     
C                                                                       
C                *IERR*  =  ERROR FLAG.  IF IERR > 0 THEN END OF DATA   
C                           DETECTED ON A GET CALL WILL NOT ISSUE A     
C                           LOG MESSAGE.  IF IERR .LE. 0 THE END OF     
C                           DATA WILL ISSUE LOG MESSAGE.                
C                                                                       
C      EXIT   -  *IERR*  =   0 IF NO ERRORS FOUND.                      
C                        =   1 IF NO ERRORS BUT USER PUT WORD           
C                              COUNT TO LARGE TO FIT IN PAGE            
C                              TABLE IN ONE PASS. THUS CALL             
C                              WAS NOT TRULY ASYNCHRONOUS.              
C                        =  -1 IF INVALID UNIT NUMBER FOUND.            
C                        =  -2 IF MAX NUMBER OF FILES EXCEEDED.         
C                        =  -3 IF ATTEMPT TO READ PAST EOI.             
C                        =  -4 IF WORD ADDRESS .LE. 0 ON CALL.          
C                        =  -5 IF WORD COUNT .GT. MAXIMUM.              
C                        =  -6 IF INVALID ASCII FILE NAME.              
C                        =  -7 IF WORD COUNT .LE. 0.                    
C                                                                       
C                                                                       
C      CALLS  -  *WINIT*   TO INITIALIZE THE FILE AND CONSTANTS.        
C                                                                       
C                *WRITEWA* TO BLANK OUT THE DISK.                       
C                                                                       
C                *WGETB*   TO GET BUFFERS FROM THE PAGED BUFFER AREA.   
C                                                                       
C                *WFIND*   TO FIND IF ANY PAGES ARE LEFT AROUND WITH    
C                          THE CORRECT ADDRESSES.                       
C                                                                       
C                *WMOVE*   TO MOVE DATA FROM USER AREA TO PAGED BUFFERS.
C                                                                       
C                *WFLUSH*  TO FLUSH THE BUFFERS IN WRITE MODE TO DISK.  
C                                                                       
                                                                        
       ENTRY APUTWA(FIL,SOURCE,ADDR,COUN,IERR)                          
                                                                        
       READ  = .FALSE.                                                  
       WRITE = .TRUE.                                                   
       ADDRESS = ADDR                                                   
       FIND    = 1                                                      
       FILE    = FIL                                                    
       COUNT   = COUN                                                   
       INOM = 0                                                         
       IF(NUMARG().GE.5)THEN                                        
         INOM = 1                                                      
         IERR = 0                                                       
       ENDIF                                                            
       IERR1 = 0                                                        
       CALL WINIT(IERR1,PAGES,1,0)
                                                                        
C      CHECK FOR ERROR FOUND IN INITIALIZE ROUTINE, AND IF SO SEE       
C      IF IT IS INFORMATIVE (LT 100) OR SERIOUS (GE 100).               
                                                                        
       IF(IERR1 .NE. 0)                                                 
     +   THEN                                                           
           IF((IERR1 .GE. 100) .AND. (NUMARG() .LT. 5))                 
     +       THEN                                                       
               CALL ABORT                                               
             ELSE                                                       
               IERR = IERR1                                             
               RETURN                                                   
             ENDIF                                                      
         ENDIF                                                          
                                                                        
C      UPDATE COUNTERS.                                                 
                                                                        
       FET(15,INDEX) = FET(15,INDEX) + 1                                
       FET(9,INDEX)  = FET(9,INDEX)  + COUNT                            
       BK            = MIN0(BUFFERS,BLKCNT)                             
       WADD1         = BSIZE * (FIRST - 1) + 1                          
       K             = BK                                               
                                                                        
C      NOW SEE IF BY CHANCE USER HAS ALL THE BLOCKS HE NEEDS IN CORE.   
                                                                        
       DO 110 I = 1,K                                                   
       IF(IXMM@(WABP,BUFFERS,MSK,WADD1).NE.0)BK = BK - 1                 
110    WADD1 = WADD1 + BSIZE                                            
                                                                        
       IF(BK.NE.0)                                                      
     +   THEN                                                           
                                                                        
C          USER DID NOT HAVE THEM. FLUSH BUFFERS AND ASSIGN NEW ONES.   
                                                                        
           CALL WFLUSH(1,BUFFERS)                                       
           CALL WGETB(1)                                                
         ENDIF                                                          
       CALL WMOVE(DEST,SOURCE,COUNT)                                    
                                                                        
       IF(BLKCNT.EQ.0)                                                  
     +   THEN                                                           
                                                                        
C          BLKCNT .EQ. 0 MEANS ALL BLOCKS WERE MOVED IN ONE PASS.       
C          THIS MEANS WE WERE ABLE TO DO THE XFER ASYNCHRONOUSLY.       
                                                                        
           IF(NUMARG().GE.5) IERR = 0                                   
           CALL WFLUSH(0,BUFFERS)                                       
           RETURN                                                       
         ENDIF                                                          
                                                                        
                                                                        
C      USER IS MOVING MORE DATA THAN HE HAS BLOCKS. IN THIS CASE THE    
C      LAST XFER ONLY WILL BE DONE ASYNCHRONOUSLY.                      
                                                                        
       IF(NUMARG().GE.5) IERR = 1                                       
       FET(15,INDEX) = FET(15,INDEX) + SHIFTL(1,24)                     
       DO 120 I=FIRST,LAST,BUFFERS                                      
       CALL WGETB(0)                                                    
       CALL WMOVE(DEST,SOURCE,COUNT)                                    
120    CONTINUE                                                         
       CALL WFLUSH(0,BUFFERS)                                           
       RETURN                                                           
       END                                                              
