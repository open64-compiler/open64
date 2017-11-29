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



       SUBROUTINE WMOVE(DEST,SOURCE,COUN)                               
CDIR$ ID "@(#) libu/waio/wmove.f	92.0	10/08/98 14:57:41"
                                                                        
CC     WMOVE - MOVE WORDS TO/FROM USER.                                 
C                                                                       
C      ENTRY  -  *DEST*  =  DESTINATION ADDRESS IF DOING A READ.        
C                                                                       
C              *SOURCE*  =  SOURCE ADDRESS IF DOING A WRITE.            
C                                                                       
C                *COUN*  =  COUNT OF WORDS TO MOVE.                     
C                                                                       
C      EXIT   -  NONE.                                                  
C                                                                       
C      CALLS  -  NONE.                                                  
C                                                                       
C      METHOD:                                                          
C                                                                       
C            ON ENTRY *FIRST* CONTAINS THE NUMBER OF THE FIRST BLOCK    
C           TO READ/WRITE, AND *LAST* HOLDS THE LAST  BLOCK NUMBER.     
C           *WMOVE*  WILL CALL THE VECTOR SEARCH ROUTINE TO SEE IF A    
C           MATCH TO *FIRST* CAN BE FOUND. IF SO IT  WILL READ/WRITE    
C           DATA FROM/TO THAT BUFFER TO/FROM THE USER. LOOP AND BLOCK   
C           VARIABLES ARE UPDATED AND THE PROCESS IS REPEATED UNTIL     
C           ALL BLOCKS(OR THE MAXI ALLOWED PER PASS) ARE TRANSFERRED    
C           OR THE NEEDED WORD ADDRESS  IS NOT FOUND IN THE *WABP*      
C           ARRAY.  *WMOVE*  BASICALLY  MOVES  WHAT IT CAN AND THEN     
C           QUITS.  MORE THAN ONE CALL MAY  NECESSARY TO DO THE WHOLE   
C           USER XFER.                                                  
C                                                                       
C      FLOW:                                                            
C                                                                       
C      1.  SET *ENDB* TO THE ENDING BLOCK NUMER OR MAXIMUM PER PASS.    
C                                                                       
C      2.  SET *WADD* (THE WORD ADDRESS) FROM CURRENT *FIRST* IE THE    
C          FIRST BLOCK NUMBER.                                          
C                                                                       
C      3.  CALL THE VECTOR SEARCH ROUTINE TO SEARCH FOR A MATCH TO      
C          *WADD* IN THE *WABP* ARRAY.  IF NOT FOUND RETURN.            
C                                                                       
C      4.  SET *PRU* TO OFFSET INTO THE WORD ADDRESS BUFFER FOR THIS    
C          FILE.                                                        
C                                                                       
C      5.  SET *ENDBLK* TO THE ADDRESS OF THE END OF THIS BLOCK OR      
C          TO THE END OF THE TRANSFER WHATEVER IS LESS.                 
C                                                                       
C      6.  IF READING FROM DISK TO USER SKIP TO 9.                      
C                                                                       
C      7.  WE ARE WRITING.  SET FLAG IN FET (WORD 9) TO INDICATE WRITE  
C          DONE ON THIS FILE.  SET THE SIGN BIT FOR THIS BUFFER IN THE  
C          THE *WABP* ARRAY AND THEN MOVE THE DATA FROM USER TO BUFFER  
C          UNTIL BLOCK DONE OR TRANSFER COMPLETE.                       
C                                                                       
C      8.  SKIP TO 10                                                   
C                                                                       
C      9.  READING - TRANSFER DATA FROM BUFFER TO USER UNTIL BLOCK      
C          DONE OR TRANSFER COMPLETE.                                   
C                                                                       
C     10.  INCREASE BLOCK NUMBER AND WORD ADDRESS AND DECREASE          
C          WORD COUNT AND BLOCK COUNT.  IF NOT DONE ALL BLOCKS          
C          OR MAX LOOP TO 2.                                            
C                                                                       
C     11.  RETURN.                                                      
C                                                                       
CDIR$ EJECT                                                             
       IMPLICIT INTEGER (A-Z)                                           
       DIMENSION DEST(COUN),SOURCE(COUN)                                
                                                                        
       INCLUDE "wavars.fh"
                                                                        
       ENDB = FIRST + BUFFERS                                           
       ENDB = MIN0(ENDB,LAST)                                           
       L    = FIRST                                                     
       DO 50 I = L,ENDB                                                 
       WADD   = BSIZE * (FIRST -1) + 1                                  
       ENDBLK = WADD + BSIZE - 1                                        
       IBLOCK = IXMM@(WABP,BUFFERS,MSK,WADD)                             
       IF(IBLOCK.EQ.0) RETURN                                           
                                                                        
       PRU    = BSIZE * (IBLOCK - 1) + 1                                
       PRU    = PRU + (ADDRESS-WADD)                                    
       ENDADD = ADDRESS + COUNT - 1                                     
       ENDBLK = MIN0(ENDBLK,ENDADD)                                     
       PART   = ENDBLK - ADDRESS + 1                                    
       IF(WRITE)                                                        
     +   THEN                                                           
           FET(9,INDEX) = (FET(9,INDEX)).OR.(SHIFTL(1,62))              
           WABP(IBLOCK) = WADD + SIGNBIT                                
           DO 40 J = 1,PART                                             
           WAA(PRU) = SOURCE(MOVED)                                     
           MOVED = MOVED + 1                                            
           PRU = PRU + 1                                                
40         CONTINUE                                                     
         ELSE                                                           
           DO 41 J = 1,PART                                             
           DEST(MOVED) = WAA(PRU)                                       
           MOVED = MOVED + 1                                            
           PRU = PRU + 1                                                
41         CONTINUE                                                     
         ENDIF                                                          
                                                                        
C      UPDATE LOOP VARIABLES                                            
                                                                        
                                                                        
       COUNT   = COUNT - PART                                           
       ADDRESS = ENDBLK + 1                                             
       IBLOCK  = IBLOCK + 1                                             
       FIRST   = FIRST  + 1                                             
       BLKCNT  = BLKCNT - 1                                             
                                                                        
50     CONTINUE                                                         
       RETURN                                                           
       END                                                              
