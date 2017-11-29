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



       SUBROUTINE WFIND(RECALL)
CDIR$ ID "@(#) libu/waio/wfind.f	92.0	10/08/98 14:57:41"

CC     WFIND.  -  FIND  ANY BUFFERS THAT MATCH TARGET WORD ADDRESSES.
C
C      WFIND IS CALLED IF THE WORD ADDRESS DESIRED IS LESS THAN EOI
C      WORD ADDRESS.  WFINDS JOB IS TO FIND IF THE PAGES NEEDED ARE
C      STILL IN CORE.
C
C      ENTRY  -  NONE
C
C      EXIT   -  RECALL = .TRUE. IF THE CONTIGUOUS BUFFER RESIDENT 
C                         INITIAL PORTION OF THE REQUEST IS AT THE END OF 
C                         THE BUFFER.  THE CALLING ROUTINE CAN THEN
C                         MOVE THESE PAGES AND THEN CALL *WFIND* AGAIN
C                         TO LOOK FOR MORE PAGES ELSEWHERE IN THE
C                         BUFFER.
C
C                RECALL = .FALSE. IF ABOVE STATEMENT NOT TRUE.
C
C
C      CALLS  -  *WGETB*  TO GET BUFFERS IF NO MATCHS FOUND.
C
C                *WFLUSH* TO FLUSH BUFFERS IF ROOM AT TOP.
C
C                *READWA* TO READ FROM DISK TO BUFFERS.
C
C      METHOD:
C
C        FIRST A GLOBAL SWEEP OF ALL RESIDENT WORD ADDRESSES IS MADE.
C      IF ALL THE NECESSARY WORD ADDRESSES ARE FOUND THE SEARCH IS
C      DONE AND WE EXIT.  IF WE DONT FIND ALL THE WORD ADDRESSES WE
C      THEN SEARCH FOR CONTIGOUS TARGET WORD ADDRESSES STARTING WITH
C      THE FIRST ONE.  IF WE FIND NONE *WGETB* IS CALLED TO GET THE
C      BUFFERS AND WE THEN RETURN.  IF THE BUFFER OR BUFFERS FOUND
C      ARE AT THE END OF THE BUFFER POOL WE SET RECALL = .TRUE. AND
C      THEN WE RETURN.  IF THEY ARE NOT AT END THEN THERE IS ROOM AT
C      THE END FOR SOME OF THE PAGES THAT WE NEED TO FIND.  *WFLUSH*
C      IS CALLED TO FLUSH OUT THE BUFFERS AT THE END OF THE BUFFER
C      POOL.  IF WE ARE WRITING TO THE DISK WE NEED ONLY ENTER THE
C      WORD ADDRESSES FOR THESE NEW BUFFERS IN THE *WABP* ARRAY UNLESS
C      WE ARE DOING THE LAST BUFFER OF THIS  TRANSFER AND IT  IS A
C      PARTIAL BUFFER.  IF IT IS THEN WE MUST PULL THIS BUFFER FROM
C      DISK (ALSO THE LAST BUFFER MUST BE LESS THAN EOI) SO THAT THE
C      NEW DATA CAN BE MERGED WITH THE OLD.  IF WE ARE READING WE READ
C      IN THE BUFFERS IN ONE CALL AND THEN RETURN.
C
C      FLOW:
C
C      1. SET NUMBER OF BLOCKS TO SEARCH FOR.  SET THE RECALL FLAG
C         = .FALSE.
C
C      2. DO A GLOBAL SWEEP OF ALL BUFFERS LOOKING FOR THE NECESSARY
C         WORD ADDRESSES IN ANY ORDER.
C
C      3. IF WE FOUND THEM ALL EXIT.
C
C      4. SEARCH *WABP* ARRAY FOR BLOCK NUMBER TO MATCH FIRST.
C
C      5. IF NO MATCH CALL *WGETB* AND THEN RETURN.
C
C      6. WE HAVE FOUND THE FIRST BLOCK. IF IT IS THE LAST PHYSICAL
C         ONE IN THE BUFFER POOL THAN SKIP TO 10.
C
C      7. LOOK AT NEXT BLOCK.  IF IT DOESNT MATCH THE NEXT DESIRED
C         WORD ADDRESS SKIP TO 10.
C
C      8. DECREMENT BLOCK COUNTER.
C
C      9. IF ALL BLOCKS NOT SEARCHED LOOP TO 7.
C
C     10. WE HAVE FOUND AT LEAST ONE BLOCK. IF THE BLOCK OR BLOCKS
C         WERE THE LAST PHYSICAL ONES IN THE BUFFER POOL SET THE
C         RECALL FLAG = .TRUE. AND RETURN.
C
C     11. WE ARE SHORT BLOCKS AND THERE IS ROOM AT THE END OF THE
C         BUFFER POOL.  IF WE ARE READING RECOMPUTE THE ENDING
C         BLOCK NUMBER BASED ON THE HISTORY OF SEQUENTIAL READING
C         DONE UP TO THIS POINT.  IF THERE IS MORE THAN ONE BLOCK
C         INVOLVED OR IF IT IS ONE BLOCK AND IT IS IN WRITE MODE
C         *WFLUSH* IS CALLED TO FLUSH TO DISK.
C
C     12. PUT THE WORD ADDRESSES OF THE BUFFERS WE NEED IN THE
C         *WABP* ARRAY, MAKING SURE THERE ARE NO DUPLICATE ENTRIES.
C
C     13. IF READING FROM DISK SKIP TO 16.
C
C     14. IF WRITING AND THE LAST BLOCK ADDRESS PUT INTO THE *WABP*
C         ARRAY IS THE LAST BLOCK TO TRANSFER AND IF THIS LAST
C         BLOCK IS NOT BEYOND EOI AND IF THIS LAST BLOCK IS A
C         PARTIAL ONE THEN WE MUST READ IT IN FROM DISK TO MERGE
C         THE OLD AND THE NEW DATA TOGETHER.
C
C     15. SKIP TO 17.
C
C     16. READ IN THE BLOCKS TO THE HIGH BUFFERS.
C
C     17. RETURN.

CDIR$  EJECT
       IMPLICIT INTEGER (A-Z)

       INCLUDE "wavars.fh"

       LOGICAL RECALL

       WABP(BUFFERS+1) = 0
       RECALL = .FALSE.
       BLOCKO = MIN0(BUFFERS,BLKCNT)
       K      = BLOCKO

C      Search for the first BLOCKO file pages in the buffer cache.  
C      If all are present, then we return.   An inital segment of the
C      current request is completely buffer resident in that case.

       WADD1   = BSIZE * (FIRST - 1) + 1
       BK      = K

       DO  5 I = 1,K
           IF (IXMM@(WABP,BUFFERS,MSK,WADD1).NE.0) BK = BK - 1
           WADD1   = WADD1 + BSIZE
5      CONTINUE
       IF(BK.EQ.0) THEN
           FET(4,INDEX) = FET(4,INDEX) + SHIFTL(1,32)
           RETURN
       ENDIF

C      If the large initial segment was not completely buffer resident, then
C      rescan to see if at least the first page is buffer resident.

       WADD1  = BSIZE * (FIRST - 1) + 1
       IBLK   = FIRST
       HIGH   = 0
       LOW    = BUFFERS + 1
       BEGIN  = .FALSE.
       I      = IXMM@(WABP,BUFFERS,MSK,WADD1)

       IF(I.EQ.0) THEN

C          The first word address is not buffer resident.   Call WGETB to 
C          assign a buffer to this page (and those pages which follow?).

           CALL WGETB(0)
           FET(3,INDEX) = FET(3,INDEX) + SHIFTL(1,32)
           RETURN
       ENDIF

       K    = K - 1
       ISA  = I + 1
       IBLK = FIRST+1

C      Find the largest initial contiguous segment which is buffer resident.

       IF (I.NE.BUFFERS) THEN
           DO 10 J = I+1,BUFFERS+1
               WADD1   = WADD1 + BSIZE
               IF ((WABP(J).AND.MSK).EQ.WADD1) THEN
                   K    = K - 1
                   ISA  = J + 1
                   IBLK = IBLK+1
               ELSE
                   GO TO 20
               ENDIF
10         CONTINUE
20         CONTINUE
       ENDIF

C      Here we have found some buffer matches but not all.

       FET(5,INDEX) = FET(5,INDEX) + SHIFTL(1,32)       ! partial hit count
       IF (I.EQ.BUFFERS .OR. J.EQ.(BUFFERS+1)) THEN
           RECALL = .TRUE.                              ! at end of buffer
           RETURN
       ENDIF

C      Check if there's room at the end for more file pages for this request.

       SHORT = K
       END   = ISA + SHORT - 1
       LIM   = ISA + (EOIB-IBLK)
       IF (READ) THEN
           LSTSEQ        = FET(12,INDEX).AND.MASK(128-32)
           LSTSEQ        = (LSTSEQ + SHIFTR(FET(11,INDEX),32)) / 2
           FET(12,INDEX) = FET(12,INDEX).AND.MASK(32) + LSTSEQ
           END           = MAX0(END,ISA + LSTSEQ - 1)
           END           = MIN0(END,LIM)
       ENDIF
       IF (END.GT.BUFFERS) END = BUFFERS

C      Check for dirty blocks between ISA and END inclusive.   If there are
C      any, then flush them to disk.  The signbit indicates dirty buffers.

       IF (END.GT.(ISA+1)) THEN
           IX = IXMM@(WABP(ISA),END-ISA+1,SIGNBIT,SIGNBIT)
           IF(IX.NE.0) CALL WFLUSH(ISA,END)
       ELSE
           IF((WABP(ISA).LT.0).OR.(WABP(END).LT.0))CALL WFLUSH(ISA,END)
       ENDIF

C      Check for trailing pages which are buffer resident in another part of 
C      the buffer.   Invalidate these pages.  We will assign new blocks
C      to these file pages so that the initial segment is contiguous in 
C      the buffer.  

       WADD1   = (WABP(ISA-1).AND.MSK)
       IF ((EOIB.LT.IBLK).AND.WRITE) THEN
           DO 30 I = ISA,END
               WADD1   = WADD1 + BSIZE
               WABP(I) = WADD1
30         CONTINUE
       ELSE
           DO 40 I = ISA,END
               WADD1   = WADD1 + BSIZE
               J       = IXMM@(WABP,BUFFERS,MSK,WADD1)
               IF (J.NE.0) THEN
                   IF (WABP(J).LT.0) THEN
                       CALL WFLUSH(1,BUFFERS)
                   ENDIF
                   WABP(J) = 2                          ! mark page as unused
               ENDIF
               WABP(I) = WADD1
40         CONTINUE
       ENDIF

       IF (WRITE) THEN
           IF (BLKCNT.LE.BUFFERS .AND. PARTEND .AND. WADD1.EQ.LBWA .AND.
     +         LAST.LE.EOIB) THEN

C              We are processing the last block containing the tail of the 
C              requested range of word addresses.  If the request ends between 
C              block boundaries, the block must be pre-read from disk.
C
C              But first flush the rest of the buffer to disk if the block we 
C              are about to read would otherwise be past the end of file.

               IF (LAST.GT.EOIBW) CALL WFLUSH(1,BUFFERS)

               II    = BSIZE * (END - 1) + 1
               FET(6,INDEX) = FET(6,INDEX) + 1      ! cnt of READWA calls 
               FET(13,INDEX)= FET(13,INDEX) + BSIZE ! wrd cnt of phys xfers
               IT1          = IRTC()
               CALL READWA(INDEX,WAA(II),WADD1,BSIZE,0)
               FET(10,INDEX)= FET(10,INDEX)+(IRTC()-IT1) ! I/O time
           ENDIF
       ELSE

C          Read in the following sequence of file blocks.   But first flush 
C          dirty buffers if necessary to extend the disk image to the full 
C          file size.


           WADDR = WABP(ISA)
           II    = BSIZE * (ISA - 1) + 1
           ICDK  = BSIZE * (END - ISA + 1)
           IF (((WADDR+ICDK-1)/BSIZE).GT.EOIBW) CALL WFLUSH(1,BUFFERS)
           FET(6,INDEX) = FET(6,INDEX) + 1          ! cnt of READWA calls
           IT1          = IRTC()
           FET(13,INDEX)= FET(13,INDEX) + ICDK      ! wrd cnt of phys xfers

           CALL READWA(INDEX,WAA(II),WADDR,ICDK,0)
           FET(10,INDEX) = FET(10,INDEX) + (IRTC() - IT1)

       ENDIF

       RETURN
       END
