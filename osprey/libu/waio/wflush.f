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



       SUBROUTINE WFLUSH(IISA,END)
CDIR$ ID "@(#) libu/waio/wflush.f	92.0	10/08/98 14:57:41"

CC     *WFLUSH *  -  FLUSH BUFFERS THAT ARE IN WRITE MODE TO DISK.
C
C      ENTRY  -  *IISA* =  STARTING INDEX OF WORD ADDRESS BUFFER
C                          POINTER ARRAY(*WABP*) TO BEGIN THE FLUSH.
C
C                *END*  =  ENDING INDEX OF *WABP* ARRAY TO FLUSH.
C
C      EXIT   -  NONE.
C
C      CALLS  -  *WRITEWA* TO WRITE TO DISK.
C
C      METHOD:
C
C      FIRST THE WRITE FLAG FROM THE *FET* TABLE IS CHECKED. IF
C      THIS IS NOT SET NO WRITES HAVE BEEN DONE SINCE THE LAST
C      GLOBAL FLUSH SO WE CAN SIMPLY RETURN TO CALLER.  IF WRITES
C      HAVE BEEN DONE WE CALL THE BUFFER SEARCH ROUTINE TO SEE
C      IF THERE ARE ANY BUFFERS IN WRITE MODE IN THE RANGE FROM
C      WABP(ISA) TO WABP(END).  IF NONE ARE FOUND RETURN TO CALLER.
C      NEXT WE CHECK TO SEE IF THIS IS A GLOBAL FLUSH AND IF SO
C      WE CAN CLEAR OUT THE WRITE FLAG IN THE *FET* TABLE. NEXT THE
C      BUFFERS FROM *WABP(ISA)* TO *WABP(END)* ARE LOOKED AT.
C      IF ANY OF THE BUFFERS ARE IN WRITE MODE (INDICATED BY
C      HAVING THE SIGN BIT SET IN THE *WABP* ENTRY) THEY ARE
C      FLUSHED TO DISK.  IF THE BUFFERS CONTAIN LOGICALLY CONTINUOUS
C      ADDRESSES THEY ARE FLUSHED WITH ONE WRITE TO MINIMIZE DISK
C      ACCESSES. THE WRITE FLAGS FOR ALL BUFFERS WILL BE CLEARED
C      WHEN THIS ROUTINE IS EXITED BUT THE DATA IN THE BUFFERS WILL
C      BE INTACT.
C
C      FLOW:
C
C      1. CHECK THE WRITE DONE SINCE THE LAST GLOBAL FLUSH FLAG AND
C         IF CLEAR THERE CAN BE NO BUFFERS IN WRITE MODE SO RETURN.
C
C      2. THERE ARE SOME BUFFERS IN WRITE MODE.  IF THIS IS A FULL
C         FLUSH CLEAR THE WRITE FLAG IN THE *FET* TABLE AND SKIP TO
C         4.
C
C      3. THIS IS NOT A FULL FLUSH. CALL THE VECTOR SEARCH ROUTINE
C         TO SEE IF THERE ARE ANY BUFFERS IN WRITE MODE IN THE RANGE
C         OF THIS FLUSH.  IF NONE FOUND RETURN.
C
C      4. SET UP THE BUFFER SEARCH PARAMETERS.
C
C      5. SET 0 AFTER THE LAST INTERESTED BUFFER ADDRESS TO
C         FORCE A WRITE ON LOOP EXIT.
C
C      6. LOOK AT *WABP* ENTRY.  IF IT IS NOT IN WRITE MODE(POSITIVE)
C         SKIP TO 11.
C
C      7. BUFFER IS IN WRITE MODE.  CLEAR THE SIGN BIT AND PEEK AT
C         THE NEXT ENTRY IN THE *WABP* ARRAY.
C
C      8. IF THE NEXT ENTRY IS ALSO IN WRITE MODE AND THE WORD ADDRESS
C         IT CONTAINS IS CONTIGUOUS SKIP TO 10.
C
C      9. IF HERE WE HAVE FOUND AT LEAST ONE BUFFER TO WRITE OUT.
C         A CALL IS MADE TO *WRITEWA* TO WRITE THIS BUFFER OR
C         BUFFERS OUT TO THE DISK. SKIP TO 11.
C
C     10. WE ARE IN A POOL OF CONTIGUOUS BUFFERS. ADVANCE THE WORD
C         COUNT FOR THE IMPENDING WRITE.
C
C     11. LOOP BACK TO 6 UNTIL ALL BUFFERS EXAMINED.
C
C     12. ALL DONE.  RESET PROPER VALUE IN *WABP(END+1)* AND RETURN.
C

CDIR$  EJECT
       IMPLICIT INTEGER (A-Z)

       INCLUDE "wavars.fh"

C      FIRST CHECK IF ANY WRITE DONE SINCE LAST FULL FLUSH.

       ISA = IISA       ! make it local so as not to change parameter
       IF(SHIFTR(FET(9,INDEX),62).EQ.0) RETURN
       FET(6,INDEX)= FET(6,INDEX) + SHIFTL(1,32)
       ISINC = 0
       IF(ISA.EQ.0)
     +   THEN
           ISA   = 1
           ISINC = 1
         ENDIF

C      NOW CHECK IF THIS IS A FULL FLUSH.  IF NOT CHECK IF THE
C      RANGE IN QUESTION HAS ANY WRITE MODE BUFFERS.

       IF((ISA.EQ.1).AND.(END.EQ.BUFFERS))
     +   THEN
           FET(9,INDEX) = FET(9,INDEX).AND.MASK(128-60)
         ELSE
           IX = IXMM@(WABP(ISA),END-ISA+1,SIGNBIT,SIGNBIT)
           IF(IX.EQ.0)RETURN
         ENDIF

       II          = BSIZE * (ISA - 1) + 1
       ICDK        = BSIZE
       WSAVE       = WABP(END+1)
       WABP(END+1) = 0
       WADD1       = 0

       DO 30 I = ISA,END
       IF(WABP(I).LT.0)
     +   THEN
           WABP(I) = WABP(I).AND.MSK
           I1      = WABP(I)
           IF(WADD1.EQ.0)
     +       THEN
               WADD1 = I1
               II    = BSIZE * (I - 1) + 1
             ENDIF
           I2 = WABP(I+1).AND.MSK
           IF((WABP(I+1).GE.0).OR.((I2 - I1).NE.BSIZE))
     +       THEN
               FET(5,INDEX) = FET(5,INDEX)  + 1
               FET(14,INDEX)= FET(14,INDEX) + ICDK
               IT1          = IRTC()
               CALL WRITEWA(INDEX,WAA(II),WADD1,ICDK,ISINC)
               EOIBT = (WADD1 + ICDK - 1) / BSIZE
               EOIBW = MAX0(EOIBT,EOIBW)
               IF(ISINC.NE.0)FET(2,INDEX)=FET(2,INDEX).OR.SIGNBIT
               FET(10,INDEX) = FET(10,INDEX) + (IRTC() - IT1)
               ICDK  = BSIZE
               WADD1 = 0
             ELSE
               ICDK  = ICDK + BSIZE
             ENDIF

         ENDIF
30     CONTINUE
       WABP(END+1) = WSAVE
       FET(16,INDEX) = EOIBW
       RETURN
       END
