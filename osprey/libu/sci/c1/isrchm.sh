*
*
*  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
*
*  This program is free software; you can redistribute it and/or modify it
*  under the terms of version 2.1 of the GNU Lesser General Public License 
*  as published by the Free Software Foundation.
*
*  This program is distributed in the hope that it would be useful, but
*  WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
*
*  Further, this software is distributed without any warranty that it is
*  free of the rightful claim of any third person regarding infringement 
*  or the like.  Any license provided herein, whether implied or 
*  otherwise, applies only to this software file.  Patent licenses, if
*  any, provided herein do not apply to combinations of this program with 
*  other software, or any other product whatsoever.  
*
*  You should have received a copy of the GNU Lesser General Public 
*  License along with this program; if not, write the Free Software 
*  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
*  USA.
*
*  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
*  Mountain View, CA 94043, or:
*
*  http://www.sgi.com
*
*  For further information regarding this notice, see:
*
*  http://oss.sgi.com/projects/GenInfo/NoticeExplan
*
*

*
* USMID "@(#) libu/sci/c1/isrchm.sh	92.0	10/08/98 14:57:41"
*-------------------------------------------------------------------
*\Documentation:
*
*\Name:  ISRCHMMA
*
*\Description:
*     This macro performs the actual search for the first modified
*     value that has a specified relation to the target.  That 
*     relation is determined by the routines that call this macro.
*     These routines are ISRCHMEQ, ISRCHMGE, ISRCHMGT, ISRCHMLE,
*     ISRCHMLT, and ISRCHMNE.  By "modified value", it is meant
*     that this algorithm is looking at subfields of the array 
*     elements by performing a right shift on the array value and
*     then masking it.  This allows one to compare the values of
*     subfields of the array elements with that of the target.
*     A1 ... ( input )  Address of N, the size of the problem.
*     A2 ... ( input )  Address of the array being searched.
*     A3 ... ( input )  Address of the stride through the input array.
*     A4 ... ( input )  Target being searched for.
*     A5 ... ( input )  Address of the MASK chosen by the user.
*     A6 ... ( input )  Address of the amount that the array value
*                       is to be right-shifted.
*     S1 ... ( output ) Index of the first element that has a true
*                       relation to the target as defined by A4.
*
*\Usage:
*     ISRCHMMA
*
*\Arguments:
*
*\Register arguments:
*     A1, A2, A3, A4, A5, A6
*     S5
*
*\Modified registers:
*     All A-registers
*     All S-registers
*     All V-regiters
*
*\Remarks:
*
*\Examples:
*
*\Enddoc
*
*---------------------------------------------------------------------
*
*\Lib
*
*\Local variables:
*
*\Author:
*      Barrie Kletscher
*      Mathematical Software Group
*      Cray Research, Inc.
*
*\References:
*
*\Keywords:
*      search, table lookup
*
*\Macros called:
*      CHECK - What this macro does depends on the routine that invokes
*              this macro.  In some of the routines, this subtracts
*              value in S register from values in a vector register
*              and puts the results in another vector register.
*              In others, an addition is performed.  It all depends
*              on what the routine is trying to accomplish.
*      LZCNT - This macro gets the leading zero count of the vector
*              mask register.
*      NEGATARG - In some of the ISRCHM routines, the target is
*                 negated.  In others, this is a no-op.
*      SETVM - Sets the vector mask.
*      VMTOS - On a YMP, one does SVM <-- VM.  This macro does that
*              step.  If the code is being executed on a Y16, it
*              performs the additional step of doing SVM1 <-- VM1.
*
*\Revision history:
*      02 Jan  91    Original version
*
*\Performance data:
*
*\Endlib
*---------------------------------------------------------------------
*
         MACRO
         ISRCHMMA
CRAY16   IFE       MAX$VL,NE,64
         S5        <VBIT          MASK FOR VL CHECK
         S1        0              RETURN VALUE FOR N <= 0
         A1        ,A1            N
         A3        ,A3            INC
         S4        ,A4            TARGET
         S2        ,A5            MASK
         S3        ,A6            RIGHT
         A6        VLEN           CONSTANT FOR SHORT VECTOR CHECK
         A0        -A1            -N
         A5        -A1            -N
         A7        A1*A3          N * INC
         NEGATARG  S4
         JAP       RETURN         RETURN IF N <= 0
         A0        A3             INC
         A7        A7-A3          (N * INC) - INC = (N - 1) * INC
         S6        A5             -N
         $IF       AM             IF INC < 0
           A2        A2-A7        ADJUST ADDRESS FOR INC < 0
         $ENDIF
         A0        A6-A1          VLEN - N
         S7        #S6&S5         REMAINDER MODULO VLEN (-1)
         A7        S7             REMAINDER MODULO VLEN (-1)
         A7        A7+1           REMAINDER MODULO VLEN
         JAM       NOTSHORT       JUMP IF N > VLEN
         A0        A2             ARRAY ADDRESS
         VL        A1             VECTOR LENGTH = N
         V0        ,A0,A3         LOAD ARRAY
         A6        S3             GET RIGHT SHIFT
         V1        V0>A6          SHIFT TO LOWER BITS
         V2        S2&V1          ISOLATE BITS FOR THE TEST
         A4        A1             INDEX INTO ARRAY
         A7        A1             LAST VL VALUE
         CHECK     V3,S4,V2       COMPARE ARRAY AGAINST TARGET IN S4
         SETVM     V3             SET VM WHERE CONDITION IS TRUE
DONE     =         *              N EXHAUSTED
         VMTOS     S1,S7
         S0        S1!S7          VM!VM1
         A5        0              2ND LAST VL NOT NEEDED
         JSN       FOUND          FOUND TARGET IF VM <> 0
         A1        A1+1           N+1
         S1        A1             RETURN N+1
         J         RETURN
NOTSHORT =         *              N > VLEN
         A0        A2             ARRAY ADDRESS
         VL        A7             REMAINDER
         V0        ,A0,A3         LOAD ARRAY REMAINDER
         A4        S3             GET RIGHT SHIFT
         V1        V0>A4          SHIFT TO LOWER BITS
         V2        S2&V1          ISOLATE BITS FOR THE TEST
         A4        A7             INITIAL INDEX INTO ARRAY
         A6        A3*A7          START CALCULATING NEW ARRAY ADDRESS
         A5        A7             ORIGINAL VL
         A7        VLEN           NEW VECTOR LENGTH
         CHECK     V3,S4,V2       COMPARE ARRAY AGAINST TARGET IN S4
         A2        A2+A6          UPDATE ARRAY ADDRESS
         A6        A3*A7          INC * VL (FOR UPDATING ADDRESS)
         SETVM     V3             SET VM WHERE CONDITION IS TRUE
         VL        A7             FROM NOW ON VL=VLEN
LOOP     =         *
         A0        A2             NEW ARRAY ADDRESS
         V4        ,A0,A3         LOAD ARRAY REMAINDER
         S6        A6
         A6        S3             GET RIGHT SHIFT
         V5        V4>A6          SHIFT TO LOWER BITS
         A6        S6
         V6        S2&V5          ISOLATE BITS FOR THE TEST
         CHECK     V7,S4,V6       COMPARE ARRAY AGAINST TARGET IN S4
         VMTOS     S1,S7
         A4        A4+A7          UPDATE INDEX
         SETVM     V7             SET VM WHERE CONDITION IS TRUE
         S0        S7!S1          VM!VM1
         A0        A4-A1          CURRENT INDEX - N
         JSN       FOUND          FOUND ARRAY ELEMENT WITH CONDITION
         JAP       DONE           EXHAUSTED N
         A5        VLEN           2ND LAST VL
         A2        A2+A6          UPDATE ARRAY ADDRESS
         A0        A2
         V0        ,A0,A3         LOAD ARRAY REMAINDER
         S6        A6
         A6        S3             GET RIGHT SHIFT
         V1        V0>A6          SHIFT TO LOWER BITS
         A6        S6
         V2        S2&V1          ISOLATE BITS FOR THE TEST
         CHECK     V3,S4,V2       COMPARE ARRAY AGAINST TARGET IN S4
         VMTOS     S1,S7
         A4        A4+A7          UPDATE INDEX
         SETVM     V3             SET VM WHERE CONDITION IS TRUE
         S0        S7!S1          VM!VM1
         A0        A4-A1          CURRENT INDEX - N
         JSN       FOUND          FOUND ARRAY ELEMENT WITH CONDITION
         JAP       DONE           EXHAUSTED N
         A2        A2+A6          UPDATE ARRAY ADDRESS
         J         LOOP           TRY AGAIN
FOUND    =         *              ARRAY ELEMENT FOUND
         A7        A5+A7          LAST TWO VL VALUES
         LZCNT     A1,S1,S7,A2,A5
         A4        A4-A7          GET RID OF LAST TWO VL VALUES
         A1        A1+1           NUMBER OF ELEMENTS INTO VECTOR
         A4        A4+A1          NUMBER OF ELEMENTS INTO ARRAY
         S1        A4             INDEX INTO ARRAY
RETURN   =         *
CRAY16   ELSE
         S5        <VBIT          MASK FOR VL CHECK
         S1        0              RETURN VALUE FOR N <= 0
         A1        ,A1            N
         A3        ,A3            INC
         S4        ,A4            TARGET
         S2        ,A5            MASK
         S3        ,A6            RIGHT
         A6        VLEN           CONSTANT FOR SHORT VECTOR CHECK
         A0        -A1            -N
         A5        -A1            -N
         A7        A1*A3          N * INC
         NEGATARG  S4
         JAP       RETURN         RETURN IF N <= 0
         A0        A3             INC
         A7        A7-A3          (N * INC) - INC = (N - 1) * INC
         S6        A5             -N
         $IF       AM             IF INC < 0
           A2        A2-A7        ADJUST ADDRESS FOR INC < 0
         $ENDIF
         A0        A6-A1          VLEN - N
         S7        #S6&S5         REMAINDER MODULO VLEN (-1)
         A7        S7             REMAINDER MODULO VLEN (-1)
         A7        A7+1           REMAINDER MODULO VLEN
         JAM       NOTSHORT       JUMP IF N > VLEN
         A0        A2             ARRAY ADDRESS
         VL        A1             VECTOR LENGTH = N
         V0        ,A0,A3         LOAD ARRAY
         A6        S3             GET RIGHT SHIFT
         V1        V0>A6          SHIFT TO LOWER BITS
         V2        S2&V1          ISOLATE BITS FOR THE TEST
         A4        A1             INDEX INTO ARRAY
         A7        A1             LAST VL VALUE
         CHECK     V3,S4,V2       COMPARE ARRAY AGAINST TARGET IN S4
         SETVM     V3             SET VM WHERE CONDITION IS TRUE
DONE     =         *              N EXHAUSTED
         VMTOS     S1,S7
         S0        S1             VM
         A5        0              2ND LAST VL NOT NEEDED
         JSN       FOUND          FOUND TARGET IF VM <> 0
         A1        A1+1           N+1
         S1        A1             RETURN N+1
         J         RETURN
NOTSHORT =         *              N > VLEN
         A0        A2             ARRAY ADDRESS
         VL        A7             REMAINDER
         V0        ,A0,A3         LOAD ARRAY REMAINDER
         A4        S3             GET RIGHT SHIFT
         V1        V0>A4          SHIFT TO LOWER BITS
         V2        S2&V1          ISOLATE BITS FOR THE TEST
         A4        A7             INITIAL INDEX INTO ARRAY
         A6        A3*A7          START CALCULATING NEW ARRAY ADDRESS
         A5        A7             ORIGINAL VL
         A7        VLEN           NEW VECTOR LENGTH
         CHECK     V3,S4,V2       COMPARE ARRAY AGAINST TARGET IN S4
         A2        A2+A6          UPDATE ARRAY ADDRESS
         A6        A3*A7          INC * VL (FOR UPDATING ADDRESS)
         SETVM     V3             SET VM WHERE CONDITION IS TRUE
         VL        A7             FROM NOW ON VL=VLEN
LOOP     =         *
         A0        A2             NEW ARRAY ADDRESS
         V4        ,A0,A3         LOAD ARRAY REMAINDER
         S6        A6
         A6        S3             GET RIGHT SHIFT
         V5        V4>A6          SHIFT TO LOWER BITS
         A6        S6
         V6        S2&V5          ISOLATE BITS FOR THE TEST
         CHECK     V7,S4,V6       COMPARE ARRAY AGAINST TARGET IN S4
         VMTOS     S1,S7
         A4        A4+A7          UPDATE INDEX
         SETVM     V7             SET VM WHERE CONDITION IS TRUE
         S0        S1             VM
         A0        A4-A1          CURRENT INDEX - N
         JSN       FOUND          FOUND ARRAY ELEMENT WITH CONDITION
         JAP       DONE           EXHAUSTED N
         A5        VLEN           2ND LAST VL
         A2        A2+A6          UPDATE ARRAY ADDRESS
         A0        A2
         V0        ,A0,A3         LOAD ARRAY REMAINDER
         S6        A6
         A6        S3             GET RIGHT SHIFT
         V1        V0>A6          SHIFT TO LOWER BITS
         A6        S6
         V2        S2&V1          ISOLATE BITS FOR THE TEST
         CHECK     V3,S4,V2       COMPARE ARRAY AGAINST TARGET IN S4
         VMTOS     S1,S7
         A4        A4+A7          UPDATE INDEX
         SETVM     V3             SET VM WHERE CONDITION IS TRUE
         S0        S1             VM
         A0        A4-A1          CURRENT INDEX - N
         JSN       FOUND          FOUND ARRAY ELEMENT WITH CONDITION
         JAP       DONE           EXHAUSTED N
         A2        A2+A6          UPDATE ARRAY ADDRESS
         J         LOOP           TRY AGAIN
FOUND    =         *              ARRAY ELEMENT FOUND
         A7        A5+A7          LAST TWO VL VALUES
         LZCNT     A1,S1,S7,A2,A5
         A4        A4-A7          GET RID OF LAST TWO VL VALUES
         A1        A1+1           NUMBER OF ELEMENTS INTO VECTOR
         A4        A4+A1          NUMBER OF ELEMENTS INTO ARRAY
         S1        A4             INDEX INTO ARRAY
RETURN   =         *
CRAY16   ENDIF
ISRCHMMA ENDM
