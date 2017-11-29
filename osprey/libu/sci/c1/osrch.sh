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
* USMID "@(#) libu/sci/c1/osrch.sh	92.0	10/08/98 14:57:41"
*
*-------------------------------------------------------------------
*\Documentation:
*
*\Name:  OSRCHMAC
*
*\Description:
*     This macro is called by OSRCHI and OSRCHF.
*     It returns the index of the first location in an
*     ordered array such that the corresponding value is
*     equal to the target.  It also returns the hypothetical
*     location of the target in the array should the target
*     be added to the array such the array still remains
*     ordered.
*     A1 ... ( input )  N, the size of the problem.
*     A2 ... ( output ) Address of where the target should be located
*                       should it be added to the the sorted array.
*     A3 ... ( input )  The stride through the input array.
*     A4 ... ( output ) Address of the NUMBER parameter.  This 
*                       parameter will receive the number of values
*                       that are equal to the target.
*     A5 ... ( output ) Address of INDEX.  It is this variable which 
*                       be the recipient of the location of the first 
*                       array element to be equal to the target.
*     S1 ... ( output ) Index of the first element that has a true
*                       relation to the target as defined by A4.
*     S4 ... ( input )  Target being searched for.
*
*\Usage:
*     OSRCHMAC
*
*\Arguments:
*     B-registers
*       C90SVA
*       INDX
*       NMBR
*       WHR
*       
*\Register arguments:
*     A1
*     S0, S2, S4
*     V2
*
*\Modified registers:
*     A0, A1, A2, A3, A4, A5, A6, A7
*     S0, S1, S2, S3, S4, S5, S6, S7
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
*     Barrie Kletscher
*     Mathematical Software Group
*     Cray Research, Inc.
*
*\References:
*
*\Macros called:
*     CHECK - What this macro does depends on the routine that invokes
*             this macro.  In some of the routines, this subtracts
*             value in S register from values in a vector register
*             and puts the results in another vector register.
*             In others, an addition is performed.  It all depends
*             on what the routine is trying to accomplish.
*     COMPARE - Compare first true value with the target.
*     LZCNT - This macro gets the leading zero count of the vector
*             mask register.
*     TARGET - Negates the target.
*     VMTOS - On a YMP, one does SVM <-- VM.  This macro does that
*             step.  If the code is being executed on a Y16, it
*             performs the additional step of doing SVM1 <-- VM1.
*
*\Keywords:
*     ordered search
*
*\Routines called:
*     None
*
*\Revision history:
*     02 Jan 91       orginal version
*
*\Performance data:
*
*\Endlib
*---------------------------------------------------------------------
*
         MACRO
         OSRCHMAC
         A6        PART           VL FOR LONG N
         VL        A6             PRESET VL IF NEEDED
         A6        VLEN           NEEDED FOR SHORT VECTOR CHECK
         S3        1              SET LOWER BIT FOR MASK AND NUMBER ONE
         S2        +A1            N
         S5        +A1            N
         A7        A1*A3          CALCULATE TOTAL BLOCK
         S2        S2>SHIFT       N/16
         A0        A3             CHECK FOR NEGATIVE SKIP INCREMENT
         S2        S2-S3          (N/16) - 1
         TARGET    S4             TARGET MACRO
         S0        -S5            -N
         A7        A7-A3          READJUST TOTAL BLOCK FOR LAST ELEMENT
         JAP       INCPOS         INC < 0
         A2        A2-A7          FIX ARRAY STARTING ADDRESS
INCPOS   =         *
         A0        A6-A1          VLEN-N
         B.ORGN    A1             ORIGINAL N VALUE
         S2        S3!S2          MAKE SURE LOWER BIT SET (ODD)
         A5        S2             SKIP INCREMENT
         A6        A3*A5          ADDRESS SKIP INCREMENT
         A4        0              INDEX INTO ARRAY
         JSM       GOODN          N > 0
         A1        B.INDX         ADDRESS OF INDEX
         ,A1       A4             STORE WHERE FOUND (0)
         A5        B.WHR          ADDRESS OF IWHERE
         ,A5       A4             STORE WHERE SHOULD BE (0)
         J         RETURN
GOODN    =         *
         JAP       SHORTN         N <= VLEN
*REPEAT 2
AGAIN    =         *
         A7        A6-A3          ADJUST FOR FIRST ELEMENT
         B.INDINC  A5             LAST INDEX SKIP INCREMENT
         A0        A2+A7          ADDRESS OF ARRAY
         S3        1              SET LOWER BIT FOR MASK AND NUMBER 1
         V1        ,A0,A6         LOAD ARRAY ELEMENTS
         S2        S2>SHIFT       DIVIDE LAST SKIP INCREMENT BY 16
         S2        S2-S3          (INC/16) - 1
         CHECK     V2,S4,V1       CHECK AGAINST TARGET
         B.ADDINC  A6             LAST ADDRESS SKIP INCREMENT
         S2        S3!S2          MAKE SURE LOWER BIT IS SET (ODD)
         S0        S2             MAKE SURE NEW INC IS NOT ZERO
         VM        V2,P           VM SET IF TARGET >=
         VMTOS     S1,S6
         LZCNT     A1,S1,S6,A2,A7
CRAY16   IFE       MAX$VL,NE,64
         B.C90SVA  A2
         A7        PS1
         A2        PS6
         A0        A2+A7          CHECK IF FOUND IN THIS RANGE
         A2        B.C90SVA
CRAY16   ELSE
         A0        PS1            CHECK IF FOUND IN THIS RANGE
CRAY16   ENDIF
         A6        A6*A1          LAST ADDRESS SKIP INCREMENT TIMES WHER
         A7        A5*A1          LAST INDEX SKIP INCREMENT TIMES WHERE
         A5        S2             NEW CALCULATE SKIP INCREMENT
         A1        B.INDINC       LAST INDEX SKIP INCREMENT
         JAZ       BOTTOM         NOT FOUND IN THIS RANGE
         A2        A2+A6          UPDATE ADDRESS
         A6        A3*A5          ADDRESS SKIP INCREMENT
         A4        A4+A7          UPDATE INDEX
         JSP       AGAIN          NEED TO NARROW IT DOWN FURTHER
         A1        A1+1           VL NEEDS TO BE 1 MORE THAT LAST INC
SHORTN   =         *
         A0        A2             NEW ADDRESS
         VL        A1             SET VL TO N
         V1        ,A0,A3         LOAD ARRAY ELEMENTS
         A6        B.ORGN         ORIGINAL N
         A6        A6+1           N+1
         CHECK     V2,S4,V1       CHECK AGAINST TARGET
         VM        V2,P           VM SET IF TARGET =
         A7        B.NMBR         ADDRESS OF 7TH ARGUMENT
         S7        ,A7            GET VALUE OF 7TH ARGUMENT
         S5        0
         VMTOS     S2,S6
         LZCNT     A7,S2,S6,A2,A5
CRAY16   IFE       MAX$VL,NE,64
         B.C90SVA  A2
         A2        PS2            CHECK IF FOUND
         A1        PS6
         A0        A2+A1
         A2        B.C90SVA
CRAY16   ELSE
         A0        PS2            CHECK IF FOUND
CRAY16   ENDIF
         S6        V1,A7          GET FIRST TRUE VALUE
         A7        A7+1           ADJUST TO START COUNTING FROM 1
         A4        A4+A7          POSITION IN ARRAY FROM BEGINNING
         COMPARE   S0,S4,S6       COMPARE FIRST TRUE VALUE WITH TARGET
         A7        A7-1           ADJUST TO START COUNTING FROM 0
         A1        B.INDX         ADDRESS OF INDEX
         A5        B.WHR          ADDRESS OF WHERE
         JSZ       FOUND          FIRST TRUE VALUE = TARGET
         JAZ       NOFIND         FOUND ARRAY ELEMENT = TO TARGET
MISSING  =         *
         ,A1       A6             STORE WHERE FOUND (N+1)
         ,A5       A4             STORE WHERE SHOULD BE
         A2        B.NMBR         ADDRESS OF NUMBER
         ,A2       S5             STORE NUMBER OF EQUAL ELEMENTS (0)
         J         RETURN
FOUND    =         *
         S0        S7             GET 7TH ARGUMENT
         A7        A7*A3          LAST BLOCK UNTIL FIRST TRUE VALUE
         ,A1       A4             STORE WHERE FOUND
         ,A5       A4             STORE WHERE SHOULD BE
         JSZ       RETURN         DO NOT CALCULATE HOW MANY ELEMENTS =
         A2        A2+A7          ADJUST STARTING ADDRESS
         A7        A4-A6          -VL FROM FOUND TO N
         S2        <VBIT          MASK FOR DETERMINING VL
         S1        A7             -VL
         A5        0              NUMBER OF EQUAL ELEMENTS
         S1        #S1&S2         VL-1 < VLEN
         A7        S1             VL-1
         A7        A7+1           INITIAL VL
LOOP     =         *
         A0        A2             ADDRESS
         VL        A7             SET VL
         V1        ,A0,A3         LOAD FROM FIRST EQUAL ELEMENT
         A1        A7*A3          VL * INC
         A4        A4+A7          READJUST INDEX
         CHECK     V2,S4,V1       CHECK AGAINST TARGET
         A2        A2+A1          READJUST ADDRESS
         VM        V2,Z           SET VM FOR EQUAL ELEMENTS
         S2        A7             LAST VL
         A0        A4-A6          INDEX - (N+1)
         VMTOS     S1,S6
         A1        PS1            NUMBER OF EQUAL ELEMENTS THIS PASS
CRAY16   IFE       MAX$VL,NE,64
         A7        PS6            POPULATION COUNT OF VM1
         A1        A1+A7          TOTAL POPULATION COUNT
CRAY16   ENDIF
         A7        VLEN           NEW VL
         S1        A1             NUMBER OF EQUAL ELEMENTS THIS PASS
         A5        A5+A1          TOTAL NUMBER OF EQUAL ELEMENTS
         S0        S1\S2          NUMBER OF EQUAL VALUES EQUAL TO LAST V
         JSN       SET            FOUND ALL EQUAL ELEMENTS
         JAN       LOOP           TRY IT AGAIN
SET      =         *
         A2        B.NMBR         ADDRESS OF NUMBER
         ,A2       A5             STORE NUMBER OF EQUAL ELEMENTS
RETURN   =         *
         EXIT      MODE=USER
NOFIND   =         *
         A1        B.INDX         ADDRESS OF INDEX
         ,A1       A6             STORE WHERE FOUND (N+1)
         A5        B.WHR          ADDRESS OF WHERE
         ,A5       A6             STORE WHERE SHOULD BE (N+1)
         A2        B.NMBR         ADDRESS OF NUMBER
         ,A2       S5             STORE NUMBER OF EQUAL ELEMENTS (0)
         J         RETURN
BOTTOM   =         *
         A5        PART           LAST VL
         A6        B.INDINC       LAST INDEX SKIP INCREMENT
         A6        A5*A6          INDEX BLOCK
         A7        B.ADDINC       LAST ADDRESS SKIP INCREMENT
         A7        A5*A7          ADDRESS BLOCK
         S3        1              SET LOWER BIT FOR MASK AND NUMBER 1
         A1        B.ORGN         ORIGINAL N
         A4        A4+A6          UPDATE INDEX
         A1        A1-A4          NUMBER LEFT TO SEARCH
         S5        0              RETURN VALUE IF NOT FOUND
         A6        VLEN          NEEDED FOR SHORT VECTOR CHECK
         S2        A1             NEW N
         S0        A1             NEW N
         A0        A6-A1          SEARCH FOR SHORT VECTOR
         S2        S2>SHIFT       DIVIDE LAST SKIP INCREMENT BY 16
         A2        A2+A7          UPDATE ADDRESS
         S2        S2-S3          (INC/16) - 1
         A6        B.ORGN         N
         A6        A6+1           N+1
         JSZ       NOFIND
         JAP       SHORTN
         S2        S2!S3          MAKE SURE LOWER BIT IS SET (ODD)
         A5        S2             CALCULATE NEW SKIP INCREMENT
         A6        A3*A5          ADDRESS SKIP INCREMENT
         J         AGAIN
OSRCHMAC ENDM
