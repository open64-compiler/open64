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
* USMID "@(#) libu/sci/c1/isrch.sh	92.0	10/08/98 14:57:41"
*
         MACRO
         ISRCHMAC
         S5        <VBIT          MASK FOR VL CHECK
         S1        0              RETURN VALUE FOR N <= 0
         A1        ,A1            N
         A3        ,A3            INC
         S4        ,A4            TARGET
         A6        VLEN           CONSTANT FOR SHORT VECTOR CHECK
         A0        -A1            -N
         A5        -A1            -N
         A7        A1*A3          N * INC
         NEGATARG  S4             -TARGET
         JAM       GOODN          JUMP IF N > 0
         J         RETURN         RETURN IF N <= 0
GOODN    =         *
         A0        A3             INC
         A7        A7-A3          (N * INC) - INC = (N - 1) * INC
         S6        A5             -N
         JAP       NPOS           JUMP IF INC >= 0
         A2        A2-A7          ADJUST ADDRESS FOR INC < 0
NPOS     =         *
         A0        A6-A1          VLEN - N
         S7        #S6&S5         REMAINDER MODULO VLEN (-1)
         A7        S7             REMAINDER MODULO VLEN (-1)
         A7        A7+1           REMAINDER MODULO VLEN
         JAM       NOTSHORT       JUMP IF N > VLEN
         A0        A2             ARRAY ADDRESS
         VL        A1             VECTOR LENGTH = N
         V1        ,A0,A3         LOAD ARRAY
         A4        A1             INDEX INTO ARRAY
         A7        A1             LAST VL VALUE
         CHECK     V2,S4,V1       COMPARE ARRAY AGAINST TARGET IN S4
         SETVM     V2             SET VM WHERE CONDITION IS TRUE
DONE     =         *              N EXHAUSTED
         VMTOS     S1,S2
CRAY16   IFE       MAX$VL,NE,64
         S0        S2!S1          VM!VM1
CRAY16   ELSE
         S0        S1             VM
CRAY16   ENDIF
         A5        0              2ND LAST VL NOT NEEDED
         JSN       FOUND          FOUND TARGET IF VM <> 0
         A1        A1+1           N+1
         S1        A1             RETURN N+1
         J         RETURN
NOTSHORT =         *              N > VLEN
         A0        A2             ARRAY ADDRESS
         VL        A7             REMAINDER
         V1        ,A0,A3         LOAD ARRAY REMAINDER
         A4        A7             INITIAL INDEX INTO ARRAY
         A6        A3*A7          START CALCULATING NEW ARRAY ADDRESS
         A5        A7             ORIGINAL VL
         A7        VLEN           NEW VECTOR LENGTH
         CHECK     V2,S4,V1       COMPARE ARRAY AGAINST TARGET IN S4
         A2        A2+A6          UPDATE ARRAY ADDRESS
         A6        A3*A7          INC * VL (FOR UPDATING ADDRESS)
         SETVM     V2             SET VM WHERE CONDITION IS TRUE
         VL        A7             FROM NOW ON VL=VLEN
LOOP     =         *
         A0        A2             NEW ARRAY ADDRESS
         V3        ,A0,A3         LOAD NEW PORTION OF ARRAY
         CHECK     V4,S4,V3       COMPARE ARRAY AGAINST TARGET IN S4
         VMTOS     S1,S2
         A4        A4+A7          UPDATE INDEX
         SETVM     V4             SET VM WHERE CONDITION IS TRUE
CRAY16   IFE       MAX$VL,NE,64
         S0        S2!S1          VM!VM1
CRAY16   ELSE
         S0        S1             VM
CRAY16   ENDIF
         A0        A4-A1          CURRENT INDEX - N
         JSN       FOUND          FOUND ARRAY ELEMENT WITH CONDITION
         JAP       DONE           EXHAUSTED N
         A5        VLEN           2ND LAST VL
         A2        A2+A6          UPDATE ARRAY ADDRESS
         A0        A2
         V5        ,A0,A3         LOAD NEW PORTION OF ARRAY
         CHECK     V6,S4,V5       COMPARE ARRAY AGAINST TARGET IN S4
         VMTOS     S1,S2
         A4        A4+A7          UPDATE INDEX
         SETVM     V6             SET VM WHERE CONDITION IS TRUE
CRAY16   IFE       MAX$VL,NE,64
         S0        S2!S1
CRAY16   ELSE
         S0        S1             VM
CRAY16   ENDIF
         A0        A4-A1          CURRENT INDEX - N
         JSN       FOUND          FOUND ARRAY ELEMENT WITH CONDITION
         JAP       DONE           EXHAUSTED N
         A2        A2+A6          UPDATE ARRAY ADDRESS
         J         LOOP           TRY AGAIN
FOUND    =         *              ARRAY ELEMENT FOUND
         A7        A5+A7          LAST TWO VL VALUES
         LZCNT     A1,S1,S2,A2,A3
         A4        A4-A7          GET RID OF LAST TWO VL VALUES
         A1        A1+1           NUMBER OF ELEMENTS INTO VECTOR
         A4        A4+A1          NUMBER OF ELEMENTS INTO ARRAY
         S1        A4             INDEX INTO ARRAY
RETURN   =         *
ISRCHMAC ENDM
