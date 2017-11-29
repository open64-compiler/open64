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
* USMID "@(#) libu/sci/c1/whenm.sh	92.0	10/08/98 14:57:41"
*
*-------------------------------------------------------------------
*\Documentation:
*
*\Name:  WHENMMAC
*
*\Description:
*     This macro finds locations of elements whose subfields
*     have a specified relationship to the target.  This 
*     relationship is to be determined by the calling routines.
*     These routines are WHENMEQ, WHENMGE, WHENMGT, WHENMLE,
*     WHENMLT, and WHENMNE.  By subfields, it is meant that this
*     algorithm is looking at subfields of the array elements by
*     performing a right shift on the array value and then masking
*     it.  This allows one to compare the values of subfields of the
*     array elements with that of the target.
*     A2       ... ( input )  Address of the amount that the array value
*                             is to be right-shifted.
*     S2       ... ( input )  Address of the MASK chosen by the user.
*     B.A      ... ( input )  Address of the array, A, being searched.
*     B.INCA   ... ( input )  The stride through the input array.
*     B.INDEX  ... ( output ) Array to receive the locations of
*                             modified elements that have a true 
*                             relation to the target.  For WHENMEQ, 
*                             all modified elements that are equal 
*                             to the target.  For WHENMNE, all 
*                             modified elements that are not equal
*                             to the target.
*     B.N      ... ( input )  N, the size of the problem.
*     B.NN     ... ( output ) Number of objects found.
*     T.OBJECT ... ( input )  Target, OBJECT,  being searched for.
*
*\Usage:
*     WHENMMAC
*
*\Arguments:
*     B-registers
*       A
*       INCA
*       N
*       VLENG
*       NN
*       INDEX
*     T-registers
*       OBJECT     
*
*\Register arguments:
*     A1,  A2
*
*\Modified registers:
*     All A-registers
*     S1, S2, S5, S7
*     V0, V1, V3, V4
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
*     COMPUTE - calling this macro.  If the routine is looking
*               for elements that are equal to the target, then
*               COMPUTE will subtract OBJECT from the elements
*               in A and then SETMASK will set the appropriate
*               bit in the vector mask on a zero hit.  In the
*               case of looking for elements that are less than
*               the target, the vector mask looks for a hit on
*               minus.
*     LOADTOBJ - Loads the object being searched in a T-register
*                called OBJECT.
*     LZCNT - This macro gets the leading zero count of the vector
*             mask register.
*     SETMASK - What this does is contingent on the routine calling
*               this macro.  In some, it sets the vector mask on
*               a zero hit.  In others, it sets the vector mask on
*               a minus hit.  It sets the vector mask.  SETMASK
*               and COMPUTE work together.
*     VMTOS - On a YMP, one does SVM <-- VM.  This macro does that
*             step.  If the code is being executed on a Y16, it
*             performs the additional step of doing SVM1 <-- VM1.
*
*\Keywords:
*     search, table lookup 
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
         WHENMMAC
         ARGADD    A1,ARGMASK
         ARGADD    A2,ARGRD
         S2        ,A1
         A2        ,A2
         T.MASK    S2
         B.RD      A2
         A0        B.INCA         CHECK FOR NEG INCREMENT
         A1        B.INCA
         A1        -A1
         $IF       AM
           A2        B.N
           A2        A2-1         (N-1)
           A3        A1*A2        ABS(INCA)*(N-1)
           A4        B.A          BASE ADDRESS OF A
           A4        A4+A3        (N-1)*ABS(INCA)
           B.A       A4           STORE IN B REGISTER
         $ENDIF
         LOADTOBJ
         A1        B.N
         A0        -A1            -VL
         A3        -A1
         S2        <VBIT          MASK
         S1        A3
         JAP       DONE           ZERO VECTOR LENGTH
         S1        #S1&S2
         A3        S1
         A3        A3+1           FIRST SEG LENGTH
         VL        A3
*
         A1        B.A
         A2        B.INCA
         A7        0
         B.I       A7
         A0        A1
         V0        ,A0,A2
         S1        T.MASK
         A7        B.RD
         V1        V0>A7
         V3        S1&V1          VALUES ISOLATED FOR THE TEST
         A2        A2*A3          HOW MUCH TO INCREASE THE ADDRESS
         S1        T.OBJECT
         COMPUTE   V4,S1,V3       WILL BE ZERO ON A HIT
         A1        A1+A2          INCREASE A BY THAT VALUE
         B.A       A1             RESET A ADDRESS
         SETMASK   V4             SET MASK ON ZERO HIT
         A5        B.N
         A4        A5-A3          CHECK POSITION
         B.VLENG   A3             KEEP OLD VECTOR LENGTH
         A3        VLEN
         A0        A4
         B.N       A4
         JAZ       COMPLETE       THIS CASE IS LESS THAN/EQUAL TO VLEN
*
LOOP     =         *
         VL        A3             SET VL
         A1        B.A
         A2        B.INCA
         A0        A1
         V0        ,A0,A2
         S1        T.MASK
         A7        B.RD
         V1        V0>A7
         V3        S1&V1          VALUES ISOLATED FOR THE TEST
         A2        A2*A3          HOW MUCH TO INCREASE THE ADDRESS
         S1        T.OBJECT
         COMPUTE   V4,S1,V3       WILL BE ZERO ON A HIT
         A1        A1+A2          INCREASE A BY THAT VALUE
         B.A       A1             RESET A ADDRESS
         VMTOS     S7,S5
         SETMASK   V4             SET MASK ON ZERO HIT
         A7        PS7            POPULATION COUNT
CRAY16   IFE       MAX$VL,NE,64
         A5        PS5            POPULATION COUNT OF VM1
         A7        A7+A5          TOTAL POPULATION COUNT
CRAY16   ENDIF
         A5        B.N
         A1        A5-A3          CHECK POSITION
         A0        A7
         JAN       ANALYZE
HOME     =         *
         A3        VLEN
         A0        A1
         A5        B.VLENG        VECTOR LENGTH FROM OLD MASK
         A6        B.I            CURRENT I POSITION FOR OLD MASK
         A6        A6+A5          ADD TO UPDATE
         B.VLENG   A3             UPDATE VECTOR LENGTH
         B.I       A6             UPDATE I POINTER
         B.N       A1
         JAN       LOOP           LOOP BACK
         J         COMPLETE
ANALYZE  =         *
         LZCNT     A6,S7,S5,A4,A5
         A4        B.NN
         A4        A4+A7
         A5        B.INDEX
         B.NN      A4
         A4        B.I
         A6        A6+1
         J         ALIGNED
         ALIGN
ALIGNED  =         *
LOOPA    =         *
CRAY16   IFE       MAX$VL,NE,64
         S7        S7,S5<A6       SHIFT OFF ZEROS
         S5        S5<A6
CRAY16   ELSE
         S7        S7<A6          SHIFT OFF ZEROS
CRAY16   ENDIF
         A7        A7-1
         A4        A6+A4          CURRENT POINT ON I
         LZCNT     A6,S7,S5,A4,A2 DO LEADING ZERO COUNT
         ,A5       A4             STORE THE LATEST INDEX
         A0        A7             CHECK FOR LOOP BACK
         A5        A5+1           INCREMENT INDEX ADDRESS
         A6        A6+1
         JAZ       RETURNS
CRAY16   IFE       MAX$VL,NE,64
         S7        S7,S5<A6       SHIFT OFF ZEROS
         S5        S5<A6
CRAY16   ELSE
         S7        S7<A6          SHIFT OFF ZEROS
CRAY16   ENDIF
         A7        A7-1
         A4        A6+A4          CURRENT POINT ON I
         LZCNT     A6,S7,S5,A4,A2 DO LEADING ZERO COUNT
         ,A5       A4             STORE THE LATEST INDEX
         A0        A7             CHECK FOR LOOP BACK
         A5        A5+1           INCREMENT INDEX ADDRESS
         A6        A6+1
         JAZ       RETURNS
CRAY16   IFE       MAX$VL,NE,64
         S7        S7,S5<A6       SHIFT OFF ZEROS
         S5        S5<A6
CRAY16   ELSE
         S7        S7<A6          SHIFT OFF ZEROS
CRAY16   ENDIF
         A7        A7-1
         A4        A6+A4          CURRENT POINT ON I
         LZCNT     A6,S7,S5,A4,A2 DO LEADING ZERO COUNT
         ,A5       A4             STORE THE LATEST INDEX
         A0        A7             CHECK FOR LOOP BACK
         A5        A5+1           INCREMENT INDEX ADDRESS
         A6        A6+1
         JAZ       RETURNS
CRAY16   IFE       MAX$VL,NE,64
         S7        S7,S5<A6       SHIFT OFF ZEROS
         S5        S5<A6
CRAY16   ELSE
         S7        S7<A6          SHIFT OFF ZEROS
CRAY16   ENDIF
         A7        A7-1
         A4        A6+A4          CURRENT POINT ON I
         LZCNT     A6,S7,S5,A4,A2 DO LEADING ZERO COUNT
         ,A5       A4             STORE THE LATEST INDEX
         A0        A7             CHECK FOR LOOP BACK
         A5        A5+1           INCREMENT INDEX ADDRESS
         A6        A6+1
         JAZ       RETURNS
CRAY16   IFE       MAX$VL,NE,64
         S7        S7,S5<A6       SHIFT OFF ZEROS
         S5        S5<A6
CRAY16   ELSE
         S7        S7<A6          SHIFT OFF ZEROS
CRAY16   ENDIF
         A7        A7-1
         A4        A6+A4          CURRENT POINT ON I
         LZCNT     A6,S7,S5,A4,A2 DO LEADING ZERO COUNT
         ,A5       A4             STORE THE LATEST INDEX
         A0        A7             CHECK FOR LOOP BACK
         A5        A5+1           INCREMENT INDEX ADDRESS
         A6        A6+1
         JAN       LOOPA
RETURNS  =         *
         A0        A1
         B.INDEX   A5
         JAP       HOME
         J         DONE
*
COMPLETE =         *
         VMTOS     S7,S5
         A7        PS7            POPULATION COUNT
CRAY16   IFE       MAX$VL,NE,64
         A5        PS5            POPULATION COUNT OF VM1
         A7        A7+A5          TOTAL POPULATION COUNT
CRAY16   ENDIF
         A1        -1             KNOW NOT TO RETURN FROM ANALYZE
         A0        A7
         JAN       ANALYZE
*
DONE     =         *
         A1        B.NNA
         A2        B.NN
         ,A1       A2             STORE NN
WHENMMAC ENDM
