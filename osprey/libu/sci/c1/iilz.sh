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
* USMID "@(#) libu/sci/c1/iilz.sh	92.0	10/08/98 14:57:41"
*-------------------------------------------------------------------
*\Documentation:
*
*\Name:  IILZMAC
*
*\Description:
*      If the vector being searched is INTEGER or REAL, this macro 
*      returns the number of leading zero elements.  If the vector
*      is typed LOGICAL, the macro returns the number of leading
*      FALSE values.  This macro is called by the routine
*      IILZ.
*      A2       ... ( input )  The address of array, L is stored here.
*                              This is the array being searched.
*      A3       ... ( input )  This is the stride through L.
*      A1       ... ( input )  The address of N, the size of the 
*                              problem, is stored here.
*      A4       ... ( output ) Number of leading elements that meet
*                              the conditions as stated by the
*                              macro FORMMASK.
*
*\Usage:  IILZMAC
*
*\Arguments:
*
*\Register arguments:
*      A1, A3, S2
*
*\Modified registers:
*      All A-registers
*      B.INCX
*      S1, S2, S3, S6, S7 
*      V1 
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
*\Keywords:
*     search, leading elements
*
*\Macros called:
*     FORMMASK - Forms a vector mask.
*     LZCNT - This macro gets the leading zero count of the vector
*             mask register.
*     VMTOS - On a YMP, one does SVM <-- VM.  This macro does that
*             step.  If the code is being executed on a Y16, it
*             performs the additional step of doing SVM1 <-- VM1.
*
*\Routines called:
*     None
*
*\Revision history:
*     02 Jan 1991    Original version
*
*\Endlib
*---------------------------------------------------------------------
*
         MACRO
         IILZMAC
CRAY16   IFE       MAX$VL,NE,64
         A4        0
         A6        VLEN
         A1        ,A1            N
         A3        ,A3            INCX
         S2        <VBIT
         A0        -A1
         S1        A1
         JAP       RETURN         Return if N <= 0
         A0        A3
         JAP       POSINCX
         A7        A1-1
         A7        A7*A3
         A2        A2-A7          A2=A2-(N-1)*INCX
POSINCX  =         *
         S1        S1&S2
         A0        S1
         JAZ       LOOP           VL is a multiple of VLEN
         A6        S1             A6 = VL mod VLEN
LOOP     =         *
         VL        A6
         A0        A2
         V1        ,A0,A3
         FORMMASK  V1             Form mask
         A5        A6*A3
         A2        A2+A5          Address of next segment
         S1        <D'64
         S2        <D'64
         S1        S2,S1>A6
         S3        0
         S2        S3,S2>A6
         VMTOS     S7,S6          VM,VM1
         S7        S2!S7          
         S6        S1!S6          FLAG UNUSED VM BITS
         B.INCX    A3             SAVE INCX
         LZCNT     A7,S7,S6,A5,A3
         A3        B.INCX         RESTORE INCX
         A0        A7-A6
         A4        A4+A7
         JAM       RETURN
         A0        A6-A1          VL - remaining elements
         A1        A1-A6          Remaining elements
         A6        VLEN
         JAN       LOOP
RETURN   =         *
         S1        A4
         A1        B.TRACEBK      restore traceback info for exit
CRAY16   ELSE
         A4        0
         A6        VLEN
         A1        ,A1            N
         A3        ,A3            INCX
         S2        <VBIT
         A0        -A1
         S1        A1
         JAP       RETURN         Return if N <= 0
         A0        A3
         JAP       POSINCX
         A7        A1-1
         A7        A7*A3
         A2        A2-A7          A2=A2-(N-1)*INCX
POSINCX  =         *
         S1        S1&S2
         A0        S1
         JAZ       LOOP           VL is a multiple of VLEN
         A6        S1             A6 = VL mod VLEN
LOOP     =         *
         VL        A6
         A0        A2
         V1        ,A0,A3
         FORMMASK  V1             Form mask
         A5        A6*A3
         A2        A2+A5          Address of next segment
         S1        <D'64
         S1        S1>A6
         VMTOS     S7,S6          VM,DUMMY
         S7        S1!S7
         LZCNT     A7,S7,S6,A5,A3
         A0        A7-A6
         A4        A4+A7
         JAM       RETURN
         A0        A6-A1          VL - remaining elements
         A1        A1-A6          Remaining elements
         A6        VLEN
         JAN       LOOP
RETURN   =         *
         S1        A4
         A1        B.TRACEBK      restore traceback info for exit
CRAY16   ENDIF
IILZMAC  ENDM
