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
* USMID "@(#) libu/sci/c1/clus.sh	92.0	10/08/98 14:57:41"
*
*-------------------------------------------------------------------
*\Documentation:
*
*\Name:  CLUSMAC
*
*\Description:
*     This macro actually performs the task of searching for 
*     clusters of values which have a specified relation to the
*     target.  That relation is determined by calling routine.
*     This macro is called by CLUSEQ, CLUSFGE, CLUSFGT, CLUSFLE,
*     CLUSFLT, CLUSIGE, CLUSIGT, CLUSILE, CLUSILT, and CLUSNE.
*     Before entry to this routine -
*       B.A      ... ( input )  The address of array, A is stored here.
*                               This is the array being searched.
*       B.INCA   ... ( input )  This is the stride through A.
*       B.INDEX  ... ( output ) This is the address of the result array.
*       B.A      ... ( input )  The size of the problem is stored here.
*       B.NN     ... ( output ) The address of NN is stored here.  When 
*                               When the routine is done, this will 
*                               contain the number of clusters.
*       T.OBJECT ... ( input )  The object being searched for is stored
*                               here. 
*       T.PMASK  ... ( input )  The vector mask is stored in here.
*
*\Usage:
*     CLUSMAC
*
*\Arguments:
*   B registers:
*       A, I, INCA, INDEX, N, NNA, NN, VLENG
*   T registers
*       OBJECT, PMASK
*
*\Modified registers:
*     All A-registers
*     All S-registers
*     V0, V1, V2, V3, V4
*
*\Enddoc
*
*---------------------------------------------------------------------
*
*\Lib
*
*\Revision history:
*      02 Jan  91    Original version
*      14 Dec  92    Fix bug
*
*\Author:
*      Barrie Kletscher
*      Mathematical Software Group
*      Cray Research, Inc.
*
*\Keywords:
*      search, clusters
*
*\Macros called:
*      COMPUTE - This determines whether the relation between the
*                target and the array is EQ, NE, GT, GE, LE, or
*                LT.
*      LOADTOBJ - Loads the object being searched in a T-register
*                 called OBJECT. 
*      LZCNT - This macro gets the leading zero count of the vector
*              mask register.
*      SETMASK - This sets the VM register according to the
*                relationship specified by the COMPUTE macro.
*      VMTOS - On a YMP, one does SVM <-- VM.  This macro does that
*              step.  If the code is being executed on a Y16, it
*              performs the additional step of doing SVM1 <-- VM1.
*
*\Performance data:
*
*\Endlib
*---------------------------------------------------------------------
*
         MACRO
         CLUSMAC
*
         A0        B.INCA         Check for neg increment
         A1        B.INCA
         A1        -A1
         JAP       POSINC
         A2        B.N
         A2        A2-1           (N-1)
         A3        A1*A2          ABS(INCA)*(N-1)
         A4        B.A            Base address of a
         A4        A4+A3          (N-1)*ABS(INCA)
         B.A       A4             Store in B register
POSINC   =         *
         LOADTOBJ                 Now I have loaded B's and T's
         A1        B.N
         A0        -A1            -VL
         A3        -A1
         S2        <6
         S1        A3
         JAP       DONE           Zero vector length
         S1        #S1&S2
         A3        S1
         A3        A3+1           First seg length
         VL        A3
*
         A1        B.A
         A2        B.INCA
         A7        0
         B.I       A7
         A0        A1
         V3        ,A0,A2
         A2        A2*A3          How much to increase the address
         S1        T.OBJECT
         COMPUTE   V4,S1,V3       Will be zero on a hit
         A1        A1+A2          Increase a by that value
         B.A       A1             Reset a address
         SETMASK   V4             Set mask on zero hit
         A5        B.N
         A4        A5-A3          Check position
         B.VLENG   A3             Keep old vector length
         A3        D'64
         A0        A4
         B.N       A4
         JAZ       COMPLETE       This case is less than/equal to VLEN
*
LOOP     =         *
         VL        A3             Set VL
         A1        B.A
         A2        B.INCA
         A0        A1
         V0        ,A0,A2
         A2        A2*A3          How much to increase the address
         S1        T.OBJECT
         COMPUTE   V1,S1,V0       Will be zero on a hit
         A1        A1+A2          Increase a by that value
         B.A       A1             Reset a address
         S7        VM
         SETMASK   V1             Set mask on zero hit
         A7        1              Shift count
         S6        T.PMASK        Get previous mask
         S5        S7             Copy of new mask
         S5        S6,S5>A7       Shift previous mask into new mask
         S3        <D'64          All ones mask
         S6        0
         A7        B.VLENG
         S6        S3,S6>A7       Mask of VL
         S2        0
         S2        S2,S7<A7       Right justify vector mask
         T.PMASK   S2             New previous mask
         S5        S5&S6          Allow only VL length mask
         S7        S5\S7          XOR of shifted mask and mask
*                                 Will have 1'S in cluster transitions
         A7        PS7            Population count
         A5        B.N
         A1        A5-A3          Check position
         A0        A7
         JAN       ANALYZE
HOME     =         *
         A3        D'64
         A0        A1
         A5        B.VLENG        Vector length from old mask
         A6        B.I            Current I position for old mask
         A6        A6+A5          Add to update
         B.VLENG   A3             Update vector length
         B.I       A6             Update I pointer
         B.N       A1
         JAZ       COMPLETE
         VL        A3             Set VL
         A1        B.A
         A2        B.INCA
         A0        A1
         V3        ,A0,A2
         A2        A2*A3          How much to increase the address
         S1        T.OBJECT
         COMPUTE   V4,S1,V3       Will be zero on a hit
         A1        A1+A2          Increase a by that value
         B.A       A1             Reset a address
         S7        VM
         SETMASK   V4             Set mask on zero hit
         A7        1              Shift count
         S6        T.PMASK        Get previous mask
         S5        S7             Copy of new mask
         S5        S6,S5>A7       Shift previous mask into new mask
         S3        <D'64          All ones mask
         S6        0
         A7        B.VLENG
         S6        S3,S6>A7       Mask of VL
         S2        0
         S2        S2,S7<A7       Right justify vector mask
         T.PMASK   S2             New previous mask
         S5        S5&S6          Allow only VL length mask
         S7        S5\S7          XOR of shifted mask and mask
*                                 Will have 1'S in cluster transitions
         A7        PS7            Population count
         A5        B.N
         A1        A5-A3          Check position
         A0        A7
         JAN       ANALYZE
         A3        D'64
         A0        A1
         A5        B.VLENG        Vector length from old mask
         A6        B.I            Current I position for old mask
         A6        A6+A5          Add to update
         B.VLENG   A3             Update vector length
         B.I       A6             Update I pointer
         B.N       A1
         JAN       LOOP           Loop back
         J         COMPLETE
ANALYZE  =         *
         A6        ZS7
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
         S7        S7<A6          Shift off zeros
         A7        A7-1
         A4        A6+A4          Current point on I
         A6        ZS7
         ,A5       A4             Store the latest index
         A0        A7             Check for loop back
         A5        A5+1           Increment INDEX address
         A6        A6+1
         JAZ       RETURNS
         S7        S7<A6          Shift off zeros
         A7        A7-1
         A4        A6+A4          Current point on I
         A6        ZS7
         ,A5       A4             Store the latest index
         A0        A7             Check for loop back
         A5        A5+1           Increment INDEX address
         A6        A6+1
         JAZ       RETURNS
         S7        S7<A6          Shift off zeros
         A7        A7-1
         A4        A6+A4          Current point on I
         A6        ZS7
         ,A5       A4             Store the latest index
         A0        A7             Check for loop back
         A5        A5+1           Increment INDEX address
         A6        A6+1
         JAZ       RETURNS
         S7        S7<A6          Shift off zeros
         A7        A7-1
         A4        A6+A4          Current point on I
         A6        ZS7
         ,A5       A4             Store the latest index
         A0        A7             Check for loop back
         A5        A5+1           Increment INDEX address
         A6        A6+1
         JAZ       RETURNS
         S7        S7<A6          Shift off zeros
         A7        A7-1
         A4        A6+A4          Current point on I
         A6        ZS7
         ,A5       A4             Store the latest index
         A0        A7             Check for loop back
         A5        A5+1           Increment INDEX address
         A6        A6+1
         JAN       LOOPA
RETURNS  =         *
         A0        A1
         B.INDEX   A5
         JAP       HOME
         J         DONE
*
COMPLETE =         *
         S7        VM
         A7        1              Shift count
         S6        T.PMASK        Get previous mask
         S5        S7             Copy of new mask
         S5        S6,S5>A7       Shift previous mask into new mask
         S3        <D'64          All ones mask
         S6        0
         A7        B.VLENG
         S6        S3,S6>A7       Mask of VL
         S2        0
         S2        S2,S7<A7       Right justify vector mask
         T.PMASK   S2             New previous mask
         S5        S5&S6          Allow only VL length mask
         S7        S5\S7          XOR of shifted mask and mask
*                                 Will have 1'S in cluster transitions
         A7        PS7            Population count
         A1        -1             Know not to return from analyze
         A0        A7
         JAN       ANALYZE
*
DONE     =         *
         A1        B.NNA
         A2        B.NN
         S1        A2             Transmit NN to S register
         S0        S1<D'63        Copy of NN
         JSP       STORENN        NN is even last cluster is complete
*
** Complete last cluster here
*
         A2        A2+1           Increase NN by one
         A4        B.I
         A4        A4+1
         A6        B.VLENG
         A4        A4+A6
         A5        B.INDEX
         S1        A2             Reset NN in S1
         ,A5       A4             Store N+1 in last values of INDEX
         A5        A5+1
         B.INDEX   A5
*
STORENN  =         *
         S1        S1>1           NN/2
         ,A1       S1             Store NN
*
** Need to reduce all the second index values by 1
** I will do this in a vector loop
*
         A1        B.INDEX
         A1        A1-1           Undo the last address increment
         B.INDEX   A1             Store that fix
         A1        S1             Put NN in A1
         A3        -A1
         A0        -A1
         S2        <6
         S1        A3
         JAP       NOFIX          Nothing to modify
         S1        #S1&S2
         A3        S1
         A3        A3+1           First segment length
         A4        -2             Skip increment
FIXLOOP  =         *
         VL        A3
         A0        B.INDEX
         V1        ,A0,A4         Load INDEX(2,NN)...INDEX(2,1)
         S1        1
         S1        -S1
         V2        S1+V1          Reduce by one
         A6        A3+A3          2*VL
         A5        B.INDEX
         A5        A5-A6          Reduce by 2*VL
         A1        A1-A3          Reduce NN by VL
         ,A0,A4    V2             Store the new result
         A0        A1             Remaining VL
         B.INDEX   A5             Restore B.INDEX
         A3        D'64
         JAN       FIXLOOP
NOFIX    =         *
*
CLUSMAC  ENDM
