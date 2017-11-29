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
* USMID "@(#) libu/sci/c1/target.sh	92.0	10/08/98 14:57:41"
*-------------------------------------------------------------------
*\Documentation:
*
*\Name:  TARGETH
*
*\Description:
*   This file defines some terms and macros that allow conversion
*   LIBSCI CAL codes to be used either on a YMP or Y16.  The macros
*   defined are -
*   LZSVREG - This macro only executes on a Y16 and is called by
*             the LZCNT macro.  It defines an additional B-register
*             and an additonal T-register for use in the LZCNT
*             macro.
*   LZCNT - This macro gets the leading zero count of the vector
*           mask register.
*   POPC - This returns the population count of S-registers.  It
*          is assumed that the S-registers contain VM and, if on
*          a C90, VM1.
*   STOVM - On a YMP, one does VM <-- SVM.  This macro does that
*           step.  If the code is being executed on a Y16, it 
*           performs the additional step of VM1 <-- SVM1.
*   VMTOS - On a YMP, one does SVM <-- VM.  This macro does that
*           step.  If the code is being executed on a Y16, it 
*           performs the additional step of doing SVM1 <-- VM1.
*   SVMTS0 - This macro is used to determine if VM is set and stores
*            the result in S0 for use with JSZ instruction
*
*   The terms defined are -
*   Define VLEN -   length of vector register
*               =   64 for pre-C90
*               =   128 for C90
*   Define VLENM1 - length of vector register - 1
*                 = 63 for pre-C90
*                 = 127 for C90
*   Define DVLEN -  2*length of vector register
*                =  128 for pre-C90
*                =  256 for C90
*   Define VBIT -   2**VBIT = VLEN
*               =   6 for pre-C90
*               =   7 for C90
*   Define VBITM1 - 2**VBITM1 = VLEN/2
*                 = 5 for pre-C90
*                 = 6 for C90
*    
*\Usage:
*   VMTOS     SVM,SVM1
*   SVMTS0    SVM,SVM1
*   STOVM     SVM,SVM1
*   LZCNT     ARES,SVM,SVM1,ASCR1,ASCR2
*
*\Arguments:
*     VMTOS
*       SVM   S-register argument to contain VM. (output)
*       SVM1  S-register argument to cotain VM1 on C90. (output)
*     SVMTS0
*       SVM   S-register argument to contain VM. (output)
*       SVM1  S-register argument to cotain VM1 on C90. (output)
*     STOVM
*       SVM   S-register argument containing VM. (input)
*       SVM1  S-register argument containing VM1. (input)
*     LZCNT
*       ARES  A-register argument. (output)
*       SVM   S-register argument containing VM. (input)
*       SVM1  S-reguster argument containing VM1 on C90. (input)
*       ASCR1 Scratch A-register argument - saved.
*       ASCR2 Scratch A-register argument - destroyed.
*
*\Register arguments:
*
*\Modified registers:
*     VMTOS
*       SVM
*       SVM1
*     STOVM
*     LZCNT
*       ARES
*       ASCR1
*       ASCR2
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
*\Keywords:
*     C90, DVLEN, VBIT, VLEN, VBITM1, VLENM1
*
*\Revision history:
*     Dec 90      Original Implementation
*     Nov 91      Modified so that the string that
*                   IF( $CPU="CRAY C90" ) THEN
*                     Expand C90 code
*
*                 Before, it was
*                   IF( $CPU="CRAY Y16")
*                     Expand C90 code
*
*     Mar 92      Add POPC macro.
*
*     Oct 93      Modified conditional code statement to
*                   be more generic -- based on MAX$VL
*                   rather thatn $CPU.  Also corrected
*                   and added additional comments
*
*     Mar 94      Moved SVMTS0 from when-cisg.sh to fix problems
*                   seen in isrchm.sh, iilz.sh, and when-cisg.sh
*                   with the failure to set S0 correctly -- reported
*                   by SPR 76383
*
*\Endlib
*---------------------------------------------------------------------
*
*
         CPUTYPE
*
CRAY16   IFE       MAX$VL,EQ,128
VBIT     =         D'7
VBITM1   =         D'6
VLEN     =         D'128
VLENM1   =         D'127
DVLEN    =         D'256
CRAY16   ELSE
VBIT     =         D'6
VBITM1   =         D'5
VLEN     =         D'64
VLENM1   =         D'63
DVLEN    =         D'128
CRAY16   ENDIF
*
*
         MACRO
         POPC      SVM,SVM1,AVM,AVM1
           AVM     P_SVM
CRAY16     IFE     MAX$VL,EQ,128
             AVM1    P_SVM1
             AVM     AVM+AVM1
CRAY16     ENDIF
POPC     ENDM
*
*
         MACRO
         VMTOS     SVM,SVM1
         SVM       VM
CRAY16   IFE       MAX$VL,EQ,128
         SVM1      VM1
CRAY16   ENDIF
VMTOS    ENDM
*
*
         MACRO
         STOVM     SVM,SVM1
         VM        SVM
CRAY16   IFE       MAX$VL,EQ,128
         VM1       SVM1
CRAY16   ENDIF
STOVM    ENDM
*
*
         MACRO
         LZSVREG
CRAY16   IFE       MAX$VL,EQ,128
C90SVA   DEFB
C90SVS   DEFT      TEMP
CRAY16   ENDIF
LZSVREG  ENDM
*
*
         MACRO
         LZCNT     ARES,SVM,SVM1,ASCR1,ASCR2
*        ARES      Result in an A register
*        SVM       S register containing VM
*        SVM1      S register containing VM1 on C90
*        ASCR1     scratch A register - saved
*        ASCR2     scratch A register - destroyed
CRAY16   IFE       MAX$VL,EQ,128
         B.C90SVA  ASCR1
         ASCR1     Z_SVM          LEADING ZERO COUNT OF VM
         T.C90SVS  SVM
         ARES      Z_SVM1         LEADING ZERO COUNT OF VM1
         SVM       ASCR1
         SVM       SVM>6          If leading zeros of VM0 < 64, set SVM = 0
*                                 If leading zeros of VM0 = 64, set SVM = 1
         ASCR2     SVM
         ARES      ARES*ASCR2     If VM0 was < 64, ignore VM1 by mult. by 0
*                                 If VM0 all zeros, save VM1 by mult. by 1
         SVM       T.C90SVS
         ARES      ARES+ASCR1     Add either VM1 count or 0 to VM0 count
         ASCR1     B.C90SVA
CRAY16   ELSE
         ARES      Z_SVM          LEADING ZERO COUNT
CRAY16   ENDIF
LZCNT    ENDM
*
*-----------------------------------------------------------------------
*\Documentation:
*
*\Name:  SVMTS0
*
*\Description:
*     The assumption of this macro is that the two S-registers
*     being passed were previously used to read the vector
*     mask just prior to this call.  If running on a YMP,
*     only one of the S-registers, SVM, will be needed and
*     it can be directly read into S0.  Otherwise, the logical
*     OR of the two S-registers is put into S0.  This logical
*     OR indicates whether bits are set at all in the vector mask. 
*     The result in S0 can then be used to perform a conditional
*     branch.
*
*\Macros called:
*     None.
*
*\Usage:
*     SVMTS0    SVM,SVM1
*
*\Arguments:
*     SVM -     [ S register,input ]
*               Contains VM
*
*     SVM1 -    [ S register,input ]
*               Contains VM1
*
*\Register arguments:
*     None.
*
*\Modified registers:
*     S0
*
         MACRO
         SVMTS0    SVM,SVM1
CRAY16   IFE       MAX$VL,NE,64
           S0      SVM!SVM1
CRAY16   ELSE
           S0      SVM
CRAY16   ENDIF
SVMTS0   ENDM

*
*  IEE special
*
XEEX     IFE       IEEE$ON,EQ,1
*
MULSV    OPDEF
LSV      V.V1      S.S1*RV.V2
         V.V1      S.S1*FV.V2
MULSV    ENDM
*
MULVV    OPDEF
LVV      V.V1      V.V2*RV.V3
         V.V1      V.V2*FV.V3
MULVV    ENDM
*
MLCSV    OPDEF
CSV      V.V1      S.S1*V.V2
         V.V1      S.S1*LV.V2
MLCSV    ENDM
*
MULSS    OPDEF
LSS      S.S1      S.S2*RS.S3
         S.S1      S.S2*FS.S3
MULSS    ENDM
*
*CSET1    OPDEF
*LC1      S.S1      1.
*         S.S1      0
*         S.S1      D'1023S20:S.S1
*CSET1    ENDM
*
*SQQ      OPDEF
*LSQ      CALLV     SQRT%,,USE=A7
*         S1        SQRT,S1
*SQQ      ENDM
**
*SSQ      OPDEF
*LSSQ     CALLV     SQRT%,,STKPTR=A7
*         S1        SQRT,S1
*SSQ      ENDM 
*
SQ        OPDEF
LSQ       CALLV     SQRT%
          S1        SQRT,S1
SQ        ENDM
*
XEEX     ENDIF
