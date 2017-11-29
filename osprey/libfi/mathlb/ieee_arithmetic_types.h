!
! Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
!
! Copyright (c) 2006. QLogic Corporation.  All rights reserved.
!
! Unpublished -- rights reserved under the copyright laws of the United
! States. USE OF A COPYRIGHT NOTICE DOES NOT IMPLY PUBLICATION OR
! DISCLOSURE. THIS SOFTWARE CONTAINS CONFIDENTIAL INFORMATION AND TRADE
! SECRETS OF QLOGIC CORPORATION. USE, DISCLOSURE, OR REPRODUCTION IS
! PROHIBITED WITHOUT THE PRIOR EXPRESS WRITTEN PERMISSION OF QLOGIC,
! CORPORATION.
!
! U.S. Government Restricted Rights:
! The Software is a "commercial item," as that term is defined at 48
! C.F.R. 2.101 (OCT 1995), consisting of "commercial computer software"
! and "commercial computer software documentation," as such terms are used
! in 48 C.F.R. 12.212 (SEPT 1995).  Consistent with 48 C.F.R. 12.212 and
! 48 C.F.R. 227-7202-1 through 227-7202-4 (JUNE 1995), all U.S. Government
! End Users acquire the Software with only those rights set forth in the
! accompanying license agreement. QLogic Corporation, 26650 Aliso Viejo 
! Parkway, Aliso Viejo, CA 92656.
!

!
! Types needed to implement TR15580/F2003 intrinsic module IEEE_ARITHMETIC
!
  type :: IEEE_CLASS_TYPE
    sequence
    integer value;
  end type IEEE_CLASS_TYPE
  type :: IEEE_ROUND_TYPE
    sequence
    integer value;
  end type IEEE_ROUND_TYPE
