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
! Implement TR15580/F2003 intrinsic module IEEE_FEATURES
!
module ieee_features

  private

  type, public :: IEEE_FEATURES_TYPE
    private
    sequence
    integer :: value
  end type IEEE_FEATURES_TYPE

  type(IEEE_FEATURES_TYPE), parameter, public :: &
    IEEE_DATATYPE = IEEE_FEATURES_TYPE(1), &
    IEEE_DENORMAL = IEEE_FEATURES_TYPE(2), &
    IEEE_DIVIDE = IEEE_FEATURES_TYPE(4), &
    IEEE_HALTING = IEEE_FEATURES_TYPE(8), &
    IEEE_INEXACT_FLAG = IEEE_FEATURES_TYPE(16), &
    IEEE_INF = IEEE_FEATURES_TYPE(32), &
    IEEE_INVALID_FLAG = IEEE_FEATURES_TYPE(64), &
    IEEE_NAN = IEEE_FEATURES_TYPE(128), &
    IEEE_ROUNDING = IEEE_FEATURES_TYPE(256), &
    IEEE_SQRT = IEEE_FEATURES_TYPE(512), &
    IEEE_UNDERFLOW_FLAG = IEEE_FEATURES_TYPE(1024)

end module ieee_features
