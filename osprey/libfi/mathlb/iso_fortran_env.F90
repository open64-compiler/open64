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
! Intrinsic module specified by ISO/IEC 1539-1:2004 Section 13.8.2
!
! These values are correct for x86 (and most other modern architectures)
! The Fortran standard has no notion of our -i8 and -r8 command-line options.
! It doesn't seem useful to attempt to change NUMERIC_STORAGE_SIZE to 64 when
! those options are in effect (What if -i8 is in effect but not -r8? What
! about the dictum that doubleprecision must occupy twice the space of default
! integer?) so we say that -i8 and -r8 do not change the size of the default
! types, but rather they cause "integer" and "real" to be mapped during
! compilation onto the appropriate non-default types.
module iso_fortran_env
  public

#include "ieee_cpp_macros.h"

  integer, parameter :: CHARACTER_STORAGE_SIZE = 8
  integer, parameter :: ERROR_UNIT = 0 ! See libf/open.c
  integer, parameter :: FILE_STORAGE_SIZE = 8
  integer, parameter :: INPUT_UNIT = 5 ! See libf/open.c
  integer, parameter :: IOSTAT_END = FERDENDR ! See clibinc/liberrno.h
  integer, parameter :: IOSTAT_EOR = FEEORCND ! See clibinc/liberrno.h
  integer, parameter :: NUMERIC_STORAGE_SIZE = 32
  integer, parameter :: OUTPUT_UNIT = 6 ! See libf/open.c

end module iso_fortran_env
