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
! Implement TR15580/F2003 intrinsic module IEEE_EXCEPTIONS
!
module ieee_exceptions

  private

#include "ieee_exceptions_types.h"

! The values for IEEE_FLAG_TYPE must be in synch with the values in the C
! fenv.h file, which we can't include directly since it probably contains
! C syntax in addition to cpp definitions
#include "ieee_cpp_macros.h"

  type(IEEE_FLAG_TYPE), parameter, public :: &
    IEEE_INVALID = IEEE_FLAG_TYPE(FE_INVALID), &
    IEEE_DIVIDE_BY_ZERO = IEEE_FLAG_TYPE(FE_DIVBYZERO), &
    IEEE_OVERFLOW = IEEE_FLAG_TYPE(FE_OVERFLOW), &
    IEEE_UNDERFLOW = IEEE_FLAG_TYPE(FE_UNDERFLOW), &
    IEEE_INEXACT = IEEE_FLAG_TYPE(FE_INEXACT)

  type(IEEE_FLAG_TYPE), dimension(3), parameter, public :: IEEE_USUAL = (/ &
    IEEE_OVERFLOW, &
    IEEE_DIVIDE_BY_ZERO, &
    IEEE_INVALID &
    /)

  type(IEEE_FLAG_TYPE), dimension(5), parameter, public :: IEEE_ALL = (/ &
    IEEE_USUAL, &
    IEEE_UNDERFLOW, &
    IEEE_INEXACT &
    /)

  ! F2003 doesn't seem to require these for ieee_flag_type, but they're hard
  ! to do without, and it does require them for ieee_class_type and
  ! ieee_round_type; seems like an oversight

  interface operator (==)
    module procedure test_equality_flag
  end interface operator(==)

  interface operator(/=)
    module procedure test_inequality_flag
  end interface operator(/=)

  interface IEEE_SUPPORT_FLAG

    logical function ieee_support_flag(FLAG)
#include "ieee_exceptions_types.h"
      type(IEEE_FLAG_TYPE), intent(in) :: FLAG
    end function ieee_support_flag

    logical function ieee_support_flag_4(FLAG, X)
#include "ieee_exceptions_types.h"
      type(IEEE_FLAG_TYPE), intent(in) :: FLAG
      real(kind=4) :: X
    end function ieee_support_flag_4

    logical function ieee_support_flag_4a(FLAG, X)
#include "ieee_exceptions_types.h"
      type(IEEE_FLAG_TYPE), intent(in) :: FLAG
      real(kind=4), dimension(*) :: X
    end function ieee_support_flag_4a

    logical function ieee_support_flag_4b(FLAG, X)
#include "ieee_exceptions_types.h"
      type(IEEE_FLAG_TYPE), intent(in) :: FLAG
      real(kind=4), dimension(:,:) :: X
    end function ieee_support_flag_4b

    logical function ieee_support_flag_4c(FLAG, X)
#include "ieee_exceptions_types.h"
      type(IEEE_FLAG_TYPE), intent(in) :: FLAG
      real(kind=4), dimension(:,:,:) :: X
    end function ieee_support_flag_4c

    logical function ieee_support_flag_4d(FLAG, X)
#include "ieee_exceptions_types.h"
      type(IEEE_FLAG_TYPE), intent(in) :: FLAG
      real(kind=4), dimension(:,:,:,:) :: X
    end function ieee_support_flag_4d

    logical function ieee_support_flag_4e(FLAG, X)
#include "ieee_exceptions_types.h"
      type(IEEE_FLAG_TYPE), intent(in) :: FLAG
      real(kind=4), dimension(:,:,:,:,:) :: X
    end function ieee_support_flag_4e

    logical function ieee_support_flag_4f(FLAG, X)
#include "ieee_exceptions_types.h"
      type(IEEE_FLAG_TYPE), intent(in) :: FLAG
      real(kind=4), dimension(:,:,:,:,:,:) :: X
    end function ieee_support_flag_4f

    logical function ieee_support_flag_4g(FLAG, X)
#include "ieee_exceptions_types.h"
      type(IEEE_FLAG_TYPE), intent(in) :: FLAG
      real(kind=4), dimension(:,:,:,:,:,:,:) :: X
    end function ieee_support_flag_4g

    logical function ieee_support_flag_8(FLAG, X)
#include "ieee_exceptions_types.h"
      type(IEEE_FLAG_TYPE), intent(in) :: FLAG
      real(kind=8) :: X
    end function ieee_support_flag_8

    logical function ieee_support_flag_8a(FLAG, X)
#include "ieee_exceptions_types.h"
      type(IEEE_FLAG_TYPE), intent(in) :: FLAG
      real(kind=8), dimension(*) :: X
    end function ieee_support_flag_8a

    logical function ieee_support_flag_8b(FLAG, X)
#include "ieee_exceptions_types.h"
      type(IEEE_FLAG_TYPE), intent(in) :: FLAG
      real(kind=8), dimension(:,:) :: X
    end function ieee_support_flag_8b

    logical function ieee_support_flag_8c(FLAG, X)
#include "ieee_exceptions_types.h"
      type(IEEE_FLAG_TYPE), intent(in) :: FLAG
      real(kind=8), dimension(:,:,:) :: X
    end function ieee_support_flag_8c

    logical function ieee_support_flag_8d(FLAG, X)
#include "ieee_exceptions_types.h"
      type(IEEE_FLAG_TYPE), intent(in) :: FLAG
      real(kind=8), dimension(:,:,:,:) :: X
    end function ieee_support_flag_8d

    logical function ieee_support_flag_8e(FLAG, X)
#include "ieee_exceptions_types.h"
      type(IEEE_FLAG_TYPE), intent(in) :: FLAG
      real(kind=8), dimension(:,:,:,:,:) :: X
    end function ieee_support_flag_8e

    logical function ieee_support_flag_8f(FLAG, X)
#include "ieee_exceptions_types.h"
      type(IEEE_FLAG_TYPE), intent(in) :: FLAG
      real(kind=8), dimension(:,:,:,:,:,:) :: X
    end function ieee_support_flag_8f

    logical function ieee_support_flag_8g(FLAG, X)
#include "ieee_exceptions_types.h"
      type(IEEE_FLAG_TYPE), intent(in) :: FLAG
      real(kind=8), dimension(:,:,:,:,:,:,:) :: X
    end function ieee_support_flag_8g

#ifdef REAL16
    logical function ieee_support_flag_16(FLAG, X)
#include "ieee_exceptions_types.h"
      type(IEEE_FLAG_TYPE), intent(in) :: FLAG
      real(kind=16) :: X
    end function ieee_support_flag_16

    logical function ieee_support_flag_16a(FLAG, X)
#include "ieee_exceptions_types.h"
      type(IEEE_FLAG_TYPE), intent(in) :: FLAG
      real(kind=16), dimension(*) :: X
    end function ieee_support_flag_16a

    logical function ieee_support_flag_16b(FLAG, X)
#include "ieee_exceptions_types.h"
      type(IEEE_FLAG_TYPE), intent(in) :: FLAG
      real(kind=16), dimension(:,:) :: X
    end function ieee_support_flag_16b

    logical function ieee_support_flag_16c(FLAG, X)
#include "ieee_exceptions_types.h"
      type(IEEE_FLAG_TYPE), intent(in) :: FLAG
      real(kind=16), dimension(:,:,:) :: X
    end function ieee_support_flag_16c

    logical function ieee_support_flag_16d(FLAG, X)
#include "ieee_exceptions_types.h"
      type(IEEE_FLAG_TYPE), intent(in) :: FLAG
      real(kind=16), dimension(:,:,:,:) :: X
    end function ieee_support_flag_16d

    logical function ieee_support_flag_16e(FLAG, X)
#include "ieee_exceptions_types.h"
      type(IEEE_FLAG_TYPE), intent(in) :: FLAG
      real(kind=16), dimension(:,:,:,:,:) :: X
    end function ieee_support_flag_16e

    logical function ieee_support_flag_16f(FLAG, X)
#include "ieee_exceptions_types.h"
      type(IEEE_FLAG_TYPE), intent(in) :: FLAG
      real(kind=16), dimension(:,:,:,:,:,:) :: X
    end function ieee_support_flag_16f

    logical function ieee_support_flag_16g(FLAG, X)
#include "ieee_exceptions_types.h"
      type(IEEE_FLAG_TYPE), intent(in) :: FLAG
      real(kind=16), dimension(:,:,:,:,:,:,:) :: X
    end function ieee_support_flag_16g

#endif /* REAL16 */

  end interface IEEE_SUPPORT_FLAG

  interface IEEE_GET_FLAG

    elemental subroutine ieee_get_flag(FLAG, FLAG_VALUE)
#include "ieee_exceptions_types.h"
      type(IEEE_FLAG_TYPE), intent(in) :: FLAG
      logical, intent(out) :: FLAG_VALUE
    end subroutine ieee_get_flag

  end interface IEEE_GET_FLAG

  ! TR15580 had these as "elemental"; F2003 changes them to "pure", evidently
  ! because two elements of "flag" might be the same but the corresponding
  ! values of "halting" might be different (see J3 meeting/168/04-288r1.txt.)
  ! That seems mistaken because both TR15580 and F2003 say that elements of
  ! "flag" must all be different.
  interface IEEE_SET_FLAG

    elemental subroutine ieee_set_flag(FLAG, FLAG_VALUE)
#include "ieee_exceptions_types.h"
      type(IEEE_FLAG_TYPE), intent(in) :: FLAG
      logical, intent(in) :: FLAG_VALUE
    end subroutine ieee_set_flag

  end interface IEEE_SET_FLAG

  interface IEEE_SUPPORT_HALTING

    logical function ieee_support_halting(FLAG)
#include "ieee_exceptions_types.h"
      type(IEEE_FLAG_TYPE), intent(in) :: FLAG
    end function ieee_support_halting

  end interface IEEE_SUPPORT_HALTING

  interface IEEE_GET_HALTING_MODE

    elemental subroutine ieee_get_halting_mode(FLAG, HALTING)
#include "ieee_exceptions_types.h"
      type(IEEE_FLAG_TYPE), intent(in) :: FLAG
      logical, intent(out) :: HALTING
    end subroutine ieee_get_halting_mode

  end interface IEEE_GET_HALTING_MODE

  ! See comment about "elemental" w.r.t ieee_set_flag
  interface IEEE_SET_HALTING_MODE

    elemental subroutine ieee_set_halting_mode(FLAG, HALTING)
#include "ieee_exceptions_types.h"
      type(IEEE_FLAG_TYPE), intent(in) :: FLAG
      logical, intent(in) :: HALTING
    end subroutine ieee_set_halting_mode

  end interface IEEE_SET_HALTING_MODE

  interface IEEE_GET_STATUS

    subroutine ieee_get_status(STATUS_VALUE)
#include "ieee_exceptions_types.h"
      type(IEEE_STATUS_TYPE), intent(out) :: STATUS_VALUE
    end subroutine ieee_get_status

  end interface IEEE_GET_STATUS

  interface IEEE_SET_STATUS

    subroutine ieee_set_status(STATUS_VALUE)
#include "ieee_exceptions_types.h"
      type(IEEE_STATUS_TYPE), intent(in) :: STATUS_VALUE
    end subroutine ieee_set_status

  end interface IEEE_SET_STATUS

  public &
    operator(==), &
    operator(/=), &
    IEEE_SUPPORT_FLAG, &
    IEEE_GET_FLAG, &
    IEEE_SET_FLAG, &
    IEEE_GET_HALTING_MODE, &
    IEEE_SET_HALTING_MODE, &
    IEEE_SUPPORT_HALTING, &
    IEEE_GET_STATUS, &
    IEEE_SET_STATUS, &
    IEEE_FLAG_TYPE, &
    IEEE_STATUS_TYPE

contains 

    logical function test_equality_flag(a, b)
      type(IEEE_FLAG_TYPE), intent(in) :: a, b
      test_equality_flag = (a%value == b%value)
    end function test_equality_flag

    logical function test_inequality_flag(a, b)
      type(IEEE_FLAG_TYPE), intent(in) :: a, b
      test_inequality_flag = (a%value /= b%value)
    end function test_inequality_flag

end module ieee_exceptions
