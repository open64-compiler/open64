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
! Implement TR15580/F2003 intrinsic module IEEE_ARITHMETIC
!
module ieee_arithmetic

  private

  include "ieee_arithmetic_types.h"

! The values for IEEE_CLASS_TYPE and IEEE_ROUND_TYPE must be in synch with the
! values in the C math.h and fenv.h files, which we can't include directly
! since they probably contain C syntax in addition to cpp definitions
#include "ieee_cpp_macros.h"

  type(IEEE_CLASS_TYPE), parameter, public ::  &
    IEEE_SIGNALING_NAN = IEEE_CLASS_TYPE(FP_NANS),  &
    IEEE_QUIET_NAN = IEEE_CLASS_TYPE(FP_NAN),  &
    IEEE_NEGATIVE_INF = IEEE_CLASS_TYPE(-FP_INFINITE),  &
    IEEE_NEGATIVE_NORMAL = IEEE_CLASS_TYPE(-FP_NORMAL),  &
    IEEE_NEGATIVE_DENORMAL = IEEE_CLASS_TYPE(-FP_SUBNORMAL),  &
    IEEE_NEGATIVE_ZERO = IEEE_CLASS_TYPE(-FP_ZERO),  &
    IEEE_POSITIVE_ZERO = IEEE_CLASS_TYPE(FP_ZERO),  &
    IEEE_POSITIVE_DENORMAL = IEEE_CLASS_TYPE(FP_SUBNORMAL),  &
    IEEE_POSITIVE_NORMAL = IEEE_CLASS_TYPE(FP_NORMAL),  &
    IEEE_POSITIVE_INF = IEEE_CLASS_TYPE(FP_INFINITE), &
    IEEE_OTHER_VALUE = IEEE_CLASS_TYPE(FP_OTHER)

  type(IEEE_ROUND_TYPE), parameter, public ::  &
    IEEE_NEAREST = IEEE_ROUND_TYPE(FE_TONEAREST),  &
    IEEE_TO_ZERO = IEEE_ROUND_TYPE(FE_TOWARDZERO),  &
    IEEE_UP = IEEE_ROUND_TYPE(FE_UPWARD),  &
    IEEE_DOWN = IEEE_ROUND_TYPE(FE_DOWNWARD),  &
    IEEE_OTHER = IEEE_ROUND_TYPE(-1)

  interface operator (==)
    module procedure test_equality_class, &
      test_equality_round
  end interface operator(==)

  interface operator(/=)
    module procedure test_inequality_class, &
      test_inequality_round
  end interface operator(/=)

  interface IEEE_SUPPORT_DATATYPE

    logical function ieee_support_datatype()
    end function ieee_support_datatype

    logical function ieee_support_datatype_4(X)
      real(kind=4) :: X
    end function ieee_support_datatype_4

    logical function ieee_support_datatype_4a(X)
      real(kind=4), dimension(*) :: X
    end function ieee_support_datatype_4a

    logical function ieee_support_datatype_4b(X)
      real(kind=4), dimension(:,:) :: X
    end function ieee_support_datatype_4b

    logical function ieee_support_datatype_4c(X)
      real(kind=4), dimension(:,:,:) :: X
    end function ieee_support_datatype_4c

    logical function ieee_support_datatype_4d(X)
      real(kind=4), dimension(:,:,:,:) :: X
    end function ieee_support_datatype_4d

    logical function ieee_support_datatype_4e(X)
      real(kind=4), dimension(:,:,:,:,:) :: X
    end function ieee_support_datatype_4e

    logical function ieee_support_datatype_4f(X)
      real(kind=4), dimension(:,:,:,:,:,:) :: X
    end function ieee_support_datatype_4f

    logical function ieee_support_datatype_4g(X)
      real(kind=4), dimension(:,:,:,:,:,:,:) :: X
    end function ieee_support_datatype_4g

    logical function ieee_support_datatype_8(X)
      real(kind=8) :: X
    end function ieee_support_datatype_8

    logical function ieee_support_datatype_8a(X)
      real(kind=8), dimension(*) :: X
    end function ieee_support_datatype_8a

    logical function ieee_support_datatype_8b(X)
      real(kind=8), dimension(:,:) :: X
    end function ieee_support_datatype_8b

    logical function ieee_support_datatype_8c(X)
      real(kind=8), dimension(:,:,:) :: X
    end function ieee_support_datatype_8c

    logical function ieee_support_datatype_8d(X)
      real(kind=8), dimension(:,:,:,:) :: X
    end function ieee_support_datatype_8d

    logical function ieee_support_datatype_8e(X)
      real(kind=8), dimension(:,:,:,:,:) :: X
    end function ieee_support_datatype_8e

    logical function ieee_support_datatype_8f(X)
      real(kind=8), dimension(:,:,:,:,:,:) :: X
    end function ieee_support_datatype_8f

    logical function ieee_support_datatype_8g(X)
      real(kind=8), dimension(:,:,:,:,:,:,:) :: X
    end function ieee_support_datatype_8g

#ifdef REAL16
    logical function ieee_support_datatype_16(X)
      real(kind=16) :: X
    end function ieee_support_datatype_16

    logical function ieee_support_datatype_16a(X)
      real(kind=16), dimension(*) :: X
    end function ieee_support_datatype_16a

    logical function ieee_support_datatype_16b(X)
      real(kind=16), dimension(:,:) :: X
    end function ieee_support_datatype_16b

    logical function ieee_support_datatype_16c(X)
      real(kind=16), dimension(:,:,:) :: X
    end function ieee_support_datatype_16c

    logical function ieee_support_datatype_16d(X)
      real(kind=16), dimension(:,:,:,:) :: X
    end function ieee_support_datatype_16d

    logical function ieee_support_datatype_16e(X)
      real(kind=16), dimension(:,:,:,:,:) :: X
    end function ieee_support_datatype_16e

    logical function ieee_support_datatype_16f(X)
      real(kind=16), dimension(:,:,:,:,:,:) :: X
    end function ieee_support_datatype_16f

    logical function ieee_support_datatype_16g(X)
      real(kind=16), dimension(:,:,:,:,:,:,:) :: X
    end function ieee_support_datatype_16g
#endif /* REAL16 */

  end interface IEEE_SUPPORT_DATATYPE

  interface IEEE_SUPPORT_DENORMAL

    logical function ieee_support_denormal()
    end function ieee_support_denormal

    logical function ieee_support_denormal_4(X)
      real(kind=4) :: X
    end function ieee_support_denormal_4

    logical function ieee_support_denormal_4a(X)
      real(kind=4), dimension(*) :: X
    end function ieee_support_denormal_4a

    logical function ieee_support_denormal_4b(X)
      real(kind=4), dimension(:,:) :: X
    end function ieee_support_denormal_4b

    logical function ieee_support_denormal_4c(X)
      real(kind=4), dimension(:,:,:) :: X
    end function ieee_support_denormal_4c

    logical function ieee_support_denormal_4d(X)
      real(kind=4), dimension(:,:,:,:) :: X
    end function ieee_support_denormal_4d

    logical function ieee_support_denormal_4e(X)
      real(kind=4), dimension(:,:,:,:,:) :: X
    end function ieee_support_denormal_4e

    logical function ieee_support_denormal_4f(X)
      real(kind=4), dimension(:,:,:,:,:,:) :: X
    end function ieee_support_denormal_4f

    logical function ieee_support_denormal_4g(X)
      real(kind=4), dimension(:,:,:,:,:,:,:) :: X
    end function ieee_support_denormal_4g

    logical function ieee_support_denormal_8(X)
      real(kind=8) :: X
    end function ieee_support_denormal_8

    logical function ieee_support_denormal_8a(X)
      real(kind=8), dimension(*) :: X
    end function ieee_support_denormal_8a

    logical function ieee_support_denormal_8b(X)
      real(kind=8), dimension(:,:) :: X
    end function ieee_support_denormal_8b

    logical function ieee_support_denormal_8c(X)
      real(kind=8), dimension(:,:,:) :: X
    end function ieee_support_denormal_8c

    logical function ieee_support_denormal_8d(X)
      real(kind=8), dimension(:,:,:,:) :: X
    end function ieee_support_denormal_8d

    logical function ieee_support_denormal_8e(X)
      real(kind=8), dimension(:,:,:,:,:) :: X
    end function ieee_support_denormal_8e

    logical function ieee_support_denormal_8f(X)
      real(kind=8), dimension(:,:,:,:,:,:) :: X
    end function ieee_support_denormal_8f

    logical function ieee_support_denormal_8g(X)
      real(kind=8), dimension(:,:,:,:,:,:,:) :: X
    end function ieee_support_denormal_8g

#ifdef REAL16
    logical function ieee_support_denormal_16(X)
      real(kind=16) :: X
    end function ieee_support_denormal_16

    logical function ieee_support_denormal_16a(X)
      real(kind=16), dimension(*) :: X
    end function ieee_support_denormal_16a

    logical function ieee_support_denormal_16b(X)
      real(kind=16), dimension(:,:) :: X
    end function ieee_support_denormal_16b

    logical function ieee_support_denormal_16c(X)
      real(kind=16), dimension(:,:,:) :: X
    end function ieee_support_denormal_16c

    logical function ieee_support_denormal_16d(X)
      real(kind=16), dimension(:,:,:,:) :: X
    end function ieee_support_denormal_16d

    logical function ieee_support_denormal_16e(X)
      real(kind=16), dimension(:,:,:,:,:) :: X
    end function ieee_support_denormal_16e

    logical function ieee_support_denormal_16f(X)
      real(kind=16), dimension(:,:,:,:,:,:) :: X
    end function ieee_support_denormal_16f

    logical function ieee_support_denormal_16g(X)
      real(kind=16), dimension(:,:,:,:,:,:,:) :: X
    end function ieee_support_denormal_16g
#endif /* REAL16 */

  end interface IEEE_SUPPORT_DENORMAL

  interface IEEE_SUPPORT_DIVIDE

    logical function ieee_support_divide()
    end function ieee_support_divide

    logical function ieee_support_divide_4(X)
      real(kind=4) :: X
    end function ieee_support_divide_4

    logical function ieee_support_divide_4a(X)
      real(kind=4), dimension(*) :: X
    end function ieee_support_divide_4a

    logical function ieee_support_divide_4b(X)
      real(kind=4), dimension(:,:) :: X
    end function ieee_support_divide_4b

    logical function ieee_support_divide_4c(X)
      real(kind=4), dimension(:,:,:) :: X
    end function ieee_support_divide_4c

    logical function ieee_support_divide_4d(X)
      real(kind=4), dimension(:,:,:,:) :: X
    end function ieee_support_divide_4d

    logical function ieee_support_divide_4e(X)
      real(kind=4), dimension(:,:,:,:,:) :: X
    end function ieee_support_divide_4e

    logical function ieee_support_divide_4f(X)
      real(kind=4), dimension(:,:,:,:,:,:) :: X
    end function ieee_support_divide_4f

    logical function ieee_support_divide_4g(X)
      real(kind=4), dimension(:,:,:,:,:,:,:) :: X
    end function ieee_support_divide_4g

    logical function ieee_support_divide_8(X)
      real(kind=8) :: X
    end function ieee_support_divide_8

    logical function ieee_support_divide_8a(X)
      real(kind=8), dimension(*) :: X
    end function ieee_support_divide_8a

    logical function ieee_support_divide_8b(X)
      real(kind=8), dimension(:,:) :: X
    end function ieee_support_divide_8b

    logical function ieee_support_divide_8c(X)
      real(kind=8), dimension(:,:,:) :: X
    end function ieee_support_divide_8c

    logical function ieee_support_divide_8d(X)
      real(kind=8), dimension(:,:,:,:) :: X
    end function ieee_support_divide_8d

    logical function ieee_support_divide_8e(X)
      real(kind=8), dimension(:,:,:,:,:) :: X
    end function ieee_support_divide_8e

    logical function ieee_support_divide_8f(X)
      real(kind=8), dimension(:,:,:,:,:,:) :: X
    end function ieee_support_divide_8f

    logical function ieee_support_divide_8g(X)
      real(kind=8), dimension(:,:,:,:,:,:,:) :: X
    end function ieee_support_divide_8g

#ifdef REAL16
    logical function ieee_support_divide_16(X)
      real(kind=16) :: X
    end function ieee_support_divide_16

    logical function ieee_support_divide_16a(X)
      real(kind=16), dimension(*) :: X
    end function ieee_support_divide_16a

    logical function ieee_support_divide_16b(X)
      real(kind=16), dimension(:,:) :: X
    end function ieee_support_divide_16b

    logical function ieee_support_divide_16c(X)
      real(kind=16), dimension(:,:,:) :: X
    end function ieee_support_divide_16c

    logical function ieee_support_divide_16d(X)
      real(kind=16), dimension(:,:,:,:) :: X
    end function ieee_support_divide_16d

    logical function ieee_support_divide_16e(X)
      real(kind=16), dimension(:,:,:,:,:) :: X
    end function ieee_support_divide_16e

    logical function ieee_support_divide_16f(X)
      real(kind=16), dimension(:,:,:,:,:,:) :: X
    end function ieee_support_divide_16f

    logical function ieee_support_divide_16g(X)
      real(kind=16), dimension(:,:,:,:,:,:,:) :: X
    end function ieee_support_divide_16g
#endif /* REAL16 */

  end interface IEEE_SUPPORT_DIVIDE

  interface IEEE_SUPPORT_INF

    logical function ieee_support_inf()
    end function ieee_support_inf

    logical function ieee_support_inf_4(X)
      real(kind=4) :: X
    end function ieee_support_inf_4

    logical function ieee_support_inf_4a(X)
      real(kind=4), dimension(*) :: X
    end function ieee_support_inf_4a

    logical function ieee_support_inf_4b(X)
      real(kind=4), dimension(:,:) :: X
    end function ieee_support_inf_4b

    logical function ieee_support_inf_4c(X)
      real(kind=4), dimension(:,:,:) :: X
    end function ieee_support_inf_4c

    logical function ieee_support_inf_4d(X)
      real(kind=4), dimension(:,:,:,:) :: X
    end function ieee_support_inf_4d

    logical function ieee_support_inf_4e(X)
      real(kind=4), dimension(:,:,:,:,:) :: X
    end function ieee_support_inf_4e

    logical function ieee_support_inf_4f(X)
      real(kind=4), dimension(:,:,:,:,:,:) :: X
    end function ieee_support_inf_4f

    logical function ieee_support_inf_4g(X)
      real(kind=4), dimension(:,:,:,:,:,:,:) :: X
    end function ieee_support_inf_4g

    logical function ieee_support_inf_8(X)
      real(kind=8) :: X
    end function ieee_support_inf_8

    logical function ieee_support_inf_8a(X)
      real(kind=8), dimension(*) :: X
    end function ieee_support_inf_8a

    logical function ieee_support_inf_8b(X)
      real(kind=8), dimension(:,:) :: X
    end function ieee_support_inf_8b

    logical function ieee_support_inf_8c(X)
      real(kind=8), dimension(:,:,:) :: X
    end function ieee_support_inf_8c

    logical function ieee_support_inf_8d(X)
      real(kind=8), dimension(:,:,:,:) :: X
    end function ieee_support_inf_8d

    logical function ieee_support_inf_8e(X)
      real(kind=8), dimension(:,:,:,:,:) :: X
    end function ieee_support_inf_8e

    logical function ieee_support_inf_8f(X)
      real(kind=8), dimension(:,:,:,:,:,:) :: X
    end function ieee_support_inf_8f

    logical function ieee_support_inf_8g(X)
      real(kind=8), dimension(:,:,:,:,:,:,:) :: X
    end function ieee_support_inf_8g

#ifdef REAL16
    logical function ieee_support_inf_16(X)
      real(kind=16) :: X
    end function ieee_support_inf_16

    logical function ieee_support_inf_16a(X)
      real(kind=16), dimension(*) :: X
    end function ieee_support_inf_16a

    logical function ieee_support_inf_16b(X)
      real(kind=16), dimension(:,:) :: X
    end function ieee_support_inf_16b

    logical function ieee_support_inf_16c(X)
      real(kind=16), dimension(:,:,:) :: X
    end function ieee_support_inf_16c

    logical function ieee_support_inf_16d(X)
      real(kind=16), dimension(:,:,:,:) :: X
    end function ieee_support_inf_16d

    logical function ieee_support_inf_16e(X)
      real(kind=16), dimension(:,:,:,:,:) :: X
    end function ieee_support_inf_16e

    logical function ieee_support_inf_16f(X)
      real(kind=16), dimension(:,:,:,:,:,:) :: X
    end function ieee_support_inf_16f

    logical function ieee_support_inf_16g(X)
      real(kind=16), dimension(:,:,:,:,:,:,:) :: X
    end function ieee_support_inf_16g
#endif /* REAL16 */

  end interface IEEE_SUPPORT_INF

  interface IEEE_SUPPORT_IO

    logical function ieee_support_io()
    end function ieee_support_io

    logical function ieee_support_io_4(X)
      real(kind=4) :: X
    end function ieee_support_io_4

    logical function ieee_support_io_4a(X)
      real(kind=4), dimension(*) :: X
    end function ieee_support_io_4a

    logical function ieee_support_io_4b(X)
      real(kind=4), dimension(:,:) :: X
    end function ieee_support_io_4b

    logical function ieee_support_io_4c(X)
      real(kind=4), dimension(:,:,:) :: X
    end function ieee_support_io_4c

    logical function ieee_support_io_4d(X)
      real(kind=4), dimension(:,:,:,:) :: X
    end function ieee_support_io_4d

    logical function ieee_support_io_4e(X)
      real(kind=4), dimension(:,:,:,:,:) :: X
    end function ieee_support_io_4e

    logical function ieee_support_io_4f(X)
      real(kind=4), dimension(:,:,:,:,:,:) :: X
    end function ieee_support_io_4f

    logical function ieee_support_io_4g(X)
      real(kind=4), dimension(:,:,:,:,:,:,:) :: X
    end function ieee_support_io_4g

    logical function ieee_support_io_8(X)
      real(kind=8) :: X
    end function ieee_support_io_8

    logical function ieee_support_io_8a(X)
      real(kind=8), dimension(*) :: X
    end function ieee_support_io_8a

    logical function ieee_support_io_8b(X)
      real(kind=8), dimension(:,:) :: X
    end function ieee_support_io_8b

    logical function ieee_support_io_8c(X)
      real(kind=8), dimension(:,:,:) :: X
    end function ieee_support_io_8c

    logical function ieee_support_io_8d(X)
      real(kind=8), dimension(:,:,:,:) :: X
    end function ieee_support_io_8d

    logical function ieee_support_io_8e(X)
      real(kind=8), dimension(:,:,:,:,:) :: X
    end function ieee_support_io_8e

    logical function ieee_support_io_8f(X)
      real(kind=8), dimension(:,:,:,:,:,:) :: X
    end function ieee_support_io_8f

    logical function ieee_support_io_8g(X)
      real(kind=8), dimension(:,:,:,:,:,:,:) :: X
    end function ieee_support_io_8g

#ifdef REAL16
    logical function ieee_support_io_16(X)
      real(kind=16) :: X
    end function ieee_support_io_16

    logical function ieee_support_io_16a(X)
      real(kind=16), dimension(*) :: X
    end function ieee_support_io_16a

    logical function ieee_support_io_16b(X)
      real(kind=16), dimension(:,:) :: X
    end function ieee_support_io_16b

    logical function ieee_support_io_16c(X)
      real(kind=16), dimension(:,:,:) :: X
    end function ieee_support_io_16c

    logical function ieee_support_io_16d(X)
      real(kind=16), dimension(:,:,:,:) :: X
    end function ieee_support_io_16d

    logical function ieee_support_io_16e(X)
      real(kind=16), dimension(:,:,:,:,:) :: X
    end function ieee_support_io_16e

    logical function ieee_support_io_16f(X)
      real(kind=16), dimension(:,:,:,:,:,:) :: X
    end function ieee_support_io_16f

    logical function ieee_support_io_16g(X)
      real(kind=16), dimension(:,:,:,:,:,:,:) :: X
    end function ieee_support_io_16g
#endif /* REAL16 */

  end interface IEEE_SUPPORT_IO

  interface IEEE_SUPPORT_NAN

    logical function ieee_support_nan()
    end function ieee_support_nan

    logical function ieee_support_nan_4(X)
      real(kind=4) :: X
    end function ieee_support_nan_4

    logical function ieee_support_nan_4a(X)
      real(kind=4), dimension(*) :: X
    end function ieee_support_nan_4a

    logical function ieee_support_nan_4b(X)
      real(kind=4), dimension(:,:) :: X
    end function ieee_support_nan_4b

    logical function ieee_support_nan_4c(X)
      real(kind=4), dimension(:,:,:) :: X
    end function ieee_support_nan_4c

    logical function ieee_support_nan_4d(X)
      real(kind=4), dimension(:,:,:,:) :: X
    end function ieee_support_nan_4d

    logical function ieee_support_nan_4e(X)
      real(kind=4), dimension(:,:,:,:,:) :: X
    end function ieee_support_nan_4e

    logical function ieee_support_nan_4f(X)
      real(kind=4), dimension(:,:,:,:,:,:) :: X
    end function ieee_support_nan_4f

    logical function ieee_support_nan_4g(X)
      real(kind=4), dimension(:,:,:,:,:,:,:) :: X
    end function ieee_support_nan_4g

    logical function ieee_support_nan_8(X)
      real(kind=8) :: X
    end function ieee_support_nan_8

    logical function ieee_support_nan_8a(X)
      real(kind=8), dimension(*) :: X
    end function ieee_support_nan_8a

    logical function ieee_support_nan_8b(X)
      real(kind=8), dimension(:,:) :: X
    end function ieee_support_nan_8b

    logical function ieee_support_nan_8c(X)
      real(kind=8), dimension(:,:,:) :: X
    end function ieee_support_nan_8c

    logical function ieee_support_nan_8d(X)
      real(kind=8), dimension(:,:,:,:) :: X
    end function ieee_support_nan_8d

    logical function ieee_support_nan_8e(X)
      real(kind=8), dimension(:,:,:,:,:) :: X
    end function ieee_support_nan_8e

    logical function ieee_support_nan_8f(X)
      real(kind=8), dimension(:,:,:,:,:,:) :: X
    end function ieee_support_nan_8f

    logical function ieee_support_nan_8g(X)
      real(kind=8), dimension(:,:,:,:,:,:,:) :: X
    end function ieee_support_nan_8g

#ifdef REAL16
    logical function ieee_support_nan_16(X)
      real(kind=16) :: X
    end function ieee_support_nan_16

    logical function ieee_support_nan_16a(X)
      real(kind=16), dimension(*) :: X
    end function ieee_support_nan_16a

    logical function ieee_support_nan_16b(X)
      real(kind=16), dimension(:,:) :: X
    end function ieee_support_nan_16b

    logical function ieee_support_nan_16c(X)
      real(kind=16), dimension(:,:,:) :: X
    end function ieee_support_nan_16c

    logical function ieee_support_nan_16d(X)
      real(kind=16), dimension(:,:,:,:) :: X
    end function ieee_support_nan_16d

    logical function ieee_support_nan_16e(X)
      real(kind=16), dimension(:,:,:,:,:) :: X
    end function ieee_support_nan_16e

    logical function ieee_support_nan_16f(X)
      real(kind=16), dimension(:,:,:,:,:,:) :: X
    end function ieee_support_nan_16f

    logical function ieee_support_nan_16g(X)
      real(kind=16), dimension(:,:,:,:,:,:,:) :: X
    end function ieee_support_nan_16g
#endif /* REAL16 */

  end interface IEEE_SUPPORT_NAN

  interface IEEE_SUPPORT_ROUNDING

    logical function ieee_support_rounding(ROUND_VALUE)
      include "ieee_arithmetic_types.h"
      type(ieee_round_type), intent(in) :: ROUND_VALUE
    end function ieee_support_rounding

    logical function ieee_support_rounding_4(ROUND_VALUE, X)
      include "ieee_arithmetic_types.h"
      type(ieee_round_type), intent(in) :: ROUND_VALUE
      real(kind=4) :: X
    end function ieee_support_rounding_4

    logical function ieee_support_rounding_4a(ROUND_VALUE, X)
      include "ieee_arithmetic_types.h"
      type(ieee_round_type), intent(in) :: ROUND_VALUE
      real(kind=4), dimension(*) :: X
    end function ieee_support_rounding_4a

    logical function ieee_support_rounding_4b(ROUND_VALUE, X)
      include "ieee_arithmetic_types.h"
      type(ieee_round_type), intent(in) :: ROUND_VALUE
      real(kind=4), dimension(:,:) :: X
    end function ieee_support_rounding_4b

    logical function ieee_support_rounding_4c(ROUND_VALUE, X)
      include "ieee_arithmetic_types.h"
      type(ieee_round_type), intent(in) :: ROUND_VALUE
      real(kind=4), dimension(:,:,:) :: X
    end function ieee_support_rounding_4c

    logical function ieee_support_rounding_4d(ROUND_VALUE, X)
      include "ieee_arithmetic_types.h"
      type(ieee_round_type), intent(in) :: ROUND_VALUE
      real(kind=4), dimension(:,:,:,:) :: X
    end function ieee_support_rounding_4d

    logical function ieee_support_rounding_4e(ROUND_VALUE, X)
      include "ieee_arithmetic_types.h"
      type(ieee_round_type), intent(in) :: ROUND_VALUE
      real(kind=4), dimension(:,:,:,:,:) :: X
    end function ieee_support_rounding_4e

    logical function ieee_support_rounding_4f(ROUND_VALUE, X)
      include "ieee_arithmetic_types.h"
      type(ieee_round_type), intent(in) :: ROUND_VALUE
      real(kind=4), dimension(:,:,:,:,:,:) :: X
    end function ieee_support_rounding_4f

    logical function ieee_support_rounding_4g(ROUND_VALUE, X)
      include "ieee_arithmetic_types.h"
      type(ieee_round_type), intent(in) :: ROUND_VALUE
      real(kind=4), dimension(:,:,:,:,:,:,:) :: X
    end function ieee_support_rounding_4g

    logical function ieee_support_rounding_8(ROUND_VALUE, X)
      include "ieee_arithmetic_types.h"
      type(ieee_round_type), intent(in) :: ROUND_VALUE
      real(kind=8) :: X
    end function ieee_support_rounding_8

    logical function ieee_support_rounding_8a(ROUND_VALUE, X)
      include "ieee_arithmetic_types.h"
      type(ieee_round_type), intent(in) :: ROUND_VALUE
      real(kind=8), dimension(*) :: X
    end function ieee_support_rounding_8a

    logical function ieee_support_rounding_8b(ROUND_VALUE, X)
      include "ieee_arithmetic_types.h"
      type(ieee_round_type), intent(in) :: ROUND_VALUE
      real(kind=8), dimension(:,:) :: X
    end function ieee_support_rounding_8b

    logical function ieee_support_rounding_8c(ROUND_VALUE, X)
      include "ieee_arithmetic_types.h"
      type(ieee_round_type), intent(in) :: ROUND_VALUE
      real(kind=8), dimension(:,:,:) :: X
    end function ieee_support_rounding_8c

    logical function ieee_support_rounding_8d(ROUND_VALUE, X)
      include "ieee_arithmetic_types.h"
      type(ieee_round_type), intent(in) :: ROUND_VALUE
      real(kind=8), dimension(:,:,:,:) :: X
    end function ieee_support_rounding_8d

    logical function ieee_support_rounding_8e(ROUND_VALUE, X)
      include "ieee_arithmetic_types.h"
      type(ieee_round_type), intent(in) :: ROUND_VALUE
      real(kind=8), dimension(:,:,:,:,:) :: X
    end function ieee_support_rounding_8e

    logical function ieee_support_rounding_8f(ROUND_VALUE, X)
      include "ieee_arithmetic_types.h"
      type(ieee_round_type), intent(in) :: ROUND_VALUE
      real(kind=8), dimension(:,:,:,:,:,:) :: X
    end function ieee_support_rounding_8f

    logical function ieee_support_rounding_8g(ROUND_VALUE, X)
      include "ieee_arithmetic_types.h"
      type(ieee_round_type), intent(in) :: ROUND_VALUE
      real(kind=8), dimension(:,:,:,:,:,:,:) :: X
    end function ieee_support_rounding_8g

#ifdef REAL16
    logical function ieee_support_rounding_16(ROUND_VALUE, X)
      include "ieee_arithmetic_types.h"
      type(ieee_round_type), intent(in) :: ROUND_VALUE
      real(kind=16) :: X
    end function ieee_support_rounding_16

    logical function ieee_support_rounding_16a(ROUND_VALUE, X)
      include "ieee_arithmetic_types.h"
      type(ieee_round_type), intent(in) :: ROUND_VALUE
      real(kind=16), dimension(*) :: X
    end function ieee_support_rounding_16a

    logical function ieee_support_rounding_16b(ROUND_VALUE, X)
      include "ieee_arithmetic_types.h"
      type(ieee_round_type), intent(in) :: ROUND_VALUE
      real(kind=16), dimension(:,:) :: X
    end function ieee_support_rounding_16b

    logical function ieee_support_rounding_16c(ROUND_VALUE, X)
      include "ieee_arithmetic_types.h"
      type(ieee_round_type), intent(in) :: ROUND_VALUE
      real(kind=16), dimension(:,:,:) :: X
    end function ieee_support_rounding_16c

    logical function ieee_support_rounding_16d(ROUND_VALUE, X)
      include "ieee_arithmetic_types.h"
      type(ieee_round_type), intent(in) :: ROUND_VALUE
      real(kind=16), dimension(:,:,:,:) :: X
    end function ieee_support_rounding_16d

    logical function ieee_support_rounding_16e(ROUND_VALUE, X)
      include "ieee_arithmetic_types.h"
      type(ieee_round_type), intent(in) :: ROUND_VALUE
      real(kind=16), dimension(:,:,:,:,:) :: X
    end function ieee_support_rounding_16e

    logical function ieee_support_rounding_16f(ROUND_VALUE, X)
      include "ieee_arithmetic_types.h"
      type(ieee_round_type), intent(in) :: ROUND_VALUE
      real(kind=16), dimension(:,:,:,:,:,:) :: X
    end function ieee_support_rounding_16f

    logical function ieee_support_rounding_16g(ROUND_VALUE, X)
      include "ieee_arithmetic_types.h"
      type(ieee_round_type), intent(in) :: ROUND_VALUE
      real(kind=16), dimension(:,:,:,:,:,:,:) :: X
    end function ieee_support_rounding_16g
#endif /* REAL16 */

  end interface IEEE_SUPPORT_ROUNDING

  interface IEEE_SUPPORT_SQRT

    logical function ieee_support_sqrt()
    end function ieee_support_sqrt

    logical function ieee_support_sqrt_4(X)
      real(kind=4) :: X
    end function ieee_support_sqrt_4

    logical function ieee_support_sqrt_4a(X)
      real(kind=4), dimension(*) :: X
    end function ieee_support_sqrt_4a

    logical function ieee_support_sqrt_4b(X)
      real(kind=4), dimension(:,:) :: X
    end function ieee_support_sqrt_4b

    logical function ieee_support_sqrt_4c(X)
      real(kind=4), dimension(:,:,:) :: X
    end function ieee_support_sqrt_4c

    logical function ieee_support_sqrt_4d(X)
      real(kind=4), dimension(:,:,:,:) :: X
    end function ieee_support_sqrt_4d

    logical function ieee_support_sqrt_4e(X)
      real(kind=4), dimension(:,:,:,:,:) :: X
    end function ieee_support_sqrt_4e

    logical function ieee_support_sqrt_4f(X)
      real(kind=4), dimension(:,:,:,:,:,:) :: X
    end function ieee_support_sqrt_4f

    logical function ieee_support_sqrt_4g(X)
      real(kind=4), dimension(:,:,:,:,:,:,:) :: X
    end function ieee_support_sqrt_4g

    logical function ieee_support_sqrt_8(X)
      real(kind=8) :: X
    end function ieee_support_sqrt_8

    logical function ieee_support_sqrt_8a(X)
      real(kind=8), dimension(*) :: X
    end function ieee_support_sqrt_8a

    logical function ieee_support_sqrt_8b(X)
      real(kind=8), dimension(:,:) :: X
    end function ieee_support_sqrt_8b

    logical function ieee_support_sqrt_8c(X)
      real(kind=8), dimension(:,:,:) :: X
    end function ieee_support_sqrt_8c

    logical function ieee_support_sqrt_8d(X)
      real(kind=8), dimension(:,:,:,:) :: X
    end function ieee_support_sqrt_8d

    logical function ieee_support_sqrt_8e(X)
      real(kind=8), dimension(:,:,:,:,:) :: X
    end function ieee_support_sqrt_8e

    logical function ieee_support_sqrt_8f(X)
      real(kind=8), dimension(:,:,:,:,:,:) :: X
    end function ieee_support_sqrt_8f

    logical function ieee_support_sqrt_8g(X)
      real(kind=8), dimension(:,:,:,:,:,:,:) :: X
    end function ieee_support_sqrt_8g

#ifdef REAL16
    logical function ieee_support_sqrt_16(X)
      real(kind=16) :: X
    end function ieee_support_sqrt_16

    logical function ieee_support_sqrt_16a(X)
      real(kind=16), dimension(*) :: X
    end function ieee_support_sqrt_16a

    logical function ieee_support_sqrt_16b(X)
      real(kind=16), dimension(:,:) :: X
    end function ieee_support_sqrt_16b

    logical function ieee_support_sqrt_16c(X)
      real(kind=16), dimension(:,:,:) :: X
    end function ieee_support_sqrt_16c

    logical function ieee_support_sqrt_16d(X)
      real(kind=16), dimension(:,:,:,:) :: X
    end function ieee_support_sqrt_16d

    logical function ieee_support_sqrt_16e(X)
      real(kind=16), dimension(:,:,:,:,:) :: X
    end function ieee_support_sqrt_16e

    logical function ieee_support_sqrt_16f(X)
      real(kind=16), dimension(:,:,:,:,:,:) :: X
    end function ieee_support_sqrt_16f

    logical function ieee_support_sqrt_16g(X)
      real(kind=16), dimension(:,:,:,:,:,:,:) :: X
    end function ieee_support_sqrt_16g
#endif /* REAL16 */

  end interface IEEE_SUPPORT_SQRT

  interface IEEE_SUPPORT_STANDARD

    logical function ieee_support_standard()
    end function ieee_support_standard

    logical function ieee_support_standard_4(X)
      real(kind=4) :: X
    end function ieee_support_standard_4

    logical function ieee_support_standard_4a(X)
      real(kind=4), dimension(*) :: X
    end function ieee_support_standard_4a

    logical function ieee_support_standard_4b(X)
      real(kind=4), dimension(:,:) :: X
    end function ieee_support_standard_4b

    logical function ieee_support_standard_4c(X)
      real(kind=4), dimension(:,:,:) :: X
    end function ieee_support_standard_4c

    logical function ieee_support_standard_4d(X)
      real(kind=4), dimension(:,:,:,:) :: X
    end function ieee_support_standard_4d

    logical function ieee_support_standard_4e(X)
      real(kind=4), dimension(:,:,:,:,:) :: X
    end function ieee_support_standard_4e

    logical function ieee_support_standard_4f(X)
      real(kind=4), dimension(:,:,:,:,:,:) :: X
    end function ieee_support_standard_4f

    logical function ieee_support_standard_4g(X)
      real(kind=4), dimension(:,:,:,:,:,:,:) :: X
    end function ieee_support_standard_4g

    logical function ieee_support_standard_8(X)
      real(kind=8) :: X
    end function ieee_support_standard_8

    logical function ieee_support_standard_8a(X)
      real(kind=8), dimension(*) :: X
    end function ieee_support_standard_8a

    logical function ieee_support_standard_8b(X)
      real(kind=8), dimension(:,:) :: X
    end function ieee_support_standard_8b

    logical function ieee_support_standard_8c(X)
      real(kind=8), dimension(:,:,:) :: X
    end function ieee_support_standard_8c

    logical function ieee_support_standard_8d(X)
      real(kind=8), dimension(:,:,:,:) :: X
    end function ieee_support_standard_8d

    logical function ieee_support_standard_8e(X)
      real(kind=8), dimension(:,:,:,:,:) :: X
    end function ieee_support_standard_8e

    logical function ieee_support_standard_8f(X)
      real(kind=8), dimension(:,:,:,:,:,:) :: X
    end function ieee_support_standard_8f

    logical function ieee_support_standard_8g(X)
      real(kind=8), dimension(:,:,:,:,:,:,:) :: X
    end function ieee_support_standard_8g

#ifdef REAL16
    logical function ieee_support_standard_16(X)
      real(kind=16) :: X
    end function ieee_support_standard_16

    logical function ieee_support_standard_16a(X)
      real(kind=16), dimension(*) :: X
    end function ieee_support_standard_16a

    logical function ieee_support_standard_16b(X)
      real(kind=16), dimension(:,:) :: X
    end function ieee_support_standard_16b

    logical function ieee_support_standard_16c(X)
      real(kind=16), dimension(:,:,:) :: X
    end function ieee_support_standard_16c

    logical function ieee_support_standard_16d(X)
      real(kind=16), dimension(:,:,:,:) :: X
    end function ieee_support_standard_16d

    logical function ieee_support_standard_16e(X)
      real(kind=16), dimension(:,:,:,:,:) :: X
    end function ieee_support_standard_16e

    logical function ieee_support_standard_16f(X)
      real(kind=16), dimension(:,:,:,:,:,:) :: X
    end function ieee_support_standard_16f

    logical function ieee_support_standard_16g(X)
      real(kind=16), dimension(:,:,:,:,:,:,:) :: X
    end function ieee_support_standard_16g
#endif /* REAL16 */

  end interface IEEE_SUPPORT_STANDARD

  interface IEEE_SUPPORT_UNDERFLOW_CONTROL

    logical function ieee_support_underflow_control()
    end function ieee_support_underflow_control

    logical function ieee_support_underflow_control_4(X)
      real(kind=4) :: X
    end function ieee_support_underflow_control_4

    logical function ieee_support_underflow_control_4a(X)
      real(kind=4), dimension(*) :: X
    end function ieee_support_underflow_control_4a

    logical function ieee_support_underflow_control_4b(X)
      real(kind=4), dimension(:,:) :: X
    end function ieee_support_underflow_control_4b

    logical function ieee_support_underflow_control_4c(X)
      real(kind=4), dimension(:,:,:) :: X
    end function ieee_support_underflow_control_4c

    logical function ieee_support_underflow_control_4d(X)
      real(kind=4), dimension(:,:,:,:) :: X
    end function ieee_support_underflow_control_4d

    logical function ieee_support_underflow_control_4e(X)
      real(kind=4), dimension(:,:,:,:,:) :: X
    end function ieee_support_underflow_control_4e

    logical function ieee_support_underflow_control_4f(X)
      real(kind=4), dimension(:,:,:,:,:,:) :: X
    end function ieee_support_underflow_control_4f

    logical function ieee_support_underflow_control_4g(X)
      real(kind=4), dimension(:,:,:,:,:,:,:) :: X
    end function ieee_support_underflow_control_4g

    logical function ieee_support_underflow_control_8(X)
      real(kind=8) :: X
    end function ieee_support_underflow_control_8

    logical function ieee_support_underflow_control_8a(X)
      real(kind=8), dimension(*) :: X
    end function ieee_support_underflow_control_8a

    logical function ieee_support_underflow_control_8b(X)
      real(kind=8), dimension(:,:) :: X
    end function ieee_support_underflow_control_8b

    logical function ieee_support_underflow_control_8c(X)
      real(kind=8), dimension(:,:,:) :: X
    end function ieee_support_underflow_control_8c

    logical function ieee_support_underflow_control_8d(X)
      real(kind=8), dimension(:,:,:,:) :: X
    end function ieee_support_underflow_control_8d

    logical function ieee_support_underflow_control_8e(X)
      real(kind=8), dimension(:,:,:,:,:) :: X
    end function ieee_support_underflow_control_8e

    logical function ieee_support_underflow_control_8f(X)
      real(kind=8), dimension(:,:,:,:,:,:) :: X
    end function ieee_support_underflow_control_8f

    logical function ieee_support_underflow_control_8g(X)
      real(kind=8), dimension(:,:,:,:,:,:,:) :: X
    end function ieee_support_underflow_control_8g

#ifdef REAL16
    logical function ieee_support_underflow_control_16(X)
      real(kind=16) :: X
    end function ieee_support_underflow_control_16

    logical function ieee_support_underflow_control_16a(X)
      real(kind=16), dimension(*) :: X
    end function ieee_support_underflow_control_16a

    logical function ieee_support_underflow_control_16b(X)
      real(kind=16), dimension(:,:) :: X
    end function ieee_support_underflow_control_16b

    logical function ieee_support_underflow_control_16c(X)
      real(kind=16), dimension(:,:,:) :: X
    end function ieee_support_underflow_control_16c

    logical function ieee_support_underflow_control_16d(X)
      real(kind=16), dimension(:,:,:,:) :: X
    end function ieee_support_underflow_control_16d

    logical function ieee_support_underflow_control_16e(X)
      real(kind=16), dimension(:,:,:,:,:) :: X
    end function ieee_support_underflow_control_16e

    logical function ieee_support_underflow_control_16f(X)
      real(kind=16), dimension(:,:,:,:,:,:) :: X
    end function ieee_support_underflow_control_16f

    logical function ieee_support_underflow_control_16g(X)
      real(kind=16), dimension(:,:,:,:,:,:,:) :: X
    end function ieee_support_underflow_control_16g
#endif /* REAL16 */

  end interface IEEE_SUPPORT_UNDERFLOW_CONTROL

  interface IEEE_CLASS

    elemental type(IEEE_CLASS_TYPE) function ieee_class_4(X)
      include "ieee_arithmetic_types.h"
      real(kind=4), intent(in) :: X
    end function ieee_class_4

    elemental type(IEEE_CLASS_TYPE) function ieee_class_8(X)
      include "ieee_arithmetic_types.h"
      real(kind=8), intent(in) :: X
    end function ieee_class_8

#ifdef REAL16
    elemental type(IEEE_CLASS_TYPE) function ieee_class_16(X)
      include "ieee_arithmetic_types.h"
      real(kind=16), intent(in) :: X
    end function ieee_class_16
#endif /* REAL16 */

  end interface IEEE_CLASS

  interface IEEE_COPY_SIGN

    elemental real(kind=4) function ieee_copy_sign_4(X, Y)
      real(kind=4), intent(in) :: X, Y
    end function ieee_copy_sign_4

    elemental real(kind=4) function ieee_copy_sign_4_8(X, Y)
      real(kind=4), intent(in) :: X
      real(kind=8), intent(in) :: Y
    end function ieee_copy_sign_4_8

    elemental real(kind=8) function ieee_copy_sign_8_4(X, Y)
      real(kind=8), intent(in) :: X
      real(kind=4), intent(in) :: Y
    end function ieee_copy_sign_8_4

    elemental real(kind=8) function ieee_copy_sign_8(X, Y)
      real(kind=8), intent(in) :: X, Y
    end function ieee_copy_sign_8

#ifdef REAL16
    elemental real(kind=16) function ieee_copy_sign_16(X, Y)
      real(kind=16), intent(in) :: X, Y
    end function ieee_copy_sign_16

    elemental real(kind=16) function ieee_copy_sign_16_4(X, Y)
      real(kind=16), intent(in) :: X
      real(kind=4), intent(in) :: Y
    end function ieee_copy_sign_16_4

    elemental real(kind=4) function ieee_copy_sign_4_16(X, Y)
      real(kind=4), intent(in) :: X
      real(kind=16), intent(in) :: Y
    end function ieee_copy_sign_16_4

    elemental real(kind=16) function ieee_copy_sign_16_8(X, Y)
      real(kind=16), intent(in) :: X
      real(kind=8), intent(in) :: Y
    end function ieee_copy_sign_16_8

    elemental real(kind=8) function ieee_copy_sign_8_16(X, Y)
      real(kind=8), intent(in) :: X
      real(kind=16), intent(in) :: Y
    end function ieee_copy_sign_16_8
#endif /* REAL16 */

  end interface IEEE_COPY_SIGN

  interface IEEE_IS_FINITE

    elemental logical function ieee_is_finite_4(X)
      real(kind=4), intent(in) :: X
    end function ieee_is_finite_4

    elemental logical function ieee_is_finite_8(X)
      real(kind=8), intent(in) :: X
    end function ieee_is_finite_8

#ifdef REAL16
    elemental logical function ieee_is_finite_16(X)
      real(kind=16), intent(in) :: X
    end function ieee_is_finite_16
#endif /* REAL16 */

  end interface IEEE_IS_FINITE

  interface IEEE_IS_NAN

    elemental logical function ieee_is_nan_4(X)
      real(kind=4), intent(in) :: X
    end function ieee_is_nan_4

    elemental logical function ieee_is_nan_8(X)
      real(kind=8), intent(in) :: X
    end function ieee_is_nan_8

#ifdef REAL16
    elemental logical function ieee_is_nan_16(X)
      real(kind=16), intent(in) :: X
    end function ieee_is_nan_16
#endif /* REAL16 */

  end interface IEEE_IS_NAN

  interface IEEE_IS_NEGATIVE

    elemental logical function ieee_is_negative_4(X)
      real(kind=4), intent(in) :: X
    end function ieee_is_negative_4

    elemental logical function ieee_is_negative_8(X)
      real(kind=8), intent(in) :: X
    end function ieee_is_negative_8

#ifdef REAL16
    elemental logical function ieee_is_negative_16(X)
      real(kind=16), intent(in) :: X
    end function ieee_is_negative_16
#endif /* REAL16 */

  end interface IEEE_IS_NEGATIVE

  interface IEEE_IS_NORMAL

    elemental logical function ieee_is_normal_4(X)
      real(kind=4), intent(in) :: X
    end function ieee_is_normal_4

    elemental logical function ieee_is_normal_8(X)
      real(kind=8), intent(in) :: X
    end function ieee_is_normal_8

#ifdef REAL16
    elemental logical function ieee_is_normal_16(X)
      real(kind=16), intent(in) :: X
    end function ieee_is_normal_16
#endif /* REAL16 */

  end interface IEEE_IS_NORMAL

  interface IEEE_LOGB

    elemental real(kind=4) function ieee_logb_4(X)
      real(kind=4), intent(in) :: X
    end function ieee_logb_4

    elemental real(kind=8) function ieee_logb_8(X)
      real(kind=8), intent(in) :: X
    end function ieee_logb_8

#ifdef REAL16
    elemental real(kind=16) function ieee_logb_16(X)
      real(kind=16), intent(in) :: X
    end function ieee_logb_16
#endif /* REAL16 */

  end interface IEEE_LOGB

  interface IEEE_NEXT_AFTER

    elemental real(kind=4) function ieee_next_after_4(X, Y)
      real(kind=4), intent(in) :: X
      real(kind=4), intent(in) :: Y
    end function ieee_next_after_4

    elemental real(kind=4) function ieee_next_after_4_8(X, Y)
      real(kind=4), intent(in) :: X
      real(kind=8), intent(in) :: Y
    end function ieee_next_after_4_8

    elemental real(kind=8) function ieee_next_after_8(X, Y)
      real(kind=8), intent(in) :: X
      real(kind=8), intent(in) :: Y
    end function ieee_next_after_8

    elemental real(kind=8) function ieee_next_after_8_4(X, Y)
      real(kind=8), intent(in) :: X
      real(kind=4), intent(in) :: Y
    end function ieee_next_after_8_4

#ifdef REAL16
    elemental real(kind=16) function ieee_next_after_16(X, Y)
      real(kind=16), intent(in) :: X, Y
    end function ieee_next_after_16

    elemental real(kind=16) function ieee_next_after_16_4(X, Y)
      real(kind=16), intent(in) :: X
      real(kind=4), intent(in) :: Y
    end function ieee_next_after_16_4

    elemental real(kind=4) function ieee_next_after_4_16(X, Y)
      real(kind=4), intent(in) :: X
      real(kind=16), intent(in) :: Y
    end function ieee_next_after_16_4

    elemental real(kind=16) function ieee_next_after_16_8(X, Y)
      real(kind=16), intent(in) :: X
      real(kind=8), intent(in) :: Y
    end function ieee_next_after_16_8

    elemental real(kind=8) function ieee_next_after_8_16(X, Y)
      real(kind=8), intent(in) :: X
      real(kind=16), intent(in) :: Y
    end function ieee_next_after_16_8
#endif /* REAL16 */

  end interface IEEE_NEXT_AFTER

  interface IEEE_REM

    elemental real(kind=4) function ieee_rem_4(X, Y)
      real(kind=4), intent(in) :: X
      real(kind=4), intent(in) :: Y
    end function ieee_rem_4

    elemental real(kind=4) function ieee_rem_4_8(X, Y)
      real(kind=4), intent(in) :: X
      real(kind=8), intent(in) :: Y
    end function ieee_rem_4_8

    elemental real(kind=8) function ieee_rem_8(X, Y)
      real(kind=8), intent(in) :: X
      real(kind=8), intent(in) :: Y
    end function ieee_rem_8

    elemental real(kind=8) function ieee_rem_8_4(X, Y)
      real(kind=8), intent(in) :: X
      real(kind=4), intent(in) :: Y
    end function ieee_rem_8_4

#ifdef REAL16
    elemental real(kind=16) function ieee_rem_16(X, Y)
      real(kind=16), intent(in) :: X, Y
    end function ieee_rem_16

    elemental real(kind=16) function ieee_rem_16_4(X, Y)
      real(kind=16), intent(in) :: X
      real(kind=4), intent(in) :: Y
    end function ieee_rem_16_4

    elemental real(kind=4) function ieee_rem_4_16(X, Y)
      real(kind=4), intent(in) :: X
      real(kind=16), intent(in) :: Y
    end function ieee_rem_16_4

    elemental real(kind=16) function ieee_rem_16_8(X, Y)
      real(kind=16), intent(in) :: X
      real(kind=8), intent(in) :: Y
    end function ieee_rem_16_8

    elemental real(kind=8) function ieee_rem_8_16(X, Y)
      real(kind=8), intent(in) :: X
      real(kind=16), intent(in) :: Y
    end function ieee_rem_16_8
#endif /* REAL16 */

  end interface IEEE_REM

  interface IEEE_RINT

    elemental real(kind=4) function ieee_rint_4(X)
      real(kind=4), intent(in) :: X
    end function ieee_rint_4

    elemental real(kind=8) function ieee_rint_8(X)
      real(kind=8), intent(in) :: X
    end function ieee_rint_8

#ifdef REAL16
    elemental real(kind=16) function ieee_rint_16(X)
      real(kind=16), intent(in) :: X
    end function ieee_rint_16
#endif /* REAL16 */

  end interface IEEE_RINT

  interface IEEE_SCALB

    elemental real(kind=4) function ieee_scalb_4(X, I)
      real(kind=4), intent(in) :: X
      integer(kind=4), intent(in) :: I
    end function ieee_scalb_4

    elemental real(kind=4) function ieee_scalb_4_8(X, I)
      real(kind=4), intent(in) :: X
      integer(kind=8), intent(in) :: I
    end function ieee_scalb_4_8

    elemental real(kind=8) function ieee_scalb_8(X, I)
      real(kind=8), intent(in) :: X
      integer(kind=8), intent(in) :: I
    end function ieee_scalb_8

    elemental real(kind=8) function ieee_scalb_8_4(X, I)
      real(kind=8), intent(in) :: X
      integer(kind=4), intent(in) :: I
    end function ieee_scalb_8_4

#ifdef REAL16
    elemental real(kind=16) function ieee_scalb_16_4(X, I)
      real(kind=16), intent(in) :: X
      integer(kind=4), intent(in) :: I
    end function ieee_scalb_16_4

    elemental real(kind=16) function ieee_scalb_16_8(X, I)
      real(kind=16), intent(in) :: X
      integer(kind=8) :: I
    end function ieee_scalb_16_8
#endif /* REAL16 */

  end interface IEEE_SCALB

  interface IEEE_UNORDERED

    elemental logical function ieee_unordered_4(X, Y)
      real(kind=4), intent(in) :: X
      real(kind=4), intent(in) :: Y
    end function ieee_unordered_4

    elemental logical function ieee_unordered_4_8(X, Y)
      real(kind=4), intent(in) :: X
      real(kind=8), intent(in) :: Y
    end function ieee_unordered_4_8

    elemental logical function ieee_unordered_8(X, Y)
      real(kind=8), intent(in) :: X
      real(kind=8), intent(in) :: Y
    end function ieee_unordered_8

    elemental logical function ieee_unordered_8_4(X, Y)
      real(kind=8), intent(in) :: X
      real(kind=4), intent(in) :: Y
    end function ieee_unordered_8_4

  end interface IEEE_UNORDERED

  interface IEEE_VALUE

    elemental real(kind=4) function ieee_value_4(X, CLASS)
      include "ieee_arithmetic_types.h"
      real(kind=4), intent(in) :: X
      type(IEEE_CLASS_TYPE), intent(in) :: CLASS
    end function ieee_value_4

    elemental real(kind=8) function ieee_value_8(X, CLASS)
      include "ieee_arithmetic_types.h"
      real(kind=8), intent(in) :: X
      type(IEEE_CLASS_TYPE), intent(in) :: CLASS
    end function ieee_value_8

#ifdef REAL16
    elemental real(kind=16) function ieee_value_16(X, CLASS)
      include "ieee_arithmetic_types.h"
      real(kind=16), intent(in) :: X
      type(IEEE_CLASS_TYPE), intent(in) :: CLASS
    end function ieee_value_16
#endif /* REAL16 */

  end interface IEEE_VALUE

  interface IEEE_SELECTED_REAL_KIND

    integer function ieee_selected_real_kind(P, R)
      integer, intent(in) :: p, r
    end function ieee_selected_real_kind

  end interface IEEE_SELECTED_REAL_KIND

  interface IEEE_GET_ROUNDING_MODE

    subroutine ieee_get_rounding_mode(ROUND_VALUE)
      include "ieee_arithmetic_types.h"
      type(IEEE_ROUND_TYPE), intent(out) :: ROUND_VALUE
    end subroutine ieee_get_rounding_mode

  end interface IEEE_GET_ROUNDING_MODE

  interface IEEE_SET_ROUNDING_MODE

    subroutine ieee_set_rounding_mode(ROUND_VALUE)
      include "ieee_arithmetic_types.h"
      type(IEEE_ROUND_TYPE), intent(in) :: ROUND_VALUE
    end subroutine ieee_set_rounding_mode

  end interface IEEE_SET_ROUNDING_MODE

  interface IEEE_GET_UNDERFLOW_MODE

    subroutine ieee_get_underflow_mode(GRADUAL)
      include "ieee_arithmetic_types.h"
      logical, intent(out) :: GRADUAL
    end subroutine ieee_get_underflow_mode

  end interface IEEE_GET_UNDERFLOW_MODE

  interface IEEE_SET_UNDERFLOW_MODE

    subroutine ieee_set_underflow_mode(GRADUAL)
      include "ieee_arithmetic_types.h"
      logical, intent(in) :: GRADUAL
    end subroutine ieee_set_underflow_mode

  end interface IEEE_SET_UNDERFLOW_MODE

  public &
    operator(==), &
    operator(/=), &
    IEEE_SUPPORT_DATATYPE, &
    IEEE_SUPPORT_DENORMAL, &
    IEEE_SUPPORT_DIVIDE, &
    IEEE_SUPPORT_INF, &
    IEEE_SUPPORT_IO, &
    IEEE_SUPPORT_NAN, &
    IEEE_SUPPORT_ROUNDING, &
    IEEE_SUPPORT_SQRT, &
    IEEE_SUPPORT_STANDARD, &
    IEEE_SUPPORT_UNDERFLOW_CONTROL, &
    IEEE_CLASS, &
    IEEE_COPY_SIGN, &
    IEEE_IS_FINITE, &
    IEEE_IS_NAN, &
    IEEE_IS_NEGATIVE, &
    IEEE_IS_NORMAL, &
    IEEE_LOGB, &
    IEEE_NEXT_AFTER, &
    IEEE_REM, &
    IEEE_RINT, &
    IEEE_SCALB, &
    IEEE_UNORDERED, &
    IEEE_VALUE, &
    IEEE_SELECTED_REAL_KIND, &
    IEEE_GET_ROUNDING_MODE, &
    IEEE_SET_ROUNDING_MODE, &
    IEEE_GET_UNDERFLOW_MODE, &
    IEEE_SET_UNDERFLOW_MODE, &
    IEEE_CLASS_TYPE, &
    IEEE_ROUND_TYPE

contains

    elemental logical function test_equality_class(a, b)
      type(IEEE_CLASS_TYPE), intent(in) :: a, b
      test_equality_class = (a%value == b%value)
    end function test_equality_class

    elemental logical function test_equality_round(a, b)
      type(IEEE_ROUND_TYPE), intent(in) :: a, b
      test_equality_round = (a%value == b%value)
    end function test_equality_round

    elemental logical function test_inequality_class(a, b)
      type(IEEE_CLASS_TYPE), intent(in) :: a, b
      test_inequality_class = (a%value /= b%value)
    end function test_inequality_class

    elemental logical function test_inequality_round(a, b)
      type(IEEE_ROUND_TYPE), intent(in) :: a, b
      test_inequality_round = (a%value /= b%value)
    end function test_inequality_round

end module ieee_arithmetic
