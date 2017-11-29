
/*
*  Copyright (C) 2008-2009 Advanced Micro Devices, Inc. All Rights Reserved.
*
*  This file is part of libacml_mv.
*
*  libacml_mv is free software; you can redistribute it and/or
*  modify it under the terms of the GNU Lesser General Public
*  License as published by the Free Software Foundation; either
*  version 2.1 of the License, or (at your option) any later version.
*
*  libacml_mv is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*  Lesser General Public License for more details.
*
*  You should have received a copy of the GNU Lesser General Public
*  License along with libacml_mv.  If not, see
*  <http://www.gnu.org/licenses/>.
*
*/


#include "../inc/libm_amd.h"
#include "../inc/libm_util_amd.h"

#ifdef USE_SOFTWARE_SQRT
#define USE_SQRTF_AMD_INLINE
#endif
#define USE_INFINITYF_WITH_FLAGS
#define USE_HANDLE_ERRORF
#include "../inc/libm_inlines_amd.h"
#ifdef USE_SOFTWARE_SQRT
#undef USE_SQRTF_AMD_INLINE
#endif
#undef USE_INFINITYF_WITH_FLAGS
#undef USE_HANDLE_ERRORF

#include "../inc/libm_errno_amd.h"

#ifndef WINDOWS
/* Deal with errno for out-of-range result */
static inline float retval_errno_erange_overflow(float x, float y)
{
  struct exception exc;
  exc.arg1 = (double)x;
  exc.arg2 = (double)y;
  exc.type = OVERFLOW;
  exc.name = (char *)"hypotf";
  if (_LIB_VERSION == _SVID_)
    exc.retval = HUGE;
  else
    exc.retval = infinityf_with_flags(AMD_F_OVERFLOW | AMD_F_INEXACT);
  if (_LIB_VERSION == _POSIX_)
    __set_errno(ERANGE);
  else if (!matherr(&exc))
    __set_errno(ERANGE);
  return exc.retval;
}
#endif

#ifdef WINDOWS
float FN_PROTOTYPE(hypotf)(float x, float y)
#else
float FN_PROTOTYPE(hypotf)(float x, float y)
#endif
{
  /* Returns sqrt(x*x + y*y) with no overflow or underflow unless
     the result warrants it */

    /* Do intermediate computations in double precision
       and use sqrt instruction from chip if available. */
    double dx = x, dy = y, dr, retval;

    /* The largest finite float, stored as a double */
    const double large = 3.40282346638528859812e+38; /* 0x47efffffe0000000 */


  unsigned long long ux, uy, avx, avy;

  GET_BITS_DP64(x, avx);
  avx &= ~SIGNBIT_DP64;
  GET_BITS_DP64(y, avy);
  avy &= ~SIGNBIT_DP64;
  ux = (avx >> EXPSHIFTBITS_DP64);
  uy = (avy >> EXPSHIFTBITS_DP64);

  if (ux == BIASEDEMAX_DP64 + 1 || uy == BIASEDEMAX_DP64 + 1)
    {
      retval = x*x + y*y;
      /* One or both of the arguments are NaN or infinity. The
         result will also be NaN or infinity. */
      if (((ux == BIASEDEMAX_DP64 + 1) && !(avx & MANTBITS_DP64)) ||
          ((uy == BIASEDEMAX_DP64 + 1) && !(avy & MANTBITS_DP64)))
        /* x or y is infinity. ISO C99 defines that we must
           return +infinity, even if the other argument is NaN.
           Note that the computation of x*x + y*y above will already
           have raised invalid if either x or y is a signalling NaN. */
        return infinityf_with_flags(0);
      else
        /* One or both of x or y is NaN, and neither is infinity.
           Raise invalid if it's a signalling NaN */
        return (float)retval;
    }

    dr = (dx*dx + dy*dy);

#if USE_SOFTWARE_SQRT
    retval = sqrtf_amd_inline(r);
#else
#ifdef WINDOWS
  /* VC++ intrinsic call */
  _mm_store_sd(&retval, _mm_sqrt_sd(_mm_setzero_pd(), _mm_load_sd(&dr)));
#else
    /* Hammer sqrt instruction */
    asm volatile ("sqrtsd %1, %0" : "=x" (retval) : "x" (dr));
#endif
#endif

    if (retval > large)
#ifdef WINDOWS
      return handle_errorf("hypotf", PINFBITPATT_SP32, _OVERFLOW,
                           AMD_F_OVERFLOW | AMD_F_INEXACT, ERANGE, x, y);
#else
      return retval_errno_erange_overflow(x, y);
#endif
    else
      return (float)retval;
  }

weak_alias (__hypotf, hypotf)
