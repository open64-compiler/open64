
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

#include <stdio.h>

#define USE_NANF_WITH_FLAGS
#define USE_HANDLE_ERRORF
#include "../inc/libm_inlines_amd.h"
#undef USE_NANF_WITH_FLAGS
#undef USE_HANDLE_ERRORF

#include "../inc/libm_errno_amd.h"

#ifndef WINDOWS
/* Deal with errno for out-of-range argument */
static inline float retval_errno_edom(float x)
{
  struct exception exc;
  exc.arg1 = (double)x;
  exc.arg2 = (double)x;
  exc.type = DOMAIN;
  exc.name = (char *)"acoshf";
  if (_LIB_VERSION == _SVID_)
    exc.retval = -HUGE;
  else
    exc.retval = nanf_with_flags(AMD_F_INVALID);
  if (_LIB_VERSION == _POSIX_)
    __set_errno(EDOM);
  else if (!matherr(&exc))
    {
      if(_LIB_VERSION == _SVID_)
        (void)fputs("acoshf: DOMAIN error\n", stderr);
    __set_errno(EDOM);
    }
  return exc.retval;
}
#endif

#undef _FUNCNAME
#define _FUNCNAME "acoshf"
float FN_PROTOTYPE(acoshf)(float x)
{

  unsigned int ux;
  double dx, r, rarg, t;

  static const unsigned int
    recrteps = 0x46000000; /* 1/sqrt(eps) = 4.09600000000000000000e+03 */

  static const double
    log2 = 6.93147180559945286227e-01;  /* 0x3fe62e42fefa39ef */

  GET_BITS_SP32(x, ux);

  if ((ux & EXPBITS_SP32) == EXPBITS_SP32)
    {
      /* x is either NaN or infinity */
      if (ux & MANTBITS_SP32)
        {
          /* x is NaN */
#ifdef WINDOWS
          return handle_errorf(_FUNCNAME, ux|0x00400000, _DOMAIN,
                               0, EDOM, x, 0.0F);
#else
          return x + x; /* Raise invalid if it is a signalling NaN */
#endif
        }
      else
        {
          /* x is infinity */
          if (ux & SIGNBIT_SP32)
            /* x is negative infinity. Return a NaN. */
#ifdef WINDOWS
            return handle_errorf(_FUNCNAME, INDEFBITPATT_SP32, _DOMAIN,
                                 AMD_F_INVALID, EDOM, x, 0.0F);
#else
            return retval_errno_edom(x);
#endif
          else
            /* Return positive infinity with no signal */
            return x;
        }
    }
  else if ((ux & SIGNBIT_SP32) || (ux < 0x3f800000))
    {
      /* x is less than 1.0. Return a NaN. */
#ifdef WINDOWS
      return handle_errorf(_FUNCNAME, INDEFBITPATT_SP32, _DOMAIN,
                           AMD_F_INVALID, EDOM, x, 0.0F);
#else
      return retval_errno_edom(x);
#endif
    }

  dx = x;

  if (ux > recrteps)
    {
      /* Arguments greater than 1/sqrt(epsilon) in magnitude are
         approximated by acoshf(x) = ln(2) + ln(x) */
      r = FN_PROTOTYPE(log)(dx) + log2;
    }
  else if (ux > 0x40000000)
    {
      /* 2.0 <= x <= 1/sqrt(epsilon) */
      /* acoshf for these arguments is approximated by
         acoshf(x) = ln(x + sqrt(x*x-1)) */
      rarg = dx*dx-1.0;
      /* Use assembly instruction to compute r = sqrt(rarg); */
      ASMSQRT(rarg,r);
      rarg = r + dx;
      r = FN_PROTOTYPE(log)(rarg);
    }
  else
    {
      /* sqrt(epsilon) <= x <= 2.0 */
      t = dx - 1.0;
      rarg = 2.0*t + t*t;
      ASMSQRT(rarg,r);  /* r = sqrt(rarg) */
      rarg = t + r;
      r = FN_PROTOTYPE(log1p)(rarg);
    }
  return (float)(r);
}

weak_alias (__acoshf, acoshf)
