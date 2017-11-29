
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

#define USE_NAN_WITH_FLAGS
#define USE_VAL_WITH_FLAGS
#define USE_INFINITY_WITH_FLAGS
#define USE_HANDLE_ERROR
#include "../inc/libm_inlines_amd.h"
#undef USE_NAN_WITH_FLAGS
#undef USE_VAL_WITH_FLAGS
#undef USE_INFINITY_WITH_FLAGS
#undef USE_HANDLE_ERROR

#include "../inc/libm_errno_amd.h"

#ifndef WINDOWS
/* Deal with errno for out-of-range argument */
static inline double retval_errno_edom(double x, double retval)
{
  struct exception exc;
  exc.arg1 = x;
  exc.arg2 = x;
  exc.type = DOMAIN;
  exc.name = (char *)"atanh";
  if (_LIB_VERSION == _SVID_)
    exc.retval = -HUGE;
  else
    exc.retval = retval;
  if (_LIB_VERSION == _POSIX_)
    __set_errno(EDOM);
  else if (!matherr(&exc))
    {
      if(_LIB_VERSION == _SVID_)
        (void)fputs("atanh: DOMAIN error\n", stderr);
    __set_errno(EDOM);
    }
  return exc.retval;
}
#endif

#undef _FUNCNAME
#define _FUNCNAME "atanh"
double FN_PROTOTYPE(atanh)(double x)
{

  unsigned long long ux, ax;
  double r, absx, t, poly;


  GET_BITS_DP64(x, ux);
  ax = ux & ~SIGNBIT_DP64;
  PUT_BITS_DP64(ax, absx);

  if ((ux & EXPBITS_DP64) == EXPBITS_DP64)
    {
      /* x is either NaN or infinity */
      if (ux & MANTBITS_DP64)
        {
          /* x is NaN */
#ifdef WINDOWS
          return handle_error(_FUNCNAME, ux|0x0008000000000000, _DOMAIN,
                              AMD_F_INVALID, EDOM, x, 0.0);
#else
          return x + x; /* Raise invalid if it is a signalling NaN */
#endif
        }
      else
        {
          /* x is infinity; return a NaN */
#ifdef WINDOWS
          return handle_error(_FUNCNAME, INDEFBITPATT_DP64, _DOMAIN,
                              AMD_F_INVALID, EDOM, x, 0.0);
#else
          return retval_errno_edom(x,nan_with_flags(AMD_F_INVALID));
#endif
        }
    }
  else if (ax >= 0x3ff0000000000000)
    {
      if (ax > 0x3ff0000000000000)
        {
          /* abs(x) > 1.0; return NaN */
#ifdef WINDOWS
          return handle_error(_FUNCNAME, INDEFBITPATT_DP64, _DOMAIN,
                              AMD_F_INVALID, EDOM, x, 0.0);
#else
          return retval_errno_edom(x,nan_with_flags(AMD_F_INVALID));
#endif
        }
      else if (ux == 0x3ff0000000000000)
        {
          /* x = +1.0; return infinity with the same sign as x
             and set the divbyzero status flag */
#ifdef WINDOWS
          return handle_error(_FUNCNAME, PINFBITPATT_DP64, _DOMAIN,
                              AMD_F_INVALID, EDOM, x, 0.0);
#else
          return retval_errno_edom(x,infinity_with_flags(AMD_F_DIVBYZERO));
#endif
        }
      else
        {
          /* x = -1.0; return infinity with the same sign as x */
#ifdef WINDOWS
          return handle_error(_FUNCNAME, NINFBITPATT_DP64, _DOMAIN,
                              AMD_F_INVALID, EDOM, x, 0.0);
#else
          return retval_errno_edom(x,-infinity_with_flags(AMD_F_DIVBYZERO));
#endif
        }
    }


  if (ax < 0x3e30000000000000)
    {
      if (ax == 0x0000000000000000)
        {
          /* x is +/-zero. Return the same zero. */
          return x;
        }
      else
        {
          /* Arguments smaller than 2^(-28) in magnitude are
             approximated by atanh(x) = x, raising inexact flag. */
          return val_with_flags(x, AMD_F_INEXACT);
        }
    }
  else
    {
      if (ax < 0x3fe0000000000000)
        {
          /* Arguments up to 0.5 in magnitude are
             approximated by a [5,5] minimax polynomial */
          t = x*x;
          poly =
            (0.47482573589747356373e0 +
             (-0.11028356797846341457e1 +
              (0.88468142536501647470e0 +
               (-0.28180210961780814148e0 +
                (0.28728638600548514553e-1 -
                 0.10468158892753136958e-3 * t) * t) * t) * t) * t) /
            (0.14244772076924206909e1 +
             (-0.41631933639693546274e1 +
              (0.45414700626084508355e1 +
               (-0.22608883748988489342e1 +
                (0.49561196555503101989e0 -
                 0.35861554370169537512e-1 * t) * t) * t) * t) * t);
          return x + x*t*poly;
        }
      else
        {
          /* abs(x) >= 0.5 */
          /* Note that
               atanh(x) = 0.5 * ln((1+x)/(1-x))
             (see Abramowitz and Stegun 4.6.22).
             For greater accuracy we use the variant formula
             atanh(x) = log(1 + 2x/(1-x)) = log1p(2x/(1-x)).
          */
          r = (2.0 * absx) / (1.0 - absx);
          r = 0.5 * FN_PROTOTYPE(log1p)(r);
          if (ux & SIGNBIT_DP64)
            /* Argument x is negative */
            return -r;
          else
            return r;
        }
    }
}

weak_alias (__atanh, atanh)
