
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
#define USE_VALF_WITH_FLAGS
#define USE_INFINITYF_WITH_FLAGS
#define USE_HANDLE_ERRORF
#include "../inc/libm_inlines_amd.h"
#undef USE_NANF_WITH_FLAGS
#undef USE_VALF_WITH_FLAGS
#undef USE_INFINITYF_WITH_FLAGS
#undef USE_HANDLE_ERRORF

#include "../inc/libm_errno_amd.h"

#ifndef WINDOWS
/* Deal with errno for out-of-range argument */
static inline float retval_errno_edom(float x, float retval)
{
  struct exception exc;
  exc.arg1 = (double)x;
  exc.arg2 = (double)x;
  exc.type = DOMAIN;
  exc.name = (char *)"atanhf";
  if (_LIB_VERSION == _SVID_)
    exc.retval = -HUGE;
  else
    exc.retval = (double)retval;
  if (_LIB_VERSION == _POSIX_)
    __set_errno(EDOM);
  else if (!matherr(&exc))
    {
      if(_LIB_VERSION == _SVID_)
        (void)fputs("atanhf: DOMAIN error\n", stderr);
    __set_errno(EDOM);
    }
  return exc.retval;
}
#endif

#undef _FUNCNAME
#define _FUNCNAME "atanhf"
float FN_PROTOTYPE(atanhf)(float x)
{

  double dx;
  unsigned int ux, ax;
  double r, t, poly;

  GET_BITS_SP32(x, ux);
  ax = ux & ~SIGNBIT_SP32;

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
          /* x is infinity; return a NaN */
#ifdef WINDOWS
          return handle_errorf(_FUNCNAME, INDEFBITPATT_SP32, _DOMAIN,
                               AMD_F_INVALID, EDOM, x, 0.0F);
#else
          return retval_errno_edom(x,nanf_with_flags(AMD_F_INVALID));
#endif
        }
    }
  else if (ax >= 0x3f800000)
    {
      if (ax > 0x3f800000)
        {
          /* abs(x) > 1.0; return NaN */
#ifdef WINDOWS
          return handle_errorf(_FUNCNAME, INDEFBITPATT_SP32, _DOMAIN,
                               AMD_F_INVALID, EDOM, x, 0.0F);
#else
          return retval_errno_edom(x,nanf_with_flags(AMD_F_INVALID));
#endif
        }
      else if (ux == 0x3f800000)
        {
          /* x = +1.0; return infinity with the same sign as x
             and set the divbyzero status flag */
#ifdef WINDOWS
          return handle_errorf(_FUNCNAME, PINFBITPATT_SP32, _DOMAIN,
                               AMD_F_INVALID, EDOM, x, 0.0F);
#else
          return retval_errno_edom(x,infinityf_with_flags(AMD_F_DIVBYZERO));
#endif
        }
      else
        {
          /* x = -1.0; return infinity with the same sign as x */
#ifdef WINDOWS
          return handle_errorf(_FUNCNAME, NINFBITPATT_SP32, _DOMAIN,
                               AMD_F_INVALID, EDOM, x, 0.0F);
#else
          return retval_errno_edom(x,-infinityf_with_flags(AMD_F_DIVBYZERO));
#endif
        }
    }

  if (ax < 0x39000000)
    {
      if (ax == 0x00000000)
        {
          /* x is +/-zero. Return the same zero. */
          return x;
        }
      else
        {
          /* Arguments smaller than 2^(-13) in magnitude are
             approximated by atanhf(x) = x, raising inexact flag. */
          return valf_with_flags(x, AMD_F_INEXACT);
        }
    }
  else
    {
      dx = x;
      if (ax < 0x3f000000)
        {
          /* Arguments up to 0.5 in magnitude are
             approximated by a [2,2] minimax polynomial */
          t = dx*dx;
          poly =
            (0.39453629046e0 +
           (-0.28120347286e0 +
             0.92834212715e-2 * t) * t) /
            (0.11836088638e1 + 
           (-0.15537744551e1 +
             0.45281890445e0 * t) * t);
          return (float)(dx + dx*t*poly);
        }
      else
        {
          /* abs(x) >= 0.5 */
          /* Note that
               atanhf(x) = 0.5 * ln((1+x)/(1-x))
             (see Abramowitz and Stegun 4.6.22).
             For greater accuracy we use the variant formula
             atanhf(x) = log(1 + 2x/(1-x)) = log1p(2x/(1-x)).
          */
          if (ux & SIGNBIT_SP32)
            {
              /* Argument x is negative */
              r = (-2.0 * dx) / (1.0 + dx);
              r = 0.5 * FN_PROTOTYPE(log1p)(r);
              return (float)-r;
            }
          else
            {
              r = (2.0 * dx) / (1.0 - dx);
              r = 0.5 * FN_PROTOTYPE(log1p)(r);
              return (float)r;
            }
        }
    }
}

weak_alias (__atanhf, atanhf)
