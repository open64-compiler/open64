
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

#define USE_VAL_WITH_FLAGS
#define USE_NAN_WITH_FLAGS
#define USE_HANDLE_ERROR
#include "../inc/libm_inlines_amd.h"
#undef USE_NAN_WITH_FLAGS
#undef USE_VAL_WITH_FLAGS
#undef USE_HANDLE_ERROR

#include "../inc/libm_errno_amd.h"

#ifndef WINDOWS
/* Deal with errno for out-of-range argument */
static inline double retval_errno_edom(double x)
{
  struct exception exc;
  exc.arg1 = x;
  exc.arg2 = x;
  exc.type = DOMAIN;
  exc.name = (char *)"asin";
  if (_LIB_VERSION == _SVID_)
    exc.retval = HUGE;
  else
    exc.retval = nan_with_flags(AMD_F_INVALID);
  if (_LIB_VERSION == _POSIX_)
    __set_errno(EDOM);
  else if (!matherr(&exc))
    {
      if(_LIB_VERSION == _SVID_)
        (void)fputs("asin: DOMAIN error\n", stderr);
    __set_errno(EDOM);
    }
  return exc.retval;
}
#endif

#ifdef WINDOWS
#pragma function(asin)
#endif

double FN_PROTOTYPE(asin)(double x)
{
  /* Computes arcsin(x).
     The argument is first reduced by noting that arcsin(x)
     is invalid for abs(x) > 1 and arcsin(-x) = -arcsin(x).
     For denormal and small arguments arcsin(x) = x to machine
     accuracy. Remaining argument ranges are handled as follows.
     For abs(x) <= 0.5 use
     arcsin(x) = x + x^3*R(x^2)
     where R(x^2) is a rational minimax approximation to
     (arcsin(x) - x)/x^3.
     For abs(x) > 0.5 exploit the identity:
      arcsin(x) = pi/2 - 2*arcsin(sqrt(1-x)/2)
     together with the above rational approximation, and
     reconstruct the terms carefully.
    */

  /* Some constants and split constants. */

  static const double
    piby2_tail  = 6.1232339957367660e-17, /* 0x3c91a62633145c07 */
    hpiby2_head = 7.8539816339744831e-01, /* 0x3fe921fb54442d18 */
    piby2       = 1.5707963267948965e+00; /* 0x3ff921fb54442d18 */
  double u, v, y, s=0.0, r;
  int xexp, xnan, transform=0;

  unsigned long long ux, aux, xneg;
  GET_BITS_DP64(x, ux);
  aux = ux & ~SIGNBIT_DP64;
  xneg = (ux & SIGNBIT_DP64);
  xnan = (aux > PINFBITPATT_DP64);
  xexp = (int)((ux & EXPBITS_DP64) >> EXPSHIFTBITS_DP64) - EXPBIAS_DP64;

  /* Special cases */

  if (xnan)
    {
#ifdef WINDOWS
      return handle_error("asin", ux|0x0008000000000000, _DOMAIN,
                          0, EDOM, x, 0.0);
#else
      return x + x; /* With invalid if it's a signalling NaN */
#endif
    }
  else if (xexp < -28)
    { /* y small enough that arcsin(x) = x */
      return val_with_flags(x, AMD_F_INEXACT);
    }
  else if (xexp >= 0)
    { /* abs(x) >= 1.0 */
      if (x == 1.0)
        return val_with_flags(piby2, AMD_F_INEXACT);
      else if (x == -1.0)
        return val_with_flags(-piby2, AMD_F_INEXACT);
      else
#ifdef WINDOWS
        return handle_error("asin", INDEFBITPATT_DP64, _DOMAIN,
                            AMD_F_INVALID, EDOM, x, 0.0);
#else
        return retval_errno_edom(x);
#endif
    }

  if (xneg) y = -x;
  else y = x;

  transform = (xexp >= -1); /* abs(x) >= 0.5 */

  if (transform)
    { /* Transform y into the range [0,0.5) */
      r = 0.5*(1.0 - y);
#ifdef WINDOWS
      /* VC++ intrinsic call */
      _mm_store_sd(&s, _mm_sqrt_sd(_mm_setzero_pd(), _mm_load_sd(&r)));
#else
      /* Hammer sqrt instruction */
      asm volatile ("sqrtsd %1, %0" : "=x" (s) : "x" (r));
#endif
      y = s;
    }
  else
    r = y*y;

  /* Use a rational approximation for [0.0, 0.5] */

  u = r*(0.227485835556935010735943483075 +
         (-0.445017216867635649900123110649 +
          (0.275558175256937652532686256258 +
           (-0.0549989809235685841612020091328 +
            (0.00109242697235074662306043804220 +
             0.0000482901920344786991880522822991*r)*r)*r)*r)*r)/
    (1.36491501334161032038194214209 +
     (-3.28431505720958658909889444194 +
      (2.76568859157270989520376345954 +
       (-0.943639137032492685763471240072 +
        0.105869422087204370341222318533*r)*r)*r)*r);

  if (transform)
    { /* Reconstruct asin carefully in transformed region */
        {
          double c, s1, p, q;
          unsigned long long us;
          GET_BITS_DP64(s, us);
          PUT_BITS_DP64(0xffffffff00000000 & us, s1);
          c = (r-s1*s1)/(s+s1);
          p = 2.0*s*u - (piby2_tail-2.0*c);
          q = hpiby2_head - 2.0*s1;
          v = hpiby2_head - (p-q);
        }
    }
  else
    {
#ifdef WINDOWS
      /* Use a temporary variable to prevent VC++ rearranging
            y + y*u
         into
            y * (1 + u)
         and getting an incorrectly rounded result */
      double tmp;
      tmp = y * u;
      v = y + tmp;
#else
      v = y + y*u;
#endif
    }

  if (xneg) return -v;
  else return v;
}

weak_alias (__asin, asin)
