
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

#define USE_HANDLE_ERRORF
#define USE_VALF_WITH_FLAGS
#include "../inc/libm_inlines_amd.h"
#undef USE_HANDLE_ERRORF
#undef VALF_WITH_FLAGS

#undef _FUNCNAME
#define _FUNCNAME "asinhf"
float FN_PROTOTYPE(asinhf)(float x)
{

  double dx;
  unsigned int ux, ax, xneg;
  double absx, r, rarg, t, poly;

  static const unsigned int
    rteps = 0x39800000,    /* sqrt(eps) = 2.44140625000000000000e-04 */
    recrteps = 0x46000000; /* 1/rteps = 4.09600000000000000000e+03 */

  static const double
    log2 = 6.93147180559945286227e-01;  /* 0x3fe62e42fefa39ef */

  GET_BITS_SP32(x, ux);
  ax = ux & ~SIGNBIT_SP32;
  xneg = ux & SIGNBIT_SP32;

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
          /* x is infinity. Return the same infinity. */
#ifdef WINDOWS
          if (ux & SIGNBIT_SP32)
            return handle_errorf(_FUNCNAME, NINFBITPATT_SP32, _DOMAIN,
                                 AMD_F_INVALID, EDOM, x, 0.0F);
          else
            return handle_errorf(_FUNCNAME, PINFBITPATT_SP32, _DOMAIN,
                                 AMD_F_INVALID, EDOM, x, 0.0F);
#else
          return x;
#endif
        }
    }
  else if (ax < rteps) /* abs(x) < sqrt(epsilon) */
    {
      if (ax == 0x00000000)
        {
          /* x is +/-zero. Return the same zero. */
          return x;
        }
      else
        {
          /* Tiny arguments approximated by asinhf(x) = x
             - avoid slow operations on denormalized numbers */
          return valf_with_flags(x,AMD_F_INEXACT);
        }
    }

  dx = x;
  if (xneg)
    absx = -dx;
  else
    absx = dx;

  if (ax <= 0x40800000) /* abs(x) <= 4.0 */
    {
      /* Arguments less than 4.0 in magnitude are
         approximated by [4,4] minimax polynomials
      */
      t = dx*dx;
      if (ax <= 0x40000000) /* abs(x) <= 2 */
        poly =
          (-0.1152965835871758072e-1 +
          (-0.1480204186473758321e-1 +
          (-0.5063201055468483248e-2 +
          (-0.4162727710583425360e-3 -
            0.1177198915954942694e-5 * t) * t) * t) * t) /
           (0.6917795026025976739e-1 +
           (0.1199423176003939087e+0 +
           (0.6582362487198468066e-1 +
           (0.1260024978680227945e-1 +
            0.6284381367285534560e-3 * t) * t) * t) * t);
      else
        poly =
           (-0.185462290695578589e-2 +
           (-0.113672533502734019e-2 +
           (-0.142208387300570402e-3 +
           (-0.339546014993079977e-5 -
             0.151054665394480990e-8 * t) * t) * t) * t) /
            (0.111486158580024771e-1 +
            (0.117782437980439561e-1 +
            (0.325903773532674833e-2 +
            (0.255902049924065424e-3 +
             0.434150786948890837e-5 * t) * t) * t) * t);
      return (float)(dx + dx*t*poly);
    }
  else
    {
      /* abs(x) > 4.0 */
      if (ax > recrteps)
        {
          /* Arguments greater than 1/sqrt(epsilon) in magnitude are
             approximated by asinhf(x) = ln(2) + ln(abs(x)), with sign of x */
          r = FN_PROTOTYPE(log)(absx) + log2;
        }
      else
        {
          rarg = absx*absx+1.0;
          /* Arguments such that 4.0 <= abs(x) <= 1/sqrt(epsilon) are
             approximated by
               asinhf(x) = ln(abs(x) + sqrt(x*x+1))
             with the sign of x (see Abramowitz and Stegun 4.6.20) */
          /* Use assembly instruction to compute r = sqrt(rarg); */
          ASMSQRT(rarg,r);
          r += absx;
          r = FN_PROTOTYPE(log)(r);
        }
      if (xneg)
        return (float)(-r);
      else
        return (float)r;
    }
}

weak_alias (__asinhf, asinhf)
