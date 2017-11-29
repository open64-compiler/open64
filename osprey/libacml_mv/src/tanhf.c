
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



#define USE_SPLITEXPF
#define USE_SCALEFLOAT_2
#define USE_VALF_WITH_FLAGS
#include "../inc/libm_inlines_amd.h"
#undef USE_SPLITEXPF
#undef USE_SCALEFLOAT_2
#undef USE_VALF_WITH_FLAGS

#include "../inc/libm_errno_amd.h"

float FN_PROTOTYPE(tanhf)(float x)
{
  /*
    The definition of tanh(x) is sinh(x)/cosh(x), which is also equivalent
    to the following three formulae:
    1.  (exp(x) - exp(-x))/(exp(x) + exp(-x))
    2.  (1 - (2/(exp(2*x) + 1 )))
    3.  (exp(2*x) - 1)/(exp(2*x) + 1)
    but computationally, some formulae are better on some ranges.
  */
  static const float
    thirtytwo_by_log2 =  4.6166240692e+01F, /* 0x4238aa3b */
    log2_by_32_lead =  2.1659851074e-02F, /* 0x3cb17000 */
    log2_by_32_tail =  9.9831822808e-07F, /* 0x3585fdf4 */
    large_threshold = 10.0F; /* 0x41200000 */

  unsigned int ux, aux;
  float y, z, p, z1, z2, xneg;
  int m;

  /* Special cases */

  GET_BITS_SP32(x, ux);
  aux = ux & ~SIGNBIT_SP32;
  if (aux < 0x39000000) /* |x| small enough that tanh(x) = x */
    {
      if (aux == 0)
        return x; /* with no inexact */
      else
        return valf_with_flags(x, AMD_F_INEXACT);
    }
  else if (aux > 0x7f800000) /* |x| is NaN */
    return x + x;

  xneg = 1.0F - 2.0F * (aux != ux);

  y = xneg * x;

  if (y > large_threshold)
    {
      /* If x is large then exp(-x) is negligible and
         formula 1 reduces to plus or minus 1.0 */
      z = 1.0F;
    }
  else if (y <= 1.0F)
    {
      float y2;
      y2 = y*y;

      if (y < 0.9F)
        {
          /* Use a [2,1] Remez approximation on [0,0.9]. */
          z = y + y*y2*
            (-0.28192806108402678e0F +
             (-0.14628356048797849e-2F +
              0.4891631088530669873e-4F*y2)*y2)/
            (0.845784192581041099e0F +
             0.3427017942262751343e0F*y2);
        }
      else
        {
          /* Use a [2,1] Remez approximation on [0.9,1]. */
          z = y + y*y2*
            (-0.24069858695196524e0F +
             (-0.12325644183611929e-2F +
              0.3827534993599483396e-4F*y2)*y2)/
            (0.72209738473684982e0F +
             0.292529068698052819e0F*y2);
        }
    }
  else
    {
      /* Compute p = exp(2*y) + 1. The code is basically inlined
         from exp_amd. */

      splitexpf(2*y, 1.0F, thirtytwo_by_log2, log2_by_32_lead,
	       log2_by_32_tail, &m, &z1, &z2);
      p = scaleFloat_2(z1 + z2, m) + 1.0F;
      /* Now reconstruct tanh from p. */
      z = (1.0F - 2.0F/p);
    }

  return xneg * z;
}


weak_alias (__tanhf, tanhf)
