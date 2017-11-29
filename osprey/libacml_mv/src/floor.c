
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

#ifdef WINDOWS
#include "../inc/libm_errno_amd.h"
#define USE_HANDLE_ERROR
#include "../inc/libm_inlines_amd.h"
#undef USE_HANDLE_ERROR
#endif

#ifdef WINDOWS
#pragma function(floor)
#endif

double FN_PROTOTYPE(floor)(double x)
{
  double r;
  long long rexp, xneg;


  unsigned long long ux, ax, ur, mask;

  GET_BITS_DP64(x, ux);
  ax = ux & (~SIGNBIT_DP64);
  xneg = (ux != ax);

  if (ax >= 0x4340000000000000)
    {
      /* abs(x) is either NaN, infinity, or >= 2^53 */
      if (ax > 0x7ff0000000000000)
        /* x is NaN */
#ifdef WINDOWS
        return handle_error("floor", ux|0x0008000000000000, _DOMAIN,
                            0, EDOM, x, 0.0);
#else
        return x + x; /* Raise invalid if it is a signalling NaN */
#endif
      else
        return x;
    }
  else if (ax < 0x3ff0000000000000) /* abs(x) < 1.0 */
    {
      if (ax == 0x0000000000000000)
        /* x is +zero or -zero; return the same zero */
        return x;
      else if (xneg) /* x < 0.0 */
        return -1.0;
      else
        return 0.0;
    }
  else
    {
      r = x;
      rexp = ((ux & EXPBITS_DP64) >> EXPSHIFTBITS_DP64) - EXPBIAS_DP64;
      /* Mask out the bits of r that we don't want */
      mask = 1;
      mask = (mask << (EXPSHIFTBITS_DP64 - rexp)) - 1;
      ur = (ux & ~mask);
      PUT_BITS_DP64(ur, r);
      if (xneg && (ur != ux))
        /* We threw some bits away and x was negative */
        return r - 1.0;
      else
        return r;
    }

}

weak_alias (__floor, floor)
