
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
#define USE_HANDLE_ERRORF
#include "../inc/libm_inlines_amd.h"
#undef USE_HANDLE_ERRORF
#endif

#ifdef WINDOWS
#pragma function(floorf)
#endif

float FN_PROTOTYPE(floorf)(float x)
{
  float r;
  int rexp, xneg;
  unsigned int ux, ax, ur, mask;

  GET_BITS_SP32(x, ux);
  ax = ux & (~SIGNBIT_SP32);
  xneg = (ux != ax);

  if (ax >= 0x4b800000)
    {
      /* abs(x) is either NaN, infinity, or >= 2^24 */
      if (ax > 0x7f800000)
        /* x is NaN */
#ifdef WINDOWS
        return handle_errorf("floorf", ux|0x00400000, _DOMAIN,
                             0, EDOM, x, 0.0F);
#else
        return x + x; /* Raise invalid if it is a signalling NaN */
#endif
      else
        return x;
    }
  else if (ax < 0x3f800000) /* abs(x) < 1.0 */
    {
      if (ax == 0x00000000)
        /* x is +zero or -zero; return the same zero */
        return x;
      else if (xneg) /* x < 0.0 */
        return -1.0F;
      else
        return 0.0F;
    }
  else
    {
      rexp = ((ux & EXPBITS_SP32) >> EXPSHIFTBITS_SP32) - EXPBIAS_SP32;
      /* Mask out the bits of r that we don't want */
      mask = (1 << (EXPSHIFTBITS_SP32 - rexp)) - 1;
      ur = (ux & ~mask);
      PUT_BITS_SP32(ur, r);
      if (xneg && (ux != ur))
        /* We threw some bits away and x was negative */
        return r - 1.0F;
      else
        return r;
    }
}

weak_alias (__floorf, floorf)
