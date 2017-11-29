
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

#define USE_INFINITYF_WITH_FLAGS
#define USE_HANDLE_ERRORF
#include "../inc/libm_inlines_amd.h"
#undef USE_INFINITYF_WITH_FLAGS
#undef USE_HANDLE_ERRORF

#ifdef WINDOWS
#include "../inc/libm_errno_amd.h"
#endif

#ifdef WINDOWS
float FN_PROTOTYPE(logbf)(float x)
#else
float FN_PROTOTYPE(logbf)(float x)
#endif
{
  unsigned int ux;
  int u;
  GET_BITS_SP32(x, ux);
  u = ((ux & EXPBITS_SP32) >> EXPSHIFTBITS_SP32) - EXPBIAS_SP32;
  if ((ux & ~SIGNBIT_SP32) == 0)
    /* x is +/-zero. Return -infinity with div-by-zero flag. */
#ifdef WINDOWS
    return handle_errorf("logbf", NINFBITPATT_SP32, _SING,
                         AMD_F_DIVBYZERO, ERANGE, x, 0.0F);
#else
    return -infinityf_with_flags(AMD_F_DIVBYZERO);
#endif
  else if (EMIN_SP32 <= u && u <= EMAX_SP32)
    /* x is a normal number */
    return (float)u;
  else if (u > EMAX_SP32)
    {
      /* x is infinity or NaN */
      if ((ux & MANTBITS_SP32) == 0)
#ifdef WINDOWS
        /* x is +/-infinity. For VC++, return infinity of same sign. */
        return x;
#else
        /* x is +/-infinity. Return +infinity with no flags. */
        return infinityf_with_flags(0);
#endif
      else
        /* x is NaN, result is NaN */
#ifdef WINDOWS
        return handle_errorf("logbf", ux|0x00400000, _DOMAIN,
                             AMD_F_INVALID, EDOM, x, 0.0F);
#else
        return x + x; /* Raise invalid if it is a signalling NaN */
#endif
    }
  else
    {
      /* x is denormalized. */
#ifdef FOLLOW_IEEE754_LOGB
      /* Return the value of the minimum exponent to ensure that
         the relationship between logb and scalb, defined in
         IEEE 754, holds. */
      return EMIN_SP32;
#else
      /* Follow the rule set by IEEE 854 for logb */
      ux &= MANTBITS_SP32;
      u = EMIN_SP32;
      while (ux < IMPBIT_SP32)
        {
          ux <<= 1;
          u--;
        }
      return (float)u;
#endif
    }
}

weak_alias (__logbf, logbf)
