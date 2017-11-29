
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

#define USE_INFINITY_WITH_FLAGS
#define USE_HANDLE_ERROR
#include "../inc/libm_inlines_amd.h"
#undef USE_INFINITY_WITH_FLAGS
#undef USE_HANDLE_ERROR

#ifdef WINDOWS
#include "../inc/libm_errno_amd.h"
#endif

#ifdef WINDOWS
double FN_PROTOTYPE(logb)(double x)
#else
double FN_PROTOTYPE(logb)(double x)
#endif
{

  unsigned long long ux;
  long long u;
  GET_BITS_DP64(x, ux);
  u = ((ux & EXPBITS_DP64) >> EXPSHIFTBITS_DP64) - EXPBIAS_DP64;
  if ((ux & ~SIGNBIT_DP64) == 0)
    /* x is +/-zero. Return -infinity with div-by-zero flag. */
#ifdef WINDOWS
    return handle_error("logb", NINFBITPATT_DP64, _SING,
                        AMD_F_DIVBYZERO, ERANGE, x, 0.0);
#else
    return -infinity_with_flags(AMD_F_DIVBYZERO);
#endif
  else if (EMIN_DP64 <= u && u <= EMAX_DP64)
    /* x is a normal number */
    return (double)u;
  else if (u > EMAX_DP64)
    {
      /* x is infinity or NaN */
      if ((ux & MANTBITS_DP64) == 0)
#ifdef WINDOWS
        /* x is +/-infinity. For VC++, return infinity of same sign. */
        return x;
#else
        /* x is +/-infinity. Return +infinity with no flags. */
        return infinity_with_flags(0);
#endif
      else
        /* x is NaN, result is NaN */
#ifdef WINDOWS
        return handle_error("logb", ux|0x0008000000000000, _DOMAIN,
                            AMD_F_INVALID, EDOM, x, 0.0);
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
      return EMIN_DP64;
#else
      /* Follow the rule set by IEEE 854 for logb */
      ux &= MANTBITS_DP64;
      u = EMIN_DP64;
      while (ux < IMPBIT_DP64)
        {
          ux <<= 1;
          u--;
        }
      return (double)u;
#endif
    }

}

weak_alias (__logb, logb)
