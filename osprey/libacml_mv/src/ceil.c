
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
#pragma function(ceil)
#endif

double FN_PROTOTYPE(ceil)(double x)
{
  double r;
  long long rexp, xneg;
  unsigned long long ux, ax, ur, mask;

  GET_BITS_DP64(x, ux);
  /*ax is |x|*/
  ax = ux & (~SIGNBIT_DP64);
  /*xneg stores the sign of the input x*/
  xneg = (ux != ax);
  /*The range is divided into 
    > 2^53. ceil will either the number itself or Nan
            always returns a QNan. Raises exception if input is a SNan
    < 1.0   If 0.0 then return with the appropriate sign
            If input is less than -0.0 and greater than -1.0 then return -0.0
            If input is greater than 0.0 and less than 1.0 then return 1.0
    1.0 < |x| < 2^53 
            appropriately check the exponent and set the return Value by shifting
            */
  if (ax >= 0x4340000000000000) /* abs(x) > 2^53*/
    {
      /* abs(x) is either NaN, infinity, or >= 2^53 */
      if (ax > 0x7ff0000000000000)
        /* x is NaN */
#ifdef WINDOWS
        return handle_error("ceil", ux|0x0008000000000000, _DOMAIN, 0,
                            EDOM, x, 0.0);
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
      else if (xneg) /* x < 0.0; return -0.0 */
        {
          PUT_BITS_DP64(0x8000000000000000, r);
          return r;
        }
      else
        return 1.0;
    }
  else
    {
      /*Get the exponent for the floating point number. Should be between 0 and 53*/
      rexp = ((ux & EXPBITS_DP64) >> EXPSHIFTBITS_DP64) - EXPBIAS_DP64;
      /* Mask out the bits of r that we don't want */
      mask = 1;
      mask = (mask << (EXPSHIFTBITS_DP64 - rexp)) - 1;
      /*Keeps the exponent part and the required mantissa.*/
      ur = (ux & ~mask);
      PUT_BITS_DP64(ur, r);
      if (xneg || (ur == ux))
        return r;
      else
        /* We threw some bits away and x was positive */
        return r + 1.0;
    }

}

weak_alias (__ceil, ceil)
